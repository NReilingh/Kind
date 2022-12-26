//! This module is a generalization of the driver
//! module. It is useful both for LSPs, Watch, Repl
//! and many other things.

use std::{
    collections::VecDeque,
    fmt::Display,
    fs,
    path::{Path, PathBuf},
    sync::mpsc::{self, Sender},
};

use diagnostic::QueryDiagnostic;
use fxhash::{FxHashMap, FxHashSet};
use kind_pass::{expand::{expand_module, uses::expand_uses}, unbound};
use kind_pass::unbound::UnboundCollector;
use kind_report::{data::Diagnostic, report::FileCache};
use kind_span::{Range, SyntaxCtxIndex};
use kind_tree::{concrete::{self}, symbol::Ident};
use kind_tree::concrete::{Book, Module, TopLevel};
use kind_tree::symbol::Symbol;
use kind_tree::{concrete::visitor::Visitor, symbol::QualifiedIdent};
use petgraph::prelude::DiGraphMap;
use petgraph::visit::{depth_first_search, Control, Reversed};
use petgraph::Direction;
use strsim::jaro;
use std::fmt::Debug;
use std::hash::Hash;

pub mod diagnostic;

#[derive(PartialEq, Debug, Eq, Hash, PartialOrd, Ord, Copy, Clone, Default)]
pub struct Index(usize);

impl Display for Index {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Default)]
pub struct IndexState<K = Index>
where
    K: Eq,
{
    deleted_keys: VecDeque<K>,
    count: K,
}

pub trait ReusableIndex: Copy + Clone + Eq + PartialEq + Default + Hash + Ord {
    fn inc(&self) -> Self;
}

impl<Key> IndexState<Key>
where
    Key: ReusableIndex,
{
    pub fn new_key(&mut self) -> Key {
        self.deleted_keys.pop_front().unwrap_or_else(|| {
            let key = self.count;
            self.count = self.count.inc();
            Key::from(key)
        })
    }

    pub fn remove(&mut self, key: &Key) {
        self.deleted_keys.push_back(*key);
    }
}

impl ReusableIndex for Index {
    fn inc(&self) -> Self {
        Index(self.0 + 1)
    }
}

// Resources

pub struct File<URI> {
    pub name: Option<Symbol>,
    pub path: URI,
    pub source: String,
}

pub struct ModuleInterface {
    module: concrete::Module,
    exposed: FxHashSet<Symbol>,
    ranges: FxHashMap<Symbol, Range>,
    using: FxHashMap<String, FxHashSet<QualifiedIdent>>,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Status {
    Valid,
    Invalid,
    NotCompiled,
    ToGraph,
}

pub struct Node<T> {
    pub status: Status,
    hash: u64,
    interface: T,
    diagnostics: Vec<QueryDiagnostic>,
}

impl Node<ModuleInterface> {
    pub fn get_used_names(&self) -> FxHashSet<String> {
        self.interface.using.keys().cloned().collect()
    }
}

pub struct Database<Index: ReusableIndex, URI>
where
    URI: Hash,
{
    pub indexes: IndexState<Index>,
    pub nodes: FxHashMap<Index, Node<ModuleInterface>>,
    pub files: FxHashMap<Index, File<URI>>,
    pub names: FxHashMap<Symbol, (Range, Index)>,
    pub paths: FxHashMap<URI, (Index, QualifiedIdent)>,
}

// Just to avoid default constraints
impl<Index, URI> Default for Database<Index, URI>
where
    Index: ReusableIndex,
    URI: Hash,
{
    fn default() -> Self {
        Self {
            indexes: Default::default(),
            nodes: Default::default(),
            files: Default::default(),
            names: Default::default(),
            paths: Default::default(),
        }
    }
}

impl<Key, URI> Database<Key, URI>
where
    Key: ReusableIndex + Debug,
    URI: Hash,
{
    pub fn new_node(&mut self) -> Key {
        self.indexes.new_key()
    }

    pub fn invalidate(&mut self, node: Key) -> bool {
        if let Some(res) = self.nodes.get_mut(&node) {
            println!("Invalidated {:?}", node);
            let was_invalid = res.status == Status::Invalid;
            res.status = Status::Invalid;
            return was_invalid;
        }
        return false;
    }

    pub fn remove(&mut self, node: &Key) {
        self.files.remove(node);
        self.indexes.remove(node);
    }
}

pub enum Outcome {
    Compiled,
    StillValid,
}

// Session

pub struct Session<Index: ReusableIndex, URI>
where
    URI: Hash,
{
    pub database: Database<Index, URI>,
    pub graph: DiGraphMap<Index, ()>,
    root: PathBuf,
}

impl<Key, URI> Session<Key, URI>
where
    Key: ReusableIndex + Debug,
    URI: Hash,
{
    pub fn new(root: PathBuf) -> Self {
        Self {
            database: Default::default(),
            graph: Default::default(),
            root,
        }
    }

    pub fn get_used_names(&mut self, key: &Key) -> Option<FxHashSet<String>> {
        let node = self.database.nodes.get(key)?;
        Some(node.get_used_names())
    }

    pub fn new_node(&mut self) -> Key {
        let id = self.database.new_node();
        self.graph.add_node(id);
        id
    }

    pub fn set_file(&mut self, key: &Key, file: File<URI>) {
        self.database.files.insert(*key, file);
    }

    pub fn set_interface(&mut self, key: &Key, module: ModuleInterface) {
        self.database.nodes.insert(
            *key,
            Node {
                status: Status::ToGraph,
                hash: 0, // TODO: Calculate the hash
                interface: module,
                diagnostics: Default::default(),
            },
        );
    }

    pub fn update_keys(
        &mut self,
        key: &Key,
        new_module: &ModuleInterface,
    ) -> Result<(), Vec<QueryDiagnostic>> {
        let mut errs = Vec::new();

        let old_module = self.database.nodes.get(key);
        let old_keys = old_module
            .map(|x| x.interface.exposed.clone())
            .unwrap_or_default();

        let added_keys: FxHashSet<&Symbol> = new_module.exposed.difference(&old_keys).collect();
        let deleted_keys: FxHashSet<&Symbol> = old_keys.difference(&new_module.exposed).collect();

        for added_key in added_keys.clone() {
            if let Some(old_range) = self.database.names.get(added_key).map(|x| x.0) {
                let new_range = new_module.ranges.get(added_key).unwrap();
                errs.push(QueryDiagnostic::DuplicatedDefinition(
                    added_key.clone(),
                    *new_range,
                    old_range,
                ));
            }
        }

        if !errs.is_empty() {
            return Err(errs);
        }

        for added_key in added_keys {
            let res = self.database.names.insert(
                added_key.clone(),
                (new_module.ranges.get(added_key).cloned().unwrap(), *key),
            );

            assert!(res.is_none())
        }

        for deleted_key in deleted_keys {
            if let Some(old_key) = self.database.names.remove(deleted_key).map(|x| x.1) {
                self.graph.remove_node(old_key);
            }
        }

        Ok(())
    }
}

/// The extension of kind2 files.
const EXT: &str = "kind2";

/// Tries to accumulate on a buffer all of the
/// paths that exists (so we can just throw an
/// error about ambiguous resolution to the user)
fn accumulate_neighbour_paths(
    ident: &QualifiedIdent,
    raw_path: &Path,
) -> Result<Option<PathBuf>, Box<dyn Diagnostic>> {
    let mut canon_path = raw_path.to_path_buf();
    let mut dir_file_path = canon_path.clone();
    let dir_path = canon_path.clone();

    canon_path.set_extension(EXT);

    dir_file_path.push("_");
    dir_file_path.set_extension(EXT);

    if canon_path.exists() && dir_path.exists() && canon_path.is_file() && dir_path.is_dir() {
        Err(Box::new(QueryDiagnostic::MultiplePaths(
            ident.clone(),
            vec![canon_path, dir_path],
        )))
    } else if canon_path.is_file() {
        Ok(Some(canon_path))
    } else if dir_file_path.is_file() {
        Ok(Some(dir_file_path))
    } else {
        Ok(None)
    }
}

/// Gets an identifier and tries to get all of the
/// paths that it can refer into a single path. If
/// multiple paths are found then we just throw an
/// error about ambiguous paths.
fn ident_to_path(
    root: &Path,
    ident: &QualifiedIdent,
    search_on_parent: bool,
) -> Result<Option<PathBuf>, Box<dyn Diagnostic>> {
    let name = ident.to_string();
    let segments = name.as_str().split('.').collect::<Vec<&str>>();
    let mut raw_path = root.to_path_buf();

    raw_path.push(PathBuf::from(segments.join("/")));

    match accumulate_neighbour_paths(ident, &raw_path) {
        Ok(None) if search_on_parent => {
            raw_path.pop();
            accumulate_neighbour_paths(ident, &raw_path)
        }
        rest => rest,
    }
}

impl From<Index> for SyntaxCtxIndex {
    fn from(value: Index) -> Self {
        SyntaxCtxIndex(value.0)
    }
}

fn module_to_book<'a>(module: Module, book: &'a mut Book) -> FxHashSet<String> {
    let mut public_names = FxHashSet::default();

    for entry in module.entries {
        match entry {
            TopLevel::SumType(sum) => {
                let name = sum.name.to_string();

                public_names.insert(name.clone());

                for cons in &sum.constructors {
                    let mut cons_ident = sum.name.add_segment(cons.name.to_str());
                    cons_ident.range = cons.name.range;
                    book.names
                        .insert(cons_ident.to_string(), cons_ident.clone());
                    let cons_name = cons_ident.to_string();
                    public_names.insert(cons_name.clone());
                    book.meta.insert(cons_name, cons.extract_book_info(&sum));
                }

                book.names.insert(sum.name.to_string(), sum.name.clone());
                book.meta.insert(name.clone(), sum.extract_book_info());
                book.entries.insert(name, TopLevel::SumType(sum));
            }
            TopLevel::RecordType(rec) => {
                let name = rec.name.to_string();
                public_names.insert(name.clone());
                book.meta.insert(name.clone(), rec.extract_book_info());

                book.names.insert(rec.name.to_string(), rec.name.clone());

                let cons_ident = rec.name.add_segment(rec.constructor.to_str());
                public_names.insert(cons_ident.to_string());

                book.meta.insert(
                    cons_ident.to_string(),
                    rec.extract_book_info_of_constructor(),
                );

                book.names.insert(cons_ident.to_string(), cons_ident);
                book.entries.insert(name.clone(), TopLevel::RecordType(rec));
            }
            TopLevel::Entry(entr) => {
                let name = entr.name.to_string();
                book.names.insert(entr.name.to_string(), entr.name.clone());
                public_names.insert(name.clone());
                book.meta.insert(name.clone(), entr.extract_book_info());
                book.entries.insert(name, TopLevel::Entry(entr));
            }
        }
    }

    public_names
}


fn unbound_variable(sender: Sender<Box<dyn Diagnostic>>, book: &Book, idents: &[Ident]) {
    let mut similar_names = book
        .names
        .keys()
        .map(|x| (jaro(x, idents[0].to_str()).abs(), x))
        .filter(|x| x.0 > 0.8)
        .collect::<Vec<_>>();

    similar_names.sort_by(|x, y| x.0.total_cmp(&y.0));

    let err = Box::new(QueryDiagnostic::UnboundVariable(
        idents.to_vec(),
        similar_names.iter().take(5).map(|x| x.1.clone()).collect(),
    ));

    sender.send(err).unwrap();
}

impl<Key> Session<Key, PathBuf>
where
    Key: ReusableIndex + Into<SyntaxCtxIndex> + Debug,
{
    pub fn get_file(&self, path: &Path) -> Result<File<PathBuf>, Vec<Box<dyn Diagnostic>>> {
        let canon = &fs::canonicalize(path).unwrap();

        if let Ok(source) = fs::read_to_string(canon) {
            Ok(File {
                name: None,
                path: canon.to_path_buf(),
                source,
            })
        } else {
            let err = Box::new(QueryDiagnostic::CannotFindPath(canon.clone()));
            Err(vec![err])
        }
    }

    pub fn pre_compile_node(
        &mut self,
        key: &Key,
        path: &Path,
        ident: &QualifiedIdent,
    ) -> Result<Outcome, Vec<Box<dyn Diagnostic>>> {
        if let Some(node) = self.database.nodes.get(&key) {
            if node.status == Status::Valid || node.status == Status::ToGraph {
                return Ok(Outcome::StillValid);
            }
        }

        let (tx, rx) = mpsc::channel();

        let file = self.get_file(path)?;

        let (mut module, _) = kind_parser::parse_book(tx.clone(), (*key).into(), &file.source);
        expand_uses(&mut module, tx.clone());
        expand_module(tx.clone(), &mut module);

        self.set_file(key, file);

        let mut state = UnboundCollector::new(tx.clone(), false);
        state.visit_module(&mut module);

        let mut errs: Vec<_> = rx.try_iter().collect();

        if errs.is_empty() {
            let ranges = state.top_level_defs.clone();
            let exposed = ranges.keys().cloned().collect();

            let interface = ModuleInterface {
                module,
                exposed,
                ranges,
                using: state.unbound_top_level.clone(),
            };

            if let Err(err) = self.update_keys(key, &interface) {
                for err in err {
                    errs.push(Box::new(err));
                }
                return Err(errs);
            }

            self.set_interface(key, interface);

            self.database
                .paths
                .insert(path.canonicalize().unwrap(), (*key, ident.clone()));

            Ok(Outcome::Compiled)
        } else {
            Err(errs)
        }
    }

    pub fn get_node_for_ident(
        &mut self,
        ident: &QualifiedIdent,
    ) -> Result<Option<(Status, Key, PathBuf)>, Box<dyn Diagnostic>> {
        match ident_to_path(&self.root, ident, true)? {
            Some(path) => {
                let canon = path.canonicalize().unwrap();
                if let Some((id, old_ident)) = self.database.paths.get(&canon) {
                    if old_ident.get_root_symbol() == ident.get_root_symbol() {
                        let status = self.database.nodes.get(id).unwrap().status;
                        return Ok(Some((status, *id, canon)));
                    }
                    todo!("Here probably both nodes have the same path but different identifiers.")
                }
                let id = self.new_node();
                Ok(Some((Status::NotCompiled, id, canon)))
            }
            None => Ok(None),
        }
    }

    pub fn invalidate(&mut self, node: &Key) {
        depth_first_search(Reversed(&self.graph), Some(*node), |event| {
            use petgraph::visit::DfsEvent::*;
            if let Discover(node, _) = event {
                if self.database.invalidate(node) {
                    return Control::Break(0);
                }
            }
            Control::Continue
        });
    }

    pub fn remove(&mut self, key: &Key, path: &Path) {
        self.invalidate(key);
        self.database.remove(key);
        self.graph.remove_node(*key);
        self.database.paths.remove(path);
    }

    pub fn compile_root(
        &mut self,
        parent_key: &Key,
        path: &Path,
        ident: &str,
    ) -> Result<(), Vec<Box<dyn Diagnostic>>> {
        self.compile(
            parent_key,
            path,
            QualifiedIdent::new_static(ident, None, Range::ghost_range()),
        )
    }

    pub fn reconnect(&mut self, parent_key: Key) -> Option<()> {
        let node = self.database.nodes.get_mut(&parent_key)?;

        node.status = Status::Valid;

        let children = node
            .interface
            .using
            .values()
            .map(|x| x.iter().next().unwrap().clone());

        for child in children {
            if let Some((_, child_key)) = self.database.names.get(child.get_root_symbol()) {
                self.graph.add_edge(parent_key, *child_key, ());
            }
        }

        Some(())
    }

    pub fn pre_compile(
        &mut self,
        parent_key: &Key,
        path: &Path,
        ident: QualifiedIdent,
    ) -> Result<bool, Vec<Box<dyn Diagnostic>>> {
        let mut visited = Vec::new();
        let mut queue = Vec::new();
        let mut errs = Vec::new();

        let mut connect = Vec::new();

        queue.push((*parent_key, path.to_path_buf(), ident));

        let edges: Vec<Key> = self
            .graph
            .neighbors_directed(*parent_key, Direction::Outgoing)
            .into_iter()
            .collect();

        for node in edges {
            self.graph.remove_edge(*parent_key, node);
        }

        let mut recompiled = false;

        while let Some((key, path, ident)) = queue.pop() {
            if visited.contains(&key) {
                continue;
            }

            visited.push(key);

            let old_used_names: FxHashSet<_> = self.get_used_names(&key).unwrap_or_default();

            match self.pre_compile_node(&key, &path, &ident) {
                Ok(Outcome::StillValid) => (),
                Ok(Outcome::Compiled) => {
                    recompiled = true;
                    connect.push(key);
                    let node = self.database.nodes.get(&key).unwrap();
                    let new_used_names: FxHashSet<_> = node.get_used_names();
                    let removed_connections = old_used_names.difference(&new_used_names);

                    let children: Vec<QualifiedIdent> = node
                        .interface
                        .using
                        .values()
                        .map(|x| x.iter().next().unwrap())
                        .cloned()
                        .collect();

                    for removed_ident in removed_connections {
                        if let Some((_, child_key)) = self
                            .database
                            .names
                            .get(&Symbol::new(removed_ident.to_string()))
                        {
                            self.graph.remove_edge(key, *child_key);
                        }
                    }

                    for ident in children {
                        match self.get_node_for_ident(&ident) {
                            Ok(None) => (),
                            Ok(Some((status, key, value))) => {
                                queue.push((key, value, ident.clone()));
                                if status == Status::Invalid {
                                    self.graph.remove_edge(*parent_key, key);
                                }
                            }
                            Err(err) => errs.push(err),
                        }
                    }
                }
                Err(new_errs) => {
                    for err in new_errs {
                        errs.push(err);
                    }
                }
            }
        }

        // TODO: Reconnect

        if errs.is_empty() {
            for conn in connect {
                self.reconnect(conn);
            }
            Ok(recompiled)
        } else {
            Err(errs)
        }
    }

    pub fn compile(
        &mut self,
        parent_key: &Key,
        path: &Path,
        ident: QualifiedIdent,
    ) -> Result<(), Vec<Box<dyn Diagnostic>>> {
        let mut errs = Vec::new();

        match self.pre_compile(parent_key, path, ident) {
            Ok(recomp) if !recomp => {
                return Ok(())
            },
            Err(err) => {
                errs = err;
            },
            _ => ()
        }

        let mut book = Book::default();

        depth_first_search(&self.graph, Some(*parent_key), |event| {
            use petgraph::visit::DfsEvent::*;
            if let Discover(node, _) = event {
                if let Some(node) = self.database.nodes.get(&node) {
                    module_to_book(node.interface.module.clone(), &mut book);
                }
            }
        });

        let (tx, rx) = mpsc::channel();

        let (unbound_names, unbound_tops) = unbound::get_book_unbound(tx.clone(), &mut book, true);

        for unbound in unbound_tops.values() {
            let res: Vec<Ident> = unbound
                .iter()
                .filter(|x| !x.generated)
                .map(|x| x.to_ident())
                .collect();

            if !res.is_empty() {
                unbound_variable(tx.clone(), &book, &res);
            }
        }

        for unbound in unbound_names.values() {
            unbound_variable(tx.clone(), &book, &unbound);
        }

        for err in rx.try_iter() {
            errs.push(err)
        }

        if !errs.is_empty() {
            Err(errs)
        } else {
            Ok(())
        }
    }
}

impl FileCache for Session<Index, PathBuf> {
    fn fetch(&self, ctx: SyntaxCtxIndex) -> Option<(PathBuf, &String)> {
        let module = self.database.files.get(&Index(ctx.0))?;
        Some((module.path.clone(), &module.source))
    }
}
