//! Describes a compilation session. It's not the finished
//! model because I want to change it to a query based compiler
//! later.

use std::path::PathBuf;
use std::rc::Rc;
use std::sync::mpsc::Sender;

use fxhash::{FxHashMap, FxHashSet};
use kind_report::data::Diagnostic;

#[derive(Debug, Clone)]
pub struct Session {
    pub loaded_paths: Vec<Rc<PathBuf>>,
    pub loaded_sources: Vec<String>,

    pub loaded_paths_map: FxHashMap<PathBuf, usize>,

    /// It will be useful in the future
    /// to make the public and private decls
    pub public_names: FxHashSet<String>,

    pub diagnostic_sender: Sender<Box<dyn Diagnostic>>,
    pub root: PathBuf,

    pub book_counter: usize,
}

impl Session {
    pub fn new(root: PathBuf, sender: Sender<Box<dyn Diagnostic>>) -> Session {
        Session {
            loaded_paths: Vec::new(),
            loaded_sources: Vec::new(),
            loaded_paths_map: FxHashMap::default(),
            public_names: FxHashSet::default(),
            root,
            book_counter: 0,
            diagnostic_sender: sender,
        }
    }

    pub fn add_path(&mut self, path: Rc<PathBuf>, code: String) -> usize {
        let id = self.book_counter;
        self.book_counter += 1;
        self.loaded_paths_map
            .insert((*path).clone(), self.book_counter);
        self.loaded_paths.push(path);
        self.loaded_sources.push(code);
        id
    }
}
