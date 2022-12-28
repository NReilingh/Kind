use fxhash::FxHashMap;
use kind_report::data::Diagnostic;
use kind_tree::concrete::{visitor::{Visitor, walk_top_level}, Module, TopLevel, Meta, MetaKind};
/// Expands sum type and record definitions to a lot of
/// helper definitions like eliminators and replace qualified identifiers
/// by their module names.
use std::sync::mpsc::Sender;

use crate::{diagnostic::PassDiagnostic, desugar::top_level};

pub struct Expand {
    pub names: FxHashMap<String, String>,
    pub errors: Sender<Box<dyn Diagnostic>>,
    pub defined: FxHashMap<String, Meta>,
    pub failed: bool,
}

impl Visitor for Expand {
    fn visit_top_level(&mut self, top_level: &mut kind_tree::concrete::TopLevel) {
        walk_top_level(self, top_level);

        match top_level {
            TopLevel::SumType(sum) => {
                let name = sum.name.to_string();

                self.defined.insert(name.to_string(), Meta {
                    docs: sum.docs.clone(),
                    kind: MetaKind::TypeDef
                });

                for cons in &sum.constructors {
                    let cons_ident = sum.name.add_segment(cons.name.to_str());
                    self.defined.insert(cons_ident.to_string(), Meta {
                        docs: cons.docs.clone(),
                        kind: MetaKind::Cons
                    });
                }
            }
            TopLevel::RecordType(rec) => {
                let name = rec.name.to_string();
                self.defined.insert(name.clone(), Meta {
                    docs: rec.docs.clone(),
                    kind: MetaKind::RecordDef
                });
                let cons_ident = rec.name.add_segment(rec.constructor.to_str());
                self.defined.insert(cons_ident.to_string(), Meta {
                    docs: vec![],
                    kind: MetaKind::Cons
                });
            }
            TopLevel::Entry(entr) => {
                self.defined.insert(entr.name.to_string(), Meta {
                    docs: entr.docs.clone(),
                    kind: MetaKind::Function
                });
            }
        }
    }

    fn visit_qualified_ident(&mut self, ident: &mut kind_tree::symbol::QualifiedIdent) {
        if ident.get_aux().is_none() {
            return;
        }
        let alias = match self.names.get(&ident.get_root()) {
            Some(path) => path,
            None => {
                self.errors
                    .send(Box::new(PassDiagnostic::CannotFindAlias(
                        ident.get_root(),
                        ident.range,
                    )))
                    .unwrap();
                self.failed = true;
                return;
            }
        };
        match &ident.get_aux() {
            Some(post) => {
                ident.change_root(format!("{}.{}", alias, post));
                ident.reset_aux()
            }
            None => ident.change_root(alias.clone()),
        }
    }
}

pub fn expand_uses(module: &mut Module, errors: Sender<Box<dyn Diagnostic>>) -> bool {
    let mut session = Expand {
        names: module.uses.clone(),
        defined: Default::default(),
        errors,
        failed: false,
    };

    for entry in module.entries.iter_mut() {
        session.visit_top_level(entry)
    }

    module.names = session.defined;

    session.failed
}
