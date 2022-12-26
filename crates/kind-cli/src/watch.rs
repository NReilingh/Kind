use core::fmt;
use std::{
    collections::HashMap,
    io,
    path::{Path, PathBuf},
};

use crate::watch::notify::Watcher;
use kind_query::{Index, Session};
use kind_report::{
    report::{FileCache, Report},
    RenderConfig,
};
use notify::{
    event::{AccessKind, ModifyKind, RenameMode},
    Config, RecommendedWatcher, RecursiveMode,
};

extern crate notify;

struct ToWriteFmt<T>(pub T);

impl<T> fmt::Write for ToWriteFmt<T>
where
    T: io::Write,
{
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0.write_all(s.as_bytes()).map_err(|_| fmt::Error)
    }
}

pub fn render_to_stderr<T, E>(render_config: &RenderConfig, session: &T, err: &E)
where
    T: FileCache,
    E: Report,
{
    Report::render(
        err,
        session,
        render_config,
        &mut ToWriteFmt(std::io::stderr()),
    )
    .unwrap();
}

pub fn run(root: PathBuf, file: String) {
    let mut session: Session<Index, PathBuf> = Session::new(root.clone());

    let root_id = session.new_node();
    let root = root.canonicalize().unwrap();
    let file = PathBuf::from(file);
    let _ = session.compile_root(&root_id, &file, "Main");

    let render_config = kind_report::check_if_utf8_is_supported(false, 2);

    if let Err(diagnostics) = session.compile_root(&root_id, &file, "Main") {
        for diagnostic in diagnostics {
            render_to_stderr(&render_config, &session, &diagnostic)
        }
    }

    let (tx, rx) = std::sync::mpsc::channel();
    let mut watcher = RecommendedWatcher::new(tx, Config::default()).unwrap();

    watcher
        .watch(root.as_ref(), RecursiveMode::Recursive)
        .unwrap();

    for res in rx {
        use notify::EventKind::{Access, Create, Modify};
        match res {
            Ok(event) => match event.kind {
                Access(AccessKind::Close(_)) | Modify(ModifyKind::Name(RenameMode::To)) => {
                    for path in event.paths {
                        if let Some((node_id, _)) = session.database.paths.get(&path).cloned() {
                            session.invalidate(&node_id);
                            if let Err(errs) = session.compile_root(&root_id, &file, "Main") {
                                for diagnostic in errs {
                                    render_to_stderr(&render_config, &session, &diagnostic)
                                }
                            }
                        }
                    }
                }
                Modify(ModifyKind::Name(RenameMode::From)) => {
                    for path in event.paths {
                        if let Some((id, _)) = session.database.paths.get(&path).cloned() {
                            session.remove(&id, &path)
                        }
                    }
                }
                Create(_) => {
                    if let Err(errs) = session.compile_root(&root_id, &file, "Main") {
                        for diagnostic in errs {
                            render_to_stderr(&render_config, &session, &diagnostic)
                        }
                    }
                }
                _ => ()
            },
            Err(e) => println!("watch error: {:?}", e),
        }
    }
}
