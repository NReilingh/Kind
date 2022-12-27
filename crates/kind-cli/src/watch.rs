use std::path::{Path, PathBuf};

use async_trait::async_trait;
use kind_query::{CompilationServer, Index, ReusableIndex, Session};
use kind_report::data::Diagnostic;
use notify::event::{AccessKind, ModifyKind, RenameMode};
use notify::{Config, RecommendedWatcher, RecursiveMode};
use notify::{Event, Watcher};
use tokio::{runtime::Handle, sync::mpsc};

pub struct Backend<I>
where
    I: ReusableIndex,
{
    session: Session<I, PathBuf, Event>,
    root_node: Index,
    root_path: PathBuf,
}

#[async_trait]
impl CompilationServer<notify::Event> for Backend<Index> {
    async fn compiled_module(&mut self, key: Index) {
        println!("Compiled {}", key);
    }

    async fn invalidated_module(&mut self, key: Index) {
        println!("Invalidated {}", key);
    }

    async fn removed_module(&mut self, key: Index) {
        println!("Removed {}", key);
    }

    async fn diagnostic(&mut self, key: Index, err: Box<dyn Diagnostic>) {
        println!("Diagnostic {}", key);
    }

    async fn other_events(&mut self, ev: notify::Event) {
        use notify::EventKind::*;
        match ev.kind {
            Access(AccessKind::Close(_)) | Modify(ModifyKind::Name(RenameMode::To)) => {
                for path in ev.paths {
                    self.modified_file(path).await
                }
            }
            Modify(ModifyKind::Name(RenameMode::From)) => {
                for path in ev.paths {
                    self.deleted_file(path).await
                }
            }
            Create(_) => {
                for path in ev.paths {
                    self.created_file(path).await
                }
            }
            _ => (),
        }
    }
}

impl Backend<Index> {
    async fn recompile(&mut self) {
        self.session
            .compile_root(&self.root_node, &self.root_path, "Main")
            .await;
    }

    async fn created_file(&mut self, path: PathBuf) {
        println!("Created {:?}", path);
    }

    async fn modified_file(&mut self, path: PathBuf) {
        println!("Modified {:?}", path);
        if let Some((index, _)) = self.session.database.paths.get(&path).cloned() {
            self.session.invalidate(&index);
        }
        self.recompile().await
    }

    async fn deleted_file(&mut self, path: PathBuf) {
        println!("Deleted {:?}", path)
    }
}
pub async fn run(root: &Path, file: &Path) {
    let (event_sender, mut rx) = tokio::sync::mpsc::channel(32);

    let mut session = Session::new(root, event_sender.clone());
    let root_node = session.new_node();

    let mut backend: Backend<Index> = Backend {
        session,
        root_node,
        root_path: file.to_path_buf(),
    };

    let (watcher_tx, mut watcher_rx) = mpsc::channel(1);

    let handle = Handle::current();

    let mut watcher = RecommendedWatcher::new(
        move |r| handle.block_on(async { watcher_tx.clone().send(r).await.unwrap() }),
        Config::default(),
    )
    .unwrap();

    watcher
        .watch(root.as_ref(), RecursiveMode::Recursive)
        .unwrap();

    let watcher_task = tokio::spawn(async move {
        println!("Receiving events from watcher");
        while let Some(result) = watcher_rx.recv().await {
            match result {
                Ok(res) => {
                    event_sender.send(kind_query::Event::Other(res)).await;
                }
                Err(_) => (),
            }
        }
    });

    tokio::spawn(async {
        let _ = watcher_task.await;
    });

    backend.receive(&mut rx).await;

    todo!()
}
