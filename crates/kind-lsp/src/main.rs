use std::{
    env,
    path::{Path, PathBuf},
    sync::Arc,
};

use async_trait::async_trait;
use kind_query::{CompilationServer, File, Index, Session};
use kind_tree::concrete::MetaKind;
use tokio::sync::RwLock;
use tower_lsp::lsp_types::*;
use tower_lsp::{jsonrpc::Result, lsp_types::notification::PublishDiagnostics};

use kind_report::{
    data::{Diagnostic as CompilerDiagnostic, DiagnosticFrame, Severity},
    report::{find_in_line_guide, get_code_line_guide},
};

use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Clone)]
struct Backend {
    client: Client,
    // TODO: Change it later
    session: Arc<RwLock<Session<Index, PathBuf, ()>>>,
    root_node: Index,
    root_path: PathBuf,

    initialized: bool,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions::default()),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        ..Default::default()
                    },
                )),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, s: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;

        let cwd = env::current_dir().unwrap();

        self.client
            .log_message(MessageType::INFO, &format!("init {:?}", cwd))
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let mut session = self.session.write().await;
        let path = PathBuf::from(params.text_document.uri.path());

        if let Some((index, _)) = session.database.paths.get(&path).cloned() {
            session.invalidate(&index);
            session.compile_root(&index, &path, "Main").await;
        } else {
            let parent_key = session.new_node();
            session.compile_root(&parent_key, &path, "Main").await;
        }

        self.client
            .log_message(MessageType::INFO, &format!("{}", params.text_document.uri))
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let mut session = self.session.write().await;
        let path = PathBuf::from(params.text_document.uri.path())
            .canonicalize()
            .unwrap();

        if let Some((index, _)) = session.database.paths.get(&path).cloned() {
            let file = params.content_changes[0].text.clone();

            session.invalidate(&index);
            session.set_controlled(&index);

            session.set_file(
                &index,
                File {
                    name: None,
                    path: path.clone(),
                    source: file,
                },
            );

            session.compile_root(&index, &path, "Main").await;
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let mut session = self.session.write().await;

        let mut completions = vec![];

        for node in session.database.nodes.values() {
            for (name, meta) in &node.interface.module.names {
                completions.push(CompletionItem {
                    label: name.to_string(),
                    documentation: Some(Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: meta.docs.join("\n"),
                    })),
                    kind: Some(match meta.kind {
                        MetaKind::Function => CompletionItemKind::FUNCTION,
                        MetaKind::TypeDef => CompletionItemKind::ENUM,
                        MetaKind::RecordDef => CompletionItemKind::STRUCT,
                        MetaKind::Cons => CompletionItemKind::ENUM_MEMBER,
                    }),
                    ..Default::default()
                })
            }
        }

        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        self.client
            .log_message(MessageType::INFO, &format!("hover {:?}", params.clone()))
            .await;

        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("Awoo!".to_string())),
            range: None,
        }))
    }
}

#[async_trait]
impl CompilationServer<()> for Backend {
    async fn compiled_module(&mut self, key: Index) {
        let session = self.session.write().await;

        let Some(file) = session.database.files.get(&key) else { return };
        let Some(node) = session.database.nodes.get(&key) else { return };

        let mut diagnostics = vec![];
        let line_guide = get_code_line_guide(&file.source);

        self.client
            .log_message(MessageType::INFO, &format!("compiled {}", key))
            .await;

        for diagnostic in &node.diagnostics {
            let frame = diagnostic.to_diagnostic_frame();

            self.client
                .log_message(MessageType::INFO, &format!("frame {}", frame.title))
                .await;

            for pos in frame.positions {
                if pos.position.ctx.0 == usize::from(key) {
                    let range = Range {
                        start: {
                            let point = find_in_line_guide(pos.position.start, &line_guide);
                            Position {
                                line: point.line as u32,
                                character: point.column as u32,
                            }
                        },
                        end: {
                            let point = find_in_line_guide(pos.position.end, &line_guide);
                            Position {
                                line: point.line as u32,
                                character: point.column as u32,
                            }
                        },
                    };

                    diagnostics.push(Diagnostic {
                        range,
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: Some(NumberOrString::Number(frame.code as i32)),
                        code_description: None,
                        source: None,
                        message: frame.title.clone(),
                        related_information: None,
                        tags: None,
                        data: None,
                    })
                }
            }
        }

        self.client
            .publish_diagnostics(
                Url::from_file_path(file.path.clone()).unwrap(),
                diagnostics,
                Some(0),
            )
            .await;
    }

    async fn invalidated_module(&mut self, key: Index) {
        self.client
            .log_message(MessageType::INFO, &format!("invalidated {}", key))
            .await;
    }

    async fn removed_module(&mut self, key: Index) {
        self.client
            .log_message(MessageType::INFO, &format!("removed {}", key))
            .await;
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let cur_dir = env::current_dir().unwrap();

    let (event_sender, mut event_receiver) = tokio::sync::mpsc::channel(32);

    let (service, socket) = LspService::new(move |client| {
        let session = Arc::new(RwLock::new(Session::new(&cur_dir, event_sender)));

        let mut backend = Backend {
            client,
            session,
            root_node: Default::default(),
            root_path: cur_dir.clone(),
            initialized: false,
        };

        let cloned = backend.clone();

        tokio::spawn(async move {
            backend.receive(&mut event_receiver).await;
        });

        cloned
    });

    Server::new(stdin, stdout, socket).serve(service).await
}
