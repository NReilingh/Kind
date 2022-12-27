use std::path::PathBuf;

use kind_report::data::{Diagnostic, Marker, Severity, Color, Subtitle, Word};
use kind_span::Range;
use kind_tree::symbol::{Symbol, QualifiedIdent, Ident};
use kind_report::data::DiagnosticFrame;

pub enum QueryDiagnostic {
    DuplicatedDefinition(Symbol, Range, Range),
    UnboundVariable(Vec<Ident>, Vec<String>),
    MultiplePaths(QualifiedIdent, Vec<PathBuf>),
    CannotFindPath(PathBuf)
}

impl Diagnostic for QueryDiagnostic {
    fn get_syntax_ctx(&self) -> Option<kind_span::SyntaxCtxIndex> {
        match self {
            QueryDiagnostic::DuplicatedDefinition(_, range, _) => Some(range.ctx),
            QueryDiagnostic::MultiplePaths(ident, _) => Some(ident.range.ctx),
            QueryDiagnostic::CannotFindPath(_) => None,
            QueryDiagnostic::UnboundVariable(v, _) => Some(v[0].range.ctx),
        }
    }

    fn to_diagnostic_frame(&self) -> kind_report::data::DiagnosticFrame {
        match self {
            QueryDiagnostic::UnboundVariable(idents, suggestions) => DiagnosticFrame {
                code: 100,
                severity: Severity::Error,
                title: format!("Cannot find the definition '{}'.", idents[0].to_str()),
                subtitles: vec![],
                hints: vec![if !suggestions.is_empty() {
                    format!(
                        "Maybe you're looking for {}",
                        suggestions.iter().map(|x| format!("'{}'", x)).collect::<Vec<String>>().join(", ")
                    )
                } else {
                    "Take a look at the rules for name searching at https://github.com/Kindelia/Kind2/blob/master/guide/naming.md".to_string()
                }],
                positions: idents
                    .iter()
                    .map(|ident| Marker {
                        position: ident.range,
                        color: Color::Fst,
                        text: "Here!".to_string(),
                        no_code: false,
                        main: true,
                    })
                    .collect(),
            },
            QueryDiagnostic::DuplicatedDefinition(_, fst, snd) => DiagnosticFrame {
                code: 102,
                severity: Severity::Error,
                title: "Defined multiple times for the same name".to_string(),
                subtitles: vec![],
                hints: vec!["Rename one of the definitions or remove and look at how names work in Kind at https://github.com/Kindelia/Kind2/blob/master/guide/naming.md".to_string()],
                positions: vec![
                    Marker {
                        position: *fst,
                        color: Color::Fst,
                        text: "The first ocorrence".to_string(),
                        no_code: false,
                        main: true,
                    },
                    Marker {
                        position: *snd,
                        color: Color::Snd,
                        text: "Second occorrence here!".to_string(),
                        no_code: false,
                        main: false,
                    },
                ],
            },
            QueryDiagnostic::CannotFindPath(file) => DiagnosticFrame {
                code: 103,
                severity: Severity::Error,
                title: format!("Cannot find file '{}'", file.to_string_lossy()),
                subtitles: vec![],
                hints: vec![],
                positions: vec![],
            },
            QueryDiagnostic::MultiplePaths(ident, paths) => DiagnosticFrame {
                code: 101,
                severity: Severity::Error,
                title: "Ambiguous definition location for the same name".to_string(),
                subtitles: paths
                    .iter()
                    .map(|path| Subtitle::Phrase(Color::Fst, vec![Word::White(path.display().to_string())]))
                    .collect(),
                hints: vec!["Take a look at the rules for name searching at https://github.com/Kindelia/Kind2/blob/master/guide/naming.md".to_string()],
                positions: vec![Marker {
                    position: ident.range,
                    color: Color::Fst,
                    text: "Here!".to_string(),
                    no_code: false,
                    main: true,
                }],
            },

        }
    }
}