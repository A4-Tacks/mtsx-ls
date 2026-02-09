use std::collections::HashSet;

use line_column::span::Span;
use lsp_types::DiagnosticSeverity;
use syntax::{AstNode, SyntaxNode, TextRange, ast};

fn lsp_pos(span: &Span) -> lsp_types::Position {
    let (line, column) = span.line_column();
    lsp_types::Position { line: line-1, character: column-1 }
}

fn lsp_range(span: &Span) -> lsp_types::Range {
    lsp_types::Range { start: lsp_pos(span), end: lsp_pos(&span.end()) }
}

pub fn diagnostics(file: &str) -> Vec<lsp_types::Diagnostic> {
    let span = Span::new_full(file);
    let mut analysis = Analysis::new(span);
    analysis.analysis();
    analysis.diagnostics()
}

enum Location {
    Manifest,
    Styles,
    CommentDef,
    Defines,
    IncludeRegex,
    IncludeMatcher,
    Color,
    Value,
}

struct Analysis {
    source: Span,
    root: ast::SourceFile,
    diagnostics: Vec<(Span, String)>,
}

impl Analysis {
    fn new(source: Span) -> Self {
        let (source_file, diagnostics) = syntax::parse_file(source.clone());

        Self {
            source,
            root: source_file,
            diagnostics,
        }
    }

    fn error(&mut self, range: TextRange, msg: impl std::fmt::Display) {
        self.diagnostics.push((self.source.create(range), msg.to_string()));
    }

    fn diagnostics(&self) -> Vec<lsp_types::Diagnostic> {
        self.diagnostics
            .iter()
            .map(|(span, lint)| (lsp_range(span), lint))
            .map(|(range, lint)| lsp_types::Diagnostic {
                range,
                message: lint.replace('$', "$$"),
                severity: Some(DiagnosticSeverity::ERROR),
                ..Default::default()
            })
            .collect()
    }

    fn analysis(&mut self) -> Option<()> {
        self.root.syntax().descendants().for_each(|node| {
            self.check_missing_keys(&node);
        });
        Some(())
    }

    fn check_missing_keys(&mut self, node: &SyntaxNode) -> Option<()> {
        let table = ast::Table::cast(node.clone())?;
        let mut defined = HashSet::new();
        table.pairs()
            .filter_map(|it| match it.key()? {
                ast::Key::IDENT(it) => Some(it),
                ast::Key::NUMBER(_) => None,
            })
            .for_each(|name| {
                if !defined.insert(name.to_string()) {
                    self.error(name.text_range(), format_args!(
                            "duplicate key: `{name}`"));
                }
            });
        Some(())
    }
}
