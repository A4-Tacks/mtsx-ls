use std::collections::HashSet;

use line_column::span::Span;
use lsp_types::DiagnosticSeverity;
use syntax::{AstNode, AstToken, SyntaxNode, TextRange, ast};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Location {
    Manifest,
    Styles,
    CommentDef,
    Defines,
    IncludeRegex,
    IncludeMatcher,
    Color,
    Value,
    Group,
    BuiltinMatcher,
    BuiltinFormatter,
    BuiltinShinker,
    Disabled,
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

    fn location(&self, node: &SyntaxNode) -> Location {
        if let Some(call) = ast::Call::cast(node.clone())
            && let Some(name) = call.name()
        {
            return match name.syntax().text() {
                "include" => Location::IncludeRegex,
                _ => Location::Disabled,
            }
        }
        node.ancestors()
            .filter_map(ast::Pair::cast)
            .find_map(|p| Some(match p.key()? {
                ast::Key::NUMBER(_) => match p.value() {
                    Some(ast::Value::Table(_)) => Location::Value,
                    _ => Location::Color,
                },
                ast::Key::IDENT(name) => match name.text() {
                    "contains" => Location::Value,
                    "comment" => Location::CommentDef,
                    "styles" => Location::Styles,
                    "colors" => Location::Styles,
                    "style" => Location::Color,
                    "color" => Location::Color,
                    "name" => Location::Disabled,
                    "defines" => Location::Defines,
                    "match" | "start" | "end" => Location::Value,
                    "include" => Location::IncludeMatcher,
                    "group" => Location::Group,
                    "builtin" => Location::BuiltinMatcher,
                    "codeFormatter" => Location::BuiltinFormatter,
                    "codeShinker" => Location::BuiltinShinker,
                    _ => return None,
                },
            }))
            .unwrap_or(Location::Manifest)
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

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};
    use syntax::TextSize;

    use super::*;

    #[track_caller]
    fn check_loc(src: &str, expect: Expect) {
        let index = src.find("$0").expect("must a `$0`");
        let src = src.replacen("$0", "", 1);
        let src = Span::new_full(src);
        let mut analysis = Analysis::new(src);
        analysis.analysis();
        assert!(analysis.diagnostics.is_empty());
        let element = analysis
            .root
            .syntax()
            .covering_element(TextRange::empty(TextSize::new(index.try_into().unwrap())));
        let node = match element {
            ast::NodeOrToken::Node(n) => n,
            ast::NodeOrToken::Token(t) => t.parent().unwrap(),
        };
        let loc = analysis.location(&node);
        expect.assert_eq(&format!("{loc:?}"));
    }

    #[test]
    fn test_location() {
        check_loc(r#"{$0}"#, expect!["Manifest"]);
        check_loc(r#"{name: [$0]}"#, expect!["Disabled"]);
        check_loc(r#"{styles: [$0]}"#, expect!["Styles"]);
        check_loc(r#"{contains: [$0]}"#, expect!["Value"]);
        check_loc(r#"{contains: [{$0}]}"#, expect!["Value"]);
        check_loc(r#"{contains: [{match: /foo/$0}]}"#, expect!["Value"]);
        check_loc(r#"{contains: [{match: /foo/, 0:""$0}]}"#, expect!["Color"]);
        check_loc(r#"{contains: [{builtin: #$0}]}"#, expect!["BuiltinMatcher"]);
        check_loc(r#"{codeFormatter: place$0}"#, expect!["BuiltinFormatter"]);
        check_loc(r#"{contains: [{match: include($0)}]}"#, expect!["IncludeRegex"]);
        check_loc(r#"{contains: [{match: include("$0")}]}"#, expect!["IncludeRegex"]);
        check_loc(r#"{contains: [{match: keywordsToRegex("$0")}]}"#, expect!["Disabled"]);
    }
}
