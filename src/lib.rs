use std::collections::HashSet;

use line_column::span::Span;
use lsp_types::{DiagnosticSeverity, InsertTextFormat};
use syntax::{AstNode, AstToken, SyntaxNode, TextRange, TextSize, ast};

fn lsp_pos(span: &Span) -> lsp_types::Position {
    let (line, column) = span.line_column();
    lsp_types::Position { line: line-1, character: column-1 }
}

fn lsp_range(span: &Span) -> lsp_types::Range {
    lsp_types::Range { start: lsp_pos(span), end: lsp_pos(&span.end()) }
}

fn srv_index(src: &str, pos: lsp_types::Position) -> usize {
    line_column::index(src, pos.line+1, pos.character+1)
}

pub fn diagnostics(file: &str) -> Vec<lsp_types::Diagnostic> {
    let span = Span::new_full(file);
    let mut analysis = Analysis::new(span);
    analysis.collect_diagnostics();
    analysis.diagnostics()
}

pub fn completions(file: &str, at: lsp_types::Position) -> Vec<lsp_types::CompletionItem> {
    let index = srv_index(file, at);
    let for_complete = [&file[..index], COMPLETE_MARKER, &file[index..]].concat();
    let span = Span::new_full(for_complete);
    let analysis = Analysis::new(span);
    let cover_range = TextRange::at(TextSize::from(index as u32), TextSize::of(COMPLETE_MARKER));
    analysis.completions(cover_range)
}

const COMPLETE_MARKER: &str = "abcdef"; // 考虑到颜色, 不要超出f字母
const MANIFEST_ATTRS: &[(&str, &str)] = &[
    ("name",                    r#"name: ["$1", "$2"]"#),
    ("hide",                    r#"hide: ${1:false}"#),
    ("ignoreCase",              r#"ignoreCase: ${1:false}"#),
    ("styles",                  r#"styles: [$0]"#),
    ("comment",                 r#"comment: {startsWith: "$1"$2}"#),
    ("bracketPairs",            r#"bracketPairs: [$1]"#),
    ("lineBackground",          r#"lineBackground: {$0}"#),
    ("defines",                 r#"defines: [$0]"#),
    ("contains",                r#"contains: [$0]"#),
    ("codeFormatter",           r#"codeFormatter: #$1#"#),
    ("codeShrinker",            r#"codeShrinker: #$1#"#),
];
const BUILTIN_COLORS: &[(&str, &str, &str)] = &[
    ("default",      "000000",     "A9B7C6",),
    ("string",       "067D17",     "6A8759",),
    ("strEscape",    "0037A6",     "CC7832",),
    ("comment",      "8C8C8C",     "808080",),
    ("meta",         "9E880D",     "BBB529",),
    ("number",       "1750EB",     "6897BB",),
    ("keyword",      "0033B3",     "CC7832",),
    ("keyword2",     "800000",     "AE8ABE",),
    ("constant",     "871094",     "9876AA",),
    ("type",         "808000",     "808000",),
    ("label",        "7050E0",     "6080B0",),
    ("variable",     "1750EB",     "58908A",),
    ("operator",     "205060",     "508090",),
    ("propKey",      "083080",     "CC7832",),
    ("propVal",      "067D17",     "6A8759",),
    ("tagName",      "0030B3",     "E8BF6A",),
    ("attrName",     "174AD4",     "BABABA",),
    ("namespace",    "871094",     "9876AA",),
    ("error",        "F50000",     "BC3F3C",),
];
const BUILTIN_MATCHERS: &[(&str, &str)] = &[
    ("ESCAPED_CHAR",                 r#"匹配转义符号，仅包含\x"#,),
    ("SINGLE_QUOTED_STRING",         r#"单引号字符串"#,),
    ("DOUBLE_QUOTED_STRING",         r#"双引号字符串"#,),
    ("QUOTED_STRING",                r#"单双引号字符串"#,),
    ("JAVA_ESCAPED_CHAR",            r#"匹配Java转义符号，包含 \b \t \n \f \r \" \' \\ \000 \u0000，匹配失败时会进行红色标记"#,),
    ("JAVA_SINGLE_QUOTED_STRING",    r#"Java单引号字符串"#,),
    ("JAVA_DOUBLE_QUOTED_STRING",    r#"Java双引号字符串"#,),
    ("JAVA_QUOTED_STRING",           r#"Java单双引号字符串"#,),
    ("C_ESCAPED_CHAR",               r#"匹配C转义符号，包含 \b \t \n \f \r \x \" \' \\ \000 \u0000，匹配失败时会进行红色标记"#,),
    ("C_SINGLE_QUOTED_STRING",       r#"C单引号字符串"#,),
    ("C_DOUBLE_QUOTED_STRING",       r#"C双引号字符串"#,),
    ("C_QUOTED_STRING",              r#"C单双引号字符串"#,),
    ("NORMAL_NUMBER",                r#"匹配数字，支持整数与小数"#,),
    ("PROGRAM_NUMBER",               r#"编程语言数字，大多数语言适用"#,),
    ("PROGRAM_NUMBER2",              r#"编程语言数字，与上面比取消了二进制支持"#,),
    ("JAVA_NUMBER",                  r#"Java数字"#,),
    ("C_NUMBER",                     r#"C语言数字"#,),
];
const BUILTIN_FORMATTERS: &[&str] = &[
    "BUILT_IN_CSS_FORMATTER",
    "BUILT_IN_HTML_FORMATTER",
    "BUILT_IN_JAVA_FORMATTER",
    "BUILT_IN_JS_FORMATTER",
    "BUILT_IN_JSON_FORMATTER",
    "BUILT_IN_XML_FORMATTER",
    "BUILT_IN_SMALI_FORMATTER",
];
const BUILTIN_SHINKERS: &[&str] = &[
    "BUILT_IN_CSS_SHRINKER",
    "BUILT_IN_HTML_SHRINKER",
    "BUILT_IN_JSON_SHRINKER",
];

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

    fn completions(&self, at: TextRange) -> Vec<lsp_types::CompletionItem> {
        let elem = self.root.syntax().covering_element(at);
        let loc = match &elem {
            ast::NodeOrToken::Node(node) => self.location(node),
            ast::NodeOrToken::Token(t) => self.location(&t.parent().unwrap()),
        };
        let make_item = |label: &str, mut snip: &str, detail: &str| {
            if let Some(tok) = elem.as_token() {
                let text = tok.text();
                let side = ['"', '#', '<', '>'];
                if text.starts_with(side) && snip.starts_with(&text[..1]) {
                    snip = &snip[1..]
                }
                if text.ends_with(side) && snip.ends_with(text.chars().next_back().unwrap()) {
                    snip = &snip[..snip.len()-1]
                }
            }
            lsp_types::CompletionItem {
                label: label.to_owned(),
                detail: Some(detail.to_owned()),
                insert_text: Some(snip.to_owned()),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            }
        };
        match loc {
            Location::Manifest => {
                MANIFEST_ATTRS.iter().map(|(name, snip)| make_item(*name, *snip, "")).collect()
            },
            Location::BuiltinMatcher => {
                BUILTIN_MATCHERS.iter().map(|(name, detail)| {
                    make_item(*name, &format!("#{name}#"), *detail)
                }).collect()
            },
            Location::BuiltinFormatter => {
                BUILTIN_FORMATTERS.iter().map(|name| {
                    make_item(*name, &format!("#{name}#"), "")
                }).collect()
            },
            Location::BuiltinShinker => {
                BUILTIN_SHINKERS.iter().map(|name| {
                    make_item(*name, &format!("#{name}#"), "")
                }).collect()
            },
            Location::Color => {
                BUILTIN_COLORS.iter().map(|(name, lg, dk)| {
                    let detail = format!("light: #{lg}, dark: #{dk}");
                    make_item(*name, &format!(r#""{name}""#), &detail)
                }).collect()
            },
            Location::Styles => vec![],
            Location::CommentDef => vec![],
            Location::Defines => vec![],
            Location::IncludeRegex => vec![],
            Location::IncludeMatcher => vec![],
            Location::Value => vec![],
            Location::Group => vec![],
            Location::Disabled => vec![],
        }
    }

    fn collect_diagnostics(&mut self) -> Option<()> {
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
        analysis.collect_diagnostics();
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
