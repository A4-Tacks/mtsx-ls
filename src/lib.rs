use std::{collections::HashSet, time::SystemTime};

use line_column::span::Span;
use lsp_types::{DiagnosticSeverity, InsertTextFormat};
use syntax::{AstNode, AstToken, Direction, NodeOrToken, SyntaxKind, SyntaxNode, SyntaxToken, TextRange, TextSize, ast::{self, Or}};

pub struct Tracer {
    pub trace: bool,
    start_at: SystemTime,
    name: &'static str,
}

impl Tracer {
    pub fn new(name: &'static str) -> Self {
        Tracer { trace: false, start_at: SystemTime::now(), name }
    }

    pub fn trace(&self, s: impl std::fmt::Display) {
        if !self.trace {
            return;
        }
        let now = SystemTime::now()
            .duration_since(self.start_at)
            .unwrap_or_else(|_| std::time::Duration::from_secs(0));
        let now = now.as_secs_f64();
        let args = format_args!("{now:010.5} {s}\n");
        eprint!("{args}");
        let mut log = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(format!("{}.log", self.name))
            .expect("cannot open log file");
        let _ = std::io::Write::write_fmt(&mut log, args);
    }
}

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

pub fn completions(file: &str, at: lsp_types::Position, tracer: &Tracer) -> Vec<lsp_types::CompletionItem> {
    let index = srv_index(file, at);
    let for_complete = [&file[..index], COMPLETE_MARKER, &file[index..]].concat();
    let span = Span::new_full(for_complete);
    let analysis = Analysis::new(span);
    let cover_range = TextRange::at(TextSize::from(index as u32), TextSize::of(COMPLETE_MARKER));
    analysis.completions(cover_range, tracer)
}

const COMPLETE_MARKER: &str = "abcdef"; // 考虑到颜色, 不要超出f字母
const MANIFEST_ATTRS: &[(&str, &str)] = &[
    ("name",                    r#"name: ["$1", "$2"]"#),
    ("hide",                    r#"hide: ${0:false}"#),
    ("ignoreCase",              r#"ignoreCase: ${0:false}"#),
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
const MATCHER_SCHEMA: &[((&str, &str), &[(&str, &str)])] = &[
    (("match", "match: $0"), &[
        ("recordAllGroups", "recordAllGroups: ${0:false}"),
    ]),
    (("start", "start: $0"), &[
        ("end", "end: $0"),
        ("style", "style: \"$1\""),
        ("childrenStyle", "childrenStyle: \"$1\""),
        ("matchEndFirst", "matchEndFirst: ${0:false}"),
        ("endPriority", "endPriority: ${0:0}"),
        ("mustMatchEnd", "mustMatchEnd: ${0:false}"),
        ("contains", "contains: [$0]"),
    ]),
    (("group", "group: $0"), &[
        ("style", "style: \"$1\""),
        ("contains", "contains: [$0]"),
    ]),
    (("number", "number: \"$1\""), &[
        ("iSuffixes", "iSuffixes: \"$1\""),
        ("style", "style: \"$1\""),
    ]),
    (("builtin", "builtin: #$1#"), &[]),
    (("include", "include: \"$1\""), &[]),
];
const PATTERNS: &[(&str, &str)] = &[
    ("include",                 r#"include("$1")"#),
    ("keywordsToRegex",         r#"keywordsToRegex("$1")"#),
];
const ALLOW_DUP_KEYS: &[&str] = &["comment", "lineBackground"];

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
    Boolean,
    Pattern,
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
        node.ancestors()
            .filter_map(Or::<Or<ast::Pair, ast::Item>, Or<ast::Call, ast::Table>>::cast)
            .find_map(|it| Some(match it {
                Or::A(Or::A(p)) => match p.key()? {
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
                        "match" => Location::Pattern,
                        "start" | "end" => Location::Value,
                        "include" => Location::IncludeMatcher,
                        "group" => Location::Group,
                        "builtin" => Location::BuiltinMatcher,
                        "codeFormatter" => Location::BuiltinFormatter,
                        "codeShinker" => Location::BuiltinShinker,
                        "hide" | "recordAllGroups" | "matchEndFirst" |
                        "mustMatchEnd" | "ignoreCase" | "insertSpace" |
                        "addToContains" => Location::Boolean,
                        _ => return None,
                    },
                },
                Or::A(Or::B(_item)) => return None,
                Or::B(Or::A(call)) => {
                    if let Some(name) = call.name() {
                        match name.syntax().text() {
                            "include" => Location::IncludeRegex,
                            _ => Location::Disabled,
                        }
                    } else {
                        Location::Disabled
                    }
                },
                Or::B(Or::B(table)) => {
                    if table.syntax().parent().and_then(ast::SourceFile::cast).is_some() {
                        Location::Manifest
                    } else if let Some(pair) = table.syntax().parent().and_then(ast::Pair::cast)
                        && let Some(key) = pair.key().and_then(ast::Key::into_ident)
                    {
                        match key.text() {
                            "comment" => Location::CommentDef,
                            _ => Location::Value,
                        }
                    } else {
                        Location::Value
                    }
                },
            }))
            .unwrap_or(Location::Disabled)
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

    fn completions(&self, at: TextRange, tracer: &Tracer) -> Vec<lsp_types::CompletionItem> {
        let elem = self.root.syntax().covering_element(at);
        let loc = match &elem {
            NodeOrToken::Node(node) => self.location(node),
            NodeOrToken::Token(t) => self.location(&t.parent().unwrap()),
        };
        tracer.trace(format_args!("complete location: {loc:?}"));
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
        fn is_matcher(value: &ast::Value) -> bool {
            matches!(value, ast::Value::Table(_) | ast::Value::Array(_))
        }
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
            Location::Value => {
                let Some(table) = elem.ancestors().find_map(ast::Table::cast) else {
                    return vec![];
                };
                if let Some((_, schema)) = table.pairs()
                    .filter_map(|pair| pair.key()?.into_ident())
                    .find_map(|name| {
                        MATCHER_SCHEMA.iter().find(|((title, _), _)| *title == name.text())
                    })
                {
                    schema.iter().map(|(label, snip)| {
                        make_item(*label, *snip, "")
                    }).collect()
                } else {
                    MATCHER_SCHEMA.iter().map(|((label, snip), _)| {
                        make_item(*label, *snip, "")
                    }).collect()
                }
            },
            Location::Styles => vec![],
            Location::CommentDef => vec![],
            Location::Defines => vec![],
            Location::IncludeRegex => {
                self.defines()
                    .filter(|(_, value)| !is_matcher(value))
                    .map(|(name, value)| {
                        let detail = format!("{}{name}: {value}", docs(&name));
                        make_item(name.text(), name.text(), &detail)
                    })
                    .collect()
            },
            Location::IncludeMatcher => {
                self.defines()
                    .filter(|(_, value)| is_matcher(value))
                    .map(|(name, value)| {
                        let detail = format!("{}{name}: {value}", docs(&name));
                        make_item(name.text(), name.text(), &detail)
                    })
                    .collect()
            },
            Location::Group => {
                ["link", "linkAll", "select"].iter().map(|name| make_item(*name, *name, "")).collect()
            },
            Location::Boolean => {
                ["true", "false"].iter().map(|name| make_item(*name, *name, "")).collect()
            },
            Location::Pattern => {
                PATTERNS.iter().map(|(label, snip)| {
                    make_item(*label, *snip, "")
                }).collect()
            },
            Location::Disabled => vec![],
        }
    }

    fn get_manifest(&self, name: &str) -> Option<ast::Value> {
        self.root.table()?.pairs().find_map(|pair| {
            (pair.key()?.into_ident()?.text() == name)
                .then(|| pair.value())
                .flatten()
        })
    }

    fn defines(&self) -> impl Iterator<Item = (SyntaxToken, ast::Value)> {
        (|| {
            let ast::Value::Array(defines) = self.get_manifest("defines")? else { return None };
            Some(defines.items()
                .filter_map(|item| {
                    let key = item.value()?.into_literal()?.lit()?.into_string()?;
                    Some((key, item.assoc()?))
                }))
        })().into_iter().flatten()
    }

    fn collect_diagnostics(&mut self) -> Option<()> {
        self.root.syntax().descendants().for_each(|node| {
            self.check_duplicate_keys(&node);
        });
        Some(())
    }

    fn check_duplicate_keys(&mut self, node: &SyntaxNode) -> Option<()> {
        let table = ast::Table::cast(node.clone())?;
        let mut defined = HashSet::new();
        table.pairs()
            .filter_map(|it| match it.key()? {
                ast::Key::IDENT(it) => Some(it),
                ast::Key::NUMBER(_) => None,
            })
            .for_each(|name| {
                if !defined.insert(name.to_string())
                    && !ALLOW_DUP_KEYS.contains(&name.text())
                {
                    self.error(name.text_range(), format_args!(
                            "duplicate key: `{name}`"));
                }
            });
        Some(())
    }
}

fn docs(name: &SyntaxToken) -> String {
    (|| {
        let def = name.parent_ancestors().find_map(Or::<ast::Pair, ast::Item>::cast)?;

        let suf_docs = def.syntax()
            .siblings_with_tokens(Direction::Next)
            .skip(1)
            .map_while(NodeOrToken::into_token)
            .take_while(|it| it.kind() == SyntaxKind::WHITESPACE)
            .take_while(|it| !it.text().contains('\n'))
            .last()
            .into_iter()
            .flat_map(|it| it.siblings_with_tokens(Direction::Next).skip(1))
            .map_while(NodeOrToken::into_token)
            .take_while(|it| it.kind().is_trivia())
            ;

        let docs = def.syntax()
            .siblings_with_tokens(Direction::Prev)
            .skip(1)
            .map_while(NodeOrToken::into_token)
            .take_while(|it| it.kind().is_trivia())
            .chain(suf_docs)
            .filter(|it| it.kind() == SyntaxKind::COMMENT)
            .map(|it| format!("{it}\n"))
            .collect::<Vec<_>>();
        Some(String::from_iter(docs.into_iter().rev()))
    })().unwrap_or(String::new()).to_owned()
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};
    use syntax::TextSize;

    use super::*;

    #[track_caller]
    fn check_loc(src: &str, expect: Expect) {
        let index = src.find("$0").expect("must a `$0`");
        let mut actual = String::new();

        for src in [
            src.replacen("$0", "", 1),
            src.replacen("$0", COMPLETE_MARKER, 1),
        ] {
            let src = Span::new_full(src);
            let mut analysis = Analysis::new(src);
            analysis.collect_diagnostics();
            let extra = if analysis.diagnostics.is_empty() { "" } else { &format!(" !{}", analysis.diagnostics.len()) };
            let element = analysis
                .root
                .syntax()
                .covering_element(TextRange::empty(TextSize::new(index.try_into().unwrap())));
            let node = match element {
                NodeOrToken::Node(n) => n,
                NodeOrToken::Token(t) => t.parent().unwrap(),
            };
            let loc = analysis.location(&node);

            if !actual.is_empty() {
                actual += ", ";
            }
            actual += &format!("{loc:?}{extra}");
        }
        expect.assert_eq(&actual);
    }

    #[track_caller]
    fn check(src: &str, expect: Expect) {
        let index = src.find("$0").expect("must a `$0`");
        let src = src.replacen("$0", "", 1);
        let src = Span::new_full(src);
        let pos = lsp_pos(&src.create(TextRange::empty(TextSize::new(index.try_into().unwrap()))));
        let completions = completions(src.source(), pos, &Tracer::new("test"));
        let completions = completions.iter().map(|item| {
            if let Some(detail) = &item.detail && !detail.is_empty() {
                format!("{:20}{detail:?}\n", item.label)
            } else {
                format!("{}\n", item.label)
            }
        }).collect::<String>();
        expect.assert_eq(&completions);
    }

    #[track_caller]
    fn check_complete(src: &str, expect: Expect) {
        let index = src.find("$0").expect("must a `$0`");
        let src = src.replacen("$0", "", 1);
        let src = Span::new_full(src);
        let pos = lsp_pos(&src.create(TextRange::empty(TextSize::new(index.try_into().unwrap()))));
        let completions = completions(src.source(), pos, &Tracer::new("test"));
        let completions = completions.iter().map(|item| {
            let text = item.insert_text.as_deref().unwrap_or(&item.label);
            format!("{:20}{text:?}\n", item.label)
        }).collect::<String>();
        expect.assert_eq(&completions);
    }

    #[test]
    fn test_location() {
        check_loc(
            r#"{$0}"#,
            expect!["Manifest, Manifest !1"],
        );
        check_loc(
            r#"{name: [$0]}"#,
            expect!["Disabled, Disabled"],
        );
        check_loc(
            r#"{styles: [$0]}"#,
            expect!["Styles, Styles"],
        );
        check_loc(
            r#"{comment: {$0}}"#,
            expect!["CommentDef, CommentDef !1"],
        );
        check_loc(
            r#"{contains: [$0]}"#,
            expect!["Value, Value"],
        );
        check_loc(
            r#"{contains: [{$0}]}"#,
            expect!["Value, Value !1"],
        );
        check_loc(
            r#"{contains: [{start: /foo/$0}]}"#,
            expect!["Value, Value !1"],
        );
        check_loc(
            r#"{contains: [{match: /foo/$0}]}"#,
            expect!["Pattern, Pattern !1"],
        );
        check_loc(
            r#"{contains: [{match: /foo/+$0}]}"#,
            expect!["Pattern !1, Pattern"],
        );
        check_loc(
            r#"{contains: [{match: $0+/foo/}]}"#,
            expect!["Pattern !1, Pattern"],
        );
        check_loc(
            r#"{contains: [{match: /foo/, 0:""$0}]}"#,
            expect!["Color, Color !1"],
        );
        check_loc(
            r#"{contains: [{builtin: $0}]}"#,
            expect!["Value !1, BuiltinMatcher"],
        );
        check_loc(
            r#"{contains: [{builtin: #$0}]}"#,
            expect!["BuiltinMatcher, BuiltinMatcher"],
        );
        check_loc(
            r#"{codeFormatter: $0}"#,
            expect!["Manifest !1, BuiltinFormatter"],
        );
        check_loc(
            r#"{contains: [{match: include($0)}]}"#,
            expect!["IncludeRegex, IncludeRegex !1"],
        );
        check_loc(
            r#"{contains: [{match: include("$0")}]}"#,
            expect!["IncludeRegex, IncludeRegex"],
        );
        check_loc(
            r#"{contains: [{match: keywordsToRegex("$0")}]}"#,
            expect!["Disabled, Disabled"],
        );
        check_loc(
            r#"{defines: [$0]}"#,
            expect!["Defines, Defines"],
        );
        check_loc(
            r#"{defines: ["x": {$0}]}"#,
            expect!["Value, Value !1"],
        );
        check_loc(
            r#"{defines: ["x": {x:$0}]}"#,
            expect!["Value !1, Value"],
        );
        check_loc(
            r#"{defines: ["x": $0]}"#,
            expect!["Defines !1, Defines"],
        );
    }

    #[test]
    fn test_completions() {
        check(r#"{$0}"#, expect![[r#"
            name
            hide
            ignoreCase
            styles
            comment
            bracketPairs
            lineBackground
            defines
            contains
            codeFormatter
            codeShrinker
        "#]]);
        check(r#"$0"#, expect![""]);
        check(r#"{name: $0}"#, expect![]);
        check(r#"{name: [$0]}"#, expect![""]);
    }

    #[test]
    fn test_trim_complete_sides() {
        check_complete(
            r#"{
                contains: [
                    {0: $0}
                ]
            }"#,
            expect![[r#"
                default             "\"default\""
                string              "\"string\""
                strEscape           "\"strEscape\""
                comment             "\"comment\""
                meta                "\"meta\""
                number              "\"number\""
                keyword             "\"keyword\""
                keyword2            "\"keyword2\""
                constant            "\"constant\""
                type                "\"type\""
                label               "\"label\""
                variable            "\"variable\""
                operator            "\"operator\""
                propKey             "\"propKey\""
                propVal             "\"propVal\""
                tagName             "\"tagName\""
                attrName            "\"attrName\""
                namespace           "\"namespace\""
                error               "\"error\""
            "#]],
        );
        check_complete(
            r#"{
                contains: [
                    {0: "$0"}
                ]
            }"#,
            expect![[r#"
                default             "default"
                string              "string"
                strEscape           "strEscape"
                comment             "comment"
                meta                "meta"
                number              "number"
                keyword             "keyword"
                keyword2            "keyword2"
                constant            "constant"
                type                "type"
                label               "label"
                variable            "variable"
                operator            "operator"
                propKey             "propKey"
                propVal             "propVal"
                tagName             "tagName"
                attrName            "attrName"
                namespace           "namespace"
                error               "error"
            "#]],
        );
        check_complete(
            r#"{
                contains: [
                    {0: "def$0"}
                ]
            }"#,
            expect![[r#"
                default             "default"
                string              "string"
                strEscape           "strEscape"
                comment             "comment"
                meta                "meta"
                number              "number"
                keyword             "keyword"
                keyword2            "keyword2"
                constant            "constant"
                type                "type"
                label               "label"
                variable            "variable"
                operator            "operator"
                propKey             "propKey"
                propVal             "propVal"
                tagName             "tagName"
                attrName            "attrName"
                namespace           "namespace"
                error               "error"
            "#]],
        );
        check_complete(
            r#"{
                defines: [
                    "x": /foo/
                ]
                contains: [
                    {0: include($0)}
                ]
            }"#,
            expect![[r#"
                "x"                 "\"x\""
            "#]],
        );
        check_complete(
            r#"{
                defines: [
                    "x": /foo/
                ]
                contains: [
                    {0: include("$0")}
                ]
            }"#,
            expect![[r#"
                "x"                 "x"
            "#]],
        );
        check_complete(
            r#"{
                defines: [
                    "x": {match: /foo/}
                ]
                contains: [
                    {include: "$0"}
                ]
            }"#,
            expect![[r#"
                "x"                 "x"
            "#]],
        );
        check_complete(
            r#"{
                codeShinker: $0
            }"#,
            expect![[r##"
                BUILT_IN_CSS_SHRINKER"#BUILT_IN_CSS_SHRINKER#"
                BUILT_IN_HTML_SHRINKER"#BUILT_IN_HTML_SHRINKER#"
                BUILT_IN_JSON_SHRINKER"#BUILT_IN_JSON_SHRINKER#"
            "##]],
        );
        check_complete(
            r#"{
                codeShinker: #$0
            }"#,
            expect![[r#"
                BUILT_IN_CSS_SHRINKER"BUILT_IN_CSS_SHRINKER#"
                BUILT_IN_HTML_SHRINKER"BUILT_IN_HTML_SHRINKER#"
                BUILT_IN_JSON_SHRINKER"BUILT_IN_JSON_SHRINKER#"
            "#]],
        );
        check_complete(
            r#"{
                codeShinker: #$0#
            }"#,
            expect![[r#"
                BUILT_IN_CSS_SHRINKER"BUILT_IN_CSS_SHRINKER"
                BUILT_IN_HTML_SHRINKER"BUILT_IN_HTML_SHRINKER"
                BUILT_IN_JSON_SHRINKER"BUILT_IN_JSON_SHRINKER"
            "#]],
        );
    }

    #[test]
    fn test_define_completion() {
        check(
            r#"{
                defines: [
                    "x": /a/
                    // docs
                    //
                    // ...
                    "y": /b/
                    "matcher": {match: /x/}
                    // xxx
                    "matcher1": {match: /x/}
                    "matcher2": [{match: /x/}]
                ]
                contains: [
                    {match: include("$0")}
                ]
            }"#,
            expect![[r#"
                "x"                 "\"x\": /a/"
                "y"                 "// docs\n//\n// ...\n\"y\": /b/"
            "#]],
        );
        check(
            r#"{
                defines: [
                    "x": /a/
                    // docs
                    //
                    // ...
                    "y": /b/
                    "matcher": {match: /x/}
                    // xxx
                    "matcher1": {match: /x/}
                    "matcher2": [{match: /x/}]
                ]
                contains: [
                    {include: "$0"}
                ]
            }"#,
            expect![[r#"
                "matcher"           "\"matcher\": {match: /x/}"
                "matcher1"          "// xxx\n\"matcher1\": {match: /x/}"
                "matcher2"          "\"matcher2\": [{match: /x/}]"
            "#]],
        );
        check(
            r#"{
                defines: [
                    // xxx
                    "x": {match: /a/} // foo
                ]
                contains: [
                    {include: "$0"}
                ]
            }"#,
            expect![[r#"
                "x"                 "// foo\n// xxx\n\"x\": {match: /a/}"
            "#]],
        );
    }
}
