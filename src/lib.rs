use std::{collections::HashSet, time::SystemTime};

use line_column::span::Span;
use lsp_types::{DiagnosticSeverity, InsertTextFormat};
use syntax::{AstNode, AstToken, Direction, NodeOrToken, SyntaxKind, SyntaxNode, SyntaxToken, T, TextRange, TextSize, ast::{self, Or}};
use defs::*;

mod defs;

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

pub fn hover_doc(file: &str, at: lsp_types::Position) -> Option<(String, lsp_types::Range)> {
    let index = srv_index(file, at);
    let span = Span::new_full(file);
    let analysis = Analysis::new(span);
    let cover_range = TextRange::empty(TextSize::from(index as u32));
    analysis.hover_doc(cover_range)
        .map(|(doc, sym)| {
            (doc, lsp_range(&analysis.source.slice(sym.range)))
        })
        .or_else(|| analysis.hover_doc_token(cover_range))
}

pub fn goto_define(file: &str, at: lsp_types::Position) -> Option<lsp_types::Range> {
    let index = srv_index(file, at);
    let span = Span::new_full(file);
    let analysis = Analysis::new(span);
    let cover_range = TextRange::empty(TextSize::from(index as u32));
    analysis.goto_define(cover_range).map(|sym| {
        lsp_range(&analysis.source.slice(sym.range))
    })
}

pub fn references(file: &str, at: lsp_types::Position) -> Option<Vec<lsp_types::Range>> {
    let index = srv_index(file, at);
    let span = Span::new_full(file);
    let analysis = Analysis::new(span);
    let cover_range = TextRange::empty(TextSize::from(index as u32));
    analysis.references(cover_range).map(|ranges| {
        ranges.into_iter().map(|sym| {
            lsp_range(&analysis.source.slice(sym.range))
        }).collect()
    })
}

pub fn rename(file: &str, at: lsp_types::Position, new_name: String) -> Option<Vec<lsp_types::TextEdit>> {
    let index = srv_index(file, at);
    let span = Span::new_full(file);
    let analysis = Analysis::new(span);
    let cover_range = TextRange::empty(TextSize::from(index as u32));
    let ranges = analysis
        .references(cover_range)?;
    let new_text = new_name.trim_matches('"');
    Some(ranges.into_iter().map(|sym| {
        let range = lsp_range(&analysis.source.slice(sym.range));
        lsp_types::TextEdit { range, new_text: new_text.to_owned() }
    }).collect())
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

    fn element(&self, at: TextRange) -> NodeOrToken<SyntaxNode, SyntaxToken> {
        let elem = self.root.syntax().covering_element(at);
        if let NodeOrToken::Token(t) = &elem
            && (t.kind().is_trivia() || t.kind().is_open_delim())
            && t.text_range().end() == at.start()
            && at.is_empty()
            && at.end() < self.root.syntax().text_range().end()
            && let Some(next_token) = t.next_token()
        {
            next_token.into()
        } else {
            elem
        }
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
                Or::A(Or::B(item)) => {
                    if !item.assoc()?.syntax().text_range().contains_range(node.text_range()) {
                        return None;
                    }
                    if item.sep()?.kind() != T![>] {
                        return None;
                    }
                    Location::Color
                },
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
        let elem = self.element(at);
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
        match loc {
            Location::Manifest => {
                let manifest = self.root.table();
                MANIFEST_ATTRS.iter()
                    .filter(|(name, _)| retain_attrs(name, manifest.as_ref()))
                    .map(|(name, snip)| make_item(*name, *snip, ""))
                    .collect()
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
                let user_colors = self.styles().map(|(name, qualifiers)| {
                    let mut range = name.token.text_range();
                    if let Some(last) = qualifiers.last() {
                        range = range.cover(last.syntax().text_range())
                    }
                    let def = &self.source.text()[range];
                    make_item(name.sym_text(), name.token.text(), def)
                });
                let builtins = BUILTIN_COLORS.iter().map(|(name, lg, dk)| {
                    let detail = style_detail(name, lg, dk);
                    make_item(*name, &format!(r#""{name}""#), &detail)
                });
                user_colors.chain(builtins).collect()
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
                    schema.iter()
                        .filter(|(name, _)| retain_attrs(name, Some(&table)))
                        .map(|(label, snip)| {
                            make_item(*label, *snip, "")
                        }).collect()
                } else {
                    MATCHER_SCHEMA.iter().map(|((label, snip), _)| {
                        make_item(*label, *snip, "")
                    }).collect()
                }
            },
            Location::Styles => vec![],
            Location::CommentDef => {
                COMMENT_DEFS.iter().map(|(name, snip)| {
                    make_item(*name, *snip, "")
                }).collect()
            },
            Location::Defines => vec![],
            Location::IncludeRegex => {
                self.defines()
                    .filter(|(_, value)| !is_matcher(value))
                    .map(|(name, value)| {
                        let detail = def_detail(&name, &value);
                        make_item(name.sym_text(), name.token.text(), &detail)
                    })
                    .collect()
            },
            Location::IncludeMatcher => {
                self.defines()
                    .filter(|(_, value)| is_matcher(value))
                    .map(|(name, value)| {
                        let detail = def_detail(&name, &value);
                        make_item(name.sym_text(), name.token.text(), &detail)
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

    fn hover_doc(&self, at: TextRange) -> Option<(String, SymId)> {
        let elem = self.element(at);
        let NodeOrToken::Token(tok) = &elem else { return None };

        if tok.kind() != SyntaxKind::STRING {
            return None;
        }

        let id = SymId::new(tok.clone());
        let loc = self.location(&tok.parent()?);
        let doc = match loc {
            Location::Manifest | Location::Styles | Location::CommentDef
            | Location::Defines | Location::Value | Location::Group
            | Location::BuiltinMatcher | Location::BuiltinFormatter
            | Location::BuiltinShinker | Location::Boolean | Location::Pattern
            | Location::Disabled => return None,
            Location::IncludeRegex => {
                let (name, value) = self.defines().find(|(name, value)| {
                    id == name && !is_matcher(value)
                })?;
                def_detail(&name, &value)
            },
            Location::IncludeMatcher => {
                let (name, value) = self.defines().find(|(name, value)| {
                    id == name && is_matcher(value)
                })?;
                def_detail(&name, &value)
            },
            Location::Color => {
                self.styles()
                    .find(|(name, _)| id == name)
                    .map(|(name, qualifiers)| {
                        let mut range = name.token.text_range();
                        if let Some(last) = qualifiers.last() {
                            range = range.cover(last.syntax().text_range())
                        }
                        let def = &self.source.text()[range];
                        def.to_string()
                    })
                    .or_else(|| {
                        BUILTIN_COLORS.iter()
                            .find(|(name, _, _)| *name == id.sym_text())
                            .map(|(name, lg, dk)| {
                                style_detail(name, lg, dk)
                            })
                    })?
            },
        };
        Some((doc, SymId::new(tok.clone())))
    }

    fn hover_doc_token(&self, at: TextRange) -> Option<(String, lsp_types::Range)> {
        let elem = self.element(at);
        let NodeOrToken::Token(tok) = &elem else { return None };
        let mut query = tok.text();

        if query == "include" && tok.parent().and_then(ast::Call::cast).is_some() {
            query = "include()"
        }

        let (_, doc) = HARD_DOCS.iter().find(|(name, _)| *name == query)?;

        Some((doc.to_string(), lsp_range(&self.source.slice(tok.text_range()))))
    }

    fn goto_define(&self, at: TextRange) -> Option<SymId> {
        let elem = self.element(at);
        let NodeOrToken::Token(tok) = &elem else { return None };

        if tok.kind() != SyntaxKind::STRING {
            return None;
        }

        let id = SymId::new(tok.clone());
        let loc = self.location(&tok.parent()?);

        let text_range = match loc {
            Location::Manifest | Location::CommentDef | Location::Value | Location::Group
            | Location::BuiltinMatcher | Location::BuiltinFormatter | Location::BuiltinShinker
            | Location::Boolean | Location::Pattern | Location::Disabled => return None,
            Location::IncludeRegex => {
                self.defines().find(|(name, value)| {
                    id == name && !is_matcher(value)
                })?.0
            },
            Location::IncludeMatcher => {
                self.defines().find(|(name, value)| {
                    id == name && is_matcher(value)
                })?.0
            },
            Location::Defines => {
                let def = tok.parent_ancestors().find_map(ast::Item::cast).and_then(|it| it.assoc());
                let def_is_matcher = def.as_ref().is_none_or(is_matcher);
                self.defines().find(|(name, value)| {
                    id == name && is_matcher(value) == def_is_matcher
                })?.0
            },
            Location::Color | Location::Styles => {
                self.styles()
                    .find(|(name, _)| id == name)?
                    .0
            },
        };
        Some(text_range)
    }

    fn references(&self, at: TextRange) -> Option<Vec<SymId>> {
        let def_id = self.goto_define(at)?;
        let elem = self.element(def_id.token.text_range());
        let NodeOrToken::Token(def) = &elem else { return None };

        if def.kind() != SyntaxKind::STRING {
            return None;
        }
        let loc = self.location(&elem.ancestors().next()?);
        let assoc = def.parent()
            .and_then(ast::Literal::cast)
            .and_then(|it| it.syntax().parent().and_then(ast::Item::cast))
            .and_then(|it| it.assoc());

        let mut syms = match loc {
            Location::Styles => {
                self.root.syntax()
                    .descendants()
                    .filter_map(ast::Table::cast)
                    .flat_map(|table| table.get(|k| {
                        matches!(k, "style" | "color")
                            || k.chars().all(|ch| ch.is_ascii_digit())
                    }))
                    .filter_map(|value| value.into_literal()?.lit())
                    .chain(self.styles().flat_map(|(_, qualifiers)| qualifiers))
                    .filter_map(ast::Lit::into_string)
                    .map(SymId::new)
                    .filter(|it| it == def_id)
                    .collect()
            },
            Location::Defines if assoc.as_ref().is_some_and(is_matcher) => {
                self.root.syntax()
                    .descendants()
                    .filter_map(ast::Table::cast)
                    .flat_map(|table| table.get(|k| k == "include"))
                    .filter_map(|value| value.into_literal()?.lit()?.into_string())
                    .map(SymId::new)
                    .filter(|it| it == def_id)
                    .collect()
            },
            Location::Defines => {
                self.root.syntax()
                    .descendants()
                    .filter_map(ast::Call::cast)
                    .filter_map(|call| {
                        (call.name()?.into_ident()?.text() == "include").then_some(call)
                    })
                    .flat_map(|call| call.syntax().children_with_tokens())
                    .filter_map(NodeOrToken::into_token)
                    .filter(|it| it.kind() == SyntaxKind::STRING)
                    .map(SymId::new)
                    .filter(|it| it == def_id)
                    .collect()
            },
            _ => return None,
        };
        Vec::push(&mut syms, def_id);
        Some(syms)
    }

    fn get_manifest(&self, name: &str) -> Option<ast::Value> {
        self.root.table()?.get(|k| k == name).next()
    }

    fn styles(&self) -> impl Iterator<Item = (SymId, impl Iterator<Item = ast::Lit>)> {
        self.get_manifest("styles")
            .or_else(|| self.get_manifest("colors"))
            .and_then(ast::Value::into_array)
            .into_iter()
            .flat_map(|arr| arr.items())
            .filter_map(|it| Some((it.value()?.into_literal()?.lit()?.into_string()?, it)))
            .map(|(name, it)| {
                let members_qualifiers = it.syntax()
                    .siblings(Direction::Next)
                    .skip(1)
                    .filter_map(ast::Item::cast)
                    .take_while(|it| it.assoc().is_none())
                    .map_while(|it| it.value()?.into_literal()?.lit())
                    .take_while(|it| !matches!(it, ast::Lit::STRING(_)));
                let values = it.sep()
                    .filter(|it| it.kind() == T![>])
                    .into_iter()
                    .flat_map(move |_| it.assoc().into_iter().filter_map(|it| it.into_literal()?.lit()))
                    .chain(members_qualifiers);
                (SymId::new(name), values)
            })
    }

    fn defines(&self) -> impl Iterator<Item = (SymId, ast::Value)> {
        (|| {
            let ast::Value::Array(defines) = self.get_manifest("defines")? else { return None };
            Some(defines.items()
                .filter_map(|item| {
                    let key = item.value()?.into_literal()?.lit()?.into_string()?;
                    Some((SymId::new(key), item.assoc()?))
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

struct SymId {
    token: SyntaxToken,
    range: TextRange,
}

impl PartialEq for SymId {
    fn eq(&self, other: &Self) -> bool {
        self.sym_text() == other.sym_text()
    }
}
impl PartialEq<&Self> for SymId {
    fn eq(&self, other: &&Self) -> bool {
        self == *other
    }
}
impl PartialEq<SymId> for &SymId {
    fn eq(&self, other: &SymId) -> bool {
        *self == other
    }
}

impl SymId {
    fn new(token: SyntaxToken) -> Self {
        let (_, range) = extract_style(&token);
        Self { token, range }
    }

    fn sym_text(&self) -> &str {
        let offset = self.token.text_range().start();
        &self.token.text()[self.range - offset]
    }
}

fn extract_style(tok: &SyntaxToken) -> (&str, TextRange) {
    let mut range = tok.text_range();
    let mut text = tok.text();

    if tok.kind() != SyntaxKind::STRING {
        return (text, range);
    }

    macro_rules! eat_start {
        ($prefix:expr) => {
            if text.starts_with($prefix) {
                range = TextRange::new(range.start() + TextSize::of($prefix), range.end());
                text = &text[$prefix.len()..];
                true
            } else {
                false
            }
        };
    }
    macro_rules! eat_end {
        ($prefix:expr) => {
            if text.ends_with($prefix) {
                range = TextRange::new(range.start(), range.end() - TextSize::of($prefix));
                text = &text[..text.len()-$prefix.len()];
                true
            } else {
                false
            }
        };
    }

    eat_start!("\"");
    eat_end!("\"");

    if eat_start!("parseColor(") {
        while eat_end!(")") || eat_end!(",") || eat_end!(" ") {}

        if let Some((pre, _)) = text.rsplit_once(',') {
            eat_start!(pre);
            eat_start!(",");
            while eat_start!(" ") {}
        }
    }

    (text, range)
}

#[test]
fn test_extract_style() {
    let datas = [
        (r#""foo"#, "foo"),
        (r#""foo""#, "foo"),
        (r#""parseColor(2, _, HEX, default)""#, "default"),
        (r#""parseColor(2, _, HEX, default )""#, "default"),
        (r#""parseColor(2, _, HEX, default , )""#, "default"),
    ];
    for (src, expect) in datas {
        let source = Span::new_full(format!("it {src}"));
        let (root, _) = syntax::parse_file(source.clone());
        let tok = root.syntax()
            .descendants_with_tokens()
            .find(|it| it.kind() == SyntaxKind::STRING)
            .unwrap()
            .into_token()
            .unwrap();
        assert_eq!(tok.text(), src);
        let extracted = extract_style(&tok);
        let out = source.slice(extracted.1);
        assert_eq!(out.text(), extracted.0);
        assert_eq!(out.text(), expect);
    }
}

fn is_matcher(value: &ast::Value) -> bool {
    matches!(value, ast::Value::Table(_) | ast::Value::Array(_))
}

fn def_detail(name: &SymId, value: &ast::Value) -> String {
    format!(
        "{}{}: {}",
        docs(&name),
        name.token,
        dedent(&value.to_string(), indent_spaces(value.syntax().clone()))
    )
}

fn retain_attrs(name: &str, table: Option<&ast::Table>) -> bool {
    if ALLOW_DUP_KEYS.contains(&name) {
        return true;
    }
    table.is_none_or(|table| {
        table.get(|it| it == name).next().is_none()
    })
}

fn style_detail(name: &str, lg: &str, dk: &str) -> String {
    let dname = format!("{name:?}");
    format!("{dname:<15}{lg}     {dk}")
}

fn docs(name: &SymId) -> String {
    (|| {
        let def = name.token.parent_ancestors().find_map(Or::<ast::Pair, ast::Item>::cast)?;

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

fn indent_spaces(node: impl Into<NodeOrToken<SyntaxNode, SyntaxToken>>) -> usize {
    let first = match node.into() {
        NodeOrToken::Node(n) => n.first_token(),
        NodeOrToken::Token(t) => Some(t),
    };
    fn get_indent(text: &str) -> usize {
        let (_, indent) = text.rsplit_once('\n').unwrap();
        indent.len()
    }
    std::iter::successors(first, |it| it.prev_token())
        .filter(|it| it.kind() == SyntaxKind::WHITESPACE)
        .find(|it| it.text().contains('\n'))
        .map_or(0, |tok| get_indent(tok.text()))
}

fn dedent(s: &str, spaces: usize) -> String {
    s.split_inclusive('\n')
        .map(|line| {
            let pure = line.trim_ascii_start();
            let indent = line.len() - pure.len();
            if indent < spaces {
                pure
            } else {
                &line[spaces..]
            }
        })
        .collect()
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
            let element = analysis.element(TextRange::empty(TextSize::new(index.try_into().unwrap())));
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
    fn check_hover(src: &str, expect: Expect) {
        let index = src.find("$0").expect("must a `$0`");
        let src = src.replacen("$0", "", 1);
        let src = Span::new_full(src);
        let pos = lsp_pos(&src.create(TextRange::empty(TextSize::new(index.try_into().unwrap()))));
        let (hover, _) = hover_doc(src.source(), pos).expect("hover is none");
        expect.assert_eq(&format!("{hover}\n"));
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

    #[track_caller]
    fn check_goto_define(src: &str, expect: Expect) {
        let index = src.find("$0").expect("must a `$0`");
        let src = src.replacen("$0", "", 1);
        let src = Span::new_full(src);
        let pos = lsp_pos(&src.create(TextRange::empty(TextSize::new(index.try_into().unwrap()))));
        let lsp_types::Range { start: lsp_types::Position { mut line, mut character }, .. } = goto_define(src.source(), pos).expect("hover is none");
        line += 1;
        character += 1;
        expect.assert_eq(&format!("{line}:{character}"));
    }

    #[track_caller]
    fn check_references(src: &str, expect: Expect) {
        let index = src.find("$0").expect("must a `$0`");
        let src = src.replacen("$0", "", 1);
        let src = Span::new_full(src);
        let pos = lsp_pos(&src.create(TextRange::empty(TextSize::new(index.try_into().unwrap()))));
        let refs = references(src.source(), pos).expect("references is none");
        let refs = refs.into_iter()
            .map(|lsp_types::Range { start: lsp_types::Position { line, character }, .. }| {
                (line+1, character+1)
            })
            .map(|(line, character)| format!("{line}:{character}\n"))
            .collect::<String>();
        expect.assert_eq(&refs);
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
            r#"{styles: ["red" > $0]}"#,
            expect!["Styles !1, Color"],
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
            r#"{contains: [{include: $0}]}"#,
            expect!["Value !1, IncludeMatcher"],
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
        check(r#"{comment: {$0}}"#, expect![[r#"
            startsWith
            endsWith
            insertSpace
            addToContains
        "#]]);
        check(r#"$0"#, expect![""]);
        check(r#"{name: $0}"#, expect![]);
        check(r#"{name: [$0]}"#, expect![""]);
    }

    #[test]
    fn test_completions_dedup() {
        check(r#"{
            name: ["rust", ".rs"]
            hide: false
            comment: {startsWith: "//"}
            $0
        }"#, expect![[r#"
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
        check(r#"{
            contains: [
                {
                    start: {match: /x/}
                    end: {match: /y/}
                    matchEndFirst: true
                    $0
                }
            ]
        }"#, expect![[r#"
            style
            childrenStyle
            endPriority
            mustMatchEnd
            contains
        "#]]);
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
                x                   "\"x\""
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
                x                   "x"
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
                x                   "x"
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
                x                   "\"x\": /a/"
                y                   "// docs\n//\n// ...\n\"y\": /b/"
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
                matcher             "\"matcher\": {match: /x/}"
                matcher1            "// xxx\n\"matcher1\": {match: /x/}"
                matcher2            "\"matcher2\": [{match: /x/}]"
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
                x                   "// foo\n// xxx\n\"x\": {match: /a/}"
            "#]],
        );
    }

    #[test]
    fn test_style_completion() {
        check(
            r#"{
                styles: [
                    "red", #ff0000#303030, #dd0000
                    "custom" > "red", @BI
                ]
                contains: [
                    {match: /x/, 0: $0}
                ]
            }"#,
            expect![[r#"
                red                 "\"red\", #ff0000#303030, #dd0000"
                custom              "\"custom\" > \"red\", @BI"
                default             "\"default\"      #000000     #A9B7C6"
                string              "\"string\"       #067D17     #6A8759"
                strEscape           "\"strEscape\"    #0037A6     #CC7832"
                comment             "\"comment\"      #8C8C8C     #808080"
                meta                "\"meta\"         #9E880D     #BBB529"
                number              "\"number\"       #1750EB     #6897BB"
                keyword             "\"keyword\"      #0033B3     #CC7832"
                keyword2            "\"keyword2\"     #800000     #AE8ABE"
                constant            "\"constant\"     #871094     #9876AA"
                type                "\"type\"         #808000     #808000"
                label               "\"label\"        #7050E0     #6080B0"
                variable            "\"variable\"     #1750EB     #58908A"
                operator            "\"operator\"     #205060     #508090"
                propKey             "\"propKey\"      #083080     #CC7832"
                propVal             "\"propVal\"      #067D17     #6A8759"
                tagName             "\"tagName\"      #0030B3     #E8BF6A"
                attrName            "\"attrName\"     #174AD4     #BABABA"
                namespace           "\"namespace\"    #871094     #9876AA"
                error               "\"error\"        #F50000     #BC3F3C"
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
                matcher             "\"matcher\": {match: /x/}"
                matcher1            "// xxx\n\"matcher1\": {match: /x/}"
                matcher2            "\"matcher2\": [{match: /x/}]"
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
                x                   "// foo\n// xxx\n\"x\": {match: /a/}"
            "#]],
        );
    }

    #[test]
    fn test_docs_indent() {
        check(
            r#"{
                defines: [
                    // docs
                    //   * docs
                    "y": {
                        match: /x/
                        0: "red"
                    }
                ]
                contains: [
                    {include: $0}
                ]
            }"#,
            expect![[r#"
                y                   "// docs\n//   * docs\n\"y\": {\n    match: /x/\n    0: \"red\"\n}"
            "#]],
        );
    }

    #[test]
    fn test_hover_docs() {
        check_hover(
            r#"{
                defines: [
                    // docs
                    //   * docs
                    "y": {
                        match: /x/
                        0: "red"
                    }
                ]
                contains: [
                    {include: "y$0"}
                ]
            }"#,
            expect![[r#"
                // docs
                //   * docs
                "y": {
                    match: /x/
                    0: "red"
                }
            "#]],
        );
        check_hover(
            r#"{
                defines: [
                    // docs
                    //   * docs
                    "y": /foo/
                ]
                contains: [
                    {match: include("y$0")}
                ]
            }"#,
            expect![[r#"
                // docs
                //   * docs
                "y": /foo/
            "#]],
        );
        check_hover(
            r#"{
                styles: [
                    "red" > "blue" @BI
                ]
                contains: [
                    {match: /a/, 0: "red$0"}
                ]
            }"#,
            expect![[r#"
                "red" > "blue" @BI
            "#]],
        );
        check_hover(
            r#"{
                styles: [
                    "red" > "blue" @BI
                    "hint" > "red$0"
                ]
            }"#,
            expect![[r#"
                "red" > "blue" @BI
            "#]],
        );
        check_hover(
            r#"{
                contains: [
                    {match: /a/, 0: "string$0"}
                ]
            }"#,
            expect![[r#"
                "string"       #067D17     #6A8759
            "#]],
        );
    }

    #[test]
    fn test_hover_hard_docs() {
        check_hover(
            r#"{
                $0contains: []
            }"#,
            expect![[r#"
                用于指定子匹配器
            "#]],
        );
        check_hover(
            r#"{
                contains: [{$0start: {match: /x/}}]
            }"#,
            expect![[r#"
                start-end 匹配器
                匹配头匹配器, 然后让子匹配器和尾匹配器竟争匹配
            "#]],
        );
        check_hover(
            r#"{
                contains: [{include$0: "foo"}]
            }"#,
            expect![[r#"
                include 匹配器
                引用匹配器, 可以互递归
            "#]],
        );
        check_hover(
            r#"{
                contains: [{match: $0include("foo")}]
            }"#,
            expect![[r#"
                include 函数
                将 defines: [] 块中的 "name": /regex/ 内联
            "#]],
        );
        check_hover(
            r#"{
                contains: [
                    {match: include("foo")} => $0FAIL
                ]
            }"#,
            expect![[r#"
                Fail 标记
                用于指定 contains 中的某个子匹配器匹配成功会使得整个 start-end 匹配器匹配失败。
            "#]],
        );
    }

    #[test]
    fn test_goto_define() {
        check_goto_define(
            r#"{
                defines: [
                    "a": /x/
                    "a": {match: /x/}
                ]
                contains: [
                    {include: "a$0"}
                ]
            }"#,
            expect!["4:22"],
        );
        check_goto_define(
            r#"{
                defines: [
                    "a": /x/
                    "a": {match: /x/}
                ]
                contains: [
                    {match: include("a$0")}
                ]
            }"#,
            expect!["3:22"],
        );
        check_goto_define(
            r#"{
                styles: [
                    "a" > "red"
                ]
                contains: [
                    {match: /x/ 0: "a$0"}
                ]
            }"#,
            expect!["3:22"],
        );
        check_goto_define(
            r#"{
                styles: [
                    "red" > "string"
                    "a" > "red$0"
                ]
            }"#,
            expect!["3:22"],
        );
        check_goto_define(
            r#"{
                styles: [
                    "$0red" > "string"
                ]
            }"#,
            expect!["3:22"],
        );
        check_goto_define(
            r#"{
                defines: [
                    "$0red": /x/
                ]
            }"#,
            expect!["3:22"],
        );
        check_goto_define(
            r#"{
                defines: [
                    "$0red": {match: /x/}
                ]
            }"#,
            expect!["3:22"],
        );
    }

    #[test]
    fn test_goto_references() {
        check_references(
            r#"{
                styles: [
                    "a" > "red"
                    "x" > "a"
                ]
                defines: [
                    "a": /x/
                    $0"a": {match: /x/, 0: "a"}
                    "foo": {start: {match: /x/}, style: "a"}
                ]
                contains: [
                    {include: "a"}
                    {match: include("a")}
                ]
            }"#,
            expect![[r#"
                12:32
                8:22
            "#]],
        );
        check_references(
            r#"{
                styles: [
                    "a" > "red"
                    "x" > "a"
                ]
                defines: [
                    $0"a": /x/
                    "a": {match: /x/, 0: "a"}
                    "foo": {start: {match: /x/}, style: "a"}
                ]
                contains: [
                    {include: "a"}
                    {match: include("a")}
                ]
            }"#,
            expect![[r#"
                13:38
                7:22
            "#]],
        );
        check_references(
            r#"{
                styles: [
                    $0"a" > "red"
                    "x" > "a"
                ]
                defines: [
                    "a": /x/
                    "a": {match: /x/, 0: "a"}
                    "foo": {start: {match: /x/}, style: "a"}
                ]
                contains: [
                    {include: "a"}
                    {match: include("a")}
                ]
            }"#,
            expect![[r#"
                8:43
                9:58
                4:28
                3:22
            "#]],
        );
        check_references(
            r#"{
                styles: [
                    $0"a" > "red"
                    "x" > "a"
                ]
                defines: [
                    "a": /x/
                    "a": {match: /x/, 0: "a"}
                    "foo": {start: {match: /x/}, style: "a"}
                    "bar": {start: {match: /x/}, style: "parseColor(a, a, a, a)"}
                ]
                contains: [
                    {include: "a"}
                    {match: include("a")}
                ]
            }"#,
            expect![[r#"
                8:43
                9:58
                10:78
                4:28
                3:22
            "#]],
        );
    }
}
