use either::Either::{Left, Right};
use rowan::{GreenNode, GreenNodeBuilder, GreenNodeData, GreenToken, NodeOrToken, TextRange, TextSize};

use crate::{SyntaxKind, SyntaxNode, T, lexer::Lexer};
use SyntaxKind::*;

// SOURCE_FILE  = TABLE
// TABLE        = "{" PAIR* "}"
// PAIR         = key ":" value [","]
// key          = IDENT | NUMBER
// value        = JOIN | TABLE | ARRAY | BUILTIN | STRING | NUMBER | REGEX | MARK | COLOR | CALL | IDENT
// JOIN         = value "+" value
// ARRAY        = "{" ITEM "}"
// ITEM         = value [">"] [":" value] [","]
// CALL         = IDENT "(" *(STRING [","]) ")"
pub struct Parser<'input> {
    lexer: Lexer<'input>,
    source: &'input str,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<(TextRange, String)>,
}

impl<'input> Parser<'input> {
    pub fn new(source: &'input str) -> Self {
        Self {
            lexer: Lexer::new(source),
            source,
            errors: vec![],
            builder: GreenNodeBuilder::new(),
        }
    }

    fn bump_trivias(&mut self) {
        while let Some((kind, text)) = self.lexer.current()
            && kind.is_trivia()
        {
            self.builder.token(kind.into(), text);
            self.lexer.bump_any();
        }
    }

    #[track_caller]
    fn bump(&mut self, kind: SyntaxKind) {
        let Some((current, text)) = self.lexer.current() else { panic!("bump eof") };
        assert_eq!(current, kind);
        assert!(!kind.is_trivia(), "{kind:?}");

        self.builder.token(kind.into(), text);
        self.lexer.bump_any();
    }

    fn current(&mut self) -> SyntaxKind {
        self.bump_trivias();
        self.lexer.kind().unwrap_or(SyntaxKind::ERROR)
    }

    fn is_eof(&mut self) -> bool {
        self.current() == ERROR
            && self.lexer.kind().is_none()
    }

    #[must_use]
    fn mark(&mut self) -> rowan::Checkpoint {
        self.bump_trivias();
        self.builder.checkpoint()
    }

    fn node(&mut self, kind: SyntaxKind, mark: rowan::Checkpoint) {
        self.builder.start_node_at(mark, kind.into());
        self.builder.finish_node();
    }

    fn bump_any(&mut self) {
        let current = self.current();
        self.bump(current);
    }

    fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.current() == kind {
            self.bump(kind);
            true
        } else {
            false
        }
    }

    #[expect(unused)]
    fn bump_expect(&mut self, kind: SyntaxKind) -> bool {
        if self.current() == kind {
            self.bump(kind);
            true
        } else {
            self.bump_error(format_args!("expected {}", kind.human_readable()));
            false
        }
    }

    fn bump_or_expect(&mut self, kind: SyntaxKind) -> bool {
        let current = self.current();
        if current == kind {
            self.bump(kind);
            true
        } else {
            self.report_error(format_args!("expected {}", kind.human_readable()));
            false
        }
    }

    fn report_error(&mut self, msg: impl ToString) {
        let text = self.lexer.token().unwrap_or("");
        let index = TextSize::try_from(self.lexer.index()).unwrap();
        let range = TextRange::at(index, TextSize::of(text));

        self.errors.push((range, msg.to_string()));
    }

    fn bump_error(&mut self, msg: impl ToString) {
        let mark = self.mark();
        let kind = self.current();
        self.report_error(msg);
        self.bump(kind);
        self.node(ERROR, mark);
    }
}

impl<'input> Parser<'input> {
    fn value(&mut self) {
        let mark = self.mark();

        for nr in 0.. {
            let kind = self.current();
            match kind {
                L_CURLY => self.table(),
                L_BRACK => self.array(),
                R_CURLY | R_BRACK | R_PAREN => {
                    self.report_error(format_args!("unexpected {}, expected value", kind.human_readable()));
                }
                IDENT => self.ident_or_call(),
                BUILTIN | STRING | NUMBER | REGEX | MARK | COLOR => self.bump(kind),
                _ => {
                    self.bump_error(format_args!("unexpected {}, expected value", kind.human_readable()));
                }
            }
            if nr != 0 {
                self.node(JOIN, mark);
            }
            if !self.eat(T![+]) {
                break;
            }
        }
    }

    fn table(&mut self) {
        let mark = self.mark();
        self.bump(L_CURLY);

        while matches!(self.current(), IDENT | STRING | NUMBER) {
            self.pair();
        }

        self.bump_or_expect(R_CURLY);
        self.node(TABLE, mark);
    }

    fn pair(&mut self) {
        let mark = self.mark();
        self.bump_any();
        if self.bump_or_expect(T![:]) {
            self.value();
        }
        self.eat(T![,]);
        self.node(PAIR, mark);
    }

    fn array(&mut self) {
        self.bump(L_BRACK);
        while self.current() != R_BRACK {
            self.item();
        }
        self.bump_or_expect(R_BRACK);
    }

    fn ident_or_call(&mut self) {
        let mark = self.mark();
        self.bump(IDENT);

        if self.eat(L_PAREN) {
            while self.eat(STRING) {
                self.eat(T![,]);
            }
            self.bump_or_expect(R_PAREN);
            self.node(CALL, mark);
        }
    }

    fn item(&mut self) {
        let mark = self.mark();
        self.value();
        self.eat(T![>]);

        if self.eat(T![:]) {
            self.value();
        }

        self.eat(T![,]);
        self.node(ITEM, mark);
    }

    pub fn source_file(&mut self) {
        let mark = self.builder.checkpoint();

        while self.current() != L_CURLY {
            let tok = self.current().human_readable();
            self.bump_error(format_args!("unexpected {tok}, expected `{{`"));
        }

        if self.current() == L_CURLY {
            self.table();
        } else {
            self.report_error("expected `{`");
        }

        while !self.is_eof() {
            let tok = self.current().human_readable();
            self.bump_error(format_args!("unexpected {tok}, expected end of file"));
        }

        let _mark_end = self.mark();
        self.node(SOURCE_FILE, mark);
    }

    pub fn finish(self) -> (SyntaxNode, Vec<(TextRange, String)>) {
        let green_node = self.builder.finish();
        let clean_node = clean_childs_trivias(&green_node);
        let node = SyntaxNode::new_root(clean_node);
        assert_eq!(node.text_range().len(), TextSize::of(self.source), "{node:#?}");
        (node, self.errors)
    }
}

fn clean_childs_trivias(node: &GreenNodeData) -> GreenNode {
    let children = node.children()
        .flat_map(|child| match child {
            NodeOrToken::Node(node) => Left(clean_trivias(node)),
            NodeOrToken::Token(tok) => Right([tok.to_owned().into()]),
        }.into_iter())
        .collect::<Vec<_>>();
    GreenNode::new(node.kind(), children)
}

fn clean_trivias(node: &GreenNodeData) -> impl Iterator<Item = NodeOrToken<GreenNode, GreenToken>> {
    let is_solid = |it: NodeOrToken<&GreenNodeData, &rowan::GreenTokenData>| {
        !SyntaxKind::from(it.kind()).is_trivia()
    };
    let Some(first_solid) = node
        .children()
        .position(is_solid)
    else {
        // 对于全由trivia组成的节点, 调整其打开后trivia在前还是后
        let iter = node.children()
            .map(NodeOrToken::to_owned)
            .chain([GreenNode::new(node.kind(), []).into()]);
        return Left(iter);
    };
    let last_solid = node.children().rposition(is_solid).unwrap();

    let first = node.children().take(first_solid);
    let last = node.children().skip(last_solid + 1);
    let children = node.children().take(last_solid + 1).skip(first_solid);
    let trimmed = children.flat_map(|child| {
        match child {
            NodeOrToken::Node(node) => Left(clean_trivias(node)),
            NodeOrToken::Token(tok) => Right([tok.to_owned().into()]),
        }.into_iter()
    });

    Right(first.map(NodeOrToken::to_owned)
        .chain([GreenNode::new(node.kind(), trimmed.collect::<Vec<_>>()).into()])
        .chain(last.map(NodeOrToken::to_owned)))
}

#[cfg(test)]
mod tests {
    use std::fmt::Write;
    use expect_test::{Expect, expect};

    use super::*;

    fn clean_inline_lints(src: &str) -> String {
        let has_eol = src.trim_end_matches(' ').ends_with('\n');
        let mut output: String = src.split_inclusive('\n')
            .filter(|line| !line.trim_start().starts_with("// ^"))
            .collect();
        if !has_eol && output.chars().next_back() == Some('\n') {
            output.pop().unwrap();
        }
        output
    }

    #[track_caller]
    fn apply_inline_lints(src: &str, mut lints: Vec<(TextRange, String)>) -> String {
        lints.sort_by_key(|(range, lint)| {
            (range.start(), range.len(), lint.len())
        });
        let mut output = String::new();
        let mut real_range = TextRange::empty(TextSize::new(0));

        for line in src.split_inclusive('\n') {
            let line_range = TextRange::at(real_range.end(), TextSize::of(line));
            output += line;

            let has_eol = line.chars().next_back() == Some('\n');
            if !has_eol {
                output += "\n";
            }

            for (range, lint) in &lints {
                if !line_range.contains_range(*range) {
                    continue;
                }
                let inline = range - line_range.start();
                let column = inline.start();
                let Some(place) = column.checked_sub(TextSize::of("// ")) else {
                    panic!("inline lint on column {column:?} is unsupported (`{lint}`)")
                };
                let indent = " ".repeat(place.into());
                let arrow = "^".repeat(inline.len().into());
                writeln!(output, "{indent}// {arrow} {lint}").unwrap();
            }

            if !has_eol && output.ends_with('\n') {
                output.pop().unwrap();
            }

            real_range = real_range.cover(line_range);
        }

        output
    }

    #[test]
    fn test_clean_inline_lints() {
        let src = r#"
            foo bar baz
             // ^^^ at bar error
                 // ^^^ at baz error
        "#;
        expect![[r#"
            "\n            foo bar baz\n        "
        "#]].assert_debug_eq(&clean_inline_lints(src));
        expect![[r#"
            ""
        "#]].assert_debug_eq(&clean_inline_lints(""));
        expect![[r#"
            "a"
        "#]].assert_debug_eq(&clean_inline_lints("a"));
        expect![[r#"
            "a\n"
        "#]].assert_debug_eq(&clean_inline_lints("a\n"));
    }

    #[test]
    fn test_apply_inline_lints() {
        let src = "foo bar baz\nxxx yyy";
        let lints = [
            (
                TextRange::at(TextSize::of("foo "), TextSize::of("bar")),
                "bar lint",
            ),
            (
                TextRange::at(TextSize::of("foo "), TextSize::of("b")),
                "bar sub lint",
            ),
            (
                TextRange::at(TextSize::of("foo bar "), TextSize::of("baz")),
                "baz lint",
            ),
            (
                TextRange::at(TextSize::of("foo bar baz\nxxx "), TextSize::of("yyy")),
                "yyy lint",
            ),
        ].map(|(rng, lint)| (rng, lint.to_owned())).to_vec();
        expect![[r#"
            foo bar baz
             // ^ bar sub lint
             // ^^^ bar lint
                 // ^^^ baz lint
            xxx yyy
             // ^^^ yyy lint"#]].assert_eq(&apply_inline_lints(src, lints));
        expect![[r#"
            ""
        "#]].assert_debug_eq(&apply_inline_lints("", vec![]));
        expect![[r#"
            "a"
        "#]].assert_debug_eq(&apply_inline_lints("a", vec![]));
        expect![[r#"
            "a\n"
        "#]].assert_debug_eq(&apply_inline_lints("a\n", vec![]));
    }

    #[track_caller]
    fn check(src: &str, expect: Expect) {
        let clean_src = &clean_inline_lints(src);

        let mut parser = Parser::new(clean_src);
        parser.source_file();
        let (node, errors) = parser.finish();

        expect.assert_eq(&format!("{node:#?}"));
        let linted_src = apply_inline_lints(clean_src, errors);
        if linted_src != src {
            print_chunks(dissimilar::diff(src, &linted_src));
            panic!("lints is not matched\n left: {src:?}\nright: {linted_src:?}")
        }
    }

    fn print_chunks(chunks: Vec<dissimilar::Chunk>) {
        for chunk in chunks {
            let formatted = match chunk {
                dissimilar::Chunk::Equal(text) => text.into(),
                dissimilar::Chunk::Delete(text) => format!("\x1b[41m{}\x1b[0m", text),
                dissimilar::Chunk::Insert(text) => format!("\x1b[42m{}\x1b[0m", text),
            };
            print!("{formatted}")
        }
    }

    #[test]
    fn parse_source_file() {
        check(r#"{}"#, expect![[r#"
            SOURCE_FILE@0..2
              TABLE@0..2
                L_CURLY@0..1 "{"
                R_CURLY@1..2 "}"
        "#]]);
        check(r#" { } "#, expect![[r#"
            SOURCE_FILE@0..5
              WHITESPACE@0..1 " "
              TABLE@1..4
                L_CURLY@1..2 "{"
                WHITESPACE@2..3 " "
                R_CURLY@3..4 "}"
              WHITESPACE@4..5 " "
        "#]]);
    }

    #[test]
    fn parse_table() {
        check(
            r#"{
                match: {}
                0: {},
                "string": {}
            }"#,
            expect![[r#"
                SOURCE_FILE@0..93
                  TABLE@0..93
                    L_CURLY@0..1 "{"
                    WHITESPACE@1..18 "\n                "
                    PAIR@18..27
                      IDENT@18..23 "match"
                      COLON@23..24 ":"
                      WHITESPACE@24..25 " "
                      TABLE@25..27
                        L_CURLY@25..26 "{"
                        R_CURLY@26..27 "}"
                    WHITESPACE@27..44 "\n                "
                    PAIR@44..50
                      NUMBER@44..45 "0"
                      COLON@45..46 ":"
                      WHITESPACE@46..47 " "
                      TABLE@47..49
                        L_CURLY@47..48 "{"
                        R_CURLY@48..49 "}"
                      COMMA@49..50 ","
                    WHITESPACE@50..67 "\n                "
                    PAIR@67..79
                      STRING@67..75 "\"string\""
                      COLON@75..76 ":"
                      WHITESPACE@76..77 " "
                      TABLE@77..79
                        L_CURLY@77..78 "{"
                        R_CURLY@78..79 "}"
                    WHITESPACE@79..92 "\n            "
                    R_CURLY@92..93 "}"
            "#]],
        );
        check(
            r#"{
                missing:
            }
         // ^ unexpected `}`, expected value"#,
            expect![[r#"
                SOURCE_FILE@0..40
                  TABLE@0..40
                    L_CURLY@0..1 "{"
                    WHITESPACE@1..18 "\n                "
                    PAIR@18..26
                      IDENT@18..25 "missing"
                      COLON@25..26 ":"
                    WHITESPACE@26..39 "\n            "
                    R_CURLY@39..40 "}"
            "#]],
        );
    }

    #[test]
    fn parse_join() {
        check(
            r#"{
                match: {} + {}
                and: {} + {} + {}
            }"#,
            expect![[r#"
                SOURCE_FILE@0..80
                  TABLE@0..80
                    L_CURLY@0..1 "{"
                    WHITESPACE@1..18 "\n                "
                    PAIR@18..32
                      IDENT@18..23 "match"
                      COLON@23..24 ":"
                      WHITESPACE@24..25 " "
                      JOIN@25..32
                        TABLE@25..27
                          L_CURLY@25..26 "{"
                          R_CURLY@26..27 "}"
                        WHITESPACE@27..28 " "
                        PLUS@28..29 "+"
                        WHITESPACE@29..30 " "
                        TABLE@30..32
                          L_CURLY@30..31 "{"
                          R_CURLY@31..32 "}"
                    WHITESPACE@32..49 "\n                "
                    PAIR@49..66
                      IDENT@49..52 "and"
                      COLON@52..53 ":"
                      WHITESPACE@53..54 " "
                      JOIN@54..66
                        JOIN@54..61
                          TABLE@54..56
                            L_CURLY@54..55 "{"
                            R_CURLY@55..56 "}"
                          WHITESPACE@56..57 " "
                          PLUS@57..58 "+"
                          WHITESPACE@58..59 " "
                          TABLE@59..61
                            L_CURLY@59..60 "{"
                            R_CURLY@60..61 "}"
                        WHITESPACE@61..62 " "
                        PLUS@62..63 "+"
                        WHITESPACE@63..64 " "
                        TABLE@64..66
                          L_CURLY@64..65 "{"
                          R_CURLY@65..66 "}"
                    WHITESPACE@66..79 "\n            "
                    R_CURLY@79..80 "}"
            "#]],
        );
    }

    #[test]
    fn parse_array() {
        check(
            r#"{
                defines: ["x": /a/]
                contains: [{x: 2}]
            }"#,
            expect![[r#"
                SOURCE_FILE@0..86
                  TABLE@0..86
                    L_CURLY@0..1 "{"
                    WHITESPACE@1..18 "\n                "
                    PAIR@18..37
                      IDENT@18..25 "defines"
                      COLON@25..26 ":"
                      WHITESPACE@26..27 " "
                      L_BRACK@27..28 "["
                      ITEM@28..36
                        STRING@28..31 "\"x\""
                        COLON@31..32 ":"
                        WHITESPACE@32..33 " "
                        REGEX@33..36 "/a/"
                      R_BRACK@36..37 "]"
                    WHITESPACE@37..54 "\n                "
                    PAIR@54..72
                      IDENT@54..62 "contains"
                      COLON@62..63 ":"
                      WHITESPACE@63..64 " "
                      L_BRACK@64..65 "["
                      ITEM@65..71
                        TABLE@65..71
                          L_CURLY@65..66 "{"
                          PAIR@66..70
                            IDENT@66..67 "x"
                            COLON@67..68 ":"
                            WHITESPACE@68..69 " "
                            NUMBER@69..70 "2"
                          R_CURLY@70..71 "}"
                      R_BRACK@71..72 "]"
                    WHITESPACE@72..85 "\n            "
                    R_CURLY@85..86 "}"
            "#]],
        );
    }

    #[test]
    fn parse_values() {
        check(
            r#"{
                match: /a/ + "b" + include("foo") + keywordsToRegex("foo bar" "baz",)
                format: #BUILTIN#
                color: [
                    #ff0000 > "color"
                ]
            }"#,
            expect![[r##"
                SOURCE_FILE@0..216
                  TABLE@0..216
                    L_CURLY@0..1 "{"
                    WHITESPACE@1..18 "\n                "
                    PAIR@18..87
                      IDENT@18..23 "match"
                      COLON@23..24 ":"
                      WHITESPACE@24..25 " "
                      JOIN@25..87
                        JOIN@25..51
                          JOIN@25..34
                            REGEX@25..28 "/a/"
                            WHITESPACE@28..29 " "
                            PLUS@29..30 "+"
                            WHITESPACE@30..31 " "
                            STRING@31..34 "\"b\""
                          WHITESPACE@34..35 " "
                          PLUS@35..36 "+"
                          WHITESPACE@36..37 " "
                          CALL@37..51
                            IDENT@37..44 "include"
                            L_PAREN@44..45 "("
                            STRING@45..50 "\"foo\""
                            R_PAREN@50..51 ")"
                        WHITESPACE@51..52 " "
                        PLUS@52..53 "+"
                        WHITESPACE@53..54 " "
                        CALL@54..87
                          IDENT@54..69 "keywordsToRegex"
                          L_PAREN@69..70 "("
                          STRING@70..79 "\"foo bar\""
                          WHITESPACE@79..80 " "
                          STRING@80..85 "\"baz\""
                          COMMA@85..86 ","
                          R_PAREN@86..87 ")"
                    WHITESPACE@87..104 "\n                "
                    PAIR@104..121
                      IDENT@104..110 "format"
                      COLON@110..111 ":"
                      WHITESPACE@111..112 " "
                      BUILTIN@112..121 "#BUILTIN#"
                    WHITESPACE@121..138 "\n                "
                    PAIR@138..202
                      IDENT@138..143 "color"
                      COLON@143..144 ":"
                      WHITESPACE@144..145 " "
                      L_BRACK@145..146 "["
                      WHITESPACE@146..167 "\n                    "
                      ITEM@167..176
                        COLOR@167..174 "#ff0000"
                        WHITESPACE@174..175 " "
                        R_ANGLE@175..176 ">"
                      WHITESPACE@176..177 " "
                      ITEM@177..184
                        STRING@177..184 "\"color\""
                      WHITESPACE@184..201 "\n                "
                      R_BRACK@201..202 "]"
                    WHITESPACE@202..215 "\n            "
                    R_CURLY@215..216 "}"
            "##]],
        );
    }

    #[test]
    fn unexpected_top_input() {
        check(
            r#"
            foo: 2
         // ^^^ unexpected ident, expected `{`
            // ^ unexpected colon, expected `{`
              // ^ unexpected number, expected `{`
            {bar: 3}
            {}
         // ^ unexpected `{`, expected end of file
          // ^ unexpected `}`, expected end of file
            "#,
            expect![[r#"
                SOURCE_FILE@0..68
                  WHITESPACE@0..13 "\n            "
                  ERROR@13..16
                    IDENT@13..16 "foo"
                  ERROR@16..17
                    COLON@16..17 ":"
                  WHITESPACE@17..18 " "
                  ERROR@18..19
                    NUMBER@18..19 "2"
                  WHITESPACE@19..32 "\n            "
                  TABLE@32..40
                    L_CURLY@32..33 "{"
                    PAIR@33..39
                      IDENT@33..36 "bar"
                      COLON@36..37 ":"
                      WHITESPACE@37..38 " "
                      NUMBER@38..39 "3"
                    R_CURLY@39..40 "}"
                  WHITESPACE@40..53 "\n            "
                  ERROR@53..54
                    L_CURLY@53..54 "{"
                  ERROR@54..55
                    R_CURLY@54..55 "}"
                  WHITESPACE@55..68 "\n            "
            "#]],
        );
    }
}
