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
        "\n        foo bar baz\n    "
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
            SOURCE_FILE@0..77
              TABLE@0..77
                L_CURLY@0..1 "{"
                WHITESPACE@1..14 "\n            "
                PAIR@14..23
                  IDENT@14..19 "match"
                  COLON@19..20 ":"
                  WHITESPACE@20..21 " "
                  TABLE@21..23
                    L_CURLY@21..22 "{"
                    R_CURLY@22..23 "}"
                WHITESPACE@23..36 "\n            "
                PAIR@36..42
                  NUMBER@36..37 "0"
                  COLON@37..38 ":"
                  WHITESPACE@38..39 " "
                  TABLE@39..41
                    L_CURLY@39..40 "{"
                    R_CURLY@40..41 "}"
                  COMMA@41..42 ","
                WHITESPACE@42..55 "\n            "
                PAIR@55..67
                  STRING@55..63 "\"string\""
                  COLON@63..64 ":"
                  WHITESPACE@64..65 " "
                  TABLE@65..67
                    L_CURLY@65..66 "{"
                    R_CURLY@66..67 "}"
                WHITESPACE@67..76 "\n        "
                R_CURLY@76..77 "}"
        "#]],
    );
    check(
        r#"{
            missing:
        }
     // ^ unexpected `}`, expected value"#,
        expect![[r#"
            SOURCE_FILE@0..32
              TABLE@0..32
                L_CURLY@0..1 "{"
                WHITESPACE@1..14 "\n            "
                PAIR@14..22
                  IDENT@14..21 "missing"
                  COLON@21..22 ":"
                WHITESPACE@22..31 "\n        "
                R_CURLY@31..32 "}"
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
            SOURCE_FILE@0..68
              TABLE@0..68
                L_CURLY@0..1 "{"
                WHITESPACE@1..14 "\n            "
                PAIR@14..28
                  IDENT@14..19 "match"
                  COLON@19..20 ":"
                  WHITESPACE@20..21 " "
                  JOIN@21..28
                    TABLE@21..23
                      L_CURLY@21..22 "{"
                      R_CURLY@22..23 "}"
                    WHITESPACE@23..24 " "
                    PLUS@24..25 "+"
                    WHITESPACE@25..26 " "
                    TABLE@26..28
                      L_CURLY@26..27 "{"
                      R_CURLY@27..28 "}"
                WHITESPACE@28..41 "\n            "
                PAIR@41..58
                  IDENT@41..44 "and"
                  COLON@44..45 ":"
                  WHITESPACE@45..46 " "
                  JOIN@46..58
                    JOIN@46..53
                      TABLE@46..48
                        L_CURLY@46..47 "{"
                        R_CURLY@47..48 "}"
                      WHITESPACE@48..49 " "
                      PLUS@49..50 "+"
                      WHITESPACE@50..51 " "
                      TABLE@51..53
                        L_CURLY@51..52 "{"
                        R_CURLY@52..53 "}"
                    WHITESPACE@53..54 " "
                    PLUS@54..55 "+"
                    WHITESPACE@55..56 " "
                    TABLE@56..58
                      L_CURLY@56..57 "{"
                      R_CURLY@57..58 "}"
                WHITESPACE@58..67 "\n        "
                R_CURLY@67..68 "}"
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
            SOURCE_FILE@0..74
              TABLE@0..74
                L_CURLY@0..1 "{"
                WHITESPACE@1..14 "\n            "
                PAIR@14..33
                  IDENT@14..21 "defines"
                  COLON@21..22 ":"
                  WHITESPACE@22..23 " "
                  L_BRACK@23..24 "["
                  ITEM@24..32
                    STRING@24..27 "\"x\""
                    COLON@27..28 ":"
                    WHITESPACE@28..29 " "
                    REGEX@29..32 "/a/"
                  R_BRACK@32..33 "]"
                WHITESPACE@33..46 "\n            "
                PAIR@46..64
                  IDENT@46..54 "contains"
                  COLON@54..55 ":"
                  WHITESPACE@55..56 " "
                  L_BRACK@56..57 "["
                  ITEM@57..63
                    TABLE@57..63
                      L_CURLY@57..58 "{"
                      PAIR@58..62
                        IDENT@58..59 "x"
                        COLON@59..60 ":"
                        WHITESPACE@60..61 " "
                        NUMBER@61..62 "2"
                      R_CURLY@62..63 "}"
                  R_BRACK@63..64 "]"
                WHITESPACE@64..73 "\n        "
                R_CURLY@73..74 "}"
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
            SOURCE_FILE@0..192
              TABLE@0..192
                L_CURLY@0..1 "{"
                WHITESPACE@1..14 "\n            "
                PAIR@14..83
                  IDENT@14..19 "match"
                  COLON@19..20 ":"
                  WHITESPACE@20..21 " "
                  JOIN@21..83
                    JOIN@21..47
                      JOIN@21..30
                        REGEX@21..24 "/a/"
                        WHITESPACE@24..25 " "
                        PLUS@25..26 "+"
                        WHITESPACE@26..27 " "
                        STRING@27..30 "\"b\""
                      WHITESPACE@30..31 " "
                      PLUS@31..32 "+"
                      WHITESPACE@32..33 " "
                      CALL@33..47
                        IDENT@33..40 "include"
                        L_PAREN@40..41 "("
                        STRING@41..46 "\"foo\""
                        R_PAREN@46..47 ")"
                    WHITESPACE@47..48 " "
                    PLUS@48..49 "+"
                    WHITESPACE@49..50 " "
                    CALL@50..83
                      IDENT@50..65 "keywordsToRegex"
                      L_PAREN@65..66 "("
                      STRING@66..75 "\"foo bar\""
                      WHITESPACE@75..76 " "
                      STRING@76..81 "\"baz\""
                      COMMA@81..82 ","
                      R_PAREN@82..83 ")"
                WHITESPACE@83..96 "\n            "
                PAIR@96..113
                  IDENT@96..102 "format"
                  COLON@102..103 ":"
                  WHITESPACE@103..104 " "
                  BUILTIN@104..113 "#BUILTIN#"
                WHITESPACE@113..126 "\n            "
                PAIR@126..182
                  IDENT@126..131 "color"
                  COLON@131..132 ":"
                  WHITESPACE@132..133 " "
                  L_BRACK@133..134 "["
                  WHITESPACE@134..151 "\n                "
                  ITEM@151..160
                    COLOR@151..158 "#ff0000"
                    WHITESPACE@158..159 " "
                    R_ANGLE@159..160 ">"
                  WHITESPACE@160..161 " "
                  ITEM@161..168
                    STRING@161..168 "\"color\""
                  WHITESPACE@168..181 "\n            "
                  R_BRACK@181..182 "]"
                WHITESPACE@182..191 "\n        "
                R_CURLY@191..192 "}"
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
            SOURCE_FILE@0..52
              WHITESPACE@0..9 "\n        "
              ERROR@9..12
                IDENT@9..12 "foo"
              ERROR@12..13
                COLON@12..13 ":"
              WHITESPACE@13..14 " "
              ERROR@14..15
                NUMBER@14..15 "2"
              WHITESPACE@15..24 "\n        "
              TABLE@24..32
                L_CURLY@24..25 "{"
                PAIR@25..31
                  IDENT@25..28 "bar"
                  COLON@28..29 ":"
                  WHITESPACE@29..30 " "
                  NUMBER@30..31 "3"
                R_CURLY@31..32 "}"
              WHITESPACE@32..41 "\n        "
              ERROR@41..42
                L_CURLY@41..42 "{"
              ERROR@42..43
                R_CURLY@42..43 "}"
              WHITESPACE@43..52 "\n        "
        "#]],
    );
}
