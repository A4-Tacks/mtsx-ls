use expect_test::{expect, expect_file};
use test_utils::{ExpectLike, apply_inline_lints, assert_text_eq, clean_inline_lints};

use super::*;

#[track_caller]
fn check(src: &str, expect: impl ExpectLike) {
    let clean_src = &clean_inline_lints(src);

    let mut parser = Parser::new(clean_src);
    parser.source_file();
    let (node, errors) = parser.finish();

    expect.assert_eq(&format!("{node:#?}"));
    let linted_src = apply_inline_lints(clean_src, errors);
    assert_text_eq!(linted_src, src, "lints is not matched\n left: {src:?}\nright: {linted_src:?}");
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
        }"#,
        expect![[r#"
            SOURCE_FILE@0..52
              TABLE@0..52
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
                WHITESPACE@42..51 "\n        "
                R_CURLY@51..52 "}"
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
                  ARRAY@23..33
                    L_BRACK@23..24 "["
                    ITEM@24..32
                      LITERAL@24..27
                        STRING@24..27 "\"x\""
                      COLON@27..28 ":"
                      WHITESPACE@28..29 " "
                      LITERAL@29..32
                        REGEX@29..32 "/a/"
                    R_BRACK@32..33 "]"
                WHITESPACE@33..46 "\n            "
                PAIR@46..64
                  IDENT@46..54 "contains"
                  COLON@54..55 ":"
                  WHITESPACE@55..56 " "
                  ARRAY@56..64
                    L_BRACK@56..57 "["
                    ITEM@57..63
                      TABLE@57..63
                        L_CURLY@57..58 "{"
                        PAIR@58..62
                          IDENT@58..59 "x"
                          COLON@59..60 ":"
                          WHITESPACE@60..61 " "
                          LITERAL@61..62
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
                        LITERAL@21..24
                          REGEX@21..24 "/a/"
                        WHITESPACE@24..25 " "
                        PLUS@25..26 "+"
                        WHITESPACE@26..27 " "
                        LITERAL@27..30
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
                  LITERAL@104..113
                    BUILTIN@104..113 "#BUILTIN#"
                WHITESPACE@113..126 "\n            "
                PAIR@126..182
                  IDENT@126..131 "color"
                  COLON@131..132 ":"
                  WHITESPACE@132..133 " "
                  ARRAY@133..182
                    L_BRACK@133..134 "["
                    WHITESPACE@134..151 "\n                "
                    ITEM@151..160
                      LITERAL@151..158
                        COLOR@151..158 "#ff0000"
                      WHITESPACE@158..159 " "
                      R_ANGLE@159..160 ">"
                    WHITESPACE@160..161 " "
                    ITEM@161..168
                      LITERAL@161..168
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
                  LITERAL@30..31
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
    check(
        r#"
        foo
     // ^^^ unexpected ident, expected `{`
        "#,
        expect![[r#"
            SOURCE_FILE@0..21
              WHITESPACE@0..9 "\n        "
              ERROR@9..12
                IDENT@9..12 "foo"
              WHITESPACE@12..21 "\n        "
        "#]],
    );
}

#[test]
fn some_incomplete_state() {
    check(
        r#" "#,
        expect![[r#"
            SOURCE_FILE@0..1
              WHITESPACE@0..1 " "
        "#]],
    );
    check(
        r#"
        {
            com
        }
     // ^ unexpected `}`, expected colon
        "#,
        expect![[r#"
            SOURCE_FILE@0..45
              WHITESPACE@0..9 "\n        "
              TABLE@9..36
                L_CURLY@9..10 "{"
                WHITESPACE@10..23 "\n            "
                PAIR@23..26
                  IDENT@23..26 "com"
                WHITESPACE@26..35 "\n        "
                R_CURLY@35..36 "}"
              WHITESPACE@36..45 "\n        "
        "#]],
    );
    check(
        r#"
        {
            comment:
        }
     // ^ unexpected `}`, expected value
        "#,
        expect![[r#"
            SOURCE_FILE@0..50
              WHITESPACE@0..9 "\n        "
              TABLE@9..41
                L_CURLY@9..10 "{"
                WHITESPACE@10..23 "\n            "
                PAIR@23..31
                  IDENT@23..30 "comment"
                  COLON@30..31 ":"
                WHITESPACE@31..40 "\n        "
                R_CURLY@40..41 "}"
              WHITESPACE@41..50 "\n        "
        "#]],
    );
    check(
        r#"
        {
            comment:
            foo: []
            // ^ expected a ident or number
        }
        "#,
        expect![[r#"
            SOURCE_FILE@0..70
              WHITESPACE@0..9 "\n        "
              TABLE@9..61
                L_CURLY@9..10 "{"
                WHITESPACE@10..23 "\n            "
                PAIR@23..47
                  IDENT@23..30 "comment"
                  COLON@30..31 ":"
                  WHITESPACE@31..44 "\n            "
                  LITERAL@44..47
                    IDENT@44..47 "foo"
                PAIR@47..51
                  COLON@47..48 ":"
                  WHITESPACE@48..49 " "
                  ARRAY@49..51
                    L_BRACK@49..50 "["
                    R_BRACK@50..51 "]"
                WHITESPACE@51..60 "\n        "
                R_CURLY@60..61 "}"
              WHITESPACE@61..70 "\n        "
        "#]],
    );
    check(
        r#"
        {
            foo: [
        }
     // ^ unexpected `}`, expected `]`
        "#,
        expect![[r#"
            SOURCE_FILE@0..48
              WHITESPACE@0..9 "\n        "
              TABLE@9..39
                L_CURLY@9..10 "{"
                WHITESPACE@10..23 "\n            "
                PAIR@23..38
                  IDENT@23..26 "foo"
                  COLON@26..27 ":"
                  WHITESPACE@27..28 " "
                  ARRAY@28..29
                    L_BRACK@28..29 "["
                  WHITESPACE@29..38 "\n        "
                R_CURLY@38..39 "}"
              WHITESPACE@39..48 "\n        "
        "#]],
    );
    check(
        r#"
        {
            match: /a/+
        }
     // ^ unexpected `}`, expected value
        "#,
        expect![[r#"
            SOURCE_FILE@0..53
              WHITESPACE@0..9 "\n        "
              TABLE@9..44
                L_CURLY@9..10 "{"
                WHITESPACE@10..23 "\n            "
                PAIR@23..43
                  IDENT@23..28 "match"
                  COLON@28..29 ":"
                  WHITESPACE@29..30 " "
                  JOIN@30..34
                    LITERAL@30..33
                      REGEX@30..33 "/a/"
                    PLUS@33..34 "+"
                  WHITESPACE@34..43 "\n        "
                R_CURLY@43..44 "}"
              WHITESPACE@44..53 "\n        "
        "#]],
    );
    check(
        r#"
        {
            match: +/a/
                // ^ expected value
        }
        "#,
        expect![[r#"
            SOURCE_FILE@0..53
              WHITESPACE@0..9 "\n        "
              TABLE@9..44
                L_CURLY@9..10 "{"
                WHITESPACE@10..23 "\n            "
                PAIR@23..34
                  IDENT@23..28 "match"
                  COLON@28..29 ":"
                  WHITESPACE@29..30 " "
                  JOIN@30..34
                    PLUS@30..31 "+"
                    LITERAL@31..34
                      REGEX@31..34 "/a/"
                WHITESPACE@34..43 "\n        "
                R_CURLY@43..44 "}"
              WHITESPACE@44..53 "\n        "
        "#]],
    );
}

#[test]
fn full_work() {
    let src = include_str!("./test_datas/rust-syntax.mtsx");
    let expect = expect_file!["./test_datas/rust-syntax.rast"];

    check(src, expect);
}
