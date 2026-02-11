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
    check(
        r#"{
            styles: ["red" > "string"]
            contains: [{x: 2} => Fail]
        }"#,
        expect![[r#"
            SOURCE_FILE@0..89
              TABLE@0..89
                L_CURLY@0..1 "{"
                WHITESPACE@1..14 "\n            "
                PAIR@14..40
                  IDENT@14..20 "styles"
                  COLON@20..21 ":"
                  WHITESPACE@21..22 " "
                  ARRAY@22..40
                    L_BRACK@22..23 "["
                    ITEM@23..39
                      LITERAL@23..28
                        STRING@23..28 "\"red\""
                      WHITESPACE@28..29 " "
                      R_ANGLE@29..30 ">"
                      WHITESPACE@30..31 " "
                      LITERAL@31..39
                        STRING@31..39 "\"string\""
                    R_BRACK@39..40 "]"
                WHITESPACE@40..53 "\n            "
                PAIR@53..79
                  IDENT@53..61 "contains"
                  COLON@61..62 ":"
                  WHITESPACE@62..63 " "
                  ARRAY@63..79
                    L_BRACK@63..64 "["
                    ITEM@64..78
                      TABLE@64..70
                        L_CURLY@64..65 "{"
                        PAIR@65..69
                          IDENT@65..66 "x"
                          COLON@66..67 ":"
                          WHITESPACE@67..68 " "
                          LITERAL@68..69
                            NUMBER@68..69 "2"
                        R_CURLY@69..70 "}"
                      WHITESPACE@70..71 " "
                      FAT_ARROW@71..73 "=>"
                      WHITESPACE@73..74 " "
                      LITERAL@74..78
                        IDENT@74..78 "Fail"
                    R_BRACK@78..79 "]"
                WHITESPACE@79..88 "\n        "
                R_CURLY@88..89 "}"
        "#]],
    );
}

#[test]
fn parse_values() {
    check(
        r#"{
            match: /a/ + "b" + include("foo") + keywordsToRegex("foo bar" + "baz",)
            format: #BUILTIN#
            color: [
                #ff0000 > "color"
            ]
        }"#,
        expect![[r##"
            SOURCE_FILE@0..194
              TABLE@0..194
                L_CURLY@0..1 "{"
                WHITESPACE@1..14 "\n            "
                PAIR@14..85
                  IDENT@14..19 "match"
                  COLON@19..20 ":"
                  WHITESPACE@20..21 " "
                  JOIN@21..85
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
                    CALL@50..85
                      IDENT@50..65 "keywordsToRegex"
                      L_PAREN@65..66 "("
                      STRING@66..75 "\"foo bar\""
                      WHITESPACE@75..76 " "
                      PLUS@76..77 "+"
                      WHITESPACE@77..78 " "
                      STRING@78..83 "\"baz\""
                      COMMA@83..84 ","
                      R_PAREN@84..85 ")"
                WHITESPACE@85..98 "\n            "
                PAIR@98..115
                  IDENT@98..104 "format"
                  COLON@104..105 ":"
                  WHITESPACE@105..106 " "
                  LITERAL@106..115
                    BUILTIN@106..115 "#BUILTIN#"
                WHITESPACE@115..128 "\n            "
                PAIR@128..184
                  IDENT@128..133 "color"
                  COLON@133..134 ":"
                  WHITESPACE@134..135 " "
                  ARRAY@135..184
                    L_BRACK@135..136 "["
                    WHITESPACE@136..153 "\n                "
                    ITEM@153..170
                      LITERAL@153..160
                        COLOR@153..160 "#ff0000"
                      WHITESPACE@160..161 " "
                      R_ANGLE@161..162 ">"
                      WHITESPACE@162..163 " "
                      LITERAL@163..170
                        STRING@163..170 "\"color\""
                    WHITESPACE@170..183 "\n            "
                    R_BRACK@183..184 "]"
                WHITESPACE@184..193 "\n        "
                R_CURLY@193..194 "}"
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
    check(
        r#"
        {
            match: include(foo)
                        // ^^^ unexpected ident, expected string
        }
        "#,
        expect![[r#"
            SOURCE_FILE@0..61
              WHITESPACE@0..9 "\n        "
              TABLE@9..52
                L_CURLY@9..10 "{"
                WHITESPACE@10..23 "\n            "
                PAIR@23..42
                  IDENT@23..28 "match"
                  COLON@28..29 ":"
                  WHITESPACE@29..30 " "
                  CALL@30..42
                    IDENT@30..37 "include"
                    L_PAREN@37..38 "("
                    ERROR@38..41
                      IDENT@38..41 "foo"
                    R_PAREN@41..42 ")"
                WHITESPACE@42..51 "\n        "
                R_CURLY@51..52 "}"
              WHITESPACE@52..61 "\n        "
        "#]],
    );
    check(
        r#"
        {
            {a b c}
         // ^ expected a ident or number
            // ^ unexpected ident, expected colon
              // ^ unexpected ident, expected colon
               // ^ unexpected `}`, expected colon
            match: /x/
        }
        "#,
        expect![[r#"
            SOURCE_FILE@0..72
              WHITESPACE@0..9 "\n        "
              TABLE@9..63
                L_CURLY@9..10 "{"
                WHITESPACE@10..23 "\n            "
                PAIR@23..30
                  TABLE@23..30
                    L_CURLY@23..24 "{"
                    PAIR@24..25
                      IDENT@24..25 "a"
                    WHITESPACE@25..26 " "
                    PAIR@26..27
                      IDENT@26..27 "b"
                    WHITESPACE@27..28 " "
                    PAIR@28..29
                      IDENT@28..29 "c"
                    R_CURLY@29..30 "}"
                WHITESPACE@30..43 "\n            "
                PAIR@43..53
                  IDENT@43..48 "match"
                  COLON@48..49 ":"
                  WHITESPACE@49..50 " "
                  LITERAL@50..53
                    REGEX@50..53 "/x/"
                WHITESPACE@53..62 "\n        "
                R_CURLY@62..63 "}"
              WHITESPACE@63..72 "\n        "
        "#]],
    );
    check(
        r#"
        {
            [a b c]
         // ^ expected a ident or number
            match: /x/
        }
        "#,
        expect![[r#"
            SOURCE_FILE@0..72
              WHITESPACE@0..9 "\n        "
              TABLE@9..63
                L_CURLY@9..10 "{"
                WHITESPACE@10..23 "\n            "
                PAIR@23..30
                  ARRAY@23..30
                    L_BRACK@23..24 "["
                    ITEM@24..26
                      LITERAL@24..25
                        IDENT@24..25 "a"
                      WHITESPACE@25..26 " "
                    ITEM@26..28
                      LITERAL@26..27
                        IDENT@26..27 "b"
                      WHITESPACE@27..28 " "
                    ITEM@28..29
                      LITERAL@28..29
                        IDENT@28..29 "c"
                    R_BRACK@29..30 "]"
                WHITESPACE@30..43 "\n            "
                PAIR@43..53
                  IDENT@43..48 "match"
                  COLON@48..49 ":"
                  WHITESPACE@49..50 " "
                  LITERAL@50..53
                    REGEX@50..53 "/x/"
                WHITESPACE@53..62 "\n        "
                R_CURLY@62..63 "}"
              WHITESPACE@63..72 "\n        "
        "#]],
    );
    check(
        r#"
        {
            (a b c)
         // ^ expected a ident or number
          // ^ unexpected ident, expected string
            // ^ unexpected ident, expected string
              // ^ unexpected ident, expected string
            match: /x/
        }
        "#,
        expect![[r#"
            SOURCE_FILE@0..72
              WHITESPACE@0..9 "\n        "
              TABLE@9..63
                L_CURLY@9..10 "{"
                WHITESPACE@10..23 "\n            "
                PAIR@23..30
                  CALL@23..30
                    L_PAREN@23..24 "("
                    ERROR@24..25
                      IDENT@24..25 "a"
                    WHITESPACE@25..26 " "
                    ERROR@26..27
                      IDENT@26..27 "b"
                    WHITESPACE@27..28 " "
                    ERROR@28..29
                      IDENT@28..29 "c"
                    R_PAREN@29..30 ")"
                WHITESPACE@30..43 "\n            "
                PAIR@43..53
                  IDENT@43..48 "match"
                  COLON@48..49 ":"
                  WHITESPACE@49..50 " "
                  LITERAL@50..53
                    REGEX@50..53 "/x/"
                WHITESPACE@53..62 "\n        "
                R_CURLY@62..63 "}"
              WHITESPACE@63..72 "\n        "
        "#]],
    );
}

#[test]
fn full_incomplete_state_fuzz() {
    let mut tokens = b"i/2#ag@,:{}[]+/<>~\n ".repeat(300);
    let mut randoms: [usize; 64] = [
        27888, 12846, 43407, 18871, 48402, 48763, 16047, 6652, 44121, 43293, 35057, 32662, 45616,
        6746, 13655, 51167, 4811, 39039, 48057, 13451, 39180, 18097, 10770, 22717, 41917, 36809,
        35166, 64691, 44689, 56057, 2665, 63445, 33740, 26928, 21503, 54775, 53518, 61781, 2620,
        15538, 39713, 108, 47141, 24899, 45101, 62987, 15925, 89, 2673, 39546, 29404, 295, 26896,
        34453, 35342, 46255, 9271, 13694, 25843, 35455, 6217, 61899, 57806, 15416,
    ];
    fn fake_shuf<T>(tokens: &mut [T], randoms: &[usize]) {
        for (i, &r) in (0..tokens.len()).zip(randoms.iter().cycle()) {
            let b = r % tokens.len();
            tokens.swap(i, b);
        }
    }
    for _ in 0..30 {
        fake_shuf(&mut tokens, &randoms);
        let (a, b) = randoms.split_at_mut(32);
        if a[0] & 1 == 0 {
            fake_shuf(a, b);
        } else {
            fake_shuf(b, a);
        }
    }

    let expect = expect_file!["./test_datas/full_incomplete_state.rast"];
    let mut at = 0;
    #[allow(unused)]
    let mut nodes_dump = String::new();

    for _ in 0..100 {
        if at >= tokens.len() {
            break;
        }
        println!("parse start at {at} in {}", tokens.len());
        let src = &format!("{{contains: [[[[{}]]]]}}", str::from_utf8(&tokens[at..]).unwrap());

        let mut parser = Parser::new(src);
        parser.source_file();
        let (node, _) = parser.finish();
        //std::fmt::Write::write_fmt(&mut nodes_dump, format_args!("{node:#?}")).unwrap();
        println!("parse finish");
        if let Some(err) = node.children().find(|it| it.kind() == ERROR) {
            let end = err.text_range().end();
            at += usize::from(end);
        } else {
            break;
        }
    }
    assert_ne!(at, 0);
    assert!(at >= tokens.len() / 3);
    expect.assert_eq(&nodes_dump);
}

#[test]
fn full_work() {
    let src = include_str!("./test_datas/rust-syntax.mtsx");
    let expect = expect_file!["./test_datas/rust-syntax.rast"];

    check(src, expect);
}
