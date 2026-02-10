pub const COMPLETE_MARKER: &str = "abcdef"; // 考虑到颜色, 不要超出f字母
pub const MANIFEST_ATTRS: &[(&str, &str)] = &[
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
pub const BUILTIN_COLORS: &[(&str, &str, &str)] = &[
    ("default",      "#000000",     "#A9B7C6"),
    ("string",       "#067D17",     "#6A8759"),
    ("strEscape",    "#0037A6",     "#CC7832"),
    ("comment",      "#8C8C8C",     "#808080"),
    ("meta",         "#9E880D",     "#BBB529"),
    ("number",       "#1750EB",     "#6897BB"),
    ("keyword",      "#0033B3",     "#CC7832"),
    ("keyword2",     "#800000",     "#AE8ABE"),
    ("constant",     "#871094",     "#9876AA"),
    ("type",         "#808000",     "#808000"),
    ("label",        "#7050E0",     "#6080B0"),
    ("variable",     "#1750EB",     "#58908A"),
    ("operator",     "#205060",     "#508090"),
    ("propKey",      "#083080",     "#CC7832"),
    ("propVal",      "#067D17",     "#6A8759"),
    ("tagName",      "#0030B3",     "#E8BF6A"),
    ("attrName",     "#174AD4",     "#BABABA"),
    ("namespace",    "#871094",     "#9876AA"),
    ("error",        "#F50000",     "#BC3F3C"),
];
pub const BUILTIN_MATCHERS: &[(&str, &str)] = &[
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
pub const BUILTIN_FORMATTERS: &[&str] = &[
    "BUILT_IN_CSS_FORMATTER",
    "BUILT_IN_HTML_FORMATTER",
    "BUILT_IN_JAVA_FORMATTER",
    "BUILT_IN_JS_FORMATTER",
    "BUILT_IN_JSON_FORMATTER",
    "BUILT_IN_XML_FORMATTER",
    "BUILT_IN_SMALI_FORMATTER",
];
pub const BUILTIN_SHINKERS: &[&str] = &[
    "BUILT_IN_CSS_SHRINKER",
    "BUILT_IN_HTML_SHRINKER",
    "BUILT_IN_JSON_SHRINKER",
];
pub const MATCHER_SCHEMA: &[((&str, &str), &[(&str, &str)])] = &[
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
pub const PATTERNS: &[(&str, &str)] = &[
    ("include",                 r#"include("$1")"#),
    ("keywordsToRegex",         r#"keywordsToRegex("$1")"#),
];
pub const ALLOW_DUP_KEYS: &[&str] = &["comment", "lineBackground"];
