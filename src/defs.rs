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
pub const COMMENT_DEFS: &[(&str, &str)] = &[
    ("startsWith",              r#"startsWith: "$1""#),
    ("endsWith",                r#"endsWith: "$1""#),
    ("insertSpace",             r#"insertSpace: ${1:false}"#),
    ("addToContains",           r#"addToContains: ${1:false}"#),
];
pub const HARD_DOCS: &[(&str, &str)] = &[
    ("keywordsToRegex", "keywordsToRegex 函数\n将输入的字符串内以空格分隔的关键字转换成正则表达式\n例如 keywordsToRegex(\"foo bar foobaz\") -> /\\b(?:foo(?:baz)?|bar)\\b/"),
    ("include()", "include 函数\n将 defines: [] 块中的 \"name\": /regex/ 内联"),
    ("match", "match 匹配器\nmatch 匹配器使用指定的正则表达式进行匹配，一旦匹配成功，会根据匹配组的风格对文本进行着色，如果要对整个匹配到的文本进行着色，则对应的捕获组序号为 0"),
    ("recordAllGroups", "recordAllGroups 属性\n正则表达式的每个子匹配组只能记录最后一个位置，如果你希望记录子匹配组的所有匹配位置并进行着色，将该属性设为 true 即可。"),
    ("start", "start-end 匹配器\n匹配头匹配器, 然后让子匹配器和尾匹配器竟争匹配"),
    ("end", "start-end 匹配器的尾匹配器"),
    ("matchEndFirst", "matchEndFirst 属性\nstart-end 匹配器在默认情况下，其算法是先匹配起点，然后终点匹配器与子匹配器进行竞争匹配，直到匹配到终点，或者到达文字末尾，才结束匹配。\n如果将该属性设为 true，则算法是先匹配起点和终点，如果终点匹配失败则默认为匹配到文本结尾，然后起点和终点之间剩余的文本再使用子匹配器去匹配。"),
    ("endPriority", "endPriority 属性\n末尾匹配器 (end) 的匹配优先级, 可用 <EndMatcher> 代替\n该属性仅在 matchEndFirst 为 false 时有效"),
    ("contains", "用于指定子匹配器"),
    ("<EndMatcher>", "EndMatcher 标记\n如果你觉得 endPriority 不够直观，那么你可以在 contains 中添加 <EndMatcher> 标记，功能与 endPriority 一样"),
    ("mustMatchEnd", "mustMatchEnd 属性\n如果你有特殊需求，一定要 end 成功匹配，而不是可接受文件末尾，那么只需设置 mustMatchEnd: true 即可。"),
    ("FAIL", "Fail 标记\n用于指定 contains 中的某个子匹配器匹配成功会使得整个 start-end 匹配器匹配失败。"),
    ("group", "group 匹配器\n将多个匹配器连接或只匹配其中一个"),
    ("link", "link\ngroup 的子匹配器，必须首尾相连, 之间不能有任何字符, 可以中途匹配失败"),
    ("linkAll", "linkAll\ngroup 的子匹配器，必须首尾相连, 之间不能有任何字符, 必须所有子匹配器都匹配成功"),
    ("select", "select\n匹配 group 的任意一个子匹配器"),
    ("number", "number 匹配器\n简单的定义常见编程语言中的数字匹配, 可控制进制、整数后缀、浮点后缀、数字分隔等"),
    ("builtin", "builtin 匹配器\n内置的一些预定义好的匹配器"),
    ("include", "include 匹配器\n引用匹配器, 可以互递归"),
    ("name", "name\n指定语法名称与文件后缀，第一个字符串为语法名称，第二个开始均为后缀名，后缀名可以有多个。"),
    ("hide", "hide\n设置是否隐藏，隐藏后在文本编辑器中手动选择语法时将无法看到该语法，默认为 false。"),
    ("ignoreCase", "ignoreCase\n设置是否不区分大小写，设置为 true 后，所有正则表达式在匹配时均不会区分大小写，默认为 false。"),
    ("styles", "styles\n自定义颜色"),
    ("colors", "styles\n自定义颜色"),
    ("style", "style\n指定匹配器的颜色风格"),
    ("color", "style\n指定匹配器的颜色风格"),
    ("comment", "comment\n定义语言注释, 主要用于切换注释功能"),
    ("startsWith", "自定义语言注释的起始字符 (不支持正则)"),
    ("endsWith", "自定义语言注释的终止字符 (不支持正则)"),
    ("insertSpace", "自定义语言注释在切换注释时是否插入空格"),
    ("addToContains", "自定义语言注释默认会将注释的匹配器插入 contains 中, 复杂注释时该功能可能是阻碍, 可以禁用该功能"),
    ("bracketPairs", "bracketPairs\n定义语言括号, 用于支持括号跳转等"),
    ("lineBackground", "lineBackground\n如果正则表达式可以完全匹配某一行的文本内容，则为该行添加背景颜色。"),
    ("defines", "defines\n定义可引用的匹配器、匹配器组与用于内联的正则"),
    ("codeFormatter", "codeFormatter\n指定该语法的代码格式化器，设置后可以在文本编辑器中使用代码格式化功能。"),
    ("codeShrinker", "codeShrinker\n指定该语法的代码压缩器，设置后可以在文本编辑器中使用代码压缩功能。例如将 json 的空白符号去除"),
];

pub struct DOC {}
pub const DOC: DOC = DOC {};

impl DOC {
    pub(crate) fn parse_color(&self) -> String {
        trim_indent(r#"
            该功能用于根据 match 正则捕获组匹配的内容动态设置文本前景色和背景色
            parseColor 共有 4 个参数，分别是：

                1. 前景色和 2. 背景色
                    可以是：匹配组序号、_、auto
                3. 颜色数值格式
                    可以是：HEX、HEXA、RGB、HSL、HSV、RGBA、HSLA、HSVA、RGBX、XRGB
                4. 基础风格
                    填写风格名称
        "#)
    }
}

fn trim_indent(mut s: &str) -> String {
    if let Some(strip_prefix) = s.strip_prefix('\n') {
        s = strip_prefix
    }
    s = s.trim_end();
    let ind = s.lines()
        .filter(|line| !line.trim_ascii().is_empty())
        .map(|line| {
            let pure = line.trim_ascii_start();
            line.len() - pure.len()
        })
        .min()
        .unwrap_or(0);
    s.split_inclusive('\n')
        .map(|line| if line.len() < ind {
            line.trim_ascii_start()
        } else {
            &line[ind..]
        })
        .collect()
}
