use syntax::{AstNode, NodeOrToken, Or, SyntaxKind, SyntaxNode, SyntaxToken, TextRange, ast};

use crate::{Analysis, Location, indent_spaces};

type Elem = NodeOrToken<SyntaxNode, SyntaxToken>;

pub(crate) fn extract_pattern(ctx: &Analysis, elem: Elem) -> Option<Vec<(TextRange, String)>> {
    let tok = elem.into_token()?;

    if !matches!(tok.kind(), SyntaxKind::REGEX | SyntaxKind::IDENT) {
        return None;
    }
    if tok.text() == "include" {
        return None;
    }

    let place = tok.parent_ancestors()
        .take_while(|it| {
            !Or::<ast::Item, ast::Pair>::can_cast(it.kind())
        })
        .find_map(Or::<ast::Literal, ast::Call>::cast)?;

    let defines = ctx.get_manifest("defines")?.into_array()?;
    let last_def = defines.items().last()?;
    let whitespace = last_def.syntax().prev_sibling_or_token()
        .and_then(NodeOrToken::into_token)
        .filter(|it| it.kind() == SyntaxKind::WHITESPACE)
        .map_or(String::new(), |it| it.to_string());

    let place_indent = indent_spaces(place.syntax().clone());
    let to_indent = indent_spaces(last_def.syntax().clone());
    let dedent = place_indent - to_indent;

    let to_extract = format!(r#"{whitespace}"new_name": {}"#, crate::dedent(&place.to_string(), dedent));
    let to_replace = format!(r#"include("new_name")"#);

    Some([
        (TextRange::empty(last_def.syntax().text_range().end()), to_extract),
        (place.syntax().text_range(), to_replace),
    ].to_vec())
}

pub(crate) fn extract_matcher(ctx: &Analysis, elem: Elem) -> Option<Vec<(TextRange, String)>> {
    let tok = elem.into_token()?;

    if tok.kind() != SyntaxKind::IDENT {
        return None;
    }
    if tok.text() == "include" {
        return None;
    }
    let item = tok.parent_ancestors().find_map(ast::Pair::cast)?;
    let table = item.syntax().parent().and_then(ast::Table::cast)?;

    let defines = ctx.get_manifest("defines")?.into_array()?;
    let last_def = defines.items().last()?;
    let whitespace = last_def.syntax().prev_sibling_or_token()
        .and_then(NodeOrToken::into_token)
        .filter(|it| it.kind() == SyntaxKind::WHITESPACE)
        .map_or(String::new(), |it| it.to_string());

    let place_indent = indent_spaces(table.syntax().clone());
    let to_indent = indent_spaces(last_def.syntax().clone());
    let dedent = place_indent - to_indent;

    let to_table = super::dedent(&table.to_string(), dedent);
    let to_extract = format!(r#"{whitespace}"new_name": {to_table}"#);
    let to_replace = format!(r#"{{include: "new_name"}}"#);

    Some([
        (TextRange::empty(last_def.syntax().text_range().end()), to_extract),
        (table.syntax().text_range(), to_replace),
    ].to_vec())
}

pub(crate) fn inline_matcher(ctx: &Analysis, elem: Elem) -> Option<Vec<(TextRange, String)>> {
    let tok = elem.into_token()?;

    if tok.kind() != SyntaxKind::STRING {
        return None;
    }
    if ctx.location(&tok.parent()?) != Location::IncludeMatcher {
        return None;
    }

    let def = ctx.goto_define(tok.text_range())?;
    let item = def.token.parent_ancestors().find_map(ast::Item::cast)?;
    let assoc = item.assoc()?;

    let inliner = tok.parent_ancestors().find_map(ast::Table::cast)?;

    let place_indent = indent_spaces(assoc.syntax().clone());
    let to_indent = indent_spaces(inliner.syntax().clone());
    let dedent = place_indent - to_indent;

    let to_table = super::dedent(&assoc.to_string(), dedent);

    Some([
        (inliner.syntax().text_range(), to_table),
    ].to_vec())
}

pub(crate) fn inline_pattern(ctx: &Analysis, elem: Elem) -> Option<Vec<(TextRange, String)>> {
    let tok = elem.into_token()?;

    if tok.kind() != SyntaxKind::STRING {
        return None;
    }
    if ctx.location(&tok.parent()?) != Location::IncludeRegex {
        return None;
    }

    let def = ctx.goto_define(tok.text_range())?;
    let item = def.token.parent_ancestors().find_map(ast::Item::cast)?;
    let assoc = item.assoc()?;

    let inliner = tok.parent_ancestors().find_map(ast::Call::cast)?;

    let place_indent = indent_spaces(assoc.syntax().clone());
    let to_indent = indent_spaces(inliner.syntax().clone());
    let dedent = place_indent - to_indent;

    let to_call = super::dedent(&assoc.to_string(), dedent);

    Some([
        (inliner.syntax().text_range(), to_call),
    ].to_vec())
}

pub(crate) fn convert_matcher_list(ctx: &Analysis, elem: Elem) -> Option<Vec<(TextRange, String)>> {
    let tok = elem.into_token()?;

    if tok.kind() != SyntaxKind::L_CURLY {
        return None;
    }
    if ctx.location(&tok.parent()?) != Location::Value {
        return None;
    }
    let table = tok.parent().and_then(ast::Table::cast)?;

    let indent = indent_spaces(ctx.get_manifest("name")?.syntax().clone());
    let place_indent = indent_spaces(table.syntax().clone());

    let new_table = super::dedent(&table.to_string(), place_indent);
    let new_table = super::dedent(&format!("[\n{new_table}"), -indent) + "\n]";
    let new_table = super::dedent(&new_table, -place_indent);

    Some([(table.syntax().text_range(), new_table)].to_vec())
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};
    use line_column::span::Span;
    use syntax::TextSize;

    use super::*;

    type Assist = fn(&Analysis, Elem) -> Option<Vec<(TextRange, String)>>;

    #[track_caller]
    fn check(assist: Assist, src: &str, expect: Expect) {
        let src = dedent_for_test_input(src);
        let index = src.find("$0").expect("must a `$0`");
        let src = Span::new_full(src.replacen("$0", "", 1));
        let analysis = Analysis::new(src.clone());
        let at = TextRange::empty(TextSize::new(index.try_into().unwrap()));
        let elem = analysis.element(at);

        let mut edited = src.text().to_string();
        let mut edits = assist(&analysis, elem).unwrap_or_else(|| {
            edited = "<not applicable>".to_owned();
            vec![]
        });
        edits.sort_by_key(|it| it.0.start());
        for (edit, new) in edits.iter().rev() {
            let range = usize::from(edit.start())..edit.end().into();
            edited.replace_range(range, new);
        }
        expect.assert_eq(&edited);
    }

    fn dedent_for_test_input(src: &str) -> String {
        assert!(src.starts_with('\n'));
        let src = &src[1..];
        let pure = src.trim_ascii_start();
        crate::dedent(src, (src.len()-pure.len()).try_into().unwrap())
    }

    #[test]
    fn test_convert_matcher_into_list() {
        check(
            convert_matcher_list,
            r#"
            {
                name: ["x"]
                defines: [
                    "x": $0{
                        match: /x/
                    }
                ]
            }
            "#,
            expect![[r#"
                {
                    name: ["x"]
                    defines: [
                        "x": [
                            {
                                match: /x/
                            }
                        ]
                    ]
                }
            "#]],
        );
    }

    #[test]
    fn test_extract_matcher() {
        check(
            extract_matcher,
            r#"
            {
                name: ["x"]
                defines: [
                    "x": {match: /x/}
                ]
                contains: [
                    {
                        $0match: /y/
                    }
                ]
            }
            "#,
            expect![[r#"
                {
                    name: ["x"]
                    defines: [
                        "x": {match: /x/}
                        "new_name": {
                            match: /y/
                        }
                    ]
                    contains: [
                        {include: "new_name"}
                    ]
                }
            "#]],
        );
        check(
            extract_matcher,
            r#"
            {
                name: ["x"]
                defines: [
                    "x": {match: /x/}
                ]
                contains: [
                    {
                        $0include: "x"
                    }
                ]
            }
            "#,
            expect!["<not applicable>"],
        );
    }

    #[test]
    fn test_extract_regex() {
        check(
            extract_pattern,
            r#"
            {
                name: ["x"]
                defines: [
                    "x": {match: /x/}
                ]
                contains: [
                    {
                        match: $0/y/
                    }
                ]
            }
            "#,
            expect![[r#"
                {
                    name: ["x"]
                    defines: [
                        "x": {match: /x/}
                        "new_name": /y/
                    ]
                    contains: [
                        {
                            match: include("new_name")
                        }
                    ]
                }
            "#]],
        );
    }

    #[test]
    fn test_extract_call() {
        check(
            extract_pattern,
            r#"
            {
                name: ["x"]
                defines: [
                    "x": {match: /x/}
                ]
                contains: [
                    {
                        match: $0keywordsToRegex(
                            "a b"
                            "c d"
                        )
                    }
                ]
            }
            "#,
            expect![[r#"
                {
                    name: ["x"]
                    defines: [
                        "x": {match: /x/}
                        "new_name": keywordsToRegex(
                            "a b"
                            "c d"
                        )
                    ]
                    contains: [
                        {
                            match: include("new_name")
                        }
                    ]
                }
            "#]],
        );
        check(
            extract_pattern,
            r#"
            {
                name: ["x"]
                defines: [
                    "x": /x/
                ]
                contains: [
                    {
                        match: $0include("x")
                    }
                ]
            }
            "#,
            expect!["<not applicable>"],
        );
    }

    #[test]
    fn test_inline_pattern() {
        check(
            inline_pattern,
            r#"
            {
                name: ["x"]
                defines: [
                    "x": /x/ + /t/
                ]
                contains: [
                    {
                        match: include($0"x")
                    }
                ]
            }
            "#,
            expect![[r#"
                {
                    name: ["x"]
                    defines: [
                        "x": /x/ + /t/
                    ]
                    contains: [
                        {
                            match: /x/ + /t/
                        }
                    ]
                }
            "#]],
        );
    }

    #[test]
    fn test_inline_matcher() {
        check(
            inline_matcher,
            r#"
            {
                name: ["x"]
                defines: [
                    "x": {match: /x/ + /t/}
                ]
                contains: [
                    {
                        include: $0"x"
                    }
                ]
            }
            "#,
            expect![[r#"
                {
                    name: ["x"]
                    defines: [
                        "x": {match: /x/ + /t/}
                    ]
                    contains: [
                        {match: /x/ + /t/}
                    ]
                }
            "#]],
        );
    }
}
