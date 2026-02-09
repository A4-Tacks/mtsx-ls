use line_column::span::Span;
pub use parser::{AstNode, SyntaxKind, SyntaxNode, SyntaxToken, T, TextSize, TextRange};

pub mod ast;

pub fn parse_file(src: &str) -> (ast::SourceFile, Vec<(Span, String)>) {
    let mut parser = parser::parser::Parser::new(src);
    let full_span = Span::new_full(src);

    parser.source_file();
    let (syntax_node, errors) = parser.finish();

    let errors = errors
        .into_iter()
        .map(|(range, lint)| (full_span.create(range), lint))
        .collect();

    let source_file = ast::SourceFile::cast(syntax_node).unwrap();
    (source_file, errors)
}

pub trait AstToken: std::fmt::Display {
    fn syntax(&self) -> SyntaxToken;
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;

    #[test]
    fn basic_ast_methods() {
        let src = r#"
        {
            match: /foo/,
            1: "red"
        }
        "#;
        let (root, errors) = parse_file(src);
        assert!(errors.is_empty());

        let table = root.table().unwrap();
        let pairs = table.pairs().collect::<Vec<_>>();
        assert_eq!(pairs.len(), 2);
        let keys = pairs.iter().map(|it| it.key().unwrap()).collect::<Vec<_>>();
        let values = pairs.iter().map(|it| it.value().unwrap()).collect::<Vec<_>>();
        expect![[r#"
            [
                IDENT(
                    IDENT@23..28 "match",
                ),
                NUMBER(
                    NUMBER@49..50 "1",
                ),
            ]
        "#]].assert_debug_eq(&keys);
        expect![[r#"
            [
                Literal(
                    Literal(
                        LITERAL@30..35
                          REGEX@30..35 "/foo/"
                        ,
                    ),
                ),
                Literal(
                    Literal(
                        LITERAL@52..57
                          STRING@52..57 "\"red\""
                        ,
                    ),
                ),
            ]
        "#]].assert_debug_eq(&values);
    }
}
