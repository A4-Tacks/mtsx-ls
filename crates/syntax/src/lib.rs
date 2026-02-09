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
