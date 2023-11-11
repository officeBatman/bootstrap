use crate::token::LToken;

pub fn lex(source: &str) -> Vec<LToken> {
    nessie_lex::lex(source)
}

#[cfg(test)]
mod tests {
    use crate::{global::Pipe, token::{Token, Symbol, Keyword}};

    use super::*;
    use indoc::indoc;
    use nessie_lex::{NewLine, range::IntoLocated, Quote};

    #[test]
    fn lex_empty() {
        let source = "";
        assert_eq!(
            lex(source),
            vec![NewLine::EmptyLine.pipe(Token::NewLine).into_located(0..0)]
        );
    }

    #[test]
    fn test_newline_token() {
        let source = "\n";
        assert_eq!(
            lex(source),
            vec![
                NewLine::EmptyLine.pipe(Token::NewLine).into_located(0..0),
                NewLine::EmptyLine.pipe(Token::NewLine).into_located(0..1),
            ]
        );
    }

    #[test]
    fn test_lex() {
        let source = indoc! {r#"
            x: prop = 5

            y: type = x . "a"
        "#};

        assert_eq!(
            lex(source),
            vec![
                Token::NewLine(NewLine::NewLine { indent: 0 }).into_located(0..0),
                Token::Ident("x").into_located(0..1),
                Token::Symbol(Symbol::Colon).into_located(1..2),
                Token::Ident("prop").into_located(3..7),
                Token::Symbol(Symbol::Equal).into_located(8..9),
                Token::Int(5).into_located(10..11),
                Token::NewLine(NewLine::EmptyLine).into_located(11..12),
                Token::NewLine(NewLine::NewLine { indent: 0 }).into_located(12..13),
                Token::Ident("y").into_located(13..14),
                Token::Symbol(Symbol::Colon).into_located(14..15),
                Token::Keyword(Keyword::Type).into_located(16..20),
                Token::Symbol(Symbol::Equal).into_located(21..22),
                Token::Ident("x").into_located(23..24),
                Token::Symbol(Symbol::Dot).into_located(25..26),
                Token::String(Quote::Double, "a").into_located(27..30),
                Token::NewLine(NewLine::EmptyLine).into_located(30..31),
            ]
        );
    }
}
