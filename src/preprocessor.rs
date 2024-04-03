use genawaiter::{yield_,stack::let_gen};

pub fn preprocess(code: String) -> String {
    let_gen!(remove_cr, {
        enum RmCrState {
            Normal,
            Cr,
        }
        use RmCrState::*;
        let mut state = Normal;
        for c in code.chars() {
            match (&state, c) {
                (Normal, '\r') => state = Cr,
                (Normal, _) => yield_!(c),
                (Cr, '\n') => {
                    state = Normal;
                    yield_!('\n');
                }
                (Cr, '\r') => yield_!('\n'),
                (Cr, _) => {
                    state = Normal;
                    yield_!('\n');
                    yield_!(c);
                }
            }
        }
        if let Cr = state { yield_!('\r'); }
    });
    let_gen!(phy_line_to_logical_line, {
        enum P2LState {
            Normal,
            Backslash,
        }
        use P2LState::*;
        let mut state = Normal;
        for c in remove_cr {
            match (&state, c) {
                (Normal, '\\') => state = Backslash,
                (Normal, _) => yield_!(c),
                (Backslash, '\n') => state = Normal,
                (Backslash, '\\') => yield_!('\\'),
                (Backslash, _) => {
                    state = Normal;
                    yield_!('\\');
                    yield_!(c);
                }
            }
        }
        if let Backslash = state { yield_!('\\'); }
    });
    let_gen!(remove_comments, {
        enum State {
            Code,
            CodeWithSlash,
            StringLiteral,
            StringLiteralWithEscape,
            Comment,
            CxxComment,
            CommentWithStar,
        }
        use State::*;
        let mut state = Code;
        for c in phy_line_to_logical_line {
            match (&state, c) {
                (Code, '/') => state = CodeWithSlash,
                (Code, '"') => {
                    state = StringLiteral;
                    yield_!('"');
                }
                (Code, _) => yield_!(c),
                (CodeWithSlash, '/') => state = CxxComment,
                (CodeWithSlash, '*') => state = Comment,
                (CodeWithSlash, _) => {
                    state = Code;
                    yield_!('/');
                    yield_!(c);
                }
                (StringLiteral, '\\') => state = StringLiteralWithEscape,
                (StringLiteral, '"') => {
                    state = Code;
                    yield_!('"');
                }
                (StringLiteral, _) => yield_!(c),
                (StringLiteralWithEscape, _) => {
                    state = StringLiteral;
                    yield_!('\\');
                    yield_!(c);
                }
                (Comment, '*') => state = CommentWithStar,
                (Comment, _) => (),
                (CommentWithStar, '/') => {
                    state = Code;
                    yield_!(' ');
                }
                (CommentWithStar, '*') => (),
                (CommentWithStar, _) => state = Comment,
                (CxxComment, '\n') => {
                    state = Code;
                    yield_!('\n');
                }
                (CxxComment, _) => (),
            }
        }
    });
    remove_comments.into_iter().collect()
}
