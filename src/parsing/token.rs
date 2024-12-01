#[derive(PartialEq, Copy, Clone, Debug,)]
pub enum TokenType {
    Undefined,
    Unknown,
    Identifier,
    ConstIntegerExpression,
    ConstStringExpression,
    BracketOpen,
    BracketClose,
    CurlyBracketOpen ,
    CurlyBracketClose,
    SquareBracketOpen,
    SquareBracketClose,
    Assign,
    Equal,
    NotEqual,
    GreaterThan,
    LowerThan,
    GreaterEqualThan,
    LowerEqualThan,
    And,
    Or,
    Dot,
    Comma,
    Semicolon,
    Plus,
    Minus,
    Divide,
    Multiply,
    Reference,
    Dereference,
    Increment,
    Decrement,
}

impl TokenType {
    pub fn to_string(&self) -> String {
        match self {
            TokenType::Undefined => "Undefined".to_string(),
            TokenType::Unknown => "Unknown".to_string(),
            TokenType::Identifier => "Identifier".to_string(),
            TokenType::ConstIntegerExpression => "Constant integer expression".to_string(),
            TokenType::ConstStringExpression => "Constant string expression".to_string(),
            TokenType::BracketOpen => "(".to_string(),
            TokenType::BracketClose => ")".to_string(),
            TokenType::CurlyBracketOpen => "{".to_string(),
            TokenType::CurlyBracketClose => "}".to_string(),
            TokenType::SquareBracketOpen => "[".to_string(),
            TokenType::SquareBracketClose => "]".to_string(),
            TokenType::Assign => "=".to_string(),
            TokenType::Equal => "==".to_string(),
            TokenType::NotEqual => "!=".to_string(),
            TokenType::GreaterThan => ">".to_string(),
            TokenType::LowerThan => "<".to_string(),
            TokenType::GreaterEqualThan => ">=".to_string(),
            TokenType::LowerEqualThan => "<=".to_string(),
            TokenType::Decrement => "--".to_string(),
            TokenType::Increment => "++".to_string(),
            TokenType::And => "&&".to_string(),
            TokenType::Or => "||".to_string(),
            TokenType::Dot => ".".to_string(),
            TokenType::Comma => ",".to_string(),
            TokenType::Semicolon => ";".to_string(),
            TokenType::Plus => "+".to_string(),
            TokenType::Minus => "-".to_string(),
            TokenType::Divide => "/".to_string(),
            TokenType::Multiply => "*".to_string(),
            TokenType::Reference => "&".to_string(),
            TokenType::Dereference => "*".to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub value: String,
    pub token_start : u32,
    pub token_end : u32
}


impl Token {
    pub fn new(value : String, token_start : u32, token_end : u32) -> Token {
        Token {
            value,
            token_start,
            token_end,
        }
    }

    pub fn get_calculated_token_type(&self) -> TokenType {
        let token: &str = &self.value;
        match token {
            "(" => TokenType::BracketOpen,
            ")" => TokenType::BracketClose,
            "{" => TokenType::CurlyBracketOpen,
            "}" => TokenType::CurlyBracketClose,
            "[" => TokenType::SquareBracketOpen,
            "]" => TokenType::SquareBracketClose,
            "=" => TokenType::Assign,
            "==" => TokenType::Equal,
            "!=" => TokenType::NotEqual,
            "++" => TokenType::Increment,
            "--" => TokenType::Decrement,
            ">" => TokenType::GreaterThan,
            "<" => TokenType::LowerThan,
            ">=" => TokenType::GreaterEqualThan,
            "<=" => TokenType::LowerEqualThan,
            "&&" => TokenType::And,
            "||" => TokenType::Or,
            "." => TokenType::Dot,
            "," => TokenType::Comma,
            ";" => TokenType::Semicolon,
            "+" => TokenType::Plus,
            "-" => TokenType::Minus,
            "/" => TokenType::Divide,
            "*" => TokenType::Multiply,
            "&" => TokenType::Reference,
            _ => {
                if token.starts_with('"') && token.ends_with('"') {
                    TokenType::ConstStringExpression
                } else if let Ok(_) = &self.value.parse::<i32>() {
                    TokenType::ConstIntegerExpression
                } else if let Ok(_) = &self.value.parse::<i32>() {
                    TokenType::ConstIntegerExpression
                } else {
                    TokenType::Identifier
                }
            }
        }
    }

    pub fn get_token_type(&mut self) -> TokenType {
        self.get_calculated_token_type()
    }
}