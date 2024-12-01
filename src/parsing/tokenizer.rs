use std::collections::HashMap;
use crate::parsing::token::{Token, TokenType};

#[derive(Clone)]
pub struct Tokenized {
    current_token_index: i32,
    tokens: Vec<Token>,

    remember_list: Vec<usize>,
}

impl Tokenized {
    pub fn new() -> Tokenized {
        Tokenized {
            current_token_index: -1,
            tokens: vec![],
            remember_list: vec![],
        }
    }

    pub fn is_currently_in_range(&self) -> bool {
        (self.current_token_index as usize)  < self.get_token_count()
    }

    pub fn is_in_range(&self, index: usize) -> bool {
        index < self.get_token_count() && index >= 0
    }

    pub fn get_token_count(&self) -> usize {
        self.tokens.len()
    }

    pub fn get_token(&self, index : usize) -> Option<Token> {
        if index >= self.get_token_count() {
            return None;
        }

        Some(self.tokens[index].clone())
    }

    pub fn get_current_token(&self) -> Option<Token> {
        if !self.is_currently_in_range() {
            return None;
        }

        Some(self.tokens[self.current_token_index as usize].clone())
    }

    pub fn get_current_token_index(&self) -> usize {
        self.current_token_index as usize
    }

    pub fn position(&self) -> usize {
        self.get_current_token_index()
    }

    pub fn set_current_token_index(&mut self, index: usize) {
        self.current_token_index = index as i32;
    }

    pub fn next(&mut self) -> Option<&Token> {
        self.current_token_index += 1;
        if !self.is_currently_in_range() {
            return None;
        }
        Some(&self.tokens[self.current_token_index as usize])
    }

    pub fn peek_next(&self, jump_count : usize) -> Option<&Token> {
        if self.is_in_range(self.current_token_index as usize + jump_count) {
            return None;
        }

        Some(&self.tokens[self.current_token_index as usize + jump_count])
    }

    pub fn prev(&mut self) -> Option<&Token> {
        if self.current_token_index as usize >= self.get_token_count() {
            return None;
        }

        self.current_token_index -= 1;
        if self.current_token_index < 0 {
            return None;
        }

        Some(&self.tokens[self.current_token_index as usize])
    }

    pub fn peek_prev(&self, jump_count : usize) -> Option<&Token> {
        if self.is_in_range(self.current_token_index as usize - jump_count) {
            return None;
        }

        Some(&self.tokens[self.current_token_index as usize - jump_count])
    }

    pub fn remember(&mut self) -> usize {
        self.remember_list.push(self.current_token_index as usize);
        self.remember_list.len() - 1
    }

    pub fn forget(&mut self) {
        if let Some(value) = self.remember_list.pop() {
            self.current_token_index = value as i32;
        } else {
            self.current_token_index = 0;
        }
    }

    pub fn forget_until(&mut self, index : usize) {
        loop {
            if self.remember_list.len() <= index {
                break;
            }

            self.forget();
        }
    }

    pub fn reset_index(&mut self) {
        self.current_token_index = 0;
    }
}

impl Iterator for Tokenized {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if (self.current_token_index as usize) < self.tokens.len() {
            let token = self.tokens[self.current_token_index as usize].clone();
            self.current_token_index += 1;
            Some(token)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenizerState {
    Unknown,
    String,
    StartStopComment,
    LineComment,
    StopChar,
}

pub struct Tokenizer {
    source: String,

    keywords: HashMap<String, TokenType>,
    comment_start_token: String,
    comment_end_token: String,
    string_chars: Vec<char>,
    string_escape_char: char,
    stop_chars: Vec<char>,
    ignore_chars: Vec<char>,
    keep_together: Vec<String>,


    buffer: String,
    current_string_char: char,
    string_escaped: bool,
    state: TokenizerState,
    current_line_pos: usize,
    current_char_index: usize,
}

impl Tokenizer {
    pub fn new() -> Tokenizer {
        Tokenizer {
            source: String::new(),
            keywords: Default::default(),
            comment_start_token: "/*".to_owned(),
            comment_end_token: "*/".to_owned(),
            string_chars: vec!['"', '\'',],
            string_escape_char: '\\',
            stop_chars: vec![
                ' ', ',', '.', '*', '/', '|', '&', '[', ']', '(', ')', '{', '}', '+', '$', '-', '@',
                '\n', '\r', ';', '^', ':', '=', '<', '>', '"', '\'',
            ],
            ignore_chars: vec![' ', '\n', '\r', '"', '\''],
            keep_together: vec![
                "==".to_owned(),
                "!=".to_owned(),
                ">=".to_owned(),
                "<=".to_owned(),
                "&&".to_owned(),
                "||".to_owned(),
                "++".to_owned(),
                "--".to_owned(),
                "..".to_owned()
            ],
            buffer: String::new(),
            current_string_char: ' ',
            string_escaped: false,
            state: TokenizerState::Unknown,
            current_line_pos: 0,
            current_char_index: 0,
        }
    }

    pub fn tokenize(&mut self, source: &str) -> Tokenized {
        let mut tokenized = Tokenized::new();
        self.source = source.to_string();

        self.buffer.clear();
        self.state = TokenizerState::Unknown;
        self.current_char_index = 0;
        self.current_line_pos = 0;

        let chars: Vec<char> = self.source.chars().collect();
        while self.current_char_index < chars.len() {
            let current_char = chars[self.current_char_index];

            let next_char = if self.current_char_index + 1 < chars.len() {
                Some(chars[self.current_char_index + 1])
            } else {
                None
            };

            if current_char == '\n' {
                self.current_line_pos = 0;
            }

            self.do_read_char(current_char, next_char, &mut tokenized);

            self.current_char_index += 1;
            self.current_line_pos += 1;
        }

        // Add any remaining buffer as a token
        if !self.buffer.is_empty() {
            self.add_token(&mut tokenized);
        }

        tokenized
    }

    pub fn do_read_char(&mut self, current_char: char, next_char: Option<char>, tokenized: &mut Tokenized) {
        if self.state == TokenizerState::Unknown {
            if self.string_chars.contains(&current_char) {
                self.set_state_and_add_token(TokenizerState::String, tokenized);
                self.current_string_char = current_char;
                self.buffer.push(current_char);
                return;
            } else if current_char == '/' && next_char == Some('*') {
                self.set_state_and_add_token(TokenizerState::StartStopComment, tokenized);
                return;
            } else if current_char == '/' && next_char == Some('/') {
                self.set_state_and_add_token(TokenizerState::LineComment, tokenized);
                return;
            }

            if self.stop_chars.contains(&current_char) {
                self.set_state_and_add_token(TokenizerState::StopChar, tokenized);
            }
        }

        match self.state {
            TokenizerState::Unknown => {
                self.buffer.push(current_char);
            }

            TokenizerState::String => {
                if current_char == self.string_escape_char {
                    self.string_escaped = true;
                }

                self.buffer.push(current_char);

                if !self.string_escaped && current_char == self.current_string_char {
                    self.state = TokenizerState::Unknown;
                }

                if current_char != self.string_escape_char && self.string_escaped {
                    self.string_escaped = false;
                }
            }

            TokenizerState::StartStopComment => {
                if current_char == '*' && next_char == Some('/') {
                    self.state = TokenizerState::Unknown;
                }
            }

            TokenizerState::LineComment => {
                if current_char == '\n' || current_char == '\r' {
                    self.set_state_and_add_token(TokenizerState::Unknown, tokenized);
                }
            }

            TokenizerState::StopChar => {
                let mut found_together = false;
                for keep_together in &self.keep_together {
                    if self.is_next_sequence(keep_together) {
                        self.buffer.push_str(keep_together);
                        self.advance_chars(keep_together.len() - 1);
                        found_together = true;
                        break;
                    }
                }

                if !found_together && !self.ignore_chars.contains(&current_char) {
                    self.buffer.push(current_char);
                }

                self.set_state_and_add_token(TokenizerState::Unknown, tokenized);
            }
        }
    }

    fn is_next_sequence(&self, sequence: &str) -> bool {
        // Try to get the slice starting from current_char_index - 1
        let start_index = self.current_char_index;
        let end_index = self.current_char_index + sequence.len();

        // Safely attempt to create the slice, returns None if out of bounds
        if let Some(slice) = self.source.get(start_index..end_index) {
            // Compare the slice to the original sequence
            if slice == sequence {
                return true;
            }
        }
        // If the slice couldn't be created or doesn't match, return false
        false
    }

    fn advance_chars(&mut self, count: usize) {
        self.current_char_index += count;
    }

    fn add_token(&mut self, tokenized: &mut Tokenized) {
        if self.buffer.is_empty() {
            return;
        }

        let token_str = self.buffer.clone();
        tokenized.tokens.push(Token::new(token_str.to_string(), self.current_char_index as u32, 0));
        self.buffer.clear();
    }

    fn set_state_and_add_token(&mut self, state: TokenizerState, tokenized: &mut Tokenized) {
        self.state = state;
        self.add_token(tokenized);
    }
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    fn prepare_test(input : &str) -> Tokenized {
        let mut tokenizer = Tokenizer::new();
        let tokenized = tokenizer.tokenize(input);
        tokenized
    }

    #[test]
    fn test_word_space_word() {
        let tokenized = self::prepare_test("test test");

        assert_eq!(tokenized.get_token_count(), 2);
    }

    #[test]
    fn test_word_stopchar_word() {
        let tokenized = self::prepare_test("test-test");

        assert_eq!(tokenized.get_token_count(), 3);
    }

    #[test]
    fn test_word_stopchar_ignorechar_word() {
        let tokenized = self::prepare_test("test{ test");

        assert_eq!(tokenized.get_token_count(), 3);
    }

    #[test]
    fn test_word_pair_word() {
        let tokenized = self::prepare_test("test>=test");

        assert_eq!(tokenized.get_token_count(), 3);
    }
}