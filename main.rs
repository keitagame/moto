use std::collections::HashMap;
use std::fmt;
use std::io::{self, Write};

// ========== トークン定義 ==========
#[derive(Debug, Clone, PartialEq)]
enum Token {
    Number(f64),
    String(String),
    Ident(String),
    True, False, Null,
    Plus, Minus, Star, Slash, Percent,
    Equal, EqualEqual, NotEqual, Less, LessEqual, Greater, GreaterEqual,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Let, Fn, If, Else, While, For, Return, Break, Continue,
    Semicolon, Comma, Dot, Colon,
    And, Or, Not,
    Print, Input, Typeof,
    Eof,
}

// ========== 字句解析器 ==========
struct Lexer {
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    fn new(input: &str) -> Self {
        Lexer {
            input: input.chars().collect(),
            pos: 0,
        }
    }

    fn current(&self) -> Option<char> {
        if self.pos < self.input.len() {
            Some(self.input[self.pos])
        } else {
            None
        }
    }

    fn peek(&self, offset: usize) -> Option<char> {
        if self.pos + offset < self.input.len() {
            Some(self.input[self.pos + offset])
        } else {
            None
        }
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current() {
            if ch.is_whitespace() {
                self.advance();
            } else if ch == '/' && self.peek(1) == Some('/') {
                // 単一行コメント
                while let Some(c) = self.current() {
                    if c == '\n' {
                        break;
                    }
                    self.advance();
                }
            } else if ch == '/' && self.peek(1) == Some('*') {
                // 複数行コメント
                self.advance();
                self.advance();
                while let Some(c) = self.current() {
                    if c == '*' && self.peek(1) == Some('/') {
                        self.advance();
                        self.advance();
                        break;
                    }
                    self.advance();
                }
            } else {
                break;
            }
        }
    }

    fn read_number(&mut self) -> f64 {
        let start = self.pos;
        while let Some(ch) = self.current() {
            if ch.is_numeric() || ch == '.' {
                self.advance();
            } else {
                break;
            }
        }
        self.input[start..self.pos].iter().collect::<String>().parse().unwrap()
    }

    fn read_string(&mut self) -> String {
        self.advance(); // skip opening "
        let start = self.pos;
        while let Some(ch) = self.current() {
            if ch == '"' {
                break;
            }
            if ch == '\\' {
                self.advance();
            }
            self.advance();
        }
        let s = self.input[start..self.pos].iter().collect::<String>();
        self.advance(); // skip closing "
        s.replace("\\n", "\n").replace("\\t", "\t").replace("\\\"", "\"")
    }

    fn read_ident(&mut self) -> String {
        let start = self.pos;
        while let Some(ch) = self.current() {
            if ch.is_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }
        self.input[start..self.pos].iter().collect()
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.current() {
            None => Token::Eof,
            Some('"') => Token::String(self.read_string()),
            Some(ch) => {
                if ch.is_numeric() {
                    Token::Number(self.read_number())
                } else if ch.is_alphabetic() || ch == '_' {
                    let ident = self.read_ident();
                    match ident.as_str() {
                        "let" => Token::Let,
                        "fn" => Token::Fn,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "while" => Token::While,
                        "for" => Token::For,
                        "return" => Token::Return,
                        "break" => Token::Break,
                        "continue" => Token::Continue,
                        "true" => Token::True,
                        "false" => Token::False,
                        "null" => Token::Null,
                        "print" => Token::Print,
                        "input" => Token::Input,
                        "typeof" => Token::Typeof,
                        _ => Token::Ident(ident),
                    }
                } else {
                    self.advance();
                    match ch {
                        '+' => Token::Plus,
                        '-' => Token::Minus,
                        '*' => Token::Star,
                        '/' => Token::Slash,
                        '%' => Token::Percent,
                        '(' => Token::LParen,
                        ')' => Token::RParen,
                        '{' => Token::LBrace,
                        '}' => Token::RBrace,
                        '[' => Token::LBracket,
                        ']' => Token::RBracket,
                        ';' => Token::Semicolon,
                        ',' => Token::Comma,
                        '.' => Token::Dot,
                        ':' => Token::Colon,
                        '=' => {
                            if self.current() == Some('=') {
                                self.advance();
                                Token::EqualEqual
                            } else {
                                Token::Equal
                            }
                        }
                        '!' => {
                            if self.current() == Some('=') {
                                self.advance();
                                Token::NotEqual
                            } else {
                                Token::Not
                            }
                        }
                        '<' => {
                            if self.current() == Some('=') {
                                self.advance();
                                Token::LessEqual
                            } else {
                                Token::Less
                            }
                        }
                        '>' => {
                            if self.current() == Some('=') {
                                self.advance();
                                Token::GreaterEqual
                            } else {
                                Token::Greater
                            }
                        }
                        '&' => {
                            if self.current() == Some('&') {
                                self.advance();
                                Token::And
                            } else {
                                self.next_token()
                            }
                        }
                        '|' => {
                            if self.current() == Some('|') {
                                self.advance();
                                Token::Or
                            } else {
                                self.next_token()
                            }
                        }
                        _ => self.next_token(),
                    }
                }
            }
        }
    }
}

// ========== AST定義 ==========
#[derive(Debug, Clone)]
enum Expr {
    Number(f64),
    String(String),
    Bool(bool),
    Null,
    Ident(String),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Unary(UnOp, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Assign(String, Box<Expr>),
    Array(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Object(Vec<(String, Expr)>),
    Member(Box<Expr>, String),
}

#[derive(Debug, Clone)]
enum BinOp {
    Add, Sub, Mul, Div, Mod,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or,
}

#[derive(Debug, Clone)]
enum UnOp {
    Neg, Not,
}

#[derive(Debug, Clone)]
enum Stmt {
    Expr(Expr),
    Let(String, Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    For(String, Expr, Expr, Box<Stmt>),
    Function(String, Vec<String>, Box<Stmt>),
    Return(Option<Expr>),
    Break,
    Continue,
    Print(Vec<Expr>),
}

// ========== パーサー ==========
struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn current(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token::Eof)
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn expect(&mut self, expected: Token) -> bool {
        if self.current() == &expected {
            self.advance();
            true
        } else {
            false
        }
    }

    fn parse_program(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while self.current() != &Token::Eof {
            stmts.push(self.parse_stmt());
        }
        stmts
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.current() {
            Token::Let => self.parse_let(),
            Token::Fn => self.parse_function(),
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::For => self.parse_for(),
            Token::Return => self.parse_return(),
            Token::Break => {
                self.advance();
                self.expect(Token::Semicolon);
                Stmt::Break
            }
            Token::Continue => {
                self.advance();
                self.expect(Token::Semicolon);
                Stmt::Continue
            }
            Token::Print => self.parse_print(),
            Token::LBrace => self.parse_block(),
            _ => {
                let expr = self.parse_expr();
                self.expect(Token::Semicolon);
                Stmt::Expr(expr)
            }
        }
    }

    fn parse_print(&mut self) -> Stmt {
        self.advance(); // print
        self.expect(Token::LParen);
        let mut args = Vec::new();
        while self.current() != &Token::RParen {
            args.push(self.parse_expr());
            if self.current() != &Token::RParen {
                self.expect(Token::Comma);
            }
        }
        self.expect(Token::RParen);
        self.expect(Token::Semicolon);
        Stmt::Print(args)
    }

    fn parse_let(&mut self) -> Stmt {
        self.advance();
        let Token::Ident(name) = self.current().clone() else {
            panic!("Expected identifier");
        };
        self.advance();
        self.expect(Token::Equal);
        let expr = self.parse_expr();
        self.expect(Token::Semicolon);
        Stmt::Let(name, expr)
    }

    fn parse_function(&mut self) -> Stmt {
        self.advance();
        let Token::Ident(name) = self.current().clone() else {
            panic!("Expected function name");
        };
        self.advance();
        self.expect(Token::LParen);
        
        let mut params = Vec::new();
        while self.current() != &Token::RParen {
            let Token::Ident(param) = self.current().clone() else {
                panic!("Expected parameter name");
            };
            params.push(param);
            self.advance();
            if self.current() != &Token::RParen {
                self.expect(Token::Comma);
            }
        }
        self.expect(Token::RParen);
        
        let body = Box::new(self.parse_block());
        Stmt::Function(name, params, body)
    }

    fn parse_if(&mut self) -> Stmt {
        self.advance();
        self.expect(Token::LParen);
        let cond = self.parse_expr();
        self.expect(Token::RParen);
        let then_branch = Box::new(self.parse_stmt());
        let else_branch = if self.current() == &Token::Else {
            self.advance();
            Some(Box::new(self.parse_stmt()))
        } else {
            None
        };
        Stmt::If(cond, then_branch, else_branch)
    }

    fn parse_while(&mut self) -> Stmt {
        self.advance();
        self.expect(Token::LParen);
        let cond = self.parse_expr();
        self.expect(Token::RParen);
        let body = Box::new(self.parse_stmt());
        Stmt::While(cond, body)
    }

    fn parse_for(&mut self) -> Stmt {
        self.advance();
        self.expect(Token::LParen);
        let Token::Ident(var) = self.current().clone() else {
            panic!("Expected variable name");
        };
        self.advance();
        self.expect(Token::Equal);
        let start = self.parse_expr();
        self.expect(Token::Semicolon);
        let end = self.parse_expr();
        self.expect(Token::RParen);
        let body = Box::new(self.parse_stmt());
        Stmt::For(var, start, end, body)
    }

    fn parse_return(&mut self) -> Stmt {
        self.advance();
        let expr = if self.current() == &Token::Semicolon {
            None
        } else {
            Some(self.parse_expr())
        };
        self.expect(Token::Semicolon);
        Stmt::Return(expr)
    }

    fn parse_block(&mut self) -> Stmt {
        self.expect(Token::LBrace);
        let mut stmts = Vec::new();
        while self.current() != &Token::RBrace && self.current() != &Token::Eof {
            stmts.push(self.parse_stmt());
        }
        self.expect(Token::RBrace);
        Stmt::Block(stmts)
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Expr {
        let mut left = self.parse_and();
        while self.current() == &Token::Or {
            self.advance();
            let right = self.parse_and();
            left = Expr::Binary(Box::new(left), BinOp::Or, Box::new(right));
        }
        left
    }

    fn parse_and(&mut self) -> Expr {
        let mut left = self.parse_equality();
        while self.current() == &Token::And {
            self.advance();
            let right = self.parse_equality();
            left = Expr::Binary(Box::new(left), BinOp::And, Box::new(right));
        }
        left
    }

    fn parse_equality(&mut self) -> Expr {
        let mut left = self.parse_comparison();
        loop {
            let op = match self.current() {
                Token::EqualEqual => BinOp::Eq,
                Token::NotEqual => BinOp::Ne,
                _ => break,
            };
            self.advance();
            let right = self.parse_comparison();
            left = Expr::Binary(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn parse_comparison(&mut self) -> Expr {
        let mut left = self.parse_term();
        loop {
            let op = match self.current() {
                Token::Less => BinOp::Lt,
                Token::LessEqual => BinOp::Le,
                Token::Greater => BinOp::Gt,
                Token::GreaterEqual => BinOp::Ge,
                _ => break,
            };
            self.advance();
            let right = self.parse_term();
            left = Expr::Binary(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn parse_term(&mut self) -> Expr {
        let mut left = self.parse_factor();
        loop {
            let op = match self.current() {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_factor();
            left = Expr::Binary(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn parse_factor(&mut self) -> Expr {
        let mut left = self.parse_unary();
        loop {
            let op = match self.current() {
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::Percent => BinOp::Mod,
                _ => break,
            };
            self.advance();
            let right = self.parse_unary();
            left = Expr::Binary(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn parse_unary(&mut self) -> Expr {
        match self.current() {
            Token::Minus => {
                self.advance();
                Expr::Unary(UnOp::Neg, Box::new(self.parse_unary()))
            }
            Token::Not => {
                self.advance();
                Expr::Unary(UnOp::Not, Box::new(self.parse_unary()))
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Expr {
        let mut expr = self.parse_primary();
        loop {
            match self.current() {
                Token::LParen => {
                    self.advance();
                    let mut args = Vec::new();
                    while self.current() != &Token::RParen {
                        args.push(self.parse_expr());
                        if self.current() != &Token::RParen {
                            self.expect(Token::Comma);
                        }
                    }
                    self.expect(Token::RParen);
                    expr = Expr::Call(Box::new(expr), args);
                }
                Token::LBracket => {
                    self.advance();
                    let index = self.parse_expr();
                    self.expect(Token::RBracket);
                    expr = Expr::Index(Box::new(expr), Box::new(index));
                }
                Token::Dot => {
                    self.advance();
                    let Token::Ident(member) = self.current().clone() else {
                        panic!("Expected member name");
                    };
                    self.advance();
                    expr = Expr::Member(Box::new(expr), member);
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_primary(&mut self) -> Expr {
        match self.current().clone() {
            Token::Number(n) => {
                self.advance();
                Expr::Number(n)
            }
            Token::String(s) => {
                self.advance();
                Expr::String(s)
            }
            Token::True => {
                self.advance();
                Expr::Bool(true)
            }
            Token::False => {
                self.advance();
                Expr::Bool(false)
            }
            Token::Null => {
                self.advance();
                Expr::Null
            }
            Token::Ident(name) => {
                self.advance();
                if self.current() == &Token::Equal {
                    self.advance();
                    let value = self.parse_expr();
                    Expr::Assign(name, Box::new(value))
                } else {
                    Expr::Ident(name)
                }
            }
            Token::LBracket => {
                self.advance();
                let mut elements = Vec::new();
                while self.current() != &Token::RBracket {
                    elements.push(self.parse_expr());
                    if self.current() != &Token::RBracket {
                        self.expect(Token::Comma);
                    }
                }
                self.expect(Token::RBracket);
                Expr::Array(elements)
            }
            Token::LBrace => {
                self.advance();
                let mut pairs = Vec::new();
                while self.current() != &Token::RBrace {
                    let Token::Ident(key) = self.current().clone() else {
                        panic!("Expected key");
                    };
                    self.advance();
                    self.expect(Token::Colon);
                    let value = self.parse_expr();
                    pairs.push((key, value));
                    if self.current() != &Token::RBrace {
                        self.expect(Token::Comma);
                    }
                }
                self.expect(Token::RBrace);
                Expr::Object(pairs)
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expr();
                self.expect(Token::RParen);
                expr
            }
            Token::Input => {
                self.advance();
                self.expect(Token::LParen);
                self.expect(Token::RParen);
                Expr::Call(Box::new(Expr::Ident("__input".to_string())), vec![])
            }
            Token::Typeof => {
                self.advance();
                self.expect(Token::LParen);
                let expr = self.parse_expr();
                self.expect(Token::RParen);
                Expr::Call(Box::new(Expr::Ident("__typeof".to_string())), vec![expr])
            }
            _ => panic!("Unexpected token: {:?}", self.current()),
        }
    }
}

// ========== 値型 ==========
#[derive(Debug, Clone)]
enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Null,
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
    Function(Vec<String>, Box<Stmt>),
    NativeFunction(fn(&mut Interpreter, Vec<Value>) -> Value),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Null => write!(f, "null"),
            Value::Array(arr) => {
                write!(f, "[")?;
                for (i, v) in arr.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Object(obj) => {
                write!(f, "{{")?;
                for (i, (k, v)) in obj.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, "}}")
            }
            Value::Function(_, _) => write!(f, "<function>"),
            Value::NativeFunction(_) => write!(f, "<native function>"),
        }
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Null => false,
            Value::Number(n) => *n != 0.0,
            Value::String(s) => !s.is_empty(),
            _ => true,
        }
    }

    fn to_number(&self) -> f64 {
        match self {
            Value::Number(n) => *n,
            Value::Bool(true) => 1.0,
            Value::Bool(false) => 0.0,
            Value::String(s) => s.parse().unwrap_or(0.0),
            _ => 0.0,
        }
    }

    fn type_name(&self) -> &str {
        match self {
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Bool(_) => "boolean",
            Value::Null => "null",
            Value::Array(_) => "array",
            Value::Object(_) => "object",
            Value::Function(_, _) | Value::NativeFunction(_) => "function",
        }
    }
}

// ========== フロー制御 ==========
enum FlowControl {
    None,
    Return(Option<Value>),
    Break,
    Continue,
}

// ========== インタプリタ ==========
struct Interpreter {
    globals: HashMap<String, Value>,
    locals: Vec<HashMap<String, Value>>,
}

impl Interpreter {
    fn new() -> Self {
        let mut interpreter = Interpreter {
            globals: HashMap::new(),
            locals: vec![HashMap::new()],
        };
        
        // 組み込み関数の登録
        interpreter.globals.insert(
            "__input".to_string(),
            Value::NativeFunction(|_, _| {
                print!("> ");
                io::stdout().flush().unwrap();
                let mut input = String::new();
                io::stdin().read_line(&mut input).unwrap();
                Value::String(input.trim().to_string())
            })
        );
        
        interpreter.globals.insert(
            "__typeof".to_string(),
            Value::NativeFunction(|_, args| {
                Value::String(args[0].type_name().to_string())
            })
        );
        
        interpreter.globals.insert(
            "len".to_string(),
            Value::NativeFunction(|_, args| {
                match &args[0] {
                    Value::Array(arr) => Value::Number(arr.len() as f64),
                    Value::String(s) => Value::Number(s.len() as f64),
                    Value::Object(obj) => Value::Number(obj.len() as f64),
                    _ => Value::Number(0.0),
                }
            })
        );
        
        interpreter.globals.insert(
            "push".to_string(),
            Value::NativeFunction(|_, args| {
                if let Value::Array(arr) = &args[0] {
                    let mut new_arr = arr.clone();
                    new_arr.push(args[1].clone());
                    Value::Array(new_arr)
                } else {
                    Value::Null
                }
            })
        );
        
        interpreter.globals.insert(
            "str".to_string(),
            Value::NativeFunction(|_, args| {
                Value::String(format!("{}", args[0]))
            })
        );
        
        interpreter.globals.insert(
            "num".to_string(),
            Value::NativeFunction(|_, args| {
                Value::Number(args[0].to_number())
            })
        );
        
        interpreter
    }

    fn get_var(&self, name: &str) -> Value {
        for scope in self.locals.iter().rev() {
            if let Some(val) = scope.get(name) {
                return val.clone();
            }
        }
        self.globals.get(name).cloned().unwrap_or(Value::Null)
    }

    fn set_var(&mut self, name: String, value: Value) {
        if let Some(scope) = self.locals.last_mut() {
            scope.insert(name, value);
        }
    }

    fn push_scope(&mut self) {
        self.locals.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.locals.pop();
    }

    fn eval_expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Number(n) => Value::Number(*n),
            Expr::String(s) => Value::String(s.clone()),
            Expr::Bool(b) => Value::Bool(*b),
            Expr::Null => Value::Null,
            Expr::Ident(name) => self.get_var(name),
            Expr::Binary(left, op, right) => {
                let l = self.eval_expr(left);
                let r = self.eval_expr(right);
                
                match op {
                    BinOp::Add => match (&l, &r) {
                        (Value::String(s1), Value::String(s2)) => Value::String(format!("{}{}", s1, s2)),
                        (Value::String(s), v) | (v, Value::String(s)) => Value::String(format!("{}{}", s, v)),
                        _ => Value::Number(l.to_number() + r.to_number()),
                    },
                    BinOp::Sub => Value::Number(l.to_number() - r.to_number()),
                    BinOp::Mul => Value::Number(l.to_number() * r.to_number()),
                    BinOp::Div => Value::Number(l.to_number() / r.to_number()),
                    BinOp::Mod => Value::Number(l.to_number() % r.to_number()),
                    BinOp::Eq => Value::Bool(format!("{}", l) == format!("{}", r)),
                    BinOp::Ne => Value::Bool(format!("{}", l) != format!("{}", r)),
                    BinOp::Lt => Value::Bool(l.to_number() < r.to_number()),
                    BinOp::Le => Value::Bool(l.to_number() <= r.to_number()),
                    BinOp::Gt => Value::Bool(l.to_number() > r.to_number()),
                    BinOp::Ge => Value::Bool(l.to_number() >= r.to_number()),
                    BinOp::And => Value::Bool(l.is_truthy() && r.is_truthy()),
                    BinOp::Or => Value::Bool(l.is_truthy() || r.is_truthy()),
                }
            }
            Expr::Unary(op, expr) => {
                let val = self.eval_expr(expr);
                match op {
                    UnOp::Neg => Value::Number(-val.to_number()),
                    UnOp::Not => Value::Bool(!val.is_truthy()),
                }
            }
            Expr::Call(func_expr, args) => {
                let func = self.eval_expr(func_expr);
                let arg_vals: Vec<Value> = args.iter().map(|a| self.eval_expr(a)).collect();
                
                match func {
                    Value::Function(params, body) => {
                        self.push_scope();
                        for (param, arg) in params.iter().zip(arg_vals.iter()) {
                            self.set_var(param.clone(), arg.clone());
                        }
                        
                        let result = match self.eval_stmt(&body) {
                            FlowControl::Return(Some(val)) => val,
                            _ => Value::Null,
                        };
                        
                        self.pop_scope();
                        result
                    }
                    Value::NativeFunction(f) => f(self, arg_vals),
                    _ => Value::Null,
                }
            }
            Expr::Assign(name, expr) => {
                let val = self.eval_expr(expr);
                self.set_var(name.clone(), val.clone());
                val
            }
            Expr::Array(elements) => {
                let values: Vec<Value> = elements.iter().map(|e| self.eval_expr(e)).collect();
                Value::Array(values)
            }
            Expr::Index(arr_expr, idx_expr) => {
                let arr = self.eval_expr(arr_expr);
                let idx = self.eval_expr(idx_expr);
                
                match (arr, idx) {
                    (Value::Array(arr), Value::Number(n)) => {
                        let i = n as usize;
                        arr.get(i).cloned().unwrap_or(Value::Null)
                    }
                    (Value::Object(obj), Value::String(key)) => {
                        obj.get(&key).cloned().unwrap_or(Value::Null)
                    }
                    _ => Value::Null,
                }
            }
            Expr::Object(pairs) => {
                let mut obj = HashMap::new();
                for (key, value_expr) in pairs {
                    let value = self.eval_expr(value_expr);
                    obj.insert(key.clone(), value);
                }
                Value::Object(obj)
            }
            Expr::Member(obj_expr, member) => {
                let obj = self.eval_expr(obj_expr);
                if let Value::Object(map) = obj {
                    map.get(member).cloned().unwrap_or(Value::Null)
                } else {
                    Value::Null
                }
            }
        }
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> FlowControl {
        match stmt {
            Stmt::Expr(expr) => {
                self.eval_expr(expr);
                FlowControl::None
            }
            Stmt::Let(name, expr) => {
                let val = self.eval_expr(expr);
                self.set_var(name.clone(), val);
                FlowControl::None
            }
            Stmt::Block(stmts) => {
                for stmt in stmts {
                    match self.eval_stmt(stmt) {
                        FlowControl::None => continue,
                        flow => return flow,
                    }
                }
                FlowControl::None
            }
            Stmt::If(cond, then_branch, else_branch) => {
                let c = self.eval_expr(cond);
                if c.is_truthy() {
                    self.eval_stmt(then_branch)
                } else if let Some(else_branch) = else_branch {
                    self.eval_stmt(else_branch)
                } else {
                    FlowControl::None
                }
            }
            Stmt::While(cond, body) => {
                loop {
                    let c = self.eval_expr(cond);
                    if !c.is_truthy() {
                        break;
                    }
                    match self.eval_stmt(body) {
                        FlowControl::Break => break,
                        FlowControl::Continue => continue,
                        FlowControl::Return(val) => return FlowControl::Return(val),
                        FlowControl::None => {}
                    }
                }
                FlowControl::None
            }
            Stmt::For(var, start, end, body) => {
                let s = self.eval_expr(start).to_number();
                let e = self.eval_expr(end).to_number();
                
                let mut i = s;
                while i < e {
                    self.set_var(var.clone(), Value::Number(i));
                    match self.eval_stmt(body) {
                        FlowControl::Break => break,
                        FlowControl::Continue => {
                            i += 1.0;
                            continue;
                        }
                        FlowControl::Return(val) => return FlowControl::Return(val),
                        FlowControl::None => {}
                    }
                    i += 1.0;
                }
                FlowControl::None
            }
            Stmt::Function(name, params, body) => {
                let func = Value::Function(params.clone(), body.clone());
                self.globals.insert(name.clone(), func);
                FlowControl::None
            }
            Stmt::Return(expr) => {
                let val = expr.as_ref().map(|e| self.eval_expr(e));
                FlowControl::Return(val)
            }
            Stmt::Break => FlowControl::Break,
            Stmt::Continue => FlowControl::Continue,
            Stmt::Print(args) => {
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { print!(" "); }
                    print!("{}", self.eval_expr(arg));
                }
                println!();
                FlowControl::None
            }
        }
    }

    fn run(&mut self, program: &[Stmt]) {
        for stmt in program {
            self.eval_stmt(stmt);
        }
    }
}

// ========== メイン実行 ==========
fn main() {
    let code = r#"
        // ========== 基本的な型のデモ ==========
        print("=== 型システムのデモ ===");
        
        let num = 42;
        let text = "Hello, World!";
        let flag = true;
        let empty = null;
        
        print("Number:", num);
        print("String:", text);
        print("Boolean:", flag);
        print("Null:", empty);
        
        // ========== 配列 ==========
        print("\n=== 配列のデモ ===");
        
        let arr = [1, 2, 3, 4, 5];
        print("Array:", arr);
        print("Length:", len(arr));
        print("First element:", arr[0]);
        print("Last element:", arr[4]);
        
        // 配列の操作
        let arr2 = push(arr, 6);
        print("After push:", arr2);
        
        // ========== オブジェクト ==========
        print("\n=== オブジェクトのデモ ===");
        
        let person = {
            name: "Alice",
            age: 30,
            city: "Tokyo"
        };
        
        print("Person:", person);
        print("Name:", person.name);
        print("Age:", person["age"]);
        
        // ========== 文字列操作 ==========
        print("\n=== 文字列操作のデモ ===");
        
        let greeting = "Hello" + " " + "World";
        print(greeting);
        
        let message = "Number is: " + str(123);
        print(message);
        
        // ========== 型チェック ==========
        print("\n=== 型チェックのデモ ===");
        
        print("typeof 42:", typeof(42));
        print("typeof 'text':", typeof("text"));
        print("typeof true:", typeof(true));
        print("typeof [1,2,3]:", typeof([1, 2, 3]));
        print("typeof {x: 1}:", typeof({x: 1}));
        
        // ========== 高度な関数 ==========
        print("\n=== 高度な関数のデモ ===");
        
        // マップ関数（配列の各要素を2倍にする）
        fn map_double(arr) {
            let result = [];
            for (i = 0; i < len(arr)) {
                result = push(result, arr[i] * 2);
            }
            return result;
        }
        
        let numbers = [1, 2, 3, 4, 5];
        let doubled = map_double(numbers);
        print("Original:", numbers);
        print("Doubled:", doubled);
        
        // フィルター関数（偶数のみを抽出）
        fn filter_even(arr) {
            let result = [];
            for (i = 0; i < len(arr)) {
                if (arr[i] % 2 == 0) {
                    result = push(result, arr[i]);
                }
            }
            return result;
        }
        
        let evens = filter_even([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
        print("Even numbers:", evens);
        
        // リデュース関数（合計を計算）
        fn sum(arr) {
            let total = 0;
            for (i = 0; i < len(arr)) {
                total = total + arr[i];
            }
            return total;
        }
        
        print("Sum of [1,2,3,4,5]:", sum([1, 2, 3, 4, 5]));
        
        // ========== クロージャー風の例 ==========
        print("\n=== カウンター関数のデモ ===");
        
        let counter = 0;
        
        fn increment() {
            counter = counter + 1;
            return counter;
        }
        
        print("Count:", increment());
        print("Count:", increment());
        print("Count:", increment());
        
        // ========== 再帰の例 ==========
        print("\n=== 再帰のデモ ===");
        
        fn fibonacci(n) {
            if (n <= 1) {
                return n;
            }
            return fibonacci(n - 1) + fibonacci(n - 2);
        }
        
        print("Fibonacci(10):", fibonacci(10));
        
        // ========== 論理演算 ==========
        print("\n=== 論理演算のデモ ===");
        
        print("true && true:", true && true);
        print("true && false:", true && false);
        print("true || false:", true || false);
        print("!true:", !true);
        print("!false:", !false);
        
        // ========== エラーハンドリング風 ==========
        print("\n=== 値のチェックのデモ ===");
        
        fn safe_divide(a, b) {
            if (b == 0) {
                print("Error: Division by zero!");
                return null;
            }
            return a / b;
        }
        
        print("10 / 2 =", safe_divide(10, 2));
        print("10 / 0 =", safe_divide(10, 0));
        
        // ========== 完了メッセージ ==========
        print("\n=== すべてのテストが完了しました! ===");
    "#;

    println!("完全なインタプリタ言語 - 実行開始\n");

    // 字句解析
    let mut lexer = Lexer::new(code);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token();
        if token == Token::Eof {
            tokens.push(token);
            break;
        }
        tokens.push(token);
    }

    // 構文解析
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program();

    // 実行
    let mut interpreter = Interpreter::new();
    interpreter.run(&program);
}

