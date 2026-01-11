use std::collections::HashMap;
use std::env;
use std::fs;
use std::process::Command;

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Number(i64),
    Ident(String),
    Plus, Minus, Star, Slash, Percent,
    Eq, EqEq, NotEq, Lt, Le, Gt, Ge,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Semicolon, Comma, Ampersand, Pipe,
    Let, Function, If, Else, While, Return, Syscall,
    Eof,
}

struct Lexer {
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    fn new(input: &str) -> Self {
        Lexer { input: input.chars().collect(), pos: 0 }
    }

    fn peek(&self) -> Option<char> {
        if self.pos < self.input.len() { Some(self.input[self.pos]) } else { None }
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.peek();
        self.pos += 1;
        c
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() { self.advance(); } else { break; }
        }
    }

    fn read_number(&mut self) -> i64 {
        let mut num = 0;
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                num = num * 10 + (c as i64 - '0' as i64);
                self.advance();
            } else { break; }
        }
        num
    }

    fn read_ident(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                s.push(c);
                self.advance();
            } else { break; }
        }
        s
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        match self.peek() {
            None => Token::Eof,
            Some(c) => {
                if c.is_ascii_digit() {
                    Token::Number(self.read_number())
                } else if c.is_alphabetic() || c == '_' {
                    let ident = self.read_ident();
                    match ident.as_str() {
                        "let" => Token::Let,
                        "function" => Token::Function,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "while" => Token::While,
                        "return" => Token::Return,
                        "syscall" => Token::Syscall,
                        _ => Token::Ident(ident),
                    }
                } else {
                    self.advance();
                    match c {
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
                        '&' => Token::Ampersand,
                        '|' => Token::Pipe,
                        '=' => {
                            if self.peek() == Some('=') {
                                self.advance();
                                Token::EqEq
                            } else {
                                Token::Eq
                            }
                        }
                        '!' => {
                            if self.peek() == Some('=') {
                                self.advance();
                                Token::NotEq
                            } else {
                                panic!("Unexpected !");
                            }
                        }
                        '<' => {
                            if self.peek() == Some('=') {
                                self.advance();
                                Token::Le
                            } else {
                                Token::Lt
                            }
                        }
                        '>' => {
                            if self.peek() == Some('=') {
                                self.advance();
                                Token::Ge
                            } else {
                                Token::Gt
                            }
                        }
                        _ => panic!("Unexpected char: {}", c),
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Num(i64),
    Var(String),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Assign(String, Box<Expr>),
    Call(String, Vec<Expr>),
    Deref(Box<Expr>),
    AddrOf(String),
    Index(Box<Expr>, Box<Expr>),
    Syscall(Vec<Expr>),
}

#[derive(Debug, Clone, Copy)]
enum BinOp { Add, Sub, Mul, Div, Mod, Eq, Ne, Lt, Le, Gt, Ge, BitAnd, BitOr }

#[derive(Debug, Clone)]
enum Stmt {
    Expr(Expr),
    Let(String, Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Return(Expr),
}

#[derive(Debug, Clone)]
struct Function {
    name: String,
    params: Vec<String>,
    body: Vec<Stmt>,
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> &Token {
        if self.pos < self.tokens.len() { &self.tokens[self.pos] } else { &Token::Eof }
    }

    fn advance(&mut self) -> Token {
        let t = self.peek().clone();
        self.pos += 1;
        t
    }

    fn expect(&mut self, tok: Token) {
        let t = self.advance();
        if t != tok { panic!("Expected {:?}, got {:?}", tok, t); }
    }

    fn parse_program(&mut self) -> Vec<Function> {
        let mut funcs = Vec::new();
        while *self.peek() != Token::Eof {
            funcs.push(self.parse_function());
        }
        funcs
    }

    fn parse_function(&mut self) -> Function {
        self.expect(Token::Function);
        let name = if let Token::Ident(n) = self.advance() { n } else { panic!() };
        self.expect(Token::LParen);
        let mut params = Vec::new();
        if *self.peek() != Token::RParen {
            loop {
                if let Token::Ident(p) = self.advance() { params.push(p); } else { panic!() }
                if *self.peek() == Token::Comma { self.advance(); } else { break; }
            }
        }
        self.expect(Token::RParen);
        self.expect(Token::LBrace);
        let mut body = Vec::new();
        while *self.peek() != Token::RBrace {
            body.push(self.parse_stmt());
        }
        self.expect(Token::RBrace);
        Function { name, params, body }
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.peek() {
            Token::Let => {
                self.advance();
                let name = if let Token::Ident(n) = self.advance() { n } else { panic!() };
                self.expect(Token::Eq);
                let expr = self.parse_expr();
                self.expect(Token::Semicolon);
                Stmt::Let(name, expr)
            }
            Token::LBrace => {
                self.advance();
                let mut stmts = Vec::new();
                while *self.peek() != Token::RBrace {
                    stmts.push(self.parse_stmt());
                }
                self.expect(Token::RBrace);
                Stmt::Block(stmts)
            }
            Token::If => {
                self.advance();
                self.expect(Token::LParen);
                let cond = self.parse_expr();
                self.expect(Token::RParen);
                let then = Box::new(self.parse_stmt());
                let els = if *self.peek() == Token::Else {
                    self.advance();
                    Some(Box::new(self.parse_stmt()))
                } else { None };
                Stmt::If(cond, then, els)
            }
            Token::While => {
                self.advance();
                self.expect(Token::LParen);
                let cond = self.parse_expr();
                self.expect(Token::RParen);
                let body = Box::new(self.parse_stmt());
                Stmt::While(cond, body)
            }
            Token::Return => {
                self.advance();
                let expr = self.parse_expr();
                self.expect(Token::Semicolon);
                Stmt::Return(expr)
            }
            _ => {
                let expr = self.parse_expr();
                self.expect(Token::Semicolon);
                Stmt::Expr(expr)
            }
        }
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_assign()
    }

    fn parse_assign(&mut self) -> Expr {
        let expr = self.parse_or();
        if *self.peek() == Token::Eq {
            if let Expr::Var(name) = expr {
                self.advance();
                let rhs = self.parse_assign();
                return Expr::Assign(name, Box::new(rhs));
            }
        }
        expr
    }

    fn parse_or(&mut self) -> Expr {
        let mut left = self.parse_and();
        while *self.peek() == Token::Pipe {
            self.advance();
            let right = self.parse_and();
            left = Expr::Binary(Box::new(left), BinOp::BitOr, Box::new(right));
        }
        left
    }

    fn parse_and(&mut self) -> Expr {
        let mut left = self.parse_equality();
        while *self.peek() == Token::Ampersand {
            self.advance();
            let right = self.parse_equality();
            left = Expr::Binary(Box::new(left), BinOp::BitAnd, Box::new(right));
        }
        left
    }

    fn parse_equality(&mut self) -> Expr {
        let mut left = self.parse_relational();
        loop {
            let op = match self.peek() {
                Token::EqEq => BinOp::Eq,
                Token::NotEq => BinOp::Ne,
                _ => break,
            };
            self.advance();
            let right = self.parse_relational();
            left = Expr::Binary(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn parse_relational(&mut self) -> Expr {
        let mut left = self.parse_additive();
        loop {
            let op = match self.peek() {
                Token::Lt => BinOp::Lt,
                Token::Le => BinOp::Le,
                Token::Gt => BinOp::Gt,
                Token::Ge => BinOp::Ge,
                _ => break,
            };
            self.advance();
            let right = self.parse_additive();
            left = Expr::Binary(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn parse_additive(&mut self) -> Expr {
        let mut left = self.parse_multiplicative();
        loop {
            let op = match self.peek() {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative();
            left = Expr::Binary(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn parse_multiplicative(&mut self) -> Expr {
        let mut left = self.parse_unary();
        loop {
            let op = match self.peek() {
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
        match self.peek() {
            Token::Star => {
                self.advance();
                Expr::Deref(Box::new(self.parse_unary()))
            }
            Token::Ampersand => {
                self.advance();
                if let Token::Ident(name) = self.advance() {
                    Expr::AddrOf(name)
                } else { panic!() }
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Expr {
        let mut expr = self.parse_primary();
        loop {
            match self.peek() {
                Token::LBracket => {
                    self.advance();
                    let idx = self.parse_expr();
                    self.expect(Token::RBracket);
                    expr = Expr::Index(Box::new(expr), Box::new(idx));
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_primary(&mut self) -> Expr {
        match self.peek() {
            Token::Number(n) => {
                let n = *n;
                self.advance();
                Expr::Num(n)
            }
            Token::Ident(name) => {
                let name = name.clone();
                self.advance();
                if *self.peek() == Token::LParen {
                    self.advance();
                    let mut args = Vec::new();
                    if *self.peek() != Token::RParen {
                        loop {
                            args.push(self.parse_expr());
                            if *self.peek() == Token::Comma { self.advance(); } else { break; }
                        }
                    }
                    self.expect(Token::RParen);
                    Expr::Call(name, args)
                } else {
                    Expr::Var(name)
                }
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expr();
                self.expect(Token::RParen);
                expr
            }
            Token::Syscall => {
                self.advance();
                self.expect(Token::LParen);
                let mut args = Vec::new();
                if *self.peek() != Token::RParen {
                    loop {
                        args.push(self.parse_expr());
                        if *self.peek() == Token::Comma { self.advance(); } else { break; }
                    }
                }
                self.expect(Token::RParen);
                Expr::Syscall(args)
            }
            _ => panic!("Unexpected token: {:?}", self.peek()),
        }
    }
}

struct Codegen {
    output: String,
    label_count: usize,
    stack_pos: HashMap<String, i64>,
    stack_size: i64,
}

impl Codegen {
    fn new() -> Self {
        Codegen {
            output: String::new(),
            label_count: 0,
            stack_pos: HashMap::new(),
            stack_size: 0,
        }
    }

    fn emit(&mut self, s: &str) {
        self.output.push_str(s);
        self.output.push('\n');
    }

    fn gen_label(&mut self) -> String {
        let l = format!(".L{}", self.label_count);
        self.label_count += 1;
        l
    }

    fn generate(&mut self, funcs: Vec<Function>) -> String {
        self.emit(".intel_syntax noprefix");
        self.emit(".global main");
        
        for func in funcs {
            self.emit(&format!("{}:", func.name));
            self.emit("  push rbp");
            self.emit("  mov rbp, rsp");
            
            self.stack_pos.clear();
            self.stack_size = 0;
            
            for (i, param) in func.params.iter().enumerate() {
                self.stack_size += 8;
                self.stack_pos.insert(param.clone(), self.stack_size);
            }
            
            let regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
            for (i, param) in func.params.iter().enumerate() {
                if i < regs.len() {
                    self.emit(&format!("  mov [rbp-{}], {}", self.stack_pos[param], regs[i]));
                }
            }
            
            for stmt in func.body {
                self.gen_stmt(&stmt);
            }
            
            self.emit("  mov rsp, rbp");
            self.emit("  pop rbp");
            self.emit("  ret");
        }
        
        self.output.clone()
    }

    fn gen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(e) => {
                self.gen_expr(e);
                self.emit("  pop rax");
            }
            Stmt::Let(name, expr) => {
                self.gen_expr(expr);
                self.stack_size += 8;
                self.stack_pos.insert(name.clone(), self.stack_size);
                self.emit("  pop rax");
                self.emit(&format!("  mov [rbp-{}], rax", self.stack_pos[name]));
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.gen_stmt(s);
                }
            }
            Stmt::If(cond, then, els) => {
                let else_label = self.gen_label();
                let end_label = self.gen_label();
                self.gen_expr(cond);
                self.emit("  pop rax");
                self.emit("  cmp rax, 0");
                self.emit(&format!("  je {}", else_label));
                self.gen_stmt(then);
                self.emit(&format!("  jmp {}", end_label));
                self.emit(&format!("{}:", else_label));
                if let Some(e) = els {
                    self.gen_stmt(e);
                }
                self.emit(&format!("{}:", end_label));
            }
            Stmt::While(cond, body) => {
                let begin = self.gen_label();
                let end = self.gen_label();
                self.emit(&format!("{}:", begin));
                self.gen_expr(cond);
                self.emit("  pop rax");
                self.emit("  cmp rax, 0");
                self.emit(&format!("  je {}", end));
                self.gen_stmt(body);
                self.emit(&format!("  jmp {}", begin));
                self.emit(&format!("{}:", end));
            }
            Stmt::Return(expr) => {
                self.gen_expr(expr);
                self.emit("  pop rax");
                self.emit("  mov rsp, rbp");
                self.emit("  pop rbp");
                self.emit("  ret");
            }
        }
    }

    fn gen_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Num(n) => {
                self.emit(&format!("  push {}", n));
            }
            Expr::Var(name) => {
                if let Some(&pos) = self.stack_pos.get(name) {
                    self.emit(&format!("  mov rax, [rbp-{}]", pos));
                    self.emit("  push rax");
                } else {
                    panic!("Undefined variable: {}", name);
                }
            }
            Expr::Binary(l, op, r) => {
                self.gen_expr(l);
                self.gen_expr(r);
                self.emit("  pop rdi");
                self.emit("  pop rax");
                match op {
                    BinOp::Add => self.emit("  add rax, rdi"),
                    BinOp::Sub => self.emit("  sub rax, rdi"),
                    BinOp::Mul => self.emit("  imul rax, rdi"),
                    BinOp::Div => {
                        self.emit("  cqo");
                        self.emit("  idiv rdi");
                    }
                    BinOp::Mod => {
                        self.emit("  cqo");
                        self.emit("  idiv rdi");
                        self.emit("  mov rax, rdx");
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                        self.emit("  cmp rax, rdi");
                        let setcc = match op {
                            BinOp::Eq => "sete",
                            BinOp::Ne => "setne",
                            BinOp::Lt => "setl",
                            BinOp::Le => "setle",
                            BinOp::Gt => "setg",
                            BinOp::Ge => "setge",
                            _ => unreachable!(),
                        };
                        self.emit(&format!("  {} al", setcc));
                        self.emit("  movzx rax, al");
                    }
                    BinOp::BitAnd => self.emit("  and rax, rdi"),
                    BinOp::BitOr => self.emit("  or rax, rdi"),
                }
                self.emit("  push rax");
            }
            Expr::Assign(name, expr) => {
                self.gen_expr(expr);
                if let Some(&pos) = self.stack_pos.get(name) {
                    self.emit("  pop rax");
                    self.emit(&format!("  mov [rbp-{}], rax", pos));
                    self.emit("  push rax");
                } else {
                    panic!("Undefined variable: {}", name);
                }
            }
            Expr::Call(name, args) => {
                let regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                for arg in args {
                    self.gen_expr(arg);
                }
                for i in (0..args.len().min(6)).rev() {
                    self.emit(&format!("  pop {}", regs[i]));
                }
                self.emit(&format!("  call {}", name));
                self.emit("  push rax");
            }
            Expr::Deref(expr) => {
                self.gen_expr(expr);
                self.emit("  pop rax");
                self.emit("  mov rax, [rax]");
                self.emit("  push rax");
            }
            Expr::AddrOf(name) => {
                if let Some(&pos) = self.stack_pos.get(name) {
                    self.emit("  lea rax, [rbp-{}]", pos);
                    self.emit("  push rax");
                } else {
                    panic!("Undefined variable: {}", name);
                }
            }
            Expr::Index(base, idx) => {
                self.gen_expr(base);
                self.gen_expr(idx);
                self.emit("  pop rdi");
                self.emit("  pop rax");
                self.emit("  imul rdi, 8");
                self.emit("  add rax, rdi");
                self.emit("  mov rax, [rax]");
                self.emit("  push rax");
            }
            Expr::Syscall(args) => {
                let regs = ["rax", "rdi", "rsi", "rdx", "r10", "r8", "r9"];
                for arg in args {
                    self.gen_expr(arg);
                }
                for i in (0..args.len().min(7)).rev() {
                    self.emit(&format!("  pop {}", regs[i]));
                }
                self.emit("  syscall");
                self.emit("  push rax");
            }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <source.js>", args[0]);
        std::process::exit(1);
    }

    let source = fs::read_to_string(&args[1]).expect("Failed to read source file");
    
    let mut lexer = Lexer::new(&source);
    let mut tokens = Vec::new();
    loop {
        let tok = lexer.next_token();
        if tok == Token::Eof {
            tokens.push(tok);
            break;
        }
        tokens.push(tok);
    }

    let mut parser = Parser::new(tokens);
    let funcs = parser.parse_program();

    let mut codegen = Codegen::new();
    let asm = codegen.generate(funcs);

    let asm_file = "output.s";
    fs::write(asm_file, asm).expect("Failed to write assembly");

    Command::new("gcc")
        .args(&["-static", "-o", "output", asm_file])
        .status()
        .expect("Failed to compile");

    println!("Compiled successfully to ./output");
}