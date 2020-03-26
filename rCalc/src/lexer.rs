use std::iter::Peekable;
use std::collections::HashMap;
use std::iter::Iterator;

#[derive(Debug)]
pub enum Expr {
    Identifier(String),
    Float(f64),
    Unary(Op, Box<Expr>),
    Operator(Op, Box<Expr>, Box<Expr>),
    Assignment(Box<Expr>, Box<Expr>)
}

#[derive(Debug, Clone)]
pub enum Token {
    Identifier(String),
    Float(f64),
    Operator(Op),
    LParen,
    RParen,
    Equals,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

pub type TokenStream = Vec<Token>;

pub enum LexerError {
    UnknownCharacter
}

impl LexerError {
    pub fn panic(&self) -> &str {
        match *self {
            LexerError::UnknownCharacter => "Unexpected character!"
        }
    }
}



pub fn lex(input_str: &str) -> Result<TokenStream, LexerError> {
    let mut input = input_str.chars().peekable();
    let mut tokens: TokenStream = Vec::new();
    


    loop {
        if let Some(ix) = input.next() {
            let mut ip = ix;

            if ip.is_digit(10) {
                let mut ret: String = String::new();
                loop {
                    if ip == '.' {
                        ret.push(ip);
                        match input.next() {
                            Some(i) => ip = i,
                            None => break
                        }
                    }
                    match ip.is_digit(10) {
                        true => ret.push(ip),
                        false => break
                    }
                    match input.next() {
                        Some(i) => ip = i,
                        None => break
                    }
                }
                tokens.push(Token::Float(ret.parse::<f64>().unwrap()));
            }

            if ip.is_alphanumeric() {
                let mut ret: String = String::new();
                loop {
                    match ip.is_whitespace() {
                        true => break,
                        false => ret.push(ip)
                    }
                    match input.next() {
                        Some(i) => ip = i,
                        None => break
                    }
                }
                tokens.push(Token::Identifier(ret));
            }

            if ip.is_whitespace() {
                continue;
            }


            match ip {
                '+' => tokens.push(Token::Operator(Op::Add)),
                '-' => tokens.push(Token::Operator(Op::Sub)),
                '*' => tokens.push(Token::Operator(Op::Mul)),
                '/' => tokens.push(Token::Operator(Op::Div)),
                '(' => tokens.push(Token::LParen),
                ')' => tokens.push(Token::RParen),
                '=' => tokens.push(Token::Equals),
                '\n' => continue,
                _ => { println!("{:?}", ip); return Err(LexerError::UnknownCharacter) }
            }
        } else {
            break;
        }
    }
    Ok(tokens)
}


struct TokenIterator {
    tokens: TokenStream,
    index: usize
}

impl<'a> Iterator for TokenIterator {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.tokens.get(self.index);
        self.index += 1;
        match ret {
            Some(n) => Some(n.clone()),
            None => None
        }
    }
}

impl Token {
    fn is_literal(&self) -> bool {
        match *self {
            Token::Float(_) => true,
            Token::Identifier(_) => true,
            _ => false,
        }
    }
    fn is_operator(&self) -> Option<Op> {
        match *self {
            Token::Operator(n) => Some(n),
            _ => None,
        }
    }
    fn is_equals(&self) -> bool {
        match *self {
            Token::Equals => true,
            _ => false,
        }
    }
    fn is_paren(&self) -> bool {
        match *self {
            Token::LParen => true,
            Token::RParen => true,
            _ => false,
        }
    }
}

fn match_token(input: &mut Peekable<TokenIterator>, tokens: &[Token]) -> Option<Token> {
    for token in tokens {
        if let Some(n) = input.peek() {
            if token.is_literal() && n.is_literal() {
                return Some(input.next().unwrap())
            }
            if token.is_equals() && n.is_equals() {
                return Some(input.next().unwrap())
            }
            if token.is_paren() && n.is_paren() {
                return Some(input.next().unwrap())
            }
            if let Some(t) = token.is_operator() { 
                if let Some(l) = n.is_operator() {
                    if t == l {
                        return Some(input.next().unwrap())
                    }
                }
            }
        }
    }
    None
}

fn atom(input: &mut Peekable<TokenIterator>) -> Expr {
    if let Some(n) = match_token(input, &[Token::Float(0.0), Token::Identifier("".to_string()), Token::LParen]) {
        if let Token::Float(f) = n {
            return Expr::Float(f)
        }
        if let Token::Identifier(i) = n {
            return Expr::Identifier(i)
        }
        if let Token::LParen = n {
            let expr = assignment(input);
            match match_token(input, &[Token::RParen]) {
                Some(n) => return expr,
                None => panic!("Expected a closing parenthesis after expression!")
            }
        }
    }
    match input.peek() {
        None => return Expr::Float(0.0),
        Some(l) => panic!("Unexpected Literal {:?}!", l)
    }
}

fn unary(input: &mut Peekable<TokenIterator>) -> Expr {
    if let Some(p) = match_token(input, &[Token::Operator(Op::Sub)]) {
        if let Token::Operator(o) = p {
            return Expr::Unary(o, Box::new(unary(input)))
        }
    }

    return atom(input);
}

fn multiplicative(input: &mut Peekable<TokenIterator>) -> Expr {
    let mut expr = unary(input);
    
    while let Some(op) = match_token(input, &[Token::Operator(Op::Mul), Token::Operator(Op::Div)]) {
        let right = unary(input);
        if let Token::Operator(o) = op {
            expr = Expr::Operator(o, Box::new(expr), Box::new(right));
        }
    }
    
    expr
}

fn additive(input: &mut Peekable<TokenIterator>) -> Expr {
    let mut expr = multiplicative(input);
    
    while let Some(op) = match_token(input, &[Token::Operator(Op::Add), Token::Operator(Op::Sub)]) {
        let right = multiplicative(input);
        if let Token::Operator(o) = op {
            expr = Expr::Operator(o, Box::new(expr), Box::new(right));
        }
    }
    
    expr
}

fn assignment(input: &mut Peekable<TokenIterator>) -> Expr {
    let mut expr = additive(input);
    while let Some(_op) = match_token(input, &[Token::Equals]) {
        let right = additive(input);
        expr = Expr::Assignment(Box::new(expr), Box::new(right));
    }

    expr
}

#[derive(Debug, Clone, Copy)]
enum Literal {
    Float(f64)
}

impl Literal {
    fn as_float(&self) -> Option<f64> {
        match *self {
            Literal::Float(f) => Some(f)
        }
    }
}

struct Environment {
    memory: HashMap<String, Literal>
}

fn visit_op(op: Op, left: &Box<Expr>, right: &Box<Expr>, env: &mut Environment) -> Literal {
    match op {
        Op::Add => Literal::Float(left.visit(env).as_float().unwrap() + right.visit(env).as_float().unwrap()),
        Op::Sub => Literal::Float(left.visit(env).as_float().unwrap() - right.visit(env).as_float().unwrap()),
        Op::Mul => Literal::Float(left.visit(env).as_float().unwrap() * right.visit(env).as_float().unwrap()),
        Op::Div => Literal::Float(left.visit(env).as_float().unwrap() / right.visit(env).as_float().unwrap())
    }
}

fn visit_unary(op: Op, left: &Box<Expr>, env: &mut Environment) -> Literal {
    match op {
        Op::Sub => Literal::Float(-left.visit(env).as_float().unwrap()),
        _ => panic!("Evaluation error! {:?} not supported for unary operations!", op)
    }
}

fn visit_identifier(s: &String, env: &mut Environment) -> Literal {
    match env.memory.get(s) {
        Some(n) => *n,
        None => panic!("{:?} not found in memory!", s)
    }
}

fn visit_assignment(name: &Box<Expr>, expr: &Box<Expr>, env: &mut Environment) -> Literal {
    let visited_expr = expr.visit(env);
    if let Expr::Identifier(n) = &**name {
        env.memory.insert(n.clone(), visited_expr);
    }
    visited_expr
}

impl Expr {
    fn visit(&self, mut env: &mut Environment) -> Literal {
        match self {
            &Expr::Float(f) => Literal::Float(f),
            &Expr::Operator(op, ref left, ref right) => visit_op(op, left, right, &mut env),
            &Expr::Unary(op, ref left) => visit_unary(op, left, &mut env),
            &Expr::Identifier(ref s) => visit_identifier(s, &mut env),
            &Expr::Assignment(ref name, ref expr) => visit_assignment(name, expr, &mut env)
        }
    } 
}

pub struct Parser {
    env: Environment
}

impl Parser {
    pub fn new() -> Parser {
        Parser { env: Environment { memory: HashMap::new() } }
    }
}

fn parse_expr(input: TokenStream, parser: &mut Parser) {
    let tokiter = TokenIterator {
        tokens: input,
        index: 0
    };
    let pti = &mut tokiter.peekable();
    println!("{:?}\nMemory: {:?}", assignment(pti).visit(&mut parser.env), parser.env.memory);
}


pub fn parse(input: Result<TokenStream, LexerError>, parser: &mut Parser) {
    match input {
        Ok(v) => parse_expr(v, parser),
        Err(e) => println!("{}", e.panic())
    }
}