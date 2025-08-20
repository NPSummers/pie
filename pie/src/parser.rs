use crate::{
    ast::*,
    diagnostics::{Diagnostic, ErrorKind},
    lexer::Token,
};
use logos::{Logos, Span};
use std::ops::Range;

// RIP Tok{String}
pub struct Parser<'t> {
    tokens: Vec<(Token<'t>, Span)>,
    remaining: Range<usize>,
    source: &'t str,
}

impl<'t> Parser<'t> {
    pub fn new(source: &'t str) -> Result<Self, Diagnostic> {
        let mut tokens: Vec<(Token<'t>, Span)> = Vec::new();
        for (res, span) in Token::lexer(source).spanned() {
            match res {
                Ok(tok) => tokens.push((tok, span)),
                Err(()) => {
                    let bad = &source[span.clone()];
                    return Err(Diagnostic {
                        kind: ErrorKind::Lex,
                        message: format!("unexpected token '{}'", bad.escape_default()),
                        span,
                    });
                }
            }
        }
        let remaining = 0..tokens.len();
        Ok(Parser {
            tokens,
            remaining,
            source,
        })
    }
    fn remaining(&self) -> &[(Token<'t>, Span)] {
        self.tokens
            .get(self.remaining.clone())
            .expect("remaining range out of bounds for source")
    }
    fn current_span(&self) -> Span {
        if let Some((_, s)) = self.remaining().first() {
            return s.clone();
        }
        if let Some((_, s)) = self.tokens.last() {
            return s.end..s.end;
        }
        0..0
    }
    fn advance(&mut self) -> Option<Token<'t>> {
        if self.remaining.start >= self.tokens.len() {
            return None;
        }
        self.remaining.start += 1;
        Some(self.tokens[self.remaining.start - 1].clone().0)
    }
    fn last_span(&self) -> Span {
        if self.remaining.start == 0 {
            return self.current_span();
        }
        let (_, s) = &self.tokens[self.remaining.start - 1];
        s.clone()
    }
    fn unconsume_one(&mut self) {
        self.remaining.start = self.remaining.start.saturating_sub(1);
    }
    fn last_consumed(&mut self) -> Option<Token<'t>> {
        if self.remaining.start == 0 {
            return None;
        }
        self.unconsume_one();
        self.advance()
    }
    fn peek(&self) -> Option<&Token<'_>> {
        self.remaining().first().map(|(t, _)| t)
    }
    fn check(&self, predicate: impl FnOnce(&Token) -> bool) -> bool {
        self.peek().is_some_and(predicate)
    }
    fn advance_if(&mut self, predicate: impl FnOnce(&Token) -> bool) -> Option<Token<'t>> {
        let advance = self.check(predicate);
        if advance {
            return self.advance();
        }
        None
    }
    fn consume_if(&mut self, predicate: impl FnOnce(&Token) -> bool) -> bool {
        self.advance_if(predicate).is_some()
    }
}

impl<'s> Parser<'s> {
    fn err_here(&self, kind: ErrorKind, msg: impl Into<String>) -> Diagnostic {
        Diagnostic {
            kind,
            message: msg.into(),
            span: self.current_span(),
        }
    }
    fn err_last(&self, kind: ErrorKind, msg: impl Into<String>) -> Diagnostic {
        Diagnostic {
            kind,
            message: msg.into(),
            span: self.last_span(),
        }
    }

    fn parse_unary(&mut self) -> Result<Expression<'s>, Diagnostic> {
        if let Some(tok) = self.advance_if(|t| matches!(t, Token::Not | Token::Minus | Token::Plus))
        {
            let op = match tok {
                Token::Not => UnaryOp::Not,
                Token::Minus => UnaryOp::Sub,
                Token::Plus => UnaryOp::Add,
                _ => unreachable!(),
            };
            let expr = self.parse_primary()?;
            return Ok(Expression::Unary(op, Box::new(expr)));
        }
        self.parse_primary()
    }
    fn parse_primary(&mut self) -> Result<Expression<'s>, Diagnostic> {
        let Some(first) = self.advance() else {
            return Err(self.err_here(ErrorKind::Parse, "expected a token in primary"));
        };
        match first {
            Token::StringLit(s) => Ok(Expression::Str(s)),
            Token::IntLit(i) => Ok(Expression::Int(i)),
            Token::FloatLit(f) => Ok(Expression::Float(f)),
            Token::BoolLit(b) => Ok(Expression::Bool(b)),
            Token::Char(c) => Ok(Expression::Int(c as i64)),
            // function call
            tok @ Token::Ident(_) | tok @ Token::ModuleAccess(_) | tok @ Token::MemberAccess(_)
                if self.consume_if(|t| matches!(t, Token::LParen)) =>
            {
                let callee = match tok {
                    Token::Ident(name) => Expression::Ident(name),
                    Token::ModuleAccess(components) => Expression::ModuleAccess { components },
                    Token::MemberAccess(components) => Expression::MemberAccess { components },
                    _ => unreachable!(),
                };
                let mut args = Vec::new();

                while !self.consume_if(|t| matches!(t, Token::RParen)) {
                    let arg = self.parse_expression()?;
                    args.push(arg);
                    if !self.consume_if(|t| matches!(t, Token::Comma)) {
                        if !matches!(self.advance(), Some(Token::RParen)) {
                            return Err(self.err_here(
                                ErrorKind::Parse,
                                "expected ')' following function call",
                            ));
                        }
                        break;
                    }
                }
                Ok(Expression::Call {
                    callee: Box::new(callee),
                    args,
                })
            }
            Token::ModuleAccess(components) => Ok(Expression::ModuleAccess { components }),
            Token::Ident(id) => Ok(Expression::Ident(id)),
            Token::LParen => {
                let expr = self.parse_expression()?;
                if !matches!(self.advance(), Some(Token::RParen)) {
                    return Err(self.err_here(ErrorKind::Parse, "expected ')' to close group"));
                }
                Ok(expr)
            }
            Token::LBracket => {
                let mut items = Vec::new();
                while !self.consume_if(|t| matches!(t, Token::RBracket)) {
                    let val = self.parse_expression()?;
                    items.push(val);
                    if !self.consume_if(|t| matches!(t, Token::Comma)) {
                        let Some(Token::RBracket) = self.advance() else {
                            return Err(
                                self.err_here(ErrorKind::Parse, "expected ']' following list")
                            );
                        };
                        break;
                    }
                }
                Ok(Expression::ListLiteral(items))
            }
            Token::LBrace => {
                let mut pairs = Vec::new();
                while !self.consume_if(|t| matches!(t, Token::RBrace)) {
                    let key = self.parse_expression()?;
                    if !self.consume_if(|t| matches!(t, Token::Colon)) {
                        return Err(
                            self.err_here(ErrorKind::Parse, "expected ':' following map key")
                        );
                    }
                    let value = self.parse_expression()?;
                    pairs.push((key, value));
                    if !self.consume_if(|t| matches!(t, Token::Comma)) {
                        let Some(Token::RBrace) = self.advance() else {
                            return Err(
                                self.err_here(ErrorKind::Parse, "expected '}' to close map")
                            );
                        };
                        break;
                    }
                }
                Ok(Expression::MapLiteral(pairs))
            }
            other => Err(self.err_last(
                ErrorKind::Parse,
                format!("unexpected token in primary: {other:?}"),
            )),
        }
    }

    // simple precedence climbing for + - and * /
    fn parse_binary_op_rhs(
        &mut self,
        expr_prec: u16,
        mut lhs: Expression<'s>,
    ) -> Result<Expression<'s>, Diagnostic> {
        fn token_to_bop(token: &Token) -> Option<BinaryOp> {
            Some(match token {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                Token::Star => BinaryOp::Mul,
                Token::Slash => BinaryOp::Div,
                Token::EqEq => BinaryOp::Eq,
                Token::Ne => BinaryOp::Ne,
                Token::Gt => BinaryOp::Gt,
                Token::Lt => BinaryOp::Lt,
                Token::GtEq => BinaryOp::GtEq,
                Token::LtEq => BinaryOp::LtEq,
                Token::Percent => BinaryOp::Rem,
                _ => return None,
            })
        }
        while let Some(op) = self.peek() {
            let Some(op) = token_to_bop(op) else { break };
            let prec = op.precedence();
            if prec < expr_prec {
                break;
            }
            self.advance();
            let mut rhs = self.parse_unary()?;
            while let Some(op) = self.peek() {
                let Some(op) = token_to_bop(op) else { break };
                let next_prec = op.precedence();
                if next_prec <= prec {
                    break;
                }
                rhs = self.parse_binary_op_rhs(prec + 1, rhs)?;
            }
            lhs = Expression::Binary(Box::new(lhs), op, Box::new(rhs));
        }
        Ok(lhs)
    }

    fn parse_expression(&mut self) -> Result<Expression<'s>, Diagnostic> {
        let lhs = self.parse_unary()?;
        self.parse_binary_op_rhs(0, lhs)
    }
    fn parse_if_condition_and_body(
        &mut self,
        top_level: bool,
    ) -> Result<Statement<'s>, Diagnostic> {
        let cond = self.parse_expression()?;
        let Some(Token::LBrace) = self.advance() else {
            return Err(self.err_here(ErrorKind::Parse, "expected '{' after if condition"));
        };
        let mut then_body = Vec::new();
        while !self.consume_if(|t| matches!(t, Token::RBrace)) {
            let statement = self.parse_statement()?;
            then_body.push(statement);
        }
        let mut else_body = Vec::new();
        if self.consume_if(|t| matches!(t, Token::Elif)) {
            else_body.push(self.parse_if_condition_and_body(false)?);
        }
        if top_level && self.consume_if(|t| matches!(t, Token::Else)) {
            else_body.push(self.parse_if_condition_and_body(false)?);
        }
        let else_body = (!else_body.is_empty()).then_some(else_body);
        Ok(Statement::If {
            cond,
            then_body,
            else_body,
        })
    }

    fn parse_statement(&mut self) -> Result<Statement<'s>, Diagnostic> {
        let Some(first) = self.advance() else {
            return Err(self.err_here(ErrorKind::Parse, "unexpected EOF in statement"));
        };
        match first {
            Token::If => self.parse_if_condition_and_body(true),
            Token::Let => {
                let Some(Token::Ident(typ)) = self.advance() else {
                    return Err(self.err_here(ErrorKind::Parse, "expected a type after 'let'"));
                };
                let typ = typ.into();
                let Some(Token::Ident(name)) = self.advance() else {
                    return Err(self.err_here(ErrorKind::Parse, "expected a name after 'let'"));
                };
                let Some(Token::Eq) = self.advance() else {
                    return Err(self.err_here(ErrorKind::Parse, "expected '=' after 'let'"));
                };
                let expr = self.parse_expression()?;
                if !self.consume_if(|t| matches!(t, Token::Semicolon)) {
                    return Err(self.err_here(ErrorKind::Parse, "expected ';' after statement"));
                }
                Ok(Statement::Let { typ, name, expr })
            }
            Token::Return => {
                if self.consume_if(|t| matches!(t, Token::Semicolon)) {
                    return Ok(Statement::Return(None));
                }
                let expr = self.parse_expression()?;
                if !self.consume_if(|t| matches!(t, Token::Semicolon)) {
                    return Err(self.err_here(ErrorKind::Parse, "expected ';' after return"));
                }
                Ok(Statement::Return(Some(expr)))
            }
            Token::For => {
                let Some(Token::Ident(var)) = self.advance() else {
                    panic!("Expected a name after for")
                };
                let Some(Token::In) = self.advance() else {
                    panic!("Expected in after the variable name in for loop")
                };
                let iterable = self.parse_expression()?;
                let Some(Token::LBrace) = self.advance() else {
                    panic!("Expected an {{ after for loop")
                };
                let mut body = Vec::new();
                while !self.consume_if(|t| matches!(t, Token::RBrace)) {
                    let statement = self.parse_statement()?;
                    body.push(statement);
                }
                Ok(Statement::For {
                    iterable,
                    var,
                    body,
                })
            }
            Token::While => {
                let cond = self.parse_expression()?;
                let Some(Token::LBrace) = self.advance() else {
                    panic!("Expected an {{ after while loop")
                };
                let mut body = Vec::new();
                while !self.consume_if(|t| matches!(t, Token::RBrace)) {
                    let statement = self.parse_statement()?;
                    body.push(statement);
                }
                Ok(Statement::While { cond, body })
            }
            _ => {
                self.unconsume_one();
                let expr = self.parse_expression()?;
                let Some(next) = self.advance() else {
                    return Err(
                        self.err_here(ErrorKind::Parse, "expected '=' or ';' after expression")
                    );
                };
                let op = match next {
                    Token::Eq => Some(AssignOp::Assign),
                    Token::PlusEq => Some(AssignOp::Plus),
                    Token::MinusEq => Some(AssignOp::Minus),
                    Token::StarEq => Some(AssignOp::Star),
                    Token::SlashEq => Some(AssignOp::Slash),
                    _ => {
                        self.unconsume_one();
                        None
                    }
                };
                let stmt = if let Some(op) = op {
                    // Assignment
                    let target = expr;
                    let value = self.parse_expression()?;
                    Statement::Assignment { target, op, value }
                } else {
                    Statement::Expr(expr)
                };
                if !self.consume_if(|t| matches!(t, Token::Semicolon)) {
                    return Err(self.err_here(ErrorKind::Parse, "expected ';' after statement"));
                }
                Ok(stmt)
            }
        }
    }

    fn parse_module_item(&mut self) -> Result<ModuleItem<'s>, Diagnostic> {
        let Some(first) = self.advance() else {
            return Err(self.err_here(ErrorKind::Parse, "unexpected EOF in module item"));
        };
        match first {
            Token::Let => {
                let Some(Token::Ident(typ)) = self.advance() else {
                    return Err(self.err_here(ErrorKind::Parse, "expected a type after 'let'"));
                };
                let typ = typ.into();
                let Some(Token::Ident(name)) = self.advance() else {
                    return Err(self.err_here(ErrorKind::Parse, "expected a name after 'let'"));
                };
                let Some(Token::Eq) = self.advance() else {
                    return Err(self.err_here(ErrorKind::Parse, "expected '=' after 'let'"));
                };
                let expr = self.parse_expression()?;
                if !self.consume_if(|t| matches!(t, Token::Semicolon)) {
                    return Err(self.err_here(ErrorKind::Parse, "expected ';' after let statement"));
                }
                Ok(ModuleItem::Let { typ, name, expr })
            }
            Token::Def => {
                let Some(Token::Ident(ret)) = self.advance() else {
                    return Err(self.err_here(ErrorKind::Parse, "expected a type after 'def'"));
                };
                let ret = ret.into();
                let Some(Token::Ident(name)) = self.advance() else {
                    return Err(self.err_here(ErrorKind::Parse, "expected a name after 'def'"));
                };
                let Some(Token::LParen) = self.advance() else {
                    return Err(self.err_here(ErrorKind::Parse, "expected '(' after function name"));
                };
                let mut params = Vec::new();
                while !self.consume_if(|t| matches!(t, Token::RParen)) {
                    let Some(Token::Ident(typ)) = self.advance() else {
                        return Err(self
                            .err_here(ErrorKind::Parse, "expected a type in function definition"));
                    };
                    let typ: TypeName<'s> = typ.into();
                    let Some(Token::Ident(name)) = self.advance() else {
                        return Err(self
                            .err_here(ErrorKind::Parse, "expected a name in function definition"));
                    };
                    params.push((typ, name));
                    if !self.consume_if(|t| matches!(t, Token::Comma)) {
                        if !self.consume_if(|t| matches!(t, Token::RParen)) {
                            return Err(self.err_here(
                                ErrorKind::Parse,
                                "expected ')' after parameters in function definition",
                            ));
                        }
                        break;
                    };
                }
                let Some(Token::LBrace) = self.advance() else {
                    return Err(
                        self.err_here(ErrorKind::Parse, "expected '{' after function definition")
                    );
                };
                let mut body = Vec::new();
                while !self.consume_if(|t| matches!(t, Token::RBrace)) {
                    let statement = self.parse_statement()?;
                    body.push(statement);
                }
                Ok(ModuleItem::Function(Function {
                    name,
                    params,
                    ret,
                    body,
                }))
            }
            Token::Module => {
                let Some(Token::Ident(name)) = self.advance() else {
                    return Err(
                        self.err_here(ErrorKind::Parse, "expected an identifier after 'module'")
                    );
                };
                let Some(Token::LBrace) = self.advance() else {
                    return Err(self.err_here(ErrorKind::Parse, "expected '{' after module name"));
                };
                let mut items = Vec::new();
                while !self.consume_if(|t| matches!(t, Token::RBrace)) {
                    let item = self.parse_module_item()?;
                    items.push(item);
                }
                Ok(ModuleItem::Module(Module { name, items }))
            }
            other => Err(self.err_last(
                ErrorKind::Parse,
                format!("unsupported token in module item: {other:?}"),
            )),
        }
    }

    fn parse_item(&mut self) -> Result<Item<'s>, Diagnostic> {
        let Some(first) = self.advance() else {
            return Err(self.err_here(ErrorKind::Parse, "unexpected EOF in item"));
        };
        match first {
            Token::Use => {
                let Some(Token::Ident(root)) = self.advance() else {
                    return Err(
                        self.err_here(ErrorKind::Parse, "expected an identifier after 'use'")
                    );
                };
                let mut path = vec![root];
                while self.consume_if(|t| matches!(t, Token::Slash)) {
                    let Some(Token::Ident(component)) = self.advance() else {
                        return Err(self.err_here(
                            ErrorKind::Parse,
                            "expected an identifier after '/' in use",
                        ));
                    };
                    path.push(component);
                }
                if !self.consume_if(|t| matches!(t, Token::Semicolon)) {
                    return Err(self.err_here(ErrorKind::Parse, "expected ';' after use statement"));
                }
                Ok(Item::Use(path))
            }
            Token::Module => {
                let Some(Token::Ident(name)) = self.advance() else {
                    return Err(
                        self.err_here(ErrorKind::Parse, "expected an identifier after 'module'")
                    );
                };
                let Some(Token::LBrace) = self.advance() else {
                    return Err(self.err_here(ErrorKind::Parse, "expected '{' after module name"));
                };
                let mut items = Vec::new();
                while !self.consume_if(|t| matches!(t, Token::RBrace)) {
                    let item = self.parse_module_item()?;
                    items.push(item);
                }
                Ok(Item::Module(Module { name, items }))
            }
            _ => {
                self.unconsume_one();
                let stmt = self.parse_statement()?;
                Ok(Item::TopStatement(stmt))
            }
        }
    }

    pub fn parse(mut self) -> Result<Program<'s>, Diagnostic> {
        let mut items = Vec::new();
        let mut main_items = Vec::new();

        while !self.remaining().is_empty() {
            match self.peek().unwrap() {
                Token::Def | Token::Let => {
                    let main_item = self.parse_module_item()?;
                    main_items.push(main_item);
                }
                _ => {
                    let item = self.parse_item()?;
                    items.push(item);
                }
            }
        }
        items.push(Item::Module(Module {
            name: "main",
            items: main_items,
        }));
        Ok(Program { items })
    }
}
