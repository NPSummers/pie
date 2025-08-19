use crate::{ast::*, lexer::Token};
use logos::{Logos, Span};
use std::ops::Range;

// RIP Tok{String}
pub struct Parser<'t> {
    tokens: Vec<(Token<'t>, Span)>,
    remaining: Range<usize>,
}

impl<'t> Parser<'t> {
    pub fn new(source: &'t str) -> Result<Self, ()> {
        let tokens: Vec<_> = Token::lexer(source)
            .spanned()
            .map(|(tok, r)| tok.map(|t| (t, r)))
            .collect::<Result<_, ()>>()?;
        let remaining = 0..tokens.len();
        Ok(Parser { tokens, remaining })
    }
    fn remaining(&self) -> &[(Token<'t>, Span)] {
        self.tokens
            .get(self.remaining.clone())
            .expect("remaining range out of bounds for source")
    }
    fn advance(&mut self) -> Option<Token<'t>> {
        if self.remaining.start >= self.tokens.len() {
            return None;
        }
        self.remaining.start += 1;
        Some(self.tokens[self.remaining.start - 1].clone().0)
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
    fn parse_unary(&mut self) -> Result<Expression<'s>, String> {
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
    fn parse_primary(&mut self) -> Result<Expression<'s>, String> {
        let Some(first) = self.advance() else {
            return Err("expected a token in primary".into());
        };
        match first {
            Token::StringLit(s) => Ok(Expression::Str(s)),
            Token::IntLit(i) => Ok(Expression::Int(i)),
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
                        assert!(
                            matches!(self.advance(), Some(Token::RParen)),
                            "Expected a ) following function call"
                        );
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
                assert!(
                    matches!(self.advance(), Some(Token::RParen)),
                    "Expected a ) following group `(expr...`"
                );
                Ok(expr)
            }
            Token::LBracket => {
                let mut items = Vec::new();
                while !self.consume_if(|t| matches!(t, Token::RBracket)) {
                    let val = self.parse_expression()?;
                    items.push(val);
                    if !self.consume_if(|t| matches!(t, Token::Comma)) {
                        let Some(Token::RBracket) = self.advance() else {
                            return Err("Expected a ] following list `[...`".into());
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
                        return Err("Expected a : following map `{key...`".into());
                    }
                    let value = self.parse_expression()?;
                    pairs.push((key, value));
                    if !self.consume_if(|t| matches!(t, Token::Comma)) {
                        let Some(Token::RBrace) = self.advance() else {
                            return Err("Expected a } following map `{...`".into());
                        };
                        break;
                    }
                }
                Ok(Expression::MapLiteral(pairs))
            }
            other => Err(format!("unexpected token in primary: {other:?}")),
        }
    }

    // simple precedence climbing for + - and * /
    fn parse_binary_op_rhs(
        &mut self,
        expr_prec: u16,
        mut lhs: Expression<'s>,
    ) -> Result<Expression<'s>, String> {
        while let Some(op) = self.peek() {
            let op = match op {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                Token::Star => BinaryOp::Mul,
                Token::Slash => BinaryOp::Div,
                _ => break,
            };
            let prec = op.precedence();
            if prec < expr_prec {
                break;
            }
            self.advance();
            let mut rhs = self.parse_unary()?;
            while let Some(op) = self.peek() {
                let op = match op {
                    Token::Plus => BinaryOp::Add,
                    Token::Minus => BinaryOp::Sub,
                    Token::Star => BinaryOp::Mul,
                    Token::Slash => BinaryOp::Div,
                    _ => break,
                };
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

    fn parse_expression(&mut self) -> Result<Expression<'s>, String> {
        let lhs = dbg!(self.parse_unary())?;
        self.parse_binary_op_rhs(0, lhs)
    }
    fn parse_statement(&mut self) -> Result<Statement<'s>, String> {
        let Some(first) = self.advance() else {
            panic!("Unexpected EOF in statement")
        };
        match first {
            Token::Let => {
                let Some(Token::Ident(typ)) = self.advance() else {
                    panic!("Expected a type after let")
                };
                let typ = typ.into();
                let Some(Token::Ident(name)) = self.advance() else {
                    panic!("Expected a name after let")
                };
                let Some(Token::Eq) = self.advance() else {
                    panic!("Expected a = after let")
                };
                let expr = self.parse_expression()?;
                if !self.consume_if(|t| matches!(t, Token::Semicolon)) {
                    panic!("Expected a ; after a statement")
                }
                Ok(Statement::Let { typ, name, expr })
            }
            Token::Return => {
                if self.consume_if(|t| matches!(t, Token::Semicolon)) {
                    return Ok(Statement::Return(None));
                }
                let expr = self.parse_expression()?;
                if !self.consume_if(|t| matches!(t, Token::Semicolon)) {
                    panic!("Expected a ; after a return statement")
                }
                Ok(Statement::Return(Some(expr)))
            }
            _ => {
                self.unconsume_one();
                let expr = self.parse_expression()?;
                let Some(next) = self.advance() else {
                    panic!("Expected a = or ; after expression")
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
                    panic!("Expected a ; after a statement")
                }
                Ok(stmt)
            }
        }
    }

    fn parse_module_item(&mut self) -> Result<ModuleItem<'s>, String> {
        let Some(first) = self.advance() else {
            panic!("Unexpected EOF in module item")
        };
        match first {
            Token::Let => {
                let Some(Token::Ident(typ)) = self.advance() else {
                    panic!("Expected a type after let")
                };
                let typ = typ.into();
                let Some(Token::Ident(name)) = self.advance() else {
                    panic!("Expected a name after let")
                };
                let Some(Token::Eq) = self.advance() else {
                    panic!("Expected a = after let")
                };
                let expr = self.parse_expression()?;
                if !self.consume_if(|t| matches!(t, Token::Semicolon)) {
                    panic!("Expected a ; after let statement")
                }
                Ok(ModuleItem::Let { typ, name, expr })
            }
            Token::Def => {
                let Some(Token::Ident(ret)) = self.advance() else {
                    panic!("Expected a type after def")
                };
                let ret = ret.into();
                let Some(Token::Ident(name)) = self.advance() else {
                    panic!("Expected a name after def")
                };
                let Some(Token::LParen) = self.advance() else {
                    panic!("Expected a name after def")
                };
                let mut params = Vec::new();
                while !self.consume_if(|t| matches!(t, Token::RParen)) {
                    let Some(Token::Ident(typ)) = self.advance() else {
                        panic!("Expected a type in function definition")
                    };
                    let typ: TypeName<'s> = typ.into();
                    let Some(Token::Ident(name)) = self.advance() else {
                        panic!("Expected a name in function definition")
                    };
                    params.push((typ, name));
                    if !self.consume_if(|t| matches!(t, Token::Comma)) {
                        if !self.consume_if(|t| matches!(t, Token::RParen)) {
                            panic!("Expected a ) after parameters in function definition")
                        }
                        break;
                    };
                }
                let Some(Token::LBrace) = self.advance() else {
                    panic!("Expected an {{ after function definition")
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
                    panic!("Expected an identifier after module")
                };
                let Some(Token::LBrace) = self.advance() else {
                    panic!("Expected an {{ after module name")
                };
                let mut items = Vec::new();
                while !self.consume_if(|t| matches!(t, Token::RBrace)) {
                    let item = self.parse_module_item()?;
                    items.push(item);
                }
                Ok(ModuleItem::Module(Module { name, items }))
            }
            other => Err(format!("Unsupported token in module item: {other:?}")),
        }
    }

    fn parse_item(&mut self) -> Result<Item<'s>, String> {
        let Some(first) = self.advance() else {
            panic!("Unexpected EOF in item")
        };
        match first {
            Token::Use => {
                let Some(Token::Ident(root)) = self.advance() else {
                    panic!("Expected an identifier after use")
                };
                let mut path = vec![root];
                while self.consume_if(|t| matches!(t, Token::Slash)) {
                    let Some(Token::Ident(component)) = self.advance() else {
                        panic!("Expected an identifier after / in use")
                    };
                    path.push(component);
                }
                if !self.consume_if(|t| matches!(t, Token::Semicolon)) {
                    panic!("Expected a ; after use statement")
                }
                Ok(Item::Use(path))
            }
            Token::Module => {
                let Some(Token::Ident(name)) = self.advance() else {
                    panic!("Expected an identifier after module")
                };
                let Some(Token::LBrace) = self.advance() else {
                    panic!("Expected an {{ after module name")
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
                let stmt = dbg!(self.parse_statement())?;
                Ok(Item::TopStatement(stmt))
            }
        }
    }

    pub fn parse(mut self) -> Result<Program<'s>, String> {
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
                    items.push(dbg!(item));
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
