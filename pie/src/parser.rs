use crate::ast::*;
use logos::Logos;

#[derive(Debug, Clone)]
struct Tok {
    text: String,
}

#[derive(Logos, Debug, PartialEq)]
enum LToken {
    // skip whitespace and comments
    #[regex(r"[ \t\n\r]+", logos::skip)]
    #[regex(r"#.*", logos::skip)]
    Skip,

    // literals
    #[regex(r#""[^"]*""#)]
    StringLiteral,
    #[regex(r"[0-9]+")]
    Number,

    // identifiers (allow slash for module paths like `pie/std`)
    #[regex(r"[A-Za-z_][A-Za-z0-9_/]*")]
    Ident,

    // multi-char punctuation
    #[token("::")]
    DoubleColon,
    #[token("+=")]
    PlusEq,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    StarEq,
    #[token("/=")]
    SlashEq,

    // single-char tokens
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token(":")]
    Colon,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(".")]
    Dot,
    #[token("=")]
    Eq,

    #[error]
    Error,
}

fn tokenize(src: &str) -> Vec<Tok> {
    let mut lexer = LToken::lexer(src);
    let mut tokens = Vec::new();
    while let Some(tok) = lexer.next() {
        // logos gives us the slice for each token via lexer.slice()
        match tok {
            LToken::Skip => continue,
            LToken::StringLiteral | LToken::Number | LToken::Ident => {
                tokens.push(Tok {
                    text: lexer.slice().to_string(),
                });
            }
            LToken::DoubleColon => tokens.push(Tok { text: "::".into() }),
            LToken::PlusEq => tokens.push(Tok { text: "+=".into() }),
            LToken::MinusEq => tokens.push(Tok { text: "-=".into() }),
            LToken::StarEq => tokens.push(Tok { text: "*=".into() }),
            LToken::SlashEq => tokens.push(Tok { text: "/=".into() }),
            LToken::Plus => tokens.push(Tok { text: "+".into() }),
            LToken::Minus => tokens.push(Tok { text: "-".into() }),
            LToken::Star => tokens.push(Tok { text: "*".into() }),
            LToken::Slash => tokens.push(Tok { text: "/".into() }),
            LToken::Colon => tokens.push(Tok { text: ":".into() }),
            LToken::LParen => tokens.push(Tok { text: "(".into() }),
            LToken::RParen => tokens.push(Tok { text: ")".into() }),
            LToken::LBrace => tokens.push(Tok { text: "{".into() }),
            LToken::RBrace => tokens.push(Tok { text: "}".into() }),
            LToken::LBracket => tokens.push(Tok { text: "[".into() }),
            LToken::RBracket => tokens.push(Tok { text: "]".into() }),
            LToken::Comma => tokens.push(Tok { text: ",".into() }),
            LToken::Semicolon => tokens.push(Tok { text: ";".into() }),
            LToken::Dot => tokens.push(Tok { text: ".".into() }),
            LToken::Eq => tokens.push(Tok { text: "=".into() }),
            LToken::Error => {
                // fallback: push the raw slice (likely a single char)
                tokens.push(Tok {
                    text: lexer.slice().to_string(),
                });
            }
        }
    }
    tokens
}

// parser helpers
fn peek(toks: &[Tok], i: usize) -> Option<&str> {
    toks.get(i).map(|t| t.text.as_str())
}
fn eat(toks: &[Tok], i: &mut usize, expected: &str) -> Result<(), String> {
    if let Some(s) = peek(toks, *i) {
        if s == expected {
            *i += 1;
            Ok(())
        } else {
            Err(format!("expected {}, found {}", expected, s))
        }
    } else {
        Err(format!("expected {}, found EOF", expected))
    }
}

fn parse_primary(toks: &Vec<Tok>, i: &mut usize) -> Result<Expression, String> {
    if let Some(s) = peek(toks, *i) {
        // string literal
        if s.starts_with('"') {
            *i += 1;
            return Ok(Expression::Str(s.trim_matches('"').to_string()));
        }
        // integer
        if s.chars().all(|c| c.is_ascii_digit()) {
            let v = s.parse::<i64>().map_err(|e| e.to_string())?;
            *i += 1;
            return Ok(Expression::Int(v));
        }
        // identifier or module access or call
        if s.chars().next().unwrap().is_ascii_alphabetic() || s.starts_with('_') {
            let id = s.to_string();
            *i += 1;
            // module access? (support chaining like a::b::c)
            if peek(toks, *i) == Some("::") {
                *i += 1;
                if let Some(name0) = peek(toks, *i) {
                    let mut name = name0.to_string();
                    *i += 1;
                    // allow further :: chaining
                    while peek(toks, *i) == Some("::") {
                        *i += 1;
                        if let Some(n2) = peek(toks, *i) {
                            name = format!("{}::{}", name, n2);
                            *i += 1;
                        } else {
                            return Err("expected name after ::".into());
                        }
                    }
                    let module_access = Expression::ModuleAccess { module: id, name };
                    // Check if this is a call
                    if peek(toks, *i) == Some("(") {
                        *i += 1;
                        let mut args = Vec::new();
                        while peek(toks, *i) != Some(")") {
                            let a = parse_expression(toks, i)?;
                            args.push(a);
                            if peek(toks, *i) == Some(",") {
                                *i += 1;
                            } else {
                                break;
                            }
                        }
                        eat(toks, i, ")")?;
                        return Ok(Expression::Call {
                            callee: Box::new(module_access),
                            args,
                        });
                    }
                    return Ok(module_access);
                } else {
                    return Err("expected name after ::".into());
                }
            }
            // dot member access? (support chaining like a.b.c)
            if peek(toks, *i) == Some(".") {
                *i += 1;
                if let Some(name0) = peek(toks, *i) {
                    let mut name = name0.to_string();
                    *i += 1;
                    // allow further . chaining
                    while peek(toks, *i) == Some(".") {
                        *i += 1;
                        if let Some(n2) = peek(toks, *i) {
                            name = format!("{}.{}", name, n2);
                            *i += 1;
                        } else {
                            return Err("expected name after .".into());
                        }
                    }
                    let module_access = Expression::ModuleAccess { module: id, name };
                    // Check if this is a call
                    if peek(toks, *i) == Some("(") {
                        *i += 1;
                        let mut args = Vec::new();
                        while peek(toks, *i) != Some(")") {
                            let a = parse_expression(toks, i)?;
                            args.push(a);
                            if peek(toks, *i) == Some(",") {
                                *i += 1;
                            } else {
                                break;
                            }
                        }
                        eat(toks, i, ")")?;
                        return Ok(Expression::Call {
                            callee: Box::new(module_access),
                            args,
                        });
                    }
                    return Ok(module_access);
                } else {
                    return Err("expected name after .".into());
                }
            }
            // call?
            if peek(toks, *i) == Some("(") {
                *i += 1;
                let mut args = Vec::new();
                while peek(toks, *i) != Some(")") {
                    let a = parse_expression(toks, i)?;
                    args.push(a);
                    if peek(toks, *i) == Some(",") {
                        *i += 1;
                    } else {
                        break;
                    }
                }
                eat(toks, i, ")")?;
                return Ok(Expression::Call {
                    callee: Box::new(Expression::Ident(id)),
                    args,
                });
            }
            return Ok(Expression::Ident(id));
        }
        if s == "(" {
            *i += 1;
            let e = parse_expression(toks, i)?;
            eat(toks, i, ")")?;
            return Ok(e);
        }
        if s == "[" {
            *i += 1;
            let mut items = Vec::new();
            while peek(toks, *i) != Some("]") {
                let e = parse_expression(toks, i)?;
                items.push(e);
                if peek(toks, *i) == Some(",") {
                    *i += 1;
                } else {
                    break;
                }
            }
            eat(toks, i, "]")?;
            return Ok(Expression::ListLiteral(items));
        }
        if s == "{" {
            *i += 1;
            let mut pairs = Vec::new();
            while peek(toks, *i) != Some("}") {
                let key = peek(toks, *i).ok_or("expected key in map")?.to_string();
                *i += 1;
                eat(toks, i, ":")?;
                let val = parse_expression(toks, i)?;
                pairs.push((key, val));
                if peek(toks, *i) == Some(",") {
                    *i += 1;
                }
            }
            eat(toks, i, "}")?;
            return Ok(Expression::MapLiteral(pairs));
        }
    }
    Err("unexpected token in primary".into())
}

// simple precedence climbing for + - and * /
fn parse_binary_op_rhs(
    toks: &Vec<Tok>,
    i: &mut usize,
    expr_prec: i32,
    mut lhs: Expression,
) -> Result<Expression, String> {
    while let Some(op) = peek(toks, *i) {
        if op.len() != 1 || !"+-*/".contains(op) {
            break;
        };
        let op = op.chars().next().unwrap();
        let prec = if op == '+' || op == '-' { 10 } else { 20 };
        if prec < expr_prec {
            break;
        }
        *i += 1;
        let mut rhs = parse_primary(toks, i)?;
        loop {
            let next_op = if let Some(no) = peek(toks, *i) {
                if no.len() == 1 && "+-*/".contains(no) {
                    no.chars().next().unwrap()
                } else {
                    '\0'
                }
            } else {
                '\0'
            };
            let next_prec = if next_op == '+' || next_op == '-' {
                10
            } else if next_op == '*' || next_op == '/' {
                20
            } else {
                -1
            };
            if next_prec > prec {
                rhs = parse_binary_op_rhs(toks, i, prec + 1, rhs)?;
            } else {
                break;
            }
        }
        lhs = Expression::Binary(Box::new(lhs), op, Box::new(rhs));
    }
    Ok(lhs)
}

fn parse_expression(toks: &Vec<Tok>, i: &mut usize) -> Result<Expression, String> {
    let lhs = parse_primary(toks, i)?;
    parse_binary_op_rhs(toks, i, 0, lhs)
}

pub fn parse(src: &str) -> Result<Program, String> {
    let toks = tokenize(src);
    let mut i = 0usize;
    let mut items = Vec::new();
    let mut main_items = Vec::new();
    let has_std_import;
    let mut has_main_function = false;

    // First, check for required std import
    if let Some(t) = peek(&toks, i) {
        if t == "use" {
            i += 1;
            if let Some(u) = peek(&toks, i) {
                if u == "pie/std" {
                    i += 1;
                    if peek(&toks, i) == Some(";") {
                        i += 1;
                        has_std_import = true;
                        items.push(Item::Use("pie/std".to_string()));
                    } else {
                        return Err("expected semicolon after 'use pie/std'".into());
                    }
                } else {
                    return Err("expected 'pie/std' after 'use'".into());
                }
            } else {
                return Err("expected module name after 'use'".into());
            }
        } else {
            return Err("file must start with 'use pie/std;'".into());
        }
    } else {
        return Err("file must start with 'use pie/std;'".into());
    }

    while i < toks.len() {
        let t = peek(&toks, i).unwrap().to_string();
        if t == "use" {
            i += 1;
            if let Some(u) = peek(&toks, i) {
                if u == "pie/std" {
                    return Err("'use pie/std;' can only appear once at the top of the file".into());
                }
                items.push(Item::Use(u.to_string()));
                i += 1;
                continue;
            } else {
                return Err("expected module after use".into());
            }
        }
        if t == "module" {
            i += 1;
            let name = peek(&toks, i).ok_or("expected module name")?.to_string();
            i += 1;
            eat(&toks, &mut i, "{")?;
            let mut mod_items = Vec::new();
            while peek(&toks, i) != Some("}") {
                if peek(&toks, i) == Some("let") {
                    i += 1;
                    let typ = match peek(&toks, i).ok_or("expected type")? {
                        "int" => TypeName::Int,
                        "string" => TypeName::String,
                        "list" => TypeName::List(Box::new(TypeName::Custom("any".into()))),
                        s => TypeName::Custom(s.to_string()),
                    };
                    i += 1;
                    let name = peek(&toks, i).ok_or("expected name")?.to_string();
                    i += 1;
                    if peek(&toks, i) == Some("=") {
                        i += 1;
                        let expr = parse_expression(&toks, &mut i)?; // optional semicolon
                        if peek(&toks, i) == Some(";") {
                            i += 1;
                        }
                        mod_items.push(ModuleItem::Let { typ, name, expr });
                    } else {
                        mod_items.push(ModuleItem::Let {
                            typ,
                            name,
                            expr: Expression::ListLiteral(Vec::new()),
                        });
                    }
                    continue;
                }
                let mut is_public = false;
                if peek(&toks, i) == Some("public") {
                    is_public = true;
                    i += 1;
                }
                if peek(&toks, i) == Some("def") {
                    i += 1;
                    let ret = match peek(&toks, i).ok_or("expected return type")? {
                        "void" => TypeName::Void,
                        "int" => TypeName::Int,
                        "string" => TypeName::String,
                        s => TypeName::Custom(s.to_string()),
                    };
                    i += 1;
                    let fname = peek(&toks, i).ok_or("expected fn name")?.to_string();
                    i += 1;

                    // Check if this is the main function
                    if fname == "main" {
                        if !is_public {
                            return Err("main function must be public".into());
                        }
                        if ret != TypeName::Void {
                            return Err("main function must return void".into());
                        }
                        has_main_function = true;
                    }

                    eat(&toks, &mut i, "(")?;
                    let mut params = Vec::new();
                    while peek(&toks, i) != Some(")") {
                        let ptype = match peek(&toks, i).ok_or("expected param type")? {
                            "int" => TypeName::Int,
                            "string" => TypeName::String,
                            s => TypeName::Custom(s.to_string()),
                        };
                        i += 1;
                        let pname = peek(&toks, i).ok_or("expected param name")?.to_string();
                        i += 1;
                        params.push((ptype, pname));
                        if peek(&toks, i) == Some(",") {
                            i += 1;
                        }
                    }
                    eat(&toks, &mut i, ")")?;
                    eat(&toks, &mut i, "{")?;
                    // parse simple statements until '}'
                    let mut body = Vec::new();
                    while peek(&toks, i) != Some("}") {
                        if peek(&toks, i) == Some("let") {
                            i += 1;
                            let ltyp = match peek(&toks, i).ok_or("expected type")? {
                                "int" => TypeName::Int,
                                "string" => TypeName::String,
                                "list" => TypeName::List(Box::new(TypeName::Custom("any".into()))),
                                "map" => TypeName::Map,
                                s => TypeName::Custom(s.to_string()),
                            };
                            i += 1;
                            let lname = peek(&toks, i).ok_or("expected param name")?.to_string();
                            i += 1;
                            eat(&toks, &mut i, "=")?;
                            let lexpr = parse_expression(&toks, &mut i)?;
                            if peek(&toks, i) == Some(";") {
                                i += 1;
                            }
                            body.push(Statement::Let {
                                typ: ltyp,
                                name: lname,
                                expr: lexpr,
                            });
                            continue;
                        }
                        // return statement
                        if peek(&toks, i) == Some("return") {
                            i += 1; // consume "return"
                            let return_expr = if peek(&toks, i) == Some(";") {
                                None
                            } else {
                                let expr = parse_expression(&toks, &mut i)?;
                                if peek(&toks, i) == Some(";") {
                                    i += 1;
                                }
                                Some(expr)
                            };
                            body.push(Statement::Return(return_expr));
                            continue;
                        }
                        // expression statement or assignment
                        // Look ahead to see if this is an assignment pattern: identifier += expression
                        if let Some(id) = peek(&toks, i) {
                            if id.chars().next().unwrap().is_ascii_alphabetic()
                                || id.starts_with('_')
                            {
                                // Check if the next token after the identifier is an assignment operator
                                if let Some(op) = peek(&toks, i + 1) {
                                    if op == "+=" || op == "-=" || op == "*=" || op == "/=" {
                                        // This is an assignment, parse it directly
                                        let target = parse_expression(&toks, &mut i)?;
                                        let op_token = op.to_string();
                                        i += 1; // consume the operator
                                        let value = parse_expression(&toks, &mut i)?;
                                        if peek(&toks, i) == Some(";") {
                                            i += 1;
                                        }
                                        body.push(Statement::Assignment {
                                            target,
                                            op: op_token,
                                            value,
                                        });
                                        continue;
                                    }
                                }
                            }
                        }
                        // Regular expression statement
                        let e = parse_expression(&toks, &mut i)?;
                        if peek(&toks, i) == Some(";") {
                            i += 1;
                        }
                        body.push(Statement::Expr(e));
                    }
                    eat(&toks, &mut i, "}")?;
                    mod_items.push(ModuleItem::Function(Function {
                        name: fname,
                        params,
                        ret,
                        body,
                    }));
                    continue;
                }
                // skip unknown
                i += 1;
            }
            eat(&toks, &mut i, "}")?;
            items.push(Item::Module(Module {
                name,
                items: mod_items,
            }));
            continue;
        }
        // top-level let
        if t == "let" {
            i += 1;
            if let Some(typ_s) = peek(&toks, i) {
                let typ = match typ_s {
                    "int" => TypeName::Int,
                    "string" => TypeName::String,
                    "list" => TypeName::List(Box::new(TypeName::Custom("any".into()))),
                    "map" => TypeName::Map,
                    s => TypeName::Custom(s.to_string()),
                };
                i += 1;
                if let Some(name) = peek(&toks, i) {
                    i += 1;
                    if peek(&toks, i) == Some("=") {
                        i += 1;
                        let expr = parse_expression(&toks, &mut i)?;
                        if peek(&toks, i) == Some(";") {
                            i += 1;
                        }

                        // Add to main items
                        let let_stmt = ModuleItem::Let {
                            typ,
                            name: name.to_string(),
                            expr,
                        };
                        main_items.push(let_stmt);
                    }
                }
                continue;
            } else {
                return Err("unexpected EOF after let".into());
            }
        }
        // top-level function declaration
        if t == "public" || t == "def" {
            let mut is_public = false;
            if t == "public" {
                is_public = true;
                i += 1;
                if peek(&toks, i) != Some("def") {
                    return Err("expected 'def' after 'public'".into());
                }
            }
            i += 1; // consume "def"
            let ret = match peek(&toks, i).ok_or("expected return type")? {
                "void" => TypeName::Void,
                "int" => TypeName::Int,
                "string" => TypeName::String,
                s => TypeName::Custom(s.to_string()),
            };
            i += 1;
            let fname = peek(&toks, i).ok_or("expected fn name")?.to_string();
            i += 1;

            // Check if this is the main function
            if fname == "main" {
                if !is_public {
                    return Err("main function must be public".into());
                }
                if ret != TypeName::Void {
                    return Err("main function must return void".into());
                }
                has_main_function = true;
            }

            eat(&toks, &mut i, "(")?;
            let mut params = Vec::new();
            while peek(&toks, i) != Some(")") {
                let ptype = match peek(&toks, i).ok_or("expected param type")? {
                    "int" => TypeName::Int,
                    "string" => TypeName::String,
                    s => TypeName::Custom(s.to_string()),
                };
                i += 1;
                let pname = peek(&toks, i).ok_or("expected param name")?.to_string();
                i += 1;
                params.push((ptype, pname));
                if peek(&toks, i) == Some(",") {
                    i += 1;
                }
            }
            eat(&toks, &mut i, ")")?;
            eat(&toks, &mut i, "{")?;

            // parse function body
            let mut body = Vec::new();
            while peek(&toks, i) != Some("}") {
                if peek(&toks, i) == Some("let") {
                    i += 1;
                    let ltyp = match peek(&toks, i).ok_or("expected type")? {
                        "int" => TypeName::Int,
                        "string" => TypeName::String,
                        s => TypeName::Custom(s.to_string()),
                    };
                    i += 1;
                    let lname = peek(&toks, i).ok_or("expected param name")?.to_string();
                    i += 1;
                    eat(&toks, &mut i, "=")?;
                    let lexpr = parse_expression(&toks, &mut i)?;
                    if peek(&toks, i) == Some(";") {
                        i += 1;
                    }
                    body.push(Statement::Let {
                        typ: ltyp,
                        name: lname,
                        expr: lexpr,
                    });
                    continue;
                }
                // return statement
                if peek(&toks, i) == Some("return") {
                    i += 1; // consume "return"
                    let return_expr = if peek(&toks, i) == Some(";") {
                        None
                    } else {
                        let expr = parse_expression(&toks, &mut i)?;
                        if peek(&toks, i) == Some(";") {
                            i += 1;
                        }
                        Some(expr)
                    };
                    body.push(Statement::Return(return_expr));
                    continue;
                }
                // expression statement or assignment
                // Look ahead to see if this is an assignment pattern: identifier += expression
                if let Some(id) = peek(&toks, i) {
                    if id.chars().next().unwrap().is_ascii_alphabetic() || id.starts_with('_') {
                        // Check if the next token after the identifier is an assignment operator
                        if let Some(op) = peek(&toks, i + 1) {
                            if op == "+=" || op == "-=" || op == "*=" || op == "/=" {
                                // This is an assignment, parse it directly
                                let target = parse_expression(&toks, &mut i)?;
                                let op_token = op.to_string();
                                i += 1; // consume the operator
                                let value = parse_expression(&toks, &mut i)?;
                                if peek(&toks, i) == Some(";") {
                                    i += 1;
                                }
                                body.push(Statement::Assignment {
                                    target,
                                    op: op_token,
                                    value,
                                });
                                continue;
                            }
                        }
                    }
                }
                // Regular expression statement
                let e = parse_expression(&toks, &mut i)?;
                if peek(&toks, i) == Some(";") {
                    i += 1;
                }
                body.push(Statement::Expr(e));
            }
            eat(&toks, &mut i, "}")?;

            // Add to main items
            let func = Function {
                name: fname,
                params,
                ret,
                body,
            };
            main_items.push(ModuleItem::Function(func));
            continue;
        }

        // top-level assignment statement
        // Check if this looks like an assignment: identifier followed by +=, -=, etc.
        if let Some(id) = peek(&toks, i) {
            if id.chars().next().unwrap().is_ascii_alphabetic() || id.starts_with('_') {
                // Check if next token is an assignment operator
                if let Some(op) = peek(&toks, i + 1) {
                    if op == "+=" || op == "-=" || op == "*=" || op == "/=" {
                        // This is an assignment, parse it directly
                        let target = parse_expression(&toks, &mut i)?;
                        let op_token = op.to_string();
                        i += 1; // consume the operator
                        let value = parse_expression(&toks, &mut i)?;
                        if peek(&toks, i) == Some(";") {
                            i += 1;
                        }

                        // Add as top-level statement
                        let assign_stmt = Statement::Assignment {
                            target,
                            op: op_token,
                            value,
                        };
                        items.push(Item::TopStatement(assign_stmt));
                        continue;
                    }
                }
            }
        }

        // top-level expression statement (like function calls)
        // Try to parse as an expression statement
        let saved_i = i;
        if let Ok(expr) = parse_expression(&toks, &mut i) {
            if peek(&toks, i) == Some(";") {
                i += 1; // consume semicolon
                let expr_stmt = Statement::Expr(expr);
                items.push(Item::TopStatement(expr_stmt));
                continue;
            } else {
                // Not a statement, revert
                i = saved_i;
            }
        } else {
            // Not an expression, revert
            i = saved_i;
        }

        // fallback
        i += 1;
    }

    // Check requirements
    if !has_std_import {
        return Err("file must start with 'use pie/std;'".into());
    }
    if !has_main_function {
        return Err("file must contain 'public def void main() {}'".into());
    }

    // Add main module if there are any main items
    if !main_items.is_empty() {
        let main_module = Module {
            name: "main".to_string(),
            items: main_items,
        };
        items.push(Item::Module(main_module));
    }

    Ok(Program { items })
}
