use std::borrow::Cow;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeName<'s> {
    Int,
    Float,
    Bool,
    String,
    List,
    Map,
    Void,
    Custom(&'s str),
}

impl<'s> From<&'s str> for TypeName<'s> {
    fn from(value: &'s str) -> Self {
        match value {
            "void" => TypeName::Void,
            "string" => TypeName::String,
            "int" => TypeName::Int,
            "float" => TypeName::Float,
            "bool" => TypeName::Bool,
            "map" => TypeName::Map,
            "list" => TypeName::List,
            custom => TypeName::Custom(custom),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program<'s> {
    pub items: Vec<Item<'s>>,
}

#[derive(Debug, Clone)]
pub enum Item<'s> {
    Use(Vec<&'s str>),
    Module(Module<'s>),
    TopStatement(Statement<'s>),
}

#[derive(Debug, Clone)]
pub struct Module<'s> {
    pub name: &'s str,
    pub items: Vec<ModuleItem<'s>>,
}

#[derive(Debug, Clone)]
pub enum ModuleItem<'s> {
    Let {
        typ: TypeName<'s>,
        name: &'s str,
        expr: Expression<'s>,
    },
    Function(Function<'s>),
    Module(Module<'s>),
}

#[derive(Debug, Clone)]
pub struct Function<'s> {
    pub name: &'s str,
    pub params: Vec<(TypeName<'s>, &'s str)>,
    pub ret: TypeName<'s>,
    pub body: Vec<Statement<'s>>,
}

#[derive(Debug, Clone)]
pub enum AssignOp {
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
}

#[derive(Debug, Clone)]
pub enum Statement<'s> {
    For {
        iterable: Expression<'s>,
        var: &'s str,
        body: Vec<Statement<'s>>,
    },
    While {
        cond: Expression<'s>,
        body: Vec<Statement<'s>>,
    },
    If {
        cond: Expression<'s>,
        then_body: Vec<Statement<'s>>,
        else_body: Option<Vec<Statement<'s>>>,
    },
    Let {
        typ: TypeName<'s>,
        name: &'s str,
        expr: Expression<'s>,
    },
    Expr(Expression<'s>),
    Return(Option<Expression<'s>>),
    Assignment {
        target: Expression<'s>,
        op: AssignOp,
        value: Expression<'s>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Mul,
    Div,
    Add,
    Sub,
    Rem,
    Eq,
    LtEq,
    GtEq,
    Ne,
    Lt,
    Gt,
    And,
    Or,
}

impl BinaryOp {
    pub fn precedence(&self) -> u16 {
        use BinaryOp::*;
        match self {
            Or => 0,
            And => 1,
            Lt | LtEq | Gt | GtEq | Ne | Eq => 10,
            Add | Sub => 20,
            Mul | Div | Rem => 30,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Add,
    Sub,
    Not,
}

#[derive(Debug, Clone)]
pub enum Expression<'s> {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(Cow<'s, str>),
    Ident(&'s str),
    Binary(Box<Expression<'s>>, BinaryOp, Box<Expression<'s>>),
    Unary(UnaryOp, Box<Expression<'s>>),
    Call {
        callee: Box<Expression<'s>>,
        args: Vec<Expression<'s>>,
    },
    ModuleAccess {
        components: Vec<&'s str>,
    },
    MemberAccess {
        components: Vec<&'s str>,
    },
    ListLiteral(Vec<Expression<'s>>),
    MapLiteral(Vec<(Expression<'s>, Expression<'s>)>),
}
