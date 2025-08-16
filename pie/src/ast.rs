#[derive(Debug, Clone, PartialEq)]
pub enum TypeName {
    Int,
    String,
    List(Box<TypeName>),
    Map,
    Void,
    Custom(String),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Item {
    Use(String),
    Module(Module),
    TopStatement(Statement),
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub items: Vec<ModuleItem>,
}

#[derive(Debug, Clone)]
pub enum ModuleItem {
    Let { typ: TypeName, name: String, expr: Expression },
    Function(Function),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub public: bool,
    pub name: String,
    pub params: Vec<(TypeName, String)>,
    pub ret: TypeName,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let { typ: TypeName, name: String, expr: Expression },
    Expr(Expression),
    Return(Option<Expression>),
    For { typ: TypeName, name: String, iter: Expression, body: Vec<Statement> },
    AssignmentPattern { pattern: Expression, expr: Expression },
    Assignment { target: Expression, op: String, value: Expression },
}

#[derive(Debug, Clone)]
pub enum Expression {
    Int(i64),
    Str(String),
    Ident(String),
    Binary(Box<Expression>, char, Box<Expression>),
    Call { callee: Box<Expression>, args: Vec<Expression> },
    ModuleAccess { module: String, name: String },
    ListLiteral(Vec<Expression>),
    MapLiteral(Vec<(String, Expression)>),
    PatternMap(Vec<(String, String)>), // {name: user_name, phone: user_phone}
}


