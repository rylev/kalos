#[derive(PartialEq, Clone, Debug)]
pub struct Ast<'a> {
    nodes: Vec<AstNode<'a>>
}

impl<'a> Ast<'a> {
    pub fn new(nodes: Vec<AstNode<'a>>) -> Ast<'a> {
        Ast { nodes: nodes }
    }

    pub fn nodes(&self) -> &Vec<AstNode<'a>> {
        &self.nodes
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum AstNode<'a> {
    FuncDeclartion(Function<'a>)
}

#[derive(PartialEq, Clone, Debug)]
struct Prototype<'a>  {
    name: &'a str,
    args: Vec<&'a str>
}

impl<'a> Prototype<'a> {
    fn new(name: &'a str, args: Vec<&'a str>) -> Self {
        Prototype{name: name, args: args}
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct FunctionBody<'a> {
    expressions: Vec<Expression<'a>> // TODO: make a vec
}

impl<'a> FunctionBody<'a> {
    fn new(expressions: Vec<Expression<'a>>) -> Self {
        FunctionBody { expressions: expressions }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Function<'a> {
    prototype: Prototype<'a>,
    body: FunctionBody<'a>
}

impl<'a> Function<'a> {
    pub fn new(name: &'a str, args: Vec<&'a str>, body: Expression<'a>) -> Self {
        Function {
            prototype: Prototype::new(name, args),
            body: FunctionBody::new(vec!(body)),
        }
    }

    pub fn name(&self) -> &'a str {
        self.prototype.name
    }

    // TODO: rename to params
    pub fn args(&self) -> &Vec<&'a str> {
        &self.prototype.args
    }

    pub fn body(&self) -> &Vec<Expression<'a>> {
        &self.body.expressions
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expression<'a> {
    Value(f64),
    Negate(Box<Expression<'a>>),
    Add(Box<Expression<'a>>, Box<Expression<'a>>),
    Subtract(Box<Expression<'a>>, Box<Expression<'a>>),
    Multiply(Box<Expression<'a>>, Box<Expression<'a>>),
    Divide(Box<Expression<'a>>, Box<Expression<'a>>),
    Variable(&'a str),
    FunctionCall(&'a str, Vec<Expression<'a>>)
}
