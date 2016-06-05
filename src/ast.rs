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
pub struct Prototype<'a>  {
    name: &'a str,
    args: Vec<&'a str>
}

impl<'a> Prototype<'a> {
    pub fn new(name: &'a str, args: Vec<&'a str>) -> Prototype<'a> {
        Prototype{name: name, args: args}
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Function<'a> {
    prototype: Box<Prototype<'a>>,
    body: Box<Expression<'a>>
}

impl<'a> Function<'a> {
    pub fn new(name: &'a str, args: Vec<&'a str>, body: Expression<'a>) -> Function<'a> {
        Function {
            prototype: Box::new(Prototype::new(name, args)),
            body: Box::new(body)
        }
    }

    pub fn name(&self) -> &'a str {
        self.prototype.name
    }

    pub fn args(&self) -> &Vec<&'a str> {
        &self.prototype.args
    }

    pub fn body(&self) -> &Expression<'a> {
        self.body.as_ref()
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
