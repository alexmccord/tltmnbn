unutterablec_idx::newindex!(pub ExprId);

#[derive(Debug)]
pub struct Expr {
    id: ExprId,
    kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    // x
    Var(Var),
    // \x -> x * 2
    Lam(Lam),
    // f x
    App(App),
    // e : T
    Ann(Ann),
    // 5
    Num(Num),
    // "str"
    Str(Str),
    Char(Char),
    // ()
    Unit,
    // let e = x
    // let e = x in e
    Let(Let),
    // do print "a"
    //    print "b"
    Do(Do),
    // if cond then t else f
    If(IfThenElse),
    // length =
    //   function
    //     pat -> expr
    Function(Function),
    // match f x with
    //   pat -> expr
    Match(Match),
    // (e)
    Assoc(Assoc),
    // 1: forall f a. a -> f a
    // 2: forall n | n < 2^64. n
    Forall(Forall),
    // exists a. f a -> g a
    Exists(Exists),
    Error(Option<ExprId>),
}

#[derive(Debug)]
pub struct Var(pub String);

#[derive(Debug)]
pub struct Lam(pub Box<Expr>, pub Box<Expr>);

#[derive(Debug)]
pub struct App(pub Box<Expr>, pub Box<Expr>);

#[derive(Debug)]
pub struct Ann(pub Box<Expr>, pub Box<Expr>);

#[derive(Debug)]
pub struct Num(pub String);

#[derive(Debug)]
pub struct Str(pub String);

#[derive(Debug)]
pub struct Char(pub String);

#[derive(Debug)]
pub enum Let {
    DeclExpr(Box<Expr>, Box<Expr>),
    DeclExprIn(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
pub struct Do(pub Vec<Stmt>);

#[derive(Debug)]
pub enum Stmt {
    Expr(Box<Expr>),
    Bind(Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
pub struct IfThenElse {
    antecedent: Box<Expr>,
    consequent: Box<Expr>,
    alternative: Box<Expr>,
}

#[derive(Debug)]
pub struct Arm {
    pub pat: Expr,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct Function {
    pub alternatives: Vec<Arm>,
}

#[derive(Debug)]
pub struct Match {
    pub antecedent: Box<Expr>,
    pub alternatives: Vec<Arm>,
}

#[derive(Debug)]
pub struct Assoc(pub Box<Expr>);

#[derive(Debug)]
pub struct Forall(pub Box<Expr>, pub Option<Box<Expr>>, pub Box<Expr>);

#[derive(Debug)]
pub struct Exists(pub Box<Expr>, pub Option<Box<Expr>>, pub Box<Expr>);

impl Expr {
    pub fn new(id: ExprId, kind: ExprKind) -> Expr {
        Expr { id, kind }
    }

    pub fn id(&self) -> ExprId {
        self.id
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }

    pub fn as_var(&self) -> Option<&Var> {
        match self.kind() {
            ExprKind::Var(v) => Some(v),
            _ => None,
        }
    }

    pub fn as_lam(&self) -> Option<&Lam> {
        match self.kind() {
            ExprKind::Lam(l) => Some(l),
            _ => None,
        }
    }

    pub fn as_app(&self) -> Option<&App> {
        match self.kind() {
            ExprKind::App(a) => Some(a),
            _ => None,
        }
    }

    pub fn as_ann(&self) -> Option<&Ann> {
        match self.kind() {
            ExprKind::Ann(a) => Some(a),
            _ => None,
        }
    }

    pub fn as_num(&self) -> Option<&Num> {
        match self.kind() {
            ExprKind::Num(n) => Some(n),
            _ => None,
        }
    }

    pub fn is_unit(&self) -> bool {
        matches!(self.kind(), ExprKind::Unit)
    }

    pub fn as_let(&self) -> Option<&Let> {
        match self.kind() {
            ExprKind::Let(l) => Some(l),
            _ => None,
        }
    }

    pub fn as_do(&self) -> Option<&Do> {
        match self.kind() {
            ExprKind::Do(d) => Some(d),
            _ => None,
        }
    }

    pub fn as_if(&self) -> Option<&IfThenElse> {
        match self.kind() {
            ExprKind::If(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_function(&self) -> Option<&Function> {
        match self.kind() {
            ExprKind::Function(f) => Some(f),
            _ => None,
        }
    }

    pub fn as_match(&self) -> Option<&Match> {
        match self.kind() {
            ExprKind::Match(m) => Some(m),
            _ => None,
        }
    }

    pub fn as_assoc(&self) -> Option<&Assoc> {
        match self.kind() {
            ExprKind::Assoc(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_forall(&self) -> Option<&Forall> {
        match self.kind() {
            ExprKind::Forall(f) => Some(f),
            _ => None,
        }
    }

    pub fn as_exists(&self) -> Option<&Exists> {
        match self.kind() {
            ExprKind::Exists(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_error(&self) -> Option<Option<ExprId>> {
        match self.kind() {
            ExprKind::Error(e) => Some(e.clone()),
            _ => None,
        }
    }
}

impl ExprKind {
    pub fn var(id: String) -> ExprKind {
        ExprKind::Var(Var::new(id))
    }

    pub fn lam(p: Expr, e: Expr) -> ExprKind {
        ExprKind::Lam(Lam::new(p, e))
    }

    pub fn app(f: Expr, a: Expr) -> ExprKind {
        ExprKind::App(App::new(f, a))
    }

    pub fn ann(e: Expr, t: Expr) -> ExprKind {
        ExprKind::Ann(Ann::new(e, t))
    }

    pub fn num(num: String) -> ExprKind {
        ExprKind::Num(Num::new(num))
    }

    pub fn str(str: String) -> ExprKind {
        ExprKind::Str(Str::new(str))
    }

    pub fn char(str: String) -> ExprKind {
        ExprKind::Char(Char::new(str))
    }

    pub fn unit() -> ExprKind {
        ExprKind::Unit
    }

    pub fn let_be(e1: Expr, e2: Expr) -> ExprKind {
        ExprKind::Let(Let::the_expr_be(e1, e2))
    }

    pub fn let_be_in(e1: Expr, e2: Expr, e3: Expr) -> ExprKind {
        ExprKind::Let(Let::the_expr_be_in(e1, e2, e3))
    }

    pub fn do_block(stmts: Vec<Stmt>) -> ExprKind {
        ExprKind::Do(Do::new(stmts))
    }

    pub fn if_then_else(antecedent: Expr, consequent: Expr, alternative: Expr) -> ExprKind {
        ExprKind::If(IfThenElse::new(antecedent, consequent, alternative))
    }

    pub fn function(alternatives: Vec<Arm>) -> ExprKind {
        ExprKind::Function(Function::new(alternatives))
    }

    pub fn match_with(antecedent: Expr, alternatives: Vec<Arm>) -> ExprKind {
        ExprKind::Match(Match::new(antecedent, alternatives))
    }

    pub fn forall(param: Expr, constraint: Option<Expr>, expr: Expr) -> ExprKind {
        ExprKind::Forall(Forall::new(param, constraint, expr))
    }

    pub fn exists(param: Expr, constraint: Option<Expr>, expr: Expr) -> ExprKind {
        ExprKind::Exists(Exists::new(param, constraint, expr))
    }

    pub fn assoc(expr: Expr) -> ExprKind {
        ExprKind::Assoc(Assoc::new(expr))
    }
}

impl Var {
    pub fn new(id: String) -> Var {
        Var(id)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Lam {
    pub fn new(p: Expr, e: Expr) -> Lam {
        Lam(Box::new(p), Box::new(e))
    }

    pub fn param(&self) -> &Expr {
        &self.0
    }

    pub fn expr(&self) -> &Expr {
        &self.1
    }
}

impl App {
    pub fn new(f: Expr, a: Expr) -> App {
        App(Box::new(f), Box::new(a))
    }

    pub fn function(&self) -> &Expr {
        &self.0
    }

    pub fn argument(&self) -> &Expr {
        &self.1
    }
}

impl Ann {
    pub fn new(e: Expr, t: Expr) -> Ann {
        Ann(Box::new(e), Box::new(t))
    }

    pub fn expr(&self) -> &Expr {
        &self.0
    }

    pub fn ty(&self) -> &Expr {
        &self.1
    }
}

impl Num {
    pub fn new(num: String) -> Num {
        Num(num)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Str {
    pub fn new(str: String) -> Str {
        Str(str)
    }
}

impl Char {
    pub fn new(str: String) -> Char {
        Char(str)
    }
}

impl Let {
    pub fn the_expr_be(e1: Expr, e2: Expr) -> Let {
        Let::DeclExpr(Box::new(e1), Box::new(e2))
    }

    pub fn the_expr_be_in(e1: Expr, e2: Expr, e3: Expr) -> Let {
        Let::DeclExprIn(Box::new(e1), Box::new(e2), Box::new(e3))
    }

    pub fn get_the_expr(&self) -> &Expr {
        match self {
            Let::DeclExpr(the, _) => &the,
            Let::DeclExprIn(the, _, _) => &the,
        }
    }

    pub fn get_be_expr(&self) -> &Expr {
        match self {
            Let::DeclExpr(_, be) => &be,
            Let::DeclExprIn(_, be, _) => &be,
        }
    }

    pub fn get_in_expr(&self) -> Option<&Expr> {
        match self {
            Let::DeclExpr(_, _) => None,
            Let::DeclExprIn(_, _, in_the) => Some(&in_the),
        }
    }
}

impl Do {
    pub fn new(stmts: Vec<Stmt>) -> Do {
        Do(stmts)
    }
}

impl Stmt {
    pub fn expr(expr: Expr) -> Stmt {
        Stmt::Expr(Box::new(expr))
    }
}

impl IfThenElse {
    pub fn new(antecedent: Expr, consequent: Expr, alternative: Expr) -> IfThenElse {
        IfThenElse {
            antecedent: Box::new(antecedent),
            consequent: Box::new(consequent),
            alternative: Box::new(alternative),
        }
    }

    pub fn antecedent(&self) -> &Expr {
        &self.antecedent
    }

    pub fn consequent(&self) -> &Expr {
        &self.consequent
    }

    pub fn alternative(&self) -> &Expr {
        &self.alternative
    }
}

impl Function {
    pub fn new(alternatives: Vec<Arm>) -> Function {
        Function { alternatives }
    }
}

impl Match {
    pub fn new(antecedent: Expr, alternatives: Vec<Arm>) -> Match {
        Match {
            antecedent: Box::new(antecedent),
            alternatives,
        }
    }
}

impl Assoc {
    pub fn new(expr: Expr) -> Assoc {
        Assoc(Box::new(expr))
    }
}

impl Forall {
    pub fn new(param: Expr, constraint: Option<Expr>, expr: Expr) -> Forall {
        Forall(Box::new(param), constraint.map(Box::new), Box::new(expr))
    }
}

impl Exists {
    pub fn new(param: Expr, constraint: Option<Expr>, expr: Expr) -> Exists {
        Exists(Box::new(param), constraint.map(Box::new), Box::new(expr))
    }
}
