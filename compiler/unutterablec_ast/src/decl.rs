use crate::expr::Expr;

unutterablec_idx::newindex!(pub DeclId);

#[derive(Debug)]
pub struct Decl {
    id: DeclId,
    kind: DeclKind,
}

#[derive(Debug)]
pub enum DeclKind {
    // module Foo
    Module(Module),
    // import Foo.Bar
    Import(Import),
    // export <decl>
    Export(Export),
    // pub <decl>
    Public(Public),
    // open <decl>
    Open(Open),
    // record Foo where
    //   a : A
    //   b : B
    Record(Record),
    // data Vec n a where
    //   []   : Vec 0 a
    //   (::) : a -> Vec n a -> Vec (n + 1) a
    Data(Data),
    // trait Functor f where
    //   map : (a -> b) -> f a -> f b
    Trait(Trait),
    // impl Functor Maybe where
    //   map f = function
    //     Just x  -> Just (f x)
    //     Nothing -> Nothing
    Impl(Impl),
    // e : T
    Sig(Sig),
    // e1 = e2
    Equation(Equation),
    // In REPL mode only.
    Expr(Expr),
    Error(Option<DeclId>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Path {
    nodes: Vec<PathNode>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PathNode {
    Name(String),
    Missing,
}

#[derive(Debug)]
pub struct Module {
    path: Option<Path>,
    decls: Option<Vec<Decl>>,
}

#[derive(Debug)]
pub struct Import {
    path: Path,
}

#[derive(Debug)]
pub struct Export(pub Box<Decl>); // TODO: export (..)

#[derive(Debug)]
pub struct Public(pub Box<Decl>);

#[derive(Debug)]
pub struct Open(pub Box<Decl>);

#[derive(Debug)]
pub struct Record(pub Expr, pub Vec<Expr>);

#[derive(Debug)]
pub struct Data {
    sig: Expr,
    kind: DataKind,
    deriving: Option<Deriving>,
}

#[derive(Debug)]
pub enum DataKind {
    NoCons,
    Equals(Expr),
    Where(Vec<Expr>),
}

#[derive(Debug)]
pub struct Deriving(pub Expr);

#[derive(Debug)]
pub struct Trait(pub Expr, pub Vec<Expr>);

#[derive(Debug)]
pub struct Impl(pub Expr, pub Vec<Expr>);

#[derive(Debug)]
pub struct Sig(pub Expr, pub Expr);

#[derive(Debug)]
pub struct Equation(pub Expr, pub Expr);

impl Decl {
    pub fn new(id: DeclId, kind: DeclKind) -> Decl {
        Decl { id, kind }
    }
    // pub fn new(id: DeclId, kind: DeclKind, token_span: TokenSpan) -> Decl {
    //     Decl {
    //         id,
    //         kind,
    //         token_span,
    //     }
    // }

    pub fn id(&self) -> DeclId {
        self.id
    }

    pub fn kind(&self) -> &DeclKind {
        &self.kind
    }

    // pub fn token_span(&self) -> TokenSpan {
    //     self.token_span.clone()
    // }

    // pub fn begin_token(&self) -> TokenId {
    //     self.token_span.begin
    // }

    // pub fn end_token(&self) -> TokenId {
    //     self.token_span.end
    // }

    pub fn as_module(&self) -> Option<&Module> {
        match self.kind() {
            DeclKind::Module(m) => Some(m),
            _ => None,
        }
    }

    pub fn as_import(&self) -> Option<&Import> {
        match self.kind() {
            DeclKind::Import(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_export(&self) -> Option<&Export> {
        match self.kind() {
            DeclKind::Export(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_public(&self) -> Option<&Public> {
        match self.kind() {
            DeclKind::Public(p) => Some(p),
            _ => None,
        }
    }

    pub fn as_open(&self) -> Option<&Open> {
        match self.kind() {
            DeclKind::Open(o) => Some(o),
            _ => None,
        }
    }

    pub fn as_record(&self) -> Option<&Record> {
        match self.kind() {
            DeclKind::Record(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_data(&self) -> Option<&Data> {
        match self.kind() {
            DeclKind::Data(d) => Some(d),
            _ => None,
        }
    }

    pub fn as_trait(&self) -> Option<&Trait> {
        match self.kind() {
            DeclKind::Trait(c) => Some(c),
            _ => None,
        }
    }

    pub fn as_impl(&self) -> Option<&Impl> {
        match self.kind() {
            DeclKind::Impl(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_sig(&self) -> Option<&Sig> {
        match self.kind() {
            DeclKind::Sig(sig) => Some(sig),
            _ => None,
        }
    }

    pub fn as_equation(&self) -> Option<&Equation> {
        match self.kind() {
            DeclKind::Equation(eq) => Some(eq),
            _ => None,
        }
    }

    pub fn as_expr(&self) -> Option<&Expr> {
        match self.kind() {
            DeclKind::Expr(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_error(&self) -> Option<Option<DeclId>> {
        match self.kind() {
            DeclKind::Error(e) => Some(e.clone()),
            _ => None,
        }
    }
}

impl DeclKind {
    pub fn new_module(path: Option<Path>, decls: Option<Vec<Decl>>) -> DeclKind {
        DeclKind::Module(Module::new(path, decls))
    }

    pub fn new_import(path: Path) -> DeclKind {
        DeclKind::Import(Import::new(path))
    }

    pub fn new_export(decl: Decl) -> DeclKind {
        DeclKind::Export(Export::new(decl))
    }

    pub fn new_public(decl: Decl) -> DeclKind {
        DeclKind::Public(Public::new(decl))
    }
    pub fn new_open(decl: Decl) -> DeclKind {
        DeclKind::Open(Open::new(decl))
    }

    pub fn new_record(sig: Expr, fields: Vec<Expr>) -> DeclKind {
        DeclKind::Record(Record::new(sig, fields))
    }

    pub fn new_data(sig: Expr, cons: DataKind, deriving: Option<Deriving>) -> DeclKind {
        DeclKind::Data(Data::new(sig, cons, deriving))
    }

    pub fn empty_data(sig: Expr) -> DeclKind {
        DeclKind::Data(Data::empty(sig))
    }

    pub fn new_data_adt(sig: Expr, constructors: Expr, deriving: Option<Deriving>) -> DeclKind {
        DeclKind::Data(Data::adt(sig, constructors, deriving))
    }

    pub fn new_data_gadt(
        sig: Expr,
        constructors: Vec<Expr>,
        deriving: Option<Deriving>,
    ) -> DeclKind {
        DeclKind::Data(Data::gadt(sig, constructors, deriving))
    }

    pub fn new_trait(sig: Expr, body: Vec<Expr>) -> DeclKind {
        DeclKind::Trait(Trait::new(sig, body))
    }

    pub fn new_impl(sig: Expr, body: Vec<Expr>) -> DeclKind {
        DeclKind::Impl(Impl::new(sig, body))
    }

    pub fn new_sig(sig: Expr, ty: Expr) -> DeclKind {
        DeclKind::Sig(Sig::new(sig, ty))
    }

    pub fn new_equation(pat: Expr, expr: Expr) -> DeclKind {
        DeclKind::Equation(Equation::new(pat, expr))
    }

    pub fn expr(expr: Expr) -> DeclKind {
        DeclKind::Expr(expr)
    }
}

impl Path {
    pub fn new(nodes: Vec<PathNode>) -> Path {
        Path { nodes }
    }
}

impl Module {
    pub fn new(path: Option<Path>, decls: Option<Vec<Decl>>) -> Module {
        Module { path, decls }
    }

    pub fn path(&self) -> Option<&Path> {
        self.path.as_ref()
    }

    pub fn decls(&self) -> Option<&Vec<Decl>> {
        self.decls.as_ref()
    }
}

impl Import {
    pub fn new(path: Path) -> Import {
        Import { path }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl Export {
    pub fn new(decl: Decl) -> Export {
        Export(Box::new(decl))
    }

    pub fn decl(&self) -> &Decl {
        &self.0
    }
}

impl Public {
    pub fn new(decl: Decl) -> Public {
        Public(Box::new(decl))
    }

    pub fn decl(&self) -> &Decl {
        &self.0
    }
}

impl Open {
    pub fn new(decl: Decl) -> Open {
        Open(Box::new(decl))
    }

    pub fn decl(&self) -> &Decl {
        &self.0
    }
}

impl Record {
    pub fn new(sig: Expr, fields: Vec<Expr>) -> Record {
        Record(sig, fields)
    }

    pub fn sig(&self) -> &Expr {
        &self.0
    }

    pub fn fields(&self) -> &Vec<Expr> {
        &self.1
    }
}

impl Data {
    pub fn new(sig: Expr, kind: DataKind, deriving: Option<Deriving>) -> Data {
        Data {
            sig,
            kind,
            deriving,
        }
    }

    pub fn empty(sig: Expr) -> Data {
        Data::new(sig, DataKind::NoCons, None)
    }

    pub fn adt(sig: Expr, constructors: Expr, deriving: Option<Deriving>) -> Data {
        Data::new(sig, DataKind::Equals(constructors), deriving)
    }

    pub fn gadt(sig: Expr, constructors: Vec<Expr>, deriving: Option<Deriving>) -> Data {
        Data::new(sig, DataKind::Where(constructors), deriving)
    }

    pub fn sig(&self) -> &Expr {
        &self.sig
    }

    pub fn kind(&self) -> &DataKind {
        &self.kind
    }

    pub fn deriving(&self) -> Option<&Deriving> {
        self.deriving.as_ref()
    }
}

impl Trait {
    pub fn new(sig: Expr, body: Vec<Expr>) -> Trait {
        Trait(sig, body)
    }

    pub fn sig(&self) -> &Expr {
        &self.0
    }

    pub fn body(&self) -> &Vec<Expr> {
        &self.1
    }
}

impl Impl {
    pub fn new(sig: Expr, body: Vec<Expr>) -> Impl {
        Impl(sig, body)
    }

    pub fn sig(&self) -> &Expr {
        &self.0
    }

    pub fn body(&self) -> &Vec<Expr> {
        &self.1
    }
}

impl Sig {
    pub fn new(sig: Expr, expr: Expr) -> Sig {
        Sig(sig, expr)
    }

    pub fn sig(&self) -> &Expr {
        &self.0
    }
}

impl Equation {
    pub fn new(lhs: Expr, rhs: Expr) -> Equation {
        Equation(lhs, rhs)
    }

    pub fn function(&self) -> &Expr {
        &self.0
    }

    pub fn expression(&self) -> &Expr {
        &self.1
    }
}
