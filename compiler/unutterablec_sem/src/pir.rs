use std::collections::HashMap;

use unutterablec_idx;

// Yes.
//
// In the very first line of writing the type system, you can
// already see an opportunity to encode Girard's paradox by
// exploiting integer overflow here.
//
// I do not care. It's a bootstrapping compiler.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Level(u128);

impl Level {
    fn successor(self) -> Level {
        Level(self.0 + 1)
    }
}

#[derive(Debug)]
pub enum Universe {
    Uni(Level), // proof relevant
    Omega,      // proof irrelevant
    Effect,     // dependent effects for free
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Var(u64);

#[derive(Debug)]
pub struct Context {
    bindings: HashMap<TermId, TermId>,
}

impl Context {
    fn new() -> Context {
        Context {
            bindings: HashMap::default(),
        }
    }

    fn insert(&mut self, v: TermId, t: TermId) {
        self.bindings.insert(v, t);
    }

    fn lookup(&self, v: TermId) -> Option<TermId> {
        self.bindings.get(&v).cloned()
    }
}

#[derive(Debug)]
pub struct Binding(Var, TermId);

#[derive(Debug)]
pub struct Sort(Universe);

unutterablec_idx::newindex!(pub TermId);

pub type TermVec = unutterablec_idx::IndexedVec<TermId, Term>;

pub struct Term {
    id: TermId,
    kind: TermKind,
}

impl Term {
    fn new(id: TermId, kind: TermKind) -> Term {
        Term { id, kind }
    }

    pub fn id(&self) -> TermId {
        self.id
    }

    pub fn kind(&self) -> &TermKind {
        &self.kind
    }
}

#[derive(Debug)]
pub enum TermKind {
    // x
    Var(Var),
    // s
    Sort(Sort),
    // \(x : A). t
    Lam(Binding, TermId),
    // t u
    App(TermId, TermId),
    // Pi s,s' (x : A). B
    Pi(Sort, Sort, Binding, TermId),
    // !-elim(A, t)
    BottomElim(TermId, TermId),
    // !
    Bottom,
    // t ~A u
    Equiv(TermId, TermId, TermId),
    // refl(t)
    Refl(TermId),
    // transp(A, t, B, u, t', e)
    Transp(TermId, TermId, TermId, TermId, TermId, TermId),
    // cast(A, B, e, t)
    Cast(TermId, TermId, TermId, TermId),
    /// First projection (domain) of equality between two dependent function types
    Pi1,
    /// Second projection (codomain) of equality between two dependent function types
    Pi2,
    /// Omega universe extensionality: if two propositions implies each other.
    OmegaExt,
    /// Function extensionality: forall x in `f`, if `g x = f x` then `g = f`.
    PiExt,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypingError {
    UnknownVar(Var),
}

pub fn check(ctx: &Context, term: &Term) -> Result<(), TypingError> {
    match term.kind() {
        TermKind::Var(var) => {
            let res = ctx.lookup(term.id()).ok_or(TypingError::UnknownVar(*var))?;
            Ok(()) // TODO: Conv typing rule
        }
        TermKind::Sort(sort) => todo!(),
        TermKind::Lam(binding, cell) => todo!(),
        TermKind::App(cell, cell1) => todo!(),
        TermKind::Pi(level, sort, binding, cell) => todo!(),
        TermKind::BottomElim(cell, cell1) => todo!(),
        TermKind::Bottom => todo!(),
        TermKind::Equiv(cell, cell1, cell2) => todo!(),
        TermKind::Refl(cell) => todo!(),
        TermKind::Transp(cell, cell1, cell2, cell3, cell4, cell5) => todo!(),
        TermKind::Cast(cell, cell1, cell2, cell3) => todo!(),
        TermKind::Pi1 => todo!(),
        TermKind::Pi2 => todo!(),
        TermKind::OmegaExt => todo!(),
        TermKind::PiExt => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_var_in_context() {
        let mut terms = TermVec::new();
        let mut ctx = Context::new();

        let x_term = terms.allocate_id(|id| Term::new(id, TermKind::Var(Var(0))));

        let a = Sort(Universe::Uni(Level(0)));
        let a_term = terms.allocate_id(|id| Term::new(id, TermKind::Sort(a)));

        ctx.insert(x_term, a_term);

        let res = check(&ctx, &terms[x_term]);
        assert_eq!(res, Ok(()));
    }

    #[test]
    fn check_var_not_in_context() {
        let mut terms = TermVec::new();
        let ctx = Context::new();

        let x = Var(0);
        let x_term = terms.allocate_id(|id| Term::new(id, TermKind::Var(x)));

        let res = check(&ctx, &terms[x_term]);
        assert_eq!(res, Err(TypingError::UnknownVar(x)));
    }
}
