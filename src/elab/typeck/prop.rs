use std::ops;

use id_arena::{Arena, Id};

#[derive(Debug, Default, Clone)]
pub struct PropositionalCalculus {
    props: PropositionArena,
}

impl PropositionalCalculus {
    pub fn new() -> PropositionalCalculus {
        PropositionalCalculus {
            props: PropositionArena::new(),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct PropositionArena {
    arena: Arena<Proposition>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PropositionId(Id<Proposition>);

impl PropositionArena {
    pub fn new() -> PropositionArena {
        PropositionArena {
            arena: Arena::new(),
        }
    }

    pub fn alloc(&mut self, prop: impl Into<Proposition>) -> PropositionId {
        PropositionId(self.arena.alloc(prop.into()))
    }

    pub fn and(&mut self, p: impl Into<Proposition>, q: impl Into<Proposition>) -> PropositionId {
        let p = self.alloc(p);
        let q = self.alloc(q);
        self.alloc(Proposition::And { p, q })
    }

    pub fn or(&mut self, p: impl Into<Proposition>, q: impl Into<Proposition>) -> PropositionId {
        let p = self.alloc(p);
        let q = self.alloc(q);
        self.alloc(Proposition::Or { p, q })
    }

    pub fn not(&mut self, p: impl Into<Proposition>) -> PropositionId {
        let p = self.alloc(p);
        self.alloc(Proposition::Not { p })
    }

    pub fn eq(&mut self, p: impl Into<Proposition>, q: impl Into<Proposition>) -> PropositionId {
        let p = self.alloc(p);
        let q = self.alloc(q);
        self.alloc(Proposition::Eq { p, q })
    }

    pub fn implies(
        &mut self,
        p: impl Into<Proposition>,
        q: impl Into<Proposition>,
    ) -> PropositionId {
        let p = self.alloc(p);
        let q = self.alloc(q);
        self.alloc(Proposition::Implies { p, q })
    }
}

impl PropositionId {
    pub fn index(&self) -> usize {
        self.0.index()
    }
}

impl ops::Index<PropositionId> for PropositionArena {
    type Output = Proposition;

    fn index(&self, index: PropositionId) -> &Self::Output {
        &self.arena[index.0]
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Proposition {
    And { p: PropositionId, q: PropositionId },
    Or { p: PropositionId, q: PropositionId },
    Not { p: PropositionId },
    Eq { p: PropositionId, q: PropositionId },
    Implies { p: PropositionId, q: PropositionId },
}

impl Proposition {
    pub fn as_conjunction(&self) -> Option<Conjunction> {
        match self {
            &Proposition::And { p, q } => Some(Conjunction { p, q }),
            _ => None,
        }
    }

    pub fn as_disjunction(&self) -> Option<Disjunction> {
        match self {
            &Proposition::Or { p, q } => Some(Disjunction { p, q }),
            _ => None,
        }
    }

    pub fn as_negation(&self) -> Option<Negation> {
        match self {
            &Proposition::Not { p } => Some(Negation { p }),
            _ => None,
        }
    }

    pub fn as_equivalent(&self) -> Option<Equivalent> {
        match self {
            &Proposition::Eq { p, q } => Some(Equivalent { p, q }),
            _ => None,
        }
    }

    pub fn as_implication(&self) -> Option<Implication> {
        match self {
            &Proposition::Implies { p, q } => Some(Implication { p, q }),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Conjunction {
    pub p: PropositionId,
    pub q: PropositionId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Disjunction {
    pub p: PropositionId,
    pub q: PropositionId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Negation {
    pub p: PropositionId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Equivalent {
    pub p: PropositionId,
    pub q: PropositionId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Implication {
    pub p: PropositionId,
    pub q: PropositionId,
}

impl From<Conjunction> for Proposition {
    fn from(value: Conjunction) -> Self {
        Proposition::And {
            p: value.p,
            q: value.q,
        }
    }
}

impl From<Disjunction> for Proposition {
    fn from(value: Disjunction) -> Self {
        Proposition::Or {
            p: value.p,
            q: value.q,
        }
    }
}

impl From<Equivalent> for Proposition {
    fn from(value: Equivalent) -> Self {
        Proposition::Eq {
            p: value.p,
            q: value.q,
        }
    }
}

impl From<Implication> for Proposition {
    fn from(value: Implication) -> Self {
        Proposition::Implies {
            p: value.p,
            q: value.q,
        }
    }
}
