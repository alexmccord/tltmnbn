use std::ops;

use crate::elab::typeck::obligations::{Obligation, ObligationId};

#[derive(Debug, Default, Clone)]
pub struct ObligationForest {
    nodes: Vec<ObligationNode>,
}

#[derive(Debug, Clone)]
pub struct ObligationNode {
    parent: Option<ObligationId>,
    children: Vec<ObligationId>,
    obligation: Obligation,
}

#[derive(Debug)]
pub struct ObligationNodeRef<'a> {
    forest: &'a ObligationForest,
    obligation: ObligationId,
}

#[derive(Debug)]
pub struct ObligationNodeMut<'a> {
    forest: &'a mut ObligationForest,
    obligation: ObligationId,
}

impl ObligationForest {
    pub fn new() -> ObligationForest {
        ObligationForest { nodes: Vec::new() }
    }

    pub fn get(&self, obligation_id: ObligationId) -> ObligationNodeRef<'_> {
        ObligationNodeRef {
            forest: self,
            obligation: obligation_id,
        }
    }

    pub fn get_mut(&mut self, obligation_id: ObligationId) -> ObligationNodeMut<'_> {
        ObligationNodeMut {
            forest: self,
            obligation: obligation_id,
        }
    }

    pub fn insert(
        &mut self,
        parent: Option<ObligationId>,
        obligation: impl Into<Obligation>,
    ) -> ObligationNodeMut<'_> {
        let id = ObligationId(self.nodes.len() as u32);

        let node = ObligationNode::new(parent, obligation);
        self.nodes.push(node);

        let mut node = self.get_mut(id);
        if let Some(mut parent_node) = node.parent_mut() {
            parent_node.node_mut().children.push(id);
        }

        self.get_mut(id)
    }

    pub fn insert_root(&mut self, obligation: impl Into<Obligation>) -> ObligationNodeMut<'_> {
        self.insert(None, obligation)
    }

    pub fn insert_child(
        &mut self,
        parent: ObligationId,
        obligation: impl Into<Obligation>,
    ) -> ObligationNodeMut<'_> {
        self.insert(Some(parent), obligation)
    }
}

impl ObligationNode {
    fn new(parent: Option<ObligationId>, obligation: impl Into<Obligation>) -> ObligationNode {
        ObligationNode {
            parent,
            children: Vec::new(),
            obligation: obligation.into(),
        }
    }

    pub fn parent(&self) -> Option<ObligationId> {
        self.parent
    }

    pub fn children(&self) -> &[ObligationId] {
        &self.children
    }

    pub fn obligation(&self) -> &Obligation {
        &self.obligation
    }
}

impl ObligationNodeRef<'_> {
    pub fn id(&self) -> ObligationId {
        self.obligation
    }

    pub fn node(&self) -> &ObligationNode {
        &self.forest[self.id()]
    }

    pub fn parent(&self) -> Option<ObligationNodeRef<'_>> {
        self.node().parent().map(|parent| self.forest.get(parent))
    }

    pub fn obligation(&self) -> &Obligation {
        self.node().obligation()
    }
}

impl ObligationNodeMut<'_> {
    pub fn id(&self) -> ObligationId {
        self.obligation
    }

    pub fn node(&self) -> &ObligationNode {
        &self.forest[self.id()]
    }

    pub fn node_mut(&mut self) -> &mut ObligationNode {
        let id = self.id();
        &mut self.forest[id]
    }

    pub fn parent(&self) -> Option<ObligationNodeRef<'_>> {
        self.node().parent().map(|parent| self.forest.get(parent))
    }

    pub fn parent_mut(&mut self) -> Option<ObligationNodeMut<'_>> {
        self.node()
            .parent()
            .map(|parent| self.forest.get_mut(parent))
    }

    pub fn obligation(&self) -> &Obligation {
        self.node().obligation()
    }

    pub fn add(&mut self, obligation: impl Into<Obligation>) -> ObligationNodeMut<'_> {
        self.forest.insert_child(self.obligation, obligation)
    }
}

impl ops::Index<ObligationId> for ObligationForest {
    type Output = ObligationNode;

    fn index(&self, index: ObligationId) -> &Self::Output {
        &self.nodes[index.index()]
    }
}

impl ops::IndexMut<ObligationId> for ObligationForest {
    fn index_mut(&mut self, index: ObligationId) -> &mut Self::Output {
        &mut self.nodes[index.index()]
    }
}

#[cfg(test)]
mod tests {
    use crate::elab::typeck::obligations::SubtypeObligation;
    use crate::elab::typeck::{ty::ir::PrimitiveTy, type_graph::TypeGraph};

    use super::*;

    #[test]
    fn parent_child_relations() {
        let mut type_graph = TypeGraph::new();
        let nil_ty = type_graph.alloc_ty(PrimitiveTy::Nil);
        let num_ty = type_graph.alloc_ty(PrimitiveTy::Number);

        let mut forest = ObligationForest::new();

        let ob1 = forest
            .insert_root(SubtypeObligation::new(num_ty, nil_ty))
            .id();

        assert!(forest[ob1].parent().is_none());
        assert!(forest[ob1].children().is_empty());

        let ob2 = forest
            .insert_child(ob1, SubtypeObligation::new(nil_ty, num_ty))
            .id();

        assert!(forest[ob2].parent().is_some_and(|p| p == ob1));
        assert!(forest[ob2].children().is_empty());
        assert!(forest[ob1].children().len() == 1);
        assert!(forest[ob1].children()[0] == ob2);
    }
}
