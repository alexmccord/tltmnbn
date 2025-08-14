use std::collections::VecDeque;

use crate::ast::AstNodeId;

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub struct AstQueue {
    queue: VecDeque<AstNodeId>,
}

impl AstQueue {
    pub fn new() -> AstQueue {
        AstQueue {
            queue: VecDeque::new(),
        }
    }
}
