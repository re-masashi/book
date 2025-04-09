#![allow(clippy::needless_lifetimes)]
// clippy goes nuts for no reason at all over false positives.
// see https://github.com/rust-lang/rust-clippy/issues/740
// and https://github.com/rust-lang/rust-clippy/issues/13749 maybe

use crate::codegen::TypedExpr;

use std::collections::HashMap;
#[derive(Debug, Clone)]
pub enum Node<'a> {
    Start,
    End,
    BasicBlock(Vec<TypedExpr<'a>>), // Modified to use Basic Blocks
    Condition(TypedExpr<'a>),
}

#[derive(Debug)]
pub struct ControlFlowGraph<'a> {
    pub nodes: Vec<Node<'a>>,
    pub edges: HashMap<usize, Vec<usize>>, // Adjacency list
    pub next_node_id: usize,
}

impl<'a> Default for ControlFlowGraph<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> ControlFlowGraph<'a> {
    pub fn new() -> Self {
        ControlFlowGraph {
            nodes: vec![],
            edges: HashMap::new(), // Adjacency list
            next_node_id: 0,
        }
    }

    fn add_node(&mut self, node: Node<'a>) -> usize {
        self.nodes.push(node);
        let id = self.next_node_id;
        self.next_node_id += 1;
        id
    }

    fn add_edge(&mut self, from: usize, to: usize) {
        self.edges.entry(from).or_default().push(to);
    }

    pub fn build_from_expr(
        &mut self,
        expr: TypedExpr<'a>,
        current_block: &mut Vec<TypedExpr<'a>>,
    ) -> usize {
        //'
        match expr {
            TypedExpr::If(condition, if_branch, else_branch, _, _span, _file) => {
                let cond_id = self.add_node(Node::Condition(*condition));
                let mut then_block = Vec::new();
                let then_end = self.build_from_expr(*if_branch, &mut then_block);
                let then_id = self.add_node(Node::BasicBlock(then_block));

                let else_branch = match else_branch {
                    Some(expr) => expr,
                    None => return cond_id,
                };

                let mut else_block = Vec::new();
                let else_end = self.build_from_expr(*else_branch, &mut else_block);
                let else_id = self.add_node(Node::BasicBlock(else_block));

                self.add_edge(cond_id, then_id);
                self.add_edge(cond_id, else_id);

                // Create merge point after if-else
                // In a real implementation, you might need more sophisticated merging
                // especially if there are returns/breaks inside the branches.
                let merge_id = self.add_node(Node::BasicBlock(Vec::new()));
                self.add_edge(then_end, merge_id);
                self.add_edge(else_end, merge_id);
                merge_id
            }
            TypedExpr::While(condition, body, _, _span, _file) => {
                let cond_id = self.add_node(Node::Condition(*condition));

                let mut body_block = Vec::new();
                let body_end = self.build_from_expr(*body, &mut body_block);
                let body_id = self.add_node(Node::BasicBlock(body_block));

                self.add_edge(cond_id, body_id); // True branch (enter loop)
                self.add_edge(body_end, cond_id); // Loop back to condition

                // Exit point for the loop:
                let merge_id = self.add_node(Node::BasicBlock(Vec::new()));
                self.add_edge(cond_id, merge_id); // False branch (exit loop)

                merge_id // Return the merge node id
            }
            TypedExpr::BinaryOp(ref lhs, op, ref rhs, ref type_, span, ref file) => {
                let _lhs_id = self.build_from_expr(*lhs.clone(), current_block);
                let _rhs_id = self.build_from_expr(*rhs.clone(), current_block);

                current_block.push(TypedExpr::BinaryOp(
                    Box::new(expr.clone()),
                    op,
                    Box::new(expr.clone()),
                    type_.clone(),
                    span,
                    file.clone(),
                ));

                let block_id = self.add_node(Node::BasicBlock(current_block.clone())); // Create the basic block

                block_id // Return the current block id
            }

            other => {
                current_block.push(other.clone());
                //Fix for subtraction overflow:
                if self.nodes.is_empty() {
                    0
                } else {
                    self.nodes.len() - 1
                }
            }
        }
    }
}
