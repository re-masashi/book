use crate::interpreter::cfg::{ControlFlowGraph, Node};
use crate::interpreter::{TypedExpr, Literal};

use std::fs::File;
use std::io::Write;

const TAB_SIZE: i8 = 4;

impl<'a> ControlFlowGraph<'a> {
    /// VERY UNSABLE FOR NOW
    pub fn emit_graphviz(&self, filename: &str) -> Result<(), std::io::Error> {
        let mut file = File::create(filename)?;

        writeln!(file, "digraph CFG {{")?;

        // Emit nodes
        for (i, node) in self.nodes.iter().enumerate() {
            match node {
                Node::Start => writeln!(file, "    n{} [label=\"Start\"];", i)?,
                Node::End => writeln!(file, "    n{} [label=\"End\"];", i)?,
                Node::BasicBlock(exprs) => {
                    write!(file, "    n{} [label=\"Basic Block{}:\\n", i, i)?;
                    for expr in exprs {
                        write!(file, "expr;\\n")?; // Write each expression on a new line
                    }
                    writeln!(file, "\", shape=box];")?; // Close the label string *after* all expressions
                },
                Node::Condition(expr) => writeln!(
                    file,
                    "    n{} [label=\"Condition: expr\", shape=diamond];",
                    i
                )?,
            }
        }

        // Emit edges
        for (&from, to_nodes) in &self.edges {
            for &to in to_nodes {
                writeln!(file, "    n{} -> n{};", from, to)?;
            }
        }

        writeln!(file, "}}")?;

        Ok(())
    }
}


pub fn expr_to_text(expr: &TypedExpr, padding: i8) -> String {
    match expr {
        TypedExpr::Let(name, expr, _) => {
            // let mut exp = expr.clone();
            return format!("{name}={}", expr_to_text(expr, padding))
        }
        TypedExpr::Variable(name, _) => {
            return format!("{name}")
        }
        TypedExpr::Lambda(args, expr, _) => {
            return "".to_string()// format!("{} {}", args.into_iter().map(|x|x.to_string()).collect::<Vec<_>>().join(" ,"), expr_to_text(expr, padding))
        },
        TypedExpr::Literal(lit, _) => {
            match lit.as_ref() {
                Literal::Boolean(b) => return b.to_string(),
                Literal::Int(i) => return i.to_string(),
                Literal::Float(f) => return f.to_string(),
                Literal::String(s) => return s.to_string(),
            }
        } // skip
        TypedExpr::If(cond, if_, else_, _) => {
            if let Some(pat) = else_ {
                return format!(
"if {} then {} else {}",
                    expr_to_text(cond, padding),
                    expr_to_text(if_, padding+1),
                    expr_to_text(pat, padding+1),
                )
            }else{
                return format!(
"if {} then {}",
                    expr_to_text(cond, padding),
                    expr_to_text(if_, padding+1),
                )
            }
        }
        TypedExpr::Call(func, args, _) => {
            return format!(
"{}({})",
            expr_to_text(func, padding),
            args.into_iter().map(|x|expr_to_text(x, padding)).collect::<Vec<_>>().join(" ,"),
            )
        }
        TypedExpr::While(cond, expr, _) => {
            return format!(
"while {} {}",
                expr_to_text(cond, padding),
                expr_to_text(expr, padding+1),
            )
        }
        TypedExpr::BinaryOp(lhs, op, rhs, _) => {
            todo!()
        }
        _=>todo!()
    }
}