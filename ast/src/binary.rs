use std::fmt;

use crate::{Literal, LocalRw, RValue, RcLocal, Reduce, SideEffects, Traverse};

use super::{Unary, UnaryOperation};

#[derive(Debug, PartialEq, Eq, PartialOrd, Copy, Clone)]
pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Concat,
    Equal,
    NotEqual,
    LessThanOrEqual,
    GreaterThanOrEqual,
    LessThan,
    GreaterThan,
    And,
    Or,
    IDiv,
}

impl BinaryOperation {
    pub fn is_comparator(&self) -> bool {
        matches!(
            self,
            BinaryOperation::Equal
                | BinaryOperation::NotEqual
                | BinaryOperation::LessThanOrEqual
                | BinaryOperation::GreaterThanOrEqual
                | BinaryOperation::LessThan
                | BinaryOperation::GreaterThan
        )
    }
}

impl fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOperation::Add => "+",
                BinaryOperation::Sub => "-",
                BinaryOperation::Mul => "*",
                BinaryOperation::Div => "/",
                BinaryOperation::Mod => "%",
                BinaryOperation::Pow => "^",
                BinaryOperation::Concat => "..",
                BinaryOperation::Equal => "==",
                BinaryOperation::NotEqual => "~=",
                BinaryOperation::LessThanOrEqual => "<=",
                BinaryOperation::GreaterThanOrEqual => ">=",
                BinaryOperation::LessThan => "<",
                BinaryOperation::GreaterThan => ">",
                BinaryOperation::And => "and",
                BinaryOperation::Or => "or",
                BinaryOperation::IDiv => "//",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub left: Box<RValue>,
    pub right: Box<RValue>,
    pub operation: BinaryOperation,
}

impl Traverse for Binary {
    fn rvalues_mut(&mut self) -> Vec<&mut RValue> {
        vec![&mut self.left, &mut self.right]
    }

    fn rvalues(&self) -> Vec<&RValue> {
        vec![&self.left, &self.right]
    }
}

impl SideEffects for Binary {
    fn has_side_effects(&self) -> bool {
        // TODO: do this properly
        match self.operation {
            BinaryOperation::And | BinaryOperation::Or => {
                self.left.has_side_effects() || self.right.has_side_effects()
            }
            _ => true,
        }
    }
}

impl<'a: 'b, 'b> Reduce for Binary {
    fn reduce(self) -> RValue {
        // TODO: true == true, true == false, etc.
        // really anything without side effects should be true if l == r
        match (self.left.reduce(), self.right.reduce(), self.operation) {
            (
                RValue::Unary(Unary {
                    operation: UnaryOperation::Not,
                    value: left,
                }),
                RValue::Unary(Unary {
                    operation: UnaryOperation::Not,
                    value: right,
                }),
                BinaryOperation::And | BinaryOperation::Or,
            ) => Unary {
                value: Box::new(
                    Binary {
                        left,
                        right,
                        operation: if self.operation == BinaryOperation::And {
                            BinaryOperation::Or
                        } else {
                            BinaryOperation::And
                        },
                    }
                    .into(),
                ),
                operation: UnaryOperation::Not,
            }
            .into(),
            (
                RValue::Literal(Literal::Boolean(left)),
                RValue::Literal(Literal::Boolean(right)),
                BinaryOperation::And | BinaryOperation::Or,
            ) => Literal::Boolean(if self.operation == BinaryOperation::And {
                left && right
            } else {
                left || right
            })
            .into(),
            (
                RValue::Literal(Literal::Boolean(left)),
                right,
                BinaryOperation::And | BinaryOperation::Or,
            ) => match self.operation {
                BinaryOperation::And if !left => RValue::Literal(Literal::Boolean(false)),
                BinaryOperation::And => right.reduce(),
                BinaryOperation::Or if left => RValue::Literal(Literal::Boolean(true)),
                BinaryOperation::Or => right.reduce(),
                _ => unreachable!(),
            },
            (left, right, BinaryOperation::And)
                if !left.has_side_effects() && !right.has_side_effects() && left == right =>
            {
                left
            }
            (
                RValue::Binary(Binary {
                    left:
                        box value @ RValue::Unary(Unary {
                            operation: UnaryOperation::Not,
                            ..
                        }),
                    right: box RValue::Literal(Literal::Boolean(true)),
                    operation: BinaryOperation::And,
                }),
                RValue::Literal(Literal::Boolean(false)),
                BinaryOperation::Or,
            ) => value,
            (left, right, BinaryOperation::Or) if left == right => left,
            // TODO: concat numbers
            (
                RValue::Literal(Literal::String(left)),
                RValue::Literal(Literal::String(right)),
                BinaryOperation::Concat,
            ) => RValue::Literal(Literal::String(
                left.into_iter().chain(right.into_iter()).collect(),
            )),
            (left, right, operation) => Self {
                left: Box::new(left),
                right: Box::new(right),
                operation,
            }
            .into(),
        }
    }

    fn reduce_condition(self) -> RValue {
        let (left, right) = if matches!(self.operation, BinaryOperation::And | BinaryOperation::Or)
        {
            (self.left.reduce_condition(), self.right.reduce_condition())
        } else {
            (self.left.reduce(), self.right.reduce())
        };
        match (left, right, self.operation) {
            (
                RValue::Unary(Unary {
                    operation: UnaryOperation::Not,
                    value: left,
                }),
                RValue::Unary(Unary {
                    operation: UnaryOperation::Not,
                    value: right,
                }),
                BinaryOperation::And | BinaryOperation::Or,
            ) => Unary {
                value: Box::new(
                    Binary {
                        left,
                        right,
                        operation: if self.operation == BinaryOperation::And {
                            BinaryOperation::Or
                        } else {
                            BinaryOperation::And
                        },
                    }
                    .into(),
                ),
                operation: UnaryOperation::Not,
            }
            .into(),
            (
                RValue::Literal(Literal::Boolean(left)),
                RValue::Literal(Literal::Boolean(right)),
                BinaryOperation::And | BinaryOperation::Or,
            ) => Literal::Boolean(if self.operation == BinaryOperation::And {
                left && right
            } else {
                left || right
            })
            .into(),
            (
                RValue::Literal(Literal::Boolean(left)),
                right,
                BinaryOperation::And | BinaryOperation::Or,
            ) => match self.operation {
                BinaryOperation::And if !left => RValue::Literal(Literal::Boolean(false)),
                BinaryOperation::And => right.reduce(),
                BinaryOperation::Or if left => RValue::Literal(Literal::Boolean(true)),
                BinaryOperation::Or => right.reduce(),
                _ => unreachable!(),
            },
            (
                left,
                RValue::Literal(Literal::Boolean(right)),
                BinaryOperation::And | BinaryOperation::Or,
            ) => match self.operation {
                BinaryOperation::And if !right => RValue::Literal(Literal::Boolean(false)),
                BinaryOperation::And => left.reduce(),
                BinaryOperation::Or if right => RValue::Literal(Literal::Boolean(true)),
                BinaryOperation::Or => left.reduce(),
                _ => unreachable!(),
            },
            // TODO: concat numbers
            (
                RValue::Literal(Literal::String(left)),
                RValue::Literal(Literal::String(right)),
                BinaryOperation::Concat,
            ) => RValue::Literal(Literal::String(
                left.into_iter().chain(right.into_iter()).collect(),
            )),
            (left, right, operation) => Self {
                left: Box::new(left),
                right: Box::new(right),
                operation,
            }
            .into(),
        }
    }
}

impl Binary {
    pub fn new(left: RValue, right: RValue, operation: BinaryOperation) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
            operation,
        }
    }

    pub fn precedence(&self) -> usize {
        match self.operation {
            BinaryOperation::Pow => 8,
            BinaryOperation::Mul
            | BinaryOperation::Div
            | BinaryOperation::Mod
            | BinaryOperation::IDiv => 6,
            BinaryOperation::Add | BinaryOperation::Sub => 5,
            BinaryOperation::Concat => 4,
            BinaryOperation::LessThan
            | BinaryOperation::GreaterThan
            | BinaryOperation::LessThanOrEqual
            | BinaryOperation::GreaterThanOrEqual
            | BinaryOperation::Equal
            | BinaryOperation::NotEqual => 3,
            BinaryOperation::And => 2,
            BinaryOperation::Or => 1,
        }
    }

    pub fn right_associative(&self) -> bool {
        matches!(
            self.operation,
            BinaryOperation::Pow | BinaryOperation::Concat
        )
    }

    pub fn left_group(&self) -> bool {
        self.precedence() > self.left.precedence()
            || (self.precedence() == self.left.precedence() && self.right_associative())
    }

    pub fn right_group(&self) -> bool {
        self.precedence() > self.right.precedence()
            || (self.precedence() == self.right.precedence() && !self.right_associative())
    }
}

impl LocalRw for Binary {
    fn values_read(&self) -> Vec<&RcLocal> {
        self.left
            .values_read()
            .into_iter()
            .chain(self.right.values_read().into_iter())
            .collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.left
            .values_read_mut()
            .into_iter()
            .chain(self.right.values_read_mut().into_iter())
            .collect()
    }
}

impl fmt::Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let parentheses = |group: bool, rvalue: &RValue| {
            if group {
                format!("({})", rvalue)
            } else {
                format!("{}", rvalue)
            }
        };

        write!(
            f,
            "{} {} {}",
            parentheses(self.left_group(), self.left.as_ref()),
            self.operation,
            parentheses(self.right_group(), self.right.as_ref()),
        )
    }
}
