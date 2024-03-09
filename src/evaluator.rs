use crate::ast::expressions::{IfExpression, InfixOperator, PrefixOperator};
use crate::ast::statements::{BlockStatement, Statement};
use crate::ast::Program;
use crate::ast::{expressions::Expression, Node};
use crate::object::{self, Object};

pub fn eval(node: Node) -> Option<Object> {
    match node {
        Node::Statement(stmt) => eval_statement(stmt),
        Node::Expression(expr) => eval_expression(expr),
        Node::Program(prog) => eval_program(prog),
    }
}

pub fn eval_program(prog: Program) -> Option<Object> {
    let mut res = None;

    for statement in prog.statements {
        res = eval_statement(statement);

        if let Some(Object::Return(ret)) = res {
            return Some(*ret.0);
        }
        if let Some(Object::Error(err)) = res {
            return Some(Object::Error(err));
        }
    }

    res
}

pub fn eval_block_statement(block: BlockStatement) -> Option<Object> {
    let mut res = None;

    for statement in block.0 {
        res = eval_statement(statement);

        if let Some(Object::Return(ret)) = res {
            return Some(Object::Return(ret));
        }
        if let Some(Object::Error(err)) = res {
            return Some(Object::Error(err));
        }
    }

    res
}

pub fn eval_statement(stmt: Statement) -> Option<Object> {
    match stmt {
        Statement::Let(_) => todo!(),
        Statement::Return(ret) => {
            let val = eval(ret.0.into())
                .expect("A return statement had an invalid operand");
            if let Object::Error(err) = val {
                return Some(Object::Error(err))
            }

            Some(Object::Return(object::Return(Box::new(val))))
        },
        Statement::Expression(expr) => eval_expression(expr.0),
        Statement::Block(block) => eval_block_statement(block),
    }
}

pub fn eval_expression(expr: Expression) -> Option<Object> {
    Some(match expr {
        Expression::Identifier(_) => todo!(),
        Expression::IntegerLiteral(int_literal) => Object::Integer(object::Integer(int_literal.0)),
        Expression::Prefix(prefix_expr) => {
            let right = eval((*prefix_expr.right).into())
                .expect("A prefix operation had an invalid operand");
            if let Object::Error(err) = right {
                return Some(Object::Error(err))
            }

            eval_prefix_expression(prefix_expr.operator, right)?
        }
        Expression::Infix(infix_expr) => {
            let left = eval((*infix_expr.left).into())
                .expect("An infix operation had an invalid left operand");
            if let Object::Error(err) = left {
                return Some(Object::Error(err))
            }

            let right = eval((*infix_expr.right).into())
                .expect("An infix operation had an invalid right operand");
            if let Object::Error(err) = right {
                return Some(Object::Error(err))
            }

            eval_infix_expression(left, infix_expr.operator, right)?
        }
        // because we're cool like that - creating objects is cheap, unlike how they are in Go, so we don't
        // need to optimize booleans values in singletons
        // It's also tedious to change the return value to a reference to an `Object`
        Expression::Boolean(bool_literal) => Object::Boolean(object::Boolean(bool_literal.0)),
        Expression::If(if_expr) => eval_if_expression(if_expr)?,
        Expression::Function(_) => todo!(),
        Expression::Call(_) => todo!(),
    })
}

pub fn eval_prefix_expression(operator: PrefixOperator, right: Object) -> Option<Object> {
    match operator {
        PrefixOperator::Minus => eval_minus_prefix_operator_expression(right),
        PrefixOperator::Not => eval_not_operator_expression(right),
    }
}

pub fn eval_not_operator_expression(right: Object) -> Option<Object> {
    Some(match right {
        Object::Boolean(object::Boolean(val)) => Object::Boolean(object::Boolean(!val)),
        Object::Null => Object::Boolean(object::Boolean(true)),
        _ => Object::Boolean(object::Boolean(false)),
    })
}

pub fn eval_minus_prefix_operator_expression(right: Object) -> Option<Object> {
    let Object::Integer(object::Integer(int_val)) = right else {
        return Some(Object::Error(object::Error(format!("unknown operator: -{}", right.obj_type()))));
    };

    Some(Object::Integer(object::Integer(-int_val)))
}

pub fn eval_infix_expression(
    left: Object,
    operator: InfixOperator,
    right: Object,
) -> Option<Object> {
    match (left, operator, right) {
        (
            Object::Integer(object::Integer(left_int)),
            operator,
            Object::Integer(object::Integer(right_int)),
        ) => eval_integer_infix_expression(left_int, operator, right_int),
        // We're not using pointers here, but this should work as good (this also works for Integer types
        // too, but we're not gonna "optimize" out two branches in `eval_integer_infix_expression` because of this)
        (left, InfixOperator::Equal, right) => {
            Some(Object::Boolean(object::Boolean(left == right)))
        }
        (left, InfixOperator::NotEqual, right) => {
            Some(Object::Boolean(object::Boolean(left != right)))
        }
        (left, operator, right) if left.obj_type() != right.obj_type() => {
            Some(Object::Error(object::Error(format!(
                "type mismatch: {} {} {}",
                left.obj_type(),
                operator,
                right.obj_type()
            ))))
        }

        // a default path
        (left, operator, right) => Some(Object::Error(object::Error(format!(
            "unknown operator: {} {} {}",
            left.obj_type(),
            operator,
            right.obj_type()
        )))),
    }
}

pub fn eval_integer_infix_expression(
    left: i64,
    operator: InfixOperator,
    right: i64,
) -> Option<Object> {
    Some(match operator {
        InfixOperator::Plus => Object::Integer(object::Integer(left + right)),
        InfixOperator::Minus => Object::Integer(object::Integer(left - right)),
        InfixOperator::Multiply => Object::Integer(object::Integer(left * right)),
        InfixOperator::Divide => Object::Integer(object::Integer(left / right)),
        InfixOperator::Gt => Object::Boolean(object::Boolean(left > right)),
        InfixOperator::Lt => Object::Boolean(object::Boolean(left < right)),
        InfixOperator::Equal => Object::Boolean(object::Boolean(left == right)),
        InfixOperator::NotEqual => Object::Boolean(object::Boolean(left != right)),
    })
}

pub fn eval_if_expression(if_expr: IfExpression) -> Option<Object> {
    let condition = eval((*if_expr.condition).into())?;
    if let Object::Error(err) = condition {
        return Some(Object::Error(err))
    }

    if is_truthy(condition) {
        // annoying, but can't blanket impl From<_> for Node for inner Expression and Statement types
        eval(Statement::Block(if_expr.consequence).into())
    } else {
        if let Some(alt) = if_expr.alternative {
            eval(Statement::Block(alt).into())
        } else {
            Some(Object::Null)
        }
    }
}

pub fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null | Object::Boolean(object::Boolean(false)) => false,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        object::{self, Object},
        parser::Parser,
    };

    use super::eval;

    fn test_eval(input: &str) -> Option<Object> {
        let parser = Parser::new(input);
        let (program, _) = parser.parse_program();

        eval(program.into())
    }

    fn assert_integer_object(obj: Object, expected: i64) {
        let Object::Integer(res) = obj else {
            panic!("Expected `Integer` object, {:?} got", obj)
        };

        assert_eq!(res.0, expected);
    }

    fn assert_boolean_object(obj: Object, expected: bool) {
        let Object::Boolean(res) = obj else {
            panic!("Expected `Boolean` object, {:?} got", obj)
        };

        assert_eq!(res.0, expected);
    }

    fn assert_null_object(obj: Object) {
        let Object::Null = obj else {
            panic!("Expected `Null` object, {:?} got", obj)
        };
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected_int) in tests {
            let evaluated = test_eval(input);
            assert_integer_object(evaluated.unwrap(), expected_int);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected_bool) in tests {
            let evaluated = test_eval(input);
            assert_boolean_object(evaluated.unwrap(), expected_bool);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected_bool) in tests {
            let evaluated = test_eval(input);
            assert_boolean_object(evaluated.unwrap(), expected_bool);
        }
    }

    #[test]
    fn test_if_else_expression() {
        let tests = [
            ("if (true) { 10 }", Some(10)),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ];

        for (input, expected_eval) in tests {
            let evaluated = test_eval(input).unwrap();

            if let Some(expected_int) = expected_eval {
                assert_integer_object(evaluated, expected_int);
            } else {
                assert_null_object(evaluated);
            };
        }
    }

    #[test]
    fn test_return_statement() {
        let tests = [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                r#"
                if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    
                    return 1;
                }
                "#,
                10,
            ),
        ];

        for (input, expected_ret) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_integer_object(evaluated, expected_ret)
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = [
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                r#"
                if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }
                    return 1;
                }
                "#,
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
        ];

        for (input, expected_err) in tests {
            let evaluated = test_eval(input).unwrap();

            let Object::Error(object::Error(err)) = evaluated else {
                panic!("Expected `Error` object, {:?} got", evaluated);
            };

            assert_eq!(err, expected_err);
        }
    }
}
