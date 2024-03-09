use crate::ast::expressions::{PrefixExpression, PrefixOperator};
use crate::ast::statements::Statement;
use crate::ast::{expressions::Expression, Node};
use crate::object::{self, Object};

pub fn eval(node: Node) -> Option<Object> {
    match node {
        Node::Statement(_) => todo!(),
        Node::Expression(expr) => eval_expression(expr),
        Node::Program(prog) => eval_statements(prog.statements),
    }
}

pub fn eval_statements(stmts: Vec<Statement>) -> Option<Object> {
    let mut res = None;

    for statement in stmts {
        res = eval_statement(statement)
    }

    res
}

pub fn eval_statement(stmt: Statement) -> Option<Object> {
    match stmt {
        Statement::Let(_) => todo!(),
        Statement::Return(_) => todo!(),
        Statement::Expression(expr) => eval_expression(expr.0),
        Statement::Block(_) => todo!(),
    }
}

pub fn eval_expression(expr: Expression) -> Option<Object> {
    Some(match expr {
        Expression::Identifier(_) => todo!(),
        Expression::IntegerLiteral(int_literal) => Object::Integer(object::Integer(int_literal.0)),
        Expression::Prefix(prefix_expr) => {
            let right = eval((*prefix_expr.right).into()).expect("A prefix operation had an invalid operand");
            eval_prefix_expression(prefix_expr.operator, right)?
        },
        Expression::Infix(_) => todo!(),
        // because we're cool like that - creating objects is cheap, unlike how they are in Go, so we don't
        // need to optimize booleans values in singletons
        // It's also tedious to change the return value to a reference to an `Object`
        Expression::Boolean(bool_literal) => Object::Boolean(object::Boolean(bool_literal.0)),
        Expression::If(_) => todo!(),
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
        _ => Object::Boolean(object::Boolean(false))
    })
}

pub fn eval_minus_prefix_operator_expression(right: Object) -> Option<Object> {
    let Object::Integer(object::Integer(int_val)) = right else {
        return Some(Object::Null);
    };

    Some(Object::Integer(object::Integer(-int_val)))
}

#[cfg(test)]
mod tests {
    use crate::{object::Object, parser::Parser};

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

    #[test]
    fn test_eval_integer_expression() {
        let tests = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
        ];

        for (input, expected_int) in tests {
            let evaluated = test_eval(input);
            assert_integer_object(evaluated.unwrap(), expected_int);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = [("true", true), ("false", false)];

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
}
