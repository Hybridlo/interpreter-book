use crate::ast::expressions::{IdentifierExpression, IfExpression, InfixOperator, PrefixOperator};
use crate::ast::statements::{BlockStatement, Statement};
use crate::ast::Program;
use crate::ast::{expressions::Expression, Node};
use crate::builtins::BUILTINS;
use crate::object::environment::Environment;
use crate::object::Object;

pub fn eval(node: Node, env: &mut Environment) -> Object {
    match node {
        Node::Statement(stmt) => eval_statement(stmt, env),
        Node::Expression(expr) => eval_expression(expr, env),
        Node::Program(prog) => eval_program(prog, env),
    }
}

pub fn eval_program(prog: Program, env: &mut Environment) -> Object {
    let mut res = Object::Null;

    for statement in prog.statements {
        res = eval_statement(statement, env);

        if let Object::Return(ret) = res {
            return *ret;
        }
        if let Object::Error(err) = res {
            return Object::Error(err);
        }
    }

    res
}

pub fn eval_block_statement(block: BlockStatement, env: &mut Environment) -> Object {
    let mut res = Object::Null;

    for statement in block.0 {
        res = eval_statement(statement, env);

        if let Object::Return(_) = res {
            return res;
        }
        if let Object::Error(_) = res {
            return res;
        }
    }

    res
}

pub fn eval_statement(stmt: Statement, env: &mut Environment) -> Object {
    match stmt {
        Statement::Let(let_stmt) => {
            let val = eval(let_stmt.value.into(), env);
            if let Object::Error(_) = val {
                return val;
            }

            env.set(let_stmt.name.0, val)
        }
        Statement::Return(ret) => {
            let val = eval(ret.0.into(), env);
            if let Object::Error(err) = val {
                return Object::Error(err);
            }

            Object::Return(Box::new(val))
        }
        Statement::Expression(expr) => eval_expression(expr.0, env),
        Statement::Block(block) => eval_block_statement(block, env),
    }
}

pub fn eval_expression(expr: Expression, env: &mut Environment) -> Object {
    match expr {
        Expression::Identifier(ident) => {
            if let Some(obj) = env.get(&ident.0) {
                obj
            } else if let Some(builtin) = BUILTINS.get(&ident.0) {
                Object::BuiltinFunction(builtin)
            } else {
                Object::Error(format!("identifier not found: {}", ident.0))
            }
        }
        Expression::IntegerLiteral(int_literal) => Object::Integer(int_literal.0),
        Expression::Prefix(prefix_expr) => {
            let right = eval((*prefix_expr.right).into(), env);
            if let Object::Error(_) = right {
                return right;
            }

            eval_prefix_expression(prefix_expr.operator, right)
        }
        Expression::Infix(infix_expr) => {
            let left = eval((*infix_expr.left).into(), env);
            if let Object::Error(_) = left {
                return left;
            }

            let right = eval((*infix_expr.right).into(), env);
            if let Object::Error(_) = right {
                return right;
            }

            eval_infix_expression(left, infix_expr.operator, right)
        }
        // because we're cool like that - creating objects is cheap, unlike how they are in Go, so we don't
        // need to optimize booleans values in singletons
        // It's also tedious to change the return value to a reference to an `Object`
        Expression::Boolean(bool_literal) => Object::Boolean(bool_literal.0),
        Expression::If(if_expr) => eval_if_expression(if_expr, env),
        Expression::Function(func_expr) => Object::Function {
            parameters: func_expr.parameters,
            body: func_expr.body,
            env: env.clone(),
        },
        Expression::Call(call_expr) => {
            let function = eval(Expression::from(call_expr.function).into(), env);
            if let Object::Error(_) = function {
                return function;
            }

            let args = eval_expressions(call_expr.arguments, env);
            if let Some(Object::Error(err)) = args.first() {
                return Object::Error(err.clone());
            }

            apply_function(function, args)
        }
        Expression::StringLiteral(str_lit) => Object::String(str_lit.0),
    }
}

pub fn eval_prefix_expression(operator: PrefixOperator, right: Object) -> Object {
    match operator {
        PrefixOperator::Minus => eval_minus_prefix_operator_expression(right),
        PrefixOperator::Not => eval_not_operator_expression(right),
    }
}

pub fn eval_not_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(val) => Object::Boolean(!val),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

pub fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    let Object::Integer(int_val) = right else {
        return Object::Error(format!("unknown operator: -{}", right.obj_type()));
    };

    Object::Integer(-int_val)
}

pub fn eval_infix_expression(left: Object, operator: InfixOperator, right: Object) -> Object {
    match (left, operator, right) {
        (Object::Integer(left_int), operator, Object::Integer(right_int)) => {
            eval_integer_infix_expression(left_int, operator, right_int)
        }
        (Object::String(left_str), operator, Object::String(right_str)) => {
            eval_string_infix_expression(left_str, operator, right_str)
        }
        (left, operator, right) if left.obj_type() != right.obj_type() => Object::Error(format!(
            "type mismatch: {} {} {}",
            left.obj_type(),
            operator,
            right.obj_type()
        )),

        // Hand writing these, because it's impossible to have `PartialEq` with `Function` object type anyway
        (Object::Boolean(left_bool), InfixOperator::Equal, Object::Boolean(right_bool)) => {
            Object::Boolean(left_bool == right_bool)
        }
        (Object::Boolean(left_bool), InfixOperator::NotEqual, Object::Boolean(right_bool)) => {
            Object::Boolean(left_bool != right_bool)
        }
        (Object::Null, InfixOperator::Equal, Object::Null) => Object::Boolean(true),
        (Object::Null, InfixOperator::NotEqual, Object::Null) => Object::Boolean(false),

        // a default path
        (left, operator, right) => Object::Error(format!(
            "unknown operator: {} {} {}",
            left.obj_type(),
            operator,
            right.obj_type()
        )),
    }
}

pub fn eval_integer_infix_expression(left: i64, operator: InfixOperator, right: i64) -> Object {
    match operator {
        InfixOperator::Plus => Object::Integer(left + right),
        InfixOperator::Minus => Object::Integer(left - right),
        InfixOperator::Multiply => Object::Integer(left * right),
        InfixOperator::Divide => Object::Integer(left / right),
        InfixOperator::Gt => Object::Boolean(left > right),
        InfixOperator::Lt => Object::Boolean(left < right),
        InfixOperator::Equal => Object::Boolean(left == right),
        InfixOperator::NotEqual => Object::Boolean(left != right),
    }
}

pub fn eval_string_infix_expression(
    left: String,
    operator: InfixOperator,
    right: String,
) -> Object {
    match operator {
        InfixOperator::Plus => Object::String(left + &right),
        _ => Object::Error(format!("unknown operator: STRING {} STRING", operator)),
    }
}

pub fn eval_if_expression(if_expr: IfExpression, env: &mut Environment) -> Object {
    let condition = eval((*if_expr.condition).into(), env);
    if let Object::Error(err) = condition {
        return Object::Error(err);
    }

    if is_truthy(condition) {
        // annoying, but can't blanket impl From<_> for Node for inner Expression and Statement types
        eval(Statement::Block(if_expr.consequence).into(), env)
    } else if let Some(alt) = if_expr.alternative {
        eval(Statement::Block(alt).into(), env)
    } else {
        Object::Null
    }
}

pub fn eval_expressions(exprs: Vec<Expression>, env: &mut Environment) -> Vec<Object> {
    let mut res = vec![];

    for expr in exprs {
        let evaluated = eval(expr.into(), env);

        if let Object::Error(err) = evaluated {
            return vec![Object::Error(err)];
        }

        res.push(evaluated);
    }

    res
}

pub fn apply_function(function: Object, args: Vec<Object>) -> Object {
    match function {
        Object::Function {
            parameters,
            body,
            env,
        } => {
            let mut extended_env = extended_function_env(parameters, args, env);
            let evaluated = eval(Statement::Block(body).into(), &mut extended_env);
            unwrap_return_value(evaluated)
        }
        Object::BuiltinFunction(builtin) => (builtin.0)(args),
        _ => Object::Error(format!("not a function: {:?}", function)),
    }
}

pub fn extended_function_env(
    fn_params: Vec<IdentifierExpression>,
    args: Vec<Object>,
    fn_env: Environment,
) -> Environment {
    let env = fn_env.new_enclosed();

    for (param, arg) in fn_params.into_iter().zip(args.into_iter()) {
        env.set(param.0, arg);
    }

    env
}

pub fn unwrap_return_value(obj: Object) -> Object {
    if let Object::Return(ret) = obj {
        return *ret;
    }

    obj
}

pub fn is_truthy(obj: Object) -> bool {
    !matches!(obj, Object::Null | Object::Boolean(false))
}

#[cfg(test)]
mod tests {
    use crate::{
        object::{environment::Environment, Object},
        parser::Parser,
    };

    use super::eval;

    fn test_eval(input: &str) -> Object {
        let parser = Parser::new(input);
        let (program, _) = parser.parse_program();
        let mut env = Environment::new();

        eval(program.into(), &mut env)
    }

    fn assert_integer_object(obj: Object, expected: i64) {
        let Object::Integer(res) = obj else {
            panic!("Expected `Integer` object, {:?} got", obj)
        };

        assert_eq!(res, expected);
    }

    fn assert_boolean_object(obj: Object, expected: bool) {
        let Object::Boolean(res) = obj else {
            panic!("Expected `Boolean` object, {:?} got", obj)
        };

        assert_eq!(res, expected);
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
            assert_integer_object(evaluated, expected_int);
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
            assert_boolean_object(evaluated, expected_bool);
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
            assert_boolean_object(evaluated, expected_bool);
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
            let evaluated = test_eval(input);

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
            let evaluated = test_eval(input);
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
            ("foobar", "identifier not found: foobar"),
            (r#""Hello" - "World""#, "unknown operator: STRING - STRING"),
        ];

        for (input, expected_err) in tests {
            let evaluated = test_eval(input);

            let Object::Error(err) = evaluated else {
                panic!("Expected `Error` object, {:?} got", evaluated);
            };

            assert_eq!(err, expected_err);
        }
    }

    #[test]
    fn test_let_statement() {
        let tests = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected_res) in tests {
            let evaluated = test_eval(input);
            assert_integer_object(evaluated, expected_res)
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; }";

        let evaluated = test_eval(input);

        let Object::Function {
            parameters, body, ..
        } = evaluated
        else {
            panic!("Expected `Function`, {:?} got", evaluated)
        };

        assert_eq!(parameters.len(), 1);
        assert_eq!(parameters[0].0, "x");
        assert_eq!(body.to_string(), "(x + 2)");
    }

    #[test]
    fn test_function_calling() {
        let tests = [
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected_res) in tests {
            let evaluated = test_eval(input);
            assert_integer_object(evaluated, expected_res)
        }
    }

    #[test]
    fn test_closures() {
        let input = r#"
        let newAdder = fn(x) {
            fn(y) { x + y };
        };
        let addTwo = newAdder(2);
        addTwo(2);
        "#;

        assert_integer_object(test_eval(input), 4)
    }

    #[test]
    fn test_string_literal() {
        let input = r#""Hello World!""#;

        let evaluated = test_eval(input);

        let Object::String(str) = evaluated else {
            panic!("Expected a `String` object, {:?} got", evaluated)
        };

        assert_eq!(str, "Hello World!");
    }

    #[test]
    fn test_string_concatenation() {
        let input = r#""Hello" + " " + "World!""#;

        let evaluated = test_eval(input);

        let Object::String(str) = evaluated else {
            panic!("Expected a `String` object, {:?} got", evaluated)
        };

        assert_eq!(str, "Hello World!");
    }

    #[test]
    fn test_builtin_functions() {
        let tests = [
            (r#"len("")"#, Object::Integer(0)),
            (r#"len("four")"#, Object::Integer(4)),
            (r#"len("hello world")"#, Object::Integer(11)),
            (
                r#"len(1)"#,
                Object::Error("argument to `len` not supported, got INTEGER".to_string()),
            ),
            (
                r#"len("one", "two")"#,
                Object::Error("wrong number of arguments. got=2, want=1".to_string()),
            ),
        ];

        for (input, expected_res) in tests {
            let evaluated = test_eval(input);
            if let Object::Integer(expected_val) = expected_res {
                assert_integer_object(evaluated, expected_val);
            } else if let Object::Error(expected_err_msg) = expected_res {
                let Object::Error(actual_err_msg) = evaluated else {
                    panic!("Expected `Error`, {:?} got", evaluated)
                };

                assert_eq!(actual_err_msg, expected_err_msg);
            }
        }
    }
}
