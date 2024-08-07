use crate::ast::ASTNode;
use crate::object::{Environment, Object};

pub fn eval<T: ASTNode>(node: T, environment: &mut Environment) -> Object {
    return node.evaluate(environment);
}

#[cfg(test)]
mod tests {
    use crate::evaluator::eval;
    use crate::lexer::Lexer;
    use crate::object::{Environment, Error, Integer, Object, FALSE, NULL, TRUE};
    use crate::parser::Parser;

    // =========================================================
    // Helper functions for testing
    // =========================================================

    fn evaluate_input(input: &String) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut env = Environment::new();

        return eval(program, &mut env);
    }

    fn build_integer_object(value: i64) -> Object {
        return Object::Integer(Integer { value: value });
    }

    // =========================================================
    // Tests
    // =========================================================

    #[test]
    fn test_eval_integer_expression() {
        struct TestData {
            input: String,
            expected_eval: Object,
        }
        let tests = vec![
            TestData {
                input: "5;".to_string(),
                expected_eval: build_integer_object(5),
            },
            TestData {
                input: "10".to_string(),
                expected_eval: build_integer_object(10),
            },
            TestData {
                input: "-5".to_string(),
                expected_eval: build_integer_object(-5),
            },
            TestData {
                input: "-10".to_string(),
                expected_eval: build_integer_object(-10),
            },
            TestData {
                input: "5 + 5 + 5 + 5 - 10".to_string(),
                expected_eval: build_integer_object(10),
            },
            TestData {
                input: "2 * 2 * 2 * 2 * 2".to_string(),
                expected_eval: build_integer_object(32),
            },
            TestData {
                input: "-50 + 100 + -50".to_string(),
                expected_eval: build_integer_object(0),
            },
            TestData {
                input: "5 * 2 + 10".to_string(),
                expected_eval: build_integer_object(20),
            },
            TestData {
                input: "5 + 2 * 10".to_string(),
                expected_eval: build_integer_object(25),
            },
            TestData {
                input: "20 + 2 * -10".to_string(),
                expected_eval: build_integer_object(0),
            },
            TestData {
                input: "50 / 2 * 2 + 10".to_string(),
                expected_eval: build_integer_object(60),
            },
            TestData {
                input: "2 * (5 + 10)".to_string(),
                expected_eval: build_integer_object(30),
            },
            TestData {
                input: "3 * 3 * 3 + 10".to_string(),
                expected_eval: build_integer_object(37),
            },
            TestData {
                input: "3 * (3 * 3) + 10".to_string(),
                expected_eval: build_integer_object(37),
            },
            TestData {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(),
                expected_eval: build_integer_object(50),
            },
        ];

        for test in tests.iter() {
            let evaluated = evaluate_input(&test.input);
            assert_eq!(evaluated, test.expected_eval);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        struct TestData {
            input: String,
            expected_eval: Object,
        }
        let tests = vec![
            TestData {
                input: "true;".to_string(),
                expected_eval: TRUE,
            },
            TestData {
                input: "false".to_string(),
                expected_eval: FALSE,
            },
            TestData {
                input: "1 < 2".to_string(),
                expected_eval: TRUE,
            },
            TestData {
                input: "1 > 2".to_string(),
                expected_eval: FALSE,
            },
            TestData {
                input: "1 < 1".to_string(),
                expected_eval: FALSE,
            },
            TestData {
                input: "1 > 1".to_string(),
                expected_eval: FALSE,
            },
            TestData {
                input: "1 == 1".to_string(),
                expected_eval: TRUE,
            },
            TestData {
                input: "1 != 1".to_string(),
                expected_eval: FALSE,
            },
            TestData {
                input: "1 == 2".to_string(),
                expected_eval: FALSE,
            },
            TestData {
                input: "1 != 2".to_string(),
                expected_eval: TRUE,
            },
            TestData {
                input: "true == true".to_string(),
                expected_eval: TRUE,
            },
            TestData {
                input: "false == false".to_string(),
                expected_eval: TRUE,
            },
            TestData {
                input: "true == false".to_string(),
                expected_eval: FALSE,
            },
            TestData {
                input: "true != false".to_string(),
                expected_eval: TRUE,
            },
            TestData {
                input: "false != true".to_string(),
                expected_eval: TRUE,
            },
            TestData {
                input: "(1 < 2) == true".to_string(),
                expected_eval: TRUE,
            },
            TestData {
                input: "(1 < 2) == false".to_string(),
                expected_eval: FALSE,
            },
            TestData {
                input: "(1 > 2) == true".to_string(),
                expected_eval: FALSE,
            },
            TestData {
                input: "(1 > 2) == false".to_string(),
                expected_eval: TRUE,
            },
        ];

        for test in tests.iter() {
            let evaluated = evaluate_input(&test.input);
            assert_eq!(evaluated, test.expected_eval);
        }
    }

    #[test]
    fn test_bang_operator() {
        struct TestData {
            input: String,
            expected_eval: Object,
        }
        let tests = vec![
            TestData {
                input: "!true".to_string(),
                expected_eval: FALSE,
            },
            TestData {
                input: "!false".to_string(),
                expected_eval: TRUE,
            },
            TestData {
                input: "!5".to_string(),
                expected_eval: FALSE,
            },
            TestData {
                input: "!!true".to_string(),
                expected_eval: TRUE,
            },
            TestData {
                input: "!!false".to_string(),
                expected_eval: FALSE,
            },
            TestData {
                input: "!!5".to_string(),
                expected_eval: TRUE,
            },
            TestData {
                input: "!0".to_string(),
                expected_eval: TRUE,
            },
            TestData {
                input: "!!0".to_string(),
                expected_eval: FALSE,
            },
            TestData {
                input: "!-5".to_string(),
                expected_eval: FALSE,
            },
        ];

        for test in tests.iter() {
            let evaluated = evaluate_input(&test.input);
            assert_eq!(evaluated, test.expected_eval);
        }
    }

    #[test]
    fn test_if_else_expression() {
        struct TestData {
            input: String,
            expected_eval: Object,
        }
        let tests = vec![
            TestData {
                input: "if (true) { 10 }".to_string(),
                expected_eval: build_integer_object(10),
            },
            TestData {
                input: "if (false) { 10 }".to_string(),
                expected_eval: NULL,
            },
            TestData {
                input: "if (1) { 10 }".to_string(),
                expected_eval: build_integer_object(10),
            },
            TestData {
                input: "if (1 < 2) { 10 }".to_string(),
                expected_eval: build_integer_object(10),
            },
            TestData {
                input: "if (1 > 2) { 10 }".to_string(),
                expected_eval: NULL,
            },
            TestData {
                input: "if (1 > 2) { 10 } else { 20 }".to_string(),
                expected_eval: build_integer_object(20),
            },
            TestData {
                input: "if (1 < 2) { 10 } else { 20 }".to_string(),
                expected_eval: build_integer_object(10),
            },
            TestData {
                input: "if (0) { 10 } else { 20 }".to_string(),
                expected_eval: build_integer_object(20),
            },
            TestData {
                input: "if (true) {}".to_string(),
                expected_eval: NULL,
            },
        ];

        for test in tests.iter() {
            let evaluated = evaluate_input(&test.input);
            assert_eq!(evaluated, test.expected_eval);
        }
    }

    #[test]
    fn test_return_statement() {
        struct TestData {
            input: String,
            expected_eval: Object,
        }
        let tests = vec![
            TestData {
                input: "return 10;".to_string(),
                expected_eval: build_integer_object(10),
            },
            TestData {
                input: "return 10; 9;".to_string(),
                expected_eval: build_integer_object(10),
            },
            TestData {
                input: "return 2 * 5; 9;".to_string(),
                expected_eval: build_integer_object(10),
            },
            TestData {
                input: "9; return 2 * 5; 9;".to_string(),
                expected_eval: build_integer_object(10),
            },
            TestData {
                input: "\
                    if (10 > 1) {\
                        if (10 > 1) {\
                            return 10;\
                        }\
                        return 1;\
                    }\
                "
                .to_string(),
                expected_eval: build_integer_object(10),
            },
        ];

        for test in tests.iter() {
            let evaluated = evaluate_input(&test.input);
            assert_eq!(evaluated, test.expected_eval);
        }
    }

    #[test]
    fn test_let_statements() {
        struct TestData {
            input: String,
            expected_eval: Object,
        }
        let tests = vec![
            TestData {
                input: "let a = 5; a;".to_string(),
                expected_eval: build_integer_object(5),
            },
            TestData {
                input: "let a = 5 * 5; a;".to_string(),
                expected_eval: build_integer_object(25),
            },
            TestData {
                input: "let a = 5; let b = a; b;".to_string(),
                expected_eval: build_integer_object(5),
            },
            TestData {
                input: "let a = 5; let b = a; let c = a + b + 5; c;".to_string(),
                expected_eval: build_integer_object(15),
            },
        ];

        for test in tests.iter() {
            let evaluated = evaluate_input(&test.input);
            assert_eq!(evaluated, test.expected_eval);
        }
    }

    #[test]
    fn test_functions() {
        struct TestData {
            input: String,
            expected_parameters: Vec<String>,
            expected_body: String,
        }
        let tests = vec![TestData {
            input: "fn(x) { x + 2; };".to_string(),
            expected_parameters: vec!["x".to_string()],
            expected_body: "(x + 2)".to_string(),
        }];

        for test in tests.iter() {
            let evaluated = evaluate_input(&test.input);
            match evaluated {
                Object::Function(function) => {
                    assert_eq!(function.parameters.len(), test.expected_parameters.len());
                    for (param, expected_param) in function
                        .parameters
                        .iter()
                        .zip(test.expected_parameters.iter())
                    {
                        assert_eq!(&param.value, expected_param);
                    }
                    assert_eq!(format!("{}", function.body), test.expected_body);
                }
                _ => panic!("Expected to evaluate a function!"),
            }
        }
    }

    #[test]
    fn test_function_application() {
        struct TestData {
            input: String,
            expected_eval: Object,
        }
        let tests = vec![
            TestData {
                input: "let identity = fn(x) { x; }; identity(5);".to_string(),
                expected_eval: build_integer_object(5),
            },
            TestData {
                input: "let identity = fn(x) { return x; }; identity(5);".to_string(),
                expected_eval: build_integer_object(5),
            },
            TestData {
                input: "let double = fn(x) { x * 2; }; double(5);".to_string(),
                expected_eval: build_integer_object(10),
            },
            TestData {
                input: "let add = fn(x, y) { x + y; }; add(5, 5);".to_string(),
                expected_eval: build_integer_object(10),
            },
            TestData {
                input: "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));".to_string(),
                expected_eval: build_integer_object(20),
            },
            TestData {
                input: "fn(x) { x; }(5)".to_string(),
                expected_eval: build_integer_object(5),
            },
        ];

        for test in tests.iter() {
            let evaluated = evaluate_input(&test.input);
            assert_eq!(evaluated, test.expected_eval);
        }
    }

    #[test]
    fn test_closures() {
        let input = "\
            let newAdder = fn(x) {\n\
                fn(y) { x + y };\n\
            };\n\
            let addTwo = newAdder(2);\n\
            addTwo(2);"
            .to_string();
        let evaluated = evaluate_input(&input);
        assert_eq!(evaluated, build_integer_object(4));
    }

    #[test]
    fn test_error_handling() {
        struct TestData {
            input: String,
            expected_error: Object,
        }
        let tests = vec![
            TestData {
                input: "5 + true;".to_string(),
                expected_error: Object::Error(Error {
                    message: "type mismatch: INTEGER + BOOLEAN".to_string(),
                }),
            },
            TestData {
                input: "5 + true; 5;".to_string(),
                expected_error: Object::Error(Error {
                    message: "type mismatch: INTEGER + BOOLEAN".to_string(),
                }),
            },
            TestData {
                input: "-true".to_string(),
                expected_error: Object::Error(Error {
                    message: "unknown operator: -BOOLEAN".to_string(),
                }),
            },
            TestData {
                input: "true + false;".to_string(),
                expected_error: Object::Error(Error {
                    message: "unknown operator: BOOLEAN + BOOLEAN".to_string(),
                }),
            },
            TestData {
                input: "5; true + false; 5".to_string(),
                expected_error: Object::Error(Error {
                    message: "unknown operator: BOOLEAN + BOOLEAN".to_string(),
                }),
            },
            TestData {
                input: "if (10 > 1) { true + false; }".to_string(),
                expected_error: Object::Error(Error {
                    message: "unknown operator: BOOLEAN + BOOLEAN".to_string(),
                }),
            },
            TestData {
                input: "\
                    if (10 > 1) {\
                        if (10 > 1) {\
                            return true + false;\
                        }\
                        return 1;\
                    }\
                "
                .to_string(),
                expected_error: Object::Error(Error {
                    message: "unknown operator: BOOLEAN + BOOLEAN".to_string(),
                }),
            },
            TestData {
                input: "\
                    if (10 > 1) {\
                        if (10 > 1) {\
                            true + false;\
                            return 1;
                        }\
                        return 1;\
                    }\
                "
                .to_string(),
                expected_error: Object::Error(Error {
                    message: "unknown operator: BOOLEAN + BOOLEAN".to_string(),
                }),
            },
            TestData {
                input: "if (true + false) {return 1 + true;}".to_string(),
                expected_error: Object::Error(Error {
                    message: "unknown operator: BOOLEAN + BOOLEAN".to_string(),
                }),
            },
            TestData {
                input: "foobar".to_string(),
                expected_error: Object::Error(Error {
                    message: "identifier not found: foobar".to_string(),
                }),
            },
        ];

        for test in tests.iter() {
            let evaluated = evaluate_input(&test.input);
            assert_eq!(evaluated, test.expected_error);
        }
    }
}
