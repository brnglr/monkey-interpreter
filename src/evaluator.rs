use crate::ast::ASTNode;
use crate::object::{Environment, Object};
use std::cell::RefCell;
use std::rc::Rc;

pub fn eval<T: ASTNode>(node: T, environment: &Rc<RefCell<Environment>>) -> Object {
    return node.evaluate(environment);
}

#[cfg(test)]
mod tests {
    use crate::evaluator::eval;
    use crate::lexer::Lexer;
    use crate::object::{
        Array, Environment, Error, Hashable, Integer, MonkeyHashKey, Object, String, FALSE, NULL,
        TRUE,
    };
    use crate::parser::Parser;
    use std::collections::HashMap;

    // =========================================================
    // Helper functions for testing
    // =========================================================

    fn evaluate_input(input: &std::string::String) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let env = Environment::new();

        return eval(program, &env);
    }

    fn build_integer_object(value: i64) -> Object {
        return Object::Integer(Integer { value: value });
    }

    fn build_string_object(value: &str) -> Object {
        return Object::String(String {
            value: value.to_string(),
        });
    }

    // =========================================================
    // Tests
    // =========================================================

    #[test]
    fn test_eval_integer_expression() {
        struct TestData {
            input: std::string::String,
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
            input: std::string::String,
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
            input: std::string::String,
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
            input: std::string::String,
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
            input: std::string::String,
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
            input: std::string::String,
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
            input: std::string::String,
            expected_parameters: Vec<std::string::String>,
            expected_body: std::string::String,
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
            input: std::string::String,
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
    fn test_recursive_function() {
        let input =
            "let counter = fn(x) {if (x > 5) {return true;} else {return counter(x+1)}};counter(0)"
                .to_string();
        let evaluated = evaluate_input(&input);
        assert_eq!(evaluated, TRUE);
    }

    #[test]
    fn test_string_literal() {
        let input = "\"Hello World!\"".to_string();
        let evaluated = evaluate_input(&input);
        assert_eq!(evaluated, build_string_object("Hello World!"));
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World!\"".to_string();
        let evaluated = evaluate_input(&input);
        assert_eq!(evaluated, build_string_object("Hello World!"));
    }

    #[test]
    fn test_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]".to_string();
        let evaluated = evaluate_input(&input);

        assert_eq!(
            evaluated,
            Object::Array(Array {
                elements: vec![
                    build_integer_object(1),
                    build_integer_object(4),
                    build_integer_object(6),
                ],
            })
        );
    }

    #[test]
    fn test_array_index_expressions() {
        struct TestData {
            input: std::string::String,
            expected_output: Object,
        }
        let tests = vec![
            TestData {
                input: "[1, 2, 3][0]".to_string(),
                expected_output: build_integer_object(1),
            },
            TestData {
                input: "[1, 2, 3][1]".to_string(),
                expected_output: build_integer_object(2),
            },
            TestData {
                input: "[1, 2, 3][2]".to_string(),
                expected_output: build_integer_object(3),
            },
            TestData {
                input: "let i = 0; [1][i];".to_string(),
                expected_output: build_integer_object(1),
            },
            TestData {
                input: "[1, 2, 3][1 + 1];".to_string(),
                expected_output: build_integer_object(3),
            },
            TestData {
                input: "let myArray = [1, 2, 3]; myArray[2];".to_string(),
                expected_output: build_integer_object(3),
            },
            TestData {
                input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];".to_string(),
                expected_output: build_integer_object(6),
            },
            TestData {
                input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]".to_string(),
                expected_output: build_integer_object(2),
            },
            TestData {
                input: "[1, 2, 3][3]".to_string(),
                expected_output: NULL,
            },
            TestData {
                input: "[1, 2, 3][-1]".to_string(),
                expected_output: NULL,
            },
        ];

        for test in tests.iter() {
            let evaluated = evaluate_input(&test.input);
            assert_eq!(evaluated, test.expected_output);
        }
    }

    #[test]
    fn test_builtin_functions() {
        struct TestData {
            input: std::string::String,
            expected_output: Object,
        }
        let tests = vec![
            TestData {
                input: "len(\"\")".to_string(),
                expected_output: build_integer_object(0),
            },
            TestData {
                input: "len(\"four\")".to_string(),
                expected_output: build_integer_object(4),
            },
            TestData {
                input: "len(\"hello world\")".to_string(),
                expected_output: build_integer_object(11),
            },
            TestData {
                input: "len([1,2,3])".to_string(),
                expected_output: build_integer_object(3),
            },
            TestData {
                input: "len([1,2,3,4,5,6])".to_string(),
                expected_output: build_integer_object(6),
            },
            TestData {
                input: "len([])".to_string(),
                expected_output: build_integer_object(0),
            },
            TestData {
                input: "first([])".to_string(),
                expected_output: NULL,
            },
            TestData {
                input: "first([1,2,3,4])".to_string(),
                expected_output: build_integer_object(1),
            },
            TestData {
                input: "last([1,2,3,4])".to_string(),
                expected_output: build_integer_object(4),
            },
            TestData {
                input: "rest([1,2,3,4])".to_string(),
                expected_output: Object::Array(Array {
                    elements: vec![
                        build_integer_object(2),
                        build_integer_object(3),
                        build_integer_object(4),
                    ],
                }),
            },
            TestData {
                input: "rest(rest([1,2,3,4]))".to_string(),
                expected_output: Object::Array(Array {
                    elements: vec![build_integer_object(3), build_integer_object(4)],
                }),
            },
            TestData {
                input: "rest(rest(rest([1,2,3,4])))".to_string(),
                expected_output: Object::Array(Array {
                    elements: vec![build_integer_object(4)],
                }),
            },
            TestData {
                input: "rest(rest(rest(rest([1,2,3,4]))))".to_string(),
                expected_output: Object::Array(Array { elements: vec![] }),
            },
            TestData {
                input: "rest(rest(rest(rest(rest([1,2,3,4])))))".to_string(),
                expected_output: NULL,
            },
            TestData {
                input: "push([1,2,3,4], 5)".to_string(),
                expected_output: Object::Array(Array {
                    elements: vec![
                        build_integer_object(1),
                        build_integer_object(2),
                        build_integer_object(3),
                        build_integer_object(4),
                        build_integer_object(5),
                    ],
                }),
            },
            TestData {
                input: "push([], 5)".to_string(),
                expected_output: Object::Array(Array {
                    elements: vec![build_integer_object(5)],
                }),
            },
            TestData {
                input: "len(1)".to_string(),
                expected_output: Object::Error(Error {
                    message: "argument to `len` not supported, got INTEGER".to_string(),
                }),
            },
            TestData {
                input: "len(\"one\", \"two\")".to_string(),
                expected_output: Object::Error(Error {
                    message: "wrong number of arguments. got=2, want=1".to_string(),
                }),
            },
        ];

        for test in tests.iter() {
            let evaluated = evaluate_input(&test.input);
            assert_eq!(evaluated, test.expected_output);
        }
    }

    #[test]
    fn test_hash_literal() {
        let input = "\
            let two = \"two\";
            {
                \"one\": 10 - 9,
                two: 1 + 1,
                \"thr\" + \"ee\": 6 / 2,
                4: 4,
                true: 5,
                false: 6,
            }"
        .to_string();

        let evaluated = evaluate_input(&input);
        let expected = {
            let mut map: HashMap<MonkeyHashKey, i64> = HashMap::new();
            map.insert(
                Object::String(String {
                    value: "one".to_string(),
                })
                .hash_key()
                .expect("String should implement hash_key()"),
                1,
            );
            map.insert(
                Object::String(String {
                    value: "two".to_string(),
                })
                .hash_key()
                .expect("String should implement hash_key()"),
                2,
            );
            map.insert(
                Object::String(String {
                    value: "three".to_string(),
                })
                .hash_key()
                .expect("String should implement hash_key()"),
                3,
            );
            map.insert(
                Object::Integer(Integer { value: 4 })
                    .hash_key()
                    .expect("Integer should implement hash_key()"),
                4,
            );
            map.insert(
                TRUE.hash_key()
                    .expect("Boolean should implement hash_key()"),
                5,
            );
            map.insert(
                FALSE
                    .hash_key()
                    .expect("Boolean should implement hash_key()"),
                6,
            );
            map
        };

        if let Object::MonkeyHashMap(evaluated_map) = evaluated {
            assert!(evaluated_map.pairs.len() == expected.len());
            for (expected_key, expected_value) in expected.iter() {
                assert!(evaluated_map.pairs.contains_key(expected_key));
                let pair = evaluated_map.pairs.get(expected_key);
                if let Some(pair) = pair {
                    if let Object::Integer(val) = &pair.value {
                        assert_eq!(val.value, *expected_value);
                    } else {
                        panic!("Value is not an integer like expected: {}", pair.value);
                    }
                }
            }
        }
    }

    #[test]
    fn test_hash_index_expressions() {
        struct TestData {
            input: std::string::String,
            expected: Object,
        }
        let tests = vec![
            TestData {
                input: "{\"foo\": 5}[\"foo\"]".to_string(),
                expected: build_integer_object(5),
            },
            TestData {
                input: "{\"foo\": 5}[\"bar\"]".to_string(),
                expected: NULL,
            },
            TestData {
                input: "let key = \"foo\"; {\"foo\": 5}[key];".to_string(),
                expected: build_integer_object(5),
            },
            TestData {
                input: "{}[\"foo\"]".to_string(),
                expected: NULL,
            },
            TestData {
                input: "{5: 5}[5]".to_string(),
                expected: build_integer_object(5),
            },
            TestData {
                input: "{true: 5}[true]".to_string(),
                expected: build_integer_object(5),
            },
            TestData {
                input: "{false: 5}[false]".to_string(),
                expected: build_integer_object(5),
            },
        ];

        for test in tests.iter() {
            println!("{}", test.input);
            let evaluated = evaluate_input(&test.input);
            assert_eq!(evaluated, test.expected);
        }
    }

    #[test]
    fn test_error_handling() {
        struct TestData {
            input: std::string::String,
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
            TestData {
                input: "\"Hello\" - \"World\"".to_string(),
                expected_error: Object::Error(Error {
                    message: "unknown operator: STRING - STRING".to_string(),
                }),
            },
            TestData {
                input: "{\"name\": \"Monkey\"}[fn(x) { x }];".to_string(),
                expected_error: Object::Error(Error {
                    message: "unusable as hash key: FUNCTION".to_string(),
                }),
            },
        ];

        for test in tests.iter() {
            let evaluated = evaluate_input(&test.input);
            assert_eq!(evaluated, test.expected_error);
        }
    }
}
