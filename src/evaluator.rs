use crate::ast::ASTNode;
use crate::object::Object;

pub fn eval<T: ASTNode>(node: T) -> Object {
    return node.evaluate();
}

#[cfg(test)]
mod tests {
    use crate::evaluator::eval;
    use crate::lexer::Lexer;
    use crate::object::{Integer, Object, FALSE, NULL, TRUE};
    use crate::parser::Parser;

    // =========================================================
    // Helper functions for testing
    // =========================================================

    fn evaluate_input(input: &String) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        return eval(program);
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
        ];

        for test in tests.iter() {
            let evaluated = evaluate_input(&test.input);
            assert_eq!(evaluated, test.expected_eval);
        }
    }
}
