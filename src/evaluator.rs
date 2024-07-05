use crate::ast::ASTNode;
use crate::object::Object;

fn eval<T: ASTNode>(node: T) -> Object {
    return node.evaluate();
}

#[cfg(test)]
mod tests {
    use crate::evaluator::eval;
    use crate::lexer::Lexer;
    use crate::object::{Integer, Object};
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
            expected_evaluation: Object,
        }
        let tests = vec![
            TestData {
                input: "5;".to_string(),
                expected_evaluation: build_integer_object(5),
            },
            TestData {
                input: "10".to_string(),
                expected_evaluation: build_integer_object(10),
            },
        ];

        for test in tests.iter() {
            let evaluated = evaluate_input(&test.input);
            assert_eq!(evaluated, test.expected_evaluation);
        }
    }
}
