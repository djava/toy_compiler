mod utils;
use utils::*;

// ── Complex expressions ──────────────────────────────────────────

#[test]
fn test_nested_arithmetic() {
    execute_test_case(TestCase {
        input: "\
x = read_int()
y = read_int()
print_int((x + y) + (x - y))",
        inputs: VecDeque::from(vec![10, 3]),
        // (10+3) + (10-3) = 13 + 7 = 20
        expected_outputs: VecDeque::from(vec![20]),
    });
}

#[test]
fn test_deeply_nested_expression() {
    execute_test_case(TestCase {
        input: "print_int(1 + (2 + (3 + (4 + (5 + 6)))))",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![21]),
    });
}

#[test]
fn test_expression_with_multiple_reads() {
    execute_test_case(TestCase {
        input: "print_int(read_int() + read_int() + read_int())",
        inputs: VecDeque::from(vec![11, 22, 33]),
        expected_outputs: VecDeque::from(vec![66]),
    });
}
