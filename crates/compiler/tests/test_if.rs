mod utils;
use utils::*;

// ── Control flow: if/else ────────────────────────────────────────

#[test]
fn test_if_true_branch() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 1
if x == 1 {
    print_int(100)
} else {
    print_int(200)
}
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![100]),
    });
}

#[test]
fn test_if_false_branch() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 0
if x == 1 {
    print_int(100)
} else {
    print_int(200)
}
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![200]),
    });
}

#[test]
fn test_if_no_else() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 5
if x > 0 {
    print_int(x)
}
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![5]),
    });
}

#[test]
fn test_if_no_else_not_taken() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = -1
if x > 0 {
    print_int(x)
}
print_int(0)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_if_assigns_variable() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
result = 0
if x > 0 {
    result = 1
} else {
    result = -1
}
print_int(result)
}",
        inputs: VecDeque::from(vec![42]),
        expected_outputs: VecDeque::from(vec![1]),
    });
}

#[test]
fn test_if_assigns_variable_negative_input() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
result = 0
if x > 0 {
    result = 1
} else {
    result = -1
}
print_int(result)
}",
        inputs: VecDeque::from(vec![-5]),
        expected_outputs: VecDeque::from(vec![-1]),
    });
}

#[test]
fn test_nested_if() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
if x > 0 {
    if x > 10 {
        print_int(2)
    } else {
        print_int(1)
    }
} else {
    print_int(0)
}
}",
        inputs: VecDeque::from(vec![5]),
        expected_outputs: VecDeque::from(vec![1]),
    });
}

#[test]
fn test_nested_if_deep() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
if x > 0 {
    if x > 10 {
        print_int(2)
    } else {
        print_int(1)
    }
} else {
    print_int(0)
}
}",
        inputs: VecDeque::from(vec![50]),
        expected_outputs: VecDeque::from(vec![2]),
    });
}

#[test]
fn test_if_comparison_operators() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = 5
b = 10
if a < b { print_int(1) } else { print_int(0) }
if a > b { print_int(1) } else { print_int(0) }
if a == b { print_int(1) } else { print_int(0) }
if a != b { print_int(1) } else { print_int(0) }
if a <= b { print_int(1) } else { print_int(0) }
if a >= b { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        // a<b T, a>b F, a==b F, a!=b T, a<=b T, a>=b F
        expected_outputs: VecDeque::from(vec![1, 0, 0, 1, 1, 0]),
    });
}

#[test]
fn test_if_equal_values() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = 7
b = 7
if a == b { print_int(1) } else { print_int(0) }
if a != b { print_int(1) } else { print_int(0) }
if a <= b { print_int(1) } else { print_int(0) }
if a >= b { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        // ==T, !=F, <=T, >=T
        expected_outputs: VecDeque::from(vec![1, 0, 1, 1]),
    });
}

// ── Ternary expressions ──────────────────────────────────────────

#[test]
fn test_ternary_true() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 1
y = x > 0 ? 100 : 200
print_int(y)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![100]),
    });
}

#[test]
fn test_ternary_false() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = -1
y = x > 0 ? 100 : 200
print_int(y)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![200]),
    });
}

#[test]
fn test_ternary_nested() {
    // Right-associative: x > 0 ? 1 : (x == 0 ? 0 : -1)
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
y = x > 0 ? 1 : x == 0 ? 0 : -1
print_int(y)
}",
        inputs: VecDeque::from(vec![0]),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_ternary_with_variables() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = 10
b = 20
max = a > b ? a : b
print_int(max)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![20]),
    });
}

// ── Boolean variables and logic ──────────────────────────────────

#[test]
fn test_boolean_true() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = true
if x { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1]),
    });
}

#[test]
fn test_boolean_false() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = false
if x { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_boolean_not() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = true
if !x { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_boolean_and() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = true
b = false
if a && b { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_boolean_or() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = true
b = false
if a || b { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1]),
    });
}

#[test]
fn test_boolean_and_both_true() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = true
b = true
if a && b { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1]),
    });
}

#[test]
fn test_boolean_or_both_false() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = false
b = false
if a || b { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_comparison_result_in_variable() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 5
y = 10
cmp = x < y
if cmp { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1]),
    });
}

// ── Short-circuit evaluation ─────────────────────────────────────

#[test]
fn test_and_short_circuit() {
    // No inputs provided; test fails if RHS of && is evaluated
    execute_test_case(TestCase {
        input: "fn main() -> int { if false && (read_int() == 1) {
    print_int(1)
} else {
    print_int(0)
}
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_or_short_circuit() {
    // No inputs provided; test fails if RHS of || is evaluated
    execute_test_case(TestCase {
        input: "fn main() -> int { if true || (read_int() == 1) {
    print_int(1)
} else {
    print_int(0)
}
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1]),
    });
}

// ── Interaction: conditionals + variables ─────────────────────────

#[test]
fn test_variable_not_modified_by_other_branch() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 42
y = 0
if y > 0 {
    x = 99
}
print_int(x)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

#[test]
fn test_complex_condition_expression() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
if (x + 1) > 5 {
    print_int(1)
} else {
    print_int(0)
}
}",
        inputs: VecDeque::from(vec![4]),
        // 4+1 = 5, 5 > 5 is false
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_complex_condition_expression_true() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
if (x + 1) > 5 {
    print_int(1)
} else {
    print_int(0)
}
}",
        inputs: VecDeque::from(vec![5]),
        // 5+1 = 6, 6 > 5 is true
        expected_outputs: VecDeque::from(vec![1]),
    });
}
