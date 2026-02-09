mod utils;
use utils::*;

// ── Complex expressions ──────────────────────────────────────────

#[test]
fn test_nested_arithmetic() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
y = read_int()
print_int((x + y) + (x - y))
}",
        inputs: VecDeque::from(vec![10, 3]),
        // (10+3) + (10-3) = 13 + 7 = 20
        expected_outputs: VecDeque::from(vec![20]),
    });
}

#[test]
fn test_deeply_nested_expression() {
    execute_test_case(TestCase {
        input: "fn main() -> int { print_int(1 + (2 + (3 + (4 + (5 + 6))))) }",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![21]),
    });
}

#[test]
fn test_expression_with_multiple_reads() {
    execute_test_case(TestCase {
        input: "fn main() -> int { print_int(read_int() + read_int() + read_int()) }",
        inputs: VecDeque::from(vec![11, 22, 33]),
        expected_outputs: VecDeque::from(vec![66]),
    });
}

// ── Multiplication ──────────────────────────────────────────────

#[test]
fn test_multiply_expr() {
    execute_test_case(TestCase {
        input: "fn main() -> int { print_int((read_int() * read_int()) + (read_int() * read_int())) }",
        inputs: VecDeque::from([10, 20, 30, 40]),
        expected_outputs: VecDeque::from([(10 * 20) + (30 * 40)]),
    });
}

#[test]
fn test_multiply_constants() {
    execute_test_case(TestCase {
        input: "fn main() -> int { print_int(3 * 4) }",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from([12]),
    });
}

#[test]
fn test_multiply_by_zero() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
print_int(x * 0)
}",
        inputs: VecDeque::from([42]),
        expected_outputs: VecDeque::from([0]),
    });
}

#[test]
fn test_multiply_by_one() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
print_int(x * 1)
}",
        inputs: VecDeque::from([99]),
        expected_outputs: VecDeque::from([99]),
    });
}

#[test]
fn test_multiply_negative() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
y = read_int()
print_int(x * y)
}",
        inputs: VecDeque::from([-3, 7]),
        expected_outputs: VecDeque::from([-21]),
    });
}

#[test]
fn test_multiply_both_negative() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
y = read_int()
print_int(x * y)
}",
        inputs: VecDeque::from([-4, -5]),
        expected_outputs: VecDeque::from([20]),
    });
}

#[test]
fn test_multiply_variables() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
y = read_int()
z = x * y
print_int(z)
}",
        inputs: VecDeque::from([6, 7]),
        expected_outputs: VecDeque::from([42]),
    });
}

#[test]
fn test_multiply_self_assign() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
x = x * x
print_int(x)
}",
        inputs: VecDeque::from([5]),
        expected_outputs: VecDeque::from([25]),
    });
}

#[test]
fn test_multiply_chained() {
    execute_test_case(TestCase {
        input: "fn main() -> int { print_int(read_int() * read_int() * read_int()) }",
        inputs: VecDeque::from([2, 3, 4]),
        expected_outputs: VecDeque::from([24]),
    });
}

#[test]
fn test_multiply_mixed_with_add_sub() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = read_int()
b = read_int()
c = read_int()
print_int(a * b + c)
}",
        inputs: VecDeque::from([3, 4, 5]),
        expected_outputs: VecDeque::from([3 * 4 + 5]),
    });
}

#[test]
fn test_multiply_in_conditional() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
if x > 0 { print_int(x * 10) }
else { print_int(x * 20) }
}",
        inputs: VecDeque::from([3]),
        expected_outputs: VecDeque::from([30]),
    });
}

#[test]
fn test_multiply_in_loop() {
    // Compute factorial of 5 iteratively
    execute_test_case(TestCase {
        input: "fn main() -> int { n = read_int()
result = 1
while n > 0 {
    result = result * n
    n = n - 1
}
print_int(result)
}",
        inputs: VecDeque::from([5]),
        expected_outputs: VecDeque::from([120]),
    });
}

#[test]
fn test_multiply_large_values() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
y = read_int()
print_int(x * y)
}",
        inputs: VecDeque::from([100000, 100000]),
        expected_outputs: VecDeque::from([10000000000_i64]),
    });
}

#[test]
fn test_multiply_register_pressure() {
    // Many live variables across a multiply to stress register allocation
    execute_test_case(TestCase {
        input: "fn main() -> int { a = read_int()
b = read_int()
c = read_int()
d = read_int()
e = read_int()
f = read_int()
print_int(a * b)
print_int(c * d)
print_int(e * f)
print_int(a + b + c + d + e + f)
}",
        inputs: VecDeque::from([2, 3, 4, 5, 6, 7]),
        expected_outputs: VecDeque::from([6, 20, 42, 2 + 3 + 4 + 5 + 6 + 7]),
    });
}
