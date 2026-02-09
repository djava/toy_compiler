mod utils;
use utils::*;

// ── Basic assignment + print ─────────────────────────────────────

#[test]
fn test_assign_constant_and_print() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 42\nprint_int(x) }",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

#[test]
fn test_assign_zero() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 0\nprint_int(x) }",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_assign_negative() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = -1\nprint_int(x) }",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![-1]),
    });
}

#[test]
fn test_assign_large_positive() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 1000000000\nprint_int(x) }",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1_000_000_000]),
    });
}

#[test]
fn test_assign_large_negative() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = -999999999\nprint_int(x) }",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![-999_999_999]),
    });
}

// ── read_int / print_int ─────────────────────────────────────────

#[test]
fn test_read_and_print() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()\nprint_int(x) }",
        inputs: VecDeque::from(vec![7]),
        expected_outputs: VecDeque::from(vec![7]),
    });
}

#[test]
fn test_read_negative_input() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()\nprint_int(x) }",
        inputs: VecDeque::from(vec![-42]),
        expected_outputs: VecDeque::from(vec![-42]),
    });
}

#[test]
fn test_read_zero_input() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()\nprint_int(x) }",
        inputs: VecDeque::from(vec![0]),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_multiple_reads() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = read_int()
b = read_int()
c = read_int()
print_int(a)
print_int(b)
print_int(c)
}",
        inputs: VecDeque::from(vec![10, 20, 30]),
        expected_outputs: VecDeque::from(vec![10, 20, 30]),
    });
}

#[test]
fn test_print_expression_directly() {
    execute_test_case(TestCase {
        input: "fn main() -> int { print_int(1 + 2) }",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![3]),
    });
}

#[test]
fn test_print_read_directly() {
    execute_test_case(TestCase {
        input: "fn main() -> int { print_int(read_int()) }",
        inputs: VecDeque::from(vec![55]),
        expected_outputs: VecDeque::from(vec![55]),
    });
}

#[test]
fn test_multiple_prints_same_variable() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 5
print_int(x)
print_int(x)
print_int(x)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![5, 5, 5]),
    });
}

// ── Arithmetic on variables ──────────────────────────────────────

#[test]
fn test_add_two_variables() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = 10
b = 20
print_int(a + b)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![30]),
    });
}

#[test]
fn test_subtract_two_variables() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = 50
b = 30
print_int(a - b)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![20]),
    });
}

#[test]
fn test_subtract_to_negative() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = 3
b = 10
print_int(a - b)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![-7]),
    });
}

#[test]
fn test_unary_negate_variable() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 42
print_int(-x)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![-42]),
    });
}

#[test]
fn test_double_negate() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 7
print_int(- -x)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![7]),
    });
}

#[test]
fn test_chained_addition() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = 1
b = 2
c = 3
d = 4
print_int(a + b + c + d)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10]),
    });
}

#[test]
fn test_mixed_add_subtract() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = 10
b = 3
c = 7
print_int(a - b + c)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![14]),
    });
}

#[test]
fn test_assign_from_expression() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = 5
b = 10
c = a + b
print_int(c)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![15]),
    });
}

#[test]
fn test_read_and_compute() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = read_int()
b = read_int()
print_int(a + b)
}",
        inputs: VecDeque::from(vec![100, 200]),
        expected_outputs: VecDeque::from(vec![300]),
    });
}

#[test]
fn test_read_subtract_to_zero() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
print_int(x - x)
}",
        inputs: VecDeque::from(vec![999]),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_unary_plus() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = +5
print_int(x)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![5]),
    });
}

#[test]
fn test_zero_minus_zero() {
    execute_test_case(TestCase {
        input: "fn main() -> int { print_int(0 - 0) }",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_negate_zero() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 0
print_int(-x)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

// ── Reassignment / overwriting ───────────────────────────────────

#[test]
fn test_reassign_variable() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 1
print_int(x)
x = 2
print_int(x)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 2]),
    });
}

#[test]
fn test_reassign_multiple_times() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 10
x = 20
x = 30
x = 40
print_int(x)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![40]),
    });
}

#[test]
fn test_self_increment() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 5
x = x + 1
print_int(x)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![6]),
    });
}

#[test]
fn test_self_decrement() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 10
x = x - 3
print_int(x)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![7]),
    });
}

#[test]
fn test_accumulate() {
    // x = x + x repeatedly: 1 -> 2 -> 4 -> 8
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 1
x = x + x
x = x + x
x = x + x
print_int(x)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![8]),
    });
}

#[test]
fn test_overwrite_with_read() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 100
print_int(x)
x = read_int()
print_int(x)
}",
        inputs: VecDeque::from(vec![200]),
        expected_outputs: VecDeque::from(vec![100, 200]),
    });
}

// ── Cascading dependencies ───────────────────────────────────────

#[test]
fn test_cascading_assignments() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = 1
b = a + 1
c = b + 1
d = c + 1
print_int(d)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![4]),
    });
}

#[test]
fn test_triangle_dependency() {
    // c depends on both a and b
    execute_test_case(TestCase {
        input: "fn main() -> int { a = read_int()
b = read_int()
c = a + b
print_int(a)
print_int(b)
print_int(c)
}",
        inputs: VecDeque::from(vec![3, 7]),
        expected_outputs: VecDeque::from(vec![3, 7, 10]),
    });
}

#[test]
fn test_diamond_dependency() {
    // d depends on b and c, which both depend on a
    execute_test_case(TestCase {
        input: "fn main() -> int { a = read_int()
b = a + 1
c = a + 2
d = b + c
print_int(d)
}",
        inputs: VecDeque::from(vec![10]),
        // b=11, c=12, d=23
        expected_outputs: VecDeque::from(vec![23]),
    });
}

#[test]
fn test_long_chain() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = 1
b = a + a
c = b + b
d = c + c
e = d + d
f = e + e
print_int(f)
}",
        inputs: VecDeque::new(),
        // 1, 2, 4, 8, 16, 32
        expected_outputs: VecDeque::from(vec![32]),
    });
}

// ── Many variables (register pressure) ───────────────────────────

#[test]
fn test_many_live_variables() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = 1
b = 2
c = 3
d = 4
e = 5
f = 6
g = 7
h = 8
i = 9
j = 10
k = 11
l = 12
print_int(a + b + c + d + e + f + g + h + i + j + k + l)
}",
        inputs: VecDeque::new(),
        // 1+2+...+12 = 78
        expected_outputs: VecDeque::from(vec![78]),
    });
}

#[test]
fn test_many_reads_all_alive() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = read_int()
b = read_int()
c = read_int()
d = read_int()
e = read_int()
print_int(a + b + c + d + e)
print_int(a)
print_int(e)
}",
        inputs: VecDeque::from(vec![1, 2, 3, 4, 5]),
        expected_outputs: VecDeque::from(vec![15, 1, 5]),
    });
}

#[test]
fn test_many_statements_sequential() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = 1
print_int(a)
b = a + 1
print_int(b)
c = b + 1
print_int(c)
d = c + 1
print_int(d)
e = d + 1
print_int(e)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 2, 3, 4, 5]),
    });
}
