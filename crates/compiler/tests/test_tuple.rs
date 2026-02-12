mod utils;
use utils::*;

// ── Basic creation and read ──────────────────────────────────────

#[test]
fn test_two_element_tuple() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (10, 20)
print_int(t[0])
print_int(t[1])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 20]),
    });
}

#[test]
fn test_three_element_tuple() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (1, 2, 3)
print_int(t[0])
print_int(t[1])
print_int(t[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 2, 3]),
    });
}

#[test]
fn test_single_element_tuple() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (42,)
print_int(t[0])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

#[test]
fn test_large_tuple() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (1, 2, 3, 4, 5, 6, 7, 8)
print_int(t[0])
print_int(t[3])
print_int(t[7])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 4, 8]),
    });
}

#[test]
fn test_tuple_with_negative_values() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (-1, 0, 1)
print_int(t[0])
print_int(t[1])
print_int(t[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![-1, 0, 1]),
    });
}

#[test]
fn test_tuple_with_zero() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (0, 0, 0)
print_int(t[0])
print_int(t[1])
print_int(t[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0, 0, 0]),
    });
}

// ── Subscript write ──────────────────────────────────────────────

#[test]
fn test_subscript_write() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (1, 2, 3)
t[0] = 99
print_int(t[0])
print_int(t[1])
print_int(t[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![99, 2, 3]),
    });
}

#[test]
fn test_subscript_write_middle() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (10, 20, 30)
t[1] = 99
print_int(t[0])
print_int(t[1])
print_int(t[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 99, 30]),
    });
}

#[test]
fn test_subscript_write_last() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (10, 20, 30)
t[2] = 99
print_int(t[0])
print_int(t[1])
print_int(t[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 20, 99]),
    });
}

#[test]
fn test_subscript_write_all() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (0, 0, 0)
t[0] = 10
t[1] = 20
t[2] = 30
print_int(t[0])
print_int(t[1])
print_int(t[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 20, 30]),
    });
}

#[test]
fn test_subscript_overwrite_same_index() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (1, 2)
t[0] = 10
t[0] = 20
t[0] = 30
print_int(t[0])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![30]),
    });
}

// ── Element arithmetic ───────────────────────────────────────────

#[test]
fn test_sum_elements() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (10, 20, 30)
print_int(t[0] + t[1] + t[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![60]),
    });
}

#[test]
fn test_element_subtract() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (100, 30)
print_int(t[0] - t[1])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![70]),
    });
}

#[test]
fn test_element_to_variable() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (5, 10)
x = t[0] + t[1]
print_int(x)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![15]),
    });
}

#[test]
fn test_write_computed_value() {
    // t[0] = t[1] + t[2]
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (0, 3, 7)
t[0] = t[1] + t[2]
print_int(t[0])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10]),
    });
}

#[test]
fn test_cascading_mutations() {
    // t = (1, 2, 3)
    // t[0] = t[1] + t[2]  -> (5, 2, 3)
    // t[1] = t[0] + t[2]  -> (5, 8, 3)
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (1, 2, 3)
t[0] = t[1] + t[2]
t[1] = t[0] + t[2]
print_int(t[0])
print_int(t[1])
print_int(t[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![5, 8, 3]),
    });
}

// ── Tuples with read_int ─────────────────────────────────────────

#[test]
fn test_tuple_from_inputs() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (read_int(), read_int(), read_int())
print_int(t[0])
print_int(t[1])
print_int(t[2])
}",
        inputs: VecDeque::from(vec![10, 20, 30]),
        expected_outputs: VecDeque::from(vec![10, 20, 30]),
    });
}

#[test]
fn test_tuple_mixed_constants_and_inputs() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (1, read_int(), 3)
print_int(t[0])
print_int(t[1])
print_int(t[2])
}",
        inputs: VecDeque::from(vec![42]),
        expected_outputs: VecDeque::from(vec![1, 42, 3]),
    });
}

#[test]
fn test_write_input_to_element() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (0, 0)
t[0] = read_int()
t[1] = read_int()
print_int(t[0])
print_int(t[1])
}",
        inputs: VecDeque::from(vec![7, 8]),
        expected_outputs: VecDeque::from(vec![7, 8]),
    });
}

// ── Multiple tuples ──────────────────────────────────────────────

#[test]
fn test_two_tuples() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = (1, 2)
b = (3, 4)
print_int(a[0] + b[0])
print_int(a[1] + b[1])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![4, 6]),
    });
}

#[test]
fn test_three_tuples() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = (1, 2, 3)
b = (10, 20, 30)
c = (100, 200, 300)
print_int(a[0] + b[1] + c[2])
}",
        inputs: VecDeque::new(),
        // 1 + 20 + 300 = 321
        expected_outputs: VecDeque::from(vec![321]),
    });
}

#[test]
fn test_tuple_alias_independence() {
    // Modifying one tuple should not affect another
    execute_test_case(TestCase {
        input: "fn main() -> int { a = (1, 2)
b = (3, 4)
a[0] = 99
print_int(a[0])
print_int(b[0])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![99, 3]),
    });
}

// ── Tuples with scalars (register pressure) ──────────────────────

#[test]
fn test_tuple_with_many_scalars() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = 1
b = 2
c = 3
d = 4
e = 5
f = 6
t = (100, 200)
print_int(a + b + c + d + e + f + t[0])
}",
        inputs: VecDeque::new(),
        // 1+2+3+4+5+6+100 = 121
        expected_outputs: VecDeque::from(vec![121]),
    });
}

#[test]
fn test_scalar_and_tuple_interleaved() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 10
t = (20, 30)
y = 40
print_int(x + t[0] + t[1] + y)
}",
        inputs: VecDeque::new(),
        // 10 + 20 + 30 + 40 = 100
        expected_outputs: VecDeque::from(vec![100]),
    });
}

// ── Tuples in control flow ───────────────────────────────────────

#[test]
fn test_tuple_in_if_branch() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (1, 2)
x = read_int()
if x > 0 {
    print_int(t[0])
} else {
    print_int(t[1])
}
}",
        inputs: VecDeque::from(vec![1]),
        expected_outputs: VecDeque::from(vec![1]),
    });
}

#[test]
fn test_tuple_in_if_branch_false() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (1, 2)
x = read_int()
if x > 0 {
    print_int(t[0])
} else {
    print_int(t[1])
}
}",
        inputs: VecDeque::from(vec![-1]),
        expected_outputs: VecDeque::from(vec![2]),
    });
}

#[test]
fn test_tuple_write_in_branch() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (0, 0)
x = read_int()
if x > 0 {
    t[0] = 1
} else {
    t[0] = -1
}
print_int(t[0])
}",
        inputs: VecDeque::from(vec![5]),
        expected_outputs: VecDeque::from(vec![1]),
    });
}

#[test]
fn test_tuple_in_while_loop() {
    // Read tuple element in each iteration
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (7, 3)
i = 0
sum = 0
while i < 4 {
    sum = sum + t[0] + t[1]
    i = i + 1
}
print_int(sum)
}",
        inputs: VecDeque::new(),
        // 4 * (7 + 3) = 40
        expected_outputs: VecDeque::from(vec![40]),
    });
}

#[test]
fn test_tuple_mutation_in_loop() {
    // Accumulate into tuple element
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (0, 1)
i = 0
while i < 5 {
    t[0] = t[0] + t[1]
    i = i + 1
}
print_int(t[0])
}",
        inputs: VecDeque::new(),
        // 0+1=1, 1+1=2, 2+1=3, 3+1=4, 4+1=5
        expected_outputs: VecDeque::from(vec![5]),
    });
}

#[test]
fn test_fibonacci_in_tuple() {
    // fib(8) = 21; use tuple to hold (a, b) state
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (1, 0)
i = 0
while i < 8 {
    next = t[0] + t[1]
    t[0] = t[1]
    t[1] = next
    i = i + 1
}
print_int(t[0])
print_int(t[1])
}",
        inputs: VecDeque::new(),
        // sequence: (1,0)->(0,1)->(1,1)->(1,2)->(2,3)->(3,5)->(5,8)->(8,13)->(13,21)
        expected_outputs: VecDeque::from(vec![13, 21]),
    });
}

// ── Nested tuples ────────────────────────────────────────────────

#[test]
fn test_nested_tuple_read() {
    execute_test_case(TestCase {
        input: "fn main() -> int { inner = (10, 20)
outer = (inner, 30)
print_int(outer[0][0])
print_int(outer[0][1])
print_int(outer[1])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 20, 30]),
    });
}

#[test]
fn test_nested_tuple_two_inner() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = (1, 2)
b = (3, 4)
pair = (a, b)
print_int(pair[0][0])
print_int(pair[0][1])
print_int(pair[1][0])
print_int(pair[1][1])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 2, 3, 4]),
    });
}

#[test]
fn test_nested_tuple_write_inner() {
    execute_test_case(TestCase {
        input: "fn main() -> int { inner = (10, 20)
outer = (inner, 99)
inner[0] = 55
print_int(outer[0][0])
print_int(inner[0])
}",
        inputs: VecDeque::new(),
        // outer[0] is a pointer to inner, so mutation is visible
        expected_outputs: VecDeque::from(vec![55, 55]),
    });
}

// ── Tuple element used as condition ──────────────────────────────

#[test]
fn test_tuple_element_as_condition() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (1, 0)
if t[0] > 0 {
    print_int(1)
} else {
    print_int(0)
}
if t[1] > 0 {
    print_int(1)
} else {
    print_int(0)
}
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 0]),
    });
}

#[test]
fn test_tuple_element_in_while_condition() {
    // Use tuple element as loop bound
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (0, 5)
while t[0] < t[1] {
    t[0] = t[0] + 1
}
print_int(t[0])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![5]),
    });
}

// ── Tuple with expressions as elements ───────────────────────────

#[test]
fn test_tuple_with_computed_elements() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 10
y = 20
t = (x + y, x - y)
print_int(t[0])
print_int(t[1])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![30, -10]),
    });
}

#[test]
fn test_tuple_with_variable_elements() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = 1
b = 2
c = 3
t = (a, b, c)
print_int(t[0])
print_int(t[1])
print_int(t[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 2, 3]),
    });
}

// ── Tuple created inside control flow ────────────────────────────

#[test]
fn test_tuple_created_in_branch() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
t = (0, 0)
if x > 0 {
    t = (1, 2)
} else {
    t = (3, 4)
}
print_int(t[0])
print_int(t[1])
}",
        inputs: VecDeque::from(vec![1]),
        expected_outputs: VecDeque::from(vec![1, 2]),
    });
}

#[test]
fn test_tuple_created_in_branch_false() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
t = (0, 0)
if x > 0 {
    t = (1, 2)
} else {
    t = (3, 4)
}
print_int(t[0])
print_int(t[1])
}",
        inputs: VecDeque::from(vec![-1]),
        expected_outputs: VecDeque::from(vec![3, 4]),
    });
}

#[test]
fn test_tuple_created_in_loop() {
    // Each iteration creates a new tuple; only last one survives
    execute_test_case(TestCase {
        input: "fn main() -> int { i = 0
t = (0, 0)
while i < 3 {
    t = (i, i + 1)
    i = i + 1
}
print_int(t[0])
print_int(t[1])
}",
        inputs: VecDeque::new(),
        // Last iteration: i=2, t=(2, 3)
        expected_outputs: VecDeque::from(vec![2, 3]),
    });
}

// ── Swap via tuple elements ──────────────────────────────────────

#[test]
fn test_swap_via_tuple() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (10, 20)
tmp = t[0]
t[0] = t[1]
t[1] = tmp
print_int(t[0])
print_int(t[1])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![20, 10]),
    });
}

// ── Stress: many elements read back ──────────────────────────────

#[test]
fn test_read_all_five_elements() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (10, 20, 30, 40, 50)
print_int(t[0])
print_int(t[1])
print_int(t[2])
print_int(t[3])
print_int(t[4])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 20, 30, 40, 50]),
    });
}

#[test]
fn test_sum_five_elements() {
    execute_test_case(TestCase {
        input: "fn main() -> int { t = (1, 2, 3, 4, 5)
print_int(t[0] + t[1] + t[2] + t[3] + t[4])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![15]),
    });
}
