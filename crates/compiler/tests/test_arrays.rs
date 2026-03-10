mod utils;
use utils::*;

// ── Basic creation and read ──────────────────────────────────────

#[test]
fn test_two_element_array() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [10, 20]
print_int(a[0])
print_int(a[1])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 20]),
    });
}

#[test]
fn test_three_element_array() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [1, 2, 3]
print_int(a[0])
print_int(a[1])
print_int(a[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 2, 3]),
    });
}

#[test]
fn test_single_element_array() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [42]
print_int(a[0])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

#[test]
fn test_large_array() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [1, 2, 3, 4, 5, 6, 7, 8]
print_int(a[0])
print_int(a[3])
print_int(a[7])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 4, 8]),
    });
}

#[test]
fn test_array_with_negative_values() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [-1, 0, 1]
print_int(a[0])
print_int(a[1])
print_int(a[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![-1, 0, 1]),
    });
}

#[test]
fn test_array_with_zeros() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [0, 0, 0]
print_int(a[0])
print_int(a[1])
print_int(a[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0, 0, 0]),
    });
}

// ── Subscript write ──────────────────────────────────────────────

#[test]
fn test_subscript_write() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [1, 2, 3]
a[0] = 99
print_int(a[0])
print_int(a[1])
print_int(a[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![99, 2, 3]),
    });
}

#[test]
fn test_subscript_write_middle() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [10, 20, 30]
a[1] = 99
print_int(a[0])
print_int(a[1])
print_int(a[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 99, 30]),
    });
}

#[test]
fn test_subscript_write_last() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [10, 20, 30]
a[2] = 99
print_int(a[0])
print_int(a[1])
print_int(a[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 20, 99]),
    });
}

#[test]
fn test_subscript_write_all() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [0, 0, 0]
a[0] = 10
a[1] = 20
a[2] = 30
print_int(a[0])
print_int(a[1])
print_int(a[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 20, 30]),
    });
}

#[test]
fn test_subscript_overwrite_same_index() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [1, 2]
a[0] = 10
a[0] = 20
a[0] = 30
print_int(a[0])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![30]),
    });
}

// ── Element arithmetic ───────────────────────────────────────────

#[test]
fn test_sum_elements() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [10, 20, 30]
print_int(a[0] + a[1] + a[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![60]),
    });
}

#[test]
fn test_element_subtract() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [100, 30]
print_int(a[0] - a[1])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![70]),
    });
}

#[test]
fn test_element_to_variable() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [5, 10]
x = a[0] + a[1]
print_int(x)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![15]),
    });
}

#[test]
fn test_write_computed_value() {
    // a[0] = a[1] + a[2]
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [0, 3, 7]
a[0] = a[1] + a[2]
print_int(a[0])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10]),
    });
}

#[test]
fn test_cascading_mutations() {
    // a = [1, 2, 3]
    // a[0] = a[1] + a[2]  -> [5, 2, 3]
    // a[1] = a[0] + a[2]  -> [5, 8, 3]
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [1, 2, 3]
a[0] = a[1] + a[2]
a[1] = a[0] + a[2]
print_int(a[0])
print_int(a[1])
print_int(a[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![5, 8, 3]),
    });
}

// ── Arrays with read_int ─────────────────────────────────────────

#[test]
fn test_array_from_inputs() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [read_int(), read_int(), read_int()]
print_int(a[0])
print_int(a[1])
print_int(a[2])
}",
        inputs: VecDeque::from(vec![10, 20, 30]),
        expected_outputs: VecDeque::from(vec![10, 20, 30]),
    });
}

#[test]
fn test_array_mixed_constants_and_inputs() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [1, read_int(), 3]
print_int(a[0])
print_int(a[1])
print_int(a[2])
}",
        inputs: VecDeque::from(vec![42]),
        expected_outputs: VecDeque::from(vec![1, 42, 3]),
    });
}

#[test]
fn test_write_input_to_element() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [0, 0]
a[0] = read_int()
a[1] = read_int()
print_int(a[0])
print_int(a[1])
}",
        inputs: VecDeque::from(vec![7, 8]),
        expected_outputs: VecDeque::from(vec![7, 8]),
    });
}

// ── Multiple arrays ──────────────────────────────────────────────

#[test]
fn test_two_arrays() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [1, 2]
b = [3, 4]
print_int(a[0] + b[0])
print_int(a[1] + b[1])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![4, 6]),
    });
}

#[test]
fn test_three_arrays() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [1, 2, 3]
b = [10, 20, 30]
c = [100, 200, 300]
print_int(a[0] + b[1] + c[2])
}",
        inputs: VecDeque::new(),
        // 1 + 20 + 300 = 321
        expected_outputs: VecDeque::from(vec![321]),
    });
}

#[test]
fn test_array_independence() {
    // Modifying one array should not affect another
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [1, 2]
b = [3, 4]
a[0] = 99
print_int(a[0])
print_int(b[0])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![99, 3]),
    });
}

// ── Arrays with scalars (register pressure) ──────────────────────

#[test]
fn test_array_with_many_scalars() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 1
y = 2
z = 3
w = 4
v = 5
u = 6
a = [100, 200]
print_int(x + y + z + w + v + u + a[0])
}",
        inputs: VecDeque::new(),
        // 1+2+3+4+5+6+100 = 121
        expected_outputs: VecDeque::from(vec![121]),
    });
}

#[test]
fn test_scalar_and_array_interleaved() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 10
a = [20, 30]
y = 40
print_int(x + a[0] + a[1] + y)
}",
        inputs: VecDeque::new(),
        // 10 + 20 + 30 + 40 = 100
        expected_outputs: VecDeque::from(vec![100]),
    });
}

// ── Arrays in control flow ───────────────────────────────────────

#[test]
fn test_array_in_if_branch() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [1, 2]
x = read_int()
if x > 0 {
    print_int(a[0])
} else {
    print_int(a[1])
}
}",
        inputs: VecDeque::from(vec![1]),
        expected_outputs: VecDeque::from(vec![1]),
    });
}

#[test]
fn test_array_in_if_branch_false() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [1, 2]
x = read_int()
if x > 0 {
    print_int(a[0])
} else {
    print_int(a[1])
}
}",
        inputs: VecDeque::from(vec![-1]),
        expected_outputs: VecDeque::from(vec![2]),
    });
}

#[test]
fn test_array_write_in_branch() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [0, 0]
x = read_int()
if x > 0 {
    a[0] = 1
} else {
    a[0] = -1
}
print_int(a[0])
}",
        inputs: VecDeque::from(vec![5]),
        expected_outputs: VecDeque::from(vec![1]),
    });
}

#[test]
fn test_array_write_in_branch_false() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [0, 0]
x = read_int()
if x > 0 {
    a[0] = 1
} else {
    a[0] = -1
}
print_int(a[0])
}",
        inputs: VecDeque::from(vec![-5]),
        expected_outputs: VecDeque::from(vec![-1]),
    });
}

#[test]
fn test_array_in_while_loop() {
    // Read array elements in each iteration
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [7, 3]
i = 0
sum = 0
while i < 4 {
    sum = sum + a[0] + a[1]
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
fn test_array_mutation_in_loop() {
    // Accumulate into array element
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [0, 1]
i = 0
while i < 5 {
    a[0] = a[0] + a[1]
    i = i + 1
}
print_int(a[0])
}",
        inputs: VecDeque::new(),
        // 0+1=1, 1+1=2, 2+1=3, 3+1=4, 4+1=5
        expected_outputs: VecDeque::from(vec![5]),
    });
}

#[test]
fn test_fibonacci_in_array() {
    // fib(8) = 21; use array to hold [a, b] state
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [1, 0]
i = 0
while i < 8 {
    next = a[0] + a[1]
    a[0] = a[1]
    a[1] = next
    i = i + 1
}
print_int(a[0])
print_int(a[1])
}",
        inputs: VecDeque::new(),
        // sequence: [1,0]->[0,1]->[1,1]->[1,2]->[2,3]->[3,5]->[5,8]->[8,13]->[13,21]
        expected_outputs: VecDeque::from(vec![13, 21]),
    });
}

// ── Array element used as condition ──────────────────────────────

#[test]
fn test_array_element_as_condition() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [1, 0]
if a[0] > 0 {
    print_int(1)
} else {
    print_int(0)
}
if a[1] > 0 {
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
fn test_array_element_in_while_condition() {
    // Use array element as loop bound
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [0, 5]
while a[0] < a[1] {
    a[0] = a[0] + 1
}
print_int(a[0])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![5]),
    });
}

// ── Array with computed elements ─────────────────────────────────

#[test]
fn test_array_with_computed_elements() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 10
y = 20
a = [x + y, x - y]
print_int(a[0])
print_int(a[1])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![30, -10]),
    });
}

#[test]
fn test_array_with_variable_elements() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 1
y = 2
z = 3
a = [x, y, z]
print_int(a[0])
print_int(a[1])
print_int(a[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 2, 3]),
    });
}

// ── Array created inside control flow ────────────────────────────

#[test]
fn test_array_created_in_branch() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
a = [0, 0]
if x > 0 {
    a = [1, 2]
} else {
    a = [3, 4]
}
print_int(a[0])
print_int(a[1])
}",
        inputs: VecDeque::from(vec![1]),
        expected_outputs: VecDeque::from(vec![1, 2]),
    });
}

#[test]
fn test_array_created_in_branch_false() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = read_int()
a = [0, 0]
if x > 0 {
    a = [1, 2]
} else {
    a = [3, 4]
}
print_int(a[0])
print_int(a[1])
}",
        inputs: VecDeque::from(vec![-1]),
        expected_outputs: VecDeque::from(vec![3, 4]),
    });
}

#[test]
fn test_array_created_in_loop() {
    // Each iteration creates a new array; only last one survives
    execute_test_case(TestCase {
        input: "fn main() -> int { i = 0
a = [0, 0]
while i < 3 {
    a = [i, i + 1]
    i = i + 1
}
print_int(a[0])
print_int(a[1])
}",
        inputs: VecDeque::new(),
        // Last iteration: i=2, a=[2, 3]
        expected_outputs: VecDeque::from(vec![2, 3]),
    });
}

// ── Swap via array elements ──────────────────────────────────────

#[test]
fn test_swap_via_array() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [10, 20]
tmp = a[0]
a[0] = a[1]
a[1] = tmp
print_int(a[0])
print_int(a[1])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![20, 10]),
    });
}

// ── Stress: many elements ────────────────────────────────────────

#[test]
fn test_read_all_five_elements() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [10, 20, 30, 40, 50]
print_int(a[0])
print_int(a[1])
print_int(a[2])
print_int(a[3])
print_int(a[4])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 20, 30, 40, 50]),
    });
}

#[test]
fn test_sum_five_elements() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [1, 2, 3, 4, 5]
print_int(a[0] + a[1] + a[2] + a[3] + a[4])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![15]),
    });
}

// ── Variable and expression indices ──────────────────────────────

#[test]
fn test_variable_index_read() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [10, 20, 30]
i = 1
print_int(a[i])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![20]),
    });
}

#[test]
fn test_variable_index_write() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [10, 20, 30]
i = 1
a[i] = 99
print_int(a[0])
print_int(a[1])
print_int(a[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 99, 30]),
    });
}

#[test]
fn test_expr_index_read() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [10, 20, 30]
i = 1
print_int(a[i + 1])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![30]),
    });
}

#[test]
fn test_expr_index_write() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [10, 20, 30]
i = 1
a[i + 1] = 99
print_int(a[0])
print_int(a[1])
print_int(a[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 20, 99]),
    });
}

#[test]
fn test_index_from_input() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [10, 20, 30]
i = read_int()
print_int(a[i])
}",
        inputs: VecDeque::from(vec![2]),
        expected_outputs: VecDeque::from(vec![30]),
    });
}

#[test]
fn test_index_write_from_input() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [0, 0, 0]
i = read_int()
a[i] = 42
print_int(a[0])
print_int(a[1])
print_int(a[2])
}",
        inputs: VecDeque::from(vec![1]),
        expected_outputs: VecDeque::from(vec![0, 42, 0]),
    });
}

#[test]
fn test_loop_scan_array() {
    // Sum all elements using a variable index
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [1, 2, 3, 4, 5]
sum = 0
i = 0
while i < 5 {
    sum = sum + a[i]
    i = i + 1
}
print_int(sum)
}",
        inputs: VecDeque::new(),
        // 1+2+3+4+5 = 15
        expected_outputs: VecDeque::from(vec![15]),
    });
}

#[test]
fn test_loop_fill_array() {
    // Write to every element with a variable index
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [0, 0, 0]
i = 0
while i < 3 {
    a[i] = i + 1
    i = i + 1
}
print_int(a[0])
print_int(a[1])
print_int(a[2])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 2, 3]),
    });
}

#[test]
fn test_variable_index_both_read_and_write() {
    // Copy a[i] to a[i+1] using the same index variable
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [10, 0, 0]
i = 0
a[i + 1] = a[i]
print_int(a[0])
print_int(a[1])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 10]),
    });
}

// ── Arrays of bools ──────────────────────────────────────────────

#[test]
fn test_bool_array_read() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [true, false]
if a[0] { print_int(1) } else { print_int(0) }
if a[1] { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 0]),
    });
}

#[test]
fn test_bool_array_three_elements() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [false, true, false]
if a[0] { print_int(1) } else { print_int(0) }
if a[1] { print_int(1) } else { print_int(0) }
if a[2] { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0, 1, 0]),
    });
}

#[test]
fn test_bool_array_write() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [false, false]
a[0] = true
if a[0] { print_int(1) } else { print_int(0) }
if a[1] { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 0]),
    });
}

#[test]
fn test_bool_array_overwrite() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [true, true]
a[0] = false
a[1] = false
if a[0] { print_int(1) } else { print_int(0) }
if a[1] { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0, 0]),
    });
}

#[test]
fn test_bool_array_not_element() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [true, false]
if !a[0] { print_int(1) } else { print_int(0) }
if !a[1] { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0, 1]),
    });
}

#[test]
fn test_bool_array_and_elements() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [true, false, true]
if a[0] && a[1] { print_int(1) } else { print_int(0) }
if a[0] && a[2] { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0, 1]),
    });
}

#[test]
fn test_bool_array_or_elements() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [false, true]
if a[0] || a[1] { print_int(1) } else { print_int(0) }
if a[0] || a[0] { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 0]),
    });
}

#[test]
fn test_bool_array_element_as_while_condition() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [true, false]
x = 0
while a[0] {
    x = x + 1
    a[0] = false
}
print_int(x)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1]),
    });
}

#[test]
fn test_bool_array_write_in_branch() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [false, false]
x = read_int()
if x > 0 {
    a[0] = true
} else {
    a[1] = true
}
if a[0] { print_int(1) } else { print_int(0) }
if a[1] { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::from(vec![1]),
        expected_outputs: VecDeque::from(vec![1, 0]),
    });
}

#[test]
fn test_bool_array_write_in_branch_false() {
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [false, false]
x = read_int()
if x > 0 {
    a[0] = true
} else {
    a[1] = true
}
if a[0] { print_int(1) } else { print_int(0) }
if a[1] { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::from(vec![-1]),
        expected_outputs: VecDeque::from(vec![0, 1]),
    });
}

#[test]
fn test_bool_array_store_comparison() {
    // Store a comparison result into a bool array element
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [false, false]
x = read_int()
a[0] = x > 0
a[1] = x < 0
if a[0] { print_int(1) } else { print_int(0) }
if a[1] { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::from(vec![5]),
        expected_outputs: VecDeque::from(vec![1, 0]),
    });
}

#[test]
fn test_bool_array_independence() {
    // Modifying one bool array should not affect another
    execute_test_case(TestCase {
        input: "fn main() -> int { a = [true, true]
b = [false, false]
a[0] = false
if a[0] { print_int(1) } else { print_int(0) }
if b[0] { print_int(1) } else { print_int(0) }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0, 0]),
    });
}
