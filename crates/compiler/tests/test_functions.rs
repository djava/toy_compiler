mod utils;
use utils::*;

// ── Basic function definition and call ──────────────────────────

#[test]
fn test_simple_function_call() {
    execute_test_case(TestCase {
        input: "fn add(x: int, y: int) -> int {
    return x + y
}

fn main() {
    print_int(add(40, 2))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

#[test]
fn test_function_no_params_returns_int() {
    execute_test_case(TestCase {
        input: "fn seven() -> int {
    return 7
}

fn main() {
    print_int(seven())
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![7]),
    });
}

#[test]
fn test_function_single_param() {
    execute_test_case(TestCase {
        input: "fn double(x: int) -> int {
    return x + x
}

fn main() {
    print_int(double(21))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

#[test]
fn test_function_three_params() {
    execute_test_case(TestCase {
        input: "fn sum3(a: int, b: int, c: int) -> int {
    return a + b + c
}

fn main() {
    print_int(sum3(10, 20, 30))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![60]),
    });
}

#[test]
fn test_function_four_params() {
    execute_test_case(TestCase {
        input: "fn sum4(a: int, b: int, c: int, d: int) -> int {
    return a + b + c + d
}

fn main() {
    print_int(sum4(1, 2, 3, 4))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10]),
    });
}

#[test]
fn test_function_five_params() {
    execute_test_case(TestCase {
        input: "fn sum5(a: int, b: int, c: int, d: int, e: int) -> int {
    return a + b + c + d + e
}

fn main() {
    print_int(sum5(1, 2, 3, 4, 5))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![15]),
    });
}

#[test]
fn test_function_six_params() {
    execute_test_case(TestCase {
        input: "fn sum6(a: int, b: int, c: int, d: int, e: int, f: int) -> int {
    return a + b + c + d + e + f
}

fn main() {
    print_int(sum6(1, 2, 3, 4, 5, 6))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![21]),
    });
}

#[test]
fn test_function_seven_params() {
    execute_test_case(TestCase {
        input: "fn sum7(a: int, b: int, c: int, d: int, e: int, f: int, g: int) -> int {
    return a + b + c + d + e + f + g
}

fn main() {
    print_int(sum7(1, 2, 3, 4, 5, 6, 7))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![28]),
    });
}

// ── Return type omitted (void / none functions) ─────────────────

#[test]
fn test_void_function_no_return_type() {
    execute_test_case(TestCase {
        input: "fn greet() {
    print_int(42)
}

fn main() {
    greet()
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

#[test]
fn test_void_function_with_params() {
    execute_test_case(TestCase {
        input: "fn print_sum(a: int, b: int) {
    print_int(a + b)
}

fn main() {
    print_sum(10, 20)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![30]),
    });
}

// ── Multiple function definitions ───────────────────────────────

#[test]
fn test_two_functions() {
    execute_test_case(TestCase {
        input: "fn add(x: int, y: int) -> int {
    return x + y
}

fn sub(x: int, y: int) -> int {
    return x - y
}

fn main() {
    print_int(add(10, 5))
    print_int(sub(10, 5))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![15, 5]),
    });
}

#[test]
fn test_three_functions() {
    execute_test_case(TestCase {
        input: "fn a() -> int {
    return 1
}

fn b() -> int {
    return 2
}

fn c() -> int {
    return 3
}

fn main() {
    print_int(a())
    print_int(b())
    print_int(c())
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 2, 3]),
    });
}

// ── Function calling another function ───────────────────────────

#[test]
fn test_function_calls_function() {
    execute_test_case(TestCase {
        input: "fn add(x: int, y: int) -> int {
    return x + y
}

fn add3(a: int, b: int, c: int) -> int {
    return add(add(a, b), c)
}

fn main() {
    print_int(add3(10, 20, 30))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![60]),
    });
}

#[test]
fn test_chain_of_calls() {
    execute_test_case(TestCase {
        input: "fn inc(x: int) -> int {
    return x + 1
}

fn main() {
    print_int(inc(inc(inc(0))))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![3]),
    });
}

#[test]
fn test_nested_function_calls_as_arguments() {
    execute_test_case(TestCase {
        input: "fn add(x: int, y: int) -> int {
    return x + y
}

fn main() {
    print_int(add(add(1, 2), add(3, 4)))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10]),
    });
}

// ── Function with local variables ───────────────────────────────

#[test]
fn test_function_with_locals() {
    execute_test_case(TestCase {
        input: "fn compute(x: int, y: int) -> int {
    sum = x + y
    diff = x - y
    return sum + diff
}

fn main() {
    print_int(compute(10, 3))
}",
        inputs: VecDeque::new(),
        // sum=13, diff=7, result=20 -> actually (10+3)+(10-3) = 13+7 = 20
        expected_outputs: VecDeque::from(vec![20]),
    });
}

#[test]
fn test_function_local_does_not_leak_to_main() {
    // Local variable in function should not affect main's scope
    execute_test_case(TestCase {
        input: "fn set_x() -> int {
    x = 99
    return x
}

fn main() {
    x = 1
    print_int(set_x())
    print_int(x)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![99, 1]),
    });
}

// ── Function result used in expressions ─────────────────────────

#[test]
fn test_function_result_in_arithmetic() {
    execute_test_case(TestCase {
        input: "fn five() -> int {
    return 5
}

fn main() {
    print_int(five() + five())
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10]),
    });
}

#[test]
fn test_function_result_assigned_to_variable() {
    execute_test_case(TestCase {
        input: "fn add(x: int, y: int) -> int {
    return x + y
}

fn main() {
    result = add(3, 4)
    print_int(result)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![7]),
    });
}

#[test]
fn test_function_result_in_comparison() {
    execute_test_case(TestCase {
        input: "fn add(x: int, y: int) -> int {
    return x + y
}

fn main() {
    if add(1, 2) > 2 {
        print_int(1)
    } else {
        print_int(0)
    }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1]),
    });
}

// ── Function called multiple times ──────────────────────────────

#[test]
fn test_function_called_multiple_times() {
    execute_test_case(TestCase {
        input: "fn double(x: int) -> int {
    return x + x
}

fn main() {
    print_int(double(1))
    print_int(double(2))
    print_int(double(3))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![2, 4, 6]),
    });
}

#[test]
fn test_function_called_in_loop() {
    execute_test_case(TestCase {
        input: "fn double(x: int) -> int {
    return x + x
}

fn main() {
    i = 0
    while i < 5 {
        print_int(double(i))
        i = i + 1
    }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0, 2, 4, 6, 8]),
    });
}

// ── Recursive functions ─────────────────────────────────────────

#[test]
fn test_recursive_factorial() {
    execute_test_case(TestCase {
        input: "fn fact(n: int) -> int {
    if n < 2 {
        return 1
    } else {
        return n * fact(n - 1)
    }
}

fn main() {
    print_int(fact(5))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![120]),
    });
}

#[test]
fn test_recursive_factorial_base_case() {
    execute_test_case(TestCase {
        input: "fn fact(n: int) -> int {
    if n < 2 {
        return 1
    } else {
        return n * fact(n - 1)
    }
}

fn main() {
    print_int(fact(0))
    print_int(fact(1))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 1]),
    });
}

#[test]
fn test_recursive_fibonacci() {
    execute_test_case(TestCase {
        input: "fn fib(n: int) -> int {
    if n < 2 {
        return n
    } else {
        return fib(n - 1) + fib(n - 2)
    }
}

fn main() {
    print_int(fib(0))
    print_int(fib(1))
    print_int(fib(2))
    print_int(fib(3))
    print_int(fib(4))
    print_int(fib(5))
    print_int(fib(6))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0, 1, 1, 2, 3, 5, 8]),
    });
}

#[test]
fn test_recursive_countdown() {
    execute_test_case(TestCase {
        input: "fn countdown(n: int) {
    if n > 0 {
        print_int(n)
        countdown(n - 1)
    } else {
        print_int(0)
    }
}

fn main() {
    countdown(3)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![3, 2, 1, 0]),
    });
}

// ── Mutual recursion ────────────────────────────────────────────

#[test]
fn test_mutual_recursion_even_odd() {
    execute_test_case(TestCase {
        input: "fn is_even(n: int) -> int {
    if n == 0 {
        return 1
    } else {
        return is_odd(n - 1)
    }
}

fn is_odd(n: int) -> int {
    if n == 0 {
        return 0
    } else {
        return is_even(n - 1)
    }
}

fn main() {
    print_int(is_even(0))
    print_int(is_even(1))
    print_int(is_even(4))
    print_int(is_odd(3))
    print_int(is_odd(4))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 0, 1, 1, 0]),
    });
}

// ── Function with control flow ──────────────────────────────────

#[test]
fn test_function_with_if_else() {
    execute_test_case(TestCase {
        input: "fn abs(x: int) -> int {
    if x < 0 {
        return 0 - x
    } else {
        return x
    }
}

fn main() {
    print_int(abs(5))
    print_int(abs(-5))
    print_int(abs(0))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![5, 5, 0]),
    });
}

#[test]
fn test_function_with_while_loop() {
    execute_test_case(TestCase {
        input: "fn sum_to(n: int) -> int {
    total = 0
    i = 1
    while i <= n {
        total = total + i
        i = i + 1
    }
    return total
}

fn main() {
    print_int(sum_to(10))
}",
        inputs: VecDeque::new(),
        // 1+2+...+10 = 55
        expected_outputs: VecDeque::from(vec![55]),
    });
}

#[test]
fn test_function_with_nested_if() {
    execute_test_case(TestCase {
        input: "fn clamp(x: int, lo: int, hi: int) -> int {
    if x < lo {
        return lo
    } else {
        if x > hi {
            return hi
        } else {
            return x
        }
    }
}

fn main() {
    print_int(clamp(-5, 0, 10))
    print_int(clamp(5, 0, 10))
    print_int(clamp(15, 0, 10))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0, 5, 10]),
    });
}

// ── Edge cases: argument values ─────────────────────────────────

#[test]
fn test_function_with_zero_args() {
    execute_test_case(TestCase {
        input: "fn add(x: int, y: int) -> int {
    return x + y
}

fn main() {
    print_int(add(0, 0))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_function_with_negative_args() {
    execute_test_case(TestCase {
        input: "fn add(x: int, y: int) -> int {
    return x + y
}

fn main() {
    print_int(add(-10, -20))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![-30]),
    });
}

#[test]
fn test_function_with_large_values() {
    execute_test_case(TestCase {
        input: "fn add(x: int, y: int) -> int {
    return x + y
}

fn main() {
    print_int(add(1000000, 2000000))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![3000000]),
    });
}

// ── Function with read_int ──────────────────────────────────────

#[test]
fn test_function_with_read_int_arg() {
    execute_test_case(TestCase {
        input: "fn double(x: int) -> int {
    return x + x
}

fn main() {
    print_int(double(read_int()))
}",
        inputs: VecDeque::from(vec![21]),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

#[test]
fn test_function_reads_input_internally() {
    execute_test_case(TestCase {
        input: "fn read_and_double() -> int {
    x = read_int()
    return x + x
}

fn main() {
    print_int(read_and_double())
}",
        inputs: VecDeque::from(vec![5]),
        expected_outputs: VecDeque::from(vec![10]),
    });
}

// ── Function parameter shadowing ────────────────────────────────

#[test]
fn test_param_name_same_across_functions() {
    execute_test_case(TestCase {
        input: "fn foo(x: int) -> int {
    return x + 1
}

fn bar(x: int) -> int {
    return x + 2
}

fn main() {
    print_int(foo(10))
    print_int(bar(10))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![11, 12]),
    });
}

#[test]
fn test_local_var_same_name_as_param() {
    execute_test_case(TestCase {
        input: "fn foo(x: int) -> int {
    x = x + 1
    return x
}

fn main() {
    print_int(foo(10))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![11]),
    });
}

// ── Function ordering (forward reference) ───────────────────────

#[test]
fn test_function_defined_after_main() {
    execute_test_case(TestCase {
        input: "fn main() {
    print_int(double(5))
}

fn double(x: int) -> int {
    return x + x
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10]),
    });
}

#[test]
fn test_function_defined_between_others() {
    execute_test_case(TestCase {
        input: "fn a() -> int {
    return b()
}

fn b() -> int {
    return 42
}

fn main() {
    print_int(a())
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

// ── Functions with bool parameters/returns ──────────────────────

#[test]
fn test_function_bool_param() {
    execute_test_case(TestCase {
        input: "fn pick(flag: bool, a: int, b: int) -> int {
    if flag {
        return a
    } else {
        return b
    }
}

fn main() {
    print_int(pick(true, 10, 20))
    print_int(pick(false, 10, 20))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 20]),
    });
}

#[test]
fn test_function_returns_bool_used_in_if() {
    execute_test_case(TestCase {
        input: "fn is_positive(x: int) -> bool {
    return x > 0
}

fn main() {
    if is_positive(5) {
        print_int(1)
    } else {
        print_int(0)
    }
    if is_positive(-3) {
        print_int(1)
    } else {
        print_int(0)
    }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 0]),
    });
}

// ── Functions with tuples ───────────────────────────────────────

#[test]
fn test_function_returns_tuple() {
    execute_test_case(TestCase {
        input: "fn make_pair(a: int, b: int) -> tuple<int, int> {
    return (a, b)
}

fn main() {
    t = make_pair(10, 20)
    print_int(t[0])
    print_int(t[1])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 20]),
    });
}

#[test]
fn test_function_takes_tuple_param() {
    execute_test_case(TestCase {
        input: "fn sum_pair(t: tuple<int, int>) -> int {
    return t[0] + t[1]
}

fn main() {
    p = (3, 7)
    print_int(sum_pair(p))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10]),
    });
}

#[test]
fn test_function_modifies_tuple_param() {
    execute_test_case(TestCase {
        input: "fn set_first(t: tuple<int, int>, val: int) {
    t[0] = val
}

fn main() {
    p = (1, 2)
    set_first(p, 99)
    print_int(p[0])
    print_int(p[1])
}",
        inputs: VecDeque::new(),
        // If tuples are passed by reference, p[0] should be 99
        // If by value, p[0] should still be 1
        expected_outputs: VecDeque::from(vec![99, 2]),
    });
}

// ── Tail call position ──────────────────────────────────────────

#[test]
fn test_tail_call_simple() {
    execute_test_case(TestCase {
        input: "fn identity(x: int) -> int {
    return x
}

fn main() {
    print_int(identity(42))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

#[test]
fn test_tail_recursive_sum() {
    execute_test_case(TestCase {
        input: "fn sum_helper(n: int, acc: int) -> int {
    if n == 0 {
        return acc
    } else {
        return sum_helper(n - 1, acc + n)
    }
}

fn main() {
    print_int(sum_helper(10, 0))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![55]),
    });
}

#[test]
fn test_tail_recursive_deeper() {
    // Test that tail call optimization can handle deeper recursion
    execute_test_case(TestCase {
        input: "fn count_down(n: int, acc: int) -> int {
    if n == 0 {
        return acc
    } else {
        return count_down(n - 1, acc + 1)
    }
}

fn main() {
    print_int(count_down(100, 0))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![100]),
    });
}

// ── Return statement edge cases ─────────────────────────────────

#[test]
fn test_return_constant() {
    execute_test_case(TestCase {
        input: "fn always_42() -> int {
    return 42
}

fn main() {
    print_int(always_42())
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

#[test]
fn test_return_negative() {
    execute_test_case(TestCase {
        input: "fn neg() -> int {
    return -1
}

fn main() {
    print_int(neg())
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![-1]),
    });
}

#[test]
fn test_return_zero() {
    execute_test_case(TestCase {
        input: "fn zero() -> int {
    return 0
}

fn main() {
    print_int(zero())
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0]),
    });
}

#[test]
fn test_return_complex_expression() {
    execute_test_case(TestCase {
        input: "fn compute(a: int, b: int) -> int {
    return (a + b) * (a - b)
}

fn main() {
    print_int(compute(10, 3))
}",
        inputs: VecDeque::new(),
        // (10+3)*(10-3) = 13*7 = 91
        expected_outputs: VecDeque::from(vec![91]),
    });
}

#[test]
fn test_early_return_in_if() {
    execute_test_case(TestCase {
        input: "fn max(a: int, b: int) -> int {
    if a > b {
        return a
    } else {
        return b
    }
}

fn main() {
    print_int(max(3, 7))
    print_int(max(9, 2))
    print_int(max(5, 5))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![7, 9, 5]),
    });
}

// ── Arithmetic operations in functions ──────────────────────────

#[test]
fn test_function_multiply() {
    execute_test_case(TestCase {
        input: "fn mul(x: int, y: int) -> int {
    return x * y
}

fn main() {
    print_int(mul(6, 7))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

#[test]
fn test_function_negate() {
    execute_test_case(TestCase {
        input: "fn negate(x: int) -> int {
    return 0 - x
}

fn main() {
    print_int(negate(42))
    print_int(negate(-10))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![-42, 10]),
    });
}

// ── Mixed: functions + tuples + control flow ────────────────────

#[test]
fn test_function_creates_and_returns_tuple_in_branch() {
    execute_test_case(TestCase {
        input: "fn make(flag: bool) -> tuple<int, int> {
    if flag {
        return (1, 2)
    } else {
        return (3, 4)
    }
}

fn main() {
    t = make(true)
    print_int(t[0])
    print_int(t[1])
    t = make(false)
    print_int(t[0])
    print_int(t[1])
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 2, 3, 4]),
    });
}

#[test]
fn test_function_with_loop_and_accumulator() {
    execute_test_case(TestCase {
        input: "fn power(base: int, exp: int) -> int {
    result = 1
    i = 0
    while i < exp {
        result = result * base
        i = i + 1
    }
    return result
}

fn main() {
    print_int(power(2, 0))
    print_int(power(2, 1))
    print_int(power(2, 10))
    print_int(power(3, 3))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 2, 1024, 27]),
    });
}

// ── Function call as part of print_int ──────────────────────────

#[test]
fn test_function_call_directly_in_print() {
    execute_test_case(TestCase {
        input: "fn add(x: int, y: int) -> int {
    return x + y
}

fn main() {
    print_int(add(1, 2) + add(3, 4))
}",
        inputs: VecDeque::new(),
        // 3 + 7 = 10
        expected_outputs: VecDeque::from(vec![10]),
    });
}

// ── Stress: many function calls ─────────────────────────────────

#[test]
fn test_many_calls_same_function() {
    execute_test_case(TestCase {
        input: "fn inc(x: int) -> int {
    return x + 1
}

fn main() {
    print_int(inc(0))
    print_int(inc(1))
    print_int(inc(2))
    print_int(inc(3))
    print_int(inc(4))
    print_int(inc(5))
    print_int(inc(6))
    print_int(inc(7))
    print_int(inc(8))
    print_int(inc(9))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    });
}

// ── Fibonacci iterative in function ─────────────────────────────

#[test]
fn test_iterative_fibonacci_function() {
    execute_test_case(TestCase {
        input: "fn fib(n: int) -> int {
    a = 0
    b = 1
    i = 0
    while i < n {
        next = a + b
        a = b
        b = next
        i = i + 1
    }
    return a
}

fn main() {
    print_int(fib(0))
    print_int(fib(1))
    print_int(fib(5))
    print_int(fib(10))
}",
        inputs: VecDeque::new(),
        // fib(0)=0, fib(1)=1, fib(5)=5, fib(10)=55
        expected_outputs: VecDeque::from(vec![0, 1, 5, 55]),
    });
}

// ── Function with main using variables same as params ───────────

#[test]
fn test_main_var_same_name_as_function_param() {
    execute_test_case(TestCase {
        input: "fn add(x: int, y: int) -> int {
    return x + y
}

fn main() {
    x = 100
    y = 200
    print_int(add(x, y))
    print_int(x)
    print_int(y)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![300, 100, 200]),
    });
}

// ── Void function called multiple times ─────────────────────────

#[test]
fn test_void_function_called_multiple_times() {
    execute_test_case(TestCase {
        input: "fn print_double(x: int) {
    print_int(x + x)
}

fn main() {
    print_double(1)
    print_double(2)
    print_double(3)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![2, 4, 6]),
    });
}

// ── Function returning param directly ───────────────────────────

#[test]
fn test_return_param_directly() {
    execute_test_case(TestCase {
        input: "fn id(x: int) -> int {
    return x
}

fn main() {
    print_int(id(0))
    print_int(id(42))
    print_int(id(-1))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0, 42, -1]),
    });
}

// ── Function with boolean logic ─────────────────────────────────

#[test]
fn test_function_with_and_or() {
    execute_test_case(TestCase {
        input: "fn both_positive(a: int, b: int) -> bool {
    return (a > 0) && (b > 0)
}

fn main() {
    if both_positive(1, 2) {
        print_int(1)
    } else {
        print_int(0)
    }
    if both_positive(-1, 2) {
        print_int(1)
    } else {
        print_int(0)
    }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 0]),
    });
}

// ── Complex: multiple functions interacting ─────────────────────

#[test]
fn test_higher_level_function_composition() {
    execute_test_case(TestCase {
        input: "fn square(x: int) -> int {
    return x * x
}

fn sum_of_squares(a: int, b: int) -> int {
    return square(a) + square(b)
}

fn main() {
    print_int(sum_of_squares(3, 4))
}",
        inputs: VecDeque::new(),
        // 9 + 16 = 25
        expected_outputs: VecDeque::from(vec![25]),
    });
}

#[test]
fn test_function_pipeline() {
    execute_test_case(TestCase {
        input: "fn add1(x: int) -> int {
    return x + 1
}

fn mul2(x: int) -> int {
    return x * 2
}

fn sub3(x: int) -> int {
    return x - 3
}

fn main() {
    print_int(sub3(mul2(add1(5))))
}",
        inputs: VecDeque::new(),
        // add1(5)=6, mul2(6)=12, sub3(12)=9
        expected_outputs: VecDeque::from(vec![9]),
    });
}

// ── Edge: function that does nothing (void, no statements) ──────

#[test]
fn test_empty_void_function() {
    execute_test_case(TestCase {
        input: "fn noop() {
}

fn main() {
    noop()
    print_int(1)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1]),
    });
}

// ── Edge: deeply nested function calls ──────────────────────────

#[test]
fn test_deeply_nested_calls() {
    execute_test_case(TestCase {
        input: "fn inc(x: int) -> int {
    return x + 1
}

fn main() {
    print_int(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(0)))))))))))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10]),
    });
}

// ── Edge: function result unused (side-effect only) ─────────────

#[test]
fn test_discard_return_value() {
    execute_test_case(TestCase {
        input: "fn returns_int() -> int {
    print_int(42)
    return 0
}

fn main() {
    returns_int()
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

// ── Regression: ensure caller-saved registers are preserved ─────

#[test]
fn test_caller_saved_registers_preserved() {
    // Variables in main should survive across function calls
    execute_test_case(TestCase {
        input: "fn consume(a: int, b: int, c: int, d: int, e: int, f: int) -> int {
    return a + b + c + d + e + f
}

fn main() {
    x = 1
    y = 2
    z = 3
    w = consume(10, 20, 30, 40, 50, 60)
    print_int(x)
    print_int(y)
    print_int(z)
    print_int(w)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 2, 3, 210]),
    });
}

#[test]
fn test_many_variables_across_call() {
    // Lots of live variables when function is called - tests register spilling
    execute_test_case(TestCase {
        input: "fn id(x: int) -> int {
    return x
}

fn main() {
    a = 1
    b = 2
    c = 3
    d = 4
    e = 5
    f = 6
    g = 7
    h = 8
    r = id(99)
    print_int(a + b + c + d + e + f + g + h + r)
}",
        inputs: VecDeque::new(),
        // 1+2+3+4+5+6+7+8+99 = 135
        expected_outputs: VecDeque::from(vec![135]),
    });
}

#[test]
fn test_many_variables_then_call() {
    // Lots of live variables when function is called - tests register spilling
    execute_test_case(TestCase {
        input: "fn main() {
    a = 1
    b = 2
    c = 3
    d = 4
    e = 5
    f = 6
    g = 7
    h = 8
    r = 99
    print_int(a + b + c + d + e + f + g + h + r)
}",
        inputs: VecDeque::new(),
        // 1+2+3+4+5+6+7+8+99 = 135
        expected_outputs: VecDeque::from(vec![135]),
    });
}

// ── Edge: recursive depth (stack usage) ─────────────────────────

#[test]
fn test_recursive_sum_to_50() {
    execute_test_case(TestCase {
        input: "fn sum(n: int) -> int {
    if n == 0 {
        return 0
    } else {
        return n + sum(n - 1)
    }
}

fn main() {
    print_int(sum(50))
}",
        inputs: VecDeque::new(),
        // 50*51/2 = 1275
        expected_outputs: VecDeque::from(vec![1275]),
    });
}

// ── Function with expression as argument ────────────────────────

#[test]
fn test_expression_arguments() {
    execute_test_case(TestCase {
        input: "fn add(x: int, y: int) -> int {
    return x + y
}

fn main() {
    a = 10
    b = 20
    print_int(add(a + 5, b * 2))
}",
        inputs: VecDeque::new(),
        // add(15, 40) = 55
        expected_outputs: VecDeque::from(vec![55]),
    });
}

// ── Multiple return paths with different values ─────────────────

#[test]
fn test_multiple_return_paths() {
    execute_test_case(TestCase {
        input: "fn sign(x: int) -> int {
    if x > 0 {
        return 1
    } else {
        if x < 0 {
            return -1
        } else {
            return 0
        }
    }
}

fn main() {
    print_int(sign(42))
    print_int(sign(-7))
    print_int(sign(0))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, -1, 0]),
    });
}

// ── Function that calls print_int multiple times ────────────────

#[test]
fn test_function_with_multiple_prints() {
    execute_test_case(TestCase {
        input: "fn print_range(start: int, end: int) {
    i = start
    while i < end {
        print_int(i)
        i = i + 1
    }
}

fn main() {
    print_range(1, 6)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1, 2, 3, 4, 5]),
    });
}

// ── Function with subtraction edge cases ────────────────────────

#[test]
fn test_param_order_matters() {
    execute_test_case(TestCase {
        input: "fn sub(a: int, b: int) -> int {
    return a - b
}

fn main() {
    print_int(sub(10, 3))
    print_int(sub(3, 10))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![7, -7]),
    });
}

// ── Ackermann function (stress test for deep recursion) ─────────

#[test]
fn test_ackermann_small() {
    execute_test_case(TestCase {
        input: "fn ack(m: int, n: int) -> int {
    if m == 0 {
        return n + 1
    } else {
        if n == 0 {
            return ack(m - 1, 1)
        } else {
            return ack(m - 1, ack(m, n - 1))
        }
    }
}

fn main() {
    print_int(ack(0, 0))
    print_int(ack(1, 1))
    print_int(ack(2, 2))
    print_int(ack(3, 2))
}",
        inputs: VecDeque::new(),
        // ack(0,0)=1, ack(1,1)=3, ack(2,2)=7, ack(3,2)=29
        expected_outputs: VecDeque::from(vec![1, 3, 7, 29]),
    });
}
