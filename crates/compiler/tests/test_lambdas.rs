mod utils;
use utils::*;

// ── Basic lambda: the canonical a.bob example ───────────────────

#[test]
fn test_lambda_captures_two_vars() {
    // Straight from a.bob: lambda captures a and b from enclosing scope
    execute_test_case(TestCase {
        input: "fn main() {
    a = 1
    b = 2
    x: callable<[int], int> = lambda y: a + b + y
    print_int(x(3))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![6]),
    });
}

// ── No-capture lambda ────────────────────────────────────────────

#[test]
fn test_lambda_no_captures() {
    execute_test_case(TestCase {
        input: "fn main() {
    f: callable<[int], int> = lambda x: x * x
    print_int(f(5))
    print_int(f(3))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![25, 9]),
    });
}

#[test]
fn test_lambda_no_captures_constant_body() {
    execute_test_case(TestCase {
        input: "fn main() {
    f: callable<[int], int> = lambda x: 42
    print_int(f(0))
    print_int(f(99))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42, 42]),
    });
}

// ── Two-parameter lambda ─────────────────────────────────────────

#[test]
fn test_lambda_two_params() {
    execute_test_case(TestCase {
        input: "fn main() {
    add: callable<[int, int], int> = lambda x, y: x + y
    print_int(add(10, 32))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

#[test]
fn test_lambda_two_params_multiple_calls() {
    execute_test_case(TestCase {
        input: "fn main() {
    mul: callable<[int, int], int> = lambda x, y: x * y
    print_int(mul(2, 3))
    print_int(mul(5, 7))
    print_int(mul(10, 10))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![6, 35, 100]),
    });
}

// ── Capturing multiple variables ─────────────────────────────────

#[test]
fn test_lambda_captures_three_vars() {
    execute_test_case(TestCase {
        input: "fn main() {
    a = 5
    b = 10
    c = 20
    f: callable<[int], int> = lambda x: a + b + c + x
    print_int(f(7))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

#[test]
fn test_lambda_captures_and_ignores_param() {
    // Lambda captures outer var, param is not used
    execute_test_case(TestCase {
        input: "fn main() {
    n = 100
    f: callable<[int], int> = lambda x: n
    print_int(f(0))
    print_int(f(999))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![100, 100]),
    });
}

// ── Lambda called multiple times ─────────────────────────────────

#[test]
fn test_lambda_called_in_loop() {
    execute_test_case(TestCase {
        input: "fn main() {
    f: callable<[int], int> = lambda x: x + 1
    x = 0
    i = 0
    while i < 5 {
        x = f(x)
        i = i + 1
    }
    print_int(x)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![5]),
    });
}

// ── Lambda as argument to a higher-order function ────────────────

#[test]
fn test_lambda_passed_as_argument() {
    execute_test_case(TestCase {
        input: "fn apply(f: callable<[int], int>, x: int) -> int {
    return f(x)
}

fn main() {
    double: callable<[int], int> = lambda x: x + x
    print_int(apply(double, 21))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

#[test]
fn test_lambda_and_named_fn_passed_to_same_function() {
    execute_test_case(TestCase {
        input: "fn negate(x: int) -> int {
    return 0 - x
}

fn apply(f: callable<[int], int>, x: int) -> int {
    return f(x)
}

fn main() {
    inc: callable<[int], int> = lambda x: x + 1
    print_int(apply(negate, 10))
    print_int(apply(inc, 10))
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![-10, 11]),
    });
}

// ── Lambda capturing a variable read from stdin ──────────────────

#[test]
fn test_lambda_captures_runtime_value() {
    execute_test_case(TestCase {
        input: "fn main() {
    base = read_int()
    f: callable<[int], int> = lambda x: base + x
    print_int(f(1))
    print_int(f(2))
    print_int(f(3))
}",
        inputs: VecDeque::from(vec![10]),
        expected_outputs: VecDeque::from(vec![11, 12, 13]),
    });
}

// ── Lambda with arithmetic expressions ──────────────────────────

#[test]
fn test_lambda_arithmetic_body() {
    execute_test_case(TestCase {
        input: "fn main() {
    offset = 3
    f: callable<[int], int> = lambda x: x * x + offset
    print_int(f(2))
    print_int(f(4))
}",
        inputs: VecDeque::new(),
        // 2*2 + 3 = 7, 4*4 + 3 = 19
        expected_outputs: VecDeque::from(vec![7, 19]),
    });
}

// ── Lambda assigned in a conditional branch ──────────────────────

#[test]
fn test_lambda_assigned_in_conditional() {
    // Lambda created inside a branch — the captured var differs per branch
    execute_test_case(TestCase {
        input: "fn main() {
    flag = read_int()
    a = 10
    b = 20
    if flag > 0 {
        f: callable<[int], int> = lambda x: a + x
        print_int(f(5))
    } else {
        f: callable<[int], int> = lambda x: b + x
        print_int(f(5))
    }
}",
        inputs: VecDeque::from(vec![1]),
        expected_outputs: VecDeque::from(vec![15]),
    });
}

#[test]
fn test_lambda_assigned_in_conditional_negative_branch() {
    execute_test_case(TestCase {
        input: "fn main() {
    flag = read_int()
    a = 10
    b = 20
    if flag > 0 {
        f: callable<[int], int> = lambda x: a + x
        print_int(f(5))
    } else {
        f: callable<[int], int> = lambda x: b + x
        print_int(f(5))
    }
}",
        inputs: VecDeque::from(vec![-1]),
        expected_outputs: VecDeque::from(vec![25]),
    });
}

// ── Multiline lambda (block body) ───────────────────────────────

#[test]
fn test_lambda_multiline_body() {
    execute_test_case(TestCase {
        input: "fn main() {
    offset = 7
    f: callable<[int], int> = lambda x: {
        y = x * 2
        return y + offset
    }
    print_int(f(3))
    print_int(f(10))
}",
        inputs: VecDeque::new(),
        // 3*2 + 7 = 13, 10*2 + 7 = 27
        expected_outputs: VecDeque::from(vec![13, 27]),
    });
}

#[test]
fn test_lambda_multiline_no_captures() {
    execute_test_case(TestCase {
        input: "fn main() {
    f: callable<[int, int], int> = lambda x, y: {
        a = x + y
        b = a * a
        return b
    }
    print_int(f(2, 3))
    print_int(f(1, 4))
}",
        inputs: VecDeque::new(),
        // (2+3)^2 = 25, (1+4)^2 = 25
        expected_outputs: VecDeque::from(vec![25, 25]),
    });
}

// ── Lambda capturing a loop variable ────────────────────────────

#[test]
fn test_lambda_captures_loop_variable() {
    // Each iteration creates a new closure capturing the current i
    execute_test_case(TestCase {
        input: "fn apply(f: callable<[int], int>, x: int) -> int {
    return f(x)
}

fn main() {
    i = 0
    while i < 3 {
        f: callable<[int], int> = lambda x: x + i
        print_int(apply(f, 10))
        i = i + 1
    }
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![10, 11, 12]),
    });
}
