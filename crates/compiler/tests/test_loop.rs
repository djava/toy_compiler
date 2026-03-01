mod utils;
use utils::*;

// ── Control flow: while loop ─────────────────────────────────────

#[test]
fn test_while_countdown() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 5
while x > 0 {
    print_int(x)
    x = x - 1
}
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![5, 4, 3, 2, 1]),
    });
}

#[test]
fn test_while_never_enters() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 0
while x > 0 {
    print_int(x)
    x = x - 1
}
print_int(99)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![99]),
    });
}

#[test]
fn test_while_single_iteration() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 1
while x > 0 {
    print_int(x)
    x = x - 1
}
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![1]),
    });
}

#[test]
fn test_while_accumulate_sum() {
    // sum = 1 + 2 + ... + 10 = 55
    execute_test_case(TestCase {
        input: "fn main() -> int { i = 1
sum = 0
while i <= 10 {
    sum = sum + i
    i = i + 1
}
print_int(sum)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![55]),
    });
}

#[test]
fn test_while_with_read() {
    // Sum 3 read_int() values in a loop
    execute_test_case(TestCase {
        input: "fn main() -> int { i = 0
sum = 0
while i < 3 {
    sum = sum + read_int()
    i = i + 1
}
print_int(sum)
}",
        inputs: VecDeque::from(vec![10, 20, 30]),
        expected_outputs: VecDeque::from(vec![60]),
    });
}

#[test]
fn test_nested_while() {
    // Multiplication via nested loops: 3 * 4 = 12
    execute_test_case(TestCase {
        input: "fn main() -> int { i = 0
result = 0
while i < 3 {
    j = 0
    while j < 4 {
        result = result + 1
        j = j + 1
    }
    i = i + 1
}
print_int(result)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![12]),
    });
}

#[test]
fn test_while_variable_survives_after_loop() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 100
i = 5
while i > 0 {
    i = i - 1
}
print_int(x)
print_int(i)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![100, 0]),
    });
}

// ── Interaction: loops + variables + reads ────────────────────────

#[test]
fn test_repeated_multiply_by_nine() {
    // result = result * 9 via repeated addition, n times
    // n=3: 1 -> 9 -> 81 -> 729
    execute_test_case(TestCase {
        input: "fn main() -> int { n = read_int()
result = 1
i = 1
while i <= n {
    result = result + result + result + result + result + result + result + result + result
    i = i + 1
}
print_int(result)
}",
        inputs: VecDeque::from(vec![3]),
        expected_outputs: VecDeque::from(vec![729]),
    });
}

#[test]
fn test_fibonacci() {
    // fib(7) = 13; sequence: 0, 1, 1, 2, 3, 5, 8, 13
    execute_test_case(TestCase {
        input: "fn main() -> int { a = 0
b = 1
i = 0
while i < 7 {
    temp = b
    b = a + b
    a = temp
    i = i + 1
}
print_int(a)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![13]),
    });
}

#[test]
fn test_if_inside_while() {
    // Prints i for i in {0, 2, 4}
    execute_test_case(TestCase {
        input: "fn main() -> int { i = 0
while i < 6 {
    if i == 0 { print_int(i) }
    else { if i == 2 { print_int(i) }
    else { if i == 4 { print_int(i) } } }
    i = i + 1
}
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![0, 2, 4]),
    });
}

#[test]
fn test_while_modifies_condition_variable() {
    execute_test_case(TestCase {
        input: "fn main() -> int { x = 10
while x > 0 {
    x = x - 3
}
print_int(x)
}",
        inputs: VecDeque::new(),
        // 10 -> 7 -> 4 -> 1 -> -2
        expected_outputs: VecDeque::from(vec![-2]),
    });
}

// ── Control flow: for loop ────────────────────────────────────────

#[test]
fn test_for_countdown() {
    execute_test_case(TestCase {
        input: "fn main() -> int { for (x = 3; x > 0; x = x - 1) {
    print_int(x)
}
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![3, 2, 1]),
    });
}

#[test]
fn test_for_never_enters() {
    execute_test_case(TestCase {
        input: "fn main() -> int { for (i = 5; i < 5; i = i + 1) {
    print_int(i)
}
print_int(99)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![99]),
    });
}

#[test]
fn test_for_accumulate_sum() {
    // sum = 1 + 2 + ... + 10 = 55
    execute_test_case(TestCase {
        input: "fn main() -> int { sum = 0
for (i = 1; i <= 10; i = i + 1) {
    sum = sum + i
}
print_int(sum)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![55]),
    });
}

#[test]
fn test_for_single_iteration() {
    execute_test_case(TestCase {
        input: "fn main() -> int { for (i = 0; i < 1; i = i + 1) {
    print_int(42)
}
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![42]),
    });
}

#[test]
fn test_for_variable_survives_after_loop() {
    execute_test_case(TestCase {
        input: "fn main() -> int { for (i = 0; i < 3; i = i + 1) {
    print_int(i)
}
print_int(i)
}",
        inputs: VecDeque::new(),
        // i is 3 after the loop exits
        expected_outputs: VecDeque::from(vec![0, 1, 2, 3]),
    });
}

#[test]
fn test_for_with_read() {
    execute_test_case(TestCase {
        input: "fn main() -> int { n = read_int()
sum = 0
for (i = 1; i <= n; i = i + 1) {
    sum = sum + i
}
print_int(sum)
}",
        inputs: VecDeque::from(vec![5]),
        // 1+2+3+4+5 = 15
        expected_outputs: VecDeque::from(vec![15]),
    });
}

#[test]
fn test_nested_for() {
    // 3 * 4 = 12 iterations
    execute_test_case(TestCase {
        input: "fn main() -> int { result = 0
for (i = 0; i < 3; i = i + 1) {
    for (j = 0; j < 4; j = j + 1) {
        result = result + 1
    }
}
print_int(result)
}",
        inputs: VecDeque::new(),
        expected_outputs: VecDeque::from(vec![12]),
    });
}

#[test]
fn test_for_and_while_mixed() {
    execute_test_case(TestCase {
        input: "fn main() -> int { for (i = 0; i < 3; i = i + 1) {
    j = i
    while j > 0 {
        print_int(j)
        j = j - 1
    }
}
}",
        inputs: VecDeque::new(),
        // i=0: nothing; i=1: 1; i=2: 2, 1
        expected_outputs: VecDeque::from(vec![1, 2, 1]),
    });
}
