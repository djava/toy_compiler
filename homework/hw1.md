# Problem 1
```
Running tests\L_int_test.rs (target\debug\deps\L_int_test-c0257f89c135747f.exe)

running 4 tests
test test_partial_eval_subinput ... ok
test test_partial_eval_input ... ok
test test_partial_eval_zero ... ok
test test_partial_eval_add ... ok

test result: ok. 4 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s
```
.. The tests DO pass - the assignment says they shouldn't, and I'm not
sure why not. If you're just returning the unmodified AST then it would
like, still work in the interpreter, right? Just not optimized.

# Problem 2
- Tests where partial-eval *does* modify the syntactic form:
    - Add
- Tests where partial-eval *does not* modify the syntactic form:
    - Input (because the input isn't known at compile-time)
    - Sub-Input (because the input isn't known at compile-time)
    - Zero (because there's nothing to simplify)
- Test-cases that aren't hit:
    - Recursive evaluation: ie nested expressions
    - int_input() + Constant