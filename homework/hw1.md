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

# Problem 3
```
     Running tests\L_int_test.rs (target\debug\deps\L_int_test-c0257f89c135747f.exe)

running 6 tests
test test_partial_eval_add ... 
==================
AST before Partial Eval: Expr(Call("print", [BinaryOp(Constant(I64(40)), Add, Constant(I64(2)))]))
AST after Partial Eval: Expr(Call("print", [Constant(I64(42))]))
ok
test test_partial_eval_input ... 
==================
AST before Partial Eval: Expr(Call("print", [Call("input_int", [])]))
AST after Partial Eval: Expr(Call("print", [Call("input_int", [])]))
ok
test test_partial_eval_mixed ... 
==================
AST before Partial Eval: Expr(Call("print", [BinaryOp(BinaryOp(Call("input_int", []), Add, Constant(I64(2))), Add, BinaryOp(Constant(I64(40)), Add, Constant(I64(2))))]))
AST after Partial Eval: Expr(Call("print", [BinaryOp(BinaryOp(Call("input_int", []), Add, Constant(I64(2))), Add, Constant(I64(42)))]))
ok
test test_partial_eval_nested ... 
==================
AST before Partial Eval: Expr(Call("print", [BinaryOp(BinaryOp(Constant(I64(40)), Add, Constant(I64(2))), Add, BinaryOp(Constant(I64(40)), Add, Constant(I64(2))))]))    
AST after Partial Eval: Expr(Call("print", [Constant(I64(84))]))
ok
test test_partial_eval_subinput ...
==================
AST before Partial Eval: Expr(Call("print", [BinaryOp(Call("input_int", []), Subtract, Call("input_int", []))]))
AST after Partial Eval: Expr(Call("print", [BinaryOp(Call("input_int", []), Subtract, Call("input_int", []))]))
ok
test test_partial_eval_zero ...
==================
AST before Partial Eval: Expr(Call("print", [Constant(I64(0))]))
AST after Partial Eval: Expr(Call("print", [Constant(I64(0))]))
ok

test result: ok. 6 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.01s
```