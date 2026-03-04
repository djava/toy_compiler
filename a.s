	.att_syntax
	.align 8
	.globl main
main:
	pushq %r15
	pushq %r14
	pushq %r12
	pushq %r13
	pushq %rbx
	movq $32768, %rdi
	movq $32768, %rsi
	callq __gc_initialize
	movq __gc_rootstack_begin(%rip), %r15
	movq %rdi, %rbx
	movq __gc_free_ptr(%rip), %rcx
	addq $88, %rcx
	cmpq __gc_fromspace_end(%rip), %rcx
	jl .EE_26
	movq $88, %rdi
	callq __gc_collect
.EE_26:
	movq __gc_free_ptr(%rip), %r11
	addq $88, __gc_free_ptr(%rip)
	movq $4611686018427387944, %rax
	movq %rax, 0(%r11)
	movq %r11, %rbx
	movq %rbx, %r11
	movq $1, 8(%r11)
	movq %rbx, %r11
	movq $2, 16(%r11)
	movq %rbx, %r11
	movq $3, 24(%r11)
	movq %rbx, %r11
	movq $4, 32(%r11)
	movq %rbx, %r11
	movq $5, 40(%r11)
	movq %rbx, %r11
	movq $0, 48(%r11)
	movq %rbx, %r11
	movq $0, 56(%r11)
	movq %rbx, %r11
	movq $0, 64(%r11)
	movq %rbx, %r11
	movq $0, 72(%r11)
	movq %rbx, %r11
	movq $0, 80(%r11)
	movq __gc_free_ptr(%rip), %rcx
	addq $88, %rcx
	cmpq __gc_fromspace_end(%rip), %rcx
	jl .EE_24
	movq $88, %rdi
	callq __gc_collect
.EE_24:
	movq __gc_free_ptr(%rip), %r11
	addq $88, __gc_free_ptr(%rip)
	movq $4611686018427387944, %rax
	movq %rax, 0(%r11)
	movq %r11, %r13
	movq %r13, %r11
	movq $0, 8(%r11)
	movq %r13, %r11
	movq $0, 16(%r11)
	movq %r13, %r11
	movq $0, 24(%r11)
	movq %r13, %r11
	movq $0, 32(%r11)
	movq %r13, %r11
	movq $0, 40(%r11)
	movq %r13, %r11
	movq $6, 48(%r11)
	movq %r13, %r11
	movq $7, 56(%r11)
	movq %r13, %r11
	movq $8, 64(%r11)
	movq %r13, %r11
	movq $9, 72(%r11)
	movq %r13, %r11
	movq $10, 80(%r11)
	movq $0, %r12
.EE_16:
	movq %rbx, %rdi
	callq len
	movq %rax, %rcx
	cmpq %rcx, %r12
	jl .EE_23
	movq $0, %r12
.EE_13:
	movq %rbx, %rdi
	callq len
	movq %rax, %rcx
	cmpq %rcx, %r12
	jl .EE_14
	popq %rbx
	popq %r13
	popq %r12
	popq %r14
	popq %r15
	retq
.EE_23:
	cmpq $5, %r12
	jl .EE_21
	movq %rbx, %r14
.EE_20:
	cmpq $5, %r12
	jl .EE_18
	movq %r13, %rdi
.EE_17:
	movq %r12, %rsi
	callq __subscript_array
	movq %rax, %rdx
	movq %r14, %rdi
	movq %r12, %rsi
	callq __assign_to_array_elem
	addq $1, %r12
	jmp .EE_16
.EE_14:
	movq %rbx, %rdi
	movq %r12, %rsi
	callq __subscript_array
	movq %rax, %rdi
	callq print_int
	movq %r13, %rdi
	movq %r12, %rsi
	callq __subscript_array
	movq %rax, %rdi
	callq print_int
	addq $1, %r12
	jmp .EE_13
.EE_21:
	movq %r13, %r14
	jmp .EE_20
.EE_18:
	movq %rbx, %rdi
	jmp .EE_17

	.section .note.GNU-stack,"",@progbits
