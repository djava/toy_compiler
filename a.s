	.att_syntax
	.align 8
a:
	movq $1, %rdi
	callq print_int
	retq

	.align 8
b:
	movq $2, %rdi
	callq print_int
	retq

	.align 8
	.globl main
main:
	pushq %rbx
	pushq %r12
	pushq %r15
	movq $524288, %rdi
	movq $524288, %rsi
	callq __gc_initialize
	movq __gc_rootstack_begin(%rip), %r15
	movq $0, %rbx
	jmp .EE_8
.EE_8:
	cmpq $1000, %rbx
	jl .EE_11
	popq %r15
	popq %r12
	popq %rbx
	retq
.EE_9:
	movq __gc_free_ptr(%rip), %r11
	addq $408, __gc_free_ptr(%rip)
	movq $100, 0(%r11)
	movq %r11, %r12
	movq %r12, %r11
	movq $1, 8(%r11)
	movq %r12, %r11
	movq $1, 16(%r11)
	movq %r12, %r11
	movq $1, 24(%r11)
	movq %r12, %r11
	movq $1, 32(%r11)
	movq %r12, %r11
	movq $1, 40(%r11)
	movq %r12, %r11
	movq $1, 48(%r11)
	movq %r12, %r11
	movq $1, 56(%r11)
	movq %r12, %r11
	movq $1, 64(%r11)
	movq %r12, %r11
	movq $1, 72(%r11)
	movq %r12, %r11
	movq $1, 80(%r11)
	movq %r12, %r11
	movq $1, 88(%r11)
	movq %r12, %r11
	movq $1, 96(%r11)
	movq %r12, %r11
	movq $1, 104(%r11)
	movq %r12, %r11
	movq $1, 112(%r11)
	movq %r12, %r11
	movq $1, 120(%r11)
	movq %r12, %r11
	movq $1, 128(%r11)
	movq %r12, %r11
	movq $1, 136(%r11)
	movq %r12, %r11
	movq $1, 144(%r11)
	movq %r12, %r11
	movq $1, 152(%r11)
	movq %r12, %r11
	movq $1, 160(%r11)
	movq %r12, %r11
	movq $1, 168(%r11)
	movq %r12, %r11
	movq $1, 176(%r11)
	movq %r12, %r11
	movq $1, 184(%r11)
	movq %r12, %r11
	movq $1, 192(%r11)
	movq %r12, %r11
	movq $1, 200(%r11)
	movq %r12, %r11
	movq $1, 208(%r11)
	movq %r12, %r11
	movq $1, 216(%r11)
	movq %r12, %r11
	movq $1, 224(%r11)
	movq %r12, %r11
	movq $1, 232(%r11)
	movq %r12, %r11
	movq $1, 240(%r11)
	movq %r12, %r11
	movq $1, 248(%r11)
	movq %r12, %r11
	movq $1, 256(%r11)
	movq %r12, %r11
	movq $1, 264(%r11)
	movq %r12, %r11
	movq $1, 272(%r11)
	movq %r12, %r11
	movq $1, 280(%r11)
	movq %r12, %r11
	movq $1, 288(%r11)
	movq %r12, %r11
	movq $1, 296(%r11)
	movq %r12, %r11
	movq $1, 304(%r11)
	movq %r12, %r11
	movq $1, 312(%r11)
	movq %r12, %r11
	movq $1, 320(%r11)
	movq %r12, %r11
	movq $1, 328(%r11)
	movq %r12, %r11
	movq $1, 336(%r11)
	movq %r12, %r11
	movq $1, 344(%r11)
	movq %r12, %r11
	movq $1, 352(%r11)
	movq %r12, %r11
	movq $1, 360(%r11)
	movq %r12, %r11
	movq $1, 368(%r11)
	movq %r12, %r11
	movq $1, 376(%r11)
	movq %r12, %r11
	movq $1, 384(%r11)
	movq %r12, %r11
	movq $1, 392(%r11)
	movq %r12, %r11
	movq $1, 400(%r11)
	addq $1, %rbx
	jmp .EE_8
.EE_11:
	movq __gc_free_ptr(%rip), %rcx
	addq $408, %rcx
	cmpq __gc_fromspace_end(%rip), %rcx
	jl .EE_9
	movq $408, %rdi
	callq __gc_collect
	jmp .EE_9

	.section .note.GNU-stack,"",@progbits
