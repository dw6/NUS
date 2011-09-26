	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.align	4, 0x90
_main:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	movl	$1, -8(%ebp)
	jmp	LBB1_2
LBB1_1:
	movl	-8(%ebp), %eax
	subl	$1, %eax
	movl	%eax, -8(%ebp)
LBB1_2:
	movl	-8(%ebp), %eax
	cmpl	$0, %eax
	jg	LBB1_1
	movl	-4(%ebp), %eax
	addl	$8, %esp
	popl	%ebp
	ret


.subsections_via_symbols
