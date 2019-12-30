	.text
	.file	"binary"
	.globl	myadd
	.align	16, 0x90
	.type	myadd,@function
myadd:                                  # @myadd
	.cfi_startproc
# BB#0:                                 # %entry
	leal	(%rdi,%rsi), %eax
	retq
.Lfunc_end0:
	.size	myadd, .Lfunc_end0-myadd
	.cfi_endproc

	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:
	pushq	%rax
.Ltmp0:
	.cfi_def_cfa_offset 16
	movl	$.L.str, %edi
	callq	puts
	xorl	%eax, %eax
	popq	%rcx
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"hello world\n"
	.size	.L.str, 13


	.section	".note.GNU-stack","",@progbits
