	.text
	.file	"examples/manually.bit"
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
	movl	$1, %edi
	movl	$2, %esi
	callq	myadd
	xorl	%eax, %eax
	popq	%rcx
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
