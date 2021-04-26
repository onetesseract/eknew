	.text
	.file	"ex.txt"
	.globl	pp                              # -- Begin function pp
	.p2align	4, 0x90
	.type	pp,@function
pp:                                     # @pp
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	printd
	movq	%rbx, %rdi
	callq	printi
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	pp, .Lfunc_end0-pp
	.cfi_endproc
                                        # -- End function
	.globl	v                               # -- Begin function v
	.p2align	4, 0x90
	.type	v,@function
v:                                      # @v
	.cfi_startproc
# %bb.0:                                # %entry
	movl	$1, %eax
	retq
.Lfunc_end1:
	.size	v, .Lfunc_end1-v
	.cfi_endproc
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3                               # -- Begin function neonmain
.LCPI2_0:
	.quad	0x4022000000000000              # double 9
.LCPI2_1:
	.quad	0x4021cccccccccccd              # double 8.9000000000000004
	.text
	.globl	neonmain
	.p2align	4, 0x90
	.type	neonmain,@function
neonmain:                               # @neonmain
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$9, %edi
	callq	printi
	movsd	.LCPI2_0(%rip), %xmm0           # xmm0 = mem[0],zero
	movl	$8, %edi
	callq	pp
	movsd	.LCPI2_1(%rip), %xmm0           # xmm0 = mem[0],zero
	callq	v
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	neonmain, .Lfunc_end2-neonmain
	.cfi_endproc
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
