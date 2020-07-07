	.file	"core001.c"
	.section	.rodata
.LC0:
	.string	""
	.text
.globl repStr
	.type	repStr, @function
repStr:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$40, %esp
	movl	$.LC0, -16(%ebp)
	movl	$0, -12(%ebp)
	jmp	.L2
.L3:
	movl	8(%ebp), %eax
	movl	%eax, 4(%esp)
	movl	-16(%ebp), %eax
	movl	%eax, (%esp)
	call	__concat
	movl	%eax, -16(%ebp)
	addl	$1, -12(%ebp)
.L2:
	movl	-12(%ebp), %eax
	cmpl	12(%ebp), %eax
	jl	.L3
	movl	-16(%ebp), %eax
	leave
	ret
	.size	repStr, .-repStr
.globl fac
	.type	fac, @function
fac:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	movl	$1, -8(%ebp)
	movl	8(%ebp), %eax
	movl	%eax, -4(%ebp)
	jmp	.L6
.L7:
	movl	-8(%ebp), %eax
	imull	-4(%ebp), %eax
	movl	%eax, -8(%ebp)
	subl	$1, -4(%ebp)
.L6:
	cmpl	$0, -4(%ebp)
	jg	.L7
	movl	-8(%ebp), %eax
	leave
	ret
	.size	fac, .-fac
.globl rfac
	.type	rfac, @function
rfac:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	cmpl	$0, 8(%ebp)
	jne	.L10
	movl	$1, %eax
	jmp	.L11
.L10:
	movl	8(%ebp), %eax
	subl	$1, %eax
	movl	%eax, (%esp)
	call	rfac
	imull	8(%ebp), %eax
.L11:
	leave
	ret
	.size	rfac, .-rfac
.globl mfac
	.type	mfac, @function
mfac:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	cmpl	$0, 8(%ebp)
	jne	.L14
	movl	$1, %eax
	jmp	.L15
.L14:
	movl	8(%ebp), %eax
	subl	$1, %eax
	movl	%eax, (%esp)
	call	nfac
	imull	8(%ebp), %eax
.L15:
	leave
	ret
	.size	mfac, .-mfac
.globl nfac
	.type	nfac, @function
nfac:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	cmpl	$0, 8(%ebp)
	je	.L18
	movl	8(%ebp), %eax
	subl	$1, %eax
	movl	%eax, (%esp)
	call	mfac
	imull	8(%ebp), %eax
	jmp	.L19
.L18:
	movl	$1, %eax
.L19:
	leave
	ret
	.size	nfac, .-nfac
.globl ifac2f
	.type	ifac2f, @function
ifac2f:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$36, %esp
	movl	8(%ebp), %eax
	cmpl	12(%ebp), %eax
	jne	.L22
	movl	8(%ebp), %eax
	jmp	.L23
.L22:
	movl	8(%ebp), %eax
	cmpl	12(%ebp), %eax
	jle	.L24
	movl	$1, %eax
	jmp	.L23
.L24:
	movl	12(%ebp), %eax
	movl	8(%ebp), %edx
	leal	(%edx,%eax), %eax
	movl	%eax, %edx
	shrl	$31, %edx
	leal	(%edx,%eax), %eax
	sarl	%eax
	movl	%eax, -12(%ebp)
	movl	-12(%ebp), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	ifac2f
	movl	%eax, %ebx
	movl	-12(%ebp), %eax
	leal	1(%eax), %edx
	movl	12(%ebp), %eax
	movl	%eax, 4(%esp)
	movl	%edx, (%esp)
	call	ifac2f
	imull	%ebx, %eax
.L23:
	addl	$36, %esp
	popl	%ebx
	popl	%ebp
	ret
	.size	ifac2f, .-ifac2f
.globl ifac
	.type	ifac, @function
ifac:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	movl	8(%ebp), %eax
	movl	%eax, 4(%esp)
	movl	$1, (%esp)
	call	ifac2f
	leave
	ret
	.size	ifac, .-ifac
	.section	.rodata
.LC1:
	.string	"="
.LC2:
	.string	"hello */"
.LC3:
	.string	"/* world"
	.text
.globl main
	.type	main, @function
main:
	pushl	%ebp
	movl	%esp, %ebp
	andl	$-16, %esp
	subl	$32, %esp
	movl	$10, (%esp)
	call	fac
	movl	%eax, (%esp)
	call	printInt
	movl	$10, (%esp)
	call	rfac
	movl	%eax, (%esp)
	call	printInt
	movl	$10, (%esp)
	call	mfac
	movl	%eax, (%esp)
	call	printInt
	movl	$10, (%esp)
	call	ifac
	movl	%eax, (%esp)
	call	printInt
	movl	$.LC0, 20(%esp)
	movl	$10, 24(%esp)
	movl	$1, 28(%esp)
	jmp	.L29
.L30:
	movl	28(%esp), %eax
	imull	24(%esp), %eax
	movl	%eax, 28(%esp)
	subl	$1, 24(%esp)
.L29:
	cmpl	$0, 24(%esp)
	jg	.L30
	movl	28(%esp), %eax
	movl	%eax, (%esp)
	call	printInt
	movl	$60, 4(%esp)
	movl	$.LC1, (%esp)
	call	repStr
	movl	%eax, (%esp)
	call	printString
	movl	$.LC2, (%esp)
	call	printString
	movl	$.LC3, (%esp)
	call	printString
	movl	$0, %eax
	leave
	ret
	.size	main, .-main
	.ident	"GCC: (Debian 4.4.5-8) 4.4.5"
	.section	.note.GNU-stack,"",@progbits
