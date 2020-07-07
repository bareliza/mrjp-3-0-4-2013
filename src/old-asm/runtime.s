	.file	"runtime.c"
	.section	.rodata
.LC0:
	.string	"%d\n"
	.text
.globl printInt
	.type	printInt, @function
printInt:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	movl	$.LC0, %eax
	movl	8(%ebp), %edx
	movl	%edx, 4(%esp)
	movl	%eax, (%esp)
	call	printf
	leave
	ret
	.size	printInt, .-printInt
.globl printString
	.type	printString, @function
printString:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	puts
	leave
	ret
	.size	printString, .-printString
.globl readString
	.type	readString, @function
readString:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$40, %esp
	movl	$0, -12(%ebp)
	movl	stdin, %eax
	movl	%eax, 8(%esp)
	leal	-16(%ebp), %eax
	movl	%eax, 4(%esp)
	leal	-12(%ebp), %eax
	movl	%eax, (%esp)
	call	getline
	movl	-12(%ebp), %eax
	leave
	ret
	.size	readString, .-readString
.globl readInt
	.type	readInt, @function
readInt:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$40, %esp
	movl	$0, -16(%ebp)
	movl	stdin, %eax
	movl	%eax, 8(%esp)
	leal	-20(%ebp), %eax
	movl	%eax, 4(%esp)
	leal	-16(%ebp), %eax
	movl	%eax, (%esp)
	call	getline
	movl	-16(%ebp), %eax
	movl	%eax, (%esp)
	call	atoi
	movl	%eax, -12(%ebp)
	movl	-16(%ebp), %eax
	movl	%eax, (%esp)
	call	free
	movl	-12(%ebp), %eax
	leave
	ret
	.size	readInt, .-readInt
	.section	.rodata
.LC1:
	.string	"runtime error"
	.text
.globl error
	.type	error, @function
error:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	movl	$.LC1, (%esp)
	call	puts
	movl	$1, (%esp)
	call	exit
	.size	error, .-error
.globl __concat
	.type	__concat, @function
__concat:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$36, %esp
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	strlen
	movl	%eax, %ebx
	movl	12(%ebp), %eax
	movl	%eax, (%esp)
	call	strlen
	leal	(%ebx,%eax), %eax
	addl	$1, %eax
	movl	%eax, (%esp)
	call	malloc
	movl	%eax, -12(%ebp)
	movl	8(%ebp), %eax
	movl	%eax, 4(%esp)
	movl	-12(%ebp), %eax
	movl	%eax, (%esp)
	call	strcpy
	movl	12(%ebp), %eax
	movl	%eax, 4(%esp)
	movl	-12(%ebp), %eax
	movl	%eax, (%esp)
	call	strcat
	movl	-12(%ebp), %eax
	addl	$36, %esp
	popl	%ebx
	popl	%ebp
	ret
	.size	__concat, .-__concat
	.ident	"GCC: (Debian 4.4.5-8) 4.4.5"
	.section	.note.GNU-stack,"",@progbits
