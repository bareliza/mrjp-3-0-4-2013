String$1$1:
  .string ""
String$2$17:
  .string "1:"
String$4$17:
  .string "2:"
.globl a
  .type a, @function
a:
  pushl %ebp
  movl %esp, %ebp
  lea -4(%ebp), %esp
  mov $String$2$17, %eax
  push %eax
  call printString
  add $4, %esp
  mov 8(%ebp), %eax   # eax=a
  push %eax
  call printInt
  add $4, %esp
  mov $String$4$17, %eax
  push %eax
  call printString
  add $4, %esp
  mov 12(%ebp), %eax   # eax=b
  push %eax
  call printInt
  add $4, %esp
  mov %ebp, %esp
  pop %ebp
  ret


.globl main
  .type main, @function
main:
  pushl %ebp
  movl %esp, %ebp
  lea -4(%ebp), %esp
  mov $1, %eax
  push %eax
  mov $10, %eax
  push %eax
  call a
  add $8, %esp
  mov $10, %eax
  push %eax
  mov $1, %eax
  push %eax
  call a
  add $8, %esp
# Ret
#####
  mov $0, %eax
  mov %ebp, %esp
  pop %ebp
  ret
  mov %ebp, %esp
  pop %ebp
  ret


