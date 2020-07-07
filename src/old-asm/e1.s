.globl main
  .type main, @function
main:
  pushl %ebp
  movl %esp, %ebp
  lea -4(%ebp), %esp
  mov $10, %eax
  push %eax
  call printInt
# Ret
#####
  mov $0, %eax
  mov %ebp, %esp
  pop %ebp
  ret


