String$1$1:
  .string ""
.globl main
  .type main, @function
main:
  pushl %ebp
  movl %esp, %ebp
  lea -4(%ebp), %esp
  mov $2, %eax
  push %eax
  call suma
  add $4, %esp
  push %eax
  call printInt
  add $4, %esp
# Ret
#####
  mov $0, %eax
  mov %ebp, %esp
  pop %ebp
  ret
  mov %ebp, %esp
  pop %ebp
  ret


.globl suma
  .type suma, @function
suma:
  pushl %ebp
  movl %esp, %ebp
  lea -8(%ebp), %esp
# CondElse
##########
  mov $0, %eax
  mov %eax, -4(%ebp)   # tmp0=eax
  mov 8(%ebp), %eax   # eax=i
  cmp -4(%ebp), %eax   # e2, e1
  setg %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $1, %eax
  jne ifElse0
# Ret
#####
  mov $1, %eax
  mov %eax, -4(%ebp)   # tmp0=eax
  mov 8(%ebp), %eax   # eax=i
  sub -4(%ebp), %eax   # eax=e1-e2
  push %eax
  call suma
  add $4, %esp
  mov %eax, -4(%ebp)   # tmp0=eax
  mov 8(%ebp), %eax   # eax=i
  add -4(%ebp), %eax   # eax=e1+e2
  mov %ebp, %esp
  pop %ebp
  ret
  jmp ifEnd0
ifElse0:
# Ret
#####
  mov $0, %eax
  mov %ebp, %esp
  pop %ebp
  ret
ifEnd0:
  mov %ebp, %esp
  pop %ebp
  ret


