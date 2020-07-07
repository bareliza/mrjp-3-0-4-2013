String$1$1:
  .string ""
String$16$29:
  .string "="
String$17$22:
  .string "hello */"
String$18$22:
  .string "/* world"
String$69$14:
  .string ""
.globl main
  .type main, @function
main:
  pushl %ebp
  movl %esp, %ebp
  lea -20(%ebp), %esp
  mov $10, %eax
  push %eax
  call fac
  add $4, %esp
  push %eax
  call printInt
  add $4, %esp
  mov $10, %eax
  push %eax
  call rfac
  add $4, %esp
  push %eax
  call printInt
  add $4, %esp
  mov $2, %eax
  push %eax
  call mfac
  add $4, %esp
  push %eax
  call printInt
  add $4, %esp
  mov $2, %eax
  push %eax
  call ifac
  add $4, %esp
  push %eax
  call printInt
  add $4, %esp
# Ass
#####
  mov $String$1$1, %eax
  mov %eax, -16(%ebp)   # tmp0= expr. value 
  lea -4(%ebp), %eax   # eax=adres r
  mov -16(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
  mov $10, %eax
  mov %eax, -8(%ebp)   # n=eax
  mov $1, %eax
  mov %eax, -12(%ebp)   # r=eax
# While
#######
  jmp whileCond0
whileLoop0:
# Ass
#####
  mov -8(%ebp), %eax   # eax=n
  mov %eax, -16(%ebp)   # tmp0=eax
  mov -12(%ebp), %eax   # eax=r
  imul -16(%ebp), %eax   # eax=e1*e2
  mov %eax, -16(%ebp)   # tmp0= expr. value 
  lea -12(%ebp), %eax   # eax=adres r
  mov -16(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
# Decr
######
  lea -8(%ebp), %eax   # eax=adres n
  decl (%eax)
whileCond0:
  mov $0, %eax
  mov %eax, -16(%ebp)   # tmp0=eax
  mov -8(%ebp), %eax   # eax=n
  cmp -16(%ebp), %eax   # e2, e1
  setg %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $1, %eax
  je whileLoop0
  mov -12(%ebp), %eax   # eax=r
  push %eax
  call printInt
  add $4, %esp
  mov $2, %eax
  push %eax
  mov $String$16$29, %eax
  push %eax
  call repStr
  add $8, %esp
  push %eax
  call printString
  add $4, %esp
  mov $String$17$22, %eax
  push %eax
  call printString
  add $4, %esp
  mov $String$18$22, %eax
  push %eax
  call printString
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


.globl fac
  .type fac, @function
fac:
  pushl %ebp
  movl %esp, %ebp
  lea -16(%ebp), %esp
# Ass
#####
  mov $0, %eax
  mov %eax, -12(%ebp)   # tmp0= expr. value 
  lea -4(%ebp), %eax   # eax=adres r
  mov -12(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
# Ass
#####
  mov $0, %eax
  mov %eax, -12(%ebp)   # tmp0= expr. value 
  lea -8(%ebp), %eax   # eax=adres n
  mov -12(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
# Ass
#####
  mov $1, %eax
  mov %eax, -12(%ebp)   # tmp0= expr. value 
  lea -4(%ebp), %eax   # eax=adres r
  mov -12(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
# Ass
#####
  mov 8(%ebp), %eax   # eax=a
  mov %eax, -12(%ebp)   # tmp0= expr. value 
  lea -8(%ebp), %eax   # eax=adres n
  mov -12(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
# While
#######
  jmp whileCond1
whileLoop1:
# Ass
#####
  mov -8(%ebp), %eax   # eax=n
  mov %eax, -12(%ebp)   # tmp0=eax
  mov -4(%ebp), %eax   # eax=r
  imul -12(%ebp), %eax   # eax=e1*e2
  mov %eax, -12(%ebp)   # tmp0= expr. value 
  lea -4(%ebp), %eax   # eax=adres r
  mov -12(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
# Ass
#####
  mov $1, %eax
  mov %eax, -12(%ebp)   # tmp0=eax
  mov -8(%ebp), %eax   # eax=n
  sub -12(%ebp), %eax   # eax=e1-e2
  mov %eax, -12(%ebp)   # tmp0= expr. value 
  lea -8(%ebp), %eax   # eax=adres n
  mov -12(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
whileCond1:
  mov $0, %eax
  mov %eax, -12(%ebp)   # tmp0=eax
  mov -8(%ebp), %eax   # eax=n
  cmp -12(%ebp), %eax   # e2, e1
  setg %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $1, %eax
  je whileLoop1
# Ret
#####
  mov -4(%ebp), %eax   # eax=r
  mov %ebp, %esp
  pop %ebp
  ret
  mov %ebp, %esp
  pop %ebp
  ret


.globl rfac
  .type rfac, @function
rfac:
  pushl %ebp
  movl %esp, %ebp
  lea -8(%ebp), %esp
# CondElse
##########
  mov $0, %eax
  mov %eax, -4(%ebp)   # tmp0=eax
  mov 8(%ebp), %eax   # eax=n
  cmp -4(%ebp), %eax   # e2, e1
  sete %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $1, %eax
  jne ifElse2
# Ret
#####
  mov $1, %eax
  mov %ebp, %esp
  pop %ebp
  ret
  jmp ifEnd2
ifElse2:
# Ret
#####
  mov $1, %eax
  mov %eax, -4(%ebp)   # tmp0=eax
  mov 8(%ebp), %eax   # eax=n
  sub -4(%ebp), %eax   # eax=e1-e2
  push %eax
  call rfac
  add $4, %esp
  mov %eax, -4(%ebp)   # tmp0=eax
  mov 8(%ebp), %eax   # eax=n
  imul -4(%ebp), %eax   # eax=e1*e2
  mov %ebp, %esp
  pop %ebp
  ret
ifEnd2:
  mov %ebp, %esp
  pop %ebp
  ret


.globl mfac
  .type mfac, @function
mfac:
  pushl %ebp
  movl %esp, %ebp
  lea -8(%ebp), %esp
# CondElse
##########
  mov $0, %eax
  mov %eax, -4(%ebp)   # tmp0=eax
  mov 8(%ebp), %eax   # eax=n
  cmp -4(%ebp), %eax   # e2, e1
  sete %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $1, %eax
  jne ifElse3
# Ret
#####
  mov $1, %eax
  mov %ebp, %esp
  pop %ebp
  ret
  jmp ifEnd3
ifElse3:
# Ret
#####
  mov $1, %eax
  mov %eax, -4(%ebp)   # tmp0=eax
  mov 8(%ebp), %eax   # eax=n
  sub -4(%ebp), %eax   # eax=e1-e2
  push %eax
  call nfac
  add $4, %esp
  mov %eax, -4(%ebp)   # tmp0=eax
  mov 8(%ebp), %eax   # eax=n
  imul -4(%ebp), %eax   # eax=e1*e2
  mov %ebp, %esp
  pop %ebp
  ret
ifEnd3:
  mov %ebp, %esp
  pop %ebp
  ret


.globl nfac
  .type nfac, @function
nfac:
  pushl %ebp
  movl %esp, %ebp
  lea -12(%ebp), %esp
# CondElse
##########
  mov $0, %eax
  mov %eax, -4(%ebp)   # tmp0=eax
  mov 8(%ebp), %eax   # eax=n
  cmp -4(%ebp), %eax   # e2, e1
  setne %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $1, %eax
  jne ifElse4
# Ret
#####
  mov 8(%ebp), %eax   # eax=n
  mov %eax, -4(%ebp)   # tmp0=eax
  mov $1, %eax
  mov %eax, -8(%ebp)   # tmp1=eax
  mov 8(%ebp), %eax   # eax=n
  sub -8(%ebp), %eax   # eax=e1-e2
  push %eax
  call mfac
  add $4, %esp
  imul -4(%ebp), %eax   # eax=e1*e2
  mov %ebp, %esp
  pop %ebp
  ret
  jmp ifEnd4
ifElse4:
# Ret
#####
  mov $1, %eax
  mov %ebp, %esp
  pop %ebp
  ret
ifEnd4:
  mov %ebp, %esp
  pop %ebp
  ret


.globl ifac
  .type ifac, @function
ifac:
  pushl %ebp
  movl %esp, %ebp
  lea -4(%ebp), %esp
# Ret
#####
  mov 8(%ebp), %eax   # eax=n
  push %eax
  mov $1, %eax
  push %eax
  call ifac2f
  add $8, %esp
  mov %ebp, %esp
  pop %ebp
  ret
  mov %ebp, %esp
  pop %ebp
  ret


.globl ifac2f
  .type ifac2f, @function
ifac2f:
  pushl %ebp
  movl %esp, %ebp
  lea -16(%ebp), %esp
# Cond
######
  mov 12(%ebp), %eax   # eax=h
  mov %eax, -8(%ebp)   # tmp0=eax
  mov 8(%ebp), %eax   # eax=l
  cmp -8(%ebp), %eax   # e2, e1
  sete %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $1, %eax
  jne ifEnd5
# Ret
#####
  mov 8(%ebp), %eax   # eax=l
  mov %ebp, %esp
  pop %ebp
  ret
ifEnd5:
# Cond
######
  mov 12(%ebp), %eax   # eax=h
  mov %eax, -8(%ebp)   # tmp0=eax
  mov 8(%ebp), %eax   # eax=l
  cmp -8(%ebp), %eax   # e2, e1
  setg %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $1, %eax
  jne ifEnd6
# Ret
#####
  mov $1, %eax
  mov %ebp, %esp
  pop %ebp
  ret
ifEnd6:
# Ass
#####
  mov $0, %eax
  mov %eax, -8(%ebp)   # tmp0= expr. value 
  lea -4(%ebp), %eax   # eax=adres m
  mov -8(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
# Ass
#####
  mov $2, %eax
  mov %eax, -8(%ebp)   # tmp0=eax
  mov 12(%ebp), %eax   # eax=h
  mov %eax, -12(%ebp)   # tmp1=eax
  mov 8(%ebp), %eax   # eax=l
  add -12(%ebp), %eax   # eax=e1+e2
  mov  %eax, %edx
  sar  $31, %edx
  idivl -8(%ebp)   # eax=e1/e2
  mov %eax, -8(%ebp)   # tmp0= expr. value 
  lea -4(%ebp), %eax   # eax=adres m
  mov -8(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
# Ret
#####
  mov 12(%ebp), %eax   # eax=h
  push %eax
  mov $1, %eax
  mov %eax, -8(%ebp)   # tmp0=eax
  mov -4(%ebp), %eax   # eax=m
  add -8(%ebp), %eax   # eax=e1+e2
  push %eax
  call ifac2f
  add $8, %esp
  mov %eax, -8(%ebp)   # tmp0=eax
  mov -4(%ebp), %eax   # eax=m
  push %eax
  mov 8(%ebp), %eax   # eax=l
  push %eax
  call ifac2f
  add $8, %esp
  imul -8(%ebp), %eax   # eax=e1*e2
  mov %ebp, %esp
  pop %ebp
  ret
  mov %ebp, %esp
  pop %ebp
  ret


.globl repStr
  .type repStr, @function
repStr:
  pushl %ebp
  movl %esp, %ebp
  lea -16(%ebp), %esp
  mov $String$69$14, %eax
  mov %eax, -4(%ebp)   # r=eax
  mov $0, %eax
  mov %eax, -8(%ebp)   # i=eax
# While
#######
  jmp whileCond7
whileLoop7:
# Ass
#####
  mov 8(%ebp), %eax   # eax=s
  mov %eax, -12(%ebp)   # tmp0=eax
  mov -4(%ebp), %eax   # eax=r
  push -12(%ebp)
  push %eax
  call __concat   # eax=e1+e2 (Str)
  add 8, %esp
  mov %eax, -12(%ebp)   # tmp0= expr. value 
  lea -4(%ebp), %eax   # eax=adres r
  mov -12(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
# Incr
######
  lea -8(%ebp), %eax   # eax=adres i
  incl (%eax)
whileCond7:
  mov 12(%ebp), %eax   # eax=n
  mov %eax, -12(%ebp)   # tmp0=eax
  mov -8(%ebp), %eax   # eax=i
  cmp -12(%ebp), %eax   # e2, e1
  setl %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $1, %eax
  je whileLoop7
# Ret
#####
  mov -4(%ebp), %eax   # eax=r
  mov %ebp, %esp
  pop %ebp
  ret
  mov %ebp, %esp
  pop %ebp
  ret


