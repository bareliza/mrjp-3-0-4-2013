String$1$1:
  .string ""
String$6$12:
  .string "a"
String$16$11:
  .string "aaa"
String$17$19:
  .string "aaa"
String$34$9:
  .string "b"
String$39$26:
  .string "true"
String$39$51:
  .string "false"
String$43$14:
  .string "abc"
.globl p
  .type p, @function
p:
  pushl %ebp
  movl %esp, %ebp
  lea -4(%ebp), %esp
  mov %ebp, %esp
  pop %ebp
  ret


.globl main
  .type main, @function
main:
  pushl %ebp
  movl %esp, %ebp
  lea -44(%ebp), %esp
  mov $3, %eax
  mov %eax, -4(%ebp)   # a=eax
# Ass
#####
  mov $0, %eax
  mov %eax, -32(%ebp)   # tmp0= expr. value 
  lea -8(%ebp), %eax   # eax=adres b
  mov -32(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
  mov $String$6$12, %eax
  mov %eax, -12(%ebp)   # c=eax
# Ass
#####
  mov $String$1$1, %eax
  mov %eax, -32(%ebp)   # tmp0= expr. value 
  lea -16(%ebp), %eax   # eax=adres d
  mov -32(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
  mov $1, %eax   # eax=true
  mov %eax, -20(%ebp)   # e=eax
# Ass
#####
  xor %eax, %eax   # eax=false
  mov %eax, -32(%ebp)   # tmp0= expr. value 
  lea -24(%ebp), %eax   # eax=adres f
  mov -32(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
# Ass
#####
  mov -4(%ebp), %eax   # eax=a
  mov %eax, -32(%ebp)   # tmp0=eax
  mov $7, %eax
  mov  %eax, %edx
  sar  $31, %edx
  idivl -32(%ebp)
  mov %edx, %eax   # eax=e1%e2
  mov %eax, -32(%ebp)   # tmp0=eax
  mov -4(%ebp), %eax   # eax=a
  mov %eax, -36(%ebp)   # tmp1=eax
  mov $6, %eax
  mov  %eax, %edx
  sar  $31, %edx
  idivl -36(%ebp)   # eax=e1/e2
  mov %eax, -36(%ebp)   # tmp1=eax
  mov -4(%ebp), %eax   # eax=a
  mov %eax, -40(%ebp)   # tmp2=eax
  mov $2, %eax
  imul -40(%ebp), %eax   # eax=e1*e2
  sub -36(%ebp), %eax   # eax=e1-e2
  add -32(%ebp), %eax   # eax=e1+e2
  mov %eax, -32(%ebp)   # tmp0= expr. value 
  lea -8(%ebp), %eax   # eax=adres b
  mov -32(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
# Decr
######
  lea -8(%ebp), %eax   # eax=adres b
  decl (%eax)
# Incr
######
  lea -4(%ebp), %eax   # eax=adres a
  incl (%eax)
  mov -8(%ebp), %eax   # eax=b
  push %eax
  call printInt
  add $4, %esp
  call p
  add $0, %esp
  mov -12(%ebp), %eax   # eax=c
  push %eax
  mov $2, %eax
  push %eax
  call q
  add $8, %esp
  mov -16(%ebp), %eax   # eax=d
  push %eax
  mov $1, %eax
  push %eax
  call q
  add $8, %esp
# Ass
#####
  call readString
  add $0, %esp
  mov %eax, -32(%ebp)   # tmp0= expr. value 
  lea -16(%ebp), %eax   # eax=adres d
  mov -32(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
  mov $String$16$11, %eax
  mov %eax, -32(%ebp)   # tmp0=eax
  mov -16(%ebp), %eax   # eax=d
  mov %eax, -36(%ebp)   # tmp1=eax
  mov -12(%ebp), %eax   # eax=c
  push -36(%ebp)
  push %eax
  call __concat   # eax=e1+e2 (Str)
  add $8, %esp
  push -32(%ebp)
  push %eax
  call __concat   # eax=e1+e2 (Str)
  add $8, %esp
  push %eax
  mov $0, %eax
  push %eax
  call q
  add $8, %esp
  mov $String$17$19, %eax
  mov %eax, -32(%ebp)   # tmp0=eax
  mov -16(%ebp), %eax   # eax=d
  mov %eax, -36(%ebp)   # tmp1=eax
  mov -12(%ebp), %eax   # eax=c
  push -36(%ebp)
  push %eax
  call __concat   # eax=e1+e2 (Str)
  add $8, %esp
  push -32(%ebp)
  push %eax
  call __concat   # eax=e1+e2 (Str)
  add $8, %esp
  push %eax
  call readInt
  add $0, %esp
  push %eax
  call q
  add $8, %esp
  mov $5, %eax
  mov %eax, -28(%ebp)   # a=eax
# While
#######
  jmp whileCond13
whileLoop13:
# Decr
######
  lea -28(%ebp), %eax   # eax=adres a
  decl (%eax)
# Ass
#####
  mov -20(%ebp), %eax   # eax=e
  xor $1, %eax
  mov %eax, -32(%ebp)   # tmp0= expr. value 
  lea -20(%ebp), %eax   # eax=adres e
  mov -32(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
# Ass
#####
  mov -20(%ebp), %eax   # eax=e
  cmp $0, %eax
  je OrFirstFalse0
  jmp OrTrue0
OrFirstFalse0:
  xor %eax, %eax   # eax=false
  cmp $1, %eax
  je AndFirstTrue1
  jmp AndFalse1
AndFirstTrue1:
  mov -20(%ebp), %eax   # eax=e
  cmp $0, %eax
  je OrFirstFalse3
  jmp OrTrue3
OrFirstFalse3:
  mov -24(%ebp), %eax   # eax=f
  jmp OrEnd3
OrTrue3:
  mov $1, %eax
OrEnd3:
  cmp $1, %eax
  je AndFirstTrue2
  jmp AndFalse2
AndFirstTrue2:
  mov -8(%ebp), %eax   # eax=b
  mov %eax, -32(%ebp)   # tmp0=eax
  mov -28(%ebp), %eax   # eax=a
  cmp -32(%ebp), %eax   # e2, e1
  setg %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $0, %eax
  je OrFirstFalse5
  jmp OrTrue5
OrFirstFalse5:
  mov -8(%ebp), %eax   # eax=b
  mov %eax, -32(%ebp)   # tmp0=eax
  mov -28(%ebp), %eax   # eax=a
  cmp -32(%ebp), %eax   # e2, e1
  setl %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $0, %eax
  je OrFirstFalse6
  jmp OrTrue6
OrFirstFalse6:
  mov -8(%ebp), %eax   # eax=b
  mov %eax, -32(%ebp)   # tmp0=eax
  mov -28(%ebp), %eax   # eax=a
  cmp -32(%ebp), %eax   # e2, e1
  sete %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $0, %eax
  je OrFirstFalse7
  jmp OrTrue7
OrFirstFalse7:
  mov -8(%ebp), %eax   # eax=b
  mov %eax, -32(%ebp)   # tmp0=eax
  mov -28(%ebp), %eax   # eax=a
  cmp -32(%ebp), %eax   # e2, e1
  setle %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $0, %eax
  je OrFirstFalse8
  jmp OrTrue8
OrFirstFalse8:
  mov -8(%ebp), %eax   # eax=b
  mov %eax, -32(%ebp)   # tmp0=eax
  mov -28(%ebp), %eax   # eax=a
  cmp -32(%ebp), %eax   # e2, e1
  setge %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $0, %eax
  je OrFirstFalse9
  jmp OrTrue9
OrFirstFalse9:
  mov -8(%ebp), %eax   # eax=b
  mov %eax, -32(%ebp)   # tmp0=eax
  mov -28(%ebp), %eax   # eax=a
  cmp -32(%ebp), %eax   # e2, e1
  setne %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  jmp OrEnd9
OrTrue9:
  mov $1, %eax
OrEnd9:
  jmp OrEnd8
OrTrue8:
  mov $1, %eax
OrEnd8:
  jmp OrEnd7
OrTrue7:
  mov $1, %eax
OrEnd7:
  jmp OrEnd6
OrTrue6:
  mov $1, %eax
OrEnd6:
  jmp OrEnd5
OrTrue5:
  mov $1, %eax
OrEnd5:
  cmp $1, %eax
  je AndFirstTrue4
  jmp AndFalse4
AndFirstTrue4:
  mov -16(%ebp), %eax   # eax=d
  mov %eax, -32(%ebp)   # tmp0=eax
  mov -12(%ebp), %eax   # eax=c
  cmp -32(%ebp), %eax   # e2, e1
  sete %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $0, %eax
  je OrFirstFalse11
  jmp OrTrue11
OrFirstFalse11:
  mov -16(%ebp), %eax   # eax=d
  mov %eax, -32(%ebp)   # tmp0=eax
  mov -12(%ebp), %eax   # eax=c
  cmp -32(%ebp), %eax   # e2, e1
  setne %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  jmp OrEnd11
OrTrue11:
  mov $1, %eax
OrEnd11:
  cmp $1, %eax
  je AndFirstTrue10
  jmp AndFalse10
AndFirstTrue10:
  mov -24(%ebp), %eax   # eax=f
  mov %eax, -32(%ebp)   # tmp0=eax
  mov -20(%ebp), %eax   # eax=e
  cmp -32(%ebp), %eax   # e2, e1
  sete %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $0, %eax
  je OrFirstFalse12
  jmp OrTrue12
OrFirstFalse12:
  mov -24(%ebp), %eax   # eax=f
  mov %eax, -32(%ebp)   # tmp0=eax
  mov -20(%ebp), %eax   # eax=e
  cmp -32(%ebp), %eax   # e2, e1
  setne %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  jmp OrEnd12
OrTrue12:
  mov $1, %eax
OrEnd12:
  jmp AndEnd10
AndFalse10:
  xor %eax,%eax
AndEnd10:
  jmp AndEnd4
AndFalse4:
  xor %eax,%eax
AndEnd4:
  jmp AndEnd2
AndFalse2:
  xor %eax,%eax
AndEnd2:
  jmp AndEnd1
AndFalse1:
  xor %eax,%eax
AndEnd1:
  jmp OrEnd0
OrTrue0:
  mov $1, %eax
OrEnd0:
  mov %eax, -32(%ebp)   # tmp0= expr. value 
  lea -24(%ebp), %eax   # eax=adres f
  mov -32(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
# Ass
#####
  mov -28(%ebp), %eax   # eax=a
  mov %eax, -32(%ebp)   # tmp0= expr. value 
  lea -28(%ebp), %eax   # eax=adres a
  mov -32(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
# Ass
#####
  mov -8(%ebp), %eax   # eax=b
  neg %eax
  mov %eax, -32(%ebp)   # tmp0= expr. value 
  lea -8(%ebp), %eax   # eax=adres b
  mov -32(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
  mov -28(%ebp), %eax   # eax=a
  push %eax
  call printInt
  add $4, %esp
  mov -8(%ebp), %eax   # eax=b
  push %eax
  call printInt
  add $4, %esp
  mov -24(%ebp), %eax   # eax=f
  push %eax
  call printBool
  add $4, %esp
whileCond13:
  mov $0, %eax
  mov %eax, -32(%ebp)   # tmp0=eax
  mov -28(%ebp), %eax   # eax=a
  cmp -32(%ebp), %eax   # e2, e1
  setne %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $1, %eax
  je whileLoop13
# Cond
######
  mov $String$34$9, %eax
  mov %eax, -32(%ebp)   # tmp0=eax
  mov -16(%ebp), %eax   # eax=d
  cmp -32(%ebp), %eax   # e2, e1
  sete %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $1, %eax
  jne ifEnd14
  call error
  add $0, %esp
ifEnd14:
# Ret
#####
  mov $0, %eax
  mov %ebp, %esp
  pop %ebp
  ret
  mov %ebp, %esp
  pop %ebp
  ret


.globl printBool
  .type printBool, @function
printBool:
  pushl %ebp
  movl %esp, %ebp
  lea -8(%ebp), %esp
# CondElse
##########
  mov $1, %eax   # eax=true
  mov %eax, -4(%ebp)   # tmp0=eax
  mov 8(%ebp), %eax   # eax=b
  cmp -4(%ebp), %eax   # e2, e1
  sete %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $1, %eax
  jne ifElse15
  mov $String$39$26, %eax
  push %eax
  call printString
  add $4, %esp
  jmp ifEnd15
ifElse15:
  mov $String$39$51, %eax
  push %eax
  call printString
  add $4, %esp
ifEnd15:
  mov %ebp, %esp
  pop %ebp
  ret


.globl q
  .type q, @function
q:
  pushl %ebp
  movl %esp, %ebp
  lea -8(%ebp), %esp
# Cond
######
  mov $0, %eax
  mov %eax, -4(%ebp)   # tmp0=eax
  mov 8(%ebp), %eax   # eax=a
  cmp -4(%ebp), %eax   # e2, e1
  setg %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $1, %eax
  jne ifEnd16
# Ass
#####
  mov $String$43$14, %eax
  mov %eax, -4(%ebp)   # tmp0=eax
  mov 12(%ebp), %eax   # eax=b
  push -4(%ebp)
  push %eax
  call __concat   # eax=e1+e2 (Str)
  add $8, %esp
  mov %eax, -4(%ebp)   # tmp0= expr. value 
  lea 12(%ebp), %eax   # eax=adres b
  mov -4(%ebp), %ecx   # ebx=tmp0
  mov %ecx, (%eax)
ifEnd16:
  mov 12(%ebp), %eax   # eax=b
  push %eax
  call printString
  add $4, %esp
# CondElse
##########
  mov $1, %eax
  mov %eax, -4(%ebp)   # tmp0=eax
  mov 8(%ebp), %eax   # eax=a
  cmp -4(%ebp), %eax   # e2, e1
  sete %al
  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0
  cmp $1, %eax
  jne ifElse17
# VRet
######
  mov %ebp, %esp
  pop %ebp
  ret
  jmp ifEnd17
ifElse17:
  mov 8(%ebp), %eax   # eax=a
  push %eax
  call printInt
  add $4, %esp
ifEnd17:
  mov %ebp, %esp
  pop %ebp
  ret


