; Runtime.j

; Generated by ClassFileAnalyzer (Can)
; Analyzer and Disassembler for Java class files
; (Jasmin syntax 2, http://jasmin.sourceforge.net)
;
; ClassFileAnalyzer, version 0.7.0 


.bytecode 50.0
.source Runtime.java
.class public lib/Runtime
.super java/lang/Object

.method public <init>()V
  .limit stack 1
  .limit locals 1
  .line 7
  0: aload_0
  1: invokespecial java/lang/Object/<init>()V
  4: return
.end method

.method public static readString()Ljava/lang/String;
  .limit stack 3
  .limit locals 2
  .line 9
  0: new java/io/InputStreamReader
  3: dup
  4: getstatic java/lang/System/in Ljava/io/InputStream;
  7: invokespecial java/io/InputStreamReader/<init>(Ljava/io/InputStream;)V
  10: astore_0
  .line 10
  11: new java/io/BufferedReader
  14: dup
  15: aload_0
  16: invokespecial java/io/BufferedReader/<init>(Ljava/io/Reader;)V
  19: astore_1
  .line 12
  20: aload_1
  21: invokevirtual java/io/BufferedReader/readLine()Ljava/lang/String;
  24: areturn
  .throws java/io/IOException
.end method

.method public static readInt()I
  .limit stack 1
  .limit locals 1
  .line 16
  0: invokestatic lib/Runtime/readString()Ljava/lang/String;
  3: astore_0
  .line 18
  4: aload_0
  5: invokestatic java/lang/Integer/parseInt(Ljava/lang/String;)I
  8: ireturn
  .throws java/io/IOException
.end method

.method public static printString(Ljava/lang/String;)V
  .limit stack 2
  .limit locals 1
  .line 22
  0: getstatic java/lang/System/out Ljava/io/PrintStream;
  3: aload_0
  4: invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
  .line 23
  7: return
.end method

.method public static printInt(I)V
  .limit stack 2
  .limit locals 1
  .line 26
  0: getstatic java/lang/System/out Ljava/io/PrintStream;
  3: iload_0
  4: invokevirtual java/io/PrintStream/println(I)V
  .line 27
  7: return
.end method

.method public static error()V
  .limit stack 1
  .limit locals 0
  .line 30
  0: iconst_1
  1: invokestatic java/lang/System/exit(I)V
  .line 31
  4: return
.end method

