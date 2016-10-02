// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Put your code here.
    //initialize i to 0
    @i
    M=0
    @2
    M=0
// while(i < r0) {
//     ret += r1
//     i++;
// }
(LOOP)
    @i
    D=M
    @0
    A=M
    D=D-A
    @END
    D;JGE
    @2
    D=M
    @1
    A=M
    D=D+A
    @2
    M=D
    @i
    M=M+1
    @LOOP
    0;JMP
(END)
    @END
    0;JMP
