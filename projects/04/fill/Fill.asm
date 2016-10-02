// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, the
// program clears the screen, i.e. writes "white" in every pixel.

// Put your code here.
    @i
    M=0
    @color
    M=0
(LOOP)
    @KBD
    D=M
    @BLACK
    D;JNE
    @color
    M=0
    @DRAW
    0;JMP
(BLACK)
    @color
    M=-1
(DRAW)
    @i
    D=M
    @SCREEN
    D=D+A
    @screen_pos
    M=D
    @color
    D=M
    @screen_pos
    A=M
    M=D
    @i
    M=M+1
    D=M
    @8192
    D=D-A
    @LOOP
    D;JLT
    @i
    M=0
    @LOOP
    0;JMP
