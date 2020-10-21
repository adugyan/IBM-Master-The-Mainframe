//KC03C96 JOB ,'Kofi Adu-Gyan' ,MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
******************************************************************
*                                                                *
*  CSCI 360-1           LAB EXERCISE 3                FALL 2020  *
*                                                                *
*  DATE DUE:  09/28/2020                                         *
*  TIME DUE:  11:59PM                                            *
*                                                                *
*  AUTHOR: Kofi Adu-Gyan                                         *
*                                                                *
*  The purpose of this implement a simple linear equation and    *
*  use it to process data in records that you will read from     *
*  an input member.                                              *
*                                                                *
*  REGISTER USAGE:                                               *
*    EVEN-ODD PAIRS (2,3), (4,5), AND (6,7) USED FOR MULTI       *
*    AND DIVI                                                    *
*                                                                *
******************************************************************
*        COL. 10
*        |     COL . 16
*        |     |
*        v     v
MAIN     CSECT
         USING MAIN,15            ESTABLISH ADDRESSABILITY ON REG 15
*
*** <STEP 1> LOAD COEFFICENTS C1,C2,C3
*
         LA    2,2                LOAD 2 INTO REG 2
         LA    3,5                LOAD 5 INTO REG 3
         LA    4,10               LOAD 10 INTO REG 10
*
*** <STEP 2> Perform priming read and go into record processing loop
*
         XREAD RDBUFFER,80        READ FIRST RECORD
LOOP1    BC    B'0100',RDEND      BRANCH TO ENDLOOP1 IF EOF
*
*** <STEP 3>   CALCULATIONS
*
         XDECI 7,RDBUFFER  CONVERT FIRST NUM TO BINARY STORE IN REG 7
         XDECI 9,0(1)      CONVERT 2ND NUM TO BINARY STORE IN REG 9
*
         XDECO 7,X1        CONVERT REG 7 X1 TO CHARACTERS TO PRINT
         XDECO 9,X2        CONVERT REG 9 X2 TO CHARACTERS TO PRINT
*
         MR    6,7         MULT REG 7 TIMES REG 7
         MR    6,2         MULT REG 7 TIMES REG 2 STORE IN EVEN-ODD 6-7
         MR    8,3         MULT REG 9 TIMES REG 3 STORE IN EVEN-ODD 8-9
         AR    7,9         ADD REG 7 AND 9 STORE IN REG 7
         M     6,=F'1'     ZERO OUT EVEN REG OF EVEN ODD PAIR
         DR    6,4         DIVIDE REG 7 BY REG 4 STORE IN 6-7 EVEN ODD
*
         XDECO 7,Y         CONVERT REG 7 Y TO CHARACTERS TO PRINT
         XPRNT PRNTLINE,133
*
         XREAD RDBUFFER,80       PRIMING NEXT READ
         B     LOOP1      BRANCHING TO TOP OF RDLOOP
RDEND    DS    0H          BREAK OUT
*
         BCR   B'1111',14  UNCONDITIONAL RETURN TO CALLER ( OS )
*
         LTORG
* ====== Program storage =========================================
RDBUFFER DS    CL80          READ BUFFER
*
PRNTLINE DC    C' '          SET SINGLE-SPACING ON THIS LINE
         DC    C'x1 ='       X1 LABEL
X1       DS    CL12             FIELD
         DC    C' | x2 ='    X2 LABEL
X2       DS    CL12             FIELD
         DC    C' | y ='     Y LABEL
Y        DS    CL12             FIELD
         DC    79C' '        FILL REMAINING PRINT LINE
*
         END   MAIN
/*
//*
//FT05F001 DD DSN=KC02038.CSCI360.DATAFA20(LABEX3DA),DISP=SHR
//*
//FT06F001 DD SYSOUT=*
//*
//SYSPRINT DD SYSOUT=*
//
