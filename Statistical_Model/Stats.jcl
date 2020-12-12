//KC03C96 JOB ,'Kofi Adu-Gyan' ,MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
******************************************************************
*                                                                *
*  CSCI 360-1           ASSIGN4                       FALL 2020  *
*                                                                *
*  DATE DUE:  11/25/2020                                         *
*  TIME DUE:  11:59PM                                            *
*                                                                *
*  AUTHOR: Kofi Adu-Gyan                                         *
*                                                                *
*  The purpose of this implement a simple linear equation and    *
*  use it to process data in records that you will read from     *
*  an input member.                                              *
*                                                                *
*  REGISTER USAGE:                                               *
*                                                                *
*                                                                *
*                                                                *
******************************************************************
*        COL. 10
*        |     COL . 16
*        |     |
*        v     v
* ====== Dummy Section =========================================
$SMPLTBL DSECT
$X1      DS    PL7
$X2      DS    PL7
$YHAT    DS    PL7
$Y       DS    PL7
* ====== Prepare Registers =========================================
MAIN     CSECT
*
*** <STEP 1> Standard entry linkage
*
         STM   14,12,12(13)         Save regs in caller's save area
         LR    12,15                Copy CSECT address into R12
         USING MAIN,12        Establish addressability on R12
         LA    14,SAVEAREA      Point R14 at this CSECT 's save area
         ST    14,8(,13)       Store address of this CSECT 's save area
         ST    13,4(,14)          Store address of caller 's save area
         LR    13,14               Point R13 at this save area
*
*** <STEP 2> Main calls the external programs
*
         LA    1,PARMS1         POINT R1 AT AT PARMS1 LIST
         L     15,=V(RDTSTDAT)  R15 -> VIRTUAL ADDRESS OF RDTSTDAT
         BALR  14,15            BRANCH & LINK TO SUBPROGRAM
*
         LA    1,PARMS2        POINT R1 AT PARMS2 PARAMETER LIST
         L     15,=V(TSTMODEL) R15 -> VIRTUAL ADDRESS OF TSTMODEL
         BALR  14,15           BRANCH & LINK TO SUBPROGRAM
*
         L     15,=V(PRNTSUMM) R15 -> VIRTUAL ADDRESS OF PRNTSUMM
         BALR  14,15           BRANCH & LINK TO SUBPROGRAM
*
*** <STEP 3> Standard exit linkage
*
         SR    15,15         SET RETURN CODE TO 0
         L     13,4(,13)     POINT R13 TO CALLER'S SAVE AREA
         L     14,12(,13)    RESTORE R14
         LM    0,12,20(13)   RESTORE R0 THRU R12
         BR    14            RETURN TO CALLER
*
         LTORG
*
SAVEAREA DS    18F                Register Save Area
*
TABLE    DS    11CL28                Table that holds data
RDBUFFER DS    CL80                   READ BUFFER
PARMS1   DC    A(TABLE,RDBUFFER)
PARMS2   DC    A(TABLE,RDBUFFER)
*
******************************************************************
*
*  "RDTSTDAT"  read test sample, convert into packed decimal
*   format, and store in a table
*
******************************************************************
*
RDTSTDAT CSECT
*
*** <STEP 1> Standard entry linkage
*
         STM   14,12,12(13)     SAVE REGS IN CALLER'S SAVE AREA
         LR    12,15            COPY CSECT ADDRESS INTO R12
         USING RDTSTDAT,12      ESTABLISH ADDRESSABILITY ON R12
         USING $SMPLTBL,2       USE A DSECT ON R2
         LA    14,RDSAVE        POINT R14 AT THIS CSECT'S SAVE AREA
         ST    14,8(,13)        STORE ADDRESS OF THIS CSECT'S SAVE AREA
         ST    13,4(,14)        STORE ADDRESS OF CALLER'S SAVE AREA
         LR    13,14            POINT R13 AT THIS SAVE AREA
*
*** <STEP 2> Dereference Parameter
*
         LM    2,3,0(1)         R2->RDBUFFER, R3->TABLE
*
*** <STEP 3> Go through buffer and pack values to store in the table
*
LOOP     XREAD 0(,3),80         READ FROM INPUT BUFFER
         MVC   ENDBUF(1),0(3)   MOVES FIRST CHAR READ TO CHECK FOR *
         CLC   ENDBUF(0),=C'*'  IF CHAR IS A *, THE LOOP BREAKS
         BC    B'1000',ENDLOOP
*
         PACK  $X1(7),0(7,3)    PACKS THE FIRST VALUE READ INTO X1
         PACK  $X2(7),8(7,3)    PACKS THE SECOND VALUE READ INTO X2
         PACK  $YHAT(7),16(7,3) PACKS THE THIRD VALUE READ INTO YHAT
         LA    2,28(,2)         MOVES TO THE NEXT LINE OF THE TABLE
         B     LOOP
*
*** <STEP 4> Exit linkage
*
ENDLOOP  SR    15,15         SET RETURN CODE TO 0
         L     13,4(,13)     POINT R13 TO CALLER'S SAVE AREA
         L     14,12(,13)    RESTORE R14
         LM    0,12,20(13)   RESTORE R0 THRU R12
         BR    14            RETURN TO CALLER
*
         DROP  2             DROP THE DSECT ONCE FINISHED
*
         LTORG
*               =C'*'
*
RDSAVE   DS    18F          THIS SUBPROGRAM'S SAVE AREA
ENDBUF   DS    CL1          BUFFER TO HOLD FIRST CHAR OF A LINE
******************************************************************
*                           TSTMODEL
*   Takes the model weights and a test sample table passed by
*   reference in a parameter list. Iterates over the test sample
*   table and calls CALCYRES (below) to calculate the ys, and
*   then calls PRSMPRES to print the per sample output lines.
*
******************************************************************
TSTMODEL CSECT
*
*** <STEP 1> Standard entry linkage
*
         STM   14,12,12(13)  Save regs in caller's save area
         LR    12,15         Copy CSECT address into R12
         USING TSTMODEL,12   Establish addressibility on R12
         LA    14,SQRTSAVE   Point R14 at this CSECT's save area
         ST    14,8(,13)     Store address of this CSECT's save area
         ST    13,4(,14)     Store address of caller's save area
         LR    13,14         Point R13 at current save area
*
*** <STEP 2> Dereference Parameter
*
         LM    2,3,0(1)         R2->RDBUFFER, R3->TABLE
*
*** <STEP 3> Read values
*
         XREAD 0(,3),80         READS LAST LINE OF RDBUFFER
         PACK  W1(7),0(7,3)     PACKS THE FIRST VALUE READ INTO TW1
         PACK  W2(7),8(7,3)     PACKS THE SECOND VALUE READ INTO W2
         LA    4,11             SETS VALUE OF REG 4 AS 11
*
LOOP2    LTR   4,4              CHECK IF R4 HAS A VALUE OF 0
         BZ    ENDLOOP2         IF R4=0 THEN THE LOOP BREAKS
         MVC   TSTTBL(28),0(2)  FOCUS THE TABLE ON THE FIRST LINE
         LA    2,28(,2)         MOVES R2 TO NEXT LINE IN TABLE
*
*** <STEP 4> Call external programs CALCYRES and PRSMPRES
*
         LA    1,PARMS3         POINT R1 AT PARMS3 PARAMETER LIST
         L     15,=V(CALCYRES)  R15-> VIRTUAL ADDRESS OF CALCYRES
         BALR  14,15            BRANCH & LINK TO SUBPROGRAM
*
         LA    1,PARMS4         POINT R1 AT PARMS4 PARAMETER LIST
         L     15,=V(PRSMPRES)  R15-> VIRTUAL ADDRESS OF PRSMPRES
         BALR  14,15            BRANCH & LINK TO SUBPROGRAM
*
         BCTR  4,0              DECREMENT R4 AND BRANCH TO TOP
         B     LOOP2
*
*        //STANDARD EXIT LINKAGE
ENDLOOP2 SR    15,15         SET RETURN CODE TO 0
         L     13,4(,13)     POINT R13 TO CALLER'S SAVE AREA
         L     14,12(,13)    RESTORE R14
         LM    0,12,20(13)   RESTORE R0 THRU R12
         BR    14            RETURN TO CALLER
*
         LTORG
*
SQRTSAVE DS    18F           THIS SUBPROGRAM'S SAVE AREA
W1       DS    PL7           FIRST WEIGHT VALUE IN PACKED FORM
W2       DS    PL7           SECOND WEIGHT VALUE IN PACKED FORM
TSTTBL   DS    CL28          TEST SAMPLE TABLE
*
PARMS3   DC    A(W1,W2,TSTTBL)
PARMS4   DC    A(TSTTBL)
**************************************
*   CALCYRES
**************************************
CALCYRES CSECT
         STM   14,12,12(13)     SAVE REGS IN CALLER'S SAVE AREA
         LR    12,15            COPY CSECT ADDRESS INTO R12
         USING CALCYRES,12      ESTABLISH ADDRESSABILITY ON R12
         USING $SMPLTBL,4       USE A DSECT ON R4
         LA    14,CALCSAVE      POINT R14 AT THIS CSECT'S SAVE AREA
         ST    14,8(,13)        STORE ADDRESS OF THIS CSECT'S SAVE AREA
         ST    13,4(,14)        STORE ADDRESS OF CALLER'S SAVE AREA
         LR    13,14            POINT R13 AT THIS SAVE AREA
*
*** <STEP 2> Dereference Parameter
*
         LM    2,4,0(1)         R2->W1, R3->W2, R4->TSTTBL
*
         ZAP   TEMP1(14),$X1(7)    ZAPS X1 INTO LARGER TEMP1
         ZAP   TEMP2(14),$X2(7)    ZAPS X2 INTO LARGER TEMP2
         MP    TEMP1(14),$X1       MULTIPLY X1 BY ITSELF (SQUARES X1)
         SRP   TEMP1(14),64-3,5    SHIFTING AND ROUNDING
         MP    TEMP1(14),0(7,2)    MULTIPLY X1 BY VALUE OF W1
         SRP   TEMP1(14),64-3,5    SHIFTING AND ROUNDING
         MP    TEMP2(14),0(7,3)    MULTIPLY X2 BY VALUE OF W2
         SRP   TEMP2(14),64-3,5    SHIFTING AND ROUNDING
         AP    TEMP1(14),TEMP2(14)   ADD THE TWO PRODUCTS TOGETHER
         ZAP   $Y(7),TEMP1+7(7)    SETS THE SUM AS THE VALUE OF Y
*
*        //STANDARD EXIT LINKAGE
         SR    15,15         SET RETURN CODE TO 0
         L     13,4(,13)     POINT R13 TO CALLER'S SAVE AREA
         L     14,12(,13)    RESTORE R14
         LM    0,12,20(13)   RESTORE R0 THRU R12
         BR    14            RETURN TO CALLER
*
         DROP  4             DROP THE DSECT ONCE FINISHED
*
         LTORG
*
CALCSAVE DS    18F           THIS SUBPROGRAM'S SAVE AREA
TEMP1    DC    PL14'0'       BUFFERS TO HOLD VALUES FOR MULTIPLYING
TEMP2    DC    PL14'0'
*
**************************************
*   PRSMPRES                         *
**************************************
PRSMPRES CSECT
*        //STANDARD ENTRY LINKAGE
         STM   14,12,12(13)     SAVE REGS IN CALLER'S SAVE AREA
         LR    12,15            COPY CSECT ADDRESS INTO R12
         USING PRSMPRES,12      ESTABLISH ADDRESSABILITY ON R12
         LA    14,PRSMSAVE      POINT R14 AT THIS CSECT'S SAVE AREA
         ST    14,8(,13)        STORE ADDRESS OF THIS CSECT'S SAVE AREA
         ST    13,4(,14)        STORE ADDRESS OF CALLER'S SAVE AREA
         LR    13,14            POINT R13 AT THIS SAVE AREA
*        //DEREFERENCE PARAMETERS
         LM    2,2,0(1)         R2->TSTTBL
         MVC   X1OUT(9),=X'40202021204B202020'
         ED    X1OUT(9),3(2)    EDITS OUTPUT FIELD FOR PRINTING
         LA    2,7(,2)          SETS REG 2 TO NEXT VALUE TO PRINT
         MVC   X2OUT(9),=X'40202021204B202020'
         ED    X2OUT(9),3(2)    EDITS OUTPUT FIELD FOR PRINTING
         LA    2,7(,2)          SETS REG 2 TO NEXT VALUE TO PRINT
         MVC   YHATOUT(9),=X'40202021204B202020'
         ED    YHATOUT(9),3(2)  EDITS OUTPUT FIELD FOR PRINTING
         LA    2,7(,2)          SETS REG 2 TO NEXT VALUE TO PRINT
         MVC   YOUT(11),=X'402020202021204B202020'
         ED    YOUT(11),2(2)    EDITS OUTPUT FIELD FOR PRINTING
         XPRNT PRNTLINE         PRINTS LINE OF OUTPUT
*
*        //STANDARD EXIT LINKAGE
         SR    15,15         SET RETURN CODE TO 0
         L     13,4(,13)     POINT R13 TO CALLER'S SAVE AREA
         L     14,12(,13)    RESTORE R14
         LM    0,12,20(13)   RESTORE R0 THRU R12
         BR    14            RETURN TO CALLER
*
         LTORG
*               =X'40202021204B202020'
*               =X'402020202021204B202020'
*
PRSMSAVE DS    18F           THIS SUBPROGRAM'S SAVE AREA
*
PRNTLINE DC    C'0'          SET DOUBLE SPACING ON THIS LINE
         DC    C'x1:<'
X1OUT    DS    CL9           X1 OUTPUT FIELD
         DC    C'> | x2:<'
X2OUT    DS    CL9           X2 OUTPUT FIELD
         DC    C'> | y:<'
YOUT     DS    CL11          Y OUTPUT FIELD
         DC    C'>  |(yhat:<'
YHATOUT  DS    CL9           YHAT OUTPUT FIELD
         DC    C'>)'
         DC    59C' '        FILL REMAINING PRINT LINE
*
**************************************
*   External subprogram: PRNTSUMM    *
**************************************
PRNTSUMM CSECT
*        //STANDARD ENTRY LINKAGE
         STM   14,12,12(13)     SAVE REGS IN CALLER'S SAVE AREA
         LR    12,15            COPY CSECT ADDRESS INTO R12
         USING PRNTSUMM,12      ESTABLISH ADDRESSABILITY ON R12
         LA    14,PRNTSAVE      POINT R14 AT THIS CSECT'S SAVE AREA
         ST    14,8(,13)        STORE ADDRESS OF THIS CSECT'S SAVE AREA
         ST    13,4(,14)        STORE ADDRESS OF CALLER'S SAVE AREA
         LR    13,14            POINT R13 AT THIS SAVE AREA
*
         XPRNT PRNTSUM          PRINTS EVAL COMPLETE MESSAGE
*
*        //STANDARD EXIT LINKAGE
         SR    15,15         SET RETURN CODE TO 0
         L     13,4(,13)     POINT R13 TO CALLER'S SAVE AREA
         L     14,12(,13)    RESTORE R14
         LM    0,12,20(13)   RESTORE R0 THRU R12
         BR    14            RETURN TO CALLER
*
         LTORG
*
PRNTSAVE DS    18F          SAVEAREA
*
PRNTSUM  DC    C'0'          SET DOUBLE SPACING ON THIS LINE
         DC    C'-Evaluation complete'
         DC    111C' '       FILL REMAINING PRINT LINE
*
         END   MAIN
/*
//*
//FT05F001 DD DSN=KC02038.CSCI360.DATAFA20(ASGN4DAT),DISP=SHR
//*
//FT06F001 DD SYSOUT=*
//*
//SYSPRINT DD SYSOUT=*
//
