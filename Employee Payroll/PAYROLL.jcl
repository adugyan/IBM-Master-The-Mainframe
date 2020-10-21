//KC03C96 JOB ,'Kofi Adu-Gyan' ,MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
******************************************************************
*                                                                *
*  CSCI 360-1           ASSIGN3                       FALL 2020  *
*                                                                *
*  DATE DUE:  10/16/2020                                         *
*  TIME DUE:  11:59PM                                            *
*                                                                *
*  AUTHOR: Kofi Adu-Gyan                                         *
*                                                                *
*    An Assembler program that  calculates the payroll for a     *
*    buisness.                                                   *
*                                                                *
*                                                                *
*  REGISTER USAGE:                                               *
*    REG 5-11 HOLDS BUFFER INPUT DATA                            *
*                                                                *
*                                                                *
******************************************************************
*        COL. 10
*        |     COL . 16
*        |     |
*        v     v
PAYROLL1 CSECT
         USING PAYROLL1,15       ESTABLISH ADDRESSABILITY ON REG 15
*
*** <STEP 1> Perform priming read and go into record processing loop
*
         XDECI 11,RDBUFFER       CONVRT 1ST NUM FRM INPUT TO BINARY R11
         XREAD RDBUFFER,80
*
*** <STEP 2> Place values from the input record into registers
*
         ST    11,TXWTHHOL       STORE NUM IN R11 INTO TXWTHHOL
         XDECI 11,0(0,1)         CONVERT 2ND NUM FRM INPUT INTO R11
         ST    11,SENRTY1        STORE NUM IN R11 INTO SENRTY1
         XDECI 11,0(0,1)         3RD NUM FROM INPUT RECORD INTO R11
         ST    11,SENRTY2        STORE NUM IN R11 INTO SENRTY2
*
         XREAD RDBUFFER,80       READ NEXT RECORD
         L     12,EMPCOUNT       LOAD EMPLOYEE COUNTER WITH 0 IN R12
         L     8,TGROSS          LOAD TOTAL GROSS COUNTER WITH 0 IN R8
         LA    11,RDBUFFER       LOAD RDBUFFER ADDR INTO R11
*
*** <STEP 3> Copy bytes to the detail line and to prep for calcs
*
LOOP     BNE   ENDLOOP           ENTER RECORD PROCESSING LOOP
         A     12,=F'1'          ADD 1 TO EMPLOYEE COUNTER
         MVC   EMPLYNAM(25),0(11) MOVE 25 BYTES FRM RDBUFFER TO EMPLY
         MVC   ID(5),25(11)       MOVE 5 BYTES FROM 25 OFF 11 INTO ID
         MVC   SENRTY(1),31(11)   MOVE 1 BYTE 31 OFF 11 TO SENRTY
         L     10,SENRTY          LOAD CHARS IN SENRTY TO  R10
         MVC   RATE(2),33(11)     MOVE 2 BYTES INTO FULLWORD RATE
         XDECI 9,33(11)           CONVERT CHARS AT 33(11) TO BINARY R9
         XDECO 9,TEMP             R9(RATE) TO CHAR IN TEMP STORAGE
         LA    5,TEMP             LOAD ADDRESS OF TEMP(RATE) INTO R5
         MVC   ORATE(5),7(5)      5 BYTs FROM 7(5) TO ORATE FOR PRNT
         MVC   HOURS(2),36(11)    MOVE 2 BYTES 36 OFF 11 INTO HOURS
         XDECI 7,0(0,1)           NEXT CHARS/CONVER TO BINARY IN R7
         XDECO 7,TEMP             STORE 12 BYTS CHAR DATA INTO TEMP
         LA    5,TEMP             LOAD ADDR OF TEMP(HOURS) INTO R5
         MVC   OHOURS(5),7(5)     MV 5 BYTS 7 OFF (5) TO OHOUR FR PRNT
*
*** <STEP 4> Calcs
*
         MR    6,9                R7 HOLDS (RATE * HOURS)
         MVC   DEDUCTN(2),41(11)  MOVE 2 BYTES 41 OFF 11 INTO DEDUCTN
         XDECI 4,0(0,1)          READ NEXT CHARS/CONVER TO BINARY IN R4
         XDECO 4,TEMP            CONVERT/STORE BINARY TO CHAR IN TEMP
         LA    5,TEMP           LOAD ADDR OF TEMP(DEDUCTION) TO R5
         MVC   ODUDUCT(5),7(5) 5 BYT 7 OFF 5 INTO ODUDUCT FOR OUTPUT
         SR    7,4             R7(RATE*HOURS) MINUS R4(DEDUCTIONS)
         MVC   BONUS(3),44(11) MOVE 3 BYTES 44 OFF 11 INTO BONUS
         XDECI 3,0(0,1)        READ NEXT CHARS/CONVER TO BINARY IN R3
         L     9,=F'0'         INITIALIZE REG 9 WITH 0 FOR TBONUS COUNT
         AR    9,3             ADD CURRENT BONUS TO RUNNING TOTAL BONUS
         A     9,TBONUS        ADD BONUS TO TOTAL BONUS COUNT
         ST    9,TBONUS        STORE UPDATED TOTAL BONUS IN TBONUS
         C     10,SYMBL1       COMPARE R10 WITH 1ST BONUS MULTIPLIER
         BNE   SEN2            IF NOT EQUAL BRANCH TO SEN2 LABEL
         M     2,SENRTY1       ELSE MULTPLY R3(BONUS) SENRTY1
SEN2     C     10,SYMBL2       COMPARE R10 WITH 2ND BONUS MULTPR
         BNE   SEN3            IF NOT EQUAL BRANCH TO SEN3 LABEL
         M     2,SENRTY2       ELSE MULTIPLY BONUS IN R3 BY SENRTY2
SEN3     AR    7,3             ADD R3(BONUS)TO R7(RATE*HOURS)
         XDECO 3,TEMP          UPDATE CALCULATED BONUS STORED IN TEMP
         LA    5,TEMP          LOAD ADDR OF TEMP(BONUS) TO R5
         MVC   OBONUS(6),6(5)  COPY 6 BYT 6 OFF 5 INTO OBONUS
         XDECO 7,TEMP          CONVRT GROSS IN R7 TO CHARS IN TEMP
         LA    5,TEMP          LOAD ADDR OF TEMP(GROSS) INTO R5
         MVC   OGROSS(7),5(5)  COPY 7 BYTS 5 OFF R5 INTO 0GROSS
         ST    7,GROSS         STORE R7 CONTENTS INTO GROSS
         A     8,GROSS         ADD GROSS TO RUNNING TOTAL GROSS IN R8
         M     6,TXWTHHOL      R7 (GROSS)*(WITHHOLDING PERCENTAGE)
         M     6,=F'1'         ZERO OUT EVEN REG OF EVEN-ODD PAIR
         D     6,=F'100'       ((GROSS)*(WITHHOLDING PERCENTAGE))/100
         XDECO 7,TEMP          STORE WITHELD FOR OUTPUT
         LA    5,TEMP          LOAD ADDR OF TEMP(WITHHELD) INTO R5
         MVC   OWTHELD(6),6(5)  COPY 6 BYT 6 OFF 5 INTO OWTHELD
         L     5,GROSS         LOAD GROSS AMOUNT INTO R5 THEN
         SR    5,7             SUBTRACT R7(WITHELD) FROM R5(GROSS)
         XDECO 5,TEMP          CONVERT R5(NET) TO CHARS IN TEMP
         LA    10,TEMP         LOAD ADDR OF TEMP(NET) INTO R10
         MVC   ONET(7),5(10)   COPY 7 BYTS 5 OFF R5 INTO ONET
         L     6,=F'0'         INITIALIZE R6 TO 0 TO HOLD EMPLY DEDUCTN
         AR    6,4             ADD CURRENT EMPLOYEE DEDUCTION TO COUNT
         A     6,TDEDCT        ADD DUDUCTION TO TOTAL DEDUCTION COUNT
         ST    6,TDEDCT        STORE UPDATED TOTAL DEDUCTION IN TDEDCT
         L     3,=F'0'         LOAD REG 3 WITH 0 TO HOLD EMPLOYEE BONUS
         AR    3,7             ADD CURRENT EMPLOYEE WITHHELD TO REG 3
         A     3,TWITHLD       ADD WITHHELD TO TOTAL WITHHELD COUNT
         ST    3,TWITHLD       STORE UPDATED TOTAL WITHHELD IN TWITHLD
         L     2,=F'0'         LOAD REG 2 WITH 0 TO HOLD EMPLOYEE NET
         AR    2,5             ADD CURRENT EMPLOYEE NET TO REG 2
         A     2,TNET          ADD NET TO TOTAL NET COUNT
         ST    2,TNET          STORE UPDATED TOTAL NET IN TNET
*
*** <STEP 5> Prepare Detail line and print results
*
         XPRNT PRNTLINE,133    PRINT PRNTLINE
         XREAD RDBUFFER,80     READS 80 BYTES INTO RDBUFFER
         B     LOOP            BRANCH TO LOOP
ENDLOOP  DS    0H              BRANCH HERE WHEN NO MORE RECORDS TO READ
         XDECO 12,TEMP         CONVERT EMPLOYEE COUNT TO CHARACTERS
         LA    5,TEMP          LOAD ADDR OF TEMP(EMPLOYEE COUNT)INTO R5
         MVC   OEMPCNT(7),5(5) COPY 7 BYTS 5 OFF R5 IN TEMP FOR EMPLO
         XPRNT PRNTLN2,133     PRINT PRNTLN2
         L     6,TDEDCT        LOAD TOTAL DEDUCTIONS INTO R6
         XDECO 6,TEMP          CONVERT TOTAL DEDUCTION IN R6 TO CHARS
         LA    5,TEMP          LOAD ADDR OF TEMP(TOTAL DEDUCTN) INTO R5
         MVC   OTDEDCT(7),5(5) COPY 7 BYTS 5 OFF R5 IN OTDEDUCT
         XPRNT PRNTLN3,133     PRINT PRNTLN3
         L     6,TBONUS        LOAD TOTAL BONUS INTO R6
         XDECO 6,TEMP          CONVERT TBONUS IN R6 TO CHARS IN TEMP
         LA    5,TEMP          LOAD BEGINNING ADDR OF TEMP(TBONUS) R5
         MVC   OTBONUS(7),5(5) COPY 7 BYTS 5 OFF R5 TO MOVE T BONUS
         XPRNT PRNTLN4,133     PRINT PRINTLN4
         XDECO 8,TEMP          CONVERT TGROSS IN R8 TO CHARS IN TEMP
         LA    5,TEMP          LOAD BEGINNING ADDR OF TEMP(TGROSS) R5
         MVC   OTGROSS(7),5(5) MOVE 7 BYTS 5 OFF R5 TO MOVE T GROSS
         XPRNT PRNTLN5,133     PRINT PRINTLN5
         L     6,TWITHLD       LOAD BEGINNING ADDR OF TWITHLD INTO R6
         XDECO 6,TEMP          CONVERT TOTAL WITHHELD IN R6 TO CHARS
         LA    5,TEMP          LOAD BEGINNING ADDR TEMP(TOTAL WITHLD)R5
         MVC   OTWTHLD(7),5(5) COPY 7 BYTS 5 OFF R5 TO MOVE OTWTHLD
         XPRNT PRNTLN6,133     PRINT PRINTLN6
         L     6,TNET          LOAD ADDR OF TNET INTO R6
         XDECO 6,TEMP          CONVERT TOTAL NET IN R6 TO CHARS
         LA    5,TEMP          LOAD BEGINNING ADDR TEMP(TOTAL NET) R5
         MVC   OTNET(7),5(5)   MOVE 7 BYTS 5 OFF R5 TO MOVE OTNET
         XPRNT PRNTLN7,133     PRINT PRNTLN7
         LA    7,0(0,8)        LOAD ADDRESS OF TOTAL GROSS INTO R7
         M     6,=F'1'         ZERO OUT EVEN REG OF EVEN-ODD PAIR 6 7
         DR    6,12            DIVIDE R7(TGROSS) BY R12(EMPLOYE COUNT)
         ST    7,AVGROSS       STORE (TGROSS/EMPCOUNT) INTO AVGROSS
         L     6,AVGROSS       LOAD ADDR OF AVGROSS INTO R6
         XDECO 6,TEMP          CONVERT AVERAGE GROSS IN R6 TO CHARS
         LA    5,TEMP           LOAD BEGINNING ADDR OF TEMP INTO R5
         MVC   OAVGROSS(7),5(5)  MOVE 7 BYTS 5 OFF R5 TO MOVE OAVGROSS
         XPRNT PRNTLN8,133      PRINT PRNTLN8
         LA    7,0(0,2)        LOAD ADDRESS OF TOTAL NET INTO R7
         M     6,=F'1'         ZERO OUT EVEN REG OF EVEN-ODD PAIR
         DR    6,12            DIVIDE R7(OAVNET) BY R12(EMPLOYE COUNT)
         ST    7,AVNET         STORE (TNET/EMPCOUNT) INTO OAVNET
         L     6,AVNET         LOAD ADDR OF OAVNET INTO R6
         XDECO 6,TEMP          CONVERT AVERAGE NET IN R6 TO CHARS
         LA    5,TEMP          LOAD BEGINNING ADDR OF TEMP(AVNET) TO R5
         MVC   OAVNET(7),5(5)  MOVE 7 BYTS 5 OFF R5 TO MOVE OAVNET
         XPRNT PRNTLN9,133     PRINT PRNTLN9
         BR    14              UNCONDITIONAL RETURN TO CALLER
*
* ====== Program storage =========================================
*
         LTORG
EMPCOUNT DC    F'0'       STORAGE TO COUNT NUMBER OF EMPLOYEES
TGROSS   DC    F'0'       TOTAL GROSS INITIALIZED TO 0
TDEDCT   DC    F'0'       TOTAL DEDUCTION COUNTER INITIALIZED TO 0
TBONUS   DC    F'0'       TOTAL BONUS COUNTER INITIALIZED TO 0
TWITHLD  DC    F'0'       TOTAL WITHHELD COUNTER INITIALIZED TO 0
TNET     DC    F'0'       TOTAL NET COUNTER INITIALIZED TO 0
AVGROSS  DS    F          STORAGE TO HOLD AVERAGE GROSS
AVNET    DS    F          STORAGE TO HOLD AVERAGE NET
TXWTHHOL DS    F          STORAGE FOR TAX WITHHOLDING PERCENTAGE
SENRTY1  DS    F          STORAGE FOR BONUS MULTIPLIER 1
SENRTY2  DS    F             STORAGE FOR BONUS MULTIPLIER 2
SYMBL1   DC    F'1559623157'   SENIORITY CHARACTER INDICATOR 1
SYMBL2   DC    F'1324742133'   SENIORITY CHARACTER INDICATOR 2
SENRTY   DS    F             STORAGE FOR SENIORITY LEVEL
RATE     DS    F          FULLWORD TO HOLD RATE BINARY DATA
HOURS    DS    F          HOURS FIELD
DEDUCTN  DS    F          STORAGE FOR DEDUCTION
BONUS    DS    F          STORAGE FOR BONUS
GROSS    DS    F          STORAGE FOR GROSS
WITHHELD DS    F          STORAGE FOR WITHHELD
NET      DS    F          STORAGE FOR NET
TEMP     DS    0H         TEMP STORAGE FOR XDECO DATA
RDBUFFER DS    CL80       STORAGE FOR READ BUFFER
PRNTLINE DC    C'0'       SET DOUBLE SPACING ON THIS LINE
EMPLYNAM DS    CL25       EMPLYNAM FIELD
ID       DS    CL5        ID FIELD
ORATE    DS    CL5        RATE FIELD
OHOURS   DS    CL5        OUTPUT HOURS FIELD
ODUDUCT  DS    CL5        DEDUCTN FIELD
OBONUS   DS    CL6        BONUS FIELD
OGROSS   DS    CL7        OUTPUT GROSS FIELD
OWTHELD  DS    CL6        OUPUT FOR WITHELD
ONET     DS    CL7        OUTPUT NET FIELD
         DC    61C' '
PRNTLN2  DC    C'0'                   SET DOUBLE SPACING ON THIS LINE
EMPCTFLD DC    C' Total employees:'   EMPLOYEE COUNT FIELD
OEMPCNT  DS    CL7                    OUTPUT FOR EMPLOYEE COUNT
         DC    110C' '                SPACING TO FILL LEFTOVER BLANKS
PRNTLN3  DC    C'0'                   SET DOUBLE SPACING ON THIS LINE
TDEDFLD  DC    C'Total deductions:'   TOTAL DEDUCTIONS FIELD
OTDEDCT  DS    CL7                    OUTPUT FOR TOTAL DEDUCTIONS
         DC    110C' '                110 CHARACTER SPACES
PRNTLN4  DC    C'0'                   SET DOUBLE SPACING ON THIS LINE
TBNUSFLD DC    C'   Total bonuses:'   TOTAL BONUS FIELD
OTBONUS  DS    CL7                    OUTPUT STORAGE FOR TOTAL BONUS
         DC    110C' '                SPACING TO FILL LEFTOVER BLANKS
PRNTLN5  DC    C'0'                   SET DOUBLE SPACING ON THIS LINE
TGRSSFLD DC    C'     Total gross:'   TOTAL GROSS FIELD
OTGROSS  DS    CL7                    OUTPUT STORAGE FOR TOTAL GROSS
         DC    110C' '                SPACING TO FILL LEFTOVER BLANKS
PRNTLN6  DC    C'0'                   SET DOUBLE SPACING ON THIS LINE
TWITHFLD DC    C'  Total withheld:'   TOTAL WITHHELD FIELD
OTWTHLD  DS    CL7                    OUTPUT STORAGE FOR TOTAL WITHHLD
         DC    110C' '                SPACING TO FILL LEFTOVER BLANKS
PRNTLN7  DC    C'0'                   SET DOUBLE SPACING ON THIS LINE
TNETFLD  DC    C'       Total net:'   TOTAL NET FIELD
OTNET    DS    CL7                    STORAGE TO HOLD TOTAL NET
         DC    110C' '                SPACING TO FILL LEFTOVER BLANKS
PRNTLN8  DC    C'0'                   SET DOUBLE SPACING ON THIS LINE
AGROSFLD DC    C'   Average gross:'   AVERAGE GROSS FIELD
OAVGROSS DS    CL7                    STORAGE TO HOLD AVERAGE GROSS
         DC    110C' '                SPACING TO FILL LEFTOVER BLANKS
PRNTLN9  DC    C'0'                   SET DOUBLE SPACING ON THIS LINE
ANETFLD  DC    C'     Average net:'   AVERAGE NET FIELD
OAVNET   DS    CL7                    STORAGE TO HOLD AVERAGE NET
         DC    110C' '                SPACING TO FILL LEFTOVER BLANKS
*
         END   PAYROLL1
/*
//*
//FT05F001 DD DSN=KC02038.CSCI360.DATAFA20(ASGN3DAT),DISP=SHR
//*
//FT06F001 DD SYSOUT=*
//*
//SYSPRINT DD SYSOUT=*
/*
