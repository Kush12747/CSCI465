//KC03C53A JOB ,'K.GANDHI',MSGCLASS=H
//*
//*****************************************************************
//*                                                               *
//*  CSCI 465               ASSIGNMENT 6           FALL 2023      *
//*                                                               *
//*  DEVELOPER NAME: KUSH GANDHI                                  *
//*        DUE DATE: 11/17/2023                                   *
//*                                                               *
//*  PURPOSE: THIS PROGRAM USES ASSEMBLER CODE TO READ IN DATA    *
//*            AND OUTPUT TO A TEMP DATASET AND SAVES IT IN THE   *
//*            LOADLIB.                                           *
//*                                                               *
//*****************************************************************
//*
//*****************************************************************
//*                                                               *
//* JSTEP01 - TAEKS ASSEMBLER SOURCE CODE AND COMPILES IT USING   *
//*           THE CURRENT COBOL COMPILER.                         *
//*                                                               *
//* SYSLIB    INPUT: LIBRARY CONTAINING ASSEMBLER MACROS          *
//* SYSIN:    INPUT: COMPILES THE ASSEMBLER SOURCE CODE           *
//*                                                               *
//* SYSPRINT  OUTPUT: LISTS ANY MESSAGES FROM COMPILATION PROCESS *
//* SYSLIN    OUTPUT: INDICATES NON-EXECUTABLE OBJECT MODULE      *
//*                                                               *
//*****************************************************************
//*
//JSTEP01  EXEC PGM=ASMA90,PARM=ASA
//*
//SYSLIB   DD DSN=SYS1.MACLIB,DISP=SHR
//*
//SYSIN    DD *
         PRINT  NOGEN                MACRO CALL SUPPRESSION
******************************************************************
*                                                                *
* PROGRAM NAME: SALESRPT                                         *
* PROGRAMMER:   KUSH GANDHI                                      *
* DUE DATE:     11/17/2023                                       *
*                                                                *
* FUNCTION:  THIS PROGRAM READS INPUT DATA RECORDS AND THEN      *
*            WRITES THEM TO STANDARD OUTPUT.                     *
*                                                                *
* INPUT:     INDATA - THE INPUT DATA RECORDS READ IN.            *
*                                                                *
* OUTPUT:    REPORT - THE INPUT DATA RECORDS WRITTEN OUT.        *
*                                                                *
* ENTRY CONDITIONS: NONE.                                        *
*                                                                *
* EXIT CONDITIONS:  R15 RETURNS A RETURN CODE.                   *
*                                                                *
* NOTES: NONE.                                                   *
*                                                                *
******************************************************************
*
$FUNDREC DSECT  0H
$ISHRPRC   DS   ZL5
$IFUNDNME  DS   CL35
$ICOMPCT1  DS   ZL6
$ICOMPCT2  DS   ZL6
$ICOMPCT3  DS   ZL6
           DS   CL22
*
$SALEREC DSECT  0H
$IBRNCNME  DS   CL20
$IBRKRNME  DS   CL25
$IDEPAMT   DS   ZL10
           DS   CL4
$ICOMMFLG  DS   ZL1
           DS   CL20
*
SALESRPT CSECT                       BEGIN SALESRPT
*
         STM   14,12,12(13)          SAVE CALLER'S REGS
         LR    12,15                 SET R12 TO R15
         USING SALESRPT,12           ESTABLISH R12 AS 1ST BASE REG
         LA    14,REGS               R14 -> CURRENT SAVE AREA
         ST    13,4(,14)             SAVE CALLER'S SAVE AREA ADDR
         ST    14,8(,14)             SAVE CURRENT SAVE AREA ADDR
         LR    13,14                 R13 -> CURRENT SAVE AREA
*
*
         LINK  EP=JOBNAME,PARAM=JOBNAME,VL=1
*
* TIME AND DATE
*
         TIME  DEC,DATETIME,LINKAGE=SYSTEM,DATETYPE=MMDDYYYY
*
         MVC   ODATE(13),=X'40202120206120206120202020'
         ED    ODATE(13),DATETIME+7
         MVC   OTIME(6),=X'20207A2020'
         ED    OTIME(6),DATETIME
*
         OPEN  (SALESDCB,(INPUT))    OPEN INPUT FILE
         LTR   15,15                 CHECK FOR SUCCESSFUL OPEN
         BZ    OPENOK1               BRANCH IF SUCCESSFUL TO OPENOK1
         ABEND 222,DUMP              ABEND IF OPEN FAILS & DISPLAY 222
*
OPENOK1  OPEN  (RPTDCB,(OUTPUT))     OPEN OUTPUT FILE
         LTR   15,15                 CHECK FOR SUCCESSFUL OPEN
         BZ    OPENOK2               BRANCH IF SUCCESSFUL TO OPENOK2
         ABEND 333,DUMP              ABEND IF OPEN FAILS & DISPLAY 333
*
OPENOK2  OPEN  (HSDCB,(OUTPUT))      OPEN INPUT FILE
         LTR   15,15                 CHECK FOR SUCCESSFUL OPEN
         BZ    OPENOK3               BRANCH IF SUCCESSFUL TO OPENNOK3
         ABEND 444,DUMP              ABEND IF OPEN FAILS & DISPLAYS 444
*
OPENOK3  GET   SALESDCB,RECORD       READ THE FIRST RECORD
*
         CLI   EOFFLAG,C'Y'          CHECK FOR END OF INPUT DATA
*
         BE    ENDPGM                BRANCH TO END OF PROGRAM
*
         LR    3,1                   LOCATION OF RECORD
*
         USING $FUNDREC,3            SET ADDRESS FOR DSECT
*
         PACK  PSHRPRC(3),$ISHRPRC(5)   SAVE THE SHARE PRICE TO STORAGE
         MVC   OFUNDNME(35),$IFUNDNME   MOVE FUND NAME TO HEADERS
         PACK  PCOMPCT1(4),$ICOMPCT1(6) SAVE COMM-PCT-1 TO STORAGE
         PACK  PCOMPCT2(4),$ICOMPCT2(6) SAVE COMM-PCT-2 TO STORAGE
         PACK  PCOMPCT3(4),$ICOMPCT3(6) SACE COMM-PCT-3 TO STORAGE
*
         LA    1,OSHRPRC+3                     SET POSITION FOR $
         MVC   OSHRPRC(7),=X'402021204B2020'   EDIT PATTERN
         EDMK  OSHRPRC(7),PSHRPRC              EDIT TO PRINT LINE
         BCTR  1,0                             DECREMENT BY 1
         MVI   0(1),C'$'                       PLACE A $
*
         DROP  3                     DROP ADDRESSIBILITY
*
         LA    2,99                  LINE COUNTER SET TO 99
*
         USING $SALEREC,3            SET ADDRESS FOR DSECT
*
         GET   SALESDCB,RECORD       READ THE NEXT RECORD
*
LOOP1    CLI   EOFFLAG,C'Y'
         BE    ENDLOOP1
*
         MVI   DETAIL+1,C' '
         MVC   DETAIL+2(131),DETAIL+1
*
         MVC   RECORD(80),0(1)
*
         AP    PSALECTR(2),=PL1'1'   ADD 1 TO RECORD COUNTER
*
*
         MVC   OBRNNME(20),$IBRNCNME    MOVE BRANCH NAME TO OUT
         MVC   OBRKRNME(25),$IBRKRNME   MOVE BROKER NAME TO OUT
         PACK  PDEPAMT(6),$IDEPAMT(10)  PACK DEPOSIT AMT
         LA    1,ODEPAMT+11             SET POSITION FOR $
*
         MVC   ODEPAMT(15),=X'402020206B2020206B2021204B2020'
*
         EDMK  ODEPAMT(15),PDEPAMT     EDIT TO PRINT LINE
         BCTR  1,0                     DECREMENT BY 1
         MVI   0(1),C'$'               PRINTS $
*
         AP    PTDEPAMT(7),PDEPAMT     ADD DEP AMT TO TOTAL
*
         ZAP   PCALC(11),PDEPAMT(6)    COPY DEPOSIT AMT TO PCALC
         SRP   PCALC(11),4,0           SHIFT LEFT 4 = 3 + 1
         DP    PCALC(11),PSHRPRC(3)    DEP AMT / SHR PRC = SHR AMT
         SRP   PCALC(8),64-1,5         SHIFT AND ROUND
         SRP   PCALC(8),1,0            ADD 0 AT THE END
*
         MVC   OSHRAMT(20),=X'4020206B2020206B2020206B2021204B20202020'
*
         ED    OSHRAMT(20),PCALC       EDIT TO PRINT LINE
*
         AP    PTSHRAMT(9),PCALC(8)    ADD SHR AMT TO TOTAL
*
         CP    PCALC(8),=PL8'50000.0000'  CHECK FOR HIGH SALES AMT
         BNH   COMFLG                     BRANCH TO COMMISSION FLAG
         MVC   HSBRKNME(25),$IBRKRNME     MOVE BROKER NAME TO HS
         ZAP   HSSHRAMT(8),PCALC(8)       COPY SHARE AMT TO HS
*
         PUT   HSDCB,HSREC                WRITE HIGH SALES
*
COMFLG   MVC   COMMFLAG(1),$ICOMMFLG      MOVE COMMISSION FLAG
         CLI   COMMFLAG,C'1'              CHECK IF 1
         BE    COMM1                      BRANCH TO CALCULATION
         CLI   COMMFLAG,C'2'              CHECK  IF 2
         BE    COMM1                      BRANCH TO CALCULATION
         CLI   COMMFLAG,C'3'              CHECK IF 3
         BE    COMM1                      BRANCH TO CALCULATION
*
COMM1    ZAP   PTCOMPCT(11),PDEPAMT(6)    COPY DEP AMT TO TEMP COMM
         MP    PTCOMPCT(11),PCOMPCT1(4)   MULTLIPLY BY COMMSSION PCT
         SRP   PTCOMPCT(11),64-5,5        ROUND
         LA    1,OCOMAMT+11               SET POSITION FOR $
         MVC   OCOMAMT(15),=X'402020206B2020206B2021204B2020'
         EDMK  OCOMAMT(15),PTCOMPCT+5     EDIT TO PRINT LINE
         BCTR  1,0                        DECREMENT BY 1
         MVI   0(1),C'$'                  PRINT A $
*
         AP    PTCOMAMT(7),PTCOMPCT+5(6)  ADD TO TOTAL COMMISSION AMT
*
         B     LINES                     BRANCH TO CHECK LINE COUNT
*
LINES    C     2,=F'18'                  CHECK TO SEE PRINT PAGES
         BL    NOHDRS1                   SKIP TO PRINT DETAILS
*
         AP    PPAGECTR(2),=PL1'1'       ADD 1 TO PAGE COUNTER
         MVC   OPAGECTR(4),=X'40202120'  EDIT PATTERN
         ED    OPAGECTR(4),PPAGECTR      EDIT PAGE COUNTER TO EDIT LINE
*
         PUT   RPTDCB
         MVC   0(133,1),HEADER1           PRINT HEADER 1
*
         PUT   RPTDCB
         MVC   0(133,1),HEADER2           PRINT HEADER 2
*
         PUT   RPTDCB
         MVC   0(133,1),HEADER3           PRINT HEADER 3
*
         PUT   RPTDCB
         MVC   0(133,1),COLHEAD           PRINT COULUMN HEADERS
*
         PUT   RPTDCB
         MVC   0(133,1),RECHYP            PRINT HYPHENS
*
         SR    2,2                        SET LINE COUNTER TO 0
*
NOHDRS1  PUT   RPTDCB
         MVC   0(133,1),DETAIL            PRINT DETAIL RECORDS
*
         LA    2,1(,2)                    ADD 1 TO LINE COUNTER
*
         GET   SALESDCB,RECORD            GET NEXT RECORD
*
         B     LOOP1                      BRANCH TO LOOP
*
ENDLOOP1 DS    0H
*
         AP    PPAGECTR(2),=PL1'1'        ADD 1 TO PAGE COUNTER
         MVC   OPAGECTR(4),=X'40202120'   EDIT PATTERN
         ED    OPAGECTR(4),PPAGECTR      EDIT PAGE COUNTER TO PRINT LIN
*
         PUT   RPTDCB
         MVC   0(133,1),HEADER1           PRINT HEADER 1
*
         PUT   RPTDCB
         MVC   0(133,1),HEADER2           PRINT HEADER 2
*
         PUT   RPTDCB
         MVC   0(133,1),HEADER3           PRINT HEADER 3
*
         PUT   RPTDCB
         MVC   0(133,1),TOTHEAD           PRINT TOTAL HEADER
*
         PUT   RPTDCB
         MVC   0(133,1),TOTHYPEN          PRINT TOTAL HYPHENS
*
         ED    OSALECTR(4),PSALECTR  MOVE RECORD COUNTER TO PRINT LINE
*
         LA    1,OTDEPAMT+13           SET REGISTER FOR $
         MVC   OTDEPAMT(18),=X'4020206B2020206B2020206B2021204B2020'
         EDMK  OTDEPAMT(18),PTDEPAMT
         BCTR  1,0                     DECREMENT BY 1
         MVI   0(1),C'$'               PRINTS A $
*
         ED    OTSHRAMT(23),PTSHRAMT   MOVE TOT SHARE AMT TO PRINT LINE
*
         LA    1,OTCOMAMT+6            SETS BYTE FOR $
*
         MVC   OTCOMAMT(18),=X'4020206B2020206B2020206B2021204B2020'
*
         EDMK  OTCOMAMT(18),PTCOMAMT   EDIT TO PRINT LINE
         BCTR  1,0                     DECREMENT BY 1
         MVI   0(1),C'$'               PRINT A $
*
         PUT   RPTDCB
         MVC   0(133,1),TOTALS       PRINTS TOTALS DATA
*
         CLOSE (SALESDCB,,HSDCB)     CLOSES THE 2 DCBs
*
         OPEN  (HSDCB,(INPUT))       OPEN HIGH SALES FILE
         LTR   15,15                 CHECK FOR SUCCESSFUL OPEN
         BZ    OPENOK4               BRANCH IF SUCCESSFUL TO OPENOK2
         ABEND 555,DUMP              ABEND IF OPEN FAILS & DISPLAY 555
*
OPENOK4  GET   HSDCB,HSREC
*
         ZAP   PSALECTR(2),=PL1'0'   SET RECORD COUNTER TO 0
         ZAP   PPAGECTR(2),=PL1'0'   SER PAGE COUNTER TO 0
*
LOOP2    CLI   EOFFLAG2,C'Y'
         BE    ENDLOOP2
*
         AP    PSALECTR(2),=PL1'1'   ADD 1 TO RECORD COUNTER
*
         MVC   OHBRKNME(25),HSBRKNME    MOVE BROKER NAME
         ED    OHSHRAMT(20),HSSHRAMT    CONVERT TO PRINTABLE EBCDIC
*
         AP    PHSTSHRS(9),HSSHRAMT     ADD TO TOTAL HS SHARES
*
         C     2,=F'18'                 CHECK TO SEE PRINT PAGES
         BL    NOHDRS2                  BRANCH TO NOHDRS2
*
         AP    PPAGECTR(2),=PL1'1'       ADD 1 TO PAGE COUNTER
         MVC   OPAGECTR(4),=X'40202120'  EDIT PATTERN
         ED    OPAGECTR(4),PPAGECTR      EDIT PAGE COUNTER TO EDIT LINE
*
         PUT   RPTDCB
         MVC   0(133,1),HEADER1          PRINT HEADER1
*
         PUT   RPTDCB
         MVC   0(133,1),HEADER2          PRINT HEADER2
*
         PUT   RPTDCB
         MVC   0(133,1),HEADER3          PRINT HEADER3
*
         PUT   RPTDCB
         MVC   0(133,1),HSHEAD           PRINT HEADER COLUMNS
*
         PUT   RPTDCB
         MVC   0(133,1),HSHYPHEN         PRINT HYPHENS
*
         SR    2,2                       SET LINE COUNTER TO 0
*
NOHDRS2  PUT   RPTDCB
         MVC   0(133,1),HSREC            PRINT HIGH SALES RECORDS
*
         LA    2,1(,2)                   ADD 1 TO LINE COUNTER
*
         GET   HSDCB,HSREC               GET THE NEXT RECORD
*
         B     LOOP2                     BRANCH BACK TO LOOP2
*
ENDLOOP2 DS    0H
*
         AP    PPAGECTR(2),=PL1'1'       ADD 1 TO PAGE COUNTER
         MVC   OPAGECTR(4),=X'40202120'  EDIT PATTERN
         ED    OPAGECTR(4),PPAGECTR      EDIT PAGE COUNTER TO PRINT LIN
*
         PUT   RPTDCB
         MVC   0(133,1),HEADER1          PRINT HEADER1
*
ENDPGM   CLOSE (HSDCB,,RPTDCB)       CLOSE THE TWO DCBs
*
         SR    15,15                 SET RC TO 0
         L     13,4(,13)             R13 -> CALLER'S SAVE AREA
         L     14,12(,13)            RESTORE R14
         LM    0,12,20(13)           RESTORE R0 THROUGH R12
*
         BR    14                    RETURN TO CALLER
*
         ORG   SALESRPT+((*-SALESRPT+31)/32)*32
         DC    C'HERE IS THE STORAGE FOR SALESRPT'
*
         LTORG                       LTORG TO ORGANIZE LITERALS
*
REGS     DC    18F'0'                MAINREGS FOR STANDARD LINKAGE
DATETIME DS    4F
COMMFLAG DS    ZL1
RECORD   DS    CL80
*
PSALECTR DC    PL2'0'                PACKED RECORD CTR FIELD
PPAGECTR DC    PL2'0'                PACKED PAGER COUNTER
*
* TEMP VARIABLES
*
PCALC    DC    PL11'0'               PACKED TEMP CALC
PTCOMPCT DC    PL11'0'               PACKED TEMP COMMISSION
*
PSHRPRC  DC    PL3'0'                PACKED SHARE PRICE
PCOMPCT1 DC    PL4'0'                PACKED COMMISSION PERCENT 1
PCOMPCT2 DC    PL4'0'                PACKED COMMISSION PERCENT 2
PCOMPCT3 DC    PL4'0'                PACKED COMMISSION PERCENT 3
PDEPAMT  DC    PL6'0'                PACKED DEPOSIT AMOUNT
PTDEPAMT DC    PL7'0'                PACKED TOTAL DEPOSIT AMOUNT
PSHRAMT  DC    PL8'0'                PACKED SHARE AMOUNT
PTSHRAMT DC    PL9'0'                PACKED TOTAL SHARE AMOUNT
PCOMMAMT DC    PL6'0'                PACKED COMMISSION AMOUNT
PTCOMAMT DC    PL7'0'                PACKED TOTAL COMMISSION AMOUNT
*
* HIGH SALES VARIABLES
*
PHSTSHRS DC    PL9'0'
*
HSREC    DS    0H                    HIGH SALES RECORDS
HSBRKNME DS    CL25                  HIGH SALES BROKER NAME
HSSHRAMT DS    PL8                   HIGH SALES SHARE AMOUNT
*
* OUTPUT DETAIL LINE
*
DETAIL   DC    C'0'                  DETAIL LINE OF RECORDS
OBRNNME  DS    CL25                  BRANCH NAME
         DC    8C' '                 SPACES
OBRKRNME DS    CL25                  BROKER NAME
         DC    8C' '                 SPACES
ODEPAMT  DS    CL15                  DEPOSIT AMOUNT
         DC    8C' '                 SPACES
OSHRAMT  DS    CL20                  SHARE AMOUNT
         DC    8C' '                 SPACES
OCOMAMT  DS    CL15                  COMMISSION AMOUNT
*
* HIGH SALES RECORDS
*
HSRECORD DC    C'0'
         DC    37C' '
OHBRKNME DS    CL25
         DC    13C' '
OHSHRAMT DC    X'4020206B2020206B2020206B2021204B20202020'
         DC    37C' '
*
* TOTALS LINE
*
TOTALS   DC    C'0'
         DC    50C' '
OSALECTR DC    X'40202120'
         DC    9C' '
OTDEPAMT DS    CL18
         DC    4C' '
OTSHRAMT DC    X'40206B2020206B2020206B2020206B2021204B20202020'
         DC    6C' '
OTCOMAMT DS    CL18
*
* HIGH SALES TOTALS
*
HSTOTALS DC    C'0'
         DC    58C' '
OHSCTR   DC    X'40202120'
         DC    10C' '
OHSTSHRS DC    X'40206B2020206B2020206B2020206B2021204B20202020'
         DC    37C' '
*
* HEADER 1
*
HEADER1  DC    C'0'
ODATE    DS    CL10
         DC    39C' '
OFUNDNME DS    CL35
         DC    38C' '
         DC    C'PAGE: '
OPAGECTR DS    CL3
         DC    7C' '
*
* HEADER 2
*
HEADER2  DC    C'0'
OTIME    DS    CL6
         DC    43C' '
OTITLE   DC    C'BROKER SALES AND COMMISSION REPORT'
         DC    32C' '
         DC    C'JOBNAME: '
JOBNAME  DC    C'JOBNAME '
*
* HEADER 3
*
HEADER3  DC    C'0'
         DC    55C' '
         DC    C'SHARE PRICE: '
OSHRPRC  DS    CL7
         DC    57C' '
*
* COLUMN HEADERS
*
COLHEAD  DC    C'0'
         DC    C'BRANCH NAME'
         DC    22C' '
         DC    C'BROKER NAME'
         DC    22C' '
         DC    C'DEPOSIT AMOUNT'
         DC    16C' '
         DC    C'SHARE AMOUNT'
         DC    14C' '
         DC    C'COMMISSION'
*
* HIGH SALES COLUMN HEADERS
*
HSHEAD   DC    C'0'
         DC    40C' '
         DC    C'HIGH SALES BROKER NAME'
         DC    10C' '
         DC    C'HIGH SALES SHARE AMOUNT'
         DC    37C' '
*
* TOTALS COLUMN HEADER
*
TOTHEAD  DC    C'0'
         DC    39C' '
         DC    C'NUMBER OF SALES'
         DC    9C' '
         DC    C'TOTAL SALES AMOUNT'
         DC    9C' '
         DC    C'TOTAL SHARE AMOUNT'
         DC    8C' '
         DC    C'TOTAL COMMISSION'
         DC    67C' '
*
* HIGHSALES TOTAL COLUMN HEADER
*
HSTOTCOL DC    C'0'
         DC    34C' '
         DC    C'NUMBER OF HIGH SALES BROKERS'
         DC    4C' '
         DC    C'TOTAL HIGH SALES SHARE AMOUNT'
         DC    37C' '
*
* HYPHENS FOR SALES COLUMNS
*
RECHYP   DC    C' '
         DC    20C'-'
         DC    13C' '
         DC    25C'-'
         DC    9C' '
         DC    14C'-'
         DC    9C' '
         DC    18C'-'
         DC    10C' '
         DC    14C'-'
*
* HYPHENS FOR HIGHSALES
*
HSHYPHEN DC    C' '
         DC    37C' '
         DC    24C'-'
         DC    10C' '
         DC    22C'-'
         DC    39C' '
*
* TOTALS HYPHENS
*
TOTHYPEN DC    C' '
         DC    39C' '
         DC    15C'-'
         DC    9C' '
         DC    18C'-'
         DC    5C' '
         DC    22C'-'
         DC    6C' '
         DC    18C'-'
*
* HYPHENS FOR HIGHSALES TOTALS
*
*
HSTOTHYP DC     C' '
         DC     34C' '
         DC     28C'-'
         DC     4C' '
         DC     29C'-'
         DC     37C' '
*
*  INPUT DCB
*
SALESDCB  DCB  DDNAME=SALEFLE,                                         X
               DEVD=DA,                                                X
               DSORG=PS,                                               X
               MACRF=GM,                                               X
               RECFM=FB,                                               X
               LRECL=80,                                               X
               EODAD=EOFRTN
*
*  INPUT DCB'S EOF FLAG AND EOF ROUTINE
*
EOFFLAG  DC    C'N'                 EOFFLAG FOR INPUT FILE
*
EOFRTN   MVI   EOFFLAG,C'Y'          SET EOF FLAG TO Y
         BR    14                    BRANCH TO STMT FOLLOWING GET
*
*  OUTPUT DCB
*
RPTDCB   DCB   DDNAME=RPTFILE,                                         X
               DEVD=DA,                                                X
               DSORG=PS,                                               X
               MACRF=PL,                                               X
               RECFM=FBA,                                              X
               LRECL=133,                                              X
               BLKSIZE=2660
*
*
*
HSDCB    DCB   DDNAME=HSFILE,                                          X
               DEVD=DA,                                                X
               DSORG=PS,                                               X
               MACRF=(GM,PM),                                          X
               RECFM=FB,                                               X
               LRECL=133,                                              X
               BLKSIZE=2660,                                           X
               EODAD=EOFRTN2
*
*  INPUT DCB'S EOF FLAG AND EOF ROUTINE
*
EOFFLAG2 DC    C'N'                  EOFFLAG FOR INPUT FILE
*
EOFRTN2  MVI   EOFFLAG2,C'Y'         SET EOF FLAG TO Y
         BR    14                    BRANCH TO STMT FOLLOWING GET
*
         END   SALESRPT
/*
//*
//SYSLIN   DD DSN=&&OBJMOD,
//            SPACE=(CYL,(1,1)),
//            DISP=(MOD,PASS)
//*
//SYSPRINT DD SYSOUT=*
//*
//**************************************************************
//*                                                            *
//* THE FOLLOWING DATA SETS ARE REQUIRED BY THE COMPILER.      *
//*                                                            *
//**************************************************************
//*
//SYSUT1   DD SPACE=(CYL,(1,1))
//*
//***************************************************************
//*                                                             *
//* JSTEP02 - BINDS AND CREATES A LOAD MODULE.                  *
//*                                                             *
//* SYSLIB    INPUT: REQUIRED LIBRARY NEEDED FOR LOAD MODULE    *
//* SYSLIN    INPUT: OBJECT MODULE                              *
//*                                                             *
//* SYSLMOD   OUTPUT: LOAD MODULE CREATED BY BINDER             *
//* SYSPRINT  OUTPUT: DIAGNOSTIC MESSAGES                       *
//*                                                             *
//***************************************************************
//*
//JSTEP02  EXEC PGM=IEWBLINK,COND=(0,LT)
//*
//SYSLIB   DD DSN=CEE.SCEELKED,DISP=SHR
//*
//SYSLIN   DD DSN=&&OBJMOD,DISP=(OLD,DELETE,DELETE)
//*
//SYSLMOD  DD DSN=KC03C53.CSCI465.LOADLIB(SALESRPT),
//            SPACE=(1024,(50,20,1)),
//            DSNTYPE=LIBRARY,
//            DISP=(MOD,KEEP,KEEP)
//*
//SYSPRINT DD SYSOUT=*
//*
//***************************************************************
//*                                                             *
//* JSTEP03 - FETCHES THE PROGRAM DATA AND EXECUTES IT.         *
//*                                                             *
//* STEPLIB   INPUT: LOCATION OF PROGRAM OBJECT COBOL3          *
//* INDATA    INPUT: INPUT FILE DATA                            *
//*                                                             *
//* RPTDATA   OUTPUT: OUTPUT OF THE PROGRAM                     *
//* SYSDUMP   OUTPUT: ABEND DUMP                                *
//*                                                             *
//***************************************************************
//*
//JSTEP03  EXEC PGM=SALESRPT,COND=(0,LT)
//*
//STEPLIB  DD DSN=KC03C53.CSCI465.LOADLIB,DISP=SHR
//         DD DSN=KC02322.CSCI465.LOADLIB,DISP=SHR
//*
//SALEFLE  DD DSN=KC02322.CSCI465.DATAFA23(DATA6),DISP=SHR
//*
//HSFILE   DD SPACE=(TRK,(1,1))
//*
//RPTFILE  DD SYSOUT=*
//*
//SYSDUMP  DD SYSOUT=*
//
