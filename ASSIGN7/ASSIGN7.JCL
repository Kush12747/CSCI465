//KC03C53A JOB ,'K.GANDHI',MSGCLASS=H
//*
//*****************************************************************
//*                                                               *
//*  CSCI 465               ASSIGNMENT 7           FALL 2023      *
//*                                                               *
//*  DEVELOPER NAME: KUSH GANDHI                                  *
//*        DUE DATE: 12/8/2023                                    *
//*                                                               *
//*  PURPOSE:  THIS ASSIGNMENT IS ABOUT BUILDING A MORE           *
//*  COMPLEX SALES REPORT.                                        *
//*                                                               *
//*****************************************************************
//*
//*****************************************************************
//*                                                               *
//* JSTEP01 - DFSORT STEP TO SORT THE PDSE MEMEBER FUNDS.         *
//*                                                               *
//* SORTIN    INPUT: FUND FILE TO BE SORTED                       *
//* SYSIN     INPUT: COMPILES THE CODE                            *
//*****************************************************************
//*
//JSTEP01  EXEC PGM=SORT,PARM='CMP=CLC'
//*
//SORTIN   DD DSN=KC02322.CSCI465.DATAFA23(FUNDS),DISP=SHR
//SORTOUT  DD DSN=&&SORTED,SPACE=(TRK,(1,5)),DISP=(NEW,PASS)
//*
//SYSOUT   DD SYSOUT=*
//*
//SYSIN    DD *
  SORT FIELDS=(1,3,ZD,A)
/*
//*
//***************************************************************
//*                                                             *
//* JSTEP02 - COBOL SOURCE CODE SALESRPT                        *
//*                                                             *
//* SYSLIB    INPUT: LIBRARY CONTAINING COBOL MACROS            *
//* SYSIN:    INPUT: COMPILES THE COBOL SOURCE CODE             *
//*                                                             *
//* SYSPRINT  OUTPUT: LISTS ANY MESSAGES FROM COMPILATION       *
//* SYSLIN    OUTPUT: INDICATES NON-EXECUTABLE OBJECT MODULE    *
//*                                                             *
//***************************************************************
//*
//JSTEP02 EXEC PGM=IGYCRCTL,PARM=APOST,REGION=0M,COND=(0,LT)
//*
//STEPLIB  DD DSN=IGY630.SIGYCOMP,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//         DD DSN=CEE.SCEERUN2,DISP=SHR
//*
//SYSLIB   DD DSN=KC02322.CSCI465.COPYLIB,DISP=SHR
//*
//SYSIN    DD *
      ********************************************************
      *                                                      *
      * PROGRAM NAME: SALESRPT                               *
      * PROGRAMMER:   KUSH GANDHI                            *
      * DUE DATE:     12/08/2023                             *
      *                                                      *
      * FUNCTION: THIS PROGRAM READS INPUT DATA RECORDS      *
      *           OF A SALES REPORT AND WRITES A REPORT OUT. *
      *                                                      *
      * INPUT: SALESFLE - THE INPUT DATA RECORDS READ IN.    *
      *                                                      *
      * OUTPUT: RPTFLE - THE INPUT DATA RECORDS WRITTEN      *
      *                  MAKING A REPORT OUT.                *
      *                                                      *
      * NOTES: NONE                                          *
      *                                                      *
      ********************************************************

       IDENTIFICATION DIVISION.

       PROGRAM-ID.    SALESRPT.
       AUTHOR.        KUSH GANDHI.
       DATE-WRITTEN.  11/15/2023.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT SALES-FILE ASSIGN TO SALESFLE.
           SELECT REPORT-FILE ASSIGN TO RPTFLE.
           SELECT HIGH-SALES-FILE ASSIGN TO HSFLE.

       DATA DIVISION.
       FILE SECTION.
      ********************************************************
      * SALES-FILE IS A FILE THAT CONTAINES RECORDS OF       *
      * BRANCH NAMES, BROKER NAMES, AND AMOUNT. ALSO, AN     *
      * UNKNOWN NUMBER OF RECORDS.                           *
      *                                                      *
      * FIELD:              DESCRIPITION:                    *
      *                                                      *
      * SALES-RECORD        INPUT VARIABLES USED TO COLLECT  *
      *                     VALUES AND STORE THEM            *
      *                                                      *
      * FUND-RECORD         READS THE FIRST FILE OF DATA     *
      *                                                      *
      * HIGH-SALES-RECORD   PROCESS HIGH SALES RECORDS       *
      *                                                      *
      * VARIABLES:                                           *
      *                                                      *
      * IN-BRANCH-NME       BRANCH NAME FROM SALES-FILE      *
      * IN-BROKER-NME       BROKER NAME FROM SALES-FILE      *
      * IN-DEP-NME          DEPARTMENT NAME FROM SALES-FILE  *
      * IN-SHR-PRC          SHARE PRICE                      *
      * IN-FUND-NME         FUND NAME                        *
      * IN-COMM-PCT-1,2,3   COMMISION PERCENT                *
      * IN-HS-BROKER-NME    HIGH SALES BROKER NAME           *
      * IN-HS-SHR-AMT       HIGH SALES SHARE AMOUNT          *
      ********************************************************

      *******************************************************
      * SALES-FILE IS A FILE THAT PROCESS AND READS IN      *
      * RECORDS LIKE COMMISSION PERCENT, FUND NAME, BRANCH  *
      * NAME, BROKER NAME, DEPOSIT AMT, ETC.                *
      *******************************************************
       FD  SALES-FILE
           RECORDING MODE F.

       COPY SALESREC.

      *******************************************************
      * HIGH-SALES-FILE IS A FILE FOR A TEMPORARY DATA SET  *
      * TO PROCESS BROKER THAT HAVE SOLD MORE THAN 10,000   *
      * SHARES WHICH TAKES THE NAME AND AMOUNT.             *
      *******************************************************
       FD  HIGH-SALES-FILE
           RECORDING MODE F.

       COPY HSREC.
      *******************************************************
      * REPORT-FILE CONTAINS A GENERIC PRINT LINE THAT IS   *
      * USED TO PRINT A COMPLEX SALES REPORT FROM INPUT     *
      * VARIABLES.                                          *
      *******************************************************
       FD  REPORT-FILE
           RECORDING MODE F.

       01  REPORT-RECORD             PIC X(132).

       WORKING-STORAGE SECTION.

      *******************************************************
      * VARIABLES:                                          *
      *                                                     *
      * EOF-FLAG         CHANGED TO 'Y' WHEN END OF FILE    *
      *                  OCCURS.                            *
      *                                                     *
      * LINE-CTR         LINE COUNT.                        *
      * PAGE-CTR         PAGE COUNT.                        *
      * RECORD-CTR       NUMBER OF RECORDS COUNTER.         *
      * COMM-PCT         COMMISSION PERCENT                 *
      * SHR-PRC          PRICE OF A SHARE.                  *
      * DEP-AMT          DEPOSIT AMOUNT.                    *
      * TOT-DEP-AMT      USED TO CALCULATE TOTAL DEPOSIT    *
      *                  AMOUNT.                            *
      * TOT-SHR-AMT      USED TO CALCULATE TOTAL SHARE      *
      *                  AMOUNT.                            *
      * TOT-COMM-AMT     USED TO CALCULATE TOTAL COMMISSION *
      *                  AMOUNT.                            *
      * HS-SHR-AMT       RECORDS THE BROKER > 50,000 SHARES *
      * HS-TOT-SHR-AMT   USED TO CALCULATE THE TOTAL        *
      *                  HIGH SALES AMOUNT.                 *
      *******************************************************

       01  EOF-FLAG                  PIC X     VALUE 'N'.
       01  PRC-SUB                   PIC S9    BINARY SYNC.
       01  BUILDTBL                  PIC X(8)  VALUE 'BUILDTBL'.

       01  FUND-TBL.
           05  TBL-FUND-CTR          PIC S9(3) BINARY SYNC VALUE 0.
           05  TBL-FUND-NTRY         OCCURS 1 TO 199
                                     DEPENDING ON TBL-FUND-CTR
                                     ASCENDING KEY TBL-FUND-NBR
                                     INDEXED BY TBL-FUND-NDX.
       COPY FUNDTBL.

       01  COUNTER.
           05  LINE-CTR              PIC S9(2) BINARY SYNC VALUE 99.
           05  BROKER-CTR            PIC S9(3) PACKED-DECIMAL VALUE 0.
           05  PAGE-CTR              PIC S9(3) PACKED-DECIMAL VALUE 0.
           05  RECORD-CTR            PIC S9(3) BINARY SYNC VALUE 0.

       01  CURRENT-DATE-FIELDS.
           05  CURRENT-DATE.
               10  CURRENT-YEAR      PIC 9(4).
               10  CURRENT-MONTH     PIC 9(2).
               10  CURRENT-DAY       PIC 9(2).
           05  CURRENT-TIME.
               10  CURRENT-HOUR      PIC 9(2).
               10  CURRENT-MINUTE    PIC 9(2).
               10  CURRENT-SECOND    PIC 9(2).
               10  CURRENT-MS        PIC 9(2).
           05  DIFF-FROM-GMT         PIC S9(4).

       01  VARIABLE-DATA.

           05 COMM-PCT               PIC V9999 BINARY SYNC VALUE 0.

           05 SHR-PRC                PIC S9(3)V99    PACKED-DECIMAL
                                                     VALUE 0.

           05 DEP-AMT                PIC S9(8)V99    PACKED-DECIMAL
                                                     VALUE 0.

           05 TOT-DEP-AMT            PIC S9(10)V99   PACKED-DECIMAL
                                                     VALUE 0.

           05 TOT-SHR-AMT            PIC S9(12)V9(3) PACKED-DECIMAL
                                                     VALUE 0.

           05 SHR-AMT                PIC S9(10)V9(3) PACKED-DECIMAL
                                                     VALUE 0.

           05 COMM-AMT               PIC S9(7)V99    PACKED-DECIMAL
                                                     VALUE 0.

           05 TOT-COMM-AMT           PIC S9(8)V99    PACKED-DECIMAL
                                                     VALUE 0.

           05 HS-SHR-AMT             PIC S9(8)V9(3)  PACKED-DECIMAL
                                                     VALUE 0.

           05 HS-TOT-SHR-AMT         PIC S9(10)V9(3) PACKED-DECIMAL
                                                     VALUE 0.
       01  DETAIL-LINE.
           05  OUT-BRANCH-NME        PIC X(21).
           05                        PIC X(2) VALUE SPACES.
           05  OUT-BROKER-NME        PIC X(25).
           05                        PIC X(1) VALUE SPACE.
           05  OUT-FUND-NBR          PIC ZZZ9.
           05                        PIC X(2) VALUE SPACES.
           05  OUT-FUND-NME          PIC X(25).
           05                        PIC X(1) VALUE SPACE.
           05  OUT-DEP-AMT           PIC $$$,$$$,$$9.99.
           05                        PIC X(2) VALUE SPACES.
           05  OUT-SHR-AMT           PIC Z,ZZZ,ZZZ,ZZ9.9999.
           05                        PIC X(1) VALUE SPACES.
           05  OUT-COMM-AMT          PIC $$$,$$$,$$9.99.

       01  TOTAL-LINE.
           05                        PIC X(49) VALUE SPACES.
           05  OUT-SALE-REC          PIC Z,ZZ9.
           05                        PIC X(13) VALUE SPACES.
           05  OUT-TOT-DEP-AMT       PIC $$$,$$$,$$9.99.
           05                        PIC X(11) VALUE SPACES.
           05  OUT-TOT-SHR-AMT       PIC ZZZ,ZZZ,ZZ9.9999.
           05                        PIC X(13) VALUE SPACES.
           05  OUT-TOT-COMM-AMT      PIC $$$$,$$9.99.

       01  HEADER-RECORD-1.
            05  OUT-CURRENT-DATE.
                10  CURRENT-MONTH     PIC 9(2).
                10                    PIC X      VALUE '/'.
                10  CURRENT-DAY       PIC 9(2).
                10                    PIC X      VALUE '/'.
                10  CURRENT-YEAR      PIC 9(4).
           05                         PIC X(38)  VALUE SPACES.
           05                         PIC X(35)
                             VALUE 'WASHINGTON & LINCOLN INVESTMENTS'.
           05                         PIC X(40)  VALUE SPACES.
           05                         PIC X(6)   VALUE 'PAGE: '.
           05 OUT-PAGE-CTR            PIC ZZ9.

        01 HEADER-RECORD-2.
           05 OUT-CURRENT-TIME.
              10 HEADER-HOUR          PIC 9(2).
              10                      PIC X(1)  VALUE ':'.
              10 HEADER-MINUTE        PIC 9(2).
              10                      PIC X(1)  VALUE SPACE.
           05 OUT-PM-AM               PIC X(2)  VALUE 'AM'.
           05                         PIC X(42) VALUE SPACES.
           05                         PIC X(37) VALUE
                               'BROKER DAILY FUND SALES REPORT'.
           05                         PIC X(29) VALUE SPACES.
           05                         PIC X(9)  VALUE 'JOBNAME: '.
           05 JOBNAME                 PIC X(8)  VALUE 'JOBNAME '.

        01 HEADER-RECORD-3.
           05                         PIC X(43) VALUE SPACES.
           05                         PIC X(45)
                 VALUE '*** BROKER DAILY FUND SALES REPORT TOTALS ***'.
           05                         PIC X(44) VALUE SPACES.

        01 HEADER-COL-1.
           05                         PIC X(89) VALUE SPACES.
           05                         PIC X(7)  VALUE 'DEPOSIT'.
           05                         PIC X(26) VALUE SPACES.
           05                         PIC X(10) VALUE 'COMMISSION'.

        01 HEADER-COL-2.
           05                         PIC X(11) VALUE 'BRANCH NAME'.
           05                         PIC X(12) VALUE SPACES.
           05                         PIC X(11) VALUE 'BROKER NAME'.
           05                         PIC X(15) VALUE SPACES.
           05                         PIC X(4)  VALUE 'FUND'.
           05                         PIC X(2)  VALUE SPACES.
           05                         PIC X(9)  VALUE 'FUND NAME'.
           05                         PIC X(26) VALUE SPACES.
           05                         PIC X(6)  VALUE 'AMOUNT'.
           05                         PIC X(9)  VALUE SPACES.
           05                         PIC X(11) VALUE 'SHARES SOLD'.
           05                         PIC X(10) VALUE SPACES.
           05                         PIC X(6)  VALUE 'AMOUNT'.

        01 HEADER-HYPHENS.
           05                         PIC X(21) VALUE ALL '-'.
           05                         PIC X(2)  VALUE SPACES.
           05                         PIC X(25) VALUE ALL '-'.
           05                         PIC X(1)  VALUE SPACE.
           05                         PIC X(4)  VALUE ALL '-'.
           05                         PIC X(2)  VALUE SPACES.
           05                         PIC X(25) VALUE ALL '-'.
           05                         PIC X(1)  VALUE SPACE.
           05                         PIC X(15) VALUE ALL '-'.
           05                         PIC X(2)  VALUE SPACES.
           05                         PIC X(18) VALUE ALL '-'.
           05                         PIC X(1)  VALUE SPACE.
           05                         PIC X(15) VALUE ALL '-'.

        01 HEADER-TOTALS.
           05                         PIC X(20) VALUE SPACES.
           05                         PIC X(7)  VALUE 'BROKERS'.
           05                         PIC X(10) VALUE SPACES.
           05                         PIC X(15) VALUE 'NUMBER OF SALES'.
           05                         PIC X(20) VALUE SPACES.
           05                         PIC X(18) VALUE
                                                'TOTAL DEPOSITS'.
           05                         PIC X(10) VALUE SPACES.
           05                         PIC X(17) VALUE
                                                'TOTAL COMMISSIONS'.
           05                         PIC X(15) VALUE SPACES.

        01 TOTAL-HYPHENS.
           05                         PIC X(20) VALUE SPACES.
           05                         PIC X(7)  VALUE ALL '-'.
           05                         PIC X(10) VALUE SPACES.
           05                         PIC X(15) VALUE ALL '-'.
           05                         PIC X(20) VALUE SPACES.
           05                         PIC X(18) VALUE ALL '-'.
           05                         PIC X(10) VALUE SPACES.
           05                         PIC X(17) VALUE ALL '-'.
           05                         PIC X(15) VALUE SPACES.

       PROCEDURE DIVISION.

      *******************************************************
      * 0000-MAIN. THIS ROUTINE CONTROLS THE FLOW OF THE    *
      * PROGRAM. IT CALLS ROUTINES TO (1) READ THE FIRST    *
      * RECORD, (2) INCREMENT THE UPPER-DIVISION AND LOWER- *
      * DIVISION TOTALS, AND (3) PRINT THE SUMMARY.         *
      *******************************************************
       0000-MAIN.

           PERFORM 0130-CURRENT-DATE-AND-TIME.

           CALL JOBNAME USING JOBNAME.

           IF HEADER-HOUR > 12
             COMPUTE HEADER-HOUR = HEADER-HOUR - 12
             MOVE CURRENT-HOUR TO HEADER-HOUR
             MOVE 'AM' TO OUT-PM-AM
           END-IF.

           CALL BUILDTBL USING FUND-TBL.

           PERFORM VARYING TBL-FUND-NDX FROM 1 BY 1
             UNTIL TBL-FUND-NDX > TBL-FUND-CTR
             DISPLAY TBL-FUND-NBR (TBL-FUND-NDX) ' '
                     TBL-FUND-NME (TBL-FUND-NDX) ' '
                     TBL-FUND-COMM-PCT (TBL-FUND-NDX)
             PERFORM VARYING TBL-PRC-NDX FROM 1 BY 1
               UNTIL TBL-PRC-NDX > 4
               DISPLAY TBL-FUND-SHR-PRC (TBL-FUND-NDX, TBL-PRC-NDX)
             END-PERFORM
           END-PERFORM.

           OPEN INPUT  SALES-FILE
                OUTPUT HIGH-SALES-FILE
                OUTPUT REPORT-FILE.

           READ SALES-FILE
             AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

           PERFORM 0100-PROCESS-RECORD
             UNTIL EOF-FLAG = 'Y'.

           READ SALES-FILE
             AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

           PERFORM 0120-PROCESS-TOTAL.

           MOVE 0 TO RECORD-CTR.

           MOVE 'N' TO EOF-FLAG.

           MOVE '0' TO PAGE-CTR.

           CLOSE SALES-FILE
                 HIGH-SALES-FILE
                 REPORT-FILE.

           CALL 'HISALES' USING FUND-TBL.

           MOVE 0 TO RETURN-CODE.

           STOP RUN.

       0000-EXIT. EXIT.

      *****************************************************
      * 0100-PROCESS-RECORD. THIS PROCESS MOVES RECORDS   *
      * FOR THE FIRST REPORT AND DOES SOME CALCULATION    *
      * FOR TOTALS, COMMISSION PERCENT AND MOVES ANY      *
      * RECORDS WITH SHR-AMT > 10,000 TO HIGH-SALES.      *
      *****************************************************

       0100-PROCESS-RECORD.

           ADD 1 TO BROKER-CTR.

           READ SALES-FILE
             AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

           IF SALE-NDX = 1
             MOVE SALE-BRANCH-NME TO OUT-BRANCH-NME
             MOVE SALE-BROKER-NME TO OUT-BROKER-NME
           ELSE
             MOVE SPACES TO OUT-BRANCH-NME
             MOVE SPACES TO OUT-BROKER-NME
           END-IF.

           MOVE SALE-DEP-AMT (SALE-NDX) TO OUT-DEP-AMT.
           MOVE SALE-FUND-NBR (SALE-NDX) TO OUT-FUND-NBR.

           COMPUTE TOT-DEP-AMT ROUNDED
           = SALE-DEP-AMT (SALE-NDX) + TOT-DEP-AMT.

           SEARCH ALL TBL-FUND-NTRY
             AT END
               PERFORM 0200-NO-FUND-FOUND
             WHEN TBL-FUND-NBR (TBL-FUND-NDX) = SALE-FUND-NBR (SALE-NDX)
               PERFORM 0210-FUND-FOUND
           END-SEARCH.

           IF LINE-CTR > 18
             ADD 1 TO PAGE-CTR

             MOVE PAGE-CTR TO OUT-PAGE-CTR

             WRITE REPORT-RECORD FROM HEADER-RECORD-1 AFTER PAGE
             WRITE REPORT-RECORD FROM HEADER-RECORD-2 AFTER 1
             WRITE REPORT-RECORD FROM HEADER-COL-1 AFTER 2
             WRITE REPORT-RECORD FROM HEADER-COL-2 AFTER 1
             WRITE REPORT-RECORD FROM HEADER-HYPHENS AFTER 1

             MOVE 0 TO LINE-CTR

           END-IF.

           WRITE REPORT-RECORD FROM DETAIL-LINE AFTER 2.

           ADD 1 TO LINE-CTR.

           READ SALES-FILE
             AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

       0100-EXIT. EXIT.

      *****************************************************
      * 0120-PROCESS-TOTAL. THIS PROCESS MOVE THE TOTALS  *
      * CALCULATED TO THE REPORT BY WRITE.                *
      *****************************************************

       0120-PROCESS-TOTAL.

           ADD 1 TO PAGE-CTR.

           MOVE PAGE-CTR TO OUT-PAGE-CTR.
           MOVE BROKER-CTR TO OUT-SALE-REC.
           MOVE TOT-DEP-AMT TO OUT-TOT-DEP-AMT.
           MOVE TOT-SHR-AMT TO OUT-TOT-SHR-AMT.
           MOVE TOT-COMM-AMT TO OUT-TOT-COMM-AMT.

           WRITE REPORT-RECORD FROM HEADER-RECORD-1 AFTER PAGE.
           WRITE REPORT-RECORD FROM HEADER-RECORD-2 AFTER 1.
           WRITE REPORT-RECORD FROM HEADER-RECORD-3 AFTER 1.
           WRITE REPORT-RECORD FROM HEADER-TOTALS AFTER 2.
           WRITE REPORT-RECORD FROM TOTAL-HYPHENS AFTER 1.
           WRITE REPORT-RECORD FROM TOTAL-LINE AFTER 2.

       0120-EXIT. EXIT.

      *****************************************************
      * 0140-CURRENT-DATE-AND-TIME. THIS PROCESS MOVE THE *
      * CURRENT DATE AND TIME.                            *
      *****************************************************

       0130-CURRENT-DATE-AND-TIME.

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-FIELDS.

           MOVE CORRESPONDING CURRENT-DATE TO OUT-CURRENT-DATE.

           MOVE CURRENT-HOUR TO HEADER-HOUR.
           MOVE CURRENT-MINUTE TO HEADER-MINUTE.

       0130-EXIT. EXIT.

      *****************************************************
      * 0300-NO-FUND-FOUND. IF NO FUND IS FOUND THEN      *
      * DISPLAY A MESSAGE SAYING NOT FOUND AND MOVES 0 TO *
      * SHR-AMT AND COMM.                                 *
      *****************************************************
       0200-NO-FUND-FOUND.

            MOVE '** SALE FUND NOT FOUND **' TO OUT-FUND-NME.
            MOVE 0 TO OUT-SHR-AMT OUT-COMM-AMT.
            MOVE 0 TO OUT-COMM-AMT.

       0300-EXIT. EXIT.

      *****************************************************
      * 0310-FUND-FOUND. IF FUND IS FOUND THEN MOVE DATA  *
      * TO OUT VARIABLES AND THEN CALL CALCSHRS BY        *
      * DEP-AMT, SHR-PRC, AND SHR-AMT.                    *
      *****************************************************
       0210-FUND-FOUND.

           MOVE TBL-FUND-NME (TBL-PRC-NDX) TO OUT-FUND-NME.
           MOVE SALE-DEP-AMT (SALE-NDX) TO DEP-AMT.
           MOVE SALE-PRC-FLG (SALE-NDX) TO PRC-SUB.
           MOVE TBL-FUND-SHR-PRC (TBL-FUND-NDX, PRC-SUB) TO SHR-PRC.
           MOVE TBL-FUND-COMM-PCT (TBL-PRC-NDX) TO COMM-PCT.

           CALL 'CALCSHRS' USING DEP-AMT
                                 SHR-PRC
                                 SHR-AMT.

       0310-EXIT. EXIT.
/*
//*
//SYSLIN   DD DSN=&&OBJMOD1,
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
//SYSUT2   DD SPACE=(CYL,(1,1))
//SYSUT3   DD SPACE=(CYL,(1,1))
//SYSUT4   DD SPACE=(CYL,(1,1))
//SYSUT5   DD SPACE=(CYL,(1,1))
//SYSUT6   DD SPACE=(CYL,(1,1))
//SYSUT7   DD SPACE=(CYL,(1,1))
//SYSUT8   DD SPACE=(CYL,(1,1))
//SYSUT9   DD SPACE=(CYL,(1,1))
//SYSUT10  DD SPACE=(CYL,(1,1))
//SYSUT11  DD SPACE=(CYL,(1,1))
//SYSUT12  DD SPACE=(CYL,(1,1))
//SYSUT13  DD SPACE=(CYL,(1,1))
//SYSUT14  DD SPACE=(CYL,(1,1))
//SYSUT15  DD SPACE=(CYL,(1,1))
//SYSMDECK DD SPACE=(CYL,(1,1))
//*
//*****************************************************************
//*                                                               *
//* JSTEP03 - ASSEMBLER FOR CALCSHRS.                             *
//*                                                               *
//* SYSLIB    INPUT: LIBRARY CONTAINING ASSEMBLER MACROS          *
//* SYSIN:    INPUT: COMPILES THE ASSEMBLER SOURCE CODE           *
//*                                                               *
//* SYSPRINT  OUTPUT: LISTS ANY MESSAGES FROM COMPILATION PROCESS *
//* SYSLIN    OUTPUT: INDICATES NON-EXECUTABLE OBJECT MODULE      *
//*                                                               *
//*****************************************************************
//JSTEP03  EXEC PGM=ASMA90,PARM=ASA,COND=(0,LT)
//*
//SYSLIB   DD DSN=SYS1.MACLIB,DISP=SHR
//*
//SYSIN    DD *
         PRINT  NOGEN                MACRO CALL SUPPRESSION
******************************************************************
*                                                                *
* PROGRAM NAME: CALCSHRS                                         *
* PROGRAMMER:   KUSH GANDHI                                      *
* DUE DATE:     12/08/2023                                       *
*                                                                *
******************************************************************
*
CALCSHRS CSECT
*
         STM   14,12,12(13)          SAVE CALLER'S REGS
         LR    12,15                 SET R12 TO R15
         USING CALCSHRS,12           ESTABLISH R12 AS 1ST BASE REG
         LA    14,REGS               R14 -> CURRENT SAVE AREA
         ST    13,4(,14)             SAVE CALLER'S SAVE AREA ADDR
         ST    14,8(,14)             SAVE CURRENT SAVE AREA ADDR
         LR    13,14                 R13 -> CURRENT SAVE AREA
*
         LM    2,4,0(1)              REG 2-4 = FUND TABLE
*                                    2 = DEP-AMT
*                                    3 = SHR-PRC
*                                    4 = SHR-AMT
*
         ZAP   PCALC(11),0(4,2)      COPY DEPOSIT AMT TO PCALC
         SRP   PCALC(11),4,0         SHIFT LEFT 4 = 3 + 1
         DP    PCALC(11),0(3,3)      DEP AMT / SHR PRC = SHR AMT
         SRP   PCALC(8),64-1,5       SHIFT AND ROUND
         SRP   PCALC(8),1,0          ADD 0 AT THE END
*
         L     13,4(,13)             R13 -> CALLER'S SAVE AREA
         L     14,12(,13)            RESTORE R14
         LM    0,12,20(13)           RESTORE R0 THROUGH R12
*
         BR    14                    RETURN TO CALLER
*
         ORG   CALCSHRS+((*-CALCSHRS+31)/32)*32
         DC    C'HERE IS THE STORAGE FOR CALCSHRS'
*
         LTORG                       LTORG TO ORGANIZE LITERALS
*
REGS     DC    18F'0'                MAINREGS FOR STANDARD LINKAGE
PCALC    DC    PL11'0'               USED FOR CALCULATION
*
         END CALCSHRS
/*
//*
//SYSLIN   DD DSN=&&OBJMOD2,
//            SPACE=(CYL,(1,1)),
//            DISP=(NEW,PASS,DELETE)
//*
//SYSPRINT DD SYSOUT=*
//*
//**************************************************************
//*                                                            *
//* THE FOLLOWING DATA SETS ARE REQUIRED BY THE COMPILER.      *
//*                                                            *
//**************************************************************
//SYSUT1   DD SPACE=(CYL,(1,1))
//*
//***************************************************************
//*                                                             *
//* JSTEP04 - COBOL SOURCE CODE FOR HISALES.                    *
//*                                                             *
//* SYSLIB    INPUT: LIBRARY CONTAINING COBOL MACROS            *
//* SYSIN:    INPUT: COMPILES THE COBOL SOURCE CODE             *
//*                                                             *
//* SYSPRINT  OUTPUT: LISTS ANY MESSAGES FROM COMPILATION       *
//* SYSLIN    OUTPUT: INDICATES NON-EXECUTABLE OBJECT MODULE    *
//*                                                             *
//***************************************************************
//JSTEP04  EXEC PGM=IGYCRCTL,REGION=0M,COND=(0,LT)
//*
//STEPLIB  DD DSN=IGY630.SIGYCOMP,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//         DD DSN=CEE.SCEERUN2,DISP=SHR
//*
//SYSLIB   DD DSN=KC02322.CSCI465.COPYLIB,DISP=SHR
//*
//SYSIN    DD *
      ********************************************************
      *                                                      *
      * PROGRAM NAME: HISALES                                *
      *                                                      *
      ********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HISALES.
       DATE-WRITTEN.  11/30/2023.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT HIGH-SALES-FILE ASSIGN TO HSFLE.

       DATA DIVISION.
       FILE SECTION.

       FD  HIGH-SALES-FILE
           RECORDING MODE F.

       COPY HSREC.

       WORKING-STORAGE SECTION.

       01  EOF-FLAG                  PIC X VALUE 'N'.

       01  HIGH-SALES-NME-AMT.
           05                        PIC X(37) VALUE SPACES.
           05 OUT-HS-BROKER-NME      PIC X(25).
           05                        PIC X(15) VALUE SPACES.
           05 OUT-HS-SHR-AMT         PIC Z,ZZZ,ZZZ,ZZ9.9999.
           05                        PIC X(37) VALUE SPACES.

       01  HIGH-SALES-TOTALS.
           05                        PIC X(57) VALUE SPACES.
           05  OUT-HS-SALE-CTR       PIC Z,ZZ9.
           05                        PIC X(15) VALUE SPACES.
           05  OUT-HS-TOT-SHR-AMT    PIC Z,ZZZ,ZZZ,ZZ9.9999.
           05                        PIC X(37) VALUE SPACES.

       PROCEDURE DIVISION.
/*
//*
//SYSLIN   DD DSN=&&OBJMOD3,
//            SPACE=(CYL,(1,1)),
//            DISP=(NEW,PASS,DELETE)
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
//SYSUT2   DD SPACE=(CYL,(1,1))
//SYSUT3   DD SPACE=(CYL,(1,1))
//SYSUT4   DD SPACE=(CYL,(1,1))
//SYSUT5   DD SPACE=(CYL,(1,1))
//SYSUT6   DD SPACE=(CYL,(1,1))
//SYSUT7   DD SPACE=(CYL,(1,1))
//SYSUT8   DD SPACE=(CYL,(1,1))
//SYSUT9   DD SPACE=(CYL,(1,1))
//SYSUT10  DD SPACE=(CYL,(1,1))
//SYSUT11  DD SPACE=(CYL,(1,1))
//SYSUT12  DD SPACE=(CYL,(1,1))
//SYSUT13  DD SPACE=(CYL,(1,1))
//SYSUT14  DD SPACE=(CYL,(1,1))
//SYSUT15  DD SPACE=(CYL,(1,1))
//SYSMDECK DD SPACE=(CYL,(1,1))
//*
//***************************************************************
//*                                                             *
//* JSTEP05 - BINDER FOR THE 3 OBJECT MODULES.                  *
//*                                                             *
//* SYSLIB    INPUT: REQUIRED LIBRARY NEEDED FOR LOAD MODULE    *
//* SYSLIN    INPUT: OBJECT MODULE                              *
//*                                                             *
//* SYSLMOD   OUTPUT: LOAD MODULE CREATED BY BINDER             *
//* SYSPRINT  OUTPUT: DIAGNOSTIC MESSAGES                       *
//*                                                             *
//***************************************************************
//JSTEP05  EXEC PGM=IEWBLINK,COND=(0,LT)
//*
//SYSLIB  DD DSN=CEE.SCEELKED,DISP=SHR
//*
//SYSLIN   DD DSN=&&OBJMOD1,DISP=(OLD,DELETE,DELETE)
//         DD DSN=&&OBJMOD2,DISP=(OLD,DELETE,DELETE)
//         DD DSN=&&OBJMOD3,DISP=(OLD,DELETE,DELETE)
//         DD *
  ENTRY SALESRPT
  MODE AMODE(24)
/*
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
//* JSTEP06 - COBOL SOURCE CODE FOR BUILDTBL                    *
//*                                                             *
//* SYSLIB    INPUT: LIBRARY CONTAINING COBOL MACROS            *
//* SYSIN:    INPUT: COMPILES THE COBOL SOURCE CODE             *
//*                                                             *
//* SYSPRINT  OUTPUT: LISTS ANY MESSAGES FROM COMPILATION       *
//* SYSLIN    OUTPUT: INDICATES NON-EXECUTABLE OBJECT MODULE    *
//*                                                             *
//***************************************************************
//JSTEP06  EXEC PGM=IGYCRCTL,REGION=0M,COND=(0,LT)
//*
//STEPLIB  DD DSN=IGY630.SIGYCOMP,DISP=SHR
//*
//SYSLIB   DD DSN=KC02322.CSCI465.COPYLIB,DISP=SHR
//*
//SYSIN    DD *
      ********************************************************
      *                                                      *
      * PROGRAM NAME: BUILDTBL                               *
      *                                                      *
      ********************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    BUILDTBL.
       AUTHOR.        KUSH GANDHI.
       DATE-WRITTEN.  11/30/2023.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT FUND-FILE ASSIGN TO FUNDFLE.
           SELECT PRICE-FILE ASSIGN TO PRICEFLE.

       DATA DIVISION.
       FILE SECTION.

       FD  FUND-FILE
           RECORDING MODE F.

       COPY FUNDREC.

       FD  PRICE-FILE
           RECORDING MODE F.

       COPY PRCREC.

       WORKING-STORAGE SECTION.
       01  EOF-FLAG                  PIC X VALUE 'N'.
       01  PRC-SUB                   PIC S9 BINARY SYNC.

      ************************************************************
      *LINKAGE SECTION                                           *
      *01  FUND-TBL                                              *
      *   05  TBL-FUND-CTR          PIC S9(3) BINARY SYNC        *
      *   05  TBL-FUND-NTRY         OCCURS 1 TO 199              *
      *                             DEPENDING ON TBL-FUND-CTR    *
      *                             ASCENDING KEY TBL-FUND-NBR   *
      *                             INDEXED BY TBL-FUND-NDX      *
      *COPY FUNDTBL                                              *
      *                                                          *
      *                                                          *
      * PROCEDURE DIVISION USING FUND-TBL.                       *
      *                                                          *
      * 0000-MAIN.                                               *
      *                                                          *
      *   OPEN INPUT FUND-FILE.                                  *
      *                                                          *
      *   READ FUND-FILE                                         *
      *     AT END MOVE 'Y' TO EOF-FLAG                          *
      *   END-READ.                                              *
      *                                                          *
      *   PERFORM VARYING TBL-FUND-NDX FROM 1 BY 1               *
      *     UNTIL TBL-FUND-NDX > 199                             *
      *     MOVE IN-FUND-NBR TO TBL-FUND-NBR (TBL-FUND-NDX)      *
      *     MOVE IN-FUND-NME TO TBL-FUND-NME (TBL-FUND-NDX)      *
      *     MOVE IN-FUND-COMM-PCT TO                             *
      *       TBL-FUND-COMM-PCT(TBL-FUND-NDX)                    *
      *     ADD 1 TO TBL-FUND-CTR                                *
      *   END-PERFORM.                                           *
      *                                                          *
      *   CLOSE FUND-FILE.                                       *
      *                                                          *
      *   OPEN INPUT PRICE-FILE.                                 *
      *                                                          *
      *   READ PRICE-FILE                                        *
      *     AT END MOVE 'Y' TO EOF-FLAG                          *
      *   END-READ.                                              *
      *                                                          *
      *   PERFORM VARYING TBL-PRC-NDX FROM 1 BY 1                *
      *     UNTIL TBL-PRC-NDX > 4                                *
      *     MOVE IN-FUND-SHR-PRC (PRC-SUB)                       *
      *     TO TBL-FUND-SHR-PRC (TBL-FUND-NDX, TBL-PRC-NDX)      *
      *     ADD 1 TO PRC-SUB                                     *
      *   END-PERFORM.                                           *
      *                                                          *
      *   CLOSE PRICE-FILE.                                      *
      *                                                          *
      *   MOVE 0 TO RETURN-CODE.                                 *
      *                                                          *
      *   GOBACK.                                                *
      *                                                          *
      * 0000-EXIT. EXIT.                                         *
      ************************************************************
/*
//*
//SYSLIN   DD DSN=&&OBJMOD4,
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
//SYSUT2   DD SPACE=(CYL,(1,1))
//SYSUT3   DD SPACE=(CYL,(1,1))
//SYSUT4   DD SPACE=(CYL,(1,1))
//SYSUT5   DD SPACE=(CYL,(1,1))
//SYSUT6   DD SPACE=(CYL,(1,1))
//SYSUT7   DD SPACE=(CYL,(1,1))
//SYSUT8   DD SPACE=(CYL,(1,1))
//SYSUT9   DD SPACE=(CYL,(1,1))
//SYSUT10  DD SPACE=(CYL,(1,1))
//SYSUT11  DD SPACE=(CYL,(1,1))
//SYSUT12  DD SPACE=(CYL,(1,1))
//SYSUT13  DD SPACE=(CYL,(1,1))
//SYSUT14  DD SPACE=(CYL,(1,1))
//SYSUT15  DD SPACE=(CYL,(1,1))
//SYSMDECK DD SPACE=(CYL,(1,1))
//*
//***************************************************************
//*                                                             *
//* JSTEP07 - BINDER FOR BUILDTBL WHICH CREATS A                *
//*           PROGRAM OBJECT.                                   *
//*                                                             *
//* SYSLIB    INPUT: REQUIRED LIBRARY NEEDED FOR LOAD MODULE    *
//* SYSLIN    INPUT: OBJECT MODULE                              *
//*                                                             *
//* SYSLMOD   OUTPUT: LOAD MODULE CREATED BY BINDER             *
//* SYSPRINT  OUTPUT: DIAGNOSTIC MESSAGES                       *
//*                                                             *
//***************************************************************
//JSTEP07 EXEC PGM=IEWBLINK,COND=(0,LT)
//*
//SYSLIB  DD DSN=CEE.SCEELKED,DISP=SHR
//*
//SYSLIN  DD DSN=&&OBJMOD4,DISP=(OLD,DELETE,DELETE)
//*
//SYSLMOD DD DSN=KC03C53.CSCI465.LOADLIB(BUILDTBL),
//           SPACE=(1024,(50,20,1)),
//           DSNTYPE=LIBRARY,
//           DISP=(MOD,KEEP,KEEP)
//*
//SYSPRINT DD SYSOUT=*
//*
//***************************************************************
//*                                                             *
//* JSTEP08 - FETCH AND EXECUTE SALESRPT.                       *
//*                                                             *
//* STEPLIB   INPUT: LOCATION OF PROGRAM OBJECT SALESRPT        *
//* FUNDFLE   INPUT: SORTED DATA FILE                           *
//* SALESFLE  INPUT: SALE RECORDS                               *
//* PRICEFLE  INPUT: PRICE RECORDS                              *
//*                                                             *
//* RPTFLE    OUTPUT: OUTPUT OF THE PROGRAM                     *
//* SYSDUMP   OUTPUT: ABEND DUMP                                *
//*                                                             *
//***************************************************************
//JSTEP08  EXEC PGM=SALESRPT,COND=(0,LT)
//*
//STEPLIB  DD DSN=KC03C53.CSCI465.LOADLIB,DISP=SHR
//         DD DSN=KC02322.CSCI465.LOADLIB,DISP=SHR
//*
//FUNDFLE  DD DSN=&&SORTED,DISP=(OLD,DELETE,DELETE)
//*
//PRICEFLE DD DSN=KC02322.CSCI465.DATAFA23(PRICES),DISP=SHR
//*
//SALESFLE DD DSN=KC02322.CSCI465.DATA7(SALES),DISP=SHR
//*
//HSFLE    DD SPACE=(TRK,(1,1))
//*
//RPTFLE   DD SYSOUT=*
//*
//SYSDUMP  DD SYSOUT=*
