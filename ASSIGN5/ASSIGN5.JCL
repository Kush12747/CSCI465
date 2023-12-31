//KC03C53A JOB ,'K.GANDHI',MSGCLASS=H
//*
//*****************************************************************
//*                                                               *
//*  CSCI 465               ASSIGNMENT 5           FALL 2023      *
//*                                                               *
//*  DEVELOPER NAME: KUSH GANDHI                                  *
//*        DUE DATE: 10/27/2023                                   *
//*                                                               *
//*  PURPOSE:  THIS ASSIGNMENT IS A CONTINUATION OF ASSIGN 4      *
//*            WHERE WE ARE BUILDING A MORE COMPLEX SALES REPORT. *
//*                                                               *
//*****************************************************************
//*
//*****************************************************************
//*                                                               *
//* JSTEP01 - TAEKS COBOL SOURCE CODE AND COMPILES IT USING THE   *
//*           CURRENT COBOL COMPILER.                             *
//*                                                               *
//* SYSLIB    INPUT: LIBRARY CONTAINING COBOL MACROS              *
//* SYSIN:    INPUT: COMPILES THE COBOL SOURCE CODE               *
//*                                                               *
//* SYSPRINT  OUTPUT: LISTS ANY MESSAGES FROM COMPILATION PROCESS *
//* SYSLIN    OUTPUT: INDICATES NON-EXECUTABLE OBJECT MODULE      *
//*                                                               *
//*****************************************************************
//*
//JSTEP01  EXEC PGM=IGYCRCTL,REGION=0M,PARM=APOST
//*
//STEPLIB  DD DSN=IGY630.SIGYCOMP,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//         DD DSN=CEE.SCEERUN2,DISP=SHR
//*
//SYSIN    DD *
      ********************************************************
      *                                                      *
      * PROGRAM NAME: SALESRPT                               *
      * PROGRAMMER:   KUSH GANDHI                            *
      * DUE DATE:     10/27/2023                             *
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
       DATE-WRITTEN.  10/13/2023.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT SALES-FILE ASSIGN TO SALESFLE.
           SELECT REPORT-FILE ASSIGN TO RPTFLE.
           SELECT HIGH-SALES-FILE ASSIGN TO HSFILE.

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

       01  FUND-RECORD.
           05  IN-SHR-PRC            PIC S9(3)V99.
           05  IN-FUND-NME           PIC X(35).
           05  IN-COMM-PCT-1         PIC S9(3)V9(3).
           05  IN-COMM-PCT-2         PIC S9(3)V9(3).
           05  IN-COMM-PCT-3         PIC S9(3)V9(3).
           05                        PIC X(22) VALUE SPACES.

       01  SALES-RECORD.
           05  IN-BRANCH-NME         PIC X(20).
           05  IN-BROKER-NME         PIC X(25).
           05  IN-DEP-AMT            PIC S9(8)V99.
           05                        PIC X(4) VALUE SPACES.
           05  IN-COMM-FLG           PIC S9(1).
           05                        PIC X(20) VALUE SPACES.

      *******************************************************
      * HIGH-SALES-FILE IS A FILE FOR A TEMPORARY DATA SET  *
      * TO PROCESS BROKER THAT HAVE SOLD MORE THAN 50,000   *
      * SHARES WHICH TAKES THE NAME AND AMOUNT.             *
      *******************************************************
       FD  HIGH-SALES-FILE
           RECORDING MODE F.

       01  HIGH-SALES-RECORD.
           05  IN-HS-BROKER-NME         PIC X(25).
           05  IN-HS-SHR-AMT            PIC S9(10)V999 PACKED-DECIMAL.
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
      * COMM-PCT-1,2,3   COMMISSION PERCENT 1,2,3.          *
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

       01  COUNTER.
           05  LINE-CTR              PIC S9(2) BINARY SYNC VALUE 99.
           05  PAGE-CTR              PIC S9(3) PACKED-DECIMAL VALUE 0.
           05  RECORD-CTR            PIC S9(3) PACKED-DECIMAL VALUE 0.

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
           05  COMM-PCT-1            PIC S9V9(5) BINARY SYNC VALUE 0.
           05  COMM-PCT-2            PIC S9V9(5) BINARY SYNC VALUE 0.
           05  COMM-PCT-3            PIC S9V9(5) BINARY SYNC VALUE 0.
           05  TEMP-COMM-PCT         PIC S9V9(5) BINARY SYNC.

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
           05  OUT-BRANCH-NME        PIC X(20).
           05                        PIC X(13) VALUE SPACES.
           05  OUT-BROKER-NME        PIC X(25).
           05                        PIC X(9) VALUE SPACES.
           05  OUT-DEP-AMT           PIC $$$,$$$,$$9.99.
           05                        PIC X(9) VALUE SPACES.
           05  OUT-SHR-AMT           PIC Z,ZZZ,ZZZ,ZZ9.9999.
           05                        PIC X(10) VALUE SPACES.
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


       01  HEADER-RECORD-1.
            05  OUT-CURRENT-DATE.
                10  CURRENT-MONTH     PIC 9(2).
                10                    PIC X      VALUE '/'.
                10  CURRENT-DAY       PIC 9(2).
                10                    PIC X      VALUE '/'.
                10  CURRENT-YEAR      PIC 9(4).
           05                         PIC X(38)  VALUE SPACES.
           05 OUT-FUND-NME            PIC X(35).
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
           05                         PIC X(40) VALUE SPACES.
           05                         PIC X(35) VALUE
                                 'BROKER SALES AND COMMISSIONS REPORT'.
           05                         PIC X(32) VALUE SPACES.
           05                         PIC X(9)  VALUE 'JOBNAME: '.
           05 JOBNAME                 PIC X(8)  VALUE 'JOBNAME '.

        01 HEADER-UPDATE-2.
           05 OUT-CURRENT-TIME.
              10 HEADER-HOUR-2        PIC 9(2).
              10                      PIC X(1)  VALUE ':'.
              10 HEADER-MINUTE-2      PIC 9(2).
              10                      PIC X(1)  VALUE SPACE.
           05 OUT-PM-AM-2             PIC X(2)  VALUE 'AM'.
           05                         PIC X(36) VALUE SPACES.
           05                         PIC X(42) VALUE
                          'BROKER SALES AND COMMISSIONS REPORT TOTALS'.
           05                         PIC X(29) VALUE SPACES.
           05                         PIC X(9)  VALUE 'JOBNAME: '.
           05 JOBNAME-2               PIC X(8)  VALUE 'JOBNAME '.

        01 HIGH-SALE-HEADER-2.
           05 OUT-CURRENT-TIME.
              10 HEADER-HOUR-3        PIC 9(2).
              10                      PIC X(1)  VALUE ':'.
              10 HEADER-MINUTE-3      PIC 9(2).
              10                      PIC X(1)  VALUE SPACE.
           05 OUT-PM-AM-3             PIC X(2)  VALUE 'AM'.
           05                         PIC X(44) VALUE SPACES.
           05                         PIC X(25) VALUE
                                 'BROKER LARGE SALES REPORT'.
           05                         PIC X(38) VALUE SPACES.
           05                         PIC X(9)  VALUE 'JOBNAME: '.
           05 JOBNAME-3               PIC X(8)  VALUE 'JOBNAME '.

        01 HIGH-SALE-UPDATE-HEADER.
           05 OUT-CURRENT-TIME.
              10 HEADER-HOUR-4        PIC 9(2).
              10                      PIC X(1)  VALUE ':'.
              10 HEADER-MINUTE-4      PIC 9(2).
              10                      PIC X(1)  VALUE SPACE.
           05 OUT-PM-AM-4             PIC X(2)  VALUE 'AM'.
           05                         PIC X(41) VALUE SPACES.
           05                         PIC X(33) VALUE
                                 'BROKER LARGE SALES REPORT TOTALS'.
           05                         PIC X(33) VALUE SPACES.
           05                         PIC X(9)  VALUE 'JOBNAME: '.
           05 JOBNAME-4               PIC X(8)  VALUE 'JOBNAME '.

        01 HEADER-RECORD-3.
           05                         PIC X(55) VALUE SPACES.
           05                         PIC X(14) VALUE 'SHARE PRICE: '.
           05 OUT-SHR-PRC             PIC $$$9.99.
           05                         PIC X(55) VALUE SPACES.

        01 COLUMN-NAMES.
           05                         PIC X(11) VALUE 'BRANCH NAME'.
           05                         PIC X(22) VALUE SPACES.
           05                         PIC X(11) VALUE 'BROKER NAME'.
           05                         PIC X(23) VALUE SPACES.
           05                         PIC X(14) VALUE 'DEPOSIT AMOUNT'.
           05                         PIC X(15) VALUE SPACES.
           05                         PIC X(12) VALUE 'SHARE AMOUNT'.
           05                         PIC X(14) VALUE SPACES.
           05                         PIC X(10) VALUE 'COMMISION'.

        01 HEADER-HYPHENS.
           05                         PIC X(20) VALUE ALL '-'.
           05                         PIC X(13) VALUE SPACES.
           05                         PIC X(25) VALUE ALL '-'.
           05                         PIC X(9)  VALUE SPACES.
           05                         PIC X(14) VALUE ALL '-'.
           05                         PIC X(9)  VALUE SPACES.
           05                         PIC X(18) VALUE ALL '-'.
           05                         PIC X(10) VALUE SPACES.
           05                         PIC X(14) VALUE ALL '-'.

        01 HEADER-TOTALS.
           05                         PIC X(39) VALUE SPACES.
           05                         PIC X(15) VALUE 'NUMBER OF SALES'.
           05                         PIC X(9)  VALUE SPACES.
           05                         PIC X(18) VALUE
                                                'TOTAL SALES AMOUNT'.
           05                         PIC X(9)  VALUE SPACES.
           05                         PIC X(18) VALUE
                                                'TOTAL SHARE AMOUNT'.
           05                         PIC X(8)  VALUE SPACES.
           05                         PIC X(16) VALUE
                                                'TOTAL COMMISSION'.
        01 TOTAL-HYPHENS.
           05                         PIC X(39) VALUE SPACES.
           05                         PIC X(15) VALUE ALL '-'.
           05                         PIC X(9)  VALUE SPACES.
           05                         PIC X(18) VALUE ALL '-'.
           05                         PIC X(5)  VALUE SPACES.
           05                         PIC X(22) VALUE ALL '-'.
           05                         PIC X(6)  VALUE SPACES.
           05                         PIC X(18) VALUE ALL '-'.

        01 HIGH-SALES.
           05                         PIC X(40) VALUE SPACES.
           05                         PIC X(22) VALUE
                                              'HIGH SALES BROKER NAME'.
           05                         PIC X(10) VALUE SPACES.
           05                         PIC X(23) VALUE
                                             'HIGH SALES SHARE AMOUNT'.
           05                         PIC X(37) VALUE SPACES.

        01 HIGH-SALES-HYPHENS.
           05                         PIC X(37) VALUE SPACES.
           05                         PIC X(25) VALUE ALL '-'.
           05                         PIC X(10) VALUE SPACES.
           05                         PIC X(23) VALUE ALL '-'.
           05                         PIC X(37) VALUE SPACES.

        01 HIGH-SALES-TOT-HEADER.
           05                         PIC X(34) VALUE SPACES.
           05                         PIC X(28) VALUE
                                      'NUMBER OF HIGH SALES BROKERS'.
           05                         PIC X(4)  VALUE SPACES.
           05                         PIC X(29) VALUE
                                       'TOTAL HIGH SALES SHARE AMOUNT'.
           05                         PIC X(37) VALUE SPACES.

        01 HIGH-SALES-TOT-HYPHENS.
           05                         PIC X(34) VALUE SPACES.
           05                         PIC X(28) VALUE ALL '-'.
           05                         PIC X(4)  VALUE SPACES.
           05                         PIC X(29) VALUE ALL '-'.
           05                         PIC X(37) VALUE SPACES.

       PROCEDURE DIVISION.

      *******************************************************
      * 0000-MAIN. THIS ROUTINE CONTROLS THE FLOW OF THE    *
      * PROGRAM. IT CALLS ROUTINES TO (1) READ THE FIRST    *
      * RECORD, (2) INCREMENT THE UPPER-DIVISION AND LOWER- *
      * DIVISION TOTALS, AND (3) PRINT THE SUMMARY.         *
      *******************************************************
       0000-MAIN.

           PERFORM 0140-CURRENT-DATE-AND-TIME.

           CALL JOBNAME USING JOBNAME.
           MOVE JOBNAME TO JOBNAME-2.
           MOVE JOBNAME TO JOBNAME-3.
           MOVE JOBNAME TO JOBNAME-4.

           IF HEADER-HOUR > 12
             COMPUTE HEADER-HOUR = HEADER-HOUR - 12
             MOVE HEADER-HOUR TO HEADER-HOUR-2
             MOVE HEADER-HOUR TO HEADER-HOUR-3
             MOVE HEADER-HOUR TO HEADER-HOUR-4
             MOVE 'PM' TO OUT-PM-AM
             MOVE 'PM' TO OUT-PM-AM-2
             MOVE 'PM' TO OUT-PM-AM-3
             MOVE 'PM' TO OUT-PM-AM-4
           END-IF.

           OPEN INPUT  SALES-FILE
                OUTPUT HIGH-SALES-FILE
                OUTPUT REPORT-FILE.

           READ SALES-FILE
             AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

           PERFORM 0100-PROCESS-MONEY.

           READ SALES-FILE
             AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

           PERFORM 0120-PROCESS-RECORD
             UNTIL EOF-FLAG = 'Y'.

           PERFORM 0130-PROCESS-TOTAL.

           MOVE 0 TO RECORD-CTR.

           MOVE 'N' TO EOF-FLAG.

           MOVE '0' TO PAGE-CTR.

           CLOSE SALES-FILE
                 HIGH-SALES-FILE
                 REPORT-FILE.

           OPEN INPUT HIGH-SALES-FILE
                OUTPUT REPORT-FILE.

           READ HIGH-SALES-FILE
             AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

           PERFORM 0200-PROCESS-HS-HEADERS.
           PERFORM 0210-PROCESS-HS-RECORDS
             UNTIL EOF-FLAG = 'Y'.
           PERFORM 0220-PROCESS-HS-TOTALS.

           CLOSE HIGH-SALES-FILE
                 REPORT-FILE.

           MOVE 0 TO RETURN-CODE.

           STOP RUN.

       0000-EXIT. EXIT.

      *****************************************************
      * 0100-PROCESS-MONEY. THIS ROUTINE IS USED TO      *
      * READ DATA FROM INPUT AND MOVE IT TO OUTPUT DETAIL *
      * LINE.                                             *
      *****************************************************

       0100-PROCESS-MONEY.

           MOVE IN-SHR-PRC TO SHR-PRC.
           MOVE IN-SHR-PRC TO OUT-SHR-PRC.
           MOVE IN-COMM-PCT-1 TO COMM-PCT-1.
           MOVE IN-COMM-PCT-2 TO COMM-PCT-2.
           MOVE IN-COMM-PCT-3 TO COMM-PCT-3.
           MOVE IN-FUND-NME TO OUT-FUND-NME.

       0100-EXIT. EXIT.

      *****************************************************
      * 0120-PROCESS-RECORD. THIS PROCESS MOVES RECORDS   *
      * FOR THE FIRST REPORT AND DOES SOME CALCULATION    *
      * FOR TOTALS, COMMISSION PERCENT AND MOVES ANY      *
      * RECORDS WITH SHR-AMT > 50,000 TO HIGH-SALES.      *
      *****************************************************

       0120-PROCESS-RECORD.

           ADD 1 TO RECORD-CTR.
           MOVE IN-BRANCH-NME TO OUT-BRANCH-NME.
           MOVE IN-BROKER-NME TO OUT-BROKER-NME.
           MOVE IN-DEP-AMT TO OUT-DEP-AMT.
           MOVE IN-DEP-AMT TO DEP-AMT.

           IF IN-COMM-FLG = 1
             MOVE COMM-PCT-1 TO TEMP-COMM-PCT
           ELSE IF IN-COMM-FLG = 2
             MOVE COMM-PCT-2 TO TEMP-COMM-PCT
           ELSE
             MOVE COMM-PCT-3 TO TEMP-COMM-PCT
           END-IF.

           COMPUTE SHR-AMT ROUNDED = DEP-AMT / SHR-PRC.
           COMPUTE TOT-DEP-AMT ROUNDED = TOT-DEP-AMT + DEP-AMT.
           COMPUTE TOT-SHR-AMT ROUNDED = TOT-SHR-AMT + SHR-AMT.
           COMPUTE COMM-AMT ROUNDED = DEP-AMT * TEMP-COMM-PCT / 100.
           MOVE COMM-AMT TO OUT-COMM-AMT.
           COMPUTE TOT-COMM-AMT ROUNDED = TOT-COMM-AMT + COMM-AMT.

           MOVE SHR-AMT TO OUT-SHR-AMT.

           IF SHR-AMT > 50000.0000
             MOVE IN-BROKER-NME TO IN-HS-BROKER-NME
             MOVE OUT-SHR-AMT TO IN-HS-SHR-AMT
             WRITE HIGH-SALES-RECORD
           END-IF.

           IF LINE-CTR > 18
             ADD 1 TO PAGE-CTR

             MOVE PAGE-CTR TO OUT-PAGE-CTR
             MOVE SHR-PRC TO OUT-SHR-PRC

             WRITE REPORT-RECORD FROM HEADER-RECORD-1 AFTER PAGE
             WRITE REPORT-RECORD FROM HEADER-RECORD-2 AFTER 1
             WRITE REPORT-RECORD FROM HEADER-RECORD-3 AFTER 2
             WRITE REPORT-RECORD FROM COLUMN-NAMES AFTER 2
             WRITE REPORT-RECORD FROM HEADER-HYPHENS AFTER 1

             MOVE 0 TO LINE-CTR

           END-IF.

           WRITE REPORT-RECORD FROM DETAIL-LINE AFTER 2.

           ADD 1 TO LINE-CTR.

           READ SALES-FILE
             AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

       0120-EXIT. EXIT.

      *****************************************************
      * 0130-PROCESS-TOTAL. THIS PROCESS MOVE THE TOTALS  *
      * CALCULATED TO THE REPORT BY WRITE.                *
      *****************************************************

       0130-PROCESS-TOTAL.

           ADD 1 TO PAGE-CTR.

           MOVE PAGE-CTR TO OUT-PAGE-CTR.
           MOVE RECORD-CTR TO OUT-SALE-REC.
           MOVE TOT-DEP-AMT TO OUT-TOT-DEP-AMT.
           MOVE TOT-SHR-AMT TO OUT-TOT-SHR-AMT.
           MOVE TOT-COMM-AMT TO OUT-TOT-COMM-AMT.

           WRITE REPORT-RECORD FROM HEADER-RECORD-1 AFTER PAGE.
           WRITE REPORT-RECORD FROM HEADER-UPDATE-2 AFTER 1.
           WRITE REPORT-RECORD FROM HEADER-RECORD-3 AFTER 2.
           WRITE REPORT-RECORD FROM HEADER-TOTALS AFTER 2.
           WRITE REPORT-RECORD FROM TOTAL-HYPHENS AFTER 1.
           WRITE REPORT-RECORD FROM TOTAL-LINE AFTER 2.

       0130-EXIT. EXIT.

      *****************************************************
      * 0140-CURRENT-DATE-AND-TIME. THIS PROCESS MOVE THE *
      * CURRENT DATE AND TIME.                            *
      *****************************************************

       0140-CURRENT-DATE-AND-TIME.

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-FIELDS.

           MOVE CORRESPONDING CURRENT-DATE TO OUT-CURRENT-DATE.

           MOVE CURRENT-HOUR TO HEADER-HOUR.
           MOVE CURRENT-MINUTE TO HEADER-MINUTE.
           MOVE CURRENT-MINUTE TO HEADER-MINUTE-2.
           MOVE CURRENT-MINUTE TO HEADER-MINUTE-3.
           MOVE CURRENT-MINUTE TO HEADER-MINUTE-4.

       0140-EXIT. EXIT.

      *****************************************************
      * 0200-PROCESS-HS-HEADERS. THIS PROCESS WRITES THE  *
      * HEADER AND HYPHENS TO THE REPORT FOR HIGH SALES   *
      * RECORDS THAT WAS PROCESSED FROM BEFORE.           *
      *****************************************************

       0200-PROCESS-HS-HEADERS.

              ADD 1 TO PAGE-CTR.

              MOVE PAGE-CTR TO OUT-PAGE-CTR.

              WRITE REPORT-RECORD FROM HEADER-RECORD-1 AFTER PAGE.
              WRITE REPORT-RECORD FROM HIGH-SALE-HEADER-2 AFTER 1.
              WRITE REPORT-RECORD FROM HEADER-RECORD-3 AFTER 2.
              WRITE REPORT-RECORD FROM HIGH-SALES AFTER 2.
              WRITE REPORT-RECORD FROM HIGH-SALES-HYPHENS AFTER 1.

              MOVE 0 TO LINE-CTR.

       0200-EXIT. EXIT.

      *****************************************************
      * 0210-PROCESS-HS-RECORDS. THIS PROCESS SETS AND    *
      * GETS THE HIGH SALES DATA TO GET IT READY AND      *
      * WRITES THE NAME AND SHARE AMT.                    *
      *****************************************************

       0210-PROCESS-HS-RECORDS.

            ADD 1 TO RECORD-CTR.

            MOVE IN-HS-BROKER-NME TO OUT-HS-BROKER-NME.
            MOVE IN-HS-SHR-AMT TO OUT-HS-SHR-AMT.
            MOVE IN-HS-SHR-AMT TO HS-SHR-AMT.

            COMPUTE HS-TOT-SHR-AMT = HS-TOT-SHR-AMT + HS-SHR-AMT.

            IF LINE-CTR > 17
              PERFORM 0200-PROCESS-HS-HEADERS
            END-IF.

            WRITE REPORT-RECORD FROM HIGH-SALES-NME-AMT AFTER 2.

            ADD 1 TO LINE-CTR.

            READ HIGH-SALES-FILE
              AT END MOVE 'Y' TO EOF-FLAG
            END-READ.

       0210-EXIT. EXIT.

      *****************************************************
      * 0220-PROCESS-HS-TOTALS. THIS PROCESS MOVES SOME   *
      * DATA FOR HIGH SALSES TO REPORT AND BY WRITING THE *
      * HEADERS AND TOTALS FOR THE REPORT.                *
      *****************************************************

       0220-PROCESS-HS-TOTALS.

            ADD 1 TO PAGE-CTR.

            MOVE PAGE-CTR TO OUT-PAGE-CTR.
            MOVE RECORD-CTR TO OUT-HS-SALE-CTR.
            MOVE HS-TOT-SHR-AMT TO OUT-HS-TOT-SHR-AMT.

            WRITE REPORT-RECORD FROM HEADER-RECORD-1 AFTER PAGE.
            WRITE REPORT-RECORD FROM HIGH-SALE-UPDATE-HEADER AFTER 1.
            WRITE REPORT-RECORD FROM HEADER-RECORD-3 AFTER 2.
            WRITE REPORT-RECORD FROM HIGH-SALES-TOT-HEADER AFTER 2.
            WRITE REPORT-RECORD FROM HIGH-SALES-TOT-HYPHENS AFTER 1.
            WRITE REPORT-RECORD FROM HIGH-SALES-TOTALS AFTER 2.

       0220-EXIT. EXIT.
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
//JSTEP02 EXEC PGM=IEWBLINK,COND=(0,LT)
//*
//SYSLIB  DD DSN=CEE.SCEELKED,DISP=SHR
//*
//SYSLIN  DD DSN=&&OBJMOD,DISP=(OLD,DELETE,DELETE)
//        DD *
   ENTRY SALESRPT
   MODE AMODE(24)
/*
//*
//SYSLMOD DD DSN=KC03C53.CSCI465.LOADLIB(SALESRPT),
//           SPACE=(1024,(50,20,1)),
//           DSNTYPE=LIBRARY,
//           DISP=(MOD,KEEP,KEEP)
//*
//SYSPRINT DD SYSOUT=*
//*
//***************************************************************
//*                                                             *
//* JSTEP03 - FETCHES THE PROGRAM DATA AND EXECUTES IT.         *
//*                                                             *
//* STEPLIB   INPUT: LOCATION OF PROGRAM OBJECT SALESRPT        *
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
//SALESFLE DD DSN=KC02322.CSCI465.DATAFA23(DATA5),DISP=SHR
//*
//HSFILE   DD SPACE=(TRK,(1,1))
//*
//RPTFLE   DD SYSOUT=*
//*
//SYSDUMP  DD SYSOUT=*
//
