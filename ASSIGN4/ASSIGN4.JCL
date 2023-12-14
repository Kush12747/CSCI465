//KC03C53A JOB ,'K.GANDHI',MSGCLASS=H
//*
//*****************************************************************
//*                                                               *
//*  CSCI 465               ASSIGNMENT 4           FALL 2023      *
//*                                                               *
//*  DEVELOPER NAME: KUSH GANDHI                                  *
//*        DUE DATE: 10/11/2023                                   *
//*                                                               *
//*  PURPOSE:  THE PURPOSE OF THE PROGRAM IS TO WRITE A COBOL     *
//*            PROGRAM WHICH CREATES A SINGLE REPORT.             *
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
      * DUE DATE:     10/11/2023                             *
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
       DATE-WRITTEN.  09/28/2023.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT SALES-FILE ASSIGN TO SALESFLE.
           SELECT REPORT-FILE ASSIGN TO RPTFLE.

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
      * VARIABLES:                                           *
      *                                                      *
      * IN-BRANCH-NME       BRANCH NAME FROM SALES-FILE      *
      * IN-BROKER-NME       BROKER NAME FROM SALES-FILE      *
      * IN-DEP-NME          DEPARTMENT NAME FROM SALES-FILE  *
      ********************************************************

       FD  SALES-FILE
           RECORDING MODE F.

       01  SALES-RECORD.
           05  IN-BRANCH-NME         PIC X(20).
           05  IN-BROKER-NME         PIC X(25).
           05  IN-DEP-AMT            PIC S9(8)V99.
           05                        PIC X(25) VALUE SPACES.
      *******************************************************
      *                                                     *
      * REPORT-FILE CONTAINS A GENERIC PRINT LINE THAT IS   *
      * USED TO PRINT A SIMPLE SALES REPORT FROM INPUT      *
      * VARIABLES.                                          *
      *******************************************************

       FD  REPORT-FILE
           RECORDING MODE F.

       01  REPORT-RECORD             PIC X(132).

       WORKING-STORAGE SECTION.

       01  EOF-FLAG                  PIC X     VALUE 'N'.

       01  DETAIL-LINE.
           05  OUT-BRANCH-NME        PIC X(20).
           05                        PIC X(10) VALUE SPACES.
           05  OUT-BROKER-NME        PIC X(25).
           05                        PIC X(10) VALUE SPACES.
           05  OUT-DEP-AMT           PIC $$$,$$$,$$9.99.
           05                        PIC X(57) VALUE SPACES.

       PROCEDURE DIVISION.
      *******************************************************
      * 0000-MAIN. THIS ROUTINE CONTROLS THE FLOW OF THE    *
      * PROGRAM. IT CALLS ROUTINES TO (1) READ THE FIRST    *
      * RECORD, (2) INCREMENT THE UPPER-DIVISION AND LOWER- *
      * DIVISION TOTALS, AND (3) PRINT THE SUMMARY.         *
      *******************************************************
       0000-MAIN.

           OPEN INPUT  SALES-FILE
                OUTPUT REPORT-FILE.

           READ SALES-FILE
             AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

           PERFORM 0100-PROCESS-RECORD
             UNTIL EOF-FLAG = 'Y'.

           CLOSE SALES-FILE
                 REPORT-FILE.

           MOVE 0 TO RETURN-CODE.

           STOP RUN.

       0000-EXIT. EXIT.
      *****************************************************
      * 0100-PROCESS-RECORD. THIS ROUTINE IS USED TO      *
      * READ DATA FROM INPUT AND MOVE IT TO OUTPUT DETAIL *
      * LINE.                                             *
      *****************************************************

       0100-PROCESS-RECORD.
           MOVE IN-DEP-AMT TO OUT-DEP-AMT.
           MOVE IN-BROKER-NME TO OUT-BROKER-NME.
           MOVE IN-BRANCH-NME TO OUT-BRANCH-NME.
           WRITE REPORT-RECORD FROM DETAIL-LINE AFTER 2.

           READ SALES-FILE
             AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

       0100-EXIT. EXIT.
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
//*
//SALESFLE DD DSN=KC02322.CSCI465.DATAFA23(DATA4),DISP=SHR
//*
//RPTFLE   DD SYSOUT=*
//*
//SYSDUMP  DD SYSOUT=*
//
