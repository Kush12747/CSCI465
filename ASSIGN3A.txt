//KC03C53A JOB ,'K.GANDHI',MSGCLASS=H
//*
//*****************************************************************
//*                                                               *
//*  CSCI 465               ASSIGNMENT 3           FALL 2023      *
//*                                                               *
//*  DEVELOPER NAME: KUSH GANDHI                                  *
//*        DUE DATE: 9/29/2023                                    *
//*                                                               *
//*  PURPOSE: THIS PROGRAM USES COBOL CODE TO READ IN DATA        *
//*            AND OUTPUT TO A TEMP DATASET AND SAVES IT IN THE   *
//*            LOADLIB.                                           *
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
      * PROGRAM NAME: COBOL3      Â© 2023 Geoffrey D. Decker *
      * PROGRAMMER:   GEOFFREY D. DECKER                     *
      * DUE DATE:     08/25/2023                             *
      *                                                      *
      * FUNCTION: THIS PROGRAM READS INPUT DATA RECORDS      *
      *           AND THEN WRITES THEM TO STANDARD OUTPUT.   *
      *                                                      *
      * INPUT: INDATA - THE INPUT DATA RECORDS READ IN.      *
      *                                                      *
      * OUTPUT: OUTDATA - THE INPUT DATA RECORDS WRITTEN     *
      *                   OUT.                               *
      *                                                      *
      * NOTES: NONE                                          *
      *                                                      *
      ********************************************************

       IDENTIFICATION DIVISION.

       PROGRAM-ID.    COBOL3.
       AUTHOR.        GEOFFREY D DECKER.
       DATE-WRITTEN.  08/25/2023.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT INPUT-FILE ASSIGN TO INDATA.
           SELECT REPORT-FILE ASSIGN TO RPTDATA.

       DATA DIVISION.

       FILE SECTION.

       FD  INPUT-FILE
           RECORDING MODE F.

       01  INPUT-RECORD             PIC X(80).

       FD  REPORT-FILE
           RECORDING MODE F
           BLOCK CONTAINS 20 RECORDS.

       01  REPORT-RECORD            PIC X(132).

       WORKING-STORAGE SECTION.

       01  EOF-FLAG                  PIC X     VALUE 'N'.

       01  RECORD-CTR               PIC S9(3)  BINARY SYNC VALUE 0.

       01  DETAIL-LINE.
           05  OUTPUT-RECORD        PIC X(80).
           05  FILLER               PIC X(52)  VALUE SPACES.

       01  TOTALS-LINE.
           05  FILLER               PIC X(14)  VALUE 'RECORD COUNT: '.
           05  OUT-RECORD-CTR       PIC ZZ9.
           05  FILLER               PIC X(115) VALUE SPACES.

       PROCEDURE DIVISION.

       0000-MAIN.

           OPEN INPUT  INPUT-FILE
                OUTPUT REPORT-FILE.

           READ INPUT-FILE
             AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

           PERFORM 0100-PROCESS-RECORD
             UNTIL EOF-FLAG = 'Y'.

           PERFORM 0200-PROCESS-TOTALS.

           CLOSE INPUT-FILE
                 REPORT-FILE.

           MOVE 0 TO RETURN-CODE.

           STOP RUN.

       0000-EXIT. EXIT.

       0100-PROCESS-RECORD.

           ADD 1 TO RECORD-CTR.

           MOVE INPUT-RECORD TO OUTPUT-RECORD.

           WRITE REPORT-RECORD FROM DETAIL-LINE AFTER 2.

           READ INPUT-FILE
             AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

       0100-EXIT. EXIT.

       0200-PROCESS-TOTALS.

           MOVE RECORD-CTR TO OUT-RECORD-CTR.

           WRITE REPORT-RECORD FROM TOTALS-LINE AFTER 2.

       0200-EXIT. EXIT.
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
//SYSLMOD DD DSN=KC03C53.CSCI465.LOADLIB(COBOL3),
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
//* STEPLIB   INPUT: LOCATION OF PROGRAM OBJECT COBOL3          *
//* INDATA    INPUT: INPUT FILE DATA                            *
//*                                                             *
//* RPTDATA   OUTPUT: OUTPUT OF THE PROGRAM                     *
//* SYSDUMP   OUTPUT: ABEND DUMP                                *
//*                                                             *
//***************************************************************
//*
//JSTEP03  EXEC PGM=COBOL3,COND=(0,LT)
//*
//STEPLIB  DD DSN=KC03C53.CSCI465.LOADLIB,DISP=SHR
//*
//INDATA   DD DSN=KC02322.CSCI465.DATAFA23(DATA3),DISP=SHR
//*
//RPTDATA  DD SYSOUT=*
//*
//SYSDUMP  DD SYSOUT=*
//
