//KC03C53A JOB ,'K.GANDHI',MSGCLASS=H
//*
//*****************************************************************
//*                                                               *
//*  CSCI 465               ASSIGNMENT 2           FALL 2023      *
//*                                                               *
//*  DEVELOPER NAME: KUSH GANDHI                                  *
//*        DUE DATE: 9/15/23                                      *
//*                                                               *
//*  FUNCTION: THIS PROGRAM PROCESS A FETCHING TECHNIQUE FROM A   *
//*            EXISTING LOAD LIBRARY.                             *
//*                                                               *
//*  OUTPUT: PRINTS THE PREAMBLE OF THE U.S CONSITUTION           *
//*****************************************************************
//*
//*****************************************************************
//*                                                               *
//* JSTEP01 - THE FIRST STEPS WILL FETCH AND EXECUTES A PROGRAM   *
//*            A PROGRAM.                                         *
//*****************************************************************
//JSTEP01 EXEC PGM=READRECS     PROGRAM NAME IN LOAD LIBRARY
//*
//* DD STMT. SPECIFYING THE LOAD LIBRARY AND SET TO SHARED
//STEPLIB DD DSN=KC02322.CSCI465.LOADLIB,DISP=SHR
//*
//* DD STMT. SPECIFIES THE INPUT DATASET
//RECSIN  DD DSN=KC02322.CSCI465.DATAFA23(DATA2),DISP=SHR
//*
//* FORMAT FOR SPACE
//RECSOUT DD DSN=&&TEMP,
//           DCB=(LRECL=80,BLKSIZE=880,RECFM=FB),
//           SPACE=(TRK,(1,1)),
//           DISP=(NEW,PASS,DELETE)
//*
//SYSDUMP DD SYSOUT=*   USED TO CHECK ABENDS
//*
//***************************************************************
//*                                                             *
//* JSTEP02 -  THIS STEP WILL FETCH AND EXECUTE FROM SAME LOAD  *
//*            LIBRARY.                                         *
//***************************************************************
//*
//JSTEP02 EXEC PGM=WRTERECS,COND=(0,LT) PROGRAM NAME IN LOAD LIBRARY
//*
//* DD STMT. SPECIFYING THE LOAD LIBRARY AND SET TO SHARED
//STEPLIB DD DSN=KC02322.CSCI465.LOADLIB,DISP=SHR
//*
//* STORES IN A TEMPORARY DATASET CALLED TEMP
//RECSIN  DD DSN=&&TEMP,DISP=(MOD,DELETE,DELETE)
//*
//RECSOUT DD SYSOUT=*   PRINTS OUTPUT
//*
//SYSDUMP DD SYSOUT=*   USED TO CHECK ABENDS
//
