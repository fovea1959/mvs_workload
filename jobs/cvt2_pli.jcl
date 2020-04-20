//PLICVT2  JOB (001),'cvt2_pli.jcl',CLASS=A,MSGCLASS=A
//*
//* The PL/I vertical bar is dec 79, hex 4F
//* WITH DEFAULT HERC CODE PAGE, ascii version is dec 215, hex D7
//* with 819/1049 (TK3UPD), ascii version is dec 124, hex 7c
//*
//PI     EXEC PL1LFCLG,PARM.PL1L='LOAD,NODECK'                          00030000
//PL1L.SYSLIN DD UNIT=SYSDA                                             00040000
//PL1L.SYSIN DD *                                                       00050000
 TESTNAMSTEP: PROCEDURE OPTIONS (MAIN);         /* 21 January 2012. */
 DCL   ZERO               FIXED BIN(31) STATIC INIT(0);
 DCL   PSAPTR             POINTER BASED(ADDR(ZERO));

 DCL 1 PSA                BASED(PSAPTR),
       2 PSA0             CHARACTER(16),
       2 FLCCVT           POINTER,
       2 PSAFILL          CHARACTER(500),
       2 PSAPCCAV         POINTER,
       2 PSAPCCAR         POINTER,
       2 PSALCCAV         POINTER,
       2 PSALCCAR         POINTER,
       2 PSATNEW          POINTER,
       2 PSATOLD          POINTER,
       2 PSAANEW          POINTER,
       2 PSAAOLD          POINTER;

 DCL TCB(4)  POINTER  BASED(PSATOLD);

 DCL TIOCNJOB  CHAR(8) BASED(TCB(4));

 PUT EDIT (TIOCNJOB) (A(8));
 END TESTNAMSTEP;
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR                               00090000
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR                                00100000
//GO.SYSPRINT DD SYSOUT=A,DCB=(LRECL=133,RECFM=FA)
//                                                                      00110000
