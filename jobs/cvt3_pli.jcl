//PIPLI    JOB (001),'cvt3_pli.jcl',CLASS=A,MSGCLASS=A
//*
//* The PL/I vertical bar is dec 79, hex 4F
//* WITH DEFAULT HERC CODE PAGE, ascii version is dec 215, hex D7
//* with 819/1049 (TK3UPD), ascii version is dec 124, hex 7c
//*
//PI     EXEC PL1LFCLG,PARM.PL1L='LOAD,NODECK'                          00030000
//PL1L.SYSLIN DD UNIT=SYSDA                                             00040000
//PL1L.SYSIN DD *                                                       00050000
 CVT3: PROCEDURE OPTIONS (MAIN);
 DCL 1 PP
  DCL 3 PSAP POINTER;
 DCL 1 PSA               BASED(PSAP);
 PSAP = 0;
 END CVT3;
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR                               00090000
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR                                00100000
//GO.SYSPRINT DD SYSOUT=A,DCB=(LRECL=133,RECFM=FA)
//                                                                      00110000
