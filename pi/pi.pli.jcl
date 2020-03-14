//PIPLI    JOB (001),'PI spigot pli',TIME=60,                           00010000
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=256K            00020000
//*
//* The PL/I vertical bar is dec 79, hex 4F
//* WITH DEFAULT HERC CODE PAGE, ascii version is dec 215, hex D7
//* with 819/1049 (TK3UPD), ascii version is dec 124, hex 7c
//*
//PI     EXEC PL1LFCLG,PARM.PL1L='LOAD,NODECK,ATR,XREF'                 00030000
//PL1L.SYSLIN DD UNIT=SYSDA                                             00040000
//PL1L.SYSIN DD *                                                       00050000
 /*
  Uses the algorithm of S. Rabinowicz and S. Wagon, 
  "A Spigot Algorithm for the Digits of Pi".
 */

 SPIGOT: PROCEDURE OPTIONS (MAIN);         /* 21 January 2012. */
  DECLARE (N, LEN) FIXED BINARY (31);
 
  N = 9000;
  LEN = 10*N / 3;
  BEGIN;
   DECLARE ( I, J, K, Q, NINES, PREDIGIT ) FIXED BINARY (31);
   DECLARE X FIXED BINARY (31);
   DECLARE A(LEN) FIXED BINARY (31);
 
   A = 2; /* START WITH 2S */
   NINES, PREDIGIT = 0; /* FIRST PREDIGIT IS A 0 */
   DO J = 1 TO N;
    Q = 0;
    DO I = LEN TO 1 BY -1; /* WORK BACKWARDS */
     X = 10*A(I) + Q*I;
     A(I) = MOD (X, (2*I-1));
     Q = X / (2*I-1);
    END;
    A(1) = MOD(Q, 10); Q = Q / 10;
    IF Q = 9 THEN
     NINES = NINES + 1;
    ELSE IF Q = 10 THEN
     DO;
      PUT FILE(SYSPRINT) EDIT(PREDIGIT+1) (F(1));
      DO K = 1 TO NINES;
       PUT FILE(SYSPRINT) EDIT ('0')(A(1)); /* ZEROS */
      END;
      PREDIGIT, NINES = 0;
     END;
    ELSE
     DO;
      PUT FILE(SYSPRINT) EDIT(PREDIGIT) (F(1)); PREDIGIT = Q;
      DO K = 1 TO NINES; 
       PUT FILE(SYSPRINT) EDIT ('9')(A(1));
      END;
      NINES = 0;
     END;
   END; /* DO J = 1 TO N */
   PUT FILE(SYSPRINT) EDIT(PREDIGIT) (F(1));
  END; /* OF BEGIN BLOCK */
 END SPIGOT;
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR                               00090000
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR                                00100000
//GO.SYSPRINT DD SYSOUT=A,DCB=(LRECL=133,RECFM=FA)
//                                                                      00110000
