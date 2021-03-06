//PI4PLI   JOB (001),'Pi spigot4 PL/I',                                 00010000
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=256K            00020000
//*
//* The PL/I vertical bar is dec 79, hex 4F
//* WITH DEFAULT HERC CODE PAGE, ascii version is dec 215, hex D7
//* with 819/1049 (TK3UPD), ascii version is dec 124, hex 7c
//*
//       EXEC PL1LFCLG,PARM.PL1L='LOAD,NODECK,XREF,ATR'                 00030000
//PL1L.SYSLIN DD UNIT=SYSDA                                             00040000
//PL1L.SYSIN DD *                                                       00050000
 /*
     Uses the algorithm of S. Rabinowicz and S. Wagon, 
    "A Spigot Algorithm for the Digits of Pi".
 */

 SPIGOT4: PROCEDURE OPTIONS (MAIN);
  DECLARE DESIRED FIXED BINARY(31);
  DESIRED = 9000;

  DECLARE DIGITS FIXED BINARY(31);
  DIGITS = (DESIRED * 14) / 4;

  DECLARE SCALE FIXED BINARY(31);
  SCALE = 10000;

  BEGIN; 
   DECLARE ARR(0:DIGITS) FIXED BINARY (31);
   DECLARE (I, J, SUM, CARRY) FIXED BINARY (31);

   PUT FILE(SYSPRINT) EDIT ('0') (A);

   CARRY = 0;

   DO I = 0 TO DIGITS;
    ARR(I) = 2000;
   END;

   DO I = DIGITS TO 1 BY -14;
    SUM = 0;
    DO J = I TO 1 BY -1;
     SUM = SUM*J + SCALE*ARR(J);
     ARR(J) = MOD (SUM, J*2 - 1);
     SUM = SUM / (J*2 - 1);
    END;
    PUT FILE(SYSPRINT) EDIT(CARRY + SUM / SCALE) (P'9999');
    CARRY = MOD (SUM, SCALE);
   END;
  END;
 END SPIGOT4;
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR                               00090000
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR                                00100000
//GO.SYSPRINT DD SYSOUT=A,DCB=(RECFM=FA,LRECL=133)
//                                                                      00110000
