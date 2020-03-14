//PI4PLI   JOB (001),'PI spilog4 pli',                                  00010000
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=256K            00020000
//*
//* The PL/I vertical bar is dec 79, hex 4F
//* WITH DEFAULT HERC CODE PAGE, ascii version is dec 215, hex D7
//* with 819/1049 (TK3UPD), ascii version is dec 124, hex 7c
//*
//B992   EXEC PL1LFCLG,PARM.PL1L='LOAD,NODECK,XREF,ATR'                 00030000
//PL1L.SYSLIN DD UNIT=SYSDA                                             00040000
//PL1L.SYSIN DD *                                                       00050000
 /*
     Uses the algorithm of S. Rabinowicz and S. Wagon, 
    "A Spigot Algorithm for the Digits of Pi".
 */

 SPIGOT: PROCEDURE OPTIONS (MAIN);         /* 21 January 2012. */
  DECLARE DIGITS FIXED BINARY(31);
  DIGITS = 100;

  DECLARE SCALE FIXED BINARY(31);
  SCALE = 10000;

  BEGIN; 
   DECLARE ARR(0:DIGITS) FIXED BINARY (31);
   DECLARE I FIXED BINARY (31);
   DECLARE J FIXED BINARY (31);
   DECLARE ISUM FIXED BINARY (31);
   DECLARE CARRY FIXED BINARY (31);
   DECLARE J21 FIXED BINARY (31);
   DECLARE D FIXED BINARY(31);

   CARRY = 0;

   DO I = 0 TO DIGITS;
    ARR(I) = 2000;
   END;

   DO I = DIGITS TO 1 BY -14;
    ISUM = 0;
    DO J = I TO 1 BY -1;
     ISUM = ISUM*J + SCALE*ARR(J);
     PUT EDIT('SUMx', ISUM) (SKIP(1),A(5),F(10));
     PUT EDIT('J', J) (SKIP(1),A(5),F(10));
     J21 = J * 2 - 1;
     PUT EDIT('J21', J21) (SKIP(1),A(5),F(10));
     ARR(J) = MOD (ISUM, J21);
     /* ISUM = DIVIDE (ISUM, J21, 31, 0); */
     ISUM = ISUM / J21;
     PUT EDIT('ISUMy', ISUM) (SKIP(1),A(5),F(10));
    END;
    D = CARRY + ISUM / SCALE; 
    PUT EDIT('D4', D) (SKIP(1),A(5),F(10));
    PUT EDIT('CARRY', CARRY) (SKIP(1),A(5),F(10));
    PUT EDIT('ISUM', ISUM) (SKIP(1),A(5),F(10));
    CARRY = MOD (ISUM, SCALE);
   END;
  END;
 END SPIGOT;
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR                               00090000
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR                                00100000
//                                                                      00110000
