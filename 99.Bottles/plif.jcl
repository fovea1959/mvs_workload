//PL1B99   JOB (001),'99 BOTTLES PL1',
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=256K
//*
//* The PL/I vertical bar is dec 79, hex 4F
//* WITH DEFAULT HERC CODE PAGE, ascii version is dec 215, hex D7
//* with 819/1049 (TK3UPD), ascii version is dec 124, hex 7c
//*
//B992   EXEC PL1LFCLG
//PL1L.SYSLIN DD UNIT=SYSDA
//PL1L.SYSIN DD *
 /* Sing "99 Bottles of beer on the wall"
 This was adapted from the version on the 99-bottles-of-beer.net. */

 BOTTLES: PROC OPTIONS(MAIN);

   DCL NUM_BOT FIXED DEC(3);
   DCL NEW_NUM_BOT FIXED DEC(3);
   DCL SS CHAR(100) VAR;

   DECLARE HOWMANY RETURNS (CHAR(30) VAR);

   DO NUM_BOT = 100 TO 0 BY -1;
     SS = HOWMANY(NUM_BOT);
     PUT SKIP LIST( SS××' on the wall, '××SS××',');
     IF (NUM_BOT > 0) THEN DO;
       NEW_NUM_BOT = NUM_BOT - 1;
       PUT SKIP LIST('Take one down and pass it around,');
       END;
     ELSE DO;
       NEW_NUM_BOT = 99;
       PUT SKIP LIST('Go to the store and buy some more,');
       END;
     PUT SKIP LIST (HOWMANY(NEW_NUM_BOT)××' on the wall.');
     IF (NUM_BOT > 0) THEN PUT SKIP LIST(' ');
   END;

 HOWMANY: PROCEDURE (I) RETURNS (CHAR(30) VAR);
   DECLARE I FIXED DEC(3);
   DECLARE RV CHAR(30) VAR;

   IF I = 0 THEN
     RV = 'No more Bottles of Beer';
     ELSE
       IF I = 1 THEN
         RV = '1 more Bottle of Beer';
       ELSE DO;
         RV = I××' more Bottles of Beer';
         DO WHILE (SUBSTR(RV,1,1) = ' ');
           RV = SUBSTR(RV,2);
         END;
       END;
   RETURN (RV);
  END HOWMANY;

 END BOTTLES;
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//
