//PAS8BEER JOB (SYS),'99 BOTTLES PASCAL8K',CLASS=A,MSGCLASS=A           00010001
// EXEC P8PASCLG                                                        00040001
//SYSIN DD *                                                            00050001
(*U+*)
PROGRAM BOTTLESOFBEER (OUTPUT);
(*THIS PROGRAM PLAYS THE 99 BOTTLES OF BEER SONG*)

CONST CBOTTLE = ' BOTTLE';
      CBEER = ' OF BEER';
      CWALL = ' ON THE WALL';
      CTAKE = 'TAKE ONE DOWN, PASS IT AROUND';

TYPE TBOTTLES = 1..99;

VAR BOTTLES : TBOTTLES;

BEGIN (* MAIN PROGRAM *)
  FOR BOTTLES := 99 DOWNTO 1 DO
    BEGIN
      WRITE (' ',BOTTLES:1,CBOTTLE);
      IF BOTTLES>1 THEN WRITE ('S');
      WRITE (CBEER,CWALL,', ',BOTTLES:1,CBOTTLE);
      IF BOTTLES>1 THEN WRITE ('S');
      WRITELN (CBEER,'.');

      WRITE (' ',CTAKE,', ');
      IF BOTTLES-1 = 0 THEN
        WRITELN('NO MORE',CBOTTLE,'S',CBEER,CWALL,'.')
      ELSE
        BEGIN
          WRITE (BOTTLES-1:1,CBOTTLE);
          IF (BOTTLES-1)>1 THEN WRITE ('S');
          WRITELN(CBEER,CWALL,'.');
        END;
      WRITELN;
    END;

  WRITELN (' NO MORE',CBOTTLE,'S',CBEER,CWALL,', ',
           'NO MORE',CBOTTLE,'S',CBEER,'.');
  WRITELN (' GO TO THE STORE AND BUY SOME MORE, ',
           '99',CBOTTLE,'S',CBEER,CWALL,'.');
END.
//                                                                      00060001
