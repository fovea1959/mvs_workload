//PISBPSCL JOB (001),'Pi spigot SB Pascal',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1)
//* 
//* this is broken: 
//* difference in digit 906: '5349042875' <> '0089709314'
//*
//STNYBRK EXEC SBPASCAL,SECONDS=120
//PASCAL.SYSIN DD *
PROGRAM PI_SPIGOT(OUTPUT);
CONST
  N   = 9000;
  LEN = 3000; (* 10*N DIV 3 *)
 
VAR
  I, J, K, Q, X, NINES, PREDIGIT: INTEGER;
  A: ARRAY[0..LEN] OF INTEGER;

  COLUMN: INTEGER;
 
PROCEDURE WRITEDIGIT (V : INTEGER; FORCE: BOOLEAN);
BEGIN
 IF (V < 0) OR (V > 9) THEN
  BEGIN
   WRITELN;
   WRITE ('****** ');
   WRITELN(V);
   COLUMN := 1;
  END
 ELSE
  CASE V OF
   0 : WRITE('0');
   1 : WRITE('1');
   2 : WRITE('2');
   3 : WRITE('3');
   4 : WRITE('4');
   5 : WRITE('5');
   6 : WRITE('6');
   7 : WRITE('7');
   8 : WRITE('8');
   9 : WRITE('9');
  END;

 IF (COLUMN > 130) OR FORCE THEN
  BEGIN
   WRITELN;
   COLUMN := 1;
  END
 ELSE
  COLUMN := COLUMN + 1;
END;
 
BEGIN
 COLUMN := 1;

 FOR J := 1 TO LEN DO A[J] := 2; (* START WITH 2S *)
 NINES := 0; PREDIGIT := 0; (* FIRST PREDIGIT IS A 0 *)
 FOR J := 1 TO N DO
  BEGIN
   Q := 0;
   FOR I := LEN DOWNTO 1 DO (* WORK BACKWARDS *)
    BEGIN
     X:=10*A[I] + Q*I;
     A[I] := X MOD (2*I-1);
     Q:=X DIV (2*I-1);
    END;
   A[1] := Q MOD 10; Q := Q DIV 10;
   IF Q = 9
    THEN NINES := NINES + 1
    ELSE IF Q = 10 THEN
     BEGIN
      WRITEDIGIT(PREDIGIT+1, FALSE);
      FOR K := 1 TO NINES DO WRITEDIGIT(0,FALSE);(* ZEROS *)
      PREDIGIT := 0; NINES := 0
     END
    ELSE 
     BEGIN
      WRITEDIGIT(PREDIGIT, FALSE); PREDIGIT := Q;
      IF NINES <> 0 THEN
       BEGIN
        FOR K := 1 TO NINES DO WRITEDIGIT(9, FALSE);
        NINES := 0
       END
     END
  END;
 WRITEDIGIT(PREDIGIT, TRUE);
END.
/*
//
