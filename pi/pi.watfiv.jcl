//WATFIV   JOB (SYS),'calculate pi watfiv',CLASS=E,MSGCLASS=A
$JOB           FOO,TIME=(10,0),PROF
      INTEGER N/10000/, LEN/33333/
      INTEGER I, J, K, Q, X, NINES, PREDIG
      INTEGER A(33333)
C
      DO 10 J = 1, LEN
       A(J) = 2
10    CONTINUE
C
      NINES = 0
      PREDIG = 0
C
      DO 1000 J = 1, N
       Q = 0
       DO 500 II = 1, LEN
        I = LEN - II + 1
C
        X = 10 * A(I) + Q * I
        A(I) = MOD (X, 2 * I - 1)
        Q = X / (2 * I - 1)
 500   CONTINUE
C
       A(1) = MOD (Q, 10)
       Q = Q / 10
       IF (Q .NE. 9) GOTO 600
C       Q == 9
        NINES = NINES + 1
        GOTO 999
  600  IF (Q .NE. 10) GOTO 700
C       Q == 10
        PRINT, 'q+', PREDIG + 1
        IF (NINES .EQ. 0) GOTO 601
         DO 605 K = 1, NINES
          PRINT, 'qq', 0
  605    CONTINUE
  601   CONTINUE
        PREDIG = 0
        NINES = 0
        GOTO 999
  700   CONTINUE 
C       Q IS SOMETHING ELSE
        PRINT, 'qx', PREDIG
        PREDIG = Q
        IF (NINES .EQ. 0) GOTO 705
         DO 703 K = 1, NINES
          PRINT, 'q9', 9
  703    CONTINUE
         NINES = 0
  705   CONTINUE
  999  CONTINUE
 1000 CONTINUE  
      PRINT, 'qp', PREDIG
      STOP
      END
$ENTRY
//
