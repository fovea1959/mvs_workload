//PIWATFIV JOB (SYS),'PI spigot watfiv',CLASS=B,MSGCLASS=A,TIME=120
//        EXEC WATFIV,TIME.GO=120
//SYSIN     DD *
$JOB           ACCT,TIME=(120,0),NOCHECK,LINES=0
      INTEGER N/9000/, LEN/30000/
C     LEN = (N * 10) DIV 3
      INTEGER I, J, K, Q, X, NINES, PREDIG
      INTEGER A(30000)
C
      CALL OUTINI
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
        CALL OUT (PREDIG + 1, 'Q+')
        IF (NINES .EQ. 0) GOTO 601
         DO 605 K = 1, NINES
          CALL OUT (0, 'QQ')
  605    CONTINUE
  601   CONTINUE
        PREDIG = 0
        NINES = 0
        GOTO 999
  700   CONTINUE 
C       Q IS SOMETHING ELSE
        CALL OUT (PREDIG, 'qx')
        PREDIG = Q
        IF (NINES .EQ. 0) GOTO 705
         DO 703 K = 1, NINES
          CALL OUT (9, 'q9')
  703    CONTINUE
         NINES = 0
  705   CONTINUE
  999  CONTINUE
 1000 CONTINUE  
      CALL OUT (PREDIG, 'qp')
      CALL OUTFL
      STOP
      END
      SUBROUTINE OUTINI
      COMMON /OUTBUF/ OI, O
      INTEGER OI
      CHARACTER O(132)
      OI = 1
      DO 10 I = 1, 132
       O(I) = ' '
10    CONTINUE
      RETURN
      END
      SUBROUTINE OUT (I, Y)
      COMMON /OUTBUF/ OI, O
      INTEGER OI
      CHARACTER O(132)
C
      CHARACTER*2 Y
      CHARACTER C(10) /
     *  '0', '1', '2', '3', '4',
     *  '5', '6', '7', '8', '9'/
      O(OI) = C(I+1)
      OI = OI + 1
      IF (OI .GT. 132) CALL OUTFL
      RETURN
      END
      SUBROUTINE OUTFL
      COMMON /OUTBUF/ OI, O
      INTEGER OI
      CHARACTER O(132)
      PRINT 99, O
99    FORMAT (1H , 132A1)
      CALL OUTINI
      RETURN
      END
$ENTRY
//
