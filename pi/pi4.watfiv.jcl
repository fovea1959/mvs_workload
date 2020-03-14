//PI4WAT5  JOB (SYS),'PI4 spigot watfiv',CLASS=B,MSGCLASS=A,TIME=120
//        EXEC WATFIV,TIME.GO=120
//SYSIN     DD *
$JOB           ACCT,TIME=(120,0),NOCHECK,LINES=0
C     decimal DIGITS = 'digits' * 14 / 4
C     DIGITS = 31500 means 9000 digits
C
      INTEGER DIGITS /31500/
C
      INTEGER ARR(31500)
      INTEGER I, IREV, J, JREV, SUM, CARRY, D
C
      CALL OUTINI
      CALL OUT (0)
C
      CARRY = 0
C
      DO 10 I = 1, DIGITS
        ARR(I) = 2000
10    CONTINUE
C
      DO 1000 IREV = 1, DIGITS, 14
       I = DIGITS - IREV + 1
C
       SUM = 0
       DO 500 JREV = 1, I
        J = I - JREV + 1
C
        SUM = SUM*J + 10000*ARR(J)
        ARR(J) = MOD (SUM, J*2 - 1)
        SUM = SUM / (J*2 - 1)
500    CONTINUE
C
       D = CARRY + (SUM/10000) 
C       WRITE (6, 9999) D
C9999   FORMAT (5H D = , I4)
       CALL OUT4 (D)
       CARRY = MOD (SUM, 10000)
1000  CONTINUE
      CALL OUTFL
      STOP
      END
      SUBROUTINE OUTINI
      COMMON /OUTBUF/ OI, O
      INTEGER OI
      CHARACTER*1 O(132)
C
      CHARACTER*1 SPACE/1H /
      OI = 1
      DO 10 I = 1, 132
       O(I) = SPACE
10    CONTINUE
      RETURN
      END
      SUBROUTINE OUT4 (N)
      INTEGER I, N, NN
      NN = N
      DO 10 I = 1, 4
       J = MOD (NN / 1000, 10)
C       WRITE (6, 9999) I, J
C9999   FORMAT (5H I = , I2, 5H J = , I4)
       CALL OUT (J)
       NN = NN * 10
10    CONTINUE
      RETURN
      END
      SUBROUTINE OUT (I)
      COMMON /OUTBUF/ OI, O
      INTEGER OI
      CHARACTER*1 O(132)
C
      CHARACTER*1 DIGIT(10) /'0','1','2','3','4','5','6','7','8','9'/
      O(OI) = DIGIT(I+1)
      OI = OI + 1
      IF (OI .GT. 132) CALL OUTFL
      RETURN
      END
      SUBROUTINE OUTFL
      COMMON /OUTBUF/ OI, O
      INTEGER OI
      CHARACTER*1 O(132)
      WRITE (6, 99) O
99    FORMAT (1H , 132A1)
      CALL OUTINI
      RETURN
      END
$ENTRY
//
