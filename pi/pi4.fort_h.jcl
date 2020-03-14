//PI4FORTH JOB (001),'PI4 spigot fortran h',                            00010000
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),TIME=60                00020000
//        EXEC FORTHCLG,REGION.FORT=384K,PARM.FORT='NOLIST'
//FORT.SYSLIN DD UNIT=SYSDA                                             00040000
//FORT.SYSABEND DD SYSOUT=A                                             00050000
//FORT.SYSIN DD *                                                       00060000
C     decimal DIGITS = 'digits' * 14 / 4
C     DIGITS = 31500 means 9000 digits
C
      INTEGER DIGITS /31500/
C
      INTEGER ARR(31500)
      INTEGER I, IREV, J, JREV, SUM, CARRY, D
C
      CALL OUTINI
      CALL OUT (0, 'Ld')
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
9999   FORMAT (5H D = , I4)
       CALL OUT4 (D)
       CARRY = MOD (SUM, 10000)
1000  CONTINUE
      CALL OUTFL
      STOP
      END
      SUBROUTINE OUTINI
      COMMON /OUTBUF/ OI, O
      INTEGER OI
      INTEGER O(132)
C
      INTEGER SPACE/1H /
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
9999   FORMAT (5H I = , I2, 5H J = , I4)
       CALL OUT (J, 'i4')
       NN = NN * 10
10    CONTINUE
      RETURN
      END
      SUBROUTINE OUT (I, Y)
      COMMON /OUTBUF/ OI, O
      INTEGER OI
      INTEGER O(132)
C
      INTEGER Y
      INTEGER DIGIT(10) /1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9/
      O(OI) = DIGIT(I+1)
      OI = OI + 1
      IF (OI .GT. 132) CALL OUTFL
      RETURN
      END
      SUBROUTINE OUTFL
      COMMON /OUTBUF/ OI, O
      INTEGER OI
      INTEGER O(132)
      WRITE (6, 99) O
99    FORMAT (1H , 132A1)
      CALL OUTINI
      RETURN
      END
/*
