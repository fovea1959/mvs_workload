//W5PRSM@# JOB (SYS),'Mbrot prt noCMPX',CLASS=B,MSGCLASS=A              00010000
//*MIN: 30s
//        EXEC WATFIV                                                   00230000
//SYSIN DD *                                                            00240000
$JOB TIME=1440
      DATA IMX /20/
      CHARACTER*1 L(132)
C
      WRITE (6, 9998)
9998  FORMAT (1H1)
C
C     imaginary axis is Y
C
      CI = 1.0
      WHILE (CI .GE. -1) DO
        DO 10 I = 1, 132
          L(I) = ' '
10      CONTINUE
C
        CR = -2.0
        I = 1
        WHILE (CR .LE. 0.5) DO
          IRES = IMAND (CR, CI, ITER, 20)
          IF (IRES .GT. 0) THEN DO
            L(I) = '#'
          ELSE DO
            L(I) = ' '
          END IF
C
          CR = CR + 0.0315
          I = I + 1
        END WHILE
        WRITE (6, 999) L
999     FORMAT (1H , 132A1)
C
        CI = CI - 0.05
      END WHILE
      WRITE (6, 9998)
      STOP                                                              00100000
      END                                                               00110000
      FUNCTION IMAND (CR, CI, ITER, IMX)
C     non zero return value means in set
      ZR = 0.0
      ZI = 0.0
      ZRSQR = 0.0
      ZISQR = 0.0
      ITER = 0
      DO 100 I = 1, IMX
       ZI = ZR * ZI
       ZI = ZI + ZI
       ZI = ZI + CI
       ZR = ZRSQR - ZISQR + CR
       ZRSQR = ZR * ZR
       ZISQR = ZI * ZI
       IF (ZRSQR + ZISQR .GE. 4.0) THEN DO
        IMAND = 0
        ITER = I
        RETURN
       END IF
100   CONTINUE
      IMAND = 1
      ITER = I
      RETURN
      END
$ENTRY
/*
//
