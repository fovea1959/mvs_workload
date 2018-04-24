//MNDPRT@# JOB (SYS),'Mbrot prt noCMPX',CLASS=A,MSGCLASS=A              00010000
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
          ZR = 0.0
          ZI = 0.0
          ZRSQR = 0.0
          ZISQR = 0.0
          II = 0
20        CONTINUE
            ZI = ZR * ZI
            ZI = ZI + ZI
            ZI = ZI + CI
            ZR = ZRSQR - ZISQR + CR
            ZRSQR = ZR * ZR
            ZISQR = ZI * ZI
            II = II + 1
            IF (II .GT. IMX) GOTO 30
          IF (ZRSQR + ZISQR .LT. 4.0) GOTO 20
30        CONTINUE
C
          IF (II .GT. IMX) THEN DO
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
$ENTRY
/*
//
