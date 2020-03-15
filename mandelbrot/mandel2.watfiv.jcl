//WATFIVaa JOB (SYS),'WATFIV Mandelbrot 2',CLASS=S,MSGCLASS=A           00010000
//        EXEC WATFIV                                                   00230000
//SYSIN DD *                                                            00240000
$JOB
      COMPLEX R
      CHARACTER*1 L(80)
C
      DATA LL/80/
      COMMON IMAX
      DATA IMAX/20/
C
      INTEGER MAND
      LOGICAL CZERO
C
      WRITE (6, 9998)
9998  FORMAT (1H1)

      PUNCH , 'P2'
      PUNCH , 80
      PUNCH , 40
      PUNCH , 255
      Y = 1
      WHILE (Y .GE. -1) DO
        DO 10 I = 1, LL
          L(I) = ' '
10      CONTINUE
C
        X = -2
        I = 1
        WHILE (X .LE. 0.5) DO
          J = MAND (CMPLX (X, Y))
          PUNCH , J
          IF (J .LE. 0) THEN DO
            L(I) = '#'
          ELSE DO
            L(I) = ' '
          END IF
C
          X = X + 0.0315
          I = I + 1
        END WHILE
        WRITE (6, 999) L
999     FORMAT (1H , 132A1)
C
        Y = Y - 0.05
      END WHILE
      WRITE (6, 9998)
      STOP                                                              00100000
      END                                                               00110000
      REAL FUNCTION CABS2 (Z)
      COMPLEX Z
      REAL R, I
C
      R = REAL(Z)
      I = AIMAG(Z)
C
      CABS2 = R*R + I*I
      RETURN
      END
      LOGICAL FUNCTION CZERO (Z)
      COMPLEX Z
      CZERO = .TRUE.
      IF (CABS(Z) .GT. 0) THEN DO
        CZERO = .FALSE.
      END IF
      RETURN
      END
      INTEGER FUNCTION MAND (ZIN)
      COMPLEX ZIN
      COMMON IMAX
      COMPLEX Z, C
      Z = ZIN
      C = ZIN
      DO 10 I = 1, IMAX
        Z = Z*Z + C
        IF (CABS2(Z) .GT. 4) THEN DO
          MAND = I
          GOTO 20
        END IF
   10 CONTINUE
      MAND = 0
   20 CONTINUE
C     PUNCH, ZIN, MAND
      RETURN
      END
$ENTRY
/*
//
