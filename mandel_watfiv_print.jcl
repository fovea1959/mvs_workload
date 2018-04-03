//WATFIVaa JOB (SYS),'WATFIV Mandelbrot',CLASS=S,MSGCLASS=A             00010000
//        EXEC WATFIV                                                   00230000
//SYSIN DD *                                                            00240000
$JOB
      COMPLEX R
      CHARACTER*1 L(132)
C
      COMPLEX MAND
      LOGICAL CZERO
C
      WRITE (6, 9998)
9998  FORMAT (1H1)
      Y = 1
      WHILE (Y .GE. -1) DO
        DO 10 I = 1, 132
          L(I) = ' '
10      CONTINUE
C
        X = -2
        I = 1
        WHILE (X .LE. 0.5) DO
          R = MAND (CMPLX (X, Y))
          IF (CZERO(R)) THEN DO
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
      COMPLEX FUNCTION MAND (ZIN)
      COMPLEX ZIN
      COMPLEX Z, C
      Z = ZIN
      C = ZIN
      DO 10 I = 1, 20
        Z = Z*Z + C
        IF (CABS2(Z) .GT. 4) THEN DO
          MAND = Z
          GOTO 20
        END IF
   10 CONTINUE
      MAND = (0.0,0.0)
   20 CONTINUE
      RETURN
      END
$ENTRY
/*
//
