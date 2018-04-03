//WATFIVAA JOB (SYS),'Mandelbrot punch',CLASS=S,MSGCLASS=A              00010000
//        EXEC WATFIV                                                   00230000
//SYSIN DD *                                                            00240000
$JOB
      COMPLEX R
C
      COMPLEX MAND
      LOGICAL CZERO
C
      DATA IW /320/
      DATA IH /240/
C
      PUNCH , 'P2'
      PUNCH , IW
      PUNCH , IH
      PUNCH , 255
      IY = 1
      WHILE (IY .LE. IH) DO
        Y = 1.0 - ((IY-1)*2.0/IH)
        IX = 1
C
        WHILE (IX .LE. IW) DO
          X = (-2.0) + ((IX-1)*3.0/IW)
C          PRINT , X, Y
C
          ITER = 0
          R = MAND (CMPLX (X, Y), ITER)
          IF (CZERO(R)) THEN DO
            ITER = 0
          END IF
          PUNCH , ITER
C
          IX = IX + 1
        END WHILE
        IY = IY + 1
      END WHILE
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
      COMPLEX FUNCTION MAND (ZIN, ITER)
      COMPLEX ZIN
      COMPLEX Z, C
      Z = ZIN
      C = ZIN
      DO 10 I = 1, 255
        Z = Z*Z + C
        ITER = I
        IF (CABS2(Z) .GT. 4) THEN DO
          MAND = Z
          GOTO 20
        END IF
   10 CONTINUE
      MAND = (0.0,0.0)
   20 CONTINUE
C     PUNCH, ZIN, MAND
      RETURN
      END
$ENTRY
/*
//
