//MNDPGM@# JOB (SYS),'Mandelbrot grey',CLASS=C,MSGCLASS=A               00010000
/*JOBPARM CARDS=9999999
//*MIN: 30s
//        EXEC WATFIV                                                   00230000
//SYSIN DD *                                                            00240000
$JOB TIME=1440
      COMPLEX R
C
      COMPLEX MAND
      LOGICAL CZERO
C
      DATA IW /1024/
      DATA IH /1024/
      DATA IM /256/
C
      PUNCH , 'P2'
      PUNCH , IW
      PUNCH , IH
      PUNCH , IM
      IY = 1
      WHILE (IY .LE. IH) DO
        Y = 2.0 - ((IY-1)*4.0/IH)
        IX = 1
C
        WHILE (IX .LE. IW) DO
          X = (-2.0) + ((IX-1)*4.0/IW)
C          PRINT , X, Y
C
          ITER = 0
          R = MAND (CMPLX (X, Y), ITER, IM)
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
      COMPLEX FUNCTION MAND (ZIN, ITER, IM)
      COMPLEX ZIN
      COMPLEX Z, C
      Z = ZIN
      C = ZIN
      IMP1 = IM + 1
      DO 10 I = 1, IMP1
        Z = Z*Z + C
        ITER = I - 1
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
