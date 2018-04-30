//W5GRCX@# JOB (SYS),'Mbrot grey cmplx',CLASS=C,MSGCLASS=A              00010000
/*JOBPARM CARDS=9999999
//*MIN: 30s
//CMPLXPGM EXEC WATFIV                                                  00230000
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
      DATA Y0 /2.0/, Y1 /-2.0/
      DATA X0 /-2.0/, X1 /2.0/
C
      XS = AMIN1(X0,X1)
      YS = AMAX1(Y0,Y1)
      XSPAN = ABS(X1-X0)
      YSPAN = ABS(Y1-Y0)
C
      PUNCH , '# watfiv_complex_', XS, '_', YS, '.pgm'
      PUNCH , 'P2'
      PUNCH , IW
      PUNCH , IH
      PUNCH , IM
      IY = 1
      WHILE (IY .LE. IH) DO
        Y = YS - ((IY-1)*YSPAN/IH)
        IX = 1
C
        WHILE (IX .LE. IW) DO
          X = XS + ((IX-1)*XSPAN/IW)
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
      Z = (0.0, 0.0)
      C = ZIN
      DO 10 I = 1, IM
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
