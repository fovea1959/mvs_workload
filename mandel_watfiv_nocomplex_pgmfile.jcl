//W5GRSM@# JOB (SYS),'Mbrot grey nocmplx',CLASS=C,MSGCLASS=A            00010000
/*JOBPARM CARDS=9999999
//*MIN: 30s
//SIMPGM  EXEC WATFIV                                                   00230000
//SYSIN DD *                                                            00240000
$JOB TIME=1440
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
      PUNCH , '# watfiv_nocomplex_', XS, '_', YS, '.pgm'
      PUNCH , 'P2'
      PUNCH , IW
      PUNCH , IH
      PUNCH , IM
      IY = 1
      WHILE (IY .LE. IH) DO
        CI = YS - ((IY-1)*YSPAN/IH)
        IX = 1
C
        WHILE (IX .LE. IW) DO
          CR = XS + ((IX-1)*YSPAN/IW)
C          PRINT , X, Y
C
          IRES = IMAND (CR, CI, ITER, IM)
          IF (IRES .GT. 0) THEN DO
           PUNCH , 0
          ELSE DO
           PUNCH , ITER
          END IF
C
          IX = IX + 1
        END WHILE
        IY = IY + 1
      END WHILE
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
