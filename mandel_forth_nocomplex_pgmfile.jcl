//FHGRSM@# JOB (SYS),'Mbrot grey nocmplx',CLASS=C,MSGCLASS=A,           00010000
//             MSGLEVEL=(1,1)
/*JOBPARM CARDS=9999999
//*MIN: 30s
//SIMCLG  EXEC FORTHCLG,PARM.FORT='OPT=1'
//FORT.SYSLIN DD UNIT=SYSDA                                             00040000
//FORT.SYSABEND DD SYSOUT=A                                             00050000
//FORT.SYSIN DD *                                                       00060000
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
      WRITE (7, 9999) XS, YS
9999  FORMAT (18H# FORTH_nocomplex_, E14.8, 1H_, E14.8, 4H.PGM)
      WRITE (7, 9998) IW, IH, IM
9998  FORMAT (2HP2, 3I5)
C
      DO 1000 IY = 1, IH
       CI = YS - ((IY-1)*YSPAN/IH)
C
       DO 500 IX = 1, IW
        CR = XS + ((IX-1)*YSPAN/IW)
        IRES = IMAND (CR, CI, ITER, IM)
        IF (IRES .GT. 0) ITER = 0
        WRITE (7, 9997) ITER
9997    FORMAT (I5)
500    CONTINUE
1000  CONTINUE
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
       IF (ZRSQR + ZISQR .LT. 4.0) GOTO 90
        IMAND = 0
        ITER = I
        RETURN
90     CONTINUE
100   CONTINUE
      IMAND = 1
      ITER = I
      RETURN
      END
/*
//
