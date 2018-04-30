//MNDPPM@# JOB (SYS),'Mandelbrot color',CLASS=C,MSGCLASS=A              00010000
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
      INTEGER MAXITR
C
      LOGICAL QT /.FALSE./
C
      INTEGER IR(1024), IG(1024), IB(1024)
      INTEGER IIR, IIG, IIB
C
      DO 100 I = 1, 1024
        READ(5,*,END=110) IIR, IIG, IIB
        MAXITR = I
        IR(I) = IIR
        IG(I) = IIG
        IB(I) = IIB
        PRINT, 'RGB', I, IIR, IIG, IIB
100   CONTINUE
110   MAXITR = MAXITR - 1
      PRINT, 'MAXITR = ', MAXITR
C
      IF (QT) THEN DO
        STOP
      END IF
C
      PUNCH , '# complex.ppm'
      PUNCH , 'P3'
      PUNCH , IW
      PUNCH , IH
      PUNCH , 256
      IY = 1
      WHILE (IY .LE. IH) DO
        Y = 2.0 - ((IY-1)*4.0/IH)
        IX = 1
C
        WHILE (IX .LE. IW) DO
          X = (-2.0) + ((IX-1)*4.0/IW)
          ITER = 0
          R = MAND (CMPLX (X, Y), ITER, MAXITR)
          IF (CZERO(R)) THEN DO
            PUNCH , 0, 0, 0
          ELSE DO
            ICIDX = ITER + 1
            PUNCH , IR(ICIDX), IG(ICIDX), IB(ICIDX)
          END IF
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
      COMPLEX FUNCTION MAND (ZIN, ITER, MAXITR)
      COMPLEX ZIN
      COMPLEX Z, C
      Z = ZIN
      C = ZIN
      IMP1 = MAXITR + 1
      DO 10 I = 1, IMP1
        Z = Z*Z + C
        ITER = I - 1
        IF (CABS2(Z) .GT. 4) THEN DO
          MAND = Z
          RETURN
        END IF
   10 CONTINUE
      MAND = (0.0,0.0)
      RETURN
      END
$ENTRY
  0   0   0
  0   0   0
  0   0   4
  0   0  12
  0   0  16
  0   0  24
  0   0  32
  0   0  36
  0   0  44
  0   0  48
  0   0  56
  0   0  64
  0   0  68
  0   0  76
  0   0  80
  0   0  88
  0   0  96
  0   0 100
  0   0 108
  0   0 116
  0   0 120
  0   0 128
  0   0 132
  0   0 140
  0   0 148
  0   0 152
  0   0 160
  0   0 164
  0   0 172
  0   0 180
  0   0 184
  0   0 192
  0   0 200
  0   4 200
  0  12 200
  0  16 204
  0  24 204
  0  28 208
  0  36 208
  0  40 208
  0  48 212
  0  56 212
  0  60 216
  0  68 216
  0  72 216
  0  80 220
  0  84 220
  0  92 224
  0 100 224
  0 104 224
  0 112 228
  0 116 228
  0 124 232
  0 128 232
  0 136 232
  0 140 236
  0 148 236
  0 156 240
  0 160 240
  0 168 240
  0 172 244
  0 180 244
  0 184 248
  0 192 248
  0 200 252
  4 200 252
 12 200 252
 20 204 252
 28 204 252
 36 208 252
 44 208 252
 52 208 252
 60 212 252
 68 212 252
 76 216 252
 84 216 252
 92 216 252
100 220 252
108 220 252
116 224 252
124 224 252
132 224 252
140 228 252
148 228 252
156 232 252
164 232 252
172 232 252
180 236 252
188 236 252
196 240 252
204 240 252
212 240 252
220 244 252
228 244 252
236 248 252
244 248 252
252 252 252
248 252 252
244 252 252
240 252 252
232 252 252
228 252 252
224 252 252
216 252 252
212 252 252
208 252 252
200 252 252
196 252 252
192 252 252
184 252 252
180 252 252
176 252 252
168 252 252
164 252 252
160 252 252
156 252 252
148 252 252
144 252 252
140 252 252
132 252 252
128 252 252
124 252 252
116 252 252
112 252 252
108 252 252
100 252 252
 96 252 252
 92 252 252
 84 252 252
 80 252 252
 76 252 252
 72 252 252
 64 252 252
 60 252 252
 56 252 252
 48 252 252
 44 252 252
 40 252 252
 32 252 252
 28 252 252
 24 252 252
 16 252 252
 12 252 252
  8 252 252
  0 252 252
  0 248 252
  0 244 252
  0 240 252
  0 232 252
  0 228 252
  0 224 252
  0 216 252
  0 212 252
  0 208 252
  0 200 252
  0 196 252
  0 192 252
  0 184 252
  0 180 252
  0 176 252
  0 168 252
  0 164 252
  0 160 252
  0 156 252
  0 148 252
  0 144 252
  0 140 252
  0 132 252
  0 128 252
  0 124 252
  0 116 252
  0 112 252
  0 108 252
  0 100 252
  0  96 252
  0  92 252
  0  84 252
  0  80 252
  0  76 252
  0  72 252
  0  64 252
  0  60 252
  0  56 252
  0  48 252
  0  44 252
  0  40 252
  0  32 252
  0  28 252
  0  24 252
  0  16 252
  0  12 252
  0   8 252
  0   0 252
  0   0 248
  0   0 244
  0   0 240
  0   0 236
  0   0 232
  0   0 228
  0   0 224
  0   0 220
  0   0 216
  0   0 212
  0   0 208
  0   0 204
  0   0 200
  0   0 196
  0   0 192
  0   0 188
  0   0 184
  0   0 180
  0   0 176
  0   0 172
  0   0 168
  0   0 164
  0   0 160
  0   0 156
  0   0 152
  0   0 148
  0   0 144
  0   0 140
  0   0 136
  0   0 132
  0   0 128
  0   0 124
  0   0 120
  0   0 116
  0   0 112
  0   0 108
  0   0 104
  0   0 100
  0   0  96
  0   0  92
  0   0  88
  0   0  84
  0   0  80
  0   0  76
  0   0  72
  0   0  68
  0   0  64
  0   0  60
  0   0  56
  0   0  52
  0   0  48
  0   0  44
  0   0  40
  0   0  36
  0   0  32
  0   0  28
  0   0  24
  0   0  20
  0   0  16
  0   0  12
  0   0   8
  0   0   0
  0   0   0
/*
//
