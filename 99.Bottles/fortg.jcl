//FIVG99   JOB (001),'99 BOTTLES FORT-G',
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//F4G99    EXEC FORTGCLG,REGION.FORT=1024K
//FORT.SYSLIN DD UNIT=SYSDA
//FORT.SYSABEND DD SYSOUT=A
//FORT.SYSIN DD *
C
C FORTRAN-IV VERSION OF 99 BOTTLES OF BEER
C DAVE PITTS, DPITTS AT COZX.COM
C
      DO 30 J = 1, 98
      I = 100 - J
      WRITE (6,100) I, I
      WRITE (6,110)
      I = I - 1
      IF (I .GE. 2) GO TO 20
      WRITE (6,125) I
      GO TO 30
20    WRITE (6,120) I
30    CONTINUE
      I = 1
      WRITE (6,105) I, I
      WRITE (6,110)
      WRITE (6,130)
      CALL EXIT
C
100   FORMAT (1H0,I2,30H BOTTLES OF BEER ON THE WALL,
     1,I2,16H BOTTLES OF BEER)
105   FORMAT (1H0,I2,29H BOTTLE OF BEER ON THE WALL,
     1,I2,15H BOTTLE OF BEER)
110   FORMAT (33H TAKE ONE DOWN AND PASS IT AROUND)
120   FORMAT (1H ,I2,17H BOTTLES OF BEER.)
125   FORMAT (1H ,I2,16H BOTTLE OF BEER.)
130   FORMAT (20H NO BOTTLES OF BEER.)
      END
//
�
