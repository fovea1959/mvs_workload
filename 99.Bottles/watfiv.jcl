//WATFIV   JOB (SYS),'99 BOTTLES WATFIV',CLASS=S,MSGCLASS=A             00010000
//B99W5   EXEC WATFIV                                                   00230000
//SYSIN DD *                                                            00240000
$JOB
      DO 10 J=1,99
        I = 100 - J
        PRINT, I,' BOTTLES OF BEER ON THE WALL, ',I,' BOTTLES OF BEER'
        PRINT, 'TAKE ONE DOWN, PASS IT AROUND, ',I-1,
     .       ' BOTTLES OF BEER ON THE WALL'
10    CONTINUE
      END
$ENTRY
/*
//
