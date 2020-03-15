//FORTGCLG JOB (001),'FTG HELLO WORLD',                                 00010000
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)                        00020000
//HELOWRLD EXEC FORTGCLG,REGION.FORT=1024K                              00030000
//FORT.SYSLIN DD UNIT=SYSDA                                             00040000
//FORT.SYSABEND DD SYSOUT=A                                             00050000
//FORT.SYSIN DD *                                                       00060000
C HELLO WORLD, WE HOPE                                                  00070000
      WRITE(6,10)                                                       00080000
   10 FORMAT(12H HELLO WORLD)                                           00090000
      STOP                                                              00100000
      END                                                               00110000
//                                                                      00120000
