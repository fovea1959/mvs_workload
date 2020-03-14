//GOTHIC   JOB (001),'RUN GOTHIC',                                      00010000
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=128K            00020000
//*
//* ¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢
//* Above are cent signs (ascii x'A2')
//* w/ code page 819/1047, you add it in VIM with a <ctrl-K> 'C' 't'
//* ¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢
//*
//S1      EXEC PGM=GOTHIC                                               00030000
//SYSPRINT  DD SYSOUT=A
//*
//* parallel lines start 'on'
//* '^' EBCDIC x'5F' EBCDIC not-sign turns off lines, depends on mapping
//* '_' EBCDIC x'6D' turns on lines
//* '/' EBCDIC x'61' changes color
//* '#' EBCDIC x'7B' continue to next card
//*
//SYSIN     DD *                                                        00080000
^a/ b_c
^A#
B_b/C
/*
//S2      EXEC PGM=GOTHIC                                               00030000
//SYSPRINT  DD SYSOUT=A
//SYSIN     DD *                                                        00080000
/*
//S2      EXEC PGM=GOTHIC                                               00030000
//SYSPRINT  DD SYSOUT=A
//SYSIN     DD *                                                        00080000
^¢.(&!$);-,?:'"
^ABCDEFGHIJKLMNOPQRSTUVWXYZ
^abcdefghijklmnopqrstuvwxyz
^0123456789
/*
