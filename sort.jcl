//SORT##S JOB (SETUP), 
// 'TEST SORT', 
// CLASS=D, 
// MSGCLASS=A 
//*MIN: 30s
//SORTDG EXEC PGM=IEBDG 
//SYSPRINT DD SYSOUT=A 
//SEQOUT DD UNIT=3390,DCB=(LRECL=80,RECFM=FB,BLKSIZE=8000), 
// DSN=&&SORTTEMP,DISP=(,PASS),SPACE=(CYL,(200,20)) 
//SYSIN DD DATA 
 DSD OUTPUT=(SEQOUT) ,, 
 FD NAME=F1,LENGTH=2,STARTLOC=1,FORMAT=RA 
 FD NAME=F2,LENGTH=4,STARTLOC=3,FORMAT=RA 
 FD NAME=F3,LENGTH=4,STARTLOC=7,FORMAT=RA 
 FD NAME=F4,LENGTH=10,STARTLOC=11,FORMAT=ZD,INDEX=1 
 FD NAME=F5,LENGTH=60,STARTLOC=21 
 CREATE QUANTITY=3000000,NAME=(F1,F2,F3,F4,F5) 
 END 
/* 
//SORT EXEC PGM=SORT,REGION=512K,PARM='MSG=AP' 
//SYSOUT DD SYSOUT=A 
//SYSUDUMP DD SYSOUT=A 
//SYSPRINT DD SYSOUT=A 
//SORTLIB DD DSNAME=SYS1.SORTLIB,DISP=SHR 
//SORTOUT DD DUMMY,DCB=(BLKSIZE=80,RECFM=F) 
//SORTWK00 DD UNIT=2314,SPACE=(CYL,(185,5)),VOL=SER=SORT00 
//SORTWK01 DD UNIT=2314,SPACE=(CYL,(185,5)),VOL=SER=SORT01 
//SORTWK02 DD UNIT=2314,SPACE=(CYL,(185,5)),VOL=SER=SORT02 
//SORTWK03 DD UNIT=2314,SPACE=(CYL,(185,5)),VOL=SER=SORT03 
//SORTWK04 DD UNIT=2314,SPACE=(CYL,(185,5)),VOL=SER=SORT04 
//SORTWK05 DD UNIT=2314,SPACE=(CYL,(185,5)),VOL=SER=SORT05 
//SORTWK06 DD UNIT=2314,SPACE=(CYL,(185,5)),VOL=SER=SORT06 
//SORTWK07 DD UNIT=2314,SPACE=(CYL,(185,5)),VOL=SER=SORT07 
//SORTWK08 DD UNIT=2314,SPACE=(CYL,(185,5)),VOL=SER=SORT08 
//SORTWK09 DD UNIT=2314,SPACE=(CYL,(185,5)),VOL=SER=SORT09 
//SORTWK10 DD UNIT=2314,SPACE=(CYL,(185,5)),VOL=SER=SORT10 
//SORTWK11 DD UNIT=2314,SPACE=(CYL,(185,5)),VOL=SER=SORT11 
//SORTWK12 DD UNIT=2314,SPACE=(CYL,(185,5)),VOL=SER=SORT12 
//SORTWK13 DD UNIT=2314,SPACE=(CYL,(185,5)),VOL=SER=SORT13 
//SORTWK14 DD UNIT=2314,SPACE=(CYL,(185,5)),VOL=SER=SORT14 
//SORTWK15 DD UNIT=2314,SPACE=(CYL,(185,5)),VOL=SER=SORT15 
//SYSIN DD * 
 SORT FIELDS=(1,10,CH,A) 
 RECORD TYPE=F,LENGTH=(80) 
 END 
/* 
//SORTIN DD UNIT=3390,DSN=&&SORTTEMP,DISP=SHR 
