//VSAMLIST JOB (SYS),'VSAMLIST',CLASS=A,MSGCLASS=A
//STEP1  EXEC  PGM=VSAMLIST,PARM=ALL
//STEPLIB  DD  DSN=SYS2.LINKLIB,DISP=SHR   PROGRAM LOAD MODULES
//*                                      & MVT SORT/MERGE
//*STEPCAT  DD  DSN=VSAMCAT1,DISP=SHR      OPTIONAL STEP CATALOG
//*         .                              OPTIONAL STEP CATALOG
//*         .                              OPTIONAL STEP CATALOG
//*         .                              OPTIONAL STEP CATALOG
//*        DD  DSN=VSAMCATN,DISP=SHR       OPTIONAL STEP CATALOG
//SYSABEND DD  SYSOUT=*
//MSGFILE  DD  SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1330)
//RPTFIL1  DD  SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1330)
//RPTFIL2  DD  SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1330)
//SYSOUT   DD  SYSOUT=*
//SORTIN   DD  DSN=&&DETAIL,DISP=(NEW,DELETE),UNIT=2314,
//             SPACE=(CYL,(5,5))
//SORTLIB  DD  DSN=SYS1.SORTLIB,DISP=SHR   MVT SORT/MERGE LIBRARY
//SORTWK01 DD  UNIT=2314,SPACE=(CYL,(3))
//SORTWK02 DD  UNIT=2314,SPACE=(CYL,(3))
//SORTWK03 DD  UNIT=2314,SPACE=(CYL,(3))
//*                                                                     
//* FOLLOWING OUTPUT FILE IS OPTIONAL.  IF IT POINTS TO A DASD DATA     
//* SET AND 'PARM=ALL' IS SPECIFIED ON THE 'EXEC' CARD, A HISTORY       
//* FILE FOR VSAM ENTRIES WILL BE CREATED.  PLEASE NOTE THE SPEC'S      
//* IN THE DCB.  RECORDS ARE 158 BYTES LONG.                            
//*VSAMHIST DD  DSN=VSAM.HIST.FILE,DISP=(,PASS),                        
//*             UNIT=SYSDA,SPACE=(CYL,(2,1)),                           
//*             DCB=(RECFM=FB,LRECL=158,BLKSIZE=6162)                   
//*                                                                     
//*  FOLLOWING INPUT FILE IS FOR VSAM CATLG LIST UTIL CONTROL STMTS     
//*  '/' INDICATE AN OPTIONAL MASTER PASSWORD FOLLOWS                   
//*  ALL KEYWORDS SHOULD BE ON COL. 1 THRU 71 OF THE SAME CARD FOR      
//*  EACH VSAM CATALOG TO BE LISTED                                     
//VCLCNTL  DD  *                                                        
  LISTCAT  SYS1.UCAT.TSO
//                                                                      
