//MVS0020 JOB  (SETUP),
//             'Usermod ZUM0001',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1)
//*********************************************************************
//*
//* Name: SYS1.SETUP.CNTL(MVS0020)
//*
//* Desc: Install usermod ZUM0001 to authorize various TSO commands
//*
//*********************************************************************
//*
//ADD1    EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS1.UMODCNTL
//SYSIN    DD  DATA,DLM='><'
./ ADD NAME=ZUM0001
//ZUM0001 JOB  (SETUP),
//             'Usermod ZUM0001',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1)
//*********************************************************************
//*
//* Name: SYS1.UMODCNTL(ZUM0001)
//*
//* Desc: Install usermod ZUM0001 to authorize various TSO commands
//*
//*********************************************************************
//*
/*MESSAGE  ******************************************
/*MESSAGE  *                                        *
/*MESSAGE  * This Usermod becomes effective only    *
/*MESSAGE  * if you do an IPL with the CLPA option  *
/*MESSAGE  *                                        *
/*MESSAGE  ******************************************
//ASM     EXEC SMPASML,M=IKJEFTE2
//RECAPP  EXEC SMPAPP
//SMPPTFIN DD  *
++ USERMOD(ZUM0001).
++ VER(Z038)
   FMID(EBB1102)
   .
++ MOD(IKJEFTE2)
   DISTLIB(AOST4)
   LKLIB(UMODLIB)
   .
//SMPCNTL  DD  *
 REJECT  SELECT(ZUM0001)
 .
 RESETRC
 .
 RECEIVE SELECT(ZUM0001)
 .
 APPLY SELECT(ZUM0001)
       DIS(WRITE)
 .
><
//ADD2    EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS1.UMODSRC
//SYSIN    DD  DATA,DLM='><'
./ ADD NAME=IKJEFTE2
         ENTRY APFCTABL
IKJEFTE2 CSECT
         DC    CL8'IKJEFTE2'
         DC    CL8'&SYSDATE'
APFCTABL DC    CL8'#       '       CMDSBSYS TSO interface
         DC    CL8'CMDSBTSO'       CMDSBSYS TSO interface
         DC    CL8'IEBCOPY '       Copy under TSO
         DC    CL8'IM      '       IMON/370
         DC    CL8'LISTD   '       TSO List Dataset commands
         DC    CL8'LISTDS  '       ditto
         DC    CL8'IKJEHDS1'       ditto
         DC    CL8'PDS     '       PDS Utility
         DC    CL8'PDSAA   '       PDS Utility (Test)
         DC    CL8'PDS73   '       PDS Utility (Alias)
         DC    CL8'PDS85   '       PDS Utility (Alias)
         DC    CL8'QUEUE   '       QUEUE COMMAND
         DC    CL8'Q       '       ALIAS
         DC    CL8'QUE     '       ALIAS
         DC    CL8'RPF     '       SPARE TABLE ENTRIES
         DC    CL8'RPFMAIN '       SPARE TABLE ENTRIES
         DC    CL8'SPFCOPY '       Copy under TSO
         DC    CL8'        '       8 BLANKS TABLE TERMINATOR
         END
         ENTRY APFCTABL
><
//SUBMIT  EXEC PGM=IEBGENER
//SYSPRINT DD  DUMMY
//SYSIN    DD  DUMMY
//SYSUT1   DD  DISP=SHR,DSN=SYS1.UMODCNTL(ZUM0001)
//SYSUT2   DD  SYSOUT=(A,INTRDR)
