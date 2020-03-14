//HERC02U JOB ,,CLASS=C,MSGCLASS=A,NOTIFY=HERC01
//*
//* Changes VTAM's "Z NET" timer from 45 seconds to 1 second.
//* Since the timer normally starts after applications are down,
//* it shouldn't cause any problem?
//*
//SMPREC  EXEC PROC=SMPREC      This JCL is the SMP version of UMZNETX
//SMPPTFIN DD  DATA,DLM=XX
++ USERMOD (UMZNETX)        /*    5752-SC123-801
//UMZNETX   JOB 5752-33299-0,SC123,MSGLEVEL=(1,1),CLASS=A     */  .
++ VER (Z038)
   FMID(EVT0108)
 /*
  Problem Description(s):
           VTAM waits 45 seconds after applications have shut
           down for the applications to shut down.
           This USERMOD changes the wait to 1 second.
 */.
++ ZAP (ISTINCAW).
 NAME ISTINM01 ISTINCAW
 VER 19A8 0000002D,FFFFFFFF  TIME DC F'45' Wait seconds after Apps. end
 REP 19A8 00000001           TIME DC F'1'  Wait second  after Apps. end
 IDRDATA UMZNETX
++ ZAP (ISTINCRR).
 NAME ISTINM01 ISTINCRR
 VER 1030 0000002D        CRRTIME DC F'45' Wait seconds after Apps. end
 REP 1030 00000001        CRRTIME DC F'1'  Wait second  after Apps. end
 IDRDATA UMZNETX
XX
//SMPCNTL  DD  *
 RECEIVE SELECT(UMZNETX).
//*
//SMPAPP  EXEC PROC=SMPAPP
//SMPCNTL  DD  *
 APPLY SELECT(UMZNETX) DIS(WRITE).
//AOS26    DD  DISP=SHR,DSN=SYS1.AOS26  Needed if a RESTORE
//*
