//PLICVT   JOB (001),'pli_cvt.jcl',TIME=1,                              00010000
//             CLASS=A,MSGCLASS=A,MSGLEVEL=0,REGION=256K                00020000
//*
//* The PL/I vertical bar is dec 79, hex 4F
//* WITH DEFAULT HERC CODE PAGE, ascii version is dec 215, hex D7
//* with 819/1049 (TK3UPD), ascii version is dec 124, hex 7c
//*
//PI     EXEC PL1LFCLG,PARM.PL1L='LOAD,NODECK'                          00030000
//PL1L.SYSLIN DD UNIT=SYSDA                                             00040000
//PL1L.SYSIN DD *                                                       00050000
 NAMSTEP: PROCEDURE OPTIONS(MAIN);
             /* THIS SUBROUTINE RETURNS THE CURRENT NAME          */
             /* DATA, AS LISTED BELOW, TO THE CHARACTER STRING    */
             /* 'RETSTRING'.  THIS DATA INCLUDES:                 */
             /*    CHAR 1-8:      JOBNAME                         */
             /*    CHAR 9-16:     JOB STEP NAME, OR FOR A PROC,   */
             /*                      THE PROC STEP NAME           */
             /*    CHAR 17-24:    JOB STEP NAME FOR A PROC        */
             /* THIS ROUTINE IS WRITTEN FOR OS-MVT, REL 20.       */
 DCL RETSTRING                     CHARACTER (24),
    GENSTRING                     CHARACTER (24)  BASED (PTR_PTR),
    GENPTR                        POINTER         BASED (PTR_PTR),
    BINARY_NR                     FIXED BINARY (31, 0) STATIC,
    PTR_PTR                       POINTER  STATIC;
    BINARY_NR = 16;
    UNSPEC (PTR_PTR) = UNSPEC (BINARY_NR);
             /* PTR_PTR POINTS AT LOC 16, THE ADDR OF CVT.        */
    PTR_PTR = GENPTR;
             /* PTR_PTR POINTS AT CVT.                            */
    PTR_PTR = GENPTR;
             /* PTR_PTR POINTS AT TCB WORDS.                      */
    UNSPEC (BINARY_NR) = UNSPEC (PTR_PTR);
    BINARY_NR = BINARY_NR + 4;
    UNSPEC (PTR_PTR) = UNSPEC (BINARY_NR);
             /* PTR_PTR POINTS AT 2ND OF TCB-WORDS = 'CURRENT TCB'*/
    PTR_PTR = GENPTR;
             /* PTR_PTR POINTS AT TCB                             */
    UNSPEC (BINARY_NR) = UNSPEC (PTR_PTR);
    BINARY_NR = BINARY_NR + 12;
    UNSPEC (PTR_PTR) = UNSPEC (BINARY_NR);
    PTR_PTR = GENPTR;
             /* PTR_PTR POINTS AT TIOT.                           */
    RETSTRING = GENSTRING;
    DISPLAY(RETSTRING);
   END NAMSTEP;
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR                               00090000
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR                                00100000
//GO.SYSPRINT DD SYSOUT=A,DCB=(LRECL=133,RECFM=FA)
//                                                                      00110000
