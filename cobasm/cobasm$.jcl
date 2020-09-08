//CALLASM1 JOB (),COMPILE.COBASM,CLASS=A,MSGCLASS=A
//COMPCOB  EXEC COBUCG,                                                 00000500
//         PARM.COB='FLAGW,LOAD,SUPMAP,SIZE=2048K,BUF=1024K'            00000600
//COB.SYSPUNCH DD DUMMY                                                 00000700
//COB.SYSIN DD *                                                        00000800
      ******************************************************************00000900
       IDENTIFICATION DIVISION.                                         00001000
      ******************************************************************00001100
       PROGRAM-ID. 'CALLASM1'.                                          00001200
      ******************************************************************00001300
       ENVIRONMENT DIVISION.                                            00001400
      ******************************************************************00001500
      ******************************************************************00001600
       DATA DIVISION.                                                   00001700
      ******************************************************************00001800
       WORKING-STORAGE SECTION.                                         00001900
           01 WS-ID         PIC 9(5)   VALUE 1.                         00002000
           01 WS-SUBPGM     PIC X(8)   VALUE 'SLEEP'.                   00002102
           01 WS-SLEEP-PARM.
             03 WS-SLEEP-PARM-LENGTH PICTURE 999 COMPUTATIONAL VALUE 4.
             03 WS-SLEEP-PARM-TEXT PIC X(4) VALUE '0100'.
      ******************************************************************00002300
       PROCEDURE DIVISION.                                              00002400
      ******************************************************************00002500
           DISPLAY 'WS-ID: ' WS-ID.                                     00002600
           CALL 'SLEEP' USING WS-SLEEP-PARM.                            00002702
      *    CALL 'CALLSUB3' USING WS-SLEEP-SECS.                         00002802
           DISPLAY 'WS-ID  : ' WS-ID.                                   00002900
           STOP RUN.                                                    00003000
/*                                                                      00003100
//*OB.SYSLIB DD DISP=SHR,DSN=DASTA.PROGRAM.COBLOAD                      00003200
//GO.SYSLIB DD DISP=SHR,DSN=WEGSCD.LOADLIB
//          DD DISP=SHR,DSN=SYS1.COBLIB
//GO.SYSOUT   DD SYSOUT=*                                               00003400
//                                                                      00003500
