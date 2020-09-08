//WEGSCDX JOB (),COMPILE.SLEEP,CLASS=A,MSGCLASS=A,REGION=2M
//SLEEP   EXEC ASMFCL,MAC1='SYS2.MACLIB'                                00000703
SLEEP    TITLE 'SLEEP SECONDS'                                          00000903
SLEEP    CSECT                                                          00001000
         STM   R14,R12,12(R13)                                          00001100
         LR    R12,R15                                                  00001200
         USING SLEEP,R12                                                00001300
         LR    R15,R13                                                  00001400
         LA    R13,SAVEAREA                                             00001500
         ST    R13,8(R15)                                               00001600
         ST    R15,4(R13)                                               00001700
         LR    R5,R1                                                    00001800
         L     R2,0(0,R5)                                               00001900
         LH    R3,0(0,R2)                                               00002000
         LTR   R3,R3                                                    00002100
         BZ    PAUSA                                                    00002200
         L     R5,2(0,R2)                                               00002300
         CH    R3,=H'4'                                                 00002400
         BE    SHIFT0                                                   00002500
         CH    R3,=H'3'                                                 00002600
         BE    SHIFT8                                                   00002700
         CH    R3,=H'2'                                                 00002800
         BE    SHIFT16                                                  00002900
         CH    R3,=H'1'                                                 00003000
         BE    SHIFT24                                                  00003100
         B     PAUSA                                                    00003200
SHIFT24  SRL   R5,8(0)                                                  00003300
SHIFT16  SRL   R5,8(0)                                                  00003400
SHIFT8   SRL   R5,8(0)                                                  00003500
SHIFT0   EQU *                                                          00003600
         ST    R5,ARGDISP                                               00003700
         PACK  ARGPACK,ARGDISP                                          00003800
         CVB   R7,ARGPACK                                               00003900
         MH    R7,=H'100'                                               00004000
         ST    R7,SLEEPTIM                                              00004100
*                                                                       00004200
PAUSA    STIMER WAIT,BINTVL=SLEEPTIM                                    00004300
*                                                                       00004400
         L     R13,SAVEAREA+4                                           00004500
         LM    R14,R12,12(R13)                                          00004600
         SR    R15,R15                                                  00004700
         BR    R14                                                      00004800
SAVEAREA DC    18F'0'                                                   00004900
SLEEPTIM DC    F'500'                                                   00005000
ARGDISP  DS    CL4                                                      00005100
         DS    0D                                                       00005200
ARGPACK  DS    CL8                                                      00005300
         YREGS                                                          00005400
         END                                                            00005500
/*                                                                      00005602
//LKED.SYSLMOD DD DSN=WEGSCD.LOADLIB(SLEEP),DISP=SHR
//
