//PI4ASM   JOB (001),'Pi spigot4 asm',                                  00010000
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=1024K           00020000
//HELLO  EXEC ASMFCLG                                                   00030000
//ASM.SYSUT1 DD UNIT=SYSDA                                              00040000
//ASM.SYSUT2 DD UNIT=SYSDA                                              00050000
//ASM.SYSUT3 DD UNIT=SYSDA                                              00060000
//ASM.SYSGO DD UNIT=SYSDA                                               00070000
//ASM.SYSIN DD *                                                        00080000
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
I        EQU   4
J        EQU   5
J4       EQU   6
SUM      EQU   7
CARRY    EQU   8
ARR      EQU   9
OUTBUFI  EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
DESIRED  EQU   9000
DIGITS   EQU   DESIRED*14/4
MAIN     CSECT
         STM   R14,R12,12(R13)  SAVE THOSE REGISTERS
         LR    R12,R15
         USING MAIN,R12
         ST    R13,SAVEAREA+4   ! store address of caller's save area
         LR    R2,R13
         LA    R13,SAVEAREA     ! load address of my save area
         ST    R13,8(R2)        ! store address of my save area in
*                               !    caller's save area
         L     R1,=A(DIGITS*4+4)
         GETMAIN R,LV=(R1)
         LR    ARR,R1
*
         L     R0,=F'2000'
         L     R1,=A(DIGITS*4)
LLOOP    ST    R0,0(ARR,R1)
         S     R1,=F'4'
         BNM   LLOOP
*
* Set up output
*
         OPEN  (SYSPRINT,(OUTPUT))
*
* Set up output buffer
*
         LA    OUTBUFI,2(0,0)         Get past initial 0
*
         SR    CARRY,CARRY carry := 0
*
* FOR I=DIGITS,1,-14
*
         L     I,=A(DIGITS)
FORITOP  EQU   *
         SR    SUM,SUM
*
* FOR J = I, 1, -1
*
         LR    J,I
FORJTOP  EQU   *
*
* SUM = SUM*J + SCALE*ARR[J]
*
         LR    J4,J
         SLA   J4,2                make J4 an offset
* put SUM*J into R2/R3
         SR    R2,R2
         LR    R3,SUM
         MR    R2,J
* put SCALE*ARR[J] into R0/R1
         SR    R0,R0
         L     R1,=F'10000'
         M     R0,0(ARR,J4)
* add
         AR    R1,R3
         LR    SUM,R1
*
* ARR[J] = SUM % (J*2 - 1)
* SUM = SUM / (J*2 - 1)
*
* put J*2 - 1 into R3
         LR    R3,J
         SLA   R3,1
         S     R3,=F'1'
         SR    R0,R0
         LR    R1,SUM
         DR    R0,R3
         ST    R0,0(ARR,J4) save SUM mod R3 to A[J]
         LR    SUM,R1     save SUM div R3 to SUM
*
* finish FOR J loop
*
         S     J,=F'1'
         BP    FORJTOP    keep going if J > 0 !!!!!!!!!!!!!!!!!!
*
* 4 digits are carry + sum / SCALE
*
         SR    R0,R0
         LR    R1,SUM
         D     R0,=F'10000'
         AR    R1,CARRY   r1 = carry + (sum / scale)
         LR    CARRY,R0   carry = sum % SCALE
         CVD   R1,CVDBUF
*         XPRNT =CL133' 4 digits in R1 and in CVDBUF'
*         XDUMP
*         XDUMP DECBUF,DECBUFL
         UNPK  UNPBUF,CVDBUF
         OI    UNPBUF+3,X'F0'
*         XDUMP DECBUF,DECBUFL
*         XDECO R1,OUTBUF
         LA    R1,OUTBUF
         AR    R1,OUTBUFI
         MVC   0(4,R1),UNPBUF
*         XDUMP OUTBUF,OUTBUFL
         A     OUTBUFI,=F'4'
         C     OUTBUFI,=A(OUTBUFL)
         BL    OUTDONE
         PUT   SYSPRINT,OUTBUF
*         XPRNT OUTBUF,OUTBUFL
         S     OUTBUFI,=F'132'
         MVC   OUTBUF+1,OUTBUFX
OUTDONE  EQU   *
*
* finish FOR I loop
*
         S     I,=F'14'
         BP    FORITOP    keep going if I > 0 !!!!!!!!!!!!!!!!!!
*
* All done
*
RETURNA  EQU   *
*
* close up
*
         PUT   SYSPRINT,OUTBUF
         CLOSE (SYSPRINT)
*
         L     R1,=A(DIGITS*4+4)
         FREEMAIN R,A=(ARR),LV=(R1)
*
*        WTO   'Committing suicide'
*        ABEND 666,DUMP
*
         L     R13,SAVEAREA+4     ! load address of caller's save area
         LM    R14,R12,12(R13)    RELOAD THOSE REGISTERS
         SR    R15,R15
         BR    R14
*
         DS    0D
DECBUF   EQU   *
CVDBUF   DC    CL8' '
UNPBUF   DC    CL4' '
DECBUFL  EQU   *-DECBUF
*
SYSPRINT DCB   DDNAME=SYSPRINT,DSORG=PS,LRECL=133,MACRF=(PM),RECFM=FA
*
SAVEAREA DS    18F
         DS    0D
OUTBUF   DC    CL133' 0'
OUTBUFL  EQU   *-OUTBUF
OUTBUFX  DC    CL132' '      overflow
         LTORG
         END
$ENTRY
/*
//GO.SYSPRINT DD SYSOUT=A
//GO.SYSUDUMP DD SYSOUT=A
