//PI4ASST  JOB (SYS),'pi4 spigot ASSIST',CLASS=A,MSGCLASS=A
//PI4     EXEC ASSIST,PARM='BATCH,MACRO=H,NOCPAGE'
//SYSIN     DD *
$JOB     ASSIST XREF=3,P=1000,NOCPAGE,R=10000
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
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
DESIRED  EQU   90
DIGITS   EQU   DESIRED*14/4
MAIN     CSECT
         STM   R14,R12,12(R13)        SAVE THOSE REGISTERS
         BALR  R12,0
         USING *,R12
         LA    ARR,ARRS
         XDUMP ARRS,ALEN
         SR    CARRY,CARRY carry := 0
         XDUMP
*         XPRNT =CL133' Starting FOR J'
* FOR I=DIGITS,1,-14
*
         LA    I,DIGITS
FORITOP  EQU   *
         SR    SUM,SUM
*
* FOR J = I, 1, -14
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
         BH    FORJTOP    keep going of J > 0 !!!!!!!!!!!!!!!!!!
*
* 4 digits are carry + sum / SCALE
*
         SR    R0,R0
         LR    R1,SUM
         D     R0,=F'10000'
         AR    R1,CARRY
         LR    CARRY,R0   carry = sum % SCALE
         XPRNT =CL133' 4 digits in R1'
         XDUMP
         XDECO R1,OUTBUF
         XPRNT OUTBUF,OUTBUFL
*
* finish FOR I loop
*
         S     I,=F'14'
         BH    FORITOP    keep going if I > 0 !!!!!!!!!!!!!!!!!!
*
* All done
*
RETURNA  EQU   *
         LM    R14,R12,12(R13)        RELOAD THOSE REGISTERS
         BR    R14
ARRS     DC    (DIGITS+1)F'2000'
ALEN     EQU   *-ARRS
OUTBUF   DC    CL13' '
OUTBUFL  EQU   *-OUTBUF
         LTORG
         END
$ENTRY
/*
