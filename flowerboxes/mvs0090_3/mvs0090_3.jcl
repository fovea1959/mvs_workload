//MVS0090 JOB  (SMP),'INSTALL IEFACTRT',                                00010000
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=4096K           00020000
//********************************************************************* 00030000
//*                                                                     00040000
//*                       MVS 3.8 SYSGEN                                00050000
//*                       ==============                                00060000
//*                                                                     00070000
//* DESC: Install USERMOD ZUM0002 TO provide step accounting            00080000
//*       information and a step-to-RC summary                          00090000
//*                                                                     00100000
//********************************************************************* 00110000
//*                                                                     00120000
/*MESSAGE  *******************************************                  00130000
/*MESSAGE  *                                         *                  00140000
/*MESSAGE  * This Usermod becomes effective only     *                  00150000
/*MESSAGE  * if you do an IPL with the CLPA option   *                  00160000
/*MESSAGE  *                                         *                  00170000
/*MESSAGE  *******************************************                  00180000
//CLEANUP EXEC PGM=IDCAMS                                               00190000
//SYSPRINT DD  SYSOUT=*                                                 00200000
//SYSIN    DD  *                                                        00210000
 DELETE SYS1.UMODSRC(IEFACTRT) NONVSAM                                  00220000
 SET MAXCC = 0                                                          00230000
 SET LASTCC = 0                                                         00240000
//ADD     EXEC PGM=IEBUPDTE,PARM=NEW                                    00250000
//SYSPRINT DD  SYSOUT=*                                                 00260000
//SYSUT2   DD  DISP=SHR,DSN=SYS1.UMODSRC                                00270000
//SYSIN    DD  DATA,DLM=$$                                              00280000
./ ADD NAME=IEFACTRT                                                    00290000
*        REG                                                    *JLM*
R0       EQU   0                                                *JLM*   00000030
R1       EQU   1                                                *JLM*   00000040
R2       EQU   2                                                *JLM*   00000050
R3       EQU   3                                                *JLM*   00000060
R4       EQU   4                                                *JLM*   00000070
R5       EQU   5                                                *JLM*   00000080
R6       EQU   6                                                *JLM*   00000090
R7       EQU   7                                                *JLM*   00000100
R8       EQU   8                                                *JLM*   00000110
R9       EQU   9                                                *JLM*   00000120
R10      EQU   10                                               *JLM*   00000130
R11      EQU   11                                               *JLM*   00000140
R12      EQU   12                                               *JLM*   00000150
R13      EQU   13                                               *JLM*   00000160
R14      EQU   14                                               *JLM*   00000170
R15      EQU   15                                               *JLM*   00000180
         TITLE 'IEFACTRT - TERMINATION EXIT'
IEFACTRT CSECT
         STM   R14,R12,12(R13)     SAVE CALLER'S REGISTERS
         LR    R11,R15             SAVE ENTRY POINT ADDRESS
         USING IEFACTRT,R11        SET UP PROGRAM ADDRESSABILITY
         LR    R2,R0               SAVE R0 (STEP/JOB INDICATOR)
         LR    R3,R1               SAVE R1 (PARAMETER LIST)
         GETMAIN R,LV=WORKEND-WORKAREA,SP=253 GET WORKING STORAGE
         ST    R13,4(R1)           SAVE CALLER'S SAVE AREA ADDRESS
         ST    R1,8(R13)           SAVE OUR SAVE AREA ADDRESS
         LR    R13,R1              SET UP SAVE AREA REGISTER
         USING WORKAREA,R13        SET UP WORK AREA ADDRESSABILITY
         L     R4,36(R3)           ADDRESS OF RDW OF SMF RECORD
         LA    R4,4(R4)            ADDRESS OF START OF SMF RECORD
         CLI   1(R4),X'05'         RECORD TYPE 5?
         BH    ENDACTRT            DON'T PROCESS IT
         EJECT
***********************************************************************
*                                                                     *
*   WRITE TOP DASH LINE, BLANK LINE, HEADER AND BLANK LINE            *
*                                                                     *
***********************************************************************
         LA    R0,120              WRITE TOP DASH LINE FOR BOX
         LA    R1,DASHES
         BAL   R10,WRITE
         LA    R0,120              WRITE BLANK LINE
         LA    R1,SPACES
         BAL   R10,WRITE
********************************** SET UP HEADER LINE
         MVC   LINE,HEADER         MOVE HEADER TO WORKAREA
         L     R5,16               ADDRESS OF CVT
         LA    R5,0(R5)            CHOB (CLEAR HIGH ORDER BYTE)
         S     R5,=F'8'            GET TO CVT PREFIX
         MVC   LINE+68(2),4(R5)    MOVE RELEASE NUMBER TO HEADER
         MVC   LINE+71(2),6(R5)
         XC    DECW,DECW           CLEAR FIELD
         MVC   DECW+2(2),2(R5)     MOVE CPU MODEL NUMBER (E.G. 0145)
         MVI   DECW+4,X'0C'        ADD SIGN (E.G. 01450C)
         DP    DECW(5),=P'10'      DIVIDE BY 10 (E.G. 00145C)
         MVC   LINE+84(4),=X'40202120' SET UP EDIT MASK
         ED    LINE+84(4),DECW+1   TRANSLATE CPU MODEL NUMBER TO EBCDIC
         LA    R0,120              WRITE HEADER
         LA    R1,LINE
         BAL   R10,WRITE
********************************** WRITE BLANK LINE
         LA    R0,120
         LA    R1,SPACES
         BAL   R10,WRITE
         SPACE 3
***********************************************************************
*                                                                     *
*   SPLIT OFF TO DO STEP OR JOB TERMINATION                           *
*                                                                     *
***********************************************************************
         C     R2,=F'12'           IS IT STEP TERMINATION ?
         BE    STPACTRT            YIP - GO DO IT
         EJECT
JOBACTRT DS    0H
***********************************************************************
*                                                                     *
*   JOB TERMINATION - FIRST HEADER AND DETAIL LINE                    *
*                                                                     *
***********************************************************************
         LA    R0,120              WRITE FIRST JOB HEADER
         LA    R1,JOBHEAD1
         BAL   R10,WRITE
         MVC   LINE,SPACES
         L     R1,SMF5JIT(R4)      CONVERT JOB START TIME
         BAL   R10,TIME
         MVC   LINE+3(11),TIMECHAR
         L     R1,SMF5TME(R4)      CONVERT JOB STOP TIME
         BAL   R10,TIME
         MVC   LINE+16(11),TIMECHAR
********************************** COMPUTE JOB ELAPSED TIME
         MVC   DECW+4(4),SMF5DTE(R4) JOB END DATE
         SP    DECW+4(4),SMF5JID(4,R4) MINUS JOB START DATE
         XC    DECW(4),DECW        EQUALS NUMBER OF DAYS
         CVB   R1,DECW             CONVERT TO BINARY
         M     R0,=A(24*60*60*100)  TIMES 100THS OF SECONDS PER DAY
         A     R1,SMF5TME(R4)      PLUS JOB END TIME
         S     R1,SMF5JIT(R4)      MINUS JOB START TIME
**********************************
         BAL   R10,TIME            CONVERT JOB ELAPSED TIME
         MVC   LINE+29(11),TIMECHAR
         LA    R1,0                CONVERT JOB CPU TIME
         ICM   R1,B'0111',SMF5JCPU(R4)
         BAL   R10,TIME
         MVC   LINE+42(11),TIMECHAR
         ICM   R1,B'0111',SMF5SRBT(R4) CONVERT JOB SRB TIME
         LA    R1,0(R1)            CHOB
         BAL   R10,TIME
         MVC   LINE+55(11),TIMECHAR
         MVC   LINE+70(8),SMF5JBN(R4) MOVE IN JOBNAME
         MVC   LINE+79(7),=X'4020204B202120' CONVERT READER DATE
         ED    LINE+79(7),SMF5RSD+1(R4)
         L     R1,SMF5RST(R4)      CONVERT READER START TIME
         BAL   R10,TIME
         MVC   LINE+90(11),TIMECHAR
         MVC   LINE+111(6),=C'NORMAL'
         TM    SMF5JBTI(R4),X'02'  DID JOB ABEND ?
         BZ    CALLCOD5            NO - SKIP NEXT INSTRUCTION
         MVC   LINE+111(6),=CL6'ABEND '
CALLCOD5 LA    R0,120              WRITE FIRST JOB LINE
         LA    R1,LINE
         BAL   R10,WRITE
         LA    R0,120              WRITE BLANK LINE
         LA    R1,SPACES
         BAL   R10,WRITE
         EJECT
***********************************************************************
*                                                                     *
*   JOB TERMINATION - SECOND HEADER AND DETAIL LINE                   *
*                                                                     *
***********************************************************************
         LA    R0,120              WRITE 2ND JOB HEADER
         LA    R1,JOBHEAD2
         BAL   R10,WRITE
         MVC   LINE,SPACES         CLEAR PRINT LINE
         CLI   SMF5ACTF(R4),X'00'  ACCOUNT FIELDS PRESENT IN RECORD ?
         BE    SKIPACCT            NO - SKIP NEXT CODE
         LA    R5,LINE+5           TARGET ADDRESS FOR 1ST ACCT FIELD
         LA    R6,SMF5JSAF(R4)     1ST ACCT ENTRY IN SMF RECORD
         LA    R7,0                CLEAR IC REGISTER
         IC    R7,0(R6)            INSERT 1ST ACCT ENTRY LENGTH
         LTR   R7,R7               CHECK FOR ZERO LENGTH ?
         BZ    NOACCT1             ZERO - SKIP FIRST FIELD
         BCTR  R7,0                MINUS ONE FOR MVC
         EX    R7,MOVEACCT         MOVE 1ST ACCT FIELD
         LA    R7,1(R7)            RESET LENGTH
         LA    R5,0(R5,R7)         BUMP PAST TARGET FIELD
NOACCT1  MVI   0(R5),C','          APPEND COMMA
         LA    R5,1(R5)            BUMP PAST COMMA
         CLI   SMF5ACTF(R4),X'01'  ONLY ONE ACCT FIELD PRESENT /
         BE    SKIPACCT            YIP - SKIP NEXT CODE
         LA    R6,1(R6,R7)         2ND ACCT ENTRY IN SMF RECORD
         IC    R7,0(R6)            INSERT 2ND ACCT ENTRY LENGTH
         LTR   R7,R7               CHECK FOR ZERO LENGTH
         BZ    SKIPACCT            ZERO - SKIP NEXT CODE
         BCTR  R7,0                MINUS ONE FOR MVC
         EX    R7,MOVEACCT         MOVE 2ND ACCT FIELD
         B     SKIPACCT            SKIP OVER MVC
MOVEACCT MVC   0(0,R5),1(R6)
SKIPACCT DS    0H
         EJECT
         MVC   LINE+20(20),SMF5PRGN(R4) MOVE PROGRAMMER NAME
         L     R1,SMF5TTAT(R4)     CONVERT JOB TRANSACTION ACTIVE TIME
         M     R0,=F'1024'         TO MICROSECONDS
         D     R0,=F'10000'        TO 100THS OF SECONDS
         BAL   R10,TIME            CONVERT TO EBCDIC
         MVC   LINE+55(11),TIMECHAR
         L     R5,SMF5TJS(R4)      CONVERT JOB SERVICE UNITS
         CVD   R5,DECW
         MVC   LINE+69(12),=X'402020202020202020202120'
         ED    LINE+69(12),DECW+2
         LH    R5,SMF5PGNO(R4)     CONVERT PERFORMANCE GROUP NUMBER
         CVD   R5,DECW
         MVC   LINE+101(4),=X'40202120'
         ED    LINE+101(4),DECW+6
         L     R6,28(R3)           GET 8TH FULLWORD PARAMETER
         LA    R5,0                CLEAR IC REGISTER
         IC    R5,1(R6)            INSERT NUMBER OF JOB STEPS
         CVD   R5,DECW             CONVERT IT
         MVC   LINE+111(4),=X'40202120'
         ED    LINE+111(4),DECW+6
         LA    R0,120              WRITE 2ND JOB LINE
         LA    R1,LINE
         BAL   R10,WRITE
         LA    R0,120              WRITE A BLANK LINE
         LA    R1,SPACES
         BAL   R10,WRITE
         LA    R0,120              WRITE BOTTOM DASHED LINE
         LA    R1,DASHES
         BAL   R10,WRITE
         B     ENDACTRT            GO RETURN TO CALLER
         EJECT
***********************************************************************
*                                                                     *
*  STEP TERMINATION - FIRST HEADER AND DETAIL LINE                    *
*                                                                     *
***********************************************************************
STPACTRT LA    R0,120              WRITE FIRST STEP HEADER
         LA    R1,STPHEAD1
         BAL   R10,WRITE
         MVC   LINE,SPACES
         L     R1,SMF4SIT(R4)      CONVERT STEP START TIME
         BAL   R10,TIME
         MVC   LINE+3(11),TIMECHAR
         L     R1,SMF4TME(R4)      CONVERT STEP STOP TIME
         BAL   R10,TIME
         MVC   LINE+16(11),TIMECHAR
********************************** COMPUTE STEP ELAPSED TIME
         MVC   DECW+4(4),SMF4DTE(R4) STEP END DATE
         SP    DECW+4(4),SMF4STID(4,R4) MINUS STEP START DATE
         XC    DECW(4),DECW        EQUALS NUMBER OF DAYS
         CVB   R1,DECW             CONVERT TO BINARY
         M     R0,=A(24*60*60*100)  TIMES 100THS OF SECONDS PER DAY
         A     R1,SMF4TME(R4)      PLUS STEP END TIME
         S     R1,SMF4SIT(R4)      MINUS STEP START TIME
**********************************
         BAL   R10,TIME            CONVERT STEP ELAPSED TIME
         MVC   LINE+29(11),TIMECHAR
         L     R5,20(R3)           GET 6TH FULLWORD PARAMETER
         ICM   R1,B'0111',0(R5)    GET STEP CPU TIME
         LA    R1,0(R1)            CHOB
         BAL   R10,TIME            CONVERT STEP CPU TIME
         MVC   LINE+42(11),TIMECHAR
         ICM   R1,B'0111',SMF4SRBT(R4) CONVERT STEP SRB TIME
         LA    R1,0(R1)            CHOB
         BAL   R10,TIME
         MVC   LINE+55(11),TIMECHAR
         MVC   LINE+70(8),SMF4JBN(R4) MOVE IN JOBNAME
         L     R6,28(R3)           GET 8TH FULLWORD PARAMETER
         LA    R5,0                CLEAR IC REGISTER
         IC    R5,1(R6)            GET STEP NUMBER
         CVD   R5,DECW             CONVERT IT
         MVC   LINE+82(4),=X'40202120' SET UP EDIT MASK
         ED    LINE+82(4),DECW+6
         MVC   LINE+90(8),SMF4STMN(R4) MOVE STEP NAME
         MVC   LINE+100(8),SMF4PGMN(R4) MOVE PROGRAM NAME
         LA    R1,SMF4SCC(R4)      CONVERT STEP COMPLETION CODE
         LA    R0,0                ASSUME NO ABEND
         TM    SMF4STI(R4),X'02'   DID STEP ABEND ?
         BZ    CALLCOD4            NO - SKIP NEXT INSTRUCTION
         LA    R0,1                INDICATE STEP ABEND
CALLCOD4 BAL   R10,CODE
         MVC   LINE+111(5),CODECHAR
         LA    R0,120              WRITE FIRST STEP LINE
         LA    R1,LINE
         BAL   R10,WRITE
         LA    R0,120              WRITE BLANK LINE
         LA    R1,SPACES
         BAL   R10,WRITE
         EJECT
***********************************************************************
*                                                                     *
*  STEP TERMINATION - SECOND HEADER AND DETAIL LINE                   *
*                                                                     *
***********************************************************************
         LA    R0,120              WRITE 2ND STEP HEADER
         LA    R1,STPHEAD2
         BAL   R10,WRITE
         MVC   LINE,SPACES         CLEAR PRINT LINE
         LH    R5,SMF4RLCT(R4)     GET OFFSET TO RELOCATE SECTION
         LA    R5,0(R4,R5)         START OF RELOCATE SECTION
         L     R1,SMF4PGIN(R5)     CONVERT PAGE-INS
         CVD   R1,DECW
         MVC   LINE+2(8),=X'4020202020202120'
         ED    LINE+2(8),DECW+4
         L     R1,SMF4PGOT(R5)     CONVERT PAGE-OUTS
         CVD   R1,DECW
         MVC   LINE+16(8),=X'4020202020202120'
         ED    LINE+16(8),DECW+4
         L     R1,SMF4VPI(R5)      CONVERT VIO PAGE-INS
         CVD   R1,DECW
         MVC   LINE+30(8),=X'4020202020202120'
         ED    LINE+30(8),DECW+4
         L     R1,SMF4VPO(R5)      CONVERT VIO PAGE OUTS
         CVD   R1,DECW
         MVC   LINE+42(8),=X'4020202020202120'
         ED    LINE+42(8),DECW+4
*        L     R1,SMF4ACT(R5)      CNVRT STEP TRANS. ACTIV TM JFL80315
*        M     R0,=F'1024'         TO MICROSECONDS            JFL80315
*        D     R0,=F'10000'        TO 100THS OF SECONDS       JFL80315
         L     R1,20(R3)           SIXTH INPUT PARM           JFL80315
         SLR   R0,R0               CLEAR FOR ICM              JFL80315
         ICM   R0,7,0(R1)          STEP TCB                   JFL80315
         SLR   R1,R1               CLEAR FOR ICM              JFL80315
         ICM   R1,7,SMF4SRBT(R4)   STEP SRB                   JFL80315
         AR    R1,R0               STEP CPU = SRB + TCB       JFL80315
         BAL   R10,TIME            CONVERT TO EBCDIC
         MVC   LINE+55(11),TIMECHAR
         L     R1,SMF4SST(R5)      CONVERT STEP SU'S        FJP/12AUG80
         CVD   R1,DECW                                      FJP/12AUG80
         MVC   LINE+69(12),=X'402020202020202020202120'
         ED    LINE+69(12),DECW+2
         LH    R1,SMF4HOST(R4)     CONVERT STORAGE USED
         CVD   R1,DECW
         MVC   LINE+87(7),=X'402020202120D2'
         ED    LINE+87(6),DECW+5
         LH    R1,SMF4PGNO(R5)     CONVERT PERFORMANCE GROUP NUMBER
         CVD   R1,DECW
         MVC   LINE+101(4),=X'40202120'
         ED    LINE+101(4),DECW+6
         LA    R0,120              WRITE 2ND STEP LINE
         LA    R1,LINE
         BAL   R10,WRITE
         LA    R0,120              WRITE A BLANK LINE
         LA    R1,SPACES
         BAL   R10,WRITE
         LA    R0,120              WRITE BOTTOM DASHED LINE
         LA    R1,DASHES
         BAL   R10,WRITE
         EJECT
***********************************************************************
*                                                                     *
*   WRITE EXCP COUNT SUMMARY - SPACES, HEADER1, SPACES, HEADER2       *
*                                                                     *
***********************************************************************
EXPACTRT LH    R5,SMF4LENN(R4)     GET DEVICE ENTRY PORTION LENGTH
         SH    R5,=H'2'            MINUS 2 FOR LENGTH FIELD
         SRL   R5,3                DIVIDED BY 8
         LTR   R5,R5               EQUALS NUMBER OF DEVICE ENTRIES
         BZ    ENDACTRT            IF ZERO - SKIP EXCP COUNT SUMMARY
         LA    R0,85               WRITE A BLANK LINE
         LA    R1,SPACES2
         BAL   R10,WRITE
         LA    R0,85               WRITE FIRST EXCP HEADER
         LA    R1,EXPHEAD1
         BAL   R10,WRITE
         LA    R0,85
         LA    R1,SPACES2          WRITE A BLANK LINE
         BAL   R10,WRITE
         LA    R0,85
         LA    R1,EXPHEAD2         WRITE SECOND EXCP HEADER
         BAL   R10,WRITE
         LA    R7,0                CLEAR DASD EXCP COUNTER
         LA    R8,0                CLEAR TAPE EXCP COUNTER
         LA    R9,0                CLEAR MISC EXCP COUNTER
         LA    R6,SMF4LENN+2(R4)   ADDRESS OF FIRST DEVICE ENTRY
         EJECT
***********************************************************************
*                                                                     *
*   LOOP THROUGH ALL DEVICE ENTRIES  - WRITE EXCP COUNT LINE          *
*                                                                     *
***********************************************************************
NEXTDEVC MVC   LINE(85),SPACES2    CLEAR PRINT LINE
         MVC   DECW+5(2),2(R6)     GET UNIT ADDRESS
         MVI   DECW+7,X'0C'        APPEND JUNK CHARACTER FOR UNPK
         UNPK  LINE+37(5),DECW+5(3) UNPK IT
         MVI   LINE+37,C' '        CLEAR LEADING ZERO
         MVI   LINE+41,C' '        CLEAR TRAILING JUNK CHARACTER
         TR    LINE+38(3),TRTABLE-C'0' TRANSLATE X'FA' TO C'A'
         CLI   0(R6),X'20'         CHECK FOR DASD DEVICE CLASS
         BE    DASDDEVC
         CLI   0(R6),X'80'         CHECK FOR TAPE DEVICE CLASS
         BE    TAPEDEVC
MISCDEVC MVC   LINE+43(6),=C'MISC  ' MAKE IT MISCELLANEOUS
         A     R9,4(R6)            ADD EXCP COUNT TO MISC COUNTER
         LA    R1,LINE+72          TARGET ADDRESS FOR EXCP COUNT
         B     CONVEXCP
TAPEDEVC LA    R1,0                CLEAR IC REGISTER
         IC    R1,1(R6)            GET DEVICE TYPE
         IC    R1,TAPEOFST(R1)     GET OFFSET INTO DEVICE NAME TABLE
         LA    R1,TAPETABL(R1)     ADDRESS OF DEVICE NAME
         MVC   LINE+43(6),0(R1)    MOVE DEVICE NAME
         A     R8,4(R6)            ADD EXCP COUNT TO TAPE COUNTER
         LA    R1,LINE+61          TARGET ADDRESS FOR EXCP COUNT
         B     CONVEXCP
DASDDEVC LA    R1,0                CLEAR IC REGISTER
         IC    R1,1(R6)            GET DEVICE TYPE
         IC    R1,DASDOFST(R1)     GET OFFSET INTO DEVICE NAME TABLE
         LA    R1,DASDTABL(R1)     ADDRESS OF DEVICE NAME
         MVC   LINE+43(6),0(R1)     MOVE DEVICE NAME
         A     R7,4(R6)            ADD EXCP COUNT TO DASD COUNTER
         LA    R1,LINE+50          TARGET ADDRESS FOR EXCP COUNT
CONVEXCP L     R0,4(R6)            EXCP COUNT
         CVD   R0,DECW             CONVERT TO PACKED
         MVC   0(10,R1),=X'40202020202020202120' SET UP EDIT MASK
         ED    0(10,R1),DECW+3     CONVERT TO EBCDIC
         LA    R0,85               WRITE EXCP COUNT LINE
         LA    R1,LINE
         BAL   R10,WRITE
         LA    R6,8(R6)            BUMP TO NEXT DEVICE ENTRY
         BCT   R5,NEXTDEVC         GO AGAIN IF MORE DEVICES TO GO
         EJECT
***********************************************************************
*                                                                     *
*   END OF DEVICES - WRITE TOTALS AND POSSIBLE WARNING                *
*                                                                     *
***********************************************************************
         LA    R0,85               WRITE A BLANK LINE
         LA    R1,SPACES2
         BAL   R10,WRITE
         MVC   LINE(85),EXPTOTAL   SET UP EXCP TOTALS LINE
         CVD   R7,DECW             CONVERT DASD EXCP TOTAL
         MVC   LINE+50(10),=X'40202020202020202120'
         ED    LINE+50(10),DECW+3
         CVD   R8,DECW             CONVERT TAPE EXCP TOTAL
         MVC   LINE+61(10),=X'40202020202020202120'
         ED    LINE+61(10),DECW+3
         CVD   R9,DECW             CONVERT MISC EXCP TOTAL
         MVC   LINE+72(10),=X'40202020202020202120'
         ED    LINE+72(10),DECW+3
         LA    R0,85               WRITE EXCP TOTALS LINE
         LA    R1,LINE
         BAL   R10,WRITE
         TM    SMF4RIN(R4),X'02'   CHECK FOR EXCP COUNT ERRORS
         BZ    SKIPWARN            NO - SKIP WARNING LINE
         LA    R0,85               YES - WRITE WARNING LINE
         LA    R1,EXPWARN
         BAL   R10,WRITE
SKIPWARN LA    R0,85               WRITE A BLANK LINE
         LA    R1,SPACES2
         BAL   R10,WRITE
         MVC   LINE(85),DASHES2    WRITE BOTTOM DASHED LINE
         MVI   LINE+84,C' '
         LA    R0,85
         LA    R1,LINE
         BAL   R10,WRITE
         B     ENDACTRT
         EJECT
***********************************************************************
*                                                                     *
*   RETURN TO CALLER AFTER FREEMAIN OF WORK AREA                      *
*                                                                     *
***********************************************************************
ENDACTRT LR    R1,R13              SAVE OUR SAVE AREA ADDRESS
         L     R13,4(R13)          GET CALLER'S SAVE AREA
         FREEMAIN R,A=(R1),LV=WORKEND-WORKAREA,SP=253
         LM    R14,R12,12(R13)
         LA    R1,0                INDICATE SMF RECORD TO BE WRITTEN
         LA    R15,0               INDICATE JOB IS TO BE CONTINUED
         BR    R14                 RETURN TO CALLER
         EJECT
***********************************************************************
*                                                                     *
*   WRITE ROUTINE - WRITES RECORD TO JOB LOG                          *
*                   INPUT R0 = MESSAGE LENGTH                         *
*                         R1 = MESSAGE ADDRESS                        *
*                                                                     *
***********************************************************************
WRITE    ST    R1,36(R12)          STORE MESSAGE ADDRESS
         STH   R0,42(R12)          STORE MESSAGE LENGTH
         L     R15,=V(IEFYS)       GET IEFYS EPA
         BALR  R14,R15             CALL IEFYS
         BR    R10
         EJECT
***********************************************************************
*                                                                     *
*   TIME CONVERSION ROUTINE - INPUT R1 = TIME IN 100THS OF SECONDS    *
*                            OUTPUT TIMECHAR = HH.MM.SS.XX            *
*                                                                     *
***********************************************************************
TIME     LA    R0,0                CLEAR R0 FOR DIVISION
         D     R0,=A(60*60*100)    COMPUTE HOURS IN R1
         LR    R7,R1
         SRDL  R0,32               MOVE REMAINDER TO R1 (CLEAR R0)
         D     R0,=A(60*100)       COMPUTE MINUTES IN R1
         M     R6,=F'100'          SHIFT LEFT TWO DIGITS
         AR    R7,R1               ADD MINUTES TO HOURS
         SRDL  R0,32               MOVE REMAINDER TO R1 (CLEAR R0)
         D     R0,=A(100)          COMPUTE SECONDS IN R1
*                                  AND 100THS OF SECONDS IN R0
         M     R6,=F'100'          SHIFT LEFT TWO DIGITS
         AR    R7,R1               ADD SECONDS TO MINUTES AND HOURS
         M     R6,=F'100'          SHIFT LEFT TWO DIGITS
         AR    R7,R0               ADD 100THS OF SECONDS
         CVD   R7,DECW             CONVERT TO DECIMAL
         MVC   TIMECHAR-2(13),=X'402120207A20207A20204B2020'
         ED    TIMECHAR-2(13),DECW+3 CONVERT TO EBCDIC
         BR    R10                 AND RETURN
         EJECT
***********************************************************************
*                                                                     *
*   CODE CONVERSION ROUTINE - INPUT R0 = ZERO IF NO ABEND             *
*                                   R1 = ADDRESS OF 2 BYTE FIELD      *
*                            OUTPUT CODECHAR = XXXXX (CONDCODE)       *
*                                            = S XXX (SYSTEM ABEND)   *
*                                            = UXXXX (USER ABEND)     *
*                                                                     *
***********************************************************************
CODE     LTR   R0,R0               CHECK FOR ABEND
         BNZ   ABEND               ABEND - SKIP NEXT CODE
         LH    R1,0(R1)            LOAD CONDITION CODE
         CVD   R1,DECW             CONVERT TO PACKED DECIMAL
         MVC   CODECHAR-1(6),=X'402020202120' SET UP EDIT MASK
         ED    CODECHAR-1(6),DECW+5 CONVERT TO EBCDIC
         BR    R10                 RETURN TO CALLER
ABEND    TM    0(R1),X'80'         CHECK FOR USER ABEND
         BO    USERABND            USER ABEND - SKIP NEXT CODE
         MVC   DECW+5(2),0(R1)     MOVE SYSTEM ABEND CODE
         MVI   DECW+7,X'0C'        ADD JUNK CHARACTER FOR UNPK
         UNPK  CODECHAR+1(5),DECW+5(3) UNPK SYSTEM ABEND CODE
         TR    CODECHAR+1(5),TRTABLE-C'0' TRANSLATE X'FA' TO C'A'
         MVC   CODECHAR(2),=C'S '  INDICATE SYSTEM ABEND CODE
         BR    R10                 AND RETURN TO CALLER
USERABND ICM   R1,B'0011',0(R1)    INSERT USER ABEND CODE
         SLL   R1,17               SHIFT OUT HIGH ORDER BIT
         SRL   R1,17
         CVD   R1,DECW             CONVERT USER ABEND CODE TO PACKED
         MVC   CODECHAR-1(6),=X'402120202020' SET UP EDIT MASK
         ED    CODECHAR-1(6),DECW+5 CONVERT TO EBCDIC
         MVI   CODECHAR,C'U'       INDICATE USER ABEND CODE
         BR    R10                 AND RETURN TO CALLER
TRTABLE  DC    C'0123456789ABCDEF' TRANSLATE TABLE
         EJECT
***********************************************************************
*                                                                     *
*   CONSTANTS                                                         *
*                                                                     *
***********************************************************************
DASHES   DC    C' ',59C'- ',C' '
SPACES   DC    C' ',118C' ',C' '
HEADER   DC    C' ',30C' ',C'MEMOREX I.S.D.  DATA CENTER (VS2 REL XX.XX*
                CPU MODEL XXXX)',30C' ',C' '                 FSH/101579
STPHEAD1 DC    CL120'   STEP START   STEP STOP    STEP ELAPSE  STEP TCB*
                    STEP SRB       JOBNAME    STEP NO  STEPNAME  PGM NA*
               ME COND CODE  '
STPHEAD2 DC    CL120'   PAGE INS     PAGE OUTS    VIO PAGEINS  VIO PAGE*
               OUT  STEP CPU       SERVICE UNITS  VIRT STOR USED PERF G*
               RP            '
JOBHEAD1 DC    CL120'   JOB  START   JOB  STOP    JOB ELAPSE   JOB TCB *
                    JOB  SRB       JOBNAME   RDRDATE   RDR TIME        *
                  COMP CODE  '
JOBHEAD2 DC    CL120'   ACCOUNTING DATA  PROGRAMMER NAME               *
                    TRANS ACTIVE   SERVICE UNITS                 PERF G*
               RP   STEPS    '
DASHES2  DC    CL35' ',C' ',24C'- ',C' '
SPACES2  DC    CL35' ',C'                                              *
                   '
EXPHEAD1 DC    CL35' ',C'                   EXCP COUNTS                *
                   '
EXPHEAD2 DC    CL35' ',C'   ADR  TYPE    DASD EXCP  TAPE EXCP  MISC EXC*
               P   '
EXPTOTAL DC    CL35' ',C'     TOTALS                                   *
                   '
EXPWARN  DC    CL35' ',C'    *** WARNING : EXCP COUNTS MAY BE WRONG ****
                   '
         EJECT
***********************************************************************
*                                                                     *
*   DASD DEVICE NAME OFFSETS AND TABLE                                *
*                                                                     *
***********************************************************************
DASDOFST DC    AL1(DADASD-DASDTABL)   00
         DC    AL1(DA2311-DASDTABL)   01                        *JLM*
         DC    AL1(DA2301-DASDTABL)   02                        *JLM*
         DC    AL1(DA2303-DASDTABL)   03                        *JLM*
         DC    AL1(DA2302-DASDTABL)   04                        *JLM*
         DC    AL1(DA2321-DASDTABL)   05                        *JLM*
         DC    AL1(DA23051-DASDTABL)  06
         DC    AL1(DA23052-DASDTABL)  07
         DC    AL1(DA2314-DASDTABL)   08
         DC    AL1(DA3330-DASDTABL)   09
         DC    AL1(DA3340-DASDTABL)   0A
         DC    AL1(DA3350-DASDTABL)   0B                        *JLM*
         DC    AL1(DA3375-DASDTABL)   0C                        *JLM*
         DC    AL1(DA33301-DASDTABL)  0D
         DC    AL1(DA3380-DASDTABL)   0E                        *JLM*
         DC    AL1(DA3390-DASDTABL)   0F                        *JLM*
DASDTABL DS    0D
DADASD   DC    CL6'DASD'
DA2301   DC    CL6'2301'                                        *JLM*
DA2302   DC    CL6'2302'                                        *JLM*
DA2303   DC    CL6'2303'                                        *JLM*
DA23051  DC    CL6'2305-1'
DA23052  DC    CL6'2305-2'
DA2311   DC    CL6'2311'                                        *JLM*
DA2314   DC    CL6'2314'
DA2321   DC    CL6'2321'                                        *JLM*
DA3330   DC    CL6'3330'
DA33301  DC    CL6'3330-1'
DA3340   DC    CL6'3340'
DA3350   DC    CL6'3350'                                        *JLM*
DA3375   DC    CL6'3375'                                        *JLM*
DA3380   DC    CL6'3380'                                        *JLM*
DA3390   DC    CL6'3390'                                        *JLM*
         EJECT
***********************************************************************
*                                                                     *
*   TAPE DEVICE NAME OFFSETS AND TABLE                                *
*                                                                     *
***********************************************************************
TAPEOFST DC    AL1(TATAPE-TAPETABL)   00
         DC    AL1(TA2400-TAPETABL)   01
         DC    AL1(TATAPE-TAPETABL)   02
         DC    AL1(TA3400-TAPETABL)   03
TAPETABL DS    0D
TATAPE   DC    CL6'TAPE'
TA2400   DC    CL6'2400'
TA3400   DC    CL6'3400'
         LTORG
         EJECT
***********************************************************************
*                                                                     *
*   SMF5 EQUATES                                                      *
*                                                                     *
***********************************************************************
SMF5TME  EQU   2                   JOB TERMINATION TIME
SMF5DTE  EQU   6                   JOB TERMINATION DATE
SMF5JBN  EQU   14                  JOBNAME
SMF5RST  EQU   22                  JOB READER TIME
SMF5RSD  EQU   26                  JOB READER DATE
SMF5JIT  EQU   39                  JOB INITIATION TIME
SMF5JID  EQU   43                  JOB INITIATION DATE
SMF5JCC  EQU   51                  JOB COMPLETION CODE
SMF5JBTI EQU   62                  JOB TERMINATION INDICATOR
SMF5SRBT EQU   73                  JOB SRB TIME
SMF5TJS  EQU   76                  JOB SERVICE UNITS
SMF5TTAT EQU   80                  JOB TRANSACTION ACTIVE TIME
SMF5PGNO EQU   88                  JOB PERFORMANCE GROUP NUMBER
SMF5PRGN EQU   93                  PROGRAMMER'S NAME
SMF5JCPU EQU   113                 JOB CPU TIME
SMF5ACTF EQU   116                 NUMBER OF ACCOUNTING FIELDS
SMF5JSAF EQU   117                 FIRST ACCOUNTING ENTRY
         EJECT
***********************************************************************
*                                                                     *
*   SMF4 EQUATES                                                      *
*                                                                     *
***********************************************************************
SMF4TME  EQU   2                   STEP TERMINATION TIME
SMF4DTE  EQU   6                   STEP TERMINATION DATE
SMF4JBN  EQU   14                  JOBNAME
SMF4SIT  EQU   39                  STEP INITIATION TIME
SMF4STID EQU   43                  STEP INITIATION DATE
SMF4SCC  EQU   51                  STEP COMPLETION CODE
SMF4PGMN EQU   54                  STEP PROGRAM NAME
SMF4STMN EQU   62                  STEPNAME
SMF4HOST EQU   74                  STEP STORAGE USED IN K
SMF4STI  EQU   83                  STEP TERMINATION INDICATOR
SMF4SRBT EQU   95                  STEP SRB TIME
SMF4RIN  EQU   98                  STEP RECORD INDICATOR
SMF4RLCT EQU   100                 OFFSET TO RELOCATE SECTION
SMF4LENN EQU   102                 LENGTH OF DEVICE PORTION
SMF4PGIN EQU   0                   STEP PAGE-INS (NONVIO)
SMF4PGOT EQU   4                   STEP PAGE-OUTS (NONVIO)
SMF4VPI  EQU   20                  STEP VIO PAGE-INS
SMF4VPO  EQU   24                  STEP VIO PAGE-OUTS
SMF4SST  EQU   28                  STEP SERVICE UNITS
SMF4ACT  EQU   32                  STEP TRANSACTION ACTIVE TIME
SMF4PGNO EQU   36                  STEP PERFORMANCE GROUP NUMBER
          EJECT
***********************************************************************
*                                                                     *
*   WORKAREA DSECT                                                    *
*                                                                     *
***********************************************************************
WORKAREA DSECT
SAVEAREA DS    18F
DECW     DS    D
LINE     DS    CL120
         DS    CL2                 THESE 2 BYTES NEEDED BEFORE TIMECHAR
TIMECHAR DS    CL11
         DS    CL1                 THIS 1 BYTE NEEDED BEFORE CODECHAR
CODECHAR DS    CL5
         DS    CL1                 THIS 1 BYTE NEEDED AFTER CODECHAR
         DS    CL2                 PADDING TO DOUBLEWORD
WORKEND  EQU   *
         END
$$                                                                      00310000
//ASM    EXEC SMPASM,M=IEFACTRT,COND=(0,NE)                             00320000
//RECAPP EXEC SMPAPP,COND=(0,NE)                                        00330000
//SMPPTFIN DD  *                                                        00340000
++USERMOD (ZUM0002).                                                    00350000
++VER (Z038) FMID(EBB1102).                                             00360000
++MOD(IEFACTRT) TXLIB(UMODOBJ).                                         00370000
//SMPCNTL  DD  *                                                        00380000
 REJECT  SELECT(ZUM0002).                                               00390000
 RESETRC.                                                               00400000
 RECEIVE SELECT(ZUM0002).                                               00410000
 APPLY   SELECT(ZUM0002)                                                00420000
         DIS(WRITE)                                                     00430000
         .                                                              00440000
/*                                                                      00450000
//                                                                      00460000
