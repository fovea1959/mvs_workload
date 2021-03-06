//LCBT252  JOB (),'WEGSCD.DYNALIST.ASM',CLASS=A,MSGCLASS=A
//CREATE  EXEC PGM=PDSLOAD
//SYSPRINT  DD SYSOUT=A
//SYSUT2    DD DSNAME=WEGSCD.DYNALIST.ASM,DISP=SHR,
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120,DSORG=PO)
//SYSUT1    DD *
./ ADD NAME=$DOC                                                        02206700
$DOC ..... THIS FILE                                                    02206800
DYNALIST . NON-XA VER OF DYNALIST                                       02206900
DYNAXA ... XA     VER OF DYNALIST                                       02207000
./ ADD NAME=DYNALIST                                                    02207200
*          DATA SET CBT910V01  AT LEVEL 007 AS OF 08/31/80
         MACRO                                                          00001
*                                                                       00002
*     UCB TABLE:                                                        00003
*                                                                       00004
*           I  2  BYTE  I                                               00005
*           I  UCB ADR. I   * #UCBS                                     00006
*           I___________I   --------                                    00007
*                                                                       00008
* DEVNAME TABLE:                                                        00009
*                                                                       00010
*           I  4  BYTE  I                                               00011
*           I  # NAMES  I                                               00012
*           I___________I                                               00013
*                                                                       00014
*           I  8  BYTE  I  4 BYTE   I                                   00015
*           I    NAME   I   MASK    I   * #NAMES                        00016
*           I___________I___________I   ---------                       00017
*                                                                       00018
* DEVMASK TABLE:                                                        00019
*                                                                       00020
*           I  4  BYTE  I                                               00021
*           I # ENTRIES I                                               00022
*           I___________I                                               00023
*                                                                       00024
*           I  4  BYTE  I  4 BYTE   I  BITS * #UCBS  I                  00025
*           I  # MATCH  I   MASK    I  MATCHING UCB  I  * #ENTRIES      00026
*           I___________I___________I________________I  ----------      00027
*                                                                       00028
&NAME    CLEAR &AREA                                                    00029
         LCLC  &L                                                       00030
&L       SETC  'L'''                                                    00031
&NAME    DS    0H                                                       00032
         MVI   &AREA,C' '                                               00033
         MVC   &AREA+1(&L&AREA-1),&AREA                                 00034
         MEND                                                           00035
         EJECT                                                          00036
MASKLIST CSECT                                                          00037
R0       EQU   0                                                        00038
R1       EQU   1                                                        00039
R2       EQU   2                                                        00040
R3       EQU   3                                                        00041
R4       EQU   4                                                        00042
R5       EQU   5                                                        00043
R6       EQU   6                                                        00044
R7       EQU   7                                                        00045
R8       EQU   8                                                        00046
R9       EQU   9                                                        00047
R10      EQU   10                                                       00048
R11      EQU   11                                                       00049
R12      EQU   12                                                       00050
R13      EQU   13                                                       00051
R14      EQU   14                                                       00052
R15      EQU   15                                                       00053
         EJECT                                                          00054
         SAVE  (14,12)                                                  00055
         LR    R12,R15                                                  00056
         USING MASKLIST,R12                                             00057
         CNOP  0,4                                                      00058
         BAL   R15,START                                                00059
         DC    18F'0'                                                   00060
START    ST    R13,4(,R15)                                              00061
         ST    R15,8(,R13)                                              00062
         LR    R13,R15                                                  00063
         SPACE 2                                                        00064
         L     R1,16                                                    00065
         L     R8,40(R1)                                                00066
         LR    R1,R8                                                    00067
UCBSCAN  ICM   R0,3,0(R1)                                               00068
         CLI   0(R1),X'FF'                                              00069
         LA    R1,2(,R1)                                                00070
         BNE   UCBSCAN                                                  00071
         SR    R1,R8                    NUMBER OF UCB SLOTS+1           00072
         SRL   R1,1                                                     00073
         BCTR  R1,0                                                     00074
         LA    R0,31                                                    00075
         AR    R1,R0                    ROUND COUNT TO WORD BOUNDARY    00076
         OR    R1,R0                                                    00077
         XR    R1,R0                                                    00078
         SRL   R1,3                     TOTAL WORD COUNT                00079
         LR    R9,R1                    SAVE COUNT                      00080
         EJECT                                                          00081
LOAD     DS    0H                                                       00082
         LOAD  EP=DEVNAMET                                              00083
         LR    R10,R0                                                   00084
         LOAD  EP=DEVMASKT                                              00085
         LR    R11,R0                                                   00086
         EJECT                                                          00087
         LA    R1,NAMETAB                                               00088
         L     R0,0(,R10)                                               00089
         LA    R15,4(,R10)                                              00090
TABLSCAN MVC   0(8,R1),0(R15)                                           00091
         MVI   8(R1),C' '                                               00092
         LA    R1,9(R1)           INCREMENT OUTPUT STAK                 00093
         LA    R15,12(,R15)       INCREMENT NAMET POINTER               00094
         BCT   R0,TABLSCAN                                              00095
         EJECT                                                          00096
         OPEN  (SYSPRINT,OUTPUT,SYSIN,INPUT)                            00097
         EJECT                                                          00098
REPEAT   EQU   *                           AXC                          00099
         MVI   LINE-1,C'1'        DISPLAY                               00100
         PUT   SYSPRINT,LINE-1           ENTIRE                         00101
         LA    R2,NAMETAB                      NAME                     00102
         MVI   LINE-1,C' '                         TABLE                00103
NAMESOUT EQU   *                                                        00104
         MVC   LINE(72),0(R2)     EIGHT                                 00105
         PUT   SYSPRINT,LINE-1         NAMES                            00106
*        TPUT  LINE,72                      ACROSS                      00107
         LA    R2,72(R2)                                                00108
         CLI   0(R2),C' '                                               00109
         BNE   NAMESOUT                                                 00110
GET      MVI   LINE-1,C'1'                                              00111
GET0     TPUT  MSGENTER,L'MSGENTER                                      00112
GET1     GET   SYSIN                                                    00113
         LR    R2,R1                                                    00114
         CLI   0(R1),C'='                THE ABILITY TO REPEAT AXC      00115
         BE    REPEAT                    THE ABILITY TO REPEAT AXC      00116
         CLI   0(R1),C' '                                               00117
         BNE   GET2                                                     00118
         CLC   1(71,R1),0(R1)                                           00119
         BE    GET1                                                     00120
GET2     DS    0H                                                       00121
         CLEAR LINE                                                     00122
         MVC   LINE(80),0(R1)                                           00123
         PUT   SYSPRINT,LINE-1                                          00124
         MVI   LINE-1,C' '                                              00125
         CLI   0(R2),C'*'                                               00126
         BE    GET1                                                     00127
         LA    R14,1                                                    00128
         LR    R1,R2                                                    00129
         LA    R15,71(R2)                                               00130
FIND     CLI   0(R1),C' '                                               00131
         BNE   FIND1                                                    00132
         BXLE  R1,R14,FIND                                              00133
         B     GET1                                                     00134
FIND1    LR    R2,R1                                                    00135
FIND2    CLI   0(R1),C' '                                               00136
         BE    FIND3                                                    00137
         BXLE  R1,R14,FIND2                                             00138
FIND3    SR    R1,R2                                                    00139
         CLEAR UNITNAME                                                 00140
         BCTR  R1,0                                                     00141
         MVC   UNITNAME(0),0(R2)                                        00142
         EX    R1,*-6                                                   00143
         CLC   END,UNITNAME                                             00144
         BE    EODAD                                                    00145
         L     R0,0(,R10)                                               00146
         LA    R15,4(,R10)                                              00147
NAMESCAN CLC   UNITNAME,0(R15)                                          00148
         BE    HAVENAME                                                 00149
         LA    R15,12(,R15)                                             00150
         BCT   R0,NAMESCAN                                              00151
         PUT   SYSPRINT,=CL121' ******* INVALID UNIT GROUP'             00152
         B     GET                                                      00153
HAVENAME LH    R0,0(,R11)               GET NUMBER OF MASK ENTRIES      00154
         LA    R1,4(,R11)                                               00155
FINDMASK CLC   8(4,R15),4(R1)                                           00156
         BE    HAVEMASK                                                 00157
         LA    R1,8(R1,R9)                                              00158
         BCT   R0,FINDMASK                                              00159
         PUT   SYSPRINT,=CL121' ******* NO MASK ENTRY FOUND'            00160
         B     GET                                                      00161
HAVEMASK DS    0H                                                       00162
         MVI   LINE-1,C'-'                                              00163
         LA    R2,7(R1,R9)              END OF MASK ENTRY               00164
         L     R3,0(,R1)                NUMBER OF BITS THIS ENTRY       00165
         LA    R4,8(,R1)                                                00166
         SR    R7,R7                                                    00167
NEXTBYTE LA    R5,8                                                     00168
         LA    R6,X'80'                                                 00169
NEXTBIT  TM    0(R4),0                                                  00170
         EX    R6,NEXTBIT                                               00171
         BZ    NOTTHIS                                                  00172
         CLEAR LINE                                                     00173
         LA    R14,0(R7,R8)             DOUBLE INDEX                    00174
*                                       TO LOOKUP ENTRY                 00175
         SR    R15,R15                                                  00176
         ICM   R15,3,0(R14)                                             00177
*        LH    R15,0(,R15)                                              00178
         MVC   LINE+4(3),13(R15)                                        00179
         TM    18(R15),X'20'            DIRECT ACCESS?                  00180
         BZ    NOTDA                    NO                              00181
         TM    3(R15),X'80'             ONLINE?                         00182
         BZ    NOTONL                                                   00183
         CLI   28(R15),X'00'            YES, VOLID KNOWN?               00184
         BE    NOTDA                    NO                              00185
         MVI   LINE+8,C'-'                                              00186
         MVC   LINE+10(6),28(R15)                                       00187
         B     NOTDA                                                    00188
NOTONL   EQU   *                                                        00189
         MVI   LINE+8,C'-'                                              00190
         MVC   LINE+10(7),=CL7'OFFLINE'                                 00191
         SPACE 2                                                        00192
NOTDA    DS    0H                                                       00193
         PUT   SYSPRINT,LINE-1                                          00194
         BCT   R3,NOTTHIS                                               00195
         B     GET                                                      00196
NOTTHIS  DS    0H                                                       00197
         LA    R7,2(,R7)                                                00198
         SPACE 2                                                        00199
         SRL   R6,1                                                     00200
         BCT   R5,NEXTBIT                                               00201
         LA    R4,1(,R4)                                                00202
         CR    R4,R2                    MASK BIT COUNT ERROR?           00203
         BH    TOOMANY                  YES                             00204
         B     NEXTBYTE                                                 00205
TOOMANY  PUT   SYSPRINT,=CL121'0TOO MANY BITS IN MASK COUNT'            00206
         B     GET                                                      00207
EODAD    DS    0H                                                       00208
         CLOSE (SYSIN,,SYSPRINT)                                        00209
         L     R13,4(,R13)                                              00210
         RETURN (14,12),T,RC=0                                          00211
         EJECT                                                          00212
SYSPRINT DCB   DDNAME=SYSPRINT,LRECL=81,BLKSIZE=81,RECFM=FBA,          X00213
               DSORG=PS,MACRF=PM                                        00214
SYSIN    DCB   DDNAME=SYSIN,LRECL=80,RECFM=FB,MACRF=GL,                X00215
               EODAD=EODAD,DSORG=PS                                     00216
         LTORG                                                          00217
UNITNAME DC    CL8' '                                                   00218
         DC    C' '                                                     00219
LINE     DC    CL80' '                                                  00220
MSGENTER DC C'ENTER GROUP NAME OR ''='' TO REPEAT OR ''END'' TO END'    00221
END      DC    C'END'                                                   00222
NAMETAB  DS    0F                                                       00223
         DC    200CL9' '                                                00224
         END                                                            00225
./ ADD NAME=DYNAXA                                                      02207500
*          DATA SET CBT910     AT LEVEL 001 AS OF 01/30/84
*********************************************************************** 00001
*********************************************************************** 00002
*******             C. B. T.         1/84               LMB-CBT ******* 00003
*******                                                         ******* 00004
*******                                                         ******* 00005
******* ** THIS MODULE MUST BE LINK EDITED AS RE-ENT, OR    CBT ******* 00006
*******    IT ABENDS WHEN 'END' IS ENTERED DURING FREEMAIN  CBT ******* 00007
*******    OF SUBPOOL 230 (KEY 1 STORAGE FROM IEFAB4UV).    CBT ******* 00008
*******                                                     CBT ******* 00009
******* UPDATED WITH UCB CODING CONVENTIONS FOR X-A         CBT ******* 00010
******* THIS CODE SHOULD RUN UNDER MVS SP AND X-A           CBT ******* 00011
*******                                                     CBT ******* 00012
******* SVC 254 IS USED (CBT'S AUTHORIZATION SVC).          CBT ******* 00013
******* THIS MODULE HAS BEEN CONVERTED TO PUTLINE/GETLINE.  CBT ******* 00014
*******                                                     CBT ******* 00015
******* UNDER X-A AN OC4 WILL OCCUR IN IEFAB4UV/IEFGB4UV    CBT ******* 00016
******* UNLESS THE FOLLOWING HIPER IS INSTALLED:            CBT ******* 00017
******* APAR (OZ74838)    PTF (UZ68681)                     CBT ******* 00018
*******                                                     CBT ******* 00019
*********************************************************************** 00020
CBT910   CSECT                                                          00021
CBT910   AMODE 31     FOR DEFAULT - IF SP3 WILL CHG TO 24               00022
CBT910   RMODE ANY                                                      00023
R0       EQU   0                                                        00024
R1       EQU   1                                                        00025
R2       EQU   2                                                        00026
R3       EQU   3                                                        00027
R4       EQU   4                                                        00028
R5       EQU   5                                                        00029
R6       EQU   6                                                        00030
R7       EQU   7                                                        00031
R8       EQU   8                                                        00032
R9       EQU   9                                                        00033
R10      EQU   10                                                       00034
R11      EQU   11                                                       00035
R12      EQU   12                                                       00036
R13      EQU   13                                                       00037
R14      EQU   14                                                       00038
R15      EQU   15                                                       00039
EDTLUVSP EQU   X'000'                                                   00040
LUVENTNO EQU   X'000'                                                   00041
LUVENTLN EQU   X'004'                                                   00042
LUVENTRY EQU   X'008'                                                   00043
LUVUNAME EQU   X'000'                                                   00044
LUVVALUE EQU   X'008'                                                   00045
         EJECT                                                          00046
         USING *,R15                                                    00047
         BC    15,PROLOG                                                00048
         DC    CL8'CBT910  '       MODULE NAME                          00049
         DC    CL8'&SYSDATE'      DATE MODULE COMPILED                  00050
         DC    CL8'&SYSTIME'      TIME MODULE COMPILED                  00051
         DROP  15                                                       00052
PROLOG   EQU   *                                                        00053
         STM   R14,R12,12(R13)    SAVE REGS                             00054
         LR    R11,R15            SET UP BASE REG                       00055
         USING CBT910,R11                                               00056
         L     R8,CVTPTR          CVT ADDRESS                           00057
         USING CVTMAP,R8          ADDRESS TO DSECT                      00058
**********************************************************************  00059
* THE FOLLOWING CODE WAS USED TO TEST IF THIS IS RUNNING ON SP3 OR XA.  00060
* IT THEN EXECUTED THE PROPER BSM INSTRUCTION TO CHANGE TO 24 OR 31 BIT 00061
* ADDRESSING MODE.  UNFORTUNATELY THE BSM INSTRUCTION ONLY WORKS        00062
* UNDER XA. THEREFORE, THE MODES WERE HARDCODED (RMODE=ANY,AMODE=31),   00063
* WHICH WORKS FINE (SP3 IGNORES IT COMPLETELY).                         00064
*********************************************************************   00065
*        TM    CVTDCB,CVTMVSE     IF =1 IT IS X-A                       00066
*        BC    8,SP1              IT IS 370 SP1.3                       00067
*XA      L     R1,LABEL1          SET HIGH-ORDER BIT (0) OF R1 TO 1     00068
*                                 + PUT ADDRESS INTO BITS 1-31          00069
*        BSM   0,R1               SET AMODE=31 (DOESN'T PRESERVE AMODE) 00070
*LABEL1   DC    A(LABEL2+X'80000000')                                   00071
*LABEL2   DS    0H                                                      00072
*         BC    15,ALLSYS          CONTINUE FOR ALL SYSTEMS             00073
*SP1      LA    R1,LABEL3          SET HIGH-ORDER BIT (0) OF R1 TO 0    00074
*                                 + PUT ADDR INTO BITS 1-31             00075
*        BSM   0,R1               SET AMODE=24 (DOESN'T PRESERVE AMODE) 00076
*LABEL3   DS    0H                                                      00077
ALLSYS   GETMAIN R,LV=LDYNAMIC    LEN OF DYNAMIC AREA                   00078
         LR    R10,R1                                                   00079
         ST    R13,4(R10)         SAVE HSA IN LSA                       00080
         ST    R10,8(R13)         SAVE LSA IN HSA                       00081
         L     R1,24(R13)         RESTORE REG ONE                       00082
         LR    R13,R10            SET UP NEW SAVE AREA                  00083
         USING DYNMAREA,R13                                             00084
         ST    R1,CPPLADDR                                              00085
         MVI   MSGWORK,X'40'      CLEAR MSG BUFFER                      00086
         MVC   MSGWORK+1(120),MSGWORK                                   00087
         MVI   NAMETAB,X'40'      CLEAR NAME TABLE TO BLANKS            00088
         MVC   NAMETAB+1(255),NAMETAB                                   00089
         MVC   NAMETAB+256(255),NAMETAB                                 00090
         MVC   NAMETAB+511(255),NAMETAB                                 00091
         MVC   NAMETAB+766(255),NAMETAB                                 00092
         MVC   NAMETAB+1021(255),NAMETAB                                00093
         MVC   NAMETAB+1276(255),NAMETAB                                00094
         MVC   NAMETAB+1531(255),NAMETAB                                00095
         MVC   NAMETAB+1786(14),NAMETAB                                 00096
         SPACE 2                                                        00097
LOAD     L     R1,CVTJESCT        JES CONTROL TABLE                     00098
         DROP  R8                                                       00099
         USING JESCT,R1           ADDRESS TO DSECT                      00100
         L     R1,JESEDT          EDT                                   00101
         L     R10,EDTLUVSP(,R1)  LOOK-UP SECTION                       00102
         L     R0,LUVENTNO(,R10)  # OF UNIT NAMES                       00103
         L     R8,LUVENTLN(,R10)  LENGTH OF ENTRY                       00104
         LA    R15,LUVENTRY(,R10)      FIRST ENTRY                      00105
         LA    R1,NAMETAB                                               00106
TABLSCAN MVC   0(8,R1),0(R15)                                           00107
         MVI   8(R1),C' '                                               00108
         LA    R1,9(R1)           INCREMENT OUTPUT STAK                 00109
         LA    R15,0(R8,R15)      INCREMENT NAMET POINTER               00110
         BCT   R0,TABLSCAN                                              00111
         EJECT                                                          00112
REPEAT   EQU   *                           AXC                          00113
         LA    R2,NAMETAB                      NAME                     00114
NAMESOUT EQU   *                                                        00115
         MVC   MSGOUT(72),0(R2)   MOVE OUT 8 NAMES                      00116
         BAL   R4,PUTLINE         GO PRINT IT OUT                       00117
         LA    R2,72(R2)                                                00118
         CLI   0(R2),C' '                                               00119
         BNE   NAMESOUT                                                 00120
GET      MVC   MSGOUT(L'MSGENTER),MSGENTER                              00121
         BAL   R4,PUTLINE                                               00122
GET1     BAL   R4,GETLINE                                               00123
         MVC   UNITNAME(8),=CL8' '                                      00124
         L     R1,INPUTADR                                              00125
         LH    R7,0(R1)                                                 00126
         CLC   0(2,R1),=X'0004'   A NULL RETURN?                        00127
         BC    12,GET             NO REPLY GO ASK AGAIN                 00128
         CLI   4(R1),C'='                THE ABILITY TO REPEAT AXC      00129
         BE    REPEAT                    THE ABILITY TO REPEAT AXC      00130
GET2     DS    0H                                                       00131
         MVC   MSGOUT(80),4(R1)                                         00132
         BAL   R4,PUTLINE                                               00133
         CLI   4(R2),C'*'                                               00134
         BE    GET1                                                     00135
         LA    R1,4(R1)       POINT TO INPUT DATA                       00136
         LR    R2,R1                                                    00137
         BC    15,COMPRIT                                               00138
         MVC   UNITNAME(0),0(R2)                                        00139
COMPRIT  EX    R7,*-6                                                   00140
         TR    UNITNAME(8),TRTABLE                                      00141
         CLC   END,UNITNAME                                             00142
         BE    EODAD                                                    00143
*********************************************************************** 00144
*********************************************************************** 00145
* CHECK THAT THE CLASS NAME ENTERED IS VALID. THIS IS DONE              00146
* BY COMPARING THE NAME (GENERIC OR ESOTERIC) AGAINST THE               00147
* CLASS NAMES IN THE EDT LOOK UP SECTION.                               00148
* A DSECT FOR EDT SHOULD BE SUBSTITUTED WHEN AVAILABLE.                 00149
*********************************************************************** 00150
*********************************************************************** 00151
         L     R0,LUVENTNO(,R10)   # CLASS TYPES IN EDT                 00152
         LA    R15,LUVENTRY(,R10)   1ST ENTRY                           00153
NAMESCAN CLC   UNITNAME,0(R15)                                          00154
         BE    HAVENAME                                                 00155
         LA    R15,0(R8,R15)        GET NEXT ENTRY                      00156
         BCT   R0,NAMESCAN                                              00157
         MVC   MSGOUT(8),UNITNAME                                       00158
         BAL   R4,PUTLINE                                               00159
         MVC   MSGOUT(27),=CL27' ******* INVALID CLASS NAME'            00160
         BAL   R4,PUTLINE                                               00161
         B     GET                                                      00162
*********************************************************************** 00163
* THE DEVICE NAME ENTERED IS VALID (IT WAS FOUND AGAIN IN               00164
* THE EDT LOOKUP SECTION.  MOVE UNITNAME TO KEY 1 STORAGE               00165
* AND INVOKE IEFAB4UV TO RETURN ASSOCIATED UCB ADDRESSES.               00166
* R0, R1, R2, R8, R7, R14 CAN BE REUSED NOW.                            00167
*********************************************************************** 00168
HAVENAME SR   R1,R1               INITIALIZE IT                         00169
         L    R2,CVTPTR           GET CVT POINTER                       00170
         L    R2,0(R2)            OLD-NEW POINTER                       00171
         L    R2,4(R2)            OUT TCB POINTER                       00172
         LR   R6,R2                                                     00173
         ST   R6,TCBHOLD          HOLD ADDR OF TCB                      00174
         USING TCB,R6                                                   00175
**********YOU MUST BE SUPERVISOR MODE TO GETMAIN KEY1 SP:               00176
         SR   R1,R1                                                     00177
         SVC  254                                                       00178
         L    R9,SPNO236               SUBPOOL 236 (KEY 1)              00179
         MODESET MODE=SUP                                               00180
         GETMAIN R,LV=LKEY1SP,SP=(R9)  FOR KEY SUBPOOL STORAGE          00181
**********MUST BE KEY0 (OR KEY1) TO MOVE INTO SP236 (WRITE PROTECTED):  00182
         MODESET EXTKEY=ZERO                                            00183
         ST   R1,KEY1ADDR         SAVE KEY1 GETMAIN'D AREA PTR          00184
         LR   R7,R1              PTR TO KEY1 STORAGE                    00185
         USING KEY1SP,R7         ADDR TO DSECT                          00186
*********************************************************************** 00187
* SET UP PARAMETER LIST FOR KEY 1 STORAGE FOR IEFAB4UV                  00188
*********************************************************************** 00189
         LA   R1,UNITABLE                                               00190
         ST   R1,UTBLPTR                                                00191
         LA   R1,FLAGS                                                  00192
         ST   R1,FLAGPTR                                                00193
         LA   R1,ATTRAREA                                               00194
         ST   R1,ATTRPTR                                                00195
         MVC  UNAME(8),UNITNAME                                         00196
         MVC  FLAGS,FLAGSET                                             00197
******KEY0 NO LONGER NEEDED, ALL INFO ALREADY MOVED INTO SP236 STORAGE: 00198
         MODESET EXTKEY=TCB,WORKREG=9   R6 STILL ADDRESSED TO TCB DSECT 00199
         LA   R1,4                                                      00200
         SVC  254                                                       00201
         LA   R1,PARMLIST                                               00202
         LINK EP=IEFAB4UV                                               00203
*                                                                       00204
         LTR  R15,R15             HAVE UCBS BEEN RETURNED?              00205
         BC   8,GOTUCBS           UCBS RETURNED GO LOOK AT THEN         00206
         CH   R15,=H'4'           R15=4                                 00207
         BC   8,NONAME            SAY CLASS NAME NOT FOUND              00208
         CH   R15,=H'16'          R16=16                                00209
         BC   8,NOSTORGE          NO STORAGE AVAIL FOR UCB LIST         00210
         BC   15,ABENDIT          ABEND ALL OTHER RETURN CODES          00211
*********************************************************************** 00212
* UCBS RETURNED IN SUBPOOL 230 STORAGE. KEY 1 STORAGE (SUBPOOL 236)     00213
* CAN NOW BE FREED,  THEN PROCESS THE UCB LIST AND FREE                 00214
* SUBPOOL 230 STORAGE PRIOR TO EOJ.                                     00215
*********************************************************************** 00216
GOTUCBS  L     R7,UTBLPTR        THE ADDR OF THE UNIT TABLE             00217
         DROP  R7                FROM PARAMETER LIST DSECT              00218
         USING UNITABLE,R7       ADDRESS TO THE UNIT TABLE              00219
         MVC  UCBLIST,UCBPTR     SAVE PTR TO UCB LIST                   00220
         L     R9,SPNO236                                               00221
         L     R1,KEY1ADDR                                              00222
         FREEMAIN R,LV=LKEY1SP,SP=(R9),A=(R1)                           00223
         L     R9,UCBLIST         PTR TO UCB LIST                       00224
         CLI   0(R9),X'E6'        IS THIS THE UCB LIST?                 00225
         BC    7,NOTUCBLS         NOT UCB LIST SEND MSG                 00226
         MVC   LISTLEN,0(R9)      HOLD LEN OF LIST                      00227
         DROP  R7                 FROM KEY1 PARM DSECT                  00228
         L     R7,4(R9)           NUMBER OF UCBS IN LIST                00229
         LA    R9,8(R9)           ADDRESS OF 1ST UCB PTR                00230
         L     R15,0(R9)          FIRST UCB PTR                         00231
GETUCB   EQU   *                                                        00232
         USING UCBOB,R15                                                00233
         MVC   MSGOUT+4(3),UCBNAME                                      00234
         TM    UCBDVCLS,UCB3DACC        DIRECT ACCESS?                  00235
         BZ    NOTDA                    NO                              00236
         TM    UCBSTAT,UCBONLI          ONLINE?                         00237
         BZ    NOTONL                                                   00238
         CLI   UCBVOLI,X'00'            YES, VOLID KNOWN?               00239
         BE    NOTDA                    NO                              00240
         MVI   MSGOUT+8,C'-'                                            00241
         MVC   MSGOUT+10(6),UCBVOLI                                     00242
         B     NOTDA                                                    00243
NOTONL   EQU   *                                                        00244
         MVI   MSGOUT+8,C'-'                                            00245
         MVC   MSGOUT+10(7),=CL7'OFFLINE'                               00246
         SPACE 2                                                        00247
NOTDA    DS    0H                                                       00248
         BAL   R4,PUTLINE                                               00249
         BCT   R7,NOTTHIS                                               00250
         B     GET                                                      00251
NOTTHIS  DS    0H                                                       00252
         LA    R9,4(,R9)                                                00253
         L     R15,0(R9)                                                00254
         BC    15,GETUCB                                                00255
GETLINE  STM   R0,R15,SAVEREGS    SAVE CALLER'S REGS                    00256
         L     R2,CPPLADDR        ADDR OF CPPL                          00257
         USING CPPL,R2                                                  00258
         L     R3,CPPLUPT         UPT ADDR                              00259
         L     R4,CPPLECT         ECT ADDR                              00260
         XC    ECB,ECB            CLEAR OUT ECB                         00261
         XC    GTPBX(8),GTPBX     CLEAR GET PARMLIST TO ZEROS           00262
         XC    IOPLX(16),IOPLX    CLEAR PARM LIST TO ZEROS              00263
         LA    R1,IOPLX           ADDR OF LIST                          00264
         GETLINE PARM=GTPBX,UPT=(R3),ECT=(R4),ECB=ECB,                 X00265
               TERMGET=(EDIT,WAIT),INPUT=(ISTACK,LOGICAL),             X00266
               MF=(E,(1))                                               00267
         LA    R5,GTPBX           ADDRESSABILITY FOR THE GTPB           00268
         USING GTPB,R5                                                  00269
         L     R6,GTPBIBUF        ADDR OF INPUT LINE                    00270
         ST    R6,INPUTADR        SAVE ADDR OF INPUT BUFFER             00271
         LM    R0,R15,SAVEREGS    RESTORE CALLER'S REGS                 00272
         BR    R4                                                       00273
         EJECT                                                          00274
PUTLINE  MVC   MSGCNTL,PUTCNTL    MOVE IN PUTLINE CONTROL CHARS         00275
         STM   R0,R15,SAVEREGS    SAVE CALLER'S REGS                    00276
         LA    R5,MSGWORK         ADDR OF MSG BUFFER                    00277
         L     R2,CPPLADDR        ADDR OF CPPL                          00278
         USING CPPL,R2                                                  00279
         LA    R1,PUTLPARM        ADDR OF PLIST                         00280
         USING IOPL,R1                                                  00281
         MVC   IOPLECT,CPPLECT    MOVE ADDR OF ECT TO PLIST             00282
         MVC   IOPLUPT,CPPLUPT    MOVE ADDR OF UPT TO PLIST             00283
         LA    R0,ECB             ADDR OF ECB                           00284
         ST    R0,IOPLECB         STORE INTO PLIST                      00285
         XC    ECB,ECB            CLEAR ECB TO ZEROS                    00286
         XC    PTPB(16),PTPB      CLEAR PLIST TO ZEROS                  00287
         PUTLINE PARM=PTPB,ECB=ECB,                                    X00288
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),                     X00289
               OUTPUT=((R5),TERM,SINGLE,DATA),                         X00290
               MF=(E,(1))                                               00291
         MVI   MSGWORK,X'40'      CLEAR BUFFER TO SPACES                00292
         MVC   MSGWORK+1(120),MSGWORK                                   00293
         LM    R0,R15,SAVEREGS    RESTORE CALLER'S REGS                 00294
         BR    R4                 RETURN TO CALLER                      00295
NONAME   MVC   MSGOUT(27),=CL27'  CLASS NAME NOT FOUND - RETRY'         00296
         BAL   R4,PUTLINE                                               00297
         BC    15,GET                                                   00298
NOSTORGE MVC   MSGOUT(27),=CL27'  NO STORAGE AVAIL FOR UCB LIST'        00299
         BAL   R4,PUTLINE                                               00300
         BC    15,EODAD                                                 00301
NOTUCBLS MVC   MSGOUT(27),=CL27'  UCB LIST NOT BEING SEARCHED'          00302
         BAL   R4,PUTLINE                                               00303
         BC    15,EODAD                                                 00304
ABENDIT  MVC   MSGOUT(10),=CL10'  ABEND IT'                             00305
         BAL   R4,PUTLINE                                               00306
         BC    15,EODAD                                                 00307
EODAD    L     R6,TCBHOLD                                               00308
         USING TCB,R6                                                   00309
         SR    R1,R1              PREPARE FOR AUTHORIZATION             00310
         SVC   254                TO PREPARE FOR KEY=1                  00311
         MODESET MODE=SUP         TO PREPARE FOR KEY=1                  00312
         L     R0,LISTLEN         SUBPOOL 230 + LEN OF STORAGE          00313
         L     R1,UCBLIST         PTR TO GETMAIN'D AREA                 00314
         LTR   R1,R1              IS A FREEMAIN REQUIRED                00315
         BC    8,EODADB           NO BRANCH AROUND IT                   00316
         MODESET EXTKEY=SCHED                                           00317
         SVC   10                 FREEMAIN OF SUBPOOL 230               00318
EODADB   MODESET KEY=ZERO                                               00319
         MODESET EXTKEY=TCB,WORKREG=9                                   00320
         MODESET MODE=PROB                                              00321
         LA    R1,4                                                     00322
         SVC   254                                                      00323
         LR    R1,R13             DYNAMIC AREA ADDR                     00324
         LA    R0,LDYNAMIC        SIZE OF AREA                          00325
         L     R13,4(,R13)        PREV SAVE AREA                        00326
         LA    R1,0(,R1)          CLEAR HI ORDER                        00327
         SVC   10                 FREEMAIN                              00328
         LM    R14,R12,12(R13)    RESTORE REGS                          00329
         LA    R15,0              RETURN CODE                           00330
         MVI   12(R13),X'FF'      INDICATE RETURN                       00331
         BR    R14                GO BACK TO TMP                        00332
         EJECT                                                          00333
         LTORG                                                          00334
ALLBLANK DS    200CL9' '                                                00335
         DS    0F                                                       00336
SPNO236  DC    X'000000EC'        SUBPOOL 236                           00337
SPNO230  DC    X'000000E6'        SUBPOOL 230                           00338
FLAGSET  DC    XL2'1000'          BIT 3 SET FOR UCB SEARCH              00339
PUTCNTL  DC    H'121'             CONTROL CHARS FOR PUTLINE             00340
         DC    H'0'                                                     00341
MSGENTER DC C'ENTER GROUP NAME OR ''='' TO REPEAT OR ''END'' TO END'    00342
END      DC    C'END'                                                   00343
         DS    0D        TO DEFINE WHOLE FF BYTES OF A TABLE            00344
TRTABLE  DC    XL256'00'                                                00345
         ORG   TRTABLE+X'40'                                            00346
         DC    X'40'                    BLANK                           00347
         ORG   TRTABLE+X'60'                                            00348
         DC    X'60'                    - (DASH)                        00349
         ORG   TRTABLE+X'81'                                            00350
         DC    X'C1C2C3C4C5C6C7C8C9'    ABCDEFGHI                       00351
         ORG   TRTABLE+X'91'                                            00352
         DC    X'D1D2D3D4D5D6D7D8D9'    JKLMNOPQR                       00353
         ORG   TRTABLE+X'A2'                                            00354
         DC    X'E2E3E4E5E6E7E8E9'      STUVWXYZ                        00355
         ORG   TRTABLE+X'C1'                                            00356
         DC    X'C1C2C3C4C5C6C7C8C9'    ABCDEFGHI                       00357
         ORG   TRTABLE+X'D1'                                            00358
         DC    X'D1D2D3D4D5D6D7D8D9'    JKLMNOPQR                       00359
         ORG   TRTABLE+X'E2'                                            00360
         DC    X'E2E3E4E5E6E7E8E9'      STUVWXYZ                        00361
         ORG   TRTABLE+X'F0'                                            00362
         DC    X'F0F1F2F3F4F5F6F7F8F9'  123456789                       00363
         ORG   TRTABLE+X'100'                                           00364
GETLIST  GETLINE INPUT=(TERM,PHYSICAL),                                X00365
               TERMGET=(EDIT,WAIT),MF=L                                 00366
DYNMAREA DSECT                                                          00367
SAVEAREA DS    18F                      SAVE AREA                       00368
SAVEREGS DS    16F                      FOR PUTLINE                     00369
CPPLADDR DS    F                                                        00370
******THE FOLLOWING ARE FOR PUTLINE:                                    00371
MSGWORK  DS    0CL121                                                   00372
MSGCNTL  DS    F                  CONTROL WORD FOR PUTLINE              00373
MSGOUT   DS    CL120              FOR PUTLINE                           00374
PUTLPARM DS    10A                                                      00375
ECB      DS    F                                                        00376
PTPB     DS    4F                                                       00377
******THE FOLLOWING ARE FOR GETLINE:                                    00378
GTPBX    DS    2F                                                       00379
IOPLX    DS    4F                 I-O PARM LIST FOR GETLINE             00380
INPUTADR DS    F                  ADDR FOR INPUT BFFER                  00381
INPUTCTL DS    F                  CNTL BYTES OF INPUT BUFFER            00382
UNITNAME DC    CL8' '                                                   00383
         DC    C' '                                                     00384
NAMETAB  DS    0F                                                       00385
         DC    200CL9' '                                                00386
         DS    0F                                                       00387
TCBHOLD  DS    F                                                        00388
KEY1ADDR DS    F                                                        00389
LISTLEN  DS    F                                                        00390
UCBLIST  DS    F                   PTR TO UCB LIST                      00391
         DS    0D                                                       00392
LDYNAMIC EQU   *-DYNMAREA          SIZE OF DYNAM AREA                   00393
*********************************************************************** 00394
* DSECT FOR KEY1 SUBPOOL GETMAIN'D STORAGE                              00395
*********************************************************************** 00396
KEY1SP   DSECT                     GETMAIN DSECT FOR KEY1 SUBPOOL       00397
PARMLIST DS    2F                  PARMLIST MAPPING                     00398
         ORG   PARMLIST                                                 00399
UTBLPTR  DS    F                   ADDRESS OF 100-BYTE WORK AREA        00400
FLAGPTR  DS    F                   ADDR OF DEVTYPE BYTE TO BE SEARCHED  00401
         DS    0F                                                       00402
UNITABLE DS    CL20                                                     00403
         ORG   UNITABLE                                                 00404
UNAME    DS    8C                                                       00405
UCBPTR   DS    F                                                        00406
ATTRPTR  DS    F                                                        00407
         DS    F                                                        00408
*                                                                       00409
FLAGS    DS    XL2                                                      00410
ATTRAREA DS    CL10                                                     00411
LKEY1SP  EQU   *-KEY1SP                                                 00412
*********************************************************************** 00413
*        DSECTS                                                         00414
*********************************************************************** 00415
         EJECT                                                          00416
         IEFUCBOB                                                       00417
         EJECT                                                          00418
         IEFJESCT                                                       00419
         EJECT                                                          00420
         CVT   DSECT=YES                                                00421
         EJECT                                                          00422
         IKJTCB                                                         00423
         EJECT                                                          00424
         IKJCPPL                                                        00425
         EJECT                                                          00426
         IKJPPL                                                         00427
         EJECT                                                          00428
         IKJIOPL                                                        00429
         EJECT                                                          00430
         IKJGTPB                                                        00431
         END   CBT910                                                   00432
/*
//
