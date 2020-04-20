//DYNALIST JOB (001),'BUILD DYNALIST',
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=1024K
//HELLO  EXEC ASMFCL                                                    00030000
//ASM.SYSPUNCH DD DUMMY
//ASM.SYSIN DD *
*          DATA SET CBT910V01  AT LEVEL 007 AS OF 08/31/80
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
         MACRO                                                          00001
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
/*
//LKED.SYSLMOD DD DSN=WEGSCD.LOADLIB,DISP=SHR
//LKED.SYSUT1  DD  SPACE=(1024,(500,200))
//LKED.SYSIN DD *
 NAME DYNALIST(R)
//                                                                      00150000
