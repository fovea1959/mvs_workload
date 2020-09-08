//WEGSCDX JOB (),COMPILE.SMFSELECT,CLASS=A,MSGCLASS=A,REGION=2M
//       EXEC ASMFCL
//ASM.SYSIN DD *
         TITLE 'STACK Macro'                                            00592014
         MACRO                                                          00593014
&NAME    STACK &TYPE,&LOC=,&LEN=,&STACK=,&ADDR=                         00594014
         LCLC  &LENREG,&STKREG,&ADDREG,&LOCTN                           00595014
         AIF   ('&TYPE(1)' EQ 'PUSH').PUSH01                            00596014
         AIF   ('&TYPE(1)' EQ 'POP').POP01                              00597014
         AIF   ('&TYPE(1)' EQ 'INIT').INIT01                            00598014
         AIF   ('&TYPE(1)' EQ 'TERM').TERM01                            00599014
         MNOTE 8,'TYPE must be INIT, PUSH, POP, or TERM'                00600014
         AGO   .MEND                                                    00601014
.*                                                                      00602014
.INIT01  ANOP                                                           00603014
         AIF   ('&LEN' EQ '').INITE01                                   00604014
         AIF   ('&LEN'(1,1) NE '(').INIT02                              00605014
&LENREG  SETC  '&LEN(1)'                                                00606014
         AIF   ('&LENREG' NE  '1').INIT03                               00607014
         MNOTE 8,'LEN= register must be 0 or 2-15'                      00608014
         AGO   .MEND                                                    00609014
         AGO   .INIT03                                                  00610014
.INIT02  ANOP                                                           00611014
&LENREG  SETC  'R0'                                                     00612014
         LA    &LENREG,&LEN Length of Stack                             00613014
.INIT03  ANOP                                                           00614014
&LOCTN   SETC  '&LOC'                                                   00615014
         AIF   ('&LOC' NE '').INIT04                                    00616014
&LOCTN   SETC  'ANY'                                                    00617014
.INIT04  ANOP                                                           00618014
         LA    R1,StackHdr_Length       Length of header                00619014
         AR    &LENREG,R1               Bump length                     00620014
         STORAGE OBTAIN,                Go get our storage             +00621014
               LENGTH=(&LENREG),         this long                     +00622014
               LOC=&LOCTN                anywhere                       00623014
         ST    R0,StackHdr_StackLen-StackHdr(0,R1) Save length          00624014
         LA    R0,StackHdr_Length(0,R1) A(Current top of stack)         00625014
         ST    R0,StackHdr_NAB-StackHdr(0,R1) Save A(Next Avail Byte)   00626014
         AIF   ('&STACK' EQ '').INIT99                                  00627014
         AIF   ('&STACK'(1,1) EQ '(').INIT92                            00628014
         ST    R1,&STACK                Save Stack origin               00629014
         AGO   .INIT99                                                  00630014
.INIT92  ANOP                                                           00631014
         LR    &STACK(1),R1             Load return register            00632014
         AGO   .INIT99                                                  00633014
.INIT99  ANOP                                                           00634014
         AIF   (D'Stack).MEND                                           00635014
Stack               DSECT                                               00636014
                                                                        00637014
*-------------------------------------------------------------------*   00638014
*                                                                   *   00639014
*   This is a LIFO stack that is used by various procedures in the  *   00640014
*  program to acquire storage. The Stack is GETMAINed at the beg-   *   00641014
*  inning of the program, and is typically used by procedures to    *   00642014
*  acquire storage for local variables (i.e., variables that are    *   00643014
*  used only within the procedures), although the main procedure    *   00644014
*  also uses a stack area to store Global variables (i.e., those    *   00645014
*  that are available throughout the program).                      *   00646014
*                                                                   *   00647014
*-------------------------------------------------------------------*   00648014
                                                                        00649014
StackHdr            EQU   *           Start of Header                   00650014
StackHdr_StackLen   DS    FL4          Length of Stack                  00651014
StackHdr_NAB        DS    AL4          A(Next Available Byte in Stack)  00652014
StackHdr_Length     EQU   *-StackHdr  Length of Header                  00653014
                                                                        00654014
&SYSECT  &SYSSTYP                                                       00655014
         AGO   .MEND                                                    00656014
.INITE01 ANOP                                                           00657014
         MNOTE 8,'LEN must be specified for STACK INIT'                 00658014
         AGO   .MEND                                                    00659014
.*                                                                      00660014
.PUSH01  ANOP                                                           00661014
         AIF   ('&STACK' EQ '').PUSM02                                  00662014
         AIF   ('&STACK'(1,1) NE '(').PUSH01A                           00663014
&STKREG  SETC  '&STACK(1)'                                              00664014
         AGO   .PUSH02                                                  00665014
.PUSH01A ANOP                                                           00666014
&STKREG  SETC  'R15'                                                    00667014
         L     &STKREG,&STACK           A(Stack Origin)                 00668014
.PUSH02  ANOP                                                           00669014
         AIF   ('&LEN' EQ '').PUSHE01                                   00670014
         AIF   ('&LEN'(1,1) NE '(').PUSH03                              00671014
&LENREG  SETC  '&LEN(1)'                                                00672014
         AGO   .PUSH04                                                  00673014
.PUSH03  ANOP                                                           00674014
&LENREG  SETC  'R0'                                                     00675014
.PUSH04  ANOP                                                           00676014
         LA    &LENREG,&LEN                                             00677014
         LA    R1,7                     Make                            00678014
         AR    &LENREG,R1                a                              00679014
         SRL   &LENREG,3                 doubleword                     00680014
         SLL   &LENREG,3                 multiple                       00681014
         L     R1,StackHdr_NAB-StackHdr(0,&STKREG) A(Next Avail Byte)   00682014
         AR    R1,&LENREG               Bump by length requested        00683014
         SLR   R1,&STKREG               Get Stack used                  00684014
         C     R1,StackHdr_StackLen-StacKHdr(0,&STKREG) Too much?       00685014
         BL    *+8                      No, OK                          00686014
         BAL   R14,0                    ABEND                           00687014
         ALR   R1,&STKREG               No, offset to address           00688014
         ST    R1,StackHdr_NAB-StackHdr(0,&STKREG) A(Next Avail Byte)   00689014
         SLR   R1,&LENREG               A(Start of storage area)        00690014
         AIF   ('&ADDR' EQ '').MEND                                     00691014
         AIF   ('&ADDR'(1,1) NE '(').PUSH92                             00692014
         LR    &ADDR(1),R1             Load return register             00693014
         AGO   .MEND                                                    00694014
.PUSH92  ANOP                                                           00695014
         ST    R1,&ADDR                 Save Stack origin               00696014
         AGO   .MEND                                                    00697014
.PUSHE01 ANOP                                                           00698014
         MNOTE 8,'LEN must be specified for STACK PUSH'                 00699014
         AGO   .MEND                                                    00700014
.PUSHE02 ANOP                                                           00701014
         MNOTE 8,'STACK must be specified for STACK PUSH'               00702014
         AGO   .MEND                                                    00703014
.*                                                                      00704014
.POP01   ANOP                                                           00705014
         AIF   ('&ADDR' EQ '').POPE01                                   00706014
         AIF   ('&ADDR'(1,1) NE '(').POP02                              00707014
&ADDREG  SETC  '&ADDR(1)'                                               00708014
         AGO   .POP02A                                                  00709014
.POP02   ANOP                                                           00710014
&ADDREG  SETC  'R0'                                                     00711014
         L     &ADDREG,&ADDR                                            00712014
.POP02A  ANOP                                                           00713014
         AIF   ('&STACK' EQ '').POPE02                                  00714014
         AIF   ('&STACK'(1,1) NE '(').POP03                             00715014
&STKREG  SETC  '&STACK(1)'                                              00716014
         AGO   .POP04                                                   00717014
.POP03   ANOP                                                           00718014
&STKREG  SETC  'R15'                                                    00719014
         L     &STKREG,&STACK           A(Stack Origin)                 00720014
.POP04   ANOP                                                           00721014
         ST    &ADDREG,StackHdr_NAB-StackHdr(0,&STKREG)                 00722014
         AGO   .MEND                                                    00723014
.POPE01  ANOP                                                           00724014
         MNOTE 8,'ADDR must be specified for STACK POP'                 00725014
         AGO   .MEND                                                    00726014
.POPE02  ANOP                                                           00727014
         MNOTE 8,'STACK must be specified for STACK POP'                00728014
         AGO   .MEND                                                    00729014
.*                                                                      00730014
.TERM01  ANOP                                                           00731014
         AIF   ('&STACK' EQ '').TERME02                                 00732014
         AIF   ('&STACK'(1,1) NE '(').TERM03                            00733014
&STKREG  SETC  '&STACK(1)'                                              00734014
         AGO   .TERM04                                                  00735014
.TERM03  ANOP                                                           00736014
&STKREG  SETC  'R15'                                                    00737014
         L     &STKREG,&STACK           A(Stack Origin)                 00738014
.TERM04  ANOP                                                           00739014
         L     R0,StackHdr_StackLen-StackHdr(0,&STKREG) Length          00740014
         STORAGE RELEASE,               Free our storage               +00741014
               ADDR=(&STKREG),           starting here                 +00742014
               LENGTH=(0)                for this long                  00743014
         AGO   .MEND                                                    00744014
.TERME02 ANOP                                                           00745014
         MNOTE 8,'STACK must be specified for STACK TERM'               00746014
         AGO   .MEND                                                    00747014
.*                                                                      00748014
.MEND    ANOP                                                           00749014
         MEND                                                           00750014
         TITLE 'Assembly Variables'                                     00001014
         LCLC  &VERSION                                  @RWS 15-11-13  00002032
         LCLC  &CPERSON                                  @RWS 15-11-13  00003032
&VERSION SETC  'V1.72'                                   @RWS 15-11-13  00004032
&CPERSON SETC  'RWSUHR'                                  @RWS 15-11-13  00005032
                                                                        00006014
         LCLC  &ASMDT                                                   00007014
*&ASMDT  SETC  '&SYSDATC'(1,4).'-'.'&SYSDATC'(5,2).'-'.'&SYSDATC'(7,2)  00008028
&ASMDT   SETC  '2020-04-14'                                             00008029
SMFSLCT  TITLE 'Introduction'                                           00009014
*********************************************************************   00010014
*                                                                   *   00011014
*                 S  M  F  S  L  C  T                               *   00012014
*                                                                   *   00013014
*   This program will read SMF data (from the MAN datasets or output*   00014014
*  from IFASMFDP), and will select records according to user        *   00015014
*  specified criteria. Records can be selected by date and time,    *   00016014
*  by record type (and /or subtype), by Jobname, or by data at an   *   00017014
*  offset within the record. Records must meet date/time and        *   00018014
*  record type criteria before being eligible for selection, as     *   00019014
*  determined by Jobname and/or data specifications.                *   00020014
*                                                                   *   00021014
*   By default, any records selected from the input will be printed *   00022014
*  to SYSPRINT, unless 'PRINT=NO' is specified in the SYSIN control *   00023014
*  cards. If SYSUT2 is present, the selected input records will     *   00024014
*  also be written to it.                                           *   00025014
*                                                                   *   00026014
*   Multiple output files with different selection criteria can be  *   00027014
*  produced in one run of SMFSLCT. The output DDNames and selection *   00028014
*  criteria are specified in the control cards in SYSIN.            *   00029014
*                                                                   *   00030014
*   Two user-written exits can be used with SMFSLCT - the first     *   00031014
*  is given control after an SMF record is read, and the second     *   00032014
*  after an SMF record has been selected.                           *   00033014
*                                                                   *   00034014
*  Input:                                                           *   00035014
*      SYSUT1   - SMF dataset - either the output from IFASMFDP or  *   00036014
*                 one of the SYS1.MAN datasets. If this DDName is   *   00037014
*                 not allocated, it will be dynamically allocated to*   00038014
*                 the active SMF dataset.                           *   00039014
*      SYSIN    - Control cards (see below).                        *   00040014
*                                                                   *   00041014
*  Output:                                                          *   00042014
*      SYSPRINT - Error messages, selected SMF records.             *   00043014
*      SYSUT2   - optional, selected SMF records.                   *   00044014
*                                                                   *   00045014
*  Attributes:                                                      *   00046014
*      Re-entrant, Reuseable, Refreshable, Not Authorized,          *   00047014
*      AMODE(31), RMODE(ANY)                                        *   00048014
*                                                                   *   00049014
*  Coding    : All data areas in Dynamic Storage have names starting*   00050014
*   Notes       with '@'. One area of Dynamic Storage is acquired   *   00051014
*               during initialization, and is used as a LIFO stack  *   00052014
*               for dynamic storage.                                *   00053014
*              Conversely, data areas within the program (and thus  *   00054014
*               are read-only) have names that do not start with    *   00055014
*               an '@'.                                             *   00056014
*                                                                   *   00057014
*              The names of the Routines (and non-data labels within*   00058014
*               the routines) follow the pattern:                   *   00059014
*                 xnnnn                                             *   00060014
*               where x    is 1 or more alphabetic characters       *   00061014
*                     nnnn is a numeric sequence number             *   00062014
*                                                                   *   00063014
*              Data in dynamic storage that are globally accessible *   00064014
*               have names of the form:                             *   00065014
*                @_anyname                                          *   00066014
*              Data associated with a routine typically have names: *   00067014
*                 x_anyname or @x_anyname                           *   00068014
*               where 'x' is the alphabetic character associated    *   00069014
*                         with the routine                          *   00070014
*              Addressability to globally-accessible dynamic data is*   00071014
*               via R12; local dynamic data is addressable via R13. *   00072014
*                                                                   *   00073014
*              Each routine establishes it's own base register(s),  *   00074014
*               and has access only to it's own read-only variables.*   00075014
*               Base registers are typically R11.                   *   00076014
*                                                                   *   00077014
*  Sample Installation JCL:                                         *   00078014
*                                                                   *   00079014
*  //SMFSLCTA JOB blah,blah                                         *   00080014
*  //*                                                              *   00081014
*  //ASM     EXEC PGM=ASMA90,REGION=4M,                             *   00082014
*  //             PARM='NODECK,OBJECT,RENT,XREF(SHORT),NOALIGN'     *   00083014
*  //SYSPRINT  DD SYSOUT=*                                          *   00084014
*  //SYSLIB    DD DISP=SHR,DSN=SYS1.MACLIB                          *   00085014
*  //          DD DISP=SHR,DSN=SYS1.MODGEN                          *   00086014
*  //SYSUT1    DD DSN=&&SYSUT1,SPACE=(4096,(120,120),,,ROUND),      *   00087014
*  //             UNIT=SYSALLDA                                     *   00088014
*  //SYSLIN    DD DSN=&&OBJ,SPACE=(3040,(40,40),,,ROUND),           *   00089014
*  //             UNIT=SYSALLDA,DISP=(MOD,PASS),                    *   00090014
*  //             DCB=(BLKSIZE=3040,LRECL=80,RECFM=FB)              *   00091014
*  //SYSIN     DD DISP=SHR,DSN=source-program                       *   00092014
*  //*                                                              *   00093014
*  //LKED    EXEC PGM=HEWL,REGION=4M,COND=(0,LT),                   *   00094014
*  //             PARM='LIST,MAP,RENT,REUS,REFR'                    *   00095014
*  //SYSPRINT  DD SYSOUT=*                                          *   00096014
*  //SYSLIN    DD DSN=&&OBJ,DISP=(OLD,DELETE)                       *   00097014
*  //          DD DDNAME=SYSIN                                      *   00098014
*  //SYSUT1    DD DSN=&&SYSUT1,SPACE=(1024,(120,120),,,ROUND)       *   00099014
*  //             UNIT=SYSALLDA                                     *   00100014
*  //SYSLMOD   DD DISP=SHR,DSN=loadlib                              *   00101014
*  //                                                               *   00102014
*                                                                   *   00103014
*  Change Log:                                                      *   00104014
*      1999/04/24 SDDA030 V1.1                                      *   00105014
*                 Added code to ignore records invalidly spanned    *   00106014
*                 in non-VSAM VBS input.                            *   00107014
*                 Calculated DCB=NCP (BSAM), DCB=BUFNO (QSAM), or   *   00108014
*                 AMP=BUFSP (VSAM) so we can read a cylinder of     *   00109014
*                 input at a time (if input on DASD).               *   00110014
*                 Used MACRF=PM on output file(s), so VBS datasets  *   00111014
*                 can be created.                                   *   00112014
*                 Lives and executes in 31-bit mode.                *   00113014
*                 Used internal LIFO stack for dynamic storage      *   00114014
*                 Made Y2K compliant!                               *   00115014
*      1999/07/22 SDDA030 V1.2                                      *   00116014
*                 Added Date conversion routines.                   *   00117014
*                 Added new START=/END= date keywords.              *   00118014
*                 Added 24-bit EODAD for SYSIN, if required         *   00119014
*      1999/11/18 SDDA030 V1.3                                      *   00120014
*                 Moved SYSUT1 OPEN logic(?) to its own subroutine  *   00121014
*                 (I0010)                                           *   00122014
*                 Added comments                                    *   00123014
*                                                                   *   00124014
*      2002/02/07 SDDA030 V1.4                                      *   00125014
*                 Added stack below the line.                       *   00126014
*                 Added new parms for exits as suggested by Dave    *   00127014
*                 Alcock.                                           *   00128014
*                 Added parms to INEXIT=/OUTEXIT= statements.       *   00129014
*                 Defaulted 2-digit years on START=/END= to the     *   00130014
*                 current century.                                  *   00131014
*                 Corrected S0C4 Abends when lots of record types   *   00132014
*                 or jobnames selected (Thanks to Paul              *   00133014
*                 Luttenberger).                                    *   00134014
*                 Changed RDJFCBs to SWAREQs.                       *   00135014
*                 Corrected logic when setting DCB characteristics  *   00136014
*                 for new output datasets with no DCB spacified.    *   00137014
*                 Printed **VIO** for volser when appropriate.      *   00138014
*      2006/10/26 SDDA030 V1.5                                      *   00139014
*                 Fixed bug when printing record subtypes to be     *   00140014
*                 selected (thanks to BBordonaro).                  *   00141014
*                 Corrected calculation of SYSPRINT LRECL (fixed    *   00142014
*                 check for CC) (thanks to Martin Samson).          *   00143014
*                 Corrected non-re-entrancy error with SYSUT1 EXLST *   00144014
*                 (thanks to Martin Samson - again!).               *   00145014
*                                                                   *   00146034
*      2013-03-19 RWSUHR  V1.6                                      *   00147014
*                 Set return code to 8 if nothing is selected       *   00148014
*                                                                   *   00149014
*      2015-10-21 RWSUHR  V1.7                                      *   00150014
*                                                                   *   00151014
*      2015-11-13 RWSUHR  V1.72 - @RWS                              *   00152033
*                 Fixed an S0C7, when OUTPUT= was specified         *   00153028
*                 added assembly date and time and change person    *   00154029
*                 ID (ID of person who last changed this program)   *   00155029
*                 Roger W. Suhr (State of Indiana - IOT)            *   00156034
*                                                                   *   00157034
*  Author:                                                          *   00158014
*   All comments, criticisms and suggestions gratefully received.   *   00159014
*     Paul Dion  (pdion@canada.com)                                 *   00160014
*                                                                   *   00161014
*********************************************************************   00162014
         TITLE 'Control Cards'                                          00163014
*********************************************************************   00164014
*                                                                   *   00165014
*                 C o n t r o l   C a r d s                         *   00166014
*                                                                   *   00167014
*   Control cards are entered via SYSIN. Only columns 1-72 are      *   00168014
*  scanned, and the scan terminates with the first blank. Cont-     *   00169014
*  inuations are not allowed, but keywords can be specified more    *   00170014
*  than once. If a keyword that has a single value (ie, START=)     *   00171014
*  is specified more than once within an output group, or globally, *   00172014
*  the last value specified is used.                                *   00173014
*                                                                   *   00174014
*   OUTPUT=DDName                                                   *   00175014
*      specifies the DDName of an output dataset. All subsequent    *   00176014
*                control statements, until the next "OUTPUT="       *   00177014
*                statement or end-of-file, determine which input    *   00178014
*                records will be selected to this DDName. All       *   00179014
*                control statements appearing before an "OUTPUT="   *   00180014
*                statement apply to all output DDNames, unless      *   00181014
*                specifically overridden by control statements      *   00182014
*                entered after the "OUTPUT=" statement.             *   00183014
*                                                                   *   00184014
*   START=yyddd-hhmmss                                              *   00185014
*         yyyyddd-hhmmss                                            *   00186014
*         TODAY-hhmmss                                              *   00187014
*         (TODAY-n)-hhmmss                                          *   00188014
*         YESTERDAY-hhmmss                                          *   00189014
*         THISWEEK-hhmmss                                           *   00190014
*         LASTWEEK-hhmmss                                           *   00191014
*         THISMONTH-hhmmss                                          *   00192014
*         LASTMONTH-hhmmss                                          *   00193014
*      specifies the date and time at which selection of input      *   00194014
*                records is to start. Defaults to 00000-000000 (ie  *   00195014
*                the start of the input file). The specification of *   00196014
*                time (-hhmmss) is optional - if omitted, it def-   *   00197014
*                aults to 00:00:00.00.                              *   00198014
*                Dates using the 2-digit year format will default   *   00199014
*                to dates in the current century.                   *   00200014
*                YESTERDAY is equivalent to specifying (TODAY-1).   *   00201014
*                Note that a week starts on Monday, and ends on     *   00202014
*                Sunday. Thus, specifying START=LASTWEEK will start *   00203014
*                selecting records from the Monday of the previous  *   00204014
*                week.                                              *   00205014
*                                                                   *   00206014
*   END=yyddd-hhmmss                                                *   00207014
*       yyyyddd-hhmmss                                              *   00208014
*       TODAY-hhmmss                                                *   00209014
*       (TODAY-n)-hhmmss                                            *   00210014
*       YESTERDAY-hhmmss                                            *   00211014
*       THISWEEK-hhmmss                                             *   00212014
*       LASTWEEK-hhmmss                                             *   00213014
*       THISMONTH-hhmmss                                            *   00214014
*       LASTMONTH-hhmmss                                            *   00215014
*      specifies the date and time at which selection if input      *   00216014
*                records is to stop. Defaults to 2699999-235959 (ie *   00217014
*                the end of the input file). The specification of   *   00218014
*                time (-hhmmss) is optional - if omitted, it def-   *   00219014
*                aults to 23:59:59.99.                              *   00220014
*                Dates using the 2-digit year format will default   *   00221014
*                to dates in the current century.                   *   00222014
*                YESTERDAY is equivalent to specifying (TODAY-1).   *   00223014
*                Note that a week starts on Monday, and ends on     *   00224014
*                Sunday. Thus, specifying END=LASTWEEK will stop    *   00225014
*                selecting records on the Sunday of the previous    *   00226014
*                week.                                              *   00227014
*                                                                   *   00228014
*   PERIOD=hhmmss-hhmmss                                            *   00229014
*      specifies the start and stop time-of-day between which input *   00230014
*                records will be eligible for selection. Records for*   00231014
*                each day, as specified by start/end or defaulted,  *   00232014
*                falling outside these times will be ignored.       *   00233014
*                 The time can be specified as hh, hhmm or hhmmss;  *   00234014
*                the missing digits will default to 0.              *   00235014
*                 Defaults to "PERIOD=000000-240000".               *   00236014
*                                                                   *   00237014
*   WEEKENDS=IGNORE                                                 *   00238014
*      specifies that SMF data generated on Saturday and Sunday will*   00239014
*                be ignored.                                        *   00240014
*                                                                   *   00241014
*   INCLUDE=nn<(ss)>                                                *   00242014
*      specifies that record type nn should be selected. A subtype  *   00243014
*                ss can also be optionally included, in parentheses.*   00244014
*                If more that one record type is coded on the       *   00245014
*                control card, the record types (and subtypes)      *   00246014
*                must be enclosed in parentheses.                   *   00247014
*                SMF Record Type 30 is used as a model to determine *   00248014
*                the SubType location.                              *   00249014
*                Note that 'INCLUDE' is mutually exclusive with the *   00250014
*                'EXCLUDE' keyword within an "OUTPUT=" definition,  *   00251014
*                or globally, if no "OUTPUT=" statement is present. *   00252014
*                                                                   *   00253014
*   EXCLUDE=nn<(ss)>                                                *   00254014
*      specifies that record type nn should not be selected. A      *   00255014
*                subtype ss can optionally be included, in paren-   *   00256014
*                theses. If more than one record type is coded,     *   00257014
*                the record types (and subtypes) and subtypes must  *   00258014
*                be enclosed in parentheses.                        *   00259014
*                Note that 'EXCLUDE' is mutually exclusive with the *   00260014
*                'INCLUDE' keyword within an "OUTPUT=" definition,  *   00261014
*                or globally, if no "OUTPUT=" statement is present. *   00262014
*                                                                   *   00263014
*   PRINT=NO                                                        *   00264014
*   PRINT=EBCDIC                                                    *   00265014
*         NO     specifies that selected records are not to be      *   00266014
*                printed on SYSPRINT. If an output ddname is        *   00267014
*                present, selected records will be written to it.   *   00268014
*         EBCDIC specifies that selected records are to be printed  *   00269014
*                on SYSPRINT in EBCDIC only (the default is dump    *   00270014
*                format). If an output ddname is present, the       *   00271014
*                selected records will be written to it.            *   00272014
*                                                                   *   00273014
*   SEQUENCE=YES                                                    *   00274014
*                specifies that the input records are in date/time  *   00275014
*                sequence. If 'SEQUENCE=YES' is specified, SMFSLCT  *   00276014
*                will terminate as soon as a record with a date     *   00277014
*                stamp past the 'END=' date/time is encountered.    *   00278014
*                Normal SMFSLCT processing assumes that the input   *   00279014
*                records are not in sequence, and invokes           *   00280014
*                termination at end-of-file.                        *   00281014
*                                                                   *   00282014
*   CHECKVBS=NO                                                     *   00283014
*                specifies that no checking should be performed for *   00284014
*                variable spanned segmentation errors. If this      *   00285014
*                is specified, QSAM will be used to read SYSUT1,    *   00286014
*                and invalid segment sequences will cause an S002   *   00287014
*                (or potentially other system) abend. The default   *   00288014
*                is that BSAM will be used to read VS or VBS files, *   00289014
*                and invalid segments discarded.                    *   00290014
*                Note that using BSAM requires more CPU time; where *   00291014
*                the input file is known to be 'clean', specifying  *   00292014
*                CHECKVBS=NO will reduce the CPU time used by this  *   00293014
*                program.                                           *   00294014
*                                                                   *   00295014
*   JOBNAME=jjjjjjjj                                                *   00296014
*      specifies that all records (subject to "INCLUDE"/"EXCLUDE",  *   00297014
*                above), Job (or TSU or STC) jjjjjjjj should be     *   00298014
*                selected.                                          *   00299014
*                If more that one jobname is coded, the jobnames    *   00300014
*                should be enclosed in parentheses.                 *   00301014
*                 If the last character in the jobname is an        *   00302014
*                asterisk, the characters preceeding the asterisk   *   00303014
*                considered a prefix, and all jobnames that match   *   00304014
*                the prefix will be selected.                       *   00305014
*                SMF Record Type 5 is used as a model to determine  *   00306014
*                the location of the Jobname field.                 *   00307014
*                                                                   *   00308014
*   DATA=xxx<,nn>                                                   *   00309014
*      specifies the character string that must be present in the   *   00310014
*                input record before it will be selected.           *   00311014
*                DATA= indicates that the string xxx (specified     *   00312014
*                      either as a data(xxx...x), a character string*   00313014
*                      (C'xxx...x) or a hex string(X'xxx...x)) must *   00314014
*                      start at offset position nn (nn=0-32767,     *   00315014
*                      default=0), for the input record to be       *   00316014
*                      selected. Note that the offset must include  *   00317014
*                      the length of the RDW.                       *   00318014
*                                                                   *   00319014
*   JOB/DATA=OR                                                     *   00320014
*      specifies that the selection algorithm should select an      *   00321014
*                      input record if either the Jobname or data   *   00322014
*                      specifications are met. The default is that  *   00323014
*                      both Jobname and data criteria must be       *   00324014
*                      satisfied before a record will be selected.  *   00325014
*                                                                   *   00326014
*   STOPAFT=nnn                                                     *   00327014
*      specifies that SMFSLCT is to terminate after selecting nnn   *   00328014
*                      records (nnn = 1 to 999999999). The default  *   00329014
*                      is to terminate when end-of-file is reached  *   00330014
*                      on input.                                    *   00331014
*                                                                   *   00332014
*   INEXIT=(member-name<,parm>)                                     *   00333014
*      specifies that SMFSLCT is to invoke member-name after        *   00334014
*                      each SMF record is read from the input, and  *   00335014
*                      after checking the date/time against "START="*   00336014
*                      and "END=", but before checking any other    *   00337014
*                      selection criteria.                          *   00338014
*                      "parm" is an optional string that will be    *   00339014
*                      passed to the exit each time it is invoked.  *   00340014
*                      If present, the operands of INEXIT= must     *   00341014
*                      be enclosed in parentheses. Further, if parm *   00342014
*                      contains a close parenthesis, it must be     *   00343014
*                      enclosed in quotes (as per IBM, if parm also *   00344014
*                      contains a quote, it must be coded as two    *   00345014
*                      two consecutive quotes).                     *   00346014
*                                                                   *   00347014
*   OUTEXIT=(member-name<,parm>)                                    *   00348014
*      specifies that SMFSLCT is to invoke member-name after        *   00349014
*                      an SMF record has been selected, but before  *   00350014
*                      the record is printed and/or written to the  *   00351014
*                      output file(s).                              *   00352014
*                      "parm" is an optional string that will be    *   00353014
*                      passed to the exit each time it is invoked.  *   00354014
*                      If present, the operands of OUTEXIT= must    *   00355014
*                      be enclosed in parentheses. Further, if parm *   00356014
*                      contains a close parenthesis, it must be     *   00357014
*                      enclosed in quotes (as per IBM, if parm also *   00358014
*                      contains a quote, it must be coded as two    *   00359014
*                      two consecutive quotes).                     *   00360014
*                                                                   *   00361014
*  Example:                                                         *   00362014
*  //SMFSLCT  JOB blah,blah                                         *   00363014
*  //STEP1   EXEC PGM=SMFSLCT,REGION=6M                             *   00364014
*  //STEPLIB   DD DISP=SHR,DSN=if-required                          *   00365014
*  //SYSPRINT  DD SYSOUT=*                                          *   00366014
*  //SYSUT1    DD DISP=unloaded-man-file,DISP=SHR                   *   00367014
*  //OUT1      DD UNIT=SYSALLDA,DISP=(,CATLG,DELETE),               *   00368014
*  //             DSN=output-file1,SPACE=(blah,blah),               *   00369014
*  //             DCB=(blah,blah)                                   *   00370014
*  //OUT2      DD UNIT=SYSALLDA,DISP=(,CATLG,DELETE),               *   00371014
*  //             DSN=output-file1,SPACE=(blah,blah),               *   00372014
*  //             DCB=(blah,blah)                                   *   00373014
*  //OUT3      DD UNIT=SYSALLDA,DISP=(,CATLG,DELETE),               *   00374014
*  //             DSN=output-file1,SPACE=(blah,blah),               *   00375014
*  //             DCB=(blah,blah)                                   *   00376014
*  //SYSIN     DD *                                                 *   00377014
*  START=87015,END=87017-170000                                     *   00378014
*  OUTPUT=OUT1                                                      *   00379014
*   INCLUDE=(30(2),20,5)                                            *   00380014
*   JOBNAME=JOBONE                                                  *   00381014
*   PRINT=NO                                                        *   00382014
*  OUTPUT=OUT2                                                      *   00383014
*   START=87013-081500,END=87013-170000                             *   00384014
*   INCLUDE=34                                                      *   00385014
*   JOBNAME=(TSO1,TSO2)                                             *   00386014
*  OUTPUT=OUT3                                                      *   00387014
*   DATA=C'A B C',257                                               *   00388014
*   OUTEXIT=EXIT5                                                   *   00389014
*  /*                                                               *   00390014
*                                                                   *   00391014
*       SMFSLCT will read the unloaded MAN file specified by SYSUT1.*   00392014
*       Note that if SYSUT1 were not specified, the active MAN      *   00393014
*       file would be read.                                         *   00394014
*       SMFSLCT will ignore any input records produced before       *   00395014
*       00:00:00 on 87.015, and terminate after finding a record    *   00396014
*       produced after 17:00:00 on 87.017.                          *   00397014
*       For the output file specified by DDName OUT1:               *   00398014
*        Record types 5, 20, and 30 subtype 2, for Jobs with the    *   00399014
*        name "JOBONE" will be selected, but not printed.           *   00400014
*       For the output file specified by DDName OUT2:               *   00401014
*        Record type 34 for Jobs (TSO users) with the names "TSO1"  *   00402014
*        or "TSO2" will be selected, and printed.                   *   00403014
*        The global date specification is overridden for this       *   00404014
*        output file only.                                          *   00405014
*       For the output file specified by DDName OUT3:               *   00406014
*        All records containing the character string "A B C" in     *   00407014
*        positions 257-261 (relative to 0, including the RDW) will  *   00408014
*        be selected and printed. The exit "EXIT5" will be invoked  *   00409014
*        for each record selected for "OUT3"                        *   00410014
*                                                                   *   00411014
*       Note that the selection criteria for the different output   *   00412014
*       files are indented: this is for readability only - SMFSLCT  *   00413014
*       ignores leading blanks.                                     *   00414014
*                                                                   *   00415014
*********************************************************************   00416014
         TITLE 'Processing Overview'                                    00417014
*********************************************************************   00418014
*                                                                   *   00419014
*          P r o c e s s i n g   O v e r v i e w                    *   00420014
*                                                                   *   00421014
*  Initialization.                                                  *   00422014
*     SMFSLCT first opens SYSPRINT - if this file cannot be OPENed  *   00423014
*    SMFSLCT terminates immediately. SYSIN statements (if present)  *   00424014
*    are then read and interpreted. Assuming there were             *   00425014
*    no control card errors, an OPEN is attempted for SYSUT1.       *   00426014
*    If this fails, SYSUT1 is dynamically allocated to the active   *   00427014
*    SMF file, and OPENed. Next, the JFCB for each output DDName is *   00428014
*    read. If the output file is NEW, and DCB characteristics are   *   00429014
*    not present in the JFCB, SMFSLCT assigns the RECFM, LRECL and  *   00430014
*    BLKSIZE from SYSUT1 (if SYSUT1 is a VSAM dataset, RECFM=VBS,   *   00431014
*    LRECL=32767, BLKSIZE=4096 are assigned as a default). If the   *   00432014
*    output dataset is OLD or SHR, no DCB changes are made.         *   00433014
*     If exits were specified, SMFSLCT 'LOAD's them, and saves      *   00434014
*    their EP addresses.                                            *   00435014
*                                                                   *   00436014
*  Processing.                                                      *   00437014
*     A record is read from SYSUT1, and, as long as it is not a dump*   00438014
*    header or trailer (record type 2 or 3), it is checked against  *   00439014
*    the start and stop times. If it is before the start time, the  *   00440014
*    record is ignored, and the next record is read. If it is after *   00441014
*    the stop time, termination is initiated.                       *   00442014
*     After the start and stop time checks, the INEXIT (if present) *   00443014
*    is invoked. If the exit indicates that this record should be   *   00444014
*    selected (RC=4), all further checks are bypassed. If the exit  *   00445014
*    indicates the record should be skipped (RC=8), the record is   *   00446014
*    ignored. If the exit indicates that SMFSLCT should terminate,  *   00447014
*    the stop date and time is set to 0, so that the next input     *   00448014
*    record will cause termination to be invoked. If the exit is    *   00449014
*    not present, or indicates that normal processing should cont-  *   00450014
*    inue (RC=0), the selection criteria are checked.               *   00451014
*     The first selection check is record type (INCLUDE=/EXCLUDE=). *   00452014
*    If the record passes this check, the job name criteria         *   00453014
*    (JOBNAME=) is checked. If the SMF record does not 'belong' to  *   00454014
*    one of the Jobnames specified in the control cards (assuming   *   00455014
*    that at least one Jobname was entered), the record is skipped, *   00456014
*    unless JOB/DATA=OR was specified. If it was, or the SMF record *   00457014
*    passed the Jobname check, the data criteria (DATA=) are exam-  *   00458014
*    ined. If the data specified are present in the SMF record, the *   00459014
*    record will be selected.                                       *   00460014
*     Once an input record is selected, the output exit (OUTEXIT=), *   00461014
*    if present, is invoked. This exit can indicate that the record *   00462014
*    should be ignored (RC=8), that processing should terminate     *   00463014
*    after this record (RC=12), or that normal processing should    *   00464014
*    continue (RC=0).                                               *   00465014
*     A selected record is first printed (unless PRINT=NO was       *   00466014
*    specified in the control cards), either in the default dump    *   00467014
*    format, or in EBCDIC only (PRINT=EBCDIC). Then, if output files*   00468014
*    are open, the selected record is written to them. The output   *   00469014
*    record length is checked against the input, and the input rec- *   00470014
*    ord truncated or padded with blanks as appropriate. If the     *   00471014
*    output RECFM is fixed, the SMF record is written without the   *   00472014
*    RDW.                                                           *   00473014
*                                                                   *   00474014
*  Termination.                                                     *   00475014
*     Termination is invoked either because an input SMF record's   *   00476014
*    date and time is greater than the stop date and time, or       *   00477014
*    because EOF was reached on the input file. The counters are    *   00478014
*    printed, and all open files are closed. The exits are invoked  *   00479014
*    one last time, and then deleted. All getmained storage is      *   00480014
*    freed, and standard exit code is executed to return to OS.     *   00481014
*                                                                   *   00482014
*********************************************************************   00483014
         TITLE 'User Exits'                                             00484014
*********************************************************************   00485014
*                                                                   *   00486014
*     SMFSLCT has provision for two exits that will be invoked      *   00487014
*    after a record is read, or a record is about to be written.    *   00488014
*    Each exit receives the address of the SMF record as a parm.    *   00489014
*    The exits can cause the SMF record to be selected or ignored   *   00490014
*    (via the return codes), or can modify (or replace) the SMF     *   00491014
*    record. The only restriction is that the record returned to    *   00492014
*    SMFSLCT must remain a valid variable-length record.            *   00493014
*                                                                   *   00494014
*     SMFSLCT also provides to each exit a word of storage that the *   00495014
*    exit can use in any manner whatsoever - SMFSLCT will not alter *   00496014
*    the contents of this word (other than to initialize it to 0    *   00497014
*    before the first invocation of the exit).                      *   00498014
*                                                                   *   00499014
*     The exits are LOADed at initialization, invoked via a BASR    *   00500014
*    instruction at various points in the processing, and then      *   00501014
*    DELETEd during termination. 'Global' exits (those that are     *   00502014
*    named by INEXIT=/OUTEXIT= statements before any OUT= statement)*   00503014
*    are LOADED only once; 'local' exits (those named after an      *   00504014
*    OUT= statement, even if they are the same as the 'global'      *   00505014
*    exits) are LOADed for each output statement. Thus, if an exit  *   00506014
*    is not reusable, there may be multiple versions LOADed and     *   00507014
*    invoked.                                                       *   00508014
*                                                                   *   00509014
*     The names of the exits are specified with the "INEXIT=" and   *   00510014
*    "OUTEXIT=" control statement keywords (see above). The exits   *   00511014
*    are invoked once for each output file, at 3 different points:  *   00512014
*      Initialization: A(SMF Record) = 0                            *   00513014
*                      Input count = -1                             *   00514014
*      Processing    : A(SMF Record) > 0                            *   00515014
*      Termination   : A(SMF Record) = 0                            *   00516014
*                      Input Count >= 0                             *   00517014
*                                                                   *   00518014
*     The parms passed to the exits are the same for both INEXIT    *   00519014
*    and OUTEXIT (all parameters reside below-the-line):            *   00520014
*        R1 ---> A(SMF record) or 0                                 *   00521014
*                                                                   *   00522014
*                A(A word for the user)                             *   00523014
*                  This word is 'output-file specific'; that is,    *   00524014
*                  the same word will be passed to the INEXIT and   *   00525014
*                  the OUTEXIT for the same output file (ie, when   *   00526014
*                  the 6th parm, the address of the DDName, is      *   00527014
*                  pointing to the same value). For different output*   00528014
*                  files, a different word is passed.               *   00529014
*                                                                   *   00530014
*                A(HL2'length', parm as coded on the xxxEXIT=       *   00531014
*                  statement. Note that the length is the length of *   00532014
*                  the parm, and that any quotes surrounding the    *   00533014
*                  parm have been removed)                          *   00534014
*                                                                   *   00535014
*                A(Fullword containing input count)                 *   00536014
*                                                                   *   00537014
*                A(Fullword containing output count)                *   00538014
*                                                                   *   00539014
*                A(DDName of output file)                           *   00540014
*                                                                   *   00541014
*                A(SYSPRINT routine. This routine can be invoked to *   00542014
*                  cause SMFSLCT to write a record to SYSPRINT. If  *   00543014
*                  this routine is invoked,                         *   00544014
*                   R1 : A(2-word parmlist):                        *   00545014
*                          A(Data to be printed)                    *   00546014
*                          Length of data to be printed             *   00547014
*                   R13: A(Standard 72-byte O/S save area)          *   00548014
*                   R14: Return Address                             *   00549014
*                   R15: EPA                                        *   00550014
*                  This routine is also IDENTIFYed as SMFSLCTP, and *   00551014
*                 so can be CALLed, LINKed, etc.                    *   00552014
*                                                                   *   00553014
*  INEXIT.                                                          *   00554014
*     The input exit is specified using the "INEXIT=" control       *   00555014
*    statement keyword, and is invoked after each SMF record is     *   00556014
*    read and determined to be within the start and end times       *   00557014
*    specified in the control cards ("START=", "END="). The exit    *   00558014
*    receives the address of the SMF record as a parm, and can      *   00559014
*    modify the contents of the record, if required. If the exit    *   00560014
*    wishes to replace the SMF record with another, the exit can    *   00561014
*    change the address in the pointer to the SMF record so that it *   00562014
*    points to a different area of storage, containing the new SMF  *   00563014
*    record.                                                        *   00564014
*     Note that the input exit is invoked (perhaps non-intuitively) *   00565014
*    for each output file.                                          *   00566014
*                                                                   *   00567014
*     On Exit,                                                      *   00568014
*        R15  =  0, Continue normal processing                      *   00569014
*                4, Unconditionally select this record              *   00570014
*                8, Ignore this record                              *   00571014
*               12, Terminate SMFSLCT after this record             *   00572014
*                                                                   *   00573014
*  OUTEXIT.                                                         *   00574014
*     The output exit is specified using the "OUTEXIT=" control     *   00575014
*    statement keyword, and is invoked after an SMF record has been *   00576014
*    selected (because it satisfied selection criteria, or it was   *   00577014
*    unconditionally selected by the input exit). The exit          *   00578014
*    receives the address of the SMF record as a parm, and can      *   00579014
*    modify the contents of the record, if required. If the exit    *   00580014
*    wishes to replace the SMF record with another, the exit can    *   00581014
*    change the address in the pointer to the SMF record so that it *   00582014
*    points to a different area of storage, containing the new SMF  *   00583014
*    record.                                                        *   00584014
*     On Exit,                                                      *   00585014
*        R15  =  0, Continue normal processing                      *   00586014
*                4, Unused                                          *   00587014
*                8, Ignore this record                              *   00588014
*               12, Terminate SMFSLCT after this record             *   00589014
*                                                                   *   00590014
*********************************************************************   00591014
         TITLE 'IBM DSECTs'                                             00751014
         PUSH  PRINT                                                    00752014
         PRINT NOGEN                                                    00753014
         CVT   DSECT=YES                                                00754014
         IKJTCB                                                         00755014
         IEFTIOT1                                                       00756014
         IEESMCA                                                        00757014
         IEFJFCBN                                                       00758014
         DCBD  DSORG=PS                                                 00759014
         IHADCBE                                                        00760014
         IHADVA                                                         00761014
         IEFUCBOB DEVCLAS=DA,LIST=NO                                    00762014
         IEFZB505 LOCEPAX=YES                                           00763014
         IEFJESCT                                                       00764014
         IEFQMIDS                                                       00765014
         IFASMFR (0,5,30)                                               00766014
         IFGACB                                                         00767014
SYSUT1_ACB_Length EQU   *-IFGACB      Length of ACB                     00768014
         IFGRPL                                                         00769014
SYSUT1_RPL_Length EQU   *-IFGRPL      Length of RPL                     00770014
         IEFZB4D0                                                       00771014
         IEFZB4D2                                                       00772014
         POP   PRINT                                                    00773014
         TITLE 'Local DSECTs'                                           00774014
OutDesc_Tbl         DSECT                                               00775014
                                                                        00776014
*-------------------------------------------------------------------*   00777014
*                                                                   *   00778014
*   The Output Descriptor Table contains one entry for each output  *   00779014
*  file specified (in the "OUTPUT=" control card). Each entry is    *   00780014
*  initialized with the default parameters, and then modified with  *   00781014
*  the parameters entered via control cards. The first entry con-   *   00782014
*  tains the default parms, and is used as the "global" entry.      *   00783014
*                                                                   *   00784014
*-------------------------------------------------------------------*   00785014
                                                                        00786014
ODT_SubPool_Nbr     EQU   0           Subpool for this table            00787014
ODT_Subp_Len        DS    0F          Length and Subpool of table       00788014
ODT_SubPool         DS    X            SubPool                          00789014
ODT_Length          DS    FL3          Length                           00790014
ODT_Entry_Count     DS    HL2         Nbr entries in the table          00791014
ODT_Entry           DS    0F          Table Entry                       00792019
                                                                        00793014
*-------------------------------------------------------------------*   00794014
*                                                                   *   00795014
*    Output Descriptor Entry. There is one entry for each output    *   00796014
*   file (defined using the "OUTPUT=" statement), plus one entry    *   00797014
*   (the first) used as a default entry.                            *   00798014
*                                                                   *   00799014
*-------------------------------------------------------------------*   00800014
                                                                        00801014
ODTE_DCB_Ptr        DS    AL4          A(Output DCB)                    00802014
ODTE_DDName         DS    CL8          Associated DDName                00803014
ODTE_RTT_Ptr        DS    AL4          A(Record Type Table)             00804014
ODTE_JNT_Ptr        DS    AL4          A(JobName Table)                 00805014
ODTE_DT_Ptr         DS    AL4          A(Data Table)                    00806014
ODTE_Selected       DS    FL4          Nbr Records Selected             00807014
ODTE_StopAft        DS    FL4          Stop after selecting this many   00808014
ODTE_InExit         DS    CL8          Input Exit Name                  00809014
ODTE_InExit_Ptr     DS    AL4          A(Input Exit)                    00810014
ODTE_InExit_Data    DS    FL4          Input Exit Data Area             00811014
ODTE_InExit_Parm_Ptr DS   AL4          A(Parm data for InExit)          00812014
ODTE_OutExit        DS    CL8          Output Exit Name                 00813014
ODTE_OutExit_Ptr    DS    AL4          A(Output Exit)                   00814014
ODTE_OutExit_Data   DS    FL4          Output Exit Data Area            00815014
ODTE_OutExit_Parm_Ptr DS  AL4          A(Parm data for OutExit)         00816014
ODTE_LRECL          DS    HL2          Max Output Record Length         00817014
ODTE_StartDate      DS    PL4          Start Date                       00818014
ODTE_StartTime      DS    FL4            and Time                       00819014
ODTE_EndDate        DS    PL4          End Date                         00820014
ODTE_EndTime        DS    FL4            and Time                       00821014
ODTE_PeriodStart    DS    FL4          Period Start Time                00822014
ODTE_PeriodEnd      DS    FL4          Period End Time                  00823014
                    DS    X            Flag Byte                        00824014
ODTE_Print_NO       EQU   *-1,X'80'     1... .... PRINT=NO Specified    00825014
ODTE_Print_EB       EQU   *-1,X'40'     .1.. .... PRINT=EBCDIC Spec     00826014
ODTE_JobData_OR     EQU   *-1,X'20'     ..1. .... JOB/DATA=OR           00827014
ODTE_Trunc          EQU   *-1,X'10'     ...1 .... Output Rec Truncated  00828014
ODTE_CopyOnly       EQU   *-1,X'08'     .... 1... No selection criteria 00829014
ODTE_Inactive       EQU   *-1,X'04'     .... .1.. Inactive Entry        00830014
ODTE_Ignore_Weekend EQU   *-1,X'02'     .... ..1. WEEKEND=IGNORE        00831014
                    DS    X            Flag Byte                        00832014
ODTE_No_Dflt_InExit EQU   *-1,X'80'     1... .... Do not use default    00833014
ODTE_No_Dflt_OutExit EQU  *-1,X'40'     .1.. .... Do not use default    00834014
                                                                        00835014
ODTE_Output_Count_Table DS  256FL4     Selected count by record type    00836014
                                                                        00837014
ODTE_Length         EQU   *-ODT_Entry  Entry Length                     00838014
                                                                        00839014
                                                                        00840014
                                                                        00841014
JobName_Tbl         DSECT                                               00842014
                                                                        00843014
*-------------------------------------------------------------------*   00844014
*                                                                   *   00845014
*   The JobName Table is built when it is determined that the user  *   00846014
*  wishes to select input records by jobname. The table             *   00847014
*  consists of a header portion (describing the table as a whole),  *   00848014
*  and then one entry for each valid jobname control card found.    *   00849014
*                                                                   *   00850014
*-------------------------------------------------------------------*   00851014
                                                                        00852014
JNT_Subp_Len        DS    0F          LENGTH AND SUBPOOL OF TABLE       00853014
JNT_Subpool         DS    X            SUBPOOL                          00854014
JNT_Length          DS    FL3          LENGTH                           00855014
JNT_Entry_Count     DS    HL2          NBR ENTRIES IN THE TABLE         00856014
JNT_Entry           EQU   *            DATA ENTRY                       00857014
JNTE_JobName_Len    DS    X             EXECUTE LENGTH OF JOBNAME/PFX   00858014
JNTE_JobName        DS    CL8           JOBNAME, PADDED WITH BLANKS     00859014
JNTE_Length         EQU   *-JNT_Entry                                   00860014
                                                                        00861014
                                                                        00862014
                                                                        00863014
RecType_Tbl         DSECT                                               00864014
                                                                        00865014
*-------------------------------------------------------------------*   00866014
*                                                                   *   00867014
*   THE RECORD TABLE IS BUILT WHEN IT IS DETERMINED THAT THE USER   *   00868014
*  WISHES TO INCLUDE OR EXCLUDE RECORDS BY RECORD TYPE. EACH ENTRY  *   00869014
*  CONSISTS OF TWO BYTES, A RECORD TYPE AND A SUBTYPE. All entries  *   00870014
*  are either 'INCLUDE's or 'EXCLUDE's, as indicated by a flag in   *   00871014
*  the header of the table.                                         *   00872014
*                                                                   *   00873014
*-------------------------------------------------------------------*   00874014
                                                                        00875014
RTT_Subp_Len        DS    0F          LENGTH AND SUBPOOL OF TABLE       00876014
RTT_Subpool         DS    X            SUBPOOL                          00877014
RTT_Length          DS    FL3          LENGTH                           00878014
RTT_Entry_Count     DS    HL2         NBR ENTRIES IN TABLE              00879014
                    DS    X           Flag byte                         00880014
RTT_Entry_Include   EQU   *-1,X'80'    1... .... INCLUDE=               00881014
RTT_Entry_Exclude   EQU   *-1,X'40'    .1.. .... EXCLUDE=               00882014
                    DS    X           Reserved                          00883014
RTT_Entry           EQU   *           RECORD TABLE ENTRY                00884014
RTTE_RecType        DS    X            RECORD TYPE                      00885014
                    DS    X            Flag Byte                        00886014
RTTE_SubType_Present EQU  *-1,X'80'     1... .... Subtype present       00887014
RTTE_SubType        DS    XL2          SUBTYPE                          00888014
RTTE_Length         EQU   *-RTT_Entry Entry length                      00889014
                                                                        00890014
                                                                        00891014
                                                                        00892014
Data_Tbl            DSECT                                               00893014
                                                                        00894014
*-------------------------------------------------------------------*   00895014
*                                                                   *   00896014
*   The Data Table is built when it is determined that the user     *   00897014
*  wishes to select input records by data in the record. The table  *   00898014
*  consists of a header portion (describing the table as a whole),  *   00899014
*  and then one entry for each valid 'data' control statement       *   00900014
*  found, containing:                                               *   00901014
*                     - the offset at which the data must be found  *   00902014
*                       before the record will be selected,         *   00903014
*                     - the execute length of the data, and         *   00904014
*                     - the data.                                   *   00905014
*                                                                   *   00906014
*-------------------------------------------------------------------*   00907014
                                                                        00908014
DT_Subp_Len         DS    0F          LENGTH AND SUBPOOL OF TABLE       00909014
DT_Subpool          DS    X            SUBPOOL                          00910014
DT_Length           DS    FL3          LENGTH                           00911014
DT_Entry_Count      DS    HL2         NBR ENTRIES IN THE TABLE          00912014
DT_Entry            EQU   *           DATA ENTRY                        00913014
DTE_Offset          DS    HL2          OFFSET OF DATA                   00914014
DTE_Length          DS    X            EXECUTE LENGTH OF DATA           00915014
DTE_Data            EQU   *            DATA                             00916014
                                                                        00917014
                                                                        00918014
BuffArea            DSECT                                               00919014
                                                                        00920014
*-------------------------------------------------------------------*   00921014
*                                                                   *   00922014
*   The Buffer Area is used only when SYSUT1 is VS or VBS, and      *   00923014
*  non-VSAM. Under these circumstances, we use BSAM to read blocks  *   00924014
*  of SYSUT1, and assemble the records ourselves, so we can discard *   00925014
*  any invalidly-spanned segments.                                  *   00926014
*                                                                   *   00927014
*-------------------------------------------------------------------*   00928014
                                                                        00929014
BuffArea_Next_Ptr   DS    AL4         A(Next Buffer Area)               00930014
BuffArea_DECB_Ptr   DS    AL4         A(DECB - Below-the-Line)          00931014
BuffArea_Offset     DS    FL4         Offset within BuffArea_IOArea     00932014
BuffArea_Seg_Count  DS    FL4         Segment nbr in curr block         00933014
BuffArea_IOArea     EQU   *           I/O Area for Read                 00934014
BuffArea_Hdr_Length EQU   *-BuffArea  Length of Header info             00935014
                                                                        00936014
                                                                        00937014
                                                                        00938014
DateConv_Area       DSECT                                               00939014
                                                                        00940014
*-------------------------------------------------------------------*   00941014
*                                                                   *   00942014
*   This DSECT describes the Date Conversion parameters used to     *   00943014
*  make SMF Dates and Times useable                                 *   00944014
*                                                                   *   00945014
*-------------------------------------------------------------------*   00946014
                                                                        00947014
                    DS    X           Input Descriptor Flag             00948014
DConv_Input_Julian  EQU   *-1,X'80'    1... .... Input in YYYY and DDD  00949014
DConv_Input_Base    EQU   *-1,X'40'    .1.. .... Input in Base date     00950014
DConv_Input_YYMMDD  EQU   *-1,X'20'    ..1. .... Input in YYYY,MM,DD    00951014
DConv_Input_YYMonDD EQU   *-1,X'10'    ...1 .... Input in YYYY,Month,DD 00952014
DConv_Input_SMFDate EQU   *-1,X'08'    .... 1... Input in SMFDate       00953014
DConv_Input_Time    EQU   *-1,X'02'    .... ..1. Input in Time          00954014
DConv_Input_hhmmss  EQU   *-1,X'01'    .... ...1 Input in hh, mm, ss    00955014
                                                                        00956014
DConv_Date_SMFDate  DS    PL4         SMF Date Stamp                    00957014
DConv_Date_YYYY     DS    HL2         Year (including century)          00958014
DConv_Date_DDD      DS    HL2         Day-of-year                       00959014
DConv_Date_Base     DS    FL4         Days since Jan1,0001 (a la REXX)  00960014
DConv_Date_MM       DS    XL1         Month number (1-12)               00961014
DConv_Date_DD       DS    XL1         Day-of-month (1-31)               00962014
DConv_Date_Month    DS    CL9         Month name (January, February ..) 00963014
DConv_Date_Day      DS    CL9         Day Name (Monday, Tuesday, etc.)  00964014
DConv_Date_DOW      DS    XL1         Day-of-week (Mon=0, Tue=1, etc.)  00965014
                                                                        00966014
DConv_Time          DS    FL4         Time (secs*100 since midnight)    00967014
DConv_Time_hh       DS    XL1         Hours       (0-23)                00968014
DConv_Time_mm       DS    XL1         Minutes     (0-59)                00969014
DConv_Time_ss       DS    XL1         Seconds     (0-59)                00970014
DConv_Time_tt       DS    XL1         Seconds/100 (0-99)                00971014
DConv_Area_Length   EQU   *-DateConv_Area  Length of area               00972014
                                                                        00973014
         TITLE 'Global Dynamic Storage'                                 00974014
@_Dynam  DSECT                                                          00975014
                                                                        00976014
@_31Bit_Stack_Ptr   DS    AL4         A(Stack storage above-the-line)   00977014
@_31Bit_Stack_Len   EQU   4096-StackHdr_Length  4K Stack                00978014
                                                                        00979014
@_24Bit_Stack_Ptr   DS    AL4         A(Stack storage below-the-line)   00980014
@_24Bit_Stack_Len   EQU   2048-StackHdr_Length  2K Stack                00981014
                                                                        00982014
*-------------------------------------------------------------------*   00983014
*                                                                   *   00984014
*    SYSPRINT-related info                                          *   00985014
*                                                                   *   00986014
*-------------------------------------------------------------------*   00987014
                                                                        00988014
@_SYSPRINT_DCB_Ptr  DS    AL4         A(SYSPRINT DCB)                   00989014
@_SYSPRINT_Rec_Ptr  DS    AL4         A(SYSPRINT RECORD)                00990014
@_SYSPRINT_Rec_Len  DS    HL2         SYSPRINT RECORD LENGTH (LESS ASA) 00991014
@_SYSPRINT_Y0010_Ptr DS   AL4         A(Copy of Y0010)                  00992014
                                                                        00993014
*-------------------------------------------------------------------*   00994014
*                                                                   *   00995014
*    SYSUT1-related info                                            *   00996014
*                                                                   *   00997014
*-------------------------------------------------------------------*   00998014
                                                                        00999014
                    DS    0F                                            01000014
@_SYSUT1_DCB_Ptr    DS    AL4         A(SYSUT1 DCB)                     01001014
                    ORG   @_SYSUT1_DCB_Ptr                              01002014
@_SYSUT1_ACB_Ptr    DS    AL4         A(SYSUT1 ACB)                     01003014
                    ORG   ,                                             01004014
@_SYSUT1_Rec_Ptr    DS    AL4         A(Input record)                   01005014
@_SYSUT1_AtL_Ptr    DS    AL4         A(Above-the-line storage)         01006014
@_SYSUT1_BtL_Ptr    DS    AL4         A(Below-the-line storage)         01007014
@_SYSUT1_Rec_Length DS    FL4         Input record length               01008014
@_SYSUT1_BuffArea_Ptr DS  AL4         A(Current buffer area - BSAM)     01009014
@_SYSUT1_PBlock_Count DS  FL4         Physical Block Count - BSAM       01010014
                    ORG   @_SYSUT1_BuffArea_Ptr                         01011014
@_SYSUT1_RPL_Ptr    DS    AL4         A(SYSUT1 RPL - VSAM)              01012014
                    ORG   ,                                             01013014
                                                                        01014014
                    DS    X           Flag byte                         01015014
@_SYSUT1_VSAM       EQU   *-1,X'80'    1... .... SYSUT1:  VSAM          01016014
@_SYSUT1_QSAM       EQU   *-1,X'40'    .1.. .... SYSUT1:  QSAM          01017014
@_SYSUT1_BSAM       EQU   *-1,X'20'    ..1. .... SYSUT1:  BSAM          01018014
@_SYSUT1_Sequence   EQU   *-1,X'10'    ...1 .... Input is in sequence   01019014
@_SYSUT1_NoVBSChk   EQU   *-1,X'08'    .... 1... No VBS checking        01020014
@_SYSUT1_Seg_Error  EQU   *-1,X'01'    .... ...1 Segments dropped       01021014
                                                                        01022014
*-------------------------------------------------------------------*   01023014
*                                                                   *   01024014
*    Output-related fields                                          *   01025014
*                                                                   *   01026014
*-------------------------------------------------------------------*   01027014
                                                                        01028014
                    DS    0F                                            01029014
@_ODT_Ptr           DS    AL4         A(Output Descriptor Table)        01030014
                                                                        01031014
*-------------------------------------------------------------------*   01032014
*                                                                   *   01033014
*    Counters and Date/Times                                        *   01034014
*                                                                   *   01035014
*-------------------------------------------------------------------*   01036014
                                                                        01037014
                    DS    0F                                            01038014
@_Line_Count        DS    HL2         LINE COUNT                        01039014
@_Page_Count        DS    PL2         PAGE COUNT                        01040014
@_Input_Count       DS    FL4         INPUT COUNTER                     01041014
                                                                        01042014
@_Dump_Start_Date   DS    PL4         SMF Dump Start Date               01043014
@_Dump_Start_Time   DS    FL4                    and Time               01044014
@_Dump_End_Date     DS    PL4         SMF Dump End Date                 01045014
@_Dump_End_Time     DS    FL4                  and Time                 01046014
@_First_Rec_Date    DS    PL4         First Record Date                 01047014
@_First_Rec_Time    DS    FL4                  and Time                 01048014
@_Input_Rec_Date    DS    PL4         Current Record Date               01049014
@_Input_Rec_Time    DS    FL4                    and Time               01050014
                                                                        01051014
@_Prev_Input_Rec_Date DS  PL4         Previous record date              01052014
@_Prev_Input_Rec_DOW DS   X           Previous record Day-of-Week       01053014
                                                                        01054014
@_DateConv_Area     DS    CL(DConv_Area_Length)  Todays Date Info       01055014
                                                                        01056014
@_Input_Count_Table DS    256FL4      Input count by SMF Record Type    01057014
                                                                        01058014
         DS    0D                     ALIGNMENT                         01059014
@_DynLen EQU   *-@_Dynam               LENGTH OF STORAGE                01060014
         TITLE 'Register Equates'                                       01061014
*        YREGS                                                          01062014
         REGEQU                                                         01062015
         TITLE 'A0010: Main Program Block'                              01063014
SMFSLCT  RMODE ANY                                                      01064014
SMFSLCT  AMODE 31                                                       01065014
                                                                        01066014
SMFSLCT  RSECT                                                          01067014
                                                                        01068014
*---------------------------------------------------------------------* 01069014
*                                                                     * 01070014
*  Routine   : A0010                                                  * 01071014
*                                                                     * 01072014
*  Abstract  : Main program loop:                                     * 01073014
*               Perform Initialization                                * 01074014
*               Do until SYSUT1 EOF, or all criteria satisfied        * 01075014
*                 Read SYSIN                                          * 01076014
*                 Do for each appropriate output                      * 01077014
*                   Write to output                                   * 01078014
*                 EndDo                                               * 01079014
*               Perform Termination                                   * 01080014
*                                                                     * 01081014
*                                                                     * 01082014
*  Inputs    : Files:                                                 * 01083014
*               SYSUT1  : Input SMF File (Dynamically allocated if    * 01084014
*                                         not present in JCL)         * 01085014
*               SYSIN   : Control Cards                               * 01086014
*               SYSPRINT: Listing                                     * 01087014
*              Parms: N/A                                             * 01088014
*                                                                     * 01089014
*                                                                     * 01090014
*  Outputs   : R15: 0 - OK                                            * 01091014
*                   8 - Error encountered                             * 01092014
*                                                                     * 01093014
*  Notes     :                                                        * 01094014
*                                                                     * 01095014
*  History:                                                           * 01096014
*              1999/04/24 SDDA030 V1.1                                * 01097014
*                                 Used LIFO Stack for our dynamic     * 01098014
*                                 areas.                              * 01099014
*              ____/__/__ _______                                     * 01100014
*                                                                     * 01101014
*---------------------------------------------------------------------* 01102014
                                                                        01103014
A0010    DS    0H                                                       01104014
         USING *,R15                  ASSIGN TEMPORARY BASE             01105014
         SAVE  (14,12),T,'SMFSLCT &VERSION &ASMDT &SYSTIME &CPERSON'    01106029
*                                                       @RWS 15-11-13   01107032
         LR    R11,R15                  Load permanent base reg         01108014
         DROP  R15                      Free up temp base               01109014
         USING A0010,R11                Assign permanent base           01110014
                                                                        01111014
*-------------------------------------------------------------------*   01112014
*                                                                   *   01113014
*    Get some storage for our LIFO stack ...                        *   01114014
*                                                                   *   01115014
*-------------------------------------------------------------------*   01116014
                                                                        01117014
         L     R0,=AL4(@_31Bit_Stack_Len) Get length of storage we need 01118014
         STACK INIT,                    Go get our storage             +01119014
               LEN=(R0),                 this long                     +01120014
               LOC=ANY                   anywhere                       01121014
         LR    R2,R1                    Save its address                01122014
                                                                        01123014
*-------------------------------------------------------------------*   01124014
*                                                                   *   01125014
*    ... and get some room on it for the globally addressable       *   01126014
*     variables ...                                                 *   01127014
*                                                                   *   01128014
*-------------------------------------------------------------------*   01129014
                                                                        01130014
         STACK PUSH,                    Get Stack area                 +01131014
               LEN=@_DynLen,             this long                     +01132014
               STACK=(R2)                using this stack               01133014
         LR    R12,R1                   Point to our storage            01134014
                                                                        01135014
         USING @_Dynam,R12              Assign a base                   01136014
                                                                        01137014
         LA    R0,@_Dynam               A(Our storage)                  01138014
         LA    R1,@_DynLen              Its length                      01139014
         SLR   R14,R14                  Clear source address            01140014
         SLR   R15,R15                   and length                     01141014
         MVCL  R0,R14                   Clear our storage               01142014
                                                                        01143014
         ST    R2,@_31Bit_Stack_Ptr     Save A(Stack)                   01144014
                                                                        01145014
*-------------------------------------------------------------------*   01146014
*                                                                   *   01147014
*    ... and finally get an area on it for our local storage        *   01148014
*                                                                   *   01149014
*-------------------------------------------------------------------*   01150014
                                                                        01151014
         STACK PUSH,                    Get Stack area                 +01152014
               LEN=@A_DynLen,            this long                     +01153014
               STACK=@_31Bit_Stack_Ptr   using this stack               01154014
                                                                        01155014
         LR    R2,R1                    Save its address                01156014
         LR    R0,R1                    A(Our storage)                  01157014
         LA    R1,@A_DynLen              Its length                     01158014
         SLR   R14,R14                  Clear source address            01159014
         SLR   R15,R15                   and length                     01160014
         MVCL  R0,R14                   Clear our storage               01161014
         LR    R1,R2                    Restore A(Our storage)          01162014
                                                                        01163014
*-------------------------------------------------------------------*   01164014
*                                                                   *   01165014
*    Chain our save areas, and restore the important registers      *   01166014
*   that we have destroyed                                          *   01167014
*                                                                   *   01168014
*-------------------------------------------------------------------*   01169014
                                                                        01170014
         ST    R13,4(0,R1)              Chain                           01171014
         ST    R1,8(0,R13)               saveareas                      01172014
         LR    R13,R1                   Load dynam base                 01173014
         USING @A_DYNAM,R13             Assign a base                   01174014
         L     R15,@A_Dynam+4           Get A(HSA)                      01175014
         LM    R0,R3,20(R15)            Restore callers registers       01176014
                                                                        01177014
*-------------------------------------------------------------------*   01178014
*                                                                   *   01179014
*    Get the 24-bit stack                                           *   01180014
*                                                                   *   01181014
*-------------------------------------------------------------------*   01182014
                                                                        01183014
         L     R0,=AL4(@_24Bit_Stack_Len) Get length of storage we need 01184014
         STACK INIT,                    Go get our storage             +01185014
               LEN=(R0),                 this long                     +01186014
               LOC=BELOW                 below-the-line                 01187014
         ST    R1,@_24Bit_Stack_Ptr     Save its address                01188014
                                                                        01189014
         L     R15,@A_Dynam+4           Get A(HSA)                      01190014
         LM    R0,R1,20(R15)            Restore callers registers       01191014
                                                                        01192014
*-------------------------------------------------------------------*   01193014
*                                                                   *   01194014
*    Perform our Initialization Routines                            *   01195014
*                                                                   *   01196014
*-------------------------------------------------------------------*   01197014
                                                                        01198014
         L     R15,=AL4(B0010)        A(Initialization Routine)         01199014
         BASR  R14,R15                Go do it                          01200014
         LTR   R15,R15                How did it go?                    01201014
         BNZ   A9000                  Not so well, exit                 01202014
                                                                        01203014
A1000    DS    0H                                                       01204014
                                                                        01205014
*-------------------------------------------------------------------*   01206014
*                                                                   *   01207014
*    Start of main program loop:                                    *   01208014
*      Get an input record                                          *   01209014
*                                                                   *   01210014
*-------------------------------------------------------------------*   01211014
                                                                        01212014
         L     R15,=AL4(C0010)        A(SYSUT1 Read routine)            01213014
         BASR  R14,R15                Go do it                          01214014
         LTR   R15,R15                How did it go?                    01215014
         BNZ   A9000                  Not so well, exit                 01216014
         L     R1,@_SYSUT1_Rec_Ptr    OK, get A(Input Record)           01217014
                                                                        01218014
*-------------------------------------------------------------------*   01219014
*                                                                   *   01220014
*    Indicate that we haven't printed the record yet, in case it    *   01221014
*   is selected more than once.                                     *   01222014
*                                                                   *   01223014
*-------------------------------------------------------------------*   01224014
                                                                        01225014
         NI    @A_Rec_Printed,X'FF'-L'@A_Rec_Printed OFF Printed Flag   01226014
                                                                        01227014
*-------------------------------------------------------------------*   01228014
*                                                                   *   01229014
*    Go through the Output Descriptor Table, seeing if this record  *   01230014
*   will satisfy any of the criteria.                               *   01231014
*                                                                   *   01232014
*-------------------------------------------------------------------*   01233014
                                                                        01234014
         L     R10,@_ODT_Ptr          A(ENVIRONMENT TABLE)              01235014
         LH    R9,ODT_Entry_Count-OutDesc_Tbl(R10) NBR ENTRIES          01236014
         LA    R10,ODT_Entry-OutDesc_Tbl(R10) AND POINT TO FIRST        01237014
         USING ODT_Entry,R10           and assign a base                01238014
         OI    @A_All_Done,L'@A_All_Done Assume all ODTEs inactive      01239014
A1010    DS    0H                                                       01240014
                                                                        01241014
*-------------------------------------------------------------------*   01242014
*                                                                   *   01243014
*    If this is an active entry ...                                 *   01244014
*                                                                   *   01245014
*-------------------------------------------------------------------*   01246014
                                                                        01247014
         TM    ODTE_Inactive,L'ODTE_Inactive ACTIVE ENTRY?              01248014
         BO    A1250                   NO, SKIP                         01249014
         NI    @A_All_Done,X'FF'-L'@A_All_Done Reset flag               01250014
                                                                        01251014
*-------------------------------------------------------------------*   01252014
*                                                                   *   01253014
*    ... check the date and time of the input record against the    *   01254014
*   start and end date/time criteria.                               *   01255014
*                                                                   *   01256014
*-------------------------------------------------------------------*   01257014
                                                                        01258014
         TM    ODTE_CopyOnly,L'ODTE_CopyOnly Any selection criteria?    01259014
         BO    A1200                  No, select the record             01260014
         CLI   SMF0RTY-SMFRCD0(R1),X'02' DUMP HEADER?                   01261014
         BE    A1040                  YES, SKIP DATE/TIME CHECK         01262014
         CLI   SMF0RTY-SMFRCD0(R1),X'03' NO, DUMP TRAILER?              01263014
         BE    A1040                  YES, SKIP DATE/TIME CHECK         01264014
         CP    SMF0DTE-SMFRCD0(L'SMF0DTE,R1),ODTE_StartDate CHECK DATE  01265014
         BL    A1250                  DON'T WANT THIS ONE, SKIP         01266014
         BH    A1020                  AFTER START DATE, SKIP TIME CHK   01267014
         L     R15,SMF0TME-SMFRCD0(R1) GET INPUT TIME                   01268014
         C     R15,ODTE_StartTime     DO WE WANT THIS ONE?              01269014
         BL    A1250                  NO, GET ANOTHER ONE               01270014
A1020    DS    0H                                                       01271014
         CP    SMF0DTE-SMFRCD0(L'SMF0DTE,R1),ODTE_EndDate PAST END?     01272014
         BH    A1030                  YES, CHECK SEQUENCE               01273014
         BL    A1040                  BEFORE END DATE, SKIP TIME CHK    01274014
         L     R15,SMF0TME-SMFRCD0(R1) GET INPUT TIME                   01275014
         C     R15,ODTE_EndTime       BEFORE END TIME?                  01276014
         BNH   A1040                  YES, CONTINUE                     01277014
A1030    DS    0H                                                       01278014
         TM    @_SYSUT1_Sequence,L'@_SYSUT1_Sequence Input in sequence? 01279014
         BO    A1240                  YES, INACTIVATE THIS ENTRY        01280014
         B     A1250                  NO, IGNORE THIS RECORD            01281014
A1040    DS    0H                                                       01282014
                                                                        01283014
*-------------------------------------------------------------------*   01284014
*                                                                   *   01285014
*    If we are ignoring weekends, check it out                      *   01286014
*                                                                   *   01287014
*-------------------------------------------------------------------*   01288014
                                                                        01289014
         TM    ODTE_Ignore_Weekend,L'ODTE_Ignore_Weekend                01290014
         BZ    A1060                  No, continue                      01291014
         CP    @_Prev_Input_Rec_Date,SMF0DTE-SMFRCD0(L'SMF0DTE,R1)      01292014
         BE    A1050                  Same date as prev, skip           01293014
         XC    @A_DateConv_Area,@A_DateConv_Area No, clear Date area    01294014
         MVC   @A_DateConv_Area+DConv_Date_SMFDate-DateConv_Area(L'DCon+01295014
               v_Date_SMFDate),SMF0DTE-SMFRCD0(R1) Move SMF Date        01296014
         OI    @A_DateConv_Area+DConv_Input_SMFDate-DateConv_Area,L'DCo+01297014
               nv_Input_SMFDate      Indicate SMF Date                  01298014
         LA    R1,@A_DateConv_Area   Point to Date Conversion area      01299014
         L     R15,=AL4(K0010)       A(Format routine)                  01300014
         BASR  R14,R15               Go get Day-of-Week                 01301014
         L     R1,@_SYSUT1_Rec_Ptr   Restore A(Input Record)            01302014
         ZAP   @_Prev_Input_Rec_Date,SMF0DTE-SMFRCD0(L'SMF0DTE,R1)      01303014
         MVC   @_Prev_Input_Rec_DOW,@A_DateConv_Area+DConv_Date_DOW-Dat+01304014
               eConv_Area            Save Day-of-week                   01305014
A1050    DS    0H                                                       01306014
         CLI   @_Prev_Input_Rec_DOW,X'04' Is DOW greater than Friday?   01307014
         BH    A1250                 Yes, ignore this record            01308014
A1060    DS    0H                                                       01309014
*-------------------------------------------------------------------*   01310014
*                                                                   *   01311014
*    Start and End Date/Times are OK, check against the Period      *   01312014
*                                                                   *   01313014
*-------------------------------------------------------------------*   01314014
                                                                        01315014
         L     R15,SMF0TME-SMFRCD0(R1) GET THE TIME OF THE SMF REC      01316014
         C     R15,ODTE_PeriodStart   BEFORE PERIOD START?              01317014
         BL    A1250                  YES, IGNORE IT                    01318014
         C     R15,ODTE_PeriodEnd     NO, AFTER PERIOD END?             01319014
         BH    A1250                  YES, SKIP IT                      01320014
                                                                        01321014
*-------------------------------------------------------------------*   01322014
*                                                                   *   01323014
*    Invoke the User Exit INEXIT, if there is one.                  *   01324014
*                                                                   *   01325014
*-------------------------------------------------------------------*   01326014
                                                                        01327014
         ICM   R15,B'1111',ODTE_InExit_Ptr Get A(Input Exit)            01328014
         BZ    A1070                  None, skip                        01329014
         SLR   R0,R0                  Yes, flag as an Inexit            01330014
         L     R15,=AL4(X0010)        A(Exit invocation routine)        01331014
         BASR  R14,R15                Go do it                          01332014
         LTR   R15,R15                Check return code                 01333014
         BZ    A1070                  OK, continue normally             01334014
         CH    R15,=H'8'              Something else, check it out      01335014
         BL    A1200                  Select this record                01336014
         BE    A1250                  Ignore it                         01337014
         ZAP   ODTE_EndDate,=P'0'     Clear End Date and Time so we     01338014
         XC    ODTE_EndTime,ODTE_EndTime finish after this record       01339014
A1070    DS    0H                                                       01340014
                                                                        01341014
*-------------------------------------------------------------------*   01342014
*                                                                   *   01343014
*    Check this record against any record types specified ...       *   01344014
*                                                                   *   01345014
*-------------------------------------------------------------------*   01346014
                                                                        01347014
         ICM   R15,B'1111',ODTE_RTT_Ptr Get A(Record Table)             01348014
         BZ    A1130                  None, don't check it              01349014
         LH    R14,RTT_Entry_Count-RecType_Tbl(R15) Get nbr entries     01350014
         LA    R15,RTT_Entry-RecType_Tbl(R15) and point to the first    01351014
         SLR   R4,R4                  Clear R4                          01352014
         SLR   R5,R5                  R5 too                            01353014
A1080    DS    0H                                                       01354014
         CLC   SMF0RTY-SMFRCD0(L'SMF0RTY,R1),RTTE_RecType-RTT_Entry(R15+01355014
               ) Is this our record?                                    01356014
         BE    A1100                  Yes, skip                         01357014
A1090    DS    0H                                                       01358014
         LA    R15,RTTE_Length(R15)   No, A(Next entry in table)        01359014
         BCT   R14,A1080              and try again                     01360014
         B     A1120                  Not our record, skip              01361014
A1100    DS    0H                                                       01362014
         TM    RTTE_SubType_Present-RTT_Entry(R15),L'RTTE_SubType_Prese+01363014
               nt                     SubType present?                  01364014
         BNO   A1110                  No, found our record              01365014
         CLC   SMF30STP-SMFRCD30(L'SMF30STP,R1),RTTE_SubType-RTT_Entry(+01366014
               R15)                   Yes, our subtype?                 01367014
         BNE   A1090                  No, try again                     01368014
A1110    DS    0H                                                       01369014
                                                                        01370014
*-------------------------------------------------------------------*   01371014
*                                                                   *   01372014
*    ... and include or exclude it as required                      *   01373014
*                                                                   *   01374014
*-------------------------------------------------------------------*   01375014
                                                                        01376014
         L     R15,ODTE_RTT_Ptr       Get A(Record Table)               01377014
         TM    RTT_Entry_Exclude-RecType_Tbl(R15),L'RTT_Entry_Exclude   01378014
         BO    A1250                  EXCLUDEing, skip                  01379014
         B     A1130                  No, continue                      01380014
A1120    DS    0H                                                       01381014
                                                                        01382014
*-------------------------------------------------------------------*   01383014
*                                                                   *   01384014
*    This record is not in the Record Type Table - we must check the*   01385014
*    exclude indicator to see what we should do with it.            *   01386014
*                                                                   *   01387014
*-------------------------------------------------------------------*   01388014
                                                                        01389014
         L     R15,ODTE_RTT_Ptr       Get A(Record Table)               01390014
         TM    RTT_Entry_Exclude-RecType_Tbl(R15),L'RTT_Entry_Exclude   01391014
         BO    A1130                  Yes, include this record          01392014
         B     A1250                  No, skip it                       01393014
A1130    DS    0H                                                       01394014
                                                                        01395014
*-------------------------------------------------------------------*   01396014
*                                                                   *   01397014
*    Check this record for JobName selection.                       *   01398014
*                                                                   *   01399014
*-------------------------------------------------------------------*   01400014
                                                                        01401014
         ICM   R15,B'1111',ODTE_JNT_Ptr A(JobName Table)                01402014
         BZ    A1170                  None, skip                        01403014
*        CLI   SMF30RTY-SMFRCD30(R1),70 RMF record?                     01404014
*        BL    A1140                  No, skip                          01405014
*        CLI   SMF30RTY-SMFRCD30(R1),79 Really?                         01406014
*        BNH   A1170                  Yes, skip jobname check           01407014
A1140    DS    0H                                                       01408014
         LH    R14,JNT_Entry_Count-JobName_Tbl(R15) Nbr entries         01409014
         LA    R15,JNT_Entry-JobName_Tbl(R15) And point to first        01410014
         LA    R2,SMF5JBN-SMFRCD5(R1) A(JobName in SMF record)          01411014
         CLI   SMF30RTY-SMFRCD30(R1),30 Type 30 record?                 01412014
         BNE   A1150                  No, skip                          01413014
         L     R2,SMF30IOF-SMFRCD30(R1) Yes, offset to ID section       01414014
         AR    R2,R1                  and point to JobName              01415014
A1150    DS    0H                                                       01416014
         SLR   R3,R3                   Clear EX register                01417014
         IC    R3,JNTE_JobName_Len-JNT_Entry(R15) Len of JobName Mask   01418014
         EX    R3,A9999               Check JobNameE                    01419014
         BNE   A1160                  Not us, skip                      01420014
         TM    ODTE_JobData_OR,L'ODTE_JobData_OR JOB/DATA=OR?           01421014
         BO    A1200                  Yes, Select this record           01422014
         B     A1170                  No, go check Data Table           01423014
A1160    DS    0H                                                       01424014
         LA    R15,JNTE_Length(R15)   A(Next Entry)                     01425014
         BCT   R14,A1150              and try again                     01426014
         TM    ODTE_JobData_OR,L'ODTE_JobData_OR JOB/DATA=OR?           01427014
         BNO   A1250                  No, skip this record              01428014
         ICM   R15,B'1111',ODTE_DT_Ptr Yes, get A(Data Table)           01429014
         BZ    A1250                  None, skip this record            01430014
A1170    DS    0H                                                       01431014
                                                                        01432014
*-------------------------------------------------------------------*   01433014
*                                                                   *   01434014
*    Check for Data selection                                       *   01435014
*                                                                   *   01436014
*-------------------------------------------------------------------*   01437014
                                                                        01438014
         ICM   R15,B'1111',ODTE_DT_Ptr Get A(Data Table)                01439014
         BZ    A1200                  None, select this record          01440014
         LH    R14,DT_Entry_Count-Data_Tbl(R15) Yes, get nbr entries    01441014
         LA    R15,DT_Entry-Data_Tbl(R15) and point to the first        01442014
         SLR   R3,R3                  Clear work register               01443014
A1180    DS    0H                                                       01444014
         LH    R2,DTE_Offset-DT_Entry(R15) Offset of data               01445014
         IC    R3,DTE_Length-DT_Entry(R15) EX length if data            01446014
         LA    R4,0(R2,R3)            Offset of last char               01447014
         CH    R4,SMF0LEN-SMFRCD0(R1) Is the record long enough?        01448014
         BH    A1190                  No, data does not match           01449014
         AR    R2,R1                  Yes, A(Offset)                    01450014
         EX    R3,A9998               Check the data                    01451014
         BE    A1200                  Found it, skip                    01452014
A1190    DS    0H                                                       01453014
         LA    R15,1+DTE_Data-DT_Entry(R3,R15) A(Next Entry)            01454014
         BCT   R14,A1180              and check it                      01455014
         B     A1250                  Not found, skip this record       01456014
A1200    DS    0H                                                       01457014
                                                                        01458014
*-------------------------------------------------------------------*   01459014
*                                                                   *   01460014
*    This record has satisfied all the selection criteria for this  *   01461014
*   output file, so invoke the user exit                            *   01462014
*                                                                   *   01463014
*-------------------------------------------------------------------*   01464014
                                                                        01465014
         ICM   R15,B'1111',ODTE_OutExit_Ptr Get A(Output Exit)          01466014
         BZ    A1210                  None, skip                        01467014
         LA    R0,1                   Yes, flag as an Outexit           01468014
         L     R15,=AL4(X0010)        A(Exit invocation routine)        01469014
         BASR  R14,R15                Go do it                          01470014
         CH    R15,=H'8'              Check return code                 01471014
         BH    A1240                  Inactivate this ODT entry         01472014
         BE    A1250                  Ignore the record                 01473014
                                                                        01474014
A1210    DS    0H                                                       01475014
                                                                        01476014
*-------------------------------------------------------------------*   01477014
*                                                                   *   01478014
*    Invoke D0010 to write to the output file ...                   *   01479014
*                                                                   *   01480014
*-------------------------------------------------------------------*   01481014
                                                                        01482014
         ST    R1,@A_D0010_Parm       Save A(SMF record)                01483014
         ST    R10,@A_D0010_Parm+4    Save A(ODT Entry)                 01484014
                                                                        01485014
         LA    R1,@A_D0010_Parm       A(Parm list)                      01486014
         L     R15,=AL4(D0010)        A(Selection routine)              01487014
         BASR  R14,R15                Go select the record              01488014
                                                                        01489014
         L     R1,@_SYSUT1_Rec_Ptr    Restore A(Input Record)           01490014
                                                                        01491014
         LTR   R15,R15                How did it go?                    01492014
         BNZ   A1250                  Not great, try next ODT           01493014
                                                                        01494014
         ICM   R15,B'1111',ODTE_StopAft Get Select Limit                01495014
         BZ    A1220                  None, keep going                  01496014
         C     R15,ODTE_Selected      Yes, have we reached it?          01497014
         BH    A1220                  Not yet, keep going               01498014
         OI    ODTE_Inactive,L'ODTE_Inactive Inactive entry             01499014
A1220    DS    0H                                                       01500014
                                                                        01501014
                                                                        01502014
*-------------------------------------------------------------------*   01503014
*                                                                   *   01504014
*    ... and E0010 to print it, if required                         *   01505014
*                                                                   *   01506014
*-------------------------------------------------------------------*   01507014
                                                                        01508014
         TM    ODTE_Print_NO,L'ODTE_Print_NO Should we print?           01509014
         BO    A1250                  No, skip                          01510014
         TM    @A_Rec_Printed,L'@A_Rec_Printed Yes, already printed?    01511014
         BO    A1250                  Yes, skip                         01512014
                                                                        01513014
         L     R15,=AL4(G0010)        A(Print routine)                  01514014
         BASR  R14,R15                Skip a line (Print blanks)        01515014
                                                                        01516014
         L     R2,@_SYSPRINT_Rec_Ptr  Get A(SYSPRINT record)            01517014
         MVC   0(7,R2),=X'4020206B202120'  Move mask                    01518014
         L     R15,@_Input_Count      Get input record count            01519014
         CVD   R15,@A_DBLWD           Pack it                           01520014
         ED    0(7,R2),@A_DBLWD+5     Edit it                           01521014
         MVC   7(7,R2),=X'4020206B202120'  Move mask                    01522014
         L     R15,ODTE_Selected      Get selected count                01523014
         CVD   R15,@A_DBLWD           Pack it                           01524014
         ED    7(7,R2),@A_DBLWD+5     Edit it                           01525014
         MVC   15(8,R2),=C'RecType:'  Move Record Type title            01526014
         MVC   23(4,R2),=X'40202120'  Move mask                         01527014
         SLR   R15,R15                Clear work register               01528014
         IC    R15,SMF0RTY-SMFRCD0(R1) Get record type                  01529014
         CVD   R15,@A_DBLWD           Pack it                           01530014
         ED    23(4,R2),@A_DBLWD+6     and put it in SYSPRINT rec       01531014
         MVC   30(11,R2),=C'Date-Time: ' Move Date/Time heading         01532014
                                                                        01533014
         XC    @A_DateConv_Area,@A_DateConv_Area Clear Date area        01534014
         ZAP   @A_DateConv_Area+DConv_Date_SMFDate-DateConv_Area(L'DCon+01535014
               v_Date_SMFDate),SMF0DTE-SMFRCD0(L'SMF0DTE,R1)  Date      01536014
         MVC   @A_DateConv_Area+DConv_Time-DateConv_Area(L'DConv_Time),+01537014
               SMF0TME-SMFRCD0(R1)            Time too                  01538014
         OI    @A_DateConv_Area+DConv_Input_SMFDate-DateConv_Area,L'DCo+01539014
               nv_Input_SMFDate     Indicate SMF Date                   01540014
         OI    @A_DateConv_Area+DConv_Input_Time-DateConv_Area,L'DConv_+01541014
               Input_Time           Indicate SMF Time                   01542014
         LA    R15,@A_DateConv_Area     Point to Date Conversion area   01543014
         ST    R15,@A_F0010_Parm     Save as 1st parm                   01544014
         LA    R15,41(0,R2)          A(Output area)                     01545014
         ST    R15,@A_F0010_Parm+4   Save as 2nd parm                   01546014
         LA    R15,@A_Return_Ptr     A(Return area)                     01547014
         ST    R15,@A_F0010_Parm+8   Save as 2nd parm                   01548014
         LA    R1,@A_F0010_Parm      A(Parm pointers)                   01549014
         L     R15,=AL4(F0010)       A(Format routine)                  01550014
         BASR  R14,R15               Go convert date and time           01551014
                                                                        01552014
         L     R1,@_SYSUT1_Rec_Ptr    Restore A(Input Record)           01553014
         L     R15,@_ODT_Ptr          A(OUTPUT ENVIRONMENT TABLE)       01554014
         LH    R15,ODT_Entry_Count-OutDesc_Tbl(R15) GET NBR ENTRIES     01555014
         CH    R15,=H'1'              MORE THAN 1?                      01556014
         BNH   A1230                  NO, SKIP 'SELECTED BY' MSG        01557014
         L     R2,@A_Return_Ptr       Point to end of Date/Time         01558014
         MVC   1(13,R2),=C', selected to'  MOVE SELECTION HDG           01559014
         MVC   15(L'ODTE_DDName,R2),ODTE_DDName MOVE DDNAME             01560014
A1230    DS    0H                                                       01561014
         L     R15,=AL4(G0010)        A(Print routine)                  01562014
         BASR  R14,R15                PRINT THIS LINE                   01563014
                                                                        01564014
         ST    R1,@A_E0010_Parm       Save A(SMF record)                01565014
         ST    R10,@A_E0010_Parm+4    Save A(ODT Entry)                 01566014
                                                                        01567014
         LA    R1,@A_E0010_Parm       A(Parm list)                      01568014
         L     R15,=AL4(E0010)        A(Print routine)                  01569014
         BASR  R14,R15                Go print the record               01570014
                                                                        01571014
         OI    @A_Rec_Printed,L'@A_Rec_Printed Printed now              01572014
                                                                        01573014
         L     R1,@_SYSUT1_Rec_Ptr    Restore A(Input Record)           01574014
                                                                        01575014
         B     A1250                  And continue                      01576014
                                                                        01577014
A1240    DS    0H                                                       01578014
                                                                        01579014
*-------------------------------------------------------------------*   01580014
*                                                                   *   01581014
*    Flag this ODT Entry as Inactive                                *   01582014
*                                                                   *   01583014
*-------------------------------------------------------------------*   01584014
                                                                        01585014
         OI    ODTE_Inactive,L'ODTE_Inactive Inactive entry             01586014
A1250    DS    0H                                                       01587014
                                                                        01588014
*-------------------------------------------------------------------*   01589014
*                                                                   *   01590014
*    Point to the next ODT Entry, and check it out                  *   01591014
*                                                                   *   01592014
*-------------------------------------------------------------------*   01593014
                                                                        01594014
         AL    R10,=AL4(ODTE_Length)  Bump entry pointer                01595014
         BCT   R9,A1010               and keep going                    01596014
                                                                        01597014
*-------------------------------------------------------------------*   01598014
*                                                                   *   01599014
*    We have finished processing all ODT Entries - if they are all  *   01600014
*   inactive, we will terminate.                                    *   01601014
*                                                                   *   01602014
*-------------------------------------------------------------------*   01603014
                                                                        01604014
         TM    @A_All_Done,L'@A_All_Done All ODTs inactive?             01605014
         BZ    A1000                  No, go get more input             01606014
                                                                        01607014
         DROP  R10                    Free ODTE Base register           01608014
                                                                        01609014
         SLR   R15,R15                Yes, all done                     01610014
         B     A9000                  and exit                          01611014
A9000    DS    0H                                                       01612014
                                                                        01613014
*-------------------------------------------------------------------*   01614014
*                                                                   *   01615014
*    Perform our Termination routines ...                           *   01616014
*                                                                   *   01617014
*-------------------------------------------------------------------*   01618014
                                                                        01619014
         L     R15,=AL4(Z0010)        A(Termination Routine)            01620014
         BASR  R14,R15                Go do it                          01621014
                                                                        01622014
         LR    R3,R15                 Save the return code              01623014
                                                                        01624014
*-------------------------------------------------------------------*   01625014
*                                                                   *   01626014
*    ... get rid of the 24-bit stack ...                            *   01627014
*                                                                   *   01628014
*-------------------------------------------------------------------*   01629014
                                                                        01630014
         STACK TERM,                    Free the stack                 +01631014
               STACK=@_24Bit_Stack_Ptr   starting here                  01632014
                                                                        01633014
*-------------------------------------------------------------------*   01634014
*                                                                   *   01635014
*    ... free up our local data area ...                            *   01636014
*                                                                   *   01637014
*-------------------------------------------------------------------*   01638014
                                                                        01639014
         LA    R1,@A_Dynam              A(Local storage)                01640014
         L     R13,4(0,R13)             Rescue A(HSA)                   01641014
                                                                        01642014
         STACK POP,                     Free the stack area            +01643014
               ADDR=(R1),                starting here                 +01644014
               STACK=@_31Bit_Stack_Ptr   on this stack                  01645014
                                                                        01646014
*-------------------------------------------------------------------*   01647014
*                                                                   *   01648014
*    ... then the global data area ...                              *   01649014
*                                                                   *   01650014
*-------------------------------------------------------------------*   01651014
                                                                        01652014
         LA    R1,@_Dynam               A(Global storage)               01653014
         L     R2,@_31Bit_Stack_Ptr     Rescue Stack pointer            01654014
                                                                        01655014
         STACK POP,                     Free the stack area            +01656014
               ADDR=(R1),                starting here                 +01657014
               STACK=(R2)                on this stack                  01658014
                                                                        01659014
*-------------------------------------------------------------------*   01660014
*                                                                   *   01661014
*    ... and finally, the stack itself.                             *   01662014
*                                                                   *   01663014
*-------------------------------------------------------------------*   01664014
                                                                        01665014
         STACK TERM,                    Free the stack                 +01666014
               STACK=(2)                 starting here                  01667014
                                                                        01668014
*-------------------------------------------------------------------*   01669014
*                                                                   *   01670014
*    Restore callers registers, and return to caller.               *   01671014
*                                                                   *   01672014
*-------------------------------------------------------------------*   01673014
                                                                        01674014
                                                                        01675014
         L     R14,12(0,R13)            Restore Return address          01676014
         LR    R15,R3                   Restore return code             01677014
         LM    R0,R12,20(R13)           Restore remainder of registers  01678014
         BSM   0,R14                    and return to caller            01679014
                                                                        01680014
                                                                        01681014
         LTORG                                                          01682014
                                                                        01683014
                                                                        01684014
A9998 CLC      0(0,R2),DTE_Data-DT_Entry(R15)  Check data               01685014
A9999 CLC      0(0,R2),JNTE_JobName-JNT_Entry(R15) Check JobName        01686014
                                                                        01687014
                                                                        01688014
         LTORG                                                          01689014
                                                                        01690014
@A_Dynam        DSECT               Local Variables                     01691014
@A_Save         DS    18F            O/S Style Save Area                01692014
@A_DBLWD        DS    D                                                 01693014
@A_ParmList     DS    0F                                                01694014
@A_D0010_Parm   DS    2AL4           Parms for D0010                    01695014
                ORG   @A_ParmList                                       01696014
@A_E0010_Parm   DS    2AL4           Parms for E0010                    01697014
                ORG   @A_ParmList                                       01698014
@A_F0010_Parm   DS    3AL4           Parms for F0010                    01699014
@A_Return_Ptr   DS    AL4             Return pointer from F0010         01700014
                ORG                                                     01701014
                DS    X              Flag Byte                          01702014
@A_All_Done     EQU   *-1,X'80'       1... .... All ODTs inactive       01703014
@A_Rec_Printed  EQU   *-1,X'40'       .1.. .... Record printed          01704014
@A_DateConv_Area DS   CL(DConv_Area_Length)  Date conversion area       01705014
                DS    0D             Alignment                          01706014
@A_DynLen       EQU   *-@A_Dynam    Length                              01707014
                                                                        01708014
         DROP  R11,R13                                                  01709014
                                                                        01710014
SMFSLCT  RSECT                                                          01711014
         TITLE 'B0010: Initialization'                                  01712014
                                                                        01713014
         PUSH  USING                                                    01714014
                                                                        01715014
B0010    DS    0H                                                       01716014
                                                                        01717014
*---------------------------------------------------------------------* 01718014
*                                                                     * 01719014
*  Routine   : B0010                                                  * 01720014
*                                                                     * 01721014
*  Abstract  : Initialization routine                                 * 01722014
*               Allocate and initialize various contol blocks         * 01723014
*               Interpret SYSIN Control Cards                         * 01724014
*               OPEN all output files                                 * 01725014
*               Print info about output files                         * 01726014
*                                                                     * 01727014
*                                                                     * 01728014
*  Inputs    : Parms: N/A                                             * 01729014
*                                                                     * 01730014
*                                                                     * 01731014
*  Outputs   : R15: 0 - OK                                            * 01732014
*                   4 - Error encountered                             * 01733014
*                                                                     * 01734014
*  Notes     :                                                        * 01735014
*                                                                     * 01736014
*  History:                                                           * 01737014
*              1999/04/24 SDDA030 V1.1                                * 01738014
*                                 Used LIFO Stack for our dynamic     * 01739014
*                                 areas.                              * 01740014
*              1999/11/18 SDDA030 V1.3                                * 01741014
*                                 Added comments                      * 01742014
*              2002/02/07 SDDA030 V1.4                                * 01743014
*                                 Corrected SOC4s, etc., when we      * 01744014
*                                 overflowed SYSPRINT record when     * 01745014
*                                 listing rectypes/jobnames.          * 01746014
*                                 Invoked I0010 to OPEN SYSUT1.       * 01747014
*                                 Used W0010 to get A(JFCB), rather   * 01748014
*                                 that issue RDJFCB.                  * 01749014
*              ____/__/__ _______                                     * 01750014
*                                                                     * 01751014
*---------------------------------------------------------------------* 01752014
                                                                        01753014
         USING *,R15                                                    01754014
         SAVE  (14,12),T                                                01755014
         LR    R11,R15                  Load permanent base reg         01756014
         DROP  R15                      Free up temp base               01757014
         USING B0010,R11                Assign permanent base           01758014
                                                                        01759014
*-------------------------------------------------------------------*   01760014
*                                                                   *   01761014
*    Get an area on the stack for our local storage                 *   01762014
*                                                                   *   01763014
*-------------------------------------------------------------------*   01764014
                                                                        01765014
         STACK PUSH,                    Get Stack area                 +01766014
               LEN=@B_DynLen,            this long                     +01767014
               STACK=@_31Bit_Stack_Ptr   using this stack               01768014
                                                                        01769014
         LR    R2,R1                    Save its address                01770014
         LR    R0,R1                    A(Our storage)                  01771014
         LA    R1,@B_DynLen              Its length                     01772014
         SLR   R14,R14                  Clear source address            01773014
         SLR   R15,R15                   and length                     01774014
         MVCL  R0,R14                   Clear our storage               01775014
         LR    R1,R2                    Restore A(Our storage)          01776014
                                                                        01777014
*-------------------------------------------------------------------*   01778014
*                                                                   *   01779014
*    Chain our save areas, and restore the important registers      *   01780014
*   that we have destroyed                                          *   01781014
*                                                                   *   01782014
*-------------------------------------------------------------------*   01783014
                                                                        01784014
         ST    R13,4(0,R1)              Chain                           01785014
         ST    R1,8(0,R13)               saveareas                      01786014
         LR    R13,R1                   Load dynam base                 01787014
         USING @B_Dynam,R13             Assign a base                   01788014
         L     R15,@B_Dynam+4           Get A(HSA)                      01789014
         LM    R0,R3,20(R15)            Restore callers registers       01790014
                                                                        01791014
*-------------------------------------------------------------------*   01792014
*                                                                   *   01793014
*    Create our Output Descriptor Table, ...                        *   01794014
*                                                                   *   01795014
*-------------------------------------------------------------------*   01796014
                                                                        01797014
         LA    R0,ODT_Entry-OutDesc_Tbl Header length                   01798014
         AL    R0,=AL4(ODTE_Length)    plus len of 1 entry              01799014
         ICM   R0,B'1000',=AL1(ODT_SubPool_Nbr) set up subpool          01800014
         STORAGE OBTAIN,              Go get our storage               +01801014
               LENGTH=(0),             this long                       +01802014
               LOC=ANY                 anywhere                         01803014
         ST    R1,@_ODT_Ptr            Save A(Table)                    01804014
         ST    R0,ODT_SubPool-OutDesc_Tbl(R1) Save subpool, length      01805014
         LA    R15,1                   Nbr entries                      01806014
         STH   R15,ODT_Entry_Count-OutDesc_Tbl(R1) Init it              01807014
         LA    R0,ODT_Entry-OutDesc_Tbl(R1) Point to our entry          01808014
         L     R1,=AL4(ODTE_Length)    Get its length                   01809014
         SLR   R14,R14                 no sending area                  01810014
         SLR   R15,R15                 or length                        01811014
         MVCL  R0,R14                  Clear our entry                  01812014
         L     R10,@_ODT_Ptr           A(Output Descriptor Table)       01813014
         LA    R10,ODT_Entry-OutDesc_Tbl(R10) Point to 1st entry        01814014
                                                                        01815014
         USING ODT_Entry,R10           Assign a base                    01816014
                                                                        01817014
*-------------------------------------------------------------------*   01818014
*                                                                   *   01819014
*    ... and initialize the base (first) entry.                     *   01820014
*                                                                   *   01821014
*-------------------------------------------------------------------*   01822014
                                                                        01823014
         ZAP   ODTE_StartDate,=P'0'   Start Date = 00000                01824014
         XC    ODTE_StartTime,ODTE_StartTime Start Time = 00:00:00      01825014
         ZAP   ODTE_EndDate,=P'999999' End Date = 00365                 01826014
         MVC   ODTE_EndTime,=F'8640000' End Time = 24:00:00             01827014
         XC    ODTE_PeriodStart,ODTE_PeriodStart Period Start           01828014
         MVC   ODTE_PeriodEnd,=F'8640000' Period End                    01829014
         MVI   ODTE_InExit,C' '       CLEAR                             01830014
         MVC   ODTE_InExit+1(L'ODTE_InExit-1),ODTE_InExit Input and     01831014
         MVC   ODTE_OutExit,ODTE_InExit      Output Exit names          01832014
         SLR   R15,R15                   Clear work register            01833014
         ST    R15,ODTE_DCB_Ptr          No DCB yet                     01834014
                                                                        01835014
*-------------------------------------------------------------------*   01836014
*                                                                   *   01837014
*    Initialize our variables.                                      *   01838014
*                                                                   *   01839014
*-------------------------------------------------------------------*   01840014
                                                                        01841014
         ZAP   @_First_Rec_Date,=P'0' First Date = 00000                01842014
         XC    @_First_Rec_Time,@_First_Rec_Time First Time = 00:00:00  01843014
         ZAP   @_Dump_Start_Date,=P'999999' Dump Start Date = 00.365    01844014
         MVC   @_Dump_Start_Time,=F'8640000' Dump Start Time = 24:00:00 01845014
         ZAP   @_Dump_End_Date,=P'0'  Dump End Date = 00.000            01846014
         XC    @_Dump_End_Time,@_Dump_End_Time Dump End Time = 00:00:00 01847014
         ZAP   @_Prev_Input_Rec_Date,=P'0'                              01848014
         LA    R15,99                 Get High Line count               01849014
         STH   R15,@_Line_Count        and save it                      01850014
         ZAP   @_Page_Count,=P'0'     Clear Page count                  01851014
                                                                        01852014
*-------------------------------------------------------------------*   01853014
*                                                                   *   01854014
*    Get the current date and time ...                              *   01855014
*                                                                   *   01856014
*-------------------------------------------------------------------*   01857014
                                                                        01858014
         MVC   @B_Time,B_Time         Move L-Form of macro              01859014
         TIME  DEC,                   Get time                         C01860014
               @B_TimeArea,            and return it here              C01861014
               LINKAGE=SYSTEM,         don't use the SVC               C01862014
               DATETYPE=YYYYDDD,       format of returned data         C01863014
               MF=(E,@B_TIME)          addr of macro list               01864014
         XC    @_DateConv_Area,@_DateConv_Area Clear date conv area     01865014
         XC    @B_DBLWD,@B_DBLWD      Clear work area                   01866014
                                                                        01867014
*-------------------------------------------------------------------*   01868014
*                                                                   *   01869014
*    ... and some info about it.                                    *   01870014
*                                                                   *   01871014
*-------------------------------------------------------------------*   01872014
                                                                        01873014
         SLR   R0,R0                  Clear work register               01874014
         IC    R0,@B_TimeArea         Get HH                            01875014
         SLL   R0,4                   Make room for sign                01876014
         STH   R0,@B_DBLWD+6          Save it                           01877014
         OI    @B_DBLWD+7,X'0F'       Add a sign                        01878014
         CVB   R0,@B_DBLWD            Binarize it                       01879014
         STC   R0,@_DateConv_Area+DConv_Time_HH-DateConv_Area           01880014
         IC    R0,@B_TimeArea+1       Get MM                            01881014
         SLL   R0,4                   Make room for sign                01882014
         STH   R0,@B_DBLWD+6          Save it                           01883014
         OI    @B_DBLWD+7,X'0F'       Add a sign                        01884014
         CVB   R0,@B_DBLWD            Binarize it                       01885014
         STC   R0,@_DateConv_Area+DConv_Time_MM-DateConv_Area           01886014
         IC    R0,@B_TimeArea+2       Get SS                            01887014
         SLL   R0,4                   Make room for sign                01888014
         STH   R0,@B_DBLWD+6          Save it                           01889014
         OI    @B_DBLWD+7,X'0F'       Add a sign                        01890014
         CVB   R0,@B_DBLWD            Binarize it                       01891014
         STC   R0,@_DateConv_Area+DConv_Time_SS-DateConv_Area           01892014
         IC    R0,@B_TimeArea+3       Get tt                            01893014
         SLL   R0,4                   Make room for sign                01894014
         STH   R0,@B_DBLWD+6          Save it                           01895014
         OI    @B_DBLWD+7,X'0F'       Add a sign                        01896014
         CVB   R0,@B_DBLWD            Binarize it                       01897014
         STC   R0,@_DateConv_Area+DConv_Time_tt-DateConv_Area           01898014
         OI    @_DateConv_Area+DConv_Input_hhmmss-DateConv_Area,L'DConv+01899014
               _Input_hhmmss          Indicate time in hhmmss           01900014
                                                                        01901014
         L     R15,@B_TimeArea+8      Get Date (0yyyyddd)               01902014
         SLR   R14,R14                Clear work register               01903014
         SLDL  R14,20                 R14 = yyyy                        01904014
         SLL   R14,4                  Make room for sign                01905014
         ST    R14,@B_DBLWD+4         Save it                           01906014
         OI    @B_DBLWD+7,X'0F'       Add a sign                        01907014
         CVB   R14,@B_DBLWD           Binarize yyyy                     01908014
         STH   R14,@_DateConv_Area+DConv_Date_YYYY-DateConv_Area        01909014
         SRL   R15,16                 R15 = ddd0                        01910014
         ST    R15,@B_DBLWD+4         Save it                           01911014
         OI    @B_DBLWD+7,X'0F'       Add a sign                        01912014
         CVB   R15,@B_DBLWD           Binarize ddd                      01913014
         STH   R15,@_DateConv_Area+DConv_Date_DDD-DateConv_Area         01914014
         OI    @_DateConv_Area+DConv_Input_Julian-DateConv_Area,L'DConv+01915014
               _Input_Julian          Indicate date in YYYY and DDD     01916014
                                                                        01917014
         LA    R1,@_DateConv_Area    Point to Date Conversion area      01918014
         L     R15,=AL4(K0010)       A(Format routine)                  01919014
         BASR  R14,R15               Go get info about today            01920014
                                                                        01921014
         NI    @_DateConv_Area+DConv_Input_hhmmss-DateConv_Area,X'FF'-L+01922014
               'DConv_Input_hhmmss    Off the flag                      01923014
         NI    @_DateConv_Area+DConv_Input_Julian-DateConv_Area,X'FF'-L+01924014
               'DConv_Input_Julian    Off the flag                      01925014
                                                                        01926014
*-------------------------------------------------------------------*   01927014
*                                                                   *   01928014
*    Initialize SYSPRINT stuff                                      *   01929014
*                                                                   *   01930014
*-------------------------------------------------------------------*   01931014
                                                                        01932014
         SLR   R15,R15                  Clear work register             01933014
         ST    R15,@_SYSPRINT_DCB_Ptr   Clear A(SYSPRINT DCB)           01934014
         OI    ODTE_Print_NO,L'ODTE_Print_NO Suppress headings          01935014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               01936014
         BASR  R14,R15                Go get a record                   01937014
         NI    ODTE_Print_NO,X'FF'-L'ODTE_Print_NO Reset no print flag  01938014
         LTR   R15,R15                  OK?                             01939014
         BNZ   B8000                    No, exit with error             01940014
                                                                        01941014
*-------------------------------------------------------------------*   01942014
*                                                                   *   01943014
*    Check out SYSIN control cards                                  *   01944014
*                                                                   *   01945014
*-------------------------------------------------------------------*   01946014
                                                                        01947014
         L     R10,@_ODT_Ptr          A(Output Desc Table)              01948014
         LA    R10,ODT_Entry-OutDesc_Tbl(R10) A(default (1st) entry)    01949014
         L     R15,=AL4(H0010)        A(SYSIN processor)                01950014
         BASR  R14,R15                Go interpret SYSIN                01951014
         LTR   R15,R15                How did we do?                    01952014
         BNZ   B8000                  Not great, exit                   01953014
                                                                        01954014
*-------------------------------------------------------------------*   01955014
*                                                                   *   01956014
*    Go allocate (if necessary) and OPEN SYSUT1                     *   01957014
*                                                                   *   01958014
*-------------------------------------------------------------------*   01959014
                                                                        01960014
         L     R15,=AL4(G0010)        A(SYSPRINT Routine)               01961014
         BASR  R14,R15                A blank line                      01962014
         L     R15,=AL4(G0010)        A(SYSPRINT Routine)               01963014
         BASR  R14,R15                and another                       01964014
                                                                        01965014
         L     R15,=AL4(I0010)        A(SYSUT1 OPEN)                    01966014
         BASR  R14,R15                Go OPEN SYSUT1                    01967014
         LTR   R15,R15                How did we do?                    01968014
         BNZ   B8000                  Not great, exit                   01969014
                                                                        01970014
*-------------------------------------------------------------------*   01971014
*                                                                   *   01972014
*    We seemed to interpret the SYSIN statements OK, and SYSUT1 is  *   01973014
*   OK, so go and fix up the Output Descriptor Table:               *   01974014
*        - if there is only one entry (ie, the default), make the   *   01975014
*          output ddname = SYSUT2,                                  *   01976014
*        - if there is more than one, propagate the default exit    *   01977014
*          names, record type, jobname and data table pointer addrs,*   01978014
*          and include and exclude flags from the default to the    *   01979014
*          other entries, unless they were specified in the other   *   01980014
*          entries. Additionally, inactivate the default entry.     *   01981014
*                                                                   *   01982014
*-------------------------------------------------------------------*   01983014
                                                                        01984014
         LA    R15,1                 Set Input count                    01985014
         LNR   R15,R15                to -1 for any exits               01986014
         ST    R15,@_Input_Count      we may invoke                     01987014
                                                                        01988014
         L     R10,@_ODT_Ptr         A(Output Descriptor Table)         01989014
         LH    R9,ODT_Entry_Count-OutDesc_Tbl(R10) NBR ENTRIES          01990014
         LA    R10,ODT_Entry-OutDesc_Tbl(R10) AND POINT TO FIRST        01991014
         CH    R9,=H'1'              ONLY ONE ENTRY?                    01992014
         BH    B0020                 NO, SKIP                           01993014
         MVC   ODTE_DDName,=CL8'SYSUT2' YES, DEFAULT TO SYSUT2          01994014
         SLR   R15,R15               Clear work register                01995014
         ST    R15,ODTE_DCB_Ptr      No DCB yet                         01996014
         B     B0120                 AND SKIP                           01997014
B0020    DS    0H                                                       01998014
         LR    R1,R10                             SAVE A(DEFAULT ENTRY) 01999014
         OI    ODTE_Inactive,L'ODTE_Inactive      MAKE DEFAULT INACTIVE 02000014
         MVC   @B_Default_InExit_Name,ODTE_InExit Save default name     02001014
         MVC   @B_Default_InExit_Parm_Ptr,ODTE_InExit_Parm_Ptr          02002014
         MVC   @B_Default_OutExit_Name,ODTE_OutExit  Save default name  02003014
         MVC   @B_Default_OutExit_Parm_Ptr,ODTE_OutExit_Parm_Ptr        02004014
         B     B0110                                                    02005014
B0030    DS    0H                                                       02006014
         CLI   ODTE_InExit,C' '      INPUT EXIT NAME BLANK?             02007014
         BNE   B0040                 NO, SKIP                           02008014
         TM    ODTE_No_Dflt_InExit,L'ODTE_No_Dflt_Inexit Use default?   02009014
         BO    B0040                 No, skip                           02010014
         MVC   ODTE_InExit,@B_Default_InExit_Name Yes, move default     02011014
         MVC   ODTE_InExit_Parm_Ptr,@B_Default_InExit_Parm_Ptr          02012014
B0040    DS    0H                                                       02013014
         CLI   ODTE_OutExit,C' '     OUTPUT EXIT NAME BLANK?            02014014
         BNE   B0050                 NO, SKIP                           02015014
         TM    ODTE_No_Dflt_OutExit,L'ODTE_No_Dflt_Outexit Use default? 02016014
         BO    B0050                 No, skip                           02017014
         MVC   ODTE_OutExit,@B_Default_OutExit_Name    Yes, DEFAULT     02018014
         MVC   ODTE_OutExit_Parm_Ptr,@B_Default_OutExit_Parm_Ptr        02019014
B0050    DS    0H                                                       02020014
         ICM   R15,B'1111',ODTE_RTT_Ptr A(RECORD TABLE) = 0?            02021014
         BNZ   B0060                 NO, SKIP                           02022014
         MVC   ODTE_RTT_Ptr,ODTE_RTT_Ptr-ODT_Entry(R1) Yes, default     02023014
B0060    DS    0H                                                       02024014
         ICM   R15,B'1111',ODTE_JNT_Ptr A(JOBNAME TABLE) = 0?           02025014
         BNZ   B0070                 NO, SKIP                           02026014
         MVC   ODTE_JNT_Ptr,ODTE_JNT_Ptr-ODT_Entry(R1) Yes, default     02027014
B0070    DS    0H                                                       02028014
         ICM   R15,B'1111',ODTE_DT_Ptr A(DATA TABLE) = 0?               02029014
         BNZ   B0080                 NO, SKIP                           02030014
         MVC   ODTE_DT_Ptr,ODTE_DT_Ptr-ODT_Entry(R1) Yes, default       02031014
B0080    DS    0H                                                       02032014
         TM    ODTE_Print_NO-ODT_Entry(R1),L'ODTE_Print_NO              02033014
         BZ    B0090                                                    02034014
         OI    ODTE_Print_NO,L'ODTE_Print_NO Set flag                   02035014
B0090    DS    0H                                                       02036014
         TM    ODTE_Print_EB-ODT_Entry(R1),L'ODTE_Print_EB              02037014
         BZ    B0100                                                    02038014
         OI    ODTE_Print_EB,L'ODTE_Print_EB Set flag                   02039014
B0100    DS    0H                                                       02040014
         TM    ODTE_JobData_OR-ODT_Entry(R1),L'ODTE_JobData_OR          02041014
         BZ    B0110                                                    02042014
         OI    ODTE_JobData_OR,L'ODTE_JobData_OR Set flag               02043014
B0110    DS    0H                                                       02044014
         AL    R10,=AL4(ODTE_Length) A(Next entry)                      02045014
         BCT   R9,B0030              AND DO IT                          02046014
                                                                        02047014
*-------------------------------------------------------------------*   02048014
*                                                                   *   02049014
*    For each entry in the Output Descriptor Table ...              *   02050014
*                                                                   *   02051014
*-------------------------------------------------------------------*   02052014
                                                                        02053014
B0120    DS    0H                                                       02054014
         L     R10,@_ODT_Ptr          A(OUTPUT ENVIRONMENT TABLE)       02055014
         LH    R9,ODT_Entry_Count-OutDesc_Tbl(R10) GET NBR ENTRIES      02056014
         LA    R10,ODT_Entry-OutDesc_Tbl(R10) AND POINT TO FIRST        02057014
B0130    DS    0H                                                       02058014
                                                                        02059014
*-------------------------------------------------------------------*   02060014
*                                                                   *   02061014
*    ... get the attributes for the dataset                         *   02062014
*                                                                   *   02063014
*-------------------------------------------------------------------*   02064014
                                                                        02065014
         TM    ODTE_Inactive,L'ODTE_Inactive INACTIVE ENTRY?            02066014
         BO    B0220                  YES, SKIP DCB STUFF               02067014
                                                                        02068014
         LA    R0,B_SYSUT2_DCB_Length Length of DCB                     02069014
         STORAGE OBTAIN,              Get storage                      +02070014
               LENGTH=(0),             for the DCB                     +02071014
               LOC=BELOW               Below-the-line                   02072014
         MVC   0(B_SYSUT2_DCB_Length,R1),B_SYSUT2_DCB Move DCB          02073014
         MVC   DCBDDNAM-IHADCB(L'DCBDDNAM,R1),ODTE_DDName IN DCB        02074014
         ST    R1,ODTE_DCB_Ptr        Save A(DCB)                       02075014
*                                                               *RWS17  02076014
         L     R15,=AL4(W0010)        A(Get JFCB address routine)       02077014
         BASR  R14,R15                Go get it                         02078014
         LTR   R15,R15                How did we do?                    02079014
         BNZ   B0200                  Not good, skip                    02080014
         LR    R3,R1                  OK, point to JFCB                 02081014
                                                                        02082014
*-------------------------------------------------------------------*   02083014
*                                                                   *   02084014
*    If this is a new dataset ...                                   *   02085014
*                                                                   *   02086014
*-------------------------------------------------------------------*   02087014
                                                                        02088014
         TM    JFCBIND2-INFMJFCB(R3),JFCNEW DISP=NEW or not specified?  02089014
         BM    B0180                  NO, USE EXISTING STUFF            02090014
                                                                        02091014
*-------------------------------------------------------------------*   02092014
*                                                                   *   02093014
*    ... set up RECFM, LRECL and BLKSIZE if they were not           *   02094014
*   specified                                                       *   02095014
*                                                                   *   02096014
*-------------------------------------------------------------------*   02097014
                                                                        02098014
         L     R2,ODTE_DCB_Ptr        A(DCB)                            02099014
                                                                        02100014
         IC    R15,JFCRECFM-INFMJFCB(R3) GET JFCB RECFM                 02101014
         TM    JFCRECFM-INFMJFCB(R3),JFCFMREC ANYTHING THERE?           02102014
         BNZ   B0140                  YES, OK                           02103014
         L     R1,@_SYSUT1_DCB_Ptr    A(Input DCB)                      02104014
         IC    R15,DCBRECFM-IHADCB(R1) NO, GET RECFM FROM SYSUT1        02105014
         TM    @_SYSUT1_VSAM,L'@_SYSUT1_VSAM IS THE INPUT VSAM?         02106014
         BZ    B0140                   NO, OK                           02107014
         IC    R15,=AL1(DCBRECV+DCBRECBR+DCBRECSB) YES, DEFAULT TO VBS  02108014
B0140    DS    0H                                                       02109014
         STC   R15,DCBRECFM-IHADCB(R2) SAVE RECFM                       02110014
         TM    DCBRECFM-IHADCB(R2),DCBRECU RECFM=U?                     02111014
         BO    B0170                   YES, SKIP LRECL CHECK            02112014
         BNZ   B0150                   NO, BUT SOME LRECL SPECIFIED     02113014
         TM    JFCRECFM-INFMJFCB(R3),JFCUND JFCB RECFM = U?             02114014
         BO    B0170                   YES, SKIP LRECL CHECK            02115014
B0150    DS    0H                                                       02116014
         LH    R15,JFCLRECL-INFMJFCB(R3) GET JFCB LRECL                 02117014
         LTR   R15,R15                 IS THERE ONE?                    02118014
         BNZ   B0160                   YES, SKIP                        02119014
         L     R1,@_SYSUT1_DCB_Ptr     No, A(SYSUT1 DCB)                02120014
         LH    R15,DCBLRECL-IHADCB(R1) NO, GET SYSUT1 LRECL             02121014
         TM    @_SYSUT1_VSAM,L'@_SYSUT1_VSAM IS THE INPUT VSAM?         02122014
         BZ    B0160                   NO, OK                           02123014
         L     R15,=F'32760'           YES, DEFAULT LRECL=32760         02124014
B0160    DS    0H                                                       02125014
         STH   R15,DCBLRECL-IHADCB(R2) Update OUTPUT LRECL              02126014
B0170    DS    0H                                                       02127014
         LH    R15,JFCBLKSI-INFMJFCB(R3) GET JFCB BLKSIZE               02128014
         LTR   R15,R15                 IS THERE ONE?                    02129014
         BNZ   B0180                   YES, SKIP                        02130014
         MVC   DCBBLKSI-IHADCB(L'DCBBLKSI,R2),DCBBLKSI-IHADCB(R1)       02131014
         TM    @_SYSUT1_VSAM,L'@_SYSUT1_VSAM IS SYSUT1 VSAM?            02132014
         BZ    B0180                   NO, OK                           02133014
         SLR   R15,R15                 YES, Use SDB                     02134014
         STH   R15,DCBBLKSI-IHADCB(R2) UPDATE SYSUT2                    02135014
B0180    DS    0H                                                       02136014
         L     R2,ODTE_DCB_Ptr            A(DCB)                        02137014
         MVC   @B_OPEN,B_OPEN             MOVE OPEN PARMS               02138014
         OPEN  ((2),OUTPUT),MODE=31,MF=(E,@B_OPEN) AND OPEN IT          02139014
         TM    DCBOFLGS-IHADCB(R2),DCBOFOPN IS IT OPEN?                 02140014
         BZ    B0200                  NO, SKIP                          02141014
         LH    R15,DCBLRECL-IHADCB(R2) YES, GET LRECL                   02142014
         TM    DCBRECFM-IHADCB(R2),DCBRECU IS RECFM = U?                02143014
         BNO   B0190                  NO, OK                            02144014
         LH    R15,DCBBLKSI-IHADCB(R2) YES, GET BLKSIZE INSTEAD         02145014
B0190    DS    0H                                                       02146014
         STH   R15,ODTE_LRECL         SAVE MAX OUTPUT LRECL             02147014
                                                                        02148014
*-------------------------------------------------------------------*   02149014
*                                                                   *   02150014
*    Go list some stuff about this file                             *   02151014
*                                                                   *   02152014
*-------------------------------------------------------------------*   02153014
                                                                        02154014
         L     R1,ODTE_DCB_Ptr        A(DCB)                            02155014
         L     R15,=A(J0010)          A(Info format routine)            02156014
         BASR  R14,R15                Go do it                          02157014
         B     B0210                  and skip                          02158014
                                                                        02159014
B0200    DS    0H                                                       02160014
                                                                        02161014
*-------------------------------------------------------------------*   02162014
*                                                                   *   02163014
*    THE OUTPUT DCB COULD NOT BE OPENED - SAY SO.                   *   02164014
*                                                                   *   02165014
*-------------------------------------------------------------------*   02166014
                                                                        02167014
         L     R1,@_SYSPRINT_Rec_Ptr  A(SYSPRINT LINE)                  02168014
         L     R15,ODTE_DCB_Ptr       A(DCB)                            02169014
         MVC   1(8,R1),DCBDDNAM-IHADCB(R15) MOVE DDNAME                 02170014
         MVC   9(40,R1),=C' DD statement not found; **U4090 ABEND**'    02171018
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               02172014
         BASR  R14,R15                PRINT OUR LINE                    02173014
         LA    R12,4090                                                 02174014
         ABEND (R12),DUMP                                               02175020
                                                                        02176014
B0210    DS    0H                                                       02177014
                                                                        02178014
*-------------------------------------------------------------------*   02179014
*                                                                   *   02180014
*   Assume this entry has no selection criteria (i.e., a straight   *   02181014
*  copy). This flag will get reset each time we find (and print)    *   02182014
*  some selection criteria.                                         *   02183014
*                                                                   *   02184014
*-------------------------------------------------------------------*   02185014
                                                                        02186014
         OI    ODTE_CopyOnly,L'ODTE_CopyOnly  Assume no selection crit  02187014
                                                                        02188014
         LA    R1,ODT_Entry           Point to ODTE                     02189014
         L     R15,=AL4(L0010)        A(List routine)                   02190014
         BASR  R14,R15                Go do it                          02191014
                                                                        02192014
B0220    DS    0H                                                       02193014
                                                                        02194014
*-------------------------------------------------------------------*   02195014
*                                                                   *   02196014
*    CHECK TO SEE IF WE HAVE TO LOAD THE EXITS - IF WE DO, WE DO.   *   02197014
*                                                                   *   02198014
*-------------------------------------------------------------------*   02199014
                                                                        02200014
         CLI   ODTE_InExit,C' '       IS THERE AN INEXIT?               02201014
         BE    B0270                  NO, SKIP                          02202014
         NI    ODTE_CopyOnly,X'FF'-L'ODTE_CopyOnly Selection criteria   02203014
         CLC   ODTE_InExit,@B_Default_InExit_Name  Default?             02204014
         BNE   B0230                  No, go load it                    02205014
         ICM   R0,B'1111',@B_Default_InExit_Ptr Yes, get A(Exit)        02206014
         BNZ   B0240                  LOADed already, skip              02207014
B0230    DS    0H                                                       02208014
         LA    R0,ODTE_InExit         NO, POINT TO IT                   02209014
         LOAD  EPLOC=(0),ERRET=B0260 AND GO GET IT                      02210014
         CLC   ODTE_InExit,@B_Default_InExit_Name  Default?             02211014
         BNE   B0240                  No, skip                          02212014
         ST    R0,@B_Default_InExit_Ptr Yes, save its address           02213014
B0240    DS    0H                                                       02214014
         ST    R0,ODTE_InExit_Ptr     SAVE EPA                          02215014
                                                                        02216014
         SLR   R1,R1                  Clear A(SMF Record)               02217014
         SLR   R0,R0                  An InExit                         02218014
         L     R15,=AL4(X0010)        A(Exit invocation routine)        02219014
         BASR  R14,R15                Go do it                          02220014
         C     R15,=FL4'12'           How did it go?                    02221014
         BL    B0270                  OK, skip                          02222014
         OI    ODTE_Inactive,L'ODTE_Inactive Not good, make inactive    02223014
         B     B0360                  AND SKIP                          02224014
                                                                        02225014
B0260    DS    0H                                                       02226014
         L     R1,@_SYSPRINT_Rec_Ptr  POINT TO SYSPRINT RECORD          02227014
         MVC   20(35,R1),=C'Exit could not be LOADed - ignored.'        02228014
         MVC   10(L'ODTE_InExit,R1),ODTE_InExit MOVE EXIT NAME          02229014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               02230014
         BASR  R14,R15                GO PRINT                          02231014
         MVI   ODTE_InExit,C' '       Clear 1st byte of name            02232014
B0270    DS    0H                                                       02233014
         CLI   ODTE_OutExit,C' '      IS THERE AN OUTEXIT?              02234014
         BE    B0320                  NO, SKIP                          02235014
         CLC   ODTE_OutExit,@B_Default_OutExit_Name  Default?           02236014
         BNE   B0280                  No, go load it                    02237014
         ICM   R0,B'1111',@B_Default_OutExit_Ptr Yes, get A(Exit)       02238014
         BNZ   B0290                  LOADed already, skip              02239014
B0280    DS    0H                                                       02240014
         LA    R0,ODTE_OutExit        NO, POINT TO IT                   02241014
         LOAD  EPLOC=(0),ERRET=B0310 AND GO GET IT                      02242014
         CLC   ODTE_OutExit,@B_Default_OutExit_Name  Default?           02243014
         BNE   B0290                  No, skip                          02244014
         ST    R0,@B_Default_OutExit_Ptr Yes, save its address          02245014
B0290    DS    0H                                                       02246014
         ST    R0,ODTE_OutExit_Ptr    SAVE EPA                          02247014
                                                                        02248014
         SLR   R1,R1                  Clear A(SMF Record)               02249014
         LA    R0,1                   An OutExit                        02250014
         L     R15,=AL4(X0010)        A(Exit invocation routine)        02251014
         BASR  R14,R15                Go do it                          02252014
         C     R15,=FL4'12'           How did it go?                    02253014
         BL    B0320                  OK, skip                          02254014
         OI    ODTE_Inactive,L'ODTE_Inactive Not good, make inactive    02255014
         B     B0360                  AND SKIP                          02256014
                                                                        02257014
B0310    DS    0H                                                       02258014
         L     R1,@_SYSPRINT_Rec_Ptr  POINT TO SYSPRINT RECORD          02259014
         MVC   20(35,R1),=C'Exit could not be LOADed - ignored.'        02260014
         MVC   10(L'ODTE_OutExit,R1),ODTE_OutExit MOVE EXIT NAME        02261014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               02262014
         BASR  R14,R15                GO PRINT                          02263014
         MVI   ODTE_OutExit,C' '      Clear 1st byte of name            02264014
B0320    DS    0H                                                       02265014
                                                                        02266014
*-------------------------------------------------------------------*   02267014
*                                                                   *   02268014
*    If we are ignoring weekends, adjust the start/stop times (if   *   02269014
*   present), if they start or stop on weekends.                    *   02270014
*                                                                   *   02271014
*-------------------------------------------------------------------*   02272014
                                                                        02273014
         TM    ODTE_Ignore_Weekend,L'ODTE_Ignore_Weekend No Sat or Sun? 02274014
         BNO   B0360                 No, OK                             02275014
         NI    @B_Dates_Altered,X'FF'-L'@B_Dates_Altered Off our flag   02276014
                                                                        02277014
         CP    ODTE_StartDate,=P'0'  Any start date?                    02278014
         BE    B0330                 No, skip                           02279014
                                                                        02280014
         XC    @B_DateConv_Area,@B_DateConv_Area Clear Date area        02281014
         ZAP   @B_DateConv_Area+DConv_Date_SMFDate-DateConv_Area(L'DCon+02282014
               v_Date_SMFDate),ODTE_StartDate                           02283014
         OI    @B_DateConv_Area+DConv_Input_SMFDate-DateConv_Area,L'DCo+02284014
               nv_Input_SMFDate        Indicate SMF Date                02285014
         LA    R1,@B_DateConv_Area   Point to Date Conversion area      02286014
         L     R15,=AL4(K0010)       A(Format routine)                  02287014
         BASR  R14,R15               Go get info                        02288014
                                                                        02289014
         MVC   @B_StartDate_Base,@B_DateConv_Area+DConv_Date_Base-DateC+02290014
               onv_Area              Save start Basedate                02291014
         MVC   @B_StartDate_DOW,@B_DateConv_Area+DConv_Date_DOW-DateCon+02292014
               v_Area                Save start DOW                     02293014
         CLI   @B_DateConv_Area+DConv_Date_DOW-DateConv_Area,X'04'      02294014
         BNH   B0330                 Not starting on a weekend, skip    02295014
         SLR   R0,R0                 Clear work register                02296014
         IC    R0,@B_DateConv_Area+DConv_Date_DOW-DateConv_Area         02297014
         LA    R15,7                 DOW of the next Monday             02298014
         SR    R15,R0                Number of days until Monday        02299014
         A     R15,@B_StartDate_Base Bump base date                     02300014
         ST    R15,@B_StartDate_Base  and save it                       02301014
         ST    R15,@B_DateConv_Area+DConv_Date_Base-DateConv_Area       02302014
         OI    @B_DateConv_Area+DConv_Input_Base-DateConv_Area,L'DConv_+02303014
               Input_Base           Indicate Base Date                  02304014
         NI    @B_DateConv_Area+DConv_Input_SMFDate-DateConv_Area,X'FF'+02305014
               -L'DConv_Input_SMFDate Off SMFDate flag                  02306014
         LA    R1,@B_DateConv_Area   Point to Date Conversion area      02307014
         L     R15,=AL4(K0010)       A(Format routine)                  02308014
         BASR  R14,R15               Go get info                        02309014
                                                                        02310014
         ZAP   ODTE_StartDate,@B_DateConv_Area+DConv_Date_SMFDate-DateC+02311014
               onv_Area(L'DConv_Date_SMFDate)  Update Start Date        02312014
         OI    @B_Dates_Altered,L'@B_Dates_Altered Set our flag         02313014
                                                                        02314014
         L     R1,@_SYSPRINT_Rec_Ptr  POINT TO SYSPRINT RECORD          02315014
         MVC   10(23,R1),=C'Start Date modified to '                    02316014
         MVC   @B_DateConv_Area+DConv_Time-DateConv_Area(L'DConv_Time),+02317014
               ODTE_StartTime               Time too                    02318014
         OI    @B_DateConv_Area+DConv_Input_Base-DateConv_Area,L'DConv_+02319014
               Input_Base           Indicate Base date                  02320014
         OI    @B_DateConv_Area+DConv_Input_Time-DateConv_Area,L'DConv_+02321014
               Input_Time           Indicate SMF Time                   02322014
         LA    R15,@B_DateConv_Area     Point to Date Conversion area   02323014
         ST    R15,@B_Parms          Save as 1st parm                   02324014
         LA    R15,33(0,R1)          A(Output area)                     02325014
         ST    R15,@B_Parms+4        Save as 2nd parm                   02326014
         LA    R15,@B_Return_Ptr     A(Return area)                     02327014
         ST    R15,@B_Parms+8        Save it                            02328014
         LA    R1,@B_Parms           A(Parm pointers)                   02329014
         L     R15,=AL4(F0010)       A(Format routine)                  02330014
         BASR  R14,R15               Go convert date and time           02331014
         L     R15,=AL4(G0010)       A(SYSPRINT routine)                02332014
         BASR  R14,R15               GO PRINT                           02333014
                                                                        02334014
         NI    @B_DateConv_Area+DConv_Input_Base-DateConv_Area,X'FF'-L'+02335014
               DConv_Input_Base      Off the flag                       02336014
         NI    @B_DateConv_Area+DConv_Input_Time-DateConv_Area,X'FF'-L'+02337014
               DConv_Input_Time      Off the flag                       02338014
B0330    DS    0H                                                       02339014
         CP    ODTE_EndDate,=P'999999' Any End Date?                    02340014
         BE    B0340                 No, skip                           02341014
                                                                        02342014
         XC    @B_DateConv_Area,@B_DateConv_Area Clear Date area        02343014
         ZAP   @B_DateConv_Area+DConv_Date_SMFDate-DateConv_Area(L'DCon+02344014
               v_Date_SMFDate),ODTE_EndDate                             02345014
         OI    @B_DateConv_Area+DConv_Input_SMFDate-DateConv_Area,L'DCo+02346014
               nv_Input_SMFDate        Indicate SMF Date                02347014
         LA    R1,@B_DateConv_Area   Point to Date Conversion area      02348014
         L     R15,=AL4(K0010)       A(Format routine)                  02349014
         BASR  R14,R15               Go get info                        02350014
                                                                        02351014
         MVC   @B_EndDate_Base,@B_DateConv_Area+DConv_Date_Base-DateCon+02352014
               v_Area                Save End Basedate                  02353014
         CLI   @B_DateConv_Area+DConv_Date_DOW-DateConv_Area,X'04'      02354014
         BNH   B0340                 Not ending on a weekend, skip      02355014
         SLR   R0,R0                 Clear work register                02356014
         IC    R0,@B_DateConv_Area+DConv_Date_DOW-DateConv_Area         02357014
         LA    R15,4                 DOW of the previous Friday         02358014
         SR    R0,R15                Number of days after Friday        02359014
         L     R15,@B_EndDate_Base   Get Base date                      02360014
         SR    R15,R0                Back up to Friday                  02361014
         ST    R15,@B_EndDate_Base   and save it                        02362014
         ST    R15,@B_DateConv_Area+DConv_Date_Base-DateConv_Area       02363014
         OI    @B_DateConv_Area+DConv_Input_Base-DateConv_Area,L'DConv_+02364014
               Input_Base           Indicate Base Date                  02365014
         NI    @B_DateConv_Area+DConv_Input_SMFDate-DateConv_Area,X'FF'+02366014
               -L'DConv_Input_SMFDate Off SMFDate flag                  02367014
         LA    R1,@B_DateConv_Area   Point to Date Conversion area      02368014
         L     R15,=AL4(K0010)       A(Format routine)                  02369014
         BASR  R14,R15               Go get info                        02370014
                                                                        02371014
         ZAP   ODTE_EndDate,@B_DateConv_Area+DConv_Date_SMFDate-DateCon+02372014
               v_Area(L'DConv_Date_SMFDate) Update end date             02373014
         OI    @B_Dates_Altered,L'@B_Dates_Altered Set our flag         02374014
                                                                        02375014
         L     R1,@_SYSPRINT_Rec_Ptr  POINT TO SYSPRINT RECORD          02376014
         MVC   10(21,R1),=C'End Date modified to '                      02377014
         MVC   @B_DateConv_Area+DConv_Time-DateConv_Area(L'DConv_Time),+02378014
               ODTE_EndTime          Time too                           02379014
         OI    @B_DateConv_Area+DConv_Input_Base-DateConv_Area,L'DConv_+02380014
               Input_Base            Indicate Base date                 02381014
         OI    @B_DateConv_Area+DConv_Input_Time-DateConv_Area,L'DConv_+02382014
               Input_Time            Indicate SMF Time                  02383014
         LA    R15,@B_DateConv_Area  Point to Date Conversion area      02384014
         ST    R15,@B_Parms          Save as 1st parm                   02385014
         LA    R15,31(0,R1)          A(Output area)                     02386014
         ST    R15,@B_Parms+4        Save as 2nd parm                   02387014
         LA    R15,@B_Return_Ptr     A(Return area)                     02388014
         ST    R15,@B_Parms+8        Save it                            02389014
         LA    R1,@B_Parms           A(Parm pointers)                   02390014
         L     R15,=AL4(F0010)       A(Format routine)                  02391014
         BASR  R14,R15               Go convert date and time           02392014
         L     R15,=AL4(G0010)       A(SYSPRINT routine)                02393014
         BASR  R14,R15               GO PRINT                           02394014
                                                                        02395014
         NI    @B_DateConv_Area+DConv_Input_Base-DateConv_Area,X'FF'-L'+02396014
               DConv_Input_Base      Off the flag                       02397014
         NI    @B_DateConv_Area+DConv_Input_Time-DateConv_Area,X'FF'-L'+02398014
               DConv_Input_Time      Off the flag                       02399014
B0340    DS    0H                                                       02400014
                                                                        02401014
*-------------------------------------------------------------------*   02402014
*                                                                   *   02403014
*    If there is a StartDate and an EndDate, check the adjusted     *   02404014
*   dates ...                                                       *   02405014
*                                                                   *   02406014
*-------------------------------------------------------------------*   02407014
                                                                        02408014
         TM    @B_Dates_Altered,L'@B_Dates_Altered  Did we do anything? 02409014
         BNO   B0360                 No, skip                           02410014
         CP    ODTE_StartDate,=P'0'  Any start date?                    02411014
         BE    B0360                 No, skip                           02412014
         CP    ODTE_EndDate,=P'999999' Any End Date?                    02413014
         BE    B0360                 No, skip                           02414014
                                                                        02415014
*-------------------------------------------------------------------*   02416014
*                                                                   *   02417014
*    ... to see if we have to select anything. For instance, a      *   02418014
*   user may have specified:                                        *   02419014
*      START=Saturday,END=Sunday,WEEKENDS=IGNORE                    *   02420014
*   We will have adjusted the StartDate to Monday, and EndDate to   *   02421014
*   the previous Friday. In this case, we inactivate the entry      *   02422014
*   immediately.                                                    *   02423014
*                                                                   *   02424014
*-------------------------------------------------------------------*   02425014
                                                                        02426014
         L     R15,@B_EndDate_Base   Get base date for End date         02427014
         C     R15,@B_StartDate_Base Less than StartDate?               02428014
         BNL   B0350                 No, skip                           02429014
         OI    ODTE_Inactive,L'ODTE_Inactive Yes, set inactive          02430014
         B     B0360                and skip                            02431014
B0350    DS    0H                                                       02432014
                                                                        02433014
*-------------------------------------------------------------------*   02434014
*                                                                   *   02435014
*    If we have adjusted the dates so that a weekend does not fall  *   02436014
*   within the Start and End dates, we can turn off the 'Ignore     *   02437014
*   Weekends' flag. This will save us some processing, since we     *   02438014
*   won't have to figure out the Day-of-Week for each day.          *   02439014
*                                                                   *   02440014
*-------------------------------------------------------------------*   02441014
                                                                        02442014
         L     R15,@B_EndDate_Base   Get base date for End date         02443014
         S     R15,@B_StartDate_Base Get nbr days                       02444014
         SLR   R0,R0                 Clear work register                02445014
         IC    R0,@B_StartDate_DOW   Get starting day-of-week           02446014
         AR    R0,R15                Get DOW of last day                02447014
         C     R0,=FL4'5'            Saturday or greater?               02448014
         BNL   B0360                 Yes, skip                          02449014
         NI    ODTE_Ignore_Weekend,X'FF'-L'ODTE_Ignore_Weekend          02450014
                                                                        02451014
B0360    DS    0H                                                       02452014
                                                                        02453014
*-------------------------------------------------------------------*   02454014
*                                                                   *   02455014
*    All done with this entry, check next                           *   02456014
*                                                                   *   02457014
*-------------------------------------------------------------------*   02458014
                                                                        02459014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               02460014
         BASR  R14,R15                SKIP A LINE                       02461014
                                                                        02462014
         AL    R10,=AL4(ODTE_Length)  A(Next ODT entry)                 02463014
         BCT   R9,B0130               GO PROCESS IT                     02464014
                                                                        02465014
         DROP  R10                    Free ODT Entry base reg           02466014
                                                                        02467014
         SLR   R15,R15                Clear                             02468014
         ST    R15,@_Input_Count       Input count                      02469014
                                                                        02470014
*-------------------------------------------------------------------*   02471014
*                                                                   *   02472014
*    We have finished going through the table, so print some        *   02473014
*   blank lines ...                                                 *   02474014
*                                                                   *   02475014
*-------------------------------------------------------------------*   02476014
                                                                        02477014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               02478014
         BASR  R14,R15                SKIP A LINE                       02479014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               02480014
         BASR  R14,R15                  AND ANOTHER                     02481014
                                                                        02482014
*-------------------------------------------------------------------*   02483014
*                                                                   *   02484014
*    ...  and exit.                                                 *   02485014
*                                                                   *   02486014
*-------------------------------------------------------------------*   02487014
                                                                        02488014
         SLR   R15,R15                Clear return code                 02489014
         B     B9000                  and exit                          02490014
                                                                        02491014
                                                                        02492014
                                                                        02493014
B8000    DS    0H                                                       02494014
                                                                        02495014
*-------------------------------------------------------------------*   02496014
*                                                                   *   02497014
*    Either SYSPRINT could not be opened, or we found an error in   *   02498014
*   the SYSIN statements; set the return code, and exit             *   02499014
*                                                                   *   02500014
*-------------------------------------------------------------------*   02501014
                                                                        02502014
         LA    R15,4                  Error, set return code            02503014
                                                                        02504014
B9000    DS    0H                                                       02505014
                                                                        02506014
*-------------------------------------------------------------------*   02507014
*                                                                   *   02508014
*    Free up our local storage ...                                  *   02509014
*                                                                   *   02510014
*-------------------------------------------------------------------*   02511014
                                                                        02512014
         LR    R3,R15                 Save return code                  02513014
                                                                        02514014
         LA    R1,@B_Dynam              A(Local storage)                02515014
         L     R13,4(0,R13)             Rescue A(HSA)                   02516014
                                                                        02517014
         STACK POP,                     Free the stack area            +02518014
               ADDR=(R1),                starting here                 +02519014
               STACK=@_31Bit_Stack_Ptr   on this stack                  02520014
                                                                        02521014
*-------------------------------------------------------------------*   02522014
*                                                                   *   02523014
*    ... and return to caller                                       *   02524014
*                                                                   *   02525014
*-------------------------------------------------------------------*   02526014
                                                                        02527014
         LR    R15,R3                   Restore return code             02528014
         L     R14,12(0,R13)            Restore return address          02529014
         LM    R0,R12,20(R13)           Restore other registers         02530014
         BSM   0,R14                    and return                      02531014
                                                                        02532014
                                                                        02533014
                                                                        02534014
         PUSH  PRINT                                                    02535014
         PRINT NOGEN                                                    02536014
                                                                        02537014
B_Time   TIME  LINKAGE=SYSTEM,MF=L                                      02538014
B_Time_Length EQU *-B_Time                                              02539014
                                                                        02540014
B_SYSUT2_DCB DCB DDNAME=SYSUT2,MACRF=PM,DSORG=PS                        02541014
B_SYSUT2_DCB_Length EQU *-B_SYSUT2_DCB LENGTH OF SYSUT1 DCB             02542014
                                                                        02543014
B_OPEN   OPEN  (0,INPUT),MODE=31,MF=L                                   02544014
B_OPEN_Length EQU  *-B_OPEN                                             02545014
                                                                        02546014
         POP   PRINT                                                    02547014
                                                                        02548014
         LTORG                                                          02549014
@B_Dynam        DSECT                 Dynamic area for B                02550014
@B_Save         DS    18F              O/S Style save area              02551014
@B_DBLWD        DS    D                Work area                        02552014
@B_Parms        DS    3AL4             Parm area                        02553014
@B_Return_Ptr   DS    AL4              A(Last byte formatted by F0010)  02554014
@B_StartDate_Base DS  FL4              Start Date in REXX Base format   02555014
@B_StartDate_DOW  DS  XL1              Start Date Day-of-Week           02556014
@B_EndDate_Base   DS  FL4              End Date in REXX Base format     02557014
@B_Default_InExit_Name      DS  CL8         Default Exit Name           02558014
@B_Default_InExit_Ptr       DS  AL4         A(EPA of default exit)      02559014
@B_Default_InExit_Parm_Ptr  DS  AL4      A(Parm Ptr for InExit)         02560014
@B_Default_OutExit_Name     DS  CL8        Default Exit Name            02561014
@B_Default_OutExit_Ptr      DS  AL4        A(EPA of default exit)       02562014
@B_Default_OutExit_Parm_Ptr DS  AL4     A(Parm Ptr for OutExit)         02563014
                                                                        02564014
                DS    X                Flag Byte                        02565014
@B_Dates_Altered EQU  *-1,X'80'         1... .... Start/End dates upd   02566014
                                                                        02567014
@B_MACRO        DS    0F               Macro area                       02568014
@B_OPEN         DS    CL(B_OPEN_Length) OPEN macro                      02569014
                ORG   @B_MACRO                                          02570014
@B_Time         DS    CL(B_Time_Length) L-Form of Time                  02571014
@B_TimeArea     DS    CL16             Return area for Time             02572014
                ORG   @B_MACRO                                          02573014
@B_DateConv_Area DS   CL(DConv_Area_Length)  Date conversion area       02574014
                ORG                                                     02575014
                DS    0D               Alignment                        02576014
@B_DynLen       EQU   *-@B_Dynam      Length of storage required        02577014
                                                                        02578014
SMFSLCT  RSECT                                                          02579014
                                                                        02580014
         DROP  R11,R13                                                  02581014
         POP   USING                                                    02582014
         TITLE 'C0000: Read the Input File'                             02583014
                                                                        02584014
         PUSH  USING                                                    02585014
                                                                        02586014
C0010    DS    0H                                                       02587014
                                                                        02588014
*---------------------------------------------------------------------* 02589014
*                                                                     * 02590014
*  Routine   : C0010                                                  * 02591014
*                                                                     * 02592014
*  Abstract  : SYSUT1 Read Routine:                                   * 02593014
*               If SYSUT1 not OPEN                                    * 02594014
*                 OPEN SYSUT1                                         * 02595014
*               EndIf                                                 * 02596014
*               Do until valid SMF record found                       * 02597014
*                 Read SYSUT1                                         * 02598014
*                 Check VBS Segmentation, if required                 * 02599014
*               EndDo                                                 * 02600014
*                                                                     * 02601014
*  Inputs    : None                                                   * 02602014
*                                                                     * 02603014
*                                                                     * 02604014
*  Outputs   : R15: 0 - OK                                            * 02605014
*                   4 - Error encountered                             * 02606014
*                                                                     * 02607014
*  Notes     :                                                        * 02608014
*                                                                     * 02609014
*  History:                                                           * 02610014
*              1999/04/24 SDDA030 V1.1                                * 02611014
*                                 Used LIFO Stack for our dynamic     * 02612014
*                                 areas.                              * 02613014
*              1999/11/18 SDDA030 V1.3                                * 02614014
*                                 Moved SYSUT1 OPEN logic(?) to its   * 02615014
*                                 own subroutine (I0010)              * 02616014
*                                 Added comments                      * 02617014
*              2002/02/07 SDDA030 V1.4                                * 02618014
*                                 Removed invocation of I0010 to OPEN * 02619014
*                                 SYSUT1 (now done in B0010).         * 02620014
*                                 Updated EODAD on 1st time through.  * 02621014
*                                 Cleaned up SYSUT1 resources at EOD. * 02622014
*              ____/__/__ _______                                     * 02623014
*                                                                     * 02624014
*---------------------------------------------------------------------* 02625014
                                                                        02626014
         USING *,R15                                                    02627014
         SAVE  (14,12),T                                                02628014
         LR    R11,R15                  Load permanent base reg         02629014
         DROP  R15                      Free up temp base               02630014
         USING C0010,R11                Assign permanent base           02631014
                                                                        02632014
*-------------------------------------------------------------------*   02633014
*                                                                   *   02634014
*    Get an area on the stack for our local storage                 *   02635014
*                                                                   *   02636014
*-------------------------------------------------------------------*   02637014
                                                                        02638014
         STACK PUSH,                    Get Stack area                 +02639014
               LEN=@C_DynLen,            this long                     +02640014
               STACK=@_31Bit_Stack_Ptr   using this stack               02641014
                                                                        02642014
         LR    R2,R1                    Save its address                02643014
         LR    R0,R1                    A(Our storage)                  02644014
         LA    R1,@C_DynLen              Its length                     02645014
         SLR   R14,R14                  Clear source address            02646014
         SLR   R15,R15                   and length                     02647014
         MVCL  R0,R14                   Clear our storage               02648014
         LR    R1,R2                    Restore A(Our storage)          02649014
                                                                        02650014
*-------------------------------------------------------------------*   02651014
*                                                                   *   02652014
*    Chain our save areas, and restore the important registers      *   02653014
*   that we have destroyed                                          *   02654014
*                                                                   *   02655014
*-------------------------------------------------------------------*   02656014
                                                                        02657014
         ST    R13,4(0,R1)              Chain                           02658014
         ST    R1,8(0,R13)               saveareas                      02659014
         LR    R13,R1                   Load dynam base                 02660014
         USING @C_Dynam,R13             Assign a base                   02661014
         L     R15,@C_Dynam+4           Get A(HSA)                      02662014
         LM    R0,R3,20(R15)            Restore callers registers       02663014
                                                                        02664014
                                                                        02665014
*-------------------------------------------------------------------*   02666014
*                                                                   *   02667014
*    If this is the first time in (@_Input_Count = 0) ...           *   02668014
*                                                                   *   02669014
*-------------------------------------------------------------------*   02670014
                                                                        02671014
         ICM   R15,B'1111',@_Input_Count Get input counter              02672014
         BNZ   C0020                    Not 1st time in, skip           02673014
                                                                        02674014
*-------------------------------------------------------------------*   02675014
*                                                                   *   02676014
*    ... go update the DCBE/ACB EODAD routine address               *   02677014
*                                                                   *   02678014
*-------------------------------------------------------------------*   02679014
                                                                        02680014
         TM    @_SYSUT1_VSAM,L'@_SYSUT1_VSAM It the input VSAM?         02681014
         BO    C0010E                 Yes, skip                         02682014
                                                                        02683014
         L     R15,@_SYSUT1_DCB_Ptr   Yes, A(Input DCB)                 02684014
         L     R15,DCBDCBE-IHADCB(0,R15) A(DCBE)                        02685014
         LA    R0,C7000               A(EOD Routine)                    02686014
         ST    R0,DCBEEODA-DCBE(0,R15) Update DCBE EODAD                02687014
         B     C0010Z                  and skip                         02688014
C0010E   DS    0H                                                       02689014
                                                                        02690014
         L     R2,@_SYSUT1_ACB_Ptr    A(ACB)                            02691014
         LA    R3,@C_SYSUT1_EXLST_Ptr  A(RECEIVING AREA)                02692014
         SHOWCB ACB=(2),              YES, FROM SYSUT1 ACB             +02693014
               FIELDS=EXLST,            WE WANT THE LRECL              +02694014
               AREA=(3),                MOVED TO THIS AREA             +02695014
               LENGTH=L'@C_SYSUT1_EXLST_Ptr, for this long             +02696014
               MF=(G,@C_SHOWCB,C_SHOWCB_Length)                         02697014
                                                                        02698014
         L     R2,@C_SYSUT1_EXLST_Ptr   A(EXLST area)                   02699014
         LA    R3,C7000               Get A(EOD Routine)                02700014
         MODCB EXLST=(2),             Update SYSUT1 EXLST              +02701014
               EODAD=(3),               for EOD address                +02702014
               MF=(G,@C_MODCB,C_MODCB_Length)                           02703014
                                                                        02704014
C0010Z   DS    0H                                                       02705014
         LA    R15,99                   Get High Line count             02706014
         STH   R15,@_Line_Count          and save it                    02707014
                                                                        02708014
C0020    DS    0H                                                       02709014
                                                                        02710014
*-------------------------------------------------------------------*   02711014
*                                                                   *   02712014
*    Read the Input File.                                           *   02713014
*                                                                   *   02714014
*-------------------------------------------------------------------*   02715014
                                                                        02716014
         TM    @_SYSUT1_VSAM,L'@_SYSUT1_VSAM It the input VSAM?         02717014
         BZ    C0030                  No, skip                          02718014
         L     R1,@_SYSUT1_RPL_Ptr    Yes, A(RPL)                       02719014
         GET   RPL=(1)                and issue GET                     02720014
                                                                        02721014
*-------------------------------------------------------------------*   02722014
*                                                                   *   02723014
*    We have just read a record from the active SMF file, in which  *   02724014
*  the end will be marked by a special record, so we had better     *   02725014
*  check for it. If this is it, we signal EOF.                      *   02726014
*                                                                   *   02727014
*-------------------------------------------------------------------*   02728014
                                                                        02729014
         L     R1,@_SYSUT1_Rec_Ptr    A(INPUT RECORD)                   02730014
         LH    R15,SMF0LEN-SMFRCD0(R1) GET LENGTH                       02731014
         CH    R15,=H'14'             IS IT THE RIGHT LENGTH?           02732014
         BNE   C0180                  NO, SKIP                          02733014
         CLC   =C'SMFEOFMARK',SMF0FLG-SMFRCD0(R1) YES, IS THIS IT?      02734014
         BE    C7000                  YES, EOF                          02735014
         B     C0180                  NO, CONTINUE                      02736014
C0030    DS    0H                                                       02737014
                                                                        02738014
         TM    @_SYSUT1_QSAM,L'@_SYSUT1_QSAM Is the input QSAM?         02739014
         BNO   C0040                  No, skip                          02740014
                                                                        02741014
*-------------------------------------------------------------------*   02742014
*                                                                   *   02743014
*    The input file is QSAM - just read it normally                 *   02744014
*                                                                   *   02745014
*-------------------------------------------------------------------*   02746014
                                                                        02747014
         L     R0,@_SYSUT1_Rec_Ptr    A(input record)                   02748014
         L     R1,@_SYSUT1_DCB_Ptr    POINT TO INPUT DCB                02749014
         GET   (1),(0)                GO GET THE RECORD                 02750014
         B     C0180                  and skip                          02751014
C0040    DS    0H                                                       02752014
                                                                        02753014
*-------------------------------------------------------------------*   02754014
*                                                                   *   02755014
*    We are using BSAM to access the input file.                    *   02756014
*                                                                   *   02757014
*-------------------------------------------------------------------*   02758014
                                                                        02759014
         NI    @C_Skipping_Segs,X'FF'-L'@C_Skipping_Segs Off our flag   02760014
         SLR   R15,R15                Clear work register               02761014
         ST    R15,@C_SegErr_Seg_Count Clear nbr error segments         02762014
C0050    DS    0H                                                       02763014
         L     R15,@_SYSUT1_Rec_Ptr   A(Record area)                    02764014
         SLR   R0,R0                  Clear work reg                    02765014
         ST    R0,0(0,R15)            Clear record length               02766014
         STH   R0,@C_Seg_Count        Clear segment count               02767014
         NI    @C_Last_Segment,X'FF'-L'@C_Last_Segment Off our flag     02768014
C0060    DS    0H                                                       02769014
         L     R1,@_SYSUT1_BuffArea_Ptr Point to our buffer area        02770014
         ICM   R15,B'1111',BuffArea_Offset-BuffArea(R1) Get offset      02771014
         BNZ   C0070                  OK, skip                          02772014
         L     R1,BuffArea_DECB_Ptr-BuffArea(0,R1) None, A(DECB)        02773014
         CHECK (1)                    Wait for I/O completion           02774014
         L     R1,@_SYSUT1_BuffArea_Ptr Restore buffer pointer          02775014
         LA    R15,4                  Buffer offset                     02776014
         ST    R15,BuffArea_Offset-BuffArea(0,R1) Update it             02777014
         L     R15,@_SYSUT1_PBlock_Count Get Phys Blk Count             02778014
         LA    R15,1(0,R15)           Bump it                           02779014
         ST    R15,@_SYSUT1_PBlock_Count and save it                    02780014
         LA    R15,1                  Initialize segment                02781014
         ST    R15,BuffArea_Seg_Count-BuffArea(0,R1)  count             02782014
C0070    DS    0H                                                       02783014
                                                                        02784014
*-------------------------------------------------------------------*   02785014
*                                                                   *   02786014
*    Check that the next segment is valid, based on the segments    *   02787014
*   that have come before. Segment Type bits:                       *   02788014
*          X'00'  00 - Complete segment                             *   02789014
*          X'01'  01 - 1st segment                                  *   02790014
*          X'02'  10 - Last segment                                 *   02791014
*          X'03'  11 - Intermediate segment                         *   02792014
*                                                                   *   02793014
*-------------------------------------------------------------------*   02794014
                                                                        02795014
         L     R14,@_SYSUT1_Rec_Ptr   A(Input record area)              02796014
         LH    R0,0(0,R14)            Get current record length         02797014
                                                                        02798014
         L     R1,@_SYSUT1_BuffArea_Ptr A(Buffer area)                  02799014
         LA    R15,BuffArea_IOArea-BuffArea(0,R1) A(I/O Area)           02800014
         A     R15,BuffArea_Offset-BuffArea(0,R1) A(Next Segment)       02801014
         TM    2(R15),X'01'           Last or only segment?             02802014
         BNZ   C0080                  No, skip                          02803014
         OI    @C_Last_Segment,L'@C_Last_Segment Yes, set flag          02804014
C0080    DS    0H                                                       02805014
         TM    2(R15),X'02'           1st or complete segment?          02806014
         BZ    C0110                  Yes, skip                         02807014
         LTR   R0,R0                  No, have we processed a 1st?      02808014
         BZ    C0090                  No, segment error                 02809014
         LR    R14,R15                A(Start of segment)               02810014
         LH    R15,0(0,R14)           Segment length                    02811014
         SH    R15,=H'4'              less length of SDW                02812014
         LA    R14,4(0,R14)           point past SDW                    02813014
         B     C0140                  OK, skip                          02814014
C0090    DS    0H                                                       02815014
                                                                        02816014
*---------------------------------------------------------------------* 02817014
*                                                                     * 02818014
*   Segment error - Intermediate or last segment without a 1st.       * 02819014
*  If we are not in the middle of dropping segments, save some info   * 02820014
*  about the start of the segmenting error.                           * 02821014
*                                                                     * 02822014
*---------------------------------------------------------------------* 02823014
                                                                        02824014
         TM    @C_Skipping_Segs,L'@C_Skipping_Segs Already in error?    02825014
         BO    C0100                  Yes, skip                         02826014
         MVC   @C_SegErr_Init_PBlock,@_SYSUT1_PBlock_Count              02827014
         L     R15,@_SYSUT1_BuffArea_Ptr A(Buffarea)                    02828014
         L     R15,BuffArea_Seg_Count-BuffArea(0,R15)                   02829014
         ST    R15,@C_SegErr_Init_Seg  Save segment number              02830014
         ZAP   @C_SegErr_Init_Date,@_Input_Rec_Date                     02831014
         MVC   @C_SegErr_Init_Time,@_Input_Rec_Time                     02832014
         SLR   R15,R15                Number of segments                02833014
         ST    R15,@C_SegErr_Seg_Count Clear segment count              02834014
         OI    @_SYSUT1_Seg_Error,L'@_SYSUT1_Seg_Error                  02835014
         OI    @C_Skipping_Segs,L'@C_Skipping_Segs Segmenting error     02836014
C0100    DS    0H                                                       02837014
         L     R15,@C_SegErr_Seg_Count Get segment count                02838014
         LA    R15,1(0,R15)           Bump it                           02839014
         ST    R15,@C_SegErr_Seg_Count Save it                          02840014
                                                                        02841014
         L     R1,@_SYSUT1_BuffArea_Ptr A(Buffer area)                  02842014
         L     R15,BuffArea_Offset-BuffArea(0,R1) Offset next segment   02843014
         LA    R15,BuffArea_IOArea-BuffArea(R15,R1) A(Next segment)     02844014
         LH    R14,0(0,R15)           Get length of Segment             02845014
         A     R14,8(0,R1)            Bump offset to next segment       02846014
         ST    R14,8(0,R1)            Save it                           02847014
         NI    @C_Last_Segment,L'@C_Last_Segment Off our flag           02848014
         B     C0150                  and skip                          02849014
C0110    DS    0H                                                       02850014
         LTR   R0,R0                  Are we in the middle of a rec?    02851014
         BNZ   C0120                  Yes, error                        02852014
         LR    R14,R15                OK, A(Start of segment)           02853014
         LH    R15,0(0,R14)           Segment length                    02854014
         MVC   @C_Init_PBlock,@_SYSUT1_PBlock_Count Save init PBlock    02855014
         MVC   @C_Init_Seg,BuffArea_Seg_Count-BuffArea(R1) seg #        02856014
         B     C0140                  and skip                          02857014
C0120    DS    0H                                                       02858014
                                                                        02859014
*---------------------------------------------------------------------* 02860014
*                                                                     * 02861014
*   Segment error - No ending segment                                 * 02862014
*  If we are not in the middle of dropping segments, save some info   * 02863014
*  about the start of the segmenting error.                           * 02864014
*                                                                     * 02865014
*---------------------------------------------------------------------* 02866014
                                                                        02867014
         TM    @C_Skipping_Segs,L'@C_Skipping_Segs Already in error?    02868014
         BO    C0130                  Yes, skip                         02869014
         MVC   @C_SegErr_Init_PBlock,@C_Init_PBlock                     02870014
         MVC   @C_SegErr_Init_Seg,@C_Init_Seg                           02871014
         ZAP   @C_SegErr_Init_Date,@_Input_Rec_Date                     02872014
         MVC   @C_SegErr_Init_Time,@_Input_Rec_Time                     02873014
         OI    @C_Skipping_Segs,L'@C_Skipping_Segs Segmenting error     02874014
C0130    DS    0H                                                       02875014
         LH    R15,@C_Seg_Count       Segments in partial record        02876014
         A     R15,@C_SegErr_Seg_Count Bump error count                 02877014
         ST    R15,@C_SegErr_Seg_Count Set error count                  02878014
                                                                        02879014
         B     C0050                  Start again                       02880014
C0140    DS    0H                                                       02881014
                                                                        02882014
*---------------------------------------------------------------------* 02883014
*                                                                     * 02884014
*   At this point, R0  = Current length of input record               * 02885014
*                  R14 = A(Segment from SYSUT1)+4 (ie, past SDW)      * 02886014
*                  R15 = Length of Segment                            * 02887014
*                                                                     * 02888014
*---------------------------------------------------------------------* 02889014
                                                                        02890014
         LR    R1,R0                  Get current length                02891014
         AR    R1,R15                 Get new total length              02892014
         C     R1,@_SYSUT1_Rec_Length Will it be too long?              02893014
         BH    C0120                  Yes, error                        02894014
         A     R0,@_SYSUT1_Rec_Ptr    No, A(Next free area)             02895014
         LR    R1,R15                 Length to move                    02896014
         MVCL  R0,R14                 Move data to input area           02897014
         L     R1,@_SYSUT1_Rec_Ptr    A(Start of input rec area)        02898014
         SR    R0,R1                  Current length of input           02899014
         STH   R0,0(0,R1)             Update RDW                        02900014
         L     R1,@_SYSUT1_BuffArea_Ptr A(Input buffer area)            02901014
         LA    R15,BuffArea_IOArea-BuffArea(0,R1) A(Start of block)     02902014
         SR    R14,R15                Calculate new offset              02903014
         ST    R14,BuffArea_Offset-BuffArea(0,R1) Save it               02904014
         LH    R15,@C_Seg_Count       Get segments in curr rec          02905014
         LA    R15,1(0,R15)           Bump it                           02906014
         STH   R15,@C_Seg_Count       Save it                           02907014
C0150    DS    0H                                                       02908014
         L     R15,@_SYSUT1_BuffArea_Ptr A(Buffer area)                 02909014
         L     R14,BuffArea_Offset-BuffArea(0,R1) Get offset            02910014
         CH    R14,BuffArea_IOArea-BuffArea(0,R15) Done the block?      02911014
         BL    C0160                  No, skip                          02912014
         L     R2,@_SYSUT1_DCB_Ptr    A(SYSUT1 DCB)                     02913014
         L     R4,@_SYSUT1_BuffArea_Ptr A(buffer area)                  02914014
         L     R5,BuffArea_DECB_Ptr-BuffArea(0,R4) A(DECB)              02915014
         MVC   0(C_READ_Length,R5),C_READ  Initialize DECB              02916014
         LA    R6,BuffArea_IOArea-BuffArea(0,R4) A(I/O Area)            02917014
         READ  (5),SF,(2),(6),'S',MF=E    Issue the read                02918014
         SLR   R15,R15                Clear work register               02919014
         ST    R15,BuffArea_Offset-Buffarea(0,R4) Clear offset          02920014
         L     R15,@_SYSUT1_BuffArea_Ptr A(Current buffer area)         02921014
         L     R15,BuffArea_Next_Ptr-BuffArea(0,R15) A(Next)            02922014
         ST    R15,@_SYSUT1_BuffArea_Ptr and save the address           02923014
C0160    DS    0H                                                       02924014
         TM    @C_Last_Segment,L'@C_Last_Segment  All done?             02925014
         BZ    C0060                  No, keep going                    02926014
                                                                        02927014
         TM    @C_Skipping_Segs,L'@C_Skipping_Segs Yes, segs skipped?   02928014
         BZ    C0180                  No, OK                            02929014
                                                                        02930014
         OI    @_SYSUT1_Seg_Error,L'@_SYSUT1_Seg_Error Yes, set flag    02931014
         L     R2,@_SYSPRINT_Rec_Ptr  Get A(SYSPRINT record)            02932014
         MVC   2(3,R2),=C'***'        Move flag                         02933014
         MVC   5(7,R2),=X'4020206B202120' Segment count mask            02934014
         L     R15,@C_SegErr_Seg_Count Get segments dropped             02935014
         CVD   R15,@C_DBLWD           Pack it                           02936014
         ED    5(7,R2),@C_DBLWD+5     Edit it in                        02937014
         MVC   13(7,R2),=C'segment'   Start of literal                  02938014
         LA    R2,20(0,R2)            point to next character           02939014
         C     R15,=FL4'1'            More than 1 segment?              02940014
         BNH   C0170                  No, skip                          02941014
         MVI   0(R2),C's'             Yes, pluralize it                 02942014
         LA    R2,1(0,R2)             and bump output pointer           02943014
C0170    DS    0H                                                       02944014
         MVC   0(26,R2),=C' dropped, from (PBlk:Seg) '                  02945014
         L     R15,@C_SegErr_Init_PBlock  Get starting phys block       02946014
         CVD   R15,@C_DBLWD          Pack it                            02947014
         UNPK  26(7,R2),@C_DBLWD+4(4) Unpack it                         02948014
         OI    32(R2),X'F0'          Make it readable                   02949014
         MVI   33(R2),C':'           Move separator                     02950014
         L     R15,@C_SegErr_Init_Seg   Get starting segment number     02951014
         CVD   R15,@C_DBLWD          Pack it                            02952014
         UNPK  34(3,R2),@C_DBLWD+6(2) Unpack it                         02953014
         OI    36(R2),X'F0'          Make it readable                   02954014
         MVC   37(15,R2),=C' to (PBlk:Seg) '                            02955014
         L     R15,@C_Init_PBlock    Get starting phys block            02956014
         CVD   R15,@C_DBLWD          Pack it                            02957014
         UNPK  52(7,R2),@C_DBLWD+4(4) Unpack it                         02958014
         OI    58(R2),X'F0'          Make it readable                   02959014
         MVI   59(R2),C':'           Move separator                     02960014
         L     R15,@C_SegErr_Init_Seg   Get starting segment number     02961014
         CVD   R15,@C_DBLWD          Pack it                            02962014
         UNPK  60(3,R2),@C_DBLWD+6(2) Unpack it                         02963014
         OI    62(R2),X'F0'          Make it readable                   02964014
         L     R15,=AL4(G0010)        A(Print routine)                  02965014
         BASR  R14,R15                PRINT THIS LINE                   02966014
         L     R2,@_SYSPRINT_Rec_Ptr  A(SYSPRINT record)                02967014
         MVC   6(10,R2),=C'Data after'                                  02968014
                                                                        02969014
         XC    @C_DateConv_Area,@C_DateConv_Area Clear Date area        02970014
         ZAP   @C_DateConv_Area+DConv_Date_SMFDate-DateConv_Area(L'DCon+02971014
               v_Date_SMFDate),@C_SegErr_Init_Date  Date                02972014
         MVC   @C_DateConv_Area+DConv_Time-DateConv_Area(L'DConv_Time),+02973014
               @C_SegErr_Init_Time            Time too                  02974014
         OI    @C_DateConv_Area+DConv_Input_SMFDate-DateConv_Area,L'DCo+02975014
               nv_Input_SMFDate     Indicate SMF Date                   02976014
         OI    @C_DateConv_Area+DConv_Input_Time-DateConv_Area,L'DConv_+02977014
               Input_Time           Indicate SMF Time                   02978014
         LA    R15,@C_DateConv_Area     Point to Date Conversion area   02979014
         ST    R15,@C_FParms         Save as 1st parm                   02980014
         LA    R15,17(0,R2)          A(Output area)                     02981014
         ST    R15,@C_FParms+4       Save as 2nd parm                   02982014
         LA    R15,@C_Return_Ptr     A(Return area)                     02983014
         ST    R15,@C_FParms+8       Save it                            02984014
         LA    R1,@C_FParms          A(Parm pointers)                   02985014
         L     R15,=AL4(F0010)       A(Format routine)                  02986014
         BASR  R14,R15               Go convert date and time           02987014
                                                                        02988014
         L     R15,=AL4(G0010)        A(Print routine)                  02989014
         BASR  R14,R15                PRINT THIS LINE                   02990014
         L     R2,@_SYSPRINT_Rec_Ptr  A(SYSPRINT record)                02991014
         LA    R2,6(0,R2)             Bump pointer                      02992014
                                                                        02993014
         MVC   0(10,R2),=C'and before'                                  02994014
         XC    @C_DateConv_Area,@C_DateConv_Area Clear Date area        02995014
         L     R1,@_SYSUT1_Rec_Ptr   A(Input record)                    02996014
         ZAP   @C_DateConv_Area+DConv_Date_SMFDate-DateConv_Area(L'DCon+02997014
               v_Date_SMFDate),SMF0DTE-SMFRCD0(L'SMF0DTE,R1)            02998014
         MVC   @C_DateConv_Area+DConv_Time-DateConv_Area(L'DConv_Time),+02999014
               SMF0TME-SMFRCD0(R1)          Time too                    03000014
         OI    @C_DateConv_Area+DConv_Input_SMFDate-DateConv_Area,L'DCo+03001014
               nv_Input_SMFDate     Indicate SMF Date                   03002014
         OI    @C_DateConv_Area+DConv_Input_Time-DateConv_Area,L'DConv_+03003014
               Input_Time           Indicate SMF Time                   03004014
         LA    R15,@C_DateConv_Area     Point to Date Conversion area   03005014
         ST    R15,@C_FParms         Save as 1st parm                   03006014
         LA    R15,11(0,R2)          A(Output area)                     03007014
         ST    R15,@C_FParms+4       Save as 2nd parm                   03008014
         LA    R15,@C_Return_Ptr     A(Return area)                     03009014
         ST    R15,@C_FParms+8       Save it                            03010014
         LA    R1,@C_FParms          A(Parm pointers)                   03011014
         L     R15,=AL4(F0010)       A(Format routine)                  03012014
         BASR  R14,R15               Go convert date and time           03013014
         L     R2,@C_Return_Ptr      A(Return area)                     03014014
         MVC   1(17,R2),=C' are unavailable.'                           03015014
                                                                        03016014
         L     R15,=AL4(G0010)       A(Format routine)                  03017014
         BASR  R14,R15               Go convert date and time           03018014
                                                                        03019014
         NI    @C_Skipping_Segs,X'FF'-L'@C_Skipping_Segs                03020014
         B     C0180                  and skip                          03021014
C0180    DS    0H                                                       03022014
         L     R1,@_SYSUT1_Rec_Ptr    AND POINT TO OUR RECORD           03023014
                                                                        03024014
*-------------------------------------------------------------------*   03025014
*                                                                   *   03026014
*    Bump the input counters                                        *   03027014
*                                                                   *   03028014
*-------------------------------------------------------------------*   03029014
                                                                        03030014
         L     R15,@_Input_Count      GET THE INPUT COUNTER             03031014
         LA    R15,1(0,R15)           BUMP IT                           03032014
         ST    R15,@_Input_Count                                        03033014
                                                                        03034014
         SLR   R15,R15                Clear work regiater               03035014
         IC    R15,SMF0RTY-SMFRCD0(R1) Get record type                  03036014
         SLL   R15,2                  times 4                           03037014
         LA    R15,@_Input_Count_Table(R15) A(Our counter)              03038014
         L     R0,0(0,R15)            Get count                         03039014
         AL    R0,=FL4'1'             Bump it                           03040014
         ST    R0,0(0,R15)            Save it                           03041014
                                                                        03042014
*-------------------------------------------------------------------*   03043014
*                                                                   *   03044014
*    SAVE THE DATE/TIME OF THE FIRST RECORD.                        *   03045014
*                                                                   *   03046014
*-------------------------------------------------------------------*   03047014
                                                                        03048014
         CLI   SMF0RTY-SMFRCD0(R1),X'02' DUMP HEADER?                   03049014
         BNE   C0200                  NO, SKIP                          03050014
         CP    @_Dump_Start_Date,SMF0DTE-SMFRCD0(L'SMF0DTE,R1) 1st?     03051014
         BL    C0230                  NO, SKIP IT                       03052014
         BH    C0190                  YES, SAVE IT                      03053014
         L     R15,@_Dump_Start_Time  SAME DAY, GET PREV TIME           03054014
         C     R15,SMF0TME-SMFRCD0(R1) COMPARE AGAINST THIS ONE         03055014
         BNH   C0230                  NOT EARLIEST, SKIP                03056014
C0190    DS    0H                                                       03057014
         ZAP   @_Dump_Start_Date,SMF0DTE-SMFRCD0(L'SMF0DTE,R1) Save it  03058014
         MVC   @_Dump_Start_Time,SMF0TME-SMFRCD0(R1)                    03059014
         B     C0230                  AND SKIP                          03060014
C0200    DS    0H                                                       03061014
         CLI   SMF0RTY-SMFRCD0(R1),X'03' DUMP TRAILER                   03062014
         BNE   C0220                  NO, SKIP                          03063014
         CP    @_Dump_End_Date,SMF0DTE-SMFRCD0(L'SMF0DTE,R1) LATEST?    03064014
         BH    C0230                  NO, SKIP IT                       03065014
         BL    C0210                  YES, SAVE IT                      03066014
         L     R15,@_Dump_End_Time    SAME DAY, GET PREV TIME           03067014
         C     R15,SMF0TME-SMFRCD0(R1) COMPARE AGAINST THIS ONE         03068014
         BNL   C0230                  NOT EARLIEST, SKIP                03069014
C0210    DS    0H                                                       03070014
         ZAP   @_Dump_End_Date,SMF0DTE-SMFRCD0(L'SMF0DTE,R1)            03071014
         MVC   @_Dump_End_Time,SMF0TME-SMFRCD0(R1)                      03072014
         B     C0230                  AND SKIP                          03073014
C0220    DS    0H                                                       03074014
         ZAP   @_Input_Rec_Date,SMF0DTE-SMFRCD0(L'SMF0DTE,R1) SAVE DATE 03075014
         MVC   @_Input_Rec_Time,SMF0TME-SMFRCD0(R1) AND TIME            03076014
         CP    @_First_Rec_Date,=P'0' HAVE WE FOUND THE FIRST DATE?     03077014
         BNE   C0230                  YES,SKIP                          03078014
         ZAP   @_First_Rec_Date,SMF0DTE-SMFRCD0(L'SMF0DTE,R1) SAVE DATE 03079014
         MVC   @_First_Rec_Time,SMF0TME-SMFRCD0(R1) AND TIME            03080014
C0230    DS    0H                                                       03081014
                                                                        03082014
*-------------------------------------------------------------------*   03083014
*                                                                   *   03084014
*    WE'VE FINISHED WITH THIS RECORD, SO WE CLEAR THE RETURN CODE   *   03085014
*   AND EXIT.                                                       *   03086014
*                                                                   *   03087014
*-------------------------------------------------------------------*   03088014
                                                                        03089014
         SLR   R15,R15                CLEAR THE RETURN CODE             03090014
         B     C9000                  AND EXIT                          03091014
C7000    DS    0H                                                       03092014
                                                                        03093014
*-------------------------------------------------------------------*   03094014
*                                                                   *   03095014
*    END-OF-FILE EXIT FOR SYSUT1.                                   *   03096014
*                                                                   *   03097014
*-------------------------------------------------------------------*   03098014
                                                                        03099014
         TM    @_SYSUT1_BSAM,L'@_SYSUT1_BSAM Are we using BSAM?         03100014
         BNO   C7010                  No, skip                          03101014
         L     R15,@_SYSUT1_Rec_Ptr   Yes, A(SYSUT1 record)             03102014
         LH    R15,0(0,R15)           Get length of record              03103014
         LTR   R15,R15                Anything there?                   03104014
         BZ    C7010                  No, OK                            03105014
                                                                        03106014
         L     R2,@_SYSPRINT_Rec_Ptr  Get A(SYSPRINT record)            03107014
         MVC   2(3,R2),=C'***'        Move flag                         03108014
         MVC   5(7,R2),=X'4020206B202120' Segment count mask            03109014
         LH    R15,@C_Seg_Count       Get segments dropped              03110014
         CVD   R15,@C_DBLWD           Pack it                           03111014
         ED    5(7,R2),@C_DBLWD+5     Edit it in                        03112014
         MVC   12(35,R2),=C' segments dropped, from (PBlk:Seg) '        03113014
         L     R15,@C_Init_PBlock  Get starting phys block              03114014
         CVD   R15,@C_DBLWD          Pack it                            03115014
         UNPK  47(7,R2),@C_DBLWD+4(4) Unpack it                         03116014
         OI    53(R2),X'F0'          Make it readable                   03117014
         MVI   54(R2),C':'           Move separator                     03118014
         L     R15,@C_Init_Seg   Get starting segment number            03119014
         CVD   R15,@C_DBLWD          Pack it                            03120014
         UNPK  55(3,R2),@C_DBLWD+6(2) Unpack it                         03121014
         OI    57(R2),X'F0'          Make it readable                   03122014
         MVC   58(21,R2),=C' to the end of SYSUT1'                      03123014
         L     R15,=AL4(G0010)        A(Print routine)                  03124014
         BASR  R14,R15                PRINT THIS LINE                   03125014
         L     R2,@_SYSPRINT_Rec_Ptr  A(SYSPRINT record)                03126014
         MVC   6(11,R2),=C'Data after '                                 03127014
                                                                        03128014
         XC    @C_DateConv_Area,@C_DateConv_Area Clear Date area        03129014
         ZAP   @C_DateConv_Area+DConv_Date_SMFDate-DateConv_Area(L'DCon+03130014
               v_Date_SMFDate),@_Input_Rec_Date     Date                03131014
         MVC   @C_DateConv_Area+DConv_Time-DateConv_Area(L'DConv_Time),+03132014
               @_Input_Rec_Time               Time too                  03133014
         OI    @C_DateConv_Area+DConv_Input_SMFDate-DateConv_Area,L'DCo+03134014
               nv_Input_SMFDate     Indicate SMF Date                   03135014
         OI    @C_DateConv_Area+DConv_Input_Time-DateConv_Area,L'DConv_+03136014
               Input_Time           Indicate SMF Time                   03137014
         LA    R15,@C_DateConv_Area     Point to Date Conversion area   03138014
         ST    R15,@C_FParms         Save as 1st parm                   03139014
         LA    R15,17(0,R2)          A(Output area)                     03140014
         ST    R15,@C_FParms+4       Save as 2nd parm                   03141014
         LA    R15,@C_Return_Ptr     A(Return area)                     03142014
         ST    R15,@C_FParms+8       Save it                            03143014
         LA    R1,@C_FParms          A(Parm pointers)                   03144014
         L     R15,=AL4(F0010)       A(Format routine)                  03145014
         BASR  R14,R15               Go convert date and time           03146014
                                                                        03147014
         L     R2,@C_Return_Ptr      A(Return area)                     03148014
         MVC   1(17,R2),=C' are unavailable.'                           03149014
         L     R15,=AL4(G0010)        A(Print routine)                  03150014
         BASR  R14,R15                PRINT THIS LINE                   03151014
         NI    @C_Skipping_Segs,X'FF'-L'@C_Skipping_Segs                03152014
                                                                        03153014
C7010    DS    0H                                                       03154014
                                                                        03155014
*-------------------------------------------------------------------*   03156014
*                                                                   *   03157014
*    CLOSE SYSUT1 ...                                               *   03158014
*                                                                   *   03159014
*-------------------------------------------------------------------*   03160014
                                                                        03161014
         L     R2,@_SYSUT1_DCB_Ptr     A(DCB or ACB)                    03162014
         MVC   @C_CLOSE,C_CLOSE        Move CLOSE parms                 03163014
         CLOSE ((2)),MODE=31,MF=(E,@C_CLOSE)   and CLOSE DCB/ACB        03164014
                                                                        03165014
         ICM   R1,B'1111',@_SYSUT1_Rec_Ptr  GET A(INPUT RECORD AREA)    03166014
         BZ    C7020                  None, skip                        03167014
         L     R0,@_SYSUT1_Rec_Length YES, GET ITS LEN                  03168014
         LA    R15,7                  Make                              03169014
         AR    R0,R15                  a                                03170014
         SRL   R0,3                    doubleword                       03171014
         SLL   R0,3                    multiple                         03172014
         STORAGE RELEASE,               Free our storage               +03173014
               ADDR=(1),                 starting here                 +03174014
               LENGTH=(0)                for this long                  03175014
         SLR   R15,R15                Clear work register               03176014
         ST    R15,@_SYSUT1_Rec_Ptr   No input record                   03177014
C7020    DS    0H                                                       03178014
         ICM   R1,B'1111',@_SYSUT1_AtL_Ptr A(Above-the-line storage)    03179014
         BZ    C7030                  None, skip                        03180014
         L     R0,0(0,R1)             Get Subpool, length               03181014
         STORAGE RELEASE,               Free our storage               +03182014
               ADDR=(1),                 starting here                 +03183014
               LENGTH=(0)                for this long                  03184014
         SLR   R15,R15                Clear work register               03185014
         ST    R15,@_SYSUT1_AtL_Ptr   No storage now                    03186014
C7030    DS    0H                                                       03187014
         ICM   R1,B'1111',@_SYSUT1_BtL_Ptr A(Below-the-line storage)    03188014
         BZ    C7040                  None, skip                        03189014
         L     R0,0(0,R1)             Get Subpool, length               03190014
         STORAGE RELEASE,               Free our storage               +03191014
               ADDR=(1),                 starting here                 +03192014
               LENGTH=(0)                for this long                  03193014
         SLR   R15,R15                Clear work register               03194014
         ST    R15,@_SYSUT1_BtL_Ptr   No storage now                    03195014
                                                                        03196014
C7040    DS    0H                                                       03197014
         LA    R15,4                  SET THE RETURN CODE               03198014
         B     C9000                  AND EXIT                          03199014
                                                                        03200014
C9000    DS    0H                                                       03201014
                                                                        03202014
*-------------------------------------------------------------------*   03203014
*                                                                   *   03204014
*    Free up our local storage ...                                  *   03205014
*                                                                   *   03206014
*-------------------------------------------------------------------*   03207014
                                                                        03208014
         LR    R3,R15                 Save return code                  03209014
                                                                        03210014
         LA    R1,@C_Dynam              A(Local storage)                03211014
         L     R13,4(0,R13)             Rescue A(HSA)                   03212014
                                                                        03213014
         STACK POP,                     Free the stack area            +03214014
               ADDR=(R1),                starting here                 +03215014
               STACK=@_31Bit_Stack_Ptr   on this stack                  03216014
                                                                        03217014
         LR    R15,R3                   Restore return code             03218014
                                                                        03219014
*-------------------------------------------------------------------*   03220014
*                                                                   *   03221014
*    ... and return to caller                                       *   03222014
*                                                                   *   03223014
*-------------------------------------------------------------------*   03224014
                                                                        03225014
         L     R14,12(0,R13)            Restore return address          03226014
         LM    R0,R12,20(R13)           Restore other registers         03227014
         BSM   0,R14                    and return                      03228014
                                                                        03229014
         PUSH PRINT                                                     03230014
         PRINT NOGEN                                                    03231014
                                                                        03232014
         READ  C_READ,SF,0,0,'S',MF=L                                   03233014
C_READ_Length EQU  *-C_READ                                             03234014
                                                                        03235014
C_CLOSE    CLOSE (0),MODE=31,MF=L                                       03236014
C_CLOSE_Length EQU   *-C_CLOSE                                          03237014
                                                                        03238014
         POP   PRINT                                                    03239014
                                                                        03240014
         LTORG                                                          03241014
                                                                        03242014
@C_Dynam              DSECT           Dynamic area for C                03243014
@C_Save               DS   18F         O/S Style save area              03244014
@C_DBLWD              DS   D           Work area                        03245014
@C_Return_Ptr         DS   AL4         A(Last byte formatted by F0010)  03246014
@C_Seg_Count          DS   HL2         Nbr segs in curr record          03247014
@C_Init_PBlock        DS   FL4         Phys Block of start of record    03248014
@C_Init_Seg           DS   FL4         Seg # of start of record         03249014
@C_SegErr_Init_PBlock DS   FL4         Phys Blk of start of seg err     03250014
@C_SegErr_Init_Seg    DS   FL4         Segment# of start of seg err     03251014
@C_SegErr_Init_Date   DS   PL4         Date of valid rec pre seg err    03252014
@C_SegErr_Init_Time   DS   FL4         Time of valid rec pre seg err    03253014
@C_SegErr_Seg_Count   DS   FL4         Nbr segments dropped             03254014
                      DS   X           Flag byte                        03255014
@C_Last_Segment       EQU  *-1,X'80'    1... .... Last segment          03256014
@C_Skipping_Segs      EQU  *-1,X'40'    .1.. .... Skipping Segments     03257014
                                                                        03258014
@C_Macro              DS   0F          Temp area for Macro L-Forms      03259014
@C_EParms             EQU  *            Parms for E0010                 03260014
@C_EP_Input_Ptr       DS   AL4           A(INPUT AREA)                  03261014
@C_EP_Input_Len       DS   FL4           LEN OF INPUT AREA              03262014
@C_EP_Input_Off       DS   FL4           CURRENT OFFSET                 03263014
@C_EP_Output_Ptr      DS   AL4           A(OUTPUT AREA)                 03264014
@C_EP_Output_Len      DS   FL4           OUTPUT AREA LENGTH             03265014
@C_EP_Pr_Flag_Ptr     DS   AL4           A(Print flag)                  03266014
                      ORG  @C_Macro                                     03267014
@C_DateConv_Area      DS   CL(DConv_Area_Length)  Date conversion area  03268014
@C_FParms             DS   3AL4          Parms for F0010                03269014
                      ORG  @C_Macro                                     03270014
@C_SHOWCB             DS   CL(C_SHOWCB_Length) SHOWCB Macro             03271014
@C_SYSUT1_EXLST_Ptr   DS   AL4           A(SYSUT1 EXLST)                03272014
                      ORG  @C_Macro                                     03273014
@C_MODCB              DS   CL(C_MODCB_Length) MODCB Macro               03274014
                      ORG  @C_Macro                                     03275014
@C_Close              DS   CL(C_CLOSE_Length)  Close macro              03276014
                                                                        03277014
                      ORG  ,                                            03278014
                      DS   0D          Alignment                        03279014
@C_DynLen             EQU  *-@C_Dynam Length of storage required        03280014
                                                                        03281014
SMFSLCT  RSECT                                                          03282014
                                                                        03283014
         DROP  R11,R13                                                  03284014
         POP   USING                                                    03285014
         TITLE 'D0000: Process selected record'                         03286014
                                                                        03287014
         PUSH  USING                                                    03288014
                                                                        03289014
D0010    DS    0H                                                       03290014
                                                                        03291014
*---------------------------------------------------------------------* 03292014
*                                                                     * 03293014
*  Routine   : D0010                                                  * 03294014
*                                                                     * 03295014
*  Abstract  : Write the selected input record to the output file(s). * 03296014
*                                                                     * 03297014
*  Inputs    : R1 ----> A(Input SMF record)                           * 03298014
*                       A(ODTE entry)                                 * 03299014
*                                                                     * 03300014
*  Outputs   : R15: 0 - OK                                            * 03301014
*                   4 - Error encountered                             * 03302014
*                                                                     * 03303014
*  Notes     :                                                        * 03304014
*                                                                     * 03305014
*  History:                                                           * 03306014
*              1999/04/24 SDDA030 V1.1                                * 03307014
*                                 Used LIFO Stack for our dynamic     * 03308014
*                                 areas.                              * 03309014
*              ____/__/__ _______                                     * 03310014
*                                                                     * 03311014
*---------------------------------------------------------------------* 03312014
                                                                        03313014
         USING *,R15                                                    03314014
         SAVE  (14,12),T                                                03315014
         LR    R11,R15                  Load permanent base reg         03316014
         DROP  R15                      Free up temp base               03317014
         USING D0010,R11                Assign permanent base           03318014
                                                                        03319014
*-------------------------------------------------------------------*   03320014
*                                                                   *   03321014
*    Get an area on the stack for our local storage                 *   03322014
*                                                                   *   03323014
*-------------------------------------------------------------------*   03324014
                                                                        03325014
         STACK PUSH,                    Get Stack area                 +03326014
               LEN=@D_DynLen,            this long                     +03327014
               STACK=@_31Bit_Stack_Ptr   using this stack               03328014
                                                                        03329014
         LR    R2,R1                    Save its address                03330014
         LR    R0,R1                    A(Our storage)                  03331014
         LA    R1,@D_DynLen              Its length                     03332014
         SLR   R14,R14                  Clear source address            03333014
         SLR   R15,R15                   and length                     03334014
         MVCL  R0,R14                   Clear our storage               03335014
         LR    R1,R2                    Restore A(Our storage)          03336014
                                                                        03337014
*-------------------------------------------------------------------*   03338014
*                                                                   *   03339014
*    Chain our save areas, and restore the important registers      *   03340014
*   that we have destroyed                                          *   03341014
*                                                                   *   03342014
*-------------------------------------------------------------------*   03343014
                                                                        03344014
         ST    R13,4(0,R1)              Chain                           03345014
         ST    R1,8(0,R13)               saveareas                      03346014
         LR    R13,R1                   Load dynam base                 03347014
         USING @D_Dynam,R13             Assign a base                   03348014
         L     R15,@D_Dynam+4           Get A(HSA)                      03349014
         LM    R0,R3,20(R15)            Restore callers registers       03350014
                                                                        03351014
         NI    @D_RDW_Updated,X'FF'-L'@D_RDW_Updated  Off our flag      03352014
                                                                        03353014
*-------------------------------------------------------------------*   03354014
*                                                                   *   03355014
*    Get the address of the SMF record, and the ODT Entry that      *   03356014
*   selected it                                                     *   03357014
*                                                                   *   03358014
*-------------------------------------------------------------------*   03359014
                                                                        03360014
         L     R15,0(0,R1)              A(SMF record)                   03361014
                                                                        03362014
         L     R10,4(0,R1)              A(ODT Entry)                    03363014
         USING ODT_Entry,R10            Tell the assembler              03364014
                                                                        03365014
         LR    R1,R15                   Set up A(SMF record)            03366014
                                                                        03367014
*-------------------------------------------------------------------*   03368014
*                                                                   *   03369014
*    WE HAVE SELECTED A RECORD. WE BUMP THE SELECTED COUNT, PRINT   *   03370014
*   IT (IF REQUIRED) AND WRITE IT (IF REQUIRED) TO SYSUT2.          *   03371014
*                                                                   *   03372014
*   ON ENTRY, R1 = A(SELECTED RECORD)                               *   03373014
*                                                                   *   03374014
*-------------------------------------------------------------------*   03375014
                                                                        03376014
         L     R15,ODTE_Selected      GET SELECTED COUNT                03377014
         LA    R15,1(0,R15)           BUMP IT                           03378014
         ST    R15,ODTE_Selected      AND SAVE IT                       03379014
                                                                        03380014
         SLR   R15,R15                Clear work regiater               03381014
         IC    R15,SMF0RTY-SMFRCD0(R1) Get record type                  03382014
         SLL   R15,2                  times 4                           03383014
         LA    R15,ODTE_Output_Count_Table(R15) A(Our counter)          03384014
         L     R0,0(0,R15)            Get count                         03385014
         AL    R0,=FL4'1'             Bump it                           03386014
         ST    R0,0(0,R15)            Save it                           03387014
                                                                        03388014
         ICM   R15,B'1111',ODTE_DCB_Ptr A(Output DCB)                   03389014
         BZ    D0110                  None, exit                        03390014
         TM    DCBOFLGS-IHADCB(R15),DCBOFOPN IS DCB OPEN?               03391014
         BZ    D0110                  NO, exit quick smart              03392014
                                                                        03393014
*-------------------------------------------------------------------*   03394014
*                                                                   *   03395014
*    AND WRITE IT TO THE OUTPUT, IF POSSIBLE.                       *   03396014
*                                                                   *   03397014
*-------------------------------------------------------------------*   03398014
                                                                        03399014
         L     R2,@_SYSUT1_Rec_Ptr    SAVE A(INPUT RECORD)              03400014
         ICM   R1,B'1111',ODTE_DCB_Ptr  A(Output DCB)                   03401014
         BZ    D9000                  None, skip                        03402014
         TM    DCBOFLGS-IHADCB(R1),DCBOFOPN IS OUTPUT OPEN?             03403014
         BZ    D9000                  NO, SKIP                          03404014
         LH    R3,SMF0LEN-SMFRCD0(R2) GET INPUT RECORD LEN              03405014
         LH    R0,ODTE_LRECL          GET MAX OUTPUT RECORD LEN         03406014
         TM    DCBRECFM-IHADCB(R1),DCBRECV OUTPUT RECFM=F?              03407014
         BO    D0080                  NO, SKIP                          03408014
         LA    R2,4(0,R2)             YES, POINT PAST RDW               03409014
         SH    R3,=H'4'                DECREMENT THE LENGTH             03410014
         B     D0100                   and skip                         03411014
D0080    DS    0H                                                       03412014
         CR    R3,R0                  INPUT GREATER THAN MAX OUTPUT?    03413014
         BNH   D0090                  NO, OK                            03414014
         TM    DCBRECFM-IHADCB(R1),DCBRECV+DCBRECSB RECFM=VS?           03415014
         BO    D0090                  YES, SKIP                         03416014
         OI    ODTE_Trunc,L'ODTE_Trunc INDICATE TRUNCATION              03417014
         STH   R0,SMF0LEN-SMFRCD0(R2) Update RDW temporarily            03418014
         OI    @D_RDW_Updated,L'@D_RDW_Updated  and set our flag        03419014
D0090    DS    0H                                                       03420014
         TM    DCBRECFM-IHADCB(R1),DCBRECU OUT RECFM = U?               03421014
         BNO   D0100                  NO, SKIP                          03422014
         STH   R3,DCBLRECL-IHADCB(R1) YES, UPDATE LRECL                 03423014
D0100    DS    0H                                                       03424014
         LR    R0,R2                  A(Output record)                  03425014
         PUT   (1),(0)                Write output                      03426014
                                                                        03427014
         TM    @D_RDW_Updated,L'@D_RDW_Updated  Did we change RDW?      03428014
         BNO   D0110                  No, skip                          03429014
         STH   R3,SMF0LEN-SMFRCD0(R2) Yes, restore it                   03430014
         NI    @D_RDW_Updated,X'FF'-L'@D_RDW_Updated  and off our flag  03431014
                                                                        03432014
D0110    DS    0H                                                       03433014
         SLR   R15,R15                Clear return code                 03434014
         B     D9000                  and exit                          03435014
                                                                        03436014
D9000    DS    0H                                                       03437014
                                                                        03438014
*-------------------------------------------------------------------*   03439014
*                                                                   *   03440014
*    Free up our local storage ...                                  *   03441014
*                                                                   *   03442014
*-------------------------------------------------------------------*   03443014
                                                                        03444014
         LR    R3,R15                 Save return code                  03445014
                                                                        03446014
         LA    R1,@D_Dynam              A(Local storage)                03447014
         L     R13,4(0,R13)             Rescue A(HSA)                   03448014
                                                                        03449014
         STACK POP,                     Free the stack area            +03450014
               ADDR=(R1),                starting here                 +03451014
               STACK=@_31Bit_Stack_Ptr   on this stack                  03452014
                                                                        03453014
         LR    R15,R3                   Restore return code             03454014
                                                                        03455014
*-------------------------------------------------------------------*   03456014
*                                                                   *   03457014
*    ... and return to caller                                       *   03458014
*                                                                   *   03459014
*-------------------------------------------------------------------*   03460014
                                                                        03461014
         L     R14,12(0,R13)            Restore return address          03462014
         LM    R0,R12,20(R13)           Restore other registers         03463014
         BSM   0,R14                    and return                      03464014
                                                                        03465014
                                                                        03466014
         LTORG                                                          03467014
                                                                        03468014
@D_Dynam        DSECT                 Dynamic area for D                03469014
@D_Save         DS    18F              O/S Style save area              03470014
                DS    X                Flag Byte                        03471014
@D_RDW_Updated  EQU   *-1,X'80'         1... .... Record length updated 03472014
                DS    0D               Alignment                        03473014
@D_DynLen       EQU   *-@D_Dynam      Length of storage required        03474014
                                                                        03475014
SMFSLCT  RSECT                                                          03476014
                                                                        03477014
         DROP  R10                    Free ODT Entry base               03478014
         DROP  R11,R13                                                  03479014
         POP   USING                                                    03480014
         TITLE 'E0000: FORMAT RECORD FOR PRINT'                         03481014
                                                                        03482014
         PUSH  USING                                                    03483014
                                                                        03484014
E0010    DS    0H                                                       03485014
                                                                        03486014
*---------------------------------------------------------------------* 03487014
*                                                                     * 03488014
*  Routine   : E0010                                                  * 03489014
*                                                                     * 03490014
*  Abstract  : Print selected input record                            * 03491014
*                                                                     * 03492014
*  Inputs    : R1 ----> A(Input SMF record)                           * 03493014
*                       A(ODTE entry)                                 * 03494014
*                                                                     * 03495014
*  Outputs   : R15: 0 - OK                                            * 03496014
*                   4 - Error encountered                             * 03497014
*                                                                     * 03498014
*  Notes     :                                                        * 03499014
*                                                                     * 03500014
*  History:                                                           * 03501014
*              1999/04/24 SDDA030 V1.1                                * 03502014
*                                 Used LIFO Stack for our dynamic     * 03503014
*                                 areas.                              * 03504014
*              ____/__/__ _______                                     * 03505014
*                                                                     * 03506014
*---------------------------------------------------------------------* 03507014
                                                                        03508014
         USING *,R15                                                    03509014
         SAVE  (14,12),T                                                03510014
         LR    R11,R15                  Load permanent base reg         03511014
         DROP  R15                      Free up temp base               03512014
         USING E0010,R11                Assign permanent base           03513014
                                                                        03514014
*-------------------------------------------------------------------*   03515014
*                                                                   *   03516014
*    Get an area on the stack for our local storage                 *   03517014
*                                                                   *   03518014
*-------------------------------------------------------------------*   03519014
                                                                        03520014
         STACK PUSH,                    Get Stack area                 +03521014
               LEN=@E_DynLen,            this long                     +03522014
               STACK=@_31Bit_Stack_Ptr   using this stack               03523014
                                                                        03524014
         LR    R2,R1                    Save its address                03525014
         LR    R0,R1                    A(Our storage)                  03526014
         LA    R1,@E_DynLen              Its length                     03527014
         SLR   R14,R14                  Clear source address            03528014
         SLR   R15,R15                   and length                     03529014
         MVCL  R0,R14                   Clear our storage               03530014
         LR    R1,R2                    Restore A(Our storage)          03531014
                                                                        03532014
*-------------------------------------------------------------------*   03533014
*                                                                   *   03534014
*    Chain our save areas, and restore the important registers      *   03535014
*   that we have destroyed                                          *   03536014
*                                                                   *   03537014
*-------------------------------------------------------------------*   03538014
                                                                        03539014
         ST    R13,4(0,R1)              Chain                           03540014
         ST    R1,8(0,R13)               saveareas                      03541014
         LR    R13,R1                   Load dynam base                 03542014
         USING @E_Dynam,R13             Assign a base                   03543014
         L     R15,@E_Dynam+4           Get A(HSA)                      03544014
         LM    R0,R3,20(R15)            Restore callers registers       03545014
                                                                        03546014
*-------------------------------------------------------------------*   03547014
*                                                                   *   03548014
*    Save the addresses of the input record, and of our ODT entry   *   03549014
*                                                                   *   03550014
*-------------------------------------------------------------------*   03551014
                                                                        03552014
         L     R15,0(0,R1)            Get A(Input record)               03553014
         ST    R15,@E_Record_Ptr      Save it                           03554014
         L     R15,4(0,R1)            Get A(ODT Entry)                  03555014
         ST    R15,@E_ODTE_Ptr        Save it                           03556014
                                                                        03557014
         L     R2,@E_Record_Ptr       A(Start of record)                03558014
E0015    DS    0H                                                       03559014
                                                                        03560014
*-------------------------------------------------------------------*   03561014
*                                                                   *   03562014
*    Format the offset into the output record ...                   *   03563014
*                                                                   *   03564014
*-------------------------------------------------------------------*   03565014
                                                                        03566014
         LR    R15,R2                 Get current posn in input rec     03567014
         S     R15,@E_Record_Ptr      Convert to an offset              03568014
         STH   R15,@E_DBLWD           Save it                           03569014
         MVI   @E_DBLWD+2,X'FE'       MAKE IT "PACKED"                  03570014
         L     R15,@_SYSPRINT_Rec_Ptr Get A(Output                      03571014
         LA    R15,18(0,R15)            area)                           03572014
         UNPK  0(5,R15),@E_DBLWD(3)   UNPACK IT                         03573014
         TR    0(5,R15),E_Hexnum      TRANSLATE TO HEX                  03574014
                                                                        03575014
*-------------------------------------------------------------------*   03576014
*                                                                   *   03577014
*    ... and set up some of our addresses                           *   03578014
*                                                                   *   03579014
*-------------------------------------------------------------------*   03580014
                                                                        03581014
         L     R3,@_SYSPRINT_Rec_Ptr  A(Output                          03582014
         LA    R3,18(0,R3)              area)                           03583014
         L     R4,@E_Record_Ptr       Get length                        03584014
         AH    R4,SMF0LEN-SMFRCD0(R4)  of input record                  03585014
         SR    R4,R2                   remaining                        03586014
                                                                        03587014
*-------------------------------------------------------------------*   03588014
*                                                                   *   03589014
*    Check to see if we are printing EBCDIC                         *   03590014
*                                                                   *   03591014
*-------------------------------------------------------------------*   03592014
                                                                        03593014
         L     R15,@E_ODTE_Ptr        A(Print flag)                     03594014
         TM    ODTE_Print_EB-ODT_Entry(R15),L'ODTE_Print_EB  EBCDIC?    03595014
         BO    E0080                  Yes, go do it                     03596014
                                                                        03597014
*-------------------------------------------------------------------*   03598014
*                                                                   *   03599014
*    We are printing in Dump format (32 bytes per line) ...         *   03600014
*                                                                   *   03601014
*-------------------------------------------------------------------*   03602014
                                                                        03603014
         C     R4,=F'32'              MORE THAN OUR MAX?                03604014
         BNH   E0020                  NO, SKIP                          03605014
         L     R4,=F'32'              YES, ONLY PRINT 32                03606014
E0020    DS    0H                                                       03607014
                                                                        03608014
*-------------------------------------------------------------------*   03609014
*                                                                   *   03610014
*    ... with EBCDIC translation on the right-hand side             *   03611014
*                                                                   *   03612014
*-------------------------------------------------------------------*   03613014
                                                                        03614014
         MVI   80(R3),C'*'            OPENING ASTERISK                  03615014
         SH    R4,=H'1'               EXECUTE LENGTH OF DATA            03616014
         LA    R14,81(0,R3)           A(OUTPUT AREA)                    03617014
         EX    R4,E9996               MOVE TO EBCDIC AREA               03618014
         EX    R4,E9997               TRANSLATE UNPRNTBL TO .           03619014
         MVI   113(R3),C'*'           CLOSING ASTERISK                  03620014
         LA    R4,1(0,R4)             RESTORE LENGTH                    03621014
                                                                        03622014
*-------------------------------------------------------------------*   03623014
*                                                                   *   03624014
*    ... and hex data in groups of 4 bytes                          *   03625014
*                                                                   *   03626014
*-------------------------------------------------------------------*   03627014
                                                                        03628014
         LA    R3,6(0,R3)             A(START OF HEX DATA)              03629014
         LA    R5,4                   GROUPS OF 4 BYTES EACH            03630014
E0030    DS    0H                                                       03631014
         LA    R6,4                   DO 4 GROUPS                       03632014
E0040    DS    0H                                                       03633014
         CR    R4,R5                  CAN WE DO 4 BYTES?                03634014
         BH    E0050                  YES, SKIP                         03635014
         LR    R5,R4                  NO, DO AS MANY AS WE CAN          03636014
E0050    DS    0H                                                       03637014
         SH    R5,=H'1'               EXEC LEN OF DATA                  03638014
         LA    R14,@E_DBLWD           A(TARGET AREA)                    03639014
         EX    R5,E9996               MOVE TO DBLWD                     03640014
         LA    R15,@E_DBLWD(R5)       POINT TO LAST BYTE                03641014
         MVI   1(R15),X'FE'           PUT IN SIGN                       03642014
         LA    R5,1(0,R5)             RESET LEN OF DATA                 03643014
         LR    R0,R5                  CALCULATE UNPACKED LEN =          03644014
         SLL   R0,1                     = (R5 * 2)                      03645014
         LR    R15,R0                 PUT IN EXEC REGISTER              03646014
         SLL   R15,4                  MAKE ROOM FOR PACKED LEN          03647014
         OR    R15,R5                 GET PACKED LEN                    03648014
         EX    R15,E9999              UNPACK IT                         03649014
         LR    R14,R3                 A(OUTPUT AREA)                    03650014
         SRL   R15,4                  LENGTH OF OUTPUT ONLY             03651014
         EX    R15,E9998              AND TRANSLATE IT                  03652014
         AR    R3,R0                  A(NEXT                            03653014
         LA    R3,1(0,R3)                    OUTPUT BYTE)               03654014
         AR    R2,R5                  A(NEXT INPUT BYTE                 03655014
         SR    R4,R5                  NBR BYTES LEFT TO DO              03656014
         BNP   E0100                  None, were finished this line     03657014
         BCT   R6,E0040               DO NEXT GROUP OF 4                03658014
         LA    R3,1(0,R3)             EXTRA BLANK IN THE MIDDLE         03659014
         B     E0030                  AND DO ANOTHER GROUP OF 4         03660014
                                                                        03661014
E0080    DS    0H                                                       03662014
                                                                        03663014
*-------------------------------------------------------------------*   03664014
*                                                                   *   03665014
*    PRINT THE SELECTED RECORD IN EBCDIC ONLY.                      *   03666014
*                                                                   *   03667014
*-------------------------------------------------------------------*   03668014
                                                                        03669014
         LH    R15,@_SYSPRINT_Rec_Len GET MAX LRECL                     03670014
         SH    R15,=H'24'             LESS STUFF WE HAVE USED           03671014
         CR    R4,R15                 IS INPUT TOO LONG?                03672014
         BNH   E0090                  NO, OK                            03673014
         LR    R4,R15                 YES, PRINT AS MUCH AS WE CAN      03674014
E0090    DS    0H                                                       03675014
         LA    R14,6(0,R3)            A(OUTPUT AREA)                    03676014
         SH    R4,=H'1'               EXECUTE LENGTH OF DATA            03677014
         EX    R4,E9996               MOVE TO EBCDIC AREA               03678014
         EX    R4,E9997               TRANSLATE UNPRNTBL TO .           03679014
         LA    R4,1(0,R4)             Restore length printed            03680014
         AR    R2,R4                  Bump input pointer                03681014
E0100    DS    0H                                                       03682014
                                                                        03683014
*-------------------------------------------------------------------*   03684014
*                                                                   *   03685014
*    The print line has been formatted, so print it ...             *   03686014
*                                                                   *   03687014
*-------------------------------------------------------------------*   03688014
                                                                        03689014
         L     R15,=AL4(G0010)        A(Print routine)                  03690014
         BASR  R14,R15                Print our line                    03691014
                                                                        03692014
*-------------------------------------------------------------------*   03693014
*                                                                   *   03694014
*    ... and if there is more input record to format, do it.        *   03695014
*                                                                   *   03696014
*-------------------------------------------------------------------*   03697014
                                                                        03698014
         L     R14,@E_Record_Ptr      Get A(Input record)               03699014
         LR    R15,R2                 Calculate current                 03700014
         SR    R15,R14                 offset                           03701014
         CH    R15,SMF0LEN-SMFRCD0(R14)  Compare against record length  03702014
         BNL   E9000                  All done, exit                    03703014
         B     E0015                  Keep going                        03704014
E9000    DS    0H                                                       03705014
                                                                        03706014
*-------------------------------------------------------------------*   03707014
*                                                                   *   03708014
*    Free up our local storage ...                                  *   03709014
*                                                                   *   03710014
*-------------------------------------------------------------------*   03711014
                                                                        03712014
         LA    R1,@E_Dynam              A(Local storage)                03713014
         L     R13,4(0,R13)             Rescue A(HSA)                   03714014
                                                                        03715014
         STACK POP,                     Free the stack area            +03716014
               ADDR=(R1),                starting here                 +03717014
               STACK=@_31Bit_Stack_Ptr   on this stack                  03718014
                                                                        03719014
*-------------------------------------------------------------------*   03720014
*                                                                   *   03721014
*    ... and return to caller                                       *   03722014
*                                                                   *   03723014
*-------------------------------------------------------------------*   03724014
                                                                        03725014
         L     R14,12(0,R13)            Restore return address          03726014
         SLR   R15,R15                  Clear return code               03727014
         LM    R0,R12,20(R13)           Restore other registers         03728014
         BSM   0,R14                    and return                      03729014
                                                                        03730014
E9996    MVC   0(0,R14),0(R2)                                           03731014
E9997    TR    0(0,R14),E_PrtTbl                                        03732014
E9998    TR    0(0,R14),E_Hexnum                                        03733014
E9999    UNPK  0(0,R3),@E_DBLWD(0)                                      03734014
                                                                        03735014
E_Hexnum EQU   *-239                                                    03736014
         DC    C' 0123456789ABCDEF'                                     03737014
                                                                        03738014
E_PrtTbl DC    C'................'        00-0F                         03739014
         DC    C'................'        10-1F                         03740014
         DC    C'................'        20-2F                         03741014
         DC    C'................'        30-3F                         03742014
         DC    C' ...........<(+ã'        40-4F                         03743014
         DC    C'&&.........|$*);^'       50-5F                         03744014
         DC    C'-/.........,%_>?'        60-6F                         03745014
         DC    C'..........:#@''="'       70-7F                         03746014
         DC    C'.abcdefghi......'        80-8F                         03747014
         DC    C'.jklmnopqr......'        90-9F                         03748014
         DC    C'..stuvwxyz......'        A0-AF                         03749014
         DC    C'................'        B0-BF                         03750014
         DC    C'.ABCDEFGHI......'        C0-CF                         03751014
         DC    C'.JKLMNOPQR......'        DO-DF                         03752014
         DC    C'..STUVWXYZ......'        E0-EF                         03753014
         DC    C'0123456789......'        F0-FF                         03754014
                                                                        03755014
         LTORG                                                          03756014
                                                                        03757014
@E_Dynam        DSECT                 Dynamic area for E                03758014
@E_Save         DS    18F              O/S Style save area              03759014
@E_DBLWD        DS    D                Work area                        03760014
@E_Record_Ptr   DS    AL4              A(Input record)                  03761014
@E_ODTE_Ptr     DS    AL4              A(ODT Entry)                     03762014
                DS    0D               Alignment                        03763014
@E_DynLen       EQU   *-@E_Dynam      Length of storage required        03764014
                                                                        03765014
SMFSLCT  RSECT                                                          03766014
                                                                        03767014
         DROP  R11,R13                                                  03768014
         POP   USING                                                    03769014
         TITLE 'F0000 - Format Date/Times for output'                   03770014
         PUSH  USING                                                    03771014
                                                                        03772014
F0010    DS    0H                                                       03773014
                                                                        03774014
*---------------------------------------------------------------------* 03775014
*                                                                     * 03776014
*  Routine   : F0010                                                  * 03777014
*                                                                     * 03778014
*  Abstract  : Format Date/Time from DateConv area to printable       * 03779014
*                                                                     * 03780014
*  Inputs    : R1 ----> A(DateConv_Area)                              * 03781014
*                       A(Start of output area)                       * 03782014
*                       A(Word in which to return the address of      * 03783014
*                         the last byte of formatted data)            * 03784014
*  Outputs   : R15: 0 - OK                                            * 03785014
*                   4 - Error in conversion routine                   * 03786014
*                                                                     * 03787014
*  Notes     :                                                        * 03788014
*                                                                     * 03789014
*  History:                                                           * 03790014
*              1999/04/24 SDDA030 V1.1                                * 03791014
*                                 Used LIFO Stack for our dynamic     * 03792014
*                                 areas.                              * 03793014
*              ____/__/__ _______                                     * 03794014
*                                                                     * 03795014
*---------------------------------------------------------------------* 03796014
                                                                        03797014
         USING *,R15                                                    03798014
         SAVE  (14,12),T                                                03799014
         LR    R11,R15                  Load permanent base reg         03800014
         DROP  R15                      Free up temp base               03801014
         USING F0010,R11                Assign permanent base           03802014
                                                                        03803014
*-------------------------------------------------------------------*   03804014
*                                                                   *   03805014
*    Get an area on the stack for our local storage                 *   03806014
*                                                                   *   03807014
*-------------------------------------------------------------------*   03808014
                                                                        03809014
         STACK PUSH,                    Get Stack area                 +03810014
               LEN=@F_DynLen,            this long                     +03811014
               STACK=@_31Bit_Stack_Ptr   using this stack               03812014
                                                                        03813014
         LR    R2,R1                    Save its address                03814014
         LR    R0,R1                    A(Our storage)                  03815014
         LA    R1,@F_DynLen              Its length                     03816014
         SLR   R14,R14                  Clear source address            03817014
         SLR   R15,R15                   and length                     03818014
         MVCL  R0,R14                   Clear our storage               03819014
         LR    R1,R2                    Restore A(Our storage)          03820014
                                                                        03821014
*-------------------------------------------------------------------*   03822014
*                                                                   *   03823014
*    Chain our save areas, and restore the important registers      *   03824014
*   that we have destroyed                                          *   03825014
*                                                                   *   03826014
*-------------------------------------------------------------------*   03827014
                                                                        03828014
         ST    R13,4(0,R1)              Chain                           03829014
         ST    R1,8(0,R13)               saveareas                      03830014
         LR    R13,R1                   Load dynam base                 03831014
         USING @F_Dynam,R13             Assign a base                   03832014
         L     R15,@F_Dynam+4           Get A(HSA)                      03833014
         LM    R0,R3,20(R15)            Restore callers registers       03834014
                                                                        03835014
         L     R9,0(0,R1)               A(Date conv area)               03836014
         L     R8,4(0,R1)               A(Output area)                  03837014
         L     R15,8(0,R1)              Get A(return area)              03838014
         ST    R15,@F_Return_Ptr        Save it                         03839014
                                                                        03840014
         LR    R1,R9                    Point to DateConv area          03841014
         L     R15,=A(K0010)            A(Date conversion routine)      03842014
         BASR  R14,R15                  Go do it                        03843014
         LTR   R15,R15                  Did it work?                    03844014
         BNZ   F8010                    No, exit doing nothing          03845014
                                                                        03846014
         MVI   0(R8),C' '               OK, blank out                   03847014
         MVC   1(29,R8),0(R8)             our receiving area            03848014
         USING DateConv_Area,R9         and assign a DateConv base      03849014
                                                                        03850014
         MVC   0(L'DConv_Date_Day,R8),DConv_Date_Day                    03851014
         LA    R8,L'DConv_Date_Day-1(0,R8) A(Last char)                 03852014
F0020    DS    0H                                                       03853014
         CLI   0(R8),C' '            End of Day name?                   03854014
         BNE   F0030                 Yes, skip                          03855014
         BCT   R8,F0020              No, try again                      03856014
F0030    DS    0H                                                       03857014
         MVI   1(R8),C','            Insert separator                   03858014
         MVC   3(L'DConv_Date_Month,R8),DConv_Date_Month  Month name    03859014
         LA    R8,3+L'DConv_Date_Month-1(0,R8) A(Last char)             03860014
F0040    DS    0H                                                       03861014
         CLI   0(R8),C' '            End of Month name?                 03862014
         BNE   F0050                 Yes, skip                          03863014
         BCT   R8,F0040              No, try again                      03864014
F0050    DS    0H                                                       03865014
         SLR   R0,R0                 Clear work reg                     03866014
         IC    R0,DConv_Date_DD      Get Day of month                   03867014
         CVD   R0,@F_DBLWD           Pack it                            03868014
         UNPK  1(3,R8),@F_DBLWD+6(2)  unpack it                         03869014
         MVI   1(R8),C' '            Clear spurious digit               03870014
         LA    R8,2(0,R8)            point to 1st char                  03871014
         CLI   0(R8),C'0'            Leading 0?                         03872014
         BNE   F0060                 No, skip                           03873014
         MVC   0(1,R8),1(R8)         Yes, left align day                03874014
         SH    R8,=H'1'              and adjust pointer                 03875014
F0060    DS    0H                                                       03876014
         OI    1(R8),X'F0'           Make it readable                   03877014
         MVI   2(R8),C','            Insert separator                   03878014
         LH    R0,DConv_Date_YYYY    Get year                           03879014
         CVD   R0,@F_DBLWD           Pack it                            03880014
         UNPK  3(5,R8),@F_DBLWD+5(3) Unpack it                          03881014
         MVI   3(R8),C' '            Clear spurious character           03882014
         OI    7(R8),X'F0'           Make it readable                   03883014
         MVI   9(R8),C'('            Move separator                     03884014
         MVC   10(4,R8),4(R8)        Move YYYY                          03885014
         MVI   14(R8),C'.'           Move separator                     03886014
         LH    R0,DConv_Date_DDD     Get day of year                    03887014
         CVD   R0,@F_DBLWD           Pack it                            03888014
         UNPK  15(3,R8),@F_DBLWD+6(2) Unpack it                         03889014
         OI    17(R8),X'F0'          Make it readable                   03890014
         MVI   18(R8),C')'           Move close paren                   03891014
         SLR   R0,R0                 Clear work register                03892014
         IC    R0,DConv_Time_hh      Get hours                          03893014
         CVD   R0,@F_DBLWD           Pack it                            03894014
         UNPK  19(3,R8),@F_DBLWD+6(2) Unpack it                         03895014
         MVI   19(R8),C' '           Clear spurious char                03896014
         OI    21(R8),X'F0'          Make it readable                   03897014
         SLR   R0,R0                 Clear work register                03898014
         IC    R0,DConv_Time_mm      Get minutes                        03899014
         CVD   R0,@F_DBLWD           Pack it                            03900014
         UNPK  22(3,R8),@F_DBLWD+6(2) Unpack it                         03901014
         MVI   22(R8),C':'           Clear spurious char                03902014
         OI    24(R8),X'F0'          Make it readable                   03903014
         SLR   R0,R0                 Clear work register                03904014
         IC    R0,DConv_Time_ss      Get seconds                        03905014
         CVD   R0,@F_DBLWD           Pack it                            03906014
         UNPK  25(3,R8),@F_DBLWD+6(2) Unpack it                         03907014
         MVI   25(R8),C':'           Clear spurious char                03908014
         OI    27(R8),X'F0'          Make it readable                   03909014
         SLR   R0,R0                 Clear work register                03910014
         IC    R0,DConv_Time_tt      Get hundredths of seconds          03911014
         CVD   R0,@F_DBLWD           Pack it                            03912014
         UNPK  28(3,R8),@F_DBLWD+6(2) Unpack it                         03913014
         MVI   28(R8),C'.'           Clear spurious char                03914014
         OI    30(R8),X'F0'          Make it readable                   03915014
                                                                        03916014
         LA    R8,30(0,R8)           A(Last byte of data)               03917014
         L     R15,@F_Return_Ptr     A(Caller's area)                   03918014
         ST    R8,0(0,R15)           Save it for the caller             03919014
                                                                        03920014
         SLR   R15,R15               Clear return code                  03921014
         B     F9010                  and exit                          03922014
F8010    DS    0H                                                       03923014
                                                                        03924014
*---------------------------------------------------------------------* 03925014
*                                                                     * 03926014
*   Error from Date conversion routine                                * 03927014
*                                                                     * 03928014
*---------------------------------------------------------------------* 03929014
                                                                        03930014
         LA    R15,4           Set return code                          03931014
         B     F9010           and exit                                 03932014
                                                                        03933014
F9010    DS    0H                                                       03934014
                                                                        03935014
*-------------------------------------------------------------------*   03936014
*                                                                   *   03937014
*    Free up our local storage ...                                  *   03938014
*                                                                   *   03939014
*-------------------------------------------------------------------*   03940014
                                                                        03941014
         LR    R2,R15                   Save return code                03942014
         LA    R1,@F_Dynam              A(Local storage)                03943014
         L     R13,4(0,R13)             Rescue A(HSA)                   03944014
                                                                        03945014
         STACK POP,                     Free the stack area            +03946014
               ADDR=(R1),                starting here                 +03947014
               STACK=@_31Bit_Stack_Ptr   on this stack                  03948014
                                                                        03949014
         LR    R15,R2                   Restore return code             03950014
                                                                        03951014
*-------------------------------------------------------------------*   03952014
*                                                                   *   03953014
*    ... and return to caller                                       *   03954014
*                                                                   *   03955014
*-------------------------------------------------------------------*   03956014
                                                                        03957014
         L     R14,12(0,R13)            Restore return address          03958014
         LM    R0,R12,20(R13)           Restore other registers         03959014
         BSM   0,R14                    and return                      03960014
                                                                        03961014
                                                                        03962014
                                                                        03963014
         LTORG                                                          03964014
                                                                        03965014
@F_Dynam        DSECT                 Dynamic area for F                03966014
@F_Save         DS    18F              O/S Style save area              03967014
@F_DBLWD        DS    D                Work area                        03968014
@F_Return_Ptr   DS    AL4              A(Return area)                   03969014
                DS    0D               Alignment                        03970014
@F_DynLen       EQU   *-@F_Dynam      Length of storage required        03971014
                                                                        03972014
SMFSLCT  RSECT                                                          03973014
                                                                        03974014
         DROP  R9                     Free DateConv_Area base           03975014
         DROP  R11,R13                                                  03976014
         POP   USING                                                    03977014
         TITLE 'G0000: WRITE SYSPRINT'                                  03978014
                                                                        03979014
         PUSH  USING                                                    03980014
                                                                        03981014
G0010    DS    0H                                                       03982014
                                                                        03983014
*---------------------------------------------------------------------* 03984014
*                                                                     * 03985014
*  Routine   : G0010                                                  * 03986014
*                                                                     * 03987014
*  Abstract  : Write SYSPRINT record, producing page titles if        * 03988014
*              required                                               * 03989014
*                                                                     * 03990014
*  Inputs    : N/A                                                    * 03991014
*  Outputs   : R15: 0 - OK                                            * 03992014
*                   4 - SYSPRINT not usable                           * 03993014
*                                                                     * 03994014
*  Notes     : If SYSPRINT is not OPEN, an attempt is made to OPEN it.* 03995014
*              On exit, @SYSPRINT_Rec_Ptr is updated with the         * 03996014
*                       address of the next SYSPRINT record area.     * 03997014
*                                                                     * 03998014
*  History:                                                           * 03999014
*              1999/04/24 SDDA030 V1.1                                * 04000014
*                                 Used LIFO Stack for our dynamic     * 04001014
*                                 areas.                              * 04002014
*              ____/__/__ _______                                     * 04003014
*                                                                     * 04004014
*---------------------------------------------------------------------* 04005014
                                                                        04006014
         USING *,R15                                                    04007014
         SAVE  (14,12),T                                                04008014
         LR    R11,R15                  Load permanent base reg         04009014
         DROP  R15                      Free up temp base               04010014
         USING G0010,R11                Assign permanent base           04011014
                                                                        04012014
*-------------------------------------------------------------------*   04013014
*                                                                   *   04014014
*    Get an area on the stack for our local storage                 *   04015014
*                                                                   *   04016014
*-------------------------------------------------------------------*   04017014
                                                                        04018014
         STACK PUSH,                    Get Stack area                 +04019014
               LEN=@G_DynLen,            this long                     +04020014
               STACK=@_31Bit_Stack_Ptr   using this stack               04021014
                                                                        04022014
         LR    R2,R1                    Save its address                04023014
         LR    R0,R1                    A(Our storage)                  04024014
         LA    R1,@G_DynLen              Its length                     04025014
         SLR   R14,R14                  Clear source address            04026014
         SLR   R15,R15                   and length                     04027014
         MVCL  R0,R14                   Clear our storage               04028014
         LR    R1,R2                    Restore A(Our storage)          04029014
                                                                        04030014
*-------------------------------------------------------------------*   04031014
*                                                                   *   04032014
*    Chain our save areas, and restore the important registers      *   04033014
*   that we have destroyed                                          *   04034014
*                                                                   *   04035014
*-------------------------------------------------------------------*   04036014
                                                                        04037014
         ST    R13,4(0,R1)              Chain                           04038014
         ST    R1,8(0,R13)               saveareas                      04039014
         LR    R13,R1                   Load dynam base                 04040014
         USING @G_Dynam,R13             Assign a base                   04041014
         L     R15,@G_Dynam+4           Get A(HSA)                      04042014
         LM    R0,R3,20(R15)            Restore callers registers       04043014
                                                                        04044014
*-------------------------------------------------------------------*   04045014
*                                                                   *   04046014
*    If we havent opened our DCB yet, do so                         *   04047014
*                                                                   *   04048014
*-------------------------------------------------------------------*   04049014
                                                                        04050014
         ICM   R1,B'1111',@_SYSPRINT_DCB_Ptr A(SYSPRINT DCB)            04051014
         BNZ   G0030                    OK, skip                        04052014
                                                                        04053014
         STORAGE OBTAIN,                Go get our storage             +04054014
               LENGTH=G_SYSPRINT_DCB_Length, this long                 +04055014
               LOC=BELOW                 below-the-line                 04056014
                                                                        04057014
         ST    R1,@_SYSPRINT_DCB_Ptr    Save it                         04058014
         MVC   0(G_SYSPRINT_DCB_Length,R1),G_SYSPRINT_DCB               04059014
                                                                        04060014
         MVC   @G_Open,G_Open         Move OPEN parms                   04061014
         L     R2,@_SYSPRINT_DCB_Ptr  Point to the DCB                  04062014
         MVC   0(G_SYSPRINT_DCB_Length,R2),G_SYSPRINT_DCB               04063014
         OPEN  ((2),OUTPUT),MODE=31,MF=(E,@G_Open) and open it          04064014
         TM    DCBOFLGS-IHADCB(R2),DCBOFOPN Is it open?                 04065014
         BZ    G8000                  No, exit quick smart              04066014
         LH    R15,DCBLRECL-IHADCB(R2) Yes, get record length           04067014
         TM    DCBRECFM-IHADCB(R2),DCBRECCC CC present?                 04068014
         BZ    G0020                  No, skip                          04069014
         SH    R15,=H'1'              Yes, decrement length             04070014
G0020    DS    0H                                                       04071014
         STH   R15,@_SYSPRINT_Rec_Len Save data length of SYSPRINT      04072014
         L     R1,@_SYSPRINT_DCB_Ptr  A(SYSPRINT DCB)                   04073014
                                                                        04074014
G0030    DS    0H                                                       04075014
                                                                        04076014
*-------------------------------------------------------------------*   04077014
*                                                                   *   04078014
*    PRINT THE CURRENT SYSPRINT RECORD, CLEAR THE NEW RECORD AREA,  *   04079014
*   AND BUMP THE LINE COUNT.                                        *   04080014
*                                                                   *   04081014
*-------------------------------------------------------------------*   04082014
                                                                        04083014
         TM    DCBOFLGS-IHADCB(R1),DCBOFOPN  IS SYSPRINT OPEN?          04084014
         BNO   G8000                  NO, EXIT                          04085014
         PUT   (1)                    WRITE IT                          04086014
         ST    R1,@_SYSPRINT_Rec_Ptr  Save A(SYSPRINT Record)           04087014
         LH    R15,@_Line_Count       Get current Line Count            04088014
         LA    R15,1(0,R15)           Bump it                           04089014
         STH   R15,@_Line_Count       Save it                           04090014
         CH    R15,=H'60'             End of a page?                    04091014
         BL    G0050                  Not yet, exit                     04092014
                                                                        04093014
*-------------------------------------------------------------------*   04094014
*                                                                   *   04095014
*    SKIP TO A NEW PAGE, AND PRINT THE HEADING.                     *   04096014
*                                                                   *   04097014
*-------------------------------------------------------------------*   04098014
                                                                        04099014
         LR    R0,R1                  Clear                             04100014
         L     R1,@_SYSPRINT_DCB_Ptr  A(SYSPRINT)                       04101014
         LH    R1,DCBLRECL-IHADCB(R1)  Get output length                04102014
         SLR   R14,R14                 output                           04103014
         LA    R15,X'40'               record                           04104014
         SLL   R15,24                  area to                          04105014
         MVCL  R0,R14                  blanks                           04106014
         L     R1,@_SYSPRINT_Rec_Ptr  Restore record pointer            04107014
         MVI   0(R1),C'1'             START A NEW PAGE                  04108014
         LA    R15,@_DateConv_Area    A(Info about today)               04109014
         ST    R15,@G_Parms           Save it                           04110014
         LA    R15,1(0,R1)            A(Output area)                    04111014
         ST    R15,@G_Parms+4         Save it                           04112014
         LA    R15,@G_LastChar        A(Return area)                    04113014
         ST    R15,@G_Parms+8         Save it                           04114014
         LA    R1,@G_Parms            Point to parms                    04115014
         L     R15,=A(F0010)          A(Format routine)                 04116014
         BASR  R14,R15                Go do it                          04117014
         LH    R15,@_SYSPRINT_Rec_Len GET SYSPRINT RECORD LENGTH        04118014
         A     R15,@_SYSPRINT_Rec_Ptr POINT PAST END OF RECORD          04119014
         LA    R14,L'G_Hdg1PP+L'G_Hdg1P+1 LEN OF PAGE NUMBER STUFF      04120014
         SR    R15,R14                OFFSET OF PAGE NBR STUFF          04121014
         MVC   0(L'G_Hdg1PP+L'G_Hdg1P,R15),G_Hdg1PP MOVE IT             04122014
         AP    @_Page_Count,=P'1'     BUMP PAGE COUNT                   04123014
         ED    G_Hdg1P-G_Hdg1PP(L'G_Hdg1P,R15),@_Page_Count EDIT IT IN  04124014
         L     R14,@G_LastChar        A(Last char of Date/Time)         04125014
         SR    R15,R14                Len of white space                04126014
         LA    R14,L'G_Hdg1TITL       LEN OF TITLE                      04127014
         SR    R15,R14                GET DIFFERENCE                    04128014
         SRL   R15,1                  HALVE IT (IE, CENTRE TITLE)       04129014
         A     R15,@G_LastChar        A(output area)                    04130014
         MVC   0(L'G_Hdg1TITL,R15),G_Hdg1TITL MOVE TITLE                04131014
         L     R1,@_SYSPRINT_DCB_Ptr  A(SYSPRINT)                       04132014
         PUT   (1)                    WRITE IT                          04133014
         ST    R1,@_SYSPRINT_Rec_Ptr  Save A(SYSPRINT Record)           04134014
         LA    R15,1                  SET RECORD COUNT                  04135014
         STH   R15,@_Line_Count       AND SAVE IT                       04136014
         TM    ODTE_Print_NO-ODT_Entry(R10),L'ODTE_Print_NO             04137014
         BO    G0040                  NO, SKIP THESE HEADINGS           04138014
         LR    R0,R1                                                    04139014
         L     R1,@_SYSPRINT_DCB_Ptr  A(SYSPRINT)                       04140014
         LH    R1,DCBLRECL-IHADCB(R1)  Get output length                04141014
         LA    R15,X'40'                                                04142014
         SLL   R15,24                                                   04143014
         LA    R14,L'G_Hdg2A                                            04144014
         OR    R15,R14                                                  04145014
         LA    R14,G_Hdg2A                                              04146014
         MVCL  R0,R14                                                   04147014
         L     R1,@_SYSPRINT_DCB_Ptr  A(SYSPRINT DCB)                   04148014
         PUT   (1)                    WRITE IT                          04149014
         LR    R0,R1                                                    04150014
         L     R1,@_SYSPRINT_DCB_Ptr  A(SYSPRINT)                       04151014
         LH    R1,DCBLRECL-IHADCB(R1)  Get output length                04152014
         LA    R15,X'40'                                                04153014
         SLL   R15,24                                                   04154014
         LA    R14,L'G_Hdg2B                                            04155014
         OR    R15,R14                                                  04156014
         LA    R14,G_Hdg2B                                              04157014
         MVCL  R0,R14                                                   04158014
         L     R1,@_SYSPRINT_DCB_Ptr  A(SYSPRINT DCB)                   04159014
         PUT   (1)                    WRITE IT                          04160014
         ST    R1,@_SYSPRINT_Rec_Ptr  Save record pointer               04161014
         LR    R0,R1                  Clear                             04162014
         L     R1,@_SYSPRINT_DCB_Ptr  A(SYSPRINT)                       04163014
         LH    R1,DCBLRECL-IHADCB(R1)  Get output length                04164014
         SLR   R14,R14                 output                           04165014
         LA    R15,X'40'               record                           04166014
         SLL   R15,24                  area to                          04167014
         MVCL  R0,R14                  blanks                           04168014
         L     R1,@_SYSPRINT_Rec_Ptr  Restore record pointer            04169014
         L     R1,@_SYSPRINT_DCB_Ptr  A(SYSPRINT DCB)                   04170014
         PUT   (1)                    WRITE IT                          04171014
         ST    R1,@_SYSPRINT_Rec_Ptr  Save record pointer               04172014
         LA    R15,3                  SET RECORD COUNT                  04173014
         AH    R15,@_Line_Count       INCREMENT LINT COUNT              04174014
         STH   R15,@_Line_Count       AND SAVE IT                       04175014
G0040    DS    0H                                                       04176014
G0050    DS    0H                                                       04177014
         L     R0,@_SYSPRINT_Rec_Ptr  Clear                             04178014
         L     R1,@_SYSPRINT_DCB_Ptr  A(SYSPRINT)                       04179014
         LH    R1,DCBLRECL-IHADCB(R1)  Get output length                04180014
         SLR   R14,R14                 output                           04181014
         LA    R15,X'40'               record                           04182014
         SLL   R15,24                  area to                          04183014
         MVCL  R0,R14                  blanks                           04184014
         L     R1,@_SYSPRINT_Rec_Ptr  Restore record pointer            04185014
         L     R15,@_SYSPRINT_DCB_Ptr A(SYSPRINT)                       04186014
         TM    DCBRECFM-IHADCB(R15),DCBRECCC  ANY CONTROL CHAR?         04187014
         BZ    G0060                  NO, SKIP                          04188014
         LA    R1,1(0,R1)             YES, POINT PAST IT                04189014
G0060    DS    0H                                                       04190014
         ST    R1,@_SYSPRINT_Rec_Ptr  SAVE A(OUTPUT AREA)               04191014
         SLR   R15,R15                Clear return code                 04192014
         B     G9000                  AND EXIT                          04193014
G8000    DS    0H                                                       04194014
         LA    R15,4                  Error opening SYSPRINT            04195014
         B     G9000                  AND EXIT                          04196014
G9000    DS    0H                                                       04197014
                                                                        04198014
*-------------------------------------------------------------------*   04199014
*                                                                   *   04200014
*    Free up our local storage ...                                  *   04201014
*                                                                   *   04202014
*-------------------------------------------------------------------*   04203014
                                                                        04204014
         LR    R3,R15                   Save the return code            04205014
                                                                        04206014
         LA    R1,@G_Dynam              A(Local storage)                04207014
         L     R13,4(0,R13)             Rescue A(HSA)                   04208014
                                                                        04209014
         STACK POP,                     Free the stack area            +04210014
               ADDR=(R1),                starting here                 +04211014
               STACK=@_31Bit_Stack_Ptr   on this stack                  04212014
                                                                        04213014
*-------------------------------------------------------------------*   04214014
*                                                                   *   04215014
*    ... and return to caller                                       *   04216014
*                                                                   *   04217014
*-------------------------------------------------------------------*   04218014
                                                                        04219014
         LR    R15,R3                   Restore return code             04220014
         L     R14,12(0,R13)            Restore return address          04221014
         LM    R0,R12,20(R13)           Restore other registers         04222014
         BSM   0,R14                    and return                      04223014
                                                                        04224014
G_SYSPRINT_DCB DCB DDNAME=SYSPRINT,DSORG=PS,MACRF=PL,LRECL=133,        +04225014
               RECFM=FBA                                                04226014
G_SYSPRINT_DCB_Length EQU *-G_SYSPRINT_DCB                              04227014
                                                                        04228014
G_Open   OPEN  (0,INPUT),MODE=31,MF=L                                   04229014
G_Open_Length EQU *-G_Open                                              04230014
                                                                        04231014
G_HDG1TITL DC  C'SMFSLCT &VERSION &ASMDT &SYSTIME - &CPERSON'           04232031
*                                                       @RWS 15-11-13   04233032
G_Hdg1PP DC    C'PAGE'                                                  04234014
G_Hdg1P  DC    X'40202120'            PAGE NUMBER MASK                  04235014
                                                                        04236014
G_Hdg2A  DC    C'0  Input    Sel    Off-'                               04237014
G_Hdg2B  DC    C'   Count  Count     set  Data'                         04238014
                                                                        04239014
         LTORG                                                          04240014
                                                                        04241014
@G_Dynam        DSECT                 Dynamic area for G                04242014
@G_Save         DS    18F              O/S Style save area              04243014
                DS    0F               Alignment                        04244014
@G_Open         DS    CL(G_Open_Length)  Open parms                     04245014
@G_Parms        DS    3AL4             Parm area for F0010              04246014
@G_LastChar     DS    AL4              Return area from F0010           04247014
                DS    0D               Alignment                        04248014
@G_DynLen       EQU   *-@G_Dynam      Length of storage required        04249014
                                                                        04250014
SMFSLCT  RSECT                                                          04251014
                                                                        04252014
         DROP  R11,R13                                                  04253014
         POP   USING                                                    04254014
         TITLE 'H0000: INTERPRET CONTROL CARDS'                         04255014
                                                                        04256014
         PUSH  USING                                                    04257014
                                                                        04258014
*---------------------------------------------------------------------* 04259014
*                                                                     * 04260014
*  Routine   : H0010                                                  * 04261014
*                                                                     * 04262014
*  Abstract  : READ SYSIN control cards, and branch to interpretation * 04263014
*              routines (Hx0010)                                      * 04264014
*                                                                     * 04265014
*  Inputs    : N/A                                                    * 04266014
*  Outputs   : R15: 0 - OK                                            * 04267014
*                   4 - Error in SYSIN Control Cards                  * 04268014
*                                                                     * 04269014
*  Notes     :                                                        * 04270014
*                                                                     * 04271014
*  History:                                                           * 04272014
*              1999/04/24 SDDA030 V1.1                                * 04273014
*                                 Used LIFO Stack for our dynamic     * 04274014
*                                 areas.                              * 04275014
*              ____/__/__ _______                                     * 04276014
*                                                                     * 04277014
*---------------------------------------------------------------------* 04278014
                                                                        04279014
H0010    DS    0H                                                       04280014
                                                                        04281014
         USING *,R15                                                    04282014
         SAVE  (14,12),T                                                04283014
         LR    R11,R15                  Load permanent base reg         04284014
         DROP  R15                      Free up temp base               04285014
         USING H0010,R11                Assign permanent base           04286014
                                                                        04287014
*-------------------------------------------------------------------*   04288014
*                                                                   *   04289014
*    Get an area on the stack for our local storage                 *   04290014
*                                                                   *   04291014
*-------------------------------------------------------------------*   04292014
                                                                        04293014
         STACK PUSH,                    Get Stack area                 +04294014
               LEN=@H_DynLen,            this long                     +04295014
               STACK=@_31Bit_Stack_Ptr   using this stack               04296014
                                                                        04297014
         LR    R2,R1                    Save its address                04298014
         LR    R0,R1                    A(Our storage)                  04299014
         LA    R1,@H_DynLen              Its length                     04300014
         SLR   R14,R14                  Clear source address            04301014
         SLR   R15,R15                   and length                     04302014
         MVCL  R0,R14                   Clear our storage               04303014
         LR    R1,R2                    Restore A(Our storage)          04304014
                                                                        04305014
*-------------------------------------------------------------------*   04306014
*                                                                   *   04307014
*    Chain our save areas, and restore the important registers      *   04308014
*   that we have destroyed                                          *   04309014
*                                                                   *   04310014
*-------------------------------------------------------------------*   04311014
                                                                        04312014
         ST    R13,4(0,R1)              Chain                           04313014
         ST    R1,8(0,R13)               saveareas                      04314014
         LR    R13,R1                   Load dynam base                 04315014
         USING @H_Dynam,R13             Assign a base                   04316014
         L     R15,@H_Dynam+4           Get A(HSA)                      04317014
         LM    R0,R3,20(R15)            Restore callers registers       04318014
                                                                        04319014
         SLR   R15,R15                  Clear work register             04320014
         ST    R15,@H_rc                and clear return code           04321014
                                                                        04322014
*-------------------------------------------------------------------*   04323014
*                                                                   *   04324014
*    OPEN SYSIN.                                                    *   04325014
*                                                                   *   04326014
*-------------------------------------------------------------------*   04327014
                                                                        04328014
         L     R1,@_SYSPRINT_Rec_Ptr  A(SYSPRINT RECORD)                04329014
         MVC   5(20,R1),=C'Input Control Cards:' MOVE HEADING           04330014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               04331014
         BASR  R14,R15                GO PRINT IT                       04332014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               04333014
         BASR  R14,R15                GO PRINT blank line               04334014
         L     R1,@_SYSPRINT_Rec_Ptr  A(NEW SYSPRINT RECORD)            04335014
         MVC   1(30,R1),=C'--- No Control Cards found ---'              04336014
                                                                        04337014
         STACK PUSH,                    Get Stack area                 +04338014
               LEN=H_SYSIN_DCB_Length,   this long                     +04339014
               STACK=@_24Bit_Stack_Ptr   using this stack               04340014
                                                                        04341014
         ST    R1,@H_SYSIN_DCB_Ptr    Save A(SYSIN DCB area)            04342014
         MVC   @H_OPEN,H_OPEN         Move OPEN parms                   04343014
         L     R2,@H_SYSIN_DCB_Ptr      POINT TO THE DCB                04344014
         MVC   0(H_SYSIN_DCB_Length,R2),H_SYSIN_DCB Move DCB            04345014
         MVC   @H_SYSIN_DCBE,H_SYSIN_DCBE Move SYSIN DCBE               04346014
         LA    R15,@H_SYSIN_DCBE      Point to it                       04347014
         ST    R15,DCBDCBE-IHADCB(R2) Update address in DCB             04348014
         OPEN  ((2),INPUT),MODE=31,MF=(E,@H_OPEN) AND OPEN IT           04349014
         TM    DCBOFLGS-IHADCB(R2),DCBOFOPN  IS IT OPEN?                04350014
         BZ    H9000                  NO, SKIP                          04351014
         TM    @H_SYSIN_DCBE+DCBEFLG1-DCBE,DCBEMD31 31-bit SAM OK?      04352014
         BO    H0020                  Yes, skip                         04353014
                                                                        04354014
*---------------------------------------------------------------------* 04355014
*                                                                     * 04356014
*   31-bit SAM is not supported for SYSIN. This typically happens     * 04357014
*  when it is allocated to the terminal in a TSO session (the I/O     * 04358014
*  seems to be OK, but the EOD handling gets in trouble). So ...      * 04359014
*                                                                     * 04360014
*---------------------------------------------------------------------* 04361014
                                                                        04362014
*---------------------------------------------------------------------* 04363014
*                                                                     * 04364014
*  ... close the current DCB ...                                      * 04365014
*                                                                     * 04366014
*---------------------------------------------------------------------* 04367014
                                                                        04368014
         MVC   @H_CLOSE,H_CLOSE       Move close parms                  04369014
         L     R2,@H_SYSIN_DCB_Ptr    Point to the DCB                  04370014
         CLOSE ((2)),MODE=31,MF=(E,@H_CLOSE)  and close it              04371014
                                                                        04372014
*---------------------------------------------------------------------* 04373014
*                                                                     * 04374014
*  ... move a normal DCB (without the DCBE) to the 24-bit area ...    * 04375014
*                                                                     * 04376014
*---------------------------------------------------------------------* 04377014
                                                                        04378014
         MVC   0(H_SYSIN24_DCB_Length,R2),H_SYSIN24_DCB Move DCB        04379014
                                                                        04380014
*---------------------------------------------------------------------* 04381014
*                                                                     * 04382014
*  ... and move the 24-bit EOD to below-the-line storage              * 04383014
*                                                                     * 04384014
*---------------------------------------------------------------------* 04385014
                                                                        04386014
         STACK PUSH,                    Get Stack area                 +04387014
               LEN=H800024_Length,       this long                     +04388014
               STACK=@_24Bit_Stack_Ptr   using this stack               04389014
                                                                        04390014
         MVC   0(H800024_Length,R1),H800024_Start Move 24-bit EOD       04391014
         O     R1,DCBEODAD-IHADCB(R2)     Set up                        04392014
         ST    R1,DCBEODAD-IHADCB(R2)      EODAD                        04393014
                                                                        04394014
*---------------------------------------------------------------------* 04395014
*                                                                     * 04396014
*  Open SYSIN                                                         * 04397014
*                                                                     * 04398014
*---------------------------------------------------------------------* 04399014
                                                                        04400014
         MVC   @H_OPEN,H_OPEN           Move OPEN parms                 04401014
         L     R2,@H_SYSIN_DCB_Ptr      POINT TO THE DCB                04402014
         OPEN  ((2),INPUT),MODE=31,MF=(E,@H_OPEN) AND OPEN IT           04403014
         TM    DCBOFLGS-IHADCB(R2),DCBOFOPN  IS IT OPEN?                04404014
         BZ    H9000                  NO, SKIP                          04405014
                                                                        04406014
H0020    DS    0H                                                       04407014
         L     R10,@_ODT_Ptr          A(ODT Table)                      04408014
         LA    R10,ODT_Entry-OutDesc_Tbl(R10) AND POINT TO FIRST        04409014
                                                                        04410014
H0030    DS    0H                                                       04411014
                                                                        04412014
*------------------------------------------------------------------*    04413014
*                                                                  *    04414014
*    Read a control card from SYSIN, and print it.                 *    04415014
*                                                                  *    04416014
*------------------------------------------------------------------*    04417014
                                                                        04418014
         L     R1,@H_SYSIN_DCB_Ptr    A(SYSIN DCB)                      04419014
         GET   (1)                    READ A SYSIN RECORD               04420014
         LR    R2,R1                  SAVE A(INPUT RECORD)              04421014
         L     R15,@H_SYSIN_DCB_Ptr   A(SYSIN DCB)                      04422014
         LH    R3,DCBLRECL-IHADCB(R15) GET SYSIN RECORD LENGTH          04423014
         L     R1,@_SYSPRINT_Rec_Ptr  GET A(SYSPRINT RECORD)            04424014
         CH    R3,@_SYSPRINT_Rec_Len  CAN WE FIT SYSIN ON SYSPRINT?     04425014
         BNH   H0040                  YES, SKIP                         04426014
         LH    R3,@_SYSPRINT_Rec_Len  NO, PUT AS MUCH AS WE CAN         04427014
H0040    DS    0H                                                       04428014
         SH    R3,=H'1'               GET EXECUTE LENGTH                04429014
         EX    R3,H9998               MOVE TO SYSPRINT RECORD           04430014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               04431014
         BASR  R14,R15                AND WRITE IT                      04432014
         L     R15,@H_SYSIN_DCB_Ptr   A(SYSIN DCB)                      04433014
         LH    R3,DCBLRECL-IHADCB(R15) INPUT RECORD LENGTH              04434014
                                                                        04435014
*------------------------------------------------------------------*    04436014
*                                                                  *    04437014
*    If it is a comment ('*' in Col 1), ignore it.                 *    04438014
*                                                                  *    04439014
*------------------------------------------------------------------*    04440014
                                                                        04441014
         CLI   0(R2),C'*'             Is this a comment?                04442014
         BE    H0030                  Yes, get another record           04443014
                                                                        04444014
*------------------------------------------------------------------*    04445014
*                                                                  *    04446014
*    Otherwise, skip leading blanks.                               *    04447014
*                                                                  *    04448014
*------------------------------------------------------------------*    04449014
                                                                        04450014
H0050    DS    0H                                                       04451014
         CLI   0(R2),C' '             IS THIS A LEADING BLANK?          04452014
         BNE   H0060                  NO, SKIP                          04453014
         LA    R2,1(0,R2)             YES, BUMP INPUT POINTER           04454014
         BCT   R3,H0050               AND CHECK NEXT CHARACTER          04455014
         B     H0030                  ALL BLANK, IGNORE IT              04456014
H0060    DS    0H                                                       04457014
                                                                        04458014
                                                                        04459014
*------------------------------------------------------------------*    04460014
*                                                                  *    04461014
*    At this point, R2 --> first non-blank character               *    04462014
*                   R3 = nbr characters left on input record       *    04463014
*                                                                  *    04464014
*------------------------------------------------------------------*    04465014
                                                                        04466014
         LA    R4,H_Keyword_Table     A(Keyword Table)                  04467014
         LA    R5,H_Keyword_Table_End A(End of Keyword Table)           04468014
H0070    DS    0H                                                       04469014
         LA    R15,8                  Set return code, just in case     04470014
         CR    R4,R5                  End of Table?                     04471014
         BNL   H0100                  Yes, skip                         04472014
         SLR   R15,R15                No, clear R15                     04473014
         IC    R15,H_Keyword_Len-H_Keyword_Table(R4) Get len of Keyword 04474014
         CR    R15,R3                 Could this be it?                 04475014
         BNL   H0080                  No, check next keyword            04476014
         EX    R15,H9999              Yes, is this it?                  04477014
         BE    H0090                  Yes, skip                         04478014
H0080    DS    0H                                                       04479014
         LA    R4,1+L'H_Keyword_Rtn+L'H_Keyword_Len(R15,R4)             04480014
         B     H0070                  Check out next entry              04481014
H0090    DS    0H                                                       04482014
                                                                        04483014
*-------------------------------------------------------------------*   04484014
*                                                                   *   04485014
*    We have identified the keyword - point R2 to the start of the  *   04486014
*   operand field, adjust the length remaining, and go process the  *   04487014
*   keyword stuff.                                                  *   04488014
*                                                                   *   04489014
*-------------------------------------------------------------------*   04490014
                                                                        04491014
         LA    R2,1(R15,R2)           A(START OF OPERANDS)              04492014
         ST    R2,@H_Parms            Save it                           04493014
         SR    R3,R15                 ADJUST LENGTH REMAINING           04494014
         SH    R3,=H'1'               SET IT UP PROPERLY                04495014
         BNP   H0100                  INVALID, SKIP                     04496014
         ST    R3,@H_Parms+4          Save it                           04497014
         ST    R10,@H_Parms+8         Save A(ODT Entry)                 04498014
         LA    R1,@H_Parms            A(Parameters)                     04499014
         L     R15,H_Keyword_Rtn-H_Keyword_Table(R4) A(Proc Rtn)        04500014
         BASR  R14,R15                Go process keyword operands       04501014
         LTR   R15,R15                OK?                               04502014
         BNZ   H0100                  No, error message                 04503014
                                                                        04504014
         LM    R2,R3,@H_Parms         Reload our registers              04505014
         L     R10,@H_Parms+8         A(Current ODT Entry)              04506014
                                                                        04507014
*-------------------------------------------------------------------*   04508014
*                                                                   *   04509014
*    We are back: R2 --> next byte after operands, and              *   04510014
*                 R3 = length remaining                             *   04511014
*    If r2 is not pointing to a space, we have more keywords to     *   04512014
*   look for; if not, we are finished with this record.             *   04513014
*                                                                   *   04514014
*-------------------------------------------------------------------*   04515014
                                                                        04516014
         LTR   R3,R3                  Anything left?                    04517014
         BNP   H0030                  No, go get next record            04518014
         CLI   0(R2),C' '             Yes, are we done?                 04519014
         BE    H0030                  Yes, get next record              04520014
         CLI   0(R2),C','             No, is it a separator?            04521014
         LA    R15,4                  Set return code, just in case     04522014
         BNE   H0100                  Not a separator, error            04523014
         LA    R2,1(0,R2)             Yes, skip separator               04524014
         BCT   R3,H0060               and check next keyword            04525014
         LA    R15,4                  Non-blank last char, error        04526014
H0100    DS    0H                                                       04527014
                                                                        04528014
*-------------------------------------------------------------------*   04529014
*                                                                   *   04530014
*    We have found an error in the control cards - tell the user.   *   04531014
*                                                                   *   04532014
*-------------------------------------------------------------------*   04533014
                                                                        04534014
         C     R15,@H_rc              Is this the highest return code?  04535014
         BNH   H0110                  No, skip                          04536014
         ST    R15,@H_rc              Yes, save it                      04537014
H0110    DS    0H                                                       04538014
         L     R1,@_SYSPRINT_Rec_Ptr  A(SYSPRINT record)                04539014
         L     R15,@H_SYSIN_DCB_Ptr   A(SYSIN DCB)                      04540014
         S     R2,DCBRECAD-IHADCB(R15) R2=Offset of error               04541014
         LA    R2,1(R1,R2)            R2 = A(Column of error)           04542014
         MVI   0(R2),C'?'             Move a flag                       04543014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               04544014
         BASR  R14,R15                Go print it                       04545014
         L     R1,@_SYSPRINT_Rec_Ptr  A(SYSPRINT record)                04546014
         MVC   5(55,R1),=C'Error detected above ''?'' - remainder of re*04547014
               cord ignored.'                                           04548014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               04549014
         BASR  R14,R15                Go print it                       04550014
         B     H0030                  Read another record               04551014
                                                                        04552014
*-------------------------------------------------------------------*   04553014
*                                                                   *   04554014
*    The following is the EOD exit when 31-bit SAM is not supported *   04555014
*   for SYSIN. This code is moved to 24-bit storage, and is invoked *   04556014
*   by SYSIN EOD. All it does is branch to out 'real' EOD routine,  *   04557014
*   in 31-bit mode.                                                 *   04558014
*                                                                   *   04559014
*-------------------------------------------------------------------*   04560014
                                                                        04561014
H800024_Start DS 0H                  Start of 24-bit EOD Routine        04562014
         PUSH  USING                  Save current USINGs               04563014
         DROP  ,                      No USINGs now                     04564014
         BALR  R15,0                  Load up our base                  04565014
         USING *,R15                  Tell the Assembler                04566014
         L     R15,H800024_EOD        Get real EODAD                    04567014
         BSM   0,R15                  and go do it (in 31-bit mode)     04568014
H800024_EOD DC AL4(X'80000000'+H8000) Real EODAD                        04569014
         DROP  R15                    Free up our base reg              04570014
H800024_Length EQU *-H800024_Start   Length of 24-bit EOD Routine       04571014
         POP   USING                  Restore USING environment         04572014
                                                                        04573014
                                                                        04574014
H8000    DS    0H                                                       04575014
                                                                        04576014
*-------------------------------------------------------------------*   04577014
*                                                                   *   04578014
*    'Real' End-Of-File exit for SYSIN: Close SYSIN                 *   04579014
*                                                                   *   04580014
*-------------------------------------------------------------------*   04581014
                                                                        04582014
         ICM   R15,B'1111',@H_SYSIN_DCB_Ptr A(SYSIN DCB)                04583014
         BZ    H8020                  None, skip                        04584014
         TM    DCBOFLGS-IHADCB(R15),DCBOFOPN SYSIN OPEN?                04585014
         BZ    H8010                  No, better not close it           04586014
         MVC   @H_CLOSE,H_CLOSE       Yes, move close parms             04587014
         L     R2,@H_SYSIN_DCB_Ptr    Point to the DCB                  04588014
         CLOSE ((2)),MODE=31,MF=(E,@H_CLOSE)  and close it              04589014
H8010    DS    0H                                                       04590014
         L     R1,@H_SYSIN_DCB_Ptr    A(Below-the-line storage)         04591014
                                                                        04592014
         STACK POP,                     Free the stack area            +04593014
               ADDR=(R1),                starting here                 +04594014
               STACK=@_24Bit_Stack_Ptr   on this stack                  04595014
                                                                        04596014
H8020    DS    0H                                                       04597014
         B     H9000                  Exit                              04598014
                                                                        04599014
                                                                        04600014
H9000    DS    0H                                                       04601014
                                                                        04602014
*-------------------------------------------------------------------*   04603014
*                                                                   *   04604014
*    Free up our local storage ...                                  *   04605014
*                                                                   *   04606014
*-------------------------------------------------------------------*   04607014
                                                                        04608014
         L     R3,@H_rc                 Rescue the return code          04609014
                                                                        04610014
         LA    R1,@H_Dynam              A(Local storage)                04611014
         L     R13,4(0,R13)             Rescue A(HSA)                   04612014
                                                                        04613014
         STACK POP,                     Free the stack area            +04614014
               ADDR=(R1),                starting here                 +04615014
               STACK=@_31Bit_Stack_Ptr   on this stack                  04616014
                                                                        04617014
*-------------------------------------------------------------------*   04618014
*                                                                   *   04619014
*    ... and return to caller                                       *   04620014
*                                                                   *   04621014
*-------------------------------------------------------------------*   04622014
                                                                        04623014
         LR    R15,R3                   Restore return code             04624014
         L     R14,12(0,R13)            Restore return address          04625014
         LM    R0,R12,20(R13)           Restore other registers         04626014
         BSM   0,R14                    and return                      04627014
                                                                        04628014
H9998 MVC      0(0,R1),0(R2)          Move SYSIN record to SYSPRINT     04629014
H9999 CLC      0(0,R2),H_Keyword-H_Keyword_Table(R4) Check keyword      04630014
                                                                        04631014
*-------------------------------------------------------------------*   04632014
*                                                                   *   04633014
*    Table of command card Keywords, and A(Processing Routines).    *   04634014
*                                                                   *   04635014
*-------------------------------------------------------------------*   04636014
                                                                        04637014
H_Keyword_Table EQU  *                                                  04638014
*                                                                       04639014
H_Keyword_Rtn   DC    AL4(HA0010)            A(PROCESSING ROUTINE)      04640014
H_Keyword_Len   DC    AL1(5)                 EXECUTE LEN OF KEYWORD     04641014
H_Keyword       DC    C'START='              KEYWORD                    04642014
*                                                                       04643014
                DC    AL4(HA0010)            A(PROCESSING ROUTINE)      04644014
                DC    AL1(3)                 EXECUTE LEN OF KEYWORD     04645014
                DC    C'END='                KEYWORD                    04646014
                                                                        04647014
                DC    AL4(HB0010)            A(PROCESSING ROUTINE)      04648014
                DC    AL1(5)                 EXECUTE LEN OF KEYWORD     04649014
                DC    C'PRINT='              KEYWORD                    04650014
                                                                        04651014
                DC    AL4(HC0010)            A(PROCESSING ROUTINE)      04652014
                DC    AL1(7)                 EXECUTE LEN OF KEYWORD     04653014
                DC    C'INCLUDE='            KEYWORD                    04654014
                                                                        04655014
                DC    AL4(HC0010)            A(PROCESSING ROUTINE)      04656014
                DC    AL1(7)                 EXECUTE LEN OF KEYWORD     04657014
                DC    C'EXCLUDE='            KEYWORD                    04658014
                                                                        04659014
                DC    AL4(HD0010)            A(PROCESSING ROUTINE)      04660014
                DC    AL1(7)                 EXECUTE LEN OF KEYWORD     04661014
                DC    C'JOBNAME='            KEYWORD                    04662014
                                                                        04663014
                DC    AL4(HE0010)            A(PROCESSING ROUTINE)      04664014
                DC    AL1(4)                 EXECUTE LEN OF KEYWORD     04665014
                DC    C'DATA='               KEYWORD                    04666014
                                                                        04667014
                DC    AL4(HF0010)            A(PROCESSING ROUTINE)      04668014
                DC    AL1(8)                 EXECUTE LEN OF KEYWORD     04669014
                DC    C'JOB/DATA='           KEYWORD                    04670014
                                                                        04671014
                DC    AL4(HG0010)            A(PROCESSING ROUTINE)      04672014
                DC    AL1(7)                 EXECUTE LEN OF KEYWORD     04673014
                DC    C'STOPAFT='            KEYWORD                    04674014
                                                                        04675014
                DC    AL4(HH0010)            A(PROCESSING ROUTINE)      04676014
                DC    AL1(6)                 EXECUTE LEN OF KEYWORD     04677014
                DC    C'INEXIT='             KEYWORD                    04678014
                                                                        04679014
                DC    AL4(HH0010)            A(PROCESSING ROUTINE)      04680014
                DC    AL1(7)                 EXECUTE LEN OF KEYWORD     04681014
                DC    C'OUTEXIT='            KEYWORD                    04682014
                                                                        04683014
                DC    AL4(HI0010)            A(PROCESSING ROUTINE)      04684014
                DC    AL1(6)                 EXECUTE LEN OF KEYWORD     04685014
                DC    C'OUTPUT='             KEYWORD                    04686014
                                                                        04687014
                DC    AL4(HJ0010)            A(PROCESSING ROUTINE)      04688014
                DC    AL1(6)                 EXECUTE LEN OF KEYWORD     04689014
                DC    C'PERIOD='             KEYWORD                    04690014
                                                                        04691014
                DC    AL4(HK0010)            A(PROCESSING ROUTINE)      04692014
                DC    AL1(8)                 EXECUTE LEN OF KEYWORD     04693014
                DC    C'SEQUENCE='           KEYWORD                    04694014
                                                                        04695014
                DC    AL4(HL0010)            A(PROCESSING ROUTINE)      04696014
                DC    AL1(8)                 EXECUTE LEN OF KEYWORD     04697014
                DC    C'CHECKVBS='           KEYWORD                    04698014
                                                                        04699014
                DC    AL4(HM0010)            A(PROCESSING ROUTINE)      04700014
                DC    AL1(8)                 EXECUTE LEN OF KEYWORD     04701014
                DC    C'WEEKENDS='           KEYWORD                    04702014
                                                                        04703014
H_Keyword_Table_End EQU *                END OF KEYWORD TABLE           04704014
                                                                        04705014
         PUSH  PRINT                                                    04706014
         PRINT NOGEN                                                    04707014
H_SYSIN_DCB DCB  DDNAME=SYSIN,MACRF=GL,DSORG=PS,RECFM=FB,LRECL=80,     +04708014
               DCBE=H_SYSIN_DCBE                                        04709014
H_SYSIN_DCB_Length EQU *-H_SYSIN_DCB                                    04710014
                                                                        04711014
H_SYSIN_DCBE DCBE RMODE31=BUFF,EODAD=H8000                              04712014
H_SYSIN_DCBE_Length EQU  *-H_SYSIN_DCBE                                 04713014
                                                                        04714014
H_SYSIN24_DCB DCB  DDNAME=SYSIN,MACRF=GL,DSORG=PS,RECFM=FB,LRECL=80,   +04715014
               EODAD=0                                                  04716014
H_SYSIN24_DCB_Length EQU *-H_SYSIN24_DCB                                04717014
                                                                        04718014
H_OPEN   OPEN  (0,INPUT),MODE=31,MF=L                                   04719014
H_OPEN_Length  EQU   *-H_OPEN                                           04720014
H_CLOSE  CLOSE (0),MODE=31,MF=L                                         04721014
H_CLOSE_Length EQU   *-H_CLOSE                                          04722014
         POP   PRINT                                                    04723014
                                                                        04724014
         LTORG                                                          04725014
                                                                        04726014
@H_Dynam        DSECT                 Dynamic area for H                04727014
@H_Save         DS    18F              O/S Style save area              04728014
@H_Parms        DS    3AL4             Parm area                        04729014
@H_rc           DS    FL4              Return code                      04730014
@H_SYSIN_DCB_Ptr DS   AL4              A(SYSIN DCB)                     04731014
@H_SYSIN_DCBE   DS    CL(H_SYSIN_DCBE_Length) Dynamic DCBE              04732014
@H_Macros       DS    0F               Macro area                       04733014
@H_OPEN         DS    CL(H_OPEN_Length)                                 04734014
                ORG   @H_Macros                                         04735014
@H_CLOSE        DS    CL(H_CLOSE_Length)                                04736014
                ORG                                                     04737014
                DS    0D               Alignment                        04738014
@H_DynLen       EQU   *-@H_Dynam      Length of storage required        04739014
                                                                        04740014
SMFSLCT  RSECT                                                          04741014
                                                                        04742014
         DROP  R11,R13                                                  04743014
         POP   USING                                                    04744014
         EJECT                                                          04745014
HA0010   DS    0H                                                       04746014
                                                                        04747014
         PUSH  USING                                                    04748014
                                                                        04749014
*-------------------------------------------------------------------*   04750014
*                                                                   *   04751014
*    'START='/'END=' Keyword Processor                              *   04752014
*             Validate Date and Times specified                     *   04753014
*    ON ENTRY, R1 --->  A(Operand)                                  *   04754014
*                       Fullword containing # characters left in    *   04755014
*                        the input record                           *   04756014
*                       A(ODT Entry)                                *   04757014
*    ON EXIT,  R15 =   0 - ALL OK, ODTE_Start/EndDate set           *   04758014
*                      4 - ERROR FOUND                              *   04759014
*                                                                   *   04760014
*-------------------------------------------------------------------*   04761014
                                                                        04762014
                                                                        04763014
         USING *,R15                                                    04764014
         SAVE  (14,12),T                                                04765014
         LR    R11,R15                  Load permanent base reg         04766014
         DROP  R15                      Free up temp base               04767014
         USING HA0010,R11               Assign permanent base           04768014
                                                                        04769014
*-------------------------------------------------------------------*   04770014
*                                                                   *   04771014
*    Get an area on the stack for our local storage                 *   04772014
*                                                                   *   04773014
*-------------------------------------------------------------------*   04774014
                                                                        04775014
         STACK PUSH,                    Get Stack area                 +04776014
               LEN=@HA_DynLen,           this long                     +04777014
               STACK=@_31Bit_Stack_Ptr   using this stack               04778014
                                                                        04779014
         LR    R2,R1                    Save its address                04780014
         LR    R0,R1                    A(Our storage)                  04781014
         LA    R1,@HA_DynLen             Its length                     04782014
         SLR   R14,R14                  Clear source address            04783014
         SLR   R15,R15                   and length                     04784014
         MVCL  R0,R14                   Clear our storage               04785014
         LR    R1,R2                    Restore A(Our storage)          04786014
                                                                        04787014
*-------------------------------------------------------------------*   04788014
*                                                                   *   04789014
*    Chain our save areas, and restore the important registers      *   04790014
*   that we have destroyed                                          *   04791014
*                                                                   *   04792014
*-------------------------------------------------------------------*   04793014
                                                                        04794014
         ST    R13,4(0,R1)              Chain                           04795014
         ST    R1,8(0,R13)               saveareas                      04796014
         LR    R13,R1                   Load dynam base                 04797014
         USING @HA_Dynam,R13            Assign a base                   04798014
         L     R15,@HA_Dynam+4          Get A(HSA)                      04799014
         LM    R0,R3,20(R15)            Restore callers registers       04800014
                                                                        04801014
*-------------------------------------------------------------------*   04802014
*                                                                   *   04803014
*    Load our parm registers:                                       *   04804014
*         R2 ---> Operand of START=/END= keyword                    *   04805014
*         R3    = Nbr chars left in the input record                *   04806014
*                                                                   *   04807014
*-------------------------------------------------------------------*   04808014
                                                                        04809014
         LM    R2,R3,0(R1)              Get parms                       04810014
         L     R10,8(0,R1)              A(ODT Entry)                    04811014
                                                                        04812014
         LR    R15,R2                   Point to chars                  04813014
         SH    R15,=HL2'2'               before operand                 04814014
         CLC   =C'T=',0(R15)            START=                          04815014
         BNE   HA0020                   No, must be END=                04816014
         OI    @HA_Start_Proc,L'@HA_Start_Proc Yes, set flag            04817014
         B     HA0030                   and skip                        04818014
HA0020   DS    0H                                                       04819014
         NI    @HA_Start_Proc,X'FF'-L'@HA_Start_Proc Off flag           04820014
         B     HA0030                   and skip                        04821014
HA0030   DS    0H                                                       04822014
         OI    @HA_Date_Proc,L'@HA_Date_Proc SAY WE ARE DOING DATE      04823014
                                                                        04824014
*-------------------------------------------------------------------*   04825014
*                                                                   *   04826014
*    Check for our Date keywords                                    *   04827014
*                                                                   *   04828014
*-------------------------------------------------------------------*   04829014
                                                                        04830014
                                                                        04831014
         LA    R14,HA_Keyword_Table    A(Keyword Table)                 04832014
         LA    R15,HA_Keyword_Table_End A(End of Keyword Table)         04833014
HA0040   DS    0H                                                       04834014
         CR    R14,R15                End of Table?                     04835014
         BNL   HA0070                 Yes, skip                         04836014
         SLR   R1,R1                  No, clear work register           04837014
         IC    R1,HA_Keyword_Len-HA_Keyword_Table(R14) Get len of Kwd   04838014
         CR    R1,R3                  Could this be it?                 04839014
         BNL   HA0050                 No, check next keyword            04840014
         EX    R1,HA9999              Yes, is this it?                  04841014
         BE    HA0060                 Yes, skip                         04842014
HA0050   DS    0H                                                       04843014
         LA    R14,1+L'HA_Keyword_Rtn+L'HA_Keyword_Len(R1,R14)          04844014
         B     HA0040                 Check out next entry              04845014
HA0060   DS    0H                                                       04846014
                                                                        04847014
*-------------------------------------------------------------------*   04848014
*                                                                   *   04849014
*    We have identified the keyword - initialize our DateConv area  *   04850014
*   with the info we retrieved when we initialized (ie, Today) ...  *   04851014
*                                                                   *   04852014
*-------------------------------------------------------------------*   04853014
                                                                        04854014
         MVC   @HA_DateConv_Area,@_DateConv_Area                        04855014
                                                                        04856014
*-------------------------------------------------------------------*   04857014
*                                                                   *   04858014
*    ... and go process it.                                         *   04859014
*                                                                   *   04860014
*-------------------------------------------------------------------*   04861014
                                                                        04862014
         L     R15,HA_Keyword_Rtn-HA_Keyword_Table(R14) A(Proc Rtn)     04863014
         BR    R15                    Go process keyword operands       04864014
                                                                        04865014
HA0070   DS    0H                                                       04866014
                                                                        04867014
*-------------------------------------------------------------------*   04868014
*                                                                   *   04869014
*    COMMON 'START='/'END=' KEYWORD PROCESSING                      *   04870014
*                                                                   *   04871014
*-------------------------------------------------------------------*   04872014
                                                                        04873014
HA0080   DS    0H                                                       04874014
                                                                        04875014
*-------------------------------------------------------------------*   04876014
*                                                                   *   04877014
*    SET UP THE TEMPORARY AREA WITH 0, AND POINT TO IT.             *   04878014
*                                                                   *   04879014
*-------------------------------------------------------------------*   04880014
                                                                        04881014
         MVC   @HA_TEMP(7),=C'0000000' PRIME RECEIVING AREA             04882014
         LA    R4,@HA_TEMP            POINT TO IT                       04883014
HA0090   DS    0H                                                       04884014
                                                                        04885014
*-------------------------------------------------------------------*   04886014
*                                                                   *   04887014
*    AND COUNT THE NUMERIC CHARACTERS IN THE OPERAND.               *   04888014
*                                                                   *   04889014
*-------------------------------------------------------------------*   04890014
                                                                        04891014
         CLI   0(R2),C'0'             IS THIS NUMERIC?                  04892014
         BL    HA0100                 NO, SKIP                          04893014
         CLI   0(R2),C'9'             SO FAR, REALLY NUMERIC?           04894014
         BH    HA0100                 NO, SKIP                          04895014
         MVC   0(1,R4),0(R2)          YES, MOVE THE CHARACTER           04896014
         LA    R4,1(0,R4)             BUMP RECEIVING PTR                04897014
         LA    R2,1(0,R2)             POINT TO NEXT CHAR                04898014
         BCT   R3,HA0090              AND CHECK IT OUT                  04899014
HA0100   DS    0H                                                       04900014
                                                                        04901014
*-------------------------------------------------------------------*   04902014
*                                                                   *   04903014
*    WE HAVE EITHER FOUND A NON-NUMERIC CHARACTER (IE, A SEPARATOR),*   04904014
*   OR WE HAVE RUN OUT OF INPUT.                                    *   04905014
*                                                                   *   04906014
*-------------------------------------------------------------------*   04907014
                                                                        04908014
         LA    R0,@HA_TEMP            A(TEMP AREA)                      04909014
         SR    R4,R0                  GET CHARS IN INPUT                04910014
         BNP   HA0140                 NO DATA, SKIP                     04911014
                                                                        04912014
*-------------------------------------------------------------------*   04913014
*                                                                   *   04914014
*    THE DATA SEEMS OK, SO WE WILL PACK IT.                         *   04915014
*              R1   = A(START OF TARGET FIELDS)                     *   04916014
*                                                                   *   04917014
*-------------------------------------------------------------------*   04918014
                                                                        04919014
         TM    @HA_Time_Proc,L'@HA_Time_Proc ARE WE DOING TIME?         04920014
         BO    HA0150                 YES, GO SET IT UP                 04921014
         CH    R4,=H'5'               NO, IS DATE 5 CHARS?              04922014
         BNE   HA0110                 No, skip                          04923014
         PACK  @HA_DBLWD,@HA_TEMP(5)  YES, PACK IT                      04924014
         MVC   @HA_DBLWD+4(1),@_DateConv_Area+DConv_Date_SMFDate-DateCo+04925014
               nv_Area                Move current century              04926014
         B     HA0120                 and skip                          04927014
HA0110   DS    0H                                                       04928014
         CH    R4,=H'7'               IS DATE 7 CHARS?                  04929014
         BNE   HA8000                 NO, ERROR                         04930014
         PACK  @HA_DBLWD,@HA_TEMP(7)  Yes, pack it                      04931014
         SP    @HA_DBLWD,=P'1900000'  Adjust for SMF Date convention    04932014
HA0120   DS    0H                                                       04933014
         TM    @HA_Start_Proc,L'@HA_Start_Proc Processing START=?       04934014
         BNO   HA0130                 No, must be END=                  04935014
         ZAP   ODTE_StartDate-ODT_Entry(L'ODTE_StartDate,R10),@HA_DBLWD+04936014
               +4(4)                  Save Start Date                   04937014
         B     HA0140                 And skip                          04938014
HA0130   DS    0H                                                       04939014
         ZAP   ODTE_EndDate-ODT_Entry(L'ODTE_EndDate,R10),@HA_DBLWD+4(4+04940014
               )                      Save End date                     04941014
         B     HA0140                 And skip                          04942014
HA0140   DS    0H                                                       04943014
                                                                        04944014
*-------------------------------------------------------------------*   04945014
*                                                                   *   04946014
*    NOW SEE IF WE HAVE ANY MORE TO DO.                             *   04947014
*                                                                   *   04948014
*-------------------------------------------------------------------*   04949014
                                                                        04950014
         LTR   R3,R3                  ANY INPUT LEFT?                   04951014
         BZ    HA0180                 NO, WE'RE FINISHED                04952014
         CLI   0(R2),C'-'             IS THIS THE DATE SEPARATOR?       04953014
         BNE   HA0170                 NO, SKIP                          04954014
         TM    @HA_Date_Proc,L'@HA_Date_Proc WERE WE DOING THE DATE?    04955014
         BNO   HA8000                 NO, ERROR                         04956014
                                                                        04957014
*-------------------------------------------------------------------*   04958014
*                                                                   *   04959014
*    WE HAVE FOUND THE DATE/TIME SEPARATOR, SO WE WILL GO DO THE    *   04960014
*   TIME                                                            *   04961014
*                                                                   *   04962014
*-------------------------------------------------------------------*   04963014
                                                                        04964014
         NI    @HA_Date_Proc,X'FF'-L'@HA_Date_Proc  Yes, Off Date flag  04965014
         OI    @HA_Time_Proc,L'@HA_Time_Proc Now we are doing time      04966014
         LA    R2,1(0,R2)             POINT OVER THE SEPARATOR          04967014
         BCT   R3,HA0080              AND DO THE TIME                   04968014
         B     HA8000                 SEPARATOR BUT NO TIME, ERROR      04969014
HA0150   DS    0H                                                       04970014
                                                                        04971014
*-------------------------------------------------------------------*   04972014
*                                                                   *   04973014
*    WE HAVE FOUND THE TIME, SPECIFIED AS HHMMSS - WE MUST          *   04974014
*   CONVERT IT TO SECONDS*100 SINCE MIDNIGHT                        *   04975014
*                                                                   *   04976014
*-------------------------------------------------------------------*   04977014
                                                                        04978014
         XC    @HA_DateConv_Area,@HA_DateConv_Area Clear Date area      04979014
         PACK  @HA_DBLWD,@HA_TEMP(2)  PACK HH                           04980014
         CVB   R15,@HA_DBLWD          BINARIZE IT                       04981014
         STC   R15,@HA_DateConv_Area+DConv_Time_hh-DateConv_Area        04982014
         PACK  @HA_DBLWD,@HA_TEMP+2(2) GET NBR MINUTES                  04983014
         CVB   R15,@HA_DBLWD          BINARIZE THEM                     04984014
         STC   R15,@HA_DateConv_Area+DConv_Time_mm-DateConv_Area        04985014
         PACK  @HA_DBLWD,@HA_TEMP+4(2) PACK SECONDS                     04986014
         CVB   R15,@HA_DBLWD          BINARIZE THEM                     04987014
         STC   R15,@HA_DateConv_Area+DConv_Time_ss-DateConv_Area        04988014
         OI    @HA_DateConv_Area+DConv_Input_hhmmss-DateConv_Area,L'DCo+04989014
               nv_Input_hhmmss       Indicate Time conversion           04990014
         LA    R1,@HA_DateConv_Area  Point to Date Conversion area      04991014
         L     R15,=AL4(K0010)       A(Format routine)                  04992014
         BASR  R14,R15               Go get seconds since midnight      04993014
         LTR   R15,R15               Everything OK?                     04994014
         BNZ   HA8000                                                   04995014
                                                                        04996014
         L     R15,@HA_DateConv_Area+DConv_Time-DateConv_Area           04997014
         TM    @HA_Start_Proc,L'@HA_Start_Proc Processing START=?       04998014
         BNO   HA0160                 No, must be END=                  04999014
         ST    R15,ODTE_StartTime-ODT_Entry(R10) Save Start Time        05000014
         B     HA0170                 And skip                          05001014
HA0160   DS    0H                                                       05002014
         ST    R15,ODTE_EndTime-ODT_Entry(R10) Save End Time            05003014
         B     HA0170                 And skip                          05004014
HA0170   DS    0H                                                       05005014
                                                                        05006014
*-------------------------------------------------------------------*   05007014
*                                                                   *   05008014
*    WE HAVE FOUND A NON-NUMERIC CHARACTER - WE WILL EXIT, WITH     *   05009014
*   THE RETURN CODE 0 IF IT IS A VALID SEPARATOR, 4 IF IT IS NOT.   *   05010014
*                                                                   *   05011014
*-------------------------------------------------------------------*   05012014
                                                                        05013014
         CLI   0(R2),C','             IS THIS A VALID SPEARATOR?        05014014
         BE    HA0180                 YES, SKIP                         05015014
         CLI   0(R2),C' '             NO, TRY AGAIN                     05016014
         BNE   HA8000                 INVALID CHARACTER, ERROR          05017014
HA0180   DS    0H                                                       05018014
         SLR   R15,R15                CLEAR RETURN CODE                 05019014
         B     HA9000                 AND EXIT                          05020014
                                                                        05021014
HA1000   DS    0H                                                       05022014
                                                                        05023014
*-------------------------------------------------------------------*   05024014
*                                                                   *   05025014
*    START/END=TODAY                                                *   05026014
*     On Entry, R2 = A("TODAY")                                     *   05027014
*               R3 = Nbr chars left                                 *   05028014
*     On Exit,  R2 ---> Byte after "TODAY"                          *   05029014
*               R3 = Nbr Chars left                                 *   05030014
*               @HA_DBLWD contains SMF Date                         *   05031014
*                                                                   *   05032014
*-------------------------------------------------------------------*   05033014
                                                                        05034014
         LA    R2,5(0,R2)             Adjust pointer                    05035014
         SH    R3,=HL2'5'             Adjust chars remaining            05036014
         ZAP   @HA_DBLWD,@HA_DateConv_Area+DConv_Date_SMFDate-DateConv_+05037014
               Area(L'DConv_Date_SMFDate) Set up Date                   05038014
         B     HA0120                 and exit                          05039014
                                                                        05040014
HA2000   DS    0H                                                       05041014
                                                                        05042014
*-------------------------------------------------------------------*   05043014
*                                                                   *   05044014
*    START/END=(TODAY-n)                                            *   05045014
*     On Entry, R2 = A("(TODAY-")                                   *   05046014
*               R3 = Nbr chars left                                 *   05047014
*     On Exit,  R2 ---> Byte after "(TODAY-n)"                      *   05048014
*               R3 = Nbr Chars left                                 *   05049014
*               @HA_DBLWD contains SMF Date                         *   05050014
*                                                                   *   05051014
*-------------------------------------------------------------------*   05052014
                                                                        05053014
         LA    R2,7(0,R2)             Adjust pointer                    05054014
         SH    R3,=HL2'7'             Adjust chars remaining            05055014
         BNP   HA8000                 Error, exit                       05056014
         LR    R14,R2                 Save A(start)                     05057014
HA2010   DS    0H                                                       05058014
         CLI   0(R2),C')'             Closing paren?                    05059014
         BE    HA2020                 Yes, skip                         05060014
         CLI   0(R2),C'0'             No, is it numeric?                05061014
         BL    HA8000                 No, error                         05062014
         CLI   0(R2),C'9'             So far, is it really?             05063014
         BH    HA8000                 No, error                         05064014
         LA    R2,1(0,R2)             Yes, bump pointer                 05065014
         BCT   R3,HA2010              and try again                     05066014
         B     HA8000                 No closing paren, error           05067014
HA2020   DS    0H                                                       05068014
         LR    R15,R2                 A(closing paren)                  05069014
         SR    R15,R14                Length of numerics                05070014
         BNP   HA8000                 Nothing, error                    05071014
         S     R15,=FL4'1'            EX length                         05072014
         EX    R15,HA2999             Pack it                           05073014
         CVB   R15,@HA_DBLWD          Binarize it                       05074014
         L     R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05075014
         SR    R14,R15                Adjust Base date                  05076014
         ST    R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05077014
         OI    @HA_DateConv_Area+DConv_Input_Base-DateConv_Area,L'DConv+05078014
               _Input_Base           Indicate Base Date                 05079014
         LA    R1,@HA_DateConv_Area  Point to Date Conversion area      05080014
         L     R15,=AL4(K0010)       A(Format routine)                  05081014
         BASR  R14,R15               Go new SMFDate                     05082014
         LTR   R15,R15               Everything OK?                     05083014
         BNZ   HA8000                                                   05084014
                                                                        05085014
         ZAP   @HA_DBLWD,@HA_DateConv_Area+DConv_Date_SMFDate-DateConv_+05086014
               Area(L'DConv_Date_SMFDate) Set up Date                   05087014
         LA    R2,1(0,R2)            Point past closing paren           05088014
         S     R3,=FL4'1'            Adjust length remaining            05089014
         B     HA0120                 and exit                          05090014
                                                                        05091014
HA2999   PACK  @HA_DBLWD,0(0,R14)     Pack offset from today            05092014
                                                                        05093014
HA3000   DS    0H                                                       05094014
                                                                        05095014
*-------------------------------------------------------------------*   05096014
*                                                                   *   05097014
*    START/END=YESTERDAY                                            *   05098014
*     On Entry, R2 = A("YESTERDAY"                                  *   05099014
*               R3 = Nbr chars left                                 *   05100014
*     On Exit,  R2 ---> Byte after "YESTERDAY"                      *   05101014
*               R3 = Nbr Chars left                                 *   05102014
*               @HA_DBLWD contains SMF Date                         *   05103014
*                                                                   *   05104014
*-------------------------------------------------------------------*   05105014
                                                                        05106014
         LA    R2,9(0,R2)             Adjust pointer                    05107014
         SH    R3,=HL2'9'             Adjust chars remaining            05108014
         L     R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05109014
         S     R14,=FL4'1'            Adjust Base date                  05110014
         ST    R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05111014
         OI    @HA_DateConv_Area+DConv_Input_Base-DateConv_Area,L'DConv+05112014
               _Input_Base           Indicate Base Date                 05113014
         LA    R1,@HA_DateConv_Area  Point to Date Conversion area      05114014
         L     R15,=AL4(K0010)       A(Format routine)                  05115014
         BASR  R14,R15               Go new SMFDate                     05116014
         LTR   R15,R15               Everything OK?                     05117014
         BNZ   HA8000                                                   05118014
                                                                        05119014
         ZAP   @HA_DBLWD,@HA_DateConv_Area+DConv_Date_SMFDate-DateConv_+05120014
               Area(L'DConv_Date_SMFDate) Set up Date                   05121014
         B     HA0120                 and exit                          05122014
                                                                        05123014
HA4000   DS    0H                                                       05124014
                                                                        05125014
*-------------------------------------------------------------------*   05126014
*                                                                   *   05127014
*    START/END=THISWEEK                                             *   05128014
*     On Entry, R2 = A("THISWEEK")                                  *   05129014
*               R3 = Nbr chars left                                 *   05130014
*     On Exit,  R2 ---> Byte after "THISWEEK"                       *   05131014
*               R3 = Nbr Chars left                                 *   05132014
*               @HA_DBLWD contains SMF Date                         *   05133014
*                                                                   *   05134014
*-------------------------------------------------------------------*   05135014
                                                                        05136014
         LA    R2,8(0,R2)             Adjust pointer                    05137014
         SH    R3,=HL2'8'             Adjust chars remaining            05138014
         L     R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05139014
         SLR   R15,R15                Clear work register               05140014
         IC    R15,@HA_DateConv_Area+DConv_Date_DOW-DateConv_Area       05141014
         TM    @HA_Start_Proc,L'@HA_Start_Proc doing a START=           05142014
         BNO   HA4010                 No, skip                          05143014
         SR    R14,R15                Yes, back up to Monday            05144014
         B     HA4020                 and skip                          05145014
HA4010   DS    0H                                                       05146014
         LA    R0,6                   Day-of-week for Sunday            05147014
         SR    R0,R15                 Nbr days until Sunday             05148014
         AR    R14,R0                 Adjust base date                  05149014
HA4020   DS    0H                                                       05150014
         ST    R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05151014
         OI    @HA_DateConv_Area+DConv_Input_Base-DateConv_Area,L'DConv+05152014
               _Input_Base           Indicate Base Date                 05153014
         LA    R1,@HA_DateConv_Area  Point to Date Conversion area      05154014
         L     R15,=AL4(K0010)       A(Format routine)                  05155014
         BASR  R14,R15               Go new SMFDate                     05156014
         LTR   R15,R15               Everything OK?                     05157014
         BNZ   HA8000                                                   05158014
                                                                        05159014
         ZAP   @HA_DBLWD,@HA_DateConv_Area+DConv_Date_SMFDate-DateConv_+05160014
               Area(L'DConv_Date_SMFDate) Set up Date                   05161014
         B     HA0120                 and exit                          05162014
                                                                        05163014
HA5000   DS    0H                                                       05164014
                                                                        05165014
*-------------------------------------------------------------------*   05166014
*                                                                   *   05167014
*    START/END=LASTWEEK                                             *   05168014
*     On Entry, R2 = A("LASTWEEK")                                  *   05169014
*               R3 = Nbr chars left                                 *   05170014
*     On Exit,  R2 ---> Byte after "LASTWEEK"                       *   05171014
*               R3 = Nbr Chars left                                 *   05172014
*               @HA_DBLWD contains SMF Date                         *   05173014
*                                                                   *   05174014
*-------------------------------------------------------------------*   05175014
                                                                        05176014
         LA    R2,8(0,R2)             Adjust pointer                    05177014
         SH    R3,=HL2'8'             Adjust chars remaining            05178014
         L     R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05179014
         S     R14,=FL4'7'            Back up a week                    05180014
         SLR   R15,R15                Clear work register               05181014
         IC    R15,@HA_DateConv_Area+DConv_Date_DOW-DateConv_Area       05182014
         TM    @HA_Start_Proc,L'@HA_Start_Proc doing a START=           05183014
         BNO   HA5010                 No, skip                          05184014
         SR    R14,R15                Yes, back up to Monday            05185014
         B     HA5020                 and skip                          05186014
HA5010   DS    0H                                                       05187014
         LA    R0,6                   Day-of-week for Sunday            05188014
         SR    R0,R15                 Nbr days until Sunday             05189014
         AR    R14,R0                 Adjust base date                  05190014
HA5020   DS    0H                                                       05191014
         ST    R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05192014
         OI    @HA_DateConv_Area+DConv_Input_Base-DateConv_Area,L'DConv+05193014
               _Input_Base           Indicate Base Date                 05194014
         LA    R1,@HA_DateConv_Area  Point to Date Conversion area      05195014
         L     R15,=AL4(K0010)       A(Format routine)                  05196014
         BASR  R14,R15               Go new SMFDate                     05197014
         LTR   R15,R15               Everything OK?                     05198014
         BNZ   HA8000                                                   05199014
                                                                        05200014
         ZAP   @HA_DBLWD,@HA_DateConv_Area+DConv_Date_SMFDate-DateConv_+05201014
               Area(L'DConv_Date_SMFDate) Set up Date                   05202014
         B     HA0120                 and exit                          05203014
                                                                        05204014
HA6000   DS    0H                                                       05205014
                                                                        05206014
*-------------------------------------------------------------------*   05207014
*                                                                   *   05208014
*    START/END=THISMONTH                                            *   05209014
*     On Entry, R2 = A("THISMONTH")                                 *   05210014
*               R3 = Nbr chars left                                 *   05211014
*     On Exit,  R2 ---> Byte after "THISMONTH"                      *   05212014
*               R3 = Nbr Chars left                                 *   05213014
*               @HA_DBLWD contains SMF Date                         *   05214014
*                                                                   *   05215014
*-------------------------------------------------------------------*   05216014
                                                                        05217014
         LA    R2,9(0,R2)             Adjust pointer                    05218014
         SH    R3,=HL2'9'             Adjust chars remaining            05219014
         L     R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05220014
         SLR   R15,R15                                                  05221014
         IC    R15,@HA_DateConv_Area+DConv_Date_DD-DateConv_Area        05222014
         TM    @HA_Start_Proc,L'@HA_Start_Proc doing a START=           05223014
         BNO   HA6010                 No, skip                          05224014
         S     R15,=FL4'1'            Yes, nbr days past the 1st        05225014
         SR    R14,R15                Back up to the 1st                05226014
         B     HA6020                 and skip                          05227014
HA6010   DS    0H                                                       05228014
         LA    R0,27                  Minimum days-per-month-1          05229014
         SR    R0,R15                 Nbr days until the 27th           05230014
         AR    R14,R0                 Adjust base date                  05231014
         ST    R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05232014
         SLR   R0,R0                  Clear work register               05233014
         IC    R0,@HA_DateConv_Area+DConv_Date_MM-DateConv_Area Get mon 05234014
HA6012   DS    0H                                                       05235014
         L     R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05236014
         A     R14,=FL4'1'            Bump a day                        05237014
         ST    R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05238014
         OI    @HA_DateConv_Area+DConv_Input_Base-DateConv_Area,L'DConv+05239014
               _Input_Base           Indicate Base Date                 05240014
         LA    R1,@HA_DateConv_Area  Point to Date Conversion area      05241014
         L     R15,=AL4(K0010)       A(Format routine)                  05242014
         BASR  R14,R15               Go new SMFDate                     05243014
         LTR   R15,R15               Everything OK?                     05244014
         BNZ   HA8000                                                   05245014
                                                                        05246014
         SLR   R15,R15               Clear work register                05247014
         IC    R15,@HA_DateConv_Area+DConv_Date_MM-DateConv_Area        05248014
         CR    R15,R0                Same month?                        05249014
         BE    HA6012                Yes, bump again                    05250014
         L     R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05251014
         S     R14,=FL4'1'           Back up a day                      05252014
HA6020   DS    0H                                                       05253014
         ST    R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05254014
         OI    @HA_DateConv_Area+DConv_Input_Base-DateConv_Area,L'DConv+05255014
               _Input_Base           Indicate Base Date                 05256014
         LA    R1,@HA_DateConv_Area  Point to Date Conversion area      05257014
         L     R15,=AL4(K0010)       A(Format routine)                  05258014
         BASR  R14,R15               Go new SMFDate                     05259014
         LTR   R15,R15               Everything OK?                     05260014
         BNZ   HA8000                                                   05261014
                                                                        05262014
         ZAP   @HA_DBLWD,@HA_DateConv_Area+DConv_Date_SMFDate-DateConv_+05263014
               Area(L'DConv_Date_SMFDate) Set up Date                   05264014
         B     HA0120                 and exit                          05265014
                                                                        05266014
HA7000   DS    0H                                                       05267014
                                                                        05268014
*-------------------------------------------------------------------*   05269014
*                                                                   *   05270014
*    START/END=LASTMONTH                                            *   05271014
*     On Entry, R2 = A("LASTMONTH")                                 *   05272014
*               R3 = Nbr chars left                                 *   05273014
*     On Exit,  R2 ---> Byte after "LASTMONTH"                      *   05274014
*               R3 = Nbr Chars left                                 *   05275014
*               @HA_DBLWD contains SMF Date                         *   05276014
*                                                                   *   05277014
*-------------------------------------------------------------------*   05278014
                                                                        05279014
         LA    R2,9(0,R2)             Adjust pointer                    05280014
         SH    R3,=HL2'9'             Adjust chars remaining            05281014
         L     R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05282014
         SLR   R15,R15                                                  05283014
         IC    R15,@HA_DateConv_Area+DConv_Date_DD-DateConv_Area        05284014
         SR    R14,R15                Last day of prev month            05285014
         TM    @HA_Start_Proc,L'@HA_Start_Proc doing a START=           05286014
         BNO   HA7020                 No, all done                      05287014
                                                                        05288014
         ST    R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05289014
         OI    @HA_DateConv_Area+DConv_Input_Base-DateConv_Area,L'DConv+05290014
               _Input_Base           Indicate Base Date                 05291014
         LA    R1,@HA_DateConv_Area  Point to Date Conversion area      05292014
         L     R15,=AL4(K0010)       A(Format routine)                  05293014
         BASR  R14,R15               Go new SMFDate                     05294014
         LTR   R15,R15               Everything OK?                     05295014
         BNZ   HA8000                                                   05296014
                                                                        05297014
         SLR   R15,R15               Clear work register                05298014
         IC    R15,@HA_DateConv_Area+DConv_Date_DD-DateConv_Area        05299014
         S     R15,=FL4'1'           Nbr days until the 1st             05300014
         L     R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05301014
         SR    R14,R15               Back up to the 1st                 05302014
HA7020   DS    0H                                                       05303014
         ST    R14,@HA_DateConv_Area+DConv_Date_Base-DateConv_Area      05304014
         OI    @HA_DateConv_Area+DConv_Input_Base-DateConv_Area,L'DConv+05305014
               _Input_Base           Indicate Base Date                 05306014
         LA    R1,@HA_DateConv_Area  Point to Date Conversion area      05307014
         L     R15,=AL4(K0010)       A(Format routine)                  05308014
         BASR  R14,R15               Go new SMFDate                     05309014
         LTR   R15,R15               Everything OK?                     05310014
         BNZ   HA8000                                                   05311014
                                                                        05312014
         ZAP   @HA_DBLWD,@HA_DateConv_Area+DConv_Date_SMFDate-DateConv_+05313014
               Area(L'DConv_Date_SMFDate) Set up Date                   05314014
         B     HA0120                 and exit                          05315014
                                                                        05316014
HA8000   DS    0H                                                       05317014
         LA    R15,4                  SET RETURN CODE                   05318014
         B     HA9000                 AND EXIT                          05319014
HA9000   DS    0H                                                       05320014
                                                                        05321014
*-------------------------------------------------------------------*   05322014
*                                                                   *   05323014
*    Free up our local storage ...                                  *   05324014
*                                                                   *   05325014
*-------------------------------------------------------------------*   05326014
                                                                        05327014
         L     R1,4(0,R13)              A(HSA)                          05328014
         L     R1,24(0,R1)              Get original R1                 05329014
         STM   R2,R3,0(R1)              Update parms                    05330014
                                                                        05331014
         LR    R3,R15                   Rescue the return code          05332014
                                                                        05333014
         LA    R1,@HA_Dynam             A(Local storage)                05334014
         L     R13,4(0,R13)             Rescue A(HSA)                   05335014
                                                                        05336014
         STACK POP,                     Free the stack area            +05337014
               ADDR=(R1),                starting here                 +05338014
               STACK=@_31Bit_Stack_Ptr   on this stack                  05339014
                                                                        05340014
*-------------------------------------------------------------------*   05341014
*                                                                   *   05342014
*    ... and return to caller                                       *   05343014
*                                                                   *   05344014
*-------------------------------------------------------------------*   05345014
                                                                        05346014
         LR    R15,R3                   Restore return code             05347014
         L     R14,12(0,R13)            Restore return address          05348014
         LM    R0,R12,20(R13)           Restore other registers         05349014
         BSM   0,R14                    and return                      05350014
                                                                        05351014
HA9999 CLC     0(0,R2),HA_Keyword-HA_Keyword_Table(R14) Check keyword   05352014
                                                                        05353014
HA_Time   TIME  LINKAGE=SYSTEM,MF=L                                     05354014
HA_Time_Length EQU *-HA_Time                                            05355014
                                                                        05356014
HA_Keyword_Table EQU *                                                  05357014
                                                                        05358014
HA_Keyword_Rtn  DC    AL4(HA1000)            A(PROCESSING ROUTINE)      05359014
HA_Keyword_Len  DC    AL1(4)                 EXECUTE LEN OF KEYWORD     05360014
HA_Keyword      DC    C'TODAY'               KEYWORD                    05361014
                                                                        05362014
                DC    AL4(HA2000)            A(PROCESSING ROUTINE)      05363014
                DC    AL1(6)                 EXECUTE LEN OF KEYWORD     05364014
                DC    C'(TODAY-'             KEYWORD                    05365014
                                                                        05366014
                DC    AL4(HA3000)            A(PROCESSING ROUTINE)      05367014
                DC    AL1(8)                 EXECUTE LEN OF KEYWORD     05368014
                DC    C'YESTERDAY'           KEYWORD                    05369014
                                                                        05370014
                DC    AL4(HA4000)            A(PROCESSING ROUTINE)      05371014
                DC    AL1(7)                 EXECUTE LEN OF KEYWORD     05372014
                DC    C'THISWEEK'            KEYWORD                    05373014
                                                                        05374014
                DC    AL4(HA5000)            A(PROCESSING ROUTINE)      05375014
                DC    AL1(7)                 EXECUTE LEN OF KEYWORD     05376014
                DC    C'LASTWEEK'            KEYWORD                    05377014
                                                                        05378014
                DC    AL4(HA6000)            A(PROCESSING ROUTINE)      05379014
                DC    AL1(8)                 EXECUTE LEN OF KEYWORD     05380014
                DC    C'THISMONTH'           KEYWORD                    05381014
                                                                        05382014
                DC    AL4(HA7000)            A(PROCESSING ROUTINE)      05383014
                DC    AL1(8)                 EXECUTE LEN OF KEYWORD     05384014
                DC    C'LASTMONTH'           KEYWORD                    05385014
                                                                        05386014
HA_Keyword_Table_End EQU *                END OF KEYWORD TABLE          05387014
                                                                        05388014
         LTORG                                                          05389014
                                                                        05390014
@HA_Dynam       DSECT                 Dynamic area for HA               05391014
@HA_Save        DS    18F              O/S Style save area              05392014
@HA_DBLWD       DS    D                Work area                        05393014
@HA_TEMP        DS    CL7              Work area                        05394014
                DS    X                Flag Byte                        05395014
@HA_Start_Proc  EQU   *-1,X'80'         1... .... Processing START=     05396014
@HA_Date_Proc   EQU   *-1,X'02'         .... ..1. Processing Date       05397014
@HA_Time_Proc   EQU   *-1,X'01'         .... ...1 Processing Time       05398014
@HA_Macros      DS    0F                                                05399014
@HA_Time        DS    CL(HA_Time_Length) L-Form of Time                 05400014
@HA_TimeArea    DS    CL16             Return area for Time             05401014
                ORG   @HA_Macros                                        05402014
@HA_DateConv_Area DS  CL(DConv_Area_Length)                             05403014
                ORG                                                     05404014
                DS    0D               Alignment                        05405014
@HA_DynLen      EQU   *-@HA_Dynam     Length of storage required        05406014
                                                                        05407014
SMFSLCT  RSECT                                                          05408014
                                                                        05409014
         DROP  R11,R13                                                  05410014
         POP   USING                                                    05411014
         EJECT                                                          05412014
HB0010   DS    0H                                                       05413014
                                                                        05414014
         PUSH  USING                                                    05415014
                                                                        05416014
*-------------------------------------------------------------------*   05417014
*                                                                   *   05418014
*    'PRINT=' Keyword PROCESSOR.                                    *   05419014
*             VALID VALUES ARE:                                     *   05420014
*                     "NO"     - DO NOT PRINT SELECTED RECORDS      *   05421014
*                     "EBCDIC" - PRINT SELECTED RECORDS IN EBCDIC   *   05422014
*                                ONLY                               *   05423014
*    ON ENTRY, R1 --->  A(Operand)                                  *   05424014
*                       Fullword containing # characters left in    *   05425014
*                        the input record                           *   05426014
*                       A(ODT Entry)                                *   05427014
*    ON EXIT,  R15 =   0 - ALL OK, ODTE_Print_NO flag set           *   05428014
*                      4 - ERROR FOUND                              *   05429014
*                                                                   *   05430014
*-------------------------------------------------------------------*   05431014
                                                                        05432014
         USING *,R15                                                    05433014
         SAVE  (14,12),T                                                05434014
         LR    R11,R15                  Load permanent base reg         05435014
         DROP  R15                      Free up temp base               05436014
         USING HB0010,R11               Assign permanent base           05437014
                                                                        05438014
*-------------------------------------------------------------------*   05439014
*                                                                   *   05440014
*    Get an area on the stack for our local storage                 *   05441014
*                                                                   *   05442014
*-------------------------------------------------------------------*   05443014
                                                                        05444014
         STACK PUSH,                    Get Stack area                 +05445014
               LEN=@HB_DynLen,           this long                     +05446014
               STACK=@_31Bit_Stack_Ptr   using this stack               05447014
                                                                        05448014
         LR    R2,R1                    Save its address                05449014
         LR    R0,R1                    A(Our storage)                  05450014
         LA    R1,@HB_DynLen             Its length                     05451014
         SLR   R14,R14                  Clear source address            05452014
         SLR   R15,R15                   and length                     05453014
         MVCL  R0,R14                   Clear our storage               05454014
         LR    R1,R2                    Restore A(Our storage)          05455014
                                                                        05456014
*-------------------------------------------------------------------*   05457014
*                                                                   *   05458014
*    Chain our save areas, and restore the important registers      *   05459014
*   that we have destroyed                                          *   05460014
*                                                                   *   05461014
*-------------------------------------------------------------------*   05462014
                                                                        05463014
         ST    R13,4(0,R1)              Chain                           05464014
         ST    R1,8(0,R13)               saveareas                      05465014
         LR    R13,R1                   Load dynam base                 05466014
         USING @HB_Dynam,R13            Assign a base                   05467014
         L     R15,@HB_Dynam+4          Get A(HSA)                      05468014
         LM    R0,R3,20(R15)            Restore callers registers       05469014
                                                                        05470014
*-------------------------------------------------------------------*   05471014
*                                                                   *   05472014
*    Load our parm registers:                                       *   05473014
*         R2 ---> Operand of PRINT= keyword                         *   05474014
*         R3    = Nbr chars left in the input record                *   05475014
*                                                                   *   05476014
*-------------------------------------------------------------------*   05477014
                                                                        05478014
         LM    R2,R3,0(R1)              Get parms                       05479014
         L     R10,8(0,R1)              A(ODT Entry)                    05480014
                                                                        05481014
*-------------------------------------------------------------------*   05482014
*                                                                   *   05483014
*    CHECK FOR "NO" KEYWORD.                                        *   05484014
*                                                                   *   05485014
*-------------------------------------------------------------------*   05486014
                                                                        05487014
         CH    R3,=H'2'               ENOUGH ROOM LEFT FOR LITERAL?     05488014
         BNH   HB8000                 NO, ERROR                         05489014
         CLC   0(2,R2),=C'NO'         YES, IS THIS IT?                  05490014
         BNE   HB0020                 NO, skip                          05491014
         OI    ODTE_Print_NO-ODT_Entry(R10),L'ODTE_Print_NO YES, SET    05492014
         LA    R2,2(0,R2)             POINT PAST LITERAL                05493014
         SH    R3,=H'2'               RESET LENGTH REMAINING            05494014
         SLR   R15,R15                ASSUME EVERYTHING IS OK           05495014
         CLI   0(R2),C' '             VALID SEPARATOR?                  05496014
         BE    HB9000                 YES, OK                           05497014
         CLI   0(R2),C','             NO, TRY AGAIN                     05498014
         BE    HB9000                 YES, SKIP                         05499014
         B     HB8000                 NO, ERROR                         05500014
HB0020   DS    0H                                                       05501014
                                                                        05502014
*-------------------------------------------------------------------*   05503014
*                                                                   *   05504014
*    CHECK FOR "EBCDIC" KEYWORD.                                    *   05505014
*                                                                   *   05506014
*-------------------------------------------------------------------*   05507014
                                                                        05508014
         CH    R3,=H'6'               ENOUGH ROOM LEFT FOR LITERAL?     05509014
         BNH   HB8000                 NO, ERROR                         05510014
         CLC   0(6,R2),=C'EBCDIC'     YES, IS THIS IT?                  05511014
         BNE   HB8000                 NO, ERROR                         05512014
         OI    ODTE_Print_EB-ODT_Entry(R10),L'ODTE_Print_EB YES, SET    05513014
         LA    R2,6(0,R2)             POINT PAST LITERAL                05514014
         SH    R3,=H'6'               RESET LENGTH REMAINING            05515014
         SLR   R15,R15                ASSUME EVERYTHING IS OK           05516014
         CLI   0(R2),C' '             VALID SEPARATOR?                  05517014
         BE    HB9000                 YES, OK                           05518014
         CLI   0(R2),C','             NO, TRY AGAIN                     05519014
         BE    HB9000                 YES, SKIP                         05520014
         B     HB8000                 NO, ERROR                         05521014
HB8000   DS    0H                                                       05522014
                                                                        05523014
*-------------------------------------------------------------------*   05524014
*                                                                   *   05525014
*    SOME SORT OF ERROR FOUND, SET RETURN CODE                      *   05526014
*                                                                   *   05527014
*-------------------------------------------------------------------*   05528014
                                                                        05529014
         LA    R15,4                  FLAG AN ERROR                     05530014
         B     HB9000                 AND EXIT                          05531014
HB9000   DS    0H                                                       05532014
                                                                        05533014
*-------------------------------------------------------------------*   05534014
*                                                                   *   05535014
*    Free up our local storage ...                                  *   05536014
*                                                                   *   05537014
*-------------------------------------------------------------------*   05538014
                                                                        05539014
         L     R1,4(0,R13)              A(HSA)                          05540014
         L     R1,24(0,R1)              Get original R1                 05541014
         STM   R2,R3,0(R1)              Update parms                    05542014
                                                                        05543014
         LR    R3,R15                   Rescue the return code          05544014
                                                                        05545014
         LA    R1,@HB_Dynam             A(Local storage)                05546014
         L     R13,4(0,R13)             Rescue A(HSA)                   05547014
                                                                        05548014
         STACK POP,                     Free the stack area            +05549014
               ADDR=(R1),                starting here                 +05550014
               STACK=@_31Bit_Stack_Ptr   on this stack                  05551014
                                                                        05552014
*-------------------------------------------------------------------*   05553014
*                                                                   *   05554014
*    ... and return to caller                                       *   05555014
*                                                                   *   05556014
*-------------------------------------------------------------------*   05557014
                                                                        05558014
         LR    R15,R3                   Restore return code             05559014
         L     R14,12(0,R13)            Restore return address          05560014
         LM    R0,R12,20(R13)           Restore other registers         05561014
         BSM   0,R14                    and return                      05562014
                                                                        05563014
                                                                        05564014
         LTORG                                                          05565014
                                                                        05566014
@HB_Dynam       DSECT                 Dynamic area for J                05567014
@HB_Save        DS    18F              O/S Style save area              05568014
                DS    0D               Alignment                        05569014
@HB_DynLen      EQU   *-@HB_Dynam     Length of storage required        05570014
                                                                        05571014
SMFSLCT  RSECT                                                          05572014
                                                                        05573014
         DROP  R11,R13                                                  05574014
         POP   USING                                                    05575014
         EJECT                                                          05576014
                                                                        05577014
HC0010   DS    0H                                                       05578014
                                                                        05579014
         PUSH  USING                                                    05580014
                                                                        05581014
*-------------------------------------------------------------------*   05582014
*                                                                   *   05583014
*    'INCLUDE=/EXCLUDE=' Keyword Processor                          *   05584014
*    ON ENTRY, R1 --->  A(Operand)                                  *   05585014
*                       Fullword containing # characters left in    *   05586014
*                        the input record                           *   05587014
*                       A(ODT Entry)                                *   05588014
*    ON EXIT,  R15 =   0 - All OK, ODTE_RTT_Ptr set                 *   05589014
*                      4 - Error found                              *   05590014
*                                                                   *   05591014
*-------------------------------------------------------------------*   05592014
                                                                        05593014
         USING *,R15                                                    05594014
         SAVE  (14,12),T                                                05595014
         LR    R11,R15                  Load permanent base reg         05596014
         DROP  R15                      Free up temp base               05597014
         USING HC0010,R11               Assign permanent base           05598014
                                                                        05599014
*-------------------------------------------------------------------*   05600014
*                                                                   *   05601014
*    Get an area on the stack for our local storage                 *   05602014
*                                                                   *   05603014
*-------------------------------------------------------------------*   05604014
                                                                        05605014
         STACK PUSH,                    Get Stack area                 +05606014
               LEN=@HC_DynLen,           this long                     +05607014
               STACK=@_31Bit_Stack_Ptr   using this stack               05608014
                                                                        05609014
         LR    R2,R1                    Save its address                05610014
         LR    R0,R1                    A(Our storage)                  05611014
         LA    R1,@HC_DynLen             Its length                     05612014
         SLR   R14,R14                  Clear source address            05613014
         SLR   R15,R15                   and length                     05614014
         MVCL  R0,R14                   Clear our storage               05615014
         LR    R1,R2                    Restore A(Our storage)          05616014
                                                                        05617014
*-------------------------------------------------------------------*   05618014
*                                                                   *   05619014
*    Chain our save areas, and restore the important registers      *   05620014
*   that we have destroyed                                          *   05621014
*                                                                   *   05622014
*-------------------------------------------------------------------*   05623014
                                                                        05624014
         ST    R13,4(0,R1)              Chain                           05625014
         ST    R1,8(0,R13)               saveareas                      05626014
         LR    R13,R1                   Load dynam base                 05627014
         USING @HC_Dynam,R13            Assign a base                   05628014
         L     R15,@HC_Dynam+4          Get A(HSA)                      05629014
         LM    R0,R3,20(R15)            Restore callers registers       05630014
                                                                        05631014
*-------------------------------------------------------------------*   05632014
*                                                                   *   05633014
*    Load our parm registers:                                       *   05634014
*         R2 ---> Operand of INCLUDE=/EXCLUDE= keyword              *   05635014
*         R3    = Nbr chars left in the input record                *   05636014
*                                                                   *   05637014
*-------------------------------------------------------------------*   05638014
                                                                        05639014
         LM    R2,R3,0(R1)              Get parms                       05640014
                                                                        05641014
         L     R10,8(0,R1)              A(ODT Entry)                    05642014
                                                                        05643014
         USING ODT_Entry,R10            Tell the assembler              05644014
                                                                        05645014
         LR    R15,R2                   Back up to start of             05646014
         SH    R15,=HL2'8'               keyword                        05647014
         CLI   0(R15),C'I'              Include?                        05648014
         BNE   HC0020                   No, must be Exclude             05649014
         NI    @HC_Exclude,X'FF'-L'@HC_Exclude OFF 'EXCLUDE' FLAG       05650014
         B     HC0030                   and skip                        05651014
HC0020   DS    0H                                                       05652014
         OI    @HC_Exclude,L'@HC_Exclude Turn on Exclude flag           05653014
         B     HC0030                   and skip                        05654014
HC0030   DS    0H                                                       05655014
                                                                        05656014
                                                                        05657014
*-------------------------------------------------------------------*   05658014
*                                                                   *   05659014
*    CHECK THAT BOTH INCLUDE AND EXCLUDE HAVE NOT BEEN SPECIFIED.   *   05660014
*                                                                   *   05661014
*-------------------------------------------------------------------*   05662014
                                                                        05663014
         ICM   R15,B'1111',ODTE_RTT_Ptr A(RECORD TYPE TABLE)            05664014
         BZ    HC0050                None, skip                         05665014
         TM    @HC_Exclude,L'@HC_Exclude Is this Exclude                05666014
         BO    HC0040                 YES, SKIP                         05667014
         TM    RTT_Entry_Exclude-RecType_Tbl(R15),L'RTT_Entry_Exclude   05668014
         BO    HC8000                 Error, both INCLUDE= and EXCLUDE= 05669014
         B     HC0050                 AND CONTINUE                      05670014
HC0040   DS    0H                                                       05671014
         TM    RTT_Entry_Include-RecType_Tbl(R15),L'RTT_Entry_Include   05672014
         BO    HC8000                 Error, both INCLUDE= and EXCLUDE= 05673014
HC0050   DS    0H                                                       05674014
                                                                        05675014
*-------------------------------------------------------------------*   05676014
*                                                                   *   05677014
*    SET UP OUR TRANSLATE TABLE TO LOOK FOR NUMERICS AND SEPARATORS.*   05678014
*                                                                   *   05679014
*-------------------------------------------------------------------*   05680014
                                                                        05681014
         MVI   @HC_RecType,X'00'       Init Record type                 05682014
         XC    @HC_SubType,@HC_SubType   and sub-type                   05683014
         NI    @HC_Paren,X'FF'-L'@HC_Paren Off flag                     05684014
         NI    @HC_RecTyp,X'FF'-L'@HC_Rectyp Off flag                   05685014
         NI    @HC_SubType_Present,X'FF'-L'@HC_SubType_Present          05686014
                                                                        05687014
         MVI   @HC_TRTab,X'FE'        SET WHOLE TABLE TO                05688014
         MVC   @HC_TRTab+1(L'@HC_TRTab-1),@HC_TRTab INVALID VALUES      05689014
         MVI   @HC_TRTab+C'0',X'00'   NUMERICS ARE                      05690014
         MVC   @HC_TRTab+C'1'(9),@HC_TRTab+C'0' VALID                   05691014
         MVI   @HC_TRTab+C' ',X'01'   STOP ON A BLANK                   05692014
         MVI   @HC_TRTab+C',',X'01'        AND A COMMA                  05693014
         MVI   @HC_TRTab+C'(',X'01'        AND OPEN PARENS              05694014
         MVI   @HC_TRTab+C')',X'01'        AND CLOSE PARENS             05695014
         NI    @HC_RecTyp,X'FF'-L'@HC_RecTyp No rectype yet             05696014
         NI    @HC_Paren,X'FF'-L'@HC_Paren No parens either             05697014
         CLI   0(R2),C'('             PAREN AROUND WHOLE STRING?        05698014
         BNE   HC0060                 NO, SKIP                          05699014
         OI    @HC_Paren,L'@HC_Paren  Yes, set our flag                 05700014
         LA    R2,1(0,R2)             POINT PAST IT                     05701014
         SH    R3,=H'1'               AND REDUCE LENGTH                 05702014
HC0060   DS    0H                                                       05703014
                                                                        05704014
*-------------------------------------------------------------------*   05705014
*                                                                   *   05706014
*    FIND NEXT SEPARATOR.                                           *   05707014
*                                                                   *   05708014
*-------------------------------------------------------------------*   05709014
                                                                        05710014
         LR    R4,R2                 SAVE R2                            05711014
         LR    R5,R2                 AND AGAIN                          05712014
         SLR   R2,R2                 AND CLEAR FUNCTION BYTE            05713014
         SH    R3,=H'1'              EXECUTE LENGTH                     05714014
         LA    R1,1(R3,R4)           ALL DONE, POINT PAST LAST BYTE     05715014
         EX    R3,HC9998             TRANSLATE                          05716014
         LR    R0,R2                 SAVE FUNCTION BYTE                 05717014
         LR    R2,R5                 RESTORE A(START)                   05718014
         LR    R4,R1                 A(SEPARATOR)                       05719014
         SR    R4,R2                 R4 = LEN OF DATA                   05720014
                                                                        05721014
*-------------------------------------------------------------------*   05722014
*                                                                   *   05723014
*    AT THIS POINT, R2 = A(START OF DATA)                           *   05724014
*                   R3 = LENGTH OF DATA LEFT IN SYSIN RECORD        *   05725014
*                   R4 = LEN OF STRING (IE, R2+R4 = A(SEPARATOR)).  *   05726014
*                                                                   *   05727014
*-------------------------------------------------------------------*   05728014
                                                                        05729014
         CH    R0,=H'240'            ILLEGAL CHARACTER?                 05730014
         BNL   HC8000                YES, SKIP                          05731014
         LTR   R4,R4                 ANY DATA THERE?                    05732014
         BNP   HC8000                NONE, ERROR                        05733014
         CH    R4,=H'3'              YES, IS IT TOO LONG?               05734014
         BH    HC8000                YES, ERROR                         05735014
         LR    R15,R4                NO, GET LEN OF STRING              05736014
         SH    R15,=H'1'             EXECUTE LENGTH                     05737014
         EX    R15,HC9999            PACK IT                            05738014
         CVB   R1,@HC_DBLWD          AND BINARIZE IT                    05739014
         LA    R15,0(R2,R4)          POINT TO SEPARATOR                 05740014
         TM    @HC_RecTyp,L'@HC_RecTyp HAVE WE ALREADY GOT RECTYPE?     05741014
         BZ    HC0070                NO, SKIP                           05742014
         CLI   0(R15),C')'           YES, IS THIS END OF SUBTYPE?       05743014
         BNE   HC8000                NO, ERROR                          05744014
         LA    R4,1(0,R4)            YES, BUMP PAST IT                  05745014
         STH   R1,@HC_SubType        SAVE SUBTYPE                       05746014
         OI    @HC_SubType_Present,L'@HC_SubType_Present Set flag       05747014
         B     HC0080                AND SKIP                           05748014
HC0070   DS    0H                                                       05749014
         STC   R1,@HC_RecType        SAVE RECORD TYPE                   05750014
         CLI   0(R15),C'('           START OF SUBTYPE?                  05751014
         BNE   HC0080                NO, SKIP                           05752014
         OI    @HC_RecTyp,L'@HC_RecTyp YES, SAY WE HAVE RECORD TYPE     05753014
         LA    R2,1(R2,R4)           POINT PAST OPEN PAREN              05754014
         SR    R3,R4                 ADJUST LENGTH REMAINING            05755014
         SH    R3,=H'1'                PROPERLY                         05756014
         B     HC0060                AND KEEP GOING                     05757014
HC0080   DS    0H                                                       05758014
                                                                        05759014
*-------------------------------------------------------------------*   05760014
*                                                                   *   05761014
*    WHEN WE GET HERE, @HC_RecType CONTAINS THE RECORD TYPE,        *   05762014
*                      @HC_SubType CONTAINS THE SUBTYPE             *   05763014
*    AND WE GO TO FIND A FREE ENTRY IN THE RECTYPE TABLE            *   05764014
*                                                                   *   05765014
*-------------------------------------------------------------------*   05766014
                                                                        05767014
         LA    R1,10                 INITIAL NBR ENTRIES IN TBL         05768014
         ICM   R15,B'1111',ODTE_RTT_Ptr A(RECORD TYPE TABLE)            05769014
         BZ    HC0090                NONE, SKIP                         05770014
         LH    R1,RTT_Entry_Count-RecType_Tbl(R15) YES, GET NBR ENTRIES 05771014
         LA    R1,1(0,R1)            ADD ONE FOR OUR NEW ENTRY          05772014
         LA    R0,RTTE_Length        LENGTH OF ONE ENTRY                05773014
         MR    R0,R0                 GET LENGTH OF CURRENT ENTRIES      05774014
         LA    R1,RTT_Entry-RecType_Tbl(0,R1) ADD TBL HEADER LEN        05775014
         L     R0,RTT_Subp_Len-RecType_Tbl(R15) GET LEN                 05776014
         SLL   R0,8                  GET RID OF                         05777014
         SRL   R0,8                    SUBPOOL                          05778014
         CR    R0,R1                 IS THE TABLE BIG ENOUGH?           05779014
         BNL   HC0110                YES, GO INSERT ENTRY               05780014
         LH    R1,RTT_Entry_Count-RecType_Tbl(R15) NO, GET NBR ENTRIES  05781014
         SLL   R1,1                  DOUBLE THEM                        05782014
HC0090   DS    0H                                                       05783014
                                                                        05784014
*-------------------------------------------------------------------*   05785014
*                                                                   *   05786014
*    EITHER THE RECORD TYPE TABLE DOESN'T EXIST, OR IT IS TOO       *   05787014
*    SMALL TO HOLD OUR ENTRY, SO WE HAVE TO GO AND GET A NEW TABLE. *   05788014
*    R1 CONTAINS THE NUMBER OF ENTRIES THAT SHOULD BE IN THE TABLE. *   05789014
*                                                                   *   05790014
*-------------------------------------------------------------------*   05791014
                                                                        05792014
         LA    R0,RTTE_Length        LENGTH OF ONE ENTRY                05793014
         MR    R0,R0                 GET LENGTH OF ENTRIES              05794014
         LA    R0,RTT_Entry-RecType_Tbl(0,R1) ADD TBL HEADER LEN        05795014
         STORAGE OBTAIN,                Go get our storage             +05796014
               LENGTH=(0),               this long                     +05797014
               LOC=ANY                   anywhere                       05798014
         ST    R0,@HC_DBLWD          SAVE SUBPOOL AND LENGTH            05799014
         ST    R1,@HC_DBLWD+4        SAVE A(TABLE)                      05800014
                                                                        05801014
*-------------------------------------------------------------------*   05802014
*                                                                   *   05803014
*    Move the old table to the new, or just clear the table if      *   05804014
*   there is no old table                                           *   05805014
*                                                                   *   05806014
*-------------------------------------------------------------------*   05807014
                                                                        05808014
         LR    R0,R1                 A(New TABLE) IN R0                 05809014
         L     R1,@HC_DBLWD          LEN IN R1                          05810014
         SLR   R15,R15               Assume no old table                05811014
         ICM   R14,B'1111',ODTE_RTT_Ptr GET A(OLD TABLE)                05812014
         BZ    HC0095                NONE, SKIP                         05813014
         L     R15,RTT_Subp_Len-RecType_Tbl(R14) YES, GET CURR LEN      05814014
         SLL   R15,8                 CLEAR OUT                          05815014
         SRL   R15,8                  SUBPOOL                           05816014
HC0095   DS    0H                                                       05817014
         MVCL  R0,R14                MOVE OLD TABLE TO NEW              05818014
                                                                        05819014
         ICM   R1,B'1111',ODTE_RTT_Ptr  GET A(OLD TABLE)                05820014
         BZ    HC0100                   None, skip                      05821014
         L     R0,RTT_Subp_Len-RecType_Tbl(R1) GET ITS LENGTH           05822014
         STORAGE RELEASE,               Free our storage               +05823014
               ADDR=(1),                 starting here                 +05824014
               LENGTH=(0)                for this long                  05825014
HC0100   DS    0H                                                       05826014
         L     R15,@HC_DBLWD+4       A(NEW TABLE)                       05827014
         ST    R15,ODTE_RTT_Ptr      SAVE IT                            05828014
         MVC   RTT_Subp_Len-RecType_Tbl(L'RTT_Subp_Len,R15),@HC_DBLWD   05829014
         LH    R0,RTT_Entry_Count-RecType_Tbl(R15)                      05830014
         LTR   R0,R0                 Are we creating table?             05831014
         BNZ   HC0110                No, skip                           05832014
         TM    @HC_Exclude,L'@HC_Exclude Yes, are we EXCLUDEing?        05833014
         BO    HC0105                Yes, skip                          05834014
         OI    RTT_Entry_Include-RecType_Tbl(R15),L'RTT_Entry_Include   05835014
         B     HC0110                and skip                           05836014
HC0105   DS    0H                                                       05837014
         OI    RTT_Entry_Exclude-RecType_Tbl(R15),L'RTT_Entry_Exclude   05838014
HC0110   DS    0H                                                       05839014
                                                                        05840014
*-------------------------------------------------------------------*   05841014
*                                                                   *   05842014
*    AND NOW INSERT THE RECORD TYPE AND SUBTYPE IN THE TABLE.       *   05843014
*                                                                   *   05844014
*-------------------------------------------------------------------*   05845014
                                                                        05846014
         L     R15,ODTE_RTT_Ptr      A(RECORD TYPE TABLE)               05847014
         LH    R1,RTT_Entry_Count-RecType_Tbl(R15) GET NBR ENTRIES      05848014
         LA    R0,RTTE_Length        LENGTH OF ONE ENTRY                05849014
         MR    R0,R0                 GET LENGTH OF CURRENT ENTRIES      05850014
         LA    R1,RTT_Entry-RecType_Tbl(0,R1) ADD TBL HEADER LEN        05851014
         AR    R1,R15                A(FREE ENTRY)                      05852014
         MVC   RTTE_RecType-RTT_Entry(L'RTTE_RecType,R1),@HC_RecType    05853014
         NI    RTTE_SubType_Present-RTT_Entry(R1),X'FF'-L'RTTE_SubType_+05854014
               Present                                                  05855014
         TM    @HC_SubType_Present,L'@HC_SubType_Present SubType?       05856014
         BNO   HC0120                No, skip                           05857014
         MVC   RTTE_SubType-RTT_Entry(L'RTTE_SubType,R1),@HC_SubType    05858014
         OI    RTTE_SubType_Present-RTT_Entry(R1),L'RTTE_SubType_Presen+05859014
               t                                                        05860014
HC0120   DS    0H                                                       05861014
                                                                        05862014
         LH    R1,RTT_Entry_Count-RecType_Tbl(R15) GET NBR ENTRIES      05863014
         LA    R1,1(0,R1)            BUMP IT                            05864014
         STH   R1,RTT_Entry_Count-RecType_Tbl(R15) SAVE IT              05865014
                                                                        05866014
*-------------------------------------------------------------------*   05867014
*                                                                   *   05868014
*    SET UP TO FIND THE NEXT SUBENTRY.                              *   05869014
*                                                                   *   05870014
*-------------------------------------------------------------------*   05871014
                                                                        05872014
         AR    R2,R4                 A(SEPARATOR)                       05873014
         SR    R3,R4                 ADJUST LEN LEFT IN RECORD          05874014
         SLR   R15,R15               CLEAR RETURN CODE                  05875014
         TM    @HC_Paren,L'@HC_Paren IS THE WHOLE MESS IN PARENS?       05876014
         BZ    HC9000                NO, EXIT                           05877014
         NI    @HC_RecTyp,X'FF'-L'@HC_RecTyp YES, OFF FOUND FLAG        05878014
         MVI   @HC_RecType,X'00'     CLEAR RECORD TYPE                  05879014
         XC    @HC_SubType,@HC_SubType AND SUBTYPE                      05880014
         NI    @HC_SubType_Present,X'FF'-L'@HC_SubType_Present          05881014
         CLI   0(R2),C','            MORE RECORD TYPES?                 05882014
         BNE   HC0130                NO, SKIP                           05883014
         LA    R2,1(0,R2)            YES, BUMP PAST COMMA               05884014
         SH    R3,=H'1'              ADJUST LENGTH                      05885014
         B     HC0060                AND TRY AGAIN                      05886014
HC0130   DS    0H                                                       05887014
         CLI   0(R2),C')'            IS THIS THE END?                   05888014
         BNE   HC8000                NO, ERROR                          05889014
         LA    R2,1(0,R2)            YES, POINT PAST CLOSING PAREN      05890014
         SH    R3,=H'1'              ADJUST LENGTH REMAINING            05891014
         NI    @HC_Paren,X'FF'-L'@HC_Paren AND OFF PAREN FLAG           05892014
         SLR   R15,R15               CLEAR RETURN CODE                  05893014
         B     HC9000                AND EXIT                           05894014
HC8000   DS    0H                                                       05895014
         LA    R15,4                 SET RETURN CODE                    05896014
         B     HC9000                AND EXIT                           05897014
HC9000   DS    0H                                                       05898014
                                                                        05899014
*-------------------------------------------------------------------*   05900014
*                                                                   *   05901014
*    Free up our local storage ...                                  *   05902014
*                                                                   *   05903014
*-------------------------------------------------------------------*   05904014
                                                                        05905014
         L     R1,4(0,R13)              A(HSA)                          05906014
         L     R1,24(0,R1)              Get original R1                 05907014
         STM   R2,R3,0(R1)              Update parms                    05908014
                                                                        05909014
         LR    R3,R15                   Rescue the return code          05910014
                                                                        05911014
         LA    R1,@HC_Dynam             A(Local storage)                05912014
         L     R13,4(0,R13)             Rescue A(HSA)                   05913014
                                                                        05914014
         STACK POP,                     Free the stack area            +05915014
               ADDR=(R1),                starting here                 +05916014
               STACK=@_31Bit_Stack_Ptr   on this stack                  05917014
                                                                        05918014
*-------------------------------------------------------------------*   05919014
*                                                                   *   05920014
*    ... and return to caller                                       *   05921014
*                                                                   *   05922014
*-------------------------------------------------------------------*   05923014
                                                                        05924014
         LR    R15,R3                   Restore return code             05925014
         L     R14,12(0,R13)            Restore return address          05926014
         LM    R0,R12,20(R13)           Restore other registers         05927014
         BSM   0,R14                    and return                      05928014
                                                                        05929014
HC9998 TRT     0(0,R5),@HC_TRTab     FIND ENDING CHARACTER              05930014
HC9999 PACK    @HC_DBLWD,0(0,R2)     PACK RECORD TYPE/SUBTYPE           05931014
                                                                        05932014
         LTORG                                                          05933014
                                                                        05934014
@HC_Dynam       DSECT                 Dynamic area for J                05935014
@HC_Save        DS    18F              O/S Style save area              05936014
@HC_DBLWD       DS    D                Work area                        05937014
@HC_RecType     DS    X                Record Type                      05938014
@HC_SubType     DS    HL2              Record Type 30 Subtype           05939014
                DS    X                Flag Byte                        05940014
@HC_Paren       EQU   *-1,X'80'         1... ....  IN PARENTHESES       05941014
@HC_RecTyp      EQU   *-1,X'40'         .1.. ....  RECTYPE FOUND        05942014
@HC_Exclude     EQU   *-1,X'20'         ..1. ....  EXCLUDE SPECIFIED    05943014
@HC_SubType_Present EQU *-1,X'10'       ...1 ....  SubType found        05944014
@HC_TRTab       DS    XL256            Translate table                  05945014
                DS    0D               Alignment                        05946014
@HC_DynLen      EQU   *-@HC_Dynam     Length of storage required        05947014
                                                                        05948014
SMFSLCT  RSECT                                                          05949014
                                                                        05950014
         DROP  R10                    ODT Entry base                    05951014
         DROP  R11,R13                                                  05952014
         POP   USING                                                    05953014
         EJECT                                                          05954014
HD0010   DS    0H                                                       05955014
                                                                        05956014
         PUSH  USING                                                    05957014
                                                                        05958014
*-------------------------------------------------------------------*   05959014
*                                                                   *   05960014
*    'JOBNAME=' Keyword Processor                                   *   05961014
*    On Entry, R1 --->  A(Operand)                                  *   05962014
*                       Fullword containing # characters left in    *   05963014
*                        the input record                           *   05964014
*                       A(ODT Entry)                                *   05965014
*    On Exit,  R15 =   0 - All OK, ODTE_JNT_Ptr updated             *   05966014
*                      4 - Error found                              *   05967014
*                                                                   *   05968014
*-------------------------------------------------------------------*   05969014
                                                                        05970014
         USING *,R15                                                    05971014
         SAVE  (14,12),T                                                05972014
         LR    R11,R15                  Load permanent base reg         05973014
         DROP  R15                      Free up temp base               05974014
         USING HD0010,R11               Assign permanent base           05975014
                                                                        05976014
*-------------------------------------------------------------------*   05977014
*                                                                   *   05978014
*    Get an area on the stack for our local storage                 *   05979014
*                                                                   *   05980014
*-------------------------------------------------------------------*   05981014
                                                                        05982014
         STACK PUSH,                    Get Stack area                 +05983014
               LEN=@HD_DynLen,           this long                     +05984014
               STACK=@_31Bit_Stack_Ptr   using this stack               05985014
                                                                        05986014
         LR    R2,R1                    Save its address                05987014
         LR    R0,R1                    A(Our storage)                  05988014
         LA    R1,@HD_DynLen             Its length                     05989014
         SLR   R14,R14                  Clear source address            05990014
         SLR   R15,R15                   and length                     05991014
         MVCL  R0,R14                   Clear our storage               05992014
         LR    R1,R2                    Restore A(Our storage)          05993014
                                                                        05994014
*-------------------------------------------------------------------*   05995014
*                                                                   *   05996014
*    Chain our save areas, and restore the important registers      *   05997014
*   that we have destroyed                                          *   05998014
*                                                                   *   05999014
*-------------------------------------------------------------------*   06000014
                                                                        06001014
         ST    R13,4(0,R1)              Chain                           06002014
         ST    R1,8(0,R13)               saveareas                      06003014
         LR    R13,R1                   Load dynam base                 06004014
         USING @HD_Dynam,R13            Assign a base                   06005014
         L     R15,@HD_Dynam+4          Get A(HSA)                      06006014
         LM    R0,R3,20(R15)            Restore callers registers       06007014
                                                                        06008014
*-------------------------------------------------------------------*   06009014
*                                                                   *   06010014
*    Load our parm registers:                                       *   06011014
*         R2 ---> Operand of keyword                                *   06012014
*         R3    = Nbr chars left in the input record                *   06013014
*                                                                   *   06014014
*-------------------------------------------------------------------*   06015014
                                                                        06016014
         LM    R2,R3,0(R1)              Get parms                       06017014
                                                                        06018014
         L     R10,8(0,R1)              A(ODT Entry)                    06019014
                                                                        06020014
         USING ODT_Entry,R10            Tell the assembler              06021014
                                                                        06022014
                                                                        06023014
*-------------------------------------------------------------------*   06024014
*                                                                   *   06025014
*    SET UP OUR TRANSLATE TABLE TO STOP ON SEPARATORS.              *   06026014
*                                                                   *   06027014
*-------------------------------------------------------------------*   06028014
                                                                        06029014
         NI    @HD_Paren,X'FF'-L'@HD_Paren OFF PARENTHESES FLAG         06030014
         MVI   @HD_TRTab,X'00'        ANYTHING GOES IN A JOB NAME       06031014
         MVC   @HD_TRTab+1(L'@HD_TRTab-1),@HD_TRTab                     06032014
         MVI   @HD_TRTab+C' ',X'01'   STOP ON A BLANK                   06033014
         MVI   @HD_TRTab+C',',X'01'        AND A COMMA                  06034014
         CLI   0(R2),C'('             PAREN AROUND WHOLE STRING?        06035014
         BNE   HD0020                 NO, SKIP                          06036014
         OI    @HD_Paren,L'@HD_Paren  YES, SAY SO                       06037014
         LA    R2,1(0,R2)             POINT PAST IT                     06038014
         SH    R3,=H'1'               AND REDUCE LENGTH                 06039014
         MVI   @HD_TRTab+C')',X'01'   STOP ON CLOSE PAREN TOO           06040014
HD0020   DS    0H                                                       06041014
                                                                        06042014
*-------------------------------------------------------------------*   06043014
*                                                                   *   06044014
*    FIND NEXT SEPARATOR.                                           *   06045014
*                                                                   *   06046014
*-------------------------------------------------------------------*   06047014
                                                                        06048014
         LR    R4,R2                 SAVE R2                            06049014
         LR    R5,R2                 AND AGAIN                          06050014
         SLR   R2,R2                 AND CLEAR FUNCTION BYTE            06051014
         SH    R3,=H'1'              EXECUTE LENGTH                     06052014
         LA    R1,1(R3,R4)           ALL DONE, POINT PAST LAST BYTE     06053014
         EX    R3,HD9999             TRANSLATE                          06054014
         LR    R0,R2                 SAVE FUNCTION BYTE                 06055014
         LR    R2,R5                 RESTORE A(START)                   06056014
         LR    R4,R1                 A(SEPARATOR)                       06057014
         SR    R4,R2                 R4 = LEN OF DATA                   06058014
                                                                        06059014
*-------------------------------------------------------------------*   06060014
*                                                                   *   06061014
*    AT THIS POINT, R2 = A(START OF DATA)                           *   06062014
*                   R3 = LENGTH OF DATA LEFT IN SYSIN RECORD        *   06063014
*                   R4 = LEN OF STRING (IE, R2+R4 = A(SEPARATOR)).  *   06064014
*                                                                   *   06065014
*-------------------------------------------------------------------*   06066014
                                                                        06067014
         CH    R0,=H'240'            ILLEGAL CHARACTER?                 06068014
         BNL   HD8000                YES, SKIP                          06069014
         LTR   R4,R4                 ANY DATA THERE?                    06070014
         BNP   HD8000                NONE, ERROR                        06071014
         CH    R4,=H'8'              YES, IS IT TOO LONG?               06072014
         BH    HD8000                YES, ERROR                         06073014
                                                                        06074014
*-------------------------------------------------------------------*   06075014
*                                                                   *   06076014
*    WHEN WE GET HERE, R2 POINTS TO START OF JOBNAME,               *   06077014
*                      R4 CONTAINS ITS LENGTH                       *   06078014
*    AND WE GO TO FIND A FREE ENTRY IN THE JOBNAME TABLE            *   06079014
*                                                                   *   06080014
*-------------------------------------------------------------------*   06081014
                                                                        06082014
         LA    R1,10                 INITIAL NBR ENTRIES IN TBL         06083014
         ICM   R15,B'1111',ODTE_JNT_Ptr A(JOB NAME TABLE)               06084014
         BZ    HD0030                None, SKIP                         06085014
         LH    R1,JNT_Entry_Count-JobName_Tbl(R15) YES, GET NBR ENTRIES 06086014
         LA    R1,1(0,R1)            ADD ONE FOR OUR NEW ENTRY          06087014
         LA    R0,JNTE_Length        LENGTH OF ONE ENTRY                06088014
         MR    R0,R0                 GET LENGTH OF CURRENT ENTRIES      06089014
         LA    R1,JNT_Entry-JobName_Tbl(0,R1) ADD TBL HEADER LEN        06090014
         L     R0,JNT_Subp_Len-JobName_Tbl(R15) GET CURR LEN            06091014
         SLL   R0,8                  GET RID OF                         06092014
         SRL   R0,8                    SUBPOOL                          06093014
         CR    R0,R1                 IS THE TABLE BIG ENOUGH?           06094014
         BNL   HD0050                YES, GO INSERT ENTRY               06095014
         LH    R1,JNT_Entry_Count-JobName_Tbl(R15) NO, NBR ENTRIES      06096014
         SLL   R1,1                  DOUBLE THEM                        06097014
HD0030   DS    0H                                                       06098014
                                                                        06099014
*-------------------------------------------------------------------*   06100014
*                                                                   *   06101014
*    EITHER THE JOB NAME TABLE DOESN'T EXIST, OR IT IS TOO          *   06102014
*    SMALL TO HOLD OUR ENTRY, SO WE HAVE TO GO AND GET A NEW TABLE. *   06103014
*    R1 CONTAINS THE NUMBER OF ENTRIES THAT SHOULD BE IN THE TABLE. *   06104014
*                                                                   *   06105014
*-------------------------------------------------------------------*   06106014
                                                                        06107014
         LA    R0,JNTE_Length        LENGTH OF ONE ENTRY                06108014
         MR    R0,R0                 GET LENGTH OF ENTRIES              06109014
         LA    R0,JNT_Entry-JobName_Tbl+7(0,R1) ADD TBL HEADER LEN      06110014
         SRL   R0,3                  Make it a doubleword               06111014
         SLL   R0,3                   multiple                          06112014
         STORAGE OBTAIN,                Go get our storage             +06113014
               LENGTH=(0),               this long                     +06114014
               LOC=ANY                   anywhere                       06115014
         ST    R0,@HD_JNT_Subp_Len   SAVE SUBPOOL AND LENGTH            06116014
         ST    R1,@HD_JNT_Pointer    SAVE A(TABLE)                      06117014
         LR    R0,R1                 A(TABLE) IN R0                     06118014
         L     R1,@HD_JNT_Subp_Len   LEN IN R1                          06119014
         SLR   R14,R14               NOTHING IN R14                     06120014
         SLR   R15,R15               AND IN 15                          06121014
         MVCL  R0,R14                CLEAR THE TABLE                    06122014
         ICM   R14,B'1111',ODTE_JNT_Ptr GET A(OLD TABLE)                06123014
         BZ    HD0040                None, SKIP                         06124014
                                                                        06125014
*-------------------------------------------------------------------*   06126014
*                                                                   *   06127014
*    THERE WAS AN OLD TABLE, SO WE HAVE TO COPY IT TO THE NEW TABLE *   06128014
*    AREA, AND THEN FREE UP THE OLD ONE.                            *   06129014
*                                                                   *   06130014
*-------------------------------------------------------------------*   06131014
                                                                        06132014
         L     R15,JNT_Subp_Len-JobName_Tbl(R14) YES, GET CURR LENGTH   06133014
         SLL   R15,8                 CLEAR OUT                          06134014
         SRL   R15,8                  SUBPOOL                           06135014
         L     R0,@HD_JNT_Pointer    GET A(NEW TABLE)                   06136014
         LR    R1,R15                SAME LENGTH                        06137014
         MVCL  R0,R14                MOVE OLD TABLE TO NEW              06138014
         L     R1,ODTE_JNT_Ptr       GET A(OLD TABLE)                   06139014
         L     R0,JNT_Subp_Len-JobName_Tbl(R1) GET ITS LENGTH           06140014
         STORAGE RELEASE,               Free our storage               +06141014
               ADDR=(1),                 starting here                 +06142014
               LENGTH=(0)                for this long                  06143014
HD0040   DS    0H                                                       06144014
         L     R15,@HD_JNT_Pointer   A(NEW TABLE)                       06145014
         ST    R15,ODTE_JNT_Ptr      SAVE IT                            06146014
         MVC   JNT_Subp_Len-JobName_Tbl(L'JNT_Subp_Len,R15),@HD_JNT_Sub+06147014
               p_Len                                                    06148014
HD0050   DS    0H                                                       06149014
                                                                        06150014
*-------------------------------------------------------------------*   06151014
*                                                                   *   06152014
*    AND NOW INSERT THE JOB NAME IN THE TABLE.                      *   06153014
*                                                                   *   06154014
*-------------------------------------------------------------------*   06155014
                                                                        06156014
         L     R15,ODTE_JNT_Ptr      A(JOB NAME TABLE)                  06157014
         LH    R1,JNT_Entry_Count-JobName_Tbl(R15) GET NBR ENTRIES      06158014
         LA    R0,JNTE_Length        LENGTH OF ONE ENTRY                06159014
         MR    R0,R0                 GET LENGTH OF CURRENT ENTRIES      06160014
         LA    R1,JNT_Entry-JobName_Tbl(0,R1) ADD TBL HEADER LEN        06161014
         AR    R1,R15                A(FREE ENTRY)                      06162014
                                                                        06163014
*-------------------------------------------------------------------*   06164014
*                                                                   *   06165014
*    WE ARE NOW POINTING AT A FREE ENTRY IN THE JOBNAME TABLE -     *   06166014
*   IF THIS IS A PREFIX (IE, LAST CHAR IS AN '*'), CALCULATE THE    *   06167014
*   EXECUTE LEN AND SAVE IT; OTHERWISE, USE 7 AS THE EXECUTE LEN.   *   06168014
*                                                                   *   06169014
*-------------------------------------------------------------------*   06170014
                                                                        06171014
         LA    R14,L'JNTE_JobName-1  DEFAULT EXECUTE LENGTH             06172014
         LA    R15,0(R2,R4)          POINT PAST JOBNAME                 06173014
         SH    R15,=H'1'             BACK UP A CHARACTER                06174014
         CLI   0(R15),C'*'           IS THIS JOBNAME A PREFIX?          06175014
         BNE   HD0060                NO, SKIP                           06176014
         SH    R4,=H'1'              YES, DON'T INCLUDE THE ASTERISK    06177014
         LR    R14,R4                GET                                06178014
         SH    R14,=H'1'              EXECUTE LENGTH                    06179014
HD0060   DS    0H                                                       06180014
         STC   R14,JNTE_JobName_Len-JNT_Entry(R1) SAVE IT               06181014
                                                                        06182014
*-------------------------------------------------------------------*   06183014
*                                                                   *   06184014
*    MOVE THE JOBNAME/PREFIX TO THE TABLE.                          *   06185014
*                                                                   *   06186014
*-------------------------------------------------------------------*   06187014
                                                                        06188014
         LA    R0,JNTE_JobName-JNT_Entry(R1) A(JOBNAME FIELD)           06189014
         LA    R1,L'JNTE_JobName     LEN OF JOBNAME FIELD               06190014
         LR    R14,R2                A(JOBNAME)                         06191014
         LA    R15,C' '              PAD CHARACTER                      06192014
         SLL   R15,24                IN THE PROPER PLACE                06193014
         OR    R15,R4                INSERT LENGTH                      06194014
         MVCL  R0,R14                MOVE JOBNAME                       06195014
         L     R15,ODTE_JNT_Ptr      A(JOB NAME TABLE)                  06196014
         LH    R1,JNT_Entry_Count-JobName_Tbl(R15) GET NBR ENTRIES      06197014
         LA    R1,1(0,R1)            BUMP IT                            06198014
         STH   R1,JNT_Entry_Count-JobName_Tbl(R15) SAVE IT              06199014
                                                                        06200014
*-------------------------------------------------------------------*   06201014
*                                                                   *   06202014
*    SET UP TO FIND THE NEXT SUBENTRY.                              *   06203014
*                                                                   *   06204014
*-------------------------------------------------------------------*   06205014
                                                                        06206014
         AR    R2,R4                 A(SEPARATOR)                       06207014
         CLI   0(R2),C'*'            WAS THIS A PREFIX?                 06208014
         BNE   HD0070                NO, SKIP                           06209014
         LA    R2,1(0,R2)            YES, SKIP PAST THE ASTERISK        06210014
HD0070   DS    0H                                                       06211014
         SR    R3,R4                 ADJUST LEN LEFT IN RECORD          06212014
         SLR   R15,R15               CLEAR RETURN CODE                  06213014
         TM    @HD_Paren,L'@HD_Paren DO WE HAVE PARENS AROUND NAMES?    06214014
         BZ    HD9000                NO, EXIT                           06215014
         CLI   0(R2),C','            YES, MORE JOBNAMES?                06216014
         BNE   HD0080                NO, ERROR                          06217014
         LA    R2,1(0,R2)            YES, BUMP PAST COMMA               06218014
         SH    R3,=H'1'              ADJUST LENGTH                      06219014
         B     HD0020                AND TRY AGAIN                      06220014
HD0080   DS    0H                                                       06221014
         CLI   0(R2),C')'            CLOSING PAREN?                     06222014
         BNE   HD8000                NO, ERROR                          06223014
         LA    R2,1(0,R2)            YES, POINT PAST IT                 06224014
         SH    R3,=H'1'              ADJUST LENGTH REMAINING            06225014
         NI    @HD_Paren,X'FF'-L'@HD_Paren OFF PAREN FLAG               06226014
         B     HD9000                AND EXIT                           06227014
HD8000   DS    0H                                                       06228014
         LA    R15,4                 SET RETURN CODE                    06229014
         B     HD9000                AND EXIT                           06230014
HD9000   DS    0H                                                       06231014
                                                                        06232014
*-------------------------------------------------------------------*   06233014
*                                                                   *   06234014
*    Free up our local storage ...                                  *   06235014
*                                                                   *   06236014
*-------------------------------------------------------------------*   06237014
                                                                        06238014
         L     R1,4(0,R13)              A(HSA)                          06239014
         L     R1,24(0,R1)              Get original R1                 06240014
         STM   R2,R3,0(R1)              Update parms                    06241014
                                                                        06242014
         LR    R3,R15                   Rescue the return code          06243014
                                                                        06244014
         LA    R1,@HD_Dynam             A(Local storage)                06245014
         L     R13,4(0,R13)             Rescue A(HSA)                   06246014
                                                                        06247014
         STACK POP,                     Free the stack area            +06248014
               ADDR=(R1),                starting here                 +06249014
               STACK=@_31Bit_Stack_Ptr   on this stack                  06250014
                                                                        06251014
*-------------------------------------------------------------------*   06252014
*                                                                   *   06253014
*    ... and return to caller                                       *   06254014
*                                                                   *   06255014
*-------------------------------------------------------------------*   06256014
                                                                        06257014
         LR    R15,R3                   Restore return code             06258014
         L     R14,12(0,R13)            Restore return address          06259014
         LM    R0,R12,20(R13)           Restore other registers         06260014
         BSM   0,R14                    and return                      06261014
                                                                        06262014
HD9999 TRT     0(0,R5),@HD_TRTab     FIND ENDING CHARACTER              06263014
                                                                        06264014
         LTORG                                                          06265014
                                                                        06266014
@HD_Dynam       DSECT                 Dynamic area for HD               06267014
@HD_Save        DS    18F              O/S Style save area              06268014
@HD_JNT_Pointer DS    AL4              A(JobName Table)                 06269014
@HD_JNT_Subp_Len DS   FL4              Subpool, length of JNT           06270014
@HD_TRTab       DS    XL256            Translate table                  06271014
                DS    X                Flag byte                        06272014
@HD_Paren       EQU   *-1,X'80'         1... .... In parentheses        06273014
                DS    0D               Alignment                        06274014
@HD_DynLen      EQU   *-@HD_Dynam     Length of storage required        06275014
                                                                        06276014
SMFSLCT  RSECT                                                          06277014
                                                                        06278014
         DROP  R10                                                      06279014
         DROP  R11,R13                                                  06280014
         POP   USING                                                    06281014
         EJECT                                                          06282014
HE0010   DS    0H                                                       06283014
                                                                        06284014
         PUSH  USING                                                    06285014
                                                                        06286014
*-------------------------------------------------------------------*   06287014
*                                                                   *   06288014
*    'DATA=' Keyword Processor                                      *   06289014
*    On Entry, R1 --->  A(Operand)                                  *   06290014
*                       Fullword containing # characters left in    *   06291014
*                        the input record                           *   06292014
*                       A(ODT Entry)                                *   06293014
*    On Exit,  R15 =   0 - All OK, ODTE_DT_Ptr updated              *   06294014
*                      4 - Error found                              *   06295014
*                                                                   *   06296014
*-------------------------------------------------------------------*   06297014
                                                                        06298014
         USING *,R15                                                    06299014
         SAVE  (14,12),T                                                06300014
         LR    R11,R15                  Load permanent base reg         06301014
         DROP  R15                      Free up temp base               06302014
         USING HE0010,R11               Assign permanent base           06303014
                                                                        06304014
*-------------------------------------------------------------------*   06305014
*                                                                   *   06306014
*    Get an area on the stack for our local storage                 *   06307014
*                                                                   *   06308014
*-------------------------------------------------------------------*   06309014
                                                                        06310014
         STACK PUSH,                    Get Stack area                 +06311014
               LEN=@HE_DynLen,           this long                     +06312014
               STACK=@_31Bit_Stack_Ptr   using this stack               06313014
                                                                        06314014
         LR    R2,R1                    Save its address                06315014
         LR    R0,R1                    A(Our storage)                  06316014
         LA    R1,@HE_DynLen             Its length                     06317014
         SLR   R14,R14                  Clear source address            06318014
         SLR   R15,R15                   and length                     06319014
         MVCL  R0,R14                   Clear our storage               06320014
         LR    R1,R2                    Restore A(Our storage)          06321014
                                                                        06322014
*-------------------------------------------------------------------*   06323014
*                                                                   *   06324014
*    Chain our save areas, and restore the important registers      *   06325014
*   that we have destroyed                                          *   06326014
*                                                                   *   06327014
*-------------------------------------------------------------------*   06328014
                                                                        06329014
         ST    R13,4(0,R1)              Chain                           06330014
         ST    R1,8(0,R13)               saveareas                      06331014
         LR    R13,R1                   Load dynam base                 06332014
         USING @HE_Dynam,R13            Assign a base                   06333014
         L     R15,@HE_Dynam+4          Get A(HSA)                      06334014
         LM    R0,R3,20(R15)            Restore callers registers       06335014
                                                                        06336014
*-------------------------------------------------------------------*   06337014
*                                                                   *   06338014
*    Load our parm registers:                                       *   06339014
*         R2 ---> Operand of keyword                                *   06340014
*         R3    = Nbr chars left in the input record                *   06341014
*                                                                   *   06342014
*-------------------------------------------------------------------*   06343014
                                                                        06344014
         LM    R2,R3,0(R1)              Get parms                       06345014
                                                                        06346014
         L     R10,8(0,R1)              A(ODT Entry)                    06347014
                                                                        06348014
         USING ODT_Entry,R10            Tell the assembler              06349014
                                                                        06350014
*-------------------------------------------------------------------*   06351014
*                                                                   *   06352014
*    Set up our translate table.                                    *   06353014
*                                                                   *   06354014
*-------------------------------------------------------------------*   06355014
                                                                        06356014
         NI    @HE_Quote,X'FF'-L'@HE_Quote Off Quote flag               06357014
         NI    @HE_Hex,X'FF'-L'@HE_Hex    and hex flag                  06358014
         MVC   @HE_TRTab,HE_EBCDIC_TRTab MOVE EBCDIC TRAN TABLE         06359014
                                                                        06360014
         CH    R3,=H'1'               MORE THAN 1 CHARACTER LEFT?       06361014
         BNH   HE0070                 NO, NOT A QUOTED STRING           06362014
         CLI   0(R2),C''''            YES, START WITH A QUOTE?          06363014
         BE    HE0020                 YES, SKIP                         06364014
         CLI   0(R2),C'"'             NO, DOUBLE QUOTE?                 06365014
         BNE   HE0030                 NO, SKIP                          06366014
HE0020   DS    0H                                                       06367014
         SH    R2,=H'1'               BACK UP A BYTE                    06368014
         AH    R3,=H'1'               SET UP LENGTH                     06369014
         B     HE0040                 AND SKIP                          06370014
HE0030   DS    0H                                                       06371014
         CLI   1(R2),C''''            YES, SINGLE QUOTE?                06372014
         BE    HE0040                 YES, SKIP                         06373014
         CLI   1(R2),C'"'             NO, DOUBLE QUOTE?                 06374014
         BNE   HE0070                 NO, NO QUOTED STRING              06375014
HE0040   DS    0H                                                       06376014
         OI    @HE_Quote,L'@HE_Quote    SAY WE'RE DOING A QUOTED STRING 06377014
         CLI   0(R2),C'C'             CHARACTER STRING?                 06378014
         BE    HE0050                 YES, SKIP                         06379014
         CLI   0(R2),C'='             QUOTES WITH NO C OR H?            06380014
         BE    HE0050                 YES, ASSUME CHARACTER DATA        06381014
         CLI   0(R2),C'X'             NO, HEX STRING?                   06382014
         BNE   HE8000                 NO, ERROR                         06383014
         OI    @HE_Hex,L'@HE_Hex      YES, SAY SO                       06384014
         MVC   @HE_TRTab,HE_Hex_TRTab AND MOVE HEX CHARS TO TRANS TBL   06385014
         B     HE0060                 SKIP                              06386014
HE0050   DS    0H                                                       06387014
         MVI   @HE_TRTab,X'00'        ANYTHING GOES                     06388014
         MVC   @HE_TRTab+1(L'@HE_TRTab-1),@HE_TRTab BETWEEN QUOTES      06389014
HE0060   DS    0H                                                       06390014
         SLR   R0,R0                  CLEAR WORK REGISTER               06391014
         IC    R0,1(0,R2)             GET SEPARATOR CHARACTER           06392014
         LA    R15,@HE_TRTab          A(TRANSLATE TABLE)                06393014
         AR    R15,R0                 A(CHARACTER IN TRAN TBL           06394014
         MVI   0(R15),X'01'           STOP TRANSLATION THERE            06395014
         LA    R2,2(0,R2)             POINT PAST FIRST SEPARATOR        06396014
         SH    R3,=H'2'               AND ADJUST LENGTH                 06397014
         BNP   HE8000                 NOTHING LEFT, ERROR               06398014
HE0070   DS    0H                                                       06399014
         LR    R4,R2                 SAVE R2                            06400014
         LR    R5,R2                 AND AGAIN                          06401014
         SLR   R2,R2                 AND CLEAR FUNCTION BYTE            06402014
         SH    R3,=H'1'              EXECUTE LENGTH                     06403014
         LA    R1,1(R3,R4)           ALL DONE, POINT PAST LAST BYTE     06404014
         EX    R3,HE9997             TRANSLATE                          06405014
         LR    R0,R2                 SAVE FUNCTION BYTE                 06406014
         LR    R2,R1                 A(LAST CHARACTER)                  06407014
         SR    R1,R4                 GET LENGTH OF DATA                 06408014
         SR    R3,R1                 ADJUST LENGTH REMAINING            06409014
         AH    R3,=H'1'               PROPERLY                          06410014
         CH    R0,=H'240'            ILLEGAL CHARACTER?                 06411014
         BNL   HE8000                YES, SKIP                          06412014
         LR    R4,R2                 NO, GET STARTING ADDRESS           06413014
         SR    R4,R5                 GET LEN OF DATA                    06414014
         TM    @HE_Quote,L'@HE_Quote ARE WE DOING A QUOTED STRING?      06415014
         BNO   HE0110                NO, SKIP                           06416014
         CH    R0,=H'1'              YES, DID WE GET A FINAL SEP?       06417014
         BNE   HE8000                NO, ERROR                          06418014
         LA    R2,1(0,R2)            YES, POINT PAST IT                 06419014
         SH    R3,=H'1'              AND ADJUST LENGTH REMAINING        06420014
         TM    @HE_Hex,L'@HE_Hex     ARE WE DOING HEX?                  06421014
         BZ    HE0110                NO, SKIP                           06422014
         STC   R4,@HE_DBLWD          YES, SAVE LENGTH                   06423014
         TM    @HE_DBLWD,X'01'       IS LENGTH AN EVEN NUMBER           06424014
         BO    HE8000                NO, ERROR                          06425014
         SRL   R4,1                  GET LENGTH OF CONVERTED HEX        06426014
         LR    R15,R5                A(START OF SOURCE)                 06427014
         LR    R14,R5                A(START OF TARGET DATA             06428014
HE0080   DS    0H                                                       06429014
         LA    R0,X'FF'              WHEN R0 GOES NEGATIVE, WE HAVE     06430014
         SLL   R0,16                   GOT ONE PACKED BYTE              06431014
HE0090   DS    0H                                                       06432014
         CR    R15,R2                HAVE WE GONE FAR ENOUGH?           06433014
         BNL   HE0110                YES, SKIP                          06434014
         IC    R1,0(0,R15)           GET BYTE OF SOURCE DATA            06435014
         CLI   0(R15),C'0'           IS IT X'C1' - X'C6'?               06436014
         BNL   HE0100                NO, OK                             06437014
         LA    R1,9(0,R1)            YES, CONVERT TO X'CA' - X'CF'      06438014
HE0100   DS    0H                                                       06439014
         SLL   R1,28                 GET LOW NIBBLE AT TOP OF R1        06440014
         SLDL  R0,4                  AND INTO R0                        06441014
         LA    R15,1(0,R15)          BUMP SOURCE PTR                    06442014
         LTR   R0,R0                 NO, HAVE WE DONE 2 SOURCE BYTES?   06443014
         BNL   HE0090                NOT YET, CONTINUE                  06444014
         STC   R0,0(0,R14)           YES, SAVE HEX BYTE                 06445014
         LA    R14,1(0,R14)          BUMP DEST PTR                      06446014
         B     HE0080                AND CONTINUE                       06447014
HE0110   DS    0H                                                       06448014
                                                                        06449014
*-------------------------------------------------------------------*   06450014
*                                                                   *   06451014
*    WHEN WE GET HERE, R2 = A(NEXT CHARACTER AFTER DATA)            *   06452014
*                      R4 = LENGTH OF DATA,                         *   06453014
*                      R5 = A(START OF DATA)                        *   06454014
*    IF THE DATA WAS IN HEX (ENTERED AS X'123456'), IT HAS BEEN     *   06455014
*   CONVERTED, AND R4 REFLECTS THE NEW LENGTH.                      *   06456014
*                                                                   *   06457014
*-------------------------------------------------------------------*   06458014
                                                                        06459014
         ICM   R15,B'1111',ODTE_DT_Ptr A(DATA TABLE)                    06460014
         BZ    HE0140                None, GO GET ONE                   06461014
         LH    R14,DT_Entry_Count-Data_Tbl(R15) YES, GET NBR ENTRIES    06462014
         LA    R15,DT_Entry-Data_Tbl(R15) AND POINT TO THE FIRST ONE    06463014
         LTR   R14,R14               ARE THERE ANY ENTRIES?             06464014
         BZ    HE0130                NO, SKIP                           06465014
HE0120   DS    0H                                                       06466014
         SLR   R1,R1                 CLEAR WORK REGISTER                06467014
         IC    R1,DTE_Length-DT_Entry(R15) GET LEN OF DATA              06468014
         LA    R15,DTE_Data-DT_Entry+1(R1,R15) A(NEXT ENTRY             06469014
         BCT   R14,HE0120            AND GO TILL THE END                06470014
HE0130   DS    0H                                                       06471014
         LA    R14,DTE_Data-DT_Entry(R4,R15) A(END OF OUR NEW ENT)      06472014
         S     R14,ODTE_DT_Ptr       LENGTH OF UPDATED TABLE            06473014
         L     R1,ODTE_DT_Ptr        A(TABLE)                           06474014
         L     R1,DT_Subp_Len-Data_Tbl(R1) GET CURRENT LENGTH           06475014
         SLL   R1,8                  GET RID OF                         06476014
         SRL   R1,8                    SUBPOOL                          06477014
         CR    R1,R14                WILL OUR ENTRY FIT?                06478014
         BNL   HE0170                YES, GO DO IT                      06479014
HE0140   DS    0H                                                       06480014
         LA    R0,128                DEFAULT SUBPOOL AND LENGTH         06481014
         ICM   R1,B'1111',ODTE_DT_Ptr GET A(CURRENT TABLE)              06482014
         BZ    HE0150                None, SKIP                         06483014
         L     R1,DT_Subp_Len-Data_Tbl(R1) YES, GET SUBPOOL, LEN        06484014
         SLDL  R0,8                  PUT SUBPOOL IN R0                  06485014
         SLL   R1,1                  DOUBLE CURRENT LENGTH              06486014
         SLDL  R0,24                 AND PUT IT IN R0 TOO               06487014
HE0150   DS    0H                                                       06488014
         STORAGE OBTAIN,                Go get our storage             +06489014
               LENGTH=(0),               this long                     +06490014
               LOC=ANY                   anywhere                       06491014
         ST    R0,@HE_DT_Subp_Len    SAVE SUBPOOL, LENGTH               06492014
         ST    R1,@HE_DT_Pointer     AND A(NEW TABLE)                   06493014
         LR    R0,R1                 A(OUR TABLE)                       06494014
         L     R1,@HE_DT_Subp_Len    SUBPOOL, LENGTH OF IT              06495014
         SLL   R1,8                  GET RID OF                         06496014
         SRL   R1,8                    SUBPOOL                          06497014
         SLR   R14,R14               NO SENDING ADDRESS                 06498014
         SLR   R15,R15                 OR LENGTH                        06499014
         MVCL  R0,R14                CLEAR OUR TABLE                    06500014
         ICM   R14,B'1111',ODTE_DT_Ptr GET A(OLD TABLE)                 06501014
         BZ    HE0160                None, SKIP                         06502014
         L     R15,DT_Subp_Len-Data_Tbl(R14) YES, GET SUBPOOL, LENGTH   06503014
         SLL   R15,8                 CLEAR SUBPOOL SO THAT              06504014
         SRL   R15,8                   PAD CHAR IS X'00'                06505014
         L     R0,@HE_DT_Pointer     A(NEW TABLE AREA)                  06506014
         L     R1,@HE_DT_Subp_Len    LENGTH OF NEW TABLE                06507014
         MVCL  R0,R14                MOVE OLD TABLE TO NEW              06508014
         L     R1,ODTE_DT_Ptr        A(OLD DATA TABLE)                  06509014
         L     R0,DT_Subp_Len-Data_Tbl(R1) SUBPOOL, LENGTH OF TBL       06510014
         STORAGE RELEASE,               Free our storage               +06511014
               ADDR=(1),                 starting here                 +06512014
               LENGTH=(0)                for this long                  06513014
HE0160   DS    0H                                                       06514014
         L     R15,@HE_DT_Pointer    A(NEW TABLE AREA)                  06515014
         ST    R15,ODTE_DT_Ptr       SAVE IT                            06516014
         L     R0,@HE_DT_Subp_Len    SUBPOOL, LENGTH                    06517014
         ST    R0,DT_Subp_Len-Data_Tbl(R15) AND SAVE IN TABLE           06518014
         B     HE0110                and try again                      06519014
HE0170   DS    0H                                                       06520014
                                                                        06521014
*-------------------------------------------------------------------*   06522014
*                                                                   *   06523014
*    AT THIS POINT, WE ARE READY TO INSERT THE ENTRY IN THE TABLE   *   06524014
*                   R4 =  LENGTH OF DATA                            *   06525014
*                   R5 =  A(DATA)                                   *   06526014
*                   R15 = A(ENTRY IN TABLE)                         *   06527014
*                                                                   *   06528014
*-------------------------------------------------------------------*   06529014
                                                                        06530014
         LR    R14,R4                GET THE LENGTH OF DATA             06531014
         SH    R14,=H'1'             EXECUTE LENGTH                     06532014
         EX    R14,HE9998            MOVE IT                            06533014
         STC   R14,DTE_Length-DT_Entry(R15) AND SAVE EXEC LENGTH        06534014
                                                                        06535014
*-------------------------------------------------------------------*   06536014
*                                                                   *   06537014
*    NOW DECODE THE REST OF THE STUFF IN THE RECORD                 *   06538014
*                  R2  = A(NEXT CHAR IN INPUT)                      *   06539014
*                  R3  = LEN REMAINING IN INPUT                     *   06540014
*                  R15 = A(ENTRY IN TABLE)                          *   06541014
*    NOTE THAT WE HAVE NOT YET UPDATED THE NUMBER OF ENTRIES IN     *   06542014
*   THE TABLE (DT_Entry_Count), SO THAT IF WE FIND AN ERROR IN THE  *   06543014
*   REMAINDER OF THE INPUT CARD, WE CAN JUST EXIT.                  *   06544014
*                                                                   *   06545014
*-------------------------------------------------------------------*   06546014
                                                                        06547014
         SLR   R0,R0                 CLEAR OFFSET                       06548014
         LTR   R3,R3                 ANY DATA REMAINING IN THE INPUT?   06549014
         BZ    HE0180                NO, WE'RE FINISHED                 06550014
         CLI   0(R2),C' '            YES, HAVE WE FINISHED?             06551014
         BE    HE0180                YES, GO UPDATE TABLE               06552014
         CLI   0(R2),C','            NO, IS THIS A VALID SEPARATOR?     06553014
         BNE   HE8000                NO, ERROR                          06554014
         CLI   1(R2),C'0'            IS THE NEXT CHAR NUMERIC?          06555014
         BL    HE0180                NO, SKIP                           06556014
         CLI   1(R2),C'9'            SO FAR, CHECK AGAIN                06557014
         BH    HE0180                NOT NUMERIC, SKIP                  06558014
         LA    R2,1(0,R2)            YES, POINT PAST IT                 06559014
         SH    R3,=H'1'              ADJUST LENGTH REMAINING            06560014
         BNP   HE8000                TRAILING COMMA, ERROR              06561014
         MVI   @HE_TRTab,X'FF'       SET UP TRANSLATE TABLE             06562014
         MVC   @HE_TRTab+1(255),@HE_TRTab SO THAT ALL                   06563014
         MVI   @HE_TRTab+C'0',X'00'      CHARACTERS EXCEPT              06564014
         MVC   @HE_TRTab+C'1'(9),@HE_TRTab+C'0' NUMERICS ARE INVALID    06565014
         MVI   @HE_TRTab+C' ',X'01'  FLAG SPACE AS SPECIAL              06566014
         MVI   @HE_TRTab+C',',X'01'    COMMA TOO                        06567014
         LR    R5,R2                 SAVE A(DATA)                       06568014
         LA    R1,0(R2,R3)           A(PAST LAST CHAR)                  06569014
         LA    R2,1                  SIMULATE A SPACE                   06570014
         EX    R3,HE9997             CHECK OUR CHARACTERS               06571014
         LR    R0,R2                 SAVE R2                            06572014
         LR    R2,R5                 RESTORE R2                         06573014
         CH    R0,=H'254'            DID WE FIND AN INVALID CHAR?       06574014
         BNL   HE8000                YES, INVALID                       06575014
         SR    R1,R2                 R1 = LENGTH OF DATA                06576014
         SH    R1,=H'1'              EXEC LEN                           06577014
         BM    HE8000                NOTHING, INVALID                   06578014
         EX    R1,HE9999             PACK THE DATA                      06579014
         CVB   R0,@HE_DBLWD          AND PUT IT R0                      06580014
         C     R0,=F'32767'          IS IT TOO BIG?                     06581014
         BH    HE8000                YES, ERROR                         06582014
         LA    R1,1(0,R1)            RESTORE REAL OPERAND LEN           06583014
         AR    R2,R1                 ADJUST NEXT PTR                    06584014
         SR    R3,R1                 AND LENGTH REMAINING               06585014
HE0180   DS    0H                                                       06586014
         STH   R0,DTE_Offset-DT_Entry(R15) SAVE IT                      06587014
         SLR   R0,R0                 CLEAR REGISTER                     06588014
         L     R15,ODTE_DT_Ptr       A(DATA TABLE)                      06589014
         LH    R14,DT_Entry_Count-Data_Tbl(R15) GET NBR ENTRIES         06590014
         LA    R14,1(0,R14)          BUMP IT                            06591014
         STH   R14,DT_Entry_Count-Data_Tbl(R15) AND SAVE IT             06592014
         SLR   R15,R15               CLEAR R15                          06593014
         B     HE9000                AND EXIT                           06594014
HE8000   DS    0H                                                       06595014
         LA    R15,4                 SET RETURN CODE                    06596014
         B     HE9000                AND EXIT                           06597014
HE9000   DS    0H                                                       06598014
                                                                        06599014
*-------------------------------------------------------------------*   06600014
*                                                                   *   06601014
*    Free up our local storage ...                                  *   06602014
*                                                                   *   06603014
*-------------------------------------------------------------------*   06604014
                                                                        06605014
         L     R1,4(0,R13)              A(HSA)                          06606014
         L     R1,24(0,R1)              Get original R1                 06607014
         STM   R2,R3,0(R1)              Update parms                    06608014
                                                                        06609014
         LR    R3,R15                   Rescue the return code          06610014
                                                                        06611014
         LA    R1,@HE_Dynam             A(Local storage)                06612014
         L     R13,4(0,R13)             Rescue A(HSA)                   06613014
                                                                        06614014
         STACK POP,                     Free the stack area            +06615014
               ADDR=(R1),                starting here                 +06616014
               STACK=@_31Bit_Stack_Ptr   on this stack                  06617014
                                                                        06618014
*-------------------------------------------------------------------*   06619014
*                                                                   *   06620014
*    ... and return to caller                                       *   06621014
*                                                                   *   06622014
*-------------------------------------------------------------------*   06623014
                                                                        06624014
         LR    R15,R3                   Restore return code             06625014
         L     R14,12(0,R13)            Restore return address          06626014
         LM    R0,R12,20(R13)           Restore other registers         06627014
         BSM   0,R14                    and return                      06628014
                                                                        06629014
HE9997 TRT     0(0,R5),@HE_TRTab     FIND ENDING CHARACTER              06630014
HE9998 MVC     DTE_Data-DT_Entry(0,R15),0(R5)                           06631014
HE9999 PACK    @HE_DBLWD,0(0,R2)     PACK OFFSET/MSG CNT                06632014
                                                                        06633014
*-------------------------------------------------------------------*   06634014
*                                                                   *   06635014
*    TRANSLATE TABLES FOR HE0000, USED TO INTERPRET DATA= OPERANDS. *   06636014
*                                                                   *   06637014
*-------------------------------------------------------------------*   06638014
                                                                        06639014
HE_EBCDIC_TRTab DS XL256           EBCDIC TRANSLATE TABLE               06640014
         ORG   HE_EBCDIC_TRTab                                          06641014
         DC    256X'FE'                                                 06642014
         ORG   HE_EBCDIC_TRTab+C' '                                     06643014
         DC    X'02'                 SEPARATOR (BLANK)                  06644014
         ORG   HE_EBCDIC_TRTab+C','                                     06645014
         DC    X'02'                 SEPARATOR (,)                      06646014
         ORG   HE_EBCDIC_TRTab+C'Õ'                                     06647014
         DC    XL7'00'               Õ . < ( + ã &                      06648014
         ORG   HE_EBCDIC_TRTab+C'|'                                     06649014
         DC    XL8'00'               | $ * ) ; ^ - /                    06650014
         ORG   HE_EBCDIC_TRTab+C'%'                                     06651014
         DC    XL4'00'               % _ > ?                            06652014
         ORG   HE_EBCDIC_TRTab+C':'                                     06653014
         DC    XL6'00'               : # @ ' = "                        06654014
         ORG   HE_EBCDIC_TRTab+C'A'                                     06655014
         DC    XL9'00'               A-I                                06656014
         ORG   HE_EBCDIC_TRTab+C'J'                                     06657014
         DC    XL9'00'               J-R                                06658014
         ORG   HE_EBCDIC_TRTab+C'S'                                     06659014
         DC    XL8'00'               S-Z                                06660014
         ORG   HE_EBCDIC_TRTab+C'0'                                     06661014
         DC    XL10'00'              0-9                                06662014
         ORG                                                            06663014
                                                                        06664014
HE_Hex_TRTab DS XL256              HEX TRANSLATE TABLE                  06665014
         ORG   HE_Hex_TRTab                                             06666014
         DC    256X'FE'                                                 06667014
         ORG   HE_Hex_TRTab+C'A'                                        06668014
         DC    XL6'00'               A-F                                06669014
         ORG   HE_Hex_TRTab+C'0'                                        06670014
         DC    XL10'00'              0-9                                06671014
         ORG                                                            06672014
                                                                        06673014
         LTORG                                                          06674014
                                                                        06675014
@HE_Dynam       DSECT                 Dynamic area for HE               06676014
@HE_Save        DS    18F              O/S Style save area              06677014
@HE_DBLWD       DS    D                Work area                        06678014
@HE_DT_Pointer  DS    AL4              A(Data Table)                    06679014
@HE_DT_Subp_Len DS    FL4              Subpool, length of Dat Table     06680014
                DS    X                Flag Byte                        06681014
@HE_Quote       EQU   *-1,X'80'         1... ....  QUOTED STRING        06682014
@HE_Hex         EQU   *-1,X'40'         .1.. ....  HEX STRING           06683014
@HE_TRTab       DS    XL256            Translate table                  06684014
                DS    0D               Alignment                        06685014
@HE_DynLen      EQU   *-@HE_Dynam     Length of storage required        06686014
                                                                        06687014
SMFSLCT  RSECT                                                          06688014
                                                                        06689014
         DROP  R10                                                      06690014
         DROP  R11,R13                                                  06691014
         POP   USING                                                    06692014
         EJECT                                                          06693014
                                                                        06694014
         PUSH  USING                                                    06695014
                                                                        06696014
HF0010   DS    0H                                                       06697014
                                                                        06698014
*-------------------------------------------------------------------*   06699014
*                                                                   *   06700014
*    'JOB/DATA=' Keyword Processor                                  *   06701014
*    On Entry, R1 --->  A(Operand)                                  *   06702014
*                       Fullword containing # characters left in    *   06703014
*                        the input record                           *   06704014
*                       A(ODT Entry)                                *   06705014
*    On Exit,  R15 =   0 - All OK, ODTE_JobData_OR flag set         *   06706014
*                      4 - Error found                              *   06707014
*                                                                   *   06708014
*-------------------------------------------------------------------*   06709014
                                                                        06710014
         USING *,R15                                                    06711014
         SAVE  (14,12),T                                                06712014
         LR    R11,R15                  Load permanent base reg         06713014
         DROP  R15                      Free up temp base               06714014
         USING HF0010,R11               Assign permanent base           06715014
                                                                        06716014
*-------------------------------------------------------------------*   06717014
*                                                                   *   06718014
*    Get an area on the stack for our local storage                 *   06719014
*                                                                   *   06720014
*-------------------------------------------------------------------*   06721014
                                                                        06722014
         STACK PUSH,                    Get Stack area                 +06723014
               LEN=@HF_DynLen,           this long                     +06724014
               STACK=@_31Bit_Stack_Ptr   using this stack               06725014
                                                                        06726014
         LR    R2,R1                    Save its address                06727014
         LR    R0,R1                    A(Our storage)                  06728014
         LA    R1,@HF_DynLen             Its length                     06729014
         SLR   R14,R14                  Clear source address            06730014
         SLR   R15,R15                   and length                     06731014
         MVCL  R0,R14                   Clear our storage               06732014
         LR    R1,R2                    Restore A(Our storage)          06733014
                                                                        06734014
*-------------------------------------------------------------------*   06735014
*                                                                   *   06736014
*    Chain our save areas, and restore the important registers      *   06737014
*   that we have destroyed                                          *   06738014
*                                                                   *   06739014
*-------------------------------------------------------------------*   06740014
                                                                        06741014
         ST    R13,4(0,R1)              Chain                           06742014
         ST    R1,8(0,R13)               saveareas                      06743014
         LR    R13,R1                   Load dynam base                 06744014
         USING @HF_Dynam,R13            Assign a base                   06745014
         L     R15,@HF_Dynam+4          Get A(HSA)                      06746014
         LM    R0,R3,20(R15)            Restore callers registers       06747014
                                                                        06748014
*-------------------------------------------------------------------*   06749014
*                                                                   *   06750014
*    Load our parm registers:                                       *   06751014
*         R2 ---> Operand of keyword                                *   06752014
*         R3    = Nbr chars left in the input record                *   06753014
*                                                                   *   06754014
*-------------------------------------------------------------------*   06755014
                                                                        06756014
         LM    R2,R3,0(R1)              Get parms                       06757014
                                                                        06758014
         L     R10,8(0,R1)              A(ODT Entry)                    06759014
                                                                        06760014
                                                                        06761014
*-------------------------------------------------------------------*   06762014
*                                                                   *   06763014
*    CHECK FOR "OR" KEYWORD.                                        *   06764014
*                                                                   *   06765014
*-------------------------------------------------------------------*   06766014
                                                                        06767014
         CH    R3,=H'2'               ENOUGH ROOM LEFT FOR LITERAL?     06768014
         BNH   HF8000                 NO, ERROR                         06769014
         CLC   0(2,R2),=C'OR'         YES, IS THIS IT?                  06770014
         BNE   HF8000                 NO, ERROR                         06771014
         OI    ODTE_JobData_OR-ODT_Entry(R10),L'ODTE_JobData_OR Yes, OK 06772014
         LA    R2,2(0,R2)             POINT PAST LITERAL                06773014
         SH    R3,=H'2'               RESET LENGTH REMAINING            06774014
         SLR   R15,R15                ASSUME EVERYTHING IS OK           06775014
         B     HF9000                 AND EXIT                          06776014
HF8000   DS    0H                                                       06777014
                                                                        06778014
*-------------------------------------------------------------------*   06779014
*                                                                   *   06780014
*    SOME SORT OF ERROR FOUND, SET RETURN CODE                      *   06781014
*                                                                   *   06782014
*-------------------------------------------------------------------*   06783014
                                                                        06784014
         LA    R15,4                  FLAG AN ERROR                     06785014
         B     HF9000                 AND EXIT                          06786014
HF9000   DS    0H                                                       06787014
                                                                        06788014
*-------------------------------------------------------------------*   06789014
*                                                                   *   06790014
*    Free up our local storage ...                                  *   06791014
*                                                                   *   06792014
*-------------------------------------------------------------------*   06793014
                                                                        06794014
         L     R1,4(0,R13)              A(HSA)                          06795014
         L     R1,24(0,R1)              Get original R1                 06796014
         STM   R2,R3,0(R1)              Update parms                    06797014
                                                                        06798014
         LR    R3,R15                   Rescue the return code          06799014
                                                                        06800014
         LA    R1,@HF_Dynam             A(Local storage)                06801014
         L     R13,4(0,R13)             Rescue A(HSA)                   06802014
                                                                        06803014
         STACK POP,                     Free the stack area            +06804014
               ADDR=(R1),                starting here                 +06805014
               STACK=@_31Bit_Stack_Ptr   on this stack                  06806014
                                                                        06807014
*-------------------------------------------------------------------*   06808014
*                                                                   *   06809014
*    ... and return to caller                                       *   06810014
*                                                                   *   06811014
*-------------------------------------------------------------------*   06812014
                                                                        06813014
         LR    R15,R3                   Restore return code             06814014
         L     R14,12(0,R13)            Restore return address          06815014
         LM    R0,R12,20(R13)           Restore other registers         06816014
         BSM   0,R14                    and return                      06817014
                                                                        06818014
                                                                        06819014
         LTORG                                                          06820014
                                                                        06821014
@HF_Dynam       DSECT                 Dynamic area for HF               06822014
@HF_Save        DS    18F              O/S Style save area              06823014
                DS    0D               Alignment                        06824014
@HF_DynLen      EQU   *-@HF_Dynam     Length of storage required        06825014
                                                                        06826014
SMFSLCT  RSECT                                                          06827014
                                                                        06828014
         DROP  R11,R13                                                  06829014
         POP   USING                                                    06830014
         EJECT                                                          06831014
                                                                        06832014
         PUSH  USING                                                    06833014
                                                                        06834014
HG0010   DS    0H                                                       06835014
                                                                        06836014
*-------------------------------------------------------------------*   06837014
*                                                                   *   06838014
*    'STOPAFT=' Keyword Processor                                   *   06839014
*    On Entry, R1 --->  A(Operand)                                  *   06840014
*                       Fullword containing # characters left in    *   06841014
*                        the input record                           *   06842014
*                       A(ODT Entry)                                *   06843014
*    On Exit,  R15 =   0 - All OK, ODTE_StopAft updated             *   06844014
*                      4 - Error found                              *   06845014
*                                                                   *   06846014
*-------------------------------------------------------------------*   06847014
                                                                        06848014
         USING *,R15                                                    06849014
         SAVE  (14,12),T                                                06850014
         LR    R11,R15                  Load permanent base reg         06851014
         DROP  R15                      Free up temp base               06852014
         USING HG0010,R11               Assign permanent base           06853014
                                                                        06854014
*-------------------------------------------------------------------*   06855014
*                                                                   *   06856014
*    Get an area on the stack for our local storage                 *   06857014
*                                                                   *   06858014
*-------------------------------------------------------------------*   06859014
                                                                        06860014
         STACK PUSH,                    Get Stack area                 +06861014
               LEN=@HG_DynLen,           this long                     +06862014
               STACK=@_31Bit_Stack_Ptr   using this stack               06863014
                                                                        06864014
         LR    R2,R1                    Save its address                06865014
         LR    R0,R1                    A(Our storage)                  06866014
         LA    R1,@HG_DynLen             Its length                     06867014
         SLR   R14,R14                  Clear source address            06868014
         SLR   R15,R15                   and length                     06869014
         MVCL  R0,R14                   Clear our storage               06870014
         LR    R1,R2                    Restore A(Our storage)          06871014
                                                                        06872014
*-------------------------------------------------------------------*   06873014
*                                                                   *   06874014
*    Chain our save areas, and restore the important registers      *   06875014
*   that we have destroyed                                          *   06876014
*                                                                   *   06877014
*-------------------------------------------------------------------*   06878014
                                                                        06879014
         ST    R13,4(0,R1)              Chain                           06880014
         ST    R1,8(0,R13)               saveareas                      06881014
         LR    R13,R1                   Load dynam base                 06882014
         USING @HG_Dynam,R13            Assign a base                   06883014
         L     R15,@HG_Dynam+4          Get A(HSA)                      06884014
         LM    R0,R3,20(R15)            Restore callers registers       06885014
                                                                        06886014
*-------------------------------------------------------------------*   06887014
*                                                                   *   06888014
*    Load our parm registers:                                       *   06889014
*         R2 ---> Operand of keyword                                *   06890014
*         R3    = Nbr chars left in the input record                *   06891014
*                                                                   *   06892014
*-------------------------------------------------------------------*   06893014
                                                                        06894014
         LM    R2,R3,0(R1)              Get parms                       06895014
                                                                        06896014
         L     R10,8(0,R1)              A(ODT Entry)                    06897014
                                                                        06898014
*-------------------------------------------------------------------*   06899014
*                                                                   *   06900014
*    LOOK FOR THE FIRST NON-NUMERIC                                 *   06901014
*                                                                   *   06902014
*-------------------------------------------------------------------*   06903014
                                                                        06904014
         LR    R4,R2                  A(START OF OPERAND                06905014
HG0020   DS    0H                                                       06906014
         CLI   0(R2),C'0'             IS THIS NUMERIC?                  06907014
         BL    HG0030                 NO, SKIP                          06908014
         CLI   0(R2),C'9'             SO FAR, CHECK AGAIN               06909014
         BH    HG0020                 NO, NOT NUMERIC                   06910014
         LA    R2,1(0,R2)             OK, BUMP POINTER                  06911014
         BCT   R3,HG0020              AND TRY AGAIN                     06912014
         B     HG0040                 OK, SKIP                          06913014
HG0030   DS    0H                                                       06914014
                                                                        06915014
*-------------------------------------------------------------------*   06916014
*                                                                   *   06917014
*    WE HAVE A NON-NUMERIC CHARACTER - CHECK TO MAKE SURE IT'S A    *   06918014
*    VALID SEPARATOR (BLANK OR COMMA).                              *   06919014
*                                                                   *   06920014
*-------------------------------------------------------------------*   06921014
                                                                        06922014
         CLI   0(R2),C','             IS THIS A COMMA?                  06923014
         BE    HG0040                 YES, SKIP                         06924014
         CLI   0(R2),C' '             NO, HOW ABOUT A BLANK             06925014
         BNE   HG8000                 NO, WE HAVE AN ERROR              06926014
HG0040   DS    0H                                                       06927014
                                                                        06928014
*-------------------------------------------------------------------*   06929014
*                                                                   *   06930014
*    CHECK TO SEE IF THE NUMBER IS TOO LONG (MAX = 9 CHARACTERS)    *   06931014
*                                                                   *   06932014
*-------------------------------------------------------------------*   06933014
                                                                        06934014
         LR    R15,R2                GET A(SEPARATOR)                   06935014
         SR    R15,R4                LESS THE START                     06936014
         CH    R15,=H'9'             IS IT TOO LONG?                    06937014
         BH    HG8000                YES, ERROR                         06938014
                                                                        06939014
*-------------------------------------------------------------------*   06940014
*                                                                   *   06941014
*    IT SEEMS TO BE OK, SO BINARIZE IT AND SAVE IN ODTE_StopAft.    *   06942014
*                                                                   *   06943014
*-------------------------------------------------------------------*   06944014
                                                                        06945014
         SH    R15,=H'1'             EXECUTE LEN                        06946014
         EX    R15,HG9999            PACK IT                            06947014
         CVB   R15,@HG_DBLWD         BINARIZE IT                        06948014
         LTR   R15,R15               IS THE NUMBER 0?                   06949014
         BE    HG8000                YES, THAT'S AN ERROR               06950014
         ST    R15,ODTE_StopAft-ODT_Entry(R10) No, save it              06951014
         SLR   R15,R15               CLEAR THE RETURN CODE              06952014
         B     HG9000                AND EXIT                           06953014
HG8000   DS    0H                                                       06954014
                                                                        06955014
*-------------------------------------------------------------------*   06956014
*                                                                   *   06957014
*    WE HAVE AN ERROR, SET THE RETURN CODE AND EXIT.                *   06958014
*                                                                   *   06959014
*-------------------------------------------------------------------*   06960014
                                                                        06961014
         LA    R15,4                  FLAG AN ERROR                     06962014
         B     HG9000                 AND EXIT                          06963014
HG9000   DS    0H                                                       06964014
                                                                        06965014
*-------------------------------------------------------------------*   06966014
*                                                                   *   06967014
*    Free up our local storage ...                                  *   06968014
*                                                                   *   06969014
*-------------------------------------------------------------------*   06970014
                                                                        06971014
         L     R1,4(0,R13)              A(HSA)                          06972014
         L     R1,24(0,R1)              Get original R1                 06973014
         STM   R2,R3,0(R1)              Update parms                    06974014
                                                                        06975014
         LR    R3,R15                   Rescue the return code          06976014
                                                                        06977014
         LA    R1,@HG_Dynam             A(Local storage)                06978014
         L     R13,4(0,R13)             Rescue A(HSA)                   06979014
                                                                        06980014
         STACK POP,                     Free the stack area            +06981014
               ADDR=(R1),                starting here                 +06982014
               STACK=@_31Bit_Stack_Ptr   on this stack                  06983014
                                                                        06984014
*-------------------------------------------------------------------*   06985014
*                                                                   *   06986014
*    ... and return to caller                                       *   06987014
*                                                                   *   06988014
*-------------------------------------------------------------------*   06989014
                                                                        06990014
         LR    R15,R3                   Restore return code             06991014
         L     R14,12(0,R13)            Restore return address          06992014
         LM    R0,R12,20(R13)           Restore other registers         06993014
         BSM   0,R14                    and return                      06994014
                                                                        06995014
HG9999 PACK    @HG_DBLWD,0(0,R4)        PACK STOPAFT COUNT              06996014
                                                                        06997014
         LTORG                                                          06998014
                                                                        06999014
@HG_Dynam       DSECT                 Dynamic area for O                07000014
@HG_Save        DS    18F              O/S Style save area              07001014
@HG_DBLWD       DS    D                Work area                        07002014
                DS    0D               Alignment                        07003014
@HG_DynLen      EQU   *-@HG_Dynam     Length of storage required        07004014
                                                                        07005014
SMFSLCT  RSECT                                                          07006014
                                                                        07007014
         DROP  R11,R13                                                  07008014
         POP   USING                                                    07009014
         EJECT                                                          07010014
                                                                        07011014
         PUSH  USING                                                    07012014
                                                                        07013014
HH0010   DS    0H                                                       07014014
                                                                        07015014
*-------------------------------------------------------------------*   07016014
*                                                                   *   07017014
*    'INEXIT=/OUTEXIT=' Keyword Processor                           *   07018014
*    On Entry, R1 --->  A(Operand)                                  *   07019014
*                       Fullword containing # characters left in    *   07020014
*                        the input record                           *   07021014
*                       A(ODT Entry)                                *   07022014
*    On Exit,  R15 =   0 - All OK, ODTE_InExit or ODTE_OutExit      *   07023014
*                          updated                                  *   07024014
*                      4 - Error found                              *   07025014
*                                                                   *   07026014
*-------------------------------------------------------------------*   07027014
                                                                        07028014
         USING *,R15                                                    07029014
         SAVE  (14,12),T                                                07030014
         LR    R11,R15                  Load permanent base reg         07031014
         DROP  R15                      Free up temp base               07032014
         USING HH0010,R11               Assign permanent base           07033014
                                                                        07034014
*-------------------------------------------------------------------*   07035014
*                                                                   *   07036014
*    Get an area on the stack for our local storage                 *   07037014
*                                                                   *   07038014
*-------------------------------------------------------------------*   07039014
                                                                        07040014
         STACK PUSH,                    Get Stack area                 +07041014
               LEN=@HH_DynLen,           this long                     +07042014
               STACK=@_31Bit_Stack_Ptr   using this stack               07043014
                                                                        07044014
         LR    R2,R1                    Save its address                07045014
         LR    R0,R1                    A(Our storage)                  07046014
         LA    R1,@HH_DynLen             Its length                     07047014
         SLR   R14,R14                  Clear source address            07048014
         SLR   R15,R15                   and length                     07049014
         MVCL  R0,R14                   Clear our storage               07050014
         LR    R1,R2                    Restore A(Our storage)          07051014
                                                                        07052014
*-------------------------------------------------------------------*   07053014
*                                                                   *   07054014
*    Chain our save areas, and restore the important registers      *   07055014
*   that we have destroyed                                          *   07056014
*                                                                   *   07057014
*-------------------------------------------------------------------*   07058014
                                                                        07059014
         ST    R13,4(0,R1)              Chain                           07060014
         ST    R1,8(0,R13)               saveareas                      07061014
         LR    R13,R1                   Load dynam base                 07062014
         USING @HH_Dynam,R13            Assign a base                   07063014
         L     R15,@HH_Dynam+4          Get A(HSA)                      07064014
         LM    R0,R3,20(R15)            Restore callers registers       07065014
                                                                        07066014
*-------------------------------------------------------------------*   07067014
*                                                                   *   07068014
*    Load our parm registers:                                       *   07069014
*         R2 ---> Operand of keyword                                *   07070014
*         R3    = Nbr chars left in the input record                *   07071014
*                                                                   *   07072014
*-------------------------------------------------------------------*   07073014
                                                                        07074014
         LM    R2,R3,0(R1)              Get parms                       07075014
         L     R10,8(0,R1)              A(ODT Entry)                    07076014
         USING ODT_Entry,R10            Assign a base                   07077014
                                                                        07078014
*-------------------------------------------------------------------*   07079014
*                                                                   *   07080014
*    Figure out if we are INEXIT or OUTEXIT, and set our flag       *   07081014
*                                                                   *   07082014
*-------------------------------------------------------------------*   07083014
                                                                        07084014
         LR    R15,R2                 A(Start of operands)              07085014
         S     R15,=FL4'5'            Back up to keyword                07086014
         CLI   0(R15),C'N'            INEXIT=?                          07087014
         BNE   HH0020                 No, must be OUTEXIT=              07088014
         OI    @HH_InExit,L'@HH_InExit Yes, set our flag                07089014
HH0020   DS    0H                                                       07090014
                                                                        07091014
*-------------------------------------------------------------------*   07092014
*                                                                   *   07093014
*    Figure out what we have here. We can be:                       *   07094014
*      INEXIT=exitname                                              *   07095014
*      INEXIT=(exitname)                                            *   07096014
*      INEXIT=(exitname,parm)                                       *   07097014
*      INEXIT=(exitname,'parm')                                     *   07098014
*                                                                   *   07099014
*-------------------------------------------------------------------*   07100014
                                                                        07101014
         CLI   0(R2),C'('             Open paren?                       07102014
         BNE   HH0020C                No, skip                          07103014
         OI    @HH_Paren,L'@HH_Paren  Yes, set our flag                 07104014
         LA    R2,1(0,R2)             Point past it                     07105014
         S     R3,=FL4'1'             and decrement length              07106014
HH0020C  DS    0H                                                       07107014
                                                                        07108014
*-------------------------------------------------------------------*   07109014
*                                                                   *   07110014
*    Find the end of the exit name, by looking for a blank, comma,  *   07111014
*   or, optionally, a close paren.                                  *   07112014
*                                                                   *   07113014
*-------------------------------------------------------------------*   07114014
                                                                        07115014
         LR    R14,R2                 A(Start)                          07116014
         LR    R15,R3                 Length                            07117014
HH0030   DS    0H                                                       07118014
         CLI   0(R14),C' '            END OF NAME?                      07119014
         BE    HH0050                 YES, SKIP                         07120014
         CLI   0(R14),C','            NO, TRY AGAIN                     07121014
         BE    HH0050                 FOUND END, SKIP                   07122014
         TM    @HH_Paren,L'@HH_Paren  Are we in parentheses?            07123014
         BNO   HH0040                 No, skip                          07124014
         CLI   0(R14),C')'            Yes, end of name?                 07125014
         BE    HH0050                 Yes, skip                         07126014
HH0040   DS    0H                                                       07127014
         LA    R14,1(0,R14)           Point to next char                07128014
         BCT   R15,HH0030             and keep checking                 07129014
HH0050   DS    0H                                                       07130014
                                                                        07131014
*-------------------------------------------------------------------*   07132014
*                                                                   *   07133014
*    We have the end of the name - move it to our ODT entry         *   07134014
*                                                                   *   07135014
*-------------------------------------------------------------------*   07136014
                                                                        07137014
         SR    R14,R2                 Get length of name                07138014
         C     R14,=FL4'8'            Too long?                         07139014
         BH    HH8000                 Yes, error                        07140014
                                                                        07141014
         LR    R1,R2                  Start of data to be moved         07142014
         LR    R3,R15                 Save length remaining             07143014
         AR    R2,R14                 and reset input pointer           07144014
                                                                        07145014
         S     R14,=FL4'1'            EX Length to be moved             07146014
                                                                        07147014
         TM    @HH_InExit,L'@HH_InExit Is this INEXIT                   07148014
         BZ    HH0060                 No, skip                          07149014
         LA    R15,ODTE_InExit        Yes, A(InExit name)               07150014
         MVC   ODTE_InExit,=CL(L'ODTE_InExit)' '  Clear it              07151014
         LTR   R14,R14                Anything?                         07152014
         BNM   HH0070                 Yes, OK                           07153014
         OI    ODTE_No_Dflt_InExit,l'ODTE_No_Dflt_InExit Don't use dflt 07154014
         B     HH0070C                and skip                          07155014
HH0060   DS    0H                                                       07156014
         LA    R15,ODTE_OutExit       A(OutExit Name)                   07157014
         MVC   ODTE_OutExit,=CL(L'ODTE_OutExit)' '  Clear it            07158014
         LTR   R14,R14                Anything?                         07159014
         BNM   HH0070                 Yes, OK                           07160014
         OI    ODTE_No_Dflt_OutExit,l'ODTE_No_Dflt_OutExit No dflt      07161014
         B     HH0070C                and skip                          07162014
                                                                        07163014
HH0070   DS    0H                                                       07164014
         EX    R14,HH_OC              OC    0(0,R15),0(R1)              07165014
                                                                        07166014
HH0070C  DS    0H                                                       07167014
         TM    @HH_Paren,L'@HH_Paren  Are we in paren?                  07168014
         BNO   HH0180                 No, exit                          07169014
         CLI   0(R2),C')'             Yes, at the end?                  07170014
         BNE   HH0080                 No, skip                          07171014
         LA    R2,1(0,R2)             Yes, point past it                07172014
         S     R3,=FL4'1'             decrement the length              07173014
         B     HH0180                 and exit                          07174014
HH0080   DS    0H                                                       07175014
         CLI   0(R2),C','             Separator?                        07176014
         BNE   HH8000                 No, exit with error               07177014
         LA    R2,1(0,R2)             Yes, point past it                07178014
         S     R3,=FL4'1'             decrement the length              07179014
         SLR   R0,R0                  Clear work register               07180014
         LA    R0,C')'                Ending character                  07181014
                                                                        07182014
         CLI   0(R2),C''''            Open quote?                       07183014
         BNE   HH0090                 No, skip                          07184014
         OI    @HH_Quotes,L'@HH_Quotes Yes, set our flag                07185014
         LA    R2,1(0,R2)             Point past it                     07186014
         S     R3,=FL4'1'             Decrement length remaining        07187014
         BNP   HH8000                 Nothing left, error               07188014
         LA    R0,C''''               Ending character                  07189014
HH0090   DS    0H                                                       07190014
                                                                        07191014
         LA    R14,0(R2,R3)           A(Byte after string)              07192014
         LR    R15,R2                 A(1st byte of string)             07193014
HH0100   DS    0H                                                       07194014
         SRST  R14,R15                Find it                           07195014
         BO    HH0100                 Keep trying (CC=3)                07196014
         BNL   HH8000                 Not found, exit with error        07197014
                                                                        07198014
         SR    R14,R2                 Length of data                    07199014
         BZ    HH0110                 None, skip                        07200014
                                                                        07201014
         LR    R0,R2                  Start of data to be moved         07202014
         SR    R3,R14                 Update length remaining           07203014
         AR    R2,R14                 Bump pointer                      07204014
                                                                        07205014
         LR    R1,R14                 Length of data to be moved        07206014
         LA    R14,@HH_ParmArea       Temp save area                    07207014
         AH    R14,@HH_ParmLen        A(Next byte)                      07208014
         LH    R15,@HH_ParmLen        Bump                              07209014
         AR    R15,R1                  current                          07210014
         STH   R15,@HH_ParmLen         length                           07211014
         LR    R15,R1                 Len to be moved                   07212014
         MVCL  R14,R0                 Move parm data                    07213014
HH0110   DS    0H                                                       07214014
         TM    @HH_Quotes,L'@HH_Quotes Are we in quotes?                07215014
         BNO   HH0130                No, skip                           07216014
         LA    R2,1(0,R2)            Point past quote                   07217014
         S     R3,=FL4'1'            Decrement length remaining         07218014
         CLI   0(R2),C''''           Is this two quotes?                07219014
         BNE   HH0130                No, found the end                  07220014
                                                                        07221014
         LA    R15,@HH_ParmArea      Yes, A(Parmarea)                   07222014
         AH    R15,@HH_ParmLen       Point past current data            07223014
         MVI   0(R15),C''''          Add a quote                        07224014
         LH    R15,@HH_ParmLen       Bump the                           07225014
         A     R15,=FL4'1'            current                           07226014
         STH   R15,@HH_ParmLen        length                            07227014
         LA    R2,1(0,R2)            Point past quote                   07228014
         S     R3,=FL4'1'            Decrement length remaining         07229014
         LA    R0,C''''              Look for a quote                   07230014
         B     HH0090                and keep checking                  07231014
                                                                        07232014
HH0130   DS    0H                                                       07233014
         LA    R2,1(0,R2)            Bump past close paren              07234014
         S     R3,=FL4'1'            and decrement length               07235014
         B     HH0150                exit                               07236014
                                                                        07237014
HH0150   DS    0H                                                       07238014
                                                                        07239014
*-------------------------------------------------------------------*   07240014
*                                                                   *   07241014
*    We have assembled the parm in @HH_ParmArea. If a parm already  *   07242014
*   exists, free it up ...                                          *   07243014
*                                                                   *   07244014
*-------------------------------------------------------------------*   07245014
                                                                        07246014
         TM    @HH_InExit,L'@HH_InExit Is this INEXIT=?                 07247014
         BNO   HH0150C                No, skip                          07248014
         L     R1,ODTE_InExit_Parm_Ptr Yes, get A(Parm)                 07249014
         B     HH0150E                and skip                          07250014
HH0150C  DS    0H                                                       07251014
         L     R1,ODTE_InExit_Parm_Ptr Get A(Parm)                      07252014
HH0150E  DS    0H                                                       07253014
         LTR   R1,R1                  Any parm?                         07254014
         BZ    HH0150I                No, skip                          07255014
         LH    R0,0(0,R1)             Yes, get its length               07256014
         A     R0,=AL4(L'@HH_ParmLen) and bump for the length field     07257014
         STORAGE RELEASE,             Free the storage                 +07258014
               ADDR=(1),               starting here                   +07259014
               LENGTH=(0)              for this long                    07260014
                                                                        07261014
         SLR   R15,R15                Clear work register               07262014
         TM    @HH_Inexit,L'@HH_InExit Is this INEXIT=?                 07263014
         BNO   HH0150G                No, skip                          07264014
         ST    R15,ODTE_InExit_Parm_Ptr Yes, clear it                   07265014
         B     HH0150I                and skip                          07266014
HH0150G  DS    0H                                                       07267014
         ST    R15,ODTE_OutExit_Parm_Ptr Clear parm pointer             07268014
HH0150I  DS    0H                                                       07269014
                                                                        07270014
*-------------------------------------------------------------------*   07271014
*                                                                   *   07272014
*    ... and copy it to 24-bit storage                              *   07273014
*                                                                   *   07274014
*-------------------------------------------------------------------*   07275014
                                                                        07276014
         LA    R0,L'@HH_ParmLen       Length of Parm length             07277014
         AH    R0,@HH_ParmLen         plus length of parm               07278014
                                                                        07279014
         STORAGE OBTAIN,                Go get our storage             +07280014
               LENGTH=(0),               this long                     +07281014
               LOC=BELOW                 below-the-line                 07282014
                                                                        07283014
         TM    @HH_InExit,L'@HH_InExit Is this INEXIT=?                 07284014
         BNO   HH0160                 No, skip                          07285014
         ST    R1,ODTE_InExit_Parm_Ptr Yes, save A(Parm)                07286014
         B     HH0170                 and skip                          07287014
HH0160   DS    0H                                                       07288014
         ST    R1,ODTE_OutExit_Parm_Ptr Save A(Parm)                    07289014
         B     HH0170                 and skip                          07290014
HH0170   DS    0H                                                       07291014
                                                                        07292014
         MVC   0(L'@HH_ParmLen,R1),@HH_ParmLen Save parm length         07293014
         LA    R0,L'@HH_ParmLen(0,R1) Point past it                     07294014
         LH    R1,@HH_ParmLen         Get its length                    07295014
         LA    R14,@HH_ParmArea       Point to assenbled parm           07296014
         LR    R15,R1                 Same length                       07297014
         MVCL  R0,R14                 Move parm                         07298014
                                                                        07299014
HH0180   DS    0H                                                       07300014
         SLR   R15,R15                CLEAR RETURN CODE                 07301014
         B     HH9000                 AND EXIT                          07302014
                                                                        07303014
HH8000   DS    0H                                                       07304014
         LA    R15,4                  Bad stuff                         07305014
         B     HH9000                 AND EXIT                          07306014
                                                                        07307014
HH9000   DS    0H                                                       07308014
                                                                        07309014
*-------------------------------------------------------------------*   07310014
*                                                                   *   07311014
*    Free up our local storage ...                                  *   07312014
*                                                                   *   07313014
*-------------------------------------------------------------------*   07314014
                                                                        07315014
         L     R1,4(0,R13)              A(HSA)                          07316014
         L     R1,24(0,R1)              Get original R1                 07317014
         STM   R2,R3,0(R1)              Update parms                    07318014
                                                                        07319014
         LR    R3,R15                   Rescue the return code          07320014
                                                                        07321014
         LA    R1,@HH_Dynam             A(Local storage)                07322014
         L     R13,4(0,R13)             Rescue A(HSA)                   07323014
                                                                        07324014
         STACK POP,                     Free the stack area            +07325014
               ADDR=(R1),                starting here                 +07326014
               STACK=@_31Bit_Stack_Ptr   on this stack                  07327014
                                                                        07328014
*-------------------------------------------------------------------*   07329014
*                                                                   *   07330014
*    ... and return to caller                                       *   07331014
*                                                                   *   07332014
*-------------------------------------------------------------------*   07333014
                                                                        07334014
         LR    R15,R3                   Restore return code             07335014
         L     R14,12(0,R13)            Restore return address          07336014
         LM    R0,R12,20(R13)           Restore other registers         07337014
         BSM   0,R14                    and return                      07338014
                                                                        07339014
HH_OC    OC    0(0,R15),0(R1)           Move exit name                  07340014
                                                                        07341014
         LTORG                                                          07342014
                                                                        07343014
@HH_Dynam       DSECT                 Dynamic area for HH               07344014
@HH_Save        DS    18F              O/S Style save area              07345014
                DS    X                Flag Byte                        07346014
@HH_InExit      EQU   *-1,X'80'         1... .... INEXIT= statement     07347014
@HH_Paren       EQU   *-1,X'40'         .1.. .... Parentheses           07348014
@HH_Quotes      EQU   *-1,X'20'         ..1. .... Parm in quotes        07349014
@HH_ParmArea    DS    CL256            Parm area                        07350014
@HH_ParmLen     DS    HL2              Length of parm                   07351014
                DS    0D               Alignment                        07352014
@HH_DynLen      EQU   *-@HH_Dynam     Length of storage required        07353014
                                                                        07354014
SMFSLCT  RSECT                                                          07355014
                                                                        07356014
         DROP  R10                                                      07357014
         DROP  R11,R13                                                  07358014
         POP   USING                                                    07359014
         EJECT                                                          07360014
                                                                        07361014
         PUSH  USING                                                    07362014
                                                                        07363014
HI0010   DS    0H                                                       07364014
                                                                        07365014
*-------------------------------------------------------------------*   07366014
*                                                                   *   07367014
*    'OUTPUT=' Keyword Processor                                    *   07368014
*    On Entry, R1 --->  A(Operand)                                  *   07369014
*                       Fullword containing # characters left in    *   07370014
*                        the input record                           *   07371014
*                       A(ODT Entry)                                *   07372014
*    On Exit,  R15 =   0 - All OK, 3rd parm updated with A(new ODT  *   07373014
*                          entry)                                   *   07374014
*                      4 - Error found                              *   07375014
*                                                                   *   07376014
*-------------------------------------------------------------------*   07377014
                                                                        07378014
         USING *,R15                                                    07379014
         SAVE  (14,12),T                                                07380014
         LR    R11,R15                  Load permanent base reg         07381014
         DROP  R15                      Free up temp base               07382014
         USING HI0010,R11               Assign permanent base           07383014
                                                                        07384014
*-------------------------------------------------------------------*   07385014
*                                                                   *   07386014
*    Get an area on the stack for our local storage                 *   07387014
*                                                                   *   07388014
*-------------------------------------------------------------------*   07389014
                                                                        07390014
         STACK PUSH,                    Get Stack area                 +07391014
               LEN=@HI_DynLen,           this long                     +07392014
               STACK=@_31Bit_Stack_Ptr   using this stack               07393014
                                                                        07394014
         LR    R2,R1                    Save its address                07395014
         LR    R0,R1                    A(Our storage)                  07396014
         LA    R1,@HI_DynLen             Its length                     07397014
         SLR   R14,R14                  Clear source address            07398014
         SLR   R15,R15                   and length                     07399014
         MVCL  R0,R14                   Clear our storage               07400014
         LR    R1,R2                    Restore A(Our storage)          07401014
                                                                        07402014
*-------------------------------------------------------------------*   07403014
*                                                                   *   07404014
*    Chain our save areas, and restore the important registers      *   07405014
*   that we have destroyed                                          *   07406014
*                                                                   *   07407014
*-------------------------------------------------------------------*   07408014
                                                                        07409014
         ST    R13,4(0,R1)              Chain                           07410014
         ST    R1,8(0,R13)               saveareas                      07411014
         LR    R13,R1                   Load dynam base                 07412014
         USING @HI_Dynam,R13            Assign a base                   07413014
         L     R15,@HI_Dynam+4          Get A(HSA)                      07414014
         LM    R0,R3,20(R15)            Restore callers registers       07415014
                                                                        07416014
*-------------------------------------------------------------------*   07417014
*                                                                   *   07418014
*    Load our parm registers:                                       *   07419014
*         R2 ---> Operand of keyword                                *   07420014
*         R3    = Nbr chars left in the input record                *   07421014
*                                                                   *   07422014
*-------------------------------------------------------------------*   07423014
                                                                        07424014
         LM    R2,R3,0(R1)              Get parms                       07425014
         L     R10,8(0,R1)              A(ODT Entry)                    07426014
                                                                        07427014
                                                                        07428014
*-------------------------------------------------------------------*   07429014
*                                                                   *   07430014
*    AND MOVE THE NAME, CHARACTER BY CHARACTER.                     *   07431014
*                                                                   *   07432014
*-------------------------------------------------------------------*   07433014
                                                                        07434014
         LA    R15,@HI_DDName         A(HOLDING AREA)                   07435014
         MVC   @HI_DDName,=CL8' '     CLEAR IT OUT                      07436014
         LA    R14,9                  MAX LENGTH OF NAME + 1            07437014
         CR    R14,R3                 ARE THERE THAT MANY LEFT?         07438014
         BNH   HI0020                 YES, SKIP                         07439014
         LR    R14,R3                 NO, USE ONLY WHATS LEFT           07440014
HI0020   DS    0H                                                       07441014
         CLI   0(R2),C' '             END OF NAME?                      07442014
         BE    HI0030                 YES, SKIP                         07443014
         CLI   0(R2),C','             NO, TRY AGAIN                     07444014
         BE    HI0030                 FOUND END, SKIP                   07445014
         OC    0(1,R15),0(R2)         MOVE CHAR TO NAME FIELD           07446014
         LA    R15,1(0,R15)           BUMP RECEIVING POINTER            07447014
         LA    R2,1(0,R2)             BUMP SENDING POINTER              07448014
         SH    R3,=H'1'               DECREASE CHARS REMAINING          07449014
         BCT   R14,HI0020             AND KEEP GOING                    07450014
         LA    R15,4                  TOO MANY CHARACTERS (>8)          07451014
         B     HI9000                 SO EXIT                           07452014
HI0030   DS    0H                                                       07453014
                                                                        07454014
*-------------------------------------------------------------------*   07455014
*                                                                   *   07456014
*    WE HAVE GOT THE NAME, SO FIND AN ENTRY IN THE OUTPUT ENV TBL.  *   07457014
*   NOTE THAT AN ENTRY MAY ALREADY EXIST - IF SO, WE USE IT. IF NOT,*   07458014
*   WE CREATE ONE,                                                  *   07459014
*                                                                   *   07460014
*-------------------------------------------------------------------*   07461014
                                                                        07462014
         L     R10,@_ODT_Ptr         POINT TO OUR TABLE                 07463014
         LH    R9,ODT_Entry_Count-OutDesc_Tbl(R10) GET NBR ENTRIES      07464014
         LA    R10,ODT_Entry-OutDesc_Tbl(R10) AND POINT TO FIRST        07465014
                                                                        07466014
         USING ODT_Entry,R10            Tell the assembler              07467014
                                                                        07468014
HI0040   DS    0H                                                       07469014
         CLC   ODTE_DDName,@HI_DDName OUR ENTRY?                        07470014
         BE    HI0060                 YES, SKIP                         07471014
         AL    R10,=AL4(ODTE_Length)  No, point to next entry           07472014
         BCT   R9,HI0040              and check it out                  07473014
                                                                        07474014
*-------------------------------------------------------------------*   07475014
*                                                                   *   07476014
*    WE DIDN'T FIND OUR ENTRY IN THE TABLE, SO WE WILL CREATE ONE.  *   07477014
*                                                                   *   07478014
*-------------------------------------------------------------------*   07479014
                                                                        07480014
         L     R1,@_ODT_Ptr           A(OUR TABLE)                      07481014
         L     R15,=AL4(ODTE_Length)  LENGTH OF ONE ENTRY               07482014
         LH    R14,ODT_Entry_Count-OutDesc_Tbl(R1) Nbr entries          07483035
         LA    R14,1(0,R14)           PLUS ONE FOR THE NEW ONE          07484014
*        MR    R14,R14                LENGTH OF ENTRIES @RWS 15-11-13   07485030
         MR    R14,R15                LENGTH OF ENTRIES @RWS 15-11-13   07486028
         LA    R15,ODT_Entry-OutDesc_Tbl(0,R15)   PLUS LEN OF HEADER    07487020
         L     R1,ODT_Subp_Len-OutDesc_Tbl(R1)  Get Tbl Len, Subpool    07488020
         SLL   R1,8                   GET RID OF                        07489014
         SRL   R1,8                    SUBPOOL                          07490014
         CR    R1,R15                 DO WE HAVE ROOM?                  07491014
         BNL   HI0050                 YES, ADD AN ENTRY                 07492014
                                                                        07493014
*-------------------------------------------------------------------*   07494014
*                                                                   *   07495014
*    WE DON'T HAVE ROOM IN THE TABLE TO INSERT THE NEW ENTRY, SO    *   07496014
*   WE GETMAIN A NEW TABLE TWICE THE SIZE, AND USE IT INSTEAD.      *   07497014
*                                                                   *   07498014
*-------------------------------------------------------------------*   07499014
                                                                        07500014
         L     R15,@_ODT_Ptr          GET A(CURRENT TABLE)              07501014
         L     R1,ODT_Subp_Len-OutDesc_Tbl(R15) GET SUBPOOL, LENGTH     07502014
         SLDL  R0,8                   SUBPOOL IN R0                     07503014
         SLL   R0,24                  HIGH-ORDER BYTE OF R0             07504014
         SRL   R1,7                   DOUBLE PREVIOUS SIZE              07505014
         OR    R0,R1                  SET UP GETMAIN REGISTER           07506014
         STORAGE OBTAIN,                Go get our storage             +07507014
               LENGTH=(0),               this long                     +07508014
               LOC=ANY                   anywhere                       07509014
         ST    R0,@HI_ODT_Subp_Len    SAVE SUBPOOL, LENGTH              07510014
         ST    R1,@HI_ODT_Pointer      AND ADDRESS                      07511014
         LR    R0,R1                  R0 = A(NEW TABLE)                 07512014
         L     R1,@HI_ODT_Subp_Len    R1 = NEW TABLE LENGTH             07513014
         L     R14,@_ODT_Ptr          R14 = A(OLD TABLE)                07514014
         L     R15,ODT_Subp_Len-OutDesc_Tbl(R14) R15 = LENGTH           07515014
         SLL   R15,8                  FILL CHAR                         07516014
         SRL   R15,8                   OF X'00'                         07517014
         MVCL  R0,R14                 MOVE OLD TABLE TO NEW             07518014
         L     R1,@_ODT_Ptr           A(OLD TABLE)                      07519014
         L     R0,ODT_Subp_Len-OutDesc_Tbl(R1) IT'S SUBPOOL, LENGTH     07520014
         STORAGE RELEASE,               Free our storage               +07521014
               ADDR=(1),                 starting here                 +07522014
               LENGTH=(0)                for this long                  07523014
         L     R1,@HI_ODT_Pointer     A(NEW TABLE)                      07524014
         ST    R1,@_ODT_Ptr           SAVE IT                           07525014
         L     R0,@HI_ODT_Subp_Len    NEW TABLE LENGTH, SUBPOOL         07526014
         ST    R0,ODT_Subp_Len-OutDesc_Tbl(R1) SAVE IT IN THE TABLE     07527014
         L     R15,=AL4(ODTE_Length)   LENGTH OF ONE ENTRY              07528014
         MH    R15,ODT_Entry_Count-OutDesc_Tbl(R1) Len of entries       07529014
         LA    R10,ODT_Entry-OutDesc_Tbl(0,R15) PLUS LEN OF HEADER      07530014
         A     R10,@_ODT_Ptr         R10 = A(NEW ENTRY AREA)            07531014
HI0050   DS    0H                                                       07532014
                                                                        07533014
*-------------------------------------------------------------------*   07534014
*                                                                   *   07535014
*    INITIALIZE OUR NEW ENTRY WITH THE DEFAULT (FIRST) ENTRY.       *   07536014
*       R10 --> NEW ENTRY AREA.                                     *   07537014
*                                                                   *   07538014
*-------------------------------------------------------------------*   07539014
                                                                        07540014
         LR    R15,R10                SAVE A(NEW ENTRY)                 07541014
         L     R1,@_ODT_Ptr           A(ENV TABLE)                      07542014
         LA    R0,ODT_Entry-OutDesc_Tbl(R1) A(FIRST ENTRY)              07543014
         L     R1,=AL4(ODTE_Length)   ENTRY LENGTH                      07544014
         LR    R14,R15                A(NEW ENTRY)                      07545014
         LR    R15,R1                 SAME LENGTH                       07546014
         MVCL  R14,R0                 MOVE THE DEFAULT ENTRY            07547014
                                                                        07548014
*-------------------------------------------------------------------*   07549014
*                                                                   *   07550014
*    NOW SET UP OUR STUFF WITHIN OUR ENTRY.                         *   07551014
*                                                                   *   07552014
*-------------------------------------------------------------------*   07553014
                                                                        07554014
         MVC   ODTE_DDName,@HI_DDName SAVE OUR DDNAME                   07555014
         MVI   ODTE_InExit,C' '       CLEAR                             07556014
         MVC   ODTE_InExit+1(L'ODTE_InExit-1),ODTE_InExit INPUT AND     07557014
         MVC   ODTE_OutExit,ODTE_InExit      OUTPUT EXIT NAMES          07558014
         SLR   R15,R15                CLEAR WORK REG                    07559014
         ST    R15,ODTE_RTT_Ptr       NO RECORD TYPE TABLE              07560014
         ST    R15,ODTE_JNT_Ptr       NOR JOBNAME TABLE                 07561014
         ST    R15,ODTE_DT_Ptr        NO DATA TABLE EITHER              07562014
         L     R15,@_ODT_Ptr          A(ENVIRONMENT TABLE)              07563014
         LH    R14,ODT_Entry_Count-OutDesc_Tbl(R15) Nbr entries         07564014
         LA    R14,1(0,R14)           BUMP IT                           07565014
         STH   R14,ODT_Entry_Count-OutDesc_Tbl(R15) AND SAVE IT         07566014
HI0060   DS    0H                                                       07567014
HI0070   DS    0H                                                       07568022
         L     R1,@HI_Save+4         A(HSA)                             07569014
         L     R1,24(0,R1)           A(Original Parm list)              07570014
         ST    R10,8(0,R1)           Update A(ODT Entry)                07571014
         SLR   R15,R15                CLEAR THE RETURN CODE             07572014
         B     HI9000                 AND EXIT                          07573014
HI9000   DS    0H                                                       07574014
                                                                        07575014
*-------------------------------------------------------------------*   07576014
*                                                                   *   07577014
*    Free up our local storage ...                                  *   07578014
*                                                                   *   07579014
*-------------------------------------------------------------------*   07580014
                                                                        07581014
         L     R1,4(0,R13)              A(HSA)                          07582014
         L     R1,24(0,R1)              Get original R1                 07583014
         STM   R2,R3,0(R1)              Update parms                    07584014
                                                                        07585014
         LR    R3,R15                   Rescue the return code          07586014
                                                                        07587014
         LA    R1,@HI_Dynam             A(Local storage)                07588014
         L     R13,4(0,R13)             Rescue A(HSA)                   07589014
                                                                        07590014
         STACK POP,                     Free the stack area            +07591014
               ADDR=(R1),                starting here                 +07592014
               STACK=@_31Bit_Stack_Ptr   on this stack                  07593014
                                                                        07594014
*-------------------------------------------------------------------*   07595014
*                                                                   *   07596014
*    ... and return to caller                                       *   07597014
*                                                                   *   07598014
*-------------------------------------------------------------------*   07599014
                                                                        07600014
         LR    R15,R3                   Restore return code             07601014
         L     R14,12(0,R13)            Restore return address          07602014
         LM    R0,R12,20(R13)           Restore other registers         07603014
         BSM   0,R14                    and return                      07604014
                                                                        07605014
         LTORG                                                          07606014
                                                                        07607014
@HI_Dynam       DSECT                 Dynamic area for Q                07608014
@HI_Save        DS    18F              O/S Style save area              07609014
@HI_DDName      DS    CL8              DDName for this ODTE             07610014
@HI_ODT_Pointer DS    AL4              A(Output Desc Tbl)               07611014
@HI_ODT_Subp_Len DS   FL4              Subpool, length of ODT           07612014
                DS    0D               Alignment                        07613014
@HI_DynLen      EQU   *-@HI_Dynam     Length of storage required        07614014
                                                                        07615014
SMFSLCT  RSECT                                                          07616014
                                                                        07617014
         DROP  R10                                                      07618014
         DROP  R11,R13                                                  07619014
         POP   USING                                                    07620014
         EJECT                                                          07621014
                                                                        07622014
         PUSH  USING                                                    07623014
                                                                        07624014
HJ0010   DS    0H                                                       07625014
                                                                        07626014
*-------------------------------------------------------------------*   07627014
*                                                                   *   07628014
*    'PERIOD=' Keyword Processor                                    *   07629014
*    On Entry, R1 --->  A(Operand)                                  *   07630014
*                       Fullword containing # characters left in    *   07631014
*                        the input record                           *   07632014
*                       A(ODT Entry)                                *   07633014
*    On Exit,  R15 =   0 - All OK, ODTE_PeriodStart and             *   07634014
*                          ODTE_PeriodEnd updated.                  *   07635014
*                      4 - Error found                              *   07636014
*                                                                   *   07637014
*-------------------------------------------------------------------*   07638014
                                                                        07639014
         USING *,R15                                                    07640014
         SAVE  (14,12),T                                                07641014
         LR    R11,R15                  Load permanent base reg         07642014
         DROP  R15                      Free up temp base               07643014
         USING HJ0010,R11               Assign permanent base           07644014
                                                                        07645014
*-------------------------------------------------------------------*   07646014
*                                                                   *   07647014
*    Get an area on the stack for our local storage                 *   07648014
*                                                                   *   07649014
*-------------------------------------------------------------------*   07650014
                                                                        07651014
         STACK PUSH,                    Get Stack area                 +07652014
               LEN=@HJ_DynLen,           this long                     +07653014
               STACK=@_31Bit_Stack_Ptr   using this stack               07654014
                                                                        07655014
         LR    R2,R1                    Save its address                07656014
         LR    R0,R1                    A(Our storage)                  07657014
         LA    R1,@HJ_DynLen             Its length                     07658014
         SLR   R14,R14                  Clear source address            07659014
         SLR   R15,R15                   and length                     07660014
         MVCL  R0,R14                   Clear our storage               07661014
         LR    R1,R2                    Restore A(Our storage)          07662014
                                                                        07663014
*-------------------------------------------------------------------*   07664014
*                                                                   *   07665014
*    Chain our save areas, and restore the important registers      *   07666014
*   that we have destroyed                                          *   07667014
*                                                                   *   07668014
*-------------------------------------------------------------------*   07669014
                                                                        07670014
         ST    R13,4(0,R1)              Chain                           07671014
         ST    R1,8(0,R13)               saveareas                      07672014
         LR    R13,R1                   Load dynam base                 07673014
         USING @HJ_Dynam,R13            Assign a base                   07674014
         L     R15,@HJ_Dynam+4          Get A(HSA)                      07675014
         LM    R0,R3,20(R15)            Restore callers registers       07676014
                                                                        07677014
*-------------------------------------------------------------------*   07678014
*                                                                   *   07679014
*    Load our parm registers:                                       *   07680014
*         R2 ---> Operand of keyword                                *   07681014
*         R3    = Nbr chars left in the input record                *   07682014
*                                                                   *   07683014
*-------------------------------------------------------------------*   07684014
                                                                        07685014
         LM    R2,R3,0(R1)              Get parms                       07686014
                                                                        07687014
         L     R10,8(0,R1)              A(ODT Entry)                    07688014
                                                                        07689014
         USING ODT_Entry,R10            Tell the assembler              07690014
                                                                        07691014
*-------------------------------------------------------------------*   07692014
*                                                                   *   07693014
*    BEGIN WITH PERIOD START TIME.                                  *   07694014
*                                                                   *   07695014
*-------------------------------------------------------------------*   07696014
                                                                        07697014
         LA    R1,ODTE_PeriodStart    A(PERIOD START)                   07698014
HJ0020   DS    0H                                                       07699014
         MVC   @HJ_Time(6),=C'000000' INIT TEMP AREA                    07700014
         LA    R4,@HJ_Time            AND POINT TO IT                   07701014
HJ0030   DS    0H                                                       07702014
         CLI   0(R2),C'0'             IS THIS NUMERIC?                  07703014
         BL    HJ0040                 NO, SKIP                          07704014
         CLI   0(R2),C'9'             SO FAR, MAKE SURE                 07705014
         BH    HJ0040                 NOT NUMERIC, SKIP                 07706014
         MVC   0(1,R4),0(R2)          ITS NUMERIC, MOVE IT              07707014
         LA    R4,1(0,R4)             BUMP OUTPUT POINTER               07708014
         LA    R2,1(0,R2)             BUMP INPUT POINTER                07709014
         BCT   R3,HJ0030              AND TRY AGAIN                     07710014
HJ0040   DS    0H                                                       07711014
                                                                        07712014
*-------------------------------------------------------------------*   07713014
*                                                                   *   07714014
*    WE HAVE REACHED THE END OF THE TIME FIELD.                     *   07715014
*                                                                   *   07716014
*-------------------------------------------------------------------*   07717014
                                                                        07718014
         LA    R15,@HJ_Time          A(Start of Time)                   07719014
         SR    R4,R15                LENGTH OF DATA                     07720014
         BZ    HJ0050                NOTHING THERE, CHECK IT OUT        07721014
         CH    R4,=H'6'              TOO LONG?                          07722014
         BH    HJ8000                YES, ERROR                         07723014
                                                                        07724014
         LR    R4,R1                 Save A(Target field)               07725014
         XC    @HJ_DateConv_Area,@HJ_DateConv_Area Clear Date area      07726014
         PACK  @HJ_DBLWD,@HJ_Time(2)  PACK HH                           07727014
         CVB   R15,@HJ_DBLWD          BINARIZE IT                       07728014
         STC   R15,@HJ_DateConv_Area+DConv_Time_hh-DateConv_Area        07729014
         PACK  @HJ_DBLWD,@HJ_Time+2(2) GET NBR MINUTES                  07730014
         CVB   R15,@HJ_DBLWD          BINARIZE THEM                     07731014
         STC   R15,@HJ_DateConv_Area+DConv_Time_mm-DateConv_Area        07732014
         PACK  @HJ_DBLWD,@HJ_Time+4(2) PACK SECONDS                     07733014
         CVB   R15,@HJ_DBLWD          BINARIZE THEM                     07734014
         STC   R15,@HJ_DateConv_Area+DConv_Time_ss-DateConv_Area        07735014
         OI    @HJ_DateConv_Area+DConv_Input_hhmmss-DateConv_Area,L'DCo+07736014
               nv_Input_hhmmss       Indicate Time conversion           07737014
         LA    R1,@HJ_DateConv_Area  Point to Date Conversion area      07738014
         L     R15,=AL4(K0010)       A(Format routine)                  07739014
         BASR  R14,R15               Go get seconds since midnight      07740014
         LTR   R15,R15               Everything OK?                     07741014
         BNZ   HJ8000                No, invalid time                   07742014
                                                                        07743014
         LR    R1,R4                 Restore A(Target field)            07744014
         L     R15,@HJ_DateConv_Area+DConv_Time-DateConv_Area           07745014
         ST    R15,0(0,R1)           OK, SAVE IT                        07746014
         LA    R15,ODTE_PeriodEnd    A(PERIOD END TIME)                 07747014
         CR    R1,R15                IS THAT WHAT WE JUST DID?          07748014
         BE    HJ0050                YES, SKIP                          07749014
         CLI   0(R2),C'-'            IS THIS THE TIME SEPARATOR?        07750014
         BNE   HJ0050                NO, SKIP                           07751014
         LA    R1,ODTE_PeriodEnd     YES, A(PERIOD END TIME)            07752014
         LA    R2,1(0,R2)            POINT PAST SEPARATOR               07753014
         BCT   R3,HJ0020             AND PROCESS IT                     07754014
HJ0050   DS    0H                                                       07755014
                                                                        07756014
*-------------------------------------------------------------------*   07757014
*                                                                   *   07758014
*    WE HAVE THE END OF THE PARAMETERS - CHECK THAT WE TERMINATE    *   07759014
*   WITH A SPACE OR COMMA, AND THAT PERIOD START IS LESS THAN       *   07760014
*   PERIOD END.                                                     *   07761014
*                                                                   *   07762014
*-------------------------------------------------------------------*   07763014
                                                                        07764014
         CLI   0(R2),C' '            TERMINATING SPACE?                 07765014
         BE    HJ0060                YES, SKIP                          07766014
         CLI   0(R2),C','            NO, HOW ABOUT COMMA?               07767014
         BNE   HJ8000                NO, ERROR                          07768014
HJ0060   DS    0H                                                       07769014
         L     R15,ODTE_PeriodStart  GET PERIOD START TIME              07770014
         C     R15,ODTE_PeriodEnd    COMPARE TO PERIOD END              07771014
         BH    HJ8000                START > END, ERROR                 07772014
         SLR   R15,R15               OK, CLEAR THE RETURN CODE          07773014
         B     HJ9000                AND EXIT                           07774014
HJ8000   DS    0H                                                       07775014
                                                                        07776014
*-------------------------------------------------------------------*   07777014
*                                                                   *   07778014
*    WE HAVE AN ERROR, SET THE RETURN CODE.                         *   07779014
*                                                                   *   07780014
*-------------------------------------------------------------------*   07781014
                                                                        07782014
         LA    R15,4                 SET THE RETURN CODE                07783014
         B     HJ9000                AND EXIT                           07784014
HJ9000   DS    0H                                                       07785014
                                                                        07786014
*-------------------------------------------------------------------*   07787014
*                                                                   *   07788014
*    Free up our local storage ...                                  *   07789014
*                                                                   *   07790014
*-------------------------------------------------------------------*   07791014
                                                                        07792014
         L     R1,4(0,R13)              A(HSA)                          07793014
         L     R1,24(0,R1)              Get original R1                 07794014
         STM   R2,R3,0(R1)              Update parms                    07795014
                                                                        07796014
         LR    R3,R15                   Rescue the return code          07797014
                                                                        07798014
         LA    R1,@HJ_Dynam             A(Local storage)                07799014
         L     R13,4(0,R13)             Rescue A(HSA)                   07800014
                                                                        07801014
         STACK POP,                     Free the stack area            +07802014
               ADDR=(R1),                starting here                 +07803014
               STACK=@_31Bit_Stack_Ptr   on this stack                  07804014
                                                                        07805014
*-------------------------------------------------------------------*   07806014
*                                                                   *   07807014
*    ... and return to caller                                       *   07808014
*                                                                   *   07809014
*-------------------------------------------------------------------*   07810014
                                                                        07811014
         LR    R15,R3                   Restore return code             07812014
         L     R14,12(0,R13)            Restore return address          07813014
         LM    R0,R12,20(R13)           Restore other registers         07814014
         BSM   0,R14                    and return                      07815014
                                                                        07816014
         LTORG                                                          07817014
                                                                        07818014
@HJ_Dynam       DSECT                 Dynamic area for HJ               07819014
@HJ_Save        DS    18F              O/S Style save area              07820014
@HJ_DBLWD       DS    D                Work area                        07821014
@HJ_Time        DS    XL6              Work area                        07822014
@HJ_DateConv_Area DS   CL(DConv_Area_Length)     Our area               07823014
                DS    0D               Alignment                        07824014
@HJ_DynLen      EQU   *-@HJ_Dynam     Length of storage required        07825014
                                                                        07826014
SMFSLCT  RSECT                                                          07827014
                                                                        07828014
         DROP  R10                                                      07829014
         DROP  R11,R13                                                  07830014
         POP   USING                                                    07831014
         EJECT                                                          07832014
                                                                        07833014
         PUSH  USING                                                    07834014
                                                                        07835014
HK0010   DS    0H                                                       07836014
                                                                        07837014
*-------------------------------------------------------------------*   07838014
*                                                                   *   07839014
*    'SEQUENCE=' Keyword Processor                                  *   07840014
*    On Entry, R1 --->  A(Operand)                                  *   07841014
*                       Fullword containing # characters left in    *   07842014
*                        the input record                           *   07843014
*                       A(ODT Entry)                                *   07844014
*    On Exit,  R15 =   0 - All OK, @_SYSUT1_Sequence flag updated.  *   07845014
*                      4 - Error found                              *   07846014
*                                                                   *   07847014
*-------------------------------------------------------------------*   07848014
                                                                        07849014
         USING *,R15                                                    07850014
         SAVE  (14,12),T                                                07851014
         LR    R11,R15                  Load permanent base reg         07852014
         DROP  R15                      Free up temp base               07853014
         USING HK0010,R11               Assign permanent base           07854014
                                                                        07855014
*-------------------------------------------------------------------*   07856014
*                                                                   *   07857014
*    Get an area on the stack for our local storage                 *   07858014
*                                                                   *   07859014
*-------------------------------------------------------------------*   07860014
                                                                        07861014
         STACK PUSH,                    Get Stack area                 +07862014
               LEN=@HK_DynLen,           this long                     +07863014
               STACK=@_31Bit_Stack_Ptr   using this stack               07864014
                                                                        07865014
         LR    R2,R1                    Save its address                07866014
         LR    R0,R1                    A(Our storage)                  07867014
         LA    R1,@HK_DynLen             Its length                     07868014
         SLR   R14,R14                  Clear source address            07869014
         SLR   R15,R15                   and length                     07870014
         MVCL  R0,R14                   Clear our storage               07871014
         LR    R1,R2                    Restore A(Our storage)          07872014
                                                                        07873014
*-------------------------------------------------------------------*   07874014
*                                                                   *   07875014
*    Chain our save areas, and restore the important registers      *   07876014
*   that we have destroyed                                          *   07877014
*                                                                   *   07878014
*-------------------------------------------------------------------*   07879014
                                                                        07880014
         ST    R13,4(0,R1)              Chain                           07881014
         ST    R1,8(0,R13)               saveareas                      07882014
         LR    R13,R1                   Load dynam base                 07883014
         USING @HK_Dynam,R13            Assign a base                   07884014
         L     R15,@HK_Dynam+4          Get A(HSA)                      07885014
         LM    R0,R3,20(R15)            Restore callers registers       07886014
                                                                        07887014
*-------------------------------------------------------------------*   07888014
*                                                                   *   07889014
*    Load our parm registers:                                       *   07890014
*         R2 ---> Operand of keyword                                *   07891014
*         R3    = Nbr chars left in the input record                *   07892014
*                                                                   *   07893014
*-------------------------------------------------------------------*   07894014
                                                                        07895014
         LM    R2,R3,0(R1)              Get parms                       07896014
                                                                        07897014
*-------------------------------------------------------------------*   07898014
*                                                                   *   07899014
*    CHECK FOR "YES" KEYWORD.                                       *   07900014
*                                                                   *   07901014
*-------------------------------------------------------------------*   07902014
                                                                        07903014
         CH    R3,=H'3'               ENOUGH ROOM LEFT FOR LITERAL?     07904014
         BNH   HK8000                 NO, ERROR                         07905014
         CLC   0(3,R2),=C'YES'        YES, IS THIS IT?                  07906014
         BNE   HK8000                 NO, ERROR                         07907014
         OI    @_SYSUT1_Sequence,L'@_SYSUT1_Sequence YES, SET FLAG      07908014
         LA    R2,3(0,R2)             POINT PAST LITERAL                07909014
         SH    R3,=H'3'               RESET LENGTH REMAINING            07910014
         SLR   R15,R15                ASSUME EVERYTHING IS OK           07911014
         CLI   0(R2),C' '             VALID SEPARATOR?                  07912014
         BE    HK9000                 YES, OK                           07913014
         CLI   0(R2),C','             NO, TRY AGAIN                     07914014
         BE    HK9000                 YES, SKIP                         07915014
         B     HK8000                 NO, ERROR                         07916014
HK8000   DS    0H                                                       07917014
                                                                        07918014
*-------------------------------------------------------------------*   07919014
*                                                                   *   07920014
*    SOME SORT OF ERROR FOUND, SET RETURN CODE                      *   07921014
*                                                                   *   07922014
*-------------------------------------------------------------------*   07923014
                                                                        07924014
         LA    R15,4                  FLAG AN ERROR                     07925014
         B     HK9000                 AND EXIT                          07926014
HK9000   DS    0H                                                       07927014
                                                                        07928014
*-------------------------------------------------------------------*   07929014
*                                                                   *   07930014
*    Free up our local storage ...                                  *   07931014
*                                                                   *   07932014
*-------------------------------------------------------------------*   07933014
                                                                        07934014
         L     R1,4(0,R13)              A(HSA)                          07935014
         L     R1,24(0,R1)              Get original R1                 07936014
         STM   R2,R3,0(R1)              Update parms                    07937014
                                                                        07938014
         LR    R3,R15                   Rescue the return code          07939014
                                                                        07940014
         LA    R1,@HK_Dynam             A(Local storage)                07941014
         L     R13,4(0,R13)             Rescue A(HSA)                   07942014
                                                                        07943014
         STACK POP,                     Free the stack area            +07944014
               ADDR=(R1),                starting here                 +07945014
               STACK=@_31Bit_Stack_Ptr   on this stack                  07946014
                                                                        07947014
*-------------------------------------------------------------------*   07948014
*                                                                   *   07949014
*    ... and return to caller                                       *   07950014
*                                                                   *   07951014
*-------------------------------------------------------------------*   07952014
                                                                        07953014
         LR    R15,R3                   Restore return code             07954014
         L     R14,12(0,R13)            Restore return address          07955014
         LM    R0,R12,20(R13)           Restore other registers         07956014
         BSM   0,R14                    and return                      07957014
                                                                        07958014
         LTORG                                                          07959014
                                                                        07960014
@HK_Dynam       DSECT                 Dynamic area for S                07961014
@HK_Save        DS    18F              O/S Style save area              07962014
                DS    0D               Alignment                        07963014
@HK_DynLen      EQU   *-@HK_Dynam     Length of storage required        07964014
                                                                        07965014
SMFSLCT  RSECT                                                          07966014
                                                                        07967014
         DROP  R11,R13                                                  07968014
         POP   USING                                                    07969014
         EJECT                                                          07970014
                                                                        07971014
         PUSH  USING                                                    07972014
                                                                        07973014
HL0010   DS    0H                                                       07974014
                                                                        07975014
*-------------------------------------------------------------------*   07976014
*                                                                   *   07977014
*    'CHECKVBS=' Keyword Processor                                  *   07978014
*    On Entry, R1 --->  A(Operand)                                  *   07979014
*                       Fullword containing # characters left in    *   07980014
*                        the input record                           *   07981014
*                       A(ODT Entry)                                *   07982014
*    On Exit,  R15 =   0 - All OK, @_SYSUT1_NoVBSChk flag set       *   07983014
*                      4 - Error found                              *   07984014
*                                                                   *   07985014
*-------------------------------------------------------------------*   07986014
                                                                        07987014
         USING *,R15                                                    07988014
         SAVE  (14,12),T                                                07989014
         LR    R11,R15                  Load permanent base reg         07990014
         DROP  R15                      Free up temp base               07991014
         USING HL0010,R11               Assign permanent base           07992014
                                                                        07993014
*-------------------------------------------------------------------*   07994014
*                                                                   *   07995014
*    Get an area on the stack for our local storage                 *   07996014
*                                                                   *   07997014
*-------------------------------------------------------------------*   07998014
                                                                        07999014
         STACK PUSH,                    Get Stack area                 +08000014
               LEN=@HL_DynLen,           this long                     +08001014
               STACK=@_31Bit_Stack_Ptr   using this stack               08002014
                                                                        08003014
         LR    R2,R1                    Save its address                08004014
         LR    R0,R1                    A(Our storage)                  08005014
         LA    R1,@HL_DynLen             Its length                     08006014
         SLR   R14,R14                  Clear source address            08007014
         SLR   R15,R15                   and length                     08008014
         MVCL  R0,R14                   Clear our storage               08009014
         LR    R1,R2                    Restore A(Our storage)          08010014
                                                                        08011014
*-------------------------------------------------------------------*   08012014
*                                                                   *   08013014
*    Chain our save areas, and restore the important registers      *   08014014
*   that we have destroyed                                          *   08015014
*                                                                   *   08016014
*-------------------------------------------------------------------*   08017014
                                                                        08018014
         ST    R13,4(0,R1)              Chain                           08019014
         ST    R1,8(0,R13)               saveareas                      08020014
         LR    R13,R1                   Load dynam base                 08021014
         USING @HL_Dynam,R13            Assign a base                   08022014
         L     R15,@HL_Dynam+4          Get A(HSA)                      08023014
         LM    R0,R3,20(R15)            Restore callers registers       08024014
                                                                        08025014
*-------------------------------------------------------------------*   08026014
*                                                                   *   08027014
*    Load our parm registers:                                       *   08028014
*         R2 ---> Operand of keyword                                *   08029014
*         R3    = Nbr chars left in the input record                *   08030014
*                                                                   *   08031014
*-------------------------------------------------------------------*   08032014
                                                                        08033014
         LM    R2,R3,0(R1)              Get parms                       08034014
                                                                        08035014
*-------------------------------------------------------------------*   08036014
*                                                                   *   08037014
*    CHECK FOR "NO" KEYWORD.                                        *   08038014
*                                                                   *   08039014
*-------------------------------------------------------------------*   08040014
                                                                        08041014
         CH    R3,=H'2'               ENOUGH ROOM LEFT FOR LITERAL?     08042014
         BNH   HL8000                 NO, ERROR                         08043014
         CLC   0(2,R2),=C'NO'         YES, IS THIS IT?                  08044014
         BNE   HL8000                 NO, ERROR                         08045014
         OI    @_SYSUT1_NoVBSChk,L'@_SYSUT1_NoVBSChk YES, SET FLAG      08046014
         LA    R2,2(0,R2)             POINT PAST LITERAL                08047014
         SH    R3,=H'2'               RESET LENGTH REMAINING            08048014
         SLR   R15,R15                ASSUME EVERYTHING IS OK           08049014
         CLI   0(R2),C' '             VALID SEPARATOR?                  08050014
         BE    HL9000                 YES, OK                           08051014
         CLI   0(R2),C','             NO, TRY AGAIN                     08052014
         BE    HL9000                 YES, SKIP                         08053014
         B     HL8000                 NO, ERROR                         08054014
HL8000   DS    0H                                                       08055014
                                                                        08056014
*-------------------------------------------------------------------*   08057014
*                                                                   *   08058014
*    SOME SORT OF ERROR FOUND, SET RETURN CODE                      *   08059014
*                                                                   *   08060014
*-------------------------------------------------------------------*   08061014
                                                                        08062014
         LA    R15,4                  FLAG AN ERROR                     08063014
         B     HL9000                 AND EXIT                          08064014
HL9000   DS    0H                                                       08065014
                                                                        08066014
*-------------------------------------------------------------------*   08067014
*                                                                   *   08068014
*    Free up our local storage ...                                  *   08069014
*                                                                   *   08070014
*-------------------------------------------------------------------*   08071014
                                                                        08072014
         L     R1,4(0,R13)              A(HSA)                          08073014
         L     R1,24(0,R1)              Get original R1                 08074014
         STM   R2,R3,0(R1)              Update parms                    08075014
                                                                        08076014
         LR    R3,R15                   Rescue the return code          08077014
                                                                        08078014
         LA    R1,@HL_Dynam             A(Local storage)                08079014
         L     R13,4(0,R13)             Rescue A(HSA)                   08080014
                                                                        08081014
         STACK POP,                     Free the stack area            +08082014
               ADDR=(R1),                starting here                 +08083014
               STACK=@_31Bit_Stack_Ptr   on this stack                  08084014
                                                                        08085014
*-------------------------------------------------------------------*   08086014
*                                                                   *   08087014
*    ... and return to caller                                       *   08088014
*                                                                   *   08089014
*-------------------------------------------------------------------*   08090014
                                                                        08091014
         LR    R15,R3                   Restore return code             08092014
         L     R14,12(0,R13)            Restore return address          08093014
         LM    R0,R12,20(R13)           Restore other registers         08094014
         BSM   0,R14                    and return                      08095014
                                                                        08096014
         LTORG                                                          08097014
                                                                        08098014
@HL_Dynam       DSECT                 Dynamic area for U                08099014
@HL_Save        DS    18F              O/S Style save area              08100014
                DS    0D               Alignment                        08101014
@HL_DynLen      EQU   *-@HL_Dynam     Length of storage required        08102014
                                                                        08103014
SMFSLCT  RSECT                                                          08104014
                                                                        08105014
         DROP  R11,R13                                                  08106014
         POP   USING                                                    08107014
         EJECT                                                          08108014
HM0010   DS    0H                                                       08109014
                                                                        08110014
         PUSH  USING                                                    08111014
                                                                        08112014
*-------------------------------------------------------------------*   08113014
*                                                                   *   08114014
*    'WEEKENDS=' Keyword Processor                                  *   08115014
*             Valid values are:                                     *   08116014
*                     "IGNORE" - Do not process Saturday or Sunday  *   08117014
*    On Entry, R1 --->  A(Operand)                                  *   08118014
*                       Fullword containing # characters left in    *   08119014
*                        the input record                           *   08120014
*                       A(ODT Entry)                                *   08121014
*    On Exit,  R15 =   0 - All OK, ODTE_Ignore_Weekend flag set     *   08122014
*                      4 - Invalid Parameter                        *   08123014
*                                                                   *   08124014
*-------------------------------------------------------------------*   08125014
                                                                        08126014
         USING *,R15                                                    08127014
         SAVE  (14,12),T                                                08128014
         LR    R11,R15                  Load permanent base reg         08129014
         DROP  R15                      Free up temp base               08130014
         USING HM0010,R11               Assign permanent base           08131014
                                                                        08132014
*-------------------------------------------------------------------*   08133014
*                                                                   *   08134014
*    Get an area on the stack for our local storage                 *   08135014
*                                                                   *   08136014
*-------------------------------------------------------------------*   08137014
                                                                        08138014
         STACK PUSH,                    Get Stack area                 +08139014
               LEN=@HM_DynLen,           this long                     +08140014
               STACK=@_31Bit_Stack_Ptr   using this stack               08141014
                                                                        08142014
         LR    R2,R1                    Save its address                08143014
         LR    R0,R1                    A(Our storage)                  08144014
         LA    R1,@HM_DynLen             Its length                     08145014
         SLR   R14,R14                  Clear source address            08146014
         SLR   R15,R15                   and length                     08147014
         MVCL  R0,R14                   Clear our storage               08148014
         LR    R1,R2                    Restore A(Our storage)          08149014
                                                                        08150014
*-------------------------------------------------------------------*   08151014
*                                                                   *   08152014
*    Chain our save areas, and restore the important registers      *   08153014
*   that we have destroyed                                          *   08154014
*                                                                   *   08155014
*-------------------------------------------------------------------*   08156014
                                                                        08157014
         ST    R13,4(0,R1)              Chain                           08158014
         ST    R1,8(0,R13)               saveareas                      08159014
         LR    R13,R1                   Load dynam base                 08160014
         USING @HM_Dynam,R13            Assign a base                   08161014
         L     R15,@HM_Dynam+4          Get A(HSA)                      08162014
         LM    R0,R3,20(R15)            Restore callers registers       08163014
                                                                        08164014
*-------------------------------------------------------------------*   08165014
*                                                                   *   08166014
*    Load our parm registers:                                       *   08167014
*         R2 ---> Operand of PRINT= keyword                         *   08168014
*         R3    = Nbr chars left in the input record                *   08169014
*                                                                   *   08170014
*-------------------------------------------------------------------*   08171014
                                                                        08172014
         LM    R2,R3,0(R1)              Get parms                       08173014
         L     R10,8(0,R1)              A(ODT Entry)                    08174014
                                                                        08175014
*-------------------------------------------------------------------*   08176014
*                                                                   *   08177014
*    CHECK FOR "NO" KEYWORD.                                        *   08178014
*                                                                   *   08179014
*-------------------------------------------------------------------*   08180014
                                                                        08181014
         CH    R3,=HL2'6'             ENOUGH ROOM LEFT FOR LITERAL?     08182014
         BNH   HM8000                 NO, ERROR                         08183014
         CLC   0(6,R2),=C'IGNORE'     YES, IS THIS IT?                  08184014
         BNE   HM8000                 NO, ERROR                         08185014
         OI    ODTE_Ignore_Weekend-ODT_Entry(R10),L'ODTE_Ignore_Weekend 08186014
         LA    R2,6(0,R2)             POINT PAST LITERAL                08187014
         SH    R3,=HL2'6'             RESET LENGTH REMAINING            08188014
         SLR   R15,R15                ASSUME EVERYTHING IS OK           08189014
         CLI   0(R2),C' '             VALID SEPARATOR?                  08190014
         BE    HM9000                 YES, OK                           08191014
         CLI   0(R2),C','             NO, TRY AGAIN                     08192014
         BE    HM9000                 YES, SKIP                         08193014
         B     HM8000                 NO, ERROR                         08194014
HM8000   DS    0H                                                       08195014
                                                                        08196014
*-------------------------------------------------------------------*   08197014
*                                                                   *   08198014
*    SOME SORT OF ERROR FOUND, SET RETURN CODE                      *   08199014
*                                                                   *   08200014
*-------------------------------------------------------------------*   08201014
                                                                        08202014
         LA    R15,4                  FLAG AN ERROR                     08203014
         B     HM9000                 AND EXIT                          08204014
HM9000   DS    0H                                                       08205014
                                                                        08206014
*-------------------------------------------------------------------*   08207014
*                                                                   *   08208014
*    Free up our local storage ...                                  *   08209014
*                                                                   *   08210014
*-------------------------------------------------------------------*   08211014
                                                                        08212014
         L     R1,4(0,R13)              A(HSA)                          08213014
         L     R1,24(0,R1)              Get original R1                 08214014
         STM   R2,R3,0(R1)              Update parms                    08215014
                                                                        08216014
         LR    R3,R15                   Rescue the return code          08217014
                                                                        08218014
         LA    R1,@HM_Dynam             A(Local storage)                08219014
         L     R13,4(0,R13)             Rescue A(HSA)                   08220014
                                                                        08221014
         STACK POP,                     Free the stack area            +08222014
               ADDR=(R1),                starting here                 +08223014
               STACK=@_31Bit_Stack_Ptr   on this stack                  08224014
                                                                        08225014
*-------------------------------------------------------------------*   08226014
*                                                                   *   08227014
*    ... and return to caller                                       *   08228014
*                                                                   *   08229014
*-------------------------------------------------------------------*   08230014
                                                                        08231014
         LR    R15,R3                   Restore return code             08232014
         L     R14,12(0,R13)            Restore return address          08233014
         LM    R0,R12,20(R13)           Restore other registers         08234014
         BSM   0,R14                    and return                      08235014
                                                                        08236014
                                                                        08237014
         LTORG                                                          08238014
                                                                        08239014
@HM_Dynam       DSECT                 Dynamic area for HM               08240014
@HM_Save        DS    18F              O/S Style save area              08241014
                DS    0D               Alignment                        08242014
@HM_DynLen      EQU   *-@HM_Dynam     Length of storage required        08243014
                                                                        08244014
SMFSLCT  RSECT                                                          08245014
                                                                        08246014
         DROP  R11,R13                                                  08247014
         POP   USING                                                    08248014
         TITLE 'I0000: OPEN SYSUT1'                                     08249014
                                                                        08250014
         PUSH  USING                                                    08251014
                                                                        08252014
I0010    DS    0H                                                       08253014
                                                                        08254014
                                                                        08255014
*---------------------------------------------------------------------* 08256014
*                                                                     * 08257014
*  Routine   : I0010                                                  * 08258014
*                                                                     * 08259014
*  Abstract  : OPEN SYSUT1, allocating it to the active SMF dataset   * 08260014
*              if required. Print some interesting(?) info about it.  * 08261014
*                                                                     * 08262014
*  Inputs    : N/A                                                    * 08263014
*  Outputs   : R15: 0 - OK                                            * 08264014
*                   4 - SYSUT1 not useable.                           * 08265014
*                                                                     * 08266014
*  Notes     :                                                        * 08267014
*                                                                     * 08268014
*  History:                                                           * 08269014
*              1999/04/24 SDDA030 V1.1                                * 08270014
*                                 Used LIFO Stack for our dynamic     * 08271014
*                                 areas.                              * 08272014
*              2002/02/07 SDDA030 V1.4                                * 08273014
*                                 Removed EODAD stuff.                * 08274014
*                                 Used W0010 to get A(JFCB).          * 08275014
*              ____/__/__ _______                                     * 08276014
*                                                                     * 08277014
*---------------------------------------------------------------------* 08278014
                                                                        08279014
         USING *,R15                                                    08280014
         SAVE  (14,12),T                                                08281014
         LR    R11,R15                  Load permanent base reg         08282014
         DROP  R15                      Free up temp base               08283014
         USING I0010,R11                Assign permanent base           08284014
                                                                        08285014
*-------------------------------------------------------------------*   08286014
*                                                                   *   08287014
*    Get an area on the stack for our local storage                 *   08288014
*                                                                   *   08289014
*-------------------------------------------------------------------*   08290014
                                                                        08291014
         STACK PUSH,                    Get Stack area                 +08292014
               LEN=@I_DynLen,            this long                     +08293014
               STACK=@_31Bit_Stack_Ptr   using this stack               08294014
                                                                        08295014
         LR    R2,R1                    Save its address                08296014
         LR    R0,R1                    A(Our storage)                  08297014
         LA    R1,@I_DynLen              Its length                     08298014
         SLR   R14,R14                  Clear source address            08299014
         SLR   R15,R15                   and length                     08300014
         MVCL  R0,R14                   Clear our storage               08301014
         LR    R1,R2                    Restore A(Our storage)          08302014
                                                                        08303014
*-------------------------------------------------------------------*   08304014
*                                                                   *   08305014
*    Chain our save areas, and restore the important registers      *   08306014
*   that we have destroyed                                          *   08307014
*                                                                   *   08308014
*-------------------------------------------------------------------*   08309014
                                                                        08310014
         ST    R13,4(0,R1)              Chain                           08311014
         ST    R1,8(0,R13)               saveareas                      08312014
         LR    R13,R1                   Load dynam base                 08313014
         USING @I_Dynam,R13             Assign a base                   08314014
         L     R15,@I_Dynam+4           Get A(HSA)                      08315014
         LM    R0,R3,20(R15)            Restore callers registers       08316014
                                                                        08317014
*-------------------------------------------------------------------*   08318014
*                                                                   *   08319014
*     Get the storage for our DCB and DCBE                          *   08320014
*                                                                   *   08321014
*-------------------------------------------------------------------*   08322014
                                                                        08323014
         LA    R1,4                     Length field                    08324014
         LA    R1,I_SYSUT1_BSAM_DCB_Length(0,R1) Plus length of DCB     08325014
         LA    R0,I_SYSUT1_DCBE_Length(0,R1) Plus length of DCBE        08326014
         STORAGE OBTAIN,                Go get our storage             +08327014
               LENGTH=(0),               this long                     +08328014
               LOC=BELOW                 below-the-line                 08329014
         ST    R1,@_SYSUT1_BtL_Ptr      Save A(Storage)                 08330014
         ST    R0,0(0,R1)               Save length                     08331014
         LA    R2,4(0,R1)               Point to DCB area               08332014
         MVC   0(I_SYSUT1_BSAM_DCB_Length,R2),I_SYSUT1_BSAM_DCB Move it 08333014
         LA    R14,I_SYSUT1_BSAM_DCB_Length(0,R2) Point past DCB        08334014
         MVC   0(I_SYSUT1_DCBE_Length,R14),I_SYSUT1_DCBE  Move DCBE     08335014
         ST    R14,DCBDCBE-IHADCB(0,R2) Save A(DCBE) in DCB             08336014
                                                                        08337014
*-------------------------------------------------------------------*   08338014
*                                                                   *   08339014
*    And go get the JFCB - if this fails, we assume that SYSUT1     *   08340014
*   has not been allocated                                          *   08341014
*                                                                   *   08342014
*-------------------------------------------------------------------*   08343014
                                                                        08344014
         L     R1,@_SYSUT1_BtL_Ptr      A(Storage)                      08345014
         LA    R1,4(0,R1)               Point to DCB area               08346014
         L     R15,=AL4(W0010)          A(Get JFCB routine)             08347014
         BASR  R14,R15                  Go do it                        08348014
         LTR   R15,R15                  How did it go?                  08349014
         BZ    I0060                    OK, skip                        08350014
                                                                        08351014
*-------------------------------------------------------------------*   08352014
*                                                                   *   08353014
*    No SYSUT1 DDName found, so dynamically allocate it to the      *   08354014
*   active SMF file, if there is one.                               *   08355014
*                                                                   *   08356014
*-------------------------------------------------------------------*   08357014
                                                                        08358014
         L     R15,CVTPTR(0,0)          Get A(CVT)                      08359014
         L     R15,CVTSMCA-CVT(R15)     And then the SMCA               08360014
         TM    SMCAMISC-SMCABASE(R15),SMCAUSER  SMF recording?          08361014
         BZ    I8000                    No, no active SMF file          08362014
         L     R14,SMCAFRDS-SMCABASE(R15) Yes, get A(1st RDS)           08363014
         L     R15,SMCALRDS-SMCABASE(R15) and A(last RDS)               08364014
I0040    DS    0H                                                       08365014
         TM    12(R14),X'04'            Active SMF dataset?             08366014
         BZ    I0050                    Yes, skip                       08367014
         L     R14,4(0,R14)             No, get A(Next)                 08368014
         CR    R14,R15                  End of chain?                   08369014
         BNE   I0040                    No, keep checking               08370014
         B     I8000                    Yes, no active SMF file         08371014
I0050    DS    0H                                                       08372014
                                                                        08373014
*-------------------------------------------------------------------*   08374014
*                                                                   *   08375014
*    We have found the active SMF file (R14 points to the RDS),     *   08376014
*   so build the SVC99 control blocks ...                           *   08377014
*                                                                   *   08378014
*-------------------------------------------------------------------*   08379014
                                                                        08380014
         LA    R15,@I_SVC99_RB          A(S99 RB)                       08381014
         ST    R15,@I_SVC99_RB_Ptr+S99RBPTR-S99RBP SAVE IT              08382014
         OI    @I_SVC99_RB_Ptr+S99RBPTR-S99RBP,S99RBPND                 08383014
                                                                        08384014
         MVI   @I_SVC99_RB+S99RBLN-S99RB,S99RBEND-S99RB LENGTH          08385014
         MVI   @I_SVC99_RB+S99VERB-S99RB,S99VRBAL ALLOCATE              08386014
         SLR   R15,R15                CLEAR WORK REGISTER               08387014
         STH   R15,@I_SVC99_RB+S99FLAG1-S99RB CLEAR FLAG1               08388014
         STH   R15,@I_SVC99_RB+S99ERROR-S99RB CLEAR ERROR CODE          08389014
         STH   R15,@I_SVC99_RB+S99INFO-S99RB CLEAR INFO CODE            08390014
         LA    R15,@I_SVC99_TU_List   A(TEXT UNIT POINTERS)             08391014
         ST    R15,@I_SVC99_RB+S99TXTPP-S99RB SAVE IT                   08392014
         SLR   R15,R15                CLEAR WORK REGISTER               08393014
         ST    R15,@I_SVC99_RB+S99TXTPP+4-S99RB CLEAR RESERVED FIELD    08394014
         ST    R15,@I_SVC99_RB+S99FLAG2-S99RB CLEAR FLAG 2              08395014
                                                                        08396014
         LA    R15,@I_SVC99_DDName_TU A(DDNAME)                         08397014
         ST    R15,@I_SVC99_TU_List   SAVE IT                           08398014
         LA    R15,@I_SVC99_DSName_TU A(DSNAME)                         08399014
         ST    R15,@I_SVC99_TU_List+4 SAVE IT                           08400014
         LA    R15,@I_SVC99_Status_TU A(STATUS)                         08401014
         ST    R15,@I_SVC99_TU_List+8 SAVE IT                           08402014
         LA    R15,@I_SVC99_NDisp_TU  A(NDISP)                          08403014
         ST    R15,@I_SVC99_TU_List+12 SAVE IT                          08404014
         LA    R15,@I_SVC99_CDisp_TU  A(CDISP)                          08405014
         ST    R15,@I_SVC99_TU_List+16 SAVE IT                          08406014
         LA    R15,@I_SVC99_Free_TU   A(FREE)                           08407014
         ST    R15,@I_SVC99_TU_List+20 SAVE IT                          08408014
         OI    @I_SVC99_TU_List+20,S99TUPLN END OF LIST                 08409014
                                                                        08410014
         LA    R15,DALDDNAM           DDNAME TEXT KEY                   08411014
         STH   R15,@I_SVC99_DDName_TU+S99TUKEY-S99TUNIT SAVE IT         08412014
         LA    R15,1                  NBR PARMS                         08413014
         STH   R15,@I_SVC99_DDName_TU+S99TUNUM-S99TUNIT SAVE IT         08414014
         LA    R15,8                  LEN OF PARMS                      08415014
         STH   R15,@I_SVC99_DDName_TU+S99TULNG-S99TUNIT SAVE IT         08416014
         MVC   @I_SVC99_DDName_TU+S99TUPAR-S99TUNIT(8),=CL8'SYSUT1'     08417014
                                                                        08418014
         LA    R15,DALDSNAM           DSNAME TEXT KEY                   08419014
         STH   R15,@I_SVC99_DSName_TU+S99TUKEY-S99TUNIT SAVE IT         08420014
         LA    R15,1                  NBR PARMS                         08421014
         STH   R15,@I_SVC99_DSName_TU+S99TUNUM-S99TUNIT SAVE IT         08422014
         LA    R15,44                 LEN OF PARMS                      08423014
         STH   R15,@I_SVC99_DSName_TU+S99TULNG-S99TUNIT SAVE IT         08424014
         MVC   @I_SVC99_DSName_TU+S99TUPAR-S99TUNIT(44),16(R14) SMF DSN 08425014
                                                                        08426014
         LA    R15,DALSTATS           STATUS TEXT KEY                   08427014
         STH   R15,@I_SVC99_Status_TU+S99TUKEY-S99TUNIT SAVE IT         08428014
         LA    R15,1                  NBR PARMS                         08429014
         STH   R15,@I_SVC99_Status_TU+S99TUNUM-S99TUNIT SAVE IT         08430014
         STH   R15,@I_SVC99_Status_TU+S99TULNG-S99TUNIT LENGTH OF 1 TOO 08431014
         MVI   @I_SVC99_Status_TU+S99TUPAR-S99TUNIT,X'08' SHR           08432014
                                                                        08433014
         LA    R15,DALNDISP           NORMAL DISP                       08434014
         STH   R15,@I_SVC99_NDisp_TU+S99TUKEY-S99TUNIT SAVE IT          08435014
         LA    R15,1                  NBR PARMS                         08436014
         STH   R15,@I_SVC99_NDisp_TU+S99TUNUM-S99TUNIT SAVE IT          08437014
         STH   R15,@I_SVC99_NDisp_TU+S99TULNG-S99TUNIT LENGTH OF 1 TOO  08438014
         MVI   @I_SVC99_NDisp_TU+S99TUPAR-S99TUNIT,X'08' KEEP           08439014
                                                                        08440014
         LA    R15,DALCDISP           CONDITIONAL DISP                  08441014
         STH   R15,@I_SVC99_CDisp_TU+S99TUKEY-S99TUNIT SAVE IT          08442014
         LA    R15,1                  NBR PARMS                         08443014
         STH   R15,@I_SVC99_CDisp_TU+S99TUNUM-S99TUNIT SAVE IT          08444014
         STH   R15,@I_SVC99_CDisp_TU+S99TULNG-S99TUNIT LENGTH OF 1 TOO  08445014
         MVI   @I_SVC99_CDisp_TU+S99TUPAR-S99TUNIT,X'08' KEEP           08446014
                                                                        08447014
         LA    R15,DALCLOSE           FREE=CLOSE                        08448014
         STH   R15,@I_SVC99_Free_TU+S99TUKEY-S99TUNIT SAVE IT           08449014
         SLR   R15,R15                NBR PARMS                         08450014
         STH   R15,@I_SVC99_Free_TU+S99TUNUM-S99TUNIT SAVE IT           08451014
                                                                        08452014
*-------------------------------------------------------------------*   08453014
*                                                                   *   08454014
*    ... and invoke Dynamic Allocation                              *   08455014
*                                                                   *   08456014
*-------------------------------------------------------------------*   08457014
                                                                        08458014
         LA    R1,@I_SVC99_RB_Ptr     A(SVC99 RB Pointer)               08459014
         SVC   99                     Go do it                          08460014
         LTR   R15,R15                Did it work?                      08461014
         BNZ   I8010                  No, error                         08462014
                                                                        08463014
         OI    @_SYSUT1_Sequence,L'@_SYSUT1_Sequence Yes, in sequence   08464014
                                                                        08465014
*-------------------------------------------------------------------*   08466014
*                                                                   *   08467014
*    And go get the JFCB (again)                                    *   08468014
*                                                                   *   08469014
*-------------------------------------------------------------------*   08470014
                                                                        08471014
         L     R1,@_SYSUT1_BtL_Ptr      A(Storage)                      08472014
         LA    R1,4(0,R1)               Point to DCB area               08473014
         L     R15,=AL4(W0010)          A(Get JFCB routine)             08474014
         BASR  R14,R15                  Go do it                        08475014
         LTR   R15,R15                  How did it go?                  08476014
         BNZ   I8020                    Not good, exit with error       08477014
                                                                        08478014
I0060    DS    0H                                                       08479014
                                                                        08480014
*-------------------------------------------------------------------*   08481014
*                                                                   *   08482014
*    Save some info from the JFCB                                   *   08483014
*                                                                   *   08484014
*-------------------------------------------------------------------*   08485014
                                                                        08486014
         ST    R1,@I_JFCB_Ptr           Save A(JFCB)                    08487014
                                                                        08488014
         MVC   @I_JFCBUFNO,JFCBUFNO-INFMJFCB(R1) Save DCB=BUFNO=        08489014
         MVC   @I_JFCNCP,JFCNCP-INFMJFCB(R1) Save DCB=NCP=              08490014
                                                                        08491014
*-------------------------------------------------------------------*   08492014
*                                                                   *   08493014
*    Get some info about the device on which SYSUT1 lives           *   08494014
*                                                                   *   08495014
*-------------------------------------------------------------------*   08496014
                                                                        08497014
         XC    @I_DVACLASS,@I_DVACLASS            Device Class          08498014
         XC    @I_DVATRK,@I_DVATRK                Trks/Cyl              08499014
         XC    @I_DVATRKLN,@I_DVATRKLN            Bytes/Trk             08500014
                                                                        08501014
         LA    R1,I_SYSUT1_BSAM_DCB+DCBDDNAM-IHADCB A(DDName)           08502014
         LA    R0,@I_DEVAREA          A(Return area)                    08503014
         DEVTYPE (1),(0),DEVTAB       Get SYSUT1 device info            08504014
         LTR   R15,R15                Did it work?                      08505014
         BNZ   I0070                  No, skip                          08506014
         MVC   @I_DVACLASS,@I_DEVAREA+DVACLASS-DVAREA Device Class      08507014
         MVC   @I_DVATRK,@I_DEVAREA+DVATRK-DVAREA Trks/Cyl              08508014
         MVC   @I_DVATRKLN,@I_DEVAREA+DVATRKLN-DVAREA Bytes/Trk         08509014
I0070    DS    0H                                                       08510014
                                                                        08511014
         L     R15,@I_JFCB_Ptr        A(JFCB)                           08512014
         TM    JFCDSRG2-INFMJFCB(R15),JFCORGAM VSAM?                    08513014
         BNZ   I0170                  Yes, skip                         08514014
                                                                        08515014
*-------------------------------------------------------------------*   08516014
*                                                                   *   08517014
*    SYSUT1 is non-VSAM; OPEN the BSAM DCB ...                      *   08518014
*                                                                   *   08519014
*-------------------------------------------------------------------*   08520014
                                                                        08521014
         L     R2,@_SYSUT1_BtL_Ptr    A(Below-the-line storage)         08522014
         LA    R2,4(0,R2)             A(DCB)                            08523014
                                                                        08524014
         MVC   @I_OPEN,I_OPEN         MOVE OPEN PARMS                   08525014
         OPEN  ((2),INPUT),MODE=31,MF=(E,@I_OPEN) AND OPEN BSAM DCB     08526014
         TM    DCBOFLGS-IHADCB(R2),DCBOFOPN Is it open?                 08527014
         BNO   I8020                  No, error                         08528014
                                                                        08529014
*-------------------------------------------------------------------*   08530014
*                                                                   *   08531014
*    ... save some info from the DCB ...                            *   08532014
*                                                                   *   08533014
*-------------------------------------------------------------------*   08534014
                                                                        08535014
         MVC   @I_RECFM,DCBRECFM-IHADCB(R2) Save RECFM                  08536014
         MVC   @I_NCP,DCBNCP-IHADCB(R2)      Save NCP (Calc by OPEN)    08537014
         LH    R15,DCBLRECL-IHADCB(R2)       Get LRECL                  08538014
         ST    R15,@_SYSUT1_Rec_Length       and save it                08539014
         MVC   @I_BLKSI,DCBBLKSI-IHADCB(R2)   and BLKSIZE               08540014
                                                                        08541014
*-------------------------------------------------------------------*   08542014
*                                                                   *   08543014
*    ... and close it.                                              *   08544014
*                                                                   *   08545014
*-------------------------------------------------------------------*   08546014
                                                                        08547014
         MVC   @I_CLOSE,I_CLOSE              Move CLOSE parms           08548014
         CLOSE ((2)),MODE=31,MF=(E,@I_CLOSE)  and close BSAM DCB        08549014
                                                                        08550014
*-------------------------------------------------------------------*   08551014
*                                                                   *   08552014
*    Calculate the value of DCBNCP or DCBBUFNO that we will         *   08553014
*   use so we can read a cylinder at a time (assuming SYSUT1 lives  *   08554014
*   on DASD - if it doesn't, we use an arbitrary value of 30). OPEN *   08555014
*   has calculated the number of blocks on a track (DCBNCP).        *   08556014
*                                                                   *   08557014
*-------------------------------------------------------------------*   08558014
                                                                        08559014
         LA    R0,30                  Default number of buffers         08560014
         STH   R0,@I_Buffer_Count     Save it                           08561014
                                                                        08562014
         TM    @I_DVACLASS,UCB3DACC   DASD device?                      08563014
         BZ    I0100                  No, use default                   08564014
         SLR   R0,R0                  Yes, get                          08565014
         IC    R0,@I_NCP               blocks/track                     08566014
         MH    R0,@I_DVATRK           Times Trks/Cyl                    08567014
         LTR   R0,R0                  Is it 0?                          08568014
         BZ    I0080                  Yes, use blks/trk                 08569014
         C     R0,=FL4'255'           No, too big?                      08570014
         BNH   I0090                  No, use it                        08571014
I0080    DS    0H                                                       08572014
         SLR   R0,R0                  Yes, use                          08573014
         IC    R0,@I_NCP               blocks/trk                       08574014
I0090    DS    0H                                                       08575014
         STH   R0,@I_Buffer_Count     Save it                           08576014
I0100    DS    0H                                                       08577014
                                                                        08578014
*-------------------------------------------------------------------*   08579014
*                                                                   *   08580014
*    We know we are non-VSAM. If the RECFM is not variable spanned, *   08581014
*   or we have been told not to check for spanning errors, we will  *   08582014
*   use QSAM as our access method. Otherwise ...                    *   08583014
*                                                                   *   08584014
*-------------------------------------------------------------------*   08585014
                                                                        08586014
         TM    @_SYSUT1_NoVBSChk,L'@_SYSUT1_NoVBSChk Should we check?   08587014
         BO    I0140                  No, skip                          08588014
         TM    @I_RECFM,DCBRECV+DCBRECSB     VS or VBS?                 08589014
         BNO   I0140                  No, skip                          08590014
                                                                        08591014
*-------------------------------------------------------------------*   08592014
*                                                                   *   08593014
*    ... we will use BSAM.                                          *   08594014
*                                                                   *   08595014
*-------------------------------------------------------------------*   08596014
                                                                        08597014
         OI    @_SYSUT1_BSAM,L'@_SYSUT1_BSAM Set BSAM Input flag        08598014
                                                                        08599014
*-------------------------------------------------------------------*   08600014
*                                                                   *   08601014
*    Calculate the number of buffers we will need. If this has not  *   08602014
*   been specified by the user (DCB=NCP= or DCB=BUFNO=), we use     *   08603014
*   the value we calculated earlier that will allow us to read a    *   08604014
*   cylinder at a time.                                             *   08605014
*                                                                   *   08606014
*-------------------------------------------------------------------*   08607014
                                                                        08608014
         SLR   R0,R0                  Clear work register               08609014
         IC    R0,@I_JFCNCP           Get user specified DCB=NCP=       08610014
         LTR   R0,R0                  Anything?                         08611014
         BNZ   I0110                  Yes, skip                         08612014
         IC    R0,@I_JFCBUFNO         No, get DCB=BUFNO=                08613014
         LTR   R0,R0                  Anything?                         08614014
         BZ    I0120                  No, use default                   08615014
I0110    DS    0H                                                       08616014
         STH   R0,@I_Buffer_Count     Yes, update our variable          08617014
I0120    DS    0H                                                       08618014
                                                                        08619014
*-------------------------------------------------------------------*   08620014
*                                                                   *   08621014
*    Get some storage above-the-line for our buffers and stuff:     *   08622014
*     +0     A(Next Buffer area)                                    *   08623014
*     +4     A(DECB)                                                *   08624014
*     +8     Offset within buffer of next segment                   *   08625014
*     +C     Buffer (length of DCBBLKSI)                            *   08626014
*                                                                   *   08627014
*-------------------------------------------------------------------*   08628014
                                                                        08629014
         LA    R1,BuffArea_Hdr_Length Header bytes                      08630014
         AH    R1,@I_BLKSI            Plus buffer area                  08631014
         LA    R1,7(0,R1)             Make it                           08632014
         SRL   R1,3                    its a                            08633014
         SLL   R1,3                    doubleword                       08634014
         ST    R1,@I_Buffer_Length    Save Buffer length                08635014
         LA    R2,8(0,R1)             Minimum storage we need           08636014
         LH    R0,@I_Buffer_Count     Number of buffers we need         08637014
         MR    R0,R0                  Total length                      08638014
         LA    R0,8(0,R1)             plus header length                08639014
         STORAGE OBTAIN,                Go get our storage             +08640014
               LENGTH=((0),(2)),         this long                     +08641014
               LOC=ANY                   anywhere                       08642014
         ST    R1,@_SYSUT1_AtL_Ptr    Save the address                  08643014
         ST    R0,0(0,R1)              and the length                   08644014
         LR    R15,R0                 Get the length                    08645014
         SH    R15,=H'8'               less the header length           08646014
         SLR   R14,R14                prepare for divide                08647014
         D     R14,@I_Buffer_Length   Calculate number of buffers       08648014
         STH   R15,@I_Buffer_Count    Save number of buffers            08649014
                                                                        08650014
*-------------------------------------------------------------------*   08651014
*                                                                   *   08652014
*    If we do not have enough room below-the-line for the DCB and   *   08653014
*   associated DECBs, free it and go get enough                     *   08654014
*                                                                   *   08655014
*-------------------------------------------------------------------*   08656014
                                                                        08657014
         LA    R1,I_READ_Length       Length of 1 DECB                  08658014
         MH    R1,@I_Buffer_Count     Length of all DECBs               08659014
         LA    R1,4+I_SYSUT1_BSAM_DCB_Length+I_SYSUT1_DCBE_Length(0,R1) 08660014
         L     R15,@_SYSUT1_BtL_Ptr   A(current storage)                08661014
         C     R1,0(0,R15)            Long enough?                      08662014
         BNH   I0130                  Yes, skip                         08663014
         ST    R1,@I_DBLWD            No, save length required          08664014
         L     R1,@_SYSUT1_BtL_Ptr    A(Our storage)                    08665014
         L     R0,0(0,R1)             Get its length                    08666014
         STORAGE RELEASE,             Free the storage                 +08667014
               LENGTH=(0),             this long                       +08668014
               ADDR=(1)                starting here                    08669014
         L     R0,@I_DBLWD            Get the amount we need            08670014
         STORAGE OBTAIN,                Go get our storage             +08671014
               LENGTH=(0),               this long                     +08672014
               LOC=BELOW                 below-the-line                 08673014
         ST    R0,0(0,R1)             Save the length                   08674014
         ST    R1,@_SYSUT1_BtL_Ptr    Save A(Storage)                   08675014
I0130    DS    0H                                                       08676014
         L     R2,@_SYSUT1_BtL_Ptr    A(Below-the-line storage)         08677014
         LA    R2,4(0,R2)             A(DCB area)                       08678014
         MVC   0(I_SYSUT1_BSAM_DCB_Length,R2),I_SYSUT1_BSAM_DCB Move it 08679014
                                                                        08680014
         LH    R15,@I_Buffer_Count    Nbr buffers                       08681014
         STC   R15,DCBNCP-IHADCB(R2)  Update NCP                        08682014
                                                                        08683014
         LA    R14,I_SYSUT1_BSAM_DCB_Length(0,R2) Point past DCB        08684014
         MVC   0(I_SYSUT1_DCBE_Length,R14),I_SYSUT1_DCBE  Move DCBE     08685014
         ST    R14,DCBDCBE-IHADCB(0,R2) Save A(DCBE) in DCB             08686014
                                                                        08687014
         ST    R2,@_SYSUT1_DCB_Ptr    Init DCB address                  08688014
                                                                        08689014
         B     I0190                  and skip                          08690014
                                                                        08691014
I0140    DS    0H                                                       08692014
                                                                        08693014
*-------------------------------------------------------------------*   08694014
*                                                                   *   08695014
*    SYSUT1 is non-VSAM and not RECFM=VS or VBS, so we will use     *   08696014
*   QSAM.                                                           *   08697014
*                                                                   *   08698014
*-------------------------------------------------------------------*   08699014
                                                                        08700014
         OI    @_SYSUT1_QSAM,L'@_SYSUT1_QSAM Set QSAM Input flag        08701014
                                                                        08702014
         SLR   R0,R0                  Clear work register               08703014
         IC    R0,@I_JFCBUFNO         Get user-specified DCB=BUFNO=     08704014
         LTR   R0,R0                  Anything?                         08705014
         BNZ   I0150                  Yes, skip                         08706014
         IC    R0,@I_JFCNCP           No, get DCB=NCP=                  08707014
         LTR   R0,R0                  Anything?                         08708014
         BZ    I0160                  No, skip                          08709014
I0150    DS    0H                                                       08710014
         STH   R0,@I_Buffer_Count     Yes, update our field             08711014
I0160    DS    0H                                                       08712014
         MVC   0(I_SYSUT1_QSAM_DCB_Length,R2),I_SYSUT1_QSAM_DCB         08713014
         LH    R15,@I_Buffer_Count    Blocks per cyl                    08714014
         STC   R15,DCBBUFNO-IHADCB(R2) Save it                          08715014
                                                                        08716014
         LA    R14,I_SYSUT1_QSAM_DCB_Length(0,R2) Point past DCB        08717014
         MVC   0(I_SYSUT1_DCBE_Length,R14),I_SYSUT1_DCBE  Move DCBE     08718014
         ST    R14,DCBDCBE-IHADCB(0,R2) Save A(DCBE) in DCB             08719014
                                                                        08720014
         ST    R2,@_SYSUT1_DCB_Ptr    Init DCB address                  08721014
         B     I0190                  and skip                          08722014
                                                                        08723014
I0170    DS    0H                                                       08724014
                                                                        08725014
*-------------------------------------------------------------------*   08726014
*                                                                   *   08727014
*    SYSUT1 is VSAM, so set our flag ...                            *   08728014
*                                                                   *   08729014
*-------------------------------------------------------------------*   08730014
                                                                        08731014
         OI    @_SYSUT1_VSAM,L'@_SYSUT1_VSAM  Set VSAM Input flag       08732014
                                                                        08733014
*-------------------------------------------------------------------*   08734014
*                                                                   *   08735014
*    ... save some VSAM-related info from the JFCB ...              *   08736014
*                                                                   *   08737014
*-------------------------------------------------------------------*   08738014
                                                                        08739014
         L     R15,@I_JFCB_Ptr          A(JFCB)                         08740014
                                                                        08741014
         SLR   R0,R0                    Clear work register             08742014
         L     R0,JFCBABFS+1-INFMJFCB(R15) Get BUFSP                    08743014
         SRL   R0,8                     Align it properly               08744014
         ST    R0,@I_JFCBABFS           Save it                         08745014
         SLR   R0,R0                    Clear work register             08746014
         LH    R0,JFCBADBF-INFMJFCB(R15) Get BUFND                      08747014
         STH   R0,@I_JFCBADBF           Save it                         08748014
                                                                        08749014
*-------------------------------------------------------------------*   08750014
*                                                                   *   08751014
*    ... free the below-the-line storage ...                        *   08752014
*                                                                   *   08753014
*-------------------------------------------------------------------*   08754014
                                                                        08755014
         L     R1,@_SYSUT1_BtL_Ptr      A(Below-the-line storage)       08756014
         L     R0,0(0,R1)               Get its length                  08757014
         STORAGE RELEASE,               Free the storage               +08758014
               LENGTH=(0),               this long                     +08759014
               ADDR=(1)                  starting here                  08760014
         SLR   R15,R15                  Clear work register             08761014
         ST    R15,@_SYSUT1_BtL_Ptr     No more storage below           08762014
                                                                        08763014
*-------------------------------------------------------------------*   08764014
*                                                                   *   08765014
*    ... and get some above-the-line for the ACB, EXLST and RPL     *   08766014
*                                                                   *   08767014
*-------------------------------------------------------------------*   08768014
                                                                        08769014
         LA    R0,4+I_SYSUT1_ACB_Length+I_SYSUT1_EXLST_Length+SYSUT1_RP+08770014
               L_Length                                                 08771014
         STORAGE OBTAIN,                Get some storage               +08772014
               LENGTH=(0),               this long                     +08773014
               LOC=ANY                   from above-the-line            08774014
         ST    R1,@_SYSUT1_AtL_Ptr      Save the address                08775014
         ST    R0,0(0,R1)               Save the length                 08776014
         LA    R2,4(0,R1)               Point to ACB area               08777014
         MVC   0(I_SYSUT1_ACB_Length,R2),I_SYSUT1_ACB  Move ACB         08778014
         ST    R2,@_SYSUT1_ACB_Ptr      Save A(ACB)                     08779014
                                                                        08780014
         LA    R3,I_SYSUT1_ACB_Length(0,R2) A(Dynamic EXLST)            08781014
         MVC   0(I_SYSUT1_EXLST_Length,R3),I_SYSUT1_EXLST Move it       08782014
         MODCB ACB=(2),               Update SYSUT1 ACB                +08783014
               EXLST=(3),              with EXLST                      +08784014
               MF=(G,@I_MODCB1,I_MODCB1_Length)                         08785014
                                                                        08786014
         ICM   R0,B'1111',@I_JFCBABFS Get user-specified BUFSP=         08787014
         BNZ   I0180                  OK, skip                          08788014
         LH    R0,@I_JFCBADBF         None, get user specified BUFND=   08789014
         LTR   R0,R0                  Any?                              08790014
         BNZ   I0180                  Yes, skip                         08791014
         SLR   R15,R15                No, clear work register           08792014
         ST    R15,@I_BUFSP           Clear Buffer space                08793014
         LH    R2,@I_DVATRK           Get Trks/Cyl                      08794014
         LH    R3,@I_DVATRKLN         Get Bytes/Trk                     08795014
         SLL   R3,16                  Make sure                         08796014
         SRL   R3,16                   its positive                     08797014
         MR    R2,R2                  Get Bytes/Cyl                     08798014
         LTR   R3,R3                  Anything?                         08799014
         BZ    I0180                  No, don't modify ACB              08800014
         A     R3,=FL4'4096'          Yes, bump by a page               08801014
         L     R2,@_SYSUT1_ACB_Ptr    A(ACB)                            08802014
         MODCB ACB=(2),               Update SYSUT1 ACB                +08803014
               BUFSP=(3),               for bytes/cyl                  +08804014
               MF=(G,@I_MODCB2,I_MODCB2_Length)                         08805014
                                                                        08806014
         ST    R3,@I_BUFSP            and save Buffer Space             08807014
I0180    DS    0H                                                       08808014
         L     R2,@_SYSUT1_ACB_Ptr    A(ACB)                            08809014
         LA    R3,@_SYSUT1_Rec_Length A(RECEIVING AREA)                 08810014
         SHOWCB ACB=(2),              YES, FROM SYSUT1 ACB             *08811014
               FIELDS=LRECL,            WE WANT THE LRECL              *08812014
               AREA=(3),                MOVED TO THIS AREA             *08813014
               LENGTH=L'@_SYSUT1_Rec_Length, for this long             *08814014
               MF=(G,@I_SHOWCB,I_SHOWCB_Length)                         08815014
         LTR   R15,R15                Did it work?                      08816014
         BZ    I0190                  Yes, skip                         08817014
         LH    R15,=H'32767'          No, assume the biggest            08818014
         ST    R15,@_SYSUT1_Rec_Length and save it                      08819014
I0190    DS    0H                                                       08820014
                                                                        08821014
*-------------------------------------------------------------------*   08822014
*                                                                   *   08823014
*    Get an area in which the input record will be assembled        *   08824014
*                                                                   *   08825014
*-------------------------------------------------------------------*   08826014
                                                                        08827014
         L     R1,@_SYSUT1_Rec_Length Get size of record                08828014
         LA    R1,7(0,R1)            Make it                            08829014
         SRL   R1,3                   doubleword multiple               08830014
         SLDL  R0,35                  in R0                             08831014
         STORAGE OBTAIN,             Get some storage                  +08832014
               LENGTH=(0),            this long                        +08833014
               LOC=BELOW              below the line                    08834014
                                                                        08835014
         ST    R1,@_SYSUT1_Rec_Ptr   Save it                            08836014
                                                                        08837014
*-------------------------------------------------------------------*   08838014
*                                                                   *   08839014
*    And open SYSUT1 (@_SYSUT1_DCB_Ptr contains the address of the  *   08840014
*   DCB or ACB)                                                     *   08841014
*                                                                   *   08842014
*-------------------------------------------------------------------*   08843014
                                                                        08844014
         L     R2,@_SYSUT1_DCB_Ptr    Point to the DCB/ACB              08845014
         TM    DCBOFLGS-IHADCB(R2),DCBOFOPN DCB already open?           08846014
         BO    I0210                  Yes, skip the OPEN                08847014
         MVC   @I_OPEN,I_OPEN         MOVE OPEN PARMS                   08848014
         OPEN  ((2),INPUT),MODE=31,MF=(E,@I_OPEN) AND OPEN DCB/ACB      08849014
         LTR   R15,R15                DID IT OPEN?                      08850014
         BNZ   I8020                  NO, ERROR                         08851014
I0210    DS    0H                                                       08852014
                                                                        08853014
*-------------------------------------------------------------------*   08854014
*                                                                   *   08855014
*    SYSUT1 is open, so go list some interesting(?) info            *   08856014
*                                                                   *   08857014
*-------------------------------------------------------------------*   08858014
                                                                        08859014
         L     R1,@_SYSUT1_DCB_Ptr    A(DCB or ACB)                     08860014
         L     R15,=A(J0010)          A(DCB info routine)               08861014
         BASR  R14,R15                Go do it                          08862014
                                                                        08863014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               08864014
         BASR  R14,R15                SKIP A LINE                       08865014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               08866014
         BASR  R14,R15                  AND ANOTHER                     08867014
                                                                        08868014
*-------------------------------------------------------------------*   08869014
*                                                                   *   08870014
*    SYSUT1 has been opened successfully - if we are using BSAM     *   08871014
*   to read it ...                                                  *   08872014
*                                                                   *   08873014
*-------------------------------------------------------------------*   08874014
                                                                        08875014
         TM    @_SYSUT1_BSAM,L'@_SYSUT1_BSAM Are we using BSAM?         08876014
         BNO   I0410                  No, skip                          08877014
                                                                        08878014
*-------------------------------------------------------------------*   08879014
*                                                                   *   08880014
*    ... initialize the DECBs and buffer areas, and prime the areas *   08881014
*   with initial READs                                              *   08882014
*                                                                   *   08883014
*-------------------------------------------------------------------*   08884014
                                                                        08885014
         L     R4,@_SYSUT1_AtL_Ptr    A(Above-the-line storage)         08886014
         LA    R4,8(0,R4)             A(1st buffer area)                08887014
         L     R2,@_SYSUT1_DCB_Ptr    A(DCB)                            08888014
         LA    R5,I_SYSUT1_BSAM_DCB_Length+I_SYSUT1_DCBE_Length(0,R2)   08889014
         LH    R6,@I_Buffer_Count     Nbr buffers                       08890014
I0400    DS    0H                                                       08891014
         ST    R5,BuffArea_DECB_Ptr-BuffArea(0,R4) Save A(DECB)         08892014
         MVC   0(I_READ_Length,R5),I_READ  Initialize DECB              08893014
         LA    R3,BuffArea_IOArea-BuffArea(0,R4) A(I/O Area)            08894014
         READ  (5),SF,(2),(3),'S',MF=E    Issue the read                08895014
         SLR   R15,R15                Clear work register               08896014
         ST    R15,BuffArea_Offset-BuffArea(0,R4) Clear Offset          08897014
         L     R15,@I_Buffer_Length   Get buffer size                   08898014
         AR    R15,R4                 Point to next area                08899014
         ST    R15,BuffArea_Next_Ptr-BuffArea(0,R4) Chain BuffAreas     08900014
         LR    R4,R15                 Point to next BuffArea            08901014
         LA    R5,I_READ_Length(0,R5) A(next DECB)                      08902014
         BCT   R6,I0400               and do it too                     08903014
                                                                        08904014
         S     R4,@I_Buffer_Length    Back up to last buffer area       08905014
         L     R15,@_SYSUT1_AtL_Ptr   Point to                          08906014
         LA    R15,8(0,R15)             1st buffer area                 08907014
         ST    R15,BuffArea_Next_Ptr-BuffArea(0,R4) Chain last to 1st   08908014
         ST    R15,@_SYSUT1_BuffArea_Ptr And init our pointer           08909014
                                                                        08910014
I0410    DS    0H                                                       08911014
                                                                        08912014
*-------------------------------------------------------------------*   08913014
*                                                                   *   08914014
*    If the input is VSAM, generate an RPL in the area after the    *   08915014
*   ACB                                                             *   08916014
*                                                                   *   08917014
*-------------------------------------------------------------------*   08918014
                                                                        08919014
                                                                        08920014
         TM    @_SYSUT1_VSAM,L'@_SYSUT1_VSAM Is SYSUT1 VSAM?            08921014
         BNO   I0420                    No, skip                        08922014
                                                                        08923014
         L     R2,@_SYSUT1_ACB_Ptr      A(ACB)                          08924014
         LA    R3,I_SYSUT1_ACB_Length+I_SYSUT1_EXLST_Length(0,R2)       08925014
         L     R4,@_SYSUT1_Rec_Ptr      A(Record area)                  08926014
         L     R5,@_SYSUT1_Rec_Length   Length of a record              08927014
         GENCB BLK=RPL,               GENERATE RPL                     *08928014
               AM=VSAM,                 FOR VSAM                       *08929014
               ACB=(2),                 SYSUT1                         *08930014
               AREA=(4),                A(INPUT RECORD ADDRESS)        *08931014
               AREALEN=(5),             Len of the address             *08932014
               OPTCD=(ADR,SEQ,MVE,SYN), MOVE MODE                      *08933014
               WAREA=(3),               BUILD THE RPL HERE             *08934014
               LENGTH=SYSUT1_RPL_Length,  its this long                +08935014
               MF=(G,@I_GENCB,I_GENCB_Length)                           08936014
                                                                        08937014
         ST    R3,@_SYSUT1_RPL_Ptr      Save A(RPL)                     08938014
                                                                        08939014
I0420    DS    0H                                                       08940014
         SLR   R15,R15                Clear return code                 08941014
         B     I9000                  and exit                          08942014
                                                                        08943014
I8000    DS    0H                                                       08944014
                                                                        08945014
*-------------------------------------------------------------------*   08946014
*                                                                   *   08947014
*    No active SMF file                                             *   08948014
*                                                                   *   08949014
*-------------------------------------------------------------------*   08950014
                                                                        08951014
         L     R1,@_SYSPRINT_Rec_Ptr  A(SYSPRINT RECORD)                08952014
         MVC   1(16,R1),=C'No active SMF File found'                    08953014
         L     R15,=AL4(G0010)        A(Print routine)                  08954014
         BASR  R14,R15                GO PRINT IT                       08955014
         LA    R15,4                  FLAG AN ERROR                     08956014
         B     I9000                  AND EXIT                          08957014
                                                                        08958014
I8010    DS    0H                                                       08959014
                                                                        08960014
*-------------------------------------------------------------------*   08961014
*                                                                   *   08962014
*    Dynalloc (SVC99) error                                         *   08963014
*                                                                   *   08964014
*-------------------------------------------------------------------*   08965014
                                                                        08966014
         L     R1,@_SYSPRINT_Rec_Ptr  A(SYSPRINT RECORD)                08967014
         MVC   1(16,R1),=C'Error allocating'                            08968014
         MVC   18(10,R1),@I_SVC99_DSName_TU+S99TUPAR-S99TUNIT MOVE DSN  08969014
         MVC   28(30,R1),=C', RC=XX, Error=XXXX, Info=XXXX'             08970014
         SRL   R15,4                  MAKE ROOM FOR "SIGN"              08971014
         STH   R15,@I_DBLWD           SAVE IT                           08972014
         OI    @I_DBLWD+1,X'0F'       INSERT A  SIGN                    08973014
         UNPK  @I_DBLWD+2(3),@I_DBLWD(2) UNPACK IT                      08974014
         TR    @I_DBLWD+3(2),I_HexNum MAKE IT READABLE                  08975014
         MVC   33(2,R1),@I_DBLWD+3    MOVE TO MESSAGE                   08976014
         LH    R15,@I_SVC99_RB+S99ERROR-S99RB GET ERROR CODE            08977014
         SRL   R15,4                  MAKE ROOM FOR SIGN                08978014
         ST    R15,@I_DBLWD+4         SAVE IT                           08979014
         OI    @I_DBLWD+7,X'0F'       INSERT A SIGN                     08980014
         UNPK  @I_DBLWD(5),@I_DBLWD+5(3) UNPACK IT                      08981014
         TR    @I_DBLWD+1(4),I_HexNum MAKE IT READABLE                  08982014
         MVC   43(4,R1),@I_DBLWD+1   MOVE TO MESSAGE                    08983014
         LH    R15,@I_SVC99_RB+S99INFO-S99RB GET INFO CODE              08984014
         SRL   R15,4                  MAKE ROOM FOR SIGN                08985014
         ST    R15,@I_DBLWD+4         SAVE IT                           08986014
         OI    @I_DBLWD+7,X'0F'       INSERT A SIGN                     08987014
         UNPK  @I_DBLWD(5),@I_DBLWD+5(3) UNPACK IT                      08988014
         TR    @I_DBLWD+1(4),I_HexNum MAKE IT READABLE                  08989014
         MVC   54(4,R1),@I_DBLWD+1   MOVE TO MESSAGE                    08990014
         L     R15,=AL4(G0010)        A(Print routine)                  08991014
         BASR  R14,R15                GO PRINT IT                       08992014
         LA    R15,4                  FLAG AN ERROR                     08993014
         B     I9000                  AND EXIT                          08994014
I8020    DS    0H                                                       08995014
                                                                        08996014
*-------------------------------------------------------------------*   08997014
*                                                                   *   08998014
*    Error opening SYSUT1                                           *   08999014
*                                                                   *   09000014
*-------------------------------------------------------------------*   09001014
                                                                        09002014
         L     R1,@_SYSPRINT_Rec_Ptr  NO, GET A(SYSPRINT RECORD)        09003014
         MVC   22(40,R1),=C'SYSUT1 could not be opened - terminating'   09004014
         LA    R15,12                 SET RETURN CODE                   09005014
         B     I9000                  AND EXIT                          09006014
                                                                        09007014
I9000    DS    0H                                                       09008014
                                                                        09009014
*-------------------------------------------------------------------*   09010014
*                                                                   *   09011014
*    Free up our local storage ...                                  *   09012014
*                                                                   *   09013014
*-------------------------------------------------------------------*   09014014
                                                                        09015014
         LR    R3,R15                   Rescue the return code          09016014
                                                                        09017014
         LA    R1,@I_Dynam              A(Local storage)                09018014
         L     R13,4(0,R13)             Rescue A(HSA)                   09019014
                                                                        09020014
         STACK POP,                     Free the stack area            +09021014
               ADDR=(R1),                starting here                 +09022014
               STACK=@_31Bit_Stack_Ptr   on this stack                  09023014
                                                                        09024014
*-------------------------------------------------------------------*   09025014
*                                                                   *   09026014
*    ... and return to caller                                       *   09027014
*                                                                   *   09028014
*-------------------------------------------------------------------*   09029014
                                                                        09030014
         LR    R15,R3                   Restore return code             09031014
         L     R14,12(0,R13)            Restore return address          09032014
         LM    R0,R12,20(R13)           Restore other registers         09033014
         BSM   0,R14                    and return                      09034014
                                                                        09035014
I_HexNum EQU   *-239                                                    09036014
         DC    C' 0123456789ABCDEF'                                     09037014
                                                                        09038014
         LTORG                                                          09039014
                                                                        09040014
                                                                        09041014
         PUSH PRINT                                                     09042014
         PRINT NOGEN                                                    09043014
I_SYSUT1_QSAM_DCB DCB DDNAME=SYSUT1,MACRF=GM,DSORG=PS,OPTCD=C,         +09044014
               DCBE=1                                                   09045014
I_SYSUT1_QSAM_DCB_Length EQU *-I_SYSUT1_QSAM_DCB LENGTH OF SYSUT1 DCB   09046014
                                                                        09047014
I_SYSUT1_BSAM_DCB DCB DDNAME=SYSUT1,MACRF=RC,DSORG=PS,                 +09048014
               DCBE=1                                                   09049014
I_SYSUT1_BSAM_DCB_Length EQU *-I_SYSUT1_BSAM_DCB LENGTH OF SYSUT1 DCB   09050014
                                                                        09051014
I_SYSUT1_DCBE DCBE RMODE31=BUFF,MULTSDN=1,EODAD=1                       09052014
I_SYSUT1_DCBE_Length EQU *-I_SYSUT1_DCBE Length of SYSUT1 DCBE          09053014
                                                                        09054014
I_SYSUT1_ACB ACB AM=VSAM,               FOR VSAM                       *09055014
               DDNAME=SYSUT1,           SYSUT1                         *09056014
               BUFND=3,                 DATA BUFFERS                   *09057014
               BUFSP=131072,            128K FOR BUFFERS               *09058014
               STRNO=1,                 NBR STRINGS                    *09059014
               EXLST=I_SYSUT1_EXLST,    EODAD                          *09060014
               MACRF=(ADR,NFX,DDN,NRM,NRS,SEQ,IN),                     *09061014
               RMODE31=ALL              Everything above-the-line       09062014
             DS    0D                                                   09063014
I_SYSUT1_ACB_Length EQU   *-I_SYSUT1_ACB                                09064014
                                                                        09065014
         PRINT GEN                                                      09066014
I_SYSUT1_EXLST EXLST AM=VSAM,           For VSAM                       +09067014
               EODAD=1                   End-of-Data                    09068014
             DS    0D                                                   09069014
I_SYSUT1_EXLST_Length EQU *-I_SYSUT1_EXLST                              09070014
         PRINT NOGEN                                                    09071014
                                                                        09072014
I_OPEN   OPEN  (0,INPUT),MODE=31,MF=L                                   09073014
I_OPEN_Length EQU  *-I_OPEN                                             09074014
                                                                        09075014
         READ  I_READ,SF,0,0,'S',MF=L                                   09076014
I_READ_Length EQU  *-I_READ                                             09077014
                                                                        09078014
I_CLOSE  CLOSE (0),MODE=31,MF=L                                         09079014
I_CLOSE_Length EQU *-I_CLOSE                                            09080014
                                                                        09081014
         POP    PRINT                                                   09082014
@I_Dynam        DSECT                 Dynamic area for I                09083014
@I_Save         DS    18F              O/S Style save area              09084014
@I_DBLWD        DS    D                Workarea                         09085014
                                                                        09086014
@I_JFCB_Ptr     DS    AL4              A(JFCB)                          09087014
@I_DVACLASS     DS    X                Device class from DEVTYPE        09088014
@I_DVATRK       DS    HL2              Trks/Cyl from DEVTYPE            09089014
@I_DVATRKLN     DS    HL2              Bytes/Trk from DEVTYPE           09090014
@I_RECFM        DS    X                DCB=RECFM                        09091014
                                                                        09092014
@I_DCBChar      EQU   *                Non-VSAM DCB Characteristics     09093014
@I_JFCBUFNO     DS    X                 DCB=BUFNO from JFCB             09094014
@I_JFCNCP       DS    X                 DCB=NCP= from JFCB              09095014
@I_BLKSI        DS    HL2               SYSUT1 BLKSIZE                  09096014
@I_NCP          DS    X                 DCB=NCP (calculated by OPEN)    09097014
@I_Buffer_Count DS    HL2               Number of buffer areas          09098014
@I_Buffer_Length DS   FL4               Buffer area length              09099014
@I_ACBChar      ORG   @I_DCBChar       VSAM ACB Characteristics         09100014
@I_JFCBABFS     DS    FL4               AMP=BUFSP= from JFCB            09101014
@I_JFCBADBF     DS    HL2               AMP=BUFND= from JFCB            09102014
@I_BUFSP        DS    FL4               VSAM BUFFSP                     09103014
                ORG                                                     09104014
                                                                        09105014
@I_MACRO        DS    0F               Macro area                       09106014
                ORG   @I_MACRO                                          09107014
@I_OPEN         DS    CL(I_OPEN_Length) OPEN macro                      09108014
                ORG   @I_MACRO                                          09109014
@I_CLOSE        DS    CL(I_CLOSE_Length) CLOSE macro                    09110014
                ORG   @I_MACRO                                          09111014
@I_SHOWCB       DS    CL(I_SHOWCB_Length) SHOWCB Macro                  09112014
                ORG   @I_MACRO                                          09113014
@I_MODCB1       DS    CL(I_MODCB1_Length) MODCB Macro                   09114014
                ORG   @I_MACRO                                          09115014
@I_MODCB2       DS    CL(I_MODCB2_Length) MODCB Macro                   09116014
                ORG   @I_MACRO                                          09117014
@I_GENCB        DS    CL(I_GENCB_Length) GENCB Macro                    09118014
                ORG   @I_MACRO                                          09119014
@I_DEVAREA      DS    CL(L'DVAPREFX+L'DVATAB) DEVTAB return info        09120014
                ORG   @I_MACRO                                          09121014
*-------------------------------------------------------------------*   09122014
*                                                                   *   09123014
*    DYNAMIC ALLOCATION (SVC 99) CONTROL BLOCKS                     *   09124014
*                                                                   *   09125014
*-------------------------------------------------------------------*   09126014
                                                                        09127014
@I_SVC99_RB_Ptr    DS  F                                                09128014
@I_SVC99_RB        DS  CL(S99RBEND-S99RB)                               09129014
@I_SVC99_TU_List   DS  6A                                               09130014
@I_SVC99_DDName_TU DS  CL((S99TUPAR-S99TUNIT)+8)      DDNAME            09131014
@I_SVC99_DSName_TU DS  CL((S99TUPAR-S99TUNIT)+44)     DSNAME            09132014
@I_SVC99_Status_TU DS  CL((S99TUPAR-S99TUNIT)+1)      STATUS            09133014
@I_SVC99_NDisp_TU  DS  CL((S99TUPAR-S99TUNIT)+1)      NORMAL DISP       09134014
@I_SVC99_CDisp_TU  DS  CL((S99TUPAR-S99TUNIT)+1)      COND DISP         09135014
@I_SVC99_Free_TU   DS  CL(S99TUENT-S99TUNIT)          FREE=CLOSE        09136014
                   ORG ,                                                09137014
                                                                        09138014
                DS    0D               Alignment                        09139014
@I_DynLen       EQU   *-@I_Dynam      Length of storage required        09140014
                                                                        09141014
SMFSLCT  RSECT                                                          09142014
                                                                        09143014
         DROP  R11,R13                                                  09144014
         POP   USING                                                    09145014
         TITLE 'J0000: Print info about DCB'                            09146014
                                                                        09147014
         PUSH  USING                                                    09148014
                                                                        09149014
J0010    DS    0H                                                       09150014
                                                                        09151014
*---------------------------------------------------------------------* 09152014
*                                                                     * 09153014
*  Routine   : J0010                                                  * 09154014
*                                                                     * 09155014
*  Abstract  : Print info about a DCB or ACB.                         * 09156014
*                                                                     * 09157014
*  Inputs    : R1 ----> A(Open DCB or ACB)                            * 09158014
*  Outputs   : R15: 0 - OK                                            * 09159014
*                   4 - SYSUT1 not useable.                           * 09160014
*                                                                     * 09161014
*  Notes     :                                                        * 09162014
*                                                                     * 09163014
*  History:                                                           * 09164014
*              1999/04/24 SDDA030 V1.1                                * 09165014
*                                 Used LIFO Stack for our dynamic     * 09166014
*                                 areas.                              * 09167014
*              2002/02/07 SDDA030 V1.4                                * 09168014
*                                 Invoked W0010 to get JFCB and TIOT  * 09169014
*                                 addresses.                          * 09170014
*              ____/__/__ _______                                     * 09171014
*                                                                     * 09172014
*---------------------------------------------------------------------* 09173014
                                                                        09174014
         USING *,R15                                                    09175014
         SAVE  (14,12),T                                                09176014
         LR    R11,R15                  Load permanent base reg         09177014
         DROP  R15                      Free up temp base               09178014
         USING J0010,R11                Assign permanent base           09179014
                                                                        09180014
*-------------------------------------------------------------------*   09181014
*                                                                   *   09182014
*    Get an area on the stack for our local storage                 *   09183014
*                                                                   *   09184014
*-------------------------------------------------------------------*   09185014
                                                                        09186014
         STACK PUSH,                    Get Stack area                 +09187014
               LEN=@J_DynLen,            this long                     +09188014
               STACK=@_31Bit_Stack_Ptr   using this stack               09189014
                                                                        09190014
         LR    R2,R1                    Save its address                09191014
         LR    R0,R1                    A(Our storage)                  09192014
         LA    R1,@J_DynLen              Its length                     09193014
         SLR   R14,R14                  Clear source address            09194014
         SLR   R15,R15                   and length                     09195014
         MVCL  R0,R14                   Clear our storage               09196014
         LR    R1,R2                    Restore A(Our storage)          09197014
                                                                        09198014
*-------------------------------------------------------------------*   09199014
*                                                                   *   09200014
*    Chain our save areas, and restore the important registers      *   09201014
*   that we have destroyed                                          *   09202014
*                                                                   *   09203014
*-------------------------------------------------------------------*   09204014
                                                                        09205014
         ST    R13,4(0,R1)              Chain                           09206014
         ST    R1,8(0,R13)               saveareas                      09207014
         LR    R13,R1                   Load dynam base                 09208014
         USING @J_Dynam,R13             Assign a base                   09209014
         L     R15,@J_Dynam+4           Get A(HSA)                      09210014
         LM    R0,R3,20(R15)            Restore callers registers       09211014
                                                                        09212014
         ST    R1,@J_DCB_Ptr            Save A(DCB)                     09213014
                                                                        09214014
*-------------------------------------------------------------------*   09215014
*                                                                   *   09216014
*    Get our JFCB ...                                               *   09217014
*                                                                   *   09218014
*-------------------------------------------------------------------*   09219014
                                                                        09220014
         L     R15,=AL4(W0010)          A(Get JFCB routine)             09221014
         BASR  R14,R15                  Go do it                        09222014
                                                                        09223014
         ST    R1,@J_JFCB_Ptr           Save A(JFCB)                    09224014
         ST    R0,@J_TIOT_Entry_Ptr     Save A(TIOT Entry)              09225014
                                                                        09226014
*-------------------------------------------------------------------*   09227014
*                                                                   *   09228014
*    Put out the DSN and volsers for this DDName                    *   09229014
*                                                                   *   09230014
*-------------------------------------------------------------------*   09231014
                                                                        09232014
         L     R1,@_SYSPRINT_Rec_Ptr    Get A(SYSPRINT Record)          09233014
         L     R15,@J_TIOT_Entry_Ptr    Get A(TIOT Entry)               09234014
         MVC   1(L'TIOEDDNM,R1),TIOEDDNM-TIOENTRY(R15) Move DDName      09235014
         MVC   2+L'TIOEDDNM(5,R1),=C'DSN: ' Move heading                09236014
         L     R15,@J_JFCB_Ptr          Get A(JFCB)                     09237014
         MVC   2+L'TIOEDDNM+5(L'JFCBDSNM,R1),JFCBDSNM-INFMJFCB(R15)     09238014
         LA    R1,2+L'TIOEDDNM+5+L'JFCBDSNM-1(,R1)  A(End of DSN)       09239014
J0020    DS    0H                                                       09240014
         CLI   0(R1),C' '               Is this the end?                09241014
         BNE   J0030                    Yes, skip                       09242014
         BCT   R1,J0020                 No, back up and try again       09243014
J0030    DS    0H                                                       09244014
         MVC   1(10,R1),=C', Volser: '  Move VOLSER heading             09245014
         LA    R1,11(0,R1)              Point past it                   09246014
         L     R15,@J_JFCB_Ptr          Get A(JFCB Pointer)             09247014
         TM    JFCFLGS1-INFMJFCB(R15),JFCVRDS VIO?                      09248014
         BNO   J0030C                   No, skip                        09249014
         MVC   0(7,R1),=C'**VIO**'      Yes, dummy volser               09250014
         B     J0050                    and skip                        09251014
J0030C   DS    0H                                                       09252014
                                                                        09253014
         SLR   R0,R0                    Clear work register             09254014
         IC    R0,JFCBNVOL-INFMJFCB(R15) Get nbr volsers                09255014
         LA    R15,JFCBVOLS-INFMJFCB(R15)  and point to first           09256014
         CH    R0,=H'5'                 More than 5?                    09257014
         BNH   J0040                    No, skip                        09258014
         LA    R0,5                     Yes, only the 1st 5             09259014
J0040    DS    0H                                                       09260014
         MVC   0(6,R1),0(R15)           Move VOLSER                     09261014
         SH    R0,=H'1'                 Reduce volser count             09262014
         BNP   J0050                    thats all, skip                 09263014
         MVI   6(R1),C','               More, add a comma               09264014
         LA    R1,7(0,R1)               Point to next output area       09265014
         LA    R15,6(0,R15)             A(Next input volser)            09266014
         B     J0040                    and do it too                   09267014
J0050    DS    0H                                                       09268014
         L     R15,=AL4(G0010)          A(SYSPRINT routine)             09269014
         BASR  R14,R15                  Go print it                     09270014
                                                                        09271014
*-------------------------------------------------------------------*   09272014
*                                                                   *   09273014
*    Put out the DCB or ACB information                             *   09274014
*                                                                   *   09275014
*-------------------------------------------------------------------*   09276014
                                                                        09277014
         L     R15,@J_DCB_Ptr           A(DCB)                          09278014
         TM    DCBDSRG2-IHADCB(R15),DCBACBM Is this VSAM?               09279014
         BO    J0140                    Yes, format VSAM stuff          09280014
         L     R1,@_SYSPRINT_Rec_Ptr    No, get A(SYSPRINT Record)      09281014
         MVC   10(11,R1),=C'DCB=(RECFM=' Move heading                   09282014
         LA    R1,21(0,R1)              Point to next byte              09283014
         TM    DCBRECFM-IHADCB(R15),DCBRECF+DCBRECV  RECFM=U?           09284014
         BNO   J0060                    No, skip                        09285014
         MVI   0(R1),C'U'               Yes, say so                     09286014
         LA    R1,1(0,R1)               Bump pointer                    09287014
         B     J0110                    and skip                        09288014
J0060    DS    0H                                                       09289014
         TM    DCBRECFM-IHADCB(R15),DCBRECF  RECFM=F?                   09290014
         BZ    J0070                    No, skip                        09291014
         MVI   0(R1),C'F'               Yes, say so                     09292014
         B     J0080                    and skip                        09293014
J0070    DS    0H                                                       09294014
         MVI   0(R1),C'V'               RECFM=V                         09295014
J0080    DS    0H                                                       09296014
         LA    R1,1(0,R1)               Bump pointer                    09297014
         TM    DCBRECFM-IHADCB(R15),DCBRECBR  Blocked?                  09298014
         BZ    J0090                    No, skip                        09299014
         MVI   0(R1),C'B'               Yes, say so                     09300014
         LA    R1,1(0,R1)               Bump pointer                    09301014
J0090    DS    0H                                                       09302014
         TM    DCBRECFM-IHADCB(R15),DCBRECSB Spanned or Standard?       09303014
         BZ    J0100                    No, skip                        09304014
         MVI   0(R1),C'S'               Yes, say so                     09305014
         LA    R1,1(0,R1)               and bump pointer                09306014
J0100    DS    0H                                                       09307014
                                                                        09308014
         MVC   0(7,R1),=C',LRECL='      Move literal                    09309014
         LA    R1,7(0,R1)               Point to next byte              09310014
         LH    R0,DCBLRECL-IHADCB(R15)  Get LRECL                       09311014
         CVD   R0,@J_DBLWD              Pack it                         09312014
         MVC   @J_Temp(5),=X'2020202120' Move mask                      09313014
         LR    R2,R1                    Save A(Output Byte)             09314014
         LA    R1,@J_Temp+4             A(Significant digit)            09315014
         EDMK  @J_Temp(5),@J_DBLWD+5    Edit it                         09316014
         LR    R0,R1                    A(Start digit)                  09317014
         LA    R1,@J_Temp+5             Calculate                       09318014
         SR    R1,R0                     length                         09319014
         LR    R3,R1                    Destination length              09320014
         MVCL  R2,R0                    Move it                         09321014
         LR    R1,R2                    A(Next byte)                    09322014
J0110    DS    0H                                                       09323014
         MVC   0(9,R1),=C',BLKSIZE='    Move literal                    09324014
         LA    R1,9(0,R1)               Point to next byte              09325014
         LH    R0,DCBBLKSI-IHADCB(R15)  Get BLKSIZE                     09326014
         CVD   R0,@J_DBLWD              Pack it                         09327014
         MVC   @J_Temp(5),=X'2020202120' Move mask                      09328014
         LR    R2,R1                    Save A(Output Byte)             09329014
         LA    R1,@J_Temp+4             A(Significant digit)            09330014
         EDMK  @J_Temp(5),@J_DBLWD+5    Edit it                         09331014
         LR    R0,R1                    A(Start digit)                  09332014
         LA    R1,@J_Temp+5             Calculate                       09333014
         SR    R1,R0                     length                         09334014
         LR    R3,R1                    Destination length              09335014
         MVCL  R2,R0                    Move it                         09336014
         LR    R1,R2                    A(Next byte)                    09337014
                                                                        09338014
         TM    DCBCIND2-IHADCB(R15),DCBCNQSM  QSAM?                     09339014
         BZ    J0120                    No, skip                        09340014
                                                                        09341014
         TM    DCBCIND1-IHADCB(R15),DCBCBNDF  Yes, BUFNO default?       09342014
         BO    J0130                    Yes, skip                       09343014
         MVC   0(7,R1),=C',BUFNO='      No, move literal                09344014
         LA    R1,7(0,R1)               Point to next byte              09345014
         SLR   R0,R0                    Clear work register             09346014
         IC    R0,DCBBUFNO-IHADCB(R15)  Get BUFNO                       09347014
         CVD   R0,@J_DBLWD              Pack it                         09348014
         MVC   @J_Temp(3),=X'202120' Move mask                          09349014
         LR    R2,R1                    Save A(Output Byte)             09350014
         LA    R1,@J_Temp+2             A(Significant digit)            09351014
         EDMK  @J_Temp(3),@J_DBLWD+6    Edit it                         09352014
         LR    R0,R1                    A(Start digit)                  09353014
         LA    R1,@J_Temp+3             Calculate                       09354014
         SR    R1,R0                     length                         09355014
         LR    R3,R1                    Destination length              09356014
         MVCL  R2,R0                    Move it                         09357014
         LR    R1,R2                    A(Next byte)                    09358014
         B     J0130                    and skip                        09359014
                                                                        09360014
J0120    DS    0H                                                       09361014
         MVC   0(5,R1),=C',NCP='        No, move literal                09362014
         LA    R1,5(0,R1)               Point to next byte              09363014
         SLR   R0,R0                    Clear work register             09364014
         IC    R0,DCBNCP-IHADCB(R15)    Get NCP                         09365014
         CVD   R0,@J_DBLWD              Pack it                         09366014
         MVC   @J_Temp(3),=X'202120'    Move mask                       09367014
         LR    R2,R1                    Save A(Output Byte)             09368014
         LA    R1,@J_Temp+2             A(Significant digit)            09369014
         EDMK  @J_Temp(3),@J_DBLWD+6    Edit it                         09370014
         LR    R0,R1                    A(Start digit)                  09371014
         LA    R1,@J_Temp+3             Calculate                       09372014
         SR    R1,R0                     length                         09373014
         LR    R3,R1                    Destination length              09374014
         MVCL  R2,R0                    Move it                         09375014
         LR    R1,R2                    A(Next byte)                    09376014
J0130    DS    0H                                                       09377014
                                                                        09378014
         MVI   0(R1),C')'               Close off DCB info              09379014
                                                                        09380014
         L     R15,=AL4(G0010)          A(SYSPRINT routine)             09381014
         BASR  R14,R15                  Go print it                     09382014
                                                                        09383014
         B     J0170                    and skip                        09384014
                                                                        09385014
J0140    DS    0H                                                       09386014
                                                                        09387014
         SLR   R0,R0                  Clear work register               09388014
         ST    R0,@J_BUFND            Clear data space                  09389014
         ST    R0,@J_BUFSP            and buffer space                  09390014
         LR    R2,R15                 A(ACB)                            09391014
         LA    R3,@J_BUFND            A(Receiving area)                 09392014
         SHOWCB ACB=(2),              Get data from this ACB           +09393014
               FIELDS=(BUFND,BUFSP),    this data                      +09394014
               AREA=(3),                moved to here                  +09395014
               LENGTH=8,                for this long                  +09396014
               MF=(G,@J_SHOWCB,J_SHOWCB_Length)                         09397014
                                                                        09398014
         L     R0,@J_BUFND            Get Data space                    09399014
         A     R0,@J_BUFSP            and buffer space                  09400014
         LTR   R0,R0                  anything to say?                  09401014
         BZ    J0170                  No, skip                          09402014
                                                                        09403014
         L     R1,@_SYSPRINT_Rec_Ptr    Yes, get A(SYSPRINT Record)     09404014
         MVC   10(7,R1),=C'AMP=BUF'     Move literal                    09405014
         ICM   R0,B'1111',@J_BUFSP      Get buffer space                09406014
         BZ    J0150                    None, skip                      09407014
         MVC   17(2,R1),=C'SP'          Yes, make it BUFSP              09408014
         B     J0160                                                    09409014
J0150    DS    0H                                                       09410014
         MVC   17(2,R1),=C'ND'          Make it BUFND                   09411014
         L     R0,@J_BUFND              and load up the register        09412014
J0160    DS    0H                                                       09413014
         MVI   19(R1),C'='              Move literal                    09414014
         LA    R1,20(0,R1)              Point to next char              09415014
         CVD   R0,@J_DBLWD              Pack our value                  09416014
         MVC   @J_Temp(9),=X'202020202020202120'                        09417014
         LR    R2,R1                    Save A(Output Byte)             09418014
         LA    R1,@J_Temp+8             A(Significant digit)            09419014
         EDMK  @J_Temp(9),@J_DBLWD+3    Edit it                         09420014
         LR    R0,R1                    A(Start digit)                  09421014
         LA    R1,@J_Temp+9             Calculate                       09422014
         SR    R1,R0                     length                         09423014
         LR    R3,R1                    Destination length              09424014
         MVCL  R2,R0                    Move it                         09425014
                                                                        09426014
         L     R15,=AL4(G0010)          A(SYSPRINT routine)             09427014
         BASR  R14,R15                  Go print it                     09428014
                                                                        09429014
J0170    DS    0H                                                       09430014
                                                                        09431014
*-------------------------------------------------------------------*   09432014
*                                                                   *   09433014
*    If this is the input file (SYSUT1) ...                         *   09434014
*                                                                   *   09435014
*-------------------------------------------------------------------*   09436014
                                                                        09437014
         L     R15,@J_TIOT_Entry_Ptr    A(TIOT entry)                   09438014
         CLC   =CL8'SYSUT1',TIOEDDNM-TIOENTRY(R15)  Input file?         09439014
         BNE   J9010                    No, all done                    09440014
                                                                        09441014
*-------------------------------------------------------------------*   09442014
*                                                                   *   09443014
*    ... display some info about input processing                   *   09444014
*                                                                   *   09445014
*-------------------------------------------------------------------*   09446014
                                                                        09447014
                                                                        09448014
         TM    @_SYSUT1_VSAM,L'@_SYSUT1_VSAM Is this VSAM?              09449014
         BO    J0200                  Yes, skip VBS stuff               09450014
         L     R15,@J_DCB_Ptr         A(DCB)                            09451014
         TM    DCBRECFM-IHADCB(R15),DCBRECV+DCBRECSB VS or VBS?         09452014
         BNO   J0200                  No, skip                          09453014
         L     R2,@_SYSPRINT_Rec_Ptr  Yes, A(Print record)              09454014
         MVC   10(31,R2),=C'Variable Spanned segments will '            09455014
         LA    R2,41(0,R2)            Point to next byte                09456014
         TM    @_SYSUT1_BSAM,L'@_SYSUT1_BSAM Is this BSAM?              09457014
         BNO   J0180                  No, skip                          09458014
         TM    @_SYSUT1_NoVBSChk,L'@_SYSUT1_NoVBSChk Yes, checking VBS? 09459014
         BNO   J0190                  Yes, skip                         09460014
J0180    DS    0H                                                       09461014
         MVC   0(4,R2),=C'not '       No VBS checking                   09462014
         LA    R2,4(0,R2)             bump output pointer               09463014
J0190    DS    0H                                                       09464014
         MVC   0(27,R2),=C'be checked for consistency.'                 09465014
                                                                        09466014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               09467014
         BASR  R14,R15                GO PRINT THE LINE                 09468014
J0200    DS    0H                                                       09469014
         L     R2,@_SYSPRINT_Rec_Ptr  A(Print record)                   09470014
         MVC   10(11,R2),=C'Input data '                                09471014
         TM    @_SYSUT1_Sequence,L'@_SYSUT1_Sequence Input in sequence? 09472014
         BNO   J0210                  No, skip                          09473014
         MVC   21(14,R2),=C'assumed to be '                             09474014
         LA    R2,35(0,R2)            bump output pointer               09475014
         B     J0220                  and skip                          09476014
J0210    DS    0H                                                       09477014
         MVC   21(16,R2),=C'not necessarily '                           09478014
         LA    R2,37(0,R2)            bump output pointer               09479014
J0220    DS    0H                                                       09480014
         MVC   0(12,R2),=C'in sequence.'                                09481014
                                                                        09482014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               09483014
         BASR  R14,R15                GO PRINT THE LINE                 09484014
                                                                        09485014
J9010    DS    0H                                                       09486014
                                                                        09487014
*-------------------------------------------------------------------*   09488014
*                                                                   *   09489014
*    Free up our local storage ...                                  *   09490014
*                                                                   *   09491014
*-------------------------------------------------------------------*   09492014
                                                                        09493014
         LA    R1,@J_Dynam              A(Local storage)                09494014
         L     R13,4(0,R13)             Rescue A(HSA)                   09495014
                                                                        09496014
         STACK POP,                     Free the stack area            +09497014
               ADDR=(R1),                starting here                 +09498014
               STACK=@_31Bit_Stack_Ptr   on this stack                  09499014
                                                                        09500014
*-------------------------------------------------------------------*   09501014
*                                                                   *   09502014
*    ... and return to caller                                       *   09503014
*                                                                   *   09504014
*-------------------------------------------------------------------*   09505014
                                                                        09506014
         SLR   R15,R15                  Clear return code               09507014
         L     R14,12(0,R13)            Restore return address          09508014
         LM    R0,R12,20(R13)           Restore other registers         09509014
         BSM   0,R14                    and return                      09510014
                                                                        09511014
         LTORG                                                          09512014
                                                                        09513014
                                                                        09514014
@J_Dynam        DSECT                 Dynamic area for J                09515014
@J_Save         DS    18F              O/S Style save area              09516014
@J_DBLWD        DS    D                Work area                        09517014
@J_Temp         DS    CL10             Work area                        09518014
@J_DCB_Ptr      DS    AL4              A(DCB)                           09519014
@J_TIOT_Entry_Ptr DS  AL4              A(TIOT Entry for this DCB)       09520014
@J_JFCB_Ptr     DS    AL4              A(JFCB)                          09521014
                                                                        09522014
@J_Macro        DS    0F               Macro area                       09523014
@J_SHOWCB       DS    CL(J_SHOWCB_Length) SHOWCB Macro                  09524014
                DS    0F                                                09525014
@J_BUFND        DS    FL4              BUFND from ACB                   09526014
@J_BUFSP        DS    FL4              BUFSP from ACB                   09527014
                ORG                                                     09528014
                DS    0D               Alignment                        09529014
@J_DynLen       EQU   *-@J_Dynam      Length of storage required        09530014
                                                                        09531014
SMFSLCT  RSECT                                                          09532014
                                                                        09533014
         DROP  R11,R13                                                  09534014
         POP   USING                                                    09535014
                                                                        09536014
         TITLE 'K0000 - Date/Time Conversion Routine'                   09537014
K0010    DS    0H                                                       09538014
                                                                        09539014
         PUSH  USING                                                    09540014
                                                                        09541014
*---------------------------------------------------------------------* 09542014
*                                                                     * 09543014
*  Routine   : K0010                                                  * 09544014
*                                                                     * 09545014
*  Abstract  : Populate DateConv_Area Dates and Times                 * 09546014
*                                                                     * 09547014
*  Inputs    : R1 ----> DateConv Area                                 * 09548014
*  Outputs   : R15 = 0, DateConv_Area contains converted              * 09549014
*                       Date/Time values.                             * 09550014
*              R15 = 4, DateConv_Area unchanged                       * 09551014
*                                                                     * 09552014
*                                                                     * 09553014
*  Notes     :                                                        * 09554014
*                                                                     * 09555014
*  History:                                                           * 09556014
*              1999/04/24 SDDA030 V1.1                                * 09557014
*                                 Used LIFO Stack for our dynamic     * 09558014
*                                 areas.                              * 09559014
*              ____/__/__ _______                                     * 09560014
*                                                                     * 09561014
*---------------------------------------------------------------------* 09562014
                                                                        09563014
         USING *,R15                                                    09564014
         SAVE  (14,12),T                                                09565014
         LR    R11,R15                  Load permanent base reg         09566014
         DROP  R15                      Free up temp base               09567014
         USING K0010,R11                Assign permanent base           09568014
                                                                        09569014
*-------------------------------------------------------------------*   09570014
*                                                                   *   09571014
*    Get an area on the stack for our local storage                 *   09572014
*                                                                   *   09573014
*-------------------------------------------------------------------*   09574014
                                                                        09575014
         STACK PUSH,                    Get Stack area                 +09576014
               LEN=@K_DynLen,            this long                     +09577014
               STACK=@_31Bit_Stack_Ptr   using this stack               09578014
                                                                        09579014
         LR    R2,R1                    Save its address                09580014
         LR    R0,R1                    A(Our storage)                  09581014
         LA    R1,@K_DynLen              Its length                     09582014
         SLR   R14,R14                  Clear source address            09583014
         SLR   R15,R15                   and length                     09584014
         MVCL  R0,R14                   Clear our storage               09585014
         LR    R1,R2                    Restore A(Our storage)          09586014
                                                                        09587014
*-------------------------------------------------------------------*   09588014
*                                                                   *   09589014
*    Chain our save areas, and restore the important registers      *   09590014
*   that we have destroyed                                          *   09591014
*                                                                   *   09592014
*-------------------------------------------------------------------*   09593014
                                                                        09594014
         ST    R13,4(0,R1)              Chain                           09595014
         ST    R1,8(0,R13)               saveareas                      09596014
         LR    R13,R1                   Load dynam base                 09597014
         USING @K_Dynam,R13             Assign a base                   09598014
         L     R15,@K_Dynam+4           Get A(HSA)                      09599014
         LM    R0,R3,20(R15)            Restore callers registers       09600014
                                                                        09601014
         ST    R1,@K_DateConv_Area_Ptr  Save A(Callers DateConv_Area)   09602014
         MVC   @K_DateConv_Area,0(R1)   Copy callers DateConv_Area      09603014
         LA    R9,@K_DateConv_Area      Point to our DateConv_Area      09604014
         USING DateConv_Area,R9         Tell the assembler              09605014
                                                                        09606014
         TM    DConv_Input_Base,L'DConv_Input_Base  Base Date?          09607014
         BO    K0020                    Yes, skip                       09608014
         TM    DConv_Input_YYMMDD,L'DConv_Input_YYMMDD Std num date?    09609014
         BO    K0020                    Yes, skip                       09610014
         TM    DConv_Input_YYMonDD,L'DConv_Input_YYMonDD Normal Date?   09611014
         BO    K0020                    Yes, skip                       09612014
         TM    DConv_Input_SMFDate,L'DConv_Input_SMFDate SMF?           09613014
         BO    K0020                    Yes, skip                       09614014
         TM    DConv_Input_Julian,L'DConv_Input_Julian Julian Date?     09615014
         BZ    K0180                    No, check time                  09616014
                                                                        09617014
K0020    DS    0H                                                       09618014
         TM    DConv_Input_Base,L'DConv_Input_Base  Base Date?          09619014
         BO    K0110                    Yes, skip                       09620014
                                                                        09621014
         TM    DConv_Input_SMFDate,L'DConv_Input_SMFDate SMF?           09622014
         BNO   K0030                    No, skip                        09623014
         ZAP   @K_DBLWD,=P'1900000'     Yes, set                        09624014
         AP    @K_DBLWD,DConv_Date_SMFDate  up Y2K Date                 09625014
         L     R15,@K_DBLWD+4           Save it                         09626014
         XC    @K_DBLWD+4(2),@K_DBLWD+4 Clear out year                  09627014
         CVB   R0,@K_DBLWD              Binarize Day-of-year            09628014
         STH   R0,DConv_Date_DDD        Save it                         09629014
         SRL   R15,12                   Get rid of ddd                  09630014
         ST    R15,@K_DBLWD+4           Save it                         09631014
         OI    @K_DBLWD+7,X'0F'         Set up sign                     09632014
         CVB   R0,@K_DBLWD              Binarize Year                   09633014
         STH   R0,DConv_Date_YYYY       and save it too                 09634014
K0030    DS    0H                                                       09635014
                                                                        09636014
         LH    R15,DConv_Date_YYYY      No, get Year                    09637014
         LTR   R15,R15                  Is it 0                         09638014
         BNP   K8010                    Yes, error                      09639014
                                                                        09640014
         TM    DConv_Input_YYMMDD,L'DConv_Input_YYMMDD Std num date?    09641014
         BZ    K0040                    No, skip                        09642014
                                                                        09643014
*---------------------------------------------------------------------* 09644014
*                                                                     * 09645014
*   Standard Date (yyyymmdd): dd, mm must be consistent               * 09646014
*                                                                     * 09647014
*---------------------------------------------------------------------* 09648014
                                                                        09649014
         BAS   R14,K6000       Go validate dd, mm                       09650014
         LTR   R15,R15         OK?                                      09651014
         BNZ   K8010           No, error                                09652014
         STH   R0,DConv_Date_DDD Yes, save day-of-year                  09653014
         B     K0100           and go calculate BDate                   09654014
                                                                        09655014
K0040    DS    0H                                                       09656014
         TM    DConv_Input_YYMonDD,L'DConv_Input_YYMonDD Std num date?  09657014
         BZ    K0070                    No, skip                        09658014
                                                                        09659014
*---------------------------------------------------------------------* 09660014
*                                                                     * 09661014
*   Normal Date (yyyymondd): dd, mon must be consistent               * 09662014
*                                                                     * 09663014
*---------------------------------------------------------------------* 09664014
                                                                        09665014
         OC    DConv_Date_Month,=CL(L'DConv_Date_Month)' ' Upper-case   09666014
         LA    R15,K_MONTHS    A(Month-name table)                      09667014
         LA    R14,1           Set month number                         09668014
K0050    DS    0H                                                       09669014
         MVC   @K_DBLWD(3),2(R15) Move month name from table            09670014
         OC    @K_DBLWD(3),=C'   ' Uppercase it                         09671014
         CLC   @K_DBLWD(3),DConv_Date_Month Is this us?                 09672014
         BE    K0060           Yes, skip                                09673014
         LA    R15,11(0,R15)   No, bump to next month                   09674014
         LA    R14,1           Bump month number                        09675014
         CH    R14,=H'12'      Too far?                                 09676014
         BNH   K0050           No, try again                            09677014
         B     K8010           Unknown month, error                     09678014
K0060    DS    0H                                                       09679014
         STC   R14,DConv_Date_MM  Save month number                     09680014
         BAL   R14,K6000       Go validate dd, mm, and yy               09681014
         LTR   R15,R15         OK?                                      09682014
         BNZ   K8010           No, error                                09683014
         STH   R0,DConv_Date_DDD Yes, save day-of-year                  09684014
         B     K0100           and go calculate BDate                   09685014
                                                                        09686014
K0070    DS    0H                                                       09687014
         TM    DConv_Input_Julian,L'DConv_Input_Julian  JDate?          09688014
         BO    K0080                    Yes, do it                      09689014
         TM    DConv_Input_SMFDate,L'DConv_Input_SMFDate No, SMF        09690014
         BZ    K0180                    No, skip                        09691014
K0080    DS    0H                                                       09692014
                                                                        09693014
*---------------------------------------------------------------------* 09694014
*                                                                     * 09695014
*   Julian Date (yyyyddd): yyyy, ddd must be consistent               * 09696014
*                                                                     * 09697014
*---------------------------------------------------------------------* 09698014
                                                                        09699014
         LH    R2,DConv_Date_DDD Get Day of year                        09700014
         LTR   R2,R2           Is it 0                                  09701014
         BNP   K8010           Yes, error                               09702014
                                                                        09703014
         LA    R2,365          Days in a normal year                    09704014
         BAS   R14,K7000       Go check leap year                       09705014
         LTR   R15,R15         Is it a leap year?                       09706014
         BZ    K0090           No, skip                                 09707014
         LA    R2,1(0,R2)      Yes, bump max days                       09708014
K0090    DS    0H                                                       09709014
         CH    R2,DConv_Date_DDD Day-of-year too big?                   09710014
         BL    K8010           Yes, error                               09711014
                                                                        09712014
K0100    DS    0H                                                       09713014
                                                                        09714014
*---------------------------------------------------------------------* 09715014
*                                                                     * 09716014
*   Calculate Base Date using the Julian Date in DConv_Date_YYYY and  * 09717014
*    DConv_Date_DDD                                                   * 09718014
*   In REXX terms,                                                    * 09719014
*      YYYY = YYYY - 1             /*  Based on Year 0001          */ * 09720014
*      BDate = (YYYY * 365) +,     /*  Days per year        plus   */ * 09721014
*              (YYYY % 4)   -,     /*  Leap days            minus  */ * 09722014
*              (YYYY % 100) +,     /*  non-Leap Centuries   plus   */ * 09723014
*              (YYYY % 400) +,     /*  Leap centuries       plus   */ * 09724014
*              DDD          -,     /*  Days in year         minus  */ * 09725014
*              1                   /*  1 to get complete days      */ * 09726014
*                                                                     * 09727014
*---------------------------------------------------------------------* 09728014
                                                                        09729014
         LH    R2,DConv_Date_YYYY Get year                              09730014
         SH    R2,=H'1'         relative to year 0001                   09731014
         LR    R15,R2          Year *                                   09732014
         MH    R15,=H'365'            365                               09733014
         LR    R0,R15          Initialize accumulator                   09734014
         LR    R15,R2          Year                                     09735014
         SLR   R14,R14              %                                   09736014
         D     R14,=F'4'              4                                 09737014
         AR    R0,R15          Bump accumulator                         09738014
         LR    R15,R2          Year                                     09739014
         SLR   R14,R14              %                                   09740014
         D     R14,=F'100'            100                               09741014
         SR    R0,R15          Reduce accumulator                       09742014
         LR    R15,R2          Year                                     09743014
         SLR   R14,R14              %                                   09744014
         D     R14,=F'400'            400                               09745014
         AR    R0,R15          Bump accumulator                         09746014
         AH    R0,DConv_Date_DDD  Add days in current year              09747014
         SH    R0,=H'1'        Complete days                            09748014
         ST    R0,DConv_Date_Base  Initialize Base Date                 09749014
         B     K0110           And skip                                 09750014
K0110    DS    0H                                                       09751014
                                                                        09752014
*---------------------------------------------------------------------* 09753014
*                                                                     * 09754014
*   Calculate various bits from the base date:                        * 09755014
*     DConv_Date_YYYY                                                 * 09756014
*     DConv_Date_DDD                                                  * 09757014
*     DConv_Date_MM                                                   * 09758014
*     DConv_Date_DD                                                   * 09759014
*                                                                     * 09760014
*   First, calculate the Year, and Day-of-year ...                    * 09761014
*                                                                     * 09762014
*---------------------------------------------------------------------* 09763014
         SPACE                                                          09764014
         L     R15,DConv_Date_Base Get the base date                    09765014
         SLR   R14,R14         set up for divide                        09766014
         D     R14,=F'365'     Do it                                    09767014
         STH   R15,DConv_Date_YYYY Save Year                            09768014
         SPACE                                                          09769014
*---------------------------------------------------------------------* 09770014
*                                                                     * 09771014
*   ... and adjust the Day-of-year for leap years                     * 09772014
*                                                                     * 09773014
*---------------------------------------------------------------------* 09774014
         SPACE                                                          09775014
         LR    R0,R14          Get day-of-year                          09776014
         LH    R15,DConv_Date_YYYY Get number                           09777014
         SLR   R14,R14          of                                      09778014
         D     R14,=F'4'          leap-days                             09779014
         SR    R0,R15          Reduce day-of-year                       09780014
         LH    R15,DConv_Date_YYYY Get number                           09781014
         SLR   R14,R14          of                                      09782014
         D     R14,=F'100'        centuries                             09783014
         AR    R0,R15          Bump day-of-year                         09784014
         LH    R15,DConv_Date_YYYY Get number                           09785014
         SLR   R14,R14          of                                      09786014
         D     R14,=F'400'        leap centuries                        09787014
         SR    R0,R15          Reduce day-of-year                       09788014
         A     R0,=FL4'1'      Bump for incomplete day                  09789014
         SPACE                                                          09790014
*---------------------------------------------------------------------* 09791014
*                                                                     * 09792014
*   Day-of-year is almost certainly negative at this point, so we     * 09793014
*   will adjust it (and year) until it is greater than 1.             * 09794014
*                                                                     * 09795014
*---------------------------------------------------------------------* 09796014
         SPACE                                                          09797014
K0120    DS    0H                                                       09798014
         C     R0,=FL4'1'      Is Day-of-year OK?                       09799014
         BNL   K0140           Yes, skip                                09800014
         A     R0,=F'365'      No, bump a year's worth                  09801014
         BAS   R14,K7000       Check for leap year                      09802014
         LTR   R15,R15         Is this a leap year?                     09803014
         BZ    K0130           No, skip                                 09804014
         A     R0,=FL4'1'      Yes, bump Day-of-year                    09805014
K0130    DS    0H                                                       09806014
         LH    R15,DConv_Date_YYYY Get Year                             09807014
         S     R15,=FL4'1'     Reduce it                                09808014
         STH   R15,DConv_Date_YYYY and save it                          09809014
         B     K0120           Keep checking                            09810014
K0140    DS    0H                                                       09811014
         STH   R0,DConv_Date_DDD Save adjusted Day-of-year              09812014
         SPACE                                                          09813014
*---------------------------------------------------------------------* 09814014
*                                                                     * 09815014
*   Adjust Year, since it it relative to 0001                         * 09816014
*                                                                     * 09817014
*---------------------------------------------------------------------* 09818014
         SPACE                                                          09819014
         LH    R15,DConv_Date_YYYY Get Year                             09820014
         A     R15,=FL4'1'     Bump it                                  09821014
         STH   R15,DConv_Date_YYYY and save it                          09822014
                                                                        09823014
*---------------------------------------------------------------------* 09824014
*                                                                     * 09825014
*   Create the SMFDate from YYYY and DDD                              * 09826014
*                                                                     * 09827014
*---------------------------------------------------------------------* 09828014
                                                                        09829014
         LH    R0,DConv_Date_YYYY  Get YYYY                             09830014
         CVD   R0,@K_DBLWD         Pack it                              09831014
         L     R0,@K_DBLWD+4       Get packed year                      09832014
         SRL   R0,4                Get rid of sign                      09833014
         STH   R0,DConv_Date_SMFDate Move YYYY                          09834014
         LH    R0,DConv_Date_DDD   Get DDD                              09835014
         CVD   R0,@K_DBLWD         Pack it                              09836014
         MVC   DConv_Date_SMFDate+2(2),@K_DBLWD+6 Move DDD              09837014
         SP    DConv_Date_SMFDate,=P'1900000' Convert to SMF Date       09838014
                                                                        09839014
*---------------------------------------------------------------------* 09840014
*                                                                     * 09841014
*   Copy the Days-per-month table, and adjust the number of days in   * 09842014
*  February, if this a leap year.                                     * 09843014
*                                                                     * 09844014
*---------------------------------------------------------------------* 09845014
         SPACE                                                          09846014
         MVC   @K_Months,K_Months Copy the table                        09847014
         BAS   R14,K7000       Check for leap year                      09848014
         LTR   R15,R15         Is this a leap year?                     09849014
         BZ    K0150           No, skip                                 09850014
         LH    R15,@K_Months+11 Yes, get days in Feb                    09851014
         LA    R15,1(0,R15)    Bump it                                  09852014
         STH   R15,@K_Months+11 Save it                                 09853014
K0150    DS    0H                                                       09854014
                                                                        09855014
*---------------------------------------------------------------------* 09856014
*                                                                     * 09857014
*   Calculate the month number, and the day-of-month                  * 09858014
*                                                                     * 09859014
*---------------------------------------------------------------------* 09860014
                                                                        09861014
         LH    R0,DConv_Date_DDD Get day-of-year                        09862014
         LA    R14,1           January                                  09863014
         LA    R15,@K_Months   A(Days-per-month table)                  09864014
K0160    DS    0H                                                       09865014
         CH    R0,0(0,R15)     Is this our month                        09866014
         BNH   K0170           Yes, skip                                09867014
         SH    R0,0(0,R15)     No, reduce days                          09868014
         LA    R14,1(0,R14)    Bump month number                        09869014
         LA    R15,11(0,R15)   Bump month pointer                       09870014
         B     K0160           and keep going                           09871014
K0170    DS    0H                                                       09872014
         STC   R0,DConv_Date_DD Save Day-of-month                       09873014
         STC   R14,DConv_Date_MM and month number                       09874014
         MVC   DConv_Date_Month,2(R15)  Move month name                 09875014
                                                                        09876014
         L     R1,DConv_Date_Base Get base date                         09877014
         SLR   R0,R0           Clear for divide                         09878014
         D     R0,=F'7'        Get weekday (0=Mon, 1=Tue ...)           09879014
         STC   R0,DConv_Date_DOW Save it                                09880014
         LR    R15,R0          Put in R15                               09881014
         SLL   R15,3           Weekday num * 8                          09882014
         AR    R15,R0          Weekday num * 9                          09883014
         LA    R15,K_DAYS(R15) Point to weekday name                    09884014
         MVC   DConv_Date_Day,0(R15)  Move weekday name                 09885014
K0180    DS    0H                                                       09886014
                                                                        09887014
*---------------------------------------------------------------------* 09888014
*                                                                     * 09889014
*   Do Time conversions                                               * 09890014
*                                                                     * 09891014
*---------------------------------------------------------------------* 09892014
                                                                        09893014
         TM    DConv_Input_Time,L'DConv_Input_Time  To hh,mm,ss?        09894014
         BNO   K0190           No, skip                                 09895014
                                                                        09896014
         L     R15,DConv_Time  Yes, get secs*100 since midnight         09897014
         C     R15,=FL4'8640000' Is it too big?                         09898014
         BH    K8010           Yes, error                               09899014
         SLR   R14,R14         Set up for divide                        09900014
         D     R14,=FL4'360000' Get hours                               09901014
         STC   R15,DConv_Time_hh Save it                                09902014
         SRDL  R14,32          Set up for divide                        09903014
         D     R14,=FL4'6000'  Get minutes                              09904014
         STC   R15,DConv_Time_mm Save them                              09905014
         SRDL  R14,32          Set up for divide                        09906014
         D     R14,=FL4'100'   Get seconds                              09907014
         STC   R15,DConv_Time_ss Save them                              09908014
         STC   R14,DConv_Time_tt Save hundredths                        09909014
         B     K0200           and exit                                 09910014
                                                                        09911014
K0190    DS    0H                                                       09912014
         TM    DConv_Input_hhmmss,L'DConv_Input_hhmmss  To secs*100?    09913014
         BNO   K0200           No, skip                                 09914014
                                                                        09915014
         SLR   R0,R0           Clear accumulator                        09916014
         SLR   R15,R15         Clear work register                      09917014
         IC    R15,DConv_Time_hh Get hours                              09918014
         L     R14,=FL4'360000' multiplier for secs*100                 09919014
         MR    R14,R14         Convert it                               09920014
         AR    R0,R15          Add to accumulator                       09921014
         SLR   R15,R15         Clear work register                      09922014
         IC    R15,DConv_Time_mm Get minutes                            09923014
         L     R14,=FL4'6000'  multiplier for secs*100                  09924014
         MR    R14,R14         Convert it                               09925014
         AR    R0,R15          Add to accumulator                       09926014
         SLR   R15,R15         Clear work register                      09927014
         IC    R15,DConv_Time_ss Get seconds                            09928014
         L     R14,=FL4'100' multiplier for secs*100                    09929014
         MR    R14,R14         Convert it                               09930014
         AR    R0,R15          Add to accumulator                       09931014
         SLR   R15,R15         Clear work register                      09932014
         IC    R15,DConv_Time_tt Get hundredths of seconds              09933014
         AR    R0,R15          Add to accumulator                       09934014
         C     R0,=FL4'8640000' Is it too big?                          09935014
         BH    K8010           Yes, error                               09936014
         ST    R0,DConv_Time   Save it                                  09937014
                                                                        09938014
K0200    DS    0H                                                       09939014
                                                                        09940014
         L     R15,@K_DateConv_Area_Ptr  A(Callers DateConv_Area)       09941014
         MVC   0(DConv_Area_Length,R15),@K_DateConv_Area Copy ours      09942014
         SLR   R15,R15         Clear return code                        09943014
         B     K9010           and exit                                 09944014
                                                                        09945014
K6000    DS    0H                                                       09946014
                                                                        09947014
*---------------------------------------------------------------------* 09948014
*                                                                     * 09949014
*   Validate DConv_Date_MM, DConv_Date_DD                             * 09950014
*   If valid, day-of-year is returned in R0                           * 09951014
*                                                                     * 09952014
*---------------------------------------------------------------------* 09953014
                                                                        09954014
         ST    R14,@K_K6Save   Save return address                      09955014
         ST    R1,@K_K6Save+4  and R1                                   09956014
         SLR   R15,R15         Clear work register                      09957014
         IC    R15,DConv_Date_MM  Get month                             09958014
         LTR   R15,R15         Is it 0?                                 09959014
         BZ    K6040           Yes, invalid                             09960014
         CH    R15,=H'12'      No, is it too big?                       09961014
         BH    K6040           Yes, invalid                             09962014
         MVC   @K_Months,K_Months Move Days-per-month table             09963014
         BAS   R14,K7000       Check leap year                          09964014
         LTR   R15,R15         Is it?                                   09965014
         BZ    K6010           No, skip                                 09966014
         LH    R15,@K_Months+11 Yes, get February days                  09967014
         AH    R15,=H'1'       Bump it                                  09968014
         STH   R15,@K_Months+11 and save it                             09969014
K6010    DS    0H                                                       09970014
         SLR   R15,R15         Clear work register                      09971014
         IC    R15,DConv_Date_MM Get month number                       09972014
         MH    R15,=HL2'11'    Calculate offset                         09973014
         LA    R15,@K_Months-11(R15) Point to days in our month         09974014
         SLR   R0,R0           Clear work register                      09975014
         IC    R0,DConv_Date_DD Get day-of-month                        09976014
         LTR   R0,R0           Is it 0?                                 09977014
         BZ    K6040           Yes, error                               09978014
         CH    R0,0(R15)       Are Days too big?                        09979014
         BH    K6040           Yes, error                               09980014
         LA    R14,@K_Months   A(Our Days-per-month table)              09981014
K6020    DS    0H                                                       09982014
         CR    R14,R15         Are we done?                             09983014
         BNL   K6030           Yes, skip                                09984014
         AH    R0,0(R14)       No, bump total days                      09985014
         LA    R14,11(0,R14)   point to next month                      09986014
         B     K6020           and try again                            09987014
K6030    DS    0H                                                       09988014
         SLR   R15,R15         Clear return code                        09989014
         B     K6050           and exit                                 09990014
K6040    DS    0H                                                       09991014
         LA    R15,4           Error in date                            09992014
         SLR   R0,R0           Clear day-of-year                        09993014
         B     K6050                                                    09994014
K6050    DS    0H                                                       09995014
         L     R14,@K_K6Save   Restore return address                   09996014
         L     R1,@K_K6Save+4  Restore R1                               09997014
         BSM   0,R14           and return                               09998014
                                                                        09999014
                                                                        10000014
K7000    DS    0H                                                       10001014
                                                                        10002014
*---------------------------------------------------------------------* 10003014
*                                                                     * 10004014
*  Purpose   : This routine will determine if the year passed as a    * 10005014
*              parm is a leap year                                    * 10006014
*                                                                     * 10007014
*  Input     : DConv_Date_YYYY                                        * 10008014
*                                                                     * 10009014
*  Output    : R15 = 0: Non-Leap Year                                 * 10010014
*                    4: Leap Year                                     * 10011014
*                                                                     * 10012014
*---------------------------------------------------------------------* 10013014
                                                                        10014014
         STM   R0,R1,@K_K7Save    Save our registers                    10015014
         LH    R1,DConv_Date_YYYY Get the Year                          10016014
                                                                        10017014
*---------------------------------------------------------------------* 10018014
*                                                                     * 10019014
*   If it is not divisible by 4, it is not a leap year                * 10020014
*                                                                     * 10021014
*---------------------------------------------------------------------* 10022014
         SPACE                                                          10023014
         SLR   R0,R0           Clear for divide                         10024014
         D     R0,=FL4'4'      Divide by 4                              10025014
         LTR   R0,R0           Any remainder?                           10026014
         BNZ   K7020           Yes, not a leap year                     10027014
                                                                        10028014
*---------------------------------------------------------------------* 10029014
*                                                                     * 10030014
*   If it also not divisible by 100, it is a leap year                * 10031014
*                                                                     * 10032014
*---------------------------------------------------------------------* 10033014
                                                                        10034014
         LH    R1,DConv_Date_YYYY Get year                              10035014
         SLR   R0,R0           Clear for divide                         10036014
         D     R0,=FL4'100'    Divide by 100                            10037014
         LTR   R0,R0           Any remainder?                           10038014
         BNZ   K7030           Yes, a leap year                         10039014
                                                                        10040014
*---------------------------------------------------------------------* 10041014
*                                                                     * 10042014
*   If it also divisible by 400, it is a leap year                    * 10043014
*                                                                     * 10044014
*---------------------------------------------------------------------* 10045014
                                                                        10046014
         LH    R1,DConv_Date_YYYY Get year                              10047014
         SLR   R0,R0           Clear for divide                         10048014
         D     R0,=FL4'400'    Divide by 400                            10049014
         LTR   R0,R0           Any remainder?                           10050014
         BNZ   K7020           Yes, not a leap year                     10051014
         B     K7030           Must be a leap year                      10052014
K7020    DS    0H                                                       10053014
                                                                        10054014
*---------------------------------------------------------------------* 10055014
*                                                                     * 10056014
*   Not a leap year, clear the return code                            * 10057014
*                                                                     * 10058014
*---------------------------------------------------------------------* 10059014
                                                                        10060014
         SLR   R15,R15         Clear return code                        10061014
         B     K7090           and exit                                 10062014
K7030    DS    0H                                                       10063014
                                                                        10064014
*---------------------------------------------------------------------* 10065014
*                                                                     * 10066014
*   A leap year, set the return code                                  * 10067014
*                                                                     * 10068014
*---------------------------------------------------------------------* 10069014
                                                                        10070014
         LA    R15,4           Set return code                          10071014
         B     K7090           and exit                                 10072014
K7090    DS    0H                                                       10073014
                                                                        10074014
*---------------------------------------------------------------------* 10075014
*                                                                     * 10076014
*   Exit (R15 contains the return code)                               * 10077014
*                                                                     * 10078014
*---------------------------------------------------------------------* 10079014
                                                                        10080014
         LM    R0,R1,@K_K7Save Restore our registers                    10081014
         BSM   0,R14           and return                               10082014
K8010    DS    0H                                                       10083014
                                                                        10084014
*---------------------------------------------------------------------* 10085014
*                                                                     * 10086014
*   DConv fields contain invalid data, exit with error                * 10087014
*                                                                     * 10088014
*---------------------------------------------------------------------* 10089014
                                                                        10090014
         LA    R15,4           Set return code                          10091014
         B     K9010           and exit                                 10092014
                                                                        10093014
K9010    DS    0H                                                       10094014
                                                                        10095014
*-------------------------------------------------------------------*   10096014
*                                                                   *   10097014
*    Free up our local storage ...                                  *   10098014
*                                                                   *   10099014
*-------------------------------------------------------------------*   10100014
                                                                        10101014
         LR    R2,R15                   Save return code                10102014
         LA    R1,@K_Dynam              A(Local storage)                10103014
         L     R13,4(0,R13)             Rescue A(HSA)                   10104014
                                                                        10105014
         STACK POP,                     Free the stack area            +10106014
               ADDR=(R1),                starting here                 +10107014
               STACK=@_31Bit_Stack_Ptr   on this stack                  10108014
                                                                        10109014
         LR    R15,R2                   Restore return code             10110014
                                                                        10111014
*-------------------------------------------------------------------*   10112014
*                                                                   *   10113014
*    ... and return to caller                                       *   10114014
*                                                                   *   10115014
*-------------------------------------------------------------------*   10116014
                                                                        10117014
         L     R14,12(0,R13)            Restore return address          10118014
         LM    R0,R12,20(R13)           Restore other registers         10119014
         BSM   0,R14                    and return                      10120014
                                                                        10121014
                                                                        10122014
K_Months DC    HL2'31',C'January  '                                     10123014
         DC    HL2'28',C'February '                                     10124014
         DC    HL2'31',C'March    '                                     10125014
         DC    HL2'30',C'April    '                                     10126014
         DC    HL2'31',C'May      '                                     10127014
         DC    HL2'30',C'June     '                                     10128014
         DC    HL2'31',C'July     '                                     10129014
         DC    HL2'31',C'August   '                                     10130014
         DC    HL2'30',C'September'                                     10131014
         DC    HL2'31',C'October  '                                     10132014
         DC    HL2'30',C'November '                                     10133014
         DC    HL2'31',C'December '                                     10134014
K_Months_Length EQU  *-K_Months                                         10135014
                                                                        10136014
K_Days   DC    C'Monday   '                                             10137014
         DC    C'Tuesday  '                                             10138014
         DC    C'Wednesday'                                             10139014
         DC    C'Thursday '                                             10140014
         DC    C'Friday   '                                             10141014
         DC    C'Saturday '                                             10142014
         DC    C'Sunday   '                                             10143014
                                                                        10144014
         LTORG                                                          10145014
                                                                        10146014
@K_Dynam        DSECT                 Dynamic area for K                10147014
@K_Save         DS    18F              O/S Style save area              10148014
@K_DateConv_Area_Ptr DS AL4            A(Callers DateConv_Area)         10149014
@K_DBLWD        DS    D                Work area                        10150014
@K_DateConv_Area DS   CL(DConv_Area_Length)     Our area                10151014
@K_K6Save       DS    2AL4             K6000 save area                  10152014
@K_K7Save       DS    2AL4             K7000 save area                  10153014
@K_Months DS   CL(K_Months_Length) Month table                          10154014
                DS    0D               Alignment                        10155014
@K_DynLen       EQU   *-@K_Dynam      Length of storage required        10156014
                                                                        10157014
SMFSLCT  RSECT                                                          10158014
                                                                        10159014
         DROP  R11,R13                                                  10160014
         POP   USING                                                    10161014
                                                                        10162014
         TITLE 'L0000 - List Selection Ctiteria'                        10163014
L0010    DS    0H                                                       10164014
                                                                        10165014
         PUSH  USING                                                    10166014
                                                                        10167014
*---------------------------------------------------------------------* 10168014
*                                                                     * 10169014
*  Routine   : L0010                                                  * 10170014
*                                                                     * 10171014
*  Abstract  : List selection criteria                                * 10172014
*                                                                     * 10173014
*  Inputs    : R1 ----> ODT entry                                     * 10174014
*  Outputs   : R15 = 0                                                * 10175014
*                                                                     * 10176014
*                                                                     * 10177014
*  Notes     :                                                        * 10178014
*                                                                     * 10179014
*  History:                                                           * 10180014
*              2002/02/07 SDDA030 V1.4                                * 10181014
*                                 New. This used to be part of B0010, * 10182014
*                                 but we ran out of base register.    * 10183014
*              ____/__/__ _______                                     * 10184014
*                                                                     * 10185014
*---------------------------------------------------------------------* 10186014
                                                                        10187014
         USING *,R15                                                    10188014
         SAVE  (14,12),T                                                10189014
         LR    R11,R15                  Load permanent base reg         10190014
         DROP  R15                      Free up temp base               10191014
         USING L0010,R11                Assign permanent base           10192014
                                                                        10193014
*-------------------------------------------------------------------*   10194014
*                                                                   *   10195014
*    Get an area on the stack for our local storage                 *   10196014
*                                                                   *   10197014
*-------------------------------------------------------------------*   10198014
                                                                        10199014
         STACK PUSH,                    Get Stack area                 +10200014
               LEN=@L_DynLen,            this long                     +10201014
               STACK=@_31Bit_Stack_Ptr   using this stack               10202014
                                                                        10203014
         LR    R2,R1                    Save its address                10204014
         LR    R0,R1                    A(Our storage)                  10205014
         LA    R1,@L_DynLen              Its length                     10206014
         SLR   R14,R14                  Clear source address            10207014
         SLR   R15,R15                   and length                     10208014
         MVCL  R0,R14                   Clear our storage               10209014
         LR    R1,R2                    Restore A(Our storage)          10210014
                                                                        10211014
*-------------------------------------------------------------------*   10212014
*                                                                   *   10213014
*    Chain our save areas, and restore the important registers      *   10214014
*   that we have destroyed                                          *   10215014
*                                                                   *   10216014
*-------------------------------------------------------------------*   10217014
                                                                        10218014
         ST    R13,4(0,R1)              Chain                           10219014
         ST    R1,8(0,R13)               saveareas                      10220014
         LR    R13,R1                   Load dynam base                 10221014
         USING @L_Dynam,R13             Assign a base                   10222014
         L     R15,@L_Dynam+4           Get A(HSA)                      10223014
         LM    R0,R3,20(R15)            Restore callers registers       10224014
                                                                        10225014
         LR    R10,R1                   Point to the ODT Entry          10226014
         USING ODT_Entry,R10            and tell the assembler          10227014
                                                                        10228014
* - - - - - - - - - - -  D A T E / T I M E     - - - - - - - - - - -*   10229014
                                                                        10230014
         L     R2,@_SYSPRINT_Rec_Ptr  A(SYSPRINT RECORD)                10231014
         CP    ODTE_StartDate,=P'0'   ANY START DATE SPECIFIED?         10232014
         BNE   L0020                  YES, SKIP                         10233014
         CP    ODTE_EndDate,=P'999999' NO, ANY END DATE?                10234014
         BNE   L0020                  YES, SKIP                         10235014
         MVC   10(L'L_Sum3,R2),L_Sum3 MOVE NO DATE/TIME MESSAGE         10236014
         B     L0070                  AND SKIP                          10237014
L0020    DS    0H                                                       10238014
         NI    ODTE_CopyOnly,X'FF'-L'ODTE_CopyOnly Selection criteria   10239014
         MVC   10(L'L_Sum2A,R2),L_Sum2A MOVE FIRST PART OF MESSAGE      10240014
         CP    ODTE_StartDate,=P'0'  ANY START DATE?                    10241014
         BNE   L0030                 YES, SKIP                          10242014
         MVC   10+L'L_Sum2A(20,R2),=C' the start of SYSUT1'             10243014
         LA    R2,10+L'L_Sum2A+20(0,R2) BUMP POINTER                    10244014
         B     L0040                 AND SKIP                           10245014
L0030    DS    0H                                                       10246014
         XC    @L_DateConv_Area,@L_DateConv_Area Clear Date area        10247014
         ZAP   @L_DateConv_Area+DConv_Date_SMFDate-DateConv_Area(L'DCon+10248014
               v_Date_SMFDate),ODTE_StartDate move in Date              10249014
         MVC   @L_DateConv_Area+DConv_Time-DateConv_Area(L'DConv_Time),+10250014
               ODTE_StartTime                 Time too                  10251014
         OI    @L_DateConv_Area+DConv_Input_SMFDate-DateConv_Area,L'DCo+10252014
               nv_Input_SMFDate     Indicate SMF Date                   10253014
         OI    @L_DateConv_Area+DConv_Input_Time-DateConv_Area,L'DConv_*10254014
               Input_Time           Indicate SMF Time                   10255014
         LA    R15,@L_DateConv_Area     Point to Date Conversion area   10256014
         ST    R15,@L_Parms          Save as 1st parm                   10257014
         LA    R15,10+L'L_Sum2A+1(0,R2) A(Output area)                  10258014
         ST    R15,@L_Parms+4        Save as 2nd parm                   10259014
         LA    R15,@L_Return_Ptr     A(Return area)                     10260014
         ST    R15,@L_Parms+8        Save it                            10261014
         LA    R1,@L_Parms           A(Parm pointers)                   10262014
         L     R15,=AL4(F0010)       A(Format routine)                  10263014
         BASR  R14,R15               Go convert date and time           10264014
L0040    DS    0H                                                       10265014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               10266014
         BASR  R14,R15                GET A SYSPRINT RECORD             10267014
         L     R2,@_SYSPRINT_Rec_Ptr  POINT TO IT                       10268014
         MVC   10+L'L_Sum2A-L'L_Sum2B(L'L_Sum2B,R2),L_Sum2B Next part   10269014
         LA    R2,10+L'L_Sum2A+1(0,R2) Point past it                    10270014
         CP    ODTE_EndDate,=P'999999' ANY END DATE?                    10271014
         BNE   L0050                 YES, SKIP                          10272014
         MVC   0(19,R2),=C' the end of SYSUT1'                          10273014
         LA    R2,19(0,R2)           BUMP POINTER                       10274014
         B     L0060                 AND SKIP                           10275014
L0050    DS    0H                                                       10276014
         XC    @L_DateConv_Area,@L_DateConv_Area Clear Date area        10277014
         ZAP   @L_DateConv_Area+DConv_Date_SMFDate-DateConv_Area(L'DCon+10278014
               v_Date_SMFDate),ODTE_EndDate move in Date                10279014
         MVC   @L_DateConv_Area+DConv_Time-DateConv_Area(L'DConv_Time),+10280014
               ODTE_EndTime                 Time too                    10281014
         OI    @L_DateConv_Area+DConv_Input_SMFDate-DateConv_Area,L'DCo+10282014
               nv_Input_SMFDate     Indicate SMF Date                   10283014
         OI    @L_DateConv_Area+DConv_Input_Time-DateConv_Area,L'DConv_+10284014
               Input_Time           Indicate SMF Time                   10285014
         LA    R15,@L_DateConv_Area     Point to Date Conversion area   10286014
         ST    R15,@L_Parms          Save as 1st parm                   10287014
         LA    R15,0(0,R2)           A(Output area)                     10288014
         ST    R15,@L_Parms+4        Save as 2nd parm                   10289014
         LA    R15,@L_Return_Ptr     A(Return area)                     10290014
         ST    R15,@L_Parms+8        Save it                            10291014
         LA    R1,@L_Parms           A(Parm pointers)                   10292014
         L     R15,=AL4(F0010)       A(Format routine)                  10293014
         BASR  R14,R15               Go convert date and time           10294014
         L     R2,@L_Return_Ptr      Point past                         10295014
         LA    R2,1(0,R2)              Date/Time                        10296014
L0060    DS    0H                                                       10297014
         MVC   0(L'L_Sum2C,R2),L_Sum2C MOVE LAST PART OF MESSAGE        10298014
L0070    DS    0H                                                       10299014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               10300014
         BASR  R14,R15                GET A SYSPRINT RECORD             10301014
                                                                        10302014
* - - - - - - - - - -   P E R I O D            - - - - - - - - - - -*   10303014
                                                                        10304014
         ICM   R15,B'1111',ODTE_PeriodStart PERIOD START TIME           10305014
         BNZ   L0080                  OK, PRINT PERIOD MESSAGE          10306014
         L     R15,ODTE_PeriodEnd     NO, GET PERIOD END                10307014
         C     R15,=F'8640000'        IS THERE ONE?                     10308014
         BE    L0090                  NO, SKIP                          10309014
L0080    DS    0H                                                       10310014
         NI    ODTE_CopyOnly,X'FF'-L'ODTE_CopyOnly Selection criteria   10311014
         L     R2,@_SYSPRINT_Rec_Ptr  POINT TO IT                       10312014
         MVC   10(20,R2),=C'Only records between'                       10313014
         XC    @L_DateConv_Area,@L_DateConv_Area Clear Date area        10314014
         MVC   @L_DateConv_Area+DConv_Time-DateConv_Area(L'DConv_Time),+10315014
               ODTE_PeriodStart             Time too                    10316014
         OI    @L_DateConv_Area+DConv_Input_Time-DateConv_Area,L'DConv_+10317014
               Input_Time           Indicate SMF Time                   10318014
         LA    R1,@L_DateConv_Area   Point to Date Conversion area      10319014
         L     R15,=AL4(K0010)       A(Format routine)                  10320014
         BASR  R14,R15               Go get Time                        10321014
         SLR   R0,R0                 Clear work register                10322014
         IC    R0,@L_DateConv_Area+DConv_Time_hh-DateConv_Area          10323014
         CVD   R0,@L_DBLWD           Pack it                            10324014
         UNPK  30(3,R2),@L_DBLWD+6(2) Unpack it                         10325014
         MVI   30(R2),C' '           Clear spurious char                10326014
         OI    32(R2),X'F0'          Make it readable                   10327014
         SLR   R0,R0                 Clear work register                10328014
         IC    R0,@L_DateConv_Area+DConv_Time_mm-DateConv_Area          10329014
         CVD   R0,@L_DBLWD           Pack it                            10330014
         UNPK  33(3,R2),@L_DBLWD+6(2) Unpack it                         10331014
         MVI   33(R2),C':'           Clear spurious char                10332014
         OI    35(R2),X'F0'          Make it readable                   10333014
         SLR   R0,R0                 Clear work register                10334014
         IC    R0,@L_DateConv_Area+DConv_Time_ss-DateConv_Area          10335014
         CVD   R0,@L_DBLWD           Pack it                            10336014
         UNPK  36(3,R2),@L_DBLWD+6(2) Unpack it                         10337014
         MVI   36(R2),C':'           Clear spurious char                10338014
         OI    38(R2),X'F0'          Make it readable                   10339014
         SLR   R0,R0                 Clear work register                10340014
         IC    R0,@L_DateConv_Area+DConv_Time_tt-DateConv_Area          10341014
         CVD   R0,@L_DBLWD           Pack it                            10342014
         UNPK  39(3,R2),@L_DBLWD+6(2) Unpack it                         10343014
         MVI   39(R2),C'.'           Clear spurious char                10344014
         OI    41(R2),X'F0'          Make it readable                   10345014
                                                                        10346014
         MVC   42(4,R2),=C' and'                                        10347014
                                                                        10348014
         XC    @L_DateConv_Area,@L_DateConv_Area Clear Date area        10349014
         MVC   @L_DateConv_Area+DConv_Time-DateConv_Area(L'DConv_Time),+10350014
               ODTE_PeriodEnd               Time too                    10351014
         OI    @L_DateConv_Area+DConv_Input_Time-DateConv_Area,L'DConv_+10352014
               Input_Time           Indicate SMF Time                   10353014
         LA    R1,@L_DateConv_Area   Point to Date Conversion area      10354014
         L     R15,=AL4(K0010)       A(Format routine)                  10355014
         BASR  R14,R15               Go get Time                        10356014
         SLR   R0,R0                 Clear work register                10357014
         IC    R0,@L_DateConv_Area+DConv_Time_hh-DateConv_Area          10358014
         CVD   R0,@L_DBLWD           Pack it                            10359014
         UNPK  46(3,R2),@L_DBLWD+6(2) Unpack it                         10360014
         MVI   46(R2),C' '           Clear spurious char                10361014
         OI    48(R2),X'F0'          Make it readable                   10362014
         SLR   R0,R0                 Clear work register                10363014
         IC    R0,@L_DateConv_Area+DConv_Time_mm-DateConv_Area          10364014
         CVD   R0,@L_DBLWD           Pack it                            10365014
         UNPK  49(3,R2),@L_DBLWD+6(2) Unpack it                         10366014
         MVI   49(R2),C':'           Clear spurious char                10367014
         OI    51(R2),X'F0'          Make it readable                   10368014
         SLR   R0,R0                 Clear work register                10369014
         IC    R0,@L_DateConv_Area+DConv_Time_ss-DateConv_Area          10370014
         CVD   R0,@L_DBLWD           Pack it                            10371014
         UNPK  52(3,R2),@L_DBLWD+6(2) Unpack it                         10372014
         MVI   52(R2),C':'           Clear spurious char                10373014
         OI    54(R2),X'F0'          Make it readable                   10374014
         SLR   R0,R0                 Clear work register                10375014
         IC    R0,@L_DateConv_Area+DConv_Time_tt-DateConv_Area          10376014
         CVD   R0,@L_DBLWD           Pack it                            10377014
         UNPK  55(3,R2),@L_DBLWD+6(2) Unpack it                         10378014
         MVI   55(R2),C'.'           Clear spurious char                10379014
         OI    57(R2),X'F0'          Make it readable                   10380014
         MVC   58(18,R2),=C' will be eligible.'                         10381014
                                                                        10382014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               10383014
         BASR  R14,R15                GET A SYSPRINT RECORD             10384014
L0090    DS    0H                                                       10385014
                                                                        10386014
* - - - - - - - - - - W E E K E N D S  - - - - - - - - - - - - - - -*   10387014
                                                                        10388014
         TM    ODTE_Ignore_Weekend,L'ODTE_Ignore_Weekend No Sat or Sun? 10389014
         BNO   L0100                  No, OK                            10390014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               10391014
         BASR  R14,R15                GET A SYSPRINT RECORD             10392014
         L     R2,@_SYSPRINT_Rec_Ptr  A(SYSPRINT RECORD                 10393014
         MVC   10(77,R2),=C'SMF Data generated on Saturday and Sunday w+10394014
               ill not be eligible for selection.'                      10395014
                                                                        10396014
         L     R15,=AL4(G0010)       A(SYSPRINT routine)                10397014
         BASR  R14,R15                GET A SYSPRINT RECORD             10398014
L0100    DS    0H                                                       10399014
                                                                        10400014
* - - - - - - - - - - R E C O R D   T Y P E S  - - - - - - - - - - -*   10401014
                                                                        10402014
         L     R2,@_SYSPRINT_Rec_Ptr  A(SYSPRINT RECORD                 10403014
         ICM   R3,B'1111',ODTE_RTT_Ptr  A(RECORD TYPE TABLE)            10404014
         BNZ   L0110                  OK, SKIP                          10405014
         MVC   10(L'L_Sum5,R2),L_Sum5 NO, MOVE NO RECORD TYPE MSG       10406014
         B     L0180                  AND SKIP                          10407014
L0110    DS    0H                                                       10408014
         NI    ODTE_CopyOnly,X'FF'-L'ODTE_CopyOnly Selection criteria   10409014
         MVC   10(L'L_Sum4A,R2),L_Sum4A Move 1st part of message        10410014
         LH    R4,RTT_Entry_Count-RecType_Tbl(R3) Get nbr entries       10411014
         CH    R4,=H'1'               More than 1?                      10412014
         BNH   L0120                  No, skip                          10413014
         MVI   10+L'L_Sum4A(R2),C's'  Yes, make msg plural              10414014
         LA    R2,1(0,R2)             and bump SYSPRINT pointer         10415014
L0120    DS    0H                                                       10416014
         LA    R2,10+L'L_Sum4A+1(0,R2) A(NEXT AVAILABLE BYTE)           10417014
         LR    R15,R2                 Calculate offset                  10418014
         S     R15,@_SYSPRINT_Rec_Ptr   into SYSPRINT record            10419014
         STH   R15,@L_SYSPRINT_Rec_Offset  and save it                  10420014
         LA    R3,RTT_Entry-RecType_Tbl(R3) A(FIRST ENTRY IN TABLE)     10421014
L0130    DS    0H                                                       10422014
         LA    R15,9(0,R2)            A(last byte we might write)       10423014
         S     R15,@_SYSPRINT_Rec_Ptr Length of data we might write     10424014
         CH    R15,@_SYSPRINT_Rec_Len Too long?                         10425014
         BL    L0140                  No, OK                            10426014
         L     R15,=AL4(G0010)        Yes, A(SYSPRINT routine)          10427014
         BASR  R14,R15                GET A SYSPRINT RECORD             10428014
         L     R2,@_SYSPRINT_Rec_Ptr  Get A(New SYSPRINT Record)        10429014
         AH    R2,@L_SYSPRINT_Rec_Offset and point to appropriate byte  10430014
L0140    DS    0H                                                       10431014
         SLR   R0,R0                  CLEAR WORK REGISTER               10432014
         IC    R0,RTTE_RecType-RTT_Entry(R3) GET RECORD TYPE            10433014
         CVD   R0,@L_DBLWD            PACK IT                           10434014
         MVC   @L_DBLWD(4),=X'40202120' MOVE MASK                       10435014
         LA    R1,@L_DBLWD+3          A(LAST POSSIBLE POSITION)         10436014
         EDMK  @L_DBLWD(4),@L_DBLWD+6 EDIT IT                           10437014
         LA    R15,@L_DBLWD+3         A(LAST POSSIBLE POSITION)         10438014
         SR    R15,R1                 R3 = EXEC LEN OF EDITED RECTYPE   10439014
         EX    R15,L_MVC              MOVE IT                           10440014
         LA    R2,1(R15,R2)           A(NEXT BYTE)                      10441014
         TM    RTTE_SubType_Present-RTT_Entry(R3),L'RTTE_SubType_Presen+10442014
               t                      Any subtype?                      10443014
         BNO   L0150                  No, skip subtype stuff            10444014
         LH    R0,RTTE_SubType-RTT_Entry(R3) YES, GET SUBTYPE           10445014
         CVD   R0,@L_DBLWD            PACK IT                           10446014
         MVC   @L_DBLWD(4),=X'40202120' MOVE MASK                       10447014
         LA    R1,@L_DBLWD+3          A(LAST POSSIBLE POSITION)         10448014
         EDMK  @L_DBLWD(4),@L_DBLWD+6 EDIT IT                           10449014
         LA    R15,@L_DBLWD+3         A(LAST POSSIBLE POSITION)         10450014
         SR    R15,R1                 R15 = EXEC LEN OF SUBTYPE         10451014
         MVI   0(R2),C'('             OPENING PAREN                     10452014
         LA    R2,1(0,R2)             POINT PAST IT                     10453014
         EX    R15,L_MVC              MOVE SUBTYPE                      10454014
         LA    R2,1(R15,R2)           POINT TO NEXT BYTE                10455014
         MVI   0(R2),C')'             CLOSE PAREN                       10456014
         LA    R2,1(0,R2)             AND BUMP POINTER                  10457014
L0150    DS    0H                                                       10458014
         MVI   0(R2),C','             MOVE SEPARATOR                    10459014
         LA    R2,2(0,R2)             POINT PAST IT                     10460014
         LA    R3,RTTE_Length(0,R3)   A(NEXT ENTRY                      10461014
         BCT   R4,L0130               and do it                         10462014
                                                                        10463014
         SH    R2,=H'2'               BACK UP TO LAST COMMA             10464014
         MVI   0(R2),C' '             CLEAR IT                          10465014
         LA    R15,1+L'L_Sum4B+2+L'L_Sum4C(0,R2) A(last byte)           10466014
         S     R15,@_SYSPRINT_Rec_Ptr Length of data we might write     10467014
         CH    R15,@_SYSPRINT_Rec_Len Too long?                         10468014
         BL    L0160                  No, OK                            10469014
         L     R15,=AL4(G0010)        Yes, A(SYSPRINT routine)          10470014
         BASR  R14,R15                GET A SYSPRINT RECORD             10471014
         L     R2,@_SYSPRINT_Rec_Ptr  Get A(New SYSPRINT Record)        10472014
         AH    R2,@L_SYSPRINT_Rec_Offset and point to appropriate byte  10473014
L0160    DS    0H                                                       10474014
         MVC   1(L'L_Sum4B,R2),L_Sum4B MOVE NEXT PART OF MSG            10475014
         LA    R2,1+L'L_Sum4B(0,R2)   POINT PAST IT                     10476014
         L     R15,ODTE_RTT_Ptr       A(RECORD TYPE TABLE)              10477014
         TM    RTT_Entry_Exclude-RecType_TBL(R15),L'RTT_Entry_Exclude   10478014
         BZ    L0170                  INCLUDEing, skip                  10479014
         MVC   0(2,R2),=C'in'         EXCLUDEing, IN of INELIGIBLE      10480014
         LA    R2,2(0,R2)             AND POINT PAST IT                 10481014
L0170    DS    0H                                                       10482014
         MVC   0(L'L_Sum4C,R2),L_Sum4C MOVE LAST PART OF MESSAGE        10483014
L0180    DS    0H                                                       10484014
                                                                        10485014
         L     R15,=AL4(G0010)       A(SYSPRINT routine)                10486014
         BASR  R14,R15                GET A SYSPRINT RECORD             10487014
                                                                        10488014
* - - - - - - - - - -      J O B N A M E S     - - - - - - - - - - -*   10489014
                                                                        10490014
         L     R2,@_SYSPRINT_Rec_Ptr  A(NEW PRINT RECORD)               10491014
         ICM   R3,B'1111',ODTE_JNT_Ptr A(JOBNAME TABLE)                 10492014
         BNZ   L0190                  OK, SKIP                          10493014
         MVC   10(L'L_Sum6,R2),L_Sum6 NO, SAY SO                        10494014
         B     L0270                  AND SKIP                          10495014
L0190    DS    0H                                                       10496014
         NI    ODTE_CopyOnly,X'FF'-L'ODTE_CopyOnly Selection criteria   10497014
         MVC   10(L'L_Sum7A,R2),L_Sum7A MOVE FIRST PART OF MESSAGE      10498014
         LH    R4,JNT_Entry_Count-JobName_Tbl(R3) GET NBR JOBNAMES      10499014
         CH    R4,=H'1'               MORE THAN 1?                      10500014
         BNH   L0200                  NO, SKIP                          10501014
         MVI   10+L'L_Sum7A(R2),C's'  YES, MAKE IT PLURAL               10502014
         LA    R2,1(0,R2)             BUMP IT                           10503014
L0200    DS    0H                                                       10504014
         LA    R2,10+L'L_Sum7A+1(0,R2) A(NEXT AVAILABLE BYTE)           10505014
         LR    R15,R2                 Calculate offset                  10506014
         S     R15,@_SYSPRINT_Rec_Ptr   into SYSPRINT record            10507014
         STH   R15,@L_SYSPRINT_Rec_Offset  and save it                  10508014
         LA    R3,JNT_Entry-JobName_Tbl(R3) A(FIRST JOBNAME             10509014
L0210    DS    0H                                                       10510014
         LA    R15,L'JNTE_JobName+2(0,R2) A(Last possible byte)         10511014
         S     R15,@_SYSPRINT_Rec_Ptr Length of data we might write     10512014
         CH    R15,@_SYSPRINT_Rec_Len Too long?                         10513014
         BL    L0220                  No, OK                            10514014
         L     R15,=AL4(G0010)        Yes, A(SYSPRINT routine)          10515014
         BASR  R14,R15                GET A SYSPRINT RECORD             10516014
         L     R2,@_SYSPRINT_Rec_Ptr  Get A(New SYSPRINT Record)        10517014
         AH    R2,@L_SYSPRINT_Rec_Offset and point to appropriate byte  10518014
L0220    DS    0H                                                       10519014
         MVC   0(L'JNTE_JobName,R2),JNTE_JobName-JNT_Entry(R3)          10520014
         CLI   JNTE_JobName_Len-JNT_Entry(R3),L'JNTE_JobName-1 PREFIX?  10521014
         BE    L0230                  NO, SKIP                          10522014
         SLR   R0,R0                  YES, CLEAR WORK REG               10523014
         IC    R0,JNTE_JobName_Len-JNT_Entry(R3) GET PREFIX EXEC LEN    10524014
         AR    R2,R0                  BUMP OUTPUT POINTER               10525014
         MVI   1(R2),C'*'             ADD IN THE ASTERISK               10526014
         LA    R2,1(0,R2)             INCREMENT POINTER                 10527014
         B     L0250                  AND SKIP                          10528014
L0230    DS    0H                                                       10529014
         LA    R2,L'JNTE_JobName-1(0,R2) BUMP POINTER                   10530014
L0240    DS    0H                                                       10531014
         CLI   0(R2),C' '             END OF JOBNAME?                   10532014
         BNE   L0250                  YES, SKIP                         10533014
         BCT   R2,L0240               NO, KEEP CHECKING                 10534014
L0250    DS    0H                                                       10535014
         MVI   1(R2),C','             ADD A COMMA                       10536014
         LA    R2,3(0,R2)             A(NEXT JOBNAME AREA)              10537014
         LA    R3,JNTE_Length(0,R3)   A(NEXT JOBNAME)                   10538014
         BCT   R4,L0210               AND KEEP GOING                    10539014
                                                                        10540014
         SH    R2,=H'2'               BACK UP OVER LAST COMMA           10541014
         LA    R15,L'L_Sum7B(0,R2)    A(Last possible byte)             10542014
         S     R15,@_SYSPRINT_Rec_Ptr Length of data we might write     10543014
         CH    R15,@_SYSPRINT_Rec_Len Too long?                         10544014
         BL    L0260                  No, OK                            10545014
         L     R15,=AL4(G0010)        Yes, A(SYSPRINT routine)          10546014
         BASR  R14,R15                GET A SYSPRINT RECORD             10547014
         L     R2,@_SYSPRINT_Rec_Ptr  Get A(New SYSPRINT Record)        10548014
         AH    R2,@L_SYSPRINT_Rec_Offset and point to appropriate byte  10549014
L0260    DS    0H                                                       10550014
         MVC   0(L'L_Sum7B,R2),L_Sum7B MOVE REST OF MESSAGE             10551014
L0270    DS    0H                                                       10552014
                                                                        10553014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               10554014
         BASR  R14,R15                GET A SYSPRINT RECORD             10555014
                                                                        10556014
* - - - - - - - - - -          D A T A         - - - - - - - - - - -*   10557014
                                                                        10558014
         L     R2,@_SYSPRINT_Rec_Ptr  A(SYSPRINT RECORD)                10559014
         ICM   R15,B'1111',ODTE_DT_Ptr A(DATA TABLE)                    10560014
         BNZ   L0280                  OK, SKIP                          10561014
         MVC   10(L'L_Sum8,R2),L_Sum8 NO, SAY SO                        10562014
         B     L0300                  AND SKIP                          10563014
L0280    DS    0H                                                       10564014
         NI    ODTE_CopyOnly,X'FF'-L'ODTE_CopyOnly Selection criteria   10565014
         MVC   10(L'L_Sum9A,R2),L_Sum9A MOVE FIRST PART OF MESSAGE      10566014
         LA    R2,L'L_Sum9A+10(0,R2) POINT PAST IT                      10567014
         LH    R14,DT_Entry_Count-Data_Tbl(R15) GET NBR ENTRIES         10568014
         LA    R15,DT_Entry-Data_Tbl(R15) A(FIRST ENTRY)                10569014
L0290    DS    0H                                                       10570014
         LR    R0,R14                 SAVE BCT COUNT                    10571014
         SLR   R14,R14                CLEAR REG                         10572014
         IC    R14,DTE_Length-DT_Entry(R15) GET EXEC LEN OF DATA        10573014
         LA    R1,DTE_Data-DT_Entry(0,R15) A(DATA)                      10574014
         EX    R14,L_MVC              MOVE IT                           10575014
         LA    R2,1(R14,R2)           A(NEXT BYTE)                      10576014
         MVC   0(L'L_Sum9B,R2),L_Sum9B MOVE NEXT PART OF MSG            10577014
         LA    R2,L'L_Sum9B(0,R2)     POINT PAST IT                     10578014
         LH    R14,DTE_Offset-DT_Entry(R15) GET OFFSET                  10579014
         CVD   R14,@L_DBLWD           PACK IT                           10580014
         MVC   @L_Offset,=X'4020206B202120' MOVE MASK                   10581014
         LA    R1,@L_Offset+6         A(LAST POSSIBLE POSITION)         10582014
         EDMK  @L_Offset,@L_DBLWD+5   EDIT IT                           10583014
         LA    R14,@L_Offset+6        A(LAST POSSIBLE POSITION)         10584014
         SR    R14,R1                 R14 = EXEC LEN                    10585014
         EX    R14,L_MVC              MOVE IT                           10586014
         SLR   R14,R14                CLEAR REGISTER                    10587014
         IC    R14,DTE_Length-DT_Entry(R15) GET EXEC LEN OF DATA        10588014
         LA    R1,1+DTE_Data-DT_Entry(R14,R15) A(NEXT ENTRY)            10589014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               10590014
         BASR  R14,R15                PRINT IT                          10591014
         L     R2,@_SYSPRINT_Rec_Ptr  A(PRINT RECORD)                   10592014
         MVC   28(2,R2),=C'or'        MOVE LITERAL                      10593014
         LA    R2,31(0,R2)            POINT TO FREE POSITION            10594014
         LR    R14,R0                 RESTORE BCT REGISTER              10595014
         LR    R15,R1                 RESTORE A(TABLE ENTRY)            10596014
         BCT   R14,L0290              KEEP GOING                        10597014
         L     R2,@_SYSPRINT_Rec_Ptr  A(OUTPUT RECORD)                  10598014
         MVC   15(L'L_Sum9C,R2),L_Sum9C MOVE LAST PART                  10599014
L0300    DS    0H                                                       10600014
                                                                        10601014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               10602014
         BASR  R14,R15                YES, GET A NEW SYSPRINT REC       10603014
                                                                        10604014
* - - - - - - - - - -   S T O P A F T          - - - - - - - - - - -*   10605014
                                                                        10606014
         ICM   R15,B'1111',ODTE_StopAft GET STOPAFT COUNT               10607014
         BZ    L0310                  None, SKIP                        10608014
         L     R2,@_SYSPRINT_Rec_Ptr  POINT TO IT                       10609014
         MVC   10(58,R2),=C'A maximum of XX,XXX records will be selecte*10610014
               d to this file.'                                         10611014
         MVC   22(7,R2),=X'4020206B202120' MOVE MASK                    10612014
         L     R15,ODTE_StopAft       GET STOPAFT                       10613014
         CVD   R15,@L_DBLWD           PACK IT                           10614014
         ED    22(7,R2),@L_DBLWD+5    AND EDIT IT                       10615014
                                                                        10616014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               10617014
         BASR  R14,R15                GO PRINT IT                       10618014
L0310    DS    0H                                                       10619014
                                                                        10620014
         DROP  R10                    Free ODT Entry addressability     10621014
                                                                        10622014
         B     L9010                  and exit                          10623014
                                                                        10624014
L9010    DS    0H                                                       10625014
                                                                        10626014
*-------------------------------------------------------------------*   10627014
*                                                                   *   10628014
*    Free up our local storage ...                                  *   10629014
*                                                                   *   10630014
*-------------------------------------------------------------------*   10631014
                                                                        10632014
         LA    R1,@L_Dynam              A(Local storage)                10633014
         L     R13,4(0,R13)             Rescue A(HSA)                   10634014
                                                                        10635014
         STACK POP,                     Free the stack area            +10636014
               ADDR=(R1),                starting here                 +10637014
               STACK=@_31Bit_Stack_Ptr   on this stack                  10638014
                                                                        10639014
*-------------------------------------------------------------------*   10640014
*                                                                   *   10641014
*    ... and return to caller                                       *   10642014
*                                                                   *   10643014
*-------------------------------------------------------------------*   10644014
                                                                        10645014
         L     R14,12(0,R13)            Restore return address          10646014
         SLR   R15,R15                  Clear return code               10647014
         LM    R0,R12,20(R13)           Restore other registers         10648014
         BSM   0,R14                    and return                      10649014
                                                                        10650014
L_MVC MVC      0(0,R2),0(R1)          EXECUTED INSTRUCTION              10651014
                                                                        10652014
                                                                        10653014
L_Sum2A  DC    C'Records between'                                       10654014
L_Sum2B  DC    C' and'                                                  10655014
L_Sum2C  DC    C' will be eligible'                                     10656014
                                                                        10657014
L_Sum3   DC    C'No Date-Time criteria; all records will be eligible'   10658014
                                                                        10659014
L_Sum4A  DC    C'Record Type'                                           10660014
L_Sum4B  DC    C'will be '                                              10661014
L_Sum4C  DC    C'eligible'                                              10662014
                                                                        10663014
L_Sum5   DC    C'No Record Type criteria; all records will be eligible' 10664014
                                                                        10665014
L_Sum6   DC    C'No Jobname criteria; all records will be eligible'     10666014
                                                                        10667014
L_Sum7A  DC    C'Records for Jobname'                                   10668014
L_Sum7B  DC    C' will be eligible'                                     10669014
                                                                        10670014
L_Sum8   DC    C'No Data criteria; all records will be eligible'        10671014
                                                                        10672014
L_Sum9A DC     C'Records must contain '                                 10673014
L_Sum9B DC     C' in position '                                         10674014
L_Sum9C DC     C' to be eligible'                                       10675014
                                                                        10676014
                                                                        10677014
         LTORG                                                          10678014
                                                                        10679014
@L_Dynam        DSECT                 Dynamic area for L                10680014
@L_Save         DS    18F              O/S Style save area              10681014
@L_DBLWD        DS    D                Work area                        10682014
@L_Parms        DS    3AL4             Parm area                        10683014
@L_Return_Ptr   DS    AL4              A(Last byte formatted by F0010)  10684014
@L_SYSPRINT_Rec_Offset DS  HL2         Offset into SYSPRINT record      10685014
@L_DateConv_Area DS   CL(DConv_Area_Length)  Date conversion area       10686014
@L_Offset       DS    CL7              DATA= offset                     10687014
                DS    0D               Alignment                        10688014
@L_DynLen       EQU   *-@L_Dynam      Length of storage required        10689014
                                                                        10690014
SMFSLCT  RSECT                                                          10691014
                                                                        10692014
         DROP  R11,R13                                                  10693014
         POP   USING                                                    10694014
                                                                        10695014
         TITLE 'W0000 - Return JFCB pointer'                            10696014
W0010    DS    0H                                                       10697014
                                                                        10698014
         PUSH  USING                                                    10699014
                                                                        10700014
*---------------------------------------------------------------------* 10701014
*                                                                     * 10702014
*  Routine   : W0010                                                  * 10703014
*                                                                     * 10704014
*  Abstract  : Return A(JFCB) as returned by SWAREQ                   * 10705014
*                                                                     * 10706014
*  Inputs    : R1 : A(DCB)                                            * 10707014
*                                                                     * 10708014
*  Outputs   : R1 : A(JFCB) or 0                                      * 10709014
*              R0 : A(TIOT Entry) or 0                                * 10710014
*              R15: 0:OK, 4: No JFCB                                  * 10711014
*                                                                     * 10712014
*  Notes     :                                                        * 10713014
*                                                                     * 10714014
*  History:                                                           * 10715014
*              2002/02/07 SDDA030 V1.4                                * 10716014
*                                 New                                 * 10717014
*              ____/__/__ _______                                     * 10718014
*                                                                     * 10719014
*---------------------------------------------------------------------* 10720014
                                                                        10721014
         USING *,R15                                                    10722014
         SAVE  (14,12),T                                                10723014
         LR    R11,R15                  Load permanent base reg         10724014
         DROP  R15                      Free up temp base               10725014
         USING W0010,R11                Assign permanent base           10726014
                                                                        10727014
*-------------------------------------------------------------------*   10728014
*                                                                   *   10729014
*    Get an area on the stack for our local storage                 *   10730014
*                                                                   *   10731014
*-------------------------------------------------------------------*   10732014
                                                                        10733014
         STACK PUSH,                    Get Stack area                 +10734014
               LEN=@W_DynLen,            this long                     +10735014
               STACK=@_31Bit_Stack_Ptr   using this stack               10736014
                                                                        10737014
         LR    R2,R1                    Save its address                10738014
         LR    R0,R1                    A(Our storage)                  10739014
         LA    R1,@W_DynLen              Its length                     10740014
         SLR   R14,R14                  Clear source address            10741014
         SLR   R15,R15                   and length                     10742014
         MVCL  R0,R14                   Clear our storage               10743014
         LR    R1,R2                    Restore A(Our storage)          10744014
                                                                        10745014
*-------------------------------------------------------------------*   10746014
*                                                                   *   10747014
*    Chain our save areas, and restore the important registers      *   10748014
*   that we have destroyed                                          *   10749014
*                                                                   *   10750014
*-------------------------------------------------------------------*   10751014
                                                                        10752014
         ST    R13,4(0,R1)              Chain                           10753014
         ST    R1,8(0,R13)               saveareas                      10754014
         LR    R13,R1                   Load dynam base                 10755014
         USING @W_Dynam,R13             Assign a base                   10756014
         L     R15,@W_Dynam+4           Get A(HSA)                      10757014
         LM    R0,R3,20(R15)            Restore callers registers       10758014
                                                                        10759014
*-------------------------------------------------------------------*   10760014
*                                                                   *   10761014
*    Find our TIOT entry ...                                        *   10762014
*                                                                   *   10763014
*-------------------------------------------------------------------*   10764014
                                                                        10765014
         L     R15,CVTPTR(0,0)          A(CVT)                          10766014
         L     R15,CVTTCBP-CVT(R15)     A(Current TCB pointer)          10767014
         L     R15,0(0,R15)             A(Current TCB)                  10768014
         L     R15,TCBTIO-TCB(R15)      A(TIOT)                         10769014
                                                                        10770014
*-------------------------------------------------------------------*   10771014
*                                                                   *   10772014
*    If the DCB is OPEN, we can use DCBTIOT to find our entry ...   *   10773014
*                                                                   *   10774014
*-------------------------------------------------------------------*   10775014
                                                                        10776014
         TM    DCBOFLGS-IHADCB(R1),DCBOFOPN DCB OPen?                   10777014
         BNO   W0020                    No, skip                        10778014
                                                                        10779014
         AH    R15,DCBTIOT-IHADCB(R1)   Offset in TIOT                  10780014
         B     W0040                    and skip                        10781014
                                                                        10782014
W0020    DS    0H                                                       10783014
                                                                        10784014
*-------------------------------------------------------------------*   10785014
*                                                                   *   10786014
*    The DCB is not OPEN, chain through the TIOT looking for our    *   10787014
*   DDName                                                          *   10788014
*                                                                   *   10789014
*-------------------------------------------------------------------*   10790014
                                                                        10791014
         LA    R15,TIOENTRY-TIOT1(R15)  A(1st entry in TIOT)            10792014
W0030    DS    0H                                                       10793014
         CLI   TIOELNGH-TIOENTRY(R15),0 Last Entry?                     10794014
         BE    W8010                    Yes, DDName not found           10795014
         CLC   TIOEDDNM-TIOENTRY(L'TIOEDDNM,R15),DCBDDNAM-IHADCB(R1)    10796014
         BE    W0040                    Found our entry, skip           10797014
                                                                        10798014
         SLR   R0,R0                    Clear work reg                  10799014
         IC    R0,TIOELNGH-TIOENTRY(R15) Get entry length               10800014
         AR    R15,R0                   Point to next entry             10801014
         B     W0030                    and check it out                10802014
W0040    DS    0H                                                       10803014
                                                                        10804014
*-------------------------------------------------------------------*   10805014
*                                                                   *   10806014
*    ... and then get the JFCB address from the SVA                 *   10807014
*                                                                   *   10808014
*-------------------------------------------------------------------*   10809014
                                                                        10810014
         ST    R15,@W_TIOT_Ptr          Save A(TIOT Entry)              10811014
                                                                        10812014
         XC    @W_SWAEPAX,@W_SWAEPAX    Clear SWAEPA                    10813014
         MVC   @W_SWAEPAX+SWVA-ZB505(L'SWVA),TIOEJFCB-TIOENTRY(R15)     10814014
         LA    R15,@W_SWAEPAX           A(EPA)                          10815014
         ST    R15,@W_EPA_Ptr           Save the address                10816014
                                                                        10817014
         MVC   @W_SWAREQ,W_SWAREQ       Move L-Form of macro            10818014
         SWAREQ FCODE=RL,               Read Locate                    +10819014
               EPA=@W_EPA_Ptr,           using the EPA at this addr    +10820014
               UNAUTH=YES,               not being authorized          +10821014
               MF=(E,@W_SWAREQ)          keep ourselves re-entrant      10822014
                                                                        10823014
         L     R1,@W_SWAEPAX+SWBLKPTR-ZB505 Get A(JFCB)                 10824014
         L     R0,@W_TIOT_Ptr           A(TIOT Entry)                   10825014
         SLR   R15,R15                  Clear return code               10826014
         B     W9010                    and exit                        10827014
                                                                        10828014
W8010    DS    0H                                                       10829014
                                                                        10830014
*-------------------------------------------------------------------*   10831014
*                                                                   *   10832014
*    TIOT entry not found                                           *   10833014
*                                                                   *   10834014
*-------------------------------------------------------------------*   10835014
                                                                        10836014
         SLR   R1,R1                    Clear A(JFCB)                   10837014
         SLR   R0,R0                    Clear A(TIOT)                   10838014
         LA    R15,4                    Set return code                 10839014
         B     W9010                    and exit                        10840014
                                                                        10841014
                                                                        10842014
W9010    DS    0H                                                       10843014
                                                                        10844014
*-------------------------------------------------------------------*   10845014
*                                                                   *   10846014
*    Free up our local storage ...                                  *   10847014
*                                                                   *   10848014
*-------------------------------------------------------------------*   10849014
                                                                        10850014
         LR    R2,R15                   Save return code                10851014
         LR    R3,R0                    Save A(TIOT)                    10852014
         LR    R4,R1                    Save A(JFCB)                    10853014
         LA    R1,@W_Dynam              A(Local storage)                10854014
                                                                        10855014
         L     R13,4(0,R13)             Rescue A(HSA)                   10856014
                                                                        10857014
         STACK POP,                     Free the stack area            +10858014
               ADDR=(R1),                starting here                 +10859014
               STACK=@_31Bit_Stack_Ptr   on this stack                  10860014
                                                                        10861014
         LR    R15,R2                   Restore return code             10862014
         LR    R0,R3                    Restore A(TIOT)                 10863014
         LR    R1,R4                    Restore A(JFCB)                 10864014
                                                                        10865014
*-------------------------------------------------------------------*   10866014
*                                                                   *   10867014
*    ... and return to caller                                       *   10868014
*                                                                   *   10869014
*-------------------------------------------------------------------*   10870014
                                                                        10871014
         L     R14,12(0,R13)            Restore return address          10872014
         LM    R2,R12,28(R13)           Restore other registers         10873014
         BSM   0,R14                    and return                      10874014
                                                                        10875014
                                                                        10876014
                                                                        10877014
W_SWAREQ SWAREQ MF=L                    Read Locate                     10878014
W_SWAREQ_Length EQU *-W_SWAREQ          Length of SWAREQ Macro          10879014
                                                                        10880014
         LTORG                                                          10881014
                                                                        10882014
@W_Dynam        DSECT                 Dynamic area for W                10883014
@W_Save         DS    18F              O/S Style save area              10884014
@W_EPA_Ptr      DS    AL4              A(EPA for SWAREQ)                10885014
@W_TIOT_Ptr     DS    AL4              A(TIOT Entry for DDName)         10886014
@W_SWAREQ       DS    CL(W_SWAREQ_Length)                               10887014
                DS    0F                                                10888014
@W_SWAEPAX      DS    CL(L'SWAEPAX)                                     10889014
                                                                        10890014
                DS    0D               Alignment                        10891014
@W_DynLen       EQU   *-@W_Dynam      Length of storage required        10892014
                                                                        10893014
SMFSLCT  RSECT                                                          10894014
                                                                        10895014
         DROP  R11,R13                                                  10896014
         POP   USING                                                    10897014
                                                                        10898014
         TITLE 'X0000 - Exit Invocation Routine'                        10899014
X0010    DS    0H                                                       10900014
                                                                        10901014
         PUSH  USING                                                    10902014
                                                                        10903014
*---------------------------------------------------------------------* 10904014
*                                                                     * 10905014
*  Routine   : X0010                                                  * 10906014
*                                                                     * 10907014
*  Abstract  : Invoke user exits                                      * 10908014
*                                                                     * 10909014
*  Inputs    : R0 : 0:Inexit, 1:OutExit                               * 10910014
*              R1 : A(SMF Record)                                     * 10911014
*              R10: A(ODT Entry)                                      * 10912014
*                                                                     * 10913014
*  Outputs   : R1 : A(SMF Record as returned by exit)                 * 10914014
*              R15: As received from the user exit                    * 10915014
*                                                                     * 10916014
*                                                                     * 10917014
*  Notes     :                                                        * 10918014
*                                                                     * 10919014
*  History:                                                           * 10920014
*              2002/02/07 SDDA030 V1.4                                * 10921014
*                                 New                                 * 10922014
*              ____/__/__ _______                                     * 10923014
*                                                                     * 10924014
*---------------------------------------------------------------------* 10925014
                                                                        10926014
         USING *,R15                                                    10927014
         SAVE  (14,12),T                                                10928014
         LR    R11,R15                  Load permanent base reg         10929014
         DROP  R15                      Free up temp base               10930014
         USING X0010,R11                Assign permanent base           10931014
                                                                        10932014
*-------------------------------------------------------------------*   10933014
*                                                                   *   10934014
*    Get an area on the stack for our local storage                 *   10935014
*                                                                   *   10936014
*-------------------------------------------------------------------*   10937014
                                                                        10938014
         STACK PUSH,                    Get Stack area                 +10939014
               LEN=@X_DynLen,            this long                     +10940014
               STACK=@_31Bit_Stack_Ptr   using this stack               10941014
                                                                        10942014
         LR    R2,R1                    Save its address                10943014
         LR    R0,R1                    A(Our storage)                  10944014
         LA    R1,@X_DynLen              Its length                     10945014
         SLR   R14,R14                  Clear source address            10946014
         SLR   R15,R15                   and length                     10947014
         MVCL  R0,R14                   Clear our storage               10948014
         LR    R1,R2                    Restore A(Our storage)          10949014
                                                                        10950014
*-------------------------------------------------------------------*   10951014
*                                                                   *   10952014
*    Chain our save areas, and restore the important registers      *   10953014
*   that we have destroyed                                          *   10954014
*                                                                   *   10955014
*-------------------------------------------------------------------*   10956014
                                                                        10957014
         ST    R13,4(0,R1)              Chain                           10958014
         ST    R1,8(0,R13)               saveareas                      10959014
         LR    R13,R1                   Load dynam base                 10960014
         USING @X_Dynam,R13             Assign a base                   10961014
         L     R15,@X_Dynam+4           Get A(HSA)                      10962014
         LM    R0,R3,20(R15)            Restore callers registers       10963014
                                                                        10964014
         USING ODT_Entry,R10            Assign ODTE base                10965014
                                                                        10966014
*-------------------------------------------------------------------*   10967014
*                                                                   *   10968014
*    If the SYSPRINT User Exit Interface (Y0010) has not been       *   10969014
*   copied to 24-bit storage, do it now ...                         *   10970014
*                                                                   *   10971014
*-------------------------------------------------------------------*   10972014
                                                                        10973014
         ICM   R15,B'1111',@_SYSPRINT_Y0010_Ptr A(Interface routine)    10974014
         BNZ   X0020                    Already moved, OK               10975014
                                                                        10976014
         STORAGE OBTAIN,                Go get our storage             +10977014
               LENGTH=Y_Code_End-Y0010,  this long                     +10978014
               LOC=BELOW                 below-the-line                 10979014
                                                                        10980014
         ST    R1,@_SYSPRINT_Y0010_Ptr  Save the address                10981014
         LR    R0,R1                    Point to it                     10982014
         LA    R1,Y_Code_End-Y0010      Length of code                  10983014
         L     R14,=AL4(Y0010)          Point to start of code          10984014
         LR    R15,R1                   Same length                     10985014
         MVCL  R0,R14                   Move code to 24-bit storage     10986014
                                                                        10987014
         L     R15,@_SYSPRINT_Y0010_Ptr A(Moved code)                   10988014
         ST    R12,Y_@_Dynam_Ptr-Y0010(R15) Update A(@_Dynam)           10989014
                                                                        10990014
*-------------------------------------------------------------------*   10991014
*                                                                   *   10992014
*    ... and IDENTIFY it as SMFSLCTP, in case the exit is written   *   10993014
*   in a HLL.                                                       *   10994014
*                                                                   *   10995014
*-------------------------------------------------------------------*   10996014
                                                                        10997014
         L     R1,@_SYSPRINT_Y0010_Ptr Point to our routine             10998014
         IDENTIFY EP=SMFSLCTP,         and let MVS know                +10999014
               ENTRY=(1)                about it                        11000014
                                                                        11001014
X0020    DS    0H                                                       11002014
                                                                        11003014
*-------------------------------------------------------------------*   11004014
*                                                                   *   11005014
*    Update the pointer to the ODT in the Y0010 copy                *   11006014
*                                                                   *   11007014
*-------------------------------------------------------------------*   11008014
                                                                        11009014
         L     R15,@_SYSPRINT_Y0010_Ptr A(Moved code)                   11010014
         ST    R10,Y_@_ODT_Entry_Ptr-Y0010(R15) Update A(ODT Entry)     11011014
                                                                        11012014
*-------------------------------------------------------------------*   11013014
*                                                                   *   11014014
*    Get some 24-bit storage, and build our parm list               *   11015014
*                                                                   *   11016014
*-------------------------------------------------------------------*   11017014
                                                                        11018014
         STACK PUSH,                    Get Stack area                 +11019014
               LEN=@X_24_DynLen,         this long                     +11020014
               STACK=@_24Bit_Stack_Ptr   using this stack               11021014
                                                                        11022014
         ST    R1,@X_ParmList_Ptr       Save it                         11023014
         USING @X_24_Dynam,R1           Assign a base                   11024014
                                                                        11025014
         L     R15,@X_Dynam+4           Get A(HSA)                      11026014
         L     R0,24(0,R15)             A(SMF record) Original R1       11027014
         ST    R0,@X_24_Exit_Parmlist   Save A(SMF Record)              11028014
                                                                        11029014
         ICM   R0,B'1111',20(R15)       Get Original R0                 11030014
         BNZ   X0030                    OutExit, skip                   11031014
                                                                        11032014
         L     R0,ODTE_InExit_Data      Yes, A(Word for the user)       11033014
         ST    R0,@X_24_Exit_Word       Save it                         11034014
         LA    R0,@X_24_Exit_Word       Point to it                     11035014
         ST    R0,@X_24_Exit_Parmlist+4 Save the pointer                11036014
         L     R0,ODTE_InExit_Parm_Ptr  A(Parm for InExit)              11037014
         ST    R0,@X_24_Exit_Parmlist+8 Save it                         11038014
         L     R15,ODTE_InExit_Ptr      A(InExit EP)                    11039014
         B     X0040                    Skip                            11040014
                                                                        11041014
X0030    DS    0H                                                       11042014
                                                                        11043014
         L     R0,ODTE_OutExit_Data     Yes, A(Word for the user)       11044014
         ST    R0,@X_24_Exit_Word       Save it                         11045014
         LA    R0,@X_24_Exit_Word       Point to it                     11046014
         ST    R0,@X_24_Exit_Parmlist+4 Save it                         11047014
         L     R0,ODTE_OutExit_Parm_Ptr A(Parm for OutExit)             11048014
         ST    R0,@X_24_Exit_Parmlist+8 Save it                         11049014
         L     R15,ODTE_OutExit_Ptr     A(OutExit EP)                   11050014
         B     X0040                    Skip                            11051014
                                                                        11052014
X0040    DS    0H                                                       11053014
                                                                        11054014
         L     R0,@_Input_Count         Input count                     11055014
         ST    R0,@X_24_Input_Count     Save it                         11056014
         LA    R0,@X_24_Input_Count     Point to it                     11057014
         ST    R0,@X_24_Exit_Parmlist+12 Save it                        11058014
                                                                        11059014
         L     R0,ODTE_Selected         A(Selected count)               11060014
         ST    R0,@X_24_Selected        Save it                         11061014
         LA    R0,@X_24_Selected        Point to it                     11062014
         ST    R0,@X_24_Exit_Parmlist+16 Save it                        11063014
         MVC   @X_24_DDName,ODTE_DDName Move DDName                     11064014
         LA    R0,@X_24_DDName          A(DDName)                       11065014
         ST    R0,@X_24_Exit_Parmlist+20 Save it                        11066014
         L     R0,@_SYSPRINT_Y0010_Ptr  A(SYSPRINT routine)             11067014
         ST    R0,@X_24_Exit_Parmlist+24 Save it                        11068014
                                                                        11069014
         OI    @X_24_Exit_Parmlist+24,X'80' Flag as last parm           11070014
                                                                        11071014
         LA    R14,@X_24_Save           Point to 24-bit savearea        11072014
         ST    R14,8(0,R13)             Chain                           11073014
         ST    R13,4(0,R14)              saveareas                      11074014
         LR    R13,R14                  Point to 24-bit savearea        11075014
                                                                        11076014
         LA    R1,@X_24_Exit_Parmlist   Point to the parmlist           11077014
         BASR  R14,R15                  Go do the exit                  11078014
                                                                        11079014
         LR    R2,R15                   Save return code                11080014
                                                                        11081014
         L     R13,4(0,R13)             Restore A(Our 31-bit savearea)  11082014
                                                                        11083014
         L     R1,@X_ParmList_Ptr       Restore A(24-bit storage)       11084014
         L     R15,@X_Dynam+4           Get A(HSA)                      11085014
         ICM   R0,B'1111',20(R15)       InExit?                         11086014
         BNZ   X0050                    No, skip                        11087014
         MVC   ODTE_InExit_Data,@X_24_Exit_Word Yes, save user stuff    11088014
         B     X0060                    and skip                        11089014
X0050    DS    0H                                                       11090014
         MVC   ODTE_OutExit_Data,@X_24_Exit_Word Yes, save user stuff   11091014
X0060    DS    0H                                                       11092014
                                                                        11093014
         L     R1,@X_ParmList_Ptr       A(Parmlist)                     11094014
         L     R3,@X_24_Exit_Parmlist   Get A(SMF record)               11095014
         STACK POP,                     Free the stack area            +11096014
               ADDR=(R1),                starting here                 +11097014
               STACK=@_24Bit_Stack_Ptr   on this stack                  11098014
                                                                        11099014
         DROP  R1                       Free 24-bit storage base        11100014
                                                                        11101014
         LR    R15,R2                   Restore return code             11102014
         B     X9010                    and exit                        11103014
                                                                        11104014
X9010    DS    0H                                                       11105014
                                                                        11106014
*-------------------------------------------------------------------*   11107014
*                                                                   *   11108014
*    Free up our local storage ...                                  *   11109014
*                                                                   *   11110014
*-------------------------------------------------------------------*   11111014
                                                                        11112014
         LR    R2,R15                   Save return code                11113014
         LA    R1,@X_Dynam              A(Local storage)                11114014
         L     R13,4(0,R13)             Rescue A(HSA)                   11115014
                                                                        11116014
         STACK POP,                     Free the stack area            +11117014
               ADDR=(R1),                starting here                 +11118014
               STACK=@_31Bit_Stack_Ptr   on this stack                  11119014
                                                                        11120014
         LR    R15,R2                   Restore return code             11121014
                                                                        11122014
*-------------------------------------------------------------------*   11123014
*                                                                   *   11124014
*    ... and return to caller                                       *   11125014
*                                                                   *   11126014
*-------------------------------------------------------------------*   11127014
                                                                        11128014
         L     R14,12(0,R13)            Restore return address          11129014
         LR    R15,R2                   Set return code                 11130014
         L     R0,20(0,R13)             Restore R0                      11131014
         LR    R1,R3                    A(SMF record returned by exit)  11132014
         LM    R2,R12,28(R13)           Restore other registers         11133014
         BSM   0,R14                    and return                      11134014
                                                                        11135014
                                                                        11136014
                                                                        11137014
         LTORG                                                          11138014
                                                                        11139014
@X_Dynam        DSECT                 Dynamic area for X                11140014
@X_Save         DS    18F              O/S Style save area              11141014
@X_ParmList_Ptr DS    AL4              A(Parmlist)                      11142014
                DS    0D               Alignment                        11143014
@X_DynLen       EQU   *-@X_Dynam      Length of storage required        11144014
                                                                        11145014
@X_24_Dynam     DSECT                 24-bit Dynamic area for X         11146014
@X_24_Save      DS    18F              O/S Style save area              11147014
@X_24_Exit_Parmlist DS 7FL4            Parmlist                         11148014
@X_24_Exit_Word DS    FL4              A Word for the user              11149014
@X_24_Input_Count DS  FL4              Input count                      11150014
@X_24_Selected  DS    FL4              Selected count                   11151014
@X_24_DDName    DS    CL8              DDName                           11152014
                DS    0D               Alignment                        11153014
@X_24_DynLen    EQU   *-@X_24_Dynam   Length of 24-bit storage required 11154014
                                                                        11155014
                                                                        11156014
                                                                        11157014
SMFSLCT  RSECT                                                          11158014
                                                                        11159014
         DROP  R10                                                      11160014
         DROP  R11,R13                                                  11161014
         POP   USING                                                    11162014
         TITLE 'Y0000: SYSPRINT routine for User Exits'                 11163014
                                                                        11164014
         PUSH  USING                                                    11165014
                                                                        11166014
Y0010    DS    0H                                                       11167014
                                                                        11168014
*---------------------------------------------------------------------* 11169014
*                                                                     * 11170014
*  Routine   : Y0010                                                  * 11171014
*                                                                     * 11172014
*  Abstract  : SYSPRINT routine for User Exits:                       * 11173014
*               Load @_Dynam base                                     * 11174014
*               Move user data to SYSPRINT record                     * 11175014
*               Invoke G0010                                          * 11176014
*                                                                     * 11177014
*                                                                     * 11178014
*  Inputs    : R1 --> A(Data to be printed)                           * 11179014
*                     Fullword containing length of data              * 11180014
*  Outputs   : R15 = 0                                                * 11181014
*                                                                     * 11182014
*  Notes     : This routine is moved to 24-bit storage, and the       * 11183014
*              address of @_Dynam inserted into the copy.             * 11184014
*              Before each invocation of the exit, the address of the * 11185014
*              active ODT is also stored in Y_ODT_Entry_Ptr.          * 11186014
*                                                                     * 11187014
*                                                                     * 11188014
*  History:                                                           * 11189014
*              2002/02/07 SDDA030 V1.4                                * 11190014
*                                 New                                 * 11191014
*              ____/__/__ _______                                     * 11192014
*                                                                     * 11193014
*---------------------------------------------------------------------* 11194014
                                                                        11195014
         USING *,R15                                                    11196014
         SAVE  (14,12),T                                                11197014
         LR    R11,R15                  Load permanent base reg         11198014
         DROP  R15                      Free up temp base               11199014
         USING Y0010,R11                Assign permanent base           11200014
                                                                        11201014
*-------------------------------------------------------------------*   11202014
*                                                                   *   11203014
*    Make sure we are in AMODE(31)                                  *   11204014
*                                                                   *   11205014
*-------------------------------------------------------------------*   11206014
                                                                        11207014
         LA    R14,Y0020                A(Dummy label)                  11208014
         ICM   R14,B'1000',=XL1'80'     in 31-bit mode                  11209014
         BSM   0,R14                    Gen into 31-bit mode            11210014
Y0020    DS    0H                                                       11211014
                                                                        11212014
*-------------------------------------------------------------------*   11213014
*                                                                   *   11214014
*    Get an area on the stack for our local storage                 *   11215014
*                                                                   *   11216014
*-------------------------------------------------------------------*   11217014
                                                                        11218014
         L     R12,Y_@_Dynam_Ptr        A(@_Dynam)                      11219014
                                                                        11220014
         STACK PUSH,                    Get Stack area                 +11221014
               LEN=@Y_DynLen,            this long                     +11222014
               STACK=@_31Bit_Stack_Ptr   using this stack               11223014
                                                                        11224014
         LR    R2,R1                    Save its address                11225014
         LR    R0,R1                    A(Our storage)                  11226014
         LA    R1,@Y_DynLen              Its length                     11227014
         SLR   R14,R14                  Clear source address            11228014
         SLR   R15,R15                   and length                     11229014
         MVCL  R0,R14                   Clear our storage               11230014
         LR    R1,R2                    Restore A(Our storage)          11231014
                                                                        11232014
*-------------------------------------------------------------------*   11233014
*                                                                   *   11234014
*    Chain our save areas, and restore the important registers      *   11235014
*   that we have destroyed                                          *   11236014
*                                                                   *   11237014
*-------------------------------------------------------------------*   11238014
                                                                        11239014
         ST    R13,4(0,R1)              Chain                           11240014
         ST    R1,8(0,R13)               saveareas                      11241014
         LR    R13,R1                   Load dynam base                 11242014
         USING @Y_Dynam,R13             Assign a base                   11243014
         L     R15,@Y_Dynam+4           Get A(HSA)                      11244014
         LM    R0,R3,20(R15)            Restore callers registers       11245014
                                                                        11246014
*-------------------------------------------------------------------*   11247014
*                                                                   *   11248014
*    If SYSPRINT is OPEN ...                                        *   11249014
*                                                                   *   11250014
*-------------------------------------------------------------------*   11251014
                                                                        11252014
         L     R15,@_SYSPRINT_DCB_Ptr A(SYSPRINT DCB)                   11253014
         TM    DCBOFLGS-IHADCB(R15),DCBOFOPN Is it open?                11254014
         BZ    Y9010                  No, skip printing                 11255014
                                                                        11256014
*-------------------------------------------------------------------*   11257014
*                                                                   *   11258014
*    ... load the address of the ODT we are using ...               *   11259014
*                                                                   *   11260014
*-------------------------------------------------------------------*   11261014
                                                                        11262014
         L     R10,Y_@_ODT_Entry_Ptr  A(ODT Entry)                      11263014
                                                                        11264014
*-------------------------------------------------------------------*   11265014
*                                                                   *   11266014
*    ... move the data to the SYSPRINT record ...                   *   11267014
*                                                                   *   11268014
*-------------------------------------------------------------------*   11269014
                                                                        11270014
         L     R14,@_SYSPRINT_Rec_Ptr Get A(SYSPRINT record)            11271014
         LH    R15,@_SYSPRINT_Rec_Len Get its length                    11272014
         LM    R0,R1,0(R1)            Get ptr, length of user data      11273014
         ICM   R1,B'1000',=CL1' '     Pad character of blank            11274014
         MVCL  R14,R0                 Move user data                    11275014
                                                                        11276014
*-------------------------------------------------------------------*   11277014
*                                                                   *   11278014
*    ... and print it                                               *   11279014
*                                                                   *   11280014
*-------------------------------------------------------------------*   11281014
                                                                        11282014
         L     R15,=AL4(G0010)        A(Print routine)                  11283014
         BASR  R14,R15                Go do it                          11284014
                                                                        11285014
Y9010    DS    0H                                                       11286014
                                                                        11287014
*-------------------------------------------------------------------*   11288014
*                                                                   *   11289014
*    Free up our local storage ...                                  *   11290014
*                                                                   *   11291014
*-------------------------------------------------------------------*   11292014
                                                                        11293014
         LA    R1,@Y_Dynam              A(Local storage)                11294014
         L     R13,4(0,R13)             Rescue A(HSA)                   11295014
                                                                        11296014
         STACK POP,                     Free the stack area            +11297014
               ADDR=(R1),                starting here                 +11298014
               STACK=@_31Bit_Stack_Ptr   on this stack                  11299014
                                                                        11300014
*-------------------------------------------------------------------*   11301014
*                                                                   *   11302014
*    ... and return to caller                                       *   11303014
*                                                                   *   11304014
*-------------------------------------------------------------------*   11305014
                                                                        11306014
         L     R14,12(0,R13)            Restore return address          11307014
         SLR   R15,R15                  Clear return code               11308014
         LM    R0,R12,20(R13)           Restore other registers         11309014
         BSM   0,R14                    and return                      11310014
                                                                        11311014
Y_@_Dynam_Ptr     DC AL4(0)           A(@_Dynam)                        11312014
Y_@_ODT_Entry_Ptr DC AL4(0)           A(ODT Entry for User exit)        11313014
                                                                        11314014
         LTORG                                                          11315014
                                                                        11316014
Y_Code_End  EQU   *                   End of code to be moved           11317014
                                                                        11318014
@Y_Dynam        DSECT                 Dynamic area for Y                11319014
@Y_Save         DS    18F              O/S Style save area              11320014
                DS    0D               Alignment                        11321014
@Y_DynLen       EQU   *-@Y_Dynam      Length of storage required        11322014
                                                                        11323014
SMFSLCT  RSECT                                                          11324014
                                                                        11325014
         DROP  R11,R13                                                  11326014
         POP   USING                                                    11327014
SMFSLCT  RSECT                                                          11328014
                                                                        11329014
         TITLE 'Z0000: Termination'                                     11330014
                                                                        11331014
         PUSH  USING                                                    11332014
                                                                        11333014
Z0010    DS    0H                                                       11334014
                                                                        11335014
*---------------------------------------------------------------------* 11336014
*                                                                     * 11337014
*  Routine   : Z0010                                                  * 11338014
*                                                                     * 11339014
*  Abstract  : Termination routine:                                   * 11340014
*               Print counters                                        * 11341014
*               Close DCBs                                            * 11342014
*               Free Storage areas                                    * 11343014
*                                                                     * 11344014
*                                                                     * 11345014
*  Inputs    : N/A                                                    * 11346014
*  Outputs   : R15 = 0, at least 1 record selected to an output file  * 11347014
*                    4, no records selected to any output file.       * 11348014
*                                                                     * 11349014
*  Notes     :                                                        * 11350014
*                                                                     * 11351014
*  History:                                                           * 11352014
*              1999/04/24 SDDA030 V1.1                                * 11353014
*                                 Used LIFO Stack for our dynamic     * 11354014
*                                 areas.                              * 11355014
*              ____/__/__ _______                                     * 11356014
*                                                                     * 11357014
*---------------------------------------------------------------------* 11358014
                                                                        11359014
                                                                        11360014
                                                                        11361014
         USING *,R15                                                    11362014
         SAVE  (14,12),T                                                11363014
         LR    R11,R15                  Load permanent base reg         11364014
         DROP  R15                      Free up temp base               11365014
         USING Z0010,R11                Assign permanent base           11366014
                                                                        11367014
*-------------------------------------------------------------------*   11368014
*                                                                   *   11369014
*    Get an area on the stack for our local storage                 *   11370014
*                                                                   *   11371014
*-------------------------------------------------------------------*   11372014
                                                                        11373014
         STACK PUSH,                    Get Stack area                 +11374014
               LEN=@Z_DynLen,            this long                     +11375014
               STACK=@_31Bit_Stack_Ptr   using this stack               11376014
                                                                        11377014
         LR    R2,R1                    Save its address                11378014
         LR    R0,R1                    A(Our storage)                  11379014
         LA    R1,@Z_DynLen              Its length                     11380014
         SLR   R14,R14                  Clear source address            11381014
         SLR   R15,R15                   and length                     11382014
         MVCL  R0,R14                   Clear our storage               11383014
         LR    R1,R2                    Restore A(Our storage)          11384014
                                                                        11385014
*-------------------------------------------------------------------*   11386014
*                                                                   *   11387014
*    Chain our save areas, and restore the important registers      *   11388014
*   that we have destroyed                                          *   11389014
*                                                                   *   11390014
*-------------------------------------------------------------------*   11391014
                                                                        11392014
         ST    R13,4(0,R1)              Chain                           11393014
         ST    R1,8(0,R13)               saveareas                      11394014
         LR    R13,R1                   Load dynam base                 11395014
         USING @Z_Dynam,R13             Assign a base                   11396014
         L     R15,@Z_Dynam+4           Get A(HSA)                      11397014
         LM    R0,R3,20(R15)            Restore callers registers       11398014
                                                                        11399014
         NI    @Z_Select_OK,X'FF'-L'@Z_Select_OK Nothing selected       11400014
                                                                        11401014
*-------------------------------------------------------------------*   11402014
*                                                                   *   11403014
*    PRINT THE RECORD COUNTERS, IF POSSIBLE.                        *   11404014
*                                                                   *   11405014
*-------------------------------------------------------------------*   11406014
                                                                        11407014
         L     R15,@_SYSPRINT_DCB_Ptr A(Sysprint DCB)                   11408014
         TM    DCBOFLGS-IHADCB(R15),DCBOFOPN IS SYPRINT OPEN?           11409014
         BZ    Z0060                  NO, SKIP PRINTING                 11410014
         LA    R15,99                 GET HIGH LINE COUNT               11411014
         STH   R15,@_Line_Count       SAVE IT                           11412014
         L     R10,@_ODT_Ptr          POINT TO OUTPUT ENV TABLE         11413014
         LA    R10,ODT_Entry-OutDesc_Tbl(R10) AND POINT TO FIRST        11414014
                                                                        11415014
         USING ODT_Entry,R10          Tell the assembler                11416014
                                                                        11417014
         OI    ODTE_Print_NO,L'ODTE_Print_NO SUPPRESS HEADINGS          11418014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               11419014
         BASR  R14,R15                GO GET A NEW PAGE                 11420014
         L     R2,@_SYSPRINT_Rec_Ptr  GET A(SYSPRINT RECORD)            11421014
         ICM   R15,B'1111',@_Input_Count Nbr input records read         11422014
         BZ    Z0050                  None, skip                        11423014
         CP    @_Dump_Start_Date,=P'999999' DID WE GET A TYPE 2?        11424014
         BE    Z0040                  NO, SKIP                          11425014
         MVC   20(27,R2),=C'First Dump Header (Type 2):'                11426014
                                                                        11427014
         XC    @Z_DateConv_Area,@Z_DateConv_Area Clear Date area        11428014
         ZAP   @Z_DateConv_Area+DConv_Date_SMFDate-DateConv_Area(L'DCon+11429014
               v_Date_SMFDate),@_Dump_Start_Date    Date                11430014
         MVC   @Z_DateConv_Area+DConv_Time-DateConv_Area(L'DConv_Time),+11431014
               @_Dump_Start_Time              Time too                  11432014
         OI    @Z_DateConv_Area+DConv_Input_SMFDate-DateConv_Area,L'DCo+11433014
               nv_Input_SMFDate     Indicate SMF Date                   11434014
         OI    @Z_DateConv_Area+DConv_Input_Time-DateConv_Area,L'DConv_+11435014
               Input_Time           Indicate SMF Time                   11436014
         LA    R15,@Z_DateConv_Area     Point to Date Conversion area   11437014
         ST    R15,@Z_Parms          Save as 1st parm                   11438014
         LA    R15,51(0,R2)          A(Output area)                     11439014
         ST    R15,@Z_Parms+4        Save as 2nd parm                   11440014
         LA    R15,@Z_Return_Ptr     A(Return area)                     11441014
         ST    R15,@Z_Parms+8        Save as 2nd parm                   11442014
         LA    R1,@Z_Parms           A(Parm pointers)                   11443014
         L     R15,=AL4(F0010)       A(Format routine)                  11444014
         BASR  R14,R15               Go convert date and time           11445014
                                                                        11446014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               11447014
         BASR  R14,R15                PRINT THE RECORD                  11448014
         L     R2,@_SYSPRINT_Rec_Ptr  GET A(SYSPRINT RECORD)            11449014
         MVC   20(27,R2),=C'Last Dump Trailer (Type 3):'                11450014
         CP    @_Dump_End_Date,=P'0'  DID WE GET A TYPE 3?              11451014
         BNE   Z0020                  YES, SKIP                         11452014
         MVI   51(R2),C'?'            NO, SAY SO                        11453014
         B     Z0030                  AND SKIP                          11454014
Z0020    DS    0H                                                       11455014
                                                                        11456014
         XC    @Z_DateConv_Area,@Z_DateConv_Area Clear Date area        11457014
         ZAP   @Z_DateConv_Area+DConv_Date_SMFDate-DateConv_Area(L'DCon+11458014
               v_Date_SMFDate),@_Dump_End_Date    Date                  11459014
         MVC   @Z_DateConv_Area+DConv_Time-DateConv_Area(L'DConv_Time),+11460014
               @_Dump_End_Time              Time too                    11461014
         OI    @Z_DateConv_Area+DConv_Input_SMFDate-DateConv_Area,L'DCo+11462014
               nv_Input_SMFDate     Indicate SMF Date                   11463014
         OI    @Z_DateConv_Area+DConv_Input_Time-DateConv_Area,L'DConv_+11464014
               Input_Time           Indicate SMF Time                   11465014
         LA    R15,@Z_DateConv_Area     Point to Date Conversion area   11466014
         ST    R15,@Z_Parms          Save as 1st parm                   11467014
         LA    R15,51(0,R2)          A(Output area)                     11468014
         ST    R15,@Z_Parms+4        Save as 2nd parm                   11469014
         LA    R15,@Z_Return_Ptr     A(Return area)                     11470014
         ST    R15,@Z_Parms+8        Save as 2nd parm                   11471014
         LA    R1,@Z_Parms           A(Parm pointers)                   11472014
         L     R15,=AL4(F0010)       A(Format routine)                  11473014
         BASR  R14,R15               Go convert date and time           11474014
                                                                        11475014
Z0030    DS    0H                                                       11476014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               11477014
         BASR  R14,R15                PRINT THE RECORD                  11478014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               11479014
         BASR  R14,R15                PRINT THE RECORD                  11480014
         L     R2,@_SYSPRINT_Rec_Ptr  GET A(SYSPRINT RECORD)            11481014
Z0040    DS    0H                                                       11482014
         MVC   20(26,R2),=C'Date/Time of first record:'                 11483014
                                                                        11484014
         XC    @Z_DateConv_Area,@Z_DateConv_Area Clear Date area        11485014
         ZAP   @Z_DateConv_Area+DConv_Date_SMFDate-DateConv_Area(L'DCon+11486014
               v_Date_SMFDate),@_First_Rec_Date   Date                  11487014
         MVC   @Z_DateConv_Area+DConv_Time-DateConv_Area(L'DConv_Time),+11488014
               @_First_Rec_Time             Time too                    11489014
         OI    @Z_DateConv_Area+DConv_Input_SMFDate-DateConv_Area,L'DCo+11490014
               nv_Input_SMFDate     Indicate SMF Date                   11491014
         OI    @Z_DateConv_Area+DConv_Input_Time-DateConv_Area,L'DConv_+11492014
               Input_Time           Indicate SMF Time                   11493014
         LA    R15,@Z_DateConv_Area     Point to Date Conversion area   11494014
         ST    R15,@Z_Parms          Save as 1st parm                   11495014
         LA    R15,51(0,R2)          A(Output area)                     11496014
         ST    R15,@Z_Parms+4        Save as 2nd parm                   11497014
         LA    R15,@Z_Return_Ptr     A(Return area)                     11498014
         ST    R15,@Z_Parms+8        Save as 2nd parm                   11499014
         LA    R1,@Z_Parms           A(Parm pointers)                   11500014
         L     R15,=AL4(F0010)       A(Format routine)                  11501014
         BASR  R14,R15               Go convert date and time           11502014
                                                                        11503014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               11504014
         BASR  R14,R15                PRINT IT                          11505014
         L     R2,@_SYSPRINT_Rec_Ptr  GET A(SYSPRINT RECORD)            11506014
         MVC   20(25,R2),=C'Date/Time of last record:'                  11507014
                                                                        11508014
         XC    @Z_DateConv_Area,@Z_DateConv_Area Clear Date area        11509014
         ZAP   @Z_DateConv_Area+DConv_Date_SMFDate-DateConv_Area(L'DCon+11510014
               v_Date_SMFDate),@_Input_Rec_Date   Date                  11511014
         MVC   @Z_DateConv_Area+DConv_Time-DateConv_Area(L'DConv_Time),+11512014
               @_Input_Rec_Time             Time too                    11513014
         OI    @Z_DateConv_Area+DConv_Input_SMFDate-DateConv_Area,L'DCo+11514014
               nv_Input_SMFDate     Indicate SMF Date                   11515014
         OI    @Z_DateConv_Area+DConv_Input_Time-DateConv_Area,L'DConv_+11516014
               Input_Time           Indicate SMF Time                   11517014
         LA    R15,@Z_DateConv_Area     Point to Date Conversion area   11518014
         ST    R15,@Z_Parms          Save as 1st parm                   11519014
         LA    R15,51(0,R2)          A(Output area)                     11520014
         ST    R15,@Z_Parms+4        Save as 2nd parm                   11521014
         LA    R15,@Z_Return_Ptr     A(Output area)                     11522014
         ST    R15,@Z_Parms+8        Save as 3nd parm                   11523014
         LA    R1,@Z_Parms           A(Parm pointers)                   11524014
         L     R15,=AL4(F0010)       A(Format routine)                  11525014
         BASR  R14,R15               Go convert date and time           11526014
                                                                        11527014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               11528014
         BASR  R14,R15                PRINT IT                          11529014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               11530014
         BASR  R14,R15                A COUPLE OF                       11531014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               11532014
         BASR  R14,R15                  BLANK LINES                     11533014
Z0050    DS    0H                                                       11534014
         L     R1,@_SYSPRINT_Rec_Ptr  GET A(SYSPRINT RECORD)            11535014
         MVC   20(23,R1),=C'Number of records read:'                    11536014
         MVC   50(12,R1),=X'402020206B2020206B202120'                   11537014
         L     R15,@_Input_Count      GET INPUT COUNTER                 11538014
         CVD   R15,@Z_DBLWD           PACK IT                           11539014
         ED    50(12,R1),@Z_DBLWD+3   AND EDIT IT                       11540014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               11541014
         BASR  R14,R15                PRINT THE INPUT COUNT             11542014
         TM    @_SYSUT1_Seg_Error,L'@_SYSUT1_Seg_Error Segment errors?  11543014
         BZ    Z0060                  No, skip                          11544014
         L     R1,@_SYSPRINT_Rec_Ptr  Yes, A(SYSPRINT record)           11545014
         MVC   17(39,R1),=C'** Segment errors discarded from SYSUT1'    11546014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               11547014
         BASR  R14,R15                Print warning message             11548014
Z0060    DS    0H                                                       11549014
         L     R10,@_ODT_Ptr          POINT TO OUTPUT ENV TABLE         11550014
         LH    R9,ODT_Entry_Count-OutDesc_Tbl(R10) GET NBR ENTRIES      11551014
         LA    R10,ODT_Entry-OutDesc_Tbl(R10) AND POINT TO FIRST        11552014
         CH    R9,=H'1'               MORE THAN 1 ENTRY?                11553014
         BNH   Z0070                  No, continue                      11554014
         ST    R10,@Z_Default_ODTE_Ptr Yes, save its address            11555014
         B     Z0160                  and skip it until last            11556014
Z0070    DS    0H                                                       11557014
         ICM   R15,B'1111',@_SYSPRINT_DCB_Ptr A(Sysprint DCB)           11558014
         BZ    Z0090                  None, skip                        11559014
         TM    DCBOFLGS-IHADCB(R15),DCBOFOPN Yes, is it open?           11560014
         BNO   Z0090                  NO, SKIP                          11561014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               11562014
         BASR  R14,R15                GET A NEW SYSPRINT RECORD         11563014
         L     R1,@_SYSPRINT_Rec_Ptr  POINT TO IT                       11564014
         MVC   20(27,R1),=C'Number of          records:'                11565014
         MVC   30(8,R1),ODTE_DDName   MOVE OUTPUT DDNAME                11566014
         MVC   50(12,R1),=X'402020206B2020206B202120'                   11567014
         ICM   R15,B'1111',ODTE_Selected Get selected counter           11568014
         BZ    Z0080                  None, skip                        11569014
         OI    @Z_Select_OK,L'@Z_Select_OK Yes, set our flag            11570014
Z0080    DS    0H                                                       11571014
         CVD   R15,@Z_DBLWD           PACK IT                           11572014
         ED    50(12,R1),@Z_DBLWD+3   AND EDIT IT                       11573014
         TM    ODTE_Trunc,L'ODTE_Trunc  ANY RECORDS TRUNCATED?          11574014
         BZ    Z0090                  NO, SKIP                          11575014
         L     R15,=AL4(G0010)        A(SYSPRINT routine)               11576014
         BASR  R14,R15                YES, SKIP A LINE                  11577014
         L     R1,@_SYSPRINT_Rec_Ptr  GET A(SYSPRINT)                   11578014
         MVC   10(73,R1),=C'*****  WARNING - At least 1 record truncate*11579014
               d on XXXXXXXX - WARNING  *****'                          11580014
         MVC   58(8,R1),ODTE_DDName   MOVE OUTPUT DDNAME                11581014
Z0090    DS    0H                                                       11582014
                                                                        11583014
*-------------------------------------------------------------------*   11584014
*                                                                   *   11585014
*    LAST CALL TO INPUT EXIT, AND DELETE, IF PRESENT                *   11586014
*                                                                   *   11587014
*-------------------------------------------------------------------*   11588014
                                                                        11589014
         ICM   R15,B'1111',ODTE_InExit_Ptr A(INPUT EXIT                 11590014
         BZ    Z0100                 None, SKIP                         11591014
         SLR   R1,R1                 Yes, clear A(SMF record)           11592014
         SLR   R0,R0                 Flag as an InExit                  11593014
         L     R15,=AL4(X0010)       A(Exit interface)                  11594014
         BASR  R14,R15               Go do it                           11595014
         L     R15,@Z_Default_ODTE_Ptr A(Default entry)                 11596014
         CR    R10,R15               Are we doing default?              11597014
         BE    Z0095                 Yes, skip                          11598014
         CLC   ODTE_InExit,ODTE_InExit-ODT_Entry(R15) No, default exit? 11599014
         BE    Z0100                 YES, DON'T DELETE IT               11600014
Z0095    DS    0H                                                       11601014
         LA    R0,ODTE_InExit        A(EXIT NAME)                       11602014
         DELETE EPLOC=(0)            AND DELETE IT                      11603014
                                                                        11604014
         ICM   R1,B'1111',ODTE_InExit_Parm_Ptr Get A(Parm)              11605014
         BZ    Z0100                  None, skip                        11606014
         LH    R0,0(0,R1)             Yes, get its length               11607014
         A     R0,=AL4(L'@HH_ParmLen) and bump for the length field     11608014
         STORAGE RELEASE,             Free the storage                 +11609014
               ADDR=(1),               starting here                   +11610014
               LENGTH=(0)              for this long                    11611014
                                                                        11612014
         SLR   R15,R15                Clear work register               11613014
         ST    R15,ODTE_InExit_Parm_Ptr and clear parm pointer          11614014
Z0100    DS    0H                                                       11615014
                                                                        11616014
*-------------------------------------------------------------------*   11617014
*                                                                   *   11618014
*    LAST CALL TO OUTPUT EXIT, AND DELETE, IF PRESENT               *   11619014
*                                                                   *   11620014
*-------------------------------------------------------------------*   11621014
                                                                        11622014
         ICM   R15,B'1111',ODTE_OutExit_Ptr  A(OUTPUT EXIT)             11623014
         BZ    Z0110                 None, SKIP                         11624014
         SLR   R1,R1                 Yes, clear A(SMF record)           11625014
         LA    R0,1                  Flag as an OutExit                 11626014
         L     R15,=AL4(X0010)       A(Exit interface)                  11627014
         BASR  R14,R15               Go do it                           11628014
         L     R15,@Z_Default_ODTE_Ptr A(Default entry)                 11629014
         CR    R10,R15               Are we doing default?              11630014
         BE    Z0105                 Yes, skip                          11631014
         CLC   ODTE_OutExit,ODTE_OutExit-ODT_Entry(R15) No, default?    11632014
         BE    Z0110                 YES, DON'T DELETE IT               11633014
Z0105    DS    0H                                                       11634014
         LA    R0,ODTE_OutExit       A(EXIT NAME)                       11635014
         DELETE EPLOC=(0)            AND DELETE IT                      11636014
                                                                        11637014
         ICM   R1,B'1111',ODTE_OutExit_Parm_Ptr Get A(Parm)             11638014
         BZ    Z0110                  None, skip                        11639014
         LH    R0,0(0,R1)             Yes, get its length               11640014
         A     R0,=AL4(L'@HH_ParmLen) and bump for the length field     11641014
         STORAGE RELEASE,             Free the storage                 +11642014
               ADDR=(1),               starting here                   +11643014
               LENGTH=(0)              for this long                    11644014
                                                                        11645014
         SLR   R15,R15                Clear work register               11646014
         ST    R15,ODTE_OutExit_Parm_Ptr and clear parm pointer         11647014
Z0110    DS    0H                                                       11648014
                                                                        11649014
*-------------------------------------------------------------------*   11650014
*                                                                   *   11651014
*    CLOSE THE OUTPUT DCB, IF OPEN.                                 *   11652014
*                                                                   *   11653014
*-------------------------------------------------------------------*   11654014
                                                                        11655014
         ICM   R2,B'1111',ODTE_DCB_Ptr A(Output DCB)                    11656014
         BZ    Z0130                  None, skip                        11657014
         TM    DCBOFLGS-IHADCB(R2),DCBOFOPN Yes, is it open?            11658014
         BZ    Z0120                  NO, BETTER NOT CLOSE IT           11659014
         MVC   @Z_CLOSE,Z_CLOSE           YES, MOVE CLOSE PARMS         11660014
         CLOSE ((2)),MODE=31,MF=(E,@Z_CLOSE)    AND CLOSE IT            11661014
Z0120    DS    0H                                                       11662014
         LA    R0,B_SYSUT2_DCB_Length Length of DCB                     11663014
         LR    R1,R2                  A(DCB storage)                    11664014
         STORAGE RELEASE,             Get storage                      +11665014
               LENGTH=(0),             for the DCB                     +11666014
               ADDR=(1)                which is here                    11667014
         SLR   R15,R15                Clear work register               11668014
         ST    R15,ODTE_DCB_Ptr       No more DCB                       11669014
Z0130    DS    0H                                                       11670014
                                                                        11671014
*-------------------------------------------------------------------*   11672014
*                                                                   *   11673014
*    FREE UP RECORD TYPE TABLE, IF IT EXISTS.                       *   11674014
*                                                                   *   11675014
*-------------------------------------------------------------------*   11676014
                                                                        11677014
         ICM   R1,B'1111',ODTE_RTT_Ptr A(RECORD TYPE TABLE)             11678014
         BZ    Z0140                  None, BETTER NOT FREE IT          11679014
         L     R15,@Z_Default_ODTE_Ptr A(Default entry)                 11680014
         CR    R10,R15               Are we doing default?              11681014
         BE    Z0135                 Yes, skip                          11682014
         CLC   ODTE_RTT_Ptr,ODTE_RTT_Ptr-ODT_Entry(R15) No, default?    11683014
         BE    Z0140                 YES, DON'T DELETE IT               11684014
Z0135    DS    0H                                                       11685014
         L     R0,0(0,R1)             NO, GET SUBPOOL AND LENGTH        11686014
         STORAGE RELEASE,               Free our storage               +11687014
               ADDR=(1),                 starting here                 +11688014
               LENGTH=(0)                for this long                  11689014
Z0140    DS    0H                                                       11690014
                                                                        11691014
*-------------------------------------------------------------------*   11692014
*                                                                   *   11693014
*    FREE UP JOBNAME TABLE, IF IT EXISTS.                           *   11694014
*                                                                   *   11695014
*-------------------------------------------------------------------*   11696014
                                                                        11697014
         ICM   R1,B'1111',ODTE_JNT_Ptr A(JOBN TABLE)                    11698014
         BZ    Z0150                  None, BETTER NOT FREE IT          11699014
         L     R15,@Z_Default_ODTE_Ptr A(Default entry)                 11700014
         CR    R10,R15               Are we doing default?              11701014
         BE    Z0145                 Yes, skip                          11702014
         CLC   ODTE_JNT_Ptr,ODTE_JNT_Ptr-ODT_Entry(R15) No, default?    11703014
         BE    Z0150                 YES, DON'T DELETE IT               11704014
Z0145    DS    0H                                                       11705014
         L     R0,0(0,R1)             YES, GET SUBPOOL AND LENGTH       11706014
         STORAGE RELEASE,               Free our storage               +11707014
               ADDR=(1),                 starting here                 +11708014
               LENGTH=(0)                for this long                  11709014
Z0150    DS    0H                                                       11710014
                                                                        11711014
*-------------------------------------------------------------------*   11712014
*                                                                   *   11713014
*    FREE UP DATA TABLE, IF PRESENT.                                *   11714014
*                                                                   *   11715014
*-------------------------------------------------------------------*   11716014
                                                                        11717014
         ICM   R1,B'1111',ODTE_DT_Ptr A(DATA TABLE                      11718014
         BZ    Z0160                 None SKIP                          11719014
         L     R15,@Z_Default_ODTE_Ptr A(Default entry)                 11720014
         CR    R10,R15               Are we doing default?              11721014
         BE    Z0155                 Yes, skip                          11722014
         CLC   ODTE_DT_Ptr,ODTE_DT_Ptr-ODT_Entry(R15) No, default?      11723014
         BE    Z0160                 YES, DON'T DELETE IT               11724014
Z0155    DS    0H                                                       11725014
         L     R0,0(0,R1)            YES, GET SUBPOOL AND LENGTH        11726014
         STORAGE RELEASE,               Free our storage               +11727014
               ADDR=(1),                 starting here                 +11728014
               LENGTH=(0)                for this long                  11729014
Z0160    DS    0H                                                       11730014
         AL    R10,=AL4(ODTE_Length)  A(NEXT ENTRY)                     11731014
         BCT   R9,Z0070               GO DO IT TOO                      11732014
         L     R10,@_ODT_Ptr          A(ENVIRONMENT TABLE)              11733014
         LH    R9,ODT_Entry_Count-OutDesc_Tbl(R10) GET NBR ENTRIES      11734014
         CH    R9,=H'1'               MORE THAN 1 ENTRY?                11735014
         BNH   Z0190                  NO, SKIP                          11736014
         LA    R10,ODT_Entry-OutDesc_Tbl(R10) YES, POINT TO DEFAULT     11737014
         ICM   R15,B'0010',ODTE_InExit_Ptr GET A(INEXIT)                11738014
         BZ    Z0170                  None, SKIP                        11739014
         LA    R0,ODTE_InExit         YES, POINT TO IT'S NAME           11740014
         DELETE EPLOC=(0)             AND DELETE IT                     11741014
         XC    ODTE_InExit_Ptr,ODTE_InExit_Ptr CLEAR EXIT ADDRESS       11742014
                                                                        11743014
         ICM   R1,B'1111',ODTE_InExit_Parm_Ptr Get A(Parm)              11744014
         BZ    Z0170                  None, skip                        11745014
         LH    R0,0(0,R1)             Yes, get its length               11746014
         A     R0,=AL4(L'@HH_ParmLen) and bump for the length field     11747014
         STORAGE RELEASE,             Free the storage                 +11748014
               ADDR=(1),               starting here                   +11749014
               LENGTH=(0)              for this long                    11750014
                                                                        11751014
         SLR   R15,R15                Clear work register               11752014
         ST    R15,ODTE_InExit_Parm_Ptr and clear parm pointer          11753014
Z0170    DS    0H                                                       11754014
         ICM   R15,B'1111',ODTE_OutExit_Ptr GET A(OUTEXIT)              11755014
         BZ    Z0180                  None, SKIP                        11756014
         LA    R0,ODTE_OutExit        YES, POINT TO IT'S NAME           11757014
         DELETE EPLOC=(0)             AND DELETE IT                     11758014
         XC    ODTE_OutExit_Ptr,ODTE_OutExit_Ptr CLEAR EXIT ADDRESS     11759014
                                                                        11760014
         ICM   R1,B'1111',ODTE_OutExit_Parm_Ptr Get A(Parm)             11761014
         BZ    Z0180                  None, skip                        11762014
         LH    R0,0(0,R1)             Yes, get its length               11763014
         A     R0,=AL4(L'@HH_ParmLen) and bump for the length field     11764014
         STORAGE RELEASE,             Free the storage                 +11765014
               ADDR=(1),               starting here                   +11766014
               LENGTH=(0)              for this long                    11767014
                                                                        11768014
         SLR   R15,R15                Clear work register               11769014
         ST    R15,ODTE_OutExit_Parm_Ptr and clear parm pointer         11770014
Z0180    DS    0H                                                       11771014
         L     R15,@_ODT_Ptr          A(OUR TABLE)                      11772014
         LA    R9,1                   DO A SINGLE ENTRY                 11773014
         STH   R9,ODT_Entry_Count-OutDesc_Tbl(R15) PRETEND ONLY 1 ENTRY 11774014
         B     Z0090                  AND FREE UP DEFAULT TABLES        11775014
Z0190    DS    0H                                                       11776014
                                                                        11777014
         DROP  R10                                                      11778014
                                                                        11779014
*-------------------------------------------------------------------*   11780014
*                                                                   *   11781014
*    Print the counters by record type                              *   11782014
*                                                                   *   11783014
*-------------------------------------------------------------------*   11784014
                                                                        11785014
                                                                        11786014
*-------------------------------------------------------------------*   11787014
*                                                                   *   11788014
*    CLOSE SYSPRINT, IF REQUIRED.                                   *   11789014
*                                                                   *   11790014
*-------------------------------------------------------------------*   11791014
                                                                        11792014
         ICM   R2,B'1111',@_SYSPRINT_DCB_Ptr  A(SYSPRINT DCB)           11793014
         BZ    Z0270                  None, skip                        11794014
         TM    DCBOFLGS-IHADCB(R2),DCBOFOPN SYSPRINT OPEN?              11795014
         BZ    Z0260                  NO, BETTER NOT CLOSE IT           11796014
         MVC   @Z_CLOSE,Z_CLOSE       YES, MOVE CLOSE PARMS             11797014
         CLOSE ((2)),MODE=31,MF=(E,@Z_CLOSE)    AND CLOSE IT            11798014
Z0260    DS    0H                                                       11799014
         L     R1,@_SYSPRINT_DCB_Ptr  A(SYSPRINT DCB)                   11800014
         STORAGE RELEASE,               Free the storage               +11801014
               ADDR=(R1),                starting here                 +11802014
               LENGTH=G_SYSPRINT_DCB_Length this long                   11803014
                                                                        11804014
         SLR   R15,R15                  Clear work register             11805014
         ST    R15,@_SYSPRINT_DCB_Ptr   No more DCB                     11806014
                                                                        11807014
         ICM   R1,B'1111',@_SYSPRINT_Y0010_Ptr                          11808014
         BZ    Z0270                                                    11809014
         STORAGE RELEASE,               Free the storage               +11810014
               ADDR=(R1),                starting here                 +11811014
               LENGTH=Y_Code_End-Y0010   this long                      11812014
                                                                        11813014
         SLR   R15,R15                  Clear work register             11814014
         ST    R15,@_SYSPRINT_Y0010_Ptr No more print interface         11815014
                                                                        11816014
         DELETE EP=SMFSLCTP             Un-IDENTIFY it                  11817014
                                                                        11818014
Z0270    DS    0H                                                       11819014
                                                                        11820014
*-------------------------------------------------------------------*   11821014
*                                                                   *   11822014
*    Free the Output Descriptor Table                               *   11823014
*                                                                   *   11824014
*-------------------------------------------------------------------*   11825014
                                                                        11826014
         ICM   R1,B'1111',@_ODT_Ptr   A(ENVIRONMENT TABLE)              11827014
         BZ    Z0280                  None, dont free it                11828014
         L     R0,ODT_Subp_Len-OutDesc_Tbl(R1) GET SUBPOOL, LENGTH      11829014
         STORAGE RELEASE,               Free our storage               +11830014
               ADDR=(1),                 starting here                 +11831014
               LENGTH=(0)                for this long                  11832014
         SLR   R15,R15                Clear work register               11833014
         ST    R15,@_ODT_Ptr          No more ODT                       11834014
Z0280    DS    0H                                                       11835014
         B     Z9010                  and exit                          11836014
Z9010    DS    0H                                                       11837014
                                                                        11838014
*-------------------------------------------------------------------*   11839014
*                                                                   *   11840014
*    Set the return code, if anything has been selected or not      *   11841014
*                                                                   *   11842014
*-------------------------------------------------------------------*   11843014
                                                                        11844014
         SLR   R15,R15                  Assume all is OK                11845014
         TM    @Z_Select_OK,L'@Z_Select_OK Anything selected?           11846014
         BO    Z9020                    Yes, OK                         11847014
         LA    R15,8                    No, set return code             11848014
Z9020    DS    0H                                                       11849014
                                                                        11850014
*-------------------------------------------------------------------*   11851014
*                                                                   *   11852014
*    Free up our local storage ...                                  *   11853014
*                                                                   *   11854014
*-------------------------------------------------------------------*   11855014
                                                                        11856014
         LR    R3,R15                   Save return code                11857014
         LA    R1,@Z_Dynam              A(Local storage)                11858014
         L     R13,4(0,R13)             Rescue A(HSA)                   11859014
                                                                        11860014
         STACK POP,                     Free the stack area            +11861014
               ADDR=(R1),                starting here                 +11862014
               STACK=@_31Bit_Stack_Ptr   on this stack                  11863014
         LR    R15,R3                   Restore return code             11864014
                                                                        11865014
*-------------------------------------------------------------------*   11866014
*                                                                   *   11867014
*    ... and return to caller (return code in R15)                  *   11868014
*                                                                   *   11869014
*-------------------------------------------------------------------*   11870014
                                                                        11871014
         L     R14,12(0,R13)            Restore return address          11872014
         LM    R0,R12,20(R13)           Restore other registers         11873014
         BSM   0,R14                    and return                      11874014
                                                                        11875014
Z_CLOSE    CLOSE (0),MODE=31,MF=L                                       11876014
Z_CLOSE_Length EQU   *-Z_CLOSE                                          11877014
         LTORG                                                          11878014
                                                                        11879014
@Z_Dynam        DSECT                 Dynamic area for Z                11880014
@Z_Save         DS    18F              O/S Style save area              11881014
@Z_DBLWD        DS    D                Work area                        11882014
@Z_Default_ODTE_Ptr DS AL4             A(Defalt ODT Entry)              11883014
@Z_Parms        DS    3AL4             Parms for F0010                  11884014
@Z_Return_Ptr   DS    AL4              A(Last byte formatted by F0010)  11885014
@Z_InExit_Parm  DS    2AL4             Parms for InExit                 11886014
@Z_OutExit_Parm DS    2AL4             Parm list for OutExit            11887014
                DS    X                Flag Byte                        11888014
@Z_Select_OK    EQU   *-1,X'80'         1... .... At least 1 rec sel    11889014
@Z_Macros       DS    0F               Macro area                       11890014
@Z_Close        DS    CL(Z_CLOSE_Length)  Close macro                   11891014
                ORG   @Z_Macros                                         11892014
@Z_DateConv_Area DS   CL(DConv_Area_Length)  Date conversion area       11893014
                ORG                                                     11894014
                DS    0D               Alignment                        11895014
@Z_DynLen       EQU   *-@Z_Dynam      Length of storage required        11896014
                                                                        11897014
SMFSLCT  RSECT                                                          11898014
                                                                        11899014
         DROP  R11,R13                                                  11900014
         POP   USING                                                    11901014
         END                                                            11902014
/*
//LKED.SYSLMOD DD DSN=WEGSCD.LOADLIB,DISP=SHR
//
