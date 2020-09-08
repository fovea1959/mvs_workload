//VTOC$    JOB (001),'INSTALL VTOC',CLASS=A,MSGCLASS=X                  00010000
//*                                                                     00020000
//*------------------------------------------------------------------*  00030000
//*  INSTALL THE VTOC TSO COMMAND FROM CBT129 -                      *  00040000
//*    CREATE TEMPORARY PDS INTO WHICH SOURCE, MACROS, HELP LOADED   *  00050000
//*    ASSEMBLE AND LINK TO TSO COMMAND LIBRARY                      *  00060000
//*------------------------------------------------------------------*  00070000
//*                                                                     00080000
//VTOCLIB PROC                                                          00090000
//*                                                                     00100000
//LOADSRC EXEC PGM=IEBUPDTE,PARM=NEW                                    00110000
//SYSUT2   DD  DSN=VTOC.SOURCE,DISP=(,CATLG,DELETE),                    00120000
//             UNIT=3350,SPACE=(CYL,(10,5,5)),                          00130000
//             DCB=(SYS1.MACLIB)                                        00140000
//SYSPRINT DD  SYSOUT=*                                                 00150000
//        PEND                                                          00160000
//*                                                                     00170000
//VTOCASM PROC MEMBER=                                                  00180000
//ASM     EXEC PGM=IEUASM,PARM=(LOAD,NODECK,'LINECNT=55')               00190000
//SYSLIB   DD  DSN=VTOC.SOURCE,DISP=SHR                                 00200000
//         DD  DSN=SYS1.AMODGEN,DISP=SHR   NEED SU60 MACROS             00210000
//         DD  DSN=SYS1.MACLIB,DISP=SHR    NEED SU60 MACROS             00220000
//SYSUT1   DD  UNIT=SYSDA,SPACE=(TRK,(50,20))                           00230000
//SYSUT2   DD  UNIT=SYSDA,SPACE=(TRK,(50,20))                           00240000
//SYSUT3   DD  UNIT=SYSDA,SPACE=(TRK,(50,20))                           00250000
//SYSPRINT DD  SYSOUT=*                                                 00260000
//SYSGO    DD  DSN=&&TEMPDS,UNIT=SYSDA,SPACE=(TRK,(50,20)),             00270000
//             DISP=(MOD,PASS)                                          00280000
//SYSIN    DD  DSN=VTOC.SOURCE(&MEMBER),DISP=SHR                        00290000
//*                                                                     00300000
//LKED EXEC PGM=IEWL,                                                   00310000
//          PARM='XREF,LET,LIST,AC=0,SIZE=(140K,6400)',                 00320000
//          COND=(8,LT,ASM)                                             00330000
//SYSLIB   DD  DSN=SYS2.CMDLIB,DISP=SHR                                 00340000
//SYSLIN   DD  DSN=&&TEMPDS,DISP=(OLD,DELETE)                           00350000
//         DD  DDNAME=SYSIN                                             00360000
//SYSLMOD  DD  DSN=SYS2.CMDLIB(&MEMBER),DISP=SHR                        00370000
//SYSUT1   DD  UNIT=SYSDA,SPACE=(TRK,(50,20))                           00380000
//SYSPRINT DD  SYSOUT=*                                                 00390000
//     PEND                                                             00400000
//*                                                                     00410000
//S1      EXEC VTOCLIB                                                  00420000
//SYSIN    DD  DATA,DLM='><'                                            00430000
./       ADD   NAME=$HELP
)F FUNCTION -                                                           00000100
  THE VTOC COMMAND DISPLAYS SELECTED DATA SETS ON A DISK OR SET OF      00000200
  DISKS.  EACH DISK HAS A VOLUME TABLE OF CONTENTS ( VTOC ).  THIS      00000300
  TABLE IS SEARCHED FOR DATA SETS THAT MEET THE SPECIFICATIONS.         00000400
)X SYNTAX -                                                             00000500
         VTOC     'VOLUME-LIST'           LEVEL('DSNAME-START')         00000600
                                          CONTAINING('DSNAME-STRING')   00000700
                                          ENDING('DSNAME-END')          00000800
                                          BREAK('BREAK-CHARS')          00000900
                  CAT                                                   00001000
                  NOSORT/SORT('SORT-FIELDS')                            00001100
                  NOPRINT/PRINT('PRINT-OP' ('PRINT-ITEM-LIST'))         00001200
                  LIMIT('KEYWORD' 'OPER' 'VALUE')                       00001300
                  AND1/OR1('KEYWORD' 'OPER' 'VALUE')                    00001400
                  AND2/OR2('KEYWORD' 'OPER' 'VALUE')                    00001500
                  AND3/OR3('KEYWORD' 'OPER' 'VALUE')                    00001600
                  CHARS('CHARS-PER-LINE')                               00001700
                  LINES('LINES-PER-PAGE')                               00001800
                  NOHEADING/HEADING('TEXT')                             00001900
                  DSNLEN('LENGTH')                                      00002000
  REQUIRED - 'VOLUME-LIST'                                              00002100
  DEFAULTS - LIST ALL DATA SETS ON THE VOLUME(S) SELECTED.              00002200
             SORT, PRINT                                                00002300
)O OPERANDS -                                                           00002400
))'VOLUME-LIST'     - A VOLUME SERIAL NUMBER OR A LIST OF VOLUMES.      00002500
              IF THE FIRST ONE TO FIVE CHARACTERS OF A VOLUME           00002600
              SERIAL NUMBER ARE ENTERED, ALL VOLUMES THAT ARE           00002700
              MOUNTED ON THE MACHINE WHICH START WITH THOSE             00002800
              CHARACTERS WILL BE LISTED.                                00002900
              IF 'ALL' IS SPECIFIED, ALL NON-VIRTUAL VOLUMES            00003000
              WHICH ARE ONLINE AND READY WILL BE PROCESSED.             00003100
              IF 'ALV' IS SPECIFIED, ALL VIRTUAL VOLUMES                00003200
              WHICH ARE ONLINE AND READY WILL BE PROCESSED,             00003300
              IF THEY ARE MOUNTED PRIVATE/RESERVED OR                   00003400
              PRIVATE/RESIDENT.                                         00003500
))LEVEL('DSNAME-START')  - SPECIFIES THE HIGH LEVEL QUALIFIERS TO BE    00003600
         SEARCHED.  THIS WILL NOT BE PREFIXED BY YOUR USERID OR         00003700
         PREFIX.  ONLY DATA SETS STARTING WITH THESE PREFIXES WILL      00003800
         BE LISTED.                                                     00003900
))CONTAINING('DSNAME-STRING') - SPECIFIES A CHARACTER STRING CONTAINED  00004000
         IN THE DATA SET NAME.  AT LEAST ONE OF THE STRINGS MUST        00004100
         BE IN THE DSNAME FOR THE DATA SET TO BE LISTED.                00004200
         THESE STRINGS MUST CONFORM TO DSNAME STANDARDS.                00004300
         THEY CANNOT BEGIN WITH A PERIOD OR A NUMBER.                   00004400
))ENDING('DSNAME-END')  - SPECIFIES THE ENDING CHARACTERS OF THE        00004500
         DSNAME.  THE FINAL NONBLANK CHARACTERS OF THE DSNAME MUST      00004600
         BE ONE OF THESE STRINGS TO ALLOW THE DATA SET TO BE LISTED.    00004700
         THESE STRINGS MUST CONFORM TO DSNAME STANDARDS.                00004800
))CAT      -  A LOCATE IS DONE FOR EACH DSNAME ON THE VOLUMES LISTED    00004900
         AND STATUS IS INDICATED.  NOTE -  THIS OPTION USES A           00005000
         CONSIDERABLE AMOUNT OF PROCESSING TIME.                        00005100
         C -  CATALOGGED ON THIS VOLUME                                 00005200
         N -  NOT CATALOGGED                                            00005300
         W -  CATALOGGED ON ANOTHER VOLUME                              00005400
         E -  CATALOG PROCESSING ERROR                                  00005500
))NOSORT   -  THE DATA SETS ARE NOT SORTED.  THEY ARE OUTPUT AS THEY    00005600
              ARE FOUND.                                                00005700
))SORT('SORT-FIELDS') DATA SETS ARE SORTED INTO ALPHABETICAL ORDER,     00005800
              BASED UPON THE SORT FIELDS SPECIFIED.                     00005900
              DSNAME,VOLUME,ALLOC,USED,UNUSED,PCT,EX,DSO,RFM,           00006000
              LRECL,BLKSZ,CDATE,EXPDT,REFDT ARE VALID SORT FIELDS.      00006100
              'A/D'   ARE REQUIRED AFTER EACH SORT FIELD TO INDICATE    00006200
              ASCENDING/DESCENDING SEQUENCE.                            00006300
))BREAK('BREAK-CHARS') - THE LISTING WILL CONTAIN A NEW HEADER,         00006400
              ( ON A NEW PAGE IF THE VTOCOUT DD CARD OPTION IS USED ),  00006500
              WHENEVER THE SPECIFIED NUMBER OF CHARACTERS DIFFERS       00006600
              FROM THE PRECEDING DATA SET.  THIS OPTION FUNCTIONS       00006700
              ONLY WITH THE SORT OPTION.                                00006800
))CHARS('CHARS-PER-LINE') - SPECIFIES THE NUMBER OF CHARACTERS ON EACH  00006900
              LINE OF OUTPUT.  THE DEFAULT IS 150 FOR PRINT AND THE     00007000
              LINESIZE OF THE TERMINAL FOR TSO SESSIONS.                00007100
              YOU CAN GET MORE INFORMATION BY SPECIFYING A LARGER       00007200
              NUMBER OF CHARACTERS PER LINE OR YOU CAN LIMIT THE        00007300
              PRINTING BY SETTING A SMALLER NUMBER OF CHARACTERS        00007400
              PER LINE OF OUTPUT.                                       00007500
))LINES('LINES-PER-PAGE') - SPECIFIES THE NUMBER OF LINES BEFORE A NEW  00007600
              TITLE LINE IS PRODUCED.  IT DEFAULTS TO 60 FOR PRINT      00007700
              AND TO THE SCREEN SIZE FOR TSO SESSIONS.                  00007800
))NOHEADING   - DO NOT PRODUCE A HEADING.  THE HEADING WILL ONLY BE     00007900
              OUTPUT IF THE VTOCOUT DD STATEMENT IS PRESENT.            00008000
))HEADING('TEXT') - IF A DD STATEMENT WITH A DDNAME OF VTOCOUT IS       00008100
              PRESENT, THIS TEXT WILL BE USED TO BEGIN EVERY PAGE.      00008200
              CARRIAGE CONTROL SHOULD BE INCLUDED ( ASA ).  THE         00008300
              DEFAULT HEADER CONSISTS OF VTOC COMMAND VERSION 02        00008400
              AND THE COMMAND THAT WAS ENTERED.                         00008500
))DSNLEN('LENGTH') - SPECIFIES THE LENGTH OF THE DSNAME TO PRINT.       00008600
              THE REST OF THE DSNAME IS TRUNCATED.  THE CHARS PARAMETER 00008700
              WILL ALSO CAUSE THE DSNAME TO BE TRUNCATED, IF THE NAME   00008800
              AND THE PRECEDING INFORMATION EXCEEDS THE PRINT LINE.     00008900
))NOPRINT     - SPECIFIES THAT INDIVIDUAL ITEMS ARE NOT TO BE LISTED.   00009000
              THE COMMAND CAN BE USED TO CALCULATE TOTALS.              00009100
))PRINT('PRINT-OP' ('PRINT-ITEM-LIST')) -                               00009200
              SPECIFIES THE ITEMS TO PRINT.  THE 'PRINT-OP' IS THE      00009300
              OPERATION TO BE DONE.  THEY INCLUDE THE FOLLOWING.        00009400
                NEW - THE 'PRINT-ITEM-LIST' IS A COMPLETE LIST OF WHAT  00009500
                      TO PRINT.                                         00009600
                REP - THE FIRST 'PRINT-ITEM' WILL BE REPLACED WITH THE  00009700
                      REST OF THE ITEMS ON THE LIST.                    00009800
                ADD - THE REST OF THE 'PRINT-ITEM-LIST' WILL BE ADDED   00009900
                      AFTER THE FIRST ITEM ON THE LIST.                 00010000
                DEL - THE ITEMS ON THE 'PRINT-ITEM-LIST' WILL NOT BE    00010100
                      PRINTED.                                          00010200
              THE 'PRINT-ITEM-LIST' NAMES ARE THE SAME KEYWORDS USED    00010300
              IN LIM, AND, AND OR FUNCTIONS AND ARE ALSO THE TITLES     00010400
              AS PRINTED.                                               00010500
              THE ADD, DEL, AND REP PRINT OPERATIONS REFER TO THE       00010600
              DEFAULT PRINT LIST.  THE DEFAULT LIST IS ALLOC, UNUSED,   00010700
              PCT, EX, DSO, RFM, BLKSZ, LRECL, REFDT, CDATE, VOLUME,    00010800
              DSNAME, EXPDT, SECQ, SECT, ROUND, PASS, ACTION,           00010900
              AND TYPE.  AS NOTED UNDER CHARS ABOVE, ONLY THE ITEMS     00011000
              THAT WILL FIT ON THE PRINT LINE WILL BE LISTED.           00011100
))LIMIT('VALUE' 'OPER' 'KEYWORD') - SPECIFIES WHICH DATA SETS ARE TO    00011200
              BE LISTED.  ONLY DATA SETS THAT SATISFY THE RELATION      00011300
              ARE LISTED.                                               00011400
))'KEYWORD'   - IS THE NAME OF A DATA SET FIELD.  THE LIST OF CURRENTLY 00011500
                PROVIDED FIELDS FOLLOWS.  THE KEYWORDS AND THEIR VALUES 00011600
                ARE THE SAME AS IN THE VTOC OUTPUT.                     00011700
                                                                        00011800
                      ALLOC      DATA SET ALLOCATION                    00011900
                                    NUMBER OF KILOBYTES, TRACKS,        00012000
                                    CYLINDERS, OR MEGABYTES ALLOCATED.  00012100
                                    DEFAULT IS KILOBYTES.               00012200
                      UNUSED     AMOUNT OF UNUSED SPACE IN THE DATA     00012300
                                    SET.  SAME UNITS AS IN ALLOC.       00012400
                      USED       AMOUNT OF SPACE USED IN THE DATA SET.  00012500
                                    SAME UNITS AS IN ALLOC.             00012600
                      PCT        PERCENTAGE OF SPACE USED IN THE DATA   00012700
                                    USED.                               00012800
                      EX         NUMBER OF EXTENTS IN THE DATA SET.     00012900
                      DSO        DATA SET ORGANIZATION                  00013000
                                    PS = SEQUENTIAL   PO = PARTITIONED  00013100
                                    VS = VSAM         IS = ISAM         00013200
                                    DA = DIRECT ACCESS  U = UNMOVEABLE  00013300
                      RFM        RECORD FORMAT                          00013400
                                    F  = FIXED        V  = VARIABLE     00013500
                                    U  = UNDEFINED    B  = BLOCKED      00013600
                                    T  = TRACK OVERFLOW                 00013700
                                    S  = SPANNED OR STANDARD            00013800
                                    A  = ASA CARRIAGE CONTROL           00013900
                                    M  = MACHINE CARRIAGE CONTROL       00014000
                      BLKSZ      BLOCKSIZE FOR PHYSICAL BLOCKS OF       00014100
                                    DATA.                               00014200
                      LRECL      LOGICAL RECORD LENGTH IN BYTES.        00014300
                      CDATE      CREATION DATE IN THE FORM YYDDD,       00014400
                                    SOMETIMES CALLED JULIAN.            00014500
                      EXPDT      EXPIRATION DATE IN THE SAME FORM.      00014600
                                    THIS FIELD IS RARELY USED HERE.     00014700
                      REFDT      LAST USE DATE IN THE SAME FORM.        00014800
                                    THIS DATE IS WHEN THE DATA SET WAS  00014900
                                    LAST OPENED.                        00015000
                      SECT       TYPE OF ALLOCATION                     00015100
                                    A  = ABSOLUTE TRACK                 00015200
                                    B  = BLOCKS                         00015300
                                    T  = TRACKS                         00015400
                                    C  = CYLINDERS                      00015500
                      PASS       PROTECTION INDICATORS                  00015600
                                    N  = NONE                           00015700
                                    R  = READ AND WRITE PROTECTION      00015800
                                    W  = WRITE PROTECTION               00015900
                      ROUND      SPACE ROUNDED UP TO CYLINDERS          00016000
                                    R  = ROUND                          00016100
                                    N  = NO ROUND                       00016200
                      CCHH       CYLINDER AND HEAD ADDRESS, IN 4 OR 8   00016300
                                    HEXADECIMAL DIGITS.  IF 4 DIGITS    00016400
                                    ARE USED, ONLY THE CYLINDER IS USED 00016500
                                    FOR COMPARISON, OTHERWISE, THE CYL  00016600
                                    AND TRACK ARE COMPARED.             00016700
                      VOLUME     VOLUME SERIAL NUMBER OR DISK NAME      00016800
                      DSNAME     NAME OF THE DATA SET                   00016900
                      ACTION     SOME ERROR INDICATIONS                 00017000
                      TYPE       RESERVED FOR EXIT USAGE.               00017100
))'OPER'      - IS AN OPERATOR.  THE LIST OF OPERATORS FOLLOWS.         00017200
                        EQ       IS EQUAL TO                            00017300
                        NE       IS NOT EQUAL TO                        00017400
                        LE       IS LESS THAN OR EQUAL TO               00017500
                        LT       IS LESS THAN                           00017600
                        GE       IS GREATER THAN OR EQUAL TO            00017700
                        GT       IS GREATER THAN                        00017800
))'VALUE'     - GIVES THE VALUE OF THE ITEM FOR COMPARISON, SUCH AS     00017900
              FB, PS, R, OR A NUMBER.                                   00018000
))AND1('VALUE' 'OPER' 'KEYWORD') -  SPECIFIES WHICH DATA SETS ARE TO    00018100
                 BE LISTED.  BOTH THE LIMIT AND THIS CONDITION MUST     00018200
                 BE TRUE TO ALLOW THE LISTING.                          00018300
))OR1('VALUE' 'OPER' 'KEYWORD') -   SPECIFIES WHICH DATA SETS ARE TO    00018400
                 BE LISTED.  EITHER THE LIMIT OR THIS CONDITION MUST    00018500
                 BE TRUE TO ALLOW THE LISTING.                          00018600
))AND2('VALUE' 'OPER' 'KEYWORD') -  SPECIFIES WHICH DATA SETS ARE TO    00018700
                 BE LISTED.  BOTH THE PREVIOS RESULT AND THIS           00018800
                 CONDITION MUST BE TRUE TO ALLOW THE LISTING.           00018900
))OR2('VALUE' 'OPER' 'KEYWORD') -   SPECIFIES WHICH DATA SETS ARE TO    00019000
                 BE LISTED.  EITHER THE PREVIOUS RESULT OR THIS         00019100
                 CONDITION MUST BE TRUE TO ALLOW THE LISTING.           00019200
))AND3('VALUE' 'OPER' 'KEYWORD') -  SPECIFIES WHICH DATA SETS ARE TO    00019300
                 BE LISTED.  BOTH THE PREVIOS RESULT AND THIS           00019400
                 CONDITION MUST BE TRUE TO ALLOW THE LISTING.           00019500
))OR3('VALUE' 'OPER' 'KEYWORD') -   SPECIFIES WHICH DATA SETS ARE TO    00019600
                 BE LISTED.  EITHER THE PREVIOUS RESULT OR THIS         00019700
                 CONDITION MUST BE TRUE TO ALLOW THE LISTING.           00019800
))EXAMPLES -                                                            00019900
  LIST ALL DATA SETS ON VOL             VTOC VOL                        00020000
  LIST ALL DATA SETS ON ALL VOLUMES     VTOC ALL                        00020100
  LIST ALL DATA SETS THAT START                                         00020200
     WITH  XXX   ON ANY MVXXXX VOLUME   VTOC MV LEV(XXX)                00020300
  LIST ALL RECENTLY CREATED DATA SETS   VTOC VOL LIM(CDATE GT 79001)    00020400
  LIST DATA SETS WITH UNUSED SPACE      VTOC VOL LIM(PCT LT 50)  -      00020500
                                                OR1(UNUSED GT 30)       00020600
  LIST DATA SETS WITH MULTIPLE EXTENTS  VTOC VOL LIM(EX GT 1)           00020700
  LIST ALL OF USER'S DATA ON TSO001   VTOC TSO001 LEVEL(TMTCEXX)        00020800
  LIST ALL DATA SETS OVER 100 TRKS      VTOC VOL LIM(ALLOC GT 100)      00020900
  LIST DATA SETS UNDER THE FIXED HEADS  VTOC VOL LIM(CC EQ 0001) -      00021000
                                                 OR1(CC EQ 0002)        00021100
  LIST USED SPACE INSTEAD OF UNUSED,                                    00021200
       IN TRACKS.                VTOC VOL PRINT(REP (UNUSED USED)) TRK  00021300
  LIST CLIST DATA SETS           VTOC VOL END(CLIST)                    00021400
  LIST ALL OF THE INFORMATION ABOUT                                     00021500
       USER'S DATA SETS AT A 3270.   VTOC VOL CHAR(150)                 00021600
  CHECK IF DATASETS ARE CATALOGGED ON THIS VOLUME.                      00021700
                  VTOC VOL CAT PRINT(NEW (ALLOC PCT CAT DSNAME))        00021800
  LIST ALL DATA SETS ON 335XXX VOLUMES SORTED BY ALLOC IN DESCENDING    00021900
       SEQUENCE, VOLUME AND DSNAME IN ASCENDING SEQUENCE.               00022000
       VTOC 335 SORT(ALLOC,D,VOLUME,A,DSNAME,A)                         00022100
./       ADD   NAME=$INST
//ARNIEX JOB 527TSO000S0008,CASINGHINO,CLASS=J,MSGCLASS=1               00000010
//*                                                                     00000020
//*                                                                     00000030
//*      TO INSTALL YOU MUST FIRST CREATE A DATASET                     00000040
//*      VTOC.INSTALL.ASM  AND PUT THE VTOC COMMAND                     00000050
//*      SOURCE MODULES AND MACROS INTO IT                              00000060
//*                                                                     00000070
//*      YOU MUST HAVE DONE THIS TO GET THIS                            00000080
//*      MEMBER OF WHICH YOU ARE NOW READING                            00000090
//*                                                                     00000100
//*      YOU ALSO NEED A LOAD LIBRARY CALLED                            00000110
//*      VTOC.INSTALL.LOAD                                              00000120
//*                                                                     00000130
//*                                                                     00000140
//VTOCASM  PROC   MEMBER=                                               00000150
//ASM EXEC PGM=IEUASM,PARM=(LOAD,NODECK,'LINECNT=55')                   00000160
//SYSLIB DD DSN=VTOC.INSTALL.ASM,DISP=SHR                               00000170
//       DD  DSN=SYS1.AMODGEN,DISP=SHR   NEED SU60 MACROS               00000180
//       DD  DSN=SYS1.MACLIB,DISP=SHR    NEED SU60 MACROS               00000190
//SYSUT1  DD  UNIT=SYSDA,SPACE=(TRK,(50,20))                            00000200
//SYSUT2  DD  UNIT=SYSDA,SPACE=(TRK,(50,20))                            00000210
//SYSUT3  DD  UNIT=SYSDA,SPACE=(TRK,(50,20))                            00000220
//SYSPRINT  DD  SYSOUT=1                                                00000230
//SYSGO  DD  DSN=&&TEMPDS,UNIT=SYSDA,SPACE=(TRK,(50,20)),               00000240
//    DISP=(MOD,PASS)                                                   00000250
//SYSIN    DD  DSN=VTOC.INSTALL.ASM(&MEMBER),DISP=SHR                   00000260
//LKED EXEC PGM=IEWL,                                                   00000270
//          PARM='XREF,LET,LIST,AC=0,SIZE=(140K,6400)',                 00000280
//          COND=(8,LT,ASM)                                             00000290
//SYSLIB   DD   DSN=VTOC.INSTALL.LOAD,DISP=SHR                          00000300
//SYSLIN   DD   DSN=&&TEMPDS,DISP=(OLD,DELETE)                          00000310
//     DD   DDNAME=SYSIN                                                00000320
//SYSLMOD DD DSN=VTOC.INSTALL.LOAD(&MEMBER),DISP=SHR                    00000330
//SYSUT1 DD UNIT=SYSDA,SPACE=(TRK,(50,20))                              00000340
//SYSPRINT  DD  SYSOUT=1                                                00000350
//     PEND                                                             00000360
//S1 EXEC PGM=IEFBR14                                                   00000370
//LOADLIB  DD   DSN=VTOC.INSTALL.LOAD,UNIT=SYSDA,DISP=(,CATLG),         00000380
//  SPACE=(CYL,(10,5,10)),DCB=(RECFM=U,BLKSIZE=6144)                    00000390
//S2 EXEC VTOCASM,MEMBER=VTOCCHEK                                       00000400
//S3 EXEC VTOCASM,MEMBER=VTOCEXCP                                       00000410
//S4 EXEC VTOCASM,MEMBER=VTOCFORM                                       00000420
//S5 EXEC VTOCASM,MEMBER=VTOCMSGX                                       00000430
//LKED.SYSLMOD DD DSN=VTOC.INSTALL.LOAD(VTOCMSG),DISP=SHR               00000440
//S6 EXEC VTOCASM,MEMBER=VTOCPRNT                                       00000450
//S7 EXEC VTOCASM,MEMBER=VTOCSORT                                       00000460
//S8 EXEC VTOCASM,MEMBER=VTOC                                           00000470
./       ADD   NAME=ALLOC
         MACRO                                                          07860000
&NAME    ALLOC &DUMMY,&PERM,&DSN=,&DDN=,&DDNRET=,&MEMBER=,&DISP=,      X07870000
               &VOL=,&UNIT=,&SYSOUT=,&FREE=,&COPIES=,&LABEL=,          X07880000
               &BLKSIZE=,&DEN=,&DSORG=,&KEYLEN=,&LRECL=,&RECFM=,       X07890000
               &PASWORD=,&DSNRET=,&MF=AUTO,&PREFIX=,&ERROR=,           X07900000
               &SPACE=,&F=,&FILE=,&DA=,&QNAME=,&DSORGRT=,              X07910000
               &VOLRET=,&DCBDSN=,&DCBDDN=,&SPECIAL=,&DDNTO=,           X07920000
               &FORMS=,&DEST=,&SSREQ=,&FORUSER=,&TU=,&DSNPDE=           07930000
.********************************************************************** 07940000
.*                                                                    * 07950000
.*    THIS MACRO PROVIDES A DYNAMIC ALLOCATION FUNCTION BY BUILDING   * 07960000
.*    A DYNAMIC ALLOCATION PARAMETER LIST AND INVOKING SVC 99.        * 07970000
.*    IT FIRST SETS UP A WORKAREA ENVIRONMENT FOR THE PARAMETER LIST  * 07980000
.*    AND THEN TESTS THE KEYWORDS SUPPLIED AND INVOKES INNER MACROS   * 07990000
.*    TO BUILD THE TEXT UNITS. THE INNER MACROS THEMSELVES USE INNER  * 08000000
.*    MACROS TO UPDATE GLOBAL VARIABLES, STORE TEXT UNIT POINTERS ETC * 08010000
.*    THERE ARE THREE WAYS OF SPECIFYING THE WORK AREA ADDRESS.       * 08020000
.*    A) MF=AUTO, MF=G, MF=(E,ADDRESS,LNTHSYMB).                      * 08030000
.*    IN THE FIRST FORM, AN INNER MACRO DYNSPACE IS CALLED TO NAME    * 08040000
.*    A WORK AREA, THE NAME BEING RETURNED IN THE GLOBAL SETC         * 08050000
.*    VARIABLE &DYNSP. A DSECT IS CREATED TO MAP THIS AREA.           * 08060000
.*    THE GLOBAL VARIABLES &DTUO (TEXT UNIT OFFSET COUNTER) AND       * 08070000
.*    &DTUPO (TEXT UNIT POINTER OFFSET ACCUMULATOR) ARE SET TO ZERO.  * 08080000
.*    THESE ACCUMULATORS ARE UPDATED AS EACH TEXT UNIT PROCESSOR      * 08090000
.*    AQUIRES STORAGE. AFTER ALL TEXT UNITS HAVE BEEN BUILT, THE      * 08100000
.*    AMOUNT OF SPACE USED IS CALCULATED, AND THE DYNSPACE MACRO IS   * 08110000
.*    THEN CALLED AGAIN TO LOG THE AMOUNT NEEDED. DYNSPACE SETS A     * 08120000
.*    GLOBAL VARIABLE &DYNSPQ TO THE HIGHEST AMOUNT ANY ALLOC OR      * 08130000
.*    FREE MACRO REQUESTED, AND WHEN CALLED WITH THE EXPAND OPTION,   * 08140000
.*    (NO OPERANDS OR NAME FIELD SUPPLIED), EXPANDS INTO A DS FOR     * 08150000
.*    THAT QUANTITY. (SEE DYNSPACE)                                   * 08160000
.*    MF=G SPECIFIES THAT THE ALLOC MACRO ENTER THE BEGIN MACRO       * 08170000
.*    WORKAREA TO ACQUIRE THE STORAGE NECESSARY. IT DOES THIS VIA     * 08180000
.*    THE RCPDS MACRO. (SEE RCPDS). HOWEVER, IF THE ALLOC MACRO IS    * 08190000
.*    CALLED SEVERAL TIMES WITH THIS OPTION, A LOT OF STORAGE WILL BE * 08200000
.*    USED UP, AS THE STORAGE WILL NOT BE SHARED. THUS, THIS FORM     * 08210000
.*    SHOULD ONLY BE USED IF THE ALLOC/FREE MACRO IS ONLY TO BE USED  * 08220000
.*    ONCE OR TWICE DURING AN ASSEMBLY.                               * 08230000
.*    MF=E CAUSES THE MACRO TO USE A USER SPECIFIED WORK AREA. THE    * 08240000
.*    SECOND PARAMETER GIVES THE NAME OF THE WORKAREA, AND AN         * 08250000
.*    OPTIONAL THIRD PARAMETER IS THE NAME OF A SYMBOL TO BE EQUATED  * 08260000
.*    TO THE LENGTH OF THE REQUIRED WORK AREA.                        * 08270000
.*                                                                    * 08280000
.*    DYNAMIC ALLOCATION FUNCTIONS ARE SIMILAR TO THOSE AVAILABLE    *  08290000
.*    WITH JCL, USING THE SAME KEYWORDS. HOWEVER, CERTAIN FORMATS    *  08300000
.*    ARE SLIGHTLY DIFFERENT. FOR INSTANCE, CERTAIN KEYWORDS CAN     *  08310000
.*    HAVE VARYING PARAMETERS, EG DATASET NAME, DDNAME, VOLSER ETC.  *  08320000
.*    PROVISION IS MADE FOR BOTH VARIABLE SPECIFICATION.             *  08330000
.*    IN THE ABSOLUTE FORM, THE PARAMETER IS ENTERED IN QUOTES,      *  08340000
.*    E.G.   ALLOC DSN='SYS1.LINKLIB',DISP=SHR                       *  08350000
.*    HOWEVER, THIS NAME REMAINS FIXED FOR THE ASSEMBLY.             *  08360000
.*    IN THE VARIABLE FORMAT, THE ADDRESS OF A LOCATOR IS SPECIFIED, *  08370000
.*    WHERE THE LOCATOR CONSISTS OF A SIX BYTE FIELD, THE FIRST 4    *  08380000
.*    BYTES OF WHICH POINT TO THE PARAMETER, WHILE THE NEXT TWO      *  08390000
.*    CONTAIN THE LENGTH.                                            *  08400000
.*    EG          ALLOC DSN=LOCATOR                                  *  08410000
.*       LOCATOR  DC    A(DSN),Y(12)                                 *  08420000
.*       DSN      DC    C'SYS1.LINKLIB'                              *  08430000
.*                                                                   *  08440000
.*       NUMERIC QUANTITIES E.G. COPIES= FOR SYSOUT, SHOULD EITHER   *  08450000
.*       SPECIFY A NUMERIC VALUE, COPIES=3,                          *  08460000
.*       A VALUE IN A REGISTER, COPIES=(R3),                         *  08470000
.*       OR THE NAME OFF A FULLWORD CONTAINING THE VALUE,            *  08480000
.*          COPIES=NUMCOPYS, WHERE NUMCOPYS IS THE NAME OF A         *  08490000
.*       FULLWORD FIELD.                                             *  08500000
.*                                                                   *  08510000
.*       OTHER KEYWORDS SUCH AS DISP= CAN ONLY HAVE THE ABSOLUTE     *  08520000
.*       FORM, AND VALUES SHOULD NOT BE ENTERED WITHIN QUOTES.       *  08530000
.*       ADDITIONAL FACILITIES NOT AVAILABLE WITH JCL ARE THE        *  08540000
.*       RETURN BY THE SYSTEM OF INFORMATION ON THE DATASET, EG      *  08550000
.*       DSORG. THIS IS DONE BY SPECIFYING DSORGRT=SYMBOL, WHERE     *  08560000
.*       SYMBOL IS A SYMBOL WHICH WILL BE EQUATED TO A TWO BYTE      *  08570000
.*       FIELD CONTAINING THE DSORG TYPE (SEE JOB MANAGEMENT,        *  08580000
.*       SUPERVISOR AND TSO).                                        *  08590000
.*       THE SYSTEM CAN ALSO GENERATE AND RETURN A DDNAME. THIS IS   *  08600000
.*       CARRIED OUT BY ENTERING DDNTO=(ADDR1,ADDR2,,...)            *  08610000
.*       WHERE ADDR1,ADDR2 ETC ARE THE NAMES OF 8 BYTE FIELDS WHICH  *  08620000
.*       ARE TO RECEIVE THE DDNAME.                                  *  08630000
.*       FOR FURTHER INFORMATION ON DYNAMIC ALLOCATION, SEE          *  08640000
.*       JOB MANAGEMENT, SUPERVISOR AND TSO.                         *  08650000
.*                                                                   *  08660000
.********************************************************************** 08670000
         GBLA  &RCPDYN            COUNTER FOR NO ENTRIES TO MACRO       08680000
         GBLA  &DTUO              OFFSET TO TEXT UNITS                  08690000
         GBLA  &DTUPO             OFFSET TO TEXT UNIT POINTERS          08700000
         GBLB  &RCPS99(2)         TELL RCPDSECT NEED DSECTS             08710000
         GBLC  &DYNP              PREFIX FOR LABELS FOR THIS CALL       08720000
         GBLC  &DYNSP         NAME FOR AUTOMATIC STORAGE ALLOC          08730000
         LCLA  &DDNRTO,&DSNRTO         FOR EQUATES FOR RETURNED FLDS    08740000
         LCLA  &VOLRTO,&DSRGRTO        FOR EQUATES FOR RETURNED FIELDS  08750000
         LCLA  &I                 COUNTER                               08760000
         LCLB  &DSECT             DSECT NEEDED FOR STORAGE, MF=E        08770000
         LCLC  &C,&T,&PAR                                               08780000
.*                                                                      08790000
.*   THE ALLOC MACRO PROVIDES A DYNAMIC ALLOCATION FUNCTION,            08800000
&RCPS99(1)     SETB           1                                         08810000
&RCPDYN  SETA  &RCPDYN+1          INCEREMENT COUNTER                    08820000
&DYNP    SETC  'DYN&RCPDYN' SET DEFAULT PREFIX                          08830000
&NAME    DS    0H                                                       08840000
         AIF   ('&PREFIX' EQ '').TMF                                    08850000
         AIF   (K'&PREFIX LT 4).POK                                     08860000
         MNOTE 4,'PREFIX TOO LONG, 1ST 4 CHARS USED'                    08870000
&DYNP    SETC  '&PREFIX'(1,4)                                           08880000
         AGO   .TMF                                                     08890000
.POK     ANOP                                                           08900000
&DYNP    SETC  '&PREFIX'                                                08910000
.TMF     AIF   ('&MF(1)' EQ 'G').GEN                                    08920000
         AIF   ('&MF' NE 'AUTO').TMFE                                   08930000
NAME     DYNSPACE             GET NAME FOR SPACE                        08940000
         LA    R1,&DYNSP               LOAD ADDRESS OF PARAM LIST       08950000
         USING &DYNP.DS,R1             USE GENERATED DSECT              08960000
&T       SETC  'A'                                                      08970000
&PAR     SETC  '&DYNSP+4'                                               08980000
&DSECT   SETB  1                                                        08990000
         AGO   .START                                                   09000000
.TMFE    AIF   ('&MF(2)' NE '').E2OK                                    09010000
         MNOTE 4,'PLIST ADDRESS OMITTED, MF=G USED'                     09020000
         AGO   .GEN                                                     09030000
.E2OK    ANOP                                                           09040000
&DSECT   SETB  1                                                        09050000
         AIF   ('&MF(2)' EQ '(').RMFE                                   09060000
         LA    R1,&MF(2)               LOAD PARAM LIST ADDRESS          09070000
         USING &DYNP.DS,R1             USE GENERATED DSECT              09080000
         AGO   .START                                                   09090000
.RMFE    AIF   ('&MF(2)' EQ '(R1)' OR '&MF(2)' EQ '(1)').START          09100000
         LR    R1,&PAR                 LOAD S99 PARAM LIST ADDRESS      09110000
         AGO   .START                                                   09120000
.GEN     LA    R1,&DYNP.RBP            LOAD ADDRESS OF S99 RBP          09130000
.START   LA    R15,&DYNP.RB            LOAD ADDRESS OF S99 RB           09140000
         USING S99RB,R15                                                09150000
         ST    R15,0(R1)               AND STORE IN RB POINTER          09160000
         XC    4(&DYNP.LEN-4,R1),4(R1) ZERO PARAMETER LIST              09170000
         MVI   S99RBLN,20              MOVE IN LIST LENGTH              09180000
         MVI   S99VERB,S99VRBAL        MOVE IN VERB CODE                09190000
         LA    R14,&DYNP.TUP           LOAD ADDRESS OF TU POINTERS      09200000
         ST    R14,S99TXTPP            STORE ADDRESS IN S99 RB          09210000
         LA    R15,&DYNP.TU            POINT TO SPACE FOR TEXT UNITS    09220000
         USING S99TUNIT,R15                                             09230000
&DTUO    SETA  0                                                        09240000
&DTUPO   SETA  0                                                        09250000
         AIF   ('&SSREQ' EQ 'YES').SSREQ                                09260000
.TDSN    AIF   ('&DSN&DA' NE '').DSN                                    09270000
         AIF   ('&DSNPDE' NE '').DSNPDE                                 09280000
         AIF   ('&DSNRET' NE '').DSNRT                                  09290000
         AIF   ('&SYSOUT' NE '').SYSOUT                                 09300000
         AIF   ('&DUMMY' NE '').DUMMY                                   09310000
         AIF   ('&QNAME' NE '').QNAME                                   09320000
.TDDN    AIF   ('&DDN&FILE&F' NE '').DDN                                09330000
         AIF   ('&DDNRET&DDNTO' NE '').DDNRT                            09340000
.TUNIT   AIF   ('&UNIT&VOL' NE '').UNIT                                 09350000
.TVOLRET AIF   ('&VOLRET' NE '').VOLRET                                 09360000
.TDSRGO  AIF   ('&DSORGRT' NE '').DSORGRT                               09370000
.TLABEL  AIF   ('&LABEL' NE '').LABEL                                   09380000
.TPSWD   AIF   ('&PASWORD' NE '').PASWORD                               09390000
.TFORUSE AIF   ('&FORUSER' NE '').FORUSER                               09400000
.TTU     AIF   ('&TU' NE '').TU                                         09410000
.TDISP   AIF   ('&DISP' NE '').DISP                                     09420000
.TSPACE  AIF   ('&SPACE' NE '').SPACE                                   09430000
.TLRECL  AIF   ('&LRECL' NE '').DCB                                     09440000
         AIF   ('&DEN' NE '').DCB                                       09450000
         AIF   ('&RECFM' NE '').DCB                                     09460000
         AIF   ('&BLKSIZE' NE '').DCB                                   09470000
         AIF   ('&DSORG' NE '').DCB                                     09480000
         AIF   ('&KEYLEN' NE '').DCB                                    09490000
.TDCBDSN AIF   ('&DCBDSN' NE '').DCBDSN                                 09500000
.TDCBDDN AIF   ('&DCBDDN' NE '').DCBDDN                                 09510000
.TFREE   AIF   ('&FREE' EQ 'CLOSE').FREE                         TE7343 09520000
.TPERM   AIF   ('&PERM' EQ 'PERM' OR '&PERM' EQ 'PERMANENT').PERM       09530000
         AIF   ('&DUMMY' EQ 'PERM' OR '&DUMMY' EQ 'PERMANENT').PERM     09540000
.TSPECI  AIF   ('&SPECIAL' NE '').SPECIAL                               09550000
         AGO   .SVC99                                                   09560000
.SSREQ   RCPSSREQ                                                       09570000
         AGO   .TDSN                                                    09580000
.DSN     RCPDSN &DSN&DA,&MEMBER                                         09590000
         AGO   .TDDN                                                    09600000
.DSNPDE  RCPDSNPD &DSNPDE                                               09610000
         AGO   .TDDN                                                    09620000
.DSNRT   RCPDSNRT &DSNRET                                               09630000
&DSNRTO  SETA  &DTUO-46                                                 09640000
         AGO   .TDDN                                                    09650000
.SYSOUT  RCPSYSOU &SYSOUT,COPIES=&COPIES,FREE=&FREE,DEST=&DEST,        X09660000
               FORMS=&FORMS                                             09670000
         AGO   .TDDN                                                    09680000
.DUMMY   RCPDUMMY &DUMMY                                                09690000
         AGO   .TDDN                                                    09700000
.QNAME   RCPQNAME &QNAME                                                09710000
         AGO   .TDDN                                                    09720000
.DDN     RCPDDN &DDN&F&FILE                                             09730000
         AGO   .TUNIT                                                   09740000
.DDNRT   RCPDDNRT &DDNRET                                               09750000
&DDNRTO  SETA  &DTUO-10                                                 09760000
         AGO   .TUNIT                                                   09770000
.UNIT   RCPUNIT &UNIT,&VOL                                              09780000
         AGO   .TVOLRET                                                 09790000
.VOLRET  RCPVOLRT &VOLRET                                               09800000
&VOLRTO  SETA  &DTUO-8                                                  09810000
         AGO   .TDSRGO                                                  09820000
.DSORGRT RCPDSRGR                                                       09830000
&DSRGRTO SETA  &DTUO-2                                                  09840000
         AGO   .TLABEL                                                  09850000
.LABEL   RCPLABEL &LABEL                                                09860000
         AGO   .TPSWD                                                   09870000
.PASWORD RCPPSWD &PASWORD                                               09880000
         AGO   .TFORUSE                                                 09890000
.FORUSER RCPFORUS &FORUSER                                              09900000
         AGO   .TTU                                                     09910000
.TU      RCPTU &TU                                                      09920000
         AGO   .TDISP                                                   09930000
.DISP    RCPDISP &DISP                                                  09940000
         AGO   .TSPACE                                                  09950000
.SPACE   RCPSPACE &SPACE                                                09960000
         AGO   .TLRECL                                                  09970000
.DCB     RCPDDCB LRECL=&LRECL,DEN=&DEN,RECFM=&RECFM,BLKSIZE=&BLKSIZE,  X09980000
               DSORG=&DSORG,KEYLEN=&KEYLEN                              09990000
         AGO .TDCBDSN                                                   10000000
.DCBDSN  RCPDCBDS &DCBDSN                                               10010000
         AGO .TDCBDDN                                                   10020000
.DCBDDN  RCPDCBDD &DCBDDN                                               10030000
         AGO .TFREE                                              TE7343 10040000
.FREE    RCPFREE  &FREE                                          TE7343 10050000
         AGO   .TPERM                                                   10060000
.PERM    RCPPERM                                                        10070000
         AGO   .TSPECI                                                  10080000
.SPECIAL RCPSPEC &SPECIAL                                               10090000
.SVC99   ANOP                                                           10100000
&DTUPO   SETA  &DTUPO-4                                                 10110000
         SPACE                                                          10120000
         MVI   &DYNP.TUP+&DTUPO,X'80'  SET HIGH ORDER BIT ON TEXT PTRS  10130000
         MVI   &DYNP.RBP,X'80'         SET HIGH ORDER BIT ON RB PTR     10140000
         RCPSR2 UNSAVE                                                  10150000
&DTUPO   SETA  &DTUPO+4                                                 10160000
         AIF   (NOT &DSECT).DYNA                                        10170000
         DROP  R1,R15                  DEACTIVATE ADDRESSABILITY        10180000
         LA    R14,4(R1)               POINT TO REQUEST BLOCK           10190000
.DYNA    DYNALLOC                                                       10200000
         AIF   (NOT &DSECT).LTR                                         10210000
         USING &DYNP.RB,R14            SET UP ADDRESSABILITY            10220000
**       NOTE  R14 HAS RB ADDRESS, R15 HAS SVC 99 RETURN CODE        ** 10230000
.LTR     AIF   ('&ERROR' EQ '').TDDTO                                   10240000
         LTR   R15,R15                 TEST RETURN CODE                 10250000
         BNZ   &ERROR                  BRANCH IF NON ZERO               10260000
.TDDTO   AIF   ('&DDNTO' EQ '').RESERVE                                 10270000
&I       SETA  0                                                        10280000
.DDNTOL  ANOP                                                           10290000
&I       SETA  &I+1                                                     10300000
         AIF   ('&DDNTO(&I)' EQ '').RESERVE                             10310000
         AIF   ('&DDNTO(&I)'(1,1) EQ '(').DDNTOR                        10320000
         MVC   &DDNTO(&I).(8),&DYNP.TU+&DDNRTO+2                        10330000
         AGO   .DDNTOL                                                  10340000
.DDNTOR  ANOP                                                           10350000
&C       SETC  '&DDNTO(&I)'(2,K'&DDNTO(&I)-2)                           10360000
         MVC   0(8,&C),&DYNP.TU+&DDNRTO+2                               10370000
         AGO   .DDNTOL                                                  10380000
.RESERVE AIF   (&DSECT).RESDS                                           10390000
         SPACE 1                                                        10400000
*********************************************************************** 10410000
**       RESERVE SPACE FOR DYNALLOC PARAMETER LIST                   ** 10420000
*********************************************************************** 10430000
         RCPDS                                                          10440000
.SSP     ANOP                                                           10450000
&DYNP.RBP DS   F                       SVC 99 REQ BLOCK POINTER         10460000
&DYNP.RB  DS   5F                      SVC 99 REQUEST BLOCK             10470000
&DYNP.TUP DS   CL&DTUPO                SPACE FOR TEXT POINTERS          10480000
         AIF   (&DTUO EQ 0).DTU21                                       10490000
&DYNP.TU  DS   CL&DTUO                 SPACE FOR TEXT UNITS             10500000
         AIF   (&DSNRTO EQ 0).TDDNRTO                                   10510000
&DSNRET  EQU   &DYNP.TU+&DSNRTO        OFFSET TO RETURNED DSN           10520000
.TDDNRTO AIF   ('&DDNRET' EQ '').DTU11                                  10530000
&DDNRET  EQU   &DYNP.TU+&DDNRTO        OFFSET TO RETURNED DDNAME        10540000
.DTU11   AIF   (&VOLRTO EQ 0).DTU12                                     10550000
&VOLRET  EQU   &DYNP.TU+&VOLRTO        OFFSET TO RETURNED VOLSER        10560000
.DTU12   AIF   (&DSRGRTO EQ 0).DTU10                                    10570000
&DSORGRT EQU   &DYNP.TU+&DSRGRTO       OFFSET TO RETURNED DSORG         10580000
         AGO   .DTU10                                                   10590000
.DTU21   ANOP                                                           10600000
&DYNP.TU  DS   0C                      NO SPACE NEEDED FOR TEXT UNITS   10610000
.DTU10   ANOP                                                           10620000
&DYNP.LEN EQU  *-&DYNP.RBP             LENGTH OF SPACE USED             10630000
         AIF   (&DSECT).DSP                                             10640000
         RCPDS                                                          10650000
         SPACE 3                                                        10660000
         AGO   .EXIT                                                    10670000
.RESDS   ANOP                                                           10680000
         AIF   ('&DYNSP' EQ '').SP3                                     10690000
         DYNSPACE ADD                                                   10700000
.SP3     SPACE                                                          10710000
&DYNP.DS DSECT                         DSECT TO MAP SVC 99 DATA         10720000
         AGO   .SSP                                                     10730000
.DSP     AIF   ('&MF(3)' EQ '').END1                                    10740000
&MF(3)   EQU   &DYNP.LEN               LENGTH OF AREA                   10750000
.END1    ANOP                                                           10760000
&SYSECT  CSECT                                                          10770000
         SPACE 3                                                        10780000
.EXIT    MEND                                                           10790000
./       ADD   NAME=CLEAR
         MACRO                                                          00020000
&NAME    CLEAR &FIELD,&CHAR,&LENGTH                                     00030000
         LCLC  &FILL,&L                                                 00040000
&L       SETC  'L'''                                                    00050000
&FILL    SETC  '&CHAR'                                                  00060000
         AIF   ('&CHAR' NE '').CHSPEC                                   00070000
&FILL    SETC  '40'                                                     00080000
.CHSPEC  ANOP                                                           00090000
&NAME    MVI   &FIELD,X'&FILL'   SET THE FIRST POSITION                 00100000
         AIF   ('&LENGTH' EQ '').NOLSPEC                                00110000
         MVC   &FIELD+1(&LENGTH),&FIELD  FILL THE ENTIRE FIELD          00120000
         MEXIT                                                          00130000
.NOLSPEC ANOP                                                           00140000
         MVC   &FIELD+1(&L&FIELD-1),&FIELD  FILL THE ENTIRE FIELD       00150000
         MEND                                                           00160000
./       ADD   NAME=CONV
         MACRO                                                          00180000
&LABEL   CONV  &TO,&FROM,&LEN,&EDMASK,&SCOMP                            00190000
         LCLC  &L,&FIRSTFR,&EDM,&COMP                                   00200000
         LCLA  &COUNT                                                   00210000
&L       SETC  'L'''                                                    00220000
         AIF   ('&LABEL' EQ '').NOLABEL  SKIP LABEL IF NOT PRESENT      00230000
&LABEL   DS    0H             SET THE LABEL                             00240000
.NOLABEL ANOP                                                           00250000
&EDM     SETC  'EDMASK'      DEFAULT EDIT MASK                          00260000
         AIF   ('&EDMASK' EQ '').DEFMASK  IF NOT ENTERED USE DEFAULT    00270000
&EDM     SETC  '&EDMASK'     USE THE ENTERED VALUE                      00280000
.DEFMASK ANOP                                                           00290000
&COMP    SETC  'BLANKS'      DEFAULT COMPARISON CHARS                   00300000
         AIF   ('&SCOMP' EQ '').DEFCOMP  NOT ENTERED, USE THE DEFAULT   00310000
&COMP    SETC  '&SCOMP'      GET WHAT THE GUY WANTS                     00320000
.DEFCOMP ANOP                                                           00330000
&FIRSTFR SETC  '&FROM'(1,1)   GET FIRST CHAR OF &FROM                   00340000
         AIF   ('&FIRSTFR' EQ '(').REGISTR                              00350000
         L     R1,&FROM       GET THE DATA TO CONVERT                   00360000
         CVD   R1,DOUBLE      CONVERT TO PACKED DECIMAL                 00370000
         AGO   .INDEC                                                   00380000
.REGISTR ANOP                                                           00390000
&COUNT   SETA  K'&FROM-2                                                00400000
&FIRSTFR SETC  '&FROM'(2,&COUNT)  STRIP THE PERRONS                     00410000
         CVD   &FIRSTFR,DOUBLE   CONVERT TO PACKED DECIMAL              00420000
.INDEC   ANOP                                                           00430000
         MVC   CHARS,&EDM     PUT IN THE EDIT MASK                      00440000
         ED    CHARS,DOUBLE   CONVERT TO CHARACTERS                     00450000
         AIF   ('&LEN' NE '').LENSET                                    00460000
         MVC   &TO,CHARS+16-&L&TO  MOVE IN THE NUMBER                   00470000
         CLC   CHARS(16-&L&TO),&COMP   WAS THERE AN OVERFLOW?           00480000
         BE    *+10           NO, EVERYTHING WAS OK                     00490000
         MVC   &TO,STARS      BAD NEWS, NOTE IT                         00500000
         MEXIT                                                          00510000
.LENSET  ANOP                                                           00520000
         MVC   &TO.(&LEN),CHARS+16-&LEN MOVE IN THE NUMBER              00530000
         CLC   CHARS(16-&LEN),&COMP   WAS THERE AN OVERFLOW?            00540000
         BE    *+10           NO, EVERYTHING WAS OK                     00550000
         MVC   &TO.(&LEN),STARS   BAD NEWS, NOTE IT                     00560000
         MEND                                                           00570000
./       ADD   NAME=DYNSPACE
         MACRO                                                          10810000
&NAME    DYNSPACE &TYPE                                                 10820000
.*                                                                      10830000
.*    THIS IS AN INNER MACRO TO ALLOC/FREE.                             10840000
.*    IT IS CALLED TO   A) NAME AN AREA FOR THE PARMLIST                10850000
.*                      B) LOG THE VARIOUS AMOUNTS NEEDED BY            10860000
.*                         EACH, REMEMBERING THE LARGEST.               10870000
.*                      C) GENERATING A DS FOR THE LARGEST AMOUNT.      10880000
.*    THE FIRST TWO FUNCTIONS ARE INVOKED BY ALLOC/FREE MACROS ONLY,    10890000
.*    AND THE THIRD IS USED BY THE PROGRAMMER, EITHER EXPLICITLY,       10900000
.*    OR BY BEGINWKA, IF THE LATTER IS USED.                            10910000
.*                                                                      10920000
.*     TO INVOKE THE NAMING FUNCTION, ALLOC/FREE GENERATE               10930000
.*     NAME DYNSPACE                                                    10940000
.*     NOTE. THE NAMING OPERATION ONLY GENERATES A NAME ON THE          10950000
.*     FIRST CALL IN THE ASSEMBLY. THE NAME REMAINS THE SAME UNTIL      10960000
.*     DYNSPACE IS CALLED TO EXPAND INTO A DS.                          10970000
.*                                                                      10980000
.*     THE SECOND FUNCTION IS INVOKED BY THE MACRO CALL                 10990000
.*          DYNSPACE ADD                                                11000000
.*     (NO NAME FIELD AND ONE OPERAND)                                  11010000
.*     IT USES THE GLOBAL VARIABLES &DTUO AND &DTUPO TO CALCULATE       11020000
.*     THE SPACE FOR THIS REQUEST, AND UPDATES &DYNSPQ ONLY IF THE      11030000
.*     CURRENT REQUEST IS FOR A GREATER AMOUNT                          11040000
.*                                                                      11050000
.*     THE THIRD FUNCTION IS INVOKED BY CALLING DYNSPACE WITH NO        11060000
.*     NAME OR OPERAND FIELD.                                           11070000
.*     THIS EXPANDS INTO A DEFINE STORAGE, CLEARS THE DYNSPACE NAME     11080000
.*     GLOBAL SETC, AND THE &DYNSPQ GLOBAL SETA.                        11090000
.*     THUS, THE MACRO IS SERIALLY REUSABLE IN ALL FUNCTIONS.           11100000
.*                                                                      11110000
         GBLA  &DYNSPQ,&DTUO,&DTUPO,&RCPDYN                             11120000
         GBLC  &DYNP,&DYNSP                                             11130000
         LCLA  &I                                                       11140000
         AIF   ('&NAME' NE '').NAME                                     11150000
         AIF   ('&TYPE' EQ '').ALLOC                                    11160000
.*   THE ACCUMULATE FUNCTION IS REQUIRED                                11170000
&I       SETA  24+&DTUO+&DTUPO         GET AMOUNT FOR THIS REQUEST      11180000
         AIF   (&I LE &DYNSPQ).EXIT    IF CURRENT < MAX, EXIT           11190000
&DYNSPQ  SETA  &I                      ELSE UPDATE CURRENT MAXIMUM      11200000
         MEXIT                                                          11210000
.NAME    AIF   ('&DYNSP' NE '').EXIT   IF NAME ALREADY EXISTS, EXIT     11220000
&DYNSP   SETC  'DYNSP&RCPDYN'           ELSE GENERATE A NAME            11230000
.EXIT    MEXIT                                                          11240000
.ALLOC   AIF   ('&DYNSP' EQ '').EXIT                                    11250000
*                                                                       11260000
**     RESERVE SPACE FOR ALLOC/FREE MACRO WORK AREA                     11270000
*                                                                       11280000
&DYNSP   DS    0F,CL&DYNSPQ            RESERVE SPACE                    11290000
&DYNSP   SETC  ''                      SET MAX QUANTITY TO 0            11300000
&DYNSPQ  SETA 0                                                         11310000
         MEND                                                           11320000
./       ADD   NAME=ENTER
         MACRO                                                          00590000
&SUBR    ENTER &BASES,&SAVE,&CSECT                                      00600000
.*   THIS MACRO, USED WITH THE LEAVE MACRO, WILL PERFORM                00610000
.*   STANDARD HOUSEKEEPING FOR A CSECT, INCLUDING SAVEAREA              00620000
.*   CONSTRUCTION AND CHAINING, AND GETTING SOME STORAGE,               00630000
.*   IF THAT IS DESIRED.                                                00640000
.*   THE LEAVE MACRO WILL FREE THE GOTTEN STORAGE                       00650000
.*   THE OPERANDS ARE                                                   00660000
.*       &SUBR    ENTER  &BASES,&SAVE,&CSECT                            00670000
.*    WHERE                                                             00680000
.*       &SUBR    IS THE NAME OF THE CSECT                              00690000
.*       &BASES   ARE THE BASE REGISTERS FOR THE ROUTINE                00700000
.*       &SAVE    IS THE LABEL FOR A SAVEAREA, OR A SUBPOOL             00710000
.*                AND LENGTH FOR THE GETMAIN                            00720000
.*       &CSECT   TO CONTINUE AN EXISTING CSECT WITH ENTRY              00730000
.*                POINT &SUBR                                           00740000
.*                                                                      00750000
.*    EXAMPLES -                                                        00760000
.*               ENTER 13,*                                             00770000
.*                                                                      00780000
.*       THIS WILL GENERATE NON-REENTRANT CODE, USING SAVEAREA          00790000
.*       AS THE SAVE AREA LABEL, AND REGISTER 13 FOR THE BASE           00800000
.*       REGISTER.                                                      00810000
.*                                                                      00820000
.*       RENTMOD  ENTER (12,11),(,LDSECT)                               00830000
.*                                                                      00840000
.*       THIS WILL GENERATE REENTRANT CODE WITH REGISTERS 12 AND        00850000
.*       11 FOR BASE REGISTERS.  A GETMAIN WILL BE DONE FOR THE         00860000
.*       DEFAULT SUBPOOL (0) WITH A LENGTH 'LDSECT'.                    00870000
.*                                                                      00880000
         GBLC  &LV,&SP                                                  00890000
         LCLA  &K,&N                                                    00900000
         LCLC  &AREA,&B(16),&SUBNAME,&S                                 00910000
&SUBNAME SETC  '&SUBR'                                                  00920000
         AIF   ('&SUBNAME' NE '').SUBSPEC                               00930000
&SUBNAME SETC  'MAIN'         DEFAULT CSECT NAME                        00940000
.SUBSPEC AIF   ('&CSECT' EQ '').NOTENT  IS IT AN ENTRY POINT?           00950000
&CSECT   CSECT                                                          00960000
&SUBNAME DS    0F                                                       00970000
         AGO   .CSSPEC                                                  00980000
.NOTENT  ANOP                                                           00990000
&SUBNAME CSECT                                                          01000000
.CSSPEC  ANOP                                                           01010000
         SAVE  (14,12),T,&SUBNAME   SAVE THE REGISTERS                  01020000
         AIF   ('&BASES(1)' EQ '15' OR '&BASES' EQ '').R15SET           01030000
         AIF   ('&BASES(1)' EQ '13' AND '&SAVE' NE '').R15SET           01040000
         LR    &BASES(1),15  SET FIRST BASE REG                         01050000
.R15SET  CNOP  0,4                                                      01060000
&S       SETC  '&SUBNAME'                                               01070000
         AIF   (N'&SAVE EQ 2).P4   SUBPOOL, SIZE SPEC?                  01080000
         AIF   ('&SAVE' EQ '').P3  NO SAVEAREA - DEFAULT                01090000
&AREA    SETC  '&SAVE'                                                  01100000
         AIF   ('&SAVE' NE '*').P2                                      01110000
&AREA    SETC  'SAVEAREA'                                               01120000
.P2      AIF   ('&BASES(1)' NE '13').P4                                 01130000
&S       SETC  '*'                                                      01140000
         USING &SUBNAME,15                                              01150000
         ST    14,&AREA+4                                               01160000
         LA    14,&AREA                                                 01170000
         ST    14,8(13)                                                 01180000
         L     14,&AREA+4                                               01190000
         ST    13,&AREA+4                                               01200000
         BAL   13,*+76        SKIP AROUND THE SAVEAREA                  01210000
         DROP  15                                                       01220000
         AGO   .P4                                                      01230000
.P3      AIF   ('&BASES(1)' NE '13').P4                                 01240000
         MNOTE 8,'*** CONTENTS OF REG 13 ARE LOST.  NO SAVE AREA WAS ESX01250000
               TABLISHED.'                                              01260000
.P4      AIF   ('&BASES(1)' NE '14' OR '&SAVE' EQ '').P5                01270000
         MNOTE 8,'*** MACRO RESTRICTION - REG 14 MUST NOT BE USED AS THX01280000
               E FIRST BASE REGISTER IF A SAVE AREA IS USED.'           01290000
.P5      AIF   ('&BASES' EQ '').P9                                      01300000
&N       SETA  N'&BASES                                                 01310000
.P6      ANOP                                                           01320000
&K       SETA  &K+1                                                     01330000
&B(&K)   SETC  ','.'&BASES(&K)'                                         01340000
         AIF   (N'&SAVE EQ 1).PE                                        01350000
         AIF   ('&BASES(&K)' NE '13').P7                                01360000
         MNOTE 8,'*** REG 13 MAY NOT BE USED AS A BASE REGISTER FOR REEX01370000
               NTRANT CODE.'                                            01380000
         AGO   .P7                                                      01390000
.PE      AIF   ('&BASES(&K+1)' NE '13' OR '&SAVE' EQ '').P7             01400000
         MNOTE 8,'*** WHEN USING A SAVE AREA, REG 13 MAY NOT BE USED ASX01410000
                A SECONDARY BASE REGISTER.'                             01420000
.P7      AIF   ('&BASES(&K+1)' NE '').P6                                01430000
         USING &S&B(1)&B(2)&B(3)&B(4)&B(5)&B(6)&B(7)&B(8)&B(9)&B(10)&B(X01440000
               11)&B(12)&B(13)&B(14)&B(15)&B(16)                        01450000
&K       SETA  1                                                        01460000
         AIF   ('&BASES(1)' NE '13' OR '&SAVE' EQ '').P8                01470000
&AREA    DC    18F'0'                                                   01480000
.P8      AIF   (&K GE &N).P10                                           01490000
         LA    &BASES(&K+1),X'FFF'(&BASES(&K))                          01500000
         LA    &BASES(&K+1),1(&BASES(&K+1))                             01510000
&K       SETA  &K+1                                                     01520000
         AGO   .P8                                                      01530000
.P9      USING &SUBNAME,15                                              01540000
.P10     AIF   (N'&SAVE GE 2).P13                                       01550000
         AIF   ('&SAVE' EQ '' OR '&BASES(1)' EQ '13').P12               01560000
         AIF   ('&SAVE' GE '0').P16  NUMERIC MEANS A PASSED AREA        01570000
         ST    14,&AREA+4                                               01580000
         LA    14,&AREA                                                 01590000
         ST    14,8(13)                                                 01600000
         L     14,&AREA+4                                               01610000
         ST    13,&AREA+4                                               01620000
.P11     BAL   13,*+76       SKIP AROUND THE SAVEAREA                   01630000
&AREA    DC    18F'0'                                                   01640000
.P12     MEXIT                                                          01650000
.P13     ANOP                                                           01660000
&LV      SETC  '&SAVE(2)'                                               01670000
&SP      SETC  '0'                                                      01680000
         AIF   ('&SAVE(1)' EQ '').P14                                   01690000
&SP      SETC  '&SAVE(1)'                                               01700000
.P14     CNOP  0,4          DO A GETMAIN FOR THE AREA                   01710000
         BAL   1,*+8          POINT THE SP AND LV                       01720000
ENT&SYSNDX DC  AL1(&SP)       SUBPOOL FOR THE GETMAIN                   01730000
         DC    AL3(&LV)       LENGTH OF THE GETMAIN                     01740000
         L     0,0(1)         GET THE DATA IN REG 1                     01750000
         SVC   10             ISSUE THE GETMAIN                         01760000
.*                            CHAIN THE SAVEAREAS                       01770000
         ST    13,4(1)        PRIOR SAVEAREA ADDRESS TO MINE            01780000
         ST    1,8(13)        MY SAVEAREA ADDRESS TO HIS                01790000
         LR    2,13           KEEP THE SAVEAREA ADDRESS FOR REGS        01800000
         LR    13,1           THIS IS MY SAVEAREA                       01810000
         LM    0,2,20(2)      RESTORE ORIGINAL REGS                     01820000
         MEXIT                                                          01830000
.P16     L     1,&AREA+0(1)   NUMERIC &SAVE IMPLIES A PASSED SAVEAREA   01840000
         ST    13,4(1)        PRIOR SAVEAREA ADDRESS TO MINE            01850000
         ST    1,8(13)        MY SAVEAREA ADDRESS TO HIS                01860000
         LR    2,13           KEEP THE SAVEAREA ADDRESS FOR REGS        01870000
         LR    13,1           THIS IS MY SAVEAREA                       01880000
         LM    0,2,20(2)      RESTORE ORIGINAL REGS                     01890000
         MEND                                                           01900000
./       ADD   NAME=ENTERX
         MACRO                                                          01920000
&SUBR    ENTERX &BASES,&SAVE,&CSECT                                     01930000
.*   THIS MACRO, USED WITH THE LEAVE MACRO, WILL PERFORM                01940000
.*   STANDARD HOUSEKEEPING FOR A CSECT, INCLUDING SAVEAREA              01950000
.*   CONSTRUCTION AND CHAINING, AND GETTING SOME STORAGE,               01960000
.*   IF THAT IS DESIRED.                                                01970000
.*   THE LEAVE MACRO WILL FREE THE GOTTEN STORAGE                       01980000
.*   THE OPERANDS ARE                                                   01990000
.*       &SUBR    ENTER  &BASES,&SAVE,&CSECT                            02000000
.*    WHERE                                                             02010000
.*       &SUBR    IS THE NAME OF THE CSECT                              02020000
.*       &BASES   ARE THE BASE REGISTERS FOR THE ROUTINE                02030000
.*       &SAVE    IS THE LABEL FOR A SAVEAREA, OR A SUBPOOL             02040000
.*                AND LENGTH FOR THE GETMAIN                            02050000
.*       &CSECT   TO CONTINUE AN EXISTING CSECT WITH ENTRY              02060000
.*                POINT &SUBR                                           02070000
.*                                                                      02080000
.*    EXAMPLES -                                                        02090000
.*               ENTER 13,*                                             02100000
.*                                                                      02110000
.*       THIS WILL GENERATE NON-REENTRANT CODE, USING SAVEAREA          02120000
.*       AS THE SAVE AREA LABEL, AND REGISTER 13 FOR THE BASE           02130000
.*       REGISTER.                                                      02140000
.*                                                                      02150000
.*       RENTMOD  ENTER (12,11),(,LDSECT)                               02160000
.*                                                                      02170000
.*       THIS WILL GENERATE REENTRANT CODE WITH REGISTERS 12 AND        02180000
.*       11 FOR BASE REGISTERS.  A GETMAIN WILL BE DONE FOR THE         02190000
.*       DEFAULT SUBPOOL (0) WITH A LENGTH 'LDSECT'.                    02200000
.*                                                                      02210000
         GBLC  &LV,&SP                                                  02220000
         LCLA  &K,&N                                                    02230000
         LCLC  &AREA,&B(16),&SUBNAME,&S                                 02240000
&SUBNAME SETC  '&SUBR'                                                  02250000
         AIF   ('&SUBNAME' NE '').SUBSPEC                               02260000
&SUBNAME SETC  'MAIN'         DEFAULT CSECT NAME                        02270000
.SUBSPEC AIF   ('&CSECT' EQ '').NOTENT  IS IT AN ENTRY POINT?           02280000
&CSECT   CSECT                                                          02290000
&SUBNAME DS    0F                                                       02300000
         AGO   .CSSPEC                                                  02310000
.NOTENT  ANOP                                                           02320000
&SUBNAME CSECT                                                          02330000
.CSSPEC  ANOP                                                           02340000
         SAVE  (14,12),T,&SUBNAME   SAVE THE REGISTERS                  02350000
         AIF   ('&BASES(1)' EQ '15' OR '&BASES' EQ '').R15SET           02360000
         AIF   ('&BASES(1)' EQ '13' AND '&SAVE' NE '').R15SET           02370000
         LR    &BASES(1),15  SET FIRST BASE REG                         02380000
.R15SET  CNOP  0,4                                                      02390000
&S       SETC  '&SUBNAME'                                               02400000
         AIF   (N'&SAVE EQ 2).P4   SUBPOOL, SIZE SPEC?                  02410000
         AIF   ('&SAVE' EQ '').P3  NO SAVEAREA - DEFAULT                02420000
&AREA    SETC  '&SAVE'                                                  02430000
         AIF   ('&SAVE' NE '*').P2                                      02440000
&AREA    SETC  'SAVEAREA'                                               02450000
.P2      AIF   ('&BASES(1)' NE '13').P4                                 02460000
&S       SETC  '*'                                                      02470000
         USING &SUBNAME,15                                              02480000
         ST    14,&AREA+4                                               02490000
         LA    14,&AREA                                                 02500000
         ST    14,8(13)                                                 02510000
         L     14,&AREA+4                                               02520000
         ST    13,&AREA+4                                               02530000
         BAL   13,*+76        SKIP AROUND THE SAVEAREA                  02540000
         DROP  15                                                       02550000
         AGO   .P4                                                      02560000
.P3      AIF   ('&BASES(1)' NE '13').P4                                 02570000
         MNOTE 8,'*** CONTENTS OF REG 13 ARE LOST.  NO SAVE AREA WAS ESX02580000
               TABLISHED.'                                              02590000
.P4      AIF   ('&BASES(1)' NE '14' OR '&SAVE' EQ '').P5                02600000
         MNOTE 8,'*** MACRO RESTRICTION - REG 14 MUST NOT BE USED AS THX02610000
               E FIRST BASE REGISTER IF A SAVE AREA IS USED.'           02620000
.P5      AIF   ('&BASES' EQ '').P9                                      02630000
&N       SETA  N'&BASES                                                 02640000
.P6      ANOP                                                           02650000
&K       SETA  &K+1                                                     02660000
&B(&K)   SETC  ','.'&BASES(&K)'                                         02670000
         AIF   (N'&SAVE EQ 1).PE                                        02680000
         AIF   ('&BASES(&K)' NE '13').P7                                02690000
         MNOTE 8,'*** REG 13 MAY NOT BE USED AS A BASE REGISTER FOR REEX02700000
               NTRANT CODE.'                                            02710000
         AGO   .P7                                                      02720000
.PE      AIF   ('&BASES(&K+1)' NE '13' OR '&SAVE' EQ '').P7             02730000
         MNOTE 8,'*** WHEN USING A SAVE AREA, REG 13 MAY NOT BE USED ASX02740000
                A SECONDARY BASE REGISTER.'                             02750000
.P7      AIF   ('&BASES(&K+1)' NE '').P6                                02760000
         USING &S&B(1)&B(2)&B(3)&B(4)&B(5)&B(6)&B(7)&B(8)&B(9)&B(10)&B(X02770000
               11)&B(12)&B(13)&B(14)&B(15)&B(16)                        02780000
&K       SETA  1                                                        02790000
         AIF   ('&BASES(1)' NE '13' OR '&SAVE' EQ '').P8                02800000
&AREA    DC    18F'0'                                                   02810000
.P8      AIF   (&K GE &N).P10                                           02820000
         LA    &BASES(&K+1),X'FFF'(&BASES(&K))                          02830000
         LA    &BASES(&K+1),1(&BASES(&K+1))                             02840000
&K       SETA  &K+1                                                     02850000
         AGO   .P8                                                      02860000
.P9      USING &SUBNAME,15                                              02870000
.P10     AIF   (N'&SAVE GE 2).P13                                       02880000
         AIF   ('&SAVE' EQ '' OR '&BASES(1)' EQ '13').P12               02890000
         AIF   ('&SAVE(1,1)' GE '0').P16  NUMERIC MEANS A PASSED AREA   02900000
         ST    14,&AREA+4                                               02910000
         LA    14,&AREA                                                 02920000
         ST    14,8(13)                                                 02930000
         L     14,&AREA+4                                               02940000
         ST    13,&AREA+4                                               02950000
.P11     BAL   13,*+76       SKIP AROUND THE SAVEAREA                   02960000
&AREA    DC    18F'0'                                                   02970000
.P12     MEXIT                                                          02980000
.P13     ANOP                                                           02990000
&LV      SETC  '&SAVE(2)'                                               03000000
&SP      SETC  '0'                                                      03010000
         AIF   ('&SAVE(1)' EQ '').P14                                   03020000
&SP      SETC  '&SAVE(1)'                                               03030000
.P14     CNOP  0,4          DO A GETMAIN FOR THE AREA                   03040000
         BAL   1,*+8          POINT THE SP AND LV                       03050000
ENT&SYSNDX DC  AL1(&SP)       SUBPOOL FOR THE GETMAIN                   03060000
         DC    AL3(&LV)       LENGTH OF THE GETMAIN                     03070000
         L     0,0(1)         GET THE DATA IN REG 1                     03080000
         SVC   10             ISSUE THE GETMAIN                         03090000
.*                            CHAIN THE SAVEAREAS                       03100000
         ST    13,4(1)        PRIOR SAVEAREA ADDRESS TO MINE            03110000
         ST    1,8(13)        MY SAVEAREA ADDRESS TO HIS                03120000
         LR    2,13           KEEP THE SAVEAREA ADDRESS FOR REGS        03130000
         LR    13,1           THIS IS MY SAVEAREA                       03140000
         LA    4,12(13)       YES, POINT PAST THE CHAIN                 03150000
         L     5,ENT&SYSNDX   GET THE SIZE                              03160000
         LA    6,12           MINUS THE CHAIN AREA (12 BYTES )          03170000
         SR    5,6            GIVES THE AMOUNT TO CLEAR                 03180000
         SR    7,7            CLEAR THE FROM COUNT AND CLEAR BYTE       03190000
         MVCL  4,6            WHEE, CLEAR IT OUT                        03200000
         LM    0,7,20(2)      RESTORE THE ORIGINAL REGISTERS            03210000
         MEXIT                                                          03220000
.P16     L     1,&AREA+0(1)   NUMERIC &SAVE IMPLIES A PASSED SAVEAREA   03230000
         ST    13,4(1)        PRIOR SAVEAREA ADDRESS TO MINE            03240000
         ST    1,8(13)        MY SAVEAREA ADDRESS TO HIS                03250000
         LR    2,13           KEEP THE SAVEAREA ADDRESS FOR REGS        03260000
         LR    13,1           THIS IS MY SAVEAREA                       03270000
         LM    0,2,20(2)      RESTORE ORIGINAL REGS                     03280000
         MEND                                                           03290000
./       ADD   NAME=FREE
         MACRO                                                          11340000
&NAME    FREE  &UNALC,&DSN=,&DDN=,&MEMBER=,&DISP=,&SYSOUT=,            X11350000
               &ERROR=,&MF=AUTO,&PREFIX=,&FILE=,&F=,&DA=,&HOLD=         11360000
         GBLA  &RCPDYN            COUNTER FOR NO ENTRIES TO MACRO       11370000
         GBLA  &DTUO              OFFSET TO TEXT UNITS                  11380000
         GBLA  &DTUPO             OFFSET TO TEXT UNIT POINTERS          11390000
         GBLB  &RCPS99(2)         TELL RCPDSECT NEED DSECTS             11400000
         GBLC  &DYNP              PREFIX FOR LABELS FOR THIS CALL       11410000
         GBLC  &DYNSP         NAME FOR AUTOMATIC STORAGE ALLOC          11420000
         LCLB  &DSECT             DSECT NEEDED FOR STORAGE, MF=E        11430000
         LCLC  &C,&T,&PAR                                               11440000
&RCPS99(1)     SETB           1                                         11450000
&RCPDYN  SETA  &RCPDYN+1          INCEREMENT COUNTER                    11460000
&DYNP    SETC  'DYN&RCPDYN' SET DEFAULT PREFIX                          11470000
&NAME    DS    0H                                                       11480000
         AIF   ('&PREFIX' EQ '').TMF                                    11490000
         AIF   (K'&PREFIX LT 4).POK                                     11500000
         MNOTE 4,'PREFIX TOO LONG, 1ST 4 CHARS USED'                    11510000
&DYNP    SETC  '&PREFIX'(1,4)                                           11520000
         AGO   .TMF                                                     11530000
.POK     ANOP                                                           11540000
&DYNP    SETC  '&PREFIX'                                                11550000
.TMF     AIF   ('&MF(1)' EQ 'G').GEN                                    11560000
         AIF   ('&MF' NE 'AUTO').TMFE                                   11570000
NAME     DYNSPACE             GET NAME FOR SPACE                        11580000
         LA    R1,&DYNSP               LOAD ADDRESS OF PARAM LIST       11590000
         USING &DYNP.DS,R1             USE GENERATED DSECT              11600000
&T       SETC  'A'                                                      11610000
&PAR     SETC  '&DYNSP+4'                                               11620000
&DSECT   SETB  1                                                        11630000
         AGO   .START                                                   11640000
.TMFE    AIF   ('&MF(2)' NE '').E2OK                                    11650000
         MNOTE 4,'PLIST ADDRESS OMITTED, MF=G USED'                     11660000
         AGO   .GEN                                                     11670000
.E2OK    ANOP                                                           11680000
&DSECT   SETB  1                                                        11690000
         AIF   ('&MF(2)' EQ '(').RMFE                                   11700000
         LA    R1,&MF(2)               LOAD PARAM LIST ADDRESS          11710000
&T       SETC  'A'                                                      11720000
&PAR     SETC  '&MF(2)+4'                                               11730000
         USING &DYNP.DS,R1             USE GENERATED DSECT              11740000
         AGO   .START                                                   11750000
.RMFE    AIF   ('&MF(2)' EQ '(R1)' OR '&MF(2)' EQ '(1)').START          11760000
&PAR     SETC  '&MF(2)'(2,K'&MF(2)-2)                                   11770000
&T       SETC  'R'                                                      11780000
         LR    R1,&PAR                 LOAD S99 PARAM LIST ADDRESS      11790000
&PAR     SETC  '4&MF(2)'                                                11800000
         USING &DYNP.DS,R1             USE GENERATED DSECT              11810000
         AGO   .START                                                   11820000
.GEN     LA    R1,&DYNP.RBP            LOAD ADDRESS OF S99 RBP          11830000
&T       SETC  'A'                                                      11840000
&PAR     SETC  '&DYNP.RB'                                               11850000
.START   LA    R15,&DYNP.RB            LOAD ADDRESS OF S99 RB           11860000
         USING S99RB,R15                                                11870000
         ST    R15,0(R1)               AND STORE IN RB POINTER          11880000
         XC    4(&DYNP.LEN-4,R1),4(R1) ZERO PARAMETER LIST              11890000
         MVI   S99RBLN,20              MOVE IN LIST LENGTH              11900000
         MVI   S99VERB,S99VRBUN        MOVE IN VERB CODE                11910000
         LA    R14,&DYNP.TUP           LOAD ADDRESS OF TU POINTERS      11920000
         ST    R14,S99TXTPP            STORE ADDRESS IN S99 RB          11930000
         LA    R15,&DYNP.TU            POINT TO SPACE FOR TEXT UNITS    11940000
         USING S99TUNIT,R15                                             11950000
&DTUO    SETA  0                                                        11960000
&DTUPO   SETA  0                                                        11970000
         AIF   ('&DSN&DA' NE '').DSN                                    11980000
         AIF   ('&SYSOUT' NE '').SYSOUT                                 11990000
.TDDN    AIF   ('&DDN&FILE&F' NE '').DDN                                12000000
.TDISP   AIF   ('&DISP' NE '').DISP                                     12010000
.TUNALC  AIF   ('&UNALC' NE '').PERM                                    12020000
.THOLD   AIF   ('&HOLD' NE '').HOLD                                     12030000
         AGO   .SVC99                                                   12040000
.DSN     RCPFDSN &DSN&DA,&MEMBER                                        12050000
         AGO   .TDDN                                                    12060000
.SYSOUT  RCPFSYS &SYSOUT                                                12070000
         AGO   .TDDN                                                    12080000
.DDN     RCPFDDN &DDN&F&FILE                                            12090000
         AGO   .TDISP                                                   12100000
.DISP RCPFDISP &DISP                                                    12110000
         AGO   .TUNALC                                                  12120000
.PERM    RCPUNALC                                                       12130000
         AGO   .THOLD                                                   12140000
.HOLD    RCPFHOLD &HOLD                                                 12150000
.SVC99   ANOP                                                           12160000
&DTUPO   SETA  &DTUPO-4                                                 12170000
         SPACE                                                          12180000
         MVI   &DYNP.TUP+&DTUPO,X'80'  SET HIGH ORDER BIT ON TEXT PTRS  12190000
         MVI   &DYNP.RBP,X'80'         SET HIGH ORDER BIT ON RB PTR     12200000
         RCPSR2 UNSAVE                                                  12210000
&DTUPO   SETA  &DTUPO+4                                                 12220000
         AIF   (NOT &DSECT).DYNA                                        12230000
         DROP  R1,R15                  DEACTIVATE ADDRESSABILITY        12240000
.DYNA    DYNALLOC                                                       12250000
         AIF   ('&ERROR' EQ '').RESERVE                                 12260000
         AIF   ('&PAR' EQ '').LTR                                       12270000
         L&T   R14,&PAR                 LOAD REG 14 WITH ADDRESS OF RB  12280000
         AIF   (NOT &DSECT).LTR                                         12290000
         USING &DYNP.RB,R14            SET UP ADDRESSABILITY            12300000
.LTR     LTR   R15,R15                 TEST RETURN CODE                 12310000
         BNZ   &ERROR                  BRANCH IF NON ZERO               12320000
**       NOTE.  R14 POINTS TO REQUEST BLOCK, R15 HAS RETURN CODE     ** 12330000
.RESERVE AIF   (&DSECT).RESDS                                           12340000
         SPACE                                                          12350000
*********************************************************************** 12360000
**       RESERVE SPACE FOR DYNALLOC DATA                             ** 12370000
*********************************************************************** 12380000
         RCPDS                                                          12390000
.SSP     ANOP                                                           12400000
&DYNP.RBP DS   F                       SVC 99 REQ BLOCK POINTER         12410000
&DYNP.RB  DS   5F                      SVC 99 REQUEST BLOCK             12420000
&DYNP.TUP DS   CL&DTUPO                SPACE FOR TEXT POINTERS          12430000
         AIF   (&DTUO EQ 0).DTU11                                       12440000
&DYNP.TU  DS   CL&DTUO                 SPACE FOR TEXT UNITS             12450000
         AGO   .DTU10                                                   12460000
.DTU11   ANOP                                                           12470000
&DYNP.TU  DS   0C                      NO SPACE NEEDED FOR TEXT UNITS   12480000
.DTU10   ANOP                                                           12490000
&DYNP.LEN EQU  *-&DYNP.RBP             LENGTH OF SPACE USED             12500000
         AIF   (&DSECT).DSP                                             12510000
         RCPDS                                                          12520000
         SPACE 3                                                        12530000
         AGO   .EXIT                                                    12540000
.RESDS   ANOP                                                           12550000
         AIF   ('&DYNSP' EQ '').SP3                                     12560000
         DYNSPACE ADD                                                   12570000
.SP3     SPACE                                                          12580000
&DYNP.DS DSECT                         DSECT TO MAP SVC 99 DATA         12590000
         AGO   .SSP                                                     12600000
.DSP     AIF   ('&MF(3)' EQ '').END1                                    12610000
&MF(3)   EQU   &DYNP.LEN               LENGTH OF AREA                   12620000
.END1    ANOP                                                           12630000
&SYSECT  CSECT                                                          12640000
         SPACE 3                                                        12650000
.EXIT    MEND                                                           12660000
./       ADD   NAME=LEAVE
         MACRO                                                          03310000
&NAME    LEAVE &EQ,&RC=                                                 03320000
         GBLC  &LV,&SP                                                  03330000
&NAME    LR    2,13                                                     03340000
         L     13,4(13)                                                 03350000
         AIF   ('&RC' EQ '').L0                                         03360000
         LA    15,&RC         LOAD THE RETURN CODE                      03370000
.L0      STM   15,1,16(13)  STORE RETURN REGS                           03380000
         AIF   ('&LV' EQ '').L1  ANYTHING TO FREE?                      03390000
         FREEMAIN R,LV=&LV,SP=&SP,A=(2)  FREE THE AREA                  03400000
.L1      RETURN (14,12),T     RETURN FROM WHENCE WE CAME                03410000
         AIF   ('&EQ' NE 'EQ').L4  REGISTERS TOO?                       03420000
         COPY  REGS                                                     03430000
.L4      MEND                                                           03440000
./       ADD   NAME=MSG
         MACRO                                                          03460000
&NAME    MSG   &TEXT                                                    03470000
         LCLA  &A                                                       03480000
&A       SETA  K'&TEXT-2+4  SUBTRACT QUOTES, ADD PREFIX FOUR BYTES      03490000
&NAME    DC    H'&A',H'0',C&TEXT                                        03500000
         MEND                                                           03510000
./       ADD   NAME=PDEDSNAM
         MACRO                                                          03530000
         PDEDSNAM                                                       03540000
*                                                                       03550000
*        DEFINE A DSECT FOR THE DSNAME PARSE DESCRIPTION                03560000
*                                                                       03570000
PDEDSNAM DSECT                                                          03580000
PDEDSN   DS    A              POINTER TO DSNAME                         03590000
PDEDSNL  DS    H              LENGTH OF DSNAME                          03600000
PDEDFLG1 DS    X              DATA SET NAME FLAGS                       03610000
PDEDFLD1 EQU   X'80'          ONE IF THE DSNAME IS PRESENT              03620000
PDEDFLQ1 EQU   X'40'          ONE IF THE DSNAME IS WITHIN QUOTES        03630000
PDEDMEM  DS    A              POINTER TO MEMBER NAME                    03640000
PDEDMEML DS    H              LENGTH OF MEMBER NAME                     03650000
PDEDFLG2 DS    X              MEMBER   NAME FLAGS                       03660000
PDEDFLD2 EQU   X'80'          ONE IF THE MEMBER IS PRESENT              03670000
PDEDPASS DS    A              POINTER TO PASSWORD                       03680000
PDEDPASL DS    H              LENGTH OF PASSWORD                        03690000
PDEDFLG3 DS    X              PASSWORD      FLAGS                       03700000
PDEDFLD3 EQU   X'80'          ONE IF THE PASSWORD IS PRESENT            03710000
PDEDCHAN DS    0F             CHAIN ADDRESS                             03720000
PDEDCHNF DS    X              CHAIN FLAGS ( X'FF' FOR END )             03730000
PDEDCHN  DS    AL3            TRUE CHAIN POINTER                        03740000
         MEND                                                           03750000
./       ADD   NAME=RCPBFRGS
         MACRO                                                          12680000
         RCPBFRGS &BUFPTR,&WKREGS                                       12690000
         GBLC  &RCPBFRP,&RCPBFR1,&RCPBFR2                               12700000
         AIF   ('&BUFPTR' EQ '').TGP                                    12710000
&RCPBFRP SETC  '&BUFPTR'                                                12720000
         AGO   .TWK1                                                    12730000
.TGP     AIF   ('&RCPBFRP' NE '').TWK1                                  12740000
&RCPBFRP SETC  'R1'                                                     12750000
.TWK1    AIF   ('&WKREGS(1)' EQ '').TG1                                 12760000
&RCPBFR1 SETC  '&WKREGS(1)'                                             12770000
         AGO   .TWK2                                                    12780000
.TG1     AIF   ('&RCPBFR1' NE '').TWK2                                  12790000
&RCPBFR1 SETC  'R14'                                                    12800000
.TWK2    AIF   ('&WKREGS(2)' EQ '').TG2                                 12810000
&RCPBFR2 SETC  '&WKREGS(2)'                                             12820000
         MEXIT                                                          12830000
.TG2     AIF   ('&RCPBFR2' NE '').EXIT                                  12840000
&RCPBFR2 SETC  'R15'                                                    12850000
.EXIT    MEND                                                           12860000
./       ADD   NAME=RCPBTU
         MACRO                                                          16130000
         RCPBTU &KEY,&NUM,&PAR                                          16140000
         LCLA  &L                                                       16150000
.*                                                                      16160000
.*  INNER MACRO FOR ALLOC, TO GENERATE TEXT UNITS ENTERED               16170000
.*  IN QUOTES                                                           16180000
.*                                                                      16190000
&L       SETA  K'&PAR-2                GET LENGTH OF TEXT UNIT          16200000
         MVI   S99TUKEY+1,&KEY         SET TEXT UNIT KEY                16210000
         MVI   S99TUNUM+1,&NUM         SET NUMBER FIELD                 16220000
         MVI   S99TULNG+1,&L           MOVE IN LENGTH                   16230000
         MVC   S99TUPAR(&L.),=C&PAR    MOVE IN TEXT UNIT                16240000
&L       SETA  &L+6                                                     16250000
         AIF   (&L/2 EQ (&L+1)/2).LOK                                   16260000
&L       SETA  &L+1                                                     16270000
.LOK     RCPDINC &L                                                     16280000
         MEND                                                           16290000
./       ADD   NAME=RCPBTU2
         MACRO                                                          19880000
         RCPBTU &KEY,&NUM,&PAR                                          19890000
         GBLA  &DTUPO                                                   19900000
         GBLC  &DYNP                                                    19910000
         LCLA  &L                                                       19920000
.*                                                                      19930000
.*  INNER MACRO FOR ALLOC, TO BRANCH AROUND TEXT UNIT AND               19940000
.*  CREATE TEXT UNIT                                                    19950000
.*                                                                      19960000
&L       SETA  K'&PAR+8                GET LENGTH TO BRANCH AROUND      19970000
         AIF   (&L/2 EQ (&L+1)/2).LOK  MAKE SURE LENGTH IS EVEN         19980000
&L       SETA  &L+1                                                     19990000
.LOK     BAL   R14,*+&L                BRANCH AROUND TEXT UNIT          20000000
&L       SETA  K'&PAR-2                                                 20010000
         DC    Y(&KEY,&NUM,&L),C&PAR   TEXT UNIT                        20020000
         LA    R14,0(R14)              CLEAR HIGH ORDER BYTE            20030000
         ST    R14,&DYNP.TUP+&DTUPO    STORE TEXT UNIT ADDRESS          20040000
&DTUPO   SETA  &DTUPO+4                                                 20050000
         MEND                                                           20060000
./       ADD   NAME=RCPCKID
         MACRO                                                          23720000
&NAME    RCPCKID              &CHECKID                                  23730000
         GBLB  &RCPECT(2),&RCPPSCB(2)                                   23740000
         GBLC  &RCPPRE                                                  23750000
         LCLC  &CHARVAR,&P                                              23760000
         LCLA  &COUNTR,&L                                               23770000
&P       SETC  '&RCPPRE'                                                23780000
&RCPPSCB(1) SETB  1                                                     23790000
&RCPECT(1)  SETB  1                                                     23800000
         EJECT                                                          23810000
         SPACE 4                                                        23820000
*********************************************************************** 23830000
***  THE USERID OF THE USER IS CHECKED. IF IT IS NOT VALID, THE    **** 23840000
***   COMMAND PRETENDS IT DOES NOT EXIST BY LINKING TO EXEC IN     **** 23850000
***   THE SAME WAY THE TMP DOES IF IT CANNOT FIND THE COMMAND.     **** 23860000
*********************************************************************** 23870000
         SPACE 3                                                        23880000
         L     R1,CPPLPSCB             LOAD ADDRESS OF PSCB             23890000
         USING PSCB,R1                 PSCB ADDRESSABILITY              23900000
.NID     ANOP                                                           23910000
&COUNTR  SETA  &COUNTR+1                                                23920000
         AIF   ('&CHECKID(&COUNTR)' EQ '').ENDID                        23930000
&CHARVAR SETC  '&CHECKID(&COUNTR)'                                      23940000
&L       SETA  K'&CHARVAR                                               23950000
         AIF   ('&CHARVAR'(1,1) EQ '''').QCID                           23960000
         CLC   PSCBUSER(&L),=C'&CHARVAR'  IS THE USERID VALID?          23970000
         BE    &P.IDOK                     YES, BRANCH OUT              23980000
         AGO   .NID                                                     23990000
.QCID    ANOP                                                           24000000
&L       SETA  &L-2                                                     24010000
         CLC   PSCBUSER(&L),=C&CHARVAR    IS THE USERID VALID?          24020000
         BE    &P.IDOK                     YES, BRANCH OUT              24030000
         AGO   .NID                                                     24040000
.ENDID   L     R1,CPPLECT              LOAD ECT ADDRESS                 24050000
         SPACE 2                                                        24060000
         USING ECT,R1                                                   24070000
         MVC   ECTPCMD,&P.EXECN        MOVE IN COMMAND NAME             24080000
         DROP  R1                      KILL ECT ADDRESSABILITY          24090000
         L     R1,CPPLCBUF             LOAD CBUF ADDRESS                24100000
         XC    2(2,R1),2(R1)           ZERO OFFSET FIELD                24110000
         L     R1,&P.CPPL              RELOAD CPPL ADDRESS              24120000
         XCTL  EPLOC=&P.EXECN                                           24130000
&P.EXECN DC    CL8'EXEC'               NAME OF EXEC PROCESSOR           24140000
&P.IDOK  DS    0H                                                       24150000
         MEND                                                           24160000
./       ADD   NAME=RCPDDN
         MACRO                                                          27850000
         RCPDDN &DDN                                                    27860000
         GBLC  &DYNP                                                    27870000
         SPACE 1                                                        27880000
*********************************************************************** 27890000
**   BUILD THE DDNAME TEXT UNIT                                      ** 27900000
*********************************************************************** 27910000
         AIF   ('&DDN'(K'&DDN,1) EQ '/').BTU                            27920000
         AIF   ('&DDN'(1,1) EQ '''').Q                                  27930000
         RCPSR2                                                         27940000
         AIF   ('&DDN'(1,1) EQ '(').R                                   27950000
         L     R14,&DDN                LOAD ADDRESS OF DDNAME           27960000
         LH    R2,&DDN+4               LOAD LENGTH OF DDNAME            27970000
         AGO   .STH                                                     27980000
.R       L     R14,0&DDN               LOAD ADDRESS OF DDNAME           27990000
         LH    R2,4&DDN                LOAD LENGTH OF DDNAME            28000000
.STH     STH   R2,S99TULNG             STORE DDNAME LENGTH              28010000
         BCTR  R2,0                    DECREMENT FOR EXECUTE            28020000
         EX    R2,&DYNP.MVC            MOVE DDNAME                      28030000
         MVI   S99TUKEY+1,DALDDNAM     MOVE IN DDNAME KEY               28040000
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 28050000
         RCPDINC 14                                                     28060000
         MEXIT                                                          28070000
.Q       RCPBTU DALDDNAM,1,&DDN                                         28080000
         MEXIT                                                          28090000
.BTU     RCPTUBFR DALDDNAM,14,&DDN                                      28100000
         MEND                                                           28110000
./       ADD   NAME=RCPDDNRT
         MACRO                                                          29910000
         RCPDDNRT                                                       29920000
         SPACE 1                                                        29930000
*********************************************************************** 29940000
**    DDNAME RETURN TEXT UNIT                                        ** 29950000
*********************************************************************** 29960000
         MVI   S99TUKEY+1,DALRTDDN     SET RETURN DDNAME KEY            29970000
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 29980000
         MVI   S99TULNG+1,8            SET LENGTH FIELD                 29990000
         MVC   S99TUPAR(8),=CL8' '     INITIALIZE FIELD TO BLANKS       30000000
         RCPDINC 14                                                     30010000
         MEND                                                           30020000
./       ADD   NAME=RCPDEBUG
         MACRO                                                          12880000
         RCPDEBUG &ON                                                   12890000
         GBLA  &RCPBGN#,&RCPSWS(10)                                     12900000
         GBLB  &RCPDBUG                                                 12910000
         GBLC  &RCPPRE,&RCPWKDS,&RCPWKCS                                12920000
         AIF   ('&ON' EQ '').TSW                                        12930000
&RCPDBUG SETB 1                                                         12940000
.TSW     AIF   (&RCPDBUG).DEBUG                                         12950000
         MEXIT                                                          12960000
.DEBUG   MNOTE *,'RCPBGN# IS &RCPBGN#'                                  12970000
         MNOTE *,'RCPSWS(1) IS &RCPSWS(1)'                              12980000
         MNOTE *,'RCPSWS(2) IS &RCPSWS(2)'                              12990000
         MNOTE *,'RCPSWS(3) IS &RCPSWS(3)'                              13000000
         MNOTE *,'RCPSWS(4) IS &RCPSWS(4)'                              13010000
         MNOTE *,'RCPSWS(5) IS &RCPSWS(5)'                              13020000
         MNOTE *,'RCPWKCS IS ''&RCPWKCS'''                              13030000
         MNOTE *,'RCPWKDS IS ''&RCPWKDS'''                              13040000
         MNOTE *,'RCPPRE IS ''&RCPPRE'''                                13050000
         MEND                                                           13060000
./       ADD   NAME=RCPDFPL
         MACRO                                                          16310000
         RCPDFPL                                                        16320000
         GBLC  &RCPPRE                                                  16330000
         GBLB  &RCPDFPL(2)                                              16340000
         GBLB  &RCPDFPB(2)                                              16350000
         LCLC  &P,&L,&L1                                                16360000
&P       SETC  '&RCPPRE'                                                16370000
         EJECT                                                          16380000
         AIF   (&RCPDFPL(2)).BYPDFPL                                    16390000
&RCPDFPL(2) SETB 1                                                      16400000
         IKJDFPL                                                        16410000
L#DFPL   EQU   *-DFPL                  LENGTH OF DEFAULT PARAM LIST     16420000
         IKJDFPB                                                        16430000
L#DFPB   EQU   *-DFPB                  LENGTH OF DEFAULT PARAM BLOCK    16440000
&SYSECT  CSECT                         RESUME PROGRAM CSECT             16450000
         SPACE 3                                                        16460000
.BYPDFPL RCPDS                                                          16470000
&P.DFPL  DS    CL(L#DFPL)              RESERVE SPACE FOR DFPL           16480000
&P.DFPB  DS    CL(L#DFPB)              RESERVE SPACE FOR DFPB           16490000
&P.DSNB  DS    CL48                    RESERVE SPACE FOR DSNAME BUFFER  16500000
         RCPDS                                                          16510000
         EJECT                                                          16520000
*********************************************************************** 16530000
***   THIS CODE GENERATES AN DEFAULT SERVICE ROUTINE PARAMETER LIST *** 16540000
***       AND PARAMETER BLOCK                                       *** 16550000
*********************************************************************** 16560000
         LA    R1,&P.DFPL              LOAD DFPL ADDRESS                16570000
         USING DFPL,R1                 DFPL ADDRESSABLE                 16580000
         MVC   DFPLUPT,CPPLUPT         MOVE IN ADDRESS OF UPT           16590000
         MVC   DFPLECT,CPPLECT         MOVE IN ADDRESS OF ECT           16600000
         LA    R15,&P.ECB              LOAD ADDRESS OF ATTN ECB         16610000
         ST    R15,DFPLECB             AND STORE IN DFPL                16620000
         LA    R15,&P.DFPB             LOAD DFBP ADDRESS                16630000
         ST    R15,DFPLDFPB             AND STORE IT IN DFPB            16640000
         DROP  R1                                                       16650000
         USING DFPB,R15                ADDRESS DFPB DSECT               16660000
         XC    DFPB(L#DFPB),DFPB       CLEAR DEFAULT PARAMETER BLOCK    16670000
         MVC   DFPBPSCB,CPPLPSCB       MOVE IN ADDRESS OF PSCB          16680000
         LA    R1,&P.DSNB              LOAD DSNAME BUFFER ADDRESS       16690000
         ST    R1,DFPBDSN               AND STORE IT INTO DFPB          16700000
         MVI   DFPBCODE,DFPB04          SET ENTRY CODE                  16710000
         DROP  R15                     DFPB NO LONGER ADDRESSABLE       16720000
         EJECT                                                          16730000
         MEND                                                           16740000
./       ADD   NAME=RCPDINC
         MACRO                                                          20080000
         RCPDINC &L1                                                    20090000
         GBLA  &DTUO,&DTUPO                                             20100000
         GBLC  &DYNP                                                    20110000
         AIF   ('&L1' EQ '').T2                                         20120000
         ST    R15,&DYNP.TUP+&DTUPO    STORE TEXT UNIT ADDRESS          20130000
         LA    R15,&L1.(R15)           BUMP TEXT UNIT PTR TO NEXT SLOT  20140000
&DTUPO   SETA  &DTUPO+4                                                 20150000
&DTUO    SETA  &DTUO+&L1                                                20160000
         MEXIT                                                          20170000
.T2      ST    R14,&DYNP.TUP+&DTUPO    STORE TEXT UNIT ADDRESS          20180000
&DTUPO   SETA  &DTUPO+4                                                 20190000
         MEND                                                           20200000
./       ADD   NAME=RCPDISP
         MACRO                                                          24180000
         RCPDISP &DISP                                                  24190000
         LCLA  &I                                                       24200000
         LCLB  &B(4)                                                    24210000
         AIF   ('&DISP(1)' EQ '').TD2                                   24220000
         SPACE                                                          24230000
*********************************************************************** 24240000
**     DATA SET INITIAL STATUS                                       ** 24250000
*********************************************************************** 24260000
&B(1)    SETB  ('&DISP(1)' EQ 'SHR')                                    24270000
&B(2)    SETB  ('&DISP(1)' EQ 'NEW')                                    24280000
&B(3)    SETB  ('&DISP(1)' EQ 'MOD')                                    24290000
&B(4)    SETB  ('&DISP(1)' EQ 'OLD')                                    24300000
         AIF   (&B(1) OR &B(2) OR &B(3) OR &B(4)).OK1                   24310000
         MNOTE 8,'&DISP(1) IS INVALID, DISP=SHR USED'                   24320000
&B(1)    SETB  1                                                        24330000
.OK1     ANOP                                                           24340000
&I       SETA  8*&B(1)+4*&B(2)+2*&B(3)+&B(4)                            24350000
         MVC   S99TUKEY(8),=Y(DALSTATS,1,1,X'0&I.00')                   24360000
         RCPDINC 8                                                      24370000
.TD2     AIF   ('&DISP(2)' EQ '').TD3                                   24380000
         SPACE                                                          24390000
*********************************************************************** 24400000
**    DATA SET NORMAL DISPOSITION                                    ** 24410000
*********************************************************************** 24420000
&B(1)    SETB  ('&DISP(2)' EQ 'KEEP')                                   24430000
&B(2)    SETB  ('&DISP(2)' EQ 'DELETE')                                 24440000
&B(3)    SETB  ('&DISP(2)' EQ 'CATLG')                                  24450000
&B(4)    SETB  ('&DISP(2)' EQ 'UNCATLG')                                24460000
         AIF   (&B(1) OR &B(2) OR &B(3) OR &B(4)).OK2                   24470000
         MNOTE 8,'&DISP(2) IS INVALID, DISP=(,KEEP) USED'               24480000
&B(1)    SETB  1                                                        24490000
.OK2     ANOP                                                           24500000
&I       SETA  8*&B(1)+4*&B(2)+2*&B(3)+&B(4)                            24510000
         MVC   S99TUKEY(8),=Y(DALNDISP,1,1,X'0&I.00')                   24520000
         RCPDINC 8                                                      24530000
.TD3     AIF   ('&DISP(3)' EQ '').EXIT                                  24540000
         SPACE                                                          24550000
*********************************************************************** 24560000
**   DATASET CONDITIONAL DISPOSITION                                 ** 24570000
*********************************************************************** 24580000
&B(1)    SETB  ('&DISP(3)' EQ 'KEEP')                                   24590000
&B(2)    SETB  ('&DISP(3)' EQ 'DELETE')                                 24600000
&B(3)    SETB  ('&DISP(3)' EQ 'CATLG')                                  24610000
&B(4)    SETB  ('&DISP(3)' EQ 'UNCATLG')                                24620000
         AIF   (&B(1) OR &B(2) OR &B(3) OR &B(4)).OK3                   24630000
         MNOTE 8,'&DISP(3) IS INVALID, DISP=(,,KEEP) USED'              24640000
&B(1)    SETB  1                                                        24650000
.OK3     ANOP                                                           24660000
&I       SETA  8*&B(1)+4*&B(2)+2*&B(3)+&B(4)                            24670000
         MVI   S99TUKEY(8),=Y(DALCDISP,1,1,X'0&I.00')                   24680000
         RCPDINC 8                                                      24690000
.EXIT    MEND                                                           24700000
./       ADD   NAME=RCPDS
         MACRO                                                          28130000
         RCPDS                                                          28140000
         GBLB  &RCPDSBR                                                 28150000
         GBLC  &RCPWKDS,&RCPWKCS,&RCPDS                                 28160000
         AIF   ('&RCPDS' NE '').RESUME                                  28170000
&RCPDS   SETC  '&SYSECT'                                                28180000
         AIF   ('&RCPWKDS' EQ '').CSECT                                 28190000
&RCPWKDS DSECT                         ENTER WORKAREA DSECT             28200000
         MEXIT                                                          28210000
.CSECT   AIF   ('&RCPWKCS' EQ '').BRANCH                                28220000
&RCPWKCS CSECT                         ENTER WORKAREA CSECT             28230000
         MEXIT                                                          28240000
.RESUME  AIF   (&RCPDSBR).BRTO                                          28250000
&RCPDS   CSECT                         RESUME PROGRAM CSECT             28260000
&RCPDS   SETC  ''                                                       28270000
         MEXIT                                                          28280000
.BRANCH  ANOP                                                           28290000
&RCPDS   SETC  'RCP&SYSNDX'                                             28300000
&RCPDSBR SETB  1                                                        28310000
         B     &RCPDS                  BRANCH AROUND CONSTANTS          28320000
         MEXIT                                                          28330000
.BRTO    ANOP                                                           28340000
&RCPDS   DS    0H                                                       28350000
&RCPDSBR SETB  0                                                        28360000
&RCPDS   SETC  ''                                                       28370000
         MEND                                                           28380000
./       ADD   NAME=RCPDSECT
         MACRO                                                          30040000
&NAME    RCPDSECT &LTORG=YES                                            30050000
         AIF   ('&LTORG' NE 'YES').RCPDS                                30060000
*********************************************************************** 30070000
****                  LITERALS                                     **** 30080000
*********************************************************************** 30090000
         SPACE 3                                                        30100000
         LTORG                                                          30110000
         EJECT                                                          30120000
.RCPDS   RCPDS                                                          30130000
         MEND                                                           30140000
./       ADD   NAME=RCPDSN
         MACRO                                                          13080000
         RCPDSN &DSN,&MEM                                               13090000
         LCLC  &MEMBER                                                  13100000
         GBLC  &DYNP                                                    13110000
         SPACE                                                          13120000
*********************************************************************** 13130000
**   BUILD THE DSNAME TEXT UNIT                                      ** 13140000
*********************************************************************** 13150000
         AIF   ('&DSN'(1,1) EQ '''').Q                                  13160000
         AIF   ('&DSN'(K'&DSN,1) EQ '/').BD                             13170000
         AIF   ('&DSN'(1,1) EQ '(').REG                                 13180000
         AIF   ('&DSN'  EQ '*').TERM                                    13190000
         RCPSR2                                                         13200000
         L     R14,&DSN                LOAD ADDRESS OF DSNAME           13210000
         LH    R2,&DSN+4               LOAD LENGTH OF DSNAME            13220000
.STH     STH   R2,S99TULNG             STORE DSNAME LENGTH              13230000
         BCTR  R2,0                    DECREMENT FOR EXECUTE            13240000
         EX    R2,&DYNP.MVC            MOVE DSNAME                      13250000
         MVI   S99TUKEY+1,DALDSNAM     MOVE IN DSNAME KEY               13260000
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 13270000
         RCPDINC 50                                                     13280000
         AGO   .TMEMBER                                                 13290000
.REG     L     R14,0&DSN               LOAD ADDRESS OF DSNAME           13300000
         RCPSR2                                                         13310000
         LH    R2,4&DSN                LOAD LENGTH OF DSNAME            13320000
         AGO   .STH                                                     13330000
.TERM    MVI   S99TUKEY+1,DALTERM                                       13340000
         RCPDINC 4                                                      13350000
         MEXIT                                                          13360000
.BD      RCPTUBFR DALDSNAM,50,&DSN                                      13370000
         AGO   .TMEMBER                                                 13380000
.Q       RCPBTU DALDSNAM,1,&DSN                                         13390000
.TMEMBER AIF   ('&MEM' EQ '').EXIT                                      13400000
         SPACE                                                          13410000
*********************************************************************** 13420000
**   BUILD THE MEMBER NAME TEXT UNIT                                 ** 13430000
*********************************************************************** 13440000
&MEMBER  SETC  '&MEM'                                                   13450000
         AIF   ('&MEM' NE '*').MOK                                      13460000
         AIF   ('&DSN'(1,1) NE '''').MAST                               13470000
         MNOTE 8,'MEMBER=* INVALID WITH QUOTED DSNAME'                  13480000
         MEXIT                                                          13490000
.MAST    ANOP                                                           13500000
&MEMBER  SETC  '8+&DSN'                                                 13510000
.MOK     ANOP                                                           13520000
         AIF   ('&MEMBER'(K'&MEMBER,1) EQ '/').BM                       13530000
         RCPSR2                                                         13540000
         AIF   ('&MEMBER'(1,1) EQ '(').RM                               13550000
         LH    R2,4+&MEMBER            LOAD LENGTH OF MEMBER NAME       13560000
         LTR   R2,R2                   TEST FOR ZERO                    13570000
         BZ    *+30                    IF NO MEMBER, SKIP               13580000
         L     R14,&MEMBER             LOAD ADDRESS OF MEMBER           13590000
         AGO   .STHM                                                    13600000
.RM      LH    R2,4&MEMBER             LOAD LENGTH OF MEMBER            13610000
         LTR   R2,R2                   AND TEST FOR ZERO                13620000
         BZ    *+30                    IF NO MEMBER, SKIP               13630000
         L     R14,0&MEMBER            LOAD ADDRESS OF MEMBER           13640000
.STHM    STH   R2,S99TULNG             STORE LENGTH OF MEMBER           13650000
         BCTR  R2,0                    DECREMENT FOR EXECUTE            13660000
         EX    R2,&DYNP.MVC            MOVE IN MEMBER NAME              13670000
         MVI   S99TUKEY+1,DALMEMBR     MOVE IN MEMBER KEY               13680000
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 13690000
         RCPDINC 14                                                     13700000
         MEXIT                                                          13710000
.BM      RCPTUBFR DALMEMBR,14,&MEMBER                                   13720000
         MEXIT                                                          13730000
.QM      RCPBTU DALMEMBR,1,&MEMBER                                      13740000
.EXIT    MEND                                                           13750000
./       ADD   NAME=RCPDSNPD
         MACRO                                                          16760000
         RCPDSNPD &PDE                                                  16770000
         AIF   ('&PDE'(1,1) EQ '(').RPDE                                16780000
         RCPDSN &PDE,8+&PDE                                             16790000
         RCPPSWD 16+&PDE                                                16800000
         MEXIT                                                          16810000
.RPDE    RCPDSN &PDE,8&PDE                                              16820000
         RCPPSWD 16(&PDE)                                               16830000
         MEND                                                           16840000
./       ADD   NAME=RCPDSNRT
         MACRO                                                          20220000
         RCPDSNRT                                                       20230000
         SPACE                                                          20240000
*********************************************************************** 20250000
**    DSNAME RETURN TEXT UNIT                                        ** 20260000
*********************************************************************** 20270000
         MVI   S99TUKEY+1,DALRTDSN     SET RETURN DSNAME KEY            20280000
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 20290000
         MVI   S99TULNG+1,44           SET LENGTH FIELD                 20300000
         RCPDINC 50                                                     20310000
         MEND                                                           20320000
./       ADD   NAME=RCPDSRGR
         MACRO                                                          24720000
         RCPDSRGR                                                       24730000
         SPACE                                                          24740000
*********************************************************************** 24750000
**    DSORG RETURN TEXT UNIT                                         ** 24760000
*********************************************************************** 24770000
         MVI   S99TUKEY+1,DALRTORG     SET RETURN DSORG KEY             24780000
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 24790000
         MVI   S99TULNG+1,2            SET LENGTH FIELD                 24800000
         XC    S99TUPAR(2),S99TUPAR    INITIALIZE FIELD TO ZERO         24810000
         RCPDINC 8                                                      24820000
         MEND                                                           24830000
./       ADD   NAME=RCPDUMMY
         MACRO                                                          28400000
         RCPDUMMY &DUMMY                                                28410000
         SPACE                                                          28420000
*********************************************************************** 28430000
**      DUMMY DATASET TEXT UNIT                                      ** 28440000
*********************************************************************** 28450000
         MVI   S99TUPAR+1,DALDUMMY     MOVE IN DUMMY DS TEXT UNIT KEY   28460000
         RCPDINC 4                                                      28470000
         MEND                                                           28480000
./       ADD   NAME=RCPENDD
         MACRO                                                          30160000
&NAME    RCPENDD                                                        30170000
         GBLB  &RCPECT(2),&RCPUPT(2),&RCPPSCB(2),&RCPS99(2)             30180000
         GBLC  &RCPPRE,&RCPWKDS,&RCPDS                                  30190000
         LCLC  &P,&CS                                                   30200000
&CS      SETC  '&RCPDS'                PROGRAM CSECT                    30210000
         AIF   (NOT &RCPS99(1)).TDS                                     30220000
         DYNSPACE                                                       30230000
.TDS     AIF   ('&RCPWKDS' EQ '').RCPDS                                 30240000
         DS    0D                      ALIGN TO DOUBLEWORD              30250000
&P       SETC  '&RCPPRE'                                                30260000
&P.WKLEN EQU   *-&RCPWKDS              LENGTH OF WORK AREA              30270000
.RCPDS   RCPDS                                                          30280000
         EJECT                                                          30290000
         AIF   (NOT &RCPECT(1) OR &RCPECT(2)).TRYUPT                    30300000
         IKJECT                                                         30310000
&CS      CSECT                         REENTER MAIN CSECT               30320000
         EJECT                                                          30330000
&RCPECT(2)     SETB           1                                         30340000
.TRYUPT  AIF   (NOT &RCPUPT(1) OR &RCPUPT(2)).TRYPSCB                   30350000
         IKJUPT                                                         30360000
&CS      CSECT                         REENTER MAIN CSECT               30370000
         EJECT                                                          30380000
&RCPUPT(2) SETB  1                                                      30390000
.TRYPSCB AIF   (NOT &RCPPSCB(1) OR &RCPPSCB(2)).TRYS99                  30400000
         IKJPSCB                                                        30410000
&CS      CSECT                         REENTER MAIN CSECT               30420000
         EJECT                                                          30430000
&RCPPSCB(2) SETB  1                                                     30440000
.TRYS99  AIF   (NOT &RCPS99(1) OR &RCPS99(2)).TRYREST                   30450000
         IEFZB4D0                                                       30460000
         EJECT                                                          30470000
         IEFZB4D2                                                       30480000
&CS      CSECT                         REENTER MAIN CSECT               30490000
         EJECT                                                          30500000
&RCPS99(2) SETB  1                                                      30510000
.TRYREST MEND                                                           30520000
./       ADD   NAME=RCPFDDN
         MACRO                                                          13770000
         RCPFDDN &DDN                                                   13780000
         GBLC &DYNP                                                     13790000
         SPACE                                                          13800000
*********************************************************************** 13810000
**        FREE DDNAME TEXT UNIT                                      ** 13820000
*********************************************************************** 13830000
         AIF   ('&DDN'(1,1) EQ '''').Q                                  13840000
         AIF   ('&DDN'(K'&DDN,1) EQ '/').B                              13850000
         RCPSR2                                                         13860000
         AIF   ('&DDN'(1,1) EQ '(').R                                   13870000
         L     R14,&DDN                LOAD ADDRESS OF DDNAME           13880000
         LH    R2,&DDN+4               LOAD LENGTH OF DDNAME            13890000
         AGO   .STH                                                     13900000
.R       L     R14,0&DDN               LOAD ADDRESS OF DDNAME           13910000
         LH    R2,4&DDN                LOAD LENGTH OF DDNAME            13920000
.STH     STH   R2,S99TULNG             STORE DDNAME LENGTH              13930000
         BCTR  R2,0                    DECREMENT FOR EXECUTE            13940000
         EX    R2,&DYNP.MVC            MOVE DDNAME                      13950000
         MVI   S99TUKEY+1,DUNDDNAM     MOVE IN DDNAME KEY               13960000
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 13970000
         RCPDINC 14                                                     13980000
         MEXIT                                                          13990000
.Q       RCPBTU DUNDDNAM,1,&DDN                                         14000000
         MEXIT                                                          14010000
.B       RCPTUBFR DUNDDNAM,14,&DDN                                      14020000
         MEND                                                           14030000
./       ADD   NAME=RCPFDISP
         MACRO                                                          16860000
         RCPFDISP &DISP                                                 16870000
         LCLB  &B(4)                                                    16880000
         LCLA  &I                                                       16890000
         SPACE                                                          16900000
*********************************************************************** 16910000
**       OVERRIDING DISPOSITION                                      ** 16920000
*********************************************************************** 16930000
&B(1)    SETB  ('&DISP' EQ 'KEEP')                                      16940000
&B(2)    SETB  ('&DISP' EQ 'DELETE')                                    16950000
&B(3)    SETB  ('&DISP' EQ 'CATLG')                                     16960000
&B(4)    SETB  ('&DISP' EQ 'UNCATLG')                                   16970000
         AIF   (&B(1) OR &B(2) OR &B(3) OR &B(4)).OK3                   16980000
         MNOTE 8,'&DISP IS INVALID, DISP=KEEP USED'                     16990000
&B(1)    SETB  1                                                        17000000
.OK3     ANOP                                                           17010000
&I       SETA  8*&B(1)+4*&B(2)+2*&B(3)+&B(4)                            17020000
         MVC   S99TUKEY(8),=Y(DUNOVDSP,1,1,X'0&I.00')                   17030000
         RCPDINC 8                                                      17040000
.EXIT    MEND                                                           17050000
./       ADD   NAME=RCPFDSN
         MACRO                                                          20340000
         RCPFDSN &DSN,&MEM                                              20350000
         LCLC  &MEMBER                                                  20360000
         GBLC  &DYNP                                                    20370000
         SPACE                                                          20380000
*********************************************************************** 20390000
**      FREE DATA SET TEXT UNIT                                      ** 20400000
*********************************************************************** 20410000
         AIF   ('&DSN'(1,1) EQ '''').Q                                  20420000
         AIF   ('&DSN'(K'&DSN,1) EQ '/').BD                             20430000
         AIF   ('&DSN'(1,1) EQ '(').REG                                 20440000
         RCPSR2                                                         20450000
         L     R14,&DSN                LOAD ADDRESS OF DSNAME           20460000
         LH    R2,&DSN+4               LOAD LENGTH OF DSNAME            20470000
.STH     STH   R2,S99TULNG             STORE DSNAME LENGTH              20480000
         BCTR  R2,0                    DECREMENT FOR EXECUTE            20490000
         EX    R2,&DYNP.MVC            MOVE DSNAME                      20500000
         MVI   S99TUKEY+1,DUNDSNAM     MOVE IN DSNAME KEY               20510000
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 20520000
         RCPDINC 50                                                     20530000
         AGO   .TMEMBER                                                 20540000
.REG     L     R14,0&DSN               LOAD ADDRESS OF DSNAME           20550000
         RCPSR2                                                         20560000
         LH    R2,4&DSN                LOAD LENGTH OF DSNAME            20570000
         AGO   .STH                                                     20580000
.BD      RCPTUBFR DUNDSNAM,50,&DSN                                      20590000
         AGO   .TMEMBER                                                 20600000
.Q       RCPBTU DUNDSNAM,1,&DSN                                         20610000
.TMEMBER AIF   ('&MEM' EQ '').EXIT                                      20620000
         SPACE                                                          20630000
*********************************************************************** 20640000
**       FREE MEMBER NAME TEXT UNIT                                  ** 20650000
*********************************************************************** 20660000
&MEMBER  SETC  '&MEM'                                                   20670000
         AIF   ('&MEM' NE '*').MOK                                      20680000
         AIF   ('&DSN'(1,1) NE '''').MAST                               20690000
         MNOTE 8,'MEMBER=* INVALID WITH QUOTED DSNAME'                  20700000
         MEXIT                                                          20710000
.MAST    ANOP                                                           20720000
&MEMBER  SETC  '8+&DSN'                                                 20730000
.MOK     ANOP                                                           20740000
         AIF   ('&MEMBER'(K'&MEMBER,1) EQ '/').BM                       20750000
         RCPSR2                                                         20760000
         AIF   ('&MEMBER'(1,1) EQ '(').RM                               20770000
         LH    R2,4+&MEMBER            LOAD LENGTH OF MEMBER NAME       20780000
         LTR   R2,R2                   TEST FOR ZERO                    20790000
         BZ    *+30                    IF NO MEMBER, SKIP               20800000
         L     R14,&MEMBER             LOAD ADDRESS OF MEMBER           20810000
         AGO   .STHM                                                    20820000
.RM      LH    R2,4&MEMBER             LOAD LENGTH OF MEMBER            20830000
         LTR   R2,R2                   AND TEST FOR ZERO                20840000
         BZ    *+30                    IF NO MEMBER, SKIP               20850000
         L     R14,0&MEMBER            LOAD ADDRESS OF MEMBER           20860000
.STHM    STH   R2,S99TULNG             STORE LENGTH OF MEMBER           20870000
         BCTR  R2,0                    DECREMENT FOR EXECUTE            20880000
         EX    R2,&DYNP.MVC            MOVE IN MEMBER NAME              20890000
         MVI   S99TUKEY+1,DUNMEMBR     MOVE IN MEMBER KEY               20900000
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 20910000
         RCPDINC 14                                                     20920000
         MEXIT                                                          20930000
.BM      RCPTUBFR DUNMEMBR,14,&MEMBER                                   20940000
         MEXIT                                                          20950000
.QM      RCPBTU DUNMEMBR,1,&MEMBER                                      20960000
.EXIT    MEND                                                           20970000
./       ADD   NAME=RCPFHOLD
         MACRO                                                          24850000
         RCPFHOLD &H                                                    24860000
         AIF   ('&H' EQ 'YES').YES                                      24870000
         AIF   ('&H' EQ 'NO').NO                                        24880000
         MNOTE 4,'HOLD PARMETER VALUE INCORRECT - IGNORED'              24890000
         MEXIT                                                          24900000
.YES     ANOP                                                           24910000
         SPACE 1                                                        24920000
*********************************************************************** 24930000
**       OVERIDING SYSOUT HOLD TEXT UNIT                             ** 24940000
*********************************************************************** 24950000
         SPACE 1                                                        24960000
         MVI   S99TUKEY+1,DUNOVSHQ MOVE IN TEXT UNIT KEY                24970000
         RCPDINC 4                                                      24980000
         MEXIT                                                          24990000
.NO      ANOP                                                           25000000
         SPACE 1                                                        25010000
*********************************************************************** 25020000
**       OVERIDING SYSOUT NO HOLD TEXT UNIT                          ** 25030000
*********************************************************************** 25040000
         SPACE 1                                                        25050000
         MVI   S99TUKEY+1,DUNOVSHQ MOVE IN TEXT UNIT KEY                25060000
         RCPDINC 4                                                      25070000
         MEND                                                           25080000
./       ADD   NAME=RCPFORUS
         MACRO - TO SET UP SVC 99 TEXT UNIT 'FOR USER'                  28500000
         RCPFORUS &T                                                    28510000
         SPACE 1                                                        28520000
*********************************************************************** 28530000
**       'FOR USER' TEXT UNIT                                        ** 28540000
*********************************************************************** 28550000
         RCPVCHAR 0,5,&T,N=X'7701'                                      28560000
         MEND                                                           28570000
./       ADD   NAME=RCPFREE
         MACRO                                                          14050000
         RCPFREE &FREE                                                  14060000
         SPACE                                                          14070000
*********************************************************************** 14080000
**      UNALLOC AT CLOSE TEXT UNIT                                   ** 14090000
*********************************************************************** 14100000
         MVI   S99TUPAR+1,DALCLOSE     MOVE IN CLOSE TEXT UNIT KEY      14110000
         RCPDINC 4                                                      14120000
         MEND                                                           14130000
./       ADD   NAME=RCPIOPL
         MACRO                                                          30540000
&NAME    RCPIOPL                                                        30550000
         GBLC  &RCPPRE                                                  30560000
         GBLB  &RCPIOPL(2)                                              30570000
         GBLB  &RCPSTPB(2),&RCPPTPB(2),&RCPPGPB(2),&RCPGTPB(2)          30580000
         LCLC  &P,&L,&L1                                                30590000
&P       SETC  '&RCPPRE'                                                30600000
         EJECT                                                          30610000
         AIF   (&RCPIOPL(2)).BYPIOPL                                    30620000
&RCPIOPL(2) SETB 1                                                      30630000
         IKJIOPL                                                        30640000
L#IOPL   EQU   *-IOPL                  LENGTH OF IO PARAM LIST          30650000
&SYSECT  CSECT                         RESUME PROGRAM CSECT             30660000
         SPACE 3                                                        30670000
.BYPIOPL RCPDS                                                          30680000
&P.IOPL  DS    CL(L#IOPL)              RESERVE SPACE FOR IOPL           30690000
         RCPDS                                                          30700000
         SPACE 5                                                        30710000
*********************************************************************** 30720000
***   THIS CODE GENERATES AN I/O SERVICE ROUTINE PARAMETER LIST     *** 30730000
*********************************************************************** 30740000
         LA    R1,&P.IOPL              LOAD IOPL ADDRESS                30750000
         USING IOPL,R1                 IOPL ADDRESSABLE                 30760000
         MVC   IOPLUPT,CPPLUPT         MOVE IN ADDRESS OF UPT           30770000
         MVC   IOPLECT,CPPLECT         MOVE IN ADDRESS OF ECT           30780000
         LA    R15,&P.ECB              LOAD ADDRESS OF ATTN ECB         30790000
         ST    R15,IOPLECB             AND STORE IN IOPL                30800000
         DROP  R1                                                       30810000
  AIF (&RCPSTPB(1) OR &RCPGTPB(1) OR &RCPPGPB(1) OR &RCPPTPB(1)).I      30820000
         MEXIT                                                          30830000
.I       EJECT                                                          30840000
         AIF   (NOT &RCPSTPB(1) OR &RCPSTPB(2)).TPT                     30850000
         IKJSTPB                                                        30860000
&RCPSTPB(2) SETB 1                                                      30870000
L#STPB   EQU   *-STPB         LENGTH OF STPB                            30880000
&SYSECT  CSECT                                                          30890000
.TPT     AIF   (NOT &RCPPTPB(1) OR &RCPPTPB(2)).TGT                     30900000
         IKJPTPB                                                        30910000
&RCPPTPB(2) SETB 1                                                      30920000
L#PTPB   EQU   *-PTPB         LENGTH OF PTPB                            30930000
&SYSECT  CSECT                                                          30940000
.TGT     AIF   (NOT &RCPGTPB(1) OR &RCPGTPB(2)).TPG                     30950000
         IKJGTPB                                                        30960000
&RCPGTPB(2) SETB 1                                                      30970000
L#GTPB   EQU   *-GTPB         LENGTH OF GTPB                            30980000
&SYSECT  CSECT                                                          30990000
.TPG     AIF   (NOT &RCPPGPB(1) OR &RCPPGPB(2)).STO                     31000000
         IKJPGPB                                                        31010000
&RCPPGPB(2) SETB 1                                                      31020000
L#PGPB   EQU   *-PGPB         LENGTH OF PGPB                            31030000
&SYSECT  CSECT                                                          31040000
.STO     SPACE 3                                                        31050000
&L       SETC  ''                                                       31060000
         RCPDS                                                          31070000
         AIF   (NOT &RCPSTPB(1)).XPT                                    31080000
&P.STPB  DS    CL(L#STPB)              RESERVE SPACE FOR STPB           31090000
&L       SETC  '&L.+L#STPB'                                             31100000
.XPT     AIF   (NOT &RCPPTPB(1)).XGT                                    31110000
&P.PTPB  DS    CL(L#PTPB)              RESERVE SPACE FOR PTPB           31120000
&L       SETC  '&L.+L#PTPB'                                             31130000
.XGT     AIF   (NOT &RCPGTPB(1)).XPG                                    31140000
&P.GTPB  DS    CL(L#GTPB)              RESERVE SPACE FOR GTPB           31150000
&L       SETC  '&L.+L#GTPB'                                             31160000
.XPG     AIF   (NOT &RCPPGPB(1)).XC                                     31170000
&P.PGPB  DS    CL(L#PGPB)              RESERVE SPACE FOR PGPB           31180000
&L       SETC  '&L.+L#PGPB'                                             31190000
.XC      RCPDS                                                          31200000
&L1      SETC  '&L'(2,K'&L-1)                                           31210000
&L       SETC  '&P'.'&L1'(3,4)                                          31220000
         XC    &L.(&L1.),&L            CLEAR IOPB AREA                  31230000
         MEND                                                           31240000
./       ADD   NAME=RCPLINK
         MACRO                                                          14150000
&NAME    RCPLINK &MODULE                                                14160000
         LCLC  &OFFSET,&C                                               14170000
         AIF   ('&MODULE' EQ '').ERROR                                  14180000
         AIF   ('&MODULE' NE 'IKJPARS').T1                              14190000
&OFFSET  SETC  '524'                                                    14200000
         AGO   .START                                                   14210000
.T1      AIF   ('&MODULE' NE 'IKJDAIR').T2                              14220000
&OFFSET  SETC  '732'                                                    14230000
         AGO   .START                                                   14240000
.T2      AIF   ('&MODULE' NE 'IKJEHDEF').T3                             14250000
&OFFSET  SETC  '736'                                                    14260000
         AGO   .START                                                   14270000
.T3      AIF   ('&MODULE' NE 'IKJEHCIR').T4                             14280000
&OFFSET  SETC  '740'                                                    14290000
         AGO   .START                                                   14300000
.T4      AIF   ('&MODULE' NE 'IKJPUTL').T5                              14310000
&OFFSET  SETC  '444'                                                    14320000
         AGO   .START                                                   14330000
.T5      AIF   ('&MODULE' NE 'IKJGETL').T6                              14340000
&OFFSET  SETC  '348'                                                    14350000
         AGO   .START                                                   14360000
.T6      AIF   ('&MODULE' NE 'IKJSCAN').T7                              14370000
&OFFSET  SETC  '480'                                                    14380000
         AGO   .START                                                   14390000
.T7      AIF   ('&MODULE' NE 'IKJPTGT').T8                              14400000
&OFFSET  SETC  '464'                                                    14410000
         AGO   .START                                                   14420000
.T8      AIF   ('&MODULE' NE 'IKJSTCK').T9                              14430000
&OFFSET  SETC  '472'                                                    14440000
         AGO   .START                                                   14450000
.T9      ANOP                                                           14460000
&NAME    DS    0H                                                       14470000
*                                                                       14480000
 MNOTE *,' EP OF &MODULE. NOT IN CVT. STANDARD LINK USED'               14490000
*                                                                       14500000
         AGO   .LINK                                                    14510000
.START   ANOP                                                           14520000
&NAME    L     R15,16                  LOAD CVT ADDRESS                 14530000
         L     R15,&OFFSET.(R15)       LOAD MODULE ADDRESS              14540000
         LTR   R15,R15                 IS MODULE ADDRESS THERE?         14550000
&C       SETC  'RCP&SYSNDX'                                             14560000
         BNM   &C.L                     IF NOT, BRANCH TO LINK          14570000
         BALR  R14,R15                  ELSE BALR TO IT                 14580000
         B     &C.B                      AND BYPASS LINK                14590000
&C.L     LINK  EP=&MODULE                                               14600000
&C.B     DS    0H                      BRANCHED TO IF LINK BYPASSED     14610000
         MEXIT                                                          14620000
.LINK    ANOP                                                           14630000
&NAME    LINK  EP=&MODULE                                               14640000
         MEXIT                                                          14650000
.ERROR   MNOTE 4,'NO MODULE NAME SPECIFIED'                             14660000
         MEND                                                           14670000
./       ADD   NAME=RCPLOAD
         MACRO                                                          17070000
&NAME    RCPLOAD &MOD,&EP1                                              17080000
         GBLC  &RCPPTEP,&RCPGTEP,&RCPPGEP                               17090000
         GBLC  &RCPDFEP,&RCPSTEP,&RCPPREP                               17100000
         GBLC  &RCPPRE                                                  17110000
         LCLA  &I,&J                                                    17120000
         LCLB  &EPXISTS                                                 17130000
         LCLC  &OFFSET,&C,&EP,&MODULE                                   17140000
&EP      SETC  '&EP1'                                                   17150000
&MODULE  SETC  '&MOD'                                                   17160000
         AIF   ('&MODULE' EQ '').ERROR                                  17170000
         AIF   ('&MODULE'(K'&MOD,1) NE ')').NOBR                        17180000
&I       SETA  K'&MOD                                                   17190000
.LOOP    ANOP                                                           17200000
&I       SETA  &I-1                                                     17210000
         AIF   (&I LT 2).NOLB                                           17220000
         AIF   ('&MOD'(&I,1) NE '(').LOOP                               17230000
&MODULE  SETC  '&MOD'(1,&I-1)                                           17240000
&J       SETA  K'&MOD-1-&I                                              17250000
&EP      SETC  '&MOD'(&I+1,&J)                                          17260000
         RCPDS                                                          17270000
&EP      DS    F                       TO STORE MODULE ADDRESS          17280000
         RCPDS                                                          17290000
.NOBR    ANOP                                                           17300000
&EPXISTS  SETB  ('&EP' NE '')                                           17310000
         AIF   ('&MODULE' NE 'IKJPARS').T1                              17320000
&OFFSET  SETC  '524'                                                    17330000
&RCPPREP SETC '&EP'                                                     17340000
         AIF   (&EPXISTS).START                                         17350000
         RCPDS                                                          17360000
&RCPPREP SETC '&RCPPRE.PREP'                                            17370000
&EP      SETC  '&RCPPREP'                                               17380000
&RCPPREP DS    F                       TO HOLD ADDRESS OF IKJPARS       17390000
         RCPDS                                                          17400000
         AGO   .START                                                   17410000
.T1      AIF   ('&MODULE' NE 'IKJDAIR').T2                              17420000
&OFFSET  SETC  '732'                                                    17430000
         AGO   .START                                                   17440000
.T2      AIF   ('&MODULE' NE 'IKJEHDEF').T3                             17450000
&RCPDFEP SETC  '&EP'                                                    17460000
&OFFSET  SETC  '736'                                                    17470000
         AIF   (&EPXISTS).START                                         17480000
&RCPDFEP SETC  '&RCPPRE.DFEP'                                           17490000
         RCPDS                                                          17500000
&RCPDFEP DS    F                       ADDR OF DEFAULT SERVICE ROUTINE  17510000
         RCPDS                                                          17520000
&EP      SETC  '&RCPDFEP'                                               17530000
         AGO   .START                                                   17540000
.T3      AIF   ('&MODULE' NE 'IKJEHCIR').T4                             17550000
&OFFSET  SETC  '740'                                                    17560000
         AGO   .START                                                   17570000
.T4      AIF   ('&MODULE' NE 'IKJPUTL').T5                              17580000
&RCPPTEP SETC  '&EP'                                                    17590000
&OFFSET  SETC  '444'                                                    17600000
         AIF   (&EPXISTS).START                                         17610000
&RCPPTEP SETC  '&RCPPRE.PTEP'                                           17620000
&EP      SETC  '&RCPPTEP'                                               17630000
         RCPDS                                                          17640000
&RCPPTEP DS    F                       ADDR OF PUTLINE ROUTINE          17650000
         RCPDS                                                          17660000
         AGO   .START                                                   17670000
.T5      AIF   ('&MODULE' NE 'IKJGETL').T6                              17680000
&RCPGTEP SETC  '&EP'                                                    17690000
&OFFSET  SETC  '348'                                                    17700000
         AIF   (&EPXISTS).START                                         17710000
&RCPGTEP SETC  '&RCPPRE.GTEP'                                           17720000
&EP      SETC  '&RCPGTEP'                                               17730000
         RCPDS                                                          17740000
&RCPGTEP DS    F                       ADDR OF GETLINE ROUTINE          17750000
         RCPDS                                                          17760000
         AGO   .START                                                   17770000
.T6      AIF   ('&MODULE' NE 'IKJSCAN').T7                              17780000
&OFFSET  SETC  '480'                                                    17790000
         AGO   .START                                                   17800000
.T7      AIF   ('&MODULE' NE 'IKJPTGT').T8                              17810000
&RCPPGEP SETC  '&EP'                                                    17820000
&OFFSET  SETC  '464'                                                    17830000
         AIF   (&EPXISTS).START                                         17840000
&RCPPGEP SETC  '&RCPPRE.PGEP'                                           17850000
&EP      SETC  '&RCPPGEP'                                               17860000
         RCPDS                                                          17870000
&RCPPGEP DS    F                       ADDR OF PUTGET ROUTINE           17880000
         RCPDS                                                          17890000
         AGO   .START                                                   17900000
.T8      AIF   ('&MODULE' NE 'IKJSTCK').T9                              17910000
&RCPSTEP SETC  '&EP'                                                    17920000
&OFFSET  SETC  '472'                                                    17930000
         AIF   (&EPXISTS).START                                         17940000
&RCPSTEP SETC  '&RCPPRE.STEP'                                           17950000
&EP      SETC  '&RCPSTEP'                                               17960000
         RCPDS                                                          17970000
&RCPSTEP DS    F                       ADDR OF STACK ROUTINE            17980000
         RCPDS                                                          17990000
         AGO   .START                                                   18000000
.T9      ANOP                                                           18010000
&NAME    DS    0H                                                       18020000
*                                                                       18030000
 MNOTE *,' EP OF &MODULE. NOT IN CVT. STANDARD LOAD USED'               18040000
*                                                                       18050000
         AGO   .LOAD                                                    18060000
.START   ANOP                                                           18070000
&NAME    L     R15,16                  LOAD CVT ADDRESS                 18080000
         L     R0,&OFFSET.(R15)        LOAD MODULE ADDRESS              18090000
         LTR   R0,R0                   IS MODULE LOADED?                18100000
&C       SETC  'RCP&SYSNDX'                                             18110000
         BM    &C                      IF SO, BYPASS LOAD MACRO         18120000
.LOAD    LOAD EP=&MODULE.                                               18130000
         AIF   ('&EP' EQ '').EPERR                                      18140000
&C       ST    R0,&EP                  STORE ENTRY POINT ADDRESS        18150000
         MEXIT                                                          18160000
.EPERR   MNOTE 4,'EP RETURN FIELD NOT SPECIFIED'                        18170000
         MEXIT                                                          18180000
.ERROR   MNOTE 4,'NO MODULE NAME SPECIFIED'                             18190000
         MEXIT                                                          18200000
.NOLB    MNOTE 4,'INVALID MODULE NAME ''&MOD'''                         18210000
         MEND                                                           18220000
./       ADD   NAME=RCPLOCSW
*23456789*12345*78921234567893123456789*                                20990000
         MACRO                                                          21000000
         RCPLOCSW &SW                                                   21010000
.********************************************************************   21020000
.*                                                                  *   21030000
.*       INNER MACRO USED BY GOIF, SET, RESET AND FLIP.             *   21040000
.*       THE PARM PASSED IS THE SWITCH OR LIST OF SWITCHES.         *   21050000
.*       RCPLOCSW SCANS THE ARRAYS SET UP BY DCLSW TO SEE IF THE    *   21060000
.*       SWITCH BIT NAMES WERE DECLARED, AND IF A LIST WAS PASSED,  *   21070000
.*       WHETHER ALL THE SWITCH BITS BELONG TO THE SAME BYTE.       *   21080000
.*       RCPLOCSW PASSES BACH THE SWITCH BYTE NAME IN GLOBAL SETC   *   21090000
.*       VARIABLE &RCPDSW1 AND THE SWITCH BIT NAME IN GLOBAL SETC   *   21100000
.*       &RCPDSW2. IF A LIST OF SWITCHES WAS PASSED, &RCPDSW2       *   21110000
.*       CONTAINS THE SWITCH NAMES SEPARATED BY PLUS SIGNS.         *   21120000
.*       IF THE FIRST OR ONLY SWITCH WAS NOT FOUND, &RCPDSW1 IS SET *   21130000
.*       TO NULL. IF A LIST OF SWITCHES IS PASSED AND ANY SWITCH IS *   21140000
.*       NOT DECLARED IN THE SAME SWITCH BYTE AS THE FIRST, AN MNOTE*   21150000
.*       IS ISSUED WARNING OF POSSIBLE ERROR, BUT &RCPDSW1 IS SET   *   21160000
.*       TO THE NAME OF THE SWITCH BYTE CONTAINING THE FIRST SWITCH *   21170000
.*       BIT IN THE LIST.                                           *   21180000
.*                                                                  *   21190000
.********************************************************************   21200000
         GBLA  &RCPDSW#,&RCPGSW#       COUNTER FOR DECLARED SWITCHES    21210000
         GBLA  &RCPDSW0                NO OF SWS FOUND BY RCPLOCSW      21220000
         GBLB  &RCPDSWD(99)            DEFER DECLARE INDICATORS         21230000
         GBLB  &RCPDSW3(20)   INVERT INDICATOR                          21240000
         GBLC  &RCPDSWN(99)            SWITCH BYTE NAMES                21250000
         GBLC  &RCPDSWB(800)           SWITCH BIT NAMES                 21260000
         GBLC  &RCPGSWN(99)            GENERIC SWITCH BYTE NAMES        21270000
         GBLC  &RCPGSWB(99)            GENERIC SWITCH BIT PREFIXES      21280000
         GBLC  &RCPDSW1(20)            SWITCH BYTE NAMES                21290000
         GBLC  &RCPDSW2(20)            SWITCH BIT NAME(S)               21300000
         LCLA  &I,&J,&K,&L,&M,&N                                        21310000
         LCLB  &NOT                                                     21320000
         LCLC  &C,&SW1,&SW2                                             21330000
&RCPDSW0 SETA  0                       INITIALIZE                       21340000
&N       SETA  N'&SW                   NO OF SWITCHES ENTERED           21350000
&J       SETA  &RCPDSW#*8+8            INDEX TO LAST DECLARED SW BIT    21360000
.LOOP1   AIF   (&M GE &N).EXIT        LOOP FOR EACH SW                  21370000
&M       SETA  &M+1                                                     21380000
&SW2     SETC  '&SW(&M)'               SWITCH TO SEARCH FOR             21390000
         AIF   ('&SW2' EQ '').LOOP1    SKIP IF NULL                     21400000
&I       SETA  8                       INDEX TO FIRST DECLARED SW - 1   21410000
&NOT     SETB  0                                                        21420000
         AIF   ('&SW2'(1,1) NE '^' AND '&SW2'(1,1) NE '-').TNOT2        21430000
&SW2     SETC  '&SW2'(2,K'&SW2-1)       REMOVE NOT SIGN                 21440000
&NOT     SETB  1                       INDICATE INVERT FUNCTION         21450000
         AGO   .LOOP1A                 CONTINUE                         21460000
.TNOT2   AIF   (K'&SW2 LT 5).LOOP1A    CHECK LENGTH                     21470000
         AIF   ('&SW2'(1,4) NE 'NOT-').LOOP1A  WAS SWITCH INVERTED?     21480000
&SW2     SETC  '&SW2'(5,K'&SW2-4)      STRIP OFF 'NOT-'                 21490000
&NOT     SETB  1                       INDICATE INVERTED                21500000
.LOOP1A  AIF   (&I GE &J).TGEN         SEARCH NAME ARRAY                21510000
&I       SETA  &I+1                                                     21520000
         AIF   ('&RCPDSWB(&I)' NE '&SW2').LOOP1A                        21530000
.*                                                                      21540000
.*   WE FOUND IT                                                        21550000
.*                                                                      21560000
&L       SETA  (&I-1)/8                INDEX TO BYTE NAME               21570000
&SW1     SETC  '&RCPDSWN(&L)'          GET BYTE NAME                    21580000
.FOUNDSW ANOP                          HAVE WE HAD IT BEFORE?           21590000
&K       SETA  0                                                        21600000
.SWL1    AIF   (&K GE &RCPDSW0).NEWSW1                                  21610000
&K       SETA  &K+1                                                     21620000
         AIF   ('&RCPDSW1(&K)' NE '&SW1').SWL1                          21630000
         AIF   (&RCPDSW3(&K) NE &NOT).SWL1  ENSURE INVERT BIT THE SAME  21640000
.*                                                                      21650000
.* WE FOUND IT                                                          21660000
.*                                                                      21670000
&RCPDSW2(&K) SETC '&RCPDSW2(&K)+&SW2'  CONCATENATE CURRENT SW           21680000
         AGO   .LOOP1                  GO DO NEXT                       21690000
.NEWSW1  ANOP                                                           21700000
&RCPDSW0 SETA  &K+1                    NEXT SW BYTE INDEX               21710000
&RCPDSW1(&RCPDSW0) SETC '&SW1'         BYTE NAME                        21720000
&RCPDSW2(&RCPDSW0) SETC '&SW2'         BIT NAME                         21730000
&RCPDSW3(&RCPDSW0) SETB (&NOT)         SET INVERT INDICATOR             21740000
         AGO   .LOOP1                  GO DO NEXT                       21750000
.TGEN    ANOP  SEARCH GENERIC NAME ARRAY                                21760000
&I       SETA  0                                                        21770000
&L       SETA  K'&SW2                                                   21780000
.LOOP2   ANOP                                                           21790000
&I       SETA  &I+1                                                     21800000
         AIF   (&I GT &RCPGSW#).NOTFND                                  21810000
&C       SETC  '&RCPGSWB(&I)'                                           21820000
         AIF   (&L LT K'&C).LOOP2                                       21830000
         AIF   ('&SW2'(1,K'&C) NE '&C').LOOP2                           21840000
&SW1     SETC  '&RCPGSWN(&I)'                                           21850000
         AGO   .FOUNDSW                EUREKA                           21860000
.NOTFND  MNOTE 4,'SWITCH ''&SW2'' NOT DECLARED'                         21870000
         AGO   .LOOP1                                                   21880000
.EXIT    MEND                                                           21890000
./       ADD   NAME=RCPLOCS1
*23456789*12345*78921234567893123456789*                                25100000
         MACRO                                                          25110000
         RCPLOCSW &SW                                                   25120000
.********************************************************************   25130000
.*                                                                  *   25140000
.*       INNER MACRO USED BY GOIF, SET, RESET AND FLIP.             *   25150000
.*       THE PARM PASSED IS THE SWITCH OR LIST OF SWITCHES.         *   25160000
.*       RCPLOCSW SCANS THE ARRAYS SET UP BY DCLSW TO SEE IF THE    *   25170000
.*       SWITCH BIT NAMES WERE DECLARED, AND IF A LIST WAS PASSED,  *   25180000
.*       WHETHER ALL THE SWITCH BITS BELONG TO THE SAME BYTE.       *   25190000
.*       RCPLOCSW PASSES BACH THE SWITCH BYTE NAME IN GLOBAL SETC   *   25200000
.*       VARIABLE &RCPDSW1 AND THE SWITCH BIT NAME IN GLOBAL SETC   *   25210000
.*       &RCPDSW2. IF A LIST OF SWITCHES WAS PASSED, &RCPDSW2       *   25220000
.*       CONTAINS THE SWITCH NAMES SEPARATED BY PLUS SIGNS.         *   25230000
.*       IF THE FIRST OR ONLY SWITCH WAS NOT FOUND, &RCPDSW1 IS SET *   25240000
.*       TO NULL. IF A LIST OF SWITCHES IS PASSED AND ANY SWITCH IS *   25250000
.*       NOT DECLARED IN THE SAME SWITCH BYTE AS THE FIRST, AN MNOTE*   25260000
.*       IS ISSUED WARNING OF POSSIBLE ERROR, BUT &RCPDSW1 IS SET   *   25270000
.*       TO THE NAME OF THE SWITCH BYTE CONTAINING THE FIRST SWITCH *   25280000
.*       BIT IN THE LIST.                                           *   25290000
.*                                                                  *   25300000
.********************************************************************   25310000
         GBLA  &RCPDSW#,&RCPGSW#       COUNTER FOR DECLARED SWITCHES    25320000
         GBLA  &RCPDSW0                NO OF SWS FOUND BY RCPLOCSW      25330000
         GBLB  &RCPDSWD(99)            DEFER DECLARE INDICATORS         25340000
         GBLC  &RCPDSWN(99)            SWITCH BYTE NAMES                25350000
         GBLC  &RCPDSWB(800)           SWITCH BIT NAMES                 25360000
         GBLC  &RCPGSWN(99)            GENERIC SWITCH BYTE NAMES        25370000
         GBLC  &RCPGSWB(99)            GENERIC SWITCH BIT PREFIXES      25380000
         GBLC  &RCPDSW1(20)            SWITCH BYTE NAMES                25390000
         GBLC  &RCPDSW2(20)            SWITCH BIT NAME(S)               25400000
         LCLA  &I,&J,&K,&L,&M,&N       LOCAL COUNTERS                   25410000
         LCLC  &C,&SW1,&SW2                                             25420000
&RCPDSW0 SETA  0                       INITIALIZE                       25430000
&N       SETA  N'&SW                   NO OF SWITCHES ENTERED           25440000
&J       SETA  &RCPDSW#*8              INDEX TO LAST DECLARED SW BIT    25450000
.LOOP1   AIF   (&M GE &N).EXIT        LOOP FOR EACH SW                  25460000
&M       SETA  &M+1                                                     25470000
&SW2     SETC  '&SW(&M)'               SWITCH TO SEARCH FOR             25480000
         AIF   ('&SW2' EQ '').LOOP1    SKIP IF NULL                     25490000
&I       SETA  8                       INDEX TO FIRST DECLARED SW - 1   25500000
.LOOP1A  AIF   (&I GE &J).TGEN         SEARCH NAME ARRAY                25510000
&I       SETA  &I+1                                                     25520000
         AIF   ('&RCPDSWB(&I)' NE '&SW2').LOOP1A                        25530000
.*                                                                      25540000
.*   WE FOUND IT                                                        25550000
.*                                                                      25560000
&L       SETA  (&I-1)/8                INDEX TO BYTE NAME               25570000
&SW1     SETC  '&RCPDSWN(&L)'          GET BYTE NAME                    25580000
.FOUNDSW ANOP                          HAVE WE HAD IT BEFORE?           25590000
&K       SETA  0                                                        25600000
.SWL1    AIF   (&K GE &RCPDSW0).NEWSW1                                  25610000
&K       SETA  &K+1                                                     25620000
         AIF   ('&RCPDSW1(&K)' NE '&SW1').SWL1                          25630000
.*                                                                      25640000
.* WE FOUND IT                                                          25650000
.*                                                                      25660000
&RCPDSW2(&K) SETC '&RCPDSW2(&K)+&SW2'  CONCATENATE CURRENT SW           25670000
         AGO   .LOOP1                  GO DO NEXT                       25680000
.NENSW1  ANOP                                                           25690000
&RCPDSW0 SETA  &K+1                    NEXT SW BYTE INDEX               25700000
&RCPDSW1(&RCPDSW0) SETC '&SW1'         BYTE NAME                        25710000
&RCPDSW2(&RCPDSW0) SETC '&SW2'         BIT NAME                         25720000
         AGO   .LOOP1                  GO DO NEXT                       25730000
.TGEN    ANOP  SEARCH GENERIC NAME ARRAY                                25740000
&I       SETA  0                                                        25750000
&L       SETA  K'&SW2                                                   25760000
.LOOP2   ANOP                                                           25770000
&I       SETA  &I+1                                                     25780000
         AIF   (&I GT &RCPGSW#).NOTFND                                  25790000
&SW1     SETC  '&RCPGSWN(&I)'                                           25800000
         AIF   (&L LT K'&SW1).LOOP2                                     25810000
         AIF   ('&SW1'(1,&L) NE '&SW2').LOOP2                           25820000
         AGO   .FOUNDSW                EUREKA                           25830000
.NOTFND  MNOTE 4,'SWITCH ''&SW2'' NOT DECLARED'                         25840000
         AGO   .LOOP1                                                   25850000
.EXIT    MEND                                                           25860000
./       ADD   NAME=RCPLOCS2
*23456789*12345*78921234567893123456789*                                28590000
         MACRO                                                          28600000
         RCPLOCSW &SW                                                   28610000
.********************************************************************   28620000
.*                                                                  *   28630000
.*       INNER MACRO USED BY GOIF, SET, RESET AND FLIP.             *   28640000
.*       THE PARM PASSED IS THE SWITCH OR LIST OF SWITCHES.         *   28650000
.*       RCPLOCSW SCANS THE ARRAYS SET UP BY DCLSW TO SEE IF THE    *   28660000
.*       SWITCH BIT NAMES WERE DECLARED, AND IF A LIST WAS PASSED,  *   28670000
.*       WHETHER ALL THE SWITCH BITS BELONG TO THE SAME BYTE.       *   28680000
.*       RCPLOCSW PASSES BACH THE SWITCH BYTE NAME IN GLOBAL SETC   *   28690000
.*       VARIABLE &RCPDSW1 AND THE SWITCH BIT NAME IN GLOBAL SETC   *   28700000
.*       &RCPDSW2. IF A LIST OF SWITCHES WAS PASSED, &RCPDSW2       *   28710000
.*       CONTAINS THE SWITCH NAMES SEPARATED BY PLUS SIGNS.         *   28720000
.*       IF THE FIRST OR ONLY SWITCH WAS NOT FOUND, &RCPDSW1 IS SET *   28730000
.*       TO NULL. IF A LIST OF SWITCHES IS PASSED AND ANY SWITCH IS *   28740000
.*       NOT DECLARED IN THE SAME SWITCH BYTE AS THE FIRST, AN MNOTE*   28750000
.*       IS ISSUED WARNING OF POSSIBLE ERROR, BUT &RCPDSW1 IS SET   *   28760000
.*       TO THE NAME OF THE SWITCH BYTE CONTAINING THE FIRST SWITCH *   28770000
.*       BIT IN THE LIST.                                           *   28780000
.*                                                                  *   28790000
.********************************************************************   28800000
         GBLA  &RCPDSW#,&RCPGSW#       COUNTER FOR DECLARED SWITCHES    28810000
         GBLB  &RCPDSWD(99)            DEFER DECLARE INDICATORS         28820000
         GBLC  &RCPDSWN(99)            SWITCH BYTE NAMES                28830000
         GBLC  &RCPDSWB(800)           SWITCH BIT NAMES                 28840000
         GBLC  &RCPGSWN(99)            GENERIC SWITCH BYTE NAMES        28850000
         GBLC  &RCPGSWB(99)            GENERIC SWITCH BIT PREFIXES      28860000
         GBLC  &RCPDSW1                SWITCH BYTE NAME                 28870000
         GBLC  &RCPDSW2                SWITCH BIT NAME(S)               28880000
         LCLA  &I,&J,&K,&L,&M,&N       LOCAL COUNTERS                   28890000
         LCLC  &C                                                       28900000
&RCPDSW2 SETC  '&SW(1)'                EXTRACT 1ST SWITCH BIT           28910000
&J       SETA  &RCPDSW#*8+8            ARRAY POS OF LAST SW BIT         28920000
&I       SETA  8                       ARRAY POS-1 OF 1ST SW BIT        28930000
.LOOP1   AIF   (&I GE &J).TGEN         IF SW NOT FOUND IN 1ST ARRAY,    28940000
.*                                      GO SEARCH GENERIC NAME ARRAY    28950000
&I       SETA  &I+1                                                     28960000
         AIF   ('&RCPDSWB(&I)' NE '&RCPDSW2').LOOP1  LOOK FOR MATCH     28970000
.*                                                                      28980000
.*       OK, WE'VE FOUND A MATCH.                                       28990000
.*                                                                      29000000
&I       SETA  (&I-1)/8               GET POS OF SWITCH BYTE            29010000
&RCPDSW1 SETC  '&RCPDSWN(&I)'         MOVE IT TO EXIT PARM VAR          29020000
&I       SETA  &I*8+1                 POINT TO 1ST SW BIT IN IT         29030000
&J       SETA  &I+8                   POINT TO LAST SW BIT IN IT        29040000
&M       SETA  N'&SW                  GET NO OF SWITCHES                29050000
&L       SETA  1                                                        29060000
.*                                                                      29070000
.*       NOW WE PROCESS SUBSEQUENT SWITCHES IN THE LIST                 29080000
.*                                                                      29090000
.LOOP2   AIF   (&L GE &M).EXIT        EXIT WHEN FINISHED                29100000
&L       SETA  &L+1                   POINT TO NEXT SW IN LIST          29110000
&C       SETC  '&SW(&L)'               EXTRACT IT                       29120000
&RCPDSW2 SETC  '&RCPDSW2.+&C'           THEN APPEND TO PREVIOUS         29130000
.*                                                                      29140000
.*       NOW WE CHECK THAT THE SWITCH IS DECLARED IN THE SAME           29150000
.*       BYTE AS THE FIRST.                                             29160000
.*                                                                      29170000
&N       SETA  &I-1                     POINT TO 1ST BIT POS MINUS 1    29180000
.LOOP3   AIF   (&N GE &J).NM            IF SW NOT FOUND, ISSUE MNOTE    29190000
&N       SETA  &N+1                     POINT TO NEXT                   29200000
         AIF   ('&C' NE '&RCPDSWB(&N)').LOOP3  SEARCH FOR MATCH         29210000
         AGO   .LOOP2                   IF FOUND, GO PROCESS NEXT       29220000
.NM      MNOTE 4,'WARNING: SWITCH ''&C'' NOT DECLARED IN SAME BYTE AS  X29230000
               SWITCH ''&SW(1)'' - LOGIC ERROR MAY OCCUR'               29240000
         AGO   .LOOP2            CONTINUE FOR NEXT SWITCH BIT           29250000
.*                                                                      29260000
.*       IF THE SWITCH WAS NOT LOCATED IN THE EXPLICIT NAME ARRAY,      29270000
.*       THE GENERIC NAME ARRAY IS SEARCHED.                            29280000
.*                                                                      29290000
.TGEN    ANOP                                                           29300000
&I       SETA  0                                                        29310000
&RCPDSW2 SETC  '&SW(1)'                EXTRACT 1ST SWITCH               29320000
&L       SETA  K'&RCPDSW2              GET LENGTH OF 1ST SW             29330000
.LOOP4   AIF   (&I GE &RCPGSW#).ERROR  IF NOT SW NOT DECLARED, ERROR    29340000
&I       SETA  &I+1                                                     29350000
&C       SETC  '&RCPGSWB(&I)'          GET GENERIC PREFIX               29360000
&K       SETA  K'&C                    GET LENGTH OF GENERIC PREFIX     29370000
         AIF   (&L LT &K).LOOP4         AND SKIP IF LEN OF SWITCH NAME  29380000
.*                                          < LEN OF GENERIC PREFIX     29390000
         AIF   ('&RCPDSW2'(1,&K) NE '&C').LOOP4  ALSO SKIP IF NO MATCH  29400000
&RCPDSW1 SETC  '&RCPGSWN(&I)'          SAVE SWITCH BYTE NAME            29410000
&I       SETA   1                                                       29420000
&J       SETA   N'&SW                                                   29430000
.LOOP5   AIF   (&I GE &J).EXIT         EXIT WHEN FINISHED               29440000
&I       SETA   &I+1                                                    29450000
&RCPDSW2 SETC   '&RCPDSW2.+&SW(&I)'     APPEND THIS SWITCH              29460000
         AIF    ('&SW(&I)    '(1,&K) EQ '&C').LOOP5 CHECK PREFIX        29470000
         MNOTE 4,'WARNING: SWITCH ''&SW(&I)'' NOT GENERICALLY EQUAL TO X29480000
               SWITCH ''&SW(1)'''                                       29490000
         AGO   .LOOP5                                                   29500000
.ERROR   MNOTE 8,'SWITCH ''&SW(1)'' NOT DECLARED'                       29510000
&RCPDSW1 SETC  ''             INDICATE ERROR                            29520000
.EXIT    MEND                                                           29530000
./       ADD   NAME=RCPMCA
         MACRO                                                          31260000
         RCPMCA &DSECT=YES                                              31270000
         GBLC  &RCPPRE                                                  31280000
         GBLA  &RCPSWS(10)                                              31290000
         LCLC  &P                                                       31300000
     RCPDEBUG                                                           31310000
&P       SETC  '&RCPPRE'                                                31320000
         AIF   (&RCPSWS(2) NE 2).DSECT                                  31330000
&P.MCA   DS    0F                      MODULE COMMUNICATIONS AREA       31340000
         AGO   .MCA2                                                    31350000
.DSECT   ANOP                                                           31360000
&P.MCA   DSECT                         MODULE COMMUNICATIONS AREA       31370000
.MCA2    ANOP                                                           31380000
&P.XDS   DS    F                       ADDR OF EXTERNAL DUMMY SECTION   31390000
         AIF   (&RCPSWS(3) LT 1).EXIT                                   31400000
&P.A#GET DS    F                       ADDRESS OF LIFO GET ROUTINE      31410000
&P.A#FRE DS    F                       ADDRESS OF LIFO FREE ROUTINE     31420000
&P.#S    DS    F                       ADDRESS OF CURRENT LIFO STACK    31430000
&P.#E    DS    F                       ADDRESS OF END OF LIFO STACK     31440000
&P.#N    DS    F                       ADDRESS OF NEXT FREE AREA        31450000
&P.#C    DS    F                       ADDRESS OF NEXT LIFO STACK       31460000
&P.#L    DS    F                       LENGTH OF CURRENT LIFO STACK     31470000
.EXIT    MEND                                                           31480000
./       ADD   NAME=RCPNTU
         MACRO                                                          14690000
         RCPNTU &KEY,&LEN,&PAR                                          14700000
.*                                                                      14710000
.*     THIS IS AN ALLOC/FREE MACRO TEXT UNIT PROCESSOR SUBROUTINE       14720000
.*     MACRO. IT BUILDS NUMERIC TYPE TEXT UNITS.                        14730000
.*                                                                      14740000
         LCLA  &L,&R                                                    14750000
         LCLC  &C                                                       14760000
         GBLC  &RCPTYPE                                                 14770000
.*  ALLOC/FREE INNER MACRO TO SET UP NUMERIC TEXT UNITS                 14780000
&L       SETA  1                       DEFAULT LENGTH                   14790000
         AIF   ('&LEN' EQ '').NL                                        14800000
&L       SETA  &LEN                                                     14810000
.NL      MVI   S99TUKEY+1,&KEY         SET KEY FIELD                    14820000
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 14830000
         MVI   S99TULNG+1,&L           SET LENGTH FIELD                 14840000
         AIF   ('&PAR'(1,1) EQ '(').REG                                 14850000
         RCPTYPE &PAR                  ANALYSE PARAMETER                14860000
         AIF   ('&RCPTYPE' EQ 'N').NUMERIC                              14870000
&R       SETA  4-&L                                                     14880000
         MVC   S99TUPAR(&L),&R+&PAR    MOVE IN QUANTITY                 14890000
         RCPDINC 10                                                     14900000
         MEXIT                                                          14910000
.NUMERIC AIF   (&L EQ 1).NL1                                            14920000
         MVC   S99TUPAR(&L.),=AL&L.(&PAR) MOVE IN QUANTITY              14930000
&R       SETA  &L+6                                                     14940000
         AIF   (&R/2 EQ (&R+1)/2).LOK ENSURE LENGTH EVEN                14950000
&R       SETA  &R+1                                                     14960000
.LOK     RCPDINC &R                                                     14970000
         MEXIT                                                          14980000
.NL1     MVI   S99TUPAR,&PAR           MOVE IN QUANTITY                 14990000
         RCPDINC 8                                                      15000000
         MEXIT                                                          15010000
.REG     ANOP                                                           15020000
&C       SETC  '&PAR'(2,K'&PAR-2)                                       15030000
         AIF   (&L EQ 3).STCM                                           15040000
         AIF   (&L EQ 2).STH                                            15050000
         AIF   (&L EQ 1).STC                                            15060000
         ST    &C,S99TUPAR             STORE TEXT UNIT QUANTITY         15070000
         AGO   .RCPDINC                                                 15080000
.STH     STH   &C,S99TUPAR             STORE TEXT UNIT QUANTITY         15090000
         AGO   .RCPDINC                                                 15100000
.STC     STC   &C,S99TUPAR             STORE TEXT UNIT QUANTITY         15110000
         AGO   .RCPDINC                                                 15120000
.STCM    STCM  &C,7,S99TUPAR           STORE TEXT UNIT QUANTITY         15130000
.RCPDINC RCPDINC 10                                                     15140000
         MEND                                                           15150000
./       ADD   NAME=RCPPERM
         MACRO                                                          18240000
         RCPPERM                                                        18250000
         SPACE                                                          18260000
*********************************************************************** 18270000
**     PERMANENTLY ALLOCATED ATTRIBUTE TEXT UNIT                     ** 18280000
*********************************************************************** 18290000
         MVI   S99TUKEY+1,DALPERMA     SET TEXT UNIT KEY                18300000
         RCPDINC  4                                                     18310000
         MEND                                                           18320000
./       ADD   NAME=RCPPPL
         MACRO                                                          21910000
&NAME    RCPPPL &PCL=,&NOPARM=,&PARSERR=,&PDLREG=R11,                  X21920000
               &PDLNAME=,&PARSEP=,&PARSWKA=                             21930000
         GBLB  &RCPPPL(2),&RCPECT(2)                                    21940000
         GBLC  &RCPPRE,&RCPPREP                                         21950000
         LCLC  &P                                                       21960000
&P       SETC  '&RCPPRE'                                                21970000
         AIF   (&RCPPPL(2)).BPPL                                        21980000
         EJECT                                                          21990000
         IKJPPL                                                         22000000
L#PPL    EQU   *-PPL                   LENGTH OF PPL                    22010000
&SYSECT  CSECT                                                          22020000
         SPACE 1                                                        22030000
&RCPPPL(2) SETB 1                                                       22040000
.BPPL    RCPDS                                                          22050000
&P.PPL   DS    CL(L#PPL)               RESERVE SPACE FOR PPL            22060000
&P.PDLP  DS    F                       POINTER TO PDL                   22070000
         RCPDS                                                          22080000
         SPACE 6                                                        22090000
*********************************************************************** 22100000
***   THIS CODE GENERATES A PARSE PARAMETER LIST                    *** 22110000
*********************************************************************** 22120000
         XC    &P.PDLP,&P.PDLP         ZERO PDL POINTER                 22130000
         AIF   ('&NOPARM(1)' EQ '' OR '&NOPARM(2)' NE '').PB2           22140000
         L     R1,CPPLECT              LOAD ECT ADDRESS                 22150000
&RCPECT(1) SETB 1                                                       22160000
         USING ECT,R1                  ECT ADDRESSABLE                  22170000
         TM    ECTSWS,ECTNOPD          WERE ANY OPERANDS SUPPLIED?      22180000
         BO    &NOPARM(1)              NO, BRANCH OUT                   22190000
         SPACE                                                          22200000
.PB2     LA    R1,&P.PPL               LOAD PPL ADDRESS                 22210000
         USING PPL,R1                                                   22220000
         MVC   PPLUPT,CPPLUPT          MOVE IN UPT ADDRESS              22230000
         MVC   PPLECT,CPPLECT          MOVE IN ECT ADDRESS              22240000
         MVC   PPLCBUF,CPPLCBUF        MOVE IN CBUF ADDRESS             22250000
         LA    R15,&P.ECB              LOAD ATTN ECB ADDRESS            22260000
         ST    R15,PPLECB              AND STORE IN PPL                 22270000
         LA    R15,&P.PDLP             LOAD PDL POINTER ADDRESS         22280000
         ST    R15,PPLANS               AND STORE IN PPL                22290000
         AIF   ('&PARSWKA' EQ '').PB3                                   22300000
         AIF   ('&PARSWKA'(1,1) EQ '').PB4                              22310000
         LA    R15,&PARSWKA            LOAD ADDRESS OF WORK AREA        22320000
         ST    R15,PPLUWA               AND STORE IN PPL                22330000
         AGO   .PB3                                                     22340000
.PB4     ST    &PARSWKA(1),PPLUWA      STORE ADDRESS OF WORKAREA        22350000
.PB3     AIF   ('&PCL' EQ '').EXIT                                      22360000
         L     R15,=V(&PCL)            LOAD PCL ADDRESS                 22370000
         ST    R15,PPLPCL              AND STORE IN PPL                 22380000
         SPACE 2                                                        22390000
         AIF   ('&NOPARM(1)' EQ '' OR '&NOPARM(2)' EQ '').PB5           22400000
         L     R1,CPPLECT              LOAD ECT ADDRESS                 22410000
&RCPECT(1) SETB 1                                                       22420000
         USING ECT,R1                                                   22430000
         TM    ECTSWS,ECTNOPD          WERE ANY OPERANDS SUPPLIED?      22440000
         BO    &NOPARM(1)               NO, BRANCH OUT                  22450000
         SPACE                                                          22460000
.PB5     AIF   ('&SYSPARM' EQ 'MVT').MVTBYP                             22470000
         AIF   ('&RCPPREP' EQ '').NOPREP                                22480000
         L     R15,&RCPPREP            LOAD EP OF IKJPARS               22490000
         BALR  R14,R15                  AND ENTER IT                    22500000
         AGO   .PRET                                                    22510000
.NOPREP  ANOP                                                           22520000
         L     R15,16                  LOAD CVT ADDRESS                 22530000
         TM    524(R15),X'80'          IS IKJPARS LOADED?               22540000
         AIF   ('&PARSEP' EQ '').PBL1                                   22550000
         BZ    &P.LOAD                  NO, BRANCH TO LOAD SVC          22560000
         L     R15,524(15)             LOAD EP OF IKJPARS               22570000
         ST    R15,&PARSEP             SAVE ITS ADDRESS                 22580000
         BALR  R14,R15                 THEN BALR TO IT                  22590000
         B     &P.PLNKB                BYPASS LOAD SVC                  22600000
&P.LOAD  LOAD  EP=IKJPARS                                               22610000
         LR    R15,R0                  LOAD EP OF IKJPARS               22620000
         ST    R15,&PARSEP             SAVE IT                          22630000
         BALR  R14,R15                 THEN BALR TO IT                  22640000
&P.PLNKB DS    0H                                                       22650000
         AGO   .PRET                                                    22660000
.PBL1    BZ    &P.PLINK                 NO, BRANCH TO LINK SVC          22670000
         L     R15,524(R15)            ELSE LOAD ITS ADDRESS            22680000
         BALR  R14,R15                  AND BALR TO IT                  22690000
         B     &P.PLNKB                BYPASS LINK SVC                  22700000
.MVTBYP  ANOP                                                           22710000
&P.PLINK LINK  EP=IKJPARS                                               22720000
&P.PLNKB DS    0H                                                       22730000
.PRET    AIF   ('&PARSERR' EQ '').EXIT                                  22740000
         SPACE                                                          22750000
         LTR   R15,R15                 TEST RETURN CODE                 22760000
         BNZ   &PARSERR                 AND BRANCH ON NON-ZERO          22770000
         SPACE                                                          22780000
         AIF   ('&PDLREG' EQ '' OR '&PDLNAME' EQ '').EXIT               22790000
         L     &PDLREG,&P.PDLP         LOAD PDL ADDRESS                 22800000
         USING &PDLNAME,&PDLREG        PDL DSECT ADDRESSABLE            22810000
.EXIT    MEND                                                           22820000
./       ADD   NAME=RCPPROC
         MACRO                                                          25880000
         RCPPROC &WKCSECT=,&WKDSECT=,                                  X25890000
               &REG1=,&REG0=,&ISA=,&SAVEPRE=,                          X25900000
               &SAVESUF=,&SP=                                           25910000
         GBLA  &RCPSWS(10)                                              25920000
         GBLC  &RCPPRE,&RCPWKCS,&RCPWKDS                                25930000
         GBLC  &RCPSPN                                                  25940000
         LCLC  &P,&C                                                    25950000
         RCPDEBUG                                                       25960000
&P       SETC  '&RCPPRE'                                                25970000
         AIF   ('&WKCSECT' EQ '').TDS                                   25980000
         SPACE                                                          25990000
         MNOTE 4,'WKCSECT= OPTION INVALID WITH PROC OPTION, '           26000000
         MNOTE *,'    WKDSECT=  USED INSTEAD'                           26010000
&RCPWKDS SETC  '&WKCSECT'                                               26020000
         AGO   .SETCS                                                   26030000
.TDS     AIF   ('&WKDSECT' EQ '').SYSECT                                26040000
&RCPWKDS SETC  '&WKDSECT'                                               26050000
         AGO   .SETCS                                                   26060000
.SYSECT  ANOP                                                           26070000
&RCPWKDS SETC  '&SYSECT'                                                26080000
.SET1    AIF   (K'&RCPWKDS LT 8).LOK                                    26090000
&RCPWKDS SETC  '&RCPWKDS'(1,4)'&RCPWKDS'(6,3)'1'                        26100000
         AGO   .SETCS                                                   26110000
.LOK     ANOP                                                           26120000
&RCPWKDS SETC  '&RCPWKDS.1'                                             26130000
.SETCS   ANOP                                                           26140000
&RCPWKCS SETC  ''                                                       26150000
&RCPSWS(4) SETA &RCPSWS(2)-1 SET W/A TO BE FREED OPT IF PROC(MAIN)      26160000
         AIF   ('&ISA' EQ '').NISA                                      26170000
&RCPSWS(3) SETA 1                      SET LIFO FLAG IF ISA SPEC        26180000
.NISA    ANOP                                                           26190000
         SPACE 2                                                        26200000
         RCPDS                                                          26210000
         DS    9D                      SAVE AREA                        26220000
&P.RCODE DS    F                       RETURN CODE                      26230000
         RCPMCA                                                         26240000
         RCPDS                                                          26250000
         SPACE 2                                                        26260000
         AIF   ('&REG1' EQ '').TR0                                      26270000
         LR    &REG1,R1                SAVE CONTENTS OF REG 1           26280000
.TR0     AIF   ('&REG0' EQ '').TP                                       26290000
         LR    &REG0,R0                SAVE CONTENTS OF REG 0           26300000
.TP      AIF   (&RCPSWS(2) EQ 2).PROCMN   PROCMAIN OPTION               26310000
         AIF   (&RCPSWS(3) EQ 1).PL    LIFO OPTION                      26320000
         L     R15,0(R13)              R15 -> MODULE COMMUNIC. AREA     26330000
         L     R15,&P.XDS-&P.MCA(R15)  LOAD EXTERNAL DUMMY SECT ADDR    26340000
         AL    R15,&P.QCON             GET OFFSET TO WORK AREA          26350000
         ST    R15,8(R13)              CHAIN SAVE                       26360000
         ST    R13,4(R15)               AREAS TOGETHER                  26370000
         MVC   0(4,R15),0(R13)         COPY POINTER TO COMM AREA        26380000
         LR    R13,R15                 LOAD WORK AREA ADDRESS           26390000
         USING &RCPWKDS,R13              ESTABLISH ADDRESSABLITY TO IT  26400000
         MEXIT                                                          26410000
.PL      ANOP                                                           26420000
*********************************************************************** 26430000
*        GET WORKAREA FROM LIFO STACK                                 * 26440000
*********************************************************************** 26450000
         #GET  LV=&P.WKLEN                                              26460000
         ST    R1,8(R13)               CHAIN SAVE                       26470000
         ST    R13,4(R1)                AREAS TOGETHER                  26480000
         MVC   0(4,R1),0(R13)          PROPAGATE MODULE COMM. AREA ADDR 26490000
         LR    R13,R1                  LOAD WORK AREA ADDRESS           26500000
         USING &RCPWKDS,R13             ESTABLISH ADDRESSABILITY TO IT  26510000
         MEXIT                                                          26520000
.PROCMN  L     R0,&P.CXD               LOAD WORK AREA LENGTH            26530000
         AIF   ('&SYSPARM' EQ 'MVT').MVT                                26540000
 MNOTE *,'      GETMAIN RU,LV=(0),SP=&SP,BNDRY=PAGE'                    26550000
         GETMAIN RU,LV=(0),SP=&SP,BNDRY=PAGE                            26560000
         AGO   .CONT                                                    26570000
.MVT     AIF   ('&SP' EQ '').NOSP                                       26580000
         ICM   R0,8,=AL1(&SP)          INSERT SUBPOOL NUMBER            26590000
.NOSP    ANOP                                                           26600000
*        GETMAIN R,LV=(0)              OBTAIN A WORK AREA               26610000
.CONT    ANOP                                                           26620000
&RCPSPN  SETC  '&SP'                                                    26630000
         LR    R15,R13                 SAVE CALLER'S SAVE AREA ADDR     26640000
         LR    R13,R1                  LOAD EXT DUMMY SECTION ADDR      26650000
         AL    R13,&P.QCON              ADD OFFSET TO WORK AREA         26660000
         ST    R13,8(R15)              CHAIN SAVE                       26670000
         ST    R15,4(R13)               AREAS TOGETHER                  26680000
         USING &RCPWKDS,R13            GET WORKAREA ADDRESSABILITY      26690000
         ST    R1,&P.XDS               STORE DUMMY SECTION ADDR IN     X26700000
                                         MODULE COMMUNICATIONS AREA     26710000
         LA    R15,&P.MCA              STORE COMMUNICATIONS AREA ADDR   26720000
         ST    R15,0(R13)               IN WORD 1 OF SAVE AREA          26730000
         AIF   (&RCPSWS(3) EQ 0 AND '&ISA' EQ '').EXIT                  26740000
&RCPSWS(3) SETA 1                      SET LIFO IN CASE ONLY ISA SPEC   26750000
&C       SETC  '&ISA'                                                   26760000
         AIF   ('&ISA' NE '').TK                                        26770000
&C       SETC  '8192'                                                   26780000
         AGO   .NK                                                      26790000
.TK      AIF   ('&C'(K'&C,1) NE 'K').NK                                 26800000
&C       SETC  '&C'(1,K'&C-1)'*1024'                                    26810000
.NK      EJECT                                                          26820000
*********************************************************************** 26830000
**       INITIALIZE MODULE COMMUNICATIONS AREA WITH POINTERS         ** 26840000
**       TO LIFO STACK AND LIFO GET/FREE ROUTINES                    ** 26850000
*********************************************************************** 26860000
         SPACE 1                                                        26870000
         MVC   &P.A#GET,=V(#####GET)   MOVE LIFO GET AND FREE           26880000
         MVC   &P.A#FRE,=V(####FREE)    ROUTINE ADDRESSES TO MCA        26890000
         L     R15,=Q(#####ISA)        COMPUTE LIFO STACK               26900000
         AL    R15,&P.XDS               PSEUDO REGISTER OFFSET          26910000
         ST    R15,&P.#S                 AND INITIALIZE POINTERS        26920000
         ST    R15,&P.#N                  IN MODULE COMMUNICATIONS AREA 26930000
         L     R14,=A(&C)              LOAD SIZE OF INITIAL STACK AREA  26940000
         ST    R14,&P.#L               STORE THIS IN MCA                26950000
         ALR   R15,R14                  THEN COMPUTE STACK END ADDRESS  26960000
         ST    R15,&P.#E                 AND STORE THIS INTO MCA        26970000
         EJECT                                                          26980000
*********************************************************************** 26990000
**       LIFO STACK GET/FREE ROUTINES                                ** 27000000
*********************************************************************** 27010000
         SPACE 1                                                        27020000
#####ISA DXD   CL(&C)                  DEFINE PSEUDO REGISTER FOR ISA   27030000
         SPACE 1                                                        27040000
#####GET CSECT                         LIFO GET ROUTINE                 27050000
         USING *,R15                                                    27060000
         USING &P.MCA,R1                                                27070000
         A     R0,&P.F7                ROUND LENGTH UP TO               27080000
         N     R0,&P.F8                 A MULTIPLE OF 8                 27090000
         AL    R0,&P.#N                COMPUTE NEXT FREE LIFO SLOT ADDR 27100000
         CL    R0,&P.#E                COMPARE TO STACK END ADDRESS     27110000
         BH    &P.GA                    AND IF TOO BIG, BRANCH          27120000
         LR    R15,R1                  PRESERVE MCA ADDRESS             27130000
         USING &P.MCA,R15              NEW BASE                         27140000
         L     R1,&P.#N                LOAD ADDRESS OF SLOT             27150000
         ST    R0,&P.#N                 AND STORE ADDRESS OF NEXT SLOT  27160000
         BR    R14                     RETURN TO CALLER                 27170000
         SPACE 1                                                        27180000
&P.GA    EQU   *                       IF CURRENT SLOT TOO SMALL        27190000
*        ABEND 1000,DUMP                ABEND FOR NOW                   27200000
         ABEND 1000,DUMP                                                27210000
         SPACE 2                                                        27220000
####FREE DS    0H                      LIFO FREE ROUTINE                27230000
         ENTRY ####FREE                                                 27240000
         USING *,R15                   BASE ADDRESS                     27250000
         USING &P.MCA,R1               MCA ADDRESS                      27260000
         CL    R0,&P.#S                CHECK THAT                       27270000
         BL    &P.FA                    ADDRESS TO BE                   27280000
         CL    R0,&P.#E                  FREED IS WITHIN                27290000
         BH    &P.FA                      BOUND OF CURRENT STACK        27300000
         AL    R0,&P.F7                GET UPPER DOUBLE                 27310000
         N     R0,&P.F8                 WORD BOUNDARY                   27320000
         ST    R0,&P.#N                  AND UPDATE MCA                 27330000
         BR    R14                     RETURN TO CALLER                 27340000
         SPACE 1                                                        27350000
&P.FA    EQU   *                       IF ADDRESS NOT WITHIN THIS STACK 27360000
*        ABEND 1001,DUMP               ABEND                            27370000
         ABEND 1001,DUMP                                                27380000
         SPACE 2                                                        27390000
&P.F7    DC    F'7'                    CONSTANTS                        27400000
&P.F8    DC    F'-8'                    TO ROUND UP TO DOUBLEWORD SIZE  27410000
         DROP  R1,R15                  KILL ADDRESSABILITY              27420000
&SYSECT  CSECT                         RESUME MAIN PROGRAM CSECT        27430000
.EXIT    MEND                                                           27440000
./       ADD   NAME=RCPPSWD
         MACRO                                                          29550000
         RCPPSWD &PASSW                                                 29560000
         GBLC  &DYNP                                                    29570000
         SPACE                                                          29580000
*********************************************************************** 29590000
**   BUILD THE PASSWORD TEXT UNIT                                    ** 29600000
*********************************************************************** 29610000
         RCPVCHAR DALPASSW,14,&PASSW                                    29620000
         MEND                                                           29630000
./       ADD   NAME=RCPQNAME
         MACRO                                                          31500000
         RCPQNAME &QNAME                                                31510000
         GBLC  &DYNP                                                    31520000
         SPACE                                                          31530000
*********************************************************************** 31540000
**   BUILD THE QNAME TEXT UNIT                                       ** 31550000
*********************************************************************** 31560000
         RCPVCHAR DALQNAME,14,&QNAME                                    31570000
         MEND                                                           31580000
./       ADD   NAME=RCPRNGE
         MACRO - BREAK A RANGE PARAMETER INTO TWO                       15170000
         RCPRNGE &P                                                     15180000
         GBLC  &RCPRNGE(2)                                              15190000
         LCLA  &I,&J,&K                                                 15200000
&K       SETA  K'&P                                                     15210000
&RCPRNGE(1) SETC ''                                                     15220000
&RCPRNGE(2) SETC ''                                                     15230000
.LOOP    ANOP                                                           15240000
&I       SETA  &I+1                                                     15250000
         AIF   (&I GT &K).NR                                            15260000
         AIF   ('&P'(&I,1) NE '-' AND '&P'(&I,1) NE ':').LOOP           15270000
&RCPRNGE(1) SETC '&P'(1,&I-1)                                           15280000
&RCPRNGE(2) SETC '&P'(&I+1,&K-&I)                                       15290000
         MEXIT                                                          15300000
.NR      ANOP                                                           15310000
&RCPRNGE(1) SETC '&P'                                                   15320000
         MEND                                                           15330000
./       ADD   NAME=RCPSPACE
         MACRO                                                          18340000
         RCPSPACE &SPACE                                                18350000
         GBLA  &RCPSUB#                NO OF SUBLIST ELEMENTS           18360000
         GBLC  &RCPSUBL(100)           SUBLIST ELEMENTS                 18370000
.********************************************************************** 18380000
.*    THIS IS AN ALLOC INNER MACRO TO BUILD THE ALLOCATION SPACE        18390000
.*    QUANTITY TEXT UNIT. IT SHOULD BE SPECIFIED AS:-                   18400000
.*     SPACE=(TYPE,(PRIMARY,SECONDARY,DIRECTORY),RLSE,CONTIG,ROUND)     18410000
.*   WHERE TYPE IS 'TRK', 'CYL', 'ABSTR' OR A BLOCK QUANTITY            18420000
.*     'CYL' OR 'TRK' SHOULD NOT BE ENTERED IN QUOTES. THE BLOCK        18430000
.*     QUANTITY CAN BE A NUMBER, A REGISTER (IN BRACKETS), OR THE       18440000
.*     NAME OF A FULLWORD CONTAINING THE BLOCK SIZE.                    18450000
.********************************************************************** 18460000
         AIF   ('&SPACE(1)' EQ '' OR '&SPACE(1)' EQ 'TRK').TRK          18470000
         AIF   ('&SPACE(1)' EQ 'CYL').CYL                               18480000
*********************************************************************** 18490000
**        SPACE UNIT IN BLOCKS                                       ** 18500000
*********************************************************************** 18510000
         RCPNTU DALBLKLN,3,&SPACE(1)  GENERATE BLOCK UNIT TU            18520000
         AGO   .TPRIME        GO TEST PRIME QUANTITY                    18530000
.TRK     ANOP  TRACK SPEC REQ OR DEFAULTED                              18540000
         SPACE                                                          18550000
*********************************************************************** 18560000
**       SPACE QUANTITY IN TRACKS                                    ** 18570000
*********************************************************************** 18580000
         MVI   S99TUKEY+1,DALTRK       SET TEXT UNIT KEY                18590000
         RCPDINC 4                                                      18600000
         AGO   .TPRIME                                                  18610000
.CYL     ANOP  CYL QUANTITY                                             18620000
         SPACE 1                                                        18630000
*********************************************************************** 18640000
**      SPACE UNIT IN CYLINDERS                                      ** 18650000
*********************************************************************** 18660000
         MVI   S99TUKEY+1,DALCYL       SET TEXT UNIT KEY                18670000
         RCPDINC 4                     STORE TEXT UNIT ADDR             18680000
.TPRIME  RCPSUBL &SPACE(2)             BREAK UP SUBLIST                 18690000
         AIF   (&RCPSUB# EQ 0).TCONTIG                                  18700000
         AIF   ('&RCPSUBL(1)' EQ '').TSP2                               18710000
         SPACE                                                          18720000
*********************************************************************** 18730000
**       PRIMARY SPACE QUANTITY                                      ** 18740000
*********************************************************************** 18750000
         RCPNTU DALPRIME,3,&RCPSUBL(1)                                  18760000
.TSP2    AIF   (&RCPSUB# LT 2).TCONTIG                                  18770000
         AIF   ('&RCPSUBL(2)' EQ '').TSP3                               18780000
         SPACE                                                          18790000
*********************************************************************** 18800000
**       SECONDARY SPACE QUANTITY                                    ** 18810000
*********************************************************************** 18820000
         RCPNTU DALSECND,3,&RCPSUBL(2)                                  18830000
.TSP3    AIF   (&RCPSUB# LT 3).TCONTIG                                  18840000
         AIF   ('&RCPSUBL(3)' EQ '').TCONTIG                            18850000
         SPACE                                                          18860000
*********************************************************************** 18870000
**       DIRECTORY BLOCK QUANTITY                                    ** 18880000
*********************************************************************** 18890000
         RCPNTU DALDIR,3,&RCPSUBL(3)                                    18900000
.TCONTIG AIF  ('&SPACE(3)' EQ 'CONTIG' OR '&SPACE(4)' EQ 'CONTIG').CON  18910000
         AIF   ('&SPACE(3)' EQ 'MXIG' OR '&SPACE(4)' EQ 'MXIG').MXIG    18920000
         AIF   ('&SPACE(3)' EQ 'ALX' OR '&SPACE(4)' EQ 'ALX').ALX       18930000
.TRLSE   AIF   ('&SPACE(3)' EQ 'RLSE' OR '&SPACE(4)' EQ 'RLSE').RLSE    18940000
.TROUND  AIF   ('&SPACE(4)'EQ'ROUND'OR'&SPACE(5)'EQ'ROUND').ROUND       18950000
         MEXIT                                                          18960000
.CON     ANOP                                                           18970000
*********************************************************************** 18980000
**      CONTIGUOUS SPACE TEXT UNIT                                   ** 18990000
*********************************************************************** 19000000
         RCPNTU DALSPFRM,1,8                                            19010000
         AGO   .TRLSE                                                   19020000
.MXIG    ANOP                                                           19030000
*********************************************************************** 19040000
**       MAXIMUM CONTIGUOUS SPACE TEXT UNIT                          ** 19050000
*********************************************************************** 19060000
         RCPNTU DALSPFRM,1,4                                            19070000
         AGO   .TRLSE                                                   19080000
.ALX     ANOP                                                           19090000
*********************************************************************** 19100000
**       'ALX' SPACE TEXT UNIT                                       ** 19110000
*********************************************************************** 19120000
         RCPNTU DALSPFRM,1,2                                            19130000
         AGO   .TRLSE                                                   19140000
.RLSE    ANOP                                                           19150000
*********************************************************************** 19160000
**      RELEASE UNUSED SPACE TEXT UNIT                               ** 19170000
*********************************************************************** 19180000
         MVI   S99TUKEY+1,DALRLSE      SET TEXT UNIT KEY                19190000
         RCPDINC 4                                                      19200000
         AGO   .TROUND                                                  19210000
.ROUND   ANOP                                                           19220000
*********************************************************************** 19230000
**      RELEASE UNUSED SPACE TEXT UNIT                               ** 19240000
*********************************************************************** 19250000
         MVI   S99TUKEY+1,DALROUND     MOVE IN TEXT UNIT KEY            19260000
         RCPDINC 4                                                      19270000
         MEND                                                           19280000
./       ADD   NAME=RCPSPEC
         MACRO - SET UP USER DEFINED TEXT UNIT                          22840000
         RCPSPEC &T                                                     22850000
         LCLA  &I,&J                                                    22860000
&I       SETA  1                                                        22870000
&J       SETA  K'&T                                                     22880000
         SPACE                                                          22890000
*********************************************************************** 22900000
**       PROCESS SPECIAL TEXT UNITS                                  ** 22910000
*********************************************************************** 22920000
.LOOP    RCPVCHAR &T(&I),&T(&I+2),&T(&I+3),N=&T(&I+1)                   22930000
&I       SETA  &I+4                                                     22940000
         AIF   (&I LE &J).LOOP                                          22950000
         MEND                                                           22960000
./       ADD   NAME=RCPSR2
         MACRO                                                          27460000
         RCPSR2 &A                                                      27470000
         GBLB  &RCPSR2                                                  27480000
         GBLC  &DYNP                                                    27490000
         LCLC  &C                                                       27500000
.*   TO SAVE REG 2 IN REG 0 FOR ALLOC INNER MACROS FIRST TIME ONLY      27510000
.*    IF OPERAND SUPPLIED AND SAVE DONE, RESTORES REG 2 AND             27520000
.*    GENERATES MOVE INSTRUCTION FOR EXECUTE                            27530000
         AIF   ('&A' NE '').UNSAVE                                      27540000
         AIF   (&RCPSR2).EXIT                                           27550000
&RCPSR2  SETB  1                                                        27560000
         LR    R0,R2                   SAVE CONTENTS OF REGISTER 2      27570000
         MEXIT                                                          27580000
.UNSAVE  AIF   (NOT &RCPSR2).EXIT                                       27590000
         B     *+10                    SKIP NEXT INSTRUCTION            27600000
&C       SETC  '&DYNP.MVC'                                              27610000
&C       MVC   S99TUPAR(0),0(R14)      EXECUTED MOVE                    27620000
         LR    R2,R0                   RESTORE CONTENTS OF REGISTER 2   27630000
&RCPSR2  SETB  0                                                        27640000
.EXIT    MEND                                                           27650000
./       ADD   NAME=RCPSSREQ
         MACRO                                                          29650000
         RCPSSREQ                                                       29660000
         SPACE 1                                                        29670000
*********************************************************************** 29680000
**       SUBSYSTEM REQUEST TEXT UNIT                                 ** 29690000
*********************************************************************** 29700000
         SPACE 1                                                        29710000
         MVI   S99TUKEY+1,DALSSREQ MOVE IN TEXT UNIT KEY                29720000
         RCPDINC                   4                                    29730000
         MEND                                                           29740000
./       ADD   NAME=RCPSUBL
         MACRO - BREAK DOWN A SUBLIST                                   31600000
         RCPSUBL &L                                                     31610000
         GBLA  &RCPSUB#                NO OF ELEMENTS FOUND             31620000
         GBLC  &RCPSUBL(100)           ELEMENTS                         31630000
         LCLA  &I,&J,&K                                                 31640000
&RCPSUB# SETA  0                       INITIALIZE                       31650000
         AIF   ('&L' EQ '').EXIT       EXIT IF NULL STRING              31660000
         AIF   ('&L'(1,1) NE '(').NOSUB                                 31670000
&K       SETA  K'&L-1                                                   31680000
&I       SETA  2                                                        31690000
&J       SETA  1                                                        31700000
.LOOP    ANOP                                                           31710000
&J       SETA  &J+1                                                     31720000
         AIF   (&J  GT &K).LAST                                         31730000
         AIF   ('&L'(&J,1) NE ',').LOOP                                 31740000
&RCPSUB# SETA &RCPSUB#+1                                                31750000
         AIF   (&J EQ &I).NULL                                          31760000
&RCPSUBL(&RCPSUB#) SETC '&L'(&I,&J-&I)                                  31770000
&I       SETA  &J+1                                                     31780000
         AGO   .LOOP                                                    31790000
.NULL    ANOP                                                           31800000
&RCPSUBL(&RCPSUB#) SETC ''                                              31810000
&I       SETA  &J+1                                                     31820000
         AGO   .LOOP                                                    31830000
.LAST    AIF   (&J EQ &I).LASTNUL                                       31840000
&RCPSUB# SETA  &RCPSUB#+1                                               31850000
&RCPSUBL(&RCPSUB#) SETC '&L'(&I,&J-&I)                                  31860000
         AGO   .EXIT                                                    31870000
.LASTNUL ANOP                                                           31880000
&RCPSUB# SETA  &RCPSUB#+1                                               31890000
&RCPSUBL(&RCPSUB#) SETC ''                                              31900000
         AGO   .EXIT                                                    31910000
.NOSUB   ANOP                                                           31920000
&RCPSUBL(1) SETC '&L'                                                   31930000
&RCPSUB# SETA 1                                                         31940000
.EXIT    MEND                                                           31950000
./       ADD   NAME=RCPSYSOU
         MACRO                                                          15350000
         RCPSYSOU &CLASS,&COPIES=,&FREE=,&DEST=,&FORMS=                 15360000
         GBLC  &DYNP                                                    15370000
         LCLC  &C                                                       15380000
         AIF   ('&CLASS(1)' EQ '').TPGN                                 15390000
&C       SETC  '&CLASS(1)'                                              15400000
         SPACE                                                          15410000
*********************************************************************** 15420000
**       SYSOUT CLASS TEXT UNIT                                      ** 15430000
*********************************************************************** 15440000
         AIF   ('&C'(1,1) EQ '''').Q                                    15450000
         AIF   ('&C'(K'&C,1) EQ '/').BS                                 15460000
         AIF   ('&C'(1,1) EQ '(').REG                                   15470000
         L     R14,&C                  LOAD ADDRESS OF SYSOUT CLASS     15480000
         MVC   S99TUPAR(1),0(R14)       AND MOVE IT TO TEXT UNIT        15490000
         AGO   .SKEY                                                    15500000
.REG     MVC   S99TUPAR(1),0&C         MOVE SYSOUT CLASS TO TEXT UNIT   15510000
.SKEY    MVI   S99TUKEY+1,DALSYSOU     SET SYSOUT KEY                   15520000
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 15530000
         MVI   S99TULNG+1,1            SET LENGTH FIELD                 15540000
         RCPDINC 8                                                      15550000
         AGO   .TPGN                                                    15560000
.BS      RCPTUBFR DALSYSOU,14,&C                                        15570000
         AGO   .TPGN                                                    15580000
.Q       RCPBTU DALSYSOU,1,&C                                           15590000
.TPGN    AIF   ('&CLASS(2)' EQ '').TCOP                                 15600000
         SPACE                                                          15610000
*********************************************************************** 15620000
**   SYSOUT PROGRAM NAME TEXT UNIT                                   ** 15630000
*********************************************************************** 15640000
&C       SETC  '&CLASS(2)'                                              15650000
         RCPVCHAR DALSPGNM,14,&C                                        15660000
.TCOP    AIF   ('&COPIES' EQ '').TFREE                                  15670000
         SPACE                                                          15680000
*********************************************************************** 15690000
**    SYSOUT COPIES TEXT UNIT                                        ** 15700000
*********************************************************************** 15710000
         RCPNTU DALCOPYS,1,&COPIES                                      15720000
.TFREE   AIF   ('&FREE' EQ '').TDEST                                    15730000
         SPACE                                                          15740000
*********************************************************************** 15750000
**     FREE = CLOSE TEXT UNIT                                        ** 15760000
*********************************************************************** 15770000
         AIF   ('&FREE' EQ 'CLOSE').CLOSEOK                             15780000
         MNOTE 4,' **** FREE=&FREE INVALID, FREE=CLOSE USED'            15790000
.CLOSEOK MVI   S99TUKEY+1,DALCLOSE     MOVE IN TEXT UNIT KEY            15800000
         RCPDINC 4                                                      15810000
.TDEST   AIF   ('&DEST' EQ '').TFORMS                                   15820000
         SPACE                                                          15830000
*********************************************************************** 15840000
**       SYSOUT DESTINATION TEXT UNIT                                ** 15850000
*********************************************************************** 15860000
         RCPVCHAR DALSUSER,14,&DEST                                     15870000
.TFORMS  AIF   ('&FORMS' EQ '').EXIT                                    15880000
         SPACE                                                          15890000
*********************************************************************** 15900000
**     SYSOUT FORMS NUMBER TEXT UNIT                                 ** 15910000
*********************************************************************** 15920000
         RCPVCHAR DALSFMNO,14,&FORMS                                    15930000
.EXIT    MEND                                                           15940000
./       ADD   NAME=RCPTU
         MACRO                                                          19300000
         RCPTU &TU            TEXT UNIT LIST                            19310000
         GBLA  &DTUPO         TEXT UNIT POINTER OFFSET                  19320000
         GBLC  &DYNP          ALLOC SYMBOL PREFIX                       19330000
         LCLA  &I,&J                                                    19340000
         LCLC  &C                                                       19350000
         SPACE 1                                                        19360000
*********************************************************************** 19370000
**       ADD SPECIAL TEXT UNITS                                      ** 19380000
*********************************************************************** 19390000
&J       SETA  N'&SYSLIST                                               19400000
.LOOP    ANOP                                                           19410000
&I       SETA  &I+1                                                     19420000
         AIF   (&I GT &J).EXIT                                          19430000
         AIF   ('&TU(&I)'(1,1) EQ '(').R                                19440000
         LA    R15,&TU(&I)             LOAD TEXT UNIT ADDRESS           19450000
         ST    R15,&DYNP.TUP+&DTUPO     AND STORE IT IN POINTER LIST    19460000
&DTUPO   SETA  &DTUPO+4                                                 19470000
         AGO   .LOOP                                                    19480000
.R       ANOP                                                           19490000
&C       SETC  '&TU(&I)'(2,K'&TU(&I)-2)                                 19500000
         ST    &C,&DYNP.TUP+&DTUPO     STORE TEXT UNIT ADDR IN PTR LIST 19510000
&DTUPO   SETA  &DTUPO+4                                                 19520000
         AGO   .LOOP                                                    19530000
.EXIT    MEND                                                           19540000
./       ADD   NAME=RCPTUBFR
         MACRO  - BUILD TEXT UNIT FROM BUFFER                           22980000
         RCPTUBFR &KEY,                TEXT UNIT KEY                   X22990000
               &L,                     MAXIMUM LENGTH VALUE            X23000000
               &C,                     TEXT UNIT                       X23010000
               &N=1                    TEXT UNIT NUMBER                 23020000
         GBLC  &EXECNAM                                                 23030000
         LCLC  &C1,&C2                                                  23040000
         LCLA  &I,&K                                                    23050000
         MVI   S99TUKEY+1,&KEY         SET TEXT UNIT KEY                23060000
         AIF   ('&N' EQ '' OR '&N' EQ '1').N1                           23070000
         LA    R14,&N                  LOAD TEXT UNIT NUMBER            23080000
         STH   R14,S99TUNUM             AND STORE INTO TEXT UNIT        23090000
         AGO   .ENDN                                                    23100000
.N1      MVI   S99TUNUM+1,1            SET TEXT UNIT NUMBER             23110000
.ENDN    ANOP                                                           23120000
&K       SETA  K'&C                                                     23130000
&I       SETA  &K-1                                                     23140000
.LOOP1   ANOP                                                           23150000
&K       SETA  &K-1                                                     23160000
         AIF   (&K LE 0).STD                                            23170000
         AIF   ('&C'(&K,1) NE '/').LOOP1                                23180000
&C2      SETC  '&C'(&K+1,&I-&K)                                         23190000
&C1      SETC  '&C'(1,&K-1)                                             23200000
         AIF   ('&C1'(1,1) NE '(').TC2                                  23210000
&C1      SETC  '0&C1'                                                   23220000
.TC2     AIF   ('&C2' EQ '0000').V2B                                    23230000
         AIF   ('&C2' EQ '00').V1B                                      23240000
         AIF   ('&C2' EQ '0').V0B                                       23250000
         AIF   ('&C2'(1,1) EQ '(').RL                                   23260000
         MVI   S99TULNG+1,&C2          SET LENGTH FIELD                 23270000
         MVC   S99TUPAR(&C2.),&C1      MOVE IN TEXT UNIT                23280000
         RCPDINC &L                                                     23290000
         MEXIT                                                          23300000
.STD     ANOP                                                           23310000
&K       SETA  &L-6                                                     23320000
         MVI   S99TULNG+1,&K           SET TEXT UNIT LENGTH             23330000
&C1      SETC  '&C'(1,&I)              REMOVE TRAILING SLASH            23340000
         MVC   S99TUPAR(&K),&C1        MOVE IN TEXT UNIT                23350000
         RCPDINC &L                                                     23360000
         MEXIT                                                          23370000
.V2B     LH    R14,&C1                 LOAD TEXT UNIT LENGTH            23380000
         S     R14,=A(4)               EXCLUDE LENGTH OF HEADER         23390000
&C1      SETC  '4+&C1'                                                  23400000
         AGO   .MOVE                                                    23410000
.V1B     LH    R14,&C1                 LOAD TEXT UNIT LENGTH            23420000
&C1      SETC  '2+&C1'                                                  23430000
         AGO   .MOVE                                                    23440000
.V0B     SLR   R14,R14                 CLEAR FOR IC                     23450000
         IC    R14,&C1                 INSERT TEXT UNIT LENGTH          23460000
&C1      SETC  '1+&C1'                                                  23470000
         AGO   .MOVE                                                    23480000
.RL      ANOP                                                           23490000
&C2      SETC  '&C2'(2,K'&C2-2)                                         23500000
         LR    R14,&C2                 LOAD TEXT UNIT LENGTH            23510000
.MOVE    STH   R14,S99TULNG             AND STORE INTO LENGTH FIELD     23520000
         BCTR  R14,0                   GET MACHINE LENGTH               23530000
         EXECUTE ,MVC,S99TUPAR-S99TUNIT(0,R15),&C1                      23540000
         EX    R14,&EXECNAM            MOVE IN TEXT UNIT                23550000
         RCPDINC &L                                                     23560000
         MEND                                                           23570000
./       ADD   NAME=RCPTXTL
         MACRO - TO COUNT CHARACTERS IN A STRING                        27670000
         RCPTXTL &S                                                     27680000
         GBLA  &RCPTXTL                                                 27690000
         LCLA  &I,&K,&L                                                 27700000
&RCPTXTL SETA  0                                                        27710000
         AIF   (K'&S LT 3).MEND                                         27720000
&RCPTXTL SETA  K'&S-2                                                   27730000
&L       SETA  &RCPTXTL                                                 27740000
&I       SETA  1                                                        27750000
.LOOP    ANOP                                                           27760000
&I       SETA  &I+1                                                     27770000
.LOOP2   AIF   (&I GT &L).MEND                                          27780000
         AIF   ('&S'(&I,2) NE '''''' AND '&S'(&I,2) NE '&&').LOOP       27790000
&I       SETA  &I+2                                                     27800000
&RCPTXTL SETA  &RCPTXTL-1                                               27810000
         AGO   .LOOP2                                                   27820000
.MEND    MEND                                                           27830000
./       ADD   NAME=RCPTYPE
         MACRO                                                          29760000
         RCPTYPE &T                                                     29770000
         GBLC  &RCPTYPE                                                 29780000
         LCLA  &I,&K                                                    29790000
&K       SETA  K'&T                                                     29800000
&RCPTYPE SETC  ''                                                       29810000
         AIF   (&K EQ 0).EXIT                                           29820000
&RCPTYPE SETC  'C'                                                      29830000
.LOOP    ANOP                                                           29840000
&I       SETA  &I+1                                                     29850000
         AIF   ('&T'(&I,1) LT '0' OR '&T'(&I,1) GT '9').EXIT            29860000
         AIF   (&I LT &K).LOOP                                          29870000
&RCPTYPE SETC  'N'                                                      29880000
.EXIT    MEND                                                           29890000
./       ADD   NAME=RCPUNALC
         MACRO                                                          31970000
         RCPUNALC                                                       31980000
         SPACE 1                                                        31990000
*********************************************************************** 32000000
**     FREE EVEN IF PERMANENTLY ALLOCATED                            ** 32010000
*********************************************************************** 32020000
         MVI   S99TUKEY+1,DUNUNALC     SET TEXT UNIT KEY                32030000
         RCPDINC  4                                                     32040000
         MEND                                                           32050000
./       ADD   NAME=RCPUNIT
         MACRO                                                          15960000
         RCPUNIT &U,&V                                                  15970000
         GBLC  &DYNP                                                    15980000
         AIF   ('&U' EQ '').TVOL                                        15990000
         SPACE 1                                                        16000000
*********************************************************************** 16010000
**       UNIT NAME TEXT UNIT                                         ** 16020000
*********************************************************************** 16030000
         RCPVCHAR DALUNIT,14,&U                                         16040000
.TVOL    AIF   ('&V' EQ '').EXIT                                        16050000
         SPACE 1                                                        16060000
*********************************************************************** 16070000
**       VOLUME SERIAL TEXT UNIT                                     ** 16080000
*********************************************************************** 16090000
         RCPVCHAR DALVLSER,14,&V                                        16100000
.EXIT    MEND                                                           16110000
./       ADD   NAME=RCPVCHAR
         MACRO                                                          19560000
         RCPVCHAR &KEY,&LEN,&C,&N=1                                     19570000
         GBLC  &DYNP                                                    19580000
         AIF   ('&C'(K'&C,1) EQ '/').BM                                 19590000
         AIF   ('&C'(1,1) EQ '''').QM                                   19600000
         RCPSR2                                                         19610000
         AIF   ('&C'(1,1) EQ '(').RM                                    19620000
         LH    R2,&C+4                 LOAD LENGTH OF TEXT UNIT         19630000
         LTR   R2,R2                   TEST FOR ZERO                    19640000
         BZ    *+30                    IF NO TEXT UNIT, SKIP            19650000
         L     R14,&C                  LOAD ADDRESS OF TEXT UNIT        19660000
         AGO   .STHM                                                    19670000
.RM      LH    R2,4&C                  LOAD LENGTH OF TEXT UNIT         19680000
         LTR   R2,R2                   AND TEST FOR ZERO                19690000
         BZ    *+30                    IF NO TEXT UNIT, SKIP            19700000
         L     R14,0&C                 LOAD ADDRESS OF TEXT UNIT        19710000
.STHM    STH   R2,S99TULNG             STORE LENGTH OF TEXT UNIT        19720000
         BCTR  R2,0                    DECREMENT FOR EXECUTE            19730000
         EX    R2,&DYNP.MVC            MOVE IN TEXT UNIT                19740000
         MVI   S99TUKEY+1,&KEY         MOVE IN TEXT UNIT KEY            19750000
         AIF   ('&N' EQ '1' OR '&N' EQ '').N1                           19760000
         LA    R14,&N                  LOAD TEXT UNIT NUMBER            19770000
         STH   R14,S99TUNUM             AND STORE IT IN TEXT UNIT       19780000
         AGO   .ENDN                                                    19790000
.N1      MVI   S99TUNUM+1,1            SET NUMBER FIELD                 19800000
.ENDN    RCPDINC &LEN                                                   19810000
         MEXIT                                                          19820000
.BM      RCPTUBFR &KEY,&LEN,&C                                          19830000
         MEXIT                                                          19840000
.QM      RCPBTU &KEY,&N,&C                                              19850000
         MEND                                                           19860000
./       ADD   NAME=RCPVOLRT
         MACRO                                                          23590000
         RCPVOLRT                                                       23600000
         SPACE 1                                                        23610000
*********************************************************************** 23620000
**    VOLUME SERIAL RETURN TEXT UNIT                                 ** 23630000
*********************************************************************** 23640000
         MVI   S99TUKEY+1,DALRTVOL     SET RETURN VOLUME SERIAL KEY     23650000
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 23660000
         MVI   S99TULNG+1,8            SET LENGTH FIELD                 23670000
         MVC   S99TUPAR(8),=CL8' '     INITIALIZE FIELD TO BLANKS       23680000
         RCPDINC 14                                                     23690000
         MEND                                                           23700000
./       ADD   NAME=REGS
R0       EQU   0        *USED BY O.S.                                   03770000
R1       EQU   1        *USED BY O.S. // ADDRESS OF PARAMETER LIST      03780000
R2       EQU   2                                                        03790000
R3       EQU   3                                                        03800000
R4       EQU   4                                                        03810000
R5       EQU   5                                                        03820000
R6       EQU   6                                                        03830000
R7       EQU   7                                                        03840000
R8       EQU   8                                                        03850000
R9       EQU   9                                                        03860000
R10      EQU   10                                                       03870000
R11      EQU   11                                                       03880000
R12      EQU   12                                                       03890000
R13      EQU   13       *USED BY O.S. // SAVE-AREA ADDRESS              03900000
R14      EQU   14       *USED BY O.S. // RETURN ADDRESS                 03910000
R15      EQU   15       *USED BY O.S. // ENTRY-PT ADDR, RETURN CODE     03920000
./       ADD   NAME=S99FAIL
         MACRO                                                          32070000
&NAME    S99FAIL &RB=(R14),&RC=(R15),&CPPL=,&MF=G,&CP=                  32080000
         GBLB  &RCPCPPL(2)             CP INDICATOR                     32090000
         GBLC  &RCPPRE                                                  32100000
         LCLB  &GEN                                                     32110000
         LCLC  &C                                                       32120000
&NAME    DS    0H                                                       32130000
         AIF   ('&MF(1)' EQ 'G').GEN                                    32140000
         AIF   ('&MF(1)' EQ 'E').EXEC                                   32150000
         MNOTE 4,'&MF(1) IS AN INVALID MF, MF=G USED'                   32160000
.GEN     LA    R1,FAIL&SYSNDX     LOAD PLIST ADDRESS                    32170000
&GEN     SETB  1                                                        32180000
         AGO   .L                                                       32190000
.EXEC    AIF   ('&MF(2)' NE '').LISTOK                                  32200000
         MNOTE 8,'LIST ADDRESS NOT SPECIFIED'                           32210000
         MEXIT                                                          32220000
.LISTOK  AIF   ('&MF(3)' EQ '').TMF2                                    32230000
&MF(3)   EQU   24                      LENGTH OF PARAMETER LIST         32240000
.TMF2    AIF   ('&MF(2)' EQ '(R1)' OR '&MF(2)' EQ '(1)').L              32250000
         AIF   ('&MF(2)'(1,1) EQ '(').REG                               32260000
         LA    R1,&MF(2)          LOAD DAIRFAIL PARAM LIST ADDRESS      32270000
         AGO   .L                                                       32280000
.REG     ANOP                                                           32290000
&C       SETC  '&MF(2)'(2,K'&MF(2)-2)                                   32300000
         LR    R1,&C              LOAD DAIRFAIL PARAM LIST ADDR         32310000
.L       AIF   ('&RB'(1,1) EQ '(').RBR                                  32320000
         AIF   ('&RB' NE '').RBA                                        32330000
         MNOTE 8,'REQ BLOCK ADDRESS NOT SPECIFIED'                      32340000
         MEXIT                                                          32350000
.RBR     ST    &RB(1),0(R1)       STORE S99 RB ADDRESS                  32360000
         AGO   .RC                                                      32370000
.RBA     LA    R14,&RB            LOAD ADDRESS OF REQ BLOCK             32380000
         ST    R14,0(R1)          AND STORE IN PLIST                    32390000
.RC      AIF   ('&RC'(1,1) EQ '(').RCR                                  32400000
         LA    R14,&RC            LOAD ADDRESS OF RET CODE              32410000
         ST    R14,4(R1)          AND STORE IN PLIST                    32420000
         AGO   .EFF02                                                   32430000
.RCR     ANOP                                                           32440000
.GRC     LA    R14,20(R1)         LOAD ADDR RET CODE FLD                32450000
         ST    &RC(1),0(R14)      STORE RET CODE                        32460000
         ST    R14,4(R1)          AND STORE ITS ADDRESS                 32470000
.EFF02   LA    R14,=A(0)          LOAD ADDR OF FULLWORD OF 0            32480000
         ST    R14,8(R1)          STORE IT.                             32490000
         AIF   ('&CP' EQ 'YES' OR &RCPCPPL(1)).CPID                     32500000
         LA    R14,=X'8032'       LOAD ADDRESS OF CALLERID              32510000
         ST    R14,12(R1)          AND STORE IT                         32520000
         XC    16(4,R1),16(R1)    CLEAR CPPL POINTER                    32530000
         AGO   .GO                                                      32540000
.CPID    LA    R14,=Y(50)         LOAD ADDRESS OF CALLERID              32550000
         ST    R14,12(R1)         AND STORE IT                          32560000
         AIF   ('&CPPL' EQ '').DCPPL                                    32570000
         AIF   ('&CPPL'(1,1) EQ '(').RCPPL                              32580000
         LA    R14,&CPPL          LOAD CPPL ADDRESS                     32590000
         ST    R14,16(R1)          AND STORE IT                         32600000
         AGO   .GO                                                      32610000
.DCPPL   MVC   16(4,R1),&RCPPRE.CPPL MOVE IN CPPL ADDRESS               32620000
         AGO   .GO                                                      32630000
.RCPPL   ST    &CPPL(1),16(R1)    STORE ADDRESS OF CPPL                 32640000
.GO      LINK  EP=IKJEFF18                                              32650000
         AIF   (NOT &GEN).EXIT                                          32660000
         SPACE 1                                                        32670000
         RCPDS                                                          32680000
&C SETC 'FAIL&SYSNDX'                                                   32690000
&C       DS    6F             RESERVE SPACE FOR PARAM LIST              32700000
         RCPDS                                                          32710000
.EXIT    MEND                                                           32720000
./       ADD   NAME=VTCALL
         MACRO                                                          03940000
&LAB     VTCALL &RTN,&TEST                                              03950000
&LAB     LA    R1,VTOCOM      POINT TO THE COMMON AREA                  03960000
         L     R15,VAD&RTN    POINT TO THE ROUTINE                      03970000
         AIF ('&TEST' NE 'TEST').NOTEST                                 03980000
         LTR   R15,R15       SEE IF THE ROUTINE IS PRESENT              03990000
         BZ    *+6           DON'T CALL IT IF IT'S NOT THERE            04000000
.NOTEST  ANOP                                                           04010000
         BALR  R14,R15        THEN CALL IT                              04020000
         MEND                                                           04030000
./       ADD   NAME=VTFMT
         MACRO                                                          04050000
         VTFMT                                                          04060000
*                                                                       04070000
*        THIS DSECT DESCRIBES THE FORMATTED DSCB                        04080000
*                                                                       04090000
VTFMT    DSECT                                                          04100000
VTFNEXT  DS    A              POINTER TO NEXT DSCB                      04110000
VTFALLOC DS    F              ALLOCATION IN UNITS AS DEFINED BY THE     04120000
*                               COMMAND.  KBYTES, MBYTES, TRACKS, OR    04130000
*                               CYLS MAY BE THE UNIT.                   04140000
VTFUSED  DS    F                AMOUNT USED, SAME UNIT                  04150000
VTFUNUSD DS    F                AMOUNT UNUSED, SAME UNIT                04160000
VTFPCT   DS    H                PERCENT USED,  0-100                    04170000
VTFVOLUM DS    CL6            VOLUME SERIAL NUMBER                      04180000
VTFCREDT DS    XL3            CREATION DATE YYDDD                       04190000
VTFEXPDT DS    XL3            EXPIRATION DATE YYDDD                     04200000
VTFLSTAC DS    XL3            LAST ACCESS DATE YYDDD                    04210000
VTFNOEPV DS    AL1            NUMBER OF EXTENTS PER VOLUME              04220000
VTFDSORG DS    CL3            DATA SET ORGANIZATION                     04230000
*                               PS, PO, DA, VS, IS, PERHAPS U           04240000
VTFRECFM DS    CL5            RECORD FORMAT                             04250000
*                               F,V, OR U, B, T, S, A, M                04260000
VTFLRECL DS    H              LOGICAL RECORD LENTGH                     04270000
VTFBLKSZ DS    H              BLOCK SIZE                                04280000
VTFROUND DS    C             R IF ROUND WAS SPECIFIED                   04290000
VTFPROT  DS    C              PASSWORD PROTECTION FLAG                  04300000
VTFCATLG DS    C              CATALOG INDICATION                        04310000
VTFSECAM DS    XL2           SECONDARY AMOUNT                           04320000
VTFSECAL DS    C              SECONDARY ALLOCATION TYPE                 04330000
*                               C FOR CYL, T FOR TRKS, B FOR BLOCKS     04340000
*                               R FOR BLOCKS WITH ROUND                 04350000
VTFDSTYP DS    C              DATA SET TYPE, USER MAY DEFINE            04360000
*                               S = SYSTEM TEMPORARY DATA SET           04370000
*                               T = TEST DATA SET                       04380000
*                               P = PRODUCTION DATA SET                 04390000
VTFACTON DS    CL8            REQUESTED ACTION OR COMMENT               04400000
VTFDSNL  DS    H              LENGTH OF DSNAME                          04410000
VTFMTL   EQU   *-VTFMT        FIXED LENGTH OF THIS DSECT                04420000
VTFDSN   DS    44C            VARIABLE LENGTH FIELD                     04430000
         MEND                                                           04440000
./       ADD   NAME=VTOC
         TITLE 'VTOC COMMAND - LIST DATA SETS AND ATTRIBUTES'           00000100
*********************************************************************** 00000200
*                                                                     * 00000300
*                                                                     * 00000400
* TITLE -      VTOC COMMAND - LIST DATA SETS AND ATTRIBUTES           * 00000500
*                                                                     * 00000600
* FUNCTION -   PROVIDE THE ABILITY FOR A TSO USER OR A BATCH JOB      * 00000700
*              TO LIST THE CONTENTS OF VARIOUS VOLUMES, WITH A        * 00000800
*              FAIR AMOUNT OF SELECTION.                              * 00000900
*                                                                     * 00001000
*                                                                     * 00001100
* OPERATION -  ACCEPT FROM THE TSO USER OR BATCH JOB A COMMAND        * 00001200
*              WITH THE FOLLOWING SYNTAX.  THEN CHECK THE COMMAND     * 00001300
*              AND LOOP THROUGH, GETTING A DSCB, FORMATTING IT,       * 00001400
*              PERFORMING THE DATA SET NAME AND LIMIT CHECKS, AND     * 00001500
*              CALLING AN EXIT ROUTINE IF DESIRED, THEN PUT THE       * 00001600
*              ENTRY IN THE CORRECT SORT SEQUENCE.                    * 00001700
*              FINALLY CALL THE PRINT ROUTINE TO PRINT THE            * 00001800
*              SPECIFIED ITEMS, HEADERS, AND BREAKS, OR JUST          * 00001900
*              THE TOTALS.                                            * 00002000
*                                                                     * 00002100
*                                                                     * 00002200
* INPUT -      STANDARD COMMAND PROCESSOR PARAMETER LIST              * 00002300
*              POINTED TO BY REGISTER 1                               * 00002400
*                                                                     * 00002500
*                                                                     * 00002600
* OUTPUT -     TO SYSOUT, A LIST OF THE REQUESTED DATA SETS AND       * 00002700
*              THEIR ATTRIBUTES.                                      * 00002800
*                                                                     * 00002900
*                                                                     * 00003000
* ATTRIBUTES - REENTRANT, REUSEABLE, REFRESHABLE.                     * 00003100
*                                                                     * 00003200
*                                                                     * 00003300
*         PROGRAMMED BY R. L. MILLER  (415) 485-6241                  * 00003400
*              FIREMAN'S FUND INSURANCE  CPSD 2N                      * 00003500
*              ONE LUCAS GREEN                                        * 00003600
*              SAN RAFAEL, CA  94911                                  * 00003700
*                                                                     * 00003800
*                                                                     * 00003900
*********************************************************************** 00004000
*                                                                       00004100
         MACRO                                                          00004200
&LABEL   VTOCEXCP  &FUNC                                                00004300
         AIF   ('&FUNC' NE 'EQ').CALL                                   00004400
VTCOPEN  EQU   1              DEFINE FUNCTION CODES FOR VTOCEXCP        00004500
VTCCLOSE EQU   2                                                        00004600
VTCREAD  EQU   0                                                        00004700
         MEXIT                                                          00004800
.CALL    ANOP                 CALL VTOCEXCP                             00004900
&LABEL   MVI   VTCEFUNC,VTC&FUNC   SET THE FUNCTION CODE                00005000
         VTCALL EXCP          GO GET A DSCB                             00005100
         MEND                                                           00005200
*                                                                       00005300
*        MACRO FOR INITIALIZING SUBROUTINE WORK AREA ADDRESSES          00005400
*                                                                       00005500
         MACRO                                                          00005600
&LABEL   WORKADDR &RTN,&PRMADDR                                         00005700
&LABEL   L     R1,=A(WORK&RTN-WORKAREA)  GET THE OFFSET ( OVER 4K )     00005800
         LA    R1,0(R1,R13)   RELOCATE IT                               00005900
         ST    R1,&PRMADDR   THEN STORE IT FOR THE ROUTINES             00006000
         MEND                                                           00006100
*                                                                       00006200
         EJECT                                                          00006300
VTOCCMD  ENTERX 12,(1,LENWORK,C)     DO THE HOUSEKEEPING                00006400
         LR    R2,R1          SAVE ADDR OF CPPL                         00006500
         SPACE                                                          00006600
         USING WORKAREA,WORKREG                                         00006700
         EJECT                                                          00006800
         BAL   R14,PARSINIT   PERFORM THE PARSING                       00006900
         LTR   R15,R15        TEST THE RETURN CODE                      00007000
         BNZ   RETURN         BAD NEWS, GET OUT                         00007100
         VTCALL PRNT         INITIALIZE FOR PRINTING                    00007200
         L     R9,ADDRANSR    ADDR OF PARSE DESCRIPTOR LIST             00007300
         USING PDL,R9         RETURNED BY PARSE                         00007400
*                                                                       00007500
*                                                                       00007600
*        SCAN SORT PARSE LIST AND BUILD SORT FIELD TABLE                00007700
*                                                                       00007800
*                                                                       00007900
SORTPAR  LA    R4,SUBSORT     SORT PARSE LIST                           00008000
         LA    R5,SORTTAB     SORT FIELD TABLE                          00008100
         XC    0(64,R5),0(R5) CLEAR SORT FIELD TABLE                    00008200
         MVC   0(4,R5),SORTTABX DEFAULT TO DSNAME                       00008300
         SPACE 1                                                        00008400
SORTPAR1 LA    R1,SORTTABX-12 SORT COMPARE TABLE                        00008500
         SPACE 1                                                        00008600
SORTPAR2 LA    R1,12(0,R1)    POINT TO NEXT COMPARE ENTRY               00008700
         CLC   0(4,R1),=F'0'  END OF TABLE                              00008800
         BE    SORTPAR3       ITEM NOT FOUND, IGNORE                    00008900
         L     R6,0(0,R4)     POINT TO TEXT                             00009000
         LH    R3,4(0,R4)     TEXT LENGTH                               00009100
         LTR   R3,R3          IGNORE IF ZERO                            00009200
         BZ    SORTPAR3                                                 00009300
         BCTR  R3,0                                                     00009400
         EX    R3,SORTCOMP    FIELD NAME MATCH                          00009500
         BE    SORTPAR4       YES                                       00009600
         B     SORTPAR2       NO, TRY NEXT                              00009700
         SPACE 1                                                        00009800
SORTPAR3 ICM   R4,7,9(R4)     NEXT ITEM                                 00009900
         BNZ   SORTPAR1       CONTINUE IF MORE                          00010000
         B     SORTPAR5                                                 00010100
         SPACE 1                                                        00010200
SORTPAR4 MVC   0(4,R5),0(R1)  SET UP SORT FIELD                         00010300
         ICM   R4,7,9(R4)     ASCENDING/DESCENDING INDICATOR            00010400
         BZ    PARMERR        ERROR IF MISSING                          00010500
         L     R6,0(0,R4)     INDICATOR ADDR                            00010600
         CLC   4(2,R4),=F'0'  ERROR IF MISSING                          00010700
         BE    PARMERR                                                  00010800
         MVC   0(1,R5),0(R6)  A/D INDICATOR                             00010900
         LA    R5,4(0,R5)                                               00011000
         CLI   0(R6),C'A'     ASCENDING SORT                            00011100
         BE    SORTPAR3       YES, OK                                   00011200
         CLI   0(R6),C'D'     DESCENDING SORT                           00011300
         BNE   PARMERR        NO, ERROR                                 00011400
         B     SORTPAR3       CHECK IF ANY MORE                         00011500
         SPACE 1                                                        00011600
SORTCOMP CLC   4(0,R1),0(R6)                                            00011700
         SPACE 1                                                        00011800
SORTPAR5 LA    R3,SORTKTAB-12 SORT HEADER INDEX TABLE                   00011900
SORTK1   LA    R3,12(0,R3)    NEXT ENTRY                                00012000
         CLC   0(4,R3),=F'0'  END OF TABLE                              00012100
         BE    SORTK3         YES                                       00012200
         CLC   SORTTAB+1(1),1(R3)  ENTRY MATCH                          00012300
         BNE   SORTK1         NO, CHECK NEXT                            00012400
         SR    R4,R4                                                    00012500
         LH    R5,2(0,R3)     LOAD TABLE LENGTH                         00012600
         D     R4,=F'12'      TABLE ENTRIES                             00012700
         LA    R5,1(0,R5)                                               00012800
         LA    R6,VTCSORTH                                              00012900
         L     R4,4(0,R3)     LOAD TABLE BEGIN ADDR                     00013000
         CLI   SORTTAB,C'D'   DESCENDING SORT                           00013100
         BE    SORTK2         YES                                       00013200
         L     R4,8(0,R3)     LOAD TABLE END ADDR                       00013300
SORTK2   MVC   0(12,R6),0(R4)                                           00013400
         LA    R4,12(0,R4)                                              00013500
         LA    R6,12(0,R6)                                              00013600
         CLI   SORTTAB,C'D'   DESCENDING SORT                           00013700
         BE    *+8            YES                                       00013800
         S     R4,=F'24'                                                00013900
         BCT   R5,SORTK2                                                00014000
         B     SORTK4                                                   00014100
SORTK3   MVC   VTCSORTH(12),=3F'0'                                      00014200
SORTK4   MVC   0(12,R6),=3F'0'                                          00014300
*                                                                       00014400
*        CHECK THROUGH THE UCB'S TO SELECT THE VOLUMES TO PROCESS       00014500
*                                                                       00014600
*                                                                       00014700
**  FIND A VOLUME SERIAL NUMBER                                         00014800
*                                                                       00014900
         LA    R3,VOLS        POINT TO THE PDL                          00015000
LOOP1    L     R5,0(R3)       GET THE ADDRESS OF THE TEXT               00015100
         LH    R4,4(R3)       ALSO GET ITS LENGTH                       00015200
         LTR   R4,R4          FOR EXECUTES, GET THE LENGTH              00015300
         BZ    PHASE2         NO MORE VOLUMES, CONTINUE TO NEXT PHASE   00015400
         BCTR  R4,0           MAKE IT READY FOR THE EX INSTR            00015500
         MVC   VOLSER,BLANKS   INITIALIZE FIELD                         00015600
         EX    R4,MOVVOL                                                00015700
*                                                                       00015800
**  VOLUME FOUND - VERIFY AND CHECK FOR GLOBAL OR SPECIAL REQUESTS      00015900
*                                                                       00016000
         CH    R4,H5          IS THE ENTIRE NAME THERE?                 00016100
         BE    VOLSET         YES, IT'S A SPECIFIC VOLUME               00016200
         MVI   FLAG,X'01'     IT'S A GENERIC REQUEST                    00016300
         CH    R4,H2          CHECK FOR THE ALL KEYWORD, FIRST LENGTH   00016400
         BNE   VOLSET         NOT A GLOBAL REQUEST                      00016500
         CLC   0(3,R5),CHARALV  IS THIS THE KEYWORD 'ALLV'?             00016600
         BE    VOLSETV        NO, NOT A GLOBAL REQUEST                  00016700
         CLC   0(3,R5),CHARALL  IS THIS THE KEYWORD 'ALL'?              00016800
         BNE   VOLSET         NO, NOT A GLOBAL REQUEST                  00016900
         MVI   FLAG,X'02'   GLOBAL REQUEST                              00017000
         B     VOLSET                                                   00017100
*                                                                       00017200
**  FIND THE A(UCB)                                                     00017300
*                                                                       00017400
VOLSETV  MVI   FLAG,X'82'   GLOBAL REQUEST FOR VIRTUAL                  00017500
         B     VOLSET                                                   00017600
VOLSET   XC    LASTADR,LASTADR CLEAR THE UCB COMPARE ADDRESS            00017700
         L     R5,16   A(CVT)                                           00017800
         SR    R6,R6                                                    00017900
         L     R5,40(R5)   A(UCB ADDRESSES)                             00018000
NEXTUCB  ICM   R6,3,0(R5)   A(A UCB)                                    00018100
         LTR   R6,R6   CHECK FOR VALID ENTRIES                          00018200
         BZ    INCR1   UCB HOLE                                         00018300
         C     R6,FMIN1    CHECK FOR END                                00018400
         BE    NOTMNT   END OF UCB LIST - VOLUME NOT FOUND              00018500
         CLI   18(R6),X'20'   MUST BE DIRECT ACCESS                     00018600
         BNE   INCR1                                                    00018700
         C     R6,LASTADR   UCB ADDRESSES MUST INCREASE                 00018800
         BNH   INCR1        OTHERWISE THEY REPEAT.                      00018900
         ST    R6,LASTADR   NEW ADDRESS                                 00019000
         TM    FLAG,X'02'   CHECK FOR GLOBAL                            00019100
         BO    FNDGBL   IT IS                                           00019200
         TM    FLAG,X'01'   CHECK FOR SPECIAL REQUESTS                  00019300
         BO    SPECUCB   IT IS                                          00019400
         CLC   VOLSER,28(R6)   COMPARE FULL VOLSER                      00019500
         BE    FNDUCB   FOUND IT                                        00019600
         B     INCR1                                                    00019700
SPECUCB  EX    R4,CLCVOL   COMPARE FIRST X CHARACTERS ONLY              00019800
         BE    CHKRDY                                                   00019900
INCR1    LA    R5,2(R5)                                                 00020000
         B     NEXTUCB   TRY NEXT UCB                                   00020100
*                                                                       00020200
*        VARIOUS ERRORS, LET THE PERSON KNOW                            00020300
*                                                                       00020400
NOTMNT   TM    FLAG,X'04'     WAS A VOLUME  FOUND?                      00020500
         BO    NEXTVOL        YES, LOOK FOR THE NEXT SPEC               00020600
         MVC   MSGTEXT2,MSGNOTMT  NO, GET THE ERROR MESSAGE             00020700
SETVOL   MVC   MSGTEXT2+5(6),VOLSER ADD THE VOLUME SERIAL NUMBER        00020800
         VTOCMSG MSGTEXT2     AND ISSUE THE MESSAGE                     00020900
         B     NEXTVOL       GO GET THE NEXT VOLUME FROM PARSE          00021000
PENDING  MVC   MSGTEXT2,MSGPEND   SET UP THE MESSAGE                    00021100
*                                                                       00021200
*        SEE IF THIS IS A GENERIC OR GLOBAL REQUEST                     00021300
*                                                                       00021400
         TM    FLAG,X'03'    WAS IT ALL OR A PARTIAL VOLUME SERIAL?     00021500
         BNZ   INCR1         IN EITHER CASE, SKIP THE MESSAGE           00021600
*                            THEN FIND MORE VOLUMES                     00021700
*                                                                       00021800
*        OUTPUT THE OFFLINE PENDING MESSAGE                             00021900
         B     SETVOL         THEN ADD THE VOLUME                       00022000
OFFLINE  MVC   MSGTEXT2,MSGOFFLN SET UP THE MESSAGE                     00022100
         B     SETVOL         THEN ADD THE VOLUME                       00022200
*                                                                       00022300
**  FOR GLOBAL REQUESTS JUST LIST ONLINE PACKS                          00022400
*                                                                       00022500
FNDGBL   TM    3(R6),X'80'   ONLINE BIT                                 00022600
         BZ    INCR1   NOPE                                             00022700
*                                                                       00022800
**  FOR GLOBAL AND SPECIAL REQUESTS, CHECK FOR DEVICE READY             00022900
*                                                                       00023000
CHKRDY   TM    6(R6),X'40'   TEST READY BIT                             00023100
         BO    INCR1   NOT READY                                        00023200
         TM    FLAG,X'80'   GLOBAL REQUEST FOR VIRTUAL                  00023300
         BO    CHKVIRT                                                  00023400
         TM    FLAG,X'02'   GLOBAL REQUEST                              00023500
         BZ    FNDUCB                                                   00023600
         TM    17(R6),X'08'  VIRTUAL UCB                                00023700
         BO    INCR1   YES                                              00023800
         B     FNDUCB                                                   00023900
CHKVIRT  TM    17(R6),X'08'  VIRTUAL UCB                                00024000
         BZ    INCR1   NO                                               00024100
*                                                                       00024200
**  MOVE UCB INFORMATION TO OUTPUT LINE                                 00024300
*                                                                       00024400
FNDUCB   MVC   VOLID,28(R6)   MOVE VOLID                                00024500
         MVC   ADDR,13(R6)   MOVE UNIT ADDRESS                          00024600
         OI    FLAG,X'04'      NOTE THE VOLUME AS FOUND                 00024700
*                                                                       00024800
**  IF OFFLINE, DO NOT PROCESS                                          00024900
*                                                                       00025000
         TM    3(R6),X'40'   PENDING BIT - SHOULD BE OFF                00025100
         BO    PENDING                                                  00025200
         TM    3(R6),X'80'   ONLINE BIT - SHOULD BE ON                  00025300
         BZ    OFFLINE                                                  00025400
*                                                                       00025500
*        NOW GET DSCB'S FROM THE VOLUME                                 00025600
*                                                                       00025700
*                                                                       00025800
*        SET UP THE PARM LIST FOR VTOCEXCP                              00025900
*                                                                       00026000
         VTOCEXCP OPEN        OPEN THE VTOC                             00026100
         LTR   R15,R15        DID IT OPEN OK?                           00026200
         BNE   RETURN         NO, JUST EXIT                             00026300
READDSCB CLI   TABFULL,0     CHECK FOR FULL TABLES                      00026400
         BNE   ENDVTOC       IF FULL, TRY END OF VTOC TO CLEAR          00026500
         VTOCEXCP READ        GET A DSCB                                00026600
         CH    R15,H4         CHECK THE RETURN CODE                     00026700
         BE    ENDVTOC        END OF VTOC                               00026800
         BH    RETURN         BAD ERROR, VTOCEXCP GAVE THE MESSAGE      00026900
*                                                                       00027000
*        CHECK THE DATA SET QUALIFICATIONS, LIMIT, AND, OR              00027100
*                                                                       00027200
         VTCALL CHEK          CALL THE CHECK ROUTINE                    00027300
         LTR   R15,R15        DOES THIS DATA SET GET PASSED ON?         00027400
         BNZ   READDSCB       NO, GET ANOTHER                           00027500
*                             YES, CONTINUE PROCESSING                  00027600
*                                                                       00027700
*        FORMAT THE DSCB INFORMATION                                    00027800
*                                                                       00027900
         TM    VTCFMTCK,VTCFMTCD WAS FORMAT CALLED BY CHECK?            00028000
         BO    CALLEXIT       YES, DON'T CALL IT AGAIN                  00028100
         VTCALL FORM          CALL THE FORMATTING ROUTINE               00028200
         LTR   R15,R15        DID IT FUNCTION?                          00028300
         BNZ   READDSCB       NO, GET ANOTHER DSCB                      00028400
*                                                                       00028500
*        CALL THE EXIT ROUTINE IF ONE WAS SPECIFIED                     00028600
*                                                                       00028700
CALLEXIT VTCALL EXIT,TEST     CALL THE EXIT ROUTINE                     00028800
         LTR   R15,R15        SHOULD THE DATA SET BE PASSED ON?         00028900
         BNZ   READDSCB       NO, GET ANOTHER DSCB                      00029000
*                                                                       00029100
*        SORT THE ENTRIES INTO THE NEW LIST                             00029200
*                                                                       00029300
         VTCALL SORT          CALL THE SORT ROUTINE                     00029400
         B     READDSCB       GET ANOTHER DSCB                          00029500
*                                                                       00029600
*        END OF THE VOLUME, CHECK FOR MORE                              00029700
*                                                                       00029800
ENDVTOC  VTOCEXCP CLOSE FIRST CLOSE THE VTOC                            00029900
*                                                                       00030000
ENDVOL   TM    FLAG,X'03'         IS THIS A GENERIC VOLUME SEARCH       00030100
         BNZ   INCR1              YES, SEARCH FOR MORE                  00030200
NEXTVOL  ICM   R3,B'0111',25(R3)  GET THE NEXT VOLUME FROM THE PDL      00030300
         BP    LOOP1              THERE IS ANOTHER, GET IT              00030400
*                                                                       00030500
*        PRINT THE SELECTED ITEMS FOR THE SELECTED DATA SETS            00030600
*                                                                       00030700
PHASE2   DS    0H                                                       00030800
         VTCALL PRNT          CALL THE PRINT ROUTINE                    00030900
         B     EXIT0                                                    00031000
         EJECT                                                          00031100
*                                                                       00031200
*        PROCESSING IS COMPLETE, EXEUNT                                 00031300
*                                                                       00031400
PARMERR  LA    R15,16                                                   00031500
         B     RETURN                                                   00031600
EXIT0    SR    R15,R15                                                  00031700
         SPACE 3                                                        00031800
RETURN   LTR   R2,R15         NORMAL EXIT?                              00031900
         BZ    RETURN1        YES, LEAVE EVERY THING ALONE              00032000
         SPACE 2                                                        00032100
         LA    R1,PARMLIST    AREA FOR STACK PARM LIST                  00032200
         USING IOPL,R1        AN ERROR WAS FOUND, FLUSH THE STACK       00032300
         SPACE                                                          00032400
         MVC   IOPLUPT,ADDRUPT                                          00032500
         MVC   IOPLECT,ADDRECT                                          00032600
         LA    R0,ATTNECB                                               00032700
         MVI   ATTNECB,0                                                00032800
         ST    R0,IOPLECB                                               00032900
         SPACE 2                                                        00033000
         STACK PARM=PARMLIST+16,DELETE=ALL,MF=(E,(1))                   00033100
         SPACE 3                                                        00033200
         TCLEARQ INPUT        CLEAR INPUT BUFFERS                       00033300
         SPACE 3                                                        00033400
RETURN1  DS    0H                                                       00033500
         BAL   R14,FREEPDL    FREE THE PARSE STROAGE                    00033600
         MVI   VTCEPRNT,15    TELL PRINT TO CLEAN UP HIS ACT            00033700
*                                CLOSE DATA SETS AND FREE MAIN STORAGE  00033800
         VTCALL PRNT          CALL THE PRINT ROUTINE                    00033900
         SPACE                                                          00034000
         LR    R15,R2          GET THE RETURN CODE AGAIN                00034100
         LEAVE EQ                                                       00034200
WORKREG  EQU   13                                                       00034300
*                                                                       00034400
*        PARSE INITIALIZATION                                           00034500
*                                                                       00034600
         SPACE 3                                                        00034700
PARSINIT DS    0H                                                       00034800
         ST    R2,CPPLADDR    AND THE CPPL ADDRESS                      00034900
         USING CPPL,R2        BASE FOR COMMAND PARM LIST                00035000
         MVC   ADDRUPT,CPPLUPT ADDR OF USER PROFILE TABLE               00035100
         MVC   ADDRPSCB,CPPLPSCB                                        00035200
         MVC   ADDRECT,CPPLECT ADDR OF ENVIROMENT TABLE                 00035300
         MVC   ADDRCBUF,CPPLCBUF                                        00035400
         DROP  R2                                                       00035500
         SPACE 3                                                        00035600
*                                                                       00035700
*        PUT THE WORK AREA ADDRESSES INTO THE PARM LISTS                00035800
*                                                                       00035900
         WORKADDR MSG,VTCWMSG     WORK AREA FOR VTOCMSG                 00036000
         WORKADDR EXCP,VTCWEXCP   WORK AREA FOR VTOCEXCP                00036100
         WORKADDR CHEK,VTCWCHEK   WORK AREA FOR VTOCCHEK                00036200
         WORKADDR FORM,VTCWFORM   WORK AREA FOR VTOCFORM                00036300
         WORKADDR EXIT,VTCWEXIT   WORK AREA FOR VTOCEXIT                00036400
         WORKADDR SORT,VTCWSORT   WORK AREA FOR VTOCSORT                00036500
         WORKADDR PRNT,VTCWPRNT   WORK AREA FOR VTOCPRNT                00036600
         SPACE 3                                                        00036700
*        SET UP THE ADDRESSES FOR CALLING                               00036800
*                                                                       00036900
         MVC   VADMSG(RTNADLEN),RTNADDRS  MOVE IN THE ADDRESSES         00037000
*                                                                       00037100
*                                                                       00037200
*                                                                       00037300
*        BUILD PARSE PARAMETER LIST AND INVOKE                          00037400
*        IKJPARS TO ANALYZE COMMAND OPERANDS                            00037500
*                                                                       00037600
         SPACE 3                                                        00037700
GOPARSE  DS    0H                                                       00037800
         ST    R14,R14PARSE   SAVE THE RETURN ADDRESS                   00037900
         LA    R1,PARSELST    AREA FOR PARSE PARAMETERS                 00038000
         USING PPL,R1         BASE FOR PARSE PARAMETER LIST             00038100
         SPACE 2                                                        00038200
         MVC   PPLUPT,ADDRUPT PASS UPT ADDRESS                          00038300
         MVC   PPLECT,ADDRECT AND ECT ADDRESS                           00038400
         MVC   PPLCBUF,ADDRCBUF AND COMMAND BUFFER ADDR                 00038500
         SPACE                                                          00038600
         ST    WORKREG,PPLUWA ALSO WORK AREA ADDR FOR VALIDITY EXITS    00038700
         SPACE                                                          00038800
         LA    R0,ATTNECB     ECB FOR ATTN INTERRUPTS                   00038900
         MVI   ATTNECB,0      CLEAR ECB                                 00039000
         ST    R0,PPLECB      PASSE TO PARSE                            00039100
         SPACE                                                          00039200
         LA    R0,ADDRANSR    PASS ADDR OF WORD WHERE PARSE             00039300
         ST    R0,PPLANS      RETURNS PDL ADDRESS                       00039400
         SPACE                                                          00039500
         MVC   PPLPCL,ADDRPCL STORE PCL ADDRESS                         00039600
         SPACE 3                                                        00039700
         CALLTSSR EP=IKJPARS  INVOKE PARSE                              00039800
         DROP  R1                                                       00039900
         SPACE 2                                                        00040000
         LA    R14,MAXPARSE   RETURN CODE LIMIT                         00040100
         SPACE                                                          00040200
         CR    R15,R14        VERIFY RETURN CODE WITHIN LIMITS          00040300
         BH    PARSEBAD       NO, ERROR                                 00040400
         SPACE                                                          00040500
         B     *+4(R15)       PROCESS RETURN CODE                       00040600
         SPACE                                                          00040700
PARSERET B     PARSEOK         0- SUCESSFUL                             00040800
         B     PARSEERR        4- PARSE UNABLE TO PROMPT                00040900
         B     PARSEERR        8- USER ENTERED ATTENTION                00041000
         B     PARSEBAD       12- INVALID PARAMETERS                    00041100
         B     PARSEBAD       16- PARSE INTERNAL FAILURE                00041200
         B     PARSEERR       20 - VALIDITY CHECK ERROR                 00041300
MAXPARSE EQU   *-PARSERET                                               00041400
         SPACE 5                                                        00041500
PARSEBAD DS    0H                                                       00041600
         MVC   MSGTEXT2+4(L'MSGPARSE),MSGPARSE                          00041700
         LA    R1,MSGTEXT2+4+L'MSGPARSE                                 00041800
         SPACE                                                          00041900
         CVD   R15,DOUBLE                                               00042000
         OI    DOUBLE+7,X'0F'                                           00042100
         UNPK  0(2,R1),DOUBLE                                           00042200
         SPACE                                                          00042300
         LA    R0,MSGTEXT2-2                                            00042400
         SR    R1,R0                                                    00042500
         SLL   R1,16                                                    00042600
         ST    R1,MSGTEXT2                                              00042700
         SPACE 2                                                        00042800
         VTOCMSG MSGCMDER,MSGTEXT2    PUT OUT 'COMMAND ERROR' MSG       00042900
         SPACE 3                                                        00043000
PARSEERR LA    R15,12         ERROR CODE 12 - COMMAND FAILED            00043100
         B     PARSERTN       RETURN FROM PARSE                         00043200
         SPACE                                                          00043300
PARSEOK  SR    R15,R15        CLEAR THE RETURN CODE                     00043400
PARSERTN L     R14,R14PARSE   GET THE RETURN LOCATION                   00043500
         BR    R14            AND GET OUT OF HERE                       00043600
         SPACE                                                          00043700
         EJECT                                                          00043800
*                                                                       00043900
*        PARSE CLEANUP ROUTINE                                          00044000
*                                                                       00044100
         SPACE 3                                                        00044200
FREEPDL  DS    0H                                                       00044300
         SPACE                                                          00044400
         ST    R14,R14SAVE                                              00044500
         SPACE                                                          00044600
         IKJRLSA ADDRANSR     RELEASE THE STORAGE                       00044700
         SPACE 2                                                        00044800
         XC    ADDRANSR,ADDRANSR                                        00044900
         SPACE                                                          00045000
         L     R14,R14SAVE                                              00045100
         BR    R14                                                      00045200
         EJECT                                                          00045300
*                                                                       00045400
*                                                                       00045500
*        CONSTANTS                                                      00045600
*                                                                       00045700
*                                                                       00045800
         LTORG                                                          00045900
RTNADDRS DC    V(VTOCMSG)                                               00046000
         DC    A(0)           DUMMY ENTRY FOR THE EXIT ROUTINE          00046100
         DC    V(VTOCEXCP)                                              00046200
         DC    V(VTOCCHEK)                                              00046300
         DC    V(VTOCFORM)                                              00046400
         DC    V(VTOCPRNT)                                              00046500
         DC    V(VTOCSORT)                                              00046600
RTNADLEN EQU   *-RTNADDRS                                               00046700
ADDRPCL  DC    A(PCLMAIN)     ADDR OF MAIN PARSE CONTROL LIST           00046800
FMIN1    DC    X'0000FFFF'    END OF UCB LIST                           00046900
BLANKS   DC    CL8' '         BALNKS                                    00047000
H2       DC    H'2'                                                     00047100
H4       DC    H'4'                                                     00047200
H5       DC    H'5'                                                     00047300
*                                                                       00047400
*                                                                       00047500
*                                                                       00047600
*                                                                       00047700
*                                                                       00047800
CHARALL  DC    CL3'ALL'                                                 00047900
CHARALV  DC    CL3'ALV'                                                 00048000
MOVVOL   MVC   VOLSER(0),0(R5)                                          00048100
CLCVOL   CLC   VOLSER(0),28(R6)                                         00048200
         EJECT                                                          00048300
SORTTABX DC    AL2(VTFDSN-VTFMT),AL2(43),CL8'DSNAME'                    00048400
         DC    AL2(VTFVOLUM-VTFMT),AL2(5),CL8'VOLUME'                   00048500
         DC    AL2(VTFALLOC-VTFMT),AL2(3),CL8'ALLOC'                    00048600
         DC    AL2(VTFUSED-VTFMT),AL2(3),CL8'USED'                      00048700
         DC    AL2(VTFUNUSD-VTFMT),AL2(3),CL8'UNUSED'                   00048800
         DC    AL2(VTFPCT-VTFMT),AL2(1),CL8'PCT'                        00048900
         DC    AL2(VTFNOEPV-VTFMT),AL2(0),CL8'EX'                       00049000
         DC    AL2(VTFDSORG-VTFMT),AL2(2),CL8'DSO'                      00049100
         DC    AL2(VTFRECFM-VTFMT),AL2(4),CL8'RFM'                      00049200
         DC    AL2(VTFLRECL-VTFMT),AL2(1),CL8'LRECL'                    00049300
         DC    AL2(VTFBLKSZ-VTFMT),AL2(1),CL8'BLKSZ'                    00049400
         DC    AL2(VTFCREDT-VTFMT),AL2(2),CL8'CDATE'                    00049500
         DC    AL2(VTFEXPDT-VTFMT),AL2(2),CL8'EXPDT'                    00049600
         DC    AL2(VTFLSTAC-VTFMT),AL2(2),CL8'REFDT'                    00049700
         DC    F'0'                                                     00049800
         EJECT                                                          00049900
*                                                                       00050000
*        PROGRAM MESSAGES                                               00050100
*                                                                       00050200
         SPACE 2                                                        00050300
         PRINT NOGEN                                                    00050400
         SPACE                                                          00050500
MSGPARSE MSG   ' PARSE ERROR CODE '                                     00050600
MSGCMDER MSG   ' COMMAND SYSTEM ERROR'                                  00050700
MSGNOTMT MSG   ' VVVVVV VOLUME IS NOT MOUNTED'                          00050800
MSGOFFLN MSG   ' VVVVVV VOLUME IS OFFLINE'                              00050900
MSGPEND  MSG   ' VVVVVV VOLUME IS PENDING OFFLINE'                      00051000
*                                                                       00051100
*                                                                       00051200
         EJECT                                                          00051300
         DS    0F                                                       00051400
SORTKTAB DC    AL2(VTFDSN-VTFMT),AL2(DSNSORTE-DSNSORT)                  00051500
         DC    A(DSNSORT),A(DSNSORTE)                                   00051600
         DC    AL2(VTFVOLUM-VTFMT),AL2(VOLSORTE-VOLSORT)                00051700
         DC    A(VOLSORT),A(VOLSORTE)                                   00051800
         DC    AL2(VTFUSED-VTFMT),AL2(USESORTE-USESORT)                 00051900
         DC    A(USESORT),A(USESORTE)                                   00052000
         DC    AL2(VTFALLOC-VTFMT),AL2(ALCSORTE-ALCSORT)                00052100
         DC    A(ALCSORT),A(ALCSORTE)                                   00052200
         DC    AL2(VTFUNUSD-VTFMT),AL2(UNUSORTE-UNUSORT)                00052300
         DC    A(UNUSORT),A(UNUSORTE)                                   00052400
         DC    AL2(VTFPCT-VTFMT),AL2(PCTSORTE-PCTSORT)                  00052500
         DC    A(PCTSORT),A(PCTSORTE)                                   00052600
         DC    AL2(VTFNOEPV-VTFMT),AL2(EXTSORTE-EXTSORT)                00052700
         DC    A(EXTSORT),A(EXTSORTE)                                   00052800
         DC    AL2(VTFDSORG-VTFMT),AL2(DSOSORTE-DSOSORT)                00052900
         DC    A(DSOSORT),A(DSOSORTE)                                   00053000
         DC    AL2(VTFRECFM-VTFMT),AL2(RFMSORTE-RFMSORT)                00053100
         DC    A(RFMSORT),A(RFMSORTE)                                   00053200
         DC    AL2(VTFLRECL-VTFMT),AL2(LRCSORTE-LRCSORT)                00053300
         DC    A(LRCSORT),A(LRCSORTE)                                   00053400
         DC    AL2(VTFBLKSZ-VTFMT),AL2(BLKSORTE-BLKSORT)                00053500
         DC    A(BLKSORT),A(BLKSORTE)                                   00053600
         DC    AL2(VTFCREDT-VTFMT),AL2(CDTSORTE-CDTSORT)                00053700
         DC    A(CDTSORT),A(CDTSORTE)                                   00053800
         DC    AL2(VTFLSTAC-VTFMT),AL2(RDTSORTE-RDTSORT)                00053900
         DC    A(RDTSORT),A(RDTSORTE)                                   00054000
         DC    AL2(VTFEXPDT-VTFMT),AL2(EDTSORTE-EDTSORT)                00054100
         DC    A(EDTSORT),A(EDTSORTE)                                   00054200
         DC    2F'0'                                                    00054300
         SPACE 3                                                        00054400
DSNSORT  DC    A(0),AL2(0),CL6'Z'                                       00054500
         DC    A(0),AL2(1),CL6'TV'                                      00054600
         DC    A(0),AL2(1),CL6'TM'                                      00054700
         DC    A(0),AL2(2),CL6'T.Z'                                     00054800
         DC    A(0),AL2(2),CL6'T.Y'                                     00054900
         DC    A(0),AL2(2),CL6'T.X'                                     00055000
         DC    A(0),AL2(2),CL6'T.W'                                     00055100
         DC    A(0),AL2(2),CL6'T.V'                                     00055200
         DC    A(0),AL2(2),CL6'T.U'                                     00055300
         DC    A(0),AL2(2),CL6'T.T'                                     00055400
         DC    A(0),AL2(2),CL6'T.S'                                     00055500
         DC    A(0),AL2(2),CL6'T.R'                                     00055600
         DC    A(0),AL2(2),CL6'T.Q'                                     00055700
         DC    A(0),AL2(2),CL6'T.P'                                     00055800
         DC    A(0),AL2(2),CL6'T.O'                                     00055900
         DC    A(0),AL2(2),CL6'T.N'                                     00056000
         DC    A(0),AL2(2),CL6'T.M'                                     00056100
         DC    A(0),AL2(2),CL6'T.L'                                     00056200
         DC    A(0),AL2(2),CL6'T.K'                                     00056300
         DC    A(0),AL2(2),CL6'T.J'                                     00056400
         DC    A(0),AL2(2),CL6'T.I'                                     00056500
         DC    A(0),AL2(2),CL6'T.H'                                     00056600
         DC    A(0),AL2(2),CL6'T.G'                                     00056700
         DC    A(0),AL2(2),CL6'T.F'                                     00056800
         DC    A(0),AL2(2),CL6'T.E'                                     00056900
         DC    A(0),AL2(2),CL6'T.D'                                     00057000
         DC    A(0),AL2(2),CL6'T.C'                                     00057100
         DC    A(0),AL2(2),CL6'T.B'                                     00057200
         DC    A(0),AL2(2),CL6'T.A'                                     00057300
         DC    A(0),AL2(1),CL6'SY'                                      00057400
         DC    A(0),AL2(1),CL6'SV'                                      00057500
         DC    A(0),AL2(1),CL6'PV'                                      00057600
         DC    A(0),AL2(2),CL6'P.Z'                                     00057700
         DC    A(0),AL2(2),CL6'P.Y'                                     00057800
         DC    A(0),AL2(2),CL6'P.X'                                     00057900
         DC    A(0),AL2(2),CL6'P.W'                                     00058000
         DC    A(0),AL2(2),CL6'P.V'                                     00058100
         DC    A(0),AL2(2),CL6'P.U'                                     00058200
         DC    A(0),AL2(2),CL6'P.T'                                     00058300
         DC    A(0),AL2(2),CL6'P.S'                                     00058400
         DC    A(0),AL2(2),CL6'P.R'                                     00058500
         DC    A(0),AL2(2),CL6'P.Q'                                     00058600
         DC    A(0),AL2(2),CL6'P.P'                                     00058700
         DC    A(0),AL2(2),CL6'P.O'                                     00058800
         DC    A(0),AL2(2),CL6'P.N'                                     00058900
         DC    A(0),AL2(2),CL6'P.M'                                     00059000
         DC    A(0),AL2(2),CL6'P.L'                                     00059100
         DC    A(0),AL2(2),CL6'P.K'                                     00059200
         DC    A(0),AL2(2),CL6'P.J'                                     00059300
         DC    A(0),AL2(2),CL6'P.I'                                     00059400
         DC    A(0),AL2(2),CL6'P.H'                                     00059500
         DC    A(0),AL2(2),CL6'P.G'                                     00059600
         DC    A(0),AL2(2),CL6'P.F'                                     00059700
         DC    A(0),AL2(2),CL6'P.E'                                     00059800
         DC    A(0),AL2(2),CL6'P.D'                                     00059900
         DC    A(0),AL2(2),CL6'P.C'                                     00060000
         DC    A(0),AL2(2),CL6'P.B'                                     00060100
         DC    A(0),AL2(2),CL6'P.A'                                     00060200
         DC    A(0),AL2(0),CL6'N'                                       00060300
DSNSORTE DC    A(0),AL2(0),CL6' '                                       00060400
         SPACE 3                                                        00060500
VOLSORT  DC    A(0),AL2(4),CL6'33509'                                   00060600
         DC    A(0),AL2(4),CL6'33508'                                   00060700
         DC    A(0),AL2(4),CL6'33507'                                   00060800
         DC    A(0),AL2(4),CL6'33506'                                   00060900
         DC    A(0),AL2(4),CL6'33505'                                   00061000
         DC    A(0),AL2(4),CL6'33504'                                   00061100
         DC    A(0),AL2(4),CL6'33503'                                   00061200
         DC    A(0),AL2(4),CL6'33502'                                   00061300
         DC    A(0),AL2(4),CL6'33501'                                   00061400
         DC    A(0),AL2(4),CL6'33500'                                   00061500
         DC    A(0),AL2(4),CL6'33309'                                   00061600
         DC    A(0),AL2(4),CL6'33308'                                   00061700
         DC    A(0),AL2(4),CL6'33307'                                   00061800
         DC    A(0),AL2(4),CL6'33306'                                   00061900
         DC    A(0),AL2(4),CL6'33305'                                   00062000
         DC    A(0),AL2(4),CL6'33304'                                   00062100
         DC    A(0),AL2(4),CL6'33303'                                   00062200
         DC    A(0),AL2(4),CL6'33302'                                   00062300
         DC    A(0),AL2(4),CL6'33301'                                   00062400
         DC    A(0),AL2(4),CL6'33300'                                   00062500
         DC    A(0),AL2(0),CL6'T'                                       00062600
         DC    A(0),AL2(0),CL6'R'                                       00062700
         DC    A(0),AL2(0),CL6'P'                                       00062800
         DC    A(0),AL2(0),CL6'M'                                       00062900
         DC    A(0),AL2(0),CL6'I'                                       00063000
         DC    A(0),AL2(0),CL6'H'                                       00063100
VOLSORTE DC    A(0),AL2(0),CL6' '                                       00063200
         SPACE 3                                                        00063300
USESORT  DS    0F                                                       00063400
UNUSORT  DS    0F                                                       00063500
ALCSORT  DC    A(0),AL2(3),XL4'0000F000',XL2'00'                        00063600
         DC    A(0),AL2(3),XL4'0000C000',XL2'00'                        00063700
         DC    A(0),AL2(3),XL4'0000A000',XL2'00'                        00063800
         DC    A(0),AL2(3),XL4'00008000',XL2'00'                        00063900
         DC    A(0),AL2(3),XL4'00006000',XL2'00'                        00064000
         DC    A(0),AL2(3),XL4'00005000',XL2'00'                        00064100
         DC    A(0),AL2(3),XL4'00004000',XL2'00'                        00064200
         DC    A(0),AL2(3),XL4'00003000',XL2'00'                        00064300
         DC    A(0),AL2(3),XL4'00002000',XL2'00'                        00064400
         DC    A(0),AL2(3),XL4'00001000',XL2'00'                        00064500
         DC    A(0),AL2(3),XL4'00000C00',XL2'00'                        00064600
         DC    A(0),AL2(3),XL4'00000800',XL2'00'                        00064700
         DC    A(0),AL2(3),XL4'00000400',XL2'00'                        00064800
         DC    A(0),AL2(3),XL4'00000300',XL2'00'                        00064900
         DC    A(0),AL2(3),XL4'00000200',XL2'00'                        00065000
         DC    A(0),AL2(3),XL4'00000100',XL2'00'                        00065100
         DC    A(0),AL2(3),XL4'000000C0',XL2'00'                        00065200
         DC    A(0),AL2(3),XL4'00000080',XL2'00'                        00065300
         DC    A(0),AL2(3),XL4'00000040',XL2'00'                        00065400
         DC    A(0),AL2(3),XL4'00000010',XL2'00'                        00065500
USESORTE DS    0F                                                       00065600
UNUSORTE DS    0F                                                       00065700
ALCSORTE DC    A(0),AL2(3),XL6'00'                                      00065800
         SPACE 3                                                        00065900
PCTSORT  DC    A(0),AL2(1),XL2'0064',XL4'00'                            00066000
         DC    A(0),AL2(1),XL2'005A',XL4'00'                            00066100
         DC    A(0),AL2(1),XL2'0050',XL4'00'                            00066200
         DC    A(0),AL2(1),XL2'0046',XL4'00'                            00066300
         DC    A(0),AL2(1),XL2'003C',XL4'00'                            00066400
         DC    A(0),AL2(1),XL2'0032',XL4'00'                            00066500
         DC    A(0),AL2(1),XL2'0028',XL4'00'                            00066600
         DC    A(0),AL2(1),XL2'001E',XL4'00'                            00066700
         DC    A(0),AL2(1),XL2'0014',XL4'00'                            00066800
         DC    A(0),AL2(1),XL2'000A',XL4'00'                            00066900
PCTSORTE DC    A(0),AL2(1),XL6'00'                                      00067000
         SPACE 3                                                        00067100
EXTSORT  DC    A(0),AL2(0),CL6'0'                                       00067200
EXTSORTE DC    A(0),AL2(0),CL6'0'                                       00067300
         SPACE 3                                                        00067400
DSOSORT  DC    A(0),AL2(1),CL6'VS'                                      00067500
         DC    A(0),AL2(1),CL6'PS'                                      00067600
         DC    A(0),AL2(1),CL6'PO'                                      00067700
         DC    A(0),AL2(1),CL6'DA'                                      00067800
DSOSORTE DC    A(0),AL2(1),CL6' '                                       00067900
         SPACE 3                                                        00068000
RFMSORT  DC    A(0),AL2(1),CL6'VS'                                      00068100
         DC    A(0),AL2(2),CL6'VBS'                                     00068200
         DC    A(0),AL2(1),CL6'VB'                                      00068300
         DC    A(0),AL2(0),CL6'V'                                       00068400
         DC    A(0),AL2(0),CL6'U'                                       00068500
         DC    A(0),AL2(1),CL6'FS'                                      00068600
         DC    A(0),AL2(2),CL6'FBS'                                     00068700
         DC    A(0),AL2(1),CL6'FB'                                      00068800
         DC    A(0),AL2(0),CL6'F'                                       00068900
RFMSORTE DC    A(0),AL2(0),CL6' '                                       00069000
         SPACE 3                                                        00069100
LRCSORT  DS    0F                                                       00069200
BLKSORT  DC    A(0),AL2(1),XL2'4650',XL4'00'                            00069300
         DC    A(0),AL2(1),XL2'3A98',XL4'00'                            00069400
         DC    A(0),AL2(1),XL2'2EE0',XL4'00'                            00069500
         DC    A(0),AL2(1),XL2'2328',XL4'00'                            00069600
         DC    A(0),AL2(1),XL2'1770',XL4'00'                            00069700
         DC    A(0),AL2(1),XL2'0BB8',XL4'00'                            00069800
         DC    A(0),AL2(1),XL2'07D0',XL4'00'                            00069900
         DC    A(0),AL2(1),XL2'0640',XL4'00'                            00070000
         DC    A(0),AL2(1),XL2'04B0',XL4'00'                            00070100
         DC    A(0),AL2(1),XL2'0320',XL4'00'                            00070200
         DC    A(0),AL2(1),XL2'0258',XL4'00'                            00070300
         DC    A(0),AL2(1),XL2'0190',XL4'00'                            00070400
         DC    A(0),AL2(1),XL2'00C8',XL4'00'                            00070500
         DC    A(0),AL2(1),XL2'00A0',XL4'00'                            00070600
         DC    A(0),AL2(1),XL2'0078',XL4'00'                            00070700
         DC    A(0),AL2(1),XL2'0050',XL4'00'                            00070800
         DC    A(0),AL2(1),XL2'0028',XL4'00'                            00070900
BLKSORTE DS    0F                                                       00071000
LRCSORTE DC    A(0),AL2(1),XL6'00'                                      00071100
         SPACE 3                                                        00071200
CDTSORT  DS    0F                                                       00071300
EDTSORT  DS    0F                                                       00071400
RDTSORT  DC    A(0),AL2(2),AL1(99),AL2(0),XL3'00'                       00071500
         DC    A(0),AL2(2),AL1(83),AL2(300),XL3'00'                     00071600
         DC    A(0),AL2(2),AL1(83),AL2(200),XL3'00'                     00071700
         DC    A(0),AL2(2),AL1(83),AL2(100),XL3'00'                     00071800
         DC    A(0),AL2(2),AL1(83),AL2(000),XL3'00'                     00071900
         DC    A(0),AL2(2),AL1(82),AL2(300),XL3'00'                     00072000
         DC    A(0),AL2(2),AL1(82),AL2(200),XL3'00'                     00072100
         DC    A(0),AL2(2),AL1(82),AL2(100),XL3'00'                     00072200
         DC    A(0),AL2(2),AL1(82),AL2(000),XL3'00'                     00072300
         DC    A(0),AL2(2),AL1(81),AL2(300),XL3'00'                     00072400
         DC    A(0),AL2(2),AL1(81),AL2(200),XL3'00'                     00072500
         DC    A(0),AL2(2),AL1(81),AL2(100),XL3'00'                     00072600
         DC    A(0),AL2(2),AL1(81),AL2(000),XL3'00'                     00072700
         DC    A(0),AL2(2),AL1(80),AL2(300),XL3'00'                     00072800
         DC    A(0),AL2(2),AL1(80),AL2(200),XL3'00'                     00072900
         DC    A(0),AL2(2),AL1(80),AL2(100),XL3'00'                     00073000
         DC    A(0),AL2(2),AL1(80),AL2(000),XL3'00'                     00073100
         DC    A(0),AL2(2),AL1(79),AL2(300),XL3'00'                     00073200
         DC    A(0),AL2(2),AL1(79),AL2(200),XL3'00'                     00073300
         DC    A(0),AL2(2),AL1(79),AL2(100),XL3'00'                     00073400
         DC    A(0),AL2(2),AL1(79),AL2(000),XL3'00'                     00073500
         DC    A(0),AL2(2),AL1(78),AL2(300),XL3'00'                     00073600
         DC    A(0),AL2(2),AL1(78),AL2(200),XL3'00'                     00073700
         DC    A(0),AL2(2),AL1(78),AL2(100),XL3'00'                     00073800
         DC    A(0),AL2(2),AL1(78),AL2(000),XL3'00'                     00073900
         DC    A(0),AL2(2),AL1(77),AL2(300),XL3'00'                     00074000
         DC    A(0),AL2(2),AL1(77),AL2(200),XL3'00'                     00074100
         DC    A(0),AL2(2),AL1(77),AL2(100),XL3'00'                     00074200
         DC    A(0),AL2(2),AL1(77),AL2(000),XL3'00'                     00074300
         DC    A(0),AL2(2),AL1(76),AL2(300),XL3'00'                     00074400
         DC    A(0),AL2(2),AL1(76),AL2(200),XL3'00'                     00074500
         DC    A(0),AL2(2),AL1(76),AL2(100),XL3'00'                     00074600
         DC    A(0),AL2(2),AL1(76),AL2(000),XL3'00'                     00074700
         DC    A(0),AL2(2),AL1(75),AL2(300),XL3'00'                     00074800
         DC    A(0),AL2(2),AL1(75),AL2(200),XL3'00'                     00074900
         DC    A(0),AL2(2),AL1(75),AL2(100),XL3'00'                     00075000
         DC    A(0),AL2(2),AL1(75),AL2(000),XL3'00'                     00075100
EDTSORTE DS    0F                                                       00075200
RDTSORTE DS    0F                                                       00075300
CDTSORTE DC    A(0),AL2(2),XL6'00'                                      00075400
         EJECT                                                          00075500
*                                                                       00075600
*                                                                       00075700
*        P A R S E   C O N T R O L   L I S T                            00075800
*                                                                       00075900
*                                                                       00076000
         SPACE 3                                                        00076100
         COPY  VTOCPARS                                                 00076200
         EJECT                                                          00076300
*                                                                       00076400
*        DYNAMIC WORK AREA                                              00076500
*                                                                       00076600
         SPACE 3                                                        00076700
WORKAREA DSECT                                                          00076800
MAINSAVE DS    18A                                                      00076900
         SPACE                                                          00077000
         VTOCEXCP EQ          DEFINE VTOCEXCP CODES                     00077100
         SPACE                                                          00077200
PARSELST DS    8A             AREA FOR PARSE PARAMETER LIST             00077300
         SPACE                                                          00077400
R14SAVE  DS    A                                                        00077500
R14PARSE DS    A                                                        00077600
*                                                                       00077700
*        VTOC COMMAND COMMON AREA                                       00077800
*                                                                       00077900
         PRINT GEN                                                      00078000
         VTOCOM  NODSECT                                                00078100
         PRINT NOGEN                                                    00078200
         SPACE 3                                                        00078300
*                                                                       00078400
*        WORK AREAS FOR SUBROUTINES                                     00078500
*                                                                       00078600
WORKMSG  DS    XL256                                                    00078700
WORKEXCP DS    4XL256                                                   00078800
WORKCHEK DS    XL256                                                    00078900
WORKFORM DS    2XL256                                                   00079000
WORKEXIT DS    8XL256                                                   00079100
WORKSORT DS    XL256                                                    00079200
WORKPRNT DS    10XL256                                                  00079300
         DS    0D                                                       00079400
LENWORK  EQU   *-WORKAREA                                               00079500
         SPACE 3                                                        00079600
         VTFMT                                                          00079700
         SPACE 3                                                        00079800
         PDEDSNAM                                                       00079900
         SPACE 3                                                        00080000
         IKJPPL                                                         00080100
         SPACE 3                                                        00080200
         IKJIOPL                                                        00080300
         SPACE 3                                                        00080400
         IKJPSCB                                                        00080500
         SPACE 3                                                        00080600
         IKJECT                                                         00080700
         SPACE 3                                                        00080800
         IKJCPPL                                                        00080900
         SPACE 3                                                        00081000
         IKJUPT                                                         00081100
         SPACE 3                                                        00081200
         PRINT NOGEN                                                    00081300
         CVT                                                            00081400
         END                                                            00081500
./       ADD   NAME=VTOCCHEK
         TITLE 'VTOC COMMAND CHECK  ROUTINE'                            00000100
*********************************************************************** 00000200
*                                                                     * 00000300
*                                                                     * 00000400
* TITLE -      VTOC COMMAND CHECK  ROUTINE                            * 00000500
*                                                                     * 00000600
* FUNCTION -   CHECK THE CONDITIONS SPECIFIED ON THE VTOC COMMAND.    * 00000700
*              SEE IF THE DATA SET PASSED SHOULD BE PROCESSED.        * 00000800
*              THE LIMIT, ENDING, CONTAINING, CCHH, LIMIT, AND,       * 00000900
*              AND OR KEYWORDS ARE PROCESSED BY THIS ROUTINE.         * 00001000
*                                                                     * 00001100
* OPERATION -  FIRST GET THE LENGTH OF THE DSNAME AND SAVE IT.        * 00001200
*              THEN GET THE FORMAT 3 DSCB, IF ONE EXISTS.  TRY        * 00001300
*              EACH KEYWORD TO SEE IF IT WILL EXCLUDE THE DATA        * 00001400
*              SET FROM FURTHER PROCESSING.                           * 00001500
*                                                                     * 00001600
* INPUT -      VTOC COMMON AREA ( VTOCOM )                            * 00001700
*              POINTED TO BY REGISTER 1                               * 00001800
*              USE PARSE DATA, FORMAT 1, 3, AND 4 DSCB'S              * 00001900
*              FOR DSORG, RECFM, ALLOC, USED, PROT, CATLG, OR SECAL   * 00002000
*              CALL VTOCFORM TO FORMAT THE PARMS.  USE FORMATTED DSCB * 00002100
*                                                                     * 00002200
* OUTPUT -     A RETURN CODE OF 0 TO CONTINUE PROCESSING OR 8 TO      * 00002300
*              EXCLUDE THIS DATA SET.                                 * 00002400
*                                                                     * 00002500
* ATTRIBUTES - REENTRANT, REUSEABLE, REFRESHABLE.                     * 00002600
*                                                                     * 00002700
*                                                                     * 00002800
*         PROGRAMMED BY R. L. MILLER  (415) 485-6241                  * 00002900
*                                                                     * 00003000
*                                                                     * 00003100
*********************************************************************** 00003200
         EJECT                                                          00003300
*        MACROS FOR CHECK ROUTINE                                       00003400
*                                                                       00003500
         MACRO                                                          00003600
&LABEL   VTCHL &KEY           CALL THE KEYWORD CHECK ROUTINE            00003700
&LABEL   L     R1,SUB&KEY.OPER    GET THE OPERATOR VALUE                00003800
         BAL   R8,GETOPER    TRANSLATE TO A NUMBER                      00003900
         ST    R15,REFOPER     SAVE IT TOO                              00004000
         LA    R1,SUB&KEY.VALU   GET THE VALUE PDL                      00004100
         ST    R1,REFVAL      SAVE THAT ADDRESS THREE                   00004200
         CLI   FLAGNM&KEY,0  HAS IT BEEN CONVERTED?                     00004300
         BNE   VTP&SYSNDX    YES, SKIP ALONG                            00004400
         MVI   FLAGNM&KEY,1  NOTE IT AS CONVERTED                       00004500
         BAL   R8,PDLNUM     GO CONVERT IT                              00004600
         ST    R15,NUMBER&KEY      SAVE THE VALUE                       00004700
         LA    R4,SUB&KEY.KEY  POINT TO THE IKJIDENT FOR THE KEYWORD    00004800
         BAL   R8,GETKEY     CONVERT TEXT TO A NUMERIC KEY              00004900
         STC   R15,NUMKEY&KEY  SAVE THAT NUMERIC KEY                    00005000
         LTR   R15,R15       WAS IT SUCCESSFUL?                         00005100
         BNZ   VTP&SYSNDX    YES, SKIP ALONG                            00005200
*        ISSUE A MESSAGE - A BAD LIM, AND, OR KEYWORD                   00005300
         MVC   MSGTEXT2,KEYERR  START THE ERROR MESSAGE                 00005400
         L     R1,0(R4)      POINT TO THE TEXT                          00005500
         MVC   MSGTEXT2+49(6),0(R1)  THEN ADD IT TO THE MESSAGE         00005600
         VTOCMSG MSGTEXT2    ISSUE THE ERROR MESSAGE                    00005700
VTP&SYSNDX DS  0H                                                       00005800
         SR    R1,R1         CLEAR A REGISTER                           00005900
         ICM   R1,1,NUMKEY&KEY GET THE KEYWORD VALUE                    00006000
         BZ    VTE&SYSNDX    IF NOT SET, SKIP THE EVALUATION            00006100
         ST    R1,REFKEY      SAVE THE ADDRESS                          00006200
         LA    R1,NUMBER&KEY  GET THE ADDRESS OF CONVERTED NUMBER       00006300
         ST    R1,REFNUM      SAVE IT'S ADDRESS                         00006400
         BAL   R8,LIMEVAL     GO EVALUATE THE EXPRESSION                00006500
VTE&SYSNDX DS  0H                                                       00006600
         MEND                                                           00006700
         SPACE 3                                                        00006800
         MACRO                                                          00006900
&LABEL   VTANDOR &NUM         EVALUATE, THEN DO AND OR OR FUNCTION      00007000
&LABEL   CLI   ANDOR&NUM.K+1,0  WAS THIS KEYWORD SET?                   00007100
         BE    LIMCOMP        NO, JUST CHECK THE FINAL RESULT           00007200
         VTCHL &NUM           YES, EVALUATE                             00007300
         CLI   ANDOR&NUM.K+1,1  WAS IT AN AND ?                         00007400
         BE    VTA&SYSNDX     YES, DO THE AND                           00007500
         O     R15,LIMVAL     NO, OR IT                                 00007600
         B     VTE&SYSNDX     FINISHED WITH THIS EXPRESSION             00007700
VTA&SYSNDX N   R15,LIMVAL     AND THE EXPRESSION VALUE                  00007800
VTE&SYSNDX ST  R15,LIMVAL     SAVE THE VALUE                            00007900
         MEND                                                           00008000
*                                                                       00008100
         EJECT                                                          00008200
VTOCCHEK ENTER 12,12          DO THE HOUSEKEEPING                       00008300
         LR    R11,R1         SAVE ADDR OF VTOCOM                       00008400
         USING VTOCOM,R11     SET ITS ADDRESSABILITY                    00008500
         L     R9,ADDRANSR    POINT TO THE PARSE ANSWER                 00008600
         USING PDL,R9         SET ITS ADDRESSABILITY                    00008700
         USING CHEKWORK,R13   SET ADDRESSABILITY FOR LOCAL WORK AREA    00008800
*                                                                       00008900
*        SEE WHAT THE FORMAT ID IS                                      00009000
*                                                                       00009100
CHEKFMT  L     R7,DSCBADDR    POINT TO THE DSCB                         00009200
         LA    R7,8(R7)       GET PAST THE HEADER                       00009300
         USING DSCB1,R7       SET ADDRESSABILITY                        00009400
         CLI   FORMATK+1,0    DID HE SPECIFY VARIOUS DSCB'S             00009500
         BNE   CHEKFMTI       YES, GO DO HIS CHECKS                     00009600
CHEKFMTI DS    0H             NOT YET PROGRAMMED                        00009700
*                                                                       00009800
*        STANDARD IS ONLY TO ALLOW FORMAT ONES TO GO                    00009900
*                                                                       00010000
         CLI   DS1FMTID,C'1'  IS THIS A FORMAT 1?                       00010100
         BNE   CHECKOUT       NO, EXCLUDE IT FROM FURTHER PROCESSING    00010200
*                                                                       00010300
*              FIRST SEE HOW BIG THE DSNAME IS                          00010400
*                                                                       00010500
         LA    R1,DS1FMTID    POINT PAST THE DSNAME                     00010600
         TRT   DS1DSNAM,BLKTRTAB  FIND THE FIRST BLANK                  00010700
         SR    R1,R7          SUBTRACT TO GET THE LENGTH                00010800
         STH   R1,DSNLEN      SAVE THE DSNAME LENGTH                    00010900
         LR    R3,R1          KEEP THE LENGTH FOR LATER                 00011000
*                                                                       00011100
*        GET THE FORMAT 3 DSCB, IF IT EXISTS                            00011200
*                                                                       00011300
         XC    FMT3,FMT3      CLEAR IT FIRST                            00011400
         CLC   DS1PTRDS,=XL5'0000000000'  IS THERE A FORMAT 3?          00011500
         BE    FMT3NO         NO, SKIP ALONG                            00011600
*                                                                       00011700
*        SET UP THE CAMLST                                              00011800
*                                                                       00011900
         MVC   CAMSEEK(4),CAMSCON  MOVE IN THE FIRST WORD OF CAMLST     00012000
         LA    R1,DS1PTRDS    GET THE CCHHR ADDRESS                     00012100
         ST    R1,CAMSEEK+4   SAVE IT                                   00012200
         LA    R1,VOLID       POINT TO THE VOLUME SERIAL                00012300
         ST    R1,CAMSEEK+8   SAVE IT                                   00012400
         LA    R1,FMT3        POINT TO THE AREA FOR THE DSCB3           00012500
         ST    R1,CAMSEEK+12  SAVE IT                                   00012600
         OBTAIN CAMSEEK       GET THE DSCB                              00012700
         LTR   R15,R15        TEST THE RETURN CODE                      00012800
         BNZ   OBT3ERR        BAD NEWS, ISSUE THE MESSAGE               00012900
*                                                                       00013000
*        PROCESS THE LEVEL KEYWORD                                      00013100
*                                                                       00013200
FMT3NO   CLI   LEVKEY+1,0     WAS LEVEL SPECIFIED?                      00013300
         BE    LEVEND         NO, SKIP ON                               00013400
         LA    R4,LEVEL       YES, POINT TO THE PDE                     00013500
         USING PDEDSNAM,R4    SET ADDRESSABILITY                        00013600
         LR    R2,R7          POINT TO THE START OF THE DSNAME          00013700
LEVNEXT  SR    R5,R5          CLEAR FOR INSERT                          00013800
         ICM   R5,B'0011',PDEDSNL  GET THE DSNAME LENGTH                00013900
         BZ    CHECKOUT       END OF THE LINE, EXCLUDE IT               00014000
         L     R6,PDEDSN      POINT TO THE LEVEL                        00014100
         CR    R3,R5          COMPARE LENGTHS                           00014200
         BL    LEVINC         THIS LEVEL IS LONGER THAN DSN, NO MATCH   00014300
         BCTR  R5,0           MINUS ONE FOR THE EX                      00014400
         EX    R5,COMPARE     CHECK THE LENGTHS                         00014500
         BE    LEVEND         IT MATCHES, ALLOW IT                      00014600
LEVINC   ICM   R4,B'0111',PDEDCHN GET THE NEXT LEVEL PDE POINTER        00014700
         BNZ   LEVNEXT        IF IT'S THERE, KEEP LOOKING               00014800
         B     CHECKOUT       NO MATCHES, EXCLUDE THIS DSNAME           00014900
LEVEND   DS    0H                                                       00015000
         DROP  R4             FINISHED WITH THE PDE                     00015100
*                                                                       00015200
*        PROCESS THE ENDING KEYWORD                                     00015300
*                                                                       00015400
         CLI   ENDKEY+1,0     WAS ENDING SPECIFIED?                     00015500
         BE    ENDEND         NO, SKIP ON                               00015600
         LA    R4,ENDING      YES, POINT TO THE PDE                     00015700
         USING PDEDSNAM,R4    SET ADDRESSABILITY                        00015800
ENDNEXT  SR    R5,R5          CLEAR FOR INSERT                          00015900
         ICM   R5,B'0011',PDEDSNL  GET THE DSNAME LENGTH                00016000
         BZ    CHECKOUT       END OF THE LINE, EXCLUDE IT               00016100
         L     R6,PDEDSN      POINT TO THE ENDING                       00016200
         CR    R3,R5          COMPARE LENGTHS                           00016300
         BL    ENDINC         THIS ENDING IS LONGER THAN DSN, NO MATCH  00016400
         LR    R2,R7          POINT TO THE START OF THE DSNAME          00016500
         AR    R2,R3          POINT TO THE END                          00016600
         SR    R2,R5          BACKUP TO COMPARE THIS LENGTH             00016700
         BCTR  R5,0           MINUS ONE FOR THE EX                      00016800
         EX    R5,COMPARE     CHECK THE LENGTHS                         00016900
         BE    ENDEND         IT MATCHES, ALLOW IT                      00017000
ENDINC   ICM   R4,B'0111',PDEDCHN GET THE NEXT ENDING PDE POINTER       00017100
         BNZ   ENDNEXT        IF IT'S THERE, KEEP LOOKING               00017200
         B     CHECKOUT       NO MATCHES, EXCLUDE THIS DSNAME           00017300
ENDEND   DS    0H                                                       00017400
         DROP  R4             FINISHED WITH THE PDE                     00017500
*                                                                       00017600
*        PROCESS THE CONTAINING KEYWORD                                 00017700
*                                                                       00017800
         CLI   CONTAINK+1,0   WAS CONTAINING SPECIFIED?                 00017900
         BE    CONEND         NO, SKIP ON                               00018000
         LA    R4,CONTAIN     YES, POINT TO THE PDE                     00018100
         USING PDEDSNAM,R4    SET ADDRESSABILITY                        00018200
CONNEXT  SR    R5,R5          CLEAR FOR INSERT                          00018300
         ICM   R5,B'0011',PDEDSNL  GET THE DSNAME LENGTH                00018400
         BZ    CHECKOUT       END OF THE LINE, EXCLUDE IT               00018500
         L     R6,PDEDSN      POINT TO THE CONTAINING                   00018600
         CR    R3,R5          COMPARE LENGTHS                           00018700
         BL    CONINC         THIS CONTAIN IS LONGER THAN DSN, NO MATCH 00018800
         LR    R1,R7          POINT TO THE START OF THE DSNAME          00018900
         AR    R1,R3          POINT TO THE END                          00019000
         SR    R1,R5          BACKUP TO COMPARE THIS LENGTH - LAST ONE  00019100
         LR    R2,R7          POINT TO THE START OF THE DSNAME          00019200
         BCTR  R5,0           MINUS ONE FOR THE EX                      00019300
CONCOMP  EX    R5,COMPARE     CHECK THE LENGTHS                         00019400
         BE    CONEND         IT MATCHES, ALLOW IT                      00019500
         LA    R2,1(R2)       CHECK THE WHOLE DSNAME                    00019600
         CR    R2,R1          CHECK FOR THE END OF THE REAL DSN         00019700
         BNH   CONCOMP        NOT THERE YET                             00019800
CONINC   ICM   R4,B'0111',PDEDCHN GET THE NEXT CONTAINING PDE POINTER   00019900
         BNZ   CONNEXT        IF IT'S THERE, KEEP LOOKING               00020000
         B     CHECKOUT       NO MATCHES, EXCLUDE THIS DSNAME           00020100
CONEND   DS    0H                                                       00020200
         DROP  R4             FINISHED WITH THE PDE                     00020300
*                                                                       00020400
*        NOW THE BIG MESS, CHECK FOR LIMIT, AND'S, AND OR'S             00020500
*                                                                       00020600
         CLI   LIMITK+1,0     WAS LIMIT SPECIFIED                       00020700
         BE    LIMEND         NO, THEN THERE CAN BE NO AND'S OR OR'S    00020800
         NI    VTCFMTCK,255-VTCFMTCC  TURN OFF THE ROUTINE CALL FLAG    00020900
         VTCHL L              EVALUATE THE LIMIT 1=TRUE 0=FALSE         00021000
         ST    R15,LIMVAL     SAVE THE ANSWER                           00021100
         VTANDOR 1            CHECK AND1 OR OR1                         00021200
         VTANDOR 2            CHECK AND2 OR OR2                         00021300
         VTANDOR 3            CHECK AND3 OR OR3                         00021400
LIMCOMP  L     R15,LIMVAL     GET THE RESULT OF ALL THIS                00021500
         LTR   R15,R15        TEST IT                                   00021600
         BZ    CHECKOUT       IT GETS EXCLUDED                          00021700
LIMEND   DS    0H                                                       00021800
         B     CHECKIN        ALL TESTS PASSED, INCLUDE THIS ONE        00021900
         SPACE 5                                                        00022000
*                                                                       00022100
*        EVALUATION ROUTINE FOR   KEYWORD  OPER  VALUE                  00022200
*                                                                       00022300
LIMEVAL  L     R5,REFKEY      GET THE KEYWORD VALUE                     00022400
         MH    R5,H12         MULTIPLY IT BY 12                         00022500
         A     R5,ATABTITL    THEN RELOCATE IT                          00022600
         SR    R10,R10         CLEAR THE ROUTINE POINTER                00022700
         TM    1(R5),X'80'    IS IT A FORMATTED ITEM?                   00022800
         BO    LIMFORM        YES, GO DO IT                             00022900
         SR    R6,R6         CLEAR A REGISTER                           00023000
         IC    R6,2(R5)      GET THE OFFSET INTO VTFMT                  00023100
         IC    R10,1(R5)      GET THE ROUTINE NUMBER                    00023200
         LTR   R10,R10         SEE IF IT'S A GOOD NUMBER                00023300
         BP    LIMEVAL1       IT'S ALL RIGHT                            00023400
LIMABEND ABEND 702,DUMP       CRASH AND BURN                            00023500
LIMEVAL1 B     *(R10)         AND GO TO IT                              00023600
         B     LIMDATE        CDATE, EXPDT, REFDT                       00023700
         B     LIMLUSE        *** DUMMY ENTRY ***                       00023800
         B     LIMFORM        FORM , USED SPACE                         00023900
         B     LIMFORM        UNUSED SPACE                              00024000
         B     LIMFORM        PCT USED                                  00024100
         B     LIMEXT         EXTENTS                                   00024200
         B     LIMBLREC       LRECL, BLKSZ, SEC Q                       00024300
         B     LIMCCHH       CCHH CHECKING                              00024400
         SPACE 5                                                        00024500
*                                                                       00024600
*        PERFORM THE FORMATTED ITEM CHECKING                            00024700
*                                                                       00024800
LIMFORM  DS    0H                                                       00024900
         TM    VTCFMTCK,VTCFMTCC  WAS FORMAT CALLED BEFORE FOR THIS DS  00025000
         BO    LIMFCALD       YES, DON'T CALL IT AGAIN                  00025100
         VTCALL FORM          NO, CALL IT TO GET THE ITEMS              00025200
         OI    VTCFMTCK,VTCFMTCC+VTCFMTCD  THEN SET THE SWITCHES        00025300
LIMFCALD SR    R2,R2          CLEAR A WORK REG                          00025400
         A     R6,FORMATAD   RELOCATE THE BLOCK                         00025500
         B     *+4(R10)         AND GO TO IT                            00025600
         B     LIMFORMA      FORMATTED ITEM                             00025700
         B     LIMABEND       CDATE, EXPDT                              00025800
         B     LIMABEND       LAST USE DATE                             00025900
         B     LIMALLOC       ALLOC, USED SPACE                         00026000
         B     LIMUNUSD       UNUSED SPACE                              00026100
         B     LIMPCT         PCT USED                                  00026200
LIMFORMA DS    0H                                                       00026300
         IC    R2,2(R5)       GET THE OFFSET IN VTFMT                   00026400
         A     R2,FORMATAD    THEN RELOCATE IT                          00026500
         L     R4,REFVAL      GET THE VALUE PDE                         00026600
         LH    R3,4(R4)       GET THE LENGTH OF THE STRING              00026700
         L     R1,0(R4)       AND ITS ADDRESS                           00026800
*                                                                       00026900
*        DO THE ACTUAL COMPARE                                          00027000
*                                                                       00027100
         BCTR  R3,0           DOWN ONE FOR AN EX                        00027200
         EX    R3,COMPLIM     COMPARE AS SPECIFIED                      00027300
COMPDONE DS    0H             GET THE OPERATOR ADDRESS                  00027400
         BL    COMPLOW        CHECK THE OPERATOR, VALUE LESS THAN ITEM  00027500
         BE    COMPEQ         CHECK THE OPERATOR, KEYWORD EQUALS VALUE  00027600
*                             KEYWORD IS GREATER THAN THE VALUE         00027700
COMPHI   CLI   REFOPER+3,NE       WAS OPERATOR NE                       00027800
         BE    COMPYES        HIGH SATISFIES THE EXPRESSION             00027900
         CLI   REFOPER+3,GT       ALSO FOR GT                           00028000
         BE    COMPYES        HIGH SATISFIES THE EXPRESSION             00028100
         CLI   REFOPER+3,GE       AND FOR GE                            00028200
         BE    COMPYES        HIGH SATISFIES THE EXPRESSION             00028300
         B     COMPNO         THIS ONE DOESN'T FIT                      00028400
*                             KEYWORD IS EQUAL TO THE VALUE             00028500
COMPEQ   CLI   REFOPER+3,EQ       WAS OPERATOR EQ                       00028600
         BE    COMPYES        EQ   SATISFIES THE EXPRESSION             00028700
         CLI   REFOPER+3,LE       ALSO FOR LE                           00028800
         BE    COMPYES        EQ   SATISFIES THE EXPRESSION             00028900
         CLI   REFOPER+3,GE       AND FOR GE                            00029000
         BE    COMPYES        EQ   SATISFIES THE EXPRESSION             00029100
         B     COMPNO         THIS ONE DOESN'T FIT                      00029200
*                             KEYWORD IS LESS THAN THE VALUE            00029300
COMPLOW  CLI   REFOPER+3,NE       WAS OPERATOR NE                       00029400
         BE    COMPYES        LOW  SATISFIES THE EXPRESSION             00029500
         CLI   REFOPER+3,LT       ALSO FOR LT                           00029600
         BE    COMPYES        LOW  SATISFIES THE EXPRESSION             00029700
         CLI   REFOPER+3,LE       AND FOR LE                            00029800
         BE    COMPYES        LOW  SATISFIES THE EXPRESSION             00029900
         B     COMPNO         THIS ONE DOESN'T FIT                      00030000
*                                                                       00030100
*        IT FITS OR IT DOESN'T                                          00030200
*                                                                       00030300
COMPYES  LA    R15,1          SET A TRUE VALUE                          00030400
         BR    R8             THEN RETURN                               00030500
COMPNO   SR    R15,R15        SET A FALSE VALUE                         00030600
         BR    R8             THEN RETURN                               00030700
*                                                                       00030800
*        SPECIAL ROUTINES TO CHECK NON-FORMATTED ITEMS                  00030900
*                                                                       00031000
LIMDATE  DS    0H                                                       00031100
*                                                                       00031200
*        COMPARE DATES                                                  00031300
*                                                                       00031400
         LA    R14,DS1CREDT  POINT TO CREATION DATE                     00031500
         CLI   REFKEY+3,CDATE      IS THAT IT?                          00031600
         BE    LIMDGET       YES, THIS IS IT                            00031700
         LA    R14,DS1EXPDT  POINT TO EXPIRATION DATE                   00031800
         CLI   REFKEY+3,EXPDT      IS THAT IT?                          00031900
         BE    LIMDGET       YES, THIS IS IT                            00032000
         LA    R14,DS1REFD   NO, USE REFERENCE DATE                     00032100
LIMDGET  SR    R15,R15       CLEAR A WORK REG                           00032200
         IC    R15,0(R14)    GET THE YEAR                               00032300
         MH    R15,H1000     TIMES 1000                                 00032400
         SR    R1,R1         CLEAR ANOTHER WORK REG                     00032500
         ICM   R1,3,1(R14)    GET THE DAYS                              00032600
         AR    R15,R1        PUT THE DATE TOGETHER                      00032700
         L     R1,REFNUM     GET THE ADDRESS OF THE VALUE               00032800
         L     R1,0(R1)      GET THE VALUE ITSELF                       00032900
         CR    R15,R1        COMPARE THEM                               00033000
         B     COMPDONE      GO CHECK OPERANDS                          00033100
LIMLUSE  DS    0H                                                       00033200
*                                                                       00033300
*        LAST USE DATE                                                  00033400
*                                                                       00033500
         SR    R14,R14       CLEAR THE DATE                             00033600
         CLC   ZERO,75(R7)   CHECK FOR NO DATA                          00033700
         BE    LIMLUCMP      RIGHT, SKIP ON                             00033800
         MVC   CHEKDBLW+5(3),75(R7)  MOVE IN THE LAST USE DATE          00033900
         CVB   R14,CHEKDBLW  CONVERT IT TO BINARY                       00034000
LIMLUCMP L     R1,REFNUM     GET THE ADDRESS OF THE VALUE               00034100
         L     R1,0(R1)      GET THE VALUE                              00034200
         CR    R14,R1        DO THE COMPARE                             00034300
         B     COMPDONE      THEN CHASE DOWN THE OPERANDS               00034400
LIMEXT   DS    0H                                                       00034500
*                                                                       00034600
*        EXTENTS                                                        00034700
*                                                                       00034800
         L     R1,REFNUM     GET THE COMPARE VALUE ADDRESS              00034900
         CLC   DS1NOEPV,3(R1)        COMPARE THEM                       00035000
         B     COMPDONE      GO CHECK OPERANDS                          00035100
LIMBLREC DS    0H                                                       00035200
*                                                                       00035300
*        LRECL, BLKSZ, SECQ                                             00035400
*                                                                       00035500
         L     R1,REFNUM     GET THE ADDRESS OF THE COMPARE VALUE       00035600
         L     R15,0(R1)      THEN GET THE VALUE ITSELF                 00035700
         CLI   REFKEY+3,BLKSZ      BLOCK SIZE?                          00035800
         BNE   LIMB1         NO, KEEP CHECKING                          00035900
         LH    R1,DS1BLKL    COMPARE TO THE BLOCK SIZE                  00036000
         CR    R1,R15        COMPARE THEM                               00036100
         B     COMPDONE      GO SIFT THROUGH THE OPERANDS               00036200
LIMB1    CLI   REFKEY+3,LRECL  LOGICAL RECORD LENGTH                    00036300
         BNE   LIMB2         NO, KEEP GOING                             00036400
         LH    R1,DS1LRECL   COMPARE TO THE LRECL                       00036500
         CR    R1,R15        COMPARE THEM                               00036600
         B     COMPDONE      GO CHECK THE OPERANDS                      00036700
LIMB2    MVC   HWORK,DS1SCALO+2 GET THE SECONDARY QUANTITY              00036800
         LH    R1,HWORK      DO THE COMPARE                             00036900
         CR    R1,R15        COMPARE THEM                               00037000
         B     COMPDONE      THEN CHECK THE OPERANDS                    00037100
*                                                                       00037200
*        SPACE CHECKING ROUTINES                                        00037300
*                                                                       00037400
LIMALLOC DS    0H                                                       00037500
*                                                                       00037600
*        ALLOC AND USED                                                 00037700
*                                                                       00037800
         L     R1,REFNUM     GET THE ADDRESS OF THE CONVERTED NUMBER    00037900
         L     R1,0(R1)      GET THE VALUE                              00038000
         L     R15,0(R6)     GET THE AMOUNT                             00038100
         CR    R15,R1        COMPARE THEM                               00038200
         B     COMPDONE      THEN CHECK THE OPERANDS                    00038300
LIMUNUSD DS    0H                                                       00038400
*                                                                       00038500
*        UNUSED                                                         00038600
*                                                                       00038700
         L     R6,FORMATAD   POINT TO THE FORMATTED VTOC                00038800
         USING VTFMT,R6      SET ADDRESSABILITY                         00038900
         ICM   R14,15,VTFUSED      GET THE AMOUNT USED                  00039000
         BM    LIMUNUAL      IF MINUS, WE DON'T KNOW                    00039100
         L     R14,VTFALLOC  GET ALLOC                                  00039200
         S     R14,VTFUSED   MINUS THE AMOUNT USED                      00039300
LIMUNUAL L     R1,REFNUM     GET THE ENTERED VALUE                      00039400
         L     R1,0(R1)      NOW ITS VALUE FOR REAL                     00039500
         CR    R14,R1        COMPARE THE VALUES                         00039600
         B     COMPDONE      THEN GO CHECK THE OPERANDS                 00039700
LIMPCT   DS    0H                                                       00039800
*                                                                       00039900
*        PER CENT                                                       00040000
*                                                                       00040100
         L     R6,FORMATAD   POINT TO THE FORMATTED VTOC                00040200
         USING VTFMT,R6      SET ADDRESSABILITY                         00040300
         SR    R14,R14       CLEAR A REGISTER                           00040400
         ICM   R15,15,VTFUSED      GET THE AMOUNT USED                  00040500
         BM    LIMP100       IF UNKNOWN USED, SET 100 PER CENT          00040600
         CLC   VTFALLOC,ZERO ZERO ALLOCATED SPACE?                      00040700
         BNE   LIMPCTOK      NO, CONTINUE                               00040800
         CLC   VTFUSED,ZERO  ZERO USED SPACE?                           00040900
         BE    LIMPCOMP      YES, PCT IS ZERO                           00041000
*              ZERO ALLOCATED, NONZERO USED, INCLUDE THIS ONE           00041100
         B     COMPYES                                                  00041200
LIMP100  LA    R15,100       SET UP 100 PER CENT                        00041300
         B     LIMPCOMP      GO COMPARE                                 00041400
LIMPCTOK M     R14,F100      MULTIPLY BY 100 PERCENT                    00041500
         D     R14,VTFALLOC  DIVIDE BY THE ALLOCATION                   00041600
LIMPCOMP L     R1,REFNUM     GET THE VALUE ADDRESS                      00041700
         L     R1,0(R1)      THEN THE VALUE                             00041800
         CR    R15,R1        THEN COMPARE THEM                          00041900
         B     COMPDONE      THEN GO SIFT THROUGH THE OPERANDS          00042000
*                                                                       00042100
*        CCHH CHECKING IS NOT QUITE STANDARD BECAUSE THERE MAY          00042200
*        MAY BE UP TO 16 EXTENTS TO COMPARE.  THE DATA MAY BE           00042300
*        CC OR CCHH FORMATS.  THE DATA SET MAY BE EQUAL TO,             00042400
*        LESS THAN, AND GREATER THAN ANY PARTICULAR VALUE.              00042500
*                                                                       00042600
LIMCCHH  DS    0H                                                       00042700
*                                                                       00042800
*        FIRST SEE IF THE CCHH WAS CONVERTED                            00042900
*              CONVERT IT IF NOT, SKIP IF IT'S DONE                     00043000
*                                                                       00043100
         L     R5,REFNUM     GET THE ADDRESS OF THE COMPARISON VALUE    00043200
         L     R4,0(R5)      GET THE VALUE                              00043300
         ICM   R6,3,4(R5)    GET THE COMPARE LENGTH                     00043400
         BP    LIMCSET       IF IT'S SET, THE CONVERSION IS DONE        00043500
*                      IT WASN'T SET, CONVERT FROM CHARS TO BINARY      00043600
         L     R1,REFVAL     GET THE ADDRESS OF THE IKJIDENT            00043700
         L     R2,0(R1)      POINT TO THE TEXT                          00043800
         LH    R3,4(R1)      GET THE LENGTH OF THE TEXT                 00043900
*        IT SHOULD BE 4 OR 8 CHARACTERS                                 00044000
         XC    DOUBLE,DOUBLE CLEAR OUT A PLACE TO WORK                  00044100
         CH    R3,H4         IS IT A CYLINDER ONLY?                     00044200
         BH    LIMCCON2      NO, TRY FOR A CCHH                         00044300
         BE    LIMCCON1      YES, JUST CONVERT IT                       00044400
         VTOCMSG CCHHLEN     LESS THAN FOUR CHARS, ISSUE A MSG          00044500
LIMCCON1 LA    R6,1          SET THE COMPARE LENGTH                     00044600
         B     LIMCMOVE      GO MOVE IT IN                              00044700
LIMCCON2 LA    R6,3          SET THE COMPARE LENGTH                     00044800
         CH    R3,H8         WAS IT A CCHH?                             00044900
         BE    LIMCMOVE      YES, JUST THE RIGHT LENGTH                 00045000
         VTOCMSG CCHHLEN     WARN THE PERSON                            00045100
         CH    R3,H8         CHECK AGAIN                                00045200
         BL    LIMCMOVE      IS IT OVER 8 CHARS?                        00045300
         LH    R3,H8         YES, SET IT FOR THE MAX - IGNORE RR        00045400
LIMCMOVE BCTR  R3,0          MINUS ONE FOR THE EX                       00045500
         EX    R3,MOVECCHH   MOVE IN THE CHARS                          00045600
         TR    DOUBLE,DECTABLE TRANSLATE HEX EBCDIC TO HEX BINARY       00045700
         PACK  CYLH(5),DOUBLE(9)  SQUISH OUT THE ZONES                  00045800
         L     R4,CYLH       GET THE CCHH                               00045900
         ST    R4,0(R5)      SAVE IT FOR LATER                          00046000
         STH   R6,4(R5)      SAVE THE LENGTH TOO                        00046100
LIMCSET  DS    0H            THE NUMBER IS CONVERTED                    00046200
*                                                                       00046300
*        COMPARE THE EXTENTS TO THE CCHH VALUE.                         00046400
*        ANY EXTENT MAY BE LT, EQ, AND GT A PARTICULAR                  00046500
*        VALUE, AND ALL THE EXTENTS MUST BE CHECKED.                    00046600
*                                                                       00046700
         MVI   CCHHCOMP,0    CLEAR THE FLAGS                            00046800
         SR    R2,R2         CLEAR A REG FOR AN EXTENT COUNTER          00046900
         ICM   R2,1,DS1NOEPV GET THE NUMBER OF EXTENTS                  00047000
         BZ    COMPNO        NO EXTENTS, JUST GO SEE                    00047100
*                                                                       00047200
*        GET EACH EXTENT AND PROCESS IT                                 00047300
*                                                                       00047400
         SR    R1,R1         FIRST EXTENT                               00047500
EXTNEXT  LR    R3,R1         GET THE CURRENT EXTENT                     00047600
         SLL   R3,2          TIMES 4                                    00047700
         EX    R0,GETEXT(R3) GET THE EXTENT ADDRESS INTO R3             00047800
*                                                                       00047900
*        CHECK THE BOTTOM OF THE EXTENT                                 00048000
*                                                                       00048100
         NI    CCHHCOMP,255-CCHHX TURN OFF THE STRADDLE FLAG            00048200
         EX    R6,CLCEXTLO    DO THE COMPARE                            00048300
         BH    SETH1         THE FIELD IS HIGHER THAN THE VALUE         00048400
         BE    SETEQ1        THE FIELD IS EQUAL TO THE VALUE            00048500
         OI    CCHHCOMP,CCHHLOW+CCHHX  LOWER -  POSSIBLE STRADDLE       00048600
         B     CHECKHI       GO CHECK THE TOP OF THIS EXTENT            00048700
SETH1    OI    CCHHCOMP,CCHHHIGH  SET THE FLAG                          00048800
         B     CHECKHI       GO CHECK THE TOP OF THIS EXTENT            00048900
SETEQ1   OI    CCHHCOMP,CCHHEQ   SET THE FLAG                           00049000
*                                                                       00049100
*        CHECK THE TOP OF THE EXTENT                                    00049200
*                                                                       00049300
CHECKHI  EX    R6,CLCEXTHI   DO THE COMPARE                             00049400
         BE    SETEQ2        EQUAL, GO SET IT                           00049500
         BL    EXTSET        LOW, GO SET IT                             00049600
*                                                                       00049700
*        THIS IS THE ONLY SLIGHTLY TRICKY PART, A STRADDLE              00049800
*        IF THE BOTTOM OF THE EXTENT IS LOWER THAN THE VALUE AND THE    00049900
*        TOP OF THE EXTENT IS HIGHER THAN THE VALUE, THEN THE           00050000
*        EQ FLAG SHOULD BE SET TOO.                                     00050100
*                                                                       00050200
         OI    CCHHCOMP,CCHHHIGH  SET THE HIGH FLAG                     00050300
         TM    CCHHCOMP,CCHHX     WAS THE BOTTOM LOWER THAN THE VALUE?  00050400
         BZ    EXTSET        NO, SKIP ON                                00050500
SETEQ2   OI    CCHHCOMP,CCHHEQ    SET THE EQ FLAG                       00050600
*                                                                       00050700
*        FINISHED WITH THAT EXTENT, CHECK FOR MORE                      00050800
*                                                                       00050900
EXTSET   DS    0H                                                       00051000
         LA    R1,1(R1)      INCREMENT THE EXTENT COUNTER               00051100
         CR    R1,R2         CHECK THE EXTENT COUNTER                   00051200
         BNL   LIMCOPER      THAT'S ALL FOLKS                           00051300
         TM    CCHHCOMP,CCHHHIGH+CCHHEQ+CCHHLOW  ARE THEY ALL SET?      00051400
         BNO   EXTNEXT       NO, CONTINUE LOOKING                       00051500
*                            YES, STOP NOW - ALL THE FLAGS ARE SET      00051600
LIMCOPER L     R4,REFOPER    GET THE NUMERIC VALUE OF THE KEY           00051700
         IC    R4,CCHHTAB(R4)  GET A FLAG MASK                          00051800
         EX    R4,CCHHOPER   CHECK TO SEE IF THE CONDITION IS SET       00051900
         BZ    COMPNO        NOT THERE                                  00052000
         B     COMPYES       YES                                        00052100
*                                                                       00052200
*        EXECUTED INSTRUCTIONS TO GET THE ADDRESS OF THIS EXTENT        00052300
*                                                                       00052400
GETEXT   LA    R3,DS1EXT1     1ST EXTENT                                00052500
         LA    R3,DS1EXT2     2ND EXTENT                                00052600
         LA    R3,DS1EXT3     3RD EXTENT                                00052700
         LA    R3,DS3EXTNT    4TH EXTENT                                00052800
         LA    R3,DS3EXTNT+10 5TH EXTENT                                00052900
         LA    R3,DS3EXTNT+20 6TH EXTENT                                00053000
         LA    R3,DS3EXTNT+30 7TH EXTENT                                00053100
         LA    R3,DS3ADEXT    8TH EXTENT                                00053200
         LA    R3,DS3ADEXT+10 9TH EXTENT                                00053300
         LA    R3,DS3ADEXT+20 10TH EXTENT                               00053400
         LA    R3,DS3ADEXT+30 11TH EXTENT                               00053500
         LA    R3,DS3ADEXT+40 12TH EXTENT                               00053600
         LA    R3,DS3ADEXT+50 13TH EXTENT                               00053700
         LA    R3,DS3ADEXT+60 14TH EXTENT                               00053800
         LA    R3,DS3ADEXT+70 15TH EXTENT                               00053900
         LA    R3,DS3ADEXT+80 16TH EXTENT                               00054000
*                                                                       00054100
*        ISSUE ERROR MESSAGES AND RETURN                                00054200
*                                                                       00054300
OBT3ERR  VTOCMSG OBT3ERRM   OBTAIN ERROR MESSAGE                        00054400
CHECKOUT LA    R15,8          EXCLUDE THIS DATA SET                     00054500
         B     CHEKRET        RETURN                                    00054600
*                                                                       00054700
CHECKIN  SR    R15,R15        CLEAR THE REGISTER, PROCESS THIS DATA SET 00054800
CHEKRET  LEAVE EQ                                                       00054900
*                                                                       00055000
*                                                                       00055100
         EJECT                                                          00055200
*                                                                       00055300
*        ROUTINES USED ABOVE                                            00055400
*                                                                       00055500
         EJECT                                                          00055600
*                                                                       00055700
*        PDLNUM - CONVERT FROM CHARACTERS ( EBCDIC ) TO AN INTEGER      00055800
*              BINARY FORM, PASSED BACK VIA REGISTER 15                 00055900
*              A PARSE PDE IS THE INPUT AS SHOWN IN THE SAMPLE BELOW    00056000
*                       LA    R1,PDL     POINT TO THE PARSE DECRIPTION  00056100
*                       BAL   R8,PDLNUM  GO CONVERT TO NUMERICS         00056200
*              THE ROUTINE WILL TERMINATE IF IT FINDS NON-NUMERICS      00056300
*                 ANY CHARACTERS OTHER THEN 0-9, +, -                   00056400
*              REGISTERS 1, 2, 5, 6, AND 7 ARE USED                     00056500
*                                                                       00056600
PDLNUM   STM   R1,R8,PDLNSAVE SAVE THE REGISTERS                        00056700
         LH    R2,4(R1)       GET THE STRING ADDRESS                    00056800
         L     R1,0(R1)       GET THE STRING ADDRESS                    00056900
         MVI   PDLMINUS,0     CLEAR THE NEGATIVE NUMBER FLAG            00057000
         SR    R5,R5          CLEAR THE CHARACTER COUNTER               00057100
         SR    R15,R15        CLEAR THE ANSWER                          00057200
PDLLOOP  LA    R6,0(R5,R1)    POINT TO THIS DIGIT                       00057300
         LA    R5,1(R5)       GET TO THE NEXT DIGIT                     00057400
         CR    R5,R2          IS THIS THE END OF THE STRING?            00057500
         BH    PDLFINI        YES, EXIT                                 00057600
         SR    R7,R7          CLEAR A WORK REGISTER                     00057700
         IC    R7,0(R6)       GET THE CHARACTER                         00057800
         SH    R7,PDLH240     SUBTRACT THE CHARACTER C'0'               00057900
         BM    PDLSP          IF NEGATIVE, CHECK SPECIAL CHARACTERS     00058000
         MH    R15,PDLH10     IT'S A DIGIT, MULTIPLY PRIOR NUM BY TEN   00058100
         AR    R15,R7         ADD ON THE NEW DIGIT                      00058200
         B     PDLLOOP        AND LOOP FOR MORE                         00058300
*                                                                       00058400
*        CHECK FOR SPECIAL CHARACTERS                                   00058500
*                                                                       00058600
PDLSP    CLI   0(R6),C' '     IS IT A BLANK?                            00058700
         BE    PDLLOOP        THEN IT'S OK                              00058800
         CLI   0(R6),C'+'     IS IT A PLUS?                             00058900
         BE    PDLLOOP        THAT'S ALSO OK                            00059000
         CLI   0(R6),C'-'     IS IT A MINUS?                            00059100
         BNE   PDLFINI        NO, JUST QUIT                             00059200
         MVI   PDLMINUS,1     YES, NOTE IT                              00059300
         B     PDLLOOP        AND LOOK FOR MORE                         00059400
*                                                                       00059500
*        QUIT, AFTER SETTING R15 TO NEGATIVE IF NEEDED                  00059600
*                                                                       00059700
PDLFINI  CLI   PDLMINUS,1     WAS A MINUS SIGN FOUND?                   00059800
         BNE   PDLLEAVE       NO, EXIT                                  00059900
         LNR   R15,R15        YES, MAKE IT NEGATIVE                     00060000
PDLLEAVE LM    R1,R8,PDLNSAVE RESTORE THE REGISTERS                     00060100
         BR    R8             RETURN                                    00060200
PDLH10   DC    H'10'                                                    00060300
PDLH240  DC    H'240'                                                   00060400
         EJECT                                                          00060500
*                                                                       00060600
*        ROUTINE TO CONVERT A TEXT DSCB ITEM                            00060700
*        INTO ITS KEY NUMBER                                            00060800
*        INPUT IS REG 4 - IKJIDENT PTR                                  00060900
*        OUTPUT IS REG 15 - KEY NUMBER                                  00061000
*        ENTRY VIA BAL   R8,GETKEY                                      00061100
*                                                                       00061200
GETKEY   L     R1,ATABTITL     POINT TO THE TABLE                       00061300
         LA    R1,12(R1)     POINT TO THE FIRST ENTRY                   00061400
         LA    R15,1           SET UP THE KEY NUMBER COUNTER            00061500
         L     R6,0(R4)      POINT TO THE ENTERED TEXT                  00061600
         ICM   R3,3,4(R4)    GET THE LENGTH OF THE ENTERED TEXT         00061700
         BNP   GETKNOTF      NOT FOUND IF ZERO                          00061800
         BCTR  R3,0          MINUS ONE FOR THE EX                       00061900
GETKLOOP LA    R2,4(R1)      POINT TO THE COMPARISON TEXT               00062000
         CLI   0(R2),C' '    IS IT HERE?                                00062100
         BNE   GETKSTD       YES, THIS IS IT                            00062200
         LA    R2,1(R2)      NO, MOVE OVER ONE MORE                     00062300
         CLI   0(R2),C' '    IS IT HERE?                                00062400
         BNE   GETKSTD       YES, THIS IS IT                            00062500
         LA    R2,1(R2)      NO, MOVE OVER ONE MORE                     00062600
GETKSTD  EX    R3,GETKCOMP   COMPARE THE KEY TEXT                       00062700
         BE    GETKFND       I FOUND IT                                 00062800
         LA    R1,12(R1)     GET TO THE NEXT KEY                        00062900
         LA    R15,1(R15)    INCREMENT THE KEY COUNTER                  00063000
         CH    R15,H26       CHECK FOR THE END OF THE TABLE             00063100
         BNH   GETKLOOP      NOT YET, KEEP LOOKING                      00063200
*                                                                       00063300
*        KEY WAS NOT FOUND, SEND BACK A ZERO                            00063400
*                                                                       00063500
GETKNOTF SR    R15,R15       SET UP THE ZERO AND RETURN                 00063600
GETKFND  BR    R8            JUST RETURN                                00063700
GETKCOMP CLC   0(0,R6),0(R2) EXECUTED TEXT COMPARE                      00063800
H26      DC    H'26'                                                    00063900
         EJECT                                                          00064000
*                                                                       00064100
*        ROUTINE TO CONVERT THE OPERATOR TEXT                           00064200
*        INTO A NUMERIC VALUE                                           00064300
*                                                                       00064400
GETOPER  LA    R15,1         NUMERIC VALUE COUNTER                      00064500
GETOLOOP LR    R14,R15       GET THE NUMBER                             00064600
         SLA   R14,1         MULTIPLY BY 2                              00064700
         LA    R14,OPERS(R14)      RELOCATE IT                          00064800
         CLC   0(2,R14),0(R1)      IS THIS THE TEXT?                    00064900
         BE    GETOFND       YES, RETURN THE NUMBER                     00065000
         LA    R15,1(R15)    NO, TRY THE NEXT ONE                       00065100
         CH    R15,H7        CHECK FOR THE END                          00065200
         BL    GETOLOOP      NOT THERE YET, KEEP TRYING                 00065300
         VTOCMSG OPERERR,OPERERR2  ISSUE THE MESSAGE                    00065400
         LA    R15,1         SET THE DEFAULT OPERATOR, EQ               00065500
GETOFND  BR    R8            THEN RETURN                                00065600
         EJECT                                                          00065700
*                                                                       00065800
*        PROGRAM CONSTANTS                                              00065900
*                                                                       00066000
COMPLIM  CLC   0(0,R2),0(R1)  COMPARE KEYWORD TO VALUE                  00066100
MOVECCHH MVC   DOUBLE(0),0(R2)                                          00066200
CLCEXTLO CLC   2(0,R3),0(R5)                                            00066300
CLCEXTHI CLC   6(0,R3),0(R5)                                            00066400
CCHHOPER TM    CCHHCOMP,0                                               00066500
CCHHTAB  DC    X'0040A0206080C0'  CCHHCOMP FLAGS                        00066600
OPERS    DC    C'  EQNELTLEGTGE'                                        00066700
*    FOR  EQ, NE, LT, LE, GT, GE                                        00066800
EDMASK   DC    XL16'40202020202020202020202020202120'                   00066900
BLANKS   DC    CL16'                '                                   00067000
STARS    DC    CL16'****************'                                   00067100
BLKTRTAB DC    XL64'00',X'04',XL192'00'                                 00067200
CAMSCON  CAMLST SEEK,*,*,*                                              00067300
COMPARE  CLC   0(0,R6),0(R2)  EXECUTED COMPARE                          00067400
DECTABLE EQU   *-C'A'   CONVERT EBCDIC HEX TO BINARY                    00067500
         DC    X'0A0B0C0D0E0F'                                          00067600
         DC    (C'0'-C'F'-1)X'FF'  FILLER                               00067700
         DC    X'00010203040506070809'                                  00067800
*                                                                       00067900
*                                                                       00068000
*                                                                       00068100
H3       DC    H'3'                                                     00068200
H4       DC    H'4'                                                     00068300
H7       DC    H'7'                                                     00068400
H8       DC    H'8'                                                     00068500
H10      DC    H'10'                                                    00068600
H12      DC    H'12'                                                    00068700
ZERO     DC    F'0'                                                     00068800
F100     DC    F'100'                                                   00068900
H1000    DC    H'1000'                                                  00069000
F127     DC    F'127'                                                   00069100
*                                                                       00069200
*                                                                       00069300
         PRINT NOGEN                                                    00069400
*                                                                       00069500
*        PROGRAM MESSAGES                                               00069600
*                                                                       00069700
OBT3ERRM MSG   ' VTOCCHEK - ERROR IN OBTAIN FOR FORMAT 3 DSCB '         00069800
KEYERR   MSG   ' VTOCCHEK - LIM, AND, OR OR SUBPARM ERROR - XXXXXX '    00069900
CCHHLEN  MSG   ' VTOCCHEK - CCHH SHOULD BE 4 OR 8 HEX CHARACTERS '      00070000
OPERERR  MSG   ' VTOCCHEK - OPERATOR WAS NOT EQ, NE, LT, LE, GT, OR GE' 00070100
OPERERR2 MSG   '          - WILL ASSUME EQ'                             00070200
*                                                                       00070300
         EJECT                                                          00070400
*                                                                       00070500
*                                                                       00070600
*        P A R S E   C O N T R O L   L I S T                            00070700
*                                                                       00070800
*                                                                       00070900
         PRINT OFF                                                      00071000
         COPY  VTOCPARS                                                 00071100
         PRINT ON                                                       00071200
*                                                                       00071300
*        DYNAMIC WORK AREA                                              00071400
*                                                                       00071500
         SPACE 3                                                        00071600
CHEKWORK DSECT                                                          00071700
         DS    18A            PRINT ROUTINE SAVE AREA                   00071800
CHARS    DS    CL16           CONVERSION TO CHARACTERS                  00071900
CAMSEEK  CAMLST SEEK,*,*,*                                              00072000
CAMLEN   EQU   *-CAMSEEK                                                00072100
         DS    0D                                                       00072200
CYLH     DS    F                                                        00072300
         DS    X              PAD FOR CCHH                              00072400
EQ       EQU   1              EQUATES FOR OPERATOR VALUES               00072500
NE       EQU   2                                                        00072600
LT       EQU   3                                                        00072700
LE       EQU   4                                                        00072800
GT       EQU   5                                                        00072900
GE       EQU   6                                                        00073000
HWORK    DS    H                                                        00073100
LIMVAL   DS    F                                                        00073200
NUMBERL  DS    F                                                        00073300
NUMLENL  DS    H                                                        00073400
FLAGNML  DS    X                                                        00073500
NUMKEYL  DS    X                                                        00073600
NUMBER1  DS    F                                                        00073700
NUMLEN1  DS    H                                                        00073800
FLAGNM1  DS    X                                                        00073900
NUMKEY1  DS    X                                                        00074000
NUMBER2  DS    F                                                        00074100
NUMLEN2  DS    H                                                        00074200
FLAGNM2  DS    X                                                        00074300
NUMKEY2  DS    X                                                        00074400
NUMBER3  DS    F                                                        00074500
NUMLEN3  DS    H                                                        00074600
FLAGNM3  DS    X                                                        00074700
NUMKEY3  DS    X                                                        00074800
REFKEY   DS    F                                                        00074900
REFOPER  DS    F                                                        00075000
REFVAL   DS    A                                                        00075100
REFNUM   DS    A                                                        00075200
PDLNSAVE DS    8A             REGISTER SAVE AREA FOR PDLNUM RTN         00075300
PDLMINUS DC    X'00'                                                    00075400
CHEKDBLW DS    D                                                        00075500
CCHHCOMP DS    X                                                        00075600
CCHHHIGH EQU   X'80'                                                    00075700
CCHHEQ   EQU   X'40'                                                    00075800
CCHHLOW  EQU   X'20'                                                    00075900
CCHHX    EQU   X'08'                                                    00076000
         DS    0D                                                       00076100
LENWORK  EQU   *-CHEKWORK                                               00076200
*                                                                       00076300
*        VTOC COMMAND COMMON AREA                                       00076400
*                                                                       00076500
         PRINT NOGEN                                                    00076600
         VTOCOM                                                         00076700
         SPACE 3                                                        00076800
*                                                                       00076900
*        FORMATTED DSCB                                                 00077000
*                                                                       00077100
         VTFMT                                                          00077200
         SPACE 3                                                        00077300
         PDEDSNAM                                                       00077400
         SPACE 3                                                        00077500
         SPACE 3                                                        00077600
DSCB1    DSECT                                                          00077700
         IECSDSL1 1                                                     00077800
         END                                                            00077900
./       ADD   NAME=VTOCEXCP
         TITLE 'VTOCEXCP- VTOC READING SUBROUTINE'                      00000100
*********************************************************************** 00000200
*        SPACE                                                          00000300
* AUTHOR;  R. F. MORSE, MIT INSTRUMENTATION LABORATORY  AUG 5,1968.     00000400
* MODIFIED;   E.BANK, FIREMAN'S FUND  MAY 15,1975.                      00000500
* MODIFIED;   R.MILLER  FIREMAN'S FUND  MAR 20,1977.                    00000600
* FUNCTION; THIS SUBROUTINE READS THE VOLUME TABLE OF CONTENTS (VTOC)   00000700
*        FROM A DIRECT-ACCESS DEVICE AND PRESENTS IT TO THE CALLER      00000800
*        ONE RECORD (DSCB) AT A TIME.                                   00000900
*                                                                       00001000
* OPERATION; THIS ROUTINE IS A SPECIALIZED SEQUENTIAL ACCESS METHOD     00001100
*        FOR VTOC'S.  ITS ADVANTAGE OVER ORDINARY BSAM IS THAT IT READS 00001200
*        AN ENTIRE TRACK IN ONE REVOLUTION, THUS SAVING CONSIDERABLE    00001300
*        TIME.  THE ROUTINE HAS THREE CALL MODES;                       00001400
*                                                                       00001500
*        0 - READ.  RETURNS WITH THE CORE ADDRESS OF A DSCB IN THE 3RD  00001600
*              PARAMETER.  THE CORE CONSISTS OF 148 CONSECUTIVE BYTES,  00001700
*              CONTAINING THE COUNT (8 BYTES), KEY (44 BYTES), AND DATA 00001800
*              (96 BYTES) FOR ONE DSCB.  RETURN CODES (REGISTER 15)     00001900
*              ARE;                                                     00002000
*                      0 - NORMAL;                                      00002100
*                      4 - END OF FILE, NO DATA PRESENTED;              00002200
*                      8 - PERMANENT I/O ERROR.  THE KEY AND DATA AREAS 00002300
*                          WILL BE SET TO ZEROS; THE COUNT AREA WILL    00002400
*                          CONTAIN THE CORRECT CCHHR.  SINCE READING    00002500
*                          IS DONE A TRACK AT A TIME, ALL THE DSCB'S    00002600
*                          FOR THAT TRACK WILL BE MARKED IN ERROR.      00002700
*                          READING MAY CONTINUE ON TO THE NEXT TRACK.   00002800
*                                                                       00002900
*        1 - OPEN.  THE SECOND PARAMETER SHOULD POINT TO                00003000
*              A  6-BYTE FIELD CONTAINING THE VOLSER TO BE USED FOR THE 00003100
*              ALLOCATION.                                              00003200
*              RETURN CODES ( REG 15 )  ARE DIRECT FROM DYNAMIC ALLOC.  00003300
*                      0 - NORMAL;                                      00003400
*                      4 - UNABLE TO OPEN (PROBABLY MISSING DD CARD);   00003500
*                      8 - DD CARD DID NOT REFER TO A DIRECT-ACCESS     00003600
*                          DEVICE, OR DEVICE TYPE UNKNOWN.              00003700
*                                                                       00003800
*        2 - CLOSE.  NO ARGUMENTS ARE REQUIRED OR RETURNED.  RETURN     00003900
*              CODE ( REG 15 ) IS FROM DYNAMIC UNALLOCATION.            00004000
         SPACE                                                          00004100
* ENTRY POINTS:  ENTRY IS ALWAYS TO 'VTOCEXCP'.                         00004200
*        ARGUMENTS ARE:                                                 00004300
*                      1 - A(FULL-WORD BINARY ENTRY TYPE);              00004400
*                      2 - A(PTR FOR DSCB);                             00004500
*                      3 - A(VOLSER).                                   00004600
* DATA SETS:  READS VOLUME TABLE OF CONTENTS FROM ANY DIRECT-ACCESS     00004700
*        DEVICE.  USES EXCP TO EXECUTE A CHAINED CHANNEL PROGRAM TO     00004800
*        READ AN ENTIRE TRACK AT A TIME.                                00004900
*                                                                       00005000
* EXTERNAL ROUTINES:  USES SUPERVISOR ROUTINE 'IECPCNVT' TO CONVERT     00005100
*        A RELATIVE TRACK NUMBER TO AN ABSOLUTE ADDRESS.                00005200
         SPACE                                                          00005300
* EXITS - NORMAL;  RETURNS TO CALLER VIA R14 WITH RETURN                00005400
*        CODE IN REGISTER 15.       (SEE ABOVE FOR RETURN CODE VALUES.) 00005500
*                                                                       00005600
* TABLES AND WORK AREAS;  USES AN AREA PROVIDED BY THE CALLER FOR       00005700
*        ITS SAVEAREA AND FOR WORKING STORAGE IMMEDIATELY FOLLOWING     00005800
*        THE PRIOR SAVEAREA.  IT USES GETMAIN TO OBTAIN AN AREA FOR     00005900
*        THE DSCB'S TO BE READ INTO.  THIS COULD BE AS LARGE AS         00006000
*        8K FOR 3350'S.  IT IS FREED BY THE FINAL CALL.                 00006100
*                                                                       00006200
* ATTRIBUTES;  REENTRANT, REFRESHABLE.                                  00006300
         EJECT                                                          00006400
* ENTER HERE AND PERFORM STANDARD REGISTER SAVE AREA HOUSEKEEPING.      00006500
         SPACE                                                          00006600
VTOCEXCP ENTER 12,8                    USE THE PROVIDED SAVEAREA        00006700
         USING VTOCWORK,R13   SET ADDRESSABILITY FOR WORK AREA          00006800
         LR    R11,R1                  SAVE PARAMETER REGISTER          00006900
         USING VTOCOM,R11              SET ADDRESSABILITY               00007000
*                                                                       00007100
*        POINT TO THE DCB FOR LATER REFERENCES                          00007200
*                                                                       00007300
         LA    RDCB,VTOCDCB   POINT TO IT                               00007400
         USING IHADCB,RDCB    SET ADDRESSABILITY                        00007500
         SPACE                                                          00007600
* SELECT MODE FROM CONTENTS AT ADDRESS IN REGISTER 1.                   00007700
         SPACE                                                          00007800
         SR    RWA,RWA                 CLEAR THE REGISTER               00007900
         IC    RWA,VTCEFUNC            GET CALL MODE                    00008000
         SLL   RWA,2                   MODE TIMES 4                     00008100
         B     *+4(RWA)                BRANCH ON MODE                   00008200
         SPACE                                                          00008300
         B     GETDSB                  MODE 0, GET A DSCB               00008400
         B     OPEN                    MODE 1, OPEN A NEW VTOC          00008500
         B     CLOSE                   MODE 2, CLOSE                    00008600
         B     RETURN0                 MODE 3 NOT DEFINED, NO OP        00008700
         SPACE 3                                                        00008800
***********                                                             00008900
* RETURNS *                                                             00009000
***********                                                             00009100
         SPACE                                                          00009200
RETURN0  SR    R15,R15                 CLEAR THE RETURN CODE            00009300
RETURN   LEAVE EQ                      EXIT WITH THE CURRENT RET CODE   00009400
         EJECT                                                          00009500
*********************                                                   00009600
* MODE 0 - GET DSCB *                                                   00009700
*********************                                                   00009800
         SPACE                                                          00009900
* IF END-OF-FILE WAS REACHED, RETURN AT ONCE.                           00010000
         SPACE                                                          00010100
GETDSB   LA    R15,4                   SET THE RETURN CODE, IN CASE     00010200
         TM    MODESW,EOFSW            TEST END-OF-FILE BIT             00010300
         BO    RETURN                  RETURN CODE 4 IF ON              00010400
         SPACE                                                          00010500
* IF CHANNEL PROGRAM HAS BEEN STARTED, GO TO CHECK IT.  OTHERWISE,      00010600
* ASSUME THERE IS AT LEAST ONE FULL BUFFER.                             00010700
         SPACE                                                          00010800
         TM    MODESW,XCPRUN           TEST IF EXCP ISSUED              00010900
         BO    XCPTEST                 BRANCH IF SO                     00011000
         SPACE                                                          00011100
* SET BUFFER ADDRESS TO NEXT DSCB AND TEST IF LAST ON TRACK.  IF NOT,   00011200
* EXIT WITH ITS ADDRESS IN R1.                                          00011300
         SPACE                                                          00011400
         L     RWA,DSCBADR             LOAD BUFFER POINTER              00011500
         LA    RWA,148(RWA)            ADVANCE TO NEXT DSCB             00011600
NDXSTORE ST    RWA,DSCBADR             STORE UPDATED POINTER            00011700
         C     RWA,DSCBLIM             TEST IF LAST DSCB IN BUFFER      00011800
         BNL   LASTDSCB                BRANCH IF SO                     00011900
         LR    R1,RWA                  PASS ADDRESS TO USER             00012000
GETOUT   ST    R1,DSCBADDR             STORE IT FOR THE CALLER          00012100
         TM    MODESW,RDERR            TEST IF ERROR ON THIS TRACK      00012200
         BZ    RETURN0                 RETURN CODE 0 IF NOT             00012300
         LA    R15,8                   SET THE RETURN CODE              00012400
         B     RETURN                  RETURN CODE 8 IF ERROR           00012500
         SPACE                                                          00012600
* IF THIS IS THE LAST DSCB, MOVE IT TO THE INTERNAL BUFFER AND START    00012700
* READING THE NEXT TRACK.                                               00012800
         SPACE                                                          00012900
LASTDSCB MVC   BUFF(148),0(RWA)        MOVE LAST DSCB                   00013000
         L     RWB,TTRN                LOAD RELATIVE TRACK NUMBER       00013100
         AL    RWB,=X'00010000'        INCREMENT TO NEXT TRACK          00013200
         ST    RWB,TTRN                                                 00013300
         BAL   RRET,EXCP               START CHANNEL PROGRAM            00013400
         LA    R1,BUFF                 LOAD DSCB ADDRESS FOR CALLER     00013500
         B     GETOUT                  TO RETURN                        00013600
         EJECT                                                          00013700
* WAIT FOR CHANNEL PROGRAM COMPLETION AND TEST THE OUTCOME.             00013800
         SPACE                                                          00013900
XCPTEST  WAIT  ECB=VTOCECB                                              00014000
         SPACE                                                          00014100
         NI    MODESW,X'FF'-XCPRUN     TURN EXCP STARTED BIT OFF        00014200
         CLI   VTOCECB,X'7F'           TEST COMPLETION CODE             00014300
         BNE   PERMERR                 BRANCH IF ERROR                  00014400
SETDSCBA L     RWA,DSCBSTRT            SET BUFFER POINTER TO 1ST DSCB   00014500
         B     NDXSTORE                                                 00014600
         SPACE                                                          00014700
* PERMANENT ERROR FOR THIS TRACK.  ZERO THE DSCB'S AND FILL IN THE      00014800
* CCHHR PORTIONS OF THE COUNT AREAS.                                    00014900
         SPACE                                                          00015000
PERMERR  OI    MODESW,RDERR            SIGNAL READ ERROR                00015100
         NI    IOBFLAG1,X'FB'          TURN OFF BIT 5 OF IOB FLAG       00015200
         NI    DCBIFLGS,X'3F'          TURN OFF BITS 0 AND 1            00015300
         L     RWA,DSCBSTRT            LOAD ADDRESS OF FIRST DSCB       00015400
         LA    RWB,1                   LOAD RECORD NUMBER               00015500
         SPACE                                                          00015600
DSCBELUP XC    0(148,RWA),0(RWA)       ZERO DSCB BUFFER                 00015700
         MVC   0(4,RWA),IOBSEEK+3      INSERT CCHH IN COUNT FIELD       00015800
         STC   RWB,4(RWA)              INSERT R IN COUNT FIELD          00015900
         LA    RWA,148(RWA)            POINT TO NEXT BUFFER             00016000
         LA    RWB,1(RWB)              INCREMENT RECORD NUMBER          00016100
         C     RWA,DSCBLIM             TEST FOR LAST BUFFER             00016200
         BNH   DSCBELUP                                                 00016300
         VTOCMSG TRACKERR       ISSUE THE ERROR MESSAGE                 00016400
         B     SETDSCBA                BRANCH TO RESET BUFFER POINTER   00016500
         EJECT                                                          00016600
*****************                                                       00016700
* MODE 1 - OPEN *                                                       00016800
*****************                                                       00016900
         SPACE                                                          00017000
* ENTER WITH A DDNAME IN SECOND PARAMETER POSITION.  PERFORM CLOSE      00017100
* SUBROUTINE FIRST TO BE SURE EVERYTHING IS INITIALIZED.                00017200
         SPACE                                                          00017300
OPEN     DS    0H                                                       00017400
         BAL   RRET,CLOSESUB           CALL CLOSE SUBROUTINE            00017500
         SPACE                                                          00017600
*                                                                       00017700
*        INITIALIZE THE DATA AREAS                                      00017800
*                                                                       00017900
*        FIRST THE DCB                                                  00018000
         MVC   VTOCDCB(DCBLEN),VTOCDCBM  SET UP THE DCB                 00018100
*                                                                       00018200
*        SET UP THE JFCB LISTS                                          00018300
*                                                                       00018400
         LA    R1,JEXLST      POINT TO THE EXIT LIST                    00018500
         STCM  R1,B'0111',DCBEXLSA  PUT IT INTO THE DCB                 00018600
         LA    R1,JFCBAREA    POINT TO THE JFCB AREA                    00018700
         ST    R1,JEXLST      AND PUT THAT INTO THE EXIT LIST           00018800
         MVI   JEXLST,X'87'   END OF LIST, JFCB EXIT                    00018900
         MVI   OPENLIST,X'80' END OF THE OPEN LIST TOO                  00019000
*        INITIALIZE THE IOB                                             00019100
         MVC   VTOCIOB(IOBCONL),IOBCONST START IT OUT                   00019200
         LA    R1,VTOCECB     GET THE ECB ADDRESS                       00019300
         ST    R1,IOBECB      AND STORE IT INTO THE IOB                 00019400
         ST    RDCB,IOBDCB    STORE THE DCB ADDRESS INTO THE IOB        00019500
*        INITIALIZE THE CAMLST                                          00019600
         MVC   DSCBFMT4(4),DSCBCON SET UP THE FIRST WORD                00019700
         LA    R1,IOBSEEK+3   SEEK ADDRESS                              00019800
         ST    R1,DSCBFMT4+4  INTO THE CAMLST                           00019900
         LA    R1,VOLID       VOLUME SERIAL NUMBER                      00020000
         ST    R1,DSCBFMT4+8  INTO THE CAMLST                           00020100
         LA    R1,FMT4        DSCB AREA                                 00020200
         ST    R1,DSCBFMT4+12 INTO THE CAMLST                           00020300
*                                                                       00020400
*        ALLOCATE THE VTOC OF THE CHOSEN PACK                           00020500
*                                                                       00020600
         LA    R1,ADDR        POINT TO THE UNIT ADDRESS                 00020700
         ST    R1,UNITADDR    SAVE THE ADDRESS                          00020800
         LA    R1,3           ALSO GET THE LENGTH                       00020900
         STH   R1,UNITLEN     AND SAVE IT FOR DYNAMIC ALLOCATION MACRO  00021000
         LA    R1,VOLID       POINT TO THE VOLUME SERIAL                00021100
         ST    R1,VOLADDR     SAVE THE ADDRESS                          00021200
         LA    R1,6           ALSO GET THE LENGTH                       00021300
         STH   R1,VOLLEN      AND SAVE IT FOR DYNAMIC ALLOCATION MACRO  00021400
         ALLOC DSN=VTOCNM,VOL=VOLADDR,UNIT=UNITADDR,DISP=SHR,          X00021500
               DDNTO=DCBDDNAM,ERROR=S99FAIL                             00021600
         OI    MODESW,ALLOCSW          SET ALLOCATE FLAG ON             00021700
         SPACE                                                          00021800
* OPEN THE VTOC.                                                        00021900
         SPACE                                                          00022000
*                                                                       00022100
*        FIRST READ THE JFCB TO SWITCH THE DSNAME TO HEX 04'S           00022200
*                                                                       00022300
         RDJFCB ((RDCB)),MF=(E,OPENLIST)  READ THE JFCB                 00022400
         LTR   R15,R15        TEST THE RETURN CODE                      00022500
         BNZ   ERRJFCB        BAD NEWS                                  00022600
         LA    R1,JFCBAREA    POINT TO THE JFCB                         00022700
         USING JFCB,R1        SET UP ADDRESSABILITY                     00022800
         MVI   JFCBDSNM,X'04' PUT IN THE FIRST ONE                      00022900
         MVC   JFCBDSNM+1(L'JFCBDSNM-1),JFCBDSNM  PROPAGATE IT          00023000
         OI    JFCBTSDM,JFCNWRIT  DON'T REWRITE IT                      00023100
         DROP  R1                                                       00023200
         OPEN  ((RDCB),(INPUT)),MF=(E,OPENLIST),TYPE=J  OPEN THE VTOC   00023300
         TM    DCBOFLGS,OPENBIT        TEST IF OPEN WORKED              00023400
         BZ    OPENERR                 ERROR IF OPEN FAILED             00023500
         SPACE                                                          00023600
* ISSUE AN OBTAIN FOR THE FIRST DSCB ON THE VTOC ( FORMAT 4 )           00023700
D3       STM   R2,R13,EXCPSAVE         SAVE OUR REGS                    00023800
         LA    R3,EXCPSAVE    POINT TO THE REGISTER SAVE AREA           00023900
         ICM   R0,B'1111',=X'00000100' FIRST DSCB                       00024000
         L     R1,DCBDEBAD             DEB ADDRESS                      00024100
         LA    R2,IOBSEEK              SAVE ADDRESS OF CCHHR            00024200
         L     R15,CVT                 GET ADDRESS OF CVT               00024300
         L     R15,CVTPCNVT(R15)       GET ADDRESS OF CONVERT ROUTINE   00024400
         BALR  R14,R15                 GO TO CONVERT ROUTINE            00024500
         LM    R2,R13,0(R3)            GET MY REGS BACK                 00024600
         OBTAIN DSCBFMT4               GET FORMAT 4 DSCB                00024700
         LTR   R15,R15                 DID WE GET IT                    00024800
         BNZ   OBTERR                  NO - THEN ERROR, KEEP R15        00024900
         CLI   DS4IDFMT,X'F4'          MAKE SURE WE HAVE FORMAT 4       00025000
         BNE   NOTFMT4                 NO - THEN ERROR                  00025100
         IC    R15,DS4DEVDT            GET NUMBER OF DSCBS PER TRACK    00025200
         ST    R15,NDSCBS              SAVE THE NUMBER OF DSCBS         00025300
         OC    NDSCBS,NDSCBS           MAKE SURE NOT ZERO               00025400
         BZ    DSCBNUM0                YES - GO TELL CALLER             00025500
*                                                                       00025600
* OBTAIN CORE FOR CHANNEL PROGRAM AND DSCB BUFFERS.                     00025700
         SPACE                                                          00025800
         LA    R0,156                  CORE FOR ONE DSCB AND ITS CCW    00025900
         MH    R0,NDSCBS+2             TIMES NUMBER PER TRACK           00026000
         AH    R0,=H'15'               PLUS 1 CCW AND ROUNDING          00026100
         N     R0,=X'FFFFFFF8'         ROUND TO DOUBLE-WORD MULTIPLE    00026200
         ST    R0,CBSIZE               SAVE SIZE OF GOTTEN CORE         00026300
         GETMAIN  R,LV=(0)             GET TRACK BUFFERS                00026400
         ST    R1,CBADDR               SAVE ADDRESS OF GOTTEN CORE      00026500
         OI    MODESW,CBGOT            INDICATE CORE GOTTEN             00026600
         SPACE                                                          00026700
* GENERATE CHANNEL PROGRAM.  IT CONSISTS OF A 'READ R0' ORDER WITH      00026800
* THE SKIP FLAG ON, FOLLOWED BY A 'READ COUNT-KEY-AND-DATA' ORDER FOR   00026900
* EACH DSCB.                                                            00027000
         SPACE                                                          00027100
         L     RWA,NDSCBS              NUMBER OF DSCB'S                 00027200
         SLL   RWA,3                   TIMES   8                        00027300
         LA    RWA,8(RWA,R1)           PLUS 8 AND BASE = 1ST BUFFER ADD 00027400
         ST    RWA,DSCBSTRT            SAVE ADDRESS OF FIRST BUFFER     00027500
         SPACE                                                          00027600
         ST    R1,IOBSTART             ADDRESS OF CHANNEL PROGRAM       00027700
         MVC   0(8,R1),INITCCW         INSERT FIRST CCW                 00027800
         LA    RWB,8(R1)               PLACE FOR NEXT CCW               00027900
         LA    RWC,1                   BUFFER COUNTER                   00028000
         SPACE                                                          00028100
CCWLOOP  MVC   0(8,RWB),READCCW        INSERT READ CCW FOR ONE DSCB     00028200
         ST    RWA,0(RWB)              SET ITS BUFFER ADDRESS           00028300
         MVI   0(RWB),READCKD          RESTORE COMMAND CODE             00028400
         C     RWC,NDSCBS              TEST BUFFER COUNTER              00028500
         BNL   LASTCCW                 BRANCH IF LAST BUFFER            00028600
         LA    RWB,8(RWB)              INCREMENT CCW ADDRESS            00028700
         LA    RWA,148(RWA)            INCREMENT BUFFER ADDRESS         00028800
         LA    RWC,1(RWC)              INCREMENT BUFFER COUNTER         00028900
         B     CCWLOOP                 DO NEXT BUFFER                   00029000
         SPACE                                                          00029100
LASTCCW  NI    4(RWB),X'FF'-CC         TURN OFF COMMAND CHAIN BIT       00029200
         ST    RWA,DSCBLIM             SAVE ADDRESS OF LAST DSCB BUFFER 00029300
         SPACE                                                          00029400
* SET OTHER THINGS AND START PROGRAM TO FILL BUFFER.                    00029500
         SPACE                                                          00029600
         SR    R0,R0                                                    00029700
         ST    R0,TTRN                 SET RELATIVE TRACK NUMBER TO 0   00029800
         NI    MODESW,X'FF'-XCPRUN-RDERR-EOFSW   SET FLAGS OFF          00029900
         BAL   RRET,EXCP               START CHANNEL PROGRAM            00030000
         B     RETURN0                 INDICATE SUCCESSFUL OPEN         00030100
         EJECT                                                          00030200
******************                                                      00030300
* MODE 2 - CLOSE *                                                      00030400
******************                                                      00030500
         SPACE                                                          00030600
CLOSE    BAL   RRET,CLOSESUB           CALL CLOSED CLOSE SUBROUTINE     00030700
         B     RETURN0                                                  00030800
         SPACE 2                                                        00030900
* IF THE CHANNEL PROGRAM IS RUNNING, WAIT FOR IT BEFORE TAKING FURTHER  00031000
* ACTION.                                                               00031100
         SPACE                                                          00031200
CLOSESUB DS    0H                                                       00031300
         TM    MODESW,XCPRUN           TEST IF CHANNEL PROGRAM RUNNING  00031400
         BZ    NOEXCP                  BRANCH IF NOT                    00031500
         WAIT  ECB=VTOCECB             WAIT UNTIL COMPLETE              00031600
         NI    MODESW,X'FF'-XCPRUN     TURN RUNNING SWITCH OFF          00031700
NOEXCP   DS    0H                                                       00031800
         SPACE                                                          00031900
* CLOSE THE DCB.                                                        00032000
         SPACE                                                          00032100
         TM    DCBOFLGS,OPENBIT        TEST IF DCB OPEN                 00032200
         BZ    NOCLOSE                 BRANCH IF NOT                    00032300
         CLOSE ((RDCB)),MF=(E,OPENLIST)   CLOSE THE VTOC                00032400
NOCLOSE  DS    0H                                                       00032500
         SPACE                                                          00032600
* FREE UP THE DDNAME AND VOLUME                                         00032700
         SPACE                                                          00032800
         TM    MODESW,ALLOCSW          DID WE ALLOCATE A DEVICE         00032900
         BNO   NOALLOC                 NO  - THEN NOTHING TO FREEUP     00033000
         LA    R1,DCBDDNAM   POINT TO THE DDNAME                        00033100
         ST    R1,DDNPDL     SAVE IT FOR FREE                           00033200
         LA    R1,8          GET THE DDNAME LENGTH                      00033300
         STH   R1,DDNPDL+4   SAVE IT FOR FREE                           00033400
         FREE  UNALC,DDN=DDNPDL,ERROR=S99FAIL  FREE THE DDNAME          00033500
         NI    MODESW,X'FF'-ALLOCSW    TURN OFF ALLOCATE SW             00033600
         SPACE                                                          00033700
NOALLOC  DS    0H                                                       00033800
         SPACE                                                          00033900
* RELEASE CORE OBTAINED FOR DSCB BUFFERS.                               00034000
         SPACE                                                          00034100
         TM    MODESW,CBGOT            TEST IF CORE GOTTEN              00034200
         BZ    NOFREE                  BRANCH IF NOT                    00034300
         LM    R0,R1,CBSIZE            LOAD SIZE AND LOCATION           00034400
         FREEMAIN  R,LV=(0),A=(1)      FREE CORE                        00034500
         NI    MODESW,X'FF'-CBGOT      SET CORE GOTTEN BIT OFF          00034600
NOFREE   DS    0H                                                       00034700
         SPACE                                                          00034800
         NI    MODESW,X'FF'-RDERR      CLEAR ERROR SWITCH               00034900
         BR    RRET                                                     00035000
         EJECT                                                          00035100
****************                                                        00035200
* EXCP ROUTINE *                                                        00035300
****************                                                        00035400
         SPACE                                                          00035500
* CONVERT RELATIVE TRACK ADDRESS IN 'TTRN' TO ABSOLUTE SEEK ADDRESS IN  00035600
* 'IOBSEEK', USING SUPERVISOR CONVERSION ROUTINE.                       00035700
         SPACE                                                          00035800
EXCP     DS    0H                                                       00035900
         STM   R2,R13,EXCPSAVE         SAVE IMPORTANT REGISTERS         00036000
         LA    R3,EXCPSAVE             SAVE REGS FOR RESTORING AFTER CL 00036100
         L     R0,TTRN                 LOAD RELATIVE TRACK NUMBER       00036200
         L     R1,DCBDEBAD             LOAD DEB ADDRESS                 00036300
         LA    R2,IOBSEEK              LOAD ADDR TO RECEIVE MBBCCHHR    00036400
         L     R15,CVT                 LOAD CVT ADDRESS                 00036500
         L     R15,CVTPCNVT(R15)       LOAD ADDR OF CONVERT ROUTINE     00036600
         BALR  R14,R15                 CONVERT TTRN TO MBBCCHHR         00036700
*                                      THAT CLOBBERED BASE REG          00036800
         LM    R2,R13,0(R3)            RESTORE REGISTERS                00036900
         LTR   R15,R15                 TEST IF EXTENT VIOLATED (RC=4)   00037000
         BNZ   SETEOF                  IF SO, MEANS END-OF-FILE         00037100
         CLC   DS4HPCHR,IOBSEEK+3      CHECK FOR THE LAST FMT1          00037200
         BL    SETEOF                  IF SO, PRETEND END-OF-FILE       00037300
         SPACE                                                          00037400
* ZERO ECB AND START CHANNEL PROGRAM.                                   00037500
         SPACE                                                          00037600
         SR    R0,R0                                                    00037700
         ST    R0,VTOCECB              CLEAR ECB                        00037800
         NI    MODESW,X'FF'-RDERR      RESET ERROR SWITCH               00037900
         EXCP  VTOCIOB                 START CHANNEL PROGRAM            00038000
         OI    MODESW,XCPRUN           SET 'RUNNING' FLAG               00038100
         BR    RRET                                                     00038200
         SPACE                                                          00038300
* WHEN EXTENT IS VIOLATED, SET END-FILE AND EXIT VIA CLOSE ROUTINE.     00038400
         SPACE                                                          00038500
SETEOF   OI    MODESW,EOFSW            SET END-OF-FILE BIT              00038600
         B     CLOSESUB                EXIT VIA CLOSE SUBROUTINE        00038700
         EJECT                                                          00038800
********************************                                        00038900
* DAIRFAIL ROUTINE             *                                        00039000
********************************                                        00039100
S99FAIL  LR    R15,RRCODE     SAVE THE RETURN CODE                      00039200
         S99FAIL MF=(E,S99FLIST,S99FLEN)  ISSUE THE APPROPRIATE MSG     00039300
         LR    R15,RRCODE     RELOAD THE RETURN CODE                    00039400
         B     RETURN         AND THEN EXIT                             00039500
         SPACE 3                                                        00039600
*                                                                       00039700
*        VARIOUS OTHER ERROR ROUTINES                                   00039800
*                                                                       00039900
OPENERR  VTOCMSG OPENERRM  ISSUE THE MESSAGE                            00040000
         B     ERRET          THEN RETURN                               00040100
OBTERR   VTOCMSG OBTERRM  ISSUE THE MESSAGE                             00040200
         B     ERRET          THEN RETURN                               00040300
NOTFMT4  VTOCMSG NOTFMT4M  ISSUE THE MESSAGE                            00040400
         B     ERRET          THEN RETURN                               00040500
DSCBNUM0 VTOCMSG DSCBNUMM  ISSUE THE MESSAGE                            00040600
         B     ERRET          THEN RETURN                               00040700
*                                                                       00040800
ERRJFCB  VTOCMSG ERRJFCBM   ERROR IN READING JFCB                       00040900
*                                                                       00041000
ERRET    LA    R15,8          SET AN ERROR RETURN CODE                  00041100
         B     RETURN         THEN EXIT                                 00041200
*                                                                       00041300
         EJECT                                                          00041400
********************************                                        00041500
* CONSTANTS, VARIABLES, ETC... *                                        00041600
********************************                                        00041700
         SPACE                                                          00041800
*        ERROR MESSAGES                                                 00041900
*                                                                       00042000
OPENERRM MSG   ' VTOCEXCP - ERROR IN OPENING VTOC '                     00042100
OBTERRM  MSG   ' VTOCEXCP - ERROR IN OBTAIN '                           00042200
NOTFMT4M MSG   ' VTOCEXCP - FORMAT 4 DSCB WAS NOT FIRST'                00042300
DSCBNUMM MSG   ' VTOCEXCP - THE FORMAT 4 DSCB HAS DSCB S/TRK = 0 '      00042400
TRACKERR MSG   ' VTOCEXCP - A READ ERROR OCCURRED ON THE VTOC '         00042500
ERRJFCBM MSG   ' VTOCEXCP - A RDJFCB ERROR OCCURRED '                   00042600
         SPACE                                                          00042700
INITCCW  CCW   READR0,0,CC+SLI+SKIP,8                                   00042800
READCCW  CCW   READCKD,0,CC,148                                         00042900
         SPACE                                                          00043000
DSCBCON  CAMLST SEEK,0,0,0   FILLED IN WITH IOBSEEK+3, VOLID, FMT4      00043100
         EJECT                                                          00043200
* DATA CONTROL BLOCK                                                    00043300
         PRINT   GEN                                                    00043400
VTOCDCBM DCB   DDNAME=VTOCDD,MACRF=(E),EXLST=1                          00043500
DCBLEN   EQU   *-VTOCDCBM                                               00043600
         SPACE                                                          00043700
* IOB FOR CHANNEL PROGRAM                                               00043800
         SPACE                                                          00043900
IOBCONST DS    0D                                                       00044000
         DC    X'42000000'     COMMAND CHAIN, NOT RELATED               00044100
         DC    A(0)            ECB ADDRESS                              00044200
         DC    2F'0'                                                    00044300
         DC    A(0)            CHANNEL PROGRAM BEGINNING                00044400
         DC    A(0)            DCB ADDRESS                              00044500
         DC    X'03000000'                                              00044600
         DC    F'0'                                                     00044700
         DC    D'0'            INITIAL SEEK ADDRESS                     00044800
IOBCONL  EQU   *-IOBCONST                                               00044900
* VTOC NAME FOR ALLOCATION                                              00045000
VTOCNM   DC    A(VTOCNAME)                                              00045100
         DC    Y(12)                                                    00045200
VTOCNAME DC    CL12'FORMAT4.DSCB'   DATA SET NAME FOR VTOC              00045300
*                                                                       00045400
*                                                                       00045500
         LTORG                                                          00045600
         EJECT                                                          00045700
* SECTION DEFINITION AND REGISTER ASSIGNMENTS;                          00045800
         SPACE 2                                                        00045900
RWA      EQU   2                                                        00046000
RWB      EQU   3                                                        00046100
RWC      EQU   4                                                        00046200
RDCB     EQU   8              DCB POINTER                               00046300
RRCODE   EQU   10              RETURN CODE REGISTER                     00046400
RRET     EQU   9               LOCAL SUBROUTINE EXIT REGISTER           00046500
         SPACE 3                                                        00046600
* TAGS FOR CHANNEL COMMANDS AND FLAG BITS:                              00046700
         SPACE                                                          00046800
READR0   EQU   X'16'           READ RECORD 0                            00046900
READCKD  EQU   X'1E'           READ COUNT, KEY, AND DATA                00047000
         SPACE                                                          00047100
CC       EQU   X'40'           COMMAND CHAIN FLAG                       00047200
SLI      EQU   X'20'           SUPPRESS LENGTH INDICATION FLAG          00047300
SKIP     EQU   X'10'           SKIP DATA TRANSFER FLAG                  00047400
         SPACE 3                                                        00047500
* COMMUNICATION VECTOR TABLE (CVT) DEFINITIONS:                         00047600
         SPACE                                                          00047700
CVT      EQU   16              LOCATION OF CVT BASE ADDRESS             00047800
CVTPCNVT EQU   28              OFFSET TO CONVERT ROUTINE ADDRESS        00047900
         EJECT                                                          00048000
*                                                                       00048100
*        AREA USED BY VTOCREAD, PASSED VIA R13                          00048200
*                                                                       00048300
VTOCWORK DSECT                                                          00048400
         DS    18F             SAVE AREA                                00048500
         SPACE                                                          00048600
EXCPSAVE DS    18F             INTERNAL SAVE AREA                       00048700
CBSIZE   DS    2F              SIZE AND LOCATION OF GOTTEN CORE         00048800
CBADDR   EQU   CBSIZE+4                                                 00048900
NDSCBS   DS    F               NUMBER OF DSCB'S PER TRACK               00049000
DSCBSTRT DS    F               ADDRESS OF 1ST DSCB BUFFER               00049100
DSCBLIM  DS    F               ADDRESS OF LAST DSCB BUFFER              00049200
DSCBADR  DS    F               ADDRESS OF CURRENT DSCB                  00049300
TTRN     DS    F               RELATIVE TRACK NUMBER                    00049400
VOLADDR  DS    A               FAKE PDL FOR ALLOC MACRO - ADDRESS       00049500
VOLLEN   DS    H                       AND LENGTH OF VOLID              00049600
UNITADDR DS    A               FAKE PDL FOR ALLOC MACRO - ADDRESS       00049700
UNITLEN  DS    H                       AND LENGTH OF UNIT ADDRESS       00049800
DDNPDL   DS    2F            SPACE FOR DDNAME PDL                       00049900
         SPACE                                                          00050000
* MODE SWITCH AND BIT DEFINITIONS                                       00050100
         SPACE                                                          00050200
MODESW   DC    X'00'                                                    00050300
CBGOT    EQU   X'80'           CORE GOTTEN FOR BUFFER                   00050400
XCPRUN   EQU   X'40'           CHANNEL PROGRAM STARTED BUT NOT CHECKED  00050500
RDERR    EQU   X'20'           PERMANENT I/O ERROR                      00050600
EOFSW    EQU   X'10'           END-OF-FILE SENSED                       00050700
ALLOCSW  EQU   X'08'           ALLOCATE VOLUME FLAG                     00050800
         SPACE                                                          00050900
VTOCDCB  DCB   DDNAME=VTOCDD,MACRF=(E),EXLST=1                          00051000
         SPACE                                                          00051100
OPENBIT  EQU   X'10'                                                    00051200
OPENLIST DS    2F                                                       00051300
         SPACE                                                          00051400
* IOB FOR CHANNEL PROGRAM                                               00051500
         SPACE                                                          00051600
VTOCIOB  DS    0D                                                       00051700
IOBFLAG1 DC    X'42000000'     COMMAND CHAIN, NOT RELATED               00051800
IOBECB   DC    A(VTOCECB)                                               00051900
         DC    2F'0'                                                    00052000
IOBSTART DC    A(0)            CHANNEL PROGRAM BEGINNING                00052100
IOBDCB   DC    A(VTOCDCB)                                               00052200
         DC    X'03000000'                                              00052300
         DC    F'0'                                                     00052400
IOBSEEK  DC    D'0'            INITIAL SEEK ADDRESS                     00052500
         SPACE                                                          00052600
* EVENT CONTROL BLOCK FOR CHANNEL PROGRAM:                              00052700
         SPACE                                                          00052800
VTOCECB  DC    F'0'            EVENT CONTROL BLOCK                      00052900
         SPACE 3                                                        00053000
* INTERNAL BUFFER FOR LAST DSCB                                         00053100
BUFF     DS    XL148                                                    00053200
         SPACE 2                                                        00053300
DSCBFMT4 CAMLST SEEK,IOBSEEK+3,VOLID,FMT4                               00053400
         SPACE                                                          00053500
*   WORK AREA FOR DYNAMIC ALLOCATION                                    00053600
         DYNSPACE                                                       00053700
S99FLIST DS    XL(S99FLEN)                                              00053800
         SPACE                                                          00053900
*                                                                       00054000
*        JFCB EXIT LIST AND AREA                                        00054100
*                                                                       00054200
JEXLST   DS    F                                                        00054300
JFCBAREA DS    XL176                                                    00054400
         DS    0D                                                       00054500
VTOCWLEN EQU   *-VTOCWORK                                               00054600
         SPACE 2                                                        00054700
         PRINT GEN                                                      00054800
         VTOCOM                                                         00054900
         PRINT NOGEN                                                    00055000
         SPACE 2                                                        00055100
         IEFZB4D0                                                       00055200
         SPACE 2                                                        00055300
         IEFZB4D2                                                       00055400
         SPACE 2                                                        00055500
         DCBD  DEVD=DA,DSORG=PS                                         00055600
         SPACE 2                                                        00055700
JFCB     DSECT                                                          00055800
         IEFJFCBN                                                       00055900
         END                                                            00056000
./       ADD   NAME=VTOCFORM
         TITLE 'VTOC COMMAND FORMAT ROUTINE'                            00000100
*********************************************************************** 00000200
*                                                                     * 00000300
*                                                                     * 00000400
* TITLE -      VTOC COMMAND FORMAT ROUTINE                            * 00000500
*                                                                     * 00000600
* FUNCTION -   FORMAT THE DATA INTO THE VTFMT  DSECT FROM THE         * 00000700
*              FORMAT 1 ( AND 3 IF NEEDED ) DSCB.  THIS ROUTINE       * 00000800
*              ALSO GETS THE AREA TO CONTAIN THE FORMATTED            * 00000900
*              DSCB INFORMATION.                                      * 00001000
*                                                                     * 00001100
* OPERATION -  FIRST GET AN AREA FROM THE CURRENT BLOCK, OR GET       * 00001200
*              A BLOCK ( 32K ) OF STORAGE TO USE FOR THE FORMATTED    * 00001300
*              DSCB'S.  MOVE THE DATA OVER FROM THE FORMAT 1 DSCB.    * 00001400
*              THE SPACE CALCULATIONS MAY NEED THE FORMAT 3 DSCB.     * 00001500
*              CATALOG INFORMATION IS OBTIANED VIA LOCATE.  SOME      * 00001600
*              OF THE DSCB INFORMATION IS CONVERTED HERE.             * 00001700
*                                                                     * 00001800
* INPUT -      VTOC COMMON AREA ( VTOCOM )                            * 00001900
*              POINTED TO BY REGISTER 1                               * 00002000
*              USE PARSE DATA, CURRENT FORMATTED DSCB, LOCATE         * 00002100
*                                                                     * 00002200
* OUTPUT -     THE FORMATTED DSCB INFORMATION WITH ITS ADDRESS IN     * 00002300
*              FORMATAD.                                              * 00002400
*                                                                     * 00002500
* ATTRIBUTES - REENTRANT, REUSEABLE, REFRESHABLE.                     * 00002600
*                                                                     * 00002700
*                                                                     * 00002800
*         PROGRAMMED BY R. L. MILLER  (415) 485-6241                  * 00002900
*                                                                     * 00003000
*                                                                     * 00003100
*********************************************************************** 00003200
*                                                                       00003300
         EJECT                                                          00003400
         MACRO                                                          00003500
&LAB     DS1TST  &FIELD,&VALUE,&CODE                                    00003600
&LAB     TM    DS1&FIELD,X'&VALUE'  TEST IT                             00003700
         BNO   D&SYSNDX       IF NOT THERE, SKIP ALONG                  00003800
         MVC   VTF&FIELD,=CL3'&CODE'                                    00003900
D&SYSNDX DS    0H                                                       00004000
         MEND                                                           00004100
*                                                                       00004200
*                                                                       00004300
         EJECT                                                          00004400
VTOCFORM ENTER 12,16          DO THE HOUSEKEEPING                       00004500
         LR    R11,R1         SAVE ADDR OF VTOCOM                       00004600
         USING VTOCOM,R11     SET ITS ADDRESSABILITY                    00004700
         L     R9,ADDRANSR    POINT TO THE PARSE ANSWER                 00004800
         USING PDL,R9         SET ITS ADDRESSABILITY                    00004900
         USING FORMWORK,R13   SET ADDRESSABILITY FOR LOCAL WORK AREA    00005000
         SPACE 3                                                        00005100
*                                                                       00005200
*        CHECK FOR THE FIRST TIME THROUGH                               00005300
*        IF SO, PERFORM SOME INITIALIZATION                             00005400
*                                                                       00005500
         CLI   FIRSTFRM,0     IS THIS THE FIRST TIME?                   00005600
         BNE   GETAREA        NO, KEEP ON TRUCKIN'                      00005700
*                                                                       00005800
*        ROUTINE INITIALIZATION                                         00005900
*                                                                       00006000
         MVI   FIRSTFRM,255   NOTE THE INITIALIZATION AS DONE           00006100
         MVC   CAMLOC(CAMLEN),CAMCONST  SET UP THE CAMLST               00006200
*                                                                       00006300
*        FIND OR GET AN AREA FOR THE FORMATTED DSCB                     00006400
*              FIRST SEE HOW BIG IT IS                                  00006500
*                                                                       00006600
GETAREA  L     R7,DSCBADDR    POINT TO THE DSCB                         00006700
         LA    R7,8(R7)       GET PAST THE HEADER                       00006800
         USING DSCB1,R7       SET ADDRESSABILITY                        00006900
         LH    R1,DSNLEN     GET THE DSNAME LENGTH                      00007000
         LA    R4,VTFMTL(R1)  GET THE FORMATTED DSCB LENGTH             00007100
*                                                                       00007200
*        SEE IF THE CURRENT BLOCK CAN HANDLE IT                         00007300
*                                                                       00007400
FORMFIT  L     R3,VTCCURLN    GET THE CURRENT AVAILABLE                 00007500
         SR    R3,R4          SEE IF IT WILL FIT                        00007600
         BM    GOGETMN        NO, GET ANOTHER BLOCK                     00007700
*                                                                       00007800
*        NO SWEAT, GET THE SPACE FROM THIS BLOCK                        00007900
*                                                                       00008000
         ST    R3,VTCCURLN    STORE THE NEW ( REDUCED ) CURRENT LENGTH  00008100
         L     R3,VTCCURAD    POINT TO THE CURRENT ADDRESS              00008200
         LA    R5,0(R3,R4)    POINT TO THE END OF THE BLOCK             00008300
         ST    R5,VTCCURAD    AND PLACE THE NEW AVAILABLE ADDRESS       00008400
*                                                                       00008500
*        NOW FILL IN THE DATA IN THE FORMATTED DSCB                     00008600
*                                                                       00008700
         USING VTFMT,R3       SET FORMATTED DSCB ADDRESSABILITY         00008800
         ST    R3,FORMATAD    SAVE THIS BLOCK'S ADDRESS                 00008900
         XC    VTFNEXT,VTFNEXT  CLEAR THE SORT POINTER                  00009000
         MVC   VTFVOLUM,VOLID SAVE THE VOLUME SERIAL NUMBER             00009100
         LH    R1,DSNLEN     GET THE LENGTH OF THE DSNAME               00009200
         STH   R1,VTFDSNL     SAVE THE DSNAME LENGTH                    00009300
         BCTR  R1,0           SUBTRACT ONE FOR THE EX                   00009400
         EX    R1,MOVEDSN     MOVE IN THE DSNAME                        00009500
         MVC   VTFNOEPV,DS1NOEPV  NUMBER OF EXTENTS                     00009600
         MVC   VTFLRECL,DS1LRECL  LOGICAL RECORD LENGTH                 00009700
         MVC   VTFBLKSZ,DS1BLKL   BLOCK SIZE                            00009800
*                                                                       00009900
*     MOVE IN THE CREATION DATE, EXPIRATION DATE, AND LAST ACCESS DATE  00010000
*                                                                       00010100
         MVC   VTFCREDT,DS1CREDT  MOVE OVER CREATION DATE               00010200
         MVC   VTFEXPDT,DS1EXPDT  MOVE OVER EXPIRATION DATE             00010300
         MVC   VTFLSTAC,DS1REFD   MOVE OVER LAST ACCESS DATE            00010400
*                                                                       00010500
*        FORMAT THE RECORD FORMAT INTO CHARACTERS                       00010600
*                                                                       00010700
*                                                                       00010800
         MVC   VTFRECFM,BLANKS  BLANK THE FIELD TO START                00010900
         MVC   VTFACTON,BLANKS  ANOTHER BLANK FIELD                     00011000
         MVI   VTFDSTYP,C' ' AND STILL ANOTHER                          00011100
         LA    R2,VTFRECFM    POINT TO THE FIELD                        00011200
         TM    DS1RECFM,X'C0' UNKNOWN RECFM?                            00011300
         BZ    RECFM2         YES, TROUBLE                              00011400
         TM    DS1RECFM,X'40' IS IT FIXED?                              00011500
         BNZ   RECFM3         NO, KEEP TRYING                           00011600
         MVI   0(R2),C'F'     YES, SET UP THE FIRST CHAR                00011700
         LA    R2,1(R2)       AND BUMP THE POINTER                      00011800
         B     RECFM2         CHECK OTHER ATTRIBUTES                    00011900
RECFM3   TM    DS1RECFM,X'80' SEE IF IT'S V OR U                        00012000
         BZ    RECFM4         VARIABLE RECFM                            00012100
         MVI   0(R2),C'U'     RECFM = U                                 00012200
         B     RECFM4A        ADD TO THE POINTER AND KEEP LOOKING       00012300
RECFM4   MVI   0(R2),C'V'     VARIABLE                                  00012400
RECFM4A  LA    R2,1(R2)       GET PAST THIS CHAR                        00012500
RECFM2   DS    0H                                                       00012600
RECFM5   TM    DS1RECFM,X'10' IS IT BLOCKED?                            00012700
         BZ    RECFM6         NO, SKIP ON                               00012800
         MVI   0(R2),C'B'     YES, SET THE SYMBOL                       00012900
         LA    R2,1(R2)       GET PAST THE CHAR                         00013000
RECFM6   TM    DS1RECFM,X'08' IS IT SPANNED OR STANDARD?                00013100
         BZ    RECFM6A        NO                                        00013200
         MVI   0(R2),C'S'     YES, SET IT                               00013300
         LA    R2,1(R2)       GET PAST THIS CHARACTER                   00013400
RECFM6A  TM    DS1RECFM,X'20' CHECK TRACK OVERFLOW                      00013500
         BZ    RECFM7         NO DICE                                   00013600
         MVI   0(R2),C'T'     YES, SET IT                               00013700
         LA    R2,1(R2)       PUSH THE POINTER ON                       00013800
RECFM7   TM    DS1RECFM,X'04' IS IT ASA CONTROL                         00013900
         BZ    RECFM8         NO, SKIP ON                               00014000
         MVI   0(R2),C'A'     YES, SET IT                               00014100
         LA    R2,1(R2)       GET PAST THIS CHAR                        00014200
RECFM8   TM    DS1RECFM,X'02' HOW ABOUT MACHINE CARRIAGE CONTROL        00014300
         BZ    RECFM9         NO, SKIP ON                               00014400
         MVI   0(R2),C'M'     YES, SET IT                               00014500
RECFM9   DS    0H                                                       00014600
*                                                                       00014700
*        FORMAT THE DSORG                                               00014800
*                                                                       00014900
         MVC   VTFDSORG,=CL3'   '  CLEAR THE FIELD                      00015000
         DS1TST DSORG,80,IS   TRY ISAM                                  00015100
         DS1TST DSORG,40,PS   TRY SEQUENTIAL                            00015200
         DS1TST DSORG,20,DA   TRY DIRECT ACCESS                         00015300
         DS1TST DSORG,02,PO   TRY PARTITIONED                           00015400
         CLC   DS1DSORG(2),=X'0008'  IS IT VSAM?                        00015500
         BNE   DSORG05       NO, KEEP LOOKING                           00015600
         MVC   VTFDSORG,=CL3'VS ' YES, FLAG IT                          00015700
DSORG05  TM    DS1DSORG,X'01'      IS IT UNMOVEABLE?                    00015800
         BNO   DSORG06       NO, KEEP ON TRUCKIN'                       00015900
         MVI   VTFDSORG+2,C'U'     YES, NOTE IT                         00016000
DSORG06  DS    0H                                                       00016100
*                                                                       00016200
*        FORMAT THE SECONDARY ALLOCATION                                00016300
*                                                                       00016400
         SR    R1,R1          CLEAR A WORK REGISTER                     00016500
         IC    R1,DS1SCALO    GET THE ALLOCATION FLAG                   00016600
         SRL   R1,6           REMOVE THE BOTTOM 6 BITS ( 75 CENTS )     00016700
         IC    R2,SECAL(R1)   GET THE CHARACTER CODE                    00016800
         STC   R2,VTFSECAL    AND SAVE IT FOR LATER                     00016900
         MVC   VTFSECAM,DS1SCALO+2  SAVE THE SECONDARY AMOUNT TOO       00017000
         MVI   VTFROUND,C'N'  SET CODE FOR NO ROUND                     00017100
         TM    DS1SCALO,X'01' SEE IF ROUND WAS SET                      00017200
         BNO   PROTFORM       NO, THE CODE IS SET RIGHT                 00017300
         MVI   VTFROUND,C'R'  YES, RESET THE CODE                       00017400
*                                                                       00017500
*        FORMAT THE PASSWORD PROTECTION                                 00017600
*                                                                       00017700
PROTFORM TM    DS1DSIND,X'14' CHECK THE PASSWORD BITS                   00017800
         BO    PROTWRIT       WRITE PROTECT IS X'14'                    00017900
         BM    PROTREAD       READ PROTECT IS X'10'                     00018000
         MVI   VTFPROT,C'N'   NO PASSWORD PROTECTION                    00018100
         B     PROTEND        END OF PROTECTION FORMATTING              00018200
PROTWRIT MVI   VTFPROT,C'W'   SET CODE FOR WRITE PROTECT                00018300
         B     PROTEND        THEN CHECK OTHER ITEMS                    00018400
PROTREAD MVI   VTFPROT,C'R'   SET CODE FOR READ/WRITE PROTECT           00018500
PROTEND  DS    0H             END OF PROTECTION FORMATTING              00018600
*                                                                       00018700
*        FORMAT THE CATLG                                               00018800
*                                                                       00018900
         MVI   VTFCATLG,C' ' INITIALIZE IT TO BLANKS                    00019000
         CLI   CATK+1,0       SHOULD WE DO THE LOCATE?                  00019100
         BE    CATEND         NO, SKIP PAST IT                          00019200
*                                                                       00019300
*        SET UP THE CAMLST                                              00019400
*                                                                       00019500
         LA    R1,DS1DSNAM    POINT TO THE DSNAME                       00019600
         ST    R1,CAMLOC+4    SAVE IT IN THE CAMLST                     00019700
         LA    R1,LOCWORK     LOCATE WORKAREA                           00019800
         ST    R1,CAMLOC+12   SAVE IT IN THE CAMLST                     00019900
         LOCATE CAMLOC        CHECK THE CATALOG                         00020000
         LTR   R15,R15        TEST THE CATALOG RETURN CODE              00020100
         BZ    CATOK          ZERO, THERE IS AN ENTRY                   00020200
         MVI   VTFCATLG,C'N'  SET CODE FOR NOT CATALOGED                00020300
         CH    R15,H8         SEE IF THAT'S THE CASE                    00020400
         BE    CATEND         YES, LET IT STAND                         00020500
         MVI   VTFCATLG,C'E'  CATALOG ERROR, PROBLEMS                   00020600
*                                                                       00020700
*        CATALOG ENTRY IS THERE, SEE THAT THE VOLUME IS THIS ONE        00020800
*                                                                       00020900
CATOK    MVI   VTFCATLG,C'C'  SET UP AS A GOOD ENTRY                    00021000
         CLC   VOLID,LOCWORK+6  COMPARE THE VOLUME SERIAL NUMBERS       00021100
         BE    CATEND         GOOD, WE'RE DONE                          00021200
         MVI   VTFCATLG,C'W'  WRONG VOLUME, NOT CATALOGED               00021300
CATEND   DS    0H                                                       00021400
*                                                                       00021500
*        FORMAT THE ALLOCATION AND USED QUANTITIES                      00021600
*                                                                       00021700
         SPACE                                                          00021800
*        CHECK THROUGH THE EXTENTS                                      00021900
         SPACE                                                          00022000
         SR    R2,R2          CLEAR A WORK REGISTER                     00022100
         ICM   R2,B'0001',DS1NOEPV  GET THE NUMBER OF EXTENTS           00022200
         BZ    SPACEND        NO EXTENTS MEANS NO SPACE                 00022300
         SR    R4,R4          ZERO THE SPACE COUNTER FOR THE DATA SET   00022400
*                                                                       00022500
*        GET EACH EXTENT AND PROCESS IT                                 00022600
*                                                                       00022700
         SR    R6,R6          FIRST EXTENT                              00022800
EXTNEXT  LR    R5,R6          GET THE CURRENT EXTENT NUMBER             00022900
         SLL   R5,2           MULTIPLY IT BY FOUR                       00023000
         EX    R0,GETEXT(R5)  GET THE CORRECT ADDRESS                   00023100
*                                                                       00023200
*        PROCESS THIS EXTENT                                            00023300
*                                                                       00023400
         USING XTDSECT,R5     SET ADDRESSABILITY                        00023500
         CLI   XTFLAGS,XTNOEXT  IS THERE AN EXTENT                      00023600
         BE    NOEXT          NO, THE EXTENT ISN'T THERE                00023700
         CLI   XTFLAGS,XTCYLBD  IS IT ON CYLINDER BOUNDARIES            00023800
         BNE   FORMALOC       NO, DO IT FOR CYLS AND TRACKS             00023900
*                                                                       00024000
*        CYLINDER BOUNDS - BE SURE THE ALLOCATION IS CORRECT            00024100
*                                                                       00024200
         ICM   R1,B'0011',XTLOWHH GET THE LOWER TRACK                   00024300
         BZ    LOWOK          IT'S ZERO                                 00024400
         MVC   VTFACTON(6),=C'CYLERR'  NOTE THE ERROR                   00024500
         MVI   VTFACTON+6,C'L'  ON THE LOW CCHH                         00024600
LOWOK    LH    R1,XTHIHH      GET THE HIGH TRACK                        00024700
         LA    R1,1(R1)       ADD ONE FOR ZERO ADDRESSING               00024800
         CH    R1,DS4DEVSZ+2  IS THIS THE NUMBER OF TRACKS/CYL          00024900
         BE    FORMALOC       YES, GO CALCULATE                         00025000
         MVC   VTFACTON(6),=C'CYLERR'  NOTE THE ERROR                   00025100
         MVI   VTFACTON+7,C'H'  ON THE HIGH CCHH                        00025200
*                                                                       00025300
*        GET THE SPACE FOR NON-CYLINDER ALLOCATIONS                     00025400
*                                                                       00025500
FORMALOC LH    R1,XTHICC      GET THE HIGH CYLINDER                     00025600
         SH    R1,XTLOWCC     MINUS THE LOW CYLINDER                    00025700
         MH    R1,DS4DEVSZ+2  TIMES THE NUMBER OF TRACKS PER CYLINDER   00025800
         LH    R8,XTHIHH      GET THE HIGH TRACK                        00025900
         SH    R8,XTLOWHH     MINUS THE LOW TRACK                       00026000
         AR    R8,R1          TRACKS IN THIS EXTENT ( MINUS 1 )         00026100
         LA    R4,1(R4,R8)    ADD THE TRACKS TOGETHER FOR THIS DATA SET 00026200
*                                                                       00026300
*        GET THE NEXT EXTENT                                            00026400
*                                                                       00026500
NOEXT    LA    R6,1(R6)       INCREMENT THE EXTENT COUNTER              00026600
         CR    R6,R2          CHECK FOR THE END                         00026700
         BL    EXTNEXT        NOT YET, KEEP GOING                       00026800
*                                                                       00026900
*        ALL THE EXTENTS ARE SUMMED REGISTER 4 HAS THE SUM              00027000
*                                                                       00027100
         BAL   R8,SPACUNIT    CHANGE IT TO THE APPROPRIATE UNITS        00027200
         ST    R4,VTFALLOC    STORE IT FOR LATER                        00027300
SPACEND  DS    0H                                                       00027400
*                                                                       00027500
*        GET THE TRACKS USED                                            00027600
*                                                                       00027700
         SR    R4,R4          CLEAR THE TRACK ( WOO WOO )               00027800
         CLC   DS1LSTAR,ZEROES IS THE TRACK USED COUNTER SET?           00027900
         BNE   USEDOK         YES, ACCEPT IT                            00028000
*        NO, SEE IF THE ZERO IS VALID                                   00028100
         TM    DS1DSORG,X'40' IS IT SEQUENTIAL?                         00028200
         BO    USEDOK0        YES,THE ZERO IS VALID                     00028300
         CLC   DSORG(4),ZEROES  MAYBE IT WASN'T EVER OPENED             00028400
         BE    USEDOK0        THEN NO SPACE USED IS OK                  00028500
         TM    DS1DSORG,X'0C' CHECK FOR AN INVALID DSORG                00028600
         BO    USEDOK0        NO SPACE USED IS STILL OK                 00028700
         MVC   VTFUSED,FMIN1  SET A FLAG UNUSED SPACE UNKNOWN           00028800
         B     USEDEND        USED SPACE IS SET                         00028900
*                                                                       00029000
*        THE TRACKS USED COUNTER SEEMS OK                               00029100
*                                                                       00029200
USEDOK   LH    R4,DS1LSTAR    GET THE LAST TRACK USED                   00029300
         LA    R4,1(R4)       ADD ONE ( ZERO ADDRESSING )               00029400
         BAL   R8,SPACUNIT    CONVERT TO APPROPRIATE UNITS              00029500
USEDOK0  ST    R4,VTFUSED     SAVE THE AMOUNT OF SPACE USED             00029600
USEDEND  DS    0H                                                       00029700
         L     R14,VTFALLOC   ALLOCATED TRACKS                          00029800
         S     R14,VTFUSED    MINUS USED TRACKS                         00029900
         ST    R14,VTFUNUSD   EQUALS UNUSED TRACKS                      00030000
         SR    R14,R14                                                  00030100
         SR    R15,R15                                                  00030200
         CLC   VTFALLOC(4),=F'0'                                        00030300
         BE    USEDEND1                                                 00030400
         L     R15,VTFUSED    USED TRACKS                               00030500
         M     R14,=F'100'    MULT BY 100 TO GET PCT                    00030600
         D     R14,VTFALLOC   DIVIDE BY ALLOC TO GET PCT USED           00030700
USEDEND1 STH   R15,VTFPCT     SAVE PCT USED                             00030800
*                                                                       00030900
*        RETURN                                                         00031000
*                                                                       00031100
FORMRET  LEAVE EQ,RC=0                                                  00031200
*                                                                       00031300
*                                                                       00031400
         EJECT                                                          00031500
*                                                                       00031600
*        ROUTINES USED ABOVE                                            00031700
*                                                                       00031800
*                                                                       00031900
*        CONVERT FROM TRACKS TO THE APPROPRIATE UNITS                   00032000
*              KBYTES, MBYTES, TRKS, OR CYLS                            00032100
*                                                                       00032200
SPACUNIT LH    R1,SPACEK      GET THE UNIT TYPE                         00032300
         SLL   R1,2           MULTIPLY BY 4                             00032400
         B     *+4(R1)        THEN BRANCH TO THE CORRECT ROUTINE        00032500
         B     SPACTRK        R1=0  KILOBYTES                           00032600
         B     SPACTRK        R1=1  KILOBYTES                           00032700
         B     SPACTRK        R1=2  MEGABYTES                           00032800
         B     SPACTRK        R1=3  TRACKS                              00032900
         B     SPACTRK        R1=4  CYLINDERS                           00033000
*        TRACKS                                                         00033100
SPACTRK  BR    R8             WAS SET WHEN WE STARTED                   00033200
*        CYLINDERS                                                      00033300
SPACCYL  SR    R0,R0          CLEAR A REGISTER                          00033400
         LR    R1,R4          GET THE NUMBER OF TRACKS                  00033500
         LH    R4,DS4DEVSZ+2  GET THE NUMBER OF TRACKS PER CYLINDER     00033600
         SRL   R4,2           DIVIDE BY 2 FOR ROUNDING                  00033700
         AR    R1,R4          ADD IT IN                                 00033800
         LH    R4,DS4DEVSZ+2  GET THE NUMBER OF TRACKS PER CYLINDER     00033900
         DR    R0,R4          DIVIDE TO GET ROUNDED CYLINDERS           00034000
         LR    R4,R1          GET THE ANSWER BACK INTO R4               00034100
         BR    R8             THEN RETURN                               00034200
*        KILOBYTES                                                      00034300
SPACKB   MH    R4,DS4DEVTK    MULTIPLY BY BYTES PER TRACK               00034400
         SR    R0,R0          CLEAR THE TOP                             00034500
         LR    R1,R4          GET THE NUMBER TO DIVIDE                  00034600
         A     R1,F500        ADD UP TO ROUND                           00034700
         D     R0,F1000       DIVIDE TO GET KILOBYTES                   00034800
         LR    R4,R1          GET THE ANSWER BACK INTO R4               00034900
         BR    R8             THEN RETURN                               00035000
*        MEGABYTES                                                      00035100
SPACMB   MH    R4,DS4DEVTK    MULTIPLY BY BYTES PER TRACK               00035200
         SR    R0,R0          CLEAR THE TOP                             00035300
         LR    R1,R4          GET THE NUMBER TO DIVIDE                  00035400
         A     R1,F500000     ADD UP TO ROUND                           00035500
         D     R0,F1000000    DIVIDE TO GET MEGABYTES                   00035600
         LR    R4,R1          GET THE ANSWER BACK INTO R4               00035700
         BR    R8             THEN RETURN                               00035800
*                                                                       00035900
*        GET A NEW BLOCK OF MAIN STORAGE                                00036000
*                                                                       00036100
GOGETMN  GETMAIN R,LV=VTCGETMS  GET SOME                                00036200
         ST    R1,VTCCURAD    SET UP THE AVAILABLE ADDRESS              00036300
         LA    R2,VTCGETMS/1024   GET THE SIZE OF THE BLOCK IN K        00036400
         SLL   R2,10          GET IT INTO BYTES ( TIMES 1024 )          00036500
         ST    R2,VTCCURLN    SO THE FORMATTED DSCB'S CAN USE IT        00036600
*                                                                       00036700
*        SAVE THE BLOCK ADDRESS IN THE VTCGETMN TABLE                   00036800
*                                                                       00036900
         LA    R2,VTCGETMN    POINT TO THE TABLE                        00037000
         LA    R5,VTCGETMX    GET THE NUMBER OF ENTRIES IN THE TABLE    00037100
GOGETTAB ICM   R3,B'1111',0(R2) GET THIS ENTRY                          00037200
         BNZ   GOGETINC       IF NOT ZERO, KEEP LOOKING                 00037300
         ST    R1,0(R2)       SAVE THE NEW ENTRY                        00037400
         B     FORMFIT        THEN GO ALLOCATE A FORMATTED DSCB         00037500
*                                                                       00037600
*        THIS ENTRY WAS TAKEN, GET THE NEXT ONE                         00037700
*                                                                       00037800
GOGETINC LA    R2,4(R2)       POINT TO THE NEXT ENTRY                   00037900
         BCT   R5,GOGETTAB    COUNT AND LOOP                            00038000
*                                                                       00038100
*        TABLE OVERFLOW  - ISSUE ERROR MSG                              00038200
*              SET A FLAG TO STOP INPUT                                 00038300
*                                                                       00038400
         VTOCMSG TABOVFLW,TABOVSEC  ISSUE A MESSAGE                     00038500
         MVI   TABFULL,255    SET A STOP FLAG                           00038600
         B     FORMRET        RETURN FROM FORMATTING                    00038700
         EJECT                                                          00038800
*                                                                       00038900
*                                                                       00039000
*                                                                       00039100
*        PROGRAM CONSTANTS                                              00039200
*                                                                       00039300
         SPACE                                                          00039400
*        INSTRUCTIONS EXECUTED TO GET THE NEXT EXTENT                   00039500
GETEXT   LA    R5,DS1EXT1        1ST EXTENT                             00039600
         LA    R5,DS1EXT2        2ND EXTENT                             00039700
         LA    R5,DS1EXT3        3RD EXTENT                             00039800
         LA    R5,DS3EXTNT+00    4TH EXTENT                             00039900
         LA    R5,DS3EXTNT+10    5TH EXTENT                             00040000
         LA    R5,DS3EXTNT+20    6TH EXTENT                             00040100
         LA    R5,DS3EXTNT+30    7TH EXTENT                             00040200
         LA    R5,DS3ADEXT+00    8TH EXTENT                             00040300
         LA    R5,DS3ADEXT+10    9TH EXTENT                             00040400
         LA    R5,DS3ADEXT+20   10TH EXTENT                             00040500
         LA    R5,DS3ADEXT+30   11TH EXTENT                             00040600
         LA    R5,DS3ADEXT+40   12TH EXTENT                             00040700
         LA    R5,DS3ADEXT+50   13TH EXTENT                             00040800
         LA    R5,DS3ADEXT+60   14TH EXTENT                             00040900
         LA    R5,DS3ADEXT+70   15TH EXTENT                             00041000
         LA    R5,DS3ADEXT+80   16TH EXTENT                             00041100
MOVEDSN  MVC   VTFDSN(0),DS1DSNAM   EXECUTED COMPARE                    00041200
ZEROES   DC    2F'0'                                                    00041300
FMIN1    DC    F'-1'                                                    00041400
F500     DC    F'500'                                                   00041500
F1000    DC    F'1000'                                                  00041600
F500000  DC    F'500000'                                                00041700
F1000000 DC    F'1000000'                                               00041800
BLANKS   DC    CL8'                '                                    00041900
CAMCONST CAMLST NAME,*,,*                                               00042000
H8       DC    H'8'                                                     00042100
SECAL    DC    C'ABTC'        SECONDARY ALLOCATION CODES                00042200
*              ABSOLUTE TRK, BLOCKS, TRACKS, CYLINDERS                  00042300
*                                                                       00042400
*                                                                       00042500
*                                                                       00042600
         PRINT NOGEN                                                    00042700
*                                                                       00042800
*        PROGRAM MESSAGES                                               00042900
*                                                                       00043000
TABOVFLW MSG   ' THE VTOC TABLES (1.6 MEG) ARE NOT LARGE ENOUGH TO HANDX00043100
               LE THIS REQUEST'                                         00043200
TABOVSEC MSG   ' PARTIAL PROCESSING WILL CONTINUE '                     00043300
*                                                                       00043400
*                                                                       00043500
*                                                                       00043600
*                                                                       00043700
*                                                                       00043800
*                                                                       00043900
         EJECT                                                          00044000
*                                                                       00044100
*                                                                       00044200
*        P A R S E   C O N T R O L   L I S T                            00044300
*                                                                       00044400
*                                                                       00044500
         PRINT OFF                                                      00044600
         COPY  VTOCPARS                                                 00044700
         PRINT ON                                                       00044800
*                                                                       00044900
*        DYNAMIC WORK AREA                                              00045000
*                                                                       00045100
         SPACE 3                                                        00045200
FORMWORK DSECT                                                          00045300
         DS    18A            PRINT ROUTINE SAVE AREA                   00045400
FIRSTFRM DS    X              INITIALIZATION FOR THIS ROUTINE           00045500
CHARS    DS    CL16           CONVERSION TO CHARACTERS                  00045600
CAMLOC   CAMLST NAME,*,,*                                               00045700
CAMLEN   EQU   *-CAMLOC                                                 00045800
         DS    0D                                                       00045900
LOCWORK  DS    265C                                                     00046000
         SPACE                                                          00046100
         DS    0D                                                       00046200
LENWORK  EQU   *-FORMWORK                                               00046300
*                                                                       00046400
*        VTOC COMMAND COMMON AREA                                       00046500
*                                                                       00046600
         PRINT NOGEN                                                    00046700
         VTOCOM                                                         00046800
         SPACE 3                                                        00046900
*                                                                       00047000
*        FORMATTED DSCB                                                 00047100
*                                                                       00047200
         PRINT GEN                                                      00047300
         VTFMT                                                          00047400
         PRINT NOGEN                                                    00047500
         SPACE 3                                                        00047600
         PDEDSNAM                                                       00047700
         SPACE 3                                                        00047800
         SPACE 3                                                        00047900
DSCB1    DSECT                                                          00048000
         IECSDSL1 1                                                     00048100
         SPACE 3                                                        00048200
*        FORMAT 1 AND 3 EXTENT DESCRIPTION                              00048300
XTDSECT  DSECT                                                          00048400
XTFLAGS  DS    X                                                        00048500
XTNOEXT  EQU   X'00'          NO EXTENT                                 00048600
XTDATAB  EQU   X'01'          DAT BLOCKS                                00048700
XTOVFLW  EQU   X'02'          OVERFLOW AREA                             00048800
XTINDEX  EQU   X'04'          INDEX AREA                                00048900
XTUSRLAB EQU   X'40'          USER LABEL EXTENT                         00049000
XTSHRCYL EQU   X'80'          SHARING CYLINDERS                         00049100
XTCYLBD  EQU   X'81'          CYLINDER BOUNDARIES                       00049200
XTSEQ    DS    X              EXTENT SEQUENCE NUMBER                    00049300
XTLOWCC  DS    H              LOWER CYLINDER                            00049400
XTLOWHH  DS    H              LOWER TRACK                               00049500
XTHICC   DS    H              UPPER CYLINDER                            00049600
XTHIHH   DS    H              UPPER TRACK                               00049700
         END                                                            00049800
./       ADD   NAME=VTOCMSG
         MACRO                                                          04460000
&LAB     VTOCMSG &MSG1,&MSG2    FIRST LEVEL MESSAGE, OPTIONAL SECOND    04470000
&LAB     LA    R1,&MSG1       POINT TO THE FIRST MESSAGE                04480000
         AIF   ('&MSG2' EQ '').NOSEC  IF NO SECOND LEVEL MSG            04490000
         LA    R0,&MSG2       POINT TO THE SECOND MESSAGE               04500000
         AGO   .SETMSG        SET UP THE MESSAGES                       04510000
.NOSEC   SR    R0,R0          NO SECOND LEVEL MESSAGE                   04520000
.SETMSG  STM   R0,R1,MSGADDRS SAVE THE MESSAGE ADDRESSES                04530000
*        THEN JUST CALL THE MESSAGE ISSUING ROUTINE                     04540000
         VTCALL MSG           AWAY WE GO                                04550000
         MEND                                                           04560000
./       ADD   NAME=VTOCMSGX
*                                                                       00000100
*   VTOC ERROR MESSAGE ROUTINE, R1 POINTS TO VTOC COMMON AT ENTRY       00000200
*                                                                       00000300
VTOCMSG  ENTER 12,0           DO THE STANDARD HOUSEKEEPING              00000400
         LR    R11,R1         GET THE PARM REGISTER                     00000500
         USING VTOCOM,R11     SET ADDRESSABILITY                        00000600
         SPACE                                                          00000700
         LM    R0,R1,MSGADDRS GET THE MESSAGE(S) TO SEND                00000800
         LTR   R0,R0          SECOND LEVEL MSG?                         00000900
         BZ    ERRORM1        NO                                        00001000
         SPACE                                                          00001100
         MVC   MSGTEXT1,0(R1) INSURE MSG IN WORK AREA                   00001200
         LA    R1,MSGTEXT1                                              00001300
         SPACE                                                          00001400
         LH    R14,0(R1)      LENGTH OF FIRST LEVEL MSG                 00001500
         LA    R15,0(R14,R1)  ADDR OF END OF MSG                        00001600
         LA    R14,1(R14)     JUMP MSG LENGTH                           00001700
         STH   R14,0(R1)                                                00001800
         MVI   0(R15),C'+'    INDICATE SECOND LEVEL MSG EXISTS          00001900
         SPACE 2                                                        00002000
         SR    R14,R14        CLEAR CHAIN FIELD                         00002100
         LA    R15,1          ONE SEGMENT IN 2ND MSG                    00002200
         STM   R14,R0,PUTOLD2 CREATE SECOND-LEVEL                       00002300
*                             OUTPUT LINE DESCRIPTOR ('OLD')            00002400
         LA    R0,PUTOLD2                                               00002500
         SPACE 3                                                        00002600
ERRORM1  LR    R14,R0         NEXT 'OLD' ADDR OR ZERO                   00002700
         LA    R15,1          ONE SEGMENT                               00002800
         LR    R0,R1          MSG ADDR                                  00002900
         STM   R14,R0,PUTOLD1 FIRST LEVEL 'OLD'                         00003000
         SPACE                                                          00003100
         LA    R1,PARMLIST                                              00003200
         USING IOPL,R1                                                  00003300
         SPACE                                                          00003400
         MVC   IOPLECT,ADDRECT                                          00003500
         MVC   IOPLUPT,ADDRUPT                                          00003600
         SPACE                                                          00003700
         LA    R0,ATTNECB                                               00003800
         ST    R0,IOPLECB                                               00003900
         MVI   ATTNECB,0                                                00004000
         SPACE 3                                                        00004100
         XC    PARMLIST+16(4),PARMLIST+16                               00004200
         PUTLINE PARM=PARMLIST+16,MF=(E,(1)),                          X00004300
               OUTPUT=(PUTOLD1,TERM,MULTLVL,INFOR)                      00004400
         SPACE 3                                                        00004500
         LEAVE EQ                                                       00004600
         SPACE 3                                                        00004700
         IKJIOPL                                                        00004800
         SPACE 3                                                        00004900
         VTOCOM                                                         00005000
         END                                                            00005100
./       ADD   NAME=VTOCOM
         MACRO                                                          04580000
         VTOCOM  &TYPE                                                  04590000
*                                                                       04600000
*        THIS IS THE VTOC COMMAND COMMON AREA                           04610000
*                                                                       04620000
         AIF   ('&TYPE' EQ 'NODSECT').NODSECT                           04630000
VTOCOM   DSECT                                                          04640000
         AGO   .NODS                                                    04650000
.NODSECT ANOP                                                           04660000
VTOCOM   DS    0D                                                       04670000
.NODS    ANOP                                                           04680000
*                                                                       04690000
*        WORKING STORAGE AREAS FOR THE VARIOUS ROUTINES                 04700000
*                                                                       04710000
VTCWMSG  DS    A              WORKING STORAGE FOR THE MSG  ROUTINE      04720000
VTCWEXIT DS    A              WORKING STORAGE FOR THE EXIT ROUTINE      04730000
VTCWEXCP DS    A              WORKING STORAGE FOR THE EXCP ROUTINE      04740000
VTCWCHEK DS    A              WORKING STORAGE FOR THE CHEK ROUTINE      04750000
VTCWFORM DS    A              WORKING STORAGE FOR THE FORM ROUTINE      04760000
VTCWPRNT DS    A              WORKING STORAGE FOR THE PRNT ROUTINE      04770000
VTCWSORT DS    A              WORKING STORAGE FOR THE SORT ROUTINE      04780000
*                                                                       04790000
*        ADDRESSES OF THE ROUTINES                                      04800000
*                                                                       04810000
VADMSG   DC    V(VTOCMSG)     ADDRESS OF THE MESSAGE ROUTINE            04820000
VADEXIT  DC    V(VTOCEXIT)    ADDRESS OF THE EXIT ROUTINE               04830000
VADEXCP  DC    V(VTOCEXIT)    ADDRESS OF THE EXCP ROUTINE               04840000
VADCHEK  DC    V(VTOCEXIT)    ADDRESS OF THE CHECK ROUTINE              04850000
VADFORM  DC    V(VTOCEXIT)    ADDRESS OF THE FORMAT ROUTINE             04860000
VADPRNT  DC    V(VTOCEXIT)    ADDRESS OF THE PRINT ROUTINE              04870000
VADSORT  DC    V(VTOCEXIT)    ADDRESS OF THE SORT ROUTINE               04880000
*                                                                       04890000
*        TSO COMMAND PROCESSOR AND PARSE DATA                           04900000
*                                                                       04910000
ADDRUPT  DS    A              USER PROFILE TABLE                        04920000
ADDRECT  DS    A              ENVIRONMENT CONTROL TABLE                 04930000
ADDRPSCB DS    A              PROTECTED STEP CONTROL BLOCK              04940000
ADDRCBUF DS    A              COMMAND BUFFER                            04950000
         SPACE                                                          04960000
ADDRANSR DS    A              PARSE ANSWER OR PDL ADDRESS               04970000
         SPACE                                                          04980000
PARMLIST DS    8A             INTERNAL PARM AREA ( MSG )                04990000
ATTNECB  DS    F              ECB FOR ATTENTIONS                        05000000
DOUBLE   DS    D                                                        05010000
         SPACE                                                          05020000
MSGADDRS DS    2A             ADDRESSES OF MESSAGES FOR VTOCMSG         05030000
MSGTEXT1 DS    XL124                                                    05040000
MSGTEXT2 DS    XL124                                                    05050000
         SPACE 3                                                        05060000
PUTOLD1  DS    3F                                                       05070000
PUTOLD2  DS    3F                                                       05080000
*                                                                       05090000
*        PARAMETER LIST FOR THE EXIT ROUTINE                            05100000
*                                                                       05110000
EXITLIST DS    0F                                                       05120000
EXITAREA DS    A       WORKAREA LOCATION                                05130000
DSCBADDR DS    A       ADDRESS OF THE DSCB                              05140000
FORMATAD DS    A       ADDRESS OF THE FORMATTED DSCB                    05150000
CPPLADDR DS    A       ADDRESS OF THE CPPL                              05160000
ACTIONAD DS    A       ADDRESS OF THE RECOMMENDED OR REQUESTED ACTION   05170000
*                                                                       05180000
*        INTER ROUTINE FLAGS                                            05190000
*                                                                       05200000
VTCEFUNC DS    X              VTOCEXCP FUNCTION FLAG                    05210000
VTCFMTCK DS    X              FORMAT IS CALLED BY CHECK RTN             05220000
VTCFMTCD EQU   X'80'          FORMAT WAS CALLED BY CHECK                05230000
VTCFMTCC EQU   X'08'          FORMAT WAS CALLED BY CHECK THIS CALL      05240000
*                                                                       05250000
TABFULL  DS    X             FLAG TABLES FULL, STOP INPUT               05260000
LOCAT    DS    X              FLAG TO PERFORM CATALOG LOCATE            05270000
VTCEPRNT DS    X               PRINT END AND CLEANUP FLAG               05280000
DSNLEN   DS    H              LENGTH OF THE DSNAME (NON-BLANKS)         05290000
ATABTITL DS    A              ADDRESS OF TABLE OF TITLES, LENGTHS       05300000
*                                                                       05310000
*                                                                       05320000
*                                                                       05330000
*        WORKING STORAGE FOR VOLUME UCB SEARCH                          05340000
*                                                                       05350000
ADDR     DS    CL3     UCB ADDRESS IN CHARACTERS                        05360000
VOLSER   DS    CL6     VOLUME SERIAL NUMBER FROM PARSE                  05370000
VOLID    DS    CL6     CURRENT VOLUME SERIAL NUMBER TO PROCESS          05380000
FLAG     DS    X       UCB SEARCH FLAG                                  05390000
LASTADR  DS    F       LAST UCB ADDRESS FOUND ( NO DUP'S )              05400000
*                                                                       05410000
*                                                                       05420000
*                                                                       05430000
SORTTAB  DS    16F                                                      05440000
*                                                                       05450000
* EACH ENTRY CONTAINS A KEY OFFSET (2 BYTES) AND A KEY LENGTH (2 BYTES) 05460000
* THIS TABLE IS BUILT AT PARSE TIME ACCORDING TO THE SORT PARAMETERS    05470000
* SPECIFIED. THE 1ST PARM IS THE HIGH KEY AND SO ON.                    05480000
*                                                                       05490000
*                                                                       05500000
*                                                                       05510000
*        ADDRESSES OF GETMAIN FOR FORMATTED DATA                        05520000
*                                                                       05530000
VTCCURAD DS    A             CURRENT AVAILABLE ADDRESS                  05540000
VTCCURLN DS    A             CURRENT AVAILABLE LENGTH                   05550000
VTCGETMN DS    50A           ADDRESSES OF BLOCKS                        05560000
VTCGETMX EQU  (*-VTCGETMN)/4  NUMBER OF BLOCKS  MAXIMUM                 05570000
VTCGETMS EQU   32768          GETMAIN SIZE                              05580000
*                                                                       05590000
*        HASH SORT TABLE, POINTERS TO FIRST ENTRIES                     05600000
*                                                                       05610000
VTCSORTH DS    256A           POINT TO FORMATED ENTRIES                 05620000
VTCSORTE EQU   *              END OF LIST                               05630000
*                                                                       05640000
*        PRINT ENTRIES - PAGE AND LINE COUNTERS                         05650000
*                                                                       05660000
LINECT   DS    H              LINE COUNT                                05670000
LINEMAX  DS    H              MAXIMUM LINES PER PAGE                    05680000
PAGECT   DS    H              PAGE COUNT                                05690000
LINELEN  DS    H              LENGTH OF THE PRINT LINE                  05700000
*                                                                       05710000
*                                                                       05720000
*          VARIOUS ITEMS                                                05730000
*                                                                       05740000
FMT4     DS    XL44           SPACE FOR DSCB NAME                       05750000
         IECSDSL1 4          SAVE EACH FORMAT 4 DSCB                    05760000
         DS    0D                                                       05770000
FMT3     DS    0XL148         SPACE FOR FORMAT3 DSCB                    05780000
         IECSDSL1 3                                                     05790000
         DS    0D                                                       05800000
         MEND                                                           05810000
./       ADD   NAME=VTOCPARS
         PUSH  PRINT                                                    05830000
         PRINT NOGEN                                                    05840000
PCLMAIN  IKJPARM DSECT=PDL                                              05850000
         SPACE 2                                                        05860000
VOLS     IKJPOSIT DSNAME,VOLSER,LIST,                                  $05870000
               PROMPT='VOLUMES TO SEARCH AND OTHER PARAMETERS',        $05880000
               HELP=('VOLUME SERIAL NUMBERS WHICH ARE TO BE SEARCHED FO$05890000
               DATA SETS TO LIST')                                      05900000
         SPACE 2                                                        05910000
LEVKEY   IKJKEYWD                                                       05920000
         IKJNAME 'LEVEL',SUBFLD=SUBLEV                                  05930000
         SPACE 2                                                        05940000
ENDKEY   IKJKEYWD                                                       05950000
         IKJNAME 'ENDING',SUBFLD=SUBEND                                 05960000
         SPACE 2                                                        05970000
CONTAINK IKJKEYWD                                                       05980000
         IKJNAME 'CONTAINING',SUBFLD=SUBCONT                            05990000
         SPACE 2                                                        06000000
         SPACE 2                                                        06010000
SPACEK   IKJKEYWD DEFAULT='TRKS'                                        06020000
         IKJNAME 'TRKS'                                                 06030000
         SPACE 2                                                        06040000
CATK     IKJKEYWD                                                       06050000
         IKJNAME 'CAT'                                                  06060000
         SPACE 2                                                        06070000
SORTK    IKJKEYWD                                                       06080000
         IKJNAME 'SORT',SUBFLD=SUBSORTS                                 06090000
         IKJNAME 'NOSORT'                                               06100000
         SPACE 2                                                        06110000
BREAKK   IKJKEYWD                                                       06120000
         IKJNAME 'BREAK',SUBFLD=SUBBREAK                                06130000
         SPACE 2                                                        06140000
LIMITK   IKJKEYWD                                                       06150000
         IKJNAME 'LIMIT',SUBFLD=SUBLIMIT                                06160000
         SPACE 2                                                        06170000
ANDOR1K  IKJKEYWD                                                       06180000
         IKJNAME 'AND1',SUBFLD=SUBAO1,ALIAS='AND'                       06190000
         IKJNAME 'OR1',SUBFLD=SUBAO1,ALIAS='OR'                         06200000
         SPACE 2                                                        06210000
ANDOR2K  IKJKEYWD                                                       06220000
         IKJNAME 'AND2',SUBFLD=SUBAO2                                   06230000
         IKJNAME 'OR2',SUBFLD=SUBAO2                                    06240000
         SPACE 2                                                        06250000
ANDOR3K  IKJKEYWD                                                       06260000
         IKJNAME 'AND3',SUBFLD=SUBAO3                                   06270000
         IKJNAME 'OR3',SUBFLD=SUBAO3                                    06280000
         SPACE 2                                                        06290000
PRINTK   IKJKEYWD                                                       06300000
         IKJNAME 'PRINT',SUBFLD=SUBPRINT                                06310000
         IKJNAME 'NOPRINT'                                              06320000
         SPACE 2                                                        06330000
CHARSK   IKJKEYWD                                                       06340000
         IKJNAME 'CHARS',SUBFLD=SUBCHARS                                06350000
         SPACE 2                                                        06360000
LINESK   IKJKEYWD                                                       06370000
         IKJNAME 'LINES',SUBFLD=SUBLINES                                06380000
         SPACE 2                                                        06390000
HEADK    IKJKEYWD                                                       06400000
         IKJNAME 'HEADING',SUBFLD=SUBHEAD                               06410000
         IKJNAME 'NOHEADING'                                            06420000
         SPACE 2                                                        06430000
TOTALK   IKJKEYWD                                                       06440000
         IKJNAME 'TOTALS',SUBFLD=SUBTOTAL                               06450000
         SPACE 2                                                        06460000
OUTPUTK  IKJKEYWD                                                       06470000
         IKJNAME 'OUTPUT'                                               06480000
         SPACE 2                                                        06490000
FORMATK  IKJKEYWD                                                       06500000
         IKJNAME 'FORMAT',SUBFLD=SUBFORMT                               06510000
         SPACE 2                                                        06520000
DSNPLNK  IKJKEYWD                                                       06530000
         IKJNAME 'DSNLEN',SUBFLD=SUBDSNLN                               06540000
         SPACE 5                                                        06550000
SUBLEV   IKJSUBF                                                        06560000
LEVEL    IKJPOSIT DSNAME,LIST,                                         X06570000
               PROMPT='BEGINNING CHARACTERS OF DSNAMES TO PROCESS'      06580000
         SPACE 2                                                        06590000
SUBEND   IKJSUBF                                                        06600000
ENDING   IKJPOSIT DSNAME,LIST,                                         X06610000
               PROMPT='ENDING CHARACTERS OF DSNAMES TO PROCESS'         06620000
         SPACE 2                                                        06630000
SUBCONT  IKJSUBF                                                        06640000
CONTAIN  IKJPOSIT DSNAME,LIST,                                         X06650000
               PROMPT='CHARACTER STRING CONTAINED IN DSNAMES TO PROCESSX06660000
               '                                                        06670000
         SPACE 2                                                        06680000
SUBSORTS IKJSUBF                                                        06690000
SUBSORT  IKJIDENT 'SORT FIELDS',LIST,FIRST=ALPHA,MAXLNTH=6              06700000
         SPACE 2                                                        06710000
SUBBREAK IKJSUBF                                                        06720000
BREAK    IKJIDENT 'NUMBER OF CHARACTERS FOR A BREAK',FIRST=NUMERIC,    X06730000
               OTHER=NUMERIC,MAXLNTH=2,DEFAULT='3'                      06740000
         SPACE 2                                                        06750000
SUBCHARS IKJSUBF                                                        06760000
CHARSPL  IKJIDENT 'NUMBER OF CHARACTERS PER LINE   ',FIRST=NUMERIC,    X06770000
               OTHER=NUMERIC,MAXLNTH=3                                  06780000
BLKSZSET IKJIDENT 'PHYSICAL BLOCK SIZE',FIRST=NUMERIC,OTHER=NUMERIC,   X06790000
               MAXLNTH=5                                                06800000
         SPACE 2                                                        06810000
SUBLINES IKJSUBF                                                        06820000
LINESPP  IKJIDENT 'NUMBER OF LINES PER PAGE        ',FIRST=NUMERIC,    X06830000
               OTHER=NUMERIC,MAXLNTH=3                                  06840000
         SPACE 2                                                        06850000
SUBPRINT IKJSUBF                                                        06860000
SUBPRTKY IKJIDENT 'ADD, REP, NEW, OR DEL',                             X06870000
               FIRST=ALPHA,OTHER=ALPHA,MAXLNTH=3                        06880000
SUBPRTIT IKJIDENT 'ITEMS TO PRINT',LIST,FIRST=ALPHA,MAXLNTH=6           06890000
         SPACE 2                                                        06900000
SUBHEAD  IKJSUBF                                                        06910000
HEADING  IKJPOSIT QSTRING                                               06920000
         SPACE 2                                                        06930000
SUBTOTAL IKJSUBF                                                        06940000
TOTALN   IKJIDENT 'NUMBER OF CHARACTERS FOR TOTALS',FIRST=NUMERIC,     X06950000
               OTHER=NUMERIC,MAXLNTH=2,DEFAULT='0'                      06960000
         SPACE 3                                                        06970000
SUBLIMIT IKJSUBF                                                        06980000
SUBLKEY IKJIDENT 'FIELD IN DATA SET CONTROL BLOCK TO COMPARE',         X06990000
               FIRST=ALPHA,OTHER=ALPHANUM,MAXLNTH=8                     07000000
         SPACE 2                                                        07010000
SUBLOPER IKJIDENT 'OPERATOR FOR COMPARISON',FIRST=ALPHA,OTHER=ALPHA,   X07020000
               MAXLNTH=2,                                              X07030000
               PROMPT=' OPERATORS ARE EQ, NE, LT, LE, GT, AND GE'       07040000
         SPACE 2                                                        07050000
SUBLVALU IKJIDENT 'COMPARISON VALUE',FIRST=ALPHANUM,                   X07060000
               OTHER=ALPHANUM,                                         X07070000
               PROMPT='VALUE TO COMPARE FOR DATA SET KEYWORDS'          07080000
         SPACE 3                                                        07090000
SUBAO1   IKJSUBF                                                        07100000
SUB1KEY IKJIDENT 'FIELD IN DATA SET CONTROL BLOCK TO COMPARE',         X07110000
               FIRST=ALPHA,OTHER=ALPHANUM,MAXLNTH=8                     07120000
         SPACE 2                                                        07130000
SUB1OPER IKJIDENT 'OPERATOR FOR COMPARISON',FIRST=ALPHA,OTHER=ALPHA,   X07140000
               MAXLNTH=2,                                              X07150000
               PROMPT=' OPERATORS ARE EQ, NE, LT, LE, GT, AND GE'       07160000
         SPACE 2                                                        07170000
SUB1VALU IKJIDENT 'COMPARISON VALUE',FIRST=ALPHANUM,                   X07180000
               OTHER=ALPHANUM,                                         X07190000
               PROMPT='VALUE TO COMPARE FOR DATA SET KEYWORDS'          07200000
         SPACE 2                                                        07210000
SUBAO2   IKJSUBF                                                        07220000
SUB2KEY IKJIDENT 'FIELD IN DATA SET CONTROL BLOCK TO COMPARE',         X07230000
               FIRST=ALPHA,OTHER=ALPHANUM,MAXLNTH=8                     07240000
         SPACE 2                                                        07250000
SUB2OPER IKJIDENT 'OPERATOR FOR COMPARISON',FIRST=ALPHA,OTHER=ALPHA,   X07260000
               MAXLNTH=2,                                              X07270000
               PROMPT=' OPERATORS ARE EQ, NE, LT, LE, GT, AND GE'       07280000
         SPACE 2                                                        07290000
SUB2VALU IKJIDENT 'COMPARISON VALUE',FIRST=ALPHANUM,                   X07300000
               OTHER=ALPHANUM,                                         X07310000
               PROMPT='VALUE TO COMPARE FOR DATA SET KEYWORDS'          07320000
         SPACE 2                                                        07330000
SUBAO3   IKJSUBF                                                        07340000
SUB3KEY IKJIDENT 'FIELD IN DATA SET CONTROL BLOCK TO COMPARE',         X07350000
               FIRST=ALPHA,OTHER=ALPHANUM,MAXLNTH=8                     07360000
         SPACE 2                                                        07370000
SUB3OPER IKJIDENT 'OPERATOR FOR COMPARISON',FIRST=ALPHA,OTHER=ALPHA,   X07380000
               MAXLNTH=2,                                              X07390000
               PROMPT=' OPERATORS ARE EQ, NE, LT, LE, GT, AND GE'       07400000
         SPACE 2                                                        07410000
SUB3VALU IKJIDENT 'COMPARISON VALUE',FIRST=ALPHANUM,                   X07420000
               OTHER=ALPHANUM,                                         X07430000
               PROMPT='VALUE TO COMPARE FOR DATA SET KEYWORDS'          07440000
         SPACE 2                                                        07450000
SUBFORMT IKJSUBF                                                        07460000
FORMATSP IKJIDENT 'FORMAT TYPES TO OUTPUT',LIST,FIRST=NUMERIC,         X07470000
               MAXLNTH=1,DEFAULT='1'                                    07480000
         SPACE 2                                                        07490000
SUBDSNLN IKJSUBF                                                        07500000
DSNPLN   IKJIDENT 'LENGTH OF DSNAME TO PRINT',FIRST=NUMERIC,           X07510000
               OTHER=NUMERIC,MAXLNTH=2,DEFAULT='44'                     07520000
DSNLNTYP IKJKEYWD DEFAULT='TRUNCATE'                                    07530000
         IKJNAME 'TRUNCATE'                                             07540000
         IKJNAME 'MULTILINE'                                            07550000
         IKJENDP                                                        07560000
         SPACE 2                                                        07570000
         POP   PRINT                                                    07580000
ACTION   EQU   1                                                        07590000
VOLUME   EQU   2                                                        07600000
CDATE    EQU   3                                                        07610000
LSTUS    EQU   4                                                        07620000
EXPDT    EQU   5                                                        07630000
ALLOC    EQU   6                                                        07640000
UNUSED   EQU   7                                                        07650000
PCT      EQU   8                                                        07660000
EXT      EQU   9                                                        07670000
DSORG    EQU   10                                                       07680000
RECFM    EQU   11                                                       07690000
BLKSZ    EQU   12                                                       07700000
LRECL    EQU   13                                                       07710000
PASS     EQU   14                                                       07720000
CAT      EQU   15                                                       07730000
SECT     EQU   16                                                       07740000
SECQ     EQU   17                                                       07750000
UNIT     EQU   18                                                       07760000
ROUND    EQU   19                                                       07770000
TYPE     EQU   20                                                       07780000
USED     EQU   21                                                       07790000
CCHH     EQU   22                                                       07800000
DUMMY3   EQU   23                                                       07810000
DUMMY4   EQU   24                                                       07820000
DUMMY5   EQU   25                                                       07830000
DSNAME   EQU   26                                                       07840000
./       ADD   NAME=VTOCPRNT
         TITLE 'VTOC COMMAND PRINT ROUTINE'                             00000100
*********************************************************************** 00000200
*                                                                     * 00000300
*                                                                     * 00000400
* TITLE -      VTOC COMMAND PRINT ROUTINE                             * 00000500
*                                                                     * 00000600
* FUNCTION -   PRINT THE DATA PASSED TO IT.  IT WILL USE THE DDNAME   * 00000700
*              SYSOUT IF IT IS ALLOCATED, AND WILL USE THE VTOCMSG    * 00000800
*              ROUTINE IF NOT.  IT CAN ALSO PUT OUT THE DATA TO       * 00000900
*              AN OUTPUT DATA SET.  TOTALS ARE COMPUTED HERE.         * 00001000
*                                                                     * 00001100
* OPERATION -  FOR UNSORTED DATA, GET THE CURRENT ENTRY, ADD TO THE   * 00001200
*              TOTALS, AND OUTPUT IT.  IF THE DATA IS SORTED, THE     * 00001300
*              ACTION IS MORE COMPLEX, BECAUSE ALL THE DATA SETS      * 00001400
*              ARE TO BE OUTPUT.  THE TOTALS AND BREAKS MAY BE        * 00001500
*              NEEDED AT ANY POINT.                                   * 00001600
*                                                                     * 00001700
* INPUT -      VTOC COMMON AREA ( VTOCOM )                            * 00001800
*              POINTED TO BY REGISTER 1                               * 00001900
*              USE PARSE DATA, CURRENT FORMATTED DSCB, SORTED LIST    * 00002000
*                                                                     * 00002100
* OUTPUT -     TO SYSOUT, A LIST OF THE REQUESTED DATA SETS AND       * 00002200
*              THEIR ATTRIBUTES, WITH TOTALS AND BREAKS AS NEEDED.    * 00002300
*              ALSO TO THE OUTPUT DATA SET, IF NEEDED.  IF SYSOUT     * 00002400
*              IS NOT ALLOCATED, VTOCMSG IS USED FOR OUTPUT.          * 00002500
*                                                                     * 00002600
* ATTRIBUTES - REENTRANT, REUSEABLE, REFRESHABLE.                     * 00002700
*                                                                     * 00002800
*                                                                     * 00002900
*         PROGRAMMED BY R. L. MILLER  (415) 485-6241                  * 00003000
*                                                                     * 00003100
*                                                                     * 00003200
*********************************************************************** 00003300
*                                                                       00003400
*        MACRO FOR DEFINING FAKE PDE FOR A DEFAULT LIST                 00003500
*                                                                       00003600
         EJECT                                                          00003700
VTOCPRNT ENTER 12,20          DO THE HOUSEKEEPING                       00003800
         LR    R11,R1         SAVE ADDR OF VTOCOM                       00003900
         USING VTOCOM,R11     SET ITS ADDRESSABILITY                    00004000
         L     R9,ADDRANSR    POINT TO THE PARSE ANSWER                 00004100
         USING PDL,R9         SET ITS ADDRESSABILITY                    00004200
         USING PRNTWORK,R13   SET ADDRESSABILITY FOR LOCAL WORK AREA    00004300
         SPACE 3                                                        00004400
*                                                                       00004500
*        CHECK FOR THE PRINT CLEAN - CLOSE AND FREEMAIN                 00004600
*                                                                       00004700
         CLI   VTCEPRNT,0     IS IT TIME                                00004800
         BNE   PRNTCLEN       YES, GO DO IT                             00004900
*                                                                       00005000
*        CHECK FOR THE FIRST TIME THROUGH                               00005100
*        IF SO, SET UP THE DCB'S AND OPEN THEM                          00005200
*                                                                       00005300
         CLI   FIRSTIM,0      IS THIS THE FIRST TIME?                   00005400
         BNE   CHKSORT        NO, KEEP ON TRUCKIN'                      00005500
         B     PRTINIT    INITIALIZE FOR PRINTING                       00005600
*                                                                       00005700
*        CHECK TO SEE IF THE DATA IS SORTED                             00005800
*                                                                       00005900
CHKSORT  CLI   SORTK+1,2      IS THIS NOSORT?                           00006000
         BNE   SORTED         NO, THE ENTRIES ARE SORTED                00006100
*                                                                       00006200
*        NOSORT WAS SPECIFIED.  ONLY THE CURRENT ENTRY IS AVAILABLE     00006300
*                                                                       00006400
         L     R3,FORMATAD    POINT TO THE ENTRY                        00006500
         USING VTFMT,R3       FORMATTED DSCB ADDRESSABILITY             00006600
         LTR   R3,R3          IS IT THERE?                              00006700
         BZ    CHEKTOT        NO, SEE IF A TOTAL HAS BEEN OUTPUT        00006800
         XC    FORMATAD,FORMATAD  CLEAR THE ADDRESS FOR LATER           00006900
*                                                                       00007000
*        ADD TO THE TOTALS                                              00007100
*                                                                       00007200
         BAL   R8,ADDTOT      GO DO IT                                  00007300
*                                                                       00007400
*        SEE IF THE OUTPUT DATA SET IS WANTED                           00007500
*                                                                       00007600
         TM    OUTDCB+48,X'10'  IS THE DCB OPEN?                        00007700
         BNO   NOOUTPT        NO, SKIP ALONG                            00007800
         BAL   R8,OUTPUT      YES, GO DO IT                             00007900
*                                                                       00008000
*        SEE IF THERE'S PRINTING TO DO                                  00008100
*                                                                       00008200
NOOUTPT  CLI   PRINTK+1,2     WAS NOPRINT SPECIFIED?                    00008300
         BE    VTRET          YES, WE'RE DONE FOR NOW                   00008400
         BAL   R8,PRINT       NO, PRINT OUT THE ENTRY                   00008500
*                                                                       00008600
*        RETURN FROM WHENCE WE CAME                                     00008700
*                                                                       00008800
VTRET    LEAVE EQ,RC=0        EXEUNT                                    00008900
*                                                                       00009000
*        NOSORT, CHECK FOR OUTPUTTING THE TOTALS                        00009100
*                                                                       00009200
CHEKTOT  TM    ENDTOTAL,ENTOTOUT  WAS THE TOTAL OUTPUT BEFORE?          00009300
         BO    VTRET          YES, JUST RETURN                          00009400
         BAL   R8,PRNTOT      NO, OUTPUT THE TOTAL                      00009500
         OI    ENDTOTAL,ENTOTOUT  REMEMBER THE TOTAL IS OUT             00009600
         B     VTRET          THEN RETURN                               00009700
         EJECT                                                          00009800
*                                                                       00009900
*        THE DATA IS SORTED, SO THE LISTS MUST BE EMPTIED.              00010000
*        CHECK EACH ITEM FOR A TOTAL AND FOR A BREAK                    00010100
*        OUTPUT TO PRINT AND/OR THE OUTPUT DATA SET                     00010200
*                                                                       00010300
*                                                                       00010400
*        FIRST GET THE TOTAL AND BREAK COUNTS FOR COMPARES              00010500
*                                                                       00010600
SORTED   LA    R1,TOTALN      POINT TO THE TOTAL PDL                    00010700
         BAL   R8,PDLNUM      CONVERT IT TO A NUMBER                    00010800
         LTR   R15,R15        IS IT ZERO?                               00010900
         BNZ   SETTNUM        NO, IT'S GOOD                             00011000
         OI    ENDTOTAL,ENDTONLY  YES, TOTALS AT THE END ONLY           00011100
SETTNUM  BCTR  R15,0          CUT IT DOWN ONE                           00011200
         STH   R15,NUMTOTAL   SET THE TOTAL COUNT                       00011300
         LA    R1,BREAK       POINT TO THE BREAK PDL                    00011400
         BAL   R8,PDLNUM      CONVERT IT TO A NUMBER                    00011500
         LTR   R15,R15        IS IT ZERO?                               00011600
         BNZ   SETBNUM        NO, IT'S GOOD                             00011700
         OI    ENDTOTAL,NOBREAK   YES, BREAK  AT THE END ONLY           00011800
SETBNUM  BCTR  R15,0          CUT IT DOWN ONE                           00011900
         STH   R15,NUMBREAK   SET THE BREAK COUNT                       00012000
*                                                                       00012100
*        START GOING THROUGH THE LISTS, PROCESS THE ENTRIES             00012200
*                                                                       00012300
         LA    R4,VTCSORTH    POINT TO THE LISTS                        00012400
         LA    R5,VTCSORTE    POINT TO THE END OF THE LISTS             00012500
         ST    R5,ADDREND     SAVE THE ADDRESS                          00012600
NEWLIST  L     R3,0(R4)       GET THE FIRST ENTRY FROM THIS LIST        00012700
         LTR   R3,R3          ANYTHING ON THIS LIST?                    00012800
         BZ    NEXTLIST       NO, GET ANOTHER LIST                      00012900
*                                                                       00013000
*        THIS IS AN ENTRY, DO THE TOTALS, PRINT, AND OUTPUT             00013100
*        CHECK FOR TOTALS AND BREAKS FIRST                              00013200
*                                                                       00013300
GOTENTRY CLI   PRINTK+1,2     WAS NOPRINT SET?                          00013400
         BE    CHKOUTPT       YES, SKIP INTERIM TOTALS                  00013500
         MVI   TOTLAST,0      SET UP FLAG FOR TOTAL AS LAST ACTION      00013600
*        NOTE - EXTENSION - OUTPUT INTERIM TOTALS WITH THE KEY          00013700
         TM    ENDTOTAL,ENDTONLY  END TOTAL ONLY?                       00013800
         BO    CHKBREAK       YES, SEE ABOUT BREAKS                     00013900
         LH    R2,NUMTOTAL    GET THE LENGTH TO COMPARE                 00014000
         L     R1,LASTKEY     GET THE LAST ENTRY                        00014100
         EX    R2,COMPKEY     SEE IF THIS IS THE SAME                   00014200
         BE    CHKBREAK       YES, KEEP COUNTING                        00014300
*                                                                       00014400
*        THIS ONE IS DIFFERENT, PRINT THE TOTALS FIRST                  00014500
*                                                                       00014600
         BAL   R8,PRNTOT      PRINT THE TOTALS                          00014700
*                                                                       00014800
*        CHECK FOR A BREAK                                              00014900
*                                                                       00015000
CHKBREAK TM    ENDTOTAL,NOBREAK   NO BREAKS THIS TIME?                  00015100
         BO    SKPBREAK       YES, SKIP PAST BREAKS                     00015200
         LH    R2,NUMBREAK    GET THE LENGTH TO COMPARE                 00015300
         L     R1,LASTKEY     GET THE LAST ENTRY                        00015400
         EX    R2,COMPKEY     SEE IF THIS IS THE SAME                   00015500
         BE    SKPBREAK       YES, KEEP COUNTING                        00015600
*                                                                       00015700
*        THIS ONE IS DIFFERENT, GET A NEW PAGE                          00015800
*                                                                       00015900
         MVC   LINECT,LINEMAX BE SURE THE NEXT ITEM GETS A NEW PAGE     00016000
         MVI   TOTLAST,0      DON'T SKIP A LINE AFTER TOTAL             00016100
         LA    R1,VTFDSN     POINT TO THE DSNAME                        00016200
         ST    R1,LASTKEY    SAVE THE ADDRESS FOR BREAK COMPARES        00016300
*                                                                       00016400
*        PRINT THE ITEM                                                 00016500
*                                                                       00016600
SKPBREAK CLI   TOTLAST,0      WAS A TOTAL NOT FOLLOWED BY A BREAK?      00016700
         BE    SKPBREA2       NO, CONTINUE NORMALLY                     00016800
         MVC   MSGBL,MSGBLC   YES, SET UP A BLANK MESSAGE               00016900
         LA    R1,MSGBL       POINT TO IT                               00017000
         BAL   R8,PRNTLINE    THEN PUTPUT IT                            00017100
SKPBREA2 BAL   R8,PRINT       FINAL FORMAT AND PRINT                    00017200
*                                                                       00017300
*        CHECK FOR DATA SET OUTPUT                                      00017400
*                                                                       00017500
CHKOUTPT TM    OUTDCB+48,X'10'     IS IT OPEN AND READY                 00017600
         BNO   GOTOT          NO, GO DO THE TOTALS                      00017700
         BAL   R8,OUTPUT      YES, PUT OUT THE DATA SET ENTRY           00017800
*                                                                       00017900
*        ADD UP THE TOTALS                                              00018000
*                                                                       00018100
GOTOT    BAL   R8,ADDTOT      SUM THEM                                  00018200
*                                                                       00018300
*        GET THE NEXT ENTRY                                             00018400
*                                                                       00018500
         ICM   R3,B'1111',VTFNEXT  FOLLOW THE CHAIN                     00018600
         BNZ   GOTENTRY       SOMETHING'S THERE, USE IT                 00018700
*                                                                       00018800
*        END OF THIS LIST, TRY THE NEXT LIST                            00018900
*                                                                       00019000
NEXTLIST LA    R4,12(0,R4)    MOVE OVER ONE                             00019100
         C     R4,ADDREND     WAS THAT THE LAST LIST?                   00019200
         BL    NEWLIST        NO, KEEP TRYING                           00019300
*                                                                       00019400
*        END OF THE LISTS, OUTPUT THE FINAL TOTAL AND RETURN            00019500
*                                                                       00019600
         BAL   R8,PRNTOT      LIST THE TOTAL                            00019700
*                                                                       00019800
*        CLEAR OUT THE PRINT LISTS                                      00019900
*                                                                       00020000
         XC    VTCSORTH+000(256),VTCSORTH  CLEAR 64 ENTRIES             00020100
         XC    VTCSORTH+256(256),VTCSORTH+256 CLEAR 64 ENTRIES          00020200
         XC    VTCSORTH+512(256),VTCSORTH+512 CLEAR 64 ENTRIES          00020300
         XC    VTCSORTH+768(256),VTCSORTH+768 CLEAR 64 ENTRIES          00020400
         B     VTRET          THEN GET OUT OF HERE                      00020500
         EJECT                                                          00020600
*                                                                       00020700
*        PRINT CLEANUP ROUTINE - CLOSE DCB'S FIRST                      00020800
*                                                                       00020900
PRNTCLEN TM    SYSOUT+48,X'10'  IS SYSOUT OPEN?                         00021000
         BNO   PRNTCLO        NO, CHECK THE OUTDCB                      00021100
         CLOSE (SYSOUT),MF=(E,OPENLIST)  DO THE CLOSE                   00021200
PRNTCLO  TM    OUTDCB+48,X'10'  IS OUTDCB OPEN?                         00021300
         BNO   PRNTFREE       NO, SKIP DOWN TO THE FREEMAINS            00021400
         CLOSE (OUTDCB),MF=(E,OPENLIST)  DO THE CLOSE                   00021500
*                                                                       00021600
*        FREE UP THE STORAGE                                            00021700
*                                                                       00021800
PRNTFREE LA    R2,VTCGETMN    POINT TO THE TABLE                        00021900
         LA    R5,VTCGETMX    GET THE NUMBER OF ENTRIES IN THE TABLE    00022000
PRNTFRL  ICM   R3,B'1111',0(R2)  GET THE STORAGE ADDRESS                00022100
         BZ    VTRET          IF ZERO, WE'RE DONE                       00022200
         FREEMAIN R,LV=VTCGETMS,A=(R3)  FREE IT                         00022300
         XC    0(4,R2),0(R2)  CLEAR THE ADDRESS                         00022400
         LA    R2,4(R2)       GET THE NEXT BLOCK ADDRESS                00022500
         BCT   R5,PRNTFRL     AND LOOP UNTIL DONE                       00022600
         B     VTRET          THEN RETURN                               00022700
*                                                                       00022800
         EJECT                                                          00022900
*                                                                       00023000
*        ROUTINES USED ABOVE                                            00023100
*              ADDTOT - ADD TO THE CURRENT TOTALS                       00023200
*              PRNTOT - PRINT OUT THE TOTALS AND CLEAR THEM             00023300
*              PRINT  - PRINT OUT AN ENTRY                              00023400
*              OUTPUT - OUTPUT THE DATA SET ENTRY                       00023500
*              PDLNUM - GET A NUMBER FROM A PDL ENTRY                   00023600
*              PRNTLINE - INTERNAL ROUTINE TO COUNT LINES, OUTPUT HEAD  00023700
*                                                                       00023800
ADDTOT   L     R1,TOTDS       NUMBER OF DATA SETS                       00023900
         LA    R1,1(R1)       ADD ONE                                   00024000
         ST    R1,TOTDS       STORE IT BACK                             00024100
         L     R1,TOTALLOC    TOTAL ALLOCATION                          00024200
         A     R1,VTFALLOC    ADD IN THIS DATA SET                      00024300
         ST    R1,TOTALLOC    STORE IT BACK                             00024400
         L     R1,TOTUSED     TOTAL USED SPACE                          00024500
         A     R1,VTFUSED     ADD IN THIS DATA SET                      00024600
         ST    R1,TOTUSED     STORE IT BACK                             00024700
         BR    R8             RETURN                                    00024800
*                                                                       00024900
         EJECT                                                          00025000
*                                                                       00025100
*        PRINT THE TOTALS                                               00025200
*              FIRST FORMAT THEM, THEN PUT THE LINE OUT TO SYSOUT       00025300
*              OR USE VTOCMSG TO LIST IT                                00025400
*                                                                       00025500
PRNTOT   ST    R8,PRNTTOT8   SAVE THE RETURN ADDRESS                    00025600
         MVC   MSGWORK(MSGTLEN),MSGTOTC  INIT THE MSG                   00025700
         CONV  MSGWORK+4+11,TOTDS,5  CONVERT NO OF DATA SETS            00025800
         CONV  MSGWORK+4+28,TOTALLOC,8  CONVERT ALLOCATION              00025900
         CONV  MSGWORK+4+51,TOTUSED,8   CONVERT USED SPACE              00026000
         MVC   MSGWORK+4+37(6),SPACTYPE MOVE IN THE UNITS               00026100
         MVC   MSGWORK+4+60(6),SPACTYPE MOVE IN THE UNITS               00026200
*                                                                       00026300
*        NOW OUTPUT THE MESSAGE                                         00026400
*                                                                       00026500
         MVI   MSGWORK+4,C'0' ADD A CARRIAGE CONTROL                    00026600
         LA    R1,MSGWORK     POINT TO THE TOTAL LINE                   00026700
         BAL   R8,PRNTLINE    PUT OUT THE TOTAL LINE                    00026800
         MVI   TOTLAST,1      NOTE THAT A TOTAL WAS THE LAST ITEM       00026900
         L     R8,PRNTTOT8    GET THE RETURN ADDRESS                    00027000
         BR    R8             THEN RETURN                               00027100
         EJECT                                                          00027200
*                                                                       00027300
*        PRINT OUT THE FORMATTED DSCB                                   00027400
*              FIRST FORMAT IT                                          00027500
*                                                                       00027600
PRINT    ST    R8,PRINTR8     SAVE REGISTER 8 FOR RETURNING             00027700
         MVI   WORKLINE+4,C' ' BLANK OUT THE LINE                       00027800
         MVC   WORKLINE+5(250),WORKLINE+4 SO INDIVIUAL FIELDS DON'T     00027900
         L     R2,VTPRNTLS   GET THE PRINT ITEM LIST                    00028000
         LA    R1,WORKLINE+4  POINT TO THE WORK  LINE                   00028100
PRTLOOP  SR    R6,R6          GET THE RESERVED WORD NUMBER              00028200
         IC    R6,0(R2)       FROM THE TOP BYTE                         00028300
         MH    R6,H12         MULTIPLY BY 12 FOR THE TABLE ENTRIES      00028400
         LA    R6,TABTITL(R6) THEN RELOCATE THE MESS                    00028500
         SR    R7,R7          CLEAR A REGISTER                          00028600
         IC    R7,0(R6)       GET THE EXECUTE LENGTH                    00028700
         LA    R5,1(R1)       SAVE A PLACE TO MOVE FIELD INTO           00028800
         LA    R1,1(R1,R7)    MOVE THE POINTER OVER                     00028900
         LA    R0,WORKLINE+4  POINT TO THE BEGINNING AGAIN              00029000
         SR    R1,R0          AND FIND THE CURRENT LENGTH               00029100
         CH    R1,LINELEN     IS IT TOO LONG?                           00029200
         BNL   PRTEND         YES, WE'RE DONE                           00029300
         AR    R1,R0          NO, KEEP GOING                            00029400
*                                                                       00029500
*        MOVE IN OR CONVERT THIS ITEM                                   00029600
*                                                                       00029700
         SR    R14,R14       CLEAR A REG FOR LENGTH OF RTN NO           00029800
         IC    R14,1(R6)     GET THE LENGTH OR ROUTINE                  00029900
         N     R14,F127      CLEAR THE TOP BIT                          00030000
         SR    R15,R15       CLEAR A REG FOR VTFMT                      00030100
         IC    R15,2(R6)       DISPLACEMENT                             00030200
         AR    R15,R3        RELOCATE IT                                00030300
*                                                                       00030400
*        DECIDE WHERE TO PUT IT                                         00030500
*                                                                       00030600
         TM    1(R6),X'80'   IS IT IN CHARS                             00030700
         BNO   PRTRTN        NO, USE THE SPECIAL ROUTINE                00030800
         SR    R7,R14        GET THE DIFFERENCE IN LENGTHS              00030900
         SRL   R7,1          GET HALF THE DIFFERENCE                    00031000
         AR    R5,R7         PUT THE DATA HERE                          00031100
         EX    R14,PRTMOVE   MOVE IN THE CHARACTERS                     00031200
PRTINC   LA    R2,1(R2)            GET THE NEXT CHAIN POINTER           00031300
         CLI   0(R2),0       ARE WE DONE?                               00031400
         BNE   PRTLOOP        GO GET MORE ITEMS                         00031500
PRTEND   LA    R1,WORKLINE   POINT TO THIS LINE                         00031600
         BAL   R8,PRNTLINE   THEN GO PRINT IT                           00031700
         L     R8,PRINTR8     GET THE RETURN ADDRESS                    00031800
         BR    R8             RETURN                                    00031900
PRTMOVE  MVC   0(0,R5),0(R15)  EXECUTED MOVE                            00032000
*                                                                       00032100
*        VARIOUS ROUTINES TO FORMAT BEFORE MOVING IN THE DATA           00032200
*                                                                       00032300
PRTRTN   LTR   R14,R14       BE SURE THE OFFSET IS OK                   00032400
         BP    PRTRTN1        IT SEEMS ALL RIGHT                        00032500
         ABEND 701,DUMP       CRASH AND BURN                            00032600
PRTRTN1  B     *(R14)        GO DO YOUR THING                           00032700
         B     PRDATES        4  CDATE, EXPDT                           00032800
         B     PRLSTUS        8  LSTUS                                  00032900
         B     PRALLOC       12  ALLOC, USED                            00033000
         B     PRUNUSED      16  ALLOC - USED                           00033100
         B     PRPCT         20  100 * USED / ALLOC                     00033200
         B     PREXT         24  EXT                                    00033300
         B     PRBLREC       28  BLKSZ LRECL                            00033400
         B     PRDSN         32  DSNAME                                 00033500
*                                                                       00033600
*                                                                       00033700
*        CDATE AND EXPDT                                                00033800
*                                                                       00033900
PRDATES  SR    R14,R14       CLEAR REG FOR YEAR                         00034000
         IC    R14,0(R15)    GET THE YEAR                               00034100
         USING DUMMD,R5      ALLOW CONV TO USE SYMBOL                   00034200
         CONV  DUMMA,(R14),2 CONVERT YEAR                               00034300
         ICM   R14,B'0011',1(R15)  GET THE DAY                          00034400
         CONV  DUMMA+2,(R14),3,EDMASK0,COMP0  CONVERT THE DAY           00034500
         B     PRTINC        GO GET MORE PRINT ITEMS                    00034600
*                                                                       00034700
*        LAST USE DATE                                                  00034800
*                                                                       00034900
PRLSTUS  MVC   CHARS,EDMASK   SET UP THE EDIT MASK                      00035000
         ED    CHARS(6),0(R15)  CONVERT TO CHARACTERS                   00035100
         MVC   0(5,R5),CHARS+1  THEN MOVE THEM IN                       00035200
         B     PRTINC        GO GET MORE PRINT ITEMS                    00035300
*                                                                       00035400
*        ALLOCATION AND USED                                            00035500
*                                                                       00035600
PRALLOC  ICM   R14,B'1111',0(R15)  GET THE AMOUNT                       00035700
         BM    PRUNKN6       IF NEGATIVE, IT'S NOT KNOWN                00035800
         CONV  DUMMA,(R14),6 CONVERT THE NUMBER                         00035900
         B     PRTINC        GO GET MORE PRINT ITEMS                    00036000
PRUNKN6  MVC   0(6,R5),BLANKS  UNKNOWN AMOUNT, LEAVE IT BLANK           00036100
         B     PRTINC        GO GET MORE PRINT ITEMS                    00036200
*                                                                       00036300
*        UNUSED SPACE                                                   00036400
*                                                                       00036500
PRUNUSED ICM   R14,B'1111',VTFUSED  GET THE   USED SPACE                00036600
         BM    PRUNKN6       IF NOT KNOWN, SKIP ALONG                   00036700
         L     R14,VTFALLOC  GET THE ALLOCATED SPACE                    00036800
         S     R14,VTFUSED   MINUS THE USED SPACE GIVES UNUSED          00036900
         BZ    PRTINC        IF NO UNUSED SPACE, LEAVE BLANK            00037000
         CONV  DUMMA,(R14),6 CONVERT FOR PRINTING                       00037100
         B     PRTINC        GO GET MORE PRINT ITEMS                    00037200
*                                                                       00037300
*        PCT - PERCENTAGE USED                                          00037400
*                                                                       00037500
PRPCT    SR    R6,R6         CLEAR THE TOP PART OF THE NUMBER           00037600
         ICM   R7,B'1111',VTFUSED   GET THE AMOUNT OF USED SPACE        00037700
         BM    PRUNKN3       IF NOT KNOWN, LEAVE BLANK                  00037800
         CLC   VTFALLOC,ZERO IS THE ALLOCATION ZERO?                    00037900
         BNE   PRPCTM        NO, DO THE STANDARD STUFF                  00038000
         CLC   VTFUSED,ZERO  IS THE USED SPACE ZERO?                    00038100
         BE    PRUNKN3       YES, JUST USE BLANKS                       00038200
PRPCTERR MVC   0(3,R5),=C'ERR'  NO, IT'S AN ERROR                       00038300
         B     PRTINC        THEN GO TRY FOR MORE                       00038400
PRPCTM   M     R6,F100       MULTIPLY BY 100 FOR PERCENT                00038500
         D     R6,VTFALLOC   DIVIDE BY ALLOC TO GET THE PERCENT         00038600
         LTR   R7,R7         IS IT LESS THAN ZERO?                      00038700
         BM    PRPCTERR      YES, FLAG THE ERROR                        00038800
         C     R7,F100       ALSO CHECK FOR OVER 100 PERCENT            00038900
         BH    PRPCTERR      THAT'S ALSO AN ERROR                       00039000
         CONV  DUMMA,(R7),3  CONVERT FOR PRINTING                       00039100
         B     PRTINC        GO GET MORE PRINT ITEMS                    00039200
PRUNKN3  MVC   0(3,R5),BLANKS BLANK THE UNKNOWN                         00039300
         B     PRTINC        GO GET MORE PRINT ITEMS                    00039400
*                                                                       00039500
*        EXTENTS                                                        00039600
*                                                                       00039700
PREXT    SR    R14,R14       CLEAR REG FOR YEAR                         00039800
         IC    R14,0(R15)    GET THE YEAR                               00039900
         CONV  DUMMA,(R14),2 CONVERT THE EXTENTS                        00040000
         B     PRTINC        GO GET MORE PRINT ITEMS                    00040100
*                                                                       00040200
*        LRECL AND BLKSZ                                                00040300
*                                                                       00040400
PRBLREC  LH    R14,0(R15)    GET THE DATA ( HALFWORD )                  00040500
         CONV  DUMMA,(R14),5  CONVERT THE DATA                          00040600
         B     PRTINC        GO GET MORE PRINT ITEMS                    00040700
*                                                                       00040800
*        DATA SET NAME                                                  00040900
*                                                                       00041000
PRDSN    LH    R14,VTFDSNL   GET THE DSNAME LENGTH                      00041100
         CH    R14,DSNLENGT  CHACK FOR MAX LENGTH                       00041200
         BL    PRDMOVE       THIS DSN IS SHORT ENOUGH                   00041300
         LH    R14,DSNLENGT  CUT IT DOWN                                00041400
PRDMOVE  BCTR  R14,0         COUNT DOWN ONE FOR THE EX                  00041500
         EX    R14,PRTMOVE   MOVE IN THE DSNAME                         00041600
         AH    R1,DSNLENOF   CORRECT THE LINE POINTER                   00041700
         B     PRTINC        GO GET MORE PRINT ITEMS                    00041800
         EJECT                                                          00041900
*                                                                       00042000
*        OUTPUT THE FORMATTED DSCB AS IT IS                             00042100
*                                                                       00042200
OUTPUT   CLEAR OUTWORK       CLEAR THE OUTPUT REC                       00042300
         LH    R2,VTFDSNL     GET THE LENGTH OF THE DSNAME              00042400
         LA    R2,VTFMTL-1(R2)  GET THE FULL LENGTH MINUS ONE           00042500
         EX    R2,OUTMOVE    MOVE IN THE ACTUAL RECORD                  00042600
         PUT   OUTDCB,OUTWORK    OUTPUT THE RECORD                      00042700
         BR    R8             RETURN                                    00042800
OUTMOVE  MVC   OUTWORK(0),0(R3)  EXECUTED MOVE                          00042900
         EJECT                                                          00043000
*                                                                       00043100
*        PDLNUM - CONVERT FROM CHARACTERS ( EBCDIC ) TO AN INTEGER      00043200
*              BINARY FORM, PASSED BACK VIA REGISTER 15                 00043300
*              A PARSE PDE IS THE INPUT AS SHOWN IN THE SAMPLE BELOW    00043400
*                       LA    R1,PDL     POINT TO THE PARSE DECRIPTION  00043500
*                       BAL   R8,PDLNUM  GO CONVERT TO NUMERICS         00043600
*              THE ROUTINE WILL TERMINATE IF IT FINDS NON-NUMERICS      00043700
*                 ANY CHARACTERS OTHER THEN 0-9, +, -                   00043800
*              REGISTERS 1, 2, 5, 6, AND 7 ARE USED                     00043900
*                                                                       00044000
PDLNUM   STM   R1,R8,PDLNSAVE SAVE THE REGISTERS                        00044100
         LH    R2,4(R1)       GET THE STRING ADDRESS                    00044200
         L     R1,0(R1)       GET THE STRING ADDRESS                    00044300
         MVI   PDLMINUS,0     CLEAR THE NEGATIVE NUMBER FLAG            00044400
         SR    R5,R5          CLEAR THE CHARACTER COUNTER               00044500
         SR    R15,R15        CLEAR THE ANSWER                          00044600
PDLLOOP  LA    R6,0(R5,R1)    POINT TO THIS DIGIT                       00044700
         LA    R5,1(R5)       GET TO THE NEXT DIGIT                     00044800
         CR    R5,R2          IS THIS THE END OF THE STRING?            00044900
         BH    PDLFINI        YES, EXIT                                 00045000
         SR    R7,R7          CLEAR A WORK REGISTER                     00045100
         IC    R7,0(R6)       GET THE CHARACTER                         00045200
         SH    R7,PDLH240     SUBTRACT THE CHARACTER C'0'               00045300
         BM    PDLSP          IF NEGATIVE, CHECK SPECIAL CHARACTERS     00045400
         MH    R15,PDLH10     IT'S A DIGIT, MULTIPLY PRIOR NUM BY TEN   00045500
         AR    R15,R7         ADD ON THE NEW DIGIT                      00045600
         B     PDLLOOP        AND LOOP FOR MORE                         00045700
*                                                                       00045800
*        CHECK FOR SPECIAL CHARACTERS                                   00045900
*                                                                       00046000
PDLSP    CLI   0(R6),C' '     IS IT A BLANK?                            00046100
         BE    PDLLOOP        THEN IT'S OK                              00046200
         CLI   0(R6),C'+'     IS IT A PLUS?                             00046300
         BE    PDLLOOP        THAT'S ALSO OK                            00046400
         CLI   0(R6),C'-'     IS IT A MINUS?                            00046500
         BNE   PDLFINI        NO, JUST QUIT                             00046600
         MVI   PDLMINUS,1     YES, NOTE IT                              00046700
         B     PDLLOOP        AND LOOK FOR MORE                         00046800
*                                                                       00046900
*        QUIT, AFTER SETTING R15 TO NEGATIVE IF NEEDED                  00047000
*                                                                       00047100
PDLFINI  CLI   PDLMINUS,1     WAS A MINUS SIGN FOUND?                   00047200
         BNE   PDLLEAVE       NO, EXIT                                  00047300
         LNR   R15,R15        YES, MAKE IT NEGATIVE                     00047400
PDLLEAVE LM    R1,R8,PDLNSAVE RESTORE THE REGISTERS                     00047500
         BR    R8             RETURN                                    00047600
PDLH10   DC    H'10'                                                    00047700
PDLH240  DC    H'240'                                                   00047800
         EJECT                                                          00047900
*                                                                       00048000
*        PRNTLINE - GET EACH LINE FOR SYSOUT, THEN COUNT THE LINES      00048100
*              OUTPUT HEADERS AND TITLE LINES AS NECESSARY              00048200
*                                                                       00048300
PRNTLINE STM   R1,R8,PRTLSAVE SAVE THE REGISTERS                        00048400
         TM    SYSOUT+48,X'10' IS SYSOUT OPEN FOR BUSINESS?             00048500
         BNO   PRNTTERM       NO, IT'S NOT OPEN, USE VTOCMSG            00048600
         CLI   HEADK+1,2     NOHEADING REQUEST?                         00048700
         BE    PRNTLIN       YES, JUST OUTPUT THE DATA LINE             00048800
*                                                                       00048900
*        DO THE LINE COUNTING                                           00049000
*                                                                       00049100
         LH    R2,LINECT      GET THE LINE COUNT                        00049200
         CLI   4(R1),C'1'     IS IT REQUESTING A NEW PAGE?              00049300
         BE    PRNTPAGE       YES, DO IT                                00049400
         CLI   0(R1),C'-'     SKIP 3 LINES                              00049500
         BE    PRNTSKP3       YES, TRY IT                               00049600
         CLI   0(R1),C'0'     SKIP 2 LINES?                             00049700
         BE    PRNTSKP2       YES, DO IT                                00049800
         B     PRNTSKP1       JUST SKIP ONE                             00049900
PRNTSKP3 LA    R2,1(R2)       ADD ONE TO THE LINE COUNT                 00050000
PRNTSKP2 LA    R2,1(R2)       ADD ONE TO THE LINE COUNT                 00050100
PRNTSKP1 LA    R2,1(R2)       ADD ONE TO THE LINE COUNT                 00050200
         STH   R2,LINECT      SAVE THE LINE COUNT                       00050300
         CH    R2,LINEMAX     DOES THIS OVERFLOW  HE PAGE?              00050400
         BL    PRNTLIN        NO, JUST PUT OUT THIS LINE                00050500
*                                                                       00050600
*        PRINT OUT THE PAGE HEADER AND ITEM TITLES                      00050700
*                                                                       00050800
PRNTPAGE LH    R6,PAGECT      GET THE PAGE COUNT                        00050900
         LA    R6,1(R6)       ADD ONE TO IT                             00051000
         STH   R6,PAGECT      THEN STORE IT BACK                        00051100
         L     R7,PAGEADDR    GET THE PLACE TO PUT THE PAGE             00051200
         USING DUMMD,R7       DUMMY DSECT                               00051300
         CONV  DUMMA,(R6),5   GET THE CHARACTERS                        00051400
         DROP  R7                                                       00051500
         ST    R1,PRNTLSAV    SAVE THE INPUT REG1                       00051600
         PUT   SYSOUT,PRNTHEAD  OUTPUT THE HEADER                       00051700
         PUT   SYSOUT,PRNTTITL  OUTPUT THE ITEM TITLES                  00051800
         L     R1,PRNTLSAV    GET THE ORIGINAL LINE                     00051900
         MVI   4(R1),C'0'     ALWAYS SKIP THE FIRST LINE AFTER NEW PAGE 00052000
         LA    R7,5           SET THE LINE COUNT                        00052100
         STH   R7,LINECT      SAVE IT FOR LATER                         00052200
PRNTLIN  CLEAR OUTWORK       CLEAR A PRINT RECORD                       00052300
         LH    R6,0(R1)      GET THE RECORD LENGTH                      00052400
         SH    R6,H5         MINUS 4 FOR PREFIX, 1 FOR EX               00052500
         LA    R3,4(R1)      POINT TO THE TEXT                          00052600
         EX    R6,OUTMOVE    THEN MOVE IN THE LINE                      00052700
         PUT   SYSOUT,OUTWORK      AND FINALLY PRINT IT                 00052800
         B     PRNTLRET       THEN RETURN                               00052900
*                                                                       00053000
*        SIMPLER CHECKING FOR VTOCMSG OUTPUT                            00053100
*                                                                       00053200
PRNTTERM LH    R2,LINECT      GET THE LINE COUNT                        00053300
         LA    R2,1(R2)       IGNORE CARRIAGE CONTROL                   00053400
         STH   R2,LINECT      SAVE IT BACK                              00053500
         LR    R6,R1          SAVE THE ORIGINAL REG 1                   00053600
         MVI   4(R1),C' '    BLANK THE CARRIAGE CONTROL                 00053700
         CLI   HEADK+1,2     NOHEADING REQUEST?                         00053800
         BE    PRNTTLIN       YES, JUST OUTPUT THE DATA LINE            00053900
         CH    R2,LINEMAX     NEED A NEW TITLE?                         00054000
         BL    PRNTTLIN       NO, KEEP GOING                            00054100
         VTOCMSG PRNTTITH     YES, PUT IT OUT                           00054200
         MVC   LINECT,H2      RESET THE LINE COUNT                      00054300
PRNTTLIN VTOCMSG 0(R6)        OUTPUT THE PASSED LINE                    00054400
PRNTLRET LM    R1,R8,PRTLSAVE RESTORE THE REGISTERS                     00054500
         BR    R8             THEN RETURN                               00054600
         EJECT                                                          00054700
PRTINIT  MVI   FIRSTIM,10    FLAG THE INITIALIZATION AS DONE            00054800
         LA    R1,TABTITL    POINT TO THE BUG TABLE                     00054900
         ST    R1,ATABTITL   LET CHECK KNOW WHERE IT IS                 00055000
*                                                                       00055100
*        SET UP THE CHARACTERS FOR TYPE OF SPACE ALLOC.                 00055200
*                                                                       00055300
         LH    R2,SPACEK      GET THE SPACE TYPE KEYWORD                00055400
         SLA   R2,3           MULTIPLY BY 8                             00055500
         LA    R2,TABSPACE(R2)  RELOCATE IT                             00055600
         MVC   SPACTYPE,0(R2) SAVE THE CHARACTERS                       00055700
         CLI   PRINTK+1,2     IS THIS NOPRINT?                          00055800
         BE    NOOPEN         YES, SKIP THE OPEN                        00055900
         MVC   SYSOUT(SYSOUTL),SYSOUTC  INITIALIZE THE DCB              00056000
         MVI   OPENLIST,X'80' TERMINATE THE LIST                        00056100
         LA    R1,JFCB       POINT TO THE JFCB                          00056200
         ST    R1,DCBEXIT    AND PUT THE ADDR IN THE DCB EXIT           00056300
         MVI   DCBEXIT,X'87' NOTE IT AS A JFCB EXIT                     00056400
         LA    R1,DCBEXIT    POINT TO THE EXIT LIST                     00056500
         LA    R2,SYSOUT     AND TO THE DCB FOR ADDRESSABILITY          00056600
         USING IHADCB,R2     TELL THE ASSEMBLER ABOUT IT                00056700
         STCM  R1,B'0111',DCBEXLSA  STUFF IT INTO THE DCB               00056800
         TM    CHARSPL+6,X'80'  CHARS PER LINE ENTERED?                 00056900
         BZ    RDJFCB        NO, CONTINUE ALONG                         00057000
         LA    R1,CHARSPL    YES, POINT TO THE PDL                      00057100
         BAL   R8,PDLNUM     CONVERT TO A NUMBER                        00057200
         STH   R15,DCBLRECL  SAVE THE NEW LRECL                         00057300
         STH   R15,LINELEN   ALSO THE LINE LENGTH                       00057400
         TM    BLKSZSET+6,X'80'  BLOCKSIZE ENTERED?                     00057500
         BZ    BLKEQREC      NO, BLOCKSIZE EQUALS LRECL                 00057600
         LA    R1,BLKSZSET      POINT TO THE PDL                        00057700
         BAL   R8,PDLNUM     GET THE NUMBER                             00057800
BLKEQREC STH   R15,DCBPRECL  STUFF IT AWAY                              00057900
         DROP  R2            FINISHED WITH THE DCB                      00058000
RDJFCB   RDJFCB ((R2)),MF=(E,OPENLIST)  SEE IF IT'S THERE               00058100
         LTR   R15,R15       WAS IT THERE?                              00058200
         BNZ   NOOPEN        NO, SKIP ALONG                             00058300
         OPEN  ((R2),OUTPUT),MF=(E,OPENLIST)  OPEN THE PRINT DCB        00058400
*                                                                       00058500
*        INITIALIZE PRINT VARIABLES                                     00058600
*                                                                       00058700
*        SET LINES/PAGE AND LINESIZE                                    00058800
*                                                                       00058900
NOOPEN   MVC   LINEMAX,DEFLMAX  SET THE DEFAULT NUMBER OF LINES/PAGE    00059000
         TM    LINESPP+6,X'80'     LINES PER PAGE ENTERED?              00059100
         BZ    DEFLINPP     NO, SKIP ON                                 00059200
         LA    R1,LINESPP    YES, POINT TO THE PDL                      00059300
         BAL   R8,PDLNUM     CONVERT TO A NUMBER                        00059400
         STH   R15,LINEMAX   AND SAVE IT                                00059500
DEFLINPP MVC   LINECT,LINEMAX SET UP TO PAGE ON THE FIRST WRITE         00059600
         TM    CHARSPL+6,X'80'     CHARS PER LINE ENTERED?              00059700
         BO    LENSET        YES, USE IT                                00059800
         MVC   LINELEN,DEFLEN SET UP A DEFAULT LENGTH                   00059900
         TM    SYSOUT+48,X'10' DO WE USE SYSOUT?                        00060000
         BO    LENSET         YES, USE WHAT WE'VE GOT                   00060100
*        GET THE TERMINAL LINE SIZE TO SEE IF IT MAKES SENSE            00060200
         GTSIZE                                                         00060300
         LTR   R1,R1          SEE IF IT'S GOOD                          00060400
         BZ    LENSET         NO, JUST A ZERO, KEEP THE DEFAULTS        00060500
         BCTR  R1,0           CUT IT DOWN ONE TO AVOID A MESS           00060600
         STH   R1,LINELEN     SAVE THIS LENGTH                          00060700
         LTR   R0,R0          FOR DISPLAYS, IT'S SCREEN SIZE            00060800
         BZ    LENSET         KEEP WHAT WE'VE GOT                       00060900
         TM    LINESPP+6,X'80'  WAS LINES PER PAGE ENTERED?             00061000
         BO    LENSET           YES, DON'T OVERRIDE IT                  00061100
         STH   R0,LINEMAX     AND SAVE THE NEW PAGE LOCATION            00061200
LENSET   DS    0H                                                       00061300
*                                                                       00061400
*        SET UP THE PAGE COUNTER                                        00061500
*                                                                       00061600
PAGEAD   LH    R1,LINELEN     GET THE LENGTH OF THE LINE                00061700
         SH    R1,H10         MINUS TEN CHARACTERS                      00061800
         LA    R1,PRNTHEAD(R1) THEN RELOCATE IT                         00061900
         CLEAR PRNTHEAD       CLEAR THE LINE FIRST                      00062000
         MVC   0(4,R1),CPAGE  MOVE IN THE CHARACTERS PAGE               00062100
         LA    R1,5(R1)       MOVE OVER 5 MORE                          00062200
         ST    R1,PAGEADDR    THIS IS THE PLACE                         00062300
*                                                                       00062400
*        BUILD THE PRINT HEADER LINE                                    00062500
*                                                                       00062600
         TM    HEADING+6,X'80' IS A USER HEADING PRESENT                00062700
         BO    USERHEAD       YES, USE IT                               00062800
         MVC   PRNTHEAD(L'DEFHEAD),DEFHEAD  NO, GET A DEFAULT           00062900
*                                                                       00063000
*        ADD THE COMMAND BUFFER TO THE HEADING                          00063100
*                                                                       00063200
         LH    R1,LINELEN    GET THE LINE LENGTHE                       00063300
         SH    R1,H32        MINUS SPACES FOR PAGE, START OF HDR        00063400
         BNP   OUTOPEN       IF IT'S SHORT, SKIP ON                     00063500
         L     R14,ADDRCBUF  POINT TO THE COMMAND BUFFER                00063600
         CH    R1,0(R14)     COMPARE LENGTHS                            00063700
         BL    CBUFBIG       THE COMMAND BUFFER TOO BIG                 00063800
         LH    R1,0(R14)     GET THE COMMAND BUFFER SIZE                00063900
CBUFBIG  SH    R1,H5         MINUS 1 FOR EX, 4 FOR CBUF PREFIX          00064000
         EX    R1,MOVEHED    MOVE THE CBUF TO THE DEFAULT HEADER        00064100
         B     OUTOPEN        GO SET UP THE PAGE COUNTER                00064200
USERHEAD LH    R1,HEADING+4   GET THE LENGTH                            00064300
         BCTR  R1,0           MINUS ONE FOR THE EX                      00064400
         L     R2,HEADING     POINT TO THE USER HEAD                    00064500
         EX    R1,MOVEHEAD    THEN MOVE IT IN                           00064600
OUTOPEN  DS    0H                                                       00064700
*                                                                       00064800
*        SET UP THE WORK LINE                                           00064900
*                                                                       00065000
         LH    R1,LINELEN    GET THE LINE LENGTH                        00065100
         LA    R1,4(R1)      ADD FOUR FOR THE PREFIX                    00065200
         STH   R1,WORKLINE   OUTPUT TEXT                                00065300
         STH   R1,PRNTTITH    ITEM TITLES                               00065400
*        GET THE PRINT SPECIFICATION                                    00065500
*                                                                       00065600
         LA    R1,DEFPRNT                                               00065700
         ST    R1,VTPRNTLS   SAVE THE PRINT ITEM LIST ADDRESS           00065800
         TM    SUBPRTKY+6,X'80'  WERE ANY ITEMS SET UP                  00065900
         BE    PRTITSET      NO, THE DEFAULT LIST IS OK                 00066000
*                                                                       00066100
*        GET THE ADD, REPLACE, DELETE, AND NEW ITEMS                    00066200
*        AND BUILD THE NEW LIST                                         00066300
*                                                                       00066400
*        FIRST CONVERT THE ENTERED TEXT INTO NUMERIC KEYS               00066500
*                                                                       00066600
         LA    R4,SUBPRTIT   POINT TO THE ITEMS                         00066700
         LA    R5,VTPRNTEN   POINT TO THE OUTPUT KEYS                   00066800
         LA    R0,VTPRNTEX   POINT TO THE END OF THE LIST               00066900
ENTKEY   BAL   R8,GETKEY     GET A KEY                                  00067000
         STC   R15,0(R5)     SAVE IT                                    00067100
         LA    R5,1(R5)      GET TO THE NEXT ONE                        00067200
         CR    R0,R5         CHECK FOR THE END                          00067300
         BNH   ENTKEND       IF THAT'S ALL                              00067400
         ICM   R4,7,9(R4)    GET THE CHAIN POINTER                      00067500
         BNZ   ENTKEY        AND KEEP GOING IF THERE'S MORE             00067600
ENTKEND  DS    0H            THE KEYS ARE ENTERED INTO THE LIST         00067700
*                                                                       00067800
*        MERGE THE ENTERED ITEMS AND THE DEFAULT LIST INTO A NEW LIST   00067900
*                                                                       00068000
         LA    R6,VTPRNTL    POINT TO THE NEW LIST                      00068100
         ST    R6,VTPRNTLS   SAVE IT'S ADDRESS                          00068200
         LA    R4,VTPRNTEN   POINT TO THE ENTERED ITEMS                 00068300
         LA    R2,DEFPRNT    POINT TO THE DEFAULT LIST                  00068400
         LR    R3,R2         POINT TO THE BEGINNING - NO DEFAULT IF NEW 00068500
         L     R1,SUBPRTKY   POINT TO THE KEYWORD                       00068600
         CLI   0(R1),C'N'    IS THIS A NEW LIST?                        00068700
         BE    PRTINEW       YES, SKIP PAST DEFAULT COPY                00068800
         LA    R3,DEFPRNTE   POINT TO THE END OF THE DEFAULT LIST       00068900
*                                                                       00069000
*        ADD, REPLACE, DELETE - COPY THE DEFAULT LIST                   00069100
*                                                                       00069200
PRTICOPY CR    R2,R3         IS THIS THE END OF THE DEFAULT LIST?       00069300
         BNL   PRTITSET      YES, END OF PROCESSING FOR PRINT ITEMS     00069400
*                                                                       00069500
*        CHECK FOR ENTERED ITEMS THAT ARE ALSO                          00069600
*              IN THE DEFAULT LIST.                                     00069700
*                                                                       00069800
PRTICDLP CLC   0(1,R2),0(R4) IS THIS THE SAME ITEM                      00069900
         BE    PRTIFND       YES, SEE WHAT TO DO                        00070000
         LA    R4,1(R4)      NO, GET TO THE NEXT ITEM                   00070100
         CLI   0(R4),0       WAS THIS THE LAST ENTERED ITEM?            00070200
         BNE   PRTICDLP      NO, KEEP LOOKING                           00070300
         LA    R4,VTPRNTEN   POINT BACK TO THE TOP OF THE LIST          00070400
*                                                                       00070500
*        ADD THIS ITEM TO THE NEW LIST                                  00070600
*                                                                       00070700
PRTICSKP MVC   0(1,R6),0(R2) MOVE IN THE NEW KEY                        00070800
         LA    R6,1(R6)      POINT PAST IT                              00070900
PRTICDEL LA    R2,1(R2)      GO DOWN THE DEFAULT LIST                   00071000
         B     PRTICOPY      THEN KEEP ON CHECKING                      00071100
*                                                                       00071200
*        AN ITEM WAS ENTERED AND WAS IN THE DEFAULT LIST                00071300
*        FOR DELETE, JUST DELETE ITEMS                                  00071400
*        FOR ADD AND REPLACE, DELETE ALL BUT THE FIRST ITEM             00071500
*              TO AVOID DUPLICATES                                      00071600
*              IF IT IS THE FIRST ITEM, INSERT THE ENTERED LIST         00071700
*                                                                       00071800
PRTIFND  LA    R0,VTPRNTEN   POINT TO THE FIRST ITEM                    00071900
         CR    R0,R4         COMPARE WITH THE ITEM FOUND                00072000
         BH    PRTICDEL      NOT THE FIRST ITEM, DELETE IT              00072100
         CLI   0(R1),C'D'    IS THIS DELETE TIME?                       00072200
         BE    PRTICDEL      THEN JUST DELETE IT                        00072300
         CLI   0(R1),C'R'    IS THIS A REPLACE?                         00072400
         BE    PRTIREPA      YES, IGNORE THIS DEFAULT ITEM              00072500
*                                                                       00072600
*        ADD THE DEFAULT ITEM FIRST                                     00072700
*                                                                       00072800
PRTIASKP MVC   0(1,R6),0(R2) MOVE IN THE NEW KEY                        00072900
         LA    R6,1(R6)      POINT PAST IT                              00073000
*                                                                       00073100
*        MOVE THE ITEMS IN FROM THE ADD OR REPLACE LIST                 00073200
*                                                                       00073300
PRTIREPA LA    R2,1(R2)      GET PAST THE DEFAULT LIST ITEM             00073400
PRTIREP  LA    R4,1(R4)      GET PAST THE FIRST ENTRY                   00073500
PRTINEXT CLI   0(R4),0       IS THIS THE LAST ITEM?                     00073600
         BE    PRTICOPY      YES, SEE ABOUT MORE DEFAULTS               00073700
*                                                                       00073800
*        ADD AN ENTERED ITEM TO THE LIST                                00073900
*                                                                       00074000
PRTINSKP MVC   0(1,R6),0(R4) MOVE IN THE NEW KEY                        00074100
         LA    R6,1(R6)      POINT PAST IT                              00074200
         B     PRTIREP       GO GET MORE ENTERED ITEMS                  00074300
*                                                                       00074400
*        NEW LIST, JUST USE IT AS ENTERED                               00074500
*                                                                       00074600
PRTINEW  LA    R1,VTPRNTEN   POINT TO THE ENTERED LIST                  00074700
         ST    R1,VTPRNTLS   THEN SAVE ITS ADDRESS FOR LATER            00074800
PRTITSET DS    0H                                                       00074900
*                                                                       00075000
*        BUILD THE TITLE LINE FOR THE DSNAME FIELDS                     00075100
*                                                                       00075200
         CLEAR PRNTTITL        BLANK OUT THE TITLE LINE                 00075300
         MVC   PRNTTITL+1(139),PRNTTITL  SO THE WHOLE THING IS GOOD     00075400
         L     R2,VTPRNTLS   GET THE PRINT ITEM LIST                    00075500
         LA    R1,PRNTTITL    POINT TO THE TITLE LINE                   00075600
TITLOOP  SR    R6,R6          GET THE RESERVED WORD NUMBER              00075700
         IC    R6,0(R2)       FROM THE TOP BYTE                         00075800
         MH    R6,H12         MULTIPLY BY 12 FOR THE TABLE ENTRIES      00075900
         LA    R6,TABTITL(R6) THEN RELOCATE THE MESS                    00076000
         SR    R7,R7          CLEAR A REGISTER                          00076100
         IC    R7,0(R6)       GET THE EXECUTE LENGTH                    00076200
         EX    R7,MOVETIT     MOVE IN THE TITLE                         00076300
         LA    R1,1(R1,R7)    MOVE THE POINTER OVER                     00076400
         CLI   0(R2),DSNAME  IS THIS THE DSNAME KEY                     00076500
         BE    TITDSN        YES, SPECIAL PROCESSING                    00076600
         LA    R0,PRNTTITL    POINT TO THE BEGINNING AGAIN              00076700
         SR    R1,R0          AND FIND THE CURRENT LENGTH               00076800
         CH    R1,LINELEN     IS IT TOO LONG?                           00076900
         BNL   TITOVER        YES, PULL BACK                            00077000
         AR    R1,R0          NO, KEEP GOING                            00077100
TITINC   LA    R2,1(R2)            GET THE NEXT CHAIN POINTER           00077200
         CLI   0(R2),0       ARE WE DONE?                               00077300
         BNE   TITLOOP        GO GET MORE TITLES                        00077400
         B     TITEND         ALL DONE                                  00077500
TITDSN   LR    R6,R1         SAVE THE ADDRESS POINTER                   00077600
         LA    R1,DSNPLN     POINT TO THE PDL FOR DSN LENGTH            00077700
         BAL   R8,PDLNUM     GO TRANSLATE IT                            00077800
         LTR   R15,R15       WAS IT THERE?                              00077900
         BP    TITDSN2       YES, USE IT                                00078000
         LA    R15,44        NO, SET THE DEFAULT                        00078100
TITDSN2  STH   R15,DSNLENGT  SAVE THE LENGTH                            00078200
         SH    R15,H9        SUBTRACT THE 9 CHARS MOVED ALREADY         00078300
*              MINUS ONE FOR EX, PLUS ONE FOR SPACE                     00078400
         MVI   0(R6),C' '    GET AN INITIAL BLANK                       00078500
         EX    R15,DSNBLMOV  MOVE IN THE BLANKS                         00078600
         LA    R1,1(R15,R6)  RESET THE POINTER ( INCLUDE A SPACE )      00078700
         LA    R15,1(R15)    ADD ON THE SPACE CHARACTER                 00078800
         STH   R15,DSNLENOF  SAVE THE OFFSET                            00078900
         B     TITINC        ALLOW DSNAME TO OVERFLOW THE LINE          00079000
*                                                                       00079100
*        TITLE RAN OFF THE END, CUT IT OFF                              00079200
*                                                                       00079300
TITOVER  SR    R1,R7          SUBTRACT PAST THIS FIELD                  00079400
         AR    R1,R0          RELOCATE IT                               00079500
         BCTR  R1,0           THEN GET THE LAST CHARACTER               00079600
         MVC   0(9,R1),BLANKS THEN BLANK IT OUT                         00079700
TITEND   TM    SYSOUT+48,X'10' IS THE DCB OPEN                          00079800
         BNO   CKOUTPT       NO, TERMINAL OUTPUT, NO CC                 00079900
         MVI   PRNTTITL,C'0'  ALWAYS SKIP A LINE FOR IT                 00080000
CKOUTPT  CLI   OUTPUTK+1,1    OUTPUT THIS RUN?                          00080100
         BNE   VTRET          YES, SKIP THE OPEN, JUST RETURN           00080200
         MVC   OUTDCB(OUTDCBL),OUTDCBC  INITIALIZE THE DCB              00080300
         MVI   OPENLIST,X'80' TERMINATE THE LIST                        00080400
         OPEN  (OUTDCB,OUTPUT),MF=(E,OPENLIST)  OPEN THE DATA SET DCB   00080500
         B     VTRET          RETURN, INITIALIZATION IS DONE            00080600
         EJECT                                                          00080700
*                                                                       00080800
*        ROUTINE TO CONVERT A TEXT DSCB ITEM                            00080900
*        INTO ITS KEY NUMBER                                            00081000
*        INPUT IS REG 4 - IKJIDENT PTR                                  00081100
*        OUTPUT IS REG 15 - KEY NUMBER                                  00081200
*        ENTRY VIA BAL   R8,GETKEY                                      00081300
*                                                                       00081400
GETKEY   L     R1,ATABTITL     POINT TO THE TABLE                       00081500
         LA    R1,12(R1)     POINT TO THE FIRST ENTRY                   00081600
         LA    R15,1           SET UP THE KEY NUMBER COUNTER            00081700
         L     R6,0(R4)      POINT TO THE ENTERED TEXT                  00081800
         ICM   R3,3,4(R4)    GET THE LENGTH OF THE ENTERED TEXT         00081900
         BNP   GETKNOTF      NOT FOUND IF ZERO                          00082000
         BCTR  R3,0          MINUS ONE FOR THE EX                       00082100
GETKLOOP LA    R2,4(R1)      POINT TO THE COMPARISON TEXT               00082200
         CLI   0(R2),C' '    IS IT HERE?                                00082300
         BNE   GETKSTD       YES, THIS IS IT                            00082400
         LA    R2,1(R2)      NO, MOVE OVER ONE MORE                     00082500
         CLI   0(R2),C' '    IS IT HERE?                                00082600
         BNE   GETKSTD       YES, THIS IS IT                            00082700
         LA    R2,1(R2)      NO, MOVE OVER ONE MORE                     00082800
GETKSTD  EX    R3,GETKCOMP   COMPARE THE KEY TEXT                       00082900
         BE    GETKFND       I FOUND IT                                 00083000
         LA    R1,12(R1)     GET TO THE NEXT KEY                        00083100
         LA    R15,1(R15)    INCREMENT THE KEY COUNTER                  00083200
         CH    R15,H26       CHECK FOR THE END OF THE TABLE             00083300
         BNH   GETKLOOP      NOT YET, KEEP LOOKING                      00083400
*                                                                       00083500
*        KEY WAS NOT FOUND, SEND BACK A ZERO                            00083600
*                                                                       00083700
GETKNOTF SR    R15,R15       SET UP THE ZERO AND RETURN                 00083800
GETKFND  BR    R8            JUST RETURN                                00083900
GETKCOMP CLC   0(0,R6),0(R2) EXECUTED TEXT COMPARE                      00084000
H26      DC    H'26'                                                    00084100
         EJECT                                                          00084200
*                                                                       00084300
*        PROGRAM CONSTANTS                                              00084400
*                                                                       00084500
ZERO     DC    F'0'                                                     00084600
F100     DC    F'100'                                                   00084700
F127     DC    F'127'                                                   00084800
DEFLMAX  DC    H'60'          DEFAULT LINES PER PAGE                    00084900
DEFLEN   DC    H'132'         DEFAULT CHARS PER LINE                    00085000
H2       DC    H'2'                                                     00085100
H5       DC    H'5'                                                     00085200
H9       DC    H'9'                                                     00085300
H10      DC    H'10'                                                    00085400
H12      DC    H'12'                                                    00085500
H18      DC    H'18'                                                    00085600
H32      DC    H'32'                                                    00085700
MOVETIT  MVC   0(0,R1),3(R6)  MOVE IN THE TITLE                         00085800
COMPKEY  CLC   0(0,R1),VTFDSN  EXECUTED COMPARE                         00085900
MOVEHEAD MVC   PRNTHEAD(0),0(R2)                                        00086000
MOVEHED  MVC   PRNTHEAD+21(0),4(R14)  MOVE CMD BUF TO DEFAULT HEADER    00086100
DSNBLMOV MVC   1(0,R6),0(R6)  BLANK OUT THE DSN SPACE IN THE TITLE      00086200
CPAGE    DC    C'PAGE'                                                  00086300
EDMASK   DC    XL16'40202020202020202020202020202120'                   00086400
EDMASK0  DC    XL16'F0202020202020202020202020202120'                   00086500
SKIP     DC    C'0'                                                     00086600
COMP0    DC    CL16'0000000000000000'                                   00086700
BLANKS   DC    CL16'                '                                   00086800
STARS    DC    CL16'****************'                                   00086900
TABSPACE DC    CL8'TRKS'                                                00087000
         DC    CL8'TRKS'                                                00087100
         DC    CL8'TRKS'                                                00087200
         DC    CL8'TRKS'                                                00087300
         DC    CL8'TRKS'                                                00087400
*                                                                       00087500
         PRINT NOGEN                                                    00087600
SYSOUTC  DCB   DSORG=PS,DDNAME=VTOCOUT,MACRF=PM,                       X00087700
               RECFM=FBA,LRECL=150,BLKSIZE=1500                         00087800
OUTDCBC  DCB   DSORG=PS,DDNAME=OUTPUT,MACRF=PM,                        X00087900
               RECFM=FB,LRECL=100,BLKSIZE=6000                          00088000
*                                                                       00088100
*        PROGRAM MESSAGES                                               00088200
*                                                                       00088300
         SPACE 2                                                        00088400
         SPACE                                                          00088500
MSGTOTC  MSG   '  TOTALS -  NNNN DATA SETS, MMMMMMMM UUUUUU ALLOC, LLLLX00088600
               LLLL UUUUUU USED '                                       00088700
MSGTLEN  EQU   *-MSGTOTC                                                00088800
MSGBLC   MSG   '                '                                       00088900
*                                                                       00089000
DEFHEAD  DC    CL20'1 VTOC COMMAND  V-77'                               00089100
*                                                                       00089200
*        DEFAULT PRINT LIST                                             00089300
*                                                                       00089400
DEFPRNT  DC    AL1(ALLOC)                                               00089500
         DC    AL1(UNUSED)                                              00089600
         DC    AL1(PCT)                                                 00089700
         DC    AL1(EXT)                                                 00089800
         DC    AL1(DSORG)                                               00089900
         DC    AL1(RECFM)                                               00090000
         DC    AL1(LRECL)                                               00090100
         DC    AL1(BLKSZ)                                               00090200
         DC    AL1(CDATE)                                               00090300
         DC    AL1(LSTUS)                                               00090400
         DC    AL1(VOLUME)                                              00090500
         DC    AL1(DSNAME)                                              00090600
         DC    AL1(EXPDT)                                               00090700
         DC    AL1(SECQ)                                                00090800
         DC    AL1(SECT)                                                00090900
         DC    AL1(ROUND)                                               00091000
         DC    AL1(PASS)                                                00091100
         DC    AL1(ACTION)                                              00091200
         DC    AL1(TYPE)                                                00091300
         DC    AL1(0)        END OF THE LIST                            00091400
DEFPRNTE EQU   *                                                        00091500
DEFPRNTL EQU   *-DEFPRNT                                                00091600
*                                                                       00091700
*        TABLE OF PRINT ITEM LENGTHS AND TITLES                         00091800
*                                                                       00091900
*        ENTRIES IN THE TABLE FOR EACH FORMATTED ITEM -                 00092000
*        FIRST BYTE IS FIELD LENGTH FOR OUTPUT ( MINUS ONE FOR EX )     00092100
*        SECOND BYTE - X'80' BIT INDICATES A CHARACTER FIELD            00092200
*                            THEN BITS 0-7 GIVE VTFMT LENGTH            00092300
*                      OTHERWISE IT'S A KEY TO WHICH ROUTINE TO USE     00092400
*        THIRD BYTE - OFFSET IN FORMATTED DSCB, VTFMT                   00092500
*        4-12 TH BYTES, THE TITLE FOR THE FIELD                         00092600
*                                                                       00092700
*                                                                       00092800
TABTITL  DC    XL12'00'  DUMMY ENTRY FOR 0 ADDRESSING                   00092900
         DC    AL1(8),AL1(128+7),AL1(VTFACTON-VTFMT),CL9' ACTION  '  1  00093000
         DC    AL1(6),AL1(128+5),AL1(VTFVOLUM-VTFMT),CL9' VOLUME  '  2  00093100
         DC    AL1(5),AL1(000+4),AL1(VTFCREDT-VTFMT),CL9' CDATE   '  3  00093200
         DC    AL1(5),AL1(000+4),AL1(VTFLSTAC-VTFMT),CL9' REFDT   '  4  00093300
         DC    AL1(5),AL1(000+4),AL1(VTFEXPDT-VTFMT),CL9' EXPDT   '  5  00093400
         DC    AL1(6),AL1(00+12),AL1(VTFALLOC-VTFMT),CL9'  ALLOC  '  6  00093500
         DC    AL1(6),AL1(00+16),AL1(VTFALLOC-VTFMT),CL9' UNUSED  '  7  00093600
         DC    AL1(3),AL1(00+20),AL1(VTFALLOC-VTFMT),CL9' PCT     '  8  00093700
         DC    AL1(2),AL1(00+24),AL1(VTFNOEPV-VTFMT),CL9' EX      '  9  00093800
         DC    AL1(3),AL1(128+2),AL1(VTFDSORG-VTFMT),CL9' DSO     ' 10  00093900
         DC    AL1(3),AL1(128+3),AL1(VTFRECFM-VTFMT),CL9' RFM     ' 11  00094000
         DC    AL1(5),AL1(00+28),AL1(VTFBLKSZ-VTFMT),CL9' BLKSZ   ' 12  00094100
         DC    AL1(5),AL1(00+28),AL1(VTFLRECL-VTFMT),CL9' LRECL   ' 13  00094200
         DC    AL1(4),AL1(128+0),AL01(VTFPROT-VTFMT),CL9' PASS    ' 14  00094300
         DC    AL1(3),AL1(128+0),AL1(VTFCATLG-VTFMT),CL9' CAT     ' 15  00094400
         DC    AL1(4),AL1(128+0),AL1(VTFSECAL-VTFMT),CL9' SECT    ' 16  00094500
         DC    AL1(5),AL1(00+28),AL1(VTFSECAM-VTFMT),CL9'  SECQ   ' 17  00094600
         DC    AL1(4),AL1(00+36),AL1(VTFVOLUM-VTFMT),CL9' UNIT    ' 18  00094700
         DC    AL1(5),AL1(128+0),AL1(VTFROUND-VTFMT),CL9' ROUND   ' 19  00094800
         DC    AL1(4),AL1(128+0),AL1(VTFDSTYP-VTFMT),CL9' TYPE    ' 20  00094900
         DC    AL1(6),AL1(00+12),AL01(VTFUSED-VTFMT),CL9'   USED  ' 21  00095000
         DC    AL1(8),AL1(00+32),AL1(VTFVOLUM-VTFMT),CL9'   CCHH  ' 22  00095100
         DC    AL1(6),AL1(128+0),AL1(VTFVOLUM-VTFMT),CL9' DUMMY3  ' 23  00095200
         DC    AL1(6),AL1(128+0),AL1(VTFVOLUM-VTFMT),CL9' DUMMY4  ' 24  00095300
         DC    AL1(6),AL1(128+0),AL1(VTFVOLUM-VTFMT),CL9' DUMMY5  ' 25  00095400
         DC    AL1(8),AL1(00+32),AL001(VTFDSN-VTFMT),CL9' DSNAME  ' 26  00095500
         EJECT                                                          00095600
*                                                                       00095700
*                                                                       00095800
*        P A R S E   C O N T R O L   L I S T                            00095900
*                                                                       00096000
*                                                                       00096100
         PRINT OFF                                                      00096200
         COPY  VTOCPARS                                                 00096300
         PRINT ON                                                       00096400
*                                                                       00096500
*        DYNAMIC WORK AREA                                              00096600
*                                                                       00096700
         SPACE 3                                                        00096800
PRNTWORK DSECT                                                          00096900
         DS    18A            PRINT ROUTINE SAVE AREA                   00097000
TOTDS    DS    F              TOTAL COUNTER                             00097100
TOTALLOC DS    F              TOTAL ALLOCATION                          00097200
TOTUSED  DS    F              TOTAL USED                                00097300
FTOTDS   DS    F              FINAL TOTAL DATA SETS                     00097400
FTOTALLC DS    F              FINAL TOTAL ALLOC                         00097500
FTOTUSED DS    F              FINAL TOTAL USED                          00097600
PRNTLSAV DS    A                                                        00097700
PRINTR8  DS    A                                                        00097800
PRNTTOT8 DS    A                                                        00097900
ADDREND  DS    A                                                        00098000
PAGEADDR DS    A                                                        00098100
OPENLIST DS    2A             PARM LIST FOR OPEN                        00098200
PDLNSAVE DS    8A             REGISTER SAVE AREA FOR PDLNUM RTN         00098300
PRTLSAVE DS    8A             REGISTER SAVE AREA FOR PRNTLINE RTN       00098400
LASTKEY  DS    A              ADDRESS OF LAST KEY FOR SUBTOTALS, BREAKS 00098500
NUMBREAK DS    H              CHARACTERS TO COMPARE FOR BREAK           00098600
NUMTOTAL DS    H              CHARACTERS TO COMPARE FOR SUBTOTALS       00098700
         PRINT NOGEN                                                    00098800
SYSOUT   DCB   DSORG=PS,DDNAME=VTOCOUT,MACRF=PM,                       X00098900
               RECFM=FBA,LRECL=150,BLKSIZE=1500                         00099000
SYSOUTL  EQU   *-SYSOUT                                                 00099100
OUTDCB   DCB   DSORG=PS,DDNAME=OUTPUT,MACRF=PM,                        X00099200
               RECFM=FB,LRECL=100,BLKSIZE=6000                          00099300
OUTDCBL  EQU   *-OUTDCB                                                 00099400
ENDTOTAL DS    X              PROGRAM SWITCHES                          00099500
ENTOTOUT EQU   X'80'          THE FINAL TOTALS HAVE BEEN OUTPUT         00099600
ENDTONLY EQU   X'10'          NO SUBTOTALS, END TOTALS ONLY             00099700
NOBREAK  EQU   X'08'          NO BREAKS                                 00099800
FIRSTIM  DS    X              INITIALIZATION FOR THIS ROUTINE           00099900
PDLMINUS DC    X'00'                                                    00100000
TOTLAST  DS    X                                                        00100100
SPACTYPE DS    CL6          CHARACTERS FOR SPACE UNITS                  00100200
DSNLENGT DS    H                                                        00100300
DSNLENOF DS    H                                                        00100400
CHARS    DS    CL16           CONVERSION TO CHARACTERS                  00100500
MSGWORK  DS    CL256          AREA FOR BUILDING MESSAGES                00100600
WORKLINE DS    CL256          AREA FOR DATA SET                         00100700
PRNTHDRH DS    F                                                        00100800
PRNTHEAD DS    CL256          AREA FOR HEADER                           00100900
PRNTTITH DS    F                                                        00101000
PRNTTITL DS    CL256          AREA FOR ITEM TITLES                      00101100
MSGBL    DS    CL20           AREA FOR BLANK LINE                       00101200
OUTWORK  DS    CL256          WORKING AREA FOR OUTPUT                   00101300
DCBEXIT  DS    F                                                        00101400
JFCB     DS    XL176                                                    00101500
         SPACE                                                          00101600
VTPRNTLS DS    A             PRINT ITEM LIST ADDRESS                    00101700
VTPRNTL  DS    40C           PRINT ITEM LIST ( IF MODIFIED )            00101800
VTPRNTEN DS    40C                                                      00101900
VTPRNTEX DS    C                                                        00102000
         SPACE                                                          00102100
         DS    0D                                                       00102200
LENWORK  EQU   *-PRNTWORK                                               00102300
*                                                                       00102400
*        VTOC COMMAND COMMON AREA                                       00102500
*                                                                       00102600
         PRINT NOGEN                                                    00102700
         VTOCOM                                                         00102800
         SPACE 3                                                        00102900
*                                                                       00103000
*        FORMATTED DSCB                                                 00103100
*                                                                       00103200
         VTFMT                                                          00103300
         SPACE 3                                                        00103400
         PDEDSNAM                                                       00103500
         SPACE 3                                                        00103600
DUMMD    DSECT                                                          00103700
DUMMA    DS    C              DUMMY ENTRY TO USE FOR CONV               00103800
         PRINT NOGEN                                                    00103900
         DCBD  DSORG=PS,DEVD=DA                                         00104000
         END                                                            00104100
./       ADD   NAME=VTOCSORT
         TITLE 'VTOC COMMAND  SORT  ROUTINE'                            00000100
*********************************************************************** 00000200
*                                                                     * 00000300
*                                                                     * 00000400
* TITLE -      VTOC COMMAND  SORT  ROUTINE                            * 00000500
*                                                                     * 00000600
* FUNCTION -   PUT THIS FORMATTED DSCB INTO THE SORTED LIST.          * 00000700
*                                                                     * 00000800
* OPERATION -  IF THIS IS A NOSORT RUN, JUST CALL THE PRINT ROUTINE.  * 00000900
*              TO BUILD THE SORTED LIST, FIRST DO A SIMPLE HASH       * 00001000
*              ON THE FIRST CHARACTER.  BUILD UP TO 256 SEPARATE      * 00001100
*              LISTS TO SAVE SORT TIME.  THEN SEARCH THROUGH THESE    * 00001200
*              LISTS SEQUENTIALLY.                                    * 00001300
*                                                                     * 00001400
* INPUT -      VTOC COMMON AREA ( VTOCOM )                            * 00001500
*              POINTED TO BY REGISTER 1                               * 00001600
*              USE PARSE DATA, CURRENT FORMATTED DSCB, SORTED LIST    * 00001700
*                                                                     * 00001800
* OUTPUT -     THE FORMATTED DSCB IS PLACED INTO THE SORTED LIST.     * 00001900
*                                                                     * 00002000
* ATTRIBUTES - REENTRANT, REUSEABLE, REFRESHABLE.                     * 00002100
*                                                                     * 00002200
*                                                                     * 00002300
*         PROGRAMMED BY R. L. MILLER  (415) 485-6241                  * 00002400
*                                                                     * 00002500
*                                                                     * 00002600
*********************************************************************** 00002700
         EJECT                                                          00002800
VTOCSORT ENTER 12,24          DO THE HOUSEKEEPING                       00002900
         LR    R11,R1         SAVE ADDR OF VTOCOM                       00003000
         USING VTOCOM,R11     SET ITS ADDRESSABILITY                    00003100
         L     R9,ADDRANSR    POINT TO THE PARSE ANSWER                 00003200
         USING PDL,R9         SET ITS ADDRESSABILITY                    00003300
         USING SORTWORK,R13   SET ADDRESSABILITY FOR LOCAL WORK AREA    00003400
         SPACE 3                                                        00003500
*                                                                       00003600
*        IS THIS A NOSORT RUN ?                                         00003700
*        IF SO, JUST CALL PRINT                                         00003800
*                                                                       00003900
         CLI   SORTK+1,2      IS THIS NOSORT?                           00004000
         BNE   GOSORT         NO, KEEP ON TRUCKIN'                      00004100
         VTCALL PRNT          YES, CALL PRINT AND GET OUT               00004200
         B     SORTRET        GET OUT OF HERE                           00004300
*                                                                       00004400
*        PUT THIS ENTRY WHERE IT BELONGS                                00004500
*                                                                       00004600
GOSORT   L     R3,FORMATAD    POINT TO THE FORMATTED DSCB               00004700
         USING VTFMT,R3       SET ADDRESSABILITY                        00004800
         LA    R6,SORTTAB     POINT TO THE SORT FIELDS TABLE            00004900
         SR    R4,R4                                                    00005000
         IC    R4,1(0,R6)     LOAD HIGH KEY OFFSET                      00005100
         LA    R4,VTFMT(R4)   POINT TO HIGH KEY                         00005200
         LA    R2,VTCSORTH-12 SORT HEADER AREA                          00005300
GOSORT1  LA    R2,12(0,R2)    NEXT ENTRY                                00005400
         LH    R5,4(0,R2)     LOAD COMAPRE LENGTH                       00005500
         CLI   0(R6),C'D'     DESCENDING SORT                           00005600
         BE    GOSORT3        YES                                       00005700
         B     GOSORT4        NO                                        00005800
GOSORT2  ICM   R5,B'1111',0(R2) GET THE HEAD OF THE LIST                00005900
         BNZ   NOTFIRST       IF NON-ZERO, SEARCH THE LIST              00006000
*                                                                       00006100
*        FIRST ENTRY ON THE LIST, IT'S EASY                             00006200
*                                                                       00006300
         ST    R3,0(R2)       START UP THE LIST                         00006400
         B     SORTRET        THEN RETURN                               00006500
GOSORT3  EX    R5,GOSORTCL    COMPARE TO GET CORRECT LIST               00006600
         BL    GOSORT1                                                  00006700
         B     GOSORT2                                                  00006800
GOSORT4  EX    R5,GOSORTCL    COMPARE TO GET CORRECT LIST               00006900
         BH    GOSORT1                                                  00007000
         B     GOSORT2                                                  00007100
*                                                                       00007200
*        FIND A SLOT FOR THIS ENTRY                                     00007300
*              FIRST GET THE SHORTER DSN LENGTH                         00007400
*                                                                       00007500
NOTFIRST SR    R1,R1                                                    00007600
         IC    R1,1(0,R6)     OFFSET OF SORT FIELD                      00007700
         LA    R7,0(R1,R5)    LOAD PREV ENTRY FIELD ADDR                00007800
         LA    R8,0(R1,R3)    LOAD NEW ENTRY FIELD ADDR                 00007900
         C     R1,=A(VTFDSN-VTFMT)  DSN                                 00008000
         BNE   NOTFRST1                                                 00008100
         LH    R1,VTFDSNL-VTFMT(0,R3)                                   00008200
         CH    R1,VTFDSNL-VTFMT(0,R5)                                   00008300
         BNH   NOTFRST0                                                 00008400
         LH    R1,VTFDSNL-VTFMT(0,R5)                                   00008500
NOTFRST0 BCTR  R1,0                                                     00008600
         B     NOTFRST2                                                 00008700
NOTFRST1 LH    R1,2(0,R6)     LOAD SORT FIELD EXEC LENGTH               00008800
NOTFRST2 CLI   0(R6),C'D'     DESCENDING SORT                           00008900
         BE    NOTFRST4       YES                                       00009000
NOTFRST3 EX    R1,COMPVTF     COMPARE THE FIELDS                        00009100
         BL    NEXTENT        LIST ENTRY IS LOWER, UP THE CHAIN         00009200
         BE    CHECKNXT       IDENTICAL, CHECK NEXT FIELD               00009300
         B     INSERT                                                   00009400
NOTFRST4 EX    R1,COMPVTF     COMPARE THE FIELDS                        00009500
         BH    NEXTENT        LIST ENTRY IS LOWER, UP THE CHAIN         00009600
         BE    CHECKNXT       IDENTICAL, CHECK NEXT FIELD               00009700
*                                                                       00009800
*        THE NEW ENTRY GOES HERE                                        00009900
*                                                                       00010000
INSERT   ST    R3,0(R2)       SAVE THE NEW POINTER                      00010100
         ST    R5,VTFNEXT     JUST BEFORE THIS LIST ENTRY               00010200
         B     SORTRET        THEN EXIT                                 00010300
*                                                                       00010400
*                                                                       00010500
CHECKNXT LA    R6,4(0,R6)     NEXT SORT FIELD                           00010600
         CLC   0(4,R6),=F'0'  ANY MORE FIELDS                           00010700
         BE    INSERT         NO, PUT IT HERE                           00010800
         B     NOTFIRST       YES, CHECK IT                             00010900
*                                                                       00011000
*        GET THE NEXT ENTRY ON THIS LIST                                00011100
*                                                                       00011200
NEXTENT  LA    R2,VTFNEXT-VTFMT(R5)  POINT BACK TO THIS ENTRY           00011300
         LA    R6,SORTTAB     RELOAD SORT FIELD TABLE ADDR              00011400
         ICM   R5,B'1111',VTFNEXT-VTFMT(R5)  GET THE NEXT ENTRY         00011500
         BNZ   NOTFIRST       THERE IS ONE, CHECK IT                    00011600
         ST    R3,0(R2)       LAST ENTRY ON THE LIST, PUT IT THERE      00011700
*                                                                       00011800
*        RETURN                                                         00011900
*                                                                       00012000
SORTRET  LEAVE EQ,RC=0                                                  00012100
*                                                                       00012200
*                                                                       00012300
*                                                                       00012400
*        PROGRAM CONSTANTS                                              00012500
*                                                                       00012600
COMPVTF  CLC   0(0,R7),0(R8)     EXECUTED COMPARE                       00012700
GOSORTCL CLC   0(0,R4),6(R2)     EXECUTED COMPARE                       00012800
*                                                                       00012900
*                                                                       00013000
         PRINT NOGEN                                                    00013100
         EJECT                                                          00013200
*                                                                       00013300
*                                                                       00013400
*        P A R S E   C O N T R O L   L I S T                            00013500
*                                                                       00013600
*                                                                       00013700
         PRINT OFF                                                      00013800
         COPY  VTOCPARS                                                 00013900
         PRINT ON                                                       00014000
*                                                                       00014100
*        DYNAMIC WORK AREA                                              00014200
*                                                                       00014300
         SPACE 3                                                        00014400
SORTWORK DSECT                                                          00014500
         DS    18A            PRINT ROUTINE SAVE AREA                   00014600
         SPACE                                                          00014700
         DS    0D                                                       00014800
LENWORK  EQU   *-SORTWORK                                               00014900
*                                                                       00015000
*        VTOC COMMAND COMMON AREA                                       00015100
*                                                                       00015200
         PRINT NOGEN                                                    00015300
         VTOCOM                                                         00015400
         SPACE 3                                                        00015500
*                                                                       00015600
*        FORMATTED DSCB                                                 00015700
*                                                                       00015800
         VTFMT                                                          00015900
         SPACE 3                                                        00016000
         PDEDSNAM                                                       00016100
         SPACE 3                                                        00016200
         END                                                            00016300
><                                                                      00450000
//S2      EXEC VTOCASM,MEMBER=VTOCCHEK                                  00460000
//S3      EXEC VTOCASM,MEMBER=VTOCEXCP                                  00470000
//S4      EXEC VTOCASM,MEMBER=VTOCFORM                                  00480000
//S5      EXEC VTOCASM,MEMBER=VTOCMSGX                                  00490000
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB(VTOCMSG),DISP=SHR                     00500000
//S6      EXEC VTOCASM,MEMBER=VTOCPRNT                                  00510000
//S7      EXEC VTOCASM,MEMBER=VTOCSORT                                  00520000
//S8      EXEC VTOCASM,MEMBER=VTOC                                      00530000
//*                                                                     00540000
//S9      EXEC PGM=IEBGENER,COND=(0,NE)                                 00550000
//SYSIN    DD  DUMMY                                                    00560000
//SYSPRINT DD  SYSOUT=*                                                 00570000
//SYSUT1   DD  DSN=VTOC.SOURCE($HELP),DISP=(OLD,DELETE)                 00580000
//SYSUT2   DD  DSN=SYS2.HELP(VTOC),DISP=SHR                             00590000
//                                                                      00600000
