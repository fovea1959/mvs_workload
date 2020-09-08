//IDCAMS  JOB (1),IDCAMS,CLASS=A,MSGCLASS=X                             00010000
//IDCAMS01 EXEC PGM=IDCAMS,REGION=4096K                                 00020000
//SYSPRINT DD  SYSOUT=*                                                 00030000
//SPLA00   DD  UNIT=3390,DISP=OLD,VOL=SER=SPLA00                        00040000
//SYSIN    DD  *                                                        00050000
                                                                        00060000
   /* THERE IS A USER CATALOG IN EXISTENCE ON SPLA00 THAT       */      00070000
   /* CONTAINS CATALOG ENTRIES FOR THE DATASETS ON THAT VOLUME. */      00080000
   /* IT IS CONNECTED TO THE MASTER CATALOG AND AN ALIAS TO THE */      00090000
   /* HIGH ORDER INDEX IS DEFINED TO ALLOW ACCESS TO THE        */      00100000
   /* DATASETS CATALOGUED IN THAT USER CATALOG.                 */      00110000
                                                                        00120000
   IMPORT CONNECT OBJECTS ( -                                           00130000
          UCSPLA00  -                                                   00140000
          DEVICETYPE (3390) -                                           00150000
          VOLUMES (SPLA00) )                                            00160000
                                                                        00170000
   DEFINE ALIAS ( -                                                     00180000
         NAME (SPLA) -                                                  00190000
         RELATE (UCSPLA00) )                                            00200000
//                                                                      00220000
