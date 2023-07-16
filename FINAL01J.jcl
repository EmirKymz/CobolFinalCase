//DATAIO0J JOB ' ',CLASS=A,MSGLEVEL=(1,1),MSGCLASS=X,NOTIFY=&SYSUID
//DELET000 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE Z95620.QSAM.DD NONVSAM
  IF LASTCC LE 08 THEN SET MAXCC = 00
//SORT0100 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD *
00001948Mert Omer      Kaymaz         20080728
00002948Mevlude        Kaymaz         19740625
00004948Er  o      l   KaAymEeZ  aA Ee19700620
00009948Emirhan        Akman          20190406
//SORTOUT  DD DSN=&SYSUID..QSAM.DD,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=61)
//SYSIN    DD *
  SORT FIELDS=(1,7,CH,A)
  OUTREC FIELDS=(1,38,39,8,Y4T,TOJUL=Y4T,15C'0')
//*
//DELET200 EXEC PGM=IEFBR14
//FILE01    DD DSN=&SYSUID..QSAM.FF,
//             DISP=(MOD,DELETE,DELETE),SPACE=(TRK,0)
//SORT0300 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=&SYSUID..QSAM.DD,DISP=SHR
//SORTOUT  DD DSN=&SYSUID..QSAM.FF,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=47)
//SYSIN DD *
  SORT FIELDS=COPY
    OUTREC FIELDS=(1,5,ZD,TO=PD,LENGTH=3,
                   6,3,ZD,TO=BI,LENGTH=2,
                   9,30,
                   39,7,ZD,TO=PD,LENGTH=4,
                   46,15,ZD,TO=PD,LENGTH=8)
//***************************************************/
//***************************************************/
//***************************************************/
//COBRUN  EXEC IGYWCL
// IF RC < 5 THEN
//DELET400 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
   DELETE Z95620.QSAM.INPUT NONVSAM
   IF LASTCC LE 08 THEN SET MAXCC = 00
//RUN     EXEC PGM=SORT
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//SORTIN    DD *
R00001
R00012
D00002
D00032
W00003
W00001
U00004
U00010
C00013
X00023
//SORTOUT   DD DSN=&SYSUID..QSAM.INPUT,
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(10,10),RLSE),
//             DCB=(RECFM=FB,LRECL=6,BLKSIZE=0)
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//SYSIN     DD  *
      SORT FIELDS=COPY
// ELSE
// ENDIF
//***************************************************/
//***************************************************/
//***************************************************/
//DELET500 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN DD *
  DELETE Z95620.VSAM.AA CLUSTER PURGE
  IF LASTCC LE 08 THEN SET MAXCC = 00
  DEF CL ( NAME(Z95620.VSAM.AA)      -
           FREESPACE( 20 20 )        -
           SHR( 2,3 )                -
           KEYS(3 0)                 -
           INDEXED SPEED             -
           RECSZ(47 47)              -
           TRK (10 10)               -
           LOG(NONE)                 -
           VOLUME (VPWRKB)           -
           UNIQUE )                  -
   DATA (NAME(Z95620.VSAM.AA.DATA))  -
   INDEX ( NAME(Z95620.VSAM.AA.INDEX))
//REPRO600 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INN001   DD DSN=Z95620.QSAM.FF,DISP=SHR
//OUT001   DD DSN=Z95620.VSAM.AA,DISP=SHR
//SYSIN    DD *
  REPRO INFILE(INN001) OUTFILE(OUT001)
//***************************************************/
//***************************************************/
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(SUBPROG),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(SUBPROG),DISP=SHR
// IF RC < 5 THEN
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=Z95620.CBL(MAINPROG),DISP=SHR
//LKED.SYSLMOD DD DSN=Z95620.LOAD(MAINPROG),DISP=SHR
//LKED.SYSLIB  DD DSN=&SYSUID..LOAD(SUBPROG),DISP=SHR
// IF RC < 5 THEN
//DELET600 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
   DELETE Z95620.QSAM.OUTPUT NONVSAM
   IF LASTCC LE 08 THEN SET MAXCC = 00
//COBRUN    EXEC PGM=MAINPROG
//STEPLIB   DD DSN=Z95620.LOAD,DISP=SHR
//INPFILE   DD DSN=Z95620.QSAM.INPUT,DISP=SHR
//IDXFILE   DD DSN=Z95620.VSAM.AA,DISP=SHR
//OUTFILE   DD DSN=Z95620.QSAM.OUTPUT,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,
//             SPACE=(TRK,(20,20),RLSE),
//             DCB=(RECFM=FB,LRECL=140)
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
// ELSE
// ENDIF
// ELSE
// ENDIF
//***************************************************/
