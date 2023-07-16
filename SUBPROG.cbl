       IDENTIFICATION DIVISION.

       PROGRAM-ID.    SUBPROG.
       AUTHOR.        EMIRCAN KAYMAZ.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE ASSIGN TO IDXFILE
                           STATUS ST-IDXFILE
                           ORGANIZATION IS INDEXED
                           ACCESS RANDOM
                           RECORD KEY IS IDX-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD IDX-FILE.
         01 IDX-REC.
           03 IDX-KEY.
              05 IDX-UID           PIC S9(05) COMP-3.
           03 IDX-DVZ          PIC S9(03) COMP.
           03 IDX-NAME         PIC X(15).
           03 IDX-SURNAME      PIC X(15).
           03 IDX-DATE         PIC S9(07) COMP-3.
           03 IDX-BALANCE      PIC S9(15) COMP-3.

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
            03 ST-IDXFILE          PIC 9(02).
               88 IDX-SUCCESS             VALUE 00 97.
               88 IDX-EOF                 VALUE 10.
            03 WS-UID              PIC S9(05) COMP-3.
            03 WS-COUNT.
               04 WS-COUNT-2          PIC 9(02).
               04 WS-COUNT-1          PIC 9(02).
            03 WS-FNAME-F          PIC X(15).
            03 WS-FNAME-T          PIC X(15).
            03 WS-LNAME-F          PIC X(15).
            03 WS-LNAME-T          PIC X(15).
            03 WS-DATA-1           PIC X(09).
            03 WS-DATA-2           PIC X(95).
            03 WS-FLAG             PIC 9(01).
                88 WS-FLAG-INVALID          VALUE 1.
                88 WS-FLAG-NOT-INVALID      VALUE 0.
       LINKAGE SECTION.
         01 WS-SUB-AREA.
              05 WS-SUB-FUNC PIC 9(01).
                 88 WS-FUNC-WRITE     VALUE 1.
                 88 WS-FUNC-UPDATE    VALUE 2.
                 88 WS-FUNC-DELETE    VALUE 3.
                 88 WS-FUNC-READ      VALUE 4.
                 88 WS-FUNC-OTHER     VALUE 0.
              05 WS-SUB-UID       PIC 9(05).
              05 WS-SUB-RC        PIC 9(02).
              05 WS-SUB-DATA      PIC X(140).
       PROCEDURE DIVISION USING WS-SUB-AREA.
       0000-MAIN.
           PERFORM H100-OPEN-FILES.
           PERFORM H200-PROCESS.
           PERFORM H999-PROGRAM-EXIT.
       0000-END. EXIT.

       H100-OPEN-FILES.
           OPEN I-O IDX-FILE.
           IF NOT IDX-SUCCESS
               DISPLAY "I-O DOES NOT OPENED"
               DISPLAY "STATUS CODE: " ST-IDXFILE
               PERFORM H999-PROGRAM-EXIT
           END-IF.
               MOVE SPACES TO WS-DATA-1.
               MOVE SPACES TO WS-DATA-2.
               MOVE SPACES TO WS-FNAME-F.
               MOVE SPACES TO WS-FNAME-T.
               MOVE SPACES TO WS-LNAME-F.
               MOVE SPACES TO WS-LNAME-T.
       H100-END. EXIT.

       H200-PROCESS.
           MOVE WS-SUB-UID TO IDX-UID.
           READ IDX-FILE KEY IDX-KEY
           INVALID KEY
                SET WS-FLAG-INVALID TO TRUE
                IF WS-FUNC-WRITE THEN PERFORM H500-WRITE
                ELSE PERFORM H300-SET-DESC
                END-IF
           NOT INVALID KEY
                SET WS-FLAG-NOT-INVALID TO TRUE
                IF WS-FUNC-UPDATE THEN PERFORM H400-UPDATE
                ELSE
                   PERFORM H300-SET-DESC               
                END-IF
           END-READ.
       H200-END. EXIT.

       H300-SET-DESC.
           IF WS-FUNC-READ
                MOVE '-read-rc:' TO WS-DATA-1
                IF WS-FLAG-INVALID THEN
                    MOVE 'KAYIT BULUNAMADI.' TO WS-DATA-2
                ELSE IF WS-FLAG-NOT-INVALID THEN
                    MOVE 'KAYIT BULUNDU.' TO WS-DATA-2
                END-IF
           ELSE IF WS-FUNC-DELETE 
                MOVE '-delt-rc:' TO WS-DATA-1 
                IF WS-FLAG-INVALID THEN
                    MOVE 'KAYIT SILINEMEDI.' TO WS-DATA-2
                ELSE IF WS-FLAG-NOT-INVALID THEN
                    DELETE IDX-FILE
                    MOVE 'KAYIT SILINDI.' TO WS-DATA-2
                END-IF
           ELSE IF WS-FUNC-UPDATE 
                MOVE '-updt-rc:' TO WS-DATA-1
                IF WS-FLAG-INVALID THEN
                    MOVE 'KAYIT GUNCELLENEMEDI.' TO WS-DATA-2
                ELSE IF WS-FLAG-NOT-INVALID THEN
                    PERFORM H440-UPDT-NOT-INVLD 
                END-IF
           ELSE IF WS-FUNC-WRITE 
                MOVE '-wrte-rc:' TO WS-DATA-1
                IF WS-FLAG-INVALID THEN
                    MOVE 'KAYIT EKLENDI.' TO WS-DATA-2
                ELSE IF WS-FLAG-NOT-INVALID THEN
                    MOVE 'KAYIT EKLENEMEDI.' TO WS-DATA-2
                END-IF
           ELSE
                MOVE '-unkw-rc:' TO WS-DATA-1
                MOVE 'INP OPERATOR BULUNAMADI.' TO WS-DATA-2
           END-IF.
           PERFORM H800-PERFORM-LOG.
       H300-END. EXIT.

       
       H400-UPDATE.
           MOVE 1 TO WS-COUNT-2.
           MOVE 0 TO WS-COUNT-1
           MOVE IDX-NAME TO WS-FNAME-F.
           MOVE IDX-SURNAME TO WS-LNAME-F.
           PERFORM VARYING WS-COUNT-1 FROM 1 BY 1
                UNTIL WS-COUNT-1 > LENGTH OF WS-FNAME-F
                IF WS-FNAME-F (WS-COUNT-1:1) = ' '
                    CONTINUE
                ELSE
                    MOVE WS-FNAME-F (WS-COUNT-1:1) TO
                                        WS-FNAME-T (WS-COUNT-2:1)
                    ADD 1 TO WS-COUNT-2
                END-IF
           END-PERFORM.
           PERFORM H420-SWITCH-LETTERS.
           REWRITE IDX-REC.
           PERFORM H300-SET-DESC.
       H400-END. EXIT.

       H420-SWITCH-LETTERS.
           MOVE WS-LNAME-F TO WS-LNAME-T.
           INSPECT WS-LNAME-T REPLACING ALL 'E' BY 'I'.
           INSPECT WS-LNAME-T REPLACING ALL 'e' BY 'i'.
           INSPECT WS-LNAME-T REPLACING ALL 'A' BY 'E'.
           INSPECT WS-LNAME-T REPLACING ALL 'a' BY 'e'.
           MOVE WS-LNAME-T TO IDX-SURNAME.
           MOVE WS-FNAME-T TO IDX-NAME.
       H420-END. EXIT.

       H440-UPDT-NOT-INVLD.
            STRING 'KAYIT GUNCELLENDI.' DELIMITED BY SIZE
                   WS-FNAME-F DELIMITED BY SIZE
                   '|' DELIMITED BY SIZE
                   WS-FNAME-T DELIMITED BY SIZE
                   '|' DELIMITED BY SIZE
                   WS-LNAME-F DELIMITED BY SIZE
                   '|' DELIMITED BY SIZE
                   WS-LNAME-T DELIMITED BY SIZE
                   INTO WS-DATA-2.
       H440-END. EXIT.
       
       H500-WRITE.
              MOVE 840 TO IDX-DVZ.
              MOVE 'EMIRCAN        ' TO IDX-NAME.
              MOVE 'KAYMAZ         ' TO IDX-SURNAME.
              MOVE 19990313 TO IDX-DATE.
              MOVE ZEROS TO IDX-BALANCE.
              MOVE WS-SUB-UID TO IDX-UID.
              WRITE IDX-REC.
              PERFORM H300-SET-DESC.
       H500-END. EXIT.

       H800-PERFORM-LOG.
           MOVE SPACES TO WS-SUB-DATA.
           STRING WS-SUB-UID DELIMITED BY SIZE
                  WS-DATA-1 DELIMITED BY SIZE
                  ST-IDXFILE DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  WS-DATA-2 DELIMITED BY SIZE
                  INTO WS-SUB-DATA.
       H800-END. EXIT.

       H999-PROGRAM-EXIT.
           CLOSE IDX-FILE.
           GOBACK.
       H999-END. EXIT.
