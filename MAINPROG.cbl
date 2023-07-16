       IDENTIFICATION DIVISION.

       PROGRAM-ID.    MAINPROG.
       AUTHOR.        EMIRCAN KAYMAZ.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUT-FILE   ASSIGN OUTFILE
                             STATUS OUT-ST.
           SELECT INP-FILE   ASSIGN INPFILE
                             STATUS INP-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  OUT-FILE RECORDING MODE F.
       01  OUT-REC.
           05 OUT-FINAL      PIC X(140).
       FD  INP-FILE RECORDING MODE F.
       01  INP-REC.
           05 INP-OPRT       PIC X(01).
           05 INP-UID        PIC 9(05).

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05 WS-SUBPROG     PIC X(7) VALUE 'SUBPROG'.
           05 OUT-ST         PIC 9(2).
              88 OUT-SUCCESS          VALUE 00.
           05 INP-ST         PIC 9(2).
              88 INP-EOF              VALUE 10.
              88 INP-SUCCESS          VALUE 00.
           05 WS-OPT-TYPE    PIC 9(1).
              88 OPT-VALID            VALUE 1 THRU 4.
           05 WS-SUB-AREA.
              07 WS-SUB-FUNC PIC 9(1).
                 88 WS-FUNC-WRITE     VALUE 1.
                 88 WS-FUNC-UPDATE    VALUE 2.
                 88 WS-FUNC-DELETE    VALUE 3.
                 88 WS-FUNC-READ      VALUE 4.
                 88 WS-FUNC-OTHER     VALUE 0.
              07 WS-SUB-UID  PIC 9(5).
              07 WS-SUB-RC   PIC 9(2).
              07 WS-SUB-DATA PIC X(140).
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM H100-OPEN-FILES
           PERFORM H200-PROCESS UNTIL INP-EOF
           PERFORM H999-PROGRAM-EXIT.
       0000-END. EXIT.

       H100-OPEN-FILES.
           OPEN INPUT INP-FILE
           OPEN OUTPUT OUT-FILE.
           READ INP-FILE.
           IF NOT INP-SUCCESS
               DISPLAY 'INPUT DOES NOT OPENED'
               PERFORM H999-PROGRAM-EXIT
           END-IF.
           IF NOT OUT-SUCCESS
               DISPLAY 'OUTFILE DOES NOT OPENED'
               PERFORM H999-PROGRAM-EXIT
           END-IF.
       H100-END. EXIT.

       H200-PROCESS.
           EVALUATE INP-OPRT
                WHEN 'W'   SET WS-FUNC-WRITE TO TRUE
                WHEN 'U'   SET WS-FUNC-UPDATE TO TRUE
                WHEN 'D'   SET WS-FUNC-DELETE TO TRUE
                WHEN 'R'   SET WS-FUNC-READ TO TRUE
                WHEN OTHER SET WS-FUNC-OTHER TO TRUE
           END-EVALUATE.
           MOVE WS-SUB-FUNC TO WS-OPT-TYPE.
           PERFORM H220-CHECK-VALID.
           READ INP-FILE.
       H200-END. EXIT.

       H220-CHECK-VALID.
           MOVE INP-UID TO WS-SUB-UID
           MOVE ZEROS   TO WS-SUB-RC
           MOVE SPACES TO WS-SUB-DATA
           MOVE SPACES TO OUT-FINAL
           CALL WS-SUBPROG USING WS-SUB-AREA
           MOVE WS-SUB-DATA TO OUT-FINAL 
           WRITE OUT-REC
           IF NOT OPT-VALID
               DISPLAY INP-OPRT ' IS WRONG OPERATION'
           END-IF.
       H220-END. EXIT.

       H999-PROGRAM-EXIT.
           CLOSE INP-FILE.
           CLOSE OUT-FILE.
           DISPLAY 'PROGRAM IS EXIT'
           STOP RUN.
       H999-END. EXIT.
