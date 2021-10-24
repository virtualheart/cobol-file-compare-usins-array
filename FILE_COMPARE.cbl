       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPARE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE1 ASSIGN TO
           "/home/virtual/Desktop/file.txt"
           ACCESS        IS SEQUENTIAL
           ORGANIZATION  IS SEQUENTIAL
           FILE STATUS   IS WS-FS1.
      *
           SELECT INFILE2 ASSIGN TO
           "/home/virtual/Desktop/file1.txt"
           ACCESS        IS SEQUENTIAL
           ORGANIZATION  IS SEQUENTIAL
           FILE STATUS   IS WS-FS2.
      *
           SELECT OUTFILE1 ASSIGN TO
           "/home/virtual/Desktop/out1.txt"
           FILE STATUS   IS WS-FS3.
      *
           SELECT OUTFILE2 ASSIGN TO
           "/home/virtual/Desktop/out2.txt"
           FILE STATUS   IS WS-FS4.
      *
           SELECT OUTFILE3 ASSIGN TO
           "/home/virtual/Desktop/out3.txt"
           FILE STATUS   IS WS-FS5.
      *
           SELECT OUTFILE4 ASSIGN TO
           "/home/virtual/Desktop/out4.txt"
           FILE STATUS   IS WS-FS6.

       DATA DIVISION.
       FILE SECTION.
       FD INFILE1.
       01 FS-INFILE1.
          02  TI001-ENAME      PIC X(5).
          02  FILLER           PIC X(75).
      *
       FD INFILE2.
       01 FS-INFILE2.
          02  TI002-ENAME      PIC X(5).
          02  FILLER           PIC X(75).
      *
       FD OUTFILE1.
       01 FS-OUTFILE1.
          02  TO001-ENAME      PIC X(5).
          02  FILLER           PIC X(75).
      *
       FD OUTFILE2.
       01 FS-OUTFILE2.
          02  TO002-ENAME      PIC X(5).
          02  FILLER           PIC X(75).
      *
       FD OUTFILE3.
       01 FS-OUTFILE3.
          02  TO003-ENAME      PIC X(5).
          02  FILLER           PIC X(75).
      *
       FD OUTFILE4.
       01 FS-OUTFILE4.
          02  TO004-ENAME      PIC X(5).
          02  FILLER           PIC X(75).
      *
       WORKING-STORAGE SECTION.
       01 WS-FS1               PIC 9(2).
       01 WS-FS2               PIC 9(2).
       01 WS-FS3               PIC 9(2).
       01 WS-FS4               PIC 9(2).
       01 WS-FS5               PIC 9(2).
       01 WS-FS6               PIC 9(2).
      *
       01 WS-I                 PIC 9(2).
       01 WS-J                 PIC 9(2).
      *
       01 WS-K                 PIC 9(2).
       01 WS-L                 PIC 9(2).
       01 WS-M                 PIC 9(2).
      *
       01 TABLE1.
         04 ARR-INFILE1 OCCURS 1 TO 100 TIMES DEPENDING ON WS-I.
           08 AR001-ENAME          PIC X(5).
           08 FILLER               PIC X(75).
      *
       01 TABLE2.
         04 ARR-INFILE2 OCCURS 1 TO 100 TIMES DEPENDING ON WS-J.
             08 AR002-ENAME        PIC X(5).
             08 FILLER             PIC X.
             08 AR002-FLAG         PIC X.
             08 FILLER             PIC X(73).
      *
       PROCEDURE DIVISION.
       0000-MAIN-PARA.
           PERFORM 1000-INTIAL-PARA THRU
                   1000-INTIAL-PARA-EXIT
           PERFORM 2000-PROCESS-PARA THRU
                   2000-PROCESS-PARA-EXIT
           PERFORM 9000-TERM-PARA THRU
                   9000-TERM-PARA-EXIT.
       0000-MAIN-PARA-EXIT.
           EXIT.
      *
       1000-INTIAL-PARA.
           INITIALIZE WS-FS1 WS-FS2 WS-FS3 WS-FS4 WS-FS5.
       1000-INTIAL-PARA-EXIT.
           EXIT.
      ******************************************************************
       2000-PROCESS-PARA.
           PERFORM 2100-OPEN-PARA THRU
                   2100-OPEN-PARA-EXIT
           PERFORM 2200-READ-PARA THRU
                   2200-READ-PARA-EXIT UNTIL WS-FS1 = 10
                                         AND WS-FS2 = 10
           PERFORM 4300-CLOSE-PARA THRU
                   4300-CLOSE-PARA-EXIT.
      *
       2000-PROCESS-PARA-EXIT.
           EXIT.
      ******************************************************************
      *    OPEN ALL INPUT,OUTPUT FILE 
       2100-OPEN-PARA.
           OPEN INPUT INFILE1

           OPEN INPUT INFILE2

           OPEN OUTPUT OUTFILE1

           OPEN OUTPUT OUTFILE2

           OPEN OUTPUT OUTFILE3

           OPEN OUTPUT OUTFILE4.
      *
       2100-OPEN-PARA-EXIT.
           EXIT.
      ******************************************************************
       2200-READ-PARA.
      *    INFILE DATA MOVED TO ARRAY 1
           IF WS-FS1 NOT = 10
             READ INFILE1
             NOT AT END
              ADD 1 TO WS-I
              
              MOVE FS-INFILE1 TO ARR-INFILE1(WS-I)

             END-READ
           END-IF
      *    
      *    INFILE DATA MOVED TO ARRAY 2
           IF WS-FS2 NOT = 10
             READ INFILE2
             NOT AT END
               ADD 1 TO WS-J
               
               MOVE FS-INFILE2 TO ARR-INFILE2(WS-J)

             END-READ
           END-IF
      *    
      *    ARRAY LOADED FINISHED, PERFORM VALIDACTION
           IF WS-FS1 = 10 AND WS-FS2 = 10
              PERFORM 4400-VALID-PARA THRU
                      4400-VALID-PARA-EXIT
           END-IF.
      *
       2200-READ-PARA-EXIT.
           EXIT.
      ******************************************************************
      *    CLOSE ALL INPUT,OUTPUT FILE 
       4300-CLOSE-PARA.
           CLOSE INFILE1
                 INFILE2
                 OUTFILE1
                 OUTFILE2
                 OUTFILE3 
                 OUTFILE4.

       4300-CLOSE-PARA-EXIT.
           EXIT.

       4400-VALID-PARA.
      *    LOOP START 
      ******************************************************************
           PERFORM UNTIL WS-K = WS-I
      ******************************************************************
             ADD 1 TO WS-K
             MOVE 0 TO WS-L
      *    NESTED LOOP START
             PERFORM UNTIL WS-L = WS-J
                 ADD 1 TO WS-L
                 EVALUATE TRUE
                 WHEN ARR-INFILE1(WS-K) = ARR-INFILE2(WS-L)
                                  
      *    FLAG ADDED ARRAY2 FOR OUT 4         
                     MOVE 'A' TO AR002-FLAG(WS-L)  
                         
      *    WRITE OUT 1 COMMAN RECORD 
                     MOVE ARR-INFILE1(WS-K) TO FS-OUTFILE1
                     WRITE FS-OUTFILE1
                     
      *    MATCH FINDED TERMINATE THE NESTED LOOP
                     GO TO 4400-VALID-PARA
                 END-EVALUATE
             END-PERFORM
      ******************************************************************
      *    MATCH NOT FOUND WRITE OUT 2 AND 3 
             EVALUATE TRUE
               WHEN ARR-INFILE1(WS-K) NOT = ARR-INFILE2(WS-L)
                MOVE ARR-INFILE1(WS-K) TO FS-OUTFILE3
                MOVE ARR-INFILE1(WS-K) TO FS-OUTFILE2
                WRITE FS-OUTFILE3
                WRITE FS-OUTFILE2
             END-EVALUATE
           END-PERFORM
       
      *    NESTED LOOP 2 START CHECK ARRAY 2 NOT EQUAL *"A"*
           PERFORM UNTIL WS-M = WS-J
            ADD 1 TO WS-M
            IF ARR-INFILE2(WS-M)(7:1) NOT = "A"
            MOVE ARR-INFILE2(WS-M) TO FS-OUTFILE2
            MOVE ARR-INFILE2(WS-M) TO FS-OUTFILE4
            WRITE FS-OUTFILE2
            WRITE FS-OUTFILE4

            END-IF
           END-PERFORM.
      *
       4400-VALID-PARA-EXIT.
           EXIT.
      ******************************************************************
       9000-TERM-PARA.
           STOP RUN.
       9000-TERM-PARA-EXIT.
           EXIT.
      ******************************************************************
       END PROGRAM JARR.
