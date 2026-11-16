
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COURSE-REPORT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "INFILE.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO "OUTFILE.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  INFILE.
       01  IN-REC.
           05 C-CODE             PIC 9.
           05 YR-CODE            PIC 9.
           05 SNO-IN             PIC X(5).
           05 SNAME-IN           PIC X(25).
           05 AVE-GRD            PIC 9V99.

       FD  OUTFILE.
       01  OUT-HDG-1.
           05 FILLER             PIC X(45).
           05 HDG-1-TEXT          PIC X(41).
           05 FILLER             PIC X(46).
       01  OUT-HDG-2.
           05 FILLER             PIC X(57).
           05 HDG-2-TEXT          PIC X(17).
           05 FILLER             PIC X(58).
       01  OUT-BLANK-1           PIC X(132).
       01  OUT-HDG-3.
           05 FILLER             PIC X(44).
           05 HDG-3-TEXT          PIC X(44).
           05 FILLER             PIC X(44).
       01  OUT-BLANK-2           PIC X(132).
       01  OUT-COL-HDG.
           05 FILLER             PIC X(7).
           05 H-C-NAME            PIC X(12).
           05 FILLER              PIC X(17).
           05 H-YR-NAME           PIC X(9).
           05 FILLER              PIC X(14).
           05 H-SNO               PIC X(14).
           05 FILLER              PIC X(12).
           05 H-SNAME             PIC X(25).
           05 FILLER              PIC X(8).
           05 H-REMARKS           PIC X(7).
           05 FILLER              PIC X(7).
       01  OUT-PIC-HDG.
           05 FILLER             PIC X(7).
           05 P-C-NAME            PIC X(12).
           05 FILLER              PIC X(17).
           05 P-YR-NAME           PIC X(9).
           05 FILLER              PIC X(14).
           05 P-SNO               PIC X(14).
           05 FILLER              PIC X(12).
           05 P-SNAME             PIC X(25).
           05 FILLER              PIC X(8).
           05 P-REMARKS           PIC X(7).
           05 FILLER              PIC X(7).
       01  OUT-VAR-HDG.
           05 FILLER             PIC X(7).
           05 V-C-NAME            PIC X(12).
           05 FILLER              PIC X(17).
           05 V-YR-NAME           PIC X(9).
           05 FILLER              PIC X(14).
           05 V-SNO               PIC X(14).
           05 FILLER              PIC X(12).
           05 V-SNAME             PIC X(25).
           05 FILLER              PIC X(8).
           05 V-REMARKS           PIC X(7).
           05 FILLER              PIC X(7).
       01  OUT-STUDENT-REC.
           05 FILLER              PIC X(7).
           05 C-NAME               PIC X(4).
           05 FILLER              PIC X(8).
           05 FILLER              PIC X(17).
           05 YR-NAME              PIC X(9).
           05 FILLER              PIC X(14).
           05 SNO-OUT               PIC X(5).
           05 FILLER              PIC X(9).
           05 FILLER              PIC X(12).
           05 SNAME-OUT             PIC X(25).
           05 FILLER              PIC X(8).
           05 REMARKS               PIC X(6).
           05 FILLER              PIC X(1).
           05 FILLER              PIC X(7).
       01  OUT-COURSE-NAME.
           05 FILLER              PIC X(7).
           05 CF-LABEL            PIC X(13).
           05 CF-NAME             PIC X(45).
           05 FILLER              PIC X(67).
       01  OUT-TOTAL-REC.
           05 FILLER              PIC X(7).
           05 T-LABEL             PIC X(36).
           05 T-COUNT             PIC Z,ZZ9.
           05 FILLER              PIC X(84).
       01  OUT-CCIS-REC.
           05 FILLER              PIC X(7).
           05 CT-LABEL            PIC X(36).
           05 CT-COUNT            PIC ZZ,ZZ9.
           05 FILLER              PIC X(83).
       01  OUT-BLANK-3           PIC X(132).

       WORKING-STORAGE SECTION.
       01  EOF-SW                PIC X VALUE "N".
       01  OLD-C-CODE            PIC 9 VALUE ZERO.

       01  WS-C-NAME             PIC X(4).
       01  WS-YR-NAME            PIC X(9).
       01  COURSE-NAME           PIC X(45).
       01  WS-REMARKS            PIC X(6).

       01  TOTAL-STUDREC         PIC 9(4) VALUE ZERO.
       01  TOTAL-PASSREC         PIC 9(4) VALUE ZERO.
       01  TOTAL-FAILREC         PIC 9(4) VALUE ZERO.

       01  CCIS-STUDREC          PIC 9(5) VALUE ZERO.
       01  CCIS-PASSREC          PIC 9(5) VALUE ZERO.
       01  CCIS-FAILREC          PIC 9(5) VALUE ZERO.

       PROCEDURE DIVISION.

       MAIN-RTN.
           PERFORM INIT-RTN
           PERFORM PROC-RTN UNTIL EOF-SW = "Y"
           PERFORM FIN-RTN
           STOP RUN.

       INIT-RTN.
           OPEN INPUT INFILE
           OPEN OUTPUT OUTFILE

           PERFORM HDG-RTN

           READ INFILE
               AT END
                   MOVE "Y" TO EOF-SW
           END-READ

           IF EOF-SW NOT = "Y"
               MOVE C-CODE TO OLD-C-CODE
               PERFORM SUB-HDG-RTN
           END-IF.

       HDG-RTN.
           MOVE SPACES TO OUT-HDG-1
           MOVE "POLYTECHNIC UNIVERSITY OF THE PHILIPPINES"
             TO HDG-1-TEXT
           WRITE OUT-HDG-1

           MOVE SPACES TO OUT-HDG-2
           MOVE "STA. MESA, MANILA" TO HDG-2-TEXT
           WRITE OUT-HDG-2

           MOVE SPACES TO OUT-BLANK-1
           WRITE OUT-BLANK-1

           MOVE SPACES TO OUT-HDG-3
           MOVE "COLLEGE OF COMPUTER AND INFORMATION SCIENCES"
             TO HDG-3-TEXT
           WRITE OUT-HDG-3

           MOVE SPACES TO OUT-BLANK-2
           WRITE OUT-BLANK-2

           MOVE SPACES TO OUT-COL-HDG
           MOVE "Course Name" TO H-C-NAME
           MOVE "Year Name" TO H-YR-NAME
           MOVE "Student Number" TO H-SNO
           MOVE "Student Name" TO H-SNAME
           MOVE "Remarks" TO H-REMARKS
           WRITE OUT-COL-HDG

           MOVE SPACES TO OUT-PIC-HDG
           MOVE "X(4)" TO P-C-NAME
           MOVE "X(9)" TO P-YR-NAME
           MOVE "X(5)" TO P-SNO
           MOVE "X(25)" TO P-SNAME
           MOVE "X(6)" TO P-REMARKS
           WRITE OUT-PIC-HDG

           MOVE SPACES TO OUT-VAR-HDG
           MOVE "(C-NAME)" TO V-C-NAME
           MOVE "(YR-NAME)" TO V-YR-NAME
           MOVE "(SNO-OUT)" TO V-SNO
           MOVE "(SNAME-OUT)" TO V-SNAME
           MOVE "(REMARKS)" TO V-REMARKS
           WRITE OUT-VAR-HDG

           MOVE SPACES TO OUT-BLANK-3
           WRITE OUT-BLANK-3.

       SUB-HDG-RTN.
           EVALUATE TRUE
               WHEN C-CODE = 1
                   MOVE "BSIT" TO WS-C-NAME
                   MOVE
                     "Bachelor of Science in Information Technology"
                     TO COURSE-NAME
               WHEN C-CODE = 2
                   MOVE "BSCS" TO WS-C-NAME
                   MOVE
                     "Bachelor of Science in Computer Science"
                     TO COURSE-NAME
           END-EVALUATE.

       PROC-RTN.
           IF C-CODE NOT = OLD-C-CODE
               PERFORM BREAK-RTN
           END-IF

           EVALUATE YR-CODE
               WHEN 1
                   MOVE "Freshman" TO WS-YR-NAME
               WHEN 2
                   MOVE "Sophomore" TO WS-YR-NAME
               WHEN 3
                   MOVE "Junior" TO WS-YR-NAME
               WHEN 4
                   MOVE "Senior" TO WS-YR-NAME
           END-EVALUATE

           IF AVE-GRD < 3.01
               MOVE "PASSED" TO WS-REMARKS
               ADD 1 TO TOTAL-PASSREC
               ADD 1 TO CCIS-PASSREC
           ELSE
               MOVE "FAILED" TO WS-REMARKS
               ADD 1 TO TOTAL-FAILREC
               ADD 1 TO CCIS-FAILREC
           END-IF

           ADD 1 TO TOTAL-STUDREC
           ADD 1 TO CCIS-STUDREC

           MOVE SPACES TO OUT-STUDENT-REC
           MOVE WS-C-NAME TO C-NAME
           MOVE WS-YR-NAME TO YR-NAME
           MOVE SNO-IN TO SNO-OUT
           MOVE SNAME-IN TO SNAME-OUT
           MOVE WS-REMARKS TO REMARKS
           WRITE OUT-STUDENT-REC

           READ INFILE
               AT END
                   MOVE "Y" TO EOF-SW
           END-READ.

       BREAK-RTN.
           PERFORM FOOTER-RTN

           MOVE ZERO TO TOTAL-STUDREC
                        TOTAL-PASSREC
                        TOTAL-FAILREC

           MOVE C-CODE TO OLD-C-CODE
           PERFORM SUB-HDG-RTN.

       FOOTER-RTN.
           MOVE SPACES TO OUT-BLANK-3
           WRITE OUT-BLANK-3

           MOVE SPACES TO OUT-COURSE-NAME
           MOVE "COURSE NAME:" TO CF-LABEL
           MOVE COURSE-NAME TO CF-NAME
           WRITE OUT-COURSE-NAME

           MOVE SPACES TO OUT-TOTAL-REC
           MOVE "TOTAL STUDENTS:" TO T-LABEL
           MOVE TOTAL-STUDREC TO T-COUNT
           WRITE OUT-TOTAL-REC

           MOVE SPACES TO OUT-TOTAL-REC
           MOVE "TOTAL PASSED:" TO T-LABEL
           MOVE TOTAL-PASSREC TO T-COUNT
           WRITE OUT-TOTAL-REC

           MOVE SPACES TO OUT-TOTAL-REC
           MOVE "TOTAL FAILED:" TO T-LABEL
           MOVE TOTAL-FAILREC TO T-COUNT
           WRITE OUT-TOTAL-REC

           MOVE SPACES TO OUT-BLANK-3
           WRITE OUT-BLANK-3.

       FIN-RTN.
           IF TOTAL-STUDREC > ZERO
               PERFORM FOOTER-RTN
           END-IF

           MOVE SPACES TO OUT-BLANK-3
           WRITE OUT-BLANK-3

           MOVE SPACES TO OUT-CCIS-REC
           MOVE "TOTAL CCIS STUDENTS:" TO CT-LABEL
           MOVE CCIS-STUDREC TO CT-COUNT
           WRITE OUT-CCIS-REC

           MOVE SPACES TO OUT-CCIS-REC
           MOVE "TOTAL CCIS PASSED:" TO CT-LABEL
           MOVE CCIS-PASSREC TO CT-COUNT
           WRITE OUT-CCIS-REC

           MOVE SPACES TO OUT-CCIS-REC
           MOVE "TOTAL CCIS FAILED:" TO CT-LABEL
           MOVE CCIS-FAILREC TO CT-COUNT
           WRITE OUT-CCIS-REC

           CLOSE INFILE OUTFILE.
