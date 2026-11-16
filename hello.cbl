      **IDENTIFICATION DIVISION. 
      **PROGRAM-ID. ELEC. 
      **
      **DATA DIVISION. 
      **WORKING-STORAGE SECTION. 
      **01 HDG-1.           
      **    05 FILLER PIC X(20) VALUE SPACES.
      **    05 FILLER PIC X(27) VALUE "BROWNOUT ELECTRIC COMPANY". 
      **    05 FILLER PIC X(20) VALUE SPACES. 
      **01 HDG-2. 
      **    05 FILLER PIC X(26) VALUE SPACES.
      **    05 FILLER PIC X(23) VALUE "BILLING REPORT". 
      **    05 FILLER PIC X(26) VALUE SPACES. 
      **
      **01 ACCOUNT-NO PIC X(10) VALUE ZEROS. 
      **01 CUSTOMER-NAME PIC X(25) VALUE "".
      **01 PREVIOUS-READING PIC 9(6) VALUE ZEROS. 
      **01 CURRENT-READING PIC 9(6) VALUE ZEROS. 
      **01 ACCOUNT-CODE PIC A. 
      **01 AREA-CODE PIC 9. 
      **01 CHOICE PIC X VALUE "Y". 
      **
      **
      **
      **PROCEDURE DIVISION. 
      ***    DISPLAY SPACE WITH BLANK SCREEN. 
      **    DISPLAY HDG-1. 
      **    DISPLAY HDG-2.  
      **
      **    PERFORM UNTIL CHOICE = "N" OR CHOICE = "n"
      **    DISPLAY "Enter Account Number: " WITH NO ADVANCING
      **    ACCEPT ACCOUNT-NO
      **    DISPLAY "Enter Customer Name: " WITH NO ADVANCING
      **    ACCEPT CUSTOMER-NAME
      **    DISPLAY "Enter Previous Reading: " WITH NO ADVANCING
      **    ACCEPT PREVIOUS-READING
      **    DISPLAY "Enter Current Reading: " WITH NO ADVANCING
      **    ACCEPT CURRENT-READING 
      **    DISPLAY "Enter Account Code: " WITH NO ADVANCING
      **    ACCEPT ACCOUNT-CODE
      **    DISPLAY "Enter Area Code: " WITH NO ADVANCING
      **    ACCEPT AREA-CODE
      **    DISPLAY "Input Another Record (Y/N)? " WITH NO ADVANCING
      **    ACCEPT CHOICE
      **    END-PERFORM. 
      **
      **    STOP RUN. 
      *
      *
      *IDENTIFICATION DIVISION.
      *PROGRAM-ID. CREDIT. 
      *
      *DATA DIVISION. 
      *WORKING-STORAGE SECTION. 
      *01 HDG-1.           
      *    05 FILLER PIC X(20) VALUE SPACES.
      *    05 FILLER PIC X(27) VALUE "Pahirap Credit Card Company". 
      *    05 FILLER PIC X(20) VALUE SPACES. 
      *01 HDG-2. 
      *    05 FILLER PIC X(21) VALUE SPACES.
      *    05 FILLER PIC X(27) VALUE "Monthly Statement Report". 
      *    05 FILLER PIC X(20) VALUE SPACES.
      *
      *01 CARD-NO PIC X(10) VALUE ZEROS.
      *01 ACCOUNT-NAME PIC X(25) VALUE "".
      *01 CREDIT-CODE PIC 9. 
      *01 PREVIOUS-BALANCE PIC 9(5)V99 VALUE ZEROS. 
      *01 DDATE PIC 9(6) VALUE ZEROS. 
      *01 TRANSACTION-CODE PIC 9. 
      *01 AMOUNT PIC 9(5)V99 VALUE ZEROS.
      *01 CHOICE PIC X VALUE "Y".  
      *
      *
      *
      *PROCEDURE DIVISION.  
      *    DISPLAY HDG-1. 
      *    DISPLAY HDG-2. 
      *    
      *    DISPLAY "Card Number: " WITH NO ADVANCING.
      *    ACCEPT CARD-NO.
      *    DISPLAY "Account Name: " WITH NO ADVANCING. 
      *    ACCEPT ACCOUNT-NAME.
      *    DISPLAY "Credit Code: " WITH NO ADVANCING.
      *    ACCEPT CREDIT-CODE.
      *    DISPLAY "Previous Balance: " WITH NO ADVANCING.
      *    ACCEPT PREVIOUS-BALANCE.  
      *    DISPLAY "Date: " WITH NO ADVANCING.
      *    ACCEPT DDATE.
      *    DISPLAY "Transaction Code: " WITH NO ADVANCING.
      *    ACCEPT TRANSACTION-CODE.
      *    DISPLAY "Amount: " WITH NO ADVANCING.
      *    ACCEPT AMOUNT.
      *    DISPLAY "Input Another Record (Y/N)? " WITH NO ADVANCING.
      *    ACCEPT CHOICE. 
      *    
      *    IF CHOICE = "Y" OR CHOICE = "y"
      *    PERFORM UNTIL CHOICE = "N" OR CHOICE = "n"
      *      DISPLAY "Date: " WITH NO ADVANCING
      *      ACCEPT DDATE
      *      DISPLAY "Transaction Code: " WITH NO ADVANCING
      *      ACCEPT TRANSACTION-CODE
      *      DISPLAY "Amount: " WITH NO ADVANCING
      *      ACCEPT AMOUNT
      *      DISPLAY "Input Another Record (Y/N)? " WITH NO ADVANCING
      *      ACCEPT CHOICE
      *    END-PERFORM.
      *
      *    STOP RUN.
       
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. DOMINGO. 
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "DOMINGO.TXT"
                ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION. 
       FILE SECTION. 
       FD STUDENT-FILE.
       01 STUDENT-FILE. 
       01 STUDENT-REC. 
           05 REC-STUDENT-NO PIC X(10).
           05 FILLER PIC X(2).
           05 REC-STUDENT-NAME PIC X(25). 

       WORKING-STORAGE SECTION.
       01 HDG-1 
           05 FILLER PIC X(12) VALUE SPACES. 
           05 FILLER PIC X(41) 
               VALUE "POLYTECHNIC UNIVERSITY OF THE PHILIPPINES.".
           05 FILLER PIC X(12) VALUE SPACES. 
       01 HDG-2 
           05 FILLER PIC X(22) VALUE SPACES. 
           05 FILLER PIC X(20) VALUE "STUDENT RECORD".
           05 FILLER PIC X(23) VALUE SPACES. 

       01 STUDENT-NO PIC X(10) VALUE SPACES.
       01 STUDENT-NAME PIC X(25) VALUE SPACES. 
       01 REC-COUNT PIC 99 VALUE SPACES. 
       01 CHOICE PIC X VALUE "Y". 

      *PROCEDURE DIVISON. 
      *MAIN-RTN.
      *    PERFORM INIT-RTN.
      *    PERFORM PROCESS-RTN UNTIL CHOICE = "N" OR CHOICE ="n".
      *    PERFORM FINAL RTN. 
      *    STOP RUN. 
      *
      *INIT-RTN. 
      *    OPEN OUTPUT STUDENT-FILE. 
      *    DISPLAY HDG-1.
      *    DISPLAY HDG-2. 
      *    DISPLAY " ". 
      *    DISPLAY "Please enter at least 5 records. ". 
      *    DISPLAY " ". 

      *PROCESS-RTN. 
      *    DISPLAY "Enter Student Number: " WITH NO ADVANCING. 
      *    ACCEPT STUDENT-NO.
      *    DISPLAY "Enter Student Name: " WITH NO ADVANCING. 
      *    ACCEPT STUDENT-NAME. 

      *    MOVE SPACES TO STUDENT-REC. 
      *    MOVE STUDENT-NO TO REC-STUDENT-NO.
      *    MOVE STUDENT-NAME TO REC-STUDENT-NAME. 
      *    WRITE STUDENT-REC.
      *    ADD 1 TO REC-COUNT. 

      *    IF REC-COUNT < 5
      *        MOVE "Y" TO CHOICE
      *        DISPLAY "Record Saved. Minimun of 5 records required.". 
      *    ELSE
      *        DISPLAY "Input Another Record (Y/N)?:".WITH NO ADVANCING.
      *        ACCEPT CHOICE. 
      *    END-IF.
      *    DISPLAY " ".

      *FINAL-RTN.
      *    CLOSE STUDENT-FILE.
      *    DISPLAY "Total Records Saved: " REC-COUNT. 
      *    DISPLAY "Recors were written to DOMINGO.TXT ".
 
      *
       IDENTIFICATION DIVISON. 
       PROGRAM-ID. TRIAL.

       ENVIRONMENT DIVISON. 
       INPUT-OUTPUT SECTION. 
       FILE CONTROL. 
       SELECT INFILE ASSIGN TO "IMPUT.TXT"
       
        ORGANIZATION IS LINE SEQUENTIAL. 

       DATA DIVISION. 
       FD INFILE.
       01 INREC.
           05 STUDENT-NAME.
           05 STUD-NUM.

       WORKING-STORAGE SECTION. 
       01 EOFSW PIC 9 VALUE ZERO. 

       PROCEDURE DIVISON. 
       
  