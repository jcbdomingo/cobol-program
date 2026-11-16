       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAHIRAP.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 HDG-1.
           05 FILLER PIC X(20) VALUE SPACES.
           05 FILLER PIC X(27)
              VALUE "Pahirap Credit Card Company".
           05 FILLER PIC X(20) VALUE SPACES.

       01 HDG-2.
           05 FILLER PIC X(21) VALUE SPACES.
           05 FILLER PIC X(27)
              VALUE "Monthly Statement Report".
           05 FILLER PIC X(20) VALUE SPACES.

       01 CARD-NO PIC X(10).
       01 ACCOUNT-NAME PIC X(25).
       01 CREDIT-CODE PIC 9.
       01 PREVIOUS-BALANCE PIC 9(5)V99.
       01 DDATE PIC 9(6).
       01 TRANSACTION-CODE PIC 9.    
       01 AMOUNT PIC 9(5)V99.
       01 CHOICE PIC X VALUE "Y".  

       SCREEN SECTION.
       01 SKRIN.
           05 BLANK SCREEN.
       

       PROCEDURE DIVISION. 
      *    DISPLAY SKRIN. 
           DISPLAY HDG-1.
           DISPLAY HDG-2.
           DISPLAY " ".
           DISPLAY "Card Number: ".
           DISPLAY "Account Name: ".
           DISPLAY "Credit Code: ".
           DISPLAY "Previous Balance: ".
           DISPLAY "Date: ".
           DISPLAY "Transaction Code: ".
           DISPLAY "Amount: ".
           DISPLAY " ".
           DISPLAY "                 Input Another Transaction (Y/N)? "
           WITH NO ADVANCING. 
           ACCEPT CHOICE. 
           DISPLAY " ".

           IF CHOICE = "Y" OR CHOICE = "y"
           PERFORM UNTIL CHOICE = "N" OR CHOICE = "n"
           DISPLAY "Date: "
           DISPLAY "Transaction Code: "
           DISPLAY "Amount: "
           DISPLAY " "
           DISPLAY "                 Input Another Transaction (Y/N)? "
           WITH NO ADVANCING
           ACCEPT CHOICE
           END-PERFORM.

           STOP RUN.
