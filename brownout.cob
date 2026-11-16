       IDENTIFICATION DIVISION.
       PROGRAM-ID. BROWNOUT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 HDG-1.
           05 FILLER PIC X(20) VALUE SPACES.
           05 FILLER PIC X(27)
              VALUE "BROWNOUT ELECTRIC COMPANY".
           05 FILLER PIC X(20) VALUE SPACES.

       01 HDG-2.
           05 FILLER PIC X(26) VALUE SPACES.
           05 FILLER PIC X(23)
              VALUE "BILLING REPORT".
           05 FILLER PIC X(26) VALUE SPACES.

       01 ACCOUNT-NO PIC X(10).
       01 CUSTOMER-NAME PIC X(25).
       01 PREVIOUS-READING PIC 9(6).
       01 CURRENT-READING PIC 9(6).
       01 ACCOUNT-CODE PIC A.
       01 AREA-CODE PIC 9.

       PROCEDURE DIVISION.
           DISPLAY HDG-1.
           DISPLAY HDG-2.
           DISPLAY " ".
           DISPLAY "Account Number: ".
           DISPLAY "Customer Name: ".
           DISPLAY "Previous Reading: ".
           DISPLAY "Current Reading: ".
           DISPLAY "Account Code: ".
           DISPLAY "Area Code: ".
           DISPLAY " ".
           DISPLAY "                      Input Another Record (Y/N)? ".

           STOP RUN.
