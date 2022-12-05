      ******************************************************************
      * Author:Todd Abraham
      * Date: 10-3-2022
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIDTERM.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
               SELECT INPUT-FILE ASSIGN TO "countries.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.

               SELECT OUTPUT-FILE ASSIGN TO "midterm.rpt"
                   ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
           FILE SECTION.
           FD INPUT-FILE.
      *Reads in the files and stores in the correct pic clause
           01 INPUT-RECORD.
               05 IN-COUNTRY           PIC X(25).
               05 IN-COUNTRY-CODE      PIC XX.
               05 IN-SLUG              PIC X(18).
               05 IN-NEW-CONFIRMED     PIC 9(5).
               05 IN-TOTAL-CONFIRMED   PIC 9(6).
               05 IN-NEW-DEATHS        PIC 9(5).
               05 IN-TOTAL-DEATHS      PIC 9(5).
               05 IN-NEW-RECOVERED     PIC 9(5).
               05 IN-TOTAL-RECOVERED   PIC 9(6).
               05 IN-DATE.
                   10 IN-YEAR          PIC 9(4).
                   10 IN-DASH1         PIC X.
                   10 IN-MONTH         PIC 99.
                   10 IN-DASH2         PIC X.
                   10 IN-DAY           PIC 99.
              05 IN-TIME.
                  10                   PIC X.
                  10 IN-TIME-ALL       PIC X(8).
                  10                   PIC X.

           FD OUTPUT-FILE.
           01 OUTPUT-RECORD PIC X(100).

       WORKING-STORAGE SECTION.
      *Values for the format of the header
       01 END-OF-FILE                  PIC XXX VALUE "NO".

       01 DASHED-LINE.
           05 FILLER                   PIC X(100) VALUES ALL '*'.

       01 DATE-LINE.
           05                          PIC X(6) VALUE 'DATE: '.
           05 OUT-DATE.
               10 OUT-YEAR                 PIC 9(4).
               10 OUT-DASH1                PIC X.
               10 OUT-MONTH                PIC 99.
               10 OUT-DASH2                PIC X.
               10 OUT-DAY                  PIC 99.

       01 TIME-LINE.
           05                          PIC X(6) VALUE 'TIME: '.
           05 OUT-TIME-ALL             PIC X(8).

       01 COUNTRY-LINE.
           05                          PIC X(9) VALUE 'COUNTRY: '.
           05 OUT-COUNTRY              PIC X(25).

       01 COUNTRY-CODE-LINE.
           05                          PIC X(14) VALUE
           'COUNTRY CODE: '.
           05 OUT-COUNTRY-CODE         PIC XX.

       01 SLUG-LINE.
           05                          PIC X(6) VALUE 'SLUG: '.
           05 OUT-SLUG                 PIC X(18).

       01 NEW-CONF-LINE.
           05                          PIC X(21) VALUE
           'NEW CONFIRMED CASES: '.
           05 OUT-NEW-CONF-CASES       PIC 9(5).

       01 TOTAL-CONF-LINE.
           05                          PIC X(23) VALUE
           'TOTAL CONFIRMED CASES: '.
           05 OUT-TOTAL-CONF-CASES     PIC 9(5).

       01 NEW-DEATHS-LINE.
           05                          PIC X(12) VALUE
           'NEW DEATHS: '.
           05 OUT-NEW-DTHS             PIC 9(5).

       01  TOTAL-DEATHS-LINE.
           05                          PIC X(7) VALUE 'TOTAL: '.
           05 OUT-TOTAL-DTHS           PIC 9(5).

       01 NEW-REC-LINE.
           05                          PIC X(16) VALUE
           'NEW RECOVERIES: '.
           05 OUT-NEW-REC              PIC 9(5).

       01 TOTAL-REC-LINE.
           05                          PIC X(18) VALUE
           'TOTAL RECOVERIES: '.
           05 OUT-TOTAL-REC            PIC 9(6).

       PROCEDURE DIVISION.
       100-MAIN-PROCEDURE.
      *Runs the program
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.

           PERFORM 200-READ-RECORD
           UNTIL END-OF-FILE = "YES".

           MOVE DASHED-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD AFTER ADVANCING 1 LINE.

           CLOSE INPUT-FILE
               OUTPUT-FILE.
           STOP RUN.

       200-READ-RECORD.
      *Reads the record and stores the values in the correct spots
           READ INPUT-FILE
           AT END MOVE "YES" TO END-OF-FILE
           NOT AT END PERFORM 300-WRITE-FILE.


       300-WRITE-FILE.
      *Header dashed line
           MOVE DASHED-LINE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD AFTER ADVANCING 1 LINE.

      *Date value moved and wrote
           MOVE IN-DATE TO OUT-DATE.
           MOVE DATE-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD AFTER ADVANCING 1 LINE.

      *Time value moved and wrote
           MOVE IN-TIME-ALL TO OUT-TIME-ALL.
           MOVE TIME-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD AFTER ADVANCING 1 LINE.

      *Country value moved and wrote
           MOVE IN-COUNTRY TO OUT-COUNTRY.
           MOVE COUNTRY-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD AFTER ADVANCING 1 LINE.

      *Country code value moved and wrote
           MOVE IN-COUNTRY-CODE TO OUT-COUNTRY-CODE.
           MOVE COUNTRY-CODE-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD AFTER ADVANCING 1 LINE.

      *Slug value moved and wrote
           MOVE IN-SLUG TO OUT-SLUG.
           MOVE SLUG-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD AFTER ADVANCING 1 LINE.

      *New confirmed case value moved and wrote
           MOVE IN-NEW-CONFIRMED TO OUT-NEW-CONF-CASES.
           MOVE NEW-CONF-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD AFTER ADVANCING 1 LINE.

      *New total cases value moved and wrote
           MOVE IN-TOTAL-CONFIRMED TO OUT-TOTAL-CONF-CASES.
           MOVE TOTAL-CONF-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD AFTER ADVANCING 1 LINE.

      *New deaths value moved and wrote
           MOVE IN-NEW-DEATHS TO OUT-NEW-DTHS.
           MOVE NEW-DEATHS-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD AFTER ADVANCING 1 LINE.

      *Total deaths value moved and wrote
           MOVE IN-TOTAL-DEATHS TO OUT-TOTAL-DTHS.
           MOVE TOTAL-DEATHS-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD AFTER ADVANCING 1 LINE.

      *New recoveries value moved and wrote
           MOVE IN-NEW-RECOVERED TO OUT-NEW-REC.
           MOVE NEW-REC-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD AFTER ADVANCING 1 LINE.

      *Total Recoveries moved and wrote
           MOVE IN-TOTAL-RECOVERED TO OUT-TOTAL-REC.
           MOVE TOTAL-REC-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD AFTER ADVANCING 1 LINE.


       END PROGRAM MIDTERM.
