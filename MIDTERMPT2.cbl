      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIDTERMPT2.
       ENVIRONMENT DIVISION.
            INPUT-OUTPUT SECTION.
               FILE-CONTROL.
               SELECT INPUT-FILE ASSIGN TO "countries.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.

               SELECT OUTPUT-FILE ASSIGN TO "FORMIDT"
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
           01 OUTPUT-RECORD PIC X(120).
      ****************************************************************************
       WORKING-STORAGE SECTION.
      *Values for the format of the header
           01 END-OF-FILE                  PIC XXX VALUE "NO".

           01 TOP-LINE.
               05                            PIC X(24) VALUE SPACES.
               05 HDRCOUNTRY                 PIC X(7) VALUE 'COUNTRY'.

               05                            PIC X(5).
               05 HDRNEW1                    PIC X(3) VALUE 'NEW'.

               05                            PIC X(9).
               05 TTLHDR1                    PIC X(5) VALUE 'TOTAL'.

               05                            PIC X(8).
               05 HDRNEW2                    PIC X(3) VALUE 'NEW'.

               05                            PIC X(9).
               05 TTLHDR2                    PIC X(5) VALUE 'TOTAL'.

               05                            PIC X(9).
               05 HDRNEW3                    PIC X(3) VALUE'NEW'.

               05                            PIC X(12).
               05 TTLHDR3                    PIC X(5) VALUE 'TOTAL'.
****************************************************************************************
           01 2ND-LINE.
               05 HDR-INDENT-2               PIC X(25) VALUE SPACES.
               05 HDRCODE                    PIC X(4) VALUE 'CODE'.

               05                            PIC X(6).
               05 HDRCASES1                  PIC X(5) VALUE 'CASES'.

               05                            PIC X(8).
               05 HDRCASES2                  PIC X(5) VALUE 'CASES'.

               05                            PIC X(7).
               05 HDRDTHS1                   PIC X(6) VALUE 'DEATHS'.

               05                            PIC X(7).
               05 HDRDTHS2                   PIC X(6) VALUE 'DEATHS'.

               05                            PIC X(7).
               05 HDRREC                     PIC X(8) VALUE 'RECOVERY'.

               05                            PIC X(7).
               05 HDRREC2                    PIC X(8) VALUE 'RECOVERY'.

     *******************************************************************************
           01 DASHED-LINE.
            05 FILLER                   PIC X(120) VALUES ALL '*'.

           01 OUTLINES.
               05 OUT-COUNTRY          PIC X(25).
               05 FILLER               PIC X VALUE SPACE.
               05 OUT-COUNTRYCODE      PIC XX.
               05 FILLER               PIC X(7) VALUE SPACES.
               05 OUT-NEWCONF          PIC 9(5).
               05 FILLER               PIC X(6).
               05 OUT-TTLCONF          PIC 9(9).
               05 FILLER               PIC X(6).
               05 OUT-NEWDTHS          PIC 9(5).
               05 FILLER               PIC X(6).
               05 OUT-TTLDTHS          PIC 9(6).
               05 FILLER               PIC X(9).
               05 OUT-NEWREC           PIC 9(5).
               05 FILLER               PIC X(9).
               05 OUT-TTLREC           PIC 9(6).

           01 TOTALCOMPUTES.
               05 NCTTL                PIC 9(5).
               05 TCTTL                PIC 9(8).
               05 NDTTL                PIC 9(5).
               05 TDTTL                PIC 9(6).
               05 NRTTL                PIC 9(5).
               05 TRTTL                PIC 9(6).


           01 TOTALLINES.
               05 ENDTTL               PIC X(7) VALUE 'TOTALS:'.
               05 FILLER               PIC X(27).
               05 TOTALNEWCASES        PIC ZZ,ZZZ.
               05 FILLER               PIC X(5).
               05 TOTALTTLCASES        PIC ZZ,ZZZ,ZZZ.
               05 FILLER               PIC X(6).
               05 TOTALNEWDTHS         PIC ZZ,ZZZ.
               05 FILLER               PIC X(5).
               05 TOTALTTLDTHS         PIC ZZZ,ZZZ.
               05 FILLER               PIC X(8).
               05 TOTALNEWREC          PIC ZZ,ZZZ.
               05 FILLER               PIC X(8).
               05 TOTALTTLREC          PIC ZZZ,ZZZ.





       PROCEDURE DIVISION.
       100-MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.

           PERFORM 300-WRITE-FILE-HEADERS.
           PERFORM 200-READ-RECORD
           UNTIL END-OF-FILE = "YES".


           PERFORM 500-WRITE-TTL-LINES.
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
           STOP RUN.
      ***************************************************************************
       200-READ-RECORD.
      *Reads the record and stores the values in the correct spots
           READ INPUT-FILE
           AT END MOVE "YES" TO END-OF-FILE
           NOT AT END PERFORM 400-WRITE-DATA.
      *****************************************************************************
       300-WRITE-FILE-HEADERS.
           MOVE TOP-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           MOVE 2ND-LINE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
           MOVE DASHED-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.

      *****Transfer of values to output values.
       400-WRITE-DATA.
           PERFORM 600-COMPUTE-TOTALS.
           MOVE IN-COUNTRY TO OUT-COUNTRY
           MOVE IN-COUNTRY-CODE TO OUT-COUNTRYCODE.
           MOVE IN-NEW-CONFIRMED TO OUT-NEWCONF.
           MOVE IN-TOTAL-CONFIRMED TO OUT-TTLCONF.
           MOVE IN-NEW-DEATHS TO OUT-NEWDTHS.
           MOVE IN-TOTAL-DEATHS TO OUT-TTLDTHS.
           MOVE IN-NEW-RECOVERED TO OUT-NEWREC.
           MOVE IN-TOTAL-RECOVERED TO OUT-TTLREC
           MOVE OUTLINES TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD AFTER ADVANCING 1 LINE.

       500-WRITE-TTL-LINES.
      *MOVES  VALUES TO OUTPUT PIC
           MOVE NCTTL TO TOTALNEWCASES.
           MOVE TCTTL TO TOTALTTLCASES.
           MOVE NDTTL TO TOTALNEWDTHS.
           MOVE TDTTL TO TOTALTTLDTHS.
           MOVE NRTTL TO TOTALNEWREC.
           MOVE TRTTL TO TOTALTTLREC.
           MOVE TOTALLINES TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD AFTER ADVANCING 2 LINE.
           MOVE DASHED-LINE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD AFTER ADVANCING 1 LINE.

       600-COMPUTE-TOTALS.
           COMPUTE NCTTL = NCTTL + IN-NEW-CONFIRMED.
           COMPUTE TCTTL = TCTTL + IN-TOTAL-CONFIRMED.
           COMPUTE NDTTL = NDTTL + IN-NEW-DEATHS.
           COMPUTE TDTTL = TDTTL + IN-TOTAL-DEATHS.
           COMPUTE NRTTL = NRTTL + IN-NEW-RECOVERED.
           COMPUTE TRTTL = TRTTL + IN-TOTAL-RECOVERED.

       END PROGRAM MIDTERMPT2.
