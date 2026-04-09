       IDENTIFICATION DIVISION.                                         00010000
                                                                        00020000
       PROGRAM-ID. RPT6000.                                             00030001
                                                                        00040000
       ENVIRONMENT DIVISION.                                            00050000
                                                                        00060000
       INPUT-OUTPUT SECTION.                                            00070000
                                                                        00080000
       FILE-CONTROL.                                                    00090000
           SELECT CUSTMAST ASSIGN TO CUSTMAST.                          00100000
           SELECT SALESRPT ASSIGN TO RPOT6000.                          00110001
           SELECT SALESMAS ASSIGN TO SALESMAS.                          00120003
                                                                        00130000
       DATA DIVISION.                                                   00140000
                                                                        00150000
       FILE SECTION.                                                    00160000
                                                                        00170000
       FD  CUSTMAST                                                     00180000
           RECORDING MODE IS F                                          00190000
           LABEL RECORDS ARE STANDARD                                   00200000
           RECORD CONTAINS 130 CHARACTERS                               00210000
           BLOCK CONTAINS 130 CHARACTERS.                               00220000
       COPY CUSTMAST.                                                   00221004
                                                                        00230004
                                                                        00240004
                                                                        00250004
                                                                        00260004
                                                                        00270004
                                                                        00280004
                                                                        00290004
                                                                        00300000
       FD  SALESRPT                                                     00310000
           RECORDING MODE IS F                                          00320000
           LABEL RECORDS ARE STANDARD                                   00330000
           RECORD CONTAINS 130 CHARACTERS                               00340000
           BLOCK CONTAINS 130 CHARACTERS.                               00350000
       01  PRINT-AREA      PIC X(130).                                  00360000
                                                                        00370003
                                                                        00371004
       FD  SALESMAS                                                     00380003
           RECORDING MODE IS F                                          00390003
           LABEL RECORDS ARE STANDARD                                   00400003
           RECORD CONTAINS 130 CHARACTERS                               00410003
           BLOCK CONTAINS 130 CHARACTERS.                               00420003
       COPY SALESMAS.                                                   00430004
                                                                        00440004
                                                                        00450004
                                                                        00460004
                                                                        00470000
       WORKING-STORAGE SECTION.                                         00480000
                                                                        00490000
       01 SALESREP-TABLE.                                               00500003
          05  SALESREP-GROUP OCCURS 100 TIMES                           00510003
                             INDEXED BY SRT-INDEX.                      00520002
              10  SALESREP-NUMBER   PIC 9(2).                           00530002
              10  SALES-REP-NAME    PIC X(10).                          00540002
                                                                        00550003
       01 SALESREP-TABLE-RECORD.                                        00560003
          05  T-SALESREP-NUMBER    PIC 9(2).                            00570003
          05  T-SALESREP-NAME      PIC X(10).                           00580003
                                                                        00590002
       01 CALCULATED-FIELDS            PACKED-DECIMAL.                  00600001
          05 CHANGE-AMOUNT             PIC S9(5)V99.                    00610000
                                                                        00620002
       01  SWITCHES.                                                    00630000
           05  CUSTMAST-EOF-SWITCH     PIC X    VALUE "N".              00640000
               88  CUSTMAST-EOF                 VALUE "Y".              00650000
           05  FIRST-RECORD-SWITCH     PIC X    VALUE "Y".              00660000
               88  FIRST-RECORD                 VALUE "Y".              00670000
           05  SALESMAS-EOF-SWITCH     PIC X    VALUE "N".              00680003
               88  SALESMAS-EOF                 VALUE "Y".              00690003
                                                                        00700003
       01  CONTROL-FIELDS.                                              00710000
           05  OLD-SALESREP-NUMBER     PIC 99.                          00720000
           05  OLD-BRANCH-NUMBER       PIC 99.                          00730000
                                                                        00740000
       01  PRINT-FIELDS        PACKED-DECIMAL.                          00750001
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.                  00760000
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.                   00770000
           05  LINE-COUNT      PIC S9(3)   VALUE +99.                   00780000
           05  SPACE-CONTROL   PIC S9.                                  00790000
                                                                        00800000
       01  TOTAL-FIELDS               PACKED-DECIMAL.                   00810001
           05  SALESREP-TOTAL-THIS-YTD PIC S9(6)V99  VALUE ZERO.        00820000
           05  SALESREP-TOTAL-LAST-YTD PIC S9(6)V99  VALUE ZERO.        00830000
           05  BRANCH-TOTAL-THIS-YTD  PIC S9(6)V99   VALUE ZERO.        00840000
           05  BRANCH-TOTAL-LAST-YTD  PIC S9(6)V99   VALUE ZERO.        00850000
           05  GRAND-TOTAL-THIS-YTD   PIC S9(7)V99   VALUE ZERO.        00860000
           05  GRAND-TOTAL-LAST-YTD   PIC S9(7)V99   VALUE ZERO.        00870000
                                                                        00880000
       01  CURRENT-DATE-AND-TIME.                                       00890000
           05  CD-YEAR         PIC 9999.                                00900000
           05  CD-MONTH        PIC 99.                                  00910000
           05  CD-DAY          PIC 99.                                  00920000
           05  CD-HOURS        PIC 99.                                  00930000
           05  CD-MINUTES      PIC 99.                                  00940000
           05  FILLER          PIC X(9).                                00950000
                                                                        00960000
       01  HEADING-LINE-1.                                              00970000
           05  FILLER          PIC X(7)    VALUE "DATE:  ".             00980000
           05  HL1-MONTH       PIC 9(2).                                00990000
           05  FILLER          PIC X(1)    VALUE "/".                   01000000
           05  HL1-DAY         PIC 9(2).                                01010000
           05  FILLER          PIC X(1)    VALUE "/".                   01020000
           05  HL1-YEAR        PIC 9(4).                                01030000
           05  FILLER          PIC X(26)   VALUE SPACE.                 01040003
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".01050000
           05  FILLER          PIC X(31)   VALUE "EPORT".               01060003
           05  FILLER          PIC X(22)   VALUE SPACE.                 01070000
           05  FILLER          PIC X(6)    VALUE "PAGE: ".              01080003
           05  Hl1-PAGE-NUMBER PIC ZZZ9.                                01090000
           05  FILLER          PIC X(26)   VALUE SPACE.                 01100003
                                                                        01110000
       01  HEADING-LINE-2.                                              01120000
           05  FILLER          PIC X(7)    VALUE "TIME:  ".             01130000
           05  HL2-HOURS       PIC 9(2).                                01140000
           05  FILLER          PIC X(1)    VALUE ":".                   01150000
           05  HL2-MINUTES     PIC 9(2).                                01160000
           05  FILLER          PIC X(82)   VALUE SPACE.                 01170003
           05  FILLER          PIC X(7)    VALUE "RPT6000".             01180003
           05  FILLER          PIC X(29)   VALUE SPACE.                 01190003
                                                                        01200000
                                                                        01210000
       01  HEADING-LINE-3.                                              01220000
           05  FILLER      PIC X(54)   VALUE SPACE.                     01230003
           05  FILLER      PIC X(19)   VALUE "SALES         SALES".     01240003
           05  FILLER      PIC X(8)    VALUE SPACE.                     01250003
           05  FILLER      PIC X(17)   VALUE "CHANGE     CHANGE".       01260003
           05  FILLER      PIC X(32)   VALUE SPACE.                     01270003
                                                                        01280000
       01  HEADING-LINE-4.                                              01290000
           05  FILLER      PIC X(17)   VALUE "BRANCH   SALESREP".       01300003
           05  FILLER      PIC X(13)   VALUE SPACE.                     01310003
           05  FILLER      PIC X(8)    VALUE "CUSTOMER".                01320003
           05  FILLER      PIC X(14)   VALUE SPACE.                     01330003
           05  FILLER      PIC X(22)   VALUE "THIS YTD      LAST YTD".  01340003
           05  FILLER      PIC X(7)    VALUE SPACE.                     01350003
           05  FILLER      PIC X(18)   VALUE "AMOUNT     PERCENT".      01360003
           05  FILLER      PIC X(31)   VALUE SPACE.                     01370003
                                                                        01380000
       01  HEADING-LINE-6.                                              01390000
                                                                        01400000
           05  FILLER              PIC X(6)     VALUE ALL '-'.          01410000
           05  FILLER              PIC X(1)     VALUE SPACE.            01420000
           05  FILLER              PIC X(13)    VALUE ALL '-'.          01430003
           05  FILLER              PIC X(1)     VALUE SPACE.            01440000
           05  FILLER              PIC X(26)    VALUE ALL '-'.          01450003
           05  FILLER              PIC X(3)     VALUE SPACE.            01460003
           05  FILLER              PIC X(12)    VALUE ALL '-'.          01470003
           05  FILLER              PIC X(3)     VALUE SPACE.            01480000
           05  FILLER              PIC X(12)    VALUE ALL '-'.          01490003
           05  FILLER              PIC X(4)     VALUE SPACE.            01500000
           05  FILLER              PIC X(12)    VALUE ALL '-'.          01510003
           05  FILLER              PIC X(2)     VALUE SPACE.            01520003
           05  FILLER              PIC X(7)     VALUE ALL '-'.          01530003
           05  FILLER              PIC X(31)    VALUE SPACE.            01540003
                                                                        01550003
                                                                        01560000
       01  CUSTOMER-LINE.                                               01570000
                                                                        01580000
           05  FILLER              PIC X(2)     VALUE SPACE.            01590000
           05  CL-BRANCH-NUMBER    PIC X(2).                            01600000
           05  FILLER              PIC X(3)     VALUE SPACE.            01610003
           05  CL-SALESREP-NUMBER  PIC X(2).                            01620000
           05  FILLER              PIC X(1)     VALUE SPACE.            01630003
           05  CL-SALESREP-NAME    PIC X(10).                           01640002
           05  FILLER              PIC X(1)     VALUE SPACE.            01650003
           05  CL-CUSTOMER-NUMBER  PIC X(5).                            01660003
           05  FILLER              PIC X(1)     VALUE SPACE.            01670003
           05  CL-CUSTOMER-NAME    PIC X(20).                           01680000
           05  FILLER              PIC X(6)     VALUE SPACE.            01690003
           05  CL-SALES-THIS-YTD   PIC ZZ,ZZ9.99-.                      01700000
           05  FILLER              PIC X(4)     VALUE SPACE.            01710000
           05  CL-SALES-LAST-YTD   PIC ZZ,ZZ9.99-.                      01720000
           05  FILLER              PIC X(4)     VALUE SPACE.            01730000
           05  CL-CHANGE-AMOUNT    PIC ZZ,ZZ9.99-.                      01740000
           05  FILLER              PIC X(2)     VALUE SPACE.            01750003
           05  CL-CHANGE-PERCENT   PIC +++9.9.                          01760003
           05  CL-CHANGE-PERCENT-R REDEFINES CL-CHANGE-PERCENT          01770001
                                   PIC X(6).                            01780002
           05  FILLER              PIC X(31)    VALUE SPACE.            01790003
                                                                        01800000
       01  GRAND-TOTAL-HEADER.                                          01810000
           05  FILLER              PIC X(41)    VALUE SPACE.            01820000
           05  FILLER              PIC X(13)    VALUE ALL '='.          01830000
           05  FILLER              PIC X(1)     VALUE SPACE.            01840000
           05  FILLER              PIC X(13)    VALUE ALL '='.          01850000
           05  FILLER              PIC X(1)     VALUE SPACE.            01860000
           05  FILLER              PIC X(13)    VALUE ALL '='.          01870000
           05  FILLER              PIC X(3)     VALUE SPACE.            01880000
           05  FILLER              PIC X(6)     VALUE ALL '='.          01890000
                                                                        01900000
       01  SALESREP-FILLER-LINE.                                        01910000
           05  FILLER              PIC X(50)    VALUE SPACE.            01920004
           05  FILLER              PIC X(13)    VALUE ALL '-'.          01930000
           05  FILLER              PIC X(1)     VALUE SPACE.            01940000
           05  FILLER              PIC X(13)    VALUE ALL '-'.          01950000
           05  FILLER              PIC X(1)     VALUE SPACE.            01960000
           05  FILLER              PIC X(13)    VALUE ALL '-'.          01970000
           05  FILLER              PIC X(3)     VALUE SPACE.            01980000
           05  FILLER              PIC X(6)     VALUE ALL '-'.          01990000
                                                                        02000000
       01  SALESREP-TOTAL-LINE.                                         02010000
           05  FILLER              PIC X(38)    VALUE SPACE.            02020004
           05  FILLER              PIC X(14)    VALUE "SALES TOTAL".    02030000
           05  STL-SALES-THIS-YTD  PIC zzz,zz9.99-.                     02040000
           05  FILLER              PIC X(3)    VALUE SPACE.             02050000
           05  STL-SALES-LAST-YTD  PIC zzz,zz9.99-.                     02060000
           05  FILLER              PIC X(3)    VALUE SPACE.             02070000
           05  STL-CHANGE-AMOUNT   PIC zzz,zz9.99-.                     02080000
           05  FILLER              PIC X(2)    VALUE SPACE.             02090004
           05  STL-CHANGE-PERCENT  PIC +++9.9.                          02100002
           05  STL-CHANGE-PERCENT-R REDEFINES STL-CHANGE-PERCENT        02110002
                                   PIC X(6).                            02120002
           05  FILLER              PIC X(47)   VALUE "*".               02130000
                                                                        02140000
       01  BRANCH-FILLER-LINE.                                          02150000
           05  FILLER              PIC X(50)    VALUE SPACE.            02160004
           05  FILLER              PIC X(13)    VALUE ALL '-'.          02170000
           05  FILLER              PIC X(1)     VALUE SPACE.            02180000
           05  FILLER              PIC X(13)    VALUE ALL '-'.          02190000
           05  FILLER              PIC X(1)     VALUE SPACE.            02200000
           05  FILLER              PIC X(13)    VALUE ALL '-'.          02210000
           05  FILLER              PIC X(3)     VALUE SPACE.            02220000
           05  FILLER              PIC X(6)     VALUE ALL '-'.          02230000
                                                                        02240000
       01  BRANCH-TOTAL-LINE.                                           02250000
           05  FILLER              PIC X(35)    VALUE SPACE.            02260004
           05  FILLER              PIC X(16)    VALUE "  BRANCH TOTAL". 02270003
           05  BTL-SALES-THIS-YTD  PIC $$$$,$$9.99-.                    02280001
           05  FILLER              PIC X(3)    VALUE SPACE.             02290003
           05  BTL-SALES-LAST-YTD  PIC $$$$,$$9.99-.                    02300001
           05  FILLER              PIC X(2)    VALUE SPACE.             02310004
           05  BTL-CHANGE-AMOUNT   PIC $$$$,$$9.99-.                    02320001
           05  FILLER              PIC X(2)    VALUE SPACE.             02330002
           05  BTL-CHANGE-PERCENT  PIC +++9.9.                          02340001
           05  BTL-CHANGE-PERCENT-R REDEFINES BTL-CHANGE-PERCENT        02350001
                                   PIC X(6).                            02360001
           05  FILLER              PIC X(31)   VALUE "**".              02370003
                                                                        02380000
       01  GRAND-TOTAL-LINE.                                            02390000
           05  FILLER              PIC X(36)    VALUE SPACE.            02400003
           05  FILLER              PIC X(14)    VALUE "   GRAND TOTAL". 02410003
           05  GTL-SALES-THIS-YTD  PIC Z,ZZZ,ZZ9.99-.                   02420000
           05  FILLER              PIC X(1)     VALUE SPACE.            02430000
           05  GTL-SALES-LAST-YTD  PIC Z,ZZZ,ZZ9.99-.                   02440000
           05  FILLER              PIC X(1)     VALUE SPACE.            02450000
           05  GTL-CHANGE-AMOUNT   PIC Z,ZZZ,ZZ9.99-.                   02460000
           05  FILLER              PIC X(2)     VALUE SPACE.            02470003
           05  GTL-CHANGE-PERCENT  PIC +++9.9.                          02480002
           05  GTL-CHANGE-PERCENT-R REDEFINES GTL-CHANGE-PERCENT        02490002
                                   PIC X(6).                            02500002
           05  FILLER              PIC X(31)    VALUE " ***".           02510003
                                                                        02520000
       PROCEDURE DIVISION.                                              02530000
                                                                        02540000
       000-PREPARE-SALES-REPORT.                                        02550000
                                                                        02560000
           INITIALIZE SALESREP-TABLE.                                   02570003
           OPEN INPUT  CUSTMAST                                         02580000
                INPUT  SALESMAS                                         02590003
                OUTPUT SALESRPT.                                        02600000
           PERFORM 100-FORMAT-REPORT-HEADING.                           02610000
           PERFORM 200-LOAD-SALESREP-TABLE.                             02620003
           PERFORM 300-PREPARE-SALES-LINES                              02630000
               UNTIL CUSTMAST-EOF-SWITCH = "Y".                         02640000
           PERFORM 500-PRINT-GRAND-TOTALS.                              02650000
           CLOSE CUSTMAST                                               02660000
                 SALESMAS                                               02670003
                 SALESRPT.                                              02680000
           STOP RUN.                                                    02690000
                                                                        02700000
       100-FORMAT-REPORT-HEADING.                                       02710000
                                                                        02720000
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.         02730000
           MOVE CD-MONTH   TO HL1-MONTH.                                02740000
           MOVE CD-DAY     TO HL1-DAY.                                  02750000
           MOVE CD-YEAR    TO HL1-YEAR.                                 02760000
           MOVE CD-HOURS   TO HL2-HOURS.                                02770000
           MOVE CD-MINUTES TO HL2-MINUTES.                              02780000
                                                                        02790000
       200-LOAD-SALESREP-TABLE.                                         02800003
           PERFORM                                                      02810003
               WITH TEST AFTER                                          02820003
               VARYING SRT-INDEX FROM 1 BY 1                            02830003
               UNTIL SALESMAS-EOF OR SRT-INDEX = 100                    02840003
                  PERFORM 210-READ-SALESREP-RECORD                      02850003
                  IF NOT SALESMAS-EOF                                   02860003
                      MOVE T-SALESREP-NUMBER                            02870003
                          TO SALESREP-NUMBER (SRT-INDEX)                02880003
                      MOVE T-SALESREP-NAME                              02890003
                          TO SALES-REP-NAME (SRT-INDEX)                 02900003
                  END-IF                                                02910003
           END-PERFORM.                                                 02920003
                                                                        02930003
       210-READ-SALESREP-RECORD.                                        02940003
           READ SALESMAS RECORD INTO SALESREP-TABLE-RECORD              02950003
               AT END                                                   02960003
                   SET SALESMAS-EOF TO TRUE.                            02970003
                                                                        02980000
                                                                        02990003
       300-PREPARE-SALES-LINES.                                         03000000
           PERFORM 310-READ-CUSTOMER-RECORD                             03010000
           EVALUATE TRUE                                                03020000
               WHEN CUSTMAST-EOF                                        03030000
                PERFORM 355-PRINT-SALES-REP-LINE                        03040000
                PERFORM 360-PRINT-BRANCH-LINE                           03050000
               WHEN FIRST-RECORD                                        03060000
                PERFORM 320-PRINT-CUSTOMER-LINE                         03070000
                MOVE "N" TO FIRST-RECORD-SWITCH                         03080000
                MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER          03090000
                MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER              03100000
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                03110000
                PERFORM 355-PRINT-SALES-REP-LINE                        03120000
                PERFORM 360-PRINT-BRANCH-LINE                           03130000
                PERFORM 320-PRINT-CUSTOMER-LINE                         03140000
                MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER          03150000
                MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER              03160000
               WHEN CM-SALESREP-NUMBER > OLD-SALESREP-NUMBER            03170000
                PERFORM 355-PRINT-SALES-REP-LINE                        03180000
                PERFORM 320-PRINT-CUSTOMER-LINE                         03190000
                MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER          03200000
               WHEN OTHER                                               03210000
                PERFORM 320-PRINT-CUSTOMER-LINE                         03220000
           END-EVALUATE.                                                03230000
                                                                        03240000
       310-READ-CUSTOMER-RECORD.                                        03250000
                                                                        03260000
           READ CUSTMAST                                                03270000
               AT END                                                   03280000
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH.                     03290000
                                                                        03300000
       320-PRINT-CUSTOMER-LINE.                                         03310000
                                                                        03320000
           IF LINE-COUNT > LINES-ON-PAGE                                03330000
               PERFORM 330-PRINT-HEADING-LINES.                         03340000
           IF FIRST-RECORD-SWITCH = "Y"                                 03350000
               MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER                03360000
           ELSE                                                         03370000
               IF CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                  03380000
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            03390000
               ELSE                                                     03400000
                   MOVE SPACE TO CL-BRANCH-NUMBER.                      03410000
           MOVE CM-CUSTOMER-NUMBER TO CL-CUSTOMER-NUMBER.               03420000
           MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER.               03430000
           PERFORM 325-MOVE-SALESREP-NAME.                              03440002
           MOVE CM-CUSTOMER-NAME TO CL-CUSTOMER-NAME.                   03450000
           MOVE CM-SALES-THIS-YTD TO CL-SALES-THIS-YTD.                 03460000
           ADD CM-SALES-THIS-YTD TO SALESREP-TOTAL-THIS-YTD             03470000
           END-ADD.                                                     03480000
           ADD CM-SALES-LAST-YTD TO SALESREP-TOTAL-LAST-YTD             03490000
           END-ADD.                                                     03500000
           MOVE CM-SALES-LAST-YTD TO CL-SALES-LAST-YTD.                 03510000
           COMPUTE CHANGE-AMOUNT =                                      03520000
               CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.                   03530000
           MOVE CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.                      03540000
           IF CM-SALES-LAST-YTD = ZERO                                  03550000
               MOVE "  N/A " TO CL-CHANGE-PERCENT-R                     03560002
           ELSE                                                         03570000
               COMPUTE CL-CHANGE-PERCENT ROUNDED =                      03580000
                   CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD              03590000
                   ON SIZE ERROR                                        03600000
                       MOVE "OVRFLW" TO CL-CHANGE-PERCENT-R.            03610001
           MOVE CUSTOMER-LINE TO PRINT-AREA.                            03620000
           PERFORM 350-WRITE-REPORT-LINE.                               03630000
           MOVE 1 TO SPACE-CONTROL.                                     03640000
           ADD CM-SALES-THIS-YTD TO BRANCH-TOTAL-THIS-YTD.              03650000
           ADD CM-SALES-LAST-YTD TO BRANCH-TOTAL-LAST-YTD.              03660000
                                                                        03670000
       325-MOVE-SALESREP-NAME.                                          03680002
           SET SRT-INDEX TO 1.                                          03690002
           SEARCH SALESREP-GROUP                                        03700002
              AT END                                                    03710002
                 MOVE "UNKNOWN" TO CL-SALESREP-NAME                     03720002
              WHEN SALESREP-NUMBER (SRT-INDEX) = CM-SALESREP-NUMBER     03730002
                 MOVE SALES-REP-NAME(SRT-INDEX) TO CL-SALESREP-NAME     03740003
           END-SEARCH.                                                  03750002
                                                                        03760000
       330-PRINT-HEADING-LINES.                                         03770000
           ADD 1 TO PAGE-COUNT.                                         03780000
           MOVE PAGE-COUNT TO Hl1-PAGE-NUMBER.                          03790000
           MOVE HEADING-LINE-1 TO PRINT-AREA.                           03800000
           PERFORM 340-WRITE-PAGE-TOP-LINE.                             03810000
           MOVE HEADING-LINE-2 TO PRINT-AREA.                           03820000
           MOVE 1 TO SPACE-CONTROL.                                     03830000
           PERFORM 350-WRITE-REPORT-LINE.                               03840000
           MOVE HEADING-LINE-3 TO PRINT-AREA.                           03850000
           MOVE 2 TO SPACE-CONTROL.                                     03860000
           PERFORM 350-WRITE-REPORT-LINE.                               03870000
           MOVE HEADING-LINE-4 TO PRINT-AREA.                           03880000
           MOVE 1 TO SPACE-CONTROL.                                     03890000
           PERFORM 350-WRITE-REPORT-LINE.                               03900000
           MOVE ZERO TO LINE-COUNT.                                     03910000
           MOVE 2 TO SPACE-CONTROL.                                     03920000
                                                                        03930000
       340-WRITE-PAGE-TOP-LINE.                                         03940000
                                                                        03950000
           WRITE PRINT-AREA.                                            03960000
           MOVE 1 TO LINE-COUNT.                                        03970000
                                                                        03980000
       350-WRITE-REPORT-LINE.                                           03990000
                                                                        04000000
           WRITE PRINT-AREA.                                            04010000
                                                                        04020000
                                                                        04030000
                                                                        04040000
       355-PRINT-SALES-REP-LINE.                                        04050000
           MOVE SALESREP-TOTAL-THIS-YTD TO STL-SALES-THIS-YTD.          04060000
           MOVE SALESREP-TOTAL-LAST-YTD TO STL-SALES-LAST-YTD.          04070000
           COMPUTE CHANGE-AMOUNT =                                      04080000
               SALESREP-TOTAL-THIS-YTD - SALESREP-TOTAL-LAST-YTD.       04090000
           MOVE CHANGE-AMOUNT TO STL-CHANGE-AMOUNT.                     04100000
           IF SALESREP-TOTAL-LAST-YTD = ZERO                            04110000
               MOVE "  N/A " TO STL-CHANGE-PERCENT-R                    04120002
           ELSE                                                         04130000
               COMPUTE STL-CHANGE-PERCENT ROUNDED =                     04140000
                   CHANGE-AMOUNT * 100 / SALESREP-TOTAL-LAST-YTD        04150000
                   ON SIZE ERROR                                        04160000
                       MOVE "OVRFLW" TO STL-CHANGE-PERCENT-R.           04170002
           MOVE SALESREP-FILLER-LINE TO PRINT-AREA.                     04180000
           PERFORM 350-WRITE-REPORT-LINE.                               04190000
           MOVE SALESREP-TOTAL-LINE TO PRINT-AREA.                      04200000
           PERFORM 350-WRITE-REPORT-LINE.                               04210000
           MOVE ZERO TO SALESREP-TOTAL-LAST-YTD.                        04220000
           MOVE ZERO TO SALESREP-TOTAL-THIS-YTD.                        04230000
           INITIALIZE SALESREP-TOTAL-THIS-YTD                           04240002
                      SALESREP-TOTAL-LAST-YTD.                          04250002
                                                                        04260000
       360-PRINT-BRANCH-LINE.                                           04270000
                                                                        04280000
           MOVE BRANCH-TOTAL-THIS-YTD TO BTL-SALES-THIS-YTD.            04290000
           MOVE BRANCH-TOTAL-LAST-YTD TO BTL-SALES-LAST-YTD.            04300000
           COMPUTE CHANGE-AMOUNT =                                      04310000
               BRANCH-TOTAL-THIS-YTD - BRANCH-TOTAL-LAST-YTD.           04320000
           MOVE CHANGE-AMOUNT TO BTL-CHANGE-AMOUNT.                     04330000
           IF BRANCH-TOTAL-LAST-YTD = ZERO                              04340000
               MOVE "  N/A " TO BTL-CHANGE-PERCENT-R                    04350002
           ELSE                                                         04360000
               COMPUTE BTL-CHANGE-PERCENT ROUNDED =                     04370000
                   CHANGE-AMOUNT * 100 / BRANCH-TOTAL-LAST-YTD          04380000
                   ON SIZE ERROR                                        04390000
                       MOVE "OVRFLW" TO BTL-CHANGE-PERCENT-R.           04400001
           MOVE BRANCH-FILLER-LINE TO PRINT-AREA.                       04410000
           PERFORM 350-WRITE-REPORT-LINE.                               04420000
           MOVE BRANCH-TOTAL-LINE TO PRINT-AREA.                        04430000
           PERFORM 350-WRITE-REPORT-LINE.                               04440000
           MOVE SPACES TO PRINT-AREA.                                   04450000
           PERFORM 350-WRITE-REPORT-LINE.                               04460000
           ADD BRANCH-TOTAL-THIS-YTD TO GRAND-TOTAL-THIS-YTD.           04470000
           ADD BRANCH-TOTAL-LAST-YTD TO GRAND-TOTAL-LAST-YTD.           04480000
           INITIALIZE BRANCH-TOTAL-THIS-YTD                             04490001
                      BRANCH-TOTAL-LAST-YTD.                            04500001
                                                                        04510000
                                                                        04520000
       500-PRINT-GRAND-TOTALS.                                          04530000
                                                                        04540000
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.             04550000
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.             04560000
           COMPUTE CHANGE-AMOUNT =                                      04570000
               GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.             04580000
           MOVE CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.                     04590000
           IF GRAND-TOTAL-LAST-YTD = ZERO                               04600000
               MOVE "  N/A " TO GTL-CHANGE-PERCENT-R                    04610002
           ELSE                                                         04620000
               COMPUTE GTL-CHANGE-PERCENT ROUNDED =                     04630000
                   CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD           04640000
                   ON SIZE ERROR                                        04650000
                       MOVE "OVRFLW" TO GTL-CHANGE-PERCENT-R.           04660002
           MOVE GRAND-TOTAL-HEADER TO PRINT-AREA.                       04670000
           PERFORM 350-WRITE-REPORT-LINE.                               04680000
           MOVE GRAND-TOTAL-LINE TO PRINT-AREA.                         04690000
           PERFORM 350-WRITE-REPORT-LINE.                               04700000
           INITIALIZE GRAND-TOTAL-THIS-YTD                              04710002
                      GRAND-TOTAL-LAST-YTD.                             04720002
                                                                        04730000
