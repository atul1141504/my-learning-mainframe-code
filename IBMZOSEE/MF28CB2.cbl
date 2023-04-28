      * MF28CB2 - DATA VALIDATION MODULE V1.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MF28CB2.
       AUTHOR. APATEL.
       DATE-WRITTEN. 10/06/2020.
      *****************************************************************
      * BUSINESS FUNCTION: THIS PROGRAM IS DESIGNED TO VALIDATE BELOW
      *                    DATA RECIEVED FROM CALLING MODULE:
      *                    1. NUMERIC DATA TYPE
      *                    2. DATE FIELD VALIDATION
      *                    3. PAID AMOUNT VALIDATION
      *
      * PROGRAM TYPE: COBOL.
      *
      * PROCESSING TYPE: VIA CALLING OR DRIVER MODULE
      *
      * BMS: MF28BMS
      *
      * COPYBOOKS: NONE
      *
      * LINKAGE COPYBOOKS: MF28BMS
      *
      * TABLES: NONE
      *
      * CALLING MODULE: MF28CB1 - PROCESS TRANSACTION FROM BMS SCREEN
      *
      * CALLED MODULES: MF28CB3 - NONE
      *
      * PROGRAMMER: ATUL PATEL
      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VARIABLES.
           05 WS-CURR-DATE             PIC X(16).
           05 WS-CLAIM-DATE.
                10 WS-YYYY             PIC X(4).
                10 WS-MM               PIC X(2).
                10 WS-DD               PIC X(2).
       LINKAGE SECTION.
         COPY MF28BMS.
       01 ERROR-FLAG                   PIC X(01) VALUE SPACES.
       PROCEDURE DIVISION USING MAP1I
                                MAP1O
                                ERROR-FLAG.
      ******************************************************************
      * MAIN PARA STARTS FROM HERE                                     *
      ******************************************************************
       A1000-MAIN-PARA.
      *---------------*
           PERFORM A1100-VALIDATE-DATA
           GOBACK.
      ******************************************************************
      *                    VALIDATION OF DATA                          *
      ******************************************************************
       A1100-VALIDATE-DATA.
      *-------------------*
           INITIALIZE ERROR-FLAG.
           EVALUATE SELOPTVI
              WHEN 1
                PERFORM A1000-CHECK-CLAIM-NUM
              WHEN 2
                PERFORM A1000-CHECK-CLAIM-NUM
                IF ERROR-FLAG = 'N'
                  PERFORM A2000-CHECK-PAID
                END-IF
                IF ERROR-FLAG = 'N'
                  PERFORM A2500-CHECK-PAID-NUM
                END-IF
                IF ERROR-FLAG = 'N'
                  PERFORM A3000-CHECK-VALUE
                END-IF
                IF ERROR-FLAG = 'N'
                  PERFORM A3500-CHECK-DATE
                END-IF
              WHEN OTHER
                 MOVE 'ENTER A VALID OPTION'          TO MSGO
                 MOVE 'Y'                             TO ERROR-FLAG
              END-EVALUATE.
      ******************************************************************
      *                    CHECK CLAIM NUMBER                          *
      ******************************************************************
       A1000-CHECK-CLAIM-NUM.
      *---------------------*
           DISPLAY 'NARESH: ' CLMNUMVI
            IF CLMNUMVI = 0
               MOVE 'ENTER CLAIM NUMBER'              TO MSGO
               MOVE 'Y'                               TO ERROR-FLAG
            ELSE
                IF CLMNUMVI IS NUMERIC
                 MOVE 'N'                             TO ERROR-FLAG
                ELSE
                 MOVE 'Y'                             TO ERROR-FLAG
                 MOVE 'ENTER VALID CLAIM NO'          TO MSGO
                END-IF
            END-IF.
      ******************************************************************
      *          CHECK PAID IS LESS THAN OR EQUAL TO TOTAL VALUE       *
      ******************************************************************
       A2000-CHECK-PAID.
      *----------------*
           IF PAIDVI < VALVI
                MOVE 'N'                              TO ERROR-FLAG
           ELSE
                DISPLAY 'PAID V: ' PAIDVI
                DISPLAY 'VALUE V: ' VALVI
                MOVE 'PAID SHOULD BE LESS THAN VALUE' TO MSGO
                MOVE 'Y'                              TO ERROR-FLAG
           END-IF.
      ******************************************************************
      *                  CHECK IF PAID IS NUMERIC                      *
      ******************************************************************
       A2500-CHECK-PAID-NUM.
      *--------------------*
            IF PAIDVI IS NUMERIC
                 MOVE 'N'                             TO ERROR-FLAG
            ELSE
               IF PAIDVI = 0
                    MOVE 'N'                          TO ERROR-FLAG
               ELSE
                  DISPLAY 'PAID V: ' PAIDVI
                  MOVE 'ENTER VALID PAID VALUE '      TO MSGO
                  MOVE 'Y'                            TO ERROR-FLAG
               END-IF
            END-IF.
      ******************************************************************
      *                  CHECK VALUE IS NUMERIC                        *
      ******************************************************************
       A3000-CHECK-VALUE.
      *-----------------*
             IF VALVI IS NUMERIC
                   MOVE 'N'                           TO ERROR-FLAG
             ELSE
                  DISPLAY 'VALUE :' VALVI
                  MOVE 'VALUE IS NOT NUMERIC'         TO MSGO
                  MOVE 'Y'                            TO ERROR-FLAG
             END-IF.
      ******************************************************************
      *                  CHECK DATES ARE IN PAST                       *
      ******************************************************************
       A3500-CHECK-DATE.
      *----------------*
           MOVE CLMDATVI(1:4)                         TO WS-YYYY
           MOVE CLMDATVI(6:2)                         TO WS-MM
           MOVE CLMDATVI(9:2)                         TO WS-DD
           MOVE FUNCTION CURRENT-DATE                 TO WS-CURR-DATE
           IF WS-CLAIM-DATE > WS-CURR-DATE(1:8)
                MOVE 'Y'                              TO ERROR-FLAG
                MOVE 'CLAIM DATE SHOULD BE IN PAST'   TO MSGO
           ELSE
                MOVE 'N'                              TO ERROR-FLAG
           END-IF.
      ******************************************************************
      *                        END OF PROGRAM                          *
      ******************************************************************