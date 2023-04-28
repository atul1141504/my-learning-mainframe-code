      * CICS CLAIMS PROCESSING MODULE V1.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MF28CB1.
       AUTHOR. APATEL.
       DATE-WRITTEN. 10/06/2020.
      *****************************************************************
      * BUSINESS FUNCTION: THIS PROGRAM IS DESIGNED TO PROCESS BELOW
      *                    FUNCTIONS:
      *                    1. CLAIMS INQUIRY
      *                    2. ADD NEW CLAIM DETAILS IN DATABASE
      *
      * PROGRAM TYPE: COBOL-CICS-DB2.
      *
      * PROCESSING TYPE: CICS TRANSACTIONS VIA BMS SCREEN
      *
      * BMS: MF28BMS
      *
      * COPYBOOKS: MF28BMS
      *
      * LINKAGE COPYBOOKS: NONE
      *
      * TABLES: NONE
      *
      * CALLING MODULES: NONE(OR CICS SCREEN)
      *
      * CALLED MODULES: MF28CB2 - DATA VALIDATION MODULE.
      *                 MF28CB3 - CLAIMS/TRANSACTION PROCESSING MODULE
      *                           BY INTERACTING DB2 SYSTEM.
      *
      * PROGRAMMER: ATUL PATEL
      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        COPY MF28BMS.
       01 WS-ERROR-FLAG                PIC X(01) VALUE SPACES.
           88 ERROR-TRUE               VALUE 'Y'.
           88 ERROR-FALSE              VALUE 'N'.
       01 WS-DATE-ERROR-FLAG           PIC X(01) VALUE SPACES.
           88 DATE-ERROR-TRUE          VALUE 'Y'.
           88 DATE-ERROR-FALSE         VALUE 'N'.
       01 WS-MF28CB2                   PIC X(08) VALUE 'MF28CB2'.
       01 WS-MF28CB3                   PIC X(08) VALUE 'MF28CB3'.
       01 WS-MSG                       PIC X(24) VALUE
                                          'TRANSACTION ENDED'.
       PROCEDURE DIVISION.
      ******************************************************************
      * MAIN PARA STARTS FROM HERE                                     *
      ******************************************************************
       A1000-MAIN-PARA.
      *---------------*
           MOVE LOW-VALUES TO MAP1I
           MOVE LOW-VALUES TO MAP1O
           PERFORM A1100-SEND-MAP.
           PERFORM A2000-RECEIVE-MAP.
           PERFORM A3000-PROCESS-PARA.
           PERFORM A9999-END-PARA.
      ******************************************************************
      *                       SEND MAPSET TO CICS                      *
      ******************************************************************
       A1100-SEND-MAP.
      *--------------*
           EXEC CICS SEND
               MAP('MAP1')
               MAPSET('MF28BMS')
               FROM(MAP1O)
               ERASE
           END-EXEC.
      ******************************************************************
      *                  RECEIVE DATA FROM SCREEN                      *
      ******************************************************************
       A2000-RECEIVE-MAP.
      *-----------------*
           EXEC CICS RECEIVE
               MAP('MAP1')
               MAPSET('MF28BMS')
               INTO(MAP1I)
           END-EXEC.
      ******************************************************************
      *               PROCESS THE DATA RECEIVED FROM SCREEN            *
      ******************************************************************
       A3000-PROCESS-PARA.
      *------------------*
      *-- Call MF28CB2 for Data Validation
           CALL WS-MF28CB2 USING MAP1I
                                 MAP1O
                                 WS-ERROR-FLAG.
           IF ERROR-TRUE
              CONTINUE
           ELSE
      *-- Call MF28CB3 for Inserting and Fetching from Claims Table
              CALL WS-MF28CB3 USING MAP1I
                                    MAP1O
           END-IF.
           PERFORM A1100-SEND-MAP.
      ******************************************************************
      *             IN CASE OF ERROR DISPLAY ERROR MESSAGE             *
      ******************************************************************
       A9999-END-PARA.
      *--------------*
              EXEC CICS RETURN
                TRANSID ('MF28')
              END-EXEC.
      ******************************************************************
      *                        END OF PROGRAM                          *
      ******************************************************************