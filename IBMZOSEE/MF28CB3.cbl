      * MF28CB3 - CLAIMS/TRANSACTION PROCESSING MODULE V1.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MF28CB3.
       AUTHOR. APATEL.
       DATE-WRITTEN. 10/06/2020.
      *****************************************************************
      * BUSINESS FUNCTION: THIS PROGRAM IS DESIGNED TO INTERACT DB2
      *                    SYSTEM & PROCESS BELOW FUNCTIONS:
      *                    1. SELECT (RETRIVE) CLAIM DETAILS FROM TABLE
      *                    2. INSERT (ADD) NEW CLAIM DETAILS INTO TABLE
      *
      * PROGRAM TYPE: COBOL-DB2.
      *
      * PROCESSING TYPE: VIA CALLING MODULE
      *
      * BMS: NONE
      *
      * COPYBOOKS: MF28CP1
      *
      * LINKAGE COPYBOOKS: MF28BMS
      *
      * TABLES: MFTR28.CLAIMS (CLAIMS DATA TABLE)
      *
      * CALLING MODULE: MF28CB1 - PROCESS TRANSACTION FROM BMS SCREEN
      *
      * CALLED MODULES: NONE
      *
      * PROGRAMMER: ATUL PATEL
      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
      ***************************************************************
      * This program was used for Data interaction between DB2 tables
      * and CICS screens
      ***************************************************************
       WORKING-STORAGE SECTION.
       01 WS-SQLCODE               PIC -9(03).
       01 WS-DISPLAY-SETTINGS         PIC X(1).
           88 WS-DISPLAY-ALLOWED                  VALUE 'Y'.
           88 WS-NO-DISPLAY-ALLOWED               VALUE 'N'.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           EXEC SQL
               INCLUDE MF28CP1
           END-EXEC.
       LINKAGE SECTION.
           COPY MF28BMS.
       PROCEDURE DIVISION USING MAP1I
                                MAP1O.
      ******************************************************************
      *                  MAIN PARA                                     *
      ******************************************************************
       A1000-MAIN-PARA.
      *---------------*
           EVALUATE SELOPTVI
             WHEN '1'
               PERFORM A2000-SELECT-DATA
             WHEN '2'
               PERFORM A3000-INSERT-DATA
             WHEN OTHER
               MOVE 'OPTION IS NOT CORRECT FROM CB3'         TO MSGO
           END-EVALUATE
           GOBACK.
      ******************************************************************
      *                 FETCH DATA FROM CLAIMS TABLE                   *
      ******************************************************************
       A2000-SELECT-DATA.
      *-----------------*
           MOVE CLMNUMVI                TO CLAIMNUMBER OF CLAIMS
           DISPLAY 'CLAIM NUMBER:' CLMNUMVI
           SET WS-DISPLAY-ALLOWED         TO TRUE
            EXEC SQL
                SELECT  CLAIMDATE,
                        PAID,
                        CVALUE,
                        CAUSE,
                        OBSERVATIONS
                INTO   :CLAIMS.CLAIMDATE,
                       :CLAIMS.PAID,
                       :CLAIMS.CVALUE,
                       :CLAIMS.CAUSE,
                       :CLAIMS.OBSERVATIONS
                FROM   MFTR28.CLAIMS
                WHERE  CLAIMNUMBER = : CLAIMS.CLAIMNUMBER
                END-EXEC.
                DISPLAY 'SQLCODE:' SQLCODE
            EVALUATE SQLCODE
                WHEN 0
                 DISPLAY 'SQLCODE 0:'
      *          MOVE CLAIMDATE OF CLAIMS               TO CLMDATVO
                 MOVE FUNCTION DISPLAY-OF(CLAIMDATE)    TO CLMDATVO
                 MOVE FUNCTION DISPLAY-OF(CAUSE)        TO CAUSEVO
                 MOVE FUNCTION DISPLAY-OF(OBSERVATIONS) TO OBSVO
      *          MOVE CAUSE OF CLAIMS                   TO CAUSEVO
      *          MOVE OBSERVATIONS OF CLAIMS            TO OBSVO
                 MOVE PAID OF CLAIMS                    TO PAIDVO
                 MOVE CVALUE OF CLAIMS                  TO VALVO
                 IF WS-DISPLAY-ALLOWED
                    DISPLAY CLAIMNUMBER OF CLAIMS
                    DISPLAY PAID OF CLAIMS
                    DISPLAY CLMDATVO
                    DISPLAY OBSVO
                    DISPLAY CAUSEVO
                 END-IF
                 MOVE 'CLAIM ENQUIRY SUCCESSFUL'        TO MSGO
                WHEN 100
                    MOVE 'CLAIM NOT FOUND'              TO MSGO
                WHEN OTHER
                    MOVE SQLCODE                      TO WS-SQLCODE
                    STRING 'SQL ERROR IN FETCH - RC : ' WS-SQLCODE
                    DELIMITED BY SIZE INTO MSGO
                    END-STRING
            END-EVALUATE.
      ******************************************************************
      *                 UPDATE THE TABLE THRU SCREEN DATA              *
      ******************************************************************
       A3000-INSERT-DATA.
      *-----------------*
           MOVE CLMNUMVI                      TO CLAIMNUMBER OF CLAIMS
           MOVE PAIDVI                        TO PAID OF CLAIMS
           MOVE VALVI                         TO CVALUE OF CLAIMS
           MOVE CLMDATVI                      TO CLAIMDATE OF CLAIMS
           MOVE CAUSEVI                       TO CAUSE OF CLAIMS
           MOVE OBSVI                         TO OBSERVATIONS OF CLAIMS
           DISPLAY 'INSERT DISPLAYS START:'
           DISPLAY CLAIMNUMBER OF CLAIMS
           DISPLAY PAID OF CLAIMS
           DISPLAY CVALUE OF CLAIMS
           DISPLAY CLAIMDATE OF CLAIMS
           DISPLAY CAUSE OF CLAIMS
           DISPLAY OBSERVATIONS OF CLAIMS
           DISPLAY 'INSERT DISPLAYS END:'
            EXEC SQL
                INSERT  INTO MFTR28.CLAIMS
                       (CLAIMNUMBER,
                        CLAIMDATE,
                        PAID,
                        CVALUE,
                        CAUSE,
                        OBSERVATIONS)
                VALUES (:CLAIMS.CLAIMNUMBER,
                        :CLAIMS.CLAIMDATE,
                        :CLAIMS.PAID,
                        :CLAIMS.CVALUE,
                        :CLAIMS.CAUSE,
                        :CLAIMS.OBSERVATIONS)
            END-EXEC.
            DISPLAY 'SQLCODE INSERT' SQLCODE
            EVALUATE SQLCODE
                WHEN 0
                    MOVE 'CLAIM ADDED SUCCESSFULLY'    TO MSGO
                WHEN -803
                    MOVE 'DUPLICATE INSERT FAILED'     TO MSGO
                WHEN OTHER
                    MOVE 'SQL ERROR IN INSERT'         TO MSGO
            END-EVALUATE.
      ******************************************************************
      *                        END OF PROGRAM                          *
      ******************************************************************