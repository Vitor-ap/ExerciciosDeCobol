      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO-1.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 NOME.
           03 FILLER PIC X(15) VALUE "MARIA".
           03 FILLER PIC X(15) VALUE "JOAO".
           03 FILLER PIC X(15) VALUE "LUCAS".
           03 FILLER PIC X(15) VALUE "VITOR".
           03 FILLER PIC X(15) VALUE "CLAUDIA".
           03 FILLER PIC X(15) VALUE "HENRIQUE".
           03 FILLER PIC X(15) VALUE "JOSE".
           03 FILLER PIC X(15) VALUE "CLAUDIO".
           03 FILLER PIC X(15) VALUE "AMANDA".
           03 FILLER PIC X(15) VALUE "ALINE".
           03 WS-NOMEPROCURADO PIC X(15).

       01  FILLER REDEFINES NOME.
           03 WS-NOMES PIC X(15) OCCURS 10 TIMES.

       01  WS-CONT PIC 9(2) VALUE 0.
       01  WS-ACHADO PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "INFORME O NOME A SER PROCURADO"
           ACCEPT WS-NOMEPROCURADO

           PERFORM UNTIL WS-CONT >= 9
           IF WS-NOMEPROCURADO = WS-NOMES(WS-CONT)
               ADD 1 TO WS-ACHADO

           END-IF
           ADD 1 TO WS-CONT
           END-PERFORM

           IF WS-ACHADO >=1
               DISPLAY "ACHEI"
           ELSE
               DISPLAY "NAO ACHEI"
           END-IF


            STOP RUN.
       END PROGRAM EXERCICIO-1.
