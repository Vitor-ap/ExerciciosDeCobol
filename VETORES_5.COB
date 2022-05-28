      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO-6.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-VETORES.
           03 WS-VETOR           PIC  9(3) OCCURS 20.


       01 WS-CONTROLLER.

           03 WS-CONTADOR          PIC 9(2) VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL WS-CONTADOR IS EQUAL TO 21
           DISPLAY "DIGITE O NUMERO  DA POSICAO " WS-CONTADOR
           ACCEPT WS-VETOR(WS-CONTADOR)
           ADD 1 TO WS-CONTADOR
           END-PERFORM


           DISPLAY "VALORES NA ORDEM INVERSA SAO..."
           MOVE 20  TO WS-CONTADOR
           PERFORM UNTIL WS-CONTADOR IS EQUAL TO 0

           DISPLAY  WS-VETOR(WS-CONTADOR)
           SUBTRACT 1 FROM WS-CONTADOR

           END-PERFORM



            STOP RUN.
       END PROGRAM EXERCICIO-6.
