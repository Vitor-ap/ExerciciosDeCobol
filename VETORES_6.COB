      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO-5.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-VETORES.
           03 WS-VETOR-A           PIC  9(3) OCCURS 10.
           03 WS-VETOR-M           PIC  9(3) OCCURS 10.

       01 WS-CONTROLLER.
           03 WS-X                 PIC 9(4) VALUE 0.
           03 WS-CONTADOR          PIC 9(2) VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL WS-CONTADOR IS EQUAL TO 11
           DISPLAY "DIGITE O NUMERO  DA POSICAO " WS-CONTADOR
           ACCEPT WS-VETOR-A(WS-CONTADOR)
           ADD 1 TO WS-CONTADOR
           END-PERFORM

           DISPLAY "INFORME O NUMERO PARA MULTIPLICAR"
           " OS DADOS DO VETOR "
           ACCEPT WS-X

           DISPLAY "VETOR MULTIPLICADO..."
           MOVE 1  TO WS-CONTADOR
           PERFORM UNTIL WS-CONTADOR IS EQUAL TO 11
           COMPUTE WS-VETOR-M(WS-CONTADOR) = WS-VETOR-A(WS-CONTADOR) *
           WS-X
           DISPLAY "POSICAO " WS-CONTADOR ": " WS-VETOR-M(WS-CONTADOR)
           ADD 1 TO WS-CONTADOR

           END-PERFORM



            STOP RUN.
       END PROGRAM EXERCICIO-5.
