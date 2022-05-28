      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO-4.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-VETORES.
           03 WS-VETOR PIC 9(3) OCCURS 20 TIMES.
       01 WS-CONTROLLER.
           03 WS-CONTROLE PIC 9(2) VALUE 1.
           03 WS-POSICAO  PIC 9(2) VALUE 0.
           03 WS-MENOR    PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM UNTIL WS-CONTROLE IS EQUAL TO 21
           DISPLAY "INFORME O PROXIMO NUMERO"

           ACCEPT WS-VETOR(WS-CONTROLE)
               IF WS-VETOR(WS-CONTROLE) IS LESS THAN ZERO THEN
                   DISPLAY "NUMERO INVALIDO, TENTE NOVAMENTE"
                   ACCEPT WS-VETOR(WS-CONTROLE)
               END-IF
           IF WS-VETOR(WS-CONTROLE) IS LESS THAN
               WS-VETOR(WS-CONTROLE - 1)

               MOVE WS-VETOR(WS-CONTROLE) TO WS-MENOR

               MOVE WS-CONTROLE TO WS-POSICAO

           END-IF
           ADD 1 TO WS-CONTROLE
           END-PERFORM


           DISPLAY "MENOR VALOR DO VETOR: " WS-MENOR
           DISPLAY "POSICAO : " WS-POSICAO
           STOP RUN.
       END PROGRAM EXERCICIO-4.
