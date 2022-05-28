      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO_6.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-VETOR.
       03  WS-VETORES PIC 9(2) OCCURS 10 TIMES.
       03  WS-CONTROLE PIC 9(2) VALUE 1.
       03  WS-CONTROLE2 PIC 9(2) VALUE 1.
       03  WS-PROXIMO  PIC 9(2) VALUE 2.
       03  WS-AUX     PIC 9(2) VALUE 0.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      ***** PERCORRER O VETOR ***
       PERFORM UNTIL WS-CONTROLE IS EQUAL 11
           DISPLAY "DIGITE O NUMERO PARA A POSICAO " WS-CONTROLE
           ACCEPT WS-VETORES(WS-CONTROLE)
           ADD 1 TO WS-CONTROLE
       END-PERFORM

           MOVE 1 TO WS-CONTROLE

      ***** ORDENAR O VETOR *****

       PERFORM VARYING WS-CONTROLE FROM 1 BY 1 UNTIL
           WS-CONTROLE = 10

           PERFORM VARYING WS-PROXIMO FROM WS-CONTROLE BY 1 UNTIL
           WS-PROXIMO > 10


               IF WS-VETORES(WS-PROXIMO) < WS-VETORES(WS-CONTROLE)
                   MOVE WS-VETORES(WS-CONTROLE) TO
                   WS-AUX
                   MOVE WS-VETORES(WS-PROXIMO) TO
                   WS-VETORES(WS-CONTROLE)
                   MOVE WS-AUX TO WS-VETORES(WS-PROXIMO)
               END-IF
           END-PERFORM
       END-PERFORM


      ****EXIBIR VETOR ***
       MOVE 1 TO WS-CONTROLE

       DISPLAY "VETOR ORDENADO..."

       PERFORM UNTIL WS-CONTROLE IS EQUAL 11
       DISPLAY WS-VETORES(WS-CONTROLE)
       ADD 1 TO WS-CONTROLE
       END-PERFORM



       STOP RUN.


       END PROGRAM EXERCICIO_6.
