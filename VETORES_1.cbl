      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO-2.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
        01 WS-NOTA.
           03 WS-NOTATURMA PIC 9(2)V99 OCCURS 20 TIMES.

        01 WS-NOTAS.
            03 WS-MEDIA                PIC 9(3)V99.
            03 WS-SOMA                 PIC 9(3)V99.
            03 WS-NOTASACIMADAMEDIA    PIC 9(2).

        01 WS-CONTADOR PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           MOVE 1 TO WS-CONTADOR

           PERFORM UNTIL WS-CONTADOR EQUAL 21
           DISPLAY "INSIRA A NOTA DO ALUNO " WS-CONTADOR

           ACCEPT WS-NOTATURMA(WS-CONTADOR)
           ADD WS-NOTATURMA(WS-CONTADOR) TO WS-SOMA
           ADD 1 TO WS-CONTADOR
           END-PERFORM

           COMPUTE WS-MEDIA = WS-SOMA / 20

           MOVE 1 TO WS-CONTADOR

           PERFORM UNTIL WS-CONTADOR EQUAL 21
               IF WS-NOTATURMA(WS-CONTADOR) > WS-MEDIA
                   ADD 1 TO WS-NOTASACIMADAMEDIA
               END-IF

               ADD 1 TO WS-CONTADOR
           END-PERFORM


           DISPLAY "SOMA : " WS-SOMA

           DISPLAY "MEDIA DA TURMA" WS-MEDIA

           DISPLAY "QTDE ALUNOS ACIMA DA MEDIA" WS-NOTASACIMADAMEDIA


            STOP RUN.
       END PROGRAM EXERCICIO-2.
