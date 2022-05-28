      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAMA_EXNOTA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-ALUNO.
           05 WS-NOTA1         PIC 9(2)V99.
           05 WS-NOTA2         PIC 9(2)V99.
           05 WS-SUBSTITUTIVA  PIC 9(2)V99.
           05 WS-MEDIA         PIC 9(2)V99.
           05 WS-FALTAS        PIC 9(3)V99.
           05 WS-MASCARA       PIC Z9,99.
           05 WS-NOME      PIC A(40).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "INFORME O NOME: "
           ACCEPT WS-NOME.


       SECOND-PROCEDURE.
           DISPLAY "INFORME A PRIMEIRA NOTA: "
           ACCEPT WS-NOTA1.
           IF (WS-NOTA1 IS GREATER THAN 5)
               DISPLAY "NOTA INVALIDA"
               GO TO SECOND-PROCEDURE.


       THRID-PROCEDURE.
           DISPLAY "INFORME A SEGUNDA NOTA: "
           ACCEPT WS-NOTA2.
           IF (WS-NOTA2 IS GREATER THAN 5)
               DISPLAY "NOTA INVALIDA"
               GO TO THRID-PROCEDURE.


           DISPLAY "INFORME O NUMERO DE FALTAS: "
           ACCEPT WS-FALTAS.

           COMPUTE WS-MEDIA = WS-NOTA1 + WS-NOTA2.

           IF (WS-MEDIA IS LESS THAN 6) AND
               ((WS-NOTA1 IS GREATER THAN OR EQUAL TO 1) OR
                  (WS-NOTA2 IS GREATER THAN OR EQUAL TO 1))

               DISPLAY "INFORME A NOTA SUB "
               ACCEPT WS-SUBSTITUTIVA

               IF WS-NOTA1 IS GREATER WS-NOTA2
                   MOVE WS-SUBSTITUTIVA TO WS-NOTA2
               ELSE
                   MOVE WS-SUBSTITUTIVA TO WS-NOTA1
               END-IF

               COMPUTE WS-MEDIA = WS-NOTA1 + WS-NOTA2
           END-IF.

               IF ((WS-MEDIA IS GREATER OR EQUAL TO 6)
                   AND (WS-FALTAS IS LESS OR EQUAL TO 20))
                   DISPLAY "ALUNO APROVADO"

               ELSE IF ((WS-MEDIA IS GREATER OR EQUAL TO 6)
                   AND (WS-FALTAS IS GREATER THAN 20))
                   DISPLAY "REPROVADO POR FALTA"

               ELSE IF ((WS-MEDIA IS LESS THAN 6)
                   AND (WS-FALTAS IS LESS OR EQUAL TO 20))
                   DISPLAY "REPROVADO POR NOTA"

               ELSE IF ((WS-MEDIA IS LESS THAN 6)
                    AND (WS-FALTAS IS GREATER THAN 20))
                   DISPLAY "REPROVADO POR NOTA E FALTA"
            STOP RUN.
       END PROGRAM PROGRAMA_EXNOTA.
