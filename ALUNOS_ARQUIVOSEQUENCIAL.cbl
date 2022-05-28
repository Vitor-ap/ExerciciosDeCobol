      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAREFA_ALUNOS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  ALUNOS ASSIGN TO 'C:/PROGRAMA/alunos.txt'
           ORGANIZATION IS SEQUENTIAL
           ACCESS  MODE IS SEQUENTIAL
           FILE STATUS IS WS-FS-ALUNOS.

           SELECT  ALUNOS2021 ASSIGN TO 'C:/PROGRAMA/alunos2021.txt'
           ORGANIZATION IS SEQUENTIAL
           ACCESS  MODE IS SEQUENTIAL
           FILE STATUS IS WS-FS-ALUNOS21.


       DATA DIVISION.
       FILE SECTION.

           FD ALUNOS.
       01 REG-ALUNOS.
           03  RGM-ALUNO           PIC 9(005).
           03  NOME-ALUNO          PIC A(020).

           FD ALUNOS2021.
       01 REG-ALUNOS2021.
           03  RGM-ALUNO21           PIC 9(005).
           03  NOME-ALUNO21          PIC A(020).
           03  NOTA1-ALUNO21         PIC 9(002)V99.
           03  NOTA2-ALUNO21         PIC 9(002)V99.
           03  MEDIA-ALUNO21         PIC 9(002)V99.
           03  STATUS-ALUNO21        PIC A(010).

       WORKING-STORAGE SECTION.

       01 WS-STATUS.
           03 WS-FS-ALUNOS           PIC 99.
           03 WS-FS-ALUNOS21         PIC 99.

       01 WS-ALUNOS      PIC X(25) VALUE SPACES.
       01 FILLER REDEFINES WS-ALUNOS.
           03  WS-RGM-ALUNO           PIC 9(005).
           03  WS-NOME-ALUNO          PIC A(020).

       01 WS-ALUNOS21     PIC X(47) VALUE SPACES.
       01 FILLER REDEFINES WS-ALUNOS21.
           03  WS-RGM-ALUNO21              PIC 9(005).
           03  WS-NOME-ALUNO21             PIC A(020).
           03  WS-NOTA1-ALUNO21            PIC 9(002)V99.
           03  WS-NOTA2-ALUNO21            PIC 9(002)V99.
           03  WS-MEDIA-ALUNO21            PIC 9(002)V99.
           03  WS-STATUS-ALUNO21           PIC A(010).

       01 WS-CONTADORES.
           03 WS-CONT       PIC 9(3).


       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

       OPEN INPUT ALUNOS
       OPEN EXTEND ALUNOS2021



       PERFORM UNTIL WS-FS-ALUNOS EQUAL 1
           READ ALUNOS INTO WS-ALUNOS
           AT END MOVE 1 TO WS-FS-ALUNOS

           NOT AT END
           ADD 1 TO WS-CONT

           DISPLAY "INSIRA A NOTA 1 DO(A) " WS-RGM-ALUNO
           " - " WS-NOME-ALUNO

           ACCEPT WS-NOTA1-ALUNO21

           DISPLAY "INSIRA A NOTA 2 DO(A) " WS-NOME-ALUNO
           ACCEPT WS-NOTA2-ALUNO21

           COMPUTE WS-MEDIA-ALUNO21 =
           (WS-NOTA1-ALUNO21 + WS-NOTA2-ALUNO21) / 2


           IF WS-MEDIA-ALUNO21 IS >= 6
                   MOVE "APROVADO" TO WS-STATUS-ALUNO21
           ELSE
                   MOVE "REPROVADO" TO WS-STATUS-ALUNO21
           END-IF


                       IF WS-FS-ALUNOS21 EQUAL TO 35 THEN
                       OPEN OUTPUT ALUNOS2021
                       END-IF


                       IF WS-FS-ALUNOS21 EQUAL ZEROS

                            MOVE WS-RGM-ALUNO
                            TO RGM-ALUNO21

                            MOVE WS-NOME-ALUNO
                            TO NOME-ALUNO21

                            MOVE WS-NOTA1-ALUNO21
                            TO NOTA1-ALUNO21

                            MOVE WS-NOTA2-ALUNO21
                            TO NOTA2-ALUNO21

                            MOVE WS-MEDIA-ALUNO21
                            TO MEDIA-ALUNO21

                            MOVE WS-STATUS-ALUNO21
                            TO STATUS-ALUNO21


                        END-IF


           WRITE REG-ALUNOS2021

           END-READ
       END-PERFORM.


           CLOSE ALUNOS
           CLOSE ALUNOS2021

           DISPLAY " "
           DISPLAY " "

           DISPLAY "OK. TOTAL DE REGISTROS GRAVADOS ---> " WS-CONT

           DISPLAY " "
           DISPLAY " "

           OPEN INPUT ALUNOS2021
           PERFORM UNTIL WS-FS-ALUNOS21 EQUAL 1

           READ ALUNOS2021 INTO WS-ALUNOS21

           AT END MOVE 1 TO WS-FS-ALUNOS21

           NOT AT END
           DISPLAY "RGM: " RGM-ALUNO21
           " | NOME: " NOME-ALUNO21
           " | NOTA 1: " NOTA1-ALUNO21
           " | NOTA 2: " NOTA2-ALUNO21
           " | MEDIA:  " MEDIA-ALUNO21
           " | STATUS: " STATUS-ALUNO21

           END-PERFORM

           CLOSE ALUNOS2021


           STOP RUN.
       END PROGRAM TAREFA_ALUNOS.
