

      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATIV-CL-PRODT-VENDA.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CLIENTES ASSIGN TO
               'C:\COBOL ATIV LOJA\clientes.txt'
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS CODIGO-CLIENTE
           FILE STATUS IS WS-FS.


       SELECT PRODUTOS ASSIGN TO
               'C:\COBOL ATIV LOJA\produtos.txt'
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS CODIGO-PRODUTO
           FILE STATUS IS WS-FS.


       SELECT PEDIDOS ASSIGN TO
               'C:\COBOL ATIV LOJA\pedidos-vendas.txt'
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS CODIGO-PEDIDO
           FILE STATUS IS WS-FS.


       DATA DIVISION.
       FILE SECTION.

       FD CLIENTES.
       01 REG-CLIENTE.
           03 CODIGO-CLIENTE     PIC 9(03).
           03 NOME               PIC X(25).
           03 RG                 PIC X(15).
           03 TELEFONE                         PIC X(13).

       FD PRODUTOS.
       01 REG-PRODUTO.

           03 CODIGO-PRODUTO       PIC 9(03).
           03 PRODUTO              PIC X(25).
           03 PRECO                PIC 9(06)V99.

       FD PEDIDOS.
       01 REG-PEDIDO.
           03 CODIGO-PEDIDO                PIC 9(03).
           03 PV-CODIGO-CLIENTE            PIC 9(03).
           03 PV-NOME                      PIC X(25).
           03 PV-RG                        PIC X(15).
           03 PV-TELEFONE                  PIC X(13).
           03 PV-CODIGO-PRODUTO            PIC 9(03).
           03 PV-PRODUTO                   PIC X(25).
           03 PV-PRECO                     PIC 9(06)V99.
           03 PV-QTDE                      PIC 9(03).
           03 PV-VALOR-TOTAL               PIC 9(09)V99.

       WORKING-STORAGE SECTION.
       77 WS-FS            PIC 99.
       77 WS-ALTERA        PIC X VALUE SPACES.
       77 WS-EXCLUI        PIC X VALUE SPACES.
       77 WS-OPC           PIC X(1).



       01 WS-CLIENTE.
           03 WS-CODIGO-CLIENTE     PIC 9(03).
           03 WS-NOME               PIC X(25).
           03 WS-RG                 PIC X(15).
           03 WS-TELEFONE           PIC X(13).

       01 WS-PRODUTOS.
           03 WS-CODIGO-PRODUTO       PIC 9(03).
           03 WS-PRODUTO              PIC X(25).
           03 WS-PRECO                PIC 9(06)V99.


       01 WS-PEDIDOS.
           03 WS-CODIGO-PEDIDO                PIC 9(03).
           03 WS-PV-CODIGO-CLIENTE            PIC 9(03).
           03 WS-PV-NOME                      PIC X(25).
           03 WS-PV-RG                        PIC X(15).
           03 WS-PV-TELEFONE                  PIC X(13).
           03 WS-PV-CODIGO-PRODUTO            PIC 9(03).
           03 WS-PV-PRODUTO                   PIC X(25).
           03 WS-PV-PRECO                     PIC 9(06)V99.
           03 WS-PV-QTDE                      PIC 9(03).
           03 WS-PV-VALOR-TOTAL               PIC 9(09)V99.

       PROCEDURE DIVISION.

       MENU-INICIAL-PROCEDURE.

           DISPLAY '-----PROGRAMA CRUD EM COBOL -----'

            DISPLAY 'SELECIONE A OPCAO:'

            DISPLAY '1 - CADASTRO E CLIENTES'

            DISPLAY '2 - CADASTRO DE PRODUTOS'

            DISPLAY '3 - CADASTRO DE PEDIDO DE VENDAS'

            DISPLAY '0 - ENCERRAR APLICACAO'



            DISPLAY 'DIGITE A OPCAO: (1, 2 ou 3) ______'

            ACCEPT WS-OPC

            PERFORM UNTIL WS-OPC IS EQUAL TO 0

                IF WS-OPC IS EQUAL TO 1
                    GO TO MENU-CLIENTES-PROCEDURE
                    ELSE IF WS-OPC IS EQUAL TO 2
                        GO TO MENU-PRODUTOS-PROCEDURE
                        ELSE IF WS-OPC IS EQUAL TO 3
                            GO TO MENU-PEDIDOS-PROCEDURE
                            ELSE
                                DISPLAY 'OPCAO INVALIDA'
                                GO TO MENU-INICIAL-PROCEDURE
                END-IF
            END-PERFORM
               DISPLAY 'APLICACAO FINALIZADA!'
               STOP RUN.

       MENU-CLIENTES-PROCEDURE.


           DISPLAY ' '
           DISPLAY '---------MENU CLIENTES     -------------'
           DISPLAY 'I - INCLUSAO DE DADOS DOS CLIENTES'
           DISPLAY 'A - ALTERACAO DE DADOS DOS CLIENTES'
           DISPLAY 'C - CONSULTAR DE DADOS DOS CLIENTES'
           DISPLAY 'E - EXCLUSAO DE DADOS DOS CLIENTES'
           DISPLAY 'V - VOLTAR AO MENU PRINCIPAL'
           DISPLAY 'DIGITE A OPCAO I,A,C OU E... OU V PARA VOLTAR'


           ACCEPT WS-OPC

           PERFORM UNTIL WS-OPC IS EQUAL TO 'V'


               IF WS-OPC IS EQUAL TO 'I'
               GO TO INCLUIR-CLIENTES
                   ELSE IF WS-OPC IS EQUAL TO 'C'
               GO TO LER-CLIENTES
                   ELSE IF WS-OPC IS EQUAL TO 'A'
               GO TO ALTERA-CLIENTES
                   ELSE IF WS-OPC IS EQUAL TO 'E'
               GO TO EXCLUI-CLIENTES
               ELSE
                   DISPLAY 'OPCAO INVALIDA'
                   GO TO MENU-CLIENTES-PROCEDURE
               END-IF

            END-PERFORM

            GO TO MENU-INICIAL-PROCEDURE.

       INCLUIR-CLIENTES.
           SET WS-FS      TO 0.
            OPEN I-O CLIENTES
            IF WS-FS EQUAL 35 THEN
                OPEN OUTPUT CLIENTES
            END-IF
                IF WS-FS EQUAL ZEROS
                DISPLAY 'INFORME O CODIGO DO CLIENTE:'
                ACCEPT CODIGO-CLIENTE
                DISPLAY 'INFORME O NOME DO CLIENTE'
                ACCEPT NOME
                DISPLAY 'INFORME O RG DO CLIENTE: '
                ACCEPT RG
                DISPLAY 'INFORME O TELEFONE DO CLIENTE: '
                ACCEPT TELEFONE

                WRITE REG-CLIENTE

                IF WS-FS NOT EQUAL ZEROS
                    DISPLAY 'ERRO - NÃO FOI POSSIVEL GRAVAR O REGISTRO'
                    DISPLAY 'FILE STATUS: ' WS-FS
                ELSE
                    DISPLAY 'REGISTRO GRAVADO COM SUCESSO!'
                END-IF
             ELSE
                DISPLAY 'ERRO AO CRIAR O ARQUIVO'
                DISPLAY 'FILE STATUS: ' WS-FS
             END-IF
             CLOSE CLIENTES
             GO TO MENU-CLIENTES-PROCEDURE.

       LER-CLIENTES.
           OPEN I-O CLIENTES.

            DISPLAY 'INFORME O CODIGO DO CLIENTE'
            ACCEPT CODIGO-CLIENTE

            READ CLIENTES RECORD INTO WS-CLIENTE
              KEY IS CODIGO-CLIENTE
                   INVALID KEY
                       DISPLAY 'CODIGO DO CLIENTE INVALIDO'
                   NOT INVALID KEY
                       DISPLAY 'CODIGO DO CLIENTE: ' WS-CODIGO-CLIENTE
                       DISPLAY 'NOME DO CLIENTE: ' WS-NOME
                       DISPLAY 'RG: ' WS-RG
                       DISPLAY 'TELEFONE: ' WS-TELEFONE
            END-READ.

            CLOSE CLIENTES
            GO TO MENU-CLIENTES-PROCEDURE.



            ALTERA-CLIENTES.

            OPEN I-O CLIENTES.

            DISPLAY 'INFORME O CODIGO DO CLIENTE'
            ACCEPT CODIGO-CLIENTE

            READ CLIENTES RECORD INTO WS-CLIENTE
              KEY IS CODIGO-CLIENTE
                   INVALID KEY
                       DISPLAY 'CODIGO DO CLIENTE INVALIDO'
                   NOT INVALID KEY
                       DISPLAY 'CODIGO DO CLIENTE: ' WS-CODIGO-CLIENTE
                       DISPLAY 'NOME DO CLIENTE: ' WS-NOME
                       DISPLAY 'RG: ' WS-RG
                       DISPLAY 'TELEFONE: ' WS-TELEFONE
                       MOVE 'S' TO WS-ALTERA
            END-READ.

                IF WS-ALTERA IS EQUAL TO 'S'
                    DISPLAY "INFORME O NOVO NOME DO CLIENTE"
                    ACCEPT NOME
                    DISPLAY "INFORME O NUMERO DO RG:"
                    ACCEPT RG
                    DISPLAY "INFORME O NUMERO DO TELEFONE:"
                    ACCEPT TELEFONE
                    DISPLAY 'REGISTRO ALTERADO'
                    REWRITE REG-CLIENTE
                    END-REWRITE
                END-IF


            CLOSE PRODUTOS
            GO TO MENU-CLIENTES-PROCEDURE.

       EXCLUI-CLIENTES.
           OPEN I-O CLIENTES.

            DISPLAY 'INFORME O CODIGO DO CLIENTE'
            ACCEPT CODIGO-CLIENTE

            READ CLIENTES RECORD INTO WS-CLIENTE
              KEY IS CODIGO-CLIENTE
                   INVALID KEY
                       DISPLAY 'CODIGO DO CLIENTE INVALIDO'
                   NOT INVALID KEY
                       DISPLAY 'CODIGO DO CLIENTE: ' WS-CODIGO-CLIENTE
                       DISPLAY 'NOME DO CLIENTE: ' WS-NOME
                       MOVE 'S' TO WS-EXCLUI
            END-READ.

                IF WS-EXCLUI IS EQUAL TO 'S'
                    DELETE CLIENTES RECORD
                    INVALID KEY DISPLAY 'CODIGO DO CLIENTE INVALIDO'
                    NOT INVALID KEY DISPLAY 'REGISTRO DELETADO!'
                    END-DELETE

                END-IF


            CLOSE PRODUTOS
            GO TO MENU-CLIENTES-PROCEDURE.

       MENU-PRODUTOS-PROCEDURE.
           DISPLAY ' '
           DISPLAY '---------MENU PRODUTOS -------------'
           DISPLAY 'I - INCLUSAO DE DADOS DOS PRODUTOS'
           DISPLAY 'A - ALTERACAO DE DADOS DOS PRODUTOS'
           DISPLAY 'C - CONSULTAR DE DADOS DOS PRODUTOS'
           DISPLAY 'E - EXCLUSAO DE DADOS DOS PRODUTOS'
           DISPLAY 'V - VOLTAR AO MENU PRINCIPAL'
           DISPLAY 'DIGITE A OPCAO I,A,C OU E... OU V PARA VOLTAR'

           ACCEPT WS-OPC

           PERFORM UNTIL WS-OPC IS EQUAL TO 'V'


               IF WS-OPC IS EQUAL TO 'I'
               GO TO INCLUIR-PRODUTOS
               ELSE IF WS-OPC IS EQUAL TO 'C'
               GO TO LER-PRODUTOS
               ELSE IF WS-OPC IS EQUAL TO 'A'
               GO TO ALTERA-PRODUTOS
               ELSE IF WS-OPC IS EQUAL TO 'E'
               GO TO EXCLUI-PRODUTOS
               ELSE
               DISPLAY 'OPCAO INVALIDA'
               GO TO MENU-PRODUTOS-PROCEDURE
               END-IF

           END-PERFORM

           GO TO MENU-INICIAL-PROCEDURE.

       INCLUIR-PRODUTOS.
           SET WS-FS TO 0.
           OPEN I-O PRODUTOS
           IF WS-FS EQUAL 35 THEN
           OPEN OUTPUT PRODUTOS
           END-IF
           IF WS-FS EQUAL ZEROS
           DISPLAY 'INFORME O CODIGO DO PRODUTO:'
           ACCEPT CODIGO-PRODUTO
           DISPLAY 'INFORME O NOME DO PRODUTO'
           ACCEPT PRODUTO
           DISPLAY 'INFORME O PRECO DO PRODUTO: '
           ACCEPT PRECO


           WRITE REG-PRODUTO

               IF WS-FS NOT EQUAL ZEROS
           DISPLAY 'ERRO - NÃO FOI POSSIVEL GRAVAR O REGISTRO'
           DISPLAY 'FILE STATUS: ' WS-FS
           ELSE
           DISPLAY 'REGISTRO GRAVADO COM SUCESSO!'
           END-IF
           ELSE
           DISPLAY 'ERRO AO CRIAR O ARQUIVO'
           DISPLAY 'FILE STATUS: ' WS-FS
           END-IF
           CLOSE PRODUTOS
           GO TO MENU-PRODUTOS-PROCEDURE.

       LER-PRODUTOS.
           OPEN I-O PRODUTOS.

           DISPLAY 'INFORME O CODIGO DO PRODUTO'
           ACCEPT CODIGO-PRODUTO

               READ PRODUTOS RECORD INTO WS-PRODUTOS
           KEY IS CODIGO-PRODUTO
           INVALID KEY
           DISPLAY 'CODIGO DO PRODUTO INVALIDO'
           NOT INVALID KEY
           DISPLAY 'CODIGO DO PRODUTO: ' WS-CODIGO-PRODUTO
           DISPLAY 'NOME DO PRODUTO: ' WS-PRODUTO
           DISPLAY 'PRECO: ' WS-PRECO

               END-READ.

               CLOSE PRODUTOS
           GO TO MENU-PRODUTOS-PROCEDURE.

       ALTERA-PRODUTOS.

           OPEN I-O PRODUTOS.

           DISPLAY 'INFORME O CODIGO DO PRODUTO'
           ACCEPT CODIGO-PRODUTO

               READ PRODUTOS RECORD INTO WS-PRODUTOS
               KEY IS CODIGO-PRODUTO
               INVALID KEY
               DISPLAY 'CODIGO DO PRODUTO INVALIDO'
               NOT INVALID KEY
               DISPLAY 'CODIGO DO PRODUTO: ' WS-CODIGO-PRODUTO
               DISPLAY 'NOME DO PRODUTO: ' WS-PRODUTO
               DISPLAY 'PRECO: ' WS-PRECO

                   MOVE 'S' TO WS-ALTERA
               END-READ.

                   IF WS-ALTERA IS EQUAL TO 'S'
               DISPLAY "INFORME O NOVO NOME DO PRODUTO"
               ACCEPT PRODUTO
               DISPLAY "INFORME O NOVO PRECO:"
               ACCEPT PRECO

                   DISPLAY 'REGISTRO ALTERADO!'
               REWRITE REG-PRODUTO
               END-REWRITE
               END-IF


               CLOSE PRODUTOS
               GO TO MENU-PRODUTOS-PROCEDURE.

       EXCLUI-PRODUTOS.
           OPEN I-O PRODUTOS.

           DISPLAY 'INFORME O CODIGO DO PRODUTO'
            ACCEPT CODIGO-PRODUTO

           READ PRODUTOS RECORD INTO WS-PRODUTOS
           KEY IS CODIGO-PRODUTO
           INVALID KEY
           DISPLAY 'CODIGO DO PRODUTO INVALIDO'
           NOT INVALID KEY
           DISPLAY 'CODIGO DO PRODTUTO: ' WS-CODIGO-PRODUTO
           DISPLAY 'NOME DO PRODUTO: ' WS-PRODUTO
           MOVE 'S' TO WS-EXCLUI
           END-READ.

           IF WS-EXCLUI IS EQUAL TO 'S'
           DELETE PRODUTOS RECORD
           INVALID KEY DISPLAY 'CODIGO DO PRODUTO INVALIDO'
           NOT INVALID KEY DISPLAY 'REGISTRO DELETADO!'
           END-DELETE

           END-IF

           CLOSE PRODUTOS
           GO TO MENU-PRODUTOS-PROCEDURE.

       MENU-PEDIDOS-PROCEDURE.
           DISPLAY ' '
           DISPLAY '---------MENU PEDIDOS -------------'
           DISPLAY 'I - INCLUSAO DE DADOS DOS PEDIDOS'
           DISPLAY 'A - ALTERACAO DE DADOS DOS PEDIDOS'
           DISPLAY 'C - CONSULTAR DE DADOS DOS PEDIDOS'
           DISPLAY 'E - EXCLUSAO DE DADOS DOS PEDIDOS'
           DISPLAY 'V - VOLTAR AO MENU PRINCIPAL'
           DISPLAY 'DIGITE A OPCAO I,A,C OU E... OU V PARA VOLTAR'

           ACCEPT WS-OPC

           PERFORM UNTIL WS-OPC IS EQUAL TO 'V'


               IF WS-OPC IS EQUAL TO 'I'
               GO TO INCLUIR-PEDIDOS
               ELSE IF WS-OPC IS EQUAL TO 'C'
               GO TO LER-PEDIDOS
               ELSE IF WS-OPC IS EQUAL TO 'A'
               GO TO ALTERA-PEDIDOS
               ELSE IF WS-OPC IS EQUAL TO 'E'
               GO TO EXCLUI-PEDIDOS
               ELSE
               DISPLAY 'OPCAO INVALIDA'
               GO TO MENU-PEDIDOS-PROCEDURE
               END-IF

           END-PERFORM

           GO TO MENU-INICIAL-PROCEDURE.

       INCLUIR-PEDIDOS.
           SET WS-FS TO 0.
           OPEN I-O PEDIDOS
           IF WS-FS EQUAL 35 THEN
           OPEN OUTPUT PEDIDOS
           END-IF
           IF WS-FS EQUAL ZEROS
           DISPLAY 'INFORME O CODIGO DO PEDIDO: '
           ACCEPT CODIGO-PEDIDO
           DISPLAY 'INFORME O CODIGO DO CLIENTE: '
           ACCEPT PV-CODIGO-CLIENTE
           DISPLAY 'INFORME O NOME  DO CLIENTE: '
           ACCEPT PV-NOME
           DISPLAY 'INFORME O RG DO CLIENTE: '
           ACCEPT PV-RG
           DISPLAY 'INFORME O TELEFONE DO CLIENTE: '
           ACCEPT PV-TELEFONE
           DISPLAY 'INFORME O CODIGO DO PRODUTO DO PEDIDO: '
           ACCEPT PV-CODIGO-PRODUTO
           DISPLAY 'INFORME O NOME DO PRODUTO DO PEDIDO: '
           ACCEPT PV-PRODUTO
           DISPLAY 'INFORME O PRECO DO PRODUTO: '
           ACCEPT PV-PRECO
           DISPLAY 'INFORME A QUANTIDADE'
           ACCEPT PV-QTDE
           COMPUTE PV-VALOR-TOTAL = PV-PRECO * PV-QTDE
           DISPLAY 'VALOR TOTAL: ' PV-VALOR-TOTAL

           WRITE REG-PEDIDO

           IF WS-FS NOT EQUAL ZEROS
               DISPLAY 'ERRO - NÃO FOI POSSIVEL GRAVAR O REGISTRO'
               DISPLAY 'FILE STATUS: ' WS-FS
               ELSE
               DISPLAY 'REGISTRO GRAVADO COM SUCESSO!'
               END-IF
               ELSE
               DISPLAY 'ERRO AO CRIAR O ARQUIVO'
               DISPLAY 'FILE STATUS: ' WS-FS
           END-IF
           CLOSE PEDIDOS
           GO TO MENU-PEDIDOS-PROCEDURE.

       LER-PEDIDOS.
           OPEN I-O PEDIDOS.

           DISPLAY 'INFORME O CODIGO DO PEDIDO'
           ACCEPT CODIGO-PEDIDO

           READ PEDIDOS RECORD INTO WS-PEDIDOS
               KEY IS CODIGO-PEDIDO
               INVALID KEY
               DISPLAY 'CODIGO DO PEDIDO INVALIDO'
               NOT INVALID KEY
               DISPLAY 'PEDIDO #' CODIGO-PEDIDO
               DISPLAY 'CLIENTE: ' PV-CODIGO-CLIENTE ' ' PV-NOME
               ' RG: ' PV-RG ' TELEFONE: ' PV-TELEFONE
               DISPLAY 'PRODUTO #'PV-CODIGO-PRODUTO ' 'PV-PRODUTO
               ' R$ 'PV-PRECO
               DISPLAY 'QTDE: 'PV-QTDE' - VALOR TOTAL DO PEDIDO: '
               PV-VALOR-TOTAL
           END-READ.

               CLOSE PEDIDOS
           GO TO MENU-PEDIDOS-PROCEDURE.

       ALTERA-PEDIDOS.

           OPEN I-O PEDIDOS.

           DISPLAY 'INFORME O CODIGO DO PEDIDO'
           ACCEPT CODIGO-PEDIDO

               READ PEDIDOS RECORD INTO WS-PEDIDOS
               KEY IS CODIGO-PEDIDO
               INVALID KEY
               DISPLAY 'CODIGO DO PEDIDO INVALIDO'
               NOT INVALID KEY
               DISPLAY 'PEDIDO #' CODIGO-PEDIDO
               DISPLAY 'CLIENTE: ' PV-CODIGO-CLIENTE ' ' PV-NOME
               ' RG: ' PV-RG ' TELEFONE: ' PV-TELEFONE
               DISPLAY 'PRODUTO #'PV-CODIGO-PRODUTO ' 'PV-PRODUTO
               ' R$ 'PV-PRECO
               DISPLAY 'QTDE: 'PV-QTDE' - VALOR TOTAL DO PEDIDO: '
               PV-VALOR-TOTAL

                   MOVE 'S' TO WS-ALTERA
               END-READ.

              IF WS-ALTERA IS EQUAL TO 'S'
                   DISPLAY 'INFORME O CODIGO DO CLIENTE: '
                   ACCEPT PV-CODIGO-CLIENTE
                   DISPLAY 'INFORME O NOME  DO CLIENTE: '
                   ACCEPT PV-NOME
                   DISPLAY 'INFORME O RG DO CLIENTE: '
                   ACCEPT PV-RG
                   DISPLAY 'INFORME O TELEFONE DO CLIENTE: '
                   ACCEPT PV-TELEFONE
                   DISPLAY 'INFORME O CODIGO DO PRODUTO DO PEDIDO: '
                   ACCEPT PV-CODIGO-PRODUTO
                   DISPLAY 'INFORME O NOME DO PRODUTO DO PEDIDO: '
                   ACCEPT PV-PRODUTO
                   DISPLAY 'INFORME O PRECO DO PRODUTO: '
                   ACCEPT PV-PRECO
                   DISPLAY 'INFORME A QUANTIDADE'
                   ACCEPT PV-QTDE
                   COMPUTE PV-VALOR-TOTAL = PV-PRECO * PV-QTDE
                   DISPLAY 'VALOR TOTAL: ' PV-VALOR-TOTAL

                   DISPLAY 'REGISTRO ALTERADO!'

               REWRITE REG-PEDIDO
               END-REWRITE
               END-IF

               CLOSE PEDIDOS
               GO TO MENU-PEDIDOS-PROCEDURE.

       EXCLUI-PEDIDOS.
           OPEN I-O PEDIDOS.

           DISPLAY 'INFORME O CODIGO DO PEDIDO'
            ACCEPT CODIGO-PEDIDO

           READ PEDIDOS RECORD INTO WS-PEDIDOS
           KEY IS CODIGO-PEDIDO
           INVALID KEY
           DISPLAY 'CODIGO DO PEDIDO INVALIDO'
           NOT INVALID KEY
           DISPLAY 'PEDIDO #' CODIGO-PEDIDO
               DISPLAY 'CLIENTE: ' PV-CODIGO-CLIENTE ' ' PV-NOME
               ' RG: ' PV-RG ' TELEFONE: ' PV-TELEFONE
               DISPLAY 'PRODUTO #'PV-CODIGO-PRODUTO ' 'PV-PRODUTO
               ' R$ 'PV-PRECO
               DISPLAY 'QTDE: 'PV-QTDE' - VALOR TOTAL DO PEDIDO: '
               PV-VALOR-TOTAL

           MOVE 'S' TO WS-EXCLUI
           END-READ.

           IF WS-EXCLUI IS EQUAL TO 'S'
           DELETE PEDIDOS RECORD
           INVALID KEY DISPLAY 'CODIGO DO PEDIDO INVALIDO'
           NOT INVALID KEY DISPLAY 'REGISTRO DELETADO!'
           END-DELETE

           END-IF

           CLOSE PEDIDOS
           GO TO MENU-PEDIDOS-PROCEDURE.

            STOP RUN.
            END PROGRAM ATIV-CL-PRODT-VENDA.
