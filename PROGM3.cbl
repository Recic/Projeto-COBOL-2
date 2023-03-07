      ******************************************************************
      * Author: Renan Cicero
      * Date: 12/02/2023
      * Purpose: Desafio modulo 3 
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGM3.
       
       
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. 
           DECIMAL-POINT IS COMMA.


      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       
       
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WS-COM-AREA.
           03 WS-MENSAGEM                        PIC X(40).
           03 WS-TIPO                            PIC 9.

       77  WS-TITULO                             PIC X(40).

       77  WS-OPCAO                              PIC X.
       
       77  WS-EXT                                PIC X.
       
      ******************************************************************
       PROCEDURE DIVISION.

           PERFORM P100-MENU-INICIAL UNTIL WS-OPCAO = 'F'
           PERFORM P500-ENCERRAMENTO
           .
       P100-MENU-INICIAL.

           MOVE SPACE TO WS-OPCAO
           MOVE 0 TO WS-TIPO

           DISPLAY ' '
           DISPLAY '***************************************************'
           DISPLAY '****            SISTEMA ESCOLAR                ****'
           DISPLAY '***************************************************'
           DISPLAY '|                                                 |'
           DISPLAY '|           1 - Menu do Aluno                     |'
           DISPLAY '|           2 - Menu de Discplina                 |'
           DISPLAY '|           3 - Menu de Notas                     |'
           DISPLAY '|                                                 |'        
           DISPLAY '|                                                 |'
           DISPLAY '|           F - ENCERRAR                          |'
           DISPLAY '|                                                 |'
           DISPLAY '***************************************************'
           DISPLAY ' Selecione uma opcao: ' ACCEPT WS-OPCAO     

           EVALUATE WS-OPCAO
              WHEN '1'
                 MOVE 'ALUNOS' TO WS-TITULO
                 MOVE 1 TO WS-TIPO
                 PERFORM P200-MENU-DADOS

              WHEN '2'
                 MOVE 'DISCIPLINAS' TO WS-TITULO
                 MOVE 2 TO WS-TIPO
                 PERFORM P200-MENU-DADOS

              WHEN '3'
                 MOVE 'NOTAS' TO WS-TITULO
                 MOVE 3 TO WS-TIPO
                 PERFORM P200-MENU-DADOS

              WHEN 'F'
                 DISPLAY 'Ate a Proxima'
                 STOP RUN

              WHEN 'f'
                 DISPLAY 'Ate a Proxima'
                 STOP RUN

              WHEN OTHER 
                 DISPLAY 'OPCAO INVALIDA !'
           END-EVALUATE 
           .

       P200-MENU-DADOS.
           MOVE SPACE TO WS-OPCAO
           DISPLAY ' '
           DISPLAY '***************************************************'
           DISPLAY  'MENU DE 'WS-TITULO
           DISPLAY '***************************************************'
           DISPLAY '|                                                 |'
           DISPLAY '|           1 - Cadastrar                         |'
           DISPLAY '|           2 - Listar                            |'
           DISPLAY '|           3 - Consutar                          |'
           DISPLAY '|           4 - Alterar                           |' 
           DISPLAY '|           5 - Excluir                           |'        
           DISPLAY '|                                                 |'
           DISPLAY '|           F - Retornar ao Meno do Aluno         |'
           DISPLAY '|                                                 |'
           DISPLAY '***************************************************'
           DISPLAY ' Selecione uma opcao: ' ACCEPT WS-OPCAO

            EVALUATE WS-OPCAO
              WHEN '1'
                 MOVE WS-TITULO TO WS-MENSAGEM
                 CALL 
                 '/home/recic/Dev/Cobol/Desafio M3/Modulos/PROGINCL' 
                 USING WS-COM-AREA

              WHEN '2'
                 MOVE WS-TITULO TO WS-MENSAGEM  
                 CALL 
                 '/home/recic/Dev/Cobol/Desafio M3/Modulos/PROGLIST' 
                 USING WS-COM-AREA

              WHEN '3'
                 MOVE WS-TITULO TO WS-MENSAGEM
                 CALL 
                 '/home/recic/Dev/Cobol/Desafio M3/Modulos/PROGCONS' 
                 USING WS-COM-AREA

              WHEN '4'
                 MOVE WS-TITULO TO WS-MENSAGEM
                 CALL 
                 '/home/recic/Dev/Cobol/Desafio M3/Modulos/PROGALTR' 
                 USING WS-COM-AREA

              WHEN '5'
                 MOVE WS-TITULO TO WS-MENSAGEM
                 CALL 
                 '/home/recic/Dev/Cobol/Desafio M3/Modulos/PROGEXCL' 
                 USING WS-COM-AREA
              WHEN 'F'
                 PERFORM P100-MENU-INICIAL
              WHEN 'f'
                 PERFORM P100-MENU-INICIAL
              WHEN OTHER 
                 DISPLAY 'OPCAO INVALIDA !'
           END-EVALUATE
           .
     
 
       P500-ENCERRAMENTO.  
           STOP RUN.
       END PROGRAM PROGM3.
           