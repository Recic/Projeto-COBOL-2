      ******************************************************************
      * Author: Renan Cicero
      * Date: 12/02/2023
      * Purpose: Modulo de Alteração do Desafio 3
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGALTR.
       
       
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALUNOS ASSIGN TO 
           '/home/recic/Dev/Cobol/Desafio M3/Dados/ALUNOS.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY ID-ALUNO
           FILE STATUS IS WS-FS.

           SELECT DISCIPLINAS ASSIGN TO 
           '/home/recic/Dev/Cobol/Desafio M3/Dados/DISCIPLINAS.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY ID-DISCIPLINA
           FILE STATUS IS WS-FS.
       
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  ALUNOS.
           COPY 
           '/home/recic/Dev/Cobol/Desafio M3/Dados/FD-ALUNOS.cpy'.
           

       FD  DISCIPLINAS.
           COPY 
           '/home/recic/Dev/Cobol/Desafio M3/Dados/FD-DISCIPLINAS.cpy'.


      ******************************************************************
       WORKING-STORAGE SECTION.

       01  WS-REGISTRO-AL                    PIC X(50) VALUE SPACE.
       01  FILLER REDEFINES WS-REGISTRO-AL.
           03 ID-AL                          PIC 9(03).
           03 NM-AL                          PIC X(20).
           03 TL-AL                          PIC X(20).
       
       01  WS-REGISTRO-DP                    PIC X(50) VALUE SPACE.
       01  FILLER REDEFINES WS-REGISTRO-DP.
           03 ID-DP                          PIC 9(03).
           03 NM-DP                          PIC X(20).
           03 NT-DP                          PIC 9(02)V99.
        
       01  WS-NUM.                            
           03 WS-NUM1                        PIC 9(02)V99.
           03 WS-NUM2                        PIC 9(02)V99.
           03 WS-NUM3                        PIC 9(02)V99.
           03 WS-NUM4                        PIC 9(02)V99.
           03 WS-MD                          PIC 9(02)V99.
           
       77  WS-FS                             PIC 99.
           88 FS-OK                          VALUE 0.     

       77  WS-EXT                            PIC X.
           88 EXT-OK                         VALUE 'F' FALSE 'N'.

       77  WS-CONFIRMA                       PIC X.

       77  SAIDERA                           PIC X.

      ******************************************************************
       LINKAGE SECTION.
       01  LK-COM-AREA.
           03 LK-MENSAGEM                     PIC X(40).
           03 LK-ITEM                         PIC 9.
           

      ******************************************************************
       PROCEDURE DIVISION USING LK-COM-AREA.

           DISPLAY ' '
           DISPLAY '***************************************************'
           DISPLAY 'ALTERACAO DE ' LK-MENSAGEM
           DISPLAY '***************************************************'
           DISPLAY ' '
           SET EXT-OK TO FALSE
           PERFORM P000-ALTERA THRU P000-FIM
           UNTIL WS-EXT = 'F' or 'f'
           PERFORM P400-FIM
           .

       P000-ALTERA.
           EVALUATE LK-ITEM
              WHEN '1'
                 PERFORM P001-ALTERA-ALUNO  
              WHEN '2'
                 PERFORM P002-ALTERA-DISCIPLINA
              WHEN '3'
                 PERFORM P003-ALTERA-NOTA
           END-EVALUATE

           .
       P000-FIM.

           
       P001-ALTERA-ALUNO.
           SET EXT-OK TO TRUE
           SET FS-OK TO TRUE
           MOVE SPACE TO WS-CONFIRMA

           OPEN I-O ALUNOS
       
           IF FS-OK
              DISPLAY  'Informe o ID do aluno que deseja alterar: '
              ACCEPT ID-ALUNO
              
              READ ALUNOS INTO WS-REGISTRO-AL
                 KEY IS ID-ALUNO
                    INVALID KEY 
                       DISPLAY 'O aluno nao existe'
                    NOT INVALID KEY 
                       DISPLAY 'Nome Atual: 'NM-AL
                       DISPLAY 'Telefone Atual: 'TL-AL
                       DISPLAY '*******************************'

                       DISPLAY 'Informe o novo nome: '
                       ACCEPT NM-ALUNO IN REG-ALUNO
                       DISPLAY 'Informe o novo telefone: '
                       ACCEPT TL-ALUNO IN REG-ALUNO
                       DISPLAY 'TECLE: <S> para confirmar ou <QUALQUER '
                               'TECLA> para manter os dados atuais.'
                               ACCEPT WS-CONFIRMA
                               IF WS-CONFIRMA = 'S' OR 's'
                                   REWRITE REG-ALUNO
                                   DISPLAY 'Dados do aluno atualizados.'
                               ELSE 
                                   DISPLAY 'Alteracao nao realizada.'
           ELSE
              DISPLAY 'ERRO AO ABRIR O ARQUIVO DE ALUNOS'
              DISPLAY 'FILE STATUS: ' WS-FS
           END-IF
       
           CLOSE ALUNOS

           DISPLAY ' '
           DISPLAY 'TECLE '   
                   ' <QUALQUER TECLA> para novo cadastro, ou <F> para'
                   ' retornar ao menu.' ACCEPT WS-EXT
           
           .
       P001-FIM.

       P002-ALTERA-DISCIPLINA.
           SET EXT-OK TO TRUE
           SET FS-OK TO TRUE
           MOVE SPACE TO WS-CONFIRMA

           OPEN I-O DISCIPLINAS
       
           IF FS-OK
              DISPLAY  'Informe o ID da disciplina que deseja alterar: '
              ACCEPT ID-DISCIPLINA
              
              READ DISCIPLINAS INTO WS-REGISTRO-DP
                 KEY IS ID-DISCIPLINA
                    INVALID KEY 
                       DISPLAY 'A discplina nao existe'
                    NOT INVALID KEY 
                       DISPLAY 'Nome Atual: 'NM-DP
                       DISPLAY 'Nota minima Atual: 'NT-DP
                       DISPLAY '*******************************'

                       DISPLAY 'Informe o novo nome: '
                       ACCEPT NM-DISCIPLINA IN REG-DISCIPLINA
                       DISPLAY 'Informe a nova nota minima: '
                       ACCEPT NT-DISCIPLINA IN REG-DISCIPLINA
                       DISPLAY 'TECLE: <S> para confirmar ou <QUALQUER '
                               'TECLA> para manter os dados atuais.'
                               ACCEPT WS-CONFIRMA
                               IF WS-CONFIRMA = 'S' OR 's'
                                   REWRITE REG-DISCIPLINA
                                   DISPLAY 'Dados da disciplna'
                                           ' atualizados.'
                               ELSE 
                                   DISPLAY 'Alteracao nao realizada.'
           ELSE
              DISPLAY 'ERRO AO ABRIR O ARQUIVO DE ALUNOS'
              DISPLAY 'FILE STATUS: ' WS-FS
           END-IF
       
           CLOSE DISCIPLINAS

           DISPLAY ' '
           DISPLAY 'TECLE '   
                   ' <QUALQUER TECLA> para novo cadastro, ou <F> para'
                   ' retornar ao menu.' ACCEPT WS-EXT
           
           .
       P002-FIM.

       P003-ALTERA-NOTA.
           DISPLAY 'Opcao Invalida ! Delete a inclusao de nota cadastr'
                   'ada e cadastre novamente'
                   PERFORM P400-FIM
           .
       P003-FIM.

           
       P400-FIM.
           GOBACK.
       END PROGRAM PROGALTR.
