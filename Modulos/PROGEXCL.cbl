      ******************************************************************
      * Author: Renan Cicero
      * Date: 01/02/2023
      * Purpose: Modulo de Exlusão do Desafio 3
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGEXCL.
       
       
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

           SELECT NOTAS ASSIGN TO 
           '/home/recic/Dev/Cobol/Desafio M3/Dados/NOTAS.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM 
           RECORD KEY ID-INCLUSAO
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

       FD  NOTAS.
           COPY 
           '/home/recic/Dev/Cobol/Desafio M3/Dados/FD-NOTAS.cpy'.
           

      ******************************************************************
       WORKING-STORAGE SECTION.
       
       01  WS-REGISTRO                      PIC X(50) VALUE SPACE.
       01  FILLER REDEFINES WS-REGISTRO.
           03 WS-ID                         PIC 9(03).
           03 WS-NM                         PIC X(20).
           03 WS-DP                         PIC X(20).
           03 WS-NT                         PIC 9(02)V99.
           03 WS-ST                         PIC X(10).

       77  WS-FS                            PIC 99.
           88 FS-OK                        VALUE 0.     

       77  WS-EXT                           PIC X.
           88 EXT-OK              VALUE 'F' FALSE 'N'.

       77  WS-CONFIRMA                      PIC X.

      ******************************************************************
       LINKAGE SECTION.
       01  LK-COM-AREA.
           03 LK-MENSAGEM                     PIC X(40).
           03 LK-ITEM                         PIC 9.
           

      ******************************************************************
       PROCEDURE DIVISION USING LK-COM-AREA.
           
           DISPLAY ' '
           DISPLAY '***************************************************'
           DISPLAY 'EXLUSAO DE 'LK-MENSAGEM
           DISPLAY '***************************************************'
           SET EXT-OK TO FALSE
           
           PERFORM P100-EXCLUIR THRU P100-FIM UNTIL EXT-OK
           PERFORM P200-FIM
           .

       P100-EXCLUIR.

           SET EXT-OK TO TRUE 
           MOVE SPACE TO WS-CONFIRMA

           EVALUATE LK-ITEM
              WHEN '1'
                 OPEN I-O ALUNOS

                 IF FS-OK
                    DISPLAY 'Informe o numero de identificacao do aluno'
                    ' que deseja excluir: 'ACCEPT ID-ALUNO

                    READ ALUNOS INTO WS-REGISTRO
                       KEY IS ID-ALUNO
                          INVALID KEY 
                             DISPLAY 'Aluno nao cadastrado.'
                          NOT INVALID KEY 
                             DISPLAY 'ID: 'WS-ID ' Nome: 'WS-NM
                             DISPLAY '*********************************'

                             DISPLAY 
                                    'TECLE: '
                                    '<S> para exluir o aluno atual ou'
                                    ' <QUALQUER TECLA> para cancelar a ' 
                                    'exclusaO.' ACCEPT WS-CONFIRMA
                                    IF WS-CONFIRMA EQUAL 'S' OR 's'
                                      DELETE ALUNOS RECORD 
                                         DISPLAY 'Aluno excluido.'
                                    ELSE
                                      DISPLAY 'Exclusao cancelada.'
                                    END-IF

                 ELSE 
                    DISPLAY 'Erro ao abrir o arquivo de alunos.'
                    DISPLAY 'FILE STATUS ERROR: ' WS-FS
                 END-IF

                 CLOSE ALUNOS
                    
              WHEN '2'
                 OPEN I-O DISCIPLINAS

                 IF FS-OK
                    DISPLAY 'Informe o numero de identifcacao da discip'
                    'lina que deseja excluir: 'ACCEPT ID-DISCIPLINA
                    READ DISCIPLINAS INTO WS-REGISTRO
                       KEY IS ID-DISCIPLINA
                          INVALID KEY 
                             DISPLAY 'Disciplina nao cadastrado.'
                          NOT INVALID KEY
                             DISPLAY 'ID: 'WS-ID ' Nome: 'WS-NM
                             DISPLAY '*********************************'

                             DISPLAY 
                                    'TECLE: '
                                    '<S> para exluir a disciplina atual'
                                    ' ou <QUALQUER TECLA> para cancelar' 
                                    ' a exclusao.' ACCEPT WS-CONFIRMA
                                    IF WS-CONFIRMA EQUAL 'S' OR 's'
                                      DELETE DISCIPLINAS RECORD 
                                         DISPLAY 'Disciplina excluido.'
                                    ELSE
                                      DISPLAY 'Exclusao cancelada.'
                                    END-IF
                 ELSE 
                    DISPLAY 'Erro ao abrir o arquivo de alunos.'
                    DISPLAY 'FILE STATUS ERROR: ' WS-FS
                 END-IF

                 CLOSE DISCIPLINAS

              WHEN '3'
                 OPEN I-O NOTAS

                 IF FS-OK
                    DISPLAY 'Informe o ID da inclusao '
                    'que deseja excluir: 'ACCEPT ID-INCLUSAO
                    READ NOTAS INTO WS-REGISTRO
                       KEY IS ID-INCLUSAO
                          INVALID KEY 
                             DISPLAY 'inclusao nao cadastrado.'
                          NOT INVALID KEY
                             DISPLAY 'ID: 'WS-ID 
                                     ' Nome: 'WS-NM
                                     ' Disciplina: 'WS-DP
                                     ' Media: 'WS-NT
                                     ' Situacao: 'WS-ST
                             DISPLAY '*********************************'

                             DISPLAY 
                                    'TECLE: '
                                    '<S> para exluir a disciplina atual'
                                    ' ou <QUALQUER TECLA> para cancelar' 
                                    ' a exclusao.' ACCEPT WS-CONFIRMA
                                    IF WS-CONFIRMA EQUAL 'S' OR 's'
                                      DELETE NOTAS RECORD 
                                         DISPLAY 'Disciplina excluido.'
                                    ELSE
                                      DISPLAY 'Exclusao cancelada.'
                                    END-IF
                 ELSE 
                    DISPLAY 'Erro ao abrir o arquivo de alunos.'
                    DISPLAY 'FILE STATUS ERROR: ' WS-FS
                 END-IF

                 CLOSE NOTAS
 
           
           
           
           
           END-EVALUATE
           
           DISPLAY 
              'TECLE'   
              ' <QUALQUER TECLA> para nova Exclusao, ou '
              '<F> para retornar ao Menu.'
           ACCEPT WS-EXT
           
           
           .
       P100-FIM.

           
       P200-FIM.
           GOBACK.
       END PROGRAM PROGEXCL.
