      ******************************************************************
      * Author: Renan Cicero
      * Date: 12/02/2023
      * Purpose: Modulo de cadastro do Desafio 3
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGINCL.
       
       
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
       
       01  WS-REGISTRO                      PIC X(50) VALUE SPACE.
       01  FILLER REDEFINES WS-REGISTRO.
           03 WS-ID                         PIC 9(03).
           03 WS-NM                         PIC X(20).
           03 WS-NUM                        PIC X(20).
           03 WS-NT                         PIC 9(02)V99.

       77  WS-FS                      PIC 99.
           88 FS-OK                        VALUE 0.     

       77  WS-EXT                          PIC X.
           88 EXT-OK              VALUE 'F' FALSE 'N'.


      ******************************************************************
       LINKAGE SECTION.
       01  LK-COM-AREA.
           03 LK-MENSAGEM                     PIC X(40).
           03 LK-ITEM                         PIC 9.
           

      ******************************************************************
       PROCEDURE DIVISION USING LK-COM-AREA.

           DISPLAY ' '
           DISPLAY '***************************************************'
           DISPLAY 'CADASTRO DE ' LK-MENSAGEM
           DISPLAY '***************************************************'
           DISPLAY ' '
           SET EXT-OK TO FALSE
           PERFORM P100-CADASTRA THRU P100-FIM UNTIL WS-EXT = 'F' or 'f'
           PERFORM P200-FIM
           .

       P100-CADASTRA.
           
           SET EXT-OK TO TRUE

           DISPLAY 'Numero para identificaçao: 'ACCEPT WS-ID
           DISPLAY 'Nome: ' ACCEPT WS-NM
              
           IF LK-ITEM EQUAL '1'
              DISPLAY 'Numero para contato: ' 
              ACCEPT WS-NUM
           ELSE
              DISPLAY 'Nota minima para aprovacao: '
              ACCEPT WS-NT
           END-IF

           EVALUATE LK-ITEM

              WHEN '1'
                 OPEN I-O ALUNOS

                 IF WS-FS EQUAL 35 
                     OPEN OUTPUT ALUNOS
                 END-IF

                 IF FS-OK

                    MOVE WS-ID  TO ID-ALUNO
                    MOVE WS-NM  TO NM-ALUNO
                    MOVE WS-NUM TO TL-ALUNO

                    WRITE REG-ALUNO
                    INVALID KEY 
                       DISPLAY 'ALUNO JA CADASTRADA'
                    NOT INVALID KEY
                       DISPLAY 'ALUNO CADASTRADO COM SUCESSO'

                 ELSE
                    DISPLAY 'ERRO AO ABRIR O ARQUIVO DE ALUNOS'
                    DISPLAY 'FILE STATUS: ' WS-FS
                 END-IF

                 CLOSE ALUNOS

              WHEN '2'
                 
                 OPEN I-O DISCIPLINAS

                 IF WS-FS EQUAL 35 
                     OPEN OUTPUT DISCIPLINAS
                 END-IF

                 IF FS-OK
                    MOVE WS-ID  TO ID-DISCIPLINA
                    MOVE WS-NM  TO NM-DISCIPLINA
                    MOVE WS-NT  TO NT-DISCIPLINA
                   
                    WRITE REG-DISCIPLINA
                    INVALID KEY 
                       DISPLAY 'DISCIPLINA JA CADASTRADA'
                    NOT INVALID KEY
                       DISPLAY 'DISCIPLINA CADASTRADO COM SUCESSO'
                 ELSE
                    DISPLAY 'ERRO AO ABRIR O ARQUIVO DE DISCIPLINAS'
                    DISPLAY 'FILE STATUS: ' WS-FS
                 END-IF

                 CLOSE DISCIPLINAS
                 
           END-EVALUATE

           DISPLAY ' '
           DISPLAY 
           'TECLE '   
           ' <QUALQUER TECLA> para novo cadastro, ou <F> para'
           ' retornar ao menu.' ACCEPT WS-EXT
           
           .
          
       P100-FIM.

           
       P200-FIM.
           GOBACK.
       END PROGRAM PROGINCL.
