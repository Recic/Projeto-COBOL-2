      ******************************************************************
      * Author: Renan Cicero
      * Date: 01/02/2023
      * Purpose: Modulo de Consulta do Desafio 3
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCONS.
       
       
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
       
        01  WS-REGISTRO-NT                     PIC X(60) VALUE SPACE.
       01  FILLER REDEFINES WS-REGISTRO-NT.
           03 ID-NT                          PIC 9(03).
           03 NM-NT                          PIC X(20).
           03 DP-NT                          PIC X(20).
           03 MD-NT                          PIC 9(02)V99.
           03 ST-NT                          PIC X(10).

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
       
              77  WS-FS                           PIC 99.
           88 FS-OK                        VALUE 0.

       77  WS-EOF                          PIC X.
           88 EOF-OK               VALUE 'S' FALSE 'N'.          

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
           DISPLAY 'CONSULTA DE 'LK-MENSAGEM
           DISPLAY '***************************************************'
           SET EXT-OK TO FALSE
           
           PERFORM P100-CONSULTA THRU P100-FIM UNTIL EXT-OK
           PERFORM P200-FIM
           .

       P100-CONSULTA.

           SET EOF-OK TO FALSE
           SET FS-OK TO TRUE

           EVALUATE LK-ITEM
              WHEN '1'
                 OPEN INPUT ALUNOS

                 IF FS-OK
                    DISPLAY 'Informe o numero de identificacao do aluno'
                    ' que deseja consultar: 'ACCEPT ID-ALUNO

                    READ ALUNOS INTO WS-REGISTRO-AL
                       KEY IS ID-ALUNO
                          INVALID KEY 
                             DISPLAY 'Aluno nao cadastrado.'
                          NOT INVALID KEY 
                             DISPLAY 'ID: 'ID-AL
                                     ' Nome: 'NM-AL
                                     ' Telefone: 'TL-AL
                             DISPLAY '*********************************'

                 ELSE 
                    DISPLAY 'Erro ao abrir o arquivo de alunos.'
                    DISPLAY 'FILE STATUS ERROR: ' WS-FS
                 END-IF

                 CLOSE ALUNOS
                    
              WHEN '2'
                 OPEN INPUT DISCIPLINAS

                 IF FS-OK
                    DISPLAY 'Informe o numero de identifcacao da discip'
                    'lina que deseja consultar: 'ACCEPT ID-DISCIPLINA
                    READ DISCIPLINAS INTO WS-REGISTRO-DP
                       KEY IS ID-DISCIPLINA
                          INVALID KEY 
                             DISPLAY 'Disciplina nao cadastrada.'
                          NOT INVALID KEY 
                             DISPLAY 'ID: 'NM-DP 
                                     ' Nome: 'NM-DP
                                     ' Nota minima: ' NT-DP
                             DISPLAY '*********************************'


                 ELSE 
                    DISPLAY 'Erro ao abrir o arquivo de alunos.'
                    DISPLAY 'FILE STATUS ERROR: ' WS-FS
                 END-IF

                 CLOSE DISCIPLINAS

              WHEN '3'
                   OPEN INPUT NOTAS

                 IF FS-OK
                    DISPLAY 'Informe o ID da inclusao que deseja consul'
                    'tar: 'ACCEPT ID-INCLUSAO
                    READ NOTAS INTO WS-REGISTRO-NT
                       KEY IS ID-INCLUSAO
                          INVALID KEY 
                             DISPLAY 'Inclusao nao cadastrado.'
                          NOT INVALID KEY
                             DISPLAY 'ID inclusao: 'ID-NT 
                                     ' Nome: 'NM-NT
                                     ' Disciplina: 'DP-NT
                                     ' Media: 'MD-NT
                                     ' Situacao:'ST-NT
                             DISPLAY '*********************************'


                 ELSE 
                    DISPLAY 'Erro ao abrir o arquivo de alunos.'
                    DISPLAY 'FILE STATUS ERROR: ' WS-FS
                 END-IF

                 CLOSE DISCIPLINAS
                 


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
       END PROGRAM PROGCONS.
