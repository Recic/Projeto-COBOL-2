      ******************************************************************
      * Author: Renan Cicero
      * Date: 12/02/2023
      * Purpose: Modulo de cadastro do Desafio 3
      *IDENTIFICATION DIVISION******************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGINCL_NT.
       
       
      *ENVIRONMENT DIVISION*********************************************
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
       
      *DATA DIVISION****************************************************
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
           

      *WORKING-STORAGE SECTION******************************************
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


      *LINKAGE SECTION**************************************************
       LINKAGE SECTION.
       01  LK-COM-AREA.
           03 LK-MENSAGEM                     PIC X(40).
           03 LK-ITEM                         PIC 9.
           

      *PROCEDURE DIVISION***********************************************
       PROCEDURE DIVISION USING LK-COM-AREA.

           DISPLAY ' '
           SET EXT-OK TO FALSE
           DISPLAY '***************************************************'
           DISPLAY ' MENU DE CALCULO DE MEDIA'
           DISPLAY '***************************************************'
           DISPLAY ' '
           display 'informe o id do aluno, id da disciplina e as notas '
                   'dos 4 trismestres para calculas a media anual'
           DISPLAY ' '

           SET EXT-OK TO FALSE

           DISPLAY 'Insira o ID da inclusao: '
           ACCEPT ID-NT

           

           PERFORM P100-CONSULTA-ALUNO      THRU P100-FIM UNTIL EXT-OK
           SET EXT-OK TO FALSE

           PERFORM P200-CONSULTA-DISCIPLINA THRU P200-FIM UNTIL EXT-OK
           SET EXT-OK TO FALSE

           PERFORM P300-MEDIA               THRU P300-FIM UNTIL EXT-OK
           PERFORM P400-FIM
           .

       P100-CONSULTA-ALUNO.

           SET FS-OK  TO TRUE
           MOVE SPACE TO WS-CONFIRMA

           OPEN INPUT ALUNOS

           IF FS-OK
              DISPLAY 'Informe o numero de ID do aluno: '
              ACCEPT ID-ALUNO

              READ ALUNOS INTO WS-REGISTRO-AL
                 KEY IS ID-ALUNO
                    INVALID KEY 
                       DISPLAY 'O ALUNO NAO EXISTE'
                    NOT INVALID KEY 
                       MOVE 'F' TO WS-EXT
                       DISPLAY NM-AL

                       DISPLAY 'TECLE: '
                               '<S> para confirmar ou'
                               ' <QUALQUER TECLA> para cancelar a ' 
                               'operacao.' ACCEPT WS-CONFIRMA
                               IF WS-CONFIRMA EQUAL 'S' OR 's'
                                 MOVE NM-AL TO NM-NT
                               ELSE
                                 DISPLAY 'Operacao cancelada.'
                                 CLOSE ALUNOS
                                 PERFORM P400-FIM
                               END-IF
           
           ELSE 
              DISPLAY 'Erro ao abrir o arquivo de alunos.'
              DISPLAY 'FILE STATUS ERROR: ' WS-FS
           END-IF

           CLOSE ALUNOS

           .
       P100-FIM.

       P200-CONSULTA-DISCIPLINA.

           SET FS-OK  TO TRUE
           MOVE SPACE TO WS-CONFIRMA

           OPEN INPUT DISCIPLINAS

           IF FS-OK
              DISPLAY 'Informe o numero de ID da disciplina: '
              ACCEPT ID-DISCIPLINA

              READ DISCIPLINAS INTO WS-REGISTRO-DP
                 KEY IS ID-DISCIPLINA
                    INVALID KEY 
                       DISPLAY 'A DISCPLINA NAO EXISTE'
                       PERFORM P200-CONSULTA-DISCIPLINA
                 NOT INVALID KEY 
                       MOVE 'F' TO WS-EXT
                       DISPLAY NM-DP

                        DISPLAY 'TECLE: '
                                '<S> para confirmar ou'
                                ' <QUALQUER TECLA> para cancelar a ' 
                                'operacao.' ACCEPT WS-CONFIRMA
                                IF WS-CONFIRMA EQUAL 'S' OR 's'
                                  MOVE NM-DP TO DP-NT
                                  MOVE NT-DP TO WS-MD

                                ELSE
                                  DISPLAY 'Operacao cancelada.'
                                  CLOSE DISCIPLINAS
                                  PERFORM P400-FIM
                                END-IF

           ELSE 
              DISPLAY 'Erro ao abrir o arquivo de alunos.'
              DISPLAY 'FILE STATUS ERROR: ' WS-FS
           END-IF

           CLOSE DISCIPLINAS

           .
       P200-FIM.

       P300-MEDIA.
           
           SET FS-OK  TO TRUE

           DISPLAY 'informe a nota do primeiro trimestre: '
           ACCEPT WS-NUM1

           DISPLAY 'informe a nota do segundo trimestre: '
           ACCEPT WS-NUM2

           DISPLAY 'informe a nota do terceiro trimestre: '
           ACCEPT WS-NUM3

           DISPLAY 'informe a nota do quarto trimestre: '
           ACCEPT WS-NUM4
           
           COMPUTE MD-NT = 
           (WS-NUM1 + WS-NUM2 + WS-NUM3 + WS-NUM4) / 4

           DISPLAY 'MEDIA: 'MD-NT', MEDIA NECESSARIA PARA APROVACAO: '
           WS-MD
           
           IF MD-NT > WS-MD
              MOVE 'APROVADO' TO ST-NT 
              DISPLAY 'ALUNO 'ST-NT
           ELSE 
              MOVE 'REPROVADO' TO ST-NT
              DISPLAY 'ALUNO 'ST-NT
           END-IF
           
           DISPLAY ' '
           DISPLAY 'ID da inclusao: 'ID-NT
           DISPLAY 'Nome do aluno:  'NM-NT
           DISPLAY 'Disciplina:     'DP-NT
           DISPLAY 'Media Anual:    'MD-NT
           DISPLAY 'Situacao:       'ST-NT

           OPEN I-O NOTAS

              IF WS-FS EQUAL 35 
                 OPEN OUTPUT NOTAS
              END-IF

              IF FS-OK

                 MOVE ID-NT  TO ID-INCLUSAO   IN REG-NOTAS
                 MOVE NM-NT  TO NM-ALUNO      IN REG-NOTAS
                 MOVE DP-NT  TO NM-DISCIPLINA IN REG-NOTAS
                 MOVE MD-NT  TO MD-NOTA       IN REG-NOTAS
                 MOVE ST-NT  TO ST-ALUNO      IN REG-NOTAS
            
                 WRITE REG-NOTAS
                    INVALID KEY 
                       DISPLAY 'MEDIA JA CADASTRADA'
                    NOT INVALID KEY
                       DISPLAY 'MEDIA CADASTRADO COM SUCESSO'
              ELSE
                 DISPLAY 'ERRO AO ABRIR O ARQUIVO DE NOTAS'
                 DISPLAY 'FILE STATUS: ' WS-FS
              END-IF

           CLOSE NOTAS

           DISPLAY ' '
           DISPLAY 'TECLE '   
                   ' <QUALQUER TECLA> para novo cadastro, ou <F> para'
                   ' retornar ao menu.' ACCEPT WS-EXT
           
           .
       P300-FIM.

           
       P400-FIM.
           GOBACK.
       END PROGRAM PROGINCL_NT.
