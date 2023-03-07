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

           SELECT NOTAS ASSIGN TO 
           '/home/recic/Dev/Cobol/Desafio M3/Dados/NOTAS.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY ID-INCLUSAO
           FILE STATUS IS WS-FS.

           SELECT AL-APROV ASSIGN TO 
           '/home/recic/Dev/Cobol/Desafio M3/Dados/AL-APROV.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY ID-INC-APROV
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

       FD  AL-APROV.
           COPY 
           '/home/recic/Dev/Cobol/Desafio M3/Dados/FD-AL-APROV.cpy'.
           

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
           DISPLAY 'CADASTRO DE ' LK-MENSAGEM
           DISPLAY '***************************************************'
           DISPLAY ' '
           SET EXT-OK TO FALSE
           PERFORM P000-CADASTRA THRU P000-FIM
           UNTIL WS-EXT = 'F' or 'f'
           PERFORM P400-FIM
           .

       P000-CADASTRA.
           EVALUATE LK-ITEM
              WHEN '1'
                 PERFORM P001-CADASTRO-ALUNO  
              WHEN '2'
                 PERFORM P002-CADASTRO-DISCIPLINA
              WHEN '3'
                 PERFORM P003-CADASTRA-NOTA
           END-EVALUATE

           .
       P000-FIM.

           
       P001-CADASTRO-ALUNO.
           SET EXT-OK TO TRUE
       
              DISPLAY 'Numero para identificaçao: '
              ACCEPT ID-AL
              DISPLAY 'Nome: '
              ACCEPT NM-AL
              DISPLAY 'Numero para contato: '
              ACCEPT TL-AL
       
              OPEN I-O ALUNOS
       
              IF WS-FS EQUAL 35 
               OPEN OUTPUT ALUNOS
              END-IF
       
              IF FS-OK
                 MOVE ID-AL  TO ID-ALUNO IN REG-ALUNO
                 MOVE NM-AL  TO NM-ALUNO IN REG-ALUNO
                 MOVE TL-AL  TO TL-ALUNO IN REG-ALUNO
       
              WRITE REG-ALUNO
                 INVALID KEY 
                    DISPLAY 'ALUNO JA CADASTRADA'
                 NOT INVALID KEY
                    DISPLAY 'ALUNO CADASTRADO COM SUCESSO'
       
              ELSE
                 GOBACK
              END-IF
       
              CLOSE ALUNOS

           DISPLAY ' '
           DISPLAY 'TECLE '   
                   ' <QUALQUER TECLA> para novo cadastro, ou <F> para'
                   ' retornar ao menu.' ACCEPT WS-EXT
           
           .
       P001-FIM.

       P002-CADASTRO-DISCIPLINA.
           SET EXT-OK TO TRUE

              DISPLAY 'Numero para identificaçao: '
              ACCEPT ID-DP
              DISPLAY 'Nome: '
              ACCEPT NM-DP
              DISPLAY 'Nota minima parar aprovacao: '
              ACCEPT NT-DP

              OPEN I-O DISCIPLINAS

              IF WS-FS EQUAL 35 
                 OPEN OUTPUT DISCIPLINAS
              END-IF

              IF FS-OK
                 MOVE ID-DP  TO ID-DISCIPLINA IN REG-DISCIPLINA
                 MOVE NM-DP  TO NM-DISCIPLINA IN REG-DISCIPLINA
                 MOVE NT-DP  TO NT-DISCIPLINA IN REG-DISCIPLINA
              
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
            
           DISPLAY ' '
           DISPLAY 'TECLE '   
                   ' <QUALQUER TECLA> para novo cadastro, ou <F> para'
                   ' retornar ao menu.' ACCEPT WS-EXT
           
           .
       P002-FIM.

       P003-CADASTRA-NOTA.
           DISPLAY ' '
           SET EXT-OK TO FALSE

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
           
           .
       P003-FIM.

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
           
           DISPLAY ' '
           DISPLAY 'MEDIA: 'MD-NT', MEDIA NECESSARIA PARA APROVACAO: '
           WS-MD
           
           DISPLAY ' '
           IF MD-NT > WS-MD
              MOVE 'APROVADO' TO ST-NT 
              DISPLAY 'ALUNO 'ST-NT
              DISPLAY ' '

              OPEN I-O AL-APROV

              IF WS-FS EQUAL 35 
                 OPEN OUTPUT AL-APROV
              END-IF
                 
                 IF FS-OK

                    MOVE ID-NT  TO ID-INC-APROV  IN REG-AL-APROV
                    MOVE NM-NT  TO NM-ALUNO      IN REG-AL-APROV
                    MOVE DP-NT  TO NM-DISCIPLINA IN REG-AL-APROV
                    MOVE MD-NT  TO MD-NOTA       IN REG-AL-APROV
                    MOVE ST-NT  TO ST-ALUNO      IN REG-AL-APROV
            
                    WRITE REG-AL-APROV
                    INVALID KEY 
                       DISPLAY 'MEDIA JA CADASTRADA NOS APROVADOS'
                    NOT INVALID KEY
                       DISPLAY 'CADASTRADO COM SUCESSO NA LISTA DE '
                               'APROVADOS'
                 END-IF

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
                       DISPLAY ' '
                       DISPLAY 'MEDIA JA CADASTRADA'
                    NOT INVALID KEY
                       DISPLAY ' '
                       DISPLAY 'MEDIA CADASTRADO COM SUCESSO'
              ELSE
                 DISPLAY 'ERRO AO ABRIR O ARQUIVO DE NOTAS'
                 DISPLAY 'FILE STATUS: ' WS-FS
              END-IF

           CLOSE NOTAS
           
           DISPLAY ' '
           DISPLAY 'Tecle <QUALQUER TECLA> para nova cadastro de notas '
                   'ou <F> para retornar ao menu'
                   ACCEPT SAIDERA
                   IF SAIDERA = 'F' OR 'f'
                       PERFORM P400-FIM
                   ELSE 
                       PERFORM P003-CADASTRA-NOTA
                   END-IF
           .
       P300-FIM.
           
          
       

           
       P400-FIM.
           GOBACK.
       END PROGRAM PROGINCL.
