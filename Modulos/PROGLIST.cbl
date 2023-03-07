      ******************************************************************
      * Author: Renan Cicero
      * Date: 12/02/2023
      * Purpose: Modulo de Listagem do Desafio 3
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGLIST.
       
       
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
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY ID-ALUNO
           FILE STATUS IS WS-FS.

           SELECT DISCIPLINAS ASSIGN TO 
           '/home/recic/Dev/Cobol/Desafio M3/Dados/DISCIPLINAS.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY ID-DISCIPLINA
           FILE STATUS IS WS-FS.

           SELECT NOTAS ASSIGN TO 
           '/home/recic/Dev/Cobol/Desafio M3/Dados/NOTAS.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY ID-INCLUSAO
           FILE STATUS IS WS-FS.

           SELECT AL-APROV ASSIGN TO 
           '/home/recic/Dev/Cobol/Desafio M3/Dados/AL-APROV.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
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
        
       77  WS-FS                             PIC 99.
           88 FS-OK                          VALUE 0.     

       77  WS-EXT                            PIC X.
           88 EXT-OK                         VALUE 'F' FALSE 'N'.

       77  WS-EOF                            PIC X.
           88 EOF-OK                         VALUE 'F' FALSE 'N'.

       77  WS-CONT                           PIC 9(003) VALUE ZERO.

       77  WS-SIMBORA                        PIC X.

      ******************************************************************
       LINKAGE SECTION.
       01  LK-COM-AREA.
           03 LK-MENSAGEM                     PIC X(40).
           03 LK-ITEM                         PIC 9.
           

      ******************************************************************
       PROCEDURE DIVISION USING LK-COM-AREA.

           DISPLAY ' '
           DISPLAY '***************************************************'
           DISPLAY 'LISTA DE ' LK-MENSAGEM
           DISPLAY '***************************************************'

           SET EXT-OK TO FALSE

           PERFORM P100-LISTA
           PERFORM P200-FIM
           .

       P100-LISTA.

           SET EOF-OK TO FALSE
           SET FS-OK TO TRUE
           SET WS-CONT TO 0

           EVALUATE LK-ITEM
              WHEN '1'
                  
                    OPEN INPUT ALUNOS 
                    PERFORM UNTIL EOF-OK
                    IF FS-OK 
                       PERFORM UNTIL EOF-OK
                          READ ALUNOS INTO WS-REGISTRO-AL
                             AT END 
                                SET EOF-OK TO TRUE
                             NOT AT END 
                             ADD 1 TO WS-CONT 
                             DISPLAY 'CADASTRO '
                                      WS-CONT
                                      ': '
                                      ID-AL
                                      ' - '
                                      NM-AL
                                      ' - '
                                      TL-AL
                       END-PERFORM
                    ELSE
                       DISPLAY 'ERRO AO ABRIR O ARQUIVO DE ALUNOS.'
                       DISPLAY 'FILE STATUS ERROR: ' WS-FS
                       GOBACK
                    END-IF
                    END-PERFORM
   
                    CLOSE ALUNOS
   
              WHEN '2'
   
                    OPEN INPUT DISCIPLINAS 
                    PERFORM UNTIL EOF-OK
                    IF FS-OK 
                       PERFORM UNTIL EOF-OK
                          READ DISCIPLINAS INTO WS-REGISTRO-DP
                             AT END 
                                SET EOF-OK TO TRUE
                             NOT AT END 
                                ADD 1 TO WS-CONT
                             DISPLAY 'CADASTRO '
                                      WS-CONT
                                      ': '
                                      ID-DP
                                      ' - '
                                      NM-DP
                                      ' - '
                                      NT-DP
                       END-PERFORM
                    ELSE
                       DISPLAY 'ERRO AO ABRIR O ARQUIVO DE DICIPLINAs'
                       DISPLAY 'FILE STATUS ERROR: ' WS-FS
                       GOBACK
                    END-IF
                    END-PERFORM
   
                    CLOSE DISCIPLINAS
   
              WHEN '3'
                 DISPLAY '|                                           |'
                 DISPLAY '|         1 - Lista Geral                   |'
                 DISPLAY '|         2 - Lista de Aprovados            |'     
                 DISPLAY '|                                           |'        
                 DISPLAY '|                                           |'
                 DISPLAY '|           F - ENCERRAR                    |'
                 DISPLAY '|                                           |'
                 DISPLAY '*********************************************'
                 ACCEPT WS-SIMBORA
      
                 EVALUATE WS-SIMBORA
      
                 WHEN '1'
                    SET EOF-OK TO FALSE
                    SET FS-OK TO TRUE
                    SET WS-CONT TO 0
                    INITIALIZE WS-REGISTRO-NT

                    OPEN INPUT NOTAS 
                    PERFORM UNTIL EOF-OK
                    IF FS-OK 
                       PERFORM UNTIL EOF-OK
                          READ NOTAS INTO WS-REGISTRO-NT
                             AT END 
                                SET EOF-OK TO TRUE
                             NOT AT END 
                             ADD 1 TO WS-CONT 
                                DISPLAY 'INCLUSAO '
                                   WS-CONT
                                   ': '
                                   ID-NT
                                   ' - '
                                   NM-NT
                                   ' - '
                                   DP-NT
                                   ' - '
                                   MD-NT
                                   ' - '
                                   ST-NT
                       END-PERFORM
                    ELSE
                       DISPLAY 'ERRO AO ABRIR O ARQUIVO DE ALUNOS.'
                       DISPLAY 'FILE STATUS ERROR: ' WS-FS
                       GOBACK
                    END-IF
                    END-PERFORM
   
                    CLOSE NOTAS    

                 WHEN '2' 
                    SET EOF-OK TO FALSE
                    SET FS-OK TO TRUE
                    SET WS-CONT TO 0

                    OPEN INPUT AL-APROV 
                    PERFORM UNTIL EOF-OK
                    IF FS-OK 
                       PERFORM UNTIL EOF-OK
                          READ AL-APROV INTO WS-REGISTRO-NT
                             AT END 
                                SET EOF-OK TO TRUE
                             NOT AT END 
                             ADD 1 TO WS-CONT 
                                  DISPLAY 'INCLUSAO '
                                   WS-CONT
                                   ': '
                                   ID-NT
                                   ' - '
                                   NM-NT
                                   ' - '
                                   DP-NT
                                   ' - '
                                   MD-NT
                                   ' - '
                                   ST-NT
                       END-PERFORM
                    ELSE
                       DISPLAY 'ERRO AO ABRIR O ARQUIVO DE ALUNOS.'
                       DISPLAY 'FILE STATUS ERROR: ' WS-FS
                       GOBACK
                    END-IF
                    END-PERFORM
   
                    CLOSE AL-APROV
                 WHEN OTHER 
                    DISPLAY 'Opcao invalida'
           END-EVALUATE
           .
       P100-FIM.

           
       P200-FIM.
           GOBACK.
       END PROGRAM PROGLIST.
