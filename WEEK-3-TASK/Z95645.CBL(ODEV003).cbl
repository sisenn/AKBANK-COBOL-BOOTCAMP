       IDENTIFICATION DIVISION.
       PROGRAM-ID. ODEV003.
       AUTHOR.     SINEM SEN.
      *----
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INP-FILE ASSIGN TO INPFILE
                           STATUS CHECK-FILE-INPUT.
           SELECT OUT-FILE ASSIGN TO OUTFILE
                           STATUS CHECK-FILE-OUTPUT.
           SELECT IDX-FILE ASSIGN TO IDXFILE
                           ORGANIZATION IS INDEXED
                           ACCESS RANDOM
                           RECORD KEY IDX-FILE-KEY
                           STATUS CHECK-FILE-INDEX.
      *Bu dosya tanmlamalar, programn verileri giri dosyasndan
      *okumas, çk dosyasna yazmas ve dizinli dosyay kullanarak
      *rastgele eriim salamas için gereken bilgileri salar.
       DATA DIVISION.
       FILE SECTION.
       FD  INP-FILE RECORDING MODE F.
         01 INP-FILE-MEMBERS.
           05 INP-FILE-ID            PIC X(5).
           05 INP-FILE-DVZ           PIC X(3).
       FD  OUT-FILE RECORDING MODE F.
         01 OUT-FILE-MEMBERS.
           05 OUT-FILE-ID            PIC 9(5).
           05 OUT-FILE-DVZ           PIC 9(3).
           05 OUT-FILE-NAME          PIC X(15).
           05 OUT-FILE-SURNAME       PIC X(15).
           05 OUT-FILE-DATE          PIC 9(8).
           05 OUT-FILE-BALANCE       PIC 9(15).
       FD  IDX-FILE.
         01 IDX-FILE-MEMBERS.
           05 IDX-FILE-KEY.
             10 IDX-FILE-ID          PIC S9(5)  COMP-3.
             10 IDX-FILE-DVZ         PIC S9(3)  COMP.
           05 IDX-FILE-NAME          PIC X(15).
           05 IDX-FILE-SURNAME       PIC X(15).
           05 IDX-FILE-DATE          PIC S9(7)  COMP-3.
           05 IDX-FILE-BALANCE       PIC S9(15) COMP-3.
      *Buradaki tanmlamalar, programn çalmas için kullanlacak olan
      *giri, çk ve dizinli dosyalarn kayt yaplarn belirtir:
       WORKING-STORAGE SECTION.
       01  CONTROL-STATEMENTS.
         05 CHECK-FILE-INPUT       PIC 9(2).
           88 INPUT-FILE-SUCCESS   VALUE 00 97.
           88 INPUT-FILE-EOF       VALUE 10.
         05 CHECK-FILE-OUTPUT      PIC 9(2).
           88 OUTPUT-FILE-SUCCESS  VALUE 00 97.
         05 CHECK-FILE-INDEX       PIC 9(2).
           88 INDEX-FILE-SUCCESS   VALUE 00 97.
         05 INT-DATE               PIC 9(7).
         05 GREG-DATE              PIC 9(8).
      *Bu kontrol deikenleri, programn dosya ilemlerini izlemek ve
      *durumlarna göre program akn yönlendirmek için kullanlr.
      *Örnein, giri dosyasnn sonuna gelindiinde veya çk
      *dosyasna yazma ilemi baarsz olduunda ilgili durumlar
      *belirlemek ve buna göre programn davrann kontrol etmek için
      *kullanlabilirler.
       PROCEDURE DIVISION.
       0001-MAIN-PROCESS.
           PERFORM H100-OPEN-FILES.
           PERFORM H300-MOVE UNTIL INPUT-FILE-EOF.
           PERFORM H999-EXIT.
       0001-END. EXIT.
      *Bu bölümdeki etiketler, program içindeki farkl noktalara
      *referans oluturarak ilem süreçlerini belirlemek için kullanlr
      *Her etiketin altnda tanmlanan süreçler, belirli bir görevi
      *yerine getiren ve programn ilevselliini salayan kod
      *bloklardr.
       H100-OPEN-FILES.
           OPEN INPUT  INP-FILE.
           OPEN OUTPUT OUT-FILE.
           OPEN INPUT  IDX-FILE.
           PERFORM H110-FILE-CONTROL.
           READ INP-FILE.
       H100-END. EXIT.
      *-----
       H110-FILE-CONTROL.
           IF (CHECK-FILE-INPUT NOT = 97) AND (CHECK-FILE-INPUT NOT = 0)
              DISPLAY "FILE NOT OPENED. ERROR CODE:" CHECK-FILE-INPUT
              PERFORM H999-EXIT
           END-IF.
           IF (CHECK-FILE-OUTPUT NOT = 97) AND
      -       (CHECK-FILE-OUTPUT NOT = 0)
              DISPLAY "FILE NOT OPENED. ERROR CODE:" CHECK-FILE-OUTPUT
              PERFORM H999-EXIT
           END-IF.
           IF (CHECK-FILE-INDEX NOT = 97) AND (CHECK-FILE-INDEX NOT = 0)
              DISPLAY "FILE NOT OPENED. ERROR CODE:" CHECK-FILE-INDEX
              PERFORM H999-EXIT
           END-IF.
       H110-END. EXIT.
      *-----
       H300-MOVE.
           COMPUTE IDX-FILE-ID=FUNCTION NUMVAL (INP-FILE-ID)
           COMPUTE IDX-FILE-DVZ=FUNCTION NUMVAL (INP-FILE-DVZ)
           READ IDX-FILE KEY IS IDX-FILE-KEY
           INVALID KEY PERFORM WRONG-RECORD
           NOT INVALID KEY PERFORM VALID-RECORD.
       H300-END. EXIT.
      *-----
       WRONG-RECORD.
           DISPLAY 'WRONG RECORD' IDX-FILE-KEY.
           READ INP-FILE.
       WRONG-END. EXIT.
      *-----
       VALID-RECORD.
           COMPUTE INT-DATE=FUNCTION INTEGER-OF-DAY(IDX-FILE-DATE).
           COMPUTE GREG-DATE=FUNCTION DATE-OF-INTEGER(INT-DATE).
           MOVE IDX-FILE-ID      TO OUT-FILE-ID.
           MOVE IDX-FILE-DVZ     TO OUT-FILE-DVZ.
           MOVE IDX-FILE-NAME    TO OUT-FILE-NAME.
           MOVE IDX-FILE-SURNAME TO OUT-FILE-SURNAME.
           MOVE GREG-DATE        TO OUT-FILE-DATE.
           MOVE IDX-FILE-BALANCE TO OUT-FILE-BALANCE.
           WRITE OUT-FILE-MEMBERS.
           READ INP-FILE.
       VALID-END. EXIT.
      *----
       H999-EXIT.
           CLOSE INP-FILE.
           CLOSE OUT-FILE.
           CLOSE IDX-FILE.
           STOP RUN.
       H999-END. EXIT.
