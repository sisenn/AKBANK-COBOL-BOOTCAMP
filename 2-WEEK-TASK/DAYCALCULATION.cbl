       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAYCALCU.
       AUTHOR.     SINEM SEN.
      *Bu bölüm, programın kimlik bilgilerini içerir. Programın adı 
      *"DAYCALCU" ve yazarı "SINEM SEN" olarak belirtilmiştir.
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRT-LINE ASSIGN TO PRTLINE
                           STATUS CHECK-FILE-PRT.
           SELECT DATE-REC ASSIGN TO DATEREC
                           STATUS CHECK-FILE-DATE.
      *Bu bölüm, programın çalışma ortamını ve dosya kontrolünü tanımlar
      *"PRT-LINE" ve "DATE-REC" adında iki dosya tanımlanmıştır.
      *Dosyaların atamaları ve durum kontrol değişkenleri belirtilmiştir        
       DATA DIVISION. 
       FILE SECTION. 
       FD  PRT-LINE RECORDING MODE F.
         01 PRT-LINE-MEMBERS.
           05 PRT-LINE-NUMBERS       PIC 9(4).
           05 PRT-LINE-NAME          PIC X(15).
           05 PRT-LINE-SURNAME       PIC X(15).
           05 PRT-LINE-BIRTHDAY      PIC 9(8).
           05 PRT-LINE-TODAY         PIC 9(8).
           05 PRT-LINE-LIFE          PIC 9(8).
       FD  DATE-REC RECORDING MODE F.
         01 DATE-REC-MEMBERS.
           05 DATE-REC-NUMBERS       PIC 9(4).
           05 DATE-REC-NAME          PIC X(15).
           05 DATE-REC-SURNAME       PIC X(15).
           05 DATE-REC-BIRTHDAY      PIC 9(8).
           05 DATE-REC-TODAY         PIC 9(8).
      *Bu bölüm, programın veri bölümünü tanımlar. "PRT-LINE" ve 
      *"DATE-REC" dosyalarının alanlarını belirtir. Her bir alanın veri 
      *tipi ve boyutu (PIC) belirtilmiştir.    
       WORKING-STORAGE SECTION.
       01  VARIABLES.
         05 INT-TODAY        PIC 9(8).
         05 INT-BIRTHDAY     PIC 9(8).
         05 CHECK-FILE-PRT   PIC 9(2).
           88 CHECK-FILE-PRT-ST    VALUE 00 97.
         05 CHECK-FILE-DATE  PIC 9(2).
           88 CHECK-FILE-DATE-ST   VALUE 00 97.
           88 CHECK-EOF            VALUE 10.
      *Bu bölüm, programın çalışma değişkenlerini içerir. "INT-TODAY" 
      *ve "INT-BIRTHDAY" isimli iki tamsayı değişkeni ve 
      *"CHECK-FILE-PRT", "CHECK-FILE-DATE" ve "CHECK-EOF" isimli kontrol
      * değişkenleri tanımlanmıştır.
       PROCEDURE DIVISION.
       0001-MAIN-PROCESS.
           PERFORM H100-OPEN-FILES.
           PERFORM H200-PROCESSING UNTIL CHECK-EOF.
           PERFORM H999-EXIT.
       0001-END. EXIT.       
      *Bu bölüm, programın işlem adımlarını içerir. Ana işlem adımları 
      *"H100-OPEN-FILES", "H200-PROCESSING" ve "H999-EXIT" olarak
      *belirtilmiştir.
       H100-OPEN-FILES.
           OPEN INPUT DATE-REC.
           OPEN OUTPUT PRT-LINE.
           PERFORM H110-FILE-CONTROL.
           READ DATE-REC.
       H100-END. EXIT.
      *Bu bölüm, dosyaların açılması ve dosya kontrolünün 
      *gerçekleştirilmesini içerir. "DATE-REC" dosyası giriş olarak 
      *açılır, "PRT-LINE" dosyası çıktı olarak açılır. 
      *"H110-FILE-CONTROL" adımı dosya kontrolünü gerçekleştirir. 
      *Son olarak, ilk kaydı "DATE-REC" dosyasından okur.
       H110-FILE-CONTROL.
           IF (CHECK-FILE-PRT NOT = 97) AND (CHECK-FILE-PRT NOT = 00)
              DISPLAY "FILE NOT OPENED. ERROR CODE:" CHECK-FILE-PRT
              PERFORM H999-EXIT
           END-IF.
           IF (CHECK-FILE-DATE NOT = 97) AND (CHECK-FILE-DATE NOT = 00)
              DISPLAY "FILE NOT OPENED. ERROR CODE:" CHECK-FILE-DATE 
              PERFORM H999-EXIT
           END-IF.
       H110-END. EXIT.
      *Bu bölüm, dosya kontrolünü gerçekleştirir. Dosyaların başarıyla 
      *açılıp açılmadığını kontrol eder. Eğer dosya açma işlemi 
      *başarısız olursa ilgili hata kodunu ekrana yazdırır ve programı 
      *sonlandırır.
       H200-PROCESSING.
           PERFORM H300-MOVE.
           READ DATE-REC.
       H200-END. EXIT.
      *Bu bölüm, kayıtların işlenmesini sağlar. "H300-MOVE" adımını 
      *çağırarak veri taşıma ve yaş hesaplama işlemlerini yapar. 
      *Ardından bir sonraki kaydı "DATE-REC" dosyasından okur.
       H300-MOVE.
           COMPUTE INT-TODAY = FUNCTION INTEGER-OF-DATE(DATE-REC-TODAY).
           COMPUTE INT-BIRTHDAY = FUNCTION INTEGER-OF-DATE
      -    (DATE-REC-BIRTHDAY).
           COMPUTE PRT-LINE-LIFE = INT-TODAY - INT-BIRTHDAY.
           MOVE DATE-REC-NUMBERS   TO PRT-LINE-NUMBERS.
           MOVE DATE-REC-NAME      TO PRT-LINE-NAME.
           MOVE DATE-REC-SURNAME   TO PRT-LINE-SURNAME.
           MOVE DATE-REC-BIRTHDAY  TO PRT-LINE-BIRTHDAY.
           MOVE DATE-REC-TODAY     TO PRT-LINE-TODAY.
           WRITE PRT-LINE-MEMBERS.
       H300-END. EXIT.
      *Bu bölüm, kayıtların taşınması ve yaş hesaplamasının 
      *gerçekleştirildiği adımdır. "INT-TODAY" ve "INT-BIRTHDAY" 
      *değişkenlerine tarihleri dönüştürerek atanır. Ardından yaş farkı 
      *hesaplanır. Kayıtlar "PRT-LINE" dosyasına taşınır ve dosyaya 
      *yazılır.
       H999-EXIT.
           CLOSE DATE-REC.
           CLOSE PRT-LINE.
       H999-END. EXIT.
           STOP RUN.
      *Bu bölüm, programın sonlandırılmasını sağlar. 
      *Dosyalar kapatılır ve program sonlandırılır.
           