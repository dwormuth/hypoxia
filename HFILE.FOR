C   ./ i 0
$STORAGE:2
      SUBROUTINE H FILE (LMNO)
C
C     WRITE INFO TO DATA BASE FILE
C
      CHARACTER*4 NAME,ITITL
      INTEGER CRTI,CRTO,FILE,FORM,TEMP,FPLOT, DBUF
      COMMON ICOM1(170), CRTI,CRTO,FILE,FORM,TEMP,FPLOT,
     A   NAME(7),ISTUDY,IDATE(3),IFLAG(75),NVAR,NTABLE, PARAM(75),
     B   NVARY(15),VALUE(15,2),STEP(15),ITITL(18),NUMVAR,MAXVAR,
     C   NOUT(15),NUMOUT
C
      DATA MAX /11/
C
  117 FORMAT (/' There is already a study with the number ',I8,'.'/
     A/' Enter another positive number to create a new study with that'
     B 'number.'
     C/' Enter a 0 to over-write the old data. Or,'
     D/' Enter a negative number to do nothing.')
C   ./ r 19
  147 FORMAT (/' No more room in study file.  Maximum number of studies'
     A 'is ',I3,'.')
C
C     FOLLOWING FORMATS ARE FOR PATIENT DATA BASE
C
  407 FORMAT (28X,I8,44X)
  507 FORMAT (7A4,I8,22I2)
  517 FORMAT (40I2)
  527 FORMAT (16I2, F8.1,2F6.1,F5.1,F7.3,F7.2,F6.1, 3X)
  537 FORMAT (2F6.1,F5.2,F6.1,2F5.1,F7.3,F7.1,F5.1,F6.1,F5.2,3F5.1, 2X)
  547 FORMAT (F6.1,F5.1,F7.1,F4.0,F8.1,F5.1,2F6.1,F5.1,F6.1,F5.0,
     A   F5.1,F6.2, 6X)
  557 FORMAT (5F6.2,F5.0,F3.0,F5.0,F6.1,F5.2,F6.1,F5.1,F6.2,F7.1, 2X)
  567 FORMAT (F5.1,F6.1,F7.1,F7.3,F7.1,F5.0,F5.1,F6.3,F5.1,2F6.2,
     A   F6.1,F5.3, 4X)
  577 FORMAT (4F7.2,F6.4, 46X)
  587 FORMAT (9F7.2, 17X)
C 587 FORMAT (9F6.0, 26X)
C
C     FIND PROPER POSITION IN DATA BASE FILE.  STUDY = -999 IS EMPTY.
C
   10 LOW = 0
C
      DO 50  J = 1,MAX
      LMNO = (J-1) * 9 + 1
      READ (FILE,407,REC=LMNO) DBUF
      LMNO = LMNO + 1
      IF (DBUF .EQ. -999 .AND. LOW .EQ. 0)  LOW = LMNO
      IF (DBUF .EQ. ISTUDY) GO TO 100
   50 CONTINUE
      IF (LOW .NE. 0)  GO TO 200
      WRITE (CRTO,147)  MAX
      GO TO 900
C
C     DUPLICATE
C
  100 WRITE (CRTO,117)  ISTUDY
      READ (CRTI,*)  ICHECK
      IF (ICHECK)  900,250,180
C
C     CREATE NEW STUDY (MAKE SURE THAT NEW NO. IS O.K.)
C
  180 ISTUDY = ICHECK
      GO TO 10
C
C     NOW WRITE NEW RECORD TO DATE BASE FILE (OR OVER-WRITE)
C
  200 LMNO = LOW
C
  250 LMNO = LMNO - 1
      IF (LMNO .LT. 1) LMNO = 1
      WRITE (FILE,507,REC=LMNO) NAME, ISTUDY, IDATE, (IFLAG(I), I=1,19)
      LMNO = LMNO + 1
      WRITE (FILE,517,REC=LMNO)  (IFLAG(I), I=20,59)
      LMNO = LMNO + 1
      WRITE (FILE,527,REC=LMNO)  (IFLAG(I), I=60,75), (PARAM(I), I=1,7)
      LMNO = LMNO + 1
      WRITE (FILE,537,REC=LMNO)  (PARAM(I), I=8,21)
      LMNO = LMNO + 1
      WRITE (FILE,547,REC=LMNO)  (PARAM(I), I=22,34)
      LMNO = LMNO + 1
      WRITE (FILE,557,REC=LMNO)  (PARAM(I), I=35,48)
      LMNO = LMNO + 1
      WRITE (FILE,567,REC=LMNO)  (PARAM(I), I=49,61)
      LMNO = LMNO + 1
      WRITE (FILE,577,REC=LMNO)  (PARAM(I), I=62,66)
      LMNO = LMNO + 1
      WRITE (FILE,587,REC=LMNO)  (PARAM(I), I=67,75)
      LMNO = LMNO + 1
C
  900 RETURN
      END
