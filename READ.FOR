      SUBROUTINE READ (IOPT,LMNO)
      LOGICAL CHANGE,YES,CALCS
      CHARACTER*1 ICRT,ICRT1,IDEV,IFILE,IFILE1
      CHARACTER*4 TITLE2(3,2),TITLE,NAME
      CHARACTER*8 LABEL
      CHARACTER*6 UNITS
      INTEGER CRTI,CRTO,FILE,FORM,TEMP,FPLOT
      COMMON ICOM1(170), CRTI,CRTO,FILE,FORM,TEMP,FPLOT,
     A   NAME(7),ISTUDY,IDATE(3),IFLAG(75),NVAR,NTABLE, PARAM(75),
     B   NVARY(15),VALUE(15,2),STEP(15),TITLE(18),NUMVAR,MAXVAR,
     C   NOUT(15),NUMOUT
      COMMON /PHRED1/ LABEL(75)
      COMMON /PHRANK/ UNITS(75)
C
      DATA ICRT,IFILE,ICRT1,IFILE1 /'C','F','c','f'/
      DATA TITLE2 /'Expe','rime','ntal', 'Assu','med ','    '/
      DATA MAX /11/
C
  117 FORMAT (/' Save data to study file? (Y/N) ')
  127 FORMAT (18A4)
  137 FORMAT (3I2)
  207 FORMAT (/' Enter data from ''CRT'' or retrieve from ''FILE''? ')
  217 FORMAT (/' Enter the study name (< 28 Characters): ')
  227 FORMAT (/' Enter the study #: (Integer) ')
  237 FORMAT (/' Enter study date (MMDDYY): ')
  247 FORMAT (/' Enter data: Parameter number '','' Value.' /
     A   ' Enter ''0,0'' when done.')
  257 FORMAT (/1X,3A4)
  267 FORMAT (' Invalid parameter number.  Please try again.')
  277 FORMAT (//' Non-existant study number:',I8)
  297 FORMAT (/'  1: Calculations'
     A        /'  2: Format only (Use existing data to generate report)'
     B        /'  3: Vary parameters >1 step'
     C        /'  4: Plot     '
     D        /'  5: Abort current study and select a new study'
     E        /' 99: Stop')
  307 FORMAT (/' Enter option: ')
  347 FORMAT (20X,'*** H y p o x i a   S t u d y ***')
  357 FORMAT (/' Restoring: ',7A4,'  #',I9,I4,2('/',I2))
  367 FORMAT (/' Enter report title (< 72 Characters)')
  387 FORMAT (/' List data? (Y/N) ')
  397 FORMAT (/' ',I2,1X,A,F7.2,1X,A,2X,I2,1X,A,F7.2,1X,A,2X,I2,1X,A,
     A F7.2,1X,A)
C 397 FORMAT (3(I3,1X,A,F8.3,1X,A))
C 397 FORMAT (3(I4,1X,A,F10.3))
  407 FORMAT (' Change any data? (Y/N) ')
  417 FORMAT (/' Change study name? (Y/N) ')
  427 FORMAT (/' Enter: ''Parameter #,Start,End,Step''.  Maximum of',I3
     A   /' Enter ''0,0,0,0'' When done.')
  437 FORMAT (/' Enter: ''Parameter #''s to output''.  Maximum of',I3 /
     A   ' Enter 0 when done.')
  447 FORMAT (/' Would you like to:'
     A       //' 1. See a listing of studies in the study file'
     B        /' 2. Re-enter the study number'
     C        /' 3. Quit this option'
     D       //' Please make a selection: ')
  457 FORMAT (/' Invalid selection, please try again.')
  467 FORMAT (/' Change study date? (Y/N) ')
  477 FORMAT (/' The data from the file contains calculated values.'
     A ' These values may present'
     B/' a problem if new calculations are going to be done. It is'
     C/' suggested that you have the calculated values removed unless'
     D/' you know that they will not interfere with your current study.'
     E//' Would you like to retain only original data? (Y/N) ')
C
C     FOLLOWING FORMATS ARE FOR PATIENT DATA BASE
C
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
C     INPUT INFO.  ASK IF SOURCE IS CRT OR FILE
C
      CALL CLEAR
      IF (IOPT .EQ. 99) GOTO 122
      WRITE (CRTO,347)
   20 WRITE (CRTO,207)
      READ (CRTI,'(A)')  IDEV
      IF (IDEV .EQ. IFILE)  GO TO 100
      IF (IDEV .EQ. IFILE1) GO TO 100
      IF ((IDEV .NE. ICRT) .AND. (IDEV .NE. ICRT1))  GO TO 20
C
C     CRT INPUT.  INITIALIZE FLAGS AND VALUES
C
      DO 40  J = 1,75
      IFLAG (J) = 0
      PARAM (J) = -1.0
   40 CONTINUE
      CHANGE = .FALSE.
C
C     GET I.D.
C
   45 WRITE (CRTO,217)
      READ (CRTI,127)  NAME
      WRITE (CRTO,227)
      READ (CRTI,*)  ISTUDY
   47 WRITE (CRTO,237)
      READ (CRTI,137)  IDATE
      IF (CHANGE)  GO TO 130
C
C     INPUT EXPERIMENTAL, THEN ASSUMED DATA
C
   50 WRITE (CRTO,247)
      DO 70  K2 = 1,2
      WRITE (CRTO,257)  (TITLE2(J,K2), J=1,3)
C
   60 READ (CRTI,*) JVAR,VAL
      IF (JVAR .EQ. 0 .AND. VAL .EQ. 0.0)  GO TO 70
      IF (JVAR .GE. 1 .AND. JVAR .LE. NVAR)  GO TO 65
      WRITE (CRTO,267)
      GO TO 60
   65 IFLAG (JVAR) = K2
      IF (VAL .EQ. -1.0)  IFLAG (JVAR) = 0
      PARAM (JVAR) = VAL
      GO TO 60
C
   70 CONTINUE
C
C     SET DEFAULTS FOR UNSPECIFIED VARIABLES
C
      CALL DEF
      GO TO 200
C
C     INPUT INFO FROM DATABASE FILE
C
  100 WRITE (CRTO,227)
      LMNO = 1
      READ (CRTI,*)  ICHECK
      DO 110  K2 = 1,MAX
      LMNO = (K2-1) * 9 + 1
      READ (FILE,507,REC=LMNO)  NAME, ISTUDY, IDATE, (IFLAG(I), I=1,19)
      LMNO = LMNO + 1
      IF (ISTUDY .EQ. ICHECK)  GO TO 120
  110 CONTINUE
      GO TO 9990
C
C     GET REST OF PATIENT DATA
C
  120 READ (FILE,517,REC=LMNO)  (IFLAG(I), I=20,59)
      LMNO = LMNO + 1
      READ (FILE,527,REC=LMNO)  (IFLAG(I), I=60,75), (PARAM(I), I=1,7)
      LMNO = LMNO + 1
      READ (FILE,537,REC=LMNO)  (PARAM(I), I=8,21)
      LMNO = LMNO + 1
      READ (FILE,547,REC=LMNO)  (PARAM(I), I=22,34)
      LMNO = LMNO + 1
      READ (FILE,557,REC=LMNO)  (PARAM(I), I=35,48)
      LMNO = LMNO + 1
      READ (FILE,567,REC=LMNO)  (PARAM(I), I=49,61)
      LMNO = LMNO + 1
      READ (FILE,577,REC=LMNO)  (PARAM(I), I=62,66)
      LMNO = LMNO + 1
      READ (FILE,587,REC=LMNO)  (PARAM(I), I=67,75)
      LMNO = LMNO + 1
C
      CALL DEF
  122 WRITE (CRTO,357)  NAME,ISTUDY,IDATE
C CHECK DATA INPUT IF CALCULATED VALUES ARE PRESENT, ASK ABOUT REMOVAL
      CALL CALCK (CALCS,IFLAG)
      IF (.NOT.(CALCS)) GO TO 200
      WRITE (CRTO,477)
      CALL QQQANS (YES)
      IF (.NOT. YES)  GO TO 200
C
C     RETAIN ORIGINAL DATA ONLY.  CHANGE NAME
C
      WRITE (CRTO,417)
      CALL QQQANS (CHANGE)
      IF (CHANGE)  GO TO 45
C ASK TO CHANGE DATE
      WRITE (CRTO,467)
      CALL QQQANS (CHANGE)
      IF (CHANGE) GO TO 47
  130 DO 150  J = 1,NVAR
      IF (IFLAG(J) .NE. 3)  GO TO 150
      IFLAG (J) = 0
      PARAM (J) = -1.0
  150 CONTINUE
C
C     LOOK AT CURRENT DATA & CHANGE IF NEEDED
C
  200 WRITE (CRTO,387)
      CALL QQQANS (YES)
      IF (.NOT.(YES)) GO TO 151
      CALL CLEAR
      IDIVR = NVAR/3
      DO 7895 I=1,IDIVR
      ITT = I + IDIVR
      ITTT = I + IDIVR + IDIVR
 7895 WRITE (CRTO,397)  I,LABEL(I),PARAM(I),UNITS(I),ITT,LABEL(ITT),
     A   PARAM(ITT),UNITS(ITT),ITTT,LABEL(ITTT),PARAM(ITTT),UNITS(ITTT)
      CALL QQQANS (YES)
  151 WRITE (CRTO,407)
      CALL QQQANS (YES)
      IF (YES)  GO TO 50
C
C     GET LABEL & OPTIONS
C
      WRITE (CRTO,117)
      CALL QQQANS (YES)
      IF (YES) CALL HFILE (LMNO)
      WRITE (CRTO,367)
      READ (CRTI,127)  TITLE
C     WRITE (CRTO,287)
C     CALL QQQANS (YES)
      WRITE (CRTO,297)
  250 WRITE (CRTO,307)
      READ (CRTI,*) IOPT
      IF (IOPT .EQ. 5) GO TO 20
      IF (IOPT .EQ. 99)  STOP 99
      IF (IOPT .LT. 1 .OR. IOPT .GT. 4)  GO TO 250
      IF (IOPT .LT. 3)  GO TO 9900
      IF (IOPT .EQ. 4)  GO TO 400
C
C     OPTION 3 INFO
C
      WRITE (CRTO,427)  MAXVAR
      DO 320  J = 1,MAXVAR
      READ (CRTI,*)  NVARY(J),VALUE(J,1),VALUE(J,2),STEP(J)
      IF (NVARY(J) .EQ. 0)  GO TO 340
      JSUB = NVARY (J)
      PARAM (JSUB) = VALUE (J,1)
      IFLAG (JSUB) = 4
  320 CONTINUE
  340 NUMVAR = J - 1
C
      WRITE (CRTO,437)  MAXVAR
      DO 360  J = 1,MAXVAR
      READ (CRTI,*)  NOUT(J)
      IF (NOUT(J) .EQ. 0)  GO TO 380
  360 CONTINUE
  380 NUMOUT = J - 1
      CALL DEF
      GO TO 9900
C
C     OPTION 4 (PLOT) INFO
C
  400 CALL PLT INP
      CALL DEF
C
 9900 RETURN
  500 CALL HYPXMN (*9991)
C
 9990 WRITE (CRTO,277) ICHECK
 9991 WRITE (CRTO,447)
      READ (CRTI,*) IQUERY
      GOTO (500,100,9992), IQUERY
      WRITE (CRTO,457)
      GO TO 9991
 9992 IOPT = 5
      GO TO 9900
      END
