$STORAGE:2
      PROGRAM HYPOX
      LOGICAL YES
      CHARACTER*3 IFILE
      CHARACTER*8 FILNAM
      CHARACTER*4 NAME,ITITL
      INTEGER CRTI,CRTO,FILE,FORM,FTEMP,FPLOT
      COMMON  LIST(100),IPARM(2,35),  CRTI,CRTO,FILE,FORM,FTEMP,FPLOT,
     A   NAME(7),ISTUDY,IDATE(3),IFLAG(75),NVAR,NTABLE,
     B   PARAM(75),  NVARY(15),VALUE(15,2),STEP(15),ITITL(18),NUMVAR,
     C   MAXVAR,NOUT(15),NUMOUT
      COMMON /PHRED/ IPRN
C
C     HYPOXIA ANALYSIS STUDY PROTOTYPE 5/22/79  (1-SEP-81)
C       GARY KESSLER   KUMQUAT VERSION 7.11
C     MODIFIED FROM XEROX SIGMA 6 (UVM) FOR STANDARD FORTRAN AND
C        IBM SYSTEM/34 (SMC).SOME MODS TO CODE & VISCOSITY ADDED.
C     MODS FOR PDP-11/44, 1-SEP-81
C     MODS FOR IBM 4341/PC AUGUST 84 TO NOVEMBER 85
C     MODS FOR USARIEM OCTOBER 1985
C
C     EXTERNAL DOCUMENTATION GIVES DETAILS AS TO MEANINGS OF COMMON
C     VARIABLES AND PARAMETER NAMES & UNITS.
C
C     I/O DEVICES:
C     CRTI (0)    TERMINAL INPUT
C     CRTO (0)    TERMINAL OUTPUT
C     FILE (10)   PATIENT DATA BASE FILE 'HYPOX.DAT'
C     FTEMP (11)  TEMPORARY FILE FOR PROGRAM 'HYPOX.TMP'
C     FPLOT (12)  PLOT OUTPUT INFORMATION FILE 'HYPLT.DAT'
C     FORM (50)   OUTPUT FILE 'HYPOX.OUT'
C     IPRN (99)   PRINTER
C
C     All other programs in Kessler's original documentation
C     have been incorporated into the main program. To use
C     these, select that option in the main menu. See documentation
C     by David Wormuth for current operation.
C
C     PLOT PROGRAM 'HYPLOT' USES DATA IN A FILE CALLED 'HYPLT.DAT'
C     'HYPOX.OUT' HAS FORTRAN CARRIAGE CONTROL FEATURES
C
C     PROGRAM 'HYPDAT' CREATES DATA BASE FILE.  IT CAN BE MODIFIED
C     TO ADD PATIENTS TO THAT FILE
C     USE 'HYPDEL' TO DELETE A PATIENT FROM THE DATA BASE
C
C     SUBPROGRAMS:
C     FILE NAMES
C     ------     --------------------------------------------------
C     HYPIO       .MAIN., READ, HLIST, WRITE, WRITE2, GILBERT, HFILE,
C                 REPORT,
C     HYPMAN      HYPBD, DEF, QQQANS, CALCK, CHRT OK, GT CHRT, SETNGO
C     HYPCHART    CHART, CHART2, CHART3, CSH, DIFF, MIT, ATCO, FAHRI,
C                 ROOT, BL CO2, BL O2, O2 DISS
C     HYPADDS     CREDIT, PLT INP, HYPDEL, HYPXMN, HYPLST,
C                 HYPLOT, CLEAR
C
  107 FORMAT (//' *** Hypoxia Study completed ***'/)
  117 FORMAT (/' New case study? (Y/N)'\)
  127 FORMAT (' Calculation loop #',I5)
  157 FORMAT (/' Would you like to continue with data for ',7A,'? (Y/N)'
     A   \)
  167 FORMAT (//////'                 *** HYPOXIA SYSTEM  V85.NOV ***'
     A            //'                 1. Hypoxia Program'
     B             /'                 2. Produce Hypoxia Worksheet'
     C             /'                 3. List Patients in Data File'
     D             /'                 4. Delete Study from Data File'
     E             /'                 5. Print graph'
     F             /'                 6. Quit Hypoxia System'
     G            //'                 Please Make a Selection: '\)
  177 FORMAT (/' Invalid selection, please try again')
C
C     INITIALIZATION AND FILE OPENING
C
      CALL HYP BD
      IFILE = 'NUL'
      WRITE (*,1)
    1 FORMAT (' Would you like to list results on the printer? (Y/N) '\)
      CALL QQQANS(YES)
      IF (YES) IFILE = 'PRN'
      OPEN (IPRN,FILE=IFILE,STATUS='NEW',ACCESS='SEQUENTIAL',
     A   FORM='FORMATTED')
C     WRITE (*,2)
C   2 FORMAT (' Enter the name of the DATA file to use: [HYPOX.DAT]: '\)
C     READ (*,'(A)') FILNAME
C     IF (FILNAM .EQ. '') FILNAM = 'HYPOX.DAT'
      OPEN (FILE,FILE='EXERCISE.DAT',STATUS='OLD',ACCESS='DIRECT',
     A   FORM='FORMATTED',RECL=80)
      OPEN (FTEMP)
      OPEN (FPLOT,FILE='HYPLT.DAT',STATUS='NEW',ACCESS='SEQUENTIAL',
     A   FORM='FORMATTED')
      OPEN (FORM,FILE='EXERCISE.OUT',STATUS='NEW',ACCESS='SEQUENTIAL',
     A   FORM='FORMATTED')
C
C     WRITE MAIN MENU AND TAKE ACTION REQUESTED
C
      CALL CLEAR
      CALL CREDIT
   10 CALL CLEAR
      WRITE (CRTO,167)
      READ (CRTI,*) IQUERY
      GOTO (50,2000,3000,4000,5000,9900), IQUERY
      WRITE (CRTO,177)
      GO TO 10
 2000 CALL HYPLST (*10)
 3000 CALL HYPXMN (*10)
 4000 CALL HYPDEL (*10)
 5000 CALL HYPLOT (*10)
   50 CALL READ (IOPT,LMNO)
      GO TO (100,200,300,400), IOPT
      GO TO 10
C
  100 CALL GT CHRT
  200 CALL WRITE (1,LMNO)
      GO TO 900
C
C     VARY PARAMETERS WITH MORE THAN 1 STEP.  CREATE TABLE
C
  300 CALL GT CHRT
      CALL WRITE (4,LMNO)
C
  320 K = NUMVAR
  340 IF (K .LE. 0)  GO TO 900
      K2 = NVARY (K)
      PARAM (K2) = PARAM (K2) + STEP (K)
      IF ((STEP(K) .LT. 0 .OR. PARAM(K2) .LE. VALUE(K,2)) .AND.
     A    (STEP(K) .GE. 0 .OR. PARAM(K2) .GE. VALUE(K,2)))  GO TO 360
      PARAM (K2) = VALUE (K,1)
      K = K - 1
      GO TO 340
  360 CALL SETNGO
      CALL WRITE (4,LMNO)
      GO TO 320
C
C     PLOT
C
  400 CALL GT CHRT
      CALL WRITE (5,LMNO)
      KNT = 0
C
  420 K = 1
  440 IF (K .GT. 2)  GO TO 900
      K2 = NVARY (K)
      PARAM (K2) = PARAM(K2) + STEP (K)
      IF (PARAM(K2) .LE. VALUE(K,2))  GO TO 460
      PARAM (K2) = VALUE (K,1)
      K = K + 1
      GO TO 440
  460 CALL SETNGO
      CALL WRITE (5,LMNO)
      KNT = KNT + 1
      IF (KNT/50*50 .EQ. KNT)  WRITE (CRTO,127)  KNT
      GO TO 420
C
  900 WRITE (CRTO,157) NAME
      CALL QQQANS (YES)
      IF (YES) THEN
         IOPT = 99
         GOTO 50
      ENDIF
      WRITE (CRTO,117)
      CALL QQQANS (YES)
      IF (YES)  GO TO 50
      GO TO 10
 9900 WRITE (CRTO,107)
C
      CLOSE (FILE,STATUS='KEEP')
      CLOSE (FPLOT,STATUS='KEEP')
      CLOSE (FORM,STATUS='KEEP')
      STOP
      END