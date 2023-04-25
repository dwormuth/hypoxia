C   ./ i 0
$STORAGE:2
      SUBROUTINE PLT INP
C
C     HANDLE PLOT INFO INPUT FOR *HREAD*
C
      INTEGER CRTI,CRTO,FILE,FORM,TEMP,FPLOT,EOD
      CHARACTER*4 TITLE, TITLE2(3,2),NAME
      COMMON ICOM1(170), CRTI,CRTO,FILE,FORM,TEMP,FPLOT,
     A   NAME(7),ISTUDY,IDATE(3),IFLAG(75),NVAR,NTABLE, PARAM(75),
     B   NVARY(15),VALUE(15,2),STEP(15),TITLE(18),NUMVAR,MAXVAR,
     C   NOUT(15),NUMOUT
C
      DATA EOD /-99/
C
C   ./ r 14
  377 FORMAT (/' Enter parameter number of labels to appear on the'
     A   ' graph.'/'  Enter '0' when done.')
  447 FORMAT (I3,3F10.2)
  457 FORMAT (/' Enter X-axis:  ''Parameter #, Minimum, Maximum'':')
  467 FORMAT (/' Enter Y-axis:  ''Parameter #'':')
  477 FORMAT (/' Enter Z: ''Parameter, Minimum, Maximum, (# Curves)'':')
  487 FORMAT (I6/7A4)
C
      REWIND FPLOT
      WRITE (FPLOT,487) ISTUDY,NAME
      WRITE (CRTO,377)
      DO 420  J = 1,16
      READ (CRTI,*)  JVAR
      IF (JVAR .EQ. 0)  GO TO 440
      WRITE (FPLOT,447)  JVAR,PARAM(JVAR)
  420 CONTINUE
  440 WRITE (FPLOT,447)  EOD
C
C     X-AXIS
C
  450 WRITE (CRTO,457)
      READ (CRTI,*)  NVARY(1),VALUE(1,1),VALUE(1,2)
      STEP (1) = (VALUE(1,2) - VALUE(1,1)) / 56.0
      IF (STEP (1) .LT. 0.0)  GO TO 450
      JSUB = NVARY (1)
      PARAM (JSUB) = VALUE (1,1)
      IFLAG (JSUB) = 2
      WRITE (FPLOT,447)  NVARY(1),VALUE(1,1),VALUE(1,2)
C
C     Y-AXIS
C
      WRITE (CRTO,467)
      READ (CRTI,*)  NVARY(3)
      WRITE (FPLOT,447)  NVARY(3)
C
C     Z ISOPLETHS
C
  460 WRITE (CRTO,477)
      READ (CRTI,*)  NVARY(2),VALUE(2,1),VALUE(2,2),NC
      IF (NC .GT. 1)  STEP (2) = (VALUE(2,2) - VALUE(2,1)) / (NC - 1)
      IF (STEP (2) .LT. 0.0)  GO TO 460
      JSUB = NVARY (2)
      PARAM (JSUB) = VALUE (2,1)
      IFLAG (JSUB) = 2
      WRITE (FPLOT,447)  NVARY(2),VALUE(2,1),VALUE(2,2)
C
      RETURN
      END
