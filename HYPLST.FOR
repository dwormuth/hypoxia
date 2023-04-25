C   ./ i 0
$STORAGE:2
      SUBROUTINE HYPLST (*)
      LOGICAL OUTPUT(75)
C   ./ i 2
      CHARACTER*4 NAME,ITITL
      CHARACTER*6 UNITS
      CHARACTER*8 LABEL
C   ./ r 5
      INTEGER CRTO,CRTI,FILE,FORM,TEMP,FPLOT
      COMMON /PHRED1/ LABEL(75)
      COMMON /PHRANK/ UNITS(75)
      COMMON /LIMITS/ YLO(75), YHI(75)
C   ./ i 8
      COMMON ICOM1(170),CRTI,CRTO,FILE,FORM,TEMP,FPLOT,
     A   NAME(7),ISTUDY,IDATE(3),IFLAG(75),NVAR,NTABLE,PARAM(75),
     B   NVARY(15),VALUE(15,2),STEP(15),ITITL(18),NUMVAR,MAXVAR,
     C   NOUT(15),NUMOUT
      COMMON /PHRED/ IPRN

C   ./ d 10
      DATA OUTPUT /27*.TRUE.,2*.FALSE.,4*.TRUE.,2*.FALSE.,.TRUE.,
     A   3*.FALSE.,4*.TRUE.,9*.FALSE.,.TRUE.,6*.FALSE.,2*.TRUE.,
     B   4*.FALSE.,4*.TRUE.,2*.FALSE.,.TRUE.,3*.FALSE./
C   ./ d 14

  107 FORMAT (/' How many copies of the worksheet would you like? '\)
C   ./ r 17
  117 FORMAT (' '/30X,'Hypoxia Worksheet'/' Number',4X,
     A 'Experimental',2X,
     X 'Parameter',3X,'Assumed',6X,'Range',10X,'Units'/' ',(70('='))/)
  127 FORMAT ('   ',I2,5X,(12('_')),2X,A,2X,(12('_')),2X,'(',F5.1,' - ',
     A F6.1,') ',A)
  137 FORMAT (' ')
      WRITE (CRTO,107)
      READ (CRTI,'(I2)') NUMBER
C   ./ r 25
      DO 200 I = 1,NUMBER
      WRITE (IPRN,117)
C   ./ r 27
      DO 100 J = 1,NVAR
      IF (OUTPUT(J)) WRITE (IPRN,127) J,LABEL(J),YLO(J),YHI(J),UNITS(J)
  100 CONTINUE
      WRITE (IPRN,137)
C   ./ i 30
  200 WRITE (IPRN,'(''1'')')
      RETURN 1
      END
