      SUBROUTINE HYPLST (*)
      LOGICAL OUTPUT(75)
      CHARACTER*6 UNITS
      CHARACTER*8 LABEL
      INTEGER CRTI,CRTO
      COMMON /PHRED1/ LABEL(75)
      COMMON /PHRANK/ UNITS(75)
      COMMON /LIMITS/ YLO(75), YHI(75)

      DATA NUMVAR /69/
      DATA OUTPUT /27*.TRUE.,2*.FALSE.,4*.TRUE.,2*.FALSE.,.TRUE.,
     A   3*.FALSE.,4*.TRUE.,9*.FALSE.,.TRUE.,6*.FALSE.,2*.TRUE.,
     B   4*.FALSE.,4*.TRUE.,2*.FALSE.,.TRUE.,3*.FALSE./
      DATA IPRN,CRTO,CRTI /99,0,0/

  107 FORMAT (/' How many copies of the worksheet would you like? '\)
  117 FORMAT ('1'/30X,'Hypoxia Worksheet'/' Number',4X,
     A 'Experimental',2X,
     X 'Parameter',3X,'Assumed',6X,'Range',10X,'Units'/' ',(70('='))/)
  127 FORMAT ('   ',I2,5X,(12('_')),2X,A,2X,(12('_')),2X,'(',F5.1,' - ',
     A F6.1,') ',A)
  137 FORMAT (' ')
      WRITE (CRTO,107)
      READ (CRTI,'(I2)') NUMBER
      DO 100 I=1,NUMBER
      WRITE (IPRN,117)
      DO 100 J=1,NUMVAR
      IF (OUTPUT(J)) WRITE (IPRN,127) J,LABEL(J),YLO(J),YHI(J),UNITS(J)
  100 CONTINUE
      WRITE (IPRN,137)
      RETURN 1
      END
