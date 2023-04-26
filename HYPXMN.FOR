      SUBROUTINE HYP XMN (*)
C
C     GET LIST OF PATIENTS IN DATA BASE FILE
C
      IMPLICIT INTEGER (A-Z)
      CHARACTER*4 NAME(7)
      LOGICAL YES
      DIMENSION DATE(3)
C
      DATA MAX /11/, CRTO,FILE /0,10/
C
  107 FORMAT (//25X,'Hypoxia Data Base' // 13X,'Name',17X,'Study #',
     A   8X,'Date' / 1X,28(1H-),5X,8(1H-),5X,9(1H-) /)
  117 FORMAT (7A4,I8,3I2,44X)
  127 FORMAT (1X,7A4,I13,I7,2('/',I2))
  137 FORMAT (/'  End of Data ',
     A       //'  Press RETURN to continue ... ')
C
      CALL CLEAR
      WRITE (CRTO,107)
      DO 50  J = 1,MAX
      LMNO = (J-1) * 9 + 1
      READ (FILE,117,REC=LMNO)  NAME,STUDY,DATE
      IF (STUDY .NE. -999)  WRITE (CRTO,127)  NAME,STUDY,DATE
   50 CONTINUE
C
      WRITE (CRTO,137)
      CALL QQQANS (YES)
      RETURN 1
      END
