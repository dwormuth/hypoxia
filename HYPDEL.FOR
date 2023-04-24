      SUBROUTINE HYPDEL (*)
C
C     DELETE A  REQUESTED STUDY FROM HYPOXIA DATA BASE
C
      IMPLICIT INTEGER (A-Z)
      LOGICAL YES
      CHARACTER*4 NAME(7)
      DIMENSION DATE(3)
C
      DATA MAX /11/, CRTI,CRTO,FILE /0,0,10/
C
  107 FORMAT (/' Delete what study? (0 to stop)')
  117 FORMAT (7A4,I8,3I2,44X)
  127 FORMAT (/'  **  Study #',I8,' does not exist **'/)
  137 FORMAT (/' Delete study #',I9,4X,7A4,I6,2('/',I2),'? (Y/N)')
  147 FORMAT (32X,'-999',44X)
  157 FORMAT (80X)
  167 FORMAT (//' Would you like to see a listing of patients in the dat
     Aa file. (Y/N)')
C
   30 WRITE (CRTO,107)
      READ (CRTI,*)  DEL
      IF (DEL .LE. 0)  GO TO 200
      DO 50  J = 1,MAX
      LMNO = (J-1) * 9 + 1
      READ (FILE,117,REC=LMNO)  NAME,STUDY,DATE
      IF (STUDY .EQ. DEL)  GO TO 100
   50 CONTINUE
      WRITE (CRTO,127)  DEL
      WRITE (CRTO,167)
      CALL QQQANS (YES)
      IF (YES) CALL HYPXMN (*30)
      GO TO 30
C
  100 WRITE (CRTO,137)  STUDY,NAME,DATE
      CALL QQQANS (YES)
      IF (.NOT. YES)  GO TO 30
C
      WRITE (FILE,147,REC=LMNO)
      LMNO = LMNO + 1
C
      DO 120  I = 1,8
      WRITE (FILE,157,REC=LMNO)
  120 LMNO = LMNO + 1
      GO TO 30
C
  200 RETURN 1
      END
