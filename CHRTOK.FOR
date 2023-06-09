C   ./ i 0
$STORAGE:2
      SUBROUTINE CHRT OK (OK,NKNOWN,NCHART)
      LOGICAL OK
      CHARACTER*4 NAME
      COMMON LIST (100),IPARM(2,35), ICOM1(6), NAME(7),ISTUDY,IDATE(3),
     A   IFLAG(75),NVAR,NTABLE,  COM2(75), ICOM3(96)
C
C     FIRST, CHECK IF WE HAVE ENOUGH INFO TO USE CHART 'NCHART'.
C     IF NO UNKNOWNS, 'NKNOWN' = 0.  OTHERWISE, 'NKNOWN' HAS THE NO.
C        OF THE MISSING CHART PARAMETER.  'OK' WILL = .TRUE.
C     IF MORE THAN 1 PARAMETER MISSING, 'OK' = .FALSE.
C
      NKNOWN = 0
      OK = .TRUE.
      K2 = IPARM (1,NCHART) - 1
      JLOOP = IPARM (2,NCHART)
      DO 100  J = 1,JLOOP
      K2J = K2 + J
      IVAR = LIST (K2J)
      IF (IFLAG (IVAR) .GT. 0)  GO TO 100
      IF (NKNOWN .EQ. 0 .AND. IFLAG (IVAR) .GE. 0)  GO TO 50
      OK = .FALSE.
      GO TO 500
C
   50 NKNOWN = J
  100 CONTINUE
C
  500 RETURN
      END
