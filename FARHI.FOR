      SUBROUTINE FARHI
C   ./ i 1
C     FARHI guesses a Venous PCO2 based on the calculated Total Venous
C     CO2 Content.
C     A factor of 100 converts the stored value from percent to decimal
      CHARACTER*4 NAME
C   ./ i 2
      REAL LEEWAY
      COMMON ICOM1(176),NAME(7),ISTUDY,IDATE(3),IFLAG(75),NVAR,NTABLE,
     A   PARAM(75),ICOM2(96)
      EQUIVALENCE (V PH,PARAM (18)), (XV CO2 T,PARAM (59)),
     A  (FVPCO2,PARAM(70)), (XV PCO2,PARAM (60))
      DATA PMIN,PMAX /20.0,90.0/
C   ./ i 7
      DATA LEEWAY /0.003/
C
      RLOW  = PMIN
      RHIGH = PMAX
      T XV CO2 = XV CO2 T / 100.0
C   ./ r 12 28
   10 IF (RLOW .GE. RHIGH) THEN
         FV PCO2 = -99.0
         IFLAG(70) = 3
         RETURN
      ELSE
         RMID = (RHIGH + RLOW) / 2.0
         RGUESS = ATCO (RMID)
         RTRY = T XV CO2 - RGUESS
         IF (ABS(RTRY) .LE. LEEWAY) THEN
              FV PCO2 = RMID
              IFLAG(70) = 3
              GO TO 9900
         ELSEIF (RTRY .GT. LEEWAY) THEN
              RLOW = RMID + 1.0
              GO TO 10
         ELSEIF (RTRY .LT. -LEEWAY) THEN
              RHIGH = RMID - 1.0
              GO TO 10
         ENDIF
      ENDIF
 9900 IF (V PH .LT. -1.0) THEN
         V PH = - V PH
         IFLAG(18) = 6
      ELSEIF ((V PH .GE. -1.0) .AND. (IFLAG(18) .NE. 6)) THEN
         V PH = 8.0 - ALOG10((24.0 / 26.0) + (FV PCO2 / 10.0))
         IFLAG(18) = 5
      ENDIF

      RETURN
      END
