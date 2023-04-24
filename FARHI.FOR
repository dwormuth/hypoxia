      SUBROUTINE FARHI
      CHARACTER*4 NAME
      COMMON ICOM1(176),NAME(7),ISTUDY,IDATE(3),IFLAG(75),NVAR,NTABLE,
     A   PARAM(75),ICOM2(96)
      EQUIVALENCE (V PH,PARAM (18)), (XV CO2 T,PARAM (59)),
     A  (FVPCO2,PARAM(70)), (XV PCO2,PARAM (60))
      DATA PMIN,PMAX /20.0,90.0/
C
      RLOW  = PMIN
      RHIGH = PMAX
      T XV CO2 = XV CO2 T / 100.0
   10 IF (RLOW .GE. RHIGH) FVPCO2 = -99.0
      IF (RLOW .GE. RHIGH) RETURN
      RMID = (RHIGH + RLOW) / 2.0
      RGUESS = ATCO (RMID)
      IF (ABS(T XV CO2  - RGUESS) .LE. .003) FVPCO2 = RMID
      IF (ABS(T XV CO2  - RGUESS) .LE. .003) GO TO 9900
      IF ((T XV CO2 - RGUESS) .GT. .003) RLOW = RMID + 1.0
      IF ((T XV CO2 - RGUESS) .GT. .003) GO TO 10
      IF ((T XV CO2 - RGUESS) .LT. -.003) RHIGH = RMID - 1.0
      IF ((T XV CO2 - RGUESS) .LT. -.003) GO TO 10
 9900 IFLAG (70) = 3
C There is a problem here with setting a pH and using it repeatedly
C at the present time, only the first cycle will use this pH, all
C others will have a new pH calculated via the routine below. 7/18/85DWW
      IF (V PH .GE. 0.0) V PH = 8.0 - ALOG10(24.0/26.0 * FV PCO2/10.0)
      IF (V PH .GE. 0.0) IFLAG (18) = 5
      IF (V PH .LT. 0.0) V PH = - V PH
      RETURN
      END
