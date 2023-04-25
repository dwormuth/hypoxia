C   ./ i 0
$STORAGE:2
      SUBROUTINE REPORT(*,BHOLD,SO2,PH,PO2,TEMP,PCO2,NKNOWN)
      LOGICAL YES
  107 FORMAT (' From subroutine O2DISS. There has been an attempt to'
     A   'calculate a negative'
     B/' square root. The following values were input to the subroutine'
     C//'  BHOLD = ',F14.5,
     D /'     pH = ',F14.5,
     E /'    SO2 = ',F14.5,
     F /'    PO2 = ',F14.5,
     G /'   Temp = ',F14.5,
     H /'   PCO2 = ',F14.5,
     I /' NKNOWN = ',I2)
      WRITE (*,107) BHOLD,PH,SO2,PO2,TEMP,PCO2,NKNOWN
      RETURN 1
      END
