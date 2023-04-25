C   ./ i 0
$STORAGE:2
      SUBROUTINE CHART3 (ISUB,NKNOWN)
      REAL*4 K,KATPS
      CHARACTER*4 NAME
      COMMON ICOM1(176), NAME(7),ISTUDY,IDATE(3),IFLAG(75),NVAR,NTABLE,
     A PARAM(75), ICOM2(96)
      EQUIVALENCE (PB,PARAM(2)), (A PCO2,PARAM(4)), (RR,PARAM(25)),
     A (RMV,PARAM(26)), (VCO2,PARAM(53)), (VDVT,PARAM(67)),
     B (PECO2,PARAM(68)), (FECO2,PARAM(69)), (A PH,PARAM(10)),
     C (FACO2T,PARAM(71)), (VOL BOX,PARAM(72))
C
      DATA K /0.7969/
      ISUB26 = ISUB - 26
      GO TO (2700,2800,2900), ISUB26
C
C************************************************
C  SUBROUTINE BOHR                   CHART 27
C
C    VD/VT = A PC02 - PECO2 / A PCO2
C
C
 2700 IF ((RR .EQ. -1.0) .OR. (RMV .EQ. -1.0)) GO TO 9900
      GO TO (2710,2720,2730), NKNOWN
 2710 A PCO2 = PECO2 * (RMV / (RMV - VDVT * RMV - VOL BOX * RR))
      IF (A PH .EQ. -1.0) IFLAG (10) = 3
      IF (A PH .EQ. -1.0) A PH = 8.0 - ALOG10(A PCO2 / 10.0)
      IFLAG (4) = 3
* Below may be the correct interpretation of this function.
* FA CO2 T is also calculated in GILBERT but it is multiplied by 100.
      FACO2T = ATCO (A PCO2)
      IFLAG (72) = 3
C
      GO TO 9900
C
 2720 PECO2 = A PCO2 / (RMV / (RMV - VDVT * RMV - VOL BOX * RR))
      IFLAG (68) = 3
      GO TO 9900
C
 2730 GO TO 9900
C This is a break from tradition, but I don't want to solve for
C VDVT yet, therefore, if it is not given, it will not be calculated.
C DWW 7/8/85
C     VDVT = (A PCO2 - PECO2) / A PCO2
C     IFLAG (67) = 3
C     GO TO 9900
C
C************************************************
C  SUBROUTINE FECO2                  CHART 28
C
C  FECO2 = VCO2 / MIN VOL
C
C MULTIPLY BY FACTOR OF 100 TO GET PERCENT
C
 2800 IF (FECO2 .NE. -1.0) FRAC = FECO2 / 100.0
      GO TO (2810,2820,2830), NKNOWN
C
C Convert VCO2 to BTPS, and RMV from ATPS to BTPS
 2810 FRAC = (VCO2 / K) / RMV
      FECO2 = FRAC * 100.0
      IFLAG (69) = 3
      GO TO 9900
C
C Convert VCO2 to BTPS using K
 2820 RMV = (VCO2/K) / FRAC
      IFLAG (26) = 3
      GO TO 9900
C
C Convert from BTPS to STPD
 2830 VCO2 = RMV * K * FRAC
      IFLAG (53) = 3
      GO TO 9900
C
C************************************************
C  SUBROUTINE PECO2                  CHART 29
C
C  PECO2 = FECO2 * (BAROMETRIC PRESSURE - 47.0)
C
C
 2900 IF (PB .EQ. -1.0) GO TO 9900
      GO TO (2910,2920), NKNOWN
C
 2910 FRAC = FECO2 / 100.0
      PECO2 = FRAC * (PB - 47.0)
      IFLAG (68) = 3
      GO TO 9900
C
 2920 FRAC = PECO2 / (PB - 47.0)
      FECO2 = FRAC * 100.0
      IFLAG (69) = 3
      GO TO 9900
C
 9900 RETURN
      END
