      SUBROUTINE CHART (ISUB,NKNOWN)
C
C     'CONDENSED' CHART SUBROUTINE FOR HYPOXIA.  CHARTS 9-13 AND 15-18
C
      DIMENSION A(2), B(2)
      CHARACTER*4 NAME
      COMMON ICOM1(176), NAME(7),ISTUDY,IDATE(3),IFLAG(75),NVAR,NTABLE,
     A   PARAM(75),  ICOM2(96)
      EQUIVALENCE (ALT,PARAM(1)), (PB,PARAM(2)), (A PCO2,PARAM(4)),
     A   (A PO2,PARAM(9)), (A PH,PARAM(10)), (A SO2,PARAM(11)),
     B   (O2 CAP,PARAM(13)), (Q,PARAM(16)), (HCO3,PARAM(23)),
     C   (TIDAL,PARAM(24)), (RR,PARAM(25)), (RMV,PARAM(26)),
     D   (A PCO,PARAM(27)), (A SCO,PARAM(28)), (A SCO2,PARAM(29)),
     E   (SV,PARAM(31)), (HR,PARAM(32)), (HGB,PARAM(33)),
     F   (AGE,PARAM(40)), (SEX,PARAM(41)), (HT,PARAM(42)),
     G   (WT,PARAM(43)), (BSA,PARAM(44)), (V BR CAP,PARAM(45)),
     H   (CI,PARAM(46)), (A CO TOT,PARAM(47)), (V HR,PARAM(54))
C
      DATA A /86.5,71.3/, B /0.522,0.474/
      DATA  PASL,AA,BB /760.0,44346.236,5.256/
      DATA  C1,C2 /0.107,0.415/
C
      ISUB8 = ISUB - 8
      GO TO (900,1000,1100,1200,1300,9900,1500,1600,1700,1800), ISUB8
C
C***************************************
C     SUBROUTINE BICARB    CHART 9
C     BLOOD HCO3- ION CONCENTRATION, FROM MC LEAN, PHYSIOL. REV., 1938.
C        HENDERSON-HASSELBACH EQUATION.
C
C     PH = PK' (CO2) + LOG (HCO3 / DISSOLVED CO2)
C        WHERE PK' (CO2) = 6.16 & DISS. CO2 = 0.0301 * PCO2
C        2.22 FACTOR TO CONVERT MILLIMOLES/LITER TO VOLS PCT.
C
  900 GO TO (910,920,930), NKNOWN
  910 A PCO2 = HCO3 / (0.668 * 10.0 ** (A PH - 6.16))
      IFLAG (4) = 3
      GO TO 9900
C
  920 A PH = 6.16 + ALOG10 (HCO3 / (0.668 * A PCO2))
      IFLAG (10) = 3
      GO TO 9900
C
  930 HCO3 = A PCO2 * 0.0668 * 10.0 ** (A PH - 6.16)
      IFLAG (23) = 3
      GO TO 9900
C
C *************************************
C     SUBROUTINE MIN VOL     CHART 10
C     RESPIRATORY MINUTE VOLUME
C
C     RESP MIN VOL = TIDAL VOL * RESP RATE
C
 1000 GO TO (1010,1020,1030), NKNOWN
 1010 TIDAL = RMV / RR
      IFLAG (24) = 3
      GO TO 9900
C
 1020 RR = RMV / TIDAL
      IFLAG (25) = 3
      GO TO 9900
C
 1030 RMV = TIDAL * RR
      IFLAG (26) = 3
      GO TO 9900
C
C***************************************
C     SUBROUTINE CARDIAC OUT   CHART 11
C     CARDIAC OUTPUT CALCULATION
C
C     CARDIAC OUTPUT = STROKE VOLUME * HEART RATE
C      FACTOR OF 1000 TO CONVERT ML TO LITERS
C
 1100 GO TO (1110,1120,1130), NKNOWN
 1110 Q = SV * HR / 1000.0
      IFLAG (16) = 3
      GO TO 9900
C
 1120 SV = Q * 1000.0 / HR
      IFLAG (31) = 3
      GO TO 9900
C
 1130 HR = Q * 1000.0 / SV
      IFLAG (32) = 3
      GO TO 9900
C
C***************************************
C     SUBROUTINE HEMO     CHART 12
C     O2 CAPACITY & HEMOGLOBIN CALCULATIONS.  REF - WEST, 1974.
C
C     O2 CAPACITY = 1.39 * HEMOGLOBIN
C        SINCE 1.39 ML O2 COMBINES WITH 1 GM HGB
C
 1200 IF (NKNOWN .EQ. 2)  GO TO 1220
      O2 CAP = 1.39 * HGB
      IFLAG (13) = 3
      GO TO 9900
C
 1220 HGB = O2 CAP / 1.39
      IFLAG (33) = 3
      GO TO 9900
C
C***************************************
C     SUBROUTINE PB ALT      CHART 13
C     ALTITUDE, BAROMETRIC PRESSURE APPROXIMATION, FROM ERB, OFF BELAY
C        #39, JUNE, 1978, PG. 2.
C
C     ALT = AA (1 - (PB / PRESS AT SEA LEVEL) ** (1/BB))
C
 1300 IF (NKNOWN .EQ. 2)  GO TO 1320
      ALT = AA * (1.0 - (PB / PASL) ** (1.0 / BB))
      IF (ALT .LT. 0.0)  ALT = 0.0
      IFLAG (1) = 3
      GO TO 9900
C
 1320 PB = PASL * (1.0 - ALT / AA) ** BB
      IF (PB .GT. 760.0)  PB = 760.0
      IFLAG (2) = 3
      GO TO 9900
C
C***************************************
C     SUBROUTINE CO HALDANE    CHART 15
C     HBCO SATURATION & CO CONCENTRATION,A LA HALDANE EQN.
C        REF - GOLDSMITH & FRIBERG, 1977.
C
C     PCT. HBCO / PCT. HBO2 = 245 * PCO / PO2
C     CO CONCENTRATION = PCT. HBCO / 0.16  (PPM)
C
 1500 A SCO = A SO2 * 245.0 * A PCO / (A PO2 * 100.0)
      A CO TOT = A SCO / 1600.0
      IFLAG (28) = 3
      IFLAG (47) = 3
      GO TO 9900
C
C***************************************
C     SUBROUTINE BSA MBC      CHART 16
C     BODY SURFACE AREA & MAXIMUM BREATHING CAPACITY.  CALCULATIONS
C        FROM MERCK MANUAL.  MAXIMUM HEART RATE FROM CSH, 4/78
C
 1600 BSA = (WT ** 0.425) * (HT ** 0.725) * 0.007184
      IFLAG (44) = 3
      IF (AGE .EQ. -1)  GO TO 9900
      V HR = 210.0 - 0.65 * AGE
      IFLAG (54) = 3
      IF (SEX .EQ. -1)  GO TO 9900
      IS = IFIX (SEX)
      V BR CAP = (A (IS) - (B (IS) * AGE)) * BSA
      IFLAG (45) = 3
      GO TO 9900
C
C***************************************
C     SUBROUTINE CARD INDEX   CHART 17
C     CARDIAC INDEX CALCULATION, FROM MERCK MANUAL
C
 1700 CI = Q / BSA
      IFLAG (46) = 3
      GO TO 9900
C
C***************************************
C     SUBROUTINE CO2 GUYTON   CHART 18
C     HBCO2 DISS. CURVE APPROX. (WITHIN 1 PCT.), FROM GUYTON, 1971.
C
 1800 A SCO2 = C1 * (A PCO2 ** C2)
      IFLAG (29) = 3
C
 9900 RETURN
      END
