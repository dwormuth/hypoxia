      SUBROUTINE CSH (ISUB,NKNOWN)
C
C     'CONDENSED' CHART SUBROUTINE FOR HYPOXIA.  CHARTS 1-5, 7-8, & 14
C
      DIMENSION II(4,2), JJ(3,2)
      CHARACTER*4 NAME
      COMMON ICOM1(176), NAME(7),ISTUDY,IDATE(3),IFLAG(75),NVAR,NTABLE,
     A   PARAM(75),  ICOM2(96)
      EQUIVALENCE (PB,PARAM(2)), (PIO2,PARAM(3)), (A PCO2,PARAM(4)),
     A   (RQ,PARAM(5)), (FIO2,PARAM(6)), (PAO2,PARAM(7)),
     B   (AAGRAD,PARAM(8)), (A PO2,PARAM(9)), (A PH,PARAM(10)),
     C   (A SO2,PARAM(11)), (AVDIFF,PARAM(12)), (O2 CAP,PARAM(13)),
     D   (VO2,PARAM(15)), (Q,PARAM(16)), (V SO2,PARAM(17)),
     E   (V PH,PARAM(18)), (V PO2,PARAM(19)), (CAP PO2,PARAM(20)),
     F   (TEMP,PARAM(21)), (A O2 DIS,PARAM(34)), (A O2 CMB,PARAM(35)),
     G   (A O2 TOT,PARAM(36)), (V O2DIS,PARAM(37)),
     H   (V O2CMB,PARAM(38)), (V O2 TOT,PARAM(39)), (V PCO2,PARAM(60))
C
      DATA II /11,10,9,4,  17,18,19,60/
      DATA JJ /34,35,36, 37,38,39/
C
      IF (ISUB .EQ. 14)  GO TO 1400
      GO TO (100,200,300,400,500,9900,400,800), ISUB
C
C***************************************
C     SUBROUTINE CHART 1
C     CHART 1 FROM HOUSTON, 6/47.
C     PIO2 = FIO2 * (BAR PRESS - 47)     WHERE NORMAL FIO2 = 20.93 PCT.
C
  100 FRAC = FIO2 / 100.0
      GO TO (110,120,130), NKNOWN
  110 PB = PIO2 / FRAC + 47.0
      IF (PB .GT. 760.0)  PB = 760.0
      IFLAG (2) = 3
      GO TO 9900
C
  120 PIO2 = FRAC * (PB - 47.0)
      IFLAG (3) = 3
      GO TO 9900
C
  130 FIO2 = 100.0 * PIO2 / (PB - 47.0)
      IFLAG (6) = 3
      GO TO 9900
C
C***************************************
C     SUBROUTINE CHART 2
C     CHART 2 FROM HOUSTON, 6/47.
C     PAO2 = PIO2 - ART PCO2 (1 - FIO2 (1 - RQ) / RQ)
C
  200 FRAC = FIO2 / 100.0
C HOLD etc added to correct PaCO2 to PACO2 via Downstream correction
C used only for Alveolar air equation. 6/20/85 DWW
      HOLD = A PCO2
      IF ((TIDAL .NE. -1.0) .AND. (RR .NE. -1.0) .AND.(PECO2 .NE. -1.0))
     A  A PCO2 = HOLD - 4.4 + (2.3 * TIDAL) - (.03 * RR) + (.09 * PECO2)
      GO TO (210,220,230,240,250), NKNOWN
  210 PIO2 = PAO2 + A PCO2 * (1.0 - FRAC * (1.0 - RQ)) / RQ
      IFLAG (3) = 3
      GO TO 9900
C
  220 A PCO2 = (PIO2 - PAO2) * RQ / (1.0 - FRAC * (1.0 - RQ))
      IFLAG (4) = 3
      GO TO 9900
C
  230 RQ = A PCO2 * (1.0 - FRAC) / (PIO2 - PAO2 - A PCO2 * FRAC)
      IFLAG (5) = 3
      GO TO 9900
C
  240 FIO2 = 100.0 * ((PAO2 - PIO2) * RQ / A PCO2 + 1.0) / (1.0 - RQ)
      IFLAG (6) = 3
      GO TO 9900
C
  250 PAO2 = PIO2 - A PCO2 * (1.0 - FRAC * (1.0 - RQ)) / RQ
      IFLAG (7) = 3
      IF (PAO2 .LT. 0.0)  IFLAG (7) = -3
      GO TO 9900
C
C***************************************
C     SUBROUTINE CHART 3
C     CHART 3 FROM HOUSTON, 6/47
C     ARTERIAL PO2 = ALVEOLOAR PO2 - A-A GRADIENT
C
  300 GO TO (310,320,330), NKNOWN
  310 PAO2 = A PO2 + AA GRAD
      IFLAG (7) = 3
      GO TO 9900
C
  320 AA GRAD = PAO2 - A PO2
      IFLAG (8) = 3
      GO TO 9900
C
  330 A PO2 = PAO2 - AA GRAD
      IFLAG (9) = 3
      IF (A PO2 .LT. 0.0)  IFLAG (9) = -3
      GO TO 9900
C
C***************************************
C     SUBROUTINE CHARTS 47
C     COMBINED CHARTS 4 & 7 FROM HOUSTON, 6/47.  HANDLE CALLS FOR SO2
C        & BLOOD O2 CONCENTRATIONS FOR ARTERIAL & VENOUS BLOOD
C
  400 IBLOOD = (ISUB + 1) / 4
      K2 = II (NKNOWN,IBLOOD)
      IF (IBLOOD .EQ. 2)  GO TO 420
      CALL O2 DISS (A SO2,A PH,A PO2,TEMP,A PCO2,NKNOWN)
      IFLAG (K2) = 3
      IF (PARAM (K2) .GT. 0.0)  GO TO 410
      IFLAG (K2) = -3
      GO TO 9900
C
  410 IF (O2 CAP .EQ. -1)  GO TO 9900
      CALL BL O2 (A PO2,TEMP,O2 CAP,A SO2,A O2 DIS,A O2 CMB,A O2 TOT,
     A   0.0,0.0,1)
      GO TO 450
C
  420 CALL O2 DISS (V SO2,V PH,V PO2,TEMP,V PCO2,NKNOWN)
      IFLAG (K2) = 3
      IF (PARAM (K2) .GT. 0.0)  GO TO 430
      IFLAG (K2) = -3
      GO TO 9900
C
  430 IF (O2 CAP .EQ. -1)  GO TO 9900
      CALL BL O2 (V PO2,TEMP,O2 CAP,V SO2,V O2 DIS,V O2 CMB,V O2 TOT,
     A   0.0,0.0,1)
C
  450 DO 460  J =1,3
      JSUB = JJ (J,IBLOOD)
      IFLAG (JSUB) = 3
  460 CONTINUE
      GO TO 9900
C
C***************************************
C     SUBROUTINE CHART 5
C     COMBINED CHARTS 5 & 6 FROM HOUSTON, 6/47.
C     A-V DIFF = (VO2 * 10) / (Q * O2 CAP)
C        FACTOR OF 10 FOR CONVERSION TO PCT. SATN
C
  500 GO TO (510,520,530,540), NKNOWN
  510 AV DIFF = (VO2 * 10.0) / (Q * O2 CAP)
      IFLAG (12) = 3
      GO TO 9900
C
  520 O2 CAP = (VO2 * 10.0) / (AV DIFF * Q)
      IFLAG (13) = 3
      GO TO 9900
C
  530 VO2 = AV DIFF * Q * O2 CAP / 10.0
      IFLAG (15) = 3
      GO TO 9900
C
  540 Q = (VO2 * 10.0) / (AV DIFF * O2 CAP)
      IFLAG (16) = 3
      GO TO 9900
C
C***************************************
C     SUBROUTINE CHART 8
C     CHART 8 FROM HOUSTON, 6/47
C     MEAN CAP. PO2 = VEN POS + (ART PO2-VEN PO2) / 3  (A LA BARCROFT)
C
  800 GO TO (810,820,830), NKNOWN
  810 A PO2 = 3.0 * CAP PO2 - 2.0 * VEN PO2
      IFLAG (9) = 3
      GO TO 9900
C
  820 V PO2 = (3.0 * CAP PO2 - A PO2) / 2.0
      IFLAG (19) = 3
      GO TO 9900
C
  830 CAP PO2 = (2.0 * V PO2 + A PO2) / 3.0
      IFLAG (20) = 3
      IF (CAP PO2 .LT. 0.0)  IFLAG (20) = -3
      GO TO 9900
C
C***************************************
C     SUBROUTINE CHART 6B      CHART 14
C     SUPPLEMENT CHART 6 FROM HOUSTON, 6/47
C     VEN SO2 = ART SO2 - A-V DIFF
C
 1400 GO TO (1410,1420,1430), NKNOWN
 1410 A SO2 = V SO2 + AV DIFF
      IFLAG (11) = 3
      GO TO 9900
C
 1420 AV DIFF = A SO2 - V SO2
      IFLAG (12) = 3
      GO TO 9900
C
 1430 V SO2 = A SO2 - AV DIFF
      IFLAG (17) = 3
      IF (V SO2 .LT. 0.0)  IFLAG (17) = -3
C
C If ISUB = 2 then current A PCO2 is PACO2, reset to PaCO2
 9900 IF (ISUB .EQ. 2) A PCO2 = HOLD
      RETURN
      END
