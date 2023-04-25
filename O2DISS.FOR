C   ./ i 0
$STORAGE:2
      SUBROUTINE O2 DISS (SO2,PH,PO2,TEMP,PCO2,NKNOWN)
C
C     CALCULATE PCT SAT'N OF HBO2 OR OTHER PARAMETERS
C
      DATA  AA,BB /0.000584,0.003683/
      DATA A,B,C /0.0000306,0.00028,0.00925/
      DATA P,Q /9.150326797,302.2875816/
      DATA ALPHA,BETBAS,P3 /274.37793,-865.258301,3.050108932/
C
C     HBO2 DISSOCIATION CURVE ROUTINE (WEST & WAGNER, 1977).
C     MOD. FROM OLSZKOWKA & FAHRI, RESP. PHYSIOL., 4.270 (1968).
C
C     HBO2 = U / (1 + U)
C        WHERE U = A*PO2**3 + B*PO2**2 + C*PO2
C
C     IF PO2 .LE. 26, USE (KELMAN, RESP. PHYSIOL., 4.260 (1968)) -
C
C     HBO2 = AA*PO2**2 + BB*PO2
C
C     WHERE PO2 IS APPARENT PO2, CORRECTED AS SHOWN BELOW FOR TEMP,
C        PH AND PCO2. (KELMAN, J. APPL. PHYSIOL., 21.1375 (1966))
C
      IF (NKNOWN .GT. 1)  GO TO 100
      X = PO2 * 10.0 ** (0.024 * (37.0 - TEMP) +
     A   0.40 * (PH - 7.4) + 0.06 * ALOG10 (40.0 / PCO2))
C
      IF (X .GT. 26.0)  GO TO 50
      SO2 = 100.0 * (AA * X + B) * X
      GO TO 9900
C
   50 U = ((A * X + B) * X + C) * X
      SO2 = 100.0 * U / (1.0 + U)
      GO TO 9900
C
  100 IF (SO2 .GT. 49.0542)  GO TO 120
      BHOLD = BB * BB + 0.04 * AA * SO2
      IF (BHOLD .LT. 0) CALL REPORT(BHOLD,SO2,PH,PO2,TEMP,PCO2,NKNOWN,
     A *9900)
      PO2 APP = (-BB + SQRT (BHOLD)) / (2.0 * AA)
C Attempt to cure problem of negative square roots
C     PO2 APP = (-BB + SQRT (BB * BB + 0.04 * AA * SO2)) / (2.0 * AA)
      GO TO 150
C
  120 U = SO2 / (100.0 - SO2)
      R = U / A
      BETA = BETBAS + R
      BTEMP = BETA * BETA / 4.0 + ALPHA ** 3 / 27.0
      IF (BTEMP .LT. 0) CALL REPORT(BTEMP,S02,PH,PO2,TEMP,PCO2,NKNOWN,
     A *9900)
      Q2 = SQRT (BTEMP)
C Attempt to cure problem of negative square roots
C     Q2 = SQRT (BETA * BETA / 4.0 + ALPHA ** 3 / 27.0)
      B2 = - BETA / 2.0
      X = ROOT (B2+Q2,3) + ROOT (B2-Q2,3)
      PO2 APP = - (X + P3)
  150 NGO = NKNOWN - 1
      GO TO (200,300,400), NGO
C
C     MISSING PH
C
  200 PH = (ALOG10 (PO2 APP  / PO2) - 0.024 * (37.0 - TEMP) -
     A   0.06 * ALOG10 (40.0 / PCO2)) / 0.40 + 7.4
      GO TO 9900
C
C     MISSING PO2
C
  300 PO2 = PO2 APP / (10.0 ** (0.40 * (PH - 7.4) +
     A   0.024 * (37.0 - TEMP) + 0.06 * ALOG10 (40.0 / PCO2)))
      GO TO 9900
C
C     MISSING PCO2
C
  400 PCO2 = 40.0 / 10.0 ** ((ALOG10 (PO2 APP / PO2) -
     A   (0.024 * (37.0 - TEMP)) - (0.40 * (PH - 7.4))) / 0.06)
C
 9900 RETURN
