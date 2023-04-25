$STORAGE:2
      SUBROUTINE GILBERT (*,IPAGE)
C
      IMPLICIT INTEGER (O)
      INTEGER CRTI,CRTO,FILE,FORM,TEMP1,FPLOT, ORDER(75)
      INTEGER ORDER1(15),ORDER2(18),ORDER3(15),ORD1,ORD2,ORD3
      CHARACTER*2 STAR,BLANK,STAT1(15),STAT2(18),STAT3(12)
      CHARACTER*4 ISEX(2),NAME,ITITL
      REAL LIST(24)
      CHARACTER*8  LABEL
      COMMON ICOM1(170), CRTI,CRTO,FILE,FORM,TEMP1,FPLOT,
     A   NAME(7),ISTUDY,IDATE(3),IFLAG(75),NVAR,NTABLE, PARAM(75),
     B   NVARY(15),VALUE(15,2),STEP(15),ITITL(18),NUMVAR,MAXVAR,
     C   NOUT(15),NUMOUT
      COMMON /PHRED/ IPRN
      COMMON /PHRED1/ LABEL(75)
      EQUIVALENCE (ALT,PARAM(1)),(PB,PARAM(2)), (PIO2,PARAM(3)),
     A (A PCO2,PARAM(4)), (RQ,PARAM(5)), (FIO2,PARAM(6)),
     B (PAO2,PARAM(7)), (AAGRAD,PARAM(8)), (A PO2,PARAM(9)),
     C (A PH,PARAM(10)), (A SO2,PARAM(11)), (AVDIFF,PARAM(12)),
     D (O2 CAP,PARAM(13)), (VO2,PARAM(15)), (Q,PARAM(16)),
     E (V SO2,PARAM(17)), (V PH,PARAM(18)), (V PO2,PARAM(19)),
     F (CAP PO2,PARAM(20)),(TEMP,PARAM(21)), (TIDAL,PARAM(24)),
     G (RR,PARAM(25)), (RMV,PARAM(26)), (SV,PARAM(31)), (HR,PARAM(32)),
     H (HGB,PARAM(33)), (A O2 DIS,PARAM(34)), (A O2 CMB,PARAM(35)),
     I (A O2 TOT,PARAM(36)), (V O2DIS,PARAM(37)), (V O2CMB,PARAM(38)),
     J (V O2 TOT,PARAM(39)), (AGE,PARAM(40)), (SEX,PARAM(41)),
     K (HT,PARAM(42)), (WT,PARAM(43)), (BSA,PARAM(44)), (CI,PARAM(46)),
     L (XV PO2,PARAM(50)), (O2 DEL,PARAM(51)), (VCO2,PARAM(53)),
     M (V HR,PARAM(54)), (AL VENT,PARAM(55)), (VPR,PARAM(56)),
     N (SHUNT,PARAM(57)), (A CO2 T,PARAM(58)), (XV CO2 T,PARAM(59)),
     O (V PCO2,PARAM(60)), (VDVT,PARAM(67)), (PECO2,PARAM(68)),
     P (FECO2,PARAM(69)), (FVPCO2,PARAM(70)), (GVPCO2,PARAM(71)),
     Q (VOL BOX,PARAM(72))
C
C    FVPCO2 is the Farhi determined PCO2 for the Kelman content
C    GVPCO2 is the Farhi determined PCO2 for a 95% SO2 arterially
C    FACO2T is the Farhi content of O2 for the Kelman VPO2
C    VOL BOX is the amount of dead space in the Valve Box
C
      DATA ISEX /'   M',' Fem'/
      DATA STAR,BLANK /' *','  '/
      DATA ORDER1 /1,2,21,6,3,7,8,10,18,9,19,4,60,34,37/
      DATA ORDER2 /35,38,36,39,58,59,11,17,50,57,5,56,53,15,51,55,24,25/
C  Note: PARAM (75) is used to hold FA CO2 T
      DATA ORDER3 /26,12,16,46,31,54,13,33,68,69,67, 70,71,75,72/

      ORD1 = 15
      ORD2 = 18
      ORD3 = 16
      IPAGE = IPAGE + 1
C
  177 FORMAT (' '//16X,'Hypoxia Analysis Study',25X,'Page',I3 //
     A   3X,7A4,3X,'Study No.',I8,3X,'Date',I3,2('/',I2) // 3X,18A4 //
     B   A5,'ale   Age -',F4.0,'   Ht -',F5.0,' Cm.',5X,'Wt -',F6.1,
     C   ' Kg.    BSA -',F6.2,' Sq. M.'//)
  187 FORMAT (3X,'Altitude',F9.1,A,' Meters   Pressure',F6.1,A,' mm Hg'/
     E   3X,'Temperature',F6.1,A,' C.'//
     F   3X,'Inspired FO2',F16.2,A,' Pct.' /
     G   3X,'Inspired PO2',F16.1,A,' mm Hg' /
     H   3X,'Alveolar PO2',F16.1,A,' mm Hg' /
     I   3X,'A-a Gradient',F16.1,A,' mm Hg' //
     J   14X,'Arterial     Venous'/
     K   3X,'pH',F18.2,A,F9.2,A /
     L   3X,'PO2',F16.1,A,F9.1,A,' mm Hg' /
     M   3X,'PCO2',F15.1,A,F9.1,A,' mm Hg' //
     O   3X,'Dissolved O2 ',F7.2,A,F9.2,A,' ml/100 ml blood')
  197 FORMAT (3X,'O2 with HGB ',F8.2,A,F9.2,A,' ml/100 ml blood'/
     Q   3X,'Total O2 ',F11.2,A,F9.2,A,' ml/100 ml blood' /
     R   3X,'Total CO2  ',F9.2,A,F9.2,A,' ml/100 ml blood' //
     T   3X,'HbO2 sat''n ',F9.1,A,F9.1,A' Pct.' /
     W   3X,'Mixed Venous PO2',F12.1,A,' mm Hg' /
     X   3X,'Physiologic shunt',F11.1,A,' Pct.'/
     B   3X,'Respiratory Quotient',F10.3,A /
     D   3X,'Vent.-Perf. Ratio',F13.3,A,' ml/100 ml blood' //
     E   3X,'CO2 Production',F14.1,A,' ml/min STPD' /
     F   3X,'O2 Consumption',F14.1,A,' ml/min STPD' /
     G   3X,'Oxygen Delivery',F13.1,A,' ml/min' /
     H   3X,'Alveolar ventilation',F8.1,A,' ml/min' /
     J   3X,'Tidal volume',F16.1,A,' ml BTPS' /
     K   3X,'Respiratory rate',F11.0,A,'  /min')
  207 FORMAT (3X,'Minute ventilation',F10.1,A,' ml/min BTPS' //
     M   3X,'A-V O2 Diff',F17.1,A,' Pct. sat''n' /
     N   3X,'Cardiac output',F14.1,A,' l/min     Cardiac index',F6.1,A,
     O      ' l/min/sq m' /
     R   3X,'Stroke volume',F15.1,A,' ml' /
     S   3X,'Heart rate',F17.0,A,' /min',9X,'Maximum HR',F7.0,A,'/min'//
     T   3X,'O2 Capacity',F17.1,A,' Vols Pct.' /
     U   3X,'Hemoglobin',F18.1,A,' gm/100 ml blood' /
     Z   3X,'Mixed expired CO2',F12.2,A,' mm Hg BTPS'/
     A   3X,'Fraction expired CO2',3X,F7.3,A,' %'/
     B   3X,'VD/VT Ratio ',7X,F12.4,A,' (Physiologic dead space)'/
     C   3X,'Farhi PvCO2                 ',F7.1,A,' mm Hg'/
     D   3X,'Kelman PvCO2 with SO2 of 95%',F7.1,A,' mm Hg'/
     E   3X,'Farhi CaCO2 at Kelman PaCO2 ',F7.1,A,' vols pct'/
     F   3X,'Valve Box Volume            ',F7.1,A,' ml'/'1')
C  PARAM (75) holds FA CO2 T
      PARAM (75) = ATCO (A PCO2) * 100.0
      IFLAG (75) = 3
      DO 100 I = 1,ORD1
      STAT1(I) = STAR
      IHOLD = IFLAG(ORDER1(I))
  100 IF ((ABS(IHOLD) .EQ. 3) .OR. (IHOLD .EQ. -1)) STAT1(I) = BLANK
      DO 200 I = 1,ORD2
      STAT2(I) = STAR
      IHOLD = IFLAG(ORDER2(I))
  200 IF ((ABS(IHOLD) .EQ. 3) .OR. (IHOLD .EQ. -1)) STAT2(I) = BLANK
      DO 300 I = 1,ORD3
      STAT3(I) = STAR
      IHOLD = IFLAG(ORDER3(I))
  300 IF ((ABS(IHOLD) .EQ. 3) .OR. (IHOLD .EQ. -1)) STAT3(I) = BLANK
      CALL STAMP (IPRN)
      WRITE (IPRN,177)  PAGE,NAME,ISTUDY,IDATE,ITITL,ISEX(INT(SEX)),AGE,
     A   HT,WT,BSA
      WRITE (IPRN,187) (PARAM(ORDER1(J)),STAT1(J),J=1,ORD1)
      WRITE (IPRN,197) (PARAM(ORDER2(J)),STAT2(J),J=1,ORD2)
      WRITE (IPRN,207) (PARAM(ORDER3(J)),STAT3(J),J=1,ORD3)
      RETURN 1
      END
