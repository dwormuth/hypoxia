$STORAGE:2
      SUBROUTINE WRITE 2 (IPAGE)
      IMPLICIT INTEGER (O)
      LOGICAL YES
      INTEGER CRTI,CRTO,FILE,FORM,TEMP,FPLOT, ORDER(75)
      CHARACTER*4 ISEX(2),NAME,ITITL
      REAL LIST(24)
      CHARACTER*8  LABEL
      COMMON ICOM1(170), CRTI,CRTO,FILE,FORM,TEMP,FPLOT,
     A   NAME(7),ISTUDY,IDATE(3),IFLAG(75),NVAR,NTABLE, PARAM(75),
     B   NVARY(15),VALUE(15,2),STEP(15),ITITL(18),NUMVAR,MAXVAR,
     C   NOUT(15),NUMOUT
      COMMON /PHRED/ IPRN
      COMMON /PHRED1/ LABEL(75)
C
      DATA ISEX /'   M',' Fem'/
      DATA ORDER /40,42,43,44,1,2,21,22,6,3,7,8,10,
     A   18,9,19,4,60,27,34,37,35,38,36,39,58,59,47,11,17,
     B   29,28,50,57,    5,52,56,53,15,51,55,45,24,25,26,
     C   12,16,46,48,49,31,32,54,13,33,30,14,23,20,66,    15*0/
      DATA NCUT,NALL /34,60/
      DATA OUT1,OUT2,OUT3,OUT4 /21,13, 10,16/
C
  177 FORMAT (' '//16X,'Hypoxia Analysis Study',25X,'Page',I3 //
     A   3X,7A4,3X,'Study No.',I8,3X,'Date',I3,2('/',I2) // 3X,18A4 //
     B   A5,'ale   Age -',F4.0,'   Ht -',F5.0,' Cm.',5X,'Wt -',F6.1,
     C   ' Kg.    BSA -',F6.2,' Sq. M.'//
     D   3X,'Altitude',F9.1,' Meters   Pressure',F6.1,' mm Hg' /
     E   3X,'Temperature',F6.1,' C.',7X,'Hours at altitude -',F6.0 ///
     F   3X,'Inspired FO2',F16.2,' Pct.' //
     G   3X,'Inspired PO2',F16.1,' mm Hg' //
     H   3X,'Alveolar PO2',F16.1,' mm Hg' //
     I   3X,'A-a Gradient',F16.1,' mm Hg' //
     J   14X,'Arterial   Venous'//
     K   3X,'pH',F18.2,F9.2 /
     L   3X,'PO2',F16.1,F9.1,' mm Hg' /
     M   3X,'PCO2',F15.1,F9.1,' mm Hg' /
     N   3X,'PCO',F16.1,9X,' mm Hg' //
     O   3X,'Dissolved O2',F7.2,F9.2,' ml/100 ml blood')
  187 FORMAT (3X,'O2 with HGB',F8.2,F9.2,' ml/100 ml blood'/
     Q   3X,'Total O2',F11.2,F9.2,' ml/100 ml blood' //
     R   3X,'Total CO2 ',2F9.2,' ml/100 ml blood' //
     S   3X,'Total CO',F11.2,9X,' ml/100 ml blood' //
     T   3X,'HbO2 sat''n',2F9.1,' Pct.' /
     U   3X,'HbCO2 sat''n',F8.1,9X,' Pct.' /
     V   3X,'HbCO sat''n',F9.1,9X,' Pct.' //
     W   3X,'Mixed Venous PO2',F12.1,' mm Hg' //
     X   3X,'Physiologic shunt',F11.1,' Pct.'/)
  197 FORMAT (1H1,15X,'Hypoxia Analysis Study',25X,'Page',I3 //
     A   3X,7A4,3X,'Study No.',I8,3X,'Date',I3,2('/',I2) // 3X,18A4 //
     B   3X,'Respiratory Quotient',F10.3 /
     C   3X,'Coeff. of O2 Delivery',F9.3 /
     D   3X,'Vent.-Perf. Ratio',F13.3,' ml/100 ml blood' //
     E   3X,'CO2 Production',F17.1,' ml/min STPD' //
     F   3X,'O2 Consumption',F14.1,' ml/min STPD' /
     G   3X,'Oxygen Delivery',F13.1,' ml/min' //
     H   3X,'Alveolar ventilation',F8.1,' ml/min' //
     I   3X,'Max. breathing cap.',F9.1,' l/min' /
     J   3X,'Tidal volume',F16.1,' ml BTPS' /
     K   3X,'Respiratory rate',F11.0,'  /min')
  207 FORMAT (3X,'Minute ventilation',F10.1,' ml/min BTPS' //
     M   3X,'A-V O2 Diff',F17.1,' Pct. sat''n' //
     N   3X,'Cardiac output',F14.1,' l/min     Cardiac index',F6.1,
     O      ' l/min/sq m' /
     P   3X,'Cerebral blood flow',F9.1,' ml/min' /
     Q   3X,'Tissue blood flow',F11.1,' l/min' /
     R   3X,'Stroke volume',F15.1,' ml' /
     S   3X,'Heart rate',F17.0,' /min',9X,'Maximum HR',F7.0,'/min' //
     T   3X,'O2 Capacity',F17.1,' Vols Pct.' /
     U   3X,'Hemoglobin',F18.1,' gm/100 ml blood' /
     V   3X,'Hematocrit',F18.1,' ml/100 ml blood' /
     W   3X,'Viscosity',F21.3 //
     X   3X,'HCO3 Concentration',F10.1,' Vols Pct.' //
     Y   3X,'Mean Capillary PO2',F10.1,' mm Hg' //
     Z   3X,'Capillary Radius',F15.4,' cm.' //
     A   3X,'Tissue Diffusion',4X,4F10.3,' cm.' /
     B   3X,'Tissue PO2',9X,4F10.2,'  mm Hg' /)
  217 FORMAT (3X,'Mixed Expired CO2',F12.2,' mm Hg BTPS',
     A      /,3X,'Fraction expired CO2',3X,F7.3,' %',
     B      /,3X,'VD/VT Ratio     ',F7.2,' (Physiologic dead space)',
     C      /,3X,'Valve Box Volume',F7.2,' ml',/,'1')

  227 FORMAT (' Would you like only an abbreviated form? (Y/N) '\)
      WRITE (CRTO,227)
      CALL QQQANS (YES)
      IF (YES) CALL GILBERT (*9900,IPAGE)
C
C     ROUTINE TO WRITE 2 MAIN PAGES OF OUTPUT
C
C     PAGE 1 -
C
      IS = IFIX (PARAM (41))
      IPAGE = IPAGE + 1
      DO 20  J = 1,OUT1
      JSUB = ORDER (J)
      LIST (J) = PARAM (JSUB)
   20 CONTINUE
      WRITE (FORM,177)  IPAGE,NAME,ISTUDY,IDATE,ITITL,ISEX(IS),
     A   (LIST(J), J=1,OUT1)
      WRITE (IPRN,177)  IPAGE,NAME,ISTUDY,IDATE,ITITL,ISEX(IS),
     A   (LIST(J), J=1,OUT1)
C
      J1 = OUT1
      DO 30  J = 1,OUT2
      J1 = J1 + 1
      JSUB = ORDER (J1)
      LIST (J) = PARAM (JSUB)
   30 CONTINUE
      WRITE (FORM,187)  (LIST(J), J=1,OUT2)
      WRITE (IPRN,187)  (LIST(J), J=1,OUT2)
      CALL HLIST (5,NCUT,ORDER,LABEL)
C
C     PAGE 2 -
C
      IPAGE = IPAGE + 1
      DO 50  J = 1,OUT3
      J1 = J1 + 1
      JSUB = ORDER (J1)
      LIST (J) = PARAM (JSUB)
   50 CONTINUE
      WRITE (FORM,197)  IPAGE,NAME,ISTUDY,IDATE,ITITL,
     A   (LIST(J), J=1,OUT3)
      WRITE (IPRN,197)  IPAGE,NAME,ISTUDY,IDATE,ITITL,
     A   (LIST(J), J=1,OUT3)
C
      DO 60  J = 1,OUT4
      J1 = J1 + 1
      JSUB = ORDER (J1)
      LIST (J) = PARAM (JSUB)
   60 CONTINUE
C
      J = OUT4 + 1
      LIST (J) = PARAM (61)
      DO 70  K = 1,3
      J = J + 1
      LIST (J) = PARAM (61) + FLOAT (K) * 0.001
   70 CONTINUE
C
      DO 80  K = 1,4
      J = J + 1
      LIST (J) = PARAM (K+61)
   80 CONTINUE
      OUT48 = OUT4 + 8
C
      WRITE (FORM,207)  (LIST(J), J=1,OUT48)
      WRITE (IPRN,207)  (LIST(J), J=1,OUT48)
      WRITE (FORM,217)  PARAM(68),PARAM(69),PARAM(67),PARAM(72)
      WRITE (IPRN,217)  PARAM(68),PARAM(69),PARAM(67),PARAM(72)
      CALL HLIST (NCUT+1,NALL,ORDER,LABEL)
C
 9900 RETURN
      END
