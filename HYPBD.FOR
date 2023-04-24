$STORAGE:2
      SUBROUTINE HYP BD
      DIMENSION LIST2 (100), IPAR2(2,35), YLO2 (75), YHI2 (75)
      CHARACTER*4 NAME,ITITL
      CHARACTER*6 UNITS,UNITS2(75)
      CHARACTER*8 LABEL,LABEL2(75)
      INTEGER CRTI,CRTO,FILE,FORM,FTEMP,FPLOT
      COMMON  LIST(100),IPARM(2,35), CRTI,CRTO,FILE,FORM,FTEMP,FPLOT,
     A   NAME(7),ISTUDY,IDATE(3),IFLAG(75),NVAR,NTABLE,
     B   PARAM(75), NVARY(15),VALUE(15,2),STEP(15),ITITL(18),NUMVAR,
     C   MAXVAR,NOUT(15),NUMOUT
      COMMON /PHRED/ IPRN
      COMMON /PHRED1/ LABEL(75)
      COMMON /PHRANK/ UNITS(75)
      COMMON /LIMITS/ YLO(75),YHI(75)
C
      DATA LABEL2  /'Altitude','Pressure','Insp PO2','Art PCO2',
     A   'R.Q.    ','FIO2    ','Alv PO2 ','A-a Grad','Art PO2 ',
     B   'Art pH  ','Art SO2 ','A-V Diff','O2 Cap  ','Viscos. ',
     C   'O2 Cons ','Card Out','Ven SO2 ','Ven pH  ','Ven PO2 ',
     D   'Mcap PO2','Temp    ','Duration','HCO3    ','Tidl Vol',
     E   'Resp Rt.','Min Vent','Art PCO ','Art SCO ','Art SCO2',
     F   'Hct     ','Stroke V','Heart Rt','HGB     ','Art O2 D',
     G   'Art O2 C','Art O2 T','Ven O2 D','Ven O2 C','Ven O2 T',
     H   'Age     ','Sex     ','Height  ','Weight  ','B.S.A.  ',
     I   'Max Brth','Card Ind','Art CO T','Brain Q ','Tissue Q',
     J   'XVen PO2','O2 Del  ','C.O.D.  ','CO2 Prod','Max HR  ',
     K   'Alv Vent','Ven-Perf','Shunt   ','Art CO2 ','XVen CO2',
     L   'Ven PCO2','Tissue R','Tiss PO2','Tiss PO2','Tiss PO2',
     M   'Tiss PO2','Cap Rad ','VD/VT   ','Exp PCO2','FECO2   ',
     N   'FahriCO2','CO2 95% ','Valv Box',3*'********'/

      DATA IPAR2 /1,3, 4,5, 9,3, 12,4, 16,4, 20,1, 21,4, 25,3, 28,3,
     A   31,3, 34,3, 37,2, 39,2, 41,3, 44,3, 47,2, 49,2, 51,1, 52,3,
     B   55,9, 64,3, 67,3, 70,3, 73,2, 75,8, 83,2, 85,3, 88,3, 91,2,
     C   12*0/
      DATA  LIST2 /2,3,6, 3,4,5,6,7, 7,8,9, 11,10,9,4, 12,13,15,16,
     A   20, 17,18,19,60, 9,19,20, 4,10,23, 24,25,26,
     B   16,31,32, 13,33, 1,2, 11,12,17, 9,11,27,
     C   42,43, 16,44, 4, 4,9,16, 4,7,10,13,15,16,18,36,60,
     D   15,16,36, 5,15,53, 4,16,53, 30,33,
     E   4,10,11,16,17,18,30,53, 14,30, 4,68,67, 69,26,53, 68,69, 8*0/

      DATA UNITS2  /'meters', 3*'mm Hg ','      ',
     A   'Pct.  ', 3*'mm Hg ','      ',
     B    2*'Pct.  ','Vols %','      ','ml/min',
     C   'l/min ','Pct.  ','      ', 2*'mm Hg ',
     D   'Deg C ','Hours ','Vols %','ml    ','/min  ',
     E   'ml/min','mm Hg ', 2*'Pct.  ','Vols %',
     F   'ml    ','/min  ','grams ', 6*'Vols %',
     G   'years ','      ','cm.   ','kg    ','sq m  ',
     H   'l/min ','lminm2','Vols %','ml/min','l/min ',
     I   'mm Hg ','ml/min','      ','ml/min','/min  ',
     J   'ml/min','      ','Pct.  ', 2*'Vols %',
     K   'mm Hg ','cm    ', 4*'mm Hg ','cm    ',
     L   '      ','mm Hg ','Pct.  ',2*'mm Hg ',
     M   'ml    ',3*'      '/
C
      DATA YLO2 /3*0.0,20.0,.51,2*0.0,-30.0,0.0,6.9,3*0.0,1.0,3*0.0,
     A 6.9,20.0,0.0, 30.0,19*0.0,1.0,18*0.0,20.0,9*0.0,
     B 2*20.0,0.0,3*999.0/
      DATA YHI2 /9999.0,760.0,760.0,200.0,2.0,100.0,760.0,30.0,760.0,
     A 7.8, 3*100.0,5.0,9999.0,30.0,100.0,7.8,760.0,760.0,50.0,240.0,
     B 100.0,5000.0,200.0,99999.0,
     B 760.0,3*100.0,200.0,220.0,7*100.0,99.0,2.0,230.0,400.0,10.0,
     C 2*800.0,100.0,1000.0,100.0,760.0,1000.0,1000.0,9000.0,220.0,
     D 1000.0,1000.0,100.0,3*760.0,10.0,6*100.0,760.0,100.0,2*90.0,
     E 1000.0,3*0.0/

      NVAR = 72
      NTABLE = 29
      MAXVAR = 15
      CRTI = 0
      CRTO = 0
      FILE = 10
      FORM = 50
      FTEMP = 11
      FPLOT = 12
      IPRN  = 99
C
      DO 50  K2 = 1,100
   50 LIST (K2) = LIST2 (K2)
      DO 100  J2 = 1,2
      DO 100  K2 = 1,35
  100 IPARM (J2,K2) = IPAR2 (J2,K2)
      DO 200 I = 1,75
      YLO(I) = YLO2(I)
      YHI(I) = YHI2(I)
      UNITS(I) = UNITS2(I)
  200 LABEL(I) = LABEL2(I)
      RETURN
      END
