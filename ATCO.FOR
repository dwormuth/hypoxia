      REAL FUNCTION ATCO (X)
C   ./ i 1
C     This function accepts the PCO2 and returns the CO2 Content
      REAL*8 A,B,C
      DATA A,B,C/1.9927D-4,1.834D-6,6.8274D-9/
      ATCO = .1515 + .01361 * X - A * X * X + B * X * X * X - C *X*X*X*X
      RETURN
      END
