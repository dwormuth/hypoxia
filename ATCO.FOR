      REAL FUNCTION ATCO (X)
      REAL*8 A,B,C
      DATA A,B,C/1.9927D-4,1.834D-6,6.8274D-9/
      ATCO = .1515 + .01361 * X - A * X * X + B * X * X * X - C *X*X*X*X
      RETURN
      END
