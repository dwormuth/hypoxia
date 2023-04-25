C   ./ i 0
$STORAGE:2
      FUNCTION ROOT (X,N)
C
C     TAKE THE N-TH ROOT OF X
C
      R = 1.0
      IF (X .GE. 0.0)  GO TO 20
      IF (N/2*2 .NE. N) GO TO 10
      R = 0.0
      GO TO 20
   10 R = -1.0
C
   20 ROOT = R * ABS (X) ** (1.0 / FLOAT(N))
      RETURN
      END
