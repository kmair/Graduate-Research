      SUBROUTINE DG8EX2(NPTS, X, W, DA, DB, DC, DN)
      INTEGER NPTS
      DOUBLE PRECISION X(NPTS), W(NPTS), DA(1), DB(1), DC(1), DN(1)
      DOUBLE PRECISION DSQRT, DATAN2
      INTEGER JJ, J
C J. L. BLUE, 15 DEC 77
      JJ = 2*NPTS
      DO  1 J = 1, JJ
         DA(J) = 0.5D0
         DB(J) = 0.D0
         DC(J) = J-1
         DN(J) = 0.D0
   1     CONTINUE
C SQRT(PI)
      DN(1) = DSQRT(2.D0*DATAN2(1.D0, 0.D0))
      CALL DGAUSQ(NPTS, DA, DB, DC, DN, X, W)
      CALL DASYM(NPTS, X)
      CALL DSYM(NPTS, W)
      RETURN
      END
