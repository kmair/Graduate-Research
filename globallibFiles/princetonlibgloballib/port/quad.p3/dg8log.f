      SUBROUTINE DG8LOG(NPTS, X, W, DA, DB, DC, DN)
      INTEGER NPTS
      DOUBLE PRECISION X(NPTS), W(NPTS), DA(1), DB(1), DC(1), DN(1)
      INTEGER JJ, J
      DOUBLE PRECISION XJ, X2
C J. L. BLUE, 15 DEC 77
      DA(1) = 0.5D0
      DB(1) = 0.5D0
      DC(1) = 0.D0
      DN(1) = 1.D0
      JJ = 2*NPTS
      DO  1 J = 2, JJ
         XJ = J
         X2 = 4*J-2
         DA(J) = XJ/X2
         DB(J) = 0.5D0
         DC(J) = (XJ-1.D0)/X2
         DN(J) = DSIGN(1.D0/(XJ*(XJ-1.D0)), -DN(J-1))
   1     CONTINUE
      CALL DGAUSQ(NPTS, DA, DB, DC, DN, X, W)
      RETURN
      END
