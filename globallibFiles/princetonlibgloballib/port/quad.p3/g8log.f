      SUBROUTINE G8LOG(NPTS, X, W, DA, DB, DC, DN)
      INTEGER NPTS
      REAL X(NPTS), W(NPTS), DA(1), DB(1), DC(1), DN(1)
      INTEGER JJ, J
      REAL XJ, X2
C J. L. BLUE, 15 DEC 77
      DA(1) = 0.5E0
      DB(1) = 0.5E0
      DC(1) = 0.E0
      DN(1) = 1.E0
      JJ = 2*NPTS
      DO  1 J = 2, JJ
         XJ = J
         X2 = 4*J-2
         DA(J) = XJ/X2
         DB(J) = 0.5E0
         DC(J) = (XJ-1.E0)/X2
         DN(J) = SIGN(1.E0/(XJ*(XJ-1.E0)), -DN(J-1))
   1     CONTINUE
      CALL GAUSQ(NPTS, DA, DB, DC, DN, X, W)
      RETURN
      END
