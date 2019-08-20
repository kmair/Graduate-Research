      SUBROUTINE DG8EXA(NPTS, A, X, W, DA, DB, DC, DN)
      INTEGER NPTS
      DOUBLE PRECISION A, X(NPTS), W(NPTS), DA(1), DB(1), DC(1)
      DOUBLE PRECISION DN(1)
      INTEGER JJ, J
      DOUBLE PRECISION DGAMMA, XN
C J. L. BLUE, 15 DEC 77
      JJ = 2*NPTS
      DO  1 J = 1, JJ
         XN = J-1
         DA(J) = -J
         DB(J) = 2.D0*XN+A+1.D0
         DC(J) = -(XN+A)
         DN(J) = 0.D0
   1     CONTINUE
      DN(1) = DGAMMA(A+1.D0)
      CALL DGAUSQ(NPTS, DA, DB, DC, DN, X, W)
      RETURN
      END
