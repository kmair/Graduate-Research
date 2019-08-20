      SUBROUTINE G8EXA(NPTS, A, X, W, DA, DB, DC, DN)
      INTEGER NPTS
      REAL A, X(NPTS), W(NPTS), DA(1), DB(1), DC(1)
      REAL DN(1)
      INTEGER JJ, J
      REAL GAMMA, XN
C J. L. BLUE, 15 DEC 77
      JJ = 2*NPTS
      DO  1 J = 1, JJ
         XN = J-1
         DA(J) = -J
         DB(J) = 2.E0*XN+A+1.E0
         DC(J) = -(XN+A)
         DN(J) = 0.E0
   1     CONTINUE
      DN(1) = GAMMA(A+1.E0)
      CALL GAUSQ(NPTS, DA, DB, DC, DN, X, W)
      RETURN
      END
