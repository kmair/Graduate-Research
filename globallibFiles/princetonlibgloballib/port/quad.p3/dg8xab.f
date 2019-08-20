      SUBROUTINE DG8XAB(NPTS, A, B, X, W, DA, DB, DC, DN)
      INTEGER NPTS
      DOUBLE PRECISION A, B, X(NPTS), W(NPTS), DA(1), DB(1)
      DOUBLE PRECISION DC(1), DN(1)
      INTEGER JJ, J
      DOUBLE PRECISION DGAMMA, XN, TERM, C, X2
C J. L. BLUE, 15 DEC 77
      C = A+B+1.D0
      DA(1) = 2.D0/(C+1.D0)
      DB(1) = (B-A)/(C+1.D0)
      DC(1) = 0.
      DN(1) = 2.D0**C*DGAMMA(A+1.D0)*DGAMMA(B+1.D0)/DGAMMA(C+1.D0)
      TERM = (B+A)*(B-A)
      JJ = 2*NPTS
      DO  1 J = 2, JJ
         XN = J-1
         X2 = 2*(J-1)
         DA(J) = 2.*(XN+1.)*(XN+C)/((X2+C)*(X2+C+1.))
         DB(J) = TERM/((X2+C-1.)*(X2+1.*C))
         DC(J) = 2.*(XN+A)*(XN+B)/((X2+C-1.)*(X2+C))
         DN(J) = 0.
   1     CONTINUE
      CALL DGAUSQ(NPTS, DA, DB, DC, DN, X, W)
      IF (A .NE. B) RETURN
         CALL DASYM(NPTS, X)
         CALL DSYM(NPTS,W)
         RETURN
      END
