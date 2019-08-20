      SUBROUTINE G8XA(NPTS, A, X, W, DA, DB, DC, DN)
      INTEGER NPTS
      REAL A, X(NPTS), W(NPTS), DA(1), DB(1), DC(1)
      REAL DN(1)
      INTEGER JJ, J
      REAL XN, P, X2
C J. L. BLUE, 15 DEC 77
      P = A+1.E0
      DA(1) = 1.E0
      DB(1) = P/(P+1.E0)
      DC(1) = 0.E0
      DN(1) = 1.E0/P
      JJ = 2*NPTS
      DO  1 J = 2, JJ
         XN = J-1
         X2 = 2*(J-1)
         DA(J) = 1.E0
         DB(J) = (X2*(XN+P)+P*(P-1.E0))/((X2+P+1.E0)*(X2+P-1.E0))
         DC(J) = (XN*(XN+P-1.E0))**2/((X2+P-2.E0)*(X2+P-1.E0)**2*(X2+P))
         DN(J) = 0.E0
   1     CONTINUE
      CALL GAUSQ(NPTS, DA, DB, DC, DN, X, W)
      RETURN
      END
