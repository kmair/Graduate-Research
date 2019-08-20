      SUBROUTINE G8EX2(NPTS, X, W, DA, DB, DC, DN)
      INTEGER NPTS
      REAL X(NPTS), W(NPTS), DA(1), DB(1), DC(1), DN(1)
      INTEGER JJ, J
C J. L. BLUE, 15 DEC 77
      JJ = 2*NPTS
      DO  1 J = 1, JJ
         DA(J) = 0.5E0
         DB(J) = 0.E0
         DC(J) = J-1
         DN(J) = 0.E0
   1     CONTINUE
C SQRT(PI)
      DN(1) = SQRT(2.E0*ATAN2(1.E0, 0.E0))
      CALL GAUSQ(NPTS, DA, DB, DC, DN, X, W)
      CALL ASYM(NPTS, X)
      CALL SYM(NPTS, W)
      RETURN
      END
