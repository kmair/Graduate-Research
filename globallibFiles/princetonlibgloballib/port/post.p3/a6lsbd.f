      SUBROUTINE A6LSBD(K, NX, NU, NRHS, NRHSG, BC, E, CC, GAM,
     1   GETJAC, G, B, GDIM1, GDIM2)
      INTEGER NRHS, GDIM1, GDIM2, NU
      INTEGER K, NX, NRHSG, BC(NU, 2, 2), E(NU, 2, 2)
      REAL CC(NU, NU, 2), GAM(NU, NRHS, 2), G(GDIM1, GDIM2), B(GDIM1,
     1   NRHS)
      LOGICAL GETJAC
      INTEGER JLR, I, J, L, GIDX1
      INTEGER TEMP
      LOGICAL TEMP1
      DO  7 JLR = 1, 2
         DO  6 I = 1, NU
            TEMP1 = BC(I, 1, JLR) .NE. (-2)
            IF (TEMP1) TEMP1 = BC(I, 1, JLR) .NE. 0
C/6S
C           IF (TEMP1) CALL SETERR(
C    1         36H GLSBD - BC(I,1,JLR) NOT ONE OF -2,0, 36, 6, 2)
C/7S
            IF (TEMP1) CALL SETERR(
     1         ' GLSBD - BC(I,1,JLR) NOT ONE OF -2,0', 36, 6, 2)
C/
            TEMP1 = BC(I, 2, JLR) .NE. (-2)
            IF (TEMP1) TEMP1 = BC(I, 2, JLR) .NE. 1
C/6S
C           IF (TEMP1) CALL SETERR(
C    1         36H GLSBD - BC(I,2,JLR) NOT ONE OF -2,1, 36, 7, 2)
C/7S
            IF (TEMP1) CALL SETERR(
     1         ' GLSBD - BC(I,2,JLR) NOT ONE OF -2,1', 36, 7, 2)
C/
            IF (BC(I, 1, JLR) .NE. 0) GOTO  6
C ONLY DO DIRICHLET B.C.'S.
            TEMP1 = E(I, 1, JLR) .LT. 1
            IF (.NOT. TEMP1) TEMP1 = E(I, 1, JLR) .GT. NU
C/6S
C           IF (TEMP1) CALL SETERR(
C    1         39H GLSBD - E(I,1,JLR) NOT ONE OF 1,...,NU, 39, 8, 2)
C/7S
            IF (TEMP1) CALL SETERR(
     1         ' GLSBD - E(I,1,JLR) NOT ONE OF 1,...,NU', 39, 8, 2)
C/
            GIDX1 = E(I, 1, JLR)+(JLR-1)*(NX-K-1)*NU
            L = 1
               GOTO  2
   1           L = L+1
   2           IF (L .GT. NRHSG) GOTO  3
               B(GIDX1, L) = GAM(I, L, JLR)
               GOTO  1
   3        IF (.NOT. GETJAC) GOTO  6
            DO  4 J = 1, GDIM2
               G(GIDX1, J) = 0
   4           CONTINUE
            DO  5 J = 1, NU
               TEMP1 = CC(I, J, JLR) .NE. 0.
               IF (TEMP1) TEMP1 = BC(J, 1, JLR) .EQ. 0
C/6S
C              IF (TEMP1) CALL SETERR(
C    1        55H GLSBD - CC(I,J,JLR).NE.0 AND BC(I,1,JLR)=0=BC(J,1,JLR)
C    2            , 55, 9, 2)
C/7S
               IF (TEMP1) CALL SETERR(
     1        ' GLSBD - CC(I,J,JLR).NE.0 AND BC(I,1,JLR)=0=BC(J,1,JLR)'
     2            , 55, 9, 2)
C/
               TEMP = J-E(I, 1, JLR)+K*NU
               G(GIDX1, TEMP) = -CC(I, J, JLR)
   5           CONTINUE
            TEMP = I-E(I, 1, JLR)+K*NU
            G(GIDX1, TEMP) = 1
   6        CONTINUE
   7     CONTINUE
      RETURN
      END
