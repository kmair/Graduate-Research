      SUBROUTINE D6LSBT(K, X, NX, NU, NRHS, NRHSG, GETJAC, A1, A2,
     1   F1, AA, BB, GAM, BC, G, B, GDIM, DG, DGI, DF, SBASIS)
      INTEGER K, GDIM, NRHS, NU, NX
      INTEGER NRHSG, BC(NU, 2, 2)
      LOGICAL GETJAC
      DOUBLE PRECISION X(NX), A1(NU, NU, 2), A2(NU, NU, 2), F1(NU, NRHS,
     1   2), AA(NU, NU, 2), BB(NU, NU, 2)
      DOUBLE PRECISION GAM(NU, NRHS, 2), G(GDIM, 1), B(GDIM, NRHS), DG(
     1   NU), DGI(NU), DF(NRHS)
      DOUBLE PRECISION SBASIS(K, 2)
      INTEGER JLR, I, J, L, GIDX, ISINLR
      INTEGER ID(2)
      DOUBLE PRECISION BBP, BIP, SIGNLR
      INTEGER TEMP
      LOGICAL TEMP1, TEMP2
      DATA ID(1)/0/
      DATA ID(2)/1/
C DO L AND R TERMS.
      DO  18 JLR = 1, 2
         ISINLR = (-1)**(JLR+1)
         SIGNLR = ISINLR
C   GET THE BASIS SPLINES AT L OR R.
         TEMP = (JLR-1)*(NX-1)
         CALL DBSPL1(K, X, NX, X(TEMP+1), 1, K+(JLR-1)*(NX-2*K), ID, 2
     1      , SBASIS)
         TEMP = (JLR-1)*(K-1)
         BBP = SBASIS(TEMP+1, 2)
         TEMP = (JLR-1)*(K-3)
         BIP = SBASIS(TEMP+2, 2)
         DO  17 I = 1, NU
            GIDX = I+(JLR-1)*(NX-K-1)*NU
            CALL SETD(NU, 0D0, DG)
            CALL SETD(NU, 0D0, DGI)
            IF (NRHSG .GT. 0) CALL SETD(NRHSG, 0D0, DF)
            DO  11 J = 1, NU
C NOT MIXED.
               TEMP1 = BC(J, 1, JLR) .EQ. 0
               IF (TEMP1) GOTO 1
                  TEMP2 = BC(J, 1, JLR) .EQ. (-2)
                  IF (TEMP2) TEMP2 = BC(J, 2, JLR) .EQ. (-2)
                  TEMP1 = TEMP2
   1           IF (.NOT. TEMP1) GOTO 3
                  IF (.NOT. GETJAC) GOTO 2
                     DG(J) = DG(J)+SIGNLR*(A1(I, J, JLR)*BBP+A2(I, J,
     1                  JLR))
                     DGI(J) = DGI(J)+SIGNLR*A1(I, J, JLR)*BIP
   2              CONTINUE
   3           IF (BC(J, 2, JLR) .NE. 1) GOTO 9
                  IF (.NOT. GETJAC) GOTO 5
                     DO  4 L = 1, NU
C MIXED.
                        TEMP1 = BC(L, 2, JLR) .EQ. 1
                        IF (TEMP1) TEMP1 = BB(J, L, JLR) .NE. 0D0
C/6S
C                       IF (TEMP1) CALL SETERR(
C    1       56HDGLSBT - BC(L,2,JLR)=1=BC(J,2,JLR) WHEN BB(J,L,JLR).NE.0
C    2                     , 56, 8, 2)
C/7S
                        IF (TEMP1) CALL SETERR(
     1       'DGLSBT - BC(L,2,JLR)=1=BC(J,2,JLR) WHEN BB(J,L,JLR).NE.0'
     2                     , 56, 8, 2)
C/
                        DG(L) = DG(L)+SIGNLR*A1(I, J, JLR)*(AA(J, L,
     1                     JLR)+BB(J, L, JLR)*BBP)
                        DGI(L) = DGI(L)+SIGNLR*A1(I, J, JLR)*BB(J, L,
     1                     JLR)*BIP
   4                    CONTINUE
                     DG(J) = DG(J)+SIGNLR*A2(I, J, JLR)
   5              L = 1
                     GOTO  7
   6                 L = L+1
   7                 IF (L .GT. NRHSG) GOTO  8
                     DF(L) = DF(L)-SIGNLR*A1(I, J, JLR)*GAM(J, L, JLR)
                     GOTO  6
   8              CONTINUE
C BAD BC.
   9           TEMP1 = BC(J, 1, JLR) .NE. (-2)
               IF (TEMP1) TEMP1 = BC(J, 1, JLR) .NE. 0
               IF (TEMP1) GOTO 10
                  TEMP2 = BC(J, 2, JLR) .NE. (-2)
                  IF (TEMP2) TEMP2 = BC(J, 2, JLR) .NE. 1
                  TEMP1 = TEMP2
C/6S
C 10           IF (TEMP1) CALL SETERR(
C    1            38HDGLSBT - BC(J,.,JLR) NOT ONE OF -2,0,1, 38, 9, 2)
C/7S
  10           IF (TEMP1) CALL SETERR(
     1            'DGLSBT - BC(J,.,JLR) NOT ONE OF -2,0,1', 38, 9, 2)
C/
  11           CONTINUE
C END J.
            IF (.NOT. GETJAC) GOTO 13
               DO  12 J = 1, NU
                  TEMP = J-I+K*NU
                  G(GIDX, TEMP) = G(GIDX, TEMP)+DG(J)
                  TEMP = J-I+(K+ISINLR)*NU
                  G(GIDX, TEMP) = G(GIDX, TEMP)+DGI(J)
  12              CONTINUE
  13        L = 1
               GOTO  15
  14           L = L+1
  15           IF (L .GT. NRHSG) GOTO  16
               B(GIDX, L) = B(GIDX, L)+DF(L)+SIGNLR*F1(I, L, JLR)
               GOTO  14
  16        CONTINUE
  17        CONTINUE
C END I.
  18     CONTINUE
C END JLR.
      RETURN
      END
