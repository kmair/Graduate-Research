      LOGICAL FUNCTION A9OSTD(K, X, NX, NXMK, U, UT, NU, V, VT,
     1   NV, T, GETJAC, SEPATE, D, D1, D1T, D2, D3, D3T)
      INTEGER NXMK, NV, NX
      EXTERNAL D
      INTEGER K, NU
      REAL X(NX), U(NXMK, 1), UT(NXMK, 1), V(NV), VT(NV), T
      REAL D1(NV, NXMK, 1), D1T(NV, NXMK, 1), D2(NV), D3(NV, NV), D3T(
     1   NV, NV)
      LOGICAL GETJAC, SEPATE
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      COMMON /A9OSTT/ TT, DT
      REAL TT, DT
      COMMON /A9OSTM/ THETA, EGIVE, IZAP
      INTEGER IZAP(3)
      REAL THETA, EGIVE
      COMMON /A9OSTF/ FNUM
      INTEGER FNUM
      COMMON /POSTF/ FAILED
      LOGICAL FAILED
      INTEGER ISTKGT, ID1T, ID3T, I, J, L
      INTEGER IS(1000), ID1, ID3
      REAL RS(1000), WS(500)
      LOGICAL BEULER, LS(1000)
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
C TO LINEARIZE THE D RESULTS.
C SCRATCH SPACE ALLOCATED - S(A9OSTD) = S(D) REAL WORDS.
C (U,UT)(NXMK,NU).
C           (D1,D1T)(NV,NXMK,NU).
      BEULER = THETA .EQ. 1.
      FAILED = .FALSE.
      CALL SETR(NV, 0E0, D2)
      IF (.NOT. GETJAC) GOTO 1
         IF (NU .GT. 0) CALL SETR(2*NV*NU*NXMK, 0E0, D1)
         CALL SETR(2*NV**2, 0E0, D3)
         CALL D(T, K, X, NX, U, UT, NU, NX-K, V, VT, NV, D2, D1, D1T,
     1      D3, D3T)
         GOTO  2
   1     ID1 = ISTKGT(2*NV*(NU*NXMK+NV), 3)
         ID1T = ID1+NV*NU*NXMK
         ID3 = ID1T+NV*NU*NXMK
         ID3T = ID3+NV**2
C DEFAULT VALUES.
         CALL SETR(2*NV*(NU*NXMK+NV), 0E0, WS(ID1))
         CALL D(T, K, X, NX, U, UT, NU, NX-K, V, VT, NV, D2, WS(ID1),
     1      WS(ID1T), WS(ID3), WS(ID3T))
         CALL ISTKRL(1)
   2  IF (.NOT. FAILED) GOTO 3
         FNUM = 3
         A9OSTD = .TRUE.
         RETURN
   3  DO  9 I = 1, NV
         D2(I) = -D2(I)
         IF (.NOT. GETJAC) GOTO  9
         DO  6 J = 1, NV
            IF (.NOT. BEULER) D3(I, J) = D3(I, J)*THETA
            IF (SEPATE) GOTO 4
               D3(I, J) = -(D3(I, J)+D3T(I, J)/DT)
               GOTO  5
   4           D3(I, J) = -D3(I, J)
               D3T(I, J) = -D3T(I, J)
   5        CONTINUE
   6        CONTINUE
         IF (NU .EQ. 0) GOTO  9
         DO  8 J = 1, NU
            DO  7 L = 1, NXMK
               IF (.NOT. BEULER) D1(I, L, J) = D1(I, L, J)*THETA
               IF (.NOT. SEPATE) D1(I, L, J) = D1(I, L, J)+D1T(I, L, J)/
     1            DT
   7           CONTINUE
   8        CONTINUE
   9     CONTINUE
      A9OSTD = .FALSE.
      RETURN
      END
