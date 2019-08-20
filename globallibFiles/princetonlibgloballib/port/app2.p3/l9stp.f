      SUBROUTINE  L9STP(NPTS, MESH, FN, QK, DELK, M, N, P, Q, A, BC, BX,
     1                  C, G, IW, LIW, LW, MM, NN, W, X)
      INTEGER NPTS, M, N, MM, NN, LIW, LW
      INTEGER IW(LIW)
      REAL MESH(NPTS), FN(NPTS), QK(NPTS), DELK, P(1), Q(1)
      REAL A(1), BC(2,MM), BX(2,NN), C(NN,MM), G(NN), W(LW),
     1                 X(NN)
C
      COMMON / D5FCM/ NPTSC, MC, NC, I1, I2, I3, I4
      INTEGER NPTSC, MC, NC, I1, I2, I3, I4
C/+
      REAL  ABS
C/
      EXTERNAL  R7MDC
      REAL  R7MDC
C
      INTEGER I, J, JP, K, M1, MAXMN, MAXMN1, N1, N2, NERROR, IER
      REAL CTX, CTXNEW, FDELK, FNI, QLRG, QZ, Z
      REAL BIG, ONE, ZERO
      DATA BIG/0.E+0/, ONE/1.E+0/, ZERO/0.E+0/
C
C *** BODY ***
C
C/6S
C     IF (M .LT. 0 .OR. N .LT. 0)
C    1   CALL SETERR(26H L9STP - INVALID DIMENSION, 26, 1, 2)
C/7S
      IF (M .LT. 0 .OR. N .LT. 0)
     1   CALL SETERR(' L9STP - INVALID DIMENSION', 26, 1, 2)
C/
      IF (BIG .LE. ZERO) BIG =  R7MDC(6)
      M1 = M + 1
      N1 = N + 1
      N2 = N + 2
      NPTSC = NPTS
      MC = M
      NC = N
      I1 = NPTS
      I2 = I1 + NPTS
      I3 = I2 + N1
      I4 = I3 + N1
      CALL MOVEFR(N1, Q, X)
      CALL MOVEFR(M1, P, X(N2))
      X(NN) = ZERO
      CALL SETR(4*NPTS, ZERO, BC)
      DO 10 I = 1, I2
 10      BC(2,I) = BIG
      DO 20 I = 1, N1
         BX(2,I) = ONE
         BX(1,I) = -ONE
 20      CONTINUE
      DO 30 I = N2, NN
         BX(2,I) = BIG
         BX(1,I) = -BIG
 30      CONTINUE
      CALL SETR(NN, ZERO, G)
      G(NN) = ONE
      CALL MOVEFR(NPTS, MESH, A)
      CALL MOVEFR(NPTS, FN, A(NPTS+1))
      CALL MOVEFR(NPTS, QK, A(2*NPTS+1))
C/6S
C     IF (DELK .LE. ZERO)
C    1   CALL SETERR(25H L5STP - NONPOSITIVE DELK, 25, 3, 2)
C/7S
      IF (DELK .LE. ZERO)
     1   CALL SETERR(' L5STP - NONPOSITIVE DELK', 25, 3, 2)
C/
      A(3*NPTS+1) = DELK
      MAXMN = MAX0(M, N)
      MAXMN1 = MAXMN + 1
      DO 50 I = 1, NPTS
         K = I + NPTS
         Z = MESH(I)
         FNI = FN(I)
         QZ = QK(I)
         FDELK = DELK + FNI
         CALL  T5COF(Z, A(1), A(NPTS), MAXMN, C(1,I))
         CALL V7CPY(MAXMN1, C(1,K), C(1,I))
         J = M1
 40         JP = J + N1
            C(JP,I) = -C(J,I)
            C(JP,K) =  C(J,K)
            J = J - 1
            IF (J .GE. 1) GO TO 40
         CALL  V7SCL(N1, C(1,I), DELK+FNI, C(1,I))
         CALL  V7SCL(N1, C(1,K), DELK-FNI, C(1,K))
         C(NN,I) = QZ
         C(NN,K) = QZ
 50      CONTINUE
      CTX = ZERO
C
C   SOLVE THE LP PROBLEM   MIN G(T)X SUBJECT TO C*X .GE. 0
C   AND -1 .LE. X(I) .LE. 1 FOR I = 1(1)M+1 (THE Q COEFFICIENTS).
C
      CALL L7PF(BC, BX, C, G, IW, LIW, LW, MM, NN, NN, W, X)
C/6S
C       IF (IW(1) .NE. 0)
C    1      CALL SETERR(27HFUNNY RETURN CODE FROM L7PF, 27, IW(1), 2)
C/7S
        IF (IW(1) .NE. 0)
     1      CALL SETERR('FUNNY RETURN CODE FROM L7PF', 27, IW(1), 2)
C/
      CTXNEW = -X(NN)
      IF (NERROR(IER) .NE. 0) CALL ERROFF
      IF (CTX .GE. CTXNEW) GO TO 150
         QLRG = ZERO
         J = 1
            GO TO 70
 60         J = J+1
 70         IF (J .GT. N+1) GO TO 80
            IF (QLRG .LT.  ABS(X(J))) QLRG =  ABS(X(J))
            GO TO 60
 80      J = 1
            GO TO 100
 90         J = J+1
 100        IF (J .GT. N+1) GO TO 110
            Q(J) = X(J)/QLRG
            GO TO 90
 110     I = 0
         J = N+2
            GO TO 130
 120        J = J+1
 130        IF (J .GT. M+N+2) GO TO 140
            I = I+1
            P(I) = X(J)/QLRG
            GO TO 120
 140     CONTINUE
         GO TO 999
C/6S
C150     CALL SETERR(44H L5STP - NO IMPROVEMENT IN THE LP SUBPROBLEM,
C    1               44, 4, 1)
C/7S
 150     CALL SETERR(' L5STP - NO IMPROVEMENT IN THE LP SUBPROBLEM',
     1               44, 4, 1)
C/
 999  RETURN
C *** LAST LINE OF  L9STP FOLLOWS ***
      END
