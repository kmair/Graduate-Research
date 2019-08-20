      SUBROUTINE DL9STP(NPTS, MESH, FN, QK, DELK, M, N, P, Q, A, BC, BX,
     1                  C, G, IW, LIW, LW, MM, NN, W, X)
      INTEGER NPTS, M, N, MM, NN, LIW, LW
      INTEGER IW(LIW)
      DOUBLE PRECISION MESH(NPTS), FN(NPTS), QK(NPTS), DELK, P(1), Q(1)
      DOUBLE PRECISION A(1), BC(2,MM), BX(2,NN), C(NN,MM), G(NN), W(LW),
     1                 X(NN)
C
      COMMON /DD5FCM/ NPTSC, MC, NC, I1, I2, I3, I4
      INTEGER NPTSC, MC, NC, I1, I2, I3, I4
C/+
      DOUBLE PRECISION DABS
C/
      EXTERNAL DR7MDC
      DOUBLE PRECISION DR7MDC
C
      INTEGER I, J, JP, K, M1, MAXMN, MAXMN1, N1, N2, NERROR, IER
      DOUBLE PRECISION CTX, CTXNEW, FDELK, FNI, QLRG, QZ, Z
      DOUBLE PRECISION BIG, ONE, ZERO
      DATA BIG/0.D+0/, ONE/1.D+0/, ZERO/0.D+0/
C
C *** BODY ***
C
C/6S
C     IF (M .LT. 0 .OR. N .LT. 0)
C    1   CALL SETERR(26HDL9STP - INVALID DIMENSION, 26, 1, 2)
C/7S
      IF (M .LT. 0 .OR. N .LT. 0)
     1   CALL SETERR('DL9STP - INVALID DIMENSION', 26, 1, 2)
C/
      IF (BIG .LE. ZERO) BIG = DR7MDC(6)
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
      CALL MOVEFD(N1, Q, X)
      CALL MOVEFD(M1, P, X(N2))
      X(NN) = ZERO
      CALL SETD(4*NPTS, ZERO, BC)
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
      CALL SETD(NN, ZERO, G)
      G(NN) = ONE
      CALL MOVEFD(NPTS, MESH, A)
      CALL MOVEFD(NPTS, FN, A(NPTS+1))
      CALL MOVEFD(NPTS, QK, A(2*NPTS+1))
C/6S
C     IF (DELK .LE. ZERO)
C    1   CALL SETERR(25HDL5STP - NONPOSITIVE DELK, 25, 3, 2)
C/7S
      IF (DELK .LE. ZERO)
     1   CALL SETERR('DL5STP - NONPOSITIVE DELK', 25, 3, 2)
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
         CALL DT5COF(Z, A(1), A(NPTS), MAXMN, C(1,I))
         CALL DV7CPY(MAXMN1, C(1,K), C(1,I))
         J = M1
 40         JP = J + N1
            C(JP,I) = -C(J,I)
            C(JP,K) =  C(J,K)
            J = J - 1
            IF (J .GE. 1) GO TO 40
         CALL DV7SCL(N1, C(1,I), DELK+FNI, C(1,I))
         CALL DV7SCL(N1, C(1,K), DELK-FNI, C(1,K))
         C(NN,I) = QZ
         C(NN,K) = QZ
 50      CONTINUE
      CTX = ZERO
C
C   SOLVE THE LP PROBLEM   MIN G(T)X SUBJECT TO C*X .GE. 0
C   AND -1 .LE. X(I) .LE. 1 FOR I = 1(1)M+1 (THE Q COEFFICIENTS).
C
      CALL DL7PF(BC, BX, C, G, IW, LIW, LW, MM, NN, NN, W, X)
C/6S
C       IF (IW(1) .NE. 0)
C    1      CALL SETERR(28HFUNNY RETURN CODE FROM DL7PF, 28, IW(1), 2)
C/7S
        IF (IW(1) .NE. 0)
     1      CALL SETERR('FUNNY RETURN CODE FROM DL7PF', 28, IW(1), 2)
C/
      CTXNEW = -X(NN)
      IF (NERROR(IER) .NE. 0) CALL ERROFF
      IF (CTX .GE. CTXNEW) GO TO 150
         QLRG = ZERO
         J = 1
            GO TO 70
 60         J = J+1
 70         IF (J .GT. N+1) GO TO 80
            IF (QLRG .LT. DABS(X(J))) QLRG = DABS(X(J))
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
C150     CALL SETERR(44HDL5STP - NO IMPROVEMENT IN THE LP SUBPROBLEM,
C    1               44, 4, 1)
C/7S
 150     CALL SETERR('DL5STP - NO IMPROVEMENT IN THE LP SUBPROBLEM',
     1               44, 4, 1)
C/
 999  RETURN
C *** LAST LINE OF DL9STP FOLLOWS ***
      END
