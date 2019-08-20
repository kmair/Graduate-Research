      SUBROUTINE DL9PH2(A, M, N, AMAN, B, C, X, MAXITR, CTX, W, Q,
     1   LT, P, V, SCALE, LLIST)
      INTEGER M, N
      EXTERNAL AMAN
      INTEGER MAXITR, LLIST(2, 1)
      DOUBLE PRECISION A(1), B(M), C(N), X(N), CTX, W(M)
      DOUBLE PRECISION Q(N, N), LT(N, N), P(N), V(M), SCALE(M)
      COMMON /DL5COM/ COND, BOUND, ITRPH1, ITRPH2
      INTEGER ITRPH1, ITRPH2
      DOUBLE PRECISION COND, BOUND
      INTEGER TOP, BOTTOM, KK, INDX1, INDX2, L5NKF
      INTEGER I
      LOGICAL UNBNDD
      DOUBLE PRECISION DFLOAT, D1MACH, DNRM2, EPS, THETA, UMAX
      DOUBLE PRECISION DV5MAX, XNRM, CNRM, PNRM, TEMP1
C   INITIALIZATION.
      EPS = DFLOAT(N)*D1MACH(4)
      TOP = M+1
      XNRM = DNRM2(N, X, 1)
      CNRM = DNRM2(N, C, 1)
      DO  1 I = 1, M
         CALL AMAN(.TRUE., A, M, N, I, X, TEMP1)
         W(I) = B(I)-TEMP1
         CALL AMAN(.FALSE., A, M, N, I, P, TEMP1)
         SCALE(I) = DNRM2(N, P, 1)
C/6S
C        IF (EPS*SCALE(I)*XNRM .LT. W(I)) CALL SETERR(
C    1      29HDLPPH2 - INITIAL X INFEASIBLE, 29, 3, 2)
C/7S
         IF (EPS*SCALE(I)*XNRM .LT. W(I)) CALL SETERR(
     1      'DLPPH2 - INITIAL X INFEASIBLE', 29, 3, 2)
C/
   1     CONTINUE
      KK = 0
      CALL SETD(N**2, 0.0D0, Q)
      TEMP1 = 0.0D0
      DO  2 I = 1, N
         Q(I, I) = 1.0D0
         TEMP1 = TEMP1+C(I)*X(I)
   2     CONTINUE
      CTX = TEMP1
      LLIST(1, M+1) = 1
      LLIST(2, 1) = M+1
      DO  3 I = 1, M
         LLIST(1, I) = I+1
         LLIST(2, I+1) = I
   3     CONTINUE
      ITRPH2 = 1
         GOTO  5
   4     ITRPH2 = ITRPH2+1
   5     IF (ITRPH2 .GT. MAXITR) GOTO  29
         XNRM = DNRM2(N, X, 1)
C   STEP 1-    APPEND THE NEW ACTIVE CONSTRAINTS.
         INDX1 = L5NKF(TOP, LLIST, KK)
         I = LLIST(1, INDX1)
            GOTO  7
   6        I = LLIST(1, I)
   7        IF (I .GE. TOP) GOTO  8
            IF (W(I) .LT. (-EPS)*SCALE(I)*XNRM) GOTO  6
            CALL AMAN(.FALSE., A, M, N, I, P, TEMP1)
            CALL DM5TOP(N, N, Q, 1, N, 1, N, P, 1, P)
            PNRM = DNRM2(N-KK, P(KK+1), 1)
            IF (PNRM .LE. EPS*SCALE(I)) GOTO  6
            CALL L5NKD(LLIST, I)
            CALL L5NKI(LLIST, I, INDX1)
            INDX1 = I
            CALL DC5APP(N, KK, Q, LT, KK+1, P)
            IF (KK .EQ. N) GOTO  8
            GOTO  6
C   STEP 2-    DELETE AN OLD CONSTRAINT IF POSSIBLE.
   8     IF (0 .GE. KK) GOTO 10
            CALL DM5TOP(N, N, Q, 1, KK, 1, N, C, 1, P)
            CALL DM5TOP(N, N, LT, 1, KK, 1, KK, P, 4, V)
            UMAX = DV5MAX(KK, V, INDX2)
            IF (EPS*CNRM .GE. UMAX) GOTO 9
               CALL DC5DRP(N, KK, Q, LT, INDX2)
               I = L5NKF(TOP, LLIST, INDX2)
               CALL L5NKD(LLIST, I)
               BOTTOM = LLIST(2, TOP)
               CALL L5NKI(LLIST, I, BOTTOM)
   9        CONTINUE
C   STEP 3-    COMPUTE THE GRADIENT PROJECTION.
  10     IF (KK .NE. 0) GOTO 12
            DO  11 I = 1, N
               P(I) = C(I)
  11           CONTINUE
            GOTO  14
  12        IF (EPS*CNRM .LT. UMAX) CALL DM5TOP(N, N, Q, 1, KK, 1, N, C,
     1         1, P)
            CALL DM5TOP(N, N, Q, 1, KK, 1, N, P, 2, P)
            DO  13 I = 1, N
               P(I) = C(I)-P(I)
  13           CONTINUE
  14     PNRM = DNRM2(N, P, 1)
         IF (KK .EQ. N .OR. PNRM .LT. EPS*CNRM) RETURN
C   STEP 4-    FIND THE NEXT CONSTRAINT ALONG THE
C              GRADIENT PROJECTION.
         UNBNDD = .TRUE.
         INDX1 = L5NKF(TOP, LLIST, KK)
         I = LLIST(1, INDX1)
            GOTO  16
  15        I = LLIST(1, I)
  16        IF (I .GE. TOP) GOTO  20
            CALL AMAN(.TRUE., A, M, N, I, P, V(I))
            IF (V(I) .GE. (-EPS)*SCALE(I)*PNRM) GOTO 19
               IF (.NOT. UNBNDD) GOTO 17
                  UNBNDD = .FALSE.
                  THETA = W(I)/V(I)
                  GOTO  18
  17              IF (V(I)*THETA .LT. W(I)) THETA = W(I)/V(I)
  18        CONTINUE
  19        CONTINUE
            GOTO  15
  20     IF (.NOT. UNBNDD) GOTO 21
C/6S
C           CALL SETERR(27HDLPPH2 - UNBOUNDED SOLUTION, 27, 4, 1)
C/7S
            CALL SETERR('DLPPH2 - UNBOUNDED SOLUTION', 27, 4, 1)
C/
            RETURN
C   STEP 5-    UPDATE THE CURRENT SOLUTION.
  21     TEMP1 = 0.0D0
         DO  22 I = 1, N
            TEMP1 = TEMP1+C(I)*P(I)
  22        CONTINUE
         IF (0.0D0 .GT. TEMP1) GOTO 23
            CTX = CTX+THETA*TEMP1
            GOTO  24
C/6S
C 23        CALL SETERR(36HDLPPH2 - TERMINATED FOR CONDITIONING, 36, 7
C    1         , 1)
C/7S
  23        CALL SETERR('DLPPH2 - TERMINATED FOR CONDITIONING', 36, 7
     1         , 1)
C/
            RETURN
  24     DO  25 I = 1, N
            X(I) = X(I)+THETA*P(I)
  25        CONTINUE
         I = LLIST(1, INDX1)
            GOTO  27
  26        I = LLIST(1, I)
  27        IF (I .GE. TOP) GOTO  28
            W(I) = W(I)-THETA*V(I)
            GOTO  26
  28     CONTINUE
         GOTO  4
C/6S
C 29  CALL SETERR(45HDLPPH2 - NUMBER OF ITERATIONS EXCEEDED MAXITR, 45
C    1   , 6, 1)
C/7S
  29  CALL SETERR('DLPPH2 - NUMBER OF ITERATIONS EXCEEDED MAXITR', 45
     1   , 6, 1)
C/
      RETURN
      END
