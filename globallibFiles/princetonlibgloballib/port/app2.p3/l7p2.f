      SUBROUTINE L7P2(A, BC, BX, C, CN, G, INFO, ITER, LR, M, M0,
     1   MC, ME, MPP, MPP0, P, PC, PP, Q, R, RCOND, S, W, X)
      INTEGER MPP, MPP0, M, P, LR, PP
      INTEGER A(MPP), INFO, ITER, M0, MC, ME
      INTEGER PC
      REAL BC(2, M), BX(2, P), C(PP, M), CN(MPP0), G(P), Q(P
     1   , P)
      REAL R(LR), RCOND, S(P), W(P, 3), X(P)
      EXTERNAL  L7CHP,  B7VAL,  R7MDC, V2AXY,  D7TP1
      INTEGER NCA, NCD, I, J, K, L
      INTEGER MC1
      LOGICAL MOVE
      REAL BIG,  B7VAL,  R7MDC, MPMEPS, T
      REAL  B1, ZERO, T1, SC,  D7TP1
      DATA BIG/0.E+0/
      DATA MPMEPS/0.E+0/
      DATA ZERO/0.E+0/
C *** LOCAL VARIABLES ***
C-------------------------------  BODY  --------------------------------
      IF (BIG .GT. ZERO) GOTO 10
         BIG =  R7MDC(6)
         MPMEPS = (-(FLOAT(P)))* R7MDC(3)
  10  MC = P-PC
      M0 = MC
      ITER = 0
  20     ITER = ITER+1
         CALL  L7CHP(A, .FALSE., C, CN, G, LR, M, M0, MC, ME, MOVE, MPP,
     1      MPP0, NCA, NCD, P, PC, PP, Q, R, RCOND, S, W, W(1, 2), W(1
     2      , 3))
         IF (.NOT. MOVE) GOTO  100
         K = 0
         T = BIG
         IF (MC .GE. MPP) GOTO 60
            MC1 = MC+1
            DO  50 I = MC1, MPP
               J = IABS(A(I))
               SC =  D7TP1(C, J, P, PP, S)
               IF (SC .EQ. ZERO) GOTO  50
               IF (SC .LE. ZERO) GOTO 30
                  J = -J
                  SC = -SC
  30           B1 =  B7VAL(BC, BX, J, M, P)
               IF ( ABS(B1) .GE. BIG) GOTO  50
               T1 = (B1- D7TP1(C, J, P, PP, X))/SC
               IF (T .LE. T1) GOTO 40
                  K = I
                  L = J
                  T = T1
                  MOVE = T*SC .LT. MPMEPS* ABS(B1)
  40           CONTINUE
  50           CONTINUE
  60     IF (K .NE. 0) GOTO 70
            INFO = -1
            GOTO  110
  70     IF (.NOT. MOVE) GOTO 80
            CALL V2AXY(P, X, T, S, X)
            M0 = MC1
            A(K) = A(M0)
            A(M0) = L
            GOTO  20
  80     IF (K .LE. M0) GOTO 90
            M0 = M0+1
            A(K) = A(M0)
            A(M0) = L
            GOTO  20
  90     A(K) = L
         GOTO  20
 100  CONTINUE
 110  RETURN
      END
