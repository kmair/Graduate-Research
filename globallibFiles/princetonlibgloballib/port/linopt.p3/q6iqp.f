      SUBROUTINE Q6IQP(X, N, A, IA, RES, IP, Q, IQQ, C, BL, BU, IT
     1   , JT, JSIM, IAC, D, E, Y, ZL, G, IPRINT, MAXITR, IE
     2   , IERR, IWUNIT, EPSI, B)
      INTEGER IA, N, IQQ
      INTEGER IP, IT, JT, JSIM(N), IAC(IA), IPRINT
      INTEGER MAXITR
      REAL X(IQQ), A(IA, N), RES(IA), Q(IQQ, N), C(N), BL(N)
      REAL BU(N), D(N), E(N), Y(N), ZL(N, N), G(N), B(IP)
      INTEGER IB, IC, JDEC, JC, IQ, IS
      INTEGER JDEX, IZ, IDEX, ITER, I, J
      INTEGER K, ITM1, ITP1, JTP1
      LOGICAL POSDEF, CONVER
      REAL LAMBDA, ACUM, SDOT, F, S, SUM, EPS
      REAL SNRM2, SS, EPSI
      INTEGER IE
      LOGICAL TEMP, TEMP2
      INTEGER IWUNIT, ITMP
C
C THIS SUBROUTINE SOLVES THE INDEFINITE QUADRATIC PROGRAMMING
C PROBLEM OF MINIMIZING X(TRANSPOSE)QX/2+C(TRANSPOSE)X
C SUCH THAT AX-B=RES IS GREATER OR EQUAL TO ZERO.
C ON INPUT IT IS ASSUMED THAT THE FIRST IT CONSTRAINTS ARE
C ACTIVE AT THE INITIAL GUESS.I.E. THE ROWS OF A HAVE BEEN
C PERMUTED SO THAT RES(I)=0,I=1,...IT.
C PARAMETERS ON INPUT*
C    X    AN N VECTOR OF INITIAL GUESSES
C    N    THE LENGTH OF THE X VECTOR
C    A    AN IP X N MATRIX DEFINING THE LINEAR INEQUALITY
C          CONSTRAINTS OF ROW DIMENSION IA
C    IA   ROW DIMENSION OF A MATRIX, MUST BE AT LEAST IP
C   RES   THE RESIDUAL AT THE INITIAL GUESS
C         WHERE RES(I)=SUM OVER J OF (A(IAC(I),J)*X(J))-B(IAC(I))
C    IP   THE TOTAL NUMBER OF GENERAL EQUALITY AND INEQUALITY
C         CONSTRAINTS
C    Q    A SYMMETRIC INDEFINITE MATRIX,COMPLETELY FILLED IN
C         OF DIMENSION N X N DEFINING THE HESSIAN OF THE
C         FUNCTION TO BE MINIMIZED
C    IQQ   ROW DIMENSION OF Q MATRIX AT LEAST N.
C    C    AN N VECTOR INVOLVED IN THE SECOND TERM IN THE FUNCTION
C         TO BE MINIMIZED
C    BL   REAL ARRAY, LENGTH AT LEAST N, WHOSE ITH COMPONENT GIVES
C         LOWER BOUND ON X(I)
C    BU   REAL ARRAY, LENGTH AT LEAST N, WHOSE ITH COMPONENT GIVES
C         AN UPPER BOUND ON X(I)
C    IT   THE NUMBER OF EQUALITY AND INEQUALITY CONSTRAINTS ACTIVE
C         AT THE INITIAL GUESS
C    JT   A POINTER INTO THE JSIM ARRAY, X(JSIM(JT+1)) THROUGH
C         X(JSIM(N)) ARE CONSIDERED AT BOUNDS. USUALLY JSIM IS
C         SET INITIALLY TO N AND EVERY VARIABLE MAY MOVE.
C  JSIM   AN ARRAY OF LENGTH N CONTAINING THE NUMBERS 1 THROUGH N
C         IN ANY ORDER. THE FIRST JT NUMBERS OF JSIM INDICATE
C         THE ELEMENTS OF X WHICH ARE NOT AT THEIR BOUNDS.
C  IAC    AN ARRAY OF LENGTH IP CONTAINING THE NUMBERS 1 THOUGH IP
C         IN ANY ORDER EXCEPT THAT THE FIRST IT NUMBERS POINT TO
C         CONSTRAINTS THAT ARE INITIALLY ACTIVE
C  IPRINT IF IPRINT .LE. 0, NOTHING IS PRINTED. OTHERWISE
C         FUNCTION IS PRINTED EACH ITERATION
C MAXITR  MAXIMUM NUMBER OF PERMITTED ITERATIONS
C    IE   THE TOTAL NUMBER OF EQUALITY CONSTRAINTS
C         THE FIRST IE ROWS OF A SHOULD BE THE EQUALITY CONSTRAINTS
C         AND THE FIRST IE ELEMENTS OF IAC SHOULD POINT TO THEM.
C  IWUNIT OUTPUT UNIT NUMBER FOR PRINTING
C  EPSI   MACHINE PRECISION
C
C SCRATCH SPACE
C    D    SCRATCH VECTOR OF LENGTH N
C    E    SCRATCH VECTOR OF LENGTH N
C    Y    SCRATCH VECTOR OF LENGTH N
C    ZL   SCRATCH ARRAY DIMENSIONED (N,N) WHICH WILL HAVE
C         ORTHOGONAL DECOMPOSITION FOR ACTIVE CONSTRAINT MATRIX.
C    G    A SCRATCH VECTOR OF LENGTH NIP .GT. MAX(N,IP)
C
C PARAMETERS ON OUTPUT
C   X    THE SOLUTION
C   IT   THE NUMBER OF ACTIVE CONSTRAINTS AT THE SOLUTION
C   Q    THE LOWER HALF OF THIS MATRIX HAS BEEN DESTROYED
C  IERR  ERROR CONDITION,IF 0 -NO PROBLEM
C                        IF 4 - MAXIMUM NUMBER OF ITERATONS USED
C
C DETERINE E,D,L,Z
C
      ITER = 0
      RELDX = 0.0E0
      IQ1=0
      IF (JT .EQ. 0) GOTO 1
         CALL G6TZD(A, Q, N, IT, IP, POSDEF, IA, IQQ, JT, JSIM, IAC, D
     1      , E, Y, ZL, IE, EPSI)
         GOTO  2
   1     IT = 0
   2  IZ = N
      CONVER = .FALSE.
C
C COMPUTE THE GRADIENT= QX+C
C
      ITP1 = IT+1
   3     ITER = ITER+1
         IF (ITER .LE. MAXITR) GOTO 4
            IERR = 1
            RETURN
   4     DO  5 I = 1, N
            G(I)=C(I)
            IF(I.GT.1)G(I) = C(I)+SDOT(I-1, Q(1, I), 1, X(1), 1)
            G(I) = G(I)+SDOT(N-I+1, Q(I, I), IQQ, X(I), 1)
   5        CONTINUE
         IF (IPRINT .LE. 0) GOTO 12
C           F = SDOT(N, G(1), 1, X(1), 1)
            F = .5 * (SDOT(N, G(1), 1, X(1), 1) +
     1          SDOT(N, C(1), 1, X(1), 1))
              RELDF=0.0
             IF (ITER.NE.1.AND.F.EQ.0.0)RELDF=F-OLDF
             IF(ITER.NE.1.AND.F.NE.0.0)RELDF=(F-OLDF)/F
              OLDF=F
  12     TEMP = IT .GE. JT
         S=-1.0E0
         IS=0
         IS1=0
         IF (.NOT. TEMP) TEMP = CONVER
         IF (.NOT. TEMP) GOTO 36
            CONVER = .FALSE.
C
C HAVE FOUND MINIMUM IN THE SUBSPACE,DETERMINE IF IT IS
C ALSO THE MINIMUM OF THE PROBLEM BY FINDING THE LAGRANGE
C MULTIPLIERS
 745         CONTINUE
            S = 1.E0
C
            IF (IT .LE. 0) GOTO 21
               IF (JT .NE. 0) GOTO 14
                  DO  13 I = 1, IT
                     Y(I) = 0.E0
  13                 CONTINUE
                  GOTO  20
  14              DO  16 I = 1, IT
                     ACUM = 0.E0
                     DO  15 J = 1, JT
                        JDEC = JSIM(J)
                        ACUM = ACUM+ZL(J, I)*G(JDEC)
  15                    CONTINUE
                     Y(I) = ACUM
  16                 CONTINUE
                  IS = IT
                  Y(IT) = Y(IT)/D(IT)
                  IF (IT.GT.IE)S = Y(IT)
                  IF (IT .EQ. 1) GOTO 19
                     ITM1 = IT-1
                     DO  18 IB = 1, ITM1
                        I = IT-IB
                        Y(I) = (Y(I)-SDOT(IB, Q(I+1, I), 1, Y(I+1), 1))/
     1                     D(I)
                        IF (Y(I).GT.S.OR.I.LE.IE) GO TO 17
                           IS = I
                           S = Y(I)
  17                    CONTINUE
  18                    CONTINUE
  19              CONTINUE
  20        CONTINUE
  21        IF (JT .GE. N) GOTO 26
               JTP1 = JT+1
               DO  25 I = JTP1, N
                  IDEX = JSIM(I)
                  ACUM = G(IDEX)
                  IF (IT .LT. 1) GOTO 23
                     DO  22 J = 1, IT
                        JC = IAC(J)
                        ACUM = ACUM-A(JC, IDEX)*Y(J)
  22                    CONTINUE
  23              IF (X(IDEX) .GT. BL(IDEX)) ACUM = -ACUM
                  IF (ACUM .GE. S) GOTO 24
                     IS = -I
                     S = ACUM
  24              CONTINUE
  25              CONTINUE
C
C TEST FOR CONVERGENCE BY SEEING IF S IS NEGATIVE
C
  26        CONTINUE
             IF (IPRINT.LE.0) GO TO 27
             IS1=0
             IF(IS.GT.0)IS1=IAC(IS)
             ISM=-IS
             IF(IS.LT.0)IS1=-JSIM(ISM)
             IF (S.GT.0.0E0)IS1=0
  27        IF (S .LE. 0.E0) GOTO 29
               IF(JT-ITP1.GT.0)GO TO35
             IF (IPRINT.GT.0)
     1        CALL QP2NT(ITER,N,IP,X,A,IA,B,F,RELDF,RELDX,JT,JSIM,
     1       IT,IAC,IS1,IQ1,POSDEF,BU)
               RETURN
C
C DROP CONSTRAINT IS, NO CONVERGENCE
C
 29         CONTINUE
            IF (IS .LE. 0) GOTO 32
               CALL Q6DRP(Q, IQQ, IS, IT, IP, N, POSDEF, JT, IAC,
     1            JSIM, D, E, Y, ZL, IWUNIT)
               GOTO  35
 32            CALL S6IMD(A, IA, N, Q, IQQ, IS, IT, JT, POSDEF, JSIM,
     1            IAC, D, E, Y, ZL, IWUNIT)
  35        CONTINUE
C
C
C COMPUTE THE PROJECTED GRADIENT,UHY=Z(TRANSPOSE)G
C
  36     ITP1 = IT+1
         DO  38 I = ITP1, JT
            SUM = 0.E0
            DO  37 J = 1, JT
               JDEX = JSIM(J)
               SUM = SUM+ZL(J, I)*G(JDEX)
  37           CONTINUE
            Y(I) = SUM
  38        CONTINUE
              IF (IPRINT.GT.0)
     1        CALL QP2NT(ITER,N,IP,X,A,IA,B,F,RELDF,RELDX,JT,JSIM,
     1       IT,IAC,IS1,IQ1,POSDEF,BU)
 389         IF(S.GT.0.E0) RETURN
            EPS=EPSI*10.0
             SS=0.0E0
             DO 367 I=ITP1,JT
                SS=SS+ABS(Y(I))
 367         CONTINUE
            IF (SS.LT.EPS*10.0.AND.S.LT.0.0E0)GO TO 745
             IF (SS.LT.EPS)RETURN
C
C COMPUTE DBAR AND G=-DBAR(INVERSE)Y
C
         CALL B6ARD(G, JT, ITP1, POSDEF, D, E, Y, EPSI)
C
C COMPUTE THE SEARCH DIRECTION AND PUT IT IN Y
C
         DO  39 I = 1, JT
            Y(I) = SDOT(JT-IT, ZL(I, ITP1), IZ, G(ITP1), 1)
  39        CONTINUE
C
C FIND TH  DISTANCE TO THE NEAREST CONSTRAINT ALONG Y
C
         IQ = 0
         EPS=EPSI*SNRM2(JT,Y,1)
         LAMBDA = -1.E0
         IF (POSDEF) LAMBDA = 1.E0
         IF (IT .EQ. IP) GOTO 44
            DO  43 I = ITP1, IP
               SUM = 0.E0
               IC = IAC(I)
               DO  40 K = 1, JT
                  JDEX = JSIM(K)
                  SUM = SUM+A(IC, JDEX)*Y(K)
  40              CONTINUE
               G(I) = SUM
               ACUM = G(I)
               IF (-ACUM.LT.EPS) GO TO 43
                  ACUM = (-RES(IC))/ACUM
                  TEMP = LAMBDA .LT. 0.E0
                  IF (.NOT. TEMP) TEMP = ACUM .LT. LAMBDA
                  IF (.NOT. TEMP) GOTO 41
                     IQ = I
                     LAMBDA = ACUM
  41              CONTINUE
  42           IF (LAMBDA .EQ. 0.E0) GOTO  51
  43           CONTINUE
  44     DO  48 I = 1, JT
            J = JSIM(I)
            IF (ABS(Y(I)).LT.EPS) GO TO 48
            IF (Y(I) .GE. 0.E0) GOTO 45
             IH=-1
               ACUM = (-(X(J)-BL(J)))/Y(I)
               GOTO  46
  45           ACUM = ((-X(J))+BU(J))/Y(I)
                IH=1
  46        TEMP = LAMBDA .LT. 0.E0
            IF (.NOT. TEMP) TEMP = ACUM .LT. LAMBDA
            IF (.NOT. TEMP) GOTO 47
              IHIT=IH
               LAMBDA = ACUM
               IQ = -I
  47        IF (LAMBDA .EQ. 0.E0) GOTO  51
  48        CONTINUE
C
C UPDAT RESIDUALS
C
         IF (IP.LT.ITP1) GO TO 501
         DO  49 I = ITP1, IP
            IC = IAC(I)
            RES(IC) = RES(IC)+LAMBDA*G(I)
  49        CONTINUE
          IF (IQ.LE.0) GO TO 501
             IC=IAC(IQ)
             RES(IC)=0.0E0
 501       CONTINUE
C
C
C FIND NEW MINIMUM
C
          RELDX=0.0
         DO  50 I = 1, JT
            JDEX = JSIM(I)
            X(JDEX) = X(JDEX)+LAMBDA*Y(I)
            RELDX=RELDX+Y(I)*Y(I)
  50        CONTINUE
            RELDX=SQRT(RELDX)*LAMBDA
            XNORM=SNRM2(N,X,1)
             IF (XNORM.NE.0.0)RELDX=RELDX/XNORM
  51     CONTINUE
C
C HAS AA CONSTRAINT BEEN HIT(
C
         TEMP = POSDEF
         IF (TEMP) TEMP = IQ .EQ. 0
         IF (.NOT. TEMP) GOTO 53
            CONVER = .TRUE.
            TEMP2 = IT .EQ. 0
            IF (TEMP2) TEMP2 = JT .EQ. N
            IF (.NOT. TEMP2) GOTO 52
               RETURN
  52        CONTINUE
            IQ1=IQ
            GOTO  3
  53          IF (IQ)56,61,55
  55         CONTINUE
             IQ1=IAC(IQ)
               CALL Q6ADD(A, IQ, N, IT, POSDEF, IA, Q, IQQ, JT, JSIM,
     1            IAC, D, E, Y, ZL)
               GOTO  3
  56           ITMP = -IQ
               J=JSIM(ITMP)
               X(J)=BU(J)
               IF(IHIT.LT.0) X(J) =BL(J)
               IQ1=-J
  58           CALL A6SIM(Q, IQQ, IT, JT, IQ, POSDEF, JSIM, D, E, Y,
     1            ZL, N)
         GOTO  3
  61  IERR = 2
      RETURN
      END
