      SUBROUTINE GAUSQ(N,A,B,C,NU,X,W)
C
C  J. L. BLUE,  03 JAN 78
C
C  LET THE RECURRENCE RELATION
C
C  X*P(X,L)=A(L)*P(X,L+1)+B(L)*P(X,L)+C(L)*P(X,L-1), L=1,...,2*N
C
C  DEFINE POLYNOMIALS OF DEGREE L-1, WITH P(X,0)=0 AND P(X,1)=1.
C
C  LET NU(L)=INTEGRAL(A TO B)(W(X)*P(X,L))DX, L=1,...,2*N,
C
C  WHERE W(X) IS SOME NON-NEGATIVE WEIGHT FUNCTION ON (A,B).
C
C  THIS ROUTINE THEN RETURNS (X(I),W(I)), I=1,...,N, SUCH THAT
C  THE QUADRATURE RULE GIVEN BY
C
C      INTEGRAL(A TO B)(W(X)*F(X))DX = SUM(I=1,...,N)(W(I)*F(X(I)))
C
C  IS EXACT FOR ALL POLYNOMIALS OF DEGREE LESS THAN 2*N.
C
C  SCRATCH SPACE ALLOCATED - 9*N REAL WORDS.
C
C  ERROR STATES -
C
C    1 - N.LT.1.
C    2 - A(I)=0.
C    3 - CANNOT OBTAIN X AND W.
C
C  SACK AND DONOVAN, NUM. MATH. 18, 465-478(1972).
C
      REAL A(1),B(1),C(1),NU(1),X(N),W(N)
C     REAL (A,B,C,NU)(2*N)
C
      REAL ABCMAX,MEPS,R1MACH
      LOGICAL OK,A6AUS0
C
      COMMON /CSTAK/DS
      DOUBLE PRECISION DS(500)
      REAL WS(1)
      EQUIVALENCE (DS(1),WS(1))
C
C ... MEPS IS THE ROUNDING ERROR LEVEL OF THE MACHINE.
C
      MEPS=R1MACH(4)
C
C/6S
C     IF (N.LT.1) CALL SETERR(15H GAUSQ - N.LT.1,15,1,2)
C/7S
      IF (N.LT.1) CALL SETERR(' GAUSQ - N.LT.1',15,1,2)
C/
C
      NM1=N-1
      N2=2*N
C
C ... SCALE TO PREVENT OVER OR UNDER FLOW.
C
      ABCMAX=0.0E0
      DO 10 I=1,N2
C/6S
C        IF (A(I).EQ.0.0E0) CALL SETERR(15H GAUSQ - A(I)=0,15,2,2)
C/7S
         IF (A(I).EQ.0.0E0) CALL SETERR(' GAUSQ - A(I)=0',15,2,2)
C/
 10      ABCMAX=AMAX1(ABCMAX,ABS(A(I)),ABS(B(I)),ABS(C(I)))
      DO 20 I=1,N2
         A(I)=A(I)/ABCMAX
         B(I)=B(I)/ABCMAX
         C(I)=C(I)/ABCMAX
 20      NU(I)=NU(I)/ABCMAX
C
C ... ALLOCATE SCRATCH SPACE.
C
      IAC=ISTKGT(9*N,3)-1
      IB=IAC+N2
      IA2=IB+N
      IE=IA2+N
      IR1=IE+N
      IR2=IR1+N2
C
      DO 30 L=2,N2
         IDXAC=IAC+L
 30      WS(IDXAC)=A(L-1)*C(L)
C
C ... COMPUTE ORTHOGONAL POLYNOMIALS WITH GIVEN WEIGHT
C ... AND THEN COMPUTE TRIDIAGONAL MATRIX
C
      CALL A6AUSQ(A,B,WS(IAC+1),NU,WS(IB+1),WS(IA2+1),N,WS(IR1+1),
     1            WS(IR2+1))
C
      OK=.FALSE.
      IF (1.GT.NM1) GO TO 50
      DO 40 L=1,NM1
         IDXE=IE+L+1
         IDXA2=IA2+L
         IDXB=IB+L
         IF (WS(IDXA2).LT.0.0E0) GO TO 70
         WS(IDXE)=SQRT(WS(IDXA2))
 40      X(L)=WS(IDXB)
 50   X(N)=WS(IA2)
C
C ... SOLVE THE EIGENSYSTEM.
C
      OK=A6AUS0(N,MEPS,X,WS(IE+1),W)
C
      DO 60 L=1,N
         X(L)=X(L)*ABCMAX
 60      W(L)=NU(1)*(W(L)**2)*ABCMAX
C
   70 CALL ISTKRL(1)
C
C/6S
C     IF (.NOT.OK)
C    1   CALL SETERR(30H GAUSQ - CANNOT OBTAIN X AND W,30,3,1)
C/7S
      IF (.NOT.OK)
     1   CALL SETERR(' GAUSQ - CANNOT OBTAIN X AND W',30,3,1)
C/
C
      RETURN
C
      END