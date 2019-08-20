      SUBROUTINE DCL2SS(MDIM,M,N,AR,AI,BR,BI,D,XR,XI)
C
C  LEAST SQUARES SOLUTION OF THE COMPLEX LINEAR SYSTEM OF ALGEBRAIC
C  EQUATIONS A*X=B, WHERE A HAS BEEN FACTORED BY A CALL TO DCL2SD.
C
C  INPUT
C
C    MDIM - THE DIMENSIONED COLUMN SIZE OF AR, AI, BR AND BI.
C    M    - THE NUMBER OF EQUATIONS.
C    N    - THE NUMBER OF UNKNOWNS.
C    AR   - THE REAL PART OF THE DECOMPOSED MATRIX A.
C    AI   - THE IMAGINARY PART OF THE DECOMPOSED MATRIX A.
C    BR   - THE REAL PART OF THE RIGHT HAND SIDE B.
C    BI   - THE IMAGINARY PART OF THE RIGHT HAND SIDE B.
C    D    - AS GIVEN BY DCL2SD.
C
C  OUTPUT
C
C    BR   - BOTH BR AND BI HAVE BEEN CLOBBERED.
C           SQRT(SUM(I=N+1,M)(BR(I)**2+BI(I)**2)) IS THE
C    BI   - L2 NORM OF THE RESIDUAL IN THE SOLUTION OF THE EQUATIONS.
C    XR   - THE REAL PART OF THE SOLUTION VECTOR X.
C    XI   - THE IMAGINARY PART OF THE SOLUTION VECTOR X.
C           X=B IS OK.
C
C  SCRATCH SPACE ALLOCATED - NONE.
C
C  ERROR STATES -
C
C    1 - MDIM.LT.M.
C    2 - N.LT.1.
C    3 - M.LT.N.
C    4 - D(L).LE.0.
C    5 - A(L,L)=0.
C
      DOUBLE PRECISION AR(MDIM,N),AI(MDIM,N),BR(M),BI(M),D(N),
     1                 XR(N),XI(N)
C
      DOUBLE PRECISION Z,ZPW,QR,QI,DSQRT
C
C ... CHECK THE INPUT.
C
C/6S
C     IF (MDIM.LT.M) CALL SETERR(18HDCL2SS - MDIM.LT.M,18,1,2)
C     IF (N.LT.1) CALL SETERR(15HDCL2SS - N.LT.1,15,2,2)
C     IF (M.LT.N) CALL SETERR(15HDCL2SS - M.LT.N,15,3,2)
C/7S
      IF (MDIM.LT.M) CALL SETERR('DCL2SS - MDIM.LT.M',18,1,2)
      IF (N.LT.1) CALL SETERR('DCL2SS - N.LT.1',15,2,2)
      IF (M.LT.N) CALL SETERR('DCL2SS - M.LT.N',15,3,2)
C/
C
C ... APPLY Q-STAR TO THE RIGHT-HAND-SIDE B.
C
      DO 30 L=1,N
         Z=D(L)
C/6S
C        IF (Z.LE.0.0D0) CALL SETERR(18HDCL2SS - D(L).LE.0,18,4,2)
C/7S
         IF (Z.LE.0.0D0) CALL SETERR('DCL2SS - D(L).LE.0',18,4,2)
C/
         ZPW=DSQRT(AR(L,L)**2+AI(L,L)**2)
C/6S
C        IF (ZPW.EQ.0.0D0) CALL SETERR(17HDCL2SS - A(L,L)=0,17,5,2)
C/7S
         IF (ZPW.EQ.0.0D0) CALL SETERR('DCL2SS - A(L,L)=0',17,5,2)
C/
         QR=0.D0
         QI=0.D0
         DO 10 I=L,M
            QR=QR+AR(I,L)*BR(I)+AI(I,L)*BI(I)
   10       QI=QI+AR(I,L)*BI(I)-AI(I,L)*BR(I)
         QR=QR/(Z*ZPW)
         QI=QI/(Z*ZPW)
         DO 20 I=L,M
            BR(I)=BR(I)-QR*AR(I,L)+QI*AI(I,L)
   20       BI(I)=BI(I)-QR*AI(I,L)-QI*AR(I,L)
         XR(L)=BR(L)
   30    XI(L)=BI(L)
C
C ... BACK-SOLVE THE UPPER-TRIANGULAR SYSTEM U*X=(Q-STAR)*B.
C
      DO 60 II=1,N
         I=N+1-II
         ZPW=DSQRT(AR(I,I)**2+AI(I,I)**2)
         QR=XR(I)
         QI=XI(I)
         IP1=I+1
         IF(IP1.GT.N)GOTO 50
         DO 40 J=IP1,N
            QR=QR-AR(I,J)*XR(J)+AI(I,J)*XI(J)
   40       QI=QI-AR(I,J)*XI(J)-AI(I,J)*XR(J)
   50    XR(I)=-( QR*AR(I,I)+QI*AI(I,I))/(ZPW*D(I))
   60    XI(I)=-(-QR*AI(I,I)+QI*AR(I,I))/(ZPW*D(I))
      RETURN
      END
