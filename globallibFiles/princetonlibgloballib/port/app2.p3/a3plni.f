      LOGICAL FUNCTION A3PLNI(K,T,N,
     1                        X,NX,ILEFT,
     2                        BIX,
     3                        COL,DM,DP)
C
C  TO OBTAIN THE INTEGRAL OF ALL NON-ZERO BASIS SPLINES,
C
C           BIX(IX,I) =
C           INTEGRAL ( T(1) TO X(IX) ) B(ILEFT+I-K) (ZETA) DZETA
C
C  FOR IX=1,...,NX, AND I=1,...,K.
C
C  INPUT -
C
C    K      - THE ORDER OF THE B-SPLINES TO BE USED.
C             2.LE.K IS ASSUMED.
C    T      - THE B-SPLINE MESH.
C    N      - THE NUMBER OF POINTS IN THE MESH T.
C    X      - POINTS OF EVALUATION FOR THE INTEGRALS OF THE B-SPLINES.
C    NX     - THE NUMBER OF POINTS IN X.
C    ILEFT  - MUST HAVE T(ILEFT).LT.T(ILEFT+1) AND
C             T(ILEFT) .LE. X(IX) .LE. T(ILEFT+1), IX=1,...,NX.
C    COL    - COL(L)=INTEGRAL B(ILEFT+L-K) (ZETA) DZETA.
C    DM     - A SCRATCH ARRAY OF LENGTH K.
C    DP     - A SCRATCH ARRAY OF LENGTH K.
C
C  OUTPUT -
C
C    BIX    - THE INTEGRALS OF THE BASIS SPLINES.
C    A3PLNI - A3PLNI = .TRUE. IF SUCCESSFUL.
C             A3PLNI = .FALSE. IF T IS NOT MONOTONE INCREASING.
C
C  SCRATCH SPACE ALLOCATED - NONE.
C
C  ERROR STATES - NONE.
C
      REAL T(N),X(NX),BIX(NX,K),COL(K),DM(K),DP(K)
C
      REAL FACTOR
      LOGICAL A3PLNN
C
      A3PLNI=.TRUE.
C
      CALL SETR(NX*K,0.0E0,BIX)
C
      DO 30 IX=1,NX
C
         JJ=0
         DO 20 J=1,K
            IF (.NOT.A3PLNN(T,N,X(IX),ILEFT,J,
     1                      COL,DM,DP,JJ)) GO TO 70
C
            DO 10 L=1,J
C
               IDX1=MAX0(1,ILEFT+L-J)
               IDX2=MIN0(N,ILEFT+L)
C
               IF (T(IDX1).GE.T(IDX2)) GO TO 70
C
 10            BIX(IX,L)=BIX(IX,L)+((X(IX)-T(IDX1))/(T(IDX2)-T(IDX1)))*
     1                             COL(L)
 20         CONTINUE
 30      CONTINUE
C
      DO 60 L=1,K
         IDX1=MIN0(N,ILEFT+L)
         IDX2=MAX0(1,ILEFT+L-K)
         FACTOR=(T(IDX1)-T(IDX2))/FLOAT(K)
         DO 50 IX=1,NX
 50         BIX(IX,L)=BIX(IX,L)*FACTOR
 60      CONTINUE
C
      GO TO 80
C
 70   A3PLNI=.FALSE.
C
 80   RETURN
C
      END
