      LOGICAL FUNCTION D3PLNE(K,T,N,A,X,NX,ID,NID,FX,IDIM,ADIFF,ILO,
     1                        ILEFT,IDM)
C
C  TO EVALUATE THE B-SPLINE AND ITS DERIVATIVES
C
C        FX(I,J) = F(ID(J))(X(I))
C
C  FOR I=1,...,NX AND J=1,...,NID WHERE
C
C      F(X) = SUM(I=1,...,N-K)(A(I)*B(I)(X)).
C
C  INPUT
C
C    K     - THE ORDER OF THE B-SPLINES TO BE USED.
C            2.LE.K IS ASSUMED.
C    T     - THE B-SPLINE MESH.
C    N     - THE NUMBER OF POINTS IN THE MESH T.
C    A     - THE B-SPLINE COEFFICIENTS, N-K OF THEM.
C    X     - POINTS OF EVALUATION FOR THE B-SPLINE.
C            X MONOTONE INCREASING WOULD HELP, BUT IT IS NOT NECESSARY.
C    NX    - THE NUMBER OF POINTS IN X.
C    ID    - DERIVATIVES OF THE B-SPLINE DESIRED.
C            0.LE.ID(1).LT.ID(2).LT. ... .LT.ID(NID).LT.K IS ASSUMED.
C    NID   - THE NUMBER OF DERIVATIVES DESIRED, THE LENGTH OF ID.
C    IDIM  - K.LE.IDIM IS ASSUMED. THE LARGER IDIM IS, THE FASTER D3PLNE
C            RUNS, SO LONG AS X IS MONOTONE INCREASING.
C            IDIM NEED NOT BE LARGER THAN N+K-2.
C    ADIFF - A DOUBLE PRECISION SCRATCH ARRAY OF LENGTH IDIM*NID.
C    ILO   - IF ILO.LE.0 THEN COMPUTATION STARTS FROM SCRATCH.
C            IF ILO.GT.0 THEN THE VALUES OF K,T,N,A,ID,NID,IDIM,ILO AND
C            ADIFF ARE ASSUMED TO BE UNCHANGED FROM THE LAST CALL.
C    ILEFT - T(ILEFT).LE.X(1).LT.T(ILEFT+1) WOULD HELP, BUT IT IS NOT
C            NECESSARY.
C    IDM   - POINTER TO 3*K SCRATCH WORKING LOCATIONS.
C
C  OUTPUT
C
C    FX     - THE VALUES OF F AND ITS DERIVATIVES.
C    ADIFF  - SCRATCH VALUES ALTERED.
C    ILO    - SCRATCH VALUE ALTERED.
C    ILEFT  - THE INTERVAL INDICATOR FOR X(NX).
C    D3PLNE - .TRUE. IF SUCCESSFUL.
C             .FALSE. IF T IS NOT MONOTONE INCREASING.
C
C  SCRATCH SPACE ALLOCATED - NONE.
C
C  ERROR STATES - NONE.
C
      DOUBLE PRECISION T(N),A(1),X(NX),FX(NX,NID),ADIFF(IDIM,NID)
C     DOUBLE PRECISION A(N-K)
      INTEGER ID(NID)
C
      LOGICAL D3PLND,D3PLNV
C
      COMMON /CSTAK/DS
      DOUBLE PRECISION DS(500)
      DOUBLE PRECISION WS(1)
      EQUIVALENCE (DS(1),WS(1))
C
      D3PLNE=.TRUE.
C
      IDP=IDM+K
      ICOL=IDP+K
C
      IDIML=MIN0(IDIM,N+K-1-ILEFT)
C
      IF (T(1).GE.T(N)) GO TO 50
C
      DO 40 I=1,NX
C
         ILEFT=INTRVD(N,T,X(I))
         IF (ILEFT.EQ.N) ILEFT=0
         IF (ILEFT.EQ.0) GO TO 20
C
C ...... GET ADIFF, IF NECESSARY.
C
         IF (ILO.LE.ILEFT.AND.ILEFT.LE.IDIM+ILO-K.AND.ILO.GE.1) GO TO 10
C
         ILO=ILEFT
         IDIML=MIN0(N+K-1-ILEFT,IDIM)
C
         IF (.NOT.D3PLND(K,T,N,A,ID,NID,ILO,IDIML,ADIFF)) GO TO 50
C
 10      IF (.NOT.D3PLNV(T,ADIFF,N,K,X(I),I,ILEFT,ID,NID,ILO,IDIML,FX,
     1                   NX,WS(ICOL),WS(IDM),WS(IDP))) GO TO 50
C
         GO TO 40
C
C ...... X(I) IS OUTSIDE (T(1),T(N)).
C
 20      DO 30 J=1,NID
 30         FX(I,J)=0.0D0
C
 40      CONTINUE
C
      GO TO 60
C
 50   D3PLNE=.FALSE.
C
 60   RETURN
C
      END
