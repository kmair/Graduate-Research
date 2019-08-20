      SUBROUTINE DCLST2(MDIM,NDIM,M,N,AR,AI,BR,BI,NB,XR,XI)
C
C  LEAST SQUARES SOLUTION OF THE COMPLEX LINEAR ALGEBRAIC SYSTEM
C  OF EQUATIONS A*X=B.
C
C  METHOD - QR DECOMPOSITION OF A BY HOUSEHOLDER TRANSFORMATIONS.
C
C  INPUT
C
C    MDIM - THE DIMENSIONED COLUMN SIZE OF AR, AI, BR AND BI.
C    NDIM - THE DIMENSIONED COLUMN SIZE OF XR AND XI.
C    M    - THE NUMBER OF EQUATIONS.
C    N    - THE NUMBER OF UNKNOWNS. N.LE.M IS ASSUMED.
C    AR   - THE REAL PART OF THE MATRIX A.
C    AI   - THE IMAGINARY PART OF THE MATRIX A.
C    BR   - THE REAL PART OF THE RIGHT HAND SIDES B.
C    BI   - THE IMAGINARY PART OF THE RIGHT HAND SIDES B.
C    NB   - THE NUMBER OF RIGHT HAND SIDES B.
C
C  OUTPUT
C
C    AR   - BOTH THE REAL AND IMAGINARY PARTS OF THE
C    AI   - MATRIX A HAVE BEEN CLOBBERED.
C    BR   - BOTH BR AND BI HAVE BEEN CLOBBERED.
C           SQRT(SUM(I=N+1,M)(BR(I,J)**2+BI(I,J)**2)) IS THE
C    BI   - L2 NORM OF THE RESIDUAL IN THE SOLUTION FOR THE
C           J-TH RIGHT HAND-SIDE, J=1,...,NB.
C    XR   - THE REAL PART OF THE SOLUTION VECTOR X.
C    XI   - THE IMAGINARY PART OF THE SOLUTION VECTOR X.
C           X=B IS OK IF NDIM=MDIM.
C
C  SCRATCH SPACE ALLOCATED - N DOUBLE PRECISION WORDS.
C
C  ERROR STATES -
C
C    1 - MDIM.LT.M.
C    2 - NDIM.LT.N.
C    3 - N.LT.1.
C    4 - M.LT.N.
C    5 - NB.LT.1.
C    6 - WHEN XR=BR OR XI=BI MUST HAVE NDIM=MDIM.
C    7 - A IS RANK-DEFICIENT. (RECOVERABLE)
C
      DOUBLE PRECISION AR(MDIM,N),AI(MDIM,N),BR(MDIM,NB),BI(MDIM,NB),
     1                 XR(NDIM,NB),XI(NDIM,NB)
C
      COMMON /CSTAK/S
      DOUBLE PRECISION S(500)
      DOUBLE PRECISION QR,QI
C
      CALL ENTER(1)
C
C ... CHECK THE INPUT.
C
C/6S
C     IF (MDIM.LT.M) CALL SETERR(18HDCLST2 - MDIM.LT.M,18,1,2)
C     IF (NDIM.LT.N) CALL SETERR(18HDCLST2 - NDIM.LT.N,18,2,2)
C     IF (N.LT.1) CALL SETERR(15HDCLST2 - N.LT.1,15,3,2)
C     IF (M.LT.N) CALL SETERR(15HDCLST2 - M.LT.N,15,4,2)
C     IF (NB.LT.1) CALL SETERR(16HDCLST2 - NB.LT.1,16,5,2)
C/7S
      IF (MDIM.LT.M) CALL SETERR('DCLST2 - MDIM.LT.M',18,1,2)
      IF (NDIM.LT.N) CALL SETERR('DCLST2 - NDIM.LT.N',18,2,2)
      IF (N.LT.1) CALL SETERR('DCLST2 - N.LT.1',15,3,2)
      IF (M.LT.N) CALL SETERR('DCLST2 - M.LT.N',15,4,2)
      IF (NB.LT.1) CALL SETERR('DCLST2 - NB.LT.1',16,5,2)
C/
C
C ... MAKE SURE THAT NDIM=MDIM IF BR=XR OR BI=XI AS ARRAYS.
C
      QR=BR(1,1)
      QI=BI(1,1)
      XR(1,1)=+1.0D0
      XI(1,1)=+1.0D0
      BR(1,1)=-1.0D0
      BI(1,1)=-1.0D0
C/6S
C     IF ((XR(1,1).EQ.BR(1,1).OR.XI(1,1).EQ.BI(1,1)) .AND.
C    1     NDIM.NE.MDIM ) CALL SETERR
C    2   (48HDCLST2 - WHEN XR=BR OR XI=BI MUST HAVE NDIM=MDIM,48,6,2)
C/7S
      IF ((XR(1,1).EQ.BR(1,1).OR.XI(1,1).EQ.BI(1,1)) .AND.
     1     NDIM.NE.MDIM ) CALL SETERR
     2   ('DCLST2 - WHEN XR=BR OR XI=BI MUST HAVE NDIM=MDIM',48,6,2)
C/
      BR(1,1)=QR
      BI(1,1)=QI
C
      ID=ISTKGT(N,4)
C
C ... GET THE QR DECOMPOSITION OF A.
C
      CALL DCL2SD(MDIM,M,N,AR,AI,S(ID))
C
C ... IF A HAD RANK = N, SOLVE THE EQUATIONS ONE AT A TIME.
C
      IF (NERROR(NERR).NE.0) GO TO 20
C
      DO 10 J=1,NB
 10   CALL DCL2SS(MDIM,M,N,AR,AI,BR(1,J),BI(1,J),S(ID),XR(1,J),XI(1,J))
C
      GO TO 30
C
C ... HERE FOR A RANK-DEFICIENT MATRIX A.
C
 20   CALL ERROFF
C/6S
C     CALL SETERR(28HDCLST2 - A IS RANK-DEFICIENT,28,7,1)
C/7S
      CALL SETERR('DCLST2 - A IS RANK-DEFICIENT',28,7,1)
C/
C
 30   CALL LEAVE
C
      RETURN
C
      END
