      SUBROUTINE L2SFQ(FW,MD,XQ,WQ,MQ,K,T,NT,A)
C
C
C TO FIT A FUNCTION F WITH A B-SPLINE.
C
C THE MNEMONIC IS - DOUBLE PRECISION L2 SPLINE FIT TO A FUNCTION,
C                   USING A QUADRATURE RULE.
C
C METHOD - WEIGHTED LEAST-SQUARES FIT.
C
C   THE FUNCTIONAL
C
C       SUM(J = 1,...,MD) INTEGRAL ( T(1) TO T(NT) )
C          ( (F-SUP-(J-1) - F-SUP-(J-1))**2 * W-SUB-J (X) ) DX
C
C   IS MINIMIZED OVER ALL B-SPLINES F OF ORDER K ON THE MESH T.
C
C   THE INTEGRALS ARE DONE PIECEWISE WITH THE MQ POINT
C   QUADRATURE RULE WITH WEIGHTS WQ AND ABSCISSAE XQ.
C
C INPUT -
C
C   FW     - THE NAME OF A USER SUPPLIED SUBROUTINE
C
C                    FW(X,NX,MD,FX,WX)
C
C            F MUST BE DECLARED EXTERNAL IN THE USER S CALLING PROGRAM
C
C            INPUT -
C
C              X    - THE POINTS WHERE F AND W ARE TO BE EVALUATED
C              NX   - THE LENGTH OF THE ARRAY X.
C              MD   - THE NUMBER OF DERIVATIVES TO BE EVALUATED AN
C
C            OUTPUT -
C
C              FX - FX(I,J) = THE (J-1)-ST DERIVATIVE OF F AT X(I)
C                             J = 1,...,MD, I = 1,...,NX.
C              WX - WX(I,J) = THE WEIGHT W-SUB-J (X) FOR THE
C                             (J-1)-ST DERIVATIVE OF F AT X(I),
C                             J = 1,...,MD, I = 1,...,NX.
C                   IF WX IS NOT SET, WX = 1 IS THE DEFAULT.
C
C   MD     - THE NUMBER OF DERIVATIVES TO BE USED IN THE FITTING.
C   XQ     - THE INTEGRALS ARE DONE PIECEWISE USING THE QUADRATURE
C   WQ     - RULE ON (-1,+1) WITH MQ ABSCISSAE XQ AND WEIGHTS WQ.
C   MQ     - THE NUMBER OF POINTS IN THE QUADRATURE RULE.
C   K      - THE ORDER OF THE B-SPLINE TO BE USED.
C   T      - THE B-SPLINE MESH TO BE USED IN THE FITTING PROCEDURE
C   NT     - THE NUMBER OF POINTS IN THE MESH T.
C
C OUTPUT -
C
C   A      - THE COEFFICIENTS FOR THE B-SPLINE APPROXIMATION.
C
C SCRATCH SPACE ALLOCATED -
C
C   LET NDT = THE NUMBER OF DISTINCT POINTS IN THE B-SPLINE MESH T
C   AND NXFIT = (NDT-1) * MQ
C
C   THEN,         S( L2SFQ) =
C
C     NXFIT*(2*MD+1) +
C
C     MAX( S(FW) , (NT-K+MD)*K +
C
C                  MAX( 3*K , K*(MIN((K+1)/2,MD) + 1) ) + MD INTEG
C
C   WORDS.
C
C ERROR STATES -
C
C    1 - MD.LT.1.
C    2 - MD.GT.K.
C    3 - MQ.LT.1.
C    4 - XQ(I) NOT IN (-1,+1).
C    5 - K.LT.2.
C    6 - NT.LE.K.
C    7 - THE MESH T IS NOT MONOTONE INCREASING.
C    8 - MULT(T(I)).GT.K.
C    9 - (NDT-1)*MQ*MD.LT.NT-K.
C   10 - NEGATIVE WEIGHTS. (RECOVERABLE)
C   11 - SINGULAR LEAST SQUARES MATRIX. (RECOVERABLE)
C
      LOGICAL LS(1000)
      INTEGER IS(1000)
      REAL RS(1000)
      REAL WS(500)
      DOUBLE PRECISION DS(500)
      COMMON /CSTAK/DS
            INTEGER I,J,NDT,NXFIT,IXFIT,JXFIT,IY,IW,IQ,ISTKGT,NERROR,NER
     *R
      EXTERNAL FW
      INTEGER MD,MQ,K,NT
      REAL XQ(MQ),WQ(MQ),T(NT),A(1)
C
      EQUIVALENCE ( DS(1),WS(1),RS(1),IS(1),LS(1) )
C
C
C A(NT-K)
C
C
C
      CALL ENTER(1)
C
C CHECK THE INPUT FOR ERRORS.
C
      IF( MD.GE.1 )      GOTO 1000
C/6S
C     CALL SETERR(16H L2SFQ - MD.LT.1,16,1,2)
C/7S
      CALL SETERR(' L2SFQ - MD.LT.1',16,1,2)
C/
 1000 CONTINUE
      IF( MD.LE.K )      GOTO 1002
C/6S
C     CALL SETERR(16H L2SFQ - MD.GT.K,16,2,2)
C/7S
      CALL SETERR(' L2SFQ - MD.GT.K',16,2,2)
C/
 1002 CONTINUE
      IF( MQ.GE.1 )      GOTO 1004
C/6S
C     CALL SETERR(16H L2SFQ - MQ.LT.1,16,3,2)
C/7S
      CALL SETERR(' L2SFQ - MQ.LT.1',16,3,2)
C/
 1004 CONTINUE
      DO 1006 I = 1, MQ
            IF( XQ(I).GE.(-1.0E0).AND.XQ(I).LE.(+1.0E0) )      GOTO 1009
C/6S
C     CALL SETERR(29H L2SFQ - XQ(I) NOT IN (-1,+1),29,4,2)
C/7S
      CALL SETERR(' L2SFQ - XQ(I) NOT IN (-1,+1)',29,4,2)
C/
 1009 CONTINUE
 1006 CONTINUE
 1007 CONTINUE
      IF( K.GE.2 )      GOTO 1011
C/6S
C     CALL SETERR(15H L2SFQ - K.LT.2,15,5,2)
C/7S
      CALL SETERR(' L2SFQ - K.LT.2',15,5,2)
C/
 1011 CONTINUE
      IF( NT.GT.K )      GOTO 1013
C/6S
C     CALL SETERR(16H L2SFQ - NT.LE.K,16,6,2)
C/7S
      CALL SETERR(' L2SFQ - NT.LE.K',16,6,2)
C/
 1013 CONTINUE
C
C GET THE FITTING POINTS.
C
      NDT = 1
C FIND THE NUMBER OF DISTINCT MESH POINTS.
      I21017 = NT-1
      DO 1015 I = 1, I21017
      IF( T(I).LE.T(I+1) )      GOTO 1018
C/6S
C           CALL SETERR(46H L2SFQ - THE MESH T IS NOT MONOTONE INCREASIN
C    *G,46,7,2)
C/7S
            CALL SETERR(' L2SFQ - THE MESH T IS NOT MONOTONE INCREASING'
     *,46,7,2)
C/
 1018 CONTINUE
      IF( T(I+1).LE.T(I) )      GOTO 1020
      NDT = NDT + (1)
 1020 CONTINUE
      IF( I+K.LE.NT )      GOTO 1022
      GOTO 1015
 1022 CONTINUE
      IX1005 = I+K
      IF( T(IX1005).NE.T(I) )      GOTO 1024
C/6S
C     CALL SETERR(24H L2SFQ - MULT(T(I)).GT.K,24,8,2)
C/7S
      CALL SETERR(' L2SFQ - MULT(T(I)).GT.K',24,8,2)
C/
 1024 CONTINUE
C
 1015 CONTINUE
 1016 CONTINUE
      NXFIT = (NDT-1)*MQ
C
      IF( NXFIT*MD.GE.NT-K )      GOTO 1026
C/6S
C     CALL SETERR(30H L2SFQ - (NDT-1)*MQ*MD.LT.NT-K,30,9,2)
C/7S
      CALL SETERR(' L2SFQ - (NDT-1)*MQ*MD.LT.NT-K',30,9,2)
C/
 1026 CONTINUE
C
      IXFIT = ISTKGT(NXFIT,3)
C CREATE THE FITTING POINTS.
C
      JXFIT = IXFIT
      I21030 = NT-1
      DO 1028 I = 1, I21030
      IF( T(I).NE.T(I+1) )      GOTO 1031
      GOTO 1028
 1031 CONTINUE
C
      DO 1033 J = 1, MQ
            WS(JXFIT) = AMAX1(T(I),AMIN1(0.5E0*((T(I)+T(I+1))+(T(I+1)-T(
     *I))*XQ(J)),T(I+1)))
      JXFIT = JXFIT + (1)
 1033 CONTINUE
 1034 CONTINUE
C
 1028 CONTINUE
 1029 CONTINUE
      IY = ISTKGT(MD*NXFIT,3)
      IW = ISTKGT(MD*NXFIT,3)
C
      CALL SETR(MD*NXFIT,1.0E0,WS(IW))
C DEFAULT WEIGHTS.
C
      CALL FW(WS(IXFIT),NXFIT,MD,WS(IY),WS(IW))
C
      JXFIT = IW
C NOW ADJUST THE WEIGHTS.
      I21038 = NT-1
      DO 1036 I = 1, I21038
      IF( T(I).NE.T(I+1) )      GOTO 1039
      GOTO 1036
 1039 CONTINUE
C
      DO 1041 IQ = 1, MQ
      DO 1044 J = 1, MD
      IX1001 = JXFIT+(J-1)*NXFIT
      WS(IX1001) = WS(IX1001) * (0.5E0*(T(I+1)-T(I))*WQ(IQ))
      IX1005 = JXFIT+(J-1)*NXFIT
      IF( WS(IX1005).GE.0.0E0 )      GOTO 1047
C/6S
C     CALL SETERR(25H L2SFQ - NEGATIVE WEIGHTS,25,10,1)
C/7S
      CALL SETERR(' L2SFQ - NEGATIVE WEIGHTS',25,10,1)
C/
      CALL LEAVE
      RETURN
 1047 CONTINUE
 1044 CONTINUE
 1045 CONTINUE
      JXFIT = JXFIT + (1)
 1041 CONTINUE
 1042 CONTINUE
C
 1036 CONTINUE
 1037 CONTINUE
      CALL DL2SH(WS(IXFIT),WS(IY),NXFIT,MD,WS(IW),K,T,NT,A)
C
      IF( NERROR(NERR).EQ.0 )      GOTO 1049
      CALL ERROFF
C/6S
C           CALL SETERR(38H L2SFQ - SINGULAR LEAST SQUARES MATRIX,38,11,
C    *1)
C/7S
            CALL SETERR(' L2SFQ - SINGULAR LEAST SQUARES MATRIX',38,11,
     *1)
C/
C
 1049 CONTINUE
      CALL LEAVE
C
      RETURN
C
      END