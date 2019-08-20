      REAL FUNCTION R2POLY(NN,PT,Q)
C R2POLY COMPUTES A LOWER BOUND ON THE MODULI OF THE ZEROS OF A
C POLYNOMIAL - PT IS THE MODULUS OF THE COEFFICIENTS.
      REAL Q(1),PT(1),X,XM,F,DX,DF,
     1   ABS,EXP,ALOG
      PT(NN) = -PT(NN)
C COMPUTE UPPER ESTIMATE OF BOUND.
      N = NN-1
      X = EXP( (ALOG(-PT(NN)) - ALOG(PT(1)))/FLOAT(N) )
      IF (PT(N).EQ.0.0E0) GO TO 20
C IF NEWTON STEP AT THE ORIGIN IS BETTER, USE IT.
          XM = -PT(NN)/PT(N)
          IF (XM.LT.X) X=XM
C CHOP THE INTERVAL (0,X) UNTIL F LE 0.
   20 XM = X*.1E0
      F = PT(1)
      DO 30 I = 2,NN
          F = F*XM+PT(I)
   30 CONTINUE
      IF (F.LE. 0.0E0) GO TO 40
          X = XM
          GO TO 20
   40 DX = X
C DO NEWTON ITERATION UNTIL X CONVERGES TO TWO DECIMAL PLACES.
   50 IF (ABS(DX/X) .LE. .005E0) GO TO 70
          Q(1) = PT(1)
          DO 60 I = 2,NN
               Q(I) = Q(I-1)*X+PT(I)
   60     CONTINUE
          F = Q(NN)
          DF = Q(1)
          DO 65 I = 2,N
               DF = DF*X+Q(I)
   65     CONTINUE
          DX = F/DF
          X = X-DX
          GO TO 50
   70 R2POLY = X
      RETURN
      END
