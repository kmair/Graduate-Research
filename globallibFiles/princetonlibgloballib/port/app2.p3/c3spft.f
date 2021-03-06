      REAL FUNCTION C3SPFT(X,Y,XX)
C
C USE THE LAGRANGE INTERPOLATION FORMULA TO FIT A CUBIC POLY-
C NOMIAL TO DATA PAIRS (X(I),Y(I),I=1,2,3,4).  EVALUATE AND RE-
C TURN THE SECOND DERIVATIVE OF THE CUBIC AT POINT XX.
C
      REAL X(4),Y(4),XX,XSUM,PROD
C
      XSUM=0.
      DO 10 I=1,4
   10    XSUM=XSUM+X(I)
      C3SPFT=0.
      DO 30 I=1,4
         PROD=1.
         DO 20 J=1,4
   20       IF (I .NE. J) PROD=PROD*(X(I)-X(J))
   30    C3SPFT=C3SPFT+2.*Y(I)*(3.*XX+X(I)-XSUM)/PROD
      RETURN
      END
