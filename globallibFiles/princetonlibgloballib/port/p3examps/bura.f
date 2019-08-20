C$SEQ BURA 0 20
C$TEST BURA
C***********************************************************************
C
C  EXAMPLE OF USE OF THE PORT PROGRAM BURAM
C
C***********************************************************************
      INTEGER IWRITE,I,M,N,NPTS
      REAL XMESH(11), F(11), P(3), Q(3), DELTA, STEP, X, XL, XR,
     1   ERROR(11), TCHBP
C
      IWRITE = I1MACH(2)
C
      NPTS = 11
      M = 2
      N = 2
      XL = -1.0E0
      XR = 1.0E0
      STEP = (XR-XL)/FLOAT(10)
C
      DO 10 I=1,11
         XMESH(I) = XL + FLOAT(I-1)*STEP
         F(I) = EXP(XMESH(I))
  10  CONTINUE
C
C
C   COMPUTE THE APPROXIMATION.
C
      CALL BURAM(NPTS, XMESH, F, M, N, P, Q, DELTA)
C
C   PRINT OUT THE ERRORS.
C
      WRITE (IWRITE,99)
  99  FORMAT (7H   MESH, 4X, 3HEXP, 7X, 5HERROR)
      DO 20 I=1,NPTS
         X = XMESH(I)
C
C   NOTE THAT TO EVALUATE THE APPROXIMATION WE MUST USE THE
C   FUNCTION TCHBP, WHICH EVALUATES A POLYNOMIAL GIVEN IN
C   TERMS OF ITS TCHEBYCHEFF EXPANSION.
C
         ERROR(I) = F(I) - TCHBP(M,P,X,XL,XR)/TCHBP(N,Q,X,XL,XR)
         WRITE (IWRITE,98) XMESH(I), F(I), ERROR(I)
  98     FORMAT (2F8.4,1PE12.2)
  20  CONTINUE
      STOP
      END
