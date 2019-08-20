      SUBROUTINE C2SPQU(X, Y, YP, YPP, N, X1, X2, ANS)
      INTEGER N
      REAL X(N), Y(N), YP(N), YPP(N), X1, X2
      REAL ANS
      INTEGER J, INTRVR, K, JP, M
      REAL H, Y3P, Y3M, XP, XM, S
      REAL SP, SPP, S3
C
C GIVEN CUBIC SPLINE, INTEGRATE FROM X1 TO X2
C
C/6S
C     IF (N .LT. 2) CALL SETERR(17HC2SPQU - N .LT. 2, 17, 1, 2)
C     IF (X1 .LT. X(1)) CALL SETERR(21HC2SPQU - X1 .LT. X(1), 21, 2, 2)
C     IF (X2 .GT. X(N)) CALL SETERR(21HC2SPQU - X2 .GT. X(N), 21, 3, 2)
C     IF (X2 .LT. X1) CALL SETERR(19HC2SPQU - X2 .LT. X1, 19, 4, 2)
C/7S
      IF (N .LT. 2) CALL SETERR('C2SPQU - N .LT. 2', 17, 1, 2)
      IF (X1 .LT. X(1)) CALL SETERR('C2SPQU - X1 .LT. X(1)', 21, 2, 2)
      IF (X2 .GT. X(N)) CALL SETERR('C2SPQU - X2 .GT. X(N)', 21, 3, 2)
      IF (X2 .LT. X1) CALL SETERR('C2SPQU - X2 .LT. X1', 19, 4, 2)
C/
      J = INTRVR(N, X, X1)
      K = INTRVR(N, X, X2)
      IF (X2 .EQ. X(K)) K = K-1
      ANS = 0.0E0
      IF (J .GE. K) GOTO 2
         Y3M = (YPP(J+1)-YPP(J))/(X(J+1)-X(J))
         JP = J+1
         DO  1 M = JP, K
            Y3P = (YPP(M+1)-YPP(M))/(X(M+1)-X(M))
            ANS = ANS+(Y3P-Y3M)*X(M)**4
            Y3M = Y3P
   1        CONTINUE
C
C LOWER LIMIT
C
   2  M = J+1
      H = X(M)-X(J)
      XP = (X(M)-X1)/H
      XM = (X1-X(J))/H
      S = Y(J)*XP+Y(M)*XM-H*H*(YPP(J)*XP*(1.0-XP*XP)+YPP(M)*XM*(1.0-XM*
     1   XM))/6.0
      SP = (Y(M)-Y(J))/H-H*(YPP(J)*(3.0*XP*XP-1.0)+YPP(M)*(1.0-3.0*XM*
     1   XM))/6.0
      SPP = YPP(J)*XP+YPP(M)*XM
      S3 = (YPP(M)-YPP(J))/H
      ANS = ANS-X1*(24.*S-X1*(12.*SP-X1*(4.*SPP-X1*S3)))
C
C UPPER LIMIT
C
      M = K+1
      H = X(M)-X(K)
      XP = (X(M)-X2)/H
      XM = (X2-X(K))/H
      S = Y(K)*XP+Y(M)*XM-H*H*(YPP(K)*XP*(1.0-XP*XP)+YPP(M)*XM*(1.0-XM*
     1   XM))/6.0
      SP = (Y(M)-Y(K))/H-H*(YPP(K)*(3.0*XP*XP-1.0)+YPP(M)*(1.0-3.0*XM*
     1   XM))/6.0
      SPP = YPP(K)*XP+YPP(M)*XM
      S3 = (YPP(M)-YPP(K))/H
      ANS = ANS+X2*(24.*S-X2*(12.*SP-X2*(4.*SPP-X2*S3)))
      ANS = ANS/24.E0
      RETURN
      END
