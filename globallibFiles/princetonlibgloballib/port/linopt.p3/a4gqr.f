      SUBROUTINE A4GQR(K, M, N, Q, R, B, RHS)
C
C THIS SUBROUTINE UPDATES THE QR DECOMPOSITION OF A MATRIX
C WHEN THE VECTOR B IS ADDED AS THE LAST COLUMN OF THE MATRIX
C
      INTEGER K, I, M, N
      REAL Q(K, 1), R( 1), B(1), RHS(K)
      REAL BETA, ALPHA, F, SDOT
       MMN=M-N
      N = N+1
      CALL H4HG(MMN,B(N),ALPHA,BETA,1)
      DO 10 I=1,M
         F=SDOT(MMN,Q(N,I),1,B(N),1)/ALPHA
         CALL SAXPY(MMN,F,B(N),1,Q(N,I),1)
 10   CONTINUE
      F=SDOT(MMN,B(N),1,RHS(N),1)/ALPHA
      CALL SAXPY(MMN,F,B(N),1,RHS(N),1)
      IS=(N*(N-1))/2+1
      IF (N.GT.1)CALL SCOPY(N-1,B,1,R(IS),1)
      IS=IS+N-1
      R(IS)=BETA
      RETURN
      END
