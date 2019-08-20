      REAL FUNCTION BPNM(N,ML,G,IG)
C
C THIS SUBROUTINE COMPUTES THE MAXIMUM COLUMN SUM(1-NORM) OF
C A BAND POSITIVE DEFINITE MATRIX
C
      INTEGER N,ML,IG
      REAL G(IG,N)
      REAL SUM
C/6S
C      IF (N.LT.1) CALL SETERR(12H BPNM-N.LT.1,12,1,2)
C      IF (ML.LT.1) CALL SETERR(13H BPNM-ML.LT.1,13,2,2)
C      IF (IG.LT.ML) CALL SETERR(14H BPNM-IG.LT.ML,14,3,2)
C/7S
       IF (N.LT.1) CALL SETERR(' BPNM-N.LT.1',12,1,2)
       IF (ML.LT.1) CALL SETERR(' BPNM-ML.LT.1',13,2,2)
       IF (IG.LT.ML) CALL SETERR(' BPNM-IG.LT.ML',14,3,2)
C/
      BPNM =0.0
      NMML = N-ML
C COMPUTE SUM OF COLUMN ELEMENTS STORED AS A COLUMN IN G
      JE = ML
      DO 4 I=1,N
         SUM = 0.0
         DO 1 J = 1,JE
 1          SUM = SUM + ABS(G(J,I))
         IF ( NMML.LT.I) JE = JE - 1
C COMPUTE SUM OF COLUMN ELEMENTS STORED ON DIAGONAL IN G
      K = I-1
      L = 2
 2    IF ( (L.GT.ML) .OR. (K.LT.1)  ) GO TO 3
      SUM = SUM + ABS(G(L,K))
      K = K-1
      L = L+1
      GO TO 2
 3    IF (SUM.GT.BPNM) BPNM = SUM
 4    CONTINUE
      RETURN
      END