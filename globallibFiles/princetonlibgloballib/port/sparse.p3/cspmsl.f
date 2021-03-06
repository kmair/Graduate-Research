      SUBROUTINE CSPMSL(N, MRP, MCP, IA, JA, A, IL, B, IB, NB)
      INTEGER N, IB, NB
      INTEGER MRP(N), MCP(N), IA(1), JA(1), IL(N)
      COMPLEX A(1), B(IB, NB)
      COMPLEX D(500)
      COMMON /CSTAK/ D
      INTEGER ISTKGT, ITMP, II(1)
      EQUIVALENCE (D(1), II(1))
C FORWARD SOLVE FOR SPARSE MATRICES
C INPUT PARAMETERS
C N     NUMBER OF EQUATIONS
C MRP   INTEGER VECTOR OF ROW PERMUTATIONS
C MCP   INTEGER VECTOR OF COLUMNS PERMUTATIONS
C IA    INTEGER VECTOR OF LENGTH N+1 GIVING BEGINNING OF
C       EACH ROW OF DECOMPOSITION IN A NAD JA ARRAYS
C JA    INTEGER VECTOR COMPUTED BY  SPLU
C A     COMPLEX ARRAY COMPUTED BY  CSPMLU
C IL    INTEGER VECTOR OF LENGTH N+1 POINTING TO L IN LU
C       DECOMPOSITION,COMPUTED BY CSPLU OR CSPCE
C B     MATRIX OF RIGHT HAND SIDES
C IB    ROW DIMENSION OF B MATRIX
C NB    NUMBER OF RIGHT HAND SIDES
C OUTPUT PARAMETERS
C B     THE SOLUTION MATRIX
C STORAGE SPACE ALLOCATED AND DEALLOCATED -N COMPLEX LOCATIONS
C ERROR CONDITIONS
C 1 N.LT.1    FATAL
C 2 IB.LT.N   FATAL
C 3 NB.LT.1   FATAL
C/6S
C     IF (N .LT. 1) CALL SETERR(13HCSPMSL-N.LT.1, 13, 1, 2)
C     IF (IB .LT. N) CALL SETERR(14HCSPMSL-IB.LT.N, 14, 2, 2)
C     IF (NB .LT. 1) CALL SETERR(14HCSPMSL-NB.LT.1, 14, 3, 2)
C/7S
      IF (N .LT. 1) CALL SETERR('CSPMSL-N.LT.1', 13, 1, 2)
      IF (IB .LT. N) CALL SETERR('CSPMSL-IB.LT.N', 14, 2, 2)
      IF (NB .LT. 1) CALL SETERR('CSPMSL-NB.LT.1', 14, 3, 2)
C/
      CALL ENTER(1)
      ITMP = ISTKGT(N, 5)
      CALL CS4MSL(N, MRP, MCP, IA, JA, A, IL, B, IB, NB, D(ITMP))
      CALL LEAVE
      RETURN
      END
