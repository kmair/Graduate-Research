      SUBROUTINE  SPFSF(N, MRP, MCP, JROW, IWORK, IWMAX, IFILL)
      INTEGER N
      INTEGER MRP(N), MCP(N), IWORK(IWMAX)
      EXTERNAL JROW
      INTEGER IFILL, INDEX
      DOUBLE PRECISION DD(500)
      COMMON /CSTAK/ DD
      INTEGER IM, ISTKGT
      INTEGER I, K, Q
      INTEGER ISP(1000)
      LOGICAL TEMP1
      EQUIVALENCE (DD(1), ISP(1))
C       SYMBOLIC FACTORIZATION OF A MATRIX
C INPUT PARAMTERS
C N     ORDER OF SYSTEM
C MRP   INTEGER VECTOR LENGTH N, ROW ORDER
C MCP    INTEGER VECTOR LENGTH N,COLUMN ORDER
C JROW   SUBROUTINE,DECLARED EXTERNAL IN THE MAIN PROGRAM
C        WHCIH HAS THE CALLING SEQUENCE JROW(I,JCOL,NUM)
C         WHICH HAS INPUT PARAMETER I AND OUTPUT PARAMETERS
C         JCOL AND NUM WHICH RETURNS IN JCOL THE NUM COLUMN
C         INDICES CORRESPONDING TO THE NUM NONZERO ENTRIES
C         OF THE ITH ROW OF THE MATRIX
C IWMAX LENGTH OF WORK STACK, SHOULD BE AT LEAST 3N+2
C       PREFERABLY TWICE THAT SIZE
C OUTPUT PARAMTERS
C IWORK     INTEGER VECTOR  WHICH HAS THE INFORMATION OF THE
C           SYMBOLIC FACTORIZATION. FIRST N+1 QUANTITIES INDICATE
C           THE BEGINNING OF EACH L ROW IN SYMBOLIC FACT. NEXY
C           N+1 GIVE BEGINNING OF EACH L ROW, AFTER THAT COMES
C           THE COLUMN INDICES OF THE FACTORIZATION
C IFILL     AMOUNT OF STORAGE NEEDED TO STORE FULL LU FACTOR-
C           IZATION OF A MATRIX
C ERROR STATES
C 1    N.LT.1     FATAL
C 2    R NOT IN 1 THROUGH N     FATAL
C 3    ELEMENTS OF C NOT IN 1 THROUGH N
C 4 IWMAX LESS THAN 3N+1
C 10+K  KTH ROW IN A IS NULL     FATAL
C 10+N+K DUPLICATE ENTRY IN ROW K   FATAL
C 10+2N+K STORAGE EXCEEDED WHILE PROCESSING ROW K    RECOVERABLE
C 10+3N+K NULL PIVOT AT ROW K FATAL
C STORAGE TAKEN FROM PORT STACK-3N INTEGER LOCATIONS
C/6S
C     IF (N .LT. 1) CALL SETERR(13H SPFSF-N.LT.1, 13, 1, 2)
C/7S
      IF (N .LT. 1) CALL SETERR(' SPFSF-N.LT.1', 13, 1, 2)
C/
      CALL ENTER(1)
      Q=ISTKGT(3*N,2)
      IM=Q+N
      DO  1 I = 1, N
         TEMP1 = MRP(I) .LT. 1
         IF (.NOT. TEMP1) TEMP1 = MRP(I) .GT. N
C/6S
C        IF (TEMP1) CALL SETERR(23H SPFSF-MRP OUT OF RANGE, 23, 2, 2)
C/7S
         IF (TEMP1) CALL SETERR(' SPFSF-MRP OUT OF RANGE', 23, 2, 2)
C/
         K = MCP(I)
         TEMP1 = K .LT. 1
         IF (.NOT. TEMP1) TEMP1 = K .GT. N
C/6S
C        IF (TEMP1) CALL SETERR(23H SPFSF-MCP OUT OF RANGE,23,3,2)
C/7S
         IF (TEMP1) CALL SETERR(' SPFSF-MCP OUT OF RANGE',23,3,2)
C/
         INDEX=IM+K
         ISP(INDEX)=I
   1     CONTINUE
C/6S
C     IF (IWMAX.LT.3*N+1) CALL SETERR(
C    122H SPFSF-IWMAX TOO SMALL,22,4,2)
C/7S
      IF (IWMAX.LT.3*N+1) CALL SETERR(
     1' SPFSF-IWMAX TOO SMALL',22,4,2)
C/
      IM=IM+1
      JLU=2*N+2
       JCOL=Q+2*N+1
       IAMAX=IWMAX-2*N-2
      CALL DS4FSF(N,MRP,ISP(IM),JROW,IWORK,IWORK(N+2),IWORK(JLU),IAMAX,
     1 ISP(Q),MCP,IERR,ISP(JCOL))
C/6S
C     IF (IERR.GT.10+3*N) CALL SETERR(17H SPFSF-NULL PIVOT,
C    1   17,IERR,2)
C/7S
      IF (IERR.GT.10+3*N) CALL SETERR(' SPFSF-NULL PIVOT',
     1   17,IERR,2)
C/
      IF (IERR.LT.10+2*N) GO TO 3
C/6S
C       CALL SETERR(23H SPFSF-STORAGE EXCEEDED,23,IERR,1)
C/7S
        CALL SETERR(' SPFSF-STORAGE EXCEEDED',23,IERR,1)
C/
        CALL LEAVE
        RETURN
C/6S
C3    IF(IERR.GT.10+N)CALL SETERR(
C    1  29H SPFSF-DUPLICATE ENTRY IN ROW,29,IERR,2)
C      IF (IERR.GT.0) CALL SETERR(20H SPFSF-NULL ROW IN A,
C    1   20,IERR,2)
C/7S
 3    IF(IERR.GT.10+N)CALL SETERR(
     1  ' SPFSF-DUPLICATE ENTRY IN ROW',29,IERR,2)
       IF (IERR.GT.0) CALL SETERR(' SPFSF-NULL ROW IN A',
     1   20,IERR,2)
C/
           IM=IM-1
           DO 20 I=1,N
              INDEX=IM+I
              INDEX1=ISP(INDEX)
              MCP(INDEX1)=I
  20        CONTINUE
           IFILL=IWORK(N+1)-1
          CALL LEAVE
   4  RETURN
      END
