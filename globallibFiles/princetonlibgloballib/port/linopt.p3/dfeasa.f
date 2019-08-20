       SUBROUTINE DFEASA(A,M,N,AMAN,IA,B,X,MAXITR,S,SIMP,
     1   ISIMP,E, PRINT,IAG,IAS,IPTG)
C THIS SUBROUTINE DETERMINES IF POSSIBLE A POINT WHICH SATISFIES
C A SYSTEM OF LINEAR INEQUALITY AND EQUALITY CONSTRAINTS
C AND OPTIONAL UPPPER AND LOWER BOUND CONSTRAINTS ON THE
C VARIABLES. THE USER PROVIDES A FUNCTION WHICH FOR A GIVEN
C ROW RETURNS EITHER THAT ROW OR THE INNER PRODUCT OF THAT
C ROW AND A SPECIFIED VECTOR. THUS IF THE CONSTRAINT MATRIX
C IS SPARSE, THE USER MAY TAKE ADVANTAGE OF THIS. THE
C ALGORITHM IS SIMPLEX-LIKE BUT INITIALLY PROJECTED GRADIENT
C STEPS MAY BE TAKEN-I.E. ONE CAN CUT ACROSS THE FEASIBLE
C REGION RATHER THAN VISITING VERTICES. THE QR DECOMPOSITION
C OF THE BASIS MATRIX IS SAVED. NO ATTEMPT IS MADE TO UTILIZE
C THE SPARSE STRUCTURE OF THE CONSTRAINTS EXCEPT FOR THE LOWER AND
C UPPER BOUND CONSTRAINTS.
C THE PARAMETERS HAVE THE FOLLOWING INTERPRETATION
C
C A     A SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION AMAN
C M     NUMBER OF GENERAL EQUALITY AND INEQUALITY CONSTRAINTS
C N     NUMBER OF UNKNOWNS
C AMAN  USER PROVIDED FUNCTION WHICH EITHER RETURNS A ROW OF THE MATRIX
C       OR DOES A VECTOR-VECTOR INNER PRODUCT WITH A PARTICULAR ROW.
C       AMAN IS INVOKED AS FOLLOWS
C       CALL AMAN(INNER,A,IA,N,IROW,P,T)
C       WHERE
C       INNER    INPUT LOGICAL VARIABLE, IF .TRUE., AMAN
C                SHOULD RETURN IN T THE INNER PRODUCT OF P AND
C                THE IROWTH ROW OF THE CONSTRAINT MATRIX
C       A        REAL VECTOR PASSED THROUGH TO THE SUBROUTINE.
C                IF THE USER DECIDES TO USE THE DEFAULT SUBROUTINE
C                DLPMAN, A SHOULD CONTAIN THE CONSTRAINT MATRIX
C                DIMENSIONED A(IA,N) IN THE MAIN PROGRAM.
C       IA       INTEGER VECTOR PASSED THROUGH TO THE SUBROUTINE.
C                IF THE USER DECIDES TO USE THE DEFAULT SUBROUTINE
C                DLPMAN, IA IS JUST A SCALAR GIVING THE ROW DIMENSION
C                OF THE CONSTRAINT MATRIX A
C       N        INPUT VARIABLE GIVING LENGTH OF P VECTOR
C       IROW     THE IROWTH ROW IS EITHER TO BE PUT IN P OR INVOLVED
C                IN THE INNER PRODUCT DEPENDING ON INNER
C       P        IF INNER IS .TRUE., THEN P IS AN INPUT VECTOR OF
C                LENGTH N INVOLVED IN THE INNER PRODUCT. IF INNER
C                IS .FALSE., ON OUTPUT IT SHOULD CONTAIN THE IROWTH
C                ROW OF A
C       T        IF INNER IS .TRUE., ON OUTPUT, T=THE INNER
C                PRODUCT OF P AND THE IROWTH ROW OF A
C                IF INNER .FALSE.,T NEED NOT BE TOUCHED
C IA    INTEGER SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION
C       AMAN
C B     INPUT VECTOR LENGTH M OF RIGHT HAND SIDE OF GENERAL CONSTRAINTS
C X     REAL VECTOR LENGTH N, ON INPUT APPROXIMATION TO THE SOLUTION,
C       WHICH NEED NOT BE FEASIBLE, ON OUTPUT-THE SOLUTION
C MAXITR MAXIMUM NUMBER OF ITERATIONS TOLERATED
C S     INTEGER INPUT SCALAR OF NUMBER OF SIMPLE CONSTRAINTS
C SIMP  REAL INPUT VECTOR OF LENGTH S HAVING SIMPLE CONSTRAINTS
C ISIMP INTEGER INPUT VECTOR TELLING THE ELEMENT OF X THE SIMPLE
C       CONSTRAINT PERTAINS, IF NEGATIVE IT IS AN UPPER BOUND
C       IF SIMP(1)=-5, SIMP(2)=10.0D0, ISIMP(1)=3, ISIMP(2)=-3
C       THE SOLUTION MUST SATISFY X(3) .GE. -5.0,X(3) .LE. 10.0D0
C E     INTEGER INPUT SCALAR GIVING THE NUMBER OF EQUALITY CONSTRAINTS
C       IT IS ASSUMED THAT THE FIRST E CONSTRAINTS ARE EQUALITY CONS.
C PRINT USER WRITTEN SUBOURINTE WHICH PRINTS INFORMATION ABOUT LP
C      EACH ITERATION. DEFAULT IS DLPRNT WHICH PRINTS NOTHING
C
C STORAGE TAKEN FROM PORT STACK -3N*N/2+4M+11N/2 REAL LOCATIONS
C      AND 2M+N+S INTEGER LOCATIONS
       INTEGER ISIMP(1), M, E, S, N
       INTEGER TPTR
       INTEGER IPSPTR,DVSPTR,DVGPTR
       EXTERNAL AMAN,PRINT
       INTEGER MAXITR
       DOUBLE PRECISION A(1),B(1),X(N),CTX,SIMP(1)
       INTEGER IA(1),IPTG(1)
       COMMON /CSTAK/ DSTAK
       DOUBLE PRECISION DSTAK(500)
       INTEGER WPTR,QPTR,LTPTR,PPTR,VPTR,SCLPTR
       INTEGER ISTAK(1000),ISTKGT
       DOUBLE PRECISION WS(500)
       EQUIVALENCE (DSTAK(1),ISTAK(1))
       EQUIVALENCE(DSTAK(1),WS(1))
C/6S
C      IF (N.LT.1)CALL SETERR(13HDFEASA-N.LT.1,13,1,2)
C      IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(
C    1 20HDFEASA-M,S,OR E.LT.0,20,2,2)
C      IF(E.GT.M.OR.S.GT.2*N)CALL SETERR(
C    1 24HDFEASA-E.GT.M.OR.S.GT.2N,24,3,2)
C      IF (MAXITR.LT.1)CALL SETERR(
C    1 18HDFEASA-MAXITR.LT.1,18,4,2)
C/7S
       IF (N.LT.1)CALL SETERR('DFEASA-N.LT.1',13,1,2)
       IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(
     1 'DFEASA-M,S,OR E.LT.0',20,2,2)
       IF(E.GT.M.OR.S.GT.2*N)CALL SETERR(
     1 'DFEASA-E.GT.M.OR.S.GT.2N',24,3,2)
       IF (MAXITR.LT.1)CALL SETERR(
     1 'DFEASA-MAXITR.LT.1',18,4,2)
C/
       CALL ENTER(1)
       WPTR = ISTKGT(M,4)
       IUPTR = ISTKGT(N,4)
       QPTR = ISTKGT(N**2,4)
       LTI=(N*(N+1))/2
       LTPTR = ISTKGT(LTI, 4)
       PPTR =ISTKGT(N, 4)
       VPTR = ISTKGT(M, 4)
       SCLPTR = ISTKGT(M, 4)
       IPSPTR = ISTKGT(N, 2)
       DVSPTR=1
       IF (S.GT.0)DVSPTR =ISTKGT(S, 2)
       DVGPTR = ISTKGT(M, 2)
       TPTR = ISTKGT(N, 4)
       IRHS = ISTKGT(N,4)
       ICCPTR = ISTKGT(N,4)
       IQ=N
       CALL DI4NTL(N,WS(QPTR),IQ,IPTG,M,ISTAK(IPSPTR),
     1 A,IA,AMAN,WS(SCLPTR),WS(TPTR))
       CALL DL4PH1(A,M,N,AMAN,IA,B,X,MAXITR,CTX,S,SIMP,ISIMP,E,
     1   WS(WPTR),WS(QPTR),IQ,WS(LTPTR),WS(PPTR),WS(VPTR),WS(SCLPTR),
     1   ISTAK(IPSPTR),IPTG,ISTAK(DVSPTR),ISTAK(DVGPTR),
     1   WS(TPTR),WS(IUPTR),PRINT,WS(IRHS),IAS,IAG,KK,WS(ICCPTR),IERR)
        IF (IERR.EQ.0)GO TO 10
        CALL ERROFF
C/6S
C     IF (IERR.EQ.6) CALL SETERR(
C    133HDFEASA-NO.OF ITER. EXCEEDS MAXITR,33,6,1)
C       IF (IERR.EQ.8)CALL SETERR(
C    127HDFEASA-NO FEASIBLE SOLUTION,27,8,1)
C      IF (IERR.EQ.9)CALL SETERR(
C    127HDFEASA-CONDITIONING PROBLEM,27,9,1)
C/7S
      IF (IERR.EQ.6) CALL SETERR(
     1'DFEASA-NO.OF ITER. EXCEEDS MAXITR',33,6,1)
        IF (IERR.EQ.8)CALL SETERR(
     1'DFEASA-NO FEASIBLE SOLUTION',27,8,1)
       IF (IERR.EQ.9)CALL SETERR(
     1'DFEASA-CONDITIONING PROBLEM',27,9,1)
C/
 10     CONTINUE
 20    CALL LEAVE
       RETURN
       END