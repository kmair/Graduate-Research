      SUBROUTINE DODEQ(N, F, A, B, EPS, ANS)
C
C  USING THE DIFFERENTIAL-EQUATION ROUTINE, DODES1,
C  TO INTEGRATE A SET OF INTEGRALS.
C
C  INPUT
C
C    N      - THE NUMBER OF INTEGRALS
C    F      - CALL F(X,ANS,N,FVAL) SHOULD RETURN FVAL(I)=F(X)(I),
C             FOR I=1,...,N.
C             F SHOULD BE DECLARED EXTERNAL IN THE SUBPROGRAM
C             CALLING DODEQ.
C    A      - THE LOWER LIMIT FOR THE INTEGRAL
C    B      - THE UPPER LIMIT
C    EPS    - ERROR TOLERATED IN THE ANSWERS
C
C  OUTPUT
C
C    ANS    - THE VECTOR OF VALUES FOR THE INTEGRALS
C
C
C  SCRATCH SPACE (USED IN DODES1)
C
C     LET Z REPRESENT THE RATIO OF THE SPACE USED
C     BY DOUBLE-PRECISION LOCATIONS TO THAT USED FOR REALS.
C
C     THEN THE SPACE USED, IN TERMS OF REAL LOCATIONS, IS
C
C    (32 + 12N)*Z + 101 + MAX(2NZ + S(F), 11N + 10Z + 10)
C
C     WHERE S(F) IS THE STORAGE USED IN THE USERS F PROGRAM.
C
C
C  ERROR STATES
C
C    1 - N.LT.1.
C    2 - EPS IS ZERO OR NEGATIVE
C    3 - DODES1 ERROR 7,INTEGRATION TROUBLE (RECOVERABLE)
C
      DOUBLE PRECISION ANS(N),A,B,DT,EPS
      REAL ERRPAR(2)
      EXTERNAL F, DODESQ, DODESH
C
C/6S
C     IF (N .LT. 1) CALL SETERR(16HDODEQ - N .LT. 1,16,1,2)
C     IF (EPS .LE. 0.0D0)
C    1    CALL SETERR(31HDODEQ - EPS IS ZERO OR NEGATIVE,31,2,2)
C/7S
      IF (N .LT. 1) CALL SETERR('DODEQ - N .LT. 1',16,1,2)
      IF (EPS .LE. 0.0D0)
     1    CALL SETERR('DODEQ - EPS IS ZERO OR NEGATIVE',31,2,2)
C/
C
C  SET THE INITIAL VALUES OF THE INTEGRALS TO ZERO
C
      DO 10 K=1,N
 10     ANS(K) = 0.0D0
C
C  IF A EQUALS B, RETURN
C
      IF(A .EQ. B) RETURN
C
C  PUT IN AN ESTIMATE FOR THE INITIAL DT
C
      DT = SIGN( AMAX1( R1MACH(1),ABS(SNGL(EPS*(B-A)))), SNGL(B-A))
C
      ERRPAR(2) = EPS
      CALL ENTSRC(IRSAVE, 1)
C
      CALL DODES1(F,ANS,N,A,B,DT,DODESQ,ERRPAR,DODESH,.FALSE.,.FALSE.)
C
      IF (NERROR(NERR) .NE. 7) GO TO 20
      CALL ERROFF
C/6S
C     CALL SETERR (40H DODEQ - INTEGRATION CANNOT BE PERFORMED,40,3,1)
C/7S
      CALL SETERR (' DODEQ - INTEGRATION CANNOT BE PERFORMED',40,3,1)
C/
C
 20   CALL RETSRC(IRSAVE)
C
      RETURN
      END