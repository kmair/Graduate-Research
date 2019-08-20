      SUBROUTINE ODEQ(N, F, A, B, EPS, ANS)
C
C  USING THE DIFFERENTIAL-EQUATION ROUTINE, ODES1,
C  TO INTEGRATE A SET OF INTEGRALS.
C
C  INPUT
C
C    N      - THE NUMBER OF INTEGRALS
C    F      - CALL F(X,ANS,N,FVAL) SHOULD RETURN FVAL(I)=F(X)(I),
C             FOR I=1,...,N.
C             F SHOULD BE DECLARED EXTERNAL IN THE SUBPROGRAM
C             CALLING ODEQ.
C    A      - THE LOWER LIMIT FOR THE INTEGRAL
C    B      - THE UPPER LIMIT
C    EPS    - ERROR TOLERATED IN THE ANSWERS
C
C  OUTPUT
C
C    ANS    - THE VECTOR OF VALUES FOR THE INTEGRALS
C
C
C  SCRATCH SPACE (USED IN ODES1)
C
C     THE SPACE USED, IN TERMS OF REAL LOCATIONS, IS
C
C    133 + 12N + MAX(2N + S(F), 11N + 20)
C
C     WHERE S(F) IS THE STORAGE USED IN THE USERS F PROGRAM.
C
C
C  ERROR STATES
C
C    1 - N.LT.1.
C    2 - EPS IS ZERO OR NEGATIVE
C    3 - ODES1 ERROR 7, INTEGRATION TROUBLE (RECOVERABLE)
C
      REAL ANS(N),A,B,DT,EPS
      REAL ERRPAR(2)
      EXTERNAL F, ODESQ, ODESH
C
C/6S
C     IF (N .LT. 1) CALL SETERR(16H ODEQ - N .LT. 1,16,1,2)
C     IF (EPS .LE. 0.0E0)
C    1    CALL SETERR(31H ODEQ - EPS IS ZERO OR NEGATIVE,31,2,2)
C/7S
      IF (N .LT. 1) CALL SETERR(' ODEQ - N .LT. 1',16,1,2)
      IF (EPS .LE. 0.0E0)
     1    CALL SETERR(' ODEQ - EPS IS ZERO OR NEGATIVE',31,2,2)
C/
C
C  SET THE INITIAL VALUES OF THE INTEGRALS TO ZERO
C
      DO 10 K=1,N
 10     ANS(K) = 0.0E0
C
C  IF A EQUALS B, RETURN
C
      IF(A .EQ. B) RETURN
C
C  PUT IN AN ESTIMATE FOR THE INITIAL DT
C
      DT = SIGN( AMAX1( R1MACH(1),EPS*ABS(B-A)), B-A)
C
      ERRPAR(2) = EPS
      CALL ENTSRC(IRSAVE, 1)
C
      CALL ODES1(F,ANS,N,A,B,DT,ODESQ,ERRPAR,ODESH,.FALSE.,.FALSE.)
C
      IF (NERROR(NERR) .NE. 7) GO TO 20
      CALL ERROFF
C/6S
C     CALL SETERR (39H ODEQ - INTEGRATION CANNOT BE PERFORMED,39,3,1)
C/7S
      CALL SETERR (' ODEQ - INTEGRATION CANNOT BE PERFORMED',39,3,1)
C/
C
 20   CALL RETSRC(IRSAVE)
C
      RETURN
      END
