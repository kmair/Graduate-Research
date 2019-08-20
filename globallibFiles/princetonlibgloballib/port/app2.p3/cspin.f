      SUBROUTINE CSPIN(X,Y,N,XX,YY,NN)
       REAL X(N),Y(N),B(6),XX(NN),YY(NN),YYP,YYPP
       REAL C3SPFT
C
C INPUTS
C N DATA PAIRS (X,Y)
C XX IS ARRAY OF VALUES AT WHICH TO INTERPOLATE
C NN IS NUMBER OF XX-VALUES
C
C OUTPUT
C CUBIC SPLINE FIT TO Y(X).
C YY(J) IS INTERPOLATED VALUE AT XX(J),J=1,...,NN.
C
       COMMON/CSTAK/D
       DOUBLE PRECISION D(500)
       REAL R(1000)
       EQUIVALENCE (D(1),R(1))
C
C CHECK FOR ERRORS IN INPUT PARAMETERS
C
C/6S
C      IF (NN .LT. 1) CALL SETERR(
C    1    46HCSPIN - MUST INTERPOLATE AT ONE POINT AT LEAST,46,1,2)
C/7S
       IF (NN .LT. 1) CALL SETERR(
     1    'CSPIN - MUST INTERPOLATE AT ONE POINT AT LEAST',46,1,2)
C/
C
C/6S
C      IF (N.LT.4)   CALL SETERR(
C    1   35HCSPIN - MUST HAVE AT LEAST FOUR X S,35,2,2)
C/7S
       IF (N.LT.4)   CALL SETERR(
     1   'CSPIN - MUST HAVE AT LEAST FOUR X S',35,2,2)
C/
C
       N1 = N-1
       DO 10 J=1,N1
C/6S
C  10  IF (X(J).GE.X(J+1))   CALL SETERR(
C    1    43HCSPIN - X ARRAY MUST BE IN INCREASING ORDER,43,3,2)
C/7S
   10  IF (X(J).GE.X(J+1))   CALL SETERR(
     1    'CSPIN - X ARRAY MUST BE IN INCREASING ORDER',43,3,2)
C/
C
       DO 20 K=1,NN
C/6S
C  20  IF (XX(K) .LT.  X(1) .OR. XX(K) .GT. X(N)) CALL SETERR(
C    1    34HCSPIN -  POINT NOT INSIDE INTERVAL,34,4,2)
C/7S
   20  IF (XX(K) .LT.  X(1) .OR. XX(K) .GT. X(N)) CALL SETERR(
     1    'CSPIN -  POINT NOT INSIDE INTERVAL',34,4,2)
C/
C
C BOUNDARY CONDITIONS - APPROXIMATE SECOND DERIVATIVES
C
       B(1)=0.
       B(2)=1.
       B(3)=C3SPFT(X(1),Y(1),X(1))
       B(4)=0.
       B(5)=1.
       B(6)=C3SPFT(X(N-3),Y(N-3),X(N))
C
C SPACE FOR PARAMETERS OF SPLINE
C
C/6S
C      IF (ISTKQU(3) .LT. 2*N) CALL SETERR(
C    1   46HCSPIN - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,46,5,2)
C/7S
       IF (ISTKQU(3) .LT. 2*N) CALL SETERR(
     1   'CSPIN - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',46,5,2)
C/
       IYP=ISTKGT(2*N,3)
       IYPP=IYP+N
C
C CHECK IF HAVE ENOUGH SPACE FOR CSPFI
C
C/6S
C      IF (ISTKQU(3) .LT. 4*N) CALL SETERR(
C    1   46HCSPIN - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,46,5,2)
C/7S
       IF (ISTKQU(3) .LT. 4*N) CALL SETERR(
     1   'CSPIN - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',46,5,2)
C/
C
C FIT THE SPLINE
C
       CALL CSPFI(X,Y,N,B,R(IYP),R(IYPP))
C
C DO THE INTERPOLATION
C
       DO 50 J=1,NN
C
   50  CALL C2SPFT(X,Y,R(IYP),R(IYPP),N,XX(J),YY(J),YYP,YYPP)
C
       CALL ISTKRL(1)
       RETURN
       END
