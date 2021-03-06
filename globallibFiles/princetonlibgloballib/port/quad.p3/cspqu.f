      SUBROUTINE CSPQU(X,Y,N,XLOW,XHIGH,ANS)
       REAL X(N),Y(N),B(6),XLOW,XHIGH,ANS,XL,XH
       REAL C3SPFT
C
C INPUTS
C N DATA PAIRS (X,Y)
C XLOW AND XHIGH ARE LIMITS OF INTEGRATION
C
C OUTPUT
C CUBIC SPLINE FIT TO Y(X).
C ANS IS INTEGRAL OF CUBIC SPLINE FROM XLOW TO XHIGH.
C
       COMMON/CSTAK/D
       DOUBLE PRECISION D(500)
       REAL R(1000)
       EQUIVALENCE (D(1),R(1))
C
C CHECK FOR ERRORS IN INPUT PARAMETERS
C
C/6S
C      IF (N.LT.4)   CALL SETERR(
C    1   35HCSPQU - MUST HAVE AT LEAST FOUR X S,35,1,2)
C/7S
       IF (N.LT.4)   CALL SETERR(
     1   'CSPQU - MUST HAVE AT LEAST FOUR X S',35,1,2)
C/
C
       N1 = N-1
       DO 10 J=1,N1
C/6S
C  10  IF (X(J).GE.X(J+1))   CALL SETERR(
C    1    43HCSPQU - X ARRAY MUST BE IN INCREASING ORDER,43,2,2)
C/7S
   10  IF (X(J).GE.X(J+1))   CALL SETERR(
     1    'CSPQU - X ARRAY MUST BE IN INCREASING ORDER',43,2,2)
C/
C
       XL=AMIN1(XLOW,XHIGH)
       XH=AMAX1(XLOW,XHIGH)
C/6S
C      IF (XL .LT.  X(1) .OR. XH .GT. X(N)) CALL SETERR(
C    1    35HCSPQU -  LIMITS NOT INSIDE INTERVAL,35,3,2)
C/7S
       IF (XL .LT.  X(1) .OR. XH .GT. X(N)) CALL SETERR(
     1    'CSPQU -  LIMITS NOT INSIDE INTERVAL',35,3,2)
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
C    1   46HCSPQU - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,46,4,2)
C/7S
       IF (ISTKQU(3) .LT. 2*N) CALL SETERR(
     1   'CSPQU - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',46,4,2)
C/
       IYP=ISTKGT(2*N,3)
       IYPP=IYP+N
C
C CHECK IF HAVE ENOUGH SPACE FOR CSPFI
C
C/6S
C      IF (ISTKQU(3) .LT. 4*N) CALL SETERR(
C    1   46HCSPQU - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,46,4,2)
C/7S
       IF (ISTKQU(3) .LT. 4*N) CALL SETERR(
     1   'CSPQU - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',46,4,2)
C/
C
C FIT THE SPLINE
C
       CALL CSPFI(X,Y,N,B,R(IYP),R(IYPP))
C
C
C DO THE INTEGRATION
C
       CALL C2SPQU(X,Y,R(IYP),R(IYPP),N,XL,XH,ANS)
       IF (XLOW .GT. XHIGH) ANS = -ANS
C
       CALL ISTKRL(1)
       RETURN
       END
