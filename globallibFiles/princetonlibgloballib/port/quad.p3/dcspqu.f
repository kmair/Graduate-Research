      SUBROUTINE DCSPQU(X,Y,N,XLOW,XHIGH,ANS)
       DOUBLE PRECISION X(N),Y(N),B(6),XLOW,XHIGH,ANS,XL,XH
       DOUBLE PRECISION DC3SPF
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
C
C CHECK FOR ERRORS IN INPUT PARAMETERS
C
C/6S
C      IF (N.LT.4)   CALL SETERR(
C    1   36HDCSPQU - MUST HAVE AT LEAST FOUR X S,36,1,2)
C/7S
       IF (N.LT.4)   CALL SETERR(
     1   'DCSPQU - MUST HAVE AT LEAST FOUR X S',36,1,2)
C/
C
       N1 = N-1
       DO 10 J=1,N1
C/6S
C  10  IF (X(J).GE.X(J+1))   CALL SETERR(
C    1    44HDCSPQU - X ARRAY MUST BE IN INCREASING ORDER,44,2,2)
C/7S
   10  IF (X(J).GE.X(J+1))   CALL SETERR(
     1    'DCSPQU - X ARRAY MUST BE IN INCREASING ORDER',44,2,2)
C/
C
       XL=DMIN1(XLOW,XHIGH)
       XH=DMAX1(XLOW,XHIGH)
C/6S
C      IF (XL .LT.  X(1) .OR. XH .GT. X(N)) CALL SETERR(
C    1    36HDCSPQU -  LIMITS NOT INSIDE INTERVAL,36,3,2)
C/7S
       IF (XL .LT.  X(1) .OR. XH .GT. X(N)) CALL SETERR(
     1    'DCSPQU -  LIMITS NOT INSIDE INTERVAL',36,3,2)
C/
C
C BOUNDARY CONDITIONS - APPROXIMATE SECOND DERIVATIVES
C
       B(1)=0.
       B(2)=1.
       B(3)=DC3SPF(X(1),Y(1),X(1))
       B(4)=0.
       B(5)=1.
       B(6)=DC3SPF(X(N-3),Y(N-3),X(N))
C
C SPACE FOR PARAMETERS OF SPLINE
C
C/6S
C      IF (ISTKQU(4) .LT. 2*N) CALL SETERR(
C    1   47HDCSPQU - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,47,4,2)
C/7S
       IF (ISTKQU(4) .LT. 2*N) CALL SETERR(
     1   'DCSPQU - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',47,4,2)
C/
       IYP=ISTKGT(2*N,4)
       IYPP=IYP+N
C
C CHECK IF HAVE ENOUGH SPACE FOR DCSPFI
C
C/6S
C      IF (ISTKQU(4) .LT. 4*N) CALL SETERR(
C    1   47HDCSPQU - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,47,4,2)
C/7S
       IF (ISTKQU(4) .LT. 4*N) CALL SETERR(
     1   'DCSPQU - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',47,4,2)
C/
C
C FIT THE SPLINE
C
       CALL DCSPFI(X,Y,N,B,D(IYP),D(IYPP))
C
C
C DO THE INTEGRATION
C
       CALL DC2SPQ(X,Y,D(IYP),D(IYPP),N,XL,XH,ANS)
       IF (XLOW .GT. XHIGH) ANS = -ANS
C
       CALL ISTKRL(1)
       RETURN
       END
