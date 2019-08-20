      SUBROUTINE DCSPFI(X,Y,N,B,YP,YPP)
       DOUBLE PRECISION X(N),Y(N),B(6),YP(N),YPP(N)
C
C INPUTS
C N DATA PAIRS (X,Y)
C B IS BOUNDARY CONDITION MATRIX
C
C      B(1)*YP(1) + B(2)*YPP(1) = B(3) AT X(1)
C      B(4)*YP(N) + B(5)*YPP(N) = B(6) AT X(N)
C
C WHERE
C YP AND YPP ARE THE DERIVATIVES OF THE FITTED CUBIC SPLINE
C XX IS ARRAY OF VALUES AT WHICH TO INTERPOLATE
C NN IS NUMBER OF XX-VALUES
C
C OUTPUT
C CUBIC SPLINE FIT TO Y(X).
C PARAMETER VECTORS, YP AND YPP AT DATA POINTS, ARE RETURNED.
C
       COMMON/CSTAK/D
       DOUBLE PRECISION D(500)
C
C CHECK FOR ERRORS IN INPUT
C
C/6S
C      IF (N.LT.2)   CALL SETERR(
C    1   34HDCSPFI - MUST HAVE MORE THAN ONE X,34,1,2)
C/7S
       IF (N.LT.2)   CALL SETERR(
     1   'DCSPFI - MUST HAVE MORE THAN ONE X',34,1,2)
C/
C
       N1=N-1
       DO 10 J=1,N1
C/6S
C  10  IF (X(J).GE.X(J+1))   CALL SETERR(
C    1    44HDCSPFI - X ARRAY MUST BE IN INCREASING ORDER,44,2,2)
C/7S
   10  IF (X(J).GE.X(J+1))   CALL SETERR(
     1    'DCSPFI - X ARRAY MUST BE IN INCREASING ORDER',44,2,2)
C/
C
C/6S
C      IF (DMAX1(DABS(B(1)),DABS(B(2))).LE.0.0)  CALL SETERR(
C    1    42HDCSPFI - B(1) AND B(2) CANNOT BOTH BE ZERO,42,3,2)
C/7S
       IF (DMAX1(DABS(B(1)),DABS(B(2))).LE.0.0)  CALL SETERR(
     1    'DCSPFI - B(1) AND B(2) CANNOT BOTH BE ZERO',42,3,2)
C/
C
C/6S
C      IF (DMAX1(DABS(B(4)),DABS(B(5))).LE.0.0)  CALL SETERR(
C    1   42HDCSPFI - B(4) AND B(5) CANNOT BOTH BE ZERO,42,4,2)
C/7S
       IF (DMAX1(DABS(B(4)),DABS(B(5))).LE.0.0)  CALL SETERR(
     1   'DCSPFI - B(4) AND B(5) CANNOT BOTH BE ZERO',42,4,2)
C/
C
C/6S
C      IF (ISTKQU(4) .LT. 4*N) CALL SETERR(
C    1   47HDCSPFI - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,47,5,2)
C/7S
       IF (ISTKQU(4) .LT. 4*N) CALL SETERR(
     1   'DCSPFI - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',47,5,2)
C/
C
C SET UP THE STACK AND THEN CALL THE ACTUAL FITTING PROCESS
C
       IH=ISTKGT(4*N,4)
       IU=IH+N
       IV=IU+N
       ID=IV+N
C
C TURN RECOVERY ON AND SAVE OLD RECOVERY SWITCH
C
       CALL ENTSRC (IRSAVE, 1)
C
       CALL DC1SPF(X,Y,N,B,YP,YPP,D(IH),D(IU),D(IV),D(ID))
C
       IF (NERROR(NERR) .EQ. 0)  GOTO 30
C
C ERROR 1 IN DC1SPF.
C TURN OFF ERROR STATE AND RESET IT TO
C GIVE A LOCAL ERROR STATE FOR DCSPFI.
C
       CALL ERROFF
C/6S
C      CALL SETERR (38HDCSPFI - SINGULAR MATRIX. CHECK INPUTS,38,6,2)
C/7S
       CALL SETERR ('DCSPFI - SINGULAR MATRIX. CHECK INPUTS',38,6,2)
C/
C
C
   30  CALL RETSRC (IRSAVE)
       CALL ISTKRL(1)
C
       RETURN
       END