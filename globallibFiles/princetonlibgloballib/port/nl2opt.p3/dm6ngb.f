      SUBROUTINE DM6NGB(N, D, X, B, CALCF, CALCG, IV, LIV, LV, V)
C
C  ***  MINIMIZE GENERAL SIMPLY BOUNDED OBJECTIVE FUNCTION USING  ***
C  ***  ANALYTIC GRADIENT AND HESSIAN APPROX. FROM SECANT UPDATE  ***
C
      INTEGER N, LIV, LV
      INTEGER IV(LIV)
      DOUBLE PRECISION D(N), X(N), B(2,N), V(LV)
C     DIMENSION V(71 + N*(N+15)/2), UIPARM(*), URPARM(*)
      EXTERNAL CALCF, CALCG
C
C  ***  DISCUSSION  ***
C
C        THIS ROUTINE IS LIKE  DMNG, EXCEPT FOR THE EXTRA PARAMETER B,
C     AN ARRAY OF LOWER AND UPPER BOUNDS ON X...  DMNGB ENFORCES THE
C     CONSTRAINTS THAT  B(1,I) .LE. X(I) .LE. B(2,I), I = 1(1)N.
C     (INSTEAD OF CALLING  DRMNG,  DMNGB CALLS  DRMNGB.)
C.
C
C----------------------------  DECLARATIONS  ---------------------------
C
      EXTERNAL DIVSET,  DRMNGB
C
C DIVSET.... SUPPLIES DEFAULT IV AND V INPUT COMPONENTS.
C  DRMNGB... REVERSE-COMMUNICATION ROUTINE THAT CARRIES OUT  DMNG ALGO-
C             RITHM.
C
      INTEGER G1, IV1, NF
      DOUBLE PRECISION F
C
C  ***  SUBSCRIPTS FOR IV   ***
C
      INTEGER NEXTV, NFCALL, NFGCAL, G, TOOBIG, VNEED
C
C/6
C     DATA NEXTV/47/, NFCALL/6/, NFGCAL/7/, G/28/, TOOBIG/2/, VNEED/4/
C/7
      PARAMETER (NEXTV=47, NFCALL=6, NFGCAL=7, G=28, TOOBIG=2, VNEED=4)
C/
C
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C
      IF (IV(1) .EQ. 0) CALL DIVSET(2, IV, LIV, LV, V)
      IV1 = IV(1)
      IF (IV1 .EQ. 14) GO TO 10
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 10
      IF (IV1 .EQ. 12) IV(1) = 13
      IF (IV(1) .EQ. 13) IV(VNEED) = IV(VNEED) + N
      CALL  DRMNGB(B, D, F, V, IV, LIV, LV, N, V, X)
      IF (IV(1) .NE. 14) GO TO 50
C
C  ***  STORAGE ALLOCATION
C
      IV(G) = IV(NEXTV)
      IV(NEXTV) = IV(G) + N
      IF (IV1 .EQ. 13) GO TO 50
C
 10   G1 = IV(G)
C
 20   CALL  DRMNGB(B, D, F, V(G1), IV, LIV, LV, N, V, X)
      IF (IV(1) - 2) 30, 40, 50
C
 30   NF = IV(NFCALL)
      CALL CALCF(N, X, NF, F)
      IF (NF .LE. 0) IV(TOOBIG) = 1
      GO TO 20
C
 40   NF = IV(NFGCAL)
      CALL CALCG(N, X, NF, V(G1))
      IF (NF .LE. 0) IV(TOOBIG) = 1
      GO TO 20
C
 50   RETURN
C  ***  LAST CARD OF DM6NGB FOLLOWS  ***
      END
