      SUBROUTINE  M6NHB(N, D, X, B, CALCF, CALCGH, IV, LIV, LV, V)
C
C  ***  MINIMIZE GENERAL SIMPLY BOUNDED OBJECTIVE FUNCTION USING   ***
C  ***  (ANALYTIC) GRADIENT AND HESSIAN PROVIDED BY THE CALLER.    ***
C
      INTEGER LIV, LV, N
      INTEGER IV(LIV)
      REAL B(2,N), D(N), X(N), V(LV)
C     DIMENSION V(78 + N*(N+12))
      EXTERNAL CALCF, CALCGH
C
C------------------------------  DISCUSSION  ---------------------------
C
C        THIS ROUTINE IS LIKE   MNGB, EXCEPT THAT THE SUBROUTINE PARA-
C     METER CALCG OF   MNGB (WHICH COMPUTES THE GRADIENT OF THE OBJEC-
C     TIVE FUNCTION) IS REPLACED BY THE SUBROUTINE PARAMETER CALCGH,
C     WHICH COMPUTES BOTH THE GRADIENT AND (LOWER TRIANGLE OF THE)
C----------------------------  DECLARATIONS  ---------------------------
C
      EXTERNAL IVSET,  RMNHB
C
C IVSET.... PROVIDES DEFAULT INPUT VALUES FOR IV AND V.
C  RMNHB... REVERSE-COMMUNICATION ROUTINE THAT DOES   MNHB ALGORITHM.
C
      INTEGER G1, H1, IV1, LH, NF
      REAL F
C
C  ***  SUBSCRIPTS FOR IV   ***
C
      INTEGER G, H, NEXTV, NFCALL, NFGCAL, TOOBIG, VNEED
C
C/6
C     DATA NEXTV/47/, NFCALL/6/, NFGCAL/7/, G/28/, H/56/, TOOBIG/2/,
C    1     VNEED/4/
C/7
      PARAMETER (NEXTV=47, NFCALL=6, NFGCAL=7, G=28, H=56, TOOBIG=2,
     1           VNEED=4)
C/
C
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C
      LH = N * (N + 1) / 2
      IF (IV(1) .EQ. 0) CALL IVSET(2, IV, LIV, LV, V)
      IV1 = IV(1)
      IF (IV1 .EQ. 14) GO TO 10
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 10
      IF (IV1 .EQ. 12) IV(1) = 13
      IF (IV(1) .EQ. 13) IV(VNEED) = IV(VNEED) + N*(N+3)/2
      CALL  RMNHB(B, D, F, V, V, IV, LH, LIV, LV, N, V, X)
      IF (IV(1) .NE. 14) GO TO 50
C
C  ***  STORAGE ALLOCATION
C
      IV(G) = IV(NEXTV)
      IV(H) = IV(G) + N
      IV(NEXTV) = IV(H) + N*(N+1)/2
      IF (IV1 .EQ. 13) GO TO 50
C
 10   G1 = IV(G)
      H1 = IV(H)
C
 20   CALL  RMNHB(B, D, F, V(G1), V(H1), IV, LH, LIV, LV, N, V, X)
      IF (IV(1) - 2) 30, 40, 50
C
 30   NF = IV(NFCALL)
      CALL CALCF(N, X, NF, F)
      IF (NF .LE. 0) IV(TOOBIG) = 1
      GO TO 20
C
 40   NF = IV(NFGCAL)
      CALL CALCGH(N, X, NF, V(G1), V(H1))
      IF (NF .LE. 0) IV(TOOBIG) = 1
      GO TO 20
C
 50   RETURN
C  ***  LAST CARD OF  M6NHB FOLLOWS  ***
      END
