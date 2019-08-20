      SUBROUTINE PUMD(XB,NXB,NA,X,NX)
C
C  TO SET UP THE GRID X WHICH INCLUDES THE POINTS
C
C      XB(I)+(J-1)*((XB(I+1)-XB(I))/(NA(I)-1))
C
C  FOR I=1,...,NXB-1 AND J=1,...,NA(I).
C
C  INPUT -
C
C    XB  - THE BASIC POINTS FOR DEFINING THE GRID X.
C          XB MUST BE EITHER MONOTONE INCREASING OR MONOTONE DECREASING.
C          ANY MULTIPLICITIES IN XB ARE IGNORED.
C    NXB - THE NUMBER OF POINTS IN XB.
C    NA  - NA(I) GIVES THE NUMBER OF POINTS TO BE USED IN THE GRID
C          X FROM XB(I) TO XB(I+1), FOR I=1,...,NXB-1.
C
C  OUTPUT -
C
C    X  - THE GRID X.
C    NX - THE NUMBER OF MESH POINTS IN X.
C
C           NX = 1 + (SUM(I=1,...,NXB-1,XB(I).NE.XB(I+1))(NA(I)-1).
C
C  SCRATCH SPACE ALLOCATED - NONE.
C
C  ERROR STATES -
C
C    1 - NXB.LT.2.
C    2 - XB IS NOT MONOTONE.
C    3 - NA(I).LT.2.
C
      REAL XB(NXB),X(1)
C     REAL X(NX)
      INTEGER NA(1)
C     INTEGER NA(NXB-1)
C
      REAL DIR
C
C/6S
C     IF (NXB.LT.2) CALL SETERR(16H PUMD - NXB.LT.2,16,1,2)
C/7S
      IF (NXB.LT.2) CALL SETERR(' PUMD - NXB.LT.2',16,1,2)
C/
C
      DIR=XB(NXB)-XB(1)
C/6S
C     IF (DIR.EQ.0.0E0) CALL SETERR
C    1  (26H PUMD - XB IS NOT MONOTONE,26,2,2)
C/7S
      IF (DIR.EQ.0.0E0) CALL SETERR
     1  (' PUMD - XB IS NOT MONOTONE',26,2,2)
C/
C
      NXBM1=NXB-1
      DO 10 I=1,NXBM1
        IF (XB(I).EQ.XB(I+1)) GO TO 10
C/6S
C       IF ((XB(I+1)-XB(I))*(DIR/ABS(DIR)).LT.0.0E0) CALL SETERR
C    1    (26H PUMD - XB IS NOT MONOTONE,26,2,2)
C       IF (NA(I).LT.2) CALL SETERR(18H PUMD - NA(I).LT.2,18,3,2)
C/7S
        IF ((XB(I+1)-XB(I))*(DIR/ABS(DIR)).LT.0.0E0) CALL SETERR
     1    (' PUMD - XB IS NOT MONOTONE',26,2,2)
        IF (NA(I).LT.2) CALL SETERR(' PUMD - NA(I).LT.2',18,3,2)
C/
 10     CONTINUE
C
      CALL PUMB(XB,NXB,NA,1,X,NX)
C
      RETURN
C
      END
