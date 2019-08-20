        SUBROUTINE  M4DP
     *     (K,EK, V,L, HEAD,LAST,NEXT, MARK)
        INTEGER  EK,  V(1), L(1),  HEAD(1), LAST(1), NEXT(1),  MARK(1),
     *     TAG, FREE, LI,VI,LVI,EVI, S,LS,ES, ILP,ILPMAX
C
C  M4DP -- PURGE INACTIVE ELEMENTS AND DO MASS ELIMINATION
C
C----INITIALIZE TAG AND LIST OF PROTOTYPE VERTICES
        TAG = MARK(EK)
        LIST = 0
C
C----FOR EACH VERTEX VI IN EK
        LI = EK
        ILPMAX = LAST(EK)
        IF (ILPMAX.LE.0)  GO TO 12
        DO 11 ILP=1,ILPMAX
          I = LI
          LI = L(I)
          VI = V(LI)
C
C------REMOVE VI FROM DEGREE LIST
          LIVI=LAST(VI)
          IF (LIVI.EQ.0)  GO TO 3
            IF (LIVI.GT.0)  GO TO 1
              MLIVI= -LIVI
              HEAD(MLIVI) = NEXT(VI)
              GO TO 2
   1          NEXT(LIVI) = NEXT(VI)
   2        NIVI=NEXT(VI)
            IF (NIVI.GT.0)  LAST(NIVI) = LAST(VI)
C
C------REMOVE INACTIVE ITEMS FROM ELEMENT LIST OF VI
   3      LS = VI
   4      S = LS
          LS = L(S)
          IF (LS.EQ.0)  GO TO 6
            ES = V(LS)
            IF (MARK(ES).LT.TAG)  GO TO 5
              FREE = LS
              L(S) = L(LS)
              LS = S
   5        GO TO 4
C
C------IF VI IS INTERIOR VERTEX, THEN REMOVE FROM LIST AND ELIMINATE
   6      LVI = L(VI)
          IF (LVI.NE.0)  GO TO 7
            L(I) = L(LI)
            LI = I
C
            K = K+1
            NEXT(VI) = -K
            LAST(EK) = LAST(EK) - 1
            GO TO 11
C
C------ELSE ...
C--------CLASSIFY VERTEX VI-
   7        IF (L(LVI).NE.0)  GO TO 9
              EVI = V(LVI)
              IF (NEXT(EVI).GE.0)  GO TO 9
                IF (MARK(EVI).LT.0)  GO TO 8
C
C----------IF VI IS PROTOTYPE VERTEX, THEN MARK AS SUCH, INITIALIZE
C----------OVERLAP COUNT FOR CORRESPONDING ELEMENT, AND MOVE VI TO LIST
C----------OF PROTOTYPE VERTICES
                  LAST(VI) = EVI
                  MARK(EVI) = -1
                  L(I) = L(LI)
                  L(LI) = LIST
                  LIST = LI
                  LI = I
                  GO TO 10
C
C----------ELSE IF VI IS DUPLICATE VERTEX, THEN MARK AS SUCH AND ADJUST
C----------OVERLAP COUNT FOR CORRESPONDING ELEMENT
   8              LAST(VI) = 0
                  MARK(EVI) = MARK(EVI) - 1
                  GO TO 10
C
C----------ELSE MARK VI TO COMPUTE DEGREE
   9              LAST(VI) = -EK
C
C--------INSERT EK IN ELEMENT LIST OF VI
  10        V(FREE) = EK
            L(FREE) = L(VI)
            L(VI) = FREE
  11      CONTINUE
C
C----APPEND LIST OF PROTOTYPE VERTICES TO END OF BOUNDARY LIST
  12    L(LI) = LIST
C
        RETURN
        END
