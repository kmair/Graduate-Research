        SUBROUTINE  M4DM
     *     (VK,TAG, V,L, HEAD,LAST,NEXT, MARK)
        INTEGER  VK, TAG,  V(1), L(1),  HEAD(1), LAST(1), NEXT(1),
     *     MARK(1),  S,LS,VS,ES, B,LB,VB, BLP,BLPMAX
        EQUIVALENCE  (VS, ES)
C
C  M4DM -- FORM ELEMENT FROM UNELIMINATED NEIGHBORS OF VK
C
C----TAG VK AND INITIALIZE LIST OF UNELIMINATED NEIGHBORS
        MARK(VK) = TAG
        LIST = 0
C
C----FOR EACH VERTEX/ELEMENT VS/ES IN ELEMENT LIST OF VK
        LS = VK
   1    S = LS
        LS = L(S)
        IF (LS.EQ.0)  GO TO 5
          VS = V(LS)
          IF (NEXT(VS).LT.0)  GO TO 2
C
C------IF VS IS UNELIMINATED VERTEX, THEN TAG AND ADD TO LIST OF
C------UNELIMINATED NEIGHBORS
            MARK(VS) = TAG
            L(S) = L(LS)
            L(LS) = LIST
            LIST = LS
            LS = S
            GO TO 4
C
C------IF ES IS ACTIVE ELEMENT, THEN ...
C--------FOR EACH VERTEX VB IN BOUNDARY LIST OF ELEMENT ES
   2        LB = ES
            BLPMAX = LAST(ES)
            DO 3 BLP=1,BLPMAX
              B = LB
              LB = L(B)
              VB = V(LB)
C
C----------IF VB IS UNTAGGED VERTEX, THEN TAG AND ADD TO LIST OF
C----------UNELIMINATED NEIGHBORS
              IF (MARK(VB).GE.TAG)  GO TO 3
                MARK(VB) = TAG
                L(B) = L(LB)
                L(LB) = LIST
                LIST = LB
                LB = B
   3          CONTINUE
C
C--------MARK ES INACTIVE
            MARK(ES) = TAG
C
   4      GO TO 1
C
C----ATTACH LIST OF UNELIMINATED NEIGHBORS TO EK
   5    L(VK) = LIST
C
        RETURN
        END
