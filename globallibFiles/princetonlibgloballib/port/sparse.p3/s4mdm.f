        SUBROUTINE  S4MDM
     *     (N, JCOL, MAX, V,L, HEAD,LAST,NEXT, MARK, FLAG,IA,JA,MA)
        INTEGER     V(1), L(1),  HEAD(1), LAST(1), NEXT(1),
     *     MARK(1),  FLAG,  TAG, DMIN, VK,EK, IA(1),JA(1)
         EXTERNAL JCOL
        EQUIVALENCE  (VK,EK)
C
C
C    VARIABLES-
C
C    ---------+-----------------------------+---------------------------
C    HEAD(D)  * VJ GE VJ HEAD OF D-LIST D
C             *  0 GE NO VERTEX IN D-LIST D
C
C
C             *                  VI UNELIMINATED VERTEX
C             *          VI IN EK           *       VI NOT IN EK
C    ---------+-----------------------------+---------------------------
C    NEXT(VI) * UNDEFINED BUT NONNEGATIVE   * VJ GE VJ NEXT IN D-LIST
C             *                             *  0 GE VI TAIL OF D-LIST
C    ---------+-----------------------------+---------------------------
C    LAST(VI) * (NOT SET UNTIL M4DP)        * -D GE VI HEAD OF D-LIST D
C             *-VK GE COMPUTE DEGREE        * VJ GE VJ LAST IN D-LIST
C             * EJ GE VI PROTOTYPE OF EJ    *  0 GE VI NOT IN ANY D-LIST
C             *  0 GE DO NOT COMPUTE DEGREE *
C    ---------+-----------------------------+---------------------------
C    MARK(VI) * MARK(VK)                    * NONNEG TAG .LT. MARK(VK)
C
C
C             *                   VI ELIMINATED VERTEX
C             *      EI ACTIVE ELEMENT      *           OTHERWISE
C    ---------+-----------------------------+---------------------------
C    NEXT(VI) * -J GE VI WAS J-TH VERTEX    * -J GE VI WAS J-TH VERTEX
C             *       TO BE ELIMINATED      *       TO BE ELIMINATED
C    ---------+-----------------------------+---------------------------
C    LAST(VI) *  M GE SIZE OF EI = M        * UNDEFINED
C    ---------+-----------------------------+---------------------------
C    MARK(VI) * -M GE OVERLAP COUNT OF EI   * UNDEFINED
C             *       WITH EK = M           *
C             * OTHERWISE NONNEGATIVE TAG   *
C             *    .LT. MARK(VK)            *
C    ---------+-----------------------------+---------------------------
C
C
C----INITIALIZATION
        TAG = 0
        IF (MA.EQ.0)CALL  M4DIF
     *     (N,JCOL,IA,LAST,MAX,V,L,HEAD,LAST,NEXT,MARK,TAG,FLAG,IBIG,MA)
        IF (MA.EQ.1)CALL  M4DIF
     *     (N,JCOL,IA,JA,MAX,V,L,HEAD,LAST,NEXT,MARK,TAG,FLAG,IBIG,MA)
        IF (FLAG.NE.0)  RETURN
C
        K = 0
        DMIN = 1
C
C----WHILE  K .LT. IBIG  DO
   1    IF (K.GE.IBIG)  GO TO 4
C
C------SEARCH FOR VERTEX OF MINIMUM DEGREE
   2      IF (HEAD(DMIN).GT.0)  GO TO 3
            DMIN = DMIN + 1
            GO TO 2
C
C------REMOVE VERTEX VK OF MINIMUM DEGREE FROM DEGREE LIST
   3      VK = HEAD(DMIN)
          HEAD(DMIN) = NEXT(VK)
          NVK=HEAD(DMIN)
          IF (NVK.GT.0)  LAST(NVK) = -DMIN
C
C------NUMBER VERTEX VK AND ADJUST TAG
          K = K+1
          NEXT(VK) = -K
          LAST(EK) = DMIN - 1
          TAG = TAG + LAST(EK)
C
C------FORM ELEMENT EK FROM UNELIMINATED NEIGHBORS OF VK
          CALL  M4DM
     *       (VK,TAG, V,L, HEAD,LAST,NEXT, MARK)
C
C------PURGE INACTIVE ELEMENTS AND DO MASS ELIMINATION
          CALL  M4DP
     *       (K,EK, V,L, HEAD,LAST,NEXT, MARK)
C
C------UPDATE DEGREES OF UNELIMINATED VERTICES IN EK
          CALL  M4DU
     *       (EK,DMIN, V,L, HEAD,LAST,NEXT, MARK)
C
          GO TO 1
C
C----GENERATE INVERSE PERMUTATION FROM PERMUTATION
   4    DO 5 K=1,N
          NEXT(K) = -NEXT(K)
          NK=NEXT(K)
   5      LAST(NK) = K
C
        RETURN
        END
