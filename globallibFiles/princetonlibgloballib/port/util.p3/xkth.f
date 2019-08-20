      REAL FUNCTION XKTH (N, K, X)
C
C  XKTH FINDS THE KTH SMALLEST ELEMENT IN A SET S OF N NUMBERS.
C
C ERROR SITUATIONS
C        ERROR 1 - N .LE. 0     (FATAL)
C        ERROR 2 - K .GT. N     (FATAL)
C        ERROR 3 - K .LE. 0     (FATAL)
C
C STACK STORAGE
C        N LOCATIONS
C
C  PHYLLIS FOX           AUGUST 10, 1982
C
C  THE ALGORITHM USES THE FIRST STAGE OF QUICKSORT - SEE, FOR EXAMPLE
C  KNUTH VOL. 3, PAGES 114-116.
C
C  AFTER THE FIRST CYCLE OF THE ALGORITHM, THE (ORIGINAL) FIRST ELEMENT
C  IS POSITIONED AT ITS FINAL RESTING PLACE, WITH ALL SMALLER ITEMS
C  BEFORE IT AND ALL LARGER AFTERWARDS.
C  IF THIS RESTING PLACE IS THE KTH, THEN ONE IS DONE.
C  OTHERWISE THE KTH IS ONE OF THE OTHER SEGMENTS AND THE PROCESS
C  ITERATES.
C
C  THE STACK IS USED IN SUCH A WAY THAT THE SEGMENT BEING WORKED AT
C  IS ALWAYS IN THE BEGINNING OF THE CURRENT STACK ALLOCATION.
C
C  UNDER THE CONSTRAINT OF USING FORTRAN 66, THIS METHOD TURNED OUT
C  TO BE FASTER THAN DIVIDE-AND-CONQUER, AND (OF COURSE) FASTER
C  THAN SORTING.
C
      COMMON /CSTAK/DSTAK(500)
      INTEGER IPNT, IBASE, IFIRST, I, J, JJ, K, KK, N
      INTEGER ISTKGT
      REAL RSTAK(1000), X(1), TEMP
      DOUBLE PRECISION DSTAK
C
      EQUIVALENCE (DSTAK(1),RSTAK(1))
C
C/6S
C     IF (N .LE. 0) CALL SETERR(
C    1    17H  XKTH - N .LE. 0, 17, 1, 2)
C/7S
      IF (N .LE. 0) CALL SETERR(
     1    '  XKTH - N .LE. 0', 17, 1, 2)
C/
C
C/6S
C     IF (K .GT. N) CALL SETERR(
C    1    17H  XKTH - K .GT. N, 17, 2, 2)
C/7S
      IF (K .GT. N) CALL SETERR(
     1    '  XKTH - K .GT. N', 17, 2, 2)
C/
C
C/6S
C     IF (K .LE. 0) CALL SETERR(
C    1    17H  XKTH - K .LE. 0, 17, 3, 2)
C/7S
      IF (K .LE. 0) CALL SETERR(
     1    '  XKTH - K .LE. 0', 17, 3, 2)
C/
C
C  IF THERE IS ONLY ONE ELEMENT IN S, IT IS THE ANSWER.
C
      IF (N .NE. 1) GO TO 1
      XKTH = X(1)
      RETURN
C
C  NOW SET UP A LOCATION OF LENGTH N IN THE STACK -
C
  1   IPNT = ISTKGT(N, 3)
      IBASE = IPNT - 1
C
C  MOVE THE ARRAY X TO THE STACK.
C
      CALL MOVEFR(N, X(1), RSTAK(IPNT))
C
C  SET KK TO K AND JJ TO N.
C
      KK = K + IBASE
      JJ = N + IBASE
C
C  SET UP THE SEARCH IN THE J-DECREASING DIRECTION.
C
 10   J = JJ
      I = 1 + IBASE
C
 20   IF (RSTAK(I) .GT. RSTAK(J)) GO TO 40
C
C  OTHERWISE KEEP DECREASING J.
C
 30   J = J - 1
      IF (I .LE. J) GO TO 20
C
      GO TO 60
C
C  COMES HERE WHEN, IN J-DECREASING SEARCH, FINDS INTERCHANGE.
C
 40   TEMP = RSTAK(J)
      RSTAK(J) = RSTAK(I)
      RSTAK(I) = TEMP
C
C  NOW DO I-INCREASING STAGE
C
 50   I = I + 1
      IF (I .GE. J) GO TO 60
      IF (RSTAK(I) .LE. RSTAK(J)) GO TO 50
C
C  OTHERWISE, INTERCHANGE, AND GO BECK TO INCREASING J.
C
      TEMP = RSTAK(J)
      RSTAK(J) = RSTAK(I)
      RSTAK(I) = TEMP
C
      GO TO 30
C  COMES HERE WHEN I=J.
C
C  SEE IF THE KTH SMALLEST IS FOUND, OR IS IN THE FIRST OR
C  SECOND SEGMENT.
C
 60   IF (I .EQ. KK) GO TO 80
      IF (I .GT. KK) GO TO 70
C
C  HERE I IS LESS THAN KK SO KTH IS IN SECOND SEGMENT.
C
C  MOVE IT INTO THE STACK AND SET NEW LENGTH.
C
      IFIRST = I + 1
      CALL MOVEFR (JJ-I, RSTAK(IFIRST), RSTAK(IPNT))
      JJ = JJ - I + IBASE
C
C  REDUCE THE KTH COUNTER BY THE NUMBER OF ITEMS IN THE
C  FIRST SEGMENT.
C
      KK = KK - I + IBASE
      GO TO 10
C
C  HERE KTH IS IN FIRST SEGMENT.
C
 70   JJ = I - 1
      GO TO 10
C
C  HERE KTH IS FOUND
C
 80   XKTH = RSTAK(I)
      CALL ISTKRL(1)
      RETURN
      END