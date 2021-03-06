      INTEGER FUNCTION IUMD(A,B,NAB)
C
C  TO SET UP THE MESH X(1),...,X(NAB) ON (A,B) WHICH INCLUDES THE
C  POINTS A + (I-1)*((B-A)/(NAB-1)), I=1,...,NAB.
C
C  INPUT -
C
C    A   - LEFT HAND END POINT.
C    B   - RIGHT HAND END POINT.
C    NAB - NUMBER OF POINTS TO BE USED ON (A,B).
C
C  OUTPUT -
C
C    IUMD - THE POINTER TO THE MESH OF LENGTH NAB IN THE STACK.
C
C  SCRATCH SPACE ALLOCATED - NAB REAL LOCATIONS
C                            ARE LEFT ON THE STACK.
C
C  ERROR STATES -
C
C    1 - A=B.
C    2 - NAB.LT.2.
C
      REAL A,B
C
C ... CHECK THE INPUT.
C
C/6S
C     IF (A.EQ.B) CALL SETERR(11H IUMD - A=B,11,1,2)
C     IF (NAB.LT.2) CALL SETERR(16H IUMD - NAB.LT.2,16,2,2)
C/7S
      IF (A.EQ.B) CALL SETERR(' IUMD - A=B',11,1,2)
      IF (NAB.LT.2) CALL SETERR(' IUMD - NAB.LT.2',16,2,2)
C/
C
      IUMD=IUMB(A,B,NAB,1,NX)
C
      RETURN
C
      END
