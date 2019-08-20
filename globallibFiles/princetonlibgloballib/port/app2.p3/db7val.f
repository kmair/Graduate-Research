        DOUBLE PRECISION FUNCTION DB7VAL(BC, BX, J, M, P)
C
C  *** RETURN BX(1,J) FOR 1 .LE. J .LE. P, BC(1,J-P) FOR J .GT. P,
C  *** -BX(2,-J) FOR -P .LE. J .LE. -1,  -BC(2,-J-P) FOR J .LT. -P.
C
        INTEGER J, M, P
        DOUBLE PRECISION BC(2,M), BX(2,P)
C
        INTEGER I
C
C  ***  BODY
C
        IF (J .LT. 0) GO TO 20
            IF (J .GT. P) GO TO 10
                DB7VAL = BX(1,J)
                GO TO 999
C
 10         I = J - P
            DB7VAL = BC(1,I)
            GO TO 999
C
 20     I = -J
        IF (I .GT. P) GO TO 30
            DB7VAL = -BX(2,I)
            GO TO 999
C
 30     I = I - P
        DB7VAL = -BC(2,I)
C
 999    RETURN
        END
