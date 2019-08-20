        INTEGER FUNCTION ISAMAX(N,X,INCX)
        INTEGER N,INCX
        REAL X(INCX,1),SMAX
C THIS FUNCTION RETURNS THE INDEX OF THE COMPONENT
C OF X HAVING MAXIMUM MAGNITUDE. ONLY EVERY
C INCXTH COMPONENT OF X IS CONSIDERED
C
        ISAMAX=0
        IF(N.EQ.0) RETURN
C/6S
C       IF(N.LT.0) CALL SETERR(13HISAMAX-N.LT.0,13,1,2)
C       IF(INCX.LT.1)CALL SETERR(16HISAMAX-INCX.LT.1,16,2,2)
C/7S
        IF(N.LT.0) CALL SETERR('ISAMAX-N.LT.0',13,1,2)
        IF(INCX.LT.1)CALL SETERR('ISAMAX-INCX.LT.1',16,2,2)
C/
        SMAX=0.0
        ISAMAX=1
        DO 10 I=1,N
        IF(SMAX.GE.ABS(X(1,I))) GO TO 10
           SMAX=ABS(X(1,I))
           ISAMAX=I
 10     CONTINUE
        RETURN
        END
