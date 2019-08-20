        SUBROUTINE DSWAP(N,X,INCX,Y,INCY)
C
C THIS SUBROUTINE SWAPS EVERY INCXTH AND INCYTH COMPONENT
C OF X AND Y
       DOUBLE PRECISION X(INCX,1),Y(INCY,1),TEMP
       IF(N.EQ.0) RETURN
C/6S
C      IF(N.LT.0) CALL SETERR(12HDSWAP-N.LT.0,12,1,2)
C      IF(INCX.LT.1) CALL SETERR(15HDSWAP-INCX.LT.1,15,2,2)
C      IF(INCY.LT.1)CALL SETERR(15HDSWAP-INCY.LT.1,15,3,2)
C/7S
       IF(N.LT.0) CALL SETERR('DSWAP-N.LT.0',12,1,2)
       IF(INCX.LT.1) CALL SETERR('DSWAP-INCX.LT.1',15,2,2)
       IF(INCY.LT.1)CALL SETERR('DSWAP-INCY.LT.1',15,3,2)
C/
       DO 10 I=1,N
          TEMP=X(1,I)
          X(1,I)=Y(1,I)
          Y(1,I)=TEMP
 10    CONTINUE
       RETURN
       END
