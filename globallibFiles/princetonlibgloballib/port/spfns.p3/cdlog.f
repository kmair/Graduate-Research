      SUBROUTINE CDLOG(X,LOG)
      DOUBLE PRECISION X(2),LOG(2),R,DLOG,DSQRT, DATAN2
C
C COMPLEX DOUBLE PRECISION LOGARITHM
C
      IF (DABS(X(1)) .GT. DABS(X(2))) GO TO 10
      R = X(1)/X(2)
      LOG(1) = DLOG(DABS ( X(2) )*DSQRT( 1.D0 + R*R ) )
      GO TO 20
C
 10   R = X(2)/X(1)
      LOG(1) = DLOG(DABS ( X(1) )*DSQRT( 1.D0 + R*R ) )
 20   LOG(2) = DATAN2( X(2),X(1) )
C
      RETURN
      END