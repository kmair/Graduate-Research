      SUBROUTINE MNPD(X,NX,N,T,NT)
C
C
C TO CREATE A MESH FROM AN ARRAY OF FITTING POINTS.
C
C METHOD - USE AT AT LEAST N FITTING POINTS X IN EACH MESH INTERVA
C
C MNEMONIC - DOUBLE PRECISION MESH GENERATOR
C            USING N POINTS PER INTERVAL, DISTINCT POINTS.
C
C INPUT -
C
C    X  - THE MONOTONE INCREASING ARRAY OF FITTING POINTS.
C         ANY MULTIPLICITIES IN X WILL BE IGNORED.
C    NX - THE NUMBER OF POINTS IN THE ARRAY X.
C    N  - THE NUMBER OF POINTS X TO BE USED IN EACH MESH INTERVAL.
C
C OUTPUT -
C
C    T   - THE MESH OF POINTS TAKEN FROM THE ARRAY X.
C    NT  - THE NUMBER OF POINTS IN THE MESH.
C
C          IF ( NDX .GE. 2*N-1 )
C
C            NT = 3 + (NDX-2*N+1)/(N-1)
C
C          OTHERWISE
C
C            NT = 2
C
C          WHERE NDX IS THE NUMBER OF DISTINCT POINTS IN THE ARRAY
C
C ERROR STATES -
C
C   1 - NX .LT. 2.
C   2 - N .LT. 2
C   3 - THE ARRAY X IS NOT MONOTONE INCREASING.
C   4 - THE NUMBER OF DISTINCT POINTS IN THE ARRAY X IS ONE.
C
C STORAGE ALLOCATED - NONE.
C
      INTEGER NDX,I
      INTEGER NX,N,NT
      REAL X(NX),T(1)
C
C
C
C T(NT).
C
C
C CHECK THE INPUT FOR ERRORS.
C
      IF( NX.GE.2 )      GOTO 1000
C/6S
C     CALL SETERR(15H MNPD - NX.LT.2,15,1,2)
C/7S
      CALL SETERR(' MNPD - NX.LT.2',15,1,2)
C/
 1000 CONTINUE
      IF( N.GE.2 )      GOTO 1002
C/6S
C     CALL SETERR(14H MNPD - N.LT.2,14,2,2)
C/7S
      CALL SETERR(' MNPD - N.LT.2',14,2,2)
C/
 1002 CONTINUE
C
      NDX = 1
C GET THE NUMBER OF DISTINCT POINTS IN X.
      DO 1004 I = 2, NX
      IF( X(I).GE.X(I-1) )      GOTO 1007
C/6S
C     CALL SETERR(36H MNPD - X IS NOT MONOTONE INCREASING,36,3,2)
C/7S
      CALL SETERR(' MNPD - X IS NOT MONOTONE INCREASING',36,3,2)
C/
 1007 CONTINUE
      IF( X(I).LE.X(I-1) )      GOTO 1009
      NDX = NDX+1
 1009 CONTINUE
 1004 CONTINUE
 1005 CONTINUE
      IF( NDX.NE.1 )      GOTO 1011
C/6S
C           CALL SETERR(37H MNPD - ONLY ONE POINT IN THE ARRAY X,37,4,2)
C/7S
            CALL SETERR(' MNPD - ONLY ONE POINT IN THE ARRAY X',37,4,2)
C/
 1011 CONTINUE
C
      CALL MNPB(X,NX,N,1,T,NT)
C
      RETURN
C
      END
