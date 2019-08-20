      REAL FUNCTION EESFF(K,T,N,A,F)
C
C
C PURPOSE -
C
C   TO FIND THE MAXIMUM ABSOLUTE DIFFERENCE BETWEEN F(Y) AND S(Y),
C   WHERE S(Y) IS THE B-SPLINE GIVEN BY (K,T,N,A).
C
C METHOD -
C
C   LET MEST = (2*K-1)*PI + 1.
C
C   THE MAXIMUM IS FOUND (ESTIMATED) BY SEARCHING EACH T
C   INTERVAL AT A PATTERN OF MEST POINTS GIVEN BY
C   COS(THETA) WHERE MEST EQUALLY SPACED THETA VALUES ON (-PI,0) A
C   THIS SEARCH GUARANTEES THAT IF F IS A POLYNOMIAL OF DEGREE LES
C   2*K, THE ERROR ESTIMATE WILL BE COMPUTED ACCURATE TO WITHIN A
C   FACTOR OF 2.
C
C MNEMONIC - DOUBLE PRECISION ESTIMATION OF THE ERROR IN A
C            SPLINE FIT TO A FUNCTION.
C
C INPUT -
C
C   K      - THE B-SPLINE ORDER USED.
C   T      - THE B-SPLINE MESH.
C   N      - THE NUMBER OF POINTS IN THE MESH T.
C   A      - THE B-SPLINE COEFFICIENTS.
C   F      - THE NAME OF A USER SUPPLIED SUBROUTINE
C
C                    F(X,NX,FX,WX)
C
C            F MUST DECLARED EXTERNAL IN THE USERS CALLING PROGRAM
C
C            INPUT -
C
C              X    - THE POINTS WHERE F IS TO BE EVALUATED.
C              NX   - THE LENGTH OF THE ARRAY X.
C
C            OUTPUT -
C
C              FX - FX(I) = F( X(I) ), I = 1,...,NX.
C              WX - WX IS A DUMMY ARRAY OF LENGTH NX TO MAKE THE C
C                   CONSISTANT WITH THAT TO F IN DL2SFF.
C
C OUTPUT -
C
C   PROCEDURE VALUE - THE MAXIMUM ERROR FOUND.
C
C SCRATCH SPACE ALLOCATED - 4*K + (2*K-1)*PI + 1 REAL WORDS.
C
C ERROR STATES -
C
C   1 - K.LT.2.
C   2 - N.LE.K.
C   3 - T NOT MONOTONE INCREASING.
C
      INTEGER I
      REAL X(2),ERR(2),EESFI
      EXTERNAL F
      INTEGER K,N
      REAL T(N),A(1)
C
C
C
C A(N-K).
C
C
C CHECK THE INPUT.
C
      IF( K.GE.2 )      GOTO 1000
C/6S
C     CALL SETERR(15H EESFF - K.LT.2,15,1,2)
C/7S
      CALL SETERR(' EESFF - K.LT.2',15,1,2)
C/
 1000 CONTINUE
      IF( N.GT.K )      GOTO 1002
C/6S
C     CALL SETERR(15H EESFF - N.LE.K,15,2,2)
C/7S
      CALL SETERR(' EESFF - N.LE.K',15,2,2)
C/
 1002 CONTINUE
C
      I21006 = N-1
      DO 1004 I = 1, I21006
      IF( T(I).LE.T(I+1) )      GOTO 1007
C/6S
C     CALL SETERR(34H EESFF - T NOT MONOTONE INCREASING,34,3,2)
C/7S
      CALL SETERR(' EESFF - T NOT MONOTONE INCREASING',34,3,2)
C/
 1007 CONTINUE
      IF( I+K.LE.N )      GOTO 1009
      GOTO 1004
 1009 CONTINUE
      IX1005 = I+K
      IF( T(IX1005).GT.T(I) )      GOTO 1011
C/6S
C     CALL SETERR(34H EESFF - T NOT MONOTONE INCREASING,34,3,2)
C/7S
      CALL SETERR(' EESFF - T NOT MONOTONE INCREASING',34,3,2)
C/
 1011 CONTINUE
C
 1004 CONTINUE
 1005 CONTINUE
      X(1) = T(1)
      X(2) = T(N)
C
      EESFF = EESFI(K,T,N,A,F,X,2,ERR)
      RETURN
C
      END