              SUBROUTINE   BABS(N,U,IU,B,IB,NB,MU)
C
C THIS SUBROUTINE SOLVES TX = B WHERE T IS AN
C UPPER TRIANGULAR BANDED MATRIX
C
C INPUT PARAMETERS
C N         THE ORDER OF THE SYSTEM
C U         AN IU X N ARRAY CONTAINING THE UPPERTRIANGULAR
C           MATRIX T WHERE U(1+J-I,I)=T(I,J)
C IU        ROW DIMENSION OF U
C B         THE RIGHT HAND SIDES;OVERWRITTEN ON OUTPUT
C IB        ROW DIMENSION OF B MATRIX
C NB        NUMBER OF RIGHT HAND SIDES
C MU        THE NUMBER OF NONZERO DIAGONALS OF T
C OUTPUT PARAMETERS
C B         THE SOLUTION MATRIX X
C SCRATCH SPACE NEEDED -NONE
C ERROR CONDITIONS
C   1       N.LT.1        FATAL
C   2       IU.LT.MU      FATAL
C   3       MU.LT.1       FATAL
C   4       IB.LT.N     FATAL
C  5        NB.LT.1       FATAL
C   10+K    SINGULAR U OF RANK K  RECOVERABLE
        INTEGER IU,N,MU
        REAL U(IU,N),B(IB,NB)
        REAL X
C/6S
C       IF (N.LT.1) CALL SETERR(13H  BABS-N.LT.1,13,1,2)
C       IF (IU.LT.MU) CALL SETERR(15H  BABS-IU.LT.MU,15,2,2)
C       IF (MU.LT.1) CALL SETERR(14H  BABS-MU.LT.1,14,3,2)
C       IF (IB.LT.N) CALL SETERR(14H  BABS-IB.LT.N,14,4,2)
C       IF (NB.LT.1) CALL SETERR(14H  BABS-NB.LT.1,14,5,2)
C/7S
        IF (N.LT.1) CALL SETERR('  BABS-N.LT.1',13,1,2)
        IF (IU.LT.MU) CALL SETERR('  BABS-IU.LT.MU',15,2,2)
        IF (MU.LT.1) CALL SETERR('  BABS-MU.LT.1',14,3,2)
        IF (IB.LT.N) CALL SETERR('  BABS-IB.LT.N',14,4,2)
        IF (NB.LT.1) CALL SETERR('  BABS-NB.LT.1',14,5,2)
C/
        CALL ENTER(1)
        L=1
        NP1=N+1
        DO 100 II=1,N
            I=NP1-II
            DO 50 J=1,NB
            X=B(I,J)
            IF (L.LT.2) GO TO 30
            KB=I
            DO 20 K=2,L
               KB=KB+1
               X=X-U(K,I)*B(KB,J)
 20         CONTINUE
 30         IF (U(1,I).NE.0.0) GO TO 40
C/6S
C              CALL SETERR(22H  BABS-SINGULAR MATRIX,22,9+I,1)
C/7S
               CALL SETERR('  BABS-SINGULAR MATRIX',22,9+I,1)
C/
               GO TO 150
 40         B(I,J)=X/U(1,I)
 50      CONTINUE
            L=MIN0(L+1,MU)
 100     CONTINUE
 150     CALL LEAVE
         RETURN
         END
