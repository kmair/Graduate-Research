        SUBROUTINE GEFS(N,A,IA,B,IB,NB,INTER)
C MNEMONIC-GENERAL FORWARD SOLVE
        INTEGER N,IA,IB,NB
        REAL A(IA,N),B(IB,NB),T
        INTEGER INTER(N)
C THIS SUBROUTINE SOLVES AX=B WHERE A IS A LOWER
C UNIT TRIANGULAR MATRIX. IT MAY BE USE AS PART OF
C OF A PACKAGE FOR SOLVING AX=B WHEN A IS A GENERAL
C GENERAL MATRIX AND GELU HAS BEEN USED
C INPUT PARAMETERS
C     N    THE ORDER OF THE PROBLEM
C     A    AN IA X N ARRAY CONTAINING A UNIT LOWER TRIANGULAR
C          MATRIX OR THE DECOMPOSITION DETERMINED IN GELU
C          DETERMINED IN GELU
C     IA   ROW DIMENSION OF THE A MATRIX
C     B    A VECTOR CONTAINING THE RIGHT HAND SIDE
C          IT WILL BE DESTROYED ON OUTPUT
C     IB   ROW DIMENSION OF THE B ARRAY,MUST BE AT LEAST N
C     NB   NUMBER OF RIGHT HAND SIDES
C     INTER AN INTEGER VECTOR CONTAINING A RECORD OF THE
C           PERMUTATIONS PERFORMED IN GELU
C OUTPUT PARAMTERS
C     B    THE SOLUTION X OF THE PROBLEM
C ERROR CONDITIONS
C    1      N LESS THAN 1    FATAL
C    2      IA.LT.N          FATAL
C    5      INTER NOT IN 1,....N      FATAL
C    3      IB.LT.N          FATAL
C    4      NB.LT.1          FATAL
C
C/6S
C      IF(N.LT.1) CALL SETERR(12H GEFS-N.LT.1,12,1,2)
C      IF (IA.LT.N) CALL SETERR(13H GEFS-IA.LT.N,13,2,2)
C      IF(IB.LT.N) CALL SETERR(13H GEFS-IB.LT.N,13,3,2)
C      IF(NB.LT.1) CALL SETERR(13H GEFS-NB.LT.1,13,4,2)
C/7S
       IF(N.LT.1) CALL SETERR(' GEFS-N.LT.1',12,1,2)
       IF (IA.LT.N) CALL SETERR(' GEFS-IA.LT.N',13,2,2)
       IF(IB.LT.N) CALL SETERR(' GEFS-IB.LT.N',13,3,2)
       IF(NB.LT.1) CALL SETERR(' GEFS-NB.LT.1',13,4,2)
C/
C FORWARD SOLVE
       IF (N.EQ.1) RETURN
       NM1=N-1
       DO 40 K=1,NM1
          M=INTER(K)
          IF (M.GT.0.AND.M.LE.N) GO TO 10
C/6S
C         CALL SETERR(22H GEFS-INTER NOT IN 1-N,22,5,2)
C/7S
          CALL SETERR(' GEFS-INTER NOT IN 1-N',22,5,2)
C/
          RETURN
 10      CONTINUE
         DO 35 I=1,NB
            T=B(M,I)
            IF (M.EQ.K) GO TO 20
            B(M,I)=B(K,I)
            B(K,I)=T
 20         KP1=K+1
            DO 30 J=KP1,N
               B(J,I)=B(J,I)+T*A(J,K)
 30         CONTINUE
 35      CONTINUE
 40    CONTINUE
       RETURN
       END
