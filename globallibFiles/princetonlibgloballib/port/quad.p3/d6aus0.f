      LOGICAL FUNCTION D6AUS0 (N,MEPS,D,E,Z)
      INTEGER N
      DOUBLE PRECISION MEPS,D(1),E(1),Z(1)
C..PETER BUSINGERS QL METHOD FOR FINDING THE EIGEN VALUES
C..AND VECTORS OF A TRIDIAGONAL MATRIX.
      INTEGER I,J,K,L,M
      DOUBLE PRECISION B,C,F,G,H,P,R,S
      DOUBLE PRECISION DSQRT
      D6AUS0=.FALSE.
      IF(2.GT.N)GOTO 11
      DO 10 I=2,N
         Z(I)=0.0D0
   10    E(I-1)=E(I)
   11 E(N)=0.D0
      Z(1)=1.0D0
      B=0.D0
      F=0.D0
      DO 85 L=1,N
         J=0
         H=MEPS*(DABS(D(L))+DABS(E(L)))
         IF(B.LT.H)B=H
         DO 20 M=L,N
            IF(DABS(E(M)).LE.B)GOTO 30
   20       CONTINUE
   30    IF(M.EQ.L)GOTO 85
   31    IF(J.EQ.30)RETURN
         J=J+1
         G=D(L)
         P=(D(L+1)-G)/(2.D0*E(L))
         IF(DABS(P).LT.1.D0)R=DSQRT(1.D0+P*P)
         IF(DABS(P).GE.1.D0)R=DABS(P)*DSQRT(1.D0+1.D0/P/P)
         IF(P.LT.0.D0)D(L)=E(L)/(P-R)
         IF(P.GE.0.D0)D(L)=E(L)/(P+R)
         H=G-D(L)
         L1=L+1
         IF(L1.GT.N)GOTO 41
         DO 40 I=L1,N
   40       D(I)=D(I)-H
   41    F=F+H
         P=D(M)
         C=1.D0
         S=0.D0
         M1=M-1
         IF(L.GT.M1)GOTO 81
         DO 80 II=L,M1
            I=M1+L-II
            G=C*E(I)
            H=C*P
            IF(DABS(P).LT.DABS(E(I)))GOTO 50
            C=E(I)/P
            R=DSQRT(C*C+1.D0)
            E(I+1)=S*P*R
            S=C/R
            C=1.D0/R
            GOTO 60
   50       C=P/E(I)
            R=DSQRT(C*C+1.D0)
            E(I+1)=S*E(I)*R
            S=1.D0/R
            C=C/R
   60       P=C*D(I)-S*G
            D(I+1)=H+S*(C*G+S*D(I))
            H=Z(I+1)
            Z(I+1)=S*Z(I)+C*H
            Z(I)=C*Z(I)-S*H
   80       CONTINUE
   81    E(L)=S*P
         D(L)=C*P
         IF(DABS(E(L)).GT.B)GOTO 31
   85    D(L)=D(L)+F
      DO 110 I=1,N
         K=I
         P=D(I)
         I1=I+1
         IF(I1.GT.N)GOTO 91
         DO 90 J=I1,N
            IF(D(J).GE.P)GOTO 90
            K=J
            P=D(J)
   90       CONTINUE
   91    IF(K.EQ.I)GOTO 110
         D(K)=D(I)
         D(I)=P
         P=Z(I)
         Z(I)=Z(K)
         Z(K)=P
  110    CONTINUE
      D6AUS0=.TRUE.
      RETURN
      END
