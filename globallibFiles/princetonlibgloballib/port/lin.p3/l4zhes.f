      SUBROUTINE L4ZHES(N,A,NA,B,NB,X,NX,WANTX)
C
C THIS SUBROUTINE REDUCES THE COMPLEX MATRIX A TO UPPER
C HESSENBERG FORM AND REDUCES THE COMPLEX MATRIX B TO
C TRIANGULAR FORM
C
C INPUT PARAMETERS-
C
C N   THE ORDER OF THE A AND B MATRICES
C
C A   A COMPLEX MATRIX
C
C NA  THE ROW DIMENSION OF THE A MATRIX
C
C B   A COMPLEX MATRIX
C
C NB  THE ROW DIMENSION OF THE B MATRIX
C
C NX  THE ROW DIMENSION OF THE X MATRIX
C
C WANTX A LOGICAL VARIABLE WHICH IS SET TO .TRUE. IF
C       THE EIGENVECTORS ARE WANTED. OTHERWISE IT SHOULD
C     BE SET TO .FALSE.
C
C OUTPUT PARAMETERS-
C
C A  ON OUTPUT A IS AN UPPER HESSENBERG MATRIX, THE
C    ORIGINAL MATRIX HAS BEEN DESTROYED
C
C B  AN UPPER TRIANGULAR MATRIX, THE ORIGINAL MATRIX
C    HAS BEEN DESTROYED
C
C X  CONTAINS THE TRANSFORMATIONS NEEDED TO COMPUTE
C    THE EIGENVECTORS OF THE ORIGINAL SYSTEM
C
C
      COMPLEX Y,W,Z,A(NA,N),B(NB,N),X(NX,N)
      REAL C,D
      LOGICAL WANTX
      NM1=N-1
C
C REDUCE B TO TRIANGULAR FORM USING ELEMENTARY
C TRANSFORMATIONS
C
      DO 30 I=1,NM1
      D=0.00
        IP1=I+1
        DO 10 K=IP1,N
          Y=B(K,I)
          C= ABS(REAL(Y))+ABS(AIMAG(Y))
          IF (C.LE.D) GO TO 10
                  D=C
                  II=K
   10   CONTINUE
        IF (D.EQ.0.0) GO TO 30
        Y=B(I,I)
        IF(D.LE.ABS(REAL(Y))+ABS(AIMAG(Y))) GO TO 15
C
C MUST INTERCHANGE
C
          DO 11 J=1,N
            Y=A(I,J)
            A(I,J)=A(II,J)
            A(II,J)=Y
   11     CONTINUE
          DO 12 J=I,N
            Y=B(I,J)
            B(I,J)=B(II,J)
            B(II,J)=Y
   12   CONTINUE
   15   DO 20 J=IP1,N
          Y=B(J,I)/B(I,I)
          IF (REAL(Y).EQ.0.0.AND.AIMAG(Y).EQ.0.0) GO TO 20
          DO 18 K=1,N
            A(J,K)=A(J,K)-Y*A(I,K)
   18     CONTINUE
          DO 19 K=IP1,N
            B(J,K)=B(J,K)-Y*B(I,K)
   19     CONTINUE
   20   CONTINUE
        B(IP1,I)=(0.0,0.0)
   30 CONTINUE
C
C INITIALIZE X
C
      IF (.NOT.WANTX) GO TO 40
      DO 38 I=1,N
        DO 37 J=1,N
          X(I,J)=(0.0,0.0)
   37   CONTINUE
        X(I,I)=(1.0,0.00)
   38 CONTINUE
C
C REDUCE A TO UPPER HESSENBERG FORM
C
   40 NM2=N-2
      IF (NM2.LT.1) GO TO 100
      DO 90 J=1,NM2
        JM2=NM1-J
        JP1=J+1
        DO 80 II=1,JM2
          I=N+1-II
          IM1=I-1
          IMJ=I-J
          W=A(I,J)
          Z=A(IM1,J)
          IF (ABS(REAL(W))+ABS(AIMAG(W)).LE.
     C    ABS(REAL(Z))+ABS(AIMAG(Z))) GO TO 50
C
C MUST INTERCHANGE ROWS
C
            DO 45 K=J,N
              Y=A(I,K)
              A(I,K)=A(IM1,K)
              A(IM1,K)=Y
   45       CONTINUE
            DO 46 K=IM1,N
              Y=B(I,K)
              B(I,K)=B(IM1,K)
              B(IM1,K)=Y
   46       CONTINUE
   50     Z=A(I,J)
          IF (REAL(Z).EQ.0.0.AND.AIMAG(Z).EQ.0.0) GO TO 58
          Y=Z/A(IM1,J)
          DO 52 K=JP1,N
            A(I,K)=A(I,K)-Y*A(IM1,K)
   52     CONTINUE
          DO 54 K=IM1,N
            B(I,K)=B(I,K)-Y*B(IM1,K)
   54     CONTINUE
C
C TRANSFORMATION FROM THE RIGHT
C
   58       W=B(I,IM1)
            Z=B(I,I)
            IF(ABS(REAL(W))+ABS(AIMAG(W)).LE.
     1      ABS(REAL(Z))+ABS(AIMAG(Z))) GO TO 70
C
C MUST INTERCHANGE COLUMNS
C
            DO 60 K=1,I
              Y=B(K,I)
              B(K,I)=B(K,IM1)
              B(K,IM1)=Y
   60       CONTINUE
            DO 64 K=1,N
              Y=A(K,I)
              A(K,I)=A(K,IM1)
              A(K,IM1)=Y
   64       CONTINUE
            IF (.NOT.WANTX) GO TO 70
            DO 68 K=IMJ,N
              Y=X(K,I)
              X(K,I)=X(K,IM1)
              X(K,IM1)=Y
   68       CONTINUE
   70     Z=B(I,IM1)
          IF (REAL(Z).EQ.0.0.AND.AIMAG(Z).EQ.0.0) GO TO 80
          Y=Z/B(I,I)
          DO 72 K=1,IM1
            B(K,IM1)=B(K,IM1)-Y*B(K,I)
   72     CONTINUE
          B(I,IM1)=(0.0,0.0)
          DO 74 K=1,N
            A(K,IM1)=A(K,IM1)-Y*A(K,I)
   74     CONTINUE
          IF (.NOT.WANTX) GO TO 80
          DO 76 K=IMJ,N
            X(K,IM1)=X(K,IM1)-Y*X(K,I)
   76     CONTINUE
   80   CONTINUE
        A(JP1+1,J)=(0.0,0.0)
   90 CONTINUE
  100 RETURN
      END
