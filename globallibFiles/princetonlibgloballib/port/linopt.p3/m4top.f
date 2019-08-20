        SUBROUTINE M4TOP(N,R,RHS,X)
C
C THIS SUBROUTINE SOLVES AN UPPER TRIANGULAR SYSTEM
C STORED IN 1 VECTOR
        REAL RHS(N),X(N),R(N), T
        L=(N*(N+1))/2
        DO 10 I=1,N
           X(I)=-RHS(I)
 10     CONTINUE
        L=(N*(N+1))/2
        NP1=N+1
        DO 30 IB=1,N
           I=NP1-IB
           X(I)=-X(I)/R(L)
           T=X(I)
           L=L-1
           J=I-1
 20        IF (J.LE.0)GO TO 30
           X(J)=X(J)+T*R(L)
           L=L-1
           J=J-1
           GO TO 20
 30     CONTINUE
        RETURN
        END
