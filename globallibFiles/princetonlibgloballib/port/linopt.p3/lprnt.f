       SUBROUTINE LPRNT(A,M,N,AMAN,IA,B,C,X,CTX,IS,SIMP,ISIMP,IE,
     1 ITER,IPTG,IAG,IAS,U,IEND,IPH)
C
C THIS IS A PRINT ROUTINE
C
       REAL CTX,A(1),X(N),B(1)
       EXTERNAL AMAN
       LOGICAL IEND
       INTEGER IA(1),IPTG(N),ISIMP(1)
       REAL SIMP(1),C(1),U(1)
       IEND= .FALSE.
       RETURN
       END