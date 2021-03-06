       SUBROUTINE DQP1NT(A,M,N,AMAN,IA,B,C,X,CTX,IS,SIMP,ISIMP,IE,
     1 ITER,IPTG,IAG,IAS,U,IEND,IPH)
C
C THIS IS A PRINT ROUTINE
C
       INTEGER IA,N
       DOUBLE PRECISION CTX,A(IA,1),X(N),B(1)
       LOGICAL IEND
       EXTERNAL AMAN
       INTEGER IPTG(N),ISIMP(1)
       DOUBLE PRECISION SIMP(1),C(1),U(1)
       DOUBLE PRECISION TOUTD
       DOUBLE PRECISION EPSI,D1MACH
       IWRITE=I1MACH(2)
       EPSI=-1000.D0*D1MACH(4)
       IF(ITER.EQ.1)WRITE(IWRITE,20)
 20    FORMAT(1H ,30H TRYING TO FIND FEASIBLE POINT)
       IEND = .FALSE.
       IAGPE=IAG+IE
       WRITE(IWRITE,1)ITER,IAGPE,IAS
 1     FORMAT(/14H AT ITERATION ,I5
     1  /18H NO.OF ACT. GEN.= ,I5,15H NO.OF ACT.SIM=,I5)
C      WRITE(IWRITE,2)(X(I),I=1,N)
 2     FORMAT(3H X ,5D15.5)
       DO 10 I=1,M
          TOUTD=-B(I)
          DO 8 J=1,N
             TOUTD=TOUTD+A(I,J)*X(J)
 8       CONTINUE
          IF (TOUTD.LT.EPSI)WRITE(IWRITE,9)I,TOUTD
 9        FORMAT(15H AT CONSTRAINT ,I5,11H RESIDUAL= ,D15.5)
 10    CONTINUE
       IF (IAGPE.EQ.0)GO TO 12
       WRITE(IWRITE,11)(IPTG(I),I=1,IAGPE)
 11    FORMAT(29H  ACTIVE GENERAL CONSTRAINTS ,10I4)
 12    IF (IAS.LT.1)RETURN
       DO 15 I=1,IAS
          IP=IABS(ISIMP(I))
          IF (ISIMP(I).GT.0)WRITE(IWRITE,13)IP
 13       FORMAT(18H LOWER BOUND ON X(,I2,11H) IS ACTIVE)
          IF (ISIMP(I).LT.0)WRITE(IWRITE,14)IP
 14       FORMAT(18H UPPER BOUND ON X(,I2,11H) IS ACTIVE)
 15    CONTINUE
       RETURN
       END
