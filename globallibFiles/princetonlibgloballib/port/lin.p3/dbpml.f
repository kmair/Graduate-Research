       SUBROUTINE DBPML(N,ML,G,IG,X,Y)
C
C BAND SYMMETRIC POSITIVE DEFINITE MULTIPLICATION
C
C THIS SUBROUTINE MULTIPLIES A VECTOR X BY A SYMMETRIC
C BAND POSITIVE MATRIX A PACKED INTO G
C
C INPUT PARAMETERS
C N       ORDER OF THE MATRIX
C ML      NUMBER OF NONZERO DIAGONALS ON AND BELOW THE DIAGONAL
C         OF A
C G       MATRIX INTO WHICH A IS PACKED-G(J-I+1,I)=A(I,J)
C IG      ROW DIMENSION OF G
C X       VDOUBLE PRECISION VECTOR OF LENGTH N
C OUTPUT PARAMTERS
C Y       A TIMES X
C
C ERROR CONDITIONS
C 1  N<1
C 2  ML.LT.1
C 3  IG.LT.ML
           DOUBLE PRECISION G(IG,N),X(N),Y(N)
C/6S
C          IF(N.LT.1) CALL SETERR(13HDBPML-N.LT.1 ,13,1,2)
C          IF(ML.LT.1) CALL SETERR(13HDBPML-ML.LT.1,13,2,2)
C          IF(IG.LT.ML)CALL SETERR(14HDBPML-IG.LT.ML,14,3,2)
C/7S
           IF(N.LT.1) CALL SETERR('DBPML-N.LT.1 ',13,1,2)
           IF(ML.LT.1) CALL SETERR('DBPML-ML.LT.1',13,2,2)
           IF(IG.LT.ML)CALL SETERR('DBPML-IG.LT.ML',14,3,2)
C/
           DO 10 I=1,N
              Y(I)=G(1,I)*X(I)
 10        CONTINUE
           IF(ML.EQ.1) RETURN
           DO 30 J=2,ML
              JEND=N+1-J
              JM1=J-1
              DO 20 I=1,JEND
                 IPJM1=I+JM1
                 Y(I)=Y(I)+G(J,I)*X(IPJM1)
                 Y(IPJM1)=Y(IPJM1)+G(J,I)*X(I)
 20           CONTINUE
 30        CONTINUE
           RETURN
           END
