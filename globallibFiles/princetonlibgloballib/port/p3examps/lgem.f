C$SEQ LGEM 0 20
C$TEST LGEM
C***********************************************************************
C
C  EXAMPLE OF USE OF THE PORT PROGRAM GEML
C
C***********************************************************************
        INTEGER I, J, IWRITE, I1MACH, N
        REAL A(10, 10), X(10), B(10)
        REAL ERR, SASUM, UNI, COND
        N=10
C
C CONSTRUCT A MATRIX
C
        DO 20 I=1,N
           DO 10 J=I,N
              A(I,J)=J-I
              A(J,I)=J-I + 1
  10       CONTINUE
  20    CONTINUE
C
C CONSTRUCT A RANDOM VECTOR X
C
        DO 30 I=1,N
           X(I)=UNI(0)
  30    CONTINUE
C
C FIND THE VECTOR B=AX
C
       CALL GEML(N,A,10,X,B)
C
C SOLVE THE SYSTEM AX=B
C
       CALL GESS(N,A,10,B,N,1,COND)
C
C PRINT THE COMPUTED AND TRUE SOLUTION
C
       IWRITE=I1MACH(2)
       WRITE(IWRITE,31)
  31   FORMAT(34H TRUE SOLUTION   COMPUTED SOLUTION)
       WRITE(IWRITE,32)(X(I),B(I),I=1,N)
  32   FORMAT(1H ,2E17.8)
C
C COMPUTE THE RELATIVE ERROR
C
       ERR=0.0
       DO 40 I=1,N
          ERR=ERR + ABS(B(I)-X(I))
  40   CONTINUE
       ERR=ERR/SASUM(N,X,1)
       WRITE(IWRITE,41)ERR
  41   FORMAT(19H RELATIVE ERROR IS ,1PE15.7)
       WRITE(6,42)COND
  42   FORMAT(21H CONDITION NUMBER IS ,1PE15.7)
       STOP
       END
