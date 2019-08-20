C$SEQ MFTE 0 20
C$TEST MFTE
C***********************************************************************
C
C  TEST OF PORT PROGRAMS FOR MULTIPLE FFT -- MFTCC, MFTRC, MFTCR
C
C***********************************************************************
C
      REAL DSTAK(16000)
      COMMON /CSTAK/DSTAK
C
      CALL ISTKIN(16000,3)
      CALL TESTER(70)
      CALL TESTER(210)
      STOP
      END
      SUBROUTINE TESTER(N)
      INTEGER N
C
C   TEST COMPUTES FORWARD AND BACKWARD TRANSFORMS ON RANDOM
C   INPUT DATA
C
      EXTERNAL I1MACH, ISTKST, UNI
      INTEGER  I1MACH, ISTKST
      REAL UNI
      REAL T(1000),A(630),B(630)
      REAL C(630),D(630),E(630),F(630)
      REAL ERR,FM1,FN1,FN2,Q1,Q2,SGN
      INTEGER IFX(25)
      INTEGER I,ICUR,IJA,IMAX,IOUT,IWRITE,J,K,M
      REAL DSTAK(16000)
C
      COMMON /CSTAK/DSTAK
C
C TEST MFTCC, EACH RADIX, FOR RANDOM INPUT SEQUENCES
C
C
      IWRITE = I1MACH(2)
C
      WRITE(IWRITE,130)
C
      M = 3
C
      CALL MFTCI(N,IFX,T)
      I = IFX(2) + 2
      WRITE(IWRITE,140)(IFX(K),K=1,I)
C
      DO 20 J = 1,M
         IJA = (J-1)*N+1
         DO 10 I = 1,N
            Q1     = (UNI(0))
            Q2     = (UNI(0))
            A(IJA) = Q1
            E(IJA) = Q1
            B(IJA) = Q2
            F(IJA) = Q2
            IJA    = IJA+1
 10      CONTINUE
 20   CONTINUE
C
C COMPUTE SIGN = +1.0 TRANSFORM
      SGN = +1.0E0
      CALL MFTCC(N,M,A(1),B(1),1,N,C(1),D(1),1,N,IFX,T,SGN)
C NOW COMPUTE UN-NORMALIZED INVERSE
      SGN = -1.0E0
      CALL MFTCC(N,M,C(1),D(1),1,N,A(1),B(1),1,N,IFX,T,SGN)
C NOW COMPARE INPUT SEQUENCE TO RESULT OF FORWARD/BACKWARD X-FORM
      FN1 = 1.E0/FLOAT(N)
      FN2 = FN1*0.5E0
C E AND F ARE THE REAL/IMAGINARY PARTS OF RESIDUAL
      DO 40 J = 1,M
         IJA = (J-1)*N+1
         DO 30 I = 1,N
            E(IJA) = E(IJA) - FN1*A(IJA)
            F(IJA) = F(IJA) - FN1*B(IJA)
            IJA    = IJA + 1
 30      CONTINUE
 40   CONTINUE
      FM1 = 1.E0/FLOAT(M)
      ERR = 0.E0
      DO 60 J = 1,M
         IJA = (J-1)*N+1
         DO 50 I = 1,N
            ERR = ERR +  ABS(E(IJA)) +  ABS(F(IJA))
            IJA = IJA+1
 50      CONTINUE
 60   CONTINUE
      ERR = ERR*FN2*FM1
      WRITE(IWRITE,150)ERR
C
      IOUT = ISTKST(1)
      ICUR = ISTKST(2)
      IMAX = ISTKST(3)
      WRITE(IWRITE,160)IOUT,ICUR,IMAX
C
C NOW TEST THE REAL TO COMPLEX ROUTINES
C
      WRITE(IWRITE,170)
C
      CALL MFTRI(N,IFX,T)
      WRITE(IWRITE,140)(IFX(K),K=1,5)
C
      DO 80 J = 1,M
         IJA = (J-1)*N+1
         DO 70 I = 1,N
            Q1     = (UNI(0))
            A(IJA) = Q1
            E(IJA) = A(IJA)
            IJA    = IJA + 1
 70      CONTINUE
 80   CONTINUE
C
C COMPUTE FORWARD (SIGN=1.) REAL TO COMPLEX TRANSFORM
C
      SGN = 1.0E0
      CALL MFTRC(N,M,A,1,N,C(1),D(1),1,N,IFX,T,SGN)
C NOW COMPUTE INVERSE
      SGN = -1.0E0
      CALL MFTCR(N,M,C(1),D(1),1,N,A,1,N,IFX,T,SGN)
C
C NOW FIND THE ERROR
C
      FN1 = 1.E0/FLOAT(N)
      FM1 = 1.E0/FLOAT(M)
      DO 100 J = 1,M
         IJA = (J-1)*N+1
         DO 90 I = 1,N
            E(IJA) = E(IJA) - FN1*A(IJA)
            IJA    = IJA +1
 90      CONTINUE
 100  CONTINUE
C
      ERR = 0.0E0
      DO 120 J = 1,M
         IJA = (J-1)*N+1
         DO 110 I = 1,N
            ERR = ERR +  ABS(E(IJA))
            IJA = IJA+1
 110     CONTINUE
 120  CONTINUE
C
      ERR = ERR*FN1*FM1
      WRITE(IWRITE,150)ERR
      IOUT = ISTKST(1)
      ICUR = ISTKST(2)
      IMAX = ISTKST(3)
      WRITE(IWRITE,160)IOUT,ICUR,IMAX
C
C OUTPUT FORMATS
C
 130  FORMAT(/1X,46H ******* MFTCC RESULTS AND DIAGNOSTICS *******)
 140  FORMAT(1X,7H IFX = /10(1X,I5))
 150  FORMAT(1X,26H AVERAGE ABSOLUTE ERROR = ,E20.8)
 160  FORMAT(43H NUMBER OF OUTSTANDING STACK ALLOCATIONS = ,I5
     1      /43H NUMBER OF CURRENT ACTIVE ALLOCATIONS    = ,I5
     2      /43H MAXIMUM ACTIVE LENGTH ACHIEVED          = ,I5)
 170  FORMAT(/1X,47H ****** MFTRC-MFTCR RESULTS, DIAGNOSTICS ******)
      RETURN
      END
