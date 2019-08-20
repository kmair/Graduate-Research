C$SEQ SPLF 0 20
C$TEST SPLF
C***********************************************************************
C
C  EXAMPLE OF USE OF THE PORT PROGRAM SPLNI
C
C***********************************************************************
      INTEGER K,I,N,IWRITE,I1MACH,NT
      REAL X(51),Y(51),T(100),A(100),SINT,TINT
C
      K = 4
C
C MAKE THE ABSCISSAE FOR THE FIT.
C
      CALL UMD(0.0E0,3.14E0,51,X)
C
C MAKE THE DATA.
C
      DO 1000 I = 1, 51
      Y(I) = SIN(X(I))
 1000 CONTINUE
C
C MAKE THE MESH.
C
      N = 2
C
      CALL MNPB(X,51,N,K,T,NT)
C
C DO THE FIT.
C
      CALL DL2SF(X,Y,51,K,T,NT,A)
C
C EVALUATE THE SPLINE INTEGRAL AND THE TRUE INTEGRAL.
C
      CALL SPLNI(K,T,NT,A,T(NT),1,SINT)
C
      TINT = 1.0E0-COS(3.14E0)
C
      IWRITE = I1MACH(2)
      WRITE(IWRITE,1003) SINT,TINT
 1003 FORMAT(18H SPLINE INTEGRAL =,E20.8//
     1   18H TRUE INTEGRAL   =,E20.8)
C
      STOP
C
      END
