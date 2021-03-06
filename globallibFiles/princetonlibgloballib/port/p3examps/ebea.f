C$SEQ EBEA 0 20
C$TEST EBEA
C***********************************************************************
C
C  EXAMPLE OF USE OF THE PORT PROGRAM EEBSF
C
C***********************************************************************
      INTEGER I1MACH,IWRITE,K,NT1,NT2
      EXTERNAL F
      REAL EESFF,EEBSF,T1(100),A1(100),T2(100),A2(100),
     1 ERROR(2),ERREST(2)
C
C MAKE THE MESH
C
      K = 4
      CALL UMB(0.0E0,3.14E0,16,K,T1,NT1)
      CALL UMB(0.0E0,3.14E0,21,K,T2,NT2)
C
C DO THE FITTING
C
      CALL L2SFF(F,K,T1,NT1,A1)
      CALL L2SFF(F,K,T2,NT2,A2)
C
C GET THE ERROR
C
      ERROR(1) = EESFF(K,T1,NT1,A1,F)
      ERROR(2) = EESFF(K,T2,NT2,A2,F)
C
      ERREST(1) = EEBSF(K,T1,NT1,A1,T2,NT2,A2)
      ERREST(2) = ERREST(1)*(FLOAT(NT1-2*K+1)/FLOAT(
     1 NT2-2*K-1))**K
      IWRITE = I1MACH(2)
      WRITE(IWRITE,99)ERROR(1),ERROR(2),ERREST(1),ERREST(2)
   99  FORMAT(8H ERROR = ,2E10.2,8H ESTER = ,2E10.2)
C
      STOP
C
      END
      SUBROUTINE F(X,NX,FX,WX)
C
      REAL X(NX),FX(NX),WX(NX)
C
      DO 10 I = 1,NX
      FX(I) = SIN(X(I))
   10 CONTINUE
C
      RETURN
C
      END
