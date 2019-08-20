C$SEQ QBLC 0 20
C$TEST QBLC
C***********************************************************************
C
C  EXAMPLE OF USE OF THE PORT PROGRAM BQUAD
C
C***********************************************************************
      INTEGER NCALL,IWRITE,I1MACH
      REAL SFUNC,X(3),ANS,ERREST,TRUERR
      EXTERNAL SFUNC
      COMMON/COUNT/NCALL
      NCALL=0
      X(1) = -1.0E0
      X(2) =   0.0E0
      X(3) = +1.0E0
C
C  BQUAD WILL TAKE INTO ACCOUNT THE BREAK AT X=0
C
      CALL BQUAD (SFUNC,3,X,1.E-6,ANS,ERREST)
      TRUERR=EXP(1.E0) - ANS
      IWRITE=I1MACH(2)
      WRITE(IWRITE, 99) ANS, ERREST, TRUERR, NCALL
  99   FORMAT(1X,4HANS=,1PE15.7,10H   ERREST=,1PE12.3,
     1     10H   TRUERR=,1PE12.3/1X,6HNCALL=,I4)
      STOP
      END
      REAL FUNCTION SFUNC(X)
      REAL X
      COMMON/COUNT/NCALL
      NCALL = NCALL+1
      SFUNC = AMAX1(1.E0, EXP(X))
      RETURN
      END
