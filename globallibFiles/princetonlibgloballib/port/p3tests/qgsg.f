C$SEQ QGSG 0 20
C$TEST QGSG
C***********************************************************************
C
C  TEST OF THE PORT PROGRAMS FOR GAUSS QUADRATURE
C
C***********************************************************************
      COMMON/CSTAK/D(500)
      DOUBLE PRECISION D
C
      CALL ISTKIN(500, 4)
C
      CALL TG1
      CALL TGEX
      CALL TGEX2
      CALL TGEXA
      CALL TGLOG
      CALL TGXA
      CALL TGXAB
C
      STOP
      END
      SUBROUTINE TGEX2
      REAL X(5),W(5),FEX2,SUM,TRUE,PI,ERR
C
      CALL  GQEX2(5,X,W)
      IOUT=I1MACH(2)
      WRITE(IOUT,1)
      DO 10 J=1,5
   10    WRITE(IOUT,2) J, X(J),W(J)
      SUM=0.E0
      DO 20 J=1,5
   20    SUM=SUM+W(J)*FEX2(X(J))
      PI=2.E0*ATAN2(1.E0,0.E0)
      TRUE=SQRT(PI)*EXP(-0.25E0)
      ERR=TRUE-SUM
      WRITE(IOUT,3) TRUE,SUM,ERR
      RETURN
    1 FORMAT(///15H TEST OF  GQEX2//30H0ABSCISSAS AND WEIGHTS FOR N=5)
    2 FORMAT(I4,0P2E16.7)
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)
      END
      REAL FUNCTION FEX2(X)
      REAL X
      FEX2=COS(X)
      RETURN
      END
      SUBROUTINE TGEXA
      REAL X(5),W(5),FEXA,SUM,TRUE,PI,ERR
C
      CALL  GQEXA(5,-0.5E0,X,W)
      IOUT=I1MACH(2)
      WRITE(IOUT,1)
      DO 10 J=1,5
   10    WRITE(IOUT,2) J, X(J),W(J)
      SUM=0.E0
      DO 20 J=1,5
   20    SUM=SUM+W(J)*FEXA(X(J))
      PI=2.E0*ATAN2(1.E0,0.E0)
      TRUE=0.5E0*SQRT(PI)*(1.E0-1.E0/SQRT(3.E0))
      ERR=TRUE-SUM
      WRITE(IOUT,3) TRUE,SUM,ERR
      RETURN
    1 FORMAT(///15H TEST OF  GQEXA//30H0ABSCISSAS AND WEIGHTS FOR N=5)
    2 FORMAT(I4,0P2E16.7)
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)
      END
      REAL FUNCTION FEXA(X)
      REAL X
      FEXA=0.5E0*(1.E0-EXP(-2.E0*X))
      RETURN
      END
      SUBROUTINE TGLOG
      REAL X(5),W(5),FLOG,SUM,TRUE,PI2,ERR
C
      CALL  GQLOG(5,X,W)
      IOUT=I1MACH(2)
      WRITE(IOUT,1)
      DO 10 J=1,5
   10    WRITE(IOUT,2) J, X(J),W(J)
      SUM=0.E0
      DO 20 J=1,5
   20    SUM=SUM+W(J)*FLOG(X(J))
      PI2=ATAN2(1.E0,0.E0)
      TRUE=-(PI2**2/3.E0)
      ERR=TRUE-SUM
      WRITE(IOUT,3) TRUE,SUM,ERR
      RETURN
    1 FORMAT(///15H TEST OF  GQLOG//30H0ABSCISSAS AND WEIGHTS FOR N=5)
    2 FORMAT(I4,0P2E16.7)
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)
      END
      REAL FUNCTION FLOG(X)
      REAL X
      FLOG=-1.E0/(1.E0+X)
      RETURN
      END
      SUBROUTINE TGXA
      REAL X(5),W(5),FXA,SUM,TRUE,B(1),PI2,ERR
C
      CALL  GQXA(5,-0.5E0,X,W)
      IOUT=I1MACH(2)
      WRITE(IOUT,1)
      DO 10 J=1,5
   10    WRITE(IOUT,2) J, X(J),W(J)
      SUM=0.E0
      DO 20 J=1,5
   20    SUM=SUM+W(J)*FXA(X(J))
      CALL BESRJ(1.E0,1,B)
      PI2=ATAN2(1.E0,0.E0)
      TRUE=PI2*B(1)
      ERR=TRUE-SUM
      WRITE(IOUT,3) TRUE,SUM,ERR
      RETURN
    1 FORMAT(///14H TEST OF  GQXA//30H0ABSCISSAS AND WEIGHTS FOR N=5)
    2 FORMAT(I4,0P2E16.7)
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)
      END
      REAL FUNCTION FXA(X)
      REAL X
      FXA=COS(1.E0-X)/SQRT(2.E0-X)
      RETURN
      END
      SUBROUTINE TGXAB
      REAL X(5),W(5),FXAB,SUM,TRUE,PI,ERR
C
      CALL  GQXAB(5,-0.5E0,0.5E0,X,W)
      IOUT=I1MACH(2)
      WRITE(IOUT,1)
      DO 10 J=1,5
   10    WRITE(IOUT,2) J, X(J),W(J)
      SUM=0.E0
      DO 20 J=1,5
   20    SUM=SUM+W(J)*FXAB(X(J))
      PI=2.E0*ATAN2(1.E0,0.E0)
      TRUE=PI*(1.E0-1.E0/SQRT(3.E0))
      ERR=TRUE-SUM
      WRITE(IOUT,3) TRUE,SUM,ERR
      RETURN
    1 FORMAT(///15H TEST OF  GQXAB//30H0ABSCISSAS AND WEIGHTS FOR N=5)
    2 FORMAT(I4,0P2E16.7)
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)
      END
      REAL FUNCTION FXAB(X)
      REAL X
      FXAB=1.E0/(2.E0+X)
      RETURN
      END
      REAL FUNCTION  GAMMA(X)
      REAL X
      REAL Y, SQRT, ATAN2
C J. L. BLUE, 16 DEC 77
C DUMMY VERSION - ONLY GOOD FOR INTEGERS OR HALF-INTEGERS
      Y = X
       GAMMA = 1.E0
   1  IF (Y .LE. 1.E0) GOTO  2
         Y = Y-1.E0
          GAMMA = Y* GAMMA
         GOTO  1
   2  IF (Y .NE. 1.E0) GOTO 3
         GOTO  6
   3     IF (Y .NE. 0.5E0) GOTO 4
             GAMMA =  GAMMA*SQRT(2.E0*ATAN2(1.E0, 0.E0))
            GOTO  5
C/6S
C  4        CALL SETERR(16H GAMMA - NOT YET, 16, 1, 2)
C/7S
   4        CALL SETERR(' GAMMA - NOT YET', 16, 1, 2)
C/
   5  CONTINUE
   6  RETURN
      END
      SUBROUTINE TGEX
      REAL X(5),W(5),FEX,SUM,TRUE,PI,ERR
C
      CALL  GQEX(5,X,W)
      IOUT=I1MACH(2)
      WRITE(IOUT,1)
      DO 10 J=1,5
   10    WRITE(IOUT,2) J, X(J),W(J)
      SUM=0.E0
      DO 20 J=1,5
   20    SUM=SUM+W(J)*FEX(X(J))
      PI=2.E0*ATAN2(1.E0,0.E0)
      TRUE=PI**2/6.E0
      ERR=TRUE-SUM
      WRITE(IOUT,3) TRUE,SUM,ERR
      RETURN
    1 FORMAT(///14H TEST OF  GQEX//30H0ABSCISSAS AND WEIGHTS FOR N=5)
    2 FORMAT(I4,0P2E16.7)
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)
      END
      REAL FUNCTION FEX(X)
      REAL X
      FEX=X/(1.E0-EXP(-X))
      RETURN
      END
      SUBROUTINE TG1
      REAL X(5),W(5),F1,SUM,TRUE,ERR
C
      CALL  GQ1(5,X,W)
      IOUT=I1MACH(2)
      WRITE(IOUT,1)
      DO 10 J=1,5
   10    WRITE(IOUT,2) J, X(J),W(J)
      SUM=0.E0
      DO 20 J=1,5
   20    SUM=SUM+W(J)*F1(X(J))
      TRUE=ALOG(3.E0)
      ERR=TRUE-SUM
      WRITE(IOUT,3) TRUE,SUM,ERR
      RETURN
    1 FORMAT(///13H TEST OF  GQ1//30H0ABSCISSAS AND WEIGHTS FOR N=5)
    2 FORMAT(I4,0P2E16.7)
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)
      END
      REAL FUNCTION F1(X)
      REAL X
      F1=1.E0/(2.E0+X)
      RETURN
      END
C OUTPUT FROM TEST....
C
C TEST OF  GQ1
C ABSCISSAS AND WEIGHTS FOR N=5
C   1  -0.9061799E 00   0.2369269E 00
C   2  -0.5384693E 00   0.4786287E 00
C   3   0.              0.5688889E 00
C   4   0.5384693E 00   0.4786287E 00
C   5   0.9061799E 00   0.2369269E 00
C SAMPLE PROBLEM
C TRUE=   1.0986123E 00
C CALC=   1.0986093E 00
C ERR =   2.99E-06
C TEST OF  GQEX
C ABSCISSAS AND WEIGHTS FOR N=5
C   1   0.2635603E 00   0.5217556E 00
C   2   0.1413403E 01   0.3986668E 00
C   3   0.3596426E 01   0.7594245E-01
C   4   0.7085810E 01   0.3611759E-02
C   5   0.1264080E 02   0.2336997E-04
C SAMPLE PROBLEM
C TRUE=   1.6449341E 00
C CALC=   1.6449244E 00
C ERR =   9.68E-06
C TEST OF  GQEX2
C ABSCISSAS AND WEIGHTS FOR N=5
C   1  -0.2020183E 01   0.1995325E-01
C   2  -0.9585725E 00   0.3936193E 00
C   3   0.              0.9453087E 00
C   4   0.9585725E 00   0.3936193E 00
C   5   0.2020183E 01   0.1995325E-01
C SAMPLE PROBLEM
C TRUE=   1.3803884E 00
C CALC=   1.3803901E 00
C ERR =  -1.65E-06
C TEST OF  GQEXA
C ABSCISSAS AND WEIGHTS FOR N=5
C   1   0.1175813E 00   0.1221725E 01
C   2   0.1074562E 01   0.4802772E 00
C   3   0.3085937E 01   0.6774879E-01
C   4   0.6414729E 01   0.2687291E-02
C   5   0.1180719E 02   0.1528086E-04
C SAMPLE PROBLEM
C TRUE=   3.7456357E-01
C CALC=   3.7530771E-01
C ERR =  -7.44E-04
C TEST OF  GQLOG
C ABSCISSAS AND WEIGHTS FOR N=5
C   1   0.2913447E-01   0.2978935E 00
C   2   0.1739772E 00   0.3497762E 00
C   3   0.4117025E 00   0.2344883E 00
C   4   0.6773142E 00   0.9893046E-01
C   5   0.8947714E 00   0.1891155E-01
C SAMPLE PROBLEM
C TRUE=  -8.2246703E-01
C CALC=  -8.2246699E-01
C ERR =  -4.05E-08
C TEST OF  GQXA
C ABSCISSAS AND WEIGHTS FOR N=5
C   1   0.2216357E-01   0.5910485E 00
C   2   0.1878316E 00   0.5385334E 00
C   3   0.4615974E 00   0.4381727E 00
C   4   0.7483346E 00   0.2989027E 00
C   5   0.9484939E 00   0.1333427E 00
C SAMPLE PROBLEM
C TRUE=   1.2019697E 00
C CALC=   1.2019697E 00
C ERR =  -9.66E-09
C TEST OF  GQXAB
C ABSCISSAS AND WEIGHTS FOR N=5
C   1  -0.8412535E 00   0.9067575E-01
C   2  -0.4154150E 00   0.3339142E 00
C   3   0.1423148E 00   0.6524887E 00
C   4   0.6548607E 00   0.9452542E 00
C   5   0.9594930E 00   0.1119260E 01
C SAMPLE PROBLEM
C TRUE=   1.3277933E 00
C CALC=   1.3277914E 00
C ERR =   1.88E-06