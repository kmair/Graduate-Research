C$SEQ CSPA 0 20
C$TEST CSPA
C***********************************************************************
C
C  EXAMPLE OF USE OF THE PORT PROGRAM CSPQU
C
C***********************************************************************
      INTEGER NPTS,J,IWRITE,I1MACH
      REAL PI,X(65),Y(65),ANS,ZZI
      PI=3.14159265
C
      NPTS=9
C
C COMPUTE THE POINTS AT WHICH THE SPLINE IS TO BE FITTED
C
      DO 10 J=1,NPTS
      X(J)=FLOAT(J-1)/FLOAT(NPTS-1)
      Y(J)=SIN(X(J)*PI/2.)
   10 CONTINUE
C
C THE INTEGRATION:
C
      CALL CSPQU(X,Y,NPTS,X(1),X(NPTS),ANS)
C
C ERROR IN INTEGRATION
C
      ZZI=ANS-2./PI
C
C
C SET THE OUTPUT UNIT
C
      IWRITE=I1MACH(2)
C
      WRITE (IWRITE,9998) ANS
 9998 FORMAT(48H THE INTEGRAL OF SINE(X*PI/2) FROM X=0 TO X=1 IS,E16.8)
C
      WRITE (IWRITE,9999) ZZI
 9999 FORMAT(17H WITH AN ERROR OF,1PE10.2)
C
      STOP
      END