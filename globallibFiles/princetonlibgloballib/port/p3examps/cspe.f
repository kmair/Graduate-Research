C$SEQ CSPE 0 20
C$TEST CSPE
C***********************************************************************
C
C  EXAMPLE OF USE OF THE PORT PROGRAM CSPDI
C
C***********************************************************************
       INTEGER IWRITE,I1MACH,J,K
       REAL PI,X(9),Y(9),YY(9),XX(4),YYP(4),ZZ(4),ZZD(4)
C
       PI=3.14159265
C
C COMPUTE THE POINTS AT WHICH THE SPLINE IS TO BE FITTED
C
       DO 10 J=1,9
       X(J)=FLOAT(J-1)/8.
       Y(J)=SIN(X(J)*PI/2.)
   10  CONTINUE
C
C
C SET THE POINTS AT WHICH THE INTERPOLATION AND
C DIFFERENTIATION ARE TO BE DONE
C
       XX(1)=.1
       XX(2)=.3
       XX(3)=.6
       XX(4)=.9
C
C THE INTERPOLATION:
C
       CALL CSPIN(X,Y,9,XX,YY,4)
C
C COMPUTE THE INTERPOLATION ERROR
C
       DO 20 K=1,4
   20  ZZ(K)=YY(K)-SIN(XX(K)*PI/2.)
C
C THE DIFFERENTIATION:
C
       CALL CSPDI(X,Y,9,XX,YY,YYP,4)
C
C COMPUTE THE DIFFERENTIATION ERROR
C
       DO 30 K=1,4
   30  ZZD(K)=(2./PI)*YYP(K)-COS(XX(K)*PI/2.)
C
C
C SET THE OUTPUT UNIT
C
       IWRITE=I1MACH(2)
C
       WRITE (IWRITE,9997)
 9997     FORMAT(2X,2HXX,10X,13HINTERPOLATION,9X,15HDIFFERENTIATION/)
C
       WRITE (IWRITE,9998)
 9998     FORMAT(12X,5HVALUE,6X,5HERROR,7X,5HVALUE,6X,6H ERROR//)
C
       WRITE (IWRITE,9999)(XX(K),YY(K),ZZ(K),YYP(K),ZZD(K), K=1,4)
 9999     FORMAT(0PF6.3,0PF12.6,1PE12.3,0PF12.6,1PE12.3/)
C
       STOP
       END