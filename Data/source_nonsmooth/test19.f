* SUBROUTINE TIUD19                ALL SYSTEMS                99/12/01
C PORTABILITY : ALL SYSTEMS
C 94/12/01 VL : ORIGINAL VERSION
*
* PURPOSE :
*  INITIATION OF VARIABLES FOR NONSMOOTH OPTIMIZATION.
*  UNCONSTRAINED DENSE VERSION.
*
* PARAMETERS :
*  II  N  NUMBER OF VARIABLES.
*  RO  X(N)  VECTOR OF VARIABLES.
*  RO  FMIN  LOWER BOUND FOR VALUE OF THE OBJECTIVE FUNCTION.
*  RO  XMAX  MAXIMUM STEPSIZE.
*  II  NEXT  NUMBER OF THE TEST PROBLEM.
*  IO  IERR  ERROR INDICATOR.
*
      SUBROUTINE TIUD19(N,X,FMIN,XMAX,NEXT,IERR)
      INTEGER N,NEXT,IERR,I,J,K,KK,L,COUNT
      REAL*8 X(N),FMIN,XMAX,ETA9,Y(2700),AI,AJ,AK,XR(2700)
      INTEGER*2 AA(59),PP(23),CC(95)
      COMMON /EMPR19/ Y
      PARAMETER (ETA9=1.0D 60)
*  MODIFIED BY LUIS M RIOS
*  NX IS A DUMMY VARIABLE, SINCE THE VALUE OF N IS PROVIDED IN THE MAIN PROGRAM      
      INTEGER NX
      DATA AA/5*0,2,3*1,3,1,2,1,1,2,1,4,1,2,2,3,2,1,0,1,0,2,1,0,7*1,0,1
     &,2,1,0,0,2,1,0,1,1,2,0,0,1,5,10,2,4,3,3*6/
      DATA PP/0,2,3,4,5,6,2,3,-1,4*2,1,1,5,4*1,2,3,2/
      DATA CC/-16,4*0,2,2*-1,2*1,2,-2,0,-2,-9,0,-1,-2,2,1,
     & 2*0,2,0,-2,-4,-1,-3,3,1,1,4,0,-4,1,0,-1,-2,4,1,
     & 0,2,0,-1,2*0,2*-1,5,1,-40,-2,0,2*-4,-1,-40,-60,5,1,
     & 30,-20,-10,32,-10,-20,39,-6,-31,32,-10,-6,10,-6,-10,
     &32,-31,-6,39,-20,-10,32,-10,-20,30,4,8,10,6,2,-15,-27,-36,-18,-12/
      FMIN=0.0D 0
      XMAX=1.0D 3
      IERR=0
      COUNT = 0
      IF (N.LT.2) GO TO 999
      DO 1 I=1,N
      X(I)=0D 0
   1  CONTINUE
      GO TO (10,20,30,40,50,60,70,80,90,180,100,110,230,240,170,120,
     & 210,220,130,130,150,160,190,190,250), NEXT
   10 NX=2
      X(1)=-1.2D 0
      X(2)=1.0D 0
      RETURN
   20 NX=2
      X(1)=-1.5D 0
      X(2)=2.0D 0
      RETURN
   30 NX=2
      X(1)=1.0D 0
      X(2)=-0.1D 0
      RETURN
   40 NX=2
      X(1)=2.0D 0
      X(2)=2.0D 0
      RETURN
   50 NX=2
   51 FMIN=-ETA9
   52 DO 53 I=1,N
      X(I)=1.0D 0
   53 CONTINUE
      RETURN
   60 NX=2
      X(1)=-1.0D 0
      X(2)=5.0D 0
      RETURN
   70 NX=2
      X(1)=-0.5D 0
      X(2)=-0.5D 0
  777 FMIN=-ETA9
      RETURN
   80 NX=2
      X(1)=0.8D 0
      X(2)=0.6D 0
      GO TO 777
   90 NX=2
      X(1)=-1.0D 0
      X(2)=-1.0D 0
      GO TO 777
  180 NX=2
      X(1)=3.0D 0
      X(2)=2.0D 0
      GO TO 777
  100 IF (N.LT.4) GO TO 999
      NX=4
      GO TO 777
  110 IF (N.LT.5) GO TO 999
      NX=5
      X(5)=1.0D 0
      DO 111 I=1,59
      Y(I)=AA(I)
  111 CONTINUE
      Y(57)=1.7D 0
      Y(58)=2.5D 0
      Y(60)=3.5D 0
      RETURN
  230 IF (N.LT.5) GO TO 999
      NX=5
      X(5)=1.0D 0
      FMIN=-ETA9
  232 DO 233 I=1,95
      Y(I)=CC(I)
  233 CONTINUE
      Y(3)=-3.5D 0
      Y(45)=-2.8D 0
      Y(53)=-.25D 0
      RETURN
  240 IF (N.LT.5) GO TO 999
      NX=5
      X(1)=-2.0D 0
      X(2)= 1.5D 0
      X(3)= 2.0D 0
      X(4)=-1.0D 0
      X(5)=-1.0D 0
      FMIN=-ETA9
      XMAX= 1.0D 0
      RETURN
  170 IF (N.LT.6) GO TO 999
      NX=6
      XMAX=2D 0
      X(1)=2.0D 0
      X(2)=2.0D 0
      X(3)=7.0D 0
      X(5)=-2.0D 0
      X(6)=1.0D 0
      RETURN
  120 IF (N.LT.10) GO TO 999
      NX=10
      KK=0
      DO 122 K=1,5
      AK=DBLE(K)
      DO 123 I=1,N
      AI=DBLE(I)
      DO 124 J=I,N
      AJ=DBLE(J)
      Y(KK+(I-1)*N+J)=EXP(AI/AJ)*COS(AI*AJ)*SIN(AK)
      Y(KK+(J-1)*N+I)=Y(KK+(I-1)*N+J)
  124 CONTINUE
  123 CONTINUE
      DO 126 I=1,N
      AI=DBLE(I)
      Y(100+KK+I)=EXP(AI/AK)*SIN(AI*AK)
      L=KK+(I-1)*N+I
      Y(L)=ABS(SIN(AK))*AI/DBLE(N)
      DO 126 J=1,N
      IF (J.NE.I) Y(L)=Y(L)+ABS(Y(KK+(I-1)*N+J))
  126 CONTINUE
      KK=KK+110
  122 CONTINUE
      GO TO 51
  210 IF (N.LT.10) GO TO 999
      NX=10
      XMAX=1.0D 1
      DO 211 I=1,N
      X(I)=-0.1D 0
  211 CONTINUE
      RETURN
  220 IF (N.LT.12) GO TO 999
      NX=12
      DO 221 I=1,23
      Y(I)=PP(I)
  221 CONTINUE
      Y(10)=-0.5D 0
      X(1)=2.0D 0/3.0D 0
      X(7)=5.0D 0/3.0D 0
      DO 222 I=2,5
      X(I)=(X(I-1)+Y(I)+Y(I+1))/3.0D 0
      X(I+6)=(X(I+5)+Y(I+6)+Y(I+7))/3.0D 0
  222 CONTINUE
      X(6)=(X(5)+11.5D 0)/3.0D 0
      X(12)=(X(11)+1.0D 0)/3.0D 0
      RETURN
  130 IF (N.LT.20) GO TO 999
      NX=20
      IF (NEXT.EQ.19) XMAX= 5.0D 0
      IF (NEXT.EQ.20) XMAX= 1.0D 1
      DO 131 I=1,10
      X(I)=DBLE(I)
      X(I+10)=DBLE(-I-10)
  131 CONTINUE
      RETURN
  150 IF (N.LT.48) GO TO 999
      NX=48
      OPEN(5,FILE='TEST19.DAT')
C      READ(777,*)((Y(N*(I-2)+J),J=I,N),I=2,N)
C     & ,(Y(N*N+I),I=1,N),(Y(N*N+N+I),I=1,N)
      DO 980 I=1,70      
      READ(5,*) XR(1+COUNT), XR(2+COUNT), XR(3+COUNT), XR(4+COUNT)
     + ,XR(5+COUNT), XR(6+COUNT), XR(7+COUNT), XR(8+COUNT)
     + ,XR(9+COUNT), XR(10+COUNT), XR(11+COUNT), XR(12+COUNT)
     + ,XR(13+COUNT), XR(14+COUNT), XR(15+COUNT), XR(16+COUNT)
      COUNT = COUNT + 16
  980 CONTINUE
      READ(5,*) XR(1+COUNT), XR(2+COUNT), XR(3+COUNT), XR(4+COUNT)
     + ,XR(5+COUNT), XR(6+COUNT), XR(7+COUNT), XR(8+COUNT)
      COUNT = 1
      DO 981 I=2,N
      DO 982 J=I,N
C      READ(5,*) Y(N*(I-2)+J)
      Y(N*(I-2)+J) = XR(COUNT)
      COUNT = COUNT + 1
  982 CONTINUE
  981 CONTINUE
      COUNT = 0
      READ(5,*) XR(1+COUNT), XR(2+COUNT), XR(3+COUNT), XR(4+COUNT)
     + ,XR(5+COUNT), XR(6+COUNT), XR(7+COUNT), XR(8+COUNT)
     + ,XR(9+COUNT), XR(10+COUNT), XR(11+COUNT), XR(12+COUNT)
     + ,XR(13+COUNT), XR(14+COUNT), XR(15+COUNT), XR(16+COUNT)
     + ,XR(17+COUNT), XR(18+COUNT), XR(19+COUNT), XR(20+COUNT)
     + ,XR(21+COUNT), XR(22+COUNT), XR(23+COUNT), XR(24+COUNT)
      COUNT = COUNT + 24
      READ(5,*) XR(1+COUNT), XR(2+COUNT), XR(3+COUNT), XR(4+COUNT)
     + ,XR(5+COUNT), XR(6+COUNT), XR(7+COUNT), XR(8+COUNT)
     + ,XR(9+COUNT), XR(10+COUNT), XR(11+COUNT), XR(12+COUNT)
     + ,XR(13+COUNT), XR(14+COUNT), XR(15+COUNT), XR(16+COUNT)
     + ,XR(17+COUNT), XR(18+COUNT), XR(19+COUNT), XR(20+COUNT)
     + ,XR(21+COUNT), XR(22+COUNT), XR(23+COUNT), XR(24+COUNT)
      COUNT = 1
      DO 983 I=1,N
C      READ(5,*) Y(N*N+I)
      Y(N*N+I) = XR(COUNT)
      COUNT = COUNT + 1
  983 CONTINUE
      COUNT = 0
      READ(5,*) XR(1+COUNT), XR(2+COUNT), XR(3+COUNT), XR(4+COUNT)
     + ,XR(5+COUNT), XR(6+COUNT), XR(7+COUNT), XR(8+COUNT)
     + ,XR(9+COUNT), XR(10+COUNT), XR(11+COUNT), XR(12+COUNT)
     + ,XR(13+COUNT), XR(14+COUNT), XR(15+COUNT), XR(16+COUNT)
     + ,XR(17+COUNT), XR(18+COUNT), XR(19+COUNT), XR(20+COUNT)
     + ,XR(21+COUNT), XR(22+COUNT), XR(23+COUNT), XR(24+COUNT)
      COUNT = COUNT + 24
      READ(5,*) XR(1+COUNT), XR(2+COUNT), XR(3+COUNT), XR(4+COUNT)
     + ,XR(5+COUNT), XR(6+COUNT), XR(7+COUNT), XR(8+COUNT)
     + ,XR(9+COUNT), XR(10+COUNT), XR(11+COUNT), XR(12+COUNT)
     + ,XR(13+COUNT), XR(14+COUNT), XR(15+COUNT), XR(16+COUNT)
     + ,XR(17+COUNT), XR(18+COUNT), XR(19+COUNT), XR(20+COUNT)
     + ,XR(21+COUNT), XR(22+COUNT), XR(23+COUNT), XR(24+COUNT)
      COUNT = 1
      DO 984 I=1,N
C      READ(5,*) Y(N*N+N+I)
      Y(N*N+N+I) = XR(COUNT)
      COUNT = COUNT + 1
  984 CONTINUE
      DO 151 I=1,N
      Y(N*(I-1)+I)=1.0D 5
      DO 151 J=1,I-1
      Y(N*(I-1)+J)=Y(N*(J-1)+I)
  151 CONTINUE
      CLOSE(5)
      GO TO 777
  160 IF (N.LT.50) GO TO 999
      NX=50
      XMAX=1.0D 1
      DO 161 I=1,N
      X(I)=DBLE(I)-25.5D 0
  161 CONTINUE
      RETURN
  190 IF (N.LT.30) GO TO 999
      IF (N.GE.50) NX=50
      IF (N.LT.50) NX=30
      IF (NEXT.EQ.23) XMAX= 5.0D 0
      GO TO 52
  250 IF (N.LT.15) GO TO 999
      NX=15
      DO 251 I=1,N
      X(I)=1.0D-4
  251 CONTINUE
      X(12)=6.0D 1
      GO TO 232
  999 IERR=1
      RETURN
      END
* SUBROUTINE TFFU19                ALL SYSTEMS                99/12/01
C PORTABILITY : ALL SYSTEMS
C 94/12/01 RA : ORIGINAL VERSION
*
* PURPOSE :
*  VALUE OF THE NONSMOOTH OBJECTIVE FUNCTION.
*
* PARAMETERS :
*  II  N  NUMBER OF VARIABLES.
*  RI  X(N)  VECTOR OF VARIABLES.
*  RO  F  VALUE OF THE OBJECTIVE FUNCTION.
*  II  NEXT  NUMBER OF THE TEST PROBLEM.
*
      SUBROUTINE TFFU19(N,X,F,NEXT)
      INTEGER N,NEXT
      REAL*8 X(N),F
      REAL*8 AI,AJ,ETA9,F1,F2,F3,F4,T,Y(2700),Z
      INTEGER I,J,KK,LL,K
      COMMON /EMPR19/ Y
      PARAMETER (ETA9=1.0D 60)
      F=0.0D 0
      GO TO (10,20,30,40,50,60,70,80,90,180,100,110,230,240,170,120,
     & 210,220,130,140,150,160,190,200,250), NEXT
   10 F=100.0D 0*(X(2)-X(1)**2)**2+(1.0D 0-X(1))**2
      RETURN
   20 T=X(1)**2+(X(2)-1.0D 0)**2-1.0D 0
      F=X(2)+ABS(T)
      RETURN
   30 F1=X(1)**2+X(2)**4
      F2=(2.0D 0-X(1))**2+(2.0D 0-X(2))**2
      F3=2.0D 0*EXP(-X(1)+X(2))
      F=MAX(F1,F2,F3)
      RETURN
   40 F1=X(1)**4+X(2)**2
      F2=(2.0D 0-X(1))**2+(2.0D 0-X(2))**2
      F3=2.0D 0*EXP(-X(1)+X(2))
      F=MAX(F1,F2,F3)
      RETURN
   50 F1=5.0D 0*X(1)+X(2)
      F2=-5.0D 0*X(1)+X(2)
      F3=X(1)**2+X(2)**2+4.0D 0*X(2)
      F=MAX(F1,F2,F3)
      RETURN
   60 F1=X(1)**2+X(2)**2
      F2=F1+10.0D 0*(-4.0D 0*X(1)-X(2)+4.0D 0)
      F3=F1+10.0D 0*(-X(1)-2.0D 0*X(2)+6.0D 0)
      F=MAX(F1,F2,F3)
      RETURN
   70 F1=-X(1)-X(2)
      F2=F1+(X(1)**2+X(2)**2-1.0D 0)
      F=MAX(F1,F2)
      RETURN
   80 F1=X(1)**2+X(2)**2-1.0D 0
      F2=0.0D 0
      F=MAX(F1,F2)
      F=20.0D 0*F-X(1)
      RETURN
   90 F1=X(1)**2+X(2)**2-1.0D 0
      IF (F1.LT.0.0D 0) F1=-F1
      F=-X(1)+2.0D 0*(X(1)**2+X(2)**2-1.0D 0)+1.75D 0*F1
      RETURN
  180 IF (X(1).GT.ABS(X(2))) THEN
      F=5.0D 0*SQRT(9.0D 0*X(1)**2+16.0D 0*X(2)**2)
      ELSEIF ((X(1).GT.0.0D 0).AND.(X(1).LE.ABS(X(2)))) THEN
      F=9.0D 0*X(1)+16.0D 0*ABS(X(2))
      ELSEIF (X(1).LE.0.0D 0) THEN
      F=9.0D 0*X(1)+16.0D 0*ABS(X(2))-X(1)**9
      ENDIF
      RETURN
  100 F=X(1)**2+X(2)**2+X(3)**2
      F1=F+X(3)**2+X(4)**2-5.0D 0*(X(1)+X(2))-21.0D 0*X(3)+7.0D 0*X(4)
      F2=F+X(4)**2+X(1)-X(2)+X(3)-X(4)-8.0D 0
      F3=F+X(2)**2+2.0D 0*X(4)**2-X(1)-X(4)-10.0D 0
      F4=F+2.0D 0*X(1)-X(2)-X(4)-5.0D 0
      F=F1+10.0D 0*MAX(0.0D 0,F2,F3,F4)
      RETURN
  110 F=0.0D 0
      DO 111 J=1,5
      F=F+(X(J)-Y(J))**2
  111 CONTINUE
      F=F*Y(50+1)
      DO 112 I=2,10
      F1=0.0D 0
      DO 113 J=1,5
      F1=F1+(X(J)-Y(5*(I-1)+J))**2
  113 CONTINUE
      F1=F1*Y(50+I)
      F=MAX(F,F1)
  112 CONTINUE
      RETURN
  230 F=0.0D 0
      DO 231 I=1,10
      T=Y(50+I)
      DO 232 J=1,5
      T=T-Y(I+J*10-10)*X(J)
  232 CONTINUE
      F=MAX(F,T)
  231 CONTINUE
      F=F*5D 1
      DO 234 J=1,5
      T=0D 0
      DO 233 I=1,5
      T=T+Y(55+I+J*5)*X(I)
  233 CONTINUE
      F=F+Y(85+J)*X(J)**3+Y(90+J)*X(J)+T*X(J)
  234 CONTINUE
      RETURN
  240 F1=X(1)*X(2)*X(3)*X(4)*X(5)
      F2=ABS(X(1)**2+X(2)**2+X(3)**2+X(4)**2+X(5)**2-1.0D 1)
      F2=F2+ABS(X(2)*X(3)-5.0D 0*X(4)*X(5))
      F2=F2+ABS(X(1)**3+X(2)**3+1.0D 0)
      F=F1+1.0D 1*F2
      RETURN
  170 F=0.0D 0
      DO 171 I=1,51
      T=DBLE(I-1)/10.0D 0
      Z=0.5D 0*EXP(-T)-EXP(-T*2.0D 0)+0.5D 0*EXP(-T*3.0D 0)+
     & 1.5D 0*EXP(-T*1.5D 0)*SIN(7.0D 0*T)+
     & EXP(-T*2.5D 0)*SIN(5.0D 0*T)
      F1=EXP(-X(2)*T)
      F2=EXP(-X(6)*T)
      F3=COS(X(3)*T+X(4))
      F=F+ABS(X(1)*F1*F3+X(5)*F2-Z)
  171 CONTINUE
      RETURN
  120 F=0.0D 0
      K=1
      DO 121 I=1,N
      F=F-Y(100+I)*X(I)
      DO 122 J=1,N
      F=F+Y((I-1)*N+J)*X(I)*X(J)
  122 CONTINUE
  121 CONTINUE
      KK=110
      DO 123 K=2,5
      F1=0.0D 0
      DO 124 I=1,N
      F1=F1-Y(KK+100+I)*X(I)
      DO 125 J=1,N
      F1=F1+Y(KK+(I-1)*N+J)*X(I)*X(J)
  125 CONTINUE
  124 CONTINUE
      F=MAX(F,F1)
      KK=KK+110
  123 CONTINUE
      RETURN
  210 F1=0.0D 0
      DO 211 I=1,10
      F1=F1+X(I)**2
  211 CONTINUE
      F4=F1-0.25D 0
      F1=1.0D -3*F4*F4
      DO 212 I=1,10
      F1=F1+(X(I)-1.0D 0)**2
  212 CONTINUE
      F2=0.0D 0
      DO 215 I=2,30
      AI=DBLE(I-1)/29D 0
      F=0.0D 0
      DO 213 J=1,10
      F=F+X(J)*AI**(J-1)
  213 CONTINUE
      F=-F*F-1.0D 0
      DO 214 J=2,10
      AJ=DBLE(J-1)
      F=F+X(J)*AJ*AI**(J-2)
  214 CONTINUE
      F2=F2+F*F
  215 CONTINUE
      F2=F2+X(1)**2+(X(2)-X(1)**2-1.0D 0)**2
      F3=0.0D 0
      DO 216 I=2,10
      F3=F3+100.0D 0*(X(I)-X(I-1)**2)**2+(1.0D 0-X(I))**2
  216 CONTINUE
      F=MAX(F1,F2,F3)
      RETURN
  220 F=SQRT(X(1)**2+X(7)**2)+
     & SQRT((5.5D 0-X(6))**2+(1.0D 0+X(12))**2)
      DO 221 J=1,6
      F=F+Y(12+J)*SQRT((Y(J)-X(J))**2+(Y(6+J)-X(6+J))**2)
  221 CONTINUE
      DO 222 J=1,5
      F=F+Y(18+J)*SQRT((X(J)-X(J+1))**2+(X(J+6)-X(J+7))**2)
  222 CONTINUE
      RETURN
  130 F=X(1)**2
      DO 131 I=2,20
      F1=X(I)**2
      F=MAX(F,F1)
  131 CONTINUE
      RETURN
  140 F=ABS(X(1))
      DO 141 I=2,20
      F1=ABS(X(I))
      F=MAX(F,F1)
  141 CONTINUE
      RETURN
  150 NX=48
      F=0.0D 0
      KK=N*N
      LL=N*N+N
      DO 151 I=1,N
      F=F+Y(LL+I)*X(I)
  151 CONTINUE
      DO 152 J=1,N
      Z=ETA9
      DO 153 I=1,N
      T=Y((I-1)*N+J)-X(I)
      IF (T.GE.Z) GO TO 153
      Z=T
      K=I
  153 CONTINUE
      F=F+Y(KK+J)*Z
  152 CONTINUE
      F=-F
      RETURN
  160 F1=0.0D 0
      F=-ETA9
      DO 161 I=1,50
      F2=X(I)
      F1=F1+F2
      F=MAX(F,F2)
  161 CONTINUE
      F=5.0D 1*F-F1
      RETURN
  190 F=0.0D 0
      DO 191 J=1,N
      F=F+X(J)/DBLE(1+J-1)
  191 CONTINUE
      F=ABS(F)
      DO 192 I=2,N
      F1=0.0D 0
      DO 193 J=1,N
      F1=F1+X(J)/DBLE(I+J-1)
  193 CONTINUE
      F1=ABS(F1)
      F=MAX(F1,F)
  192 CONTINUE
      RETURN
  200 F=0.0D 0
      DO 202 J=1,N
      F1=0.0D 0
      DO 203 I=1,N
      F1=F1+X(I)/DBLE(I+J-1)
  203 CONTINUE
      F1=ABS(F1)
      F=F+F1
  202 CONTINUE
      RETURN
  250 F1=0.0D 0
      DO 251 J=1,5
      F1=F1+Y(85+J)*X(J)**3
  251 CONTINUE
      F=ABS(F1+F1)
      DO 253 J=1,5
      T=0.0D 0
      DO 252 I=1,5
      T=T+Y(55+I+J*5)*X(I)
  252 CONTINUE
      F=F+T*X(J)
  253 CONTINUE
      DO 254 J=6,15
      F=F-Y(45+J)*X(J)
  254 CONTINUE
      DO 257 J=1,5
      T=-3.0D 0*Y(85+J)*X(J)*X(J)-Y(90+J)
      DO 255 I=1,15
      IF (I.LE.5) T=T-2.0D 0*Y(55+I+J*5)*X(I)
      IF (I.GT.5) T=T+Y(I+J*10-15)*X(I)
  255 CONTINUE
      IF (T.GT.0.0D 0) F=F+1.0D 2*T
  257 CONTINUE
      DO 258 I=1,15
      IF (X(I).LT.0.0D 0) F=F-1.0D 2*X(I)
  258 CONTINUE
      RETURN
      END
* SUBROUTINE TFGU19                ALL SYSTEMS                99/12/01
C PORTABILITY : ALL SYSTEMS
C 94/12/01 VL : ORIGINAL VERSION
*
* PURPOSE :
*  GRADIENT OF THE NONSMOOTH OBJECTIVE FUNCTION.
*
* PARAMETERS :
*  II  N  NUMBER OF VARIABLES.
*  RI  X(N)  VECTOR OF VARIABLES.
*  RO  G(N)  GRADIENT OF THE OBJECTIVE FUNCTION.
*  II  NEXT  NUMBER OF THE TEST PROBLEM.
*
      SUBROUTINE TFGU19(N,X,G,NEXT)
      INTEGER N,NEXT,I,J,K,KK,L
      REAL*8 X(N),G(N),AI,AJ,ETA9,F1,F2,F3,F4,F,T,Y(2700)
      COMMON /EMPR19/ Y
      PARAMETER (ETA9=1.0D 60)
      DO 1 I=1,N
      G(I)=0.0D 0
    1 CONTINUE
      GO TO (10,20,30,30,50,60,70,80,90,180,100,110,230,240,170,120,
     & 210,220,130,140,150,160,190,200,250), NEXT
   10 G(2)=200.0D 0*(X(2)-X(1)**2)
      G(1)=2.0D 0*X(1)*(1.0D 0-G(2))-2.0D 0
      RETURN
   20 T=X(1)**2+(X(2)-1.0D 0)**2-1.0D 0
      F=SIGN(2.0D 0,T)
      G(1)=F*X(1)
      G(2)=F*(X(2)-1.0D 0)+1.0D 0
      RETURN
   30 I=NEXT-2
      J=5-NEXT
      F1=X(I)**2+X(J)**4
      F2=(2.0D 0-X(1))**2+(2.0D 0-X(2))**2
      F3=2.0D 0*EXP(-X(1)+X(2))
      IF ((F1.GE.F2).AND.(F1.GE.F3)) THEN
        G(I)=2.0D 0*X(I)
        G(J)=4.0D 0*X(J)**3
      ELSEIF ((F2.GE.F1).AND.(F2.GE.F3)) THEN
        G(1)=-2.0D 0*(2.0D 0-X(1))
        G(2)=-2.0D 0*(2.0D 0-X(2))
      ELSE
        G(2)=2.0D 0*EXP(-X(1)+X(2))
        G(1)=-G(2)
      ENDIF
      RETURN
   50 F1=5.0D 0*X(1)+X(2)
      F2=-5.0D 0*X(1)+X(2)
      F3=X(1)**2+X(2)**2+4.0D 0*X(2)
      G(2)=1.0D 0
      IF ((F1.GE.F2).AND.(F1.GE.F3)) THEN
        G(1)=5.0D 0
      ELSEIF ((F2.GE.F1).AND.(F2.GE.F3)) THEN
        G(1)=-5.0D 0
      ELSE
        G(1)=2.0D 0*X(1)
        G(2)=2.0D 0*X(2)+4.0D 0
      ENDIF
      RETURN
   60 F1=X(1)**2+X(2)**2
      F2=F1+10.0D 0*(-4.0D 0*X(1)-X(2)+4.0D 0)
      F3=F1+10.0D 0*(-X(1)-2.0D 0*X(2)+6.0D 0)
      G(1)=2.0D 0*X(1)
      G(2)=2.0D 0*X(2)
      IF ((F1.GE.F2).AND.(F1.GE.F3)) THEN
      ELSEIF ((F2.GE.F1).AND.(F2.GE.F3)) THEN
        G(1)=G(1)-40.0D 0
        G(2)=G(2)-10.0D 0
      ELSE
        G(1)=G(1)-10.0D 0
        G(2)=G(2)-20.0D 0
      ENDIF
      RETURN
   70 F1=-X(1)-X(2)
      F2=F1+(X(1)**2+X(2)**2-1.0D 0)
      IF (F1.GE.F2) THEN
        G(1)=-1.0D 0
        G(2)=-1.0D 0
      ELSE
        G(1)=-1.0D 0+2.0D 0*X(1)
        G(2)=-1.0D 0+2.0D 0*X(2)
      ENDIF
      RETURN
   80 F1=X(1)**2+X(2)**2-1.0D 0
      G(1)=-1.0D 0
      IF (F1.GE.0.0D 0) THEN
        G(1)=40.0D 0*X(1)-1.0D 0
        G(2)=40.0D 0*X(2)
      ENDIF
      RETURN
   90 F1=SIGN(3.5D 0,X(1)**2+X(2)**2-1.0D 0)+4.0D 0
      G(1)=F1*X(1)-1.0D 0
      G(2)=F1*X(2)
      RETURN
  180 IF (X(1).GT.ABS(X(2))) THEN
        G(1)=45.0D 0*X(1)/SQRT(9.0D 0*X(1)**2+16.0D 0*X(2)**2)
        G(2)=80.0D 0*X(2)/SQRT(9.0D 0*X(1)**2+16.0D 0*X(2)**2)
      ELSE
        G(1)=9.0D 0
        IF (X(1).LT.0.0D 0) G(1)=9.0D 0-9.0D 0*X(1)**8
        G(2)=SIGN(16.0D 0,X(2))
      ENDIF
      RETURN
  100 F=X(1)**2+X(2)**2+X(3)**2
      F2=F+X(4)**2+X(1)-X(2)+X(3)-X(4)-8.0D 0
      F3=F+X(2)**2+2.0D 0*X(4)**2-X(1)-X(4)-10.0D 0
      F4=F+2.0D 0*X(1)-X(2)-X(4)-5.0D 0
      L=1
      IF (F2.GT.0.0D 0) L=2
      IF (F3.GT.MAX(F2,0.0D 0)) L=3
      IF (F4.GT.MAX(F2,F3,0.0D 0)) L=4
      GO TO (101,102,103,104),L
  101 G(1)=2.0D 0*X(1)-5.0D 0
      G(2)=2.0D 0*X(2)-5.0D 0
      G(3)=4.0D 0*X(3)-21.0D 0
      G(4)=2.0D 0*X(4)+7.0D 0
      RETURN
  102 G(1)=22.0D 0*X(1)+5.0D 0
      G(2)=22.0D 0*X(2)-15.0D 0
      G(3)=24.0D 0*X(3)-11.0D 0
      G(4)=22.0D 0*X(4)-3.0D 0
      RETURN
  103 G(1)=22.0D 0*X(1)-15.0D 0
      G(2)=42.0D 0*X(2)-5.0D 0
      G(4)=42.0D 0*X(4)-3.0D 0
      GO TO 105
  104 G(1)=22.0D 0*X(1)+15.0D 0
      G(2)=22.0D 0*X(2)-15.0D 0
      G(4)=2.0D 0*X(4)-3.0D 0
  105 G(3)=24.0D 0*X(3)-21.0D 0
      RETURN
  110 F=-ETA9
      DO 112 I=1,10
      F1=0.0D 0
      DO 113 J=1,5
      F1=F1+(X(J)-Y(5*(I-1)+J))**2
  113 CONTINUE
      F1=F1*Y(50+I)
      IF (F.LT.F1) K=I
      F=MAX(F,F1)
  112 CONTINUE
      DO 114 J=1,5
      G(J)=2.0D 0*Y(50+K)*(X(J)-Y(5*(K-1)+J))
  114 CONTINUE
      RETURN
  230 F1=0.0D 0
      DO 231 I=1,10
      T=Y(50+I)
      DO 232 J=1,5
      T=T-Y(I+J*10-10)*X(J)
  232 CONTINUE
      IF (T.GT.F1) K=I
      F1=MAX(F1,T)
  231 CONTINUE
      DO 234 J=1,5
      T=0.0D 0
      DO 233 I=1,5
      T=T+Y(55+I+J*5)*X(I)
  233 CONTINUE
      G(J)=3.0D 0*Y(85+J)*X(J)*X(J)+T+T+Y(90+J)
      IF (F1.GT.0.0D 0) G(J)=G(J)-5.0D 1*Y(K+J*10-10)
  234 CONTINUE
      RETURN
  240 G(1)=X(2)*X(3)*X(4)*X(5)
      G(2)=X(1)*X(3)*X(4)*X(5)
      G(3)=X(1)*X(2)*X(4)*X(5)
      G(4)=X(1)*X(2)*X(3)*X(5)
      G(5)=X(1)*X(2)*X(3)*X(4)
      F1=X(1)**2+X(2)**2+X(3)**2+X(4)**2+X(5)**2-1.0D 1
      F4=1.0D 0
      IF (F1.LT.0.0D 0) F4=-F4
      DO 241 I=1,5
      G(I)=G(I)+2.0D 1*F4*X(I)
  241 CONTINUE
      F2=X(2)*X(3)-5.0D 0*X(4)*X(5)
      F4=1.0D 0
      IF (F2.LT.0.0D 0) F4=-F4
      G(2)=G(2)+1.0D 1*F4*X(3)
      G(3)=G(3)+1.0D 1*F4*X(2)
      G(4)=G(4)-5.0D 1*F4*X(5)
      G(5)=G(5)-5.0D 1*F4*X(4)
      F3=X(1)**3+X(2)**3+1.0D 0
      F4=1.0D 0
      IF (F3.LT.0.0D 0) F4=-F4
      G(1)=G(1)+3.0D 1*F4*X(1)**2
      G(2)=G(2)+3.0D 1*F4*X(2)**2
      RETURN
  170 DO 172 I=1,51
      T=DBLE(I-1)/10.0D 0
      F=0.5D 0*EXP(-T)-EXP(-T*2.0D 0)+0.5D 0*EXP(-T*3.0D 0)+
     & 1.5D 0*EXP(-T*1.5D 0)*SIN(7.0D 0*T)+
     & EXP(-T*2.5D 0)*SIN(5.0D 0*T)
      F1=EXP(-X(2)*T)
      F2=EXP(-X(6)*T)
      F3=COS(X(3)*T+X(4))
      F4=SIN(X(3)*T+X(4))
      AI=SIGN(1.0D 0,X(1)*F1*F3+X(5)*F2-F)
      G(1)=G(1)+AI*F1*F3
      G(2)=G(2)-AI*F1*F3*X(1)*T
      G(3)=G(3)-AI*F1*F4*X(1)*T
      G(4)=G(4)-AI*F1*F4*X(1)
      G(5)=G(5)+AI*F2
      G(6)=G(6)-AI*F2*X(5)*T
  172 CONTINUE
      RETURN
  120 F=-ETA9
      L=1
      KK=0
      DO 123 K=1,5
      F1=0.0D 0
      DO 122 I=1,N
      F1=F1-Y(KK+100+I)*X(I)
      DO 122 J=1,N
      F1=F1+Y(KK+(I-1)*N+J)*X(I)*X(J)
  122 CONTINUE
      IF (F.LT.F1) L=K
      F=MAX(F,F1)
      KK=KK+110
  123 CONTINUE
      DO 126 I=1,N
      G(I)=-Y((L-1)*110+100+I)
      DO 126 J=1,N
      G(I)=G(I)+2.0D 0*Y((L-1)*110+(I-1)*N+J)*X(J)
  126 CONTINUE
      RETURN
  210 F1=0.0D 0
      DO 211 I=1,10
      F1=F1+X(I)**2
  211 CONTINUE
      F4=F1-0.25D 0
      F1=1.0D -3*F4*F4
      DO 212 I=1,10
      F1=F1+(X(I)-1.0D 0)**2
  212 CONTINUE
      F2=0.0D 0
      DO 215 I=2,30
      AI=DBLE(I-1)/29D 0
      F=0.0D 0
      DO 213 J=1,10
      F=F+X(J)*AI**(J-1)
  213 CONTINUE
      F=-F*F-1.0D 0
      DO 214 J=2,10
      AJ=DBLE(J-1)
      F=F+X(J)*AJ*AI**(J-2)
  214 CONTINUE
      F2=F2+F*F
  215 CONTINUE
      F2=F2+X(1)**2+(X(2)-X(1)**2-1.0D 0)**2
      F3=0.0D 0
      DO 216 I=2,10
      F3=F3+100.0D 0*(X(I)-X(I-1)**2)**2+(1.0D 0-X(I))**2
  216 CONTINUE
      IF ((F1.GE.F2).AND.(F1.GE.F3)) THEN
        DO 218 I=1,10
        G(I)=2.0D 0*X(I)-2.0D 0+4.0D -3*X(I)*F4
  218   CONTINUE
      ELSEIF ((F2.GE.F1).AND.(F2.GE.F3)) THEN
        DO 2185 J=1,10
        DO 2185 I=2,30
        AI=DBLE(I-1)/29D 0
        F=0.0D 0
        DO 2183 K=1,10
        F=F-X(K)*AI**(K-1)
 2183   CONTINUE
        T=2.0D 0*F*AI**(J-1)
        IF (J.GE.2) T=T+(J-1)*AI**(J-2)
        F=-F*F-1.0D 0
        DO 2184 K=2,10
        F=F+X(K)*(K-1)*AI**(K-2)
 2184   CONTINUE
        G(J)=G(J)+2.0D 0*F*T
 2185   CONTINUE
        G(1)=G(1)+2.0D 0*X(1)-4.0D 0*X(1)*(X(2)-X(1)**2-1.0D 0)
        G(2)=G(2)+2.0D 0*(X(2)-X(1)**2-1.0D 0)
      ELSE
        DO 219 I=1,10
        G(I)=0.0D 0
        IF (I.GE.2) G(I)=G(I)+2.0D 2*(X(I)-X(I-1)**2)-
     &                        2.0D 0*(1.0D 0-X(I))
        IF (I.LE.9) G(I)=G(I)-4.0D 2*X(I)*(X(I+1)-X(I)**2)
  219   CONTINUE
      ENDIF
      RETURN
  220 G(1)=X(1)/SQRT(X(1)**2+X(7)**2)
      G(7)=X(7)/SQRT(X(1)**2+X(7)**2)
      T=SQRT((5.5D 0-X(6))**2+(1.0D 0+X(12))**2)
      G(6)=-(5.5D 0-X(6))/T
      G(12)=(1.0D 0+X(12))/T
      DO 223 J=1,6
      T=SQRT((Y(J)-X(J))**2+(Y(6+J)-X(6+J))**2)
      G(J)=G(J)-Y(12+J)*(Y(J)-X(J))/T
      G(6+J)=G(6+J)-Y(12+J)*(Y(J+6)-X(J+6))/T
  223 CONTINUE
      DO 224 J=1,5
      T=SQRT((X(J)-X(J+1))**2+(X(J+6)-X(J+7))**2)
      G(J)=G(J)+Y(18+J)*(X(J)-X(J+1))/T
      G(J+1)=G(J+1)-Y(18+J)*(X(J)-X(J+1))/T
      G(J+6)=G(J+6)+Y(18+J)*(X(J+6)-X(J+7))/T
      G(J+7)=G(J+7)-Y(18+J)*(X(J+6)-X(J+7))/T
  224 CONTINUE
      RETURN
  130 F=X(1)**2
      K=1
      DO 131 I=2,20
      F1=X(I)**2
      IF (F.LT.F1) K=I
      F=MAX(F,F1)
  131 CONTINUE
      G(K)=2.0D 0*X(K)
      RETURN
  140 F=ABS(X(1))
      K=1
      DO 141 I=2,20
      F1=ABS(X(I))
      IF (F.LT.F1) K=I
      F=MAX(F,F1)
  141 CONTINUE
      G(K)=SIGN(1.0D 0,X(K))
      RETURN
  150 KK=N*N
      DO 151 I=1,N
      G(I)=-Y(KK+N+I)
  151 CONTINUE
      DO 152 J=1,N
      F=ETA9
      DO 153 I=1,N
      T=Y((I-1)*N+J)-X(I)
      IF (T.GE.F) GO TO 153
      F=T
      K=I
  153 CONTINUE
      G(K)=G(K)+Y(KK+J)
  152 CONTINUE
      RETURN
  160 F=-ETA9
      DO 162 I=1,50
      F2=X(I)
      IF (F.LT.F2) K=I
      F=MAX(F,F2)
      G(I)=-1.0D 0
  162 CONTINUE
      G(K)=G(K)+5D 1
      RETURN
  190 F1=-ETA9
      DO 192 I=1,N
      F=0.0D 0
      DO 191 J=1,N
      F=F+X(J)/DBLE(I+J-1)
  191 CONTINUE
      IF (F1.GE.ABS(F)) GO TO 192
      K=I
      AI=SIGN(1D 0,F)
      F1=ABS(F)
  192 CONTINUE
      DO 194 J=1,N
      G(J)=AI/DBLE(K+J-1)
  194 CONTINUE
      RETURN
  200 DO 204 J=1,N
      F1=0.0D 0
      DO 203 I=1,N
      F1=F1+X(I)/DBLE(I+J-1)
  203 CONTINUE
      AJ=SIGN(1.0D 0,F1)
      DO 204 I=1,N
      G(I)=G(I)+AJ/DBLE(I+J-1)
  204 CONTINUE
      RETURN
  250 F1=0.0D 0
      DO 251 J=1,5
      F1=F1+Y(85+J)*X(J)**3
  251 CONTINUE
      DO 253 J=1,5
      T=0.0D 0
      DO 252 I=1,5
      T=T+Y(55+I+J*5)*X(I)
  252 CONTINUE
      G(J)=SIGN(6D 0,F1)*Y(85+J)*X(J)*X(J)+T+T
  253 CONTINUE
      DO 254 J=6,15
      G(J)=-Y(45+J)
  254 CONTINUE
      DO 257 J=1,5
      T=-3.0D 0*Y(85+J)*X(J)*X(J)-Y(90+J)
      DO 255 I=1,15
      IF (I.LE.5) T=T-2.0D 0*Y(55+I+J*5)*X(I)
      IF (I.GT.5) T=T+Y(I+J*10-15)*X(I)
  255 CONTINUE
      IF (T.LE.0.0D 0) GO TO 257
      G(J)=G(J)-6.0D 2*Y(85+J)*X(J)
      DO 256 I=1,15
      IF (I.LE.5) G(I)=G(I)-2.0D 2*Y(55+I+J*5)
      IF (I.GT.5) G(I)=G(I)+1.0D 2*Y(I+J*10-15)
  256 CONTINUE
  257 CONTINUE
      DO 258 I=1,15
      IF (X(I).LT.0.0D 0) G(I)=G(I)-1.0D 2
  258 CONTINUE
      RETURN
      END
* SUBROUTINE TFHD19                ALL SYSTEMS                99/12/01
C PORTABILITY : ALL SYSTEMS
C 95/12/01 VL : ORIGINAL VERSION
*
* PURPOSE :
*  HESSIAN MATRIX OF THE NONSMOOTH OBJECTIVE FUNCTION.
*  DENSE VERSION.
*
* PARAMETERS :
*  II  N  NUMBER OF VARIABLES.
*  RI  X(N)  VECTOR OF VARIABLES.
*  RO  H(N*(N+1)/2)  HESSIAN MATRIX OF THE OBJECTIVE FUNCTION.
*  II  NEXT  NUMBER OF THE TEST PROBLEM.
*
      SUBROUTINE TFHD19(N,X,H,NEXT)
      INTEGER N,NEXT,I,IN,J,K,KK,L
      REAL*8 X(N),H(N*(N+1)/2)
      REAL*8 AI,AJ,ETA9,F1,F2,F3,F4,F,T,Y(2700)
      COMMON /EMPR19/ Y
      PARAMETER (ETA9=1.0D 60)
      IN(I,J)=(J-1)*J/2+I
      DO 1 I=1,N*(N+1)/2
        H(I)=0.0D0
    1 CONTINUE
      GO TO (10,20,30,30,50,60,70,80,90,180,100,110,230,240,170,120,
     & 210,220,130,140,150,160,190,200,250), NEXT
   10 H(1)=1200D 0*X(1)**2-400D 0*X(2)+2.0D 0
      H(2)=-400D 0*X(1)
      H(3)= 200D 0
      RETURN
   20 T=X(1)**2+(X(2)-1.0D 0)**2-1.0D 0
      F=SIGN(2.0D 0,T)
      H(1)=F
      H(3)=F
      RETURN
   30 I=NEXT-2
      J=5-NEXT
      F1=X(I)**2+X(J)**4
      F2=(2.0D 0-X(1))**2+(2.0D 0-X(2))**2
      F3=2.0D 0*EXP(-X(1)+X(2))
      IF ((F1.GE.F2).AND.(F1.GE.F3)) THEN
        H(NEXT*2-5)=2.0D 0
        H(9-NEXT*2)=12.0D 0*X(J)**2
      ELSE IF ((F2.GE.F1).AND.(F2.GE.F3)) THEN
        H(1)=2.0D 0
        H(3)=2.0D 0
      ELSE
        H(1)= 2.0D 0*EXP(X(2)-X(1))
        H(2)=-H(1)
        H(3)= H(1)
      ENDIF
      RETURN
   50 F1=5.0D 0*X(1)+X(2)
      F2=-5.0D 0*X(1)+X(2)
      F3=X(1)**2+X(2)**2+4.0D 0*X(2)
      IF ((F1.GE.F2).AND.(F1.GE.F3)) THEN
      ELSE IF ((F2.GE.F1).AND.(F2.GE.F3)) THEN
      ELSE
        H(1)=2.0D 0
        H(3)=2.0D 0
      ENDIF
      RETURN
   60 H(1)=2.0D 0
      H(3)=2.0D 0
      RETURN
   70 F1=-X(1)-X(2)
      F2=F1+(X(1)**2+X(2)**2-1.0D 0)
      IF (F1.LT.F2) THEN
        H(1)=2.0D 0
        H(3)=2.0D 0
      ENDIF
      RETURN
   80 F1=X(1)**2+X(2)**2-1.0D 0
      IF (F1.GE.0.0D 0) THEN
        H(1)=40.0D 0
        H(3)=40.0D 0
      ENDIF
      RETURN
   90 F1=SIGN(3.5D 0,X(1)**2+X(2)**2-1.0D 0)+4.0D 0
      H(1)=F1
      H(3)=F1
      RETURN
  180 IF (X(1).GT.ABS(X(2))) THEN
        F1=720D 0*(9.0D 0*X(1)**2+16.0D 0*X(2)**2)**(-1.5D 0)
        H(1)= F1*X(2)**2
        H(2)=-F1*X(1)*X(2)
        H(3)= F1*X(1)**2
      ELSE
        IF (X(1).LT.0.0D 0) H(1)=-72.0D 0*X(1)**8
      ENDIF
      RETURN
  100 F=X(1)**2+X(2)**2+X(3)**2
      F2=F+X(4)**2+X(1)-X(2)+X(3)-X(4)-8.0D 0
      F3=F+X(2)**2+2.0D 0*X(4)**2-X(1)-X(4)-10.0D 0
      F4=F+2.0D 0*X(1)-X(2)-X(4)-5.0D 0
      L=1
      IF (F2.GT.0.0D 0) L=2
      IF (F3.GT.MAX(F2,0.0D 0)) L=3
      IF (F4.GT.MAX(F2,F3,0.0D 0)) L=4
      GO TO (101,102,103,104),L
  101 H(1)= 2.0D 0
      H(3)= 2.0D 0
      H(6)= 4.0D 0
      H(10)=2.0D 0
      RETURN
  102 H(10)=22.0D 0
      GO TO 106
  103 H(3)= 42.0D 0
      H(10)=42.0D 0
      GO TO 105
  104 H(10)= 2.0D 0
  106 H(3)= 22.0D 0
  105 H(1)= 22.0D 0
      H(6)= 24.0D 0
      RETURN
  110 F=-ETA9
      DO 112 I=1,10
      F1=0.0D 0
      DO 113 J=1,5
      F1=F1+(X(J)-Y(5*(I-1)+J))**2
  113 CONTINUE
      F1=F1*Y(50+I)
      IF (F.LT.F1) K=I
      F=MAX(F,F1)
  112 CONTINUE
      DO 114 J=1,5
      H(J*(J+1)/2)=2.0D 0*Y(50+K)
  114 CONTINUE
      RETURN
  230 DO 231 J=1,5
      H(J*(J+1)/2)=6.0D 0*Y(85+J)*X(J)
  231 CONTINUE
      K=1
      DO 232 I=1,N
      DO 232 J=1,I
      H(K)=H(K)+2.0D 0*Y(55+I+J*5)
      K=K+1
  232 CONTINUE
      RETURN
  240 H(1)= 0.0D 0
      H(2)= X(3)*X(4)*X(5)
      H(3)= 0.0D 0
      H(4)= X(2)*X(4)*X(5)
      H(5)= X(1)*X(4)*X(5)
      H(6)= 0.0D 0
      H(7)= X(2)*X(3)*X(5)
      H(8)= X(1)*X(3)*X(5)
      H(9)= X(1)*X(2)*X(5)
      H(10)=0.0D 0
      H(11)=X(2)*X(3)*X(4)
      H(12)=X(1)*X(3)*X(4)
      H(13)=X(1)*X(2)*X(4)
      H(14)=X(1)*X(2)*X(3)
      H(15)=0.0D 0
      F1=X(1)**2+X(2)**2+X(3)**2+X(4)**2+X(5)**2-1.0D 1
      F4=1.0D 0
      IF (F1.LT.0.0D 0) F4=-F4
      L=0
      DO 241 I=1,5
      L=L+I
      H(L)=H(L)+2.0D 1*F4
  241 CONTINUE
      F2=X(2)*X(3)-5.0D 0*X(4)*X(5)
      F4=1.0D 0
      IF (F2.LT.0.0D 0) F4=-F4
      H( 5)=H( 5)+1.0D 1*F4
      H(14)=H(14)-5.0D 1*F4
      F3=X(1)**3+X(2)**3+1.0D 0
      F4=1.0D 0
      IF (F3.LT.0.0D 0) F4=-F4
      H(1)=H(1)+6.0D 1*F4*X(1)
      H(3)=H(3)+6.0D 1*F4*X(2)
      RETURN
  170 DO 172 I=1,51
      T=DBLE(I-1)/10.0D 0
      F=0.5D 0*EXP(-T)-EXP(-T*2.0D 0)+0.5D 0*EXP(-T*3.0D 0)+
     & 1.5D 0*EXP(-T*1.5D 0)*SIN(7.0D 0*T)+
     & EXP(-T*2.5D 0)*SIN(5.0D 0*T)
      F1=EXP(-X(2)*T)
      F2=EXP(-X(6)*T)
      F3=COS(X(3)*T+X(4))
      F4=SIN(X(3)*T+X(4))
      AI=SIGN(1.0D 0,X(1)*F1*F3+X(5)*F2-F)
      H(2)=H(2)-AI*F1*F3*T
      H(3)=H(3)+AI*F1*F3*T*T*X(1)
      H(4)=H(4)-AI*F1*F4*T
      H(5)=H(5)+AI*F1*F4*T*T*X(1)
      H(6)=H(6)-AI*F1*F3*T*T*X(1)
      H(7)=H(7)-AI*F1*F4
      H(8)=H(8)+AI*F1*F4*T*X(1)
      H(9)=H(9)-AI*F1*F3*T*X(1)
      H(10)=H(10)-AI*F1*F3*X(1)
      H(15)=H(15)-AI*F2*T
      H(20)=H(20)-AI*F2*T
      H(21)=H(21)+AI*F2*T*T*X(5)
  172 CONTINUE
      RETURN
  120 F=-ETA9
      L=1
      KK=0
      DO 123 K=1,5
      F1=0.0D 0
      DO 122 I=1,N
      F1=F1-Y(KK+100+I)*X(I)
      DO 122 J=1,N
      F1=F1+Y(KK+(I-1)*N+J)*X(I)*X(J)
  122 CONTINUE
      IF (F.LT.F1) L=K
      F=MAX(F,F1)
      KK=KK+110
  123 CONTINUE
      K=1
      DO 124 I=1,N
      DO 124 J=1,I
      H(K)=2.0D 0*Y((L-1)*110+(I-1)*N+J)
      K=K+1
  124 CONTINUE
      RETURN
  210 F1=0.0D 0
      DO 211 I=1,10
      F1=F1+X(I)**2
  211 CONTINUE
      F4=F1-0.25D 0
      F1=1.0D -3*F4*F4
      DO 212 I=1,10
      F1=F1+(X(I)-1.0D 0)**2
  212 CONTINUE
      F2=0.0D 0
      DO 215 I=2,30
      AI=DBLE(I-1)/29D 0
      F=0.0D 0
      DO 213 J=1,10
      F=F+X(J)*AI**(J-1)
  213 CONTINUE
      F=-F*F-1.0D 0
      DO 214 J=2,10
      AJ=DBLE(J-1)
      F=F+X(J)*AJ*AI**(J-2)
  214 CONTINUE
      F2=F2+F*F
  215 CONTINUE
      F2=F2+X(1)**2+(X(2)-X(1)**2-1.0D 0)**2
      F3=0.0D 0
      DO 216 I=2,10
      F3=F3+100.0D 0*(X(I)-X(I-1)**2)**2+(1.0D 0-X(I))**2
  216 CONTINUE
      IF ((F1.GE.F2).AND.(F1.GE.F3)) THEN
        L=1
        DO 218 I=1,N
        DO 218 J=1,I
        H(L)=8.0D-3*X(I)*X(J)
        IF (J.EQ.I) H(L)=H(L)+2.0D 0+4.0D-3*F4
        L=L+1
  218   CONTINUE
      ELSE IF ((F2.GE.F1).AND.(F2.GE.F3)) THEN
        KK=1
        DO 2186 J=1,N
        DO 2186 L=1,J
        DO 2185 I=2,30
        AI=DBLE(I-1)/29.0D 0
        F=0.0D 0
        DO 2183 K=1,10
        F=F-X(K)*AI**(K-1)
 2183   CONTINUE
        T=2.0D 0*F*AI**(J-1)
        IF (J.GE.2) T=T+(J-1)*AI**(J-2)
        F4=2.0D 0*F*AI**(L-1)
        IF (L.GE.2) F4=F4+(L-1)*AI**(L-2)
        F=-F*F-1.0D 0
        DO 2184 K=2,10
        F=F+X(K)*(K-1)*AI**(K-2)
 2184   CONTINUE
        H(KK)=H(KK)+2.0D 0*(T*F4-2.0D 0*F*AI**(J+L-2))
 2185   CONTINUE
        KK=KK+1
 2186   CONTINUE
        H(1)=H(1)+12.0D 0*X(1)**2-4.0D 0*X(2)+6.0D 0
        H(2)=H(2)-4.0D 0*X(1)
        H(3)=H(3)+2.0D 0
      ELSE
        DO 219 I=1,10
        J=I*(I+1)/2
        IF (I.GE.2) H(J)=202.0D 0
        IF (I.LE.9) H(J)=H(J)+4.0D 2*(3.0D 0*X(I)**2-X(I+1))
        IF (I.LE.9) H(J+I)=-4.0D 2*X(I)
  219   CONTINUE
      ENDIF
      RETURN
  220 T= (X(1)**2+X(7)**2)**1.5D 0
      H(1) = X(7)**2/T
      H(22)=-X(1)*X(7)/T
      H(28)= X(1)**2/T
      T=((5.5D 0-X(6))**2+(1.0D 0+X(12))**2)**1.5D 0
      H(21)=(1.0D 0+X(12))**2/T
      H(72)=(5.5D 0-X(6))*(1.0D 0+X(12))/T
      H(78)=(5.5D 0-X(6))**2/T
      DO 223 J=1,6
      T=Y(12+J)/((Y(J)-X(J))**2+(Y(6+J)-X(6+J))**2)**1.5D 0
      H(IN(J,J))    =H(IN(J,J))    +(Y(J+6)-X(J+6))**2*T
      H(IN(J,J+6))  =H(IN(J,J+6))  -(Y(J+6)-X(J+6))*(Y(J)-X(J))*T
      H(IN(J+6,J+6))=H(IN(J+6,J+6))+(Y(J)-X(J))**2*T
  223 CONTINUE
      DO 224 J=1,6
      IF (J.LT.6) THEN
        F1=X(J)-X(J+1)
        F2=X(J+6)-X(J+7)
        T=Y(18+J)/(F1*F1+F2*F2)**1.5D 0
        H(IN(J,J))    =H(IN(J,J))    +F2*F2*T
        H(IN(J,J+1))  =H(IN(J,J+1))  -F2*F2*T
        H(IN(J,J+6))  =H(IN(J,J+6))  -F1*F2*T
        H(IN(J,J+7))  =H(IN(J,J+7))  +F1*F2*T
        H(IN(J+6,J+6))=H(IN(J+6,J+6))+F1*F1*T
        H(IN(J+6,J+7))=H(IN(J+6,J+7))-F1*F1*T
      ENDIF
      IF (J.GT.1) THEN
        F1=X(J)-X(J-1)
        F2=X(J+6)-X(J+5)
        T=Y(17+J)/(F1*F1+F2*F2)**1.5D 0
        H(IN(J,J))    =H(IN(J,J))    +F2*F2*T
        H(IN(J,J+5))  =H(IN(J,J+5))  +F1*F2*T
        H(IN(J,J+6))  =H(IN(J,J+6))  -F1*F2*T
        H(IN(J+6,J+6))=H(IN(J+6,J+6))+F1*F1*T
      ENDIF
  224 CONTINUE
      RETURN
  130 F=X(1)**2
      K=1
      DO 131 I=2,20
      F1=X(I)**2
      IF (F.LT.F1) K=I
      F=MAX(F,F1)
  131 CONTINUE
      H(K*(K+1)/2)=2.0D 0
      RETURN
  140 RETURN
  150 RETURN
  160 RETURN
  190 RETURN
  200 RETURN
  250 F1=0D 0
      DO 251 J=1,5
      F1=F1+Y(85+J)*X(J)**3
  251 CONTINUE
      DO 252 J=1,5
      H(J*(J+1)/2)=SIGN(12.0D 0,F1)*Y(85+J)*X(J)
  252 CONTINUE
      K=1
      DO 253 I=1,5
      DO 253 J=1,I
      H(K)=H(K)+2.0D 0*Y(55+I+J*5)
      K=K+1
  253 CONTINUE
      DO 257 J=1,5
      T=-3.0D 0*Y(85+J)*X(J)*X(J)-Y(90+J)
      DO 255 I=1,15
      IF (I.LE.5) T=T-2.0D 0*Y(55+I+J*5)*X(I)
      IF (I.GT.5) T=T+Y(I+J*10-15)*X(I)
  255 CONTINUE
      IF (T.LE.0.0D 0) GO TO 257
      I=J*(J+1)/2
      H(I)=H(I)-6.0D 2*Y(85+J)
  257 CONTINUE
      RETURN
      END
