      LOGICAL FUNCTION A3PLNB(T,N,K,X,NX,ILEFT,ID,NID,BX,ADIFF,F)
C
C  TO GET THE DERIVATIVES OF ALL BASIS SPLINES.
C
C  A3PLNB=.TRUE. IS RETURNED IF SUCCESSFUL.
C  A3PLNB=.FALSE. IS RETURNED IF THE MESH T IS NOT MONOTONE INCREASING.
C
      REAL T(N),X(NX),BX(NX,K,NID),ADIFF(K,1),F(K)
C     REAL ADIFF(K,MIN((K+1)/2,ID(NID)+1))
      INTEGER ID(NID)
C
      REAL FKMD,FKMDIF
C
      A3PLNB=.TRUE.
C
      ILMK=ILEFT-K
      KP1=K+1
      JMAX=MIN0(KP1/2,ID(NID)+1)
C
C ... ADIFF(I,J) IS THE COEFFICIENT VECTOR FOR THE I-TH SPLINES
C ... JD-TH DERIVATIVE.
C
      DO 20 I=1,K
         DO 10 J=1,JMAX
 10         ADIFF(I,J)=0.0E0
 20      ADIFF(I,1)=1.0E0
C
      IDERIV=1
      JD=2
      IF (ID(1).NE.0) JD=1
C
C ...... LOOP ON JD, THE INDEX INTO ID(JD).
C
 30      IF (JD.GT.NID) GO TO 170
         ILMKPD=ILMK+IDERIV
         KMID=K-IDERIV
         FKMD=FLOAT(KMID)
         ILMD=ILEFT-IDERIV
         IDP1=IDERIV+1
         MIND=MIN0(IDP1,KMID)
C
C ...... COMPUTE ALL ADIFF WITH SAME DENOMINATOR.
C
         DO 100 JSMD=1,KMID
            IF (JSMD.GT.IDERIV) GO TO 70
C
C ......... JS INCREASING HERE.
C
            IDX1=MIN0(ILEFT+JSMD,N)
            IDX2=MAX0(ILMKPD+JSMD,1)
            FKMDIF=T(IDX1)-T(IDX2)
C
            IF (FKMDIF.LE.0.0E0) GO TO 160
C
            FKMDIF=FKMD/FKMDIF
            IF (JSMD.EQ.JMAX) GO TO 50
            JSMDP1=JSMD+1
            DO 40 I=JSMD,IDERIV
 40            ADIFF(I,JSMD)=FKMDIF*(ADIFF(I,JSMDP1)-ADIFF(I,JSMD))
            GO TO 70
 50         DO 60 I=JSMD,IDERIV
 60            ADIFF(I,JSMD)=-FKMDIF*ADIFF(I,JSMD)
C
C ......... JS DECREASING HERE.
C
 70         JS=KP1-JSMD
            JSP1=JS+1
            IDX1=MIN0(ILMD+JS,N)
            IDX2=MAX0(ILMK+JS,1)
            FKMDIF=T(IDX1)-T(IDX2)
C
            IF (FKMDIF.LE.0.0E0) GO TO 160
C
            FKMDIF=FKMD/FKMDIF
            I0=MAX0(IDP1,JS-IDERIV)
            I1=JS-1
            IF (I0.GT.I1) GO TO 90
            J=JSP1-I0
            DO 80 I=I0,I1
               ADIFF(I,J)=FKMDIF*(ADIFF(I,J)-ADIFF(I,J-1))
 80            J=J-1
 90         ADIFF(JS,1)=FKMDIF*ADIFF(JS,1)
 100        CONTINUE
         IF (IDERIV.NE.ID(JD)) GO TO 150
C
C ...... EVALUATE THE ID(JD)-TH DERIVATIVE OF THE BASIS SPLINES.
C
         DO 140 IX=1,NX
            DO 120 I=1,K
               JHI=MIN0(KP1-I,I,MIND)
               IDX=MAX0(I-IDERIV,1)
               F(I)=0.0E0
               DO 110 J=1,JHI
                  F(I)=F(I)+ADIFF(I,J)*BX(IX,IDX,JD)
 110              IDX=IDX+1
 120           CONTINUE
            DO 130 I=1,K
 130           BX(IX,I,JD)=F(I)
 140        CONTINUE
         JD=JD+1
 150     IDERIV=IDP1
         GO TO 30
C
 160  A3PLNB=.FALSE.
C
 170  RETURN
C
      END