      LOGICAL FUNCTION D3PLNN(T,N,X,ILEFT,J,COL,DM,DP,JJ)
C
C  TO SET
C
C  COL(I) = N(ILEFT+I-J,J)(X)  I=1,...,J
C
C  FOR X IN (T(ILEFT),T(ILEFT+1)).
C
C  THIS ROUTINE IS MEANT TO BE CALLED SEQUENTIALLY (J=1,...,K)
C  AND A START FROM SCRATCH IS SIGNALLED BY JJ=0, OTHERWISE
C  EVERYTHING BUT J IS ASSUMED TO BE AS THE LAST CALL LEFT THEM.
C
C  D3PLNN=.TRUE. IS RETURNED IF SUCCESSFUL.
C  D3PLNN=.FALSE. IS RETURNED IF THE MESH T IS NOT MONOTONE INCREASING.
C
      DOUBLE PRECISION T(N),X,COL(J),DM(J),DP(J)
C
      DOUBLE PRECISION PREV,TEMP
C
      D3PLNN=.TRUE.
C
      IF (JJ.NE.0) GO TO 10
      JJ=1
      COL(1)=1.0D0
 10      IF (JJ.GE.J) GO TO 40
         IPJJ=MIN0(ILEFT+JJ,N)
         DP(JJ)=T(IPJJ)-X
C
         IF (DP(JJ).LT.0.0D0) GO TO 30
C
         IMJJP1=MAX0(ILEFT+1-JJ,1)
         DM(JJ)=X-T(IMJJP1)
C
         IF (DM(JJ).LT.0.0D0) GO TO 30
C
         PREV=0.0D0
         JJP1=JJ+1
         DO 20 I=1,JJ
            JJP1MI=JJP1-I
            TEMP=DP(I)+DM(JJP1MI)
C
            IF (TEMP.EQ.0.0D0) GO TO 30
C
            TEMP=COL(I)/TEMP
            COL(I)=PREV+TEMP*DP(I)
 20         PREV=DM(JJP1MI)*TEMP
         COL(JJP1)=PREV
         JJ=JJP1
         GO TO 10
C
 30   D3PLNN=.FALSE.
C
 40   RETURN
C
      END
