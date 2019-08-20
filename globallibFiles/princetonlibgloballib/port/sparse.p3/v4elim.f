           SUBROUTINE V4ELIM(N,L,V,IV,MARK,IBIG)
           INTEGER L(N),V(N),MARK(N)
C THIS SUBROUTINE ELIMINATES VERTEX IV FROM THE LINK LIST
C STRUCTURE
C
      M=MARK(IV)-1
      IF (M.LT.1) GO TO 30
      LK=L(IV)
      DO 20 I=1,M
          LN=V(LK)
          MARK(LN)=MARK(LN)-1
  10      LN1=LN
          LN=L(LN)
          IF (V(LN).NE.IV) GO TO 10
          L(LN1)=L(LN)
          LK=L(LK)
  20   CONTINUE
  30   IBIG=IBIG-1
       MARK(IV)=-1
       RETURN
       END
