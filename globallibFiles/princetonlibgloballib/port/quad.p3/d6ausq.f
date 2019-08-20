      SUBROUTINE D6AUSQ(A,B,AC,NU,BETA,ALPHA2,N,R1,R2)
C
C THIS IS A TRANSLATION OF THE ALGOL PROGRAM IN
C SACK AND DONOVAN, NUM. MATH. 18, 465-478(1972).
C
      DOUBLE PRECISION A(1),B(1),AC(1),NU(1),BETA(1),ALPHA2(1),R1(2)
      DOUBLE PRECISION R2(1),TERM,SIGMA
      LOGICAL EVEN
      IMAX=N
      JMAX=IMAX+N
      TERM=1.0D0/NU(1)
      DO 10 J=1,JMAX
         R1(J)=TERM*NU(J)
         R2(J)=0.0D0
 10      TERM=TERM*A(J)
      SIGMA=R1(2)+B(1)
      BETA(1)=SIGMA
      EVEN=.TRUE.
      IF (2.GT.IMAX) GO TO 80
      DO 70 I=2,IMAX
         IP1=I+1
         IF (EVEN) GO TO 40
         JMAX=JMAX-1
         EVEN=.NOT.EVEN
         IF (I.GT.JMAX) GO TO 25
         DO 20 J=I,JMAX
 20         R1(J)=(B(J)-SIGMA)*R2(J)+R2(J+1)+AC(J)*R2(J-1)-R1(J)
 25      TERM=R1(I)
         ALPHA2(I-1)=TERM
         TERM=1.0D0/TERM
         R1(I)=1.0D0
         IF (IP1.GT.JMAX) GO  TO 35
         DO 30 J=IP1,JMAX
 30         R1(J)=R1(J)*TERM
 35      SIGMA=R1(IP1)-R2(I)+B(I)
         GO TO 70
 40      JMAX=JMAX-1
         EVEN=.NOT.EVEN
         IF (I.GT.JMAX) GO TO 55
         DO 50 J=I,JMAX
 50         R2(J)=(B(J)-SIGMA)*R1(J)+R1(J+1)+AC(J)*R1(J-1)-R2(J)
 55      TERM=R2(I)
         ALPHA2(I-1)=TERM
         TERM=1.0D0/TERM
         R2(I)=1.0D0
         IF (IP1.GT.JMAX) GO TO 65
         DO 60 J=IP1,JMAX
 60         R2(J)=R2(J)*TERM
 65      SIGMA=R2(IP1)-R1(I)+B(I)
 70      BETA(I)=SIGMA
 80   ALPHA2(IMAX)=0.0D0
      RETURN
      END
