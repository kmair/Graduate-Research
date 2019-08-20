      SUBROUTINE CS4YCE(N, A, COND, INTER, ANORM, Z)
      INTEGER N
      INTEGER INTER(N)
      REAL COND, ANORM
      COMPLEX A(1), Z(N)
      INTEGER JI, JK,    L
      INTEGER IP1,  I, J, ICH, JIS
      REAL CABS1,S,SM
      COMPLEX T,EK,WK,WKM,TEMP,AJI,SAVE
      COMPLEX Z1,Z2,CSIGN1
      REAL  SCASUM, R1MACH, YNORM, CAJI, ARGE
       CSIGN1(Z1,Z2)=CABS1(Z1)*(Z2/CABS1(Z2))
C TEST FOR 1 X 1
      IF (N .NE. 1) GOTO 1
         COND = 1
         RETURN
   1  EK = (1.0,0.0)
      DO  2 J = 1, N
         Z(J) = (0.0,0.0)
   2     CONTINUE
         L=0
      I = 1
      JI = 1
C SOLVE LDY=E WHERE THE COMPONENTS OF E ARE PLUS
C AND MINUS 1 WHERE THE SIGNS ARE CHOSEN TO PRODUCE MAXIMUM
C GROWTH. LOOK AHEAD IS USED ONLY FOR 1 X1 PIVOTS
   3  IF (I .GE. N) GOTO  13
   4        ICH = INTER(I)
            SAVE = Z(ICH)
            IF ((CABS1(SAVE)) .NE. 0.0) EK = CSIGN1(EK, -SAVE)
            IP1 = I+1
            IF (INTER(IP1) .LT. 0) GOTO 9
               Z(ICH) = Z(I)
C PROCESS A 1 X1  PIVOT
               IF (CABS1(EK-SAVE) .LE. CABS1(A(JI))) GOTO 5
                  S = CABS1(A(JI)/(EK-SAVE))
                  CALL CSSCAL(N, S, Z, 1)
                  EK = CMPLX(S,0.0)*EK
                  SAVE = CMPLX(S,0.0)*SAVE
   5           WK = EK-SAVE
               WKM = (-EK)-SAVE
               S = CABS1(WK)
               SM = CABS1(WKM)
               AJI = A(JI)
               JI = JI+1
               JIS = JI
               DO  6 J = IP1, N
                  SM = SM+CABS1(Z(J)-WKM*(A(JI)))
                  Z(J) = Z(J)-WK*(A(JI))
                  S = S+CABS1(Z(J))
                  JI = JI+1
   6              CONTINUE
               IF (S .GE. SM) GOTO 8
                  T = WKM-WK
                  WK = WKM
                  DO  7 J = IP1, N
                     Z(J) = Z(J)-T*(A(JIS))
                     JIS = JIS+1
   7                 CONTINUE
   8           CAJI=CABS1(A(JI))
               IF (CAJI.EQ.0.0)Z(I)=(1.E0,0.E0)
               IF (CAJI.NE.0.0)Z(I)= WK /AJI
               I = IP1
C PROCESS A 2 X 2 PIVOT
               GOTO  12
   9           Z(ICH) = Z(IP1)
               SAVE = SAVE-EK
               TEMP = Z(I)
               IF (CABS1(TEMP) .NE. 0.0) EK = CSIGN1(EK, -TEMP)
               TEMP = TEMP-EK
               CALL CS42B2(N,I,JI,A,Z,TEMP,SAVE)
  12        CONTINUE
C SOLVE MDY=B AND STORE Y IN B
         GOTO  3
  13  IF (I .NE. N) GOTO 14
         IF (CABS1(A(JI)).EQ.0.0) GO TO 138
            IF (CABS1(Z(I)).NE.0.0) EK=CSIGN1(EK,-Z(I))
            Z(I)=(EK-Z(I))/A(JI)
            GO TO 139
 138        Z(I)=(1.0,0.0)
 139     JK = (N*(N+1))/2-1
         I = N-1
         GOTO  15
  14     JK = (N*(N+1))/2-4
         I = N-2
  15  S = 1.0/SCASUM(N, Z, 1)
      CALL CSSCAL(N, S, Z, 1)
 16   CALL C4HBS(N,I,A,Z,JK,INTER)
  19  S = 1.0/SCASUM(N, Z, 1)
      CALL CSSCAL(N, S, Z, 1)
      YNORM = 1.0
      I=1
      JI=1
 1010 IF (I.GE.N) GO TO 1200
      ICH=INTER(I)
      SAVE=Z(ICH)
      IP1=I+1
      IF(INTER(IP1).LT.0) GO TO 1030
         Z(ICH)=Z(I)
         IF (CABS1(SAVE).LE.CABS1(A(JI))) GO TO 1015
             S=CABS1(A(JI)/SAVE)
             CALL CSSCAL(N,S,Z,1)
             YNORM=S*YNORM
             SAVE=CMPLX(S,0.0)*SAVE
 1015     CAJI=CABS1(A(JI))
          IF (CAJI.EQ.0.0) Z(I)=(1.0,0.0)
          IF (CAJI.NE.0.0) Z(I)=SAVE/A(JI)
         JI=JI+1
         DO 1020 J=IP1,N
            Z(J)=Z(J)+SAVE*(A(JI))
            JI=JI+1
 1020    CONTINUE
         I=IP1
         GO TO 1010
 1030    TEMP=Z(I)
         Z(ICH)=Z(IP1)
         CALL CS42B2(N,I,JI,A,Z,TEMP,SAVE)
         GO TO 1010
 1200    IF (I.NE.N)GO TO 1202
         Z(I)=Z(I)/A(JI)
         JK=(N*(N+1))/2-1
         I=N-1
         GO TO1210
 1202    JK=(N*(N+1))/2-3
         I=N-2
 1210    CALL C4HBS(N,I,A,Z,JK,INTER)
      S = 1.0/SCASUM(N, Z, 1)
       CALL CSSCAL(N,S,Z,1)
      YNORM = YNORM*S
      ARGE = R1MACH(2)
      IF (YNORM .GT. 1.0) GOTO 21
         IF (ANORM .LE. YNORM*ARGE) GOTO 20
            COND = ARGE
            RETURN
  20  CONTINUE
  21  COND = ANORM/YNORM
      RETURN
      END
