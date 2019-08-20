      SUBROUTINE S4YCE(N, A, COND, INTER, ANORM, Z)
      INTEGER N
      INTEGER INTER(N)
      REAL A(1), COND, ANORM, Z(N)
      INTEGER JI, JK,    L
      INTEGER IP1,  I, J, ICH, JIS
      REAL EK, ABS, ARGE, SM, WK, SAVE
      REAL TEMP,   AJI, S, T
      REAL  SIGN, WKM, SASUM, R1MACH, YNORM
C TEST FOR 1 X 1
      IF (N .NE. 1) GOTO 1
         COND = 1
         RETURN
   1  EK = 1.0
      DO  2 J = 1, N
         Z(J) = 0.0
   2     CONTINUE
         L=0
         IEND=N*(N+1)/2
      I = 1
      JI = 1
C SOLVE LDY=E WHERE THE COMPONENTS OF E ARE PLUS
C AND MINUS 1 WHERE THE SIGNS ARE CHOSEN TO PRODUCE MAXIMUM
C GROWTH. LOOK AHEAD IS USED ONLY FOR 1 X1 PIVOTS
   3  IF (I .GE. N) GOTO  13
   4        ICH = INTER(I)
            SAVE = Z(ICH)
            IF (SAVE .NE. 0.0) EK = SIGN(EK, -SAVE)
            IP1 = I+1
            IF (INTER(IP1) .LT. 0) GOTO 9
               Z(ICH) = Z(I)
C PROCESS A 1 X1  PIVOT
               IF (ABS(EK-SAVE) .LE. ABS(A(JI))) GOTO 5
                  S = ABS(A(JI)/(EK-SAVE))
                  CALL SSCAL(N, S, Z, 1)
                  EK = S*EK
                  SAVE = S*SAVE
   5           WK = EK-SAVE
               WKM = (-EK)-SAVE
               S = ABS(WK)
               SM = ABS(WKM)
               AJI = A(JI)
               JI = JI+1
               JIS = JI
               DO  6 J = IP1, N
                  SM = SM+ABS(Z(J)-WKM*A(JI))
                  Z(J) = Z(J)-WK*A(JI)
                  S = S+ABS(Z(J))
                  JI = JI+1
   6              CONTINUE
               IF (S .GE. SM) GOTO 8
                  T = WKM-WK
                  WK = WKM
                  DO  7 J = IP1, N
                     Z(J) = Z(J)-T*A(JIS)
                     JIS = JIS+1
   7                 CONTINUE
   8           IF (AJI.EQ.0.0)Z(I)=1.E0
               IF (AJI.NE.0.0)Z(I)= WK /AJI
               I = IP1
C PROCESS A 2 X 2 PIVOT
               GOTO  12
   9           Z(ICH) = Z(IP1)
               SAVE = SAVE-EK
               TEMP = Z(I)
               IF (TEMP .NE. 0.0) EK = SIGN(EK, -TEMP)
               TEMP = TEMP-EK
               CALL S42B2(N,I,JI,A,Z,TEMP,SAVE)
  12        CONTINUE
C SOLVE MDY=B AND STORE Y IN B
         GOTO  3
  13  IF (I .NE. N) GOTO 14
         IF (A(JI).EQ.0.0) Z(I)=1.0
         IF(A(JI).NE.0.0)Z(I) =- (Z(I)+SIGN(EK, Z(I)))/A(JI)
         JK = (N*(N+1))/2-1
         I = N-1
         GOTO  15
  14     JK = (N*(N+1))/2-4
         I = N-2
  15  S = 1.0/SASUM(N, Z, 1)
      CALL SSCAL(N, S, Z, 1)
 16   CALL S4BS(N,I,A,Z,JK,INTER)
  19  S = 1.0/SASUM(N, Z, 1)
      CALL SSCAL(N, S, Z, 1)
      YNORM = 1.0
      I=1
      JI=1
 1010 IF (I.GE.N) GO TO 1200
      ICH=INTER(I)
      SAVE=Z(ICH)
      IP1=I+1
      IF(INTER(IP1).LT.0) GO TO 1030
         Z(ICH)=Z(I)
         IF (ABS(SAVE).LE.ABS(A(JI))) GO TO 1015
             S=ABS(A(JI))/ABS(SAVE)
             CALL SSCAL(N,S,Z,1)
             YNORM=S*YNORM
             SAVE=S*SAVE
 1015     IF (A(JI).EQ.0.0) Z(I)=1.0
          IF (A(JI).NE.0.0) Z(I)=SAVE/A(JI)
         JI=JI+1
         DO 1020 J=IP1,N
            Z(J)=Z(J)+SAVE*A(JI)
            JI=JI+1
 1020    CONTINUE
         I=IP1
         GO TO 1010
 1030    TEMP=Z(I)
         Z(ICH)=Z(IP1)
         CALL S42B2(N,I,JI,A,Z,TEMP,SAVE)
         GO TO 1010
 1200    IF (I.NE.N)GO TO 1202
         Z(I)=Z(I)/A(JI)
         JK=(N*(N+1))/2-1
         I=N-1
         GO TO1210
 1202    JK=(N*(N+1))/2-3
         I=N-2
 1210    CALL S4BS(N,I,A,Z,JK,INTER)
      S = 1.0/SASUM(N, Z, 1)
       CALL SSCAL(N,S,Z,1)
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
