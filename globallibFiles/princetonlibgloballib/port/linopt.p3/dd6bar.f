      SUBROUTINE DD6BAR(W, N, IBEGIN, POSDEF, D, E, Y, EPSI)
      INTEGER N
      INTEGER IBEGIN
      LOGICAL POSDEF
      DOUBLE PRECISION W(N), D(N), E(N), Y(N)
      INTEGER J, K
      DOUBLE PRECISION EPS, EPSI
      DOUBLE PRECISION ALFA, DJ, EJ, BETA, YJ, TEMP
      DOUBLE PRECISION YTDY, GAMMA, DELTA, SGN, DENOM, DSQRT
      DOUBLE PRECISION YTDIY, DJP1, YJP1
C
C THIS SUBROUTINE DETERMINES W=DBAR(INVERSE)*Y
C
       EPS=EPSI
      J = IBEGIN
      K = 0
      GAMMA = 0.D0
      ALFA = 0.D0
   1     J = J+K
         IF (J .GT. N) GOTO  11
         DJ = D(J)
         EJ = E(J)
         YJ = Y(J)
         IF (EJ .NE. 0.D0) GOTO 5
            K = 1
C
C THE JTH BLOCK IN D IS 1-BY-1
C
            W(J) = -YJ
            IF (DJ .GE. 0D0) GOTO 2
               W(J) = YJ/DJ
               ALFA = ALFA+W(J)*YJ
               GOTO  4
   2           IF (DJ .EQ. 0D0) GOTO 3
                  W(J) = (-YJ)/DJ
                  GAMMA = GAMMA+(-YJ)*W(J)
   3        CONTINUE
   4        CONTINUE
            GOTO  10
   5        K = 2
C
C THE JTH BLOCK IN D IS 2-BY-2
C
            DJP1 = D(J+1)
            YJP1 = Y(J+1)
            DENOM = DJ*DJP1-EJ**2
            W(J) = (DJP1*YJ-EJ*YJP1)/DENOM
            W(J+1) = (DJ*YJP1-EJ*YJ)/DENOM
            YTDIY = W(J)*YJ+W(J+1)*YJP1
            YTDY = DJ*YJ**2+2.*EJ*YJ*YJP1+DJP1*YJP1**2
            IF (YTDIY .GE. 0D0) GOTO 6
               ALFA = ALFA+YTDIY
               GOTO  9
   6           IF (YTDY .GE. 0D0) GOTO 7
                  W(J) = -YJ
                  W(J+1) = -YJP1
                  ALFA = ALFA+YTDY
                  GOTO  8
   7              W(J) = YJP1
                  W(J+1) = -YJ
                  ALFA = YTDIY*DENOM+ALFA
   8        CONTINUE
   9        CONTINUE
  10     CONTINUE
         GOTO  1
  11  IF (POSDEF) GOTO 22
         IF (ALFA .EQ. 0.D0) GOTO 16
            BETA = 1.
            IF (GAMMA.GT.EPS) BETA=DSQRT(2.D0*EPS-ALFA)/
     1          DSQRT(GAMMA)
            J = IBEGIN
            K = 0
  12           J = J+K
               IF (J .GT. N) GOTO  15
               IF (E(J) .NE. 0.D0) GOTO 13
                  K = 1
                  IF (D(J) .LE. 0.D0) W(J) = W(J)/BETA
                  GOTO  14
  13              K = 2
                  W(J) = W(J)/BETA
                  W(J+1) = W(J+1)/BETA
  14           CONTINUE
               GOTO  12
  15        CONTINUE
            GOTO  21
  16        J = IBEGIN
C
C ALPHA IS EQUAL TO 0 SO COMPUTE W DIRECTLY
C
            K = 0
  17           J = J+K
               IF (J .GT. N) GOTO  20
               IF (E(J) .NE. 0.D0) GOTO 18
                  K = 1
                  W(J) = 0.D0
                  IF (D(J) .LE. 0.D0) W(J) = DSIGN(1.D0, -Y(J))
                  GOTO  19
  18              K = 2
                  TEMP = (D(J)-D(J+1))/2.
                  DELTA = TEMP-DSQRT(TEMP**2+E(J)**2)
                  SGN = DSIGN(1.D0, E(J)*(-Y(J))+DELTA*(-Y(J+1)))
                  W(J) = E(J)*SGN
                  W(J+1) = DELTA*SGN
  19           CONTINUE
               GOTO  17
  20        CONTINUE
  21  CONTINUE
  22  RETURN
      END
