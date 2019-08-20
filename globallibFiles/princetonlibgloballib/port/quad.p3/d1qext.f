      SUBROUTINE D1QEXT(M,KDIV,NCHECK,TROUND,ANS,ERREST,R,K,LROUND)
C
C CAUTIOUS RICHARDSON POLYNOMIAL EXTRAPOLATION.
C DO MTH EXTRAPOLATION = CALCULATE (M+1)ST ROW IN T-TABLE.
C (1 .LE. M .LE. 12 IS ASSUMED.)
C NEWEST ESTIMATE IS IN TIN.  ERROR IN TIN IS ASSUMED TO BE OF
C THE FORM SUM OF H**(GAMMA*N),N=1,2,3,...
C FOR THE SEQUENCE OF HS USED, VALUES OF 1/H**GAMMA ARE IN ARRAY X.
C USUAL APPLICATION IS TO ROMBERG INTEGRATION BASED ON TRAPEZOIDAL
C RULE--THEN GAMMA = 2.
C
C ON ENTRY, ANS AND ERREST HAVE GUESS AT EXTRAPOLATED VALUE AND AN
C ERROR ESTIMATE FOR IT.  IF THIS EXTRAPOLATION GENERATES A VALUE
C WITH LOWER ESTIMATED ERROR, RETURN NEW VALUES FOR ANS AND ERREST.
C
C T CONTAINS ROW M OF T-TABLE ON ENTRY, AND S CONTAINS THE DIFFERENCE
C BETWEEN ROWS M AND (M-1).  THESE ARE UPDATED BEFORE EXITING.
C
C DONT CHECK ERROR OF NEW VALUES IF NCHECK IS TRUE ON INPUT
C K IS RETURNED TO INDICATE THE DEGREE OF ASYMPTOTICITY OF THE TRIANGLE.
C LROUND IS SET TRUE IF ERROR ESTIMATE IS LESS THAN GUESS AT ROUNDOFF
C VALUES OF RATIOS OF DIFFERENCES OF T-TABLE ENTRIES ARE SAVED IN
C ARRAY R FOR POSSIBLE USE IN CHANGING VARIABLES.
C
      LOGICAL NCHECK,LROUND,NOGOOD,SKIP
      COMMON /D1QCM1/ DUMMY(6),NPRINT,NDUMMY(8)
      COMMON /D1QCM4/ TIN,T(13),S(13),X(13)
      DOUBLE PRECISION TROUND,ANS,ERREST,ERR,DELTA,RDELTA,TNEW,TNEXT
      DOUBLE PRECISION DUMMY,TIN,T,S,X
      DIMENSION R(1)
      DATA TOL/0.05/
C
      JOUT = I1MACH(2)
C
      IF (M.EQ.1 .AND. NPRINT.GT.1) WRITE(JOUT,5) T(1)
    5 FORMAT(1X,1PD15.8,10X,7HT-TABLE)
      SKIP = NCHECK
      TNEW = TIN
      K = 0
C
      DO 100 J = 1,M
C
C           TIN IS COLUMN 1 IN NEW ROW
C           EXTRAPOLATE TO GET COLUMN J+1, AND ERROR IN COLUMN J.
C
         DELTA = TNEW-T(J)
         ERR = DABS(DELTA)
         JJ = M+1-J
         RDELTA = DELTA*X(JJ)/(X(M+1)-X(JJ))
         TNEXT = TNEW+RDELTA
C
         IF (SKIP .OR. (J.EQ.M .AND. M.EQ.2 .AND. K.LT.2)) GO TO 60
            IF (ERR.LE.TROUND .OR.  J.EQ.M) GO TO 40
C
C                 ESTIMATE ERROR IN TNEW.
C                 ALL PREVIOUS COLUMNS WERE ASYMPTOTIC, OR ALMOST SO.
C
               RTEMP = (X(JJ-1)*(X(M+1)-X(JJ))*S(J))/
     X                 (X(M+1)*(X(M)-X(JJ-1))*DELTA)
               IF (ABS(RTEMP-1.) .GT. TOL*FLOAT(J)) GO TO 20
C
C                   ASYMPTOTIC COLUMN. USE RUNGE ERROR ESTIMATE.
C
                  ERR = DABS(RDELTA)
                  K = K+2
                  GO TO 40
C
C                    NON-ASYMPTOTIC COLUMN. USE MORE CONSERVATIVE
C                    ERROR ESTIMATE.
C
   20             SKIP = J.GT.1
                  IF (J.EQ.1 .AND. M.LT.KDIV) GO TO 60
                    IF (J.EQ.1) ERR = 2.*(ERR+DABS(S(1)))
                    NOGOOD = ABS(RTEMP-1.) .GT. TOL*FLOAT(J+4)
                    IF(.NOT. NOGOOD) K = K+1
                    SKIP = SKIP .OR. NOGOOD
                    IF ((RTEMP.LT.0.25 .OR. RTEMP.GT.4.0)
     X                  .AND. J.GT.1) GO TO 60
C
C                 SAVE NEW VALUES IF LOWER ERROR ESTIMATE
C
   40          IF (ERR.GT.ERREST) GO TO 60
                  ANS = TNEW
                  ERREST = ERR
C
C           SHIFT FOR NEXT EXTRAPOLATION. SAVE FIRST 3 RATIOS.
C
   60    IF (J.GT.MIN0(3,M-1)) GO TO 80
            R(J) = 0.
            IF (ERR.GT.0.D0) R(J) = S(J)/DELTA
   80    S(J) = DELTA
         T(J) = TNEW
         TNEW = TNEXT
  100    CONTINUE
C
      T(M+1) = TNEW
      LROUND = ERREST.LE.TROUND
      MP =M+1
      IF (NPRINT.GT.1) WRITE(JOUT,15) (T(J),J = 1,MP)
   15 FORMAT(1X,1P8D15.8)
      RETURN
      END
