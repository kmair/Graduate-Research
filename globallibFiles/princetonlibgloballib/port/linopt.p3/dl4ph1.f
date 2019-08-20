      SUBROUTINE DL4PH1(A,M, N, AMAN, IA, B,  X, ITRMAX, CTX, S, SIMP,
     1  ISIMP, E, W, Q,IQ, LT, P, V, SCALE, IPTS, IPTG, DVECS, DVECG
     1   ,TT,U1,PRINT, RHS,AS,AG,KK,CC,IERR)
C THIS IS PHASE2 OF THE LINEAR PROGRAMMING PACKAGE
C
C THE PARAMTERS HAVE THE FOLLOWING INTERPRETATIONS
C A  SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION AMAN.
C M  NUMBER OF GENERAL EQUALITY AND INEQUALITY CONSTRAINTS
C N  NUMBER OF UNKNOWNS
C AMAN - USER PROVIDED FUNCTION WHICH EITHER RETURNS A ROW
C    OF THE CONSTRAINT MATRIX OR DOES A MATRIX-VECTOR
C    INNER PRODUCT
C IA  INTEGER SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION
C     AMAN
C B   RIGHT HAND SIDE CONSTRAINT VECTOR
C X   FEASIBLE VECTOR ON INPUT, SOLUTION ON OUTPUT
C ITRMAX   MAXIMUM NUMBER OF ITERATIONS TOLERATED.
C CTX ON OUTPUT THE COST FUNCTION TO BE MAXIMIZED
C S   NUMBER OF SIMPLE CONSTRAINTS
C SIMP VECTOR GIVING LOWER OR UPPERBOUND
C ISIMP VECTOR TELLING ON WHICH ELEMENT THE SIMPLE CONSTRAINT
C     PERTAINS. IF NEGATIVE IT IS AN UPPER BOUND
C E   NUMBER OF EQUALITY CONSTRAINTS
C W   M VECTOR OF RESIDUALS
C Q   AN N XN ARRAY STORING Q FACTOR OF LQ FACTORIZATION OF ACTIVE
C     CONSTRAINT MATRIX
C LT  AN N X N ARRAY STORYING THE TRANPOSE OF L IN QL FACTORIZATION
C P   SCRATCH VECTOR WHICH WILL CONTAIN SEARCH DIRECTION
C V   SCRATCH VECTOR LENGTH M WHICH WILL CONTAIN INACTIVE CONSTRAINT
C     MATRIX TIMES P
C SCALE SCRATCH VECTOR WHICH WILL CONTAIN NORM OF CONSTRAINT ROWS
C IPTS SCRATCH VECTOR GIVING PERMUTATION OF SIMPLE CONSTRAINTS
C IPTG SCRATCH VECTOR POINTING TO GENERAL CONSTRAINTS
C DVECS SCRATCH VECTOR STATING WHETHER SIMPLE CONSTAINT HAS BEEN
C       DROPPED-TO PREVENT CYCLING
C DVECG SCRATCH VECTOR STATING WHETHER GENERAL CONSTRAINT HAS
C       BEEN DROPPED-TO PREVENT CYCLING
C TT    PURE SCRATCH VECTOR
C U1    SCRATCH VECTOR TO STORE LAGRANGE MULTIPLIERS
C PRINT   - USER WRITTEN SUBROUTINE WHICH  PRINTS STUFF EACH ITER.
      INTEGER M, N, S, ISIMP(1),IA(1)
      EXTERNAL AMAN,PRINT
      LOGICAL ISTOP,FIRST
      INTEGER ITRMAX
      DOUBLE PRECISION A(1), B(1),  X(N), CTX, W(1), ET
       DOUBLE PRECISION TT(N),RHS(N)
      DOUBLE PRECISION Q(IQ, N), LT( N), P(N), V(1), SCALE(1)
      INTEGER  NMS,  AGP1,  E
      INTEGER I, K, AGPE, FLAG, IPTG(1)
      INTEGER IPTS(N), INDX2, AG, II, AS, DVECG(1)
      INTEGER KK, DVECS(1),   ITRPH1
      LOGICAL DONE
      DOUBLE PRECISION EPS,  CNRM, TEMP, TOLL, DDOT,C
      DOUBLE PRECISION PNRM, SIMP(1), TOLU, XNRM, U1(1),  CC(1)
      DOUBLE PRECISION  DNRM2,  THETA,DFLOAT
      DOUBLE PRECISION CTEMP,  D1MACH, UMAX,BOUND, BIGBND
      INTEGER ISMA(1000)
      COMMON /CSTAK/ISMA
      CALL ENTER(0)
      MAXMN=MAX0(M,N)
      MAXSM=MAX0(S,M)
      ICYCL=0
      IIHIT=ISTKGT(MAXMN, 2)
      INVHIT=1
      IF(MAXSM.NE.0) INVHIT=ISTKGT(MAXSM, 2)
      EPS =DFLOAT(N)*D1MACH(4)*10.0D0
      BIGBND=D1MACH(2)*(1.0D0-EPS)
      BOUND=EPS*100.0D0
      IHIT=0
      TOLL = 1.D0 + D1MACH(4)
       ISTOP=.FALSE.
      TOLU = 1.D0 - D1MACH(4)
      XNRM = DNRM2(N, X, 1)
      JDEPS=0
      JDEPG=E
      IF (M .EQ. 0) GOTO 20
      DO  10 II = 1, M
         V(II)=0.0D0
         DVECG(II) = 0
 10      CONTINUE
 20      CONTINUE
      DO 25 I=1,N
 25      P(I)=0.0D0
       IF (S .EQ. 0) GOTO 40
       DO 30 II=1,S
           IF (ISIMP(II).LT.0.AND.SIMP(II).GT.BIGBND)SIMP(II)=BIGBND
           IF (ISIMP(II).GT.0.AND.SIMP(II).LT.-BIGBND)SIMP(II)=-BIGBND
          DVECS(II) = 0
 30       CONTINUE
 40    CONTINUE
      AS = 0
      AG = 0
      KK=E
      IERR=0
      INDX2=0
C
C HANDLE EQUALITY CONSTRAINTS
C
       IF (E.GT.0)
     1CALL DE4QL(A,N,AMAN,IA,E,Q,IQ,TT,B,X,RHS,V,LT)
C
C CREATE RESIDUAL VECTOR
C
       IF (M.EQ.0) GO TO 60
       DO 50 I=1,M
          CALL AMAN(.TRUE.,A,IA,N,I,X,TEMP)
          W(I)=B(I)-TEMP
 50    CONTINUE
 60    CONTINUE
      ITRPH1 = 1
         GOTO  80
 70      ITRPH1 = ITRPH1+1
 80      IF (ITRPH1 .GT. ITRMAX) GOTO  210
       FIRST=.TRUE.
       MMAG = M - AG - E
C
C GET NEW GRADIENT
C
           AGPE=AG+E
       IST=AGPE
       IF (JDEPS.EQ.AS) GO TO 92
       JDEPS1=JDEPS+1
       DO 91 I=JDEPS1,AS
          II=ISIMP(I)
          X(II)=SIMP(I)
91     CONTINUE
92     IST=JDEPG
 90    CONTINUE
       CALL DG4ETC(N,M,S,AS,AGPE,A,IA,AMAN,X,CC,RHS,
     1     W,SCALE,Q,IQ,IPTS,ISIMP,SIMP,IPTG,DONE,CTX,
     2     CNRM,IST,FIRST)
       IF (DONE.AND.FIRST)GOTO 233
       IF (DONE.AND..NOT.FIRST)GO TO 230
       IF (ITRPH1.EQ.1) GO TO 95
C
C CALL SUBROUTINE TO TEST IF SIMPLE CONSTRAINTS SHOULD BE
C ADDED AND THEN ADD THEM AND UPDATE LQ DECOMPOSTION
C
         NMS=N-AS
          ISTRIK=IHIT-M
         IF (NMS .NE. 0) CALL DA4PPS(A, M, N, IA, KK,S, Q,IQ, LT, AS,
     1      AG, E, IPTS, DVECS, X, SIMP, ISIMP, TOLL, TOLU, EPS, TT,
     2    IPRINT,RHS,INDX2,P,1, JDEPS,ISMA(IIHIT),ISMA(INVHIT),ISTRIK)
C
C CALL SUBROUTINE TO TEST IF GENERAL CONSTRAINTS SHOULD BE
C ADDED AND THEN ADD THEM AND UPDATE LQ DECOMPOSITION
C
         IF (MMAG .NE. 0) CALL DA4PPG(A, M, N, IA, KK, Q,IQ, LT, AG,
     1      AS,E,IPTG,IPTS,W,SCALE,XNRM,AMAN,EPS,TT,P,IPRINT,RHS,
     2      INDX2,V,JDEPG,ISMA(IIHIT),ISMA(INVHIT),IHIT,DVECG)
 95         AGPE=AG+E
         FLAG=1
       IF (ISTOP)GOTO 233
C
C DETERMINE WHICH CONSTRAINT TO DROP
C AND DROP THEM
C
C COMPUTE LAGRANGE MULTIPLIERS
C
            IF (AGPE .EQ. 0) GOTO 100
C
C COMPUTE LAGRANGE MULTIPLIERS FOR GENERAL CONSTRAINTS
C
            ASP1 = AS + 1
          CALL DM4TOP(AGPE,LT,RHS,U1)
 100        CONTINUE
            AGP1=AG+E+1
C
C COMPUTE LAGRANGE MULTIPLIERS FOR SIMPLE CONSTRAINTS
C
            IF (AS .NE. 0) CALL DL4AGS(A,M,N,IA,AMAN,AS,U1,U1(AGP1),CC,
     1           ISIMP,AGPE,IPTS,IPTG,TT)
C
C DETERMINE WHICH CONSTRAINT TO DROP GIVEN THE LAGRANGE MULTIPLIERS
C
       CALL PRINT(A,M,N,AMAN,IA,B,C,X,CTX,S,SIMP,ISIMP,E,ITRPH1,
     1 IPTG,AG,AS,U1,ISTOP,1)
       IF (ISTOP)GOTO 231
         IF (E .GE. KK) GOTO 150
C
 110        CALL DD4CLM(U1, AG, AS, E, UMAX, INDX2, FLAG, DVECS, DVECG,
     1         IPTG, IPTS, BOUND,1,KK,N,ICYCL)
            IF (FLAG .NE. 1) GOTO 120
            INDX2=0
            GOTO 150
 120   IF(INDX2.LE.AGPE) GO TO 130
C
C SINCE A SIMPLE CONSTRAINT IS TO BE DROPPED, UPDATE APPROPRIATE
C ARRAYS AND THE LQ DECOMPOSITION ACCORDINGLY
C
          IND2=INDX2-AGPE
          ITEMP=IPTS(IND2)
          NMS=N-AS
          RHS(NMS+1)=CC(ITEMP)
          IF (AS .NE. 0) CALL DD4RPS(A,M,N,IA,AMAN,KK,Q,IQ,LT,AG,AS,E,
     1   IPTG,IPTS,DVECS,SIMP,ISIMP,INDX2,TT,P, RHS)
          INDX2=1
          GOTO 140
 130      CONTINUE
C
C SINCE A GENERAL CONSTRAINT IS TO BE DROPPED, UPDATE APPROPRIATE
C ARRAYS AND LQ DECOMPOSITION ACCORDINGLY
C
       IF (AG .NE. 0) CALL DD4RPG (M,N,KK,Q,IQ,LT,AG,AS,E,IPTG,INDX2
     1                ,DVECG,RHS,CC,IPTS)
       INDX2=-1
 140   CONTINUE
       JDEPS=AS
       JDEPG=AG+E
 150   CONTINUE
C COMPUTE NEW SEARCH DIRECTION
C
         CALL DP4RJD(N, AS, AG, E, Q,IQ, IPTS, CC, P, TT,RHS)
         IF(JDEPS.EQ.AS) GO TO 132
         ASP1=AS+1
         DO 131 I=ASP1,JDEPS
            I2=ISIMP(I)
 131     P(I2)=0.0D0
 132      CONTINUE
         PNRM = DNRM2(N, P, 1)
C
C CHECK IF FINISHED
C
         IF (KK .EQ. N ) GO TO 230
         IF (ITRPH1.EQ.1)
     1 CALL PRINT(A,M,N,AMAN,IA,B,C,X,CTX,S,SIMP,ISIMP,E,ITRPH1,
     1 IPTG,AG,AS,U1,ISTOP,1)
          IF (PNRM.LT.EPS.AND.FLAG.NE.1) GO TO 95
         IF (PNRM.LT.EPS.AND..NOT.FIRST)GO TO 230
          FIRST=.FALSE.
         IF (PNRM.LT.EPS) GO TO 90
C
C DETERMINE HOW FAR ONE SHOULD PROCEED IN THE SEARCH DIRECTION
C
           AGPE=AG+E
           CALL DC4ONS(A,M,N,IA,S,P,V,W,JDEPG,JDEPS,AS,PNRM,IPTS
     1  ,IPTG,SIMP,ISIMP, AMAN,EPS,SCALE,X,THETA,TT,IHIT,IERR)
         CTEMP=DDOT(N,CC,1,P,1)
         IF (0.0D0 .LE. CTEMP) GOTO 160
            IERR=9
            GOTO 231
 160        CTX = CTX+THETA*CTEMP
            DO  170 K = 1, N
               X(K) = X(K)+THETA*P(K)
 170           CONTINUE
            IF (IHIT.LE.M) GO TO 180
                I1=IHIT-M
                I2=IABS(ISIMP(I1))
                X(I2)=SIMP(I1)
 180      CONTINUE
            AGP1 = AG+E+1
            IF(M.LT.AGP1) GO TO 200
C
C UPDATE RESIDUALS
C
            DO  190 I = AGP1, M
               K = IPTG(I)
               W(K) = W(K)-THETA*V(K)
 190           CONTINUE
 200        CONTINUE
            IF (IHIT.LE.M.AND.IHIT.GT.0)W(IHIT)=0.0D0
         GOTO  70
 210   IERR=6
 220  GOTO231
 230  IERR=8
 231   CALL LEAVE
      RETURN
 233   CONTINUE
       CALL LEAVE
      IF(S.EQ.0)RETURN
      DO 234 I=1,S
         ET=1.0D0
         IF(ISIMP(I).LT.0)ET=-1.0D0
         I1=IABS(ISIMP(I))
         IF (ET*X(I1).LT.ET*SIMP(I))X(I1)=SIMP(I)
234   CONTINUE
      RETURN
      END
