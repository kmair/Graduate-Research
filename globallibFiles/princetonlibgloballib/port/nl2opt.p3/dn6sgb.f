      SUBROUTINE  DN6SGB(N,P,L,ALF,B,C,Y,CALCA,CALCB,INC,IINC,IV,
     1                  LIV, LV, V)
C
C  ***  SOLVE SEPARABLE NONLINEAR LEAST SQUARES USING  ***
C  ***  ANALYTICALLY COMPUTED DERIVATIVES.             ***
C *** BOUNDS ON NONLINEAR PARAMETERS
C
C  ***  PARAMETER DECLARATIONS  ***
C
      INTEGER IINC, L, LIV, LV, N, P
      INTEGER INC(IINC,P), IV(LIV)
      DOUBLE PRECISION ALF(P), C(L), V(LV), Y(N), B(2,P)
      EXTERNAL CALCA, CALCB
C
C  ***  PURPOSE  ***
C
C GIVEN A SET OF N OBSERVATIONS Y(1)....Y(N) OF A DEPENDENT VARIABLE
C T(1)...T(N),    DNSGBATTEMPTS TO COMPUTE A LEAST SQUARES FIT
C TO A FUNCTION  ETA  (THE MODEL) WHICH IS A LINEAR COMBINATION
C
C                  L
C ETA(C,ALF,T) =  SUM C * PHI(ALF,T) +PHI   (ALF,T)
C                 J=1  J     J           L+1
C
C OF NONLINEAR FUNCTIONS PHI(J) DEPENDENT ON T AND ALF(1),...,ALF(P)
C (.E.G. A SUM OF EXPONENTIALS OR GAUSSIANS).  THAT IS, IT DETERMINES
C NONLINEAR PARAMETERS ALF WHICH MINIMIZE
C
C                   2    N                      2
C     NORM(RESIDUAL)  = SUM  (Y - ETA(C,ALF,T )).
C                       I=1    I             I
C
C SUBJECT TO SIMPLE BOUND CONSTRAINTS B(1,I).LE.X(I).LE.B(2,I)
C THE (L+1)ST TERM IS OPTIONAL.
C
C
C
C  ***  EXTERNAL SUBROUTINES  ***
C
      EXTERNAL DIVSET,   DRNSGB
C
C DIVSET.... PROVIDES DEFAULT IV AND V VALUES.
C   DRNSG... CARRIES OUT NL2SOL ALGORITHM.
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER A1, DA1, I, IN1, IV1, K, L1, LP1, M, M0, NF
C
C  ***  SUBSCRIPTS FOR IV AND V  ***
C
      INTEGER AMAT, D, DAMAT, IN, IVNEED, J, L1SAV, MSAVE, NEXTIV,
     1        NEXTV, NFCALL, NFGCAL, PERM, R, TOOBIG, VNEED
C
C  ***  IV SUBSCRIPT VALUES  ***
C
C/6
C     DATA AMAT/113/, D/27/, DAMAT/114/, IN/112/, IVNEED/3/, J/70/,
C    1     L1SAV/111/, MSAVE/115/, NEXTIV/46/, NEXTV/47/, NFCALL/6/,
C    2     NFGCAL/7/, PERM/58/, R/61/, TOOBIG/2/, VNEED/4/
C/7
      PARAMETER (AMAT=113, D=27, DAMAT=114, IN=112, IVNEED=3, J=70,
     1           L1SAV=111, MSAVE=115, NEXTIV=46, NEXTV=47, NFCALL=6,
     2           NFGCAL=7, PERM=58, R=61, TOOBIG=2, VNEED=4)
C/
C
C++++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++
C
      IF (IV(1) .EQ. 0) CALL DIVSET(1, IV, LIV, LV, V)
      IF (P .LE. 0 .OR. L .LT. 0 .OR. IINC .LE. L) GO TO 50
      IV1 = IV(1)
      IF (IV1 .EQ. 14) GO TO 90
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 90
      IF (IV1 .EQ. 12) IV(1) = 13
      IF (IV(1) .NE. 13) GO TO 60
      IF (IV(PERM) .LE. MSAVE) IV(PERM) = MSAVE + 1
      LP1 = L + 1
      L1 = 0
      M = 0
      DO 40 I = 1, P
         M0 = M
         IF (L .EQ. 0) GO TO 20
         DO 10 K = 1, L
            IF (INC(K,I) .LT. 0 .OR. INC(K,I) .GT. 1) GO TO 50
            IF (INC(K,I) .EQ. 1) M = M + 1
 10         CONTINUE
 20      IF (INC(LP1,I) .NE. 1) GO TO 30
            M = M + 1
            L1 = 1
 30      IF (M .EQ. M0 .OR. INC(LP1,I) .LT. 0
     1                 .OR. INC(LP1,I) .GT. 1) GO TO 50
 40      CONTINUE
C
      IV(IVNEED) = IV(IVNEED) + 2*M
      L1 = L + L1
      IV(VNEED) = IV(VNEED) + N*(L1+M)
      GO TO 60
C
 50   IV(1) = 66
C
 60   CALL   DRNSGB(V,ALF,B,C,V,IV,IV,L,1,N,LIV,LV,N,M,P,V,Y)
      IF (IV(1) .NE. 14) GO TO 130
C
C  ***  STORAGE ALLOCATION  ***
C
      IV(IN) = IV(NEXTIV)
      IV(NEXTIV) = IV(IN) + 2*M
      IV(AMAT) = IV(NEXTV)
      IV(DAMAT) = IV(AMAT) + N*L1
      IV(NEXTV) = IV(DAMAT) + N*M
      IV(L1SAV) = L1
      IV(MSAVE) = M
C
C  ***  SET UP IN ARRAY  ***
C
      IN1 = IV(IN)
      DO 80 I = 1, P
         DO 70 K = 1, LP1
            IF (INC(K,I) .EQ. 0) GO TO 70
               IV(IN1) = I
               IV(IN1+1) = K
               IN1 = IN1 + 2
 70         CONTINUE
 80      CONTINUE
      IF (IV1 .EQ. 13) GO TO 130
C
 90   A1 = IV(AMAT)
      DA1 = IV(DAMAT)
      IN1 = IV(IN)
      L1 = IV(L1SAV)
      M = IV(MSAVE)
C
 100  CALL   DRNSGB(V(A1),ALF,B,C,V(DA1),IV(IN1),IV,L,L1,N,LIV,LV,
     1            N, M, P, V, Y)
      IF (IV(1)-2) 110, 120, 130
C
C  ***  NEW FUNCTION VALUE (R VALUE) NEEDED  ***
C
 110  NF = IV(NFCALL)
      CALL CALCA(N, P, L, ALF,  NF,V(A1))
      IF (NF .LE. 0) IV(TOOBIG) = 1
      GO TO 100
C
C  ***  COMPUTE DR = GRADIENT OF R COMPONENTS  ***
C
 120  CALL CALCB(N, P, L, ALF, IV(NFGCAL), V(DA1))
      IF (IV(NFGCAL) .EQ. 0) IV(TOOBIG) = 1
      GO TO 100
C
 130  RETURN
C
C  ***  LAST CARD OF    DNSGBFOLLOWS  ***
      END
