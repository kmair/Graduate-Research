      SUBROUTINE G2SVD(NAU,NV,M,N,A,W,MATU,U,MATV,V,B,IRHS,RV1)
C
      INTEGER I,J,K,L,M,N,II,I1,KK,K1,LL,L1,MN,NAU,NV,ITS,IRHS
      REAL A(NAU,N),W(N),U(NAU,N),V(NV,N),B(NAU,IRHS),RV1(N)
      REAL C,F,G,H,S,X,Y,Z,EPS,SCALE,MACHEP,R1MACH
      REAL SQRT,AMAX1,ABS,SIGN
      LOGICAL MATU,MATV
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE SVD,
C     NUM. MATH. 14, 403-420(1970) BY GOLUB AND REINSCH.
C     HANDBOOK FOR AUTO. COMP., VOL II-LINEAR ALGEBRA, 134-151(1971).
C
C     THIS SUBROUTINE DETERMINES THE SINGULAR VALUE DECOMPOSITION
C          T
C     A=UWV  OF A REAL M BY N RECTANGULAR MATRIX.  HOUSEHOLDER
C     BIDIAGONALIZATION AND A VARIANT OF THE QR ALGORITHM ARE USED.
C     G2SVD ASSUMES M .GE. N.  IF M .LT. N, THEN COMPUTE THE SINGULAR
C                             T       T    T            T
C     VALUE DECOMPOSITION OF A .  IF A =UWV , THEN A=VWU .
C
C     G2SVD CAN ALSO BE USED TO COMPUTE THE MINIMAL LENGTH LEAST SQUARES
C     SOLUTION TO THE OVERDETERMINED LINEAR SYSTEM A*X=B.
C
C     ON INPUT -
C
C        NAU MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL
C          ARRAY PARAMETERS A,U AND B AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.  NOTE THAT NAU MUST BE AT LEAST
C          AS LARGE AS M,
C
C        NV MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL
C          ARRAY PARAMETER V AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT. NV MUST BE AT LEAST AS LARGE AS N,
C
C        M IS THE NUMBER OF ROWS OF A (AND U),
C
C        N IS THE NUMBER OF COLUMNS OF A (AND U) AND THE ORDER OF V,
C
C        A CONTAINS THE RECTANGULAR INPUT MATRIX TO BE DECOMPOSED,
C
C        B CONTAINS THE IRHS RIGHT-HAND-SIDES OF THE OVERDETERMINED
C         LINEAR SYSTEM A*X=B. IF IRHS .GT. 0,
C         THEN ON OUTPUT, THESE IRHS COLUMNS
C                       T
C         WILL CONTAIN U B. THUS, TO COMPUTE THE MINIMAL LENGTH LEAST
C                                               +
C         SQUARES SOLUTION, ONE MUST COMPUTE V*W  TIMES THE COLUMNS OF
C                   +                        +
C         B, WHERE W  IS A DIAGONAL MATRIX, W (I)=0 IF W(I) IS
C         NEGLIGIBLE, OTHERWISE IS 1/W(I). IF IRHS=0, B MAY COINCIDE
C         WITH A OR U AND WILL NOT BE REFERENCED,
C
C        IRHS IS THE NUMBER OF RIGHT-HAND-SIDES OF THE OVERDETERMINED
C         SYSTEM A*X=B. IRHS SHOULD BE SET TO ZERO IF ONLY THE SINGULAR
C         VALUE DECOMPOSITION OF A IS DESIRED,
C
C        MATU SHOULD BE SET TO .TRUE. IF THE U MATRIX IN THE
C          DECOMPOSITION IS DESIRED, AND TO .FALSE. OTHERWISE,
C
C        MATV SHOULD BE SET TO .TRUE. IF THE V MATRIX IN THE
C          DECOMPOSITION IS DESIRED, AND TO .FALSE. OTHERWISE.
C
C     ON OUTPUT -
C
C        A IS UNALTERED (UNLESS OVERWRITTEN BY U OR V),
C
C        W CONTAINS THE N (NON-NEGATIVE) SINGULAR VALUES OF A (THE
C          DIAGONAL ELEMENTS OF W).  THEY ARE UNORDERED.  IF AN
C          ERROR EXIT IS MADE, THE SINGULAR VALUES SHOULD BE CORRECT
C          FOR INDICES IERR-10+1,IERR-10+2,...,N,
C
C        U CONTAINS THE MATRIX U (ORTHOGONAL COLUMN VECTORS) OF THE
C          DECOMPOSITION IF MATU HAS BEEN SET TO .TRUE.  OTHERWISE
C          U IS USED AS A TEMPORARY ARRAY.  U MAY COINCIDE WITH A.
C          IF AN ERROR EXIT IS MADE, THE COLUMNS OF U CORRESPONDING
C          TO INDICES OF CORRECT SINGULAR VALUES SHOULD BE CORRECT,
C
C        V CONTAINS THE MATRIX V (ORTHOGONAL) OF THE DECOMPOSITION IF
C          MATV HAS BEEN SET TO .TRUE.  OTHERWISE V IS NOT REFERENCED.
C          V MAY ALSO COINCIDE WITH A IF U IS NOT NEEDED.  IF AN ERROR
C          EXIT IS MADE, THE COLUMNS OF V CORRESPONDING TO INDICES OF
C          CORRECT SINGULAR VALUES SHOULD BE CORRECT,
C
C        ERROR CONDITIONS-
C          10+K       IF THE K-TH SINGULAR VALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS,
C          1          IF IRHS .LT. 0 .
C          2          IF M .LT. N .
C          3          IF NAU .LT.M .
C          4          IF NV .LT. N .
C
C        RV1 IS A TEMPORARY STORAGE ARRAY (OF LENGTH N).
C
C
C     THIS SUBROUTINE HAS BEEN CHECKED BY THE PFORT VERIFIER
C     (RYDER,B.G. THE PFORT VERIFIER, SOFTWARE - PRACTICE AND
C     EXPERIENCE, VOL.4, 359-377, 1974) FOR ADHERENCE TO A LARGE,
C     CAREFULLY DEFINED, PORTABLE SUBSET OF AMERICAN NATIONAL STANDARD
C     FORTRAN CALLED PFORT.
C
C     ORIGINAL VERSION OF THIS CODE IS SUBROUTINE SVD IN RELEASE 2 OF
C         EISPACK.
C     MODIFIED BY TONY CHAN, COMP. SCI. DEPT. STANFORD UNIV.,CA94305.
C     LAST MODIFIED  2 SEPTEMBER, 1976.
C
C     ------------------------------------------------------------------
C
       MACHEP = R1MACH(4)
C
      DO 100 I = 1, M
C
         DO 100 J = 1, N
            U(I,J) = A(I,J)
  100 CONTINUE
C     ********** HOUSEHOLDER REDUCTION TO BIDIAGONAL FORM **********
      G = 0.0
      SCALE = 0.0
      X = 0.0
C
      DO 300 I = 1, N
         L = I + 1
         RV1(I) = SCALE * G
         G = 0.0
         S = 0.0
         SCALE = 0.0
C
C     COMPUTE LEFT TRANSFORMATIONS THAT ZEROS THE SUBDIAGONAL ELEMENTS
C         OF THE I-TH COLUMN.
C
         DO 120 K = I, M
  120    SCALE = SCALE + ABS(U(K,I))
C
         IF (SCALE .EQ. 0.0) GO TO 210
C
         DO 130 K = I, M
            U(K,I) = U(K,I) / SCALE
            S = S + U(K,I)**2
  130    CONTINUE
C
         F = U(I,I)
         G = -SIGN(SQRT(S),F)
         H = F * G - S
         U(I,I) = F - G
         IF (I .EQ. N) GO TO 155
C
C     APPLY LEFT TRANSFORMATIONS TO REMAINING COLUMNS OF A
C
         DO 150 J = L, N
            S = 0.0
C
            DO 140 K = I, M
  140       S = S + U(K,I) * U(K,J)
C
            F = S / H
C
            DO 150 K = I, M
               U(K,J) = U(K,J) + F * U(K,I)
  150    CONTINUE
C
C     APPLY LEFT TRANSFORMATIONS TO THE COLUMNS OF B IF IRHS .GT. 0.
C
 155      IF (IRHS .EQ. 0) GO TO 190
          DO 160 J=1,IRHS
              S=0.0
              DO 170 K=I,M
  170             S = S + U(K,I)*B(K,J)
              F = S/H
              DO 180 K=I,M
  180             B(K,J) = B(K,J) + F*U(K,I)
  160     CONTINUE
C
C     COMPUTE RIGHT TRANSFORMATIONS.
C
  190    DO 200 K = I, M
  200    U(K,I) = SCALE * U(K,I)
C
  210    W(I) = SCALE * G
         G = 0.0
         S = 0.0
         SCALE = 0.0
         IF (I .GT. M .OR. I .EQ. N) GO TO 290
C
         DO 220 K = L, N
  220    SCALE = SCALE + ABS(U(I,K))
C
         IF (SCALE .EQ. 0.0) GO TO 290
C
         DO 230 K = L, N
            U(I,K) = U(I,K) / SCALE
            S = S + U(I,K)**2
  230    CONTINUE
C
         F = U(I,L)
         G = -SIGN(SQRT(S),F)
         H = F * G - S
         U(I,L) = F - G
C
         DO 240 K = L, N
  240    RV1(K) = U(I,K) / H
C
         IF (I .EQ. M) GO TO 270
C
         DO 260 J = L, M
            S = 0.0
C
            DO 250 K = L, N
  250       S = S + U(J,K) * U(I,K)
C
            DO 260 K = L, N
               U(J,K) = U(J,K) + S * RV1(K)
  260    CONTINUE
C
  270    DO 280 K = L, N
  280    U(I,K) = SCALE * U(I,K)
C
  290    X = AMAX1(X,ABS(W(I))+ABS(RV1(I)))
  300 CONTINUE
C     ********** ACCUMULATION OF RIGHT-HAND TRANSFORMATIONS **********
      IF (.NOT. MATV) GO TO 410
C     ********** FOR I=N STEP -1 UNTIL 1 DO -- **********
      DO 400 II = 1, N
         I = N + 1 - II
         IF (I .EQ. N) GO TO 390
         IF (G .EQ. 0.0) GO TO 360
C
         DO 320 J = L, N
C     ********** DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW **********
  320    V(J,I) = (U(I,J) / U(I,L)) / G
C
         DO 350 J = L, N
            S = 0.0
C
            DO 340 K = L, N
  340       S = S + U(I,K) * V(K,J)
C
            DO 350 K = L, N
               V(K,J) = V(K,J) + S * V(K,I)
  350    CONTINUE
C
  360    DO 380 J = L, N
            V(I,J) = 0.0
            V(J,I) = 0.0
  380    CONTINUE
C
  390    V(I,I) = 1.0
         G = RV1(I)
         L = I
  400 CONTINUE
C     ********** ACCUMULATION OF LEFT-HAND TRANSFORMATIONS **********
  410 IF (.NOT. MATU) GO TO 510
C     **********FOR I=MIN(M,N) STEP -1 UNTIL 1 DO -- **********
      MN = N
      IF (M .LT. N) MN = M
C
      DO 500 II = 1, MN
         I = MN + 1 - II
         L = I + 1
         G = W(I)
         IF (I .EQ. N) GO TO 430
C
         DO 420 J = L, N
  420    U(I,J) = 0.0
C
  430    IF (G .EQ. 0.0) GO TO 475
         IF (I .EQ. MN) GO TO 460
C
         DO 450 J = L, N
            S = 0.0
C
            DO 440 K = L, M
  440       S = S + U(K,I) * U(K,J)
C     ********** DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW **********
            F = (S / U(I,I)) / G
C
            DO 450 K = I, M
               U(K,J) = U(K,J) + F * U(K,I)
  450    CONTINUE
C
  460    DO 470 J = I, M
  470    U(J,I) = U(J,I) / G
C
         GO TO 490
C
  475    DO 480 J = I, M
  480    U(J,I) = 0.0
C
  490    U(I,I) = U(I,I) + 1.0
  500 CONTINUE
C     ********** DIAGONALIZATION OF THE BIDIAGONAL FORM **********
  510 EPS = MACHEP * X
C     ********** FOR K=N STEP -1 UNTIL 1 DO -- **********
      DO 700 KK = 1, N
         K1 = N - KK
         K = K1 + 1
         ITS = 0
C     ********** TEST FOR SPLITTING.
C                FOR L=K STEP -1 UNTIL 1 DO -- **********
  520    DO 530 LL = 1, K
            L1 = K - LL
            L = L1 + 1
            IF (ABS(RV1(L)) .LE. EPS) GO TO 565
C     ********** RV1(1) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP **********
            IF (ABS(W(L1)) .LE. EPS) GO TO 540
  530    CONTINUE
C     ********** CANCELLATION OF RV1(L) IF L GREATER THAN 1 **********
  540    C = 0.0
         S = 1.0
C
         DO 560 I = L, K
            F = S * RV1(I)
            RV1(I) = C * RV1(I)
            IF (ABS(F) .LE. EPS) GO TO 565
            G = W(I)
            H = SQRT(F*F+G*G)
            W(I) = H
            C = G / H
            S = -F / H
C
C     APPLY LEFT TRANSFORMATIONS TO B IF IRHS .GT. 0.
C
              IF (IRHS .EQ. 0) GO TO 542
              DO 545 J=1,IRHS
                  Y=B(L1,J)
                  Z=B(I,J)
                  B(L1,J) = Y*C + Z*S
                  B(I,J) = -Y*S + Z*C
  545         CONTINUE
  542         CONTINUE
C
            IF (.NOT. MATU) GO TO 560
C
            DO 550 J = 1, M
               Y = U(J,L1)
               Z = U(J,I)
               U(J,L1) = Y * C + Z * S
               U(J,I) = -Y * S + Z * C
  550       CONTINUE
C
  560    CONTINUE
C     ********** TEST FOR CONVERGENCE **********
  565    Z = W(K)
         IF (L .EQ. K) GO TO 650
C     ********** SHIFT FROM BOTTOM 2 BY 2 MINOR **********
         IF (ITS .EQ. 30) GO TO 1000
         ITS = ITS + 1
         X = W(L)
         Y = W(K1)
         G = RV1(K1)
         H = RV1(K)
         F=0.5*(((G+Z)/H)*((G-Z)/Y)+Y/H-H/Y)
         SCALE=ABS(F)+1.0
         G=SCALE*SQRT((F/SCALE)**2+(1.0/SCALE)**2)
         F=X-(Z/X)*Z+(H/X)*(Y/(F+SIGN(G,F))-H)
C     ********** NEXT QR TRANSFORMATION **********
         C = 1.0
         S = 1.0
C
         DO 600 I1 = L, K1
            I = I1 + 1
            G = RV1(I)
            Y = W(I)
            H = S * G
            G = C * G
            SCALE=ABS(F)+ABS(H)
            Z=SCALE*SQRT((F/SCALE)**2+(H/SCALE)**2)
            RV1(I1) = Z
            C = F / Z
            S = H / Z
            F = X * C + G * S
            G = -X * S + G * C
            H = Y * S
            Y = Y * C
            IF (.NOT. MATV) GO TO 575
C
            DO 570 J = 1, N
               X = V(J,I1)
               Z = V(J,I)
               V(J,I1) = X * C + Z * S
               V(J,I) = -X * S + Z * C
  570       CONTINUE
C
  575       Z=ABS(F)+ABS(H)
            IF(Z.NE.0.0)Z=Z*SQRT((F/Z)**2+(H/Z)**2)
            W(I1) = Z
C     ********** ROTATION CAN BE ARBITRARY IF Z IS ZERO **********
            IF (Z .EQ. 0.0) GO TO 580
            C = F / Z
            S = H / Z
  580       F = C * G + S * Y
            X = -S * G + C * Y
C
C     APPLY LEFT TRANSFORMATIONS TO B IF IRHS .GT. 0.
C
              IF (IRHS .EQ. 0) GO TO 582
              DO 585 J=1,IRHS
                  Y = B(I1,J)
                  Z = B(I,J)
                  B(I1,J) = Y*C + Z*S
                  B(I,J) = -Y*S + Z*C
  585         CONTINUE
  582         CONTINUE
C
            IF (.NOT. MATU) GO TO 600
C
            DO 590 J = 1, M
               Y = U(J,I1)
               Z = U(J,I)
               U(J,I1) = Y * C + Z * S
               U(J,I) = -Y * S + Z * C
  590       CONTINUE
C
  600    CONTINUE
C
         RV1(L) = 0.0
         RV1(K) = F
         W(K) = X
         GO TO 520
C     ********** CONVERGENCE **********
  650    IF (Z .GE. 0.0) GO TO 700
C     ********** W(K) IS MADE NON-NEGATIVE **********
         W(K) = -Z
         IF (.NOT. MATV) GO TO 700
C
         DO 690 J = 1, N
  690    V(J,K) = -V(J,K)
C
  700 CONTINUE
C
      GO TO 1001
C     SET ERROR -- NO CONVERGENCE TO A
C     SINGULAR VALUE AFTER 30 ITERATIONS
C/6S
C1000 CALL SETERR(35HG2SVD- NO CONVERGENCE AFTER 30 ITER,35,K+10,1)
C/7S
 1000 CALL SETERR('G2SVD- NO CONVERGENCE AFTER 30 ITER',35,K+10,1)
C/
 1001 RETURN
      END
