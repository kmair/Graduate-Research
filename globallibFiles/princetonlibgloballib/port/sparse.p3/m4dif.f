        SUBROUTINE  M4DIF
     *     (N,JCOL,IA,JA,MAX,V,L,HEAD,LAST,NEXT,MARK,TAG,FLAG,IBIG,MA)
        INTEGER   IA(1),JA(N),  V(N), L(N),  HEAD(N), LAST(N), NEXT(N),
     *     MARK(N), TAG,  FLAG,  SFS, VI,DVI, VJ
         EXTERNAL JCOL
C
C----INITIALIZE DEGREES, ELEMENT LISTS, AND DEGREE LISTS
        DO 1 VI=1,N
          MARK(VI) = 1
          L(VI) = 0
   1      HEAD(VI) = 0
        SFS = N+1
C
C----CREATE NONZERO STRUCTURE-
C----FOR EACH NONZERO ENTRY A(VI,VJ) IN STRICT UPPER TRIANGLE
         IBIG=N
        LIM=N/2+1
        DO 3 VI=1,N
           IF(MARK(VI).LT.0) GO TO 3
           IF (MA.EQ.0) GO TO 103
C
C MATRIX INPUT
C
              JMIN=IA(VI)
              JMAX=IA(VI+1)-1
              NUM=JMAX-JMIN+1
              GO TO 133
 103     CONTINUE
           CALL JCOL(VI,JA,NUM)
           JMIN=1
           JMAX=NUM
 133       CONTINUE
           IF (NUM.LT.1) GO TO 102
          IF (NUM.LE.LIM) GO TO 10
             CALL V4ELIM(N,L,V,VI,MARK,IBIG)
             GO TO 3
 10       CONTINUE
          M=MARK(VI)-1
          LKK=L(VI)
          DO 2 J=JMIN,JMAX
            VJ = JA(J)
            IF (VJ.LT.1.OR.VJ.GT.N) GO TO 105
            IF (VI.EQ.VJ.OR.MARK(VJ).LT.0)  GO TO 2
            IF (VI.LT.VJ)GO TO 30
                 LK=LKK
                  IF (M.EQ.0) GO TO 12
                 DO 11 I=1,M
                    IF (V(LK).EQ.VJ) GO TO 2
                    LK=L(LK)
  11              CONTINUE
  12             CONTINUE
  30          IF (SFS.GE.MAX)  GO TO 101
C
C------ENTER VJ IN ELEMENT LIST FOR VI
              MARK(VI) = MARK(VI) + 1
              V(SFS) = VJ
              L(SFS) = L(VI)
              L(VI) = SFS
              SFS = SFS+1
C
C------ENTER VI IN ELEMENT LIST FOR VJ
              MARK(VJ) = MARK(VJ) + 1
              V(SFS) = VI
              L(SFS) = L(VJ)
              L(VJ) = SFS
              SFS = SFS+1
              IF (MARK(VJ).GE.LIM)CALL V4ELIM(N,L,V,VJ,MARK,IBIG)
              IF (MARK(VI).LT.LIM) GO TO 2
                  CALL V4ELIM(N,L,V,VI,MARK,IBIG)
                  GO TO 3
   2        CONTINUE
   3      CONTINUE
C
C----CREATE DEGREE LISTS AND INITIALIZE MARK VECTOR
        IB=IBIG+1
        DO 4 VI=1,N
          IF (MARK(VI).GT.0) GO TO 104
              NEXT(VI)=-IB
              IB=IB+1
              GO TO 4
 104      CONTINUE
          DVI = MARK(VI)
          NEXT(VI) = HEAD(DVI)
          HEAD(DVI) = VI
          LAST(VI) = -DVI
          NVI=NEXT(VI)
          IF (NVI.GT.0)  LAST(NVI) = VI
   4      MARK(VI) = TAG
C
        RETURN
C
C ** ERROR-  INSUFFICIENT STORAGE
 101    FLAG = 2*N + VI+10
        RETURN
 102    FLAG = 10+VI
        RETURN
C INCORRECT COLUMN INDEX
 105    FLAG=10+N+VI
        RETURN
        END
