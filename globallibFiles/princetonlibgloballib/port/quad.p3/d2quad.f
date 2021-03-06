      SUBROUTINE D2QUAD(F,N,X,EPS,HS,NFCALL,LYSTAK,KMAXEX,KDIVID,
     X          JPRINT,ANS,ERREST,KWARN)
C
C INTEGRATE F(X) FROM X(1) TO X(N) WITH ABSOLUTE ACCURACY EPS.
C MUST TAKE STEP SIZE AT LEAST AS SMALL AS HS*DABS(B-A).
C USE NO MORE THAN NFCALL SAMPLING POINTS, OR CALLS TO F.
C FOR TEMPORARY STORAGE OF FUNCTION VALUES, ALLOCATE LYSTAK WORDS.
C DO AT MOST KMAXEX EXTRAPOLATIONS.
C DO NOT DIVIDE INTERVAL UNTIL AFTER KDIVID EXTRAPOLATIONS.
C JPRINT = 0 FOR NO PRINTING, 1 FOR SOME, 2 FOR LOTS.
C TO START, DIVIDE (X(1),X(N)) INTO NUM INTERVALS.  INTERNAL
C BREAKPOINTS ARE GIVEN IN ARRAY X.  NUM+1 = NUMBER OF DISTINCT X S.
C RETURN VALUE AND ITS ESTIMATED ERROR IN ANS AND ERREST.
C RETURN KWARN AS WARNING FLAG.
C
C FUNCTION ISTKQU RETURNS AMOUNT OF SPACE LEFT IN STACK.
C FUNCTION ISTKGT ALLOCATES SPACE IN STACK AND GIVES POINTER TO IT.
C SUBROUTINE LEAVE DE-ALLOCATES SPACE IN STACK.
C THE DEFAULT STACK LENGTH IS 500 DOUBLE PRECISION WORDS.
C IF A LARGER STACK IS NECESSARY, THE PROGRAM CALLING D2QUAD
C SHOULD INITIALIZE THE STACK  USING SUBROUTINE ISTKIN.
C
      DOUBLE PRECISION F,EPS,HS,ANS,ERREST,X(1)
      EXTERNAL F
      COMMON /D1QCM1/ DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL,
     X   NPRINT,NUM,MAXY,MAXINT,MAXF,KMIN,KMAX,KDIV,NMAX
      DOUBLE PRECISION DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL
      DOUBLE PRECISION DLOG
C
      COMMON/CSTAK/DS
      DOUBLE PRECISION DS(500),D1MACH
      INTEGER IS(1000)
      EQUIVALENCE (DS(1),IS(1))
C
C        MACHINE- AND PRECISION-DEPENDENT NUMBERS
C
      HSMALL = 50.D0*D1MACH(4)
      DROUND = 50.D0*D1MACH(4)
      DSMALL = 10.D0*D1MACH(1)
      DLARGE = 0.1D0*D1MACH(2)
C
C         PARAMETERS
C
      NPRINT = JPRINT
      NUM=N-1
      DO 90 J=2,N
   90    IF (X(J-1).EQ.X(J)) NUM=NUM-1
      NUM=MAX0(NUM,1)
      KMIN = 2
      KMAX = MAX0(4,MIN0(12,KMAXEX))
      KMAX = 2*(KMAX/2)
      KDIV = MIN0(MAX0(KDIVID,4),KMAX)
      KDIV = 2*(KDIV/2)
      HSAMPL = DMIN1(HS,1.D0)
      ESMALL = EPS*1.D-3
      NMAX = 1.-DLOG(DROUND)/DLOG(6.D0)
      NMAX = MIN0(25,NMAX)
C
C        ALLOCATE SCRATCH SPACE
C
      MAXY = MAX0(NUM+7,LYSTAK)
      MAXF = MAX0(4*NUM+1,NFCALL)
      MAXINT=1.-DLOG(HSMALL)/DLOG(2.D0)
      MAXINT = MAX0(MAXINT,NUM+2)
      CALL ENTER(0)
      NSHORT = ISTKQU(2)-2*MAXINT
      IF (NSHORT .LT. 0) GO TO 120
      IIDIV = ISTKGT(2*MAXINT,2)
      II = IIDIV+MAXINT
      NSHORT = ISTKQU(4)-(MAXINT+1+MAXY)
      IF (NSHORT .LT. 0) GO TO 120
      IX = ISTKGT(MAXINT+1+MAXY,4)
      IY = IX+MAXINT+1
C
      JJ=IX
      DS(JJ)=X(1)
      DO 110 J=2,N
         IF (X(J).EQ.X(J-1)) GO TO 110
            JJ=JJ+1
            DS(JJ)=X(J)
  110 CONTINUE
      JJ=IX+NUM
      DS(JJ)=X(N)
C
      CALL D1QDAP(F,X(1),X(N),EPS,DS(IY),DS(IX),IS(II),IS(IIDIV),
     X    ANS,ERREST,KWARN)
C
      CALL LEAVE
      RETURN
C
  120 KWARN = NSHORT
      ANS = 0.
      ERREST = DLARGE
      CALL LEAVE
      RETURN
      END
