C$SEQ THYP 0 20
C$TEST THYP
C **********************************************************************
C
C  TEST OF PORT LINGUISTIC HYPOTHESES
C
C **********************************************************************
C
      COMMON /OK/ CREM, DREM, RREM, IREM, LREM, CIDX, DIDX, RIDX, IIDX
     1   , LIDX
      LOGICAL CREM, DREM, RREM, IREM, LREM, CIDX
      LOGICAL DIDX, RIDX, IIDX, LIDX
      COMMON /LNAME/ L
      LOGICAL L(1000)
      COMMON /INAME/ I
      INTEGER I(1000)
      COMMON /RNAME/ R
      REAL R(1000)
      COMMON /DNAME/ D
      DOUBLE PRECISION D(1000)
      COMMON /CNAME/ C
C/R
C     REAL C(2,1000)
C/C
      COMPLEX C(1000)
C/
      INTEGER P, I1MACH
      REAL FLOAT
      LOGICAL LOGVAR
C/R
C/C
      COMPLEX CMPLX
C/
      INTEGER TEMP
C TO TEST THE PORT LINGUISTIC HYPOTHESES.
C .REM MEANS REMEMBERED AND .IDX MEANS INDEXING OK IN THE FOLLOWING.
      CREM = .TRUE.
      DREM = .TRUE.
      RREM = .TRUE.
      IREM = .TRUE.
      LREM = .TRUE.
      CIDX = .TRUE.
      DIDX = .TRUE.
      RIDX = .TRUE.
      IIDX = .TRUE.
      LIDX = .TRUE.
      LOGVAR = .TRUE.
C MAKE SOME DATA.
      DO  1 P = 1, 1000
C/R
C        C(1,P) = -FLOAT(P)
C        C(2,P) = FLOAT(P)
C/C
         C(P) = CMPLX(-FLOAT(P), FLOAT(P))
C/
         D(P) = P
         R(P) = -P
         I(P) = P
         L(P) = LOGVAR
         LOGVAR = .NOT. LOGVAR
   1     CONTINUE
C CHECK IT IN ARGUMENT AND COMMON LISTS.
      DO  2 P = 1, 999
         CALL CHECK(C, D, R, I, L, P)
   2     CONTINUE
      IF (CREM) GOTO 4
         TEMP = I1MACH(2)
         WRITE (TEMP,  3)
   3     FORMAT (49H COMPLEX VALUES NOT REMEMBERED BY DATA STATEMENT.)
   4  IF (DREM) GOTO 6
         TEMP = I1MACH(2)
         WRITE (TEMP,  5)
   5     FORMAT (
     1     58H DOUBLE PRECISION VALUES NOT REMEMBERED BY DATA STATEMENT.
     2      )
   6  IF (RREM) GOTO 8
         TEMP = I1MACH(2)
         WRITE (TEMP,  7)
   7     FORMAT (46H REAL VALUES NOT REMEMBERED BY DATA STATEMENT.)
   8  IF (IREM) GOTO 10
         TEMP = I1MACH(2)
         WRITE (TEMP,  9)
   9     FORMAT (49H INTEGER VALUES NOT REMEMBERED BY DATA STATEMENT.)
  10  IF (LREM) GOTO 12
         TEMP = I1MACH(2)
         WRITE (TEMP,  11)
  11     FORMAT (49H LOGICAL VALUES NOT REMEMBERED BY DATA STATEMENT.)
  12  IF (CIDX) GOTO 14
         TEMP = I1MACH(2)
         WRITE (TEMP,  13)
  13     FORMAT (45H COMPLEX INDICES LARGER THAN ONE NOT ALLOWED.)
  14  IF (DIDX) GOTO 16
         TEMP = I1MACH(2)
         WRITE (TEMP,  15)
  15     FORMAT (
     1      54H DOUBLE PRECISION INDICES LARGER THAN ONE NOT ALLOWED.)
  16  IF (RIDX) GOTO 18
         TEMP = I1MACH(2)
         WRITE (TEMP,  17)
  17     FORMAT (42H REAL INDICES LARGER THAN ONE NOT ALLOWED.)
  18  IF (IIDX) GOTO 20
         TEMP = I1MACH(2)
         WRITE (TEMP,  19)
  19     FORMAT (45H INTEGER INDICES LARGER THAN ONE NOT ALLOWED.)
  20  IF (LIDX) GOTO 22
         TEMP = I1MACH(2)
         WRITE (TEMP,  21)
  21     FORMAT (45H LOGICAL INDICES LARGER THAN ONE NOT ALLOWED.)
  22  TEMP = I1MACH(2)
      WRITE (TEMP,  23)
  23  FORMAT (45H TEST OF PORT LINGUISTIC HYPOTHESES COMPLETE.)
      STOP
      END
      SUBROUTINE CHECK(C, D, R, I, L, P)
      INTEGER I(1), P
      REAL R(1)
      LOGICAL L(1)
C/R
C     REAL C(2,1)
C/C
      COMPLEX C(1)
C/
      DOUBLE PRECISION D(1)
      COMMON /OK/ CREM, DREM, RREM, IREM, LREM, CIDX, DIDX, RIDX, IIDX
     1   , LIDX
      LOGICAL CREM, DREM, RREM, IREM, LREM, CIDX
      LOGICAL DIDX, RIDX, IIDX, LIDX
      COMMON /LNAME/ CL
      LOGICAL CL(1000)
      COMMON /INAME/ CI
      INTEGER CI(1000)
      COMMON /RNAME/ CR
      REAL CR(1000)
      COMMON /DNAME/ CD
      DOUBLE PRECISION CD(1000)
      COMMON /CNAME/ CC
C/R
C     REAL CC(2,1000), LC(2)
C/C
      COMPLEX CC(1000), LC
C/
      INTEGER LI, Q, I1MACH
      REAL LR, AIMAG, REAL
      LOGICAL LL
      DOUBLE PRECISION LD
      INTEGER TEMP
C/R
C     DATA LC(1),LC(2)/-1., 1./
C/C
      DATA LC/(-1.,  1.)/
C/
      DATA LD/1.0D0/
      DATA LR/-1.0E0/
      DATA LI/1/
      DATA LL/.TRUE./
C LOCAL VARIABLES.
C START LOCAL VARIABLES AS COMMON
C VARIABLES BEGIN.
      IF (P .NE. 1) GOTO 11
C/R
C        IF (LC(1) .EQ. C(1,P) .AND. LC(2) .EQ. C(2,P)
C    1      ) GOTO 2
C/C
         IF (REAL(LC) .EQ. REAL(C(P)) .AND. AIMAG(LC) .EQ. AIMAG(C(P))
     1      ) GOTO 2
C/
            TEMP = I1MACH(2)
C CHECK DATA VALUES OF LOCAL VARIABLES.
            WRITE (TEMP,  1)
   1        FORMAT (34H DATA STATEMENT FAILS FOR COMPLEX.)
   2     IF (LD .EQ. D(P)) GOTO 4
            TEMP = I1MACH(2)
            WRITE (TEMP,  3)
   3        FORMAT (43H DATA STATEMENT FAILS FOR DOUBLE PRECISION.)
   4     IF (LR .EQ. R(P)) GOTO 6
            TEMP = I1MACH(2)
            WRITE (TEMP,  5)
   5        FORMAT (31H DATA STATEMENT FAILS FOR REAL.)
   6     IF (LI .EQ. I(P)) GOTO 8
            TEMP = I1MACH(2)
            WRITE (TEMP,  7)
   7        FORMAT (34H DATA STATEMENT FAILS FOR INTEGER.)
   8     IF ((LL .OR. (.NOT. L(P))) .AND. ((.NOT. LL) .OR. L(P))) GOTO
     1      10
            TEMP = I1MACH(2)
            WRITE (TEMP,  9)
   9        FORMAT (34H DATA STATEMENT FAILS FOR LOGICAL.)
  10     CONTINUE
         GOTO  12
C/R
C 11     IF (LC(1) .NE. C(1,P) .OR. LC(2) .NE. C(2,P))
C    1       CREM = .FALSE.
C/C
  11     IF (REAL(LC) .NE. REAL(C(P)) .OR. AIMAG(LC) .NE. AIMAG(C(P)))
     1       CREM = .FALSE.
C/
C CHECK THE REMEMBERED LOCAL VALUES.
         IF (LD .NE. D(P)) DREM = .FALSE.
         IF (LR .NE. R(P)) RREM = .FALSE.
         IF (LI .NE. I(P)) IREM = .FALSE.
         IF ((.NOT. LL) .AND. L(P) .OR. LL .AND. (.NOT. L(P))) LREM =
     1      .FALSE.
C MAKE A LOCAL COPY OF THE NEXT VALUES TO BE SEEN BY CHECK.
C/R
C 12  LC(1) = C(1,P+1)
C     LC(2) = C(2,P+1)
C/C
  12  LC = C(P+1)
C/
      LD = D(P+1)
      LR = R(P+1)
      LI = I(P+1)
      LL = L(P+1)
      IF (P .NE. 1) GOTO 13
C/R
C        IF (LC(1) .EQ. (-1.) .AND. LC(2) .EQ. 1.) CIDX =
C    1      .FALSE.
C/C
         IF (REAL(LC) .EQ. (-1.) .AND. AIMAG(LC) .EQ. 1.) CIDX =
     1      .FALSE.
C/
C CHECK THAT LOCAL VARIABLES WERE REALLY UPDATED.
         IF (LD .EQ. 1D0) DIDX = .FALSE.
         IF (LR .EQ. (-1.)) RIDX = .FALSE.
         IF (LI .EQ. 1) IIDX = .FALSE.
         IF (LL) LIDX = .FALSE.
C CHECK THAT THE LOCAL VARIABLES WERE UPDATED.
C/R
C 13  IF (LC(1) .EQ. C(1,P) .AND. LC(2) .EQ. C(2,P))
C    1   CIDX = .FALSE.
C/C
  13  IF (REAL(LC) .EQ. REAL(C(P)) .AND. AIMAG(LC) .EQ. AIMAG(C(P)))
     1   CIDX = .FALSE.
C/
      IF (LD .EQ. D(P)) DIDX = .FALSE.
      IF (LR .EQ. R(P)) RIDX = .FALSE.
      IF (LI .EQ. I(P)) IIDX = .FALSE.
      IF (LL .AND. L(P) .OR. (.NOT. LL) .AND. (.NOT. L(P))) LIDX =
     1   .FALSE.
      DO  14 Q = P, 999
C/R
C        IF (C(1,Q+1) .NE. CC(1,Q+1) .OR. C(2,Q+1) .NE.
C    1      CC(2,Q+1)) CIDX = .FALSE.
C/C
         IF (REAL(C(Q+1)) .NE. REAL(CC(Q+1)) .OR. AIMAG(C(Q+1)) .NE.
     1      AIMAG(CC(Q+1))) CIDX = .FALSE.
C/
         IF (D(Q+1) .NE. CD(Q+1)) DIDX = .FALSE.
         IF (R(Q+1) .NE. CR(Q+1)) RIDX = .FALSE.
         IF (I(Q+1) .NE. CI(Q+1)) IIDX = .FALSE.
         IF ((.NOT. L(Q+1)) .AND. CL(Q+1) .OR. L(Q+1) .AND. (.NOT. CL(Q+
     1      1))) LIDX = .FALSE.
  14     CONTINUE
      RETURN
      END
