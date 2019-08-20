      SUBROUTINE DLTSQ(M,N,A,NA,B,IRHS,W)
      INTEGER NA,M,N,IRHS,MFLAG
      INTEGER ISTKGT,IERR
      INTEGER IRV1, ICD, IV
      DOUBLE PRECISION A(NA,N),B(NA,IRHS),W(N)
      LOGICAL MATU, MATV
      DOUBLE PRECISION DSTAK(500)
      COMMON /CSTAK/DSTAK
C
      CALL ENTER(0)
C/6S
C     IF (IRHS .LT. 0) CALL SETERR(16HDLTSQ  IRHS.LT.0,16,1,2)
C     IF (M .LT. N) CALL SETERR(13HDLTSQ  M.LT.N,13,2,2)
C     IF (NA .LT. M) CALL SETERR (14HDLTSQ  NA.LT.M,14,3,2)
C     IF (M .LT. 1) CALL SETERR (13HDLTSQ  M.LT.1,13,4,2)
C     IF (N .LT.1) CALL SETERR (13HDLTSQ  N.LT.1,13,5,2)
C/7S
      IF (IRHS .LT. 0) CALL SETERR('DLTSQ  IRHS.LT.0',16,1,2)
      IF (M .LT. N) CALL SETERR('DLTSQ  M.LT.N',13,2,2)
      IF (NA .LT. M) CALL SETERR ('DLTSQ  NA.LT.M',14,3,2)
      IF (M .LT. 1) CALL SETERR ('DLTSQ  M.LT.1',13,4,2)
      IF (N .LT.1) CALL SETERR ('DLTSQ  N.LT.1',13,5,2)
C/
      IRV1 = ISTKGT(N,4)
      ICD = ISTKGT(N,4)
      IV = ISTKGT(N**2,4)
      MATV = .TRUE.
      MATU = .FALSE.
      CALL DC1RSS(M,N,MATU,MATV,MFLAG)
      CALL DG1SVD(NA,N,M,N,A,W,MATU,A,MATV,DSTAK(IV),
     1   A,B,IRHS,DSTAK(IRV1),DSTAK(ICD),MFLAG)
      IERR = NERROR(NERR)
      IF (IERR.LE.10) GOTO 200
         CALL ERROFF
C/6S
C        CALL SETERR(35HDLTSQ  NO CONVERGENCE AFTER 30 ITER,
C    1      35,IERR,1)
C/7S
         CALL SETERR('DLTSQ  NO CONVERGENCE AFTER 30 ITER',
     1      35,IERR,1)
C/
         GO TO 210
C
 200  CALL DC2LSQ(DSTAK(ICD),W,B,DSTAK(IV),N,M,IRHS,NA)
 210  CALL LEAVE
      RETURN
      END