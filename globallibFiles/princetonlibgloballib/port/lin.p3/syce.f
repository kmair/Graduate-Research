      SUBROUTINE  SYCE(N, C, INTER, COND)
      INTEGER N
      INTEGER INTER(N)
      REAL C(1), COND
      COMMON /CSTAK/ D
      REAL D(1000)
      INTEGER IZ, IERR, ISTKGT, NERROR
      REAL ANORM, R1MACH, SYNM
C SYMMETRIC CONDITION ESTIMATION
C INPUT PARMETERS
C N          NUMBER OF ROWS IN A
C C       VECTOR INTO WHICH THE SYMMETRIC MATRIX A IS
C         PACKED BY COLUMNS ACCORDING TO THE FOLLOWING
C         TEMPLATE
C            1
C            2  5
C            3  6  8
C            4  7  9  10
C OUTPUT PARAMETERS
C C          MDM(TRANSPOSE) DECOMPOSITION SUITABLE AS INPUT
C            INTO SYFBS
C INTER      INTEGER ARRAY OF LENGTH N OF INTERCHANGES PERFORMED
C            DURING DECOMPOSITION
C            SUITABLE OF INPUT INTO SYFBS
C COND       LOWER BOUND ON THE CONDITION NUMBER OF A
C STORAGE ALLOCATED- N REAL LOCATIONS
C ERROR CONDITIONS
C 1          N.LT.1
C/6S
C     IF (N .LT. 1) CALL SETERR(12H SYCE-N.LT.1, 12, 1, 2)
C/7S
      IF (N .LT. 1) CALL SETERR(' SYCE-N.LT.1', 12, 1, 2)
C/
      CALL ENTER(1)
      ANORM=SYNM(N,C)
      CALL  SYMD(N, C, INTER,0.0)
      IF (NERROR(IERR) .EQ. 0) GOTO 1
         COND = R1MACH(2)
         CALL LEAVE
         RETURN
   1  IZ = ISTKGT(N, 3)
      CALL S4YCE(N, C, COND, INTER, ANORM, D(IZ))
      CALL LEAVE
      RETURN
      END