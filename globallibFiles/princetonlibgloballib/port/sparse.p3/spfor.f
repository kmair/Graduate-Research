      SUBROUTINE SPFOR(N, IROW, C)
      INTEGER N
      INTEGER C(N)
       EXTERNAL IROW
      DOUBLE PRECISION DD(500)
      COMMON /CSTAK/ DD
      INTEGER IV, LV, FLAG, ISTKGT, ISTKQU, MAX
      INTEGER ISP(1), NSP
      EQUIVALENCE (DD(1), ISP(1))
C MNEUMONIC -UNSYMMETRIC ORDER SCHEME
C THIS SUBROUTINE USES THE MINIMUM DEGREE ORDERING
C SCHEME TO REORDER A MATRIX
C INPUT PARAMETERS
C N      ORDER OF THE MATRIX
C IROW    FUNCTION DECLARED EXTERNAL IN MAIN PROGRAM WHICH
C          RETURNS TO SPFOR THE COLUMN INDICES IN JCOL OF THE
C          NUM NONZERO ELEMENTS IN THE ITH ROW. ONLY I
C          IS AN INPUT PARAMTER.
C         CALL IROW(I, JCOL, NUM)
C OUTPUT PARAMETERS
C C      INTEGER VECTOR OF LENGTH N OF REORDERED COLUMNS(AND
C        ROWS)
C ERROR STATES
C 1      N.LT.1     FATAL
C 2      STORAGE EXCEEDED IMMEDIATELY   FATAL
C N+K    KTH ROW OF MATRIX IS NULL
C 2N+K   STORAGE EXCEED AT KTH ROW
C MINIMUM STORAGE TAKEN FROM STACK-2N+M,WHERE
C M IS THE NUMBER OF NONZEROS IN SPARSE MATRIX
      CALL ENTER(1)
C/6S
C     IF (N .LT. 1) CALL SETERR(13H SPFOR-N.LT.1, 13, 1, 2)
C/7S
      IF (N .LT. 1) CALL SETERR(' SPFOR-N.LT.1', 13, 1, 2)
C/
      IC =ISTKGT(N,2)
      NSP = ISTKQU(2)
      IV = ISTKGT(NSP, 2)
      MAX=NSP/2-N
      LV=IV+MAX
      IHEAD=LV+MAX
      NEXT=IHEAD+N
      FLAG=0
C/6S
C     IF (MAX .LT. N) CALL SETERR(25H SPFOR-INSUFFICIENT SPACE, 25, 2, 2
C    1   )
C/7S
      IF (MAX .LT. N) CALL SETERR(' SPFOR-INSUFFICIENT SPACE', 25, 2, 2
     1   )
C/
      CALL S4MDM(N,IROW,MAX,ISP(IV),ISP(LV),ISP(IHEAD),C,ISP(IC),
     1 ISP(IV),FLAG,ISP,ISP,0)
      IF (FLAG .EQ. 0) GOTO 1
C/6S
C     IF (FLAG.GT.2*N+10)
C    2   CALL SETERR(27H SPFOR-INSUFFICIENT STORAGE,27,FLAG,1)
C     IF (FLAG.LE.N+10)
C    1   CALL SETERR(15H SPFOR-NULL ROW,15,FLAG,1)
C     IF (FLAG.GT.N+10.AND.FLAG.LE.10+2*N)
C    1   CALL SETERR(29H SPFOR-INCORRECT COLUMN INDEX,29,FLAG,1)
C/7S
      IF (FLAG.GT.2*N+10)
     2   CALL SETERR(' SPFOR-INSUFFICIENT STORAGE',27,FLAG,1)
      IF (FLAG.LE.N+10)
     1   CALL SETERR(' SPFOR-NULL ROW',15,FLAG,1)
      IF (FLAG.GT.N+10.AND.FLAG.LE.10+2*N)
     1   CALL SETERR(' SPFOR-INCORRECT COLUMN INDEX',29,FLAG,1)
C/
   1  CALL LEAVE
      RETURN
      END
