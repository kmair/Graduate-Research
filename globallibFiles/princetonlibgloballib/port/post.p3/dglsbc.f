      LOGICAL FUNCTION DGLSBC(NU, ORDER, BC, E)
      INTEGER NU
      INTEGER ORDER(NU, NU, 2), BC(NU, 2, 2), E(NU, 2, 2)
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      INTEGER ICOUNT, ISTKGT, I, J, L, IMAORD
      INTEGER IS(1000)
      REAL RS(1000)
      LOGICAL FAILED, ALLERO, LS(1000), D6LSBC
      DOUBLE PRECISION WS(500)
      LOGICAL TEMP, TEMP1
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
C TO CHECK THAT THE BOUNDARY CONDITION PLACEMENT GIVEN BY E
C IS CORRECT.
C MNEMONIC - DOUBLE PRECISION GALERKIN'S METHOD FOR LINEAR SYSTEMS,
C            BOUNDARY PLACEMENT CHECK.
C SCRATCH SPACE ALLOCATED - 4*NU INTEGER WORDS.
C CHECK THE INPUT.
C/6S
C     IF (NU .LT. 1) CALL SETERR(16HDGLSBC - NU.LT.1, 16, 1, 2)
C/7S
      IF (NU .LT. 1) CALL SETERR('DGLSBC - NU.LT.1', 16, 1, 2)
C/
      DO  6 L = 1, 2
         DO  5 I = 1, NU
            TEMP = E(I, 1, L) .LT. 1
            IF (.NOT. TEMP) TEMP = E(I, 1, L) .GT. NU
            IF (TEMP) TEMP = BC(I, 1, L) .EQ. 0
            IF (TEMP) GOTO 1
               TEMP1 = E(I, 2, L) .LT. 1
               IF (.NOT. TEMP1) TEMP1 = E(I, 2, L) .GT. NU
               IF (TEMP1) TEMP1 = BC(I, 2, L) .EQ. 1
               TEMP = TEMP1
   1        IF (.NOT. TEMP) GOTO 2
               DGLSBC = .FALSE.
               RETURN
C IS ORDER(I,.,L) = (-1, ... , -1)?
   2        ALLERO = .TRUE.
            DO  3 J = 1, NU
               ALLERO = ALLERO .AND. ORDER(I, J, L) .EQ. (-1)
               TEMP = ORDER(I, J, L) .LT. (-1)
               IF (.NOT. TEMP) TEMP = ORDER(I, J, L) .GT. 2
C/6S
C              IF (TEMP) CALL SETERR(
C    1            41HDGLSBC - ORDER(I,J,L) NOT ONE OF -1,0,1,2, 41, 3, 2
C    2            )
C/7S
               IF (TEMP) CALL SETERR(
     1            'DGLSBC - ORDER(I,J,L) NOT ONE OF -1,0,1,2', 41, 3, 2
     2            )
C/
   3           CONTINUE
            TEMP = BC(I, 1, L) .NE. (-2)
            IF (TEMP) TEMP = BC(I, 1, L) .NE. 0
            IF (TEMP) GOTO 4
               TEMP1 = BC(I, 2, L) .NE. (-2)
               IF (TEMP1) TEMP1 = BC(I, 2, L) .NE. 1
               TEMP = TEMP1
C/6S
C  4        IF (TEMP) CALL SETERR(
C    1         36HDGLSBC - BC(I,.,L) NOT ONE OF -2,0,1, 36, 4, 2)
C           IF (ALLERO) CALL SETERR(
C    1         33HDGLSBC - ORDER(I,.,L)=(-1,...,-1), 33, 5, 2)
C/7S
   4        IF (TEMP) CALL SETERR(
     1         'DGLSBC - BC(I,.,L) NOT ONE OF -2,0,1', 36, 4, 2)
            IF (ALLERO) CALL SETERR(
     1         'DGLSBC - ORDER(I,.,L)=(-1,...,-1)', 33, 5, 2)
C/
   5        CONTINUE
   6     CONTINUE
      CALL ENTER(1)
      IMAORD = ISTKGT(4*NU, 2)
      ICOUNT = IMAORD+2*NU
      FAILED = D6LSBC(NU, ORDER, BC, E, IS(IMAORD), IS(ICOUNT))
      CALL LEAVE
      DGLSBC = FAILED
      RETURN
      END
