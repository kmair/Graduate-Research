      SUBROUTINE GLSBP(NU, ORDER, BC, E)
      INTEGER NU
      INTEGER ORDER(NU, NU, 2), BC(NU, 2, 2), E(NU, 2, 2)
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      INTEGER ICE, ISTKGT, MAX0, I, J, L
      INTEGER IPPS, INOW, IMAORD, INOOLD, IS(1000)
      REAL RS(1000), WS(500)
      LOGICAL ALLERO, LS(1000)
      INTEGER TEMP1, TEMP2
      LOGICAL TEMP, TEMP3
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
C TO DETERMINE WHICH ODE SHOULD USE WHICH BOUNDARY CONDITION.
C MNEMONIC - GALERKIN'S METHOD FOR LINEAR SYSTEMS,
C            BOUNDARY CONDITION PLACEMENT.
C SCRATCH SPACE ALLOCATED -
C       S( GLSBP) <= NU*(4*NU+15)
C INTEGER WORDS.
C CHECK THE INPUT FOR ERRORS.
C/6S
C     IF (NU .LT. 1) CALL SETERR(16H GLSBP - NU.LT.1, 16, 1, 2)
C/7S
      IF (NU .LT. 1) CALL SETERR(' GLSBP - NU.LT.1', 16, 1, 2)
C/
      DO  4 L = 1, 2
         DO  3 I = 1, NU
C IS ORDER(I,.,L) = (-1, ... , -1)?
            ALLERO = .TRUE.
            DO  1 J = 1, NU
               ALLERO = ALLERO .AND. ORDER(I, J, L) .EQ. (-1)
               TEMP = ORDER(I, J, L) .LT. (-1)
               IF (.NOT. TEMP) TEMP = ORDER(I, J, L) .GT. 2
C/6S
C              IF (TEMP) CALL SETERR(
C    1            41H GLSBP - ORDER(I,J,L) NOT ONE OF -1,0,1,2, 41, 2, 2
C    2            )
C/7S
               IF (TEMP) CALL SETERR(
     1            ' GLSBP - ORDER(I,J,L) NOT ONE OF -1,0,1,2', 41, 2, 2
     2            )
C/
   1           CONTINUE
            TEMP = BC(I, 1, L) .NE. (-2)
            IF (TEMP) TEMP = BC(I, 1, L) .NE. 0
            IF (TEMP) GOTO 2
               TEMP3 = BC(I, 2, L) .NE. (-2)
               IF (TEMP3) TEMP3 = BC(I, 2, L) .NE. 1
               TEMP = TEMP3
C/6S
C  2        IF (TEMP) CALL SETERR(
C    1         36H GLSBP - BC(I,.,L) NOT ONE OF -2,0,1, 36, 3, 2)
C           IF (ALLERO) CALL SETERR(
C    1         33H GLSBP - ORDER(I,.,L)=(-1,...,-1), 33, 4, 2)
C/7S
   2        IF (TEMP) CALL SETERR(
     1         ' GLSBP - BC(I,.,L) NOT ONE OF -2,0,1', 36, 3, 2)
            IF (ALLERO) CALL SETERR(
     1         ' GLSBP - ORDER(I,.,L)=(-1,...,-1)', 33, 4, 2)
C/
   3        CONTINUE
   4     CONTINUE
      CALL ENTER(1)
C COMPLEMENT OF E.
      ICE = ISTKGT(NU, 2)
C MAXORD(I,L) = MAX OVER J = 1, ... , NU ORDER(I,J,L).
      IMAORD = ISTKGT(2*NU, 2)
      CALL SETI(2*NU, -1, IS(IMAORD))
      DO  7 L = 1, 2
         DO  6 I = 1, NU
            DO  5 J = 1, NU
               TEMP2 = IMAORD+I-1+(L-1)*NU
               TEMP1 = IMAORD+I-1+(L-1)*NU
               IS(TEMP2) = MAX0(IS(TEMP1), ORDER(I, J, L))
   5           CONTINUE
   6        CONTINUE
   7     CONTINUE
      I = 0
      IPPS = 1
      INOW = 0
   8  TEMP = I .GE. 4*NU
      IF (TEMP) TEMP = IPPS .EQ. 1
      IF (TEMP) GOTO  16
         GOTO  13
C MAKE A NODE.
   9        INOOLD = INOW
            I = I+1
            INOW = ISTKGT(NU+3, 2)
            IS(INOW) = INOOLD
C       GET THE CANDIDATES FOR E(I).
            CALL A6LSBP(I, NU, ORDER, BC, E, IS(IMAORD), IS(ICE), IS(
     1         INOW+3), IS(INOW+1))
            IS(INOW+2) = 0
            IPPS = 0
            GOTO  14
C SEARCHING A NODE.
  10        IS(INOW+2) = IS(INOW+2)+1
            IF (IS(INOW+2) .LE. IS(INOW+1)) GOTO 11
               IPPS = -1
C BACK-UP.
               GOTO  8
  11        TEMP1 = INOW+2+IS(INOW+2)-1
            E(I, 1, 1) = IS(TEMP1+1)
            IPPS = 1
            GOTO  14
C BACKING UP A NODE.
  12        INOW = IS(INOW)
            CALL ISTKRL(1)
            I = I-1
            IPPS = 0
            GOTO  14
  13        TEMP1 = IPPS+2
            IF (TEMP1 .GT. 0 .AND. TEMP1 .LE. 3) GOTO ( 12,  10,  9),
     1         TEMP1
C END SWITCH.
  14     IF (I .NE. 0) GOTO 15
C/6S
C           CALL SETERR(37H GLSBP - IMPROPER BOUNDARY CONDITIONS, 37, 5,
C    1         1)
C/7S
            CALL SETERR(' GLSBP - IMPROPER BOUNDARY CONDITIONS', 37, 5,
     1         1)
C/
            GOTO  16
  15     CONTINUE
         GOTO  8
C END WHILE.
  16  CALL LEAVE
      RETURN
      END
