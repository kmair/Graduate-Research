      INTEGER FUNCTION IMMMD(IMESH, NMESH, P, MP)
      INTEGER IMESH, NMESH, MP
      DOUBLE PRECISION P
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      INTEGER IROLD, J, ISTKMD, ISTKST, IS(1000), CLEGTH
      INTEGER I3MMD
      REAL RS(1000)
      LOGICAL LS(1000)
      DOUBLE PRECISION WS(500)
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
C TO MAKE P A MESH POINT OF MULTIPLICITY MP IN THE MESH INDICATED
C BY THE STACK-POINTER IMESH.
C THE MESH MUST BE AT THE TOP OF THE STACK, THAT IS,
C IT MUST BE THE MOST RECENT ALLOCATION.
C MNEMONIC - MESH MULTIPLICITY MODIFIER.
C INPUT -
C   IMESH - THE MESH IS STACK(IMESH), ..., STACK(IMESH+NMESH-1).
C   NMESH - THE LENGTH OF THE MESH.
C   P     - THE POINT TO BE MODIFIED.
C   MP    - THE MULTIPLICITY P IS TO HAVE.
C OUTPUT -
C   NMESH - THE LENGTH OF THE MODIFIED MESH ON THE STACK.
C ERROR STATES -
C   1 - NOTHING ON THE STACK
C   2 - IMESH < 1
C   3 - NMESH < 0
C   4 - MP < 0
C   5 - TOP OF STACK NOT OF LENGTH NMESH
C   6 - THE MESH IS NOT AT THE TOP OF THE STACK
C   7 - MESH NOT MONOTONE INCREASING
C CHECK THE INPUT FOR ERRORS.
C/6S
C     IF (ISTKST(1) .LT. 1) CALL SETERR(28HIMMMD - NOTHING ON THE STACK,
C    1   28, 1, 2)
C     IF (IMESH .LT. 1) CALL SETERR(20HIMMMD - IMESH .LT. 1, 20, 2, 2)
C     IF (NMESH .LT. 0) CALL SETERR(20HIMMMD - NMESH .LT. 0, 20, 3, 2)
C     IF (MP .LT. 0) CALL SETERR(17HIMMMD - MP .LT. 0, 17, 4, 2)
C/7S
      IF (ISTKST(1) .LT. 1) CALL SETERR('IMMMD - NOTHING ON THE STACK',
     1   28, 1, 2)
      IF (IMESH .LT. 1) CALL SETERR('IMMMD - IMESH .LT. 1', 20, 2, 2)
      IF (NMESH .LT. 0) CALL SETERR('IMMMD - NMESH .LT. 0', 20, 3, 2)
      IF (MP .LT. 0) CALL SETERR('IMMMD - MP .LT. 0', 17, 4, 2)
C/
C THE CURRENT LENGTH OF THE STACK.
      CLEGTH = ISTKST(2)
      J = ISTKMD(NMESH)
C/6S
C     IF (ISTKST(2) .NE. CLEGTH) CALL SETERR(
C    1   40HIMMMD - TOP OF STACK NOT OF LENGTH NMESH, 40, 5, 2)
C     IF (J .NE. IMESH) CALL SETERR(
C    1   47HIMMMD - THE MESH IS NOT AT THE TOP OF THE STACK, 47, 6, 2)
C/7S
      IF (ISTKST(2) .NE. CLEGTH) CALL SETERR(
     1   'IMMMD - TOP OF STACK NOT OF LENGTH NMESH', 40, 5, 2)
      IF (J .NE. IMESH) CALL SETERR(
     1   'IMMMD - THE MESH IS NOT AT THE TOP OF THE STACK', 47, 6, 2)
C/
      CALL ENTSRC(IROLD, 1)
      NMESH = I3MMD(IMESH, WS(IMESH), NMESH, P, MP)
      CALL RETSRC(IROLD)
      IMMMD = IMESH
      RETURN
      END
