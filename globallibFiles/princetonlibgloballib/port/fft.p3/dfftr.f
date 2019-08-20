      SUBROUTINE DFFTR(NNP2,A,B)
C
C DFFTC FINDS THE FOURIER TRANSFORM OF THE 2N REAL DATA
C POINTS STORED IN THE REAL ARRAY, A, OF DIMENSION N PLUS 2,
C BY SIMPLY SETTING UP THE CORRECT CALLS TO DFFT AND DRLTR.
C
C STORAGE IN THE DYNAMIC STORAGE STACK IS REQUIRED FOR
C 220 INTEGER VARIABLES AND 92 DOUBLE PRECISION VARIABLES.
C
C     INPUT
C
C       NNP2  -FOR 2N INPUT POINTS, NNP2 MUST BE INPUT AS 2*N+2
C              NOTE THAT THE NUMBER OF INPUT POINTS MUST BE EVEN.
C       A     -A VECTOR OF LENGTH NNP2 CONTAINING THE 2N REAL
C              DATA IN THE FIRST 2N LOCATIONS
C
C     OUTPUT
C
C       A     -THE FIRST N+1 LOCATIONS OF A CONTAIN THE REAL (COSINE)
C              COMPONENTS OF THE FOURIER COEFFICIENTS
C       B     -A VECTOR OF LENGTH N+1 CONTAINING THE N+1 IMAGINARY
C              (SINE) COMPONENTS OF THE FOURIER COEFFICIENTS
C              ( WITH B(N+1) = 0.)
C
C
C NUMBER OF FACTORS OF N MUST NOT EXCEED 11.
C MAXIMUM PRIME FACTOR OF N MUST NOT EXCEED 23.
C PRODUCT OF THE SQUARE-FREE FACTORS, IF THERE EXISTS MORE THAN ONE,
C MUST NOT EXCEED 210.
C
C
C     ERROR STATES
C          1.  NNP2 (2N+2) IS LESS THAN 6
C          2.  NNP2 IS NOT EVEN
C          3.  PRIME FACTOR .GT. 23
C              (RECOVERABLE)
C          4.  SQUARE-FREE FACTOR PRODUCT .GT. 210
C              (RECOVERABLE)
C
C
C COMMON AREA
      COMMON/CSTAK/DSTAK(500)
C
      DOUBLE PRECISION A(2),B(2)
      DOUBLE PRECISION DSTAK
C
C  TEST THE VALIDITY OF THE INPUTS
C
C/6S
C     IF(NNP2 .LT. 6) CALL SETERR(
C    1   27HDFFTR - NNP2 IS LESS THAN 6,27,1,2)
C/7S
      IF(NNP2 .LT. 6) CALL SETERR(
     1   'DFFTR - NNP2 IS LESS THAN 6',27,1,2)
C/
      J = NNP2
C/6S
C     IF (J/2*2 .NE. J) CALL SETERR(
C    1   34HDFFTR - NNP2 IS NOT AN EVEN NUMBER,34,2,2)
C/7S
      IF (J/2*2 .NE. J) CALL SETERR(
     1   'DFFTR - NNP2 IS NOT AN EVEN NUMBER',34,2,2)
C/
C
C ENTER THE RECOVERY MODE (STORING THE PREVIOUS)
C
      CALL ENTER(1)
C
C  CALL THE SUBPROGRAMS, DFFT AND DRLTR
C
      J = NNP2/2 - 1
      CALL DFFT(A,A(2),J,J,J,2)
      CALL DRLTR(A,A(2),J,2)
C
C
C  CHECK FOR ERRORS FROM DFFT
C
      IF (NERROR(NERR) .EQ. 0) GO TO 10
C
      CALL ERROFF
C/6S
C     IF (NERR .EQ. 5) CALL SETERR(
C    1   36HDFFTR - PRIME FACTOR GREATER THAN 23,36,3,1)
C/7S
      IF (NERR .EQ. 5) CALL SETERR(
     1   'DFFTR - PRIME FACTOR GREATER THAN 23',36,3,1)
C/
C
C/6S
C     IF (NERR .EQ. 6) CALL SETERR(
C    1   44HDFFTR - SQUARE-FREE PRODUCT GREATER THAN 210,44,4,1)
C/7S
      IF (NERR .EQ. 6) CALL SETERR(
     1   'DFFTR - SQUARE-FREE PRODUCT GREATER THAN 210',44,4,1)
C/
C
      GO TO 40
C
 10   CONTINUE
C  FOR CONVENIENCE TO THE USER, PUT THE REAL COEFFICIENTS,
C  WHICH DRLTR STORES IN THE LOCATIONS A(1), A(3),...,A(2N+1)
C  INTO THE BEGINNING OF THE A ARRAY, AFTER FIRST PUTTING
C  THE IMAGINARY COEFFICIENTS, RETURNED BY DRLTR IN LOCATIONS
C  A(2),A(4),...,A(2N+2), INTO THE ARRAY B.
C
      J = 2
      KK = NNP2/2
      DO 20 K=1,KK
      B(K) = A(J)
      J = J+2
 20   CONTINUE
C
      J=3
      DO 30 K=2,KK
      A(K) = A(J)
      J = J+2
 30   CONTINUE
C
 40   CALL LEAVE
C
      RETURN
      END
