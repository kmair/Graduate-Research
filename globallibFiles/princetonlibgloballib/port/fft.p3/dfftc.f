      SUBROUTINE DFFTC(N,AR,AI)
C
C DFFTC FINDS THE FOURIER TRANSFORM OF THE COMPLEX
C DATA STORED IN THE REAL AND IMAGINARY ARRAYS, AR AND AI
C BY SIMPLY SETTING UP THE CORRECT CALL TO DFFT.
C
C STORAGE IN THE DYNAMIC STORAGE STACK IS REQUIRED FOR
C 220 INTEGER VARIABLES AND 92 DOUBLE PRECISION VARIABLES.
C
C     INPUT
C
C       N     -NUMBER OF COMPLEX DATA POINTS
C       AR    -VECTOR OF LENGTH N CONTAINING REAL COMPONENTS
C       AI    -VECTOR OF LENGTH N CONTAINING IMAGINARY COMPONENTS
C
C     OUTPUT
C
C       AR    -CONTAINS THE REAL (COSINE)
C              COMPONENTS OF THE FOURIER COEFFICIENTS
C       AI    -CONTAINS THE IMAGINARY (SINE)
C              COMPONENTS OF THE FOURIER COEFFICIENTS
C
C
C NUMBER OF FACTORS OF N MUST NOT EXCEED 11.
C MAXIMUM PRIME FACTOR OF N MUST NOT EXCEED 23.
C PRODUCT OF THE SQUARE-FREE FACTORS, IF THERE EXISTS MORE THAN ONE,
C MUST NOT EXCEED 210.
C
C
C     ERROR STATES
C          1.  N IS LESS THAN 2
C          2.  PRIME FACTOR .GT. 23
C              (RECOVERABLE)
C          3.  SQUARE-FREE FACTOR PRODUCT .GT. 210
C              (RECOVERABLE)
C
C
C COMMON AREA
      COMMON/CSTAK/DSTAK(500)
C
      DOUBLE PRECISION AR(1),AI(1)
      DOUBLE PRECISION DSTAK
C
C  TEST THE VALIDITY OF THE INPUTS
C
C/6S
C     IF(N .LT. 2) CALL SETERR(
C    1   24HDFFTC - N IS LESS THAN 2,24,1,2)
C/7S
      IF(N .LT. 2) CALL SETERR(
     1   'DFFTC - N IS LESS THAN 2',24,1,2)
C/
C ENTER THE RECOVERY MODE (STORING THE PREVIOUS)
C
      CALL ENTER(1)
C
C  CALL THE SUBPROGRAM, DFFT.
C
      CALL DFFT(AR,AI,N,N,N,1)
C
C  CHECK FOR ERRORS FROM FFT
C
      IF (NERROR(NERR) .EQ. 0) GO TO 10
C
      CALL ERROFF
C
C/6S
C     IF (NERR .EQ. 5) CALL SETERR(
C    1   36HDFFTC - PRIME FACTOR GREATER THAN 23,36,2,1)
C/7S
      IF (NERR .EQ. 5) CALL SETERR(
     1   'DFFTC - PRIME FACTOR GREATER THAN 23',36,2,1)
C/
C
C/6S
C     IF (NERR .EQ. 6) CALL SETERR(
C    1   44HDFFTC - SQUARE-FREE PRODUCT GREATER THAN 210,44,3,1)
C/7S
      IF (NERR .EQ. 6) CALL SETERR(
     1   'DFFTC - SQUARE-FREE PRODUCT GREATER THAN 210',44,3,1)
C/
C
 10   CALL LEAVE
C
      RETURN
      END