      SUBROUTINE FFTCI(N,FR,FI)
C
C FFTCI FINDS THE INVERSE FOURIER TRANSFORM BY
C CALLING FFT TO INVERT THE TRANSFORM WHOSE REAL
C AND COMPLEX FOURIER COEFFICIENTS ARE PROVIDED
C IN THE ARRAYS FR AND FI.
C
C STORAGE IN THE DYNAMIC STORAGE STACK IS REQUIRED FOR
C 220 INTEGER VARIABLES AND 92 REAL VARIABLES.
C
C     INPUT
C
C       N     -NUMBER OF COEFFICIENTS
C       FR    -VECTOR OF LENGTH N CONTAINING THE REAL COEFFICIENTS
C       FI    -VECTOR OF LENGTH N CONTAINING THE IMAGINARY COEFFICIENTS
C
C     OUTPUT
C
C       FR    -CONTAINS THE REAL COMPONENTS OF THE INVERSE TRANSFORM
C       FI    -CONTAINS THE IMAGINARY COMPONENTS
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
      REAL FR(1),FI(1)
      DOUBLE PRECISION DSTAK
C
C  TEST THE VALIDITY OF THE INPUTS
C
C/6S
C     IF(N .LT. 2) CALL SETERR(
C    1   25H FFTCI - N IS LESS THAN 2,25,1,2)
C/7S
      IF(N .LT. 2) CALL SETERR(
     1   ' FFTCI - N IS LESS THAN 2',25,1,2)
C/
C ENTER THE RECOVERY MODE (STORING THE PREVIOUS)
C
      CALL ENTER(1)
C
C  CALL THE SUBPROGRAM, FFT.
C
      CALL FFT(FR,FI,N,N,N,-1)
C
C  CHECK FOR ERRORS FROM FFT
C
      IF (NERROR(NERR) .EQ. 0) GO TO 10
C
      CALL ERROFF
C
C/6S
C     IF (NERR .EQ. 5) CALL SETERR(
C    1   37H FFTCI - PRIME FACTOR GREATER THAN 23,37,2,1)
C/7S
      IF (NERR .EQ. 5) CALL SETERR(
     1   ' FFTCI - PRIME FACTOR GREATER THAN 23',37,2,1)
C/
C
C/6S
C     IF (NERR .EQ. 6) CALL SETERR(
C    1   45H FFTCI - SQUARE-FREE PRODUCT GREATER THAN 210,45,3,1)
C/7S
      IF (NERR .EQ. 6) CALL SETERR(
     1   ' FFTCI - SQUARE-FREE PRODUCT GREATER THAN 210',45,3,1)
C/
C
 10   CALL LEAVE
C
      RETURN
      END
