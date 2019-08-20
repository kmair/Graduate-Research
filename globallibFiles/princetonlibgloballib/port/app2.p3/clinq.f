      SUBROUTINE CLINQ (N,AR,AI,BR,BI,NB,XR,XI)
C
C SOLUTION OF A SET OF COMPLEX LINEAR EQUATIONS
C
C  METHOD - QR DECOMPOSITION OF A BY HOUSEHOLDER TRANSFORMATIONS
C           FOLLOWED BY BACK SUBSTITUTION
C
C  INPUT
C
C    N    - THE NUMBER OF EQUATIONS
C    AR   - THE REAL PART OF THE MATRIX
C    AI   - THE IMAGINARY PART OF THE MATRIX
C    BR   - THE REAL PART OF THE RIGHT-HAND SIDES
C    BI   - THE IMAGINARY PART OF THE RIGHT-HAND SIDES
C    NB   - THE NUMBER OF RIGHT-HAND SIDES
C
C  OUTPUT
C
C    AR   - BOTH THE REAL AND IMAGINARY PARTS OF THE
C    AI   - MATRIX HAVE BEEN CLOBBERED.
C    BR   - BOTH THE REAL AND IMAGINARY PARTS OF THE
C    BI   - MATRIX HAVE BEEN CLOBBERED.
C    XR   - THE REAL PART OF THE SOLUTION VECTORS
C    XI   - THE IMAGINARY PART OF THE SOLUTION VECTORS
C
C  ERROR STATES -
C
C    1 - N.LT.1
C    2 - NB.LT.1
C    3 - A IS SINGULAR (RECOVERABLE)
C
      REAL AR(N,N),BR(N,NB),XR(N,NB)
      REAL AI(N,N),BI(N,NB),XI(N,NB)
C
C   SAVE AND TURN ON RECOVERY SWITCH
C
      CALL ENTSRC(IRSAVE, 1)
C
C ... CHECK THE INPUT.
C
C/6S
C     IF (N.LT.1) CALL SETERR(15H CLINQ - N.LT.1,15,1,2)
C/7S
      IF (N.LT.1) CALL SETERR(' CLINQ - N.LT.1',15,1,2)
C/
C
C/6S
C     IF (NB.LT.1) CALL SETERR(16H CLINQ - NB.LT.1,16,2,2)
C/7S
      IF (NB.LT.1) CALL SETERR(' CLINQ - NB.LT.1',16,2,2)
C/
C
C ... CALL CLST2 TO GET THE SOLUTION
C
      CALL CLST2(N,N,N,N,AR,AI,BR,BI,NB,XR,XI)
C
      IF (NERROR(NERR).EQ.0) GO TO 10
C
C ... SINGULAR MATRIX
C
      CALL ERROFF
C/6S
C     CALL SETERR(22H CLINQ - A IS SINGULAR,22,3,1)
C/7S
      CALL SETERR(' CLINQ - A IS SINGULAR',22,3,1)
C/
C
 10   CALL RETSRC(IRSAVE)
      RETURN
      END