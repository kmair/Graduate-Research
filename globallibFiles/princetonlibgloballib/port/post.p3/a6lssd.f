      SUBROUTINE A6LSSD(ALFA, GAMMA, NU, NRHS, CC, SGAMMA, BC, C
     1   , G, ED, NDEQS, PIVOT, DIAG)
      INTEGER NRHS, NU, NDEQS
      INTEGER BC(NU), ED(NU), PIVOT(NU)
      REAL ALFA(NU, NU), GAMMA(NU, NRHS), CC(NU, NU), SGAMMA(NU, NRHS)
     1   , C(NDEQS, NU), G(NDEQS, NRHS)
      REAL DIAG(NU)
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      INTEGER ISTKGT, NERROR, IPIVOT, I, J, NERR
      INTEGER IS(1000), NDVAR
      REAL RS(1000), WS(500)
      LOGICAL LS(1000)
      INTEGER TEMP, TEMP1
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
C SCRATCH SPACE ALLOCATED - S(A6LSSD) <= NU REAL WORDS +
C                                        NU INTEGER WORDS.
      CALL ENTER(1)
C COPY ALFA AND GAMMA INTO
      DO  3 I = 1, NDEQS
C PROPERLY DIMENSIONED ARRAYS.
         DO  1 J = 1, NU
            TEMP1 = ED(I)
            C(I, J) = ALFA(TEMP1, J)
   1        CONTINUE
         DO  2 J = 1, NRHS
            TEMP1 = ED(I)
            G(I, J) = GAMMA(TEMP1, J)
   2        CONTINUE
   3     CONTINUE
      NDVAR = 0
C COUNT THE VARIABLES.
      DO  7 J = 1, NU
         DO  5 I = 1, NDEQS
            IF (C(I, J) .EQ. 0.) GOTO 4
               NDVAR = NDVAR+1
               GOTO  6
   4        CONTINUE
   5        CONTINUE
   6     CONTINUE
   7     CONTINUE
      IF (NDVAR .GE. NDEQS) GOTO 8
C/6S
C        CALL SETERR(
C    1      57H GLSSB - DIRICHLET BOUNDARY CONDITIONS ARE OVERDETERMINED
C    2      , 57, 4, 1)
C/7S
         CALL SETERR(
     1      ' GLSSB - DIRICHLET BOUNDARY CONDITIONS ARE OVERDETERMINED'
     2      , 57, 4, 1)
C/
         CALL LEAVE
         RETURN
C GET THE QR DECOMPOSITION OF C.
   8  CALL QRD(NDEQS, NU, C, DIAG, PIVOT)
      IF (NERROR(NERR) .EQ. 0) GOTO 9
         CALL ERROFF
C/6S
C        CALL SETERR(37H GLSSB - SINGULAR DIRICHLET SUBSYSTEM, 37, 6, 1)
C/7S
         CALL SETERR(' GLSSB - SINGULAR DIRICHLET SUBSYSTEM', 37, 6, 1)
C/
         CALL LEAVE
         RETURN
C FORM G = Q*G.
   9  CALL QRQTV(NDEQS, NU, C, DIAG, NRHS, G)
C GET A PHONY PIVOT(I) = I ARRAY.
      IPIVOT = ISTKGT(NU, 2)
      DO  10 I = 1, NU
         TEMP1 = IPIVOT-1+I
         IS(TEMP1) = I
  10     CONTINUE
      CALL QRBS(NDEQS, NDEQS, C, DIAG, IS(IPIVOT), NRHS, G, G)
      IF (NDEQS .LT. NU) CALL QRBS(NDEQS, NDEQS, C, DIAG, IS(IPIVOT),
     1   NU-NDEQS, C(1, NDEQS+1), C(1, NDEQS+1))
C PUT THE STANDARD BC'S INTO CC AND SGAMMA.
      DO  15 I = 1, NDEQS
         J = NDEQS+1
            GOTO  12
  11        J = J+1
  12        IF (J .GT. NU) GOTO  13
            TEMP1 = PIVOT(I)
            TEMP = PIVOT(J)
            CC(TEMP1, TEMP) = -C(I, J)
            GOTO  11
  13     DO  14 J = 1, NRHS
            TEMP = PIVOT(I)
            SGAMMA(TEMP, J) = G(I, J)
  14        CONTINUE
         TEMP = PIVOT(I)
         BC(TEMP) = 0
  15     CONTINUE
      CALL LEAVE
      RETURN
      END