      SUBROUTINE SRTRR ( A, SA, P, SP, N )
C
C     SRTRR REARRANGES A(1), A(SA+1), ..., A((N-1)*SA+1)
C     ACCORDING TO PERMUTATION OF INTEGERS 1 TO N STORED
C     IN P(1), P(SP+1), ..., P((N-1)*SP+1)
C
      INTEGER SA, SP, P(SP, 1)
      REAL A(SA, 1), TEMP
C
C     CHECK INPUT PARAMETERS
C
      CALL I1SRT( SA, SP, N )
C
C     CHECK IF SORTING NECESSARY
C
      IF ( N .LE. 1 ) RETURN
      I = 0
C
C       CHECK IF HAVE COMPLETED PERMUTATION
C
 10     I = I + 1
        IF ( I .GT. N ) GOTO 40
        IF ( P(1, I) .LE. 0 ) GOTO 10
C
C       REARRANGE A ACCORDING TO CYCLE STARTING AT I
C
        TEMP = A(1, I)
 20       L = I
          I = P(1, I)
          P(1, L) = -P(1, L)
          IF ( P(1, I) .LE. 0 ) GOTO 30
          A(1, L) = A(1, I)
          GOTO 20
C
C       FINISH CYCLE
C
 30     A(1, L) = TEMP
        GOTO 10
C
C     RESTORE P ARRAY
C
 40   DO 50 I = 1, N
 50     P(1, I) = - P(1, I)
C
      RETURN
C
      END
