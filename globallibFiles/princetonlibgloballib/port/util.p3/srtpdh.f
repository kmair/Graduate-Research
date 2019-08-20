      SUBROUTINE SRTPDH ( A, L, SA, P, SP, N )
C
C     SRTPDH SETS P(1) = 1, P(SP+1) = 2, ..., P((N-1)*SP+1) = N
C     AND THEN REARRANGES P(1), P(SP+1), ..., P((N-1)*SP+1) SO THAT
C     A( (P(I)-1)*SA+1 ) .GE. A( (P(J)-1)*SA+1 ) IF AND ONLY IF
C     I .GT. J, WHERE I AND J SUBSCRIPT PROPER ELEMENTS OF P
C
      INTEGER SP, P(SP, 1), SA, H, PIH, PI
      INTEGER A(SA, 1)
C
C     CHECK INPUT PARAMETERS AND INITIALIZE H
C
      CALL I1SRT( SA, SP, N )
C/6S
C     IF ( L .GT. SA  .OR.  L .LT. 1 )
C    1   CALL SETERR( 27HSRTPDH - ILLEGAL VALUE OF L, 27, 4, 2)
C/7S
      IF ( L .GT. SA  .OR.  L .LT. 1 )
     1   CALL SETERR( 'SRTPDH - ILLEGAL VALUE OF L', 27, 4, 2)
C/
      IF ( I0SRT( 1, N, H ) .LT. 1 ) RETURN
C
C     INITIALIZE P
C
      DO 10 I = 1, N
 10     P(1, I) = I
C
C       CHECK IF DONE WITH SORT
C
 20     IF ( H .LT. 1 ) RETURN
        K = N - H
C
C       COMPARE
C
        DO 100 J = 1, K
          I = J
 30         IH = I + H
            PI = P(1, I)
            PIH = P(1, IH)
C
C           CHECK ORDER OF HOLLERITH DATA
C
            DO 70 M = 1, L
              IF ( A(M, PI) ) 40, 50, 50
 40           IF ( A(M, PIH) ) 60, 100, 100
 50           IF ( A(M, PIH) ) 80, 60, 60
 60           IF ( A(M, PI) - A(M, PIH) ) 80, 70, 100
 70           CONTINUE
C
C           EXCHANGE
C
 80         P(1, I) = P(1, IH)
            P(1, IH) = PI
C
C           PERCOLATE EXCHANGED LIST ELEMENT UP TO PROPER PLACE
C
            I = I - H
            IF ( I .GE. 1 ) GOTO 30
 100      CONTINUE
C
        H = ( H - 1 ) / 3
        GOTO 20
C
      END
