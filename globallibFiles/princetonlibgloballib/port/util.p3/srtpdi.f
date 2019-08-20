      SUBROUTINE SRTPDI ( A, SA, P, SP, N )
C
C     SRTPDI SETS P(1) = 1, P(SP+1) = 2, ..., P((N-1)*SP+1) = N
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
        DO 40 J = 1, K
          I = J
 30         IH = I + H
            PI = P(1, I)
            PIH = P(1, IH)
            IF ( A(1, PI) .GE. A(1, PIH) ) GOTO 40
C
C           EXCHANGE
C
            P(1, I) = PIH
            P(1, IH) = PI
C
C           PERCOLATE EXCHANGED LIST ELEMENT UP TO PROPER PLACE
C
            I = I - H
            IF ( I .GE. 1 ) GOTO 30
 40       CONTINUE
C
        H = ( H - 1 ) / 3
        GOTO 20
C
      END
