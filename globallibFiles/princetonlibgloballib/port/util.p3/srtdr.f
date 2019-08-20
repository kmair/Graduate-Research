      SUBROUTINE SRTDR ( A, SA, N )
C
C     SRTDR SORTS THE N ITEMS
C     A(1), A(SA+1), ..., A((N-1)*SA+1)
C     INTO DESCENDING ORDER
C
      INTEGER SA, H
      REAL A(1), TEMP
C
C     CHECK INPUT PARAMETERS
C
      NN = I0SRT( SA, N, H )
C
 10     IF ( H .LT. SA ) RETURN
        K = NN - H
C
C       COMPARE
C
        DO 30 J = 1, K, SA
          I = J
 20         IH = I + H
            IF ( A(IH) .LE. A(I) ) GOTO 30
C
C           EXCHANGE
C
            TEMP = A(IH)
            A(IH) = A(I)
            A(I) = TEMP
C
C           PERCOLATE EXCHANGED ITEM UP LIST TO PROPER PLACE
C
            I = I - H
            IF (I .GE. 1) GOTO 20
 30       CONTINUE
C
        H = (H - SA) / 3
        GOTO 10
C
      END
