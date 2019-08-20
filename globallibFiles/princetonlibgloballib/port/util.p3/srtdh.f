      SUBROUTINE SRTDH ( A, L, SA, N )
C
C     SRTDH SORTS THE N HOLLERITH DATA ITEMS,
C     EACH L WORDS IN LENGTH, STARTING AT
C     A(1), A(SA+1), ..., A((N-1)*SA+1)
C     INTO DESCENDING COLLATING ORDER
C
      INTEGER A(1), SA, H
C
      NN = I0SRT( SA, N, H )
C/6S
C     IF ( L .GT. SA  .OR.  L .LT. 1 )
C    1   CALL SETERR( 26HSRTDH - ILLEGAL VALUE OF L, 26, 3, 2 )
C/7S
      IF ( L .GT. SA  .OR.  L .LT. 1 )
     1   CALL SETERR( 'SRTDH - ILLEGAL VALUE OF L', 26, 3, 2 )
C/
C
 10     IF ( H .LT. SA ) RETURN
        K = NN - H
C
C       MAIN LOOP - INSERTION SORT ON DATA ITEMS H APART IN A
C
        DO 90 J = 1, K, SA
          I = J
 20         M = I + L - 1
C
C           COMPARE DATA ITEMS AT A(I+H) AND A(I)
C
            DO 60 K1 = I, M
              K2 = K1 + H
              IF ( A(K1) ) 30, 40, 40
 30           IF ( A(K2) ) 50, 90, 90
 40           IF ( A(K2) ) 70, 50, 50
 50           IF ( A(K1) - A(K2) ) 70, 60, 90
 60           CONTINUE
C
C           SWITCH DATA ITEMS AT A(I+H) AND A(I)
C
 70         DO 80 K1 = I, M
              K2 = K1 + H
              NTEMP = A(K1)
              A(K1) = A(K2)
              A(K2) = NTEMP
 80           CONTINUE
C
            I = I - H
            IF ( I .GE. 1 ) GOTO 20
 90       CONTINUE
C
        H = (H - SA) / 3
        GOTO 10
C
      END
