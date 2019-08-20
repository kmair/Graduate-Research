      SUBROUTINE POSTP(I, J, A, M, N)
      INTEGER I, J, M, N
      REAL A(1)
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      COMMON /A9OSTS/ IMEM
      INTEGER IMEM(4)
      INTEGER ISTKGT, II, IS(1000)
      REAL RS(1000), WS(1000)
      LOGICAL LS(1000)
      INTEGER TEMP1, TEMP2, TEMP3, TEMP4, TEMP5, TEMP6
      INTEGER TEMP7, TEMP8
      LOGICAL TEMP
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
C THE DEFAULT LINEAR SYSTEM PRE-CONDITIONING ROUTINE FOR POSTS.
C A(M,N)
C THE PORT LIBRARY STACK AND ITS ALIASES.
      II = I
      IF (IMEM(I) .NE. 0) GOTO 3
         IMEM(I) = ISTKGT(5, 2)
C INITIALIZE.
C R, CA, RCA, CB AND RCB.
CR.
         TEMP3 = IMEM(II)
         IS(TEMP3) = ISTKGT(M, 3)
C CA.
         TEMP3 = IMEM(II)
         IS(TEMP3+1) = ISTKGT(M, 3)
C RCA.
         TEMP3 = IMEM(II)
         IS(TEMP3+2) = ISTKGT(M, 2)
         IF (I .NE. 4) GOTO 2
            TEMP3 = IMEM(II)
            IS(TEMP3+3) = ISTKGT(1, 3)
            TEMP3 = IMEM(II)
            IS(TEMP3+4) = ISTKGT(1, 2)
            DO  1 II = 1, 3
               TEMP3 = IMEM(II)
               IS(TEMP3+3) = ISTKGT(M+1, 3)
               TEMP3 = IMEM(II)
               IS(TEMP3+4) = ISTKGT(M+1, 2)
   1           CONTINUE
   2     CONTINUE
         GOTO  10
   3     TEMP = I .EQ. 1
C SCALE.
         IF (TEMP) TEMP = J .EQ. 1
         IF (.NOT. TEMP) GOTO 4
            TEMP3 = IMEM(II)
C SCALE THE BANDED PDE JACOBIAN.
            TEMP4 = IS(TEMP3)
            TEMP5 = IMEM(II)
            TEMP6 = IS(TEMP5+1)
            TEMP7 = IMEM(II)
            TEMP8 = IS(TEMP7+2)
            CALL RCSBA(A, M, N, WS(TEMP4), WS(TEMP6), IS(TEMP8))
            GOTO  9
   4        IF (J .NE. 1) GOTO 5
               TEMP8 = IMEM(II)
C SCALE THE DENSE JACOBIAN.
               TEMP7 = IS(TEMP8)
               TEMP6 = IMEM(II)
               TEMP5 = IS(TEMP6+1)
               TEMP4 = IMEM(II)
               TEMP3 = IS(TEMP4+2)
               CALL RCSA(A, M, N, WS(TEMP7), WS(TEMP5), IS(TEMP3))
               GOTO  8
   5           IF (J .NE. 2) GOTO 6
                  TEMP3 = IMEM(II)
C SCALE THE RHS OF THE EQUATIONS.
                  TEMP4 = IS(TEMP3)
                  TEMP5 = IMEM(II)
                  TEMP6 = IS(TEMP5+3)
                  TEMP7 = IMEM(II)
                  TEMP8 = IS(TEMP7+4)
                  CALL RCSB(A, M, N, WS(TEMP4), WS(TEMP6), IS(TEMP8))
                  GOTO  7
   6              TEMP8 = IMEM(II)
C SCALE THE SOLUTION X.
                  TEMP7 = IS(TEMP8+1)
                  TEMP6 = IMEM(II)
                  TEMP5 = IS(TEMP6+2)
                  TEMP4 = IMEM(II)
                  TEMP3 = IS(TEMP4+3)
                  TEMP2 = IMEM(II)
                  TEMP1 = IS(TEMP2+4)
                  CALL RCSX(A, M, N, WS(TEMP7), IS(TEMP5), WS(TEMP3),
     1               IS(TEMP1))
   7        CONTINUE
   8     CONTINUE
   9     CONTINUE
  10  RETURN
      END
