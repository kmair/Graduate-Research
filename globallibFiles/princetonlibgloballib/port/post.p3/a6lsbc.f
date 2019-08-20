      LOGICAL FUNCTION A6LSBC(NU, ORDER, BC, E, MAXORD, COUNT)
      INTEGER NU
      INTEGER ORDER(NU, NU, 2), BC(NU, 2, 2), E(NU, 2, 2), MAXORD(NU, 2)
     1   , COUNT(NU, 2)
      INTEGER MAX0, I, J, L
      INTEGER TEMP1
      LOGICAL TEMP
      CALL SETI(2*NU, -1, MAXORD)
      CALL SETI(2*NU, 0, COUNT)
      DO  3 L = 1, 2
         DO  2 I = 1, NU
            DO  1 J = 1, NU
               MAXORD(I, L) = MAX0(MAXORD(I, L), ORDER(I, J, L))
   1           CONTINUE
   2        CONTINUE
   3     CONTINUE
      DO  9 L = 1, 2
         DO  8 I = 1, NU
            IF (BC(I, 1, L) .NE. 0) GOTO 5
               TEMP1 = E(I, 1, L)
               COUNT(TEMP1, L) = COUNT(TEMP1, L)+1
               TEMP1 = E(I, 1, L)
               IF (MAXORD(TEMP1, L) .GT. BC(I, 1, L)) GOTO 4
                  A6LSBC = .FALSE.
                  RETURN
   4           CONTINUE
   5        IF (BC(I, 2, L) .NE. 1) GOTO 7
               TEMP1 = E(I, 2, L)
               COUNT(TEMP1, L) = COUNT(TEMP1, L)+1
               TEMP1 = E(I, 2, L)
               IF (ORDER(TEMP1, I, L) .GT. BC(I, 2, L)) GOTO 6
                  A6LSBC = .FALSE.
                  RETURN
   6           CONTINUE
   7        CONTINUE
   8        CONTINUE
   9     CONTINUE
      DO  12 I = 1, NU
         TEMP = COUNT(I, 1) .GT. 1
         IF (.NOT. TEMP) TEMP = COUNT(I, 2) .GT. 1
         IF (.NOT. TEMP) GOTO 10
            A6LSBC = .FALSE.
            RETURN
  10     IF (COUNT(I, 1)+COUNT(I, 2) .LE. MAX0(MAXORD(I, 1), MAXORD(I, 2
     1      ))) GOTO 11
            A6LSBC = .FALSE.
            RETURN
  11     CONTINUE
  12     CONTINUE
      A6LSBC = .TRUE.
      RETURN
      END
