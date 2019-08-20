      LOGICAL FUNCTION A6LSBI(K, X, NX, AF, AF1, NU, NRHS, NRHSG
     1   , GETJAC, SEPATE, A1, A2, F1, A1T, A2T, F1T, AS, FS, SBASIS)
      INTEGER K, NRHS, NU, NX
      EXTERNAL AF, AF1
      INTEGER NRHSG
      REAL X(NX), A1(NU, NU, 2), A2(NU, NU, 2), F1(NU, NRHS, 2), A1T(NU,
     1   NU, 2), A2T(NU, NU, 2)
      REAL F1T(NU, NRHS, 2), AS(NU, NU, 8), FS(NU, NRHS, 4), SBASIS(K, 2
     1   )
      LOGICAL AF, GETJAC, SEPATE
      INTEGER JLR, ID(2)
      REAL LR(2)
      LOGICAL TEMP
      DATA ID(1)/0/
      DATA ID(2)/1/
      LR(1) = X(1)
      LR(2) = X(NX)
C DO L AND R TERMS.
      DO  4 JLR = 1, 2
C   GET THE BASIS SPLINES AT L OR R.
         CALL BSPL1(K, X, NX, LR(JLR), 1, K+(JLR-1)*(NX-2*K), ID, 2,
     1      SBASIS)
C DEFAULT A AND F VALUES.
         CALL SETR(4*NU*(2*NU+NRHS), 0E0, AS)
         IF (.NOT. AF(LR(JLR), 1, NU, NRHS, NRHSG, GETJAC, SEPATE, AS(1,
     1      1, 1), AS(1, 1, 2), AS(1, 1, 3), AS(1, 1, 4), AS(1, 1, 5),
     2      AS(1, 1, 6), AS(1, 1, 7), AS(1, 1, 8), FS(1, 1, 1), FS(1, 1,
     3      2), FS(1, 1, 3), FS(1, 1, 4), K+(JLR-1)*(NX-2*K), SBASIS, K,
     4      X, NX, AF1)) GOTO 1
            A6LSBI = .TRUE.
            RETURN
   1     IF (.NOT. GETJAC) GOTO 3
            CALL MOVEFR(NU**2, AS(1, 1, 1), A1(1, 1, JLR))
            CALL MOVEFR(NU**2, AS(1, 1, 3), A2(1, 1, JLR))
            IF (.NOT. SEPATE) GOTO 2
               CALL MOVEFR(NU**2, AS(1, 1, 2), A1T(1, 1, JLR))
               CALL MOVEFR(NU**2, AS(1, 1, 4), A2T(1, 1, JLR))
   2        CONTINUE
   3     IF (NRHSG .GT. 0) CALL MOVEFR(NU*NRHSG, FS(1, 1, 1), F1(1, 1,
     1      JLR))
         TEMP = SEPATE
         IF (TEMP) TEMP = NRHSG .GT. 0
         IF (TEMP) CALL MOVEFR(NU*NRHSG, FS(1, 1, 2), F1T(1, 1, JLR))
   4     CONTINUE
C END JLR.
      A6LSBI = .FALSE.
      RETURN
      END
