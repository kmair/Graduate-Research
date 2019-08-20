C$SEQ NMSK 0 20
C$TEST NMSK
C***********************************************************************
C
C  EXAMPLE OF USE OF THE PORT PROGRAMS NSGB AND NSFB
C
C***********************************************************************
C *** NSGB AND NSFB EXAMPLE PROGRAM ***
C
C *** FIT N = 33 DATA POINTS (T,Y) TO THE CURVE
C *** X(1) + X(2)*EXP(T*X(4)) + X(3)*EXP(T*X(5))
C
C *** THE FOLLOWING CODE IS FOR CALLING NSGB.  DIFFERENCES FOR
C *** CALLING NSFB ARE EXPLAINED IN COMMENTS.
C
      INTEGER I, J, INC(4,2), IV(130), LIV, LTY, LV, UI(1)
      REAL BX(2,2), BIG, C(3), T(33), Y(33), V(461), X(5)
      EXTERNAL DUMMY, OSB1A, OSB1B, R1MACH
      REAL R1MACH
C
C *** FOR NSFB, OMIT OSB1B FROM THE EXTERNAL STATEMENT.
C
      DATA LIV/130/, LTY/50/, LV/461/
C
C *** TO MAKE THIS EXAMPLE SELF-CONTAINED, WE USE A DATA STATEMENT
C *** AND DO LOOP TO SUPPLY (T(I),Y(I)) PAIRS.
C
C *** Y VALUES...
C
      DATA Y(1) /8.44E-1/, Y(2) /9.08E-1/, Y(3)/9.32E-1/,
     1     Y(4) /9.36E-1/, Y(5) /9.25E-1/, Y(6)/9.08E-1/,
     2     Y(7) /8.81E-1/, Y(8) /8.50E-1/, Y(9)/8.18E-1/,
     3     Y(10)/7.84E-1/, Y(11)/7.51E-1/, Y(12)/7.18E-1/,
     4     Y(13)/6.85E-1/, Y(14)/6.58E-1/, Y(15)/6.28E-1/,
     5     Y(16)/6.03E-1/, Y(17)/5.80E-1/, Y(18)/5.58E-1/,
     6     Y(19)/5.38E-1/, Y(20)/5.22E-1/, Y(21)/5.06E-1/,
     7     Y(22)/4.90E-1/, Y(23)/4.78E-1/, Y(24)/4.67E-1/,
     8     Y(25)/4.57E-1/, Y(26)/4.48E-1/, Y(27)/4.38E-1/,
     9     Y(28)/4.31E-1/, Y(29)/4.24E-1/, Y(30)/4.20E-1/,
     A     Y(31)/4.14E-1/, Y(32)/4.11E-1/, Y(33)/4.06E-1/
C
C ***  T VALUES...
C
      DO 10 I = 1, 33
         T(I) = -10.E+0 * FLOAT(I-1)
 10      CONTINUE
C
C     ***  SET UP INC  ***
C
      DO 30 J = 1, 2
         DO 20 I = 1, 4
 20           INC(I,J) = 0
 30      CONTINUE
      INC(2,1) = 1
      INC(3,2) = 1
C
C *** SPECIFY ALL DEFAULT IV AND V INPUT COMPONENTS ***
C
      IV(1) = 0
C
C ... TO SET THE MAXIMUM NUMBER OF ITERATIONS TO 100 AND TURN OFF
C ... THE PRINTING OF THE ITERATION SUMMARY, WE WOULD REPLACE THE
C ... ABOVE ASSIGNMENT OF 0 TO IV(1) WITH THE FOLLOWING THREE LINES...
C
C     CALL IVSET(1, IV, LIV, LV, V)
C     IV(18) = 100
C     IV(19) = 0
C
C
C *** SUPPLY INITIAL GUESS...
C
      X(1) = 1.E-2
      X(2) = 2.E-2
C
C *** SET BIG TO LARGEST POSITIVE (MODEL) NUMBER...
C
      BIG = R1MACH(2)
C
C *** SUPPLY BOUNDS -- INCLUDING LOWER BOUNDS OF -BIG AND UPPER
C *** BOUNDS OF BIG WHERE WE DO NOT WISH TO IMPOSE BOUNDS...
C
      BX(1,1) = -BIG
      BX(2,1) = .0125E+0
      BX(1,2) = .03E+0
      BX(2,2) = BIG
C
C *** SOLVE THE PROBLEM -- NSGB WILL PRINT THE SOLUTION FOR US...
C
      CALL NSGB(33, 2, 3, X, BX, C, Y, OSB1A, OSB1B, INC, 4,
     1           IV, LIV, LV, V, UI, T, DUMMY)
C
C *** FOR NSFB, THE CORRESPONDING CALL WOULD BE...
C
C     CALL NSFB(33, 2, 3, X, BX, C, Y, OSB1A, INC, 4,
C    1           IV, LIV, LV, V, UI, T, DUMMY)
C
C
C *** NOTE -- ON MOST SYSTEMS, WE COULD SIMPLY PASS OSB1A (OR OSB1B)
C *** AS THE UF PARAMETER, SINCE OSB1A AND OSB1B IGNORE THIS
C *** PARAMETER.  BUT THERE EXIST SYSTEMS (E.G. UNIVAC) THAT WOULD
C *** GIVE A RUN-TIME ERROR IF WE DID THIS.  HENCE WE PASS THE
C *** IMMEDIATELY FOLLOWING DUMMY SUBROUTINE AS UF.
C
      STOP
      END
      SUBROUTINE DUMMY
      RETURN
      END
      SUBROUTINE OSB1A(N, P, L, X, NF, A, UI, T, UF)
C
C *** THIS ROUTINE COMPUTES THE A MATRIX, A = A(X),
C *** FOR TEST PROBLEM OSBORNE1.
C
      INTEGER L, N, NF, P, UI(1)
      REAL A(N,1), T(N), X(P)
      EXTERNAL UF
C
      INTEGER I
      REAL ONE, TI
      DATA ONE/1.E+0/
C
      DO 10 I = 1, N
         TI = T(I)
         A(I,1) = ONE
         A(I,2) =  EXP(TI*X(1))
         A(I,3) =  EXP(TI*X(2))
 10      CONTINUE
      RETURN
      END
      SUBROUTINE OSB1B(N, P, L, X, NF, B, UI, T, UF)
C
C *** THIS ROUTINE COMPUTES THE JACOBIAN TENSOR, B = B(X),
C *** FOR TEST PROBLEM OSBORNE1.
C
      INTEGER L, N, NF, P, UI(1)
      REAL B(N,2), T(N), X(P)
      EXTERNAL UF
C
      INTEGER I
      REAL TI
C
      DO 10 I = 1, N
         TI = T(I)
         B(I,1) = TI *  EXP(TI*X(1))
         B(I,2) = TI *  EXP(TI*X(2))
 10      CONTINUE
      RETURN
      END
