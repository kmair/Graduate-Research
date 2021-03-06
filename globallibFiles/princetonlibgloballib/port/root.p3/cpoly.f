      SUBROUTINE CPOLY(DEGREE,OPR,OPI,ZEROR,ZEROI)
C
C FINDS THE ZEROS OF A COMPLEX POLYNOMIAL.
C
C  INPUTS -
C
C     OPR, OPI      -  VECTORS OF REAL AND IMAGINARY
C                      PARTS OF THE COEFFICIENTS IN
C                      ORDER OF DECREASING POWERS.
C
C     DEGREE        -  INTEGER DEGREE OF POLYNOMIAL.
C
C  OUTPUTS -
C
C     ZEROR, ZEROI  -  VECTORS OF REAL AND IMAGINARY
C                      PARTS OF THE ZEROS.
C
C THE PROGRAM HAS BEEN WRITTEN TO REDUCE THE CHANCE OF OVERFLOW
C OCCURRING. IF IT DOES OCCUR, THERE IS STILL A POSSIBILITY THAT
C THE ZEROFINDER WILL WORK PROVIDED THE OVERFLOWED QUANTITY IS
C REPLACED BY A LARGE NUMBER.
C
C
C  ERROR STATES -
C
C    1 - DEGREE IS LESS THAN 1
C    2 - LEADING COEFFICIENT IS ZERO
C    3 - NOT ALL ZEROS HAVE BEEN FOUND (RECOVERABLE)
C    4 - THE DYNAMIC STORAGE STACK IS NOT BIG ENOUGH
C
C
C PORT NOTE -
C
C THE ORIGINAL PROGRAM HAS BEEN ADAPTED TO PORT BY -
C
C   (1) PUTTING IN AUTOMATIC ERROR HANDLING.
C   (2) SUBSTITUTING DYNAMIC STACK ALLOCATION FOR THE DIMENSIONED
C       ARRAYS IN NAMED COMMON.
C   (3) CHANGING THE NAMES OF THE INTERNAL ROUTINES TO AVOID USER
C       NAME CONFLICT.
C
C  THE FOLLOWING NAME EQUIVALENCES (ORIGINAL - NEW) APPLY -
C
C            CMOD    -  CR1MOD
C            CDIVID  -  CR1DIV
C            CALCT   -  R1POLY
C            CAUCHY  -  R2POLY
C            ERREV   -  R3POLY
C            FXSHFT  -  R4POLY
C            NEXTH   -  R5POLY
C            NOSHFT  -  R6POLY
C            POLYEV  -  R7POLY
C            SCALE   -  R8POLY
C            VRSHFT  -  R9POLY
C
C
C DYNAMIC STORAGE SPACE USED -
C
C    THE CPOLY PROGRAMS USE 10*(DEGREE+1)
C    REAL LOCATIONS IN THE DYNAMIC STORAGE STACK.
C
C COMMON AREA
      COMMON/CSTAK/D(500)
      COMMON/P88PLY/SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN
C
      INTEGER DEGREE,CNT1,CNT2
      REAL R(1000)
      REAL SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN
      REAL XX,YY,COSR,SINR,SMALNO,BASE,XXX,ZR,ZI,BND,
     1    ANG,OPR(1),OPI(1),ZEROR(1),ZEROI(1),
     2    CR1MOD,R8POLY,R2POLY,SQRT,R1MACH
      DOUBLE PRECISION D
      LOGICAL CONV
C
      EQUIVALENCE (D(1),R(1))
C
C INITIALIZATION OF CONSTANTS
C
      ETA = R1MACH(4)
      ARE = ETA
      INFIN = R1MACH(2)
      SMALNO = R1MACH(1)
      BASE = I1MACH(10)
C
      ANG = 94.E0/180.E0*(4.E0*ATAN(1.E0))
      COSR = COS(ANG)
      SINR = SIN(ANG)
C
      XX = .5E0*SQRT(2.E0)
      YY = -XX
      MRE = 4.E0*XX*ETA
C
      NN = DEGREE+1
C
C THE DEGREE IS LESS THAN 1
C
C/6S
C     IF (DEGREE .LT. 1) CALL SETERR(
C    1   33HCPOLY - THE DEGREE IS LESS THAN 1,33,1,2)
C/7S
      IF (DEGREE .LT. 1) CALL SETERR(
     1   'CPOLY - THE DEGREE IS LESS THAN 1',33,1,2)
C/
C
C ALGORITHM FAILS IF THE LEADING COEFFICIENT IS ZERO.
C
C/6S
C     IF (OPR(1) .EQ. 0.0E0 .AND. OPI(1) .EQ. 0.0E0) CALL SETERR(
C    1   42H CPOLY - LEADING COEFFICIENT INPUT AS ZERO,42,2,2)
C/7S
      IF (OPR(1) .EQ. 0.0E0 .AND. OPI(1) .EQ. 0.0E0) CALL SETERR(
     1   ' CPOLY - LEADING COEFFICIENT INPUT AS ZERO',42,2,2)
C/
C
C REMOVE THE ZEROS AT THE ORIGIN IF ANY.
   10 IF (OPR(NN) .NE. 0.0E0 .OR. OPI(NN) .NE. 0.0E0) GO TO 20
          IDNN2 = DEGREE-NN+2
          ZEROR(IDNN2) = 0.0E0
          ZEROI(IDNN2) = 0.0E0
          NN = NN-1
          GO TO 10
C
C
C SET UP THE STORAGE IN THE DYNAMIC STORAGE STACK -
C IF THERE IS ROOM
C
   20 NSHORT = ISTKQU(3) - 10*NN
C/6S
C     IF (NSHORT .LE. 0) CALL SETERR(
C    1   47H CPOLY - THE DYNAMIC STORAGE LEFT IS NOT ENOUGH,47,3,2)
C/7S
      IF (NSHORT .LE. 0) CALL SETERR(
     1   ' CPOLY - THE DYNAMIC STORAGE LEFT IS NOT ENOUGH',47,3,2)
C/
C
      NNN = 10*NN
      IPR  = ISTKGT(NNN,3)
      IPI  = IPR  + NN
      IHR  = IPI  + NN
      IHI  = IHR  + NN
      IQPR = IHI  + NN
      IQPI = IQPR + NN
      IQHR = IQPI + NN
      IQHI = IQHR + NN
      ISHR = IQHI + NN
      ISHI = ISHR + NN
C
C MAKE A COPY OF THE COEFFICIENTS.
      DO 30 I = 1,NN
        IIPR  = IPR + I -1
        IIPI  = IPI + I -1
        IISHR = ISHR + I -1
          R(IIPR)  = OPR(I)
          R(IIPI)  = OPI(I)
          R(IISHR) = CR1MOD(R(IIPR),R(IIPI))
   30 CONTINUE
C
C SCALE THE POLYNOMIAL.
      BND = R8POLY (NN,R(ISHR),ETA,INFIN,SMALNO,BASE)
      IF (BND .EQ. 1.0E0) GO TO 40
      DO 35 I = 1,NN
        IIPR = IPR + I - 1
        IIPI = IPI + I - 1
          R(IIPR) = BND*R(IIPR)
          R(IIPI) = BND*R(IIPI)
   35 CONTINUE
C
C START THE ALGORITHM FOR ONE ZERO .
C
   40 IF (NN .GT. 2) GO TO 50
      IF (NN .EQ. 1) GO TO 110
C
C CALCULATE THE FINAL ZERO AND RETURN.
          CALL CR1DIV(-R(IPR+1),-R(IPI+1),R(IPR),R(IPI),
     1    ZEROR(DEGREE),ZEROI(DEGREE))
          CALL ISTKRL(1)
          RETURN
C
C CALCULATE BND, A LOWER BOUND ON THE MODULUS OF THE ZEROS.
   50 DO 60 I = 1,NN
        IISHR = ISHR + I - 1
        IIPR = IPR + I - 1
        IIPI = IPI + I - 1
          R(IISHR) = CR1MOD(R(IIPR),R(IIPI))
   60 CONTINUE
      BND = R2POLY(NN,R(ISHR),R(ISHI))
C
C OUTER LOOP TO CONTROL 2 MAJOR PASSES WITH DIFFERENT SEQUENCES
C OF SHIFTS.
      DO 100 CNT1 = 1,2
C FIRST STAGE CALCULATION, NO SHIFT.
          CALL R6POLY(5,R(IPR),R(IPI),R(IHR),R(IHI))
C INNER LOOP TO SELECT A SHIFT.
          DO 90 CNT2 = 1,9
C SHIFT IS CHOSEN WITH MODULUS BND AND AMPLITUDE ROTATED BY
C 94 DEGREES FROM THE PREVIOUS SHIFT
               XXX = COSR*XX-SINR*YY
               YY = SINR*XX+COSR*YY
               XX = XXX
               SR = BND*XX
               SI = BND*YY
C SECOND STAGE CALCULATION, FIXED SHIFT.
          CALL R4POLY(10*CNT2,ZR,ZI,CONV,R(IPR),R(IPI),R(IHR),R(IHI),
     1         R(IQPR),R(IQPI),R(IQHR),R(IQHI),R(ISHR),R(ISHI))
               IF (.NOT. CONV) GO TO 80
C THE SECOND STAGE JUMPS DIRECTLY TO THE THIRD STAGE ITERATION.
C IF SUCCESSFUL THE ZERO IS STORED AND THE POLYNOMIAL DEFLATED.
                    IDNN2 = DEGREE-NN+2
                    ZEROR(IDNN2) = ZR
                    ZEROI(IDNN2) = ZI
                    NN = NN-1
                    DO 70 I = 1,NN
                       IIPR  = IPR + I -1
                       IIPI  = IPI + I - 1
                       IIQPR = IQPR + I - 1
                       IIQPI = IQPI + I - 1
                         R(IIPR) = R(IIQPR)
                         R(IIPI) = R(IIQPI)
   70               CONTINUE
                    GO TO 40
   80          CONTINUE
C IF THE ITERATION IS UNSUCCESSFUL ANOTHER SHIFT IS CHOSEN.
   90     CONTINUE
C IF 9 SHIFTS FAIL, THE OUTER LOOP IS REPEATED WITH ANOTHER
C SEQUENCE OF SHIFTS.
  100 CONTINUE
C
C THE ZEROFINDER HAS FAILED ON TWO MAJOR PASSES.
C RETURN EMPTY HANDED.
C
      KP10 = IDNN2 + 10
C
C/6S
C     CALL SETERR(37H CPOLY - ONLY K ZEROS HAVE BEEN FOUND,37,KP10,1)
C/7S
      CALL SETERR(' CPOLY - ONLY K ZEROS HAVE BEEN FOUND',37,KP10,1)
C/
  110 CALL ISTKRL(1)
      RETURN
      END
