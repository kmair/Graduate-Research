      LOGICAL FUNCTION A4SSOX(WV, RV, IV, LV, N, ME)
      INTEGER ME
      INTEGER IV(40), N(ME)
      REAL WV(30), RV(30)
      LOGICAL LV(20)
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      INTEGER ISTKMD, ISTKGT, MIN0, MAX0, IS(1000), ITEMP
      REAL ABS, AMAX1, RS(1000), WS(500), FLOAT
      LOGICAL LS(1000)
      INTEGER TEMP1, TEMP2, TEMP3, TEMP4, TEMP5
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
C DO THE EXTRAPOLATION.
      LV(6) = .FALSE.
      LV(7) = .FALSE.
      LV(4) = .FALSE.
      LV(5) = .FALSE.
      A4SSOX = .FALSE.
      IV(18) = ME
      TEMP5 = IV(18)
      IF (10.*AMAX1(ABS(WV(10)), ABS(WV(11)))*RV(11) .LT. ABS(WV(11)-WV(
     1   10))*RV(4)/FLOAT(N(TEMP5))) GOTO 1
C/6S
C        CALL SETERR(13H ESSOM - DT=0, 13, 15, 1)
C/7S
         CALL SETERR(' ESSOM - DT=0', 13, 15, 1)
C/
         A4SSOX = .TRUE.
         RETURN
   1  IF (LV(2)) GOTO 2
         IV(22) = ISTKGT(IV(1), 3)
         A4SSOX = .TRUE.
         RETURN
   2  IF (MIN0(IV(18), IV(2)) .LE. IV(20)) GOTO 3
         ITEMP = ISTKMD(IV(1)*MIN0(IV(18), IV(2)))
C EXPAND THE EXTRAPOLATION LOZENGE.
C/6S
C        IF (IV(13) .LT. ITEMP) CALL SETERR(
C    1      47H ESSOM - SOMEBODY IS LEAVING STUFF ON THE STACK, 47, 20
C    2      , 2)
C        IF (IV(13) .GT. ITEMP) CALL SETERR(
C    1      50H ESSOM - SOMEBODY IS REMOVING STUFF FROM THE STACK, 50,
C    2      21, 2)
C/7S
         IF (IV(13) .LT. ITEMP) CALL SETERR(
     1      ' ESSOM - SOMEBODY IS LEAVING STUFF ON THE STACK', 47, 20
     2      , 2)
         IF (IV(13) .GT. ITEMP) CALL SETERR(
     1      ' ESSOM - SOMEBODY IS REMOVING STUFF FROM THE STACK', 50,
     2      21, 2)
C/
   3  IV(20) = MAX0(IV(18), IV(20))
C THE BEST ERROR IN THE LOZENGE.
      IV(22) = ISTKGT(IV(1), 3)
C THE LOZENGE ERROR.
      IV(21) = ISTKGT(MAX0(1, IV(1)*MIN0(IV(18)-1, IV(2))), 3)
      TEMP5 = IV(12)
      TEMP4 = IV(27)
      TEMP3 = IV(13)
      TEMP2 = IV(21)
      TEMP1 = IV(22)
      CALL XTRAP(WS(TEMP5), IV(18), IV(1), WS(TEMP4), IV(2), LV(1), WS(
     1   TEMP3), RS(TEMP2), RS(TEMP1))
      RETURN
      END
