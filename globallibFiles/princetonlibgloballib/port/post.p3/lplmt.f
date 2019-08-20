      REAL FUNCTION LPLMT(T1, V1, NV, T2, V2, F, DT)
      INTEGER NV
      REAL T1, V1(NV), T2, V2(NV), F, DT
      INTEGER I, J
      REAL ABS, DTT, SGNDT, SGNT21, SGNV21
      LOGICAL TEMP
C TO RETURN THE VALUE OF DT THAT WILL CAUSE THE BREAK-POINTS TO MOVE
C NO MORE THAN A FACTOR OF F CLOSER TOGETHER OVER THE NEXT TIME-STEP.
C/6S
C     IF (T1 .EQ. T2) CALL SETERR(17H LPLMT - T1 == T2, 17, 1, 2)
C     IF (NV .LT. 2) CALL SETERR(18H LPLMT - NV .LT. 2, 18, 2, 2)
C/7S
      IF (T1 .EQ. T2) CALL SETERR(' LPLMT - T1 == T2', 17, 1, 2)
      IF (NV .LT. 2) CALL SETERR(' LPLMT - NV .LT. 2', 18, 2, 2)
C/
      TEMP = F .LE. 0.
      IF (.NOT. TEMP) TEMP = F .GE. 1.
C/6S
C     IF (TEMP) CALL SETERR(23H LPLMT - F NOT IN (0,1), 23, 3, 2)
C     IF (DT .EQ. 0.) CALL SETERR(16H LPLMT - DT == 0, 16, 4, 2)
C/7S
      IF (TEMP) CALL SETERR(' LPLMT - F NOT IN (0,1)', 23, 3, 2)
      IF (DT .EQ. 0.) CALL SETERR(' LPLMT - DT == 0', 16, 4, 2)
C/
      DTT = DT
      SGNDT = DT/ABS(DT)
      SGNT21 = (T2-T1)/ABS(T2-T1)
      DO  3 I = 1, NV
         IF (V2(I) .EQ. V1(I)) GOTO  3
         SGNV21 = (V2(I)-V1(I))/ABS(V2(I)-V1(I))
         DO  2 J = 1, NV
            IF (I .EQ. J) GOTO  2
            IF (V2(I) .NE. V2(J)) GOTO 1
               DTT = 0
               LPLMT = DTT
               RETURN
   1        TEMP = SGNDT*SGNT21*((V2(I)-V1(I))/ABS(V2(I)-V1(I)))*((V2(I)
     1         -V2(J))/ABS(V2(I)-V2(J))) .LT. 0.
            IF (TEMP) TEMP = SGNDT*SGNV21*(DTT*(V2(I)-V1(I))-(T2-T1)*(
     1         V2(I)-V2(J))*(F-1.)) .GT. 0.
            IF (TEMP) DTT = (T2-T1)*(V2(I)-V2(J))*(F-1.)/(V2(I)-V1(I))
   2        CONTINUE
   3     CONTINUE
      LPLMT = DTT
      RETURN
      END
