      DOUBLE PRECISION FUNCTION DLPLMT(T1, V1, NV, T2, V2, F, DT)
      INTEGER NV
      DOUBLE PRECISION T1, V1(NV), T2, V2(NV), F, DT
      INTEGER I, J
      DOUBLE PRECISION DTT, DABS, SGNDT, SGNT21, SGNV21
      LOGICAL TEMP
C TO RETURN THE VALUE OF DT THAT WILL CAUSE THE BREAK-POINTS TO MOVE
C NO MORE THAN A FACTOR OF F CLOSER TOGETHER OVER THE NEXT TIME-STEP.
C/6S
C     IF (T1 .EQ. T2) CALL SETERR(17HDLPLMT - T1 == T2, 17, 1, 2)
C     IF (NV .LT. 2) CALL SETERR(18HDLPLMT - NV .LT. 2, 18, 2, 2)
C/7S
      IF (T1 .EQ. T2) CALL SETERR('DLPLMT - T1 == T2', 17, 1, 2)
      IF (NV .LT. 2) CALL SETERR('DLPLMT - NV .LT. 2', 18, 2, 2)
C/
      TEMP = F .LE. 0D0
      IF (.NOT. TEMP) TEMP = F .GE. 1D0
C/6S
C     IF (TEMP) CALL SETERR(23HDLPLMT - F NOT IN (0,1), 23, 3, 2)
C     IF (DT .EQ. 0D0) CALL SETERR(16HDLPLMT - DT == 0, 16, 4, 2)
C/7S
      IF (TEMP) CALL SETERR('DLPLMT - F NOT IN (0,1)', 23, 3, 2)
      IF (DT .EQ. 0D0) CALL SETERR('DLPLMT - DT == 0', 16, 4, 2)
C/
      DTT = DT
      SGNDT = DT/DABS(DT)
      SGNT21 = (T2-T1)/DABS(T2-T1)
      DO  3 I = 1, NV
         IF (V2(I) .EQ. V1(I)) GOTO  3
         SGNV21 = (V2(I)-V1(I))/DABS(V2(I)-V1(I))
         DO  2 J = 1, NV
            IF (I .EQ. J) GOTO  2
            IF (V2(I) .NE. V2(J)) GOTO 1
               DTT = 0
               DLPLMT = DTT
               RETURN
   1        TEMP = SGNDT*SGNT21*((V2(I)-V1(I))/DABS(V2(I)-V1(I)))*((V2(I
     1         )-V2(J))/DABS(V2(I)-V2(J))) .LT. 0D0
            IF (TEMP) TEMP = SGNDT*SGNV21*(DTT*(V2(I)-V1(I))-(T2-T1)*(
     1         V2(I)-V2(J))*(F-1D0)) .GT. 0D0
            IF (TEMP) DTT = (T2-T1)*(V2(I)-V2(J))*(F-1D0)/(V2(I)-V1(I))
   2        CONTINUE
   3     CONTINUE
      DLPLMT = DTT
      RETURN
      END
