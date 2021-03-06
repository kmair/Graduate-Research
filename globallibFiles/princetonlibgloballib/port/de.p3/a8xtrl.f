      LOGICAL FUNCTION A8XTRL(E2,E,NX,M,KMAX,POW,LOGLO)
C
C  TO RETURN POW(J) TIMES THE LOGARITHM OF THE RATIO OF THE DESIRED
C  TO THE ATTAINED ERROR, FOR EACH ELEMENT IN THE LOZENGE.
C
C  A8XTRL = .TRUE. IF NOT SUCCESSFUL.
C  A8XTRL = .FALSE. IF SUCCESSFUL.
C
      REAL E2(NX,KMAX),E(NX),POW(KMAX),LOGLO
C
      REAL LOGE,V,R1MACH
C
      A8XTRL=.FALSE.
      JHI=MIN0(M-1,KMAX)
C
      DO 20 I=1,NX
C
         IF (E(I).LE.0.0E0) GO TO 30
C
         LOGE=ALOG(E(I))
C
         DO 10 J=1,JHI
C
            V=-ALOG(R1MACH(4))-4.6E0
            IF (E2(I,J).NE.0.0E0) V=LOGE-ALOG(ABS(E2(I,J)))
 10         E2(I,J)=POW(J)*V
C
 20      CONTINUE
C
      GO TO 40
C
C ... HERE FOR A NON-POSITIVE ERROR REQUEST.
C
C/6S
C30   CALL SETERR(36H SXTRP - E(I).LE.0 RETURNED BY ERROR,36,17,1)
C/7S
 30   CALL SETERR(' SXTRP - E(I).LE.0 RETURNED BY ERROR',36,17,1)
C/
      A8XTRL=.TRUE.
C
 40   RETURN
C
      END
