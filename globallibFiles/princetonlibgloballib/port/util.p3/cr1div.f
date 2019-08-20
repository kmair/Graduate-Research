      SUBROUTINE CR1DIV(AR,AI,BR,BI,CR,CI)
C
C COMPLEX DIVISION C = A/B, AVOIDING OVERFLOW.
C
      REAL AR,AI,BR,BI,CR,CI,R,D,R1MACH,ABS
C
      IF (BR .NE. 0.0E0  .OR. BI .NE. 0.0E0) GO TO 10
C
C DIVISION BY ZERO, C = INFINITY -
          CR = R1MACH(2)
          CI = CR
          RETURN
C
   10 IF (ABS(BR) .GE. ABS(BI)) GO TO 20
          R = BR/BI
          D = BI+R*BR
          CR = (AR*R+AI)/D
          CI = (AI*R-AR)/D
          RETURN
C
   20 R = BI/BR
      D = BR+R*BI
      CR = (AR+AI*R)/D
      CI = (AI-AR*R)/D
      RETURN
      END
