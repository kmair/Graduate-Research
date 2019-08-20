      SUBROUTINE DS42B2(N,I,JI,A,Z,TEMP,SAVE)
      INTEGER N,I,JI
      DOUBLE PRECISION A(N),Z(N),TEMP,SAVE
      INTEGER JIP1,NMI,JNMI,J,JIPNMI
      DOUBLE PRECISION DENOM
               JIP1 = JI+1
               IP1=I+1
               NMI = N-I
               JNMI = JIP1+NMI
               DENOM = A(JNMI)*A(JI)/A(JIP1)-A(JIP1)
               Z(IP1) = (SAVE*A(JI)/A(JIP1)-TEMP)/DENOM
               Z(I) = (SAVE-Z(IP1)*A(JNMI))/A(JIP1)
               IP2 = I+2
               JI = JI+2
               JIPNMI = JI+NMI
               IF (IP2 .GT. N) GOTO 11
                  DO  10 J = IP2, N
                     Z(J) = Z(J)+A(JI)*TEMP+A(JIPNMI)*SAVE
                     JI = JI+1
                     JIPNMI = JIPNMI+1
  10                 CONTINUE
  11           I = IP2
               JI = JI+NMI
      RETURN
      END
