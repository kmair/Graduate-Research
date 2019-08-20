       SUBROUTINE C4HBS(N,I,A,Z,JK,INTER)
       INTEGER N,I,JK
       INTEGER INTER(N)
       INTEGER IP1,K,KK,KEND,ICH,IKENT
       COMPLEX A(N),Z(N)
       COMPLEX SAVE
  16  IF (I .LE. 0) RETURN
         KEND = 1
         IF (INTER(I) .LE. 0) KEND = 2
         IP1 = I+1
         DO  18 KK = 1, KEND
            K = IP1-KK
            SAVE = Z(K)
            DO  17 J = IP1, N
               SAVE = SAVE+A(JK)*Z(J)
               JK = JK+1
  17           CONTINUE
            Z(K) = SAVE
            JK = JK-2*(N-K)-1
  18        CONTINUE
         JK = JK-1
         SAVE = Z(I)
         IKENT = IP1-KEND
         ICH = INTER(IKENT)
         Z(I) = Z(ICH)
         Z(ICH) = SAVE
         I = I-KEND
         GOTO  16
         END
