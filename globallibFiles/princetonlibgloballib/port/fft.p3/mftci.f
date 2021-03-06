         SUBROUTINE MFTCI(N,IFX,T)
C
C   INITIALIZATION ROUTINE FOR MFTCC (MULTIPLE FOURIER TRANSFORM,
C   COMPLEX TO COMPLEX DATA). REAL ARRAY T() GETS N COMPLEX
C   EXPONENTIAL FACTORS
C
C                T(1,J) = COS(2*PI*(J-1)/N)
C                T(2,J) = SIN(2*PI*(J-1)/N)
C
C   FOR J = 1,N. THE INTEGER ARRAY IFX() GETS A FACTORIZATION
C   OF TRANSFORM DIMENSION N = 2**P*3**Q*5**RR*... INTO FACTORS OF
C   2,3,5, AND GENERAL PRIME FACTORS (GREATER THAN 5). THE CONTENTS
C   OF IFX() ON EXIT ARE
C
C                IFX(1) = N
C                IFX(2) = M   THE NUMBER OF FACTORS
C                IFX(3) THROUGH IFX(M+2) ARE THE FACTORS
C
C   FOR EXAMPLE IF N = 30, THE CONTENTS OF IFX() ARE IFX(1) = 30,
C   IFX(2) = 3, IFX(3) = 2, IFX(4) = 3, AND IFX(5) = 5.
C
C   ERROR CONDITIONS
C
C       (1) IF N.LE.0, THIS ROUTINE SIMPLY RETURNS, BUT GENERATES A
C           RECOVERABLE ERROR.
C
         REAL T(2,1)
         REAL ANG,ARG,PI2
         REAL C36,S36,C72,S72,S60
         INTEGER IFX(25)
         INTEGER I,N
C
         COMMON /M55FT/C36,S36,C72,S72,S60
C
C  INPUT PARAMETER CHECKS
C
C/6S
C        IF(N.LE.0)CALL SETERR(18H  MFTCI - N .LE. 0,18,1,1)
C/7S
         IF(N.LE.0)CALL SETERR('  MFTCI - N .LE. 0',18,1,1)
C/
C
C  COMPUTE COMMON FACTORS
C
         PI2  = 8.0E0*ATAN(1.0E0)
         ANG  = 0.1E0*PI2
         C36  = COS(ANG)
         S36  = SQRT(1.0E0 - C36*C36)
         ANG  = 0.2E0*PI2
         C72  = COS(ANG)
         S72  = SQRT(1.0E0 - C72*C72)
         ANG  = PI2/6.0E0
         S60  = SIN(ANG)
C
C   FACTOR N
C
         CALL M66FT(N,IFX)
C
C   COMPUTE EXPONENTIAL FACTORS
C
         ANG  = PI2/FLOAT(N)
         DO 1 I = 1,N
            ARG  = ANG*FLOAT(I-1)
            T(1,I) = COS(ARG)
            T(2,I) = SIN(ARG)
 1       CONTINUE
         RETURN
         END
