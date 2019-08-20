C$SEQ MLLR 0 20
C$TEST MLLR
C***********************************************************************
C
C  EXAMPLE OF USE OF THE PORT PROGRAM MULLR
C
C***********************************************************************
      INTEGER ITER, MAXITR, IWRITE
      REAL EPSF, EPSZ
      COMPLEX MULLR, TESTF, Z1, Z2, Z3, ZANS
      COMPLEX CEXP, CSIN
      EXTERNAL TESTF
C
C SET UP THE INITIAL GUESSES AND TOLERANCES
C
      Z1 = CMPLX(2.0, 1.0)
      Z2 = CMPLX(5.0, 4.0)
      Z3 = CMPLX(3.0, 2.0)
C
      EPSZ = .00001
      EPSF = .000001
      MAXITR = 50
C
      ZANS = MULLR(TESTF, Z1, Z2, Z3, EPSZ, EPSF, MAXITR, ITER)
C
C WRITE IWRITE THE ANSWER AND THE NUMBER OF ITERATIONS
C
      IWRITE = I1MACH(2)
      WRITE (IWRITE, 999) ZANS, ITER
 999    FORMAT(1H ,12HTHE ZERO IS ,2F11.8, 3X,
     1     I3,21H ITERATIONS WERE USED)
C
C
      STOP
      END
      COMPLEX FUNCTION TESTF(Z)
C
      COMPLEX Z
C
       TESTF = Z*CEXP(Z) - CSIN(Z)
C
      RETURN
      END
