      SUBROUTINE RANBYT(UNI,IBYTE)
      DIMENSION IBYTE(4)
      DATA ICSEED/0/, ITSEED/0/, IFCN/2/
C
C  UNI IS RETURNED AS A SINGLE UNIFORM RANDOM VARIATE IN UNI.
C
C  IBYTE IS RETURNED WITH THE BITS OF UNI, 8 BITS PER WORD.
C  UNI=(IBYTE(1)*256**3+IBYTE(2)*256**2+IBYTE(3)*256+IBYTE(4))/2**32
C
C  IFCN = 2 IMPLIES THAT ICSEED AND ITSEED ARE IGNORED.
C
      UNI=R1UNIF(ICSEED,ITSEED,IBYTE,IFCN)
      RETURN
      END
