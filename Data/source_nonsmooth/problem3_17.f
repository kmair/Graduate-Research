      
      PROGRAM MAIN
      DOUBLE PRECISION fvalue
      DOUBLE PRECISION feval0

      PARAMETER      ( N   =  10 )

      INTEGER  NA, NEXT,IEXT,IERR
      INTEGER  KA 
           
      REAL*8  X(N), F, FA, FMIN, XMAX
      INTEGER IPRINT 
      INTEGER MAXFUN 
      CHARACTER *256 FREAD, NANREAD
      
      NEXT = 17
            
      CALL TIUD19(N,X,FMIN,XMAX,NEXT,IERR)
 
      OPEN  (UNIT = 2, FILE = 'input.in', STATUS = 'OLD')
      DO i = 1,N
        READ (2,*) X(i)
      ENDDO
      F = 0
      CLOSE (UNIT = 2)
      
	  CALL TFFU19(N,X,F,NEXT)

      IF ( F .GE. 1D+80 ) F = 1D+80
      
      OPEN  (UNIT = 3, FILE = 'output.out')
      WRITE  (3,900) F
      CLOSE (UNIT = 3)
      
      OPEN  (UNIT = 7, FILE = 'output.out', STATUS = 'OLD')
      READ (7,910) FREAD
      CLOSE (UNIT = 7)
      
      NANREAD = '            NAN'
      FA = 1D+80
      
      IF ( FREAD .EQ. NANREAD ) THEN
      OPEN  (UNIT = 15, FILE = 'output.out')
      WRITE  (15,900) FA
      CLOSE (UNIT = 15)
      ENDIF

  900 FORMAT (E15.6)
  910 FORMAT (15A)
      
      END