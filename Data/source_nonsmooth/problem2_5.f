      PROGRAM MAIN
      DOUBLE PRECISION fvalue
      DOUBLE PRECISION feval0

      PARAMETER      ( N   =  4 )

      INTEGER  NA, NEXT,IEXT,IERR
      INTEGER  OBJUPDATE
           
      REAL*8  X(N), F, FA, F1, FMIN, XMAX
      INTEGER IPRINT 
      INTEGER MAXFUN 
      CHARACTER *256 FREAD, NANREAD
      
      NA = 4
      NEXT = 5
      OBJUPDATE = 0
      IEXT = -1
      
      CALL TIUD06(N,NA,X,FMIN,XMAX,NEXT,IEXT,IERR)
      
      
      OPEN  (UNIT = 2, FILE = 'input.in', STATUS = 'OLD')
      DO i = 1,N
        READ (2,*) X(i)
      ENDDO
      F = -1D+80
      CLOSE (UNIT = 2)

      DO i = 1,NA            
		CALL TAFU06(n,i,X,FA,NEXT)
		IF ( FA .GE. F ) THEN
			F = FA
			OBJUPDATE = 1
		ENDIF		
	  ENDDO

      IF ( F .GE. 1D+80 ) F = 1D+80
      IF ( OBJUPDATE .EQ. 0 ) F = 1D+80
      
      OPEN  (UNIT = 3, FILE = 'output.out')
      WRITE  (3,900) F
      CLOSE (UNIT = 3)
            
      OPEN  (UNIT = 7, FILE = 'output.out', STATUS = 'OLD')
      READ (7,910) FREAD
      CLOSE (UNIT = 7)
      
      NANREAD = '            NAN'
      
      IF ( FREAD .EQ. NANREAD ) THEN
      OPEN  (UNIT = 15, FILE = 'output.out')
      WRITE  (15,900) 1D+80
      CLOSE (UNIT = 15)
      ENDIF

  900 FORMAT (E15.6)
  910 FORMAT (15A)
      
      END