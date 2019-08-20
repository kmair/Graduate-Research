      SUBROUTINE D9OSTX(XSTATS, IFLAG)
      INTEGER XSTATS(8), IFLAG
      INTEGER STATS(8)
      LOGICAL INPOST
      DATA STATS(1)/0/
      DATA STATS(2)/0/
      DATA STATS(3)/0/
      DATA STATS(4)/0/
      DATA STATS(5)/0/
      DATA STATS(6)/0/
      DATA STATS(7)/0/
      DATA STATS(8)/0/
      DATA INPOST/.FALSE./
C INTERNAL SAVING OF STATISTICS FOR POST.
C FOR IFLAG = 0, THE STATS ARE SIMPLY REPORTED.
C FOR IFLAG > 0, IT ENTERS POST.
C FOR IFLAG < 0, IT EXITS POST.
      IF (IFLAG .NE. 0) GOTO 1
         IF (.NOT. INPOST) CALL MOVEFI(8, STATS, XSTATS)
         GOTO  4
   1     IF (IFLAG .LE. 0) GOTO 2
            INPOST = .TRUE.
            CALL SETI(8, 0, STATS)
            CALL MOVEFI(8, STATS, XSTATS)
            GOTO  3
   2        INPOST = .FALSE.
C IFLAG < 0.
            CALL MOVEFI(8, XSTATS, STATS)
   3  CONTINUE
   4  RETURN
      END