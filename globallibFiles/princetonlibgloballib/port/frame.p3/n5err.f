      SUBROUTINE N5ERR(MESSG, NMESSG, NERR, IOPT)
      INTEGER NMESSG, NERR, IOPT
C/6S
C     INTEGER MESSG(1)
C/7S
      CHARACTER*1 MESSG(NMESSG)
C/
C  N5ERR IS A PROCEDURE USED TO REDEFINE AN ERROR STATE.
      CALL ERROFF
      CALL SETERR(MESSG, NMESSG, NERR, IOPT)
      RETURN
      END
