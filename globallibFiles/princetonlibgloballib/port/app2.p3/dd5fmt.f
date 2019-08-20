      SUBROUTINE DD5FMT(INPROD, A, MM, NN, IROW, X, DINPRD)
      INTEGER NN
      INTEGER MM, IROW
      LOGICAL INPROD
      DOUBLE PRECISION A(1), X(NN), DINPRD
      COMMON /DD5FCM/ NPTS, M, N, I1, I2, I3, I4
      INTEGER NPTS, M, N, I1, I2, I3
      INTEGER I4
      INTEGER IRM1, IRM2, IRM3, ZPTR, FNPTR, QZPTR
      INTEGER JP, MAXMN, J, MAX0
      DOUBLE PRECISION FCT, FDELK, DELK, Z, FN, QZ
      DOUBLE PRECISION DTCHBP
C   DD5FMT HANDLES REFERENCES BY THE LP ROUTINE TO
C   THE MATRIX FOR THE LINEAR PROGRAMMING SUBPROBLEM.
      CALL ENTER(1)
C/6S
C     IF (MM .NE. I4 .OR. NN .NE. M+N+3) CALL SETERR(
C    1   26HDD5FMT - INVALID DIMENSION, 26, 1, 2)
C     IF (IROW .LT. 0 .OR. MM .LT. IROW) CALL SETERR(
C    1   22HDD5FMT - INVALID INDEX, 22, 2, 2)
C/7S
      IF (MM .NE. I4 .OR. NN .NE. M+N+3) CALL SETERR(
     1   'DD5FMT - INVALID DIMENSION', 26, 1, 2)
      IF (IROW .LT. 0 .OR. MM .LT. IROW) CALL SETERR(
     1   'DD5FMT - INVALID INDEX', 22, 2, 2)
C/
      IRM1 = IROW-I1
      IRM2 = IROW-I2
      IRM3 = IROW-I3
      IF ((.NOT. INPROD) .OR. I2 .GE. IROW) GOTO 3
         IF (I3 .GE. IROW) GOTO 1
            DINPRD = -X(IRM3)
            GOTO  2
   1        DINPRD = X(IRM2)
   2     CONTINUE
         GOTO  18
   3     IF (I2 .GE. IROW) GOTO 6
            CALL SETD(NN, 0.0D0, X)
            IF (I3 .GE. IROW) GOTO 4
               X(IRM3) = -1.0D0
               GOTO  5
   4           X(IRM2) = 1.0D0
   5        CONTINUE
            GOTO  17
   6        IF (I1 .GE. IROW) GOTO 7
               FCT = -1.0D0
               ZPTR = IRM1
               GOTO  8
   7           FCT = 1.0D0
               ZPTR = IROW
   8        Z = A(ZPTR)
            FNPTR = ZPTR+NPTS
            FN = A(FNPTR)
            QZPTR = FNPTR+NPTS
            QZ = A(QZPTR)
            DELK = A(3*NPTS+1)
            FDELK = FCT*FN+DELK
            IF (.NOT. INPROD) GOTO 9
               DINPRD = FDELK*DTCHBP(N, X, Z, A(1), A(NPTS))-FCT*DTCHBP(
     1            M, X(N+2), Z, A(1), A(NPTS))+QZ*X(NN)
               GOTO  16
   9           MAXMN = MAX0(M, N)
               CALL DT5COF(Z, A(1), A(NPTS), MAXMN, X)
               J = M+1
                  GOTO  11
  10              J = J-1
  11              IF (1 .GT. J) GOTO  12
                  JP = J+N+1
                  X(JP) = (-FCT)*X(J)
                  GOTO  10
  12           J = 1
                  GOTO  14
  13              J = J+1
  14              IF (J .GT. N+1) GOTO  15
                  X(J) = FDELK*X(J)
                  GOTO  13
  15           X(NN) = QZ
  16        CONTINUE
  17  CONTINUE
  18  CALL LEAVE
      RETURN
      END
