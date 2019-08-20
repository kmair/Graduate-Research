      SUBROUTINE R7POLY(NN,SR,SI,PR,PI,QR,QI,PVR,PVI)
C EVALUATES A POLYNOMIAL  P  AT  S  BY THE HORNER RECURRENCE
C PLACING THE PARTIAL SUMS IN Q AND THE COMPUTED VALUE IN PV.
      REAL PR(1),PI(1),QR(1),QI(1),
     1    SR,SI,PVR,PVI,T
      QR(1) = PR(1)
      QI(1) = PI(1)
      PVR = QR(1)
      PVI = QI(1)
      DO 10 I = 2,NN
          T = PVR*SR-PVI*SI+PR(I)
          PVI = PVR*SI+PVI*SR+PI(I)
          PVR = T
          QR(I) = PVR
          QI(I) = PVI
   10 CONTINUE
      RETURN
      END