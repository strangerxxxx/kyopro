
           01 POW-WK.
               03 POW-A BINARY-DOUBLE SIGNED VALUE 0.
               03 POW-N BINARY-DOUBLE SIGNED VALUE 0.
               03 POW-MOD BINARY-DOUBLE SIGNED VALUE 998244353.
               03 POW-RES BINARY-DOUBLE SIGNED VALUE 0.
               03 POW-X BINARY-DOUBLE SIGNED VALUE 0.
               03 POW-Y BINARY-DOUBLE SIGNED VALUE 0.


      *>   COMPUTE POW-A ** POW-N MOD POW-MOD
       POWMOD SECTION.
           MOVE 1 TO POW-RES.
           MOVE POW-N TO POW-Y.
           PERFORM UNTIL POW-Y <= 0
               MOVE 1 TO POW-X
               COMPUTE POW-X = POW-Y / 2
               IF POW-Y - 2 * POW-X = 1
                   MULTIPLY POW-A BY POW-RES
                   COMPUTE POW-RES = FUNCTION MOD(POW-RES, POW-MOD)
               END-IF
               MULTIPLY POW-A BY POW-A
               COMPUTE POW-A = FUNCTION MOD(POW-A, POW-MOD)
               DIVIDE 2 INTO POW-Y
           END-PERFORM
       EXIT SECTION.
