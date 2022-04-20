
           01 PRIME-WK.
               03 PRIME-RES BINARY-CHAR SIGNED VALUE 0.
               03 PRIME-N BINARY-DOUBLE SIGNED VALUE 0.
               03 PRIME-I BINARY-DOUBLE SIGNED VALUE 0.


       IS-PRIME SECTION.
           MOVE 1 TO PRIME-RES.
           PERFORM VARYING PRIME-I FROM 2 BY 1 
                                   UNTIL PRIME-I ** 2 > PRIME-N
               IF FUNCTION MOD(PRIME-N, PRIME-I) = 0
                   MOVE 0 TO PRIME-RES
                   EXIT PERFORM
               END-IF
           END-PERFORM
       EXIT SECTION.

      *>   PRIME LIST
           01 PRIME-WK.
               03 PRIME-II BINARY-DOUBLE SIGNED VALUE 0.
               03 PRIME-IJ BINARY-DOUBLE SIGNED VALUE 0.
               03 PRIME-IK BINARY-DOUBLE SIGNED VALUE 0.
               03 PN BINARY-DOUBLE SIGNED VALUE 0.
               03 PRIME-LL.
                   05 PRIME-LI OCCURS 0 TO 200000 TIMES DEPENDING ON PN.
                       07 PRIME-L BINARY-CHAR SIGNED VALUE 1.

       PRIME-LIST SECTION.
           MOVE 0 TO PRIME-L(1)
           PERFORM VARYING PRIME-II FROM 2 BY 1
                                    UNTIL PRIME-II ** 2 > PN
               IF PRIME-L(PRIME-II) = 1
                   COMPUTE PRIME-IK = PRIME-II * 2
                   PERFORM VARYING PRIME-IJ FROM PRIME-IK BY PRIME-II
                                                UNTIL PRIME-IJ > PN
                       MOVE 0 TO PRIME-L(PRIME-IJ)
                   END-PERFORM
               END-IF
           END-PERFORM.
       EXIT SECTION.
