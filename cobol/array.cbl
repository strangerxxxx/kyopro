           01 WK.
               03 I BINARY-DOUBLE UNSIGNED VALUE 0.
               03 N BINARY-DOUBLE UNSIGNED VALUE 0.
           01 ARRAY-WK.
               03 ARRAY-N BINARY-DOUBLE UNSIGNED VALUE 0.
               03 ARRAY-X BINARY-DOUBLE UNSIGNED VALUE 0.
               03 ARRAY-POPPED BINARY-DOUBLE UNSIGNED VALUE 0.
               03 ARRAY-LST.
                   05 ARRAY-I OCCURS 0 TO 200000 TIMES
                                       DEPENDING ON ARRAY-N.
                       07 ARRAY BINARY-DOUBLE UNSIGNED VALUE 0.

       ARRAY-APPEND SECTION.
           ADD 1 TO ARRAY-N.
           MOVE ARRAY-X TO ARRAY(ARRAY-N).
       EXIT SECTION.
       ARRAY-POP SECTION.
           MOVE ARRAY(ARRAY-N) TO ARRAY-POPPED.
           SUBTRACT 1 FROM ARRAY-N.
       EXIT SECTION.

      *>   DEQUE
           01 WK.
               03 I BINARY-DOUBLE UNSIGNED VALUE 0.
               03 N BINARY-DOUBLE UNSIGNED VALUE 0.
           01 ARRAY-WK.
               03 ARRAY-N BINARY-DOUBLE UNSIGNED VALUE 0.
               03 ARRAY-L BINARY-DOUBLE UNSIGNED VALUE 1.
               03 ARRAY-R BINARY-DOUBLE UNSIGNED VALUE 0.
               03 ARRAY-X BINARY-DOUBLE UNSIGNED VALUE 0.
               03 ARRAY-POPPED BINARY-DOUBLE UNSIGNED VALUE 0.
               03 ARRAY-LST.
                   05 ARRAY-I OCCURS 200000 TIMES.
                       07 ARRAY BINARY-DOUBLE UNSIGNED VALUE 0.

       ARRAY-APPEND SECTION.
           ADD 1 TO ARRAY-N.
           ADD 1 TO ARRAY-R.
           MOVE ARRAY-X TO ARRAY(ARRAY-R).
       EXIT SECTION.
       ARRAY-POP SECTION.
           MOVE ARRAY(ARRAY-R) TO ARRAY-POPPED.
           SUBTRACT 1 FROM ARRAY-R.
           SUBTRACT 1 FROM ARRAY-N.
       EXIT SECTION.
       ARRAY-POPLEFT SECTION.
           MOVE ARRAY(ARRAY-L) TO ARRAY-POPPED.
           ADD 1 TO ARRAY-L.
           SUBTRACT 1 FROM ARRAY-N.
       EXIT SECTION.
