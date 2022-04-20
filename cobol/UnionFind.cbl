
           01 UF-WK.
               03 UF-P BINARY-LONG SIGNED VALUE 0.
               03 UF-TMP BINARY-LONG SIGNED VALUE 0.
               03 UF-FINDX BINARY-LONG SIGNED VALUE 0.
               03 UF-UNIONX BINARY-LONG SIGNED VALUE 0.
               03 UF-UNIONY BINARY-LONG SIGNED VALUE 0.
               03 UF-SAMEX BINARY-LONG SIGNED VALUE 0.
               03 UF-SAMEY BINARY-LONG SIGNED VALUE 0.
               03 UF-SIZEX BINARY-LONG SIGNED VALUE 0.
               03 UF-RES BINARY-LONG SIGNED VALUE 0.
               03 PARENT-L.
                   05 PARENT-I OCCURS 1 TO 200000 TIMES DEPENDING ON N.
                       07 UF-PARENT BINARY-LONG SIGNED VALUE -1.



       UF-FIND SECTION.
           IF UF-PARENT(UF-FINDX) < 0 THEN
               MOVE UF-FINDX TO UF-RES
           ELSE
               MOVE UF-FINDX TO UF-P
               PERFORM UNTIL UF-PARENT(UF-P) < 0
                   MOVE UF-PARENT(UF-P) TO UF-P
               END-PERFORM
               PERFORM UNTIL UF-PARENT(UF-FINDX) < 0
                   MOVE UF-PARENT(UF-FINDX) TO UF-TMP
                   MOVE UF-P TO UF-PARENT(UF-FINDX)
                   MOVE UF-TMP TO UF-FINDX
               END-PERFORM
               MOVE UF-P TO UF-RES
           END-IF.
       EXIT SECTION.
       UF-UNION SECTION.
           MOVE UF-UNIONX TO UF-FINDX.
           PERFORM UF-FIND.
           MOVE UF-RES TO UF-UNIONX
           MOVE UF-UNIONY TO UF-FINDX.
           PERFORM UF-FIND.
           MOVE UF-RES TO UF-UNIONY.
           IF UF-UNIONX = UF-UNIONY THEN
               MOVE 0 TO UF-RES
           ELSE
               IF UF-PARENT(UF-UNIONX) > UF-PARENT(UF-UNIONY) THEN
                   MOVE UF-UNIONX TO UF-TMP
                   MOVE UF-UNIONY TO UF-UNIONX
                   MOVE UF-TMP TO UF-UNIONY
               END-IF
               ADD UF-PARENT(UF-UNIONY) TO UF-PARENT(UF-UNIONX)
               MOVE UF-UNIONX TO UF-PARENT(UF-UNIONY)
               MOVE 1 TO UF-RES
           END-IF.
       EXIT SECTION.
       UF-SAME SECTION.
           MOVE UF-SAMEX TO UF-FINDX.
           PERFORM UF-FIND.
           MOVE UF-RES TO UF-SAMEX.
           MOVE UF-SAMEY TO UF-FINDX.
           PERFORM UF-FIND.
           MOVE UF-RES TO UF-SAMEY.
           IF UF-SAMEX = UF-SAMEY THEN
               MOVE 1 TO UF-RES
           ELSE
               MOVE 0 TO UF-RES
           END-IF.
       EXIT SECTION.
       UF-SIZE SECTION.
           MOVE UF-SIZEX TO UF-FINDX.
           PERFORM UF-FIND.
           COMPUTE UF-RES = -1 * UF-PARENT(UF-RES).
       EXIT SECTION.
       UF-GROUPS SECTION.
           MOVE 0 TO UF-RES.
           PERFORM VARYING UF-P FROM 1 BY 1 UNTIL UF-P > N
               IF UF-PARENT(UF-P) < 0
                   ADD 1 TO UF-RES
               END-IF
           END-PERFORM.
       EXIT SECTION.
