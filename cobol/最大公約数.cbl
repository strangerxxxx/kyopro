
           01 GCD-WK.
               03 GCD-RES PIC 9(18).
               03 GCD-L.
                   05 GCD-I OCCURS 2 TIMES.
                       07 GCD PIC 9(18).


       GCD-CALCULATE SECTION.
           SORT GCD-I ON DESCENDING KEY GCD.
           PERFORM UNTIL FUNCTION MOD(GCD(1), GCD(2)) = 0
               MOVE FUNCTION MOD(GCD(1), GCD(2)) TO GCD(1)
               SORT GCD-I ON DESCENDING KEY GCD
           END-PERFORM.
           MOVE GCD(2) TO GCD-RES.
       EXIT SECTION.

      *>   LCM
           01 LCM-WK.
               03 LCM-RES PIC 9(18).
               03 LCM-L.
                   05 LCM-I OCCURS 2 TIMES.
                       07 LCM PIC 9(18).

       LCM-CALCULATE SECTION.
           MOVE LCM(1) TO GCD(1)
           MOVE LCM(2) TO GCD(2)
           PERFORM GCD-CALCULATE.
           COMPUTE LCM-RES = LCM(1) * LCM(2) / GCD-RES.
       EXIT SECTION.


      *>   GCD LIST
           01 GCD-WK.
               03 GCD-RES-LIST PIC 9(18).

       GCD-CALCULATE-LIST SECTION.
           MOVE A(1) TO GCD(1)
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > N
               MOVE A(I) TO GCD(2)
               PERFORM GCD-CALCULATE
               MOVE GCD-RES TO GCD(1)
           END-PERFORM.
           MOVE GCD(1) TO GCD-RES-LIST.
       EXIT SECTION.

      *>   LCM LIST
           01 LCM-WK.
               03 LCM-RES-LIST PIC 9(18).

       LCM-CALCULATE-LIST SECTION.
           MOVE A(1) TO LCM(1)
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > N
               MOVE A(I) TO LCM(2)
               PERFORM LCM-CALCULATE
               MOVE LCM-RES TO LCM(1)
           END-PERFORM.
           MOVE LCM(1) TO LCM-RES-LIST.
       EXIT SECTION.

