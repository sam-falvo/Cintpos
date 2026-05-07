GET "mcpl.h"

GLOBAL count, spacev, spacep, spacet, pv, idv, pos, bv, iv

FUN setup : =>
   // Initialise the data structure representing
   // rotations, reflections and translations of the pieces.
   spacev := getvec 5000
   spacet := @ spacev!5000
   spacep := spacet

   pv  := getvec 11
   idv := getvec 11
   pos := getvec 11
   bv  := getvec 11
   iv  := getvec 11

   FOR i = 0 TO 11 DO
   { LET v = getvec 59
     FOR p = 0 TO 59 DO v!p := 0
     pv!i, idv!i := v, 'A'+i
     pos!i, bv!i, iv!i := 0, 0, 0 // Solution info
   }

   init(0, #0000000037)  //  * * * * *   *
   init(0, #0101010101)  //              *
                         //              *
                         //              *
                         //              *

   init(1,     #020702)  //    *
                         //  * * *
                         //    *

   init(2,   #03010101)  //    *    *    * *    * *
   init(2,   #03020202)  //    *    *      *    *
   init(2,   #01010103)  //    *    *      *    *
   init(2,   #02020203)  //  * *    * *    *    *

   init(2,       #1701)  //           *    *
   init(2,       #1710)  //     * * * *    * * * *

   init(2,       #0117)  //     * * * *    * * * *
   init(2,       #1017)  //           *    *

   init(3,     #010701)  //      *   *      * * *     *
   init(3,     #040704)  //  * * *   * * *    *       *
   init(3,     #020207)  //      *   *        *     * * *
   init(3,     #070202)

   init(4,       #0703)  //    * *   * *    * * *   * * *
   init(4,       #0706)  //  * * *   * * *    * *   * *
   init(4,       #0307)
   init(4,       #0607)
   init(4,     #030301)  //    *   *     * *   * *
   init(4,     #030302)  //  * *   * *   * *   * *
   init(4,     #010303)  //  * *   * *     *   *
   init(4,     #020303) 

   init(5,       #0316)  //  * * *       * * *
   init(5,       #1407)  //      * *   * *

   init(5,       #1603)  //      * *   * *
   init(5,       #0714)  //  * * *       * * *

   init(5,   #01030202)  //  *       *     *   *
   init(5,   #02030101)  //  *       *   * *   * *
   init(5,   #02020301)  //  * *   * *   *       *
   init(5,   #01010302)  //    *   *     *       *

   init(6,     #070101)  //    *   *       * * *   * * *
   init(6,     #070404)  //    *   *           *   *
   init(6,     #010107)  // * **   * * *       *   *
   init(6,     #040407)

   init(7,     #030604)  //  *           *     * *   * *
   init(7,     #060301)  //  * *       * *   * *       * *
   init(7,     #040603)  //    * *   * *     *           *
   init(7,     #010306)

   init(8,     #030103)  //  * *   * *   * * *   *   *
   init(8,     #030203)  //    *   *     *   *   * * *
   init(8,       #0507)  //  * *   * *
   init(8,       #0705)

   init(9,     #010704)  //  *           *   * *       * *
   init(9,     #040701)  //  * * *   * * *     *       *
   init(9,     #030206)  //      *   *         * *   * *
   init(9,     #060203)

   init(10,      #1702)  //      *       *       * * * *   * * * *
   init(10,      #1704)  //  * * * *   * * * *       *     *
   init(10,      #0217)
   init(10,      #0417)
   init(10,  #01030101)  //    *   *       *   *
   init(10,  #02030202)  //    *   *     * *   * *
   init(10,  #01010301)  //  * *   * *     *   *
   init(10,  #02020302)  //    *   *       *   *

// the comments eliminate reflectively different solutions
   init(11,    #010702)  //    *       *         *   * 
// init(11,    #040702)  //  * * *   * * *   * * *   * * *
// init(11,    #020701)  //      *   *         *       *
// init(11,    #020704)
   init(11,    #030602)  //    *       *       * *   * *
// init(11,    #060302)  //  * *       * *   * *       * *
// init(11,    #020603)  //    * *   * *       *       *
// init(11,    #020306)


FUN freespace : =>
   FOR i = 0 TO 11 DO freevec(pv!i)

   freevec pv
   freevec idv
   freevec pos
   freevec bv
   freevec iv
   freevec spacev


FUN mk2 : x, y => !---spacep := y
                  !---spacep := x
                  spacep


FUN init : piece, bits =>
  LET word=bits, height=0
  WHILE word DO { word >>:= 6; height++ }

  LET pat=bits, orig=0
  UNTIL pat&1 DO { pat >>:= 1; orig++ }

  LET v = pv!piece
  FOR p = orig TO orig + 6*(10-height) BY 6 DO
  { LET q   = p
    word   := bits

    { v!q := mk2(v!q, pat)
      IF word & #4040404040 BREAK // can't move left any more
      word <<:= 1                 // move piece left one place
      q++
    } REPEAT
  }

FUN try
: <0, ?,     ? => writef("Solution %d:\n", ++count); pr()

: n,  p, board =>
  WHILE board&1 DO { p++; board >>:= 1 }
  FOR i = 0 TO n DO
  { LET pvi=pv!i, id=idv!i

    MATCH pvi!p
    : 0    => LOOP
    : list => pv!i, idv!i := pv!n, idv!n

              { MATCH list : [next, bits] =>
                  UNLESS bits & board DO
                  { pos!n, bv!n, iv!n := p, bits, id
                    try(n-1, p, bits+board)
                  }
                  list := next
              } REPEATWHILE list
    .
    pv!i, idv!i, bv!n := pvi, id, 0
  }


FUN start : =>
  setup()
  count := 0
  try(11, 0, 0)
  writef("\nNumber of solutions is %d\n", count)
  freespace()
  RETURN 0

FUN pr : =>
  LET v = VEC 59
  FOR i = 0 TO 59 DO v!i := '-'
  FOR i = 0 TO 11 DO { LET p=pos!i, bits=bv!i, id=iv!i
                       WHILE bits DO
                       { IF bits&1 DO v!p := id
                         bits >>:= 1
                         p++
                       }
                     }
  FOR row = 0 TO 9 DO
  { FOR p = 6*row+5 TO 6*row BY -1 DO writef(" %c", v!p)
    newline()
  }
  newline()







