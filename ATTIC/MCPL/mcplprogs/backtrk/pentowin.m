GET "mcpl.h"

GLOBAL
  count:200, 
  stackv, stackp, stackt, p0, q0, p1, q1

MANIFEST
  P1=1,    P2=1<<1, P3=1<<2, P4 =1<<3, P5 =1<<4,  P6 =1<<5,
  P7=1<<6, P8=1<<7, P9=1<<8, P10=1<<9, P11=1<<10, P12=1<<11

FUN setup : =>
   // Initialise the set of possible piece placements on
   // the 8x8 board allowing for all rotations, reflections
   // and translations. The generate the set of truly
   // distinct first moves.

   stackv := getvec 500000
   stackt := @ stackv!500000
   stackp := stackv

   p1 := stackp
   mappieces addallrots
   q1 := stackp
   writef("\nThere are %4d possible first moves", (q1-p1)/(3*Bpw))

   p0 := stackp
   mappieces addminrot
   q0 := stackp
   writef("\nof which  %4d are truly distinct\n", (q0-p0)/(3*Bpw))

FUN mappieces : f =>
   hashtab := getvec Hashtabsize
   FOR i = 0 TO Hashtabsize DO hashtab!i := 0

   init(f,     #x1F, P1)  //  * * * * *     *
   init(f, #x020702, P2)  //              * * *
                          //                *

   init(f,   #x010F, P3)  //  * * * *         *
   init(f, #x010701, P4)  //        *     * * *
                          //                  *

   init(f,   #x0703, P5)  //    * *       * * *
   init(f,   #x030E, P6)  //  * * *           * *

   init(f, #x070101, P7)  //      *       *
   init(f, #x030604, P8)  //      *       * *
                          //  * * *         * *

   init(f,   #x0507, P9)  //  * * *       *
   init(f, #x010704, P10) //  *   *       * * *
                          //                  *

   init(f,   #x0F02, P11) //      *          *
   init(f, #x010702, P12) //  * * * *      * * *
                          //                   *

   freevec hashtab

FUN freestack : => freevec stackv

FUN addminrot : w1, w0, piece =>
  LET mw1=w1, mw0=w0

  rotate(@w1)
  IF w1<mw1 OR w1=mw1 AND w0<mw0 DO mw1, mw0 := w1, w0
  rotate(@w1)
  IF w1<mw1 OR w1=mw1 AND w0<mw0 DO mw1, mw0 := w1, w0
  rotate(@w1)
  IF w1<mw1 OR w1=mw1 AND w0<mw0 DO mw1, mw0 := w1, w0

  reflect(@w1)

  IF w1<mw1 OR w1=mw1 AND w0<mw0 DO mw1, mw0 := w1, w0
  rotate(@w1)
  IF w1<mw1 OR w1=mw1 AND w0<mw0 DO mw1, mw0 := w1, w0
  rotate(@w1)
  IF w1<mw1 OR w1=mw1 AND w0<mw0 DO mw1, mw0 := w1, w0
  rotate(@w1)
  IF w1<mw1 OR w1=mw1 AND w0<mw0 DO mw1, mw0 := w1, w0

  addpos(mw1, mw0, piece) 

FUN addallrots : w1, w0, piece => 
  addpos(w1, w0, piece)
  rotate(@w1)
  addpos(w1, w0, piece)
  rotate(@w1)
  addpos(w1, w0, piece)
  rotate(@w1)
  addpos(w1, w0, piece)

  reflect(@w1)

  addpos(w1, w0, piece)
  rotate(@w1)
  addpos(w1, w0, piece)
  rotate(@w1)
  addpos(w1, w0, piece)
  rotate(@w1)
  addpos(w1, w0, piece)

FUN init : f, word0, piece =>
  LET word1 = 0

  { LET w1=word1, w0=word0 

    { f(w1, w0, piece)
      IF (w0|w1) & #x80808080 BREAK // can't move left any more
      w1, w0 <<:= 1, 1              // move piece left one place
    } REPEAT
    
    IF word1 & #xFF000000 RETURN
    word1 := word1<<8 + word0>>24
    word0 <<:= 8
  } REPEAT

STATIC   hashtab

MANIFEST Hashtabsize = 4001 // Large enough for 2308 entries

FUN addpos : w1, w0, piece =>
  LET hashval = ABS((w1+1)*(w0+3)) MOD Hashtabsize

  { LET p = hashtab!hashval

    UNLESS p DO { hashtab!hashval := stackp // Make new entry
                  !stackp+++ := w1
                  !stackp+++ := w0
                  !stackp+++ := piece
                  RETURN
                }

    IF p!0=w1 AND p!1=w0 RETURN             // Match found

    hashval++
    IF hashval>Hashtabsize DO hashval := 0
  } REPEAT


FUN reflect : [w1, w0] =>
  w0 := (w0&#x01010101)<<7 | (w0&#x80808080)>>7 |
        (w0&#x02020202)<<5 | (w0&#x40404040)>>5 |
        (w0&#x04040404)<<3 | (w0&#x20202020)>>3 |
        (w0&#x08080808)<<1 | (w0&#x10101010)>>1

  w1 := (w1&#x01010101)<<7 | (w1&#x80808080)>>7 |
        (w1&#x02020202)<<5 | (w1&#x40404040)>>5 |
        (w1&#x04040404)<<3 | (w1&#x20202020)>>3 |
        (w1&#x08080808)<<1 | (w1&#x10101010)>>1

FUN rotate : [w1, w0] => 
  LET a = (w0&#x0F0F0F0F)<<4 | w1&#x0F0F0F0F
  LET b = (w1&#xF0F0F0F0)>>4 | w0&#xF0F0F0F0

  a  := (a & #X00003333)<<2 | (a & #X0000CCCC)<<16 |
        (a & #XCCCC0000)>>2 | (a & #X33330000)>>16

  b  := (b & #X00003333)<<2 | (b & #X0000CCCC)<<16 |
        (b & #XCCCC0000)>>2 | (b & #X33330000)>>16

  w0 := (a & #X00550055)<<1 | (a & #X00AA00AA)<<8  |
        (a & #XAA00AA00)>>1 | (a & #X55005500)>>8

  w1 := (b & #X00550055)<<1 | (b & #X00AA00AA)<<8  |
        (b & #XAA00AA00)>>1 | (b & #X55005500)>>8

FUN bits : 0 => 0
         : w => 1 + bits(w&(w-1))

STATIC path = VEC 12,
        w1v = VEC 12,
        w0v = VEC 12,
        mvn = VEC 12,
        mvt = VEC 12

FUN start : =>
  LET argv = VEC 50
  LET stdout = output()

  IF rdargs(",,,,,,,,,,,,TO/K", argv, 50)=0 DO
  { writef "Bad arguments\n"
    RETURN 0
  }

  FOR i = 0 TO 11 DO path!(i+1) := argv!i -> str2numb(argv!i), -1

  UNLESS argv!12=0 DO selectoutput(findoutput(argv!12))

  setup()

  TEST try76(1, p0, q0, p1, q1)
  THEN writes "\nFirst player can force a win\n"
  ELSE writes "\nFirst player cannot force a win\n"

  freestack()

  UNLESS argv!12=0 DO endwrite()

  RETURN 0

FUN try76 : n, p, q, np, nq =>
  LET s=stackp, t=p, lim=q

  UNLESS path!n<0 DO { t := @p!(3*(path!n-1)); IF t<lim DO lim := t+1 }

  WHILE t < lim DO
  { LET w1    = !t+++            // Choose a move
    LET w0    = !t+++
    LET piece = !t+++

    w1v!n, w0v!n := w1, w0       // Save the move for printing
    mvn!n, mvt!n := (t-p)/(3*Bpw), (q-p)/(3*Bpw)

    IF path!n>=0 AND path!(n+1)<0 DO
    { writef "\nConsidering board position:"
      FOR i = 1 TO n DO writef(" %d/%d", mvn!i, mvt!i)
      newline(); newline(); pr n
    }

    LET r=np, w1bits=0, w0bits=0, pbits=0
    stackp := s

    UNTIL r>=nq DO      // Form the set of of possible replies
    { LET a = !r+++
      LET b = !r+++
      LET c = !r+++
      UNLESS a&w1 OR b&w0 OR c&piece DO { !stackp+++ := a
                                          !stackp+++ := b
                                          !stackp+++ := c
                                          w1bits |:= a
                                          w0bits |:= b
                                          pbits  |:= c
                                        }
    }

    // The possible replies are stored between s and stackp

    IF s=stackp RETURN TRUE // The chosen move is a winner

    // Explore the possible replies
    TEST n>=2 AND path!(n+1)<0
    THEN UNLESS cmp64(n+1, s, stackp, w1bits, w0bits, pbits) RETURN TRUE
    ELSE UNLESS try76(n+1, s, stackp, s, stackp  )           RETURN TRUE
  } 

  // We cannot find a winning move from the available moves
  stackp := s
  RETURN FALSE

FUN cmp64 : n, p, q, w1bits, w0bits, pbits => 
  LET s = stackp

  w1bits64, w0bits64, pbits64, prn64 := w1bits, w0bits, pbits, n

  // Compress the representation of the moves from 76 to 64 bits.
  UNTIL p>=q DO { LET w1    = !p+++
                  LET w0    = !p+++
                  LET piece = !p+++
                  cmpput64(w1, w0, piece)
                }

  LET res = try64(n, s, stackp)

  prn64 := 20
  stackp := s
  RETURN res


FUN try64 : n, p, q =>
  LET s=stackp, t=p

  WHILE t < q DO
  { stackp := s

    LET w1 = !t+++     // Choose a move
    LET w0 = !t+++

    w1v!n, w0v!n := w1, w0

    mvn!n, mvt!n := (t-p)/(2*Bpw), (q-p)/(2*Bpw)

    IF n=4 DO
    { writef("\n\nTrying Move %d: %3d/%d:\n", n, mvn!n, mvt!n)
      pr n
    }
    IF n=5 DO newline()
    IF n=6 DO
    { FOR i = 1 TO n DO writef("%3d/%d ", mvn!i, mvt!i)
      writes "      \^m"
    }

    LET r=p, w1bits=0, w0bits=0

    UNTIL r>=q DO      // Form the set of of possible replies
    { LET a = !r+++
      LET b = !r+++
      UNLESS a&w1 OR b&w0 DO { !stackp+++ := a
                               !stackp+++ := b
                               w1bits, w0bits |:= a, b
                             }
    }

    // The possible replies are stored between s and stackp

    IF s=stackp RETURN TRUE // Move n is a winner

    // See if this move n was a winner
    TEST bits w1bits + bits w0bits <= 32
    THEN UNLESS cmp32(n+1, s, stackp, w1bits, w0bits) RETURN TRUE
    ELSE UNLESS try64(n+1, s, stackp)                 RETURN TRUE
  } 

  // We cannot find a winning move from the available moves
  stackp := s
  RETURN FALSE

FUN cmp32 : n, p, q, w1bits, w0bits => 
  LET s = stackp
  w1bits32, w0bits32, prn32 := w1bits, w0bits, n

  // Compact the representation of the moves from 64 to 32 bits.
  UNTIL p>=q DO { LET w1 = !p+++
                  LET w0 = !p+++
                  cmpput32(w1, w0)
                }

  LET res = try32(n, s, stackp)

  prn32 := 20
  stackp := s
  RETURN res


FUN try32 : n, p, q =>
  LET s=stackp, t=p

  WHILE t < q DO
  { LET w0 = !t+++     // Choose a move

//  w0v!n := w0
//  newline(); pr n

    LET r = p
    stackp := s

    UNTIL r>=q DO      // Form the set of possible replies
    { LET a = !r+++
      UNLESS a&w0 DO !stackp+++ := a
    }

    IF s=stackp RETURN TRUE      // Move n is a winner
    IF n=11 LOOP                 // Move n is a loser
    UNLESS try32(n+1, s, stackp) RETURN TRUE
  } 

  // We cannot find a winning move from the available moves
  stackp := s
  RETURN FALSE

STATIC
  chs = CVEC 64,
  w1bits64, w0bits64, pbits64, prn64=20, 
  w1bits32, w0bits32,          prn32=20, 
  prw1, prw0

FUN pr : n =>
  FOR i = 1 TO 64 DO chs%i := '.'

  FOR p = 1 TO n DO
  { LET ch = 'A'+p-1
    IF p=n DO ch := '*'
    prw1, prw0 := w1v!p, w0v!p

    IF p>=prn32 DO exp32()  // expand from 32 to 64 bits
    IF p>=prn64 DO exp64()  // expand from 64 to 76 bits
    // prw1 and prw0 now contain the board bits

    FOR i = 1 TO 64 DO  // Convert to and 8x8 array of chars
    { IF prw0&1 DO chs%i := ch
      prw0 >>:= 1
      UNLESS i MOD 32 DO prw0 := prw1
    }
  }

  FOR i = 1 TO 64 DO    // Output the 8x8 array
  { writef(" %c", chs%i)
    IF i MOD 8 = 0  DO newline()
  }
  newline()

FUN cmpput64 : w1, w0, piece =>
  LET w1bits=~w1bits64, w0bits=~w0bits64, pbits=pbits64
  LET pbit = ?
  WHILE pbits AND w0bits DO
  { LET w0bit = w0bits & -w0bits
    pbit  := pbits  & -pbits
    IF piece&pbit DO w0 |:= w0bit // Move a piece bit into w0
    pbits  -:= pbit
    w0bits -:= w0bit
  }
  WHILE pbits AND w1bits DO
  { LET w1bit = w1bits & -w1bits
    pbit  := pbits  & -pbits
    IF piece&pbit DO w1 |:= w1bit // Move a piece bit into w1
    pbits  -:= pbit
    w1bits -:= w1bit
  }
  !stackp+++ := w1
  !stackp+++ := w0

FUN cmpput32 : w1, w0 =>
  LET w1bits=w1bits32, w0bits=~w0bits32
  WHILE w1bits AND w0bits DO
  { LET w1bit = w1bits & -w1bits 
    LET w0bit = w0bits & -w0bits
    IF w1&w1bit DO w0 |:= w0bit // Move a w1 bit into w0
    w1bits -:= w1bit
    w0bits -:= w0bit
  }
  !stackp+++ := w0

FUN exp64 : =>
  prw1 &:= w1bits64  // Remove the piece bits from
  prw0 &:= w0bits64  // the w1 and w0 bit patterns

FUN exp32 : => // Move various bits from prw0 into prw1
  LET w1bits=w1bits32, w0bits=~w0bits32
  prw1 := 0
  WHILE w1bits AND w0bits DO
  { LET w1bit = w1bits & -w1bits
    LET w0bit = w0bits & -w0bits
    IF prw0&w0bit DO { prw0 -:= w0bit; prw1 |:= w1bit }
    w1bits -:= w1bit
    w0bits -:= w0bit
  }
