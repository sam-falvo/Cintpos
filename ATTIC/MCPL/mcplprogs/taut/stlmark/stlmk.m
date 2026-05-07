GET "mcpl.h"

/* This is an algorithm for tautology checking implemented in MCPL
   by M. Richards. It is based on Stalmarck's algorithm as described
   in the paper:

   G. Stalmarck and M. Saflund, Modelling and Verifying Systems and
   Software in Propositional Logic, IFAC, SAFECOMP90, London, UK 1990

   Stalmarck's algorithm is a patented.
*/

FUN bug : mess, a, b,c ,d => writef(mess, a, b, c, d); abort 999

MANIFEST 

Id, Lparen, Rparen,Eof,  // Lexical tokens

// There are 256 relations over 3 boolean variable. Use one byte (8 bits)
// to represent any particular relation.

//  [rel x y z]

//  x   1 1 1 1 0 0 0 0
//  y   1 1 0 0 1 1 0 0
//  z   1 0 1 0 1 0 1 0

//      a b c d e f g h
//                    h=1 <=> 000 in rel
//                  g=1 <=> 001 in rel
//                f=1 <=> 010 in rel
//              e=1 <=> 011 in rel        
//            d=1 <=> 100 in rel
//          c=1 <=> 101 in rel
//        b=1 <=> 110 in rel
//      a=1 <=> 111 in rel

NotY=#b00111100,
And =#b10000111,
Or  =#b11100001,
Imp =#b10110100,
Eqv =#b10010110

FUN relstr
:#b00000000=>"FF     ":#b00000001=>"xyz:000"
:#b00000010=>"xyz:001":#b00000011=>"000+001"
:#b00000100=>"xyz:010":#b00000101=>"000+010"
:#b00000110=>"001+010":#b00000111=>"???    "
:#b00001000=>"xyz:011":#b00001001=>"000+011"
:#b00001010=>"001+011":#b00001011=>"???    "
:#b00001100=>"010+011":#b00001101=>"???    "
:#b00001110=>"???    ":#b00001111=>"x:0    "
:#b00010000=>"xyz:100":#b00010001=>"000+100"
:#b00010010=>"001+100":#b00010011=>"???    "
:#b00010100=>"010+100":#b00010101=>"???    "
:#b00010110=>"???    ":#b00010111=>"???    "
:#b00011000=>"011+100":#b00011001=>"???    "
:#b00011010=>"???    ":#b00011011=>"???    "
:#b00011100=>"???    ":#b00011101=>"???    "
:#b00011110=>"x#y|z  ":#b00011111=>"???    "
:#b00100000=>"xyz:101":#b00100001=>"000+101"
:#b00100010=>"001+101":#b00100011=>"???    "
:#b00100100=>"010+101":#b00100101=>"???    "
:#b00100110=>"???    ":#b00100111=>"???    "
:#b00101000=>"011+101":#b00101001=>"???    "
:#b00101010=>"???    ":#b00101011=>"???    "
:#b00101100=>"???    ":#b00101101=>"x:y<z  "
:#b00101110=>"???    ":#b00101111=>"???    "
:#b00110000=>"100+101":#b00110001=>"???    "
:#b00110010=>"???    ":#b00110011=>"y:0    "
:#b00110100=>"???    ":#b00110101=>"???    "
:#b00110110=>"y#x|z  ":#b00110111=>"???    "
:#b00111000=>"???    ":#b00111001=>"z:x>y  "
:#b00111010=>"???    ":#b00111011=>"???    "
:#b00111100=>"x#y    ":#b00111101=>"???    "
:#b00111110=>"???    ":#b00111111=>"???    "
:#b01000000=>"xyz:110":#b01000001=>"001+110"
:#b01000010=>"001+110":#b01000011=>"???    "
:#b01000100=>"010+110":#b01000101=>"???    "
:#b01000110=>"???    ":#b01000111=>"???    "
:#b01001000=>"011+110":#b01001001=>"???    "
:#b01001010=>"???    ":#b01001011=>"x:y>z  "
:#b01001100=>"???    ":#b01001101=>"???    "
:#b01001110=>"???    ":#b01001111=>"???    "
:#b01010000=>"100+110":#b01010001=>"???    "
:#b01010010=>"???    ":#b01010011=>"???    "
:#b01010100=>"???    ":#b01010101=>"z:0    "
:#b01010110=>"z#x|y  ":#b01010111=>"???    "
:#b01011000=>"???    ":#b01011001=>"z:x<y  "
:#b01011010=>"x#z    ":#b01011011=>"???    "
:#b01011100=>"???    ":#b01011101=>"???    "
:#b01011110=>"???    ":#b01011111=>"???    "
:#b01100000=>"101+110":#b01100001=>"???    "
:#b01100010=>"???    ":#b01100011=>"y:x>z  "
:#b01100100=>"???    ":#b01100101=>"y:x<z  "
:#b01100110=>"y#z    ":#b01100111=>"???    "
:#b01101000=>"???    ":#b01101001=>"x#y=z  "
:#b01101010=>"z#x&y  ":#b01101011=>"???    "
:#b01101100=>"y#x&z  ":#b01101101=>"???    "
:#b01101110=>"???    ":#b01101111=>"???    "
:#b01110000=>"???    ":#b01110001=>"???    "
:#b01110010=>"???    ":#b01110011=>"???    "
:#b01110100=>"???    ":#b01110101=>"???    "
:#b01110110=>"???    ":#b01110111=>"???    "
:#b01111000=>"x#y&z  ":#b01111001=>"???    "
:#b01111010=>"???    ":#b01111011=>"???    "
:#b01111100=>"???    ":#b01111101=>"???    "
:#b01111110=>"???    ":#b01111111=>"xys#111"

:#b10000000=>"xyz:111":#b10000001=>"000+111"
:#b10000010=>"001+111":#b10000011=>"???    "
:#b10000100=>"010+111":#b10000101=>"???    "
:#b10000110=>"???    ":#b10000111=>"x:y&z  "
:#b10001000=>"011+111":#b10001001=>"???    "
:#b10001010=>"???    ":#b10001011=>"???    "
:#b10001100=>"???    ":#b10001101=>"???    "
:#b10001110=>"???    ":#b10001111=>"???    "
:#b10010000=>"100+111":#b10010001=>"???    "
:#b10010010=>"???    ":#b10010011=>"y:x&z  "
:#b10010100=>"???    ":#b10010101=>"z:x&y  "
:#b10010110=>"x:y=z  ":#b10010111=>"???    "
:#b10011000=>"???    ":#b10011001=>"y:z    "
:#b10011010=>"z#x>y  ":#b10011011=>"???    "
:#b10011100=>"y#x>z  ":#b10011101=>"???    "
:#b10011110=>"???    ":#b10011111=>"???    "
:#b10100000=>"101+111":#b10100001=>"???    "
:#b10100010=>"???    ":#b10100011=>"???    "
:#b10100100=>"???    ":#b10100101=>"x:z    "
:#b10100110=>"z#x<y  ":#b10100111=>"???    "
:#b10101000=>"???    ":#b10101001=>"z:x|y  "
:#b10101010=>"z:1    ":#b10101011=>"???    "
:#b10101100=>"???    ":#b10101101=>"???    "
:#b10101110=>"???    ":#b10101111=>"???    "
:#b10110000=>"???    ":#b10110001=>"???    "
:#b10110010=>"???    ":#b10110011=>"???    "
:#b10110100=>"x#y>z  ":#b10110101=>"???    "
:#b10110110=>"???    ":#b10110111=>"???    "
:#b10111000=>"???    ":#b10111001=>"???    "
:#b10111010=>"???    ":#b10111011=>"???    "
:#b10111100=>"???    ":#b10111101=>"???    "
:#b10111110=>"???    ":#b10111111=>"xyz#110"
:#b11000000=>"110+111":#b11000001=>"???    "
:#b11000010=>"???    ":#b11000011=>"x:y    "
:#b11000100=>"???    ":#b11000101=>"???    "
:#b11000110=>"y#x<z  ":#b11000111=>"???    "
:#b11001000=>"???    ":#b11001001=>"y:x|z  "
:#b11001010=>"???    ":#b11001011=>"???    "
:#b11001100=>"y:1    ":#b11001101=>"???    "
:#b11001110=>"???    ":#b11001111=>"???    "
:#b11010000=>"???    ":#b11010001=>"???    "
:#b11010010=>"x#y<z  ":#b11010011=>"???    "
:#b11010100=>"???    ":#b11010101=>"???    "
:#b11010110=>"???    ":#b11010111=>"???    "
:#b11011000=>"???    ":#b11011001=>"???    "
:#b11011010=>"???    ":#b11011011=>"???    "
:#b11011100=>"???    ":#b11011101=>"???    "
:#b11011110=>"???    ":#b11011111=>"xyz#101"
:#b11100000=>"???    ":#b11100001=>"x:y|z  "
:#b11100010=>"???    ":#b11100011=>"???    "
:#b11100100=>"???    ":#b11100101=>"???    "
:#b11100110=>"???    ":#b11100111=>"???    "
:#b11101000=>"???    ":#b11101001=>"???    "
:#b11101010=>"???    ":#b11101011=>"???    "
:#b11101100=>"???    ":#b11101101=>"???    "
:#b11101110=>"???    ":#b11101111=>"xyz#100"
:#b11110000=>"x:1    ":#b11110001=>"???    "
:#b11110010=>"???    ":#b11110011=>"???    "
:#b11110100=>"???    ":#b11110101=>"???    "
:#b11110110=>"???    ":#b11110111=>"xyz#011"
:#b11111000=>"???    ":#b11111001=>"???    "
:#b11111010=>"???    ":#b11111011=>"xyz#010"
:#b11111100=>"???    ":#b11111101=>"xyz#001"
:#b11111110=>"xyz#000":#b11111111=>"TT     "
:        -1=>"Void   "
:          =>"       "

MANIFEST
  E_syntax=100, E_Space,        // Exceptions
  E_FalseTermFound,
  E_NoTerms,

// Rows in the matrix representation of the propositional expression
// are of the form: [rel, x, y, z]
// where rel is a relation over three booleans
// and x, y and z are variable ids, some occuring in the expression
// while others are computer generated. The ids are represented by
// integers: 0, 1, 2,...
// Id 0 is always false
// Id 1 is always true
// Id 2 is used to mark a variable that only occurs once in the set
//      of terms and so can be treated as a don't-care value.

// and the other ids are free to hold either true or false

// Terms sometimes allow information about their variables to be
// deduced.  For example: [And, x, 1, 1] => x=1
//                        [Imp, 0, y, y] is false
//                        [Imp, 0, y, z] => y=1 and z=0
//                        [Imp, x, y, 0] => x=~y

// The pattern of operands in [rel,x,y,z] is also a relation.
// An operand pattern takes account of the following conditions:
//   x=0 x=1 y=0 y=1 z=0 z=1 y=z z=x and x=y

// The 37 possible operand patterns have the following manifest names

P000=#b00000001, P001=#b00000010, P00z=#b00000011,
P010=#b00000100, P011=#b00001000, P01z=#b00001100,
P0y0=#b00000101, P0y1=#b00001010, P0yy=#b00001001, P0yz=#b00001111,
P100=#b00010000, P101=#b00100000, P10z=#b00110000,
P110=#b01000000, P111=#b10000000, P11z=#b11000000,
P1y0=#b01010000, P1y1=#b10100000, P1yy=#b10010000, P1yz=#b11110000,
Px00=#b00010000, Px01=#b00100010, Px0x=#b00100001, Px0z=#b00110011,
Px10=#b01000100, Px11=#b10001000, Px1x=#b10000100, Px1z=#b11001100,
Pxx0=#b01000001, Pxx1=#b10000010, Pxxx=#b10000001, Pxxz=#b11000011,
Pxy0=#b01010101, Pxy1=#b10101010, Pxyx=#b10100101, Pxyy=#b10011001,
Pxyz=#b11111111

FUN patstr  // return string for pattern
:P000=>"P000" :P001=>"P001" :P00z=>"P00z"
:P010=>"P010" :P011=>"P011" :P01z=>"P01z"
:P0y0=>"P0y0" :P0y1=>"P0y1" :P0yy=>"P0yy" :P0yz=>"P0yz"
:P100=>"P100" :P101=>"P101" :P10z=>"P10z"
:P110=>"P110" :P111=>"P111" :P11z=>"P11z"
:P1y0=>"P1y0" :P1y1=>"P1y1" :P1yy=>"P1yy" :P1yz=>"P1yz"
:Px00=>"Px00" :Px01=>"Px01" :Px0x=>"Px0x" :Px0z=>"Px0z"
:Px10=>"Px10" :Px11=>"Px11" :Px1x=>"Px1x" :Px1z=>"Px1z"
:Pxx0=>"Pxx0" :Pxx1=>"Pxx1" :Pxxx=>"Pxxx" :Pxxz=>"Pxxz"
:Pxy0=>"Pxy0" :Pxy1=>"Pxy1" :Pxyx=>"Pxyx" :Pxyy=>"Pxyy" :Pxyz=>"Pxyz"
:      => "Perr"

// pattern(rel, x, y, z) returns the operand pattern

FUN pattern : x, y, z => // Return pattern of operands
  LET res = #b11111111
  IF x<=1 DO res &:= x=0 -> #b00001111, #b11110000
  IF y<=1 DO res &:= y=0 -> #b00110011, #b11001100
  IF z<=1 DO res &:= z=0 -> #b01010101, #b10101010
  IF y=z  DO res &:= #b10011001
  IF z=x  DO res &:= #b10100101
  IF x=y  DO res &:= #b11000011
//writef("pattern: %5d %5d %5d   %8b %s\n", x, y, z, res, relstr res)
  RETURN res

// As mentioned above, information can sometimes be deduced from
// the operator of a term and the pattern of its arguments.
// For example: [And, x, 1, 1] => x=1
//              [Imp, 0, y, y] is false
//              [Imp, 0, y, z] => y=1 and z=0
//              [Imp, x, y, 0] => x=~y

// There are 12 possible mapping actions.

MANIFEST   // For term: [rel, x, y, z]

  Ax0 = #b000000_000001, // Map x -> 0
  Ax1 = #b000000_000010, // Map x -> 1
  Ay0 = #b000000_000100, // Map y -> 0
  Ay1 = #b000000_001000, // Map y -> 1
  Az0 = #b000000_010000, // Map z -> 0
  Az1 = #b000000_100000, // Map z -> 1
  Aypz= #b000001_000000, // Map y -> z
  Azpx= #b000010_000000, // Map z -> x
  Axpy= #b000100_000000, // Map x -> y
  Aynz= #b001000_000000, // Map y -> ~z
  Aznx= #b010000_000000, // Map z -> ~x
  Axny= #b100000_000000  // Map x -> ~y

FUN actstr
: Ax0   => "Ax0  " : Ax1   => "Ax1  "
: Ay0   => "Ay0  " : Ay1   => "Ay1  "
: Az0   => "Az0  " : Az1   => "Az1  "
: Aypz  => "Aypz " : Azpx  => "Azpx " : Axpy  => "Axpy "
: Aynz  => "Aynz " : Aznx  => "Aznx " : Axny  => "Axny "
:       => "Aerr "


STATIC notx,   // Maps rel->rel' where [rel x y z] = [rel' ~x  y  z]
       noty,   // Maps rel->rel' where [rel x y z] = [rel'  x ~y  z]
       notz,   // Maps rel->rel' where [rel x y z] = [rel'  x  y ~z]
       swapyz, // Maps rel->rel' where [rel x y z] = [rel'  x  z  y]
       swapzx, // Maps rel->rel' where [rel x y z] = [rel'  z  y  x]
       swapxy, // Maps rel->rel' where [rel x y z] = [rel'  y  x  z]

       dontcarex, // Maps rel->rel' where [rel x y z] = [rel'  0  y  z]
                  //                when x occurs only in this term
       dontcarey, // Maps rel->rel' where [rel x y z] = [rel'  x  0  z]
                  //                when y occurs only in this term
       dontcarez, // Maps rel->rel' where [rel x y z] = [rel'  x  y  0]
                  //                when z occurs only in this term

       rel2act // Maps rel to set of actions

FUN close_tabs : =>
  freevec notx
  freevec noty
  freevec notz
  freevec swapyz
  freevec swapzx
  freevec swapxy
  freevec dontcarex
  freevec dontcarey
  freevec dontcarez
  freevec rel2act

// These vectors are allocated and initialised by the following function

FUN init_tabs : =>
  notx      := getvec(#b11111111/Bpw)
  noty      := getvec(#b11111111/Bpw)
  notz      := getvec(#b11111111/Bpw)
  swapyz    := getvec(#b11111111/Bpw)
  swapzx    := getvec(#b11111111/Bpw)
  swapxy    := getvec(#b11111111/Bpw)
  dontcarex := getvec(#b11111111/Bpw)
  dontcarey := getvec(#b11111111/Bpw)
  dontcarez := getvec(#b11111111/Bpw)
  rel2act   := getvec #b11111111

  UNLESS swapyz    AND swapzx    AND swapxy    AND
         notx      AND noty      AND notz      AND
         dontcarex AND dontcarey AND dontcarez AND
         rel2act RAISE E_Space

  FOR w = #b00000000 TO #b11111111 DO
  { 
    notx%w   := (w & #b11110000)>>4 |  // abcdefgh -> efghabcd
                (w & #b00001111)<<4
    noty%w   := (w & #b11001100)>>2 |  // abcdefgh -> cdabghef
                (w & #b00110011)<<2
    notz%w   := (w & #b10101010)>>1 |  // abcdefgh -> badcfehg
                (w & #b01010101)<<1
    swapyz%w :=  w & #b10011001     |  // abcdefgh -> acbdegfh
                (w & #b01000100)>>1 |
                (w & #b00100010)<<1
    swapzx%w :=  w & #b10100101     |  // abcdefgh -> aecgbfdh
                (w & #b01010000)>>3 |
                (w & #b00001010)<<3
    swapxy%w :=  w & #b11000011     |  // abcdefgh -> abefcdgh
                (w & #b00110000)>>2 |
                (w & #b00001100)<<2

    dontcarex%w := (w>>4 | w) & #b00001111 // abcdefgh -> 0000abcd &
                                           //             0000efgh
    dontcarey%w := (w>>2 | w) & #b00110011 // abcdefgh -> 00ab00ef &
                                           //             00cd00gh
    dontcarez%w := (w>>2 | w) & #b01010101 // abcdefgh -> 0a0c0e0g &
                                           //             0b0d0f0h

    LET acts = 0

    UNLESS w & #b11110000 DO acts +:= Ax0
    UNLESS w & #b00001111 DO acts +:= Ax1
    UNLESS w & #b11001100 DO acts +:= Ay0
    UNLESS w & #b00110011 DO acts +:= Ay1
    UNLESS w & #b10101010 DO acts +:= Az0
    UNLESS w & #b01010101 DO acts +:= Az1
    UNLESS w & #b01100110 OR acts & (Ay0+Ay1) DO acts +:= Aypz
    UNLESS w & #b01011010 OR acts & (Ax0+Ax1) DO acts +:= Azpx
    UNLESS w & #b00111100 OR acts & (Ax0+Ax1) DO acts +:= Axpy
    UNLESS w & #b10011001 OR acts & (Ay0+Ay1) DO acts +:= Aynz
    UNLESS w & #b10100101 OR acts & (Ax0+Ax1) DO acts +:= Aznx
    UNLESS w & #b11000011 OR acts & (Ax0+Ax1) DO acts +:= Axny

    rel2act!w := acts
  }

  IF debug<100 RETURN

  writef "\nTest tables\n\n"
  FOR i = 0 TO 255 DO
  { LET w = i                                //    [w x y z] = [i x y z]
    writef("%8b %s  ", w, relstr w)          // ie [w x y z] = [i x y z]
    writef("%8b %s  ", notx%w, relstr (notx%w))
    writef("%8b %s  ", noty%w, relstr (noty%w))
    writef("%8b %s\n", notz%w, relstr (notz%w))
    w := swapyz%i                            //    [w x z y] = [i x y z]
    writef("%8b %s  ", w, relstr w)          // ie [w x y z] = [i x z y]
    writef("%8b %s  ", notx%w, relstr (notx%w))
    writef("%8b %s  ", noty%w, relstr (noty%w))
    writef("%8b %s\n", notz%w, relstr (notz%w))
    w := swapxy%i                            //    [w y x z] = [i x y z]
    writef("%8b %s  ", w, relstr w)          // ie [w x y z] = [i y x z]
    writef("%8b %s  ", notx%w, relstr (notx%w))
    writef("%8b %s  ", noty%w, relstr (noty%w))
    writef("%8b %s\n", notz%w, relstr (notz%w))
    w := swapyz%(swapxy%i)                   //    [w y z x] = [i x y z]
    writef("%8b %s  ", w, relstr w)          // ie [w x y z] = [i z x y]
    writef("%8b %s  ", notx%w, relstr (notx%w))
    writef("%8b %s  ", noty%w, relstr (noty%w))
    writef("%8b %s\n", notz%w, relstr (notz%w))
    w := swapxy%(swapyz%i)                   //    [w z x y] = [i x y z]
    writef("%8b %s  ", w, relstr w)          // ie [w x y z] = [i y z x]
    writef("%8b %s  ", notx%w, relstr (notx%w))
    writef("%8b %s  ", noty%w, relstr (noty%w))
    writef("%8b %s\n", notz%w, relstr (notz%w))
    w := swapzx%i                            //    [w z y x] = [i x y z]
    writef("%8b %s  ", w, relstr w)          // ie [w x y z] = [i z y x]
    writef("%8b %s  ", notx%w, relstr (notx%w))
    writef("%8b %s  ", noty%w, relstr (noty%w))
    writef("%8b %s\n", notz%w, relstr (notz%w))
    newline()
  }

// A term is represented by a 4-tuple: [rel, x, y, z]

// Aside: when the number of distinct variables becomes less than 256
// the tuple may one day be packed into a 32 bits: 8-8-8-8, to save
// space and improve processor cache performance.

// The 4-tuples are held in a vector of 4 word cells [rel, x, y, z]
// but later versions may use 5-tuples [rel, v3,v2,v1,v0] for 
// relational terms over 4 variable, or even 16-tuples for relations
// over 8 variables [r7,r6,r5,r4,r3,r2,r1,r0, v7,v6,v5,v4,v3,v2,v1,v0]

MANIFEST // Term fields for the 4-tuple version [rel, x,y,z]
         Trel,  // An 8-bit relation on variables x, y and z
         Tx,    // The integer id of the first  argument
         Ty,    // The integer id of the second argument
         Tz,    // The integer id of the third  argument
         Tsize  // The size of a term

STATIC   curts,    // The current termset
         term1,    // Pointer to the first term in curts
         termp,    // next free term position in the curts
         termt,    // Pointer to the last element of the curts vector
         termcount // Count of number of terms in curts

                  // Termset fields
MANIFEST TsVm,    // 0 or first varmap
         TsVmax,  // largest variable id in use
         TsTerm1, // Pointer to the first term (on a 16 byte boundary)
         TsTermt, // Pointer to the last possible term position
         TsTn,    // Actual number of terms in the set
         TsSortv, // Vector of term pointers for sorting, or zero
         TsSysSize

FUN alloc_termset : tmax => // Allocate a termset large enough to
                            // hold tmax terms.

  LET tsupb = TsSysSize+15+Tsize*(tmax+1) // Leave room alignment space.
  LET ts    = getvec tsupb

  IF ts=0 RAISE (E_Space, "no space for termset vector\n")

  ts!TsVm    := 0  // No varmap
  ts!TsVmax  := 0  // Max variable number, when known

  LET t = @ts!TsSysSize
  WHILE t&15 DO t++  // Round up to next 16-byte (4-word) boundary.
  
  ts!TsTerm1 := t
  ts!TsTermt := @t!(Tsize*tmax)
  IF @ts!tsupb < @t!(Tsize*(tmax+1)) DO // Check that there is room
     bug("alloc_termset error")         // for tmax terms

  ts!TsTn    := 0           // No terms in the set yet
  ts!TsSortv := 0           // No sort vector
  RETURN ts

FUN mkcopy_termset : ts =>
  LET n = ts!TsTn
  LET nts = alloc_termset n // Allocate a termset of the right size
  LET p =  ts!TsTerm1
  LET q = nts!TsTerm1
  FOR i = 1 TO n MATCH (p, q)
  : [r,x,y,z,np], [:=r,:=x,:=y,:=z, nq] => p, q := @np, @nq
  .
  RETURN nts

FUN close_termset : ts =>
  IF ts!TsVm    DO freevec(ts!TsVm)
  IF ts!TsSortv DO freevec(ts!TsSortv)
  freevec ts

FUN mkterm : rel, y, z => varn +:= 2
                          pushterm(rel, varn, y, z)
                          RETURN varn

FUN pushterm : rel, x, y, z =>
  IF termp>termt RAISE (E_Space, "Too many terms\n")

  MATCH termp : [:=rel, :=x, :=y, :=z, np] => termcount++
                                              termp := @np


// Variables such as x, y and z in terms [rel,x,y,z] are identified
// by positive integers. Variables may get renamed by such actions
// as: Azpx (set x=z) or Axny (set x=~y). The accumulated collection
// of mappings is held in the current mapping vector varmap.

// If varmap!x>=0 then x maps to variable varmap!x
// If varmap!x<0  then x maps to the complement of variable -varmap!x


STATIC
  varmap=0   // base of the current varmap vector

FUN compact_vars : ts =>
  LET vm   = ts!TsVm       // get the varmap vector, if allocated
  LET vmax = ts!TsVmax
  UNLESS vm DO vm := getvec vmax
  UNLESS vm RAISE(E_Space, "Can't allocate a varmap vector size %d\n", vmax)

  FOR i = 3 TO vmax DO vm!i := 0
  LET n = ts!TsTn
  LET p = ts!TsTerm1

  FOR i = 1 TO n MATCH p
  : [rel, x, y, z, np] => (vm!x)++  // Mark variables in use
                          (vm!y)++
                          (vm!z)++
                          p := @np
  .

  LET var = 3
  vm!0 := 0  // False
  vm!1 := 1  // true
  vm!2 := 2  // Don't care
  FOR i = 3 TO vmax IF vm!i TEST vm!i=1 THEN vm!i := 2     // Don't care
                                        ELSE vm!i := var++
  p := ts!TsTerm1

  FOR i = 1 TO n MATCH p    // Rename variables
  : [rel, x(:=vm!x), y(:=vm!y), z(:=vm!z), np] => p := @np
  .

  IF vm DO freevec vm       // Free the old mapping vector

  ts!TsVm   := getvec var   // Allocate a new varmap
  ts!TsVmax := var

FUN compact_terms : ts => // remove void or duplicate terms

  mark_duplicates ts      // mark duplicates as void

  LET n = ts!TsTn         // Number of terms
  LET p = ts!TsTerm1      // Next term to look at
  LET k = 0               // Count of remaining terms
  LET q = p               // Next free term position

  FOR i = 1 TO n MATCH (p, q)
  : [ <0,x,y,z,np],                     ?  => p = @np // Skip void term

  : [rel,x,y,z,np], [:=rel,:=x,:=y,:=z,nq] => k++     // Copy term
                                              p, q := @np, @nq
  .
  ts!TsTn := k
  IF debug>3 AND k<n DO { writef("Compacted to %d terms\n", k)
                          prterms ts
                        }
  UNLESS n RAISE E_NoTerms


FUN mark_duplicates : ts => // Mark duplicates as void
//  writef "mark_duplicates\n"

  LET n = ts!TsTn
  LET k = 0            // Count of non void terms
  LET v = getvec n     // Allocate sort vector
  IF v=0 RAISE E_Space

  IF ts!TsSortv DO freevec(ts!TsSortv)
  ts!TsSortv := v  // Save the sort vector in ts so that it can be freed

  LET p = ts!TsTerm1

  FOR i = 1 TO n DO    // Put non void terms in the sort vector
  { UNLESS !p<0 DO { canonterm p; v!++k := p }
    p := @p!Tsize
  }

//  prmapping ts
//  writef("Terms for sorting\n")
//  FOR i = 1 TO k DO prterm(v!i)
//  newline()

  sort(v, k, cmpfull)  // Sort the non void terms

  FOR i = 1 TO k-1 DO
  { LET t  = v!i
    LET t' = v!(i+1)
    IF t!Trel>0 MATCH (t, t') // Compare adjacent two terms
                : [rel,x,y,z], [=rel,=x,=y,=z] => rel := -1
                :                              => LOOP
  }

//  writef("Terms after marking duplicated\n")
//  FOR i = 1 TO k DO prterm(v!i)
  freevec v  // Free the sort vector

FUN prterm : [rel, x, y, z] =>
  writef("%8b %s %5d %5d %5d\n", rel, relstr rel, x,  y,  z)

FUN prterms : ts =>
  writef "Terms:\n"

  LET n = ts!TsTn     // The number of terms
  LET p = ts!TsTerm1

  FOR i = 1 TO n DO { IF !p>=0 DO { writef("%5d  ", i)
                                    prterm p
                                  }
                      p := @p!Tsize
                    }
  prmapping ts

FUN prmapping : ts =>
  LET k = 0         // For layout
  LET vm = ts!TsVm
  writef "Mapping:"
  UNLESS vm DO { writef " No mapping vector\n"; RETURN }
  FOR i = 0 TO ts!TsVmax UNLESS i = vm!i DO
  { UNLESS k++ MOD 10 DO newline()
    writef("  %d->%d", i, vm!i)
  }
  newline()


FUN check : ts => // It may raise E_FalseTermFound
                  //              E_NoTerms
                  //              E_Space
{ apply_simple_rules  ts
  apply_dilemma_rule1 ts
  apply_dilemma_rule2 ts
  apply_dilemma_rule3 ts

  RETURN "Unable to decide whether it is a tautology\n"
} HANDLE : E_FalseTermFound => "It is a tautology\n"
         : E_NoTerms        => "It is NOT a tautology\n"
         : E_Space          => "Ran out of space\n"
         .

FUN apply_dilemma_rule1 : ts =>
  writef "dilemma rule1 not implemented\n"

FUN apply_dilemma_rule2 : ts =>
  writef "dilemma rule2 not implemented\n"

FUN apply_dilemma_rule3 : ts =>
  writef "dilemma rule3 not implemented\n"

STATIC
  change  // Set to TRUE whenever the variable mapping changes

FUN apply_simple_rules : ts =>
             // It may raise E_FalseTermFound
             //              E_NoTerms
             //              E_Space

  IF debug>0 DO writef "\nApplying simple rules\n"

  compact_vars ts

  varmap := ts!TsVm
  FOR i = 0 TO ts!TsVmax DO varmap!i := i // null mapping

  IF debug>0 DO { writef "Initial terms are:\n"
                  prterms ts
                }

  // The root variable was set to 0, so if the conjunction
  // of terms can be satisfied, the given expression
  // can evaluate to false.

  change := FALSE

  { apply_unit_rules ts
    compact_terms ts
    IF debug>3 DO prterms ts

    apply_pair_rules ts
    compact_terms ts
    IF debug>3 DO prterms ts

  } REPEATWHILE change

FUN mapterm : [rel, x, y, z] => // On entry, x, y and z >=0
                                // On return, x =0 or >2,
                                //            y =0 or >2
                                //        and z =0 or >2

  MATCH mapof x : t(<0) => rel := notx%rel
                           x   := -t
                : t     => x   :=  t
                .
  MATCH mapof y : t(<0) => rel := noty%rel
                           y   := -t
                : t     => y   :=  t
                .
  MATCH mapof z : t(<0) => rel := notz%rel
                           z   := -t
                : t     => z   :=  t
                .

  IF x=2 DO rel, x := dontcarex%rel, 0  // Check for don't-cares
  IF y=2 DO rel, y := dontcarey%rel, 0
  IF z=2 DO rel, z := dontcarez%rel, 0

  IF x=1 DO rel, x := notx%rel, 0       // Check for True
  IF y=1 DO rel, y := noty%rel, 0
  IF z=1 DO rel, z := notz%rel, 0


FUN mapof : x =>
  LET t = varmap!x

  TEST t<0 THEN UNLESS t = varmap!(-t) DO { t := -mapof(-t)
                                            varmap!x := t
                                          }
           ELSE UNLESS t = varmap!t    DO { t := mapof t
                                            varmap!x := t
                                          }
  RETURN t

FUN canonterm
:[ <0, :=0, :=0, :=0] => RETURN // A void term

:p[rel, x, y, z] =>

  mapterm p
  // x =0 or >2, y =0 or >2  and z =0 or >2

  // rel=abcdabcd => don't care x, so set x=0 in this term
  UNLESS (rel>>4 XOR rel) & #b00001111 DO { x := 0
                                            rel &:= #b00001111
                                          }
  // rel=ababcdcd => don't care y, so set y=0 in this term
  UNLESS (rel>>2 XOR rel) & #b00110011 DO { y := 0
                                            rel &:= #b00110011
                                          }
  // rel=aabbccdd => don't care z, so set z=0 in this term
  UNLESS (rel>>1 XOR rel) & #b01010101 DO { z := 0
                                            rel &:= #b01010101
                                          }
  // [rel 1 y z] => [rel' 0 y z]
  IF x=1 DO rel, x := notx%rel, 0
  // [rel x 1 z] => [rel' x 0 z]
  IF y=1 DO rel, y := noty%rel, 0
  // [rel x y 1] => [rel' x y 0]
  IF x=1 DO rel, x := notx%rel, 0

  // [rel x y y] => [rel' x y 0]          rel abcdefgh->aaddeehh
  IF 0<y=z DO { rel :=  rel & #b10011001     |
                       (rel & #b10001000)>>1 |
                       (rel & #b00010001)<<1
                z := 0
              }
  // [rel x y x] => [rel' x y 0]          rel abcdefgh->aaccffhh
  IF 0<x=z DO { rel :=  rel & #b10100101     |
                       (rel & #b10100000)>>1 |
                       (rel & #b00000101)<<1
                z := 0
              }
  // [rel x x z] => [rel' x 0 z]          rel abcdefgh->ababghgh
  IF 0<x=y DO { rel :=  rel & #b11000011     |
                       (rel & #b11000000)>>2 |
                       (rel & #b00000011)<<2
                y := 0
              }

//  IF rel=#b00000001 AND x=y=z=0 DO { rel := -1; RETURN }

  // Finally, sort x y z

  IF x>y DO rel, x, y := swapxy%rel, y, x
  IF y>z DO { rel, y, z := swapyz%rel, z, y
              IF x>y DO rel, x, y := swapxy%rel, y, x
            }


FUN apply_unit_rules : ts =>
                       // This function applied the mapping (in
                       // vector ts!TsVm) to the terms in ts, and
                       // applies the unit rule until convergence,
                       // leaving the final mapping in the varmap.
  varmap := ts!TsVm

  { change := FALSE // changes to TRUE if the variable mapping changes

    IF debug>0 DO writef("\n   Applying unit rules\n")

    IF debug>3 DO { prmapping ts
                    prterms ts
                    newline()
                  }

    LET n = ts!TsTn
    LET p = ts!TsTerm1

    FOR i = 1 TO n DO { UNLESS !p<0 DO apply_unit_rule p
                        p := @p!Tsize
                      }
  } REPEATWHILE change // repeat if the mapping has changed

FUN apply_unit_rule
:  [ <0, x, y, z] => // A void term 

: p[rel, x, y, z] =>

    mapterm p

    IF rel<0 RETURN

    LET rel0 = rel

    LET pat = pattern(x, y, z)
    rel &:= pat

    IF rel=0 RAISE E_FalseTermFound

    IF rel=pat DO
    { IF debug>2 DO 
        writef("unit:  %8b %s %5d %5d %5d Pat %s => satisfied\n",
                       rel, relstr rel, x, y, z, relstr pat)
      rel := -1
      RETURN
    }

    LET acts = rel2act!rel - rel2act!pat

    WHILE acts DO                // Iterate over the actions
    { LET act = acts & -acts
      acts -:= act
      IF debug>2 DO
        writef("unit:  %8b %s %5d %5d %5d Pat %s => %s\n",
                       rel0, relstr rel0, x, y, z, relstr pat, actstr act)

      rel, change := -1, TRUE // All action change varmap 
                              // and remove the term.
      MATCH act
      : Ax0  =>               varmap!x :=  0        // Set x=0
      : Ax1  =>               varmap!x :=  1        // Set x=1
      : Ay0  =>               varmap!y :=  0        // Set y=0
      : Ay1  =>               varmap!y :=  1        // Set y=1
      : Az0  =>               varmap!z :=  0        // Set z=0
      : Az1  =>               varmap!z :=  1        // Set z=1

      : Aypz => TEST y>z THEN varmap!y :=  z        // Set y=z
                         ELSE varmap!z :=  y        // Set z=y
      : Azpx => TEST z>x THEN varmap!z :=  x        // Set z=x
                         ELSE varmap!x :=  z        // Set x=z
      : Axpy => TEST x>y THEN varmap!x :=  y        // Set x=y
                         ELSE varmap!y :=  x        // Set z=x
      : Aynz => TEST y>z THEN varmap!y := -z        // Set y=~z
                         ELSE varmap!z := -y        // Set z=~y
      : Aznx => TEST z>x THEN varmap!z := -x        // Set z=~x
                         ELSE varmap!x := -z        // Set x=~z
      : Axny => TEST x>y THEN varmap!x := -y        // Set x=~y
                         ELSE varmap!y := -x        // Set y=~x

      : act  => bug("  Unknown action %12b\n", act)
      .
      //IF debug>2 DO prterms curts
    }




FUN apply_pair_rules : ts =>

  IF debug>0 DO writes "\n   Applying pair rules\n"

  // Look for pairs of the form: [rel ,   x , y,  z ]
  //                             [rel',   x', y', z']
  // with z=z' and deduce whatever is possible.

  LET n = ts!TsTn
  LET k = 0

  LET v = getvec(3*n)  // Up to 3 variables per term
  UNLESS v RAISE E_Space

  IF ts!TsSortv DO freevec(ts!TsSortv)
  ts!TsSortv := v  // Save the sort vector in ts so that it can be freed

  LET p = ts!TsTerm1 
  IF p & 15 DO bug "Alignment error\n" // Check p is on a 16 byte boundary

  varmap := ts!TsVm

  FOR i = 1 TO n DO
  { UNLESS !p<0 DO { mapterm p
                     canonterm p
                     IF p!Tx DO v!++k := @p!Tx
                     IF p!Ty DO v!++k := @p!Ty
                     IF p!Tz DO v!++k := @p!Tz
                   }
    p := @p!Tsize
  }

//  prmapping ts
//  writef("Terms for sorting\n")
//  FOR i = 1 TO k DO prterm(v!i & -16)
//  newline()
  sort(v, k, cmpval)

//  writef("Terms for pair search\n")
//  FOR i = 1 TO k DO prterm(v!i & -16)

  IF debug>3 DO
  { writef "Sorted vars:"
    FOR i = 1 TO k DO writef(" %d", !(v!i))
    newline()
  }

  FOR i = 0 TO k-1 DO
  { MATCH @v!i
    : [[x], p[y(>2)], [z]] => IF i>0 AND x=y LOOP
                              IF i<k AND y=z LOOP
                              LET t = p & -16
                              IF debug>2 DO
                              { writef("Var %5d eliminated in: ", y)
                                prterm t
                              }
                              y := 2
                              mapterm t
                              change := TRUE
    :                      => LOOP
  }

  FOR i = 1 TO k DO
  { LET p = v!i & -16
    FOR j = i+1 TO k DO 
    { IF !p<0 BREAK         // Term p is has become void
      LET q = v!j & -16
      UNLESS pair_rule(p, q) BREAK // BREAK if terms p and q have
                                   // no variables in common.
    }      
  }

  freevec v

FUN pair_rule : p[rel, x, y, z], q[rel', x', y', z'] =>

    // Find out how many variable are in common and pass the terms
    // to pair_rule1, pair_rule2 or pair_rule3 and return TRUE.
    // If there are no variables in common, return FALSE.

    IF p=q OR rel<0 OR rel'<0 RETURN TRUE

    UNLESS rel AND rel' RAISE E_FalseTermFound

    // x, y, and z are all distinct, unless zero.

    // The possible equalities are
    // z=z' z=y' z=x' y=z' y=y' y=x' x=z' x=y' x=x'
    UNTIL z=z' MATCH q
    : [?,  ?, =z,  ?] => rel', y', z' := swapyz%rel', z', y'; BREAK 
    : [?, =z,  ?,  ?] => rel', x', z' := swapzx%rel', z', x'; BREAK 
    : [?,  ?,  ?, =y] => rel,  y,  z  := swapyz%rel,  z,  y;  BREAK 
    : [?,  ?, =y,  ?] => rel,  y,  z  := swapyz%rel,  z,  y
                         rel', y', z' := swapyz%rel', z', y'; BREAK 
    : [?, =y,  ?,  ?] => rel,  y,  z  := swapyz%rel,  z,  y
                         rel', x', z' := swapzx%rel', z', x'; BREAK 
    : [?,  ?,  ?, =x] => rel,  x,  z  := swapzx%rel,  z,  x;  BREAK 
    : [?,  ?, =x,  ?] => rel,  x,  z  := swapzx%rel,  z,  x
                         rel', y', z' := swapyz%rel', z', y'; BREAK 
    : [?, =x,  ?,  ?] => rel,  x,  z  := swapzx%rel,  z,  x
                         rel', x', z' := swapzx%rel', z', x'; BREAK 
    :                 => RETURN FALSE // No variables in common 
    .
    // z=z'

    // The possible equalities are
    // y=y' y=x' x=y' x=x'
    UNTIL y=y' MATCH q
    : [?, =y,  ?,  ?] => rel', x', y' := swapxy%rel', y', x'; BREAK 
    : [?,  ?, =x,  ?] => rel,  x,  y  := swapxy%rel,  y,  x;  BREAK 
    : [?, =x,  ?,  ?] => rel,  x,  y  := swapxy%rel,  y,  x
                         rel', x', y' := swapxy%rel', y', x'; BREAK 
    :                 => pair_rule1(p, q) // Only one var in common 
                         RETURN TRUE
    .
    // y=y' and z=z'

    // The possible equalities are
    // x=x'
    TEST x=x' THEN pair_rule3(p, q) // All three vars in common 
              ELSE pair_rule2(p, q) // Only two vars in common 
    RETURN TRUE


FUN pair_rule1 : p[rel, x, y, z], q[rel', x', y', z'] => RETURN

FUN pair_rule2 : p[rel, x, y, z], q[rel', x', y', z'] =>

    IF p=q OR rel<=0 OR rel'<=0  OR
        x=x'  OR   y~=y'  OR  z~=z' DO bug "Bug found in pair_rule2\n"


    IF debug>3 DO { writef("pair_rule2: "); prterm p
                    writef("      with: "); prterm q
                    newline()
                  }

    // rel = pppp_qqqq
    // a   = pppp_pppp_qqqq_qqqq
    LET a = #b0_0001_0001 * ((rel & #b1111_0000)<<4 |
                              rel & #b0000_1111)
    // rel' = rrrr_ssss
    // b    = rrrr_ssss_rrrr_ssss
    LET b = #b1_0000_0001 * rel'
    LET crel = a & b

    IF debug>3 DO writef("crel: %16b\n", crel)

    // All the information in the terms: [rel,  x,  y, z]
    //                                   [rel', x', y, z]
    // is now contained in:           [crel, x, x', y, z]

    // Let's see what this tells us...

    IF crel=0 RAISE E_FalseTermFound // The term is unsatisfiable

    LET w = crel | crel>>4         // calculate new rel and rel'
    rel := w>>4 & #b11110000 | w&#b1111

    rel' := (crel>>8 | crel) & #b1111_1111

    UNLESS crel & #b0000_1111_1111_0000 DO // x must= x' ?
    { IF debug>2 DO { writef("pair_rule2: "); prterm p
                      writef("      with: "); prterm q
                      writes "gives: Axpx'\n"
                     }
      TEST x>x' THEN { varmap!x  := x'; mapterm p }
                ELSE { varmap!x' := x;  mapterm q }
      change := TRUE
      pair_rule3(p, q)  // Since now x = x'
    }

    UNLESS crel & #b1111_0000_0000_1111 DO // x must= ~x' ?
    { IF debug>2 DO { writef("pair_rule2: "); prterm p
                      writef("      with: "); prterm q
                      writes "gives: Axnx'\n"
                     }
      TEST x>x' THEN { varmap!x  := -x'; mapterm p }
                ELSE { varmap!x' := -x;  mapterm q }
      change := TRUE
      pair_rule3(p, q)  // Since now x = x'
    }

    apply_unit_rule p
    apply_unit_rule q


FUN pair_rule3 : p[rel, x, y, z], q[rel', x', y', z'] =>

    IF p=q OR rel<=0 OR rel'<=0 OR
        x~=x' OR  y~=y'  OR z~=z' DO bug "Bug found in pair_rule3\n"

    IF debug>2 AND rel' ~= rel&rel' DO
    { writef("pair_rule3: "); prterm p
      writef("      with: "); prterm q
    }

    rel' &:= rel

    IF debug>2 AND rel' ~= rel&rel' DO
    {  writef("     gives: "); prterm q
    }

    rel := -1

    apply_unit_rule q



//********************* Sort Function ******************

STATIC cmpfn  // cmpfn(p, q)=TRUE iff term p < term q

FUN cmpval : [<z], [z] => TRUE
           :           => FALSE

FUN cmpfull : p[ r, x, y, z], q[r', x', y', z'] =>
//  writef "Cmpfull: "; prterm p
//  writef "   with: "; prterm q
  IF z<z' RETURN TRUE
  IF z>z' RETURN FALSE
  IF y<y' RETURN TRUE
  IF y>y' RETURN FALSE
  IF x<x' RETURN TRUE
  IF x>x' RETURN FALSE
  IF r<r' RETURN TRUE
  IF r>r' RETURN FALSE
  RETURN TRUE

FUN sort : v, n, f => cmpfn := f
                      qsort(@v!1, @v!n)

FUN qsort : l, r =>
  WHILE @l!8<r DO
  { LET midpt = ((l+r)/2) & -Bpw
    // Select a good(ish) median value.
    LET val   = middle(!l, !midpt, !r)
    LET p = partition(val, l, r)
    // Only use recursion on the smaller partition.
    TEST p>midpt THEN { qsort(p, r);     r := @p!-1 }
                 ELSE { qsort(l, @p!-1); l := p     }
   }
   FOR p = @l!1 TO r BY Bpw DO  // Now perform insertion sort.
     FOR q = @p!-1 TO l BY -Bpw DO
         TEST cmpfn(q!0,q!1) THEN BREAK
                             ELSE q!0, q!1 := q!1, q!0


FUN middle : a, b, c => cmpfn(a,b) -> cmpfn(b,c) -> b,
                                      cmpfn(a,c) -> c,
                                                    a,
                        cmpfn(b,c) -> cmpfn(a,c) -> a,
                                                    c,
                        b

FUN partition : median, p, q =>
{  WHILE cmpfn(!p, median) DO p+++
   WHILE cmpfn(median, !q) DO q---
   IF p>=q RETURN p
   !p, !q := !q, !p
   p+++
   q---
} REPEAT



//********************* Syntax Analyser ******************

// This converts the ASCII representation of a propositional
// expression into a set of terms, each of the form
//        [op, x, y, z]
// where x, y, and z are variable ids
// It returns the id of the root of the entire expression.

// 0 .. 1 -->  0 .. 1
// A .. Z -->  2, 4, 6, ..., 52
// ~x     -->  [NotY, t, x, 0]
// x & y  -->  [And,  t, x, y]
// x | y  -->  [Or,   t, x, y]
// x -> y -->  [Imp,  t, x, y]
// x = y  -->  [Eqv,  t, x, y]


STATIC
  strp, ch, nch, token, lexval

FUN lex_init : str => strp := str; rch(); rch()

FUN rch : => ch, nch := nch, %strp
             UNLESS nch=0 DO strp++

FUN lex : => MATCH (ch, nch)

: ' ' | '\n' => rch(); lex()

: '0'..'1'   => token, lexval := Id, ch-'0';       rch() // 0->0 1->1
: 'A'..'Z'   => token, lexval := Id, 2*(ch-'A')+4; rch() // A->4 B->6 ...
: '('        => token := Lparen; rch()
: ')'        => token := Rparen; rch()
: '~'        => token := NotY;   rch()
: '&'        => token := And;    rch()
: '|'        => token := Or;     rch()
: '='        => token := Eqv;    rch()

: '-', '>'   => token := Imp; rch(); rch()

: 0          => token := Eof

:            => RAISE E_syntax

STATIC
  varn=0, lasteid=0

FUN parse : str, ts =>
  lex_init str
  varn  := 1       // Generated  ids are odd,  next available is 3
  lasteid := 20     // Expression ids are even, next available is 4
                   // Remember: 0 = False
                   //           1 = True
                   //           2 = Don't care, does not matter which
                   //           even>2 -- user variable
                   //           odd>1  -- system generated variable
  termp := ts!TsTerm1
  termt := ts!TsTermt
  termcount := 0

  pushterm(Eqv, 1, nexp 0, 0) // Parse the given expression
                              // test if it is FALSE

  IF varn<lasteid DO varn := lasteid

  IF ts!TsVm DO freevec(ts!TsVm)       // Free previous vm, if any
  ts!TsVm    := 0                      // No varmap vector
  ts!TsVmax  := varn                   // Maximum variable identifier used
  ts!TsTn    := termcount              // the number of terms
  IF ts!TsSortv DO freevec(ts!TsSortv) // Free previous sort vector, if any
  ts!TsSortv := 0                      // No sort vector yet
  curts := ts


FUN prim
: => MATCH token
     : Id     => LET a = lexval  // 0->0 1->1 A->4 B->6 C->8 ...
                 IF lasteid<a DO lasteid := a
                 lex()
                 a
     : Lparen => LET a = nexp 0
                 UNLESS token=Rparen RAISE(E_syntax, "')' missing\n")
                 lex()
                 a
     : NotY   => mkterm(NotY, nexp 3, 0)
     :  ?     => RAISE(E_syntax, "Bad expression\n")

FUN nexp : n => lex(); exp n

FUN exp : n =>
  LET a = prim()

  MATCH (token, n)
  : And, <3 => a := mkterm(And, a, nexp 3) 
  : Or,  <2 => a := mkterm(Or , a, nexp 2) 
  : Imp, <1 => a := mkterm(Imp, a, nexp 1) 
  : Eqv, <1 => a := mkterm(Eqv, a, nexp 1) 
  :         => RETURN a
  . REPEAT


//********************* Main Program **********************


FUN try : e => 
  { writef("\nTesting: %s\n", e)

    parse(e, curts)  // Puts the terms representing expression e
                       // into the given term set, and updates its
                       // vmax field.

    LET mess = check curts
    compact_terms curts HANDLE : => .
    IF debug>0 DO prterms curts

    writef("-------- %s\n", mess)

  } HANDLE : E_syntax, mess, a => writef(mess, a)
           : E_Space,  mess, a => writef(mess, a)

// Propositional examples supplied by Larry Paulson 
// and modified by MR

STATIC debug=0, tmax=0

FUN start : =>
    LET argv = VEC 50

    IF rdargs("D,TMAX,TO/K", argv, 50)=0 DO
    { writef "Bad arguments for STLMK\n"
      RETURN 20
    }

    LET sysout = output
    LET out    = 0

    debug := 0
    IF argv!0 DO debug := str2numb(argv!0) // Set the debugging level

    tmax := 100000
    IF argv!1 DO tmax := str2numb(argv!1) // Set max number of terms

    IF argv!2 DO { out := findoutput(argv!2)
                   IF out=0 DO
                   { writef "Bad arguments for STLMK\n"
                     RETURN 20
                   }
                   selectoutput(out)
                 }

    curts := alloc_termset tmax
    init_tabs()

    writef "\nAssociative laws of & and |\n"
    try "(P & Q) & R  =  P & (Q & R)"
    try "(P | Q) | R  =  P | (Q | R)"

    writef "\nDistributive laws of & and |\n"
    try "(P & Q) | R  = (P | R) & (Q | R)"
    try "(P | Q) & R  = (P & R) | (Q & R)"

    writef "\nLaws involving implication\n"
    try "(P|Q -> R) = (P->R) & (Q->R)"
    try "(P & Q -> R) = (P-> (Q->R))"
    try "(P -> Q & R) = (P->Q)  &  (P->R)"

    writef "\nClassical theorems\n"
    try "P | Q  ->  P | ~P & Q"
    try "(P->Q)&( ~P->R)  ->  (P&Q | R)"
    try "P & Q | ~P & R =  (P->Q) & (~P->R)"
    try "(P->Q) | (P->R) = (P -> Q | R)"
    try "(P = Q) = (Q = P)"

    /* Sample problems from F.J. Pelletier,
       Seventy-Five Problems for Testing Automatic Theorem Provers,
       J. Automated Reasoning 2 (1986), 191-216.
    */

    writef "\nProblem 5\n"
    try "((P|Q)->(P|R)) -> (P|(Q->R))"

    writef "\nProblem 9\n"
    try "((P|Q) & ( ~P | Q) & (P | ~Q)) ->  ~( ~P | ~Q)"

    writef "\nProblem 12.  Dijkstra's law\n"
    try "((P  =  Q)  =  R)  ->  (P  =  (Q  =  R))"

    writef "\nProblem 17\n"
    try "(P & (Q->R) -> S) = (( ~P | Q | S) & ( ~P | ~R | S))"

    writef "\nFALSE GOALS\n"
    try "(P | Q -> R) = (P -> (Q->R))"
    try "(P->Q) = (Q ->  ~P)"
    try " ~(P->Q) -> (Q = P)"
    try "((P->Q) -> Q)  ->  P"
    try "((P | Q) & (~P | Q) & (P | ~Q)) ->  ~(~P | Q)"

    writef "\nIndicates need for subsumption\n"
    try "((P & (Q = R)) = S) = (( ~P | Q | S) & ( ~P | ~R | S))"

// Prove that the circuit
//      -----
// X --| NOT |--> Y
//      -----
// is equivalent to:
//      -----       -----       -----
// A --| NOT |--B--| NOT |--C--| NOT |--> D
//      -----       -----       -----
    writef "\nProof of the correctness of a circuit\n"
    try "(Y=~X) & ((D=~C) & (C=~B) & (B=~A)) & (X=A)  ->  (Y=D)"

    writef "\nSuffix rule test\n"
    try "((X=A)->B) & (~(A=X)|B)"

    close_tabs()
    close_termset curts
    IF out DO endwrite out
    RETURN 0

