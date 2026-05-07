/*
(c) Martin Richards  13 November 1997

   This is an algorithm for tautology checking implemented in MCPL
   by M. Richards. It is related to Stalmarck's patented algorithm 
   described in the paper:

   G. Stalmarck and M. Saflund, Modelling and Verifying Systems and
   Software in Propositional Logic, IFAC, SAFECOMP90, London, UK 1990

   It also has features similar to those found in:

   Kunz,W. and Stoffel,D. Reasoning in Boolean Networks
   Kluwer Academic Publishers, 1997

The program is implemented in an object oriented style, with this
module implementing the Rn object which deals with relational terms over
n variables. This file implements the case n=3. Its methods are as follows:

Rn_Init#(r, tmax)

    Initialise the Rn object with space for tmax terms. Removing
    any previous set of terms if it existed.

Rn_Close#(r)

    Close down the Rn object, by freeing all its work space, but
    not its code.

Rn_AddTerm3#(r, rel, x, y, z)

    Add the 3 variable term [rel, x, y, z] to the current set of terms.

Rn_AddTerm4#(r, rel, v3, v2,v1,v0)
Rn_AddTerm5#(r, rel, v4, v3, v2,v1,v0)
Rn_AddTerm8#(r, r7,r6,r5,r4,r3,r2,r1,r0, v7,v6,v5,v4,v3,v2,v1,v0)

    Add terms of other sizes to the term set.

Rn_PrTerms#(r)

    Print the current set of terms.

Rn_PrMapping#(r)

    Print the current variable mapping.

Rn_Compact#(r)

    Compact the set of terms.

Rn_Check#(r)

    Apply the checking algorithm to the set of terms in r.
    It returns:    0 if error occured
                   1 if the terms are satisfiable
                   2 if they are not satisfiable
                   3 if unable to decide

Rn_Debug#(r, n)

    Set the Rn debug level to n.
*/

GET "../../MCPL/mcplprogs/chk.h"

STATIC rnfns = [ rnfnInit,
                 rnfnClose,
                 rnfnAddTerm3,
                 rnfnAddTerm4,
                 rnfnAddTerm5,
                 rnfnAddTerm8,
                 rnfnPrTerms,
                 rnfnPrMapping,
                 rnfnCompact,
                 rnfnUnit,
                 rnfnPair,
                 rnfnCheck
               ]

FUN mkRnobj : => LET rn = TABLE [0, 0, 0]
                 !rn := rnfns
                 RETURN rn

STATIC last_given_tmax = 100000

FUN rnfnInit      : r, tmax =>
  IF tmax DO last_given_tmax := tmax
  IF curts DO rnfnClose r
  curts := alloc_termset last_given_tmax
  init_tabs()

FUN rnfnClose     : r =>
  close_tabs()
  close_termset curts  /// ???
  curts := 0

FUN rnfnAddTerm3  : r, rel, v2,v1,v0 =>
  pushterm(rel, v2, v1, v0)

FUN rnfnAddTerm4  : r, rel, v3,v2,v1,v0 =>
  bug "fnAddTerm4 not implemented\n"

FUN rnfnAddTerm5  : r, rel, v4,v3,v2,v1,v0 =>
  bug "fnAddTerm5 not implemented\n"

FUN rnfnAddTerm8  : r, r7,r6,r5,r4,r3,r2,r1,r0,
                       v7,v6,v5,v4,v3,v2,v1,v0 =>
  bug "fnAddTerm8 not implemented\n"

FUN rnfnPrTerms   : r =>
  prterms curts

FUN rnfnPrMapping : r =>
  prmapping curts

FUN rnfnCompact   : r =>
{ compact_terms curts
  compact_vars  curts

  RETURN 3
} HANDLE : E_FalseTermFound => 2
         : E_NoTerms        => 1
         : E_Space          => 0

FUN rnfnUnit     : rn =>
{ apply_unit_rules curts

  RETURN 3
} HANDLE : E_FalseTermFound => 2
         : E_NoTerms        => 1
         : E_Space          => 0

FUN rnfnPair     : rn =>
{ apply_pair_rules curts

  RETURN 3
} HANDLE : E_FalseTermFound => 2
         : E_NoTerms        => 1
         : E_Space          => 0

FUN rnfnCheck     : rn =>
  check curts

FUN rnfnDebug     : rn, n => debug := n



MANIFEST 

Id, Lparen, Rparen,Eof  // Lexical tokens

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

FUN rel2str
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
:        -1=>"deleted"
:          =>"       "

MANIFEST
  E_Space=101,        // Exceptions
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
//writef("pattern: %5d %5d %5d   %8b %s\n", x, y, z, res, rel2str res)
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
         rel2act RAISE (E_Space, "in init_tabs\n")

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
  { LET w = i                                 //    [w x y z] = [i x y z]
    writef("%8b %s  ", w, rel2str w)          // ie [w x y z] = [i x y z]
    writef("%8b %s  ", notx%w, rel2str (notx%w))
    writef("%8b %s  ", noty%w, rel2str (noty%w))
    writef("%8b %s\n", notz%w, rel2str (notz%w))
    w := swapyz%i                             //    [w x z y] = [i x y z]
    writef("%8b %s  ", w, rel2str w)          // ie [w x y z] = [i x z y]
    writef("%8b %s  ", notx%w, rel2str (notx%w))
    writef("%8b %s  ", noty%w, rel2str (noty%w))
    writef("%8b %s\n", notz%w, rel2str (notz%w))
    w := swapxy%i                             //    [w y x z] = [i x y z]
    writef("%8b %s  ", w, rel2str w)          // ie [w x y z] = [i y x z]
    writef("%8b %s  ", notx%w, rel2str (notx%w))
    writef("%8b %s  ", noty%w, rel2str (noty%w))
    writef("%8b %s\n", notz%w, rel2str (notz%w))
    w := swapyz%(swapxy%i)                    //    [w y z x] = [i x y z]
    writef("%8b %s  ", w, rel2str w)          // ie [w x y z] = [i z x y]
    writef("%8b %s  ", notx%w, rel2str (notx%w))
    writef("%8b %s  ", noty%w, rel2str (noty%w))
    writef("%8b %s\n", notz%w, rel2str (notz%w))
    w := swapxy%(swapyz%i)                    //    [w z x y] = [i x y z]
    writef("%8b %s  ", w, rel2str w)          // ie [w x y z] = [i y z x]
    writef("%8b %s  ", notx%w, rel2str (notx%w))
    writef("%8b %s  ", noty%w, rel2str (noty%w))
    writef("%8b %s\n", notz%w, rel2str (notz%w))
    w := swapzx%i                             //    [w z y x] = [i x y z]
    writef("%8b %s  ", w, rel2str w)          // ie [w x y z] = [i z y x]
    writef("%8b %s  ", notx%w, rel2str (notx%w))
    writef("%8b %s  ", noty%w, rel2str (noty%w))
    writef("%8b %s\n", notz%w, rel2str (notz%w))
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

STATIC   curts=0,  // The current termset
         sortvec   // For registering the sort vector

                  // Termset fields
MANIFEST TsVm,    // varmap vector, if allocated
         TsVmax,  // largest variable id in use
         TsTerm1, // Pointer to the first term (on a 16 byte boundary)
         TsTermp, // Pointer to the next free term position
         TsTermt, // Pointer to the last possible term position
         TsTn,    // Actual number of terms in the set
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
  ts!TsTermp := t
  ts!TsTermt := @t!(Tsize*tmax)
  IF @ts!tsupb < @t!(Tsize*(tmax+1)) DO // Check that there is room
     bug("alloc_termset error")         // for tmax terms

  ts!TsTn    := 0           // No terms in the set yet

  RETURN ts

FUN mkcopy_termset : ts =>
  LET n = ts!TsTn
  LET nts = alloc_termset n // Allocate a termset of the right size
  UNLESS nts RAISE (E_Space, "in mkcopy_termset\n")
  LET p =  ts!TsTerm1
  LET q = nts!TsTerm1

  FOR i = 1 TO n MATCH (p, q)
  : [r,x,y,z,np], pp[:=r,:=x,:=y,:=z, nq] => p, q := @np, @nq
  .
  LET vmax = ts!TsVmax

  nts!TsVm   := 0
  nts!TsVmax := vmax
  nts!TsTn   := n

  IF vmax DO { LET vm = getvec vmax
               UNLESS vm RAISE (E_Space, "in mkcopy_termset\n")
               nts!TsVm := vm
               FOR i = 0 TO vmax DO vm!i := 0
             }

  RETURN nts

FUN close_termset : ts =>

  IF ts!TsVm DO freevec(ts!TsVm)
  IF sortvec DO { freevec sortvec  // Free the sort vector, if allocated
                  sortvec := 0
                }
  freevec ts


FUN pushterm : rel, x, y, z => 
  MATCH curts : [vm, vmax, term1, termp,termt,n] =>

  IF termp>termt RAISE (E_Space, "Too many terms\n")

  UNLESS TsVmax=1 DO abort 999
  EVERY curts : [?, <x :=x] =>  // Update TsVmax field
              : [?, <y :=y] =>  // if a larger variable
              : [?, <z :=z] =>  // id is encountered.
              .

  MATCH termp : [:=rel,:=x,:=y,:=z, np] => n++
                                           IF debug>=9 DO prterm termp
                                           termp := @np


// Variables such as x, y and z in terms [rel,x,y,z] are identified
// by positive integers. Variables may get renamed by such actions
// as: Azpx (set x=z) or Axny (set x=~y). The accumulated collection
// of mappings is held in the current mapping vector varmap.

// varmap!x =  0        =>  nothing known about x
//          =  1        =>  x-> 1         (true)
//          = -1        =>  x-> 0         (false)
//          =  2        =>  marker saying x is only used in one term
//                          and so can freely be set to either value.
//          =  y (y>2)  =>  x-> y
//          = -y (y>2)  =>  x->~y



STATIC
  varmap=0   // base of the current varmap vector

FUN compact_vars : ts =>
  IF ts!TsVm DO{ freevec(ts!TsVm)  // Free old mapping vector, if allocated
                 ts!TsVm := 0
               }

  LET vmax = ts!TsVmax
  LET vm = getvec vmax
  UNLESS vm RAISE(E_Space, "Can't allocate a varmap vector size %d\n", vmax)

  LET n = ts!TsTn
  LET p = ts!TsTerm1

  FOR i = 0 TO vmax DO vm!i := 0
  FOR i = 1 TO n MATCH p            // Form histogram of variable uses
  : [rel, x, y, z, np] => (vm!x)++
                          (vm!y)++
                          (vm!z)++
                          p := @np
  .

  LET newvmax = 2                   // Construct the renaming mapping
  vm!0 := 0  // False
  vm!1 := 1  // true
  vm!2 := 2  // Don't-care marker

  FOR i = 3 TO vmax MATCH vm!i
                    : 0 =>                    // Never used
                    : 1 => vm!i := 2          // Don't-care marker
                    :   => vm!i := ++newvmax  // give it next id
                    .
  p := ts!TsTerm1

  FOR i = 1 TO n MATCH p            // Rename variables
  : [rel,x(:=vm!x),(y:=vm!y),z(:=vm!z), np] => p := @np
  .

  ts!TsVmax := newvmax

  freevec vm             // Free the old mapping vector
  vm := getvec newvmax   // and allocate one of the new size.
  UNLESS vm RAISE(E_Space, "Can't allocate a varmap vector size %d\n", vmax)
  ts!TsVm := vm
  FOR i = 0 TO newvmax DO vm!i := 0 // Set it to the null mapping.

FUN compact_terms : ts => // remove void or duplicate terms

  mark_duplicates ts      // mark duplicates as void

  LET n = ts!TsTn         // Number of terms
  LET p = ts!TsTerm1      // Next term to look at
  LET k = 0               // Count of remaining terms
  LET q = p               // Next free term position

  FOR i = 1 TO n MATCH (p, q)
  : [ <0, x, y, z, np], ? => IF debug>=8 DO { writes "void:  "; prterm p }
                             p := @np // Skip void term

  : [rel, x, y, z, np],
    [rel',x',y',z',nq]    => k++      // Preserve non-void term
                             UNLESS p=q DO rel',x',y',z' := rel,x,y,z
                             p, q := @np, @nq
  .
  ts!TsTn := k
  IF debug>=8 AND k<n DO { writef("Compacted to %d terms\n", k)
                           prterms ts
                         }
  UNLESS k DO { writef "No Terms left\n"
                RAISE E_NoTerms
              }


FUN mark_duplicates : ts =>        // Mark duplicates as deleted

  IF sortvec DO { freevec sortvec  // Free sort vector, if allocated
                  sortvec := 0
                }
  LET n = ts!TsTn
  LET k = 0                        // Count of non void terms
  LET v = getvec n                 // Allocate sort vector
  IF v=0 RAISE (E_Space, "in mark_duplicates\n")
  sortvec := v                     // Register sort vector

  LET p = ts!TsTerm1

  FOR i = 1 TO n DO    // Put non void terms in the sort vector
  { UNLESS !p<0 DO { canonterm p; v!++k := p }
    p := @p!Tsize
  }

  UNLESS k DO { writef "No Terms left\n"
                RAISE E_NoTerms
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
                : [rel,x,y,z], [=rel,=x,=y,=z] => 
                         IF debug>=8 DO { writes "dupl:  "; prterm p }
                                                  rel := -1
                :                              => LOOP
  }

  //writef("Terms after marking duplicates\n")
  //FOR i = 1 TO k DO prterm(v!i)

  freevec v  // Free the sort vector
  sortvec := 0

FUN prterm : p[rel, x, y, z] =>
  writef("%5d: %8b %s %5d %5d %5d\n",
         (p-curts!TsTerm1)/(4*Tsize), rel, rel2str rel, x,  y,  z)

FUN prterms : ts =>

  LET n = ts!TsTn     // The number of terms
  LET p = ts!TsTerm1

  writef("Terms:\n")

  FOR i = 1 TO n DO { IF !p>=0 DO prterm p
                      p := @p!Tsize
                    }
  prmapping ts

FUN prmapping : ts =>
  LET k = 1         // For layout
  LET vm = ts!TsVm
  writef "Mapping:"
  UNLESS vm DO { writef " No mapping vector\n"; RETURN }
  FOR i = 0 TO ts!TsVmax IF vm!i DO
  { UNLESS k++ MOD 8 DO newline()
    LET y = vm!i
    IF y=-1 DO y := 0
    writef(" %d%c%d", i, (y<0->'#','='), ABS y)
  }
  newline()


FUN check : ts => // It may raise E_FalseTermFound
                  //              E_NoTerms
                  //              E_Space
{ compact_vars ts
  IF debug>=6 DO prterms ts
  apply_simple_rules  ts
  compact_terms ts
  compact_vars ts
  writef("Before Dilemma rule 1  Terms/vars: %5d/%5d\n",
          ts!TsTn, ts!TsVmax)
  apply_dilemma_rule1 ts
  compact_terms ts
  compact_vars ts
  writef("Before Dilemma rule 2  Terms/vars: %5d/%5d\n",
          ts!TsTn, ts!TsVmax)
  apply_dilemma_rule2 ts
  compact_terms ts
  compact_vars ts
  writef("Before Dilemma rule 3  Terms/vars: %5d/%5d\n",
          ts!TsTn, ts!TsVmax)
  apply_dilemma_rule3 ts
  compact_terms ts
  compact_vars ts
  writef("Before Dilemma rule 4  Terms/vars: %5d/%5d\n",
          ts!TsTn, ts!TsVmax)

  RETURN 3
} HANDLE : E_FalseTermFound => 2
         : E_NoTerms        => 1
         : E_Space          => 0

FUN combine_maps : vm, vm0, vm1, n =>
  LET flag = FALSE
  FOR i = 3 TO n DO { vm!i := vm0!i=vm1!i -> vm0!i, 0
                      IF vm!1 DO flag := TRUE
                    }
  IF debug=0 RETURN
  writef "\ncombine_maps:\n"
  FOR i = 3 TO n IF vm!i OR debug>=10 DO
                    writef("%5d: %5d %5d %5d\n", i, vm!i, vm0!i, vm1!i)
  newline()
  IF flag AND debug>=2 DO abort 333

FUN apply_dilemma_rule1 : ts =>
{ //compact_terms ts
  //compact_vars  ts  // Leaves ts!TsVm allocated

  LET vmax = ts!TsVmax
  varmap   = ts!TsVm
  UNLESS ts!TsVm DO bug "varmap not allocated\n"

  IF debug>=8 DO { writes "    Apply dilemma rule 1 to:\n"
                   prterms ts
                 }

  FOR v = 3 TO vmax UNLESS 0 <= mapof v <= 2 DO
  { curts := mkcopy_termset ts  // For variables not already set to 0,1 or 2

    LET vm0 = curts!TsVm
    varmap := vm0
    UNLESS varmap DO bug "varmap should be allocated\n"

    varmap!v := -1

    IF debug>=5 DO { writef("\nDilemma rule1 setting variable %d to 0\n\n", v)
                     IF debug>=6 DO prterms curts
                   }

    apply_simple_rules curts
    HANDLE : E_FalseTermFound => freevec vm0
                                 vm0 := 0
                                 curts!TsVm := 0
                  IF debug>=5 DO 
                    writef("####### setting %d to 0 inconsistent\n", v)
           : E_NoTerms        => // All terms have been satisfied
                                 RAISE E_NoTerms
           .

    IF debug>=6 DO prmapping curts
    curts!TsVm := 0 // So that this varmap does not get deallocated
    close_termset curts

    curts := mkcopy_termset ts
    LET vm1 = curts!TsVm
    varmap := vm1
    UNLESS varmap DO bug "varmap should be allocated\n"

    varmap!v := 1
    IF debug>=5 DO { writef("\nDilemma rule1 setting variable %d to 1\n\n", v)
                     IF debug>=6 DO prterms curts
                   }

    apply_simple_rules curts
    HANDLE : E_FalseTermFound => freevec varmap
                                 vm1 := 0
                                 curts!TsVm := 0
                  IF debug>=5 DO 
                    writef("####### setting %d to 1 inconsistent\n", v)
           : E_NoTerms        => // All terms have been satisfied
                                 RAISE E_NoTerms
           .

    IF debug>=6 DO prmapping curts
    curts!TsVm := 0 // So that this varmap does not get deallocated
    close_termset curts

    curts := ts
    varmap := ts!TsVm

    
    // Now form the intersection of the two variable mappings
    // and combine with the mapping for ts.
    TEST vm0
    THEN TEST vm1 THEN { combine_maps(varmap, vm0, vm1, ts!TsVmax)
                         freevec vm0
                         freevec vm1
                       }
                  ELSE { freevec varmap
                         varmap := vm0
                         ts!TsVm := varmap
                       }
    ELSE TEST vm1 THEN { freevec varmap
                         varmap := vm1
                         ts!TsVm := varmap
                       }
                  ELSE { IF debug>=5 DO
                           writef "Both alternatives are inconsistent\n"
                         RAISE E_FalseTermFound
                       }


    IF debug>=7 DO { writef "Intersected mapping is:\n"
                     prmapping ts
                   }

    apply_simple_rules ts
    //compact_terms ts
    // BEWARE: Do not compact the variables
    IF debug>7 DO { writes "\NTerms after applying Dilemma Rule 1:\n"
                    prterms ts 
//abort 666
                  }
//IF v>3 DO { writef "Aborting dilemma rule early\n"; BREAK }
  }

  close_termset curts
  curts := ts
}

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

  IF debug>=8 DO writef "\nApplying simple rules\n"

  varmap := ts!TsVm
  UNLESS varmap DO bug "Found in apply_simple_rules\n"

  IF debug>=7 DO { writef "Terms before applying simple rules:\n"
                   prterms ts
                 }

  change := FALSE

  { { change := FALSE
      apply_unit_rules ts
    //compact_terms ts
    } REPEATWHILE change

    IF debug>=8 DO prterms ts

    apply_pair_rules ts
    compact_terms ts
    IF ts!TsTn=0 RAISE E_NoTerms
    IF debug>=8 DO prterms ts

  } REPEATWHILE change

FUN mapterm : [rel, x, y, z] => // On entry, x, y and z >=0
                                // On return, x =0 or >2,
                                //            y =0 or >2
                                //        and z =0 or >2
                                // It may change rel.
                                // It raises no exceptions.

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
  UNLESS varmap DO bug "in mapof\n"

// varmap!x =  0        =>  nothing known about x
//          =  1        =>  x-> 1         (true)
//          = -1        =>  x-> 0         (false)
//          =  2        =>  marker saying x is only used in one term
//                          and so can freely be set to either value.
//          =  y (y>2)  =>  x-> y
//          = -y (y>2)  =>  x->~y

  MATCH varmap!x
  :    0  => RETURN x        // nothing known about x
  :   -1  => RETURN 0       // x-> 0
  :    1  => RETURN 1       // x-> 1
  :    2  => RETURN 2       // don't-care marker
  : y(>2) => y := mapof y
             varmap!x := y=0 -> -1, y
             RETURN y
  : y     => y := -mapof(-y)
             IF y= 0 DO y := 1
             IF y=-1 DO y := 0
             IF y=-2 DO y := 2
             varmap!x := y=0 -> -1, y
             RETURN y

FUN canonterm              // This canonicalizes a given term. It is
                           // guaranteed not to raise an exception.

: [ <0, ?, ?, ?] => RETURN // A void term

:p[rel, x, y, z] =>
  IF debug>=8 DO { writes "Canon: "; prterm p }
  mapterm p
  // x =0 or >2, y =0 or >2  and z =0 or >2

  // rel=abcdabcd => x ignored, so set x=0 and change rel
  UNLESS (rel>>4 XOR rel) & #b00001111 DO { x := 0
                                            rel &:= #b00001111
                                          }
  // rel=ababcdcd => y ignored, so set y=0 and change rel
  UNLESS (rel>>2 XOR rel) & #b00110011 DO { y := 0
                                            rel &:= #b00110011
                                          }
  // rel=aabbccdd => z ignored, so set z=0 and change rel
  UNLESS (rel>>1 XOR rel) & #b01010101 DO { z := 0
                                            rel &:= #b01010101
                                          }

  IF x=1 DO rel, x := notx%rel, 0  // [rel 1 y z] => [rel' 0 y z]
  IF y=1 DO rel, y := noty%rel, 0  // [rel x 1 z] => [rel' x 0 z]
  IF z=1 DO rel, z := notz%rel, 0  // [rel x y 1] => [rel' x y 0]

                                   // [rel x y y] => [rel' x y 0]
  IF 0<y=z DO { rel :=  rel & #b10011001     |  // rel  abcdefgh
                       (rel & #b10001000)>>1 |  // ->
                       (rel & #b00010001)<<1    // rel' aaddeehh
                z := 0
              }
                                   // [rel x y x] => [rel' x y 0]
  IF 0<x=z DO { rel :=  rel & #b10100101     |  // rel  abcdefgh
                       (rel & #b10100000)>>1 |  // ->
                       (rel & #b00000101)<<1    // rel' aaccffhh
                z := 0
              }
                                   // [rel x x z] => [rel' x 0 z]
  IF 0<x=y DO { rel :=  rel & #b11000011     |  // rel  abcdefgh
                       (rel & #b11000000)>>2 |  // ->
                       (rel & #b00000011)<<2    // rel' ababghgh
                y := 0
              }

  // Finally, sort x y z

  IF x>y DO {           rel, x, y := swapxy%rel, y, x
            }
  IF y>z DO {           rel, y, z := swapyz%rel, z, y
              IF x>y DO rel, x, y := swapxy%rel, y, x
            }

  IF x=0 DO rel &:= #b00001111
  IF y=0 DO rel &:= #b00110011
  IF z=0 DO rel &:= #b01010101

  IF debug>=8 DO { writes "=====> "; prterm p }


FUN apply_unit_rules : ts =>
                       // This function applied the mapping (in
                       // vector ts!TsVm) to the terms in ts, and
                       // applies the unit rule until convergence,
                       // leaving the final mapping in the varmap.
  varmap := ts!TsVm

  { change := FALSE // changes to TRUE if the variable mapping changes

    IF debug>=7 DO writef("\n   Applying unit rules\n")

    IF debug>=7 DO { prmapping ts
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
:  [ <0, x, y, z] => // A deleted term 

: p[rel, x, y, z] =>

{   canonterm p

    IF rel<0 AND debug>=5 DO { writef("void:  "); prterm p }

    IF rel<0 RETURN

    LET pat = pattern(x, y, z)
    IF debug>=5 AND rel~=rel&pat DO 
    { writef("unit:  "); prterm p
      rel &:= pat
      writef("   =>  "); prterm p
    }
    rel &:= pat

    IF rel=0 DO { IF debug>=5 DO { writef "unsat: "; prterm p }
                  RAISE E_FalseTermFound
                }

    IF rel=pat DO
    { IF debug>=5 DO { writef("del:   "); prterm p }
      rel := -1
      RETURN
    }

    LET acts = rel2act!rel - rel2act!pat

    UNLESS acts RETURN

    change := TRUE // All action change varmap 

    IF debug>=5 DO { writef("unit:  "); prterm p }

    WHILE acts DO                // Iterate over the actions
    { LET act = acts & -acts
      acts -:= act
      IF debug>=5 DO writef("   =>  %s\n", actstr act)

      MATCH act
      : Ax0  =>               varmap!x :=  -1       // Set x=0
      : Ax1  =>               varmap!x :=  1        // Set x=1
      : Ay0  =>               varmap!y :=  -1       // Set y=0
      : Ay1  =>               varmap!y :=  1        // Set y=1
      : Az0  =>               varmap!z :=  -1       // Set z=0
      : Az1  =>               varmap!z :=  1        // Set z=1

      : Aypz => UNLESS y AND z DO bug "in apply__unit_rule\n"
                TEST y>z THEN varmap!y :=  z        // Set y=z
                         ELSE varmap!z :=  y        // Set z=y
      : Azpx => UNLESS x AND z DO bug "in apply__unit_rule\n"
                TEST z>x THEN varmap!z :=  x        // Set z=x
                         ELSE varmap!x :=  z        // Set x=z
      : Axpy => UNLESS x AND y DO bug "in apply__unit_rule\n"
                TEST x>y THEN varmap!x :=  y        // Set x=y
                         ELSE varmap!y :=  x        // Set z=x
      : Aynz => UNLESS y AND z DO bug "in apply__unit_rule\n"
                TEST y>z THEN varmap!y := -z        // Set y=~z
                         ELSE varmap!z := -y        // Set z=~y
      : Aznx => UNLESS x AND z DO bug "in apply__unit_rule\n"
                TEST z>x THEN varmap!z := -x        // Set z=~x
                         ELSE varmap!x := -z        // Set x=~z
      : Axny => UNLESS x AND y DO bug "in apply__unit_rule\n"
                TEST x>y THEN varmap!x := -y        // Set x=~y
                         ELSE varmap!y := -x        // Set y=~x

      : act  => bug("  Unknown action %12b\n", act)
    }
    IF debug>=7 DO prmapping curts
} REPEAT


FUN apply_pair_rules : ts =>

  IF debug>=8 DO writes "\n   Applying pair rules\n"

  // Look for pairs of the form: [rel ,   x , y,  z ]
  //                             [rel',   x', y', z']
  // with z=z' and deduce whatever is possible.

  varmap := ts!TsVm
  UNLESS varmap DO bug "Unable to allocate varmap\n"

  LET n = ts!TsTn
  LET k = 0

  LET vupb = 3*n+ts!TsVmax // Up to 3 variables per term + 
                           // up to one marker per variable.
  LET v = getvec vupb
  UNLESS v RAISE (E_Space, "while allocating sort vector\n")

  sortvec := v              // Register the sort vector

  LET p = ts!TsTerm1 
  IF p & 15 DO bug "Alignment error\n" // Check p is suitably aligned.

  FOR i = 1 TO n DO
  { UNLESS !p<0 DO { mapterm p
                     canonterm p
                     IF p!Tx DO v!++k := @p!Tx
                     IF p!Ty DO v!++k := @p!Ty
                     IF p!Tz DO v!++k := @p!Tz
                   }
    p := @p!Tsize
  }

  sort(v, k, cmpval)

  // Separate in groups for each variable with a zero marker between
  // each group.

  LET t = vupb
  v!t := 0 
  LET curvar = 0
  FOR i = k TO 1 BY -1 DO
  { LET vp = v!i
    LET var = !vp
    IF -2<=var<=2 LOOP
    UNLESS var=curvar DO
    { IF v!t AND v!(t+1)=0 DO
      { varmap!curvar := 2  // Previous variable was a singleton
        change := TRUE
        IF debug>=5 DO { writef("el%4d:", curvar); prterm (v!t) }
      }
      curvar, v!--t := var, 0 // Place marker
    }
    v!--t := vp&-16  // Round down to start of term
  }

  IF debug>=12 DO { writef "Terms for pair search\n"
                    FOR i = t TO vupb TEST v!i
                                      THEN prterm(v!i)
                                      ELSE newline()
                  }

  FOR i = t TO vupb DO
  { LET p = v!i
    IF p=0 LOOP
    LET j = i
    UNTIL !p<0 DO
    { LET q = v!++j
      IF q=0 BREAK
      IF !q<0 LOOP
      pair_rule(p, q)
    }
  }
  
  freevec v

FUN pair_rule : p[rel, x, y, z], q[rel', x', y', z'] =>

    // Find out how many variable are in common and pass the terms
    // to pair_rule1, pair_rule2 or pair_rule3.

    IF rel<0 OR rel'<0 DO bug "Bug found in pair_rule\n"

    IF rel =0 DO { IF debug>=5 DO { writef "unsat: "; prterm p }
                   RAISE E_FalseTermFound
                 }
    IF rel'=0 DO { IF debug>=5 DO { writef "unsat: "; prterm q }
                   RAISE E_FalseTermFound
                 }
    IF p=q RETURN

    // x, y, and z are all distinct, unless zero.

    IF debug>=8 DO { writes "pair:  "; prterm p
                     writes "with:  "; prterm q
                   }

    // The possible equalities are
    // z=z' z=y' z=x' y=z' y=y' y=x' x=z' x=y' x=x'
    UNLESS z=z' MATCH q
    : [?,  ?, =z,  ?] => rel', y', z' := swapyz%rel', z', y'
    : [?, =z,  ?,  ?] => rel', x', z' := swapzx%rel', z', x'
    : [?,  ?,  ?, =y] => rel,  y,  z  := swapyz%rel,  z,  y
    : [?,  ?, =y,  ?] => rel,  y,  z  := swapyz%rel,  z,  y
                         rel', y', z' := swapyz%rel', z', y'
    : [?, =y,  ?,  ?] => rel,  y,  z  := swapyz%rel,  z,  y
                         rel', x', z' := swapzx%rel', z', x'
    : [?,  ?,  ?, =x] => rel,  x,  z  := swapzx%rel,  z,  x
    : [?,  ?, =x,  ?] => rel,  x,  z  := swapzx%rel,  z,  x
                         rel', y', z' := swapyz%rel', z', y'
    : [?, =x,  ?,  ?] => rel,  x,  z  := swapzx%rel,  z,  x
                         rel', x', z' := swapzx%rel', z', x'
    :                 => RETURN       // No variables in common 
    .

    // z=z'

    IF debug>=8 DO { writes "pair:  "; prterm p
                     writes "with:  "; prterm q
                   }
    // The possible equalities are
    // y=y' y=x' x=y' x=x'
    UNLESS y=y' MATCH q
    : [?, =y,  ?,  ?] => rel', x', y' := swapxy%rel', y', x'
    : [?,  ?, =x,  ?] => rel,  x,  y  := swapxy%rel,  y,  x
    : [?, =x,  ?,  ?] => rel,  x,  y  := swapxy%rel,  y,  x
                         rel', x', y' := swapxy%rel', y', x'
    :                 => pair_rule1(p, q) // Only one var in common 
                         RETURN
    .

    // y=y' and z=z'

    IF debug>=8 DO { writes "pair:  "; prterm p
                     writes "with:  "; prterm q
                   }
    // The possible equalities are
    // x=x'
    TEST x=x' THEN pair_rule3(p, q) // All three vars in common 
              ELSE pair_rule2(p, q) // Only two vars in common 

    RETURN


FUN pair_rule1 : p[rel,  x,  y,  z ],
                 q[rel', x', y', z'] => RETURN

FUN pair_rule2 : p[rel,  x,  y,  z ],
                 q[rel', x', y', z'] =>

    IF rel<=0 OR rel'<=0  OR
        x=x'  OR   y~=y'  OR  z~=z' DO bug "Bug found in pair_rule2\n"


    IF debug>=7 DO { writef("pair2: "); prterm p
                     writef(" with: "); prterm q
                     newline()
                   }

    IF y=0 DO
    { IF debug>=5 DO { writef("pair2: "); prterm p
                       writef(" with: "); prterm q
                     }
      y := x'                                        // [rel,   x,0,z]
      rel := (rel * #b101) & (swapxy%rel' * #b10001) // [rel',  y,0,z]
      rel' := -1                                     // ==>
      IF debug>=5 DO { writef("   ==> "); prterm p   // [rel'', x,y,z]
                       writef("  and: "); prterm q   // [void,  y,0,z]
                     }
      RETURN
    }

    LET a = rel  >> 4
    LET b = rel  &  #b1111
    LET c = rel' >> 4
    LET d = rel' &  #b1111
    a, b, c, d := a&c, b&c, a&d, b&d
    LET nrel  = (a|c)<<4 | (b|d)
    LET nrel' = (a|b)<<4 | (c|d)

    IF debug>=8 DO writef("crel: %4b %4b %4b %4b\n", a, b, c, d)

    // All the information in the terms: [rel,  x,  y, z]
    //                                   [rel', x', y, z]
    // is now contained in:           [crel, x, x', y, z]

    // Let's see what this tells us...

    UNLESS rel=nrel AND rel'=nrel' DO
    { IF debug>=5 DO { writef("pair2: "); prterm p
                       writef(" with: "); prterm q
                     }
      rel, rel' := nrel, nrel'
      IF debug>=5 DO { writef("gives: "); prterm p
                       writef("  and: "); prterm q
                     }
      UNLESS rel DO
      { IF debug>=5 DO { writef("unsat: "); prterm p }
        RAISE E_FalseTermFound
      }
      UNLESS rel' DO
      { IF debug>=5 DO { writef("unsat: "); prterm q }
        RAISE E_FalseTermFound
      }
    }

    // a|b|c|d non zero

    UNLESS b|c DO // Must x=x' ?
    { IF debug>=5 DO { writef("pgen=: "); prterm p
                       writef(" with: "); prterm q
                       writef("    => Axpx'   ie %d=%d\n", x, x')
                     }
      IF x<3 AND x'<3 DO bug "both<3 in pair3\n"
      TEST x>x' THEN varmap!x  := x'=0->-1, x'
                ELSE varmap!x' := x =0->-1, x
      change := TRUE
      mapterm p  // Must not permute the arguments
      mapterm q  // or raise an exception.
      rel, rel' := -1, rel&rel'  // Three arguments are now the same
      IF debug>=5 DO { writef("gives: "); prterm p
                       writef("  and: "); prterm q
                     }
    }

    UNLESS a|d DO // Must x=~x' ?
    { IF debug>=5 DO { writef("pgen#: "); prterm p
                       writef(" with: "); prterm q
                       writef("    => Axnx'   ie %d#%d\n", x, x')
                     }
      IF x<3 AND x'<3 DO bug "both<3 in pair3\n"
      TEST x>x' THEN varmap!x  := x'=0->1, -x'
                ELSE varmap!x' := x =0->1, -x
      change := TRUE
      mapterm p  // Must not permute the arguments
      mapterm q  // or raise an exception.
      rel, rel' := -1, rel&rel'  // Three arguments are now the same
      IF debug>=5 DO { writef("gives: "); prterm p
                       writef("  and: "); prterm q
                     }
    }

    apply_unit_rule p
    apply_unit_rule q




FUN pair_rule3 : p[rel,  x,  y,  z ],
                 q[rel', x', y', z'] =>

    IF rel<=0 OR rel'<=0 DO bug "Bug1 found in pair_rule3\n"


    UNLESS x=x' AND y=y' AND z=z' DO
    { prterm p
      prterm q
      bug "Bug found in pair_rule3\n"
    }

    LET nrel = rel&rel'

    IF debug>=5 AND rel' ~= nrel DO
    { writef("pair3: "); prterm p
      writef(" with: "); prterm q
    }

    rel, rel' := -1, nrel

    IF debug>=5 AND rel= -1 DO
    { writef("gives: "); prterm p
      writef("  and: "); prterm q
    }

    apply_unit_rule q

/************* Binary Relation Object: R2 **************

This is designed to hold known information about about
pairs of variable. There are 16 possible binary relations
over a pair of boolean variables x and y. They are encoded
as follows:

1100 x
1010 y                           --represented by varmap--
                        R2(x,y)  EqNe(x,y)   R1(x)   R1(y)
0000  Inconsistent
0001                                         x=0     y=0
0010                                         x=0     y=1
0011                                         x=0
0100                                         x=1     y=0
0101                                                 y=0
0110                               x#y
0111                    ~x->y
1000                                         x=1     y=1
1001                               x=y
1010                                                 y=1
1011                    ~x->~y
1100                                         x=1
1101                     x->y
1110                     x->~y
1111   Always satisfied

The EqNe relation and the simple simple mappings are represented
by the vector varmap.

varmap!x = -1          means variable x is known to equal 0
varmap!x =  1          means variable x is known to equal 1
varmap!x =  y    (x>y) means variables x and y have the same value
varmap!x = -y    (x>y) means variables x and y have opposite values
varmap!x =  0          means nothing known about variable x

Thus varmap contains chains with decreasing subscripts. Whenever the
mapped value of a variable x, say, is looked up (by mapof), its chain
is canonicalized, leaving one of the following conditions satisfied:

    *  varmap!x =  0                        Nothing known about x
    *  varmap!x =  y  and  varmap!y=0       x maps to y
    *  varmap!x = -y  and  varmap!y=0       x maps to ~y

The mappings in varmap are always applied to the variables of a term
before the checker attemps to apply any inference rule.

The only relations that cannot be represented by varmap are ~x->y,
~x->~y, x->y and x->~y.  relations of this sort are held as a set of
triplets [r,x,y] where r is one of {0111,1011,1101,1110}. 

If two triplets [r,x,y] and [s,x,y] referring to the same variables,
the pair can be replaced by the simpler triplet [r&s,x,y] which either
indicates inconsistency or can be represented in varmap. Some cunning
is required in order to represent R2 efficiently.

An instance of an r2 object is created by the call mkr2() and its
methods are:

R2_Init#(r2, vmax)   Initialise the r2 object to be large enough to
                     deal with variable with ids up to vmax.

R2_Close#(r2)        Close down object r2.

R2_Put#(r2, relop, x, y)
                     Add the relation [relop,x,y] to the set, and perform
                     any simplifications that are possible.
                     Return TRUE, if this relation was not already known.
                     Raise E_FalseTermFound if an inconsistency is found.

R2_Eq#(r2, x, y)     Equivalent to R2_Put#(r2, #b1001, x, y)

R2_Ne#(r2, x, y)     Equivalent to R2_Put#(r2, #b0110, x, y)


R2_Get#(r2, x, y)    Return the relation between x and y, as a 4-bit
                     pattern 0000 - 1111, based on information currently
                     held in r2 and varmap.

R2_Merge#(r2, r2')   Merge two maps into r2.

R2_Print#(r2)        Output the information currently held in r2 in some
                     form.

********************************************************/

MANIFEST // R2 fields
  R2fns,
  R2Vm,
  R2Vmax,
  R2Set,
  R2Size
  
STATIC r2fns = [ r2fnInit,
                 r2fnClose,
                 r2fnAdd,
                 r2fnEq,
                 r2fnNe,
                 r2fnRel,
                 r2fnMerge,
                 r2fnPrint
               ]

FUN mkr2 : =>
  LET r2obj = getvec R2Size
  MATCH r2obj
  : 0 => RAISE (E_Space, "Can't allocate R2\n")
  : [ := r2fns,
      :=0,          // varmap if allocated
      :=0,          // vmax
      :=0           // r2set, if allocated
    ] => r2obj

FUN r2fnInit : r2, vmax =>
  LET vm = r2!R2Vm
  IF vm DO freevec vm
  vm := getvec vmax
  r2!R2Vm := vm
  r2!R2Vmax := vmax
  r2!R2Set := 0
  UNLESS vm RAISE (E_Space, "Unable to allocate in R2fnInit\n")
  FOR i = 0 TO vmax DO vm!i := 0

FUN r2fnClose : r2 => writes "r2fnClose called\n"
  IF r2!R2Vm  DO freevec(r2!R2Vm)
  IF r2!R2Set DO freevec(r2!R2Set)
  freevec r2

FUN r2fnAdd : r2, rel, x, y =>
  writef("R2Add: %4b %5d %5d\n", rel, x, y)
  LET vm = r2!R2Vm
  IF x<y DO rel, x, y := swapyz%rel, y, x
  MATCH rel
  : #b0000 => RAISE E_FalseTermFound
  : #b0001 => setmap(vm, x, 0); setmap(vm, y, 0)
  : #b0010 => setmap(vm, x, 0); setmap(vm, y, 1)
  : #b0011 => setmap(vm, x, 0)
  : #b0100 => setmap(vm, x, 1); setmap(vm, y, 0)
  : #b0101 =>                   setmap(vm, y, 0)
  : #b0110 => setmap(vm, x, 1-y)
  : #b0111 => bug "0111 in r2fnAdd\n"
  : #b1000 => setmap(vm, x, 1); setmap(vm, y, 1)
  : #b1001 => setmap(vm, x, y)
  : #b1010 =>                   setmap(vm, y, 1)
  : #b1011 => bug "1011 in r2fnAdd\n"
  : #b1100 => setmap(vm, x, 1)
  : #b1101 => bug "1101 in r2fnAdd\n"
  : #b1110 => bug "1110 in r2fnAdd\n"
  : #b1111 =>
  :        => writes "Unknown relop\n" 

FUN r2fnEq : r2, x, y => r2fnAdd(r2, #b1001, x, y)
FUN r2fnNe : r2, x, y => r2fnAdd(r2, #b0110, x, y)

FUN r2fnRel : r2, x, y =>
  LET vm = r2!R2Vm
  MATCH (getmap(vm, x), getmap(vm, y))
  : 0,   0 => #b0001
  : 0,   1 => #b0010
  : 1,   0 => #b0100
  : 1,   1 => #b1000
  : x,  =x => #b1001
  : x, =-x => #b0110
  : 0,   ? => #b0011
  : 1,   ? => #b1100
  : ?,   0 => #b0101
  : ?,   1 => #b1010
  :        => #b1111
  
FUN r2fnMerge : r2, r2' => bug "r2fnMerge not implemented\n"

FUN r2fnPrint : r2 => 
  LET k = 0
  LET vm = r2!R2Vm
  UNLESS vm DO { writes "No variable mapping\n"; RETURN }
  writes "Mapping:\n"
  FOR x = 0 TO r2!R2Vmax DO /// NEEDS CHANGE
  { LET y = vm!x
    UNLESS y LOOP
    TEST y<0 THEN writef(" %d#%d", x, y=-1->0, -y)
             ELSE writef(" %d=%d", x,  y)
    IF ++k MOD 10 DO newline()
  }
  newline()

FUN setmap : vm, x, y => vm!x := y /// NEEDS CHANGE

FUN getmap : vm, x    => /// NEEDS CHANGE
  LET y = vm!x
  IF x=y RETURN x

  TEST y>=0 THEN UNLESS vm!  y  =  y   DO y :=   getmap(vm, y)
            ELSE UNLESS vm!(1-y)=(1-y) DO y := 1-getmap(vm,1-y)
  vm!x := y
  RETURN y

FUN testr2 : =>
  LET r2 = mkr2()
  R2_Init#(r2, 100)

  { prrels(r2, 8, 8)
    r2fnPrint r2
    writes "Type rel x y: "
    LET rel = readn()
    IF rel<0 BREAK
    LET x = readn()
    LET y = readn()
    R2_Add#(r2, rel, x, y)
  } REPEAT

  R2_Close#(r2)

FUN prrels : r2, x, y =>
  FOR i = 3 TO x DO
  { writef("%4d: ", i)
    FOR j = 3 TO y DO writef(" %4b", R2_Rel#(r2, i, j))
    newline()
  }

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


STATIC debug=0, tmax=0

FUN start : => writes "This program should be called from chk\n"
               RETURN 20


