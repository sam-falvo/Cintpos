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

Rn_Unit#(r)
    Apply the unit rules to the set of terms in r.
    It returns:    0 if error occured
                   1 if the terms are satisfiable
                   2 if they are not satisfiable
                   3 if unable to decide

Rn_Pair#(r)
    Apply the pair rules to the set of terms in r.
    It returns:    0 if error occured
                   1 if the terms are satisfiable
                   2 if they are not satisfiable
                   3 if unable to decide

Rn_Check#(r)
    Apply the checking algorithm to the set of terms in r.
    It returns:    0 if error occured
                   1 if the terms are satisfiable
                   2 if they are not satisfiable
                   3 if unable to decide
*/

GET "chk.h"

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

FUN rnfnInit : r, tmax =>
  IF tmax DO last_given_tmax := tmax
  IF curts DO rnfnClose r
  curts := alloc_termset last_given_tmax
  varmap := curts!TsVm
  init_tabs()

FUN rnfnClose : r =>
  close_tabs()
  close_termset()

FUN rnfnAddTerm3  : r, rel, v2,v1,v0 =>
  UNLESS curts DO { writef "No current term set\n"
                    RETURN
                  }
  pushterm(rel, v2, v1, v0)

FUN rnfnAddTerm4  : r, rel, v3,v2,v1,v0 =>
  bug "fnAddTerm4 not implemented\n"

FUN rnfnAddTerm5  : r, rel, v4,v3,v2,v1,v0 =>
  bug "fnAddTerm5 not implemented\n"

FUN rnfnAddTerm8  : r, r7,r6,r5,r4,r3,r2,r1,r0,
                       v7,v6,v5,v4,v3,v2,v1,v0 =>
  bug "fnAddTerm8 not implemented\n"

FUN rnfnPrTerms : r =>
  UNLESS curts DO { writef "No current term set\n"
                    RETURN
                  }
  prterms curts

FUN rnfnPrMapping : r =>
  UNLESS curts DO { writef "No current term set\n"
                    RETURN
                  }
  prmapping curts

FUN rnfnCompact   : r =>
  UNLESS curts DO { writef "No current term set\n"
                    RETURN
                  }
{ compact_vars  curts
  compact_terms curts

  RETURN 3
} HANDLE : E_FalseTermFound => 2
         : E_NoTerms        => 1
         : E_Space          => 0

FUN rnfnUnit     : rn =>
  UNLESS curts DO { writef "No current term set\n"
                    RETURN
                  }
{ apply_unit_rules curts

  RETURN 3
} HANDLE : E_FalseTermFound => 2
         : E_NoTerms        => 1
         : E_Space          => 0

FUN rnfnPair     : rn =>
  UNLESS curts DO { writef "No current term set\n"
                    RETURN
                  }
{ apply_pair_rules curts

  RETURN 3
} HANDLE : E_FalseTermFound => 2
         : E_NoTerms        => 1
         : E_Space          => 0

FUN rnfnCheck     : rn =>
  UNLESS curts DO { writef "No current term set\n"
                    RETURN
                  }
{ check curts

  RETURN 3
} HANDLE : E_FalseTermFound => 2
         : E_NoTerms        => 1
         : E_Space          => 0


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

MANIFEST

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

    dontcarex%w := (w>>4 | w) & #b00001111 // abcdefgh -> 0000abcd |
                                           //             0000efgh
    dontcarey%w := (w>>2 | w) & #b00110011 // abcdefgh -> 00ab00ef |
                                           //             00cd00gh
    dontcarez%w := (w>>1 | w) & #b01010101 // abcdefgh -> 0a0c0e0g |
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
         Trel,     // An 8-bit relation on variables x, y and z
         Tx,       // The integer id of the first  argument
         Ty,       // The integer id of the second argument
         Tz,       // The integer id of the third  argument
         Tsize     // The size of a term

STATIC   curts=0,  // The current termset
         varmap=0, // The current variable mapping
         sortvec=0 // For registering the sort vector

                   // Termset fields
MANIFEST TsVm,     // varmap vector, if allocated
         TsVmax,   // largest variable id in use
         TsTerm1,  // Pointer to the first term (on a 16 byte boundary)
         TsTermp,  // Pointer to the next free term position
         TsTermt,  // Pointer to the last possible term position
         TsTn,     // Actual number of terms in the set
         TsSysSize

FUN alloc_termset : tmax => // Allocate a termset large enough to
                            // hold tmax terms.

  LET tsupb = TsSysSize+15+Tsize*(tmax+1) // Leave room alignment space.
  LET ts    = getvec tsupb

  IF ts=0 RAISE (E_Space, "no space for termset vector\n")

  LET t = @ts!TsSysSize
  WHILE t&15 DO t++  // Round up to next 16-byte (4-word) boundary.
  
  ts!TsTerm1 := t
  ts!TsTermp := t
  ts!TsTermt := @t!(Tsize*tmax)
  IF @ts!tsupb < @t!(Tsize*(tmax+1)) DO // Check that there is room
     bug "alloc_termset error"          // for tmax terms.

  ts!TsVm    := 0  // No varmap
  ts!TsVmax  := 0  // Max variable number, when known
  ts!TsTn    := 0  // No terms in the set yet

  RETURN ts

FUN select_ts : ts => curts := ts
                      varmap := ts!TsVm

FUN mkcopy_termset : =>
  UNLESS curts DO bug "mkcopy_termset\n"
  LET n = curts!TsTn
  LET nts = alloc_termset n // Allocate a termset of the right size
  UNLESS nts RAISE (E_Space, "in mkcopy_termset\n")
  nts!TsTn := n

  LET p =  curts!TsTerm1
  LET q = nts!TsTerm1
  FOR i = 1 TO n MATCH (p, q)     // Copy the terms
  : [  r,  x,  y,  z, np],
    [:=r,:=x,:=y,:=z, nq]  => p, q := @np, @nq
  .

  alloc_vm(nts, curts!TsVmax)

  RETURN nts

FUN alloc_vm : ts, vmax =>
  UNLESS vmax DO { ts!TsVm, ts!TsVmax := 0, vmax; RETURN 0 } 
  LET vm = getvec vmax
  UNLESS vm RAISE (E_Space, "Unable to alloc varmap of size %d\n", vmax)
  ts!TsVm, ts!TsVmax := vm, vmax
  FOR i = 0 TO vmax DO vm!i := 0
  RETURN vm

FUN close_termset : =>
  UNLESS curts DO bug "close_termset\n"
  IF curts!TsVm DO freevec(curts!TsVm)
  IF sortvec DO { freevec sortvec  // Free the sort vector, if allocated
                  sortvec := 0
                }
  freevec curts
  curts := 0
  varmap := 0


FUN pushterm : rel, x, y, z => 
  UNLESS curts DO bug "pushterm\n"

  MATCH curts : [vm, vmax, term1, termp,termt,n] =>

  IF termp>termt RAISE (E_Space, "Too many terms\n")

  UNLESS TsVmax=1 DO abort 999
  EVERY curts : [?, <x :=x] =>  // Update TsVmax field
              : [?, <y :=y] =>  // if a larger variable
              : [?, <z :=z] =>  // id is encountered.
              .

  MATCH termp : [:=rel,:=x,:=y,:=z, np] => n++
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



FUN compact_vars : =>
  UNLESS curts DO bug "pushterm\n"

  IF curts!TsVm DO{ freevec(curts!TsVm)  // Free possible old vector
                    curts!TsVm := 0
                    varmap := 0
                  }

  LET vmax = curts!TsVmax

  UNLESS vmax RETURN

  LET hv = getvec vmax              // Vector for histogram.
  UNLESS hv RAISE(E_Space, "compact_vars\n")

  LET n = curts!TsTn
  LET p = curts!TsTerm1

  FOR i = 0 TO vmax DO hv!i := 0    // Form histogram of variable uses.
  FOR i = 1 TO n MATCH p
  : [rel, x, y, z, np] => (hv!x)++
                          (hv!y)++
                          (hv!z)++
                          p := @np
  .

  LET newvmax = 2                   // Construct the renaming mapping.

  hv!0 := 0  // Do not
  hv!1 := 1  //        rename 
  hv!2 := 2  //               these variables

  FOR i = 3 TO vmax MATCH hv!i      // Form the rename mapping.
  : 0 =>                    // Never used
  : 1 => hv!i := 2          // used once -- place don't care marker
  :   => hv!i := ++newvmax  // otherwise give it the next id.
  .
  p := curts!TsTerm1

  FOR i = 1 TO n MATCH p            // Apply the rename mapping.
  : [rel,x(:=hv!x),(y:=hv!y),z(:=hv!z), np] =>
      IF x=2 DO rel, x := dontcarex%rel, 0
      IF y=2 DO rel, y := dontcarey%rel, 0
      IF z=2 DO rel, z := dontcarez%rel, 0
      p := @np
  .

  freevec hv             // Free the rename mapping vector

  varmap := alloc_vm(curts, newvmax)


FUN compact_terms : => // Remove deleted or duplicate terms.
  UNLESS curts DO bug "compact_terms\n"

  UNLESS varmap DO bug "varmap=0 in compact_terms\n"

  mark_duplicates()       // Canonicalize terms and 
                          // mark duplicates as deleted.

  LET n = curts!TsTn      // Number of terms.
  LET p = curts!TsTerm1   // Next term to look at.
  LET k = 0               // Count of non-deleted terms.
  LET q = p               // Next free term position.

  FOR i = 1 TO n MATCH (p, q)
  : [ <0, x, y, z, np], ? => // A deleted term.
                             IF debug>=8 DO { writes "cmpct: "; prterm p }
                             p := @np // Skip deleted term.

  : [rel, x, y, z, np],
    [rel',x',y',z',nq]    => k++      // Preserve non-deleted term.
                             UNLESS p=q DO rel',x',y',z' := rel,x,y,z
                             p, q := @np, @nq
  .
  curts!TsTn := k         // New number of terms.
  IF debug>=8 AND k<n DO { writef("Compacted to %d terms\n", k)
                           prterms()
                         }
  UNLESS k DO { writef "No Terms left\n"
                RAISE E_NoTerms
              }


FUN mark_duplicates :  =>          // Mark duplicates as deleted.
  UNLESS curts DO bug "mark_duplicates\n"

  IF sortvec DO { freevec sortvec  // Free sort vector, if allocated
                  sortvec := 0
                }
  LET n = curts!TsTn
  LET k = 0                        // Count of non-deleted terms
  LET v = getvec n                 // Allocate sort vector
  IF v=0 RAISE (E_Space, "in mark_duplicates\n")
  sortvec := v                     // Register sort vector

  LET p = curts!TsTerm1

  FOR i = 1 TO n DO                // Put pointers to
  { canonterm p                    // canonicalized non-deleted terms
    UNLESS !p<0 DO v!++k := p      // into the sort vector.
    p := @p!Tsize
  }

  UNLESS k DO { writef "No Terms left\n"
                RAISE E_NoTerms
              }

  //prterms()
  //writef("Terms for sorting\n")
  //FOR i = 1 TO k DO prterm(v!i)
  //newline()

  sort(v, k, cmpfull)  // Sort the canonical term pointers (v!1...v!k).

  FOR i = 1 TO k-1 DO
  { LET t  = v!i
    LET t' = v!(i+1)
    EVERY (t, t')      // Search for duplicate pairs.
    : [rel,  x, y, z],
      [=rel,=x,=y,=z]  => IF debug>=8 DO { writes "dupl:  "; prterm p }
                          rel := -1   // Mark duplicate term.
  }

  //writef("Terms after marking duplicates\n")
  //FOR i = 1 TO k DO prterm(v!i)

  freevec sortvec            // Free the sort vector
  sortvec := 0

FUN prterm : p[rel, x, y, z] =>
  UNLESS curts DO bug "prterm\n"

  writef("%5d: %8b %s %5d %5d %5d\n",
         (p-curts!TsTerm1)/(4*Tsize), rel, rel2str rel, x,  y,  z)

FUN prterms :  =>
  UNLESS curts DO bug "prterms\n"

  LET n = curts!TsTn      // The number of terms
  LET p = curts!TsTerm1

  writef("Terms:\n")

  FOR i = 1 TO n DO { IF !p>=0 OR TRUE DO prterm p
                      p := @p!Tsize
                    }
  prmapping()

FUN prmapping : =>
  UNLESS curts DO bug "prmapping\n"

  LET k = 1            // For layout
  writef "Mapping:"
  UNLESS varmap DO { writef " No mapping vector\n"; RETURN }
  FOR i = 0 TO curts!TsVmax IF varmap!i DO
  { UNLESS k++ MOD 8 DO newline()
    LET y = varmap!i
    IF y=-1 DO y := 0
    writef(" %d%c%d", i, (y<0->'#','='), ABS y)
  }
  newline()


FUN check :  =>   // It may raise E_FalseTermFound
                  //              E_NoTerms
                  //              E_Space
  UNLESS curts DO bug "check\n"

  compact_vars()
  IF debug>=6 DO prterms()
  apply_simple_rules()
  apply_dilemma_rules()


FUN intersect_maps : vm, vm1, n => // Intersect maps vm, vm1 -> vm
                                   // returs TRUE if non-empty.
  UNLESS vm AND vm1 DO bug "in intersect_maps\n"

  FOR i = 3 TO n DO // Conicalize the vm mapping vector.
  { LET a = vm!i
    IF a=0 LOOP
    LET absa = ABS a
    LET b = vm!absa
    IF b=0 LOOP
    vm!absa := a<0 -> -b, b
  } 

  FOR i = 3 TO n DO // Conicalize the vm1 mapping vector.
  { LET a = vm1!i
    IF a=0 LOOP
    LET absa = ABS a
    LET b = vm1!absa
    IF b=0 LOOP
    vm1!absa := a<0 -> -b, b
  } 

  FOR i = 3 TO n DO // Form mono-lists in vm1 mapping vector
  { LET a = vm1!i
    IF a=0 LOOP
    LET absa = ABS a
    LET b = vm1!absa
    LET absb = ABS b
    IF b DO vm1!i := a*b>0 -> absb, -absb
    vm1!absa := a<0->-i,i
  }
  FOR i = 0 TO n IF ABS(vm1!i)>=i DO vm1!i := 0

  //FOR i = 0 TO n DO // Debug output
  //{ LET a = vm !i
  //  LET b = vm1!i
  //  IF a OR b DO writef("%4d: %5d   %5d\n", i, a, b)
  //}

  LET nonempty = FALSE

  FOR i = n TO 3 BY -1 DO
  { LET vmi = vm!i // Find relations involving variable i common to both. 
    vm!i := 0
    IF vmi=0 LOOP
    LET p = vm1!i
    LET neg = FALSE
    WHILE p DO
    { IF p<0 DO p, neg := -p, ~neg
      LET vmp = vm!p

      //writef("considering: %3d/%3d and %3d/%3d\n", i, vmi, p, vmp)
      TEST neg THEN IF vmi=-vmp DO { IF debug>=5 DO
                                       writef("intermap: %d#%d\n", i, p)
                                     vm!i := -p
                                     nonempty := TRUE
                                   }
               ELSE IF vmi= vmp DO { IF debug>=5 DO
                                       writef("intermap: %d=%d\n", i, p)
                                     vm!i :=  p
                                     nonempty := TRUE
                                   }
      p := vm1!p
    }
  }

  //writef "After intersection\n"     // Debug output
  //FOR i = 0 TO n DO
  //{ LET a = vm !i
  //  LET b = vm1!i
  //  IF a OR b DO writef("%4d: %5d   %5d\n", i, a, b)
  //}

  RETURN nonempty // Non-empty intersection?

FUN combine_maps : vm =>
  LET varmap = curts!TsVm
  LET vmax   = curts!TsVmax
  FOR var = 3 TO curts!TsVmax IF vm!var DO { setmap(varmap, var, vm!var)
                                             writef("dilem: %d=%d\n", var, vm!var)
                                           }
  compact_terms()

FUN apply_dilemma_rules :  =>
  seed := 0  // To make runs repeatable.
  // Depths of more than about 5 are more or less hopeless
  FOR depth = 1 TO 5 DO dilemma depth

FUN dilemma : depth =>
  compact_terms()
  compact_vars()

  LET vmax = curts!TsVmax

  UNLESS varmap DO bug "varmap not allocated\n"

  IF debug>=8 DO { writes("\nApply dilemma rule, depth %d, to:\n", depth)
                   prterms()
                 }

  LET used = getvec vmax
  UNLESS used RAISE (E_Space, "In delemma\n")

  FOR v0 = 3 TO vmax DO
  { FOR v = 0 TO vmax DO used!v := 0

    { LET p = curts!TsTerm1     // Form usage histogram
      FOR i = 1 TO curts!TsTn EVERY p
      : [rel, x, y, z, np] => (used!x)++
                              (used!y)++
                              (used!z)++
      : [  ?, x, y, z, np] => p := @np
    }
    UNTIL used!v0 OR v0>vmax DO v0++
    IF v0>vmax BREAK

    LET m0 = mapof v0
    IF m0=0 OR m0=1 LOOP // Is v0 already specified?
    IF depth=1 DO { tryall(1, v0)
                    LOOP
                  }

    FOR v1 = v0+1 TO vmax IF used!v1 DO
    { LET m1 = mapof v1
      IF m1=0 OR m1=1 LOOP    // Is v1 already specified?
      IF ABS m1 = ABS m0 LOOP // Are v1 and v0 related?
      IF m0=0 OR m0=1 BREAK   // Has v0 become specified?

      // v0 and v1 are independent unspecified variables.
      IF depth=2 DO { tryall(2, v0,v1)
                      m0 := mapof v0
                      LOOP
                    }

      FOR v2 = v1+1 TO vmax IF used!v2 DO
      { LET m2 = mapof v2
        IF m2=0 OR m2=1 LOOP    // Is v2 already specified?
        IF ABS m2 = ABS m1 LOOP // Are v2 and v1 related?
        IF ABS m2 = ABS m0 LOOP // Are v2 and v0 related?
        IF m1=0 OR m1=1 BREAK   // Has v1 become specified?
        IF m0=0 OR m0=1 BREAK   // Has v0 become specified?

        // v0, v1 and v2 are independent unspecified variables.
        IF depth=3 DO { tryall(3, v0,v1,v2)
                        m1 := mapof v1
                        m0 := mapof v0
                        LOOP
                      }

        FOR v3 = v2+1 TO vmax IF used!v3 DO
        { LET m3 = mapof v3
          IF m3=0 OR m3=1 LOOP    // Is v3 already specified?
          IF ABS m3 = ABS m2 LOOP // Are v3 and v2 related?
          IF ABS m3 = ABS m1 LOOP // Are v3 and v1 related?
          IF ABS m3 = ABS m0 LOOP // Are v3 and v0 related?
          IF m2=0 OR m2=1 BREAK   // Has v2 become specified?
          IF m1=0 OR m1=1 BREAK   // Has v1 become specified?
          IF m0=0 OR m0=1 BREAK   // Has v0 become specified?

          // v0, v1, v2 and v3 are independent unspecified variables.
          IF depth=4 DO { tryall(4, v0,v1,v2,v3)
                          m2 := mapof v2
                          m1 := mapof v1
                          m0 := mapof v0
                          LOOP
                        }

          FOR v4 = v3+1 TO vmax IF used!v4 DO
          { LET m4 = mapof v4
            IF m4=0 OR m4=1 LOOP    // Is v4 already specified?
            IF ABS m4 = ABS m3 LOOP // Are v4 and v3 related?
            IF ABS m4 = ABS m2 LOOP // Are v4 and v2 related?
            IF ABS m4 = ABS m1 LOOP // Are v4 and v1 related?
            IF ABS m4 = ABS m0 LOOP // Are v4 and v0 related?
            IF m3=0 OR m3=1 BREAK   // Has v3 become specified?
            IF m2=0 OR m2=1 BREAK   // Has v2 become specified?
            IF m1=0 OR m1=1 BREAK   // Has v1 become specified?
            IF m0=0 OR m0=1 BREAK   // Has v0 become specified?

            // v0, v1, v2, v3 and v4 are independent unspecified variables.
            IF depth=5 DO { tryall(5, v0,v1,v2,v3,v4)
                            m3 := mapof v3
                            m2 := mapof v2
                            m1 := mapof v1
                            m0 := mapof v0
                            LOOP
                          }
            writef("Unable to apply dilemma rule to depth %d\n", depth)
            RETURN
          }
        }
      }
    }
  }
  freevec used

STATIC seed=0

FUN tryall : depth, v0,v1,v2,v3,v4 =>
  LET oldts = curts
  LET vmax = curts!TsVmax
                             // depth= 1,  2,  3,  4,  5, ...
  LET mask = 1<<depth - 1    //        1,  3,  7, 15, 31, ...
  LET inc  = 2<<(depth/2)-1  //        1,  3,  3,  7,  7, ...
  seed := (seed+1) & mask    // Choose a different starting point each time
  LET bits = seed

  IF debug>=6 DO
  { writef "tryall: "
    FOR i = 0 TO depth-1 DO writef(" %3d", (@v0)!i)
    newline()
  }

  IF debug>=10 DO writef("depth=%d inc=%5b  mask=%5b seed=%5b\n",
                          depth,   inc,     mask,    seed)
  LET interv = 0

  { bits := (bits+inc) & mask
    LET nts = mkcopy_termset()
    select_ts nts
    UNLESS varmap DO bug "varmap should be allocated\n"

    // Set the selected variables.
    { LET vp = @v0
      LET settings = bits
      FOR i = 1 TO depth DO
      { LET var = !vp+++
        TEST settings&1 THEN setmap(varmap, var,  1)
                        ELSE setmap(varmap, var, -1)
        settings >>:= 1
      }
    }

    apply_simple_rules()
    HANDLE : E_FalseTermFound =>
                  select_ts nts
                  close_termset()
                  select_ts oldts
                  IF debug>=5 DO 
                    writef("####### setting inconsistent\n")
                  LOOP
           : E_NoTerms =>           // All terms have been satisfied
                  select_ts nts
                  close_termset()
                  select_ts oldts
                  RAISE E_NoTerms
           .

    // Simple rules returned normally (failed to find a solution yet).
    TEST interv
    THEN UNLESS intersect_maps(interv, varmap, vmax) BREAK // If empty.
    ELSE { interv := varmap
           varmap := 0
           nts!TsVm := 0  // So that the varmap is not deallocated.
         }
    select_ts nts
    close_termset()
    select_ts oldts
  } REPEATUNTIL bits=seed

  IF interv=0 DO // All settings lead to inconsistencies.
  { IF debug>=5 DO writef "All settings inconsisten\n"
    RAISE E_FalseTermFound
  }

  select_ts oldts
  combine_maps interv // Add in newly found mappings.
  freevec interv

  apply_simple_rules()

  IF debug>=4 DO
  { LET k = 0
    LET p = curts!TsTerm1
    FOR i = 1 TO curts!TsTn EVERY p
                            : [>=0, x, y, z, np] => k++
                            : [rel, x, y, z, np] => p := @np
                            .
    //prterms()
    writef("Non deleted terms %3d after dilemma rule", k)
    FOR i = 0 TO depth-1 DO writef(" %3d", (@v0)!i)
    newline()
  }

//********************** Apply Simple Rules **********************

STATIC
  change  // Set to TRUE whenever any change is made.

FUN apply_simple_rules : =>
             // It may raise E_FalseTermFound
             //              E_NoTerms
             //              E_Space

  IF debug>=8 DO writef "\nApplying simple rules\n"

  UNLESS varmap DO bug "Found in apply_simple_rules\n"

  IF debug>=7 DO { writef "Terms before applying simple rules:\n"
                   prterms()
                 }

  change := FALSE

  { { change := FALSE
      apply_unit_rules()
    //compact_terms()
    } REPEATWHILE change

    IF debug>=8 DO prterms()

    apply_pair_rules()
    compact_terms()
    IF curts!TsTn=0 RAISE E_NoTerms
    IF debug>=8 DO prterms()

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

  IF x=1 DO rel, x := notx%rel, 0       // Check for True
  IF y=1 DO rel, y := noty%rel, 0
  IF z=1 DO rel, z := notz%rel, 0

  // Check for don't-cares
  IF x=2 OR y=2 OR z=2 DO bug "with don't cares\n"


FUN mapof : x =>
  UNLESS varmap RETURN x // No mapping

// varmap!x =  0        =>  nothing known about x
//          =  1        =>  x-> 1         (true)
//          = -1        =>  x-> 0         (false)
//          =  y (y>2)  =>  x-> y
//          = -y (y>2)  =>  x->~y

  MATCH varmap!x
  :    0  => RETURN x        // nothing known about x
  :   -1  => RETURN 0       // x-> 0
  :    1  => RETURN 1       // x-> 1
  :    2  => bug("dont care variable %d\n", x)  // don't-care marker
  : y(>2) => y := mapof y
             varmap!x := y=0 -> -1, y
             RETURN y
  : y     => y := -mapof(-y)
             IF y= 0 DO y := 1
             IF y=-1 DO y := 0
             varmap!x := y=0 -> -1, y
             RETURN y

FUN setmap : v, x, y =>
// map variable x to equal variable y
//
  LET neg = FALSE
  UNLESS x DO x := -1
  UNLESS y DO y := -1

  { IF x<0 DO neg, x := NOT neg, -x
    UNLESS v!x BREAK
    x := v!x
  } REPEAT

  { IF y<0 DO neg, y := NOT neg, -y
    UNLESS v!y BREAK
    y := v!y
  } REPEAT

  // x>0 and y>0  and v!x=0 and v!y=0

  IF x=y DO { IF neg DO
              { IF debug>=5 DO writef "Unsatisfiable term found\n"
                RAISE E_FalseTermFound
              }
              RETURN
            }
  TEST x>y THEN v!x := neg -> -y, y
           ELSE v!y := neg -> -x, x
  change := TRUE


FUN canonterm              // This canonicalizes a given term. It is
                           // guaranteed not to raise an exception.

: [ <0, ?, ?, ?] => RETURN // A void term

:p[rel, x, y, z] =>
  IF debug>=12 DO { writes "Canon: "; prterm p }
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

  IF x>y DO             rel, x, y := swapxy%rel, y, x
  IF y>z DO {           rel, y, z := swapyz%rel, z, y
              IF x>y DO rel, x, y := swapxy%rel, y, x
            }

  IF x=0 DO rel &:= #b00001111
  IF y=0 DO rel &:= #b00110011
  IF z=0 DO rel &:= #b01010101

  IF rel=#b11111111 OR
     x=0 AND ( rel=#b00001111 OR
               y=0 AND ( rel=#b00000011 OR
                         z=0 AND rel=#b00000001
                       )
             ) DO rel, change := -1, TRUE

  IF debug>=12 DO { writes "=====> "; prterm p }


FUN apply_unit_rules : =>
                       // This function applied the mapping (in
                       // vector curts!TsVm) to the terms in curts, and
                       // applies the unit rule until convergence,
                       // leaving the final mapping in the varmap.

  { change := FALSE // changes to TRUE if the variable mapping changes

    IF debug>=7 DO writef("\n   Applying unit rules\n")

    IF debug>=7 DO prterms()

    LET n = curts!TsTn
    LET p = curts!TsTerm1
    LET k = 0 // Count of non-deleted terms
    FOR i = 1 TO n DO { UNLESS !p<0 DO { k++; apply_unit_rule p }
                        p := @p!Tsize
                      }
    IF k=0 DO { IF debug>=5 DO writef "No non-deleted terms\n"
                RAISE E_NoTerms
              }
  } REPEATWHILE change // repeat if the mapping has changed

FUN apply_unit_rule
:  [ <0, x, y, z] => // A deleted term 

: p[rel, x, y, z] =>

  { canonterm p

    IF rel<0      DO { IF debug>=5 DO { writef "void:  "; prterm p } 
                       RETURN
                     }

    LET pat  = pattern(x, y, z)
    LET nrel = rel & pat

    IF rel~=nrel DO { IF debug>=5 DO { writef "pat:   "; prterm p }
                      rel := nrel
                      IF debug>=5 DO { writef "   =>  "; prterm p }
                    }

    IF rel=0     DO { IF debug>=5 DO { writef "unsat: "; prterm p }
                      RAISE E_FalseTermFound
                    }

    IF rel=pat   DO { IF debug>=5 DO { writef "del:   "; prterm p }
                      rel := -1
                      change := TRUE
                      RETURN
                    }

    LET acts = rel2act!rel - rel2act!pat

    UNLESS acts RETURN

    IF debug>=5 DO { writef "unit:  "; prterm p }

    UNLESS varmap DO { varmap := alloc_vm(curts, curts!TsVmax)
                       abort 711
                     }

    WHILE acts DO                // Iterate over the actions
    { LET act = acts & -acts
      acts -:= act
      IF debug>=5 DO writef("   =>  %s\n", actstr act)

      MATCH act
      : Ax0  =>               setmap(varmap, x, -1) // Set x=0
      : Ax1  =>               setmap(varmap, x,  1) // Set x=1
      : Ay0  =>               setmap(varmap, y, -1) // Set y=0
      : Ay1  =>               setmap(varmap, y,  1) // Set y=1
      : Az0  =>               setmap(varmap, z, -1) // Set z=0
      : Az1  =>               setmap(varmap, z,  1) // Set z=1

      : Aypz => UNLESS y AND z DO bug "in apply__unit_rule\n"
                setmap(varmap, y,  z)               // Set y=z
      : Azpx => UNLESS x AND z DO bug "in apply__unit_rule\n"
                setmap(varmap, z,  x)               // Set z=x
      : Axpy => UNLESS x AND y DO bug "in apply__unit_rule\n"
                setmap(varmap, x,  y)               // Set x=y
      : Aynz => UNLESS y AND z DO bug "in apply__unit_rule\n"
                setmap(varmap, y, -z)               // Set y=~z
      : Aznx => UNLESS x AND z DO bug "in apply__unit_rule\n"
                setmap(varmap, z, -x)               // Set z=~x
      : Axny => UNLESS x AND y DO bug "in apply__unit_rule\n"
                setmap(varmap, x, -y)               // Set x=~y

      : act  => bug("  Unknown action %12b\n", act)
    }
    IF debug>=12 DO prmapping curts
  } REPEAT  // To canonicalize and possibly delete the term.


FUN apply_pair_rules : =>

  IF debug>=8 DO writes "\n   Applying pair rules\n"

  // Look for pairs of the form: [rel ,   x , y,  z ]
  //                             [rel',   x', y', z']
  // with z=z' and deduce whatever is possible.

  UNLESS varmap DO { varmap := alloc_vm(curts, curts!TsVmax)
                     abort 712
                   }

  LET n = curts!TsTn
  LET k = 0

  LET vupb = 3*n+curts!TsVmax // Up to 3 variables per term + 
                              // up to one marker per variable.
  LET v = getvec vupb
  UNLESS v RAISE (E_Space, "while allocating sort vector\n")

  sortvec := v              // Register the sort vector

  LET p = curts!TsTerm1 
  IF p & 15 DO bug "Alignment error\n" // Check p is suitably aligned.

  FOR i = 1 TO n DO
  { canonterm p
    UNLESS !p<0 DO { IF p!Tx DO v!++k := @p!Tx
                     IF p!Ty DO v!++k := @p!Ty
                     IF p!Tz DO v!++k := @p!Tz
                   }
    p := @p!Tsize
  }

  UNLESS k DO
  { IF debug>=5 DO writef "There are no terms with non-zero variables\n"
    freevec sortvec
    sortvec := 0
    RETURN
  }

  sort(v, k, cmpval)

  // v!1 ... v!k are pointers to non-zero variables in non-deleted terms
  // The pointers are sorted so that !(v!i) <= !(v!(i+1)), i=1,..,k-1
  // (v!i)&-16 points to the start of the term that v!i refers to.
 
  // Separate in groups for each variable with a zero marker separating
  // each group, and deal with variables that are only used once.

  LET t = vupb
  v!t := 0 
  LET ap=v!k
  LET a=!ap, b=0, c=0
  FOR i = k-1 TO 0 BY -1 DO
  { LET term = ap&-16   // Round down to start of term.
    a, b, c := 0, a, b
    IF i>0 DO { ap := v!i; a := !ap }
    UNLESS a=b OR b=c DO
    { MATCH term              // Variable b was a singleton
      : [rel :=dontcarex%rel, =b :=0,      ?,      ?] =>
      : [rel :=dontcarey%rel,      ?, =b :=0,      ?] =>
      : [rel :=dontcarez%rel,      ?,      ?, =b :=0] =>
      :   => prterm term
             bug("concerning dont care %d\n", b)
      .
      change := TRUE
      IF debug>=5 DO { writef("el%4d:", b); prterm term }
      LOOP
    }
    UNLESS b=c DO v!--t := 0 // Place separator mark.
    v!--t := term
  }

  IF debug>=12 DO { 
                    writef "Terms for pair search\n"
                    FOR i = t TO vupb TEST v!i
                                      THEN prterm(v!i)
                                      ELSE newline()
                  }

  FOR i = t TO vupb DO
  { LET p = v!i
    IF p=0 LOOP
    LET j = i
    UNTIL !p<0 DO { LET q = v!++j
                    IF q=0 BREAK
                    IF !q<0 LOOP
                    pair_rule(p, q)
                  }
  }
  
  freevec sortvec
  sortvec := 0

FUN pair_rule : p[rel,  x,  y,  z ] ,
                q[rel', x', y', z'] =>

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

    // The terms p and q have been canonicalised and so, for instance,
    // x, y, and z are all distinct, unless zero.
    // ***** NOT TRUE ******

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

    IF rel<=0 OR rel'<=0  RETURN

    IF  x=x'  OR   y~=y'  OR  z~=z' DO bug "Bug found in pair_rule2\n"


    IF debug>=12 DO { writef("pair2: "); prterm p
                      writef(" with: "); prterm q
                      newline()
                    }

    IF y=0 DO
    { IF debug>=5 DO { writef("comb:  "); prterm p
                       writef("with:  "); prterm q
                     }
      y := x'                                        // [rel,   x,0,z]
      rel := (rel * #b101) & (swapxy%rel' * #b10001) // [rel',  y,0,z]
      rel', change := -1, TRUE                       // ==>
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
    { IF debug>=5 DO { writef("simp:  "); prterm p
                       writef("with:  "); prterm q
                     }
      IF rel~=nrel DO   { rel := nrel
                          IF debug>=5 DO { writef "=====> "; prterm p }
                        }

      IF rel'~=nrel' DO { rel' := nrel'
                          IF debug>=5 DO { writef "=====> "; prterm q }
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
      setmap(varmap, x, x')
      mapterm p  // Must not permute the arguments
      mapterm q  // or raise an exception.
      rel, rel' := -1, rel&rel'  // Three arguments are now the same
      change := TRUE
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
      setmap(varmap, x, x'=0->1,-x')
      mapterm p  // Must not permute the arguments
      mapterm q  // or raise an exception.
      rel, rel' := -1, rel&rel'  // Three arguments are now the same
      change := TRUE
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
    change := TRUE

    IF debug>=5 AND rel= -1 DO
    { writef("gives: "); prterm p
      writef("  and: "); prterm q
    }

    apply_unit_rule q

//********************* Sort Function ******************

STATIC cmpfn  // cmpfn(p, q)=TRUE <=> term p < term q

FUN cmpval : [<z], [z] => TRUE
           :           => FALSE

FUN cmpfull : [r,  x,  y,  z ] ,
              [r', x', y', z'] =>
  IF z<z' RETURN TRUE
  IF z>z' RETURN FALSE
  IF y<y' RETURN TRUE
  IF y>y' RETURN FALSE
  IF x<x' RETURN TRUE
  IF x>x' RETURN FALSE
  IF r<r' RETURN TRUE
  IF r>r' RETURN FALSE
  RETURN FALSE         // This is important -- sort may loop otherwise.

FUN sort : v, n, f => cmpfn := f
                      qsort(@v!1, @v!n)

FUN qsort : l, r =>
  WHILE @l!8<r DO
  { LET midpt = ((l+r)/2) & -Bpw
    //writef("qsort: %5d  %5d\n", (l-sortvec)/Bpw, (r-sortvec)/Bpw)
    // Select a good(ish) median value.
    LET val   = middle(!l, !midpt, !r)
    LET p = partition(val, l, r)
    // Only use recursion on the smaller partition.
    TEST p>midpt THEN { qsort(p, r);     r := @p!-1 }
                 ELSE { qsort(l, @p!-1); l := p     }
   }
   //writef("isort: %5d  %5d\n", (l-sortvec)/Bpw, (r-sortvec)/Bpw)
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


FUN start : => writes "This program should be called from chk\n"
               RETURN 20


