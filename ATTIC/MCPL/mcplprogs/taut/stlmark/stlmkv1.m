GET "mcpl.h"

/* This is an algorithm for tautology checking implemented in MCPL
   by M. Richards. It is based on Stalmarck's algorithm as described
   in the paper:

   G. Stalmarck and M. Saflund, Modelling and Verifying Systems and
   Software in Propositional Logic, IFAC, SAFECOMP90, London, UK 1990

   Stalmarck's algorithm has a patent pending (or is already patented).
*/

MANIFEST 
// The 16 boolean operators are represented by bit patterns 0000..1111
// and have manifest names of the form: Fabcd

//                                    x y z
// where  0 Fabcd 0 => a    tabular   a 0 0   with  x = y op z
//        0 Fabcd 1 => b    form      b 0 1   
//        1 Fabcd 0 => c              c 1 0
//        1 Fabcd 1 => d              d 1 1

// For example, F0001=1  and represents And 
// and          F1101=13 and represents Imp 

F0000= 0, F0001= 1, F0010= 2, F0011= 3, 
F0100= 4, F0101= 5, F0110= 6, F0111= 7, 
F1000= 8, F1001= 9, F1010=10, F1011=11, 
F1100=12, F1101=13, F1110=14, F1111=15, 

Id, Lparen, Rparen,Eof,  // Lexical tokens

NotY=F1100, And =F0001, Or  =F0111, Imp =F1101, Eqv =F1001

FUN opstr   // Returns a string for a given boolean operator.
: F0000 => "FF  " : F0001 => "And " : F0010 => "Gt  " : F0011 => "Y   "
: F0100 => "Lt  " : F0101 => "Z   " : F0110 => "Xor " : F0111 => "Or  "
: F1000 => "Nor " : F1001 => "Eqv " : F1010 => "NotZ" : F1011 => "Ge  "
: F1100 => "NotY" : F1101 => "Imp " : F1110 => "Nand" : F1111 => "TT  "
: -1    => "Void"
:       => "Bad "

FUN swapyz // Return op' given op    (x = y op z) = (x = z op' y)
           // ie. F a b c d -> F a c b d
: op => TABLE [F0000, F0001, F0100, F0101,
               F0010, F0011, F0110, F0111,
               F1000, F1001, F1100, F1101,
               F1010, F1011, F1110, F1111] ! op

FUN notx   // Return op' given op    (x = y op z) = (~x = y op' z)
           // ie. F a b c d -> F~a~c~b~d
: op => #b1111 - op  // Optimisation of

FUN noty   // Return op' given op    (x = y op z) = (x = ~y op' z)
           // ie. F a b c d -> F c d a b
: op => TABLE [F0000, F0100, F1000, F1100,
               F0001, F0101, F1001, F1101,
               F0010, F0110, F1010, F1110,
               F0011, F0111, F1010, F1111] ! op

FUN notz   // Return op' given op    (x = y op z) = (x = y op' ~z)
           // ie. F a b c d -> F b a d c
: op => TABLE [F0000, F0010, F0001, F0011,
               F1000, F1010, F1001, F1011,
               F0100, F0110, F0101, F0111,
               F1100, F1110, F1101, F1111] ! op



MANIFEST
  E_syntax=100, E_space,        // Exceptions

// Rows in the matrix representation of the propositional expression
// are of the form: [op, x, y, z]
// meaning:         x = y op z
// where x, y and z are variable ids, some occuring in the expression
// while others are computer generated. The ids are represented by
// integers: 0, 1, 2,...
// Id 0 is false
// Id 1 is true
// and the other ids are free to hold either true or false

// Terms sometimes allow information about their variables to be
// deduced.  For example: [And, x, 1, 1] => x=1
//                        [Imp, 0, y, y] is false
//                        [Imp, 0, y, z] => y=1 and z=0
//                        [Imp, x, y, 0] => x=~y

// The pattern of operands in [op,x,y,z] is represented by
// a 9-bit bit integer: eee_xx_yy_zz
// eee = 100 means y=z, x different variable
//     = 010 means x=z, y different variable
//     = 001 means x=y, z different variable
//     = 000 means all different variable
//     = 111 means all the same variable

// xx  = 00  nothing known about x, could be 0 or 1
//     = 01  x=0
//     = 10  x=1
//     = 11  error -- not used

// yy and zz are defined similarly

// The 37 possible operand patterns have the following manifest names

P000 = #b111_01_01_01, P001 = #b001_01_01_10, P00z = #b001_01_01_00,
P010 = #b010_01_10_01, P011 = #b100_01_10_10, P01z = #b000_01_10_00,
P0y0 = #b010_01_00_01, P0y1 = #b000_01_00_10, P0yy = #b100_01_00_00,
                                              P0yz = #b000_01_00_00,
P100 = #b100_10_01_01, P101 = #b010_10_01_10, P10z = #b000_10_01_00,
P110 = #b001_10_10_01, P111 = #b111_10_10_10, P11z = #b001_10_10_00,
P1y0 = #b000_10_00_01, P1y1 = #b010_10_00_10, P1yy = #b100_10_00_00,
                                              P1yz = #b000_10_00_00,
Px00 = #b100_00_01_01, Px01 = #b000_00_01_10, Px0x = #b010_00_01_00,
                                              Px0z = #b000_00_01_00,
Px10 = #b000_00_10_01, Px11 = #b100_00_10_10, Px1x = #b010_00_10_00,
                                              Px1z = #b000_00_10_00,
Pxx0 = #b001_00_00_01, Pxx1 = #b001_00_00_10, Pxxx = #b111_00_00_00,
                                              Pxxz = #b001_00_00_00,
Pxy0 = #b000_00_00_01, Pxy1 = #b000_00_00_10, Pxyx = #b010_00_00_00,
                                              Pxyy = #b100_00_00_00,
                                              Pxyz = #b000_00_00_00

FUN patstr  // return string for pattern
: P000=>"P000" : P001=>"P001" : P00z=>"P00z"
: P010=>"P010" : P011=>"P011" : P01z=>"P01z"
: P0y0=>"P0y0" : P0y1=>"P0y1" : P0yy=>"P0yy" : P0yz=>"P0yz"
: P100=>"P100" : P101=>"P101" : P10z=>"P10z"
: P110=>"P110" : P111=>"P111" : P11z=>"P11z"
: P1y0=>"P1y0" : P1y1=>"P1y1" : P1yy=>"P1yy" : P1yz=>"P1yz"
: Px00=>"Px00" : Px01=>"Px01" : Px0x=>"Px0x" : Px0z=>"Px0z"
: Px10=>"Px10" : Px11=>"Px11" : Px1x=>"Px1x" : Px1z=>"Px1z"
: Pxx0=>"Pxx0" : Pxx1=>"Pxx1" : Pxxx=>"Pxxx" : Pxxz=>"Pxxz"
: Pxy0=>"Pxy0" : Pxy1=>"Pxy1" : Pxyx=>"Pxyx" : Pxyy=>"Pxyy" : Pxyz=>"Pxyz"
:      => "Perr"

// pat(op,x,y,z) returns the operand pattern

FUN pat : x, y, z => // Return pattern of operands
  LET res = 0
  IF x<=1 TEST x=0 THEN res +:= #b000_01_00_00
                   ELSE res +:= #b000_10_00_00
  IF y<=1 TEST y=0 THEN res +:= #b000_00_01_00
                   ELSE res +:= #b000_00_10_00
  IF z<=1 TEST z=0 THEN res +:= #b000_00_00_01
                   ELSE res +:= #b000_00_00_10
  IF y=z           DO   res +:= #b100_00_00_00
  IF z=x           DO   res +:= #b010_00_00_00
  IF x=y           DO   res +:= #b001_00_00_00
  RETURN res

// As mentioned above, information can sometimes be deduced from
// the operator of a term and the pattern of its arguments.
// For example: [And, x, 1, 1] => x=1
//              [Imp, 0, y, y] is false
//              [Imp, 0, y, z] => y=1 and z=0
//              [Imp, x, y, 0] => x=~y

// Each possible action is given a manifest name below:

MANIFEST   // For term: [op, x, y, z]   // We may need more actions

  Af=1,    // The term is false, so the whole expression is false

  At,      // The term is true, so can be removed

  Ax0, Ax1, Ay0, Ay1, Az0, Az1,     // Set x, y or z to 0 or 1

  Ay0z0, Ay0z1, Ay1z0, Ay1z1,       // set y and z
  Az0x0, Az0x1, Az1x0, Az1x1,       // set z and x
  Ax0y0, Ax0y1, Ax1y0, Ax1y1,       // set x and y

  Aynz,    // Set y=~z if y>z, otherwise set z=~y
  Aznx,    // Set z=~x if z>x, otherwise set x=~z
  Axny,    // Set x=~y if x>y, otherwise set y=~x

  Aypz,    // Set y=z  if y>z, otherwise set z=y
  Azpx,    // Set z=x  if z>x, otherwise set x=z
  Axpy,    // Set x=y  if x>y, otherwise set y=x

           // For all the above actions remove the term

  Akeep,   // Nothing can be deduced, keep the term    
  Abad

FUN actstr
: Af    => "Af   " : At    => "At   "
: Ax0   => "Ax0  " : Ax1   => "Ax1  "
: Ay0   => "Ay0  " : Ay1   => "Ay1  "
: Az0   => "Az0  " : Az1   => "Az1  "
: Ay0z0 => "Ay0z0" : Ay0z1 => "Ay0z1" : Ay1z0 => "Ay1z0" : Ay1z1 => "Ay1z1"
: Az0x0 => "Az0x0" : Az0x1 => "Az0x1" : Az1x0 => "Az1x0" : Az1x1 => "Az1x1" 
: Ax0y0 => "Ax0y0" : Ax0y1 => "Ax0y1" : Ax1y0 => "Ax1y0" : Ax1y1 => "Ax1y1" 
: Aynz  => "Aynz " : Aznx  => "Aznx " : Axny  => "Axny "
: Aypz  => "Aypz " : Azpx  => "Azpx " : Axpy  => "Axpy "
: Akeep => "Akeep"
: Abad  => "Abad "
:       => "Aerr "

FUN action
: F0000, pattern =>           // FF    valid triples:  000 001 010 011
  MATCH pattern
  : P000 => At   : P001 => At   : P00z => At
  : P010 => At   : P011 => At   : P01z => At
  : P0y0 => At   : P0y1 => At   : P0yy => At   : P0yz => At
  : P100 => Af   : P101 => Af   : P10z => Af
  : P110 => Af   : P111 => Af   : P11z => Af
  : P1y0 => Af   : P1y1 => Af   : P1yy => Af   : P1yz => Af
  : Px00 => Ax0  : Px01 => Ax0  : Px0x => Ax0  : Px0z => Ax0
  : Px10 => Ax0  : Px11 => Ax0  : Px1x => Ax0  : Px1z => Ax0
  : Pxx0 => Ax0  : Pxx1 => Ax0  : Pxxx => Ax0  : Pxxz => Ax0
  : Pxy0 => Ax0  : Pxy1 => Ax0  : Pxyx => Ax0  : Pxyy => Ax0  : Pxyz => Ax0
  .
: F0001, pattern =>           // And   valid triples:  000 001 010 111
  MATCH pattern
  : P000 => At   : P001 => At   : P00z => At
  : P010 => At   : P011 => Af   : P01z => Az0
  : P0y0 => At   : P0y1 => Ay0  : P0yy => Ay0  : P0yz => Akeep
  : P100 => Af   : P101 => Af   : P10z => Af
  : P110 => Af   : P111 => At   : P11z => Az1
  : P1y0 => Af   : P1y1 => Ay1  : P1yy => Ay1  : P1yz => Ay1z1
  : Px00 => Ax0  : Px01 => Ax0  : Px0x => Ax0  : Px0z => Ax0
  : Px10 => Ax0  : Px11 => Ax1  : Px1x => At   : Px1z => Azpx
  : Pxx0 => Ax0  : Pxx1 => At   : Pxxx => At   : Pxxz => Akeep
  : Pxy0 => Ax0  : Pxy1 => Axpy : Pxyx => Akeep: Pxyy => Axpy : Pxyz => Akeep
  .
: F0010, pattern =>           // Gt    valid triples:  000 001 110 011
  MATCH pattern
  : P000 => At   : P001 => At   : P00z => At
  : P010 => Af   : P011 => At   : P01z => Az1
  : P0y0 => Ay0  : P0y1 => At   : P0yy => At   : P0yz => Akeep
  : P100 => Af   : P101 => Af   : P10z => Af
  : P110 => At   : P111 => Af   : P11z => Az0
  : P1y0 => Ay1  : P1y1 => Af   : P1yy => Af   : P1yz => Ay1z0
  : Px00 => Ax0  : Px01 => Ax0  : Px0x => Ax0  : Px0z => Ax0
  : Px10 => Ax1  : Px11 => Ax0  : Px1x => Af   : Px1z => Aznx
  : Pxx0 => At   : Pxx1 => Ax0  : Pxxx => Ax0  : Pxxz => Akeep
  : Pxy0 => Axpy : Pxy1 => Ax0  : Pxyx => Ax0y0: Pxyy => Ax0  : Pxyz => Akeep
  .
: F0011, pattern =>           // Y     valid triples:  000 001 110 111
  MATCH pattern
  : P000 => At   : P001 => At   : P00z => At
  : P010 => Af   : P011 => Af   : P01z => Af
  : P0y0 => Ay0  : P0y1 => Ay0  : P0yy => Ay0  : P0yz => Ay0
  : P100 => Af   : P101 => Af   : P10z => Af
  : P110 => At   : P111 => At   : P11z => At
  : P1y0 => Ay1  : P1y1 => Ay1  : P1yy => Ay1  : P1yz => Ay1
  : Px00 => Ax0  : Px01 => Ax0  : Px0x => Ax0  : Px0z => Ax0
  : Px10 => Ax1  : Px11 => Ax1  : Px1x => Ax1  : Px1z => Ax1
  : Pxx0 => At   : Pxx1 => At   : Pxxx => At   : Pxxz => At
  : Pxy0 => Axpy : Pxy1 => Axpy : Pxyx => Axpy : Pxyy => Axpy : Pxyz => Axpy
  .
: F0100, pattern =>           // Lt    valid triples:  000 101 010 011
  MATCH pattern
  : P000 => At   : P001 => Af   : P00z => Az0
  : P010 => At   : P011 => At   : P01z => At
  : P0y0 => At   : P0y1 => Ay1  : P0yy => At   : P0yz => Akeep
  : P100 => Af   : P101 => At   : P10z => Az1
  : P110 => Af   : P111 => Af   : P11z => Af
  : P1y0 => Af   : P1y1 => Ay0  : P1yy => Af   : P1yz => Ay0z1
  : Px00 => Ax0  : Px01 => Ax1  : Px0x => At   : Px0z => Azpx
  : Px10 => Ax0  : Px11 => Ax0  : Px1x => Ax0  : Px1z => Ax0
  : Pxx0 => Ax0  : Pxx1 => Af   : Pxxx => Ax0  : Pxxz => Az0x0
  : Pxy0 => Ax0  : Pxy1 => Axny : Pxyx => Akeep: Pxyy => Ax0 : Pxyz => Akeep
  .
: F0101, pattern =>           // Z     valid triples:  000 101 010 111
  MATCH pattern
  : P000 => At   : P001 => Af   : P00z => Az0
  : P010 => At   : P011 => Af   : P01z => Az0
  : P0y0 => At   : P0y1 => Af   : P0yy => Ay0  : P0yz => Az0
  : P100 => Af   : P101 => At   : P10z => Az1
  : P110 => Af   : P111 => At   : P11z => Az1
  : P1y0 => Af   : P1y1 => At   : P1yy => Ay1  : P1yz => Az1
  : Px00 => Ax0  : Px01 => Ax1  : Px0x => At   : Px0z => Azpx
  : Px10 => Ax0  : Px11 => Ax1  : Px1x => At   : Px1z => Azpx
  : Pxx0 => Ax0  : Pxx1 => Ax1  : Pxxx => At   : Pxxz => Azpx
  : Pxy0 => Ax0  : Pxy1 => Ax1  : Pxyx => At   : Pxyy => Axpy : Pxyz => Azpx
  .
: F0110, pattern =>           // Xor   valid triples:  000 101 110 011
  MATCH pattern
  : P000 => At   : P001 => Af   : P00z => Az0
  : P010 => Af   : P011 => At   : P01z => Az1
  : P0y0 => Ay0  : P0y1 => Ay1  : P0yy => At   : P0yz => Aypz
  : P100 => Af   : P101 => At   : P10z => Az1
  : P110 => At   : P111 => Af   : P11z => Az0
  : P1y0 => Ay1  : P1y1 => Ay0  : P1yy => Af   : P1yz => Aynz
  : Px00 => Ax0  : Px01 => Ax1  : Px0x => At   : Px0z => Azpx
  : Px10 => Ax1  : Px11 => Ax0  : Px1x => Af   : Px1z => Aznx
  : Pxx0 => At   : Pxx1 => Af   : Pxxx => Ax0  : Pxxz => Az0
  : Pxy0 => Axpy : Pxy1 => Axny : Pxyx => Ay0  : Pxyy => Ax0 : Pxyz => Akeep
  .
: F0111, pattern =>           // Or    valid triples:  000 101 110 111
  MATCH pattern
  : P000 => At   : P001 => Af   : P00z => Az0
  : P010 => Af   : P011 => Af   : P01z => Af
  : P0y0 => Ay0  : P0y1 => Af   : P0yy => Ay0  : P0yz => Ay0z0
  : P100 => Af   : P101 => At   : P10z => Az1
  : P110 => At   : P111 => At   : P11z => At
  : P1y0 => Ay1  : P1y1 => At   : P1yy => Ay1  : P1yz => Akeep
  : Px00 => Ax0  : Px01 => Ax1  : Px0x => At   : Px0z => Azpx
  : Px10 => Ax1  : Px11 => Ax1  : Px1x => Ax1  : Px1z => Ax1
  : Pxx0 => At   : Pxx1 => Ax1  : Pxxx => At   : Pxxz => Akeep
  : Pxy0 => Axpy : Pxy1 => Ax1  : Pxyx => Akeep: Pxyy => Axpy : Pxyz => Akeep
  .
: F1000, pattern =>           // Nor   valid triples:  100 001 010 011
  MATCH pattern
  : P000 => Af   : P001 => At   : P00z => Az1
  : P010 => At   : P011 => At   : P01z => At
  : P0y0 => Ay1  : P0y1 => At   : P0yy => Ay1  : P0yz => Akeep
  : P100 => At   : P101 => Af   : P10z => Az0
  : P110 => Af   : P111 => Af   : P11z => Af
  : P1y0 => Ay0  : P1y1 => Af   : P1yy => Ay0  : P1yz => Ay0z0
  : Px00 => Ax1  : Px01 => Ax0  : Px0x => Af   : Px0z => Aznx
  : Px10 => Ax0  : Px11 => Ax0  : Px1x => Ax0  : Px1z => Ax0
  : Pxx0 => Af   : Pxx1 => Ax0  : Pxxx => Af   : Pxxz => Az1x0
  : Pxy0 => Axny : Pxy1 => Ax0  : Pxyx => Ax0y1: Pxyy => Axny : Pxyz => Akeep
  .
: F1001, pattern =>           // Eqv   valid triples:  100 001 010 111
  MATCH pattern
  : P000 => Af   : P001 => At   : P00z => Az1
  : P010 => At   : P011 => Af   : P01z => Az0
  : P0y0 => Ay1  : P0y1 => Ay0  : P0yy => Af   : P0yz => Aynz
  : P100 => At   : P101 => Af   : P10z => Az0
  : P110 => Af   : P111 => At   : P11z => Az1
  : P1y0 => Ay0  : P1y1 => Ay1  : P1yy => At   : P1yz => Aypz
  : Px00 => Ax1  : Px01 => Ax0  : Px0x => Af   : Px0z => Aznx
  : Px10 => Ax0  : Px11 => Ax1  : Px1x => At   : Px1z => Azpx
  : Pxx0 => Af   : Pxx1 => At   : Pxxx => Ax1  : Pxxz => Az1
  : Pxy0 => Axny : Pxy1 => Axpy : Pxyx => Ay1  : Pxyy => Ax1 : Pxyz => Akeep
  .
: F1010, pattern =>           // NotZ  valid triples:  100 001 110 011
  MATCH pattern
  : P000 => Af   : P001 => At   : P00z => Az1
  : P010 => Af   : P011 => At   : P01z => Az1
  : P0y0 => Af   : P0y1 => At   : P0yy => Ay1  : P0yz => Az1
  : P100 => At   : P101 => Af   : P10z => Az0
  : P110 => At   : P111 => Af   : P11z => Az0
  : P1y0 => At   : P1y1 => Af   : P1yy => Ay0  : P1yz => Az0
  : Px00 => Ax1  : Px01 => Ax0  : Px0x => Af   : Px0z => Aznx
  : Px10 => Ax1  : Px11 => Ax0  : Px1x => Af   : Px1z => Aznx
  : Pxx0 => Ax1  : Pxx1 => Ax0  : Pxxx => Af   : Pxxz => Aznx
  : Pxy0 => Ax1  : Pxy1 => Ax0  : Pxyx => Af   : Pxyy => Axny : Pxyz => Aznx
  .
: F1011, pattern =>           // Ge    valid triples:  100 001 110 111
  MATCH pattern
  : P000 => Af   : P001 => At   : P00z => Az1
  : P010 => Af   : P011 => Af   : P01z => Af
  : P0y0 => Af   : P0y1 => Ay0  : P0yy => Af   : P0yz => Ay0z1
  : P100 => At   : P101 => Af   : P10z => Az0
  : P110 => At   : P111 => At   : P11z => At
  : P1y0 => At   : P1y1 => Ay1  : P1yy => At   : P1yz => Akeep
  : Px00 => Ax1  : Px01 => Ax0  : Px0x => Af   : Px0z => Aznx
  : Px10 => Ax1  : Px11 => Ax1  : Px1x => Ax1  : Px1z => Ax1
  : Pxx0 => Ax1  : Pxx1 => At   : Pxxx => Ax1  : Pxxz => Akeep
  : Pxy0 => Ax1  : Pxy1 => Axpy : Pxyx => Ax1y1: Pxyy => Ax1  : Pxyz => Akeep
  .
: F1100, pattern =>           // NotY  valid triples:  100 101 010 011
  MATCH pattern
  : P000 => Af   : P001 => Af   : P00z => Af
  : P010 => At   : P011 => At   : P01z => At
  : P0y0 => Ay1  : P0y1 => Ay1  : P0yy => Ay1  : P0yz => Ay1
  : P100 => At   : P101 => At   : P10z => At
  : P110 => Af   : P111 => Af   : P11z => Af
  : P1y0 => Ay0  : P1y1 => Ay0  : P1yy => Ay0  : P1yz => Ay0
  : Px00 => Ax1  : Px01 => Ax1  : Px0x => Ax1  : Px0z => Ax1
  : Px10 => Ax0  : Px11 => Ax0  : Px1x => Ax0  : Px1z => Ax0
  : Pxx0 => Af   : Pxx1 => Af   : Pxxx => Af   : Pxxz => Af
  : Pxy0 => Axny : Pxy1 => Axny : Pxyx => Axny : Pxyy => Axny : Pxyz => Axny
  .
: F1101, pattern =>           // Imp   valid triples:  100 101 010 111
  MATCH pattern
  : P000 => Af   : P001 => Af   : P00z => Af
  : P010 => At   : P011 => Af   : P01z => Az0
  : P0y0 => Ay1  : P0y1 => Af   : P0yy => Af   : P0yz => Ay1z0
  : P100 => At   : P101 => At   : P10z => At
  : P110 => Af   : P111 => At   : P11z => Az1
  : P1y0 => Ay0  : P1y1 => At   : P1yy => At   : P1yz => Akeep
  : Px00 => Ax1  : Px01 => Ax1  : Px0x => Ax1  : Px0z => Ax1
  : Px10 => Ax0  : Px11 => Ax1  : Px1x => At   : Px1z => Azpx
  : Pxx0 => Af   : Pxx1 => Ax1  : Pxxx => Ax1  : Pxxz => Az1x1
  : Pxy0 => Axny : Pxy1 => Ax1  : Pxyx => Akeep: Pxyy => Ax1  : Pxyz => Akeep
  .
: F1110, pattern =>           // Nand  valid triples:  100 101 110 011
  MATCH pattern
  : P000 => Af   : P001 => Af   : P00z => Af
  : P010 => Af   : P011 => At   : P01z => Az1
  : P0y0 => Af   : P0y1 => Ay1  : P0yy => Ay1  : P0yz => Ay1z1
  : P100 => At   : P101 => At   : P10z => At
  : P110 => At   : P111 => Af   : P11z => Az0
  : P1y0 => At   : P1y1 => Ay0  : P1yy => Ay0  : P1yz => Akeep
  : Px00 => Ax1  : Px01 => Ax1  : Px0x => Ax1  : Px0z => Ax1
  : Px10 => Ax1  : Px11 => Ax0  : Px1x => Af   : Px1z => Aznx
  : Pxx0 => Ax1  : Pxx1 => Af   : Pxxx => Af   : Pxxz => Az0x1
  : Pxy0 => Ax1  : Pxy1 => Axny : Pxyx => Ax1y0: Pxyy => Axny : Pxyz => Akeep
  .
: F1111, pattern =>           // TT    valid triples:  100 101 110 111
  MATCH pattern
  : P000 => Af   : P001 => Af   : P00z => Af
  : P010 => Af   : P011 => Af   : P01z => Af
  : P0y0 => Af   : P0y1 => Af   : P0yy => Af   : P0yz => Af
  : P100 => At   : P101 => At   : P10z => At
  : P110 => At   : P111 => At   : P11z => At
  : P1y0 => At   : P1y1 => At   : P1yy => At   : P1yz => At
  : Px00 => Ax1  : Px01 => Ax1  : Px0x => Ax1  : Px0z => Ax1
  : Px10 => Ax1  : Px11 => Ax1  : Px1x => Ax1  : Px1z => Ax1
  : Pxx0 => Ax1  : Pxx1 => Ax1  : Pxxx => Ax1  : Pxxz => Ax1
  : Pxy0 => Ax1  : Pxy1 => Ax1  : Pxyx => Ax1  : Pxyy => Ax1 : Pxyz => Ax1
  .
:                => Abad

FUN actioncheck : => // Check the validity of the above function
                     // since its accuracy is important
  FOR op = F0000 TO F1111 DO
  { LET fv = CVEC 7
    FOR i = 0 TO 7 DO fv%i := 0
    FOR yz = 0 TO 3 DO { LET x = op >> (3-yz) & 1
                         fv%(4*x+yz) := 1
                       }
//    writef("\n%4b %s: ", op, opstr op)
//    FOR i = 0 TO 7 DO writef(" %d", fv%i)
//    newline()
    FOR xv = 0 TO 2 DO           // encoding: 0=0  1=1  2=a  3=b  4=c
      FOR yv = 0 TO xv=2->3,2 DO
        FOR zv = 0 TO xv=2->yv=3->4,3,
                            yv=2->3,2 DO
        { LET pattern = pat(xv, yv, zv)
          LET act = action(op, pattern)

          LET bits = 0  // conditions:    x=0 y=0 z=0 y=z z=x x=y
                        // bits:        F YN  YN  YN  YN  YN  YN
                        // F=1 means a matching xyz is invalid
                        // Y=1 means condition could be true
                        // N=1 means condition could be false
                        // for for some valid xyz satisfying th pattern
//        writef "Valid triple: "
          FOR abc = 0 TO 7 DO
          { LET a = abc >> 2 & 1
            LET b = abc >> 1 & 1
            LET c = abc >> 0 & 1
            LET x =                       xv=2 -> a, xv
            LET y =            yv=3 -> b, yv=2 -> a, yv
            LET z = zv=4 -> c, zv=3 -> b, zv=2 -> a, zv
            // xyz iterates over all setting that satisfy the pattern

            TEST fv%(4*x+2*y+z) // test if x = y op z
            THEN { bits |:= x=0 -> #b10_00_00_00_00_00,
                                   #b01_00_00_00_00_00
                   bits |:= y=0 -> #b00_10_00_00_00_00,
                                   #b00_01_00_00_00_00
                   bits |:= z=0 -> #b00_00_10_00_00_00,
                                   #b00_00_01_00_00_00
                   bits |:= y=z -> #b00_00_00_10_00_00,
                                   #b00_00_00_01_00_00
                   bits |:= z=x -> #b00_00_00_00_10_00,
                                   #b00_00_00_00_01_00
                   bits |:= x=y -> #b00_00_00_00_00_10,
                                   #b00_00_00_00_00_01
//                 writef(" %d%d%d", x, y, z)
                 }
            ELSE bits |:= #b1_00_00_00_00_00_00 // x ~= y op z
          }
//        newline()
          // Find out which action is applicable.
          // This depends on the pattern and bits as follows

          LET act1 = bitsact(pattern, bits)

          UNLESS act=act1 DO                       // Make the check
            writef("%4b %s %s %s bits %13b => %s\n",
                    op, opstr op, patstr pattern, actstr act, 
                    bits, actstr act1)
        }
  }

FUN bitsact
: P000|P001|P010|P011|P100|P101|P110|P111,     bits => // no variables
  IF bits & #b1_00_00_00_00_00_00
                               RETURN Af
                               RETURN At

:Px00|Px01|Px0x|Px10|Px11|Px1x|Pxx0|Pxx1|Pxxx, bits => // only x
  MATCH #b11_00_00_00_00_00 & bits
  :     #b00_00_00_00_00_00 => RETURN Af
  :     #b10_00_00_00_00_00 => RETURN Ax0
  :     #b01_00_00_00_00_00 => RETURN Ax1
  :                         => RETURN At
  .
:P0y0|P0y1|P0yy|P1y0|P1y1|P1yy,                bits => // only y
  MATCH #b00_11_00_00_00_00 & bits
  :     #b00_00_00_00_00_00 => RETURN Af
  :     #b00_10_00_00_00_00 => RETURN Ay0
  :     #b00_01_00_00_00_00 => RETURN Ay1
  :                         => RETURN At
  .
:P00z|P01z|P10z|P11z,                          bits => // only z
  MATCH #b00_00_11_00_00_00 & bits
  :     #b00_00_00_00_00_00 => RETURN Af
  :     #b00_00_10_00_00_00 => RETURN Az0
  :     #b00_00_01_00_00_00 => RETURN Az1
  :                         => RETURN At
  .
:P0yz|P1yz,                                    bits => // y and z
  MATCH #b00_11_11_00_00_00 & bits
  :     #b00_00_00_00_00_00 => RETURN Af       // yz
  :     #b00_10_10_00_00_00 => RETURN Ay0z0    // 00
  :     #b00_10_01_00_00_00 => RETURN Ay0z1    // 01
  :     #b00_01_10_00_00_00 => RETURN Ay1z0    // 10
  :     #b00_01_01_00_00_00 => RETURN Ay1z1    // 11
  :     #b00_10_11_00_00_00 => RETURN Ay0      // 00 01
  :     #b00_01_11_00_00_00 => RETURN Ay1      // 10 11
  :     #b00_11_10_00_00_00 => RETURN Az0      // 00 10
  :     #b00_11_01_00_00_00 => RETURN Az1      // 01 11
  :  => EXIT
  .
  MATCH #b00_00_00_11_00_00 & bits
  :     #b00_00_00_10_00_00 => RETURN Aypz     // 00 11
  :     #b00_00_00_01_00_00 => RETURN Aynz     // 01 10
  :  => IF bits & #b1_00_00_00_00_00_00
                               RETURN Akeep    // 3 of   00 01 10 11
                               RETURN At       // all of 00 01 10 11
  .
:Px0z|Px1z|Pxxz,                               bits => // z and x
  MATCH #b11_00_11_00_00_00 & bits
  :     #b00_00_00_00_00_00 => RETURN Af       // zx
  :     #b10_00_10_00_00_00 => RETURN Az0x0    // 00
  :     #b01_00_10_00_00_00 => RETURN Az0x1    // 01
  :     #b10_00_01_00_00_00 => RETURN Az1x0    // 10
  :     #b01_00_01_00_00_00 => RETURN Az1x1    // 11
  :     #b11_00_10_00_00_00 => RETURN Az0      // 00 01
  :     #b11_00_01_00_00_00 => RETURN Az1      // 10 11
  :     #b10_00_11_00_00_00 => RETURN Ax0      // 00 10
  :     #b01_00_11_00_00_00 => RETURN Ax1      // 01 11
  :  => EXIT
  .
  MATCH #b00_00_00_00_11_00 & bits
  :     #b00_00_00_00_10_00 => RETURN Azpx     // 00 11
  :     #b00_00_00_00_01_00 => RETURN Aznx     // 01 10
  :  => IF bits & #b1_00_00_00_00_00_00
                               RETURN Akeep    // 3 of   00 01 10 11
                               RETURN At       // all of 00 01 10 11
  .
:Pxy0|Pxy1|Pxyx|Pxyy,                          bits => // x and y
  MATCH #b11_11_00_00_00_00 & bits
  :     #b00_00_00_00_00_00 => RETURN Af       // xy
  :     #b10_10_00_00_00_00 => RETURN Ax0y0    // 00
  :     #b10_01_00_00_00_00 => RETURN Ax0y1    // 01
  :     #b01_10_00_00_00_00 => RETURN Ax1y0    // 10
  :     #b01_01_00_00_00_00 => RETURN Ax1y1    // 11
  :     #b10_11_00_00_00_00 => RETURN Ax0      // 00 01
  :     #b01_11_00_00_00_00 => RETURN Ax1      // 10 11
  :     #b11_10_00_00_00_00 => RETURN Ay0      // 00 10
  :     #b11_01_00_00_00_00 => RETURN Ay1      // 01 11
  :  => EXIT
  .
  MATCH #b00_00_00_00_00_11 & bits
  :     #b00_00_00_00_00_10 => RETURN Axpy     // 00 11
  :     #b00_00_00_00_00_01 => RETURN Axny     // 01 10
  :  => IF bits & #b1_00_00_00_00_00_00
                               RETURN Akeep    // 3 of   00 01 10 11
                               RETURN At       // all of 00 01 10 11
  .
:Pxyz,                                         bits => // x, y and z
  MATCH #b11_11_11_00_00_00 & bits  // valid triples a00 b01 c10 d11
//:     #b11_10_10_00_00_00 => RETURN Ay0z0
//:     #b11_10_01_00_00_00 => RETURN Ay0z1
//:     #b11_01_10_00_00_00 => RETURN Ay1z0
//:     #b11_01_01_00_00_00 => RETURN Ay1z1
  :     #b10_11_10_00_00_00 => RETURN Az0x0
  :     #b01_11_01_00_00_00 => RETURN Az0x1
  :     #b10_11_10_00_00_00 => RETURN Az1x0
  :     #b01_11_01_00_00_00 => RETURN Az1x1
  :     #b10_10_11_00_00_00 => RETURN Ax0y0
  :     #b10_01_11_00_00_00 => RETURN Ax0y1
  :     #b01_10_11_00_00_00 => RETURN Ax1y0
  :     #b01_01_11_00_00_00 => RETURN Ax1y1
  :     #b10_11_11_00_00_00 => RETURN Ax0
  :     #b01_11_11_00_00_00 => RETURN Ax1
//:     #b11_10_11_00_00_00 => RETURN Ay0
//:     #b11_01_11_00_00_00 => RETURN Ay1
//:     #b11_11_10_00_00_00 => RETURN Az0
//:     #b11_11_01_00_00_00 => RETURN Az1
  :  => EXIT
  .
  MATCH #b00_00_00_11_11_11 & bits
//:     #b00_00_00_11_11_10 => RETURN Aypz
//:     #b00_00_00_11_11_01 => RETURN Aynz
  :     #b00_00_00_11_10_11 => RETURN Azpx
  :     #b00_00_00_11_01_11 => RETURN Aznx
  :     #b00_00_00_11_11_10 => RETURN Axpy
  :     #b00_00_00_11_11_01 => RETURN Axny
  :                         => RETURN Akeep
  .
                            
:  => Abad

STATIC actab
MANIFEST ActabUpb = #b111_11_11_11<<4 + F1111
 
FUN init_actab : =>
  actab := getvec ActabUpb
  UNLESS actab RAISE E_space
  FOR i = 0 TO ActabUpb DO actab!i := Abad
  LET p = TABLE [ P000, P001, P00z,
                  P010, P011, P01z,
                  P0y0, P0y1, P0yy, P0yz,
                  P100, P101, P10z,
                  P110, P111, P11z,
                  P1y0, P1y1, P1yy, P1yz,
                  Px00, Px01, Px0x, Px0z,
                  Px10, Px11, Px1x, Px1z,
                  Pxx0, Pxx1, Pxxx, Pxxz,
                  Pxy0, Pxy1, Pxyx, Pxyy, Pxyz,
                 -1
                ]
  { LET pat = !p+++
    IF pat<0 RETURN
    FOR op = F0000 TO F1111 DO
      actab!(pat<<4 + op) := action(op, pat)
  } REPEAT
// A term is represented by a 4-tuple: [op, x, y, z]

// Aside: when the number of distinct variables becomes less than 512
// the tuple may one day be packed into a 31 bits: 4-9-9-9, to save
// space and improve processor cache performance.

// The 4-tuples are held in a vector of 4 word cells

STATIC
  termv=0,  // base of the vector
  termp,    // next free position in the vector
  termt     // just past the end of the vector

FUN init_termv : => termv := getvec 400000
                    IF termv=0 RAISE (E_space, "no space for termv")
                    termp, termt := termv, @termv!400000

FUN mkterm : op, y, z =>
  IF termp>=termt RAISE (E_space, "Too many terms")
  
  LET p = termp
  p!0, p!1, p!2, p!3 := op, ++varn, y, z
  termp := @p!4
  RETURN varn

// Variables such as x, y and z in terms [op,x,y,z] are identified
// by positive integers. Variables may get renamed by such actions
// as: Azpx (set x=z) or Axny (set x=~y). The accumulated collection
// of mappings is held in the vector varmap.

// If varmap!x>=0 then x maps to variable varmap!x
// If varmap!x<0  then x maps to the complement of variable -varmap!x

// Whenever a term is processed these mapping are performed before
// the pattern match is done.

STATIC
  varmap=0,   // base of the varmap vector
  varmapp,    // subscript of first unused entry
  varmapt     // subscript just beyond last entry

FUN init_varmap : => varmap := getvec 100000
                     IF varmap=0 RAISE (E_space, "no space for varmap")
                     varmapp, varmapt := 0, 100000

FUN compact_vars : =>
  FOR i = 2 TO varn DO varmap!i := 0
  LET p = termv
  UNTIL p>=termp DO    // Mark vars in use
  { MATCH p : [op, x, y, z] =>
      IF op<0 EXIT     // The term was deleted
      varmap!x := 1    // Mark variables in use
      varmap!y := 1
      varmap!z := 1
    .
    p := @ p!4
  }
  LET var = 2
  varmap!0 := 0
  varmap!1 := 1
  FOR i = 2 TO varn IF varmap!i DO varmap!i := var++
  p := termv
  UNTIL p>=termp DO
  { MATCH p : [op, x, y, z] =>
      IF op<0 EXIT     // The term was deleted
      x := varmap!x    // Map each variable
      y := varmap!y
      z := varmap!z
    .
    p := @ p!4
  }
  varn := var

FUN compact_terms : =>
  LET p = termv
  LET q = p
  UNTIL p>=termp TEST !p<0
  THEN p := @p!4 // Skip deleted terms
  ELSE { UNLESS p=q DO q!0, q!1, q!2, q!3 := p!0, p!1, p!2, p!3
         p, q := @p!4, @q!4
       }
  termp := q

FUN prterms : =>
  LET k = 0
  LET p = termv
  writef "Terms:\n"
  UNTIL p>=termp MATCH p
  : [op, x, y, z] => writef("%4d %4b %s %4d %4d %4d\n",
                             k++, op, opstr op, x, y, z)
                     p +:= 4*Bpw
FUN prmapping : =>
  LET k = 0
  writef "Mapping:"
  FOR i = 0 TO 199 UNLESS i = varmap!i DO
  { UNLESS k++ MOD 5 DO newline()
    writef("  %4d=>%4d", i, varmap!i)
  }
  newline()

STATIC
  change  // Set to TRUE whenever the variable mapping changes

FUN check : rootvar => // Return TRUE if the given expression
                       // is certainly a tautology
  compact_vars()

  IF debug>1 DO { writef "Initial terms are:\n"
                  prterms()
                }

  rootvar := varmap!rootvar
  FOR i = 0 TO varmapt DO varmap!i := i // null mapping
  varmap!rootvar := 0  // Set rootvar to 0, so if the conjunction
                       // of terms can be satisfied, the given expression
                       // can evaluate to false.

  { IF direct_rules()=FALSE RETURN TRUE // The given expression a tautology
    compact_terms()

// The and rule is just plain wrong -- so don't use it
//  IF and_rule()=FALSE RETURN TRUE // The given expression a tautology
//  compact_terms()

    IF suffix_rule()=FALSE RETURN TRUE // The given expression a tautology
    compact_terms()

  } REPEATWHILE change

  IF debug>1 DO { writef "Mapping and Terms on return from check:\n"
                  prmapping()
                  prterms()
                }

  RETURN FALSE

FUN direct_rules : => // This function applied the current mapping 
                      // and the direct rules until convergence,
                      // leaving the final mapping in varmap.
                      // It returns FALSE if there is a term that
                      // cannot be satisfied, and TRUE otherwise.

  { change := FALSE // changes to TRUE if the variable mapping changes

    IF debug>3 DO { writef("\nApplying direct rules to:\n")
                    prmapping()
                    prterms()
                  }

    LET p = termv
    UNTIL p>=termp DO
    { MATCH p : [op, x, y, z] =>
        IF op<0 EXIT

//      writef("checking term: %4b %s %3d %3d %3d\n", op, opstr op, x, y, z)

        UNTIL x = varmap!x DO            // The mapped variable may itself
        { x := varmap!x                  // be mapped. Its monotonically
          IF x<0 DO op, x := notx op, -x // decreasing so loop terminates.
        }
        UNTIL y = varmap!y DO
        { y := varmap!y
          IF y<0 DO op, y := noty op, -y
        }
        UNTIL z = varmap!z DO
        { z := varmap!z
          IF z<0 DO op, z := notz op, -z
        }
        UNLESS y<=z DO op, y, z := swapyz op, z, y  

        IF x=1 DO op, x := notx op, 0
        IF y=1 DO op, y := noty op, 0
        IF z=1 DO op, z := notz op, 0

        LET pattern = pat(x, y, z)
//        LET act = action(op, pattern)
        LET act = actab!(op + pattern<<4)

        UNLESS act=Akeep IF debug>2 DO
        { writef("term: %4b %s %3d %3d %3d", op, opstr op, x, y, z)
          writef(" %s => %s\n", patstr pattern, actstr act)
        }

        MATCH act
        : Af    => // The term is always false, so the whole expression
                   // is false, so the original expression is a tautology.
                   RETURN FALSE
        : At    => op := -1               // The term is true
                   
        : Ax0   => op, varmap!x := -1, 0               // Set x=0
                   change := TRUE
        : Ax1   => op, varmap!x := -1, 1               // Set x=1
                   change := TRUE
        : Ay0   => op, varmap!y := -1, 0               // Set y=0
                   change := TRUE
        : Ay1   => op, varmap!y := -1, 1               // Set y=1
                   change := TRUE
        : Az0   => op, varmap!z := -1, 0               // Set z=0
                   change := TRUE
        : Az1   => op, varmap!z := -1, 1               // Set z=1
                   change := TRUE

        : Ay0z0 => op, varmap!y, varmap!z := -1, 0, 0  // Set y=0, z=0
                   change := TRUE
        : Ay0z1 => op, varmap!y, varmap!z := -1, 0, 1  // Set y=0, z=1
                   change := TRUE
        : Ay1z0 => op, varmap!y, varmap!z := -1, 1, 0  // Set y=1, z=0
                   change := TRUE
        : Ay1z1 => op, varmap!y, varmap!z := -1, 1, 1  // Set y=1, z=1
                   change := TRUE

        : Az0x0 => op, varmap!z, varmap!x := -1, 0, 0  // Set z=0, x=0
                   change := TRUE
        : Az0x1 => op, varmap!z, varmap!x := -1, 0, 1  // Set z=0, x=1
                   change := TRUE
        : Az1x0 => op, varmap!z, varmap!x := -1, 1, 0  // Set z=1, x=0
                   change := TRUE
        : Az1x1 => op, varmap!z, varmap!x := -1, 1, 1  // Set z=1, x=1
                   change := TRUE

        : Ax0y0 => op, varmap!x, varmap!y := -1, 0, 0  // Set x=0, y=0
                   change := TRUE
        : Ax0y1 => op, varmap!x, varmap!y := -1, 0, 1  // Set x=0, y=1
                   change := TRUE
        : Ax1y0 => op, varmap!x, varmap!y := -1, 1, 0  // Set x=1, y=0
                   change := TRUE
        : Ax1y1 => op, varmap!x, varmap!y := -1, 1, 1  // Set x=1, y=1
                   change := TRUE

        : Aynz  => TEST y>z THEN varmap!y := -z        // Set y=~z
                            ELSE varmap!z := -y        // Set z=~y
                   op, change := -1, TRUE
        : Aznx  => TEST z>x THEN varmap!z := -x        // Set z=~x
                            ELSE varmap!x := -z        // Set x=~z
                   op, change := -1, TRUE
        : Axny  => TEST x>y THEN varmap!x := -y        // Set x=~y
                            ELSE varmap!y := -x        // Set y=~x
                   op, change := -1, TRUE

        : Aypz  => TEST y>z THEN varmap!y := z         // Set y=z
                            ELSE varmap!z := y         // Set z=y
                   op, change := -1, TRUE
        : Azpx  => TEST z>x THEN varmap!z := x         // Set z=x
                            ELSE varmap!x := z         // Set x=z
                   op, change := -1, TRUE
        : Axpy  => TEST x>y THEN varmap!x := y         // Set x=y
                            ELSE varmap!y := x         // Set z=x
                   op, change := -1, TRUE

                        // For all the above actions remove the term

        : Akeep =>      // Nothing can be deduced, keep the term    
        :   act => writef("Unknown action %d\n", act)
        .
      .
      p := @p!4
    }
  } REPEATWHILE change // repeat if varmap changed

  RETURN TRUE  // No more direct rule reduction possible
 
FUN and_rule : => // Return FALSE if one of the terms is
                  // definitly false

  // Look for pairs of the form: [op , x, y, z]
  //                             [op', x, y, z]
  // replace pair by          [op&op', x, y, z]
  // So first sort terms (increasing z, y, x)

  LET n = (termp-termv)/(Bpw*4)  // The number of terms
  UNLESS n RETURN TRUE // No terms left

  LET m = 1
  UNTIL m>n DO m := m*3 + 1  // Find first suitable value in the
                             // series:  1, 4, 13, 40, 121, 364, ...

  { m := m/3
    FOR i = m TO n-1 DO
      MATCH @termv!(i*4) : [op, x, y, z] =>
        IF op<0 LOOP
        LET opi=op, xi=x, yi=y, zi=z
        LET j = i
        { LET k = j - m
          IF k<0 BREAK
          MATCH @termv!(4*k) : [opk, xk, yk, zk] =>
            UNLESS opk<0
              IF zk<zi OR zk=zi AND (yk<yi OR yk=yi AND xk<xi) BREAK
            MATCH @termv!(4*j) : [:=opk, :=xk, :=yk, :=zk] => j := k
        } REPEAT
        MATCH @termv!(4*j) : [:=opi, :=xi, :=yi, :=zi] => LOOP
  } REPEATUNTIL m=1

  IF debug>3 DO
  { writef("Applying And-rule on:\n")
    prmapping()
    prterms()
  }

  LET p = termv
  UNTIL p>=termp DO // Look for And-rule pairs
    MATCH p
    : [op, x, y, z] =>
      LET q = p+4*Bpw
      UNTIL q>=termp DO
        MATCH q
        : [op1, =x, =y, =z] =>
IF debug>2 DO
{ writef("And-rule applies to terms:\n")
  writef("        %4b %s %5d %5d %5d\n", op,  opstr op,  x, y, z)
  writef("        %4b %s %5d %5d %5d\n", op1, opstr op1, x, y, z)
}
                               UNLESS op=op1 DO change := TRUE
                               op, op1 := op&op1, -1
IF debug>2 DO 
  writef("Giving: %4b %s %5d %5d %5d\n", op,  opstr op,  x, y, z)
                               IF op=F0000 RETURN FALSE
                               q +:= 4*Bpw
        :                   => p := q
                               BREAK
        .
    
  RETURN TRUE // Possibly all terms can be satisfied

FUN suffix_rule : => // Returns FALSE if one of the terms
                     // is certainly unsatisfiable.

  // Look for pairs of the form: [op, x,  y, z]
  //                             [op, x', y, z]
  // then delete                 [op, x', y, z]
  // and map                     x=x'

  // Canonicalise op by negating if of the form F1xxx

  FOR p = termv TO termp-1 BY 4*Bpw DO
    MATCH p : [<F1000,      ?, ?, ?] => LOOP
            : [    op, (<=1)x, ?, ?] => op, x := notx op, 1-x
            : [    op,      x, ?, ?] => op, x := notx op, -x
            .

  // So first sort terms (increasing z, y, op)

  LET n = (termp-termv)/(Bpw*4)  // The number of terms
  UNLESS n RETURN TRUE // No terms left

  LET m = 1
  UNTIL m>n DO m := m*3 + 1  // Find first suitable value in the
                             // series:  1, 4, 13, 40, 121, 364, ...

  { m := m/3
    FOR i = m TO n-1 DO
      MATCH @termv!(i*4) : [op, x, y, z] =>
        IF op<0 LOOP
        LET opi=op, xi=x, yi=y, zi=z
        LET j = i
        { LET k = j - m
          IF k<0 BREAK
          MATCH @termv!(4*k) : [opk, xk, yk, zk] =>
            UNLESS opk<0
              IF zk<zi OR zk=zi AND (yk<yi OR yk=yi AND opk<opi) BREAK
            MATCH @termv!(4*j) : [:=opk, :=xk, :=yk, :=zk] => j := k
        } REPEAT
        MATCH @termv!(4*j) : [:=opi, :=xi, :=yi, :=zi] => LOOP
  } REPEATUNTIL m=1

  IF debug>3 DO
  { writef("Applying Suffix-rule on:\n")
    prmapping()
    prterms()
  }

  LET p = termv
  UNTIL p>=termp DO // Look for Suffix-rule pairs
    MATCH p
    : [op, x, y, z] =>
      IF op<0 BREAK  // The void terms are at the end
      LET q = p+4*Bpw
      UNTIL q>=termp DO
      { MATCH q
        : [op1(=op), x1, =y, =z] =>
IF debug>2 DO
{ writef("Suffix-rule applies to terms:\n")
  writef("        %4b %s %5d %5d %5d\n", op, opstr op,  x, y, z)
  writef("        %4b %s %5d %5d %5d\n", op, opstr op, x1, y, z)
}
                               op1 := -1
                               IF x=0 AND x1=1 OR
                                  x=1 AND x1=0 RETURN FALSE
                               LET a = ABS x
                               LET b = ABS x1
                               TEST a>b
                               THEN b := x<0 -> -x1, x1
                               ELSE a, b := b, x1<0 -> -x, x
                               q +:= 4*Bpw
                               IF a=b LOOP
                               IF a=-b RETURN FALSE
                               varmap!a := b
IF debug>2 DO
  writef("giving mapping %5d -> %5d\n", a, b)
                               change := TRUE
        :                   => BREAK
        .
      }
      p := q
    .
  // Make x positive again

  FOR p = termv TO termp-1 BY 4*Bpw DO
    MATCH p : [  <0,   ?, ?, ?] => LOOP
            : [   ?, >=0, ?, ?] => LOOP
            : [  op,   x, ?, ?] => op, x := notx op, -x
            .

  RETURN TRUE // All the terms may be satisfiable.

//********************* Syntax Analyser ******************

// This converts the ASCII representation of a propositional
// expression into a set of terms, each of the form
//        [op, x, y, z]
// where x, y, and z are variable ids
// It returns the id of the root of the entire expression.

// 0 .. 1 -->  0 .. 1
// A .. Z -->  2 .. 37
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

: '0'..'1'   => token, lexval := Id, ch-'0';   rch()
: 'A'..'Z'   => token, lexval := Id, ch-'A'+2; rch()
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
  varn=0

FUN parse : str => lex_init str
                   varn := 100
                   termp := termv
                   RETURN nexp 0

FUN prim : => MATCH token
: Id     => LET a = lexval
            lex()
            a
: Lparen => LET a = nexp 0
            UNLESS token=Rparen RAISE E_syntax
            lex()
            a
: NotY   => mkterm(NotY, nexp 3, 0)
:  ?     => RAISE E_syntax

FUN nexp : n => lex(); exp n

FUN exp : n =>
  LET a = prim()

  { MATCH (token, n)
    : And, <3 => a := mkterm(And, a, nexp 3) 
    : Or,  <2 => a := mkterm(Or , a, nexp 2) 
    : Imp, <1 => a := mkterm(Imp, a, nexp 1) 
    : Eqv, <1 => a := mkterm(Eqv, a, nexp 1) 
    :         => RETURN a
  } REPEAT

//********************* Space Allocation ******************
//                (NOT needed at the moment)

STATIC
  spacev, spacep

FUN mk_init : upb => spacev := getvec upb
                     IF spacev=0 RAISE E_space
                     spacep := @ spacev!upb

FUN mk_close :    => freevec spacev

FUN mk1 : x       => !---spacep := x; spacep
FUN mk2 : x, y    => mk1 y; mk1 x
FUN mk3 : x, y, z => mk1 z; mk1 y; mk1 x

//********************* Main Program **********************


FUN try : e => 
  { writef("\nTesting: %s\n", e)

    mk_init 100_000

    TEST check(parse e)
    THEN writef("-------- It is a tautology\n")
    ELSE TEST termv=termp
         THEN writef("-------- It is a NOT tautology\n")
         ELSE writef("-------- More work needed\n")

    mk_close()
  } HANDLE : E_syntax => writef "Bad Syntax\n"
           : E_space  => writef "Insufficient space\n"

// Propositional examples supplied by Larry Paulson 
// and modified by MR

STATIC debug=0

FUN start : =>
    LET argv = VEC 50

    IF rdargs("D,TO/K", argv, 50)=0 DO
    { writef "Bad arguments for STLMK\n"
      RETURN 20
    }

    LET sysout = output
    LET out    = 0

    IF argv!0 DO debug := str2numb(argv!0)

    IF argv!1 DO { out := findoutput(argv!1)
                   IF out=0 DO
                   { writef "Bad arguments for STLMK\n"
                     RETURN 20
                   }
                   selectoutput(out)
                 }

    init_termv()
    init_varmap()
    init_actab()

    actioncheck()

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

    freevec actab
    freevec termv
    freevec varmap
    IF out DO endwrite out
    RETURN 0





