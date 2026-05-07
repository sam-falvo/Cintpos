GET "mcpl.h"

/* This is a free translation into MCPL by M. Richards
   of Stalmarck's algorithm for tautology checking.
   It is based on the paper:

   G. Stalmarck and M. Saflund, Modelling and Verifying Systems and
   Software in Propositional Logic, IFAC, SAFECOMP90, London, UK 1990

   The algorithm has a patent pending (or is already patented).
*/

MANIFEST 
// The 16 boolean operators are represented by binary integers 0000..1111
// and have manifest names of the form: Opabcd

// where      0 Opabcd 0 => a 
//            0 Opabcd 1 => b 
//            1 Opabcd 0 => c 
//            1 Opabcd 1 => d

// For example, Op0001=1  and represents And 
// and          Op1101=13 and represents Imp 

Op0000= 0, Op0001= 1, Op0010= 2, Op0011= 3, 
Op0100= 4, Op0101= 5, Op0110= 6, Op0111= 7, 
Op1000= 8, Op1001= 9, Op1010=10, Op1011=11, 
Op1100=12, Op1101=13, Op1110=14, Op1111=15, 

Id, Lparen, Rparen,Eof,

Not =Op1100, 
And =Op0001,
Or  =Op0111,
Imp =Op1101,
Eqv =Op1001,

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
// eee = 100 means y=z, x different
//     = 010 means x=z, y different
//     = 001 means x=y, z different
//     = 000 means all different
//     = 111 means all the same

// xx  = 00  nothing known about x, could be 0 or 1
//     = 01  x=0
//     = 10  x=1
//     = 11  error

// yy and zz are defined similarly

// The 37 possible operand patterns have the following manifest names

P000 = #b111_01_01_01,
P001 = #b001_01_01_10,
P00z = #b001_01_01_00,
P010 = #b010_01_10_01,
P011 = #b100_01_10_10,
P01z = #b000_01_10_00,
P0y0 = #b010_01_00_01,
P0y1 = #b000_01_00_10,
P0yy = #b100_01_00_00,
P0yz = #b000_01_00_00,
P100 = #b100_10_01_01,
P101 = #b010_10_01_10,
P10z = #b000_10_01_00,
P110 = #b001_10_10_01,
P111 = #b111_10_10_10,
P11z = #b001_10_10_00,
P1y0 = #b000_10_00_01,
P1y1 = #b010_10_00_10,
P1yy = #b100_10_00_00,
P1yz = #b000_10_00_00,
Px00 = #b100_00_01_01,
Px01 = #b000_00_01_10,
Px0x = #b010_00_01_00,
Px0z = #b000_00_01_00,
Px10 = #b000_00_10_01,
Px11 = #b100_00_10_10,
Px1x = #b010_00_10_00,
Px1z = #b000_00_10_00,
Pxx0 = #b001_00_00_01,
Pxx1 = #b001_00_00_10,
Pxxx = #b111_00_00_00,
Pxxz = #b001_00_00_00,
Pxy0 = #b000_00_00_01,
Pxy1 = #b000_00_00_10,
Pxyx = #b010_00_00_00,
Pxyy = #b100_00_00_00,
Pxyz = #b000_00_00_00

// pat(op,x,y,z) returns the operand pattern

FUN pat : op, x, y, z => // Return pattern of operands
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

  At,      // The term is true
  Ax0,     // Set x=0
  Ax1,     // Set x=1
  Ay0,     // Set y=0
  Ay1,     // Set y=1
  Az0,     // Set z=0
  Az1,     // Set z=1
  Ay1z0,   // Set y=1, z=0
  Ax1z1,   // Set x=1, z=1
  Axny,    // Set x=~y if x<y, otherwise set y=x
  Axpz,    // Set x=z  if x<z, otherwise set z=x
           // For all the above actions remove the term

  Akeep    // Nothing can be deduced, keep the term    

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
                    termp, termt := 0, 400000

FUN addterm : op, x, y, z => // a better implementation will follow
  IF y>z DO // Swap operands and correct op
  { y, z := z, y
    op := [Op0000, Op0001, Op0100, Op0101,
           Op0010, Op0011, Op0110, Op0111,
           Op1000, Op1001, Op1100, Op1101,
           Op1010, Op1011, Op1110, Op1111] ! op
  }

  // Should check whether this term is already in the set

  IF termp>=termt RAISE (E_space, "Too many terms")
  
  LET p = termp
  p!0, p!1, p!2, p!3 := op, x, y, z
  termp := @p!4
  RETURN x

// Variables such as x, y and z in terms [op,x,y,z] are identified
// by positive integers. Variables may get renamed by such actions
// as: Axpz (set x=z) or Axny (set x=~y). The accumulated collection
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
                     FOR i = 0 TO varmapt DO varmap!i := i // null mapping

FUN check : => TRUE
 
//********************* Lexical Analyser ******************

STATIC
  strp, ch, nch, token, lexval

FUN lex_init : str => strp := str; rch(); rch()

FUN rch : => ch, nch := nch, %strp
             UNLESS nch=0 DO strp++

FUN lex : => MATCH (ch, nch)

: ' ' | '\n' => rch(); lex()

: '0'..'1'   => token, lexval := Id, ch; rch()-'0' 
: 'A'..'Z'   => token, lexval := Id, ch; rch()-'A'+2
: '('        => token := Lparen; rch()
: ')'        => token := Rparen; rch()
: '~'        => token := Not;    rch()
: '&'        => token := And;    rch()
: '|'        => token := Or;     rch()
: '='        => token := Eqv;    rch()

: '-', '>'   => token := Imp; rch(); rch()

: 0          => token := Eof

:            => RAISE E_syntax

//********************* Syntax Analyser ******************

// This converts the ASCII representation of a propositional
// expression into a set of terms, each of the form
//        [op, x, y, z]
// where x, y, and z are variable ids
// It returns the id of the root of the entire expression.

// 0 .. 1 -->  0 .. 1
// A .. Z -->  2 .. 37
// ~x     -->  [Not, t, x, 0]
// x & y  -->  [And, t, x, y]
// x | y  -->  [Or,  t, x, y]
// x -> y -->  [Imp, t, x, y]
// x = y  -->  [Eqv, t, x, y]

STATIC
  varn=0

FUN parse : str => lex_init str
                   varn := 100
                   LET rootvar = nexp 0
                   LET p = termv
                   UNTIL p >=termp DO
                   { LET op=p!0, x=p!1, y=p!2, z=p!3
                     writef("%4b %3d %3d%3d\n", op, x, y, z)
                     p := @p!4
                   }
                   rootvar


FUN prim : => MATCH token
: Id     => LET a = lexval
            lex()
            a
: Lparen => LET a = nexp 0
            UNLESS token=Rparen RAISE E_syntax
            lex()
            a
: Not    => addterm(Not, ++varn, nexp 3, 0)
:  ?     => RAISE E_syntax

FUN nexp : n => lex(); exp n

FUN exp : n =>
  LET a = prim()

  { MATCH (token, n)
    : And, <3 => a := addterm(And, ++varn, a, nexp 3) 
    : Or,  <2 => a := addterm(Or , ++varn, a, nexp 2) 
    : Imp, <1 => a := addterm(Imp, ++varn, a, nexp 1) 
    : Eqv, <1 => a := addterm(Eqv, ++varn, a, nexp 1) 
    :         => RETURN a
  } REPEAT

//********************* Space Allocation ******************

STATIC
  spacev, spacep

FUN mk_init : upb =>
  spacev := getvec upb
  IF spacev=0 RAISE E_space
  spacep := @ spacev!upb

FUN mk_close : =>
  freevec spacev

FUN mk1 : x => !---spacep := x; spacep

FUN mk2 : x, y => mk1 y; mk1 x

FUN mk3 : x, y, z => mk1 z; mk1 y; mk1 x

//********************* Main Program **********************


FUN try : e => 
  { writef("\nTesting: %s\n", e)

    mk_init 100_000

    TEST check(parse e)
    THEN writef("-------- It is a tautology\n")
    ELSE writef("-------- It is NOT a tautology\n")
  } HANDLE : E_syntax => writef "Bad Syntax\n"
           : E_space  => writef "Insufficient space\n"
           .
  mk_close()

// Propositional examples supplied by Larry Paulson 
// and modified by MR

FUN start : =>

    init_termv()
    init_varmap()

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

    freevec termv
    freevec varmap
    RETURN 0





