GET "mcpl.h"

/* This is a very free translation into MCPL by M. Richards
   of the LISP version of the Wang Algorithm given in the
   LISP 1.5 book.
*/

MANIFEST 
Id, Not, And, Or, Imp, Eqv,  // Lexical tokens
Lparen, Rparen,Eof,
E_syntax=100, E_space        // Exceptions

FUN member : ?,        0 => FALSE
           : x, [=x,  ?] => TRUE
           : x, [ ?, ys] => member(x, ys)
.
FUN add : x, xs => member(x, xs) -> xs, mk2(x, xs)
.
FUN pr
:  ?,  0,  ?,               0 => FALSE

: al,  0, ar,   [[Not,x], cr] => pr( al, [x,0], ar, cr )

: al,  0, ar, [[And,x,y], cr] => pr( al, 0, ar, [x,cr] )
                                 AND
                                 pr( al, 0, ar, [y,cr] )

: al,  0, ar,  [[Or,x,y], cr] => pr( al, 0, ar, [x,[y,cr]], x)

: al,  0, ar, [[Imp,x,y], cr] => pr( al,[x,0], ar, [y,cr], x)

: al,  0, ar, [[Eqv,x,y], cr] => pr( al,[x,0], ar, [y,cr], x)
                                 AND
                                 pr( al,[y,0], ar, [x,cr], x)

: al,  0, ar,    [[Id,x], cr] => member(x, al)
                                 OR
                                 pr( al, 0, add(x,ar), cr )

: al,   [[Not,x],cl], ar, cr  => pr( al, cl, ar, [x,cr] )

: al, [[And,x,y],cl], ar, cr  => pr( al, [x,[y,cl]], ar, cr )

: al,  [[Or,x,y],cl], ar, cr  => pr( al, [x,cl], ar, cr )
                                 AND
                                 pr( al, [y,cl], ar, cr )

: al, [[Imp,x,y],cl], ar, cr  => pr( al, [y,cl], ar, cr )
                                 AND
                                 pr( al, cl, ar, [x,cr] )

: al, [[Eqv,x,y],cl], ar, cr  => pr( al, [x,[y,cl]], ar, cr )
                                 AND
                                 pr( al, cl, ar, [x,[y,cr]] )

: al,    [[Id,x],cl], ar, cr  => member(x,ar)
                                 OR
                                 pr( add(x,al), cl, ar, cr )
.

//********************* Lexical Analyser ******************

STATIC
  strp, ch, nch, token, lexval

FUN lex_init : str => strp := str; rch(); rch()
.
FUN rch : => ch, nch := nch, %strp
             UNLESS nch=0 DO strp++
.
FUN lex : => MATCH (ch, nch)

: ' ' | '\n' => rch(); lex()

: 'A'..'Z'   => token, lexval := Id, ch; rch() 
: '('        => token := Lparen; rch()
: ')'        => token := Rparen; rch()
: '~'        => token := Not;    rch()
: '&'        => token := And;    rch()
: '|'        => token := Or;     rch()
: '='        => token := Eqv;    rch()

: '-', '>'   => token := Imp; rch(); rch()

: 0          => token := Eof

:            => RAISE E_syntax
.
.
//********************* Syntax Analyser ******************


// A .. Z -->  [Id, 'A'] .. [Id, 'Z']
// ~x     -->  [Not, x]
// x & y  -->  [And,x,y]
// x | y  -->  [Or,x,y]
// x -> y -->  [Imp,x,y]
// x = y  -->  [Eqv,x,y]

FUN parse : str => lex_init str
                   LET tree = nexp 0
                   //writef("Parse tree:\n")
                   //prtree (tree, 0, 20)
                   //newline()
                   tree
.

FUN prim : => MATCH token
: Id     => LET a = mk2(Id, lexval)
            lex()
            a
: Lparen => LET a = nexp 0
            UNLESS token=Rparen RAISE E_syntax
            lex()
            a
: Not    => mk2(Not, nexp 3)
:  ?     => RAISE E_syntax
.
.
FUN nexp : n => lex(); exp n
.
FUN exp : n =>
  LET a = prim()

  { MATCH (token, n)
    : And, <3 => a := mk3(And, a, nexp 3) 
    : Or,  <2 => a := mk3(Or , a, nexp 2) 
    : Imp, <1 => a := mk3(Imp, a, nexp 1) 
    : Eqv, <1 => a := mk3(Eqv, a, nexp 1) 
    :         => RETURN a
    .
  } REPEAT
.
//********************* Space Allocation ******************

STATIC
  spacev, spacep

FUN mk_init : upb =>
  spacev := getvec upb
  IF spacev=0 RAISE E_space
  spacep := @ spacev!upb
.
FUN mk_close : =>
  freevec spacev
.
FUN mk1 : x => !---spacep := x; spacep
.
FUN mk2 : x, y => mk1 y; mk1 x
.
FUN mk3 : x, y, z => mk1 z; mk1 y; mk1 x
.
//********************* Main Program **********************


FUN try : e => 
  { writef("\nTesting: %s\n", e)

    mk_init 100_000

    TEST pr(0, 0, 0, [parse e, 0])
    THEN writef("-------- It is a tautology\n")
    ELSE writef("-------- It is NOT a tautology\n")
  } HANDLE : E_syntax => writef "Bad Syntax\n"
           : E_space  => writef "Insufficient space\n"
           .
  mk_close()
.
// Propositional examples supplied by Larry Paulson 
// and modified by MR

FUN start : =>
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
.

/**************************************************************
*
*            Print Tree Functions and Data
*
*    prtree(tree, depth, maxdepth)
*    
*    prlinev
*    
*    
*
**************************************************************/

STATIC
  prlinev = VEC 50

FUN prtree : x, depth, maxdepth => 

  LET opstr=0, upb=0

  IF x=0 DO { writef "Nil"; RETURN }
 
  { MATCH x
    : [Id, ch]            => writef("%c", ch);     RETURN
    : [And, x, y]         => opstr, upb := "And",      2
    : [Or,  x, y]         => opstr, upb := "Or",       2
    : [Imp, x, y]         => opstr, upb := "Imp",      2
    : [Eqv, x, y]         => opstr, upb := "Eqv",      2
    : [Not, x]            => opstr, upb := "Not",      1
    :                     => opstr, upb := "Unknown",  0
    .
  }
 
  IF depth=maxdepth DO { writef("Etc"); RETURN }
 
  writef("Op:%s", opstr)

  FOR i = 1 TO upb DO { newline()
                        FOR j=0 TO depth-1 DO writes( prlinev!j )
                        writes("*-")
                        prlinev!depth := i=upb-> "  ", "! "
                        prtree(x!i, depth+1, maxdepth)
                       }

.
//********** End of Print Tree code ***************************




