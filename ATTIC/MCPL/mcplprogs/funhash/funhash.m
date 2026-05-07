GET "mcpl.h"

/* This is an experimental implementation of the function
   to hash boolean functions described in
   "Probabilistic Verification of Boolean Functions" by
   Jawahar Jain et. al. in Formal Methods in System Design 1: 63-117 (1992).
*/

MANIFEST 
Id, Not, And, Or, Imp, Eqv,  // Lexical tokens
True, False, Mul,
Lparen, Rparen,Eof,
E_syntax=100, E_space,       // Exceptions

Vmax=26,

Modulus=541                  // A big prime!

FUN add : x, y => (x+y) MOD Modulus
FUN sub : x, y => (Modulus+x-y) MOD Modulus
FUN mul : x, y => (x*y) MOD Modulus

FUN add1 : x, y => writef("add(%d,%d)=>%d\n", x, y, add1(x,y)); add1(x,y)
FUN sub1 : x, y => writef("sub(%d,%d)=>%d\n", x, y, sub1(x,y)); sub1(x,y)
FUN mul1 : x, y => writef("mul(%d,%d)=>%d\n", x, y, mul1(x,y)); mul1(x,y)

FUN hash1 : x, e => 
writef("hash (env: %d %d %d %d) of\n", e!1, e!2, e!3, e!4)
prtree(x, 0, 5); newline()
hash1(x, e)

FUN hash
: [Id, v], e => lookup(v, e)
: [True],  ? => 1
: [False], ? => 0
: [Not, x], e => sub(1, hash(x,e))
: [And, x, y], e => // xy
                hash([Mul, x, y], e)
: [Or,  x, y], e => // x + y - xy
                sub(add(hash(x,e), hash(y,e)), hash([Mul, x, y], e))
: [Imp, x, y], e => // 1 - x + xy
                add(sub(1, hash(x,e)), hash([Mul, x, y], e))
: [Eqv, x, y], e => // 1 - x - y + 2xy
                add(sub(sub(1,hash(x,e)), hash(y,e)),
                    mul(2, hash([Mul, x, y], e)))
: [Mul, x, y], e => 
    IF orthogonal(x,y,e) RETURN 0
    IF either(x,y,e)     RETURN sub(add(x, y), 1)
    IF disjoint(x,y,e)   RETURN mul(hash(x,e), hash(y,e))
    LET v = findunset e
    UNLESS v RETURN mul(hash(x,e), hash(y,e))
    LET rv = lookup(v, e)
     //writef("selecting var %c rv=%d\n", 'A'+v-1, rv)
     // (1-rv)*hash(x,e[v/0])*hash(y,e[v/0]) + rv*hash(x,e[v/1])*hash(y,e[v/1])
    setenv(e,v,0)
    LET t1 = hash(x,e)
     IF t1 DO t1 := mul(t1, hash(y,e))
     t1 := mul(sub(1,rv), t1)
    setenv(e,v,1)
    LET t2 = hash(x,e)
     IF t2 DO t2 := mul(t2, hash(y,e))
     t2 := mul(rv, t2)
    setenv(e,v,rv)
     //writef("returning %d+%d = %d\n", t1, t2, add(t1,t2))
    RETURN add(t1, t2)

FUN orthogonal : x, y, e => FALSE

FUN either : x, y, e => FALSE

FUN disjoint : x, y, e => FALSE

FUN findunset : e => FOR v = 1 TO Vmax IF e!v>1 RETURN v
                     RETURN 0

FUN setenv : e, v, val => e!v := val

FUN lookup : v, e => e!v

//********************* Lexical Analyser ******************

STATIC
  strp, ch, nch, token, lexval

FUN lex_init : str => strp := str; rch(); rch()

FUN rch : => ch, nch := nch, %strp
             UNLESS nch=0 DO strp++

FUN lex : => MATCH (ch, nch)

: ' ' | '\n' => rch(); lex()

: 'A'..'Z'   => token, lexval := Id, 1+ch-'A'; rch() 
: '0'        => token := False;  rch()
: '1'        => token := True;   rch()
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


FUN prim : => MATCH token
: Id     => LET a = mk2(Id, lexval)
            lex()
            a
: True   => lex()
            mk1 True
: False  => lex()
            mk1 False
: Lparen => LET a = nexp 0
            UNLESS token=Rparen RAISE E_syntax
            lex()
            a
: Not    => mk2(Not, nexp 3)
:  ?     => RAISE E_syntax

FUN nexp : n => lex(); exp n

FUN exp : n =>
  LET a = prim()

  { MATCH (token, n)
    : And, <3 => a := mk3(And, a, nexp 3) 
    : Or,  <2 => a := mk3(Or , a, nexp 2) 
    : Imp, <1 => a := mk3(Imp, a, nexp 1) 
    : Eqv, <1 => a := mk3(Eqv, a, nexp 1) 
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

    LET env = TABLE[ 0, 2, 3, 4, 5, 6, 7, 8, 9,
                    10,11,12,13,14,15,16,17,18,19,
                    20,21,22,23,24,25,26,27,28,29 ]

    writef("Hash value: %7d\n",  hash(parse e, env))
  } HANDLE : E_syntax => writef "Bad Syntax\n"
           : E_space  => writef "Insufficient space\n"
           .
  mk_close()

// Propositional examples supplied by Larry Paulson 
// and modified by MR

FUN start : =>
  //    try "0"
  //    try "1"
  //    try "A"
  //    try "B"
  //    try "A&B"
    try "Z&Z"
RETURN
  /*
    try "A&A"
    try "A|A"
    try "~A"
    try "A->B"
    try "~B->~A"
    try "~A|B"
  */
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
    : [Id, v]            => writef("%c", 'A'-1+v);     RETURN
    : [True]              => writef("True");           RETURN
    : [False]             => writef("False");          RETURN
    : [And, x, y]         => opstr, upb := "And",      2
    : [Or,  x, y]         => opstr, upb := "Or",       2
    : [Imp, x, y]         => opstr, upb := "Imp",      2
    : [Eqv, x, y]         => opstr, upb := "Eqv",      2
    : [Mul, x, y]         => opstr, upb := "Mul",      2
    : [Not, x]            => opstr, upb := "Not",      1
    :                     => opstr, upb := "Unknown",  0
  }
 
  IF depth=maxdepth DO { writef("Etc"); RETURN }
 
  writef("Op:%s", opstr)

  FOR i = 1 TO upb DO { newline()
                        FOR j=0 TO depth-1 DO writes( prlinev!j )
                        writes("*-")
                        prlinev!depth := i=upb-> "  ", "! "
                        prtree(x!i, depth+1, maxdepth)
                       }


//********** End of Print Tree code ***************************


