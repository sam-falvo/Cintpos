GET "mcpl.h"

/* This is a simple OBDD demonstration program
   implemented in MCPL by M. Richards

   This program is based on the algorithm described in:
   Brace,K.S., Rudell, R.L. and Bryant, R.E,
   "Efficient Implementation of a BDD Package",
   27th ACM/IEEE Design Automation Conference, pp 40-45, 1990
*/

MANIFEST 
Id, Not, And, Or, Imp, Eqv,  // Lexical tokens
True, False,
Lparen, Rparen,Eof,
E_syntax=100, E_space        // Exceptions

STATIC nodelist = 0

//******************** OBDD Builder ***********************

// The current version
//     uses negated edges,
//     but no hash table for the nodes in the OBDD,
//     and no caching of previously computed calls of ite,
//     and no garbage collection.

// mkobdd(x) forms the obdd corresponding to 
//           boolean expression x (in abstract syntax tree form).
FUN mkobdd
: [True]      => 1
: [False]     => 0
: [Id,  v]    => lookup(v, 1, 0)
: [Not, x]    => 1 XOR mkobdd x
: [And, x, y] => ite(mkobdd x, mkobdd y,              0)
: [Or,  x, y] => ite(mkobdd x,        1,       mkobdd y)
: [Imp, x, y] => ite(mkobdd x, mkobdd y,              1)
: [Eqv, x, y] => ite(mkobdd x, mkobdd y, 1 XOR mkobdd y)

/*
FUN ite : f, g, h =>                   // Debugging trace
  writef("\nite: %7d %7d %7d\n", f, g, h)
  writef "\nf is:\n"; probdd(f, 0, 5)
  writef "\ng is:\n"; probdd(g, 0, 5)
  writef "\nh is:\n"; probdd(h, 0, 5)
  newline()
  ite1(f,g,h)
*/

// ite(f,g,h) forms the OBBD for:          IF f THEN g ELSE h
//            where f, g and h are OBDDs.
FUN ite
: 1,        g,        ? => g
: 0,        ?,        h => h
: f,        1,        0 => f
: f,        0,        1 => f XOR 1
: ?,        g,       =g => g
: f,       =f,        h => ite(f, 1, h)
: f,        g      , =f => ite(f, g, 0)
: f,        g, =1 XOR f => ite(f, g, 1)
: f, =1 XOR f,        h => ite(f, 0, h)
: f, g,  h =>
  LET r = findcomputed(f, g, h)
  IF r>=0 RETURN r
  LET v = topvar(f, g, h)
  LET t = ite(set1(f, v), set1(g, v), set1(h, v))
  LET e = ite(set0(f, v), set0(g, v), set0(h, v))
  IF t=e RETURN t
  r := lookup(v, t, e)
  insertcomputed(r, f, g, h)
  RETURN r

// set1(f, v) return the obbd for f with variable v set to 1

FUN set1 : 0, ? => 0
         : 1, ? => 1
         : f, v => LET neg = f&1
                   IF (f&-2)!0=v RETURN neg XOR ((f&-2)!2)
                   RETURN f

// set0(f, v) return the obbd for f with variable v set to 0

FUN set0 : 0, ? => 0
         : 1, ? => 1
         : f, v => LET neg = f&1
                   IF (f&-2)!0=v RETURN neg XOR ((f&-2)!3)
                   RETURN f

FUN findcomputed : f, g, h => -1           // Dummy for now

FUN insertcomputed : r, f, g, h => RETURN  // Dummy for now

FUN lookup : v, t, e => LET p = nodelist   // No hash table yet
                        WHILE p DO
                        { IF p!0=v AND p!2=t AND p!3=e RETURN p
                          p := p!1
                        }
                        nodelist := mk4(v, nodelist, t, e)
                        
                        RETURN nodelist

FUN topvar : f, g, h =>
  LET a = f>1 -> !(f&-2), 'Z'
  LET b = g>1 -> !(g&-2), 'Z'
  LET c = h>1 -> !(h&-2), 'Z'
  IF b<a DO a := b
  IF c<a DO a := c
  RETURN a

//********************* Lexical Analyser ******************

STATIC
  strp, ch, nch, token, lexval

FUN lex_init : str => strp := str; rch(); rch()

FUN rch : => ch, nch := nch, %strp
             UNLESS nch=0 DO strp++

FUN lex : => MATCH (ch, nch)

: ' ' | '\n' => rch(); lex()

: 't'        => token := True;   rch()
: 'f'        => token := False;  rch()
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
: True   => LET a = mk1 True
            lex()
            a
: False  => LET a = mk1 False
            lex()
            a
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

FUN mk4 : x, y, z, t => mk1 t; mk1 z; mk1 y; mk1 x

//********************* Main Program **********************


FUN try : e => 
  { writef("\nTesting: %s\n", e)

    mk_init 100_000

    LET exp = parse e
    //writef "\nParse tree:\n"
    //prtree(exp, 0, 20)
    //newline()
    nodelist := 0
    LET t = mkobdd exp
    writef "\nCorresponding OBDD:\n"
    probdd(t, 0, 20)
    newline()

    writef "\nnodelist\n"
    LET p = nodelist
    WHILE p DO
    { writef("%7d: %c %7d %7d\n", p, p!0, p!2, p!3)
      p := p!1
    }
    newline()
    //abort 1000
  } HANDLE : E_syntax => writef "Bad Syntax\n"
           : E_space  => writef "Insufficient space\n"
           .
  mk_close()

// Propositional examples supplied by Larry Paulson 
// and modified by MR

FUN start : =>
    try "P->Q"
    try "P->Q->R"
    try "P->(Q->R)"
    try "P=Q"
    try "(P=Q) -> (Q=P)"

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
*            Print Functions and Data
*
*    prlinev
*    prtree(tree, depth, maxdepth)
*    probdd(obdd, depth, maxdepth)
*    
**************************************************************/

STATIC
  prlinev = VEC 50

FUN prtree : x, depth, maxdepth => 

  LET opstr=0, upb=0

  IF x=0 DO { writef "Nil"; RETURN }
 
  { MATCH x
    : [Id, ch]            => writef("%c", ch);     RETURN
    : [True]              => writef "True";        RETURN
    : [False]             => writef "False";       RETURN
    : [And, x, y]         => opstr, upb := "And",      2
    : [Or,  x, y]         => opstr, upb := "Or",       2
    : [Imp, x, y]         => opstr, upb := "Imp",      2
    : [Eqv, x, y]         => opstr, upb := "Eqv",      2
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


FUN probdd : x, depth, maxdepth => 

  LET opstr = 0

  IF x=0 OR x=1 DO { writef("%d", x); RETURN }
 
  IF depth=maxdepth DO { writef("Etc"); RETURN }
   
  TEST x&1 THEN { x--
                  writef("%c  *%d", !x, x)
                }
           ELSE   writef("%c   %d", !x, x)

  FOR i = 2 TO 3 DO { newline()
                      FOR j=0 TO depth-1 DO writes( prlinev!j )
                      writes("*-")
                      prlinev!depth := i=3-> "  ", "! "
                      probdd(x!i, depth+1, maxdepth)
                    }


//********** End of Print Tree and OBDD code ******************

