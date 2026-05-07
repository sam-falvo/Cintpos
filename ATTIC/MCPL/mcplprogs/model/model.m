GET "mcpl.h"

/* This is a simple demonstration of symbolic model checking
   implemented in MCPL by M. Richards

   It is based on the algorithm given on page 30 of
   "Symbolic Model Checking" by K.L.McMillan, Kluwer Academic Press, 1993.
*/

MANIFEST 
Id, Atom, Not, And, Or, Imp, Eq,          // Tokens
EX, EF, EG, EU, E, AX, AF, AG, AU, A, U,
Lparen, Rparen,Eof,

E_syntax=100, E_space, E_eval,            // Exceptions

                         // Atomic boolean functions
True= #xFFFFFFFF,        // f(a,b,c,d,e) = T
False=#x00000000,        // f(a,b,c,d,e) = F

Abits=#xAAAAAAAA,        // f(a,b,c,d,e) = a
Bbits=#xCCCCCCCC,        // f(a,b,c,d,e) = b
Cbits=#xF0F0F0F0,        // f(a,b,c,d,e) = c
Dbits=#xFF00FF00,        // f(a,b,c,d,e) = d
Ebits=#xFFFF0000         // f(a,b,c,d,e) = e

//********** Model checking algorithm *************************

// The transition relation will be represented by the vector preds
// preds!i will be the bit pattern representing the set of immediate
//         predecessors of state i

STATIC preds = VEC #b11111  // Initialised later.

FUN eval
: [Atom, bits] =>   bits
: [Not, x]     => ~ eval x
: [And, x,  y] =>   eval x  & eval y
: [Or,  x,  y] =>   eval x  | eval y
: [Imp, x,  y] => ~ eval x  | eval y
: [Eq,  x,  y] => ~(eval x XOR eval y)
: [EX,  x]     =>   evalEX(  eval x)
: [AX,  x]     => ~ evalEX(~ eval x)
: [EF,  x]     =>   evalEU(    True,   eval x)
: [AG,  x]     => ~ evalEU(    True, ~ eval x)
: [AF,  x]     =>   evalAU(    True,   eval x)
: [EU,  x,  y] =>   evalEU(  eval x,   eval y)
: [EG,  x]     =>   evalEG(  eval x)
: [AU,  x,  y] =>   evalAU(  eval x,   eval y)
:              =>   RAISE E_eval
.

FUN evalEX : w =>     // Computes: EX w
  LET res=0, p=preds
  WHILE w DO { IF w&1 DO res |:= !p
               w >>:= 1
               p+++
             }
  RETURN res
.

FUN evalEU : p, q =>  // Computes: E(p U q)
  LET y = q
  { LET a = q | p & evalEX y
    IF a=y RETURN y
    y := a
  } REPEAT
.

FUN evalAU : p, q =>  // Computes: A(p U q)
  LET succs = evalEX True
  LET y = q
  { LET a = q | p & succs & ~evalEX(~y)
    IF a=y RETURN y
    y := a
  } REPEAT
.

FUN evalEG : p =>     // Computes: EG p
  LET nosuccs = ~ evalEX True
  LET y = p
  { LET a = p & (nosuccs | evalEX y)
    IF a=y RETURN y
    y := a
  } REPEAT
.

//********** End of Model checking algorithm ******************

/********************* Syntax Analyser ************************

    CTL Terms     Corresponding tree

 T -->  Id        [Atom, f]   where f represents a bool function
        ( T )
        ~ T       [Not,  T]
        T = T     [Eq,   T, T]
        T & T     [And,  T, T]
        T | T     [Or,   T, T]
        T -> T    [Imp,  T, T]
        AX T      [AX,   T]
        AF T      [AF,   T]
        AG T      [AG,   T]
        EX T      [EX,   T]
        EF T      [EF,   T]
        EG T      [EG,   T]
        A(T U T)  [AU,   T, T]
        E(T U T)  [EU,   T, T]
*/

//********************* Lexical Analyser ******************

STATIC  strp, ch, nch, token, lexval

FUN rch : => ch, nch := nch, %strp
             IF nch DO strp++
.

FUN lex_init : str => strp := str; rch(); rch()
.

FUN lex : => MATCH (ch, nch)
: ' ' | '\n' => rch(); lex()     // Ignore white space
:  0         => token := Eof     // End of file
: 'a'        => token := Id;     lexval := Abits; rch() 
: 'b'        => token := Id;     lexval := Bbits; rch() 
: 'c'        => token := Id;     lexval := Cbits; rch() 
: 'd'        => token := Id;     lexval := Dbits; rch() 
: 'e'        => token := Id;     lexval := Ebits; rch() 
: 'T'        => token := Id;     lexval := True;  rch() 
: 'F'        => token := Id;     lexval := False; rch() 
: '('        => token := Lparen;                  rch()
: ')'        => token := Rparen;                  rch()
: '~'        => token := Not;                     rch()
: '='        => token := Eq;                      rch()
: '&'        => token := And;                     rch()
: '|'        => token := Or;                      rch()
: '-', '>'   => token := Imp;              rch(); rch()
: 'A', 'X'   => token := AX;               rch(); rch()
: 'A', 'F'   => token := AF;               rch(); rch()
: 'A', 'G'   => token := AG;               rch(); rch()
: 'E', 'X'   => token := EX;               rch(); rch()
: 'E', 'F'   => token := EF;               rch(); rch()
: 'E', 'G'   => token := EG;               rch(); rch()
: 'A'        => token := A;                       rch()
: 'E'        => token := E;                       rch()
: 'U'        => token := U;                       rch()
:            => RAISE E_syntax
.
.

FUN parse : str => lex_init str;
                   LET tree = nexp 0
                   UNLESS token=Eof RAISE E_syntax
                   RETURN tree
.

FUN prim : =>
  LET op = token

  MATCH op
  : Id       => LET a = mk2(Atom, lexval)
                lex()
                RETURN a

  : Lparen   => LET a = nexp 0
                UNLESS token=Rparen RAISE E_syntax
                lex()
                RETURN a

  : Not | AX | AF | AG | EX | EF | EG
             => mk2(op,  nexp 5)

  : A | E    => lex()
                UNLESS token=Lparen RAISE E_syntax
                LET a = nexp 0
                UNLESS token=U RAISE E_syntax
                LET b = nexp 0
                UNLESS token=Rparen RAISE E_syntax
                lex()
                mk3(op=A->AU,EU, a, b)

  :          => RAISE E_syntax
  .
.

FUN nexp : n => lex(); exp n
         .

FUN  exp : n => LET a = prim()

                MATCH (token, n)
                : Eq,  <4 => a := mk3(Eq,  a, nexp 4)
                : And, <3 => a := mk3(And, a, nexp 3)
                : Or,  <2 => a := mk3(Or,  a, nexp 2)
                : Imp, <1 => a := mk3(Imp, a, nexp 1)
                :         => RETURN a
                . REPEAT
.

//********************* Space Allocation ******************


STATIC  spacev, spacep

FUN mk_init : upb    => spacev := getvec upb
                        UNLESS spacev RAISE E_space
                        spacep := @ spacev!upb
            .

FUN mk_close :       => freevec spacev
             .

FUN mk1 : x          => !---spacep := x; spacep .
FUN mk2 : x, y       => mk1 y; mk1 x .
FUN mk3 : x, y, z    => mk1 z; mk1 y; mk1 x .
FUN mk4 : x, y, z, t => mk1 t; mk1 z; mk1 y; mk1 x .

//************** Print tree function **********************

STATIC prlinev = VEC 50

FUN prtree
: 0,     ?,        ? => writef "Nil"
: ?, depth,   =depth => writef "Etc"
: x, depth, maxdepth => 
  LET upb = 1
  MATCH x
  : [Atom, =Abits]  => writef "a";           RETURN
  : [Atom, =Bbits]  => writef "b";           RETURN
  : [Atom, =Cbits]  => writef "c";           RETURN
  : [Atom, =Dbits]  => writef "d";           RETURN
  : [Atom, =Ebits]  => writef "e";           RETURN
  : [Atom, =True]   => writef "T";           RETURN
  : [Atom, =False]  => writef "F";           RETURN
  : [Atom, bits]    => writef("%32b", bits); RETURN
  : [Not, x]        => writes "Not"
  : [Eq,  x,  y]    => writes "Eq";        upb := 2
  : [And, x,  y]    => writes "And";       upb := 2
  : [Or,  x,  y]    => writes "Or";        upb := 2
  : [Imp, x,  y]    => writes "Imp";       upb := 2
  : [EX,  x]        => writes "EX"
  : [EU,  x,  y]    => writes "EU";        upb := 2
  : [EG,  x]        => writes "EG"
  : [EF,  x]        => writes "EF"
  : [AX,  x]        => writes "AX"
  : [AG,  x]        => writes "AG"
  : [AU,  x,  y]    => writes "AU";        upb := 2
  :                 => writes "Unknown";   upb := 0
  .
  FOR i = 1 TO upb DO { newline()
                        FOR j=0 TO depth-1 DO writes( prlinev!j )
                        writes("*-")
                        prlinev!depth := i=upb-> "  ", "! "
                        prtree(x!i, depth+1, maxdepth)
                       }
.

//********************* Main Program **************************

FUN try : e => 
  { mk_init 100_000
    writef("\n%s\n", e)
    LET exp = parse e
//  prtree(exp, 0, 20)
    LET res = eval exp
    FOR v = #b00000 TO #b11111 DO
    { UNLESS v MOD 8 DO newline()
      writef("%5b %c ", v, res&1=0->' ', 'Y')
      res >>:= 1
    }
    newline()
  } HANDLE : E_syntax => writef "Bad Syntax\n"
           : E_space  => writef "Insufficient space\n"
           : E_eval   => writef "Error in eval\n"
           .
  mk_close()
.

FUN start : =>
//  selectoutput(findoutput "res")

  init_nfsm_5Dcube()

  try "d&e->a&b&c"
  try "EX a & EX b & EX c & EX d & EX e"
  try "EX EX (a&b&c&d&e)"
  try "EG ~EX EX (a&b&c&d&e)"
  try "EX ~(a|b|c|d|e)"

  init_nfsm_glasses()
 
  try "~a&~b&~c -> AF ~(d|e)"
  try "AF ~(d|e)"
  try "AG ~(a&b&c)"
  try "AX F"

  init_nfsm_async()

  try "d&~c -> AX AX A( ~d U c)"
  try "d&~c -> A(d|~c U c)"
  try "EG ~(a&b&c&d)"
  try "EX EX EX EX EX EX (a&b&c&d)"

//  endwrite()
  RETURN 0
.

FUN edge : v1, v2 => preds!v2 XOR:= 1<<v1 . // Add/remove an edge

FUN init_nfsm_5Dcube : =>
  writef "\n5D Cube\n"
  FOR v = #b00000 TO #b11111 DO preds!v := 0

  FOR v = #b00000 TO #b11111 DO // Form a 5D cube with all edges 
  { edge(v, v XOR #b00001)
    edge(v, v XOR #b00010)
    edge(v, v XOR #b00100)
    edge(v, v XOR #b01000)
    edge(v, v XOR #b10000)
  }
  edge(#b11111, #b00000)        // But, add one more edge
  edge(#b11000, #b11100)        // and  remove one edge
.

FUN init_nfsm_glasses : =>
  writef "\nThe Glasses Game\n"
  FOR v = #b00000 TO #b11111 DO preds!v := 0

  // A state is represented by two oct digits #gm
  // where g=0 means all glasses are the same way up
  //       g=1 means one glass is the wrong way up
  //       g=2 means two adjacent glasses are the wrong way up
  //       g=3 means two opposite glasses are the wrong way up
  // and   m=0..7 is the move number.

  move2x 0; move2a 1; move2x 2; move1 3; move2x 4; move2a 5; move2x 6
.

FUN move1  : i => LET j = i+1
                  edge(#10+i,#00+j) // Turn one glass over
                  edge(#10+i,#20+j)
                  edge(#10+i,#30+j)  
                  edge(#20+i,#00+j)
                  edge(#30+i,#00+j)
.

FUN move2x : i => LET j = i+1
                  edge(#10+i,#10+j) // Turn two opposite glasses over
                  edge(#20+i,#20+j)
                  edge(#30+i,#00+j) 
.

FUN move2a : i => LET j = i+1
                  edge(#10+i,#10+j) // Turn two adjacent glasses over
                  edge(#20+i,#30+j)
                  edge(#30+i,#20+j) 
.

FUN init_nfsm_async : =>
  writef "\nAn Asynchronous Circuit\n"
  FOR v = #b00000 TO #b11111 DO preds!v := 0
  edge( 2, 0); edge( 2, 1); edge( 2, 3); edge( 0, 1); edge( 3, 1)
  edge( 7, 6); edge( 7, 4); edge( 7, 5); edge( 6, 4); edge( 5, 4)
  edge(13,15); edge(13,14); edge(13,12); edge(15,14); edge(12,14)
  edge( 8, 9); edge( 8,11); edge( 8,10); edge( 9,11); edge(10,11)
  edge( 1, 5); edge( 3, 5); edge( 3, 7)
  edge( 4,12); edge( 5,12); edge( 5,13)
  edge(14,10); edge(12,10); edge(12, 8)
  edge(10, 2); edge(10, 3); edge(11, 3)
.

