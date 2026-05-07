GET "mcpl.h"

/* This is a simple demonstration of Mu-Calculus symbolic model checking
   implemented in MCPL by M. Richards

   It is based on the algorithm given in
   "Model Checking Algorithms for the Mu-Calculus"
   by S.Berezin, E.Clarke, S.Jha and W.Marrero,
   CMU-CS-96-180, Sept.1996.
*/

MANIFEST 
Id, Atom, Var, Lab, Not, And, Or, Imp, Eq,          // Tokens
Mu, Nu, EX, AX, 
Dot, Rbra, Rket, Sbra, Sket, Abra, Aket, Eof,

E_syntax=100, E_space, E_eval, E_lookup,        // Exceptions

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
// preds!lab!i will be the bit pattern representing the set of
//             immediate lab-predecessors of state i

STATIC preds = [ VEC #b11111,  // p-predecessors   ) all
                 VEC #b11111,  // q-predecessors   ) initialised
                 VEC #b11111   // r-predecessors   ) later
               ]

FUN eval
: [Atom, bits],    e =>   bits
: [Var,     v],    e =>   lookup(v,e)          // v = 0, 1 or 2
: [Not,     x],    e => ~ eval(x,e)
: [And,     x, y], e =>   eval(x,e)  & eval(y,e)
: [Or,      x, y], e =>   eval(x,e)  | eval(y,e)
: [Imp,     x, y], e => ~ eval(x,e)  | eval(y,e)
: [Eq,      x, y], e => ~(eval(x,e) XOR eval(y,e))
: [EX,    lab, x], e =>   evalEX(lab,  eval(x,e))
: [AX,    lab, x], e => ~ evalEX(lab, ~eval(x,e))
: [Mu,      v, x], e =>   evalMu(v, x, e)      // v = 0, 1 or 2
: [Nu,      v, x], e =>   evalNu(v, x, e)      // v = 0, 1 or 2
:                    =>   RAISE E_eval
.

FUN lookup : i, e => e!i .  // e = [xval, yval, zval]

FUN evalEX : i, w => // returns the i-predecessors of all states in w
  LET res=0, p=preds!i
  WHILE w DO { IF w&1 DO res |:= !p
               w >>:= 1
               p+++
             }
  RETURN res
.

FUN evalMu : v, x, e =>      // Computes: Mu v.x
  LET y = False
  LET newe = [e!0, e!1, e!2] // make a copy of e
  { newe!v := y
    LET a = eval(x, newe)    // newe = e[v <- y]
    IF a=y RETURN y
    y := a
  } REPEAT
.

FUN evalNu : v, x, e =>      // Computes: Nu v.x
  LET y = True
  LET newe = [e!0, e!1, e!2] // make a copy of e
  { newe!v := y
    LET a = eval(x, newe)    // newe = e[v <- y]
    IF a=y RETURN y
    y := a
  } REPEAT
.

//********** End of Model checking algorithm ******************

/********************* Syntax Analyser ************************

    CTL Terms     Corresponding tree

 T -->  Id        [Atom, f]   where f represents a bool function
        x         [Var, 0]
        y         [Var, 1]
        z         [Var, 2]
        ( T )
        ~ T       [Not,  T]
        T = T     [Eq,   T, T]
        T & T     [And,  T, T]
        T | T     [Or,   T, T]
        T -> T    [Imp,  T, T]
        <p>T      [EX, 0, T]
        <q>T      [EX, 1, T]
        <r>T      [EX, 2, T]
        [p]T      [AX, 0, T]
        [q]T      [AX, 1, T]
        [r]T      [AX, 2, T]
        Mx.T      [Mu, 0, T]
        My.T      [Mu, 1, T]
        Mx.T      [Mu, 2, T]
        Nx.T      [Nu, 0, T]
        Ny.T      [Nu, 1, T]
        Nx.T      [Nu, 2, T]
*/

//********************* Lexical Analyser ******************

STATIC  strp, ch, nch, token, lexval

FUN rch : => ch, nch := nch, %strp
             IF nch DO strp++
.

FUN lex_init : str => strp := str; rch(); rch() .

FUN lex : => MATCH (ch, nch)
: ' ' | '\n' => rch(); lex()     // Ignore white space
:  0         => token := Eof     // End of file
: 'a'        => token := Id;     lexval := Abits;  rch() 
: 'b'        => token := Id;     lexval := Bbits;  rch() 
: 'c'        => token := Id;     lexval := Cbits;  rch() 
: 'd'        => token := Id;     lexval := Dbits;  rch() 
: 'e'        => token := Id;     lexval := Ebits;  rch() 
: 'T'        => token := Id;     lexval := True;   rch() 
: 'F'        => token := Id;     lexval := False;  rch() 
: 'x'..'z'   => token := Var;    lexval := ch-'x'; rch() 
: 'p'..'r'   => token := Lab;    lexval := ch-'p'; rch() 
: '('        => token := Rbra;                     rch()
: ')'        => token := Rket;                     rch()
: '<'        => token := Abra;                     rch()
: '>'        => token := Aket;                     rch()
: '['        => token := Sbra;                     rch()
: ']'        => token := Sket;                     rch()
: '~'        => token := Not;                      rch()
: '='        => token := Eq;                       rch()
: '&'        => token := And;                      rch()
: '|'        => token := Or;                       rch()
: '-', '>'   => token := Imp;               rch(); rch()
: 'M'        => token := Mu;                       rch()
: 'N'        => token := Nu;                       rch()
: '.'        => token := Dot;                      rch()
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

  : Var      => LET a = mk2(Var, lexval)
                lex()
                RETURN a

  : Rbra     => LET a = nexp 0
                UNLESS token=Rket RAISE E_syntax
                lex()
                RETURN a

  : Not      => mk2(op,  nexp 5)

  : Abra     => lex()                   // <p>T,  <q>T  or <r>T
                UNLESS token=Lab RAISE E_syntax
                LET a = lexval
                lex()
                UNLESS token=Aket RAISE E_syntax
                LET b = nexp 5
                mk3(EX, a, b)

  : Sbra     => lex()                   // [p]T,  [q]T  or [r]T
                UNLESS token=Lab RAISE E_syntax
                LET a = lexval
                lex()
                UNLESS token=Sket RAISE E_syntax
                LET b = nexp 5
                mk3(AX, a, b)

  : Mu | Nu  => lex()     // Mx.T, My.T, Mz.T, Nx.T, Ny.T or Nz.T
                UNLESS token=Var RAISE E_syntax
                LET a = lexval
                lex()
                UNLESS token=Dot RAISE E_syntax
                LET b = nexp 0
                mk3(op, a, b)

  :          => RAISE E_syntax
  .
.

FUN nexp : n => lex(); exp n .

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

FUN mk_close :       => freevec spacev .

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
  : [Var, i]        => writes("c", 'x'+i);   RETURN
  : [Not, f]        => writes "Not"
  : [Eq,  f, g]     => writes "Eq";        upb := 2
  : [And, f, g]     => writes "And";       upb := 2
  : [Or,  f, g]     => writes "Or";        upb := 2
  : [Imp, f, g]     => writes "Imp";       upb := 2
  : [EX,  i, f]     => writef("<%c>", 'p'+i); x+++
  : [AX,  i, f]     => writef("[%c]", 'p'+i); x+++
  : [Mu,  i, f]     => writef("M%c",  'x'+i); x+++
  : [Nu,  i, f]     => writef("N%c",  'x'+i); x+++
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
    prtree(exp, 0, 20)
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

  init_nfsm()
/*
  writes "\nTest the 5D cube -- using p-edges\n"

  writes "\nCTL: d&e->a&b&c"
  try "d&e->a&b&c"

  writes "\nCTL: EX a & EX b & EX c & EX d & EX e"
  try "<p>a & <p>b & <p>c & <p>d & <p>e"

  writes "\nCTL: EX EX (a&b&c&d&e)"
  try "<p><p> (a&b&c&d&e)"

  writes "\nCTL: EG ~EX EX (a&b&c&d&e)"
  try "Nx. (~<p><p>(a&b&c&d&e)) & <p>x"

  writes "\nCTL: EX ~(a|b|c|d|e)"
  try "<p> ~(a|b|c|d|e)"
*/
  writes "\nTest the asynchronous circuit -- using q-edges\n"

  writes "\nCTL: d&~c -> [q][q] A( ~d U c)"
  try "d&~c -> [q][q] Mx.(c | ~d & [q]x & ~[q]F)"

  writes "\nCTL: d&~c -> A(d|~c U c)"
  try "d&~c -> Mx.(c | (d|~c) & [q]x & ~[q]F)"

  writes "\nCTL: EG ~(a&b&c&d)"
  try "Nx.(~(a&b&c&d)) & (<q>x | [q]F)"

  writes "\nCTL: EX EX EX EX EX EX (a&b&c&d)"
  try "<q><q><q><q><q><q> (a&b&c&d)"

  writes "\nExists a path in which (a&b&c&d) occurs infinitely often"
  try "Ny.(<q>Mx.(<q>x | y&(a&b&c&d)))"

  writes "\nIn all paths (b&c&d) occurs infinitely often"
  try "Ny.(<q>T & [q]Mx.(<q>T & [q]x | y&(b&c&d)))"

  writes "\nThere is a path to (a&b&c&d) of length 6n"
  try "My.((a&b&c&d) | <q><q><q><q><q><q>y)"

  writes "\nTest the Glasses game -- using r-edges\n"
 
  writes "\nCTL: ~a&~b&~c -> AF ~(d|e)"
  try "~a&~b&~c -> ~ Nx.((d|e) & (<r>x | [r]F))"

  writes "\nCTL: AF ~(d|e)"
  try "~ Nx.((d|e) & (<r>x | [r]F))"

  writes "\nCTL: AG ~(a&b&c)"
  try "~ Mx.(a&b&c | <r>x)"

  writes "\nCTL: AX F"
  try "[r]F"

//  endwrite()
  RETURN 0
.

FUN edge : lab, v1, v2 => preds!lab!v2 XOR:= 1<<v1 . // Add/remove an edge

FUN init_nfsm : =>
  FOR v = #b00000 TO #b11111 DO { preds!0!v := 0 // p-preds
                                  preds!1!v := 0 // q-preds
                                  preds!2!v := 0 // r-preds
                                }

                                // Using p-edges,
  FOR v = #b00000 TO #b11111 DO // form a 5D cube with all edges 
  { edge(0, v, v XOR #b00001)
    edge(0, v, v XOR #b00010)
    edge(0, v, v XOR #b00100)
    edge(0, v, v XOR #b01000)
    edge(0, v, v XOR #b10000)
  }
  edge(0, #b11111, #b00000)     // But, add one more edge
  edge(0, #b11000, #b11100)     // and  remove one edge

  // An asynchronous circuit -- using q-edges
  edge(1, 2, 0); edge(1, 2, 1); edge(1, 2, 3); edge(1, 0, 1); edge(1, 3, 1)
  edge(1, 7, 6); edge(1, 7, 4); edge(1, 7, 5); edge(1, 6, 4); edge(1, 5, 4)
  edge(1,13,15); edge(1,13,14); edge(1,13,12); edge(1,15,14); edge(1,12,14)
  edge(1, 8, 9); edge(1, 8,11); edge(1, 8,10); edge(1, 9,11); edge(1,10,11)
  edge(1, 1, 5); edge(1, 3, 5); edge(1, 3, 7)
  edge(1, 4,12); edge(1, 5,12); edge(1, 5,13)
  edge(1,14,10); edge(1,12,10); edge(1,12, 8)
  edge(1,10, 2); edge(1,10, 3); edge(1,11, 3)

  edge(1, 31, 30)
  edge(1, 30, 29)
  edge(1, 29, 31)
  edge(1, 28, 30)
  edge(1, 27, 29)
  edge(1, 26, 28)
  edge(1, 25, 27)
  edge(1, 25, 16)
  edge(1, 24, 26)
  edge(1, 23, 25)
  edge(1, 22, 24)
  edge(1, 22, 23)
  edge(1, 21, 22)
  edge(1, 20, 21)
  edge(1, 19, 20)
  edge(1, 18, 19)
  edge(1, 17, 18)
  edge(1, 16, 17)


  // The Glasses Game        -- using r-edges
  // A state is represented by two oct digits #gm
  // where g=0 means all glasses are the same way up
  //       g=1 means one glass is the wrong way up
  //       g=2 means two adjacent glasses are the wrong way up
  //       g=3 means two opposite glasses are the wrong way up
  // and   m=0..7 is the move number.

  move2x 0; move2a 1; move2x 2; move1 3; move2x 4; move2a 5; move2x 6
.

FUN move1  : i => LET j = i+1
                  edge(2, #10+i,#00+j) // Turn one glass over
                  edge(2, #10+i,#20+j)
                  edge(2, #10+i,#30+j)  
                  edge(2, #20+i,#00+j)
                  edge(2, #30+i,#00+j)
.

FUN move2x : i => LET j = i+1
                  edge(2, #10+i,#10+j) // Turn two opposite glasses over
                  edge(2, #20+i,#20+j)
                  edge(2, #30+i,#00+j) 
.

FUN move2a : i => LET j = i+1
                  edge(2, #10+i,#10+j) // Turn two adjacent glasses over
                  edge(2, #20+i,#30+j)
                  edge(2, #30+i,#20+j) 
.


