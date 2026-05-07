GET "mcpl.h"

MANIFEST

Eof=0, Id, Num, Times, Div, Pos, Neg, Plus, Minus,
Eq, Cond, Lam, Ap, Y,

Syntax=1, Lookup, Eval

STATIC spacep

FUN lookup
: ?,             0 => RAISE Lookup
: n, [=n, val,  ?] => val
: n, [ ?,   ?,  e] => lookup(n, e)
.

FUN eval
: [Id, x],      e => lookup(x, e)
: [Num, k],     ? => k
: [Pos, x],     e => eval(x, e)
: [Neg, x],     e => - eval(x, e)
: [Times,x,y],  e => eval(x, e) * eval(y, e)
: [Div,x,y],    e => eval(x, e) / eval(y, e)
: [Plus,x,y],   e => eval(x, e) + eval(y, e)
: [Minus,x,y],  e => eval(x, e) - eval(y, e)
: [Eq,x,y],     e => eval(x, e) = eval(y, e)
: [Cond,b,x,y], e => eval(b, e) -> eval(x, e), eval(y, e)
: [Lam,x,body], e => mk3(x, body, e)
: [Ap,x,y],     e => MATCH eval(x, e)
                     : [bv, body, env] =>
                          eval(body, mk3(bv, eval(y, e), env))
                     .
: [Y, exp],     e =>
  { LET bigf = eval(exp, e)
    MATCH bigf   // bigf should be a closure whose body is an
                 // abstraction eg Lf Ln n=0 -> 1, n*f(n-1)
    : [f, [Lam, n, body], env] =>
        LET res  = mk3(n, body, 0) // environment to be filled in later
        LET env1 = mk3(f, res, env) // make environment including f
        res!2 := env1   // fill in environment component of closure
        RETURN res      // return this closure
    .
  }
: ?,            ? => RAISE Eval
.

// Construct       Corresponding Tree

// a ,.., z   -->  [Id, 'a'] ,..,  [Id, 'z']
// dddd       -->  [Num, dddd]
// x y        -->  [Ap, x, y]
// Y x        -->  [Y, x]
// x * y      -->  [Times, x, y]
// x / y      -->  [Div, x, y]
// x + y      -->  [Plus, x, y]
// x - y      -->  [Minus, x, y]
// x = y      -->  [Eq, x, y]
// b -> x, y  -->  [Cond, b, x, y]
// Li y       -->  [Lam, i, y]

STATIC strp, ch, nch, token, lexval

FUN rch
: => ch := nch
     nch := %strp
     IF nch DO strp++
.

FUN parse
: s => strp := s
       rch(); rch()
       nexp 0
.

FUN lex
: => MATCH (ch, nch)

     : '-',    '>'  => token := '->'; rch(); rch()

     : ' ' | '\n', ?  => rch(); lex()

     : '(' | ')' | '*' | '/' | '+' | '-' | 'L' | 'Y' | '=' | ',',
                 ?  => token := ch; rch()

     : 'a'..'z', ?  => token, lexval := Id, ch; rch()

     : '0'..'9', ?  => token, lexval := Num, ch-'0'
                       { rch()
                         MATCH ch
                         :'0'..'9' => lexval := 10*lexval+ch-'0'
                         : ?       => RETURN
                         .
                       } REPEAT

     :   0,     ?    => token := Eof

     :   ?,     ?    => RAISE Syntax
     .
.

FUN prim
: => MATCH token
     : Id   => LET a = mk2(Id, lexval)
               lex()
               a
     : Num  => LET a = mk2(Num, lexval)
               lex()
               a
     : 'Y'  => mk2(Y, nexp 6)
     : 'L'  => lex()
               UNLESS token=Id RAISE Syntax
               LET bv = lexval
               mk3(Lam, bv, nexp 0)
     : '('  => LET a = nexp 0
               UNLESS token=')' RAISE Syntax
               lex()
               a
     : '+'  => mk2(Pos, nexp 3)
     : '-'  => mk2(Neg, nexp 3)
     :  ?   => RAISE Syntax
     .
.

FUN nexp : n => lex(); exp n
.

FUN exp 
: n => LET a = prim()
       { MATCH (token, n)
         :   '(' | Num | Id,
                  <6 => a := mk3(   Ap, a,  exp 6)
         :   '*', <5 => a := mk3(Times, a, nexp 5) 
         :   '/', <5 => a := mk3(  Div, a, nexp 5) 
         :   '+', <4 => a := mk3( Plus, a, nexp 4) 
         :   '-', <4 => a := mk3(Minus, a, nexp 4) 
         :   '=', <3 => a := mk3(   Eq, a, nexp 3) 
         :  '->', <1 => LET b = nexp 0
                        UNLESS token=',' RAISE Syntax
                        a := mk4(Cond, a, b, nexp 0) 
         :    ?,   ? => RETURN a
         .
       } REPEAT
.

FUN mk1 : a => !---spacep := a; spacep
.

FUN mk2 : a, b => mk1(b); mk1(a)
.

FUN mk3 : a, b, c => mk1(c); mk1(b); mk1(a)
.

FUN mk4 : a, b, c, d => mk1(d); mk1(c); mk1(b); mk1(a)
.

FUN wrs : s => writef("%s\n", s)
.

FUN wrn : n => writef("%d\n", n)
.

FUN try : e => newline()
               wrs e
               spacep := @ (VEC 1000)!1000

               wrn ( eval(parse e, 0) )

               HANDLE : Syntax => wrs "Bad syntax"
                      : Lookup => wrs "Bad lookup"
                      : Eval   => wrs "Bad eval"
                      .
.

FUN start : =>
    try "(Lx x)  (Ly y) 99"
    try "1234"
    try "x"
    try "100+23"
    try "1=2 -> 3, 4"
    try "(Lx x+1) 2"
    try "(Ls Lk s k k) (Lf Lg Lx  f x (g x)) (Lx Ly x) (Lx x) 1234"
    try "(Y (Lf Ln n=0->1,n*f(n-1))) 10"
    RETURN 0
.



