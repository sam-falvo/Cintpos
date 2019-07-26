/* *********** THIS IS ONLY PARTLY WRITTEN **************

I  -> 'c..c'     // eg  'x'  'P'  't1'

OP -> '~'  |  '&'  |  '|'  |  '->'  |  '='

QU -> '@'  |  '!'      // For all  and Exists

T  ->  [Var,   I]
   |   [Param, I, Clist]
   |   [Bound, i]
   |   [Fun,   I, Tlist]

F  -> [Pred,  I, Tlist]
   |  [Conn,  OP, Flist]
   |  [Quant, QU, Id, F]

Ilist -> 0
       |  [Id, Ilist]

Tlist -> 0
      |  [T, Tlist]

Flist -> 0
      |  [F, Flist]

*/

// Some library functions

FUN foldright
: f, 0, e       => e
: f, [x, xs], e => f(x, foldright(f, xs, e))

FUN mem
: ?,        0 => FALSE
: x, [=x,  ?] => TRUE
: x, [ ?, ys] => mem(x, ys)

FUN newmem
: x, xs => mem(x,xs) -> xs, mk2(x,xs)

// End of library functions

FUN replace
: u1, u2, =u1          => u2
: u1, u2, [Fun, a, ts] => mk3(Fun, a, map (replace(u1,u2)) ts)
:  ?,  ?, t            => t

FUN abstract
: i, t, [Pred, a, ts] => mk3(Pred, a, map (replace(t, Bound i) ts)
: i, t, [Conn, b, ps] => mk3(Conn, b, map (abstract i t) ps)
: i, t, [Quant, qut, b, p] => mk4(Quant, qnt, b, abstract(i+1)t p)

FUN subst
: i, t, [Pred, a, ts] => mk3(Pred, a, map (replace(Bound i, t) ts)
: i, t, [Conn, b, ps] => mk3(Conn, b, map (subst i t) ps)
: i, t, [Quant, qut, b, p] => mk4(Quant, qnt, b, subst(i+1)t p)

FUN prec_of
:  '~'   =>  4
:  '&'   =>  3
:  '|'   =>  2
:  '->'  =>  1
:  '='   =>  1
:        => -1   // means not infixed

FUN accumform
: f, [Pred, ?, ts], xs => foldright f (ts, xs)
: f, [Conn, ?, ps], xs => foldright(accumform f) (ps, xs)
: f, [Quant, ?, ?, p], xs => accumform f (p, xs)

FUN accumgoal
: f ((ps, qs), xs) => foldright f (ps, foldright f (qs, xs))

// returns the set of vars belonging to a term
FUN termvars
: [Var, a], bs => newmem(a,bs)
: [Fun, ?, ts], bs => foldright termvars (ts, bs)
: ?, bs => bs

// returns the set of vars belonging to a goal
FUN goalvars
= accumgoal (accumform termvars)

FUN termparams
: [Param, a, bs], pairs => newmem((a,bs), pairs)
: [Fun, ?, ts], pairs => foldright termparams (ts, pairs)
: ?, pairs => pairs

FUN goalparams
= accumgoal (accumform termparams)

/*
Syntax:

Tlist -> T ,..., T

T -> I ( T ,..., T)
  |  I
  |  ? I

F -> F & F
  |  F | F
  |  F -> F
  |  F = F
  |  P

P -> @ I . F   // eg.   !z. P(z) -> (@x. P(x))
  |  ! I . F
  |  ~ P
  |  ( F )
  |  I ( T ,..., T )
  |  I

Corresponding tree:

Tlist -> 0
      |  [Comma, T, Tlist]
      |  T

T -> [Ap,  I, Tlist]    //  P(x,y)     Q
  |  [Var, I]           //  ?a1

F -> [All, I, F]        // @x. F
  |  [Ex,  I, F]        // !x. F
  |  [And, F, F]        // F & F
  |  [Or,  F, F]        // F | F
  |  [Imp, F, F]        // F -> F
  |  [Eqv, F, F]        // F = F
  |  [Neg, F]           // ~ F
  |  [Fun, I, Tlist]    // I(I,..,I)
*/


