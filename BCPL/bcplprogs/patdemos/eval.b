/*
This evaluate some simple lambda expression making extensive use
of the new pattern matching features now available in BCPL.

Implemented by Martin Richards (c) 11 Oct 2021
*/

GET "libhdr"

MANIFEST {
  Eof=0
  Id; Num; Mul; Div; Pos; Neg; Add; Sub
  Eq; Cond; Lam; Ap; Y
  Fn; Env
}

GLOBAL {
  spacep:ug
  str; strp
  rch; ch; nch
  lex; token; lexval
  mk1; mk2; mk3; mk4
  lookup
  parse
  prim; exp; nexp
  wrtree
  try; eval
}

LET mk1 : a => VALOF
{ spacep := spacep-1
  !spacep := a
  RESULTIS spacep
}
LET mk2 : a, b       => VALOF { mk1(b); RESULTIS mk1(a) }
LET mk3 : a, b, c    => VALOF { mk1(c); mk1(b); RESULTIS mk1(a) }
LET mk4 : a, b, c, d => VALOF { mk1(d); mk1(c); mk1(b); RESULTIS mk1(a) }

LET lookup
: [?,letter],   0 => 0

: [?,letter], [?,[?,=letter], val] => val

: n[?,letter], [ ?,?,?,e] => lookup(n, e)

LET intval
: exp, env => MATCH(eval(exp, env))
              : [Num, val] => val
	      :            => VALOF
	                      { writef("Error: expression:*n")
			        wrtree(exp, 0, 20)
				writef("*nin environment:*n")
                                wrtree(env, 0, 20)
				writef("*ndoes not yield a number*n")
				RESULTIS 0
			      }
LET eval
:n[Id,  letter],  env => VALOF
                         { LET val = lookup(n, env)
                           IF val=0 DO
		           { writef("Error: Id %c not declared*n", letter)
		             writef("The environment is:*n")
		             wrtree(env, 0, 20)
			     newline()
			     val := mk2(Num, 0)
			   }
                           RESULTIS val
		         }
:t[Num, k],       ? => t
: [Pos, x],       env => eval(x, env)
: [Neg, x],       env => mk2(Num, - intval(x, env))
: [Mul, x,y],     env => mk2(Num, intval(x, env) * intval(y, env))
: [Div, x,y],     env => mk2(Num, intval(x, env) / intval(y, env))
: [Add, x,y],     env => mk2(Num, intval(x, env) + intval(y, env))
: [Sub, x,y],     env => mk2(Num, intval(x, env) - intval(y, env))
: [Eq,  x,y],     env => mk2(Num, intval(x, env) = intval(y, env))

: [Cond,b,x,y],   env => intval(b, env) -> eval(x, env), eval(y, env)

: [Lam, id,body], env => mk4(Fn, id, body, env)
:t[Ap,  x,y],     env => MATCH (eval(x, env))
                       : [Fn, id, body, env1] =>
                            eval(body, mk4(Env, id, eval(y, env), env1))
		       :  => VALOF { writef("Error trying to evaluate*n")
		                     wrtree(t, 0, 20)
                                     newline()
		 	             RESULTIS 0
			           }
                       .
: [Y, exp],       env => MATCH(eval(exp, env))
  // The argument should be a closure: [Fn, id, body, env]
  // whose body is a function such as: Lf Ln n=0 -> 1, n*f(n-1)
  : [Fn, f, [Lam, n, body], env1] => VALOF
    { LET res  = mk4(Fn, n, body, 0)    // environment to be filled in later
      LET env2 = mk4(Env, f, res, env1) // make environment including f
      res!3 := env2                     // fill in environment component of
                                        // the closure
      RESULTIS res                      // return this new closure
    }
  .
: exp, env => VALOF
  { writef("Error trying to evaluate expressin:*n")
    wrtree(exp, 0, 20)
    writef("*nin environment:*n")
    wrtree(env, 0, 20)
    newline()
    RESULTIS 0
  }

// Construct       Corresponding Tree

// a ,.., z   -->  [Id, 'a'] ,..,  [Id, 'z']    Single letter identifiers
// dddd       -->  [Num, dddd]                  Decimal numbers
// x y        -->  [Ap, x, y]                   Application
// Y x        -->  [Y, x]                       Application of the Y operator
// x * y      -->  [Mul, x, y]
// x / y      -->  [Div, x, y]
// x + y      -->  [Add, x, y]
// x - y      -->  [Sub, x, y]
// x = y      -->  [Eq, x, y]
// b -> x, y  -->  [Cond, b, x, y]
// Li x       -->  [Lam, [Id,i], x]

// When a lambda expression is evaluated it yields a closure
// of the form [Fn, id, body, env]

LET wrtree(x, n, d) BE
{ LET v = TABLE 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  LET size = 0
 // writef("  wrtree: x=%i5 n=%n ", x, n)
  //IF x DO writef("[%i3 %i5 %i5 %i5] ", x!0, x!1, x!2, x!3)
  size := VALOF MATCH (x)
  : 0              BE { writes("Nil");            RETURN }
  : [ Num, val   ] BE { writef("Num: %n", val);   RETURN }
  : [ Id,  letter] BE { writef("Id: %c", letter); RETURN }
  : [ Fn         ] BE { writes("Fn");             RESULTIS 4 }
  : [ Env        ] BE { writes("Env");            RESULTIS 4 }
  : [ Lam        ] BE { writes("Lam");            RESULTIS 3 }
  : [ Cond       ] BE { writes("Cond");           RESULTIS 4 }
  : [ Mul        ] BE { writes("Mul");            RESULTIS 3 }
  : [ Div        ] BE { writes("Div");            RESULTIS 3 }
  : [ Add        ] BE { writes("Add");            RESULTIS 3 }
  : [ Sub        ] BE { writes("Sub");            RESULTIS 3 }
  : [ Eq         ] BE { writes("Eq");             RESULTIS 3 }
  : [ Ap         ] BE { writes("Ap");             RESULTIS 3 }
  : [ Pos        ] BE { writes("Pos");            RESULTIS 2 }
  : [ Neg        ] BE { writes("Neg");            RESULTIS 2 }
  : [ Y          ] BE { writes("Y");              RESULTIS 2 }
  : ?              BE { writes("Unkbown")         RETURN }

  IF n>=d DO { writes("Etc"); RETURN }

  FOR i = 2 TO size DO               // Output the operands, if any
  { newline()
    FOR j=0 TO n-1 DO writes( v!j )
    writes("**-")
    v!n := i=size->"  ","! "
    wrtree(x!(i-1), n+1, d)
  }
}
 
LET rch : BE
{ ch := nch
  TEST strp > str%0
  THEN { nch := Eof
       }
  ELSE { nch := str%strp
         strp := strp+1
       }
}

LET parse : s => VALOF
{ str, strp := s, 1
  nch := ' '
  rch()  // Get ch and nch
  rch()
  // ch is the first charatcter of the lambda expression
  RESULTIS nexp(0)
}

AND lex : BE MATCH (ch, nch)
: '-',       '>' BE { token := Cond
                      rch(); rch()
		      RETURN
		    }

: ' ' | '*n', ?  BE { rch(); lex(); RETURN }

: 'a'..'z',   ?  BE { token, lexval := Id, ch; rch(); RETURN }

: '0'..'9',   ?  BE { token, lexval := Num, ch-'0'
                      { rch()
                        MATCH (ch)
                        :'0'..'9' BE { lexval := 10*lexval+ch-'0'
			               LOOP
				     }
                        : ?       BE RETURN
                      } REPEAT
                    }

: '(' | ')' | '**' | '/' | '+' | '-' | 'L' | 'Y' | '=' | ',',
              ?  BE { token := ch; rch()
	              RETURN
	            }

:   0,        ?  BE { token := Eof; RETURN }

:   ch,     nch  BE { writef("Lex error: %c %c*n", ch, nch)
                      RETURN
		    }

AND prim : => MATCH (token)
  : Id   => VALOF { LET a = mk2(Id, lexval)
                    lex()
                    RESULTIS a
                  }
  : Num  => VALOF { LET a = mk2(Num, lexval)
                    lex()
                    RESULTIS a
                  }
  : 'Y'  => mk2(Y, nexp(6))
  : 'L'  => VALOF { LET bv = ?
                    lex()
                    UNLESS token=Id DO
                    { writef("Name expected*n")
                      lexval := 0
                    }
                    bv := prim() // Form the Id node
                    RESULTIS mk3(Lam, bv, exp(0))
                  }
  : '('  => VALOF { LET a = nexp(0)
                    UNLESS token=')'DO writef("')' expected*n")
                    lex()
                    RESULTIS a
	          }
  : '+'  => mk2(Pos, nexp(3))
  : '-'  => mk2(Neg, nexp(3))
  :  ?   => VALOF { writef("Syntax error*n")
                    RESULTIS 0
                  }

AND nexp : n => VALOF { lex(); RESULTIS exp(n) }

AND exp : prec => VALOF
{ LET a = prim()
  { MATCH (token, prec)
    :  '(' | Num | Id,
             <6 BE a := mk3(   Ap, a,  exp(6))
    :  '**', <5 BE a := mk3(  Mul, a, nexp(5))
    :  '/',  <5 BE a := mk3(  Div, a, nexp(5))
    :  '+',  <4 BE a := mk3(  Add, a, nexp(4))
    :  '-',  <4 BE a := mk3(  Sub, a, nexp(4))
    :  '=',  <3 BE a := mk3(   Eq, a, nexp(3))
    : Cond,  <1 BE { LET b = nexp(0)
                      UNLESS token=',' DO
                        writef("Syntax error in -> construct*n")
                      a := mk4(Cond, a, b, nexp(0))
	            }
    :           BE RESULTIS a
  } REPEAT
}

AND try : exp BE
{ LET tree = 0
  LET res = 0
  LET v = VEC 1000
  spacep := v+1000
  newline()
  writef("Expression %s has the following parse tree:*n", exp)
  tree := parse(exp)
  wrtree(tree, 0, 20)
  newline()
  res := eval(tree, 0)
  writef("Its result after evaluation is:*n")
  wrtree(res, 0, 20)
  newline()
}

AND start : => VALOF
{ try("1234")
  try("100/4+2")
  try("100-23")
  try("100**23")
  try("100/23")
  try("100=23")
  try("100=100")
  try("1=2 -> 3, 4")
  try("(Lx x) 99")
  try("(Lx x) (Ly y) 99")
  try("(Lx x+1) 2")

  // The following tests demonstrates that S K K = I where the
  // combinators S, K and I are defined as follows
  //    S f g x = f x (g x)
  //    K x y   = x
  //    I x     = x
  try("(Li (i 111)) (Lx x)")
  try("(Lk (k 222 333)) (Lx Ly x)")
  try("(Ls (s (Lx Ly x+y) (Lx x+1) 10)) (Lf Lg Lx f x (g x))")
  
  try("(Ls Lk s k k 1234) (Lf Lg Lx f x (g x)) (Lx Ly x)")

  // The next test defines a recursive function for factorial
  // using Y and uses it to compute factorial 5 = 120.
  try("(Y ( Lf Ln (n=0->1,n**f(n-1)) )) 5")
  RESULTIS 0
}

