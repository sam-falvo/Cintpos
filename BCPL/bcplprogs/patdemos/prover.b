GET "libhdr"

/* This is a very free translation into BCPL by M. Richards
   of the LISP version of the Wang Algorithm given in the
   LISP 1.5 book.
*/

MANIFEST {
  Id=1; Not; And; Or; Imp; Eqv  // Lexical tokens
  Lparen; Rparen
  Eof=-1
}

GLOBAL {
  stdout:ug
  str; strp; ch; nch
  token; lexval
  member
  add
  pr
  synerr
  lex_init
  rch
  lex
  pars
  prim
  nexp
  exp
  spacev; spacep
  mk_init
  mk_close
  mk1; mk2; mk3
  try
  prtree
  prlinev
}

LET member : ?,        0 => FALSE
           : x, [=x,  ?] => TRUE
           : x, [ ?, ys] => member(x, ys)

LET add : x, xs => member(x, xs) -> xs, mk2(x, xs)

LET pr
:  ?,  0,  ?,               0 => FALSE

: al,  0, ar,   [[Not,x], cr] => VALOF
  { LET a, b = x, 0
    RESULTIS pr( al, @a, ar, cr )
  }

: al,  0, ar, [[And,x,y], cr] => VALOF
  { LET a, b = x, cr
    LET c, d = y, cr
    RESULTIS pr( al,        0, ar, @a ) &
             pr( al,        0, ar, @c ) -> TRUE, FALSE
  }

: al,  0, ar,  [[Or,x,y], cr] => VALOF
  { LET a, b = y, cr
    LET c, d = x, @a
    RESULTIS pr( al,        0, ar, @c, x)
  }
  
: al,  0, ar, [[Imp,x,y], cr] => VALOF
  { LET a, b = x, 0
    LET c, d = y, cr
    RESULTIS pr( al, @a, ar, @c, x)
  }

: al,  0, ar, [[Eqv,x,y], cr] => VALOF
  { LET a, b = x, 0
    LET c, d = y, cr
    LET e, f = y, 0
    LET g, h = x, cr
    RESULTIS pr( al, @a, ar, @c, x) &
             pr( al, @e, ar, @g, x) -> TRUE, FALSE
  }

: al, 0, ar, [[Id,x], cr] => member(x, al) |
                             pr( al, 0, add(x,ar), cr ) -> TRUE, FALSE

: al, [[Not,x],cl], ar, cr  => VALOF
  { LET a, b = x, cr
    RESULTIS pr( al, cl, ar, @a )
  }

: al, [[And,x,y],cl], ar, cr  => VALOF
  { LET a, b = y, cl
    LET c, d = x, @a
    RESULTIS pr( al, @c, ar, cr )
  }

: al, [[Or,x,y],cl], ar, cr  => VALOF
  { LET a, b = x, cl
    LET c, d = y, cl
    RESULTIS pr( al, @a, ar, cr ) &
             pr( al, @c, ar, cr ) -> TRUE, FALSE
  }

: al, [[Imp,x,y],cl], ar, cr  => VALOF
  { LET a, b = y, cl
    LET c, d = x, cr
    RESULTIS pr( al, @a, ar, cr ) &
             pr( al, cl, ar, @c ) -> TRUE, FALSE
  }

: al, [[Eqv,x,y],cl], ar, cr => VALOF
  { LET a, b = y, cl
    LET c, d = x, @a
    LET e, f = y, cr
    LET g, h = x, @e
    RESULTIS pr( al, @c, ar, cr ) &
             pr( al, cl, ar, @g )
  }

: al, [[Id,x],cl], ar, cr => member(x,ar) |
                             pr( add(x,al), cl, ar, cr ) -> TRUE, FALSE

//********************* Lexical Analyser ******************

LET lex_init : s BE
{ str, strp := s, 0
  nch := ' '
  rch(); rch()
  // ch and nch are now the first and second characters of the string.
}

LET rch : BE { ch := nch
               TEST strp >= str%0
	       THEN nch := endstreamch // =-1
	       ELSE { strp := strp+1
	              nch := str%strp
		    }
	       //writef("ch = ")
	       //TEST ch>=32
	       //THEN writef("'%c'", ch)
	       //ELSE writef("%i3",  ch)
	       //newline()
	     }

LET lex : BE
{ MATCH (ch, nch)
  : ' ' | '*n' BE { rch(); lex();            RETURN }
  : 'P'|'A'..'Z'   BE { token := Id
                        lexval := ch
			EXIT
                      }
  : '('        BE { token := Lparen;           EXIT }
  : ')'        BE { token := Rparen;           EXIT }
  : '~'        BE { token := Not;              EXIT }
  : '&'        BE { token := And;              EXIT }
  : '|'        BE { token := Or;               EXIT }
  : '='        BE { token := Eqv;              EXIT }
  : '-', '>'   BE { token := Imp; rch();       EXIT }
  : -1         BE { token := Eof;            RETURN }
  : c          BE { synerr("Unexpected character '%c'*n", c)
		    EXIT
 		  }
  rch()
}

//********************* Syntax Analyser ******************

// A .. Z -->  [Id, 'A'] .. [Id, 'Z']
// ~x     -->  [Not,  x]
// x & y  -->  [And,x,y]
// x | y  -->  [Or, x,y]
// x -> y -->  [Imp,x,y]
// x = y  -->  [Eqv,x,y]

LET synerr : mess, a, b BE
{ writef("Syntac error: ")
  writef(mess, a, b)
  newline()
  FOR i = 1 TO strp-1 DO wrch(str%i)
  newline()
  rch()
}

LET parse : s => VALOF { LET a = 0
                         lex_init(s)
			 a := nexp(0)
			 UNLESS token=Eof DO
			   synerr("Incorrect termination")
                         RESULTIS a
                       }

AND prim() = VALOF
{ LET a = prim1()
  //writef("prim => %n*n", a)
  //prtree(a, 0, 20)
  //newline()
  //abort(1234)
  RESULTIS a
}

AND prim1 : => VALOF MATCH (token)
: Id     BE { LET a = mk2(Id, lexval)
              lex()
	      //abort(1001)
              RESULTIS a
	    }
: Lparen BE { LET a = nexp(0)
              UNLESS token=Rparen DO
	        synerr("Problem with a parenthesised expression")
              lex()
              RESULTIS a
	    }
: Not    BE RESULTIS mk2(Not, nexp(3))
: c      BE synerr("Bad character '%c' in expression", c)

LET nexp : n => VALOF { lex(); RESULTIS exp(n) }

LET exp : n => VALOF
{ LET a = exp1(n)
  //writef("exp(%n) => %n*n", n, a)
  //prtree(a, 0, 20)
  //newline()
  RESULTIS a
}

AND exp1 : n => VALOF
{ LET a = prim()

  { MATCH (token, n)
    : And, <3 BE { a := mk3(And, a, nexp(3)); LOOP } 
    : Or,  <2 BE { a := mk3(Or , a, nexp(2)); LOOP } 
    : Imp, <1 BE { a := mk3(Imp, a, nexp(1)); LOOP } 
    : Eqv, <1 BE { a := mk3(Eqv, a, nexp(1)); LOOP } 
    :         BE RESULTIS a
  } REPEAT
}

//********************* Space Allocation ******************

LET mk_init : upb BE
{ spacev := getvec(upb)
  IF spacev=0 DO { writef("More space needed*n")
 	           abort(999)
                 }
  spacep := @ spacev!upb
}

LET mk_close : => freevec(spacev)

LET mk1 : x => VALOF
{ spacep:=spacep-1
  !spacep := x
  //writef("%i5: %i5*n", spacep, x) 
  RESULTIS spacep
}

LET mk2 ( x, y ) = VALOF { mk1(y); RESULTIS mk1(x) }
LET mk3 : x, y, z => VALOF { mk1(z); mk1(y); RESULTIS mk1(x) }

//********************* Main Program **********************

LET try : e BE
{ LET tree, zero = 0, 0
  writef("*nTesting expression: %s*n", e)
  mk_init(100_000)
  tree := parse(e)
  writef("Its parse tree is:*n")
  prtree(tree, 0, 20)
  newline()
  TEST pr(0, 0, 0, @tree)
  THEN writef("-------- This is always true*n")
  ELSE writef("-------- This id NOT always true*n")
  mk_close()
}

// Propositional examples supplied by Larry Paulson 
// and modified by MR

LET start : => VALOF
{ writef("*nAssociative laws of & and |*n")
  try("P=P")
  try("(P & Q) & R  =  P & (Q & R)")
  try("(P | Q) | R  =  P | (Q | R)")

  writef("*nDistributive laws of & and |*n")
  try("(P & Q) | R  = (P | R) & (Q | R)")
  try("(P | Q) & R  = (P & R) | (Q & R)")

  writef("*nLaws involving implication*n")
  try("(P|Q -> R) = (P->R) & (Q->R)")
  try("(P & Q -> R) = (P-> (Q->R))")
  try("(P -> Q & R) = (P->Q)  &  (P->R)")

  writef("*nClassical theorems*n")
  try("P | Q  ->  P | ~P & Q")
  try("(P->Q)&( ~P->R)  ->  (P&Q | R)")
  try("P & Q | ~P & R =  (P->Q) & (~P->R)")
  try("(P->Q) | (P->R) = (P -> Q | R)")
  try("(P = Q) = (Q = P)")

  /* Sample problems from F.J. Pelletier,
     Seventy-Five Problems for Testing Automatic Theorem Provers,
     J. Automated Reasoning 2 (1986), 191-216.
  */

  writef("*nProblem 5*n")
  try("((P|Q)->(P|R)) -> (P|(Q->R))")

  writef("&nProblem 9*n")
  try("((P|Q) & ( ~P | Q) & (P | ~Q)) ->  ~( ~P | ~Q)")

  writef("*nProblem 12.  Dijkstra's law*n")
  try("((P  =  Q)  =  R)  ->  (P  =  (Q  =  R))")

  writef("*nProblem 17*n")
  try("(P & (Q->R) -> S) = (( ~P | Q | S) & ( ~P | ~R | S))")

  writef("*nFALSE GOALS*n")
  try("(P | Q -> R) = (P -> (Q->R))")
  try("(P->Q) = (Q ->  ~P)")
  try(" ~(P->Q) -> (Q = P)")
  try("((P->Q) -> Q)  ->  P")
  try("((P | Q) & (~P | Q) & (P | ~Q)) ->  ~(~P | Q)")

  writef("*nIndicates need for subsumption*n")
  try("((P & (Q = R)) = S) = (( ~P | Q | S) & ( ~P | ~R | S))")

// Prove that the circuit
//      -----
// X --| NOT |--> Y
//      -----
// is equivalent to:
//      -----       -----       -----
// A --| NOT |--B--| NOT |--C--| NOT |--> D
//      -----       -----       -----
  writef("*nProof of the correctness of a circuit*n")
  try("(Y=~X) & ((D=~C) & (C=~B) & (B=~A)) & (X=A)  ->  (Y=D)")
  RESULTIS 0
}

/**************************************************************
*
*            Print Tree Functions and Data
*
*    prtree(tree, depth, maxdepth)    
*    prlinev
*
**************************************************************/

LET prtree : x, depth, maxdepth BE 
{ LET opstr=0
  LET size=1
  LET v = TABLE 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

  IF x=0 DO { writef("Nil"); RETURN }

//writef("x=%n -> [%n %n %n ...]*n", x, x!0, x!1, x!2)
//abort(1000)
  MATCH (x)
  : [Id, ch]            BE { writef("Id %c", ch); RETURN }
  : [And, x, y]         BE opstr, size := "And",      3
  : [Or,  x, y]         BE opstr, size := "Or",       3
  : [Imp, x, y]         BE opstr, size := "Imp",      3
  : [Eqv, x, y]         BE opstr, size := "Eqv",      3
  : [Not, x]            BE opstr, size := "Not",      2
  :                     BE { opstr, size := "Unknown",  1
  //abort(1001)
  }
 
  IF depth=maxdepth DO { writef("Etc"); RETURN }
 
  writef("%s", opstr)

  FOR i = 1 TO size-1 DO { newline()
                           FOR j=0 TO depth-1 DO writes( v!j )
                           writes("**-")
                           v!depth := i=size-1-> "  ", "! "
                           prtree(x!i, depth+1, maxdepth)
                         }
}
