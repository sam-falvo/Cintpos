// (c) Martin Richards    17 February 1997

/*

This is a demonstration theorem prover written in MCPL based loosely
on the ML program in Technical Report No 192: "Designing a Theorem
Prover" by Larry Paulson.

*/

// It is in the very early stages of development.

GET "mcpl.h"

/*
The syntax of Terms and Formulae are given below. Within the syntax
I denotes identifiers.


T -> I ( T ,..., T)
  |  I
  |  ? I

Tlist -> T ,..., T

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

The corresponding Abstract Syntax Tree has the following structure:


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

MANIFEST                        // Tokens and Tree Operators
  Unknown, Eof,
  Id, Numb, Query, Var, Ap, Comma, Lparen, Rparen,
  Not, And, Or, Imp, Eqv, All, Ex, Dot, Semicolon,
  Sqnt,

  // Commands
  Goal, Tree, Step, Steps, Run, Rungoal, Quit

MANIFEST                         // Exception names
  E_hard, E_lex, E_syn



/*************************************************************
*
*            Main program
*
*    start()
*    
*
**************************************************************/

FUN start : =>
  LET argv = VEC 50
  LET filename = "data.prv"

  stdin := input()
  stdout := output()

  writef "Theorem Prover (14 Feb 1997)\n"

  IF rdargs("FROM,TO/K", argv, 50)=0 DO
  { writef "Bad arguments for thmprover\n"
    RETURN 20
  }

  UNLESS argv!0=0 DO filename := argv!0

  fromstr := findinput(filename)

  IF fromstr=0 DO
  { writef("Trouble with file %s\n", filename)
    RETURN 20
  }

  selectinput(fromstr)

  docommands()

  idtab_close()
  mk_close()

//********** End of Main program ****************************



/************************************************************
*
*            Space Allocation Functions and Data
*
*   mk_init(upb)
*   mk_close()
*   mkvec(upb)
*   mk1(op)
*   mk2(op, a)
*   mk3(op, a, b)
*   mk4(op, a, b, c)
*   mk5(op, a, b, c, d)
*  
*
************************************************************/

STATIC
  mkv=0, mkp=0, mkt=0
 
FUN mk_init : upb =>
  mkv := getvec(upb)
  IF mkv=0 RAISE (E_hard, "Unable to allocate work space", upb)
  mkt := mkv + upb
  mkp := mkt

FUN mk_close : => IF mkv DO freevec mkv

FUN mkvec : upb =>
  mkp := @ mkp!-(upb+1)
  IF mkp<mkv RAISE (E_hard, "Insufficient work space", upb)
  RETURN mkp

FUN mk1 : op => 
  LET p = mkvec 0
  !p := op
  RETURN p

FUN mk2 : op, a =>
  LET p = mkvec 1
  !p, p!1 := op, a
  RETURN p

FUN mk3 : op, a, b =>
  LET p = mkvec 2
  !p, p!1, p!2 := op, a, b
  RETURN p

FUN mk4 : op, a, b, c =>
  LET p = mkvec 3
  !p, p!1, p!2, p!3 := op, a, b, c
  RETURN p

FUN mk5 : op, a, b, c, d =>
  LET p = mkvec 4
  !p, p!1, p!2, p!3, p!4 := op, a, b, c, d
  RETURN p


//********** End of Space Allocation code *******************



/************************************************************
*
*           String Functions
*
*    eqstr(s1, s2)     returns TRUE if equal
*    lenstr s          return length not including final 0
*    copystr(s1, s2)   copies s1 to s2
*
************************************************************/

FUN eqstr : s1, s2 =>
{ LET ch1=%s1++, ch2=%s2++
  IF ch1=0 AND ch2=0 RETURN TRUE
  IF ch1 ~= ch2      RETURN FALSE
} REPEAT

FUN lenstr : s =>
  LET len = 0
  WHILE %s++ DO len++
  RETURN len

FUN copystr : s1, s2 =>
{ LET ch = %s1++
  %s2++ := ch
  IF ch=0 RETURN
} REPEAT

//********** End String Functions code **********************





/************************************************************
*
*            Symbol Table Functions and Data
*
*    idtab_init()
*    id_close()
*    hash(str)     returns a 31 bit hash of the string str
*    lookup(str)   return a node for identifier str, creating
*                  one if necessary
*
************************************************************/

STATIC
  idtab=0

MANIFEST
  Idtabsize=541, Idtabupb=Idtabsize-1

FUN idtab_init : =>
  idtab := getvec Idtabupb
  FOR i = 0 TO Idtabupb DO idtab!i := 0

FUN idtab_close : => 
  freevec idtab
  idtab := 0

FUN hashstr : str =>
  LET hashval = 19609 // This and 31397 are primes.
  WHILE %str DO hashval := (hashval XOR %str++) * 31397
  TEST hashval>=0 THEN RETURN hashval
                  ELSE RETURN hashval>>1

FUN lookup : str =>
  LET hash = hashstr str
  LET i    = hash MOD Idtabsize
  LET w    = idtab!i
 
  // An idnode has form [ token, hashchain, hash, chars ... ]
  // where token is either Id or a reserved word token and 
  // hash is a 31 bit hash of the identifier (to reduce the number
  // string comparisons).

  UNTIL w=0 DO
  { MATCH w: [tok, chain, whash, chars] =>
      IF hash=whash AND eqstr(str, @chars) RETURN w
      w := chain
  }

  // matching node not found, so make one.
 
  w := mkvec((lenstr str + 1)/ Bytesperword + 3)
  MATCH w : [tok, chain, whash, chars] =>
             tok, chain, whash, idtab!i := Id, idtab!i, hash, w
             copystr(str, @chars)
             RETURN w

FUN clearhints : =>
  FOR i = 0 TO Idtabupb DO
  { LET p = idtab!i
    WHILE p MATCH p
            : [tok, chain, hint, chars] => hint, p := 0, chain
  }


//********** End of Id Table code ***************************



/************************************************************
*
*             Lexical Analyser Functions and Data
*
*    lex_init()
*    lex()           set token and possibly lexval to next
*    lineno          line number of current character
*
*    add_reserved(token, str)   put a reserved word into
*                               the symbol table
*    rdword()        read an identifier into word
*    rdnumber()      read a decimal number, returns it value
*    rdstring()      read a VSL string, return a string node
*    rch()           update ch and nch
*
************************************************************/


STATIC
  ch, nch, lineno=0, token, lexval,
  word = CVEC 5000,
  fromstr=0, tostr=0, stdin=0, stdout=0

FUN lex_init : =>
  // Set up the symbol table
  idtab_init()

  add_reserved(Goal,    "goal")
  add_reserved(Tree,    "tree")
  add_reserved(Step,    "step")
  add_reserved(Steps,   "steps")
  add_reserved(Run,     "run")
  add_reserved(Rungoal, "rungoal")
  add_reserved(Quit,    "quit")

  lineno := 1
  rch()
  rch()

FUN add_reserved : tok, str =>
  LET w = lookup(str)
  !w := tok

FUN lex : =>
{ MATCH (ch, nch)

  : '\n'          => lineno++
                     rch()
                     LOOP

  : ' ' | '\t'    => rch()
                     LOOP

  : '/' , '/'     => rch() REPEATUNTIL ch='\n' OR ch=Endstreamch
                     LOOP

  : '~'           => token := Not;         rch(); RETURN
  : '&'           => token := And;         rch(); RETURN
  : '|', '-'      => token := Sqnt; rch(); rch(); RETURN
  : '|'           => token := Or;          rch(); RETURN
  : '='           => token := Eqv;         rch(); RETURN
  : '@'           => token := All;         rch(); RETURN
  : '!'           => token := Ex;          rch(); RETURN
  : '('           => token := Lparen;      rch(); RETURN
  : ')'           => token := Rparen;      rch(); RETURN
  : '?'           => token := Query;       rch(); RETURN
  : ','           => token := Comma;       rch(); RETURN
  : ';'           => token := Semicolon;   rch(); RETURN
  : '.'           => token := Dot;         rch(); RETURN

  : '-' , '>'     => token := Imp;  rch(); rch(); RETURN

  : 'A'..'Z' | 'a'..'z' =>
                     lexval := lookup(rdword())
                     token := lexval!0
                     RETURN

  : '0'..'9'      => lexval := rdnumber()
                     token := Numb
                     RETURN

  : Endstreamch   => token := Eof
                     RETURN

  : '#'           => token := Eof
                     RETURN

  :  ?            => RAISE (E_lex, "Bad character %c", ch)
                     LOOP

} REPEAT


FUN rdword : => 0
  LET p = 0
  
  WHILE 'A'<=ch<='Z' OR 'a'<=ch<='z' OR '0'<=ch<='9' DO
    { word%p++ := ch; rch() }
  word%p := 0

  RETURN word
  
FUN rdnumber : => 
  LET res = 0
  WHILE '0'<=ch<='9' DO { res := 10*res + ch - '0'; rch() }
  RETURN res
  
FUN rch : => ch := nch; nch := rdch()


//********** End of Lexical Analyser code *********************


/**************************************************************
*
*            Syntax Analyser Functions and Data
*
*    parse()  returns the parse tree
*
*    prog()
*    prim()
*    exp(n)
*    sequent()
*    com()
*
**************************************************************/

FUN parse : => sequent()

FUN checkfor
: tok, mes => UNLESS token=tok RAISE (E_syn, mes)
              lex()

FUN prim : =>
  MATCH token
  : Id     => LET a = lexval
              lex()
              UNLESS token=Lparen RETURN a
              lex()
              LET args = 0
              UNLESS token=Rparen DO args := explist()
              checkfor(Rparen, "')' expected")
              RETURN mk3(Ap, a, args)
               
  : Lparen => lex()
              LET a = exp(0)
              checkfor(Rparen, "')' expected")
              RETURN a

  : Query  => lex()
              LET id = lexval
              checkfor(Id, "Identifier expected")
              RETURN mk2(Var, id)

  : Not    => RETURN mk2(Not, nexp 4)

  : All    => lex()
              LET id = lexval
              checkfor(Id, "Identifier expected")
              checkfor(Dot, "'.' expected")
              RETURN mk3(All, id, exp 0)

  : Ex     => lex()
              LET id = lexval
              checkfor(Id, "Identifier expected")
              checkfor(Dot, "'.' expected")
              RETURN mk3(Ex, id, exp 0)

  :        => RAISE (E_syn, "Bad expression, token = %d", token)

FUN nexp : n => lex()
                RETURN exp n

FUN exp : n => 
  LET a = prim()

  { MATCH (token, n)
    : And, <3 => a := mk3(And, a, nexp 3)
    : Or,  <2 => a := mk3(Or,  a, nexp 2)
    : Eqv, <1 => a := mk3(Eqv, a, nexp 1)
    : Imp, <1 => a := mk3(Imp, a, nexp 1)
    :         => RETURN a
  } REPEAT

FUN explist : =>
  LET a = exp 0
  UNLESS token=Comma RETURN a
  lex()
  RETURN mk3(Comma, a, explist())

FUN sequent : =>
  LET a=0, b=0
  TEST token=Sqnt
  THEN { lex()
         b := explist()
       }
  ELSE { a := explist()
         TEST token=Sqnt
         THEN { lex()
                b := explist()
              }
         ELSE a, b := 0, a
       }
  mk3(Sqnt, a, b)

//********** End of Syntax Analyser code **********************




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
    : [Id, ?, ?, chars]        => writef("%s", @chars);     RETURN
    : [Var, [id, ?, ?, chars]] => writef("Var %s", @chars); RETURN

    : [Ap,  x, y]         => opstr, upb := "Ap",       2
    : [And, x, y]         => opstr, upb := "And",      2
    : [Or,  x, y]         => opstr, upb := "Or",       2
    : [Imp, x, y]         => opstr, upb := "Imp",      2
    : [Eqv, x, y]         => opstr, upb := "Eqv",      2
    : [All, x, y]         => opstr, upb := "All",      2
    : [Ex,  x, y]         => opstr, upb := "Ex",       2
    : [Not, x]            => opstr, upb := "Not",      1
    : [Comma, x, y]       => opstr, upb := "Comma",    2
    : [Sqnt, x, y]        => opstr, upb := "Sqnt",     2
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


FUN prgoal
: 0            => RETURN
:[ Sqnt, a, b] => prexplist a
                  writef(" |- ")
                  prexplist b
:              => RETURN

FUN prexplist
: 0             => RETURN
: [Comma, a, b] => prexp(a, 0); writef ", "; prexplist b
: a             => prexp a

FUN prexp : a, prec =>

  LET s1="", s2="", s3="", p=0

  IF a=0 DO { writef "Nil"; RETURN }
 
  { MATCH a
    : [Id, ?, ?, chars]        => writef("%s",  @chars); RETURN
    : [Var, [id, ?, ?, chars]] => writef("?%s", @chars); RETURN

    : [Ap,  x, y]         => p := 5;     s2, s3 := "(", ")"
    : [And, x, y]         => p := 4;     s2     := "&"
    : [Or,  x, y]         => p := 3;     s2     := "|"
    : [Imp, x, y]         => p := 2;     s2     := "->"
    : [Eqv, x, y]         => p := 2;     s2     := "="
    : [All, x, y]         => p := 1; s1, s2     := "@", "."
    : [Ex,  x, y]         => p := 1; s1, s2     := "!", "."
    : [Not, x],           => writef "~"; prexp(x, 4); RETURN
    :                     => writef "???";            RETURN
  }

  IF prec>p DO writef "("
  writef s1
  prexp(a!1, p)
  writef s2
  prexp(a!2, p)
  writef s3
  IF prec>p DO writef ")"
              

//********** End of Print Tree code ***************************

//********** Do Commands **************************************

STATIC goaltable=0

FUN docommands : =>
{ { mk_init()
    lex_init()

    lex()

    { MATCH token
      : Quit | Eof => RETURN

      : Semicolon  => lex()

      : Goal       => lex()
                      writef "\ngoal\n"
                      goaltable := parse()
                      prgoal goaltable
                      newline()

      : Tree       => lex()
                      writef "\ntree\n"
                      prtree(goaltable, 0, 20)
                      newline()

      : Step       => lex()
                      writef "\nstep\n"

      : Steps      => lex()
                      LET n = lexval
                      checkfor(Numb, "Number expected\n")
                      writef("\nsteps %d\n", n)
                      lex()

      : Run        => lex()
                      writef "\nrun\n"

      : Rungoal    => lex()
                      writef "\nrungoal\n"

      : op         => writef("Bad command, op = %d\n", op); lex()
    } REPEAT

  } HANDLE
    : E_hard,      mess, a => writef(mess, a); newline()
    : E_lex|E_syn, mess, a => writef("Syntax error near line %d: ",
                                                            lineno)
                              writef(mess, a); newline()
} REPEAT