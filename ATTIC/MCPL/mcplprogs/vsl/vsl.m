// (c) Martin Richards    February 1997

/*

This is a demonstration compiler written in MCPL for the 
very simple language VSL used in the book by J.P. Bennett
entitled "Introduction to Compiling Techniques", McGraw-Hill
(1990)

*/

// It is in the early stages of development.
// It currently forms the parse tree from a program
// and prints it out


GET "mcpl.h"

/*
The syntax of VSL is below. I, N and S denote identifiers, numbers
and strings. The notation [ xxx ] denote zero or more repetitions 
of xxx, for example I [ , I ] denotes
   I
   I , I
   I , I , I
   ...
etc. 

Program -> F [ F ]                   -- program

F -> FUNC I ( ) C                    -- function definition
   | FUNC I ( I [ , I ] ) C

C -> Id := E                         -- commands
   | RETURN E
   | PRINT P [ , P ]
   | CONTINUE
   | IF E THEN C FI
   | IF E THEN C ELSE C FI
   | WHILE E DO C DONE
   | C C
   | { C }
   | { D [ D ] C }

P -> S                               -- print arguments
   | E

E -> I                               -- expressions
   | Numb
   | ( E )
   | I ( )
   | I ( E [ , E ] )
   | - E
   | E * E
   | E / E
   | E + E
   | E - E

D -> VAR I [ , I ]                   -- declarations


The corresponding Abstract Syntax Tree has the following structure,
before scope has been called to replace Id nodes have been replace 
by their corresponding Var nodes.


Program -> F                         -- program
         | [Seq, F, Program]

F -> [ Func, V, 0,  C]               -- function definitions
   | [ Func, V, VL, C]

C -> [ Assign, I, E]                 -- commands
   | [ Return, E]
   | [ Print, PL ]
   | [ Continue ]
   | [ If1, E, C ]
   | [ If2, E, C, C ]
   | [ While, E, C ]
   | [ Seq, C, C ]
   | [ Var, VL, C ]

E -> I                               -- expressions
   | [ Numb, k ]
   | [ Apply, I, EL ]
   | [ Neg, E ]
   | [ Mul, E, E ]
   | [ Div, E, E ]
   | [ Add, E, E ]
   | [ Sub, E, E ]

EL -> E                              -- expression list
    | [Comma, E, EL]

P -> [ String, <chars> ]             -- print item
   | E

PL -> P                              -- print item list
    | [Comma, P, PL]

I -> [Id, -, -, <chars> ]            -- identifier

V -> [Var, I, -, -, -]               -- variable

VL -> V                              -- variable list
    | [Comma, V, VL]

*/

MANIFEST                        // Tokens and Tree Operators
  Unknown,
  Id, Numb, String, Apply, Comma, Seq,
  Mul, Div, Add, Sub, Neg,
  Assign, Return, Print, Continue,
  If1, If2, While, Var, Vardef, Func,
  Lparen, Rparen,Lbrace, Rbrace,
  If, Then, Else, Fi, Do, Done, Eof

MANIFEST                         // Exception names
  E_hard, E_lex, E_syn, E_sc, E_trn



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
  LET filename = "prog.vsl"

  stdin := input()
  stdout := output()

  writef "VSL (31 Jan 1997)\n"

  IF rdargs("FROM,TO/K,TREE/S", argv, 50)=0 DO
  { writef "Bad arguments for VSL\n"
    RETURN 20
  }

  UNLESS argv!0=0 DO filename := argv!0

  fromstr := findinput(filename)

  IF fromstr=0 DO
  { writef("Trouble with file %s\n", filename)
    RETURN 20
  }

  selectinput(fromstr)

  { mk_init()
    lex_init()

    lex()

    LET tree = parse()

    scope(tree)

    writef "\nParse Tree:\n\n"
    prtree(tree, 0, 20)
    newline()

  } HANDLE
    : E_hard,      mess, a => writef(mess, a); newline()
    : E_lex|E_syn, mess, a => writef("Syntax error near line %d: ",
                                                            lineno)
                              writef(mess, a); newline()
    : E_sc,        mess, a => writef("Scope error: ")
                              writef(mess, a); newline()

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

  add_reserved(Func,     "FUNC")
  add_reserved(Print,    "PRINT")
  add_reserved(Return,   "RETURN")
  add_reserved(Continue, "CONTINUE")
  add_reserved(If,       "IF")
  add_reserved(Then,     "THEN")
  add_reserved(Else,     "ELSE")
  add_reserved(Fi,       "FI")
  add_reserved(While,    "WHILE")
  add_reserved(Do,       "DO")
  add_reserved(Done,     "DONE")
  add_reserved(Var,      "VAR")

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

  : '/'           => token := Div;     rch(); RETURN
  : '*'           => token := Mul;     rch(); RETURN
  : '+'           => token := Add;     rch(); RETURN
  : '-'           => token := Sub;     rch(); RETURN
  : ','           => token := Comma;   rch(); RETURN
  : '('           => token := Lparen;  rch(); RETURN
  : ')'           => token := Rparen;  rch(); RETURN
  : '{'           => token := Lbrace;  rch(); RETURN
  : '}'           => token := Rbrace;  rch(); RETURN

  : ':' , '='     => token := Assign;  rch(); rch(); RETURN

  : 'A'..'Z' | 'a'..'z' =>
                     lexval := lookup(rdword())
                     token := lexval!0
                     RETURN

  : '0'..'9'      => lexval := rdnumber()
                     token := Numb
                     RETURN

  : '"'           => lexval := rdstring()
                     token := String
                     RETURN

  : Endstreamch   => token := Eof
                     RETURN

  : '.'           => token := Eof
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
  

FUN rdstring : =>
  LET len = 0

  { rch()
    IF ch='"'  OR ch=Endstreamch BREAK
    IF ch='\\' DO { rch(); IF ch='n' DO ch := '\n' } 
    word%len++ := ch
  } REPEAT

  word%len := 0
  rch()

  LET w = mkvec((len + 1)/Bytesperword + 1)
  w!0 := String
  copystr(word, @ w!1)
  RETURN w

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
*    com()
*
**************************************************************/

FUN parse : => prog()

FUN checkfor
: tok, mes => UNLESS token=tok RAISE (E_syn, mes)
              lex()

FUN prog : => checkfor(Func, "FUNC expected")
              LET fn = lexval
              checkfor(Id, "Function name expected")
              fn := mk5(Var, fn, 0, 0, 0)
              checkfor(Lparen, "'(' expected")
              LET parms = 0
              UNLESS token=Rparen DO parms := varlist()
              checkfor(Rparen, "')' expected")
              LET a = mk4(Func, fn, parms, com())
              UNLESS token=Func RETURN a 
              RETURN mk3(Seq, a, prog())

FUN prim : =>
  MATCH token
  : Id     => LET a = lexval
              lex()
              UNLESS token=Lparen RETURN a
              lex()
              LET args = 0
              UNLESS token=Rparen DO args := explist()
              checkfor(Rparen, "')' expected")
              RETURN mk3(Apply, a, args)
               
  : Numb   => LET a = mk2(Numb, lexval)
              lex()
              RETURN a
 
  : Lparen => lex()
              LET a = exp(0)
              checkfor(Rparen, "')' expected")
              RETURN a

  : Sub    => RETURN mk2(Neg, nexp 4)

  :        => RAISE (E_syn, "Bad expression, token = %d", token)

FUN nexp : n => lex()
                RETURN exp n

FUN exp : n => 
  LET a = prim()

  { MATCH (token, n)
    : Mul, <5 => a := mk3(Mul, a, nexp 5)
    : Div, <5 => a := mk3(Div, a, nexp 5)
    : Add, <4 => a := mk3(Add, a, nexp 4)
    : Sub, <4 => a := mk3(Sub, a, nexp 4)
    :         => RETURN a
  } REPEAT

FUN explist : =>
  LET a = exp 0
  UNLESS token=Comma RETURN a
  lex()
  RETURN mk3(Comma, a, explist())

FUN com : =>
  MATCH token
  : Id       => // I := E
                LET lhs = lexval
                lex()
                checkfor(Assign, "':=' expected")
                RETURN mk3(Assign, lhs, exp 0)
          
  : Return   => // RETURN E
                RETURN mk2(Return, nexp 0)

  : Print    => // PRINT P [ , P ]
                lex()
                RETURN mk2(Print, printlist())

  : Continue => // CONTINUE
                LET a = lexval
                lex()
                RETURN a

  : If       => // IF E THEN C FI
                // IF E THEN C ELSE C FI
                LET e = nexp 0
                checkfor(Then, "'THEN' expected")
                LET c1=com(), c2=0
                IF token=Else DO { lex(); c2 := com() }
                checkfor(Fi, "'FI' expected")
                TEST c2=0 THEN RETURN mk3(If1, e, c1)
                          ELSE RETURN mk4(If2, e, c1, c2)

  : While    => // WHILE E DO C DONE
                LET e = nexp 0
                checkfor(Do, "'Do' expected")
                LET c = com()
                checkfor(Done, "'DONE' expected")
                RETURN mk3(While, e, c)

  : Lbrace   => // { C }
                // { D [ D ] C }
                lex()
                LET a = rdblockbody()
                checkfor(Rbrace, "'}' expected")
                RETURN a

  :          => 0

FUN comlist : => LET a = com()
                 UNLESS a=0 DO a := mk3(Seq, a, comlist())
                 RETURN a
  
FUN rdblockbody : =>
  UNLESS token=Var RETURN comlist()
  lex()
  LET ids = varlist()
  LET body = rdblockbody()
  RETURN mk3(Vardef, ids, body)

FUN varlist : =>
  LET a = lexval
  checkfor(Id, "name expected")
  a := mk5(Var, a, 0, 0, 0)
  IF token=Comma DO { lex(); a := mk3(Comma, a, varlist()) }
  RETURN a

FUN printlist : =>
  LET a = printitem()
  IF token=Comma DO { lex(); a := mk3(Comma, a, printlist()) }
  RETURN a

FUN printitem : =>
  UNLESS token=String RETURN exp 0
  LET a = lexval
  lex()
  RETURN a

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
    : [Numb, k]           => writef("%d", k);          RETURN
    : [Id, ?, ?, chars]   => writef("%s", @chars);     RETURN
    : [Var, id, ?, k, n]  => writef("Var %d %d ", k, n)
                             prtree(id, depth+1, maxdepth)
                             RETURN
    : [String, chars]     => writef("\"%s\"", @chars); RETURN
    : [Func, id, args, c] => opstr, upb := "Func",     3
    : [Apply, x, y]       => opstr, upb := "Apply",    2
    : [Mul, x, y]         => opstr, upb := "Mul",      2
    : [Div, x, y]         => opstr, upb := "Div",      2
    : [Add, x, y]         => opstr, upb := "Add",      2
    : [Sub, x, y]         => opstr, upb := "Sub",      2
    : [Neg, x]            => opstr, upb := "Neg",      1
    : [Seq, x, y]         => opstr, upb := "Seq",      2
    : [Comma, x, y]       => opstr, upb := "Comma",    2
    : [Assign, x, y]      => opstr, upb := "Assign",   2
    : [If1, x, y]         => opstr, upb := "If1",      2
    : [If2, x, y, z]      => opstr, upb := "If2",      3
    : [While, x, y]       => opstr, upb := "While",    2
    : [Continue]          => opstr, upb := "Continue", 0
    : [Print, x]          => opstr, upb := "Print",    1
    : [Return, x]         => opstr, upb := "Return",   1
    : [Vardef, x, y]      => opstr, upb := "Vardef",   2

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




/**************************************************************
*
*            Scope Rule Functions and Data
*
*    scope(tree)
*       
*
**************************************************************/

MANIFEST Fnlab=1, Local

STATIC labno=0, ssp=0

FUN scope : tree =>
  writef "scope called\n"

  clearhints()

  LET fenv = scfuncs (tree, 0)

  scfnbodies(tree, fenv)

FUN scfuncs
: 0,              e => RETURN e

: [Seq, f, rest], e => RETURN scfuncs(rest, scfuncs(f, e))

: [Func, v[var, [id, chain, hint], env, k, n], args, body], e =>
                       hint  := v
                       env, k, n := e, Fnlab, ++labno
                       RETURN v 

:                   => RAISE(E_sc, "Compiler error in scfuncs")

FUN scfnbodies
: 0,              e => RETURN e

: [Seq, f, rest], e => RETURN scfnbodies(rest, scfnbodies(f, e))

: [Func, v, vl, body], e =>
                       ssp := 0
                       LET env = scvars(vl, e)
                       sccom(body, env)
                       undeclvars(e, env)
                       RETURN v 

:                   => RAISE(E_sc, "Compiler error in scfnbodies")

FUN scvars
:  0, e  => RETURN e

: [Comma, p1, p2], e => scvars(p2, scvars(p1, e))

: v[Var, [id, chain, hint, chars], env, k, n], e
                     => hint, env, k, n := v, e, Local, ++ssp

:                   => RAISE(E_sc, "Compiler error in scvars")

FUN undeclvars : e, env =>
  UNTIL e=env MATCH env
              : [Var, [id, chain, hint, chars], next, k, n]
                  => hint, env := 0, next


FUN sccom
: 0,                  env => RETURN
: [ Assign, v, e],    env => scexp(@v,  env)
                             scexp(@e,  env)
: [ Return, e],       env => scexp(@e,  env)
: [ Print, pl ],      env => scexp(@pl, env)
: [ Continue ],       env => RETURN
: [ If1, e, c ],      env => scexp(@e, env)
                             sccom(c, env)
: [ If2, e, c1, c2 ], env => scexp(@e, env)
                             sccom(c1, env)
                             sccom(c2, env)
: [ While, e, c ],    env => scexp(@e, env)
                             sccom(c, env)
: [ Seq, c1, c2 ],    env => sccom(c1, env)
                             sccom(c2, env)
: [ Vardef, vl, c ],  env => LET s = ssp
                             LET nenv = scvars(vl, env)
                             sccom(c, nenv)
                             undeclvars(env, nenv)
                             ssp := s
:                   => RAISE(E_sc, "Compiler error in sccom")

FUN findvar
: n[id, chain, hint, chars], env =>

  IF hint RETURN hint

  WHILE env MATCH env 
            : v[var, =n,    ?] => RETURN v
            :  [var, ?,  next] => env := next 

  RAISE(E_sc, "Name '%s' not declared", @ chars )


FUN scexp
: 0,                 env => RETURN

: [0],               env => RETURN

: [[Comma|Apply|Mul|Div|Add|Sub,
           x, y]],   env => scexp( @x, env)
                            scexp( @y, env)

: [[Numb|String]],   env => RETURN

: [v[Id, chain, hint, chars]], env => v := findvar(v, env)

: [[Neg, e]],        env => scexp(@e, env)

: [[op]],            env => RAISE (E_sc, "Bad expression %d", op)

                    

//********** End of Scope Rule code ***************************



/**************************************************************
*
*            Translation Phase Functions and Data
*
*    translate()
*       
*
**************************************************************/

FUN translate : tree => RETURN

//********** End of Translation Phase *************************




