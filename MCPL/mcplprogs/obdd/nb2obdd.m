// (c) Martin Richards    8 May 1997

/*

This is a demonstration program to read a combinatorial
logic circuit given in Berkeley .netblif format and convert
it into OBDD form.
*/

// It is in the early stages of development.

GET "mcpl.h"

/*
The syntax of a .netblif file is given below. Within the grammar
N denotes a signal name, eg: 1   [525]   86GAT(26)
The notation [ xxx ] denote zero or more repetitions 
of xxx, for example N [ N ] denotes
   N
   N  N
   N  N  N
   ...
etc. 

Circuit ->                                             -- program
     .model N .inputs [ N ] .outputs [ N ] G [ G ] .end

G -> .gate nor1 a=N O=N                                -- gates
     .gate nor2 a=N b=N O=N
     .gate nor3 a=N b=N c=N O=N
     .gate nor4 a=N b=N c=N d=N O=N


The corresponding Abstract Syntax Tree has the following structure.


Circuit -> [ Model, I, IL, IL, GL]   -- circuit

I -> [I, -, <chars> ]                -- Identifier

IL -> 0
      I
      [Seq, I, IL]

GL -> 0
      [Nor1, GL, I, I]               -- output N, then inputs
      [Nor2, GL, I, I, I]
      [Nor3, GL, I, I, I, I]
      [Nor4, GL, I, I, I, I, I]

*/


MANIFEST                        // Tokens and Tree Operators
  Unknown,
  Id, Seq, Model, Inputs, Outputs, Gate, End, Eq,
  Nor1, Nor2, Nor3, Nor4, Va, Vb, Vc, Vd, Out, Eof

MANIFEST                         // Exception names
  E_hard, E_lex, E_syn, E_trn



/*************************************************************
*
*            Main program
*
*    start()
*    
*
**************************************************************/

STATIC optreverse, optauto, optpr

FUN start : =>
  LET argv = VEC 50
  LET filename = "../mintcode/netblif/C432.netblif"
//LET filename = "../mintcode/netblif/rot.netblif"
//LET filename = "../mintcode/netblif/demo.netblif"

  stdin := input()
  stdout := output()

  writef "nb2obdd (8 May 1997)\n"

  IF rdargs("FROM,TO/K,TREE/S,REVERSE/S,AUTO/S,PR/S", argv, 50)=0 DO
  { writef "Bad arguments for VSL\n"
    RETURN 20
  }

  optreverse := argv!3
  optauto    := argv!4
  optpr      := argv!5

  UNLESS argv!0=0 DO filename := argv!0

  fromstr := findinput filename

  IF fromstr=0 DO
  { writef("Trouble with file %s\n", filename)
    RETURN 20
  }

  IF argv!1 DO
  { tostr := findoutput(argv!1)

    IF tostr=0 DO
    { writef("Trouble with file %s\n", argv!1)
      RETURN 20
    }
    selectoutput tostr
  }


  { mk_init 1700000

    selectinput fromstr
    lex_init()

    lex()

    LET tree = parse()
    endread()
    selectinput stdin

    IF argv!2 DO { writef "\nParse Tree:\n\n"
                   prtree(tree, 0, 20)
                   newline()
                 }
    initvte()
    initfgh()
    mkobdd tree
  } HANDLE
    : E_hard,      mess, a => writef(mess, a); newline()
    : E_lex|E_syn, mess, a => writef("Syntax error near line %d: ",
                                                            lineno)
                              writef(mess, a); newline()
    : E_trn,       mess, a => writef("Translation error: ")
                              writef(mess, a); newline()
    .
  fghtab_close()
  vtetab_close()
  idtab_close()
  mk_close()
  IF argv!1 DO endwrite()
  selectoutput stdout
  writef "End of run\n"
  RETURN 0

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
  mkv := getvec upb
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

FUN mk6 : op, a, b, c, d, e =>
  LET p = mkvec 5
  !p, p!1, p!2, p!3, p!4, p!5 := op, a, b, c, d, e
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

FUN cleardata : =>
  FOR i = 0 TO Idtabupb DO
  { LET p = idtab!i
    WHILE p MATCH p
            : [tok, chain, data, chars] => data, p := -1, chain
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
*    rch()           update ch
*
************************************************************/


STATIC
  ch, lineno=0, token, lexval,
  word = CVEC 5000,
  fromstr=0, tostr=0, stdin=0, stdout=0

FUN lex_init : =>
  // Set up the symbol table
  idtab_init()

  add_reserved(Model,    ".model")
  add_reserved(Inputs,   ".inputs")
  add_reserved(Outputs,  ".outputs")
  add_reserved(Gate,     ".gate")
  add_reserved(Nor1,     "nor1")
  add_reserved(Nor2,     "nor2")
  add_reserved(Nor3,     "nor3")
  add_reserved(Nor4,     "nor4")
  add_reserved(Va,       "a=")
  add_reserved(Vb,       "b=")
  add_reserved(Vc,       "c=")
  add_reserved(Vd,       "d=")
  add_reserved(Out,      "O=")

  lineno := 1
  rch()

FUN add_reserved : tok, str =>
  LET w = lookup(str)
  !w := tok

FUN lex1 : => lex1(); writef("token = %d", token)
                     IF token=Id DO writef("  %s", @lexval!3)
                     newline()
FUN lex : =>
{ MATCH ch

  : '\n'          => lineno++
                     rch()
                     LOOP

  : ' '|'\t'|'\\' => rch()
                     LOOP

  : '#'           => rch() REPEATUNTIL ch='\n' OR ch=Endstreamch
                     LOOP

  : '='           => token := Eq;      rch(); RETURN

  : Endstreamch   => token := Eof
                     RETURN

  :               => lexval := lookup(rdword())
                     token := lexval!0
                     RETURN

} REPEAT


FUN rdword : => 0
  LET p = 0
  
  UNTIL ch=' ' OR ch='\n' OR ch=Endstreamch DO
    { word%p++ := ch
      IF ch='=' DO { rch(); BREAK }
      rch()
    }
  word%p := 0

  RETURN word
  

FUN rch : => ch := rdch()


//********** End of Lexical Analyser code *********************


/**************************************************************
*
*            Syntax Analyser Functions and Data
*
*    parse()  returns the parse tree
*
*    circuit()
*    idlist(n)
*    gates()
*
**************************************************************/

FUN parse : => circuit()

FUN checkfor
: tok, mes => UNLESS token=tok RAISE (E_syn, mes)
              lex()

FUN circuit : => checkfor(Model, ".model expected")
                 LET model=lexval, inputs=0, outputs=0
                 checkfor(Id, "Model name expected")
                 checkfor(Inputs, ".inputs expected")
                 inputs := idlist()
                 checkfor(Outputs, ".outputs expected")
                 outputs := idlist()
                 RETURN mk5(Model, model, inputs, outputs, gates())

FUN idlist : =>
  LET a = lexval
  UNLESS token=Id RAISE(E_syn, "Id expected")
  lex()
  TEST token=Id THEN RETURN mk3(Seq, a, idlist())
                ELSE RETURN a

FUN gates : =>
  IF token=Gate DO
  { LET g = gate()
    TEST token=Gate THEN RETURN mk3(Seq, g, gates())
                    ELSE RETURN g
  }
  checkfor(Id, ".end expected")
  RETURN 0

FUN gate : => 
  checkfor(Gate, ".gate expected")
  MATCH token
  : Nor1 => lex()
            LET a = rdv Va
            RETURN mk3(Nor1, rdv Out, a)
  : Nor2 => lex()
            LET a = rdv Va
            LET b = rdv Vb
            RETURN mk4(Nor2, rdv Out, a, b)
  : Nor3 => lex()
            LET a = rdv Va
            LET b = rdv Vb
            LET c = rdv Vc
            RETURN mk5(Nor3, rdv Out, a, b, c)
  : Nor4 => lex()
            LET a = rdv Va
            LET b = rdv Vb
            LET c = rdv Vc
            LET d = rdv Vd
            RETURN mk6(Nor4, rdv Out, a, b, c, d)
  : End  => RETURN 0
  :      => RAISE(E_syn, "Unknown gate type")

FUN rdv : kind =>
  checkfor(kind, "Bad gate argument")
//  checkfor(Eq, "'=' expected")
  LET id = lexval
  checkfor(Id, "Id expected")
  RETURN id

//********** End of Syntax Analyser code **********************




/**************************************************************
*
*            Print Tree Functions and Data
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
    : [Id, ?, ?, chars]     => writef("%s", @chars);     RETURN
    : [Seq, x, y]           => opstr, upb := "Seq",      2
    : [Nor1, x, y]          => opstr, upb := "Nor1",     2
    : [Nor2, x, y, z]       => opstr, upb := "Nor2",     3
    : [Nor3, x, y, z, t]    => opstr, upb := "Nor3",     4
    : [Nor4, x, y, z, t, s] => opstr, upb := "Nor4",     5
    : [Model, x, y, z, g]   => opstr, upb := "Model",    4

    :                       => opstr, upb := "Unknown",  0
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
                  writef("V%d  *%d", x!1, x)
                }
           ELSE   writef("V%d   %d", x!1, x)

  FOR i = 2 TO 3 DO { newline()
                      FOR j=0 TO depth-1 DO writes( prlinev!j )
                      writes("*-")
                      prlinev!depth := i=3-> "  ", "! "
                      probdd(x!i, depth+1, maxdepth)
                    }


//********** End of Print Tree and OBDD code ******************

STATIC lastvar, cellno

FUN mkobdd 
: [Model, name, inputs, outputs, gates] =>
  cleardata()
  lastvar := 0
  doinputs inputs
  newline()
  dogates gates
  cellno := 0
  dooutputs outputs
  writef("\nOBDD Size = %d\n", cellno)
  

: => RAISE(E_trn, "Bad tree")

FUN doinputs
: [Id,  x, y, n] => y := optauto -> -1, allocvar(@n)
: [Seq, x, y, n] => doinputs x; doinputs y
:                => RAISE(E_trn, "Bad inputs")

FUN dooutputs
: [Id, x, y, name]  => LET a = obddsize y
                       IF optpr DO
                       { writef("\nOutput %s = %4d", @name, a)
                         TEST y&1 THEN writef("~\n\n")
                                  ELSE writef("\n\n")
                         newline()
                       }
: [Seq, x, y] => dooutputs x
                 dooutputs y
:             => RAISE(E_trn, "Bad inputs")

FUN obddsize
: f(0|1) => RETURN 0
: f => MATCH f&-2
       : [s, v(>0), t, e] => LET var = v
                             LET ts=0, es=0
                             IF t>1 AND (t&-2)!1>0 DO ts := -1
                             IF e>1 AND (e&-2)!1>0 DO es := -1
                             LET a = obddsize t
                             LET b = obddsize e
                             IF ts DO ts := !(t&-2)
                             IF es DO es := !(e&-2)
                             s := 1+ts+es
                             cellno++
                             v := -cellno
                             IF optpr DO
                             { writef("Node%4d:%9d  V%3d", cellno, s, var)
                               IF a AND cellno-a<100 DO a := a-cellno
                               IF b AND cellno-b<100 DO b := b-cellno
                               TEST t&1 THEN writef("  %4d~", a)
                                        ELSE writef("  %4d ", a)
                               TEST e&1 THEN writef("  %4d~", b)
                                        ELSE writef("  %4d ", b)
                               writef("  %9d", ts)
                               writef("  %9d", es)
                               newline()
                             }
                             RETURN cellno
       : [?,     v, t, e] => RETURN -v

FUN dogates
: [Nor1, [Id, ?, o, on]out,
         [Id, ?, a, an]] => IF a<0 DO a := allocvar(@an)
                            o := ite(a, 0, 1)
                            prsig out           // Debugging trace

: [Nor2, [Id, ?, o, on]out,
         [Id, ?, a, an],
         [Id, ?, b, bn]] => IF a<0 DO a := allocvar(@an)
                            IF b<0 DO b := allocvar(@bn)
                            LET x = ite(a, 1, b)
                            o    := ite(x, 0, 1)
                            prsig out           // Debugging trace

: [Nor3, [Id, ?, o, on]out,
         [Id, ?, a, an],
         [Id, ?, b, bn],
         [Id, ?, c, cn]] => IF a<0 DO a := allocvar(@an)
                            IF b<0 DO b := allocvar(@bn)
                            IF c<0 DO c := allocvar(@cn)
                            LET x = ite(a, 1, b)
                            x    := ite(x, 1, c)
                            o    := ite(x, 0, 1)
                            prsig out           // Debugging trace

: [Nor4, [Id, ?, o, on]out,
         [Id, ?, a, an],
         [Id, ?, b, bn],
         [Id, ?, c, cn],
         [Id, ?, d, dn]] => IF a<0 DO a := allocvar(@an)
                            IF b<0 DO b := allocvar(@bn)
                            IF c<0 DO c := allocvar(@cn)
                            IF d<0 DO d := allocvar(@dn)
                            LET x = ite(a, 1, b)
                            x    := ite(x, 1, c)
                            x    := ite(x, 1, d)
                            o    := ite(x, 0, 1)
                            prsig out           // Debugging trace

: [Seq, x, y], n => dogates x
                    dogates y
:                => RAISE(E_trn, "Bad gates")

FUN allocvar : name => writef("V%2d:  %s\n", lastvar+1, name)
                       RETURN findvte(++lastvar, 1, 0)

FUN prsig : [id, ?, o, name] =>
//  writef("Space left =%d words,    Signal %s:\n", (mkp-mkv)/4, @name)
//  probdd(o, 0, 6)
//  newline()

FUN ite1 : f, g, h =>                   // Debugging trace
  writef("\nite: %7d %7d %7d\n", f, g, h)
  writef "\nf is:\n"; probdd(f, 0, 5)
  writef "\ng is:\n"; probdd(g, 0, 5)
  writef "\nh is:\n"; probdd(h, 0, 5)
  newline()
  ite1(f,g,h)

// ite(f,g,h) forms the OBBD for:          IF f THEN g ELSE h
//            where f, g and h are OBDDs.
FUN ite
: 1,        g,        ? => g            // T-> g, h  =>  g
: 0,        ?,        h => h            // F-> g, h  =>  h
: f,        1,        0 => f            // f-> T, F  =>  f
: f,        0,        1 => f XOR 1      // f-> F, T  => ~f
: ?,        g,       =g => g            // f-> g, g  =>  g
: f,       =f,        h => ite(f, 1, h) // f-> f, h  =>  f->T,h
: f, =1 XOR f,        h => ite(f, 0, h) // f->~f, h  =>  f->F,h
: f,        g      , =f => ite(f, g, 0) // f-> g, f  =>  f->g,F
: f,        g, =1 XOR f => ite(f, g, 1) // f-> g,~f  =>  f->g,T


: f,        1,    h(<f) => ite(h,       1,       f) // f-> T, h  ==  h-> T, f
: f,    g(<f),        0 => ite(g,       f,       0) // f-> g, F  ==  g-> f, F
: f,    g(<f),        1 => ite(1 XOR g, 1 XOR f, 1) // f-> g, T  == ~g->~f, T
: f,        0,    h(<f) => ite(1 XOR h, 0, 1 XOR f) // f-> F, h  == ~h-> F,~f
: f,    g(<f), =1 XOR g => ite(g,       f, 1 XOR f) // f-> g,~g  ==  g-> f,~f

// Now ensure that both f and g are positive edges.

                                                      // neg f and neg h
: f(=f|1), g, h(=h|1) => ite(f-1, h-1, 1 XOR g) XOR 1 // => ~(~f->~h,~g)

                                                      // neg f and pos h
: f(=f|1), g,       h => ite(f-1, h,         g)       // =>  (~f-> h, g)

                                                      // pos f and neg g
: f, g(=g|1),       h => ite(f,   g-1, 1 XOR h) XOR 1 // => ~( f->~g,~h)

: f, g,  h =>                                         // pos f and pos g

  LET r = findfgh(f, g, h)
  IF r>=0 RETURN r
  LET v = topvar(f, g, h)
  LET t = ite(set1(f, v), set1(g, v), set1(h, v))
  LET e = ite(set0(f, v), set0(g, v), set0(h, v))
  IF t=e RETURN t
  r := findvte(v, t, e)
  insertfgh(f, g, h, r)
  RETURN r

FUN topvar : f, g, h =>
  LET lim = optreverse -> 0, 1000000
  LET a = f>1 -> (f&-2)!1, lim
  LET b = g>1 -> (g&-2)!1, lim
  LET c = h>1 -> (h&-2)!1, lim
  TEST optreverse THEN { IF b AND b>a DO a := b
                         IF c AND c>a DO a := c
                       }
                  ELSE { IF b AND b<a DO a := b
                         IF c AND c<a DO a := c
                       }

  RETURN a

// set1(f, v) return the obbd for f with variable v set to 1
//            (if v is in f, it must be the top variable).
FUN set1 : f(0|1), ? => f
         : f,      v => MATCH f&-2
                        : [?, =v, t, e] => t XOR f&1
                        :               => f

// set0(f, v) return the obbd for f with variable v set to 0
//            (if v is in f, it must be the top variable).
FUN set0 : f(0|1), ? => f
         : f,      v => MATCH f&-2
                        : [?, =v, t, e] => e XOR f&1
                        :               => f

/*************** The fgh hash table ************************/

STATIC fghtab

MANIFEST Fghtabupb=100001

FUN initfgh : => 
  fghtab := getvec Fghtabupb
  UNLESS fghtab RAISE(E_trn, "Unable to allocate fghtab")
  FOR i = 0 TO Fghtabupb DO fghtab!i := 0

FUN fghtab_close : => 
  IF fghtab DO freevec fghtab
  fghtab := 0

FUN findfgh : f, g, h =>
  LET hashval = fghhash(f, g, h)
  LET p = fghtab!hashval
  WHILE p DO { IF p!1=f AND p!2=g AND p!3=h DO
               {
/*
writef "\nfindfgh: Found *******************\n"
writef "f:\n"; probdd(f,0,5); newline()
writef "g:\n"; probdd(g,0,5); newline()
writef "h:\n"; probdd(h,0,5); newline()
writef "r:\n"; probdd(p!4,0,5); newline()
newline()
abort 1234
*/
                 RETURN p!4
               }
               p := p!0
             }
  RETURN -1

FUN insertfgh : f, g, h, r =>
/*
writef "\ninsertfgh:\n"
writef "f:\n"; probdd(f,0,5); newline()
writef "g:\n"; probdd(g,0,5); newline()
writef "h:\n"; probdd(h,0,5); newline()
writef "r:\n"; probdd(r,0,5); newline()
newline()
*/
  LET hashval = fghhash(f, g, h)
  fghtab!hashval := mk5(fghtab!hashval, f, g, h, r)


FUN fghhash : f, g, h => (f*12345 + (g+1)*(h+3))>>1 MOD Fghtabupb

/**************** End of vte hash table **********************/

/*************** The vte hash table **************************/

STATIC vtetab

MANIFEST Vtetabupb=100001

FUN initvte : => 
  vtetab := getvec Vtetabupb
  UNLESS vtetab RAISE(E_trn, "Unable to allocate vtetab")
  FOR i = 0 TO Vtetabupb DO vtetab!i := 0

FUN vtetab_close : => 
  IF vtetab DO freevec vtetab
  vtetab := 0

FUN findvte : v, t, e =>
  LET hashval = vtehash(v, t, e)
  LET p = vtetab!hashval
  WHILE p DO { IF p!1=v AND p!2=t AND p!3=e RETURN p
               p := p!0
             }
  p := mk4(vtetab!hashval, v, t, e)
  vtetab!hashval := p                     
  RETURN p

FUN vtehash : v, t, e => (v*12345 + (t+1)*(e+3))>>1 MOD Vtetabupb

/**************** End of vte hash table **********************/







