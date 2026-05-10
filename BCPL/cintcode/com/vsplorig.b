/*
This is a compiler and interpreter for the language VSPL
implemented in BCPL

(c) Martin Richards 20 March 2003

History

06/10/2025
Extended VSPL to allow manifest constants, initialised static
variables, and the commands break and loop. These greatly simplify
vspl.vs, the implementation of vspl in vspl.

07/08/2021
Updated

*/


GET "libhdr"
 
MANIFEST {

h1=0; h2=1; h3=2; h4=3; h5=4; h6=5

nametablesize = 541

c_tab         =   9
c_newline     =  10
c_newpage     =  12
c_space       =  32
c_cr          =  13

// Lexical tokens, parse tree operators and op-codes
  
Num=1; Name; String; True; False
Valof; Fnap; Lv; Ind; Vecap
Neg; Not; Mul; Div; Mod; Add; Sub
Eq; Ne; Le; Ge; Lt; Gt; Lsh; Rsh; And; Or; Xor
Comma; Fndef; Rtdef; Assign; Rtap
Resultis; Break; Loop; Return; Seq
Test; If; Unless; While; Until; For
Let; Vec; Static; Statvec; Manifest; Initval; Decl; Var; Const
Lparen; Rparen; Lsquare; Rsquare; Lcurly; Rcurly
To; Do; Then; Else; Be; Eof; Semicolon
Rtrn; Fnrn; Addr; Local; Lab; Data; Jt; Jf; Jump;
Ln; Lp; Llp; Ll; Laddr; Sp; Sl; Stind; Lres
Entry; Stack; Printf; Sys; Halt
}
 
GLOBAL { 
rec_p:ug; rec_l; fin_p; fin_l
fatalerr; synerr; trnerr; errcount; errmax
progfilename; tofilename
progstream; tostream
mk1; mk2; mk3; mk4; mk5; mk6
newvec; treep; treevec
optTokens; optTree; optCode; optTrace

// Globals used in LEX
chbuf; charv; ch; rch; lex; token; lexval; wordnode
chv
wrchbuf; chcount; lineno
dsw; declsyswords; namestart; nametable; lookupword
rdstrch; rdtag

// Globals used in SYN
checkfor; rdprog; rdblockbody
rnamelist; rstatlist; rname
rdef; rncom; rcom
formtree; plist
rexplist; rdseq
rnexp; rexp; rbexp
 
// Globals used in TRN and the interpreter
 
trnext:300; trprog; trcom; decldyn
declstatnames; checkdistinct; addname; cellwithname
trdecl; undeclare; jumpcond
prevvalue
assign; evalconst; load; fnbody; loadlist; transname
dvec; dvece; dvecp; dvect
comline; procname; resultlab; looplab; breaklab
ssp
outf; outfn; outfl; outfs; outentry
outlab; outvar; outstatvec; outstring; opstr; hasOperand
mem; memt; regs
codev; codep; codet; datav; datap; datat; stack; stackt
labv; refv; labmax; putc; putd; putref
setlab; nextlab; labnumber; resolvelabels
interpret; printf
}

LET rdarg(str) = VALOF
{ // Return the length of the argument found
  LET len = 0
  LET ch = 0

  ch := rdch() REPEATWHILE ch='*s'
  
  { IF ch='*n' | ch=endstreamch DO
    { unrdch()
      BREAK
    }

    UNLESS 'a' <= ch <= 'z' |  // Only allow these
           'A' <= ch <= 'Z' |  // characters in arguments.
           '0' <= ch <= '9' |
	   ch='.' | ch='/'  |
	   ch='-'           BREAK

    len := len+1
    str%0   := len
    str%len := ch
    ch := rdch()
  } REPEAT

  RESULTIS len // Return TRUE if an argument found
}

LET args() = VALOF
{ LET res = FALSE
  optTokens := FALSE
  optTree   := FALSE
  optCode   := FALSE
  optTrace  := FALSE

  progfilename%0 := 0
  tofilename%0   := 0

  // One of the arguments must be the from file name.
  WHILE rdarg(chv)>0 DO
  { TEST compstring(chv, "-l")=0 THEN optTokens := TRUE ELSE
    TEST compstring(chv, "-p")=0 THEN optTree   := TRUE ELSE
    TEST compstring(chv, "-c")=0 THEN optCode   := TRUE ELSE
    TEST compstring(chv, "-t")=0 THEN optTrace  := TRUE ELSE
    TEST compstring(chv, "-o")=0 THEN rdarg(tofilename)
    ELSE { res := TRUE; copystring(chv, progfilename) }
  }

  //writef("optTokens    = %n*n", optTokens)
  //writef("optTree      = %n*n", optTree)
  //writef("optCode      = %n*n", optCode)
  //writef("optTrace     = %n*n", optTrace)
  //writef("progfilename = %s*n", progfilename)
  //writef("tofilename   = %s*n", tofilename)

  RESULTIS res
}

LET start() = VALOF
{ LET argv = VEC 50 
  AND cv   = VEC 256/bytesperword
  AND pv   = VEC 256/bytesperword
  AND tv   = VEC 256/bytesperword
  AND stdout = output()
  AND treesize = 0
  AND memsize = 0

  chv          := cv   // Used by args
  progfilename := pv
  tofilename   := tv
  
  errmax   := 2
  errcount := 0
  fin_p, fin_l := level(), fin

  treevec, labv, refv, mem := 0, 0, 0, 0
  progstream, tostream := 0, 0
   
  writef("*nVSPL (11 Oct 2025) BCPL Version*n")

  UNLESS args() DO fatalerr("Bad arguments*n")

  treesize :=   500_000;   // Values increased 29 Sep 2025
  memsize  := 1_000_000;
  labmax   := 8000;

  progstream := findinput(progfilename)  // <filename>

  IF progstream=0 DO fatalerr("Trouble with file %s*n", progfilename)

  selectinput(progstream)
 
  IF tofilename%0 DO                         // -o <filename>
  { tostream := findoutput(tofilename)
    writef("tostream=%n for tofilename %s*n", tostream, tofilename)
    IF tostream=0 DO
      fatalerr("Trouble with code file %s*n", tofilename)
  }

  treevec := getvec(treesize)
  mem     := getvec(memsize)
  memt    := memsize
  labv    := getvec(5000)
  refv    := getvec(5000)
  labmax  := 5000

  UNLESS treevec & mem & labv & refv DO
     fatalerr("Insufficient memory*n")
   
  UNLESS tostream DO tostream := stdout
  selectoutput(tostream)

  { LET tree = 0
    LET b = VEC 64/bytesperword
    chbuf := b
    FOR i = 0 TO 63 DO chbuf%i := 0
    chcount, lineno := 0, 1
    rch()
 
    treep := treevec + treesize

    tree := formtree()              // Perform Syntax Analysis
    IF optTokens GOTO fin

    IF optTree DO { writes("Parse Tree*n")
                    plist(tree, 0, 20)
                    newline()
                  }
  
    IF errcount GOTO fin

    regs  := 10
    codev := 100
    codep := codev
    codet := 100000
    datav := codet
    datap := datav
    datat := memt

    FOR i = 0 TO memt DO mem!i := 0

    trprog(tree)                    // Translate the tree

    stack := datap
    stackt := memt

    IF errcount GOTO fin
    { LET rv = mem+regs
      AND sv = mem+stack
      rv!0 := 0        // result register
      rv!1 := stack    // p pointer
      rv!2 := stack+2  // sp
      rv!3 := codev    // pc (=100)
      rv!4 := maxint   // count

      sv!0, sv!1, sv!2 := 0, 0, 0
 
      { LET ret = interpret(regs, mem)   // Execute the interpreter
        IF ret DO writef("Return code %n*n", ret)
        writef("*nInstructions executed: %n*n", maxint-rv!4)
      }
    }
  }
   
fin:
  IF treevec       DO freevec(treevec)
  IF mem           DO freevec(mem)
  IF labv          DO freevec(labv)
  IF refv          DO freevec(refv)
  IF progstream    DO { selectinput(progstream); endread()  }
  IF tostream      DO { selectoutput(tostream)
                        UNLESS tostream=stdout DO  endwrite() }

  selectoutput(stdout)
  result2 := 0 // No reason given
  RESULTIS errcount=0 -> 0, 20
}

LET lex() BE
{ SWITCHON ch INTO
  { CASE '*p': CASE '*n':
                 lineno := lineno + 1
    CASE '*c': CASE '*t': CASE '*s':
                 rch()
                 LOOP

    CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
    CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
                lexval := 0
                WHILE '0'<=ch<='9' DO
                { lexval := 10*lexval + ch - '0'
                  rch()
                }
                token := Num
                RETURN

    CASE '#':   // Hex constants eg #7FF
              { LET ok = FALSE
                lexval := 0  // Added 9/08/2021
                { rch()
		  IF  '0'<=ch<='9' DO
                  { lexval := (lexval<<4) + ch - '0'
		    ok := TRUE
		    LOOP
                  }
		  IF  'A'<=ch<='F' DO
                  { lexval := (lexval<<4) + ch - 'A' + 10
		    ok := TRUE
		    LOOP
                  }
		  IF  'a'<=ch<='f' DO
                  { lexval := (lexval<<4) + ch - 'a' + 10
		    ok := TRUE
		    LOOP
                  }

                  UNLESS ok DO synerr("Bad hexadecimal constant")
                  token := Num
                  RETURN
		} REPEAT
              }

    CASE 'a':CASE 'b':CASE 'c':CASE 'd':CASE 'e':
    CASE 'f':CASE 'g':CASE 'h':CASE 'i':CASE 'j':
    CASE 'k':CASE 'l':CASE 'm':CASE 'n':CASE 'o':
    CASE 'p':CASE 'q':CASE 'r':CASE 's':CASE 't':
    CASE 'u':CASE 'v':CASE 'w':CASE 'x':CASE 'y':
    CASE 'z':
    CASE 'A':CASE 'B':CASE 'C':CASE 'D':CASE 'E':
    CASE 'F':CASE 'G':CASE 'H':CASE 'I':CASE 'J':
    CASE 'K':CASE 'L':CASE 'M':CASE 'N':CASE 'O':
    CASE 'P':CASE 'Q':CASE 'R':CASE 'S':CASE 'T':
    CASE 'U':CASE 'V':CASE 'W':CASE 'X':CASE 'Y':
    CASE 'Z':
                token := lookupword(rdtag())
                RETURN
 
    CASE '{': token := Lcurly;    BREAK
    CASE '}': token := Rcurly;    BREAK
    CASE '[': token := Lsquare;   BREAK
    CASE ']': token := Rsquare;   BREAK
    CASE '(': token := Lparen;    BREAK
    CASE ')': token := Rparen;    BREAK 
    CASE '!': token := Ind;       BREAK
    CASE '@': token := Lv;        BREAK
    CASE '+': token := Add;       BREAK
    CASE '-': token := Sub;       BREAK
    CASE ',': token := Comma;     BREAK
    CASE ';': token := Semicolon; BREAK
    CASE '&': token := And;       BREAK
    CASE '|': token := Or;        BREAK
    CASE '=': token := Eq;        BREAK
    CASE '**':token := Mul;       BREAK
    CASE '^': token := Xor;       BREAK
 
    CASE '/':   rch()
                IF ch='/' DO
                { rch() REPEATUNTIL ch='*n' | ch=endstreamch
                  LOOP
                }
                token := Div
                RETURN
 
    CASE '~':   rch()
                IF ch='=' DO { token := Ne;  BREAK }
                token := Not
                RETURN
 
    CASE '<':   rch()
                IF ch='=' DO { token := Le;  BREAK }
                IF ch='<' DO { token := Lsh; BREAK }
                token := Lt
                RETURN
 
    CASE '>':   rch()
                IF ch='=' DO { token := Ge;  BREAK }
                IF ch='>' DO { token := Rsh; BREAK }
                token := Gt
                RETURN
 
    CASE ':':   rch()
                IF ch='=' DO { token := Assign;  BREAK }
                synerr("'=' expected after ':'")
                RETURN
 
    CASE '"':
              { LET len = 0
                rch()
 
                UNTIL ch='"' DO
                { IF len=255 DO synerr("Bad string constant")
                  len := len + 1
                  charv%len := rdstrch()
                }
 
                charv%0 := len
                wordnode := newvec(len/bytesperword+2)
                h1!wordnode := String
                FOR i = 0 TO len DO (@h2!wordnode)%i := charv%i
                token := String
                BREAK
              }
 
    CASE '*'':  rch()
                lexval := rdstrch()
                token := Num
                UNLESS ch='*'' DO synerr("Bad character constant")
                BREAK

    DEFAULT:    UNLESS ch=endstreamch DO
                { LET badch = ch
                  ch := '*s'
                  synerr("Illegal character %x2", badch)
                }
                token := Eof
                RETURN
  } REPEAT

  // This point reached by BREAK in the above REPEAT loop
  rch()
}
 
LET lookupword(word) = VALOF
{ LET len, i = word%0, 0
  LET hashval = len
  FOR i = 1 TO len DO hashval := (13*hashval + word%i) & #xFF_FFFF
  hashval := hashval REM nametablesize
  wordnode := nametable!hashval
 
  WHILE wordnode & i<=len TEST (@h3!wordnode)%i=word%i
                          THEN i := i+1
                          ELSE wordnode, i := h2!wordnode, 0
  IF wordnode=0 DO
  { wordnode := newvec(len/bytesperword+3)
    h1!wordnode, h2!wordnode := Name, nametable!hashval
    FOR i = 0 TO len DO (@h3!wordnode)%i := word%i
    nametable!hashval := wordnode
  }
  RESULTIS h1!wordnode
}
 
AND dsw(word, tok) BE { lookupword(word); h1!wordnode := tok  }
 
AND declsyswords() BE
{ dsw("be", Be);             dsw("do", Do);         dsw("else", Else)
  dsw("false", False);       dsw("if", If);         dsw("for", For)
  dsw("let", Let);           dsw("mod", Mod);       dsw("printf", Printf)
  dsw("resultis", Resultis); dsw("return", Return); dsw("static", Static)
  dsw("sys", Sys);           dsw("test", Test);     dsw("to", To)
  dsw("true", True);         dsw("then", Then);     dsw("valof", Valof)
  dsw("vec", Vec);           dsw("unless", Unless); dsw("until", Until)
  dsw("while", While)  
  dsw("manifest", Manifest); dsw("break", Break);   dsw("loop", Loop)
  
  lookupword("start") // Create the name: start
  namestart := wordnode
} 
 
LET rch() BE
{ ch := rdch()
  chcount := chcount+1
  chbuf%(chcount&63) := ch
}
 
AND wrchbuf() BE
{ writes("*n...")
  FOR p = chcount-63 TO chcount DO
  { LET k = chbuf%(p&63)
    IF 0<k<255 DO wrch(k)
  }
  newline()
}
 
AND rdtag() = VALOF
{ LET len = 0
  WHILE 'a'<=ch<='z' | 'A'<=ch<='Z' | '0'<=ch<='9' |  ch='_' DO
  { len := len+1
    IF len>255 DO synerr("Name too long")
    charv%len := ch
    rch()
  }
  charv%0 := len
  RESULTIS charv
}
 
AND rdstrch() = VALOF
{ LET res = ch
  IF ch='*n' | ch='*p' DO
  { lineno := lineno+1
    synerr("Unescaped newline character")
  }
  IF ch='\' DO
  { rch()
    SWITCHON ch INTO
    { DEFAULT:   synerr("Bad string or character constant")
      CASE '\': CASE '*'': CASE '"':  res := ch;        ENDCASE
      CASE 't': CASE 'T':             res := c_tab;     ENDCASE
      CASE 'n': CASE 'N':             res := c_newline; ENDCASE
      CASE 's': CASE 'S':             res := c_space;   ENDCASE
      CASE 'p': CASE 'P':             res := c_newpage; ENDCASE
      CASE 'c': CASE 'C':             res := c_cr;      ENDCASE
    }
  }
  rch()
  RESULTIS res
}

LET newvec(n) = VALOF
{ treep := treep - n - 1;
  IF treep<=treevec DO fatalerr("More workspace needed")
  RESULTIS treep
}
 
AND mk1(a) = VALOF
{ LET p = newvec(0)
  p!0 := a
  RESULTIS p
}
 
AND mk2(a, b) = VALOF
{ LET p = newvec(1)
  p!0, p!1 := a, b
  RESULTIS p
}
 
AND mk3(a, b, c) = VALOF
{ LET p = newvec(2)
  p!0, p!1, p!2 := a, b, c
  RESULTIS p
}
 
AND mk4(a, b, c, d) = VALOF
{ LET p = newvec(3)
  p!0, p!1, p!2, p!3 := a, b, c, d
  RESULTIS p
}
 
AND mk5(a, b, c, d, e) = VALOF
{ LET p = newvec(4)
  p!0, p!1, p!2, p!3, p!4 := a, b, c, d, e
  RESULTIS p
}
 
AND mk6(a, b, c, d, e, f) = VALOF
{ LET p = newvec(5)
  p!0, p!1, p!2, p!3, p!4, p!5 := a, b, c, d, e, f
  RESULTIS p
}
 
AND formtree() = VALOF
{ LET res = 0
  rec_p, rec_l := level(), recover

  charv := newvec(256/bytesperword)     
  nametable := newvec(nametablesize)
  UNLESS charv & nametable DO fatalerr("More workspace needed")
  FOR i = 0 TO nametablesize DO nametable!i := 0
  declsyswords()
  lex()

  IF optTokens DO            // For debugging lex.
  { writef("token = %i3 %s", token, opstr(token))
    IF token=Num    DO writef("       %n",  lexval)
    IF token=Name   DO writef("      %s",   charv)
    IF token=String DO writef("    *"%s*"", charv)
    newline()
    IF token=Eof RESULTIS 0
    lex()
  } REPEAT

recover:
  res := rdprog()
  UNLESS token=Eof DO fatalerr("Incorrect termination")
  RESULTIS res
}
 
AND fatalerr(mess, a) BE
{ writef("*nFatal error:  ")
  writef(mess, a)
  writes("*nCompilation aborted*n")
  errcount := errcount+1
  longjump(fin_p, fin_l)
}

AND synerr(mess, a) BE
{ writef("*nError near line %n:  ", lineno)
  writef(mess, a)
  wrchbuf()
  errcount := errcount+1
  IF errcount >= errmax DO fatalerr("Too many errors")

  // Skip the rest of the input line 
  UNTIL ch='*n' | ch=endstreamch DO rch()
  lex()

  longjump(rec_p, rec_l)
}

LET checkfor(tok, mess) BE
{ UNLESS token=tok DO synerr(mess)
  lex()
}
 
LET rdprog() = VALOF
{ // Return a declation list dlist
  // prog  =   0
  // or    =>  [Decl, [Manifest, items,   ln], prog] 
  // or    =>  [Decl, [Static,   items,   ln], prog] 
  // or    =>  [Decl, [Rtdef, N, Args, C, ln], prog] 
  // or    =>  [Decl, [Fndef, N, Args, E, ln], prog]

  // items = 0
  // or    -> N                  Just a name node
  // or    -> [Initval, N, E]    Only in manifest declarations
  // or    -> [Statvec, N, E]    Only in static   declarations
  // or    -> [Comma, items, items]
  LET ln = lineno
  SWITCHON token INTO
  { DEFAULT:  synerr("Bad outer level declaration*n")

    CASE Eof: RESULTIS 0

    CASE Manifest: // Added 6/10/2025
    CASE Static:
               { LET op = token
	         LET d = ?
                 lex()
                 d := mk3(op, rstatlist(op), ln)
                 RESULTIS  mk3(Decl, d, rdprog())
               }

    CASE Let:
         { LET n, args = 0, 0
           lex()
           n := rname()
           checkfor(Lparen, "'(' missing")
           IF token=Name DO args := rnamelist()
           checkfor(Rparen, "')' missing")
 
           IF token=Be DO
           { LET d = mk5(Rtdef, n, args, rncom(), ln)
             RESULTIS mk3(Decl, d, rdprog())
           }
 
           IF token=Eq DO
           { LET d = mk5(Fndef, n, args, rnexp(0), ln)
             RESULTIS mk3(Decl, d, rdprog())
           }
 
           synerr("Bad procedure heading")
        }
  }
} REPEAT

LET rdblockbody() = VALOF
{ LET res, orec_p, orec_l = 0, rec_p, rec_l
  LET op = token
  rec_p, rec_l := level(), recover

recover:
  SWITCHON op INTO
  { DEFAULT:    res := rdseq()
                ENDCASE

    CASE Let:
    CASE Vec: { LET n, e, ln = 0, 0, lineno
                lex()
                n := rname()
                TEST op=Let
                THEN { checkfor(Eq, "Missing '='")
                       e := rexp(0)
                     }
                ELSE { checkfor(Lsquare, "Missing '['")
                       e := rexp(0)
                       UNLESS h1!e=Num DO synerr("Bad 'vec' declaration")
                       checkfor(Rsquare, "Missing ']'")
                     }
                checkfor(Semicolon, "';' expected")
                res := mk5(op, n, e, rdblockbody(), ln)
                ENDCASE
              }
  }
 
  rec_p, rec_l := orec_p, orec_l
  RESULTIS res
}
 
AND rdseq() = VALOF
{ LET a = 0
  a := rcom()
  IF token=Rcurly | token=Eof RESULTIS a
  checkfor(Semicolon, "';' expected")
  RESULTIS mk3(Seq, a, rdseq())
}

AND rnamelist() = VALOF
{ LET a = rname()
  UNLESS token=Comma RESULTIS a
  lex()
  RESULTIS mk3(Comma, a, rnamelist())
}

AND rexplist() = VALOF
{ LET a = rexp(0)
  UNLESS token=Comma RESULTIS a
  lex()
  RESULTIS mk3(Comma, a, rexplist())
}
 
AND rstatlist(op) = VALOF
{ // op is Manifest or Static
  // Return items
  // items  = 0
  // or    -> N                  Just a name node
  // or    -> [Initval, N, E]
  // or    -> [Statvec, N, E]    Only in static declarations
  // or    -> [Comma, items, items]
  
  LET a = rname()
  LET b = 0
  TEST token=Eq
  THEN { LET b = rnexp(0)        // Value given N=E
         a := mk3(Initval, a, b)
       }
  ELSE IF token=Lsquare &  // Vector declaration N[E]
          op=Static     DO // Only allowed in static declarations
       { LET b = rnexp(0)
         checkfor(Rsquare, "']' expected")
         a := mk3(Statvec, a, b)
       }
  UNLESS token=Comma RESULTIS a
  lex()
  RESULTIS mk3(Comma, a, rstatlist(op))
}

AND rname() = VALOF
{ LET a = wordnode
  checkfor(Name, "Name expected")
  RESULTIS a
}
 
LET rbexp() = VALOF
{ LET a, op, ln = 0, token, lineno
 
  SWITCHON op INTO
 
  { DEFAULT: synerr("Error in expression")

    CASE True:
    CASE False:
    CASE Name:
    CASE String: a := wordnode
                 lex()
                 RESULTIS a
 
    CASE Num:    a := mk2(Num, lexval)
                 lex()
                 RESULTIS a
 
    CASE Printf:
    CASE Sys: lex()
              checkfor(Lparen, "'(' missing")
              a := 0
              UNLESS token=Rparen DO a := rexplist()
              checkfor(Rparen, "')' missing")
              RESULTIS mk3(op, a, ln)

    CASE Break:
    CASE Loop:lex()
              RESULTIS mk2(op, ln)

    CASE Lparen: a := rnexp(0)
                 checkfor(Rparen, "')' missing")
                 RESULTIS a
 
    CASE Valof:  RESULTIS mk2(Valof, rncom())
 
    CASE Ind:
    CASE Lv:     RESULTIS mk2(op, rnexp(7))
 
    CASE Add:    RESULTIS rnexp(5)
 
    CASE Sub:    a := rnexp(5)
                 TEST h1!a=Num THEN h2!a := - h2!a
                               ELSE a := mk2(Neg, a)
                 RESULTIS a
 
    CASE Not:    RESULTIS mk2(Not, rnexp(3))
   }
}
 
AND rnexp(n) = VALOF { lex(); RESULTIS rexp(n) }
 
AND rexp(n) = VALOF
{ LET a, b, p = rbexp(), 0, 0

  { LET op, ln = token, lineno
    SWITCHON op INTO
 
    { DEFAULT:      BREAK
 
      CASE Lparen:  lex()
                    b := 0
                    UNLESS token=Rparen DO b := rexplist()
                    checkfor(Rparen, "')' missing")
                    a := mk4(Fnap, a, b, ln)
                    LOOP
 
      CASE Lsquare: b := rnexp(0)
                    checkfor(Rsquare, "']' missing")
                    a := mk3(Vecap, a, b)
                    LOOP
 
      CASE Mul:CASE Div:CASE Mod:
                    p := 7;              ENDCASE
      CASE Add:CASE Sub:
                    p := 6;              ENDCASE
      CASE Lsh:CASE Rsh:
                    p := 5;              ENDCASE
      CASE Eq:CASE Le:CASE Lt:CASE Ne:CASE Ge:CASE Gt:
                    p := 4;              ENDCASE
      CASE And:     p := 3;              ENDCASE
      CASE Or:      p := 2;              ENDCASE
      CASE Xor:     p := 1;              ENDCASE
    }
      
    IF n>=p RESULTIS a
    a := mk3(op, a, rnexp(p))
  } REPEAT

  RESULTIS a
}
  
LET rcom() = VALOF
{ LET n, a, b, op, ln = 0, 0, 0, token, lineno
 
  SWITCHON token INTO
  { DEFAULT:     synerr("Command expected")
 
    CASE Name:CASE Num:CASE Lparen:CASE Ind:
    CASE Sys:CASE Printf:
    CASE Break:CASE Loop:
    // All tokens that can start an expression.
                 a := rexp(0)
 
                 IF token=Assign DO
                 { // a is the LHS of an assignment and so must have
		   // be one of the following forms.
		   UNLESS h1!a=Name | h1!a=Vecap | h1!a=Ind DO
                     synerr("Bad assigment statement")
                   RESULTIS mk4(Assign, a, rnexp(0), ln)
                 }
 
                 IF h1!a=Fnap DO
                 { // If a is syntactically a function call turn it into
		   // a routine call.
		   h1!a := Rtap
                   RESULTIS a
                 }

                 // If the command was not an assignment or a routine call
		 // it can only be a sys or printf command.
                 UNLESS h1!a=Sys   | h1!a=Printf |
		        h1!a=Break | h1!a=Loop   DO
                   synerr("Error in command")
                 RESULTIS a
 
    CASE Resultis:
                 RESULTIS mk3(op, rnexp(0), ln)
 
    CASE If:    CASE Unless:
    CASE While: CASE Until:
                 a := rnexp(0)
                 checkfor(Do, "'do' missing")
                 RESULTIS mk4(op, a, rcom(), ln)
 
    CASE Test:   a := rnexp(0)
                 checkfor(Then, "'then' missing")
                 b := rcom()
                 checkfor(Else, "'else' missing")
                 RESULTIS mk5(Test, a, b, rcom(), ln)
 
    CASE For:    lex()
                 n := rname()
                 checkfor(Eq, "'=' expected")
                 a := rexp(0)
                 checkfor(To, "'to' expected")
                 b := rexp(0)
                 checkfor(Do, "'do' missing")
                 RESULTIS mk6(For, n, a, b, rcom(), ln)

    CASE Return: lex()
                 RESULTIS mk2(op, ln)
 
    CASE Lcurly: lex()
                 a := rdblockbody()
                 checkfor(Rcurly, "'}' expected")
                 RESULTIS a
   }
}

AND rncom() = VALOF { lex(); RESULTIS rcom() }

LET opstr(op) = VALOF SWITCHON op INTO
{ DEFAULT:       RESULTIS "Unknown"

  CASE Assign:   RESULTIS "Assign";    CASE Add:     RESULTIS "Add"
  CASE And:      RESULTIS "And";       CASE Be:      RESULTIS "Be"
  CASE Comma:    RESULTIS "Comma";     CASE Data:    RESULTIS "Data"
  CASE Decl:     RESULTIS "Decl";      CASE Div:     RESULTIS "Div"
  CASE Do:       RESULTIS "Do";        CASE Else:    RESULTIS "Else"
  CASE Entry:    RESULTIS "Entry";     CASE Eq:      RESULTIS "Eq"
  CASE False:    RESULTIS "False";     CASE Fnap:    RESULTIS "Fnap"
  CASE For:      RESULTIS "For";       CASE Fndef:   RESULTIS "Fndef"
  CASE Fnrn:     RESULTIS "Fnrn";      CASE Ge:      RESULTIS "Ge"
  CASE Gt:       RESULTIS "Gt";        CASE Halt:    RESULTIS "Halt"
  CASE If:       RESULTIS "If";        CASE Ind:     RESULTIS "Ind"
  CASE Jf:       RESULTIS "Jf";        CASE Jt:      RESULTIS "Jt"
  CASE Jump:     RESULTIS "Jump";      CASE Lab:     RESULTIS "Lab"
  CASE Laddr:    RESULTIS "Laddr";     CASE Lcurly:  RESULTIS "Lcurly"
  CASE Le:       RESULTIS "Le";        CASE Let:     RESULTIS "Let"
  CASE Ll:       RESULTIS "Ll";        CASE Llp:     RESULTIS "Llp"
  CASE Ln:       RESULTIS "Ln";        CASE Lp:      RESULTIS "Lp"
  CASE Lparen:   RESULTIS "Lparen";    CASE Lres:    RESULTIS "Lres"
  CASE Lsh:      RESULTIS "Lsh";       CASE Lsquare: RESULTIS "Lsquare"
  CASE Lt:       RESULTIS "Lt";        CASE Lv:      RESULTIS "Lv"
  CASE Mod:      RESULTIS "Mod";       CASE Mul:     RESULTIS "Mul"
  CASE Name:     RESULTIS "Name";      CASE Ne:      RESULTIS "Ne"
  CASE Neg:      RESULTIS "Neg";       CASE Not:     RESULTIS "Not"
  CASE Num:      RESULTIS "Num";       CASE Or:      RESULTIS "Or"       
  CASE Printf:   RESULTIS "Printf";    CASE Rcurly:  RESULTIS "Rcurly"
  CASE Resultis: RESULTIS "Resultis";  CASE Return:  RESULTIS "Return"
  CASE Rparen:   RESULTIS "Rparen";    CASE Rsh:     RESULTIS "Rsh"
  CASE Rsquare:  RESULTIS "Rquare";    CASE Rtap:    RESULTIS "Rtap"
  CASE Rtdef:    RESULTIS "Rtdef";     CASE Rtrn:    RESULTIS "Rtrn"
  CASE Semicolon:RESULTIS "Semicolon"; CASE Seq:     RESULTIS "Seq"
  CASE Sl:       RESULTIS "Sl";        CASE Sp:      RESULTIS "Sp"
  CASE Stack:    RESULTIS "Stack";     CASE Static:  RESULTIS "Static"
  CASE Statvec:  RESULTIS "Statvec";   CASE String:  RESULTIS "String"
  CASE Stind:    RESULTIS "Stind";     CASE Sub:     RESULTIS "Sub"
  CASE Sys:      RESULTIS "Sys";       CASE Test:    RESULTIS "Test"
  CASE Then:     RESULTIS "Then";      CASE To:      RESULTIS "To"
  CASE True:     RESULTIS "True";      CASE Valof:   RESULTIS "Valof"
  CASE Vecap:    RESULTIS "Vecap";     CASE Vec:     RESULTIS "Vec"
  CASE Unless:   RESULTIS "Unless";    CASE Until:   RESULTIS "Until"
  CASE While:    RESULTIS "While";     CASE Xor:     RESULTIS "Xor"

  CASE Manifest: RESULTIS "Manifest"
  CASE Initval:  RESULTIS "Initval"
  CASE Break:    RESULTIS "Break"
  CASE Loop:     RESULTIS "Loop"
  CASE Eof:      RESULTIS "Eof"
}

LET plist(x, n, d) BE
{ LET s, size, ln = 0, 0, 0
  LET v = TABLE 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

  IF x=0 DO { writes("Nil"); RETURN  }
 
  SWITCHON h1!x INTO
  { DEFAULT:
         size     := 1;        ENDCASE

    CASE Num:     writen(h2!x);         RETURN
    CASE Name:    writes(x+2);          RETURN
    CASE String:  writef("*"%s*"",x+1); RETURN

    CASE For:
         size, ln := 5, h6!x; ENDCASE

    CASE Fndef: CASE Rtdef:
         size, ln := 4, h5!x; ENDCASE

    CASE Let: CASE Vec: CASE Test:
         size, ln := 4, h5!x; ENDCASE

    CASE Vecap: CASE Mul: CASE Div: CASE Mod: CASE Add: CASE Sub:
    CASE Eq: CASE Ne: CASE Lt: CASE Gt: CASE Le: CASE Ge:
    CASE Lsh: CASE Rsh: CASE And: CASE Or: CASE Xor:
    CASE Comma: CASE Seq: CASE Decl: CASE Statvec: CASE Initval:
         size     := 3;       ENDCASE

    CASE Assign: CASE Rtap: CASE Fnap:
    CASE If: CASE Unless: CASE While: CASE Until:
         size, ln := 3, h4!x; ENDCASE

    CASE Valof: CASE Lv: CASE Ind: CASE Neg: CASE Not:
         size     := 2;       ENDCASE

    CASE Printf: CASE Sys: CASE Static: CASE Manifest:
    CASE Resultis:
         size, ln := 2, h3!x; ENDCASE

    CASE Break: CASE Loop:
         size, ln := 1, h2!x; ENDCASE

    CASE True: CASE False:
         size     := 1;       ENDCASE
  }
 
  IF n=d DO { writes("Etc"); RETURN }
  writef("%s", opstr(h1!x))
  IF ln DO writef("  -- line %n", ln)
  FOR i = 2 TO size DO { newline()
                         FOR j=0 TO n-1 DO writes( v!j )
                         writes("**-")
                         v!n := i=size->"  ","! "
                         plist(h1!(x+i-1), n+1, d)
                       }
}


AND trnerr(mess, a) BE
{ writes("Error")
  IF procname DO writef(" in %s", @h3!procname)
  IF comline DO writef(" near line %n", comline)
  writes(":   ")
  writef(mess, a)
  newline()
  errcount := errcount + 1
  IF errcount >= errmax DO fatalerr("*nCompilation aborted*n")
}

AND trprog(x) BE
{ dvec, dvect := treevec, treep
  h1!dvec, h2!dvec, h3!dvec := 0, 0, 0
  dvece := dvec+3
  // Clear the h2 field of every name in the name table.
  FOR i = 0 TO nametablesize-1 DO
  { LET name = nametable!i
    UNTIL name=0 DO
    { LET next = h2!name
      h2!name := 0 // Mark undeclared
      name := next
    }
  }

  FOR i = 0 TO labmax DO
    labv!i, refv!i := -1, 0 // No labels are currently set or
                            // refernced.

  resultlab := -2   // resultis not in a valof block.
  looplab := -2     // loop not a repetitive command.
  resultlab := -2   // break not in a repetitive command.
  comline, procname, labnumber := 1, 0, 1
  ssp := 2

  // Place the three initial instructions starting at codep (=100)
  outfl(Laddr, 1); ssp := ssp+1  // 1 = lab number of start
  outfn(Fnap, 3);  ssp := ssp-1
  outf(Halt)   // Leave the vspl interpreter returning
               // res as the result.
 
  // Declare all static, manifest and function names.
  declstatnames(x)
  checkdistinct(dvec+3) // Check that they are all distinct.
  WHILE x DO { trdecl(h2!x); x:=h3!x } // Compile all the functions
  resolvelabels()
  writef("Program size: %n   Data size: %n*n", codep-codev, datap-datav)
}

LET trnext(next) BE { // Compile Rtrn, Jump or nothing depending
                      // on the value of next.
		      IF next<0 DO outf(Rtrn)
                      IF next>0 DO outfl(Jump, next)
                    }
 
LET trcom(x, next) BE
// x       is the command to translate
// next<0  compile x followed by Rtrn
// next>0  compile x followed by Jump next
// next=0  compile x only
{ LET op = h1!x

  SWITCHON op INTO
  { DEFAULT: trnerr("Compiler error in Trans")
             RETURN
 
    CASE Let:
           { // Compile: let N = E; C
	     LET e, s = dvece, ssp
             comline := h5!x
             addname(h2!x, Local, ssp+1)
             load(h3!x)
             trcom(h4!x, next)
             undeclare(e)
             outfn(Stack, s)
             ssp := s
             RETURN
           }
  
    CASE Vec:
           { // Compile: let N[E]; C
	     LET e, s = dvece, ssp
             comline := h5!x
             addname(h2!x, Vec, ssp+1)
             ssp := ssp + h2!(h3!x)
             outfn(Stack, ssp)
             trcom(h4!x, next)
             undeclare(e)
             outfn(Stack, s)
             ssp := s
             RETURN
           }
  
    CASE Assign:
             comline := h4!x
             assign(h2!x, h3!x)
             trnext(next)
             RETURN

// Note that if a function is called it sets res but this value is ignored
    CASE Rtap:
           { LET s = ssp
             comline := h4!x
             ssp := ssp+3
             outfn(Stack, ssp)
             loadlist(h3!x)      // Load the arguments
             load(h2!x)          // Load the function entry address
             outfn(Rtap, s+1)    // s+1 is the p pointer increment
             ssp := s
             trnext(next)
             RETURN
           }
 
    CASE Printf:  // Printf does not set res
    CASE Sys:     // Sys may set res, but this is ignore if called as a command
           { LET s = ssp
             LET op = h1!x
             comline := h3!x
             loadlist(h2!x)
             outfn(op, s+1)
             ssp := s
             trnext(next)
             RETURN
           }
 
    CASE Unless:
    CASE If: comline := h4!x
             TEST next>0
             THEN { jumpcond(h2!x, op=Unless, next)
                    trcom(h3!x, next)
                  }
             ELSE { LET l = nextlab()
                    jumpcond(h2!x, op=Unless, l)
                    trcom(h3!x, next)
                    outlab(l)
                    trnext(next)
                  }
             RETURN
 
    CASE Test:
           { LET l, m = nextlab(), 0
             comline := h5!x
             jumpcond(h2!x, FALSE, l)
         
             TEST next=0
             THEN { m := nextlab(); trcom(h3!x, m) }
             ELSE trcom(h3!x, next)
                     
             outlab(l)
             trcom(h4!x, next)
             UNLESS m=0 DO outlab(m)
             RETURN
           }
 
    CASE Return:
             comline := h2!x
             outf(Rtrn)
             RETURN
 
    CASE Loop:
             comline := h2!x
             IF looplab DO looplab := nextlab()
	     UNLESS looplab>0 DO
             { trnerr("loop command out of context")
               RETURN
             }
	     outfl(Jump, looplab)
             RETURN
 
    CASE Break:
             IF breaklab=-1 DO breaklab := nextlab()
	     UNLESS breaklab>0 DO
             { trnerr("break command out of context")
               RETURN
             }
	     outfl(Jump, breaklab)
             RETURN
 
    CASE Resultis:
             comline := h3!x
             IF resultlab=-1 DO { fnbody(h2!x); RETURN }
             UNLESS resultlab>0 DO
             { trnerr("RESULTIS out of context")
               RETURN
             }
             load(h2!x)
             outfl(Resultis, resultlab)
             ssp := ssp - 1
             RETURN
 
    CASE Until:
    CASE While:
           { // If next>0, The while or until command is followed by
             //            code to jump to label next.
             // If next=-1 The while or until command is followed by
             //            code to return from the current function or
	     //            routine.
             // If next=0  Compile nothing after the while or until
	     //            command.
	     LET prevlooplab, prevbreaklab = looplab, breaklab
             LET bodylab = nextlab() // For start of the body
             looplab  := nextlab()
             breaklab := nextlab()
	     
             comline := h4!x
             jumpcond(h2!x, op=Until, breaklab)
             outlab(bodylab)
             trcom(h3!x, 0)

             comline := h4!x
	     outlab(looplab)
	     
             jumpcond(h2!x, op=While, bodylab)

             IF next=0 & breaklab>0 DO outlab(breaklab)

             trnext(next) // Possibly compile a jump or a return from
                          // a function or routine.

	     looplab, breaklab := prevlooplab, prevbreaklab
             RETURN
           }
 
    CASE For:
           { LET e, s = dvece, ssp
             LET l, m = nextlab(), nextlab()
	     LET prevlooplab, prevbreaklab = looplab, breaklab
	     breaklab := -1 // Now in a looping command
	     looplab := m
             comline := h5!x
             addname(h2!x, Local, ssp+1)
             load(h3!x)  // The control variable at s+1
             load(h4!x)  // The end limit        at s+2

             outfl(Jump, m)               // Jump to test

             outlab(l)                    // Start of body
             trcom(h5!x, 0)

             IF looplab>0 DO outlab(looplab)
	     
             outfn(Lp, s+1); ssp := ssp+1 // Inc control variable
             outfn(Ln, 1);   ssp := ssp+1
             outf(Add);      ssp := ssp-1
             outfn(Sp, s+1); ssp := ssp-1

             outlab(m)
             outfn(Lp, s+1); ssp := ssp+1 // Compare with limit
             outfn(Lp, s+2); ssp := ssp+1
             outf(Le);       ssp := ssp-1
             outfl(Jt, l);   ssp := ssp-1

             undeclare(e)
             outfn(Stack, s)
             ssp := s
	     IF breaklab>0 DO outlab(breaklab)
             trnext(next)
	     looplab, breaklab := prevlooplab, prevbreaklab
             RETURN
           }
  
    CASE Seq:
            trcom(h2!x, 0)
            x := h3!x
  }
} REPEAT

AND declstatnames(x) BE WHILE x DO
{ // x     = prog

  // prog  =   0
  // or    =>  [Decl, [Manifest, items,   ln], prog] 
  // or    =>  [Decl, [Static,   items,   ln], prog] 
  // or    =>  [Decl, [Rtdef, N, Args, C, ln], prog] 
  // or    =>  [Decl, [Fndef, N, Args, E, ln], prog]

  // items = 0
  // or    -> N                  Just a name node
  // or    -> [Initval, N, E]    Only in manifest declarations
  // or    -> [Statvec, N, E]    Only in static   declarations
  // or    -> [Comma, items, items]

  LET d  = h2!x  // d -> a manifest, static,
                 //      Rtdef or Fndef node.
  LET op = h1!d  // = Manifest, Static, Rtdef or Fndef
  
  prevvalue := -1 // Used when declaring manifest constants
  
  SWITCHON op INTO
  { DEFAULT:
      trnerr("Compiler error in declstatnames")
      RETURN

    CASE Static:
    { LET p, np = h2!d, 0
      // p is the collection of static
      // declaration items
      WHILE p SWITCHON h1!p INTO
      { DEFAULT:
          trnerr("Bad STATIC declaration")
          RETURN

        CASE Comma:
	  np := h3!p
          p  := h2!p
          LOOP

        CASE Name:
	{ LET lab = nextlab()
	  // Declare a static variable with value zero.
          outvar(lab, 0)
          addname(p, Var, lab) 
          p := np
          np := 0
          LOOP
        }

        CASE Initval:
        { LET lab = nextlab()
          LET val = evalconst(h3!p)
	  // Declare a static variable with specified value.
          outvar(lab, val)
          addname(h2!p, Var, lab)
          p := np
          np := 0
          LOOP
        }

        CASE Statvec:
        { LET lab = nextlab()
          LET upb = evalconst(h3!p)
	  // Declare a static vector with specified size
          outstatvec(lab, upb)
          addname(h2!p, Addr, lab)
          p := np
          np := 0
          LOOP
        }
      }
      ENDCASE      
    } // End of Manifest and Static

    CASE Manifest:
    { LET p, np = h2!d, 0
      // p is the collection of manifest
      // declaration items
      WHILE p SWITCHON h1!p INTO
      { DEFAULT:
          trnerr("Bad STATIC declaration")
          RETURN

        CASE Comma:
	  np := h3!p
          p  := h2!p
          LOOP

        CASE Name: // Just a manifest name, so give it
	           // the next possible value.
	{ prevvalue := prevvalue + 1
          addname(p, Const, prevvalue)
          p := np
          np := 0
          LOOP
        }

        CASE Initval:
        { prevvalue := evalconst(h3!p)
          addname(h2!p, Const, prevvalue)
          p := np
          np := 0
          LOOP
        }
      }
      ENDCASE      
    } // End of Manifest and Static

    CASE Fndef:
    CASE Rtdef:
    { LET name = h2!d
      LET lab = name=namestart -> 1, nextlab()
      addname(name, Addr, lab)
      ENDCASE
    }
  }

  x := h3!x // Deal with the next prog item
}

AND decldyn(x) BE UNLESS x=0 DO
 
{ IF h1!x=Name  DO { ssp := ssp+1
                     addname(x, Local, ssp)
                     RETURN
                   }
 
  IF h1!x=Comma DO { ssp := ssp+1
                     addname(h2!x, Local, ssp)
                     decldyn(h3!x)
                     RETURN
                   }
 
   trnerr("Compiler error in Decldyn")
}
 
AND checkdistinct(p) BE
{ LET lim = dvece - 3
  FOR q = p TO lim-3 BY 3 DO
  { LET n = h1!q
    FOR c = q+3 TO lim BY 3 DO
        IF h1!c=n DO trnerr("Name %s defined twice", @h3!n)
  }
}
 
AND addname(name, k, a) BE
{ LET p = dvece + 3
  IF p>dvect DO { trnerr("More workspace needed"); RETURN }
  h1!dvece, h2!dvece, h3!dvece := name, k, a
  h2!name := dvece // Remember the declaration
  dvece := p
}
 
AND undeclare(e) BE 
{ FOR t = e TO dvece-3 BY 3 DO
  { LET name = h1!t
    h2!name := 0   // Forget its declaration
  }
  dvece := e
}

AND cellwithname(n) = VALOF
{ LET t = h2!n
  UNLESS t=0 RESULTIS t  // It has been looked up before
  t := dvece
  t := t - 3 REPEATUNTIL h1!t=n | h1!t=0
  h2!n := t  // Associate the name with declaration item
  RESULTIS t
}
 
AND trdecl(x) BE SWITCHON h1!x INTO
{  CASE Static:  // Static declarations are compiled in declstatnames
               RETURN

   CASE Fndef:
   CASE Rtdef:
             { LET e = dvece
               LET name = h2!x
               LET t = cellwithname(name)
               LET strlab = nextlab()

               resultlab := -2
               procname := name

               outstring(strlab, @h3!procname)
               outentry(h3!t, strlab)
               ssp := 2
               decldyn(h3!x)  // Declare the formal paramenters
               checkdistinct(e)
               outfn(Stack, ssp)
               TEST h1!x=Rtdef THEN trcom(h4!x, -1)
                               ELSE fnbody(h4!x)
 
               undeclare(e)
               procname := 0
             }
 
  DEFAULT:   RETURN
}
 
LET jumpcond(x, b, l) BE
{ LET sw = b

  SWITCHON h1!x INTO
  { CASE False:  b := NOT b
    CASE True:   IF b DO outfl(Jump, l)
                 RETURN
 
    CASE Not:    jumpcond(h2!x, NOT b, l)
                 RETURN
 
    CASE And: sw := NOT sw
    CASE Or:  TEST sw THEN { jumpcond(h2!x, b, l)
                             jumpcond(h3!x, b, l)
                             RETURN
                           }
 
                       ELSE { LET m = nextlab()
                              jumpcond(h2!x, NOT b, m)
                              jumpcond(h3!x, b, l)
                              outlab(m)
                              RETURN
                            }
 
    DEFAULT:     load(x)
                 outfl(b -> Jt, Jf, l)
                 ssp := ssp-1
                 RETURN
  }
}

LET evalconst(x) = VALOF
{ LET op = h1!x
  LET a, b = h2!x, h3!x

  SWITCHON op INTO
  { DEFAULT:      trnerr("Bad manifest expression, op=%s", opstr(op))
                  RESULTIS 0
 
    CASE Mul:     RESULTIS evalconst(a)  *  evalconst(b)
    CASE Div:     RESULTIS evalconst(a)  /  evalconst(b)
    CASE Mod:     RESULTIS evalconst(a) MOD evalconst(b)
    CASE Add:     RESULTIS evalconst(a)  +  evalconst(b)
    CASE Sub:     RESULTIS evalconst(a)  -  evalconst(b)
    CASE Eq:      RESULTIS evalconst(a)  =  evalconst(b)
    CASE Ne:      RESULTIS evalconst(a)  ~= evalconst(b)
    CASE Lt:      RESULTIS evalconst(a)  <  evalconst(b)
    CASE Gt:      RESULTIS evalconst(a)  >  evalconst(b)
    CASE Le:      RESULTIS evalconst(a) <=  evalconst(b)
    CASE Ge:      RESULTIS evalconst(a) >=  evalconst(b)
    CASE Lsh:     RESULTIS evalconst(a) <<  evalconst(b)
    CASE Rsh:     RESULTIS evalconst(a) >>  evalconst(b)
    CASE And:     RESULTIS evalconst(a)  &  evalconst(b)
    CASE Or:      RESULTIS evalconst(a)  |  evalconst(b)
    CASE Xor:     RESULTIS evalconst(a) XOR evalconst(b)
     
    CASE Neg:     RESULTIS - evalconst(a)
    CASE Not:     RESULTIS ~ evalconst(a)

    CASE Num:     RESULTIS  a
    CASE True:    RESULTIS -1
    CASE False:   RESULTIS  0
 
    CASE Name:  { LET p = cellwithname(x)
                  // p = 0 or
		  //   -> [Name, link, k, n]
                  UNLESS p & h3!p=Const DO
		    trnerr("Bad name in manifest expression")
                  RESULTIS h4!p
		}
  }
}

LET load(x) BE
{ LET op = h1!x

  SWITCHON op INTO
  { DEFAULT:      trnerr("Compiler error in Load, op=%n", op)
                  outfl(Ln, 0)
                  ssp := ssp+1
                  RETURN
 
    CASE Vecap:
    CASE Mul: CASE Div: CASE Mod: CASE Add: CASE Sub:
    CASE Eq: CASE Ne: CASE Lt: CASE Gt: CASE Le: CASE Ge:
    CASE Lsh: CASE Rsh: CASE And: CASE Or: CASE Xor:
                  load(h2!x); load(h3!x); outf(op)
                  ssp := ssp-1
                  RETURN
 
    CASE Ind: CASE Neg: CASE Not:
                  load(h2!x)
                  outf(op)
                  RETURN

    CASE Lv:      loadlv(h2!x)
                  RETURN
 
    CASE Num:     outfn(Ln, h2!x); ssp := ssp+1; RETURN
    CASE True:    outfn(Ln, -1);   ssp := ssp+1; RETURN
    CASE False:   outfn(Ln, 0);    ssp := ssp+1; RETURN
 
    CASE String:  
                { LET strlab = nextlab()
                  outstring(strlab, @h2!x)
                  outfl(Laddr, strlab)
                  ssp := ssp+1
                  RETURN
                }
 
    CASE Name:    transname(x, Lp, Ll, Llp, Laddr, Ln)
                  ssp := ssp+1
                  RETURN
 
    CASE Valof: { LET rl = resultlab
                  resultlab := nextlab()
                  trcom(h2!x, 0)
		  // Note that resultis set res then jumps to resultlab
                  outlab(resultlab)
                  outfn(Stack, ssp)
                  outf(Lres); ssp := ssp+1
                  resultlab := rl
                  RETURN
                }

// Note that if a routine is called res is not set so the result is undefined.
    CASE Fnap:  { LET s = ssp
                  ssp := ssp+3      // Leave space for old p, ret addr, entry addr
                  outfn(Stack, ssp)
                  loadlist(h3!x)    // Load the arguments
                  load(h2!x)        // Load the entry address
                  outfn(Fnap, s+1)
                  outf(Lres); ssp := s+1 // Cause res to be loaded onto the stack
                  RETURN
                }

    CASE Printf: // Printf does note set res, so the value is undefined.
    CASE Sys:    // If Sys sets res the result to be defined.
           { LET s = ssp
             LET op = h1!x
             comline := h3!x
             loadlist(h2!x)
             outfn(op, s+1)
             ssp := s
             outf(Lres)
             ssp := ssp+1
             RETURN
           }
  }
}

AND loadlv(x) BE SWITCHON h1!x INTO
{ DEFAULT:    trnerr("Bad operand to @")
              outf(Lres); ssp := ssp+1
              RETURN

  CASE Name:  transname(x, Llp, Laddr, 0, 0, 0); ssp := ssp+1
              RETURN

  CASE Ind:   load(h2!x)
              RETURN

  CASE Vecap: load(h2!x); load(h3!x); outf(Add); ssp := ssp-1
              RETURN
}

AND fnbody(x) BE SWITCHON h1!x INTO
{ DEFAULT:      load(x)
                outf(Fnrn)
                ssp := ssp-1
                RETURN
                   
  CASE Valof: { LET e, rl = dvece, resultlab
                resultlab := -1
                trcom(h2!x, -1)
                resultlab := rl
                undeclare(e)
                RETURN
              }
}
 
AND loadlist(x) BE UNLESS x=0 TEST h1!x=Comma
                              THEN { loadlist(h2!x); loadlist(h3!x) }
                              ELSE load(x)

AND assign(x, y) BE SWITCHON h1!x INTO
{ DEFAULT:    trnerr("Bad assignment")
              RETURN
  CASE Name:  load(y)
              transname(x, Sp, Sl, 0, 0, 0)
              ssp := ssp-1
              RETURN
  CASE Vecap: load(y)
              load(h2!x); load(h3!x); outf(Add); ssp := ssp-1
              outf(Stind); ssp := ssp-2
              RETURN
  CASE Ind:   load(y)
              load(h2!x)
              outf(Stind); ssp := ssp-2
              RETURN
}
 
AND transname(x, p, l, v, a, m) BE
{ LET c = cellwithname(x)
  LET k, n = h2!c, h3!c
  LET name = @h3!x
 
  SWITCHON k INTO
  { DEFAULT:      trnerr("Name '%s' not declared", name)
   
    CASE Local:   outfn(p, n); RETURN
 
    CASE Var:     outfl(l, n); RETURN
 
    CASE Vec:     IF v=0 DO
                  { trnerr("Misuse of local vector '%s'", name)
                    v := p
                  }
                  outfn(v, n)
                  RETURN

    CASE Addr:    IF a=0 DO
                  { trnerr("Misuse of entry name '%s'", name)
                    a := l
                  }
                  outfl(a, n)
                  RETURN

    CASE Const:   IF m=0 DO
                  { trnerr("Misuse of manifest name '%s'", name)
                    c := l
                  }
                  outfn(m, n)
                  RETURN
  }
}
 
AND wrf(form, a, b, c, d) BE IF optCode DO writef(form, a, b, c, d)

AND outf(op) BE
{ wrf("%i5: %s*n", codep, opstr(op))
  putc(op)
}

AND outfn(op, a) BE
{ wrf("%i5: %s %n*n", codep, opstr(op), a)
  putc(op); putc(a)
}

AND outfl(op, lab) BE
{ wrf("%i5: %s L%n*n", codep, opstr(op), lab)
  putc(op); putref(lab)
}

AND outlab(lab) BE
{ wrf("%i5: Lab L%n*n", codep, lab)
  setlab(lab, codep)
}

AND outentry(l1, l2) BE
{ wrf("%i5: Entry L%n L%n*n", codep, l1, l2)
  putref(l2)
  setlab(l1, codep)
}

AND outstring(lab, str) BE
{ LET sv = mem+datap
  LET s = datap
  LET len = str%0  // The BCPL string has its length in byte zero
  wrf("%i5: String L%n %s*n", datap, lab, str)
  setlab(lab, s) // The VSPL string is at position datap relative
                 // to the start of VSPL memory
  FOR i = 0 TO len DO // The VSPL string is zero terminated
  { IF i MOD bytesperword = 0 DO
      putd(0) // Start a new word of bytes. The next byte of the
              // VSPL string will be at position i relative to
	      // the start of the string. The zero terminating byte
	      // is at position len.
	      // putd advances datap every time it is called. It
	      // will be called at least once.
    sv%i := i<len -> str%(i+1), 0 // assemble a zero terminated string
  }
//  writef("BCPL string *"%s*" compiles into VSPL location from %n to %n*n",
//          str, s, datap-1)
  IF optCode FOR i = s TO datap-1 TEST bytesperword=4
    THEN writef("%i5: %8x*n",  i, mem!i)
    ELSE writef("%i5: %16x*n", i, mem!i)
}

AND outstatvec(lab, a) BE
{ wrf("%i5: Statvec L%n size %n*n", datap, lab, a)
  setlab(lab, datap)
  FOR i = 0 TO a-1 DO putd(0)
}

AND outvar(lab, val) BE
{ wrf("%i5: Var L%n val=%n*n", datap, lab, val)
  setlab(lab, datap)
  putd(val)
}
 
AND putc(w) BE TEST codep>codet
               THEN trnerr("More code space needed, codep=%n codet=%n",
	                    codep, codet)
               ELSE { mem!codep := w
                      codep := codep+1
                    }

AND putd(w) BE TEST datap>datat
               THEN trnerr("More data space needed")
               ELSE { mem!datap := w
                      datap := datap+1
                    }

AND putref(lab) BE TEST codep>codet
                   THEN trnerr("More code space needed")
                   ELSE { mem!codep := refv!lab
                          refv!lab := codep
                          codep := codep+1
                        }

AND setlab(lab, addr) BE labv!lab := addr

AND nextlab() = VALOF
{ TEST labnumber>=labmax
  THEN fatalerr("More label space needed")
  ELSE labnumber := labnumber + 1
  RESULTIS labnumber
}
 

AND resolvelabels() BE FOR lab = 1 TO labnumber DO
{ LET p = refv!lab
  LET labval = labv!lab
  IF p & labval<0 TEST lab=1 THEN trnerr("start not defined")
                             ELSE trnerr("Label %n unset", lab)
  WHILE p DO { LET np = mem!p
               mem!p, p := labval, np
             }
}

AND interpret(regs, mem) = VALOF
{ // reg    address relative to mem of the registers vector
  // mem    pointer to the start of VSPL memory
  LET retcode = 0
  LET rv = mem+regs
  LET res   = rv!0      // The result register
  LET pp    = mem+rv!1  // Absolute VSPL p pointer
  LET sp    = mem+rv!2  // Absolute VSPL stack pointer
  LET pc    = mem+rv!3  // Absolute VSPL program counter
  LET count = rv!4      // The count register

  { LET op = !pc                // Fetch next instruction
    IF optTrace DO
    { // Output the current state with addresses relative to the
      // base of VSPL memory.
      writef("p:%i5  sp:%i5 B=%iA A=%iA  %i5: %t8",
              pp-mem,    sp-mem, sp!-1, sp!0, pc-mem, opstr(op))
      IF hasOperand(op) DO writef(" %n", pc!1)
      newline()
      writef("mem=%n*n", mem)
      //abort(1000)
    }
    IF count<=0 DO { retcode := 3; BREAK } // Zero count
    count := count-1
    pc := pc+1
    // pc is an absolute address.
    // It points to the operand of this instruction, if it
    // has one, or it points to the next the next instruction.
    // op is the op code of this instruction.
    // sp points to the top item of the stack.
    // pp points to the base of the current stack frame.
    // pp!0 points to the previous stack frame.
    // pp!1 is the function return address.
    // pp!2 holds the entry address of the current function.
IF sp-stack>stackt DO
{ writef("Stack overflow, sp-stack=%n, stackt=%n*n", sp-stack, stackt)
  abort(9999)
}
    SWITCHON op INTO
    { DEFAULT:      retcode := 1;    BREAK    // Unknown op code

      CASE Halt:    retcode := res; BREAK

      CASE Laddr:   sp := sp+1; sp!0 := !pc;       pc := pc+1; LOOP
      CASE Ln:      sp := sp+1; sp!0 := !pc;       pc := pc+1; LOOP
      CASE Lp:      sp := sp+1; sp!0 := pp!(!pc);  pc := pc+1; LOOP
      CASE Llp:     sp := sp+1; sp!0 := pp+!pc-mem;pc := pc+1; LOOP
      CASE Ll:      sp := sp+1; sp!0 := mem!(!pc); pc := pc+1; LOOP
      CASE Sp:      pp!(!pc) := sp!0; sp := sp-1;  pc := pc+1; LOOP
      CASE Sl:      mem!(!pc):= sp!0; sp := sp-1;  pc := pc+1; LOOP
      CASE Lres:    sp := sp+1; sp!0 := res;                   LOOP
      CASE Ind:     sp!0 :=  mem!(sp!0);                       LOOP
      CASE Neg:     sp!0 :=  -  sp!0;                          LOOP
      CASE Not:     sp!0 := NOT sp!0;                          LOOP
      CASE Stind:   sp := sp-2; mem!(sp!2) := sp!1;            LOOP
      CASE Vecap:   sp := sp-1; sp!0 := mem!(sp!0 + sp!1);     LOOP
      CASE Mul:     sp := sp-1; sp!0 := sp!0  *  sp!1;         LOOP
      CASE Div:     sp := sp-1; sp!0 := sp!0  /  sp!1;         LOOP
      CASE Mod:     sp := sp-1; sp!0 := sp!0 MOD sp!1;         LOOP
      CASE Add:     sp := sp-1; sp!0 := sp!0  +  sp!1;         LOOP
      CASE Sub:     sp := sp-1; sp!0 := sp!0  -  sp!1;         LOOP
      CASE Eq:      sp := sp-1; sp!0 := sp!0  =  sp!1;         LOOP
      CASE Ne:      sp := sp-1; sp!0 := sp!0 ~=  sp!1;         LOOP
      CASE Le:      sp := sp-1; sp!0 := sp!0 <=  sp!1;         LOOP
      CASE Ge:      sp := sp-1; sp!0 := sp!0 >=  sp!1;         LOOP
      CASE Lt:      sp := sp-1; sp!0 := sp!0  <  sp!1;         LOOP
      CASE Gt:      sp := sp-1; sp!0 := sp!0  >  sp!1;         LOOP
      CASE Lsh:     sp := sp-1; sp!0 := sp!0 <<  sp!1;         LOOP
      CASE Rsh:     sp := sp-1; sp!0 := sp!0 >>  sp!1;         LOOP
      CASE And:     sp := sp-1; sp!0 := sp!0  &  sp!1;         LOOP
      CASE Or:      sp := sp-1; sp!0 := sp!0  |  sp!1;         LOOP
      CASE Xor:     sp := sp-1; sp!0 := sp!0 XOR sp!1;         LOOP
      CASE Jt:      sp := sp-1; pc := sp!1->!pc+mem,pc+1;      LOOP
      CASE Jf:      sp := sp-1; pc := sp!1->pc+1,!pc+mem;      LOOP
      CASE Resultis:sp := sp-1; res := sp!1
      CASE Jump:    pc := !pc+mem;                             LOOP
      CASE Stack:   sp := pp + !pc; pc := pc+1;                LOOP


      CASE Fnrn:  { LET prevpp, retaddr = pp!0+mem, pp!1+mem
                    res := sp!0  // Place the function result in res
                    sp := pp-1
                    pp, pc := prevpp, retaddr
                    LOOP
                  }

      CASE Rtrn:  { LET prevpp, retaddr = pp!0+mem, pp!1+mem
                    sp := pp-1
                    pp, pc := prevpp, retaddr
                    LOOP
                  }

      CASE Rtap:
      CASE Fnap:  { LET prevpp, retaddr = pp, pc+1
                    pp, pc := pp+!pc, sp!0+mem
		    // The first three words of a stack frame hold:
		    // the previous p pointer,
		    // the return address, and
		    // the function entry address.
		    // The word before the entry address holds a
		    // pointer to the name of the function.
                    pp!0 := prevpp -mem  // The previous p pointer
                    pp!1 := retaddr-mem  // The return address
                    pp!2 := pc     -mem  // The function entry address
                    sp := pp+2
		    // The function arguments start at pp+3
                    LOOP
                  }

      CASE Printf:  // !pc is the position in the current stack frame of
                    // the printf arguments
                    sp := pp + !pc - 1 // The value of ss on return
                    pc := pc+1         // The return address
		    //sawritef("Printf: pp=%n sp=%n args %n %n %n*n",
		    //          pp-mem, sp-mem, sp!1, sp!2, sp!3)
                    printf(mem, sp!1, sp+2) // mem, format, the vector of args
                    LOOP

      CASE Sys:     // sys(n, arg)
                    // This is treated as a function call so the
		    // function result, if any, is placed in res.
                    // !pc is the position in the current stack frame of
                    // the sys arguments
                    sp := pp + !pc - 1 // The value of ss on return
                    pc := pc+1         // The return address
      //sawritef("Sys*n")
      //sawritef("Sys: args oldP=%n L=%n pp=%n sp=%n mem=%n*n",
      //          sp!1, sp!2, pp-mem, sp-mem, mem)
      //LOOP
                    SWITCHON sp!1 INTO
                    { DEFAULT: writef("*nBad sys(%n,...) call*n", sp!1)
                               retcode  := 2;                       BREAK   
                      CASE 0:  // sys(0, retcode)
		               retcode  := sp!2;                    BREAK
                      CASE 1:  // sys(1, regsv, mem) Enter the interpreter
		               res := interpret(sp!2, mem);         LOOP
                      CASE 2:  // sys(2, flag) Set tracing on or off
		               optTrace := sp!2;                    LOOP
                      CASE 3:  // sys(3, value) Set count returning the previous value
		               res := count; count := sp!2;         LOOP

                      // The following cases were added on 29 Sep 2025
		      // to allow I/O to and from files.
		      CASE 4:  // sys(4, filename) // findinput(filename)
		               //writef("sys(4,..) calling v2bstr(%n,chv)*n",sp!2)
		               v2bstr(sp!2,chv)
			       //writef("sys(4, *"%s*") calling findinput*n", chv)
		               res := findinput(chv)
			       //writef("=> %n*n", res)
		               res := findinput(chv);               LOOP
		      CASE 5:  // sys(5, filename) // findoutput(filename)
                               res := findoutput(v2bstr(sp!2,chv)); LOOP
		      CASE 6:  // sys(6, instream) // selectinput(instream)
		               //writef("calling selectinput(%n)*n", sp!2)
                               selectinput(sp!2);                   LOOP
		      CASE 7:  // sys(7, outstream) // selectoutput(outstream)
                               selectoutput(sp!2);                  LOOP
		      CASE 8:  // sys(8)           // input()
                               res := cis;                          LOOP
		      CASE 9:  // sys(9)           // output()
                               res := cos;                          LOOP
		      CASE 10: // sys(10)          // rdch()
                               res := rdch();                       LOOP
		      CASE 11: // sys(11, ch)      // wrch(ch)
                               wrch(sp!2);                          LOOP
		      CASE 12: // sys(12)          // endread()
                               endread();                           LOOP
		      CASE 13: // sys(13)          // endwrite()
                               endwrite();                          LOOP
		      CASE 14: // sys(14, n)       // abort(n)
                               abort(sp!2);                         LOOP
		      CASE 15: // sys(15, upb)     // getvec(upb)
                               res := getvec(sp!2);                 LOOP
		      CASE 16: // sys(16, p)       // freevec(p)
                               res := freevec(sp!2);                LOOP
                    }
    }
  } REPEAT // Execute another instruction

  // This point is reached by executing BREAK in the above
  // REPEAT loop.
  // Store the registers in rv and
  // return from interpret with result retcode.
  rv!0, rv!1, rv!2, rv!3, rv!4 := res, pp-mem, sp-mem, pc-mem, count
  RESULTIS retcode
}

AND v2bstr(vstr, bstr) = VALOF
{ LET vs, bs = mem+vstr, mem+bstr // Absolute addresses
  LET len = 0
  { LET ch = vs%len
    IF ch=0 BREAK
    len := len+1
    bstr%len := ch
  } REPEAT
  //writef("string len=%n*n", len)
  bstr%0 := len
  // Pad the last word with zeroes
  FOR i = len+1 TO len+8 DO
  { IF i MOD bytesperword = 0 BREAK
    //writef("Padding byte %n with zero*n", i)
    bstr%i := 0
  }
  //FOR i = 0 TO len/bytesperword TEST bytesperword=4
  //  THEN writef("%8x %8x*n",   vs!i, bstr!i)
  //  ELSE writef("%16x %16x*n", vs!i, bstr!i)
  RESULTIS bstr
}

AND printf(mem, formatstr, p) BE
{ LET fmt = formatstr + mem
  LET i = 0 // Position of next byte in the format string.
  // The possible substitution items are: % <digits> followed
  // by d, s, x or c.
  // Strings are bytes packed in words terminated by zeroes.
  { LET k = fmt%i
    i := i+1
    IF k=0 RETURN // End of format string.
    IF k='%' DO
    { LET n = 0;
      { k := fmt%i  // Evaluate the length value, if any.
        i := i+1
        UNLESS '0'<=k<='9' BREAK
        n := 10*n + k - '0'
      } REPEAT
      SWITCHON k INTO
      { DEFAULT:  wrch(k); LOOP
        CASE 'd': writed  (!p,     n); p := p+1; LOOP
        CASE 's': wrs     (mem+!p, n); p := p+1; LOOP
        CASE 'x': writehex(!p,     n); p := p+1; LOOP
        CASE 'c': wrch    (!p);        p := p+1; LOOP
      }
    }
    wrch(k) // Not a % item.
  } REPEAT
}

AND wrs(s, n) BE
{ LET len = 0
  WHILE s%len DO len := len+1 // Compute the length of the string.
  FOR i = len+1 TO n DO wrch(' ') // Ensure the length of output
                                  // is at least n.
  FOR i = 0 TO len-1 DO wrch(s%i)
}

AND hasOperand(op) = VALOF SWITCHON op INTO
{ CASE Fnrn:CASE Rtrn:CASE Lres:CASE Halt:
  CASE Vecap:CASE Ind:CASE Stind:CASE Neg:CASE Not:
  CASE Mul:CASE Div:CASE Mod:CASE Add:CASE Sub:
  CASE Eq:CASE Ne:CASE Le:CASE Ge:CASE Lt:CASE Gt:
  CASE Lsh:CASE Rsh:CASE And:CASE Or:CASE Xor:
            RESULTIS FALSE
  DEFAULT:  RESULTIS TRUE
}

