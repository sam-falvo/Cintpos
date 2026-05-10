/*
########## UNDER EARLY STAGE OF DEVELOPMENT #############

This is a compiler and interpreter for the language PAL
implemented in BCPL.

(c) Martin Richards 21 Oct 2010

Usage:

pal  "PROG/A,TO=-o/K,TOKENS=-l/S,TREE=-p/S,CODE=-c/S,TRACE=-t/S"

   PROG   gives the filename of the PAL program to run, eg test.pal
-o TO     gives the filename of the output
-l TOKENS is a switch to test the lexical analyser
-p TREE   causes the parse tree to be output
-c CODE   outputs the compiled blackboard evaluator code
-t TRACE  Traces the execution of the blackboard evaluator.

21/10/2010
Started modifying this compiler to make it follow the syntax and
POCODE of the Pal70 compiler whose original compiler listing is in
doc/pal70-mabee-17jun70.pdf

08/07/2010
Started to modify lex and syn to agree with the PAL syntax specified
in Appendix 2.1 (dated 02/17/68) with the following minor extensions.

The operators ~=, <= and >= are included.
( and [ are synonyms as are ) and ].
-> and -* are synonyms.
~ and not are synonyms.

14/06/2010
Lex more or less complete, now working on the syntax analyser.

09/06/2010
Started re-implementation of PAL based on VSPL.

*/


GET "libhdr"
 
MANIFEST {
// Selectors
h1=0; h2=1; h3=2; h4=3; h5=4; h6=5

// Syntactic operators
s_pling=1; s_eof; s_where; s_dot
s_lparen; s_rparen; s_in; s_percent
s_ifso; s_ifnot; s_do

// AE Tree nodesTest
s_def; s_let; s_lambda; s_valof; s_test s_if; s_while; s_ass
s_seq; s_colon
s_noshare; s_cond
s_comma; s_valdef
s_rec; s_and; s_within
s_mpt; s_paren

// AE nodes and POCODE symbols
s_goto; s_res
s_not; s_dummy; s_nil; s_stringconst; s_name
s_true; s_false; s_jj; s_sys
s_plus; s_minus
s_aug; s_logor; s_logand
s_eq; s_ne; s_ls; s_le; s_gr; s_ge
s_mult; s_div; s_power
s_pos; s_neg; s_apply

// POCODE symbols
i_loadL; i_loadR; i_loadE; i_loadS; i_loadN; i_loadF; i_loadJ
i_restoreE1; i_loadGuess
i_formClosure; i_formLvalue; i_formRvalue
i_members
i_jump; i_jumpF; i_save; i_return
i_testEmpty; s_lose1; i_update
i_declname; i_declnames; i_initname; i_initnames
i_decllabel; i_setlabEs; i_blocklink; i_reslink
i_setup; i_halt
i_lab;
i_dummy; i_true; i_false; i_sys; i_jj

//i_param; s_equ

// AE nodes, POCODE symbols and run-time node types
t_dummy; t_jj; t_true; t_false; Int; Real; t_sys
t_lvalue; t_string
Number; Tuple; t_stack


// Translation symbols
Val=0; Ref

// Library functions
Sys_isboolean=1
Sys_isstring
Sys_isfunction
Sys_isprogramclosure
Sys_islabel
Sys_istuple
Sys_isreal
Sys_isinteger
Sys_stem
Sys_stern
Sys_conc
Sys_itor
Sys_rtoi
Sys_stoi
Sys_lookupinj
Sys_order
Sys_print
Sys_readch
Sys_atom
Sys_null
Sys_share
}
 
GLOBAL { 
rec_p:ug; rec_l; fin_p; fin_l
fatalerr; synerr; trnerr; errcount; errmax
progstream; tostream
mk1; mk2; mk3; mk4; mk5; mk6
newvec; treep; treevec
optTokens; optTree; optCode; optTrace

// Globals used in LEX
chbuf; charv; ch; rch; lex
token; lexval; exponent; wordnode
nilnode; truenode; falsenode; dummynode; mptnode
wrchbuf; chcount; lineno
dsw; declsyswords; namestart; nametable; lookupword
rdnumber; rdstrch; rdtag

// Globals used in SYN
checkfor; rdprog
rdnamelist; rname
rdnbdef; rbdef; rndef; rdef
formtree; plist
rnexp; rexp; rnbexp; rbexp
rncom; rcom; rbcom

// Globals used in TRN and the interpreter
 
trnext:300; trprog; trdef; trans
findlabels; translabels; transrhs
loaddefinee; declguesses; initnames; transscope
mapb; mapf; length; upssp
trcom; decldyn
declstatnames; checkdistinct; addname; cellwithname
trdecl; jumpcond
assign; load; fnbody; loadlist; transname
dvec; dvece; dvecp; dvect
comline; procname; resultlab; ssp; msp
outf; outname; outstring; outfv; outfn; outfsl
outfnn; outfl; outfs; outentry
outlab; outlabset; outvar; outstatvec; outstring
opstr; hasOperand
pc; sp; env; dump; count
rega; regb
oldc
codev; codep; codet
datav; datap; datat
stack; stackp; stackt
nilrv; nilsrv; dummyrv; 
labv; refv; labmax; putc; putd; putref
setlab; setlabval; nextlab; labnumber; resolvelabels
interpret
next11
}

MANIFEST {                         //  Selectors
nametablesize = 541
c_tab         =   9
c_newline     =  10
}
 
LET start() = VALOF
{ LET treesize = 0
  AND codesize = 0
  AND datasize = 0
  AND argv = VEC 50
  AND argform =
        "PROG/A,TO=-o/K,TOKENS=-l/S,TREE=-p/S,CODE=-c/S,TRACE=-t/S"
  LET stdout = output()

  errmax   := 2
  errcount := 0
  fin_p, fin_l := level(), fin

  treevec, labv, refv := 0, 0, 0
  progstream, tostream := 0, 0
   
  writef("*nPAL (05 Mar 2024)*n")
 
  IF rdargs(argform, argv, 50)=0 DO fatalerr("Bad arguments*n")

  treesize := 10000
  codesize := 50000
  datasize :=  5000

  progstream := findinput(argv!0)      // PROG

  IF progstream=0 DO fatalerr("Trouble with file %s*n", argv!0)

  selectinput(progstream)
 
  IF argv!1                            // TO      -o
  DO { tostream := findoutput(argv!1)
       IF tostream=0 DO fatalerr("Trouble with code file %s*n", argv!1)
     }

  optTokens := argv!2                  // TOKENS  -l
  optTree   := argv!3                  // TREE    -p
  optCode   := argv!4                  // CODE    -c
  optTrace  := argv!5                  // TRACE   -t

  treevec := getvec(treesize)
  codev   := getvec(codesize)
  codev!0 := codesize
  codep   := 1  // The start address
  codet   := codesize

  datav   := getvec(datasize)
  datap   := 0
  datat   := datasize

  labv := getvec(1000)
  refv := getvec(1000)
  labmax := 1000

  UNLESS treevec & codev & datav & labv & refv DO
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

    IF optTree DO { writes("*nParse Tree*n*n")
                    plist(tree, 0, 20)
                    newline()
                  }
  
    IF errcount GOTO fin

    FOR i = 0 TO codet DO codev!i := 0
    FOR i = 0 TO datat DO datav!i := 0

    trprog(tree)                    // Translate the tree

    IF errcount GOTO fin

    // Set the initial CSED machine state
    sp := 0           // sp
    pc := 0           // pc
    env := 0          // env
    dump := 0         // dump
    count := maxint   // count

 
    writef("*nStarting the interpreter*n*n")

    { LET ret = interpret()   // Execute the interpreter
      IF ret DO writef("Return code %n*n", ret)
      writef("*nInstructions executed: %n*n", maxint-count)
    }
  }
   
fin:
  IF treevec       DO freevec(treevec)
  IF labv          DO freevec(labv)
  IF refv          DO freevec(refv)
  IF progstream    DO { selectinput(progstream); endread()  }
  IF tostream      DO { selectoutput(tostream)
                        UNLESS tostream=stdout DO  endwrite() }

  selectoutput(stdout)
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
                token := rdnumber()
                RETURN
 
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
 
    CASE '[':
    CASE '(': token := s_lparen;    BREAK
    CASE ']':
    CASE ')': token := s_rparen;    BREAK 
    CASE '%': token := s_percent;   BREAK 
    CASE '+': token := s_plus;      BREAK
    CASE ',': token := s_comma;     BREAK
    CASE '&': token := s_logand;    BREAK
    CASE '!': token := s_pling;     BREAK
    CASE '=': token := s_eq;        BREAK
    CASE '^': token := s_power;     BREAK
    CASE ';': token := s_seq;       BREAK
    CASE '$': token := s_noshare;   BREAK
    CASE '.': token := s_dot;       BREAK
 
    CASE '**':  rch()
                IF ch='**' DO { token := s_power;  BREAK }
                token := s_mult
                RETURN

    CASE '/':   rch()
                IF ch='/' DO
                { rch() REPEATUNTIL ch='*n' | ch=endstreamch
                  LOOP
                }
                token := s_div
                RETURN
 
    CASE '<':   rch()
                IF ch='=' DO { token := s_le;  BREAK }
                token := s_ls
                RETURN

    CASE '>':   rch()
                IF ch='=' DO { token := s_ge;  BREAK }
                token := s_gr
                RETURN

    CASE '~':   rch()
                IF ch='=' DO { token := s_ne;  BREAK }
                token := s_not
                RETURN

    CASE '-':   rch()
                IF ch='>' | ch='**' DO { token := s_cond; BREAK }
                token := s_minus
                RETURN

    CASE ':':   rch()
                IF ch='=' DO { token := s_ass;  BREAK }
                token := s_colon
                RETURN
 
    CASE '*'': // A string constant
              { LET len = 0
                rch()
 
                UNTIL ch='*'' DO
                { IF len=255 DO synerr("Bad string constant")
                  len := len + 1
                  charv%len := rdstrch()
                }
 
                charv%0 := len
                wordnode := newvec(len/bytesperword+2)
                h1!wordnode := s_stringconst
                FOR i = 0 TO len DO (@h2!wordnode)%i := charv%i
                token := s_stringconst
                BREAK
              }
 
    DEFAULT:    UNLESS ch=endstreamch DO
                { LET badch = ch
                  ch := '*s'
                  synerr("Illegal character %x2", badch)
                }
                token := s_eof
                RETURN
  } REPEAT
 
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
    h1!wordnode, h2!wordnode := s_name, nametable!hashval
    FOR i = 0 TO len DO (@h3!wordnode)%i := word%i
    nametable!hashval := wordnode
  }
  RESULTIS h1!wordnode
}
 
AND dsw(word, tok) BE { lookupword(word); h1!wordnode := tok  }
 
AND declsyswords() BE
{ 
  dsw("and", s_and)
  dsw("aug", s_aug)
  dsw("def", s_def)
  dsw("do", s_do)
  dsw("dummy", s_dummy)
  dummynode := wordnode
  dsw("else", s_ifnot)
  dsw("eq", s_eq)
  dsw("false", s_false)
  falsenode := wordnode
  dsw("fn", s_lambda)
  dsw("ge", s_ge)
  dsw("goto", s_goto)
  dsw("gt", s_gr)
  dsw("if", s_if)
  dsw("ifnot", s_ifnot)
  dsw("ifso", s_ifso)
  dsw("in", s_in)
  dsw("jj", s_jj)
  dsw("le", s_le)
  dsw("let", s_let)
  dsw("ll", s_lambda)
  dsw("logand", s_logand)
  dsw("lt", s_ls)
  dsw("ne", s_ne)
  dsw("nil", s_nil)
  nilnode := wordnode
  dsw("not", s_not)
  dsw("or", s_logor)
  dsw("rec", s_rec)
  dsw("res", s_res)
  ///dsw("resultis", s_res)
  dsw("sys", s_sys)
  dsw("test", s_test)
  dsw("then", s_ifso)
  dsw("true", s_true)
  truenode := wordnode
  ///dsw("val", Val)
  dsw("valof", s_valof)
  dsw("where", s_where)
  dsw("within", s_within)
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

AND rdnumber() = VALOF
{ LET tok, zeroes, ok = Int, 0, FALSE
  lexval, exponent := 0, 0

  WHILE '0'<=ch<='9' DO
  { ok := TRUE               // At least one digit
    TEST ch='0'
    THEN { zeroes := zeroes+1
         }
    ELSE { WHILE zeroes DO
           { IF lexval > maxint/10 TEST tok=Int
             THEN synerr("Integer too large")
             ELSE synerr("Too many significant digits")
             lexval := 10*lexval
             zeroes := zeroes-1
             exponent := exponent-1
           }
           IF lexval > maxint/10 TEST tok=Int
           THEN synerr("Integer too large")
           ELSE synerr("Too many significant digits")
           lexval := 10*lexval + ch - '0'
           exponent := exponent - 1
         }
    rch()
    WHILE ch='.' DO
    { IF tok=Real DO synerr("Bad real number")
      tok, ok := Real, FALSE  // No digits after dot yet
      exponent := zeroes
      rch()
    }
  }
  TEST tok=Real
  THEN { UNLESS ok DO
           synerr("No digits after decimal point")
         IF lexval>99999999 DO
           synerr("More than 8 significant digits in real number")
         IF exponent=0 DO
           synerr("No digits after decimal point in real number")
       }
  ELSE { WHILE zeroes DO
         { IF lexval > maxint/10 DO synerr("Number too large")
           lexval := 10*lexval
           zeroes := zeroes-1
         }
       }
  RESULTIS tok
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
  IF ch='**' DO
  { rch()
    SWITCHON ch INTO
    { DEFAULT:   synerr("Bad string or character constant")
      CASE '*'': CASE '"':  res := ch;     ENDCASE
      CASE 't':  CASE 'T':  res := '*t';   ENDCASE
      CASE 's':  CASE 'S':  res := '*s';   ENDCASE
      CASE 'n':  CASE 'N':  res := '*n';   ENDCASE
      CASE 'b':  CASE 'B':  res := '*b';   ENDCASE
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
  mptnode := mk1(s_mpt)

  lex()

  IF optTokens DO            // For debugging lex.
  { writef("token = %i3 %s", token, opstr(token))
    IF token=Int    DO writef("       %n",  lexval)
    IF token=Real   DO writef("      %ne%n",  lexval, exponent)
    IF token=s_name   DO writef("      %s",   charv)
    IF token=s_stringconst DO
    { writef("    *'")
      FOR i = 1 TO charv%0 SWITCHON charv%i INTO
      { DEFAULT:   wrch(charv%i); ENDCASE
        CASE '*n': writes("**n"); ENDCASE
        CASE '*p': writes("**p"); ENDCASE
        CASE '*t': writes("**t"); ENDCASE
      }
      writef("*'")
    }
    newline()
    IF token=s_eof RESULTIS 0
    lex()
  } REPEAT

recover:
  res := rdprog()
  UNLESS token=s_eof DO fatalerr("Incorrect termination")
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
{ // P -> def D0 .. def D0 in C0 eof |
  //      C0 eof
  LET prog = 0

  TEST token=s_def
  THEN { LET link = @prog

         { LET a = mk4(s_def, 0, 0, lineno)
           h2!a := rndef()
           !link := a
  	   link := @h3!a
	 } REPEATWHILE token=s_def

         checkfor(s_in, "'in' expected at the end of a 'def' list")
       }
  ELSE { prog := rcom(0)
       }
  
  UNLESS token=s_eof DO synerr("Incorrect termination")

  RESULTIS prog
}

AND rnbdef(n) = VALOF
{ lex()
  RESULTIS rbdef(n)
}

AND rbdef(n) = VALOF
{ // BD -> N,...,N = E
  //       N BV...BV = E
  //       ( D )
  //       rec D
  LET op, ln = token, lineno

  SWITCHON op INTO
  { DEFAULT:
      synerr("Bad definition, name, rec or '(' expected")

    CASE s_name:
      { LET names = rname()
        ln := lineno

        IF token=s_comma DO
        { // Must be a simultaneous definition
          // N ,..., N = C0
          names := rdnamelist(names)
          checkfor(s_eq, "Bad definition")
          RESULTIS mk4(s_valdef, names, rcom(0), ln)
        }

        IF token=s_eq RESULTIS mk4(s_valdef, names, rncom(0), ln)

        { // Must be a function definition
          // N BV ... BV = C0
          LET v = VEC 50
          AND i, b = 0, ?
          WHILE i<=50 DO
          { UNLESS token=s_lparen | token=s_name BREAK
            v!i := rbv()
            i := i+1
          }
          UNLESS i~=0 & token=s_eq DO synerr("Bad definition")
          b := rncom(0)
          WHILE i>0 DO
          { i := i-1
            b := mk4(s_lambda, v!i, b, ln)
          }
          RESULTIS mk4(s_valdef, names, b, ln)
        }
      }

    CASE s_lparen:
    { LET a = rndef(0)
      checkfor(s_rparen, "Bad definition")
      RESULTIS a
    }

    CASE s_rec:
      lex()
      UNLESS n=0 DO synerr("Redundant 'rec'")
      RESULTIS mk3(s_rec, rnbdef(2), ln)
  }
}

AND rndef(n) = VALOF { lex(); RESULTIS rdef(n) }

AND rdef(n) = VALOF
{ // D -> D and D
  //      D within D
  //      BD
  LET a = rbdef(0)
  LET b = 0

  { LET op, ln = token, lineno

//sawritef("rdef: op=%s ln=%n*n", opstr(op), ln)
    SWITCHON op INTO
    { DEFAULT:
        RESULTIS a

      CASE s_and:
        IF a=0 DO synerr("Definition missing before 'and'")
        IF n>=6 RESULTIS a
        { LET i = 1
          LET v = VEC 100
          WHILE token=s_and DO
          { v!i := rnbdef(0)
            i := i+1
          }
          b := a
          a := newvec(i+1)
          a!0, a!1, a!2 := s_and, i+1, b
          FOR j = 1 TO i-1 DO a!(j+2) := v!j
          LOOP
        }

      CASE s_within:
        IF a=0 DO synerr("Definition missing before 'within'")
        IF n>=3 RESULTIS a
        a := mk4(s_within, a, rndef(0), ln)
        LOOP
    }
  } REPEAT
}

AND rbv() = VALOF
{ // Only called when token is Name or Lparen
  LET a = ?
  IF token=s_name RESULTIS rname()
  checkfor(s_lparen, "'(' expected")
  IF token=s_rparen DO
  { lex()
    RESULTIS mptnode
  }
  a := rdnamelist(0)
  checkfor(s_rparen, "Bad bound variable list")
  RESULTIS a
}

AND rdnamelist(n) = VALOF
{ LET a, b, i, ln = 0, n, 1, lineno
  LET v = VEC 100
  IF n=0 DO
  { UNLESS token=s_name DO
      synerr("Bad name list")
    b := rname()
  }
  UNLESS token=s_comma RESULTIS b
  WHILE token=s_comma DO
  { lex()
    UNLESS token=s_name DO synerr("A name is missing")
    v!i := rname()
    i := i+1
  }
  a := newvec(i+1)
  h1!a, h2!a, h3!a := s_comma, i, b
  FOR j = 1 TO i-1 DO a!(j+2) := v!j
  RESULTIS a
}

AND rname() = VALOF
{ LET a = wordnode
  checkfor(s_name, "Name expected")
  RESULTIS a
}

AND rarg() = VALOF
{ LET a, ln = 0, lineno
sawritef("rarg: token=%s*n", opstr(token))
  SWITCHON token INTO
  { DEFAULT:
      RESULTIS 0  // Not suitable as an unparenthesised argument

  }
}
 
LET rbexp(n) = VALOF
{ LET a, op, ln = 0, token, lineno
 
  SWITCHON op INTO
 
  { DEFAULT:
      synerr("Error in expression")

    CASE s_true:
    CASE s_false:
    CASE s_name:
    CASE s_nil:
    CASE s_dummy:
    CASE s_stringconst:
    CASE s_sys:
    CASE s_jj:
      a := wordnode
      lex()
      RESULTIS a
   
    CASE s_lparen:
      lex()
      TEST token=s_rparen
      THEN a := nilnode
      ELSE a := rcom(0)
      checkfor(s_rparen, "')' missing")
      IF n<=8 DO a := mk3(s_paren, a, ln)
      RESULTIS a
 
    CASE Int:
      a := mk2(Int, lexval)
      lex()
      RESULTIS a
 
    CASE Real:
      a := mk3(Real, lexval, exponent)
      lex()
      RESULTIS a
 
    CASE s_noshare:
      UNLESS n<=36 DO synerr("'$' or 'sys' out of context")
      RESULTIS mk3(op, rnexp(38), ln)
 
    CASE s_plus:
      UNLESS n<=30 DO synerr("'+' out of context")
      RESULTIS rnexp(32)
 
    CASE s_minus:
      UNLESS n<=30 DO synerr("'-' out of context")
      a := rnexp(32)
      TEST h1!a=Int | h1!a=Real
      THEN h2!a := - h2!a
      ELSE a := mk2(s_neg, a)
      RESULTIS a
 
    CASE s_not:
      UNLESS n<=24 DO synerr("'not' out of context")
      RESULTIS mk2(s_not, rnexp(26))
  }
}
 
AND rnexp(n) = VALOF { lex(); RESULTIS rexp(n) }
 
AND rexp(n) = VALOF
{ LET a, b, p = rbexp(n), 0, 0

  { LET op, ln = token, lineno

    SWITCHON op INTO
    { DEFAULT:
        RESULTIS a
 
      // Tokens that start a function argument
      
      CASE s_nil:
      CASE s_true:
      CASE s_false:
      CASE Int:
      CASE Real:
      CASE s_dummy:
      CASE s_stringconst:
      CASE s_name:
      CASE s_jj:
        a := mk4(s_apply, a, rbexp(0), ln)
        LOOP

      CASE s_lparen:
        lex()
        IF token=s_rparen DO
        { // Empty argument list
          lex()
          RESULTIS nilnode
        }
        b := rcom(0)
        checkfor(s_rparen, "')' expected")
        a := mk4(s_apply, a, b, ln)
        LOOP

      CASE s_comma:
        IF n>14 RESULTIS a
        { LET i = 1
          LET v = VEC 500
          WHILE token=s_comma DO
          { v!i := rnexp(16)
            i := i+1
          }
          b := a
          a := newvec(i+1)
          a!0, a!1, a!2 := s_comma, i, b
          FOR j = 1 TO i-1 DO a!(j+2) := v!j
//sawritef("rexp: Comma i=%n*n", i)
          LOOP
        }

      CASE s_aug:
        IF n>16 RESULTIS a
        a := mk4(s_aug, a, rnexp(18), ln)
        LOOP

      CASE s_cond:
        IF n>18 RESULTIS a
        b := rnexp(18)
        checkfor(s_pling, "Bad conditional expression")
        a := mk5(s_cond, a, b, rexp(18), ln)
        LOOP

      CASE s_logor:
        IF n>20 RESULTIS a
        a := mk4(op, a, rnexp(22), ln)
        LOOP

      CASE s_logand:
        IF n>22 RESULTIS a
        a := mk4(op, a, rnexp(24), ln)
        LOOP

      CASE s_eq:CASE s_le:CASE s_ls:CASE s_ne:CASE s_ge:CASE s_gr:
        IF n>26 RESULTIS a
        a := mk4(op, a, rnexp(30), ln)
        LOOP

      CASE s_plus:CASE s_minus:
        IF n>30 RESULTIS a
        a := mk4(op, a, rnexp(32), ln)
        LOOP

      CASE s_mult:CASE s_div:
        IF n>32 RESULTIS a
        a := mk4(op, a, rnexp(34), ln)
        LOOP

      CASE s_power:
        IF n>36 RESULTIS a
        a := mk4(op, a, rnexp(34), ln)
        LOOP

      CASE s_percent:
        IF n>36 RESULTIS a
        lex()
        UNLESS token=s_name DO synerr("Name expected in '%' construct")
        b := rname()
        a := mk4(s_comma, 2, a, rexp(38))
        a := mk4(s_apply, b, a, ln)
        LOOP
    }
  } REPEAT
}

AND rncom(n) = VALOF
{ lex()
  RESULTIS rcom(n)
}

AND rcom(n) = VALOF
{ LET a = rbcom(n)

  { LET op, ln = token, lineno
    SWITCHON op INTO
 
    { DEFAULT:
        BREAK
 
      CASE s_seq:
        IF n>6 RESULTIS a
        a := mk4(s_seq, a, rncom(6), ln)
        LOOP

      CASE s_where:
        IF n>2 RESULTIS a
        a := mk4(s_where, a, rnbdef(0), ln)
        LOOP

      CASE s_colon:
        UNLESS h1!a=s_name & n<=8 DO
          synerr("Syntax error in label")
        a := mk5(s_colon, a, rncom(8), 0, ln)
        LOOP
    }
  } REPEAT

  RESULTIS a
}

AND rbcom(n) = VALOF
{ LET op, ln, a, b = token, lineno, 0, 0

  SWITCHON op INTO
  { DEFAULT: // Must be an expression
    { a := rexp(n)
      ln := lineno
      IF token=s_ass RESULTIS mk4(s_ass, a, rnexp(14), ln)
      RESULTIS a
    }

    CASE s_let:
    { UNLESS n=0 DO synerr("'let' out of context")
      a := rndef(0)
      checkfor(s_in, "'in' expected in 'let' construct")
      RESULTIS mk4(s_let, a, rcom(0), ln)
    }

    CASE s_lambda:
    { LET v = VEC 50
      AND i = 0
      UNLESS n=0 DO synerr("'fn' out of context")
      lex()
      WHILE i<=50 DO
      { UNLESS token=s_lparen | token=s_name BREAK
        v!i := rbv()
        i := i+1
      }
      IF i=0 DO synerr("No bound variable list after 'fn'")
      checkfor(s_dot, "'.' missing in 'fn' construct")
      a := rcom(0)
      WHILE i>0 DO
      { i := i-1
        a := mk4(s_lambda, v!i, a, ln)
      }
      RESULTIS a
    }

    CASE s_valof:
      UNLESS n<=4 DO synerr("'valof' out of context")
      RESULTIS mk3(op, rncom(6), ln)
 
    CASE s_test:
      UNLESS n<=10 DO synerr("'test' out of context")
      a := rnexp(20)
      SWITCHON token INTO
      { DEFAULT:
          synerr("Bad 'test' command")

        CASE s_ifso:
          b := rncom(8)
          checkfor(s_ifnot, "'ifnot' expected")
          RESULTIS mk5(s_cond, a, b, rncom(8), ln)

        CASE s_ifnot:
          b := rncom(8)
          checkfor(s_ifso, "'ifnot' expected")
          RESULTIS mk5(s_cond, a, rncom(8), b, ln)
      }


    CASE s_while:
    CASE s_if:
    { LET op = token
      UNLESS n<=10 DO synerr("'if' or 'while' out of context")     
      a := rnexp(20)
      checkfor(s_do, "'do' expected")
      TEST op=s_if
      THEN RESULTIS mk5(s_cond, a, rcom(8), dummynode, ln)
      ELSE RESULTIS mk5(s_while, a, rcom(8), ln)
    }

    CASE s_goto:
      RESULTIS mk3(s_goto, rnexp(38), ln)

    CASE s_res:
      RESULTIS mk3(s_res, rnexp(14), ln)

  }
}

LET opstr(op) = VALOF SWITCHON op INTO
{ DEFAULT:          sawritef("opstr: unknown op: %n*n", op)
                    RESULTIS "###Unknown op ###"

  CASE s_ass:         RESULTIS "Ass"
  CASE s_and:         RESULTIS "And"
  CASE s_apply:       RESULTIS "Apply"
  CASE s_aug:         RESULTIS "Aug"
  CASE s_pling:         RESULTIS "Pling"
  CASE i_blocklink:   RESULTIS "Blocklink"
  CASE s_colon:       RESULTIS "Colon"
  CASE s_comma:       RESULTIS "Comma"
  CASE s_cond:        RESULTIS "Cond"
  CASE i_decllabel:   RESULTIS "Decllabel"
  CASE i_declname:    RESULTIS "Declname"
  CASE i_declnames:   RESULTIS "Declnames"
  CASE s_def:         RESULTIS "Def"
  CASE s_div:         RESULTIS "Div"
  CASE s_do:          RESULTIS "Do"
  CASE s_dot:         RESULTIS "Dot"
  CASE s_dummy:
  CASE i_dummy:
  CASE t_dummy:       RESULTIS "Dummy"
  CASE s_eof:         RESULTIS "Eof"
  CASE s_eq:          RESULTIS "Eq"
  CASE s_ifnot:       RESULTIS "Ifnot"
  CASE s_power:       RESULTIS "Power"
  CASE s_false:       RESULTIS "False"
  CASE i_formClosure: RESULTIS "FormClosure"
  CASE i_formLvalue:  RESULTIS "FormLvalue"
  CASE i_formRvalue:  RESULTIS "FormRvalue"
  CASE s_ge:          RESULTIS "Ge"
  CASE s_goto:        RESULTIS "Goto"
  CASE s_gr:          RESULTIS "Gr"
  CASE i_halt:        RESULTIS "Halt"
  CASE s_if:          RESULTIS "If"
  CASE s_in:          RESULTIS "In"
  CASE i_initname:    RESULTIS "Initname"
  CASE i_initnames:   RESULTIS "Initnames"
  CASE Int:         RESULTIS "Int"
  CASE s_jj:
  CASE i_jj:
  CASE t_jj:          RESULTIS "Jj"
  CASE i_jump:        RESULTIS "Jump"
  CASE i_jumpF:       RESULTIS "JumpF"
  CASE i_lab:         RESULTIS "Lab"
  CASE s_lambda:      RESULTIS "Lambda"
  CASE s_le:          RESULTIS "le"
  CASE s_let:         RESULTIS "Let"
  CASE i_loadE:       RESULTIS "LoadE"
  CASE i_loadF:       RESULTIS "LoadF"
  CASE i_loadGuess:   RESULTIS "LoadGuess"
  CASE i_loadJ:       RESULTIS "LoadJ"
  CASE i_loadL:       RESULTIS "LoadL"
  CASE i_loadN:       RESULTIS "LoadN"
  CASE i_loadR:       RESULTIS "LoadR"
  CASE i_loadS:       RESULTIS "LoadS"
  CASE s_logand:      RESULTIS "Logand"
  CASE s_logor:       RESULTIS "Logor"
  CASE s_lose1:       RESULTIS "Lose1"
  CASE s_lparen:      RESULTIS "Lparen"
  CASE s_ls:          RESULTIS "Ls"
  CASE i_members:     RESULTIS "Members"
  CASE s_minus:       RESULTIS "s_minus"
  CASE s_mpt:         RESULTIS "Mpt"
  CASE s_mult:        RESULTIS "Mult"
  CASE s_name:        RESULTIS "Name"
  CASE s_ne:          RESULTIS "Ne"
  CASE s_neg:         RESULTIS "Neg"
  CASE s_nil:         RESULTIS "Nil"
  CASE s_not:         RESULTIS "Not"
  CASE s_paren:       RESULTIS "Paren"       
  CASE s_percent:     RESULTIS "Percent"       
  CASE s_plus:        RESULTIS "Plus"
  CASE s_pos:         RESULTIS "Pos"
  CASE Real:        RESULTIS "Real"       
  CASE s_rec:         RESULTIS "Rec"       
  CASE s_res:         RESULTIS "Res"
  CASE i_reslink:     RESULTIS "Reslink"
  CASE i_restoreE1:   RESULTIS "RestoreE1"
  CASE i_return:      RESULTIS "Return"
  CASE s_rparen:      RESULTIS "Rparen"
  CASE i_save:        RESULTIS "Save"
  CASE s_seq:         RESULTIS "Seq"
  CASE i_setlabEs:    RESULTIS "SetlabEs"
  CASE i_setup:       RESULTIS "Setup"
  CASE s_stringconst: RESULTIS "Stringconst"
  CASE s_sys:
  CASE i_sys:
  CASE t_sys:         RESULTIS "Sys"
  CASE s_test:        RESULTIS "Test"
  CASE i_testEmpty:   RESULTIS "TestEmpty"
  CASE s_ifso:        RESULTIS "Ifso"
  CASE s_true:
  CASE i_true:
  CASE t_true:        RESULTIS "True"
  CASE Tuple:       RESULTIS "Tuple"
  CASE s_noshare:     RESULTIS "Noshare"
  CASE i_update:      RESULTIS "Update"
  CASE s_valdef:      RESULTIS "Valdef"
  CASE s_valof:       RESULTIS "Valof"
  CASE s_where:       RESULTIS "Where"
  CASE s_within:      RESULTIS "Within"
}

LET plist(x, n, d) BE
{ LET op, size, ln = ?, 0, 0
  LET v = TABLE 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

  IF x=0 DO { writes("Null"); RETURN  }
 
  op := h1!x

  SWITCHON op INTO
  { DEFAULT:
writef("Default op=%s*n", opstr(op)); RETURN

    CASE Int:     writef("Int %n", h2!x);           RETURN
    CASE Real:    writef("Real %ne%n", h2!x, h3!x); RETURN
    CASE s_name:    writef("Name %s", x+2);           RETURN
    CASE s_stringconst:  
                { LET s = x+1
                  writef("s_stringconst *'")
                  FOR i = 1 TO s%0 SWITCHON s%i INTO
                  { DEFAULT:   wrch(s%i); ENDCASE
                    CASE '*n': writes("**n"); ENDCASE
                    CASE '*p': writes("**p"); ENDCASE
                    CASE '*t': writes("**t"); ENDCASE
                  }
                  writef("*'")
                  RETURN
                }

    CASE s_colon:
         size, ln := 3, h5!x; ENDCASE

    CASE s_cond: CASE s_test: CASE s_percent:
         size, ln := 4, h5!x; ENDCASE

    CASE s_power: CASE s_mult: CASE s_div: CASE s_plus: CASE s_minus:
    CASE s_eq: CASE s_ne: CASE s_ls: CASE s_gr: CASE s_le: CASE s_ge:
    CASE s_logand: CASE s_logor: CASE s_aug:
    CASE s_let: CASE s_where: CASE s_within:
    CASE i_lab:
    CASE s_ass: CASE s_apply: CASE s_lambda:
    CASE s_def: CASE s_valdef: CASE Tuple: CASE s_seq:
    CASE s_if:
         size, ln := 3, h4!x; ENDCASE

    CASE s_comma: CASE s_and:
         // x -> [op, n, a1 ,..., an]
         size := h2!x+1
//sawritef("plist: Comma size=%n*n", size)
         x := x+1
         ENDCASE

    CASE s_noshare:
    CASE s_rec:
    CASE s_valof: 
    CASE s_goto: 
    CASE s_res:
    CASE s_sys:
    CASE s_paren:
         size, ln := 2, h3!x; ENDCASE

    CASE s_true: CASE s_false:
    CASE s_nil: CASE s_mpt:
    CASE s_dummy: CASE s_jj:
         size := 1;       ENDCASE
  }
 
  IF n=d DO { writes("Etc"); RETURN }
  writef("%s", opstr(op))
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
{ LET n = ?
  FOR i = 0 TO labmax DO labv!i, refv!i := -1, 0

  comline, procname, labnumber := 1, 0, 0
  ssp, msp := 0, 1

  IF optCode DO writef("*nCompiled code:*n*n")

  n := nextlab()
  outfl(i_setup, n)

  translabels(x)

  trans(x, Val)
  UNLESS ssp=1 DO writef("*nSSP error*n")
  outf(i_halt)
  outlabset(n, msp)

  //resolvelabels()

  writef("*nProgram size: %n data size: %n*n*n",
          codep, datap)
}

LET trans(x, mode) BE
// x       is the program
// mode is Val or Ref
{ LET op = h1!x

  IF x=0 DO
  { writes("*nExpression missing*n")
    outf(s_nil)
    upssp(1)
    IF mode=Ref DO outf(i_formLvalue)
    RETURN
  }

//writef("trans: op=%s*n", opstr(op))
  SWITCHON op INTO
  { DEFAULT:
      // It must be an expression
      load(x)
      RETURN

    CASE s_let:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      comline := h5!x
      transrhs(h2!x)
      outfl(i_blocklink, lab1)
      IF ssp=msp DO msp := ssp+1
      transscope(x, lab2, mode)
      outlab(lab1)             
      RETURN
    }
  
    CASE s_def:
      transrhs(h2!x)
      declnames(h2!x)
      translabels(h3!x)
      trans(h3!x, Val)
      RETURN

    CASE s_mult: CASE s_div: CASE s_power: CASE s_plus: CASE s_minus:
    CASE s_eq: CASE s_ne: CASE s_ls: CASE s_le: CASE s_gr: CASE s_ge:
    CASE s_logand: CASE s_logor:
      trans(h3!x, Val)
      trans(h2!x, Val)
      outf(op)
      ssp := ssp-1
      IF mode=Ref DO outf(i_formLvalue)
      RETURN

    CASE s_aug:
      trans(h3!x, Ref)
      trans(h2!x, Val)
      outf(s_aug)
      ssp := ssp-1
      IF mode=Ref DO outf(i_formLvalue)
      RETURN

    CASE s_apply:
      trans(h3!x, Ref)
      trans(h2!x, Ref)
      outf(s_apply)
      ssp := ssp-1
      IF mode=Val DO outf(i_formRvalue)
      RETURN

    CASE s_pos:
    CASE s_neg:
    CASE s_not:
      trans(h2!x, Val)
      outf(op)
      IF mode=Ref DO outf(i_formLvalue)
      RETURN

    CASE s_noshare:
      trans(h2!x, Val)
      IF mode=Ref DO outf(i_formLvalue)
      RETURN

    CASE s_comma:
    { LET len = length(x)
      LET r(x) BE trans(x, Ref)
      mapb(r, x)
      outfn(Tuple, len)
      ssp := ssp - len + 1
      IF mode=Ref DO outf(i_formLvalue)
      RETURN
    }

    CASE s_lambda:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      LET lab3 = nextlab()
      outfl(i_formClosure, lab1)
      upssp(1)
      outfl(i_jump, lab2)
      outlab(lab1)
      transscope(x, lab3, Ref)
      outlab(lab2)
      IF mode=Ref DO outf(i_formLvalue)
      RETURN
    }

    CASE s_colon:
      IF h4!x=0 DO
      { trnerr("Label %s improperly used", h3!(h2!x))
      }
      outlab(h4!x)
      trans(h3!x, mode)
      RETURN

    CASE s_seq:
      trans(h2!x, Val)
      outf(s_lose1)
      trans(h3!x, mode)
      RETURN

    CASE s_valof:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      outfl(i_reslink, lab1)
      ssp := ssp+1
      IF ssp>=msp DO msp := ssp+1
      { LET a, b = ssp, msp
        ssp, msp := 0, 1
        outfl(i_save, lab2)
        outf(s_jj)
        outf(i_formLvalue)
        outf(i_declname)
        outname(s_name, 0, "**RES**")
        translabels(h2!x)
        trans(h2!x, Ref)
        outf(i_return)
        UNLESS ssp=1 DO trnerr("SSP Error")
        outlabset(lab2, msp)
        ssp, msp := a, b
      }
      outlab(lab1)
      IF mode=Val DO outf(i_formRvalue)
      RETURN
    }

    CASE s_res:
      trans(h2!x, Ref)
      outf(s_res)
      ssp := ssp-1
      RETURN

    CASE s_goto:
      trans(h2!x, Val)
      outf(s_goto)
      ssp := ssp-1
      RETURN

    CASE s_cond:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      trans(h2!x, Val)
      outfl(i_jumpF, lab1)
      ssp := ssp-1
      trans(h3!x, mode)
      outfl(i_jump, lab2)
      outlab(lab1)
      ssp := ssp-1
      trans(h4!x, mode)
      outlab(lab2)
      RETURN
    }

    CASE s_while:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      outlab(lab2)
      trans(h2!x, Val)
      outfl(i_jumpF, lab1)
      ssp := ssp-1
      trans(h3!x, Val)
      outf(s_lose1)
      outfl(i_jump, lab2)
      outlab(lab1)
      outf(i_dummy)
      IF mode=Ref DO outf(i_formLvalue)
      RETURN
    }

    CASE s_ass:
    { LET len = length(h2!x)
      comline := h4!x
      trans(h2!x, Ref)
      trans(h3!x, Val)
      outfn(i_update, len)
      ssp := ssp-1
      IF mode=Ref DO outf(i_formLvalue)
      RETURN
    }

    CASE s_paren:
      translabels(h2!x)
      trans(h2!x, mode)
      RETURN

    CASE s_nil:
    CASE s_dummy:
    CASE s_true:
    CASE s_false:
    CASE s_sys:
    CASE s_jj:
      outf(op) // Bug ################### must convert s_ to i_
      upssp(1)
      IF mode=Ref DO outf(i_formLvalue)
      RETURN

    CASE s_name:
      outfname((mode=Val -> i_loadR, i_loadL), x)
      //outname(x)
      upssp(1)
      RETURN

    CASE Int:
      outfn(i_loadN, h2!x)
      upssp(1)
      RETURN

    CASE Real: 
      outfn(i_loadF, h2!x, h3!x)
      upssp(1)
      RETURN

    CASE s_stringconst:
      outf(i_loadS)
      outstring(x)
      upssp(1)
      IF mode=Ref DO outf(i_formLvalue)
      RETURN
  }
}

AND findlabels(x) = VALOF
{ IF x=0 RESULTIS 0
  SWITCHON h1!x INTO
  { DEFAULT:
      RESULTIS 0

    CASE s_colon:
    { LET lab = nextlab()
      h4!x := lab
      outfsl(i_decllabel, h2!x, lab)
      RESULTIS 1 + findlabels(h3!x)
    }

    CASE s_paren:
      RESULTIS findlabels(h2!x)

    CASE s_cond:
      RESULTIS findlabels(h3!x) +
               findlabels(h4!x)

    CASE s_while:
      RESULTIS findlabels(h3!x)

    CASE s_seq:
      RESULTIS findlabels(h2!x) +
               findlabels(h3!x)
  }
}

AND translabels(x) BE
{ LET n = findlabels(x)
  IF n DO outf(i_setlabEs, n)
}

AND transrhs(x) BE
{ IF x=0 RETURN

  SWITCHON h1!x INTO
  { DEFAULT:
      RETURN

    CASE s_and:
    { LET len = length(x)
      mapb(transrhs, x)
      outfn(Tuple, len)
      ssp := ssp - len + 1
      outf(i_formLvalue)
      RETURN
    }

    CASE s_valdef:
      trans(h3!x, Ref)
      RETURN

    CASE s_rec:
      outf(i_loadE)
      upssp(1)
      declguesses(h2!x)
      transrhs(h2!x)
      initnames(h2!x)
      loaddefinee(h2!x)
      outf(i_restoreE1)
      ssp := ssp-1
      RETURN

    CASE s_within:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      transrhs(h2!x)
      outfl(i_blocklink, lab1)
      IF ssp=msp DO msp := ssp+1
      { LET a, b = ssp, msp
        ssp, msp := 0, 1
        outfl(i_save, lab2)
        declnames(h2!x)
        transrhs(h3!x)
        outf(i_return)
        UNLESS ssp=1 DO trnerr("SSP error")
        outlabset(lab2, msp)
        ssp, msp := a, b
      }
      outlab(lab1)
      RETURN
    }
  }
}

AND declnames(x) BE
{ IF x=0 RETURN
  SWITCHON h1!x INTO
  { DEFAULT:
      trnerr("Bad bound variable list")
      RETURN

    CASE s_name:
      outfname(i_declname, x)
      //outname(x)
      ssp := ssp-1
      RETURN

    CASE s_comma:
      outfn(i_declnames, length(x))
      ssp := ssp-1
      mapf(outname, x)
      RETURN

    CASE s_and:
    { LET len = length(x)
      outfn(i_members, len)
      upssp(len-1)
      mapf(declnames, x)
      RETURN
    }

    CASE s_rec:
    CASE s_valdef:
      declnames(h2!x)
      RETURN

    CASE s_within:
      declnames(h3!x)
      RETURN

    CASE s_mpt:
      outf(i_testEmpty)
      ssp := ssp-1
      RETURN
  }
}

AND loaddefinee(x) BE
{ IF x=0 RETURN
  SWITCHON h1!x INTO
  { DEFAULT:
      trnerr("Bad definition")
      RETURN

    CASE s_name:
      outf(i_loadR); outname(x)
      upssp(1)
      outf(i_formLvalue)
      RETURN

    CASE s_and:
    CASE s_comma:
    { LET len = length(x)
      mapb(loaddefinee, x)
      outfn(Tuple, len)
      ssp := ssp - len + 1
      outf(i_formLvalue)
      RETURN
    }

    CASE s_rec:
    CASE s_valdef:
      loaddefinee(h2!x)
      RETURN

    CASE s_within:
      loaddefinee(h3!x)
      RETURN

  }
}

AND declguesses(x) BE
{ IF x=0 RETURN
  SWITCHON h1!x INTO
  { DEFAULT:
      trnerr("Bad definition")
      RETURN

    CASE s_name:
      outf(i_loadGuess)
      IF ssp=msp DO msp := ssp+1
      outf(i_declname); outname(x)
      RETURN

    CASE s_and:
    CASE s_comma:
      mapf(declguesses, x)
      RETURN

    CASE s_rec:
    CASE s_valdef:
      declguesses(h2!x)
      RETURN

    CASE s_within:
      declguesses(h3!x)
      RETURN
  }
}

AND initnames(x) BE
{ IF x=0 RETURN
  SWITCHON h1!x INTO
  { DEFAULT:
      trnerr("Bad definition")
      RETURN

    CASE s_name:
      outf(i_initname); outname(x)
      ssp := ssp-1
      RETURN

    CASE s_and:
    { LET len = length(x)
      outfn(i_members, len)
      upssp(len-1)
      outfn(i_initnames, len)
      mapf(initnames, x)
      RETURN
    }

    CASE s_comma:
    { LET len = length(x)
      outfn(i_initnames, len)
      ssp := ssp-1
      mapf(outname, x)
      RETURN
    }

    CASE s_rec:
    CASE s_valdef:
      initnames(h2!x)
      RETURN

    CASE s_within:
      initnames(h3!x)
      RETURN
  }
}

AND transscope(x, n, mode) BE
{ LET a, b = ssp, msp
  ssp, msp := 1, 1
  outfn(i_save, n)
  declnames(h2!x)
  translabels(h3!x)
  trans(h3!x, mode)
  outf(i_return)
  UNLESS ssp=1 DO trnerr("SSP error")
  outlabset(n, msp)
  ssp, msp := a, b
}

AND mapf(r, x) BE
{ // x -> [-, n, x1, x2,...xn]
  LET len = h2!x
  FOR i = 1 TO len DO r(x!(i+1))
}

AND mapb(r, x) BE
{ LET len = h2!x
  FOR i = len TO 1 BY -1 DO r(x!(i+1))
}

AND length(x) = h1!x=s_and | h1!x=s_comma -> h2!x, 1

AND upssp(x) BE
{ ssp := ssp+x
  IF ssp>msp DO msp := ssp
}

AND wrf(form, a, b, c) BE IF optCode DO writef(form, a, b, c)

AND outf(op) BE
{ wrf("%i5: %s*n", codep, opstr(op))
  putc(op)
}

AND outfname(op, x) BE
{ wrf("%i5: %s", codep, opstr(op))
  putc(op)
  outname(x)
}

AND outname(x) BE
{ LET name = @h3!x
  LET len = name%0
  //outfn(s_name, len)
  FOR i = 1 TO len DO putc(name%i)
  IF optCode DO writef(" %s*n", name)
}

AND outstring(x) BE
{ LET name = @h3!x
  LET len = name%0
  outfn(s_stringconst, len)
  FOR i = 1 TO len DO putc(name%i)
  IF optCode DO writef(" %s", name)
}

AND outfv(op, var) BE
{ wrf("%i5: %s %s*n", codep, opstr(op), var)
  putc(op); putc(var)
}

AND outn(a) BE
{ wrf("%i5: %n*n", codep, a)
  putc(a)
}

AND outfn(op, a) BE
{ wrf("%i5: %s %n*n", codep, opstr(op), a)
  putc(op); putc(a)
}

AND outfnn(op, a, b) BE
{ wrf("%i5: %s %n %n*n", codep, opstr(op), a, b)
  putc(op); putc(a); putc(b)
}

AND outfsl(op,a, b) BE
{ wrf("%i5: %s %s L%n*n", codep, opstr(op), a, b)
  putc(op); putc(a); putc(b)
}

AND outfl(op, lab) BE
{ wrf("%i5: %s L%n*n", codep, opstr(op), lab)
  putc(op); putref(lab)
}

AND outlab(lab) BE
{ wrf("%i5: Lab L%n %n*n", codep, lab, codep)
  setlab(lab, codep)
}

AND outlabset(lab, val) BE
{ wrf("%i5: Lab L%n %n*n", codep, lab, val)
  setlab(lab, val)
}

AND outentry(l1, l2) BE
{ wrf("%i5: Entry L%n L%n*n", codep, l1, l2)
  putref(l2)
  setlab(l1, codep)
}

AND outstatvec(lab, a) BE
{ wrf("%i5: Statvec L%n %n*n", codep, lab, a)
  setlab(lab, datap)
  FOR i = 0 TO a-1 DO putd(0)
}

AND outvar(lab) BE
{ wrf("%i5: Var L%n*n", codep, lab)
  setlab(lab, datap)
  putd(0)
}
 
AND putc(w) BE TEST codep>codet
               THEN trnerr("More code space needed")
               ELSE { codev!codep := w
                      codep := codep+1
                    }

AND putd(w) BE TEST datap>datat
               THEN trnerr("More data space needed")
               ELSE { datav!datap := w
                      datap := datap+1
                    }

AND putref(lab) BE TEST codep>codet
                   THEN trnerr("More code space needed")
                   ELSE { codev!codep := refv!lab
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
  WHILE p DO { LET np = codev!p
               codev!p, p := labval, np
             }
}

AND interpret() = VALOF
{ // Execute one or more SCED instructions

  // sp holds S
  // pc holds C
  // env holds E
  // dump holds D -> previous [S, E, C, D]
  // The above variables hold absolute addresses.
  
  // rega, regb // Working registers
  // count is decremented every time a Pocode instruction
  // is executed

  // codev hold the compiled Pocode
  // The start address is 1, so the initial setting of
  // pc is codev+1.
  // codev!0 is its upperbound.

  // The runtime data is held in the self expanding vector datasxv.
  // The actual data is in datav = datasxv!2. The upb of datav is
  // held in datasxv!0. Every time the vector is expanded both
  // datasxv!0 and datasxv!1 change and datav is updated.

  // Garbage collection is done by copying all the accessible data
  // in datav to a new self expanding vector which then replaces
  // datasxv. The zeroth word of every itemin datav is the length
  // of the item. This field is replaced by the negated position
  // of the copied item in the new self expanding vector. An item
  // with a negative length has already been copied and the negated
  // length field is the new location of the item.
  // The garbage collector updates stack, sp, env, dump, rega and
  // regb appropriately. Any of these variables having the value
  // zero are left unchanged.
  
  LET retcode = 0

  { // Start of main loop
    LET op = codev!pc                // Fetch next instruction

    IF optTrace DO
    { writef("%i5: %t8", pc, opstr(op))
      //IF hasOperand(op) DO writef(" %n", pc!1)
      newline()
    }
    IF count<=0 DO { retcode := 3; BREAK } // Zero count
    count := count-1
    pc := pc+1

    SWITCHON op INTO
    { DEFAULT:      writef("Unknown Pocode op: %s*n", opstr(op))
                    retcode := 1;    BREAK    // Unknown op code

      CASE i_halt:    BREAK
/*
      CASE Laddr:   sp := sp+1; sp!0 := !pc;       pc := pc+1; LOOP
      CASE Ln:      sp := sp+1; sp!0 := !pc;       pc := pc+1; LOOP
      CASE Lp:      sp := sp+1; sp!0 := pp!(!pc);  pc := pc+1; LOOP
      CASE Llp:     sp := sp+1; sp!0 := pp+!pc-mem;pc := pc+1; LOOP
      CASE Ll:      sp := sp+1; sp!0 := mem!(!pc); pc := pc+1; LOOP
      CASE Sp:      pp!(!pc) := sp!0; sp := sp-1;  pc := pc+1; LOOP
      CASE Sl:      mem!(!pc):= sp!0; sp := sp-1;  pc := pc+1; LOOP

      CASE s_apply: { LET opp, retaddr = pp, pc+1
                    pp, pc := pp+!pc, sp!0+mem
                    pp!0, pp!1, pp!2 := opp-mem, retaddr-mem, pc-mem
                    sp := pp+2
                    LOOP
                  }

      CASE Ret:     res := sp!0
                  { LET npp, npc = pp!0+mem, pp!1+mem
                    sp := pp-1
                    pp, pc := npp, npc
                    LOOP
                  }
      CASE s_neg:     sp!0 :=  -  sp!0;                      LOOP
      CASE s_not:     sp!0 := NOT sp!0;                      LOOP
      CASE s_mult:    sp := sp-1; sp!0 := sp!0  *  sp!1;     LOOP
      CASE s_div:     sp := sp-1; sp!0 := sp!0  /  sp!1;     LOOP
      CASE Mod:     sp := sp-1; sp!0 := sp!0 REM sp!1;     LOOP
      CASE Add:     sp := sp-1; sp!0 := sp!0  +  sp!1;     LOOP
      CASE s_minus:   sp := sp-1; sp!0 := sp!0  -  sp!1;     LOOP
      CASE s_eq:      sp := sp-1; sp!0 := sp!0  =  sp!1;     LOOP
      CASE s_ne:      sp := sp-1; sp!0 := sp!0 ~=  sp!1;     LOOP
      CASE s_le:      sp := sp-1; sp!0 := sp!0 <=  sp!1;     LOOP
      CASE s_ge:      sp := sp-1; sp!0 := sp!0 >=  sp!1;     LOOP
      CASE s_ls:      sp := sp-1; sp!0 := sp!0  <  sp!1;     LOOP
      CASE s_gr:      sp := sp-1; sp!0 := sp!0  >  sp!1;     LOOP
      CASE s_logand:  sp := sp-1; sp!0 := sp!0  &  sp!1;     LOOP
      CASE s_logor:   sp := sp-1; sp!0 := sp!0  |  sp!1;     LOOP
      CASE Jt:      sp := sp-1; pc := sp!1->!pc+mem,pc+1;  LOOP
      CASE Jf:      sp := sp-1; pc := sp!1->pc+1,!pc+mem;  LOOP
      CASE i_jump:    pc := !pc+mem;                         LOOP
      CASE s_res:     sp := sp-1; res := sp!1
      CASE s_sys:     sp := pp + !pc - 1
                    pc := pc+1
                    SWITCHON sp!1 INTO
                    { DEFAULT: writef("*nBad sys(%n,...) call*n", sp!1)
                               retcode  := 2;               BREAK   
                      CASE 0:  retcode  := sp!2;            BREAK
                      CASE 1:  res := interpret(sp!2, mem); LOOP
                      CASE 2:  optTrace := sp!2;            LOOP
                      CASE 3:  res := count; count := sp!2; LOOP
                    }
*/
    }
  } REPEAT

  RESULTIS retcode
}

AND printf(mem, form, p) BE
{ LET fmt = form+mem
  LET i = 0

  { LET k = fmt%i
    i := i+1
    IF k=0 RETURN
    IF k='%' DO
    { LET n = 0;
      { k := fmt%i
        i := i+1
        UNLESS '0'<=k<='9' BREAK
        n := 10*n + k - '0'
      } REPEAT
      SWITCHON k INTO
      { DEFAULT:  wrch(k); LOOP
        CASE 'd': writed  (!p,     n); p := p+1; LOOP
        CASE 's': wrs     (mem+!p, n); p := p+1; LOOP
        CASE 'x': writehex(!p,     n); p := p+1; LOOP
      }
    }
    wrch(k)
  } REPEAT
}

AND wrs(s, n) BE
{ LET len = 0
  WHILE s%len DO len := len+1
  FOR i = len+1 TO n DO wrch(' ')
  FOR i = 0 TO len-1 DO wrch(s%i)
}

AND hasOperand(op) = VALOF SWITCHON op INTO
{ //CASE Fnrn:CASE Rtrn:CASE Lres:CASE i_halt:
  //CASE Vecap:CASE Ind:CASE Stind:CASE s_neg:CASE s_not:
  //CASE s_mult:CASE s_div:CASE Mod:CASE s_plus:CASE s_minus:
  //CASE s_eq:CASE s_ne:CASE s_le:CASE s_ge:CASE s_ls:CASE s_gr:
  //CASE Lsh:CASE Rsh:CASE s_and:CASE Or:CASE Xor:
            RESULTIS FALSE

  DEFAULT:  RESULTIS TRUE
}

AND lvofname() = 0

LET execpal(regs, mem) = VALOF
{ // Execute one or more SCED instructions
  // The PAL memory is held the self expanding vector: palsxv
  // palsxv -> [vupb, v]
  // v -> [upb, S, C, E, D, ...]
  
  // sp   holds S
  // pc   holds C
  // env  holds E -> [link, name, value]
  // dump holds D -> previous [S, E, C, D]
  // count is the count of instruction executed

  // Garbage collectonis done by copying the reachable elements of palsxv
  // into a new self expanding vector. The freeing the old one and replace
  // palsxv with the new one.
  LET retcode = 0

  sawritef("execpal: codev=%n pc=%n*n", codev, pc)
  
  { // Start of main loop
    LET op = codev!pc                // Fetch next instruction
    
    IF optTrace DO
    { writef("%i5: count=%n %t8", pc, count, opstr(op))
      IF hasOperand(op) DO writef(" %n", codev!(pc+1))
      newline()
      abort(6464)
    }
    IF count<=0 DO { retcode := 3; BREAK } // Zero count
    count := count-1
    pc := pc+1

    SWITCHON op INTO
    { DEFAULT:      retcode := 1;    BREAK    // Unknown op code

      CASE i_setup:   // Ln       Initialise the runtime system
	oldc := 0 ///LV r_finish  pc=0 causes Finish to be executed
	stack := list(5, t_stack, 4, dummyrv, 0)
	rega := list(3, t_lvalue, env)
	env := 0
	stackp := 5
	save()
	split1()
        BREAK

      CASE i_halt:    BREAK
      
      CASE i_loadL:   // x        Load the Lvalue of variable x
	pc := pc+1
	writef("Calling lvofname*n"); abort(1001)
	rega := lvofname(pc!1, env)
	TEST rega=nilrv
	THEN {	rega := list(3, t_lvalue, rega)
		errokdbg()
	     }
	ELSE next11()
        BREAK

      CASE i_loadR:   // x        Load the Rvalue of variable x
	pc := pc+1
	writef("Calling lvofname*n"); abort(1001)
	rega := lvofname(pc!1, env)
	TEST rega=nilrv
	THEN errdbg()
	ELSE { rega := h3!rega
	       next11()
	     }
        BREAK

      CASE i_loadE:   //          Load E
	rega := env
	next11()
        BREAK

      CASE i_loadS:   // s        Load string s
      { LET v = VEC 200
	LET i = 0
	unpackstring(pc!1, v)
	i := v!0
	rega := nilsrv
	WHILE i > 0 DO
	{ rega := list(4, t_string, rega, v!i)
	  i:=i-1
	}
	pc := pc+1
	next11()
        BREAK
      }
	
      CASE i_loadN:   // val      Load an integer
	rega := list(3, pc!1, pc!2)
	pc := pc+2
	next11()
        BREAK

      CASE i_loadF:   // man exp  Load float man * 10^exp
        BREAK

      CASE i_loadJ:   //          Load [C, S, E] to become the value
                      //          of *res* in the translation of valof
	rega := list(5, t_jj, h4!stack, h5!stack, h6!stack )
	next11()
        BREAK

      CASE i_sys:     //          Load Sys
        BREAK

      CASE i_blocklink:  // Ln    Place Nil on the top of the stack and
                         //       make it seem as if a parameterless
		         //       function with this block as its body
		         //       is being called.
        BREAK

      CASE i_reslink:    // Ln    Create a new Lvalue with contents Nil
                         //       and push it onto th stack. The execute
                         //       Blocklink Ln.
	stack!(stackp) := list(3, t_lvalue, nilrv)
	stackp := stackp+1
	blocklink()
        BREAK

      CASE i_declname:   // x     Add an Env node to the environment for 
                         //       name x giving it the Lvaue from the top
		         //       of the stack.

      CASE i_declnames:  // n x1.. xn Declare n names
        BREAK

      CASE i_decllabel:  // x Ln  Add and Env node for name x giving it a
                         //       new Lvalue holding {Ln, E]. The environment
		         //       field will be filled in later by a call
		         //       of SetlabEs.
        BREAK

      CASE i_setlabEs:   // n      Set the Environment fields of the
                         //        n labels just added to the environment.
        BREAK

      CASE i_initnames:  // n x1..xn
        BREAK
	
      CASE i_restoreE1:  //
	env := stack!(stackp-2)
	stackp := stackp-1
	stack!(stackp-1) := stack!(stackp)
	pc := pc+1
        BREAK

      CASE i_true:
	rega := truerv
	next11()
        BREAK
	
      CASE i_false:
	rega := falserv
	next11()
        BREAK
	
      CASE i_loadGuess:
	rega := guessrv
	nextlv11()
        BREAK
	
      CASE i_nil:
	rega := nilrv
	nextlv11()
        BREAK
	
      CASE i_dummy:
	rega := dummyrv
	nextlv11()
        BREAK
	
      CASE i_formClosure:
	rega := list(4, t_closure, env, pc!1 )
	pc := pc+1
	next11()
        BREAK
	
      CASE i_formLvalue:
	rega := list(3, t_lvalue, stack!(stackp-1))
	stack!(stackp-1) := rega
	pc := pc+1
        BREAK

      CASE i_formRvalue:
	stack!(stackp-1) := h3!(stack!(stackp-1))
	pc := pc+1
        BREAK

      CASE i_tuple:
      { LET n = pc!1
	rega := node(n+3)
	rega!0, rega!1, areg!2 := n+3, t_tuple, n
	FOR i = 3 TO n+2 DO
          stackp, rega!i := stackp-1, stack!(stackp)
	pc := pc+1
	next11()
        BREAK
      }
      
      CASE i_members:    // n     The top element of the stack is assumed
                         //       to be a tuple with n elements. These are
		         //       loaded onto the stack. This is only used
		         //       in the compilation of simultneous
		         //       assignments.
      { LET n = pc!1
	split1()
	regb := h3!rega
	FOR i = -2 TO n-3 DO
	{ stack!(stackp) := regb!(n-i)
	  stackp := stackp+1
	}
	pc := pc+2
        BREAK
      }

      CASE s_not:
	split1()
	IF h2!rega=t_false DO
	{ a := truerv
	  next11()
	  RETURN
	}
	TEST h2!rega=t_true
	THEN { rega := falserv
	       next11()
	     }
	ELSE { error1("NOT", rega, 0)
	       errdbg()
	     }
        BREAK

      CASE i_logand:
	split2()
	TEST testbools2()
	THEN { rega := h2!a=t_true -> regb, falserv
	       next11()
	     }
	ELSE { error1("&", rega, regb)
	       rega := falserv
	       errdbg()
	     }
        BREAK

      CASE i_logor:
	split2()
	TEST testbools2()
	THEN { rega := h2!rega=t_false -> regb, truerv
	       next11()
	     }
	ELSE { error1("OR", rega, regb)
	       rega := falserv
	       errdbg()
	     }
        BREAK

      CASE i_aug:
	split2()
	UNLESS h2!rega=t_tuple DO
	{ error1("AUG", rega, regb)
	  rega := nilrv
	  errdbg()
	  RETURN
	}
        { LET n = h3!rega
	  LET t = node(n+4)
	  t!0, t!1, t!2, t!(n+3) := n+4, t_tuple, n+1, regb
	  FOR i = 3 TO n+2 DO t!i := rega!i
	  rega := t
	  next11()
          BREAK
	}

      CASE i_result:
	rega := lvofname(nameres, env)
	IF a=nilrv DO
	{	rega := list(3, t_lvalue, rega)
		GOTO reserr }
	rega := h3!rega
	UNLESS h2!rega=t_jj DO
reserr:	{	error("INCORRECT USE OF RES", 0, 0, 0)
		errokdbg()
		RETURN }
	h4!stack, h5!stack, h6!stack := h3!rega, h4!rega, h5!rega
	return()
        BREAK

      CASE i_mult:
{	LET t = rega
	split2()
	IF testnumbs2()=t_number DO
	{	rega := list(3, t_number, h3!rega*h3!regb )
		next11()
		RETURN }
	IF testnumbs2()=t_real DO
	{	rega := list(3, t_real, fmult(h3!rega, h3!regb) )
		IF floterr DO {	writes("*nOVERFLOW:")
					floterr := FALSE
					GOTO fmuerr }
		next11()
		RETURN }
	rega := list(3, t_number, 0)
fmuerr:	error1("**", t, regb)
	errdbg() }
        BREAK

      CASE i_div:
{	LET t = rega
	split2()
	IF testnumbs2()=t_number DO
	{	IF h3!regb=0 GOTO derr
		rega := list(3, t_number, h3!rega/h3!regb )
		next11()
		RETURN }
	IF testnumbs2()=t_real DO
	{	rega := list(3, t_real, fdiv(h3!rega, h3!regb) )
		IF floterr DO
		{	UNLESS feq(h3!regb, 0) DO writes("*nOVERFLOW:")
			floterr := FALSE
			GOTO derr }
		next11()
		RETURN }
derr:	a := list(3, t_number, 0)
	error1("/", t, regb)
	errdbg() }
        BREAK

      CASE i_plus:
{	LET t = rega
	split2()
	IF testnumbs2()=t_number DO
	{	rega := list(3, t_number, h3!rega+h3!regb )
		next11()
		RETURN }
	IF testnumbs2()=t_real DO
	{	rega := list(3, t_real, fadd(h3!rega, h3!regb) )
		IF floterr DO {	writes("*nOVERFLOW:")
					floterr := FALSE
					GOTO fperr }
		next11()
		RETURN }
	a := list(3, t_number, 0)
fperr:	error1("+", t, regb)
	errdbg() }
        BREAK

      CASE i_minus:
{	LET t = rega
	split2()
	IF testnumbs2()=t_number DO
	{	rega := list(3, t_number, h3!rega-h3!regb )
		next11()
		RETURN }
	IF testnumbs2()=t_real DO
	{	rega := list(3, t_real, fsub(h3!rega, h3!regb) )
		IF floterr DO {	writes("*nOVERFLOW:")
					floterr := FALSE
					GOTO fmerr }
		next11()
		RETURN }
	rega := list(3, t_number, 0)
fmerr:	error1("-", t, regb)
	errdbg() }
        BREAK

      CASE i_power:
{	LET t = rega
	split2()
	UNLESS h2!regb=t_number GOTO pwerr
	IF h2!a=t_number DO
	{	LET base, exp, r = h3!rega, h3!regb, 1
		TEST exp <= 0
		THEN {	IF base=0 GOTO pwerr
			r := ABS base = 1 ->
			((-exp & 1)=0 -> 1, base), 0 }
		ELSE UNTIL exp=0 DO
		{	UNLESS (exp & 1)=0 DO r := r * base
			base := base * base
			exp := exp RSHIFT 1 }
		rega := list(3, t_number, r)
		next11()
		RETURN }
	IF h2!rega=t_real DO
	{	rega := list(3, t_real, fpower(h3!rega, h3!regb) )
		IF floterr DO {	writes("*nOVERFLOW:")
					floterr := FALSE
					GOTO pwerr }
		next11()
		RETURN }
pwerr:	rega := list(3, t_number, 0)
	error1("****", t, regb)
	errdbg() }
        BREAK

      CASE i_pos:
{	split1()
	TEST h2!rega=t_number LOGOR h2!rega=t_real
	THEN {	rega := list(3, h2!rega, h3!rega )
		next11() }
	ELSE {	error1("+", rega, 0)
		rega := list(3, t_number, 0)
		errdbg() }}
        BREAK

      CASE i_neg:
{	LET t=rega
	split1()
	IF h2!rega=t_number DO
	{	rega := list(3, t_number, -h3!rega )
		next11()
		RETURN }
	IF h2!rega=t_real DO
	{	rega := list(3, t_real, fumin(h3!rega) )
		next11()
		RETURN }
	rega := list(3, t_number, 0)
	error1("-", t, 0)
	errdbg() }
        BREAK

      CASE s_eq:
{	LET t=rega
	split2()
	rega := equal(rega, regb) -> truerv, falserv
	TEST errflag
	THEN {	error1("EQ", t, regb)
		errflag := FALSE
		errdbg() }
	ELSE next11() }
        BREAK

      CASE i_ne:
{	LET t=rega
	split2()
	rega := equal(rega, regb) -> falserv, truerv
	TEST errflag
	THEN {	error1("NE", t, regb)
		rega := falserv
		errflag := FALSE
		errdbg() }
	ELSE next11() }

      CASE i_ls:
{	split2()
	IF testnumbs2()=t_number DO
	{	rega := h3!rega < h3!regb -> truerv, falserv
		next11()
		RETURN }
	IF testnumbs2()=t_real DO
	{	rega := fls(h3!rega, h3!regb) -> truerv, falserv
		next11()
		RETURN }
	error1("LS", rega, regb)
	rega := falserv
	errdbg() }
        BREAK

      CASE i_le:
{	split2()
	IF testnumbs2()=t_number DO
	{	rega := h3!rega <= h3!regb -> truerv, falserv
		next11()
		RETURN }
	IF testnumbs2()=t_real DO
	{	rega := fle(h3!rega, h3!regb) -> truerv, falserv
		next11()
		RETURN }
	error1("LE", rega, regb)
	rega := falserv
	errdbg() }
        BREAK

      CASE i_ge:
{	split2()
	IF testnumbs2()=t_number DO
	{	rega := h3!rega>=h3!regb -> truerv, falserv
		next11()
		RETURN }
	IF testnumbs2()=t_real DO
	{	rega := fge(h3!rega, h3!regb) -> truerv, falserv
		next11()
		RETURN }
	error1("GE", rega, regb)
	rega := falserv
	errdbg() }
        BREAK

      CASE i_gr:
{	split2()
	IF testnumbs2()=t_number DO
	{	rega := h3!rega > h3!regb -> truerv, falserv
		next11()
		RETURN }
	IF testnumbs2()=t_real DO
	{	rega := fgr(h3!rega, h3!regb) -> truerv, falserv
		next11()
		RETURN }
	error1("GR", rega, regb)
	rega := falserv
	errdbg() }
        BREAK

      CASE i_jump:       // Ln     Jump to label Ln
        pc := pc!1
        BREAK

      CASE i_jumpF:      // Ln     Pop a value from the stck and jump to
                       //        label Ln if it was False
	split1()
	IF h2!rega = t_false DO
	{	pc := pc!1
		RETURN }
	IF h2!rega = t_true DO
	{	pc := pc+2
		RETURN }
	error("NOT A TRUTHVALUE: ", a, 0, 0)
	pc := pc!1 - 1
	edbg()
        BREAK







      CASE i_apply:      //       Apply a tuple, a Sys node or a Closure
	split1()
	rega := h3!rega
	SWITCHON h2!rega INTO
	{	CASE t_closure:
				stackp := stackp+1
				oldc, pc := pc+1, h4!rega
				RETURN
		CASE t_tuple:
				stackp, rb := stackp-1, stack!(stackp)
				regb := h3!rb
				UNLESS h2!regb=t_number DO
				{	error(0, rega, " APPLIED TO ", regb)
					UNLESS h3!rega=0 DO rega := h4!rega
					errlvdbg()
					RETURN }
			{	LET n = h3!regb
				TEST 1 <= n <= h3!rega
				THEN {	rega := rega!(n+2)
					next11() }
				ELSE {	error(0, rega, " APPLIED TO ", regb)
					UNLESS h3!rega=0 DO
						TEST n >= 1
						THEN rega := rega!(h3!rega+2)
						ELSE rega := h4!rega
					errlvdbg() }
				RETURN }
		CASE t_basicfn:
				(h3!rega)()
				RETURN

		DEFAULT:	error("ATTEMPT TO APPLY ",rega," TO ",stack!(stackp-1))
				edbg() }
        BREAK
 
      CASE i_save:       // Ln    Create a new stack node of size Ln+6
                       //       with appropriate return link info.
	regb := node(pc!1+6)
	h1!regb, h2!regb := pc!1+6, stack
	h3!stack := stackp
	h4!regb, h5!regb := oldc, stack
	h6!regb, h7!regb := env, stack!(stackp-2)
	env := h3!rega
	stackp, stack := 7, b
	pc := pc+2
        BREAK

      CASE i_return:  //          Return from the current block
	rega := stack!(stackp-1)
	restart()
	stackp := stackp-1
	stack!(stackp-1) := rega
        BREAK

      CASE i_testEmpty:  //       Check that the argument of a parameterless
                       //       function is indeed Nil.
        split1()
	TEST h3!rega=nilrv
	THEN pc := pc+1
	ELSE {	error1("FUNCTION OF NO ARGUMENTS", rega, 0)
		edbg() }
        BREAK

      CASE i_lose1:      //        Pop one item from the stack
	split1()
	pc := pc+1
        BREAK

      CASE i_goto:       // Ln     Jump to label Ln
	split1()
	UNLESS h2!rega=t_label DO
	{ error("CANNOT GO TO ", rega, 0, 0)
	  rega := dummyrv
	  errdbg()
	  RETURN
	}
	pc, env := h4!rega, h6!rega
	stack := node(h3!rega)
	stackp := 6
	h1!stack, h2!stack := h3!rega, stack
	rega := h5!rega
	h4!stack, h5!stack, h6!stack := h4!rega, h5!rega, h6!rega
        BREAK

      CASE i_update:     // n      Update n Lvalues
      { LET n = pc!1
	split2()
	TEST n = 1
	THEN h3!b := rega
	ELSE { UNLESS h2!rega = t_tuple & h3!rega = n DO
	       { error("CONFORMALITY ERROR IN ASSIGNMENT",0,0,0)
		 writes("THE VALUE OF THE RHS IS: ")
		 printa(rega, tupledepth)
		 writechar(output, '*n')
		 writes("THE NUMBER OF VARIABLES ON THE LHS IS: ")
		 writen(n)
		 writechar(output, '*n')
		 pc := pc+1
		 rega := dummyrv
		 errdbg()
		 RETURN
	       }
	       regb := h3!regb
	       { LET v = VEC 100
		 FOR i=3 TO n+2 DO v!i := h3!(rega!i)
		 FOR i=3 TO n+2 DO h3!(regb!i) := v!i
	       }
	     }
	rega := dummyrv
	pc := pc+1
	next11()
        BREAK
      }
    }
  } REPEAT

  RESULTIS retcode
}

AND save() BE
{ regb := node(pc!1+6)
  h1!regb, h2!regb := pc!1+6, stack
  h3!stack := stackp
  h4!regb, h5!regb := oldc, stack
  h6!regb, h7!regb := env, stack!(stackp-2)
  env := h3!rega
  stackp, stack := 7, regb
  pc := pc+2
}

AND split1() BE
{ writef("split1: Called*n")
}


AND edbg() BE
{ restartpc := pc+1
  pc := @restart
  rega := list(3, t_lvalue, nilrv)
  comdbg()
}

AND errdbg() BE
{ restartpc := pc+1
  pc := @rvrestart
  rega := list(3, t_lvalue, rega)
  comdbg()
}


AND errlvdbg() BE
{ rega := list(3, t_lvalue, rega)
  errokdbg()
}

AND list(n, a, b, c, d, e, f,g) = VALOF
{ LET p = newvec(n)
  //IF FALSE DO
  { writef("list: n=%n", n)
    IF a DO writef(" a=%i6", a)
    IF b DO writef(" b=%i6", b)
    IF c DO writef(" c=%i6", c)
    IF d DO writef(" d=%i6", d)
    IF e DO writef(" e=%i6", e)
    IF f DO writef(" f=%i6", f)
    IF g DO writef(" g=%i6", g)
    newline()
  }
  
  UNLESS p DO
  { writef("list: More space needed*n")
    abort(999)
  }
  SWITCHON n INTO
  { DEFAULT:  writef("list: Bad n=%n*n",n)
    CASE 7: p!6 := f
    CASE 6: p!5 := e
    CASE 5: p!4 := d
    CASE 4: p!3 := c
    CASE 3: p!2 := b
    CASE 2: p!1 := a
    CASE 1: p!0 := n 
  }
}

AND errokdbg() BE
{ restartpc := pc+1
  pc := @okrestart
  comdbg()
}

AND comdbg() BE
{ h3!stack := stackp
  regb := node(8)
  h1!regb, h2!regb := 8, stack
  h4!regb, h5!regb := restartc, stack
  h6!regb, h7!regb := env, a
  stack := regb
  regb := h3!errorlv
  stackp := 7
  errct := errct + 1
  IF errct >= maxerr DO pc := 0 ////LV norestart
  UNLESS h2!regb = t_closure | h2!regb=t_basicfn DO
  { UNLESS errct >= maxerr DO
      writes("EXECUTION RESUMED*n*n")
    RETURN
  }
  TEST h2!regb=t_closure
  THEN { stack!(stackp) := errorlv
	 stackp := stackp+1
	 rega := regb
	 oldc, pc := pc, h4!regb }
  ELSE { pc := pc-3
	 nil()
	 formLvalue()
	 (h3!regb)() }
	 restartc := 0
       }

AND okrestart() BE
{ rega := stack!(stackp-1)
  restart()
  stack!(stackp) := rega
  stackp := stackp+1
}

AND rvrestart() BE
{ rega := stack!(stackp-1)
  restart()
  stack!(stackp) := h3!rega
  stackp := stackp+1
}

AND norestart() BE
{ writes("*nMAXIMUM NUMBER OF RUN-TIME ERRORS REACHED*n")
  terminate1()
}


AND printf(mem, form, p) BE
{ LET fmt = form+mem
  LET i = 0

  { LET k = fmt%i
    i := i+1
    IF k=0 RETURN
    IF k='%' DO
    { LET n = 0;
      { k := fmt%i
        i := i+1
        UNLESS '0'<=k<='9' BREAK
        n := 10*n + k - '0'
      } REPEAT
      SWITCHON k INTO
      { DEFAULT:  wrch(k); LOOP
        CASE 'd': writed  (!p,     n); p := p+1; LOOP
        CASE 's': wrs     (mem+!p, n); p := p+1; LOOP
        CASE 'x': writehex(!p,     n); p := p+1; LOOP
      }
    }
    wrch(k)
  } REPEAT
}

AND wrs(s, n) BE
{ LET len = 0
  WHILE s%len DO len := len+1
  FOR i = len+1 TO n DO wrch(' ')
  FOR i = 0 TO len-1 DO wrch(s%i)
}

AND hasOperand(op) = VALOF SWITCHON op INTO
{ //CASE Fnrn:CASE Rtrn:CASE Lres:CASE i_halt:
  //CASE Vecap:CASE Ind:CASE Stind:CASE s_neg:CASE s_not:
  //CASE s_mult:CASE s_div:CASE Mod:CASE s_plus:CASE s_minus:
  //CASE s_eq:CASE s_ne:CASE s_le:CASE s_ge:CASE s_ls:CASE s_gr:
  //CASE Lsh:CASE Rsh:CASE s_and:CASE Or:CASE Xor:
            RESULTIS FALSE

  DEFAULT:  RESULTIS TRUE
}


// The rest is taken from com/pal70.b from about June 1970

/*
########## UNDER EARLY STAGE OF DEVELOPMENT #############

This is a compiler and interpreter for the language PAL
implemented in BCPL.

(c) Martin Richards 21 Oct 2010

Usage:

pal  "PROG/A,TO=-o/K,TOKENS=-l/S,TREE=-p/S,CODE=-c/S,TRACE=-t/S"

   PROG   gives the filename of the PAL program to run, eg test.pal
-o TO     gives the filename of the output
-l TOKENS is a switch to test the lexical analyser
-p TREE   causes the parse tree to be output
-c CODE   outputs the compiled blackboard evaluator code
-t TRACE  Traces the execution of the blackboard evaluator.

21/10/2010
Started modifying this compiler to make it follow the syntax and
POCODE of the Pal70 compiler whose original compiler listing is in
doc/pal70-mabee-17jun70.pdf

08/07/2010
Started to modify lex and syn to agree with the PAL syntax specified
in Appendix 2.1 (dated 02/17/68) with the following minor extensions.

The operators ~=, <= and >= are included.
( and [ are synonyms as are ) and ].
-> and -* are synonyms.
~ and not are synonyms.

14/06/2010
Lex more or less complete, now working on the syntax analyser.

09/06/2010
Started re-implementation of PAL based on VSPL.

*/

/*
GET "libhdr"
 
MANIFEST {
// Selectors
h1=0; h2=1; h3=2; h4=3; h5=4; h6=5

// Syntactic operators
s_pling=1; s_eof; s_where; s_dot
s_lparen; s_rparen; s_in; s_percent
s_ifso; s_ifnot; s_do

// AE Tree nodesTest
s_def; s_let; s_lambda; s_valof; s_test s_if; s_while; s_ass
s_seq; s_colon
s_noshare; s_cond
s_comma; s_valdef
s_rec; s_and; s_within
s_mpt; s_paren

// AE nodes and POCODE symbols
s_goto; s_res
s_not; s_dummy; s_nil; s_stringconst; s_name
s_true; s_false; s_jj; s_sys
s_plus; s_minus
s_aug; s_logor; s_logand
s_eq; s_ne; s_ls; s_le; s_gr; s_ge
s_mult; s_div; s_power
s_pos; s_neg; s_apply

// POCODE symbols
i_loadL; i_loadR; i_loadE; i_loadS; i_loadN; i_loadF; i_loadJ
i_restoreE1; i_loadGuess
i_formClosure; i_formLvalue; i_formRvalue
i_members
i_jump; i_jumpF; i_save; i_return
i_testEmpty; s_lose1; i_update
i_declname; i_declnames; i_initname; i_initnames
i_decllabel; i_setlabEs; i_blocklink; i_reslink
i_setup; i_halt
i_lab;
i_dummy; i_true; i_false; i_sys; i_jj

//i_param; s_equ

// AE nodes, POCODE symbols and run-time node types
t_dummy; t_jj; t_true; t_false; Int; Real; t_sys
t_lvalue; t_string
Number; Tuple; t_stack


// Translation symbols
Val=0; Ref

// Library functions
Sys_isboolean=1
Sys_isstring
Sys_isfunction
Sys_isprogramclosure
Sys_islabel
Sys_istuple
Sys_isreal
Sys_isinteger
Sys_stem
Sys_stern
Sys_conc
Sys_itor
Sys_rtoi
Sys_stoi
Sys_lookupinj
Sys_order
Sys_print
Sys_readch
Sys_atom
Sys_null
Sys_share
}
 
GLOBAL { 
rec_p:ug; rec_l; fin_p; fin_l
fatalerr; synerr; trnerr; errcount; errmax
progstream; tostream
mk1; mk2; mk3; mk4; mk5; mk6
newvec; treep; treevec
optTokens; optTree; optCode; optTrace

// Globals used in LEX
chbuf; charv; ch; rch; lex
token; lexval; exponent; wordnode
nilnode; truenode; falsenode; dummynode; mptnode
wrchbuf; chcount; lineno
dsw; declsyswords; namestart; nametable; lookupword
rdnumber; rdstrch; rdtag

// Globals used in SYN
checkfor; rdprog
rdnamelist; rname
rdnbdef; rbdef; rndef; rdef
formtree; plist
rnexp; rexp; rnbexp; rbexp
rncom; rcom; rbcom

// Globals used in TRN and the interpreter
 
trnext:300; trprog; trdef; trans
findlabels; translabels; transrhs
loaddefinee; declguesses; initnames; transscope
mapb; mapf; length; upssp
trcom; decldyn
declstatnames; checkdistinct; addname; cellwithname
trdecl; jumpcond
assign; load; fnbody; loadlist; transname
dvec; dvece; dvecp; dvect
comline; procname; resultlab; ssp; msp
outf; outname; outstring; outfv; outfn; outfsl
outfnn; outfl; outfs; outentry
outlab; outlabset; outvar; outstatvec; outstring
opstr; hasOperand
pc; sp; env; dump; count
rega; regb
oldc
codev; codep; codet
datav; datap; datat
stack; stackp; stackt
nilrv; nilsrv; dummyrv; 
labv; refv; labmax; putc; putd; putref
setlab; setlabval; nextlab; labnumber; resolvelabels
interpret
next11
}

MANIFEST {                         //  Selectors
nametablesize = 541
c_tab         =   9
c_newline     =  10
}
 
LET start() = VALOF
{ LET treesize = 0
  AND codesize = 0
  AND datasize = 0
  AND argv = VEC 50
  AND argform =
        "PROG/A,TO=-o/K,TOKENS=-l/S,TREE=-p/S,CODE=-c/S,TRACE=-t/S"
  LET stdout = output()

  errmax   := 2
  errcount := 0
  fin_p, fin_l := level(), fin

  treevec, labv, refv := 0, 0, 0
  progstream, tostream := 0, 0
   
  writef("*nPAL (05 Mar 2024)*n")
 
  IF rdargs(argform, argv, 50)=0 DO fatalerr("Bad arguments*n")

  treesize := 10000
  codesize := 50000
  datasize :=  5000

  progstream := findinput(argv!0)      // PROG

  IF progstream=0 DO fatalerr("Trouble with file %s*n", argv!0)

  selectinput(progstream)
 
  IF argv!1                            // TO      -o
  DO { tostream := findoutput(argv!1)
       IF tostream=0 DO fatalerr("Trouble with code file %s*n", argv!1)
     }

  optTokens := argv!2                  // TOKENS  -l
  optTree   := argv!3                  // TREE    -p
  optCode   := argv!4                  // CODE    -c
  optTrace  := argv!5                  // TRACE   -t

  treevec := getvec(treesize)
  codev   := getvec(codesize)
  codev!0 := codesize
  codep   := 1  // The start address
  codet   := codesize

  datav   := getvec(datasize)
  datap   := 0
  datat   := datasize

  labv := getvec(1000)
  refv := getvec(1000)
  labmax := 1000

  UNLESS treevec & codev & datav & labv & refv DO
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

    IF optTree DO { writes("*nParse Tree*n*n")
                    plist(tree, 0, 20)
                    newline()
                  }
  
    IF errcount GOTO fin

    FOR i = 0 TO codet DO codev!i := 0
    FOR i = 0 TO datat DO datav!i := 0

    trprog(tree)                    // Translate the tree

    IF errcount GOTO fin

    // Set the initial CSED machine state
    sp := 0           // sp
    pc := 0           // pc
    env := 0          // env
    dump := 0         // dump
    count := maxint   // count

 
    writef("*nStarting the interpreter*n*n")

    { LET ret = interpret()   // Execute the interpreter
      IF ret DO writef("Return code %n*n", ret)
      writef("*nInstructions executed: %n*n", maxint-count)
    }
  }
   
fin:
  IF treevec       DO freevec(treevec)
  IF labv          DO freevec(labv)
  IF refv          DO freevec(refv)
  IF progstream    DO { selectinput(progstream); endread()  }
  IF tostream      DO { selectoutput(tostream)
                        UNLESS tostream=stdout DO  endwrite() }

  selectoutput(stdout)
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
                token := rdnumber()
                RETURN
 
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
 
    CASE '[':
    CASE '(': token := s_lparen;    BREAK
    CASE ']':
    CASE ')': token := s_rparen;    BREAK 
    CASE '%': token := s_percent;   BREAK 
    CASE '+': token := s_plus;      BREAK
    CASE ',': token := s_comma;     BREAK
    CASE '&': token := s_logand;    BREAK
    CASE '!': token := s_pling;     BREAK
    CASE '=': token := s_eq;        BREAK
    CASE '^': token := s_power;     BREAK
    CASE ';': token := s_seq;       BREAK
    CASE '$': token := s_noshare;   BREAK
    CASE '.': token := s_dot;       BREAK
 
    CASE '**':  rch()
                IF ch='**' DO { token := s_power;  BREAK }
                token := s_mult
                RETURN

    CASE '/':   rch()
                IF ch='/' DO
                { rch() REPEATUNTIL ch='*n' | ch=endstreamch
                  LOOP
                }
                token := s_div
                RETURN
 
    CASE '<':   rch()
                IF ch='=' DO { token := s_le;  BREAK }
                token := s_ls
                RETURN

    CASE '>':   rch()
                IF ch='=' DO { token := s_ge;  BREAK }
                token := s_gr
                RETURN

    CASE '~':   rch()
                IF ch='=' DO { token := s_ne;  BREAK }
                token := s_not
                RETURN

    CASE '-':   rch()
                IF ch='>' | ch='**' DO { token := s_cond; BREAK }
                token := s_minus
                RETURN

    CASE ':':   rch()
                IF ch='=' DO { token := s_ass;  BREAK }
                token := s_colon
                RETURN
 
    CASE '*'': // A string constant
              { LET len = 0
                rch()
 
                UNTIL ch='*'' DO
                { IF len=255 DO synerr("Bad string constant")
                  len := len + 1
                  charv%len := rdstrch()
                }
 
                charv%0 := len
                wordnode := newvec(len/bytesperword+2)
                h1!wordnode := s_stringconst
                FOR i = 0 TO len DO (@h2!wordnode)%i := charv%i
                token := s_stringconst
                BREAK
              }
 
    DEFAULT:    UNLESS ch=endstreamch DO
                { LET badch = ch
                  ch := '*s'
                  synerr("Illegal character %x2", badch)
                }
                token := s_eof
                RETURN
  } REPEAT
 
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
    h1!wordnode, h2!wordnode := s_name, nametable!hashval
    FOR i = 0 TO len DO (@h3!wordnode)%i := word%i
    nametable!hashval := wordnode
  }
  RESULTIS h1!wordnode
}
 
AND dsw(word, tok) BE { lookupword(word); h1!wordnode := tok  }
 
AND declsyswords() BE
{ 
  dsw("and", s_and)
  dsw("aug", s_aug)
  dsw("def", s_def)
  dsw("do", s_do)
  dsw("dummy", s_dummy)
  dummynode := wordnode
  dsw("else", s_ifnot)
  dsw("eq", s_eq)
  dsw("false", s_false)
  falsenode := wordnode
  dsw("fn", s_lambda)
  dsw("ge", s_ge)
  dsw("goto", s_goto)
  dsw("gt", s_gr)
  dsw("if", s_if)
  dsw("ifnot", s_ifnot)
  dsw("ifso", s_ifso)
  dsw("in", s_in)
  dsw("jj", s_jj)
  dsw("le", s_le)
  dsw("let", s_let)
  dsw("ll", s_lambda)
  dsw("logand", s_logand)
  dsw("lt", s_ls)
  dsw("ne", s_ne)
  dsw("nil", s_nil)
  nilnode := wordnode
  dsw("not", s_not)
  dsw("or", s_logor)
  dsw("rec", s_rec)
  dsw("res", s_res)
  ///dsw("resultis", s_res)
  dsw("sys", s_sys)
  dsw("test", s_test)
  dsw("then", s_ifso)
  dsw("true", s_true)
  truenode := wordnode
  ///dsw("val", Val)
  dsw("valof", s_valof)
  dsw("where", s_where)
  dsw("within", s_within)
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

AND rdnumber() = VALOF
{ LET tok, zeroes, ok = Int, 0, FALSE
  lexval, exponent := 0, 0

  WHILE '0'<=ch<='9' DO
  { ok := TRUE               // At least one digit
    TEST ch='0'
    THEN { zeroes := zeroes+1
         }
    ELSE { WHILE zeroes DO
           { IF lexval > maxint/10 TEST tok=Int
             THEN synerr("Integer too large")
             ELSE synerr("Too many significant digits")
             lexval := 10*lexval
             zeroes := zeroes-1
             exponent := exponent-1
           }
           IF lexval > maxint/10 TEST tok=Int
           THEN synerr("Integer too large")
           ELSE synerr("Too many significant digits")
           lexval := 10*lexval + ch - '0'
           exponent := exponent - 1
         }
    rch()
    WHILE ch='.' DO
    { IF tok=Real DO synerr("Bad real number")
      tok, ok := Real, FALSE  // No digits after dot yet
      exponent := zeroes
      rch()
    }
  }
  TEST tok=Real
  THEN { UNLESS ok DO
           synerr("No digits after decimal point")
         IF lexval>99999999 DO
           synerr("More than 8 significant digits in real number")
         IF exponent=0 DO
           synerr("No digits after decimal point in real number")
       }
  ELSE { WHILE zeroes DO
         { IF lexval > maxint/10 DO synerr("Number too large")
           lexval := 10*lexval
           zeroes := zeroes-1
         }
       }
  RESULTIS tok
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
  IF ch='**' DO
  { rch()
    SWITCHON ch INTO
    { DEFAULT:   synerr("Bad string or character constant")
      CASE '*'': CASE '"':  res := ch;     ENDCASE
      CASE 't':  CASE 'T':  res := '*t';   ENDCASE
      CASE 's':  CASE 'S':  res := '*s';   ENDCASE
      CASE 'n':  CASE 'N':  res := '*n';   ENDCASE
      CASE 'b':  CASE 'B':  res := '*b';   ENDCASE
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
  mptnode := mk1(s_mpt)

  lex()

  IF optTokens DO            // For debugging lex.
  { writef("token = %i3 %s", token, opstr(token))
    IF token=Int    DO writef("       %n",  lexval)
    IF token=Real   DO writef("      %ne%n",  lexval, exponent)
    IF token=s_name   DO writef("      %s",   charv)
    IF token=s_stringconst DO
    { writef("    *'")
      FOR i = 1 TO charv%0 SWITCHON charv%i INTO
      { DEFAULT:   wrch(charv%i); ENDCASE
        CASE '*n': writes("**n"); ENDCASE
        CASE '*p': writes("**p"); ENDCASE
        CASE '*t': writes("**t"); ENDCASE
      }
      writef("*'")
    }
    newline()
    IF token=s_eof RESULTIS 0
    lex()
  } REPEAT

recover:
  res := rdprog()
  UNLESS token=s_eof DO fatalerr("Incorrect termination")
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
{ // P -> def D0 .. def D0 in C0 eof |
  //      C0 eof
  LET prog = 0

  TEST token=s_def
  THEN { LET link = @prog

         { LET a = mk4(s_def, 0, 0, lineno)
           h2!a := rndef()
           !link := a
  	   link := @h3!a
	 } REPEATWHILE token=s_def

         checkfor(s_in, "'in' expected at the end of a 'def' list")
       }
  ELSE { prog := rcom(0)
       }
  
  UNLESS token=s_eof DO synerr("Incorrect termination")

  RESULTIS prog
}

AND rnbdef(n) = VALOF
{ lex()
  RESULTIS rbdef(n)
}

AND rbdef(n) = VALOF
{ // BD -> N,...,N = E
  //       N BV...BV = E
  //       ( D )
  //       rec D
  LET op, ln = token, lineno

  SWITCHON op INTO
  { DEFAULT:
      synerr("Bad definition, name, rec or '(' expected")

    CASE s_name:
      { LET names = rname()
        ln := lineno

        IF token=s_comma DO
        { // Must be a simultaneous definition
          // N ,..., N = C0
          names := rdnamelist(names)
          checkfor(s_eq, "Bad definition")
          RESULTIS mk4(s_valdef, names, rcom(0), ln)
        }

        IF token=s_eq RESULTIS mk4(s_valdef, names, rncom(0), ln)

        { // Must be a function definition
          // N BV ... BV = C0
          LET v = VEC 50
          AND i, b = 0, ?
          WHILE i<=50 DO
          { UNLESS token=s_lparen | token=s_name BREAK
            v!i := rbv()
            i := i+1
          }
          UNLESS i~=0 & token=s_eq DO synerr("Bad definition")
          b := rncom(0)
          WHILE i>0 DO
          { i := i-1
            b := mk4(s_lambda, v!i, b, ln)
          }
          RESULTIS mk4(s_valdef, names, b, ln)
        }
      }

    CASE s_lparen:
    { LET a = rndef(0)
      checkfor(s_rparen, "Bad definition")
      RESULTIS a
    }

    CASE s_rec:
      lex()
      UNLESS n=0 DO synerr("Redundant 'rec'")
      RESULTIS mk3(s_rec, rnbdef(2), ln)
  }
}

AND rndef(n) = VALOF { lex(); RESULTIS rdef(n) }

AND rdef(n) = VALOF
{ // D -> D and D
  //      D within D
  //      BD
  LET a = rbdef(0)
  LET b = 0

  { LET op, ln = token, lineno

//sawritef("rdef: op=%s ln=%n*n", opstr(op), ln)
    SWITCHON op INTO
    { DEFAULT:
        RESULTIS a

      CASE s_and:
        IF a=0 DO synerr("Definition missing before 'and'")
        IF n>=6 RESULTIS a
        { LET i = 1
          LET v = VEC 100
          WHILE token=s_and DO
          { v!i := rnbdef(0)
            i := i+1
          }
          b := a
          a := newvec(i+1)
          a!0, a!1, a!2 := s_and, i+1, b
          FOR j = 1 TO i-1 DO a!(j+2) := v!j
          LOOP
        }

      CASE s_within:
        IF a=0 DO synerr("Definition missing before 'within'")
        IF n>=3 RESULTIS a
        a := mk4(s_within, a, rndef(0), ln)
        LOOP
    }
  } REPEAT
}

AND rbv() = VALOF
{ // Only called when token is Name or Lparen
  LET a = ?
  IF token=s_name RESULTIS rname()
  checkfor(s_lparen, "'(' expected")
  IF token=s_rparen DO
  { lex()
    RESULTIS mptnode
  }
  a := rdnamelist(0)
  checkfor(s_rparen, "Bad bound variable list")
  RESULTIS a
}

AND rdnamelist(n) = VALOF
{ LET a, b, i, ln = 0, n, 1, lineno
  LET v = VEC 100
  IF n=0 DO
  { UNLESS token=s_name DO
      synerr("Bad name list")
    b := rname()
  }
  UNLESS token=s_comma RESULTIS b
  WHILE token=s_comma DO
  { lex()
    UNLESS token=s_name DO synerr("A name is missing")
    v!i := rname()
    i := i+1
  }
  a := newvec(i+1)
  h1!a, h2!a, h3!a := s_comma, i, b
  FOR j = 1 TO i-1 DO a!(j+2) := v!j
  RESULTIS a
}

AND rname() = VALOF
{ LET a = wordnode
  checkfor(s_name, "Name expected")
  RESULTIS a
}

AND rarg() = VALOF
{ LET a, ln = 0, lineno
sawritef("rarg: token=%s*n", opstr(token))
  SWITCHON token INTO
  { DEFAULT:
      RESULTIS 0  // Not suitable as an unparenthesised argument

  }
}
 
LET rbexp(n) = VALOF
{ LET a, op, ln = 0, token, lineno
 
  SWITCHON op INTO
 
  { DEFAULT:
      synerr("Error in expression")

    CASE s_true:
    CASE s_false:
    CASE s_name:
    CASE s_nil:
    CASE s_dummy:
    CASE s_stringconst:
    CASE s_sys:
    CASE s_jj:
      a := wordnode
      lex()
      RESULTIS a
   
    CASE s_lparen:
      lex()
      TEST token=s_rparen
      THEN a := nilnode
      ELSE a := rcom(0)
      checkfor(s_rparen, "')' missing")
      IF n<=8 DO a := mk3(s_paren, a, ln)
      RESULTIS a
 
    CASE Int:
      a := mk2(Int, lexval)
      lex()
      RESULTIS a
 
    CASE Real:
      a := mk3(Real, lexval, exponent)
      lex()
      RESULTIS a
 
    CASE s_noshare:
      UNLESS n<=36 DO synerr("'$' or 'sys' out of context")
      RESULTIS mk3(op, rnexp(38), ln)
 
    CASE s_plus:
      UNLESS n<=30 DO synerr("'+' out of context")
      RESULTIS rnexp(32)
 
    CASE s_minus:
      UNLESS n<=30 DO synerr("'-' out of context")
      a := rnexp(32)
      TEST h1!a=Int | h1!a=Real
      THEN h2!a := - h2!a
      ELSE a := mk2(s_neg, a)
      RESULTIS a
 
    CASE s_not:
      UNLESS n<=24 DO synerr("'not' out of context")
      RESULTIS mk2(s_not, rnexp(26))
  }
}
 
AND rnexp(n) = VALOF { lex(); RESULTIS rexp(n) }
 
AND rexp(n) = VALOF
{ LET a, b, p = rbexp(n), 0, 0

  { LET op, ln = token, lineno

    SWITCHON op INTO
    { DEFAULT:
        RESULTIS a
 
      // Tokens that start a function argument
      
      CASE s_nil:
      CASE s_true:
      CASE s_false:
      CASE Int:
      CASE Real:
      CASE s_dummy:
      CASE s_stringconst:
      CASE s_name:
      CASE s_jj:
        a := mk4(s_apply, a, rbexp(0), ln)
        LOOP

      CASE s_lparen:
        lex()
        IF token=s_rparen DO
        { // Empty argument list
          lex()
          RESULTIS nilnode
        }
        b := rcom(0)
        checkfor(s_rparen, "')' expected")
        a := mk4(s_apply, a, b, ln)
        LOOP

      CASE s_comma:
        IF n>14 RESULTIS a
        { LET i = 1
          LET v = VEC 500
          WHILE token=s_comma DO
          { v!i := rnexp(16)
            i := i+1
          }
          b := a
          a := newvec(i+1)
          a!0, a!1, a!2 := s_comma, i, b
          FOR j = 1 TO i-1 DO a!(j+2) := v!j
//sawritef("rexp: Comma i=%n*n", i)
          LOOP
        }

      CASE s_aug:
        IF n>16 RESULTIS a
        a := mk4(s_aug, a, rnexp(18), ln)
        LOOP

      CASE s_cond:
        IF n>18 RESULTIS a
        b := rnexp(18)
        checkfor(s_pling, "Bad conditional expression")
        a := mk5(s_cond, a, b, rexp(18), ln)
        LOOP

      CASE s_logor:
        IF n>20 RESULTIS a
        a := mk4(op, a, rnexp(22), ln)
        LOOP

      CASE s_logand:
        IF n>22 RESULTIS a
        a := mk4(op, a, rnexp(24), ln)
        LOOP

      CASE s_eq:CASE s_le:CASE s_ls:CASE s_ne:CASE s_ge:CASE s_gr:
        IF n>26 RESULTIS a
        a := mk4(op, a, rnexp(30), ln)
        LOOP

      CASE s_plus:CASE s_minus:
        IF n>30 RESULTIS a
        a := mk4(op, a, rnexp(32), ln)
        LOOP

      CASE s_mult:CASE s_div:
        IF n>32 RESULTIS a
        a := mk4(op, a, rnexp(34), ln)
        LOOP

      CASE s_power:
        IF n>36 RESULTIS a
        a := mk4(op, a, rnexp(34), ln)
        LOOP

      CASE s_percent:
        IF n>36 RESULTIS a
        lex()
        UNLESS token=s_name DO synerr("Name expected in '%' construct")
        b := rname()
        a := mk4(s_comma, 2, a, rexp(38))
        a := mk4(s_apply, b, a, ln)
        LOOP
    }
  } REPEAT
}

AND rncom(n) = VALOF
{ lex()
  RESULTIS rcom(n)
}

AND rcom(n) = VALOF
{ LET a = rbcom(n)

  { LET op, ln = token, lineno
    SWITCHON op INTO
 
    { DEFAULT:
        BREAK
 
      CASE s_seq:
        IF n>6 RESULTIS a
        a := mk4(s_seq, a, rncom(6), ln)
        LOOP

      CASE s_where:
        IF n>2 RESULTIS a
        a := mk4(s_where, a, rnbdef(0), ln)
        LOOP

      CASE s_colon:
        UNLESS h1!a=s_name & n<=8 DO
          synerr("Syntax error in label")
        a := mk5(s_colon, a, rncom(8), 0, ln)
        LOOP
    }
  } REPEAT

  RESULTIS a
}

AND rbcom(n) = VALOF
{ LET op, ln, a, b = token, lineno, 0, 0

  SWITCHON op INTO
  { DEFAULT: // Must be an expression
    { a := rexp(n)
      ln := lineno
      IF token=s_ass RESULTIS mk4(s_ass, a, rnexp(14), ln)
      RESULTIS a
    }

    CASE s_let:
    { UNLESS n=0 DO synerr("'let' out of context")
      a := rndef(0)
      checkfor(s_in, "'in' expected in 'let' construct")
      RESULTIS mk4(s_let, a, rcom(0), ln)
    }

    CASE s_lambda:
    { LET v = VEC 50
      AND i = 0
      UNLESS n=0 DO synerr("'fn' out of context")
      lex()
      WHILE i<=50 DO
      { UNLESS token=s_lparen | token=s_name BREAK
        v!i := rbv()
        i := i+1
      }
      IF i=0 DO synerr("No bound variable list after 'fn'")
      checkfor(s_dot, "'.' missing in 'fn' construct")
      a := rcom(0)
      WHILE i>0 DO
      { i := i-1
        a := mk4(s_lambda, v!i, a, ln)
      }
      RESULTIS a
    }

    CASE s_valof:
      UNLESS n<=4 DO synerr("'valof' out of context")
      RESULTIS mk3(op, rncom(6), ln)
 
    CASE s_test:
      UNLESS n<=10 DO synerr("'test' out of context")
      a := rnexp(20)
      SWITCHON token INTO
      { DEFAULT:
          synerr("Bad 'test' command")

        CASE s_ifso:
          b := rncom(8)
          checkfor(s_ifnot, "'ifnot' expected")
          RESULTIS mk5(s_cond, a, b, rncom(8), ln)

        CASE s_ifnot:
          b := rncom(8)
          checkfor(s_ifso, "'ifnot' expected")
          RESULTIS mk5(s_cond, a, rncom(8), b, ln)
      }


    CASE s_while:
    CASE s_if:
    { LET op = token
      UNLESS n<=10 DO synerr("'if' or 'while' out of context")     
      a := rnexp(20)
      checkfor(s_do, "'do' expected")
      TEST op=s_if
      THEN RESULTIS mk5(s_cond, a, rcom(8), dummynode, ln)
      ELSE RESULTIS mk5(s_while, a, rcom(8), ln)
    }

    CASE s_goto:
      RESULTIS mk3(s_goto, rnexp(38), ln)

    CASE s_res:
      RESULTIS mk3(s_res, rnexp(14), ln)

  }
}

LET opstr(op) = VALOF SWITCHON op INTO
{ DEFAULT:          sawritef("opstr: unknown op: %n*n", op)
                    RESULTIS "###Unknown op ###"

  CASE s_ass:         RESULTIS "Ass"
  CASE s_and:         RESULTIS "And"
  CASE s_apply:       RESULTIS "Apply"
  CASE s_aug:         RESULTIS "Aug"
  CASE s_pling:         RESULTIS "Pling"
  CASE i_blocklink:   RESULTIS "Blocklink"
  CASE s_colon:       RESULTIS "Colon"
  CASE s_comma:       RESULTIS "Comma"
  CASE s_cond:        RESULTIS "Cond"
  CASE i_decllabel:   RESULTIS "Decllabel"
  CASE i_declname:    RESULTIS "Declname"
  CASE i_declnames:   RESULTIS "Declnames"
  CASE s_def:         RESULTIS "Def"
  CASE s_div:         RESULTIS "Div"
  CASE s_do:          RESULTIS "Do"
  CASE s_dot:         RESULTIS "Dot"
  CASE s_dummy:
  CASE i_dummy:
  CASE t_dummy:       RESULTIS "Dummy"
  CASE s_eof:         RESULTIS "Eof"
  CASE s_eq:          RESULTIS "Eq"
  CASE s_ifnot:       RESULTIS "Ifnot"
  CASE s_power:       RESULTIS "Power"
  CASE s_false:       RESULTIS "False"
  CASE i_formClosure: RESULTIS "FormClosure"
  CASE i_formLvalue:  RESULTIS "FormLvalue"
  CASE i_formRvalue:  RESULTIS "FormRvalue"
  CASE s_ge:          RESULTIS "Ge"
  CASE s_goto:        RESULTIS "Goto"
  CASE s_gr:          RESULTIS "Gr"
  CASE i_halt:        RESULTIS "Halt"
  CASE s_if:          RESULTIS "If"
  CASE s_in:          RESULTIS "In"
  CASE i_initname:    RESULTIS "Initname"
  CASE i_initnames:   RESULTIS "Initnames"
  CASE Int:         RESULTIS "Int"
  CASE s_jj:
  CASE i_jj:
  CASE t_jj:          RESULTIS "Jj"
  CASE i_jump:        RESULTIS "Jump"
  CASE i_jumpF:       RESULTIS "JumpF"
  CASE i_lab:         RESULTIS "Lab"
  CASE s_lambda:      RESULTIS "Lambda"
  CASE s_le:          RESULTIS "le"
  CASE s_let:         RESULTIS "Let"
  CASE i_loadE:       RESULTIS "LoadE"
  CASE i_loadF:       RESULTIS "LoadF"
  CASE i_loadGuess:   RESULTIS "LoadGuess"
  CASE i_loadJ:       RESULTIS "LoadJ"
  CASE i_loadL:       RESULTIS "LoadL"
  CASE i_loadN:       RESULTIS "LoadN"
  CASE i_loadR:       RESULTIS "LoadR"
  CASE i_loadS:       RESULTIS "LoadS"
  CASE s_logand:      RESULTIS "Logand"
  CASE s_logor:       RESULTIS "Logor"
  CASE s_lose1:       RESULTIS "Lose1"
  CASE s_lparen:      RESULTIS "Lparen"
  CASE s_ls:          RESULTIS "Ls"
  CASE i_members:     RESULTIS "Members"
  CASE s_minus:       RESULTIS "s_minus"
  CASE s_mpt:         RESULTIS "Mpt"
  CASE s_mult:        RESULTIS "Mult"
  CASE s_name:        RESULTIS "Name"
  CASE s_ne:          RESULTIS "Ne"
  CASE s_neg:         RESULTIS "Neg"
  CASE s_nil:         RESULTIS "Nil"
  CASE s_not:         RESULTIS "Not"
  CASE s_paren:       RESULTIS "Paren"       
  CASE s_percent:     RESULTIS "Percent"       
  CASE s_plus:        RESULTIS "Plus"
  CASE s_pos:         RESULTIS "Pos"
  CASE Real:        RESULTIS "Real"       
  CASE s_rec:         RESULTIS "Rec"       
  CASE s_res:         RESULTIS "Res"
  CASE i_reslink:     RESULTIS "Reslink"
  CASE i_restoreE1:   RESULTIS "RestoreE1"
  CASE i_return:      RESULTIS "Return"
  CASE s_rparen:      RESULTIS "Rparen"
  CASE i_save:        RESULTIS "Save"
  CASE s_seq:         RESULTIS "Seq"
  CASE i_setlabEs:    RESULTIS "SetlabEs"
  CASE i_setup:       RESULTIS "Setup"
  CASE s_stringconst: RESULTIS "Stringconst"
  CASE s_sys:
  CASE i_sys:
  CASE t_sys:         RESULTIS "Sys"
  CASE s_test:        RESULTIS "Test"
  CASE i_testEmpty:   RESULTIS "TestEmpty"
  CASE s_ifso:        RESULTIS "Ifso"
  CASE s_true:
  CASE i_true:
  CASE t_true:        RESULTIS "True"
  CASE Tuple:       RESULTIS "Tuple"
  CASE s_noshare:     RESULTIS "Noshare"
  CASE i_update:      RESULTIS "Update"
  CASE s_valdef:      RESULTIS "Valdef"
  CASE s_valof:       RESULTIS "Valof"
  CASE s_where:       RESULTIS "Where"
  CASE s_within:      RESULTIS "Within"
}

LET plist(x, n, d) BE
{ LET op, size, ln = ?, 0, 0
  LET v = TABLE 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

  IF x=0 DO { writes("Null"); RETURN  }
 
  op := h1!x

  SWITCHON op INTO
  { DEFAULT:
writef("Default op=%s*n", opstr(op)); RETURN

    CASE Int:     writef("Int %n", h2!x);           RETURN
    CASE Real:    writef("Real %ne%n", h2!x, h3!x); RETURN
    CASE s_name:    writef("Name %s", x+2);           RETURN
    CASE s_stringconst:  
                { LET s = x+1
                  writef("s_stringconst *'")
                  FOR i = 1 TO s%0 SWITCHON s%i INTO
                  { DEFAULT:   wrch(s%i); ENDCASE
                    CASE '*n': writes("**n"); ENDCASE
                    CASE '*p': writes("**p"); ENDCASE
                    CASE '*t': writes("**t"); ENDCASE
                  }
                  writef("*'")
                  RETURN
                }

    CASE s_colon:
         size, ln := 3, h5!x; ENDCASE

    CASE s_cond: CASE s_test: CASE s_percent:
         size, ln := 4, h5!x; ENDCASE

    CASE s_power: CASE s_mult: CASE s_div: CASE s_plus: CASE s_minus:
    CASE s_eq: CASE s_ne: CASE s_ls: CASE s_gr: CASE s_le: CASE s_ge:
    CASE s_logand: CASE s_logor: CASE s_aug:
    CASE s_let: CASE s_where: CASE s_within:
    CASE i_lab:
    CASE s_ass: CASE s_apply: CASE s_lambda:
    CASE s_def: CASE s_valdef: CASE Tuple: CASE s_seq:
    CASE s_if:
         size, ln := 3, h4!x; ENDCASE

    CASE s_comma: CASE s_and:
         // x -> [op, n, a1 ,..., an]
         size := h2!x+1
//sawritef("plist: Comma size=%n*n", size)
         x := x+1
         ENDCASE

    CASE s_noshare:
    CASE s_rec:
    CASE s_valof: 
    CASE s_goto: 
    CASE s_res:
    CASE s_sys:
    CASE s_paren:
         size, ln := 2, h3!x; ENDCASE

    CASE s_true: CASE s_false:
    CASE s_nil: CASE s_mpt:
    CASE s_dummy: CASE s_jj:
         size := 1;       ENDCASE
  }
 
  IF n=d DO { writes("Etc"); RETURN }
  writef("%s", opstr(op))
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
{ LET n = ?
  FOR i = 0 TO labmax DO labv!i, refv!i := -1, 0

  comline, procname, labnumber := 1, 0, 0
  ssp, msp := 0, 1

  IF optCode DO writef("*nCompiled code:*n*n")

  n := nextlab()
  outfl(i_setup, n)

  translabels(x)

  trans(x, Val)
  UNLESS ssp=1 DO writef("*nSSP error*n")
  outf(i_halt)
  outlabset(n, msp)

  //resolvelabels()

  writef("*nProgram size: %n data size: %n*n*n",
          codep, datap)
}

LET trans(x, mode) BE
// x       is the program
// mode is Val or Ref
{ LET op = h1!x

  IF x=0 DO
  { writes("*nExpression missing*n")
    outf(s_nil)
    upssp(1)
    IF mode=Ref DO outf(i_formLvalue)
    RETURN
  }

//writef("trans: op=%s*n", opstr(op))
  SWITCHON op INTO
  { DEFAULT:
      // It must be an expression
      load(x)
      RETURN

    CASE s_let:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      comline := h5!x
      transrhs(h2!x)
      outfl(i_blocklink, lab1)
      IF ssp=msp DO msp := ssp+1
      transscope(x, lab2, mode)
      outlab(lab1)             
      RETURN
    }
  
    CASE s_def:
      transrhs(h2!x)
      declnames(h2!x)
      translabels(h3!x)
      trans(h3!x, Val)
      RETURN

    CASE s_mult: CASE s_div: CASE s_power: CASE s_plus: CASE s_minus:
    CASE s_eq: CASE s_ne: CASE s_ls: CASE s_le: CASE s_gr: CASE s_ge:
    CASE s_logand: CASE s_logor:
      trans(h3!x, Val)
      trans(h2!x, Val)
      outf(op)
      ssp := ssp-1
      IF mode=Ref DO outf(i_formLvalue)
      RETURN

    CASE s_aug:
      trans(h3!x, Ref)
      trans(h2!x, Val)
      outf(s_aug)
      ssp := ssp-1
      IF mode=Ref DO outf(i_formLvalue)
      RETURN

    CASE s_apply:
      trans(h3!x, Ref)
      trans(h2!x, Ref)
      outf(s_apply)
      ssp := ssp-1
      IF mode=Val DO outf(i_formRvalue)
      RETURN

    CASE s_pos:
    CASE s_neg:
    CASE s_not:
      trans(h2!x, Val)
      outf(op)
      IF mode=Ref DO outf(i_formLvalue)
      RETURN

    CASE s_noshare:
      trans(h2!x, Val)
      IF mode=Ref DO outf(i_formLvalue)
      RETURN

    CASE s_comma:
    { LET len = length(x)
      LET r(x) BE trans(x, Ref)
      mapb(r, x)
      outfn(Tuple, len)
      ssp := ssp - len + 1
      IF mode=Ref DO outf(i_formLvalue)
      RETURN
    }

    CASE s_lambda:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      LET lab3 = nextlab()
      outfl(i_formClosure, lab1)
      upssp(1)
      outfl(i_jump, lab2)
      outlab(lab1)
      transscope(x, lab3, Ref)
      outlab(lab2)
      IF mode=Ref DO outf(i_formLvalue)
      RETURN
    }

    CASE s_colon:
      IF h4!x=0 DO
      { trnerr("Label %s improperly used", h3!(h2!x))
      }
      outlab(h4!x)
      trans(h3!x, mode)
      RETURN

    CASE s_seq:
      trans(h2!x, Val)
      outf(s_lose1)
      trans(h3!x, mode)
      RETURN

    CASE s_valof:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      outfl(i_reslink, lab1)
      ssp := ssp+1
      IF ssp>=msp DO msp := ssp+1
      { LET a, b = ssp, msp
        ssp, msp := 0, 1
        outfl(i_save, lab2)
        outf(s_jj)
        outf(i_formLvalue)
        outf(i_declname)
        outname(s_name, 0, "**RES**")
        translabels(h2!x)
        trans(h2!x, Ref)
        outf(i_return)
        UNLESS ssp=1 DO trnerr("SSP Error")
        outlabset(lab2, msp)
        ssp, msp := a, b
      }
      outlab(lab1)
      IF mode=Val DO outf(i_formRvalue)
      RETURN
    }

    CASE s_res:
      trans(h2!x, Ref)
      outf(s_res)
      ssp := ssp-1
      RETURN

    CASE s_goto:
      trans(h2!x, Val)
      outf(s_goto)
      ssp := ssp-1
      RETURN

    CASE s_cond:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      trans(h2!x, Val)
      outfl(i_jumpF, lab1)
      ssp := ssp-1
      trans(h3!x, mode)
      outfl(i_jump, lab2)
      outlab(lab1)
      ssp := ssp-1
      trans(h4!x, mode)
      outlab(lab2)
      RETURN
    }

    CASE s_while:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      outlab(lab2)
      trans(h2!x, Val)
      outfl(i_jumpF, lab1)
      ssp := ssp-1
      trans(h3!x, Val)
      outf(s_lose1)
      outfl(i_jump, lab2)
      outlab(lab1)
      outf(i_dummy)
      IF mode=Ref DO outf(i_formLvalue)
      RETURN
    }

    CASE s_ass:
    { LET len = length(h2!x)
      comline := h4!x
      trans(h2!x, Ref)
      trans(h3!x, Val)
      outfn(i_update, len)
      ssp := ssp-1
      IF mode=Ref DO outf(i_formLvalue)
      RETURN
    }

    CASE s_paren:
      translabels(h2!x)
      trans(h2!x, mode)
      RETURN

    CASE s_nil:
    CASE s_dummy:
    CASE s_true:
    CASE s_false:
    CASE s_sys:
    CASE s_jj:
      outf(op) // Bug ################### must convert s_ to i_
      upssp(1)
      IF mode=Ref DO outf(i_formLvalue)
      RETURN

    CASE s_name:
      outfname((mode=Val -> i_loadR, i_loadL), x)
      //outname(x)
      upssp(1)
      RETURN

    CASE Int:
      outfn(i_loadN, h2!x)
      upssp(1)
      RETURN

    CASE Real: 
      outfn(i_loadF, h2!x, h3!x)
      upssp(1)
      RETURN

    CASE s_stringconst:
      outf(i_loadS)
      outstring(x)
      upssp(1)
      IF mode=Ref DO outf(i_formLvalue)
      RETURN
  }
}

AND findlabels(x) = VALOF
{ IF x=0 RESULTIS 0
  SWITCHON h1!x INTO
  { DEFAULT:
      RESULTIS 0

    CASE s_colon:
    { LET lab = nextlab()
      h4!x := lab
      outfsl(i_decllabel, h2!x, lab)
      RESULTIS 1 + findlabels(h3!x)
    }

    CASE s_paren:
      RESULTIS findlabels(h2!x)

    CASE s_cond:
      RESULTIS findlabels(h3!x) +
               findlabels(h4!x)

    CASE s_while:
      RESULTIS findlabels(h3!x)

    CASE s_seq:
      RESULTIS findlabels(h2!x) +
               findlabels(h3!x)
  }
}

AND translabels(x) BE
{ LET n = findlabels(x)
  IF n DO outf(i_setlabEs, n)
}

AND transrhs(x) BE
{ IF x=0 RETURN

  SWITCHON h1!x INTO
  { DEFAULT:
      RETURN

    CASE s_and:
    { LET len = length(x)
      mapb(transrhs, x)
      outfn(Tuple, len)
      ssp := ssp - len + 1
      outf(i_formLvalue)
      RETURN
    }

    CASE s_valdef:
      trans(h3!x, Ref)
      RETURN

    CASE s_rec:
      outf(i_loadE)
      upssp(1)
      declguesses(h2!x)
      transrhs(h2!x)
      initnames(h2!x)
      loaddefinee(h2!x)
      outf(i_restoreE1)
      ssp := ssp-1
      RETURN

    CASE s_within:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      transrhs(h2!x)
      outfl(i_blocklink, lab1)
      IF ssp=msp DO msp := ssp+1
      { LET a, b = ssp, msp
        ssp, msp := 0, 1
        outfl(i_save, lab2)
        declnames(h2!x)
        transrhs(h3!x)
        outf(i_return)
        UNLESS ssp=1 DO trnerr("SSP error")
        outlabset(lab2, msp)
        ssp, msp := a, b
      }
      outlab(lab1)
      RETURN
    }
  }
}

AND declnames(x) BE
{ IF x=0 RETURN
  SWITCHON h1!x INTO
  { DEFAULT:
      trnerr("Bad bound variable list")
      RETURN

    CASE s_name:
      outfname(i_declname, x)
      //outname(x)
      ssp := ssp-1
      RETURN

    CASE s_comma:
      outfn(i_declnames, length(x))
      ssp := ssp-1
      mapf(outname, x)
      RETURN

    CASE s_and:
    { LET len = length(x)
      outfn(i_members, len)
      upssp(len-1)
      mapf(declnames, x)
      RETURN
    }

    CASE s_rec:
    CASE s_valdef:
      declnames(h2!x)
      RETURN

    CASE s_within:
      declnames(h3!x)
      RETURN

    CASE s_mpt:
      outf(i_testEmpty)
      ssp := ssp-1
      RETURN
  }
}

AND loaddefinee(x) BE
{ IF x=0 RETURN
  SWITCHON h1!x INTO
  { DEFAULT:
      trnerr("Bad definition")
      RETURN

    CASE s_name:
      outf(i_loadR); outname(x)
      upssp(1)
      outf(i_formLvalue)
      RETURN

    CASE s_and:
    CASE s_comma:
    { LET len = length(x)
      mapb(loaddefinee, x)
      outfn(Tuple, len)
      ssp := ssp - len + 1
      outf(i_formLvalue)
      RETURN
    }

    CASE s_rec:
    CASE s_valdef:
      loaddefinee(h2!x)
      RETURN

    CASE s_within:
      loaddefinee(h3!x)
      RETURN

  }
}

AND declguesses(x) BE
{ IF x=0 RETURN
  SWITCHON h1!x INTO
  { DEFAULT:
      trnerr("Bad definition")
      RETURN

    CASE s_name:
      outf(i_loadGuess)
      IF ssp=msp DO msp := ssp+1
      outf(i_declname); outname(x)
      RETURN

    CASE s_and:
    CASE s_comma:
      mapf(declguesses, x)
      RETURN

    CASE s_rec:
    CASE s_valdef:
      declguesses(h2!x)
      RETURN

    CASE s_within:
      declguesses(h3!x)
      RETURN
  }
}

AND initnames(x) BE
{ IF x=0 RETURN
  SWITCHON h1!x INTO
  { DEFAULT:
      trnerr("Bad definition")
      RETURN

    CASE s_name:
      outf(i_initname); outname(x)
      ssp := ssp-1
      RETURN

    CASE s_and:
    { LET len = length(x)
      outfn(i_members, len)
      upssp(len-1)
      outfn(i_initnames, len)
      mapf(initnames, x)
      RETURN
    }

    CASE s_comma:
    { LET len = length(x)
      outfn(i_initnames, len)
      ssp := ssp-1
      mapf(outname, x)
      RETURN
    }

    CASE s_rec:
    CASE s_valdef:
      initnames(h2!x)
      RETURN

    CASE s_within:
      initnames(h3!x)
      RETURN
  }
}

AND transscope(x, n, mode) BE
{ LET a, b = ssp, msp
  ssp, msp := 1, 1
  outfn(i_save, n)
  declnames(h2!x)
  translabels(h3!x)
  trans(h3!x, mode)
  outf(i_return)
  UNLESS ssp=1 DO trnerr("SSP error")
  outlabset(n, msp)
  ssp, msp := a, b
}

AND mapf(r, x) BE
{ // x -> [-, n, x1, x2,...xn]
  LET len = h2!x
  FOR i = 1 TO len DO r(x!(i+1))
}

AND mapb(r, x) BE
{ LET len = h2!x
  FOR i = len TO 1 BY -1 DO r(x!(i+1))
}

AND length(x) = h1!x=s_and | h1!x=s_comma -> h2!x, 1

AND upssp(x) BE
{ ssp := ssp+x
  IF ssp>msp DO msp := ssp
}

AND wrf(form, a, b, c) BE IF optCode DO writef(form, a, b, c)

AND outf(op) BE
{ wrf("%i5: %s*n", codep, opstr(op))
  putc(op)
}

AND outfname(op, x) BE
{ wrf("%i5: %s", codep, opstr(op))
  putc(op)
  outname(x)
}

AND outname(x) BE
{ LET name = @h3!x
  LET len = name%0
  //outfn(s_name, len)
  FOR i = 1 TO len DO putc(name%i)
  IF optCode DO writef(" %s*n", name)
}

AND outstring(x) BE
{ LET name = @h3!x
  LET len = name%0
  outfn(s_stringconst, len)
  FOR i = 1 TO len DO putc(name%i)
  IF optCode DO writef(" %s", name)
}

AND outfv(op, var) BE
{ wrf("%i5: %s %s*n", codep, opstr(op), var)
  putc(op); putc(var)
}

AND outn(a) BE
{ wrf("%i5: %n*n", codep, a)
  putc(a)
}

AND outfn(op, a) BE
{ wrf("%i5: %s %n*n", codep, opstr(op), a)
  putc(op); putc(a)
}

AND outfnn(op, a, b) BE
{ wrf("%i5: %s %n %n*n", codep, opstr(op), a, b)
  putc(op); putc(a); putc(b)
}

AND outfsl(op,a, b) BE
{ wrf("%i5: %s %s L%n*n", codep, opstr(op), a, b)
  putc(op); putc(a); putc(b)
}

AND outfl(op, lab) BE
{ wrf("%i5: %s L%n*n", codep, opstr(op), lab)
  putc(op); putref(lab)
}

AND outlab(lab) BE
{ wrf("%i5: Lab L%n %n*n", codep, lab, codep)
  setlab(lab, codep)
}

AND outlabset(lab, val) BE
{ wrf("%i5: Lab L%n %n*n", codep, lab, val)
  setlab(lab, val)
}

AND outentry(l1, l2) BE
{ wrf("%i5: Entry L%n L%n*n", codep, l1, l2)
  putref(l2)
  setlab(l1, codep)
}

AND outstatvec(lab, a) BE
{ wrf("%i5: Statvec L%n %n*n", codep, lab, a)
  setlab(lab, datap)
  FOR i = 0 TO a-1 DO putd(0)
}

AND outvar(lab) BE
{ wrf("%i5: Var L%n*n", codep, lab)
  setlab(lab, datap)
  putd(0)
}
 
AND putc(w) BE TEST codep>codet
               THEN trnerr("More code space needed")
               ELSE { codev!codep := w
                      codep := codep+1
                    }

AND putd(w) BE TEST datap>datat
               THEN trnerr("More data space needed")
               ELSE { datav!datap := w
                      datap := datap+1
                    }

AND putref(lab) BE TEST codep>codet
                   THEN trnerr("More code space needed")
                   ELSE { codev!codep := refv!lab
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
  WHILE p DO { LET np = codev!p
               codev!p, p := labval, np
             }
}

*/


LET interpret() = VALOF
{ // Execute one or more SCED instructions

  // sp holds S
  // pc holds C
  // env holds E
  // dump holds D -> previous [S, E, C, D]
  // The above variables hold absolute addresses.
  
  // rega, regb // Working registers
  // count is decremented every time a Pocode instruction
  // is executed

  // codev hold the compiled Pocode
  // The start address is 1, so the initial setting of
  // pc is codev+1.
  // codev!0 is its upperbound.

  // The runtime data is held in the self expanding vector datasxv.
  // The actual data is in datav = datasxv!2. The upb of datav is
  // held in datasxv!0. Every time the vector is expanded both
  // datasxv!0 and datasxv!1 change and datav is updated.

  // Garbage collection is done by copying all the accessible data
  // in datav to a new self expanding vector which then replaces
  // datasxv. The zeroth word of every itemin datav is the length
  // of the item. This field is replaced by the negated position
  // of the copied item in the new self expanding vector. An item
  // with a negative length has already been copied and the negated
  // length field is the new location of the item.
  // The garbage collector updates stack, sp, env, dump, rega and
  // regb appropriately. Any of these variables having the value
  // zero are left unchanged.
  
  LET retcode = 0

  { // Start of main loop
    LET op = codev!pc                // Fetch next instruction

    IF optTrace DO
    { writef("%i5: %t8", pc, opstr(op))
      //IF hasOperand(op) DO writef(" %n", pc!1)
      newline()
    }
    IF count<=0 DO { retcode := 3; BREAK } // Zero count
    count := count-1
    pc := pc+1

    SWITCHON op INTO
    { DEFAULT:      writef("Unknown Pocode op: %s*n", opstr(op))
                    retcode := 1;    BREAK    // Unknown op code

      CASE i_halt:    BREAK
    }
  } REPEAT

  RESULTIS retcode
}

AND printf(mem, form, p) BE
{ LET fmt = form+mem
  LET i = 0

  { LET k = fmt%i
    i := i+1
    IF k=0 RETURN
    IF k='%' DO
    { LET n = 0;
      { k := fmt%i
        i := i+1
        UNLESS '0'<=k<='9' BREAK
        n := 10*n + k - '0'
      } REPEAT
      SWITCHON k INTO
      { DEFAULT:  wrch(k); LOOP
        CASE 'd': writed  (!p,     n); p := p+1; LOOP
        CASE 's': wrs     (mem+!p, n); p := p+1; LOOP
        CASE 'x': writehex(!p,     n); p := p+1; LOOP
      }
    }
    wrch(k)
  } REPEAT
}

AND wrs(s, n) BE
{ LET len = 0
  WHILE s%len DO len := len+1
  FOR i = len+1 TO n DO wrch(' ')
  FOR i = 0 TO len-1 DO wrch(s%i)
}

AND hasOperand(op) = VALOF SWITCHON op INTO
{ //CASE Fnrn:CASE Rtrn:CASE Lres:CASE i_halt:
  //CASE Vecap:CASE Ind:CASE Stind:CASE s_neg:CASE s_not:
  //CASE s_mult:CASE s_div:CASE Mod:CASE s_plus:CASE s_minus:
  //CASE s_eq:CASE s_ne:CASE s_le:CASE s_ge:CASE s_ls:CASE s_gr:
  //CASE Lsh:CASE Rsh:CASE s_and:CASE Or:CASE Xor:
            RESULTIS FALSE

  DEFAULT:  RESULTIS TRUE
}

AND lvofname() = 0

LET execpal(regs, mem) = VALOF
{ // Execute one or more SCED instructions
  // The PAL memory is held the self expanding vector: palsxv
  // palsxv -> [vupb, v]
  // v -> [upb, S, C, E, D, ...]
  
  // sp   holds S
  // pc   holds C
  // env  holds E -> [link, name, value]
  // dump holds D -> previous [S, E, C, D]
  // count is the count of instruction executed

  // Garbage collectonis done by copying the reachable elements of palsxv
  // into a new self expanding vector. The freeing the old one and replace
  // palsxv with the new one.
  LET retcode = 0

  sawritef("execpal: codev=%n pc=%n*n", codev, pc)
  
  { // Start of main loop
    LET op = codev!pc                // Fetch next instruction
    
    IF optTrace DO
    { writef("%i5: count=%n %t8", pc, count, opstr(op))
      IF hasOperand(op) DO writef(" %n", codev!(pc+1))
      newline()
      abort(6464)
    }
    IF count<=0 DO { retcode := 3; BREAK } // Zero count
    count := count-1
    pc := pc+1

    SWITCHON op INTO
    { DEFAULT:      retcode := 1;    BREAK    // Unknown op code

      CASE i_setup:   // Ln       Initialise the runtime system
	oldc := 0 ///LV r_finish  pc=0 causes Finish to be executed
	stack := list(5, t_stack, 4, dummyrv, 0)
	rega := list(3, t_lvalue, env)
	env := 0
	stackp := 5
	save()
	split1()
        BREAK

      CASE i_halt:    BREAK
      
      CASE i_loadL:   // x        Load the Lvalue of variable x
	pc := pc+1
	writef("Calling lvofname*n"); abort(1001)
	rega := lvofname(pc!1, env)
	TEST rega=nilrv
	THEN {	rega := list(3, t_lvalue, rega)
		errokdbg()
	     }
	ELSE next11()
        BREAK

      CASE i_loadR:   // x        Load the Rvalue of variable x
	pc := pc+1
	writef("Calling lvofname*n"); abort(1001)
	rega := lvofname(pc!1, env)
	TEST rega=nilrv
	THEN errdbg()
	ELSE { rega := h3!rega
	       next11()
	     }
        BREAK

      CASE i_loadE:   //          Load E
	rega := env
	next11()
        BREAK

      CASE i_loadS:   // s        Load string s
      { LET v = VEC 200
	LET i = 0
	unpackstring(pc!1, v)
	i := v!0
	rega := nilsrv
	WHILE i > 0 DO
	{ rega := list(4, t_string, rega, v!i)
	  i:=i-1
	}
	pc := pc+1
	next11()
        BREAK
      }
	
      CASE i_loadN:   // val      Load an integer
	rega := list(3, pc!1, pc!2)
	pc := pc+2
	next11()
        BREAK

      CASE i_loadF:   // man exp  Load float man * 10^exp
        BREAK

      CASE i_loadJ:   //          Load [C, S, E] to become the value
                      //          of *res* in the translation of valof
	rega := list(5, t_jj, h4!stack, h5!stack, h6!stack )
	next11()
        BREAK

      CASE i_sys:     //          Load Sys
        BREAK

      CASE i_blocklink:  // Ln    Place Nil on the top of the stack and
                         //       make it seem as if a parameterless
		         //       function with this block as its body
		         //       is being called.
        BREAK

      CASE i_reslink:    // Ln    Create a new Lvalue with contents Nil
                         //       and push it onto th stack. The execute
                         //       Blocklink Ln.
	stack!(stackp) := list(3, t_lvalue, nilrv)
	stackp := stackp+1
	blocklink()
        BREAK

      CASE i_declname:   // x     Add an Env node to the environment for 
                         //       name x giving it the Lvaue from the top
		         //       of the stack.

      CASE i_declnames:  // n x1.. xn Declare n names
        BREAK

      CASE i_decllabel:  // x Ln  Add and Env node for name x giving it a
                         //       new Lvalue holding {Ln, E]. The environment
		         //       field will be filled in later by a call
		         //       of SetlabEs.
        BREAK

      CASE i_setlabEs:   // n      Set the Environment fields of the
                         //        n labels just added to the environment.
        BREAK

      CASE i_initnames:  // n x1..xn
        BREAK
	
      CASE i_restoreE1:  //
	env := stack!(stackp-2)
	stackp := stackp-1
	stack!(stackp-1) := stack!(stackp)
	pc := pc+1
        BREAK

      CASE i_true:
	rega := truerv
	next11()
        BREAK
	
      CASE i_false:
	rega := falserv
	next11()
        BREAK
	
      CASE i_loadGuess:
	rega := guessrv
	nextlv11()
        BREAK
	
      CASE i_nil:
	rega := nilrv
	nextlv11()
        BREAK
	
      CASE i_dummy:
	rega := dummyrv
	nextlv11()
        BREAK
	
      CASE i_formClosure:
	rega := list(4, t_closure, env, pc!1 )
	pc := pc+1
	next11()
        BREAK
	
      CASE i_formLvalue:
	rega := list(3, t_lvalue, stack!(stackp-1))
	stack!(stackp-1) := rega
	pc := pc+1
        BREAK

      CASE i_formRvalue:
	stack!(stackp-1) := h3!(stack!(stackp-1))
	pc := pc+1
        BREAK

      CASE i_tuple:
      { LET n = pc!1
	rega := node(n+3)
	rega!0, rega!1, areg!2 := n+3, t_tuple, n
	FOR i = 3 TO n+2 DO
          stackp, rega!i := stackp-1, stack!(stackp)
	pc := pc+1
	next11()
        BREAK
      }
      
      CASE i_members:    // n     The top element of the stack is assumed
                         //       to be a tuple with n elements. These are
		         //       loaded onto the stack. This is only used
		         //       in the compilation of simultneous
		         //       assignments.
      { LET n = pc!1
	split1()
	regb := h3!rega
	FOR i = -2 TO n-3 DO
	{ stack!(stackp) := regb!(n-i)
	  stackp := stackp+1
	}
	pc := pc+2
        BREAK
      }

      CASE s_not:
	split1()
	IF h2!rega=t_false DO
	{ a := truerv
	  next11()
	  RETURN
	}
	TEST h2!rega=t_true
	THEN { rega := falserv
	       next11()
	     }
	ELSE { error1("NOT", rega, 0)
	       errdbg()
	     }
        BREAK

      CASE i_logand:
	split2()
	TEST testbools2()
	THEN { rega := h2!a=t_true -> regb, falserv
	       next11()
	     }
	ELSE { error1("&", rega, regb)
	       rega := falserv
	       errdbg()
	     }
        BREAK

      CASE i_logor:
	split2()
	TEST testbools2()
	THEN { rega := h2!rega=t_false -> regb, truerv
	       next11()
	     }
	ELSE { error1("OR", rega, regb)
	       rega := falserv
	       errdbg()
	     }
        BREAK

      CASE i_aug:
	split2()
	UNLESS h2!rega=t_tuple DO
	{ error1("AUG", rega, regb)
	  rega := nilrv
	  errdbg()
	  RETURN
	}
        { LET n = h3!rega
	  LET t = node(n+4)
	  t!0, t!1, t!2, t!(n+3) := n+4, t_tuple, n+1, regb
	  FOR i = 3 TO n+2 DO t!i := rega!i
	  rega := t
	  next11()
          BREAK
	}

      CASE i_result:
	rega := lvofname(nameres, env)
	IF a=nilrv DO
	{	rega := list(3, t_lvalue, rega)
		GOTO reserr }
	rega := h3!rega
	UNLESS h2!rega=t_jj DO
reserr:	{	error("INCORRECT USE OF RES", 0, 0, 0)
		errokdbg()
		RETURN }
	h4!stack, h5!stack, h6!stack := h3!rega, h4!rega, h5!rega
	return()
        BREAK

      CASE i_mult:
{	LET t = rega
	split2()
	IF testnumbs2()=t_number DO
	{	rega := list(3, t_number, h3!rega*h3!regb )
		next11()
		RETURN }
	IF testnumbs2()=t_real DO
	{	rega := list(3, t_real, fmult(h3!rega, h3!regb) )
		IF floterr DO {	writes("*nOVERFLOW:")
					floterr := FALSE
					GOTO fmuerr }
		next11()
		RETURN }
	rega := list(3, t_number, 0)
fmuerr:	error1("**", t, regb)
	errdbg() }
        BREAK

      CASE i_div:
{	LET t = rega
	split2()
	IF testnumbs2()=t_number DO
	{	IF h3!regb=0 GOTO derr
		rega := list(3, t_number, h3!rega/h3!regb )
		next11()
		RETURN }
	IF testnumbs2()=t_real DO
	{	rega := list(3, t_real, fdiv(h3!rega, h3!regb) )
		IF floterr DO
		{	UNLESS feq(h3!regb, 0) DO writes("*nOVERFLOW:")
			floterr := FALSE
			GOTO derr }
		next11()
		RETURN }
derr:	a := list(3, t_number, 0)
	error1("/", t, regb)
	errdbg() }
        BREAK

      CASE i_plus:
{	LET t = rega
	split2()
	IF testnumbs2()=t_number DO
	{	rega := list(3, t_number, h3!rega+h3!regb )
		next11()
		RETURN }
	IF testnumbs2()=t_real DO
	{	rega := list(3, t_real, fadd(h3!rega, h3!regb) )
		IF floterr DO {	writes("*nOVERFLOW:")
					floterr := FALSE
					GOTO fperr }
		next11()
		RETURN }
	a := list(3, t_number, 0)
fperr:	error1("+", t, regb)
	errdbg() }
        BREAK

      CASE i_minus:
{	LET t = rega
	split2()
	IF testnumbs2()=t_number DO
	{	rega := list(3, t_number, h3!rega-h3!regb )
		next11()
		RETURN }
	IF testnumbs2()=t_real DO
	{	rega := list(3, t_real, fsub(h3!rega, h3!regb) )
		IF floterr DO {	writes("*nOVERFLOW:")
					floterr := FALSE
					GOTO fmerr }
		next11()
		RETURN }
	rega := list(3, t_number, 0)
fmerr:	error1("-", t, regb)
	errdbg() }
        BREAK

      CASE i_power:
{	LET t = rega
	split2()
	UNLESS h2!regb=t_number GOTO pwerr
	IF h2!a=t_number DO
	{	LET base, exp, r = h3!rega, h3!regb, 1
		TEST exp <= 0
		THEN {	IF base=0 GOTO pwerr
			r := ABS base = 1 ->
			((-exp & 1)=0 -> 1, base), 0 }
		ELSE UNTIL exp=0 DO
		{	UNLESS (exp & 1)=0 DO r := r * base
			base := base * base
			exp := exp RSHIFT 1 }
		rega := list(3, t_number, r)
		next11()
		RETURN }
	IF h2!rega=t_real DO
	{	rega := list(3, t_real, fpower(h3!rega, h3!regb) )
		IF floterr DO {	writes("*nOVERFLOW:")
					floterr := FALSE
					GOTO pwerr }
		next11()
		RETURN }
pwerr:	rega := list(3, t_number, 0)
	error1("****", t, regb)
	errdbg() }
        BREAK

      CASE i_pos:
{	split1()
	TEST h2!rega=t_number LOGOR h2!rega=t_real
	THEN {	rega := list(3, h2!rega, h3!rega )
		next11() }
	ELSE {	error1("+", rega, 0)
		rega := list(3, t_number, 0)
		errdbg() }}
        BREAK

      CASE i_neg:
{	LET t=rega
	split1()
	IF h2!rega=t_number DO
	{	rega := list(3, t_number, -h3!rega )
		next11()
		RETURN }
	IF h2!rega=t_real DO
	{	rega := list(3, t_real, fumin(h3!rega) )
		next11()
		RETURN }
	rega := list(3, t_number, 0)
	error1("-", t, 0)
	errdbg() }
        BREAK

      CASE s_eq:
{	LET t=rega
	split2()
	rega := equal(rega, regb) -> truerv, falserv
	TEST errflag
	THEN {	error1("EQ", t, regb)
		errflag := FALSE
		errdbg() }
	ELSE next11() }
        BREAK

      CASE i_ne:
{	LET t=rega
	split2()
	rega := equal(rega, regb) -> falserv, truerv
	TEST errflag
	THEN {	error1("NE", t, regb)
		rega := falserv
		errflag := FALSE
		errdbg() }
	ELSE next11() }

      CASE i_ls:
{	split2()
	IF testnumbs2()=t_number DO
	{	rega := h3!rega < h3!regb -> truerv, falserv
		next11()
		RETURN }
	IF testnumbs2()=t_real DO
	{	rega := fls(h3!rega, h3!regb) -> truerv, falserv
		next11()
		RETURN }
	error1("LS", rega, regb)
	rega := falserv
	errdbg() }
        BREAK

      CASE i_le:
{	split2()
	IF testnumbs2()=t_number DO
	{	rega := h3!rega <= h3!regb -> truerv, falserv
		next11()
		RETURN }
	IF testnumbs2()=t_real DO
	{	rega := fle(h3!rega, h3!regb) -> truerv, falserv
		next11()
		RETURN }
	error1("LE", rega, regb)
	rega := falserv
	errdbg() }
        BREAK

      CASE i_ge:
{	split2()
	IF testnumbs2()=t_number DO
	{	rega := h3!rega>=h3!regb -> truerv, falserv
		next11()
		RETURN }
	IF testnumbs2()=t_real DO
	{	rega := fge(h3!rega, h3!regb) -> truerv, falserv
		next11()
		RETURN }
	error1("GE", rega, regb)
	rega := falserv
	errdbg() }
        BREAK

      CASE i_gr:
{	split2()
	IF testnumbs2()=t_number DO
	{	rega := h3!rega > h3!regb -> truerv, falserv
		next11()
		RETURN }
	IF testnumbs2()=t_real DO
	{	rega := fgr(h3!rega, h3!regb) -> truerv, falserv
		next11()
		RETURN }
	error1("GR", rega, regb)
	rega := falserv
	errdbg() }
        BREAK

      CASE i_jump:       // Ln     Jump to label Ln
        pc := pc!1
        BREAK

      CASE i_jumpF:      // Ln     Pop a value from the stck and jump to
                       //        label Ln if it was False
	split1()
	IF h2!rega = t_false DO
	{	pc := pc!1
		RETURN }
	IF h2!rega = t_true DO
	{	pc := pc+2
		RETURN }
	error("NOT A TRUTHVALUE: ", a, 0, 0)
	pc := pc!1 - 1
	edbg()
        BREAK







      CASE i_apply:      //       Apply a tuple, a Sys node or a Closure
	split1()
	rega := h3!rega
	SWITCHON h2!rega INTO
	{	CASE t_closure:
				stackp := stackp+1
				oldc, pc := pc+1, h4!rega
				RETURN
		CASE t_tuple:
				stackp, rb := stackp-1, stack!(stackp)
				regb := h3!rb
				UNLESS h2!regb=t_number DO
				{	error(0, rega, " APPLIED TO ", regb)
					UNLESS h3!rega=0 DO rega := h4!rega
					errlvdbg()
					RETURN }
			{	LET n = h3!regb
				TEST 1 <= n <= h3!rega
				THEN {	rega := rega!(n+2)
					next11() }
				ELSE {	error(0, rega, " APPLIED TO ", regb)
					UNLESS h3!rega=0 DO
						TEST n >= 1
						THEN rega := rega!(h3!rega+2)
						ELSE rega := h4!rega
					errlvdbg() }
				RETURN }
		CASE t_basicfn:
				(h3!rega)()
				RETURN

		DEFAULT:	error("ATTEMPT TO APPLY ",rega," TO ",stack!(stackp-1))
				edbg() }
        BREAK
 
      CASE i_save:       // Ln    Create a new stack node of size Ln+6
                       //       with appropriate return link info.
	regb := node(pc!1+6)
	h1!regb, h2!regb := pc!1+6, stack
	h3!stack := stackp
	h4!regb, h5!regb := oldc, stack
	h6!regb, h7!regb := env, stack!(stackp-2)
	env := h3!rega
	stackp, stack := 7, b
	pc := pc+2
        BREAK

      CASE i_return:  //          Return from the current block
	rega := stack!(stackp-1)
	restart()
	stackp := stackp-1
	stack!(stackp-1) := rega
        BREAK

      CASE i_testEmpty:  //       Check that the argument of a parameterless
                       //       function is indeed Nil.
        split1()
	TEST h3!rega=nilrv
	THEN pc := pc+1
	ELSE {	error1("FUNCTION OF NO ARGUMENTS", rega, 0)
		edbg() }
        BREAK

      CASE i_lose1:      //        Pop one item from the stack
	split1()
	pc := pc+1
        BREAK

      CASE i_goto:       // Ln     Jump to label Ln
	split1()
	UNLESS h2!rega=t_label DO
	{ error("CANNOT GO TO ", rega, 0, 0)
	  rega := dummyrv
	  errdbg()
	  RETURN
	}
	pc, env := h4!rega, h6!rega
	stack := node(h3!rega)
	stackp := 6
	h1!stack, h2!stack := h3!rega, stack
	rega := h5!rega
	h4!stack, h5!stack, h6!stack := h4!rega, h5!rega, h6!rega
        BREAK

      CASE i_update:     // n      Update n Lvalues
      { LET n = pc!1
	split2()
	TEST n = 1
	THEN h3!b := rega
	ELSE { UNLESS h2!rega = t_tuple & h3!rega = n DO
	       { error("CONFORMALITY ERROR IN ASSIGNMENT",0,0,0)
		 writes("THE VALUE OF THE RHS IS: ")
		 printa(rega, tupledepth)
		 writechar(output, '*n')
		 writes("THE NUMBER OF VARIABLES ON THE LHS IS: ")
		 writen(n)
		 writechar(output, '*n')
		 pc := pc+1
		 rega := dummyrv
		 errdbg()
		 RETURN
	       }
	       regb := h3!regb
	       { LET v = VEC 100
		 FOR i=3 TO n+2 DO v!i := h3!(rega!i)
		 FOR i=3 TO n+2 DO h3!(regb!i) := v!i
	       }
	     }
	rega := dummyrv
	pc := pc+1
	next11()
        BREAK
      }
    }
  } REPEAT

  RESULTIS retcode
}

AND save() BE
{ regb := node(pc!1+6)
  h1!regb, h2!regb := pc!1+6, stack
  h3!stack := stackp
  h4!regb, h5!regb := oldc, stack
  h6!regb, h7!regb := env, stack!(stackp-2)
  env := h3!rega
  stackp, stack := 7, regb
  pc := pc+2
}

AND split1() BE
{ writef("split1: Called*n")
}


AND edbg() BE
{ restartpc := pc+1
  pc := @restart
  rega := list(3, t_lvalue, nilrv)
  comdbg()
}

AND errdbg() BE
{ restartpc := pc+1
  pc := @rvrestart
  rega := list(3, t_lvalue, rega)
  comdbg()
}


AND errlvdbg() BE
{ rega := list(3, t_lvalue, rega)
  errokdbg()
}

AND list(n, a, b, c, d, e, f,g) = VALOF
{ LET p = newvec(n)
  //IF FALSE DO
  { writef("list: n=%n", n)
    IF a DO writef(" a=%i6", a)
    IF b DO writef(" b=%i6", b)
    IF c DO writef(" c=%i6", c)
    IF d DO writef(" d=%i6", d)
    IF e DO writef(" e=%i6", e)
    IF f DO writef(" f=%i6", f)
    IF g DO writef(" g=%i6", g)
    newline()
  }
  
  UNLESS p DO
  { writef("list: More space needed*n")
    abort(999)
  }
  SWITCHON n INTO
  { DEFAULT:  writef("list: Bad n=%n*n",n)
    CASE 7: p!6 := f
    CASE 6: p!5 := e
    CASE 5: p!4 := d
    CASE 4: p!3 := c
    CASE 3: p!2 := b
    CASE 2: p!1 := a
    CASE 1: p!0 := n 
  }
}

AND errokdbg() BE
{ restartpc := pc+1
  pc := @okrestart
  comdbg()
}

AND comdbg() BE
{ h3!stack := stackp
  regb := node(8)
  h1!regb, h2!regb := 8, stack
  h4!regb, h5!regb := restartc, stack
  h6!regb, h7!regb := env, a
  stack := regb
  regb := h3!errorlv
  stackp := 7
  errct := errct + 1
  IF errct >= maxerr DO pc := 0 ////LV norestart
  UNLESS h2!regb = t_closure | h2!regb=t_basicfn DO
  { UNLESS errct >= maxerr DO
      writes("EXECUTION RESUMED*n*n")
    RETURN
  }
  TEST h2!regb=t_closure
  THEN { stack!(stackp) := errorlv
	 stackp := stackp+1
	 rega := regb
	 oldc, pc := pc, h4!regb }
  ELSE { pc := pc-3
	 nil()
	 formLvalue()
	 (h3!regb)() }
	 restartc := 0
       }

AND okrestart() BE
{ rega := stack!(stackp-1)
  restart()
  stack!(stackp) := rega
  stackp := stackp+1
}

AND rvrestart() BE
{ rega := stack!(stackp-1)
  restart()
  stack!(stackp) := h3!rega
  stackp := stackp+1
}

AND norestart() BE
{ writes("*nMAXIMUM NUMBER OF RUN-TIME ERRORS REACHED*n")
  terminate1()
}


AND printf(mem, form, p) BE
{ LET fmt = form+mem
  LET i = 0

  { LET k = fmt%i
    i := i+1
    IF k=0 RETURN
    IF k='%' DO
    { LET n = 0;
      { k := fmt%i
        i := i+1
        UNLESS '0'<=k<='9' BREAK
        n := 10*n + k - '0'
      } REPEAT
      SWITCHON k INTO
      { DEFAULT:  wrch(k); LOOP
        CASE 'd': writed  (!p,     n); p := p+1; LOOP
        CASE 's': wrs     (mem+!p, n); p := p+1; LOOP
        CASE 'x': writehex(!p,     n); p := p+1; LOOP
      }
    }
    wrch(k)
  } REPEAT
}

AND wrs(s, n) BE
{ LET len = 0
  WHILE s%len DO len := len+1
  FOR i = len+1 TO n DO wrch(' ')
  FOR i = 0 TO len-1 DO wrch(s%i)
}

AND hasOperand(op) = VALOF SWITCHON op INTO
{ //CASE Fnrn:CASE Rtrn:CASE Lres:CASE i_halt:
  //CASE Vecap:CASE Ind:CASE Stind:CASE s_neg:CASE s_not:
  //CASE s_mult:CASE s_div:CASE Mod:CASE s_plus:CASE s_minus:
  //CASE s_eq:CASE s_ne:CASE s_le:CASE s_ge:CASE s_ls:CASE s_gr:
  //CASE Lsh:CASE Rsh:CASE s_and:CASE Or:CASE Xor:
            RESULTIS FALSE

  DEFAULT:  RESULTIS TRUE
}



