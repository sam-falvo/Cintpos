/*

This program reads a file containing the results of using bugify with
different seeds. It output a file containing the frequencies of the different
outcomes.

This program is based on the lexical anaylser used in vspl.b.

(c) Martin Richards 27 July 2021
*/


GET "libhdr"
 
MANIFEST {  // Lexical tokens, parse tree operators and op-codes

s_num=1; s_name; s_eof
s_L   // Bug detected in the lexical analyser
s_S   // Bug detected in the syntax analyser
s_T   // Bug detected in the translation phase
s_C   // Bug detected in the codegenerator
s_N   // No bug detected during compilation
s_NF  // Failed to compiler and run cmpltest.b with the modified compiler
s_NS  // Successfully compiled and ran cmpltest.b with the modified compiler
s_NSX // Successfully compiled and ran cmpltest.b but the modified compiler
      // does have a bug not detected by cmpltest.b.

countvupb = s_NSX
}
 
GLOBAL { 
rec_p:ug; rec_l; fin_p; fin_l
fatalerr; synerr; errcount; errmax
fromfilename; tofilename
fromstream; tostream
newvec; treep; treevec
optTokens

// Globals used in LEX
chbuf; charv; ch; rch; lex; token; lexval; wordnode
wrchbuf; chcount; lineno
dsw; declsyswords; nametable; lookupword
rdtag
readdata
rdprog
opstr
countv
trials
}

MANIFEST {                         //  Selectors
h1=0; h2=1; h3=2
nametablesize =  41
}
 
LET start() = VALOF
{ LET treesize = 0
  AND argform = "FROM,TO=-o/K,TOKENS=-l/S"
  LET stdout = output()
  AND argv = VEC 50
  AND cv = VEC countvupb  FOR i = 0 TO countvupb DO cv!i := 0
  countv := cv
  
  errmax   := 2
  errcount := 0
  fin_p, fin_l := level(), fin

  treevec := 0
  fromfilename := "bugbcplferesults.txt"
  tofilename := 0
  fromstream, tostream := 0, 0
   
  writef("*nbugstats (30 Jul 2021)*n*n")
 
  IF rdargs(argform, argv, 50)=0 DO fatalerr("Bad arguments*n")

  treesize := 1000

  IF argv!0 DO fromfilename := argv!0      // FROM
  IF argv!1 DO tofilename := argv!1        // TO=-o/K
  optTokens := argv!2                      // TOKENS=-l/S

  treevec := getvec(treesize)

  UNLESS treevec DO fatalerr("Insufficient memory*n")
   
  fromstream := findinput(fromfilename)
  UNLESS fromstream DO fatalerr("Trouble with file %s*n", fromfilename)

  IF tofilename
  DO { tostream := findoutput(tofilename)
       IF tostream=0 DO fatalerr("Trouble with TO file %s*n", tofilename)
     }
  UNLESS tostream DO tostream := stdout

  selectinput(fromstream)
  selectoutput(tostream)

  { LET tree = 0
    LET b = VEC 64/bytesperword
    chbuf := b
    FOR i = 0 TO 63 DO chbuf%i := 0
    chcount, lineno := 0, 1
    rch()
 
    treep := treevec + treesize
    trials := 0
    
    tree := readdata()
    IF optTokens GOTO fin

    writef("*nThe results after %n trials*n*n", trials)
    FOR i = s_L TO s_NSX DO writef("%t5 %i5 = %5.1d%%*n",
                                    opstr(i), countv!i, countv!i*1000 / trials)
    
    IF errcount GOTO fin
  }
   
fin:
  IF treevec       DO freevec(treevec)
  IF fromstream    DO endstream(fromstream)
  IF tostream      DO UNLESS tostream=stdout DO  endstream(tostream)

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
                token := s_num
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
 
    CASE '#':   { rch()
                  IF ch=endstreamch BREAK
		  wrch(ch)
                } REPEATUNTIL ch='*n'
                LOOP
 
    DEFAULT:    UNLESS ch=endstreamch DO
                { LET badch = ch
                  ch := '*s'
                  synerr("Illegal character '%c' %x2", badch, badch)
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
{ dsw("L",   s_L)
  dsw("S",   s_S)
  dsw("T",   s_T)
  dsw("C",   s_C)
  dsw("N",   s_N)
  dsw("NF",  s_NF)
  dsw("NS",  s_NS)
  dsw("NSX", s_NSX)
} 
 
LET rch() BE
{ ch := rdch()
  chcount := chcount+1
  chbuf%(chcount&63) := ch
}
 
AND wrchbuf() BE
{ writes("*n...")
  FOR p = chcount-20 TO chcount DO
  { LET k = chbuf%(p&63)
    IF 0<k<255 DO wrch(k)
  }
  newline()
}
 
AND rdtag() = VALOF
{ LET len = 0
  WHILE 'a'<=ch<='z' | 'A'<=ch<='Z' | '0'<=ch<='9' |  ch='_' DO
  { len := len+1
    IF len>255 DO synerr("s_name too long")
    charv%len := ch
    rch()
  }
  charv%0 := len
  RESULTIS charv
}
 
LET newvec(n) = VALOF
{ treep := treep - n - 1;
  IF treep<=treevec DO fatalerr("More workspace needed")
  RESULTIS treep
}
 
AND readdata() = VALOF
{ LET res = 0
  rec_p, rec_l := level(), recover

  charv := newvec(256/bytesperword)     
  nametable := newvec(nametablesize)
  UNLESS charv & nametable DO fatalerr("More workspace needed")
  FOR i = 0 TO nametablesize DO nametable!i := 0
  declsyswords()
  lex()

  IF optTokens DO            // For debugging lex.
  { IF token=s_eof RESULTIS 0
    writef("token = %i3 %s", token, opstr(token))
    IF token=s_num    DO writef("       %n",  lexval)
    IF token=s_name   DO writef("      %s",   charv)
    newline()
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

LET rdprog() = VALOF
{ LET ln = lineno
  SWITCHON token INTO
  { DEFAULT:  synerr("Unexpected token %s*n", opstr(token))

    CASE s_eof: RESULTIS 0

    CASE s_name: synerr("Unexpected Name %s*n", @h3!wordnode)
    
    CASE s_num:  lex()  // Ignore the seed value
                 LOOP
    CASE s_L:
    CASE s_S:
    CASE s_T:
    CASE s_C:
    CASE s_N:
    CASE s_NF:
    CASE s_NS:
    CASE s_NSX:  countv!token := countv!token + 1
                 trials := trials + 1
		 //writef("token=%n countv!%n=%n trials=%n*n",
		 //        token, token, countv!token, trials)   
                 lex()
    
  }
} REPEAT

LET opstr(op) = VALOF SWITCHON op INTO
{ DEFAULT:       RESULTIS "Unknown"

  CASE s_num:    RESULTIS "Num"
  CASE s_name:   RESULTIS "Name"
  CASE s_eof:    RESULTIS "Eof"
  CASE s_L:      RESULTIS "L"
  CASE s_S:      RESULTIS "S"
  CASE s_T:      RESULTIS "T"
  CASE s_C:      RESULTIS "C"
  CASE s_N:      RESULTIS "N"
  CASE s_NF:     RESULTIS "NF"
  CASE s_NS:     RESULTIS "NS"
  CASE s_NSX:    RESULTIS "NSX"
}

