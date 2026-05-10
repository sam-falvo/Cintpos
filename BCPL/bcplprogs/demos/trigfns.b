/*
This is a demonstration of how to parse simple expressions involving
trig function. It also prints out the parse trees resulting from
various test cases. It is based on the vspl compiler in the VSPL
distribution.


(c) Martin Richards 30 December 2017
*/


GET "libhdr"
 
MANIFEST {  // Lexical tokens and parse tree operators.

Num=1; Name;
Eof
Neg; Pow; Mul; Div; Add; Sub
Eq
Lparen; Rparen

Arccos
Arccse
Arcsec
Arcsin
Arctan
Cos
Cot
Cse
Exp
Lg
Ln
Log
Prime
Sec
Sin
Sqrt
Sub
Tan
}
 
GLOBAL { 
rec_p:ug; rec_l; fin_p; fin_l
fatalerr; synerr; trnerr; errcount; errmax
tostream
mk1; mk2; mk3; mk4; mk5; mk6
newvec; treep; treevec
optTokens

// Globals used in LEX
chbuf; charv; ch; rch; lex; token; lexval; wordnode
wrchbuf; chcount; lineno
dsw; declsyswords; namestart; nametable; lookupword
rdstrch; rdtag

// Globals used in SYN
checkfor
rname
formtree; plist
rnexp; rexp; rbexp
opstr
try
expstr; strp; strlen
}
 
MANIFEST {                         //  Selectors
h1=0; h2=1; h3=2; h4=3; h5=4; h6=5
nametablesize = 541
treesize = 10000
}
 
LET start() = VALOF
{ LET argv = VEC 50
  AND argform = "TO=-o/K,TOKENS=-l/S"
  LET stdout = output()

  errmax   := 2
  errcount := 0
  fin_p, fin_l := level(), fin

  treevec := 0
  tostream := 0
   
  IF rdargs(argform, argv, 50)=0 DO fatalerr("Bad arguments*n")

  IF argv!0 DO                         // TO      -o
  { tostream := findoutput(argv!0)
    IF tostream=0 DO fatalerr("Trouble with code file %s*n", argv!0)
  }

  optTokens := argv!1                  // TOKENS  -l

  treevec := getvec(treesize)

  UNLESS treevec DO
     fatalerr("Insufficient memory*n")
   
  UNLESS tostream DO tostream := stdout
  selectoutput(tostream)

  writef("*nTrig expressions*n")
 
  try("y = x^7")
  try("y = 1 / x^5")
//GOTO fin
  try("y = 1 / x''")
  try("y = cos x")
  try("y = tan x")
  try("y = cot x")
  try("y = cse x")
  try("y = sec x")
  try("y = exp x")
  try("y = a^x")
  try("y = (sin x)^2")
  try("y = (cos x)^2")
  try("y = (1 - x^2) ** tan x")
  try("y = sec x ** tan x")
  try("y = sec x ** (log x - 3 ** x^2)")
  try("y = cot x ** ( sqrt x - 4 ** exp x)")
  try("y = exp x ** sin x")
  try("y = (x^2 + 3 ** x) ** exp x")
  try("y = x ** exp x ** sin x")
  try("y = x ** arctan x")
  try("y = x^3 ** arccos x")
  try("y = sqrt x ** arccos x")
  try("y = exp x ** arccse x")
  try("y = exp x / ( exp x + 1)")
  try("y = log x / (1 + log x)")
  try("y = (x + sin x) / (x + cos x)")
  try("y = log x / (1 + sqrt x)")
  try("y = x ** tan x / ( sec x + tan x)")
  try("y = (exp x - 1) / (exp x + 1)")
  try("y = 2^x ** cot x / sqrt x")
  try("y = arccos((x^2-1)/(x^2+1))")
  try("y = log x + ln x + lg x")

fin:
  IF treevec       DO freevec(treevec)
  IF tostream      DO { selectoutput(tostream)
                        UNLESS tostream=stdout DO  endwrite() }

  selectoutput(stdout)
  RESULTIS errcount=0 -> 0, 20
}

LET lex() BE
{ lex1()
  //writef("lex: token=%n %s", token, opstr(token))
  //IF token=Num    DO writef("       %n",  lexval)
  //IF token=Name   DO writef("      %s",   charv)
  //newline()
}

AND lex1() BE
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
 
    CASE '(': token := Lparen;    BREAK
    CASE ')': token := Rparen;    BREAK 
    CASE '+': token := Add;       BREAK
    CASE '-': token := Sub;       BREAK
    CASE '=': token := Eq;        BREAK
    CASE '**':token := Mul;       BREAK
    CASE '^': token := Pow;       BREAK
 
    CASE '/':   rch()
                IF ch='/' DO
                { rch() REPEATUNTIL ch='*n' | ch=endstreamch
                  LOOP
                }
                token := Div
                RETURN
 
    CASE '*'':  token := Prime;     BREAK
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
{ dsw("arccos", Arccos)  
  dsw("arccse", Arccse)  
  dsw("arcsec", Arcsec)  
  dsw("arctan", Arctan)  
  dsw("cos", Cos)  
  dsw("cot", Cot)  
  dsw("cse", Cse)  
  dsw("exp", Exp)  
  dsw("lg", Lg)  
  dsw("ln", Ln)  
  dsw("log", Log)  
  dsw("sec", Sec)  
  dsw("sin", Sin)  
  dsw("sqrt", Sqrt)  
  dsw("tan", Tan)  
} 
 
LET rch() BE
{ // Read characters from expstr
  TEST strp>strlen
  THEN { ch := endstreamch
       }
  ELSE { ch := expstr%strp
         strp := strp+1
       }
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
 
LET newvec(n) = VALOF
{ treep := treep - n - 1;
  IF treep<=treevec DO fatalerr("More workspace needed")
  RESULTIS treep
}
 
AND mk1(a) = VALOF
{ LET p = newvec(0)
  p!0 := a
//writef("mk1: %i5 => [%n]*n", p, a)
  RESULTIS p
}
 
AND mk2(a, b) = VALOF
{ LET p = newvec(1)
  p!0, p!1 := a, b
//writef("mk2: %i5 => [%n, %n]*n", p, a, b)
  RESULTIS p
}
 
AND mk3(a, b, c) = VALOF
{ LET p = newvec(2)
//writef("mk3: %i5 => [%n, %n, %n]*n", p, a, b, c)
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
    newline()
    IF token=Eof RESULTIS 0
    lex()
  } REPEAT

recover:
  res := rdformula()
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

AND checkfor(tok, mess) BE
{ UNLESS token=tok DO synerr(mess)
  lex()
}
 
AND rdformula() = VALOF
{ LET ln = lineno
  SWITCHON token INTO
  { DEFAULT:  synerr("Bad trig expression*n")

    CASE Eof: RESULTIS 0

    CASE Name:
               { LET n = wordnode
                 lex()
                 checkfor(Eq, "'=' expected")
                 
                 RESULTIS  mk3(Eq, n, rexp(0))
               }
  }
}

AND rname() = VALOF
{ LET a = wordnode
  checkfor(Name, "Name expected")
  RESULTIS a
}
 
AND rbexp() = VALOF
{ LET a, op, ln = 0, token, lineno
 
  SWITCHON op INTO
 
  { DEFAULT: synerr("Error in expression")

    CASE Name:   a := wordnode
                 lex()
                 RESULTIS a
 
    CASE Num:    a := mk2(Num, lexval)
                 lex()
                 RESULTIS a
 
    CASE Lparen: a := rnexp(0)
                 checkfor(Rparen, "')' missing")
                 RESULTIS a
 
    CASE Add:    RESULTIS rnexp(5)
 
    CASE Sub:    a := rnexp(5)
                 TEST h1!a=Num THEN h2!a := - h2!a
                               ELSE a := mk2(Neg, a)
                 RESULTIS a

    CASE Arccos:
    CASE Arccse:
    CASE Arcsec:
    CASE Arcsin:
    CASE Arctan:
    CASE Cos:
    CASE Cot:
    CASE Exp:
    CASE Sin:
    CASE Sec:
    CASE Tan:
    CASE Cse:
    CASE Lg:
    CASE Ln:
    CASE Log:
    CASE Sqrt:
//writef("rbexp: token=%n %s*n", op, opstr(op))
                 RESULTIS mk2(op, rnexp(3))
 
   }
}
 
AND rnexp(n) = VALOF { lex(); RESULTIS rexp(n) }
 
AND rexp(n) = VALOF
{ LET a, b, p = rbexp(), 0, 0

  { LET op, ln = token, lineno
    SWITCHON op INTO
 
    { DEFAULT:      BREAK
 
      CASE Prime:   a :=  mk2(Prime, a)
                    lex()
                    LOOP

      CASE Pow:     p := 3;              ENDCASE
      CASE Mul:CASE Div:
                    p := 2;              ENDCASE
      CASE Add:CASE Sub:
                    p := 1;              ENDCASE
    }
      
    IF n>=p RESULTIS a
    a := mk3(op, a, rnexp(p))
  } REPEAT

  RESULTIS a
}
  

LET opstr(op) = VALOF SWITCHON op INTO
{ DEFAULT:       RESULTIS "Unknown"

  CASE Add:      RESULTIS "Add"
  CASE Arccos:   RESULTIS "Arccos"
  CASE Arccse:   RESULTIS "Arccse"
  CASE Arcsec:   RESULTIS "Arcsec"
  CASE Arcsin:   RESULTIS "Arcsin"
  CASE Arctan:   RESULTIS "Arctan"
  CASE Cos:      RESULTIS "Cos"
  CASE Cot:      RESULTIS "Cot"
  CASE Cse:      RESULTIS "Cse"
  CASE Div:      RESULTIS "Div"
  CASE Eof:      RESULTIS "Eof"
  CASE Eq:       RESULTIS "Eq"
  CASE Exp:      RESULTIS "Exp"
  CASE Lg:       RESULTIS "Lg"
  CASE Ln:       RESULTIS "Ln"
  CASE Log:      RESULTIS "Log"
  CASE Lparen:   RESULTIS "Lparen"
  CASE Mul:      RESULTIS "Mul"
  CASE Name:     RESULTIS "Name"
  CASE Neg:      RESULTIS "Neg"
  CASE Num:      RESULTIS "Num"
  CASE Pow:      RESULTIS "Pow"
  CASE Prime:    RESULTIS "Prime"
  CASE Rparen:   RESULTIS "Rparen"
  CASE Sec:      RESULTIS "Sec"
  CASE Sin:      RESULTIS "Sin"
  CASE Sqrt:     RESULTIS "Sqrt"
  CASE Sub:      RESULTIS "Sub"
  CASE Tan:      RESULTIS "Tan"

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

    CASE Pow:
    CASE Mul: CASE Div: CASE Add: CASE Sub:
    CASE Eq:
         size     := 3;       ENDCASE

    CASE Neg: CASE Prime:
    CASE Arccos:
    CASE Arccse:
    CASE Arcsec:
    CASE Arcsin:
    CASE Arctan:
    CASE Cos:
    CASE Cot:
    CASE Exp:
    CASE Sin:
    CASE Sec:
    CASE Tan:
    CASE Cse:
    CASE Lg:
    CASE Ln:
    CASE Log:
    CASE Sqrt:
         size     := 2;       ENDCASE
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

AND try(str) BE
{ LET tree = 0
  LET b = VEC 64/bytesperword
  chbuf := b
  FOR i = 0 TO 63 DO chbuf%i := 0
  chcount, lineno := 0, 1
  
  expstr, strp, strlen := str, 1, str%0
  rch()
 
  treep := treevec + treesize

  writef("*nExpression: %s*n", str)

  tree := formtree()              // Perform Syntax Analysis

  UNLESS tree DO
  { writef("No tree formed*n")
    RETURN
  }
  writef("Parse tree:*n")
  plist(tree, 0, 20)
  newline()
}

/*
This program can be compiled and run generating the following output.

solestreet:$ 
solestreet:$ cd ~/distribution/BCPL/bcplprogs/demos
solestreet:$ cintsys

BCPL 32-bit Cintcode System (21 Oct 2015)
0.000> c b trigfns
bcpl trigfns.b to trigfns hdrs BCPLHDRS t32 

BCPL (10 Oct 2014) with simple floating point
Code size =  4740 bytes of 32-bit little ender Cintcode
0.050> trigfns to junk.txt
0.020> type junk.txt

Trig expressions

Expression: y = x^7
Parse tree:
Eq
*-y
*-Pow
  *-x
  *-7

Expression: y = 1 / x^5
Parse tree:
Eq
*-y
*-Div
  *-1
  *-Pow
    *-x
    *-5

Expression: y = 1 / x''
Parse tree:
Eq
*-y
*-Div
  *-1
  *-Prime
    *-Prime
      *-x

Expression: y = cos x
Parse tree:
Eq
*-y
*-Cos
  *-x

Expression: y = tan x
Parse tree:
Eq
*-y
*-Tan
  *-x

Expression: y = cot x
Parse tree:
Eq
*-y
*-Cot
  *-x

Expression: y = cse x
Parse tree:
Eq
*-y
*-Cse
  *-x

Expression: y = sec x
Parse tree:
Eq
*-y
*-Sec
  *-x

Expression: y = exp x
Parse tree:
Eq
*-y
*-Exp
  *-x

Expression: y = a^x
Parse tree:
Eq
*-y
*-Pow
  *-a
  *-x

Expression: y = (sin x)^2
Parse tree:
Eq
*-y
*-Pow
  *-Sin
  ! *-x
  *-2

Expression: y = (cos x)^2
Parse tree:
Eq
*-y
*-Pow
  *-Cos
  ! *-x
  *-2

Expression: y = (1 - x^2) * tan x
Parse tree:
Eq
*-y
*-Mul
  *-Sub
  ! *-1
  ! *-Pow
  !   *-x
  !   *-2
  *-Tan
    *-x

Expression: y = sec x * tan x
Parse tree:
Eq
*-y
*-Mul
  *-Sec
  ! *-x
  *-Tan
    *-x

Expression: y = sec x * (log x - 3 * x^2)
Parse tree:
Eq
*-y
*-Mul
  *-Sec
  ! *-x
  *-Sub
    *-Log
    ! *-x
    *-Mul
      *-3
      *-Pow
        *-x
        *-2

Expression: y = cot x * ( sqrt x - 4 * exp x)
Parse tree:
Eq
*-y
*-Mul
  *-Cot
  ! *-x
  *-Sub
    *-Sqrt
    ! *-x
    *-Mul
      *-4
      *-Exp
        *-x

Expression: y = exp x * sin x
Parse tree:
Eq
*-y
*-Mul
  *-Exp
  ! *-x
  *-Sin
    *-x

Expression: y = (x^2 + 3 * x) * exp x
Parse tree:
Eq
*-y
*-Mul
  *-Add
  ! *-Pow
  ! ! *-x
  ! ! *-2
  ! *-Mul
  !   *-3
  !   *-x
  *-Exp
    *-x

Expression: y = x * exp x * sin x
Parse tree:
Eq
*-y
*-Mul
  *-Mul
  ! *-x
  ! *-Exp
  !   *-x
  *-Sin
    *-x

Expression: y = x * arctan x
Parse tree:
Eq
*-y
*-Mul
  *-x
  *-Arctan
    *-x

Expression: y = x^3 * arccos x
Parse tree:
Eq
*-y
*-Mul
  *-Pow
  ! *-x
  ! *-3
  *-Arccos
    *-x

Expression: y = sqrt x * arccos x
Parse tree:
Eq
*-y
*-Mul
  *-Sqrt
  ! *-x
  *-Arccos
    *-x

Expression: y = exp x * arccse x
Parse tree:
Eq
*-y
*-Mul
  *-Exp
  ! *-x
  *-Arccse
    *-x

Expression: y = exp x / ( exp x + 1)
Parse tree:
Eq
*-y
*-Div
  *-Exp
  ! *-x
  *-Add
    *-Exp
    ! *-x
    *-1

Expression: y = log x / (1 + log x)
Parse tree:
Eq
*-y
*-Div
  *-Log
  ! *-x
  *-Add
    *-1
    *-Log
      *-x

Expression: y = (x + sin x) / (x + cos x)
Parse tree:
Eq
*-y
*-Div
  *-Add
  ! *-x
  ! *-Sin
  !   *-x
  *-Add
    *-x
    *-Cos
      *-x

Expression: y = log x / (1 + sqrt x)
Parse tree:
Eq
*-y
*-Div
  *-Log
  ! *-x
  *-Add
    *-1
    *-Sqrt
      *-x

Expression: y = x * tan x / ( sec x + tan x)
Parse tree:
Eq
*-y
*-Div
  *-Mul
  ! *-x
  ! *-Tan
  !   *-x
  *-Add
    *-Sec
    ! *-x
    *-Tan
      *-x

Expression: y = (exp x - 1) / (exp x + 1)
Parse tree:
Eq
*-y
*-Div
  *-Sub
  ! *-Exp
  ! ! *-x
  ! *-1
  *-Add
    *-Exp
    ! *-x
    *-1

Expression: y = 2^x * cot x / sqrt x
Parse tree:
Eq
*-y
*-Div
  *-Mul
  ! *-Pow
  ! ! *-2
  ! ! *-x
  ! *-Cot
  !   *-x
  *-Sqrt
    *-x

Expression: y = arccos((x^2-1)/(x^2+1))
Parse tree:
Eq
*-y
*-Arccos
  *-Div
    *-Sub
    ! *-Pow
    ! ! *-x
    ! ! *-2
    ! *-1
    *-Add
      *-Pow
      ! *-x
      ! *-2
      *-1

Expression: y = log x + ln x + lg x
Parse tree:
Eq
*-y
*-Add
  *-Add
  ! *-Log
  ! ! *-x
  ! *-Ln
  !   *-x
  *-Lg
    *-x
0.010> 
0.000> 
*/
