/*
########## UNDER DEVELOPMENT #############

This is a compiler and interpreter for the language PAL
implemented in BCPL, based on the version for the IBM 360
at MIT last modified by R Mabee in June 1970.

Substatially modified to run under modern Cintcode BCPL
(c) Martin Richards 04 Mar 2024

Usage:

pal  "PROG/A,TO=-o/K,TOKENS=-l/S,TREE=-p/S,CODE=-c/S,TRACE=-t/S"

   PROG   gives the filename of the PAL program to run, eg test.pal
-o TO     gives the filename of the output
-l TOKENS is a switch to test the lexical analyser
-p TREE   causes the parse tree to be output
-c CODE   outputs the compiled blackboard evaluator code
-t TRACE  Traces the execution of the blackboard evaluator.

History

25/03/2024
Still working on the interpreter in pal.b based on xpal70.

25/02/2024
Started modifying this compiler to make it follow the syntax and
POCODE of the Pal70 compiler whose original compiler listing is in
PAL/pal/docs/pal70-mabee-17jun70.pdf and the xpal files in PAL.

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
h1=0; h2; h3; h4; h5; h6; h7

// Lexical tokens and parse tree operators
s_and=1
s_apply
s_ass
s_aug
s_colon
s_comma
s_cond
s_def
s_div
s_do
s_dot
s_dummy  // Not in pal70
s_else   // Not in pal70
s_eof
s_eq
s_false  // Not in pal70
s_ge
s_goto
s_gr
s_if
s_ifnot
s_ifso
s_in
s_int
s_jj
s_lab
s_lambda
s_lcurly
s_le
s_let
s_logand
s_logor
s_lparen
s_ls
s_minus
s_mpt
s_mult
s_name
s_ne
s_neg
s_nil
s_noshare
s_not
s_paren
s_percent
s_pling   // '!' Not in pal70
s_plus
s_power
s_rcurly
s_real
s_rec
s_res
s_rparen
s_seq
s_stringconst
s_test
s_then
s_true   // Not in pal70
s_tuple
s_valdef
s_valof
s_where
s_while
s_within

s_nameres

// POCODE operators
i_apply
i_aug
i_blocklink
i_decllabel
i_declvar
i_declvars
i_div
i_dummy   // Not in Xpal70
i_eq
i_false   // Not in Xpal70
i_finish
i_formClosure
i_formLvalue
i_formRvalue
i_ge
i_goto
i_gr
i_halt    // Not in Xpal70
i_initvar
i_initvars
i_jj      // Not in xpal70
i_jump
i_jumpF;
i_le
i_loadE
i_loadF
i_loadGuess
i_loadJ   // Not in Xpal70
i_loadL
i_loadN
i_loadR
i_loadS
i_logand
i_logor
i_lose1
i_ls
i_members
i_minus
i_mult
i_ne
i_neg
i_nil
i_norestart
i_not
i_okrestart
i_plus
i_power
i_res
i_reslink
i_restart
i_restoreE1
i_result  // Not in xpal70
i_return
i_rvrestart
i_save
i_setlabEs
i_setup
i_testEmpty;
i_true    // Not in Xpal70
i_tuple   // Not in Xpal70
i_update
i_lineno  // Not in Xpal70

//Integer; Lab; Param; Equ // Pocode operators only used
                           // by xpal70 loader.

t_basicfn
t_closure
t_dummy;
t_env
t_false
t_guess
t_int
t_jj
t_label
t_loadGuess
t_lvalue
t_nil
t_nils;
t_real
t_stack
t_string
t_true
t_tuple

// Translation symbols
m_val=0; m_ref  // L and R mode

bytemax=255
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
tree
token; decval; fltval; exponent
wordnode
nilnode; truenode; falsenode; dummynode; mptnode
wrchbuf; chcount; lineno; nextlineno
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
loaddefinee; declguesses; initvars; transscope
mapb; mapf; length; upssp
trcom; decldyn
declstatnames; checkdistinct; addname; cellwithname
trdecl; jumpcond
assign; load; fnbody; loadlist; transname
dvec; dvece; dvecp; dvect
comline; procname; resultlab; ssp; mssp
outf; outfvar; outname; outvar; outnamepos; outstring; outfv; outfn; outfvarl
outfnn; outfl; outfs; outentry
outlab; outlabset; outvars; outstatvec; outssp
opstr; Operands

mem; memt
codev; codep; codet

datavupb
datav
palsxv
prevdatav
lookupcounter // For the t_int node used by LOOKUPNO

// Runtime state
pc      // Abs address of the next instruction in codev
oldc    // Abs address of a location in codev or zero
sp      // Address of 2nd from top of stack rel to stack
asp     // Abs address to top of 2nd from top of stack
        // = datav + stack + sp, As a check it is reset
	// to #xABABABAB whenever datav changes
	
currlineo // Generate i_lineno when the line number changes

// Abs addresses of locations within datav
env     // Address rel to datav to [5, Env, link, name, value]
rega    // Address rel to datav or zero
regb    // Address rel to datav or zero
stack   // Address rel to datav

// Pointers to nodes in datav held in runtime nodes such
// as stacks are relative to datav (not absolute).

count
tstasp
prstate
prvalue
printa
printb

labv; refv; labmax; putc; putd; putref
setlab; setlabval; nextlab; labnumber; resolvelabels
execpal; printf
writechar

gclimit     // call the garbage collector when h1!data>gclimit
garbcollect // If garbage collection does not recover sufficient
            // space increase gclimit somewhat.
gc

// PAL runtime globals
retcode

//stackp
dummyrv
nilrv
nilsrv
truerv
falserv
guessrv

list     // Allocate runtime n0des of size 1 to 7
node     // Allocate runtime n0des of any size

storage; storaget
xpal
control
strvupb; strv // Self expanding vector of distinct variable names
              // strv is zero or strv!0 is the subscript of the
	      // last element in strv. Items in strv are of the form
	      // [ legth in words, <packed string> ] eg
	      // p:   3          Item length in words
	      // p+1: 43424104   BCPL string "ABCD"
	      // p+2: 00000044
	      // p+3:            Start of next item
	      // The last item has a length field of zero
sxvstr        // This will point to strvupb

str2varno     // This function has an argument which is a name or
              // string node. It looks it up in sxvstr, adding it
	      // if necessary, returning the position in strv
	      // where the characters of the name or string are
	      // stored. It returns this position as the result.
	      // This is typically called the variable number
	      // of the name or string.
sxvpush

codefilep
namechain
mapliblist
parv
reft
r_finish
listv; listp; listt; listl
linev; linep; linet
gcmark
lookupno
lookupnovarno
resvarno
nset
time_exceeded
timeovfl
terminate
codefile
refp
stof
stackwarning
gcdbg
writex
dataflag
lvch
readch
//q
errlvdbg
edbg
errdbg
errokdbg
done

f_apply
f_atom
f_aug
f_blocklink
f_conc
f_cton
f_decllabel
f_declvar
f_declvars
f_decllib
f_diagnose
f_div
f_dummy
f_eq
f_false
f_finish
f_formClosure
f_formLvalue
f_formRvalue
f_ftos
f_ge
f_goto
f_gr
f_halt
f_initvar
f_initvars
f_isdummy
f_isenvironment
f_isfunction
f_islabel
f_isnumber
f_isreal
f_isstring
f_istruthvalue
f_istuple
f_itoc
f_itor
f_jump
f_jumpF
f_lastfn
f_le
f_length
f_libname
f_loadE
f_loadF
f_loadF   // Load real
f_loadGuess
f_loadJ
f_loadL
f_loadN   // Load int
f_loadR
f_loadS
f_logand
f_logor
f_lookupinE
f_lose1
f_ls
f_members
f_minus
f_mult
//f_name
f_ne
f_neg
f_nil
f_not
f_ntor
f_null
//f_number
f_plus
f_power
f_print
f_rdchar
f_reslink
f_restoreE1
f_res
f_return
f_rtoi
f_rton
f_rvrestart
f_save
f_saveenv
f_setlabEs
f_setup
f_share
f_stem
f_stern
f_ston
//f_stringconst
f_table
f_testEmpty
f_true
f_tuple
f_update
f_userpage

finishLoc
restartLoc
rvrestartLoc
okrestartLoc
norestartLoc
startLoc


save
push
pop
lvofname
pushlva
mklvnode
nextlv11
blocklink
error
error1
errflag
testbools2
nameres
resstr
return
testnumbs2
fpower
fmult
fdiv
fadd
fsub
fumin
equal
feq
fls
fle
fge
fgr
floterr
restart
tupledepth
printa
//writechar
restartc
rvrestart
okrestart
errorlv
////////
errcount
maxerr
nil
formLvalue
terminate1
restartpc
xpend; xpendlevel

//glob #####
}

MANIFEST {                         //  Selectors
nametablesize = 541
c_tab         =   9
c_newline     =  10
}

//LET abort(n) BE writef("*nabort(%n) called*n", n)

LET start() = VALOF
{ LET treesize = 0
  AND codesize = 0
  AND argv = VEC 50
  AND argform =
        "PROG/A,TO=-o/K,TOKENS=-l/S,TREE=-p/S,CODE=-c/S,TRACE=-t/S"
  LET stdout = output()

  tupledepth := 3
  errmax   := 10
  errcount := 0
  fin_p, fin_l := level(), fin

  treevec, labv, refv, mem := 0, 0, 0, 0
  progstream := 0 // The pal program
  tostream := 0
   
  writef("*nPAL (12 Mar 2024)*n")
 
  IF rdargs(argform, argv, 50)=0 DO fatalerr("Bad arguments*n")

  treesize := 10000
  codesize := 50000

  progstream := findinput(argv!0)      // PROG  The Pal prgram

  UNLESS progstream DO fatalerr("Trouble with file %s*n", argv!0)

  selectinput(progstream)
 
  IF argv!1                            // TO      -o
  DO { tostream := findoutput(argv!1)
       IF tostream=0 DO
         fatalerr("Trouble with code file %s*n", argv!1)
     }

  optTokens := argv!2                  // TOKENS  -l
  optTree   := argv!3                  // TREE    -p
  optCode   := argv!4                  // CODE    -c
  optTrace  := argv!5                  // TRACE   -t

  treevec := getvec(treesize)

  codev   := getvec(codesize)  // For the compiled Pocode
  codep   := 0                 // It should be replaced by
  codet   := codesize          // a self expanding vector

  strvupb, strv := 0, 0  // Initialise the sxvstr selfexpanding vector.
  sxvstr := @ strvupb
  sxvpush(sxvstr, 0)
  
  // Initialise the Pal runtime data space

  datavupb := 0  // Self expanding vector control block
  datav    := 0  // Only changed by calls of node or list
  palsxv   := @datavupb
  prevdatav := 0

  labv := getvec(2000)
  refv := getvec(2000)
  
  labmax := 2000

  UNLESS treevec & codev & labv & refv DO
     fatalerr("Insufficient memory*n")
   
  UNLESS tostream DO tostream := stdout
  selectoutput(tostream)

  chbuf := getvec(64/bytesperword)
  FOR i = 0 TO 63 DO chbuf%i := 0
  chcount, lineno := 0, 1
  token, wordnode := 0, 0
  rch()
 
  treep := treevec + treesize

  tree := formtree()              // Perform Syntax Analysis

  IF optTokens GOTO fin

  IF optTree DO { writes("*nParse Tree*n*n")
                  plist(tree, 0, 20)
                  newline()
		  GOTO fin
		  //abort(1000)
                }
  
  IF errcount GOTO fin

  FOR i = 0 TO codet DO codev!i := 0

  trprog(tree)                    // Translate the tree

  IF optCode | errcount GOTO fin

  writef("*nStarting the interpreter*n*n")

  xpendlevel := level()

  sp := 0
  asp := 0
  pc := 0
  env := 0
  stack := 0
  oldc := 0
  
  execpal()   // Execute Pocode instructions
  IF retcode DO writef("Return code %n*n", retcode)
  writef("*nInstructions executed: %n*n", count)
   
xpend:
fin:
  IF treevec       DO freevec(treevec)
  IF chbuf         DO freevec(chbuf)
  IF mem           DO freevec(mem)
  IF labv          DO freevec(labv)
  IF refv          DO freevec(refv)
  IF progstream    DO endstream(progstream)
  IF tostream UNLESS tostream=stdout DO endstream(tostream)

  selectoutput(stdout)
  RESULTIS errcount=0 -> 0, 20
}

AND str2varno(str) = VALOF
{ // str holds the name for PAL variable or a string constant
  // All strings in sxvstr are distinct.
  LET p = 1
  LET upb = str%0/bytesperword // upb of str in words
  //writef("*nstr2varno: str='%s' upb=%n p=%n strv!p=%n*n",
  //       str, upb, p, strv!p)
  //FOR i = 0 TO upb DO writef(" %x8", str!i)
  //newline()
  //abort(1002)
  WHILE strv!p DO // Item size
  { LET s = strv+p+1 // The next string in strv to inspect.
    LET found = TRUE
    //writef("str2varno: item size=%n str='%s' s='%s' p=%n*n",
    //       strv!p, str, s, p) 
    FOR i = 0 TO upb UNLESS str!i=s!i
    { found := FALSE
      BREAK
    }
    IF found DO 
    { //writef("str2varno: Name found at p=%n*n", p+1)
      RESULTIS p+1 // Position of matching string
                          // relative to strv
    }
    // Not found
    p := p+strv!p
    //writef("str2varno: Item at p=%n does not match*n", p)
  }
  // str not found so add it
  //writef("str2varno: match not found in sxvstr p=%n upb=%n '%s' strv=%n*n",
  //        p, upb, str, strv)
  //writef("str2varno: sxvstr=%n -> [ %n %n ]*n", sxvstr, sxvstr!0, sxvstr!1)
  //abort(1001)
  strv!p := upb+2 // Size of the new item
  FOR i = 0 TO upb DO sxvpush(sxvstr, str!i)
  sxvpush(sxvstr, 0)
  //writef("str2varno: sxvstr data after insertion is ")
  //FOR i = 0 TO strv!0 DO  //{ IF i MOD 8 = 0 DO newline()
  //  writef(" %x8", strv!i)
  //}
  //newline()
  //writef("str2varno: str='%s' returning %n*n", str, p+1)
  //abort(1884)
  RESULTIS p+1
}

LET lex() BE
{ // writef("lex: ch=%n '%c' lineno=%n*n", ch, ch>=32 ->ch, '?', lineno)
  //abort(1267)
  SWITCHON ch INTO
  { DEFAULT:
      UNLESS ch=endstreamch DO
      { LET badch = ch
        ch := '*s'
        synerr("Illegal character %x2", badch)
      }
      token := s_eof
      RETURN

    CASE '*p': CASE '*n':
      lineno := lineno + 1
    CASE '*c': CASE '*t': CASE '*s':
      rch()
      LOOP

    CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
    CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
      rdnumber()
      //writef("lex: token=%n decval=%n fltval=%8.3f*n",
      //        token, decval, fltval)
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
      
    CASE '*'': // A string constant
              { LET len = 0
	        LET upb = 0
                rch()
 
                UNTIL ch='*'' DO
                { IF len=255 DO synerr("String constant too long")
                  len := len + 1
                  charv%len := rdstrch()
                }
                charv%0 := len
		upb := len / bytesperword

		{ len := len+1  //Pad with zero bytes
		  UNLESS len MOD bytesperword BREAK
		  charv%len := 0
		} REPEAT

                wordnode := newvec(upb+2)
                h1!wordnode := s_stringconst
                FOR i = 0 TO upb DO wordnode!(i+1) := charv!i
                token := s_stringconst
		// wordnode -> [ Stringconst, <packed characters> ]
		//writef("Stringconst: '%s'*n", @h2!wordnode)
		//FOR i = 0 TO upb DO writef(" %x8", wordnode!(i+1))
		//newline()
                BREAK
              }
 
    CASE '(': token := s_lparen;    BREAK
    CASE ')': token := s_rparen;    BREAK 
    CASE '{': token := s_lcurly;    BREAK
    CASE '}': token := s_rcurly;    BREAK 
    CASE '%': token := s_percent;   BREAK 
    CASE '+': token := s_plus;      BREAK
    CASE ',': token := s_comma;     BREAK
    CASE '&': token := s_logand;    BREAK
    CASE '|': token := s_logor;     BREAK
    CASE '=': token := s_eq;        BREAK // This was valdef in pal70
    CASE ';': token := s_seq;       BREAK
    CASE '$': token := s_noshare;   BREAK
    CASE '.': token := s_dot;       BREAK // Not in pal70
 
    CASE '**':
      rch()
      IF ch='**' DO { token := s_power;  BREAK }
      token := s_mult
      RETURN

    CASE '/':
      rch()
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
  } REPEAT
 
  rch()
}
 
LET lookupword(word) = VALOF
{ // word is a BCPL string padded with zero bytes
  LET len, i = word%0, 0
  LET upb = len / bytesperword
  LET hashval = 0
  //writef("lookupword: word=%s %x8 %x8*n", word, word!0, word!1)
  //abort(6666)
  //writef("lookupword: word=%n=%s len=%n upb=%n*n", word, word, len, upb)
  //abort(1011)
  FOR i = 0 TO upb DO hashval := (13*hashval + word!i) / 3 MOD nametablesize
  IF hashval<0 DO abort(999)
  wordnode := nametable!hashval
 
  WHILE wordnode & i<=upb TEST (@h3!wordnode)!i=word!i
                          THEN i := i+1
                          ELSE wordnode, i := h2!wordnode, 0
  IF wordnode=0 DO
  { LET upb = len/bytesperword
    wordnode := newvec(upb+2)
    h1!wordnode, h2!wordnode := s_name, nametable!hashval
    FOR i = 0 TO upb DO (@h3!wordnode)!i := word!i
    nametable!hashval := wordnode
  }
  //writef("lookupword: word=%s => %n s_name=%n wordnode=%n*n",
  //        word, h1!wordnode, s_name, wordnode)
  //abort(3001)
  RESULTIS h1!wordnode
}
 
AND dsw(word, tok) BE
{ //IF tok=69 DO abort(1009)
  lookupword(word)
//IF tok=69 DO abort(1010)
  h1!wordnode := tok
  //writef("dsw: entered, word=%s tok=%n wordnode=%n*n", word, tok, wordnode)
  //abort(998)
}
 
AND declsyswords() BE
{ 
  dsw("and", s_and)
  dsw("aug", s_aug)
  dsw("def", s_def)
  dsw("do", s_do)
  dsw("dummy", s_dummy)
  dummynode := wordnode
  dsw("else", s_else) // Not in pal70
  dsw("eq", s_eq)
  dsw("false", s_false)
  falsenode := wordnode
  dsw("fn", s_lambda)
  dsw("ge", s_ge)
  dsw("goto", s_goto)
  dsw("gr", s_gr)
  dsw("if", s_if)
  dsw("ifnot", s_ifnot)
  dsw("ifso", s_ifso)
  dsw("in", s_in)
  dsw("jj", s_jj) // Not in pal70
  dsw("le", s_le)
  dsw("let", s_let)
  dsw("ll", s_lambda) // Not in pal70
  dsw("logand", s_logand) // Not in pal70
  dsw("logor", s_logor)
  dsw("ls", s_ls)
  dsw("ne", s_ne)
  dsw("nil", s_nil)
  nilnode := wordnode
  dsw("not", s_not)
  dsw("or", s_logor)
  dsw("rec", s_rec)
  dsw("res", s_res)
  dsw("test", s_test)
  dsw("then", s_then) // Not in pal70
  dsw("true", s_true)
  truenode := wordnode
  dsw("valof", s_valof)
  dsw("where", s_where)
  dsw("while", s_while)
  dsw("within", s_within)
  
  //dsw("#RES#", s_nameres) // Used by the impplementation of valof
  //resstr := wordnode
  resstr := "#RES#"
  resvarno := str2varno(resstr)
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

AND rdnumber() BE
{ // Read an integer or floating point constant
  // setting token to s_number with the integer value in decval
  // or s_fnum with the floating point value in fltval which will
  // be in IEE standard 32 or 64 bit format depending on the BCPL
  // word length of the compiler.
  // The strategy is to simultaneously construct both the integer
  // and floating point values. It stops constructing the integer
  // value after reading a decimal point or e, ie when the constant
  // is known to be floating point. Care is needed with eg 123..
  // which is s_number followed by s_range.
  LET pos      = 0    // Number of integer and fractional digits
                      // in the number.
  LET sigpos   = 0    // Position of the last significant digit
  LET pointpos = 0    // Position of the digit just left of the
                      // decimal point
  LET FLT flt0  = 0.0
  LET FLT flt1  = 1.0
  LET FLT flt10 = 10.0
//abort(1001)  
  token := s_int      // Until '.' or 'e' encountered
  decval, exponent, fltval := 0, 0, flt0

  // Ignore spaces
  WHILE ch='*s' | ch='*t' DO rch()

  // A number must start with a digit.
  UNLESS '0'<=ch<='9' DO synerr("Bad number")

  WHILE '0'<=ch<='9' | ch='_' | ch='.' DO
  { // Deal with digits before e, if any.
    //writef("ch=%c pos=%n token=%n decval=%i4 exponent=%n*n",
    //        ch, pos, token, decval, exponent)
    SWITCHON ch INTO
    { DEFAULT: BREAK // ch is either e, E or terminates the number.

      CASE '0': CASE '1': CASE '2': CASE '3': CASE '4': 
      CASE '5': CASE '6': CASE '7': CASE '8': CASE '9':
      { LET x = sys(Sys_flt, fl_mul, fltval, flt10)  // = 10 * fltval
        pos := pos+1                 // Increment count of digits
        IF token=s_int DO pointpos := pos

        decval := 10*decval + ch-'0' // Accumulate the integer value
        // decval might overflow if too many digits are given.
	
        IF sys(Sys_flt, fl_eq, x, sys(Sys_flt, fl_add, x, flt1)) ENDCASE

        // fltval * 10 + 1 is not equal to fltval * 10, so
        // the digit is significant
        // Perform fltval := x + FLOAT(ch-'0') and increment sigpos.
        fltval := sys(Sys_flt,
                      fl_add, x, sys(Sys_flt, fl_float, ch-'0'))
        sigpos := sigpos+1
        ENDCASE
      }

      CASE '.':
      { LET k = rdch()
        unrdch() // Unread the character after the dot.
        IF k='.' DO
	{ // Found .. which is s_range, so the dot is not part of a
	  // floating point number.
	  RETURN   // Return with token=s_int
	}
        IF token=s_real DO synerr("Two decimal points in a number")
        token := s_real
        ENDCASE
      }
      
      CASE '_':  // Ignore underlines in numbers.
        ENDCASE
    }
    rch()
  }

//sawritef("rdnumber: token=%n decval=%n fltval=%13.1e *
//         *pos=%n sigpos=%n pointpos=%n*n",
//          token, decval, fltval, pos, sigpos, pointpos)

  IF ch='e' | ch='E' DO // Not in pal70
  { LET expneg = FALSE
    token := s_real
    rch()
    IF ch='-' DO { expneg := TRUE; rch() }
    WHILE '0'<=ch<='9' | ch='_' DO
    { UNLESS ch='_' DO exponent := 10*exponent + ch-'0'
      rch()
    }
    IF expneg DO exponent := -exponent
  }

  IF token=s_int DO
  { // There was no decimal point or e so leave token=s_int
    // and the integer value in decval.
    RETURN
  }

  // token is s_real

//sawritef("*nrdnumber: making fnumber fltval=%13.1e *
//         *exponent=%n sigpos=%n, pointpos=%n*n",
//          fltval, exponent, sigpos, pointpos)
  // Correct the exponent
  exponent := exponent + pointpos - sigpos

  UNLESS -127 <= exponent <= 127 DO
    synerr("Floating point exponent out of range")

  // Set fltval to fltval x 10^exponent
  TEST exponent>=0
  THEN FOR i = 1 TO exponent DO
         fltval := sys(Sys_flt, fl_mul, fltval, flt10)
  ELSE FOR i = -1 TO exponent BY -1 DO
         fltval := sys(Sys_flt, fl_div, fltval, flt10)
//sawritef("*n=> fltval=%13e*n", fltval)

  // fltval is a floating point number of the same size as
  // the BCPL word length.
}


AND rdtag() = VALOF
{ LET len = 0
  // In pal70 underlines were not allowed in tags.
  WHILE 'a'<=ch<='z' | 'A'<=ch<='Z' | '0'<=ch<='9' |  ch='_' DO
  { len := len+1
    IF len>255 DO synerr("Name too long")
    charv%len := ch
    rch()
  }
  charv%0 := len

  // Pad the string with zero bytes
  { len := len+1
  //writef("rdtag: padding with zero bytes, len=%n*n", len)
    UNLESS len MOD bytesperword BREAK
    charv%len := 0 // Pad with zero bytes
  } REPEAT

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
    SWITCHON capitalch(ch) INTO
    { DEFAULT:   synerr("Bad string or character constant")
      CASE '*'':            res := ch;     ENDCASE // Not in pal70
      CASE 't':  CASE 'T':  res := '*t';   ENDCASE
      CASE 's':  CASE 'S':  res := '*s';   ENDCASE
      CASE 'n':  CASE 'N':  res := '*n';   ENDCASE
      CASE 'b':  CASE 'B':  res := '*b';   ENDCASE // Not in pal70
      CASE 'p':  CASE 'P':  res := '*b';   ENDCASE // Not in pal70
    }
  }
  rch()
  RESULTIS res
}

LET newvec(n) = VALOF // Not used for runtime data
{ // n is the upb of the new vector
  treep := treep - n - 1;
  IF treep<=treevec DO fatalerr("More workspace needed")
  RESULTIS treep
}
 
AND mk1(a) = VALOF
{ LET p = newvec(0)
  p!0 := a
  //writef("mk1: %n->[%s]*n", p, opstr(a))
  RESULTIS p
}
 
AND mk2(a, b) = VALOF
{ LET p = newvec(1)
  p!0, p!1 := a, b
  //writef("mk2: %n->[%s %n]*n", p, opstr(a),b)
  RESULTIS p
}
 
AND mk3(a, b, c) = VALOF
{ LET p = newvec(2)
  p!0, p!1, p!2 := a, b, c
  //writef("mk3: %n->[%s %n %n]*n", p, opstr(a),b,c)
  RESULTIS p
}
 
AND mk4(a, b, c, d) = VALOF
{ LET p = newvec(3)
  p!0, p!1, p!2, p!3 := a, b, c, d
  //writef("mk4: %n->[%s %n %n %n]*n", p, opstr(a),b,c,d)
  RESULTIS p
}
 
AND mk5(a, b, c, d, e) = VALOF
{ LET p = newvec(4)
  p!0, p!1, p!2, p!3, p!4 := a, b, c, d, e
  //writef("mk5: %n->[%s %n %n %n %n]*n", p, opstr(a),b,c,d,e)
  RESULTIS p
}
 
AND mk6(a, b, c, d, e, f) = VALOF
{ LET p = newvec(5)
  p!0, p!1, p!2, p!3, p!4, p!5 := a, b, c, d, e, f
  //writef("mk6: %n->[%s %n %n %n %n %n]*n", p, opstr(a),b,c,d,e,f)
  RESULTIS p
}
 
AND formtree() = VALOF
{ LET res = 0
  rec_p, rec_l := level(), recover
//writef("formtree: entered*n")
  charv := newvec(256/bytesperword)     
  nametable := newvec(nametablesize)
  UNLESS charv & nametable DO fatalerr("More workspace needed")
  FOR i = 0 TO nametablesize DO nametable!i := 0
//writef("formtree: calling declsyswords*n")
  declsyswords()
  mptnode := mk1(s_mpt)
//writef("formtree: calling lex*n")

  lex()

  IF optTokens DO            // For debugging lex.
  { //abort(1000)
    writef("token = %i3 %s", token, opstr(token))
    IF token=s_int    DO writef("       %n",   decval)
    IF token=s_real   DO writef("      %5.3f", fltval)
    IF token=s_name   DO writef("      %s",    @h3!wordnode)//charv)
    IF token=s_stringconst DO
    { LET str = @h2!wordnode
      writef("    *'")
      FOR i = 1 TO str%0 SWITCHON str%i INTO
      { DEFAULT:   wrch(charv%i); ENDCASE
        CASE '*'': writes("**'"); ENDCASE
        CASE '*n': writes("**n"); ENDCASE
        CASE '*p': writes("**p"); ENDCASE
        CASE '*t': writes("**t"); ENDCASE
        CASE '*b': writes("**b"); ENDCASE
      }
      writef("*'")
    }
    newline()
    IF token=s_eof DO
    { //abort(999)
      RESULTIS 0
    }
    lex()
  } REPEAT

recover:
  // lex has already been called.
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
  abort(6611)
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
  LET ln = lineno

  IF token=s_def DO
  { LET res = 0
    LET p = @res
    { LET def = rndef(0)
      !p := mk3(s_def, def, 0)
      p := @h3!def
    } REPEATWHILE token=s_def
    // In pal70 a sequence of defs was not folloed by 'in C0'
    checkfor(s_in, "'in' expected after a 'def' sequence")
    // A Paren node causes a scan for labels.
    ln := lineno
    !p := mk2(s_paren, rncom(0), ln)
    RESULTIS res
  }

  // A Paren node causes a scan for labels.
  RESULTIS rcom(0)
}

AND rnbdef() = VALOF
{ lex()
  RESULTIS rbdef()
}

AND rbdef() = VALOF
{ // BD -> N,...,N = E
  //       N BV...BV = E
  //       ( D )
  //       rec BD
  LET recursive, recln = FALSE, lineno
  LET res = 0
  LET op, ln = token, lineno
//  IF op=s_rec DO
//  { recursive := TRUE
//    lex()
//    op, ln := token, lineno
//  }
  
  SWITCHON op INTO
  { DEFAULT:
      synerr("Bad definition, name or '(' expected")

    CASE s_name:
      { LET name = rname()
        ln := lineno

        IF token=s_comma DO
        { // Must be a simultaneous definition
          // N ,..., N = C0
          LET names = rdnamelist(name)
          checkfor(s_eq, "Bad definition")
	  res := mk4(s_valdef, names, rcom(4), ln)
	  IF recursive DO res := mk3(s_rec, res, recln)
          RESULTIS res
        }

        IF token=s_eq DO
	{ res := mk4(s_valdef, name, rncom(8), ln)
	  IF recursive DO res := mk3(s_rec, res, recln)
	  RESULTIS res
	}

        // We have a name not followed by a comma
	
        { // Must be a function definition
          // N BV ... BV = C0
          LET v = VEC 50
          AND i, b = 0, ?
          WHILE i<=50 DO
          { UNLESS token=s_lparen | token=s_name BREAK
            v!i := rbv() // Read a name or list of names
	                 // enclosed on parens
            i := i+1
          }
          UNLESS i~=0 & token=s_eq DO synerr("Bad definition")
          b := rncom(8) // Read the function body
          WHILE i>0 DO
          { i := i-1
            b := mk4(s_lambda, v!i, b, ln) // Form lambda expressions
          }
	  res := mk4(s_valdef, name, b, ln)
	  IF recursive DO res := mk3(s_rec, res, recln)
          RESULTIS res
        }
      }

    CASE s_lcurly:
      { res := rndef(0)
        checkfor(s_rcurly, "Bad definition, '}' expected")
	IF recursive DO res := mk3(s_rec, res, recln)
        RESULTIS res
      }

    CASE s_rec:
      //synerr("Redundant 'rec'")
      RESULTIS mk3(s_rec, rndef(0), ln)
  }
}

AND rndef(n) = VALOF { lex(); RESULTIS rdef(n) }

AND rdef(n) = VALOF
{ // D -> D and D
  //      D within D
  //      BD
  LET a = rbdef()
  LET b = 0

  { LET op, ln = token, lineno

//sawritef("rdef: op=%s ln=%n*n", opstr(op), ln)
    SWITCHON op INTO
    { DEFAULT:
        RESULTIS a

      CASE s_and:
        IF n>=6 RESULTIS a
        IF a=0 DO synerr("Definition missing before 'and'")
        { LET i = 0
          LET v = VEC 100 // To the second and subsequent definitions
	                  // a is the first definitions
          WHILE token=s_and DO
          { i := i+1
	    v!i := rndef(6) // Up to and, within or where
          }
	  //i is the number of additional definistions
          b := a
          a := newvec(i+2) // Room for [ And, n, d1,...,dn ]
          a!0, a!1, a!2 := s_and, i+1, b // b is the first definition
	                                 // i+1 is the total number
					 // of definitions in the
					 // and construct.
          FOR j = 1 TO i DO a!(j+2) := v!j
	  //FOR j = 0 TO i+2 DO writef(" %n ", a!j)
	  //newline()
	  //abort(1255)
          LOOP
        }

      CASE s_within:
        IF n>=3 RESULTIS a
        IF a=0 DO synerr("Definition missing before 'within'")
        a := mk4(s_within, a, rndef(3), ln)
        LOOP
	
      CASE s_where:
        IF n>=4 RESULTIS a
        IF a=0 DO synerr("Definition missing before 'where'")
        a := mk4(s_where, a, rndef(4), ln)
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
  checkfor(s_rparen, "XBad Bound variable list")
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

LET rbexp(n) = VALOF
{ LET a, b, op, ln = 0, 0, token, lineno
  SWITCHON op INTO
  { DEFAULT:
      RESULTIS 0
      //synerr("Error in expression token=%s", opstr(token))

    CASE s_name:
      a := wordnode
      lex()
      RESULTIS a
    
    CASE s_lparen:
      a := rnexp(0)
      UNLESS a DO a := nilnode
      checkfor(s_rparen, "')' missing")
      RESULTIS a

    CASE s_true:
    CASE s_false:
    CASE s_nil:
    CASE s_stringconst:
    CASE s_dummy:
    CASE s_jj:
      a := wordnode
      lex()
      RESULTIS a
   
    CASE s_int:
      a := mk2(op, decval)
      lex()
      RESULTIS a
 
    CASE s_real:
      a := mk2(op, fltval)
      lex()
      RESULTIS a
 
    CASE s_noshare:
      a := rnexp(38)
      RESULTIS mk3(op, a, ln)
 
    CASE s_lambda:
    { LET v = VEC 50 // For the list of bound variables
      AND i = 0
      lex()
      WHILE i<=50 DO
      { UNLESS token=s_lparen | token=s_name BREAK
        v!i := rbv()
        i := i+1
      }
      IF i=0 DO synerr("No bound variable list after 'fn'")
      checkfor(s_dot, "'.' missing in 'fn' construct")
      a := rcom(4)
      WHILE i>0 DO
      { i := i-1
        a := mk4(s_lambda, v!i, a, ln)
      }
      RESULTIS a
    }

    CASE s_plus:
      a := rnexp(32)
      RESULTIS a
 
    CASE s_minus:
      a :=   mk3(s_neg, rnexp(32), ln)
      RESULTIS a
 
    CASE s_not:
      RESULTIS mk3(s_not, rnexp(26), ln)

    CASE s_valof:
      RESULTIS mk3(op, rncom(6), ln) // Up to ; or where
  }
}
 
AND rnexp(n) = VALOF { lex(); RESULTIS rexp(n) }
 
AND rexp(n) = VALOF
{ LET a, b, p = 0, 0, 0
  a := rbexp(-1)  // No arg expected

  UNLESS a RESULTIS 0

  { LET op, ln = token, lineno
    SWITCHON op INTO
    { DEFAULT:
        RESULTIS a
 
      CASE s_name:   // Juxtaposition with these cause an application
      CASE s_lparen:
      CASE s_true:
      CASE s_false:
      CASE s_nil:
      CASE s_jj:
      CASE s_stringconst:
      CASE s_dummy:
      CASE s_int:
      CASE s_real:
        a := mk4(s_apply, a, rbexp(n), ln)
        LOOP

      CASE s_comma:
      { LET i = 1
        LET v = VEC 500
        IF n>14 RESULTIS a
        WHILE token=s_comma DO
        { v!i := rnexp(16)
          i := i+1
        }
        b := a
        a := newvec(i+1)
        a!0, a!1, a!2 := s_comma, i, b
        FOR j = 1 TO i-1 DO a!(j+2) := v!j
//sawritef("rexp: s_comma i=%n*n", i)
        LOOP
      }

      CASE s_where:
        RESULTIS mk4(s_where, a, rndef(0), ln)
	
      CASE s_aug:
        IF n>16 RESULTIS a
        a := mk4(s_aug, a, rnexp(18), ln)
        LOOP

      CASE s_cond:
        IF n>18 RESULTIS a
        b := rnexp(18)
        checkfor(s_comma, "Bad conditional expression")
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
{ // Many commands yield the result Dummy. But if it is syntactically an
  // expression it returns the value of the expression. Commands can be
  // labelled. Labels can occur at the end of a command sequence.
  LET firsttoken = token
  // Read a basic command, ie one starting with
  // a label, Lcurly, Let, Test, If, While, Goto, Res, Dummy,
  // or an expression
  LET a = 0
  a := rbcom() // Returns zero if no command found
  UNLESS a RESULTIS 0

  { LET op, ln = token, lineno
    SWITCHON op INTO
 
    { DEFAULT:
        RESULTIS a

      CASE s_seq:
        IF n<4 DO
        { LET b = rncom(4) // Read where defs but not seq
          UNLESS b DO synerr("Command expected after a semicolon")
          a := mk4(s_seq, a, b, ln)
          LOOP
        }
        RESULTIS a

      CASE s_where:
        IF n>6 RESULTIS a
        a := mk4(s_where, a, rnbdef(0), ln)
        LOOP
     }
  } REPEAT
}

AND rnbcom() = VALOF
{ lex()
  RESULTIS rbcom()
}

AND rbcom() = VALOF
{ LET op, ln, a, b = token, lineno, 0, 0
  LET firsttoken = token
  SWITCHON op INTO
  { DEFAULT: // Must be an expression, a labelled command or an assignment
      a := rexp(4)
      ln := lineno
    
      UNLESS a RESULTIS 0  // No command present
      
      IF token=s_colon DO
      { UNLESS firsttoken=s_name & h1!a=s_name DO
          synerr("Bad command, ':' not preceeded by a name")
	firsttoken := 0
        RESULTIS mk5(s_colon, a, rncom(8), 0, ln)
      }
      
      IF token=s_ass
        RESULTIS mk4(s_ass, a, rnexp(14), ln)

      RESULTIS a

    CASE s_lcurly:
    { LET a = rncom(0)
      checkfor(s_rcurly, "Bad command, '}' expected")
      RESULTIS a
    }

    CASE s_let:
    { a := rndef(0)
      checkfor(s_in, "'in' expected in 'let' construct")
      b := rcom(4)
      UNLESS b DO synerr("Command missing after 'in'")
      RESULTIS mk4(s_let, a, b, ln)
    }

    CASE s_test:
      a := rnexp(20)
      SWITCHON token INTO
      { DEFAULT:
          synerr("Bad 'test' command")

        CASE s_then:
          b := rncom(4)
          checkfor(s_else, "'else' expected")
          RESULTIS mk5(s_cond, a, b, rncom(8), ln)

        CASE s_ifso:
          b := rncom(4)
          checkfor(s_ifnot, "'ifnot' expected")
          RESULTIS mk5(s_cond, a, b, rncom(8), ln)

        CASE s_ifnot:
          b := rncom(4)
          checkfor(s_ifso, "'ifso' expected")
          RESULTIS mk5(s_cond, a, rncom(8), b, ln)
      }

    CASE s_while:
      a := rnexp(20)
      checkfor(s_do, "'do' expected")
      RESULTIS mk5(s_while, a, rcom(8), ln)

    CASE s_if:
      a := rnexp(20)
      checkfor(s_do, "'do' expected")
      RESULTIS mk5(s_cond, a, rcom(4), dummynode, ln)

    CASE s_goto:
      RESULTIS mk3(s_goto, rnexp(38), ln)

    CASE s_res:
      RESULTIS mk3(s_res, rnexp(14), ln)

    CASE s_dummy:
      RESULTIS dummynode
  }
}

LET opstr(op) = VALOF SWITCHON op INTO
{ DEFAULT:          writef("*n### opstr: unknown op: %n ###*n", op)
abort(999)
                    RESULTIS "###Unknown op ###"

  CASE s_ass:         RESULTIS "Ass"
  CASE s_and:         RESULTIS "And"
  CASE s_apply:
  CASE i_apply:       RESULTIS "Apply"
  CASE s_aug:
  CASE i_aug:         RESULTIS "Aug"
  CASE t_basicfn:     RESULTIS "Basicfn"
  CASE i_blocklink:   RESULTIS "Blocklink"
  CASE t_closure:     RESULTIS "Closure"
  CASE s_colon:       RESULTIS "Colon"
  CASE s_comma:       RESULTIS "Comma"
  CASE s_cond:        RESULTIS "Cond"
  CASE i_decllabel:   RESULTIS "Decllabel"
  CASE i_declvar:     RESULTIS "Declvar"
  CASE i_declvars:    RESULTIS "Declvars"
  CASE s_def:         RESULTIS "Def"
  CASE s_div:
  CASE i_div:         RESULTIS "Div"
  CASE s_do:          RESULTIS "Do"
  CASE s_dot:         RESULTIS "Dot"
  CASE s_dummy:
  CASE t_dummy:       RESULTIS "Dummy"
  CASE s_else:        RESULTIS "Else"
  CASE s_eof:         RESULTIS "Eof"
  CASE s_eq:
  CASE i_eq:          RESULTIS "Eq"
  CASE t_env:         RESULTIS "Env"
  CASE s_false:
  CASE i_false:
  CASE t_false:       RESULTIS "False"
  CASE i_finish:      RESULTIS "Finish"
  CASE i_formClosure: RESULTIS "FormClosure"
  CASE i_formLvalue:  RESULTIS "FormLvalue"
  CASE i_formRvalue:  RESULTIS "FormRvalue"
  CASE s_ge:
  CASE i_ge:          RESULTIS "Ge"
  CASE s_goto:        RESULTIS "Goto"
  CASE s_gr:
  CASE i_gr:          RESULTIS "Gt"
  CASE t_guess:       RESULTIS "Guess"
  CASE i_halt:        RESULTIS "Halt"
  CASE s_if:          RESULTIS "If"
  CASE s_ifso:        RESULTIS "Ifso"
  CASE s_ifnot:       RESULTIS "Ifnot"
  CASE s_in:          RESULTIS "In"
  CASE i_initvar:     RESULTIS "Initvar"
  CASE i_initvars:    RESULTIS "Initvars"
  CASE s_int:
  CASE t_int:         RESULTIS "Int"
  CASE s_jj:
  CASE i_jj:
  CASE t_jj:          RESULTIS "Jj"
  CASE i_jump:        RESULTIS "Jump"
  CASE i_jumpF:       RESULTIS "JumpF"
  CASE s_lab:         RESULTIS "Lab"
  CASE t_label:       RESULTIS "Label"
  CASE s_lambda:      RESULTIS "Lambda"
  CASE s_lcurly:      RESULTIS "Lcurly"
  CASE s_le:
  CASE i_le:          RESULTIS "Le"
  CASE s_let:         RESULTIS "Let"
  CASE i_loadE:       RESULTIS "LoadE"
  CASE i_loadF:       RESULTIS "LoadF"
  CASE i_loadGuess:
  CASE t_loadGuess:   RESULTIS "LoadGuess"
  CASE i_loadJ:       RESULTIS "LoadJ"
  CASE i_loadL:       RESULTIS "LoadL"
  CASE i_loadN:       RESULTIS "LoadN"
  CASE i_loadR:       RESULTIS "LoadR"
  CASE i_loadS:       RESULTIS "LoadS"
  CASE s_logand:
  CASE i_logand:      RESULTIS "Logand"
  CASE s_logor:
  CASE i_logor:       RESULTIS "Logor"
  CASE i_lose1:       RESULTIS "Lose1"
  CASE s_lparen:      RESULTIS "Lparen"
  CASE s_ls:
  CASE i_ls:          RESULTIS "Ls"
  CASE t_lvalue:      RESULTIS "Lvalue"
  CASE i_members:     RESULTIS "Members"
  CASE s_minus:
  CASE i_minus:       RESULTIS "Minus"
  CASE s_mpt:         RESULTIS "Mpt"
  CASE s_mult:
  CASE i_mult:        RESULTIS "Mult"
  CASE s_name:        RESULTIS "Name"
  CASE s_ne:
  CASE i_ne:          RESULTIS "Ne"
  CASE s_neg:
  CASE i_neg:         RESULTIS "Neg"
  CASE s_nil:
  CASE i_nil:
  CASE t_nil:         RESULTIS "Nil"
  CASE t_nils:        RESULTIS "Nils"
  CASE i_norestart:   RESULTIS "Norestart"
  CASE s_noshare:     RESULTIS "Noshare"
  CASE s_not:
  CASE i_not:         RESULTIS "Not"
  CASE i_okrestart:   RESULTIS "Okrestart"
  CASE s_percent:     RESULTIS "Percent"       
  CASE s_paren:       RESULTIS "Paren"       
  CASE s_pling:       RESULTIS "Pling"
  CASE s_plus:
  CASE i_plus:        RESULTIS "Plus"
  CASE s_power:
  CASE i_power:       RESULTIS "Power"
  CASE s_rcurly:      RESULTIS "Rcurly"
  CASE s_real:
  CASE t_real:        RESULTIS "Real"       
  CASE s_rec:         RESULTIS "Rec"       
  CASE s_res:
  CASE i_res:         RESULTIS "Res"
  CASE i_reslink:     RESULTIS "Reslink"
  CASE i_restart:     RESULTIS "Restart"
  CASE i_restoreE1:   RESULTIS "RestoreE1"
  CASE i_return:      RESULTIS "Return"
  CASE s_rparen:      RESULTIS "Rparen" 
  CASE i_rvrestart:   RESULTIS "Rvrestart"
  CASE i_save:        RESULTIS "Save"
  CASE s_seq:         RESULTIS "Seq"
  CASE i_setlabEs:    RESULTIS "SetlabEs"
  CASE i_setup:       RESULTIS "Setup"
  CASE s_stringconst: RESULTIS "Stringconst"
  CASE t_string:      RESULTIS "String"
  CASE t_stack:       RESULTIS "Stack"
  CASE s_test:        RESULTIS "Test"
  CASE i_testEmpty:   RESULTIS "TestEmpty"
  CASE s_then:        RESULTIS "Then"
  CASE s_true:
  CASE i_true:
  CASE t_true:        RESULTIS "True"
  CASE i_tuple:
  CASE t_tuple:       RESULTIS "Tuple"
  CASE i_update:      RESULTIS "Update"
  CASE s_valdef:      RESULTIS "Valdef"
  CASE s_valof:       RESULTIS "Valof"
  CASE s_where:       RESULTIS "Where"
  CASE s_while:       RESULTIS "While"
  CASE s_within:      RESULTIS "Within"
  CASE s_nameres:     RESULTIS "#RES#"
}

LET plist(x, n, d) BE
{ LET op, size, ln = ?, 0, 0
  LET v = TABLE 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

  writef("%i5: ", x)
  
  IF x=0 DO { writes("Null"); RETURN  }
 
  op := h1!x

  SWITCHON op INTO
  { DEFAULT:
      writef("plist: Default op=%s*n", opstr(op)); RETURN

    CASE s_int:  // x -> [Int, value]
      writef("Int %i4", h2!x);          RETURN
	 
    CASE s_real: // x -> [Real, value]
      writef("Real %6.3f", h2!x);       RETURN

    CASE s_name: // x -> [Name, link, <chars>] link is initially the hash link
      writef("Name %s", @h3!x);         RETURN
	 
    CASE s_stringconst: // [ Stingconst, <packed characters> ] 
    { LET s = x+1
      writef("Stringconst '")
      FOR i = 1 TO s%0 SWITCHON s%i INTO
      { DEFAULT:   wrch(s%i);      ENDCASE
        CASE '*n': writes("**n");  ENDCASE
        CASE '*p': writes("**p");  ENDCASE
        CASE '*t': writes("**t");  ENDCASE
        CASE '*s': writes("**s");  ENDCASE
        CASE '**': writes("****"); ENDCASE
      }
      writef("'")
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
    CASE s_lab:
    CASE s_ass: CASE s_apply: CASE s_lambda:
    CASE s_def: CASE s_valdef: CASE s_tuple: CASE s_seq:
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
    CASE s_paren:
    CASE s_neg:
    CASE s_not:
      size, ln := 2, h3!x
      ENDCASE

    CASE s_true: CASE s_false:
    CASE s_nil: CASE s_mpt:
    CASE s_dummy:
    CASE s_jj:
      size := 1;  //man #####
      ENDCASE
  }
 
  IF n=d DO { writes("Etc"); RETURN }
  writef("%s", opstr(op))
  IF op=s_and | op=s_comma DO writef(" %n", size-1)
  IF ln DO writef("  -- line %n", ln)
  FOR i = 2 TO size DO
  { newline()
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
  ssp, mssp := 0, 0
  
  IF optCode DO writef("*nCompiled code:*n*n")

  codep := 1
  finishLoc    := codep; outf(i_finish)
  restartLoc   := codep; outf(i_restart)
  rvrestartLoc := codep; outf(i_rvrestart)
  okrestartLoc := codep; outf(i_okrestart)
  norestartLoc := codep; outf(i_norestart)
  startLoc     := codep
  
  n := nextlab()  outfl(i_setup, n)

  translabels(x)
//writef("trprog: calling trans(%n, m_val)*n", x)
//abort(1001)
  trans(x, m_val)
  UNLESS ssp=1 DO
    writef("*nSSP error just before finish, ssp=%n should be 1*n", ssp)
  outf(i_finish)
  //writef("*ntrprog: mssp=%n*n", mssp)
  //abort(1234)
  outlabset(n, mssp)

  resolvelabels()
}

LET outssp() BE IF optTrace & optCode DO
{ writef("ssp=%i3 mssp=%i3  ", ssp, mssp)
  IF ssp<0 DO
  { writef("###### ssp is negative ########*n")
    abort(999)
  }
  abort(900)
}
  
LET trans(x, mode) BE
// x       is the program
// mode is m_val or m_ref
{ LET op = h1!x
  IF x=0 DO
  { writes("*nExpression missing*n")
    outf(i_nil)
    upssp(1)
    IF mode=m_ref DO outf(i_formLvalue)
    RETURN
  }

//writef("trans: x=%n op=%s*n", x, opstr(op))
  SWITCHON op INTO
  { DEFAULT:
      writef("trans: DEFAULT case reached op=%s*n", opstr(op))
      abort(999)
      RETURN

    CASE s_let:                 // x -> [Let,   def, body, ln]
    CASE s_where:               // x -> [Where, body, def, ln]
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      LET defs, body = 0, 0
      TEST op=s_let THEN defs, body := h2!x, h3!x
                    ELSE defs, body := h3!x, h2!x
      comline := h4!x
      //writef("CASE op=%s: calling transrhs(%n) h1!defs=%s*n",
      //        opstr(op), defs, opstr(h1!defs))
      //abort(1000)
      transrhs(defs)
      outfl(i_blocklink, lab1)
      //writef("About to call transscope*n")
      //abort(5333)
      transscope(defs, body, lab2, mode)
      outlab(lab1)             
      RETURN
    }
  
    CASE s_def:
    //writef("trans: CASE def:*n")
    //abort(1000)
      transrhs(h2!x)
      declvars(h2!x)
      translabels(h3!x)
      trans(h3!x, m_val)
      RETURN

    CASE s_mult: CASE s_div: CASE s_power: CASE s_plus: CASE s_minus:
    CASE s_eq: CASE s_ne: CASE s_ls: CASE s_le: CASE s_gr: CASE s_ge:
    CASE s_logand: CASE s_logor:
      trans(h3!x, m_val)
      trans(h2!x, m_val) 
      outf(cvs2i(op))
      ssp := ssp-1
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN

    CASE s_aug:
      trans(h3!x, m_ref) // Right hand argument of aug
      trans(h2!x, m_val) // Left hand argument of aug
      outf(i_aug)
      ssp := ssp-1
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN

    CASE s_apply:
      trans(h3!x, m_ref)  // The argument as an LV 
                          // often the LV of a tuple.
			  // A tuple is used for calls
			  // of functions with multiple
			  // formal parameters.
      trans(h2!x, m_val)  // A closure, tuple or basic function.
      outf(i_apply)
      ssp := ssp-1        // Replace the top two stack items
                          // by one result.
      IF mode=m_val DO outf(i_formRvalue)
      RETURN

    CASE s_neg:
    CASE s_not:
      trans(h2!x, m_val)
      outf(cvs2i(op))
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN

    CASE s_noshare:
      trans(h2!x, m_val)
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN

    CASE s_comma:
    { LET len = length(x)
      LET r(x) BE trans(x, m_ref)
      mapb(r, x)
      outfn(i_tuple, len)
      ssp := ssp - len + 1
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN
    }

    CASE s_lambda:
    { LET lab1 = nextlab()  // Entry of the function
      LET lab2 = nextlab()  // Label for the jump over the fn code
      LET lab3 = nextlab()  // label for the fn stack size
      outfl(i_formClosure, lab1)
      upssp(1)
      outfl(i_jump, lab2)
      outlab(lab1)
      transscope(h2!x, h3!x, lab3, m_ref)
      outlab(lab2)
      IF mode=m_ref DO outf(i_formLvalue)
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
      trans(h2!x, m_val)
      outf(i_lose1)
      ssp := ssp-1
      trans(h3!x, mode)
      RETURN

    CASE s_valof:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      outfl(i_reslink, lab1)
      upssp(1)
      { LET a, b = ssp, mssp
        ssp, mssp := 0, 1
        outfl(i_save, lab2) // 6+lab2 is the new stack size
        outf(i_jj)
	upssp(1)
        outf(i_formLvalue)
	//abort(3439)
        outfvar(i_declvar, nameres) // Variable nameres holds the
	                            // destination used by res.
        ssp := ssp-1
        translabels(h2!x)
        trans(h2!x, m_ref)
        outf(i_return)
        UNLESS ssp=1 DO trnerr("SSP Error ssp=%n should be 1", ssp)
        outlabset(lab2, mssp)
        ssp, mssp := a, b
      }
      outlab(lab1)
      IF mode=m_val DO outf(i_formRvalue)
      RETURN
    }

    CASE s_res:
      trans(h2!x, m_ref)
      outf(i_res)
      //ssp := ssp-1
      RETURN

    CASE s_goto:
      trans(h2!x, m_val)
      outf(i_goto)
      ssp := ssp-1
      RETURN

    CASE s_cond:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      trans(h2!x, m_val)
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
      trans(h2!x, m_val)
      outfl(i_jumpF, lab1)
      ssp := ssp-1
      trans(h3!x, m_val)
      outf(i_lose1)
      outfl(i_jump, lab2)
      outlab(lab1)
      outf(i_dummy)
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN
    }

    CASE s_ass:
    { LET len = length(h2!x)
      comline := h4!x
      trans(h2!x, m_ref)
      trans(h3!x, m_val)
      outfn(i_update, len)
      ssp := ssp-1
      IF mode=m_ref DO outf(i_formLvalue)
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
    CASE s_jj:
      outf(cvs2i(op))
      upssp(1)
      abort(12998)
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN

    CASE s_name:
      //writef("s_name: %s %x8 %x8*n", @h3!x, h3!x, h4!x)
      outfvar((mode=m_val -> i_loadR, i_loadL), x)
      upssp(1)
      RETURN

    CASE s_stringconst:
      //writef("s_stringconst: %s*n", @h2!x)
      outfstring(i_loadS, x)
      upssp(1)
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN

    CASE s_int:
      outfn(i_loadN, h2!x)
      upssp(1)
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN

    CASE s_real:
      outfflt(i_loadF, h2!x)
      upssp(1)
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN
  }
}

AND cvs2i(op) = VALOF SWITCHON op INTO
{ DEFAULT:        trnerr("System error in cvs2i,op=%s*n", op)
                  RESULTIS 0
	    
  CASE s_true:    RESULTIS i_true
  CASE s_false:   RESULTIS i_false
  CASE s_nil:     RESULTIS i_nil
  CASE s_jj:      RESULTIS i_jj
  CASE s_mult:    RESULTIS i_mult
  CASE s_div:     RESULTIS i_div
  CASE s_power:   RESULTIS i_power
  CASE s_plus:    RESULTIS i_plus
  CASE s_minus:   RESULTIS i_minus
  CASE s_neg:     RESULTIS i_neg
  CASE s_eq:      RESULTIS i_eq
  CASE s_ne:      RESULTIS i_ne
  CASE s_ls:      RESULTIS i_ls
  CASE s_le:      RESULTIS i_le
  CASE s_gr:      RESULTIS i_gr
  CASE s_ge:      RESULTIS i_ge
  CASE s_not:     RESULTIS i_not
  CASE s_logand:  RESULTIS i_logand
  CASE s_logor:   RESULTIS i_logor
}

AND findlabels(x) = VALOF
{ IF x=0 RESULTIS 0
  SWITCHON h1!x INTO
  { DEFAULT:
      RESULTIS 0

    CASE s_colon:
    { LET lab = nextlab()
      h4!x := lab
      outfvarl(i_decllabel, h2!x, lab)
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
//  IF n DO writef("translabels: found n=%n*n", n)
  IF n DO outfn(i_setlabEs, n)
}

AND transrhs(x) BE
{ IF x=0 RETURN
  SWITCHON h1!x INTO
  { DEFAULT:
      RETURN

    CASE s_and:
    { LET len = length(x)
      mapb(transrhs, x)
      outfn(i_tuple, len)
      ssp := ssp - len + 1
      outf(i_formLvalue)
      RETURN
    }

    CASE s_valdef:
      trans(h3!x, m_ref)
      RETURN

    CASE s_rec:
    { LET defs = h2!x
      outf(i_loadE)
      upssp(1)
      declguesses(defs) 
      //abort(666601)
      transrhs(defs)
      //abort(666602)
      initvars(defs)
      //abort(666603)
      loaddefinee(defs)
      outf(i_restoreE1)
      ssp := ssp-1
      RETURN
    }

    CASE s_where:
    CASE s_within:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      LET defs, body = 0, 0
      TEST h1!x=s_within THEN defs, body := h2!x, h3!x
                         ELSE defs, body := h3!x, h2!x
      transrhs(defs)
      outfl(i_blocklink, lab1)
      IF ssp>mssp DO mssp := ssp
      { LET a, b = ssp, mssp
        ssp, mssp := 1, 1
        outfl(i_save, lab2) // lab2 will be set to the
	                    // required size of the stack.
        declvars(defs)
        transrhs(body)
        outf(i_return)
        UNLESS ssp=1 DO trnerr("SSP error ssp=%n should be 1", ssp)
        outlabset(lab2, mssp)
        ssp, mssp := a, b
      }
      outlab(lab1)
      RETURN
    }
  }
}

AND declvars(x) BE
{ IF x=0 RETURN
//writef("declvars: x=%n h1!x=%s*n", x, opstr(h1!x))
  SWITCHON h1!x INTO
  { DEFAULT:
      trnerr("declvars: Bad bound variable list, x=%n", x)
      abort(999)
      RETURN

    CASE s_name:
      // x -> [Name, link, <packed chars>] ie a name node.
      // i_declvar declares variable x with initial value
      // taken from the stack.
      outfvar(i_declvar, x)
      ssp := ssp-1
      RETURN

    CASE s_comma:
    { LET len = length(x)
      // Use i_declnames to declare the names whose initial
      // values are on the stack.
      LET prevssp = ssp
      outfn(i_declvars, length(x))
      mapf(outnameitem, x) // Output the name items
      ssp := prevssp-1
      RETURN
    }

    CASE s_and:
    { LET len = length(x)
      outfn(i_members, len)
      upssp(len-1+len)
      mapf(declvars, x)
      ssp := ssp-len
      RETURN
    }

    CASE s_rec:
    CASE s_valdef:
      declvars(h2!x)
      RETURN

    CASE s_where:
    CASE s_within:
      declvars(h3!x)
      RETURN

    CASE s_mpt:
      outf(i_testEmpty)
      ssp := ssp-1
      RETURN
  }
}

AND declvar(name) BE
{ //abort(12347)
  outvar(name)
  outnamecomment(name)
  ssp := ssp-1
}

AND initvar(name) BE
{ //abort(12348)
  outvar(name)
  ssp := ssp-1
}

AND loaddefinee(x) BE
{ IF x=0 RETURN
  SWITCHON h1!x INTO
  { DEFAULT:
      trnerr("Bad definition")
      RETURN

    CASE s_name:
      outfvar(i_loadR, x)
      upssp(1)
      outf(i_formLvalue)
      RETURN

    CASE s_and:
    CASE s_comma:
    { LET len = length(x)
      LET prevssp = ssp
      mapb(loaddefinee, x)
      outfn(i_tuple, len)
      ssp := prevssp
      upssp(1)
      outf(i_formLvalue)
      RETURN
    }

    CASE s_rec:
    CASE s_valdef:
    CASE s_where:
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
      upssp(1)
      outfvar(i_declvar, x)
      ssp := ssp-1
      RETURN

    CASE s_and:
    CASE s_comma:
      mapf(declguesses, x)
      RETURN

    CASE s_rec:
    CASE s_valdef:
    CASE s_where:
      declguesses(h2!x)
      RETURN

    CASE s_within:
      declguesses(h3!x)
      RETURN
  }
}

AND initvars(x) BE
{ IF x=0 RETURN
//writef("*ninitvars, location:%n op=%s*n", codep, opstr(h1!x))
//abort(99901)
  SWITCHON h1!x INTO
  { DEFAULT:
      trnerr("Bad definition")
      RETURN

    CASE s_name:
      // Output the location, varno and name comment
      outnameitem(x)
      ssp := ssp-1
      RETURN

    CASE s_and:
    { // The top of the stack will be a tuple of initial values
      // of a simultaneous recursive definition.
      // First expand the tuple as separate elements on th stack
      LET len = length(x)
      //writef("After expanding the tuple, len=%n*n", len)
      //abort(12347)
      outfn(i_members, len)
      upssp(len-1)
      // The use i_initvars to update the values of the variables declared
      // in the and construct.
      outfn(i_initvars, len)
      //abort(1000)
      mapf(initvars, x) // Output lines such as: 34: 19 // x
                        // There should len of them.
      //abort(555501)
      //ssp := ssp-len
      RETURN
    }

    CASE s_comma:
    { // The top elements of the stack are the initial values
      // of a simultaneous recursive definition.
      LET len = length(x)
      outfn(i_initvars, len)
      mapf(initvars, x) // Output the variable numbers on separate lines
      ssp := ssp-1-len
      RETURN
    }

    CASE s_rec:        // x -> [Rec, defs]
    CASE s_valdef:     // x -> [Valdef, defs, body]
      initvars(h2!x)
      RETURN

    CASE s_where:      // x -> [Where, body, defs]
      initvars(h3!x)
      RETURN

    CASE s_within:
      initvars(h3!x)
      RETURN
  }
}

AND transscope(defs, body, saveL, mode) BE
{ // The argument is on the top of the stack.
  // This holds the value(s) of any variables declared at the
  // start of the scope.
  // saveL is the label number that will hold the maximum value
  // of ssp observed during the translation of the body.
  // Note that mssp is always >= ssp Both are initially set to 1.
  LET a, b = ssp, mssp
  ssp, mssp := 1, 1
  outfl(i_save, saveL)
  // Save creates a stack of the required size for the function
  // Note that transscope is called when compiling
  // a lambda expression,
  // a let construct expression or
  // a valof expression.
  declvars(defs)
  translabels(body)
  trans(body, mode)
  UNLESS ssp=1 DO trnerr("SSP error ssp=%n should be 1", ssp)
  outf(i_return)
  outlabset(saveL, mssp)
  ssp, mssp := a, b
}

AND mapf(fn, x) BE
{ // x -> [op, len, <elements>]
  LET len = h2!x
  FOR i = 1 TO len DO fn(x!(i+1))
}

AND mapb(fn, x) BE
{ // x -> [op, len, <elements>]
  LET len = h2!x
  FOR i = len TO 1 BY -1 DO fn(x!(i+1))
}

AND length(x) = h1!x=s_and | h1!x=s_comma -> h2!x, 1

AND upssp(x) BE
{ ssp := ssp+x
  IF ssp>mssp DO mssp := ssp
}

AND wrf(form, a, b, c, d, e, f, g) BE IF optCode DO
  writef(form, a, b, c, d, e, f, g)

AND outf(op) BE
{
  //abort(12999)
  outssp()
  wrf("%i5: %s*n", codep, opstr(op))
  putc(op)
}

AND outfflt(op, val) BE
{ outssp()
  wrf("%i5: %s %6.3f*n", codep, opstr(op), val)
  putc(op)
  putc(val)
}

AND outfvar(op, name) BE
{ // Output location, op, variable number and name comment
  outssp()
  wrf("%i5: %s ", codep, opstr(op))
  putc(op)
  outvar(name)
  outnamecomment(name)
  //abort(1000)
}

AND outname(name) BE
{ // Output location, varno and name as a comment.
  // Used in simultaneous recursive definitions giving
  // the variable numbers in an initvars instruction.
  outssp()
  wrf("%i5: ", codep)
  outvar(name)
  outnamecomment(name)
}

AND outnameitem(name) BE
{ // Output location, varno and name as a comment.
  // Used in simultaneous recursive definitions giving
  // the variable numbers in an initvars instruction.
  wrf("%i5: ", codep)
  outvar(name)
  outnamecomment(name)
}

AND outvar(name) BE
{ // name is a name node for a Pal variable
  // Output just the varno of the name
  LET varno = str2varno(@h3!name)
  putc(varno)
  wrf("%n", varno)
  //abort(555501)
}

AND outfstring(op, x) BE
{ // Output location, op, string number and string
  wrf("%i5: %s ", codep, opstr(op))
  putc(op)
  outstring(x)
}

AND outstring(x) BE
{ // Output location, varno and name
  LET str = @h2!x
  LET strno = str2varno(str)
  wrf("%n   // '%s'*n", strno, str)
  putc(strno)
}

AND outfv(op, var) BE
{ outssp()
  wrf("%i5: %s %s*n", codep, opstr(op), var)
  putc(op); putc(var)
}

AND outfn(op, a) BE
{ outssp()
  wrf("%i5: %s %n*n", codep, opstr(op), a)
  putc(op); putc(a)
}

AND outfnn(op,a, b) BE
{ outssp()
  wrf("%i5: %s %n %n*n", codep, opstr(op), a, b)
  putc(op); putc(a); putc(b)
}

AND outfvarl(op, name, lab) BE
{ // name points to the name or string node of a Pal variable
  // It is looked up in the self expanding sxvstr, adding a
  // new entry if necessary.
  //LET namestr = @h3!name
  //LET namepos = str2varno(namestr)
  putc(op)
  outvar(name)
  putc(lab)
  outssp()
  wrf("%i5: %s", codep, opstr(op))
  wrf(" L%n", lab)
  outnamecomment(name)
}

AND outnamecomment(name) BE
{ wrf(" // %s*n", @h3!name)
}

AND outfl(op, lab) BE
{ outssp()
  wrf("%i5: %s L%n*n", codep, opstr(op), lab)
  putc(op); putref(lab)
}

AND outlab(lab) BE
{ outssp()
  wrf("%i5: Lab L%n*n", codep, lab)
  setlab(lab, codep)
}

AND outlabset(lab, val) BE
{ outssp()
  wrf("%i5: Equ L%n %n*n", codep, lab, val)
  setlab(lab, val)
}

AND outentry(l1, l2) BE
{ outssp()
  wrf("%i5: Entry L%n L%n*n", codep, l1, l2)
  putref(l2)
  setlab(l1, codep)
}

AND putc(w) BE TEST codep>codet
               THEN trnerr("More code space needed")
               ELSE { codev!codep := w
                      codep := codep+1
                    }

AND putref(lab) BE TEST codep>codet
                   THEN trnerr("More code space needed")
                   ELSE { codev!codep := refv!lab
                          refv!lab := codep
                          codep := codep+1
                        }

AND setlab(lab, addr) BE
{ ///writef("setlab: labv=%n lab=L%n addr=%n*n", labv, lab, addr)
  labv!lab := addr
}

AND nextlab() = VALOF
{ TEST labnumber>=labmax
  THEN fatalerr("More label space needed")
  ELSE labnumber := labnumber + 1
  RESULTIS labnumber
}
 

AND resolvelabels() BE FOR lab = 1 TO labnumber DO
{ LET p      = refv!lab
  AND labval = labv!lab
  //sawritef("resolvelabels: Entered*n")
  IF p & labval<0 TEST lab=1 THEN trnerr("start not defined")
                             ELSE trnerr("Label %n unset", lab)
  WHILE p DO { LET np = codev!p
               codev!p := labval
               p := np
             }
}

// End of the front end.

AND sxvpush(sxv, val) = VALOF
{ // Push a value into the self expanding vector sxv.
  LET upb = sxv!0      // =0 or the upb of v
  LET v   = sxv!1      // =0 or a getvec'd vector for the elements.
  LET p = v -> v!0, 0  // Position of the previous element, if any.
  // Initially upb, v, and p are all zero.
  // If v is not zero, v!0 will be the subscript of its latest element in v.
  // If the vector is full, pushval will allocate another larger vector
  // and copy the existing elements into it before pushing x.
  // The result is the subscript of v where val is stored.
//  writef("sxvpush(%n,%i6): upb=%n v=%n p=%n*n", sxv,val,upb,v,p)
//abort(7154)  
  IF p>=upb DO
  { LET newupb = 3*upb/2 + 100 // upb of the new larger vector
    LET newv = getvec(newupb)
    UNLESS newv DO
    { writef("More memory needed for pushval*n")
      abort(999)
      RETURN
    }
    sxv!0 := newupb // Update the control block
    sxv!1 := newv

    FOR i = 0 TO upb DO newv!i := v!i      // Copy the existing elements
    FOR i = upb+1 TO newupb DO newv!i := 0 // Pad with zeroes

    IF v DO freevec(v) // Free the old vector if it existed.

    v := newv
  }
  p := p+1
  v!0, v!p := p, val
  RESULTIS p
}


// The rest is xpal derived from the June 1970 version for
// the IBM 360 machine at MIT. The systematic changes are
// as follows.

// Most identifiers are changed to lower case.
// *( is replaced by !(
// Redundant parens are removed.
// The variables s, c and e are replaced by stack, pc and env.
// a and b are replaced by rega and regb.
// Runtime data is held in the self expanding vector palsxv.
// Garbage collection is done by copying accessible data in
// palsxv into a new self expanding vector and replacing the
// old one with the new copy.
// Floating point operations use the BCPL floating point
// operators providing 32 or 64 bit floating point depending
// on which BCPL system is being used.
// The required header declarations are combined with those
// at the start of pal.b.


LET execpal() = VALOF
{ // Execute one or more Pocode instructions.
  // It return 0 for successful completion
  // A non zero result indicates an error.
  
  // The PAL runtime memory is held the self expanding vector:
  // palsxv -> [datavupb, datav]
  // datav -> [pos,...] pos is the position of the next free element.
  
  // sp   is the addr of the second to top item of the stack
  //      relative to stack.
  // asp  is the abs addr of the second to top item of the stack.
  //      ie datav_stack_sp. It is reset to #xAABBAABB whenever
  //      datav changes.
  // pc   abs pointer to the next instruction to execute in codev.
  //      This is only used after all the code has been compiled.
  // env  holds E -> [5, Env, link, name, value]

  // h3!(datav_lookupcounter)  is a counter that is incremented
  //      every time a Pocode instruction is executed. This is
  //      the value of the Pal variable LOOKUPNO.

  // Garbage collecton is done by copying all reachable elements
  // of palsxv into a new self expanding vector. Then freeing the
  // old one and replacing palsxv with the new one.

  LET errorstr    = "SYSTEMERROR"
  LET lookupnostr = "LOOKUPNO"
  LET resstr      = "#res#"
  
  // This implementation is designed to run on the modern
  // BCPL Cintcode system. It is closely related to the 1970
  // frontend PAL/pal/docs/pal70-mabee-12jun70.pdf and the
  // interpreter PAL/pal/xpal70orig.b
  
  // For more efficient Cintcode
  // asp    is datav+stck+sp, reset to #xAABBAABB whenever datav changes.
  // asp!1  is the top item on the stack and
  // asp!0  is the next item down
  // pc     is CODEV+C of xpal70orig.b so
  // pc!0   is the next Pocode op code
  //        Op codes are integers like i_loadR which
  //        calls function f_loadR when executed.
  // pc!1   is the first argument of the instruction, if any

  retcode := 0 // The return code from the interpreter
               // 0 means halt was obeyed
 
  // Set the initial runtime state

  pc := codev + startLoc // Rel addr of the next instrucion to obey.
  oldc := 0

  sp := 0    // Address relative to datav+stack of the second to
             // top element, when set
  asp := 0   // The absolute address corresponding to sp
             // typically = datav+stack+sp
  // asp!1   // Holds the top element of the stack
  // asp!0   // Holds the second from top element of the stack
  stack := 0
  env   := 0 // abs addr -> [5, Env, link, name, value] or zero
  rega  := 0 // Zero or abs address of a runtime node.
  regb  := 0 // Zero or abs address of a runtime node.

  // Setup the runtime data space
  datavupb := 0
  datav    := 0
  palsxv   := @datavupb
  sxvpush(palsxv, 0) // Ensure that datav is non zero
  lookupcounter := list(3, t_int, 0)
  count := 0                  // Count of instruction executions
  
  gclimit := 1_000 // Choose a larger value later
  
  writef("XPAL 5 ENTERED*n*n")
  
  mapliblist(f_libname)

  //nameres := f_libname(resstr,   0)

//  f_libname(errorstr, 0)
//  f_libname(lookupnostr,   0)

  gcmark := 0
  mapliblist(f_decllib)

  // The following variables must be regarded as root by the
  // garbage collector.
  guessrv := list(2, t_guess)
  truerv  := list(2, t_true)
  falserv := list(2, t_false)
  nilrv   := list(3, t_tuple, 0)  // nil is a null tuple
  dummyrv := list(2, t_dummy)
  nilsrv  := list(2, t_nils)

//env := list(5, t_env, env, 0, env) // DUMMY ENV NODE USED BY DIAGNOSE
                                     // varname zero has value env

  rega    := list(3, t_basicfn, f_diagnose)
  errorlv := list(3, t_lvalue, rega)
  env     := list(5, t_env, env, str2varno(errorstr), errorlv)
  rega := 0

  lookupnovarno := str2varno(lookupnostr)
  rega := list(3, t_lvalue, lookupcounter) // Count of lookups
  env := list(5, t_env, env, lookupnovarno, rega)
  rega := 0

  errflag := FALSE
  floterr := FALSE
  listv := listp
  restartc := 0
  nset := FALSE
  errcount := 0
  maxerr := 10
  done := FALSE

 //prstate("execpal: entering the execution loop*n*n")
 //abort(3344)
  UNTIL done DO
  {
    IF optTrace DO
    { prstate("execpal: About to execute an instruction")
      abort(10000)
    }

    count := count+1

    SWITCHON h1!pc INTO
    { DEFAULT:
        writef("*npc=%i5: Unimplemented Pocode op: ", pc-codev)
        prinstr(pc)
	newline()
        done := TRUE
	abort(999)
        LOOP

      CASE i_setlabEs:    f_setlabEs();    LOOP
      CASE i_restoreE1:   f_restoreE1();   LOOP
      CASE i_formRvalue:  f_formRvalue();  LOOP
      CASE i_formLvalue:  f_formLvalue();  LOOP
      CASE i_tuple:       f_tuple();       LOOP
      CASE i_members:     f_members();     LOOP
      CASE i_loadGuess:   f_loadGuess();   LOOP
      CASE i_true:        f_true();        LOOP
      CASE i_false:       f_false();       LOOP
      CASE i_finish:      f_finish();      LOOP
      CASE i_lose1:       f_lose1();       LOOP
      CASE i_mult:        f_mult();        LOOP
      CASE i_div:         f_div();         LOOP
      CASE i_plus:        f_plus();        LOOP
      CASE i_minus:       f_minus();       LOOP
      CASE i_neg:         f_neg();         LOOP
      CASE i_eq:          f_eq();          LOOP
      CASE i_ls:          f_ls();          LOOP
      CASE i_gr:          f_gr();          LOOP
      CASE i_le:          f_le();          LOOP
      CASE i_ne:          f_ne();          LOOP
      CASE i_ge:          f_ge();          LOOP
      CASE i_logand:      f_logand();      LOOP
      CASE i_logor:       f_logor();       LOOP
      CASE i_save:        f_save();        LOOP
      CASE i_apply:       f_apply();       LOOP
      CASE i_not:         f_not();         LOOP
      CASE i_jj:          f_loadJ();       LOOP
      CASE i_update:      f_update();      LOOP
      CASE i_res:         f_res();         LOOP
      //CASE i_goto:        f_goto();        LOOP
      CASE i_loadE:       f_loadE();      LOOP
      CASE i_loadR:       f_loadR();       LOOP
      CASE i_loadL:       f_loadL();       LOOP
      CASE i_loadS:       f_loadS();       LOOP
      CASE i_loadN:       f_loadN();       LOOP
      CASE i_loadF:       f_loadF();       LOOP
      //CASE i_testEmpty:   f_testEmpty();   LOOP
      CASE i_declvar:     f_declvar();    LOOP
      CASE i_declvars:    f_declvars();   LOOP
      CASE i_initvar:     f_initvar();    LOOP
      CASE i_initvars:    f_initvars();   LOOP
      CASE i_formClosure: f_formClosure(); LOOP
      CASE i_jumpF:       f_jumpF();       LOOP
      CASE i_jump:        f_jump();        LOOP
      CASE i_decllabel:   f_decllabel();   LOOP
      CASE i_return:      f_return();      LOOP
      CASE i_blocklink:   f_blocklink();   LOOP
      CASE i_reslink:     f_reslink();     LOOP
      CASE i_power:       f_power();       LOOP
      CASE i_nil:         f_nil();         LOOP
      //CASE i_halt:        f_halt();        LOOP
      //CASE i_dummy:       f_dummy();       LOOP
      CASE i_aug:         f_aug();         LOOP
      CASE i_setup:       f_setup();       LOOP
      //CASE i_rvrestart:   f_rvrestart();   LOOP
      CASE i_okrestart:   okrestart();     LOOP
    }
  }

  RESULTIS retcode
}

AND prstate(str, x) BE //IF FALSE DO
{ LET e = env
  LET s = datav+stack
  writef("*nprstate: ")
  writef(str, x)
  newline()
  writef("palsxv->[%n %n] codev=%n oldc=%n*n",
          datavupb, datav, codev,   oldc)
  writef("strsxv->[%n %n]*n", strvupb, strv)

  writef("rega=")
  //newline()
  //abort(1000)
  prvalue(rega, 3)
  newline()
  writef("regb=")
  //abort(4001)
  prvalue(regb, 3)
  newline()

  writef("stack=")
  prvalue(stack, 2)
  newline()
  IF stack & sp DO
  { LET s = datav+stack         // Abs addr of stack
    LET p = s+sp
    WHILE h4!s DO
    { s := h5!s
      writef("      ")
      prvalue(s, 2)
      s := datav+s
      newline()
    }
    writef("  Top: %2i: ", sp+1); prvalue(p!1); newline()
    writef("  2nd: %2i: ", sp+0); prvalue(p!0); newline()
  }
  writef("sp  = %n*n", sp)
  writef("asp = %n", asp)
  IF asp TEST asp>0
  THEN writef(" = %n + %n + %n", datav, stack, sp)
  ELSE writef(" = #x%8x", asp)
  newline()
//abort(1928)
//IF FALSE DO
  IF env DO
  { LET layout = 0
    LET e = env
    writef("env nodes:")
    WHILE e>0 DO
    { writef(" %n", e)
      e := h3!(datav+e)
      layout := layout+1
      IF layout > 10 DO { newline(); layout := 0 }
    }
    IF layout DO newline()
    e := env
    FOR i = 1 TO 8 IF e DO
    { // e -> [ 5, Env, link, name, val]
      LET p = datav+e
      LET link = h3!p
      writef("%i5: %16t value: ",
              e, strv+h4!p)
      prvalue(h5!p, 3)
      newline()
      UNLESS link BREAK
      e := link
    }
  }
  
  writef("pc = %n", pc)
  //newline()
  IF pc DO writef(" = %n + %n", codev, pc-codev)
  writef(": ")
  //IF env>100 DO abort(1235)
  prinstr(pc)
  newline()
  IF codep = 68 DO abort(3333)
}

AND prinstr(p) BE
{ 
  writef("%s", opstr(h1!p))
  SWITCHON h1!p INTO
  { DEFAULT:
      FOR i = 1 TO Operands(h1!p) DO writef(" %i2", p!i)
      newline()
      ENDCASE
       
    CASE i_loadF:
      writef(" %5.3f*n", p!1)
      ENDCASE
      
    CASE i_loadR:
    CASE i_loadL:
    CASE i_loadS:
      writef(" %n  // '%s'*n", p!1, strv+p!1)
      ENDCASE

    CASE i_decllabel:
      writef(" %n L%n  // '%s'*n", pc!1, p!2, strv+p!1)
      ENDCASE

    CASE i_initvars:
      FOR i = 1 TO 1+h2!p DO writef(" %i2", p!i)
      newline()
      ENDCASE
  }
  IF codep=68 DO abort(111100)
  //abort(1111)
}

AND node(n) = VALOF
{ LET res = h1!datav + 1 // Rel address of the new node in datav
  LET args = @n
  LET olddatav = datav
  mvdatav()
  FOR i = 0 TO n-1 DO sxvpush(palsxv, 0)
  asp := #xAABBAABB
  IF FALSE DO
  UNLESS datav=olddatav DO
  { writef("node: n=%n olddatav=%n datav=%n*n", n, olddatav, datav)
    abort(6665)
  }
  RESULTIS res // relative to datav which may have been changed by sxvpush
}

AND mvdatav() BE //IF FALSE DO
  IF datav DO
  { LET upb = datavupb
    LET v = getvec(upb)
    //writef("mvdatav: upb=%n prev datav=%n new datvv=%n*n", upb, datav, v)
    //abort(1007)
    FOR i = 0 TO upb DO v!i := datav!i
    freevec(datav)
    datav := v
    asp := #xAABBAABB
    //abort(1009)
  }

AND tstasp() BE IF FALSE DO
IF asp & stack & sp DO
{ TEST asp>0
  THEN UNLESS asp = datav+stack+sp DO
       { writef("datav=%n stack=%n sp=%n asp=%n*n", datav, stack, sp, asp)
         abort(7722)
       }
  ELSE { writef("asp=#x%8x*n", asp)
         abort(7723)
       }
}

AND list(n, a, b, c, d, e, f, g) = VALOF
{ LET olddatav = datav
  LET res = sxvpush(palsxv, n) // Address rel to datav of next free location
                               // Note that datav may have been changed
  LET args = @n
//  writef("list n=%n a=%n res=%n datav=%n*n", n, a, res, datav)
  FOR i = 1 TO n-1 DO sxvpush(palsxv, args!i) // push values into datav
  mvdatav()
  // datav may have changed, so various abs addresses auch as sp may be
  // invalid
  asp := sp -> datav+stack+sp, 0
//  abort(1008)
  IF FALSE DO
    UNLESS datav=olddatav DO
  { writef("#######  list: n=%n olddatav=%n datav=%n*n", n, olddatav, datav)
    abort(6665)
  }

  IF FALSE DO
  { LET r = datav+res
    writef("list: => %n+%n -> %n[", r, datav, res)
    FOR i = 0 TO n-1 SWITCHON i INTO
    { DEFAULT:
        writef(" %n", r!i)
        ENDCASE

      CASE 1:
        //writef(" r!i=%n=", r!i)
        writef(" %s", opstr(r!i))
	//writef("*nXXX*n")
        ENDCASE
      
      CASE 3:
      { LET val = r!i
        //writef(" i=%n val=%n", i, val)
        TEST a=t_env
        THEN writef(" %s", strv+val)
        ELSE writef(" %n", val)
     
        ENDCASE
      }
    }
    writef("]*n")
    //IF a=t_env & c=0 DO
    //abort(5678)
  }
  
  RESULTIS res // Address rel of item in palsxv, ie rel to datav
}

AND printf(mem, form, p) BE  // Not used ?????
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

AND wrs(s, n) BE // Only used by printf ?????
{ LET len = 0
  WHILE s%len DO len := len+1
  FOR i = len+1 TO n DO wrch(' ')
  FOR i = 0 TO len-1 DO wrch(s%i)
}

AND Operands1(op) = VALOF
{ LET n = Operands1(op)
  writef("Operands: op=%n=%s*n", op, opstr(op))
  writef("Operands => %n*n", n)
  //abort(1000)
  RESULTIS n
}

AND Operands(op) = VALOF SWITCHON op INTO
{ DEFAULT:
    writef("*nOperands: op=%n=%s result unknown*n", op, opstr(op))
    abort(999)
    RESULTIS 0

  CASE i_decllabel:
    RESULTIS 2

  CASE i_save:
  CASE i_loadR:
  CASE i_loadL:
  CASE i_loadS:
  CASE i_loadN:
  CASE i_loadF:
  CASE i_setup:
  CASE i_tuple:
  CASE i_blocklink:
  CASE i_declvar:
  CASE i_declvars:
  CASE i_formClosure:
  CASE i_jump:
  CASE i_update:
  CASE i_members:
  CASE i_initvar:
  CASE i_initvars:
  CASE i_jumpF:
  CASE i_setlabEs:
  CASE i_reslink:
    RESULTIS 1

  CASE i_loadE:
  CASE i_loadGuess:
  CASE i_true:
  CASE i_false:
  CASE i_nil:
  CASE i_lose1:
  CASE i_power:
  CASE i_mult:
  CASE i_div:
  CASE i_plus:
  CASE i_minus:
  CASE i_neg:
  CASE i_eq:
  CASE i_ls:
  CASE i_gr:
  CASE i_le:
  CASE i_ne:
  CASE i_ge:
  CASE i_logand:
  CASE i_logor:
  CASE i_not:
  CASE i_jj:
  CASE i_goto:
  CASE i_formRvalue:
  CASE i_formLvalue:
  CASE i_halt:
  CASE i_apply:
  CASE i_finish:
  CASE i_rvrestart:
  CASE i_return:
  CASE i_aug:
  CASE i_restoreE1:
  CASE i_okrestart:
  CASE i_res:
    RESULTIS 0
}


// The rest is XPal dating from about June 1970



// XPALHD LAST MODIFIED ON FRIDAY, 12 JUNE 1970
// AT 5:29:07.24 BY R MABEE
// LISTING OF PAL RUN TIME SYSTEM (XPAL) HEADFILE AND BCPL/360 BASIC
// HEADFILE GOTTEN WITHIN SUPPRESSED BY NOLIST DIRECTIVE. TO OVERRIDE
// DIRECTIVE, SPECIFY ALLSOURCE OPTION TO BCPL COMPILER.
//>>> nolist
//>>> eject
//
//	 ******************************
//	 *                            *
//	 *           XPALHD           *
//	 *                            *
//	 *  (COMPATIBLE WITH PALSYS)  *
//	 *                            *
//	 ******************************
//
// GET BASIC BCPL/360 HEAD FILE
//>>> GET "BASIC"


//>>> eject
// XPAL1B
LET mapliblist(f) BE
{ // Apply function f to every library action function.
  // f will be either f_libname or f_decllib
  
  f("PRINT",         f_print)
//  f("PAGE",          f_userpage)
//  f("STEM",          f_stem)
//  f("STERN",         f_stern)
//  f("CONC",          f_conc)
//  f("ATOM",          f_atom)
//  f("NULL",          f_null)
//  f("ORDER",         f_length)
//  f("ISTRUTHVALUE",  f_istruthvalue)
//  f("ISINTEGER",     f_isnumber)
//  f("ISREAL",        f_isreal)
//  f("ISSTRING",      f_isstring)
//  f("ISFUNCTION",    f_isfunction)
//  f("ISLABEL",       f_islabel)
//  f("ISTUPLE",       f_istuple)
//  f("ISDUMMY",       f_isdummy)
//  f("ISENVIRONMENT", f_isenvironment)
//  f("FINISH",        f_finish)
//  f("SHARE",         f_share)
//  f("STOI",          f_ston)
//  f("CTOI",          f_cton)
//  f("ITOC",          f_itoc)
//  f("RTOI",          f_rton)
//  f("ITOR",          f_ntor)
//  f("READCH",        f_rdchar)
//  f("DIAGNOSE",      f_diagnose)
//  f("LASTFN",        f_lastfn)
//  f("TABLE",         f_table)
//  f("LOOKUPINE",     f_lookupinE)
//  f("SAVEENV",       f_saveenv)
}

AND f_libname(x, y) = VALOF
{ // Store a name in the table of names
  // x is the BCPL string of the variable name
  // y is its value, ignored by f_libname.
  // The string x is looked up/stored in sxvstr
  // The result is the corresponding variable number ie the
  // position in strv of the stored string.
  LET varname = str2varno(x) // Put the name string in sxvstr
//writef("libname: x=%s y=%n varname=%n*n", x, y, varname)
//abort(1006)
  RESULTIS varname
}

AND f_decllib(x, y) BE
{ // x is the name of a builtin library function
  //   as a BCPL string.
  // y is the corresponding basicfn number.
  // A suitable item is added to the start of env.
  LET varname = str2varno(x)
  //writef("f_decllib: x=%s %n*n", x, y, varname)
  rega := list(3, t_basicfn, y)
  rega := list(3, t_lvalue, rega)
  env  := list(5, t_env, env, varname, rega)
  rega := 0
  //writef("f_decllib: %n+%n -> [", datav, env)
  //FOR i = 0 TO 4 DO writef(" %n", datav!(env+i))
  //writef(" ]*n")
//  abort(1007)
}

// XPAL2

LET f_loadL() BE
{ rega := lvofname(pc!1, env)
  TEST rega=nilrv
  THEN { rega := list(3, t_lvalue, rega)
	 errokdbg()
       }
  ELSE { push(rega)
         pc := pc+2
       }
}

AND f_loadR() BE  // [ LoadR, var ]
{ rega := lvofname(pc!1, env)
  TEST rega=nilrv
  THEN errdbg()
  ELSE { rega := h3!(datav + rega)
	 push(rega)
	 pc := pc+2
       }
}

AND f_loadJ() BE
{ LET s = datav + stack
  rega := list(5, t_jj, h4!s, h5!s, h6!s )
  push(rega)
  writef("f_loadJ: rega=%n = %n+%n -> [", datav+rega, datav, rega)
  FOR i = 0 TO 4 DO
    writef(" %n", datav!(rega+i))
  writef("]*n")
//abort(3333)
  pc:=pc+1
}

AND f_loadE() BE
{ rega := env
  push(rega)
  pc:=pc+1
}

AND f_loadS() BE
{ LET str = strv+pc!1
  LET upb = str%0
  rega := nilsrv
  FOR i = upb TO 0 BY -1 DO
    rega := list(4, t_string, rega, str%i)
  push(rega)
  pc:=pc+2
}

AND f_loadN() BE // Load an int
{ rega := list(3, t_int, pc!1)
  asp := datav+stack+sp
  push(rega)
  tstasp()
  pc:=pc+2
}

AND f_loadF() BE // Not in xpal70, This loads a real
{ rega := list(3, t_real, pc!1)
  asp := datav+stack+sp
  push(rega)
  tstasp()
  pc:=pc+2
}

AND f_restoreE1() BE
{ env := asp!0  //stack!(stackp-2)
  tstasp()
  asp!0 := asp!1 // stack!(stackp-1) := stack!(stackp) after dec stackp
  sp := sp-1
  asp := asp-1
  tstasp()
  pc := pc+1
}

AND f_true() BE
{ rega := truerv
  push(rega)
  tstasp()
  pc:=pc+1
}

AND f_false() BE
{ rega := falserv
  push(rega)
  tstasp()
  pc:=pc+1
}

AND f_loadGuess() BE
{ // Push the LV of guessrv onto the stack
  rega := mklvnode(guessrv)
  push(rega)
  tstasp()
  pc:=pc+1
}

AND f_nil() BE
{ rega := mklvnode(nilrv)
  push(rega)
  tstasp()
  pc:=pc+1
}

AND f_dummy() BE
{ rega := mklvnode(dummyrv)
  push(rega)
  tstasp()
  pc:=pc+1
}

AND f_formClosure() BE
{ rega := list(4, t_closure, env, pc!1)
  push(rega)
  tstasp()
  pc := pc+2
}

AND f_formLvalue() BE
{ LET prevdatav = datav
  tstasp()
  rega := list(3, t_lvalue, asp!1)
  asp := datav+stack+sp
  asp!1 := rega
  pc := pc+1
}

AND mklvnode(x) = VALOF
{ LET a = list(3, t_lvalue, x)
  asp := datav + stack + sp
  tstasp()
  //writef("mklvnode: a=%n*n", a)
  //prstate("In mklvnode, sp=%n", sp)
  RESULTIS a
}

AND nextlv11() BE
{ rega := mklvnode(rega)
  push(rega)
  tstasp()
  pc:=pc+1
}

AND pushlva() BE
{ rega := mklvnode(rega)
  push(rega)
  tstasp()
}

AND f_formRvalue() BE
{ UNLESS h2!(datav+asp!1)=t_lvalue DO
  { writef("System error in f_formRvalue*n")
    abort(999)
  }
  tstasp()
  asp!1 := h3!(datav+asp!1)
  pc := pc+1
}

AND f_tuple() BE
{ // Create a pointer to [n+2, Tuple, n, a1,a2,...,an]
  // where n is the argument of i_tuple
  LET n = pc!1 // The number of elements in the tuple
  LET a = 0
  LET s = 0
  rega := node(n+3)
  a := datav+rega
  s := datav+stack
  asp := s+sp
  tstasp()
  a!0, a!1, a!2 := n+3, t_tuple, n
  FOR i = 3 TO n+2 DO
  { a!i := asp!1
    sp := sp - 1
    asp := asp-1
  }
  push(rega)
  tstasp()
  pc:=pc+2
}

AND f_members() BE
{ LET n = pc!1 // The number of members
  LET a, b = 0, 0
  tstasp()
  rega := pop() // Should be a tuple with n members
                // ie rega -> [n+2, Tuple, n, x1, .. xn]
  //prstate("f_members:*n")
  //abort(1000)
  regb := h3!(datav+rega) // Get the RV of the tuple
  b := datav + regb         // Get the abs address of the tuple
  FOR i = -2 TO n-3 DO
  { sp := sp+1            // Load the tuple elements in reverse order
    asp := asp+1
    asp!1 := b!(n-i)
  }
  pc := pc+2
}

AND f_not() BE
{ LET a = 0
  tstasp()
  rega := pop()
  a := datav + rega
//prstate()
//abort(1000)
  IF h2!a=t_false DO
  { rega := truerv
    push(rega)
    pc:=pc+1
    RETURN
  }

  IF h2!a=t_true DO
  { rega := falserv
    push(rega)
    pc:=pc+1
    RETURN
  }
  
  error1("NOT", rega, 0)
  errdbg()
}

AND f_logand() BE
{ LET a = 0
  tstasp()
  rega := pop()
  regb := pop()
  a := datav+rega
  TEST testbools2()
  THEN { rega := h2!a=t_true -> regb, falserv
	 push(rega)
	 pc:=pc+1
       }
  ELSE { error1("&", rega, regb)
	 rega := falserv
         errdbg()
       }
}

AND f_logor() BE
{ LET a = 0
  tstasp()
  rega := pop()
  regb := pop()
  a := datav+rega
  TEST testbools2()
  THEN { rega := h2!a=t_false -> regb, truerv
	 push(rega)
	 pc:=pc+1
       }
  ELSE { error1("OR", rega, regb)
	 rega := falserv
	 errdbg()
       }
}

AND f_aug() BE
{ LET a, b = 0, 0
  tstasp()
  rega := pop() // The tuple Rvalue
  regb := pop() // The right hand argument of aug, an Lvalue
  a := datav+rega
  b := datav+regb
  UNLESS h2!a=t_tuple DO
  { error1("AUG", rega, regb)
    rega := nilrv
    //abort(8679)
    errdbg()
    RETURN
  }
  { LET n = h3!a         // Number of elements in the tuple
    LET tup = node(n+4)
    LET t = datav + tup
    t!0, t!1, t!2 := n+4, t_tuple, n+1
    FOR i = 3 TO n+2 DO t!i := a!i
    t!(n+3) := regb
    rega := tup
    tstasp()
    push(rega)
    pc:=pc+1
  }
}

AND f_res() BE
{ // nameres points to the name node for special variable #res#
  // resno is the corresponding variable number.
  LET a, s = 0, 0
  prstate("f_res: Before calling lvofname")
  abort(432101)
  rega := lvofname(nameres, env)
  prstate("f_res: After calling lvofname")
  abort(432182)
  IF rega=nilrv DO
  { rega := list(3, t_lvalue, rega)
    GOTO reserr
  }
  writef("f_res: arga=%n*n", rega)
  abort(4412)
  rega := h3!(datav+rega)
  a := datav + rega
  UNLESS h2!a=t_jj DO
reserr:	{ error("INCORRECT USE OF RES", 0, 0, 0)
	  errokdbg()
	  RETURN
	}
  s := datav + stack
  h4!s, h5!s, h6!s := h3!a, h4!a, h5!a
  f_return()
}

LET f_mult() BE
{ LET b = pop() + datav // Right operand
  LET a = pop() + datav // Left operand
  tstasp()

  IF h2!b=t_int DO
  { IF h2!a=t_int  DO
    { push(list(3, t_int, h3!b * h3!a))
      pc := pc+1
      RETURN
    }
    IF h2!a=t_real DO
    { push(list(3, t_real, FLOAT h3!b #* h3!a))
      pc := pc+1
      RETURN
    }
  }

  IF h2!b=t_real DO
  { IF h2!a=t_int  DO
    { push(list(3, t_real, h3!b #* FLOAT h3!a))
      pc := pc+1
      RETURN
    }
    IF h2!a=t_real DO
    { push(list(3, t_real, h3!b #+ h3!a))
      pc := pc+1
      RETURN
    }
  }

  push(list(3, t_int, 0))
  error1("+", a, b)
  errdbg()
}

AND f_div() BE
{ LET b = pop() + datav // Right operand
  LET a = pop() + datav // Left operand
  tstasp()

  IF h2!b=t_int DO
  { IF h2!a=t_int  DO
    { IF h3!b=0 GOTO derr
      push(list(3, t_int, h3!b / h3!a))
      pc := pc+1
      RETURN
    }
    IF h2!a=t_real DO
    { IF h3!b=0.0 GOTO derr
      push(list(3, t_real, FLOAT h3!b #/ h3!a))
      pc := pc+1
      RETURN
    }
  }

  IF h2!b=t_real DO
  { IF h3!b=0.0 GOTO derr
    IF h2!a=t_int  DO
    { push(list(3, t_real, h3!b #/ FLOAT h3!a))
      pc := pc+1
      RETURN
    }
    IF h2!a=t_real DO
    { push(list(3, t_real, h3!b #/ h3!a))
      pc := pc+1
      RETURN
    }
  }
derr:
  push(list(3, t_int, 0))
  error1("/", a, b)
  errdbg()
}

AND f_plus() BE
{ LET b = pop() + datav // Right operand
  LET a = pop() + datav // Left operand
  tstasp()

  IF h2!b=t_int DO
  { IF h2!a=t_int  DO
    { push(list(3, t_int, h3!b + h3!a))
      pc := pc+1
      RETURN
    }
    IF h2!a=t_real DO
    { push(list(3, t_real, FLOAT h3!b #+ h3!a))
      pc := pc+1
      RETURN
    }
  }

  IF h2!b=t_real DO
  { IF h2!a=t_int  DO
    { push(list(3, t_real, h3!b #+ FLOAT h3!a))
      pc := pc+1
      RETURN
    }
    IF h2!a=t_real DO
    { push(list(3, t_real, h3!b #+ h3!a))
      pc := pc+1
      RETURN
    }
  }

  push(list(3, t_int, 0))
  error1("+", a, b)
  errdbg()
}

AND f_minus() BE
{ LET b = pop() + datav // Right operand
  LET a = pop() + datav // Left operand
  tstasp()

  IF h2!b=t_int DO
  { IF h2!a=t_int  DO
    { push(list(3, t_int, h3!b - h3!a))
      pc := pc+1
      RETURN
    }
    IF h2!a=t_real DO
    { push(list(3, t_real, FLOAT h3!b #- h3!a))
      pc := pc+1
      RETURN
    }
  }

  IF h2!b=t_real DO
  { IF h2!a=t_int  DO
    { push(list(3, t_real, h3!b #- FLOAT h3!a))
      pc := pc+1
      RETURN
    }
    IF h2!a=t_real DO
    { push(list(3, t_real, h3!b #- h3!a))
      pc := pc+1
      RETURN
    }
  }

  push(list(3, t_int, 0))
  error1("+", a, b)
  errdbg()
}

AND f_power() BE // More work needed
{ LET y = pop()  // Right operand, the exponent
  LET x = pop()  // Left operand
  LET a = y+datav
  LET b = x+datav
  tstasp()

  UNLESS h2!b=t_int GOTO pwerr

  IF h2!a=t_int DO
  { LET base, exp, r = h3!a, h3!b, 1
    TEST exp < 0
    THEN { IF base=0 GOTO pwerr
	   r := ABS base = 1 -> ((-exp & 1)=0 -> 1, base), 0
	 }
    ELSE WHILE exp DO
         { UNLESS (exp & 1)=0 DO r := r * base
	   base := base * base
	   exp := exp >> 1
	 }
    rega := list(3, t_int, r)
    push(rega)
    pc:=pc+1
    RETURN
  }
  
  IF h2!a=t_real DO
  { LET FLT base, exp, FLT r = h3!a, h3!b, 1.0
    TEST exp <= 0
    THEN { IF base=0.0 GOTO pwerr
	   r := ABS base = 1.0 -> ((-exp & 1)=0 -> 1.0, base), 0.0
	 }
    ELSE WHILE exp DO
         { UNLESS (exp & 1)=0 DO r := r * base
	   base := base * base
	   exp := exp >> 1
	 }
    rega := list(3, t_real, r)
    push(rega)
    pc:=pc+1
    RETURN
  }

pwerr:
  { LET t = list(3, t_int, 0)
  //prstate("In f_power")
  //abort(5556)
    asp := datav+stack+sp
    push(t)
    asp := datav+stack+sp
    error1("****", y, x)
    errdbg()
  }
}

AND f_neg() BE
{ LET t = pop()
  LET a = datav + t
  tstasp()
  rega := t
  IF h2!a=t_int DO
  { rega := list(3, t_int, -h3!a )
    push(rega)
    pc:=pc+1
    RETURN
  }
  IF h2!rega=t_real DO
  { rega := list(3, t_real, #- h3!a )
    push(rega)
    pc:=pc+1
    RETURN
  }
  rega := list(3, t_int, 0)
  error1("-", t, 0)
  errdbg()
}

AND f_eq() BE
{ LET t=rega
  rega := pop() // Left operand
  regb := pop() // Right operand
  rega := equal(rega, regb) -> truerv, falserv
  TEST errflag
  THEN { error1("EQ", t, regb)
	 errflag := FALSE
	 errdbg()
       }
  ELSE { push(rega); pc:=pc+1 }
}

AND f_ne() BE
{ LET t=rega
  tstasp()
  rega := pop() // Left operand
  regb := pop() // Right operand
  rega := equal(rega, regb) -> falserv, truerv
  TEST errflag
  THEN { error1("NE", t, regb)
	 rega := falserv
	 errflag := FALSE
	 errdbg()
       }
  ELSE { push(rega); pc:=pc+1 }
}

AND f_ls() BE
{ LET a, b = 0, 0
  tstasp()
  rega := pop() // Left operand
  regb := pop() // Right operand
  a := datav+rega
  b := datav+regb
  IF testnumbs2()=t_int DO
  { rega := h3!a < h3!b -> truerv, falserv
    push(rega)
    pc:=pc+1
    RETURN
  }
  IF testnumbs2()=t_real DO
  { rega := h3!a #< h3!b -> truerv, falserv
    push(rega); pc:=pc+1
    RETURN
  }
  error1("LS", rega, regb)
  rega := falserv
  errdbg()
}

AND f_le() BE
{ LET a, b = 0, 0
  tstasp()
  rega := pop() // Left operand
  regb := pop() // Right operand
  a := datav+rega
  b := datav+regb
  IF testnumbs2()=t_int DO
  { rega := h3!a <= h3!b -> truerv, falserv
    push(rega); pc:=pc+1
    RETURN
  }
  IF testnumbs2()=t_real DO
  { rega := h3!a #<= h3!b -> truerv, falserv
    push(rega); pc:=pc+1
    RETURN
  }
  error1("LE", rega, regb)
  rega := falserv
  errdbg()
}

AND f_ge() BE
{ LET a, b = 0, 0
  tstasp()
  rega := pop() // Left operand
  regb := pop() // Right operand
  a := datav+rega
  b := datav+regb
  IF testnumbs2()=t_int DO
  { rega := h3!a >= h3!b -> truerv, falserv
    push(rega); pc:=pc+1
    RETURN
  }
  IF testnumbs2()=t_real DO
  { rega := h3!a #>= h3!b -> truerv, falserv
    push(rega); pc:=pc+1
    RETURN
  }
  error1("GE", rega, regb)
  rega := falserv
  errdbg()
}

AND f_gr() BE
{ LET a, b = 0, 0
  tstasp()
  rega := pop() // Left operand
  regb := pop() // Right operand
  a := datav+rega
  b := datav+regb
  IF testnumbs2()=t_int DO
  { rega := h3!a > h3!b -> truerv, falserv
    push(rega); pc:=pc+1
    RETURN
  }
  IF testnumbs2()=t_real DO
  { rega := h3!a #> h3!b -> truerv, falserv
    push(rega); pc:=pc+1
    RETURN
  }
  error1("GR", rega, regb)
  rega := falserv
  errdbg()
}

AND f_jump() BE
{ pc := codev + pc!1
}

AND f_jumpF() BE
{ LET a = 0
  tstasp()
  rega := pop()
  a := datav + rega
  IF h2!a = t_false DO
  { pc := codev+pc!1
    RETURN
  }
  IF h2!a = t_true DO
  { pc := pc+2
    RETURN
  }
  error("NOT A TRUTHVALUE: ", rega, 0, 0)
  pc := pc+2
  edbg()
}

AND edbg() BE
{ restartc := pc+1
  tstasp()
  pc := codev + restartLoc
  rega := list(3, t_lvalue, nilrv)
  comdbg()
}

AND errdbg() BE
{ restartc := pc+1
  pc := codev+rvrestartLoc
  tstasp()
  rega := list(3, t_lvalue, rega)
  tstasp()
  //prstate("From errdbg")
  //abort(1008)
  comdbg()
}

AND errlvdbg() BE
{ rega := list(3, t_lvalue, rega)
  asp := datav+stack+sp
  prstate("In errlvdbg")
  tstasp()
  errokdbg()
}

AND errokdbg() BE
{ restartc := pc+1
  pc := codev+okrestartLoc
  tstasp()
  comdbg()
}

AND comdbg() BE
{ LET b = 0
  LET s = datav + stack // Abs address of the current stack
//writef("*ncomdbg: entered, sp=%n stack=%n datav=%n*n", sp, stack, datav)
//abort(2000)
  tstasp()
  h3!s := sp        // Rel address within the current stack
  regb := node(8)
  asp := datav+stack+sp
  tstasp()
  b := datav + regb // Abs address of regb
  h1!b := 8
  h2!b := t_stack
  h3!b := 0       // Location to hold the top of current stack
  h4!b := restartc
  h5!b := stack
  h6!b := env
  h7!b := rega
  stack := regb
  // errorlv is a Rel address to an Lvalue node, typically
  //         -> [3, Lvalue, rval]  rval is rel to datav
  //writef("comdbg: New stack created*n")
  //writef("comdbg: datav=%n errorlv=%n*n", datav, errorlv)
  //abort(5551)
  regb := h3!(datav+errorlv)
  b := datav + regb
  sp := 5
  asp := datav+stack+sp
  //prstate("In comdbg, pc=%n", pc)
  //abort(2001)
  errcount := errcount + 1
  IF errcount >= maxerr DO pc := codev+norestartLoc
  UNLESS h2!b = t_closure | h2!b=t_basicfn DO
  { UNLESS errcount >= maxerr DO
    writes("EXECUTION RESUMED*n*n")
    RETURN
  }
  TEST h2!b=t_closure
  THEN { sp := sp+1       // Call a closure
         asp := asp+1
         asp!1 := errorlv
	 rega := regb
	 oldc, pc := pc, h4!(datav+regb)
       }
  ELSE { //pc := pc-3       // Call a basic function
         rega := mklvnode(nilrv)
	 asp := datav+stack+sp
         push(rega)
	 //f_nil()
	 asp := datav+stack+sp
         rega := list(3, t_lvalue, asp!1)
         asp := datav+stack+sp
         asp!1 := rega
	 //f_formLvalue()
	 asp := datav+stack+sp
	 //prstate("Calling a basic function in comdbg")
	 //abort(3439)
	 (h3!(datav+regb))()
       }
  restartc := 0
}

AND okrestart() BE
{ // 
  rega := (datav+stack+sp)!1
prstate("In okrestart after setting rega from the top of the stack")
abort(4499)
  restart()
  sp := sp+1
  asp := datav+stack+sp
  tstasp()
  asp!1 := rega
}

AND f_rvrestart() BE
{ //writef("*nf_rvrestart:*n")
  //abort(1000)
  rega := asp!1
  restart()
  sp := sp+1
  asp := asp+1
  tstasp()
  asp!1 := h3!(datav+rega)
}

AND norestart() BE
{ writes("*nMAXIMUM NUMBER OF RUN-TIME ERRORS REACHED*n")
  terminate1()
}

AND f_apply() BE
{ // The top stack item is the RV of the fn, tuple or basic fn
  // being applied.
  // The next item down is the argument, an LV item.
  LET a, b = 0, 0
  tstasp()
  rega := asp!1 // Get the RV of the Closure, Tuple or basic fn to
  // apply from the top of the stack
  // rega should be the RV of a closure, a tuple or a basic function
  a := datav+rega
  // a should be the abs addr of a closure, tuple or basic fn.
  //prstate("f_apply: About to apply a %s", opstr(h2!a))
  //abort(1013)
  SWITCHON h2!a INTO
  { CASE t_closure:
      // a -> [4, Closure, env, pc]
      //      env is rel to datav
      //      pc  is rel to codev
      oldc := pc+1-codev // The return address as a rel addr
      pc := codev + h4!a // Abs addr of the function entry point
                         // whose first intruction will be [Save, n]
      // The next instruction should be 'save n'. This expects
      // save expects the h3 component of arga to be the environment
      // to use.
      RETURN
      
    CASE t_tuple:
      // a -> [4, Tuple, n, a1, a2,..., an]
      // asp!1 is the tuple and
      // asp!0 is the LV of the subscript.
      regb := asp!0     // Get the LV of the subscript
      b := datav + regb
      regb := h3!b      // Get the RV of the subscript
      b := datav + regb // The abd addr of the subscript
      
      // Chcck that the subscript is an int.
      UNLESS h2!b=t_int DO
      { error(0, rega, " APPLIED TO ", regb)
        prstate("Error in f_tuple")
        UNLESS h3!a=0 DO rega := h4!a
        errlvdbg()
        RETURN
      }
      { LET n = h3!b
        tstasp()
        TEST 1 <= n <= h3!a
        THEN { rega := a!(n+2) // Get the selected element
	       asp!1 := rega
	       pc:=pc+1
	       RETURN
	     }
        ELSE { error(0, rega, " APPLIED TO ", regb)
	       UNLESS h3!a=0 DO
	       TEST n >= 1
	       THEN rega := a!(h3!a+2) // Get the last element
	       ELSE rega := h4!a       // Get the first element
	       prstate("In f_apply")
      abort(6633)
	       errlvdbg()
	       RETURN
	     }
      }

    CASE t_basicfn:
      sp := sp-1     // Lose one stack item
      asp := datav + stack + sp
      tstasp()
      // The top item is now the basic fn's argument, an LV.
      //prstate("f_apply: about to call a basic fn*n")
      pc := pc+1  // Normally advance pc to just after the
                  // Apply instruction.
      (h3!a)()    // The argument of the basic fn will be on
                  // the top of the stack as an LV.
      RETURN

    DEFAULT:
      writef("*nERROR in f_apply: rega=%n asp!1=%n*n", rega, asp!1)
      abort(1012)
      tstasp()
      error("ATTEMPT TO APPLY ", rega," TO ", asp!1)
      edbg()
  }
}
 
AND f_save() BE
{ // Normally called to execute instruction [i_save,n] as the
  // first instruction of the body of a function. In this situation
  // the top two arguments in the caller's stack are the argument of
  // the call and the closure being called. Both values are LV nodes.
  // The save instruction is also used at the start of a block immediately
  // after executing blocklink. This treats the body of the block as if it
  // was the body of a function whose argument is the LV of an item that
  // specifies the initial values of the variables declared in the block.
  // As with a function call this argument is located as the second from
  // top element of the stack. The top element is not the LV of a closure
  // as require in a function call but holds the LV of the current
  // environment. The h3 ???### It is also used
  // immediately after reslink in the compilation of a valof expression.
  // Another context is in the compilation of a within declaration. The
  // only other time f_save is called is in f_setup, where the main
  // program is treated as the body of a function that fas no formal
  // parameters to declare. In all these cases f_save is called with
  // arga holding a node whose h3 element is the environment for the body
  // before the formal parameters, if any, are declared. The size of the
  // stack for the call is 6+pc!1 and the return address in in oldc.
  // Evaluation of the body completes by executing the i_return
  // instruction which rwturns control to the previous stack replacing
  // its top two element with the result of the function.


  LET n = pc!1 + 1  // The maximum number of anonymous results at any time
                // during the execution of the function.
  
  // oldc  is the return address relative to codev, pointing to i_finish
  //       if save is called from setup, it is an appropriate return
  //       address for the other contexts in which save is called.
  
  // rega  is a node whose h3 element is new environment before declaring
  //       any formal parameters if any. rega is either a closure or
  //       an lvalue node holding an environment.
  
  // stack is the previous stack node. If a closure is being called the
  //       top element is the LV of the closure being called and the 2nd
  //       element is the function argument. In other contexts these two
  //       stack items may hold dummy values that are not used by f_save.

  // asp!0  is the argument of the call.
  // asp!1  is typically the LV of a closure being called, but in other
  //       contexts may be a dummy values.
  //       function being called. asp!1 is not used by f_save.

  LET newstack = node(6+n) // Allocate a new stack with 6+n elements
                           // Uninitialised.
  LET s = datav + newstack // Abs address of the new stack
  //writef("f_save: stack=%n sp=%n*n", stack, sp)
  //writef("f_save: newstack=%n  s=%n = %n+%n*n", newstack, s, datav, newstack)
  //abort(6660)
  // Save the previous sp in the previous stack at position h3
  h3!(datav+stack) := sp // Save sp in the previous stack

  regb  := newstack     // To preserve the new stack is garbage
                        // collection happens.
                        // s is its abs address of the new stack
  h1!s  := 6+n          // The number of elements in the  new stack
  h2!s  := t_stack      // The node type
  h3!s  := 0            // Space to save this function's sp  
  h4!s  := oldc         // Save the return address
  h5!s  := stack        // Save thhe previous stack
  h6!s  := env          // Save the previous environment
  h7!s  := (datav+stack)!sp // Copy of the function argument taken from
                        // the second from top element of the  previous
			// stack. This will either be the function
			// argument or a dummy value.

  stack := newstack
  sp := 5               // Currently there is one possibly dummy value
                        // on the stack.
  asp := datav+stack+sp
  // rega points to a node whose third element is the new environment
  env   := h3!(datav+rega)
  pc    := pc+2
}

AND f_return() BE
{ rega := asp!1
  tstasp()
  restart()
  sp    := sp-1
  asp   := asp-1
  asp!1 := rega
  tstasp()
}

AND f_testEmpty() BE
{ tstasp()
  rega := pop()
  TEST h3!(datav+rega)=nilrv
  THEN pc := pc+1
  ELSE { error1("FUNCTION OF NO ARGUMENTS", rega, 0)
	 edbg()
       }
}

AND f_lose1() BE
{ tstasp()
  rega := pop()
  pc := pc+1
}

AND f_goto() BE
{ LET a = 0
  tstasp()
  rega := pop()
  a := datav + rega
  UNLESS h2!a=t_label DO
  { error("CANNOT GO TO ", rega, 0, 0)
    rega := dummyrv
    errdbg()
    RETURN
  }
  // rega -> [ 6, t_label, supb, c, s, e ]
  pc, env := h4!a, h6!a
  stack := node(h3!a)
  sp := 4
  h1!stack, h2!stack := h3!rega, stack
  rega := h5!rega
  h4!stack, h5!stack, h6!stack := h4!rega, h5!rega, h6!rega
}

AND f_update() BE
{ LET n = pc!1 // The number of variables to update
  tstasp()
  rega := pop() // The RV of the value being assigned
  tstasp()
  regb := pop() // The LV of the variable to be updated
  TEST n = 1
  THEN h3!(datav+regb) := rega
  ELSE { LET a = datav+rega
         UNLESS h2!a = t_tuple & h3!a = n DO
	 { error("CONFORMALITY ERROR IN ASSIGNMENT",0,0,0)
	   writes("THE VALUE OF THE RHS IS: ")
	   printa(rega, tupledepth)
	   newline()
	   writes("THE NUMBER OF VARIABLES ON THE LHS IS: ")
	   writen(n)
	   newline()
	   pc := pc + 1
	   rega := dummyrv
	   errdbg()
	   RETURN
	 }
	 { LET b = datav+regb // Abs addr of the LV of the
	                      // tuple of LVs to update
	   LET v = VEC 100
	   b := datav+h3!b    // Abs addr of tuple of LVs to update
//prstate("Inside update(%n)", n)
//	   writef("b=%n*n", b)
//abort(1000)
	   FOR i=3 TO n+2 DO v!i := h3!(datav+a!i)
	   FOR i=3 TO n+2 DO h3!(datav+b!i) := v!i
	 }
       }
  rega := dummyrv
  tstasp()
  push(rega)
  pc:=pc+2
}

// XPAL2D
MANIFEST { lfield=#o177777; ndist=24 }

LET error(ms1, db1, ms2, db2) BE
{ writes("*n*nRUN TIME ERROR: ")
  UNLESS ms1 = 0 DO writes(ms1)
  UNLESS db1 = 0 DO printa(db1, tupledepth)
  UNLESS ms2 = 0 DO writes(ms2)
  UNLESS db2 = 0 DO printa(db2, tupledepth)
  newline()
//  abort(8761)
}

AND error1(op, arg1, arg2) BE
{ // arg1 and arg2 are subscripts of datav
  writes("*n*nRUN TIME ERROR: ")
  writes(op)
  writes(" APPLIED TO ")
  //abort(1112)
  printa(arg1, tupledepth)
  UNLESS arg2=0 DO
  { writes(" AND ")
    printa(arg2, tupledepth)
  }
  newline()
  writef("error1: arg1=%n arg2=%n tupledepth=%n*n", arg1, arg2, tupledepth)
}

AND prvalue(x, depth) BE
{ // n is the nesting depth limit
  // x is zero or points to a runtime value [upb, op, ... ]
  LET p = datav+x
  tstasp()
  UNLESS 0 <= x < 1_000_000 DO
  { writef("%x8", x)
    RETURN
  }
  writef("%3i", x)
  //newline() //#######
  IF x=0 DO
  { //writef("Null")
    //newline() //#######
    RETURN
  }
  SWITCHON h2!p INTO
  { DEFAULT:
      writef("*nprvalue: DEFAULT: p=%n+%n=%n  -> [ %n %n ... ]*n",
              datav, x, p, h1!p, h2!p)
      RETURN
      
    CASE t_int:
      writef("[%n Int %n]", h1!p, h3!p)
      RETURN

    CASE t_real:
      writef("[%n Real %9.5f]", h1!p, h3!p)
      RETURN

    CASE t_true:
    CASE t_false:
    CASE t_nil:
    CASE t_nils:
    CASE t_dummy:
    CASE t_guess:
      writef("[%n %s]", h1!p, opstr(h2!p))
      RETURN

    CASE t_stack:
      //writef("[%n %s ..]", h1!p, opstr(h2!p))
    { LET s = datav+x
      LET p = h1!s //x=stack -> sp-datav-stack, h3!s
      LET ssp = s+p
      //writef("datav=%n x=%n stack=%n p=%n*n", datav, x, stack, p)
      writef("[%2i %s", s!0, opstr(s!1))
      IF p>10 DO p := 10
      FOR i = 2 TO p-1 DO
      { writef(" %n", s!i)
        //newline()
      }
      writef("]")
      //abort(4003)
      RETURN
    }


    CASE t_string:
      IF depth=0 DO
      { writef("-")
        RETURN
      }
      writef("[%2i %s ", h1!p, opstr(h2!p))
      prvalue(h3!p, depth-1)
      writef(" '%c']", h4!p)
      RETURN

    CASE t_env:
      IF depth=0 DO
      { writef("-")
        RETURN
      }
      writef("[%n %s %n", h1!p, opstr(h2!p), h3!p)
      writef(" '%s' ", strv+h4!p)
      prvalue(h5!p, depth-1)
      writef("]")
      RETURN

    CASE t_lvalue:
      writef("[%n %s ", h1!p, opstr(h2!p))
      IF depth=0 DO
      { writef("..]")
        RETURN
      }
      //writef("depth=%n ", depth)
      prvalue(h3!p, depth-1)
      writef("]")
      RETURN
      
    CASE t_tuple:
    { LET n = h3!p
      IF n = 0 DO
      { writes("[3 Tuple 0]")
        RETURN
      }
      IF depth=0 DO
      { writes("etc")
        RETURN
      }
      writef("[%n %s %n ", h1!p, opstr(h2!p), h3!p)
      FOR i = 3 TO n+1 DO
      { prvalue(p!i, depth-1)
        writes(" ")
      }
      prvalue(p!(n+2), depth-1)
      wrch(']')
      RETURN
    }

    CASE t_closure:
      writef("[%n Closure %n %n]", h1!p, h3!p, h4!p)
      RETURN

    CASE t_basicfn:
      writef("[%n Bfn %n]", h1!p, h3!p)
      RETURN

    CASE t_label:
      writes("Label")
      RETURN

    CASE t_jj:
      writef("[%n Jj %n %n %n]", h1!p, h4!p, h5!p, h6!p)
      RETURN
  }
}

AND printb(x) BE
{ LET p = datav+x
  //writef("*nprintb: x=%n*n", x)
  //abort(1011)
  IF x=0 RETURN
  tstasp()
  SWITCHON h2!p INTO
  { DEFAULT:
      writef("DEFAULT: printb x=%n[ %n %s %n ... ]*n",
              x, h1!p, opstr(h2!p), h3!p)
      RETURN
      
    CASE t_int:
      writen(h3!p)
      RETURN

    CASE t_real:
    { writef("%9.5f", h3!p)
      RETURN
    }

    CASE t_string:
      wrch(h4!p)
      printb(h3!p)

    CASE t_nils:
      RETURN

    CASE t_tuple:
    { LET n = h3!p
      IF n = 0 DO
      { writes("NIL")
        RETURN
      }
      //IF @ x > stackwarning DO
      //{ writes("( ETC )")
      //  RETURN
      //}
      wrch('(')
      FOR i = 3 TO n+1 DO
      { printb(p!i)
        writes(", ")
      }
      printb(p!(n+2))
      wrch(')')
      RETURN
    }

    CASE t_true:
      writes("TRUE")
      RETURN

    CASE t_false:
      writes("FALSE")
      RETURN

    CASE t_lvalue:
      printb(h3!p)
      RETURN

    CASE t_closure:
    CASE t_basicfn:
      writes("$FUNCTION$")
      RETURN

    CASE t_label:
      writes("$LABEL$")
      RETURN

    CASE t_jj:
      writes("$ENVIRONMENT$")
      RETURN

    CASE t_dummy:
      writes("$DUMMY$")
      RETURN
  }
}

AND printa(x, n) BE
{ LET p = datav+x
  IF x=0 RETURN
  tstasp()
  IF n <= 0 DO
  { writes(" ETC ")
    RETURN
  }
  
  SWITCHON h2!p INTO
  { DEFAULT:
      wrch(' ')
      printb(x)
      wrch(' ')
      RETURN

    CASE t_string:
    CASE t_nils:
      wrch('*'')
      printb(x)
      wrch('*'')
      RETURN

    CASE t_tuple:
    { LET m = h3!p
      IF m=0 DO
      { writes(" NIL ")
        RETURN
      }
      wrch('(')
      FOR i = 3 TO m+1 DO
      { printa(p!i, n-1)
        wrch(',')
      }
      printa(p!(m+2), n-1)
      wrch(')')
      RETURN
    }

    CASE t_lvalue:
      printa(h3!p, n)
      RETURN
  }
}

AND equal(ra,rb) = VALOF
{ LET btag = h2!(datav+rb)
  tstasp()
  SWITCHON btag INTO
  { CASE t_true:
    CASE t_false:
    CASE t_int:
    CASE t_real:
    CASE t_string:
    CASE t_nils:
      errflag := FALSE
      SWITCHON h2!(datav+ra) INTO
      { CASE t_true:
          IF btag=t_true RESULTIS TRUE
          RESULTIS FALSE
        CASE t_false:
	  IF btag=t_false RESULTIS TRUE
          RESULTIS FALSE
        CASE t_int:
	  IF btag=t_int & h3!(datav+ra)=h3!(datav+rb) RESULTIS TRUE
          RESULTIS FALSE
        CASE t_real:
	  IF btag=t_real & h3!(datav+ra)=h3!(datav+rb) RESULTIS TRUE
          RESULTIS FALSE
        CASE t_string:
	  IF btag=t_string & h4!(datav+ra)=h4!(datav+rb)
 	    RESULTIS equal(h3!(datav+ra),h3!(datav+rb))
          RESULTIS FALSE
        CASE t_nils:
          IF btag=t_nils RESULTIS TRUE
          RESULTIS FALSE
      }
  }
  errflag := TRUE
  RESULTIS FALSE
}

AND testnumbs2() = h2!(datav+rega)=t_int  & h2!(datav+regb)=t_int  -> t_int,
		   h2!(datav+rega)=t_real & h2!(datav+regb)=t_real -> t_real,
		   t_false

AND testbools2() = VALOF
{ LET a = datav + rega // Left operand
  LET b = datav + regb // right operand
  IF ( h2!a=t_true | h2!a=t_false ) &
     ( h2!b=t_true | h2!b=t_false ) RESULTIS TRUE
}

AND lvofname(n, p) = VALOF
{ // n is the position in strv of a variable.
  // p is the rel address of the current environment.
  //writef("lvofname: n=%n=%s p=%n*n", n, strv+n, p)
  h3!(datav+lookupcounter) := h3!(datav+lookupcounter) + 1
  tstasp()
  UNTIL p = 0 DO
  { LET q = datav + p
    //writef("lvofname: n=%s h4!q=%s p=%n*n", strv+n, strv+h4!q, p)
    //prstate("In lvoname")
    //writef("q -> [ %n %n %n %n %n ]*n", h1!q, (h2!q), h3!q, h4!q, h5!q)
    //abort(9766)
    IF h4!q = n DO
    { //writef("lvofname: Returning %n*n", h5!q)
      //abort(9767)
      RESULTIS h5!q
    }
    //writef("lvofname: No match at p=%n*n", p)
    p := h3!q
  }
  //writef("lvofname: Var %n not declared, strv=%n*n", n, strv)
  //abort(999)
  UNLESS n=nameres DO
    error("UNDECLARED NAME: ", 0, strv+n, 0)
  RESULTIS nilrv
}

AND nameoflv(l, p) = VALOF
{ UNTIL p=0 DO
  { IF h5!p=l RESULTIS h4!p
    p := h3!p
  }
  RESULTIS 0
}

AND restart() BE
{ LET p = datav + stack // Abs address of saved stack
  LET b = 0
  LET s = 0
//  writef("restart: stack=%n abs addr of stack=%n*n", stack, p)
//  abort(1022)
  tstasp()
  pc   := codev + h4!p    // Abs addr of return location.
  regb := h5!p            // Rel addr of the previous stack.
  b    := datav + regb    // Abs addr of the previous stack.
  env  := h6!p            // Recover the previous environment.
//  prstate("In restart, h4!p=%n", h4!p)
//  abort(1991)
  stack := node(h1!b) // Create a new stack of the right size
  s := datav + stack  // Abs addr of new stack
  sp := h3!b          // Previous stack pointer rel to previous stack
  asp := datav+stack+sp
//  prstate("In restart")
//  abort(1001)
  FOR i = 0 TO sp+1 DO s!i := b!i // Copy elements from old to new stack
  FOR i = sp+2 TO s!0 DO s!i := 0 // Pad with zeroes.
 //prstate("Called from restart")
 //abort(1000)
}

AND terminate() BE
{ listt := listt + 6 // CREATE EXTRA SPACE FOR FINAL DIAGNOSE
  tstasp()
  f_diagnose()
  terminate1()
}

AND terminate1() BE
{ //control(output, 2)
  //writen(h3!lookupcount)
  //writes(" LOOKUPS *T")
  writen(count)
  writes(" Instructions executed*n")
  //gcmark := gcmark >> 16
  //writen(gcmark)
  //writes(" GARBAGE COLLECTIONS*n")
  longjump(xpendlevel, xpend)
}

AND lastfn1(q) = VALOF
{ LET name, arg = 0, 0
  LET y, n = 0, 0
  IF h6!q=0 RESULTIS FALSE
  { y := h5!q
    n := h3!y
    TEST n>6
    THEN { name := y!(n-1)
           UNLESS name=nilrv DO
           { name := nameoflv(name, h6!q)
             IF name=0 DO name := "ANONYMOUS"
             arg := y!(n-2)
	   }
	 }
    ELSE name := nilrv
    q := y
    /////IF p=0 RESULTIS TRUE
    IF h6!q=0 RESULTIS FALSE
  } REPEATWHILE name=nilrv
  writes("AT THIS TIME, THE FUNCTION BEING EXECUTED IS: ")
  writes(name)
  writes("*nTHE ARGUMENT TO WHICH IT IS BEING APPLIED IS: ")
  printa(arg, tupledepth)
  newline()
  RESULTIS TRUE
}

AND writenode(n) BE
{ writen(n RSHIFT ndist)
  wrch('*T')
  writes(h4!rega)
  wrch('*T')
  printa(h5!rega, tupledepth)
  newline()
}

//>>> eject
// XPAL2E
MANIFEST { lfield=#o177777; mfield=#o77600000; gc1=#o200000 }

LET nextarea(n) BE
{ LET b = FALSE
  IF gcdbg DO writes("*n*nNEXTAREA RECLAIMATION PHASE*n")
  { UNLESS listp=listl DO h1!listp := listl - listp
    IF listl=listt DO
    { IF b DO
      { writes("*n*nRUN TIME SPACE EXHAUSTED*n")
        terminate()
      }
      mark()
      IF gcdbg DO writes("*nMARKLIST PREFORMED*n")
      listl, b := listv, TRUE
    }
    h1!listt := 0
    WHILE ( h1!listl & mfield ) = gcmark DO
      listl := listl + ( h1!listl & lfield )
    listp := listl
    h1!listt := gcmark
    UNTIL ( h1!listl & mfield ) = gcmark DO
      listl := listl + ( h1!listl & lfield )
    IF gcdbg DO
    { writes("*S*S")
      writen(listl-listp)
    }
  } REPEATWHILE listp+n >= listl
  IF gcdbg DO writes("*S*n")
  RETURN
}

AND marklist(x) BE
{
l:IF @ x > stackwarning DO
  { writes("*n*nMAXIMUM NODE DEPTH EXCEEDED*n")
    terminate()
  }
  IF x=0 RETURN
  IF ( h1!x & mfield ) = gcmark RETURN
  h1!x := h1!x & lfield | gcmark
  SWITCHON h2!x INTO
  { DEFAULT:
      writes("*n*nMARKLIST ERROR*n")
      writex(x); writes(" H1!X="); writex(h1!x)
      writes(" NODE TYPE IS "); writen(h2!x)
      writes("*S*n")
      RETURN

    CASE t_tuple:
      FOR i = 1 TO h3!x DO marklist(x!(i+2))
      RETURN

    CASE t_env:
      marklist(h5!x)
      x := (h3!x)
      GOTO l

    CASE t_stack:
      FOR i = 4 TO h3!x-1 DO marklist(x!i)
      RETURN

    CASE t_jj:
      marklist(h5!x)
      x := (h4!x)
      GOTO l

    CASE t_label:
      marklist(h6!x)
      x := (h5!x)
      GOTO l

    CASE t_lvalue:
    CASE t_closure:
    CASE t_string:
      x := (h3!x)
      GOTO l

    CASE t_int:
    CASE t_true:
    CASE t_false:
    CASE t_nil:
    CASE t_nils:
    CASE t_basicfn:
    CASE t_guess:
    CASE t_dummy:
    CASE t_real:
      RETURN
  }
}

AND mark() BE
{ gcmark := gcmark + gc1
  nset := FALSE
  IF ( gcmark & mfield ) = 0 DO
  { writes("*n*nMAXIMUM NUMBER OF ")
    writes("GARBAGE COLLECTIONS PERFORMED*n")
    terminate()
  }
  marklist(env)
  h3!stack := sp - stack
  marklist(stack)
  marklist(rega)
  marklist(regb)
  RETURN
}

AND list1(n, a, b, c, d, e, f) = VALOF
{ LET p = node(n)
  p := datav + p
  SWITCHON n INTO
  { DEFAULT:
    CASE 7: p!6 := f
    CASE 6: p!5 := e
    CASE 5: p!4 := d
    CASE 4: p!3 := c
    CASE 3: p!2 := b
    CASE 2: p!1 := a
    CASE 1: p!0 := n
  }
  FOR i = 7 TO n DO p!i := 0 // Pad with zeros 
  RESULTIS p
}

// XPAL2F
MANIFEST { lfield=#o177777 }

// Note sp is the abs addr of the secod from top stack item

LET pop() = VALOF
{ tstasp()
  sp   := sp-1
  asp   := asp-1
  RESULTIS asp!2
}

AND push(x) BE
{ tstasp()
  sp := sp+1
  asp := datav + stack + sp
  asp!1 := x
}

AND f_declvar() BE
{ // Prepend a new env node for name pc!1 giving it the value
  // popped from the stack.
  tstasp()
  env := list(5, t_env, env, pc!1, asp!1)
  sp := sp-1
  asp := datav+stack+sp
  tstasp()
  pc := pc + 2
}

AND f_declvars() BE
{ LET n = pc!1 // Number of variable names
  LET a, b = 0, 0
  tstasp()
  // the top of the stack is a tuple of initial values
  rega := pop()
  a := datav+rega
  // rega should be the LV of a tuple
  //prstate("declvars: n=%n", n)
  //abort(1000)
  rega := h3!a
  a := datav+rega
  //prstate("declvars: n=%n", n)
  //abort(1001)
  UNLESS h2!a=t_tuple & h3!a=n DO
  { error("CONFORMALITY ERROR IN DEFINITION", 0, 0, 0)
    nameerror(n,1)
    RETURN
  }
  //prstate("declvars: n=%n", n)
  //abort(1002)
  FOR i = 2 TO n+1 DO r_name(i, 1)
  pc := pc+2+n
}

AND f_initvar() BE
{ tstasp()
  asp := datav+stack+sp
  rega := pop()
  //prstate("f_initvar")
  //abort(4477)
  r_name(1, 7)
  pc := pc+2
}

AND f_initvars() BE
{ // pc -> [Initvars n, id1, id2,..., idn]
  // The top of the stack is expected to hold the values
  // of the variables being initialised by this command.
  LET n = pc!1 // The number of names to initialise.
  LET a = 0
//  prstate("On entry to f_initvars, n=%n", n)
  //abort(1177)
  //tstasp()
  //rega := pop()          // Get the LV of the tuple
  //prstate("After setting rega to LV of a tuple")
  //abort(2993)
  //a := datav + rega
  //rega := h3!a           // Get the tuple
  //a := datav + rega
  //UNLESS h2!a=t_tuple & h3!a=n DO
  //{ writef("f_initvars: h2!a=%s h3!a=%n n=%n*n", opstr(h2!a), h3!a, n)
  //  error("CONFORMALITY ERROR IN RECURSIVE DEFINITION",0,0,0)
  //  nameerror(n,4)
  //  RETURN
  //}
  FOR i = 2 TO n+1 DO
  { // pc!i holds the id of the variable to be updated
    // its value is popped from the stack.
    rega := pop()
    r_name(i, 5)
    //prstate("After updatins a variable in initvars")
  }
  pc := pc+2+n
}

AND r_name(i, p) BE
{ LET a = datav+rega
  LET b = 0
  //writef("r_name: i=%n p=%n*n", i, p)
  //abort(1005)
  tstasp()
  TEST p <= 3
  THEN { LET t = p=1 -> a!(i+1), list(3, t_lvalue, (p=2 -> rega, nilrv))
         asp := datav+stack+sp
         env := list(5, t_env, env, pc!i, t)
         asp := datav+stack+sp
       }
                   
  ELSE { regb := lvofname(pc!i, env)
         asp := datav+stack+sp
         IF regb=nilrv DO
	 { regb := list(3, t_lvalue, regb)
	   asp := datav+stack+sp
	 }
	 b := datav+regb // This is the LV of the variable to update.
	 //abort(1006)
         SWITCHON p INTO
         { CASE 4: // Update the value of a variable with the
	           // RV of a tuple element.
	           h3!b := h3!(datav+a!(i+1)); RETURN
           CASE 5: // Update the value of a variable with the
	           // value rega
	           h3!b := h3!(datav+rega);    RETURN
           CASE 6: // Update the value of a variable with the
	           // value rega
                   h3!b := nilrv;              RETURN
           CASE 7: // Update the value of a variable with the
	           // value in the third element of top value
		   // of the stack (an LV node).
                   h3!b := h3!(datav+asp!2);   RETURN
	 }
       }
}

AND nameerror(n,p) BE
{ writes("THE NAMES BEING DECLARED ARE:*n")
  FOR i = 2 TO n+1 DO
  { writes(pc!i)
    newline()
  }
  writes("THE VALUE(S) PROVIDED ARE: ")
  printa(rega, tupledepth)
  newline()
  TEST h2!rega=t_tuple
  THEN { LET m=n
         IF m>h3!rega DO m := h3!rega
         FOR i = 2 TO m+1 DO r_name(i,p)
         FOR i = m+2 TO n+1 DO r_name(i,p+2)
       }
  ELSE { r_name(2,p+1)
         FOR i = 3 TO n+1 DO r_name(i,p+2)
       }
  pc := pc+n+1
  edbg()
}

AND f_decllabel() BE
{ LET s = datav+stack
  tstasp()
//abort(1236)
  rega := list(6, t_stack, 4, h4!s, h5!s, h6!s)
  s := datav+stack
  rega := list(6, t_label, h1!s, pc!2, rega, env)
  rega := list(3, t_lvalue, rega)
  env := list(5, t_env, env, pc!1, rega)
  pc := pc + 3
}

AND f_setlabEs() BE
{ LET a = env
  tstasp()
  FOR i = 1 TO pc!1 DO
  { LET lvlab = h5!(datav+a)
    LET lab = h3!(datav+lvlab)
    h6!(datav+lab) := env
    a := h3!a
  }
  pc := pc + 2
}

AND f_blocklink() BE
{ // pc!1 it the return address label
  //      This is placed in oldc
  // Push nilrv onto the stack
  // rega is set to point to [ 3, Lvalue, env ]
  // The top of the stack is the block argument ???
  // Blocklink is always followed by a Save instruction
  tstasp()
  oldc := pc!1
  push(nilrv) // Push a dummy value into the stack
  // asp!0 is the argument giving the value(s) of the variables
  //       to be declared in the clock.
  rega := list(3, t_lvalue, env)
  // As required h3!rega is the current environment 
  pc := pc+2
}

AND f_reslink() BE
{ sp := sp+1 // Push a dummy argument into the stack
  asp := datav+stack+sp
  tstasp()
  asp!1 := list(3, t_lvalue, nilrv)
  tstasp()
  f_blocklink()
}

AND f_setup() BE
{ // Initialise the runtime state
  // env must contain just the builtin functions and variables.
  // pc -> [i_setup, n] // n is the number of user locations in the
  //                    // user's main stack.
  LET s = 0     // To hold the abs address of the user's main stack.
                // The outermost stack is empty and is only present
		// to allow f_save (called below) to save sp.
  //writef("f_setup: entered*n")
  //abort(6521)

  oldc  := finishLoc       // This is the return address pointing to
                           // the instruction i_finish. This is executed
			   // when the execution of the main program
			   // is complete.

  // Create the dummy outermost level empty stack
  stack := list(6,         // The size of the outermost stack item with
                           // no anonymous results
                t_stack,   // the type
		4,         // Space for saved sp
		0,         // Dummy return address, not used.
		           // stack returns to finishLoc
		0,         // The previous stack -- none
                0)         // No a previous environment. These two zeroes
		           // are dummy values for the top two elements
			   // of the calling stack. This function calls
			   // save which 

//  writef("f_setup: returned from list*n")
  sp := 4               // addr rel to stack of the arguments, if any, in the
                        // outermost stack. Note that stack is as stack
			// node relative to datav.
			// The top of the stack is asp!1 and holds the
			// dummy value zero. This will be copied
			// to the top element of a new stack created
			// by the call of f_save below.
  asp := datav+stack+sp
  tstasp()
//prstate("Before mklvnode(env) in setup, sp=%n", sp)
//abort(7001)
  rega := mklvnode(env) // Create a node whose h3 element holds the
                        // environment holding all the built in library
			// names.

//  prstate("Outermost stack created, sp=%n", sp)
//  writef("f_setup: returned from mklvnode*n")
//  abort(6522)

// Note that formal parameters are added to the environment using
// declvar or declarenames after executing save in the compilation
// of a function. An argument is pushed onto the stack before calling
// a function, a tuple or a basic function, but no argument is placed
// on the stack when running the main program.

//  writef("f_setup: rega=%n*n", rega)
  env := 0  // The outermost stack has an empty environment
  //prstate("Before save() in setup")
//  abort(9911)
  // Call the program as the main program with lvalue
  // of env as the argumemt
  //writef("f_setup: rega=%n*n", rega)
  //writef("f_setup: Before calling f_save*n")
  //prstate("Before f_save()")
  f_save()
  //writef("f_setup: rega=%n*n", rega)
  //writef("f_setup: After calling f_save*n")
  //prstate("After f_save()")
  //abort(6524)
  //writef("f_setup: returned from f_save*n")
//  writef("f_setup: rega=%n*n", rega)
  tstasp()
  rega := pop() // Throw away the dummy 'argument' because the body of
                // a program does not declare formal parameters.
  tstasp()
  //writef("f_setup: rega=%n*n", rega)
  //writef("f_setup: Feturned from f_save*n")
  //prstate("Just done f_save")
  //abort(6525)
}


// XPAL3 LAST MODIFIED ON FRIDAY, 12 JUNE 1970
// AT 5:37:38.68 BY R MABEE
//>>> filename "XPAL3"
//
//	***********
//	*         *
//	*  XPAL3  *
//	*         *
//	***********
//
//>>> GET "XPALHD"
//>>> eject
// XPAL3A
LET f_finish() BE
{ writes("*n*nEXECUTION FINISHED*n")
  tstasp()
  terminate1()
}

AND f_print() BE
{ //writef("f_print: entered*n")
  //abort(1000)
  tstasp()
  rega := pop()
  //prstate("f_print: after pop(), rega=%n", rega)
  printb(rega)
  rega := mklvnode(dummyrv)
  push(rega)
  tstasp()
  //nextlv11()
}

AND f_userpage() BE
{ rega := pop()
  tstasp()
  //control(output, -1)
  rega := dummyrv
  nextlv11()
  terminate()
}

AND f_stem() BE
{ rega := pop()
  tstasp()
  regb := h3!rega
  rega := nilsrv
  UNLESS h2!regb=t_string DO
  { error1("STEM", regb, 0)
    errlvdbg()
    RETURN
  }
  rega := list(4, t_string, rega, h4!regb )
  nextlv11()
}

AND f_stern() BE
{ rega := pop()
  rega := h3!rega
  UNLESS h2!rega=t_string DO
  { error1("STERN", rega, 0)
    rega := nilsrv
    errlvdbg()
    RETURN
  }
  rega := h3!rega
  nextlv11()
}

AND f_conc() BE
{ LET x, y = 0, 0
  LET v = VEC 512
  tstasp()
  rega := h3!(asp!1)
  UNLESS h2!rega=t_tuple & h3!rega=2 DO
concerr:{ error1("CONC", rega, 0)
	  rega := pop()
	  rega := nilsrv
	  errlvdbg()
	  RETURN
	}
  x, y := h2!(h3!(h4!rega)), h2!(h3!(h5!rega))
  UNLESS ( x=t_string | x=t_nils ) &
	 ( y=t_string | y=t_nils ) GOTO concerr
  regb, x := h3!(h4!rega), 1
  UNTIL h2!regb = t_nils DO
  { v!x := h4!regb
    regb := h3!regb
    x := x+1
  }
  IF x=1 DO
  { regb := h3!(h5!rega)
    rega := pop()
    rega := regb
    nextlv11()
    RETURN
  }
  regb := list(4, t_string, 0, 0)/////v!i)
  rega := regb
  FOR i = 2 TO x-1 DO
  { h3!rega := list(4, t_string, 0, v!i)
    rega := h3!rega
  }
  h3!rega := h3!(h5!(h3!(asp!1)))
  rega := pop()
  rega := regb
  nextlv11()
}

AND f_atom() BE
{ rega := pop()
  tstasp()
  SWITCHON h2!(h3!rega) INTO
  { CASE t_true:
    CASE t_false:
    CASE t_int:
    CASE t_real:
    CASE t_string:
    CASE t_nils:
      rega := truerv
      nextlv11()
      RETURN
  }
  rega := falserv
  nextlv11()}


AND f_null() BE
{ rega := pop()
  tstasp()
  rega := h2!(h3!(datav+rega))=t_tuple &
          h3!(h3!(datav+rega))=0 -> truerv, falserv
  nextlv11()
}

AND f_length() BE
{ rega := pop()
  tstasp()
  UNLESS h2!(h3!(datav+rega))=t_tuple DO
  { error1("ORDER", rega, 0)
    rega := list(3, t_int, 0)
    errlvdbg()
    RETURN
  }
  rega := list(3, t_int, h3!(h3!rega) )
  nextlv11()
}

AND f_istruthvalue() BE
{ rega := pop()
  tstasp()
  SWITCHON h2!(h3!(datav+rega)) INTO
  { CASE t_true:
    CASE t_false:
      rega := truerv
      nextlv11()
      RETURN
  }
  rega := falserv
  nextlv11()
}

AND f_isnumber() BE
{ rega := pop()
  tstasp()
  rega := h2!(h3!(datav+rega))=t_int -> truerv, falserv
  nextlv11()
}

AND f_isstring() BE
{ rega := pop()
  tstasp()
  SWITCHON h2!(h3!rega) INTO
  { CASE t_string:
    CASE t_nils:
      rega := truerv
      nextlv11()
      RETURN
  }
  rega := falserv
  nextlv11()
}

AND f_isfunction() BE
{ rega := pop()
  tstasp()
  SWITCHON h2!(h3!rega) INTO
  { CASE t_closure:
    CASE t_basicfn:
      rega := truerv
      nextlv11()
      RETURN
  }
  rega := falserv
  nextlv11()
}

AND f_isenvironment() BE
{ rega := pop()
  tstasp()
  rega := h2!(h3!rega)=t_jj -> truerv, falserv
  nextlv11()
}

AND f_islabel() BE
{ rega := pop()
  tstasp()
  rega := h2!(h3!rega)=t_label -> truerv, falserv
  nextlv11()
}

AND f_istuple() BE
{ rega := pop()
  tstasp()
  rega := h2!(h3!rega)=t_tuple -> truerv, falserv
  nextlv11()
}

AND f_isreal() BE
{ rega := pop()
  tstasp()
  rega := h2!(h3!rega)=t_real -> truerv, falserv
  nextlv11()
}

AND f_isdummy() BE
{ rega := pop()
  tstasp()
  rega := h2!(h3!rega)=t_dummy -> truerv, falserv
  nextlv11()
}

AND f_share() BE
{ rega := pop()
  tstasp()
  rega := h3!rega
  UNLESS h2!rega=t_tuple & h3!rega=2 DO
  { error1("SHARE", rega, 0)
    rega := falserv
    errlvdbg()
    RETURN
  }
  rega := h4!rega=h5!rega -> truerv, falserv
  nextlv11()
}

//>>> eject
// XPAL3B
MANIFEST {	nfield=#o67700000000; n1=#o100000000 }

LET f_ston() BE
{ rega := pop()
  tstasp()
  rega := h3!rega
  UNLESS h2!rega=t_string DO
  { error1("STOI", rega, 0)
    rega := list(3, t_int, 0)
    errlvdbg()
    RETURN
  }
  { LET regb = 0
    WHILE h2!rega=t_string DO
    { regb := regb*10 + h4!rega - '0'
      rega := h3!rega
    }
    rega := list(3, t_int, regb)
    nextlv11()
  }
}

AND f_cton() BE
{ rega := pop()
  tstasp()
  rega := h3!rega
  UNLESS h2!rega=t_string LOGAND h2!(h3!rega)=t_nils DO
  { error1("CTOI", rega, 0)
    rega := list(3, t_int, 0)
    errlvdbg()
    RETURN
  }
  rega := list(3, t_int, h4!rega )
  nextlv11()
}

AND f_ntoc() BE
{ rega := pop()
  tstasp()
  rega := h3!rega
  UNLESS h2!rega=t_int & h3!rega < 256 & h3!rega >= 0 DO
  { error1("ITOC", rega, 0)
    rega := nilsrv
    errlvdbg()
    RETURN
  }
  rega := list(4, t_string, nilsrv, h3!rega )
  nextlv11()
}

AND ntor() BE
{ rega := pop()
  tstasp()
  rega := h3!rega
  UNLESS h2!rega=t_int DO
  { error1("ITOR", rega, 0)
    rega := list(3, t_real, 0)
    errlvdbg()
    RETURN
  }
  rega := list(3, t_real, f_itor(h3!rega) )
  nextlv11()
}

AND f_rton() BE
{ rega := pop()
  tstasp()
  rega := h3!rega
  UNLESS h2!rega=t_real DO
  { error1("RTOI", rega, 0)
    rega := list(3, t_int, 0)
    errlvdbg()
    RETURN
  }
  rega := list(3, t_int, f_rtoi(h3!rega) )
  nextlv11()
}

AND rdchar() BE
{ // This reads the next character from the keyboard.
  // The characters of the current input line are in linev.
  // It normally returns a stringof length one, but when
  // eof is reached it returns a null string.
  // Re-implemented by MR 30/04/2024
  LET ch = rdch()
  tstasp()
  rega := pop() // Discard the argument of READCH
  TEST ch=endstreamch
  THEN rega := nilsrv
  ELSE rega := list(4, t_string, nilsrv, ch)
  nextlv11()
}

/*
{ rega := pop() // Discard the argument of READCHAR
  rega := list(2, t_nils)
  IF linep>linet DO
  { UNLESS dataflag GOTO enddata
    IF ch='#' DO
    TEST dataflag
    THEN { dataflag := FALSE
           nextlv11() // VALUE OF NILS INDICATES EOD
           RETURN
	 }
    ELSE
enddata: { writes("*nEND OF DATA FILE ENCOUNTERED*n*n")
           terminate1()
	 }
    linet := linev
    linet!0 := ch
    UNTIL ch="*n" DO
    { readch(input, lvch)
      linet := linet + 1
      linet!0 := ch
    }
    readch(input, lvch)
    linep := linev
  }
  rega := list(4, t_string, rega, linep!0 )
  linep := linep + 1
  nextlv11()
}
*/

AND f_table() BE
{ rega := pop();
  tstasp()
  rega := h3!rega
  UNLESS h2!rega = t_tuple & h3!rega = 2 DO
tablerr:{ error1("TABLE", rega, 0)
          rega := nilrv
          errlvdbg()
          RETURN
	}
  { LET n = h3!(h4!rega)
    UNLESS h2!n = t_int GOTO tablerr
    n := h3!n
    regb := h3!(h5!rega)
    rega := node(n+3)
    rega!0, rega!1, rega!2 := n+3, t_tuple, n
    FOR i = 3 TO n+2 DO
      rega!i := list(3, t_lvalue, regb)
    nextlv11()
  }
}

AND f_diagnose() BE
{ LET n, i = 0, 1000
  LET q = 0
  tstasp()
  //prstate("In f_diagnose")
  //abort(3437)
  rega := asp!1
  asp!1 := list(3, t_lvalue, dummyrv) // RETURN VALUE
                                     //REPLACES ARGUMENT ON STACK

  writef("Returning from diagnose*n")
  RETURN
  
  prstate("In f_diagnose")
  abort(3438)
  pc := pc+1
  IF h2!(h3!(datav+rega))=t_int DO i := h3!(h3!(datav+rega))
  errorlv := list(3, t_lvalue, list(3, t_basicfn, lastfn) )
  //IF nset DO // 2 SUCCESSIVE EXECUTIONS OF DIAGNOSE REQUIRE
             // AN INTERVENING MARKING PHASE
  //{ mark()
  //  listl := listv  // TAKE ADVANTAGE OF THE EXTRA
  //}
  // MARKING PHASE
  nset := TRUE
  //control(output, -1)
  writes("THE CURRENT ENVIRONMENT IS:*n*n")
  abort(9111)
  rega := env
  q := stack
  IF h4!(datav+stack)=restartc DO // TRUE IFF CALL IS FROM COMDBG
    lastfn1(0)                    // PEEL OFF TOP STACK NODE
l:writes("*TVARIABLE*TRVALUE*n*n")
  WHILE h4!(datav+rega) ~= 0 DO
  { LET m = h1!(datav+rega)// & nfield
    TEST m ~= 0
    THEN { writenode(m)
           writes("ETC*n")
           BREAK
	 }
    ELSE { n := n+n1
           //h1!rega := h1!rega | n
           writenode(n)
           rega := h3!(datav+rega)
	 }
  }
  i := i-1
  rega := h6!(datav+q)
  //control(output, 3)
  ///UNLESS lastfn1(1) DO
fini:
  { //control(output, -1)
    RETURN
  }
  IF i <= 0 GOTO fini
  writes("*n*nTHE ENVIRONMENT IN WHICH ")
  writes("THE ABOVE APPLICATION TAKES PLACE IS:*n*n")
  GOTO l
}

AND lastfn() BE
{ LET q = 0
  asp!1 := list(3, t_lvalue, dummyrv) // RETURN VALUE
  tstasp()
  // REPLACES ARGUMENT ON STACK
  pc := pc+1
  //control(output, 2)
  q := stack
  IF h4!stack=restartc DO // TRUE IFF CALL IS FROM COMDBG
    lastfn1(0) // PEEL OFF TOP STACK NODE
  UNLESS lastfn1(1) DO
    writes("ERROR OCCURRED IN OUTER LEVEL OF PROGRAM*n")
  //control(output, 3)
}

AND lookupine() BE
{ // The top of the stack is assumed to be the LV of a 2-tuple
  // ( PAL string os a variable name, a JJ node)
  tstasp()
  rega := pop()
  rega := h3!rega
  UNLESS h2!rega = t_tuple & h3!rega = 2 DO
lerr: { error1("LOOKUPINE", rega, 0)
        rega := nilrv
        errlvdbg()
        RETURN
      }
  { LET x, i, l = h3!(h5!rega), 1, namechain
    LET vp = VEC 10
    LET v = VEC 40
    regb := h3!(h4!rega)
    UNLESS h2!regb=t_string & h2!x=t_jj GOTO lerr
    WHILE h2!regb=t_string DO
    { v!i := h4!regb
      regb := h3!regb
      i := i+1
    }
    v!0 := i-1
    packstring(v, vp)
    i := ( i-1 ) /bytesperword + 1
    UNTIL l=0 DO
    { LET v = l!1
      IF vp!0=v!0 DO
      { IF i=1 BREAK
        IF vp!1=v!1 DO
        { IF i=2 BREAK
          IF vp!2=v!2 DO
     {	IF i=3 BREAK
                IF vp!3=v!3 DO
		{ IF i=4 BREAK
                  IF vp!4=v!4 DO
		  { IF i=5 BREAK
		  }
		}
		l := l!0
              }
	    }
	TEST l=0
	THEN i := vp
	ELSE i := l!1 
	rega := lvofname(i, h5!x)
	TEST rega=nilrv
	THEN errlvdbg()
	ELSE { push(rega); pc:=pc+1 } }}}}

AND f_saveenv() BE
{ rega := pop()
  tstasp()
  rega := list(5, t_jj, h4!stack, h5!stack, h6!stack )
  nextlv11()
}

AND garbcollect() BE
{ // This just copies all accessible data in datav into
  // a new selfexpading array. All accessible data is
  // referenced directly or indirectly from the following
  // roots which are all relative to datav:
  //     datav!1 to datav!10
  //     stack
  //     env
  //     rega
  //     regb
  // Whenever a node is copied its usb is to minus the
  // new location.
  
  tstasp()
  prevdatav := datav // save the previous datav
  
  datavupb  := 0  // Reset palsxv
  datav     := 0

  FOR i = 1 TO 10 DO // Allocate elements 1 to 10
    sxvpush(palsxv, 0)
  FOR i = 1 TO 10 DO datav!i := gc(datav!i)
  stack := gc(stack)
  env   := gc(env)
  rega  := gc(rega)
  regb  := gc(regb)
}

AND gc(p) = VALOF
{ // p is relative to datav
  LET upb, op, newnode = 0, 0, 0
  UNLESS p RESULTIS 0
  p := prevdatav+p        // Convert p to absolute address in datav
  upb := h1!p
  IF upb<0 RESULTIS -upb  // Already copied, return the new location
  op := h2!p
  newnode := node(upb)    // Allocate the new node
  h1!p := -newnode
  
  h1!newnode := upb
  h2!newnode := op
  
  SWITCHON op INTO // All possible runtime node types 
  { DEFAULT:
      writef("*nSystem error in gc: op=%s*n", op)
      abort(999)
      RESULTIS newnode

    CASE t_dummy:        // [ 2, t_dummy ]
    CASE t_false:        // [ 2, t_false ]
    CASE t_guess:        // [ 2, t_guess ]
    CASE t_nil:          // [ 2, t_nil ]
    CASE t_nils:         // [ 2, t_nils ]
    CASE t_true:         // [ 2, t_true ]
      RESULTIS newnode

    CASE t_basicfn:      // [ 3, t_basicfn, val ] 
    CASE t_int:          // [ 3, t_int,     val ] 
    CASE t_real:         // [ 3, t_real,    val ] 
      h3!newnode := h3!p      //val
      RESULTIS newnode
      
    CASE t_closure:      // [ 4, t_closure, e, c ]
      h3!newnode := gc(h3!p)   // e
      h4!newnode := h4!p       // c
      RESULTIS newnode
    
    CASE t_env:          // [ 5, t_env, link, varname, val ]
      h3!newnode := gc(h3!p)    // link
      h4!newnode := h4!p        // varname
      h5!newnode := gc(h5!p)    // val
      RESULTIS newnode
    
    CASE t_jj:           // [ 6, t_jj, c, s, e ] 
      h3!newnode := h3!p        // c
      h4!newnode := gc(h4!p)    // s
      h5!newnode := gc(h5!p)    // e
      RESULTIS newnode
    
    CASE t_label:        // [ 6, t_label, relsp, c, s, e ] 
      h3!newnode := h3!p        // relsp
      h4!newnode := h4!p        // c
      h5!newnode := gc(h5!p)    // s
      h6!newnode := gc(h6!p)    // e
      RESULTIS newnode

    CASE t_lvalue:       // [ 3, t_lvalue, val ] 
      h3!newnode := gc(h3!p)   // val
      RESULTIS newnode


    CASE t_string:       // [ 4, t_string, link, ch ]
      h3!newnode := gc(h3!p)    // link
      h4!newnode := h4!p        // ch
      RESULTIS newnode

    CASE t_stack:        // [ upb, t_stack, relsp, c, s, e, <locals> ]
      h3!newnode := h3!p                             // relsp
      h4!newnode := h4!p                             // c
      FOR i = 4 TO h3!p+1    DO newnode!i := gc(p!i) // s, e, <locals>
      FOR i = h3!p+2 TO upb  DO newnode!i := 0       // Clear others
      RESULTIS newnode
    
    CASE t_tuple:        // [ upb, t-tuple, n, <elements> ]
      h3!newnode := h3!p                          // n
      FOR i = 3 TO h3!p+2 DO newnode!i := gc(p!i) // <elements>
      RESULTIS newnode

  }
  RESULTIS 0  
}


