// This is the BCPL compiler front end used with several
// codegenerators including those for 32- and 64-bit Cintcode.

// Implemented by Martin Richards (c) 10 Dec 2018

/* Change history

29/09/2021
This version of the compiler is now the one provided in the BCPL
distribution bcpl.tgz, even though the new pattern matching 
features are not yet fully implemented. The tree structure
for match lists is currently being changed to use a chain in
match item nodes rather than using seq nodes. Much work is still
to be done. bcpl.tgz will next be updated when the compiler works
well enough to compileand run the pattern matching version of
coins.b in BCPL/bcplprogs/mdemos/coins.b.

30/08/2021
Started to implement the MCPL style pattern matching feature including
pattern matching in function and routine definitions, MATCH commands
and expressions, and the NEXT and EXIT commands. Labels are now
allocated using genlab.

01/10/2020
Added option T16 to allow the frontend to be used with codegenerators
for 16 bit machines such as the Z80.

11/12/2018
Major addition to standard BCPL, including operators such as <>, #:=
and the op:= operators with corresponding additions to the FLT
feature. For example x, y +:= 1, 1.0 is allowed and automatically
compiled as { x := x+1; y := y#+1.0 }.  The manual has not yet been
updated to reflect the change. This change is intended to eliminate
the need for the xbcpl compiler which is now out of date.

26/11/2018
### I have decided bot to make this change ###
I am about to make some modifications and extensions to standard BCPL
with the aim of removing the need to keep the xbcpl compiler. The
first change is the removal of old redundant system words that were
require in 1967 when BCPL was first implemented. At the time character
sets were very limited but since about 1972 most BCPL systems use the
ASCII character set. The system words that have been removed are: EQ
GE GR LV LE LS LOGOR LOGAND LSHIFT NE OR RSHIFT and RV. These were
removed by commenting out the relevant calls of dsw in
declsyswords. It is easy to correct any program that still uses any of
them, alternatively, undo the comments in declsyswords. REM and NEQV
are still synonyms of MOD and XOR.

06/11/2018
There are new OCODE operators SELLD and SELST to improve the
efficiency of the OF operator. The compiler option NOSELST causes the
compiler to generate OCODE that does no use these operators. Although
SIAL now has the selld and selst instructions not all SIAL
codegenerators have been suitably modified.

27/08/2018
Systematically replaced s_fnumber by s_fnum.

18/08/2018
Define sourcenamev only in bcplfe.b and not in bcplfecg.h.

02/03/2018
Added the FLT feature to make floating point operations more
convenient.  It allows some variables to be given the FLT tag and use
these variables to cause some integer operators to be replaced by
their floating point versions. See bcplman.pdf for details.

08/10/2014
Slightly modified the translation of switchon commands to use
the OCODE operators RES and Rstack. This change was made to simplify
the optimisation of Sial.

06/08/2014
Added floating point numbers and the operators FIX FLOAT #ABS #* #/
#MOD #+ #- #= #~= #< #> #<= #>=. This version of the compiler
generates Cintcode that runs under the standard Cintcode interpreter
(which was modified for xbcpl in 2010), and it is compatible with
procode.  bcplcgsial.b, sial-sasm.b have been modified appropriately,
but sial-386.b and sial-arm.b will need modification. This version
allows 32-bit manifest floating point constants when running under
32-bit Cintcode and 64-bit floating point when running unser 64-bit
Cintcode.  It uses the standard IEEE floating point formats.

19/04/2014
Systematically changed mult to mul, plus to add and minus to sub.

30/04/2014
Do not increment line number on *p for compatiblity with emacs.

05/02/2014
Allow // comments in multi-line string constants.

08/01/2014
Added $~tag ... $>tag conditional compilation feature to allow
code to be included if a conditional tag is not set..

03/12/2013
Added the compiler option OPT/K to set conditional compilation
options. The argument is a string of option names consisting of
letters, digits, underlines and dots separated by plus signs or
indeed any characters not allowed in option names.

13/05/2013
This is a version of the BCPL compiler front end is used by many
variants of the compiler including those that generate 32- or
64-cintcode.  It is designed to run on both 32- and 64-bit
systems. The options t32 and t64 specify the bit length of the BCPL
word in the target system. The default is the same as that of the
current system.  On 64-bit systems numerical constants are compiles to
full precision, but on 32-bit systems they are truncated to 32 bits
then sign extended to 64 bits. 64-bit Cintcode has one new instruction
(MW) that sets a new 64 bit register called MW that is used to modify
the operand of the next W type instruction (KW, LLPW, LW, LPW, SPW,
APW and AW). The MW instruction has a 32 bit operand that is placed in
the senior half of the MW register. The junior half is cleared.  MW is
added to the operand of any W type instruction and is immediately
cleared after use.

18/01/2011
If VER and XREF are both specified, verification output is opened
using findappend. 

05/01/2011
Modified g/bcplfecg.h to be usable by bcpl.b, xbcpl.b and procode.b

05/10/2010
Modified the treatment of EQCASES to preserve the case of the first
occurrence of each name for use in eg cross reference listings.
Removed SKIP reserved word, {} can be used instead.

20/10/09
Corrected bug in performget relating to sourcefile names and
numbers.

10/07/2009
Stopped '.' terminating GET streams so that GET streams can contain
several sections separated by dots. BEWARE: this is an incompatible
change, since the first section of a GET stream has in the past been
used as a header.
Re-organised the compiler into g/bcplfecg.h, bcplfe.b and bcplcgcin.b,
and reallocating most of the compiler globals.

08/05/2009
Increased the default treesize to 200000.

03/07/2007
Modified the treatment of *# escapes in string and character constants
to allow both UTF8 and GB2312 encoding. Added compiler options UTF8
and GB2312 to set the default encoding. *#U and *#G in a string and
character constant temporarily set the encoding to UTF8 and GB2312,
respectively, overriding the default setting. In GB2312 mode, *#dddd
contains up to 4 decimal digits. See the BCPL manual.

27/06/2007
Added the Unicode escape sequences *#hhhh and *##hhhhhhhh to string
and character constants. Within string they are converted to the
corresponding UTF8 sequence of bytes and within a character constant
they yield the corresponding Unicode integer. See the last few tests
in com/cmpltest.b

27/07/2006
Changed the implementation of the GET directive to make it system
independent. Performget now obtains the headers environment variable
from the root node (rootnode!rtn_hdrsvar) this is normally either
"BCPLHDRS" or "POSHDRS". If the header file does not end in .h or .b,
.h is appended. The search order is as follows:

(1) The current directory.
(2) The directories specified by the headers environment variable,
    if set.
(3) The subdirectory g/ of the root specified by the environment
    variable rootnode!rtn_rootvar, if set.

05/04/2006
Correcteded a bug in trans concerning the tranlation of SKIP.

18/01/2006
Based on Dave Lewis's suggestion, in outputsection(), added:
   IF objline1%0 DO writef("%s*n", objline1)
where objline1 is the first line of file objline1 if it can be found
in the current directory or in the HDRS directory. This will typically
put a line such as:
#!/usr/local/bin/cintsys -c
as the first line of the compiled object module. This line is ignored
by the CLI but may be useful under Linux. If objline1 cannot be found
no such line is inserted at the start of the object module.

30/8/2005
Defined the function default_hdrs() near the start to allow easy change
from cintsys to cintpos versions of the compiler.
 
22/6/2005
Added the command SKIP and let empty blocks be equivalent to
SKIP. Empty section brackets are now also allowed after MANIFEST,
STATIC and GLOBAL.  These changes make program development marginally
easier.

17/6/2004
Made GET first look in the current directory.
Added bcpl command option HDRS to allow the environment variable
specifying the headers directory to be changed. The default is
BCPLHDRS.

23/4/2004
Updated the standard BCPL compiler with all the Cintpos extensions
including cross referencing and 11 character names.
Make GET directives use the BCPLHDRS environment variable.

11/6/2002
Changed square brackets to mean subscription with same precedence
and function calls.

18/3/2002
Use BCPLHDRS and BCPLPATH in GET directives.

14/1/2002
Added XREF option to output name information during compilation.

11/7/2001
Added language extensions for the Ford dialect of BCPL.
i.e. modified performget
     added SLCT and OF (also ::)
     added || comments
     treesize set to 100000

15/1/2001
Complain if global number is larger than 65535.

10/8/2000
Change the maximum number of error messages from 30 to 10.

14/12/1999
Made / * ... * /  comments nest.
Allow the constants in MANIFEST, STATIC and GLOBAL declarations 
to be optional. If absent the value is one greater than the
previous value. Unless specified the first value is zero, so
MANIFEST { a; b=10; c } declares a, b and c to be 0, 10 and 11,
respectively.

09/06/1999
Made changes to buffer OCODE in memory. When bcpl is called
without the TO argument it writes numeric ocode to the file ocode.
Lex treats CR (13) correctly to improve convenience when running
under Windows and WindowsCE.

26/02/1999
Added BIN option to the compiler to generate a binary (rather than
hex) hunk format for the compiled code. This is primarily for the
Windows CE version of the cintcode system where compactness is
particularly important. There is a related change to loadseg in
cintmain.c

17/11/1998
Changed the workspacesize to 40000 and added the SIZE keyword
to allow the user to specify this size.

9/11/1998
Made GET directives search the current working directory
then directories given by the shell variable BCPLPATH, if set.
It uses the BLIB function pathfindinput.

15/12/1996
Correct a bug in cellwithname.

16/08/1996
Added one line to readnumber to allow underscores in numbers after 
the first digit.

07/06/1996
Implemented the method application operator for object oriented
programming in BCPL. E # (E1, E2,..., En) is equivalent to
((!E1)!E)(E1, E2,..., En)

24/12/1995
Improved the efficiency of cellwithname in TRN (using the hash chain
link in name node).
Improved the efficiency of outputsection in CG by introducing
wrhex2 and wrword_at.

24/07/1995
Removed bug in atbinfo, define addinfo_b change some global numbers.
Implement constant folding in TRN.

13/07/1995
Allowed { and } to represent untagged section brackets.

22/06/1993
Reverse order in SWB and have a minimum of 7 cases
to allow faster interpreter.

02/06/1993
Changed code for SWB to use heap-like binary tree.

19/05/1993
Put in code to compile BTC and XPBYT instructions.

23/04/1993
Allowed the codegenerator to compiler the S instruction.

21/12/1992
Cured bug in compilation of (b -> f, g)(1,2,3)

24/11/1992 
Cured bug in compilation of a, b := s%0 > 0, s%1 = '!'

23/07/1992:
Renamed nextlab as newlab, load as loadval in the CG.
Put back simpler hashing function in lookupword.
Removed rdargs fudge.
Removed S2 compiler option.
Cured bug concerning the closing of gostream when equal to stdout.
*/

SECTION "BCPL"

GET "libhdr"
GET "bcplfecg"
 
LET default_hdrs() = VALOF // Changed MR 12/07/09
{ LET hdrs = rootnode!rtn_hdrsvar // Typically "BCPLHDRS" or "POSHDRS" or 0
  IF hdrs RESULTIS hdrs
  // The following is only executed if cintsys or cintsys64 fails to set
  // the hdrs field in the rootnode.
  // Note that tcb=0 when running under cintsys.
  TEST t64
  THEN RESULTIS tcb -> "POS64HDRS", "BCPL64HDRS"
  ELSE RESULTIS tcb -> "POSHDRS",   "BCPLHDRS"
}

GLOBAL {
// Globals used in LEX
chbuf:feg
decval; fltval; exponent; getstreams; charv
hdrs  // MR 10/7/04

workvec
readdecimal; readnumber; rdstrch
token; wordnode; ch
rdtag; performget
lex; dsw; declsyswords; nlpending
lookupword; eqlookupword; rch
sourcenamev; sourcefileno; sourcenamevupb
skiptag; wrchbuf; chcount; lineno
nulltag; rec_p; rec_l
 
// Globals used in SYN
rdblockbody;  rdsect
rdmatchlist

rdplist  // Read a list of zero or more pattern terms separated with commas

rnamelist; rname
rdef; rcom; rncom; rbcom
rdcdefs
formtree; synerr//; opname       declared in bcplfecg.h
rnexplist; rexplist; rdseq
mk1; mk2; mk3
mk4; mk5; mk6; mk7
mk3list               // Free list of nodes of size 3
unmk3                 // Return a node of size 3
newvec
rnexp; rexp; rbexp

calib

lastsynglobal // Used to check for global overlap with trng
}
 
 
MANIFEST {
c_backspace =  8
c_tab       =  9
c_newline   = 10
c_newpage   = 12
c_return    = 13
c_escape    = 27
c_space     = 32
}

LET chkglobals() = VALOF
{ // Return TRUE if there is a global variable allocation problem
  
  LET lastintgn = @lastintglobal - @glob0
  LET lastsyngn = @lastsynglobal - @glob0
  LET lasttrngn = @lasttrnglobal - @glob0
  
//  IF debug>0 DO
//  { writef("lastintgn=%i3   feg=%i3*n", lastintgn,  feg)
//    writef("lastsyngn=%i3  trng=%i3*n", lastsyngn, trng)
//    writef("lasttrngn=%i3   cgg=%i3*n", lasttrngn,  cgg)
//  }

  IF lastintgn>=feg DO
  { writef("SYSTEM ERROR: lastintgn=%i3   feg=%i3*n", lastintgn,  feg)
    RESULTIS TRUE
  }
 
  IF lastsyngn>=trng DO
  { writef("SYSTEM ERROR: lastsyngn=%i3  trng=%i3*n", lastsyngn, trng)
    RESULTIS TRUE
  }
 
  IF lasttrngn>=cgg DO
  { writef("SYSTEM ERROR: lasttrngn=%i3   cgg=%i3*n", lasttrngn,  cgg)
    RESULTIS TRUE
  }
 
  RESULTIS FALSE
}
  
LET floatingchk() BE
{ TEST t64
  THEN UNLESS ON64 DO
    synerr("64-bit floating point constants cannot be compiled using 32 bit BCPL")
  ELSE IF ON64 DO
    synerr("32-bit floating point constants cannot be compiled using 64 bit BCPL")
}
 
LET start() = VALOF
{ LET treesize = 0
  AND argv = VEC 50
  AND argform = "FROM/A,TO/K,VER/K,SIZE/K/N,TREE/S,NONAMES/S,*
                *D1/S,D2/S,OENDER/S,EQCASES/S,BIN/S,XREF/S,GDEFS/S,HDRS/K,*
                *GB2312/S,UTF8/S,SAVESIZE/K/N,HARD/S,*
                *T16/S,T32/S,T64/S,OPT/K,TREE2/S,NOSELST/S,MAP/K,LIST/K"
  // T16, t32, MAP and LIST added by MR 01/10/2020
  LET stdout = output()
  LET objline1vec = VEC 256/bytesperword+1
  LET optstringvec = VEC 256/bytesperword+1

  debug := 0

  IF chkglobals() DO
  { // There was a problem with the global variable declarations
    result2 := 0
    RESULTIS 20
  }

  UNLESS s_opmax < 256 DO
  { writef("SYSTEM ERROR: s_opmax=%n is too large*n")
    abort(999)
    result2 := 0
    RESULTIS 20
  }
  
  objline1 := objline1vec
  objline1%0 := 0
  optstring := optstringvec
  optstring%0 := 0
  errmax   := 10
  errcount := 0
  fin_p, fin_l := level(), fin

  flt0  := sys(Sys_flt, fl_mk, 0, 0)
  flt1  := sys(Sys_flt, fl_mk, 1, 0)
  flt10 := sys(Sys_flt, fl_mk, 10, 0)

  treevec      := 0
  obuf         := 0
  sourcestream := 0
  ocodeout     := 0
  gostream     := 0
  getstreams   := 0

  sysprint := stdout
  selectoutput(sysprint)
 
   // Allocate vector for source file names
  sourcenamevupb := 1000
  sourcenamev := getvec(sourcenamevupb)
  UNLESS sourcenamev DO
  { writef("Insufficient space available*n")
    abort(999)
    errcount := 1
    GOTO fin
  }
  sourcefileno := 0
  FOR i = 0 TO sourcenamevupb-1 DO sourcenamev!i := 0  // Corrected 19/08/2018   
 
  // Set the current system wordlength flag
  // ON64 is defined in libhdr.h. Previously called c64.

  // Set the target system wordlength flag
  t64 := ON64 // Set the default target word length

  IF rdargs(argform, argv, 50)=0 DO { writes("Bad arguments*n")
                                      errcount := 1
                                      GOTO fin
                                    }

  bigender := (!"AAAAAAA" & 255) ~= 7    // =TRUE if on a bigender m/c

  t16, t32, t64, wordbytelen, wordbitlen := FALSE, TRUE, FALSE, 4, 32  // T32 is the default setting
  
  IF argv!18 DO                           // T16/S
  { t16, t32, t64, wordbytelen, wordbitlen :=  TRUE, FALSE, TRUE, 2, 16
    IF argv!19 | argv!20 DO
    { writef("Only one of T16, T32 or T64 is allowed*n")
      RESULTIS 0
    }
  }
  IF argv!19 DO                           // T32/S
  { t16, t32, t64, wordbytelen, wordbitlen := FALSE, TRUE, FALSE, 4, 32
    IF argv!20 DO
    { writef("Only one of T16, T32 or T64 is allowed*n")
      RESULTIS 0
    }
  }
  IF argv!20 DO                           // T64/S
  { t16, t32, t64, wordbytelen, wordbitlen :=  FALSE, FALSE, TRUE, 8, 64
  }

  writef("*n%n bit BCPL (10 Oct 2021) with pattern matching, %n bit target*n",
          bitsperword, wordbitlen)

  IF argv!21 DO                           // OPT/K
  { LET s = argv!20
    FOR i = 0 TO s%0 DO optstring%i := s%i
//writef("*nopt=%s*n", optstring)
  }
  treesize := 200_000
  IF argv!3 DO treesize := !argv!3        // SIZE/K/N
  IF treesize<10_000 DO treesize := 10_000
  obufsize := treesize/2

  prtree        := argv!4                 // TREE/S
  savespacesize := 3

  // Code generator options 
  naming := TRUE

  // This must be done after T64 is properly set
  hdrs := default_hdrs()                  // Set the default HDRS

  IF argv!5 DO naming   := FALSE          // NONAMES/S
  IF argv!6 DO debug    := debug+1        // D1/S
  IF argv!7 DO debug    := debug+2        // D2/S
  IF argv!8 DO bigender := ~bigender      // OENDER/S
  eqcases  := argv!9                      // EQCASES/S
  bining   := argv!10                     // BIN/S (binary hunk)
  xrefing  := argv!11                     // XREF/S
  gdefsing := argv!12                     // GDEFS/S
  IF argv!13 DO hdrs := argv!13           // HDRS/K
  defaultencoding := UTF8
  IF argv!14 DO defaultencoding := GB2312 // GB2312/S
  IF argv!15 DO defaultencoding := UTF8   // UTF8/S
  encoding := defaultencoding
  IF argv!16 DO savespacesize := !(argv!16) // SAVESIZE/K/N
  hard := argv!17                         // HARD/S
                                          // t16/S is 18
                                          // t32/S is 19
                                          // t64/S is 20
                                          // OPT   is 21
  prtree2 := argv!22                      // TREE2/S -- print tree after trans
                                          // to test the FLT feature.

  noselst := argv!23                      // NOSELST/S
                                          // Do not generate SELLD or
                                          // SELST Ocode instructions.

  mapfilename  := argv!24                  // MAP/K  For the Z80 codegenerator
  listfilename := argv!25                  // LIST/K and possibly others

  // Added 5/10/2010
  IF eqcases DO lookupword := eqlookupword

  IF debug DO chkglobals()
  
//writef("BCPL hdrs = %s*n", hdrs)
IF noselst DO writef("NOSELST option was given*n")

  { // Feature added by MR 17/01/06
    // If file objline1 can be found, its first line will be written
    // at the start of the compiled Cintcode file. It first looks in the
    // current directory then the HDRS directory and finally it tries
    // g/objline1 in the system root directory.
    LET line1stream = findinput("objline1")
    LET len = 0

    UNLESS line1stream DO
      line1stream := pathfindinput("objline1", hdrs)
    UNLESS line1stream IF rootnode!rtn_rootvar DO
      line1stream := pathfindinput("g/objline1", rootnode!rtn_rootvar)
    
    IF line1stream DO
    { // Copy first line of objline1 into string objline1
      selectinput(line1stream)
      WHILE len<255 DO
      { LET ch = rdch()
        IF ch='*n' | ch=endstreamch BREAK
        len := len+1
        objline1%len := ch
      }
      endread()
      line1stream := 0
    }
    objline1%0 := len
    objline1written := FALSE
  }

  sourcestream := findinput(argv!0)       // FROM/A
  sourcenamev!0 := argv!0    // File number zero is the FROM file
  sourcefileno  := 0

  IF sourcestream=0 DO { writef("Trouble with file %s*n", argv!0)
                         IF hard DO abort(1000)
                         errcount := 1
                         GOTO fin
                       }

  selectinput(sourcestream)
 
  TEST argv!1                             // TO/K
  THEN { // Change cin/ to cin64/ if necessary.
         LET arg1 = argv!1
	 LET len  = arg1%0
         LET tofilenamev = VEC 64+1 // Room for maximum length string.
         tofilename := tofilenamev
	 FOR i = 0 TO len DO tofilename%i := arg1%i
         IF t64 & len>4 &
	    arg1%1='c'  &
	    arg1%2='i'  &
	    arg1%3='n'  &
	    arg1%4='/'  DO
         { // The target code is 64 bit and the destination starts
	   // with cin/ so replace it by cin64/
	   tofilename%4 := '6'
           tofilename%5 := '4'
           FOR i = 4 TO len DO tofilename%(i+2) := arg1%i
	   tofilename%0 := len+2
         }

         //writef("bcpl compiling to file: %s*n", tofilename)

         gostream := findoutput(tofilename)
         IF gostream=0 DO
         { writef("Trouble with code file %s*n", tofilename)
           IF hard DO abort(1000)
           errcount := 1
           GOTO fin
         }
       }
  ELSE { ocodeout := findoutput("ocode")
         IF ocodeout=0 DO
         { writes("Trouble with file ocode*n")
           IF hard DO abort(1000)
           errcount := 1
           GOTO fin
         }
       }

  treevec := getvec(treesize)
  obuf    := getvec(obufsize)

  IF treevec=0 | obuf=0 DO
  { writes("Insufficient memory*n")
    errcount := 1
    GOTO fin
  }
   
  IF argv!2 DO                            // VER/K
  { TEST xrefing
    THEN sysprint := findappend(argv!2)
    ELSE sysprint := findoutput(argv!2)
    IF sysprint=0 DO
    { sysprint := stdout
      writef("Trouble with file %s*n", argv!2)
      IF hard DO abort(1000)
      errcount := 1
      GOTO fin
    }
  }

  selectoutput(sysprint)

  // Now syntax analyse, translate and code-generate each section
  { LET b = VEC 64/bytesperword+1
    chbuf := b
    FOR i = 0 TO 63 DO chbuf%i := 0
    // Sourcefile 0 is the FROM filename
    // others are GET files of the current section
    sourcenamev!0 := argv!0
    sourcefileno := 0
    FOR i = 1 TO sourcenamevupb DO sourcenamev!i := 0 // Done for safety
    chcount, lineno := 0, (sourcefileno<<20) + 1
    token, decval, fltval := 0, 0, flt0
    rch()
 
    { // Start of loop to process each section
      LET tree = ?
      treep := treevec + treesize
      mk3list := 0
      obufp := 0
      obuft := obufsize * bytesperword

      tree := formtree()
      UNLESS tree BREAK

      //writef("Tree size %n*n", treesize+treevec-treep)
 
      IF prtree DO { writes("*nParse Tree*n")
                     plist(tree, 0, 20)
                     newline()
                     newline()
		     //abort(2345)
                   }

      IF errcount GOTO fin
 
      translate(tree)

      IF prtree2 DO { writes("*nParse Tree after calling translate*n")
                      plist(tree, 0, 20)
                      newline()
                      newline()
                    }
  
      obufq := obufp     // Prepare to read from OCODE buffer
      obufp := 0

      TEST argv!1=0  // TO/K
      THEN { // Comment out one of the following lines
             writeocode()  // Write OCODE file if no TO argument
             //writeocodebytes()
           }
      ELSE codegenerate(treevec, treesize)
    } REPEATWHILE token=s_dot
  }
   
fin:
  IF getstreams    DO { LET p = getstreams
                        getstreams := !p
                        freevec(p)
                      }
  FOR i = 0 TO sourcefileno DO
  { LET str = sourcenamev!i
    IF str DO
    { //sawritef("fileno %n %s*n", i, str)
      IF i DO freevec(str)
    }
  }

  IF sourcenamev   DO freevec(sourcenamev)

  IF treevec       DO freevec(treevec)
  IF obuf          DO freevec(obuf)
  IF sourcestream  DO IF sourcestream DO endstream(sourcestream)
  IF ocodeout      IF ocodeout UNLESS ocodeout=stdout DO endstream(ocodeout)
  IF gostream      IF gostream UNLESS gostream=stdout DO endstream(gostream)
  UNLESS sysprint=stdout DO endstream(sysprint)

  selectoutput(stdout)

  result2 := 0
  RESULTIS errcount=0 -> 0, 20
}

// ************* OCODE I/O Routines **************************

/*
The OCODE buffer variables are:

obuf         is the OCODE buffer -- (obuf=workvec)
obufp        position of next byte in the OCODE buffer
obufq        another pointer into the OCODE buffer
obuft        end of the OCODE buffer.
obufsize     size of obuf (in words)
*/

AND writeocode() BE
{ LET layout = 0
  selectoutput(ocodeout)

  UNTIL obufp>=obufq DO
  { writef(" %n", rdn())
    layout := layout+1
    UNLESS layout MOD 16 DO newline()
  }
  newline()
  selectoutput(sysprint)
  writef("OCODE size: %i5/%n*n", obufq, obuft)
}

AND rdn() = VALOF
{ LET byte = obuf%obufp
  IF obufp>=obufq RESULTIS 0
  obufp := obufp+1
  IF byte<223 RESULTIS byte
  IF byte=223 RESULTIS -1
  RESULTIS (byte&31) + (rdn()<<5)
}

AND writeocodebytes() BE
{ LET layout = 0
  selectoutput(ocodeout)

  UNTIL obufp>=obufq DO
  { writef(" => %n*n", rdnbytes())
  }
  newline()
  selectoutput(sysprint)
  writef("OCODE size: %i5/%n*n", obufq, obuft)
}

AND rdnbytes() = VALOF
{ LET byte = obuf%obufp
  writef(" %i3", byte)
  IF obufp>=obufq RESULTIS 0
  obufp := obufp+1
  IF byte<223 RESULTIS byte
  IF byte=223 RESULTIS -1
  RESULTIS (byte&31) + (rdnbytes()<<5)
}

AND wrn(n) BE
{ IF obufp>=obuft DO
  { errmax := 0 // Make it fatal
    trnerr("More workspace needed for OCODE buffer*n")
  }
  IF -1<=n<223 DO    // This is the normal case
  { IF n=-1 DO n := 223
    obuf%obufp := n
    obufp := obufp + 1
    RETURN
  }
  obuf%obufp := 224 + (n&31)
  obufp := obufp + 1
  // Perform an arithmetic right shift of 5 places
  n := n & (~31)
  n := n/32
  //n := n>>5
} REPEAT

// ************* End of  OCODE I/O Routines *******************

LET calib(a) = a!10000 + a!-10000
// calib is provided to help check the calibration of
// graphs draw using rastsys and raster.

LET lex() BE
{ LET assop = ?
  nlpending := FALSE
  //calib(1_000_000) 
  //calib(treep)
  
  {
//sawritef("lex: ch=%i3 '%c'*n", ch, ch)
 SWITCHON ch INTO
 
    { DEFAULT:
              // The following gets around a
              // bug on the Itanium
              IF ch=endstreamch GOTO endstr

            { LET badch = ch
              ch := '*s'
              synerr("Illegal character %x2", badch)
            }

      CASE '*n':  // Newline character
               lineno := lineno + 1
               nlpending := TRUE  // IGNORABLE CHARACTERS
      CASE '*p':  // Newpage character - do not increment lineno
      CASE '*c':
      CASE '*t':
      CASE '*s':
               rch() REPEATWHILE ch='*s'
               LOOP

      CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
      CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
              readdecimal()
              // token is either s_number with decval set
              // or s_fnum with fltval set. Care is needed with
	      // eg 123.. which is s_number followed by s_range
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
              token := lookupword(rdtag(ch))
              SWITCHON token INTO
              { DEFAULT: RETURN

                CASE s_get:  performget(); LOOP                    // GET

                CASE s_bitsperbcplword:                            // BITSPERBCPLWORD
                   token := s_number
                   decval := wordbitlen // Target code word length
                   RETURN

                // Some reserved words become assignment operators
                // if immediately followed by :=

                CASE s_mod:    assop := s_assmod;    GOTO checkass // MOD:=
                CASE s_lshift: assop := s_asslshift; GOTO checkass // LSHIFT:=
                CASE s_rshift: assop := s_assrshift; GOTO checkass // RSHIFT:=
                CASE s_logand: assop := s_asslogand; GOTO checkass // LOGAND:=
                CASE s_logor:  assop := s_asslogor;  GOTO checkass // LOGOR:=
                CASE s_eqv:    assop := s_asseqv;    GOTO checkass // EQV:=
                CASE s_xor:    assop := s_assxor;    GOTO checkass // XOR:=
              }

      CASE '$':
              rch()
              IF ch='$' | ch='<' | ch='>' | ch='~' DO
              { LET k = ch
//sawritef("*nprocessing $%c*n", ch)
                token := lookupword(rdtag('<'))
//sawritef("charv=%s token=%n*n", charv, token)
                // token = s_true             if the tag is set
                //       = s_false or s_name  otherwise
 
                // $>tag   marks the end of a conditional
                //         skipping section
                IF k='>' DO
                { IF skiptag=wordnode DO
                    skiptag := 0   // Matching $>tag found
                  LOOP
                }
 
                IF skiptag LOOP

                // Only process $<tag and $$tag if not skipping
 
                IF k='$' DO
                { // $$tag  complements the value of a tag
                  h1!wordnode := token=s_true -> s_false, s_true
                  LOOP
                }
 
                IF k='<' DO
                { // $<tag
                  IF token=s_true LOOP // Option set so don't skip
                }

                IF k='~' DO
                { // $~tag
                  UNLESS token=s_true LOOP // Option not set so don't skip
                }

                // Skip tokens until matching $>tag, EOF or end of section
                skiptag := wordnode
                UNTIL skiptag=0 | token=s_dot | token=s_eof DO lex()
                skiptag := 0
                RETURN
              }
 
              UNLESS ch='(' | ch=')' DO synerr("'$' out of context")
              token := ch='(' -> s_lsect, s_rsect                    // $(tag
              lookupword(rdtag('$'))                                 // $)tag
              RETURN
 
      CASE '{': token, wordnode := s_lsect, nulltag; BREAK           // {
      CASE '}': token, wordnode := s_rsect, nulltag; BREAK           // }

      CASE '#':
              token := s_number
              rch()
              IF '0'<=ch<='7' DO                                      // #377
              { decval := readnumber( 8, 100)
                RETURN
              }
              IF ch='b' | ch='B' DO                                   // #B1101
              { rch()
                decval := readnumber( 2, 100)
                RETURN
              }
              IF ch='o' | ch='O' DO                                   // #O477
              { rch()
                decval := readnumber( 8, 100)
                RETURN
              }
              IF ch='x' | ch='X' DO                                    // #X7FF4
              { rch()
                decval := readnumber(16, 100)
                RETURN
              }
              IF ch='(' DO                                             // #(
              { token := s_mthap
                RETURN
              }
              UNLESS ch<32 DO
              { // Get the next token
                lex()
                SWITCHON token INTO
                { DEFAULT:       ENDCASE

                  CASE s_abs:    token := s_fabs;    RETURN // #ABS
                  CASE s_range:  token := s_frange;  RETURN // #..

                  CASE s_mul :   token := s_fmul;    RETURN // #*
                  CASE s_div:    token := s_fdiv;    RETURN // #/
                  CASE s_mod:    token := s_fmod;    RETURN // #MOD
                  CASE s_add:    token := s_fadd;    RETURN // #+
                  CASE s_sub:    token := s_fsub;    RETURN // #-

                  CASE s_ass:    token := s_fass;    RETURN // #:=
                  CASE s_assmul: token := s_assfmul; RETURN // #*:=
                  CASE s_assdiv: token := s_assfdiv; RETURN // #/:=
                  CASE s_assmod: token := s_assfmod; RETURN // #MOD:=
                  CASE s_assadd: token := s_assfadd; RETURN // #+:=
                  CASE s_asssub: token := s_assfsub; RETURN // #-:=

                  CASE s_eq:     token := s_feq;     RETURN // #=
                  CASE s_ne:     token := s_fne;     RETURN // #~=
                  CASE s_ls:     token := s_fls;     RETURN // #<
                  CASE s_le:     token := s_fle;     RETURN // #<=
                  CASE s_gr:     token := s_fgr;     RETURN // #>
                  CASE s_ge:     token := s_fge;     RETURN // #>=

                  CASE s_cond:   token := s_fcond;   RETURN // #->
                }
              }
              synerr("'#' out of context")

      CASE '[': token := s_sbra;      BREAK                  // [
      CASE ']': token := s_sket;      BREAK                  // ]
      CASE '(': token := s_lparen;    BREAK                  // (
      CASE ')': token := s_rparen;    BREAK                  // }
      CASE '?': token := s_query;     BREAK                  // ?
      CASE ',': token := s_comma;     BREAK                  // ,
      CASE ';': token := s_semicolon; BREAK                  // :
      CASE '@': token := s_lv;        BREAK                  // @
      CASE '%': token := s_byteap;    BREAK                  // %

      CASE '=': rch()
                IF ch='>' DO { token := s_yields; BREAK }    // =>
                token := s_eq                                // =
                RETURN

      CASE '.': rch()
                IF ch='.' DO { token := s_range; BREAK }      // ..
                token := s_dot                                // .
               UNLESS getstreams RETURN
		synerr("A section separating dot is not allowed in GET files")
		LOOP

checkassx:      rch()
checkass:       UNLESS ch=':' RETURN
                rch()
                UNLESS ch='=' DO synerr("Bad assignment operator")
                token := assop
                BREAK
 
      CASE '!': token, assop := s_vecap, s_assvecap;   GOTO checkassx // !:= or !
      CASE '**':token, assop := s_mul, s_assmul;       GOTO checkassx // *:= or *
      CASE '+': token, assop := s_add, s_assadd;       GOTO checkassx // +:= or +
      CASE '&': token, assop := s_logand, s_asslogand; GOTO checkassx // &:= or &
      CASE '|': token, assop := s_logor, s_asslogor;   GOTO checkassx // |:= or |
 
      CASE '/':
              rch()
              //IF ch='\' DO    // Disallow /\ for &
              //{ token, assop := s_logand, s_asslogand
              //  GOTO checkassx
              //}
              IF ch='/' DO
              { rch() REPEATUNTIL ch='*n' |
                                  //ch='*p' | // Do not increment lineno
                                  ch=endstreamch
                LOOP
              }
 
              IF ch='**' DO
              { LET depth = 1

                { rch()
                  IF ch='**' DO
                  { rch() REPEATWHILE ch='**'
                    IF ch='/' DO { depth := depth-1; LOOP }
                  }
                  IF ch='/' DO
                  { rch()
                    IF ch='**' DO { depth := depth+1; LOOP }
                  }
                  IF ch='*n' DO lineno := lineno+1
                  IF ch=endstreamch DO synerr("Missing '**/'")
                } REPEATUNTIL depth=0

                rch()
                LOOP
              }

              token, assop := s_div, s_assdiv
              GOTO checkass
 
      CASE '~':
              rch()
              IF ch='=' DO { token := s_ne;     BREAK }          // ~=
              token := s_not                                     // ~
              RETURN
 
      CASE '\':
              rch()
              //IF ch='/' DO    // Disallow \/ for |
              //{ token, assop := s_logor, s_asslogor
              //  GOTO checkassx
              //}
              IF ch='=' DO { token := s_ne;     BREAK }           // \=
              token := s_not                                      // \
              RETURN
 
      CASE '<': rch()
              IF ch='=' DO { token := s_le;     BREAK }            // <=
              IF ch='<' DO
              { token, assop := s_lshift, s_asslshift              // << or <<:=
                GOTO checkassx
              }
              IF ch='>' DO { token := s_seq;    BREAK }            // <>
              token := s_ls                                        // <
              RETURN
 
      CASE '>': rch()
              IF ch='=' DO { token := s_ge;     BREAK }            // >=
              IF ch='>' DO
              { token, assop := s_rshift, s_assrshift              // >> or >>:=
                GOTO checkassx
              }
              token := s_gr                                        // >
              RETURN
 
      CASE '-': rch()
              IF ch='>' DO { token := s_cond; BREAK  }             // ->
              token, assop := s_sub, s_asssub                      // - or -:=
              GOTO checkass
 
      CASE ':': rch()
              IF ch='=' DO { token := s_ass; BREAK  }              // :=
              IF ch=':' DO { token := s_of;  BREAK  }              // ::
              token := s_colon                                     // :
              RETURN
 
      CASE '"':                                                    // "string"
           { LET len = 0
             rch()
             encoding := defaultencoding // encoding for *# escapes

             UNTIL ch='"' DO
             { LET code = rdstrch()
               TEST result2
               THEN { // A  *# code found.
                      // Convert it to UTF8 or GB2312 format.
                      TEST encoding=GB2312
                      THEN { // Convert to GB2312 sequence
                             IF code>#x7F DO
                             { LET hi = code  /  100 + 160
                               LET lo = code MOD 100 + 160
                               IF len>=254 DO synerr("Bad string constant")
                               TEST bigender
                               THEN { charv%(len+1) := hi 
                                      charv%(len+2) := lo
                                    }
                               ELSE { charv%(len+1) := lo 
                                      charv%(len+2) := hi
                                    }
                               len := len + 2
                               LOOP
                             }
                             IF len>=255 DO synerr("Bad string constant")
                             charv%(len+1) := code // Ordinary ASCII char
                             len := len + 1
                             LOOP
                           }
                      ELSE { // Convert to UTF8 sequence
                             IF code<=#x7F DO
                             { IF len>=255 DO synerr("Bad string constant")
                               charv%(len+1) := code   // 0xxxxxxx
                               len := len + 1
                               LOOP
                             }
                             IF code<=#x7FF DO
                             { IF len>=254 DO synerr("Bad string constant")
                               charv%(len+1) := #b1100_0000+(code>>6)  // 110xxxxx
                               charv%(len+2) := #x80+( code    &#x3F)  // 10xxxxxx
                               len := len + 2
                               LOOP
                             }
                             IF code<=#xFFFF DO
                             { IF len>=253 DO synerr("Bad string constant")
                               charv%(len+1) := #b1110_0000+(code>>12) // 1110xxxx
                               charv%(len+2) := #x80+((code>>6)&#x3F)  // 10xxxxxx
                               charv%(len+3) := #x80+( code    &#x3F)  // 10xxxxxx
                               len := len + 3
                               LOOP
                             }
                             IF code<=#x1F_FFFF DO
                             { IF len>=252 DO synerr("Bad string constant")
                               charv%(len+1) := #b1111_0000+(code>>18) // 11110xxx
                               charv%(len+2) := #x80+((code>>12)&#x3F) // 10xxxxxx
                               charv%(len+3) := #x80+((code>> 6)&#x3F) // 10xxxxxx
                               charv%(len+4) := #x80+( code     &#x3F) // 10xxxxxx
                               len := len + 4
                               LOOP
                             }
                             IF code<=#x3FF_FFFF DO
                             { IF len>=251 DO synerr("Bad string constant")
                               charv%(len+1) := #b1111_1000+(code>>24) // 111110xx
                               charv%(len+2) := #x80+((code>>18)&#x3F) // 10xxxxxx
                               charv%(len+3) := #x80+((code>>12)&#x3F) // 10xxxxxx
                               charv%(len+4) := #x80+((code>> 6)&#x3F) // 10xxxxxx
                               charv%(len+5) := #x80+( code     &#x3F) // 10xxxxxx
                               len := len + 5
                               LOOP
                             }
                             IF code<=#x7FFF_FFFF DO
                             { IF len>=250 DO synerr("Bad string constant")
                               charv%(len+1) := #b1111_1100+(code>>30) // 1111110x
                               charv%(len+2) := #x80+((code>>24)&#x3F) // 10xxxxxx
                               charv%(len+3) := #x80+((code>>18)&#x3F) // 10xxxxxx
                               charv%(len+4) := #x80+((code>>12)&#x3F) // 10xxxxxx
                               charv%(len+5) := #x80+((code>> 6)&#x3F) // 10xxxxxx
                               charv%(len+6) := #x80+( code     &#x3F) // 10xxxxxx
                               len := len + 6
                               LOOP
                             }
                             synerr("Bad Unicode character")
                           }
                    }
               ELSE { // Not a Unicode character
                      IF len=255 DO synerr("Bad string constant")
                      len := len + 1
                      charv%len := code
                    }
             }
 
             charv%0 := len
             wordnode := newvec(len/bytesperword+2)
             h1!wordnode := s_string
             FOR i = 0 TO len DO (@h2!wordnode)%i := charv%i
             token := s_string
             BREAK
          }
 
      CASE '*'':                                                  // 'c'
              rch()
              encoding := defaultencoding
              decval := rdstrch()
              token := s_number
              UNLESS ch='*'' DO synerr("Bad character constant")
              BREAK
 
 endstr:
      //CASE endstreamch: // Commented out because of an Itanium bug
              IF getstreams DO
              { // Return from a 'GET' stream
                LET p = getstreams
                endread()
                ch           := h4!getstreams
                lineno       := h3!getstreams
                sourcestream := h2!getstreams
                getstreams   := h1!getstreams
                freevec(p) // Free the GET node
                selectinput(sourcestream)
                LOOP
              }
              // endstreamch => EOF only at outermost GET level 
              token := s_eof                                         // eof
              RETURN
    }
  } REPEAT
 
  rch()
}
 
LET lookupword(word) = VALOF
{ LET len, i = word%0, 0
  LET hashval = 19609 // This and 31397 are primes.
  FOR j = 0 TO len DO hashval := (hashval XOR word%j) * 31397
  hashval := (hashval>>1) MOD nametablesize

  wordnode := nametable!hashval
 
  UNTIL wordnode=0 | i>len TEST (@h3!wordnode)%i=word%i
                           THEN i := i+1
                           ELSE wordnode, i := h2!wordnode, 0
 
  UNLESS wordnode DO
  { wordnode := newvec(len/bytesperword+2)
    h1!wordnode, h2!wordnode := s_name, nametable!hashval
    FOR i = 0 TO len DO (@h3!wordnode)%i := word%i
    nametable!hashval := wordnode
  }
 
  RESULTIS h1!wordnode
}
 
LET eqlookupword(word) = VALOF
{ // This version equates the cases but keeps the cases of
  // the first word encountered. If EQCASES is given this version
  // replaces lookupword.
  LET len, i = word%0, 0
  LET hashval = 19609 // This and 31397 are primes.
  // This hash function ignores the case of letters.
  FOR j = 0 TO len DO hashval := (hashval XOR (word%j & 31)) * 31397
  hashval := (hashval>>1) MOD nametablesize

  wordnode := nametable!hashval
 
  UNTIL wordnode=0 | i>len TEST compch((@h3!wordnode)%i, word%i)=0
                           THEN i := i+1
                           ELSE wordnode, i := h2!wordnode, 0
 
  UNLESS wordnode DO
  { wordnode := newvec(len/bytesperword+2)
    h1!wordnode, h2!wordnode := s_name, nametable!hashval
    FOR i = 0 TO len DO (@h3!wordnode)%i := word%i
    nametable!hashval := wordnode
  }

  RESULTIS h1!wordnode
}
 
AND dsw(word, sym) BE { lookupword(word); h1!wordnode := sym  }
 
AND declsyswords() BE
{ dsw("AND", s_and)  // Added old 1980s style reserved word for historic reasons.
  dsw("ABS", s_abs)
  dsw("BE", s_be)
  dsw("BITSPERBCPLWORD", s_bitsperbcplword)
  dsw("BREAK", s_break)
  dsw("BY", s_by)
  dsw("CASE", s_case)
  dsw("DO", s_do)
  dsw("DEFAULT", s_default)
  dsw("EQ", s_eq)
  dsw("EQV", s_eqv)
  dsw("ELSE", s_else)
  dsw("ENDCASE", s_endcase)
  dsw("EVERY", s_every)
  dsw("EXIT", s_exit)
  dsw("FALSE", s_false)
  dsw("FINISH", s_finish)
  dsw("FIX", s_fix)
  dsw("FLOAT", s_float)
  dsw("FLT", s_flt)
  dsw("FOR", s_for)
  dsw("GOTO", s_goto)
  dsw("GE", s_ge)
  dsw("GR", s_gr)
  dsw("GLOBAL", s_global)
  dsw("GET", s_get)
  dsw("IF", s_if)
  dsw("INTO", s_into)
  dsw("LET", s_let)
  dsw("LV", s_lv)
  dsw("LE", s_le)
  dsw("LS", s_ls)
  dsw("LOGOR", s_logor)
  dsw("LOGAND", s_logand)
  dsw("LOOP", s_loop)
  dsw("LSHIFT", s_lshift)
  dsw("MANIFEST", s_manifest)
  dsw("MATCH", s_match)
  dsw("MOD", s_mod)
  dsw("NE", s_ne)
  dsw("NEEDS", s_needs)
  dsw("NEQV", s_xor)
  dsw("NEXT", s_next)
  dsw("NOT", s_not)
  dsw("OF", s_of)                   // Inserted 11/7/01
  dsw("OR", s_else)
  dsw("RESULTIS", s_resultis)
  dsw("RETURN", s_return)
  dsw("REM", s_mod)
  dsw("RSHIFT", s_rshift)
  dsw("RV", s_rv)
  dsw("REPEAT", s_repeat)
  dsw("REPEATWHILE", s_repeatwhile)
  dsw("REPEATUNTIL", s_repeatuntil)
  dsw("SECTION", s_section)
  dsw("SLCT", s_slct)               // Inserted 11/7/01
  dsw("STATIC", s_static)
  dsw("SWITCHON", s_switchon)
  dsw("TO", s_to)
  dsw("TEST", s_test)
  dsw("TRUE", s_true)
  dsw("THEN", s_do)
  dsw("TABLE", s_table)
  dsw("UNLESS", s_unless)
  dsw("UNTIL", s_until)
  dsw("VEC", s_vec)
  dsw("VALOF", s_valof)
  dsw("WHILE", s_while)
  dsw("XOR", s_xor)
  dsw("$", 0)
 
  nulltag := wordnode
} 
 
LET rch() BE
{ ch := rdch()
  chcount := chcount + 1
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
 
 
AND rdoptstring() = VALOF
{ LET pos = 1 // The position of the next optstring
              // character to consider
  LET optstringlen = optstring%0
  LET optch = ?

  { // Get next option name, if any
    LET len = 1
    charv%0, charv%1 := 1, '<'
 
    // Skip characters before option name
    WHILE pos<=optstringlen DO
    { optch := optstring%pos
      IF 'a'<=optch<='z' | 'A'<=optch<='Z' |
         '0'<=optch<='9' | optch='.' | optch='_' BREAK
      pos := pos+1
    }

    // Copy option name, if any, into charv
    WHILE pos<=optstringlen DO
    { optch := optstring%pos
      UNLESS 'a'<=optch<='z' | 'A'<=optch<='Z' |
             '0'<=optch<='9' | optch='.' | optch='_' BREAK
      // Copy next option name character into charv, if room
      len := len+1
      IF len<=255 DO charv%0, charv%len := len, optch
      pos := pos+1
    }

    IF len<=1 BREAK // No more option names

    // Declare option name
    token := lookupword(charv)
    h1!wordnode := s_true

//sawritef("Option name: ", wordnode, h1!wordnode)
//FOR i = 2 TO charv%0 DO sawrch(charv%i)
//sawritef(" declared*n")

  } REPEAT    // Read next option name, if any
}

AND rdtag(ch1) = VALOF
{ LET len = 1
  ///IF eqcases & 'a'<=ch1<='z' DO ch1 := ch1 + 'A' - 'a'
  charv%1 := ch1
 
  { rch()

    IF ch='.' DO // Disallow .. in tags since this is the range operator
    { LET k = rdch()
      unrdch()
      IF k='.' BREAK
    }
      
    UNLESS 'a'<=ch<='z' | 'A'<=ch<='Z' |
           '0'<=ch<='9' | ch='.' | ch='_' BREAK
    ///IF eqcases & 'a'<=ch<='z' DO ch := ch + 'A' - 'a'
    len := len+1
    charv%len := ch
  } REPEAT
 
  charv%0 := len
  RESULTIS charv
}

AND catstr(s1, s2) = VALOF
// Concatenate strings s1 and s2 leaving the result in s1.
// s1 is assumed to be able to hold a string of length 255.
// The resulting string is truncated to length 255, if necessary. 
{ LET len = s1%0
  LET n = len
  FOR i = 1 TO s2%0 DO
  { n := n+1
    IF n>255 BREAK
    s1%n := s2%i
  }
  s1%0 := n
} 
 
AND performget() BE
{ LET stream = ?
  LET len = 0
  lex()
  UNLESS token=s_string DO synerr("Bad GET directive")
  len := charv%0

  // Append .h to the GET filename does not end in .h or .b
  UNLESS len>=2 & charv%(len-1)='.' & 
         (charv%len='h' | charv%len='b') DO
  { len := len+2
    charv%0, charv%(len-1), charv%len := len, '.', 'h'
  }

  // Treat filenames like sys:xxx as sys/xxx -- deprecated feature 
  FOR i = 1 TO charv%0 IF charv%i=':' DO charv%i := '/'

  // First look in the current directory
  //writef("Searching for *"%s*" in the current directory*n", charv)
  stream := pathfindinput(charv, 0)


  // Then try the headers directories
  //UNLESS stream DO sawritef("Searching for *"%s*" in %s*n", charv, hdrs)
  // The value of hdrs is typically: ...../BCPL/cintcode/g
  UNLESS stream DO stream := pathfindinput(charv, hdrs)

  UNLESS stream DO
  { synerr("Unable to find GET file %s", charv)
    RETURN
  }

  IF sourcefileno>=sourcenamevupb DO
  { synerr("Too many GET files")
    RETURN
  }

  { LET len  = charv%0
    LET node = getvec(3)  // Freed at end of GET insertion
    LET str  = getvec(len/bytesperword+1) // Freed at end of compilation

    UNLESS node & str DO
    { IF node DO freevec(node)
      IF str  DO freevec(str)
      synerr("getvec failure in performget")
    }
    FOR i = 0 TO len DO str%i := charv%i
    sourcefileno := sourcefileno+1
    sourcenamev!sourcefileno := str

    node!0, node!1, node!2, node!3 := getstreams, sourcestream, lineno, ch
    getstreams := node
  }
  sourcestream := stream
  selectinput(sourcestream)
  lineno := (sourcefileno<<20) + 1
  rch()
}

AND readdecimal() BE
{ // Read an integer or floating point constant
  // setting token to s_number with the integer value in decval
  // or s_fnum with the floating point value in fltval.
  // The strategy is to simultaneously construct both the integer
  // and floating point values. It stops constructing the integer
  // value after reading a decimal point or e, ie when the
  // constant is known to be floating point.
  // Care is needed with eg 123.. which is s_number followed by s_range
  LET pos      = 0    // Number of integer and fractional digits
                      // in the number.
  LET sigpos   = 0    // Position of the last significant digit
  LET pointpos = 0    // Position of the digit just left of the
                      // decimal point

  token := s_number // Until '.' or 'e' encountered
  decval, exponent, fltval := 0, 0, flt0

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
        IF token=s_number DO pointpos := pos

        decval := 10*decval + ch-'0' // Accumulate the integer value

        IF sys(Sys_flt, fl_eq, x, sys(Sys_flt, fl_add, x, flt1)) ENDCASE

        // fltval * 10 + 1 is not equal to fltval * 10, so
        // the digit is significant
        // Perform fltval := x + FLOAT(ch-'0') and increment sigpos .
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
	  RETURN   // Return with token=s_number
	}
        IF token=s_fnum DO synerr("Two decimal points in a number")
        token := s_fnum
        ENDCASE
      }
      
      CASE '_':  // Ignore underlines in numbers.
        ENDCASE
    }
    rch()
  }

//sawritef("readdecimal: token=%s decval=%n fltval=%13.1e *
//         *pos=%n sigpos=%n pointpos=%n*n",
//          opname(token), decval, fltval, pos, sigpos, pointpos)

  IF ch='e' | ch='E' DO
  { LET expneg = FALSE
    token := s_fnum
    rch()
    IF ch='-' DO { expneg := TRUE; rch() }
    WHILE '0'<=ch<='9' | ch='_' DO
    { UNLESS ch='_' DO exponent := 10*exponent + ch-'0'
      rch()
    }
    IF expneg DO exponent := -exponent
  }

  IF token=s_number DO
  { // There was no decimal point or e so leave token=s_number
    // and the integer value in decval.
    RETURN
  }

  // token is s_fnum

//sawritef("*nreaddecimal: making fnumber fltval=%13.1e *
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

AND readnumber(radix, digs) = VALOF
// Read a binary, octal, decimal or hexadecimal unsigned number
// with between 1 and digs digits. Underlines are allowed.
// This function is only used for numerical constants starting
// with # or numerical escapes in string and character constants.
{ LET i, res = 0, 0
 
  { UNLESS ch='_' DO // ignore underlines
    { LET d = value(ch)
      IF d>=radix BREAK
      i := i+1       // Increment count of digits
      res := radix*res + d
    }
    rch()
  } REPEATWHILE i<digs

  UNLESS i DO synerr("Bad number")
  RESULTIS res
}
 
AND value(ch) = '0'<=ch<='9' -> ch-'0',
                'A'<=ch<='F' -> ch-'A'+10,
                'a'<=ch<='f' -> ch-'a'+10,
                100
 
AND rdstrch() = VALOF
{ // Return the integer code for the next string character
  // Set result2=TRUE if *# character code was found, otherwise FALSE
  LET k = ch

  IF k='*n' DO
  { lineno := lineno+1
    synerr("Unescaped newline character")
  }
 
  IF k='**' DO
  { rch()
    k := ch
    IF 'a'<=k<='z' DO k := k + 'A' - 'a'
    SWITCHON k INTO
    { CASE '*n':
      CASE '*c':
      CASE '*p':
      CASE '*s':
      CASE '*t':
      CASE  '/': // Ignore white space until the next asterisk.
                 // Comments starting with '//' are treated as
                 // white space, but those starting with '/*'
                 // are not.
                 { WHILE ch='*n' | ch='*c' | ch='*p' | ch='*s' | ch='*t' DO
                   { IF //ch='*p' |  // Do not increment lineno
                        ch='*n' DO lineno := lineno+1
                     rch()
                   }
                   IF ch='/' DO
                   { rch()
                     IF ch='/' DO
                     { // Skip over a '//' comment
                       rch() REPEATUNTIL ch='*n' |
                                         ch='*p' |
                                         ch=endstreamch
                       LOOP
                     }
                   }
                   BREAK
                 } REPEAT
                 IF ch='**' DO { rch(); LOOP  }

      DEFAULT:   synerr("Bad string or character constant, ch=%n", ch)
         
      CASE '**':
      CASE '*'':
      CASE '"':                    ENDCASE
         
      CASE 'T':  k := c_tab;       ENDCASE
      CASE 'S':  k := c_space;     ENDCASE
      CASE 'N':  k := c_newline;   ENDCASE
      CASE 'E':  k := c_escape;    ENDCASE
      CASE 'B':  k := c_backspace; ENDCASE
      CASE 'P':  k := c_newpage;   ENDCASE
      CASE 'C':  k := c_return;    ENDCASE
         
      CASE 'X':  // *xhh  -- A character escape in hexadecimal
                 rch()
                 k := readnumber(16,2)
                 result2 := FALSE
                 RESULTIS k

      CASE '#':  // *#u   set UTF8 mode
                 // *#g   set GB2312 mode
                 // In UTF8 mode
                 //     *#hhhh or *##hhhhhhhh  -- a Unicode character
                 // In GB2312
                 //     *#dddd                 -- A GB2312 code
               { LET digs = 4
                 rch()
                 IF ch='u' | ch='U' DO { encoding := UTF8;   rch(); LOOP }
                 IF ch='g' | ch='G' DO { encoding := GB2312; rch(); LOOP }
                 TEST encoding=GB2312
                 THEN { 
                        k := readnumber(10, digs)
//sawritef("rdstrch: GB2312: %i4*n", k)
                      }
                 ELSE { IF ch='#' DO { rch(); digs := 8 }
                        k := readnumber(16, digs)
//sawritef("rdstrch: Unicode: %x4*n", k)
                      }
                 result2 := TRUE
                 RESULTIS k
               }

      CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
      CASE '5':CASE '6':CASE '7':
                 // *ooo -- A character escape in octal 
                 k := readnumber(8,3)
                 IF k>255 DO 
                       synerr("Bad string or character constant")
                 result2 := FALSE
                 RESULTIS k
    }
  }
   
  rch()
  result2 := FALSE
  RESULTIS k
} REPEAT

LET newvec(n) = VALOF
{ treep := treep - n - 1;
  IF treep<=treevec DO
  { errmax := 0  // Make it fatal
    synerr("More workspace needed")
  }
  RESULTIS treep
}
 
AND mk1(x) = VALOF
{ LET p = newvec(0)
  p!0 := x
  RESULTIS p
}
 
AND mk2(x, y) = VALOF
{ LET p = newvec(1)
  p!0, p!1 := x, y
  RESULTIS p
}
 
AND mk3(x, y, z) = VALOF
{ LET p = mk3list
  TEST p THEN mk3list := h1!mk3list
         ELSE p := newvec(2)
  p!0, p!1, p!2 := x, y, z
  RESULTIS p
}

AND unmk3(p) BE
{ // Only used by cvvaldef and cvass to recover space
  // used by comma nodes.
  h1!p := mk3list
  mk3list := p
}
 
AND mk4(x, y, z, t) = VALOF
{ LET p = newvec(3)
  p!0, p!1, p!2, p!3 := x, y, z, t
  RESULTIS p
}
 
AND mk5(x, y, z, t, u) = VALOF
{ LET p = newvec(4)
  p!0, p!1, p!2, p!3, p!4 := x, y, z, t, u
  RESULTIS p
}
 
AND mk6(x, y, z, t, u, v) = VALOF
{ LET p = newvec(5)
  p!0, p!1, p!2, p!3, p!4, p!5 := x, y, z, t, u, v
  //sawritef("mk6 => %n*n", p*4)
  RESULTIS p
}
 
AND mk7(x, y, z, t, u, v, w) = VALOF
{ LET p = newvec(6)
  p!0, p!1, p!2, p!3, p!4, p!5, p!6 := x, y, z, t, u, v, w
  RESULTIS p
}
 
AND formtree() =  VALOF
{ LET res = 0

  nametablesize := 541

  charv      := newvec(256/bytesperword+1)
  charv%0 := 0
  nametable  := newvec(nametablesize) 
  FOR i = 0 TO nametablesize DO nametable!i := 0
  skiptag := 0
  declsyswords()

  rec_p, rec_l := level(), rec
 
  token, decval := 0, 0

  rdoptstring()

  lex()

  IF token=s_query DO            // For debugging lex.
  { LET ln, name = ?, ?
    lex()
    ln := lineno & #xFFFFF
    //abort(1000)
    name := opname(token)

    SWITCHON token INTO
    { DEFAULT:
        writef("token =%i3 ln=%i5 %12t  *n",   token, ln, name)
        ENDCASE

      CASE s_name:
        writef("token =%i3 ln=%i5 %12t  %s*n", token, ln, name, @h3!wordnode)
        ENDCASE  

      CASE s_number:
        writef("token =%i3 ln=%i5 %12t  %n*n", token, ln, name, decval)
        ENDCASE  

      CASE s_fnum:
        writef("token =%i3 ln=%i5 %12t %13.9e *n", token, ln, name, fltval)
        ENDCASE  

      CASE s_string:
      { LET s = @h2!wordnode
        writef("token =%i3 ln=%i5 %12t *"", token, ln, name)
        FOR i = 1 TO s%0 DO
        { LET ch = s%i
          SWITCHON ch INTO
          { DEFAULT:     wrch(ch);    LOOP

            CASE '*n': writes("**n"); LOOP
            CASE '*s': writes("**s"); LOOP
            CASE '*p': writes("**p"); LOOP
            CASE '*t': writes("**t"); LOOP
          }
        }
        writes("*"*n")
        ENDCASE
      }  
    }

    IF token=s_eof RESULTIS 0
  } REPEAT

rec:
  res := token=s_section -> rprog(s_section),
         token=s_needs   -> rprog(s_needs),
	                    rdblockbody(TRUE)

  UNLESS token=s_dot |
         token=s_eof DO synerr("Incorrect termination")
 
  RESULTIS res
}
 
AND rprog(op) = VALOF
{ // op is either s_section or s_needs
  LET a = 0
  lex()
  a := rbexp()
  UNLESS h1!a=s_string DO synerr("Bad SECTION or NEEDS name")
  RESULTIS mk3(op, a,
               token=s_needs -> rprog(s_needs),
                                rdblockbody(TRUE)) // TRUE=outmost level
}
 
 
AND synerr(mess, a) BE
{ LET fno = lineno>>20
  LET ln = lineno & #xFFFFF
  LET filename = sourcenamev!fno
  errcount := errcount + 1
  writef("*nError near ")
  IF filename DO writef("%s", filename)
  writef("[%n]:  ", ln)
  writef(mess, a)
  wrchbuf()
  IF hard DO abort(1000)
  IF errcount > errmax DO
  { writes("*nCompilation aborted*n")
    longjump(fin_p, fin_l)
  }
  nlpending := FALSE
 
  UNTIL token=s_lsect | token=s_rsect |
        token=s_let | token=s_and |
        token=s_dot | token=s_eof | nlpending DO lex()

  IF token=s_and DO token := s_let
  longjump(rec_p, rec_l)
}
 
AND paterr(mess, a) BE
{ LET fno = lineno>>20
  LET ln = lineno & #xFFFFF
  LET filename = sourcenamev!fno
  errcount := errcount + 1
  writef("*nError near ")
  IF filename DO writef("%s", filename)
  writef("[%n]:  ", ln)
  writef(mess, a)
  wrchbuf()
  IF hard DO abort(1000)
  IF errcount > errmax DO
  { writes("*nCompilation aborted*n")
    longjump(fin_p, fin_l)
  }
}
 
LET rdblockbody(outerlevel) = VALOF
{ // If outerlevel=TRUE, local and vector declarations are not permitted.
  LET p, l = rec_p, rec_l
  LET a, ln = 0, ?
 
  rec_p, rec_l := level(), recover

recover:  
  IF token=s_semicolon DO lex()
 
  ln := lineno
   
  SWITCHON token INTO
  { CASE s_manifest:
    CASE s_static:
    CASE s_global:
              { LET op = token
                lex()
                a := rdsect(rdcdefs, op=s_global->s_colon,s_eq)
                a := mk4(op, a, rdblockbody(outerlevel), ln)
                ENDCASE
              }
 
 
    CASE s_let: lex()
                a := rdef(outerlevel)
                WHILE token=s_and DO
                { LET ln1 = lineno
                  lex()
                  a := mk4(s_and, a, rdef(outerlevel), ln1)
                }
                a := mk4(s_let, a, rdblockbody(outerlevel), ln)
                ENDCASE
 
    DEFAULT:    IF outerlevel DO
                { errmax := 0 // Make it fatal.
                  synerr("Bad outer level declaration")
                }
                a := rdseq()
                UNLESS token=s_rsect DO synerr("Error in command")
 
    CASE s_rsect:IF outerlevel DO lex()
    CASE s_dot:
    CASE s_eof:
  }
 
  rec_p, rec_l := p, l
  RESULTIS a
}
 
AND rdseq() = VALOF
{ LET a = 0
  IF token=s_semicolon DO lex()
  a := rcom()
  IF token=s_rsect | token=s_dot | token=s_eof RESULTIS a
  RESULTIS mk3(s_seq, a, rdseq())
}

AND rdcdefs(sep) = VALOF
{ LET res, id = 0, 0
  LET ptr = @res
  LET p, l = rec_p, rec_l
  LET kexp = 0

  { LET ln = lineno
    rec_p, rec_l := level(), recov
    kexp := 0
    id := rname()
    IF token=sep DO kexp := rnexp(0)
    !ptr := mk5(s_constdef, 0, id, kexp, ln)
    ptr := @h2!(!ptr)

recov:IF token=s_semicolon DO lex()
  } REPEATWHILE token=s_name | token=s_flt
 
  rec_p, rec_l := p, l
  RESULTIS res
}
 
AND rdsect(r, arg) = VALOF
// Used only for MANIFEST, STATIC and GLOBAL declarations,
// SWITCHON commands and blocks.
{ LET tag, res = wordnode, 0
  UNLESS token=s_lsect DO synerr("'{' or '$(' expected")
  lex()
  UNLESS token=s_rsect DO res := r(arg) // Allow { }  MR 22/6/05
  UNLESS token=s_rsect DO synerr("'}' or '$)' expected")
  TEST tag=wordnode THEN lex()
                    ELSE IF wordnode=nulltag DO
                         { token := 0
                           synerr("Untagged '$)' mismatch")
                         }
  // res=0 for empty section brackets { }
  RESULTIS res
}

AND rnamelist() = VALOF
{ // Read a list of names each possibly prefixed by FLT
  LET a = rname()
  UNLESS token=s_comma RESULTIS a
  lex()
  RESULTIS mk3(s_comma, a, rnamelist())
}

AND rname() = VALOF
{ // Read a name possibly prefixed by FLT.
  LET a = ?
  TEST token = s_flt
  THEN { lex()
         UNLESS token=s_name DO synerr("Name expected")
	 a := mk2(s_flt, wordnode)
	 // Note: a FLT always have a name node as operand.
       }
  ELSE { UNLESS token=s_name DO synerr("Name expected")
         a := wordnode
       }
  lex()
  RESULTIS a
}

LET rnbexp() = VALOF
{ lex()
  RESULTIS rbexp()
}

LET rbexp() = VALOF
{ LET a, op = 0, token

  SWITCHON token INTO
 
  { DEFAULT:
      synerr("Error in expression")

    CASE s_query:
      lex()
      RESULTIS mk1(s_query)
 
    CASE s_true:
    CASE s_false:
    CASE s_name:
    CASE s_string:
      a := wordnode
      lex()
      RESULTIS a
 
    CASE s_break:
    CASE s_loop:
    CASE s_endcase:
    CASE s_next:
    CASE s_exit:
      RESULTIS rbcom()
	
    CASE s_number:
      a := mk2(s_number, decval)
      lex()
      RESULTIS a

    CASE s_fnum:
      UNLESS -128<=exponent<=127 DO
        synerr("Exponent of floating point constant out of range")
      floatingchk()
      a := mk2(s_fnum, fltval)
      lex()
      RESULTIS a

    CASE s_match:
    CASE s_every:
    { LET args = 0
      LET mlist = 0
      LET ln = lineno
      lex()
      UNLESS token=s_lparen DO synerr("'(' expected after MATCH")
      UNLESS token=s_rparen DO args := rnexplist() // Allow () ...
      UNLESS token=s_rparen DO
      synerr("')' missing at the end of the MATCH argument list")
      lex() 
      mlist := rdmatchlist(s_yields)
      // mlist -> [matchiteme, Plist, E, link, ln]

      // ie match items linked through the h4 field

      RESULTIS mk4(op=s_match -> s_matche, s_everye,
                   args, mlist, ln)
    }

    CASE s_slct:
    { LET len, sh, offset = 0, 0, 0  // Inserted 11/7/01

      // Allow   SLCT offset
      // or      SLCT sh:offset
      // or      SLCT len:sh:offset

      offset := rnexp(9)

      IF token=s_colon DO
      { sh := offset
        offset := rnexp(9)
      }
      IF token=s_colon DO
      { len := sh
        sh := offset
        offset := rnexp(9)
      }

      RESULTIS mk4(s_slct, len, sh, offset)
    }
 
  CASE s_lparen:
    a := rnexp(0)
    UNLESS token=s_rparen DO synerr("')' missing")
    lex()
    RESULTIS a
 
  CASE s_valof:
    lex()
    RESULTIS mk2(s_valof, rcom())
 
  CASE s_vecap:
    op := s_rv
  CASE s_rv:
  CASE s_lv:
  CASE s_float:
  CASE s_fix:
    RESULTIS mk2(op, rnexp(7))
 
  CASE s_fadd:
  CASE s_add:
    RESULTIS rnexp(5)
 
  CASE s_sub:
    a := rnexp(5)
    TEST h1!a=s_number THEN h2!a := - h2!a
                       ELSE a := mk2(s_neg, a)
    RESULTIS a
    
  CASE s_fsub:
    a := rnexp(5)
    a := mk2(s_fneg, a)
    RESULTIS a
 
    CASE s_fabs:
    CASE s_abs:
      RESULTIS mk2(op, rnexp(5))
 
    CASE s_not:
      RESULTIS mk2(s_not, rnexp(3))
 
    CASE s_table:
      lex()
      RESULTIS mk2(s_table, rexplist())
  }
}
 
AND rnexp(n) = VALOF
{ lex()
  RESULTIS rexp(n)
}
 
AND rexp(n) = VALOF
{ LET a, b, p = rbexp(), 0, 0

   UNTIL nlpending DO 
   { LET op = token
 
      SWITCHON op INTO
 
      { DEFAULT:       RESULTIS a
 
         CASE s_lparen: lex()
                        b := 0
                        UNLESS token=s_rparen DO
			{
			  b := rexplist()
			}
                        UNLESS token=s_rparen DO synerr("')' missing")
                        lex()
                        a := mk4(s_fnap, a, b, 0)
                        LOOP
 
         CASE s_sbra:   b := rnexp(0)   // Inserted 11/6/02
                        UNLESS token=s_sket DO synerr("']' missing")
                        lex()
                        a := mk3(s_vecap, a, b)
                        LOOP
 
         CASE s_mthap:{ LET e1 = 0
                        lex()
                        UNLESS token=s_lparen DO synerr("'(' missing")
                        lex()
                        b := 0
                        UNLESS token=s_rparen DO b := rexplist()
                        IF b=0 DO synerr("argument expression missing")
                        UNLESS token=s_rparen DO synerr("')' missing")
                        lex()
                        TEST h1!b=s_comma
                        THEN e1 := h2!b
                        ELSE e1 := b
                        a := mk3(s_vecap, mk2(s_rv, e1), a)
                        a := mk4(s_fnap, a, b, 0)
                        LOOP
                      }
 
         CASE s_of:     p := 8; ENDCASE // Inserted 11/7/01

         CASE s_vecap:  p := 8; ENDCASE
         CASE s_byteap: p := 8; ENDCASE // Changed from 7 on 16 Dec 1999
         CASE s_fmul:
         CASE s_fdiv:
         CASE s_fmod:
         CASE s_mul:
         CASE s_div:
         CASE s_mod:    p := 6; ENDCASE

         CASE s_fadd:
         CASE s_fsub:
         CASE s_add:
         CASE s_sub:    p := 5; ENDCASE
 
         CASE s_feq:CASE s_fle:CASE s_fls:
         CASE s_fne:CASE s_fge:CASE s_fgr:
         CASE s_eq:CASE s_le:CASE s_ls:
         CASE s_ne:CASE s_ge:CASE s_gr:
                        IF n>=4 RESULTIS a
                        b := rnexp(4)
                        a := mk3(op, a, b)
                        WHILE  s_eq<=token<=s_ge |
                               s_feq<=token<=s_fge DO
                        { LET c = b
                           op := token
                           b := rnexp(4)
                           a := mk3(s_logand, a, mk3(op, c, b))
                        }
                        LOOP
 
         CASE s_lshift:
         CASE s_rshift: IF n>=4 RESULTIS a
                        a := mk3(op, a, rnexp(4))
                        LOOP

         CASE s_logand: p := 3; ENDCASE
         CASE s_logor:  p := 2; ENDCASE
         CASE s_eqv:
         CASE s_xor:    p := 1; ENDCASE
 
         CASE s_fcond:
         CASE s_cond:   IF n>=1 RESULTIS a
                        b := rnexp(0)
                        UNLESS token=s_comma DO
                               synerr("Bad conditional expression")
                        a := mk4(op, a, b, rnexp(0))
                        LOOP
      }
      
      IF n>=p RESULTIS a
      a := mk3(op, a, rnexp(p))
   }
   
   RESULTIS a
}

LET rnexplist() = VALOF
{ lex()
  RESULTIS rexplist()
}

LET rexplist() = VALOF
{ LET res, a = 0, rexp(0)
  LET ptr = @res
 
  WHILE token=s_comma DO { !ptr := mk3(s_comma, a, 0)
                           ptr := @h3!(!ptr)
                           a := rnexp(0)
                         }
  !ptr := a
  RESULTIS res
}
 
LET rdef(outerlevel) = VALOF
{ // This is only called after LET or AND.
  // token holds the next token
  LET n = rnamelist() // Read a name or a namelist.
                      // The names may be prefixed by FLT
  LET ln = lineno

  SWITCHON token INTO
 
  { CASE s_lparen: // This must be a fndef or rtdef
      { LET a = 0
        lex()
        // Check that the function has a single name not qualified by FLT.
        UNLESS h1!n=s_name DO synerr("Bad function definition")
	// Read the formal parameters which must be names possibly qualified
	// by FLT and separated by commas.
        IF token=s_name | token=s_flt DO a := rnamelist()
	// a is either zero or a name list

        UNLESS token=s_rparen DO
	  synerr("')' missing at the end of a formal parameter list")
        lex()
 
        IF token=s_be DO
        { lex()
          RESULTIS mk6(s_rtdef, n, a, rcom(), 0, ln)
        }
 
        IF token=s_eq DO
	{ lex()
          a := mk6(s_fndef, n, a, rexp(0), 0, ln)
          RESULTIS a
        }
 
        synerr("Bad procedure heading")
      }

    CASE s_colon:
    { // This must be a function or routine defined using patterns.
      LET mlist = rdmatchlist(0) // Read Ematchlist or Cmatchlist
      LET sort = result2     // =s_yields or s_be
      LET op = sort=s_yields -> s_patfndef, s_patrtdef
      RESULTIS mk5(op, n, mlist, 0, ln)
    }
    
    DEFAULT: synerr("Bad declaration")
 
    CASE s_eq:
        IF outerlevel DO synerr("Bad outer level declaration")
        lex()
        IF token=s_vec DO
        { //IF h1!n=s_flt DO synerr("Vector name must not have the FLT tag")
          UNLESS h1!n=s_name | h1!n=s_flt DO synerr("Name required before = VEC")
            RESULTIS mk4(s_vecdef, n, rnexp(0), ln)
        }
        RESULTIS cvvaldef(n, rexplist(), ln)
  }
}

AND rdmatchlist(sort) = VALOF
{ // sort is s_yields or s_be, or zero if not yet known

  // Returns a match list with result2 set to s_yields or s_be

  // matchlist -> [ matchiteme, Plist, E, link, ln ]
  //           or [ matchitemc, Plist, C, link, ln ]
  // link points to the next match item. if any
  
  // The syntax of a matchlist is either
  //    : Plist => E
  //    ...
  //    : Plist => E
  //    .              The dot may be omitted
  // or
  //    : Plist BE C
  //    ...
  //    : Plist BE C
  //    .              The dot may be omitted

  LET res = rdmatchitem(sort)
  LET lastitem = res
  sort := result2
  WHILE token=s_colon DO
  { LET item = rdmatchitem(sort)
    h4!lastitem := item
    lastitem := item
  }
  IF token = s_dot DO lex() // The final dot may be omitted
  result2 := sort           // Because result2 may have been corrupted
  RESULTIS res
}

AND rdmatchitem(sort) = VALOF
{ // sort is either s_yields or s_be,
  // or zero if not yet known.

  // It returns a pointer to a match item with a null link, ie
  //       [ matchiteme, Plist, E, 0, ln ]
  // or    [ matchitemc, Plist, C, 0, ln ]

  // result2 is set to s_yields or s_be, as appropriate

  LET res = 0
  LET patlist = 0
  LET ln = lineno

  UNLESS token = s_colon DO
    synerr("':' expected at the start of a match item")
  
  lex() // Skip over the colon
//writef("rdmatchitem: calling rdplist(), token = %s*n", opname(token))  

  // Allow an empty pattern list between ':' and '=>' or BE.
  UNLESS token=s_yields | token=s_be DO patlist := rdplist()

  UNLESS token=s_yields | token=s_be DO synerr("=> or BE expected")
  UNLESS sort DO sort := token
  ln := lineno  // The line number of => or BE

  UNLESS sort=token TEST sort=s_yields
                    THEN paterr("'=>' expected")
                    ELSE paterr("BE expected")

  TEST sort=s_yields
  THEN res := mk5(s_matchiteme, patlist, rnexp(0), 0, ln)
  ELSE res := mk5(s_matchitemc, patlist, rncom(),  0, ln)

  result2 := sort
  RESULTIS res
}


// The syntax of patterns is described using syntactic categories P0 to P6
// corresponding to the precedence of the components involved. P0 is the
// lowest precedent category representing a list of patterns separated by
// comma, and P5 is the highest precedent category representing pattern
// constants. These definition of these categories is as follows.

// Plist -> P1           List of patterns matching consecutive locations
//       |  P1 , Plist   of memory

// P1 -> P2              Consecutive set of patterns that are all applied
//    |  P2 P1           to the same location

// P2 -> P3
//    |  non manifest name
//    |  BREAK
//    |  LOOP
//    |  ENDCASE
//    |  NEXT
//    |  EXIT
//    |  ?
//    |  relop P5
//    |  relop ( E )
//    |  [ Plist ]       The Plist must be non null

// P3 -> P4              A list of constants and ranges separated by
//    |  P4 | P3         vertical bars. This pattern is satisfied if any
//                       of its members are satisfied

// P4 -> P5              A constant or a range of constants
//    |  P5 .. P5        Equivalent to (>=P5)(<=P5)

// P5 -> manifest name
//    !  a character constant, treated as an integer constant
//    |  true, treated as -1
//    |  false treated as 0
//    |  + P6
//    |  - P6
//    |  P6

// P6 -> an integer
//    |  a floating point number


AND rdplist() = VALOF
{ // Read a pattern of precedence 0 ie a pattern list.

  LET res1 = rdpat1()
  LET res2 = 0
  IF res1 = 0 DO synerr("A pattern term is expected")
  IF token=s_comma DO
  { lex()
    res2 := rdplist()
    UNLESS res2 DO synerr("A pattern term is expected after a comma")
    res1 := mk3(s_comma, res1, res2)
  }
  RESULTIS res1
}

AND rdpat1() = VALOF
{ // Read a P1 pattern term
  // It returns zero if no pattern term is found.
  
  // P1 -> P2        Consecutive set of P2 patterns all applied
  //    |  P2 P1     to the same location

  LET res1 = rdpat2()
  IF res1 DO
  { LET res2 = rdpat1()
    IF res2 DO res1 := mk3(s_patand, res1, res2)
  }
  RESULTIS res1
}

AND rdpat2() = VALOF
{ // Read a P2 pattern term
  // It returns zero if no pattern term is found.

  // P2 -> P3
  //    |  non manifest name
  //    |  BREAK
  //    |  LOOP
  //    |  ENDCASE
  //    |  NEXT
  //    |  EXIT
  //    |  ?
  //    |  relop ( E )
  //    |  relop name
  //    |  relop P5
  //    |  [ Plist ]

  LET res = rdpat3()
  
  IF res RESULTIS res

  SWITCHON token INTO
  { DEFAULT:
      RESULTIS 0

    CASE s_break:
    CASE s_loop:
    CASE s_endcase:
    CASE s_next:
    CASE s_exit:
      RESULTIS rbcom()

    CASE s_query:
      lex()
      RESULTIS mk1(s_query)
      

    CASE s_eq:    CASE s_feq:
    CASE s_ne:    CASE s_fne:
    CASE s_le:    CASE s_fle:
    CASE s_ge:    CASE s_fge:
    CASE s_ls:    CASE s_fls:
    CASE s_gr:    CASE s_fgr:
    { LET relop = rel2patrel(token)
      lex()
      IF token=s_lparen DO
      { res := mk2(relop, rnexp(0))                // relop ( E )
        UNLESS token=s_rparen DO
          synerr("The ')' seems to be missing after '(' relop E")
        lex() // Skip over the ')'
        RESULTIS res
      }
      IF token=s_name RESULTIS mk2(relop, rname())  // relop name

      RESULTIS mk2(relop, rdpat5())                 // relop P5
    }
   
    CASE s_sbra:
      lex()
      UNLESS token=s_sket DO res := rdplist()
      UNLESS token=s_sket DO
        synerr("The ']' seems to be missing after '[' pattern-list")
      lex() // Skip over the ']'
      RESULTIS mk3(s_patptr, res)
  }
    
  RESULTIS res
}

AND rdpat3() = VALOF
{ // Read a P3 pattern term
  // It returns zero if no pattern term is found.

  // P3 -> P4              A list of constants and ranges separated by
  //    |  P4 | P3         vertical bars. This pattern is satisfied if any
  //                       of its members are satisfied

  LET res1 = rdpat4()
  LET res2 = 0
  IF res1 = 0 RESULTIS 0
  
  IF token=s_logor DO
  { lex()
    res2 := rdpat3()
    UNLESS res2 DO
      synerr("A constant or range expected after a vertical bar in a pattern") 
    res1 := mk3(s_pator, res1, res2)
  }
  RESULTIS res1
}

AND rdpat4() = VALOF
{ // Read a P4 pattern
  // It returns zero if no pattern term is found.

  // P4 -> P5              A constant or a range of constants
  //    |  P5 .. P5        Equivalent to (>=P5)(<=P5)
  //    |  P5 #.. P5       Equivalent to (#>=P5)(#<=P5)


  LET res1 = rdpat5()
  LET res2 = 0
  LET op = token
  IF res1 = 0 RESULTIS 0
  IF op=s_range | op=s_frange DO
  { lex()
    res2 := rdpat5()
    UNLESS res2 DO
      synerr("A constant expected after '..' in a pattern") 
    res1 := mk3(op, res1, res2)
  }
  RESULTIS res1
}

AND rdpat5() = VALOF
{ // Read a P5 pattern Term
  // It returns zero if no pattern term is found.

// P5 -> manifest name
//    !  a character constant, treated as an integer constant
//    |  true, treated as -1
//    |  false treated as 0
//    |  + P6
//    |  - P6
//    |  P6

  LET res = 0
//writef("rdpat5: token=%s*n", opname(token))

  SWITCHON token INTO
  { DEFAULT:
      RESULTIS rdpat6()

    CASE s_flt:
    CASE s_name:
      RESULTIS rname()
      
    CASE s_true:
      lex()
      RESULTIS mk2(s_number, -1)

    CASE s_false:
      lex()
      RESULTIS mk2(s_number, 0)

    CASE s_add: // Monadic plus
      lex()
      RESULTIS rdpat6()  //eg  +12  +'A'  +1.0

    CASE s_sub:          //eg  -12  -'A'  -1.0
      lex()
      res := rdpat6()
      IF res DO
      { IF h1!res=s_number DO { h2!res :=  - h2!res; RESULTIS res }
        IF h1!res=s_fnum   DO { h2!res := #- h2!res; RESULTIS res }
      }
      synerr("A numerical constant expected in a pattern")
      RESULTIS res
  }
  // This point should not be reached
  RESULTIS res
}

AND rdpat6() = VALOF
{ // Read a P6 pattern Term
  // It returns zero if no P6 term is found.

  // P6 -> an integer or floating point number

  LET res = 0

  SWITCHON token INTO
  { DEFAULT:
      RESULTIS 0 // A P6 must be an integer or a floating point number.

    CASE s_number:             // This includes character constants
      res := mk2(s_number, decval)
      lex()
      RESULTIS res

    CASE s_fnum:
      UNLESS -128<=exponent<=127 DO
        synerr("Exponent of floating point constant out of range")
      floatingchk()
      res := mk2(s_fnum, fltval)
      lex()
      RESULTIS res
  }
  // This point should not be reached
  RESULTIS res
}

AND cvvaldef(lhs, rhs, ln) = VALOF // Added on 28/11/2018
{ // This converts a simultaneous declaration of local
  // variables to a collection of simple declarations connected
  // by ANDs. This is done for the benefit of the FLT feature.
  // The mk3 nodes used for the comma lists are returned to
  // freestore using unmk3.

  LET res = 0
  LET ptr = @res
 
  WHILE h1!lhs=s_comma & h1!rhs=s_comma DO
  { LET a = mk4(s_valdef, h2!lhs, h2!rhs, ln)
    !ptr := mk3(s_and, a, 0)
    ptr := @h3!(!ptr)
    unmk3(lhs) // Return the comma nodes to free store.
    unmk3(rhs) // These only change h1!lhs and h1!rhs
    lhs, rhs := h3!lhs, h3!rhs
  }
  IF h1!lhs=s_comma | h1!rhs=s_comma DO
    synerr("Lhs and rhs do not match")

  !ptr := mk4(s_valdef, lhs, rhs, ln)
  RESULTIS res
}
 
AND cvassign(assop, lhs, rhs, ln) = VALOF // Added on 28/11/2018
{ // This converts a simultaneous assignment to a sequence of
  // simple assignments connected by s_seq. Each simple assigment
  // is given the assignment operator assop.
  // This is done for the benefit of the FLT feature.
  // The mk3 nodes used for the comma lists are returned to
  // freestore using unmk3.

  LET res = 0
  LET ptr = @res
 
  WHILE h1!lhs=s_comma & h1!rhs=s_comma DO
  { LET a = mk4(assop, h2!lhs, h2!rhs, ln)
    !ptr := mk3(s_seq, a, 0)
    ptr := @h3!(!ptr)
    unmk3(lhs) // Return the comma nodes to free store.
    unmk3(rhs) // These only change h1!lhs and h1!rhs
    lhs, rhs := h3!lhs, h3!rhs
  }
  IF h1!lhs=s_comma | h1!rhs=s_comma DO
    synerr("Lhs and rhs do not match")

  !ptr := mk4(assop, lhs, rhs, ln)
  RESULTIS res
}
 
AND rbcom() = VALOF
{ LET a, b, ln = 0, 0, lineno
  LET op = token
 
  SWITCHON op INTO
  { DEFAULT: RESULTIS 0
 
    CASE s_name:CASE s_number:CASE s_fnum:
    CASE s_string:CASE s_lparen:
    CASE s_true:CASE s_false:CASE s_lv:CASE s_rv:CASE s_vecap:
    CASE s_slct:        // Inserted 11/7/01
    CASE s_add:CASE s_sub:CASE s_abs:CASE s_not:
    CASE s_fadd:CASE s_fsub:CASE s_fabs:CASE s_fix:CASE s_float:
    CASE s_table:CASE s_valof:CASE s_query:
      // All these tokens that can start an expression,
      // but not allowing MATCH or EVERY which will be commands
      // not expressions.
      a := rexplist()
      op := token
 
      SWITCHON op INTO
      { DEFAULT:
          IF h1!a=s_fnap DO
          { h1!a, h4!a := s_rtap, ln
            RESULTIS a
          }
          synerr("Error in command")
          RESULTIS a

        CASE s_ass:      CASE s_fass:
        CASE s_assvecap:
        CASE s_assfmul:  CASE s_assfdiv: CASE s_assfmod:
        CASE s_assfadd:  CASE s_assfsub:
        CASE s_assmul:   CASE s_assdiv:  CASE s_assmod:
        CASE s_assadd:   CASE s_asssub:
        CASE s_asslshift:CASE s_assrshift:
        CASE s_asslogand:CASE s_asslogor:
        CASE s_asseqv:   CASE s_assxor:
          lex()
          // Replace a simultaneous assignment by a
          // sequence of simple assigments.
          RESULTIS cvassign(op, a, rexplist(), ln)

        CASE s_colon:
	  // Note: A name declared as a label cannot have a
	  // FLT prefix.
          UNLESS h1!a=s_name DO synerr("Unexpected ':'")
          lex()
          RESULTIS mk5(op, a, rbcom(), 0, ln)
      }

    CASE s_match:
    CASE s_every:
    { LET args = 0
      LET b = 0
      LET ln = lineno
      lex()
      UNLESS token=s_lparen DO synerr("'(' expected after MATCH")
      UNLESS token=s_rparen DO args := rnexplist() // Allow () ...
      UNLESS token=s_rparen DO
      synerr("')' missing at the end of the MATCH argument list")
      lex() 
      RESULTIS mk4(op=s_match -> s_matchc, s_everyc,
                   args, rdmatchlist(s_be), ln)
    }

    CASE s_goto:
    CASE s_resultis:
      RESULTIS mk3(op, rnexp(0), ln)
 
    CASE s_if:
    CASE s_unless:
    CASE s_while:
    CASE s_until:
      a := rnexp(0)
      IF token=s_do DO lex()
      RESULTIS mk4(op, a, rcom(), ln)
 
    CASE s_test:
      a := rnexp(0)
      IF token=s_do DO lex()
      b := rcom()
      UNLESS token=s_else DO synerr("ELSE missing")
      lex()
      RESULTIS mk5(s_test, a, b, rcom(), ln)
 
    CASE s_for:
    { LET i, j, k = 0, 0, 0
      lex()
      a := rname()
      UNLESS token=s_eq DO synerr("'=' missing")
      i := rnexp(0)
      //UNLESS token=s_to DO synerr("TO missing") // Changed 20/08/2021
      IF token=s_to DO j := rnexp(0)
      IF token=s_by DO k := rnexp(0)
      IF token=s_do DO lex()
      RESULTIS mk7(s_for, a, i, j, k, rcom(), ln)
    }
 
    CASE s_loop:
    CASE s_break:
    CASE s_next:   // MR 28/08/2021
    CASE s_exit:   // MR 28/08/2021
    CASE s_return:
    CASE s_finish:
    CASE s_endcase:
      lex()
      RESULTIS mk2(op, ln)
 
    CASE s_switchon:
      a := rnexp(0)
      UNLESS token=s_into DO synerr("INTO missing")
      lex()
      { LET skipln = lineno
        b := rdsect(rdseq)
        UNLESS b DO
        b := mk2(s_skip, skipln)         // MR 5/4/06
      }
      RESULTIS mk4(s_switchon, a, b, ln)
 
    CASE s_case:
      a := rnexp(0)
      UNLESS token=s_colon DO synerr("Bad CASE label")
      lex()
      RESULTIS mk4(s_case, a, rbcom(), ln)
 
    CASE s_default:
      lex()
      UNLESS token=s_colon DO synerr("Bad DEFAULT label")
      lex()
      RESULTIS mk3(s_default, rbcom(), ln)
 
    CASE s_lsect:
      a := rdsect(rdblockbody, FALSE)
      UNLESS a DO
        a := mk2(s_skip, ln)        // MR 5/4/06
      RESULTIS a
  }
}

AND rbseq() = VALOF
{ LET a = rbcom()
  WHILE token=s_seq DO
  { LET ln = lineno
    lex()
    a := mk4(s_seq, a, rbcom(), ln)
  }
}

AND rncom() = VALOF
{ lex()
  RESULTIS rcom()
}

AND rcom() = VALOF // Added <> 18/07/2010
// Reads:  BCOM <> BCOM <>...<> BCOM 
// possibly qualified by repeat, repeatwhile or repeatuntil clauses
{ LET a = rbcom()
 
  // Empty section brackets { } form SKIP nodes, MR 22/6/05
  IF a=0 DO synerr("Error in command")
 
  WHILE token=s_seq DO
  { LET ln = lineno
    lex()
    a := mk4(s_seq, a, rbcom(), ln)
  }

  WHILE token=s_repeat | token=s_repeatwhile | token=s_repeatuntil DO
  { LET op, ln = token, lineno
    UNLESS op=s_repeat { a := mk4(op, a, rnexp(0), ln); LOOP }
    a := mk3(op, a, ln)
    lex()
  }
 
  RESULTIS a
}

LET plist1(x) BE // Rename plist1 and plist to debug the hash table
{ writef("*nName table contents, size = %n*n", nametablesize)
   FOR i = 0 TO nametablesize-1 DO
   { LET p, n = nametable!i, 0
      WHILE p DO p, n := p!1, n+1
      writef("%i3:%n", i, n)
      p := nametable!i
      WHILE p DO { writef(" %s", p+2); p := p!1  }
      newline()
   }
}

LET plist(x, n, d) BE
{ LET size, ln = 0, 0
  LET v = TABLE 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

  IF x=0 DO { writes("Nil"); RETURN  }
 //writef("%n -> [ %n %n %n ] ", x, h1!x, h2!x, h3!x)
  SWITCHON h1!x INTO
  { CASE s_number:
                 { // x -> [ number, val ]
		   LET val = h2!x
                   TEST -1000000<=val<=1000000
                   THEN writef("NUMBER: %n", val)
                   ELSE writef("NUMBER: #x%x8", val)
                   RETURN
                 }

    CASE s_fnum:   // x -> [ fnum, fval ]
                   writef("FNUM: %14.6f", h2!x); RETURN

    CASE s_name:   // x -> [ name, -, <name> ]
                   writef("NAME: %s", x+2); RETURN
 
    CASE s_string: // x -> [ string, <string> ]
                { LET s = x+1
                  writef("STRING: *"")
                  FOR i = 1 TO s%0 SWITCHON s%i INTO
                  { DEFAULT:   wrch(s%i); LOOP
                    CASE '*n': writes("**n"); LOOP
                    CASE '*p': writes("**p"); LOOP
                    CASE '*s': writes("**s"); LOOP
                    CASE '*t': writes("**t"); LOOP
                  }
                  writes("*"")
                  RETURN
                }
 
      CASE s_for:    // x -> [ for, name, E, E, E, C, ln ]
                     //   |  [ for, name, E, E, 0, C, ln ]
                     //   |  [ for, name, E, 0, E, C, ln ]
                     //   |  [ for, name, E, 0, 0, C, ln ]
                     size, ln := 6, h7!x;  ENDCASE
 
      CASE s_fndef:  // x ->  [ fndef, name, Nlist, E, -, ln ]
      CASE s_rtdef:  //   |   [ rtdef, name, Nlist, C, -, ln ]
                     size, ln := 4, h6!x;  ENDCASE

      CASE s_patfndef:  // x ->  [ patfndef, name, Ematchlist, -, ln ]
      CASE s_patrtdef:  //   |   [ patrtdef, name, Cmatchlist, -, ln ]
                     size, ln := 3, h5!x;  ENDCASE

      CASE s_matchiteme:  // x ->  [ ematchiteme, plist, E, link, ln ]
      CASE s_matchitemc:  // x ->  [ cmatchitemc, plist, C, link, ln ]
                     size, ln := 4, h5!x;  ENDCASE

      CASE s_fcond:
      CASE s_cond:
      CASE s_slct:       // Inserted 11/7/01
                     size := 4;            ENDCASE
 
      CASE s_test:CASE s_constdef:
                     size, ln := 4, h5!x;  ENDCASE
 
      CASE s_needs:CASE s_section:CASE s_vecap:CASE s_byteap:CASE s_fnap:
      CASE s_of:  // Inserted 11/7/01
      CASE s_fmul:CASE s_fdiv:CASE s_fmod:
      CASE s_fadd:CASE s_fsub:
      CASE s_mul:CASE s_div:CASE s_mod:CASE s_add:CASE s_sub:
      CASE s_feq:CASE s_fne:CASE s_fls:CASE s_fgr:CASE s_fle:CASE s_fge:
      CASE s_eq:CASE s_ne:CASE s_ls:CASE s_gr:CASE s_le:CASE s_ge:
      CASE s_lshift:CASE s_rshift:CASE s_logand:CASE s_logor:
      CASE s_eqv:CASE s_xor:CASE s_comma:
      CASE s_patand:CASE s_pator:CASE s_range:CASE s_frange:
      CASE s_seq:
                     size := 3;            ENDCASE
                     
      CASE s_valdef:CASE s_vecdef:
                     size, ln := 3, h4!x;  ENDCASE

      CASE s_colon:
                     size, ln := 3, h5!x;  ENDCASE
 
      CASE s_and:
      CASE s_ass:CASE s_fass:
      CASE s_assvecap:
      CASE s_assfmul:  CASE s_assfdiv: CASE s_assfmod:
      CASE s_assfadd:  CASE s_assfsub:
      CASE s_assmul:   CASE s_assdiv:  CASE s_assmod:
      CASE s_assadd:   CASE s_asssub:
      CASE s_asslshift:CASE s_assrshift:
      CASE s_asslogand:CASE s_asslogor:
      CASE s_asseqv:   CASE s_assxor:
      CASE s_rtap:CASE s_if:CASE s_unless:
      CASE s_while:CASE s_until:CASE s_repeatwhile:
      CASE s_repeatuntil:
      CASE s_switchon:CASE s_case:CASE s_let:
      CASE s_manifest:CASE s_static:CASE s_global:
      CASE s_matche:CASE s_matchc:
      CASE s_everye:CASE s_everyc:
                     size, ln := 3, h4!x;  ENDCASE
 
      CASE s_valof:CASE s_lv:CASE s_rv:CASE s_neg:CASE s_not:
      CASE s_table:CASE s_abs:
      CASE s_fabs:CASE s_fneg:CASE s_fix:CASE s_float:
      CASE s_flt:
      CASE s_patptr:  
      CASE s_patfeq:CASE s_patfne:CASE s_patfls:
      CASE s_patfgr:CASE s_patfle:CASE s_patfge:
      CASE s_pateq:CASE s_patne:CASE s_patls:
      CASE s_patgr:CASE s_patle:CASE s_patge:
                     size := 2;            ENDCASE
 
      CASE s_goto:CASE s_resultis:CASE s_repeat:CASE s_default:
                     size, ln := 2, h3!x;  ENDCASE
 
      CASE s_true:CASE s_false:CASE s_query:
                     size := 1;            ENDCASE
      
      CASE s_break:
      CASE s_loop:
      CASE s_endcase:
      CASE s_next: // MR 28/08/2021
      CASE s_exit: // MR 28/08/2021
      CASE s_return:
      CASE s_skip: // MR 22/06/2005
      CASE s_finish:
                     size, ln := 1, h2!x;  ENDCASE

      DEFAULT:       size := 1
   }
 
   IF n=d DO { writes("Etc"); RETURN }
 
//   writef("Op %n", h1!x)
   writef(opname(h1!x), h1!x)
//   abort(6111)
// IF ln>0 DO writef("  line %n", ln)
   IF ln>0 DO
   { LET fno = ln>>20
     LET lno = ln & #xFFFFF
     LET filename = sourcenamev!fno
     writef("  ")
     IF filename DO writef("%s", filename)
     writef("[%n]", lno)
   }
   FOR i = 2 TO size DO { newline()
			  //abort(7111)
			  //writef("n=%n size=%n %n -> [%n %n %n]*n",
			  //       n, size, v, v!0, v!1, v!2)
			  //abort(7112)
                          FOR j=0 TO n-1 DO writes( v!j )
                          writes("**-")
                          v!n := i=size->"  ","! "
                          plist(h1!(x+i-1), n+1, d)
                        }
}
 
AND opname(op) = VALOF SWITCHON op INTO
{ DEFAULT:            writef("*nUnknown opname = %n*n", op)
                      RESULTIS "Unknown op"

  CASE s_abs:         RESULTIS "ABS"
  CASE s_and:         RESULTIS "AND"
  CASE s_ass:         RESULTIS "ASS"
  CASE s_assdiv:      RESULTIS "ASSDIV"
  CASE s_asseqv:      RESULTIS "ASSEQV"
  CASE s_assfdiv:     RESULTIS "ASSFDIV"
  CASE s_assfmod:     RESULTIS "ASSFMOD"
  CASE s_assfsub:     RESULTIS "ASSFSUB"
  CASE s_assfmul:     RESULTIS "ASSFMUL"
  CASE s_assfadd:     RESULTIS "ASSFADD"
  CASE s_asslogand:   RESULTIS "ASSLOGAND"
  CASE s_asslogor:    RESULTIS "ASSLOGOR"
  CASE s_asslshift:   RESULTIS "ASSLSHIFT"
  CASE s_asssub:      RESULTIS "ASSSUB"
  CASE s_assmul:      RESULTIS "ASSMUL"
  CASE s_assxor:      RESULTIS "ASSXOR"
  CASE s_assadd:      RESULTIS "ASSADD"
  CASE s_assmod:      RESULTIS "ASSMOD"
  CASE s_assrshift:   RESULTIS "ASSRSHIFT"
  CASE s_assvecap:    RESULTIS "ASSVECAP"
  CASE s_be:          RESULTIS "BE"
  CASE s_by:          RESULTIS "BY"
  CASE s_break:       RESULTIS "BREAK"
  CASE s_byteap:      RESULTIS "BYTEAP"
  CASE s_case:        RESULTIS "CASE"
  CASE s_colon:       RESULTIS "COLON"
  CASE s_comma:       RESULTIS "COMMA"
  CASE s_cond:        RESULTIS "COND"
  CASE s_constdef:    RESULTIS "CONSTDEF"
  CASE s_datalab:     RESULTIS "DATALAB"
  CASE s_default:     RESULTIS "DEFAULT"
  CASE s_div:         RESULTIS "DIV"
  CASE s_do:          RESULTIS "DO"
  CASE s_dot:         RESULTIS "DOT"
  CASE s_else:        RESULTIS "ELSE"
  CASE s_eof:         RESULTIS "EOF"
  CASE s_endcase:     RESULTIS "ENDCASE"
  CASE s_endfor:      RESULTIS "ENDFOR"
  CASE s_endproc:     RESULTIS "ENDPROC"
  CASE s_entry:       RESULTIS "ENTRY"
  CASE s_eq:          RESULTIS "EQ"
  CASE s_eqv:         RESULTIS "EQV"
  CASE s_every:       RESULTIS "EVERY"
  CASE s_everyc:      RESULTIS "EVERYC"
  CASE s_everye:      RESULTIS "EVERYE"
  CASE s_exit:        RESULTIS "EXIT"
  CASE s_fabs:        RESULTIS "FABS"
  CASE s_fadd:        RESULTIS "FADD"
  CASE s_false:       RESULTIS "FALSE"
  CASE s_fass:        RESULTIS "FASS"
  CASE s_fcond:       RESULTIS "FCOND"
  CASE s_fdiv:        RESULTIS "FDIV"
  CASE s_feq:         RESULTIS "FEQ"
  CASE s_fge:         RESULTIS "FGE"
  CASE s_fgr:         RESULTIS "FGR"
  CASE s_fglobal:     RESULTIS "FGLOBAL"
  CASE s_finish:      RESULTIS "FINISH"
  CASE s_fix:         RESULTIS "FIX"
  CASE s_fle:         RESULTIS "FLE"
  CASE s_float:       RESULTIS "FLOAT"
  CASE s_flocal:      RESULTIS "FLOCAL"
  CASE s_flt:         RESULTIS "FLT"
  CASE s_fls:         RESULTIS "FLS"
  CASE s_fltop:       RESULTIS "FLTOP"
  CASE s_fmanifest:   RESULTIS "FMANIFEST"
  CASE s_fmod:        RESULTIS "FMOD"
  CASE s_fnap:        RESULTIS "FNAP"
  CASE s_fnrn:        RESULTIS "FNRN"
  CASE s_fndef:       RESULTIS "FNDEF"
  CASE s_fne:         RESULTIS "FNE"
  CASE s_fneg:        RESULTIS "FNEG"
  CASE s_fnum:        RESULTIS "FNUM"
  CASE s_fmul:        RESULTIS "FMUL"
  CASE s_fpath1:      RESULTIS "FPATH1"
  CASE s_fpath2:      RESULTIS "FPATH2"
  CASE s_fpath3:      RESULTIS "FPATH3"
  CASE s_fpath4:      RESULTIS "FPATH4"
  CASE s_frange:      RESULTIS "FRANGE"

  CASE s_fstatic:     RESULTIS "FSTATIC"
  CASE s_fsub:        RESULTIS "FSUB"

  CASE s_for:         RESULTIS "FOR"
  CASE s_ge:          RESULTIS "GE"
  CASE s_get:         RESULTIS "GET"
  CASE s_getbyte:     RESULTIS "GETBYTE"
  CASE s_global:      RESULTIS "GLOBAL"
  CASE s_goto:        RESULTIS "GOTO"
  CASE s_gr:          RESULTIS "GR"
  CASE s_if:          RESULTIS "IF"
  CASE s_into:        RESULTIS "INTO"
  CASE s_itemn:       RESULTIS "ITEMN"
  CASE s_jf:          RESULTIS "JF"
  CASE s_jt:          RESULTIS "JT"
  CASE s_jump:        RESULTIS "JUMP"
  CASE s_lab:         RESULTIS "LAB"
  CASE s_le:          RESULTIS "LE"
  CASE s_let:         RESULTIS "LET"
  CASE s_lf:          RESULTIS "LF"
  CASE s_lg:          RESULTIS "LG"
  CASE s_ll:          RESULTIS "LL"
  CASE s_llg:         RESULTIS "LLG"
  CASE s_lll:         RESULTIS "LLl"
  CASE s_llp:         RESULTIS "LLP"
  CASE s_ln:          RESULTIS "LN"
  CASE s_local:       RESULTIS "LOCAL"
  CASE s_logand:      RESULTIS "LOGAND"
  CASE s_logor:       RESULTIS "LOGOR"
  CASE s_loop:        RESULTIS "LOOP"
  CASE s_lp:          RESULTIS "LP"
  CASE s_lparen:      RESULTIS "LPAREN"
  CASE s_ls:          RESULTIS "LS"
  CASE s_lsect:       RESULTIS "LSECT"
  CASE s_lshift:      RESULTIS "LSHIFT"
  CASE s_lstr:        RESULTIS "LSTR"
  CASE s_lv:          RESULTIS "LV"
  CASE s_manifest:    RESULTIS "MANIFEST"
  CASE s_match:       RESULTIS "MATCH"
  CASE s_matchc:      RESULTIS "MATCHC"
  CASE s_matche:      RESULTIS "MATCHE"
  CASE s_matchitemc:  RESULTIS "MATCHITEMC"
  CASE s_matchiteme:  RESULTIS "MATCHITEME"
  CASE s_mthap:       RESULTIS "MTHAP"
  CASE s_mul:         RESULTIS "MUL"
  CASE s_name:        RESULTIS "NAME"
  CASE s_ne:          RESULTIS "NE"
  CASE s_needs:       RESULTIS "NEEDS"
  CASE s_neg:         RESULTIS "NEG"
  CASE s_next:        RESULTIS "NEXT"
  CASE s_none:        RESULTIS "NONE"
  CASE s_not:         RESULTIS "NOT"
  CASE s_number:      RESULTIS "NUMBER"
  CASE s_of:          RESULTIS "OF"
  CASE s_add:         RESULTIS "ADD"
  CASE s_patfndef:    RESULTIS "PATFNDEF"
  CASE s_patrtdef:    RESULTIS "PATRTDEF"
  CASE s_patptr:      RESULTIS "PATPTR"

  CASE s_pateq:       RESULTIS "PATEQ"
  CASE s_patne:       RESULTIS "PATNE"
  CASE s_patls:       RESULTIS "PATLS"
  CASE s_patgr:       RESULTIS "PATGR"
  CASE s_patle:       RESULTIS "PATLE"
  CASE s_patge:       RESULTIS "PATGE"

  CASE s_patfeq:      RESULTIS "PATFEQ"
  CASE s_patfne:      RESULTIS "PATFNE"
  CASE s_patfls:      RESULTIS "PATFLS"
  CASE s_patfgr:      RESULTIS "PATFGR"
  CASE s_patfle:      RESULTIS "PATFLE"
  CASE s_patfge:      RESULTIS "PATFGE"

  CASE s_path1:       RESULTIS "PATH1"
  CASE s_path2:       RESULTIS "PATH2"
  CASE s_path3:       RESULTIS "PATH3"
  CASE s_path4:       RESULTIS "PATH4"

  CASE s_patand:      RESULTIS "PATAND"
  CASE s_pator:       RESULTIS "PATOR"

  CASE s_putbyte:     RESULTIS "PUTBYTE"
  CASE s_query:       RESULTIS "QUERY"
  CASE s_mod:         RESULTIS "MOD"
  CASE s_range:       RESULTIS "RANGE"
  CASE s_repeat:      RESULTIS "REPEAT"
  CASE s_repeatuntil: RESULTIS "REPEATUNTIL"
  CASE s_repeatwhile: RESULTIS "REPEATWHILE"
  CASE s_res:         RESULTIS "RES"
  CASE s_resultis:    RESULTIS "RESULTIS"
  CASE s_return:      RESULTIS "RETURN"
  CASE s_rparen:      RESULTIS "RPAREN"
  CASE s_rsect:       RESULTIS "RSECT"
  CASE s_rshift:      RESULTIS "RSHIFT"
  CASE s_rstack:      RESULTIS "RSTACK"
  CASE s_rtap:        RESULTIS "RTAP"
  CASE s_rtdef:       RESULTIS "RTDEF"
  CASE s_rtrn:        RESULTIS "RTRN"
  CASE s_rv:          RESULTIS "RV"
  CASE s_save:        RESULTIS "SAVE"
  CASE s_sbra:        RESULTIS "SBRA"
  CASE s_section:     RESULTIS "SECTION"
  CASE s_semicolon:   RESULTIS "SEMICOLON"
  CASE s_seq:         RESULTIS "SEQ"
  CASE s_sg:          RESULTIS "SG"
  CASE s_sket:        RESULTIS "SKET"
  CASE s_skip:        RESULTIS "SKIP"
  CASE s_sl:          RESULTIS "SL"
  CASE s_slct:        RESULTIS "SLCT"
  CASE s_selld:       RESULTIS "SELLD"
  CASE s_selst:       RESULTIS "SELST"
  CASE s_sp:          RESULTIS "SP"
  CASE s_stack:       RESULTIS "STACK"
  CASE s_static:      RESULTIS "STATIC"
  CASE s_stind:       RESULTIS "STIND"
  CASE s_store:       RESULTIS "STORE"
  CASE s_string:      RESULTIS "STRING"
  CASE s_sub:         RESULTIS "SUB"
  CASE s_switchon:    RESULTIS "SWITCHON"
  CASE s_table:       RESULTIS "TABLE"
  CASE s_test:        RESULTIS "TEST"
  CASE s_to:          RESULTIS "TO"
  CASE s_true:        RESULTIS "TRUE"
  CASE s_unless:      RESULTIS "UNLESS"
  CASE s_until:       RESULTIS "UNTIL"
  CASE s_valdef:      RESULTIS "VALDEF"
  CASE s_valof:       RESULTIS "VALOF"
  CASE s_vec:         RESULTIS "VEC"
  CASE s_vecap:       RESULTIS "VECAP"
  CASE s_vecdef:      RESULTIS "VECDEF"
  CASE s_while:       RESULTIS "WHILE"
  CASE s_yields:      RESULTIS "YIELDS"
  CASE s_xor:         RESULTIS "XOR"
}

AND flopname(flop) = VALOF SWITCHON flop INTO
{ DEFAULT:            writef("*nUnknown flopname = %n*n", flop)
                      abort(999)
                      RESULTIS "Flop %n"

  CASE fl_mk:         RESULTIS "MK"
  CASE fl_float:      RESULTIS "FLOAT"
  CASE fl_fix:        RESULTIS "FIX"
  CASE fl_neg:        RESULTIS "NEG"
  CASE fl_abs:        RESULTIS "ABS"
  CASE fl_mul:        RESULTIS "MUL"
  CASE fl_mod:        RESULTIS "MOD"
  CASE fl_div:        RESULTIS "DIV"
  CASE fl_add:        RESULTIS "ADD"
  CASE fl_sub:        RESULTIS "SUB"
  CASE fl_eq:         RESULTIS "EQ"
  CASE fl_ne:         RESULTIS "NE"
  CASE fl_ls:         RESULTIS "LS"
  CASE fl_gr:         RESULTIS "GR"
  CASE fl_le:         RESULTIS "LE"
  CASE fl_ge:         RESULTIS "GE"
}

AND sfname(sfop) = VALOF SWITCHON sfop INTO
{ DEFAULT:       writef("sfname: bad sfop = %n*n", sfop)
                 RESULTIS "UNKNOWN"

  CASE sf_none:   RESULTIS "NONE"
  CASE sf_vecap:  RESULTIS "VECAP"
  CASE sf_fmul:   RESULTIS "FMUL"
  CASE sf_fdiv:   RESULTIS "FDIV"
  CASE sf_fmod:   RESULTIS "FMOD"
  CASE sf_fadd:   RESULTIS "FADD"
  CASE sf_fsub:   RESULTIS "FSUB"
  CASE sf_mul:    RESULTIS "MUL"
  CASE sf_div:    RESULTIS "DIV"
  CASE sf_mod:    RESULTIS "MOD"
  CASE sf_add:    RESULTIS "ADD"
  CASE sf_sub:    RESULTIS "SUB"
  CASE sf_lshift: RESULTIS "LSHIFT"
  CASE sf_rshift: RESULTIS "RSHIFT"
  CASE sf_logand: RESULTIS "LOGAND"
  CASE sf_logor:  RESULTIS "LOGOR"
  CASE sf_eqv:    RESULTIS "EQV"
  CASE sf_xor:    RESULTIS "XOR"
}

