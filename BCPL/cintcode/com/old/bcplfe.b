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

  bigender := (!"A" & 255) ~= 1    // =TRUE if currently running
                                   // on a bigender m/c
  // Note that "A" points to the word
  //            00006101          in 32 bit little ender code
  //            0000000000006101  in 64 bit little ender code
  //            01510000          in 32 bit big ender code
  //            0161000000000000  in 64 bit big ender code
  
  t16, t32, t64, wordbytelen, wordbitlen := FALSE, TRUE, FALSE, 4, 32
  // T32 is the default setting
  
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

.

// Althought SYN and TRN were combined into one section, they have now
// been separated because the combine section became to large to Cintcode.

SECTION "TRN"

GET "libhdr"
GET "bcplfecg"

// Global declarations for TRN

GLOBAL  {
trnext:trng
trans; destlabel; declnames; decldyn
genjumpotodest
declstat; checkdistinct; addname; cellwithname
transdef; scanlabel
transmatchlist
transnext; transexit
decllabels; undeclare
jumpcond; transswitch; transfor
assop2op; op2sfop; cv2flt; rel2patrel; patrel2rel
assign; load; fnbody; loadlv; loadlist
isflt; isconst; iszero; evalconst; transname; xref
genlab; labnumber
newblk
dvec; dvece; dvecp; dvect
caselist      // Linked list of case constants of the current switch
casecount     // If =-1 CASE and DEFAULT labels are illegal,
              // but ENDCASE may be allowed, eg in a loop in a switch.
		    
endcaselab    // If =-2 ENDCASE are illegal
              // If =-1 ENDCASE compiles as RTRN
	      // If >=0 ENDCASE compiles as jump endcaselab,
	      // allocating the label if necessary.
	      
defaultlab    // If casecount<0 DEFAuLT is illegal
              // If in a switch defaultlab will be a an
	      // allocated label.

resultlab     // =-2 RESULTIS illegal
              // RESULTIS is legal in repetitive commands within
	      // the current function.
              // If =-1 RESULTIS compiles as
	      // load the result followed by FNRN
	      // This is used when a VALOF expression is the body
	      // of a function.
	      // If >=0 RESULTIS compiles as
	      // load the result followed by RES resultlab
	      // allocating resultlab if necessary.
	      // If the matchcontext is s_patfndef, match item
	      // expressions are treated as operands of RESULTIS.

looplab       // If =-2 LOOP is illegal
              // If >=0 LOOP copiles as a jump
	      // allocating looplab if necessary.

breaklab      // If =-2 BREAK illegal
              // If =-1 BREAK compiles as RTRN
	      // If >=0 BREAK compiles as a jump
	      // allocating breaklab if necessary.

nextlab       // Label of the next match item, if any
              // If =-2 NEXT is illegal
              // If >=0 NEXT compiles as a jump
	      // allocating nextlab if necessary.
	
exitlab       // Label for the end of a match list
              // If =-2 EXIT is illegal
              // If >=0 EXIT compiles as a jump
	      // allocating exitlab if necessary.

context; comline; procname
matchcontext // Equals s_patfndef, s_patrtdef,
             //        s_matche, s_matche, s_everye, s_everyc,
	     // otherwise zero
patresultpos // Position of the result of an MATCH and EVERY expressions.
ssp; vecssp
gdeflist; gdefcount
outstring; out1; out2; out3; out4
floatingchk    // Made a global to avoid relative address out of range error

lasttrnglobal // Used to check for global overlap with cgg

}


LET genlab() = VALOF
{ labnumber := labnumber + 1
  RESULTIS labnumber
}
 
AND trnerr(mess, a, b) BE
{ LET fno = comline>>20
  LET lno = comline & #xFFFFF
  LET filename = sourcenamev!fno
  writes("Error ")
  UNLESS procname=0 DO writef("in %s ", @h3!procname)
  writef("near ")
  IF filename DO writef("%s", filename)
  writef("[%n]: ", lno)
  writef(mess, a, b)
  newline()
  IF hard DO abort(1000)
  errcount := errcount + 1
  IF errcount >= errmax DO { writes("*nCompilation aborted*n")
                             longjump(fin_p, fin_l)
                           }
}

AND newblk(x, y, z) = VALOF
{ // The is used for global and case lists 
  LET p = dvect - 3
  IF dvece>p DO { errmax := 0        // Make it fatal.
                  trnerr("More workspace needed")
                }
  p!0, p!1, p!2 := x, y, z
  dvect := p
  RESULTIS p
}

AND translate(x) BE
{ // First check that the trn globals do not overlap with
  // the codegenerator globals.
  LET lasttrngn = @lasttrnglobal - @glob0
  
  IF debug>0 DO writef("lasttrngn=%i3   cgg=%i3*n", lasttrngn, cgg)

  IF lasttrngn>=cgg DO
  { writef("SYSTEM ERROR: lasttrngn=%i3   cgg=%i3*n", lasttrngn, cgg)
    RESULTIS TRUE
  }
 
  dvec,  dvect := treevec, treep
  h1!dvec, h2!dvec, h3!dvec, h4!dvec := 0, 0, 0, 0
  dvece := dvec+4
  dvecp := dvece
//selectoutput(sysprint)

  // Clear the h2 field of all names in the name table
  // This will be used to the dvec cell for the current declaration
  // of the name.
  // Note that the size of dvec cells has increased to 4 to allow
  // for path variables in the pattern matching extension.
  
  FOR i = 0 TO nametablesize-1 DO
  { LET name = nametable!i // The first name in hash list i
    UNTIL name=0 DO
    { LET next = h2!name
      h2!name := 0 // Mark undeclared
//   writef("Undeclare %s*n", name+2)
      name := next
    }
  }

  // Initialise all the translation variable
  gdeflist, gdefcount := 0, 0
  caselist := 0
  casecount := -1 // CASE and DEFAULT illegal
  defaultlab := 0
  resultlab, breaklab, looplab, endcaselab := -2, -2, -2, -2
  nextlab, exitlab := -2, -2
  matchcontext, patresultpos := 0, 0
  context, comline, procname, labnumber := 0, 1, 0, 0
  ssp, vecssp := savespacesize, savespacesize

  WHILE x~=0 & (h1!x=s_section | h1!x=s_needs) DO
  { LET op, a = h1!x, h2!x
    out1(op)
    outstring(@h2!a)
    x:=h3!x
  }

  trans(x, 0)
  out2(s_global, gdefcount)
  UNTIL gdeflist=0 DO { out2(h2!gdeflist, h3!gdeflist)
                        gdeflist := h1!gdeflist
                      }  
}

LET trnext(next) BE { IF next<0 DO out1(s_rtrn)
                      IF next>0 DO out2(s_jump, next)
                    }
 
LET trans(x, next) BE
// x       is the parse tree of the command to translate
// next<0  compile x followed by RTRN
// next>0  compile x followed by JUMP next
// next=0  compile x only
{ LET op, sfop, sw, ff = ?, ?, FALSE, FALSE

  IF x=0 DO { trnext(next) // Compile RTRN, a jump or nothing
              RETURN
	    }

  op := h1!x // op is the leading operator of
             // the command to translate.

  SWITCHON op INTO
  { DEFAULT: trnerr("Compiler error in Trans, op = %s",
                    opname(op))
             RETURN
 
    CASE s_let:
    { // x -> [s_let, defs, body, lm]
      LET cc = casecount
      LET e, s, s1 = dvece, ssp, 0
      LET v = vecssp
      casecount := -1 // Disallow CASE and DEFAULT labels
                      // but ENDCASE may still be allowed.
      context, comline := x, h4!x
      declnames(h2!x)
      checkdistinct(e)
      vecssp, s1 := ssp, ssp
      ssp := s
      context, comline := x, h4!x
      transdef(h2!x)
      UNLESS ssp=s1 DO trnerr("Lhs and rhs do not match")
      UNLESS ssp=vecssp DO { ssp := vecssp; out2(s_stack, ssp) }
      out1(s_store)
      decllabels(h3!x)
      trans(h3!x, next)
      vecssp := v
      UNLESS ssp=s DO out2(s_stack, s)
      ssp := s
      casecount := cc
      undeclare(e)
      RETURN
    }
 
    CASE s_static:
    CASE s_global:
    CASE s_manifest:
    { LET e, s = dvece, ssp
      AND y = h2!x
      AND n = 0
      LET prevk = 0 // The previous integer or floating point value
      LET prevt = 0 // =0, s_notflt or s_flt
      LET cc = casecount
   
      casecount := -1 // Disallow CASE and DEFAULT labels
                      // but ENDCASE may still be allowed
      context, comline := x, h4!x
 
      WHILE y DO
      { LET name = h3!y // name is a Name or Flt node.
        LET fop = op    // = s_static, s_global or s_manifest
        ff := FALSE     // ff will only be TRUE for static and manifest
                        // names with the FLT tag. If TRUE the value of
                        // the constant will be a floating point number.
			// For glbal declarations the value is an
			// integer global number.

        context, comline := y, h5!y

        // If the name is prefixed by FLT remove the prefix and
        // modify fop as follows
        //     s_static   -> s_fstatic
        //     s_manifest -> s_fmanifest
        // and s_global   -> s_fglobal

        IF h1!name=s_flt DO name, fop := h2!name, op | s_fltbit

        // If fop is s_fstatic or s_fmanifest the constant
        // expression in evaluated in an FLT context, in all
        // other cases it is evaluated in a non FLT context.

        IF fop=s_fstatic | fop=s_fmanifest DO ff := TRUE

        TEST h4!y
        THEN { //sawritef("Calling evalconst*n")
               //plist(h4!y, 0, 5)
               //newline()
               n := evalconst(h4!y, ff)
               //writef("giving n=%n*n", n)
               //abort(1000)
             }
        ELSE { // The constant expression was not given so the
               // value is chosen as follows:

//sawritef("*nname=%s fop=%i3 fopname=%10t prevt=%i3 prevk=%x8(%i3) ff=%i3*n",
//    @h3!name, fop, opname(fop), prevt, prevk, prevk, ff)
//abort(9876)
               // If there was no previous value the value is
               // 0 or 0.0.
               TEST prevt=0
               THEN { n := ff -> flt0, 0
//sawritef("Setting n=%x8(%i3)*n", n, n)
                    }
               ELSE n := VALOF SWITCHON fop INTO
	            { DEFAULT:
		        trnerr("System error in trans")
			RESULTIS 0

                      CASE s_static:
		        // For s_static    the value is 0
                        RESULTIS 0

                      CASE s_fstatic:
		        // For s_fstatic    the value is 0.0
                        RESULTIS flt0

                      CASE s_manifest:
                        // for s_manifest  the next value is one larger
                        //                 than the previous value
                        //                 converted to integer, if necessary.
                        IF prevt=s_flt DO n := sys(Sys_flt, fl_fix, n)
                        RESULTIS n+1

                      CASE s_fmanifest:
                        // for s_fmanifest the value is 1.0 larger than the
                        //                 previous value is converted to
                        //                 floating point, if necessary.
                        IF prevt=s_notflt DO n := sys(Sys_flt, fl_float, n)
                        RESULTIS sys(Sys_flt, fl_add, n, flt1)

                      CASE s_global:
                      CASE s_fglobal:
                        // For s_global and s_fglobal the next value is a
                        // global number one larger than the previous one.
                        RESULTIS n+1
                    }

             }

        // prevk is the previous value (integer or floating point).
        // prevt is s_flt if the previous value was floating point.
        //       it is s_notflt the previous value was an integer.

        prevk := n
        prevt := ff -> s_flt, s_notflt
//sawritef("setting prevk=%n prevt=%n*n", prevk, prevt)

//sawritef("name=%s fop=%i3 fopname=%10t prevt=%i3 prevk=%x8(%i3) ff=%i3 n=%x8(%i3)*n",
//    @h3!name, fop, opname(fop), prevt, prevk, prevk, ff, n, n)

        IF op=s_static DO
        { LET k = n
          n := genlab()      // n is now the label for the static variable
          out2(s_datalab, n)
          out2(s_itemn, k)
        }

        IF op=s_global UNLESS 0<=n<=65535 DO
          trnerr("Global number %n too large for: %s*n", n, @h3!name)

        // n is a global number, a manifest value or a label for a
        // static variable or entry point.
        addname(name, fop, n, 0)
        IF xrefing DO xref(name,
                           (fop=s_global->"G:",
                            fop=s_fglobal->"FG:",
                            fop=s_static->"S:",
                            fop=s_fstatic->"FS:",
                            fop=s_manifest->"M:",
                            fop=s_fmanifest->"FM:",
                            "??:"),
                           n,
                           s_constdef
                          )
        y := h2!y
      }
 
      decllabels(h3!x)
      trans(h3!x, next)
      ssp := s
      casecount := cc
      undeclare(e)
      RETURN
    }

    CASE s_matchc: // x -> [s_matchc, args, mlist, ln]
    CASE s_everyc: //   |  [s_everyc, args, mlist, ln]
    { // This has much in common with a SWITCHON command in that is
      // can select one or more of many alternative and can use a
      // selection of simple commands to escape from a match item
      // or the entire matchlist. These commands are EXIT, NEXT,
      // BREAK, LOOP, ENDCASE and RESULTIS provided the jump does
      // not leave the current function or routine. Of course
      // RETURN is allowed as a special case.

      // No result location needs to be allocated
      
      LET argpos = ssp // The position of the first MATCH or
                       // EVERY argument, if any.

      context, comline := x, h4!x

      // Do not allocate space for a result because it is not needed.

      // Load the match arguments
      loadlist(h2!x)
      out1(s_store) // Ensure that the match arguments are in memory

      // Translate the match items
      transmatchlist(op,     // Context s_matchc or s_everyc
                     h3!x,   // mlist -> [matchitemc, plist, C, link, ln]
                     argpos) // Position of the first argument
      // This point is reached if all match items fail or FAIL is
      // encountered.
      trnext(next) // Compile RTRN, a jump or nothing.
      RETURN
    }
     
    CASE s_assmul: // Assignment operators that might
    CASE s_assdiv: // be promoted to floating point.
    CASE s_assmod:
    CASE s_assadd:
    CASE s_asssub:
    CASE s_ass:
      // Note that simultaneous assignments have already been
      // replaced by sequences of simple assignments.
      // Convert op:= to #op:= if either operand has the FLT tag.
      UNLESS isflt(h3!x) | isflt(h2!x) DO
      { // Compile a non FLT assignment
        context, comline := x, h4!x
        op := assop2op(op)
        assign(h2!x, h3!x, FALSE, op)
        trnext(next)
        RETURN
      }

      // Promote op:= to #op:=
      op := cv2flt(op)
      h1!x := op
      // Fall through

    CASE s_assfmul: // The floating point assignment operators
    CASE s_assfdiv:
    CASE s_assfmod:
    CASE s_assfadd:
    CASE s_assfsub:
    CASE s_fass:
      context, comline := x, h4!x
      op := assop2op(op)
      assign(h2!x, h3!x, TRUE, op)
      trnext(next)
      RETURN

    CASE s_assvecap:  // All the other assignment operators
    CASE s_asslshift:
    CASE s_assrshift:
    CASE s_asslogand:
    CASE s_asslogor:
    CASE s_asseqv:
    CASE s_assxor:
      context, comline := x, h4!x
      op := assop2op(op)
      assign(h2!x, h3!x, FALSE, op)
      trnext(next)
      RETURN
 
    CASE s_rtap:
    { LET s = ssp
      context, comline := x, h4!x
      ssp := ssp+savespacesize
      out2(s_stack, ssp)
      loadlist(h3!x) // Load arguments in non FLT mode
      load(h2!x, FALSE)
      out2(s_rtap, s)
      ssp := s
      trnext(next)
      RETURN
    }
 
    CASE s_goto:
      context, comline := x, h3!x
      load(h2!x, FALSE)
      out1(s_goto)
      ssp := ssp-1
      RETURN
 
    CASE s_colon:
      context, comline := x, h5!x
      out2(s_lab, h4!x)
      trans(h3!x, next)
      RETURN
 
    CASE s_unless: sw := TRUE
    CASE s_if:
      context, comline := x, h4!x

      { // Optimise IF/UNLESS exp BREAK/LOOP/ENDCASE/NEXT/EXIT,
        // if possible.
        LET bodyop = h1!(h3!x) // The leading operator of the body
        LET destlab = destlabel(bodyop)
        // destlab is the destination label if the body
        // was BREAK, LOOP or ENDCASE. Otherwise it is zero.
        IF destlab>0 DO
        { jumpcond(h2!x, ~sw, destlab) //???
          trnext(next)
//sawritef("IF exp BREAK/LOOP/ENDCASE optimised*n")
          RETURN
        }
      }

      TEST next>0 THEN { jumpcond(h2!x, sw, next)
                         trans(h3!x, next)
                       }
                  ELSE { LET l = genlab()
                         jumpcond(h2!x, sw, l)
                         trans(h3!x, next)
                         out2(s_lab, l)
                         trnext(next)
                       }
      RETURN
 
    CASE s_test:
    { LET l, m = genlab(), 0
      context, comline := x, h5!x
      jumpcond(h2!x, FALSE, l)
         
      TEST next=0 THEN { m := genlab(); trans(h3!x, m) }
                  ELSE trans(h3!x, next)
                     
      out2(s_lab, l)
      trans(h4!x, next)
      UNLESS m=0 DO out2(s_lab, m)
      RETURN
    }
 
    CASE s_break:
      context, comline := x, h2!x
      IF breaklab=-2 DO trnerr("BREAK not inside a repetitive command")
      IF breaklab=-1 DO { out1(s_rtrn); RETURN }
      IF breaklab= 0 DO breaklab := genlab()
      out2(s_jump, breaklab)
      RETURN
 
    CASE s_loop:
      context, comline := x, h2!x
      IF looplab=-2 DO trnerr("LOOP not inside a repetitive command")
      IF looplab=-1 DO { out1(s_rtrn); RETURN }
      IF looplab= 0 DO looplab := genlab()
      out2(s_jump, looplab)
      RETURN

    CASE s_next:          // MR 28/08/2021
      context, comline := x, h2!x
      IF nextlab=-2 DO trnerr("NEXT not inside a match list")
      IF nextlab=-1 DO { out1(s_rtrn); RETURN }
      IF nextlab= 0 DO nextlab := genlab()
      out2(s_jump, nextlab)
      RETURN

    CASE s_exit:          // MR 28/08/2021
      context, comline := x, h2!x
      IF exitlab=-2 DO trnerr("EXIT not inside a match list")
      IF exitlab=-1 DO { out1(s_rtrn); RETURN }
      IF exitlab= 0 DO exitlab := genlab()
      out2(s_jump, exitlab)
      RETURN

    CASE s_return:
      context, comline := x, h2!x
      out1(s_rtrn)
      RETURN
 
    CASE s_skip:  // MR 05/4/06
      trnext(next)
      RETURN

    CASE s_finish:
      context, comline := x, h2!x
      out1(s_finish)
      RETURN
 
    CASE s_resultis:
      context, comline := x, h3!x
      IF resultlab=-1 DO { fnbody(h2!x, FALSE); RETURN }
      UNLESS resultlab>0 DO trnerr("RESULTIS out of context")
      load(h2!x, FALSE)
      out2(s_res, resultlab)
      ssp := ssp - 1
      RETURN
 
    CASE s_while: sw := TRUE
    CASE s_until:
    { // If next>0, label to jump next to after completing the command.
      // If next=-1 compile RTRN when the command completes.
      // otherwise compile nothing after the command.
      LET L = genlab()  // Label for start of the body
      LET M = next>0 -> next, genlab()
      // M is the Label to jump to if the body is never executed.
      LET prevbreaklab, prevlooplab = breaklab, looplab
      context, comline := x, h4!x
      breaklab := next  // Destination, possibly -1 for RTRN
                        // It describes how to compile BREAK
			// If breaklab is zero it will be allocated
			// by the first occurence of BREAK.
      looplab  := 0     // This will be alllocated by LOOP, if necessary.
      jumpcond(h2!x, ~sw, M)
      out2(s_lab, L)    // Label the start of the body
      trans(h3!x, 0)    // Zero because the body will be followed by
                        // the conditional jump code
      IF looplab DO out2(s_lab, looplab) // Only compiled if LOOP occurred.
      context, comline := x, h4!x
      jumpcond(h2!x, sw, L) // Compile the conditional jump.
      UNLESS nextlab=M DO out2(s_lab, M)
      IF breaklab>0 DO out2(s_lab, breaklab) // A BREAK command
                                             // jumped to breaklab
      trnext(next) // Possibly compile RTRN or JUMP Lnext
      breaklab, looplab := prevbreaklab, prevlooplab
      RETURN
    }
 
    CASE s_repeatwhile: sw := TRUE
    CASE s_repeatuntil:
    { LET L = genlab()
      LET prevbreaklab, prevlooplab = breaklab, looplab
      context, comline := x, h4!x
      breaklab := next // Cause BREAK to compile RTRN, JUMP Lnext
                       // or JUMP Lbreaklab with newly allocated label
      looplab := 0     // Allocated by the first LOOP, if any
      out2(s_lab, L) // Label start of body
      trans(h2!x, 0) // Zero because it is followed by the conditional jump
      UNLESS looplab=0 DO out2(s_lab, looplab)
      context, comline := x, h4!x
      jumpcond(h3!x, sw, L)

      IF breaklab>0 & breaklab~=next DO out2(s_lab, breaklab)

      trnext(next)
      breaklab, looplab := prevbreaklab, prevlooplab
      RETURN
    }
 
    CASE s_repeat:
    { LET bl, ll = breaklab, looplab
      context, comline := x, h4!x
      breaklab, looplab := next, genlab()
      out2(s_lab, looplab)

      trans(h2!x, looplab)

      //IF next=0 & breaklab>0 DO out2(s_lab, breaklab)
      IF breaklab>0 DO out2(s_lab, breaklab)

      trnext(next)
      breaklab, looplab := bl, ll
      RETURN
    }
 
    CASE s_case:
    { LET l, k, p = genlab(), ?, caselist
      context, comline := x, h4!x
      k := evalconst(h2!x, FALSE)
      IF casecount<0 DO trnerr("CASE label out of context")
      WHILE p DO
      { IF h2!p=k DO trnerr("'CASE %n:' occurs twice", k)
        p := h1!p
      }
      caselist := newblk(caselist, k, l)
      casecount := casecount + 1
      out2(s_lab, l)
      trans(h3!x, next)
      RETURN
    }
 
    CASE s_default:
      context, comline := x, h3!x
      IF casecount<0 DO trnerr("DEFAULT out of context")
      IF defaultlab DO trnerr("DEFAULT defined twice")
      defaultlab := genlab()
      out2(s_lab, defaultlab)
      trans(h2!x, next)
      RETURN
 
    CASE s_endcase:
      context, comline := x, h2!x
      IF endcaselab=-2 DO trnerr("ENDCASE not inside a SWITCHON command")
      IF endcaselab=-1 DO out1(s_rtrn)
      IF endcaselab= 0 DO endcaselab := genlab()
      out2(s_jump, endcaselab)
      RETURN
 
    CASE s_switchon:
      transswitch(x, next)
      RETURN
 
    CASE s_for:
      transfor(x, next)
      RETURN
 
    CASE s_seq:
      trans(h2!x, 0)
      x := h3!x
  }
} REPEAT

AND genjumpoptodest(op, destptr) BE
{ // op is the conditional or unconditioal Cintcode jump imstruction
  // destptr is a pointer to the destination label, namely
  // @breaklab, @looplab, @endcaselab, @nextlab, @exitlab

    IF !destptr=-2 DO trnerr("Illegal BREAK,LOOP,ENDCASE,NEXT or EXIT")
    IF !destptr=-1 DO
    { IF matchcontext=s_patfndef DO { out1(s_fnrn); RETURN }
      IF matchcontext=s_patrtdef DO { out1(s_rtrn); RETURN }
      !destptr := 0
    }
    UNLESS !destptr DO !destptr := genlab()
    out2(op, !destptr)
    RETURN
}

AND destlabel(op) = VALOF SWITCHON op INTO
{ // Choose the label for a (possibly conditional) jump
  // for one of the following commands:
  // BREAK, LOOP, ENDCASE, NEXT or EXIT
  // Return zero if not one of these commands.
  DEFAULT:
    RESULTIS 0

  CASE s_loop:
    IF looplab<0 DO trnerr("Illegal use of LOOP")
    IF looplab=0 DO looplab := genlab()
    RESULTIS looplab

  CASE s_next:
    IF nextlab=-2 DO trnerr("Illegal use of NEXT")
    IF nextlab= 0 DO nextlab := genlab()
    RESULTIS nextlab

  CASE s_exit:
    IF exitlab=-2 DO trnerr("Illegal use of EXIT")
    IF exitlab= 0 DO exitlab := genlab()
    RESULTIS exitlab

  CASE s_break:
    IF breaklab=-2 DO trnerr("Illegal use of BREAK")
    IF breaklab= 0 DO breaklab := genlab()
    RESULTIS breaklab

  CASE s_endcase:
    IF endcaselab=-2 DO trnerr("Illegal use of ENDCASE")
    UNLESS endcaselab DO endcaselab := genlab()
    RESULTIS endcaselab
}

LET declnames(x) BE UNLESS x=0 SWITCHON h1!x INTO
  // x is the definition(s) following LET, so the leading operator is
  // one of s_vecdef, s_valdef, s_fndef, s_rtdef, s_patfndef, s_patrtdef
  // or s_and. This function adds names to the declaration vector.
{ DEFAULT:
    trnerr("Compiler error in Declnames, op=%s", opname(h1!x))
    RETURN
 
  CASE s_vecdef:
    context, comline := x, h4!x
    IF h1!(h2!x)=s_flt DO
    { trnerr("Vector name must not have the FLT tag")
      h2!x := h2!(h2!x) // Remove the FLT tag
    }
    decldyn(h2!x)
    RETURN
 
  CASE s_valdef:
    context, comline := x, h4!x
    decldyn(h2!x)
                 RETURN
 
  CASE s_rtdef:  // x -> [ rtdef, name, namelist, C, entrylab, ln ]
  CASE s_fndef:  //   |  [ fndef, name, namelist, E, entrylab, ln ]
    context, comline := x, h6!x
    // 28feb2023  Functions declarations my now use the FLT prefix
    // to indicate the result of a call has the FLT tag.
    //IF h1!(h2!x)=s_flt DO
    //{ trnerr("Function name must not have the FLT tag")
    //  h2!x := h2!(h2!x) // Remove the FLT tag
    //}
    h5!x := genlab()
    declstat(h2!x, h5!x) // Declare the fn or rt name
    RETURN
 
  CASE s_patrtdef: // x -> [ patrtdef, name, matchlist, entrylab, ln ]
  CASE s_patfndef: //   |  [ patfndef, name, matchlist, entrylab, ln ]
    context, comline := x, h5!x
    // 28feb2023  Functions declarations my now use the FLT prefix
    // to indicate the result of a call has the FLT tag.
    //IF h1!(h2!x)=s_flt DO
    //{ trnerr("Function name must not have the FLT tag")
    //  h2!x := h2!(h2!x) // Remove the FLT tag
    //}
    h4!x := genlab()
    declstat(h2!x, h4!x) // Declare the patfn or patrt name
    RETURN
 
  CASE s_and:
    declnames(h2!x)
    declnames(h3!x)
}
 
AND decldyn(x) BE UNLESS x=0 DO 
{ // x is a list of names with possible FLT tags.
  // They are added to the declaration vector with kind
  // s_local or s_flocal.
  LET k = s_local

  IF h1!x=s_flt DO
  { k := s_flocal     // x -> [s_flt, [s_name.chain,<caracters>]
    x := h2!x
  }

  IF h1!x=s_name DO
  { addname(x, k, ssp, 0)
    IF xrefing DO
      xref(x,
           (k=s_local -> "P:", "FP:"),
           ssp, s_local)
      ssp := ssp + 1
      RETURN
    }
 
  IF h1!x=s_comma DO
  { decldyn(h2!x)
    decldyn(h3!x)
    RETURN
  }
 
  trnerr("Compiler error in Decldyn")
}
 
AND declstat(x, lab) BE
{ // x is the name of a function, routine or a
  // pattern function or routine, or a label.
  LET c = cellwithname(x)
  LET fk = h2!c
  LET k = fk & s_fltmask 
  TEST k=s_global
  THEN { // x is being declared in the scope of a global
         // declaration of the same name. So the global
	 // must be initialised with the entry point.
	 // If either x or the global declaration has
	 // the FLT prefix they must both have this
	 // prefix.
	 LET gn = h3!c
         gdeflist := newblk(gdeflist, gn, lab)
         gdefcount := gdefcount + 1
         //addname(x, s_global, gn, 0)
         addname(x, fk, gn, 0)     // Modified 28feb2023
         IF xrefing DO
           xref(x,
                (fk=s_fglobal -> "FG:", "G:"),
                gn, h1!context)
         IF gdefsing DO
         { writef("G%i3 = %s*n", gn, @h3!x)
           //abort(1000)
         }
      }
 ELSE { addname(x, s_label, lab, 0)
        IF xrefing DO
          xref(x,
               (fk=s_flocal -> "FF:", "F:"),
               lab, h1!context)
      }
}
 
AND decllabels(x) BE
{ // Declare the labels in the body globale, static, manifest,
  // let bolcks and the body of rtdef, patrtdef and valof blocks,
  // and for commands.
  LET e = dvece
  scanlabels(x)
  checkdistinct(e)
}
 
AND checkdistinct(p) BE
{ LET lim = dvece - 4
  FOR q = p TO lim-4 BY 4 DO
  { LET n = h1!q
    FOR c = q+4 TO lim BY 4 DO
        IF h1!c=n DO trnerr("Name %s defined twice", @h3!n)
  }
}
 
AND addname(name, k, a, path) BE
{ LET p = dvece + 4
  IF p>dvect DO trnerr("More workspace needed")
  h1!dvece, h2!dvece, h3!dvece, h4!dvece := name, k, a, path
  h2!name := dvece // Remember the declaration
  dvece := p
  IF prtree2 DO
    sawritef("addname: name cell at=%n %s k=%n a=%n path=%x8*n",
            dvece-3, @h3!name, k, a, path)
}
 
AND undeclare(e) BE 
{ FOR t = e TO dvece-4 BY 4 DO
  { LET name = h1!t
    h2!name := 0   // Forget the declaration of this name.
  }
  dvece := e
}

AND cellwithname(n) = VALOF
{ // n is a name node not prefixed by FLT.
  LET t = h2!n
  IF t RESULTIS t  // It has been looked up before
  t := dvece
  t := t - 4 REPEATUNTIL h1!t=n | h1!t=0
  h2!n := t  // Associate the name with declaration item
  RESULTIS t
}
 
AND scanlabels(x) BE UNLESS x=0 SWITCHON h1!x INTO
 
{ CASE s_colon:   context, comline := x, h5!x
                  h4!x := genlab()
                  declstat(h2!x, h4!x)
 
  CASE s_if: CASE s_unless: CASE s_while: CASE s_until:
  CASE s_switchon: CASE s_case:
                  scanlabels(h3!x)
                  RETURN
 
  CASE s_seq:     scanlabels(h3!x)
 
  CASE s_repeat:
  CASE s_repeatwhile:
  CASE s_repeatuntil:
  CASE s_default: scanlabels(h2!x)
                  RETURN
 
  CASE s_test:    scanlabels(h3!x)
                  scanlabels(h4!x)
  DEFAULT:        RETURN
}
 
AND transdef(x) BE
{ LET ctxt, ln = context, comline
  transdyndefs(x)
  context, comline := ctxt, ln
  IF statdefs(x) DO { LET l, s = genlab(), ssp
                      out2(s_jump, l)
                      transstatdefs(x)
                      ssp := s
                      out2(s_stack, ssp)
                      out2(s_lab, l)
                    }
  context, comline := ctxt, ln
}
 
 
AND transdyndefs(x) BE SWITCHON h1!x INTO
{ CASE s_and:    transdyndefs(h2!x)
                 transdyndefs(h3!x)
                 RETURN
 
  CASE s_vecdef: context, comline := x, h4!x
                 out2(s_llp, vecssp)
                 ssp := ssp + 1
                 vecssp := vecssp + 1 + evalconst(h3!x, FALSE)
                 RETURN
 
  CASE s_valdef: // Compile initialisation code for declaration
                 // N = E  or  FLT N = E
                 context, comline := h3!x, h4!x
                 load(h3!x, h1!(h2!x)=s_flt -> TRUE, FALSE)
  DEFAULT:       RETURN
}
 
AND transstatdefs(x) BE SWITCHON h1!x INTO
{ CASE s_and:  transstatdefs(h2!x)
               transstatdefs(h3!x)
               RETURN
 
  CASE s_rtdef:  // x -> [ rtdef, name, namelist, C, entrylab, ln ]
  CASE s_fndef:  //   |  [ fndef, name, namelist, E, entrylab, ln ]
  { LET e, p = dvece, dvecp
    AND oldpn = procname
    AND xl = exitlab
    AND nl = nextlab
    AND bl, ll = breaklab,  looplab
    AND rl, el = resultlab, endcaselab
    AND cl, cc = caselist,  casecount
    AND argpos = savespacesize
    AND name = h2!x // The FLT tag if any will have been 
                    // removed by declstat.
    AND body = h4!x

    nextlab := -2
    exitlab := -2
    breaklab,  looplab    := -2, -2 // BREAK and LOOP illegal
    resultlab, endcaselab := -2, -2 // RESULTIS and ENDCASE illegal
    caselist,  casecount  :=  0, -1 // CASE and DEFAULT illegal
    procname := name
    context, comline := x, h6!x

    out2(s_entry, h5!x)
    outstring(@h3!procname)

    dvecp := dvece
    
    ssp := savespacesize

    decldyn(h3!x)    // Declare the formal parameters
    checkdistinct(e) // Check that they are distinct.

    out2(s_save, ssp)

    context, comline := body, h6!x
    TEST h1!x=s_rtdef
    THEN { LET e1 = dvece
           decllabels(body)
           trans(body, -1)     // Compile body followed by RTRN
           undeclare(e1)       // Undeclare the labels
         }
    ELSE { fnbody(body, FALSE) // Compile body in non FLT mode
         }                     // followed by FNRN
    out1(s_endproc)

    exitlab := xl
    nextlab := nl
    breaklab,  looplab    := bl, ll
    resultlab, endcaselab := rl, el
    caselist,  casecount  := cl, cc
    procname := oldpn
    dvecp := p
    undeclare(e) // Undeclare the formal parameters
    RETURN
  }
 
  CASE s_patrtdef: // x -> [ patrtdef, name, mlist, entrylab, ln ]
  CASE s_patfndef: //   |  [ patfndef, name, mlist, entrylab, ln ]
  { LET prevdvecp     = dvecp
    AND prevprocname  = procname

    AND bl, ll = breaklab,  looplab    // Thes must be set to make
    AND rl, el = resultlab, endcaselab // BREAK, LOOP, RESULTIS,
                                       // and ENDCASE escapes illegal.

    AND name   = h2!x // The FLT tag if any will have been 
                      // removed by declstat.
    AND mlist = h3!x
    AND argpos = savespacesize

    breaklab,  looplab    := -2, -2 // BREAK, LOOP illegal
    resultlab, endcaselab := -2, -2 // RESULTIS, RETURN illegal

    procname := name
    context, comline := x, h5!x

    out2(s_entry, h4!x)
    outstring(@h3!procname)

    // The arguments are laid out in the stack starting
    // at argpos.

    // Note that SAVE will be compiled by transmatchlist when it
    // knows how many arguments are inspected by the patterns.

    // Translate all the match items
    transmatchlist(h1!x,   // The context is s_patfndef or s_patrtdef
                   mlist,  // mlist -> [matchiteme, plist, E, link, ln]
                           //       |  [matchitemc, plist, C, link, ln]
                   argpos) // Position of the first match argument
    // This point in the compiled code will not be reached.
    out1(s_endproc)

    breaklab,  looplab    := bl, ll
    resultlab, endcaselab := rl, el

    procname := prevprocname
    dvecp := prevdvecp
  }
 
  DEFAULT:     RETURN
}

AND arglength(mlist) = VALOF
{ // mlist ->  [ matchiteme, plist, E, link, ln ]
  //       |   [ matchitemc, plist, C, link, ln ]
  // link points to the next match item, if any

  // Return the maximum number of arguments discovered in any
  // match item in mlist.
  LET res = 0

  WHILE mlist DO
  { IF h2!mlist DO
    { LET len = patarglength(h2!mlist)
      IF res < len DO res := len
    }
    mlist := h4!mlist
  }

  RESULTIS res
}

AND patarglength(plist) = VALOF
{ // plist => 0                           => zero
  //       |  [ comma. plist, plist ]     => sum of the two plists
  //       |  all othe pat nodes          => length 1
  IF plist=0  RESULTIS 0
  IF h1!plist=s_comma RESULTIS patarglength(h2!plist) + patarglength(h3!plist)
  RESULTIS 1
}

AND transnext() BE
{ IF nextlab =-2 DO trnerr("NEXT out of context")
  IF nextlab=-1 DO { out1(s_rtrn); RETURN }
  IF nextlab=0 DO nextlab := genlab()
  out2(s_jump, nextlab)
}

AND transexit() BE
{ IF exitlab =-2 DO trnerr("EXIT out of context")
  IF exitlab=-1 DO { out1(s_rtrn); RETURN }
  IF exitlab=0 DO exitlab := genlab()
  out2(s_jump, exitlab)
}

AND transmatchlist(mcontext, mlist, argpos) BE
{ // This function is only used to compile a match list in one of the
  // contexts specified by mcontext. It does not call itself.
  
  // mcontext is s_patfndef, s_patrtdef,
  //             s_matche,   s_matchc,
  //             s_everye or s_everyc

  // mlist ->  [ matchiteme, plist, E, link, ln ]
  //       or  [ matchitemc, plist, C, link, ln ]

  //           link points to the next match item node, if any.

  // argpos is the position relative to P of the first argument,
  // if any. These arguments are already laid out on the the top
  // of the stack.

  // dvece, nextlab, exitlab and casecount are saved and restored.
  // If in a pattern matching function or routine, dvecp, breaklab,
  // looplab, resultlab and endcaselab are also saved and restored.
  // Note that caselist is saved and restored by transswitch.

  // If the match context is s_matche or s_everye, patresultpos holds
  // the stack location relative to P for the result. This will be
  // just before the first argument position. In both cases this
  // location is initialised to zero.

  // The global nextlab is used to label the end of the  current
  // match item. It is used by the conditional jumps in the
  // implementation of patterns and in the compilation of NEXT.
  // It is set to -2 when not in a match construct. When greater
  // than 0 it holds the label number but is zero before the
  // label number is allocated. When set to -1 a jump to nextlab
  // is replaced by FNRN or RTRN depending on the context.

  // exitlab is used to label the end of the match sequence.
  // It is set to -2 when not compiling a match construct. If is
  // is set to -1 jumps to exitlab are replaced by FNRN or RTRN.
  // If exitlab is zero, it given a new label number and a jump
  // is compiled.

  // When the match context is patfndef the compilation of match item
  // expressions followed by FNRN causing the function to return.

  // When the match context is patrtdef the compilation of match item
  // commands are followed by RTRN causing the routine to return.

  // When the match context is s_matche or s_everye, a stack location
  // will have already be allocated and its position relative to P
  // will be in the global patresultpos. For matche, the compilation
  // of the match expressions will be followed by: SP patresultpos
  // and a jump to exitlab. For everye, the added code would be
  // LP patresultpos; ADD; SP patresultpos before falling into the
  // code for the next match item. This will cause the successful
  // results to be summed.
  
  // When the match context is matchc, the compilation of match commands
  // are followed by jumps to exitlab
  
  // When the match context is everyc, the compilation of match commands 
  // fall into the start of the next match item, if any.
  
  LET prevmatchcontext = matchcontext
  LET prevpatresultpos = patresultpos
  
  LET prevdvece      = dvece     // Save all the variables that
  LET prevdvecp      = dvecp     // must be preserved
  
  LET prevexitlab    = exitlab    // These quantities must be saved and
  LET prevnextlab    = nextlab    // restored before returning
  LET prevcasecount  = casecount

  LET prevbreaklab   = breaklab   // These are only restored if in
  LET prevlooplab    = looplab    // a pattern matching function or
  LET prevendcaselab = endcaselab // or routine
  LET prevresultlab  = resultlab
  
  LET op    = h1!mlist
  LET plist = h2!mlist
  LET body  = h3!mlist
  
  LET argcount = arglength(mlist) // The number of arguments
                                  // inspected by the match items

  // Note that the number of arguments supplied may be more or
  // less than the number inspected by the match items.

  // The translation of each match item starts with ssp set to
  // argpos+argcount. This ensures the the evaluation of the
  // bodies of match items do not corrupt arguments inspected by
  // later match items. This is necessary for EVERY constructs and
  // when NEXT is executed.

  matchcontext := mcontext
  
  // mcontext is placed in the global matchcontext since it affects
  // the translation of the NEXT and EXIT commands which might be
  // encountered in inner calls of functions such as trans and
  // transpattern. The previous version of matchcontext is restored
  // before returning from transmatchlist.
  
  exitlab := 0        // These are only allocated label if needed
  casecount := -1     // Disallow CASE and DEFAULT labels
  
  ssp := argpos+argcount
  TEST matchcontext=s_patfndef | matchcontext=s_patrtdef
  THEN { // We can now compile the SAVE statement since we know
         // how many arguments are present,
	 out2(s_save, ssp)  // Save the return link and set ssp
         // BREAK, LOOP, ENDCASE, RESULTIS are not allowed to
	 // cause a jump out of the current function or routine.
	 breaklab,     looplab := -2, -2
	 endcaselab, resultlab := -2, -2
       }
  ELSE { out2(s_stack, ssp) // Just set ssp
       }
  
  IF matchcontext=s_matche | matchcontext=s_everye DO
  { // For MATCH and EVERY expressions initialise the result location.
    out2(s_ln, 0)
    ssp := ssp+1
    out2(s_sp, patresultpos)
    ssp := ssp-1
  }
  
  // We are now ready to translate the match items.
  WHILE mlist DO
  { // mlist -> [ matchiteme, plist, E, link, ln ]
    //       |  [ matchitemc, plist, C, link, ln ]
    LET pattern, body = h2!mlist, h3!mlist
    LET prevdvece = dvece

    context, comline := mlist, h5!mlist
    ssp := argpos + argcount
    out2(s_stack, ssp)
    
    nextlab := 0
//    IF h4!mlist=0 & // Is it the last match item of a
//                    // function or routine
//       (matchcontext=s_patfndef | matchcontext=s_patrtdef) DO
//      nextlab := -1
      
    declpatnames(pattern,  // The pattern
                 argpos,   // Position relative to P of the first
                           // match argument
                 0,        // The current square bracket depth
                           // The arguments are at depth zero
                 0)        // Packed set of up to four 8-bit path offsets

    checkdistinct(prevdvece) // The pattern names must be distinct.

    // Translate the pattern causing a jump to next if not satisfied.
    transpattern(pattern, argpos,    // Argument position relative to P
                          0,         // The indirection depth
			  0)         // The indirection path

    // The pattern was matched successfully so translate
    // the body of the match item.

    SWITCHON matchcontext INTO
    { DEFAULT:
        trnerr("SYSTEM ERROR in transmatchlist")
        abort(999)
        ENDCASE

      CASE s_patfndef:
        fnbody(body, FALSE) // Compile body in non FLT mode
                            // followed by FNRN
        ENDCASE
	
      CASE s_patrtdef:
        decllabels(body)
        trans(body, -1)     // Compile body followed by RTRN
        ENDCASE
	
      CASE s_matche:
        // Compile the match item expression followed by
	// SP patresultpos and a jump to exitlab
        load(body, FALSE)
	out2(s_sp, patresultpos)
	ssp := ssp-1
	genjumpoptodest(s_jump, @exitlab)
        //UNLESS exitlab DO exitlab := genlab()
        //out2(s_jump, exitlab)
        ENDCASE
	
      CASE s_matchc:
        // Compile the match item command followed by
	// a jump to exitlab
        UNLESS exitlab DO exitlab := genlab()
        decllabels(body)
        trans(body, exitlab)
        ENDCASE
	
      CASE s_everye:
        load(body, FALSE)
        out2(s_lp, patresultpos)
	ssp := ssp+1
        out1(s_add)
	ssp := ssp-1
        out2(s_sp, patresultpos)
	ssp := ssp-1
        // Fall through to next matchitem
        ENDCASE
	
      CASE s_everyc:
        decllabels(body)
        trans(body, 0)
        // Fall through to next matchitem
        ENDCASE
    }

    // Compile a LAB statement for nextlab if needed
    IF nextlab>0 DO out2(s_lab, nextlab)
      
    undeclare(prevdvece)  // Undeclare the variables declared
                          // in this match item.

    // Translate the next match item, if any.
    mlist := h4!mlist
  }

  // Compile a LAB statement for exitlab is needed
  IF exitlab>0 DO out2(s_lab, exitlab)

  

  ssp := argpos
  out2(s_stack, ssp)   // Reset ssp

  exitlab := prevexitlab
  nextlab := prevnextlab
  casecount := prevcasecount

  IF op=s_patfndef | s_patrtdef DO
  { // Restore dvecp and the labels for
    // BREAK, LOOP, ENDCASE, RESULTIS which were disallowed
    dvecp := prevdvecp
    breaklab,     looplab := prevbreaklab,     prevlooplab
    endcaselab, resultlab := prevendcaselab, prevresultlab
  }

  matchcontext := prevmatchcontext
  patresultpos := prevpatresultpos
}

AND declpatnames(plist, argpos, depth, path) BE IF plist DO
{ LET op = h1!plist
  LET k = s_local

  // Check that depth and path are allowable
  IF depth>4 DO
    trnerr("Pattern depth is not allowed to be greater than 4")
  IF (path & 255) > 254 DO
    trnerr("The pattern list is too long")
    
  SWITCHON op INTO
  { DEFAULT:
      RETURN            // All other pattern operators

    CASE s_flt:
      plist := h2!plist     // x should point to [s_flt, [s_name, ... ]]
      op  := h1!plist
      UNLESS op=s_name DO
         trnerr("SYSTEM ERROR in declpatnames, FLT not followed by a name")
      k := s_flocal
      // Fall through to CASE s_name
      
    CASE s_name:
    { LET cell = cellwithname(plist)
     
      IF (h2!cell & s_fltmask) = s_manifest  RETURN
      // The name is not a manifest so define it as a path variable
      
      IF depth=0 DO { addname(plist, k, argpos, 0)
                      IF xrefing DO
                        xref(plist, (k=s_local -> "P:", "FP:"), argpos, k)
//writef("Declaring pattern variable %s to be %s %n*n",
//        @h3!plist, opname(k), argpos)
                      RETURN
                    }
      TEST k=s_local
      THEN k := s_path1  + depth - 1
      ELSE k := s_fpath1 + depth - 1

      addname(plist, k, argpos, path)
      IF xrefing DO xref(plist, (k=s_local -> "I:", "FI:"), argpos, k, path)
//writef("Declaring pattern variable %s to be %s %n path=%x8*n",
//        @h3!plist, opname(k), argpos, path)
      RETURN
    }
    
    CASE s_comma:
       declpatnames(h2!plist, argpos, depth, path)
       TEST depth=0
       THEN declpatnames(h3!plist, argpos+1, depth, path)
       ELSE declpatnames(h3!plist, argpos,   depth, path+1)
       RETURN

    CASE s_patand:
       declpatnames(h2!plist, argpos, depth, path)
       declpatnames(h3!plist, argpos, depth, path)
       RETURN

    CASE s_patptr:
       declpatnames(h2!plist, argpos, depth+1, path<<8)
       RETURN
  }
}

AND transpattern(x, argpos, depth, path) BE IF x DO
{ LET op = h1!x
  LET ff = FALSE // Assume integer tests
  LET patrelop = 0
  
//writef("transpattern: op=%s rsltpos=%n argpos=%n ssp=%n depth=%n path=%X8*n",
//        opname(op), patresultpos, argpos, ssp, depth, path)
//abort(1123)

  SWITCHON op INTO
  { DEFAULT:
      trnerr("SYSTEMERROR in transpattern, op=%s", opname(op))

    CASE s_break:   genjumpoptodest(s_jump, @breaklab);   RETURN
    CASE s_loop:    genjumpoptodest(s_jump, @looplab);    RETURN
    CASE s_endcase: genjumpoptodest(s_jump, @endcaselab); RETURN
    CASE s_next:    genjumpoptodest(s_jump, @nextlab);    RETURN
    CASE s_exit:    genjumpoptodest(s_jump, @exitlab);    RETURN

    CASE s_query:
      RETURN
  
    CASE s_fnum:
      ff := TRUE
    CASE s_number:
//writef("transpattern: do   number op=%s rsltpos=%n argpos=%n ssp=%n depth=%n path=%X8*n",
//        opname(op), patresultpos, argpos, ssp, depth, path)
      IF nextlab<=0 DO nextlab := genlab()
      oppath(s_lp, argpos, depth, path)
      ssp := ssp+1
      out2(s_ln, h2!x)
      ssp := ssp+1
      out1(ff -> s_feq, s_eq)
      ssp := ssp-1
      out2(s_jf, nextlab)
      ssp := ssp-1
//writef("transpattern: done number op=%s rsltpos=%n argpos=%n ssp=%n depth=%n path=%X8*n",
//        opname(op), patresultpos, argpos, ssp, depth, path)
      RETURN

    CASE s_flt:
      x := h2!x
      UNLESS h1!x=s_name DO trnerr("Operand of FLT should be a name")
      // Fall through to CASE s_name:
    CASE s_name:
    { LET cell = cellwithname(x)
      LET k, n = h2!cell, h3!cell
      IF cell & (k=s_manifest | k=s_fmanifest) DO
      { // Treat a manifest constant as an integer constant
        IF k=s_fmanifest DO ff := TRUE
        IF nextlab<=0 DO nextlab := genlab()
        oppath(s_lp, argpos, depth, path)
	ssp := ssp+1
        out2(s_ln, n)
	ssp := ssp+1
        out1(ff -> s_feq, s_eq)
	ssp := ssp-1
        out2(s_jf, nextlab)
	ssp := ssp-1
        RETURN
      }
      // A non manifest name is always successful.
      RETURN
    }
      
    CASE s_comma:
      transpattern(h2!x, argpos, depth, path)
      TEST depth
      THEN transpattern(h3!x, argpos,   depth, path+1)
      ELSE transpattern(h3!x, argpos+1, depth, path)
      RETURN

    CASE s_patand:
      // Form:  P Q
      // Compile code to jump to nextlab if
      // either P or Q fails to match the current location
      transpattern(h2!x, argpos, depth, path)
      transpattern(h3!x, argpos, depth, path)
      RETURN

    CASE s_frange:
      ff := TRUE
    CASE s_range:
      UNLESS ff IF isflt(h2!x) | isflt(h3!x) DO
      { op := cv2flt(op) // Promote to floating point if needed
        h1!x := op
	ff := TRUE
      }
      // P  .. Q  is equivalent to   >=P  <=Q
      // P #.. Q  is equivalent to  #>=P #<=Q
      // Compile code to jump to nextlab if
      // the current value is not in the range
      IF nextlab<=0 DO nextlab := genlab()
      oppath(s_lp, argpos, depth, path)
      ssp := ssp+1
      load(h2!x, ff)
      out1(ff -> s_fls, s_ls)
      ssp := ssp-1
      out2(s_jt, nextlab)
      ssp := ssp-1
      oppath(s_lp, argpos, depth, path)
      ssp := ssp+1
      load(h3!x, ff)
      out1(ff -> s_fgr, s_gr)
      ssp := ssp-1
      out2(s_jt, nextlab)
      ssp := ssp-1
      RETURN
      
    CASE s_pator:
    { // x is a collections of constants or ranges
      LET L = genlab()
      transpator(x, argpos, depth, path, L)
      IF nextlab=0 DO nextlab := genlab()
      out2(s_jump, nextlab) // Jump taken if the match failed
      out2(s_lab, L)        // Point reached if the match was successful
      RETURN
    }
    
    CASE s_patptr:
      transpattern(h2!x, argpos,   depth+1, path<<8)
      RETURN

     CASE s_patfeq:
     CASE s_patfne:
     CASE s_patfls:
     CASE s_patfgr:
     CASE s_patfle:
     CASE s_patfge:
       ff := TRUE
     CASE s_pateq:
     CASE s_patne:
     CASE s_patls:
     CASE s_patgr:
     CASE s_patle:
     CASE s_patge:
       UNLESS ff IF isflt(h2!x) DO
       { op := cv2flt(op) // Promote to floating point if needed
	 h1!x := op
	 ff := TRUE
       }
       oppath(s_lp, argpos, depth, path)
       ssp := ssp+1
       load(h2!x, FALSE)
       IF nextlab<=0 DO nextlab := genlab()
       out1(patrel2rel(op))
       ssp := ssp-1
       out2(s_jf, nextlab)
       ssp := ssp-1
       RETURN
  }
}

AND transpator(x, argpos, depth, path, L) BE
{ // L is the label for successful matches.
  LET op = h1!x
  LET ff = FALSE // Assume integer test unless floating point specified.
//writef("transpator: op=%s  argpos=%n depth=%n path=%X8 L=%n*n",
//        opname(h1!x), argpos, depth, path, L)
//abort(1124)

  SWITCHON op INTO
  { DEFAULT:
      trnerr("Unexpected operator in a pattern OR construct, op=%s",
              opname(op))

    CASE s_pator:
      transpator(h2!x, argpos, depth, path, L)
      transpator(h3!x, argpos, depth, path, L)
      RETURN

    CASE s_fnum:
      ff := TRUE
    CASE s_number:
      oppath(s_lp, argpos, depth, path)
      ssp := ssp+1
      load(x, ff)
      out1(ff -> s_feq, s_eq)
      ssp := ssp-1
      out2(s_jt, L)
      ssp := ssp-1
      RETURN

    CASE s_name:
    { LET cell = cellwithname(x)
      LET k, n = h2!cell, h3!cell
      UNLESS k=s_manifest | k=s_fmanifest DO
      { trnerr("The name %s should be a manifest constant", @h3!x)
        RETURN    
      }
      oppath(s_lp, argpos, depth, path)
      ssp := ssp+1
      IF k=s_fmanifest DO ff := TRUE
      load(x, ff)
      out1(ff -> s_feq, s_eq)
      ssp := ssp-1
      out2(s_jt, L)
      ssp := ssp-1
      RETURN
    }

    CASE s_frange:
      ff := TRUE
    CASE s_range:
    { LET M = genlab() // Jump to M if current value not in the range
      UNLESS ff IF isflt(h2!x) | isflt(h3!x) DO
      { op := cv2flt(op) // Promote to floating point if needed
        h1!x := op
	ff := TRUE
      }
      // P  .. Q  is equivalent to   >=P  <=Q
      // P #.. Q  is equivalent to  #>=P #<=Q
      // Compile jump to L if the match is successful
      oppath(s_lp, argpos, depth, path)
      ssp := ssp+1
      UNLESS isconst(h2!x) & isconst(h3!x) DO
        trnerr("The operands of a range must be constants")
      load(h2!x, ff)
      out1(ff -> s_fls, s_ls)
      ssp := ssp-1
      out2(s_jt, M) // Jump if current value too small
      ssp := ssp-1
      oppath(s_lp, argpos, depth, path)
      ssp := ssp+1
      load(h3!x, ff)
      out1(ff -> s_fle, s_le)
      ssp := ssp-1
      out2(s_jt, L)  // Jump to L if the range is satisfied
      ssp := ssp-1
      out2(s_lab, M) // The range test has failed
      RETURN
    }
  }
}

AND oppath(op,        // s_lp, s_llp or s_sp
           pos,
           depth,
	   path) BE
{ // Load or store a value specified by a path onto/from the top of the stack.
  // This function does not change ssp. That is done in the calling code.
//writef("oppath: pos=%n depth=%n path=%X8*n", pos, depth, path)
//abort(5524)
  LET offset = path&255
  
  IF depth<=0 DO
  { out2(op, pos)
    RETURN
  }
  oppath(s_lp, pos, depth-1, path>>8)
  IF offset DO
  { out2(s_ln, offset)
    out1(s_add)
  }
  IF op=s_lp DO
  { out1(s_rv)
    RETURN
  }
  IF op=s_sp DO
  { out1(s_stind)
    RETURN
  }
  IF op=s_llp RETURN
  trnerr("SYSTEM ERROR: in oppath")
}

AND statdefs(x) = h1!x=s_fndef | h1!x=s_rtdef       -> TRUE,
                  h1!x=s_patfndef | h1!x=s_patrtdef -> TRUE,
                  h1!x ~= s_and                     -> FALSE,
                  statdefs(h2!x)                    -> TRUE,
                  statdefs(h3!x)
 
 
LET jumpcond(x, b, L) BE
{ // L is a label number > 0
  LET sw = b

  SWITCHON h1!x INTO
  { CASE s_false:  b := NOT b
    CASE s_true:   IF b DO out2(s_jump, L)
                   RETURN
 
    CASE s_not:    jumpcond(h2!x, NOT b, L)
                   RETURN
 
    CASE s_logand: sw := NOT sw
    CASE s_logor:  TEST sw THEN { jumpcond(h2!x, b, L)
                                  jumpcond(h3!x, b, L)
                                  RETURN
                                }
 
                           ELSE { LET M = genlab()
                                  jumpcond(h2!x, NOT b, M)
                                  jumpcond(h3!x, b, L)
                                  out2(s_lab, M)
                                  RETURN
                                }
 
    DEFAULT:       load(x, FALSE)
                   out2(b -> s_jt, s_jf, L)
                   ssp := ssp - 1
                   RETURN
  }
}
 
AND transswitch(x, next) BE
{ LET cl, cc = caselist, casecount     // These must be saved and restored 
  LET dl, el = defaultlab, endcaselab
  
  LET L, dlab = genlab(), ?
  
  caselist, casecount, defaultlab := 0, 0, 0
  endcaselab := next
  UNLESS endcaselab DO endcaselab := genlab()
  
  context, comline := x, h4!x

  load(h2!x, FALSE)  // Evaluate the switch expression

  out2(s_res, L) // Make a jump to the end of the switch
                 // with the switch expression in<res>
  ssp := ssp-1

  // Compile the switch body collecting the case label data
  trans(h3!x, endcaselab)
 
  context, comline := x, h4!x
  out2(s_lab, L)      // The switch value is on the top of the stack
  out2(s_rstack, ssp) // Load <res> onto the top of the stack
  ssp := ssp+1

  dlab := defaultlab>0 -> defaultlab,
          endcaselab>0 -> endcaselab,
          genlab()

  // The switch expression value is on the top of the stack
  out2(s_switchon, casecount); out1(dlab) 
  WHILE caselist DO { out2(h2!caselist, h3!caselist)
                      caselist := h1!caselist
                    }
  ssp := ssp-1

  IF next=0                DO   out2(s_lab, endcaselab)
  IF next<0 & defaultlab=0 DO { out2(s_lab, dlab)
                                out1(s_rtrn)
                              }

  defaultlab, endcaselab := dl, el
  caselist,   casecount  := cl, cc
}
 
AND transfor(x, next) BE
{ // x -> [s_for, N, initval, lim, step, c, ln]
  LET e, m, blab = dvece, genlab(), 0
  LET bl, ll = breaklab, looplab
  LET cc = casecount
//  LET el = endcaselab
  LET k, n, step = 0, 0, 1
  LET s = ssp
  LET name = h2!x

  casecount := -1  // Disallow CASE and DEFAULT labels.   
  breaklab, looplab := next, 0
   
  context, comline := x, h7!x
 
  IF h1!name=s_flt DO
  { trnerr("FOR loop control variable must not have the FLT tag")
    name := h2!name  // Remove the FLT tag
    h2!x := name
  }

  addname(name, s_local, s, 0)
  load(h3!x, FALSE)       // The initial value

  // Set k, n to be the instruction to load the end limit, if there is one
  // k is zero if no end limit was specified.???
  IF h4!x TEST h1!(h4!x)=s_number
          THEN   k, n := s_ln, h2!(h4!x)
          ELSE { k, n := s_lp, ssp
                 load(h4!x, FALSE) // Place the end limit in the stack
               }
  // k=0 if there is no TO expression
  
  IF h5!x DO step := evalconst(h5!x, FALSE) // Set step if BY given
 
  out1(s_store)  // Ensure the control variable and possible end limit
                 // is stored in memory
   
  TEST k=s_ln & h1!(h3!x)=s_number  // check for constant limit expression
  THEN { // The initial and limit values are both constants 
         LET initval = h2!(h3!x)
         IF step>=0 & initval>n | step<0 & initval<n DO
         { // The body of this FOR loop will not be executed
	   TEST next<0
           THEN out1(s_rtrn)
           ELSE TEST next>0
                THEN out2(s_jump, next)
                ELSE { blab := breaklab>0 -> breaklab, genlab()
                       out2(s_jump, blab)
                     }
         }
       }
  ELSE { IF next<=0 DO blab := genlab()
         // Only perform a conditional jump if the TO expression was given.
	 IF k DO
         { out2(s_lp, s)
           out2(k, n)
           out1(step>=0 -> s_gr, s_ls)
           out2(s_jt, next>0 -> next, blab)
	 }
       }

  IF breaklab=0 & blab>0 DO breaklab := blab
   
  context, comline := x, h7!x
  out2(s_lab, m)
  decllabels(h6!x)
  trans(h6!x, 0)   // Translate the body of the for loop.
  UNLESS looplab=0 DO out2(s_lab, looplab)
  // Compile code to increment the control variable
  out2(s_lp, s); out2(s_ln, step); out1(s_add); out2(s_sp, s)
  TEST k
  THEN { // If the TO expression is given compile the conditional jump.
         out2(s_lp,s); out2(k,n); out1(step>=0 -> s_le, s_ge)
         out2(s_jt, m)
       }
  ELSE { // No TO expression given so compile an unconditional jump
         out2(s_jump, m)
       }
 
  IF next<=0 TEST blab>0 
             THEN                  out2(s_lab, blab)
             ELSE IF breaklab>0 DO out2(s_lab, breaklab)
  trnext(next)
  casecount := cc
  breaklab, looplab, ssp := bl, ll, s
  out2(s_stack, ssp)
  undeclare(e)
}

LET isflt(x) = x=0 -> FALSE, VALOF
{ // Return TRUE if expression x is an fnumber, a name declared
  // with the FLT tag or has a leading operator such as #+ or #-
  // that returns a floating point value. Remember the operators
  // such as + and - are converted to #+ and #- if they have
  // floating point operands.
  // Added 28 Feb 2023: If x is a function call f(args) return
  // TRUE if isflt(f) is TRUE, iereturn isflt(h2!x).
  SWITCHON h1!x INTO
  { DEFAULT:  RESULTIS FALSE

    CASE s_name: { LET c = cellwithname(x)
                   IF (h2!c & s_fltbit)=0 RESULTIS FALSE
                   RESULTIS TRUE
                 }

    CASE s_float: CASE s_fabs:
    CASE s_fpos:  CASE s_fneg:
    CASE s_fadd:  CASE s_fsub:
    CASE s_fmul:  CASE s_fdiv: CASE s_fmod:
    CASE s_fcond:
    CASE s_fnum:  RESULTIS TRUE

    CASE s_neg: CASE s_abs:
    CASE s_fnap:             // Added 28 Feb 2023
      RESULTIS isflt(h2!x)

    CASE s_add: CASE s_sub:
    CASE s_mul: CASE s_div: CASE s_mod:
      IF isflt(h2!x) | isflt(h3!x) RESULTIS TRUE
      RESULTIS FALSE

    CASE s_cond:
      IF isflt(h3!x) | isflt(h4!x) RESULTIS TRUE
      RESULTIS FALSE
  }    
}
 
LET load(x, ff) BE
{ // Translate expression x into Ocode.
  // This will add one to ssp.
  // If ff=TRUE, the expression is in an FLT context and will
  // convert, for example, + and - to #+ and #-.
  LET op = h1!x

  IF isconst(x) DO
  { out2(s_ln, evalconst(x, ff | isflt(x)))
    ssp := ssp + 1
    RETURN
  }
 
  SWITCHON op INTO
  { DEFAULT:
           trnerr("Compiler error in Load, op=%s", opname(op))
           out2(s_ln, 0)
           ssp := ssp + 1
           RETURN

    CASE s_break:   genjumpoptodest(s_jump, @breaklab);   ssp:=ssp+1;RETURN
    CASE s_loop:    genjumpoptodest(s_jump, @looplab);    ssp:=ssp+1;RETURN
    CASE s_endcase: genjumpoptodest(s_jump, @endcaselab); ssp:=ssp+1;RETURN
    CASE s_next:    genjumpoptodest(s_jump, @nextlab);    ssp:=ssp+1;RETURN
    CASE s_exit:    genjumpoptodest(s_jump, @exitlab);    ssp:=ssp+1;RETURN

    CASE s_of:
         { LET slct = evalconst(h2!x, FALSE) // Inserted 11/7/01
           LET len = slct>>24
           LET sh  = slct>>16 & 255
           LET offset = slct & #xFFFF
           load(h3!x, FALSE)
	   
           IF offset DO
           { out2(s_ln, offset)
             out1(s_add)
           }

           // Optimise accessing a complete word.
           IF sh=0 & (len=0 | len=wordbitlen) DO
           { out1(s_rv) // The source field is a complete word
             RETURN
           }

           // Compile (SLCT len:sh:0)(E+offset)
           TEST noselst
           THEN { // Old version not using SELLD
                  out1(s_rv)
                  IF sh DO
                  { out2(s_ln, sh)
                    out1(s_rshift)
                  }
                  IF len>0 & (len+sh~=wordbitlen) DO
                  { // Applying a mask is necessary
                    LET mask = (1<<len)-1
                    out2(s_ln, mask)
                    out1(s_logand)
                  }
                }
           ELSE { // New version using SELLD
                  out3(s_selld, len, sh)
                }
           RETURN
         }

    CASE s_div: CASE s_mod: CASE s_sub:
         // Convert to floating point if in FLT mode or
         // has a floating point operand.
         IF ff | isflt(x) DO
         { // Convert to floating point operators.
           h1!x := cv2flt(op)
           load(x, TRUE)
           RETURN
         }
         load(h2!x, FALSE)
         load(h3!x, FALSE)
         out1(op)
         ssp := ssp - 1
         RETURN
                      
    CASE s_fdiv: CASE s_fmod: CASE s_fsub:
         load(h2!x, TRUE)
         load(h3!x, TRUE)
         out1(op)
         ssp := ssp - 1
         RETURN

    CASE s_ls: CASE s_gr: CASE s_le: CASE s_ge:
         // Only convert to floating point if they have
         // a floating point operand.
         IF isflt(h2!x) | isflt(h3!x) DO
         { // Convert to floating point operators.
           h1!x := cv2flt(op)
           load(x, TRUE)
           RETURN
         }
         load(h2!x, FALSE)
         load(h3!x, FALSE)
         out1(op)
         ssp := ssp - 1
         RETURN

    CASE s_fls: CASE s_fgr: CASE s_fle: CASE s_fge:
         load(h2!x, TRUE)
         load(h3!x, TRUE)
         out1(op)
         ssp := ssp - 1
         RETURN
 
    CASE s_byteap:
         load(h2!x, FALSE)
         load(h3!x, FALSE)
         out1(s_getbyte)    // Compiling: E1%E2
         ssp := ssp - 1
         RETURN

    CASE s_lshift: CASE s_rshift:
         load(h2!x, FALSE)  // Compiling: E1<<E2  or  E1>>E2
         UNLESS iszero(h3!x,FALSE) DO
         { load(h3!x, FALSE)
           out1(op)
           ssp := ssp - 1
         }
         RETURN
 
    CASE s_eq: CASE s_ne:
         // Relational operators are only converted if they
         // have floating point operands.
         IF isflt(h2!x) | isflt(h3!x) DO
         { // Convert to floating point.
           h1!x := cv2flt(op)
           load(x, TRUE)
           RETURN
         }
         GOTO intsymmetric

    CASE s_mul: CASE s_add:
          // Convert to floating point if in FLT mode or
          // has a floating point operand.
         IF ff | isflt(x) DO
         { h1!x := cv2flt(op)
           load(x, TRUE)
           RETURN
         }
         // Fall through

    CASE s_vecap:
    CASE s_logand: CASE s_logor: CASE s_eqv: CASE s_xor:
intsymmetric:
       // Symmetric non FLT dyadic operators.
       { LET a, b = h2!x, h3!x
         TEST h1!a=s_name   |
              h1!a=s_number |
              h1!a=s_fnum THEN { load(b, FALSE); load(a, FALSE) }
                          ELSE { load(a, FALSE); load(b, FALSE) }
         TEST op=s_vecap THEN out2(s_add, s_rv)
                         ELSE out1(op)
         ssp := ssp - 1
         RETURN
       }
 
    CASE s_fmul: CASE s_fadd: CASE s_feq: CASE s_fne:
       { LET a, b = h2!x, h3!x
         TEST h1!a=s_name   |
              h1!a=s_number |
              h1!a=s_fnum THEN { load(b, TRUE); load(a, TRUE) }
                          ELSE { load(a, TRUE); load(b, TRUE) }
         out1(op)
         ssp := ssp - 1
         RETURN
       }
 
    CASE s_neg: CASE s_abs:
       IF ff | isflt(x) DO
       { h1!x := cv2flt(op)
         load(x, TRUE)
         RETURN
      }
      load(h2!x, FALSE)
      out1(op)
      RETURN

    CASE s_fneg: CASE s_fabs:CASE s_fix:
      load(h2!x, TRUE)
      out1(op)
      RETURN
 
    CASE s_float: CASE s_not: CASE s_rv:
      load(h2!x, FALSE)
      out1(op)
      RETURN
 
    CASE s_true: CASE s_false: CASE s_query:
       out1(op)
       ssp := ssp + 1
       RETURN
 
    CASE s_lv:
       loadlv(h2!x)
       RETURN
 
    CASE s_number:
//sawritef("number %n  ff=%n*n", h2!x, ff)
       IF ff DO
       { // Convert the integer constant to floating point
         h1!x := s_fnum
         h2!x := sys(Sys_flt, fl_mk, h2!x, 0)
//sawritef("number converted to fnumber %13e*n", h2!x)
       }
       // Fall through
    CASE s_fnum:
       out2(s_ln, h2!x)
       ssp := ssp + 1
       RETURN
 
    CASE s_string:
       out1(s_lstr)
       outstring(@ h2!x)
       ssp := ssp + 1
       RETURN
 
    CASE s_name:
       transname(x, s_lp, s_lg, s_ll, s_lf, s_ln)
       ssp := ssp + 1
       RETURN
 
    CASE s_valof:
     { LET e, rl, el, cc = dvece, resultlab, endcaselab, casecount
       casecount := -2 // Disallow CASE & DEFAULT labels
       resultlab := genlab()
       decllabels(h2!x)
       trans(h2!x, 0)
       out2(s_lab, resultlab)
       out2(s_rstack, ssp)
       ssp := ssp + 1
       resultlab, endcaselab, casecount := rl, el, cc
       undeclare(e)
       RETURN
     }
 
    CASE s_matche:
    CASE s_everye:
    { // This is an expression but has much in common with a SWITCHON
      // command in that is can select one of many alternative and can
      // use various simple commands to escape from a match item
      // or the entire matchlist. These commands are EXIT, NEXT,
      // BREAK, LOOP, ENDCASE and RESULTIS, provided the jump does
      // not leave the current function or routine.
      
      LET argpos = ssp+1 // Position relative to P of first argument
                         // leaving space for the result location.
//writef("transmatchlist: matche/everye ssp=%n*n", ssp)

      // Allocate a stack location for the result
      patresultpos := ssp
      ssp := argpos
      out2(s_stack, ssp)
      out1(s_store)
      
      context, comline := x, h5!x

      // Load the MATCHe or EVERYe arguments
      loadlist(h2!x)
      out1(s_store) // Ensure that the arguments are in memory

      // Translate the match items
      transmatchlist(op,     // Context is s_matche or s_everye
                     h3!x,   // mlist -> [matchiteme, plist, E, link, ln]
                     argpos) // Position of the first match arg
      RETURN
    }
 
    
    CASE s_fnap:
     { LET s = ssp
       ssp := ssp + savespacesize
       out2(s_stack, ssp)
       loadlist(h3!x) // Load arguments in non FLT mode
       load(h2!x, FALSE)
       out2(s_fnap, s)
       ssp := s + 1
       RETURN
     }

    CASE s_fcond:
       ff := TRUE
       GOTO cond

    CASE s_cond:
       IF ff | isflt(x) DO
       { h1!x := s_fcond
         load(x, TRUE)
         RETURN
       }
cond:
     { LET l, m = genlab(), genlab()
       LET s = ssp
       jumpcond(h2!x, FALSE, m)
       load(h3!x, ff)
       out2(s_res,l)
       ssp := s; out2(s_stack, ssp)
       out2(s_lab, m)
       load(h4!x, ff)
       out2(s_res,l)
       out2(s_lab, l)
       out2(s_rstack,s)
       RETURN
     }
 
    CASE s_table:
     { LET m = genlab()
       out2(s_datalab, m)
       x := h2!x
       WHILE h1!x=s_comma DO
       { out2(s_itemn, evalconst(h2!x, FALSE))
         x := h3!x
       }
       out2(s_itemn, evalconst(x, FALSE))
       out2(s_lll, m)
       ssp := ssp + 1
       RETURN
     }
  }
}

AND fnbody(x, ff) BE SWITCHON h1!x INTO
{ // If ff is TRUE  compile x in FLT mode
  // If ff is FALSE compile x in non FLT mode
  // followed by FNRN
  DEFAULT:         load(x, ff)
                   out1(s_fnrn)
                   ssp := ssp - 1
                   RETURN
                   
  CASE s_valof: { LET e, rl, cc = dvece, resultlab, casecount
                  casecount := -1 // Disallow CASE & DEFAULT labels
                  resultlab := -1 // RES replaced by FNRN
                  decllabels(h2!x)
                  trans(h2!x, -1)
                  resultlab, casecount := rl, cc
                  undeclare(e)
                  RETURN
                }

  CASE s_fcond: { LET l = genlab()
                  jumpcond(h2!x, FALSE, l)
                  fnbody(h3!x, TRUE)
                  out2(s_lab, l)
                  fnbody(h4!x, TRUE)
                  RETURN
                }

  CASE s_cond:  { LET l = genlab()
                  IF ff | isflt(x) DO
                  { h1!x := s_fcond    // Replace -> by #->
                    fnbody(x, TRUE)
                    RETURN
                  }
                  jumpcond(h2!x, FALSE, l)
                  fnbody(h3!x, ff)
                  out2(s_lab, l)
                  fnbody(h4!x, ff)
                }
}
 
 
AND loadlv(x) BE
{ UNLESS x=0 SWITCHON h1!x INTO
  { DEFAULT:         ENDCASE
 
    CASE s_name:     transname(x, s_llp, s_llg, s_lll, 0, 0)
                     ssp := ssp + 1
                     RETURN
 
    CASE s_rv:       load(h2!x, FALSE)
                     RETURN
 
    CASE s_vecap: { LET a, b = h2!x, h3!x
                    TEST h1!a=s_name   |
                         h1!a=s_number |
                         h1!a=s_fnum THEN { load(b, FALSE); load(a, FALSE) }
                                     ELSE { load(a, FALSE); load(b, FALSE) }
                    out1(s_add)
                    ssp := ssp - 1
                    RETURN
                  }
  }

  trnerr("Ltype expression needed")
  out2(s_ln, 0)
  ssp := ssp + 1
}
 
AND loadlist(x) BE
{ // Load function, routine or MATCH arguments
  UNLESS x=0 TEST h1!x=s_comma
             THEN { loadlist(h2!x); loadlist(h3!x) }
             ELSE load(x, FALSE)
}

// The conversion function are:
//    op2sfop      convert an expression op to a selst sfop
//    assop2op     convert op:= to op
//    cv2flt       convert an integer op or assignment op
//                 to the floating point version.
//    patrel2rel convert a pattern relation to an ordinary relation

AND op2sfop(op) = VALOF SWITCHON op INTO
{ DEFAULT:       sawritef("Syserr in op2sfop op=%s not in switch*n",
                          opname(op))
                 RESULTIS op

  CASE s_none:   RESULTIS sf_none

  CASE s_vecap:  RESULTIS sf_vecap

  CASE s_mul:    RESULTIS sf_mul
  CASE s_div:    RESULTIS sf_div
  CASE s_mod:    RESULTIS sf_mod
  CASE s_add:    RESULTIS sf_add
  CASE s_sub:    RESULTIS sf_sub

  CASE s_fmul:   RESULTIS sf_fmul
  CASE s_fdiv:   RESULTIS sf_fdiv
  CASE s_fmod:   RESULTIS sf_fmod
  CASE s_fadd:   RESULTIS sf_fadd
  CASE s_fsub:   RESULTIS sf_fsub

  CASE s_lshift: RESULTIS sf_lshift
  CASE s_rshift: RESULTIS sf_rshift
  CASE s_logand: RESULTIS sf_logand
  CASE s_logor:  RESULTIS sf_logor
  CASE s_eqv:    RESULTIS sf_eqv
  CASE s_xor:    RESULTIS sf_xor

}

AND assop2op(op) = VALOF SWITCHON op INTO
{ DEFAULT:       sawritef("Syserr in assop2op unknown op=%s*n",
                          opname(op))
                 RESULTIS op

//  CASE  0:       RESULTIS 0

  CASE s_assfmul:   RESULTIS s_fmul
  CASE s_assfdiv:   RESULTIS s_fdiv
  CASE s_assfmod:   RESULTIS s_fmod
  CASE s_assfadd:   RESULTIS s_fadd
  CASE s_assfsub:   RESULTIS s_fsub

  CASE s_assmul:    RESULTIS s_mul
  CASE s_assdiv:    RESULTIS s_div
  CASE s_assmod:    RESULTIS s_mod
  CASE s_assadd:    RESULTIS s_add
  CASE s_asssub:    RESULTIS s_sub

  CASE s_assvecap:  RESULTIS s_vecap
  CASE s_asslshift: RESULTIS s_lshift
  CASE s_assrshift: RESULTIS s_rshift
  CASE s_asslogand: RESULTIS s_logand
  CASE s_asslogor:  RESULTIS s_logor
  CASE s_asseqv:    RESULTIS s_eqv
  CASE s_assxor:    RESULTIS s_xor

  CASE s_fass:
  CASE s_ass:       RESULTIS s_none
}

AND rel2patrel(op) = VALOF SWITCHON op INTO
{ DEFAULT:    writef("SYSTEM ERROR: in rel2patrel op=%s*n",
                     opname(op))
	      abort(999)
	      RESULTIS s_pateq
	      
  CASE s_eq:  RESULTIS s_pateq
  CASE s_feq: RESULTIS s_patfeq
  CASE s_ne:  RESULTIS s_patne
  CASE s_fne: RESULTIS s_patfne
  CASE s_le:  RESULTIS s_patle
  CASE s_fle: RESULTIS s_patfle
  CASE s_ge:  RESULTIS s_patge
  CASE s_fge: RESULTIS s_patfge
  CASE s_ls:  RESULTIS s_patls
  CASE s_fls: RESULTIS s_patfls
  CASE s_gr:  RESULTIS s_patgr
  CASE s_fgr: RESULTIS s_patfgr
}

AND patrel2rel(op) = VALOF SWITCHON op INTO
{ DEFAULT:    writef("SYSTEM ERROR: in patrel2rel op=%s*n",
                     opname(op))
	      abort(999)
	      RESULTIS s_eq
	      
  CASE s_pateq:  RESULTIS s_eq
  CASE s_patfeq: RESULTIS s_feq
  CASE s_patne:  RESULTIS s_ne
  CASE s_patfne: RESULTIS s_fne
  CASE s_patle:  RESULTIS s_le
  CASE s_patfle: RESULTIS s_fle
  CASE s_patge:  RESULTIS s_ge
  CASE s_patfge: RESULTIS s_fge
  CASE s_patls:  RESULTIS s_ls
  CASE s_patfls: RESULTIS s_fls
  CASE s_patgr:  RESULTIS s_gr
  CASE s_patfgr: RESULTIS s_fgr
}

AND cv2flt(op) = VALOF SWITCHON op INTO
{ DEFAULT:       sawritef("Syserr in cv2flt op=%s not in switch*n",
                          opname(op))
                 RESULTIS op

  // Expression operators
  CASE s_neg:    RESULTIS s_fneg
  CASE s_abs:    RESULTIS s_fabs
  CASE s_number: RESULTIS s_fnum
  CASE s_mul:    RESULTIS s_fmul
  CASE s_div:    RESULTIS s_fdiv
  CASE s_mod:    RESULTIS s_fmod
  CASE s_add:    RESULTIS s_fadd
  CASE s_sub:    RESULTIS s_fsub
  CASE s_eq:     RESULTIS s_feq
  CASE s_ne:     RESULTIS s_fne
  CASE s_ls:     RESULTIS s_fls
  CASE s_gr:     RESULTIS s_fgr
  CASE s_le:     RESULTIS s_fle
  CASE s_ge:     RESULTIS s_fge
  CASE s_cond:   RESULTIS s_fcond

  // Pattern operators
  CASE s_range:  RESULTIS s_frange
  CASE s_pateq:  RESULTIS s_patfeq
  CASE s_patne:  RESULTIS s_patfne
  CASE s_patls:  RESULTIS s_patfls
  CASE s_patgr:  RESULTIS s_patfgr
  CASE s_patle:  RESULTIS s_patfle
  CASE s_patge:  RESULTIS s_patfge
  
  // Assignment operators
  CASE s_assmul: RESULTIS s_assfmul
  CASE s_assdiv: RESULTIS s_assfdiv
  CASE s_assmod: RESULTIS s_assfmod
  CASE s_assadd: RESULTIS s_assfadd
  CASE s_asssub: RESULTIS s_assfsub
  CASE s_ass:    RESULTIS s_fass
}

LET isconst(x) = VALOF
{ // Return TRUE if the expression x has a value that can
  // be determined at compile time. These are manifest names,
  // integer or floating point constants, the SLCT construct,
  // TRUE or FALSE, and any expression whose operands are constants
  // other than rv, vecap or byteap expressions.

  IF x=0 RESULTIS FALSE
 
  SWITCHON h1!x INTO
  { CASE s_name:
        { LET c = cellwithname(x)
          LET k = h2!c & s_fltmask
          RESULTIS k=s_manifest -> TRUE, FALSE
        }

    CASE s_fnum:
    CASE s_number:
    CASE s_slct:
    CASE s_true:
    CASE s_false:  RESULTIS TRUE
 
    CASE s_fneg:
    CASE s_fabs:
    CASE s_float:
    CASE s_fix:
    CASE s_neg:
    CASE s_abs:
    CASE s_not:    RESULTIS isconst(h2!x)
       
    CASE s_fmul:
    CASE s_fdiv:
    CASE s_fmod:
    CASE s_fadd:
    CASE s_fsub:
    CASE s_feq:
    CASE s_fne:
    CASE s_fls:
    CASE s_fgr:
    CASE s_fle:
    CASE s_fge:

    CASE s_mul:
    CASE s_div:
    CASE s_mod:
    CASE s_add:
    CASE s_sub:
    CASE s_lshift:
    CASE s_rshift:
    CASE s_logor:
    CASE s_logand:
    CASE s_eqv:
    CASE s_xor:
    CASE s_eq:
    CASE s_ne:
    CASE s_ls:
    CASE s_gr:
    CASE s_le:
    CASE s_ge:
                   UNLESS isconst(h2!x) RESULTIS FALSE
                   RESULTIS isconst(h3!x)

    CASE s_fcond:
    CASE s_cond:   UNLESS isconst(h2!x) RESULTIS FALSE
                   UNLESS isconst(h3!x) RESULTIS FALSE
                   RESULTIS isconst(h4!x)

    DEFAULT:       RESULTIS FALSE

  }
}

LET iszero(x, ff) = isconst(x) & evalconst(x, ff)=0 -> TRUE, FALSE

LET evalconst(x, ff) = VALOF
{ // If ff=TRUE the expression x is to be evaluated in
  // an FLT context, causing integer expression operators
  // to be automatically converted to their floating
  // point versions. Integer constants are also converted
  // to floating point.

  LET op, a, b = 0, 0, 0

  IF x=0 DO { trnerr("Compiler error in Evalconst")
              RESULTIS 0
            }

  IF isflt(x) DO ff := TRUE
 
  op := h1!x
//sawritef("evalconst: op=%s ff=%n*n", opname(op), ff)

  SWITCHON op INTO
  { CASE s_name: { LET c = cellwithname(x)
                   LET k = h2!c
                   LET a = h3!c
                   IF (k & s_fltmask)=s_manifest DO
                   { IF xrefing DO
                       xref(x,
                            (k=s_manifest -> "M:", "FM:"),
                            a, s_const)
                     RESULTIS a
                   }
                   TEST k
                   THEN trnerr("%s must be a MANIFEST constant", @h3!x)
                   ELSE trnerr("Name '%s' is not declared", @h3!x)
                   RESULTIS 0
                 }
 
    CASE s_number: UNLESS ff RESULTIS h2!x
                   // Convert from integer to floating point.
                   h1!x := s_fnum
                   h2!x := sys(Sys_flt, fl_mk, h2!x, 0)

    CASE s_fnum:   RESULTIS h2!x

    CASE s_true:   RESULTIS TRUE
    CASE s_false:  RESULTIS FALSE
    CASE s_query:  RESULTIS 0
 
    CASE s_slct: { LET len, sh, offset = 0, 0, 0     // Inserted 11/7/01
                   IF h2!x DO len    := evalconst(h2!x, FALSE)
                   IF h3!x DO sh     := evalconst(h3!x, FALSE)
                   IF h4!x DO offset := evalconst(h4!x, FALSE)
                   UNLESS 0<=len<=255 & 0<=sh<=255 & 0<=offset<=#xFFFF DO
                       trnerr("A field too large in a SLCT expression")
                   RESULTIS len<<24 | sh<<16 | offset
                 }

    CASE s_fneg:
    CASE s_fabs:
    CASE s_fix:    floatingchk()
                   a := evalconst(h2!x, TRUE)
                   ENDCASE

    CASE s_neg:
    CASE s_abs:    IF ff | isflt(x) DO 
                   { h1!x := cv2flt(op)
                     RESULTIS evalconst(x, TRUE)
                   }
                   a := evalconst(h2!x, FALSE)
                   ENDCASE

    CASE s_not:       
    CASE s_float:  a := evalconst(h2!x, FALSE)
                   ENDCASE

    CASE s_fmul:
    CASE s_fdiv:
    CASE s_fmod:
    CASE s_fadd:
    CASE s_fsub:
    CASE s_feq:
    CASE s_fne:
    CASE s_fls:
    CASE s_fgr:
    CASE s_fle:
    CASE s_fge:  floatingchk()
                 a, b := evalconst(h2!x, TRUE), evalconst(h3!x, TRUE)
                 ENDCASE

    CASE s_mul:
    CASE s_div:
    CASE s_mod:
    CASE s_add:
    CASE s_sub:  IF ff | isflt(x) DO
                 { // Convert to floating point.
                   h1!x := cv2flt(op)
                   RESULTIS evalconst(x, TRUE)
                 }
                 a, b := evalconst(h2!x, FALSE), evalconst(h3!x, FALSE)
                 ENDCASE

    CASE s_eq:
    CASE s_ne:
    CASE s_ls:
    CASE s_gr:
    CASE s_le:
    CASE s_ge:   // Only convert to floating point if there is
                 // a floating point operand.
                 IF isflt(h2!x) | isflt(h3!x) DO
                 { // Convert to floating point.
                   h1!x := cv2flt(op)
                   RESULTIS evalconst(x, TRUE)
                 }
                 a, b := evalconst(h2!x, FALSE), evalconst(h3!x, FALSE)
                 ENDCASE


    CASE s_lshift:
    CASE s_rshift:
    CASE s_logor:
    CASE s_logand:
    CASE s_eqv:
    CASE s_xor:    a, b := evalconst(h2!x, FALSE), evalconst(h3!x, FALSE)
                   ENDCASE

    CASE s_fcond:  a, b := evalconst(h2!x, TRUE), evalconst(h3!x, TRUE)
                   ENDCASE

    CASE s_cond:   IF ff | isflt(x) DO
                   { // Convert to floating point.
                     h1!x := s_fcond
                     RESULTIS evalconst(x, TRUE)
                   }
                   a, b := evalconst(h2!x, FALSE), evalconst(h3!x, FALSE)
                   ENDCASE

    DEFAULT:
  }
    
  SWITCHON h1!x INTO
  { CASE s_neg:    RESULTIS  -  a
    CASE s_abs:    RESULTIS ABS a
    CASE s_not:    RESULTIS NOT a
       
    CASE s_fneg:   RESULTIS sys(Sys_flt, fl_neg,   a)
    CASE s_fabs:   RESULTIS sys(Sys_flt, fl_abs,   a)
    CASE s_fix:    RESULTIS sys(Sys_flt, fl_fix,   a)
    CASE s_float:  RESULTIS sys(Sys_flt, fl_float, a)
       
    CASE s_fmul:   RESULTIS sys(Sys_flt, fl_mul, a,  b)
    CASE s_fdiv:   RESULTIS sys(Sys_flt, fl_div, a,  b)
    CASE s_fmod:   RESULTIS sys(Sys_flt, fl_mod, a,  b)
    CASE s_fadd:   RESULTIS sys(Sys_flt, fl_add, a,  b)
    CASE s_fsub:   RESULTIS sys(Sys_flt, fl_sub, a,  b)

    CASE s_feq:    RESULTIS sys(Sys_flt, fl_eq, a,  b)
    CASE s_fne:    RESULTIS sys(Sys_flt, fl_ne, a,  b)
    CASE s_fls:    RESULTIS sys(Sys_flt, fl_ls, a,  b)
    CASE s_fgr:    RESULTIS sys(Sys_flt, fl_gr, a,  b)
    CASE s_fle:    RESULTIS sys(Sys_flt, fl_le, a,  b)
    CASE s_fge:    RESULTIS sys(Sys_flt, fl_ge, a,  b)

    CASE s_mul:    RESULTIS a   *   b
    CASE s_add:    RESULTIS a   +   b
    CASE s_sub:    RESULTIS a   -   b
    CASE s_lshift: RESULTIS a   <<  b
    CASE s_rshift: RESULTIS a   >>  b
    CASE s_logor:  RESULTIS a   |   b
    CASE s_logand: RESULTIS a   &   b
    CASE s_eqv:    RESULTIS a  EQV  b
    CASE s_xor:    RESULTIS a  XOR  b
    CASE s_div:    RESULTIS b=0 -> 0, a  /  b
    CASE s_mod:    RESULTIS b=0 -> 0, a MOD b
    CASE s_eq:     RESULTIS a =  b
    CASE s_ne:     RESULTIS a ~= b
    CASE s_ls:     RESULTIS a <  b
    CASE s_gr:     RESULTIS a >  b
    CASE s_le:     RESULTIS a <= b
    CASE s_ge:     RESULTIS a >= b

    CASE s_cond:   RESULTIS a -> b, evalconst(h4!x, FALSE)
    CASE s_fcond:  RESULTIS a -> b, evalconst(h4!x, TRUE)
   
    DEFAULT:       ENDCASE
  }

  trnerr("Error in manifest expression, op = %s", opname(h1!x))
  RESULTIS 0
}

AND assign(lhs, rhs, ff, op) BE
// Compile a simple assignment: lhs := rhs, lhs #:= rhs or lhs op := rhs.
// Note that for simultaneous assignments have already been replaced by
// sequences of simple assignments by cvassign.

// If op=s_none the assigment is either lhs := rhs or lhs #:= rhs,
// otherwise it is of the form: lhs op:= rhs where op is one of
// the dyadic expression operators allowed in assignments, namely:
//    s_vecap,
//    s_mul,   s_div,  s_mod,  s_add,  s_sub,
//    s_fmul,  s_fdiv, s_fmod, s_fadd, s_fsub,
//    s_lshift, s_rshift, s_logand, s_logor, s_eqv or s_xor.

// ff=TRUE if the rhs is to be evaluated in FLT mode.

{ LET sfop = op2sfop(op)
  // sfop is either sf_none or
  // an operator used in SELST Ocode instructions, namely:
  //   sf_vecap,
  //   sf_mul,  sf_div,  sf_mod,  sf_add,    sf_sub,
  //   sf_fmul, sf_fdiv, sf_fmod, sf_fadd or sf_fsub.

  SWITCHON h1!lhs INTO
  { CASE s_name:        // name op:= E
      TEST op=s_none
      THEN { // Compile: name := E
             load(rhs, ff)
             transname(lhs, s_sp, s_sg, s_sl, 0, 0)
             ssp := ssp - 1
           }
      ELSE { // Compile: name sfop:= E
             TEST noselst
             THEN { // Load: lhs op rhs
                    // op is a dyadic operator
                    LET operator, a, b = op, lhs, rhs
                    load(@operator, ff)
                    transname(lhs, s_sp, s_sg, s_sl, 0, 0)
                    ssp := ssp - 1
                  }
             ELSE { load(rhs, ff)
                    loadlv(lhs)
                    out4(s_selst, sfop, 0, 0)
                    ssp := ssp - 2
                  }
           }
      RETURN
 
    CASE s_rv:
    CASE s_vecap:  IF op=s_none DO
                   { load(rhs, ff)
                     loadlv(lhs)
                     out1(s_stind)
                     ssp := ssp - 2
                     RETURN
                   }

                   // op is a dyadic expression operator allowed
                   // in assignments.
                   IF noselst DO
                   { // Load: lhs op rhs
                     // op is a dyadic operator
                     LET operator, a, b = op, lhs, rhs
                     // Load the expression whose tree node is [op,lhs,rhs]
                     load(@operator, ff)
                     loadlv(lhs)
                     out1(s_stind)
                     ssp := ssp - 2
                     RETURN
                   }

                   // Compile using SELST
                   load(rhs, ff)
                   loadlv(lhs)
                   out4(s_selst, sfop, 0, 0) 
                   ssp := ssp - 2
                   RETURN

    CASE s_of:   { LET slct = evalconst(h2!lhs, FALSE) // Inserted 11/7/01
                   LET len = slct>>24
                   LET sh  = slct>>16 & 255
                   LET offset = slct & #xFFFF
                   LET mask = -1
                   IF len>0 DO mask := (1<<len)-1
                   mask := mask<<sh
                   TEST noselst
                   THEN { TEST op=s_none
                          THEN { load(rhs, ff)
                               }
                          ELSE { // Load: lhs op rhs
                                 // op is a dyadic operator
                                 LET operator, a, b = op, lhs, rhs
                                 load(@operator, ff)
                               }
                          IF sh DO
                          { out2(s_ln, sh)
                            out1(s_lshift)
                          }

                          UNLESS mask=-1 DO
                          { load(h3!lhs, FALSE)
                            IF offset DO
                            { out2(s_ln, offset)
                              out1(s_add)
                            }
                            out1(s_rv)
                            out1(s_xor)
                            ssp := ssp-1
                            out2(s_ln, mask)
                            out1(s_logand) // bits to change in x
                            load(h3!lhs, FALSE)
                            IF offset DO
                            { out2(s_ln, offset)
                              out1(s_add)
                            }
                            out1(s_rv)
                            out1(s_xor)
                            ssp := ssp-1
                          }

                          load(h3!lhs, FALSE)
                          IF offset DO
                          { out2(s_ln, offset)
                            out1(s_add)
                          }
                          out1(s_stind)
                        }
                   ELSE { // Compile using SELST
                          load(rhs, ff)
                          load(h3!lhs, FALSE)
                          IF offset DO
                          { out2(s_ln, offset)
                            out1(s_add)
                          }
                          TEST len=0 & sh=0 & sfop=sf_none
                          THEN out1(s_stind) // Full word field
                                             // and no op.
                          ELSE out4(s_selst, sfop, len, sh) 
                        }
                   ssp := ssp-2
                   RETURN
                 }

    CASE s_byteap:
      TEST op=s_none
      THEN { // Compiling: E1%E2 := E3
             load(rhs, ff)
           }
      ELSE { // Compiling: E1%E2 op:= E3
             // op is a dyadic expression operator allowed
             // in assignments.
             LET operator, a, b = op, lhs, rhs

             // The destination is not a full word field,
             // so some operators are not permitted.
             IF sfop=sf_fmul | sfop=sf_fdiv | sfop=sf_fmod |
                sfop=sf_fadd | sfop=sf_fsub | sfop=sf_vecap DO
               trnerr("Bad op in E%E op:= E")

             // Load an expression whose tree node is [op,lhs,rhs]
             load(@operator, FALSE)
           }
      load(h2!lhs, FALSE)
      load(h3!lhs, FALSE)
      out1(s_putbyte)
      ssp:=ssp-3
      RETURN

    DEFAULT:
      trnerr("Ltype expression needed")
  }
}
 
 
AND transname(x, p, g, l, f, n) BE
{ // x is a name node
  // p is s_lp, s_llp or s_sp   It determines the code for path variables
  LET c = cellwithname(x)
  LET k, a, path = h2!c, h3!c, h4!c
 
  // Must deal with s_fglobal, s_flocal, s_f_static, s_flabel, s_fmanifest
  // as if they were the integer versions. The sole purpose of these
  // is to indicate than a name has been declared with the FLT tag.
  // Note that s_fglobal = s_global  + s_fltbit,
  // and       s_global  = s_fglobal & s_fltmask, etc
  // where s_fltbit is 128 s_fltmask is 127.

  SWITCHON k INTO
  { DEFAULT:        trnerr("Name '%s' not declared", @h3!x)
   
    CASE s_fglobal:
    CASE s_global:  out2(g, a)
                    IF xrefing DO
                      xref(x,
                           ((k & s_fltbit)=0 -> "G:", "FG:"),
                           a, g)
                    RETURN
 
    CASE s_flocal:
    CASE s_local:   IF c<dvecp DO
                         trnerr("Dynamic free variable '%s' used", @h3!x)
                    out2(p, a)
                    //IF xrefing DO
                    //  xref(x,
                    //       ((k & s_fltbit)=0 -> "P:", "FP:"),
                    //       a, p)
                    RETURN
 
    CASE s_path1: CASE s_fpath1:
    CASE s_path2: CASE s_fpath2:
    CASE s_path3: CASE s_fpath3:
    CASE s_path4: CASE s_fpath4:
//   writef("transname: %s op=%s k=%s n=%n path=%x8*n",
//           @h3!x, opname(p), opname(k), n, path)
//   abort(5522)
                    IF c<dvecp DO
                         trnerr("Dynamic free variable '%s' used", @h3!x)
                    oppath(p,                      // s_lp, s_llp or s_sp
		           a,                      // argpos
		           (k&s_fltmask)-s_path1+1,// depth
		           path)                   // path
//   abort(5523)
                    //IF xrefing DO
                    //  xref(x,
                    //       ((k & s_fltbit)=0 -> "T:", "FT:"),
                    //       a, p)
                    RETURN
 
    CASE s_fstatic:
    CASE s_static:  out2(l, a)
                    IF xrefing DO
                      xref(x,
                           ((k & s_fltbit)=0 -> "S:", "FS:"),
                           a, l)
                    RETURN
 
    CASE s_label:   IF f=0 DO
                    { trnerr("Misuse of entry name '%s'", @h3!x)
                      f := p
                    }
                    out2(f, a)
                    IF xrefing DO xref(x, "F:", a, f)
                    RETURN

    CASE s_fmanifest:
    CASE s_manifest:IF n=0 DO
                    { trnerr("Misuse of MANIFEST name '%s'", @h3!x)
                      n := p
                    }
                    out2(n, a)
                    IF xrefing DO
                      xref(x,
                           ((k & s_fltbit)=0 -> "M:", "FM:"),
                           a, n)
  }
}

AND xref(x, kstr, n, op) BE
{ // Output a line of cross reference info
  // x is the name node
  // kstr, n describe how the name is being used
  // op decribes the context
  LET name = @h3!x
  LET fno = comline>>20
  LET lno = comline & #xFFFFF
  LET file = sourcenamev!fno
  writef("%s %s", name, kstr)
  TEST -10_000_000 <= n <= 10_000_000
  THEN writef("%n ", n)
  ELSE writef("#x%8x ", n)

  SWITCHON op INTO
  { DEFAULT:         writef("op%n", op); ENDCASE

    CASE s_fndef:    writef("FN");       ENDCASE
    CASE s_rtdef:    writef("RT");       ENDCASE
    CASE s_local:    writef("LOC");      ENDCASE
    CASE s_valdef:   writef("VAL");      ENDCASE
    CASE s_vecdef:   writef("VEC");      ENDCASE
    CASE s_constdef: writef("DEF");      ENDCASE
    CASE s_const:    writef("MAN");      ENDCASE
    CASE s_colon:    writef("LAB");      ENDCASE
    CASE s_sp:       writef("SP");       ENDCASE
    CASE s_sg:       writef("SG");       ENDCASE
    CASE s_sl:       writef("SL");       ENDCASE
    CASE s_llp:      writef("LLP");      ENDCASE
    CASE s_llg:      writef("LLG");      ENDCASE
    CASE s_lll:      writef("LLL");      ENDCASE
    CASE s_lp:       writef("LP");       ENDCASE
    CASE s_lg:       writef("LG");       ENDCASE
    CASE s_ll:       writef("LL");       ENDCASE
    CASE s_lf:       writef("LF");       ENDCASE
    CASE s_ln:       writef("LN");       ENDCASE
  }
  wrch(' ')
  IF file DO writef("%s", file)
  writef("[%n] ", lno)

  prctxt(context)

  newline()
}

AND prctxt(x) BE IF x DO 
{ LET op, str = h1!x, ""

  SWITCHON op INTO
  { DEFAULT:  prctxte(x, 7, 0); RETURN

    CASE s_fndef:
         writef("LET ")
         prctxte(h2!x, 7, 0)
         wrch('(')
         prctxte(h3!x, 7, 0)
         writef(")=..")
         RETURN

    CASE s_rtdef:
         writef("LET ")
         prctxte(h2!x, 7, 0)
         wrch('(')
         prctxte(h3!x, 7, 0)
         writef(")BE..")
         RETURN

    CASE s_valdef:
         writef("LET ")
         prctxte(h2!x, 6, 0)
         writef("=")
         prctxte(h3!x, 6, 0)
         RETURN

    CASE s_vecdef:
         writef("LET ")
         prctxte(h2!x, 6, 0)
         writef("=VEC ")
         prctxte(h3!x, 6, 0)
         RETURN

    CASE s_constdef:
         prctxte(h3!x, 6, 0)
         writef("=")
         prctxte(h4!x, 6, 0)
         RETURN

    CASE s_let:
         writef("LET ")
         prctxtd(h2!x, 6)
         writef("; ")
         prctxtc(h3!x, 6)
         RETURN
 
    CASE s_static:    writef("STATIC..");    RETURN
    CASE s_global:    writef("GLOBAL..");    RETURN
    CASE s_manifest:  writef("MANIFEST..");  RETURN


    CASE s_assvecap:  str := "!";    GOTO case_ass
    CASE s_assfmul:   str := "#**";  GOTO case_ass
    CASE s_assfdiv:   str := "#/";   GOTO case_ass
    CASE s_assfmod:   str := "#MOD"; GOTO case_ass
    CASE s_assfadd:   str := "#+";   GOTO case_ass
    CASE s_assfsub:   str := "#-";   GOTO case_ass
    CASE s_assmul:    str := "**";   GOTO case_ass
    CASE s_assdiv:    str := "/";    GOTO case_ass
    CASE s_assmod:    str := "MOD";  GOTO case_ass
    CASE s_assadd:    str := "+";    GOTO case_ass
    CASE s_asssub:    str := "-";    GOTO case_ass
    CASE s_asslshift: str := "<<";   GOTO case_ass
    CASE s_assrshift: str := ">>";   GOTO case_ass
    CASE s_asslogand: str := "&";    GOTO case_ass
    CASE s_asslogor:  str := "|";    GOTO case_ass
    CASE s_asseqv:    str := "EQV";  GOTO case_ass
    CASE s_assxor:    str := "XOR";  GOTO case_ass

    CASE s_fass:      str := "#";    GOTO case_ass

    CASE s_ass:       str := ""
case_ass:
         prctxte(h2!x, 4, 0)
         writef("%s:=", str)
         prctxte(h3!x, 4, 0)
         RETURN
 
    CASE s_rtap:
         prctxte(h2!x, 6, 12)
         writef("(")
         prctxte(h3!x, 6, 0)
         writef(")")
         RETURN
 
    CASE s_goto:
         writef("GOTO ")
         prctxte(h2!x, 6, 0)
         RETURN
 
    CASE s_colon:
         prctxte(h2!x, 6, 0)
         writef(":")
         prctxt(h3!x, 6)
         RETURN
 
    CASE s_unless:
    CASE s_if:
    CASE s_while:
    CASE s_until:
         writef(op=s_unless->"UNLESS ",
                op=s_if->"IF ",
                op=s_until->"UNTIL ",
                "WHILE "
               )
         prctxte(h2!x, 6, 0)
         writef(" DO ")
         prctxtc(h3!x, 6)
         RETURN

 
    CASE s_test:
         writef("TEST ")
         prctxte(h2!x, 6, 0)
         writef(" THEN ")
         prctxtc(h3!x, 6)
         writef(" ELSE ")
         prctxtc(h4!x, 6)
         RETURN
 
    CASE s_loop:
         writef("LOOP")
         RETURN
 
    CASE s_exit:
         writef("EXIT")
         RETURN
 
    CASE s_next:
         writef("NEXT")
         RETURN
 
    CASE s_skip:
         writef("{}")
         RETURN
 
    CASE s_break:
         writef("BREAK")
         RETURN
 
    CASE s_return:
         writef("RETURN")
         RETURN
 
    CASE s_finish:
         writef("FINISH")
         RETURN
 
    CASE s_resultis:
         writef("RESULTIS ")
         prctxte(h2!x, 6, 0)
         RETURN
 
    CASE s_repeatwhile:
    CASE s_repeatuntil:
         prctxtc(h2!x, 6)
         writef(op=s_repeatwhile -> " REPEATWHILE ", " REPEATUNTIL ")
         prctxte(h3!x, 6, 0)
         RETURN
 
    CASE s_repeat:
         prctxtc(h2!x, 6)
         writef(" REPEAT")
         RETURN
 
    CASE s_case:
         writef("CASE ")
         prctxte(h2!x, 6, 0)
         writef(":.. ")
         RETURN
 
    CASE s_default:
         writef("DEFAULT:..")
         RETURN
 
    CASE s_endcase:
         writef("ENDCASE")
         RETURN
 
    CASE s_switchon:
         writef("SWITCHON ")
         prctxte(h2!x, 6, 0)
         writef(" INTO..")
         RETURN
 
    CASE s_for:
         writef("FOR ")
         prctxte(h2!x, 6, 0)
         writef("=")
         prctxte(h3!x, 6, 0)
         writef(" TO ")
         prctxte(h4!x, 6, 0)
         IF h5!x DO { writef(" BY "); prctxte(h5!x, 6, 0) }
         writef(" DO..")
         RETURN
 
    CASE s_seq:
         prctxtc(h2!x, 6)
         writef(";")
         prctxtc(h3!x, 6)
         RETURN
  }
}

AND prctxtd(x, d) BE writef("..")
AND prctxtc(x, d) BE writef("..")

AND wrhexval(n) BE
{ LET lsdig = n & #xFF
  n := n>>8
  IF n DO wrhexval(n)
  writef("%2x", lsdig)
}

AND prctxte(x, d, prec) BE IF x DO
{ LET op = h1!x

  SWITCHON op INTO
  { DEFAULT: ENDCASE

    CASE s_number: 
                 { LET n = h2!x
                   TEST -1_000_000<=n<=1_000_000
                   THEN writef("%n", n)
                   ELSE { IF n<0 DO
		          { wrch('-')
			    n := -n
			  }
		          writef("#x")
			  wrhexval(n)
			}
                   RETURN
                 } 
    
    CASE s_fnum: 
                 { LET n = h2!x
                   writef("%5.3f", n)
                   RETURN
                 } 
    
    CASE s_flt :   writef("FLT %s", @h3!(h2!x)); RETURN
    CASE s_name:   writef("%s", @h3!x);          RETURN
    CASE s_true:   writef("TRUE");               RETURN
    CASE s_false:  writef("FALSE");              RETURN
    CASE s_query:  wrch('?');                    RETURN

    CASE s_string: 
                 { LET s = @h2!x
                   LET len = s%0
                   wrch('"')
                   FOR i = 1 TO len DO
                   { LET ch = s%i
                     IF i=6 & len>6+8 DO { writef("'"); LOOP }
                     IF i<=6 | i>len-8 DO // First 5 and last 8 chars
                     { SWITCHON ch INTO
                       { CASE '**': writef("****"); LOOP
                         CASE '*"': writef("***""); LOOP
                         CASE '*n': writef("**n");  LOOP
                       }
                       UNLESS 32<=ch<=127 DO ch := '?'
                       wrch(ch)
                     }
                   }
                   wrch('"')
                   RETURN
                 }

  }

  IF d=0 DO { writef("..."); RETURN }

  IF prec>=12 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE

    CASE s_fnap:
         prctxte(h2!x, d-1, 11)
         wrch('(')
         prctxte(h3!x, d-1, 0)
         wrch(')')
         RETURN
  }

  IF prec>=11 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE

    CASE s_slct:
      {  writes("SLCT ")
         prctxte(h2!x, d-1, 10)
         writes(":")
         prctxte(h3!x, d-1, 10)
         writes(":")
         prctxte(h4!x, d-1, 10)
         RETURN
      }

    CASE s_of:
    CASE s_byteap:
    CASE s_vecap:
         prctxte(h2!x, d-1, 10)
         writes(op=s_of->"::", op=s_byteap->"%", "!")
         prctxte(h3!x, d-1, 10)
         RETURN

    CASE s_float:
    CASE s_fix:
    CASE s_rv:
    CASE s_lv:
         writef(op=s_float->"FLOAT ",
                op=s_fix  ->"FIX ",
                op=s_rv   ->"!",
                            "@")
         prctxte(h2!x, d-1, 10)
         RETURN
  }

  IF prec>=10 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_mul: CASE s_div: CASE s_mod:
         prctxte(h2!x, d-1, 9)
         writef(op=s_mul->"**", op=s_div->"/", " MOD ")
         prctxte(h3!x, d-1, 9)
         RETURN

    CASE s_fmul: CASE s_fdiv: CASE s_fmod:
         prctxte(h2!x, d-1, 9)
         writef(op=s_fmul -> "#**",
                op=s_fdiv -> "#/",
                             "#MOD ")
         prctxte(h3!x, d-1, 9)
         RETURN
  }

  IF prec>=9 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_add:
    CASE s_sub:
         prctxte(h2!x, d-1, 8)
         writef(op=s_add->"+","-")
         prctxte(h3!x, d-1, 8)
         RETURN

    CASE s_fadd:
    CASE s_fsub:
         prctxte(h2!x, d-1, 8)
         writef(op=s_fadd->"#+","#-")
         prctxte(h3!x, d-1, 8)
         RETURN

    CASE s_neg:
    CASE s_fneg:
    CASE s_fabs:
    CASE s_abs:
         writef(op=s_neg ->"-",
                op=s_fneg->"#-",
                op=s_fabs->"#ABS ",
                           "ABS ")
         prctxte(h2!x, d-1, 8)
         RETURN
  }

  IF prec>=8 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_eq: CASE s_ne:
         prctxte(h2!x, d-1, 7)
         writef(op=s_eq->"=","~=")
         prctxte(h3!x, d-1, 7)
         RETURN

    CASE s_feq: CASE s_fne:
         prctxte(h2!x, d-1, 7)
         writef(op=s_feq->"#=","#~=")
         prctxte(h3!x, d-1, 7)
         RETURN

    CASE s_ls: CASE s_gr:
         prctxte(h2!x, d-1, 7)
         writef(op=s_ls->"<",">")
         prctxte(h3!x, d-1, 7)
         RETURN

    CASE s_fls: CASE s_fgr:
         prctxte(h2!x, d-1, 7)
         writef(op=s_fls->"#<","#>")
         prctxte(h3!x, d-1, 7)
         RETURN

    CASE s_le: CASE s_ge:
         prctxte(h2!x, d-1, 7)
         writef(op=s_le->"<=",">=")
         prctxte(h3!x, d-1, 7)
         RETURN

    CASE s_fle: CASE s_fge:
         prctxte(h2!x, d-1, 7)
         writef(op=s_fle->"#<=","#>=")
         prctxte(h3!x, d-1, 7)
         RETURN
  }

  IF prec>=7 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_lshift: CASE s_rshift:
         prctxte(h2!x, d-1, 6)
         writef(op=s_lshift->"<<",">>")
         prctxte(h3!x, d-1, 6)
         RETURN
  }

  IF prec>=6 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_not:
         wrch('~')
         prctxte(h2!x, d-1, 5)
         RETURN
  }

  IF prec>=5 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_logand:
         prctxte(h2!x, d-1, 4)
         wrch('&')
         prctxte(h3!x, d-1, 4)
         RETURN
  }

  IF prec>=4 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_logor:
         prctxte(h2!x, d-1, 3)
         wrch('|')
         prctxte(h3!x, d-1, 3)
         RETURN
  }

  IF prec>=3 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_eqv:
    CASE s_xor:
         prctxte(h2!x, d-1, 2)
         writef(op=s_eqv->" EQV "," XOR ")
         prctxte(h3!x, d-1, 2)
         RETURN

  }

  IF prec>=2 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE

    CASE s_cond:
    CASE s_fcond:
         prctxte(h2!x, d-1, 1)
         writef(op=s_cond -> "->", "#->")
         prctxte(h3!x, d-1, 1)
         writef(",")
         prctxte(h4!x, d-1, 1)
         RETURN
  }

  IF prec>=1 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: writef("Op%n", op); RETURN

    CASE s_table:
         writef("TABLE ")
         prctxte(h2!x, d-1, 0)
         RETURN
         
    CASE s_valof:
         writef("VALOF{")
         prctxtc(h2!x, d-1)
         wrch('}')
         RETURN

    CASE s_comma:
         prctxte(h2!x, d-1, 0)
         writef(",")
         prctxte(h3!x, d-1, 0)
         RETURN
  }
}


AND out1(x) BE wrn(x)
 
AND out2(x, y) BE { out1(x); out1(y) }
 
AND out3(x, y, z) BE { out1(x); out1(y); out1(z) }
 
AND out4(x, y, z, t) BE { out1(x); out1(y); out1(z); out1(t) }
 
AND outstring(s) BE FOR i = 0 TO s%0 DO out1(s%i)

