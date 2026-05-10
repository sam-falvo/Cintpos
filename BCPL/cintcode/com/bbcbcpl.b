/*

This is a reconstruction of the BCPL compiler for the BBC BCPL
System. It should generate exactly the same 16-bit Cintcode that the
original BBC BCPL compiler generated.  This version is designed to run
under the current 32-bit BCPL Cintcode system.

This version was reconstructed by Martin Richards (c) Nov 2019

This program is still under development.


History

25/11/2019

Started new version of bbcbcpl.b.  It reads source and GET files from
the current directory.  All references to functions in BBCLIB and
functions declared in LIBHDR32.h and SYSHDR32.h will be replaces by
functions in the standard BCPL library. The entire BBBCBCPL compiler
will be reduced to two sections: the front end and the codegenerator.
Dynamic overlaying of code will not be needed. Since this version runs
on the modern 32-bit BCPL system it is capable of compiling much
larger sections than those that could be compiled on the original BBC
version. A cross reference listing (xbbcbcpl) of this compiler can be
created by the linux shell command: make xbbcbcpl.

This version compiles BBC BCPL to 16-bit BBC Cintcode. In due course a
new version (bbcbcpl32.b) using a different codegenerator will compile
BBC BCPL to modern 32-bit Cintcode.

*/

SECTION "BCPL"

GET "libhdr"

GLOBAL
{
// Global variables used by both the front end and the
// codegenerator, These same declarations also occur at the
// start of the codegenerator and provide the interface
//between the front end and the codegenerator.

ocodeinstream:ug
codestream

naming

sectname      // This is needed when generating 16 bit BBC Cintcode
sectionlen    // This is needed when generating 16 bit BBC Cintcode

codegenerate:400
}


MANIFEST { // Manifests used by both the front end and the codegenerator.
// selectors
h1=0; h2; h3; h4; h5; h6

// Lexical tokens and AE tree and OCODE operators although
// only the Ocode operators are needed by the codoegenerator.

s_number=1; s_name=2; s_string=3
s_true=4; s_false=5
s_valof=6; s_lv=7
s_rv=8; s_vecap=9; s_fnap=10
s_mult=11; s_div=12; s_rem=13; s_plus=14
s_minus=15; s_query=16; s_neg=17; s_abs=19
s_eq=20; s_ne=21; s_ls=22; s_gr=23; s_le=24; s_ge=25
s_byteap=28
s_not=30; s_lshift=31; s_rshift=32; s_logand=33
s_logor=34; s_eqv=35; s_neqv=36
s_cond=37; s_comma=38; s_table=39
s_and=40; s_valdef=41; s_vecdef=42
s_commalist=43; s_fndef=44; s_rtdef=45
s_needs=48; s_section=49
s_ass=50; s_rtap=51; s_goto=52
s_resultis=53; s_colon=54
s_test=55; s_for=56; s_if=57; s_unless=58
s_while=59; s_until=60; s_repeat=61
s_repeatwhile=62; s_repeatuntil=63
s_loop=65; s_break=66; s_return=67; s_finish=68
s_endcase=69; s_switchon=70; s_case=71; s_default=72
s_semicolonlist=73; s_let=74
s_manifest=75; s_global=76; s_static=79
s_be=89; s_end=90; s_lsect=91; s_rsect=92
s_get=93; s_semicolon=97; s_into=98
s_to=99; s_by=100; s_do=101; s_or=102
s_vec=103; s_lparen=105; s_rparen=106

s_setcond=107
s_lcond=108;
s_rcond=109

// OCODE operators that are not previously declared.
s_lf=39; s_fnlab=39
s_lp=40; s_lg=41; s_ln=42; s_lstr=43; s_ll=44
s_llp=45; s_llg=46; s_lll=47
s_local=77; s_label=78
s_sp=80; s_sg=81; s_sl=82; s_stind=83
s_jump=85; s_jt=86; s_jf=87; s_endfor=88; s_xlab=89
s_lab=90; s_stack=91; s_store=92; s_rstack=93
s_entry=94; s_save=95; s_fnrn=96; s_rtrn=97
s_res=98; s_datalab=100; s_iteml=101; s_itemn=102
s_endproc=103
s_debug=109; s_none=111
s_getbyte=120; s_putbyte=121
}

GLOBAL {   // Globals variables used only in the front end.

stdin:250
stdout

err_p
err_l

rc
spacev
spacevupb
verstream
ocodefile
ocodeoutstream
maxoption
blk

nametable
getv
wordv
gett
chbuf

// Globals used by TRN

dvec
dvect
globdecl
globdeclt
casek
casel
caset

declsize
printtree
charcode
transchars
savespacesize
sourcestream
ch
linenumber
linecount
reportcount
errcount
errvec
blkt
blkp
treevec
zeronode

sourcestream
linecount

treep
treeq

symb
decval
wordnode
wordv
chbuf
chcount
nlpending
nulltag

getp
rec_p
rec_l
skipnode
listp
blklist


// Globals use by TRN


jumpcond
transswitch
transfor


complab
compdatalab
compjump
wrpn
endocode
wrc

paramnumber
comcount
ssp
vecssp
currentbranch

dvece
dvecp

globdecls

casep

caseb
breaklabel
resultlabel
defaultlabel
endcaselabel
looplabel
ocount

// BCPL

//freeocode
smallnumber

// ARGS

bcplargs

// SYN1

//nextsymb1
nextsymb
//multichar

// SYN2

lookupword
compstring
declsyswords

// SYN3

rch
wrchbuf
rdtag
performget
readnumber
//value
rdstrch
//readoctalorhex

// SYN4

bcplsyn
newvec
list1
list2
list3
list4
list5
list6
makelist
synreport

// SYN5

rdblockbody
rdseq
rdcdefs
rdsect
rnamelist
rname
ignore
checkfor

// SYN6

rbexp
rexp
rexplist
rdef


// SYN7

rbcom
rcom

// TRN1

bcpltrn
//nextparam
//transreport
//wrtransmess

// TRN2

trans

// TRN3

declnames
decldyn
declstat
decllabels
checkdistinct
addname
cellwithname
scanlabels
transdef
transdyndefs
transstatdefs
statdefs


// TRN4

jumpcond
transswitch
transfor

// TRN5

load
loadlv
loadzero
loadlist

// TRN6

evalconst
assign
transname
compentry
outstring
out1
out1pfx
out2
out3        // G:386

}

MANIFEST {         // Constants used only by the front end
reportmax=10
nametablesize=541
getmax=50
wordmax=255/bytesperword+1
}


LET freeocode() BE
{ LET p = blk
  blk := 0
  WHILE p DO
  { LET q = p
    p := !p
    freevec(q)
  }
  RETURN
}

// Numbers in the range 1 to 899 are represented in the
// AE tree by values in this range. Other numbers are represented by
// pointers to number nodes such as [s_number 1234].

AND smallnumber(x) = 0<x<900 -> TRUE, FALSE

LET start() = VALOF
{ // Some sections of the BBC BCPL compiler were compiled using
  // a different version of the compiler. So that this version
  // can generate appropriate code for every section, section
  // names are store in the global sectname accessible to all
  // parts of the compile using testsectname,
  // eg compstr(sectname,"CCG5A")=0.

  LET v = VEC 10
  sectname := v
  FOR i = 0 TO 10 DO sectname!i := 0
  // The above code is only used in the 32-bit version.

  stdin  := input()
  stdout := output()

  spacevupb := 50000
  spacev := getvec(spacevupb)
  UNLESS spacev DO
  { writef("More space needed*n")
    RESULTIS 0
  }
   
  blk := 0
  codestream := 0
  bcplargs()
  UNLESS rc=0 GOTO fail
  UNLESS sourcestream=0 DO
  { LET a = bcplsyn()
    IF ch=endstreamch DO endread()
    IF a=0 | rc BREAK
    bcpltrn(a)
    freeocode()
  } REPEATUNTIL ch=endstreamch | rc~=0

  endread()
  selectoutput(ocodeoutstream)
  wrch(0)
  endwrite()
  ocodeoutstream := 0
  selectoutput(verstream)
  freeocode()
  TEST rc=0 & codestream
  THEN { codegenerate(spacev, spacevupb)
         selectoutput(verstream)
       }
  ELSE { writes("*nNo Code Generated")
       }
fail:
  newline()
  UNLESS verstream=stdout DO endwrite()
  UNLESS stdin=input() DO endread()
  IF codestream DO
  { selectoutput(codestream)
    endwrite()
  }
  selectoutput(stdout)
  // Do not delete OCODE$$ if no TO file was specified.
  IF codestream DO deletefile("OCODE$$")
  IF spacev DO freevec(spacev)
  stop(rc, 0)
  RESULTIS 0
}



//SECTION "ARGS"


LET bcplargs() = VALOF
{ LET args = "FROM/A,TO/K,REPORT/K,NONAMES/S,MAX/S,SECTLEN/S"
  LET ocodename = "OCODE$$"  // $$ get overwritten by two digits
  LET title = "*nBCPL - RCP V2.2*n"
  LET errarg = "Bad args for %S"
  LET errfil = "Cannot open %S"
  LET oldoutput = output()
  LET argv = VEC 80

  LET error(mess, a, b) BE
  { rc := result2
    writes("*nError. ")
    writef(mess, a, b)
    newline()
    longjump(rec_p, rec_l)
    RETURN
  }
  rec_p := level()
  rec_l := fail
  rc := 0
  sourcestream := 0
  ocodeoutstream := 0
  codestream := 0
  verstream := oldoutput

  UNLESS rdargs(args, argv, 80) DO
  { result2 := 11
    error(errarg, args)
  }

  IF argv!2 DO                        // REPORT/K
  { verstream := findoutput(argv!2)   // REPORT/K
    UNLESS verstream DO
    { verstream := oldoutput
      error(errfil, argv!2)   // REPORT/K
    }
    selectoutput(verstream)
  }
  writes(title)
  naming := ~argv!3         // NONAMES/S

// spacev is allocated by bcpl.b and has upb 10000

// In bcplsyn spacev is used to hold the following vectors:

// nametable    size nametablesize=541   The hash table
// chbuf        size 64                  For error syntax messages
// wordv        size 1000                Used to hold names and strings
// getv         size 50                  To hold GET streams
// gett                                  Points to just beyond end of getv

// In bcpltrn spacev is used to hold the following vectors:

// dvec         size declsize=3080       The declaration vector
// globdecl     size 400                 To hold global entry points
// casek        size 400                 To hold case constants
// casel        size 400                 To hold case label numbers

// In codegenerate spacev is used to hold the following vectors:

// spacev2    upb        The hash table

sawritef("bcplargs: Initialising variables*n")

  nametable := spacev
  dvec := nametable
//  cgworkspace := nametable
//  cgworksize := 6000
  chbuf := nametable + 541
  wordv := chbuf + 64
  getv := wordv + 1000
  gett := getv + 50
  declsize := 3000
  savespacesize := 3

  { LET globdeclsize = 400
    LET p = dvec + declsize  // declsize=1280
    dvect := declsize
    globdecl := p
    globdeclt := globdeclsize
    casek := globdeclsize + p
    casel := 2*globdeclsize + p
    caset := globdeclsize
    maxoption := argv!4        // MAX/S

    sectionlen := argv!5       // SECTLEN/S
  }
    //UNLESS argv!4 DO               // MAX/S
    //{ ///UNLESS FILENAME(argv!0, 0) |
      ///       FINDSTFILE(argv!0) DO
      ///{ READ(argv!0, 0, 0)             // FROM/A
      ///  { LET filevec = FINDSTFILE(argv!0)
      ///    IF filevec DO
      ///    { filevec!-1 := filevec!-1 & #x7FFF
      ///    }
      ///  }
    //}

  sourcestream := findinput(argv!0)    // FROM/A
  UNLESS sourcestream DO error(errfil, argv!0)
  ocodeoutstream := findoutput(ocodename)
  UNLESS ocodeoutstream DO error(errfil, ocodename)

  IF argv!1 DO                           // TO
  { codestream := findoutput(argv!1)
    UNLESS codestream DO error(errfil, argv!1)
  }
  selectinput(sourcestream)
  linenumber := 1
  reportcount := 0
//sawritef("bcplargs: returning*n")
  RETURN
 
fail:
  endstream(sourcestream)
  endstream(ocodeoutstream)
  deletefile(ocodename)
  UNLESS verstream=oldoutput DO endstream(verstream)
}



//SECTION "SYN1"

// Start of the lexical abalyser.


LET nextsymb1() BE
{ nextsymb1()
  sawritef("nextsymb() => %n", symb)
  IF symb=s_name DO sawritef(" Name %s", @h3!wordnode)
  IF symb=s_string DO sawritef(" String %s", wordv)
  IF symb=s_number DO sawritef(" Number %n", decval)
  IF symb=s_lsect DO sawritef(" lsect")
  IF symb=s_rsect DO sawritef(" rsect")
  IF symb=s_let DO sawritef(" let")
  IF symb=s_global DO sawritef(" global")
  IF symb=s_colon DO sawritef(" colon")
  IF symb=s_end DO sawritef(" end")
  IF symb=s_fndef DO sawritef(" fndef")
  IF symb=s_rtdef DO sawritef(" rtdef")
  IF symb=s_rtap DO sawritef(" rtap")
  IF symb=s_fnap DO sawritef(" fnap")
  IF symb=s_section DO sawritef(" section")
  IF symb=s_lparen DO sawritef(" lparen")
  IF symb=s_rparen DO sawritef(" rparen")
  IF symb=s_goto DO sawritef(" goto")

  sawritef("*n")
}

AND nextsymb() BE
{ nlpending := FALSE

{ // Main loop
  SWITCHON ch INTO
  {
    CASE '*C':
    CASE '*N':
      nlpending := TRUE

    CASE '*s':
    CASE '*t':
      rch() REPEATWHILE ch='*s'
      LOOP

    CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
    CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
      symb := s_number
      readnumber(10)
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
      { LET c = ch
        rch()
        rdtag(c)
        IF skipnode LOOP
        symb := lookupword()
        IF symb=s_get DO { performget(); LOOP  }
        RETURN
      }

    CASE '{':
      symb := s_lsect
      rch()
      GOTO L128

    CASE '}':
      symb := s_rsect
      rch()
      GOTO L128

    CASE '$':
      multichar("()$<>[]{}", s_lsect, s_rsect,
                             s_setcond, s_lcond, s_rcond,
                             s_lsect, s_rsect, s_lsect, s_rsect, 0)
      IF symb=0 DO synreport(91)

L128: IF symb=s_lsect | symb=s_rsect DO
      { rdtag('$')
        IF skipnode LOOP
        lookupword()
        RETURN
      }

      { LET op = symb
        rdtag('<')
        symb := lookupword()
        IF op=s_rcond DO
        { IF skipnode=wordnode DO skipnode := 0
          LOOP
        }

        IF skipnode LOOP

        IF op=s_setcond DO
        { h1!wordnode := symb=s_true -> s_false,
                                        s_true
                           // store A in h1!wordnode
          LOOP
        }

        IF symb=s_true LOOP
        skipnode := wordnode

        WHILE skipnode DO nextsymb()
        RETURN
      }

    CASE '[':
    CASE '(':
      symb := s_lparen
      BREAK

    CASE ']':
    CASE ')':
      symb := s_rparen
      BREAK

    CASE '#':
    { LET radix = 8
      rch()
      UNLESS '0'<=ch<='7' DO
      { SWITCHON capitalch(ch) INTO
        { DEFAULT:  synreport(33)
          CASE 'B': radix := 2; ENDCASE
          CASE 'O': radix := 8; ENDCASE
          CASE 'X': radix := 16
        }
        rch()
      }
      readnumber(radix)
      symb := s_number
      RETURN
    }

    CASE '?':
      symb := s_query
      BREAK

    CASE '+':
      symb := s_plus
      BREAK

    CASE ',':
      symb := s_comma
      BREAK

    CASE ';':
      symb := s_semicolon
      BREAK

    CASE '@':
      symb := s_lv
      BREAK

    CASE '&':
      symb := s_logand
      BREAK

    CASE '=':
      symb := s_eq
      BREAK

    CASE '!':
      symb := s_vecap
      BREAK

    CASE '%':
      symb := s_byteap
      BREAK

    CASE '**':
      symb := s_mult
      BREAK

    CASE '|':
      multichar("|", 0, s_logor)
      UNLESS symb=0 RETURN

cmnt: UNTIL ch='*N' | ch='*C' | ch='*P' | ch=endstreamch DO rch()
      LOOP

    CASE '/':
      multichar("\/**", s_logand, 0, -1, s_div)
      IF symb>0 RETURN
      IF symb=0 GOTO cmnt

      { IF ch='**' DO
        { rch()
          IF ch='/' BREAK
          LOOP
        }
        IF ch=endstreamch DO synreport(63)
        rch()
      } REPEAT

      rch()
      LOOP

    CASE '~':
      multichar("=", s_ne, s_not)
      RETURN

    CASE '\':
      multichar("/=", s_logor, s_ne, s_not)
      RETURN

    CASE '<':
      multichar("=<", s_le, s_lshift, s_ls)
      RETURN

    CASE '>':
      multichar("=>", s_ge, s_rshift, s_gr)
      RETURN

    CASE '-':
      multichar(">", s_cond, s_minus)
      RETURN

    CASE ':':
      multichar("=", s_ass, s_colon)
      RETURN

    CASE '"':
    { LET i = 0
      rch()

      UNTIL ch='"' DO
      { IF i=255 DO synreport(34)
        i := i + 1
        wordv%i := rdstrch()
      }

      wordv%0 := i
      symb := s_string
      BREAK
    }

    CASE '`':
    CASE '*'':
      rch()
      decval := rdstrch()
      symb := s_number
      UNLESS ch='*'' | ch='`' DO synreport(34)
      BREAK

    CASE '_':
    CASE '^':
    CASE 31:
    CASE 30:
    CASE 29:
    CASE 28:
    CASE 27:
    CASE 26:
    CASE 25:
    CASE 24:
    CASE 23:
    CASE 22:
    CASE 21:
    CASE 20:
    CASE 19:
    CASE 18:
    CASE 17:
    CASE 16:
    CASE 15:
    CASE 14:
    CASE 12:
    CASE 11:
    CASE 8:
    CASE 7:
    CASE 6:
    CASE 5:
    CASE 4:
    CASE 3:
    CASE 2:
    CASE 1:
    CASE 0:

    DEFAULT:
      ch := '*S'
      synreport(94)

    CASE '.':
    CASE endstreamch:
      IF getp=0 DO
      { symb := s_end
        BREAK
      }

      // Indirect resolving words

      endread()
      skipnode := 0
      getp := getp - 3
      sourcestream := getv!getp
      selectinput(sourcestream)
      linecount := getv!(getp+1)
      ch := getv!(getp+2)
      LOOP
    }

  } REPEAT

  rch()
}

AND multichar(chars, a, b, c, d, e, f, g, h, i, j) BE
{ LET t = @chars
  LET i, lim = 1, chars%0
  rch()

  UNTIL i>lim DO
  { IF ch=chars%i DO
    { rch()
      BREAK
    }
    i := i+1
  }
  symb := t!i
}



//SECTION "SYN2"



LET lookupword() = VALOF
{ LET hashval = VALOF
  { LET res = wordv%0
    FOR i = 1 TO res DO
      res := (res+res+res + capitalch(wordv%i))
    RESULTIS res & 127     // The size of the nametable is 128 words.
  }

  LET i = 0
  wordnode := nametable!hashval
  UNTIL wordnode=0 |
        compstring(wordnode+2, wordv)=0 DO
    wordnode := h2!wordnode

  IF wordnode=0 DO
  { LET wordsize = wordv%0 >> 2  // For 32-bit BCPL
    wordnode := newvec(wordsize+2)
    wordnode!0 := s_name
    wordnode!1 := nametable!hashval
    FOR i = 0 TO wordsize DO
      wordnode!(i+2) := wordv!i
    nametable!hashval := wordnode
  }

  RESULTIS h1!wordnode
}

AND compstring(s1, s2) = VALOF
{ // Return 0 if strings s1 and s2 are equal ignoring case
  // return 2 otherwise.
  LET len1 = s1%0
  UNLESS len1=s2%0 RESULTIS 2
  FOR i = 1 TO len1 DO
    UNLESS capitalch(s1%i)=capitalch(s2%i) RESULTIS 2

  RESULTIS 0
}

AND declsyswords() BE
{ symb := TABLE
      s_and,s_abs,
      s_be,s_break,s_by,
      s_case,
      s_do,s_default,
      s_eq,s_eqv,s_or,s_endcase,
      s_false,s_for,s_finish,
      s_goto,s_ge,s_gr,s_gr,s_global,s_get,
      s_if,s_into,
      s_let,s_lv,s_le,s_ls,s_ls,s_logor,
          s_logand,s_loop,s_lshift,
      s_manifest,
      s_ne,s_query,s_not,s_neqv,s_needs,
      s_or,
      s_resultis,s_return,s_rem,s_rshift,s_rv,
      s_repeat,s_repeatwhile,s_repeatuntil,
      s_switchon,s_static,s_section,
      s_to,s_test,s_true,s_do,s_table,
      s_until,s_unless,
      s_vec,s_valof,
      s_while,
      0

  d("AND/ABS/*
    *BE/BREAK/BY/*
    *CASE/*
    *DO/DEFAULT/*
    *EQ/EQV/ELSE/ENDCASE/*
    *FALSE/FOR/FINISH/*
    *GOTO/GE/GR/GT/GLOBAL/GET/*
    *IF/INTO/*
    *LET/LV/LE/LS/LT/LOGOR/LOGAND/LOOP/LSHIFT//")

  d("MANIFEST/*
    *NE/NIL/NOT/NEQV/NEEDS/*
    *OR/*
    *RESULTIS/RETURN/REM/RSHIFT/RV/*
    *REPEAT/REPEATWHILE/REPEATUNTIL/*
    *SWITCHON/STATIC/SECTION/*
    *TO/TEST/TRUE/THEN/TABLE/*
    *UNTIL/UNLESS/*
    *VEC/VALOF/*
    *WHILE/*
    *$//")

   nulltag := wordnode
}

AND d(words) BE
{ LET i, length = 1, 0

  { LET ch = words%i
    TEST ch='/'
    THEN { IF length=0 RETURN
           wordv%0 := length
           lookupword()
           h1!wordnode := !symb
           symb := symb + 1
           length := 0
         }
    ELSE { length := length + 1
           wordv%length := ch
         }
    i := i + 1
  } REPEAT
}



//SECTION "SYN3"



LET rch() BE
{ { ch := rdch()

    IF ch>=32 BREAK
      
    IF ch='*N' |
       ch='*P' |
       ch='*C' DO
    { ch := '*N'
      linecount := linecount+1
      BREAK
    }

    IF ch<0 BREAK
    IF ch='*T' BREAK
    IF ch=0 BREAK
  } REPEAT

  chcount := chcount + 1
  chbuf!(chcount&63) := ch
}

AND wrchbuf() BE
{ writes("*N...")
  FOR p = chcount-63 TO chcount DO
  { LET k = chbuf!(p&63)
    IF k>0 DO wrch(k)
  }
  newline()
}

AND rdtag(char) BE
{ LET i = 1
  wordv%i := char

  { UNLESS 'A'<=ch<='Z' |
           'a'<=ch<='z' |
           '0'<=ch<='9' |
           ch='.'       |
           FALSE        BREAK
    i := i+1
    wordv%i := ch
    rch()
  } REPEAT

  wordv%0 := i
}

AND performget() BE
{ LET s = 0
  nextsymb()
  UNLESS symb=s_string & getp+2<=getmax DO
    synreport(97)
     
  s := findinput(wordv)
  UNLESS s DO
  { synreport(96, wordv)
  }
  getv!getp := sourcestream
  getv!(getp+1) := linecount
  getv!(getp+2) := ch
  getp := getp + 3
  linecount := 1
  sourcestream := s
  selectinput(s)
  rch()
  RETURN
}

AND readnumber(radix) BE
{ LET d = value(ch)
  decval := d
  IF d>=radix DO synreport(33)

  { rch()
    d := value(ch)
    IF d>=radix RETURN
    decval := radix*decval + d
  } REPEAT
}

AND value(ch) = VALOF
{ LET c = capitalch(ch)
  RESULTIS '0'<=c<='9' -> c-'0',
           'A'<=c<='F' -> c-'A'+10,
           100
}

AND rdstrch() = VALOF
{ { LET k = ch
    rch()
    IF k='*N' DO synreport(34)
    IF k='**' DO
    { IF ch='*N' | ch='*S' | ch=13 | ch='*T' DO
      { rch() REPEATWHILE ch='*S' | ch='*N' | ch='*C' | ch='*T'
        UNLESS ch='**' DO synreport(34)
        rch()
        LOOP
      }
      k := ch
      ch := capitalch(ch)
      IF ch='T' DO k := '*T'
      IF ch='S' DO k := '*S'
      IF ch='N' DO k := '*N'
      IF ch='E' DO k := '*E'
      IF ch='B' DO k := '*B'
      IF ch='C' DO k := '*C'
      IF ch='P' DO k := '*P'
  
      TEST ch='X' |
           ch='O' |
          '0'<=ch<='9'
      THEN { LET r, n = 8, 3
             IF ch='O' DO rch()
             IF ch='X' DO
             { r, n := 16, 2
               rch()
             }
             k := readoctalorhex(r, n)
             IF k>255 DO synreport(34)
             RESULTIS k  // don't translate *Xnn or *nnn
           }
      ELSE { rch()
           }
    }
    RESULTIS k
  } REPEAT
  RETURN
}

AND readoctalorhex(radix,digits) = VALOF
{ LET answer = 0
  FOR j = 1 TO digits DO
  { LET valch = value(ch)
    IF valch>=radix DO synreport(34)
    answer:=answer*radix + valch
    rch()
  }
  RESULTIS answer
}



//SECTION "SYN4"

LET bcplsyn() =  VALOF
{ LET a = 0    // p3
  nametable!0 := 0
  ///MOVE(nametable, nametable+1, 192)  // Clear vector nametable and chbuf
  ///                                   // 192 = 128 +    nametable
  ///                                   //        64      chbuf
  clear_words(nametable, 128+64)
    
  chcount := 0
  getp := 0
  listp := gett
  err_p, err_l := level(), fail
  skipnode := 0
  blk := getvec(129)
  h1!blk := 0
  blklist := @blk
  blkt := blk + 129 // blkt  pointer to last word of blk
  blkp := blk+1     // blkp pointer to first word of blk after the link word.

  zeronode := list2(s_number, 0) // Tree node representing 0

  declsyswords()
  rch()
  IF ch=endstreamch GOTO exit
  rec_p, rec_l := err_p, reclab

reclab:
  nextsymb()

  { LET rdsectionorneeds() = VALOF
    { LET op, a, b = symb, 0, 0
      nextsymb()
      UNLESS symb=s_string DO synreport(95)
      a := rbexp()
      IF op=s_section DO
      { writef("Section *"%S*"*n", a+1)
        FOR i = 0 TO (a+1)%0 DO sectname%i := (a+1)%i // 32-bit version only
      }
      b := symb = s_needs -> rdsectionorneeds(), rdblockbody()
      RESULTIS list3(op, a, b)
    }

    a := symb=s_section | symb=s_needs -> rdsectionorneeds(),
                                          rdblockbody()
    UNLESS symb=s_end DO
    { synreport(99)
    }
  }

  UNTIL ch='*n' | ch=endstreamch DO rch()

  rch() REPEATWHILE ch='*s' | ch='*t' | ch='*n'
    
  UNLESS ch=endstreamch DO unrdch()
  writes("Returning from bcplsyn*n")
fail:
exit:
  RESULTIS a
}


AND newvec(n) = VALOF
{ IF blkp+n<blkt DO
  { blkt := blkt-n-1
    RESULTIS blkt
  }
  TEST n > 13
  THEN { LET p = getvec(n+1)
         h1!p := blkp-1
         h1!blklist := p
         blklist := p
         RESULTIS p+1
       }
   
  ELSE { LET p = getvec(129)
         h1!p := 0
         blklist := blkp-1
         h1!blklist := p
         blkp := p+1
         blkt := p+129-n
         RESULTIS blkt
       }
}

AND list1(x) = VALOF
{ LET p = newvec(0)
  p!0 := x
  RESULTIS p
}

AND list2(x, y) = VALOF
{ LET p = newvec(1)
  p!0, p!1 := x, y
  RESULTIS p
}

AND list3(x, y, z) = VALOF
{ LET p = newvec(2)
  p!0, p!1, p!2 := x, y, z
  RESULTIS p
}

AND list4(x, y, z, t) = VALOF
{ LET p = newvec(3)
  p!0, p!1, p!2, p!3 := x, y, z, t
  RESULTIS p
}

AND list5(x, y, z, t, u) = VALOF
{ LET p = newvec(4)
  p!0, p!1, p!2, p!3, p!4 := x, y, z, t, u
  RESULTIS p
}

AND list6(x, y, z, t, u, v) = VALOF
{ LET p = newvec(5)
  p!0, p!1, p!2, p!3, p!4, p!5 := x, y, z, t, u, v
  RESULTIS p
}


AND makelist(k, n) = VALOF
{ LET p = newvec(n+1)
  h1!p := k    // Typically s_commalist
  h2!p := n    // Number of elements in the list

  ///MOVE(listp, p+2, n)  // Copy n items from listp to h3!p...
  FOR i = 0 TO n-1 DO (p+2)!i := listp!i
   
  RESULTIS p
}

AND synreport(n, a) BE
{ LET s = 0     // p5
  s := VALOF SWITCHON n INTO
  { DEFAULT: a := n
             RESULTIS "Error %N"
    CASE  6: RESULTIS "{ expected"
    CASE  7: RESULTIS "} expected"
    CASE  8:CASE 40:CASE 43:
             RESULTIS "Name expected"
    CASE  9: RESULTIS "Untagged } mismatch"
    CASE 15:CASE 19:CASE 41:
             RESULTIS ") missing"
    CASE 30: RESULTIS "Bad condition"
    CASE 32: RESULTIS "Bad expression"
    CASE 33: RESULTIS "Bad number"
    CASE 34: RESULTIS "Bad string*
                      * or character constant"
    CASE 42: RESULTIS "Bad procedure heading"
    CASE 44:CASE 45:
             RESULTIS "Bad declaration"
    CASE 50: RESULTIS "Unexpected :"
    CASE 51: RESULTIS "Bad command"
    CASE 54: RESULTIS "ELSE expected"
    CASE 57:CASE 58:
             RESULTIS "Bad FOR loop"
    CASE 60: RESULTIS "INTO expected"
    CASE 61:CASE 62:
             RESULTIS ": expected"
    CASE 63: RESULTIS "**/ missing"
    CASE 91: RESULTIS "Unexpected $"
    CASE 94: RESULTIS "Bad character"
    CASE 95: RESULTIS "Bad section name"
    CASE 96: RESULTIS "Cannot GET %S"
    CASE 97: RESULTIS "Bad GET directive"
    CASE 98: RESULTIS "Program too large"
    CASE 99: RESULTIS "Incorrect termination"
  }

  rc := n=96 -> result2, 40
  reportcount := reportcount+1
  writef("*NError near line %N:*n", linecount)
  writef(s, a)
  wrchbuf()
  IF n=1 | n=96 | n=98 DO
  { writes("*nFATAL ERROR*n")
    GOTO fin
  }

  IF reportcount>reportmax DO
  { writes("*NTOO MANY ERRORS*n")
    GOTO fin
  }

  nlpending := 0

  UNTIL symb=s_lsect |
        symb=s_rsect |
        symb=s_let |
        symb=s_and |
        symb=s_end |
        nlpending DO
  { nextsymb()
  }
  longjump(rec_p, rec_l)

fin:
  UNTIL getp=0 DO
  { endread()
    getp := getp-3
    sourcestream := getv!getp
    selectinput(sourcestream)
  }

  longjump(err_p, err_l)
  RETURN
}


//SECTION "SYN5"


LET rdblockbody() = VALOF
{ LET rp, rl = rec_p, rec_l
  LET a = 0
  LET ptr = @a

  { LET op = 0
    rec_p, rec_l := level(), recover
    ignore(s_semicolon)
    SWITCHON symb INTO
    { CASE s_manifest:
      CASE s_static:
      CASE s_global:
              op := symb
              nextsymb()
              !ptr := rdsect(rdcdefs)
              ENDCASE

      CASE s_let:
              nextsymb()
              !ptr := rdef()
  recover:
            { LET qtr = ptr
              WHILE symb=s_and DO
              { nextsymb()
                !qtr := list3(s_and, !qtr, rdef())
                qtr := @h3!(!qtr)
              }
              op := s_let
              ENDCASE
            }

      DEFAULT:
            { LET dummy = ?
              !ptr := rdseq()
              UNLESS symb=s_rsect | symb=s_end DO
                       synreport(51)
            }

      CASE s_rsect: CASE s_end:
              BREAK
    }

    !ptr := list3(op, !ptr, 0)
    ptr := @h3!(!ptr)
  } REPEAT

  rec_p, rec_l := rp, rl
  RESULTIS a
}

AND rdseq() = VALOF
{ LET n = 0
  LET q = listp
  LET rp, rl = rec_p, rec_l
  rec_p, rec_l := level(), rec

  { 
mklist:
    ignore(s_semicolon)
    !listp := rcom()
    listp, n := listp+1, n+1
  } REPEATUNTIL symb=s_rsect |
                 symb=s_end

mkseq:
  rec_p, rec_l := rp, rl
  listp := q
  IF n=1 RESULTIS !listp
  IF n=2 RESULTIS list3(s_semicolon, listp!0, listp!1)
  RESULTIS makelist(s_semicolonlist, n)

rec: // Only reached if there was a syntax error.

  IF symb=s_rsect |
     symb=s_end   DO GOTO mkseq
  rec_p, rec_l := rp, rl
  GOTO mklist

  RETURN 
}


AND rdcdefs() = VALOF
{ LET n = 0
  LET rp, rl = rec_p, rec_l
  LET p = listp
  rec_p, rec_l := level(), rec

  { !listp := rname()
    listp := listp+1
    UNLESS symb=s_eq | symb=s_colon DO
          synreport(45)
    nextsymb()
    !listp := rexp(0)
    listp, n := listp+1, n+2

rec:
    ignore(s_semicolon)
  } REPEATWHILE symb=s_name

  listp := p
  rec_p, rec_l := rp, rl
  RESULTIS makelist(s_semicolonlist, n)
}

AND rdsect(r) = VALOF
{ LET tag, a = wordnode, 0
  checkfor(s_lsect, 6)
  a := r()
  UNLESS symb=s_rsect DO synreport(7)
  TEST tag=wordnode
  THEN nextsymb()
  ELSE IF wordnode=nulltag DO
       { symb := 0
         synreport(9)
       }
  RESULTIS a
}

AND rnamelist() = VALOF
{ LET n, p = 0, listp

  { !listp := rname()
    listp, n := listp+1, n+1
    UNLESS symb=s_comma BREAK
    nextsymb()
  } REPEAT
  listp := p
  IF n=1 RESULTIS !listp
  IF n=2 RESULTIS list3(s_comma, listp!0, listp!1)
  RESULTIS makelist(s_commalist, n)
}


AND rname() = VALOF
{ LET a = wordnode
  checkfor(s_name, 8)
  RESULTIS a
}

AND ignore(item) BE IF symb=item DO nextsymb()

AND checkfor(item, n) BE
{ UNLESS symb=item DO synreport(n)
  nextsymb()
}



//SECTION "SYN6"


LET rbexp() = VALOF
{ LET a, op = 0, symb

  SWITCHON symb INTO

  { DEFAULT:
      synreport(32)

    CASE s_query:
      nextsymb()
      RESULTIS list1(s_query)

    CASE s_true:
    CASE s_false:
    CASE s_name:
      a := wordnode
      nextsymb()
      RESULTIS a

    CASE s_string:
    { LET wordsize = wordv%0/bytesperword
      a := newvec(wordsize+1)
      a!0 := s_string
      FOR i = 0 TO wordsize DO a!(i+1) := wordv!i
      nextsymb()
      RESULTIS a
    }

    CASE s_number:
    { LET k = decval
      nextsymb()
      IF k=0 RESULTIS zeronode
      IF smallnumber(k) RESULTIS k
      RESULTIS list2(s_number, k)
    }

    CASE s_lparen:
      nextsymb()
      a := rexp(0)
      checkfor(s_rparen, 15)
      RESULTIS a

    CASE s_valof:
      nextsymb()
      RESULTIS list2(s_valof, rcom())

    CASE s_vecap:
      op := s_rv
    CASE s_lv:
    CASE s_rv:
      nextsymb()
      RESULTIS list2(op, rexp(37))

    CASE s_plus:
      nextsymb()
      RESULTIS rexp(34)

    CASE s_minus:
      nextsymb()
      a := rexp(34)
      IF smallnumber(a) RESULTIS list2(s_number, -a)
      RESULTIS list2(s_neg, a)

    CASE s_not:
      nextsymb()
      RESULTIS list2(s_not, rexp(24))

    CASE s_abs:
      nextsymb()
      RESULTIS list2(s_abs, rexp(35))

    CASE s_table:
      nextsymb()
      RESULTIS list2(s_table, rexplist())
  }
}


AND rexp(n) = VALOF
{ LET a = rbexp()
  LET b, c, p, q = 0, 0, 0, 0

  { LET op = symb
    IF nlpending RESULTIS a
    SWITCHON op INTO

    { DEFAULT: RESULTIS a

      CASE s_lparen: nextsymb()
        b := 0
        UNLESS symb=s_rparen DO b := rexplist()
        checkfor(s_rparen, 19)
        a := list3(s_fnap, a, b)
        LOOP

      CASE s_vecap:
        p := 40; GOTO lassoc

      CASE s_rv:
        symb := s_vecap
      CASE s_byteap:
        p := 36; GOTO lassoc

      CASE s_rem:CASE s_mult:CASE s_div:
         p := 35; GOTO lassoc

      CASE s_plus:CASE s_minus:
         p := 34; GOTO lassoc

      CASE s_eq:CASE s_ne:
      CASE s_le:CASE s_ge:
      CASE s_ls:CASE s_gr:
         IF n>=30 RESULTIS a

         { nextsymb()
           b := rexp(30)
           a := list3(op, a, b)
           TEST c=0 THEN c :=  a
                    ELSE c := list3(s_logand, c, a)
           a, op := b, symb
         } REPEATWHILE s_eq<=op<=s_ge
         a := c
         LOOP

      CASE s_lshift:CASE s_rshift:
        p, q := 25, 30; GOTO dyadic

      CASE s_logand:
        p := 23; GOTO lassoc

      CASE s_logor:
        p := 22; GOTO lassoc

      CASE s_eqv:CASE s_neqv:
        p := 21; GOTO lassoc

      CASE s_cond:
        IF n>=13 RESULTIS a
        nextsymb()
        b := rexp(0)
        checkfor(s_comma, 30)
        a := list4(s_cond, a, b, rexp(0))
        LOOP

      lassoc:
        q := p

      dyadic:
        IF n>=p RESULTIS a
        nextsymb()
        a := list3(op, a, rexp(q))
        LOOP
    }
  } REPEAT
  RETURN
}

AND rexplist() = VALOF
{ LET a = 0
  LET n = 0
  LET q = listp

  { !listp := rexp(0)
    listp, n := listp+1, n+1
    UNLESS symb=s_comma BREAK
    nextsymb()
  } REPEAT
  listp := q
  IF n=1 RESULTIS listp!0
  IF n=2 RESULTIS list3(s_comma, listp!0, listp!1)
  RESULTIS makelist(s_commalist, n)
}


AND rdef() = VALOF
{ LET n = rnamelist()

  SWITCHON symb INTO

  { CASE s_lparen:
      { LET a = 0
        nextsymb()     // Get the symb after the lparen
        UNLESS h1!n=s_name DO synreport(40)
        IF symb=s_name DO a := rnamelist()
        checkfor(s_rparen, 41)
        IF symb=s_be DO
        { nextsymb()
          RESULTIS list5(s_rtdef, n, a, rcom(), 0)
        }
        IF symb=s_eq DO
        { nextsymb()
          RESULTIS list5(s_fndef, n, a, rexp(0), 0)
        }
        synreport(42)
      }

    DEFAULT:
        synreport(44)

    CASE s_eq:
        nextsymb()
        IF symb=s_vec DO
        { nextsymb()
          UNLESS h1!n=s_name DO synreport(43)
          RESULTIS list3(s_vecdef, n, rexp(0))
        }
        RESULTIS list3(s_valdef, n, rexplist())
  }
}



//SECTION "SYN7"


LET rbcom() = VALOF
{ LET a = ?

  SWITCHON symb INTO
  { DEFAULT: RESULTIS 0

    CASE s_name:CASE s_number:CASE s_string:
    CASE s_true:CASE s_false:
    CASE s_lv:CASE s_rv:CASE s_vecap:
    CASE s_lparen:
            a := rexplist()
            IF symb=s_ass DO
            { LET op = symb
              nextsymb()
              RESULTIS list3(op, a, rexplist())
            }
            IF smallnumber(a) DO synreport(51)
            IF symb=s_colon DO
            { UNLESS h1!a=s_name DO synreport(50)
              nextsymb()
              RESULTIS list4(s_colon, a, rbcom(),0)
            }
            IF h1!a=s_fnap DO
            { h1!a := s_rtap
              RESULTIS a
            }
            synreport(51)
            RESULTIS a

    CASE s_goto:CASE s_resultis:
          { LET op = symb
            nextsymb()
            RESULTIS list2(op, rexp(0))
          }

    CASE s_if:CASE s_unless:
    CASE s_while:CASE s_until:
          { LET op = symb  // p4
            nextsymb()
            a := rexp(0)
            ignore(s_do)
            RESULTIS list3(op, a, rcom())
          }

    CASE s_test:
          { LET b = ?
            nextsymb()
            a := rexp(0)
            ignore(s_do)
            b := rcom()
            checkfor(s_or, 54)
            RESULTIS list4(s_test, a, b, rcom())
          }

    CASE s_for:
         { LET i, j, k = ?, ?, 0
           nextsymb()
           a := rname()
           checkfor(s_eq, 57)
           i := rexp(0)
           checkfor(s_to, 58)
           j := rexp(0)
           IF symb=s_by DO { nextsymb()
                             k := rexp(0)
                           }
           ignore(s_do)
           RESULTIS list6(s_for, a, i, j, k, rcom())  }

    CASE s_loop:CASE s_break:CASE s_endcase:
    CASE s_return:CASE s_finish:
           a := wordnode
           nextsymb()
           RESULTIS a

    CASE s_switchon:
           nextsymb()
           a := rexp(0)
           checkfor(s_into, 60)
           RESULTIS list3(s_switchon, a, rdsect(rdseq))

    CASE s_case:
         { LET b = ?
           nextsymb()
           a := rexp(0)
           checkfor(s_colon, 61)
           b := rbcom()
           RESULTIS list3(s_case, a, b)
         }

    CASE s_default:
           nextsymb()
           checkfor(s_colon, 62)
           RESULTIS list2(s_default, rbcom())

    CASE s_lsect:
           RESULTIS rdsect(rdblockbody)
  }
}


AND rcom() = VALOF
{ LET a = rbcom()

  IF a=0 DO synreport(51)
  WHILE symb=s_repeat | symb=s_repeatwhile |
                        symb=s_repeatuntil DO
  { LET op = symb
    nextsymb()
    TEST op=s_repeat
    THEN a := list2(op, a)
    ELSE a := list3(op, a, rexp(0))
   }
   RESULTIS a
}




//SECTION "TRN1"


LET bcpltrn(x) BE
{ err_p, err_l := level(), fail
  errcount := 0
  dvece, dvecp := 3, 3
  dvec!0, dvec!1, dvec!2 := 0, 0, 0

  globdecls := 0
  casep, caseb := 0, -1
  endcaselabel, defaultlabel := 0, 0
  resultlabel, breaklabel, looplabel := -1, -1, -1
  comcount, currentbranch := 0, x
  ocount, paramnumber := 0, 0
  selectoutput(ocodeoutstream)

  WHILE x~=0 &
       (h1!x=s_section | h1!x=s_needs) DO
  { out1(h1!x)
    outstring(h2!x+1)
    x:=h3!x
  }

  ssp := savespacesize
  out2(s_stack, ssp)
  decllabels(x)
  trans(x)
  out2(s_global, globdecls/2)

  FOR i = 0 TO globdecls-2 BY 2 DO
    out2(globdecl!i, globdecl!(i+1))

fail:
  selectoutput(verstream)
}


AND nextparam() = VALOF
{ paramnumber := paramnumber + 1
  RESULTIS paramnumber
}

AND transreport(n, x) BE
{ LET oldout = output()
  selectoutput(verstream)
  wrtransmess(n, x, comcount)
  errcount := errcount+1
  reportcount := reportcount+1
  rc := 40
  IF reportcount>=10 DO
  { writes("*nTOO MANY ERRORS*n")
    longjump(err_p, err_l)
  }
  IF n=141 |
     n=143 |
     n=144 DO
  { writes("*nFATAL ERROR*n")
    longjump(err_p, err_l)
  }
  selectoutput(oldout)
  RETURN
}

AND wrtransmess(n, x, count) BE
{ LET mess = VALOF SWITCHON n INTO
  { DEFAULT: RESULTIS "Bad Expression"
    CASE 101:RESULTIS "DEFAULT ??"
    CASE 104:RESULTIS "BREAK, LOOP or RESULTIS ??"
    CASE 105:RESULTIS "CASE ??"
    CASE 106:RESULTIS "Same CASE twice"
    CASE 113:
    CASE 109:RESULTIS "Bad LHS expr."
    CASE 112:
    CASE 110:RESULTIS "LHS & RHS mismatch"
    CASE 115:RESULTIS "*"%S*" not declared"
    CASE 116:RESULTIS "*"%S*" out of scope"
    CASE 119:
    CASE 118:
    CASE 117:RESULTIS "Bad constant"
    CASE 141:RESULTIS "Too many cases"
    CASE 142:RESULTIS "*"%S*" declared twice"
    CASE 143:RESULTIS "Too many names"
    CASE 144:RESULTIS "Too many globals"
  }
  writef("Error after %N commands*n", count)
  writef(mess, @h3!x)
  newline()
  RETURN
}


//SECTION "TRN2"

LET trans(x) BE
{ again:
  IF x=0 RETURN

  { LET sw = FALSE
    comcount := comcount+1
    currentbranch := x
    SWITCHON h1!x INTO
    { CASE 96:
      CASE 95:
      CASE 94:
      CASE 93:
      CASE 92:
      CASE 91:
      CASE 90:
      CASE 89:
      CASE 88:
      CASE 87:
      CASE 86:
      CASE 85:
      CASE 84:
      CASE 83:
      CASE 82:
      CASE 81:
      CASE 80:
      CASE 78:
      CASE 77:
      CASE 64:
      DEFAULT:
        transreport(100, x)
        ENDCASE

      CASE s_let:
      { LET a, s, s1 = dvece, ssp, 0
        LET v = vecssp
        declnames(h2!x)
        checkdistinct(a, dvece)
        vecssp, s1 := ssp, ssp
        ssp := s
        transdef(h2!x)
        UNLESS ssp=s1 DO transreport(110, x)
        UNLESS ssp=vecssp DO { ssp := vecssp
                               out2(s_stack, ssp)
                             }
        out1(s_store)
        decllabels(h3!x)
        trans(h3!x)
        vecssp := v
        UNLESS ssp=s DO out2(s_stack, s)
        dvece, ssp := a, s
        ENDCASE
      }

      CASE s_static:
      CASE s_global:
      CASE s_manifest:
      { LET a, s = dvece, ssp
        AND op = h1!x
        LET list = h2!x
        LET p = list + 2
        IF op=s_manifest DO op := s_number
        FOR i = 0 TO h2!list-1 BY 2 DO
        { LET name = p!i
          LET k = evalconst(p!(i+1))
          TEST op=s_static
          THEN { LET m = nextparam()
                 addname(name, s_label, m)
                 out2(s_datalab, m)
                 out2(s_itemn, k)
               }

          ELSE { addname(name, op, k)
               }
        }

        decllabels(h3!x)
        trans(h3!x)
        dvece, ssp := a, s
        ENDCASE
      }

      CASE s_ass:
        assign(h2!x, h3!x)
        ENDCASE

      CASE s_rtap:
      { LET s = ssp
        ssp := ssp+savespacesize
        out2(s_stack, ssp)
        loadlist(h3!x)
        load(h2!x)
        out2(s_rtap, s)
        ssp := s
        ENDCASE
      }

      CASE s_goto:
        load(h2!x)
        out1(s_goto)
        ssp := ssp-1
        ENDCASE

      CASE s_colon:
        out2(s_xlab, h4!x)
        comcount := comcount-1
        x := h3!x
        GOTO again

      CASE s_unless:
        sw := TRUE
      CASE s_if:
        // Optimize commands like IF <exp> BREAK
        // only done in 32-bit version.
        // Could do the same with LOOP and ENDCASE.
        IF compstring(sectname, "SYN3")=0 &
           h1!(h3!x)=s_break DO
        { IF breaklabel<0 DO transreport(104, x)
          IF breaklabel=0 DO breaklabel := nextparam()
          jumpcond(h2!x, ~sw, breaklabel)
          ENDCASE
        }

      { LET l = nextparam()
        jumpcond(h2!x, sw, l)
        trans(h3!x)
        out2(s_lab, l)
        ENDCASE
      }

      CASE s_test:
      { LET l, m = nextparam(), nextparam()
        jumpcond(h2!x, FALSE, l)
        trans(h3!x)
        out2(s_jump, m)
        out2(s_lab, l)
        trans(h4!x)
        out2(s_lab, m)
        ENDCASE
      }

      CASE s_loop:
        IF looplabel<0 DO transreport(104, x)
        IF looplabel=0 DO looplabel := nextparam()
        out2(s_jump, looplabel)
        ENDCASE

      CASE s_break:
        IF breaklabel<0 DO transreport(104, x)
        IF breaklabel=0 DO breaklabel := nextparam()
        out2(s_jump, breaklabel)
        ENDCASE

      CASE s_return:
        out1(s_rtrn)
        ENDCASE

      CASE s_finish:
        out1(s_finish)
        ENDCASE

      CASE s_resultis:
        IF resultlabel<0 DO transreport(104, x)
        load(h2!x)
        out2(s_res, resultlabel)
        ssp := ssp - 1
        ENDCASE

      CASE s_while:
        sw := TRUE
      CASE s_until:
      { LET l, m = nextparam(), nextparam()
        LET bl, ll = breaklabel, looplabel
        breaklabel, looplabel := 0, m
        out2(s_jump, m)
        out2(s_lab, l)
        trans(h3!x)
        out2(s_lab, m)
        jumpcond(h2!x, sw, l)
        UNLESS breaklabel=0 DO out2(s_lab, breaklabel)
        breaklabel, looplabel := bl, ll
        ENDCASE
      }

      CASE s_repeatwhile:
        sw := TRUE
      CASE s_repeatuntil:
      CASE s_repeat:
      { LET l,bl,ll = nextparam(),breaklabel,looplabel
        breaklabel, looplabel := 0, 0
        out2(s_lab, l)
        TEST h1!x=s_repeat
        THEN { looplabel := l
               trans(h2!x)
               out2(s_jump, l)
             }
        ELSE { trans(h2!x)
               UNLESS looplabel=0 DO
                 out2(s_lab, looplabel)
               jumpcond(h3!x, sw, l)
             }
        UNLESS breaklabel=0 DO out2(s_lab, breaklabel)
        breaklabel, looplabel := bl, ll
        ENDCASE
      }

      CASE s_case:
      { LET l, k = nextparam(), evalconst(h2!x)
        IF casep>=caset DO
        { transreport(141, x)
        }
        IF caseb<0 DO transreport(105, x)
        FOR i = caseb TO casep-1 DO
          IF casek!i=k DO transreport(106, x)
        casek!casep := k
        casel!casep := l
        casep := casep + 1
        out2(s_lab, l)
        x := h3!x
        GOTO again
      }

      CASE s_default:
        IF caseb<0 DO transreport(105, x)
        UNLESS defaultlabel=0 DO
          transreport(101, x)
        defaultlabel := nextparam()
        out2(s_lab, defaultlabel)
        x := h2!x
        GOTO again

      CASE s_endcase:
        IF caseb<0 DO transreport(105, x)
        out2(s_jump, endcaselabel)
        ENDCASE

      CASE s_switchon:
        transswitch(x)
        ENDCASE

      CASE s_for:
        transfor(x)
        ENDCASE

      CASE s_semicolon:
        comcount := comcount-1
        trans(h2!x)
        x := h3!x
        GOTO again

      CASE s_semicolonlist:
        comcount := comcount - 1
        FOR h = 2 TO h2!x+1 DO trans(h!x)
        ENDCASE
    }
  }
}



//SECTION "TRN3"


LET declnames(x) BE
    UNTIL x=0 SWITCHON h1!x INTO
    { DEFAULT:transreport(102, currentbranch)
               BREAK

      CASE s_vecdef: CASE s_valdef:
               decldyn(h2!x)
               BREAK

      CASE s_rtdef: CASE s_fndef:
               h5!x := nextparam()
               declstat(h2!x, h5!x)
               BREAK

      CASE s_and:
               declnames(h2!x)
               x := h3!x
               LOOP
    }


AND decldyn(x) BE
{ UNLESS x=0 DO
  SWITCHON h1!x INTO
  { CASE s_name:
           addname(x, s_local, ssp)
           ssp := ssp + 1
           ENDCASE

    CASE s_comma:
           addname(h2! x, s_local, ssp)
           ssp := ssp + 1
           decldyn(h3!x)
           ENDCASE

    CASE s_commalist:
           FOR h = 2 TO h2!x+1 DO decldyn(h!x)
           ENDCASE

    DEFAULT:
           transreport(103, x)
  }
}

AND declstat(x, l) BE
{ LET t = cellwithname(x)
  IF dvec!(t+1)=s_global DO
  { LET n = dvec!(t+2)
    addname(x, s_global, n)
    IF globdecls+1>=globdeclt DO
    { transreport(144, x)
    }
    globdecl!globdecls := n
    globdecl!(globdecls+1) := l
    globdecls := globdecls + 2
    RETURN
  }

  addname(x, s_fnlab, l)
}

AND decllabels(x) BE
{ LET b = dvece
  scanlabels(x)
  checkdistinct(b, dvece)
}


AND checkdistinct(p, q) BE
  FOR s = q-3 TO p BY -3 DO
  { LET n = dvec!s
    FOR r = p TO s-3 BY 3 DO
      IF dvec!r=n DO transreport(142, n)
  }


AND addname(n, p, a) BE
{ LET t = dvec+dvece
  dvece := dvece + 3
  IF dvece>dvect DO
  { transreport(143, currentbranch)
  }
  h1!t, h2!t, h3!t := n, p, a
  RETURN
}


AND cellwithname(n) = VALOF
{ LET x = dvece
  x := x - 3 REPEATUNTIL x=0 | dvec!x=n
  RESULTIS x
}


AND scanlabels(x) BE UNLESS x=0 DO
  SWITCHON h1!x INTO
  { CASE s_colon:
      h4!x := nextparam()
      declstat(h2!x, h4!x)

    CASE s_if: CASE s_unless: CASE s_while:
    CASE s_until: CASE s_switchon: CASE s_case:
      scanlabels(h3!x)
      ENDCASE

    CASE s_semicolonlist:
      FOR h = 2 TO h2!x+1 DO scanlabels(h!x)
      ENDCASE

    CASE s_semicolon:
      scanlabels(h3!x)
    CASE s_repeat: CASE s_repeatwhile:
    CASE s_repeatuntil: CASE s_default:
      scanlabels(h2!x)
      ENDCASE

    CASE s_test:
      scanlabels(h3!x)
      scanlabels(h4!x)
      ENDCASE
  }




AND transdef(x) BE
{ transdyndefs(x)
  IF statdefs(x) DO
  { LET l, s= nextparam(), ssp
    out2(s_jump, l)
    transstatdefs(x)
    ssp := s
    out2(s_stack, ssp)
    out2(s_lab, l)
  }
}


AND transdyndefs(x) BE
    SWITCHON h1!x INTO
    { CASE s_and:
           transdyndefs(h2!x)
           x := h3!x
           LOOP

       CASE s_vecdef:
           out2(s_llp, vecssp)
           ssp := ssp + 1
           vecssp := vecssp + 1 + evalconst(h3!x)
           BREAK

       CASE s_valdef:
           loadlist(h3!x)
           BREAK

       DEFAULT:
           BREAK

    } REPEAT

AND transstatdefs(x) BE
{ WHILE h1!x=s_and DO
  { transstatdefs(h2!x)
    x := h3!x
  }
  IF h1!x=s_fndef | h1!x=s_rtdef DO
  { LET a, c = dvece, dvecp
    AND bl, ll = breaklabel, looplabel
    AND rl, cb = resultlabel, caseb
    breaklabel, looplabel := -1, -1
    resultlabel, caseb := -1, -1

    compentry(h2!x, h5!x)
    ssp := savespacesize

    dvecp := dvece
    decldyn(h3!x)
    checkdistinct(a, dvece)
    decllabels(h4!x)
    out2(s_save, ssp)

    TEST h1!x=s_fndef
    THEN { load(h4!x)
           out1(s_fnrn)
         }
    ELSE { trans(h4!x);
           out1(s_rtrn)
         }
    out2(s_endproc, 0)
    breaklabel, looplabel := bl, ll
    resultlabel, caseb := rl, cb
    dvece, dvecp := a, c
  }
}

AND statdefs(x) = h1!x=s_fndef | h1!x=s_rtdef -> TRUE,
                  h1!x ~= s_and -> FALSE,
                  statdefs(h2!x) -> TRUE,
                  statdefs(h3!x)


//SECTION "TRN4"


LET jumpcond(x, b, lab) BE
{ LET sw = b
  UNLESS smallnumber(x) SWITCHON h1!x INTO
  { CASE s_false: b := NOT b
        
    CASE s_true: IF b DO out2(s_jump, lab)
                 RETURN

    CASE s_not: jumpcond(h2!x, NOT b, lab)
                RETURN

    CASE s_logand: sw := NOT sw
    CASE s_logor:
      TEST sw THEN { jumpcond(h2!x, b, lab)
                     jumpcond(h3!x, b, lab)
                   }

              ELSE { LET m = nextparam()
                     jumpcond(h2!x, NOT b, m)
                     jumpcond(h3!x, b, lab)
                     out2(s_lab, m)
                   }
         RETURN

    DEFAULT:
  }

  load(x)
  out2(b -> s_jt, s_jf, lab)
  ssp := ssp - 1
  RETURN
}

AND transswitch(x) BE
{ LET p, b, dl = casep, caseb, defaultlabel
  AND ecl = endcaselabel
  LET l = nextparam()
  endcaselabel := nextparam()
  caseb := casep
  out2(s_jump, l)
  defaultlabel := 0
  trans(h3!x)
  out2(s_jump, endcaselabel)
  out2(s_lab, l)
  load(h2!x)
  IF defaultlabel=0 DO defaultlabel := endcaselabel
  out3(s_switchon, casep-p, defaultlabel)
  FOR i = caseb TO casep-1 DO out2(casek!i, casel!i)
  ssp := ssp - 1
  out2(s_lab, endcaselabel)
  endcaselabel := ecl
  casep, caseb, defaultlabel := p, b, dl
  RETURN
}

AND transfor(x) BE
{ LET a = dvece
  LET l, m = nextparam(), nextparam()
  LET bl, ll = breaklabel, looplabel
  LET k, n = 0, 0
  LET step = 1
  LET s = ssp
  breaklabel, looplabel := 0, 0
  load(h3!x)
  k, n := s_ln, h4!x
  UNLESS smallnumber(n) DO
    TEST h1!n=s_number
    THEN { n := h2!n
         }
    ELSE { k, n := s_lp, ssp
           load(h4!x)
         }
  addname(h2!x, s_local, s)
  UNLESS h5!x=0 DO step := evalconst(h5!x)
  out1(s_store)
  out2(s_jump, l)
  decllabels(h6!x)
  out2(s_lab, m)
  trans(h6!x)
  UNLESS looplabel=0 DO out2(s_lab, looplabel)
  out2(s_lp, s); out2(s_ln, step)
  out1(s_plus); out2(s_sp, s)
  out2(s_lab, l)
  TEST step > 0
  THEN { out2(s_lp,s)
         out2(k,n)
       }
  ELSE { out2(k,n)
         out2(s_lp,s)
       }
  out2(s_endfor, m)
  UNLESS breaklabel=0 DO out2(s_lab, breaklabel)
  breaklabel, looplabel, ssp := bl, ll, s
  out2(s_stack, ssp)
  dvece := a
  RETURN
}



//SECTION "TRN5"


LET load(x) BE
{ IF x=0 DO { transreport(148, currentbranch)
              loadzero()
              RETURN
            }
  IF smallnumber(x) DO
  { out2(s_ln, x)
    ssp := ssp + 1
    RETURN
  }
  { LET op = h1!x
    SWITCHON op INTO

    { CASE 38:   // Unused case constants to cause SWL to be used.
      CASE 29:
      CASE 27:
      CASE 26:
      CASE 18:
      DEFAULT:
        transreport(147, currentbranch)
        loadzero()
        ENDCASE

      CASE s_byteap: op:=s_getbyte

      CASE s_div: CASE s_rem: CASE s_minus:
      CASE s_ls: CASE s_gr: CASE s_le: CASE s_ge:
      CASE s_lshift: CASE s_rshift:
        load(h2!x)
        load(h3!x)
        out1(op)
        ssp := ssp - 1
        ENDCASE

      CASE s_vecap: CASE s_mult: CASE s_plus:
      CASE s_eq: CASE s_ne: CASE s_logand:
      CASE s_logor: CASE s_eqv: CASE s_neqv:
      { LET a, b = h2!x, h3!x
 
        IF smallnumber(a) |
           h1!a=s_name |
           h1!a=s_number DO
        { a, b := h3!x, h2!x // Make b (the right hand operand) a number
	                     // if possible.
        }
        load(a)
        load(b)
        IF op=s_vecap DO
        { out1(s_plus)
          op := s_rv
        }
        out1(op)
        ssp := ssp - 1
        ENDCASE
      }

      CASE s_neg: CASE s_not: CASE s_rv: CASE s_abs:
        load(h2!x)
        out1(op)
        ENDCASE

      CASE s_true: CASE s_false: CASE s_query:
        out1(op)
        ssp := ssp + 1
        ENDCASE

      CASE s_lv:
        loadlv(h2!x)
        ENDCASE

      CASE s_number:
        out2(s_ln, h2!x)
        ssp := ssp + 1
        ENDCASE

      CASE s_string:
      { out1(s_lstr)
        outstring(@ h2!x)
        ssp := ssp + 1
        ENDCASE
      }

      CASE s_name:
        transname(x, s_lp, s_lg, s_ll, s_ln, s_lf)
        ssp := ssp + 1
        ENDCASE

      CASE s_valof:
      { LET rl = resultlabel
        LET a = dvece
        decllabels(h2!x)
        resultlabel := nextparam()
        trans(h2!x)
        out2(s_lab, resultlabel)
        out2(s_rstack, ssp)
        ssp := ssp + 1
        dvece := a
        resultlabel := rl
        ENDCASE
      }

      CASE s_fnap:
      { LET s = ssp
        ssp := ssp + savespacesize
        out2(s_stack, ssp)
        loadlist(h3!x)
        load(h2!x)
        out2(s_fnap, s)
        ssp := s + 1
        ENDCASE
      }

      CASE s_cond:
      { LET l, m = nextparam(), nextparam()
        LET s = ssp
        jumpcond(h2!x, FALSE, m)
        load(h3!x)
        out2(s_res,l)
        ssp := s; out2(s_stack, ssp)
        out2(s_lab, m)
        load(h4!x)
        out2(s_res,l)
        out2(s_lab, l)
        out2(s_rstack,s)
        ENDCASE
      }

      CASE s_table:
      { LET m = nextparam()
        LET a = h2!x
        out2(s_lll, m)
        out2(s_datalab, m)
        ssp := ssp + 1
        UNLESS smallnumber(a) DO
        { LET p, n = 0, 0
          IF h1!a=s_comma DO p, n := a+1, 2
          IF h1!a=s_commalist DO p, n := a+2, h2!a
          UNLESS p=0 DO
          { FOR h = 0 TO n-1 DO
              out2(s_itemn, evalconst(h!p))
            ENDCASE
          }
        }
        out2(s_itemn, evalconst(a))
        ENDCASE
      }
    }
  }
}


AND loadlv(x) BE
{ IF x=0 | smallnumber(x) GOTO err
  SWITCHON h1!x INTO

  { DEFAULT:

err:  transreport(113, currentbranch)
      loadzero()
      ENDCASE

    CASE s_name:
      transname(x, s_llp, s_llg, s_lll, 0, 0)
      ssp := ssp + 1
      ENDCASE

    CASE s_rv:
      load(h2!x)
      ENDCASE

    CASE s_vecap:
    { LET a, b = h2!x, h3!x
      IF smallnumber(a) |
         h1!a=s_name DO
        a, b := h3!x, h2!x
      load(a)
      load(b)
      out1(s_plus) 
      ssp := ssp - 1
      ENDCASE
    }
  }
  RETURN
}

AND loadzero() BE
{ out2(s_ln, 0)
  ssp := ssp + 1
  RETURN
}

AND loadlist(x) BE
{ UNLESS x=0 DO
  { UNLESS smallnumber(x) DO
    { LET p, n = 0, 0
      IF h1!x=s_comma DO p, n := x+1, 2
      IF h1!x=s_commalist DO p, n := x+2, h2!x
      UNLESS p=0 DO
      { FOR h = 0 TO n-1 DO load(h!p)
        RETURN
      }
    }
    load(x)
  }
  RETURN
}



//SECTION "TRN6"


LET evalconst(x) = VALOF
{ LET a, b = 0, 0
  IF x=0 DO { transreport(117, currentbranch)
              RESULTIS 0
            }
  IF smallnumber(x) RESULTIS x
  SWITCHON h1!x INTO
  { DEFAULT:
      transreport(118, x)
      RESULTIS 0

    CASE s_name:
    { LET t = cellwithname(x)
      IF dvec!(t+1)=s_number RESULTIS dvec!(t+2)
      transreport(119, x)
      RESULTIS 0
    }

    CASE s_number: RESULTIS h2!x
    CASE s_true:   RESULTIS TRUE
    CASE s_query:
    CASE s_false:  RESULTIS FALSE

    CASE s_mult:   // dyadic operators
    CASE s_div:
    CASE s_rem:
    CASE s_plus:
    CASE s_minus:
    CASE s_lshift:
    CASE s_rshift:
    CASE s_logor:
    CASE s_logand:
    CASE s_eqv:
    CASE s_neqv:   b := evalconst(h3!x)

    CASE s_abs:    // monadic operators
    CASE s_neg:
    CASE s_not:    a := evalconst(h2!x)
  }

  SWITCHON h1!x INTO
  { CASE s_abs:   RESULTIS ABS a
    CASE s_neg:   RESULTIS -a
    CASE s_not:   RESULTIS ~a

    CASE s_mult:  RESULTIS a * b
    CASE s_div:   RESULTIS a / b
    CASE s_rem:   RESULTIS a REM b
    CASE s_plus:  RESULTIS a + b
    CASE s_minus: RESULTIS a - b
    CASE s_lshift:RESULTIS a << b
    CASE s_rshift:RESULTIS a >> b
    CASE s_logand:RESULTIS a & b
    CASE s_logor: RESULTIS a | b
    CASE s_eqv:   RESULTIS a EQV b
    CASE s_neqv:  RESULTIS a NEQV b
  }
}

AND assign(x, y) BE
{ IF x=0 |
     smallnumber(x) |
     y=0 DO
  { transreport(110, currentbranch)
    RETURN
  }
  SWITCHON h1!x INTO
  { CASE s_comma:
    CASE s_commalist:
      IF smallnumber(y) |
         h1!x~=h1!y DO
      { transreport(112, currentbranch)
        ENDCASE
      }

      { LET l, n = h2, 2
        IF h1!x=s_commalist DO
        { l, n := h3, h2!x
          UNLESS h2!y=n DO
          { transreport(112, currentbranch)
            ENDCASE
          }
        }
        // 
        FOR h = l TO l+n-1 DO
          assign(h!x, h!y)
      }
      ENDCASE

    CASE s_name:
      load(y)
      transname(x, s_sp, s_sg, s_sl, 0, 0)
      ssp := ssp - 1
      ENDCASE

    CASE s_byteap:
      load(y)
      load(h2!x)
      load(h3!x)
      out1(s_putbyte)
      ssp:=ssp-3
      ENDCASE

    CASE s_rv: CASE s_vecap:
      load(y)
      loadlv(x)
      out1(s_stind)
      ssp := ssp - 2
      ENDCASE

    DEFAULT:
      transreport(109, currentbranch)
  }
  RETURN
}

AND transname(x, p, g, l, n, f) BE
{ // This compile a name depending on how the name was declared.
  // s_local      use  p
  // s_global     use  g
  // s_label      use  l
  // s_numb       use  n  provided n is non zero
  // s_fnlab      use  f  provided f is non zero

  // There are only three call of transname depending on the context.

  // transname(s_lp,  s_lg,  s_ll, s_ln, s_lf)    if loading the Rvalue
  // transname(s_llp, s_llg, s_lll,   0,    0)    if loading the Lvalue
  // transname(s_sp,  s_sg,  s_sl,    0,    0)    if assigning to the name

  LET t = cellwithname(x)
  LET k, a = dvec!(t+1), dvec!(t+2)
  LET op = g                        // the Cintcode instruction
  SWITCHON k INTO
  { DEFAULT:       transreport(115, x)
                   ENDCASE

    CASE s_local:  IF t-dvecp<0 DO
                     transreport(116, x)
                   op := p

    CASE s_global: ENDCASE

    CASE s_label:  op := l
                   ENDCASE

    CASE s_fnlab:  n := f

    CASE s_number: TEST n=0
                   THEN transreport(113, x)
                   ELSE op := n
  }

  out2(op, a)
  RETURN
}

AND compentry(n, lab) BE
{ LET s = @h3!n
  LET len = s%0
  out3(s_entry, len, lab)  // Based on BCPLTRN.map
  FOR i = 1 TO len DO
  { LET ch = s%i
    out1(ch)
  }
  RETURN
}

AND outstring(x) BE
{ LET l = x%0
  out1(l)
  FOR i=1 TO l DO out1(x%i)
  RETURN
}

AND out1(n) BE
{ LET a = n>>7
  TEST a
  THEN { out1pfx(a)
         binwrch(n & 127)
       }
  ELSE { binwrch(n)
       }
  RETURN
}

AND out1pfx(x) BE
{ TEST x>=128
  THEN { out1pfx(x>>7)
         binwrch(x | 128)
        }
  ELSE { binwrch(x | 128)
       }
  RETURN
} 

AND out2(x, y) BE
{ out1(x)
  out1(y)
  RETURN
}

AND out3(x, y, z) BE
{ out1(x)
  out1(y)
  out1(z)
  RETURN
}


.

SECTION "CCG"

GET "libhdr"

GLOBAL
{
// Global variables used by both the front end and the
// codegenerator, These same declarations also occur at the
// start of the codegenerator and provide the interface
//between the front end and the codegenerator.

ocodeinstream:ug
codestream

naming

sectname      // This is needed when generating 16 bit BBC Cintcode
sectionlen    // This is needed when generating 16 bit BBC Cintcode

codegenerate:400
}

// Manifests used by the front end and the codegenerator.

MANIFEST { // Manifests used by both the front end and the codegenerator.
// selectors
h1=0; h2; h3; h4; h5; h6

// Lexical tokens and AE tree and OCODE operators although
// only the Ocode operators are needed by the codoegenerator.

s_number=1; s_name=2; s_string=3
s_true=4; s_false=5
s_valof=6; s_lv=7
s_rv=8; s_vecap=9; s_fnap=10
s_mult=11; s_div=12; s_rem=13; s_plus=14
s_minus=15; s_query=16; s_neg=17; s_abs=19
s_eq=20; s_ne=21; s_ls=22; s_gr=23; s_le=24; s_ge=25
s_byteap=28
s_not=30; s_lshift=31; s_rshift=32; s_logand=33
s_logor=34; s_eqv=35; s_neqv=36
s_cond=37; s_comma=38; s_table=39
s_and=40; s_valdef=41; s_vecdef=42
s_commalist=43; s_fndef=44; s_rtdef=45
s_needs=48; s_section=49
s_ass=50; s_rtap=51; s_goto=52
s_resultis=53; s_colon=54
s_test=55; s_for=56; s_if=57; s_unless=58
s_while=59; s_until=60; s_repeat=61
s_repeatwhile=62; s_repeatuntil=63
s_loop=65; s_break=66; s_return=67; s_finish=68
s_endcase=69; s_switchon=70; s_case=71; s_default=72
s_semicolonlist=73; s_let=74
s_manifest=75; s_global=76; s_static=79
s_be=89; s_end=90; s_lsect=91; s_rsect=92
s_get=93; s_semicolon=97; s_into=98
s_to=99; s_by=100; s_do=101; s_or=102
s_vec=103; s_lparen=105; s_rparen=106

s_setcond=107
s_lcond=108;
s_rcond=109

// OCODE operators that are not previously declared.
s_lf=39; s_fnlab=39
s_lp=40; s_lg=41; s_ln=42; s_lstr=43; s_ll=44
s_llp=45; s_llg=46; s_lll=47
s_local=77; s_label=78
s_sp=80; s_sg=81; s_sl=82; s_stind=83
s_jump=85; s_jt=86; s_jf=87; s_endfor=88; s_xlab=89
s_lab=90; s_stack=91; s_store=92; s_rstack=93
s_entry=94; s_save=95; s_fnrn=96; s_rtrn=97
s_res=98; s_datalab=100; s_iteml=101; s_itemn=102
s_endproc=103
s_debug=109; s_none=111
s_getbyte=120; s_putbyte=121
}




MANIFEST {  //Manifests used only by the codegenerator.
t_hunk  = 1000       // Object module item types.
t_bhunk = 3000       // binary hunk (not hex)
t_end   =  992

sectword   = #xFDDF   // SECTION name marker.
needsword  = #xFEED   // NEEDS name marker.
entryword  = #xDFDF   // Function name marker.

}

GLOBAL {

// Global variables used only by the codegenerator..

//codegenerate:400    defined above

arg1:401
arg2

casek
casel

ssp

dpblk
dq
dpblklist

tempt
tempv
stv
stvp

dp
freelist

incode
labv

maxgn
maxlab
maxssp

op
labnumber
pendingop
procdepth

progsize

infok_a
infon_a
infok_b
infon_b
reflist
refliste
rlist
rliste
glist
gliste
nlist
nliste
skiplab

debug

oldoutput

sectpos


// CCG1

//codegenerate // Already defined to be G:400
cgsects

// CCG1A

rdn
rdl
rdgn
newlab
checklab
cgerror
initstack
stack
store

// CCG2

scan

// CCG3

cgpendingop
//cgconstexp
//cgconstdyadic

// CCG4

loadval
loadba
genxch
genatb
loada
push
loadboth

inreg_a
inreg_b
setinfo_a
setinfo_b
addinfo_a
forget_a
forget_b
forgetall
forgetvar
forgetallvars
mustforget

// CCG4A

isnum
iszero
storet
gensp
loadt
lose1
swapargs
cgbyteop
cgstind
storein

// CCG5

cgrv
cgplus
cgglobal

// CCG5A

cgentry
cgsave
cgapply
cgreturn
cgcondjump
jmpfn
jfn0
revjfn
compjfn
prepj

// CCG6

cgswitch
cgswitchb
cgswitchl
cgstring
setlab
cgdatalab
cgstatics

// CCG6A

newblk
freeblk
appendblk
initdatalists
geng
gen
genb
genr
genw
checkspace
codeb
codew
coder
getw
putw
aligneven

// CCG7

chkrefs
dealwithrefs
genindword
inrange_d
inrange_i
fillref_d
fillref_i
relref

// CCG8

outputsection
objword

// CCG9

dboutput
wrkn
wrcode
wrfcode
op2str

}


MANIFEST
{
// Value descriptors.
k_none=1; k_numb=2; k_fnlab=2
k_loc=3; k_glob=4; k_lab=5;
// 6,7,8 unknown 
k_lvloc=9; k_lvglob=10; k_lvlab=11
k_a=12; k_b=13; k_c=14
k_loc0=15; k_loc1=16; k_loc2=17; k_loc3=18; k_loc4=19
k_glob0=20; k_glob1=21; k_glob2=22

swapped=TRUE; notswapped=FALSE

// Global routine numbers.
gn_stop=2
}

// CINTCODE function codes.
MANIFEST {
f_k0   =   0
f_brk  =   2
f_code1=  13
f_lm   =  14
f_lm1  =  15
f_l0   =  16
f_fhop =  27
f_jeq  =  28
f_jeq0 =  30

f_k    =  32
f_kw   =  33
f_s0g  =  34
f_k0g  =  32

f_l0g  =  45
f_l1g  =  46
f_l2g  =  47
f_lg   =  48
f_sg   =  49
f_llg  =  50
f_ag   =  51
f_mul  =  52
f_div  =  53
f_rem  =  54
f_xor  =  55
f_sl   =  56
f_ll   =  58
f_jne  =  60
f_jne0 =  62

f_llp  =  64
f_llpw =  65
f_s0g1 =  66

f_k0g1  =  32+32
f_l0g1  =  45+32
f_l1g1  =  46+32
f_l2g1  =  47+32
f_lg1   =  48+32
f_sg1   =  49+32
f_llg1  =  50+32
f_ag1   =  51+32

f_add  =  84
f_sub  =  85
f_lsh  =  86
f_rsh  =  87
f_and  =  88
f_or   =  89
f_lll  =  90
f_jls  =  92
f_jls0 =  94

f_l    =  96
f_lw   =  97
f_s0g2 =  98

f_k0g2  =  32+64
f_l0g2  =  45+64
f_l1g2  =  46+64
f_l2g2  =  47+64
f_lg2   =  48+64
f_sg2   =  49+64
f_llg2  =  50+64
f_ag2   =  51+64

f_rv   = 116
f_rtn  = 123
f_jgr  = 124
f_jgr0 = 126

f_lp   = 128
f_lpw  = 129

f_lp0  = 128

f_sys  = 145   // Added by MR
f_swb  = 146
f_swl  = 147
f_st   = 148
f_st0  = 148
f_stp0 = 149
f_goto = 155
f_jle  = 156
f_jle0 = 158

f_sp   = 160
f_spw  = 161

f_sp0  = 160
f_s0   = 176
f_xch  = 181
f_gbyt = 182
f_pbyt = 183
f_atc  = 184
f_atb  = 185
f_j    = 186
f_jge  = 188
f_jge0 = 190

f_ap   = 192
f_apw  = 193

f_ap0  = 192

f_code2= 207
f_nop  = 208
f_a0   = 208
f_rvp0 = 211
f_st0p0= 216
f_st1p0= 218

f_a    = 224
f_aw   = 225

f_l0p0 = 224

f_neg  = 241
f_not  = 242
f_l1p0 = 240
f_l2p0 = 244
f_l3p0 = 247
f_l4p0 = 249

}

//SECTION "CCG1"

LET codegenerate(v, upb) BE
{ oldoutput := output()
  writes("RCP CINTCODE generation*n")
  debug := 0  //+1 //+1 //+1
  ocodeinstream := findinput("OCODE$$")
  UNLESS ocodeinstream DO
  { LET r2 = result2
    cgerror("CANNOT OPEN OCODE$$")
    stop(r2, 0)
  }
  selectinput(ocodeinstream)
  progsize := 0
  sectpos := 0   // Position of the start of the current section 
  op := rdn()
  cgsects(v, upb)
  writef("CINTCODE size = %N words*n", progsize/2)
}

AND cgsects(workvec, workvecsize) BE UNTIL op=0 DO
{ LET p = workvec
  tempv := p
  p := p+90
  tempt := p
  dp := workvec+workvecsize
  labv := p
  labnumber := 2000
  p := p+labnumber
  FOR i = 0 TO labnumber-1 DO labv!i := -1
  
  stv := p
  stvp := 0
  dpblklist := 0
  initdatalists() // This initialises reflist, rlist, nlist glist
                  // and sets freelist, dpblk and dp to zero

  incode := FALSE
  maxgn := 0
  maxlab := 0
  maxssp := 0
  procdepth := 0
  initstack(3)
  forgetall()

  //naming := FALSE // 32-bit version only

  IF sectionlen DO
  { //sawritef("Compiling length word beacause of SECTLEN option*n")
    codew(0)
  }

  WHILE op=s_section | op=s_needs DO
  { LET n = rdn()
    LET v = VEC 3
    v%0 := 7
    sectname%0 := n
    FOR i = 1 TO n DO  { LET c = rdn()
                         sectname%i := c  // 32-bit version only
                         IF i<=7 DO v%i := c
                       }

    FOR i = n+1 TO 7 DO v%i := 32  //Pad with ASCII spaces.
    TEST op=s_section
    THEN { 
           sawritef("%i5: cgsects: progsize=%n sectname=%s*n",
                    stvp, progsize, sectname)

           // Fudge to include the length field for various sections
           IF sectionlen |
              compstring(sectname, "BCPL")=0  |
              compstring(sectname, "ARGS")=0  |
              compstring(sectname, "SYN1")=0  |
              compstring(sectname, "SYN2")=0  |
              compstring(sectname, "SYN3")=0  |
              compstring(sectname, "SYN4")=0  |
              compstring(sectname, "SYN5")=0  |
              compstring(sectname, "SYN6")=0  |
              compstring(sectname, "SYN7")=0  |
              compstring(sectname, "TRN1")=0  |
              compstring(sectname, "TRN2")=0  |
              compstring(sectname, "TRN3")=0  |
              compstring(sectname, "TRN4")=0  | // Not TRN5 or TRN6
              compstring(sectname, "CCG1")=0  |
              compstring(sectname, "CCG1A")=0 |
              compstring(sectname, "CCG2")=0  |
              compstring(sectname, "CCG3")=0  |
              compstring(sectname, "CCG4")=0  |
              compstring(sectname, "CCG4A")=0 |
              compstring(sectname, "CCG5")=0  |
              compstring(sectname, "CCG5A")=0 |
              compstring(sectname, "CCG6")=0  |
              compstring(sectname, "CCG8")=0  UNLESS sectionlen DO
           { sawritef("Compiling length word for section %s*n", sectname)
             codew(0)  // For size of module.
           }

           // Cause CCG8 to be compiled with naming=TRUE
           IF compstring(sectname, "CCG8")=0 DO naming := TRUE

//sawritef("Section %s naming=%n*n", sectname, naming)

           codew(sectword)  // FDDF
         }
    ELSE { codew(needsword)  // FEED
         }

sawritef("codegenerate: section name: *"%s*"*n", sectname)

    FOR i = 0 TO 6 BY 2 DO codew(capitalch(v%i) | capitalch(v%(i+1))<<8)
    op := rdn()

  }

  scan()
  op := rdn()
  outputsection(op=0)
  progsize := progsize + stvp
  sectpos := sectpos + stvp + 4
  { LET p = dpblklist
    WHILE p DO
    { LET t = p
      p := !p
      freevec(t)
    }
  }
}


//SECTION "CCG1A"

// Read an OCODE operator or argument.

LET rdn() = VALOF
{ LET n, res = binrdch(), ?
  IF (n & 128) = 0 RESULTIS n 
  res := n & 127

  { n := binrdch()
    res := (res << 7) + (n & 127)
    IF n+1=0 RESULTIS 0  // Test for endstreamch
  } REPEATWHILE (n & 128) ~= 0
  RESULTIS res
}

// Read in an OCODE label.
AND rdl() = VALOF
{ LET l = rdn()
  IF maxlab<l DO
  { maxlab := l
    checklab()
  }
  RESULTIS l
}

// Read in a global number.
AND rdgn() = VALOF
{ LET g = rdn()
  IF maxgn<g DO maxgn := g
  RESULTIS g
}

// Generate next label number.
AND newlab() = VALOF
{ labnumber := labnumber-1
  checklab()
  RESULTIS labnumber
}

AND checklab() BE IF maxlab>=labnumber DO
{ cgerror("TOO MANY LABELS")
  stop(40, 0)
}


AND cgerror(mes, a) BE
{ writes("*nERROR: ")
  writef(mes, a)
  newline()
  RETURN
}

// Initialize the simulated stack (SS).
LET initstack(n) BE
{ arg2, arg1, ssp := tempv, tempv+3, n
  pendingop := s_none
  h1!arg2, h2!arg2, h3!arg2 := k_loc, ssp-2, ssp-2
  h1!arg1, h2!arg1, h3!arg1 := k_loc, ssp-1, ssp-1
  IF maxssp<ssp DO maxssp := ssp
}

// Move simulated stack (SS) pointer to N.
AND stack(n) BE
{ IF maxssp<n DO maxssp := n
  IF n>=ssp+4 DO { store(0,ssp-1)
                   initstack(n)
                   RETURN
                 }

  WHILE n>ssp DO loadt(k_loc, ssp)

  UNTIL n=ssp DO
  { IF arg2=tempv DO
    { TEST n=ssp-1
      THEN { ssp := n
             h1!arg1, h2!arg1, h3!arg1 := h1!arg2, h2!arg2, ssp-1
             h1!arg2, h2!arg2, h3!arg2 := k_loc, ssp-2, ssp-2
           }
      ELSE initstack(n)
      RETURN
    }
    arg1, arg2, ssp := arg1-3, arg2-3, ssp-1
  }
}

// store all ss items from s1 to s2 in their true
// locations on the stack.
// it may corrupt both registers a and b.
AND store(s1, s2) BE FOR p = tempv TO arg1 BY 3 DO
                     { LET s = h3!p
                       IF s>s2 RETURN
                       IF s>=s1 DO storet(p)
                     }



//SECTION "CCG2"



LET scan() BE
{ IF debug>1 DO
  { dboutput()
    sawritef("op=%n %s*n", op, op2str(op))
    abort(2345)
  }

  SWITCHON op INTO

  { DEFAULT:      cgerror("BAD OP %N", op)
                  ENDCASE

    CASE 0:       RETURN
      
    CASE s_debug: debug := (debug+1) REM 3
                  ENDCASE

      CASE s_lp:   loadt(k_loc,   rdn());   ENDCASE
      CASE s_lg:   loadt(k_glob,  rdgn());  ENDCASE
      CASE s_ll:   loadt(k_lab,   rdl());   ENDCASE
      CASE s_ln:   loadt(k_numb,  rdn());   ENDCASE

      CASE s_lstr: cgstring(rdn());         ENDCASE

      CASE s_true: loadt(k_numb, -1);       ENDCASE
      CASE s_false:loadt(k_numb,  0);       ENDCASE

      CASE s_llp:  loadt(k_lvloc,  rdn());  ENDCASE
      CASE s_llg:  loadt(k_lvglob, rdgn()); ENDCASE
      CASE s_lf:
      CASE s_lll:  loadt(k_lvlab,  rdl());  ENDCASE

      CASE s_sp:   storein(k_loc,  rdn());  ENDCASE
      CASE s_sg:   storein(k_glob, rdgn()); ENDCASE
      CASE s_sl:   storein(k_lab,  rdl());  ENDCASE

      CASE s_stind:cgstind(); ENDCASE

      CASE s_rv:   cgrv(); ENDCASE

      CASE s_mult:CASE s_div:CASE s_rem:
      CASE s_plus:CASE s_minus:
      CASE s_eq:CASE s_ne:
      CASE s_ls:CASE s_gr:CASE s_le:CASE s_ge:
      CASE s_lshift:CASE s_rshift:
      CASE s_logand:CASE s_logor:CASE s_eqv:CASE s_neqv:
      CASE s_not:CASE s_neg:CASE s_abs:
                   cgpendingop()
                   pendingop := op
                   ENDCASE

      CASE s_endfor:
                   cgpendingop()
                   pendingop := s_le

      CASE s_jt:   cgcondjump(TRUE, rdl());
                   ENDCASE  // 16-bit version
                   //LOOP     // 32-bit version

      CASE s_jf:   cgcondjump(FALSE, rdl())
                   ENDCASE  // 16-bit version
                   //LOOP     // 32-bit version

      CASE s_goto: cgpendingop()
                   store(0, ssp-2)
                   loada(arg1)
                   gen(f_goto)
                   stack(ssp-1)
                   incode := FALSE
                   // this is a good place to deal with
                   // some outstanding forward refs.
                   chkrefs(50)
                   ENDCASE

      CASE s_xlab:                 // set label on even address
      CASE s_lab:  cgpendingop()
                   UNLESS incode DO chkrefs(30)
                   store(0, ssp-1)
                   IF op=s_xlab DO aligneven()
                   setlab(rdl())
                   forgetall()
                   incode := procdepth>0
                   ENDCASE

      CASE s_query:loadt(k_loc, ssp)
                   ENDCASE

      CASE s_stack:cgpendingop()
                   stack(rdn())
                   ENDCASE

      CASE s_store:cgpendingop()
                   store(0, ssp-1)
                   ENDCASE

      CASE s_entry:
                { LET n = rdn()
                  LET lab = rdl()
                  cgentry(n, lab)
                  procdepth := procdepth + 1
                  ENDCASE
                }

      CASE s_save:
                { cgsave(rdn())
                  ENDCASE
                }

      CASE s_fnap:
      CASE s_rtap: cgapply(op, rdn())
                   ENDCASE

      CASE s_rtrn:                   
      CASE s_fnrn: cgreturn(op)
                   ENDCASE

      CASE s_endproc:
                 { LET n = rdn()
                   cgstatics(n)   // The argument is ignored
                   procdepth := procdepth - 1
                   ENDCASE
                 }

      CASE s_res:
      CASE s_jump:
                { LET lab = rdl()
                  cgpendingop()
                  store(0, ssp-2)
                  TEST op=s_jump
                  THEN { storet(arg1)
                       }
                  ELSE { loada(arg1)
                         stack(ssp-1)
                       }

                  { op := rdn()
                    UNLESS op=s_stack BREAK
                    stack(rdn())
                  } REPEAT

                  TEST op=s_lab
                  THEN { LET m = rdl()
                         UNLESS lab=m DO genr(f_j, lab)
                         setlab(m)
                         forgetall()
                         incode := procdepth>0
                         op := rdn()
                       }
                  ELSE { genr(f_j, lab)
                         incode := FALSE
                         // deal with some refs.
                         chkrefs(50)
                       }
                  LOOP
                }

      // rstack always occurs immediately after a lab statement
      // at a time when cgpendingop() and store(0, ssp-2) have
      // been called.

      CASE s_rstack: initstack(rdn())
                     loadt(k_a, 0)
                     ENDCASE

      CASE s_finish:  // compile code for:  stop(0).
         { LET k = ssp
           stack(ssp+3)
           loadt(k_numb, 0)
           loadt(k_glob, gn_stop)
           cgapply(s_rtap, k)    // simulate the call: stop(0, 0)
           ENDCASE
         }

      CASE s_switchon:
                     { LET upb = 2*rdn() + 1
                       LET v = getvec(upb)
                       UNLESS v DO
                       { cgerror("NO ROOM FOR SWITCH")
                         stop(40, 0)
                       }
                       cgswitch(v, upb)
                       freevec(v)
                       ENDCASE
                     }

      CASE s_getbyte:  
      CASE s_putbyte:  cgbyteop(op)
                       ENDCASE

      CASE s_global:   cgglobal(rdn())
                       RETURN

      CASE s_datalab:  cgdatalab(rdl())
                       LOOP
   }
   op := rdn()
} REPEAT



//SECTION "CCG3"


// Compiles code to deal with any pending op.
LET cgpendingop() BE
{ LET f = 0
  LET sym = TRUE
  LET pndop = pendingop
  pendingop := s_none

  IF isnum(arg1) &
    cgconstexp(pndop) DO
      RETURN

  SWITCHON pndop INTO
  { DEFAULT:      cgerror("BAD PENDINGOP %N", pndop)

    CASE s_none:  RETURN

    CASE s_abs:   loada(arg1)
                  chkrefs(3)
                  genb(jfn0(f_jge), 127+2) // conditionally skip
                  gen(f_neg)           // over this neg instruction.
                  forget_a()
                  RETURN

    CASE s_neg:   loada(arg1)
                  gen(f_neg)
                  forget_a()
                  RETURN

    CASE s_not:   loada(arg1)
                  gen(f_not)
                  forget_a()
                  RETURN

    CASE s_eq: CASE s_ne:
    CASE s_ls: CASE s_gr:
    CASE s_le: CASE s_ge:
                  f := prepj(jmpfn(pndop))
                  chkrefs(4)
                  genb(f, 127+2)    // jump to    ---
                  gen(f_fhop)       //               |
                  gen(f_lm1)        // this point  <-
                  lose1(k_a, 0)
                  forgetall()
                  RETURN

    CASE s_minus: UNLESS isnum(arg1) DO
                  { f, sym := f_sub, FALSE
                    ENDCASE
                  }
                  h2!arg1 := -h2!arg1

    CASE s_plus:  cgplus(); RETURN

    CASE s_mult:  f      := f_mul;        ENDCASE
    CASE s_div:   f, sym := f_div, FALSE; ENDCASE
    CASE s_rem:   f, sym := f_rem, FALSE; ENDCASE
    CASE s_lshift:f, sym := f_lsh, FALSE; ENDCASE
    CASE s_rshift:f, sym := f_rsh, FALSE; ENDCASE
    CASE s_logand:f      := f_and;        ENDCASE
    CASE s_logor: f      := f_or;         ENDCASE
    CASE s_eqv:
    CASE s_neqv:  f      := f_xor;        ENDCASE
  }

  TEST sym THEN loadboth()
           ELSE loadba()

  gen(f)
  forget_a()
  IF pndop=s_eqv DO gen(f_not)
  lose1(k_a, 0)
  RETURN
}

AND cgconstexp(op) = VALOF
{ SWITCHON op INTO
  { CASE s_none:     RESULTIS TRUE

    CASE s_abs:      h2!arg1 := ABS h2!arg1
                     RESULTIS TRUE

    CASE s_neg:      h2!arg1 := -h2!arg1
                     RESULTIS TRUE

    CASE s_not:      h2!arg1 := ~h2!arg1
                     RESULTIS TRUE

    DEFAULT:         UNLESS isnum(arg2)
                       RESULTIS FALSE
                     // Both arg2 and arg1 are numbers.
                     RESULTIS cgconstdyadic(op)
  }
}

AND cgconstdyadic(op) = VALOF
{ // Apply op to two numbers in arg2 and arg1
  // If no applicable return FALSE
  // otherwise replace arg2 and arg1 by the value of the
  // expression and return TRUE.

  LET n1 = h2!arg2
  LET n2 = h2!arg1
  LET res = TRUE
  LET val = VALOF SWITCHON op INTO
  { DEFAULT:         res := FALSE
                     RESULTIS 0
    CASE s_minus:    RESULTIS n1 - n2
    CASE s_plus:     RESULTIS n1 + n2
    CASE s_mult:     RESULTIS n1 * n2
    CASE s_div:      RESULTIS n1 / n2
    CASE s_rem:      RESULTIS n1 REM n2
    CASE s_lshift:   RESULTIS n1 << n2
    CASE s_rshift:   RESULTIS n1 >> n2
    CASE s_logand:   RESULTIS n1 & n2
    CASE s_logor:    RESULTIS n1 | n2
    CASE s_eqv:      RESULTIS n1 EQV n2
    CASE s_neqv:     RESULTIS n1 NEQV n2
  }
  IF res DO lose1(k_numb, val)
  RESULTIS res
}


//SECTION "CCG4"



LET loadval(x, pushing) BE  // ONLY called from loada and push.
// Load compiles code to have the following effect:
// If pushing=TRUE    B := A; A := <x>.
// If pushing=FALSE   B := ?; A := <x>.
{ LET k, n = h1!x, h2!x

  UNLESS pushing |
         h1!x=k_a DO  // Dump A register if necessary.
  { FOR t = arg1 TO tempv BY -3 DO
    { IF h1!t=k_a DO
      { storet(t)
        BREAK
      }
    }
  }

  IF infok_a=k &
     infon_a=n DO
  { h1!x := k_a
    h2!x := 0
  }

  SWITCHON h1!x INTO
  { CASE k_c:
    CASE k_b:
    CASE 8:
    CASE 7:
    CASE 6:
    DEFAULT:  cgerror("IN LOADA %N", k)
              stop(40, 0)

    CASE k_a: IF pushing DO
              { UNLESS inreg_b(infok_a, infon_a) DO  // THIS CODE IS WRONG
                                                     // This call of inreg_b returns FALSE
                                                     // so genatb is always called.
                //UNLESS infok_a=infok_b & infon_a=infon_b DO  // Correction
                { genatb()
                }
              }
              RETURN

     CASE k_numb:
     { TEST -1<=n<=10
       THEN gen(f_l0+n)
       ELSE TEST 0<=n<=255
            THEN genb(f_l, n)
            ELSE TEST -255<=n<=0
                 THEN genb(f_lm, -n)
                 ELSE genw(f_lw, n)
       ENDCASE
     }

     CASE k_loc:  TEST 3<=n<=16
                  THEN gen(f_lp0+n)
                  ELSE TEST 0<=n<=255
                       THEN genb(f_lp, n)
                       ELSE genw(f_lpw, n)
                  ENDCASE

     CASE k_glob: geng(f_lg, n)
                  ENDCASE

     CASE k_lab:  genr(f_ll, n)
                  ENDCASE

     CASE k_lvloc:TEST 0<=n<=255
                  THEN genb(f_llp, n)
                  ELSE genw(f_llpw, n)
                  ENDCASE

     CASE k_lvglob:geng(f_llg, n)
                   ENDCASE

     CASE k_lvlab: genr(f_lll, n)
                   ENDCASE

     CASE k_loc0:  gen(f_l0p0+n)
                   ENDCASE

     CASE k_loc1:  gen(f_l1p0+n)
                   ENDCASE

     CASE k_loc2:  gen(f_l2p0+n)
                   ENDCASE

     CASE k_loc3:  gen(f_l3p0+n)
                   ENDCASE

     CASE k_loc4:  gen(f_l4p0+n)
                   ENDCASE

     CASE k_glob0: geng(f_l0g, n)
                   ENDCASE

     CASE k_glob1: geng(f_l1g, n)
                   ENDCASE

     CASE k_glob2: geng(f_l2g, n)
                   ENDCASE
  }

  // A loading instruction has just been compiled.

  setinfo_b(infok_a, infon_a)
  setinfo_a(h1!x, h2!x)
  h1!x, h2!x := k_a, 0
  RETURN
}

AND loadba() BE IF loadboth()=swapped DO genxch()

AND genxch() BE
{ LET k, n = infok_a, infon_a
  setinfo_a(infok_b, infon_b)
  setinfo_b(k, n)
  gen(f_xch)
  RETURN
}

AND genatb() BE
{ gen(f_atb)
  setinfo_b(infok_a, infon_a)
  RETURN
}

LET loada(x)   BE
{ loadval(x, FALSE)
  RETURN
}

AND push(x) BE
{ loadval(x, TRUE)
  RETURN
}

AND loadboth() = VALOF
// Compiles code to cause
//   either    arg2 -> B  and  arg1 -> A
//             giving result notswapped
//   or        arg2 -> A  and  arg2 -> B
//             giving result swapped.
// loadboth only swaps if this saves code.
{ LET x = arg2
  LET y = arg1

  // First ensure that no other stack item uses reg A.
  FOR t = tempv TO arg2-3 BY 3 DO
  { IF h1!t=k_a DO
      storet(t)
  }
  
  { LET xa, ya = inreg_a(x), inreg_a(y)
    AND xb, yb = inreg_b(x), inreg_b(y)

    IF h1!x=k_a DO
    { IF yb RESULTIS swapped
      IF ya DO
      { genatb()
        RESULTIS notswapped
      }
      push(y)
      RESULTIS notswapped
    }

    IF xa DO
    { IF yb RESULTIS swapped
      IF ya |
         h1!y=k_a DO
      { genatb() // x and y are both in A so copy A to B
        RESULTIS notswapped
      }

      push(y)  // x is in A so Compile A->B; y->A
      RESULTIS notswapped         
    }

    IF xb DO
    { IF ya | h1!y=k_a RESULTIS notswapped

      genxch()        // Copy B into A
      IF yb DO
      { genatb()  // x and y are both in A so compile A->B
        RESULTIS notswapped
      }
      push(y)   // x is in A so compile A->B; y->A
      RESULTIS notswapped
    }

    IF ya |
       h1!y=k_a DO
    { push(x)  // y is in A so compile A->B; x->A
      RESULTIS swapped
    }

    IF yb DO
    { LET yk = h1!y
      LET yn = h2!y
      UNLESS yk=k_loc &
             3<=yn<=16 DO
      { UNLESS yk=k_numb &
               -1<=yn<=10 DO
        { genxch()
          push(x)
          RESULTIS swapped
        }
      }
    }

    loada(x)
    push(y)
    RESULTIS notswapped
  }
}

AND inreg_a(x) = h1!x=infok_a & h2!x=infon_a -> TRUE, FALSE

AND inreg_b(x) = h1!x=infok_b & h2!x=infon_b -> TRUE, FALSE

AND setinfo_a(k, n) BE infok_a, infon_a := k, n

AND setinfo_b(k, n) BE infok_b, infon_b := k, n

AND addinfo_a(k, n) BE
  IF infok_a=k_none DO setinfo_a(k, n)

AND forget_a() BE setinfo_a(k_none, 0)

AND forget_b() BE setinfo_b(k_none, 0)

AND forgetall() BE
{ forget_a()
  forget_b()
}

// Forgetvar is called just after a simple variable (k, n) has been
// updated.  k is k_loc, k_glob or k_lab.  Note that register
// information about indirect local and global values
// must also be thrown away.
AND forgetvar(k, n) BE
{ IF mustforget(k, n, infok_a, infon_a) DO forget_a()
  IF mustforget(k, n, infok_b, infon_b) DO forget_b()
}

AND forgetallvars() BE  // Called after STIND or PUTBYTE.
{ // If A is known to hold the value of a local, global or static
  // or indirectly refers to a location addressed via a local or
  // global, the information is infoa_k and infon_a must be cleared.
  // IE if infok_a is any of k_loc, k_glob, k_lab, k_loc0 to k_loc4,
  // or k_glob0 to k_glob2, call forget_a.
  // Similarly conditionally call forget_b.

  IF infok_a=k_loc |
     infok_a=k_glob |
     infok_a=k_lab |
     infok_a>=k_loc0 DO forget_a()

  IF infok_b=k_loc |
     infok_b=k_glob |
     infok_b=k_lab |
     infok_b>=k_loc0 DO forget_b()
}

AND mustforget(k,n, infok,infon) = VALOF
{ // This is only called from forgetvar indicating that the
  // local or global variable specified by (k,n) has just been
  // updated. This means that information about the current
  // value held in A or B may now be invalid. (infok,infon)
  // holds the current information about either A or B.
  // The resultis TRUE if forget_a or forget_b must be called.

  IF n=infon DO            // return false if n ~= infon
  { IF k=infok |           // return true if n=infon and k=infok
       ( k=k_loc &
         k_loc0<=infok<=k_loc4 ) |
       ( k=k_glob &
         k_glob0<=infok<=k_glob2) DO
    { RESULTIS TRUE  // resultis is true if n=infon and
                     // either k=infok
                     // or     k=k_loc  and infok id k_loc0  to k_loc4
                     // or     k=k_glob and infok id k_glob0 to k_glob2
    }
  }
  RESULTIS FALSE
}



//SECTION "CCG4A"


LET isnum(x) = h1!x = k_numb

AND iszero(a) = h1!a=k_numb & h2!a=0 -> TRUE, FALSE

// Store the value of a SS item in its true stack location.
AND storet(x) BE
{ LET s = h3!x
  IF h1!x=k_loc & h2!x=s RETURN
  loada(x)
  gensp(s)
  forgetvar(k_loc, s)
  addinfo_a(k_loc, s)
  h1!x, h2!x := k_loc, s
}

AND gensp(s) BE
{ TEST 3<=s<=16
  THEN gen(f_sp0+s)
  ELSE TEST 0<=s<=255
       THEN genb(f_sp, s)
       ELSE genw(f_spw, s)
  RETURN
}

// Load an item (K,N) onto the SS. It may move SS items.
AND loadt(k, n) BE
{ cgpendingop()

  TEST arg1+3=tempt
  THEN { storet(tempv)  // SS stack overflow.
	 
         ///MOVE(tempv+3, tempv, arg1-tempv)
	 FOR i = tempv TO arg1-3 DO tempv!i := tempv!(i+3)
       }
  ELSE { arg2, arg1 := arg2+3, arg1+3
       }
  h1!arg1,h2!arg1,h3!arg1 := k,n,ssp
  ssp := ssp + 1
  IF maxssp<ssp DO maxssp := ssp
  RETURN
}


// Replace the top two SS items by (K,N) and set PENDINGOP=S_NONE.
AND lose1(k, n) BE
{ ssp := ssp - 1

  TEST arg2=tempv
  THEN { h1!arg2,h2!arg2 := k_loc,ssp-2
         h3!arg2 := ssp-2
       }
  ELSE { arg1 := arg2
         arg2 := arg2-3
       }
  h1!arg1, h2!arg1, h3!arg1 := k,n,ssp-1
  pendingop := s_none
  RETURN
}

AND swapargs() BE
{ LET k, n = h1!arg1, h2!arg1
  h1!arg1, h2!arg1 := h1!arg2, h2!arg2
  h1!arg2, h2!arg2 := k, n
  RETURN
}

AND cgbyteop(op) BE
{ cgpendingop()
  TEST op=s_getbyte
  THEN { loadba()
         gen(f_gbyt)
         forget_a()
         lose1(k_a, 0)
       }
  ELSE { // op=s_putbyte
         LET arg3 = arg2-3
         TEST arg3 - tempv < 0
         THEN { loadt(k_loc, ssp-3)
                loada(arg1)
                stack(ssp-1)
              }
         ELSE { loada(arg3)
              }
         gen(f_atc)
         loadba()
         gen(f_pbyt)
         forgetallvars()
         stack(ssp-3)
       }
  RETURN
}

AND cgstind() BE
{ // This compiles indirect assignments.
  // Without optimisation the strategy is
  // (1) call cgpendingop to clear the pendingop
  // (2) compile code to place arg2 and arg1 in B and A
  // (3) generate instuction ST
  // (4) call stack(ssp-2)
  // (5) call forgetallvars
  // However, it may be possible to use:
  // ST ST1 ST2 ST3             eg A!2 := B
  // STP3 STP4 STP5             eg (P!4)!A := B
  // ST0P3 ST0P4 ST1P3 ST1P4    eg (P!4)!1 := A
  // S0G0 S0G1 S0G2             eg (G!n)!0 := A

  LET t = VALOF
  { IF pendingop=s_plus DO
    { // Compile arg1!arg2 := arg3
      IF isnum(arg2) DO swapargs()
      IF isnum(arg1) DO
      { // Compile n!arg2 := arg3
        LET n = h2!arg1

        IF 0<=n<=3 DO
        { stack(ssp-1)
          pendingop := s_none
          // Compile n!arg1 := arg2  0<=n<=3 using
          // ST ST1 ST3 or ST3 or possibly
          // ST0P3 ST0P4 ST0P5 ST1P3 ST1P2 ST1P5 S0G S0G1 S0G2

          RESULTIS n     // 0<=n<=3
        }
      }
      // pendingop=s_plus and
      // arg1 is either a number not in range 0 to 3 or
      // it is not a number.

      // Check whether arg1 or arg2 is local 3, 4 or 5
      IF h1!arg2=k_loc &
         3<=h2!arg2<=5 DO swapargs()

      IF h1!arg1=k_loc &
         3<=h2!arg1<=5 DO
      { // pendingop is s_plus and arg1 is local 3, 4 or 5 so
        // compile: P3!arg2:=arg3  P4!arg2:=arg3  P5!arg2:=arg3  
        LET n = h2!arg1

        stack(ssp-1)
        pendingop := s_none
        // Compile: P3!arg1:=arg2  P4!arg1:=arg2  P5!arg1:=arg2
        // Set t to 4, 5 or 6 and compile using
        // STP3 to STP5   eg  (P!4)!A := B
        RESULTIS n+1  // The codes for SP3, SP4 and SP5.
      }

      UNLESS arg2=tempv DO
      { LET arg3 = arg2 - 3
        // arg3 exists
        IF h1!arg3=k_a DO
        { // arg3 is in A
          IF h1!arg2=k_loc |
             h1!arg2=k_glob |
             h1!arg2=k_numb DO swapargs()

          IF h1!arg1=k_loc |
             h1!arg1=k_glob |
             h1!arg1=k_numb DO
          // Optimize the case  <arg2>!<arg1> := <arg3>
          // where <arg3> is already in A
          // and <arg1> is a local, a global or a number.
          { // Compile  Pn!arg2 := A  Gn!arg2 := A  or n!arg2 := A
            // These all push arg2 then add Pn Gn or n followed by ST

            push(arg2)  // Compile: A, B := arg2, arg3
            cgplus()    // Compile using A, AP or AG
            gen(f_st)
            stack(ssp-2)
            forgetallvars()
            RETURN
          }
        }
      }
    }

    // pendingop optimisations were not possible
    // so compile the pendingop and return 0 to
    // cause !arg1 := arg2 to be compiled.

    cgpendingop()
    // Compile !arg1 := arg2
    RESULTIS 0
  }

  // 0<=t<=6

  // if t = 0 1 2 3  compile t!arg1 := arg2
  // if t = 4 5 6    compile P3!arg1:=arg2 P4!arg1:=arg2 P5!arg1:=arg2 

  { LET k, n = h1!arg1, h2!arg1
    // Choose a Cintcode function code.
    LET cinop = VALOF
    { IF k=k_glob &
         t=0 DO
      { // Compile 0!Gn := arg2
        RESULTIS f_s0g
      }
      IF k = k_loc &
         3<=n<=4 DO
      { IF t=0 DO
        { // Compile P3!0:=arg2  P4!0:=arg2 for ST0P3 or ST0P4
          RESULTIS f_st0p0+n
        }
        IF t=1 DO
        { // Compile P3!1:=arg2  P4!1:=arg2 for ST1P3 or ST1P4
          RESULTIS f_st1p0+n
        }
      }
      RESULTIS 0  // No Cintcode op code chosen.
    }

    TEST cinop=0
    THEN { loadba()
           gen(f_st0+t)
         }
    ELSE { loada(arg2)
           TEST cinop=f_s0g
           THEN geng(cinop, n)
           ELSE gen(cinop)
         }
    stack(ssp-2)
    forgetallvars()
    RETURN 
  }
}

// Store the top item of the SS in (K,N).
AND storein(k, n) BE
// K is K_LOC, K_GLOB or K_LAB.
{ cgpendingop()
  loada(arg1)

  SWITCHON k INTO
  { DEFAULT:     cgerror("IN STOREIN %N", k)
                 stop(40, 0)

    CASE k_loc:  gensp(n);       ENDCASE
    CASE k_glob: geng(f_sg, n);  ENDCASE
    CASE k_lab:  genr(f_sl, n);  ENDCASE
  }

  forgetvar(k, n)
  addinfo_a(k, n)
  stack(ssp-1)
  RETURN
}



//SECTION "CCG5"


LET cgrv() BE
{ // Without optimisation the strategy is
  // (1) call cgpendingop to clear prendingop
  // (2) call loada(arg1) to place arg1 in A
  // (3) compile instuction RV
  // but optimisations may be possible using
  // RV1 to RV6           eg A := A!4
  // L0P3 to L0P12        eg A := (P!8)!0
  // L1P3 to L1P6         eg A := (P!5)!1
  // L2P3 to L2P5         eg A := (P!4)!2
  // L3P3 to L3P4         eg A := (P!4)!3
  // L4P3 to L4P4         eg A := (P!4)!4
  // L0G L0G1 L0G2        eg A := (G!n)!0
  // L1G L1G1 L1G2        eg A := (G!n)!1
  // L2G L2G1 L2G2        eg A := (G!n)!2

  LET t = VALOF
  { IF pendingop=s_plus DO
    { IF isnum(arg2) DO swapargs()
      IF isnum(arg1) DO
      { LET n = h2!arg1
        IF 0<=n<=6 DO
        { stack(ssp-1)
          pendingop := s_none
          // Use RV RV1 to RV6
          RESULTIS n
        }
      }

      IF h1!arg2=k_loc &
         3<=h2!arg2<=7 DO swapargs()

      IF h1!arg1=k_loc &
         3<=h2!arg1<=7 DO
      { LET n = h2!arg1
        stack(ssp-1)
        pendingop := s_none
        // Use RVP3 to RVP7
        RESULTIS 10 + n
      }
    }
    cgpendingop()
    RESULTIS 0
  }

  // Now compile code for A := S!<arg1>
  // where          S is 0,..., 6, P!3 ,..., P!7
  // depending on   t =  0,..., 6,  13 ,...,  17

  LET k, n = h1!arg1, h2!arg1

  IF k=k_glob &
     0<=t<=2 DO
     { h1!arg1 := k_glob0 + t
       RETURN
     }
  IF k=k_loc & n>=3 DO
    IF t=0 & n<=12 |
       t=1 & n<=6  |
       t=2 & n<=5  |
       t=3 & n<=4  |
       t=4 & n<=4  DO
    { h1!arg1 := k_loc0 + t
      RETURN
    }
  loada(arg1)
  TEST t<=6
  THEN gen(f_rv+t)
  ELSE gen(f_rvp0 + t - 10)
  forget_a()
  h1!arg1, h2!arg1 := k_a, 0
  RETURN
}

AND cgplus() BE
// Compiles code to compute <arg2> + <arg1>.
// It does not look at PENDINGOP.

{ IF iszero(arg1) DO
  { stack(ssp-1)
    RETURN
  }
  IF iszero(arg2) DO
  { IF h2!arg1=ssp-1 &
       ( h1!arg1=k_loc |
         k_loc0<=h1!arg1<=k_loc4) DO
      loada(arg1)
    lose1(h1!arg1, h2!arg1)
    RETURN
  }

  TEST inreg_a(arg1)
  THEN loada(arg1)
  ELSE IF inreg_a(arg2) DO
         loada(arg2)

  IF h1!arg1=k_a DO swapargs()
  IF h1!arg2=k_loc &
     3<=h2!arg2<=12 DO
    swapargs()

  IF h1!arg1=k_loc &
     3<=h2!arg1<=12 DO
     { loada(arg2)
       gen(f_ap0 + h2!arg1)
       forget_a()
       lose1(k_a, 0)
       RETURN
     }

   IF h1!arg2=k_numb &
      -4<=h2!arg2<=5 DO swapargs()
   IF h1!arg1=k_numb &
      -4<=h2!arg1<=5 DO { LET k = h2!arg1
                          loada(arg2)
                          TEST k < 0
                          THEN gen(f_s0 - k)
                          ELSE gen(f_a0 + k)
                          forget_a()
                          lose1(k_a, 0)
                          RETURN
                        }

   IF h1!arg2=k_loc DO swapargs()
   IF h1!arg1=k_loc DO
   { LET n = h2!arg1
     loada(arg2)
     TEST 3<=n<=12 THEN gen(f_ap0 + n)
                   ELSE TEST 0<=n<=255
                        THEN genb(f_ap, n)
                        ELSE genw(f_apw, n)
     forget_a()
     lose1(k_a, 0)
     RETURN
   }

   IF h1!arg2=k_glob DO swapargs()
   IF h1!arg1=k_glob DO { loada(arg2)
                          geng(f_ag, h2!arg1)
                          forget_a()
                          lose1(k_a, 0)
                          RETURN
                        }

   IF h1!arg2=k_numb DO swapargs()
   IF h1!arg1=k_numb DO { LET n = h2!arg1
                          loada(arg2)
                          TEST 0<=n<=255
                          THEN genb(f_a, n)
                          ELSE genw(f_aw, n)
                          forget_a()
                          lose1(k_a, 0)
                          RETURN
                        }
   loadboth()
   gen(f_add)
   forget_a()
   lose1(k_a, 0)
}

AND cgglobal(k) BE
{ LET len = 0
  LET p = glist
  WHILE p DO
  { len := len+1
    p := !p
  }
  // len is now the length of glist.

  incode := FALSE
  aligneven()
  cgstatics()
  WHILE dealwithrefs(2*len + stvp) LOOP

  p := glist

  WHILE p DO
  { setlab(h2!p)
    codew(labv!(h3!p))
    p := h1!p
  }

  codew(-len)
  FOR i = 1 TO k DO
  { codew(rdgn())
    codew(labv!rdl())
  }
  codew(maxgn)
}


//SECTION "CCG5A"



LET cgentry(n, lab) BE
{ LET v = VEC 3
  v%0 := 7
  FOR i = 1 TO n DO { LET c = rdn()
                      IF i<=7 DO v%i := c
                    }
  FOR i = n+1 TO 7 DO v%i := 32  // Ascii SPACE.
  chkrefs(100)  // Deal with some forward refs.
  aligneven()
  IF naming DO { codew(entryword)
                 FOR i = 0 TO 6 BY 2 DO
                 { codew(capitalch(v%i) | capitalch(v%(i+1))<<8)
                 }
               }
   setlab(lab)
   incode := TRUE
   forgetall()
}

AND cgsave(n) BE
{ IF n>3 DO setinfo_a(k_loc, 3)
  initstack(n)
}

// Function or routine call.
AND cgapply(op, k) BE
{ LET sa = k+3  // Stack address of first arg (if any).
  cgpendingop()

// Deal with non args.
  FOR t = tempv TO arg2 BY 3 DO
  { IF h3!t>=k BREAK
    IF h1!t=k_a DO storet(t)
  }

// Deal with args 2, 3 ...
  FOR t = tempv TO arg2 BY 3 DO
  { LET s = h3!t
    IF s=sa DO
    { // We have found the SS item for the first arg.
      IF h1!t=k_a &
         t+3=arg2 DO
      { // Two argument call with the first arg already in A.
        push(arg2)
        storet(arg2)    // Store second arg.
        genxch()        // Restore first arg back to A.
        BREAK
      }
    }
    // Ensure that all arguments other than arg1 are stored in the stack
    IF s>sa DO storet(t)
  }

  // Move first arg (if any) into A.

  FOR t = arg2 TO tempv BY -3 DO
  { LET s = h3!t
    IF s<sa BREAK
    IF s=sa DO loada(t)
  }

  IF sa<h3!tempv DO
  { loadt(k_loc, sa)
    loada(arg1)
    stack(ssp-1)
  }

  // First arg (if any) is now in A.

  TEST h1!arg1=k_glob &
       3 <= k <= 12
  THEN geng(f_k0g+k, h2!arg1)
  ELSE { push(arg1)
         // First arg (if any) is now in B
         // and the procedure address is in A.
         TEST 3<=k<=12
         THEN gen(f_k0+k)
         ELSE TEST 0<=k<=255
              THEN genb(f_k, k)
              ELSE genw(f_kw, k)
       }

  forgetall()
  stack(k)
  IF op=s_fnap DO loadt(k_a, 0)
}

AND cgreturn(op) BE  // used by OCODE operators FNRN and RTRN
{ cgpendingop()
  IF op=s_fnrn DO loada(arg1)
  gen(f_rtn)
  //stack(ssp-1)                     // BUG
  //IF op=s_fnrn DO stack(ssp-1)   // Correction by MR
  incode := FALSE
}

// Used for OCODE operators JT and JF.
AND cgcondjump(b,lab) BE
{ LET f = jmpfn(pendingop)    // p5
  IF f=0 DO { loadt(k_numb,0); f := f_jne }
  pendingop := s_none
  UNLESS b DO f := compjfn(f)
  store(0,ssp-3)
  genr(prepj(f),lab)
  stack(ssp-2)
}

AND jmpfn(op) = VALOF SWITCHON op INTO
{ DEFAULT:   RESULTIS 0
  CASE s_eq: RESULTIS f_jeq
  CASE s_ne: RESULTIS f_jne
  CASE s_ls: RESULTIS f_jls
  CASE s_gr: RESULTIS f_jgr
  CASE s_le: RESULTIS f_jle
  CASE s_ge: RESULTIS f_jge
}

AND jfn0(f) = f+2 // Change F_JEQ into F_JEQ0  etc...

AND revjfn(f) = f=f_jls -> f_jgr,
                f=f_jgr -> f_jls,
                f=f_jle -> f_jge,
                f=f_jge -> f_jle,
                f

AND compjfn(f) = f=f_jeq -> f_jne,
                 f=f_jne -> f_jeq,
                 f=f_jls -> f_jge,
                 f=f_jge -> f_jls,
                 f=f_jgr -> f_jle,
                 f=f_jle -> f_jgr,
                 f

AND prepj(f) = VALOF  // Returns the appropriate m/c fn.
{ IF iszero(arg2) DO
  { swapargs()
    f := revjfn(f)
  }
  IF iszero(arg1) DO
  { loada(arg2)
    RESULTIS jfn0(f)
  }
  IF loadboth()=swapped RESULTIS revjfn(f)
  RESULTIS f
}



//SECTION "CCG6"



// Compiles code for SWITCHON.
LET cgswitch(v, upb) BE
{ LET n = (upb-1)/2     // Number of cases.
  LET dlab = rdl()      // Default label.

  casek, casel := v, v+n

  // Read and sort (K,L) pairs.
  FOR i = 1 TO n DO
  { LET k = rdn()
    LET l = rdl()
    LET j = i-1
    WHILE j DO
    { IF k > casek!j BREAK
      casek!(j+1), casel!(j+1) := casek!j, casel!j
      j := j - 1
    }
    casek!(j+1), casel!(j+1) := k, l
  }
  // The case constants are in casek!1 to casek!n in ascending oerder.
  // The corresponding labels are in casel!1 to casel!n.
  cgpendingop()
  store(0, ssp-2)
  loada(arg1)        // The argument og SWITCHON
  stack(ssp-1)
  { TEST n=0 |
         n < casek!n/2 - casek!1/2 + 4
    THEN { // Compile SWB switch
           cgswitchb(n, dlab)  // Compile a binary chop switch
         }
    ELSE { // Compile a SWL switch
           cgswitchl(n, dlab)  // Compile a label vector switch
         }
  }
}

// Code has already been compiled to set A to the 
// value of the switch expression.
AND cgswitchb(n, dlab) BE  // Binary chop switch
{ chkrefs(4*n+6)
  gen(f_swb)
  aligneven()
  codew(n)
  coder(dlab)
  FOR i = 1 TO n DO
  { codew(casek!i)
    coder(casel!i)
  }
}


AND cgswitchl(n, dlab) BE  // Label vector switch
{ LET p = 1
  chkrefs((casek!n - casek!1) * 2 + 10)
  gen(f_swl)
  aligneven()
  codew(casek!n - casek!1 + 1)
  coder(dlab)
  codew(casek!1) // Minimum case constant
  FOR k = casek!1 TO casek!n DO
  { TEST k = casek!p
    THEN { coder(casel!p)
           p := p+1
         }
    ELSE coder(dlab)
  }
}

AND cgstring(n) BE
{ // This ensures that the string constant is padded with
  // a zero if n is even.

  LET lab, a = newlab(), n
  loadt(k_lvlab,lab)
  { IF n DO a := a | rdn()<<8
    nliste := appendblk(nliste, lab, a)
    lab := 0
    IF n<=1 BREAK
    n := n-2
    a := rdn()
  } REPEAT
}

AND setlab(lab) BE
{ LET p = @rlist

  // rlist is a list of positions of instructions such as
  // JNE or LL that refer to labels that have not yet been set.
  // This functions sets label lab in labv and deals with all
  // instructions that refer to this label. If the such an
  // instruction is in direct relative addressing range, its
  // operand is updated and the rlist item removed from rlist.
  // If it is out of direct relative addressing range, it will
  // be resolved later using an indirect resolving word
  // generated by a call of chkrefs or dealwithrefs. These
  // resolving words are delayed to maximise the the number of
  // references that can share them.

  IF debug>0 DO
    writef("     L%N:*n", lab)

  labv!lab := stvp  // Set the label in labv.
                    // Unset labels have the special value -1.

  // Fill in all the refs that are in range.
  { LET r = !p
    IF r=0 BREAK

    TEST h3!r=lab &
         inrange_d(h2!r, stvp)
    THEN { fillref_d(h2!r, stvp)
           !p := !r   // Remove item from RLIST.
           freeblk(r)
         }
    ELSE { p := r  // Keep the item.
         }
  } REPEAT

  rliste := p     // Ensure that rliste is sensible.

  p := @reflist

  // reflist is a list of words that need to be set to the value
  // of labels. reflist items are of the form [next, addr, lab].
  // refliste points to the last reflist item, if any, or is zero.

  { LET r = !p // p points to global variable reflist or to
               // the link field of a reflist item.
    IF r=0 BREAK  // We have reached the end of reflist.
     
    TEST h3!r=lab
    THEN { // r points to a reflist item that refers to
           // the label that has just been set.
           LET a = h2!r
           putw(a,stvp-a) // Fill in the relative address.
           !p := !r       // Remove item from reflist.
           freeblk(r)
         }
    ELSE p := r  // Keep the reflist item.
  } REPEAT

  refliste := p   // Ensure refliste is sensible.
                  // It will point to a link word containing zero,
                  // being either reflist or the link word of the
                  // last item in reflist.
  RETURN
}

AND cgdatalab(lab) BE
{ // This compiles an OCODE sequence such as
  // DATALAB Llab  ITEMN 10 ITEMN 11
  // This will append [link, lab, 10] and [link, 0, 11] onto
  // then end of nlist. These static variables will be inserted
  // into the compiled code by the next call of cgstatics.
  // This compiler does not use any ITEML statements.

  op := rdn()
  IF op=s_itemn DO
  { nliste := appendblk(nliste, lab, rdn())
    lab := 0
    LOOP
  }
  IF op=s_iteml DO
  { gliste := appendblk(gliste, lab, rdl())
    lab := 0
    LOOP
  }
  RETURN
} REPEAT


AND cgstatics() BE // note: no argument
                   UNTIL nlist=0 DO
{ LET nl, len = nlist, 0

  nliste := @nlist  // All NLIST items will be freed.

  len, nl := len+2, !nl REPEATUNTIL nl=0 | h2!nl ~= 0

  aligneven()
  chkrefs(len)

  setlab(h2!nlist)  // NLIST always starts labelled.

  { LET blk = nlist
    nlist := !nlist
    freeblk(blk)
    codew(h3!blk)
  } REPEATUNTIL nlist=0 | h2!nlist ~= 0
}



//SECTION "CCG6A"


LET newblk(a, b, c) = VALOF
{ // Allocate and initialise a block [a,b,c] from
  // either the freelist of blocks or from space
  // obtained using GETVEC. The GETVEC blocks are
  // held in the dpblklist chain.

  LET p = freelist
  TEST p=0
  THEN { // The freelist is empty so allocate a block
         // from the current GETVEC block pointed to
         // by dpblk. If there is no more room in dpblk
         // allocate another using GETVEC.

         dq := dq-3

         IF dq < dpblk DO
         { dpblk := getvec(129)
           dq := dpblk + 126 + 1
           !dpblk := dpblklist
           dpblklist := dpblk
         }
         p := dq
       }
  ELSE freelist := !p

  h1!p, h2!p, h3!p := a, b, c
  RESULTIS p
}

AND freeblk(p) BE
{ !p := freelist
  freelist := p
}

AND appendblk(a, b, c) = VALOF
{ // Create a block [0, b, c] and
  // place a pointer to it in location a.

  LET p = newblk(0, b, c)
  !a := p
  RESULTIS p
}

AND initdatalists() BE
{ reflist, refliste := 0, @reflist
  rlist,   rliste   := 0, @rlist
  nlist,   nliste   := 0, @nlist
  glist,   gliste   := 0, @glist
  freelist, dpblk, dq := 0, 0, 0  // MR correction
}

LET geng(f, n) BE
{ genb(f+32*(n/256), n REM 256)
}

LET gen(f) BE IF incode DO
{ chkrefs(1)
  IF debug DO wrcode("", f)
  codeb(f)
}

LET genb(f, a) BE
{ IF incode DO
  { chkrefs(2)
    IF debug>0 DO
      wrcode("%I3", f, a)
    codeb(f)
    codeb(a)
  }
}

LET genr(f, n) BE
{ IF incode DO
  { chkrefs(2)
    IF debug>0 DO wrcode("L%N", f, n)
    codeb(f)
    codeb(0)
    relref(stvp-2, n)
  }
}

LET genw(f, w) BE
{ chkrefs(3)
  IF debug>0 DO wrcode("W%N", f, w)
  codeb(f)
  codeb(w & 255)
  codeb((w>>8) & 255)
}

AND checkspace() BE IF stvp/2>dp-stv DO
                    //IF stvp>32000 DO
{ cgerror("PROGRAM TOO LARGE %N BYTES COMPILED", stvp)
  stop(40, 0)
}


AND codeb(byte) BE
{ stv%stvp := byte
  stvp := stvp + 1
  checkspace()
}

AND codew(w) BE
{ IF debug>0 DO
    writef("%I3:   DATA %I3 %I3*n", stvp, w>>8 & 255, w & 255)

  codeb(w & 255)
  codeb(w>>8 & 255)
}

AND coder(lab) BE
{ // Compile a word containing the relative address to label lab.

  LET labval = labv!lab   // labval=p4
  IF debug>0 DO writef("%I3:   DATA L%N-$*n", stvp, lab)
  codeb(0)
  codeb(0)
  TEST labval=-1 THEN { // lab is unset, so append a reflist item.
                        refliste := appendblk(refliste, stvp-2, lab)
                      }
                 ELSE { putw(stvp-2, labval-stvp+2)
                      }
}

AND getw(a) = stv%a | stv%(a+1)<<8

AND putw(a, w) BE stv%a, stv%(a+1) := w, w>>8

AND aligneven() BE IF (stvp & 1) ~= 0 DO codeb(f_nop)



//SECTION "CCG7"



LET chkrefs(n) BE  // Resolve references until it is possible
                   // to compile n bytes without a reference
                   // going out of range.
{ LET p = @rlist
  skiplab := 0

  { LET r, a = !p, ?
    IF r=0 BREAK

    a := h2!r // RLIST is ordered in increasing A.

    IF (stv%a & 1) = 0 DO     // -> 6314
    { // An unresolved reference at address A
      IF inrange_i(a, stvp+n+3) BREAK
      // This point is reached if there is an unresolved
      // ref at A which would be able to access an
      // indirection word at stvp+n+3 and so an indirect
      // data word must be compiled now. The +3 is to
      // allow for a possible skip jump instruction and
      // a possible filler byte.

      genindword(h3!r)
    }

    // At this point the reference at A is in range of
    // a resolving indirect data word and should be
    // removed from rlist if there is no chance that
    // it can be resolved by a direct relative address.
    TEST inrange_d(a, stvp)   // -> 6326
    THEN { p := r        // Keep the item.
         }
    ELSE { !p := !r   // Free item if already resolved
           freeblk(r) // and no longer in direct range.
           IF !p=0 DO rliste := p  // Correct RLISTE.
         }
  } REPEAT

  // At this point all necessary indirect data words have
  // been compiled.

  UNLESS skiplab=0 DO { setlab(skiplab)
                        skiplab, incode := 0, TRUE
                      }
}

AND dealwithrefs(n) = VALOF
{ // Ensure that n bytes of code can be compiled before
  // any forward references go out of range. This may
  // require the compilation of some indirect resolving words.
  // It return TRUE is it generated at least one resolving word.

  LET p = @rlist

  WHILE h1!p DO
  { LET np = h1!p
    LET addr = h2!np  // byte address
    LET lab = h3!np
    IF (stv%addr & 1) = 0
    { TEST labv!lab=-1
      THEN { UNLESS inrange_d(addr, n+2) DO
             { genindword(lab)
               RESULTIS TRUE
             }
           }
      ELSE { genindword(lab)
             RESULTIS TRUE
           }
    }

    p := np
  }
  RESULTIS FALSE
}

AND genindword(lab) BE  // Called only from chkrefs and dealwithrefs.
{ // This function generates an indirect reference resolving word
  // only used by relative addressing instructions that cannot use
  // direct relative addresses. Resolving words are used if the
  // relative address is too far back or too far forward, ie not
  // within direct relative addressing range.

  LET r = rlist // Assume RLIST ~= 0
                // This is because genindword is only called
                // when there is at least one outstanding
                // forward reference to label lab.
  IF incode DO
  { // Compile a skip jump around the resolving word
    skiplab := newlab()
    IF debug>0 DO
      wrcode("L%N", f_j, skiplab)
    codeb(f_j)
    codeb(0)
    relref(stvp-2, skiplab)
    incode := FALSE
  }
  aligneven()
  UNTIL r=0 DO
  { // r points to an rlist item of the form [next, addr, lab]
    IF h3!r=lab &
       (stv%(h2!r) & 1)=0 DO
    { fillref_i(h2!r, stvp)
    }
    r := !r
  }
  coder(lab) // Generate an indirect resolving word for label lab.
}

AND inrange_d(a, p) =
  a-126 <= p <= a+129
// The result is TRUE if direct relative instr (eg J) at
// A can address location P directly.

AND inrange_i(a, p) = VALOF
// The result is TRUE if indirect relative instr (eg J)
// at A can address a resolving word at p.
{ LET rel = (p-a-2)/2
  RESULTIS 0 <= rel <= 255
}

AND fillref_d(a, p) BE
{ stv%a := stv%a & 254  // Back to direct form if neccessary.
  stv%(a+1) := p-a+126
}

AND fillref_i(a, p) BE  // p is the (even) address of the resolving word.
                        // a is the address of a relative addressing instruction.
                        // This will be made indirect and the operand byte
                        // at a+1 will be set to the appropriate relative
                        // address.
{ stv%a := stv%a | 1    // Force indirect form.
  stv%(a+1) := (p-a-2)/2
}

AND relref(a, l) BE
// RELREF is only called just after compiling
// a relative reference instruction at
// address A (=stvp-2).
{ LET labval = labv!l
   IF labval>=0 &
      inrange_d(a, labval) DO
   { fillref_d(a, labval)
     RETURN
   }

   // All other references in RLIST have
   // addresses smaller than A and so RLIST will
   // remain properly ordered if this item
   // is added to the end.

   !rliste := newblk(0, a, l)
   rliste := !rliste
}



//SECTION "CCG8"


LET outputsection(lastsection) BE
{ WHILE reflist DO
  { cgerror("LABEL L%N UNSET", h3!reflist)
    reflist := !reflist
  }
  IF codestream DO
  { selectoutput(codestream)
    objword(t_hunk)
    objword(stvp/2)
    // Fudge for sections CCG6A and CCG7
    IF getw(0)=0 DO // Only fill in the length word if present
      putw(0, stvp/2) // Fill in the hunk length -- add by MR
    //writewords(stv, stvp>>1) // stvp>>1 is the number of 16-bit words to write
    //                         // stvp is assumed to be even.
    FOR i = 0 TO stvp-1 DO binwrch(stv%i)
    IF lastsection DO objword(t_end)
    selectoutput(oldoutput)
  }
}

AND objword(w) BE
{ binwrch(w)
  binwrch(w>>8)
}



//SECTION "CCG9"

//       Extra debugging code not in the standard codegenerator.




LET dboutput() BE
{ writef("%i5:", stvp)
  writes(" A="); wrkn(infok_a, infon_a)
  writes(" B="); wrkn(infok_b, infon_b)
  writef(" ssp=%i3", ssp)
  writef(" pndop=%s", op2str(pendingop))
  writef(" incode=%c", incode -> 'T', 'F')
  
  IF debug>=2 DO { writes("  STK: ")
                   FOR p=tempv TO arg1 BY 3  DO
                   { IF (p-tempv) REM 30 = 10 DO newline()
                     wrkn(h1!p,h2!p)
                     wrch('*s')
                   }
                 }
   
  IF debug=3 DO {  LET l = rlist
                   writes("*nrlist: ")
                   UNTIL l=0 DO { writef("%n L%n  ", l!1, l!2)
                                  l := !l
                                }
                   l := nlist
                   writes("*nnlist: ")
                   UNTIL l=0 DO { writef("L%n %x4  ", l!1, l!2)
                                  l := !l
                                }
                }
  newline()
}


AND wrkn(k,n) BE
{ LET s = VALOF SWITCHON k INTO
  { DEFAULT:       //sawritef(" wrkn: k=%n n=%n*n", k, n)
                   k := n
                   RESULTIS "?"
    CASE k_none:   RESULTIS "-"
    CASE k_numb:   RESULTIS "N%n"
    //CASE k_fnlab:  RESULTIS "F"
    CASE k_lvloc:  RESULTIS "@P%n"
    CASE k_lvglob: RESULTIS "@G%n"
    CASE k_lvlab:  RESULTIS "@L%n"
    CASE k_a:      RESULTIS "A"
    CASE k_b:      RESULTIS "B"
    CASE k_c:      RESULTIS "C"
    CASE k_loc:    RESULTIS "P%n"
    CASE k_glob:   RESULTIS "G%n"
    CASE k_lab:    RESULTIS "L%n"
    CASE k_loc0:   RESULTIS "0P%n"
    CASE k_loc1:   RESULTIS "1P%n"
    CASE k_loc2:   RESULTIS "2P%n"
    CASE k_loc3:   RESULTIS "3P%n"
    CASE k_loc4:   RESULTIS "4P%n"
    CASE k_glob0:  RESULTIS "0G%n"
    CASE k_glob1:  RESULTIS "1G%n"
    CASE k_glob2:  RESULTIS "2G%n"
  }
  writef(s, n)
}

AND wrcode(form, f, a, b) BE
{ IF debug=2 DO dboutput()
//sawritef("Calling wrcode(form=*"%s*", f=%i3, a=%i5, b=%i5*n", form, f, a, b)
  writef("%i4: ", stvp)
  wrfcode(f)
  writes("  ")
  writef(form, a, b)
  newline()
//abort(1234)
}

AND wrfcode(f) BE
{ LET s = VALOF SWITCHON f&31 INTO
  { DEFAULT:
    CASE  0: RESULTIS "     -     K   LLP     L    LP    SP    AP     A"
    CASE  1: RESULTIS "     -    KW  LLPW    LW   LPW   SPW   APW    AW"
    CASE  2: RESULTIS "   BRK   S0G  S0G1  S0G2     -     -     -     -"
    CASE  3: RESULTIS "    K3   K3G  K3G1  K3G2   LP3   SP3   AP3  L0P3"
    CASE  4: RESULTIS "    K4   K4G  K4G1  K4G2   LP4   SP4   AP4  L0P4"
    CASE  5: RESULTIS "    K5   K5G  K5G1  K5G2   LP5   SP5   AP5  L0P5"
    CASE  6: RESULTIS "    K6   K6G  K6G1  K6G2   LP6   SP6   AP6  L0P6"
    CASE  7: RESULTIS "    K7   K7G  K7G1  K7G2   LP7   SP7   AP7  L0P7"
    CASE  8: RESULTIS "    K8   K8G  K8G1  K8G2   LP8   SP8   AP8  L0P8"
    CASE  9: RESULTIS "    K9   K9G  K9G1  K9G2   LP9   SP9   AP9  L0P9"
    CASE 10: RESULTIS "   K10  K10G K10G1 K10G2  LP10  SP10  AP10 L0P10"
    CASE 11: RESULTIS "   K11  K11G K11G1 K11G2  LP11  SP11  AP11 L0P11"
    CASE 12: RESULTIS "   K12  K12G K12G1 K12G2  LP12  SP12  AP12 L0P12"
    CASE 13: RESULTIS " CODE1   L0G  L0G1  L0G2  LP13  SP13     -     -"
    CASE 14: RESULTIS "    LM   L1G  L1G1  L1G2  LP14  SP14     -     -"
    CASE 15: RESULTIS "   LM1   L2G  L2G1  L2G2  LP15  SP15 CODE2     -"
    CASE 16: RESULTIS "    L0    LG   LG1   LG2  LP16  SP16   NOP     -"
    CASE 17: RESULTIS "    L1    SG   SG1   SG2   SYS    S1    A1   NEG"
    CASE 18: RESULTIS "    L2   LLG  LLG1  LLG2   SWB    S2    A2   NOT"
    CASE 19: RESULTIS "    L3    AG   AG1   AG2   SWL    S3    A3  L1P3"
    CASE 20: RESULTIS "    L4   MUL   ADD    RV    ST    S4    A4  L1P4"
    CASE 21: RESULTIS "    L5   DIV   SUB   RV1   ST1   XCH    A5  L1P5"
    CASE 22: RESULTIS "    L6   REM   LSH   RV2   ST2  GBYT  RVP3  L1P6"
    CASE 23: RESULTIS "    L7   XOR   RSH   RV3   ST3  PBYT  RVP4  L2P3"
    CASE 24: RESULTIS "    L8    SL   AND   RV4  STP3   ATC  RVP5  L2P4"
    CASE 25: RESULTIS "    L9   SL$    OR   RV5  STP4   ATB  RVP6  L2P5"
    CASE 26: RESULTIS "   L10    LL   LLL   RV6  STP5     J  RVP7  L3P3"
    CASE 27: RESULTIS "  FHOP   LL$  LLL$   RTN  GOTO    J$ ST0P3  L3P4"
    CASE 28: RESULTIS "   JEQ   JNE   JLS   JGR   JLE   JGE ST0P4  L4P3"
    CASE 29: RESULTIS "  JEQ$  JNE$  JLS$  JGR$  JLE$  JGE$ ST1P3  L4P4"
    CASE 30: RESULTIS "  JEQ0  JNE0  JLS0  JGR0  JLE0  JGE0 ST1P4     -"
    CASE 31: RESULTIS " JEQ0$ JNE0$ JLS0$ JGR0$ JLE0$ JGE0$     -     -"
  }
  LET n = f>>5 & 7
  FOR i = 6*n+1 TO 6*(n+1) DO wrch(s%i)
}

AND op2str(op) = VALOF SWITCHON op INTO
  { DEFAULT:        RESULTIS "Unknown op"

    CASE s_true:    RESULTIS "true"  
    CASE s_false:   RESULTIS "false"  
    CASE s_rv:      RESULTIS "rv"  
    CASE s_fnap:    RESULTIS "fnap"  
    CASE s_mult:    RESULTIS "mult"  
    CASE s_div:     RESULTIS "div"  
    CASE s_rem:     RESULTIS "rem"  
    CASE s_plus:    RESULTIS "plus"  
    CASE s_minus:   RESULTIS "minus"  
    CASE s_query:   RESULTIS "query"  
    CASE s_neg:     RESULTIS "neg"  
    CASE s_abs:     RESULTIS "abs"  
    CASE s_eq:      RESULTIS "eq"  
    CASE s_ne:      RESULTIS "ne"  
    CASE s_ls:      RESULTIS "ls"  
    CASE s_gr:      RESULTIS "gr"  
    CASE s_le:      RESULTIS "le"  
    CASE s_ge:      RESULTIS "ge"  
    CASE s_not:     RESULTIS "not"  
    CASE s_lshift:  RESULTIS "lshift"  
    CASE s_rshift:  RESULTIS "rshift"  
    CASE s_logand:  RESULTIS "logand"  
    CASE s_logor:   RESULTIS "logor"  
    CASE s_eqv:     RESULTIS "eqv"  
    CASE s_neqv:    RESULTIS "neqv"  
    CASE s_lf:      RESULTIS "lf"  
    CASE s_lp:      RESULTIS "lp"  
    CASE s_lg:      RESULTIS "lg"  
    CASE s_ln:      RESULTIS "ln"  
    CASE s_lstr:    RESULTIS "lstr"  
    CASE s_ll:      RESULTIS "ll"  
    CASE s_llp:     RESULTIS "llp"  
    CASE s_llg:     RESULTIS "llg"  
    CASE s_lll:     RESULTIS "lll"  
    CASE s_needs:   RESULTIS "needs"  
    CASE s_section: RESULTIS "section"  
    CASE s_rtap:    RESULTIS "rtap"  
    CASE s_goto:    RESULTIS "goto"  
    CASE s_finish:  RESULTIS "finish"  
    CASE s_switchon:RESULTIS "switchon"  
    CASE s_global:  RESULTIS "global"  
    CASE s_sp:      RESULTIS "sp"  
    CASE s_sg:      RESULTIS "sg"  
    CASE s_sl:      RESULTIS "sl"  
    CASE s_stind:   RESULTIS "stind"  
    CASE s_jump:    RESULTIS "jump"  
    CASE s_jt:      RESULTIS "jt"  
    CASE s_jf:      RESULTIS "jf"  
    CASE s_endfor:  RESULTIS "endfor"  
    CASE s_xlab:    RESULTIS "xlab"  
    CASE s_lab:     RESULTIS "lab"  
    CASE s_stack:   RESULTIS "stack"  
    CASE s_store:   RESULTIS "store"  
    CASE s_rstack:  RESULTIS "rstack"  
    CASE s_entry:   RESULTIS "entry"  
    CASE s_save:    RESULTIS "save"  
    CASE s_fnrn:    RESULTIS "fnrn"  
    CASE s_rtrn:    RESULTIS "rtrn"  
    CASE s_res:     RESULTIS "res"  
    CASE s_datalab: RESULTIS "datalab"  
    CASE s_iteml:   RESULTIS "iteml"  
    CASE s_itemn:   RESULTIS "itemn"  
    CASE s_endproc: RESULTIS "endproc"  
    CASE s_debug:   RESULTIS "debug"  
    CASE s_none:    RESULTIS "none"  
    CASE s_getbyte: RESULTIS "getbyte"  
    CASE s_putbyte: RESULTIS "putbyte"  
  }

