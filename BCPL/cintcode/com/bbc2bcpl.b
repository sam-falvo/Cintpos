// This is a program to convert BBC BCPL to the modern version for
// with the 32 bit BCPL Cintcode System.

// Implemented by Martin Richards (c) 10 Dec 2019


/*
Usage:

bbc2bcpl "from/a,to/K"

Change history

13/12/2019
Initial implementation.


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
  TEST T64
  THEN RESULTIS tcb -> "POS64HDRS", "BCPL64HDRS"
  ELSE RESULTIS tcb -> "POSHDRS",   "BCPLHDRS"
}

GLOBAL {
// Globals used in LEX
chbuf:feg
decval; fltval; exponent; getstreams; charv
sectv; sectt; sectp // To hold the current $( tags
                    // Items are of the form [tag, charpos]
charpos
spacecount

hdrs  // MR 10/7/04
stdin
stdout
fromfilename
fromstream
tofilename
tostream
cvwrc
cvwrs
cvname
testpending

workvec
readdecimal; readnumber; rdstrch
token; wordnode; ch
rdtag; performget
lex; dsw; declsyswords; nlpending
lookupword; eqlookupword; rch
sourcenamev; sourcefileno; sourcenamevupb
skiptag; wrchbuf; chcount; lineno
nulltag; rec_p; rec_l
 
synerr; op2str

mk1; mk2; mk3
mk4; mk5; mk6; mk7
mk3list               // Free list of nodes of size 3
unmk3                 // Return a node of size 3
newvec
spacev
spacep
spacet
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

LET start() = VALOF
{ LET argv = VEC 50
  LET formatstr = "from/A,to/K,hard/S,oender/S,eqcases/S,t64/S,-h/S"
  LET res = 0

  stdout := output()
  stdin  := input()
  
  errmax   := 10
  errcount := 0
  fin_p, fin_l := level(), fin

  charpos := 0      // =0 if only spaces have been written since
  spacecount := 0   // the start of the current line.
                    // charpos is only updated when a no space character
		    // is written. Both are set to sero by newline.
  
  fromstream := 0
  tostream   := 0
  getstreams := 0

  // Allocate vector for source file names
  sourcenamevupb := 1000
  sourcenamev := getvec(sourcenamevupb)
  UNLESS sourcenamev DO
  { writef("Insufficient space available*n")
    errcount := 1
    GOTO fin
  }
  sourcefileno := 0
  FOR i = 0 TO sourcenamevupb-1 DO sourcenamev!i := 0  // Corrected 19/08/2018   
  bigender := (!"AAAAAAA" & 255) ~= 7    // =TRUE if on a bigender m/c

  UNLESS rdargs(formatstr, argv, 50) DO
  { writes("Bad arguments*n")
    errcount := 1
    argv!5 := TRUE // Cause help info to be output.
  }

  fromfilename := argv!0             // from/A
  tofilename   := argv!1             // to/K
  hard         := argv!2             // hard/S
  IF argv!3 DO bigender := ~bigender // oender/S
  eqcases := TRUE
  IF argv!4 DO eqcases  := ~eqcases  // EQCASES/S
  T64 := argv!5                      // t64/S
  
  IF argv!5 DO                       // -h/S
  { writef("*nUsage: bbc2bcpl *"%s*"*n", formatstr)
    writef("*nThis program converts BCPL for the BBC microcomputer*n")
    writef("to modern BCPL.*n")
    writef("*nBy default names and system words are looked up*n")
    writef("ignoring the case of letters, and output using the*n")
    writef("case settings of the first occurrence of each name.*n")
    writef("Reserved words are capitalized.*n")
    writef("This setting can be complemented using the eqcases option.*n")
    writef("*nThe following conversions are made:*n*n")
    writef("Dots in names are replaced by underlines.*n")
    writef("eq  ne  ls  le  gr  ge  =>  =  ~=  <  <=  >  >=*n")
    writef("logand  logor  lshift  rshift  or  =>  &  |  <<  >>  ELSE*n")
    writef("not  rv  lv  => ~  !  @*n")
    writef("||  /\  \/  =>  //  &  |*n")
    writef("DO is replaced by THEN if after TEST*n")
    writef("REM is replaced by MOD*n")
    writef("Extra close section brackets are inserted where necessary*n")
    writef("indented below the open section brackets being closed*n")
    writef("*n")
    GOTO fin
  }
  
  hdrs := default_hdrs()           // Set the default HDRS

  IF eqcases DO lookupword := eqlookupword // Ignore the case of
                                           // letters in names.
  
  sourcenamev!0 := fromfilename    // File number zero is the FROM file
  sourcefileno  := 0

  fromstream    := findinput(fromfilename)

  UNLESS fromstream DO { writef("Trouble with file %s*n", argv!0)
                         IF hard DO abort(1000)
                         errcount := 1
                         GOTO fin
                       }

  selectinput(fromstream)

  tostream := stdout // Default output stream
  IF tofilename DO
  { tostream := findoutput(tofilename)
    IF tostream=0 DO
    { writef("Trouble with code file %s*n", tofilename)
      errcount := 1
      GOTO fin
    }
  }

  spacev := getvec(100000)
  spacet := spacev+100000
  spacep := spacet

  sectv := newvec(200)  // For section bracket tags
  sectt := 200
  sectp := 0            // Not yet in a section.
  // Item in sectv are of the form [tag, charpos]
  // where tag is the wordnode for the tag of a $(,
  // or zero if the section bracket was {.
  // Tagged $) section brackets can close multiple
  // sections. This program automatically inserts
  // the extra close section brackets.
  // sectp is zero when not in a section, sectp=2
  // is the position in sectv holding the tag and
  // indentation of an outermost open section bracket.
  
  { LET b = VEC 64/bytesperword+1
    chbuf := b
    FOR i = 0 TO 63 DO chbuf%i := 0
    // Sourcefile 0 is the FROM filename
    // others are GET files of the current section
    sourcenamev!0 := argv!0
    sourcefileno := 0
    FOR i = 1 TO sourcenamevupb DO sourcenamev!i := 0 // Done for safety
    chcount, lineno := 0, (sourcefileno<<20) + 1

    nametablesize := 541

    charv      := newvec(256/bytesperword+1)
    charv%0 := 0
    nametable  := newvec(nametablesize) 
    FOR i = 0 TO nametablesize DO nametable!i := 0
    skiptag := 0
    declsyswords()

    selectoutput(tostream)
    
    rec_p, rec_l := level(), fin

    testpending := FALSE // To decide when DO should be THEN.
    rch()
 
    UNTIL ch=endstreamch DO lex()
  }

fin:
  IF getstreams    DO { LET p = getstreams
                        getstreams := !p
                        freevec(p)
                      }
  FOR i = 1 TO sourcefileno DO
  { LET str = sourcenamev!i
    IF str DO
    { //sawritef("freeing fileno %n %s*n", i, str)
      freevec(str)
    }
  }
  IF sourcenamev   DO freevec(sourcenamev)

  IF fromstream  DO endstream(fromstream)
  IF tostream      UNLESS tostream=stdout DO endstream(tostream)

  selectoutput(stdout)
  RESULTIS res
}

LET cvwrc(ch) BE UNLESS getstreams DO 
{ // Write a character but delay writing spaces in case
  // an appropriately indented close section bracket is
  // to be automatically inserted.
  IF ch='*s' DO
  { spacecount := spacecount+1
    RETURN
  }
  
  IF ch='*n' | ch='*p' DO
  { charpos, spacecount := 0, 0
    wrch('*n')
    RETURN
  }
  // The character is neither a space nor a newline so
  // output it after any pending spaces.
  FOR i = 1 TO spacecount DO wrch('*s')
  wrch(ch)
  deplete(cos)
  charpos := charpos+spacecount+1
  spacecount := 0
  // charpos is the number of characters on the output line so far.
}

AND cvwrs(s) BE UNLESS getstreams DO 
{ FOR i = 1 TO s%0 DO cvwrc(s%i)
  deplete(cos)
}

AND cvname(s) BE UNLESS getstreams DO 
{ FOR i = 1 TO s%0 DO
  { LET ch = s%i
    IF ch='.' DO ch := '_'
    cvwrc(ch)
  }
  deplete(cos)
}

AND wrindent(len) BE  UNLESS getstreams DO 
{ FOR i = 1 TO len DO cvwrc('*s')
  deplete(cos)
}

LET lex() BE
{ //writef("lex: entered, ch=%c*n", ch)

  { // Start of repeat loop
    token := 0
    // token is set to s_string for strings to allow
    // GET directives to work. All other lexical tokens
    // leave token = 0.
    SWITCHON ch INTO
 
    { DEFAULT:
            { LET badch = ch
	      cvwrc(ch)
              ch := '*s'
              synerr("Illegal character %x2", badch)
	      rch()
	      RETURN
            }

      CASE '*n':  // Newline character
               lineno := lineno + 1
      CASE '*p':  // Newpage character - do not increment lineno
      CASE '*c':
               charpos := 0
      CASE '*t':
      CASE '*s':
               cvwrc(ch)
               rch()
               LOOP // In case the next symbol is a string
	            // in a GE directive.

      CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
      CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
              readdecimal()
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
	      //writef("token=%n*n", token)
	      //abort(9986)
              SWITCHON token INTO
              { DEFAULT:
	          cvname(@h3!wordnode)
	          RETURN

                CASE s_get:  cvname(@h3!wordnode)
		             performget()
		             RETURN

                CASE s_eq:
                CASE s_ne:
                CASE s_ls:
                CASE s_le:
                CASE s_gr:
                CASE s_ge:
                CASE s_lv:
                CASE s_rv:
                CASE s_logand:
                CASE s_logor:
                CASE s_lshift:
                CASE s_rshift:
		CASE s_not:
		CASE s_else:
		CASE s_mod:
		           cvwrs(op2str(token))
			   RETURN

                CASE s_do: TEST testpending
		           THEN cvname("THEN")
		           ELSE cvname("DO")
			   testpending := FALSE
			   RETURN

                CASE s_test:
		           cvname("TEST")
			   testpending := TRUE
			   RETURN
              }
	      RETURN
 
      CASE '$':
              rch()
              IF ch='$' | ch='<' | ch='>' | ch='~'  DO
              { LET k = ch
	        cvwrc('$')
	        cvwrc(ch)
                lookupword(rdtag('<'))
		FOR i = 2 TO charv%0 DO cvwrc(charv%i)
	        RETURN
              }
 
              IF ch='(' DO
	      { lookupword(rdtag('$'))
		IF sectp>=sectt DO synerr("Sections nested to deeply")
		sectp := sectp + 2
		IF sectp > sectt DO synerr("Sections too deep")
		sectv!sectp     := wordnode            // S( bracket tag
		sectv!(sectp+1) := charpos+spacecount  // Indentation of $(
                cvwrs("$(")
		FOR i = 2 TO charv%0 DO cvwrc(charv%i) // Write the tag
		//prsectv()
		RETURN
	      }

	      IF ch=')' DO
	      { LET p = sectp
	        lookupword(rdtag('$'))
		// wordnode the the $) tag
		// If this matches the the current open section bracket
		// just output it.
		IF sectv!sectp = wordnode DO
		{ cvwrs("$)")
		  FOR i = 2 TO charv%0 DO cvwrc(charv%i) // Write the tag
		  sectp := sectp-2
		  RETURN
		}
		IF wordnode=nulltag DO
		  synerr("Untagged $) cannot close multiple sections")

                // Search for the matching open section bracket
		UNTIL p<=0 DO
		{ //writef("%n compare %s with %s*n",
		  //        p/2, @(h3!wordnode), @(h3!(sectv!p)))
		  IF sectv!p = wordnode BREAK
		  p := p-2
		}

                IF p<=0 DO
		{ // No matching $( found.
		  synerr("Unmatched $)")
		  cvwrs("$)")
		  FOR i = 2 TO charv%0 DO cvwrc(charv%i) // Write the tag
		  RETURN
		}

		// Matching section bracket found, so
		// insert one or more '$)'s
		UNTIL sectp<p DO
		{ LET tagnode = sectv!sectp
		  LET indent  = sectv!(sectp+1) // Indent of its $(
		  sectp := sectp-2
		  IF charpos DO cvwrc('*n')
		  charpos, spacecount := 0, indent
		  TEST tagnode
		  THEN { LET tagstr = @h3!tagnode
		         cvwrs("$)")
		         FOR i = 2 TO tagstr%0 DO cvwrc(tagstr%i)
		       }
		  ELSE { cvwrc('}')
		       }
		}
		sectp := sectp-2
		//prsectv()
		RETURN
	      }

	      synerr("'$)' out of context")
              RETURN
 
      CASE '{': IF sectp>=sectt DO synerr("Sections nested to deeply")
		sectp := sectp + 2
		sectv!sectp     := 0                  // Section tag for {
		sectv!(sectp+1) := charpos+spacecount // Indentation of {
                cvwrc('{')
		rch()
		//prsectv()
		RETURN
      
      CASE '}':
      	      { UNLESS sectp>0 & sectv!sectp=0 DO
	        { rch()
		  synerr("Unmatched }")
		  RETURN
		}

		sectp := sectp-2
                cvwrc('}')
	        rch()
		//prsectv()
		RETURN
	      }

      CASE '#':
              cvwrc(ch)
              rch()
              IF '0'<=ch<='7' DO
              { cvwrc(ch)
	        readnumber( 8, 100)
                RETURN
              }
              IF ch='b' | ch='B' DO
              { cvwrc(ch)
	        rch()
                readnumber( 2, 100)
                RETURN
              }
              IF ch='o' | ch='O' DO
              { cvwrc(ch)
	        rch()
                readnumber( 8, 100)
                RETURN
              }
              IF ch='x' | ch='X' DO
              { cvwrc(ch)
	        rch()
		//abort(5566)
                readnumber(16, 100)
                RETURN
              }
              IF ch='(' DO
              { cvwrc(ch)
	        rch()
                RETURN
              }
              UNLESS ch<32 DO
	      { cvwrc(ch)
	        rch()
	        RETURN
              }

              synerr("'#' out of context")
	      rch()
	      RETURN

      CASE '[': cvwrc('(')
                rch()
                RETURN

      CASE ']': cvwrc(')')
                rch()
                RETURN

      CASE '(':      
      CASE ')':
      CASE '?':
      CASE ',':
      CASE ';':
      CASE '@':
      CASE '!':
      CASE '**':
      CASE '+':
      CASE '&':
      CASE '=':
      CASE '%': cvwrc(ch)
                rch()
                RETURN

      CASE '.': cvwrc(ch)
                rch()
                UNLESS getstreams RETURN
		synerr("A section separating dot is not allowed in GET files")
		RETURN



      CASE '|':
              cvwrc(ch)
              rch()
	      IF ch='|' DO
              { { cvwrc(ch)
	          rch()
		} REPEATUNTIL ch='*n' |
                              ch=endstreamch
              }
              RETURN
 
      CASE '/':
              rch()
              IF ch='\' DO
              { cvwrc('&')
	        rch()
                RETURN
              }
              IF ch='/' DO
              { cvwrs("//")
	        { rch()
		  IF ch='*n' | ch=endstreamch RETURN
		  cvwrc(ch)
		} REPEAT
              }
 
              IF ch='**' DO
              { // Start of a /* comment.
	        LET depth = 1
                cvwrs("/**")
                { // Read the comment characters
		  rch()
		  cvwrc(ch)
                  IF ch='**' DO
                  { { rch()
		      cvwrc(ch)
		    } REPEATWHILE ch='**'
                    IF ch='/' DO
		    { // End of comment symbol found
		      depth := depth-1
		      LOOP
		    }
                  }
                  IF ch='/' DO
                  { rch()
		    cvwrc(ch)
                    IF ch='**' DO
		    { depth := depth+1
		      LOOP
		    }
                  }
                  IF ch='*n' DO lineno := lineno+1
                  IF ch=endstreamch DO synerr("Missing '**/'")
                } REPEATUNTIL depth=0

                rch()
                RETURN
              }
              cvwrc('/')
	      RETURN

 
      CASE '~':
	      cvwrc(ch)
              rch()
              IF ch='=' DO
	      { cvwrc(ch)
	        rch()
              }
              RETURN
 
      CASE '\':
              rch()
              IF ch='/' DO
              { cvwrc('|')
                rch()
		RETURN
              }
              IF ch='=' DO
	      { cvwrs("~=")
		rch()
		RETURN
              }
              cvwrc('~')
	      rch()
              RETURN
 
      CASE '<':            // <=  <<  <>  <
              cvwrc(ch)
              rch()
              IF ch='=' | ch='<' | ch='>' DO
	      { cvwrc(ch)
	        rch()
	      }
              RETURN
 
      CASE '>':            // >=  >>  >
              cvwrc(ch)
	      rch()
              IF ch='=' | ch='>' DO
	      { cvwrc(ch)
	        rch()
              }
              RETURN
 
      CASE '-':            // ->  -
              cvwrc(ch)
	      rch()
              IF ch='>' DO
	      { cvwrc(ch)
	        rch()
	      }
              RETURN
 
      CASE ':':            // :=  ::  :
              cvwrc(ch)
              rch()
              IF ch='=' | ch=':' DO
	      { cvwrc(ch)
	        rch()
              }
              RETURN
 
      CASE '"':
           { LET len = 0
	     token := s_string
	     cvwrc(ch)
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
             cvwrc('"')
             charv%0 := len
             wordnode := newvec(len/bytesperword+2)
             h1!wordnode := s_string
             FOR i = 0 TO len DO (@h2!wordnode)%i := charv%i
	     rch()
             RETURN
          }
 
      CASE '`':
      CASE '*'':
              cvwrc('*'')
              rch()
              encoding := defaultencoding
              decval := rdstrch()
              token := s_number
              UNLESS ch='*'' | ch='`' DO synerr("Bad character constant")
	      cvwrc('*'')
              BREAK
 
      CASE endstreamch:
              IF getstreams DO
              { // Return from a 'GET' stream
                LET p = getstreams
                endread()
                ch           := h4!getstreams
                lineno       := h3!getstreams
                fromstream   := h2!getstreams
                getstreams   := h1!getstreams
                freevec(p) // Free the GET node
                selectinput(fromstream)
                LOOP
              }
              // endstreamch => EOF only at outermost GET level 
              token := s_eof
              RETURN
    }
  } REPEAT
 
  rch()
}

AND prsectv() BE //IF FALSE DO
{ newline()
  FOR p = 2 TO sectp BY 2 DO
    writef("   %n:%s %n",
           p/2, @(h3!(sectv!p)), sectv!(p+1))
//  abort(8877)
}

LET lookupword(word) = VALOF
{ LET len, i = word%0, 0
  LET hashval = 19609 // This and 31397 are primes.
writef("lookupword: word='%s'*n", word)
  FOR j = 0 TO len DO hashval := (hashval XOR word%j) * 31397
  hashval := (hashval>>1) MOD nametablesize
  wordnode := nametable!hashval
 
  UNTIL wordnode=0 | i>len TEST (@h3!wordnode)%i=word%i
                           THEN i := i+1
                           ELSE wordnode, i := h2!wordnode, 0
 writef("lookupword: wordnode=%n*n", wordnode)
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
 
  IF wordnode RESULTIS h1!wordnode // Matching name node found
//abort(9987)
  // Matching name node not found, so create one.
  wordnode := newvec(len/bytesperword+2)
  h1!wordnode, h2!wordnode := s_name, nametable!hashval
  FOR i = 0 TO len DO (@h3!wordnode)%i := word%i
  nametable!hashval := wordnode
  RESULTIS s_name
}
 
AND dsw(word, sym) BE { lookupword(word); h1!wordnode := sym  }
 
AND declsyswords() BE
{ dsw("AND", s_and)
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
  dsw("FALSE", s_false)
  dsw("FINISH", s_finish)
  dsw("FIX", s_fix)
  dsw("FLOAT", s_float)
  dsw("FLT", s_flt)
  dsw("FOR", s_for)
  dsw("GOTO", s_goto)
  dsw("GE", s_ge)//
  dsw("GR", s_gr)//
  dsw("GT", s_gr)//
  dsw("GLOBAL", s_global)
  dsw("GET", s_get)
  dsw("IF", s_if)
  dsw("INTO", s_into)
  dsw("LET", s_let)
  dsw("LV", s_lv)//
  dsw("LE", s_le)//
  dsw("LS", s_ls)//
  dsw("LT", s_ls)//
  dsw("LOGOR", s_logor)//
  dsw("LOGAND", s_logand)//
  dsw("LOOP", s_loop)
  dsw("LSHIFT", s_lshift)//
  dsw("MANIFEST", s_manifest)
  dsw("MOD", s_mod)
  dsw("NE", s_ne)//
  dsw("NEEDS", s_needs)
  dsw("NEQV", s_xor)
  dsw("NOT", s_not)//
  dsw("OF", s_of)
  dsw("OR", s_else)
  dsw("RESULTIS", s_resultis)
  dsw("RETURN", s_return)
  dsw("REM", s_mod)
  dsw("RSHIFT", s_rshift)//
  dsw("RV", s_rv)//
  dsw("REPEAT", s_repeat)
  dsw("REPEATWHILE", s_repeatwhile)
  dsw("REPEATUNTIL", s_repeatuntil)
  dsw("SECTION", s_section)
  dsw("SLCT", s_slct)
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
 
  nulltag := wordnode  // The null section bracket tag
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
 
 
AND rddefstring() = VALOF
{ LET pos = 1 // The position of the next defstring
              // character to consider
  LET defstringlen = defstring%0
  LET optch = ?

  { // Get next option name, if any
    LET len = 1
    charv%0, charv%1 := 1, '<'
 
    // Skip characters before option name
    WHILE pos<=defstringlen DO
    { optch := defstring%pos
      IF 'a'<=optch<='z' | 'A'<=optch<='Z' |
         '0'<=optch<='9' | optch='.' | optch='_' BREAK
      pos := pos+1
    }

    // Copy option name, if any, into charv
    WHILE pos<=defstringlen DO
    { optch := defstring%pos
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

    node!0, node!1, node!2, node!3 := getstreams, fromstream, lineno, ch
    getstreams := node
  }
  fromstream := stream
  selectinput(fromstream)
  lineno := (sourcefileno<<20) + 1
  rch()
}

AND readdecimal() BE
{ // Read an integer or floating point constant

  // A number must start with a digit.
  UNLESS '0'<=ch<='9' DO synerr("Bad number")

  WHILE '0'<=ch<='9' | ch='_' | ch='.' DO
  { // Deal with digits before e, if any.
    //writef("ch=%c pos=%n token=%n decval=%i4 exponent=%n*n",
    //        ch, pos, token, decval, exponent)
    cvwrc(ch)
    rch()
  }

  IF ch='e' | ch='E' DO
  { cvwrc(ch)
    rch()
    IF ch='-' DO { cvwrc(ch); rch() }
    WHILE '0'<=ch<='9' | ch='_' DO { cvwrc(ch); rch() }
  }
}

AND readnumber(radix, digs) = VALOF
// Read a binary, octal, decimal or hexadecimal unsigned number
// with between 1 and digs digits. Underlines are allowed.
// This function is only used for numerical constants starting
// with # or numerical escapes in string and character constants.
{ LET i, res = 0, 0
 
  { UNLESS ch='_' DO // Ignore underlines
    { LET d = value(ch)
      IF d>=radix BREAK
      cvwrc(ch)
      i := i+1       // Increment count of digits
      res := radix*res + d
      //IF radix=16 DO writef("readnumber: ch=%c res=%8x*n", ch, res)
    }
    rch()
  } REPEATWHILE i<digs

  UNLESS i DO synerr("Bad number i=%n ch=%n=%-'%c'", i, ch)
  RESULTIS res
}
 
 
AND value(ch) = '0'<=ch<='9' -> ch-'0',
                'A'<=ch<='F' -> ch-'A'+10,
                'a'<=ch<='f' -> ch-'a'+10,
                100
 
AND rdstrch() = VALOF
{ // Start of the REPEAT loop

  // Return the integer code for the next string character
  // Set result2=TRUE if *# character code was found, otherwise FALSE
  LET k = ch
  cvwrc(ch)

  IF k='*n' DO
  { lineno := lineno+1
    synerr("Unescaped newline character")
    rch()
    LOOP
  }
 
  IF k='**' DO
  { rch()
    // Convert uppercase escape letters to lowercase.
    IF 'A'<=ch<='Z' DO ch := ch + 'a' - 'A'
    cvwrc(ch)
    k := ch
    SWITCHON k INTO
    { CASE '*n': // Star followed by one of these character
      CASE '*c': // ignore white space and // comments until
      CASE '*p': // another star is found.
      CASE '*s':
      CASE '*t':
      CASE  '/': WHILE ch='*n' | ch='*c' | ch='*p' |
		       ch='*s' | ch='*t' | ch='/' DO
                 { // Read white space and comments
		   IF ch='/' DO // Read a '//' comment
		   { // '/' already written
		     rch()
		     IF ch='/' DO
		     { // Read the '//' comment
		       UNTIL ch='*n' | ch=endstreamch DO
		       { cvwrc(ch)
		         rch()
		       }
		       IF ch='*n' DO
		       { lineno := lineno+1
		         cvwrc(ch)
		         rch()
		       }
		       LOOP // May be followed by more white space
		     }
		     synerr("Bad '//' comment in a string")
		     LOOP
		   }
		   
		   IF //ch='*p' |  // Do not increment lineno
                      ch='*n' DO lineno := lineno+1
                   rch()
	           cvwrc(ch)
                 }
		 
		 // End of sequence of white space characters
		 // so ch should now be a star.
		 UNLESS ch='**' DO
		   synerr("Bad string or character constant")
		 // The star is already written
		 rch()
                 LOOP // Back to the beginning of rdstrch
		 
      DEFAULT:   synerr("Bad escape item in a string, ch='%c'", ch)
         
      CASE '**':
      CASE '*'':
      CASE '"':                    ENDCASE
         
      CASE 't':  k := c_tab;       ENDCASE
      CASE 's':  k := c_space;     ENDCASE
      CASE 'n':  k := c_newline;   ENDCASE
      CASE 'e':  k := c_escape;    ENDCASE
      CASE 'b':  k := c_backspace; ENDCASE
      CASE 'p':  k := c_newpage;   ENDCASE
      CASE 'c':  k := c_return;    ENDCASE
         
      CASE 'x':  // *xhh  -- A character escape in hexadecimal
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
		 cvwrc(ch)
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

      CASE '0':CASE '1':CASE '2':CASE '3':
      CASE '4':CASE '5':CASE '6':CASE '7':
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
{ spacep := spacep - n - 1;
  IF spacep<=spacev DO
  { errmax := 0  // Make it fatal
    synerr("More workspace needed")
  }
  RESULTIS spacep
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
  RESULTIS p
}
 
AND mk7(x, y, z, t, u, v, w) = VALOF
{ LET p = newvec(6)
  p!0, p!1, p!2, p!3, p!4, p!5, p!6 := x, y, z, t, u, v, w
  RESULTIS p
}
 
 
AND synerr(mess, a, b) BE
{ LET fno = lineno>>20
  LET ln = lineno & #xFFFFF
  LET filename = sourcenamev!fno
  errcount := errcount + 1
  writef("*nError near ")
  IF filename DO writef("%s", filename)
  writef("[%n]:  ", ln)
  writef(mess, a, b)
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

AND op2str(op) = VALOF SWITCHON op INTO
{ DEFAULT:            writef("*nUnknown opname = %n*n", op)
                      RESULTIS "Op %n"

  CASE s_abs:         RESULTIS "ABS"
  CASE s_and:         RESULTIS "AND"
  CASE s_ass:         RESULTIS ":="
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
  CASE s_byteap:      RESULTIS "%"
  CASE s_case:        RESULTIS "CASE"
  CASE s_colon:       RESULTIS ":"
  CASE s_comma:       RESULTIS ","
  CASE s_cond:        RESULTIS "->"
  CASE s_constdef:    RESULTIS "CONSTDEF"
  CASE s_datalab:     RESULTIS "DATALAB"
  CASE s_default:     RESULTIS "DEFAULT"
  CASE s_div:         RESULTIS "/"
  CASE s_do:          RESULTIS "DO"
  CASE s_dot:         RESULTIS "."
  CASE s_else:        RESULTIS "ELSE"
  CASE s_eof:         RESULTIS "EOF"
  CASE s_endcase:     RESULTIS "ENDCASE"
  CASE s_endfor:      RESULTIS "ENDFOR"
  CASE s_endproc:     RESULTIS "ENDPROC"
  CASE s_entry:       RESULTIS "ENTRY"
  CASE s_eq:          RESULTIS "="
  CASE s_eqv:         RESULTIS "EQV"
  CASE s_fabs:        RESULTIS "#ABS"
  CASE s_fadd:        RESULTIS "#+"
  CASE s_false:       RESULTIS "FALSE"
  CASE s_fass:        RESULTIS "FASS"
  CASE s_fcond:       RESULTIS "FCOND"
  CASE s_fdiv:        RESULTIS "#/"
  CASE s_feq:         RESULTIS "#="
  CASE s_fge:         RESULTIS "#>="
  CASE s_fgr:         RESULTIS "#>"
  CASE s_fglobal:     RESULTIS "FGLOBAL"
  CASE s_finish:      RESULTIS "FINISH"
  CASE s_fix:         RESULTIS "FIX"
  CASE s_fle:         RESULTIS "FLE"
  CASE s_float:       RESULTIS "FLOAT"
  CASE s_flt:         RESULTIS "#<"
  CASE s_fls:         RESULTIS "#<"
  CASE s_fltop:       RESULTIS "FLTOP"
  CASE s_fmanifest:   RESULTIS "FMANIFEST"
  CASE s_fmod:        RESULTIS "#MOD"
  CASE s_fnap:        RESULTIS "FNAP"
  CASE s_fnrn:        RESULTIS "FNRN"
  CASE s_fndef:       RESULTIS "FNDEF"
  CASE s_fne:         RESULTIS "#~="
  CASE s_fneg:        RESULTIS "#-"
  CASE s_fnum:        RESULTIS "FNUM"
  CASE s_fmul:        RESULTIS "#**"
  CASE s_fstatic:     RESULTIS "FSTATIC"
  CASE s_fsub:        RESULTIS "#-"

  CASE s_for:         RESULTIS "FOR"
  CASE s_ge:          RESULTIS ">="
  CASE s_get:         RESULTIS "GET"
  CASE s_getbyte:     RESULTIS "GETBYTE"
  CASE s_global:      RESULTIS "GLOBAL"
  CASE s_goto:        RESULTIS "GOTO"
  CASE s_gr:          RESULTIS ">"
  CASE s_if:          RESULTIS "IF"
  CASE s_into:        RESULTIS "INTO"
  CASE s_itemn:       RESULTIS "ITEMN"
  CASE s_jf:          RESULTIS "JF"
  CASE s_jt:          RESULTIS "JT"
  CASE s_jump:        RESULTIS "JUMP"
  CASE s_lab:         RESULTIS "LAB"
  CASE s_le:          RESULTIS "<="
  CASE s_let:         RESULTIS "LET"
  CASE s_lf:          RESULTIS "LF"
  CASE s_lg:          RESULTIS "LG"
  CASE s_ll:          RESULTIS "LL"
  CASE s_llg:         RESULTIS "LLG"
  CASE s_lll:         RESULTIS "LLl"
  CASE s_llp:         RESULTIS "LLP"
  CASE s_ln:          RESULTIS "LN"
  CASE s_logand:      RESULTIS "&"
  CASE s_logor:       RESULTIS "|"
  CASE s_loop:        RESULTIS "LOOP"
  CASE s_lp:          RESULTIS "LP"
  CASE s_lparen:      RESULTIS "LPAREN"
  CASE s_ls:          RESULTIS "<"
  CASE s_lsect:       RESULTIS "{"
  CASE s_lshift:      RESULTIS "<<"
  CASE s_lstr:        RESULTIS "LSTR"
  CASE s_lv:          RESULTIS "@"
  CASE s_manifest:    RESULTIS "MANIFEST"
  CASE s_mthap:       RESULTIS "MTHAP"
  CASE s_mul:         RESULTIS "**"
  CASE s_name:        RESULTIS "NAME"
  CASE s_ne:          RESULTIS "~="
  CASE s_needs:       RESULTIS "NEEDS"
  CASE s_neg:         RESULTIS "-"
  CASE s_none:        RESULTIS "NONE"
  CASE s_not:         RESULTIS "~"
  CASE s_number:      RESULTIS "NUMBER"
  CASE s_of:          RESULTIS "OF"
  CASE s_add:         RESULTIS "+"
  CASE s_putbyte:     RESULTIS "PUTBYTE"
  CASE s_query:       RESULTIS "?"
  CASE s_mod:         RESULTIS "MOD"
  CASE s_repeat:      RESULTIS "REPEAT"
  CASE s_repeatuntil: RESULTIS "REPEATUNTIL"
  CASE s_repeatwhile: RESULTIS "REPEATWHILE"
  CASE s_res:         RESULTIS "RES"
  CASE s_resultis:    RESULTIS "RESULTIS"
  CASE s_return:      RESULTIS "RETURN"
  CASE s_rparen:      RESULTIS "RPAREN"
  CASE s_rsect:       RESULTIS "}"
  CASE s_rshift:      RESULTIS ">>"
  CASE s_rstack:      RESULTIS "RSTACK"
  CASE s_rtap:        RESULTIS "RTAP"
  CASE s_rtdef:       RESULTIS "RTDEF"
  CASE s_rtrn:        RESULTIS "RTRN"
  CASE s_rv:          RESULTIS "!"
  CASE s_save:        RESULTIS "SAVE"
  CASE s_sbra:        RESULTIS "["
  CASE s_section:     RESULTIS "SECTION"
  CASE s_semicolon:   RESULTIS ";"
  CASE s_seq:         RESULTIS "<>"
  CASE s_sg:          RESULTIS "SG"
  CASE s_sket:        RESULTIS "]"
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
  CASE s_sub:         RESULTIS "-"
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
  CASE s_xor:         RESULTIS "XOR"
}

