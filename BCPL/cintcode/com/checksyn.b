/*
This program determines whether a given BCPL program is syntactically
correct.  It is based on the syntax transiion diagrams in an appedix
of the BCPL manual bcplman.pdf. 

The program first reads the entire source program placing the lexical
tokens in a vector before attempting to parse it. If a syntax error is
detected it successively tries deleting, replacing or inserting a
token in the vector. The most successful modification is
chosem. Although the change is unlikely to create a semantically
correct program, it will usually allows better syntactic error
messages to be generated. This program is currently under development
and at the moment only finds syntactic errors without attempting to
find an optimal recovery.

Implemention started by Martin Richards (c) 10 Mar 2022

Change history

2301/2025
Updating the program to agree with the latest syntax specification in
{\tt bcplman.pdf}.

20/07/2022
Performing a systematic check that this program agrees precisely with
the current syntax transition diagrams.

24/03/2022
After initially considering changing lex to deal with brackets, I have
decided to leave lex more or less unchanged, except the global
sectiontag has been added. It is set to the tag of $( and $) backets,
having the value nulltag when the bracket is untagged. It is set to
zero for the section brackets { and }.

27/02/2022
First version of checkksyn.b was started.

*/


SECTION "checksyn"

GET "libhdr"
GET "bcplfecg"
 
GLOBAL {
stdin:feg
stdout
sourcenamev    // For source filenames
sourcefileno; sourcenamevupb
fromfilename

// Globals used in LEX
chbuf; charv; ch
token; wordnode

tokval   // Value associated with token as read by rdtok
         // If token=s_lsect or s_rsect it holds the tag or zero
         // If token=s_name   it holds the characters of the name
	 // If token=s_number it holds its integer value
	 // If token=s_fnum   it holds its floating point value
toktab
tokindex

decval; fltval; exponent
sectiontag // For the tag of $( and $) brackets or zero for { and }.

getstreams
hdrs  // MR 10/07/04
spacev; spacep; spacevupb
readdecimal; readnumber; rdstrch
rdtag; performget
lex; dsw; declsyswords
nlpending  // This is set to TRUE when token holds the first token ofa line.

eclass     // =2 If the latest call of rdexp found a routine call,
           // =1 If the latest call od rdexp found a name.
	   // =0 If neither of the above.

lookupword; eqlookupword; rch
skiptag; wrchbuf; chcount; lineno
nulltag

rec_p; rec_l // The syntax error recovery point. This is typically
             // in rdsections.

rdtok       // Read the next token from position p = ABS tokenp of the
            // self expanding vector tokensxv. If p >= tokenpmax it sets
	    // token and tokval to s_eof and zero, and sets lineno to
	    // the fileno/line of the previous entry in the vector or one
	    // if there is no previous entry.
	    // If tokenp<0 the token read by the previous call of rdtok
	    // was an insertion, and so this call sets token, tokval and
	    // lineno to values in the current entry bfore setting tokenp
	    // to p+t_size ready for the next call.
	    // If tokenp>0 the token returned depends on the setting of
	    // index = t_index!tokenp, as follows.
	    
	    // index = -1   This entry has not been modified so copy the
	    //              fields tok, val and lno into token, tokval
	    //              and lineno and move tokenp point to the next
	    //              entry. 

            // index =  0   The entry has been deleted so move tokenp to
	    //              point to the next entry and re-enter rdtok.

            // index = 2n-1 This represents the insertion of tok and val
	    //              taken from tab!(2n-1) and tab!2n. tokenp is
	    //              negated to cause the next call of rdtok to
	    //              return the original token at this position.

            // index = 2n   This represents a replacement of the current
	    //              token by tok and val taken from tab!(2n-1)
	    //              and tab!2n. tokenp is moved to the next entry
	    //              to cause the next call of rdtok to skip over
	    //              the original token at this position.

opname

rdtokens    // Read all the tokens into tokv
prtokens
prtoken
prbrackets
prtag

pushtok           // Used by rdtokens to push a token item into
                  // the self expanding vector.

tokvupb; tokv // Components of the self expanding vector of tokens.
tokensxv          // This will point to the pair [tokvupb tokv]
                  // Itis only used in calls of sxvpushval when pushing
		  // values into the self expanding vector.

tokenp       // =0 or is the subscript of a token item in tokv.
tokenpmax    // =0 or is the subscript of the last token item in tokv.
             // It is set by rdtokens.

tokenphwm    // Best recovery distance so far.
tokenplim    // The limit of how large tokenphwm can be.
tokenq       // The pointer to the token item currently being modified.
besttokenq   // The pointer to the token item that has the index value
             // that gives the best recovery found so far.
bestindex    // The corresponding best index.

parsemode    // =0  for normal parsing possibly after a token has
             //     been modified.
             // =1  when testing how successful a token modification is.

rel2patrel

rdprog
rdsections

rdcdefs            // (n) n=1 outer level declaration
                   //     n=0 non outer level declaration

rdmatchlist
rdmatchitem

rnbpat; rbpat    // Read a basic pattern
rnpat; rpat      // (n) Read a pattern of precedence n 

rncom; rcom; rnbcom; rbcom

rnexp; rexp; rnbexp; rbexp
rnexplist; rexplist

lexerr; synerr; paterr

mk1; mk2; mk3
mk4; mk5
newvec

brackettokv  // These are only used while in rdtokenens.
bracketvalv  // They hold the stack of currently open bbrackets
bracketposv  // This holds the subscripts of open bracket token
             // items in tokv.

bracketdepth // The current nesting depth of brackets.

// The tokens items for GLOBAL, MANIFEST, STATIC, LET and AND
// have their tokval fields set to their bracketting depth.
// After a syntactic error input is sometimes skipped to one of
// these tokens at the outermost level before resuming the parse.
}
 
MANIFEST {
c_backspace =  8
c_tab       =  9
c_newline   = 10
c_newpage   = 12
c_return    = 13
c_escape    = 27
c_space     = 32

// Token item selectors
t_tok=0       // The token
t_val         // Associated value
t_lno         // The packed file and line number.
t_tab         // Table of suggested replacements/insertions
t_index       // The subscript of the current selected correction.

t_size
t_upb=t_size-1
}

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

LET start() = VALOF
{ LET p = 0
  LET spacevupb = 0
  LET argformat = "FROM/A,TO/K,SIZE/K/N,HDRS/K,-d/S"
  AND argv = VEC 50

  writef("*nchecksyn (23 Jan 2024)*n")

  stdin  := input()
  stdout := output()
  rec_p, rec_l := level(), rec
  fin_p, fin_l := level(), fin

  UNLESS s_opmax < 256 DO
  { writef("SYSTEM ERROR: s_opmax=%n is too large*n")
    abort(999)
    result2 := 0
    RESULTIS 20
  }
  
  errmax   := 10
  errcount := 0

  flt0  := sys(Sys_flt, fl_mk,  0, 0)
  flt1  := sys(Sys_flt, fl_mk,  1, 0)
  flt10 := sys(Sys_flt, fl_mk, 10, 0)

  spacev       :=  0
  fromstream   :=  0
  getstreams   :=  0
  sourcefileno := -1 // No source files yet
  sourcenamev  :=  0 // Not allocated yet
  tokv         :=  0
  tokenp       :=  0
  tokenpmax    :=  0
  
  sysprint := stdout
  selectoutput(sysprint)
 
  IF rdargs(argformat, argv, 50)=0 DO
  { writef("Bad arguments for format:*n%s*n", argformat)
    errcount := 1
    GOTO fin
  }

  bigender := (!"AAAAAAA" & 255) ~= 7    // =TRUE if on a bigender m/c

  T16, T32, T64 := FALSE, TRUE, FALSE
  targetbytelen, targetbitlen := 4, 32   // T32 is the default setting
  
  fromfilename := argv!0                 // FROM/A
  tofilename   := argv!1                 // TO/K    zero or a file name
  
  spacevupb := 200_000
  IF argv!2 DO spacevupb := !argv!2      // SIZE/K/N
  IF spacevupb<10_000 DO spacevupb := 10_000
  spacev := getvec(spacevupb)

  IF spacev=0 DO
  { writes("Insufficient memory*n")
    errcount := 1
    GOTO fin
  }

  spacep := spacev + spacevupb  // Space is allocated from the top

  // It is now safe to call newvec
  
  hdrs := default_hdrs()                  // Set the default HDRS

  IF argv!3 DO hdrs := argv!3             // HDRS/K
  debug := argv!4                         // -d/S
  
  fromstream  := findinput(fromfilename) // FROM/A

  IF fromstream=0 DO { writef("Trouble with file %s*n", fromfilename)
                         IF hard DO abort(1000)
                         errcount := 1
                         GOTO fin
                       }

  selectinput(fromstream)
  
  selectoutput(sysprint)

  defaultencoding := UTF8
  encoding := defaultencoding
  hard := FALSE  // If TRUE abort on each error.

  // Allocate chbuf used in lexical error messages
  chbuf := newvec(64/bytesperword+1)
  FOR i = 0 TO 63 DO chbuf%i := 0
  chcount := 0

  // Allocate the vector for source file names
  sourcenamevupb := 1000 // Allow for plenty of source files
  sourcenamev := getvec(sourcenamevupb)
  UNLESS sourcenamev DO
  { writef("Insufficient space available*n")
    abort(999)
    errcount := 1
    GOTO fin
  }

  // Source file 0 is the FROM filename others are GET files
  sourcenamev!0 := fromfilename
  sourcefileno := 0
  FOR i = 1 TO sourcenamevupb DO sourcenamev!i := 0 // Done for safety

  lineno  := sourcefileno<<20 | 1

  decval, fltval := 0, flt0

  ch := 0
  rch() // Because lex expects ch to be the first character
        // of the next token

  tokvupb, tokv := 0, 0 // These are consecutive locations in
                            // the global vector. This pair forms the
                            // self expanding array that will hold
                            // all the tokens of the source program.
  tokensxv  := @tokvupb
  tokenp    := 0            // For safety but not needed.
  tokenpmax := 0

  token, tokval := 0, 0
  toktab, tokindex := 0, 0  // Not used at thhe moment

  rdtokens() // Read all the tokens of the program placing them in
             // the self expanding vector tokensxv.

  // After returning from rdtokens no more tokens will be pushed
  // into tokensxv and tokenpmax will be the subscript of the
  // last token item of the program.

  UNLESS tokv DO
  { writef("The program contains no tokens*n")
    abort(999)
    GOTO err
  }
  
  { // To help debug rdtok some modifications can be made to the
    // first tokens in tokv.

    LET p = tokv + 1 + 5*t_size // p is a pointer to the fifth token item
    t_tab!p := TABLE 6, s_add,0, s_valof,0, s_while,0

    // Note that index is >0 and odd for replacements.
  
    // Possible changes specified by the index field.
    //t_index!p := -1  // No replacement or insertion
    //t_index!p :=  0  // Skip over the first token
    //t_index!p :=  1  // Replace the current token with tab!1 and tab!2
    //t_index!p :=  2  // Insert tab!1 and tab!2 before the first token
    //t_index!p :=  3  // Replace the current token with tab!3 and tab!4
    //t_index!p :=  4  // Insert tab!3 and tab!4 before the first token
    //t_index!p :=  5  // Replace the current token with tab!5 and tab!6
    //t_index!p :=  6  // Insert tab!5 and tab!6 before the first token

    IF debug DO
    { writef("*nThe tokens in tokv*n*n")
      //tokenp := 0
      //lineno := 1
      //prtokens()
      //newline()
  
      tokenp := 0
      lineno := 1

      //IF FALSE DO
      { // Debugging output
        rdtok()
        IF nlpending DO writef("<NL>*n")
        prtoken(token, tokval)
        IF token=s_eof DO { newline(); BREAK }
        //abort(1000)
      } REPEAT
    }
  }
  
  tokenp := 0   // Initialise tokenp for the first call of rdtok.
  lineno := 1   // Line 1 of the FROM file

  rdtok()       // Read the first token of the program
    
//abort(8111)
  rdprog()      // Parse the program

rec:
fin:
  newline()
    
  TEST errcount=0
  THEN writef("No syntactic errors were found*n")
  ELSE writef("%n syntactic error%-%ps %-%p\was\were\ found*n", errcount)
  //abort(1666)
err:
  IF getstreams DO { LET p = getstreams
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

  IF sourcenamev DO freevec(sourcenamev)

  IF spacev DO freevec(spacev)
  //writef("The self expanding vector %n -> [%n, %n]*n",
  //        tokensxv, tokvupb, tokv)
  IF tokv DO freevec(tokv)
  IF fromstream  DO IF fromstream DO endstream(fromstream)
  UNLESS sysprint=stdout DO endstream(sysprint)

  selectoutput(stdout)

  result2 := 0
  RESULTIS 0
}

LET lex() BE
{ // This reads the next lexical token from the currently selected input,
  // ch will already hold the first character.
  // lex is only used when rdtokens is reading the tokens into tokv.
  LET assop = ?

  {
//sawritef("lex: ch=%i3 '%c'*n", ch, ch)
  SWITCHON ch INTO
 
  { DEFAULT:
      // The following gets around a
      // bug on the Itanium
      IF ch=endstreamch GOTO endstr

      lexerr("Illegal character %x2", ch)
      ch := '*s'
      LOOP

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
      { DEFAULT:
          RETURN

        CASE s_get:
	  performget(); LOOP                     // GET

        CASE s_bitsperbcplword:                  // BITSPERBCPLWORD
          token := s_number
          decval := targetbitlen // Target code word length
          RETURN

        // Some reserved words become assignment operators
        // if immediately followed by :=

        CASE s_mod:    assop := s_assmod;    GOTO chkass // MOD:=
        CASE s_lshift: assop := s_asslshift; GOTO chkass // LSHIFT:=
        CASE s_rshift: assop := s_assrshift; GOTO chkass // RSHIFT:=
        CASE s_logand: assop := s_asslogand; GOTO chkass // LOGAND:=
        CASE s_logor:  assop := s_asslogor;  GOTO chkass // LOGOR:=
        CASE s_eqv:    assop := s_asseqv;    GOTO chkass // EQV:=
        CASE s_xor:    assop := s_assxor;    GOTO chkass // XOR:=
      }

      CASE '$':
        rch()
        IF ch='$' | ch='<' | ch='>' | ch='~' DO
        { // Deal with $$  $<  $>  $~
	  LET k = ch
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
 
          IF skiptag LOOP // If skipping enter lex again

          // Only process $<tag and $$tag if not skipping
 
          IF k='$' DO
          { // $$tag  complements the value of a tag
            h1!wordnode := token=s_true -> s_false, s_true
            LOOP  // Enter lex again
          }
 
          IF k='<' DO
          { // $<tag
            IF token=s_true LOOP // Option is set so don't skip
          }

          IF k='~' DO
          { // $~tag
            UNLESS token=s_true LOOP // Option is not set so don't skip
          }

          // Skip tokens until matching $>tag, EOF or end of section
          skiptag := wordnode
          UNTIL skiptag=0 | token=s_dot | token=s_eof DO lex()
          skiptag := 0
          RETURN
        }
 
        IF ch='(' DO  // $(
        { lookupword(rdtag('$'))
          token, sectiontag := s_lsect, wordnode
	  RETURN
        }
	
        IF ch=')' DO  // $)
	{ lookupword(rdtag('$'))
          token, sectiontag := s_rsect, wordnode
	  RETURN
        }
	
        lexerr("'$' out of context")
	ch := '*s'
        LOOP

      CASE '{': rch(); token, sectiontag := s_lsect, 0; RETURN
      CASE '}': rch(); token, sectiontag := s_rsect, 0; RETURN

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
        IF ch='x' | ch='X' DO                                   // #X7FF4
        { rch()
          decval := readnumber(16, 100)
          RETURN
        }
        IF ch='(' DO                                            // #(
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
        lexerr("'#' out of context")
	
        RETURN
	
      CASE '[': token := s_sbra;      BREAK                  // [
      CASE ']': token := s_sket;      BREAK                  // ]
      CASE '(': token := s_lparen;    BREAK                  // (
      CASE ')': token := s_rparen;    BREAK                  // )
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
		lexerr("A section separating dot is not allowed in GET files")
		LOOP

chkassx:        rch()
chkass:         UNLESS ch=':' RETURN
                rch()
                UNLESS ch='=' DO lexerr("Bad assignment operator")
                token := assop
                BREAK
 
      CASE '!': token, assop := s_vecap, s_assvecap;   GOTO chkassx // !:= or !
      CASE '**':token, assop := s_mul, s_assmul;       GOTO chkassx // *:= or *
      CASE '+': token, assop := s_add, s_assadd;       GOTO chkassx // +:= or +
      CASE '&': token, assop := s_logand, s_asslogand; GOTO chkassx // &:= or &
      CASE '|': token, assop := s_logor, s_asslogor;   GOTO chkassx // |:= or |
 
      CASE '/':
              rch()
              //IF ch='\' DO    // Disallow /\ for &
              //{ token, assop := s_logand, s_asslogand
              //  GOTO chkassx
              //}
              IF ch='/' DO
              { rch() REPEATUNTIL ch='*n' |
                                  //ch='*p' | // Do not increment lineno
                                  ch=endstreamch
                LOOP
              }
 
              IF ch='**' DO
              { LET depth = 1 // Depth of nesting in /* */ comments

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
                  IF ch=endstreamch DO lexerr("Missing '**/'")
                } REPEATUNTIL depth=0

                rch()
                LOOP
              }

              token, assop := s_div, s_assdiv
              GOTO chkass
 
      CASE '~':
              rch()
              IF ch='=' DO { token := s_ne;     BREAK }          // ~=
              token := s_not                                     // ~
              RETURN
 
      CASE '\':
              rch()
              //IF ch='/' DO    // Disallow \/ for |
              //{ token, assop := s_logor, s_asslogor
              //  GOTO chkassx
              //}
              IF ch='=' DO { token := s_ne;     BREAK }           // \=
              token := s_not                                      // \
              RETURN
 
      CASE '<': rch()
              IF ch='=' DO { token := s_le;     BREAK }            // <=
              IF ch='<' DO
              { token, assop := s_lshift, s_asslshift              // << or <<:=
                GOTO chkassx
              }
              IF ch='>' DO { token := s_seq;    BREAK }            // <>
              token := s_ls                                        // <
              RETURN
 
      CASE '>': rch()
              IF ch='=' DO { token := s_ge;     BREAK }            // >=
              IF ch='>' DO
              { token, assop := s_rshift, s_assrshift              // >> or >>:=
                GOTO chkassx
              }
              token := s_gr                                        // >
              RETURN
 
      CASE '-': rch()
              IF ch='>' DO { token := s_cond; BREAK  }             // ->
              token, assop := s_sub, s_asssub                      // - or -:=
              GOTO chkass
 
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
                               IF len>=254 DO lexerr("Bad string constant")
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
                             IF len>=255 DO lexerr("Bad string constant")
                             charv%(len+1) := code // Ordinary ASCII char
                             len := len + 1
                             LOOP
                           }
                      ELSE { // Convert to UTF8 sequence
                             IF code<=#x7F DO
                             { IF len>=255 DO lexerr("Bad string constant")
                               charv%(len+1) := code   // 0xxxxxxx
                               len := len + 1
                               LOOP
                             }
                             IF code<=#x7FF DO
                             { IF len>=254 DO lexerr("Bad string constant")
                               charv%(len+1) := #b1100_0000+(code>>6)  // 110xxxxx
                               charv%(len+2) := #x80+( code    &#x3F)  // 10xxxxxx
                               len := len + 2
                               LOOP
                             }
                             IF code<=#xFFFF DO
                             { IF len>=253 DO lexerr("Bad string constant")
                               charv%(len+1) := #b1110_0000+(code>>12) // 1110xxxx
                               charv%(len+2) := #x80+((code>>6)&#x3F)  // 10xxxxxx
                               charv%(len+3) := #x80+( code    &#x3F)  // 10xxxxxx
                               len := len + 3
                               LOOP
                             }
                             IF code<=#x1F_FFFF DO
                             { IF len>=252 DO lexerr("Bad string constant")
                               charv%(len+1) := #b1111_0000+(code>>18) // 11110xxx
                               charv%(len+2) := #x80+((code>>12)&#x3F) // 10xxxxxx
                               charv%(len+3) := #x80+((code>> 6)&#x3F) // 10xxxxxx
                               charv%(len+4) := #x80+( code     &#x3F) // 10xxxxxx
                               len := len + 4
                               LOOP
                             }
                             IF code<=#x3FF_FFFF DO
                             { IF len>=251 DO lexerr("Bad string constant")
                               charv%(len+1) := #b1111_1000+(code>>24) // 111110xx
                               charv%(len+2) := #x80+((code>>18)&#x3F) // 10xxxxxx
                               charv%(len+3) := #x80+((code>>12)&#x3F) // 10xxxxxx
                               charv%(len+4) := #x80+((code>> 6)&#x3F) // 10xxxxxx
                               charv%(len+5) := #x80+( code     &#x3F) // 10xxxxxx
                               len := len + 5
                               LOOP
                             }
                             IF code<=#x7FFF_FFFF DO
                             { IF len>=250 DO lexerr("Bad string constant")
                               charv%(len+1) := #b1111_1100+(code>>30) // 1111110x
                               charv%(len+2) := #x80+((code>>24)&#x3F) // 10xxxxxx
                               charv%(len+3) := #x80+((code>>18)&#x3F) // 10xxxxxx
                               charv%(len+4) := #x80+((code>>12)&#x3F) // 10xxxxxx
                               charv%(len+5) := #x80+((code>> 6)&#x3F) // 10xxxxxx
                               charv%(len+6) := #x80+( code     &#x3F) // 10xxxxxx
                               len := len + 6
                               LOOP
                             }
                             lexerr("Bad Unicode character")
                           }
                    }
               ELSE { // Not a Unicode character
                      IF len=255 DO lexerr("Bad string constant")
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
              UNLESS ch='*'' DO lexerr("Bad character constant")
              BREAK
 
 endstr:
      //CASE endstreamch: // Commented out because of an Itanium bug
              IF getstreams DO
              { // Return from a 'GET' stream
                LET p = getstreams
                endread()
                ch           := h4!getstreams
                lineno       := h3!getstreams
                fromstream := h2!getstreams
                getstreams   := h1!getstreams
                freevec(p) // Free the GET node
                selectinput(fromstream)
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
 
AND dsw(word, sym) BE { lookupword(word); h1!wordnode := sym  }
 
AND declsyswords() BE
{ dsw("AND", s_and)  // Added some old 1980s style reserved words.
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
  dsw("SKIP", s_skip)               // Inserted 22/2/2022
  dsw("SLCT", s_slct)               // Inserted 11/7/2001
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
 
 
AND rdtag(ch1) = VALOF
{ LET len = 1
  charv%1 := ch1
 
  { rch()

    IF ch='.' DO // Disallow .. in tags since this is the range operator
    { LET k = rdch()
      unrdch()
      IF k='.' BREAK
    }
      
    UNLESS 'a'<=ch<='z' | 'A'<=ch<='Z' |
           '0'<=ch<='9' | ch='.' | ch='_' BREAK
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
  UNLESS token=s_string DO lexerr("Bad GET directive")
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
  { lexerr("Unable to find GET file %s", charv)
    RETURN
  }

  IF sourcefileno>=sourcenamevupb DO
  { lexerr("Too many GET files")
    RETURN
  }

  { LET len  = charv%0
    LET node = getvec(3)  // Freed at end of GET insertion
    LET str  = getvec(len/bytesperword+1) // Freed at end of compilation

    UNLESS node & str DO
    { IF node DO freevec(node)
      IF str  DO freevec(str)
      lexerr("getvec failure in performget")
    }
    FOR i = 0 TO len DO str%i := charv%i
    sourcefileno := sourcefileno+1
    sourcenamev!sourcefileno := str

    node!0, node!1, node!2, node!3 := getstreams, fromstream, lineno, ch
    getstreams := node
  }
  fromstream := stream
  selectinput(fromstream)
  lineno := (sourcefileno<<20) + 1 // First line of the new file
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

  // Ignore spaces
  WHILE ch='*s' | ch='*t' DO rch()

  // A number must start with a digit.
  UNLESS '0'<=ch<='9' DO lexerr("Bad number")

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
        IF token=s_fnum DO lexerr("Two decimal points in a number")
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
    lexerr("Floating point exponent out of range")

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

  UNLESS i DO lexerr("Bad number")
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
    lexerr("Unescaped newline character")
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

      DEFAULT:   lexerr("Bad string or character constant, ch=%n", ch)
         
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
                       lexerr("Bad string or character constant")
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
    lexerr("More workspace needed")
  }
  RESULTIS spacep
}
 
// Most of the following will not be needed since no parse tree
// is going to be constructed.

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
{ LET p = newvec(2)
  p!0, p!1, p!2 := x, y, z
  RESULTIS p
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
 
AND rdtok() BE
{ // If tokenp=tokenpmax there are no more tokens so return the
  // one pointed to by tokenpmax which should be the eoftoken.
  // If tokenp=0 we are at the start of the program so set
  // tokenp to 1 to read the first token.
  // If tokenp<0 rdtok would have returned an inserted token
  // in the previous call, and to on this call the original
  // token at this position must be returned.
  LET prevlineno = lineno

  { // Start of the deletion loop
  
    TEST tokenp<0
    THEN { // Return the original token after the insertion made
           // by the previous call of rdtok.
           LET p = -tokenp
           tokenp := p     // Ready for the next call of rdtok
           p := tokv + p // Absolute pointer to the token item.

           token  := t_tok!p     // The originsl token
           tokval := t_val!p
           lineno := t_lno!p

           nlpending := prevlineno~=lineno
           RETURN
         }
    ELSE { // tokenp>=0 so advance to the next token item.
           LET p, q = tokenp, 0
	   LET tab, index = ?, ?
           UNLESS p=tokenpmax DO p := p -> p+t_size, 1
	   tokenp := p
           p := tokv + p // Absolute pointer to the token item

           token  := t_tok!p
           tokval := t_val!p
           lineno := t_lno!p
           tab    := t_tab!p
           index  := t_index!p

           nlpending := prevlineno~=lineno

           IF index =-1 RETURN  // Normal return
           IF index = 0 LOOP    // This token is deleted.
           IF index =-2 LOOP    // This token was eliminated by lex.

           UNLESS 0 < index <= tab!0 DO
	   { writef("System error: in rdtok*n")
	     abort(999)
	   }
           // This is a replacement or insertion
           q := tab + (index & -2) + 1
           token  := q!0
           tokval := q!1

           IF (index & 1) = 0 DO tokenp := -tokenp // An insertion
           RETURN
         }
  } REPEAT
}

AND pushtok(tok, val) BE
{ // Before performing syntax analysis the tokens are stored in
  // the self expanding vector sxv. Each token occupies 5 words
  // in this vector: [tok, val, lno, tab, index].
  // It sets tokenp to the subscript of tokv where this item
  // in tokev.
  
  // tok     is the token
  // val     is its associated value, such as the value of a number
  //         or the tag of a section bracket.
  // lno     is the packed file number and line number of the token.

  // tab and index are used when trying to find a minimum cost
  //         syntax correction.
  
  // tab     A table of possible replacements/insertions of
  //         the form: [tok, val]. tab!0 is the upb of tab.

  // index   =-2  The token was in the program but was deleted
  //              possibly because of a bracketing error.
  //         =-1  Leave the original token unchanged.
  //         = 0  This token is deleted.
  //         = 1  Use tab!1 and tab!2 as a replacement of tok and val.
  //         = 2  Use tab!1 and tab!2 as an insertion before tok and val.
  //         = 3  Use tab!3 and tab!4 as a replacement of tok and val.
  //         = 4  Use tab!3 and tab!4 as an insertion before tok and val.
  //         etc
  
  tokenp := sxpushval(tokensxv, tok) // The original program token
  sxpushval(tokensxv, val)           // Its associated value
  sxpushval(tokensxv, lineno)        // The file/line number
  sxpushval(tokensxv, 0)             // Table of suggested modifications
  sxpushval(tokensxv, -1)            // No change specified yet

  // tokenp and tokenpmax must be set appropriately since they may be used.
  tokenp := tokv + tokv!0 // position of the next item in tokv.
  tokenpmax := tokenp            // position after the last item in tokv.
//  writef("pushtok: %i4: %s tokenp=%n tokenpmax=%n*n",
//          lineno&#xFFFFF, opname(tok), tokenp, tokenpmax)
}

AND rdtokens() BE
{ // Read the tokens of the program into the self
  // expanding vector tokensxv.
  LET fno = lineno>>20
  LET ln  = lineno & #xFFFFF
  LET filename = sourcenamev!fno
  LET ln = ?
  LET v1 = VEC 255
  LET v2 = VEC 255
  LET v3 = VEC 255

  brackettokv  := v1  
  bracketvalv  := v2  
  bracketposv  := v3  
  bracketdepth := 0

//writef("rdtokens: Initial lineno=%x8*n", lineno)

  nametablesize := 541

  charv := newvec(256/bytesperword+1)
  charv%0 := 0
  nametable  := newvec(nametablesize) 
  FOR i = 0 TO nametablesize DO nametable!i := 0
  skiptag := 0
  declsyswords()

  token, decval := 0, 0

  // The lex environment is now fully initialised.

  UNTIL token = s_eof DO
  { // Start of REPEAT loop
    // Within this loop
    //   LOOP   jumps here, used when ignoring white space
    //   BREAK  causes a call of rch() followed by RETURN

//writef("rdtokens: Calling lex()*n")
    lex()

    fno, ln := lineno>>20, lineno&#xFFFFF

    //abort(1000)
//writef("rdtokens: fno=%n ln=%i3 token=%s tokenp=%n*n",
//        fno, ln, opname(token), tokenp)
//prbrackets()
//abort(3456)
    SWITCHON token INTO
    { DEFAULT: // Tokens for which tokval should be zero.
        //IF debug DO
        //  writef("fno=%n ln=%i3: %12t  *n",
        //          fno, ln, opname(token))
        pushtok(token, 0)
        LOOP

      CASE s_name:
        //IF debug DO
        //  writef("fno=%n ln=%i3: %12t  %s*n", 
        //          fno, ln, opname(token), @h3!wordnode)
        pushtok(token, @h3!wordnode)
        LOOP  

      CASE s_number:
        //IF debug DO
        //  writef("fno=%n ln=%i3: %12t  %n*n", 
        //          fno, ln, opname(token), decval)
        pushtok(token, decval)
        LOOP  

      CASE s_fnum:
        //IF debug DO
        //  writef("fno=%n ln=%i3: %12t  %13.9e*n", 
        //          fno, ln, opname(token), fltval)
        pushtok(token, fltval)
        LOOP  

      CASE s_lsect:
        pushtok(token, sectiontag)
        bracketdepth := bracketdepth+1
        brackettokv!bracketdepth := token
        bracketvalv!bracketdepth := sectiontag
        bracketposv!bracketdepth := tokenp-t_size
        LOOP

      CASE s_rsect:
        // Check that the brackets match correctly.
	{ { // First deal with unclosed ( and [ brackets.
	    IF bracketdepth=0 BREAK
	    
	    IF brackettokv!bracketdepth=s_lparen DO
	    { lexerr("Inserting a missing ')' before a close section bracket")
	      pushtok(s_rparen, 0)
	      bracketdepth := bracketdepth-1
	      LOOP
	    }
	    IF brackettokv!bracketdepth=s_sbra DO
	    { lexerr("Inserting a missing ']' before a close section bracket")
	      pushtok(s_sket, 0)
	      bracketdepth := bracketdepth-1
	      LOOP
	    }
	    BREAK
	  } REPEAT

          // Either the bracket depth is zero or s_lsect is on top
	  // of the bracket stack.
	  
//writef("CASE s_rsect: bracketdepth=%n*n", bracketdepth)
	  IF bracketdepth=0 DO
	  { lexerr("Deleting an unexpected close section bracket")
	    // Mark this bracket as deleted.
	    pushtok(token, sectiontag)
	    tokv!(tokenp-t_size+t_index) := -2 // The deletion mark.
	    ENDCASE
	  }

          // The top item in the bracket stack is s_lsect.
//writef("CASE s_rsect: lno=%n  bracketdepth=%n current open bracket is ",
//       lineno&#xFFFFF, bracketdepth)
//TEST bracketvalv!bracketdepth
//THEN { writef("$("); prtag(bracketvalv!bracketdepth) }
//ELSE { writef("{") }
//newline()
//writef("This bracket is ")
//TEST sectiontag
//THEN { writef("$)"); prtag(sectiontag) }
//ELSE { writef("}") }
//newline()
//abort(6789)

          IF sectiontag=0 DO
	  { // The current bracket is }
	    LET opentag = bracketvalv!bracketdepth
	    IF opentag=0 DO
	    { // The open section bracket is {
	      pushtok(s_rsect, 0)
	      bracketdepth := bracketdepth-1
	      ENDCASE
	    }
	    // The open section bracket is tagged
	    writef("Error near line "); prlineno(lineno)
	    writef(": Replacing } with $)")
	    prtag(opentag)
	    newline()
            pushtok(s_rsect, opentag)
            bracketdepth := bracketdepth-1
	    errcount := errcount+1
	    ENDCASE
	  }
	  
          IF sectiontag & sectiontag~=nulltag DO
	  { // The current token is a tagged closing section bracket.
	    // First check whether a matching open bracket exits.
            LET opentag = bracketvalv!bracketdepth
	    LET ok = VALOF { FOR i = 1 TO bracketdepth DO
	                     { IF brackettokv!i=s_lsect &
	                       bracketvalv!i=sectiontag RESULTIS TRUE
	                     }
	                     RESULTIS FALSE
	                   }
	    IF ok DO
	    { // There is a matching open section bracket in
	      // the bracket stack.
	      IF opentag=sectiontag DO
	      { // The current open section bracket matches this one,
	        // so remove it from the bracket stack.
                pushtok(s_rsect, opentag)
		bracketdepth := bracketdepth-1
		ENDCASE
              }
	      // The current open section bracket does not math this
	      // one. so insert an appropriate closing bracket.
              pushtok(s_rsect, opentag)
	      bracketdepth := bracketdepth-1
	      IF debug DO
              { writef("Near line "); prlineno(lineno)
                writef(": Inserting "); prtoken(s_rsect, opentag)
	        writef(" before "); prtoken(s_rsect, sectiontag)
                newline()
	      }
              LOOP	      
	    }

	    // There is no suitable open bracket, so make this
	    // close section bracket match the current open bracket.
	    pushtok(s_rsect, opentag)
	    bracketdepth := bracketdepth-1
            writef("Error near line "); prlineno(lineno)
            writef(": Replacing "); prtoken(s_rsect, sectiontag)
            writef(" with "); prtoken(s_rsect, opentag)
            newline()
	    incerrcount()
	    ENDCASE
	  }
        } REPEAT

        // This point cannot be reached.
        ENDCASE
 
      CASE s_lparen:
        pushtok(token, 0)
        bracketdepth := bracketdepth+1
        brackettokv!bracketdepth := token
        bracketvalv!bracketdepth := 0
        bracketposv!bracketdepth := tokenp
        ENDCASE

      CASE s_rparen:
//      writef("CASE s_rparen: bracketdepth=%n current open bracket is %s*n",
//            bracketdepth, opname(brackettokv!bracketdepth))
        UNLESS bracketdepth & brackettokv!bracketdepth=s_lparen DO
	  lexerr("Mismatched ')'")
        pushtok(token, 0)
        bracketdepth := bracketdepth-1
        ENDCASE
 
      CASE s_sbra:
        pushtok(token, 0)
        bracketdepth := bracketdepth+1
        brackettokv!bracketdepth := token
        bracketvalv!bracketdepth := 0
        bracketposv!bracketdepth := tokenp
        ENDCASE

      CASE s_sket:
        UNLESS bracketdepth & bracketvalv!bracketdepth=s_lparen DO
	  lexerr("Mismatched ']'")
        pushtok(token, 0)
        bracketdepth := bracketdepth-1
        ENDCASE

      CASE s_global:    // Tokens that can start an outer level
      CASE s_manifest:  // declaration
      CASE s_static:
      CASE s_let:
      CASE s_and:
        pushtok(token, bracketdepth)
        // Note that tokval is zero if at the outermost level.
        ENDCASE
 
      CASE s_string:
      { LET s = @h2!wordnode
        //IF debug DO
        //{ writef("fno=%n ln=%i3: %12t *"", fno, ln, opname(token))
        //  FOR i = 1 TO s%0 DO
        //  { LET ch = s%i
        //    SWITCHON ch INTO
        //    { DEFAULT:     wrch(ch);    LOOP

        //      CASE '*n': writes("**n"); LOOP
        //      CASE '*s': writes("**s"); LOOP
        //      CASE '*p': writes("**p"); LOOP
        //      CASE '*t': writes("**t"); LOOP
        //    }
        //  }
        //  writes("*"*n")
        //}
        pushtok(token, s)
        ENDCASE
      }
    }

    // ENDCASE jumps here.
  } // End of the UNTIL loop.

  pushtok(s_eof, 0) // Thw last token item.
}

AND prtokens() BE
{ // Write out the vector of tokens.
  LET ln  = lineno & #xFFFFF
  AND fno = lineno>>20
  AND filename = sourcenamev!fno

  { // Output every token stored in tokv

    rdtok()         // Get the token, tokval and lineno from tokv

    ln  := lineno & #xFFFFF
    fno := lineno>>20

    //writef("tokenp=%n nlpending=%n token=%s tokval=%n  fno=%n ln=%n*n",
    //        tokenp, nlpending, opname(token), tokval, fno, ln)
//abort(9233)
    IF nlpending DO newline()

//abort(8901)
    SWITCHON token INTO
    { DEFAULT:
        writef("%i4: %12t*n", ln, opname(token))
        IF token=s_eof RETURN
        //abort(1000)
        ENDCASE


      CASE s_name:
        IF tokval=0 DO tokval:="dummy"
        writef("%i4: %12t  %s*n", ln, opname(token), tokval)
        ENDCASE  

      CASE s_number:
        writef("%i4: %12t  %n*n", ln, opname(token), tokval)
        ENDCASE  

      CASE s_fnum:
        writef("%i4: %12t %13.9e *n", ln, opname(token), tokval)
        ENDCASE  

      CASE s_lsect:
      CASE s_rsect:
        writef("%i4: %12t  ", ln, opname(token))
        TEST tokval=0
        THEN { writef("0")  // Untagged section bracket
             }
        ELSE { writef("*"") // Tagged section bracket
               prtag(tokval)
               writef("*"")
             }
        newline()
        ENDCASE  

      CASE s_string:
      { IF tokval=0 DO tokval := "dummy"
        writef("%i4: %12t *"", ln, opname(token))
        FOR i = 1 TO tokval%0 DO
        { LET ch = tokval%i
          SWITCHON ch INTO
          { DEFAULT:   wrch(ch);      LOOP

            CASE '*n': writes("**n"); LOOP
            CASE '*s': writes("**s"); LOOP
            CASE '*p': writes("**p"); LOOP
            CASE '*t': writes("**t"); LOOP
          }
        }
        writes("*"*n")
        ENDCASE
      }

      CASE s_global:
      CASE s_manifest:
      CASE s_static:
      CASE s_let:
      CASE s_and:
        writef("%i4: %12t  %n*n", ln, opname(token), tokval)
        ENDCASE
    }
  } REPEAT
}

AND prlineno(lno) BE
{ LET fno = lineno>>20
  LET ln = lineno & #xFFFFF
  LET filename = sourcenamev!fno
  IF filename=0 DO filename := "dummy"
  writef("%s[%n]", filename, ln)
}

AND lexerr(mess, a, b, c) BE
{ writef("*nError near ")
  prlineno(lineno)
  writef(":  ")
  writef(mess, a, b, c)
  wrchbuf()
  incerrcount()
  nlpending := FALSE
  //abort(998)
  // Just return from lexerr.
}
 
AND synerr(mess, a, b, c) BE
{ // Before calling synerr we must ensure tokenp is valid.
  writef("*nError near ")
  prlineno(t_lno!tokenp)
  writef(":  ")
  writef(mess, a, b, c)
  newline()
  prlocation(tokenp) // Points to the token item that caused the error.
  incerrcount()
}

AND incerrcount() BE
{ IF hard DO abort(1000)
  errcount := errcount + 1
  IF errcount > errmax DO
  { writes("*nCompilation aborted*n")
    longjump(fin_p, fin_l)
  }

  nlpending := FALSE
  //longjump(rec_p, rec_l)
}
 
AND prlocation(loc) BE IF loc > 0 DO
{ // loc is the subscript of tokv of a token item.
  // Output 3 lines of tokens up to this token.

  LET q = @tokv!loc     // The pointer to the token item.
  LET p = q
  LET ln = (t_lno!q & #xFFFFF) - 3

  // Find the token at the start of the line 3 lines earlier.
  { p := p-t_size
    IF p<tokv DO
    { p := tokv+1 // The start of the program
      BREAK
    }
    IF (t_lno!p & #xFFFFF) < ln DO
    { p := p+t_size
      BREAK
    }
  } REPEAT

  writef("Recent tokens near the error:*n")
  ln := t_lno!p & #xFFFFF
  // Output the tokens from p to q
  WHILE p<=q DO
  { LET lnp = t_lno!p & #xFFFFF
    UNLESS ln=lnp DO { newline(); ln:= lnp }
    prtoken(t_tok!p, t_val!p)
    p := p+t_size
  }
  newline()
}

AND paterr(p, mess, a, b, c) BE
{ LET lno = t_lno!p
  LET fno = lno>>20
  LET ln = lno & #xFFFFF
  LET filename = sourcenamev!fno
  errcount := errcount + 1
  writef("*nError near ")
  IF filename DO writef("%s", filename)
  writef("[%n]:  ", ln)
  writef(mess, a, b, c)
  prlocation(p)
  IF hard DO abort(1000)
  IF errcount > errmax DO
  { writes("*nCompilation aborted*n")
    longjump(fin_p, fin_l)
  }
}

// Functions to parse the token sequence
 

// By convention the rd functions expect token to hold the
// first token of the construct being read.

AND rdprog() =  VALOF
{ rec_p, rec_l := level(), restart

again:
//writef("*nCalling rdsections*n")
//abort(1987)
  rdsections()
  UNLESS token=s_eof DO synerr("Incorrect termination")
  RETURN

restart:
  bracketdepth := 0

  writef("*nSkipping to the next outer level declaration, if any.*n")

  { IF token=s_eof RETURN
    IF token=s_section | token=s_needs GOTO again
    IF token=s_and DO token:=s_let
    IF tokval=0 &
       ( token=s_global | token=s_manifest | token=s_static |
         token=s_let | token=s_and ) GOTO again
    rdtok()
  } REPEAT
}

AND rdsections() BE
{ // Read the next section of code
//writef("rdsections: token=%s*n", opname(token))
  IF token=s_section DO
  { rdtok()
    UNLESS token=s_string DO
      synerr("String expected after SECTION")
    rdtok()
  }

  WHILE token=s_needs DO
  { rdtok()
    UNLESS token=s_string DO 
      synerr("String expected after NEEDS")
    rdtok()
  }

  { rddecl(1) //  Read an outer level declaration
    IF token=s_semicolon DO rdtok()
  } REPEATWHILE token=s_global |
                token=s_manifest |
                token=s_static |
                token=s_let

  IF token=s_eof RETURN

  TEST token=s_dot
  THEN rdtok()
  ELSE synerr("*nGLOBAL, MANIFEST, STATIC, LET, Dot or EOF expected*n")
} REPEAT
 
AND rddecl(n) BE
{ // n=1   Read an outer level declaration
  // n=0   Read a non outer level decllaration
  LET op = token

//writef("rddecl(%n): token=%s*n", n, opname(token))
//abort(5999)
  SWITCHON op INTO
  { DEFAULT: synerr("A declaration must start with the word*n*
                    *GLOBAL, MANIFEST, STATIC or LET")

    CASE s_global:
    CASE s_manifest:
    CASE s_static:
//abort(5998)
      rdtok()
//abort(5997)
      UNLESS token=s_lsect DO
        synerr("{ must follow GLOBAL, MANIFEST or STATIC")
//abort(5996)
      rdtok()
      WHILE token=s_name | token=s_flt DO
      { IF token=s_flt DO
        { rdtok()
          UNLESS token=s_name DO synerr("Name expected after FLT")
        }
        rdtok()

        IF token=s_colon | token=s_eq DO
        { TEST op=s_global
          THEN UNLESS token=s_colon DO
                 synerr("Colon expected in a GLOBAL definition")
          ELSE UNLESS token=s_eq DO
                 synerr("Equal sign expected in a %s definition", opname(op))
          rnexp(0)
        }

        IF token=s_semicolon DO rdtok()
      }

      UNLESS token=s_rsect DO
        synerr("*nSomething wrong with the definitions in a %s declaration",
               opname(op))
      rdtok()
      RETURN
    
    CASE s_let:
    { // Read a sequence declarations separated by ANDs
      // LOOP will reach here if token is s_and

      rdtok() // Skip over LET or AND

      // At this point token must be s_name or
      // if n=0 it could be FLT
      IF token=s_name DO
      { rdtok()
        // At this point token must be s_lparen or s_colon
        // or if n=0 it can also be s_eq or s_comma
        SWITCHON token INTO
        { DEFAULT:
            synerr("Error after a name in a LET or AND declaration")

          CASE s_lparen:
            rdtok()

            UNLESS token=s_rparen DO
            { { IF token=s_flt DO rdtok()
                UNLESS token=s_name DO
                  synerr("Name expected in a parameter list")
                rdtok() // Skip over the name
                UNLESS token=s_comma BREAK
                rdtok()  // Skip over the comma
              } REPEAT
            }

            UNLESS token=s_rparen DO
              synerr("*nName, FLT, ',' or ')' expected in a parameter list")

            rdtok() //Skip over the rparen

            IF token=s_eq DO { rnexp(0); LOOP }
            IF token=s_be DO { rncom();  LOOP }

            synerr("*n= or BE expected after the parameter list in a*n*
		   *function or routine definition")

          CASE s_colon:
            rdmatchlist(0)
            LOOP

          CASE s_eq:
            IF n>0 DO synerr("Bad outer level LET or AND declaration")
            rdtok()
            IF token=s_vec DO rdtok()
            rexp(0)
            LOOP

          CASE s_comma:
            IF n>0 DO synerr("Bad outer level LET or AND declaration")
          GOTO localdef
        }
      }

      // At this point token follows LET or AND but is not a name,
      // the only other allowable token is FLT not occurring in an
      // outer level declaration.

      UNLESS n=0 & token=s_flt DO
        synerr("LET and AND must be followed by a name or FLT")
      rdtok() // Skip over FLT
      UNLESS token=s_name DO
        synerr("FLT must be followed by a name")
      rdtok() //Skip over the name
      IF token=s_eq DO
      { rnexp(0)
        LOOP
      }
      // At this point the only allowable token is COMMA
      UNLESS token=s_comma DO
        synerr("*nUnexpected token following a name*n*
	       *in a LET or AND declaration")

localdef:
      // At this point token is the first comma of a
      //  simultaneous declaration.

      // Read the rest of the list of local variable names
      { rdtok()
        IF token=s_flt DO rdtok()
        UNLESS token=s_name DO
          synerr("Bad local variable declaration")
        rdtok()
      } REPEATWHILE token=s_comma

      // Check for the = defining operator
      UNLESS token=s_eq DO
        synerr("Bad local variable declaration")

      // Read the list of right hand side expressions
      rnexp(0) REPEATWHILE token=s_comma
      LOOP
    } REPEATWHILE token=s_and

    // End of sequence of declarations separated by AND
    // following LET
    RETURN
  } // End of SWITCHON for cases GLOBAL, MANIFEST, STATIC and LET
}

AND rdmatchlist(sort) = VALOF
{ // sort is either s_yields or s_be or zero if not yet known.

  // Returns s_yields or s_be
  
  UNLESS token=s_colon DO
    synerr("A match list must start with a colon")
//writef("rdmatchlist: calling rdmatchitem*n")
  sort := rdmatchitem(sort) // Read the first match item

  WHILE token=s_colon DO rdmatchitem(sort)

  IF token=s_dot DO rdtok()

  RESULTIS sort
}

AND rdmatchitem(sort) = VALOF
{ // sort is either s_yields or s_be or zero if not yet known.
  // It returns the actal sort of this match item.

  UNLESS token = s_colon DO
    synerr("A match item must start with a ':'")
  //writef("rdmatchitem:token=%s*n", opname(token))
  rdtok() // Skip over the colon
//writef("rdmatchitem: calling rpat(0), token = %s*n", opname(token))  

  // Allow an empty pattern list between ':' and '=>' or BE.
  UNLESS token=s_yields | token=s_be DO
  { // There must be a pattern if token is not => or BE 
//writef("token was not => or BE so calling rpat(0)*n")
    UNLESS rpat(0) DO
      synerr("The is a problem wit the pattern following a colon")
    UNLESS token=s_yields | token=s_be DO
      synerr("The pattern was no followed by => or BE")
  }
  UNLESS sort DO sort := token

  // Check that then defining operator in all match item are the same.
  UNLESS sort=token
    TEST sort=s_yields
    THEN paterr("*nThe defining operator in this match item should be '=>'")
    ELSE paterr("*nThe defining operator in this match item should be 'BE'")

  TEST sort=s_yields
  THEN UNLESS rnexp(0) DO
         synerr("There is a problem with the expression following =>")
  ELSE UNLESS rncom() DO
         synerr("There is a problem with the expression following =>")

  RESULTIS sort
}

// ######################## Patterns ############################


AND rnbpat() BE
{ rdtok()
  rbpat()
}

AND rbpat() = VALOF
{ // Attempt to read a basic pattern,
  // ie a possibly signed integer or floating point constant.
  // a character constant, TRUE, FALSE, BITSPERBCPLWORD, ?,
  // or a name not preceeded by FLT.
  // Return TRUE if successful.

  SWITCHON token INTO
  { DEFAULT:
      RESULTIS FALSE

    CASE s_number:
    CASE s_fnum:
    CASE s_true:
    CASE s_false:
    CASE s_query:
    CASE s_bitsperbcplword:
    CASE s_name:
      rdtok()
      RESULTIS TRUE

    CASE s_add: CASE s_fadd:
    CASE s_sub: CASE s_fsub:
    CASE s_abs: CASE s_fabs:
      rdtok()
      UNLESS token=s_number | token=s_fnum DO
         synerr("A number must follow a monadic sign operator in a pattern")
      rdtok()        //Skip over thenumber
      RESULTIS TRUE
  }
}

AND rspat() = VALOF
{ // Attempt to read a simple pattern, ie one that does not
  // include comma. vertical bar or juxtaposition at the
  // outermost level.
  // Return TRUE if successful

  // It returns FALSE if token cannot start a pattern.

  SWITCHON token INTO
  { DEFAULT:
//writef("rspat: calling rbpat()*n")
      UNLESS rbpat() RESULTIS FALSE
      IF token=s_range | token=s_frange DO
        UNLESS rnbpat() DO
          synerr("Problem with the right hand operand of a range")
      RESULTIS TRUE

    // All the relop tokens
    CASE s_eq:    CASE s_feq:
    CASE s_ne:    CASE s_fne:
    CASE s_le:    CASE s_fle:
    CASE s_ge:    CASE s_fge:
    CASE s_ls:    CASE s_fls:
    CASE s_gr:    CASE s_fgr:
      rdtok()
      IF token=s_lparen DO
      { LET ok = rnexp(0)                      // relop ( E )
        UNLESS ok & token=s_rparen DO
          synerr("*n   There is a problem with the expression enclosed*n*
	           *   in parentheses after a relational operator.")
        rdtok() // Skip over the close parenthesis
        RESULTIS TRUE
      }
      UNLESS rnbpat() DO
        paterr("The is a problem after a relational operator in a pattern")
      RESULTIS TRUE

    // All the tokens of jcom
    CASE s_break:
    CASE s_loop:
    CASE s_endcase:
    CASE s_next:
    CASE s_exit:
      rdtok()
      RESULTIS TRUE

    CASE s_lparen:
    { LET ok = rnpat(0)
      UNLESS ok & token=s_rparen
        synerr("*nThere is a problem with a pattern enclosed *
	       *in parentheses")
      rdtok() // Skip over the close parenthesis
      RESULTIS TRUE
    }

    CASE s_sbra:
    { LET ok = rnpat(0)
      UNLESS ok & token=s_sket DO
        synerr("*nThere is a problem with a pattern enclosed *
	       *in square brackets")
      rdtok() // Skip over the close square bracket
      RESULTIS TRUE
    }

    CASE s_flt:
      rdtok()
      UNLESS token=s_name DO
        synerr("FLT must be followed by a name")
      rdtok()
      RESULTIS TRUE
  }
}

AND rnpat(n) BE
{ rdtok()
  rpat(n)
}

AND rpat(n) = VALOF
{ //writef("rpat(%n): calling rspat(0), token=%s*n", n, opname(token))
  UNLESS rspat(0) RESULTIS FALSE

  { // Start of loop to read consecutive patterns.
    SWITCHON token INTO
    { DEFAULT:
        UNLESS rspat() RESULTIS TRUE
        LOOP

      CASE s_comma:
        IF n<1 DO
        { UNLESS rnpat(1) DO
            paterr("Something wrong with the pattern following a comma") 
          LOOP
        }
        RESULTIS TRUE
        
      CASE s_logor:
        IF n<2 DO
        { UNLESS rnpat(2) DO
            paterr("Something wrong with the pattern following a comma") 
          LOOP
        }
        RESULTIS TRUE
    }
  } REPEAT
}

AND rnbcom() BE
{ rdtok()
  rbcom()
}

AND rbcom() BE
{ LET op = token
 
  SWITCHON op INTO
  { DEFAULT:
      synerr("Token %s cannot start a command",
             opname(op))

    CASE s_break:
    CASE s_loop:
    CASE s_next:
    CASE s_exit:
    CASE s_endcase:
    CASE s_return:
    CASE s_finish:
    CASE s_skip:
      rdtok()
      RETURN
 
    CASE s_resultis:
    CASE s_goto:
      rnexp(0)
      RETURN
 
    CASE s_test:
      rnexp(0)
      IF token=s_do DO rdtok()
      rcom()
      UNLESS token=s_else DO synerr("ELSE missing in a TEST command")
      rncom()
      RETURN
 
    CASE s_for:
      rdtok()
      UNLESS token=s_name DO synerr("Name expectted after FOR")
      rdtok()
      UNLESS token=s_eq DO synerr("'=' missing in FOR loop")
      rnexp(0)
      IF token=s_to DO rnexp(0)
      IF token=s_by DO rnexp(0)
      IF token=s_do DO rdtok()
      rcom()
      RETURN
 
    CASE s_if:
    CASE s_unless:
    CASE s_while:
    CASE s_until:
      rnexp(0)
      IF token=s_do DO rdtok()
      rcom()
      RETURN
 
    CASE s_lsect:
      rdtok()
      IF token=s_rsect DO { rdtok(); RETURN }
      WHILE token=s_global   |
            token=s_manifest |
            token=s_static   |
            token=s_let      DO
      { rddecl(0) // Read a non outer level declaration
        IF token=s_rsect DO { rdtok(); RETURN }
        IF token=s_semicolon DO rdtok()
      }

      { rcom()
        IF token=s_rsect DO { rdtok(); RETURN }
        IF token=s_semicolon DO rdtok()
      } REPEAT
 
    CASE s_switchon:
      rnexp(0)
      UNLESS token=s_into DO synerr("INTO missing")
      rdtok()
      UNLESS token=s_lsect DO
        synerr("{ missing after INTO in a SWITCHON command")
      rdtok()

      IF token=s_rsect DO { rdtok(); RETURN }

      { rcom()
        IF token=s_rsect DO { rdtok(); RETURN }
        IF token=s_semicolon DO rdtok()
      } REPEAT
 
    CASE s_match:
    CASE s_every:
    { rdtok()
      UNLESS token=s_lparen DO
         synerr("'(' expected after MATCH or EVERY")
      rdtok()
      UNLESS token=s_rparen DO
      { rexp(0)
        UNLESS token=s_comma BREAK
        rdtok()
      } REPEAT
      UNLESS token=s_rparen DO
        synerr("Bad MATCH or EVERY argument list")
      rdtok()  
      rdmatchlist(s_be)
      RETURN
    }

    // All tokens that can start an expression 
    CASE s_name:CASE s_number:CASE s_fnum:
    CASE s_string:CASE s_lparen:
    CASE s_true:CASE s_false:
    CASE s_lv:          // @  or LV
    CASE s_rv:          // RV
    CASE s_vecap:       // !
    CASE s_slct:        // Inserted 11/7/01
    CASE s_add:CASE s_sub:CASE s_abs:CASE s_not:
    CASE s_fadd:CASE s_fsub:CASE s_fabs:CASE s_fix:CASE s_float:
    CASE s_table:CASE s_valof:CASE s_query:
      // All these tokens that can start an expression,
      // but not allowing MATCH or EVERY which will be commands
      // not expressions.

      eclass := 0 // Not a call or a name

      rexp(0)
 
      IF token=s_comma DO
      { // It must be a simultaneous assignment
        rnexp(0) REPEATWHILE token=s_comma
        UNLESS isassop(token) DO
          synerr("Bad simultaneous command")
        rnexp(0) REPEATWHILE token=s_comma
        RETURN
      }

      IF isassop(token) DO
      { // It was a simple assigment
        rnexp(0)
        RETURN
      }
  
      IF eclass=2 DO
      { //writef("A routine call*n")
        RETURN // It was a routine call
      }

      IF eclass=1 DO
      { //writef("A label declartion*n")
        // The latest call of rexp(0) found a name
        UNLESS token=s_colon DO
          synerr("*nA name cannot be followed by %s in a command",
                 opname(token))
        rdtok()
        IF iscomstart(token) DO rcom()
        RETURN
      }

      writef("eclass=%n*n", eclass)
      synerr("*nA command must start with a keyword, or be a*n*
             *a routine call, an assignment or a label such as L:")

    CASE s_case:
      rnexp(0)
      UNLESS token=s_colon DO
        synerr("':' missing in a CASE label")
      rdtok() // Skip over ':'
      IF iscomstart(token) DO rcom()
      RETURN

    CASE s_default:
      rdtok()
      UNLESS token=s_colon DO 
        synerr("':' missing after DEFAULT")
      rdtok() // Skip over ':'
      IF iscomstart(token) DO rcom()
      RETURN
  }
}

AND isassop(tok) = VALOF SWITCHON tok INTO
{ DEFAULT:
    RESULTIS FALSE

  CASE s_ass:      CASE s_fass:
  CASE s_assvecap:
  CASE s_assfmul:  CASE s_assfdiv: CASE s_assfmod:
  CASE s_assfadd:  CASE s_assfsub:
  CASE s_assmul:   CASE s_assdiv:  CASE s_assmod:
  CASE s_assadd:   CASE s_asssub:
  CASE s_asslshift:CASE s_assrshift:
  CASE s_asslogand:CASE s_asslogor:
  CASE s_asseqv:   CASE s_assxor:
    RESULTIS TRUE
}

AND iscomstart(tok) = VALOF SWITCHON tok INTO
{ DEFAULT:
    RESULTIS FALSE

  CASE s_break:
  CASE s_loop:
  CASE s_next:
  CASE s_exit:
  CASE s_endcase:
  CASE s_return:
  CASE s_finish:
  CASE s_skip:
  CASE s_resultis:
  CASE s_goto:
  CASE s_test:
  CASE s_for:
  CASE s_if:
  CASE s_unless:
  CASE s_while:
  CASE s_until:
  CASE s_lsect:
  CASE s_switchon:
  CASE s_match:
  CASE s_every:

  // All tokens that can start an expression 
  CASE s_name:CASE s_number:CASE s_fnum:
  CASE s_string:CASE s_lparen:
  CASE s_true:CASE s_false:CASE s_lv:CASE s_rv:CASE s_vecap:
  CASE s_slct:        // Inserted 11/7/01
  CASE s_add:CASE s_sub:CASE s_abs:CASE s_not:
  CASE s_fadd:CASE s_fsub:CASE s_fabs:CASE s_fix:CASE s_float:
  CASE s_table:CASE s_valof:CASE s_query:
  CASE s_case:
  CASE s_default:
    RESULTIS TRUE
}

AND rncom() BE
{ rdtok()
  rcom()
}

AND rcom() BE
// Reads:  BCOM <> BCOM <>...<> BCOM 
// possibly qualified by repeat, repeatwhile or repeatuntil clauses
{ rbcom()
 
  WHILE token=s_seq DO rnbcom() // Deal with <>

  WHILE token=s_repeat | token=s_repeatwhile | token=s_repeatuntil DO
  { TEST token=s_repeat THEN rdtok()
                        ELSE rnexp(0)
  }
}

AND rnbexp() BE
{ rdtok()
  rbexp()
}

AND rbexp() BE
{ // Read a basic expression, setting
  // eclass=1  if it was a name
  // eclass-0  otherwise

  LET op = token

//abort(3999)

  SWITCHON op INTO
 
  { DEFAULT:
      writef("*nError near "); prlineno(lineno)
      writef(":  Expression expected but ")
      prtoken(token, tokval)
      writef("cannot start an expression*n")
      prlocation(tokenp) // Points to the token item that caused the error.
      incerrcount()

      eclass := 0
      RETURN

    CASE s_name:
      rdtok()
      eclass := 1 // Only time eclass set to 1
      RETURN

    CASE s_number:
    CASE s_fnum:
    CASE s_true:
    CASE s_false:
    CASE s_query:
    CASE s_string:
      rdtok()
      eclass := 0
      RETURN

    CASE s_slct:    // SLCT E9 { : E9 { : E9 } }
      rnexp(0)
      IF token=s_colon DO rnexp(0)
      IF token=s_colon DO rnexp(0)
      eclass := 0
      RETURN
 
    CASE s_break:   // BREAK LOOP ENDCASE NEXT EXIT RETURN
    CASE s_loop:
    CASE s_endcase:
    CASE s_next:
    CASE s_exit:
    CASE s_return:
      rdtok()
      eclass := 0
      RETURN

    CASE s_lparen:   // ( E0 )  or  ( )
      rnexp(0)
      UNLESS token=s_rparen DO synerr("')' missing")
      rdtok()
      eclass := 0
      RETURN
 
    CASE s_vecap:    // ! E7
    CASE s_float:    // FLOAT E7
    CASE s_fix:      // FIX E7
    CASE s_lv:       // @ E7
    CASE s_rv:       // RV E7
      rnexp(8)
      eclass := 0
      RETURN
 
    CASE s_fadd:
    CASE s_add:
    CASE s_sub:
    CASE s_fsub:
    CASE s_fabs:
    CASE s_abs:
      rnexp(7)
      eclass := 0
      RETURN
 
    CASE s_not:
      rnexp(4)
      eclass := 0
      RETURN
 
    CASE s_table:
      rnexp(0) REPEATWHILE token=s_comma
      eclass := 0
      RETURN

    CASE s_match:
    CASE s_every:
      rdtok()
      UNLESS token=s_lparen DO synerr("'(' expected after MATCH or EVERY")
      UNLESS token=s_rparen DO   // Allow ( )
      { rnexp(0) REPEATWHILE token=s_comma
      }
      UNLESS token=s_rparen DO
        synerr("')' missing at the end of the MATCH argument list")
      rdtok() 
      rdmatchlist(s_yields)
      eclass := 0
      RETURN

    CASE s_valof:
      rncom()
      eclass := 0
      RETURN
  }
}

AND rnexp(n) BE
{
//writef("rnexp(%n): token=%s*n", n, opname(token))
//abort(2665)
  rdtok()
//abort(2666)
  rexp(n)
}

AND rexp(n) BE
{ LET p = 0
//writef("rexp(%n): token=%s nlpendin=%n*n", n, opname(token), nlpending)

  rbexp() // Read a bexp, setting eclass to
          // 1    if a name
          // 0    otherwise

//writef("rexp(%n): after calling rbexp token=%s nlpending=%n*n",
//        n, opname(token), nlpending)

//abort(1999)
  UNTIL nlpending DO 
  { LET op = token
 
    SWITCHON op INTO
    { DEFAULT:
        RETURN
 
      CASE s_lparen:     // A function call
        IF n>=9 RETURN
        rdtok()

        UNLESS token=s_rparen DO
        { rexp(0)
          UNLESS token=s_comma BREAK
          rdtok()
        } REPEAT

        UNLESS token=s_rparen DO
          synerr("')' missing in a function call")
        rdtok()
        eclass := 2
        LOOP
 
      CASE s_mthap:   // #(    Method application
        IF n>=9 RETURN
        rdtok()
        IF token=s_rparen DO
          synerr("*nA method call must have at least one argument")

        { rexp(0)
	  UNLESS token=s_comma BREAK
	  rdtok()
	} REPEAT

        UNLESS token=s_rparen DO
          synerr("Problem with method call argument list")
        rdtok()
        eclass := 2
        LOOP
  
      CASE s_sbra:
        IF n>=10 RETURN
        rnexp(0)
        UNLESS token=s_sket DO synerr("']' missing")
        rdtok()
        eclass := 0
        LOOP
 
      CASE s_of:
      CASE s_vecap:
      CASE s_byteap: p := 9; ENDCASE

      CASE s_fmul:
      CASE s_fdiv:
      CASE s_fmod:
      CASE s_mul:
      CASE s_div:
      CASE s_mod:    p := 8; ENDCASE

      CASE s_fadd:
      CASE s_fsub:
      CASE s_add:
      CASE s_sub:    p := 7; ENDCASE
 
      CASE s_feq:CASE s_fle:CASE s_fls:
      CASE s_fne:CASE s_fge:CASE s_fgr:
      CASE s_eq: CASE s_le: CASE s_ls:
      CASE s_ne: CASE s_ge: CASE s_gr:
        IF n>=6 RETURN
        eclass := 0
        rnexp(6) REPEATWHILE s_eq<=token<=s_ge |
                              s_feq<=token<=s_fge
        LOOP
 
      CASE s_lshift:
      CASE s_rshift: p := 5; ENDCASE

      CASE s_logand: p := 5; ENDCASE

      CASE s_logor:  p := 3; ENDCASE

      CASE s_eqv:
      CASE s_xor:    p := 2; ENDCASE
 
      CASE s_fcond:
      CASE s_cond:
        IF n>=1 RETURN
        eclass := 0
        rnexp(0)
        UNLESS token=s_comma DO
          synerr("Bad conditional expression")
        rnexp(0)
        LOOP
    }
      
    IF n>=p RETURN
    // Left associative operator of precedence p
    rnexp(p)
    eclass := 0   // Not a name or a function call
  }
}


AND opname(op) = VALOF SWITCHON op INTO
{ DEFAULT:            writef("*nUnknown op = %n*n", op)
abort(999)
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
  CASE s_endcase:     RESULTIS "ENDCASE"
  CASE s_endfor:      RESULTIS "ENDFOR"
  CASE s_endproc:     RESULTIS "ENDPROC"
  CASE s_entry:       RESULTIS "ENTRY"
  CASE s_eof:         RESULTIS "EOF"
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
  CASE s_fpos:        RESULTIS "FPOS"
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
  CASE s_line:        RESULTIS "LINE"
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

  //CASE s_pos:         RESULTIS "POS"

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


AND prtoken(tok, val) BE SWITCHON tok INTO
{ DEFAULT:            writef("Op%n ", tok);   RETURN

  CASE s_name:        writef("%s ", val);     RETURN
  CASE s_string:      writef("*"%s*" ", val); RETURN
  CASE s_number:      writef("%n ", val);     RETURN
  CASE s_fnum:        writef("%6.3e ", val);  RETURN
  CASE s_lsect:       IF val DO
                      { LET s = val+2
		        writef("$(")
			FOR i = 2 TO s%0 DO wrch(s%i)
			wrch(' ')
			RETURN
                      }
                      writef("{ ");           RETURN
  CASE s_rsect:       IF val DO
                      { LET s = val+2
		        writef("$)")
			FOR i = 2 TO s%0 DO wrch(s%i)
			wrch(' ')
			RETURN
                      }
                      writef("} ");           RETURN

  CASE s_abs:         writef("ABS ");         RETURN
  CASE s_and:         writef("AND ");         RETURN
  CASE s_ass:         writef(":= ");          RETURN
  CASE s_assdiv:      writef("/:= ");         RETURN
  CASE s_asseqv:      writef("EQV:= ");       RETURN
  CASE s_assfdiv:     writef("#/:= ");        RETURN
  CASE s_assfmod:     writef("#MOD:= ");      RETURN
  CASE s_assfsub:     writef("#-:= ");        RETURN
  CASE s_assfmul:     writef("#**:= ");       RETURN
  CASE s_assfadd:     writef("#+:= ");        RETURN
  CASE s_asslogand:   writef("&:= ");         RETURN
  CASE s_asslogor:    writef("|:= ");         RETURN
  CASE s_asslshift:   writef("<<:= ");        RETURN
  CASE s_asssub:      writef("-:= ");         RETURN
  CASE s_assmul:      writef("**:= ");        RETURN
  CASE s_assxor:      writef("XOR:= ");       RETURN
  CASE s_assadd:      writef("+:= ");         RETURN
  CASE s_assmod:      writef("MOD:= ");       RETURN
  CASE s_assrshift:   writef(">>:= ");        RETURN
  CASE s_assvecap:    writef("!:= ");         RETURN
  CASE s_be:          writef("BE ");          RETURN
  CASE s_by:          writef("BY ");          RETURN
  CASE s_break:       writef("BREAK ");       RETURN
  CASE s_byteap:      writef("%% ");          RETURN
  CASE s_case:        writef("CASE ");        RETURN
  CASE s_colon:       writef(": ");           RETURN
  CASE s_comma:       writef(", ");           RETURN
  CASE s_cond:        writef("-> ");          RETURN
  CASE s_default:     writef("DEFAULT ");     RETURN
  CASE s_div:         writef("/ ");           RETURN
  CASE s_do:          writef("DO ");          RETURN
  CASE s_dot:         writef(". ");           RETURN
  CASE s_else:        writef("ELSE ");        RETURN
  CASE s_endcase:     writef("ENDCASE ");     RETURN
  CASE s_eof:         writef("<EOF> ");         RETURN
  CASE s_eq:          writef("= ");           RETURN
  CASE s_eqv:         writef("EQV ");         RETURN
  CASE s_every:       writef("EVERY ");       RETURN
  CASE s_exit:        writef("EXIT ");        RETURN
  CASE s_fabs:        writef("#ABS ");        RETURN
  CASE s_fadd:        writef("#+ ");          RETURN
  CASE s_false:       writef("FALSE ");       RETURN
  CASE s_fass:        writef("#:= ");         RETURN
  CASE s_fcond:       writef("#-> ");         RETURN
  CASE s_fdiv:        writef("#/ ");          RETURN
  CASE s_feq:         writef("#= ");          RETURN
  CASE s_fge:         writef("#>= ");         RETURN
  CASE s_fgr:         writef("#> ");          RETURN
  CASE s_finish:      writef("FINISH ");      RETURN
  CASE s_fix:         writef("FIX ");         RETURN
  CASE s_fle:         writef("#<= ");         RETURN
  CASE s_float:       writef("FLOAT ");       RETURN
  CASE s_flt:         writef("FLT ");         RETURN
  CASE s_fls:         writef("#< ");          RETURN
  CASE s_fmod:        writef("#MOD ");        RETURN
  CASE s_fne:         writef("#~= ");         RETURN
  CASE s_fneg:        writef("#- ");          RETURN
  CASE s_fmul:        writef("#** ");         RETURN
  CASE s_fpos:        writef("#+ ");          RETURN
  CASE s_frange:      writef("#.. ");         RETURN

  CASE s_fsub:        writef("#- ");          RETURN

  CASE s_for:         writef("FOR ");         RETURN
  CASE s_ge:          writef(">= ");          RETURN
  CASE s_get:         writef("GET ");         RETURN
  CASE s_getbyte:     writef("%% ");          RETURN
  CASE s_global:      writef("GLOBAL ");      RETURN
  CASE s_goto:        writef("GOTO ");        RETURN
  CASE s_gr:          writef("> ");           RETURN
  CASE s_if:          writef("IF ");          RETURN
  CASE s_into:        writef("INTO ");        RETURN
  CASE s_le:          writef("<= ");          RETURN
  CASE s_let:         writef("LET ");         RETURN
  CASE s_logand:      writef("& ");           RETURN
  CASE s_logor:       writef("| ");           RETURN
  CASE s_loop:        writef("LOOP ");        RETURN
  CASE s_lparen:      writef("( ");           RETURN
  CASE s_ls:          writef("< ");           RETURN
  CASE s_lshift:      writef("<< ");          RETURN
  CASE s_lv:          writef("@ ");           RETURN
  CASE s_manifest:    writef("MANIFEST ");    RETURN
  CASE s_match:       writef("MATCH ");       RETURN
  CASE s_mod:         writef("MOD ");         RETURN
  CASE s_mthap:       writef("# ");           RETURN
  CASE s_mul:         writef("** ");          RETURN
  CASE s_ne:          writef("~= ");          RETURN
  CASE s_needs:       writef("NEEDS ");       RETURN
  CASE s_neg:         writef("- ");           RETURN
  CASE s_next:        writef("NEXT ");        RETURN
  CASE s_not:         writef("~ ");           RETURN
  CASE s_of:          writef("OF ");          RETURN
  CASE s_add:         writef("+ ");           RETURN

  CASE s_pateq:       writef("= ");           RETURN
  CASE s_patne:       writef("~= ");          RETURN
  CASE s_patls:       writef("< ");           RETURN
  CASE s_patgr:       writef("> ");           RETURN
  CASE s_patle:       writef("<= ");          RETURN
  CASE s_patge:       writef(">= ");          RETURN

  CASE s_patfeq:      writef("#= ");          RETURN
  CASE s_patfne:      writef("#~= ");         RETURN
  CASE s_patfls:      writef("#< ");          RETURN
  CASE s_patfgr:      writef("#> ");          RETURN
  CASE s_patfle:      writef("#<= ");         RETURN
  CASE s_patfge:      writef("#>= ");         RETURN

  CASE s_pator:       writef("| ");           RETURN

  CASE s_putbyte:     writef("%% ");          RETURN
  CASE s_query:       writef("? ");           RETURN
  CASE s_range:       writef(".. ");          RETURN
  CASE s_repeat:      writef("REPEAT ");      RETURN
  CASE s_repeatuntil: writef("REPEATUNTIL "); RETURN
  CASE s_repeatwhile: writef("REPEATWHILE "); RETURN
  CASE s_resultis:    writef("RESULTIS ");    RETURN
  CASE s_return:      writef("RETURN ");      RETURN
  CASE s_rparen:      writef(") ");           RETURN
  CASE s_rshift:      writef(">> ");          RETURN
  CASE s_rv:          writef("! ");           RETURN
  CASE s_sbra:        writef("[ ");           RETURN
  CASE s_section:     writef("SECTION ");     RETURN
  CASE s_semicolon:   writef("; ");           RETURN
  CASE s_seq:         writef("<> ");          RETURN
  CASE s_sket:        writef("] ");           RETURN
  CASE s_skip:        writef("SKIP ");        RETURN
  CASE s_slct:        writef("SLCT ");        RETURN
  CASE s_static:      writef("STATIC ");      RETURN
  CASE s_sub:         writef("- ");           RETURN
  CASE s_switchon:    writef("SWITCHON ");    RETURN
  CASE s_table:       writef("TABLE ");       RETURN
  CASE s_test:        writef("TEST ");        RETURN
  CASE s_to:          writef("TO ");          RETURN
  CASE s_true:        writef("TRUE ");        RETURN
  CASE s_unless:      writef("UNLESS ");      RETURN
  CASE s_until:       writef("UNTIL ");       RETURN
  CASE s_valdef:      writef("VALDEF ");      RETURN
  CASE s_valof:       writef("VALOF ");       RETURN
  CASE s_vec:         writef("VEC ");         RETURN
  CASE s_vecap:       writef("! ");           RETURN
  CASE s_while:       writef("WHILE ");       RETURN
  CASE s_yields:      writef("=> ");          RETURN
  CASE s_xor:         writef("XOR ");         RETURN
}

AND prbrackets() BE
{ writef("*nCurrent open brackets: depth=%n ", bracketdepth)
  FOR i = 1 TO bracketdepth DO
  { writef("%n: tok=%n ", i, brackettokv!i)
    SWITCHON brackettokv!i INTO
    { DEFAULT:        writef(" ?");  ENDCASE
      CASE s_lparen:  writef(" (");  ENDCASE
      CASE s_sbra:    writef(" [");  ENDCASE
      CASE s_lsect: { LET tag = bracketvalv!i
                      TEST tag=0
                      THEN { writef(" {") }
                      ELSE { writef(" $("); prtag(tag) }
                      ENDCASE
                    }
    }
  }
  newline()
}

AND prtag(tag) BE
{ // tag is a name node representing a section bracket tag
  // preceeded by $
  LET s = tag+2
  FOR i = 2 TO s%0 DO wrch(s%i)
}

