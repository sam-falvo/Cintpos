/*
This is a program intended to read a .mus representation of a score
and write a corresponding MIDI file and/or play it on a MIDI device
possibly synchronising the accompanement with a solist using a
microphoe.

Implemented by Martin Richards (c) July 2019

Change history

05/07/2019
Disallowed ties in shape lists. Any shape in a block must start and
end with a value. A star is automatically insert at the start and
the end if necessary.

24/05/2019
Reworking the definition of the MUS language and making corresponding
changes to playmus. This version make considerable use of floating
point arithmetic particularly in the macrogenerator. Redesigns the
block mechanism and reimplements the treatments of shapes.

01/09/11
Play lines now have an origin such as (oer, oem) giving a point in the
real-time midi-time graph through which the play line passes and a
rate such as erate giving the slope in midi msecs per real second.
The estimated play line based on recent microphone and keyboard events
is represented by oer, oem and erate. The current play line is
represented by ocr, ocm and crate. Both are updated about 20 times per
second and the values of ocr, ocm and crate are chosen to cause the
current play line to approach the estimated play line reasonably
quickly.

25/04/11
Started implementation of new mechanism for shapes.

07/04/11
Started to implement note recognition from the microphone.

26/03/11
Started to implement keyboard commands in playmidi.

16/10/09
Changed the macrogenerator comment (%) to skip to the end of the line
and then ignore all white space characters until the next non white
space character (which may of course be a %).

08/04/09
Changed the midi data structure to be a linked list and used
mergesort to sort it.

30/03/09
Changed the macrogenerator quotes from { and } to < and > to allow
curly brackets { and } to enclose blocks in .mus files. These will
save and restore certain settings such as volume and tempo during
translation.

20/02/09
Added \pedon, \pedoff, \pedoffon, \softon, \softoff, \portaon and
\portaoff.

18/02/09
Added the :s construct into shapes. \tempo(:s4. 120) means 120 dotted
quarter notes per minute which corresponds to 180 crochets per minute.

15/10/08
Began replacing calls of scan functions by suitable calls of walktree.

25/09/08
Put in the MIDI/K option to write midi files, and the PLAY/S option to
play .mus files directly.

03/06/08
Started the implementation of playmus.b
*/

SECTION "Playmus"

GET "libhdr"
GET "playmus.h"

LET die() BE 
{ // This function causes the current coroutine to be deleted
  // by telling killco to delete this coroutine.
  // The main function of killco is just deleteco.
  // This is only used in notecofn to kill itself.
  writef("die: calling resumeco(killco, %n)*n", currco)
  resumeco(killco, currco)
}

LET concatext(str, ext, name) = VALOF
{ // Copy str into name and appending the extension ext if
  // there are no dots in str
  LET strlen = str%0
  LET len = 0
  LET dots = FALSE

  FOR i = 1 TO str%0 DO
  { LET ch = str%i
    IF ch='.' DO dots := TRUE
    len := len+1
    name%len := ch
  }

  UNLESS dots DO
  { // Append .mus
    FOR i = 1 TO ext%0 DO
    { LET ch = ext%i
      len := len+1
      name%len := ch
    }
  }

  name%0 := len
  //writef("concatext: %s %s => %s*n", str, ext, name)
  RESULTIS name
}

LET start() = VALOF
{ LET argv = VEC 50
  LET filenameroot = "t0"    // The filename without any extension.
  LET fromfilename = VEC 50  // For the name with the .mus extension.
  LET midifilename = VEC 10  // For the filename with the .mid extension.
  LET toname = 0
  LET genmidi = FALSE
  LET play = FALSE
  LET b   = VEC 64
  LET bln = VEC 64
  AND s1  = VEC 10
  AND dbv = VEC 9
  debugv := dbv

  barenv,  barmsecs  := 0, 0 // For safety
  beatenv, beatmsecs := 0, 0 // For safety
  msecsenv           := 0    // For safety

  sysin := input()
  sysout := output()
  sourcestream := 0
  tostream := 0

  veclist := 0
  killco := 0
  soundv, soundp := 0, 0
  bgpmco := 0
  bg_base := 0
  blklist := 0

  UNLESS rdargs("FROM,START/N,END/N,TADJ/N,TO/K,UPB/K/N,*
                *PP/S,LEX/S,TREE/S,PTREE/S,STRACE/S,NTRACE/S,MTRACE/S,*
                *MIDI/S,PLAY/S,ACC/S,PITCH/N,GD/S,WAIT/S,CALIB/S,DEBUG/N", argv, 50) DO
     fatalerr("Bad arguments for PLAYMUS*n")

  pitch := 0
  transposition := 0

  // Set default values of the switch variables
  // These can be set to TRUE but can be turned off using
  // command arguments.
  optPp := FALSE
  optLex := FALSE
  optTree := FALSE
  optPtree := FALSE
  optStrace := FALSE
  optNtrace := TRUE//FALSE
  optMtrace := FALSE
  genmidi := FALSE
  play := FALSE
  accompany := FALSE
  graphdata := FALSE
  waiting := FALSE
  calibrating := FALSE
  FOR i = 1 TO 9 DO debugv!i := FALSE
  //debugv!1 := TRUE // Trace pushval 


  IF argv!0 DO filenameroot := argv!0       // FROM
  IF argv!1 DO startbarno  := !(argv!1)     // START/N  -- First bar to play
  IF argv!2 DO endbarno    := !(argv!2)     // END/N    -- Last bar to play
  IF argv!3 DO erate       := !(argv!3)     // TADJ/N   -- Tempo adjustment
  IF argv!4 DO toname      := argv!4        // TO/K
  IF argv!5 DO bg_baseupb  := !(argv!5)     // UPB/N/K    -- BGPM space
  IF argv!6 DO optPp := ~optPp              // PP/S     -- Print macrogenerated text 
  IF argv!7 DO optLex := ~optLex            // LEX/S    -- Trace lexical tokens
  IF argv!8 DO optTree := ~optTree          // TREE/S   -- Print init parse tree
  IF argv!9 DO optPtree := ~ optPtree       // PTREE/S  -- Print part trees
  IF argv!10 DO optStrace := ~ optStrace    // STRACE/S -- Syn trace
  IF argv!11 DO optNtrace := ~optNtrace     // NTRACE/S -- Note tracing
  IF argv!12 DO optMtrace := ~optMtrace     // MTRACE/S -- Midi tracing playing
  IF argv!13 DO genmidi := ~genmidi         // MIDI/S   -- Generate a .mid file
  IF argv!14 DO play := ~play               // PLAY/S   -- Play the midi data
  IF argv!15 DO accompany := ~accompany     // ACC/S    -- Accompany listen to the
                                            //          -- microphone and keyboard
  IF argv!16 DO pitch := !(argv!16)         // PITCH/N  -- Change pitch
  IF argv!17 DO graphdata := ~graphdata     // GD/S     -- Generate graph data
  IF argv!18 DO waiting := ~waiting         // WAIT/S   -- Wait before playing
  IF argv!19 DO calibrating := ~calibrating // CALIB/S  -- Calibrate Midi-Mic delay
  IF argv!20 DO                             // DEBUG/N  -- debug 1 to 9
  { LET n = !argv!20
    debugv!n := ~debugv!n
  }

  IF accompany DO play := TRUE

  // filenameroot must not contain any dots.
  FOR i = 1 TO filenameroot%0 IF filenameroot%i='.' DO
  { writef("*nThe from filename must not contain any dots*n")
    GOTO fin
  }

  concatext(filenameroot, ".mus", fromfilename)
  concatext(filenameroot, ".mid", midifilename)

  //optLex := TRUE

  lineno := 0

  playmus_version := "Playmus 22 June 2019" // Used here and in writemidi.
  writef("*n%s*n*n", playmus_version)

//abort(5112)

  chbuf   := b
  chbufln := bln
  FOR i = 0 TO 63 DO chbuf!i, chbufln!i := 0, 0
  chcount := 0

  strv := s1         // Short term string buffer
  debugv := dbv
  FOR i = 0 TO 9 DO debugv!i := FALSE


  baseday := -1 // This will be initialised by first call of getrealmsecs
  chanvol := -1
  variablevol := FALSE

  killco := createco(deleteco, 500)

  errcount, errmax := 0, 5
  fin_p, fin_l := level(), fin
  rec_p, rec_l := fin_p, fin_l

  bg_baseupb := 100_000    //Default work space size for bgpm.
  startbarno, endbarno := 1, maxint/2
  start_msecs, end_msecs := 0, maxint
  solochannels := 0  // No soloists yet
  quitting := FALSE

  bg_base := 0              // Base of BGPM workspace
  sourcestream := 0
  getstreams := 0
  tostream := 0
  bgpmco := 0

  // Space for parse tree, shape data, note data, etc.
  blklist, blkp, blkt, blkitem := 0, 0, 0, 0
  // Initialise the freelists
  mk1list, mk2list, mk3list, mk4list, mk5list := 0, 0, 0, 0, 0
  mk6list, mk7list, mk8list, mk9list          := 0, 0, 0, 0

  // Room for 100 file names
  sourcefileupb := 100
  sourcenamev := newvec(sourcefileupb)
  UNLESS sourcenamev DO
  { writef("Insufficient space available*n")
    GOTO fin
  }
  sourcefileno := 1
  FOR i = 0 TO sourcefileupb DO sourcenamev!i := "unknown"   

  // Sourcefile 1 is "built-in" used during initialisation.
  // Sourcefile 2 is always the FROM filename
  // Higher numbers are for GET file names
  lineno := (1<<20) + 1 // lineno value of first line of "built-in"
  plineno := 0

  msecsbase := -1
  oer, oem, erate := getrealmsecs(), 0, 1000
  ocr, ocm, crate := oer, oem, erate
  bg_baseupb := 100_000    // BGPM workspace upb
 

  IF bg_baseupb<5000 DO bg_baseupb := 5000
  bg_base := getvec(bg_baseupb)    // BGPM workspace
  UNLESS bg_base DO
    fatalerr("Unable to allocate work space (upb = %n)*n", bg_baseupb)

  sourcefileno  := 1
  sourcenamev!1 := "built-in macro"

  { LET len = fromfilename%0
    LET str = newvec(len/bytesperword)
    IF str FOR i = 0 TO len DO str%i := fromfilename%i
    sourcefileno := sourcefileno+1
    sourcenamev!sourcefileno := str
  }

  sourcestream := findinput(fromfilename)
  lineno := (sourcefileno<<20) + 1

  UNLESS sourcestream DO fatalerr("Unable to read file %s*n", fromfilename)

  tostream := sysout
  IF toname DO
  { tostream := findoutput(toname)
    UNLESS tostream DO fatalerr("Unable to write to file %s*n", argv!1)
  }

  // Ctreate the macrogenerator coroutine.
  bgpmco := createco(bgpmfn, 2000)

  UNLESS bgpmco DO fatalerr("Unable to create bgpmco*n")

  selectinput(sourcestream)
  selectoutput(tostream)

  IF optPp DO
  { // Test the output of BGPM
    LET prevlineno = -1

    writef("*nTesting the output of the macrogenerator*n")

    { ch := rch()

      UNLESS lineno=prevlineno DO
      { newline()
        prlineno(lineno); writef(" ")
        prevlineno := lineno
      }
      IF ch<32 SWITCHON ch INTO
      { DEFAULT:
          writef("<%n>", ch)
          LOOP

        CASE endstreamch:
          writef("<eof>", ch)
          BREAK

        CASE '*n':
          writef("<**n>", ch)
          LOOP

        CASE '*t':
          writef("<**t>", ch)
          LOOP
      }
      
      wrch(ch)
    } REPEAT
    newline()
    GOTO fin
  }

  ch := rch()
 
  // Set the defaults so that the next note will be a
  // quarter note in octave 4 (middle C up to B).
  prevlengthnum := 4
  prevoctave, prevnoteletter := 4, 'f'

  tree := formtree()              // Perform Syntax Analysis

  IF optLex GOTO fin

  // Fill in all the qlen firelds
  calcqlen(tree)

  IF optTree DO { writes("*nTree before processing*n*n")
                  prtree(tree, 0, 20)
                  newline()
                }

  IF errcount GOTO fin

  timesig_t, timesig_b := 4, 4
  qbeatsperbeat := 4096/timesig_b // ie 1024 for quarter note beats
  beatcount := 1
  currpartname := 0

  midilist := 0           // Initialist the list of midi items
  midiliste := @midilist  // Pointer to final link in the list
                          // used when appending midi items.

  currbarno := 1
  variablevol := FALSE

  UNLESS trscore(tree) GOTO fin

  IF optPtree DO { writes("*nTree after calling trscore*n*n")
                   prtree(tree, 0, 20)
                   newline()
                 }


  //writef("*nUnsorted midi data*n*n")
  //prmidilist(midilist)

  midilist := mergesort(midilist)
  //writef("*nSorted midi data*n*n")
  //prmidilist(midilist)

  midilist := editnoteoffs(midilist) // Remove some note off events
  //writef("*nEdited midi data*n*n")
  //prmidilist(midilist)

  //abort(1000)

  // Initialise the events list
  eventv := newvec(eventvupb)
  eventp := 0
  prevrt := 0
  FOR i = 0 TO eventvupb DO eventv!i := 0

  IF genmidi DO writemidi(midifilename, midilist)

  IF play DO playmidi(midilist)

fin:
//  writef("*nFiles:")
//  FOR i = 1 TO sourcefileno DO
//    writef(" %n:%s", i, sourcenamev!i)
  newline()

  WHILE veclist DO
  { //writef("fin: freeing veclist vector %n*n", veclist!1)
    freevec(veclist!1)
    veclist := !veclist
  }

//writef("start: freeing killco %n*n", killco)
  IF killco DO { deleteco(killco); killco := 0 }
//writef("start: freeing soundv %n*n", soundv)
  IF soundv DO { freevec(soundv);  soundv := 0 }
//writef("start: freeing bgpmco %n*n", bgpmco)
  IF bgpmco DO { deleteco(bgpmco); bgpmco := 0 }
  IF sourcestream UNLESS sourcestream=sysin DO endstream(sourcestream)
  IF tostream UNLESS tostream=sysout DO endstream(tostream)
  selectinput(sysin)
  selectoutput(sysout)
  IF bg_base DO { freevec(bg_base); bg_base := 0 }
  WHILE blklist DO
  { LET blk = blklist
    blklist := !blk
//writef("start: freeing blklist blk %n*n", blk)
    freevec(blk)
  }
//writef("Quitting playmus*n")
//abort(1000)
  RESULTIS 0
}

.

/*
This section implements the macrogenerator used by playmus.
It is a modification of GPM designed by Strachey (in 1964)
*/

SECTION "Bgpm"

GET "libhdr"
GET "playmus.h"

LET bgputch(ch) BE
{ 
  TEST bg_h=0
  THEN { IF ch >= (1<<20) DO
         { lineno := ch
           RETURN
         }
         cowait(ch)
       }
  ELSE { UNLESS plineno=lineno DO
         { plineno := lineno
           bgpush(lineno)
         }
         bgpush(ch)
       }
}

AND bgpush(ch) = VALOF
{ UNLESS lineno=plineno DO
  { // The line number has changed so push the new
    // lineno value.
    plineno := lineno
    bgpush(lineno)
  }
  IF bg_t=bg_s DO bg_error("Insufficient work space")
  bg_s := bg_s + 1
  !bg_s := ch
  RESULTIS bg_s
}

AND bggetch() = VALOF
{ // This gets the next character either from memory or an input file.
  // It sets lineno if a lineno value is encountered when reading
  // characters from memory.

  // If a newline charcter is read from file lineno is not incremented
  // until it is copied to memory are passed to the lexical analyser
  // by bgputch.

  TEST bg_c
  THEN { // Reading from memory
         LET k = ?
         bg_c := bg_c+1
         k := !bg_c
         // bg_c points to the latest character in memory just read.

         IF k>=(1<<20) DO
         { // It was a lineno value so update lineno.
           lineno := k
           LOOP
         }
         // ch is not a line number and lineno is its lineno value.
         RESULTIS k
       } REPEAT

  ELSE { // Reading from file
         // lineno holds the file and line number of the previous
         // character read from file.
         LET k = rdch()

         { // Check for comment
           UNLESS k=c_comment RESULTIS k

           // Skip a bgpm comment. Ie skip over all characters
           // up to and including the newline and then skip
           // to the next non white space character.
           // While doing so increment lineno every time a newline
           // character is encountered.
 
           { k := rdch()
             IF k=endstreamch RESULTIS k
           } REPEATUNTIL k='*n'

           lineno := lineno + 1

           { // Skip over white space
             k := rdch()
             IF k='*s' | ch='*t' LOOP
             UNLESS  k='*n' BREAK
             lineno := lineno+1
           } REPEAT 

           // ch is a non white space character and
           // lineno is correctly set.
         } REPEAT
       }
}

AND arg(a, n) = VALOF
{ IF !a<0 DO bg_error("Too few arguments")
  IF n=0 RESULTIS a
  a, n := a+!a+1, n-1
} REPEAT

AND lookup(name) = VALOF
{ LET a, i, len = bg_e, 0, !name
  LET buf = VEC 256/bytesperword
//writef("lookup: "); prlineno(lineno); writef(" looking up *"%s*"*n",
//        arg2str(name, buf))
//abort(1000)

  WHILE a DO
  { LET p = name
    LET q = @a!2
    LET pe, qe = p+!p, q+!q
//writef("lookup: p=%n q=%n pe=%n qe=%n*n", p, q, pe, qe)
    { LET ch1 = s_eom
      LET ch2 = s_eom
      // Skip over lineno values.
      WHILE p<pe DO
      { p := p+1
        ch1 := !p
        IF ch1<(1<<20) BREAK
      }
      // Skip over lineno values.
      WHILE q<qe DO
      { q := q+1
        ch2 := !q
        IF ch2<(1<<20) BREAK
      }
//writef("lookup: ch1=%n %c ch2=%n %c*n", ch1, ch1>32->ch1,'?', ch2, ch2>32->ch2,'?')
      UNLESS ch1=ch2 BREAK
      IF ch1=s_eom RESULTIS a    // Macro definition found
      // Compare more characters
    } REPEAT
    // try the next macro definition
    a := !a      
  }

//writef("lookup: macro not found*n")

  bg_error("Macro *"%s*" not defined", arg2str(name, buf))
  longjump(fin_p, fin_l)
  RESULTIS 0
}

AND arg2str(a, str) = VALOF
{ // Convert and argument to s string removing <fno/lno> items.
  LET len = !a
  LET i, j = 0, 1
  IF len>20 DO len := 20
  FOR j = 1 TO len DO
  { LET ch = a!j
    IF ch>255 LOOP  // Ignore line number words
    i := i+1
    str%i := ch
  }
  str%0 := i
  IF !a>20 DO str%19, str%20 := '.', '.'
  RESULTIS str
}

AND define(name, code) BE
{ // Define a built in macro.
  LET s1 = bg_s
//sawritef("define: Defining %s S=%n T=%n E=%n*n", name, bg_s, bg_t, bg_e)
  // Build the macro definition on the S stack.

  // Stop bgpush from pushing an initial lineno value.
  plineno := lineno

  bgpush(bg_e)  // Save the old environment pointer
  bgpush(bg_t)  // and t
  // Push the macro name onto the stack
  bgpush(name%0+1)
  plineno := -1       // Cause bgpush to push a line value.
  FOR i = 1 TO name%0 DO bgpush(name%i)
  bgpush(1)           // The bodies of built in macros have no <fno/lno> item.
  plineno := -1       // Cause bgpush to push a line value.
  bgpush(code)        // The built-in macro code -- a negative number.
  bgpush(s_eom)       // This marks the end of the argument list.
  UNTIL bg_s=s1 DO
  { !bg_t := !bg_s
    bg_t, bg_s := bg_t-1, bg_s-1
  }
  bg_e := bg_t+1    // Set the new environment pointer 
//sawritef("define: Defined  %s S=%n T=%n E=%n*n", name, bg_s, bg_t, bg_e)
//wrenv(bg_e, 100)
//abort(1001)
}

AND bgpmfn() BE
{ // This is the main function of bgpmco which generates the sequence
  // of characters of the macro expansion of its source file.
  // It passes the expanded text to the lexical analyser by calls
  // of cowait(ch). BGPM ensures that the variable lineno holds both
  // the line number of every character it passed to the lexical
  // analyser. It does this by ensuring that lineno is correct every
  // time it reads a character from file by rdch, and it embeds lineno
  // value in any text held in memory such as macro arguments. This
  // ensures that lineno can be correctly set charcters are read from
  // file or internal memory. Macro arguments always start with a
  // lineno value and this includes the bodies of macros. When a
  // macro is called the lineno value of the semicolon that caused the
  // call is saved. This lineno value is restored when the expansion
  // of the macro completes. When a lineno value is encountered while
  // processing a macro body or argument it just update lineo which
  // will thus be correctlt set every time a character is passed to
  // the lexical analyser. The argument of cowait in bgputch is
  // always a charcter and not a lineno value. This character may
  // be endstreamch when the end of stream is encountered or the
  // special character -4 if the macrogenerator detects a fatal error.

  // The lexical analyser maintains a circular buffer of about the
  // most recent 64 characters. The characters are held in chbuf and
  // their corresponding lineno values in chbufln.

  rec_p, rec_l := level(), ret

  bg_s, bg_t, bg_h, bg_p := bg_base-1, bg_base+bg_baseupb, 0, 0
  bg_f, bg_e, bg_c       := 0, 0, 0

  define("def",     s_def)      // Define a new macro
  define("set",     s_set)      // Replace the body of a macro
  define("get",     s_get)      // Get input from a specified file
  define("eval",    s_eval)     // Evaluate a numerical expression
  define("lquote",  s_lquote)   // Return '<'
  define("rquote",  s_rquote)   // Return '>'
  define("comment", s_comment)  // Retuen '%'
  define("eof",     s_eof)      // Return eof
  define("char",    s_char)     // Convert a number to a character
  define("rep",     s_rep)      // Repeat a value a number of times
  define("rnd",     s_rnd)      // Return a signed random number
  define("urnd",    s_urnd)     // Return a non negative random number

  // lineno is initially set to the <fno/lno> value corresponding to
  // the first line of the FROM file.

  { // Start of main scanning loop.

//writef("bgpmfn: calling bggetch()*n")
    bg_ch := bggetch()

    // bg_ch is the next character to scan.
    // lineno is its <fno/lno> value.

//writef("bgpmfn: bg_ch=%x8*n", bg_ch)
sw:

//writef("bgpmfn: ch=%x8 ", bg_ch)
//IF 32<=bg_ch<=127 DO writef("'%c' ", bg_ch)
//IF bg_ch<0        DO writef(" %i3 ", bg_ch)
//writef(" "); prlineno(lineno); newline()
//abort(1009)

    SWITCHON bg_ch INTO
    { DEFAULT:
           bgputch(bg_ch)
           IF bg_ch='*n' DO lineno := lineno+1
           LOOP

      CASE endstreamch:
           IF getstreams=0 DO
           { // End of file at the outermost level
             // So send end-of-stream characters from now on.
             cowait(endstreamch) REPEAT
           }
           // Close the get stream and resume the previous input.
           endread()
           lineno       := h3!getstreams // Restore lineno of ';' of $get!...;
           sourcestream := h2!getstreams
           getstreams   := h1!getstreams
           selectinput(sourcestream)
           LOOP

      CASE c_lquote:
         { LET d = 1
           { bg_ch := bggetch()
             IF bg_ch<0 DO bg_error("Non character %n in quoted text", bg_ch)
             IF bg_ch=c_lquote DO   d := d+1
             IF bg_ch=c_rquote DO { d := d-1; IF d=0 BREAK }
             bgputch(bg_ch)
             IF bg_ch='*n' DO lineno := lineno+1
           } REPEAT
           LOOP
         }

      CASE c_call:               // '$'
           bg_f := bgpush(bg_f)  // Position of start of new macro call
           bgpush(bg_h)          // Save start of previous arg start
           bgpush(?)             // Space for <fno/lno> of ';'
           bgpush(?)             // Space for e
           bgpush(?)             //       and t
           bg_h := bgpush(0)     // Start of zeroth arg of new call
           // Every argument starts with a lineno value
           bgpush(lineno)        // Push the lineno value of this '$'
           plineno := lineno     // Set plineno appropriately.
           // It is now ready to load the zeroth argument of a call.
           LOOP

      CASE c_sep:                // '!'
           IF bg_h=0 DO
           { // Treat ! as an ordinary character if not reading macro arguments.
             bgputch(bg_ch)
             LOOP
           }
           !bg_h := bg_s-bg_h    // Fill in the length of latest arg
           bg_h := bgpush(0)     // Start a new argument.
           // Every argument starts with a lineno value
           bgpush(lineno)        // Push the lineno value of the '!'
           plineno := lineno     // Set plineno appropriately.
           // It is now ready to load the nextargument of a call.
           LOOP

      CASE c_arg:                // '#'
         { LET lno = lineno      // Save the lineno value of # in #dd

           IF bg_p=0 DO          // Ignore if not expanding a macro
           { // Treat # as an ordinary character if not loading macro arguments.
             bgputch(bg_ch)
             LOOP
           }

           bg_ch := bggetch()

           { LET n = rdint() // Read the argument number.
             LET a = arg(bg_p+5, n) // Find the start of the nth argument.

             // Copy the nth argument
             FOR q = a+1 TO a+!a DO
             { LET ch = !q
               IF ch >= (1<<20) DO
               { // ch is a lineno value so update lineno
                 lineno := ch
                 LOOP
               }
               // A argument characters will have the right lineno values.
               bgputch(ch)
             }
             lineno := lno    // Restore the lineno value of the #dd.
             GOTO sw
           }
         }

      CASE c_apply:               // Apply (;)
         { LET a = bg_f

           IF bg_h=0 DO
           { // Treat ; as an ordinary character if not reading arguments.
             bgputch(ch)
             LOOP
           }

           !bg_h := bg_s-bg_h     // Fill in the length of the latest arg
           bgpush(s_eom)          // Append EOM marking end of args
           bg_f := a!0            // Restore previous start of call pointer
           bg_h := a!1            // Restore previous start of arg pointer
           a!0 := bg_p            // Save current state
           a!1 := bg_c
           a!2 := lineno          // Save the lineno value of ';'.
           a!3 := bg_e
           a!4 := bg_t
           // Copy the call to the other stack.
           { !bg_t := !bg_s; bg_t, bg_s := bg_t-1, bg_s-1 } REPEATUNTIL bg_s<a
           bg_p := bg_t+1
           bg_c := arg(lookup(bg_p+5)+2, 1)
           // Start scanning the body of this macro.
           LOOP
         }

      CASE s_lquote:                 // Left quote ('<')
           bgputch(c_lquote)
           LOOP

      CASE s_rquote:                 // Right quote ('>')
           bgputch(c_rquote)
           LOOP
         
      CASE s_comment:                // Comment character ('%')
           bgputch(c_comment)
           LOOP
         
      CASE s_eof:                    // End of file
//writef("s_eof: reached*n")
           cowait(s_eof)
           RETURN

      CASE s_eom:                    // End of macro body
ret:       IF bg_p=0 LOOP
           bg_t   := bg_p!4
           bg_e   := bg_p!3
           lineno := bg_p!2          // Restore the previous lineno value.
           bg_c   := bg_p!1
           bg_p   := bg_p!0
           LOOP

      CASE s_def:                    // $def!name!body...;
          //            *----------------------------------------------*
          //   F H ln E T | n ln d e f | n ln name | n ln body ...     eom
          // ^ ^
          // T P
          //                         *---------------------------------*
          //                       E T | n ln name | n ln body eom ... eom
          //                       ^
          //                       E
         { LET a1 = arg(bg_p+5, 1)   // The name
           LET a2 = arg(bg_p+5, 2)   // The body
           a2!(!a2+1) := s_eom       // Mark the end of the body
           bg_e   := a1 - 2
           bg_t   := bg_e-1
           bg_e!1 := bg_p!4          // previous T
           bg_e!0 := bg_p!3          // previous E
           lineno := bg_p!2          // Restore the previous lineno value.
           bg_c   := bg_p!1          // previous C
           bg_p   := bg_p!0          // previous P
           LOOP
         }

      CASE s_set:                    // $set!name!new value;
         { LET name = arg(bg_p+5, 1)
           LET val  = arg(bg_p+5, 2)
           LET len = !val
           LET a = lookup(name)
           LET b = arg(a+2, 1)
           LET max = a!1 - b - 1  // Max length of the value.
           // Truncate the length if necessary.
           IF len>max DO len := max
           // Copy the value with its lineno values into the body
           // of the named macro.
           FOR i = 0 TO len DO b!i := val!i
           b!(len+1) := s_eom
           GOTO ret
         }

      CASE s_get:                    // $get!filename;
         { LET name = arg(bg_p+5, 1)
           LET len = !name
           LET n = 0
           LET filename = VEC 256/bytesperword
           //lineno := bg_p!2 // Use the fno/lno of the get call.
           // Remove fno/lno items from the file name
           FOR i = 1 TO len DO // Remove lineno values from the file name.
           { LET ch = name!i
             IF ch >= (1<<20) LOOP
             n := n+1
             IF n>255 DO bg_error("File name too long")
             filename%n := name!i
             filename%0 := n
           }
           // Return from $get!....;
           bg_t   := bg_p!4
           bg_e   := bg_p!3
           lineno := bg_p!2
           bg_c   := bg_p!1
           bg_p   := bg_p!0
           performget(filename)
           LOOP
         }

      CASE s_char:                    // $char!expression;
           bgputch(FIX evalarg(1))
           GOTO ret

      CASE s_eval:                    // $eval!expression;
           bgwrnum(evalarg(1))
           GOTO ret

      CASE s_rep:                     // $rep!count!text;
         { LET a = arg(bg_p+5, 2)
           FOR k = 1 TO FIX evalarg(1) DO
           { // Copy text to currest destination (memory or output),
             // dealing with lineno values appropriately.
             FOR q = a+1 TO a+!a DO
             { LET ch = !q
               IF ch >= (1<<20) DO
               { lineno := ch
                 LOOP
               }
               // ch is not a lineno value.
               bgputch(ch)
             }
           }
           GOTO ret
         }

      CASE s_rnd:      // $rnd!expression;
                       // Return a signed random number is
                       //        in specified range
         { LET rno =  randno(2_000_001) - 1_000_001
           // rno is a number in the range -1_000_000 to +1_000_000
           bgwrnum(evalarg(1) * FLOAT rno  / 1_000_000.0)
           GOTO ret
         }

      CASE s_urnd:     // $urnd!expression;
                       // Return an unsigned random number is
                       //        in specified range
         { LET rno =  randno(1_000_001) - 1
           // rno is a number in the range 0 to +1_000_000
           bgwrnum(evalarg(1) * FLOAT rno  / 1_000_000.0)
           GOTO ret
         }
    }
  } REPEAT
}

AND rdint() = VALOF
{ // Only used only in the macrogenerator when reading #ddd
  // It returns the value as an integer.
  LET val = 0

  { IF bg_ch >= (1<<20) DO
    { lineno := bg_ch
      bg_ch := bggetch()
      LOOP
    }

    UNLESS '0'<=bg_ch<='9' RESULTIS val

    val := 10*val + bg_ch - '0'
    bg_ch := bggetch()
  } REPEAT
}

AND performget(filename) BE
{ // First look in the current directory
  LET musfilename = VEC 50
  LET stream = findinput(concatext(filename, ".mus", musfilename))
  //writef("Searching for *"%s*" in the current directory*n", musfilename)

  // Then try the headers directories
  //UNLESS stream DO writef("Searching for *"%s*" in MUSHDRS*n", musfilename)
  UNLESS stream DO stream := pathfindinput(musfilename, "MUSHDRS")
//writef("performget: get stream=%n*n", stream)
  UNLESS stream DO
  { bg_error("Unable to $get!%s;", musfilename)
    RETURN
  }

  IF sourcefileno>=sourcefileupb DO
  { bg_error("Too many GET files")
    RETURN
  }

  { LET len = musfilename%0
    LET str = newvec(len+4/bytesperword)
    IF str FOR i = 0 TO musfilename%0 DO str%i := musfilename%i
    sourcefileno := sourcefileno+1
    sourcenamev!sourcefileno := str
  }

  getstreams := mk3(getstreams, sourcestream, lineno)
  sourcestream := stream
  selectinput(sourcestream)
//writef("performget: old lno = "); prlineno(lineno); newline()
  lineno := (sourcefileno<<20) + 1
//writef("performget: new lno = "); prlineno(lineno); newline()
}

AND evalarg(n) = VALOF
{ argp := arg(bg_p+5, n)
  argt := argp + !argp + 1
  RESULTIS bgexp(0)
}

AND bgbexp() = VALOF
{ // This reads and evaluates a basic expression and
  // returns its value as a floating point number.
  bg_ch := getargch()

//sawritef("bgbexp: bg_ch=%n*n", bg_ch)
  SWITCHON bg_ch INTO
  { DEFAULT:  bg_error("Bad expression, ch=%c", ch)

    CASE '*s': LOOP // Ignore spaces within expressions

    CASE '.':
    CASE '0': CASE '1': CASE '2': CASE '3': CASE '4':
    CASE '5': CASE '6': CASE '7': CASE '8': CASE '9':
            { LET res = 0
              ch := bg_ch
              res := rdnum(getargch)
              bg_ch := ch
              RESULTIS res
            }

    CASE '*'':
            { LET c = getargch()
              bg_ch := getargch()
              RESULTIS FLOAT c
            }

    CASE '+': RESULTIS   bgexp(2)
    CASE '-': RESULTIS #-bgexp(2)
    CASE '~': RESULTIS FLOAT(~ FIX bgexp(2))

    CASE '(': { LET FLT res = bgexp(1)
                UNLESS bg_ch=')' DO bg_error("')' expected")
                bg_ch := getargch()
                RESULTIS res
              }
  }
} REPEAT

AND bgexp(n) = VALOF
{ LET FLT a = bgbexp()

  { SWITCHON bg_ch INTO
    { DEFAULT:   IF n>1 | n=1 & bg_ch=')' | n=0 & bg_ch=s_eof RESULTIS a
                 bg_error("Bad expression")
      CASE '*s': bg_ch := getargch() // Ignore spaces within expressions
                 LOOP

      CASE 'R':                                // R   (right shift)
      CASE 'r':  IF n<6 DO { LET ai = FIX a
                             LET bi = FIX bgexp(6)
                             a := FLOAT(ai>>bi)
                             LOOP
                           }
                 RESULTIS a

      CASE 'L':                                // L   (left shift)
      CASE 'l':  IF n<6 DO { LET ai = FIX a
                             LET bi = FIX bgexp(6)
                             a := FLOAT(ai<<bi)
                             LOOP
                           }
                 RESULTIS a

      CASE '**': IF n<5 DO { a := a  *  bgexp(5); LOOP }
                 RESULTIS a
      CASE '/':  IF n<5 DO { a := a  /  bgexp(5); LOOP }
                 RESULTIS a
      CASE 'm':  IF n<5 DO { a := a MOD bgexp(5); LOOP }
                 RESULTIS a
      CASE '+':  IF n<4 DO { a := a  +  bgexp(4); LOOP }
                 RESULTIS a
      CASE '-':  IF n<4 DO { a := a  -  bgexp(4); LOOP }
                 RESULTIS a
      CASE '&':  IF n<3 DO { LET ai = FIX a
                             LET bi = FIX bgexp(3)
                             a := FLOAT(ai&bi)
                             LOOP
                           }
                 RESULTIS a
      CASE '|':  IF n<2 DO { LET ai = FIX a
                             LET bi = FIX bgexp(2)
                             a := FLOAT(ai|bi)
                             LOOP
                           }
                 RESULTIS a
    }
  } REPEAT
}

AND getargch() = VALOF
{ // Return the next character from memory, dealing with
  // any lineno values encoutered.
  LET p = argp+1
  IF p>=argt RESULTIS s_eof
  argp := p
  ch := !p
  UNLESS ch >= (1<<20) RESULTIS ch
  // ch is a lineno value
  lineno := ch
} REPEAT

AND rdnum(rdchfn) = VALOF
{ // Read and return a floating point number from characters
  // read by rdchfn (which is getargch if in the macro generator,
  // or by rdc if not).

  // On entry the first character of the number will be in ch.
  // On exit ch will hold the first character after the number.

  // Syntax: [digits] [. digits]

  // where digits is one or more decimal digits. There must be
  // at least one digit before or after the decimal point.
  // If successful, the result is the floating point number
  // and result2 is zero. On failure the result is zero
  // and result2 to -1.

  // The strategy is to construct the significand by repeatedly
  // multiplying it by 10 and adding the next digit. As soon as
  // a digit is encountered that would cause the significant to
  // overflow, the remaining digits are read and counted in
  // ecount without changing the value of the significand. The
  // true significand should thus be multiplied by 10^ecount.

  LET dcount = 0    // This will hold the total number of
                    // decimal digits in the number both
                    // before and after the decimal point.
                    // This must be >0 for a valid number.

  LET ecount = 0    // This will hold the number of digits
                    // encountered after the significand
                    // becomes greater than 99999999.

  LET fcount= -1    // This will the number of digits after
                    // the decimal point. It will be set to
                    // zero if a decimal point was not found.

  // After reading all the digits of the number the resulting
  // value will be the significand multiplied by 10^ecount and
  // divided by 10^fcount.

  LET val = 0 // An integer to hold the significand.
  LET ignoredigits = FALSE // This is set to TRUE when the
                           // significand can no longer
                           // accumulate decimal digits
                           // without overflowing.

//sawritef("rdnum: entered*n")

  UNLESS '0'<=ch<='9' | ch='.' GOTO fail

  // Read the significand
  WHILE '0'<=ch<='9' | ch='.' DO
  {
//sawritef("rdnum: dealing with significand ch='%c'*n", ch)
    SWITCHON ch INTO
    { DEFAULT: BREAK

      CASE '0': CASE '1': CASE '2': CASE '3': CASE '4': 
      CASE '5': CASE '6': CASE '7': CASE '8': CASE '9':
      { LET digval = ch-'0'
        dcount := dcount+1  // Count of decimal digits
        // Count digits after the decimal point, if any.
        IF fcount>=0 DO fcount := fcount+1

        UNLESS ignoredigits DO
          IF val > maxint/10 | 10*val > maxint-digval DO
            ignoredigits := TRUE // Digits can no longer be
                                 // accumulated.

        TEST ignoredigits
        THEN { // No more digits can be accumulated
               ecount := ecount+1 // Count of ignored digits
             }
        ELSE { // Accumulate the current decimal digit
               val := 10*val + digval
             }

//sawritef("rdnum: digit '%c' => val=%n ecount=%n*n", bg_ch, val, ecount)
        ENDCASE
      }

      CASE '.': 
        UNLESS fcount<0 GOTO fail // Fail if two or more decimal points.
        fcount := 0 // Begin counting digits after the decimal point.
        ENDCASE
    }
    ch := rdchfn()
  }

//sawritef("rdnum: Exited from significand loop*n")

  IF fcount<0 DO fcount := 0

  // The true value of the number is
  //    val multiplied by 10^(ecount-fcount)

  // Convert val x 10^(ecount-fcount) to a floating point number of the
  // current BCPL word length.
  val := sys(Sys_flt, fl_mk, val, ecount-fcount)
//sawritef("rdnum: return result %13.6f*n", val)
  result2 :=  0  // Successful return
//abort(1234)
  RESULTIS val

fail:
//abort(2345)
  result2 := -1
  RESULTIS 0
}


AND bgwrnum(FLT x) BE
{ // This convert the floating point number x to
  // a sequence of digit and possibly a decimal point.
  // If the number can be represented as an integer
  // there will be no decimal point.
  LET FLT frac = sys(Sys_flt, fl_modf, x)
  LET intpart = FIX result2
//sawritef("bgwrnum: x=%13.6f*n", x)
//sawritef("bgwrnum: int part of x=%n frac part of x=%13.6f*n", intpart, frac)
  IF x<0 DO
  { bgputch('-')
    intpart := -intpart
    frac := -frac
  }
  wrpn(intpart)
  IF frac > 0 DO
  { LET scaledfrac = FIX sys(Sys_flt, fl_floor, (frac+0.0000005) * 1_000_000)
    LET digits = 0
    FOR i = 1 TO 6 DO
    { digits := digits*10 + scaledfrac MOD 10
      scaledfrac := scaledfrac/10
    }
    IF digits DO
    { bgputch('.')
      WHILE digits DO
      { bgputch(digits MOD 10 + '0')
        digits := digits/10
      }
    }
  }
}

AND wrpn(n) BE
{ IF n>9 DO wrpn(n/10)
  bgputch(n MOD 10 + '0')
}

AND wrc(ch) BE IF -127<=ch<=127 DO
{ IF ch='*n' DO { newline(); chpos := 0; RETURN }
  IF chpos>70 DO wrs("*n  ")
  TEST ch<0
  THEN { writef("'%n'", ch)
         chpos := chpos+3
       }
  ELSE { UNLESS '*s'<=ch<127 DO ch := '?'  // Assume 7 bit ASCII.
         wrch(ch)
         IF ch='*n' DO wrs(" ")
         chpos := chpos+1
       }
}

AND wrs(s) BE FOR i = 1 TO s%0 DO wrc(s%i)

AND wrn(n) BE
{ IF n>9 DO wrn(n/10)
  wrc(n MOD 10 + '0')
}

AND bg_error(mess, a, b, c) BE
{ // This is for errors detected by the macrogenerator.
  LET out = output()
  selectoutput(sysout)
  writef("*n*n######### Error near "); prlineno(lineno); writef(": ")
  writef(mess, a, b, c)
  errcount := errcount+1
  IF errcount>5 DO
  { writef("*n*n######### Error near "); prlineno(lineno); writef(": ")
    writef("Too many errors*n")
    cowait(-4) // Indicate a fatal error
  }
  error()
  selectoutput(out)
}

AND error(mess, a, b, c) BE
{ LET out = output()
  selectoutput(sysout)
  wrs("*nIncomplete calls:*n")
  IF bg_f DO prcall(3, bg_f, bg_h, bg_s)
  wrs("Active macro calls:*n"); btrace(bg_p, 3)
  //wrs("*nEnvironment:*n");  wrenv(bg_e, 20)
  //wrs("######### End of error message*n")
  wrc('*n')

  errcount := errcount+1
  IF errcount >= errmax DO fatalerr("*nToo many errors")
  
  selectoutput(out)
  longjump(rec_p, rec_l)
}

AND prcall(n, f, h, s) BE UNLESS f=0 TEST n=0
                                     THEN wrs(" ...")
                                     ELSE { prcall(n-1, !f, f!1, f-1)
                                            !h := s-h
                                            wrcall(f+5, s)
                                          }

AND btrace(p, n) BE
{ IF n=0 DO wrs(" ...*n")
  IF p=0 | n=0 RETURN
  wrcall(p+5, p!4); wrc(c_apply); wrc('*n')
  p, n := !p, n-1
} REPEAT

AND wrcall(a, b) BE
{ LET sep = c_call
  LET lno = a!1
  LET filename = sourcenamev!(lno>>20)
  LET ln = lno & #xFFFFF
  prlineno(lno)
  writef("   ")
 
  UNTIL a>=b DO { wrc(sep); wrarg(a)
                  a := a + !a + 1
                  sep := c_sep
                }
}

AND wrarg(a) BE
{ LET len = !a
  LET p = a+1
  LET q = p + len - 1
  TEST len>20
  THEN { FOR i = p TO p+9 IF !i<256 DO wrc(!i)
         wrs("...")
         FOR i = q-9 TO q IF !i<256 DO wrc(!i)
       }
  ELSE { FOR i = p TO q IF !i<256 DO wrc(!i)
       }
}

// wrenv outputs the list of defined macros
AND wrenv1(e, n) BE
{ writef("wrenv(%n, %n) entered*n", e, n)
abort(1345)
  wrenv1(e, n)
}

AND wrenv(e, n) BE UNTIL e=0 DO
{ LET name  = arg(e+2, 0)
  LET value = arg(e+2, 1)
  IF n=0 DO { wrs(" ...*n"); RETURN }
  wrs(" Name: ");   wrarg(name); FOR i = !name TO 12 DO wrc('*s')
  wrs("  Value: "); wrarg(value)
  wrc('*n')
  e, n := !e, n-1
}

LET newvec(n) = VALOF
{ LET p = blkp
  blkp := p+n+1
  IF blkp>=blkt DO
  { LET v = getvec(blkupb) // Get some more space
//writef("newvec: allocation block %n upb %n*n", v, blkupb)
    UNLESS v & n<blkupb DO
    { LET out = output()
      selectoutput(sysout)
      writef("*nSystem error: newvec failure*n")
      selectoutput(out)
      abort(999)
    }
    
    v!0 := blklist
    blklist := v
    blkt := v+blkupb
    p    := v+1
    blkp := p+n+1
  }
//writef("newvec: allocated p=%n n=%i4 blklist=%n*n",
//         p, n, blklist)
  //IF optStrace DO writef("%i6 -> newvec upb %n*n", p, n)
  RESULTIS p
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
{ LET p = mk3list
  TEST p
  THEN mk3list := !p  // Use a node from the mk3 free list
  ELSE p := newvec(2) // Allocate a new node
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
 
AND mk7(a, b, c, d, e, f, g) = VALOF
{ LET p = newvec(6)
  p!0, p!1, p!2, p!3, p!4, p!5, p!6 := a, b, c, d, e, f, g
  RESULTIS p
}

AND mk8(a, b, c, d, e, f, g, h) = VALOF
{ LET p = newvec(7)
  p!0, p!1, p!2, p!3, p!4, p!5, p!6, p!7 := a, b, c, d, e, f, g, h
  RESULTIS p
}

AND mk9(a, b, c, d, e, f, g, h, i) = VALOF
{ LET p = newvec(8)
  p!0, p!1, p!2, p!3, p!4, p!5, p!6, p!7, p!8 := a, b, c, d, e, f, g, h, i
  RESULTIS p
}

AND unmk1(p) BE { !p := mk1list; mk1list := p }
AND unmk2(p) BE { !p := mk2list; mk2list := p }
AND unmk3(p) BE { !p := mk3list; mk3list := p }
AND unmk4(p) BE { !p := mk4list; mk4list := p }
AND unmk5(p) BE { !p := mk5list; mk5list := p }
AND unmk6(p) BE { !p := mk6list; mk6list := p }
AND unmk7(p) BE { !p := mk7list; mk7list := p }
AND unmk8(p) BE { !p := mk8list; mk8list := p }
AND unmk9(p) BE { !p := mk9list; mk9list := p }
.

SECTION "Lex"

GET "libhdr"
GET "playmus.h"

LET rch() = VALOF
{ // Return the next character obtained from the BGPM coroutine.

  LET char = callco(bgpmco)

  IF char<-2 DO longjump(fin_p, fin_l) // Fatal error found in BGPM

  // Save the character in the circular buffer for error messages.
  chcount := chcount+1
  chbuf  !(chcount&63) := char
  chbufln!(chcount&63) := lineno

  RESULTIS char
}

AND lex() BE
{ LET neg = FALSE

  // ch holds the first character of the token
  // and lineno holds it lineno value.
  // This function set token to be the next lexical token
  // and tokln to be the lineno value of its first character.

  tokln := lineno
  // Note that tokln is updated after white space characters
  // and comments, so when lex returns it will hold certainly
  // hold the lineno value of the first character of the token.

//writef("lex: "); prlineno(lineno); writef(": ch=%n '%c'*n", ch, ch)
//abort(1000)

  SWITCHON ch INTO
  { DEFAULT:
      UNLESS ch=endstreamch DO
      { LET badch = ch
        ch := '*s'
        synerr("Illegal character %x2 '%c'", badch, badch)
      }
      token := s_eof
      RETURN

    CASE '*p': CASE '*n':
    CASE '*c': CASE '*t': CASE '*s':
                ch := rch()
                tokln := lineno
                LOOP

    CASE '-':   neg := TRUE
    CASE '+':   ch := rch()
                UNLESS '0'<=ch<='9' DO
                  synerr("Bad number")

    CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
    CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
                // num is be used in shapes
                numval := rdnum(rch)    // A floating point value.
                IF neg DO numval := -numval
                token := s_num
                RETURN

    CASE 'a':CASE 'b':CASE 'c':CASE 'd':CASE 'e':CASE 'f':CASE 'g':
                // All notes start with a to g
//sawritef("lex: case 'a'-'g': reached*n")
                token := s_note // May change to s_notetied
                noteletter := ch
                notesharps :=  0  // = 0, 1, 2, -1 or -2
                reloctave  :=  0  // Octaves up
                notelengthnum := -1  // If not specified
                dotcount   :=  0

                ch := rch()
                IF ch='i' DO     // sharp or double sharp
                { ch := rch()
                  UNLESS ch='s' DO synerr("Bad note")
                  ch := rch()
                  UNLESS ch='i' DO
                  { notesharps := 1  // One sharp
                    GOTO rdoctave
                  }
                  ch := rch()
                  UNLESS ch='s' DO synerr("Bad note")
                  ch := rch()
                  notesharps := 2    // A double sharp
                  GOTO rdoctave
                }
                IF ch='e' DO     // flat or double double
                { ch := rch()
                  UNLESS ch='s' DO synerr("Bad note")
                  ch := rch()
                  UNLESS ch='e' DO
                  { notesharps := -1  // One flat
                    GOTO rdoctave
                  }
                  ch := rch()
                  UNLESS ch='s' DO synerr("Bad note")
                  ch := rch()
                  notesharps := -2    // A double flat
                  GOTO rdoctave
                }
rdoctave:
                WHILE ch='*'' | ch=',' DO
                { // octaves up or down
                  TEST ch='*''
                  THEN reloctave := reloctave+1 // One octave up
                  ELSE reloctave := reloctave-1 // One octave down
                  ch := rch()
                }
rdlength:
                notelengthnum := -1      // No explicit length yet
                WHILE '0'<=ch<='9' DO
                { IF notelengthnum<0 DO notelengthnum := 0
                  notelengthnum := notelengthnum*10 + ch - '0'
                  ch := rch()
                }
//writef("notelengthnum=%n*n", notelengthnum)

                dotcount := 0
                WHILE ch='.' DO
                { dotcount := dotcount+1
                  ch := rch()
                }
//writef("dotcount=%n*n", dotcount)

                IF ch='~' & token=s_note DO
                { token := s_notetied
                  ch := rch()
                }
                // token = s_note or s_notetied
                // noteletter = 'a' .. 'g'
                // notesharps = -2, -1, 0, 1, 2
                // reloctave = -9,..., 0,..., 9   an integer
                // notelengthnum = -1, 0, 1, 2, 4, 8, 16,...   an integer
                // dotcount = 0, 1, 2,...
                RETURN

    CASE 'r':  token := s_rest
               ch := rch()
               GOTO rdlength

    CASE 's':  token := s_space
               ch := rch()
               GOTO rdlength

    CASE 'z':  token := s_null         // A zero length space
               ch := rch()
               BREAK

    CASE '\':
//sawritef("case '\': tokln=<%n/%n>*n", tokln>>20, tokln & #xFFFFF)
//abort(1000)
              ch := rch()    // Reserved words, eg \vol
              token := lookupword(rdtag())
//sawritef("case '\': token=%s*n", opstr(token))
//abort(1987)
              IF token=s_word DO synerr("Unknown keyword \%s", charv)
              RETURN
 
    CASE '[': token := s_lsquare;   ch := rch(); BREAK
    CASE ']': token := s_rsquare;   ch := rch(); BREAK
    CASE '(': token := s_lparen;    ch := rch(); BREAK
    CASE ')': token := s_rparen;    ch := rch(); BREAK 
    CASE '{': token := s_lcurly;    ch := rch(); BREAK
    CASE '}': token := s_rcurly;    ch := rch(); BREAK 
    CASE ':': token := s_colon;     ch := rch(); BREAK

    CASE '**':// * can occur in shapes
              token := s_star
              ch := rch()
              RETURN

    CASE '|': ch := rch()
              IF ch='|' DO { token := s_doublebar; ch := rch(); BREAK }
              token := s_barline
              RETURN
 
    CASE '/':   ch := rch()
                IF ch='/' DO
                { ch := rch() REPEATUNTIL ch='*n' | ch=endstreamch
                  tokln := lineno
                  LOOP
                }

                IF ch='**' DO
                { LET depth = 1

                  { ch := rch()
                    IF ch='**' DO
                    { ch := rch() REPEATWHILE ch='**'
                      IF ch='/' DO { depth := depth-1; LOOP }
                    }
                    IF ch='/' DO
                    { ch := rch()
                      IF ch='**' DO { depth := depth+1; LOOP }
                    }
                    IF ch=endstreamch DO synerr("Missing '**/'")
                  } REPEATUNTIL depth=0

                  ch := rch()
                  LOOP
                }

                synerr("Bad comment")
                RETURN
 
 
    CASE '"':
              { LET len = 0
                ch := rch()
 
                UNTIL ch='"' DO
                { IF len=255 DO synerr("Bad string constant")
                  len := len + 1
                  charv%len := rdstrch()
                }
 
                charv%0 := len
                stringval := newvec(len/bytesperword)
                FOR i = 0 TO len DO stringval%i := charv%i
                token := s_string
//writef("string node %n for '%s' created*n", stringval, stringval)
                ch := rch()
                RETURN
              }
 
  } // End of switch
} REPEAT
 
LET lookupword(word) = VALOF
{ // Return the token for a keyword
  // or s_word, if not found.
  LET len, i = word%0, 0
  LET hashval = len
  FOR i = 1 TO len DO hashval := (13*hashval + word%i) & #xFF_FFFF
  hashval := hashval REM nametablesize
  wordnode := nametable!hashval
 
  WHILE wordnode & i<=len TEST (@h3!wordnode)%i=word%i
                          THEN i := i+1
                          ELSE wordnode, i := !wordnode, 0
  UNLESS wordnode DO
  { wordnode := newvec(len/bytesperword+3)
    !wordnode := nametable!hashval
    h2!wordnode := s_word
    FOR i = 0 TO len DO (@h3!wordnode)%i := word%i
    nametable!hashval := wordnode
  }
  RESULTIS h2!wordnode
}
 
AND dsw(word, tok) BE { lookupword(word); h2!wordnode := tok  }
 
AND declsyswords() BE
{ dsw("altoclef", s_altoclef)
  dsw("arranger", s_arranger)
  dsw("bank", s_bank)
  dsw("barlabel", s_barlabel)
  dsw("bassclef", s_bassclef)
  dsw("conductor", s_conductor)
  dsw("control", s_control)               dsw("ctrl", s_control)
  dsw("composer", s_composer)
  dsw("delay", s_delay);                  dsw("d", s_delay)
  dsw("delayadj", s_delayadj);            dsw("da", s_delayadj)
  dsw("instrument", s_instrument)
  dsw("instrumentname", s_instrumentname)
  dsw("instrumentshortname", s_instrumentshortname)
  dsw("keysig", s_keysig)
  dsw("legato", s_legato);                dsw("l", s_legato)
  dsw("legatoadj", s_legatoadj);          dsw("la", s_legatoadj)
  dsw("major", s_major)
  dsw("minor", s_minor)
  dsw("name", s_name)
  dsw("nonvarvol", s_nonvarvol)
  dsw("opus", s_opus)
  dsw("part", s_part)
  dsw("partlabel", s_partlabel)
  dsw("patch", s_patch)
  dsw("pedoff", s_pedoff)
  dsw("pedoffon", s_pedoffon)
  dsw("pedon", s_pedon)
  dsw("portaoff", s_portaoff)
  dsw("portaon", s_portaon)
  dsw("repeatback", s_repeatback)
  dsw("repeatbackforward", s_repeatbackforward)
  dsw("repeatforward", s_repeatforward)
  dsw("score", s_score)
  dsw("softoff", s_softoff)
  dsw("softon", s_softon)
  dsw("solo", s_solo)
  dsw("tempo", s_tempo);                  dsw("t", s_tempo)
  dsw("tempoadj", s_tempoadj);            dsw("ta", s_tempoadj)
  dsw("tenorclef", s_tenorclef)
  dsw("timesig", s_timesig)
  dsw("title", s_title)
  dsw("transposition", s_transposition)
  dsw("trebleclef", s_trebleclef)
  dsw("tuplet", s_tuplet);                dsw("tup", s_tuplet)
  dsw("varvol", s_varvol)
  dsw("vibrate", s_vibrate);              dsw("vr", s_vibrate)
  dsw("vibrateadj", s_vibrateadj);        dsw("vra", s_vibrateadj)
  dsw("vibamp", s_vibamp);                dsw("vm", s_vibamp)
  dsw("vibampadj", s_vibampadj);          dsw("vma", s_vibampadj)
  dsw("vol", s_vol);                      dsw("v", s_vol)
  dsw("voladj", s_voladj);                dsw("va", s_voladj)
  dsw("volmap", s_volmap)
} 
 
AND wrchbuf() BE
{ LET prevln = -1    // Not a valid lineno value
  writes("*n...")
  FOR p = chcount-63 TO chcount IF p>=0 DO
  { LET i = p&63
    LET k  = chbuf!i     // k is a valid character
    LET ln = chbufln!i   // ln is its lineno value
    IF 0<k<=255 DO
    { UNLESS ln=prevln DO
      { // The file number or linenumber has changed
        newline()
        prlineno(ln)
        prevln := ln
        writef("  ")
      }
      IF k='*n' DO
      { writef("<**n>")
        LOOP
      }
      IF k=endstreamch DO
      { writef("<eof>")
        LOOP
      }
      wrch(k)
    }
  }
  newline()
}

AND rdtag() = VALOF
{ // A tag is a sequence of letters and underlines
  // They only occur after \.
  LET len = 0
  WHILE 'a'<=ch<='z' | 'A'<=ch<='Z' |  ch='_' DO
  { len := len+1
    IF len>255 DO synerr("Name too long")
    charv%len := ch
    ch := rch()
  }
  charv%0 := len
  RESULTIS charv
}
 
AND rdstrch() = VALOF
{ LET res = ch
  IF ch='*n' | ch='*p' DO
  { 
    synerr("Unescaped newline character")
  }
  IF ch='\' DO
  { ch := rch()
    SWITCHON ch INTO
    { DEFAULT:   synerr("Bad string or character constant")
      CASE '\': CASE '*'': CASE '"':  res := ch;   ENDCASE
      CASE 't': CASE 'T':             res := '*t'; ENDCASE
      CASE 'n': CASE 'N':             res := '*n'; ENDCASE
    }
  }
  ch := rch()
  RESULTIS res
}

AND formtree() = VALOF
{ LET res = 0
  LET ln = lineno

  rec_p, rec_l := level(), recover

  charv := newvec(256/bytesperword)     
  nametable := newvec(nametablesize)
  UNLESS charv & nametable DO fatalerr("More workspace needed")
  FOR i = 0 TO nametablesize DO nametable!i := 0

  lineno := (1<<20) + 1 // Special lineno value during initialisation.
  declsyswords()

  lineno := ln  // Restore the lineno value

  IF optLex DO writef("*nTesting the lexical analyser*n*n")

  lex()

  WHILE optLex DO
  { // Code to test the lexical analyser.
    prlineno(tokln)
    writef("  %s", opstr(token))

    SWITCHON token INTO
    { DEFAULT:
//writef("default case")
         ENDCASE

      CASE s_string:
         writef(" *"%s*"", stringval)
         ENDCASE

      CASE s_num:
         writef(" %12.6f", numval)
         ENDCASE

      CASE s_note:
      CASE s_notetied:
         writef(" %c", capitalch(noteletter))
         IF notelengthnum>=0 DO writef("%n", notelengthnum)
         FOR i =  1 TO notesharps       DO wrch('#')
         FOR i = -1 TO notesharps BY -1 DO wrch('b')
         FOR i =  1 TO reloctave        DO wrch('*'')
         FOR i = -1 TO reloctave  BY -1 DO wrch(',')
         FOR i =  1 TO dotcount         DO wrch('.')
         ENDCASE

      CASE s_rest:
         writef(" R")
         IF notelengthnum>=0 DO writef("%n", notelengthnum)
         FOR i = 1 TO dotcount DO wrch('.')
         ENDCASE

      CASE s_space:
         writef(" S")
         IF notelengthnum>=0 DO writef("%n", notelengthnum)
         FOR i = 1 TO dotcount DO wrch('.')
         ENDCASE
    }

    IF token=s_eof DO
    { newline()
      BREAK
    }

    newline()
    lex()
  }

recover:
  IF optLex RESULTIS 0

  res := rdscore()
  UNLESS token=s_eof DO fatalsynerr("Incorrect termination")
  RESULTIS res
}

AND prlineno(ln) BE
  writef("%s[%n]", sourcenamev!fno(ln), lno(ln))

AND fatalerr(mess, a, b, c) BE
{ writes("*nFatal near "); prlineno(lineno); writes(": ")
  writef(mess, a, b, c)
  writes("*nCompilation aborted*n")
  longjump(fin_p, fin_l)
}
 
AND fatalsynerr(mess, a) BE
{ writef("*nError near "); prlineno(lineno); writes(": ")
  writef(mess, a)
  writef("*nRecent text:*n")
  wrchbuf()
  errcount := errcount+1
  writes("*nCompilation aborted*n")
  longjump(fin_p, fin_l)
}

AND synerr(mess, a, b, c) BE
{ writef("*nError near "); prlineno(lineno); writes(": ")
  writef(mess, a, b, c)
  wrchbuf()
  // Skip the rest of the input line 
  UNTIL ch='*n' | ch=endstreamch DO ch := rch()
  lex()
  //error("")
  errcount := errcount+1
  IF errcount >= errmax DO fatalerr("Too many errors")
  longjump(rec_p, rec_l)
}

AND trerr(absq, mess, a, b, c, d, e, f) BE
{ // If absq >= 0 the error message will include bar and beat numbers.
  // tokln will hold the file/line number of the current tree node.
  writef("*nTranslation error near "); prlineno(tokln)
  IF absq<0 & currqbeat>=0 DO absq := qscale(currqbeat)
  IF absq>=0 DO
  { LET bno = qbeats2barno(absq)
    writef(" at qbeat %n of bar %n", absq-barno2qbeats(bno), bno)
  } 
  IF currpartname DO writef(" in %s", currpartname)
  writes(":*n   ")
  writef(mess, a, b, c, d, e, f)
  newline()
  errcount := errcount+1
  IF errcount >= errmax DO fatalerr("Too many errors")
}

LET checkfor(tok, mess, a, b) BE
{ UNLESS token=tok DO synerr(mess, a, b)
  lex()
}
 
AND rdscore() = VALOF
{ // This reads a score returning 
  // a pointer   -> [-, Score, ln, name, conductor, partset, qlen]
  //   where
  //   conductor -> [-, Conductor, ln, notelist, qlen]
  //   partset   -> [-, Partset,   ln, partlist, qlen]
  //   partlist  -> [-, Part,      ln, noteseq, qlen]
  //   or        -> [-, Solo,      ln, noteseq, qlen]

  LET oldp, oldl = rec_p, rec_l

  IF token = s_score DO
  { // Read:   \score name [ conductor and parts ]
    LET scoreln   = tokln     // Lineno of \score
    LET name      = 0  // For the name of the score.
    LET conductor = 0
    LET partlist  = 0  // For the list of parts.
    LET partliste = @partlist

    lex()
    name := rdstring() // Get the name of this score.

    checkfor(s_lsquare, "'[' expected")
  
    { // Loop to read conductor and other parts
      LET ln = tokln  // Lineno of \conductor, \part or \solo.

      // We now expect \conductor, \part, \solo or ']'
      SWITCHON token INTO
      { DEFAULT:
          synerr("\conductor, \part or \solo or ']' expected")
          BREAK

        CASE s_rsquare:
          BREAK

        CASE s_conductor: // [-, Conductor, ln, notelist, qlen]
          IF conductor DO synerr("Only one conductor is allowed*n")
        CASE s_part:      // [-, Part,      ln, noteseq, qlen]
        CASE s_solo:      // [-, Solo,      ln, noteseq, qlen]
        { LET ln = tokln  // lineno of \conductor, \part or \solo
          LET op = token  // Remember the op
          LET item = 0

          lex()

          item :=  mk5(0, op,   // Conductor, Part or Solo
                          ln,   // Line no of the above operator.
                          rdnoteseq(), // Read the note sequence.
                          -1)
          IF optStrace DO
            writef("%i6 -> [-, %s, %n:%n, %n, -1]*n",
                    item,
                    opstr(op),
                    fno(ln), lno(ln),
                    h4!item)

          TEST op=s_conductor
          THEN { conductor := item   // Store in conductor
               }
          ELSE { !partliste := item  // Append to the partlist
                 partliste := item
               }

          LOOP
        }
      }
    } REPEAT

    checkfor(s_rsquare, "']' expected, token=%s", opstr(token))

    lex() // Skip over the right square bracket

    rec_p, rec_l := oldp, oldl

    UNLESS conductor DO fatalsynerr("A conductor is required") 
    UNLESS partlist  DO fatalsynerr("At least one part or solo is needed") 

    { // Make the partlist into a partset.
      LET ln = h3!partlist // Line number of the first part
      LET partset = mk5(0, s_partset, ln, partlist, -1)
      LET score = 0

      IF optStrace DO
        writef("%i6 -> [-, Partset, %n:%n, %n, -1]*n",
                partset,
                fno(ln), lno(ln),
                partlist)

      score := mk7(0, s_score, scoreln, name, conductor, partset, -1)

      IF optStrace DO
        writef("%i6 -> [-, Score, %n:%n, *"%s*", %n, %n, -1]*n",
                score,
                fno(scoreln), lno(scoreln),
                name,
                conductor,
                partset)

      rec_p, rec_l := oldp, oldl
      RESULTIS score
    }
  }

  synerr("rdscore: Bad score")
  RESULTIS 0
}

AND rdstring() = VALOF
{ LET a = stringval
  checkfor(s_string, "String expected")
  RESULTIS a
}

AND rdnumber() = VALOF
{ // Used only by the syntax analyser
  // Check that the current token is a number (s_num) and
  // return its (floating point) value, setting token to
  // the next lexical token.
  LET FLT a = numval
  checkfor(s_num, "Number expected")
  RESULTIS a
}

AND rdinteger() = VALOF
{ // This is only used in control, timesig, bank and patch statements.

  // Check the current token is a number (s_num) and return its
  // value rounded to the nearest integer (as an integer.
  LET FLT a = rdnumber()
  RESULTIS FIX a
}

AND note2qlen(lengthnum, prevlengthnum, dotcount) = VALOF
{ // Calculate the note or rest's qbeats
  LET qlen = 0

  IF lengthnum<0 DO lengthnum := prevlengthnum

  SWITCHON lengthnum INTO
  { DEFAULT:  synerr("Bad note length %n", lengthnum)

    CASE   0: qlen := 8192; ENDCASE
    CASE   1: qlen := 4096; ENDCASE
    CASE   2: qlen := 2048; ENDCASE
    CASE   4: qlen := 1024; ENDCASE
    CASE   8: qlen :=  512; ENDCASE
    CASE  16: qlen :=  256; ENDCASE
    CASE  32: qlen :=  128; ENDCASE
    CASE  64: qlen :=   64; ENDCASE
    CASE 128: qlen :=   32; ENDCASE
  }

  { LET q = qlen
    FOR i = 1 TO dotcount DO
    { q := q/2
      qlen := qlen + q
    }
  }
//writef("qlen=%n*n", qlen)
  RESULTIS qlen
}

AND rdnoteprim() = VALOF
{ // Read in a note item up to its end or a dyadic operator
  // such as \vol or \tuplet
  LET op, ln = token, tokln
  LET a, b = 0, 0

//writef("rdnoteprim: op=%s  <%n/%n>*n", opstr(op), fno(ln), lno(ln))
  SWITCHON op INTO
  { DEFAULT:
      RESULTIS 0

    CASE s_num: // An octave number
    { LET octaveno = FIX numval
      //writef("rdnoteprim: numval= %9.3f*n", numval)
      UNLESS 0<=octaveno<=9 DO
        synerr("Bad octave number %n", octaveno)
      prevoctave := octaveno
      prevnoteletter := 'f' // So C to B are all in the same octave 
      lex()
      RESULTIS rdnoteprim()
    }

    CASE s_lparen: // [-, Noteseq, ln, notelist, qlen]
      lex()
      a := rdnotelist()
      checkfor(s_rparen, "Syntax error in ( ... ) construct")
      a := mk5(0, s_noteseq, ln, a, -1)

      IF optStrace DO
        writef("%i6 -> [-, Noteseq, %n:%n, %n, -1]*n",
                a,
                opstr(h2!a),
                fno(ln), lno(ln),
                h4!a)

      RESULTIS a

    CASE s_lcurly: // [-, Blockseq, ln, notelist, qlen]
      lex()
      a := rdnoteseq()
      checkfor(s_rcurly, "Syntax error in { ... } construct")
      a := mk5(0, s_blockseq, ln, a, -1)

      IF optStrace DO
        writef("%i6 -> [-, Blockseq, %n:%n, %n, -1]*n",
                a,
                fno(ln), lno(ln),
                h4!a)

      RESULTIS a

    CASE s_lsquare: // [-, Par, ln, parlist, qlen]
                    // parlist -> [parlist, Noteseq, ln, notelist, qlen]
                    // or      = 0
      lex()
      a := rdparlist()
      checkfor(s_rsquare, "Syntax error in [ ... ] construct")
      a := mk5(0, s_par, ln, a, -1)
      IF optStrace DO
        writef("%i6 -> [-, Par, %n:%n, %n, -1]*n",
                a,
                fno(ln), lno(ln),
                h4!a)

      RESULTIS a

    CASE s_note:     // [-, Note,     ln, <letter,sharps,n>, qlen]
    CASE s_notetied: // [-, Notetied, ln, <letter,sharps,n>, qlen]
    { // Calculate the note number
      LET tab1 = TABLE // Octave number change table
                 //  A  B  C  D  E  F  G        -- previous note
                     0, 0,-1,-1, 0, 0, 0,  // A -- new notes
                     0, 0,-1,-1,-1, 0, 0,  // B
                     1, 1, 0, 0, 0, 0, 1,  // C
                     1, 1, 0, 0, 0, 0, 0,  // D
                     0, 1, 0, 0, 0, 0, 0,  // E
                     0, 0, 0, 0, 0, 0, 0,  // F
                     0, 0,-1, 0, 0, 0, 0   // G
      LET tab2 = TABLE // Semitones away from C in same C-B octave
                 //  A  B  C  D  E  F  G
                     9,11, 0, 2, 4, 5, 7
      LET i = noteletter-'a'
      LET j = prevnoteletter-'a'
      prevnoteletter := noteletter

      // Deal with the octave correction

      prevoctave := prevoctave +    // octave of previous note
                    reloctave  +    // count of 's and ,s
                    tab1!(7*i + j)  // letter change correction

      // Calculate the midi note number (untransposed)
      notenumber := (prevoctave+1)*12 + tab2!i + notesharps
//writef("notenumber=%n*n", notenumber)

      UNLESS 0<=notenumber<=127 DO
        synerr("Note %n out of range", notenumber)

      a := mk5(0, op, ln,
               noteletter<<16 | (notesharps&255)<<8 | notenumber,
               note2qlen(notelengthnum, prevlengthnum, dotcount))
      IF optStrace DO
        writef("%i6 -> [-, %s, %n:%n, <%c:%n:%n>, %n]*n",
                a,
                opstr(h2!a),
                fno(ln), lno(ln),
                noteletter, notesharps, notenumber,
                h5!a)

      IF notelengthnum>=0 DO prevlengthnum := notelengthnum
      lex()
      RESULTIS a
    }

    CASE s_rest:  // [-, Rest,  ln, qlen]
    CASE s_space: // [-, Space, ln, qlen]
      a := mk4(0, op, ln, note2qlen(notelengthnum, prevlengthnum, dotcount))
      IF optStrace DO
        writef("%i6 -> [-, %s, %n:%n, %n]*n",
                a,
                opstr(h2!a),
                fno(ln), lno(ln),
                h4!a)
      IF notelengthnum>=0 DO prevlengthnum := notelengthnum
      lex()
      RESULTIS a

    CASE s_null: // [-, Null, ln, qlen=0]
      a := mk4(0, op, ln, 0)
      IF optStrace DO
        writef("%i6 -> [-, Null, %n:%n, 0]*n",
                a,
                fno(ln), lno(ln))

      lex()
      RESULTIS a

    CASE s_barline:            // All yield [-, op, ln]
    CASE s_doublebar:
    CASE s_repeatback:
    CASE s_repeatforward:
    CASE s_repeatbackforward:
    CASE s_trebleclef:
    CASE s_altoclef:
    CASE s_tenorclef:
    CASE s_bassclef:
    CASE s_varvol:
    CASE s_nonvarvol:
      a := mk3(0, op, ln)
      IF optStrace DO
        writef("%i6 -> [-, %s, %n:%n]*n",
                a,
                opstr(h2!a),
                fno(ln), lno(ln))
      lex()
      RESULTIS a

    CASE s_control: // [-, Control, ln, controller-no, val]
                    // Corresponds to Midi: Bn <controller no> <val>
    CASE s_timesig: // [-, Timesig, ln, <int>, <int>]
    CASE s_bank:    // [-, Bank, ln, int, int]
      lex()
      checkfor(s_lparen, "'(' expected")
      a := rdinteger()
      b := rdinteger()
      checkfor(s_rparen, "')' expected")
      a := mk5(0, op, ln, a, b)
      IF optStrace DO
        writef("%i6 -> [-, %s, %n:%n, %n, %n]*n",
                a,
                opstr(h2!a),
                fno(ln), lno(ln),
                h4!a,
                h5!a)
      RESULTIS a

    CASE s_patch:   // [-, Patch, ln, int]
      lex()
      a := rdinteger()
      a := mk4(0, op, ln, a)
      IF optStrace DO
        writef("%i6 -> [-, Patch, %n:%n, %n]*n",
                a,
                fno, lno,
                h4!a)
      RESULTIS a

    CASE s_keysig:  // [-, keysig, ln, note, maj-min]
    { LET plet, poct, plen = prevnoteletter, prevoctave, prevlengthnum
      lex()
      checkfor(s_lparen, "'(' expected")
      a := rdnoteprim()
      UNLESS a & h2!a=s_note DO synerr("Note expected")
      UNLESS token=s_major | token=s_minor DO
        synerr("\major or \minor expected")
      a := mk5(0, op, ln, a, token)
      IF optStrace DO
        writef("%i6 -> [-, %s, %n:%n, %n, %s]*n",
                a,
                opstr(h2!a),
                fno(ln), lno(ln),
                h4!a,
                opstr(h5!a))
      lex()
      prevnoteletter, prevoctave, prevlengthnum := plet, poct, plen
      checkfor(s_rparen, "')' expected")
      RESULTIS a
    }

    CASE s_transposition: // [-, Transposition, ln, semitones-up]
    { LET plet, poct, plen = prevnoteletter, prevoctave, prevlengthnum
      LET semitones = 0
      lex()
      checkfor(s_lparen, "'(' expected")
      UNLESS token=s_note DO synerr("Note expected")
      //    note => semitones up

      // c'      12        c       0
      // b'      11        b      -1
      // bes'    10        bes    -2
      // a'       9        a      -3
      // aes'     8        aes    -4
      // g'       7        g      -5
      // ges'     6        ges    -6
      // f        5        f,     -7
      // e        4        e,     -8
      // ees      3        ees,   -9
      // d        2        d,    -10
      // des      1        des,  -11
      // c        0        c,    -12

      //                                   A  B  C  D  E  F  G
      semitones := (noteletter-'a')!TABLE -3,-1, 0, 2, 4, 5,-5

      // Deal with the accidentals, if any
      UNLESS -1<=notesharps<=1 DO synerr("Too many accidentals")
      semitones := semitones + notesharps 

      // Correct the octave
      semitones := semitones + 12*reloctave 

      //writef("transposition: %c sharps=%n reloctave=%n => semitones=%n*n",
      //        noteletter, notesharps, reloctave, semitones)
      a := mk4(0, op, ln, semitones)
      IF optStrace DO
        writef("%i6 -> [-, Transposition, %n:%n, %n]*n",
                a,
                fno,lno,
                h4!a)
      lex()
      checkfor(s_rparen, "')' expected, token=%s", opstr(token))
      prevnoteletter, prevoctave, prevlengthnum := plet, poct, plen
      RESULTIS a
    }

    CASE s_pedoff:   // All [-, op, ln]
    CASE s_pedoffon:
    CASE s_pedon:
    CASE s_portaoff:
    CASE s_portaon:
    CASE s_softoff:
    CASE s_softon:
      lex()
      a := mk3(0, op, ln)
      IF optStrace DO
        writef("%i6 -> [-, %n:%n, %s]*n",
                a,
                opstr(h2!a),
                fno(ln), lno(ln))
      RESULTIS a

    CASE s_volmap: // [-, op, ln, shape_list]
      lex()
      a := mk4(0, op, ln, rdshapeseq())
      IF optStrace DO
        writef("%i6 -> [-, Volmap, %n:%n, %n]*n",
                a,
                opstr(h2!a),
                fno(ln), lno(ln),
                h4!a)
      RESULTIS a

    CASE s_name:               // All [-, op, ln, string]
    CASE s_instrumentname:
    CASE s_instrumentshortname:
    CASE s_instrument:
    CASE s_partlabel:
    CASE s_barlabel:
    CASE s_title:
    CASE s_composer:
    CASE s_arranger:
    CASE s_opus:
//writef("rdnoteprim: token=%s*n", opstr(op))
      lex()
      a := mk4(0, op, ln, rdstring())
      IF optStrace DO
        writef("%i6 -> [-, %s, %n:%n, *"%s*"]*n",
                a,
                opstr(h2!a),
                fno(ln), lno(ln),
                h4!a)
      RESULTIS a

  }
}

AND rdshapeseq() = VALOF
// This reads the shape data that occurs as the right hand operand of
// any shape operator such as \vol or \tempo. It readd a shape item or
// a list of shape items enclosed in parentheses.
// It return a pointer -> [-, Shapeseq, ln, shapelist, qlen]
// where shapeseg is a list of value and space items chained together by
// their link fields. A shape value is one of the following

//          [-, Num,      ln, value]
//          [-, Star,     ln]
//          [-, Space,    ln, qlen]
//          [-, Rest,     ln, qlen]
//          [-, Null,     ln,    0]

// where value is a floating point number.

// Shape values are scaled by sfac/1024. So, for instance,
// if sfac=512 (corresponding to an eigth note or quaver), a tempo
// value of 120 would be halved giving a rate of 60 quarter notes
// per minute. sfac can be changed within a shape sequence by
// items such as :256 or :s8
// The main purpose of scaling is to allow, for instance, dotted
// quaver = 138 to be specified by \tempo(:s8. 138). It is typically
// not used with any other shape operator.

{ LET list = 0           // For the chain of note items
  LET liste = @list
  LET firsttoken = token // To remember whether it was Lparen.
  LET shapeseqln = tokln
  LET fno1 = tokln>>20
  LET lno1 = tokln & #xFFFFF
  LET item = 0
  LET sfac = 1024

  LET prevlen = prevlengthnum  // Save the the previous length number.
  prevlengthnum := 4           // Set the prev length number to 4 (a quarter note)

  IF token=s_lparen DO lex() // Skip over '(' if present.

  { // Start of loop to read shape items.
    LET ln = tokln

    SWITCHON token INTO
    { DEFAULT:
        BREAK

      CASE s_space:
      CASE s_rest:
      CASE s_null:
      { LET qlen = 0
        UNLESS token=s_null DO
        { qlen := note2qlen(notelengthnum, prevlengthnum, dotcount)
          IF notelengthnum>=0 DO prevlengthnum := notelengthnum
        }
        item := mk4(0, s_space, tokln, qlen)
        IF optStrace DO
          writef("%i6 -> [-, %s, %n:%n, %n]*n",
                item,
                opstr(h2!item),
                fno(ln), lno(ln),
                h4!item)
        !liste := item
        liste := item
        lex()
        IF firsttoken=s_lparen LOOP
        BREAK
      }

      CASE s_colon:
        lex()
        IF token=s_space | token=s_rest DO // eg  :s8.
        { sfac := note2qlen(notelengthnum, 4, dotcount)
          lex()
          LOOP
        }

        IF token=s_num DO  // eg :1536
        { sfac := FIX numval
//writef("rdshapeseq: sfac=%n*n", sfac) 
          lex()
          LOOP
        }

        synerr("'s' or an integer expected after ':' in shape sequence")
        LOOP

      CASE s_num:
        numval := muldiv(numval, sfac, 1024) // Apply the current scaling value.
        item := mk4(0, token, tokln, numval)
        IF optStrace DO
          writef("%i6 -> [-, %s, %n:%n, %13.6f]*n",
                item,
                opstr(h2!item),
                fno(ln), lno(ln),
                h4!item)
        !liste := item
        liste := item
        lex()
        IF firsttoken=s_lparen LOOP
        BREAK

      CASE s_star:
        item := mk3(0, token, tokln)
        // Star items are not scaled by sfac.
        IF optStrace DO
        { writef("%i6 -> [-, %s, %n:%n]*n",
                  item,
                  opstr(h2!item),
                  fno(ln), lno(ln))
        }
        !liste := item
        liste := item
        lex()
        IF firsttoken=s_lparen LOOP
        BREAK
    }
  } REPEAT

  IF firsttoken=s_lparen DO
  { UNLESS token=s_rparen DO
      synerr("Missing ')' in a shape sequence")
    lex()
  }

  prevlengthnum := prevlen
  item := mk5(0, s_shapeseq, shapeseqln, list, -1)
  IF optStrace DO
    writef("%i6 -> [-, Shapeseq, %n:%n, %n, -1]*n",
            item,
            fno(shapeseqln), lno(shapeseqln),
            h4!item)
  RESULTIS item
}

AND rdtupletqlen() = VALOF
{ // This reads the right hand operand of \tuple
  // Syntactically this operand is either
  // a space item or
  // a list of space items enclosed in parentheses.
  // The result is the qlen of the operand.

  LET qlen = 0
  LET firsttoken = token // To remember whether it was Lparen.
  LET prevlen = prevlengthnum  // Save the current value of prevlengthnum.
  prevlengthnum := 4     // Assume the prev length number was 4.

  IF token=s_lparen DO lex()   // Skip over '(' if present.

  WHILE token=s_rest | token=s_space DO
  { qlen := qlen + note2qlen(notelengthnum, prevlengthnum, dotcount)
    IF notelengthnum>=0 DO prevlengthnum := notelengthnum
    lex()
    UNLESS firsttoken=s_lparen BREAK
  }

  IF firsttoken=s_lparen DO
    checkfor(s_rparen, "Missing ')' in a shape sequence")

  // Check that qlen is valid.
  UNLESS qlen>0 DO
    synerr("A \tuplet qlen must be greater than zero")
  prevlengthnum := prevlen

  RESULTIS qlen
}

AND rdnoteitem() = VALOF
{ // Return the parse tree of a note item or zero if none found.

  LET a = rdnoteprim() // Read a note up to the first dyadic
                       // operator, if any.

  UNLESS a RESULTIS 0

  { // Look for dyadic operators such as \tuplet or \vol
    LET op = token
    LET ln = tokln          // Lineno of the operator if any.

    SWITCHON op INTO
    { DEFAULT:
        RESULTIS a

      // Infixed operators with a shape as second operand.
      CASE s_vibrate:
      CASE s_vibrateadj:
      CASE s_vibamp:
      CASE s_vibampadj:
      CASE s_vol:
      CASE s_voladj:
      CASE s_tempo:
      CASE s_tempoadj:
      CASE s_legato:
      CASE s_legatoadj:
      CASE s_delay:
      CASE s_delayadj: // [-, op, ln, noteseq, shapeseq, absqlen]
        lex()
        UNLESS h2!a=s_noteseq DO
        { // Make a Noteseq node if necessary.
          a := mk5(0, s_noteseq, ln, a, -1)
          // a -> [-, Noteseq, ln, notelist, qlen]
          IF optStrace DO
            writef("%i6 -> [-, Noteseq, %n:%n, %n, -1]*n",
                    a,
                    fno(ln), lno(ln),
                    h4!a)
        }
        a := mk6(0, op, ln, a, rdshapeseq(), -1)
        // a-> [-, op, ln, noteseq, shapseq, qlen]
        IF optStrace DO
          writef("%i6 -> [-, Noteseq, %n:%n, %n, %n, -1]*n",
                  a,
                  fno(ln), lno(ln),
                  h4!a,
                  h5!a)
        LOOP

      CASE s_tuplet: // [-, Tuplet, ln, noteseq, qlen]
                     // eg (C D E)\tup s4
                     // or (C D E)\tup 1024
                     // or (C D E)\tup(s8. s.) 
        lex()
        a := mk5(0, s_noteseq, tokln, a, -1)
        IF optStrace DO
          writef("%i6 -> [-, Noteseq, %n:%n, %n, -1]*n",
                  a,
                  fno(ln), lno(ln),
                  h4!a)
        a := mk5(0, op, ln, a, rdtupletqlen())
        IF optStrace DO
          writef("%i6 -> [-, Tuplet, %n:%n, %n, %n]*n",
                  a,
                  fno(ln), lno(ln),
                  h4!a,
                  h5!a)
        LOOP
    }
  } REPEAT

  RESULTIS 0
}

AND rdparlist() = VALOF
{ // This reads a list of noteseqs, hopefully terminated by ']'
  // It returns
  // a pointer -> [parlist, Noteseq, ln, noteseq]
  // or zero
  LET noteseqlist = 0
  LET noteseqliste = @ noteseqlist

  { LET a = rdnoteseq()
    // a -> [-, Noteseq, ln, notelist, qlen]
    // or a=0
    UNLESS a BREAK
    IF containsshapeops(a) DO
    { // This component of a Par construct contains shape operators
      // so must be converted into a block.
      LET ln = h3!a
      a := mk5(0, s_block, ln, a, -1)
      IF optStrace DO
        writef("%i6 -> [-, Block, %n:%n, %n, -1]*n",
                a,
                fno(ln), lno(ln),
                h4!a)
    }
    !noteseqliste := a
    noteseqliste := a
  } REPEAT

  RESULTIS noteseqlist
}

AND containsshapeops(t) = VALOF
{ // t points to any node in the parse tree.
  // Return TRUE if t contains a shape operator node not
  // protected by a block.
  LET op = h2!t  // The tree node operator
  LET ln = h3!t  // The lineno value

writef("containsshapeops: t=%n op=%s ", t, opstr(op)); prlineno(ln); newline()
//abort(1000)
  SWITCHON op INTO // All possible parse tree items.
  { DEFAULT:
    CASE s_score:
    CASE s_conductor:  // t -> [-. Conductor, ln, noteseq, qlen]
    CASE s_part:       // t -> [-. Part,      ln, noteseq, qlen]
    CASE s_solo:       // t -> [-. Solo,      ln, noteseq, qlen]
    CASE s_shapeseq:      // t -> [-, Shapeseq, ln, notelist, qlen]
      writef("System error in containsshapeops, t=%n unexpected op = %s*n",
             t, opstr(op))
      abort(999)
      RESULTIS 0

    CASE s_name:
    CASE s_num:
    CASE s_null:
    CASE s_barline:
    CASE s_doublebar:
    CASE s_par:
    CASE s_block:
    CASE s_partset:
    CASE s_rest:
    CASE s_space:
      RESULTIS FALSE

    CASE s_tuplet:     // t -> [-, Tuplet, ln, noteseq,  qlen]
      RESULTIS containsshapeops(h4!t)

    CASE s_delay:      // t -> [-, delay,     ln, noteseq, shapeseq, qlen]
    CASE s_delayadj:   // t -> [-, delayadj,  ln, noteseq, shapeseq, qlen]
    CASE s_legato:     // t -> [-, legato,    ln, noteseq, shapeseq, qlen]
    CASE s_legatoadj:  // t -> [-, legatoadj, ln, noteseq, shapeseq, qlen]
    CASE s_tempo:      // t -> [-, tempo,     ln, noteseq, shapeseq, qlen]
    CASE s_tempoadj:   // t -> [-, tempoadj,  ln, noteseq, shapeseq, qlen]
    CASE s_vibrate:    // t -> [-, vibrate,   ln, noteseq, shapeseq, qlen]
    CASE s_vibrateadj: // t -> [-, vibrateadj,ln, noteseq, shapeseq, qlen]
    CASE s_vibamp:     // t -> [-, vibamp,    ln, noteseq, shapeseq, qlen]
    CASE s_vibampadj:  // t -> [-, vibampadj, ln, noteseq, shapeseq, qlen]
    CASE s_vol:        // t -> [-, vol,       ln, noteseq, shapeseq, qlen]
    CASE s_voladj:     // t -> [-, voladj,    ln, noteseq, shapeseq, qlen]
      RESULTIS TRUE

    CASE s_noteseq:       // t -> [-, Noteseq,  ln, notelist, qlen]
    { LET list = h4!t
      WHILE list DO
      { IF containsshapeops(list) RESULTIS TRUE
        list := !list
      }
      RESULTIS FALSE
    }
  }
}

AND rdnoteseq() = VALOF
{ // This returns a pointer to [-, Noteseq, ln, notelist, qlen]
  // where notelist is a list of note items linked though the
  // h1 field.
  LET ln = tokln  // The line number of the first token of the note list.
  LET a = rdnotelist()

  UNLESS h2!a=s_noteseq DO
  { a := mk5(0, s_noteseq, ln, a, -1)

    IF optStrace DO
      writef("%i6 -> [-, Noteseq, %n:%n, %n, -1]*n",
                    a,
                    fno(ln), lno(ln),
                    h4!a)
  }

  RESULTIS a
}

AND rdnotelist() = VALOF
// This returns a list of note items linked though the
// h1 field.
{ LET list = 0
  LET liste = @list

  // Setup new recovery point
  LET oldp, oldl = rec_p, rec_l
  rec_p, rec_l := level(), sw

sw:
  { LET a = rdnoteitem()
    UNLESS a BREAK
    !liste := a   // Append the latest note item.
    liste := a
  } REPEAT

  rec_p, rec_l := oldp, oldl
  RESULTIS list  // return the list of note items
}

LET fno(ln) = ln>>20
AND lno(ln) = ln & #xFFFFF

LET opstr(op) = VALOF SWITCHON op INTO
{ DEFAULT:        sawritef("opstr: System error, op %n*n", op)
//abort(1000)
                  RESULTIS "Unknown"

  CASE s_altoclef:            RESULTIS "Altoclef"
  CASE s_arranger:            RESULTIS "Arranger"
  CASE s_bank:                RESULTIS "Bank"
  CASE s_barlabel:            RESULTIS "Barlabel"
  CASE s_barline:             RESULTIS "Barline"
  CASE s_bassclef:            RESULTIS "Bassclef"
  CASE s_colon:               RESULTIS "Colon"
  CASE s_composer:            RESULTIS "Composer"
  CASE s_conductor:           RESULTIS "Conductor"
  CASE s_control:             RESULTIS "Control"
  CASE s_delay:               RESULTIS "Delay"
  CASE s_delayadj:            RESULTIS "Delayadj"
  CASE s_doublebar:           RESULTIS "Doublebar"
  CASE s_eof:                 RESULTIS "Eof"
  CASE s_instrument:          RESULTIS "Instrument"
  CASE s_instrumentname:      RESULTIS "Instrumentname"
  CASE s_instrumentshortname: RESULTIS "Instrumentshortname"
  CASE s_keysig:              RESULTIS "Keysig"
  CASE s_lcurly:              RESULTIS "Lcurly"
  CASE s_legato:              RESULTIS "Legato"
  CASE s_legatoadj:           RESULTIS "Legatoadj"
  CASE s_noteseq:             RESULTIS "Noteseq"
  CASE s_shapeseq:            RESULTIS "Shapeseq"
  CASE s_lparen:              RESULTIS "Lparen"
  CASE s_lsquare:             RESULTIS "Lsquare"
  CASE s_major:               RESULTIS "Major"
  CASE s_minor:               RESULTIS "Minor"
  CASE s_msecsmap:            RESULTIS "Msecsmap"
  CASE s_name:                RESULTIS "Name"
  CASE s_neg:                 RESULTIS "Neg"
  CASE s_nonvarvol:           RESULTIS "Nonvarvol"
  CASE s_note:                RESULTIS "Note"
  CASE s_notetied:            RESULTIS "Notetied"
  CASE s_null:                RESULTIS "Null"
  CASE s_num:                 RESULTIS "Num"
  CASE s_opus:                RESULTIS "Opus"
  CASE s_par:                 RESULTIS "Par"
  CASE s_part:                RESULTIS "Part"
  CASE s_partset:             RESULTIS "Partset"
  CASE s_partlabel:           RESULTIS "Partlabel"
  CASE s_patch:               RESULTIS "Patch"
  CASE s_pedoff:              RESULTIS "Pedoff"
  CASE s_pedoffon:            RESULTIS "Pedoffon"
  CASE s_pedon:               RESULTIS "Pedon"
  CASE s_portaon:             RESULTIS "Portaon"
  CASE s_portaoff:            RESULTIS "Portaoff"
  CASE s_rcurly:              RESULTIS "Rcurly"
  CASE s_repeatback:          RESULTIS "Repeatback"
  CASE s_repeatbackforward:   RESULTIS "Repeatbackforward"
  CASE s_repeatforward:       RESULTIS "Repeatforward"
  CASE s_rest:                RESULTIS "Rest"
  CASE s_rparen:              RESULTIS "Rparen"
  CASE s_rsquare:             RESULTIS "Rquare"
  CASE s_score:               RESULTIS "Score"
  CASE s_solo:                RESULTIS "Solo"
  CASE s_space:               RESULTIS "Space" 
  CASE s_star:                RESULTIS "Star"
  CASE s_string:              RESULTIS "String"
  CASE s_softon:              RESULTIS "Softon"
  CASE s_softoff:             RESULTIS "Softoff"
  CASE s_tempo:               RESULTIS "Tempo"
  CASE s_tempoadj:            RESULTIS "Tempoadj"
  CASE s_tenorclef:           RESULTIS "Tenorclef"
  CASE s_timesig:             RESULTIS "Timesig"
  CASE s_title:               RESULTIS "Title"
  CASE s_transposition:       RESULTIS "Transposition"
  CASE s_trebleclef:          RESULTIS "Trebleclef"
  CASE s_tuplet:              RESULTIS "Tuplet"
  CASE s_varvol:              RESULTIS "Varvol"
  CASE s_vibrate:             RESULTIS "Vibrate"
  CASE s_vibrateadj:          RESULTIS "Vibrateadj"
  CASE s_vibamp:              RESULTIS "Vibamp"
  CASE s_vibampadj:           RESULTIS "Vibampadj"
  CASE s_vol:                 RESULTIS "Vol"
  CASE s_voladj:              RESULTIS "Voladj"
  CASE s_volmap:              RESULTIS "Volmap"
}

AND prnote(letter, sharps, note, qlen) BE
{ // If qbeats<0 just output the note letter possibly followed by # or b
  // otherwise output the octave number, note letter, sharps and flats, and
  // the length in qbeats.
  LET count = 0
  LET n = sharps&255     // Sharps
  // Sign extend n
  IF n>128 DO n := n-256 // Flats

  // Cause 4Ces (note 59) to print as 4Cb not 3Cb
  // Cause 3Bis (note 60) to print as 3B# not 4B#

  IF qlen>=0 DO
  { IF note DO writef("%n=", note)
    TEST note>=12   // Write the octave number
    THEN writef("%n", (note-n)/12-1)
    ELSE writef("-")
  }
  wrch(letter+'A'-'a')
  FOR i = 1 TO n  DO { wrch('#'); count := count+1 }
  FOR i = n TO -1 DO { wrch('b'); count := count+1 }
  IF qlen>=0 DO
    writef("   qlen=%n", qlen)
}

LET prtree(t, n, d) BE
{ // This prints the parse tree of a MUS score program.
  // t is either zero or points to a node [next, op, ln, ...]
  //   next is used to chain nodes together as in note or
  //      shape sequences.
  //   op is the node operator, eg s_noteseq, s_note etc.
  //   ln is the lineno value for this node.
  //   The other fields are node dependent.
  // n is the depth of node t.
  // d is the maximum depth that prtree will print.
  LET v = TABLE 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  LET op, ln, a1, a2 = ?, ?, ?, ?
  LET opname = ?

writef("%n: ", t)
  IF n>=d DO { writes("Etc"); RETURN  }
  IF t=0  DO { writes("Nil"); RETURN  }

  op, ln, a1, a2 := h2!t, h3!t, h4!t, h5!t
  opname := opstr(op)

  SWITCHON op INTO
  { DEFAULT:
         writef("%t8   ", opname)
         prlineno(ln)
         ENDCASE

    CASE s_num:      writef("%t8 %13.6f   ", opname, a1); prlineno(ln); RETURN

    CASE s_star:     writef("%t8   ", opname); prlineno(ln); RETURN

    CASE s_note:     // [-, Note,     ln, <letter,sharps,note>, qlen]
    CASE s_notetied: // [-, Notetied, ln, <letter,sharps,note>, qlen]
    { LET letter =   h4!t>>16
      LET sharps =  (h4!t>>8) & 255
      LET note   =   h4!t & 255      // MIDI number
      LET qlen   =   h5!t            // Note qlen of a quarter note = 1024
      writef("%t8 ", opname)
      prnote(letter, sharps, note, qlen)
      writef("   "); prlineno(ln)
      RETURN
    }

    CASE s_rest:    // [-, Rest,  ln, qlen]
    CASE s_space:   // [-, Space, ln, qlen]
      writef("%t7 qlen=%n   ", opname, a1); prlineno(ln)
      RETURN

    CASE s_null:   // [-, null, ln]
      writef("%s ", opname); prlineno(ln)
      RETURN

    CASE s_control:       writef("Control (%n %n)", h4!t, h5!t);  RETURN
    CASE s_timesig:       writef("Timesig (%n %n)", h4!t, h5!t);  RETURN

    CASE s_bank:          writef("Bank    (%n %n)", h4!t, h5!t);  RETURN

    CASE s_patch:         writef("Patch   %n", h4!t);           RETURN

    CASE s_transposition: writef("Transposition (%n)", h4!t);   RETURN

    CASE s_keysig:
      // [-, keysig, ln, [-, note, ln, <letter, sharps, noteno>, mode]
      writef("Keysig (")
      prnote(h4!a1>>16, (h4!a1>>8) & 255, 0, -1)
      TEST a2=s_major THEN writes(" Major)")
                      ELSE writes(" Minor)")
      RETURN

    // Operator with a string argument
    CASE s_title:
    CASE s_composer:
    CASE s_arranger:
    CASE s_opus:
    CASE s_instrument:
    CASE s_name:                 // eg Piano LH
    CASE s_instrumentname:       // eg Flute
    CASE s_instrumentshortname:
    CASE s_barlabel:
    CASE s_partlabel:
      writef("%t7 *"%s*"    ", opname, a1); prlineno(ln)
      RETURN

    CASE s_partset:   // [-, Partset,   ln, partlist, qlen]
    CASE s_blockseq:  // [-, Blockseq,  ln, notelist, qlen]
    CASE s_noteseq:   // [-, Noteseq,   ln, notelist, qlen]
    CASE s_shapeseq:  // [-, Shapeseq,  ln, shapelist, qlen]
    CASE s_conductor: // [-, Conductor, ln, noteseq, qlen]
    CASE s_part:      // [-, Part,      ln, noteseq, qlen]
    CASE s_solo:      // [-, Solo,      ln, noteseq, qlen]
    CASE s_par:       // [-, Par,       ln, parlist, qlen]
      writef("%t7 qlen=%n   ", opname, h5!t)
      prlineno(ln)

      // Print each item in the list as though they were
      // operands of the current operator.

      WHILE a1 DO
      { newline()
        FOR j=0 TO n-1 DO writes( v!j )
        writes("**-")
        v!n := !a1 ->"! ","  "
        prtree(a1, n+1, d)
        a1 := !a1
      }
      RETURN

    CASE s_score:     // [-, Score, ln, name, conductor, partset, qlen]
      writef("%s *"%s*" qlen=%n   ", opname, h4!t, h7!t); prlineno(ln); newline()
      FOR j = 0 TO n-1 DO writes( v!j )
      writes("**-")
      v!n := "! "
      prtree(h5!t, n+1, d)                // The conductor
      newline()
      FOR j = 0 TO n-1 DO writes( v!j )
      writes("**-")
      v!n := "  "
      prtree(h6!t, n+1, d)                // The partset
      RETURN       

    // Operators that perform qbeat scaling of shape data.
    CASE s_vibrate:    // [-, Vibrate,    ln, noteseq, shapeseq, qlen]
    CASE s_vibrateadj: // [-, Vibrateadj, ln, noteseq, shapeseq, qlen]
    CASE s_vibamp:     // [-, vibadj,     ln, noteseq, shapeseq, qlen]
    CASE s_vibampadj:  // [-, Vibampadj,  ln, noteseq, shapeseq, qlen]
    CASE s_vol:        // [-, Vol,        ln, noteseq, shapeseq, qlen]
    CASE s_voladj:     // [-, Voladj,     ln, noteseq, shapeseq, qlen]
    CASE s_tempo:      // [-, Temp,       ln, noteseq, shapeseq, qlen]
    CASE s_tempoadj:   // [-, tempadj,    ln, noteseq, shapeseq, qlen]
    CASE s_legato:     // [-, Legato,     ln, noteseq, shapeseq, qlen]
    CASE s_legatoadj:  // [-, Legatoadj,  ln, noteseq, shapeseq, qlen]
    CASE s_delay:      // [-, Delay,      ln, noteseq, shapeseq, qlen]
    CASE s_delayadj:   // [-, Delayadj,   ln, noteseq, shapeseq, qlen]
      writef("%s   qlen=%n   ", opname, h6!t); prlineno(ln); newline()
      FOR j = 0 TO n-1 DO writes( v!j )
      writes("**-")
      v!n := "! "
      prtree(a1, n+1, d)
      newline()
      FOR j = 0 TO n-1 DO writes( v!j )
      writes("**-")
      v!n := "  "
      prtree(a2, n+1, d)
      RETURN       

    CASE s_volmap: // [-, op, ln, shapeseq]
      writef("%s   ", opname); prlineno(ln); newline()
      FOR j = 0 TO n-1 DO writes( v!j )
      writes("**-")
      v!n := "  "
      prtree(a1, n+1, d)
      RETURN       

    CASE s_tuplet: // [-, Tuplet, ln, noteseq, qlen]
                   // eg         (4C4 D E)\tup S4
                   // previously S4\tup(4C4 D E)
      writef("%s   qlen=%n  ", opname, h5!a1); prlineno(ln)
      newline()
      FOR j = 0 TO n-1 DO writes( v!j )
      writes("**-")
      v!n := "  "
      prtree(a1, n+1, d)
      RETURN       
  }
}

.

SECTION "Trans"

// This section contains functions concerned with the translation
// of the parse tree to the linked list of midi events.

GET "libhdr"
GET "playmus.h"

/*
The parse tree structure is given below.  This is the definitive
structure whose rules are copied as comments in the functions
concerned with the various operators.

They use the following notational conventions. 

[ item, item, ... item ] represents a node containing 32 bit elements.
- The first item in a node is always either zero or a pointer to the
  the next node in a list.
Words starting with a capital better such as Scoreseq or Tempo are
  integers specifying the type of node. In the BCPL code manifest
  constants such as s_scoreseq ot s_tempo are used.
Words in lower case such as noteseq or shapelist specify kinds of
  values.
qlen is an integer giving the length of the construct in qbeats
  with 1024 being the number of qbeats in a quarter note (or crotchet).
  Its value is -1 when unset.
ln is a linenumber value consisting of a file number (fno>0) followed
  by 20 bits of line number (lno>0). Its value is (fno<<20)+lno.
msecs is an integer representing a time in milli seconds. Its value
  is -1 when unset.
The definitions of lower case words are as follows.

score -> [-, Score, ln, name, conductor, partset, qlen]

conductor -> [-, Conductor, ln, notelist, qlen]

partset => [-, Partset, ln, partlist, qlen]

partlist =  0
or       -> [partlist, Part, ln, notelist, qlen]
or       -> [partlist, Solo, ln, notelist, qlen]

notelist =  0
         -> [notelist, Note,     ln, letter:sharps:note, qlen]
         -> [notelist, Notetied, ln, letter:sharps:note, qlen]
         -> [notelist, Rest, ln, qlen]
         -> [notelist, Space, ln, qlen]
         -> [notelist, Null, ln]
         -> [notelist, Control, ln, int1, int2]
         -> [notelist, Timesig, ln, int1, int2]
         -> [notelist, Bank, ln, int1, int2]
         -> [notelist, Patch, ln, int]
         -> [notelist, Transposition, ln, int]
         -> [notelist, Keysig, ln, letter:sharps:note, mode]
         -> [notelist, Title, ln, string]
         -> [notelist, Composer, ln, string]
         -> [notelist, Arranger, ln, string]
         -> [notelist, Opus, ln, string]
         -> [notelist, Instrument, ln, string]
         -> [notelist, Name, ln, string]
         -> [notelist, Instrumentname, ln, string]
         -> [notelist, Instrumentshortname, ln, string]
         -> [notelist, Barlabel, ln, string]
         -> [notelist, Partlabel, ln, string]
         -> [notelist, Blockseq, ln, notelist, qlen]
         -> [notelist, Par, ln, parlist, qlen]
         -> [notelist, Vibrate, ln, noteseq, shapeseq, qlen]
         -> [notelist, Vibrateadj, ln, noteseq, shapeseq, qlen]
         -> [notelist, Vibampj, ln, noteseq, shapeseq, qlen]
         -> [notelist, Vibampadj, ln, noteseq, shapeseq, qlen]
         -> [notelist, Vol, ln, noteseq, shapeseq, qlen]
         -> [notelist, Voladj, ln, noteseq, shapeseq, qlen]
         -> [notelist, Tempo, ln, noteseq, shapeseq, qlen]
         -> [notelist, Tempoadj, ln, noteseq, shapeseq, qlen]
         -> [notelist, Legato, ln, noteseq, shapeseq, qlen]
         -> [notelist, Legatoadj, ln, noteseq, shapeseq, qlen]
         -> [notelist, Delay, ln, noteseq, shapeseq, qlen]
         -> [notelist, Delayadj, ln, noteseq, shapeseq, qlen]
         -> [notelist, Volmap, ln, shapeseq]
         -> [notelist, Tuplet, ln, noteseq, qlen]

noteseq => [-, Noteseq, ln, notelist, qlen]



shapeseq => [-, Shapeseq, ln, shapelist, qlen]

shapelist = 0
         -> [shapelist, Num,      ln, value]
         -> [shapelist, Star,     ln]
         -> [shapelist, Space,    ln, qlen]
         -> [shapelist, Null,     ln]
*/

LET trscore(t) = VALOF
{ // This translates the complete score. It returns
  // TRUE if successful.

  // t -> [-, Score, ln, conductor, partset, qlen]

  LET conductor = h4!t
  LET partset   = h5!t

  UNLESS t & h2!t=s_score DO
  { writef("trscore: System error -- t=%n is not a score*n", t)
    abort(999)
  }

  absmsecs := 0  // Time since the start of the composition.
  currqbeat := 0 // The init qbeat value

  // The initial scaling parameters
  scbase := qbeat
  scfac_t, scfac_b := 1, 1    // No scaling yet. These are set and
                              // restored when translating Tuplet
                              // and  shape operator constructs.

  // Initialise all the shape environment vectors.
  tempoenv      := 0   // Initialise tempo shape vectors.
  tempoadjenv   := 0

  vibrateenv    := 0   // Initialise vbrate shape vectors.
  vibrateadjenv := 0
  vibampenv     := 0   // Initialise vibamp shape vectors.
  vibampadjenv  := 0
  volenv        := 0   // Initialise vol shape vectors.
  voladjenv     := 0
  legatoenv     := 0   // Initialise legato shape vectors.
  legatoadjenv  := 0
  delayenv      := 0   // Initialise delay shape vectors.
  delayadjenv   := 0

  msecsenv      := 0   // Initialise the msecs environment.

//writef("trscore: calling rdblock(%n)*n", t)

  trblock(t) // Translate the score

//writef("trscore: Successful return from trscore*n")
  RESULTIS TRUE
}

AND trblock(t) BE
{ // t -> any one of the four block constructs, ie

  // t  -> [-, Score, ln, name, conductor, partset, qlen]
  // or -> [-, Part,  ln, noteseq, qlen]
  // or -> [-, Solo,  ln, noteseq, qlen]
  // or -> [-, Block, ln, noteseq, qlen]

  // partset  -> [-, Partset, ln, partlist, qlen]
  // partlist -> [-, Part,    ln, noteseq,  qlen]
  // or       -> [-, Solo,    ln, noteseq,  qlen]

  // Note that the syntax analyser has converted any
  // component of a Par construct to a block if it
  // contains shape operators. Other components of a
  // par construct is just an ordinary notesequence.

  // This function (1) saves the current shape environment vectors
  // and the msecs environment, (2) builds new environment vectors
  // and a new msecs environment, (3) generates its midi events
  // and finally (4) restores the saved environments.

  // Any shape data specified within a block has no effect
  // outside the block.

  // Tempo values given within the block affect the speed of
  // performance within the block but the overall speed is
  // scaled to make the performance time of the block the
  // same as if no tempo values were specified within the block.

  // This function append midi events onto the end of midilist.

  LET op = h2!t
  LET ln = h3!t

  // Create new empty shape environments. If no shape values of a
  // particular kind is found the shape environment remains empty.
  // If the first shape value is not at the start of the block,
  // the effect is as if there was a star at the start. If the
  // last value in the shape is not at its end, the effect is as
  // if there was a star at the end. Note that a non empty shape
  // will always have at least one entry.

  // All shape environments have the following structure.

  // env -> [upb, v, prevenv, sq, eq] where
  // upb is zero or the upper bound of v
  // v is zero or the vector of shape entries of form [absq,val]
  // prevenv is the previous environment of this kind
  // sq is the absolute qbeat location of the start of the shape
  // eq is the absolute qbeat location of the end of the shape

  LET tempoupb,      tempov,      prevtempo,      sq,eq,d = 0, 0, tempoenv,      0, 0, 124.0
  LET tempoadjupb,   tempoadjv,   prevtempoadj,   sq,eq,d = 0, 0, tempoadjenv,   0, 0, 100.0
  LET vibrateupb,    vibratev,    prevvibrate,    sq,eq,d = 0, 0, vibrateenv,    0, 0,   6.0
  LET vibrateadjupb, vibrateadjv, prevvibrateadj, sq,eq,d = 0, 0, vibrateadjenv, 0, 0, 100.0
  LET vibampupb,     vibampv,     prevvibamp,     sq,eq,d = 0, 0, vibampenv,     0, 0,  20.0
  LET vibampadjupb,  vibampadjv,  prevvibampadj,  sq,eq,d = 0, 0, vibampadjenv,  0, 0, 100.0
  LET volupb,        volv,        prevvol,        sq,eq,d = 0, 0, volenv,        0, 0,  82.0
  LET voladjupb,     voladjv,     prevvoladj,     sq,eq,d = 0, 0, voladjenv,     0, 0, 100.0
  LET legatoupb,     legatov,     prevlegato,     sq,eq,d = 0, 0, legatoenv,     0, 0,  90.0
  LET legatoadjupb,  legatoadjv,  prevlegatoadj,  sq,eq,d = 0, 0, legatoadjenv,  0, 0, 100.0
  LET delayupb,      delayv,      prevdelay,      sq,eq,d = 0, 0, delayenv,      0, 0,   0.0
  LET delayadjupb,   delayadjv,   prevdelayadj,   sq,eq,d = 0, 0, delayadjenv,   0, 0, 100.0

  LET msecsupb,      msecsv,      prevmsecs,      sq,eq,d = 0, 0, msecsenv,      0, 0,   0.0

//writef("trblock: t=%n entered, op=%s*n", t, opstr(op))

  tempoenv      := @tempoupb    // The new tempo environment control blocks.
  tempoadjenv   := @tempoadjupb
  vibrateenv    := @vibrateupb  // The new vibrate environment control blocks.
  vibrateadjenv := @vibrateadjupb
  vibampenv     := @vibampupb   // The new vibamp environment control blocks.
  vibampadjenv  := @vibampadjupb
  volenv        := @volupb      // The new vol environment control blocks.
  voladjenv     := @voladjupb
  legatoenv     := @legatoupb   // The new legato environment control blocks.
  legatoadjenv  := @legatoadjupb
  delayenv      := @delayupb    // The new delay environment control blocks.
  delayadjenv   := @delayadjupb

  msecsenv      := @msecsupb    // The new msecs environment
//writef("trblock: msecsenv=%n*n", msecsenv)

  // Translate the body of this kind of block.
  SWITCHON h2!t INTO
  { DEFAULT:
      // Unkown kind of block.
      trerr(-1, "trblock: System error in trblock: Unexpected operator op=%s", opstr(h2!t))
      RETURN

    CASE s_score: // t -> [-, Score, ln, name, conductor, partset, qlen]
    { // This case only occurs when t is the parse tree of the entire
      // composition.
      LET conductor = h5!t   // conductor   -> [-, Conductor, ln, notelist, qlen]
      LET partset   = h6!t   // partset     -> [-, Partset,   ln, partlist, qlen]

      // Setup the bar and beat environments for the composition
      // the space is returned just before leaving trblock.
      LET barvupb,  barv  = 0, 0 // Self expanding vector for bars
      LET beatvupb, beatv = 0, 0 // Self expanding vector for beats

      //LET msecvupb, msecv, prev, sq,eq = 0, 0, 0, 0, 0 // The msecs environment
      barenv, beatenv := @barvupb, @beatvupb

//      writef("trblock: barenv=%n beatenv=%n for the composition*n",
//              barenv, beatenv)
//      writef("trblock: these will not change*n")

      // First call barscan to setup the bar and beat self extending
      // vectors and create the outermost settings of the shape
      // environment vectors. While doing this it checks that each
      // bar has the correct qlen based on the given time signtures.
      // It also creates a selfexpanding vector that allow start
      // time in msecs of any qbeat in the composition to be
      // calculated efficiently.
      // The conductor part may not contain notes or inner blocks.
      // It is thus a list of items not containing notes, par,
      // tuplet or block constructs.

      currqbeat      := 0
      maxqbeat       := 0
      currbarqbeat   := 0
      prevbarqbeat   := 0 // Position of previous bar line
      firstnoteqbeat := 0 // Unless bar zero exists
      currpartname   := "unknown"

      timesig_t := 4     // Set the default time signature
      timesig_b := 4
      qbeatsperbeat := 4 * 1024 / timesig_b
      qbeatsperbar := timesig_t * qbeatsperbeat


//writef("trblock: t=%n calling barscan(%n) for the conductor*n", t, conductor)
      barscan(conductor)
//writef("trblock: returned from barscan(%n)*n", conductor)
//abort(1236)

IF FALSE DO
      { // Output the bar table
        LET v = barenv!1
        writef("Bar table")
        IF v FOR i = 1 TO v!0 DO
        { IF i MOD 10 = 1 DO writef("*n%i3: ", i)
          writef(" %i6", v!i)
        }
        newline()
        // Output the beat table
        v := beatenv!1
        writef("Beat table v=%n", v)
        IF v FOR i = 1 TO v!0 DO
        { IF i MOD 10 = 1 DO writef("*n%i3: ", i)
          writef(" %i6", v!i)
        }
        newline()
        newline()
//abort(1883)
      }

      // Set the scaling paramseters for the conductor and
      // set currqbeat to zero.
      scbase, scfac_t, scfac_b := 0, 1, 1
      currqbeat := 0
      blkstartq := 0
      blkendq   := h6!t
  
      // Extract the shape data from the conductor part.
      findshapedata(conductor)
      combineshapes()
//abort(1236)

      //writef("*nShape data after processing the conductor part*n")
      //prshapes()

      // Fill in the entries in the msecs environment based
      // on the conductor's Tempo and Tempoadj shapes, even if
      // no Tempo or Tempoadj constructs were present.
      conductormsecsenv := msecsenv // Save the conductor's version
      setmsecsenv(0, h5!conductor)
//FOR i = 1 TO 16 DO writef("v!%i2 = %8.3f*n", i, msecsenv!1!i)
//abort(1237)

      // Translate the parts.
//writef("trblock: calling trpartset(partset) partset=%n*n", partset)
      trpartset(partset)
      ENDCASE
    }

    CASE s_part   : // t -> [-, Part, ln, noteseq, qlen]
    CASE s_solo   : // t -> [-, Solo, ln, noteseq, qlen]
      // Set the initial scaling parameters for findshapedata.
      scbase, scfac_t, scfac_b := 0, 1, 1
      currqbeat := 0

    CASE s_block  : // t -> [-, Solo, ln, noteseq, qlen]
    { LET prevblkstartq = blkstartq
      LET prevblkendq   = blkendq
      LET noteseq = h4!t

writef("trblock: op=%s*n", opstr(op))

      // Blocks use the current scaling parameters. Note that
      // these are already set for Parts and Solos.

      blkstartq := currqbeat
      blkendq   := currqbeat + h5!t
  
      UNLESS h2!noteseq=s_noteseq DO
      { writef("System error: noteseq expected*n")
        abort(999)
      }
      // noteseq -> [-, Noteseq, ln, notelist, qlen]
      // First add shape data to the current shape environments

      findshapedata(t)
//prshapes()
//abort(12223)
      combineshapes()
prshapes()

      // Fill in the entries in the msecs environment based
      // on the part's Tempo and Tempoadj shapes. These rates
      // times are scaled to ensure that the performance time
      // agree with the outer environment's requirement.

      // Set the initial scaling parameters for setmsecsenv.
      scbase, scfac_t, scfac_b := 0, 1, 1
      currqbeat := 0

      IF h2!tempoenv | h2!tempoadjenv DO setmsecsenv(0, h5!t)

      //prshapes()
//abort(2228)
      // Then translate the noteseq sequence.

      // Set the scaling parameters for the genmidi.
      scbase, scfac_t, scfac_b := 0, 1, 1
      currqbeat := 0 // Parts always start at zero

      genmidi(noteseq)

      // Restore the start and end positions of the enclosing block.
      // This is only needed when h2!t=s_block
      blkstartq := prevblkstartq
      blkendq   := prevblkendq
      currqbeat := blkendq
      ENDCASE
    }
  }

  // Return the environment vectors to free store
  IF tempoupb      DO freevec(tempov)
  IF tempoadjupb   DO freevec(tempoadjv)
  IF vibrateupb    DO freevec(vibratev)
  IF vibrateadjupb DO freevec(vibrateadjv)
  IF vibampupb     DO freevec(vibampv)
  IF vibampadjupb  DO freevec(vibampadjv)
  IF volupb        DO freevec(volv)
  IF voladjupb     DO freevec(voladjv)
  IF legatoupb     DO freevec(legatov)
  IF legatoadjupb  DO freevec(legatoadjv)
  IF delayupb      DO freevec(delayv)
  IF delayadjupb   DO freevec(delayadjv)

  // Restore the previous shape environment vectors
  tempoenv      := prevtempo
  tempoadjenv   := prevtempoadj
  vibrateenv    := prevvibrate
  vibrateadjenv := prevvibrateadj
  vibampenv     := prevvibamp
  vibampadjenv  := prevvibampadj
  volenv        := prevvol
  voladjenv     := prevvoladj
  legatoenv     := prevlegato
  legatoadjenv  := prevlegatoadj
  delayenv      := prevdelay
  delayadjenv   := prevdelayadj
}

AND prmsecsenv() BE
{ LET v = msecsenv!1
  LET layout = 0
  LET v = h2!msecsenv
  
  writef("*nmsecsenv v=%n*n", v)
  UNLESS v RETURN

  FOR i = 1 TO v!0 BY 3 DO
  { IF layout>2 DO
    { writef("*n")
      layout := 0
    }
    writef("   %i6:%6.3f %9.3f", v!i, v!(i+1), v!(i+2))
    layout := layout+1
  }
  newline()
  newline()

  //FOR q = 0 TO 8192 BY 256 DO
  //  writef("%i6: %13.6f*n", q, shapeval(env, q))
  //newline()
}

AND prshapes() BE
{ //writef("*nprshapes: entered*n")
  IF tempoenv      & tempoenv!1      DO prshape("Tempo",      tempoenv)
  IF tempoadjenv   & tempoadjenv!1   DO prshape("Tempoadj",   tempoadjenv)
  IF vibrateenv    & vibrateenv!1    DO prshape("Vibrate",    vibrateenv)
  IF vibrateadjenv & vibrateadjenv!1 DO prshape("Vibrateadj", vibrateadjenv)
  IF vibampenv     & vibampenv!1     DO prshape("Vibamp",     vibampenv)
  IF vibampadjenv  & vibampadjenv!1  DO prshape("Vibampadj",  vibampadjenv)
  IF volenv        & volenv!1        DO prshape("Vol",        volenv)
  IF voladjenv     & voladjenv!1     DO prshape("Voladj",     voladjenv)
  IF legatoenv     & legatoenv!1     DO prshape("legato",     legatoenv)
  IF legatoadjenv  & legatoadjenv!1  DO prshape("Ligatoadj",  legatoadjenv)
  IF delayenv      & delayenv!1      DO prshape("Delay",      delayenv)
  IF delayadjenv   & delayadjenv!1   DO prshape("Delayadj",   delayadjenv)

  newline()
//abort(7446)
}

AND prshape(str, env) BE WHILE env DO
{ LET v = env!1
  LET p = h1!v
  LET prevenv = h3!env
  LET sq      = h4!env
  LET eq      = h5!env
  LET dflt    = h6!env
  LET layout = 0
  writef("%t9: env=%n prevenv=%n sq=%n eq=%n dflt=%8.3f*n",
          str, v, prevenv, sq, eq, dflt)
  env := prevenv     // Ready to print the enclosing environment
  UNLESS v LOOP
  FOR i = 1 TO p-1 BY 2 DO
  { IF layout>4 DO
    { newline()
      layout := 0
    }
    writef(" %i6:%8.3f", v!i, v!(i+1))
    layout := layout+1
  }
  newline()
  //FOR q = 0 TO 8192 BY 256 DO
  //  writef("%i6: %13.6f*n", q, shapeval(env, q))
  //newline()
}

LET calcqlen(t) = t=0 -> 0, VALOF
{ // t points to any node in the parse tree.
  // Return the qlen of this construct.
  // It explores every node reachable from t mainly to
  // fill in the all qlen fields but it does not
  // follow the h1 chain.
  LET op = h2!t  // The tree node operator
  LET ln = h3!t  // The lineno value
  LET qlen = 0

//writef("calcqlen: t=%n op=%s ", t, opstr(op)); prlineno(ln); newline()
//abort(1000)
  SWITCHON op INTO // All possible notelist and shapelist items.
  { DEFAULT:
      writef("System error in calcqlen, t=%n unexpected op = %s*n",
             t, opstr(op))
      abort(999)
      RESULTIS 0

    CASE s_name:
    CASE s_num:
    CASE s_null:
    CASE s_barline:
    CASE s_doublebar:
      RESULTIS 0

    CASE s_partset:  // t -> [-, Partset,  ln, partlist,  qlen]
                     // partlist -> [partlist, Part, ln, noteseq, qlen]
                     // partlist -> [partlist, Solo, ln, noteseq, qlen]
                     // or       = 0
    { LET partlist = h4!t
      LET qlen = 0

      IF h5!t>=0 RESULTIS h5!t

      // Accumulate the qlen of each Part or Solo.
      WHILE partlist DO
      { // partlist -> [-, Part, ln, noteseq, qlen]
        // partlist -> [-, Solo, ln, noteseq, qlen]
        LET len = calcqlen(partlist)
//writef("calcqlen: %s part qlen = %n*n", opstr(h2!partlist), len)
        IF qlen < len DO qlen := len
        partlist := !partlist
      }
//writef("calcqlen: partset qlen = %n*n", qlen)
      h5!t := qlen
      RESULTIS qlen
    }

    CASE s_score:
    { // t -> [-, Score, ln, name, conductor, partset, qlen]
      // This translates a score. It creates a self expanding vector giving
      // the mapping between barlines and q beat positions. This vector is
      // used to check that barlines in the parts are correctly placed.
      // Bar 1 is the first full bar and if bar zero exists it will have
      // fewer qbeats than a full bar.

      LET conductor = h5!t  // conductor -> [-, Conductor, ln, noteseq, qlen]
      LET conductorqlen = h5!conductor
      LET partset   = h6!t  // partset   -> [-, Partset, ln, partlist, qlen]
                            // partlist  -> [-, Part, ln, noteseq, qlen]
                            //   or      -> [-, Solo, ln, noteseq, qlen]
      LET partsetqlen = h5!partset
//writef("calcqlen: case score: t=%n conductor=%n partset=%n qlen=%n*n",
//                              t, conductor, partset, qlen)
      UNLESS conductorqlen>=0 DO
        conductorqlen := calcqlen(conductor)

//writef("calcqlen: conductor qlen = %n*n", conductorqlen)
      // Find the longest q length of the conductor and all the parts.

      UNLESS partsetqlen>=0 DO
        partsetqlen := calcqlen(partset)

      IF conductorqlen < partsetqlen DO conductorqlen := partsetqlen
//writef("calcqlen: score qlen = %n*n", conductorqlen)
//abort(1000)
      h7!t := conductorqlen
      RESULTIS conductorqlen
    }

    CASE s_conductor:  // t -> [-. Conductor, ln, noteseq, qlen]
    CASE s_part:       // t -> [-. Part,      ln, noteseq, qlen]
    CASE s_solo:       // t -> [-. Solo,      ln, noteseq, qlen]
    { LET noteseq = h4!t
      LET qlen = h5!t
      IF qlen>=0 RESULTIS qlen
      qlen := calcqlen(noteseq)
      h5!t := qlen
      RESULTIS qlen
    }

    CASE s_note:      // t -> [-, Note,     ln, <letter,sharps,n>, qlen]
    CASE s_notetied:  // t -> [-, Notetied, ln, <letter,sharps,n>, qlen]
      // Return the qlen of this note item. 
      RESULTIS h5!t

    CASE s_rest:      // t -> [-, Rest, ln, qlen]
    CASE s_space:     // t -> [-, Space, ln, qlen]
      RESULTIS h4!t

    CASE s_tuplet:     // t -> [-, Tuplet, ln, noteseq,  qlen]
      RESULTIS h5!t

    CASE s_delay:      // t -> [-, delay,     ln, noteseq, shapeseq, qlen]
    CASE s_delayadj:   // t -> [-, delayadj,  ln, noteseq, shapeseq, qlen]
    CASE s_legato:     // t -> [-, legato,    ln, noteseq, shapeseq, qlen]
    CASE s_legatoadj:  // t -> [-, legatoadj, ln, noteseq, shapeseq, qlen]
    CASE s_tempo:      // t -> [-, tempo,     ln, noteseq, shapeseq, qlen]
    CASE s_tempoadj:   // t -> [-, tempoadj,  ln, noteseq, shapeseq, qlen]
    CASE s_vibrate:    // t -> [-, vibrate,   ln, noteseq, shapeseq, qlen]
    CASE s_vibrateadj: // t -> [-, vibrateadj,ln, noteseq, shapeseq, qlen]
    CASE s_vibamp:     // t -> [-, vibamp,    ln, noteseq, shapeseq, qlen]
    CASE s_vibampadj:  // t -> [-, vibampadj, ln, noteseq, shapeseq, qlen]
    CASE s_vol:        // t -> [-, vol,       ln, noteseq, shapeseq, qlen]
    CASE s_voladj:     // t -> [-, voladj,    ln, noteseq, shapeseq, qlen]
    { LET noteseq  = h4!t
      LET shapeseq = h5!t
      LET qlen     = h6!t      

      IF qlen>=0 RESULTIS h6!t // qlen already known

      calcqlen(h5!t) // Fill in the qlen of the shape
      qlen := calcqlen(noteseq)
      h6!t := qlen
      RESULTIS qlen
    }

    CASE s_noteseq:       // t -> [-, Noteseq,  ln, notelist, qlen]
    CASE s_shapeseq:      // t -> [-, Shapeseq, ln, notelist, qlen]
    { LET list = h4!t
      LET qlen = h5!t
      IF qlen>=0 RESULTIS qlen
      qlen := 0
      WHILE list DO
      { qlen := qlen + calcqlen(list)
        list := !list
      }
      h5!t := qlen
      RESULTIS qlen
    }

    CASE s_par:       // t -> [-, Par, ln, noteseqlist, qlen]
    { LET list = h4!t // list of Par Noteseqs
      LET qlen = 0

      IF qlen=0 RESULTIS qlen

      // Find the qlen of the longest component
      WHILE list DO
      { LET len = calcqlen(list)
        IF qlen < len DO qlen := len
        list := !list
      }
      h5!t := qlen
      RESULTIS qlen
    }
  }
}

AND findshapedata(t) BE
{ // This function is called from trblock just after it has
  // setup new empty environments as it translates a block.
  // t is initially the root node of any of the four kinds
  // of block but, as a result of recursive calls, it can
  // point to any contruct where shape data can be found.
  // This function does not search inner blocks.

  // A block is one of the following constructs.

  // The noteseqs of the conductor and the parts and solos,
  // the noteseq of a block enclosed in curly brackets.
  // Note that components of a Par construct will have been
  // converted to Blocks if they contain shape constructs.

  // Shape data of the volmap construct is found by calling
  // findvolmapdata.

  // A typical shape environment is volenv which has the
  // following form:

  // volenv -> [0, 0, prevvolenv, sq, eq]

  // The first two fields are the new self expanding vector
  // for the Vol environment, and prevvolenv is the previous
  // Vol environment. sq and eq are the absolute qbeat loctions
  // of the start and end of the current block.

  // Before processing a shape list, findshapedata saves and
  // sets the scaling parameters scbase, scfac_t and scfac_b
  // used to calculate the absolute qbeat positions of
  // locations within t. When it starts to process a shape list
  // it sets currqbeat to zero. The scaling calculation normally
  // done using qscale is as follows

  // absq = scbase + muldiv(currqbeat, scfac_t, scfac_b)

  // scbase, scfac_t and scfac_b are only changed when processing
  // a Tuplet construct or constructs involving shape data. Common
  // factors of scfac_t and scfac_b are always removed. This means
  // that it is probably safe to use (currqbeat*scfac_t) / scfac_b
  // instead of the muldiv call.

  LET op = h2!t
  LET ln = h3!t

//writef("findshapedata: currqbeat=%n dealing with t=%n op=%s  ",
//        currqbeat, t, opstr(op)); prlineno(ln); newline()
//abort(9182)

  SWITCHON op INTO
  { DEFAULT:
      writef("System error in findshapedata - Unexpected op = %s ", opstr(op))
      prlineno(ln)
      newline()
      abort(999)
      RETURN

    CASE s_name:
    CASE s_num:
    CASE s_null:
    CASE s_barline:
    CASE s_doublebar:
//IF op=s_doublebar DO abort(4448)
      RETURN

    //CASE s_partset:   // t -> [-, Partset, ln, partlist,  qlen]
    //CASE s_score:     // t -> [-, Score, ln, name, conductor, partset, qlen]
 
    CASE s_conductor:   // t -> [-. Conductor, ln, noteseq, qlen]
    CASE s_part:        // t -> [-. Part,      ln, noteseq, qlen]
    CASE s_solo:        // t -> [-. Solo,      ln, noteseq, qlen]
    CASE s_block:       // t -> [-. Block,     ln, noteseq, qlen]
    { LET sq = currqbeat
      LET eq = sq + h5!t

writef("trblock: op=%s Setting env limits sq=%n eq=%n*n", opstr(op), sq, eq)
      // Set the start and end positions in all the empty shape environments.

      setenvlimits(tempoenv,      sq, eq)
      setenvlimits(tempoadjenv,   sq, eq)
      setenvlimits(vibrateenv,    sq, eq)
      setenvlimits(vibrateadjenv, sq, eq)
      setenvlimits(vibampenv,     sq, eq)
      setenvlimits(vibampadjenv,  sq, eq)
      setenvlimits(volenv,        sq, eq)
      setenvlimits(voladjenv,     sq, eq)
      setenvlimits(legatoenv,     sq, eq)
      setenvlimits(legatoadjenv,  sq, eq)
      setenvlimits(delayenv,      sq, eq)
      setenvlimits(delayadjenv,   sq, eq)

      setenvlimits(msecsenv,      sq, eq)

      findshapedata(h4!t)
      RETURN
    }

    CASE s_note:        // t -> [-, Note,     ln, <letter,sharps,n>, qlen]
    CASE s_notetied:    // t -> [-, Notetied, ln, <letter,sharps,n>, qlen]
      // Increment currqbeat
      currqbeat := currqbeat + h5!t
      RETURN

    CASE s_rest:        // t -> [-, Rest, ln, qlen]
    CASE s_space:       // t -> [-, Space, ln, qlen]
      currqbeat := currqbeat + h4!t
      RETURN

    CASE s_tuplet:      // t -> [-, Tuplet, ln, noteseq, qlen]
    { LET noteseq = h4!t
      // noteseq -> [-, Noteseq, notelist, qlen]
      LET fromqlen = h4!noteseq
      LET toqlen   = h5!noteseq
      // Save the current scaling parameters
      LET prevcurrqbeat = currqbeat
      LET prevscbase, prevscfac_t, prevscfac_b = scbase, scfac_t, scfac_b
      setscaleparams(fromqlen, toqlen)

      findshapedata(noteseq) 

      // Restore the previous scaling parameters.
      scbase, scfac_t, scfac_b := prevscbase, prevscfac_t, prevscfac_b

      currqbeat := prevcurrqbeat + toqlen
      RETURN
    }

    // All the following shape operators have the structure:
    // t -> [-, op, ln, noteseq, shapeseq, qlen]

    CASE s_delay:      addshapedata(delayenv,      t); RETURN
    CASE s_delayadj:   addshapedata(delayadjenv,   t); RETURN
    CASE s_legato:     addshapedata(legatoenv,     t); RETURN
    CASE s_legatoadj:  addshapedata(legatoadjenv,  t); RETURN
    CASE s_tempo:      addshapedata(tempoenv,      t); RETURN
    CASE s_tempoadj:   addshapedata(tempoadjenv,   t); RETURN
    CASE s_vibrate:    addshapedata(vibrateenv,    t); RETURN
    CASE s_vibrateadj: addshapedata(vibrateadjenv, t); RETURN
    CASE s_vibamp:     addshapedata(vibampenv,     t); RETURN
    CASE s_vibampadj:  addshapedata(vibampadjenv,  t); RETURN
    CASE s_vol:        addshapedata(volenv,        t); RETURN
    CASE s_voladj:     addshapedata(voladjenv,     t); RETURN

    CASE s_noteseq:       // t -> [-, Noteseq, ln, notelist, qlen]
    { LET list = h4!t
      WHILE list DO
      { findshapedata(list)
        list := !list
      }
      currqbeat := currqbeat + h5!t
//abort(2229)
      RETURN
    }

    CASE s_par:       // t -> [-, Par, ln, list, qlen]
                      // All its components are blocks so there
    //CASE s_block:     // t -> [-, Block, ln, noteseq, qlen)
      // No shapedata to find in inner blocks.
      currqbeat := currqbeat + h5!t
      RETURN
  }
}

AND setenvlimits(env, sq, eq) BE
{ // sq and eq are the absolute start and end qbeat location
  // of the shape environment.
  h4!env := sq
  h5!env := eq
}

AND setscaleparams(fromqlen, toqlen) BE
{ // This modifies the scaling parameters scbase, scfac_t
  // and scfac_b for use when processing shapelists of
  // shape operators such as \vol ot \tempoadj, or translating
  // the noteseq of a \tuple construct. The magnification
  // scfac_t/scfac_b is multiplied by toqlen/fromqlen.
  // Common factors or scfac_t and scfac_b are removed, and
  // currqbeat is set to zero.
  // The conversion to absolute q values can be done by
  // absq := qscale(q) which is equivalent to
  // absq := scbase + muldiv(q, scfac_t, scfac_b).

//writef("setscaleparams: scbase=%n scfac_t=%n scfac_b=%n fromqlen=%n toqlen=%n*n",
//        scbase, scfac_t, scfac_b, fromqlen, toqlen)
  IF fromqlen>0 DO
  { LET hcf = 0
    scfac_t := scfac_t * toqlen
    scfac_b := scfac_b * fromqlen
//writef("setscaleparams: gives scfac_t=%n scfac_b=%n*n", scfac_t, scfac_b)
    hcf := gcd(scfac_t, scfac_b)  // Find the highest common factor
    scfac_t := scfac_t / hcf     // Divide both the top and the
    scfac_b := scfac_b / hcf     // bottom by this factor.
//writef("setscaleparams: dividing by hcf=%n*n", hcf)
  }
//writef("setscaleparams: gives scbase=%n scfac_t=%n scfac_b=%n*n", scbase, scfac_t, scfac_b)

  currqbeat := 0
//abort(3775)
}

AND addshapedata(env, shapenode) = VALOF
{ // This adds shape date to shape environment env.

  // shapenode -> [-, op, ln, noteseq, shapeseq, qlen]

  // env holds shape data of the kind corresponding to
  // the shape operator op.

  // It sets up new scaling parameters then calls
  // applyshapelist to add the shape data to env
  // before restoring the previous scaling parameters.
  // Finally it calls findshapedata(noteseq) to search
  // for more shape data.

  LET op        = h2!shapenode
  LET ln        = h3!shapenode
  LET noteseq   = h4!shapenode
  LET shapeseq  = h5!shapenode
  LET toqlen    = h5!noteseq
  LET fromqlen  = h5!shapeseq


  // Save the previous scaling parameters
  LET prevcurrqbeat = currqbeat
  LET oscbase, oscfac_t, oscfac_b = scbase, scfac_t, scfac_b

writef("addshapedata: currqbeat=%n toqlen=%n fromqlen=%n*n", currqbeat, toqlen, fromqlen)

  scbase := qscale(currqbeat)       // Absolute q position of the noteseq
  setscaleparams(fromqlen, toqlen)
  currqbeat := 0 // The start position in the shape list

writef("addshapedata: currqbeat=%n scbase=%n scfac_t=%n scfac_b=%n*n",
        currqbeat, scbase, scfac_t, scfac_b)

  applyshapeseq(env, shapeseq)

  // Restore the previous scaling parameters.
  scbase, scfac_t, scfac_b := oscbase, oscfac_t, oscfac_b

  // Set currqbeat ready for the findshapedata call.
  currqbeat := prevcurrqbeat
//writef("addshapedata: calling findshapedata -- currqbeat=%n*n", currqbeat)
  findshapedata(noteseq)      // Find shape data in noteseq
//abort(5123)
  currqbeat := prevcurrqbeat + toqlen
}

AND applyshapeseq(env, shapeseq) = VALOF
{ // shapeseq -> [-, Shapeseq, ln, shapelist, qlen)
  // This is called when processing a shape node such as
  // \vol or \tempoadj.

  LET shapelist = h4!shapeseq

  prevnum := FALSE   // Set to TRUE by Num or Star
  prevqlen := 1024   // The qlen of the previous space

  WHILE shapelist DO
  { applyshapeitem(env, shapelist)
    shapelist := !shapelist
  }

//abort(5689)
}

AND applyshapeitem(env, t) BE
{ // This transfers shape data from an item in a shape list
  // to its corresponding shape environment env which is non zero.
  // The scale factors have already been set appropriately.
  // so the absolute q beat position can be calculated by
  //     absq := qscale(currqbeat)
  // Typically env -> [upb, v, prevenv, sq,eq,def]
  //     upb and v are the components of the self expanding vector
  //     prevenv is the previous environment of this type
  //     sq and eq are the start and end qbeat locations
  //     def is the default value for this kind of shape

  // t   -> [-, Num,   ln, number]
  // or  -> [-, Star,  ln]
  // or  -> [-, Space, ln, qlen]  Rest and Null have been replaced by Space
  // or  =  0

  // Shape items are linked using the h1 field.

  LET op = h2!t
  LET ln = h3!t
  LET prevenv = env -> h3!env, 0

//writef("applyshapeitem: q=%n  op=%s  ", currqbeat, opstr(op)); prlineno(ln); newline()

  SWITCHON op INTO
  { DEFAULT:
      trerr(-1, "Bad op %s in shape list", opstr(op))
      RETURN

    CASE s_star:             // t -> [-, Star, ln]
    CASE s_num:              // t -> [-, Num,  ln, value]
    { LET FLT x = h4!t //-value if op=Num
      LET prevenv = h3!env
      LET absq = ?

      IF prevnum DO
      { // Assume a space of the previous space qlen
        // between adjacent numbers
        currqbeat := currqbeat + prevqlen
      }
      prevnum := TRUE

      // Find the ablsolute qbeat location
      absq := qscale(currqbeat)
//writef("applyshapeitem: op=%s currqbeat=%n absq=%n*n", opstr(op), currqbeat, absq)

      TEST op=s_num
      THEN      x := h4!t                    // Explicit value
      ELSE TEST prevenv
           THEN x := shapeval(prevenv, absq) // Value in prevenv
           ELSE x := h6!env                  // Default value

writef("applyshapeitem: op=%s value=%7.3f*n", opstr(op), x)

      // Insert the shape entry into env.
      pushshape(env, absq, x) 
writef("applyshapeitem: op=%s returned from pushshape(env, %n, $5.3f*n", opstr(op), absq, x)
      RETURN
    }

    CASE s_space:            // t -> [-, Space, ln, qlen]
//    CASE s_rest:             // t -> [-, Space, ln, qlen]
//    CASE s_null:             // t -> [-, Space, ln, qlen]
    { LET qlen = h4!t // The qlen of this kind of space
writef("applyshapeitem: op=%s qlen=%n*n", opstr(op), qlen)
      currqbeat := currqbeat + qlen
      IF qlen DO prevqlen := qlen
      prevnum := FALSE
      RETURN
    }
  }
}

AND setmsecsenv(q1, q2) BE
{ // q1 and q2 are the absolute qbeat locations of the start
  // and end of the current block. q2 is greater then q1.

  // This function uses tempoenv and tempoadjenv to allocate
  // and set the msecsenv vector.
  // msecsenv -> [upb, v, prevmsecsenv, sq, eq, -]
  // v!0=p is the upperbound of v. In the current implementation
  //       p=upb, and v is a getveced vector.
  // v!1 holds the msecs of qbeat sq
  // v!2 holds the msecs of qbeat sq + 64
  // v!3 holds the msecs of qbeat sq + 64 * 2
  // ,,,
  // v!p holds the msecs of qbeat sq + 64 * (p-1)
  // p is the smallest value for which eq < sq + 64 * (p-1)
  // so p = (eq-sq)/64 + 2

  // When needed the times for intermediate locations are
  // computed using linear interpolation.

  // The outermost msecsenv uses the setting of tempoenv and
  // tempoadjenv as specified by the conductor part. Versions
  // of msecsenv belonging to inner blocks have their timing
  // scaled to synchronise with the conductor specification.

  LET FLT msecs = 0.0
  LET q = q1
  LET i = 1
  LET upb = (q2-q1)/64 + 2
  //    q2-q1          =>    upb
  //                                    v!1 = msecs at q1
  //    0 -  63        =>      2        v!2 = msecs at q1 +  64
  //   64 - 127        =>      3        v!3 = msecs at q1 + 128

  // For inner versions of msecsenv the timings are scaled so
  // that the times at q1 and q2 agree with the conductor's
  // timings at these locations.

  LET v = getvec(upb)
//writef("setmsecsenv: entered*n")

  UNLESS q2 > q1 DO
  { writef("System error: A block must have a qlen greater than zero*n")
    abort(999)
  }

  UNLESS v DO
  { writef("setmsecsenv: System error: More space needed*n")
    abort(999)
  }
  msecsenv!0 := upb
  msecsenv!1 := v
  v!0 := upb
  v!1 := msecs // The msecs time of the first qbeat of this block
               // before scaling.   For the conductor block
               // msecs=0.0 at absq=0

  // msecsenv -> [upb, v, prevenv, sq, eq, -]

writef("setmsecsenv: entered 1*n")
  h4!msecsenv := q1  // The start and end qbeat locations of
  h5!msecsenv := q2  // this version of msecsenv.

  { LET FLT tempo = getshapeval(q, tempoenv, tempoadjenv)
    // The tempo at absolute qbeat location q
    // Calculate the rate at q in units of msecs per 64 qbeats
    // base on tempo in quarter notes per minute.
    LET FLT msecsper64qbeats = (64*60*1000) / (1024*tempo)
    msecs := msecs + msecsper64qbeats
    // msecs is the time at location q+64
//writef("setmsecsenv: tempo=%7.3f msecsper64qbeats=%7.3f msecs=%7.3f*n",
//                     tempo, msecsper64qbeats, msecs)
//abort(3317)
    i := i+1
    v!i := msecs
    q := q+64  // 16 timing values per quarter note
               // Hopefully this give sufficient precision.
  } REPEATUNTIL q2 < q

  UNLESS i=upb DO
  { writef("setmsecsenv: System error i=%n upb=%n*n", i, upb)
    abort(999)
  }

//writef("setmsecsenv: conductormsecsenv=%n msecsenv=%n entered 2*n",
//        conductormsecsenv, msecsenv)

  UNLESS msecsenv=conductormsecsenv DO
  { // This is an inner version of msecsenv and so requires
    // its timings to be scaled.

    LET FLT conductormsecs1 = getmsecs(conductormsecsenv, q1)
    LET FLT requiredmsecs   = getmsecs(conductormsecsenv, q2) -
                              conductormsecs1
    LET FLT actualmsecs     = getmsecs(msecsenv, q2)
    LET FLT scalefactor     = requiredmsecs / actualmsecs
    FOR j = 1 TO upb DO
    { LET FLT relmsecs = v!j
      relmsecs := relmsecs * scalefactor
      v!j := relmsecs + conductormsecs1
    }
  }

//FOR q = 0 TO 12288 BY 512 DO
//{ LET i = q/64 + 1
//  writef("i=%i6 q=%i6 v!i=%10.3f  msecs=%10.3f*n",
//          i, q, v!i, q2msecs(q))
//}
//FOR q = 0 TO 1024 DO//12288 DO
//{ LET i = q/64 + 1
//  writef("i=%i6 q=%i6 v!i=%10.3f  msecs=%10.3f*n",
//          i, q, v!i, q2msecs(q))
//}

//writef("upb=%n*n", upb)
//abort(9933)
}

AND getmsecs(env, iq) = iq=0 | env=0 -> 0.0, VALOF
{ // env -> [upb, v, prevenv, sq, eq]
  // iq is the absolute qbeat location.
  // sq and eq are the absolute qbeat locations of the start
  // and end of the current block  If iq is not in the
  // range sq to eq it is looked up in prevenv.
  // If iq=0 the result is 0.0, the start of the composition.
  // v!1 = msecs at sq      // The start of the current block
  // v!2 = msecs at sq+64   // The times at every 64th
  // v!3 = msecs at sq+126  // qbeat until the end of the block
  // ...
  // v!upb = msecs at sq + (upb-1)*64 // a qbeat after eq.
  // upb is the smallest value such that eq < sq + (upb-1)*64
  // so    sq <= sq+(upb-2)*64 < eq
  // If sq<=iq<=eq there is an i such that v!i<=iq<=v!(i+1)
  // i = (iq-sq) / 64  and  upb = (eq-sq) / 64 + 1

  LET sq, eq = h4!env, h5!env

  IF sq <= iq <= eq DO
  { LET v   = h2!env
    LET sq  = h4!env
    LET eq  = h5!env
    // Remember upb = (iq-sq) / 64 + 2
    LET i   =         (iq-sq) / 64 + 1
    LET FLT q1 = FLOAT(sq + 64 * (i-1))
    LET FLT q2 = q1 + 64.0
    LET FLT x1 = v!i
    LET FLT x2 = v!(i+1)
    LET FLT q  = FLOAT iq
    // q1 <= q < q2
    LET FLT f2  = (q-q1) / (q2-q1)
    LET FLT f1  = 1.0 - f2
//writef("q2msecs: v=%n sq=%n iq=%n i=%n*n", v, sq, iq, i)
//writef("q2msecs: q1=%10.3f q2=%10.3f x1=%10.3f x2=%10.3f*n", q1, q2, x1, x2)
//writef("q2msecs: f1=%10.3f f2=%10.3f => %10.3f*n", f1, f2, x1 * f1 + x2 * f2)
//abort(7229)
    RESULTIS x1 * f1 + x2 * f2
  }

  RESULTIS getmsecs(h3!env, iq)
}

AND gcd(a, b) = VALOF
{ 
//writef("gcd: a=%n b=%n*n", a, b)
  UNTIL a=b DO
  { TEST a<b
    THEN b := b MOD a
    ELSE a := a MOD b
//writef("gcd: a=%n b=%n*n", a, b)
  }
//writef("gcd: result %n*n", a)
//abort(1000)
  RESULTIS a
}

AND barscan(t) BE IF t DO
{ // t  -> [-, Conductor, ln, noteseq, qlen]
  // or -> any note item

  // This scans the conductor part to fill entries in
  // the self expanding vectors barenv and beatenv. While
  // doing so it chacks that there are no items such as
  // notes or blocks that are not permitted in the conductor
  // part. It used currqbeat, currbarno and currbeatno during
  // the scan.

  LET op = h2!t
  LET ln = h3!t

//  writef("barscan: t=%n op=%s currqbeat=%n ln=%n/%n*n",
//                   t, opstr(op), currqbeat, fno(ln), lno(ln))
//abort(9439)
  SWITCHON op INTO
  { DEFAULT: // Ignore all the other tree nodes
      RETURN

    CASE s_conductor: // t -> [-, Conductor, ln, noteseq, qlen]
//writef("barscan: Conductor*n")
      barscan(h4!t)
      RETURN

    CASE s_noteseq:    // [-, Noteseq, ln, notelist, qlen]
    { LET list = h4!t
//writef("barscan: %s*n", opstr(op))
      WHILE list DO
      { barscan(list)
        list := h1!list
      }
      RETURN
    }

    CASE s_name: // t -> [-, Name, ln, str]
//writef("barscan: Name*n")
      currpartname := h4!t
      RETURN

    CASE s_tuplet: // t -> [-, Tuplet, ln, noteseq, qlen]
      trerr(-1, "\tuple is not permitted in the conductor's part")
      RETURN

    CASE s_par:
      trerr(-1, "\par is not permitted in the conductor's part")
      RETURN

    CASE s_note:
    CASE s_notetied:
      trerr(-1, "Notes are not permitted in the conductor's part")
      currqbeat := currqbeat + h5!t
      IF maxqbeat < currqbeat DO maxqbeat := currqbeat
      RETURN

    CASE s_space:
    CASE s_rest:
//writef("barscan: %s at qbeat %x6*n", opstr(op), currqbeat)
      currqbeat := currqbeat + h4!t
      IF maxqbeat < currqbeat DO maxqbeat := currqbeat
      RETURN

    CASE s_null:
//writef("barscan: %s*n", opstr(op))
      RETURN

    CASE s_tempo:
    CASE s_tempoadj:
    CASE s_vol:
    CASE s_voladj:
    CASE s_legato:
    CASE s_legatoadj:
    CASE s_delay:
    CASE s_delayadj:
    CASE s_vibrate:
    CASE s_vibrateadj:
    CASE s_vibamp:
    CASE s_vibampadj:  // [-, op, ln, list, shape]
//writef("barscan: %s at qbeat %x6*n", opstr(op), currqbeat)
      barscan(h4!t)  // Scan only the body of the shape construct
      RETURN

    CASE s_barline:
    CASE s_doublebar:
//writef("barscan: %s at qbeat %x6*n", opstr(op), currqbeat)
//abort(8881)
      // Normally a barline is only allowed when the latest
      // bar has the required number of qbeats. But if at
      // the first barline the number of qbeats is less then
      // qbearsperbar, bar zero is inserted. The last bar
      // of the composition is also allowed to have fewer than
      // qbeatsperbar.

      IF currqbeat < qbeatsperbar DO
      { // We are in bar zero so is must exist.
        // So set firstnoteqbeat to the position of the
        // first note and pad the bar with rests to fill
        // it.
        firstnoteqbeat := qbeatsperbar - currqbeat
        currqbeat := qbeatsperbar
        IF maxqbeat < currqbeat DO maxqbeat := currqbeat
        currbarno := 0
        currbeatno := 0
      }

//writef("barscan: currqbeat=%n prevbarqbeat=%n qbeatsperbar=%n*n",
//                 currqbeat, prevbarqbeat, qbeatsperbar)
      UNLESS currqbeat = prevbarqbeat + qbeatsperbar DO
        writef("Misplaced barline at qbeat %n*n", currqbeat)
//abort(1987)

      prevbarqbeat := currqbeat
      currbarqbeat := currqbeat
      currbarno := currbarno + 1
      IF maxbarno < currbarno DO maxbarno := currbarno
//writef("barscan: calling pushval(barenv, %n)*n", currqbeat)
//abort(9992)
      pushval(barenv, currqbeat) // Store the position of this
                                 // bar in barenv

      // Fill in entries in the beat environment vector.

      { LET q = 0
//writef("barscan: qbeatsperbar=%n maxqbeat=%n*n", qbeatsperbar, maxqbeat)
        WHILE q < qbeatsperbar &
              prevbarqbeat+q < maxqbeat DO
        { // q is a position within the latest bar
          // and before the end of the composition
writef("barscan: calling pushval(beatenv, %x8)  currbarqbeat=%x8*n",
               currbarqbeat+q, currbarqbeat)
          pushval(beatenv, prevbarqbeat+q)
          q := q + qbeatsperbeat
          currbeatno := currbeatno + 1
        }
      }

//writef("barscan: currqbeat=%x6 currbarno=%n currbeatno=%n*n",
//        currqbeat, currbarno, currbeatno)
//abort(1299)
      RETURN

    CASE s_timesig:   // [-, Timesig, ln, sig_t, sig_b]
    { // A time signature may only occur at the start of the
      // composition or just after a barline. sig_b is the length
      // number with 4 corresponding to a quarter note, 8 to a
      // quaver etc. sig_t is the number of these notes per bar.

      timesig_t, timesig_b := h4!t, h5!t
//writef("barscan: %s(%n, %n) at qbeat %x6*n", opstr(op), timesig_t, timesig_b, currqbeat)

      UNLESS currqbeat = prevbarqbeat |
             currqbeat = prevbarqbeat + qbeatsperbar DO
        trerr(currqbeat, "Timesig(%n %n) is not at the start of a bar", timesig_t, timesig_b)
      qbeatsperbeat := 4096/timesig_b
      qbeatsperbar := qbeatsperbeat + timesig_t
      RETURN
    }
  }
  writef("barscan: returning*n")
  //abort(5222)
}

LET pushval(cb, x) BE
{ // cb is the control block for a self expanding vector
  // cb is not zero.
  LET upb = cb!0      // Current upb of v, if v not zero
  LET v   = cb!1      // =0 or a getvec'd vector holding 
                      // the elements.
  // If v is not zero it points to a getvec'd vector with
  // upper bound in cb!0 and v!0 will be the position in v
  // of its latest element. v!0 will be <= upb
  // If the vector is full pushval will allocate another
  // larger copying the existing elements into it before
  // pushing x.

  LET p = v -> v!0, 0 // Position of the previous element, if any.

  // The size of v grows as needed.

  // Initially upb, p, and v are all zero, causing them to be
  // properly initialised on the first call of pushval.

  IF p>=upb DO
  { // v is not large enough, so we must allocate a larger vector
    LET newupb = 3*upb/2 + 10
    LET newv = getvec(newupb)
//writef("pushval: allocating vector %i6 upb %n*n", newv, newupb)
//abort(2222)
    UNLESS newv DO
    { trerr(-1, "More memory needed")
      RETURN
    }
    cb!0 := newupb
    cb!1 := newv
    // Copy the existing elements, but not v!0
    FOR i = 0 TO upb DO newv!i := v!i
    // Pad with zeroes
    FOR i = upb+1 TO newupb DO newv!i := 0
    // Free the old vector if it existed.
    IF v DO freevec(v)

    IF debugv!1 DO
    {  writef("pushval: replacing v=%i6 upb=%i6 with newv=%i7 upb=%i6 p=%n*n",
               v, upb, newv, newupb, p)
      abort(6666)
    }
    v := newv
  }
  p := p+1
  v!0, v!p := p, x
  IF debugv!1 DO
    writef("pushval: updating v[%i3] with %n(%8.3f) cb=%n cb!0=%n cb!1=%n v=%n*n",
            p, x, x, cb, cb!0, cb!1, v)
//abort(6223)
}

AND pushmsecsval(env, absq, FLT tempo) BE
{ // This pushes the triplet [absq, rate, 0.0] into
  // msecs environment env.
  // Convert from quarter notes per minute to msecs per qbeat.
  LET FLT rate = (60 * 1000) / (1024 * tempo)

writef("pushmsecsval: absq=%5i, adjusted tempo=%7.3f rate=%7.3f*n", absq, tempo, rate)
 
  pushval(env, absq)
  pushval(env, rate)
  pushval(env, 0.0)
abort(2840)
}

AND pushshape(env, absq, FLT x) BE
{ // This pushes the shape entry [absq,x] into shape
  // environment env. At this stage ordinary shapes
  // and adjustment shapes are independent.
  // There is also no need for shape entries
  // at positions sq and eq yet. These are inserted
  // later if needed by calls of combine shape.
//writef("pushshape: absq=%i6 x=%7.3f*n", absq, x)
  pushval(env, absq)
  pushval(env, x)
  sortenv(env) // The latest entry may be out of order.
}

AND sortenv(env) BE
{ // This is only called wheb env has at least one entry.
  // The most recently added entry may be out of order.
  LET upb = h1!env
  LET v   = h2!env
  LET p = h1!v - 1        // Subscript of the latest entry
  LET q, x = v!p, v!(p+1) // Copy the latest entry

writef("*nsortenv: entered q=%n x=%9.3f p=%n*n", q, x, p)
abort(2928)

  IF p<3 RETURN  // Only one entry, so no sorting necessary.

//writef("sortenv: more than one entry q=%n v!(p-2)=%n p=%n*n", q, v!(p-2), p)

  IF q >= v!(p-2) RETURN

//writef("sortenv: sorting needed q=%n v!(p-2)=%n p=%n*n", q, v!(p-2), p)


  WHILE p>=3 & q < v!(p-2) DO
  { 
//writef("sortenv: swapping entires q=%n v!(p-2)=%n so move the entry at p-2=%n*n", 
//                 q, v!(p-2), p-2)
    v!p, v!(p+1) := v!(p-2), v(p-1)
    p := p-2
  }
  // Insert the saving entry into correct position
//writef("sortenv: Storing entry q=%n x=%9.3f into position p-2=%n*n", 
//                 q, x, p-2)
  v!(p-2), v!(p-1) := q, x

prshapes()

abort(2929)
}

AND getval(env, pos) = VALOF
{ LET upb = h1!env
  LET v   = h2!env
  LET p   = v -> h1!v, 0
  UNLESS p & 1<=pos<=p DO
    trerr(-1, "System error in getval")
  RESULTIS v!pos
}

/*
AND shapelookup(env, q) = VALOF
{ // env -> [upb, v, prevenv, sq,es,dflt]
  // upb is the upper bound of v or zero
  // v!0=p The hghest subscript of v used
  // v!1 .. v!p hold shape entries of the form [absq,val]
  //            The absq values will be  monotonically
  //            increasing, but consective entries may
  //            have the same absq.
  // sq and eq are the start and end qbeat locations
  //           sq is always less than eq
  // dflt is the default value for this kind of shape.

  LET upb        = h1!env
  AND v          = h2!env
  LET prevenv    = h3!env
  LET sq         = h4!env
  LET eq         = h5!env
  LET defaultval = h6!env
  LET i, lasti = 1, h1!v -1
  LET q1, FLT x1 = sq, defaultval
  LET q2, FLT x2 = ?, ?

  UNLESS v RESULTIS defaultval // env was empty

  { q2, x2 := v!i, v!(i+1)
    i := i+2
    IF i > lasti RESULTIS x1  // Return the value of the last entry.
    q2, x2 := v!i, v!(i+1)
    IF q >= q2 LOOP           // Look at next entry.
    IF q <= q1 RESULTIS x1    // q is the same or earlier than the
                              // absq of the first entry.
    { // q1 < q < q2
      // so perform the linear interpolation calculation.
      LET FLT fq  = FLOAT q
      LET FLT fq1 = FLOAT q1
      LET FLT fq2 = FLOAT q2
      LET FLT f2  = (fq - fq1) / (fq2 - fq1)
      LET FLT f1  = 1.0 - f2
      RESULTIS x1*f1 + x2*f2
    }
  } REPEAT
}
*/

// Implementation of note ties.

// The ties information is held in the globals plist, tlist, clist
// pqpos, tqpos and cqpos. tlist holds a list of unresolved tied notes
// that started in the current note thread. Normally tlist is empty or
// just contains one item, but just after a Par construct the list may
// contain more than one unresolved tie. Multiple ties in tlist can
// only be resolved by multiple note threads arising from a Par
// construct. Each item in tlist is of the form [link, note, q],
// where note is the midi note number (after transposition and pitch
// change) and q is the absolute qbeat position of the start of the
// note ignoring delay effects. The nominal end position (ignoring
// legato and delay) of every note in tlist is the same and is held
// in tqpos. The list plist holds unresolved ties at the start of the
// current Par construct. These can be resolved by notes at the start
// of any of the resulting note threads. clist holds the collection of
// outstanding ties at the end of each thread of a Par construct. When
// the construct is completed, clist becomes the new tlist since the
// multiple threads have now joined to become one.

AND istied(note, absq) = VALOF
{ // note is a midi notenumber (after transposition and pitch change).
  // absq is the absolute qbeat location of this note.

  // The result is TRUE if this note resolves a tie, in which case
  // the tie item is removed from its list (tlist or plist).

  // This function is only called when a Note or Notetied
  // is encountered while generating midi data (in genmidi).

  LET a   = 0

//LET str = VEC 5
//writef("istied: Entered note=%s currqbeat=%n*n",
//        note2str(note, str), currqbeat)
//prties()

  // Choose the list to search through.
  IF plist & absq=pqpos DO a := @plist
  IF tlist & absq=tqpos DO a := @tlist

  // Attempt to find and remove a resolvable tie.

  WHILE a & !a DO
  { // Check if this note resolves a tie in this list.
    LET t = !a   // -> [link, midi_note, absqstart]
    LET midi_note = h2!t
    LET absqstart = h3!t
    IF note=midi_note DO
    { // Item t can be resolved so remove it from its list
      //writef("*nistied: note %n at %n has been resolved at %n*n",
      //        note, absqstart, absq)
      !a := !t
      unmk3(t)
      RESULTIS TRUE
    }
    a := !a
  }

  RESULTIS FALSE
}

AND checktlist(envblk) BE
{ // When called any tie in tlist is unresolvable so generate
  // error messages for any items in the list and issue appropriate
  // note_off commands.
  // envblk is used in the calculation of midimsecs value of any
  // note_off commands generated.
  // On return tlist will be zero and tqpos will be negative.

  WHILE tlist DO
  { // This tie is unresolvable so remove it and generate a warning
    // message and issue a note_off command.
    LET next      = h1!tlist
    LET midi_note = h2!tlist
    LET absqstart = h3!tlist
    LET midimsecs = q2msecs(tqpos, envblk)
    LET str = VEC 5
    note2str(midi_note, str)

    
    trerr(absqstart, "Unresolvable tied note %s", str)
    apmidi(midimsecs,
           midi_note_off+midichannel+(midi_note<<8))
    IF optNtrace DO
      writef("%9.3d Note Off: chan=%n note=%t4*n",
              midimsecs, midichannel, str)
//abort(1000)
    unmk3(tlist)  // Return the current tie item to free store.
    tlist := next
  }

  tlist, tqpos := 0, -1
}

AND prties() BE
{ LET t, c, p = tlist, clist, plist
  LET str = VEC 5
writef("prties: entered, t=%n c=%n p=%n*n", c, t, p)

  UNLESS t | c | p DO { writef("No outstanding ties*n*n"); RETURN }
  IF t DO
  { writef("*ntlist tqpos =%i6:", tqpos)
    WHILE t DO
    { writef(" (%s,%n)", note2str(t!1&127, str), t!2)
      t := !t
    }
  }
  IF c DO
  { writef("*nclist cqpos =%i6:", cqpos)
    WHILE c DO
    { writef(" (%s,%n)", note2str(c!1&127, str), c!2)
      c := !c
    }
  }
  IF p DO
  { writef("*nplist pqpos =%i6:", pqpos)
    WHILE p DO
    { writef(" (%s,%n)", note2str(p!1&127, str), p!2)
      p := !p
    }
  }
  newline()
  newline()
}

AND barno2qbeats(bno) = VALOF
{ // v = barenv!1  // Vector os bar start qvalues
  // v!0 = max barno
  // Bar 1 starts at qbeat v!1
  LET v = barenv!1
  LET maxbarno = v!0
  IF bno > maxbarno DO
  { writef("barno2qbeats: Bad bno=%n maxbarno=%n*n", bno, maxbarno)
    abort(999)
  }
  RESULTIS v!bno
}

AND qbeats2barno(qb) = VALOF
{ IF maxbarno=0 RESULTIS -1

//writef("qbeats2barno: qb=%n currbarno=%n*n", qb, currbarno)

  WHILE currbarno > 1 DO
  { IF qb >= barno2qbeats(currbarno) BREAK
    currbarno := currbarno-1
//writef("qbeats2barno: qb=%n currbarno=%n*n", qb, currbarno)
  }

  WHILE qb >= barno2qbeats(currbarno+1) DO
  { currbarno := currbarno+1
//writef("qbeats2barno: qb=%n currbarno=%n*n", qb, currbarno)
  }
//writef("qbeats2barno: returning %n*n", currbarno)
  RESULTIS currbarno
}

// Scale a local qbeat position to an absolute position
AND qscale(q) = VALOF
{ LET res = ?
//writef("qscale: q=%n scbase=%n scfac_t=%n scfac_b=%n*n", q, scbase, scfac_t, scfac_b)

  UNLESS scfac_b DO
  { writef("System error: scfac_b=0*n")
    abort(999)
    RESULTIS q
  }
  TEST scfac_t=scfac_b
  THEN res := scbase + q
  ELSE res := scbase + muldiv(q, scfac_t, scfac_b)
//  writef("qscale: q=%n scbase=%n scfac_t=%n scfac_b=%n => absq %n*n",
//          q, scbase, scfac_t, scfac_b, res)
//abort(1000)
  RESULTIS res
}

AND trpartset(t) BE
{ // t -> [-, Partset, ln, partlist, qlen]

  // Translate each part or solo as blocks.
  LET partlist = h4!t
//writef("trpartset: entered*n")

  midichannel := 0

  WHILE partlist DO
  { // partlist -> [-, Part, ln, noteseq, qlen]
    // or       -> [-, Solo, ln, noteseq, qlen]
    // or       -> 0
    // Translate each part in the score as a block
    // First set up the default scaling parameters and
    // set currqbeat to zero
    scbase, scfac_t, scfac_b := 0, 1, 1
    currqbeat := 0
    variablevol := FALSE

    tlist, tqpos := 0, 0 // Initialising the tie mechanism for
    plist, pqpos := 0, 0 // this part.
    clist, cqpos := 0, 0

    currbarno := 0
    UNLESS firstnoteqbeat DO currbarno := 1
 
    IF midichannel>15 DO
    { writef("trblock: Too many parts or solos*n")
      midichannel := 1
      abort(999)
    }
//writef("trpartset: Translating part or solo %n, midi channel=%n*n", partlist, midichannel)
    trblock(partlist)
    partlist := !partlist

    // Choose another midi channel for the next part or solo.
    midichannel := midichannel + 1 REPEATWHILE midichannel=9
  }
}

//AND trnoteseq(t) BE
//{ writef("trnoteseq: not yet implemented*n")
//  abort(999)
//}

AND genmidi(t) BE
{ // t is the leading tree node of a segment of notes.
  // qbeat hold the absolute qbeat number of the first qbeat
  // of this segment. Alsolute qbeat zero is silent. The first
  // absolute qbeat of the composition is number 1.

  // The environment of shape data is held in vectors such as
  // volshape and temposhape. These were setup when the current
  // block was entered and they all have the same structure.
  // For instance volshape -> [2n, q1, x1,... qn, xn]
  // where qi specities a qbeat number and xi is the corresponding
  // volume value (a floating point number). If qi is positive the
  // volume changes linearly until q(i+1), but if qi is negative
  // it refers to qbeat number -qi and the volume remains at xi
  // until the end of qbeat number q(i+1). Note that every qbeat
  // of the composition has a non zero number. q1 refers to the
  // first qbeat of the segment and qn refer to the q beat just
  // after the end of the seqment. xn is the volume at the end of
  // the last qbeat of the segment.

  // tlist and clist hold the lists of the current outstanding
  // note ties.

  // The scaling parameters are held in scbase, scfac_t and scfac_b.
  // These are required for the implementation of (x)\tuplet(qlen)
  // allowing local qbeat values to be mapped to absolute values..
  // scbase is the qbeat of the start of note segment x after scaling,
  // scfac_t=qlen is the number of qbeats in the seqment after scaling,
  // scfac_b is the number of qbeats in the seqment before scaling,

  // The qbeat number of qbeat q in x after scaling is
  //        scbase + muldiv(q, scfac_t, scfac_b).

  // Midi data is appended to a list of midi events (midilist). The
  // last node of this list is pointed to by midiliste. Each node
  // in the list is of the form [link, msecs, midi_triple]. Where

  // link points to the next midi item in the list.,

  // msecs is the time of this event in milli-seconds from the
  //      start of the composision,

  // midi_event is a packed collections of bytes representing a
  //      midi event with 0, 1, 2 or 3 arguments.
  //      The least significant byte is the Midi status byte such
  //      as note_on or note_off including the Midi channel number
  //      in the range 0 to 15.
  //      The senior 24 bits of midi_event proviide up to 3 operand
  //      bytes, such as a note number and pressure. Although not
  //      currently used, a non midi entry can be represented using
  //      a least significant byte less that 128.

  // The midi data is held in a linked list so that the events can
  // be sorted later into time order.

  LET op = h2!t  // The tree node operator
  LET ln = h3!t  // The fno/ln number of the tree node
  LET a1 = h4!t  // The first operand, if any, of the tree node
  LET a2 = h5!t  // The second operand, if any, of the tree node
  LET midia1, midia2, midia3 = 0, 0, 0 // Midi event argument bytes
  LET opname  = opstr(op)
  tokln := ln

//writef("genmidi: t=%n currqbeat=%n ", t, currqbeat)
//writef("op=%10s h4!t=%i7 ", opstr(op), h4!t, fno(ln), lno(ln)); prlineno(ln)
//newline()
//abort(1003)

  IF optNtrace DO
  { LET absq  = qscale(currqbeat)  // Scaled qbeat number
    LET msecs = FIX q2msecs(absq)  // Time in millisecs (an integer)
    writef("%i6: ", msecs)
    writef("%10t absq=%n q=%n barno=%n  ",
           opname, absq, currqbeat, currbarno)
    writef("tempo=%i3 ",  FIX getshapeval(absq, tempoenv,  tempoadjenv))
    writef("delay=%i3 ",  FIX getshapeval(absq, delayenv,  delayadjenv))
    writef("vol=%i3 ",    FIX getshapeval(absq, volenv,    voladjenv))
    writef("legato=%i3   ", FIX getshapeval(absq, legatoenv, legatoadjenv))
    prlineno(ln)
    newline()
//abort(9553)
  }

  SWITCHON op INTO
  { DEFAULT:
      // Ignore most node types
      RETURN

    CASE s_name:
      currpartname := h4!t
      IF optNtrace DO
      { LET absq = qscale(currqbeat)
        LET t = q2msecs(absq)
        writef("*n%i6: %s*n*n", t, currpartname)
      }
      RETURN

    CASE s_varvol:     // t -> [-, varvol,   ln]
       // Volume may change while a note is being played
       // as is typical of wind instruments
       variablevol := TRUE
       RETURN

    CASE s_nonvarvol:  // t -> [-, nonvarvol,ln]
       // Volume may not change while a note is being played
       // as is typical of keyboard instruments
       variablevol := FALSE
       RETURN

    CASE s_pedon:      // t -> [-, pedon,    ln]
    CASE s_pedoff:     // t -> [-, pedoff,   ln]
    CASE s_pedoffon:   // t -> [-, pedoffon, ln]
    CASE s_portaon:    // t -> [-, portaon,  ln]
    CASE s_portaoff:   // t -> [-, portaoff, ln]
    CASE s_softon:     // t -> [-, softon,   ln]
    CASE s_softoff:    // t -> [-, softoff,  ln]

    CASE s_control:    // t -> [-, control, ln, controller, value]
    { LET dly = FIX getshapeval(currqbeat, s_delay, s_delayadj)
      // currqbeat and dly are both unscaled qbeat values
      LET absq = qscale(currqbeat+dly)
      // Use the timemap data in the environment to calculate the
      // midi msecs value.
      LET midimsecs = q2msecs(absq)
      LET chan      = midichannel // chan in range 0 to 15 

      SWITCHON op INTO
      { DEFAULT:
          writef("genmidi: Bad op %s (%n) currqbeat=%n*n",
                 opname, op, currqbeat)
          abort(999)
          RETURN

        CASE s_pedon:     midia1, midia2 := 64, 127; ENDCASE
        CASE s_pedoff:    midia1, midia2 := 64,   0; ENDCASE
        CASE s_pedoffon:  midia1, midia2 := 64,   0; ENDCASE
        CASE s_portaon:   midia1, midia2 := 65, 127; ENDCASE
        CASE s_portaoff:  midia1, midia2 := 65,   0; ENDCASE
        CASE s_softon:    midia1, midia2 := 66, 127; ENDCASE
        CASE s_softoff:   midia1, midia2 := 66,   0; ENDCASE
        CASE s_control:                      ENDCASE
      }

      apmidi(midimsecs,                                  // Msecs
             midi_control+chan+(midia1<<8)+(midia2<<16)) // Control
      IF optNtrace DO
        writef("%i6: Control:   chan=%n ctrl=%i3  val=%n*n",
                midimsecs, chan, midia1, midia2)
      IF op=s_pedoffon DO
      { midia2 := 127   // pedoff to pedon
        // Delay pedon by 10 msecs
        apmidi(midimsecs+10,                               // Msecs
               midi_control+chan+(midia1<<8)+(midia2<<16)) // Control
        IF optNtrace DO
          writef("%i6: Control:   chan=%n ctrl=%i3  val=%n*n",
                  midimsecs, chan, midia1, midia2)
      }
      RETURN
    }

    CASE s_rest:   // t -. [-, Rest,  ln, qlen]
    CASE s_space:  // t -. [-, Space, ln, qlen]
//writef("genmidi: rest: currqbeat=%n qlen=%n*n", currqbeat, h4!t)
      currqbeat := currqbeat + h4!t
      // Check for unresolved ties is tlist.
      checktlist(tlist)
      RETURN

    CASE s_note:      // t -> [-, Note,     ln, <letter,sharps,n>, qlen]
    CASE s_notetied:  // t -> [-, Notetied, ln, <letter,sharps,n>, qlen]
//writef("CASE s_note or s_notetied:*n")
    { LET note   = h4!t
      LET n      =  note      & 255
      LET sharps = (note>> 8) & 255
      LET letter = (note>>16) & 255
      // Find the requires Midi note number
      LET midi_note  = n + transposition + pitch  // The transposed note
      LET qlen   = h5!t // Length of the note in qbeats

      LET nomqs  = qscale(currqbeat)       // Nominal start abs qbeat
      LET nomqe  = qscale(currqbeat+qlen)  // Nominal end abs qbeat

//writef("genmidi: op=%s  n=%n transposition=%n pitch=%n => %n*n",
//                 opstr(op), n, transposition, pitch,  midi_note)
//abort(3219)

      UNLESS istied(midi_note, nomqs) DO
      { // This note does not resolve a previous tie, so play it.
        LET dly       = FIX getshapeval(nomqs, delayenv, delayadjenv)
        LET legato    = FIX getshapeval(nomqs, legatoenv, legatoadjenv)
        LET absqs     = qscale(currqbeat+dly)
        LET midimsecs = FIX getmsecs(msecsenv, absqs)
        LET chan      = midichannel // chan is in range 0 to 15

        // The +1 is a fudge at the moment to avoid a problem
        // when two different shape values are at the same
        // qbeat position. May be this should not be allowed
        // to happen.
        LET vol = FIX getshapeval(nomqs, volenv, voladjenv)

        // Scale volume 0 to 100_000 to midi range 0 to 127
        vol := (vol * 127) / 100
        IF vol>127 DO vol := 127
        IF vol<0   DO vol := 0

//writef("*ngenmidi: %i6 %t8  ", midimsecs, opname)
//prnote(letter, sharps, n, qlen)
//writef(" vol=%n legato=%n*n", vol, legato)
//writef("currqbeat=%n to %n delay=%n*n", currqbeat, currqbeat+qlen, dly)

        //IF transposition DO
        //  writef("getmidi: note %i3 transposed to %i3*n  ", n, midi_note)

//writef("getmidi: variablevol=%n*n", variablevol)
//abort(1010)

        // Schedule a note_on command.
        TEST variablevol
        THEN { // This note should modify its volume as it is played
               UNLESS vol=chanvol DO
               { apmidi(midimsecs,               // Channel volume
                       midi_control+midichannel+(7<<8)+(vol<<16))
                 IF optNtrace DO
                   writef("%i6: Chan=%i2 vol:  chan=%n vol=%n*n",
                     midimsecs, midichannel, vol)
                 chanvol := vol
               }
               apmidi(midimsecs,                                // Note on
                      midi_note_on+midichannel+(midi_note<<8)+(127<<16))
               IF optNtrace DO
                 writef("%9.3d Note On:   chan=%n note=%t4  vol=%n*n",
                   midimsecs, midichannel, note2str(midi_note, strv), 127)
               FOR q = currqbeat+64 TO currqbeat+qlen-32 BY 128 DO
               { LET dly = FIX getshapeval(q, delayenv, delayadjenv)
                 LET legato = FIX getshapeval(q, legatoenv, legatoadjenv)
                 LET absqs = qscale(q+dly)
                 LET midimsecs = FIX getmsecs(msecsenv, absqs)
                 LET vol = FIX getshapeval(q+1, volenv, voladjenv)

                 // Scale volume 0 to 100_000 to midi range 0 to 127
                 vol := (127 * vol) / 100
                 IF vol>127 DO vol := 127
                 IF vol<0   DO vol := 0

//writef("genmidi: Note chanvol=%n vol=%n*n", chanvol, vol)

                 UNLESS chanvol=vol DO
                 { apmidi(midimsecs,               // Channel volume
                         midi_control+midichannel+(7<<8)+(vol<<16))
                   IF optNtrace DO
                     writef("%i6: Chan vol:  chan=%n vol=%n*n",
                       midimsecs, midichannel, vol)
                   chanvol := vol
                 }
               }
             }
        ELSE { apmidi(midimsecs,                                // Note on
                      midi_note_on+midichannel+(midi_note<<8)+(vol<<16))
               IF optNtrace DO
                 writef("%i6: Note On:   chan=%n note=%t4  vol=%n*n",
                   midimsecs, midichannel, note2str(midi_note, strv), vol)

//              apmidi(midimsecs,                 // test GS percussion
//              midi_note_on+9+(71<<8)+(vol<<16))
             }
     }
abort(3881)
     // Check that there are no unresolved ties in tlist
     //checktlist(envblk)

     // tlist is now zero

     TEST op=s_notetied
     THEN { // This note is tied with a later one so don't
            // schedule its note off command, but insert an item
            // in the current list of unresolved tied notes.
            LET absqs = qscale(currqbeat)   // Nominal start of note
//writef("genmidi: The note is tied*n")
            tqpos := qscale(currqbeat+qlen) // Nominal end of note
            tlist := mk3(0, midi_note, absqs)
//writef("genmidi: note=%n currqbeat=%n absqs=%n tqpos=%n in tlist*n",
//          midi_note, currqbeat, absqs, tqpos)
            prties()
          }
     ELSE { // This note is not tied to a later one,
            // so schedule a note off command.
            // The legatoness of a note is determined at its start.
            LET leg       = FIX getshapeval(currqbeat, legatoenv, legatoadjenv)
            LET qe        = currqbeat + (qlen * leg) / 100
            LET dly       = FIX getshapeval(qe, delayenv, delayadjenv)
            LET absqe     = qscale(qe+dly)
            LET midimsecs = FIX getmsecs(msecsenv, absqe)

writef("%i6: Note off: chan=%n note=%i3  legato=%n*n",
       midimsecs, midichannel, n, leg)
            apmidi(midimsecs,                    // Note off
                   midi_note_off+midichannel+(midi_note<<8))
            IF optNtrace DO
              writef("%i6: Note off: chan=%n note=%t4*n",
                      midimsecs, midichannel, note2str(midi_note, strv))
          }

      // Return the qbeats value of the next note item. 
      currqbeat := currqbeat + qlen
//writef("genmidi: note %n done currqbeat=%n*n", midi_note, currqbeat)
      RETURN
    }

    CASE s_transposition:
      transposition := h4!t
//writef("genmidi: transposition set to %n*n", transposition)
      RETURN


    CASE s_bank:
//writef("CASE s_bank:*n")
    { LET dly = FIX getshapeval(currqbeat, s_delay, s_delayadj)
      // currqbeat and dly are both unscaled qbeat values
      LET absq = qscale(currqbeat+dly)
      // Use the timemap data in the environment to calculate the
      // midi msecs value.
      LET midimsecs = q2msecs(absq)

      apmidi(midimsecs,                                   // Msecs
             midi_control+midichannel+(0<<8)+(h4!t<<16))  // Bank MSB
      apmidi(midimsecs,                                   // Msecs
             midi_control+midichannel+(32<<8)+(h5!t<<16)) // Bank LSB
      IF optNtrace DO
      { writef("%9.3d Bank:      chan=%n MSB=%n*n",
               midimsecs, midichannel, h4!t)
        writef("%9.3d Bank:      chan=%n LSB=%n*n",
               midimsecs, midichannel, h5!t)
      }
      RETURN
    }

    CASE s_patch:
//writef("CASE s_patch:*n")
    { LET dly = getshapeval(currqbeat, s_delay, s_delayadj)/1000
      // currqbeat and dly are both unscaled qbeat values
      LET absq = qscale(currqbeat+dly)
      // Use the timemap data in the environment to calculate the
      // midi msecs value.
      LET midimsecs = q2msecs(absq)

      apmidi(midimsecs,                            // Msecs
             midi_progchange+midichannel+(h4!t<<8))  // Patch command
      IF optNtrace DO
      { writef("%9.3d Patch:     chan=%n prog=%n*n",
               midimsecs, midichannel, h4!t)
      }
      RETURN
    }

    CASE s_part:      // [-, Part,      ln, noteseq,  qlen, midichan]
    CASE s_solo:      // [-, Solo,      ln, noteseq,  env]
      //writef("genmidi: %s*n", opname)

      barqerr := 0

      IF midichannel>=15 DO
        writef("Error: No more than 16 parts are allowed*n")

      // Choose next midi channel (avoiding 9 -- GM percussion)
      // midichannel will be in range 0 to 15
      midichannel := midichannel + 1 REPEATWHILE midichannel=9
      h6!t := midichannel
      chanvol := -1
writef("genmidi: Allocated channel %n to a part or solo*n", midichannel)
//abort(1993)

      // Allow more than one solo part
      IF h2!t=s_solo & midichannel>=0 DO solochannels := 1<<midichannel

      transposition := 0 // No transposition specified yet
      currqbeat := 0

      genmidi(h4!t)

      // Check that there are no outstanding ties.
//writef("genmidi: just finished generating a part or solo*n")
//prties()
//writef("genmidi: checking that there are no outstanding ties*n")
      //checktlist(envblk)
//prties()
//abort(1178)
      RETURN

    CASE s_delay:      // [-, delay,     ln, noteseq, shapeseq, qlen]
    CASE s_delayadj:   // [-, delayadj,  ln, noteseq, shapeseq, qlen]
    CASE s_legato:     // [-, legato,    ln, noteseq, shapeseq, qlen]
    CASE s_legatoadj:  // [-, legatoadj, ln, noteseq, shapeseq, qlen]
    CASE s_tempo:      // [-, tempo,     ln, noteseq, shapeseq, qlen]
    CASE s_tempoadj:   // [-, tempoadj,  ln, noteseq, shapeseq, qlen]
    CASE s_vibrate:    // [-, vibrate,   ln, noteseq, shapeseq, qlen]
    CASE s_vibrateadj: // [-, vibrateadj,ln, noteseq, shapeseq, qlen]
    CASE s_vibamp:     // [-, vibamp,    ln, noteseq, shapeseq, qlen]
    CASE s_vibampadj:  // [-, vibampadj, ln, noteseq, shapeseq, qlen]
    CASE s_vol:        // [-, vol,       ln, noteseq, shapeseq, qlen]
    CASE s_voladj:     // [-, voladj,    ln, noteseq, shapeseq, qlen]
      // The shape data has already been extracted and is stroed
      // in the environment vectors.
      genmidi(h4!t)
      RETURN

    CASE s_noteseq:       // [-, Seq, ln, list, qlen]
    { LET list = h4!t
      //writef("genmidi: %s -- %n*n", opname, ln)
      WHILE list DO
      { genmidi(list)
        list := !list
      }
      RETURN
    }

    CASE s_barline:
    CASE s_doublebar:
    { LET absq = qscale(currqbeat)
      LET t    = q2msecs(absq)
      LET absq = qscale(currqbeat)
      LET q1   = barno2qbeats(currbarno+1)
      LET q2   = barno2qbeats(currbarno+2)
      LET qerr = ?
      currbarno := currbarno+1

      qerr := absq - q1

      // Check tlist for unresolvable ties.
      IF tlist & tqpos~=absq DO checktlist()

      IF optNtrace DO
        writef("*n%i6: Bar: %n*n", t, currbarno)

      UNLESS qerr=barqerr DO
      { barqerr := qerr
        trerr(absq, "Misplaced barline %n qbeats*
                    * (about %n semiquaver%-%ps) %s start of bar %n*n",
              ABS qerr, ABS qerr/256, (qerr<0 -> "before", "after"), currbarno)
      }
      RETURN
    }

    CASE s_par:       // t -> [-, Par, ln, parlist, qlen]
      // list is the list of items in the par construct
      // qlen is the qbeat length of the longest par item.
//writef("genmidi: %s qlen=%n -- <%n/%n> currqbeat=%n*n",
//        opname, h5!t, fno(ln), lno(ln), currqbeat)
//prties()

    { LET parlist = h4!t // List of Par components
      LET qlen  = h5!t   // qbeat length of longest element of the par.
      LET q0 = currqbeat // qbeat position of the start
                         // of the par construct.
      LET q1 = q0 + qlen // qbeat position of the end.
      LET absq0 = qscale(q0)
      LET absq1 = qscale(q1)
      LET count = 0      // Element number

      // Save old tie lists
      LET oclist, ocqpos = clist, cqpos
      LET oplist, opqpos = -1, -1
 //writef("genmidi: %s <%n/%n> saved clist*n",
 //        opname, fno(ln), lno(ln))

      TEST pqpos=absq0
      THEN { // This Par construct can resolve ties in plist, so do
             // not change it and do note restore it at the end.
 //writef("genmidi: this Par construct can resolve ties in plist*n")
           }
      ELSE { // This par construct cannot resolve ties in plist, so
             // set plist to the current tlist
 //writef("genmidi: this Par construct cannot resolve plist ties*n")
 //writef("genmidi: so save plist and set it to tlist*n")
             oplist, opqpos := plist, pqpos
             plist, pqpos := tlist, tqpos
           }

      // Set up the new clist.
      clist, cqpos := 0, absq1

      //writef("genmidi: setting tlist and clist to null*n")

      WHILE parlist DO
      { // Translate each component of the par construct as a block.
        count := count+1       // Count of members
        chanvol := -1

        // Start each member of the par construct at the same
        // local qbeat position
        currqbeat := q0
        tlist, tqpos := 0, -1  // No outstanding ties in the current thread.
        
        //writef("genmidi: op=%s <%n/%n> starting par element %n*n",
        //        opname, fno(ln), lno(ln), count)
        //prties()
        trblock(h4!t)
        //writef("genmidi: op=%s <%n/%n> finished par element %n*n",
        //        opname, fno(ln), lno(ln), count)
        //prties()

        tokln := h3!(h4!t)

        UNLESS currqbeat-q0 = qlen DO
        { TEST h2!(h4!t)=s_part | h2!(h4!t)=s_solo
          THEN { LET bn1 = qbeats2barno(currqbeat)
                 LET bn2 = qbeats2barno(q0+qlen)
                 LET qerr = q0 + qlen - currqbeat
                 trerr(-1,
                   "Part ends %n qbeats early in bar %n, final bar is %n*n",
                   qerr, bn1, bn2)
               }
          ELSE { trerr(-1,
                  "Member %n of the par construct has qlen=%n, should be %n*n",
                  count, currqbeat-q0, qlen)
               }
        }

        // Check for unresolvable ties in tlist
        //IF tlist & tqpos~=absq1 DO
        //  checktlist(envblk)

        // Insert tlist onto the front of clist
        IF tlist DO
        { LET p = tlist
          WHILE !p DO p := !p
          !p := clist
          clist := tlist
          // cqpos is still absq1
        }

        // Inspect the next member of the par construct
//writef("tlist items have been added to clist*n")
//        writef("genmidi: tlist set to null*n")
//prties()
        parlist := !parlist
      }

      // All members of the par construct have been processed, so
      // set tlist = clist, restore old clist, and conditionally
      // restore plist.

      IF oplist>=0 DO
      { // This par construct started later than an enclosing one,
        // so all ties in plist must have been resolved by now. 
        // Check for unresolved ties in plist.
        //IF plist DO
        //{ tlist, tqpos := plist, pqpos
        //  checktlist(envblk)
        //}
        // Restore previous plist
        plist, pqpos := oplist, opqpos
      }

      // Set tlist and clist appropriately.
      tlist, tqpos :=  clist,  cqpos
      clist, cqpos := oclist, ocqpos

      currqbeat := q0 + qlen

//writef("Leaving par construct with currqbeat=%n*n", currqbeat)
//prties()
      RETURN
    }

    CASE s_tuplet:
    { // t -> [-, Tuplet, ln, noteseq, qlen]

      LET noteseq    = h4!t
      LET toqlen     = h5!t
      LET fromqlen   = calcqlen(noteseq)

      // Save the previous scaling parameters
      LET prevcurrqbeat = currqbeat
      LET oscbase, oscfac_t, oscfac_b = scbase, scfac_t, scfac_b
      scbase := qscale(currqbeat) // Absolute q position of the noteseq
      setscaleparams(fromqlen, toqlen)
      currqbeat := 0 // The start position in the noteseq


writef("genmidi: %s <%n/%n> saved clist  ", opstr(op)); prlineno(ln); newline()

      genmidi(h4!t)

      // Restore the previous scaling parameters
      scbase, scfac_t, scfac_b := oscbase, oscfac_t, oscfac_b

      currqbeat := prevcurrqbeat + toqlen

//writef("Leaving Tuplet construct with currqbeat=%n*n", currqbeat)
//prties()
      RETURN
    }
  }
}


AND apmidi(t, code) BE
{ // Append a node onto the end of the midi list
  // t is the time in msecs and
  // code is a midi duplet (op, a) or triplet (op, a, b) of bytes
  //      code = op + (a<<8) + (b<<16)
  LET node = mk3(0, t, code)
  !midiliste := node
  midiliste := node
  //writef("apmidi: t=%i6 %x8*n", t, code)
}

AND q2msecs(absq) = getmsecs(msecsenv, absq)

AND barno2msecs(bno) = VALOF
{   
  LET q  = barno2qbeats(bno)
  LET ms = q2msecs(q)
//writef("*nbarno2msecs: bno=%n maxbarno=%n q=%n*n", bno, maxbarno, q)
//writef("barno2msecs: returns msecs=%n*n", ms)
//abort(1003)
  RESULTIS ms
}

.

SECTION "Writemidi"

GET "libhdr"
GET "playmus.h"
GET "sound.h"

MANIFEST {
Meta = #xff

// Meta event defines
Meta_sequence   = 0

// The text type meta events
Meta_text       = 1
Meta_copyright  = 2
Meta_trackname  = 3
Meta_instrument = 4
Meta_lyric      = 5
Meta_marker     = 6
Meta_cue        = 7

// More meta events
Meta_channel      = #x20
Meta_port         = #x21
Meta_eot          = #x2f
Meta_tempo        = #x51
Meta_smpte_offset = #x54
Meta_time         = #x58
Meta_key          = #x59
Meta_prop         = #x7f

// The maximum of the midi defined text types
Max_text_type     = 7

Head_magic        = #x4D546864  // MHdr
Track_magic       = #x4D54726B  // MTrk
}


LET pushbyte(cb, b) BE
{ // pushbyte is only used to append a byte of midi data held in
  // the self expanding vector cb.

  // cb -> [upb, v]      // upb is the upper bound of v in words
  // v  -> [pos, ... ]   // pos is the position of the next byte in v

  // It uses getvec and freevec to allocate space as needed.

  LET upb  = cb!0                  // Current upb (in words) of v
  LET bupb = upb*bytesperword      // The upb in bytes
  LET v    = cb!1                  // is zero or a getvec'd vector holding 
                                   // the elements.
  LET p = v -> v!0, bytesperword-1 // Byte pos of prev element, if any.

//writef("cb=%n p=%n upb=%n v=%n*n", cb, p, upb, v)

  // The size of v grows as needed.

  // Initially upb, p, and v are all zero, causing them to be
  // properly initialised on the first call of pushval.

  IF p+1>bupb DO
  { // Increase the size of the self expanding vector by about 50%
    LET newupb = 3*upb/2 + 100
    LET newv = getvec(newupb)
    UNLESS newv DO
    { trerr(-1, "More memory needed")
      RETURN
    }
    cb!0 := newupb
    cb!1 := newv
    // Copy the existing table into the new one
    FOR i = 0 TO upb DO newv!i := v!i
    // Pad with zeros
    FOR i = upb+1 TO newupb DO newv!i := 0
    // Free the old table
    IF v DO freevec(v)
    v := newv
  }
  p := p+1
//writef("pushbyte: %x2(%n) at p=%n*n", b&255, b&255, p)
  v!0, v%p := p, b
}

AND pushh(cb, x) BE
{ // Push 16 bits in bigender order
  pushbyte(cb, x>>8)
  pushbyte(cb, x)
}

AND pushw(cb, x) BE
{ // Push a 32 bit value as bytes in bigender order
  pushbyte(cb, x>>24)
  pushbyte(cb, x>>16)
  pushbyte(cb, x>>8)
  pushbyte(cb, x)
}

AND pushw24(cb, x) BE
{ // Push a 24 bit value as bytes in bigender order
  pushbyte(cb, x>>16)
  pushbyte(cb, x>>8)
  pushbyte(cb, x)
}

AND pushstr(cb, s) BE
{ // Push a string of bytes
  //writef("pushstr: cb=%n %s*n", cb, s)
  pushnum(cb, s%0)
  FOR i = 1 TO s%0 DO pushbyte(cb, s%i)
}

// pushnum outputs a delta time of n ticks as a variable length sequence of
// bytes. For instance
// 1xxxxxxx 1yyyyyyy 0zzzzzzz is the encoding of n = xxxxxxxyyyyyyyzzzzzz
// The first byte is not 10000000

AND pushnum(cb, n) BE
{ pushpfx(cb, n>>7)
  pushbyte(cb, n & 127)
}

AND pushpfx(cb, n) BE IF n DO
{ pushpfx(cb, n>>7)
  pushbyte(cb, #x80+(n & 127))
}

AND packw(cb, p, x) BE
{ // Store a 32 bit value at byte position p of the midi file in
  // bigender order.
  LET upb = cb!0
  LET v   = cb!1
  LET pos = v!0
  // cb -> [upb, v]        // A self expanding vector
  // v  -> [pos, ... ]
  IF p+3 > pos RETURN
  v%p     := x>>24
  v%(p+1) := x>>16
  v%(p+2) := x>>8
  v%(p+3) := x
}

LET writemidi(filename, midilist) BE
{ // Write MIDI file filename from MIDI items in midilist that have already
  // been sorted.
  LET prevr = 0
  LET stop_msecs = end_msecs + 1_000 // Stop 1 second after end_msecs
  LET stdout = output()
  LET midiout =  0
  LET upb, v = 0, 0    // The self expanding byte vector control block
  LET cb = @upb        // Self expanding byte vector
  LET lpos = 0         // Byte position of track length field

  // Pack Midi header
  
  // Write the MIDI Header
  pushw(cb, Head_magic)      // = #x4D546864 ie MHdr

  pushw(cb, 6)               // The header byte length
  pushh(cb, 1)               // Format 1 = one or more tracks
  pushh(cb, 2)               // Number of track = 2
  pushh(cb, 1000)            // 1000 ticks per quarter note
                             // This is positive so not using SMPTE time codes

  // Write the first (control) track
  pushw(cb, Track_magic)     // = #x4D54726B ie MTrk
  lpos := v!0 + 1            // Byte position of the 32 bit length of this track

  pushw(cb, 0)               // For the track byte length

  pushnum(cb, 0)             // Delta time = 0
  pushbyte(cb, #xFF)         // Sequence/Track name
  pushbyte(cb, #x03)         // Since this is the first track using format 1
  pushstr(cb, "control track") // This is the sequence name

  pushnum(cb, 0)             // Delta time = 0
  pushbyte(cb, #xFF)         // Meta text
  pushbyte(cb, #x01)
  pushstr(cb, "creator: ")

  pushnum(cb, 0)             // Delta time = 0
  pushbyte(cb, #xFF)         // Meta text
  pushbyte(cb, #x01)
  pushstr(cb, playmus_version)
  //pushstr(cb, "GNU Lilypond 2.10.29          ")

  pushnum(cb, 0)             // Delta time = 0
  pushbyte(cb, #xFF)         // Meta time
  pushbyte(cb, #x58)
  pushbyte(cb, 4)            // length
  pushbyte(cb, 4)            // 4 beats per bar
  pushbyte(cb, 2)            // 1 beat = a crochet, ie 4/4 time
  pushbyte(cb, #x12)         // 18 midi clocks per metronome click
  pushbyte(cb, #x08)         // 8 semidemi quavers per 24 midi clocks

  pushnum(cb, 0)             // Delta time = 0
  pushbyte(cb, #xFF)         // Tempo
  pushbyte(cb, #x51)         //
  pushbyte(cb, #x03)         // 
  pushw24(cb, 1_000_000)     // 1000000 usecs per quarter note

  pushnum(cb, 0)             // Delta time = 0
  pushbyte(cb, #xFF)         // End of track
  pushbyte(cb, #x2F)         //
  pushbyte(cb, #x00)         //
  
  packw(cb, lpos, v!0-lpos-3)// Fill in byte length of the track

  // Write the (second) track
  // This contains all the notes
  pushw(cb, Track_magic)     // = #x4D54726B ie MTrk
  lpos := v!0 + 1            // Byte position in cb of the 32 bit track
                             // length field
  pushw(cb, 0)               // For the track length field

  pushnum(cb, 0)             // Delta time = 0
  pushbyte(cb, #xFF)         // Track name
  pushbyte(cb, #x03)
  pushstr(cb, "The notes")   // The notes

  WHILE midilist DO
  { // midilist = 0 or 
    //          -> [next, midimsec, triple]
    LET midimsecs = midilist!1
    LET triple    = midilist!2
    LET op, a1, a2, is_note_on = ?, ?, ?, ?
    LET fn, chan = ?, ?
    LET playing = ?
    LET dr, r = 0, 0

    IF midimsecs>stop_msecs BREAK
    midilist := !midilist

    op :=  triple      & 255
    a1 := (triple>>8)  & 255
    a2 := (triple>>16) & 255
    fn := op & #xF0     // Midi op without the channel number
    is_note_on := fn = midi_note_on
    chan := op & #x0F   // The midi channel number
    //playing := start_msecs<=midimsecs<=end_msecs

writef("%i7: midi op: %x2 %x2 %x2*n", 999, op, a1, a2)
abort(5595)

    IF midimsecs>start_msecs DO
    { // Work out the real time in msecs of the item
      r := m2r_msecs(midimsecs, ocr, ocm, crate)
      dr := r - prevr  // The delta time in msecs
    }

    SWITCHON fn INTO
    { DEFAULT:  writef("Unexpected midi op: %x2 %x2 %x2*n", op, a1, a2)
                ENDCASE

      CASE midi_note_on:
        UNLESS midimsecs>=start_msecs LOOP
        // note_on is ignored if after the end of the segment being played

      CASE midi_note_off:
      CASE midi_keypressure:
      CASE midi_control:
      CASE midi_chanpressure:
      CASE midi_pitchbend:
        pushnum (cb, dr)
        pushbyte(cb, op)
        pushbyte(cb, a1)
        pushbyte(cb, a2)
        prevr := r
        LOOP

      CASE midi_progchange:
        pushnum(cb, dr)
        pushbyte(cb, op)
        pushbyte(cb, a1)
        prevr := r
        LOOP

      //CASE midi_sysex:
      //CASE Meta:
      // playmus does not currently generate these in the note track
      LOOP
    }
  }

  pushnum(cb, 0)             // Delta time = 0
  pushbyte(cb, #xFF)         // End of track
  pushbyte(cb, #x2F)         //
  pushbyte(cb, #x00)         //
  
  packw(cb, lpos, v!0-lpos-3)// Fill in byte length of the track
/*
  IF v DO
  { FOR i = bytesperword TO v!0 DO
    { IF (i-4) MOD 16 = 0 DO newline()
      writef(" %x2", v%i)
    }
    newline()
  }
*/


  midiout := findoutput(filename)
  writef("Writing Midi file: %s*n", filename)

  UNLESS midiout DO
  { writef("Can't open MIDI output file: %s*n", filename)
    RETURN
  }

  //writef("midi byte upb=%n*n", v!0)
  selectoutput(midiout)

  // The midi bytes start in v after the 32 bit length field
  FOR i = bytesperword TO v!0 DO binwrch(v%i)

  endstream(midiout)
  selectoutput(stdout)
  freevec(v)        // Free the vector holding the Midi data
}

.

SECTION "Shapefns"

GET "libhdr"
GET "playmus.h"

/*
Note sequences can be modified by shape operators such as \vol which
can place Vol shape values within the sequence. There are many kind of
shape but they are all handled in the same way.  This description is
concentrates on the treatment of Vol settings.

Vol settings are set using the construct noteseq \vol shapeseq.  Note
sequences contain notes, rests and many other note items but Vol shape
values only affect the volume of note items that generate a sound. The
note sequence has a length in qbeats which is the sum of the lengths
of notes, rests and spaces it contains.  A shape sequence consists of
numbers and stars possibly separated by rests and spaces.  A space of
the same length as the previous space is inserted between consecutive
numbers or stars.  A shape sequence consisting of just one number or
star has length zero, otherwise its length in qbeats which is the sum
of the lengths of the spaces and rests it contains. A shape sequence
of length zero causes its value to be placed at the start of the first
qbeat of the note sequence, otherwise the shape sequence is scaled to
be the same length as the note sequence and the shape values are
placed in the corresponding qbeat positions.

The scope of shape values is limited to the current block and the are
combines to produce a shape belonging to the block. Multiple \vol
constructs within the same block all contribute to the same shape
belonging to the block. It is possible that a block contains no \vol
constructs. If this happens the Vol shape for the block is empty.  If
the first shape sequence in a block starts with a rest or space a star
is inserted at the beginning and if the last shape seqence in the
block ends wit a rest or space a star is inserted at the end. A star
has a value corresponding to the Vol value at its qbeat position in
the enclosing block. The shape belonging to a block thus has a
specified value at the start of its first qbeat and also at the end of
its last qbeat.

The value of Vol at a particular qbeat is determined by linear
interposlation between the nearest two shape points in the block's
shape, but if the Vol shape for the current block is empty, the value
is computed from the enclosing shape. If none of the enclosing blocks
Vol shapes are non empty, the default value for Vol is chosen.

If the current Vol shape and and enclosing Vol shape are both non
empty then their values at the qbeat position are combined by taking
their average. A typical example is when the conductor specifies a
certain Vol setting and a soloist specifies another. They both affect
the volume value with equal weight.

Note that shape values in the current block are exactly the values
specified by the relevant shape sequence. The detailed evaluation of
shape values is not done until later when midi events are being
generated.

The Tempo and Tempoadj shapes are somewhat exceptional since they are
used by the conductor to construct a structure that allows the start
time in msec to be computed efficiently. Tempo values within an inner
block modifies the speed of playing note within the block but the
overall speed is scaled to cause the block to take exactly the same
time as specified by the conductor. See the sefinition of setmsecsenv
for more information about how timing is computed.

*/

LET getshapeval(absq, env, envadj) = VALOF
{ // This function returns the shape value at location absq
  // of env after multiplying the corresponding adjustment
  // at the same location

  // absq is an absolute qbeat location.
  // env -> [upb, v, prevenv, sq, eq, dflt]   env is non zero
  //        upb is the upperbound of v
  //        v!0=p and v!1 .. v!p contain the shape items
  //        of the form [q,val] for this shape.
  //        sq and eq are the start and end locations of
  //        the shape.
  //        dflt   is the default value for this shape.
  // envadj is the corresponding adjustment environment.

  LET FLT val = shapeval(env,    absq)
  LET FLT adj = shapeval(envadj, absq)
  LET FLT res = (val*adj) / 100.0 // Apply the adjstment
                                  // which is a percentage.
//IF FALSE
UNLESS env=tempoenv DO
{ //writef("getshapeval: absq=%i6 val=%8.3f adj=%8.3f => res=%8.3f*n",
  //        absq, val, adj, res)
  //abort(1873)
}
  RESULTIS res
}

AND combineshapes() BE
{ // Call combineshape(env) for every non empty
  // shape environment.
  // combineshapes is only called when all shape environments
  // are non zero.

//writef("*ncombineshapes: Before processing each shape*n")
//prshapes()

  IF h2!delayenv      DO combineshape(delayenv)
  IF h2!delayadjenv   DO combineshape(delayadjenv)
  IF h2!legatoenv     DO combineshape(legatoenv)
  IF h2!legatoadjenv  DO combineshape(legatoadjenv)
  IF h2!tempoenv      DO combineshape(tempoenv)
  IF h2!tempoadjenv   DO combineshape(tempoadjenv)
  IF h2!vibrateenv    DO combineshape(vibrateenv)
  IF h2!vibrateadjenv DO combineshape(vibrateadjenv)
  IF h2!vibampenv     DO combineshape(vibampenv)
  IF h2!vibampadjenv  DO combineshape(vibampadjenv)
  IF h2!volenv        DO combineshape(volenv)
  IF h2!voladjenv     DO combineshape(voladjenv)

writef("*ncombineshapes: After processing each shape*n")
prshapes()
//abort(4417)
}

AND combineshape(env) BE
{ // env -> [upb, v, prevenv, sq, eq, dflt]
  // This environment is not empty. This function tries
  // to find a non empty enclosing environments. If there
  // is not one, the function leaves env unchaged except
  // it ensure that env has entries for sq and eq adding
  // entries with the default value where necessary.
  // If there is a non empty enclosing environments,
  // we can call it env1 and create a new environment
  // containing entries for sq and eq and every position
  // in either env or env1, taking the average of the
  // two values. Care is required if either environment
  // has multiple entries for the same location.

  LET upb, v, prevenv = h1!env, h2!env, h3!env
  LET sq, eq, FLT dflt    = h4!env, h5!env, h6!env

  // Search for the nearest non empty enclosing environment
  LET env1 = prevenv

  UNTIL env1 = 0 | h2!env1 DO
  { // env1 was empty
    env1 := h3!env1 // Consider its previous environment.
  }

  h3!env := env1 // Optimisation to improve the search
                 // next time.

  // env1 is the nearest non empty environment or zero

  UNLESS env1 DO
  { // env is an outermost environment, so ensure it has
    // entries for locations sq and eq.
    LET last = h1!v - 1 // The position of the last entry
                        // of env. 
    TEST sq=v!1
    THEN { // env has an entry for location sq, so does not
           // need to be rebuilt.
           // Insert an entry for eq if necessary.
           UNLESS eq=v!last DO pushshape(env, eq,  dflt)
           RETURN
         }
    ELSE { // env does not have an entry for location sq, so it
           // must be rebuilt.
           h1!env, h2!env := 0, 0 // Clear env.

//writef("env=%n v=%n p=%n sq=%n eq=%n dflt=%7.3f*n", env, v, p, sq, eq, dflt)
//FOR i = 1 TO p-1 BY 2 DO
//{ IF i/2 MOD 4 = 0 DO newline()
//  writef(" %i6 %9.3f", v!i, v!(i+1))
//}
//newline()
//abort(888)
           // Insert an entry for location sq.
           pushshape(env, sq,  dflt)
           // Copy the other entries.
           FOR i = 1 TO last BY 2  DO pushshape(env, v!i, v!(i+1))
           // Insert an entry for eq if necessary.
           UNLESS eq=v!last        DO pushshape(env, eq,  dflt)
           freevec(v) // Free the vector in the environment copy.
           RETURN
         }
  }

  { // env1 is a non empty enclosing environments so
    // combine env and env1.
    LET v1 = h2!env1
    LET last = h1!v - 1 // Position of the last entry of env
    LET last1 = h1!v1 - 1 // Position of the last entry of env1

    LET i  = 1    // Position of first entry in env.
    LET i1 = 1    // Position of first entry in env1.

    LET q = v!i
    LET q1 = v1!i1
    LET nq = sq // The location of the next entry to add to env.

    h1!env, h2!env := 0, 0 // Clear env, ready to be rebuilt.

    // Now merge the shape data from env1 and env2 into env

// In the following diagram illustrates how this is done.
// sq, eq          start and end locations of a block
// q1,..., qn      shape entry locations
// #               outer shape values
// o o o           outer shape interpolation lines
// X               inner shape value
// + + +           inner shape interpolation lines
// P               the average of the inner and outer values
//                 at this location. This value occurs in
//                 the combined environment.

// |                           X + + + + + + +X+
// |                  oo#o    +|         |    | +
// |              ooo   |  oo+ P         |    |  +
// d          ooo   |   |   +oo|         |    P   +        ood    default value |
// |      #oo   |   P   P  +   |oo       P    |    +   ooo   |                  |
// |      |     P   |   | +    |   oo    |    |    oPo       |                  |
// |      |     |  +X+  |+     |      oo |    ooo   |        |                  |
// |      |     X+  |  +X      |         #oo  |     |        |                  |
// |      |     |   |   |      |         |    |     |        |                  |
// |      |     |   |   |      |         |    |     |        |                  |
// |      |     q1--q2--q3-----q4--------|----q5----qn       |  ) inner shape   |
// |      |     sq      |                |          eq       |  )               |
// |      |             |                |                   |                  |
// |      q1------------q2---------------q3------------------qn ) outer shape   |
// |      sq                                                 eq )               |


    UNTIL i = last DO
    { LET FLT nx = 0.0 // This will be the value for location nq.
      // Find the position of the first entry in env for a location
      // greater than nq
      UNTIL nq < v!i DO i := i+2
      // Find the position of the first entry in env1 for a location
      // greater than nq
      UNTIL nq < v1!i1 DO i1 := i1+2

      // i-2 is the position of the last env entry for a location <= nq
      // i   is the position of the first env entry for a location > nq
      // i1-2 is the position of the last env1 entry for a location <= nq
      // i1   is the position of the first env1 entry for a location > nq

      nx := ( interpolate(nq, v !(i -2), v !(i -1), v !i,  v !(i +1)) +
              interpolate(nq, v1!(i1-2), v1!(i1-1), v1!i1, v1!(i1+1)) ) / 2.0
      pushshape(nq, nx)

      IF nq=eq BREAK // The combination is complete.

      TEST v!i < v1!i1 THEN nq := v!i
                       ELSE nq := v1!i1
    } REPEAT

    freevec(v) // Return the old env vector to free store.
  }
}

AND shapeval(env, absq) = VALOF
{ // env ->  [upb, v, prevenv, sq, eq, dflt] where
  //         upb,v is a self expanding vector
  //         prevenv is the enclosing environment
  //         sq and eq are the absolute qbeat locations of
  //         the start and end of the environment.
  //         dflt is the default value for this shape.
  // absq    is an absolute qbeat position.
  // env is non zero but may represent and empty environment
  // shapeval attempts to find the nearest non empty
  // environment whose range includes absq. If it exist
  // the value is obtained by linear interpolation
  // otherwise the result is the default value.

  LET e    = env
  LET sq   = h4!e
  LET eq   = h5!e
  LET dflt = h6!e

//writef("shapeval: absq=%n e=%n sq=%n eq=%n*n", absq, e, sq, eq)
  
  // Try to find a non empty environment containg absq
  UNTIL e=0 | h1!e & h4!e<=absq<=h5!e DO
  {
    sq, eq := h4!e, h5!e
    e := h3!e
//writef("shapeval: absq=%n e=%n sq=%n eq=%n*n", absq, e, sq, eq)
  }

  // e is the closest non empty environment containing absq or zero.
  // and sq and eq hold it start and end locations.

  UNLESS e DO
  { //writef("*nshapeval: absq=%n sq=%n eq=%n No suitable enclosing environment*n",
    //       absq, sq, eq)
    //writef("*nshapeval: Returning the default value=%7.3f*n", dflt)
    //abort(2661)

    RESULTIS dflt // There was no suitable enclosing environment.
  }

  // e is the closest non empty enclosing environment containing absq

  { LET v = h2!e          // vector of its [absq,val] entries.
                          // This will contain entries for sq and eq.
    LET i = 3             // The position of its second entry.
    LET last = h1!v - 1   // The position of its last entry
    // Find the surrounding pair of shape points.

    IF i > last DO
    {
      writef("shapeval: System error: i=%n last=%n*n", i, last)
      abort(999)
    }

    IF absq = v!last RESULTIS v!(last+1)
 
    // sq <= absq < eq

    UNTIL absq < v!i DO
    {
//writef("*nshapeval 0: absq=%n i=%n v!(i-2)=%n v!(i-1)=%7.3f v!i=%n v!(i+1)=%7.3f*n",
//        absq, i, v!(i-2), v!(i-1), v!i, v!(i+1))
      IF v!i = eq DO
      { // i is the position of the first entry for location eq

//writef("*nshapeval 1: absq=%n i=%n eq=%n v!(i-2)=%n v!(i-1)=%7.3f v!i=%n v!(i+1)=%7.3f*n",
//        absq, i, eq, v!(i-2), v!(i-1), v!i, v!(i+1))
//writef("shapeval 2: v!(last+1)=%7.3f*n", v!(last+1))
//writef("shapeval 3: => %7.3f*n", v!(last+1))
//abort(2662)

        RESULTIS v!last
      }
      i := i+2
    }

//writef("*nshapeval 4: absq=%n i=%n v!(i-2)=%n v!(i-1)=%7.3f v!i=%n v!(i+1)=%7.3f*n",
//        absq, i, v!(i-2), v!(i-1), v!i, v!(i+1))
//writef("*nshapeval 5: => %7.3f*n", interpolate(absq, v!(i-2), v!(i-1), v!i, v!(i+1)))
//abort(2663)

    RESULTIS interpolate(absq, v!(i-2), v!(i-1), v!i, v!(i+1))
  }
}

AND interpolate(q, q1, FLT x1, q2, FLT x2) = VALOF
{ // q1 <= q <= q2 and q1 < q2
  UNLESS q1 < q2 DO
  { writef("interpolate: System error q1=%n q=%n q2=%n*n", q1, q, q2)
    abort(999)
    RESULTIS x1
  }
//writef("interpolate: q=%n q1=%n x1=%7.3f q2=%n x2=%7.3f*n", q, q1, x1, q2, x2)
//abort(18834)
  IF q=q1 RESULTIS x1 // Optimisation
  IF q=q2 RESULTIS x2 // Optimisation
  // Otherwise perform linear interpolation.
  RESULTIS x1 + (x2-x1) * FLOAT(q-q1) / FLOAT(q2-q1)
}

.

SECTION "Playmidi"

GET "libhdr"
GET "playmus.h"
GET "sound.h"
GET "mc.h"

/*
Playmidi reads microphone input and commands from the keyboard while
it plays the midi events.

If option ACC is given, input from the microphone will be compared
with solo part(s) in an attempt to synchronise midi output with the
soloist.

The keyboard commands are read using pollsardch and will be as follows:

B       The nearest bar line is now
space   The nearest beat is now
+       Play faster
-       Play slower
S       Stop/Start
G       Go to start of bar n. All commands reset n to zero
P       Go to start of the previous bar
N       Go to the start of the next bar
0..9    n := 10n + digit
*/

LET genrecogfn(note) = VALOF
{ // This function is under development

  // Generate an MC function to return the amplitude of a given note.
  // The result is the function number or zero on error.
  // The resulting MC function takes one argument which is a BCPL pointer
  // to the latest the latest cumulative sample. Sufficient samples are
  // assumed to be available.
  // The result is the average amplitude of the given note.
  LET a, m, b = 0, 0, 0
  LET freq = freqtab!note
  LET samples_per_cycle = muldiv(44100, 1000, freq) // Scaled ddd.ddd
  LET qcycle = samples_per_cycle/4 // 90 degree offset
  LET v1 = soundv + soundvupb
  LET v2 = v1 - qcycle             // For sample 90 degrees out of phase
  LET p1, p2, total_by_2, amplitude = 0, 0, 0, 0
  LET cycles = (freq * 32) / 440_000  // 32 cycles for note 4A
  IF cycles<4  DO cycles := 4         // Ensure 4<=cycle<=32
  IF cycles>32 DO cycles := 32

  // Ensure that cycles is not too large for the sound buffer.
  WHILE cycles*samples_per_cycle/1000 <= soundvupb-qcycle DO
    cycles := cycles-1

  // Need to generate native code for the following:

  total_by_2 := (!v1 - !(v1-cycles*samples_per_cycle/1000)) / 2

  p1, p2 := total_by_2, total_by_2

  FOR i = 1 TO cycles DO
  { b := i * samples_per_cycle / 1000
    b := (b+1) & -2 // Round to nearest even number
    m := (a+b) / 2  // Midpoint of this cycle
    p1 := p1 - !(v1-a) + !(v1-m)
    p2 := p2 - !(v2-a) + !(v2-m)
    a := b          // Position of first sample of next cycle
  }
  // Calculate the average amplitude
  amplitude := (ABS p1 + ABS p2) / cycles
  RESULTIS amplitude
}


AND getrealmsecs() = VALOF
{ // Return a msecs value that increases even over midnight.
  MANIFEST { msecsperday = 24*60*60*1000 } // msecs in 24 hours
  LET day, msecs, filler = 0, 0, 0
  sys(Sys_datstamp, @day)

  // Initialise baseday on first call of getrealmsecs
  IF msecsbase < 0 DO msecsbase := msecs

  // Return msecs since msecsbase.
  msecs := msecs - msecsbase
  IF msecs<0 DO msecs := msecs+msecsperday
  RESULTIS msecs
}

LET notecofn(argv) = VALOF
{ // soundmsecs is the real time of the latest sample insoundv
  LET note = argv!0
  LET notetimes = argv!1    // This must be freed before the coroutine dies.
  LET noteupb = notetimes!0
  LET notep = 0             // Will hold the position in notetimes of the
                            // nearest matching note
  LET dmsecs = 0            // Difference between midi time of matching
                            // note and real_msecs  
  LET rmsecs = 0            // Real time now
  LET offset = 0 // Offset to first sample of k cycles
  LET freq = freqtab!note
  LET notename = VEC 1
  LET samples_per_cycle = muldiv(1000, 44100_000, freq) // Scaled integer
  LET mask = #xFFFF
  LET prevamp, noteon = 0, FALSE
  LET k =  note/2               // Number of cycles to use
  IF k<4 DO k := 4
  IF k>32 DO k := 32
  offset := samples_per_cycle / 44100 // offset in msecs
  // If a note is detected, assume it started at soundmsecs-offset
  note2str(note, notename)

  writef("*nnote=%s samples_per_cycle = %9.3d freq=%9.3d k=%n*n",
           notename, samples_per_cycle, freq, k)
  FOR i = 1 TO notetimes!0 DO
  { IF (i-1) MOD 8 = 0 DO newline()
    writef(" %9.3d", notetimes!i)
  }
  newline()

  rmsecs := cowait(0) // real time of latest sample, or -1 to finish

  WHILE rmsecs>=0 DO
  { LET p0amp, p1amp = 0, 0
    LET p = soundp + mask
    LET q = p + samples_per_cycle/4000 // 90 degree out of phase
    LET c = 0
    LET amp, total = ?, ?

    FOR i = 1 TO k DO
    { LET a = ((samples_per_cycle*i)/1000) & -2 // Round down to even
      LET b = (a+c)/2
      //writef("a=%i4 b=%i6 c=%i6*n", a, b, c)
      c := a
      p0amp := p0amp - soundv!((p-a)&mask) +
                       soundv!((p-b)&mask)
      p1amp := p1amp - soundv!((q-a)&mask) +
                       soundv!((q-b)&mask)
      //writef("p0amp=%i8   p1amp=%i8*n", p0amp, p1amp)
    }
    total := soundv!((p-c)&mask) - soundv!(p&mask)
    // Calculate the average amplitude of c samples
    amp := (ABS(total+2*p0amp) + ABS(total+2*p1amp)) / c
    //writef("%9.3d %i6*n", freq, amp)
    //writef("%s %i7*n", notename, amp)
    //IF amp>3500 UNLESS noteon DO
    IF amp>2500 UNLESS noteon DO
    { // A note start has just been detected
      LET startrmsecs = soundmsecs-offset // Real time of note start
      LET mmsecs = r2m_msecs(startrmsecs, oer, oem, erate)
      //writef("%9.3d: %9.3d %9.3d %s*n", rmsecs,startrmsecs,mmsecs,notename)
      //writef("  prevamp=%i6  amp=%i6*n", prevamp, amp)
      noteon := TRUE
      // A note has just started so add an event if it was expected
      { // Loop to find earliest expected note with midi time > mmsecs
        notep := notep+1
        IF notep>noteupb BREAK
        IF notetimes!notep > mmsecs DO
        { dmsecs := notetimes!notep - mmsecs
          BREAK
        }
      } REPEAT

      IF notep>1 & mmsecs - notetimes!(notep-1) < dmsecs DO
      { notep := notep-1
        dmsecs := notetimes!notep - mmsecs
      }
      // If the note is within 500 msecs of now add it to the set of events.
      // Its weight is its amplitude (0..127).
      IF -0_500 <= dmsecs <= 0_500 DO
      { addevent(mmsecs, startrmsecs, amp, note)
        totalerr, notecount := totalerr+dmsecs, notecount+1
        writef("%9.3d: mmsecs=%9.3d err %9.3d avg=%9.3d amp=%i6 %s*n",
                rmsecs, mmsecs, dmsecs, totalerr/notecount, amp, notename)
      }
    }

    IF amp<1000 IF noteon DO
    { //writef("%9.3d: %s off", rmsecs, notename)
      //writef("  prevamp=%i6  amp=%i6*n", prevamp, amp)
      noteon := FALSE
    }
    //newline()
//abort(1000)
    prevamp := amp
    rmsecs := cowait(amp)
  }

  // We have been told to commit suicide.
  IF notetimes DO freevec(notetimes)
  die()
}

AND setfreqtab() BE
{ // Set freqtab so that freqtab!n = 1000 * the note frequency
  // where n is the MIDI note number. n=60 for middle C (C4).

  freqtab := TABLE
     8_176,   8_662,   9_178,   9_723,  10_301,  10_914, //   0 -c.. -b
    11_563,  12_250,  12_979,  13_750,  14_568,  15_434,

    16_352,  17_324,  18_355,  19_446,  20_602,  21_827, //  12 0c .. 0b
    23_125,  24_500,  25_957,  27_500,  29_136,  30_868,

    32_703,  34_648,  36_709,  38_891,  41_204,  43_654, //  24 1c .. 1b
    46_250,  49_000,  51_914,  55_000,  58_271,  61_736,
  
    65_406,  69_296,  73_417,  77_782,  82_407,  87_308, //  36 2c .. b2
    92_499,  97_999, 103_827, 110_000, 116_541, 123_471,

   130_812, 138_592, 146_833, 155_564, 164_814, 174_615, //  48 3c .. 3b
   184_998, 195_998, 207_653, 220_000, 233_082, 246_942,

   261_623, 277_183, 293_665, 311_127, 329_628, 349_229, //  60 4c .. 4b
   369_995, 391_996, 415_305, 440_000, 466_164, 493_884,

   523_245, 554_366, 587_330, 622_254, 659_255, 698_457, //  72 5c .. 5b
   739_989, 783_991, 830_610, 880_000, 932_328, 987_767,

  1046_489,1108_731,1174_659,1244_508,1318_510,1396_913, //  84 6c .. 6b
  1479_978,1567_982,1661_219,1760_000,1864_655,1975_533,

  2092_978,2217_461,2349_318,2489_016,2637_020,2793_826, //  96 7c .. 7b
  2959_955,3135_963,3322_438,3520_000,3729_310,3951_066,

  4185_955,4434_922,       0,       0,       0,       0, // 108 8c .. 8b
         0,       0,       0,       0,       0,       0,

         0,       0,       0,       0,       0,       0, // 120 9c .. 9g
         0,       0

//writef("freqtab=%n*n", freqtab)
  // Check the table
  checktab( 98, 2349_318)
  checktab( 99, 2489_016)
  checktab(100, 2637_020)
  checktab(101, 2793_826)
  checktab(102, 2959_955)
  checktab(103, 3135_963)
  checktab(104, 3322_438)
  checktab(105, 3520_000)
  checktab(106, 3729_310)
  checktab(107, 3951_066)
  checktab(108, 4185_955)
  checktab(109, 4434_922)
}

AND checktab(n, f) BE WHILE n>=0 DO
  { UNLESS freqtab!n = f DO
    { writef("note=%i3 change %8.3d to %8.3d*n", n, freqtab!n, f)
      abort(999)
    }
    n, f := n-12, (f+1)/2
  }

AND findtimes(note) = VALOF
{ // Find the start times of this note played by any of the soloists.
  LET upb, v = 0, 0 // A self expanding vector
  LET p = midilist  // List of midi triples
  LET stop_msecs = end_msecs + 1_000 // Stop 1 second after end_msecs
  LET notename = VEC 1
  note2str(note, notename)

  UNLESS solochannels RESULTIS 0

  WHILE p DO
  { LET op, a1, a2 = ?, ?, ?
    LET msecs = p!1 // Time of next midi event
    LET triple = p!2
    LET midiop = triple & #xF0
    LET chan   = triple & #x0F
    LET a1 = (triple>> 8) & 255
    LET a2 = (triple>>16) & 255

    p := !p

    UNLESS a1 = note LOOP

    UNLESS midiop=midi_note_on LOOP
    IF ((1<<chan) & solochannels)=0 LOOP

    IF msecs>stop_msecs BREAK
    IF msecs<start_msecs LOOP

    pushval(@upb, msecs)
    //writef("%9.3d %s*n", msecs, notename)
  }
  RESULTIS v
}

AND addevent(rt, mt, weight, note) BE
{ // note = -1 for bar line events
  // note = -2 for beat events
  // note>=0 for note events

  LET p = eventv+eventp

//  writef("addevent: %5.3d %5.3d weight=%i6  note=%n*n",
//          rt, mt, weight, note)

  IF rt<prevrt+10 RETURN
  prevrt := rt
  p!0, p!1, p!2, p!3 :=  rt, mt, weight, note
  eventp := eventp + 4
  IF eventp >= eventvupb DO eventp := 0
  IF graphdata DO
  { LET ch = note=-1 -> 'B',
             note=-2 -> 'S',
             'M'
    writef("#%c %n %n %n*n", ch, rt, mt, weight)
  }
}

AND clearevents() BE   FOR i = 0 TO eventvupb BY 4 DO eventv!i := 0

AND calcrates() BE IF real_msecs >= calc_msecs DO
{ LET cgr, cgm, w = 0, 0, 0 // CG of recent events.
  LET count = 0             // Count of recent events.
  LET corr  = 0             // midi msecs distance from current play line.
  LET em    = 0             // Estimated midi msecs at now.
  //LET em1   = 0             // Estimated midi msecs 1 sec from now.
  LET cm    = 0             // Midi msecs now.
  LET ratediff = 0

  // Calculate new rates about 20 times per second
  calc_msecs := real_msecs + 50

  // Calculate weighted average of (rt, mt) pairs in eventv
  FOR i = 0 TO eventvupb BY 4 DO
  { LET e = @eventv!i // => [ rt, mt, weight, op]
    LET dt     = e!0 - real_msecs // Relative to now (to avoid overflow)
    LET mt     = e!1
    LET weight = e!2

    // Only consider events that occurred within the last 2 seconds
    IF eventv!0=0 | dt < -2_000 LOOP
    //writef("calcrates: rt=%5.3d mt=%5.3d weight=%n*n", rt, mt, weight)
    cgr := cgr + dt*weight
    cgm := cgm + mt*weight
    w := w + weight
    count := count+1
    //writef("calcrates: cgr=%5.3d cgm=%5.3d weight=%n*n", cgr, cgm, w)
  }

  //writef("calrates: count=%n*n", count)
  UNLESS w RETURN // No events so do not change the rates

  // Calculate the centre of gravity
  cgr, cgm := real_msecs+cgr/w, cgm/w

  // Calculate the estimated midi msecs error of CG relative to
  // the current estimated play line.
  corr := cgm - r2m_msecs(cgr, oer, oem, erate)
  // corr >0 if the soloist is ahead of estimated play line

  IF graphdata DO
    writef("#G %n %n*n", cgr, cgm)

//  writef("calrates: cgr=%5.3d cgm=%5.3d corr=%5.3d*n", cgr, cgm, corr)
//  writef("calrates: old oer=%5.3d oem=%5.3d erate=%5.3d*n",
//         oer, oem, erate)
//  writef("calrates: corr=%5.3d*n", corr)

  IF corr> 40 DO corr :=  40
  IF corr<-40 DO corr := -40
  erate := erate + corr

  // Limit the play rate but keep within 0.5 and 2.0
  IF erate>2_000 DO erate := 2_000
  IF erate<0_500 DO erate := 0_500

  // Make the new estimated play line pass through the CG
  oer, oem := cgr, cgm

//  writef("calrates: new oer=%5.3d oem=%5.3d erate=%5.3d*n",
//         oer, oem, erate)

  // oer, oem, erate now represent the new estimated play line,
  // passing through CG.

  // Choose a more recent origin for the new estimated play line
  oem := r2m_msecs(real_msecs, oer, oem, erate)
  oer := real_msecs

  // Choose the origin of the new correction play line
  ocm := r2m_msecs(real_msecs, ocr, ocm, crate)
  ocr := real_msecs

  // Choose the new rate for the correction play line
  crate := erate
  IF oem > ocm + 0_050 DO crate := erate + 0_200
  IF oem < ocm - 0_050 DO crate := erate - 0_200

  IF graphdata DO
  { writef("#E %n %n %n*n", oer, oem, erate)
    writef("#C %n %n %n*n", ocr, ocm, crate)
  }
//  writef("real_msecs=%9.3d oem=%5.3d erate=%5.3d ocm=%5.3d crate=%5.3d*n",
//          real_msecs, oem, erate, ocm, crate)
}

AND soundcofn(arg) BE
{ // Coroutine to read some microphone data
  LET soundv1024 = soundv + 1024
  LET soundvtop = soundv + soundvupb - 1024

  LET len = sys(Sys_sound, snd_waveInRead, micfd, micbuf, micbufupb+1)
    // micbuf contains signed 32-bit signed mono samples

  soundp := soundvupb

  UNLESS len DO
  { // if no sound data wait for more
    cowait(0)
    LOOP
  }

  UNLESS len=1024 DO
  { writef("Error: waveInRead returned %n samples*n", len)
  }

  // Some sound data is available
  // Get the current real time
  soundmsecs := getrealmsecs()

  // Shift the data in soundv
  FOR i = 0 TO soundvupb - 1024 DO soundv!i := soundv1024!i

  // Accummulate the new samples into the end of soundv
  FOR i = 0 TO len-1 DO
  { soundval := soundval + micbuf!i
    //IF i MOD 8 = 0 DO writef("*n%i4: ", i)
    //writef(" %i6", micbuf!i)
    soundvtop!i := soundval
  }
  //  newline()
  //writef("soundco: new data %9.3d*n", soundmsecs)
} REPEAT


AND playmidicofn(arg) BE
{ // This is the body of playmidico which is called by the clock loop
  // every time midi_msecs >= nextmidi_msecs

  // The playmidi coroutine returns TRUE when the end of performance
  // is reached.
  LET str = VEC 5
  LET midip = midilist

//  writef("playmidico: called arg=%n*n", arg)

  { // Main loop

    WHILE midip & ~quitting DO
    { // Output all midi triples that are now due
      LET mt = midip!1 // Midi time of next midi triple
      LET rt = m2r_msecs(mt, ocr, ocm, crate)

      //IF mt > stop_msecs BREAK

//writef("%9.3d playmidico: mt=%9.3d  rt=%9.3d*n", real_msecs, mt, rt)
      IF rt <= real_msecs DO
      { // This midi triple is now due so output it.
        LET triple = midip!2
        LET op     = triple & 255
        LET chan   = op & #x0F
        LET a1     = (triple>> 8) & 255
        LET a2     = (triple>>16) & 255
        LET is_note_on = (op&#xF0)=midi_note_on
        midip := !midip
//writef("%9.3d playmidico: triple %2x %2x %2x*n", real_msecs, op, a1, a2)

        // Unless calibrating, do not play the solo channels
        UNLESS calibrating IF ((1<<chan) & solochannels)~=0 LOOP
//writef("%9.3d playmidico: triple %2x %2x %2x*n", real_msecs, op, a1, a2)
//writef("%9.3d playmidico: mt=%9.3d [%5.3d %5.3d]*n",
//        real_msecs, mt, start_msecs, end_msecs)

        // Output the midi triple, but only note_on commands if mt is
        // between start_msecs and end_msecs.
        TEST is_note_on
        THEN IF start_msecs < mt <= end_msecs DO
             { wrmid3(mt, op, a1, a2)
               IF graphdata DO
                 writef("#N %n %n %s*n", real_msecs, mt, note2str(a1, str)) 
             }
        ELSE wrmid3(mt, op, a1, a2)

        LOOP
      }

//writef("%9.3d playmidico: end of performance*n", real_msecs)
      cowait(FALSE)      // Wait to be given control
    }

    // End of performance #################
    cowait(TRUE) REPEAT
  }
}

AND keycofn(arg) BE
{ // Coroutine to read the keyboard
  LET ch = sys(Sys_pollsardch)

  SWITCHON ch INTO
  { DEFAULT:
      writef("key %i3 '%c'*n", ch, ch)

    CASE '?':
    CASE 'h': CASE 'H':  // Help
      newline()
      writef("? H       Output help info*n")
      writef("Q         Quit*n")
      writef("B         A bar line is now*n")
      writef("<space>   A beat is now*n")
      writef("+         Play faster*n")
      writef("-         Play slower*n")
      writef("P         Pause/Play*n")
      writef("n G       Goto start of bar n*n")
      newline()
      LOOP

    CASE 'q':CASE 'Q':  // Quit
      writef("*nQuitting*n")
      quitting := TRUE
      LOOP

    CASE 'b':CASE 'B':
    { LET mt   = r2m_msecs(real_msecs-0_000, oer, oem, erate)
      LET bno  = msecs2barno(mt)
      LET bms  = 0
      LET err  = ?
      LET bms1 = barmsecs!(bno)
      LET bms2 = barmsecs!(bno+1)
      TEST mt < (bms1+bms2)/2
      THEN bms := bms1
      ELSE bno, bms := bno+1, bms2
      writef("%9.3d: bar  %i3      crate=%9.3d err = %6.3d*n",
                mt, bno, crate, mt-bms)
      addevent(real_msecs, bms, 127, -1) // -1 means Bar
      LOOP
    }

    CASE '*s': // Nearest beat
    { LET mt     = r2m_msecs(real_msecs-0_000, oer, oem, erate)
      LET beatno = msecs2beatno(mt) // beat number of most recent beat

      LET bms = -1   // Will be the midi time of the nearest beat
      LET weight = 0 // Will be the weight of this event
                     // =127 if exactly on a beat, =0 half way between beats
      LET b, bno, err = 1, ?, ?
      LET bms1  = beatmsecs!beatno     // Time of previous beat
      LET bms2  = beatmsecs!(beatno+1) // Time of next beat
      LET mid   = (bms1+bms2)/2
      LET range = (bms2-bms1)/2

      writef("*n %9.3d %8.3d beatno %i3 bms1=%6.3d bms2=%6.3d*n",
              real_msecs, mt, beatno, bms1, bms2)

      TEST mt < mid
      THEN { bms := bms1
             weight := (127 * (mid-mt))/range
           }
      ELSE { bms := bms2
             weight := (127 * (mt-mid))/range
             beatno := beatno+1
           }
      bno := msecs2barno(beatmsecs!beatno)
      FOR i = 0 TO 32 IF beatmsecs!(beatno-i)<=barmsecs!bno DO
      { b := i+1
        BREAK
      }
      writef(" %9.3d %8.3d beat %i3/%i3  erate=%9.3d w=%i3 err = %6.3d*n",
              real_msecs, mt, bno, b, erate, weight, mt-bms)
      addevent(real_msecs,    // Real time now
               bms,           // Midi time of nearest beat
               weight,           //
               -2) // -2 means Beat
      LOOP
    }

    CASE '+':
    CASE '=':
      clearevents()
      IF erate+50 <= 2_000 DO
      { // Calculate a new origin for the new estimated play line
        // and increase its rate a little
        oem := r2m_msecs(real_msecs, oer, oem, erate)
        oer := real_msecs
        erate := erate + 50

        // Choose a new origin for the new correction play line
        ocm := r2m_msecs(real_msecs, ocr, ocm, crate)
        ocr := real_msecs

        // Choose the new rate for the correction play line
        crate := erate
        IF oem > ocm + 0_050 DO crate := erate + 0_200
        IF oem < ocm - 0_050 DO crate := erate - 0_200

        IF graphdata DO
        { writef("#+ %n %n %n*n", oer, oem, erate)
          writef("#C %n %n %n*n", ocr, ocm, crate)
        }
      }
      sawritef(" erate = %9.3d*n", erate)
      LOOP

    CASE '-':
    CASE '_':
      clearevents()
      IF erate-50 >= 0_500 DO
      { // Calculate a new origin for the new estimated play line
        // and increase its rate a little
        oem := r2m_msecs(real_msecs, oer, oem, erate)
        oer := real_msecs
        erate := erate - 50

        // Choose a new origin for the new correction play line
        ocm := r2m_msecs(real_msecs, ocr, ocm, crate)
        ocr := real_msecs

        // Choose the new rate for the correction play line
        crate := erate
        IF oem > ocm + 0_050 DO crate := erate + 0_200
        IF oem < ocm - 0_050 DO crate := erate - 0_200

        IF graphdata DO
        { writef("#+ %n %n %n*n", oer, oem, erate)
          writef("#C %n %n %n*n", ocr, ocm, crate)
        }
      }
      sawritef(" erate = %9.3d*n", erate)
      LOOP

    CASE -3: // No keyboard character available
      ENDCASE
  }
  cowait(0)
} REPEAT



AND playmidi(midilist) BE
{ LET midiname = "/dev/midi"
  LET micname = "/dev/dsp1"
  LET micformat = 16  // S16_LE
  LET micchannels = 1 // Mono
  LET micrate = 44100 // Mic samples per second
  LET stop_msecs = end_msecs + 1_000 // Stop 1 midi second after end_msecs
  LET stdout = output()
  LET midi_msecs = 0
  LET nval = 0
  LET mb = VEC micbufupb

  totalerr, notecount := 0, 0

  // Set initial origins and rates
  ocr, ocm, crate := getrealmsecs(), 0, erate
  oer, oem := ocr, ocm

  soundco    := createco(soundcofn,    1000)
  playmidico := createco(playmidicofn, 1000)
  keyco      := createco(keycofn,      1000)

IF FALSE DO
{ writef("Testing r2m_msecs and m2r_msecs*n")
  FOR i = 1 TO 3 DO
  { LET rt = getrealmsecs()
    LET mt = r2m_msecs(rt, ocr, ocm, crate)
    writef("*nr2m_msecs(%9.3d, %9.3d, %9.3d, %9.3d) => %9.3d*n",
            rt, ocr, ocm, crate, mt)
    rt := m2r_msecs(mt, ocr, ocm, crate)
    writef("m2r_msecs(%9.3d, %9.3d, %9.3d, %9.3d) => %9.3d*n",
            mt, ocr, ocm, crate, rt)
    msdelay(500)
    crate := muldiv(crate, 1_100, 1_000)
  }
  abort(2000)
}

  micbuf := mb
  setfreqtab()

  notecov := getvec(127)
  notecoupb := 0 // No note coroutines yet

  FOR note = 0 TO 127 DO notecov!note := 0
  FOR note = 0 TO 127 IF 24<=note<=96 DO // 1C to 7C
  { LET notetimes = findtimes(note)
    // Only create note coroutines for notes played by the solists
    IF notetimes DO
    { notecoupb := notecoupb+1
      notecov!notecoupb := initco(notecofn, 1000, note, notetimes)
    }
  }
  notecop := 1 // Position in notecov of first coroutine, if any.

  midifd, micfd := 0, 0

  // Allocate the vector to hold the cummulative sound samples
  soundv := getvec(soundvupb)

  UNLESS soundv DO
  { writef("*nUnable to allocate soundv*n")
    abort(999)
  }

  FOR i = 0 TO soundvupb DO soundv!i := 0
  soundp, soundval := 0, 0

  //writef("*nsolo channel is %n*n", solochannel)

  UNLESS sys(Sys_sound, snd_test) DO
  { writef("The sound functions are not available*n")
    RETURN
  }

  // Open the Midi output device
  midifd := sys(Sys_sound, snd_midiOutOpen, midiname)

  UNLESS midifd>0 DO
  { writef("Unable to open the Midi device*n")
    GOTO fin
  }

  // Open the Microphone input device
  micfd := sys(Sys_sound, snd_waveInOpen, micname,
               micformat, micchannels, micrate)

  UNLESS micfd>0 DO
  { writef("Unable to open the Microphone device, rc=%n*n", micfd)
    GOTO fin
  }

  real_msecs := getrealmsecs()

  FOR chan = 0 TO 15 DO
  { wrmid3(midi_msecs, midi_control+chan, #x7B, 0)// Allnotes off
    wrmid3(midi_msecs, midi_control+chan, #x79, 0)// All controllers off
  }

//sawritef("Delaying for 500 msecs*n")
//  msdelay(500)
//sawritef("Delay done*n*n")

  // test microphone input
  IF FALSE DO
  { LET v = getvec(44100) // Buffer for 1 second of samples
    LET count = 0
    UNTIL count>=8195 DO
    { LET days, msecs = 0, 0
      LET hours, mins = 0, 0
      LET len = sys(Sys_sound, snd_waveInRead, micfd, micbuf, micbufupb+1)
      LET rt = getrealmsecs()
      hours := rt/(60*60*1000)
      mins  := rt/(60*1000) MOD 60
      msecs := rt MOD (60*1000)

      writef("len=%i6 %i2:%z2:%6.3d*n", len, hours, mins, msecs)
      //abort(1000)
      FOR i = 0 TO len-1 DO
      { LET w = micbuf!i // One signed 32-bit sample per element
        // Copy sample into v
        v!count := w
        count := count+1
        //IF i MOD 8 = 0 DO newline()
        //writef(" %i6", w)
      }
      //newline()
      msdelay(1)
      //abort(1000)
    }
    IF FALSE DO
    FOR i = 0 TO count-1 DO
    { IF i MOD 10 = 0 DO writef("*n%i6: ", i)
      writef(" %i6", v!i)
    }
    newline()
  }

  // Set initial origins and rates again.
  ocr, ocm, crate := getrealmsecs(), start_msecs, erate
  oer, oem := ocr, ocm

  { // Start of main timer loop

    IF quitting BREAK

    // Try to read some sound data into soundv
    callco(soundco, 1234)
    // If new sound data has been read mic_msecs will have been
    // set to the approximately real time of the latest sample.
    // mic_msecs is used by the note recognition coroutines.

    real_msecs := getrealmsecs()
    midi_msecs := r2m_msecs(real_msecs, ocr, ocm, crate)

    // Test for end of performance
    IF midi_msecs>=stop_msecs BREAK

    // Output any outstanding midi triples if any are due
    IF callco(playmidico, 2345) DO
    { // playmidico has reached the end of the performance
      BREAK
    }

    // Process any keyboard input
    callco(keyco, 3456)

    // Process up to 5 note recognisers
    FOR i = 1 TO notecoupb DO
    { callco(notecov!notecop, real_msecs)
      notecop := notecop + 1
      IF notecop>notecoupb DO notecop := 1
      IF i>=5 BREAK
    }

    // Calculate new parameters for the estimated and current play lines
    // based on their previous values and recent events in the
    // eventv circular buffer.
    calcrates(real_msecs)

    msdelay(5) // Delay 5 msecs (=1/200 sec)
  } REPEAT

  // Delay to let all sounds die down
  msdelay(1000)

  // All notes off all channels
  FOR chan = 0 TO 15 DO
    wrmid3(midi_msecs, midi_control+chan, 123, 0) // All notes off
  msdelay(500)

  IF notecount DO
    writef("*nAverage Midi-mic error %5.3d = %5.3d/%n*n",
            totalerr/notecount, totalerr, notecount)

fin:
  IF soundco DO { deleteco(soundco); soundco := 0 }
  IF keyco   DO { deleteco(keyco);   keyco   := 0 }
  FOR i = 1 TO notecoupb DO { deleteco(notecov!i);   notecov!i   := 0 }

  IF midifd>0 DO
    sys(Sys_sound, snd_midiOutClose, midifd) // Close the midi output device
  IF micfd>0 DO
    sys(Sys_sound, snd_waveInClose, micfd)   // Close the microphone device
  selectoutput(stdout)
  writef("*nEnd of performance*n")
}

AND r2m_msecs(real_msecs, or, om, rate) = VALOF
{ // Convert real time msecs to midi msecs
  // rate is the number of midi msecs per real second
  // offset is the real time in msecs at midi time zero
  LET mt = om + muldiv(real_msecs-or, rate, 1000)
  RESULTIS mt
}

AND m2r_msecs(midi_msecs, or, om, rate) = VALOF
{ // Convert midi msecs to real time
  // rate is the number of midi msecs per real second
  // offset is the real time in msecs at midi time zero
  LET rt = or + muldiv(midi_msecs-om, 1000, rate)
  RESULTIS rt
}

AND msecs2barno(m_msecs) = VALOF
{ IF currbarno<1 DO currbarno := 1
  IF currbarno>maxbarno DO currbarno := maxbarno
  WHILE m_msecs > barmsecs!currbarno DO currbarno := currbarno+1
  WHILE m_msecs < barmsecs!currbarno DO currbarno := currbarno-1
  RESULTIS currbarno
}

AND msecs2beatno(m_msecs) = VALOF
{ IF currbeatno<1 DO currbeatno := 1
  IF currbeatno>maxbeatno DO currbeatno := maxbeatno
  WHILE m_msecs > beatmsecs!currbeatno DO currbeatno := currbeatno+1
  WHILE m_msecs < beatmsecs!currbeatno DO currbeatno := currbeatno-1
  RESULTIS currbeatno
}

AND msdelay(msecs) BE IF msecs>0 DO
{ deplete(cos)
  sys(Sys_delay, msecs)
}

AND wrmid1(t, a) BE
{ IF optMtrace DO writef(" %7.3d: %x2*n", t, a)
  sys(Sys_sound, snd_midiOutWrite1, midifd, a)
}

AND wrmid2(t, a, b) BE
{ IF optMtrace DO writef(" %7.3d: %x2 %x2*n", t, a, b)
  sys(Sys_sound, snd_midiOutWrite2, midifd, a, b)
}

AND wrmid3(t, a, b, c) BE
{ IF optMtrace DO
  { LET op = a & #xF0
    LET chan = (a & #x0F) + 1
    writef(" %9.3d  %7.3d: %x2 %x2 %x2", real_msecs, t, a, b, c)
    IF op = #x80 DO
      writef("  chan %i2 Off %t4", chan, note2str(b, strv))
    IF op = #x90 DO
      writef("  chan %i2 On  %t4 vol %n", chan, note2str(b, strv), c)
    IF op = #xC0 DO
      writef("  chan %i2 Program change %n", chan, b)
    IF op = midi_control DO
    { writef("  chan %i2 Control %i3 %i3", chan, b, c)
      IF b=  0 DO writef(" Set Bank MSB=%n", c)
      IF b=  7 DO writef(" Set Volume MSB=%n", c)
      IF b= 32 DO writef(" Set Bank LSB=%n", c)
      IF b= 39 DO writef(" Set Volume LSB=%n", c)
      IF b=120 DO writef(" All sound off")
      IF b=121 DO writef(" All controllers off")
      IF b=123 DO writef(" All notes off")
    }
    newline()
  } 
  sys(Sys_sound, snd_midiOutWrite3, midifd, a, b, c)
}

AND prmidilist(list) BE WHILE list DO
{ writef("%9.3d: %x8*n", list!1, list!2)
  list := !list
}

AND note2str(n, str) = VALOF
{ // Convert a midi note number to a string (in str)
  // returning str as result.
  // eg note2str(61, str) => "4C#"
  LET oct = n/12 - 1 // 60 to 71 are in octave 4
  LET s = VALOF SWITCHON n MOD 12 INTO
  { DEFAULT: RESULTIS "??"
    CASE  0: RESULTIS "C "
    CASE  1: RESULTIS "C#"
    CASE  2: RESULTIS "D "
    CASE  3: RESULTIS "Eb"
    CASE  4: RESULTIS "E "
    CASE  5: RESULTIS "F "
    CASE  6: RESULTIS "F#"
    CASE  7: RESULTIS "G "
    CASE  8: RESULTIS "G#"
    CASE  9: RESULTIS "A "
    CASE 10: RESULTIS "Bb"
    CASE 11: RESULTIS "B "
  }
  str%0 := 3
  str%1 := oct>=0 -> oct + '0', '-'
  str%2 := s%1
  str%3 := s%2
  //writef("*nnote2str: n=%n oct=%n => *"%s*"*n", n, oct, str)
  RESULTIS str
}

AND editnoteoffs(list) = VALOF
{ // list is a list of sorted midi triples
  // This function removes note off events from the list
  // that would stop a note that should not yet be stopped
  // because of multiple note on events for that note

  LET p = @list
  // Allocate 16 vectors each of size 128 to hold counts for each
  // channel of how many times notes have be started but not yet
  // stopped.
  LET notecountv = VEC 16*128 // Notes currently playing
  FOR i = 0 TO 16*128 DO notecountv!i := 0

  WHILE !p DO
  { LET node = !p  // node is the next midi triple
    LET w = node!2
    LET op   = w & #xF0

    SWITCHON op INTO
    { DEFAULT: ENDCASE

      CASE midi_note_on:
      CASE midi_note_off:
      { LET chan = w & #x0F
        LET n = (w>>8) & #x7F
        LET i = chan<<7 | n
        LET count = notecountv!i
//writef("editnoteoffs: %x2 %x2 %x2 count=%n*n",
//        w&#xFF, w>>8 & #xFF, w>>16 & #xFF, count)

        TEST op=midi_note_on
        THEN notecountv!i := count+1
        ELSE { // Decrement the count
               notecountv!i := count-1
               IF count>1 DO
               { // Remove the triple from the list
//writef("removed*n")
                 !p := !node
                 unmk3(node)
                 LOOP
               }
             }
      }
    }
    p := node
  }

  FOR chan = 0 TO 15 FOR n = 0 TO 127 IF notecountv!(chan<<7 | n) DO
  { LET str = VEC 5
    writef("System error: note off event missing chan=%n note=%s*n",
            chan, note2str(n, str))
  }
  RESULTIS list
}

AND mergesort(list1) = VALOF
{ LET p, a, list2 = list1, list1, list1
//writef("*nmergesort:*n"); prmidilist(list1)
  UNLESS list1 & !list1  RESULTIS list1 // No sorting to do

  // list1 has at leat 2 elements

  // Split list1 into two halves list1 and list2
  { a := list2
    list2 := !list2
    p := !p
    IF p=0 BREAK
    p := !p
  } REPEATWHILE p

  !a := 0  // Terminate the left hand list
//writef("*nmergesort: list1*n"); prmidilist(list1)
//writef("*nmergesort: list2*n"); prmidilist(list2)
  RESULTIS mergelist(mergesort(list1), mergesort(list2))
}

AND mergelist(p, q) = VALOF
{ LET res = 0
  LET rese = @res

  UNLESS p RESULTIS q
  UNLESS q RESULTIS p

//writef("*nmergelist: p*n"); prmidilist(p)
//writef("mergelist: q*n"); prmidilist(q)

  { TEST p!1 <= q!1
    THEN { !rese := p
           rese := p
           p := !p
           IF p=0 DO { !rese := q; BREAK }
         }
    ELSE { !rese := q
           rese := q
           q := !q
           IF q=0 DO { !rese := p; BREAK }
         }
  } REPEAT

//writef("mergelist: res*n"); prmidilist(res)
  RESULTIS res
}

