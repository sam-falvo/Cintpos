/*

This is a program intended to read a .mus file representing a score
and write a corresponding MIDI file and/or play it on a MIDI device
possibly synchronising the accompanement with a solist using a
microphone.

Modified Implemention by Martin Richards (c) 12 Nov 2022

Usage: FROM,START/N,END/N,TADJ/N,TO/K,UPB/K/N,
       PP/S,LEX/S,TREE/S,PTREE/S,STRACE/S,NTRACE/S,MTRACE/S,
       MIDI/S,PLAY/S,ACC/S,PITCH/N,GD/S,WAIT/S,CALIB/S,DEBUG/N 

FROM      The root of the .mus file, eq beanbag for beanbag.mus
START/N   The number of the first bar to play
END/N     The number of the last bar to play
TADJ/N    The tempo adjustment as a percentage, eg 50 means half speed
TO/K      The destination file for messages
UPB/N/K   The size of the BGPM work space
PP/S      Just output the .mus file after BGPM expansion
LEX/S     Just output the .mus file as a sequence of lexical tokens
TREE/S    Just output the parse tree
PTREE/S   Output the parse tree after some processing has been performed
STRACE/S  Trace the syntax analyser processing
NTRACE/S  Trace the processing of notes
MTRACE/M  Trace the generation of midi statements
MIDI/S    Generate a .mid file base on the FROM file name
PLAY/S    Play the midi translation directly to /dev/midi1
ACC/S     Accompany a soloist using the microphone
PITCH/N   Adjust the pitch
GD/S      Generate graph data
WAIT/S    Wait before playing
CALIB/S   Calibrate the midi-microphone delay
DEBUG/N   Complement debug 0 to 9


Change history

2
12/05/2021
Minor modifications.

05/07/2019
Disallowed ties in shape lists. Any shape in a block must start and
end with a value. A star is automatically insert at the start and
the end if necessary.

24/05/2019
Reworking the definition of the MUS language and making corresponding
changes to playmus. This version makes considerable use of floating
point arithmetic particularly in the macrogenerator. Redesigns the
block mechanism and reimplements the treatments of shapes.

01/09/2011
Play lines now have an origin such as (oer, oem) giving a point in the
real-time midi-time graph through which the play line passes and a
rate such as erate giving the slope in midi msecs per real second.
The estimated play line based on recent microphone and keyboard events
is represented by oer, oem and erate. The current play line is
represented by ocr, ocm and crate. Both are updated about 20 times per
second and the values of ocr, ocm and crate are chosen to cause the
current play line to approach the estimated play line reasonably
quickly.

25/04/2011
Started implementation of new mechanism for shapes.

07/04/2011
Started to implement note recognition from the microphone.

26/03/2011
Started to implement keyboard commands in playmidi.

16/10/2009
Changed the macrogenerator comment (%) to skip to the end of the line
and then ignore all white space characters until the next non white
space character (which may of course be a %).

08/04/2009
Changed the midi data structure to be a linked list and used
mergesort to sort it.

30/03/2009
Changed the macrogenerator quotes from { and } to < and > to allow
curly brackets { and } to enclose blocks in .mus files. These will
save and restore certain settings such as volume and tempo during
translation.

20/02/2009
Added \pedon, \pedoff, \pedoffon, \softon, \softoff, \portaon and
\portaoff.

18/02/2009
Added the :s construct into shapes. \tempo(:s4. 120) means 120 dotted
quarter notes per minute which corresponds to 180 crochets per minute.

15/10/2008
Began replacing calls of scan functions by suitable calls of walktree.

25/09/08
Put in the MIDI/K option to write midi files, and the PLAY/S option to
play .mus files directly.

03/06/2008
Started the implementation of playmus.b


############# Precise definition of the parse tree structure #############


score     -> [0, Score, ln, name, conductor, parts, qlen]

conductor -> [0, Conductor, ln, block, qlen]

block     -> [-, Block, ln, notes, envs, qbeat1, qbeat2]  If env is not empty
          -> [-, Notes,  ln, notelist, qlen]              No shape data

parts     -> [0, Parts,  ln, partlist, qlen]     Lists are always
shapes    -> [0, Shapes, ln, shapelist, qlen]    operands of
envs      -> [0, Envs,   ln, envlist]            these four
notes     -> [0, Notes,  ln, notelist, qlen]     operators

partlist  -> 0
          -> [partlist, Part, ln, block, qlen, chan]
          -> [partlist, Solo, ln, block, qlen, chan]

shapelist -> 0 
          -> [shapelist, Num,   ln, value]
          -> [shapelist, Star,  ln]
          -> [shapelist, Space, ln, qlen]
          -> [shapelist, Null,  ln]

envlist   -> 0
          -> [envlist, Vibrateenv,    ln, parent, upb, v]
          -> [envlist, Vibrateadjenv, ln, parent, upb, v]
          -> [envlist, Vibampenv,     ln, parent, upb, v]
          -> [envlist, Vibampadjenv,  ln, parent, upb, v]
          -> [envlist, Volenv,        ln, parent, upb, v]
          -> [envlist, Voladjenv,     ln, parent, upb, v]
          -> [envlist, Tempoenv,      ln, parent, upb, v]
          -> [envlist, Tempoadjenv,   ln, parent, upb, v]
          -> [envlist, Legatoenv,     ln, parent, upb, v]
          -> [envlist, Legatoadjenv,  ln, parent, upb, v]
          -> [envlist, Delayenv,      ln, parent, upb, v]
          -> [envlist, Delayadjenv,   ln, parent, upb, v]

notelist -> 0          // A list of note items linked by the h1 chain
         -> [notelist, Altoclef, ln]
         -> [notelist, Arranger, ln, string]
         -> [notelist, Bank, ln, int1, int2]
         -> [notelist, Barlabel, ln, string]
         -> [notelist, Barline,  ln]
         -> [notelist, Bassclef, ln]
         -> [notelist, Block, ln, notes, envs, qbeat1, qbeat2]
         -> [notelist, Composer, ln, string]
         -> [notelist, Control, ln, int1, int2]
         -> [notelist, Delay, ln, notes, shapes, qlen]
         -> [notelist, Delayadj, ln, notes, shapes, qlen]
         -> [notelist, Doublebar, ln]
         -> [notelist, Instrument, ln, string]
         -> [notelist, Instrumentname, ln, string]
         -> [notelist, Instrumentshortname, ln, string]
         -> [notelist, Keysig, ln, letter:sharps:note, mode]
         -> [notelist, Legato, ln, notes, shapes, qlen]
         -> [notelist, Legatoadj, ln, notes, shapes, qlen]
         -> [notelist, Name, ln, string]
         -> [notelist, Nonvarvol, ln]
         -> [notelist, Note, ln, letter:sharps:note, qlen]
         -> [notelist, Notes, ln, notelist, qlen]
         -> [notelist, Notetied, ln, letter:sharps:note, qlen]
         -> [notelist, Null, ln]
         -> [notelist, Opus, ln, string]
         -> [notelist, Par, ln, parlist, qlen]
         -> [notelist, Patch, ln, int]
         -> [notelist, Partlabel, ln, string]
         -> [notelist, Pedoff, ln]
         -> [notelist, Pedon, ln]
         -> [notelist, Portaoff, ln]
         -> [notelist, Portaon, ln]
         -> [notelist, Repeatback, ln]
         -> [notelist, Repeatforward, ln]
         -> [notelist, Repeatbackforward, ln]
         -> [notelist, Rest, ln, qlen]
         -> [notelist, Softoff, ln]
         -> [notelist, Softon, ln]
         -> [notelist, Space, ln, qlen]
         -> [notelist, Tempo, ln, notes, shapes, qlen]
         -> [notelist, Tempoadj, ln, notes, shapes, qlen]
         -> [notelist, Tenorclef, ln]
         -> [notelist, Timesig, ln, int1, int2]
         -> [notelist, Title, ln, string]
         -> [notelist, Transposition, ln, int]
         -> [notelist, Trebleclef, ln]
         -> [notelist, Tuplet, ln, block, qlen]
         -> [notelist, Varvol, ln]
         -> [notelist, Vibamp, ln, notes, shapes, qlen]
         -> [notelist, Vibampadj, ln, notes, shapes, qlen]
         -> [notelist, Vibrate, ln, notes, shapes, qlen]
         -> [notelist, Vibrateadj, ln, notes, shapes, qlen]
         -> [notelist, Vol, ln, notes, shapes, qlen]
         -> [notelist, Voladj, ln, notes, shapes, qlen]
         -> [notelist, Volmap, ln, shapes]
*/

SECTION "Playmus"

GET "libhdr"
GET "playmus.h"

LET AAAfirstfn() BE RETURN
AND lastglobal() BE RETURN

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
  { // Append the extension ext, typically .mus
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
{ LET argv = VEC 75
  LET filenameroot = "t0"    // The filename without any extension.
  LET fromfilename = VEC 20  // For the name with the .mus extension.
  LET midifilename = VEC 20  // For the filename with the .mid extension.
  LET toname = 0
  LET optgenmidi = FALSE
  LET play = FALSE
  LET b   = VEC 64
  LET bln = VEC 64
  AND s1  = VEC 10
  AND dbv = VEC 9
  debugv := dbv

  // Initialise the bar and beat seld expanding vextors
  barsxv_upb, barsxv_v := 0, 0
  beatsxv_upb, beatsxv_v := 0, 0
  barsxv,  barmsecs  := @barsxv_upb,  0 // For safety
  beatsxv, beatmsecs := @beatsxv_upb, 0 // For safety
  msecsenv           := 0    // For safety

  envbits := 0
  
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
                *MIDI/S,PLAY/S,ACC/S,PITCH/N,GD/S,WAIT/S,CALIB/S,DEBUG/N",
		argv, 75) DO
     fatalerr("Bad arguments for PLAYMUS*n")

  pitch := 0
  transposition := 0

  // Set default values of the switch variables.
  // These can be complemented by the /S command arguments.
  optPp := FALSE
  optLex := FALSE
  optTree := FALSE
  optPtree := FALSE
  optStrace := FALSE
  optNtrace := TRUE//FALSE
  optMtrace := FALSE
  optgenmidi := FALSE
  play := FALSE
  accompany := FALSE
  graphdata := FALSE
  waiting := FALSE
  calibrating := FALSE
  
  FOR i = 1 TO 9 DO debugv!i := FALSE
  //debugv!1 := TRUE // Trace pushval 

  IF argv!0  DO filenameroot := argv!0       // FROM     The .mus file name root
  IF argv!1  DO startbarno   := !(argv!1)    // START/N  First bar to play
  IF argv!2  DO endbarno     := !(argv!2)    // END/N    Last bar to play
  IF argv!3  DO erate        := !(argv!3)    // TADJ/N   Tempo adjustment
  IF argv!4  DO toname       := argv!4       // TO/K
  IF argv!5  DO bg_baseupb   := !(argv!5)    // UPB/N/K  BGPM space
  IF argv!6  DO optPp        := ~optPp       // PP/S     Print macrogenerated text 
  IF argv!7  DO optLex       := ~optLex      // LEX/S    Trace lexical tokens
  IF argv!8  DO optTree      := ~optTree     // TREE/S   Print init parse tree
  IF argv!9  DO optPtree     := ~optPtree    // PTREE/S  Print part trees
  IF argv!10 DO optStrace    := ~optStrace   // STRACE/S Syntax analyser trace
  IF argv!11 DO optNtrace    := ~optNtrace   // NTRACE/S Note tracing
  IF argv!12 DO optMtrace    := ~optMtrace   // MTRACE/S Midi tracing while playing
  IF argv!13 DO optgenmidi   := ~optgenmidi  // MIDI/S   Generate a .mid file
  IF argv!14 DO play         := ~play        // PLAY/S   Play the midi directly
  IF argv!15 DO accompany    := ~accompany   // ACC/S    Accompany. synchronize
                                             //          using mic and keyboard
  IF argv!16 DO pitch        := !(argv!16)   // PITCH/N  Change pitch
  IF argv!17 DO graphdata    := ~graphdata   // GD/S     Generate graph data
  IF argv!18 DO waiting      := ~waiting     // WAIT/S   Wait before playing
  IF argv!19 DO calibrating  := ~calibrating // CALIB/S  Calibrate Midi-Mic delay
  IF argv!20 DO { LET n = !argv!20           // DEBUG/N  Complement debug 1 to 9
                  debugv!n := ~debugv!n
                }

  IF accompany DO play := TRUE

  // filenameroot must not contain any dots.
  FOR i = 1 TO filenameroot%0 IF filenameroot%i='.' DO
  { writef("*nThe FROM filename must not contain any dots*n")
    GOTO fin
  }

  concatext(filenameroot, ".mus", fromfilename)
  concatext(filenameroot, ".mid", midifilename)

  lineno := 0

  playmus_version := "Playmus V2.0 (21 Nov 2022)" // Used here and in writemidi.
  writef("*n%s*n", playmus_version)

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
  bg_base := 0              // Base of BGPM workspace

  currqbeat := 0
  startbarno, endbarno := 1, maxint/2
  start_msecs, end_msecs := 0, maxint
  solochannels := 0  // No soloists yet
  quitting := FALSE

  sourcestream := 0
  getstreams := 0
  tostream := 0
  bgpmco := 0

  // Space for parse tree, shape data, note data, etc.
  blklist, blkp, blkt, blkitem := 0, 0, 0, 0

  // Initialise the freelists of nodes of size 1 to 9
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
  prevlineno := 0

  msecsbase := -1
  oer, oem, erate := getrealmsecs(), 0, 1000
  ocr, ocm, crate := oer, oem, erate
  bg_baseupb := 100_000    // BGPM workspace upb
 
  IF bg_baseupb<5000 DO bg_baseupb := 5000
  bg_base := getvec(bg_baseupb)    // Allocate the BGPM workspace
  UNLESS bg_base DO
    fatalerr("Unable to allocate work space (upb = %n)*n", bg_baseupb)

  sourcefileno  := 1
  sourcenamev!1 := "built-in"

  { LET len = fromfilename%0
    LET str = newvec(len/bytesperword)
    IF str FOR i = 0 TO len DO str%i := fromfilename%i
    sourcefileno := sourcefileno+1
    sourcenamev!sourcefileno := str
  }

  sourcestream := findinput(fromfilename)
  lineno := (sourcefileno<<20) + 1
  // lineno is a packed word 12 buts of file number and 20 bits of line number 
  UNLESS sourcestream DO fatalerr("Unable to read file %s*n", fromfilename)

  tostream := sysout
  IF toname DO
  { tostream := findoutput(toname)
    UNLESS tostream DO fatalerr("Unable to write to file %s*n", argv!1)
  }

  // Create the macrogenerator coroutine.
  bgpmco := createco(bgpmfn, 2000)

  UNLESS bgpmco DO fatalerr("Unable to create bgpmco*n")

  selectinput(sourcestream)
  selectoutput(tostream)

  IF optPp DO                       // PP/S
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
//sawritef("Returned from formtree*n")
  IF optLex GOTO fin

IF FALSE DO
  IF optTree DO
  { writes("*nTree before calling calcqlen*n*n")
    prtree(tree, 0, 20)
    newline()
    abort(3981)
  }

  // Fill in all the qlen fields in the entire composition.
  scoreqlen := calcqlen(tree)
  
writef("scoreqlen=%n*n", scoreqlen)
//abort(2992)

//IF FALSE DO
  IF optTree DO
  { writes("*nTree after calling calcqlen but before calling trscore*n*n")
    prtree(tree, 0, 20)
    newline()
    //abort(3982)
  }

  IF errcount GOTO fin

  timesig_t, timesig_b := 4, 4
  qbeatsperbeat := 4096/timesig_b // ie 1024 for quarter note beats
  beatcount := 1
  currpartname := 0

  currbarno   := 1
  currbeatno  := 1
  variablevol := FALSE

  midilist := 0           // Initialise the list of midi items
  midiliste := @midilist  // Pointer to final link in the list
                          // used when appending midi items.

  // The score of the composition is now represented by its parse tree.

  writef("Calling barscan*n")
  //abort(3983)
  barscan(conductorpart)
  abort(5394)
  
  // Call trscore to convert the parse tree into a linked list
  // of midi statements.

writef("Calling trscore*n")
//abort(5395)
  UNLESS trscore(tree) GOTO fin

  // As a debugging aid optinally output the parse tree after translation.
  
  IF optPtree DO { writes("*nTree after calling trscore*n*n")
                   prtree(tree, 0, 20)
                   newline()
		   abort(6934)
                 }

  // The composition is now held as a list of midi statements which
  // are not yet been sorted.
writef("*nUnsorted midi data*n*n")
prmidilist(midilist)

  midilist := mergesort(midilist)
  
  // The midilist is now in sorted order.
writef("*nSorted midi data*n*n")
prmidilist(midilist)

  // Remove some note off events -- why???
//  midilist := editnoteoffs(midilist)
  
  //writef("*nEdited midi data*n*n")
  //prmidilist(midilist)

  //abort(1000)

  // Initialise the events list
  eventv := newvec(eventvupb)
  eventp := 0
  prevrt := 0
  FOR i = 0 TO eventvupb DO eventv!i := 0

  IF optgenmidi DO writemidi(midifilename, midilist) // Write a midi file.

  IF play DO playmidi(midilist) // Play the midi data directly to the device.
                                // typically /dev/midi1

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

  // Free all the block in the blklist.
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
  ELSE { UNLESS prevlineno=lineno DO
         { prevlineno := lineno
           bgpush(lineno)
         }
         bgpush(ch)
       }
}

AND bgpush(ch) = VALOF
{ UNLESS lineno=prevlineno DO
  { // The line number has changed so push the new
    // lineno value.
    prevlineno := lineno
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

  // If a newline character is read from file, lineno is not incremented
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
  prevlineno := lineno

  bgpush(bg_e)  // Save the old environment pointer
  bgpush(bg_t)  // and t
  // Push the macro name onto the stack
  bgpush(name%0+1)
  prevlineno := -1       // Cause bgpush to push a line value.
  FOR i = 1 TO name%0 DO bgpush(name%i)
  bgpush(1)           // The bodies of built in macros have no <fno/lno> item.
  prevlineno := -1       // Cause bgpush to push a line value.
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
{ // This is the main function of the bgpm coroutine bgpmco. It reads
  // characters from the current source stream and performs the macro
  // expansion passing the expanded characters to the lexical analyser
  // by calls of cowait(ch). The lexical analyser receives these characters
  // by statements such as: ch := callco(bcplco). The global lineno will
  // hold the line number and file number of the character just received.
  // The actual line number is held in the least significant 20 bits of
  // lineno, and the file number is placed to the left of this field.
  // All file numbers are greater than zero.

  // BGPM sets lineno to hold the line number and file number of each
  // character read from file, and embeds lineno value in any text held
  // in memory such as macro arguments. This ensures that lineno can
  // be correctly set when characters are read from both file or internal
  // memory. Macro arguments always start with a lineno value and this
  // includes the bodies of macros. When a macro is called, the lineno
  // value of the semicolon that caused the call is saved. This lineno
  // value is restored when the expansion of the macro completes. When a
  // lineno value is encountered while processing a macro body or argument
  // it just updates lineno which will thus be correctly set every time a
  // character is passed to the lexical analyser. The argument of cowait
  // in bgputch is always a character and not a lineno value, but it may
  // be endstreamch when the end of stream is reached or the special
  // character such as -4 if the BGPM detects a fatal error.

  // The lexical analyser maintains a circular buffer of the
  // most recent 64 characters. The characters are held in chbuf and
  // their corresponding lineno values in vector chbufln.

  rec_p, rec_l := level(), ret

  bg_s, bg_t, bg_h, bg_p := bg_base-1, bg_base+bg_baseupb, 0, 0
  bg_f, bg_e, bg_c       := 0, 0, 0

  // Put the builtin macros in the initial environment.
  
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
	   // getstreams -> [link, stream, lineno]
           lineno       := h3!getstreams // Restore lineno of ';' of $get!...;
           sourcestream := h2!getstreams
           getstreams   := h1!getstreams
           selectinput(sourcestream)
           LOOP

      CASE c_lquote:
         { LET d = 1 // The quote depth.
           { bg_ch := bggetch() // Get a character from file or memory.
             IF bg_ch<0 DO bg_error("Non character %n in quoted text", bg_ch)
	     // Quotes can be nested.
             IF bg_ch=c_lquote DO   d := d+1
             IF bg_ch=c_rquote DO { d := d-1; IF d=0 BREAK }
	     // Quoted text is just copied.
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
           prevlineno := lineno  // Set prevlineno appropriately.
           // It is now ready to load the zeroth argument of a call.
           LOOP

      CASE c_sep:                // '!'   Argument seperator
           IF bg_h=0 DO
           { // Treat ! as an ordinary character if not reading macro arguments.
             bgputch(bg_ch)
             LOOP
           }
           !bg_h := bg_s-bg_h    // Fill in the length of latest arg
           bg_h := bgpush(0)     // Start a new argument.
           // Every argument starts with a lineno value
           bgpush(lineno)        // Push the lineno value of the '!'
           prevlineno := lineno  // Set prevlineno appropriately.
           // It is now ready to load the next argument of a call.
           LOOP

      CASE c_arg:                // '#'  Copy an argument
         { LET lno = lineno      // Save the lineno value of #

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
             lineno := lno    // Restore the lineno value of the #.
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
           { // Copy text to current destination (memory or output),
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
{ lex1()
//  writef("lex() => %s*n", opstr(token))
}

AND lex1() BE
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
                // num is be used in shapes to represent a number of qbeats
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

    CASE '\': ch := rch()    // Reserved words, eg \vol
              token := lookupword(rdtag())
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
                IF ch='/' DO // Test for // comments
                { ch := rch() REPEATUNTIL ch='*n' | ch=endstreamch
                  tokln := lineno
                  LOOP
                }

                IF ch='**' DO  // test for /* comments
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
  hashval := hashval MOD nametablesize
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
  LET afternl = FALSE // Set to TRUE after first NL
  
  //writes("*n...")
  FOR p = chcount-63 TO chcount IF p>=0 DO
  { LET i = p&63
    LET k  = chbuf!i     // k is a valid character
    LET ln = chbufln!i   // ln is its lineno value
    IF k='*n' DO afternl := TRUE
    UNLESS afternl LOOP
    
    IF 0<k<=255 DO
    { //UNLESS ln=prevln DO
      //{ // The file number or linenumber has changed
      //  newline()
      //  //prlineno(ln)
      //  prevln := ln
      //  //writef("  ")
      //}
      //IF k='*n' DO
      //{ writef("<**n>")
      //  LOOP
      //}
      IF k=endstreamch DO
      { writef("<eof>*n")
        RETURN
      }
      wrch(k)
    }
  }
  newline()
}

AND rdtag() = VALOF
{ // A tag is a sequence of letters and underlines
  // Tags only occur after \s.
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

  //IF optLex DO writef("*nTesting the lexical analyser*n*n")

  lex()

  WHILE optLex DO
  { // Code to test the lexical analyser.
    prlineno(tokln)
    writef("  %s", opstr(token))

    SWITCHON token INTO
    { DEFAULT:
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
  IF errcount >= 0 DO fatalerr("Too many errors")
  longjump(rec_p, rec_l)
}

AND trerr(absq, mess, a, b, c, d, e, f) BE
{ // If absq >= 0 the error message will include bar and beat numbers.
  // tokln will hold the file/line number of the current tree node.
  writef("*nTranslation error near "); prlineno(tokln)
  newline()
  IF absq<0 & currqbeat>=0 DO absq := qscale(currqbeat)
  //IF absq>=0 DO
  //{ LET bno =  absq2barno(absq)
  //  writef(" at qbeat %n of bar %n", 123,456)//absq-barno2qbeat(bno), bno)
  //}
  newline()
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
{ // This reads a score consisting of a conductor part and set of
  // parallel instrumental parts. It returns
  // score   -> [-, Score, ln, name, conductor, parts, qlen]
  // or      =  0  If an error
  // where

  // conductor -> [0, Conductor, ln, block, qlen]

  // parts     -> [0, Parts, ln, partlist, qlen]
  
  // partlist   =  0
  //           -> [partlist, Part, ln, block, qlen]
  //           -> [partlist, Solo, ln, block, qlen]

  // block     -> [-, Block, ln, notes, envs, qbeat1, qbeat2]
  //           -> [-, Notes, ln, notelist, qlen]
  // notelist is a list of note items linked by the h1 fields.
  
  // All environments in a block relate to the same region from
  // qbeat1 to qbeat2, these can be accessed when needed via the
  // global currblock.
  
  // A block is just a Notes node if it contains no shape data,
  // except the block in the Conductor is always a Block node even
  // it no shape data.

  LET oldp, oldl = rec_p, rec_l

  LET scoreln   = tokln  // Lineno of \score
  LET name      = 0      // For the name of the score.
  LET score     = 0
  LET block     = 0
  LET part      = 0
  LET parts     = 0
  LET partlist  = 0      // For the list of parts.
  LET partliste = @partlist
  LET ln = 0
  LET op = 0

  conductorpart := 0     // To check that there is exactly one conductor.

  // Read:   \score scorename [ conductor and parts ]
//writef("token=%s*n", opstr(token))

  UNLESS token = s_score DO
  { synerr("rdscore: \score expected")
    RESULTIS 0
  }

  lex()
  name := rdstring()     // Get the name of this score.

  checkfor(s_lsquare, "'[' expected")

  // Initialise the outermost non shape environments
  
  { // Start of loop to read the conductor and other parts
    // which can occur in any order.
    ln := tokln  // Lineno of of the next part.
    op := token  // Remember the op
    block := 0
    
//writef("token=%s*n", opstr(token))
//abort(1000)

    // We now expect \conductor, \part, \solo or ']'
    SWITCHON op INTO
    { DEFAULT:
//        writef("op=%s executing BREAK*n", opstr(op))
        BREAK

      CASE s_conductor: // [0, Conductor, ln, block, qlen]
        // block   -> [-, Block, ln, notes, envs, qbeat1, qbeat2]
	// or      -> [-, Notes, ln, notelist, qlen]  if no shape data.
	// envs    -> [0, Envs, ln, envlist]
	// envlist -> [envlist, op, ln, parent, upb, v]   op is eg s_volenv
	// or      =  0

        // But for the Conductor node block is always a Block node.
	
        IF conductorpart DO
	  synerr("Only one conductor is allowed, conductorpart=%n*n",
	         conductorpart)

        lex()

        block := rdblock(rdnoteitem)
	// Force the block in the conductor to be a Block node
	// even when the conductor contains no shape data.
        // block   -> [-, Block, ln, notes, envs, qbeat1, qbeat2]
	UNLESS h2!block=s_block DO
	{ block := mk7(0, s_block,
                          ln,
                          block,
			  0,       // Give it an empty environment.
			  -1,      // qbeat1
                          -1)      // qbeat2
          IF optStrace DO
            writef("%i6 -> [%n, %s, %n:%n, %n, %n, %n, %n]*n",
            block,
	    h1!block,
            opstr(h2!block),
            fno(h3!block), lno(h3!block),
            h4!block,         // The body
            h5!block,         // The Envs node
            h6!block,         // qbeat1
            h7!block)         // qbeat2
	}

        // conductorpart -> [0, Conductor, ln, block, qlen]
        conductorpart := mk5(0, s_conductor,
                                ln,
                                block,
	 		        -1)      // qlen
        IF optStrace DO
          writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
          conductorpart,
	  h1!conductorpart,
          opstr(h2!conductorpart),
          fno(ln), lno(ln),
          h4!conductorpart,       // Body of the conductor
          h5!conductorpart)       // qlen

        conductorblock := h4!conductorpart

        LOOP

      CASE s_part:      // [-, Part, ln, block, qlen, chan]
      CASE s_solo:      // [-, Solo, ln, block, qlen, chan]
        // block   -> [-, Block, ln, notes, envs, qbeat1, qbeat2]
	// or      -> [-, Notes, ln, notelist, qlen]  if no shape data.
	// envs    -> [0, Envs, ln, envlist]
	// envlist -> [envlist, op, ln, parent, upb, v]   op is eg s_volenv
	// or      =  0
	  
        lex()

        block := rdblock(rdnoteitem)
        part := mk6(0, op,    // Part or Solo
                       ln,
                       block,
                       -1,     // qlen
	               -1)     // chan
        IF optStrace DO
           writef("%i6 -> [%n, %s, %n:%n, %n, %n, %n]*n",
                   part,
	           h1!part,
                   opstr(op),
                   fno(ln), lno(ln),
                   h4!part,
		   h5!part,
                   h6!part)
	h1!partliste := part  // Append a part to the partlist
        partliste := part
//abort(7778)
	LOOP
    }
//abort(8812)
  } REPEAT

  // Check for the final ']' of the score.
  checkfor(s_rsquare, "']' expected, token=%s", opstr(token))

  rec_p, rec_l := oldp, oldl

  UNLESS conductorpart DO fatalsynerr("A conductor is required") 
  UNLESS partlist DO fatalsynerr("At least one part or solo is needed") 

  // Form the Parts node.
  parts := mk5(0, s_parts, h3!partlist, partlist, -1)
  IF optStrace DO
    writef("%i6 -> [0, %s, %n:%n, %n, %n]*n",
            parts,
            opstr(h2!parts),
            fno(h3!parts), lno(h3!parts),
            h4!parts,
            h5!parts)

  score := mk7(0, s_score, scoreln, name, conductorpart, parts, -1)
  IF optStrace DO
    writef("%i6 -> [0, %s, %n:%n, *"%s*", %n, %n, -1]*n",
            score,
            opstr(h2!score),
            fno(scoreln), lno(scoreln),
            name,
            conductorpart,
            parts)


//writef("*n*nTree returned by rdscore*n")
//prtree(score, 0, 20)
//newline()
//newline()
//abort(1999)
  rec_p, rec_l := oldp, oldl

  RESULTIS score
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
//envbits := #b1110000
//writef("envbits=%32b*n", envbits)
//abort(5253)

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

    CASE s_lparen:
      // [-, Notes, ln, notelist, qlen]
      lex()
      //writef("CASE s_lparen: Calling rdnoteitem*n")
      //abort(1996)
      a := rdnotes()
      checkfor(s_rparen, "Syntax error in ( ... ) construct")
      //a := mk5(0, s_notes, ln, a, -1)

      //IF optStrace DO
      //  writef("%i6 -> [%n, Notes, %n:%n, %n, %n]*n",
      //          a,
      //          h1!a,
      //          opstr(h2!a),
      //          fno(ln), lno(ln),
      //          h4!a,
      //          h5!a)

      RESULTIS a

    CASE s_lcurly:
      // [-, Block, ln, notes, envs, qbeat1, qbeat2]
      // [-, Notes, ln, notelist, qlen]
      lex()
      a := rdblock(rdnotelist)
      checkfor(s_rcurly, "Syntax error in { ... } construct")
      RESULTIS a

    CASE s_lsquare:
      // [-, Par, ln, parlist, qlen]
      // parlist -> [parlist, Block, ln, notes, envs, qbeat1, qbeat2]
      // or      -> [parlist, Notes, ln, notelist, qlen]
      // or      =  0
      lex()
      a := rdparlist()
      checkfor(s_rsquare, "Syntax error in [ ... ] construct")
      a := mk5(0, s_par, ln, a, -1)
      IF optStrace DO
        writef("%i6 -> [%n, Par, %n:%n, %n, -1]*n",
                a,
		h1!a,
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
        writef("%i6 -> [%n, %s, %n:%n, <%c:%n:%n>, %n]*n",
                a,
		h1!a,
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
        writef("%i6 -> [%n, %s, %n:%n, %n]*n",
                a,
		h1!a,
                opstr(h2!a),
                fno(ln), lno(ln),
                h4!a)
      IF notelengthnum>=0 DO prevlengthnum := notelengthnum
      lex()
      RESULTIS a

    CASE s_null: // [-, Null, ln, qlen=0]
      a := mk4(0, op, ln, 0)
      IF optStrace DO
        writef("%i6 -> [%n, Null, %n:%n, 0]*n",
                a,
		h1!a,
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
        writef("%i6 -> [%n, %s, %n:%n]*n",
                a,
		h1!a,
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
        writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
                a,
		h1!a,
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
        writef("%i6 -> [%n, Patch, %n:%n, %n]*n",
                a,
		h1!a,
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
        writef("%i6 -> [%n, %s, %n:%n, %n, %s]*n",
                a,
		h1!a,
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
        writef("%i6 -> [%n, Transposition, %n:%n, %n]*n",
                a,
		h1!a,
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
        writef("%i6 -> [%n, %n:%n, %s]*n",
                a,
		h1!a,
                opstr(h2!a),
                fno(ln), lno(ln))
      RESULTIS a

    CASE s_volmap: // [-, op, ln, shape_list]
      lex()
      a := mk4(0, op, ln, rdshapes())
      IF optStrace DO
        writef("%i6 -> [%n, Volmap, %n:%n, %n]*n",
                a,
		h1!a,
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
        writef("%i6 -> [%n, %s, %n:%n, *"%s*"]*n",
                a,
		h1!a,
                opstr(h2!a),
                fno(ln), lno(ln),
                h4!a)
      RESULTIS a

  }
}

AND rdshapes(op) = VALOF
// This reads the shape data that occurs as the right hand operand of
// any shape operator held in op such as \vol or \tempo. It returns
// a Shapes node containing the list of shapes.
// It updates the bit pattern envbits indicates which shape environment
// nodes will be needed in the cuttent block.

// This function reads a shape item or a list of shape items enclosed
// in parentheses. It return a pointer -> [-,Shapelist,ln,shapelist,qlen]
// where shapelist is a list of items chained together by their h1 fields.
// A shape value is one of the following

//          [-, Num,      ln,  val]    A floating point shape value
//          [-, Star,     ln]
//          [-, Space,    ln, qlen]
//          [-, Rest,     ln, qlen]    Not used - replaced by Space
//          [-, Null,     ln,    0]    Not used - replaced by Space

// Shape values are scaled by sfac/1024. So, for instance,
// if sfac=512 (corresponding to an eighth note or quaver), a tempo
// value of 120 would be halved giving a rate of 60 quarter notes
// per minute. sfac can be changed within a shape sequence by
// items such as :256 or :s8
// Tempo values are stored in tenpo environments after they are
// scaled and there represent a rate of the number of quater notes
// (or 1024 qbeats) per minute.
// The main purpose of scaling is to allow, for instance, dotted
// quaver = 138 to be specified by \tempo(:s8. 138). Scaling is
// typically never used with any other shape operators.

{ LET list = 0           // For the chain of note items
  LET liste = @list
  LET firsttoken = token // To remember whether it was Lparen.
  LET shapelistln = tokln
  LET fno1 = tokln>>20         // The lineno of the first token of the shape
  LET lno1 = tokln & #xFFFFF
  LET item = 0
  LET sfac = 1024
  LET prevlen = prevlengthnum  // Save the the previous length number.
  LET envbit = 0

  prevlengthnum := 4           // Set the prev length number to 4 (a quarter note)

  IF token=s_lparen DO lex() // Skip over '(' if present.

  { // Start of loop to read shape items.
    LET ln = tokln

    SWITCHON token INTO
    { DEFAULT:
        BREAK

      CASE s_space:      // eg s.      These three create Space nodes.
      CASE s_rest:       // eg r4..
      CASE s_null:       // z
      { LET qlen = 0
        UNLESS token=s_null DO
        { qlen := note2qlen(notelengthnum, prevlengthnum, dotcount)
          IF notelengthnum>=0 DO prevlengthnum := notelengthnum
        }
        item := mk4(0, s_space, tokln, qlen)
        IF optStrace DO
          writef("%i6 -> [%n, %s, %n:%n, %n]*n",
                item,
		h1!item,
                opstr(h2!item),
                fno(ln), lno(ln),
                h4!item)
        lex()
        ENDCASE
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
          lex()
          LOOP
        }

        synerr("A space or number is expected after ':' in shape sequence")
        LOOP

      CASE s_num:
        numval := muldiv(numval, sfac, 1024) // Apply the current scaling value.
        item := mk4(0, token, tokln, numval)
        IF optStrace DO
          writef("%i6 -> [%n, %s, %n:%n, %13.6f]*n",
                item,
		h1!item,
                opstr(h2!item),
                fno(ln), lno(ln),
                h4!item)
        lex()
	ENDCASE

      CASE s_star:
        item := mk3(0, token, tokln)
        // Star items are not scaled by sfac.
        IF optStrace DO
        { writef("%i6 -> [%n, %s, %n:%n]*n",
                  item,
		  h1!item,
                  opstr(h2!item),
                  fno(ln), lno(ln))
        }
        lex()
        ENDCASE
    }
    
    // Append item to the list
    !liste := item
    liste := item
  } REPEATWHILE firsttoken=s_lparen

  IF firsttoken=s_lparen DO
  { UNLESS token=s_rparen DO
      synerr("Missing ')' in a shape sequence")
    lex()
  }

  prevlengthnum := prevlen
  item := mk5(0, s_shapes, shapelistln, list, -1)
  IF optStrace DO
    writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
            item,
	    h1!item,
	    opstr(h2!item),
            fno(shapelistln), lno(shapelistln),
            h4!item,
	    h5!item)

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

AND op2envbit(op) = VALOF SWITCHON op INTO
{ DEFAULT:           RESULTIS 0
  CASE s_vibrate:    RESULTIS b_vibrate
  CASE s_vibrateadj: RESULTIS b_vibrateadj
  CASE s_vibamp:     RESULTIS b_vibamp
  CASE s_vibampadj:  RESULTIS b_vibampadj
  CASE s_vol:        RESULTIS b_vol
  CASE s_voladj:     RESULTIS b_voladj
  CASE s_tempo:      RESULTIS b_tempo
  CASE s_tempoadj:   RESULTIS b_tempoadj
  CASE s_legato:     RESULTIS b_legato
  CASE s_legatoadj:  RESULTIS b_legatoadj
  CASE s_delay:      RESULTIS b_delay
  CASE s_delayadj:   RESULTIS b_delayadj
}

AND rdnoteitem() = VALOF
{ // Return the parse tree of a note item or zero if none found.

  LET a = rdnoteprim() // Read a note up to the first dyadic
                       // operator, if any.
  UNLESS a RESULTIS 0

  { // Look for a shape operator such as \tup or \vol
    LET op = token
    LET ln = tokln          // Lineno of the operator if any.

    SWITCHON op INTO
    { DEFAULT:
        RESULTIS a // Just return the primary if no shape operator

      // The infixed shape operators
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
      CASE s_delayadj: // [-, op, ln, notes, shapelist, qlen]
        envbits := envbits | op2envbit(op)
	//writef("envbits=%12b*n", envbits)
	UNLESS h2!a=s_block | h2!a=s_notes DO
	{ a := mk5(0, s_notes, h3!a, a, -1)
          // a-> [-, Notes, ln, a, -1]
          IF optStrace DO
            writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
                    a,
	 	    h1!a,
		    opstr(op),
		    fno(ln), lno(ln),
                    h4!a,
                    h5!a)
	}
        lex()
        a := mk6(0, op, ln, a, rdshapes(op), -1)
        // a-> [-, op, ln, notes, shapelist, qlen]
        IF optStrace DO
          writef("%i6 -> [%n, %s, %n:%n, %n, %n, %n]*n",
                  a,
		  h1!a,
		  opstr(op),
		  fno(ln), lno(ln),
                  h4!a,
                  h5!a,
                  h6!a)
        LOOP

      CASE s_tuplet: // [-, Tuplet, ln, block, qlen]
                     // eg (C D E)\tup s4
                     // or (C D E)\tup 1024
                     // or (C D E)\tup(s8. s.) 
        lex()
	// a is the parse tree of the left operand of \tup
	// Force it to be a Block or Notes node.
	UNLESS op=s_block | op=s_notes DO
        { a := mk5(0, s_notes, tokln, a, -1) // Make it into a Notes node.
          IF optStrace DO
            writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
	           a,
	 	   h1!a,
		   opstr(h2!a),
                   fno(ln), lno(ln),
                   h4!a,
                   h5!a)
	}
        // Read the right hand operand of \tup just returning it length.
        a := mk5(0, s_tuplet, ln, a, rdtupletqlen()) // Create the tuplet node.
        IF optStrace DO
          writef("%i6 -> [%n, Tuplet, %n:%n, %n, %n]*n",
                  a,
		  h1!a,
                  fno(ln), lno(ln),
                  h4!a,
                  h5!a)
        LOOP
    }
  } REPEAT

  RESULTIS 0
}

AND rdparlist() = VALOF
{ // This reads a list of notelists, hopefully terminated by ']'
  // It returns
  // a pointer -> [parlist, Notelist, ln, notelist]
  // or zero
  LET list = 0
  LET liste = @ list

  { LET a = rdblock(rdnoteitem)

    UNLESS a BREAK

    !liste := a
    liste := a
  } REPEAT

  RESULTIS list
}

AND rdblock(rdbodyfn) = VALOF
{ // rdbodyfn is rdnotes when reading a conductor, part or solo
  // or a block enclosed in curly brackets. When reading a component
  // of a par construct, otherwise rdbodyfn is rdnoteitem.
  // If rdbodyfn detects shape data, the result is converted to a block,
  // it otherwise it is converted to a Notes node, if necessary.
  // If a Block is formed its envs field will contain empty
  // environments of every type detected. The environments will be
  // completed by a later call of findshapedata.

  LET ln = lineno
  LET res = 0
  LET envs = 0         // For the Envs environment nodes
  LET envlist = 0      // if shape data is found.
  LET prevenvbits = envbits
  envbits := 0
  
  res := rdbodyfn()
  IF res UNLESS h2!res=s_block | h2!res=s_notes DO
  { res := mk6(0, s_notes, ln, envlist)
    IF optStrace DO
      writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
              envlist,
              h1!envlist,
	      opstr(h2!envlist),
              fno(h3!envlist), lno(h3!envlist),
              h4!envlist,
              h5!envlist)
  }
  
//  writef("envbits=%32b after calling rdbodyfn*n", envbits)

  // If envbits is nonzero create the needed environment nodes.
  WHILE envbits DO
  { LET bit = envbits & -envbits
    LET envtype = VALOF SWITCHON bit INTO
    { DEFAULT:           RESULTIS 0
      CASE b_vibrate:    RESULTIS s_vibrateenv
      CASE b_vibrateadj: RESULTIS s_vibrateadjenv
      CASE b_vibamp:     RESULTIS s_vibampenv
      CASE b_vibampadj:  RESULTIS s_vibampadjenv
      CASE b_vol:        RESULTIS s_volenv
      CASE b_voladj:     RESULTIS s_voladjenv
      CASE b_tempo:      RESULTIS s_tempoenv
      CASE b_tempoadj:   RESULTIS s_tempoadjenv
      CASE b_legato:     RESULTIS s_legatoenv
      CASE b_legatoadj:  RESULTIS s_legatoadjenv
      CASE b_delay:      RESULTIS s_delayenv
      CASE b_delayadj:   RESULTIS s_delayadjenv
    }
    //writef("envbits=%32b  bit=%32b*n", envbits, bit)
    envbits := envbits-bit
    
    // Create an empty environment node of the right type
    // but without a parent yet.
    envlist := mk6(envlist, envtype, ln, 0, 0, 0)
    IF optStrace DO
      writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
              envlist,
              h1!envlist,
	      opstr(h2!envlist),
              fno(h3!envlist), lno(h3!envlist),
              h4!envlist,
              h5!envlist)
  }
  //abort(1998)

  IF envlist DO
  { // The body contained shape data so a block must be created.
  
    envlist := mk5(0, s_envs, ln, envlist, -1)
    IF optStrace DO
        writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
                res,
		h1!res,
		opstr(h2!res),
                fno(h3!res), lno(h3!res),
                h4!res,
                h5!res)
    res := mk6(0, s_block, ln, res, envlist, -1)
    IF optStrace DO
        writef("%i6 -> [%n, %s, %n:%n, %n, %n, %n]*n",
                res,
		h1!res,
		opstr(h2!res),
                fno(h3!res), lno(h3!res),
                h4!res,
                h5!res,
                h6!res)
  }
  
  envbits := prevenvbits
  // Return either a Block or Notes node.
  RESULTIS res
}

AND rdnotes() = VALOF
// This returns a Notes node containing a list of note items
// linked though the h1 field.
{ LET ln = tokln
  LET list = 0
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
  list := mk5(0, s_notes, ln, list, -1)

  IF optStrace DO
    writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
            list,
            h1!list,
	    opstr(h2!list),
            fno(ln), lno(ln),
            h4!list,
            h5!list)

  RESULTIS list  // return the Notes node
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
  CASE s_block:               RESULTIS "Block"
  CASE s_colon:               RESULTIS "Colon"
  CASE s_composer:            RESULTIS "Composer"
  CASE s_conductor:           RESULTIS "Conductor"
  CASE s_control:             RESULTIS "Control"
  CASE s_delay:               RESULTIS "Delay"
  CASE s_delayadj:            RESULTIS "Delayadj"
  CASE s_delayadjenv:         RESULTIS "Delayadjenv"
  CASE s_delayenv:            RESULTIS "Delayenv"
  CASE s_doublebar:           RESULTIS "Doublebar"
  CASE s_eof:                 RESULTIS "Eof"
  CASE s_envs:                RESULTIS "Envs"
  CASE s_instrument:          RESULTIS "Instrument"
  CASE s_instrumentname:      RESULTIS "Instrumentname"
  CASE s_instrumentshortname: RESULTIS "Instrumentshortname"
  CASE s_keysig:              RESULTIS "Keysig"
  CASE s_lcurly:              RESULTIS "Lcurly"
  CASE s_legato:              RESULTIS "Legato"
  CASE s_legatoadj:           RESULTIS "Legatoadj"
  CASE s_legatoadjenv:        RESULTIS "Legatoadjenv"
  CASE s_legatoenv:           RESULTIS "Legatoenv"
  CASE s_notes:               RESULTIS "Notes"
  CASE s_shapes:              RESULTIS "Shapes"
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
  CASE s_parts:               RESULTIS "Parts"
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
  CASE s_tempoadjenv:         RESULTIS "Tempoadjenv"
  CASE s_tempoenv:            RESULTIS "Tempoenv"
  CASE s_tenorclef:           RESULTIS "Tenorclef"
  CASE s_timesig:             RESULTIS "Timesig"
  CASE s_title:               RESULTIS "Title"
  CASE s_tqshiftenv:          RESULTIS "Tqshiftenv"
  CASE s_transposition:       RESULTIS "Transposition"
  CASE s_trebleclef:          RESULTIS "Trebleclef"
  CASE s_tuplet:              RESULTIS "Tuplet"
  CASE s_varvol:              RESULTIS "Varvol"
  CASE s_vibrate:             RESULTIS "Vibrate"
  CASE s_vibrateadj:          RESULTIS "Vibrateadj"
  CASE s_vibrateadjenv:       RESULTIS "Vibrateadjenv"
  CASE s_vibrateenv:          RESULTIS "Vibrateenv"
  CASE s_vibamp:              RESULTIS "Vibamp"
  CASE s_vibampadj:           RESULTIS "Vibampadj"
  CASE s_vibampadjenv:        RESULTIS "Vibampadjenv"
  CASE s_vibampenv:           RESULTIS "Vibampenv"
  CASE s_vol:                 RESULTIS "Vol"
  CASE s_voladj:              RESULTIS "Voladj"
  CASE s_voladjenv:           RESULTIS "Voladjenv"
  CASE s_volenv:              RESULTIS "Volenv"
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
  //   next is used to chain nodes in lists together.
  //   op is the node operator, eg s_notelist, s_note etc.
  //   ln is the lineno value for this node.
  //   The other fields are node dependent.
  // n is the depth of node t.
  // d is the maximum depth that prtree will print.
  
  LET v = TABLE 0,0,0,0,0, 0,0,0,0,0,
                0,0,0,0,0, 0,0,0,0,0
  LET link, op, ln, a1, a2 = ?, ?, ?, ?, ?
  LET opname = ?

  writef("%n: ", t) // This is primarily to aid debugging.

  IF n>=d DO { writes("Etc"); RETURN  }
  IF t=0  DO { writes("Nil"); RETURN  }

  // Most nodes are of the form: [link, op, ln, a1 a2, a3,.. ]
  link, op, ln, a1, a2 := h1!t, h2!t, h3!t, h4!t, h5!t
  opname := opstr(op)

  // The indentation text has already been printed.

  SWITCHON op INTO
  { DEFAULT:
         writef("%t8   ", opname)
         prlineno(ln)
         RETURN

    CASE s_num:      // [-, Num, ln, val]
                     writef("%t8 %9.3f   ", opname, a1); prlineno(ln); RETURN

    CASE s_star:     // [-, Star, ln]
                     writef("%t8   ", opname); prlineno(ln); RETURN

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
      writef("%t8 qlen=%n   ", opname, a1); prlineno(ln)
      RETURN

    CASE s_null:   // [-, null, ln]
      writef("%t8 ", opname); prlineno(ln)
      RETURN

    CASE s_control:       // [-, Control, ln, a, b]
                          writef("Control (%n %n)", h4!t, h5!t);  RETURN
    CASE s_timesig:       // [-, Timesig, ln, top, bottom]
                          writef("Timesig (%n %n)", h4!t, h5!t);  RETURN

    CASE s_bank:          // [-, Bank, ln, a, b]
                          writef("Bank    (%n %n)", h4!t, h5!t);  RETURN

    CASE s_patch:         // [-, Patch, ln, a]
                          writef("Patch   %n", h4!t);             RETURN

    CASE s_transposition: // [-, Transposition, ln, a]
                          writef("Transposition (%n)", h4!t);     RETURN

    CASE s_keysig:
      // [-, Keysig, ln, [0, Note, ln, <letter, sharps, noteno>, mode]]
      writef("Keysig (")
      prnote(h4!a1>>16, (h4!a1>>8) & 255, 0, -1)
      TEST a2=s_major THEN writes(" Major)")
                      ELSE writes(" Minor)")
      RETURN

    // Operator with a string argument
    CASE s_title:                // [-, Title, ln, string]
    CASE s_composer:             // [-, Composer, ln, string]
    CASE s_arranger:             // [-, Arranger, ln, string]
    CASE s_opus:                 // [-, Opus, ln, string]
    CASE s_name:                 // [-, Name, ln, string]                eg Piano LH
    CASE s_instrument:           // [-, Instrument, ln, string]
    CASE s_instrumentname:       // [-, Instrumentname, ln, string]      eg Flute
    CASE s_instrumentshortname:  // [-, Instrumentshortname, ln, string] eg Fl
    CASE s_barlabel:             // [-, Barlabel, ln, string]
    CASE s_partlabel:            // [-, Partlabel, ln, string]
      writef("%t7 *"%s*"    ", opname, a1); prlineno(ln)
      RETURN

    CASE s_parts:     // [-, Parts,   ln, partlist,  qlen]
    CASE s_notes:     // [-, Notes,   ln, notelist,  qlen]
    CASE s_shapes:    // [-, Shapes,  ln, shapelist, qlen]
    CASE s_parlist:   // [-, Parlist, ln, block,     qlen]
    CASE s_envs:      // [-, Envs,    ln, envlist]
    { LET p = h4!t
      writef("%s ", opname)
      UNLESS op=s_envs DO writef("qlen=%n  ", h5!t)
      prlineno(ln)

      // Print each item in the list as though they were
      // operands of the current operator.

      WHILE p DO
      { v!n := h1!p -> "! ", "  "
        prnltree(p, v, n, d)
        p := !p
      }

      RETURN
    }
    

    CASE s_conductor: // [-, Conductor, ln, block, qlen]
      writef("%t7 qlen=%n ", opname, h5!t)
      writef("   ")
      prlineno(ln)
      v!n := "  "
      prnltree(h4!t, v, n, d)
      RETURN

    CASE s_part:      // [-, Part, ln, block, qlen, chan]
    CASE s_solo:      // [-, Solo, ln, block, qlen, chan]
      writef("%t7 qlen=%n chan=%n ", opname, h5!t, h6!t)
      writef("   ")
      prlineno(ln)
      v!n := "  "
      prnltree(h4!t, v, n, d)
      RETURN

    CASE s_par:       // [-, Par, ln, parlist, qlen]
      writef("%t7 qlen=%n ", opname, h5!t)
      writef("   ")
      prlineno(ln)
      v!n := "  "
      prnltree(h4!t, v, n, d)
      RETURN

    CASE s_block:     // [-, Block, ln, notes, envs, qbeat1, qbeat2]
      // envlist is the list of environments belonging to this block.
      writef("%t7 qbeat1=%n qbeat2=%n ", opname, h6!t, h7!t)
      prlineno(ln)
      v!n := "! "
      prnltree(h4!t, v, n, d)
      v!n := "  "
      prnltree(h5!t, v, n, d) // The environment list
      RETURN

    CASE s_score:     // [-, Score, ln, name, conductor, parts, qlen]
      writef("%s *"%s*" qlen=%n   ", opname, h4!t, h7!t)
      prlineno(ln); newline()
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
    CASE s_vibrate:    // [-, Vibrate,    ln, notelist, shapes, qlen]
    CASE s_vibrateadj: // [-, Vibrateadj, ln, notelist, shapes, qlen]
    CASE s_vibamp:     // [-, vibadj,     ln, notelist, shapes, qlen]
    CASE s_vibampadj:  // [-, Vibampadj,  ln, notelist, shapes, qlen]
    CASE s_vol:        // [-, Vol,        ln, notelist, shapes, qlen]
    CASE s_voladj:     // [-, Voladj,     ln, notelist, shapes, qlen]
    CASE s_tempo:      // [-, Temp,       ln, notelist, shapes, qlen]
    CASE s_tempoadj:   // [-, tempadj,    ln, notelist, shapes, qlen]
    CASE s_legato:     // [-, Legato,     ln, notelist, shapes, qlen]
    CASE s_legatoadj:  // [-, Legatoadj,  ln, notelist, shapes, qlen]
    CASE s_delay:      // [-, Delay,      ln, notelist, shapes, qlen]
    CASE s_delayadj:   // [-, Delayadj,   ln, notelist, shapest, qlen]
    //CASE s_tqshift:    // [-, Tqshift,    ln, notelist, shapest, qlen]
      writef("%s   qlen=%n   ", opname, h6!t)
      prlineno(ln); newline()
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

    CASE s_vibrateenv:    // [-, Vibrateenv,    ln, upb, v]
    CASE s_vibrateadjenv: // [-, Vibrateadjenv, ln, upb, v]
    CASE s_vibampenv:     // [-, vibadjenv,     ln, upb, v]
    CASE s_vibampadjenv:  // [-, Vibampadjenv,  ln, upb, v]
    CASE s_volenv:        // [-, Volenv,        ln, upb, v]
    CASE s_voladjenv:     // [-, Voladjenv,     ln, upb, v]
    CASE s_tempoenv:      // [-, Tempoenv,      ln, upb, v]
    CASE s_tempoadjenv:   // [-, tempadjenv,    ln, upb, v]
    CASE s_legatoenv:     // [-, Legatoenv,     ln, upb, v]
    CASE s_legatoadjenv:  // [-, Legatoadjenv,  ln, upb, v]
    CASE s_delayenv:      // [-, Delayenv,      ln, upb, v]
    CASE s_delayadjenv:   // [-, Delayadjenv,   ln, upb, v]
    //CASE s_tqshiftenv:    // [-, Tqshiftenv,    ln, upb, v]
      writef("%s   ", opname)
      prlineno(ln)
      writef("  upb=%n v=%n", h4!t, h5!t)
      RETURN
    
    CASE s_volmap: // [-, Volmap, ln, shapes]
      writef("%s   ", opname)
      prlineno(ln); newline()
      FOR j = 0 TO n-1 DO writes( v!j )
      writes("**-")
      v!n := "  "
      prtree(a1, n+1, d)
      RETURN       

    CASE s_tuplet: // [-, Tuplet, ln, block, qlen]
                   // eg         (4C4 D E)\tup S4
                   // previously S4\tup(4C4 D E)
      writef("%s   qlen=%n  ", opname, a2)
      prlineno(ln); newline()
      FOR j = 0 TO n-1 DO writes( v!j )
      writes("**-")
      v!n := "  "
      prtree(a1, n+1, d)
      RETURN       
  }
}

AND prnltree(t, v, n, d) BE
{ // On a new line print the current indentation text
  // followed by node t at depth n+1.
  newline()
  FOR j = 0 TO n-1 DO writes( v!j )
  writes("**-")
  //v!n := h1!t ->"! ","  "
  prtree(t, n+1, d)
}

.

SECTION "Trans"

// This section contains functions concerned with the translation
// of the parse tree to the linked list of midi events.

GET "libhdr"
GET "playmus.h"

LET trscore(t) = VALOF
{ // This translates the entire composition into a list of midi
  // statement, returning TRUE if successful. The score contains
  // a conductor part and the collection of other parts. The
  // conductor part specifies the overall structure of the score
  // including details of time signatures and the placement of
  // bar lines. Positions within the score are measured in qbeats
  // with a region of 1024 qbeats corresponding to the length of
  // a crotchet (or quarter note). 

  // t         -> [-, Score, ln, name, conductor, parts, qlen]
  // conductor -> [-, Conductor, ln, block, qlen]
  // parts     -> [-, Parts, ln, partlist,qlen]
  // partlist  =  0
  // or        -> [partlist, Part, ln, block, qlen]
  // or        -> [partlist, Solo, ln, block, qlen]
  // block     -> [-, Block, ln, notes, envs, qbeat1, qbeat2]
  // envs      -> [0, Envs, ln, envlist]
  // envlist   -> [envlist, op, parent, upb, v]
  // or        -> 0
  // where op is an environment name such as s_volenv.
  // parent is zero or points to the environment of the same kind
  //        one level out.
  // The pair [upb,v] is a self expanding vector to hold the shape data.
  // upb is the upper bound of v and v!0 holds the subscript of the
  // last element used in v. Items in v are pairs [qpos,val] starting
  // at subscript 1 with the first qpos=0 and the last qpos=qlen.
  // Elements added using sxpush(@upb, x).
  // envlist is the list of non empty environments belonging to the block.

  LET conductor = h5!t     // -> [-, Conductor, ln, block, qlen]
  LET parts     = h6!t     // -> [-. Parts, ln, partlist, qlen]
  LET qlen      = h7!t     // The q length of the score
  LET partlist  = h4!parts // There is always at least one part.

  // Use barscan to create all the shape environments declared
  // by the conductor. These will be filled by the trconductor function.

  // All shape environments have the following structure.

  // env -> [env, op, ln, parent, upb, v]
  // 0r  =  0     where
  // op     is the type of this environment, eg s_volenv.
  // ln     is the line/file number of the block
  // parent is zero or points to an environment node of the same type
  //        on level out.
  // upb    is zero or the upper bound of v
  // v      is zero or the vector of shape entries of form [qlen,val]
  
  // While the shape data is being collected the pair [upb,v] represents
  //        a self expanding vector with the [qlen,val] starting at
  //        subscript 1 of v. v!0 is the highest subscript of v used.
  // Sometime after all the shape data has been collected it is copied into
  //        a new vector of the right size converting the qlen positions to
  //        absolute locations. The new shape items are of the form [absq,val].
  //        After compaction upb is set to zero indicating that the env was
  //        not allocated by getvec.

  //conductorpart := h5!t
  // conductorpart -> [-, Conductor, ln, block, qlen]

  //conductorblock := h4!conductorpart
  // conductorblock -> [0, Block, ln, notes, envs, qbeat1, qbeat2]

  // Initialise the outermost shape environments
  
  tempoenv      := 0
  tempoadjenv   := 0
  vibrateenv    := 0
  vibrateadjenv := 0
  vibampenv     := 0
  vibampadjenv  := 0
  volenv        := 0
  voladjenv     := 0
  legatoenv     := 0
  legatoadjenv  := 0
  delayenv      := 0
  delayadjenv   := 0
  tqshiftenv    := 0
  
  // Initialise the outermost non shape environments
  
  msecsv := 0      // This will be a vectore used by absq2msecs to
                   // convert absolute qbeats to msecs.

  // First call barscan part to create the bar and beat vectors.

  writef("Calling barscan(conductorpart)*n")
  barscan(conductorpart)

  //Compact both barsxv and beatsxv
  compactsxv(barsxv)
  compactsxv(beatsxv)
  

  writef("maxqbeat=%n*n", maxqbeat)
  writef("maxbarno=%n*n", maxbarno)
  writef("barsxv  -> [%n,%n]*n", barsxv!0,  barsxv!1)
  writef("beatsxv -> [%n,%n]*n", beatsxv!0, beatsxv!1)

//IF FALSE DO // Disable the printing of the bar and beat tables.
  { // Output the bar table
    LET v = barsxv!1
    LET upb = v -> h1!v, 0
    writef("Bar table, upb=%n", upb)
    FOR i = 1 TO upb DO
    { IF i MOD 10 = 1 DO writef("*n%i3: ", i)
      writef(" %i6", v!i)
    }
    newline()
    // Output the beat table
    v := beatsxv!1
    upb := v -> h1!v, 0
    writef("Beat table, upb=%n", upb)
    FOR i = 1 TO upb DO
    { IF i MOD 10 = 1 DO writef("*n%i3: ", i)
      writef(" %i6", v!i)
    }
    newline()
    //newline()
//abort(1883)
  }

  // Test bqeat2barno and qbeat2beatno
  //IF FALSE DO
  { LET prevbarno, prevbeatno = -1, -1
    FOR b = 0 TO maxqbeat+10 DO
    { LET barno = absq2barno(b)
      LET beatno = absq2beatno(b)
      UNLESS barno=prevbarno & beatno=prevbeatno DO
        writef("qbeat=%i5  barno=%i3 Beatno=%n*n", b, barno, beatno)
      prevbarno, prevbeatno := barno, beatno
    }
  }
  
abort(1988)

  // Call findshapedata to create the shape environments
  // declared in the conductor part.
  findshapedata(conductorpart)
  abort(1989)

  writef("Calling trconductor(conductorpart)*n")

  trconductor(conductorpart)
  
  writef("tempoenv=%n*n", tempoenv)

  msecsv := 0 // This vector will be allocated and will hold
              // the time in msecs of every 16th qbeat of the
	      // entire score. The function qbest2msecs uses
	      // this vector and linear interpolation to
	      // calculate the time of any qbeat.

  // conductorblock -> [0, Block, ln, notes, envs, qbeat1, qbeat2]
  mkmsecsv(h5!conductorblock)

  // Now translate all the other parts.

  midilist  := 0             // Initialise the linked list of midi statements.
  midiliste := @midilist
  end_msecs := 0

  trparts(parts)
  
writef("trscore: Successful return from trscore*n")
  RESULTIS TRUE
}

AND trconductor(t) BE
{ // t -> [-, Conductor, ln, block, qlen]
  // calcqlen and barscan have already run.
  // This function mainly collects the sape data.
  
  LET op   = h2!t
  LET ln   = h3!t
  LET qlen = h5!t
writef("trconductor: t=%n qlen=%n*n", t, qlen)
  
  // If currbarno is zero there the were no barlines in the conductor
  UNLESS currbarno DO
    fatalerr("The conductor part must have at least one barline*n")

  // Set the scaling parameters for the conductor.0
  scbase, scfac_top, scfac_bot := 0, 1, 1

  // set currqbeat to zero.
  currqbeat := 0

  // Extract the shape data from the conductor part.
  // Ensure that tempoenv is non empty even when the user
  // does not supply any tempoenv entries.
  pushshape(tempoenv, 0, defaultval(s_tempo)) // Force tempoenv to start with
                                              // the default tempo.

  // If the last qbeat does no have an explicit tempo, force tempoenv to
  // end with the default tempo.
  { LET v = tempoenv!1
    LET upb = v!0
    UNLESS v!(upb-1)=qlen DO
      pushshape(tempoenv, qlen, defaultval(s_tempo))
  }
  
  writef("*nShape data after processing the conductor part*n")
  prshapes()
  //abort(1237)


}

AND mkmsecsv() BE
{ // Fill in the entries in the msecs environment based
  // on the conductor's Tempo and Tempoadj shapes, even if
  // no Tempo or Tempoadj constructs were present.

  { LET FLT msecs = FLOAT 0
    LET qlen = h5!conductorpart
    msecsvupb := qlen/16 + 1
    msecsv := getvec(msecsvupb)
    UNLESS msecsv DO
    { writef("Unable to allocate msecsv, getvec(%n) failed*n", msecsvupb)
      abort(999)
      RETURN
    }
    FOR i = 0 TO msecsvupb DO
    { LET qbeat = i*16
      LET FLT tempo = shapeval(s_tempo, tempoenv, qbeat)
                      //envlookup(qbeat, 0, qlen, tempoenv, tempoadjenv)
      // tempo is a rate of 1024 qbeats per minute.
      // tempo                   is the number of   1024 qbeats per minute
      // tempo/60                is the number of   1024 qbeats per second
      // 1024*tempo/60           is the number of   qbeats per second
      // 60/(1024*tempo)         is the number of   seconds per qbeat
      // 1000*60/(1024*tempo)    is the number of   msecs per qbeat
      // 16*1000*60/(1024*tempo) is the number of   msecs per 16 qbeat
      LET FLT factor = FLOAT(16000*60) / FLOAT 1024
      msecs := msecs + factor / tempo
    }
  }

writef("msecsv is now setup, msecsvupb=%n*n", msecsvupb)
writef("*nPart of msecsv environment*n")
  FOR i = 0 TO msecsvupb DO
  { IF i MOD 4 = 0 DO
      writef("*n%i5: ", 16*i)
    writef(" %6.1f", msecsv!i)
  }
  newline()

writef("*nTesting the absq2msecs function*n")
  FOR q = 0 TO 128 DO
  { IF q > msecsvupb BREAK
    IF q MOD 4 = 0 DO writef("*n%i5: ", q)
    writef(" %6.1f", absq2msecs(q))
  }
  newline()
  abort(1238)
}

AND trpart(t) BE
{ // Translate this part into midi statements appending them to
  // the end of midilist.
  // t  -> [-, Part,  ln, block, qlen, chan]
  // or -> [-, Solo,  ln, block, qlen, chan]
  LET op    = h2!t
  LET ln    = h3!t
  LET block = h4!t
  LET qlen  = h5!t
  LET chan  = h5!t
  writef("trpart: t=%n op=%s qlen=%n chan=%n*n", t, opstr(op), qlen, chan)
  scbase, scfac_top, scfac_bot := 0, 1, 1
  currqbeat := 0
  currbarno := 0
  variablevol := FALSE

  tlist, tqpos := 0, 0 // Initialising the tie mechanism for
  plist, pqpos := 0, 0 // this part.
  clist, cqpos := 0, 0

//abort(2289)

  trblock(block)
}

AND trtree(t) BE
{ UNLESS t RETURN

  IF h2!t = s_block DO
  { trblock(t)
    RETURN
  }

  IF h2!t = s_notes DO
  { trblock(t)
    RETURN
  }
}

AND extractenvs(envs) BE IF envs DO
{ // envs -> [0, Envs, ln, envlist]
  // envlist -> [envlist, op, ln, pareent, upb, v, qbeat1, qbeat1]
  // or      =  0
  
  // Extract the environments from envs filling the parent fields
  // with the appropriate parents.
  LET envlist = h4!envs
  WHILE envlist DO
  { // envlist -> [envlist, op, ln, parent, upb, v, qbeat1, qbeat2]
    SWITCHON h2!envlist INTO
    { DEFAULT:
        writef("System error while extracting environments*n")
        abort(999)

      CASE s_tempoenv:
        h4!envlist := tempoenv
        tempoenv := envlist
        ENDCASE
      CASE s_tempoadjenv:
        h4!envlist := tempoadjenv
        tempoadjenv := envlist
        ENDCASE
      CASE s_vibrateenv:
        h4!envlist := vibrateenv
        vibrateenv := envlist
        ENDCASE
      CASE s_vibrateadjenv:
        h4!envlist := vibrateadjenv
        vibrateadjenv := envlist
        ENDCASE
      CASE s_vibampenv:
        h4!envlist := vibampenv
        vibampenv := envlist
        ENDCASE
      CASE s_vibampadjenv:
        h4!envlist := vibampadjenv
        vibampadjenv := envlist
        ENDCASE
      CASE s_volenv:
        h4!envlist := volenv
        volenv := envlist
        ENDCASE
      CASE s_voladjenv:
        h4!envlist := voladjenv
        voladjenv := envlist
        ENDCASE
      CASE s_legatoenv:
        h4!envlist := legatoenv
        legatoenv := envlist
        ENDCASE
      CASE s_legatoadjenv:
        h4!envlist := legatoadjenv
        legatoadjenv := envlist
        ENDCASE
      CASE s_delayenv:
        h4!envlist := delayenv
        delayenv := envlist
        ENDCASE
      CASE s_delayadjenv:
        h4!envlist := delayadjenv
        delayadjenv := envlist
        ENDCASE
      CASE s_tqshiftenv:           // This is set using the tempo
        h4!envlist := tqshiftenv   // and tempoadj environments
        tqshiftenv := envlist
        ENDCASE
    }
    envlist := h1!envlist
  }
}

AND trblock(t) BE
{ // t -> [-, Block, ln, notes, envs, qbeat1, qbeat2)

  // qbeat1 and qbeat2 are the absolute start and end qbeat locations
  // of the block. Note that absqbeat locations are the ordinary
  // qbeat locations defined by the conductor part. In Parts and Solos
  // scaling is used to map local qbeat locations to the conductor's
  // absolute locations.

  // Note that any shape data specified within a block has no effect
  // outside that block.

  // Tempo values given within the conductor's block affect the rate
  // that absolute qbeats are played, but Tempo values in Parts and
  // Solos affect only how local qbeats are mapped to absolute qbeats.
  // High tempo values cause a region of local qbeats to map to a
  // smaller number of absolute locations. This effectively increases
  // the speed of performance of the region. Smaller tempo values
  // have the opposite effect. Within a block these changes are
  // scaled so that the first and last local qbeat of the block map
  // into the same absolute location that they would have if no tempo
  // values were given. The time to perform a block is thus not affected
  // by any tempo values it contains.

  // This function appends midi events onto the end of the midi list.

  LET op    = h2!t   // op = s_block
  LET ln    = h3!t
  LET notes = h4!t
  LET envs  = h5!t
  LET qlen  = h6!t
  
  // Environments hold data specifying a piecewise linear graphs
  // that are used when calculating a shape value such as the
  // tempo or volume at a specified absolute qbeat location.
  // All shape values are floating point numbers and integers
  // are used for qbeat locations.
  
  LET notelist = h4!notes

  AND prevtempoenv      = tempoenv
  AND prevtempoadjenv   = tempoadjenv
  AND prevvibrateenv    = vibrateenv
  AND prevvibrateadjenv = vibrateadjenv
  AND prevvibampenv     = vibampenv
  AND prevvibampadjenv  = vibampadjenv
  AND prevvolenv        = volenv
  AND prevvoladjenv     = voladjenv
  AND prevlegatoenv     = legatoenv
  AND prevlegatoadjenv  = legatoadjenv
  AND prevdelayenv      = delayenv
  AND prevdelayadjenv   = delayadjenv
  AND prevtqshiftenv    = tqshiftenv
  
  // Setup the new shape environments
  extractenvs(envs)

  UNLESS op=s_block DO
  { trerr(-1, "trblock not applied to a block node, op=%s*n", opstr(op))
    abort(999)
    RETURN
  }
  
writef("trblock: t=%n op=%s envs=%n qlen=%n*n", t, opstr(op), envs, qlen)
abort(1292)

  // Blocks use the current scaling parameters. Note that
  // these are already set for Parts and Solos.
  // qbeat1 and qbeat2 are the start and end qbeat locations of the block.
      
  UNLESS h2!notes=s_notes DO
  { writef("System error: notes expected*n")
    abort(999)
  }
  // notes -> [-, Notes, ln, notelist, qlen]
  // First add shape data to the current shape environments

//writef("calling findshapedata(%n)*n", t)
  findshapedata(t)
//prshapes()
//abort(7743)

  // Fill in the entries in the msecs environment based
  // on the block's Tempo and Tempoadj shapes. These
  // times are scaled to ensure that the performance time
  // agree with the outer environment's requirement.

  // Set the initial scaling parameters.
  scbase, scfac_top, scfac_bot := 0, 1, 1
  currqbeat := 0

  // Only call setmsecsenv if there are \tempo items in
  // the block.
  IF h2!tempoenv | h2!tempoadjenv DO setmsecsenv(0, h5!t)

  prshapes()
abort(2228)
  // Then translate the notelist sequence.

  // Set the scaling parameters.
  scbase, scfac_top, scfac_bot := 0, 1, 1
  currqbeat := 0
writef("Calling genmidi(notelist=%n)*n", notelist)
  genmidi(notelist)
writef("Returned from genmidi*n")

  // Restore the start and end positions of the enclosing block.
  // This is only needed when h2!t=s_block
abort(7741)

  // Restore the previous shape environment vectors
  tempoenv      := prevtempoenv
  tempoadjenv   := prevtempoadjenv
  vibrateenv    := prevvibrateenv
  vibrateadjenv := prevvibrateadjenv
  vibampenv     := prevvibampenv
  vibampadjenv  := prevvibampadjenv
  volenv        := prevvolenv
  voladjenv     := prevvoladjenv
  legatoenv     := prevlegatoenv
  legatoadjenv  := prevlegatoadjenv
  delayenv      := prevdelayenv
  delayadjenv   := prevdelayadjenv
  tqshiftenv    := prevtqshiftenv
}

AND prlocation(q) BE
{ // Output the location in msecs and absq.
  //q is the local qbeat value.
  LET absq = qscale(q)
  LET FLT msecs = absq2msecs(absq)
  writef("%7.1f %7i: ", msecs, absq)
newline()
abort(1187)
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
  IF tqshiftenv    & tqshiftenv!1    DO prshape("Tqshift",    tqshiftenv)

  newline()
//abort(7745)
}

AND prshape(str, env) BE WHILE env DO
{ LET upb     = h1!env
  LET v       = h2!env
  LET prevenv = h3!env
  LET sq      = h4!env
  LET eq      = h5!env
  LET dflt    = h6!env
  LET p = h1!v
  LET layout = 0
  writef("%t9: env=%n v=%n prevenv=%n sq=%n eq=%n dflt=%9.3f*n",
          str, env, v, prevenv, sq, eq, dflt)
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

AND prenvi(str, env) BE
{ // Print a non shape environment with integer values.
  writef("%s env=%n", str, env)
  IF env DO
  { LET upb  = h1!env
    LET v    = h2!env
    LET sq   = h3!env
    LET eq   = h4!env
    LET dflt = h5!env
    writef(" upb=%n v=%n sq=%n eq=%n dflt==%9.3f*n",
             upb,   v,   sq,  eq,   dflt)
    IF v DO
    { LET layout = 1
      LET n = h1!v
      FOR i = 1 TO n DO
      { IF layout>5 DO
        { writef("*n%i3: ", i)
	  layout := 1
	}
	writef(" %i6", v!i)
      }
      newline()
    }
  }
}

AND prmsecsenv(str, env) BE
{ // Print the msecs environment
  writef("%s env=%n", str, env)
  UNLESS env DO
  { newline()
    newline()
    RETURN
  }

  { LET upb  = h1!env
    LET v    = h2!env
    LET prev = h3!env
    LET sq   = h4!env
    LET eq   = h5!env
    LET dflt = h6!env
    LET blk  = h7!env
    writef(" upb=%n v=%n prev=%n sq=%n eq=%n dflt==%9.3f blk=%n*n",
             upb,   v,   prev,   sq,   eq,   dflt)
    IF v DO
    { // v => [n, t1, .., tn]
      LET layout = 1
      LET n = h1!v
      FOR i = 1 TO n DO
      { LET q = sq + 64*(i-1)
        LET msecs = v!i
	IF layout>5 DO
        { newline()
	  layout := 1
	}
	writef("   %5i:%9.3f", q, msecs)
        layout := layout+1
      }
      newline()
    }
    env := prev
  }
} REPEAT

LET calcqlen(t) = t=0 -> 0, VALOF
{ // t points to any node in the parse tree.
  // Return the qlen of this construct.

  // It explores every node reachable from t mainly to
  // fill in the all qlen fields but it does not
  // follow the h1 chain since t might be an element of
  // a Notes list or a Pars list.
  LET op   = h2!t  // The tree node operator
  LET ln   = h3!t  // The lineno value
  LET qlen = 0     // For the length of this construct.

//writef("calcqlen: t=%n op=%s*n", t, opstr(op))
//abort(1991)

  SWITCHON op INTO // All possible notelist and shapelist items.
  { DEFAULT:
      // We only have to work on nodes that can have a non zero length
      // all others can be ignored returning a length of zero.
      
//      writef("calcqlen, t=%n ignoring node with op = %s*n",
//             t, opstr(op))
//      abort(999)
      RESULTIS 0

    CASE s_name:
    CASE s_num:
    CASE s_barline:
    CASE s_doublebar:
//      writef("calcqlen, t=%n ignoring node with op = %s*n",
//             t, opstr(op))
      RESULTIS 0

    CASE s_score: // This is needed since the first call of calcqlen
                  // is for the entire parse tree.
    { // t -> [-, Score, ln, name, conductor, parts, qlen]
      // This applies calcqlen to the conductor and all the parts.
      // It stores the length of the longest of these in the Score node.

      LET conductor = h5!t
      // conductor -> [0, Conductor, ln, block, qlen]
      LET parts = h6!t
      // parts -> [-, Parts, ln, partlist, qlen]

      LET conductorqlen = calcqlen(conductor)
      LET partsqlen     = calcqlen(parts)

      UNLESS conductorqlen = partsqlen DO
        writef("WARNING: The lengths of the conductor and parts differ*n")

//writef("calcqlen: score qlen = %n*n", conductorqlen)
      h7!t  := conductorqlen
      RESULTIS conductorqlen
    }

    CASE s_conductor:  // t -> [-. Conductor, ln, block, qlen]
    { // If the length is not already known it calculates it using
      // a call of calcqlen and update the qlen field in the parse tree.
      LET block  = h4!t
      LET qlen = h5!t
      IF qlen>=0 RESULTIS qlen
      qlen := calcqlen(block)
      h5!t := qlen
      RESULTIS qlen
    }

    CASE s_parts:  // t -> [-, Parts,  ln, partlist, qlen]
    { LET partlist = h4!t
      // partlist -> [partlist, Part, ln, block, qlen, chan]
      // partlist -> [partlist, Solo, ln, block, qlen, chan]
      // or       = 0
      LET qlen = 0 // This will hold the qbeat length of the longest
                   // part in the partset.

      IF h5!t>=0 RESULTIS h5!t // If the length is already known
                               // just return its value.

      // Apply calcqlen to each Part or Solo node om parts and
      // set qlen to the length of the longest.
      WHILE partlist DO
      { // partlist -> [-, Part, ln, notelist, qlen, chan]
        // partlist -> [-, Solo, ln, notelist, qlen, chan]
        LET len = calcqlen(partlist)
        IF qlen < len DO qlen := len
        //writef("calcqlen: %s part len=%n => qlen=%n*n",
        //        opstr(h2!partlist), len, qlen)
        partlist := !partlist
      }
      h5!t := qlen // Store this longest length in the parse tree.
//abort(6998)
      RESULTIS qlen
    }

    CASE s_part:       // t -> [-. Part, ln, block, qlen, chan]
    CASE s_solo:       // t -> [-. Solo, ln, block, qlen, chan]
    { // If the length is not already known it calculates it using
      // a call of calcqlen and update the qlen field in the parse tree.
      LET block = h4!t
      LET qlen  = h5!t
      IF qlen>=0 RESULTIS qlen
      qlen := calcqlen(block)
      h5!t := qlen
      RESULTIS qlen
    }

    CASE s_notes:       // t -> [-, Notes,  ln, notelist, qlen]
    { LET notelist = h4!t
      LET qlen = 0 // This will hold the length of the list of notes.
//writef("calcqlen: CASE Notes  notelist=%n*n", notelist)
      IF h5!t>=0 RESULTIS h5!t // If the length is already known
                               // just return its value.

      // Apply calcqlen to each note item in the list and
      // return the sum of their lengths.
      WHILE notelist DO
      { LET len = calcqlen(notelist)
        qlen := qlen+len
//writef("calcqlen: %s node len=%n => qlen=%n*n",
//        opstr(h2!notelist), len, qlen)
        notelist := !notelist
      }

      h5!t := qlen // Store the sum ofthe length in the Notes node.
      RESULTIS qlen
    }

    CASE s_note:      // t -> [-, Note,     ln, <letter,sharps,n>, qlen]
    CASE s_notetied:  // t -> [-, Notetied, ln, <letter,sharps,n>, qlen]
      // These already have their lengths in the tree.
      RESULTIS h5!t // Return the length.

    CASE s_rest:      // t -> [-, Rest, ln, qlen]
    CASE s_space:     // t -> [-, Space, ln, qlen]
      // These already have their lengths in the tree.
      RESULTIS h4!t // Return the length.

    CASE s_null:
      RESULTIS 0

    CASE s_tuplet:     // t -> [-, Tuplet, ln, block,  qlen]
      // The length was determined during syntax analysis.
      // But we must call calcqlen explore the notelist to set the qlrn
      // fields in the sequence.
      calcqlen(h4!t)
      RESULTIS h5!t

    CASE s_delay:      // t -> [-, Delay,     ln, notelist, shapes, qlen]
    CASE s_delayadj:   // t -> [-, Delayadj,  ln, notelist, shapes, qlen]
    CASE s_legato:     // t -> [-, Legato,    ln, notelist, shapes, qlen]
    CASE s_legatoadj:  // t -> [-, Legatoadj, ln, notelist, shapes, qlen]
    CASE s_tempo:      // t -> [-, Tempo,     ln, notelist, shapes, qlen]
    CASE s_tempoadj:   // t -> [-, Tempoadj,  ln, notelist, shapes, qlen]
    CASE s_vibrate:    // t -> [-, Vibrate,   ln, notelist, shapes, qlen]
    CASE s_vibrateadj: // t -> [-, Vibrateadj,ln, notelist, shapes, qlen]
    CASE s_vibamp:     // t -> [-, Vibamp,    ln, notelist, shapes, qlen]
    CASE s_vibampadj:  // t -> [-, Vibampadj, ln, notelist, shapes, qlen]
    CASE s_vol:        // t -> [-, Vol,       ln, notelist, shapes, qlen]
    CASE s_voladj:     // t -> [-, Voladj,    ln, notelist, shapes, qlen]
    { LET notelist  = h4!t
      LET shapes    = h5!t
      LET qlen      = h6!t      

      IF qlen>=0 RESULTIS h6!t // qlen already known

//writef("calcqlen: calling calcqlenshapelist*n")
//abort(6345)
      h5!shapes := calcqlenshapes(shapes) // Fill in the qlen of the Shapes node
      qlen := calcqlen(notelist)          // and use calcqlen to obtain the length
      h6!t := qlen                        // of the shape construct. 
      RESULTIS qlen
    }

    CASE s_block:          // t -> [-, Block, ln, notes, envs, qbeat1, qbeat2]
    { LET qlen = 0
//writef("CASE Block: calcqlen(%n) qbeat1=%n qbeat2=%n*n", h4!t, h6!t, h7!t)
      IF h7!t>=0 RESULTIS h7!t-h6!t
      h6!t := currqbeat
      qlen := calcqlen(h4!t)
//writef("calcqlen(%n) => qlen=%n*n", h4!t, qlen)
//abort(7666)
      h7!t := h6!t+qlen
      RESULTIS qlen
    }

    CASE s_par:       // t -> [-, Par, ln, notelistlist, qlen]
    { LET list = h4!t // list of Par Notelists
      LET qlen = 0

      IF qlen=0 RESULTIS qlen

      // Apply calcqlen to each member of the par list
      // and determine the length of the longest.
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
{ // This function is called from trblock to collect all
  // the shape data whose scope is the current block. This
  // function does not explore inner blocks.
  // t is initially the body of a block but, as a result of
  // recursive calls, it can point to any contruct where
  // shape data can be found.

  // A block is one of the following constructs.

  // Shape data of the volmap construct is found by calling
  // findvolmapdata.

  // A typical shape environment is volenv which has the
  // following form:

  // volenv -> [-, Volenv, parent, upb, v]

  // Initially the fields upb and v form a new self expanding
  // vector for the Vol environment Both upb and v are initially
  // zero.  parent is either zero or the pointer to the
  // nearest enclosing non empty environment of the same type.
  // Once the shape data in an environment is complete it is
  // copied into a newly allocated vector of the right size.
  // The newly allocated vector is allocated using newvec rather
  // than getvec so a later call of freevec should not be done.
  // This is indicated by setting the upb field to zero.

  // Before trblock calls findshapedata it will have saved the
  // current state of the global environment variables and
  // initialised them with the environments for the current
  // block. Just before returning it reinstates the previous
  // set. The parser function rdblock only creates a Block
  // if the block body contains shape data. It places in the
  // env field of the Block node an Envs node containing the
  // list of required environment nodes.

  // Absolue qbeat locations are those set by the condunctor
  // for the whole score. Converting local qbeat positions to
  // absolute locations is done using the function qscale which
  // performs the following calculation.

  // absq = scbase + muldiv(qbeat, scfac_top, scfac_bot)

  // scbase, scfac_top and scfac_bot are only changed when
  // processing a Tuplet construct or constructions involving
  // shape data. To reduce the sizes of scfac_top and scfac_bot,
  // they are divided by their gcd.

  LET op = h2!t
  LET ln = h3!t

writef("findshapedata: currqbeat=%n dealing with t=%n op=%s  ",
        currqbeat, t, opstr(op)); prlineno(ln); newline()
abort(9182)

  SWITCHON op INTO
  { DEFAULT:
      writef("System error in findshapedata - Unexpected op = %s ", opstr(op))
      prlineno(ln)
      newline()
      abort(999)
      RETURN

    CASE s_name:
    CASE s_num:
    CASE s_barline:
    CASE s_doublebar:
//IF op=s_doublebar DO abort(4448)
      RETURN

    CASE s_conductor:   // t -> [-. Conductor, ln, block, qbeat1, qbeat2]
    CASE s_part:        // t -> [-. Part,      ln, block, qlen]
    CASE s_solo:        // t -> [-. Solo,      ln, block, qlen]
    { LET block = h4!t
      findshapedata(block)
      RETURN
    }

    CASE s_notes:       // t -> [-. Notes, ln, notelist, qlen]
    { LET notes = h4!t
      findshapedata(notes)
      RETURN
    }

    CASE s_block:       // t -> [-. Block, ln, notes, env, qlen]
    { LET notes = h4!t
      // Set the start and end positions in all the empty shape environments.
      findshapedata(notes)
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

    CASE s_null:        // t -> [-, Space, ln]
      RETURN

    CASE s_tuplet:      // t -> [-, Tuplet, ln, block, qlen]
    { LET notelist = h4!t
      // notelist -> [-, Notelist, notelist, qlen]
      LET fromqlen = h4!notelist
      LET toqlen   = h5!notelist
      // Save the current scaling parameters
      LET prevcurrqbeat = currqbeat
      LET prevscbase, prevscfac_top, prevscfac_bot = scbase, scfac_top, scfac_bot
      setscaleparams(fromqlen, toqlen)

      findshapedata(notelist) 

      // Restore the previous scaling parameters.
      scbase, scfac_top, scfac_bot := prevscbase, prevscfac_top, prevscfac_bot

      currqbeat := prevcurrqbeat + toqlen
      RETURN
    }

    // All the following shape operators have the structure:
    // t -> [-, op, ln, notelist, shapelist, qlen]

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

    CASE s_notelist:       // t -> [-, Notelist, ln, notelist, qlen]
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
{ // This modifies the scaling parameters scbase, scfac_top
  // and scfac_bot for use when processing shapelists of
  // shape operators such as \vol ot \tempoadj, or translating
  // the Notes node of a \tuple construct. The magnification
  // scfac_top/scfac_bot is multiplied by toqlen/fromqlen.
  // Common factors of scfac_top and scfac_bot are removed.

  // The conversion to absolute qbeat locations is done by
  // absq := qscale(q) which is equivalent to
  // absq := scbase + muldiv(q, scfac_top, scfac_bot).
  // where q is the local qbeat location within as notelist
  // or shapelist. In these the qbeat location always starts
  // at zero, so this function sets currqbeat to zero after
  // setting scbase appropriately.

//writef("setscaleparams: scbase=%n scfac_top=%n scfac_bot=%n fromqlen=%n toqlen=%n*n",
//        scbase, scfac_top, scfac_bot, fromqlen, toqlen)
  IF fromqlen>0 DO
  { LET hcf = 0
    scfac_top := scfac_top * toqlen
    scfac_bot := scfac_bot * fromqlen
//writef("setscaleparams: gives scfac_top=%n scfac_bot=%n*n", scfac_top, scfac_bot)
    hcf := gcd(scfac_top, scfac_bot)  // Find the highest common factor
    scfac_top := scfac_top / hcf     // Divide both the top and the
    scfac_bot := scfac_bot / hcf     // bottom by this factor.
//writef("setscaleparams: dividing by hcf=%n*n", hcf)
  }
//writef("setscaleparams: gives scbase=%n scfac_top=%n scfac_bot=%n*n",
//        scbase, scfac_top, scfac_bot)

  currqbeat := 0
//abort(3775)
}

AND apfn2region(fromqlen, toqlen, fn, t) = VALOF
{ // t points to the leading node of a region where the scaling
  // factors may change, ie an inner block or the left operand
  // of a Tuplet. currqbeat is the local qbeat location of the
  // start of the region.
  // Note the scaling parameters are used by qscale to convert
  // local qbeat values to absolute qbeat values.
  LET res = 0
  
  // Save the previous scaling parameters
  LET ocurrqbeat = currqbeat
  LET prevscbase, prevscfac_top, prevscfac_bot = scbase, scfac_top, scfac_bot
  scbase := qscale(currqbeat)  // Absolute qbeat location of
                               // start of the region.

  { LET hcf = 0
    scfac_top := scfac_top * toqlen
    scfac_bot := scfac_bot * fromqlen
writef("apfn2region: scfac_top=%n scfac_bot=%n*n", scfac_top, scfac_bot)
    hcf := gcd(scfac_top, scfac_bot) // Find the highest common factor
    scfac_top := scfac_top / hcf     // Divide both the top and the
    scfac_bot := scfac_bot / hcf     // bottom by this factor.
writef("apfn2region: dividing by hcf=%n gives*n", hcf)
  }
writef("apfn2region: scfac_top=%n scfac_bot=%n*n", scfac_top, scfac_bot)
writef("apfn2region: old scbase=n new scbase=%n*n", prevscbase, scbase)

  currqbeat := 0 // The start local qbeat position in the region.

  res := fn(t)

  // Restore the previous scaling parameters.
  scbase, scfac_top, scfac_bot := prevscbase, prevscfac_top, prevscfac_bot
  currqbeat := ocurrqbeat
  RESULTIS res
}

AND addshapedata(env, shapenode) = VALOF
{ // This adds shape data to environment env.

  // shapenode -> [-, op, ln, notelist, shapelist, qlen]

  // op   is a shape operator such as \tempo or \vol
  // env  holds shape data of the kind corresponding to op

  // It sets up new scaling parameters then calls
  // applyshapelist to add the shape data to env
  // before restoring the previous scaling parameters.
  // Finally it calls findshapedata(notelist) to search
  // for more shape data.

  LET op        = h2!shapenode
  LET ln        = h3!shapenode
  LET notelist   = h4!shapenode
  LET shapelist  = h5!shapenode
  LET toqlen    = h5!notelist
  LET fromqlen  = h5!shapelist


  // Save the previous scaling parameters
  LET ocurrqbeat = currqbeat
  LET prevscbase, prevscfac_top, prevscfac_bot = scbase, scfac_top, scfac_bot

//writef("addshapedata: currqbeat=%n toqlen=%n fromqlen=%n*n",
//        currqbeat, toqlen, fromqlen)

  scbase := qscale(currqbeat)  // Absolute qbeat location of the notelist
  setscaleparams(fromqlen, toqlen)
  currqbeat := 0 // The start position in the shape list

//writef("addshapedata: currqbeat=%n scbase=%n scfac_top=%n scfac_bot=%n*n",
//        currqbeat, scbase, scfac_top, scfac_bot)

  applyshapelist(env, shapelist)

  // Restore the previous scaling parameters.
  scbase, scfac_top, scfac_bot := prevscbase, prevscfac_top, prevscfac_bot
  currqbeat := ocurrqbeat
//writef("addshapedata: calling findshapedata -- currqbeat=%n*n", currqbeat)
  findshapedata(notelist)      // Find shape data in notelist
//abort(5123)
  currqbeat := ocurrqbeat + toqlen
}

AND applyshapelist(env, shapelist) = VALOF
{ // shapelist -> [-, Shapelist, ln, shapelist, qlen)
  // This is called when processing a shape node such as
  // \vol or \tempoadj.

  LET shapelist = h4!shapelist

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

  // Note that currqbeat is the qbeat position within the shape list.
  // It is necessary to apply qscale to obtain the absolute qbeat position
  // in left hand operand of the shape operator.

//writef("applyshapeitem: currqbeat=%n  op=%s prevnum=%n prevqlen=%n  ",
//        currqbeat, opstr(op), prevnum, prevqlen)
//prlineno(ln); newline()

  SWITCHON op INTO
  { DEFAULT:
      trerr(-1, "Bad op %s in shape list", opstr(op))
      RETURN

    CASE s_star:             // t -> [-, Star, ln]
    CASE s_num:              // t -> [-, Num,  ln, value]
    { LET FLT x = h4!t       // -value if op=Num
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

//writef("applyshapeitem: op=%s value=%9.3f*n", opstr(op), x)

      // Insert the shape entry into env.
      pushshape(env, absq, x) 
//writef("applyshapeitem: op=%s returned from pushshape(env, %n, %5.3f)*n",
//       opstr(op), absq, x)
      RETURN
    }

    CASE s_space:            // t -> [-, Space, ln, qlen]
    CASE s_rest:             // t -> [-, Rest, ln, qlen]
    { LET qlen = h4!t // The qlen of this kind of space
//writef("applyshapeitem: op=%s qlen=%n*n", opstr(op), qlen)
      currqbeat := currqbeat + qlen
      prevqlen := qlen
      prevnum := FALSE
      RETURN
    }

CASE s_null:             // t -> [-, Null, ln]
      RETURN
  }
}

AND calcqlenshapes(shapes) = VALOF
{ // This returns the qbeat length of a Shapes node.
  // shapes -> [-, Shapes, ln, shapelist, qlen]
  // shapelist   =  0
  // or          -> [shapelist, Num,   ln, number]
  // or          -> [shapelist, Star,  ln]
  // or          -> [shapelist, Space, ln, qlen]
  // or          -> [shapelist, Space, ln, qlen]
  // or          -> [shapelist, Null,  ln]


  LET shapelist = h4!shapes
  LET qlen      = h5!shapes
  LET prevnum   = FALSE   // Set to TRUE by Num or Star
  LET prevqlen  = 1024    // The qlen of the previous space

//writef("calcqlenshapes: entered shapelist=%n  op=%s qlen=%n*n",
//        shapelist, opstr(h2!shapelist), qlen)

  IF qlen>=0 RESULTIS qlen
  
  qlen := 0

  WHILE shapelist DO
  { // shapelist   -> [-, Num,   ln, number]
    // or          -> [-, Star,  ln]
    // or          -> [-, Space, ln, qlen]
    // or          -> [-, Space, ln, qlen]
    // or          -> [-, Null,  ln]

    LET op = h2!shapelist
//writef("calcqlenshapes:shapelist=%n  op=%s*n", shapelist, opstr(op))
//writef("calcqlenshapes: shapes=%n shapelist=%n*n", shapes, shapelist)
//abort(5679)
    SWITCHON op INTO
    { DEFAULT:
        trerr(-1, "Bad op %s in shape list", opstr(op))
        ENDCASE

      CASE s_star:    // shapelist -> [-, Star, ln]
      CASE s_num:     // shapelist -> [-, Num,  ln, value]
        IF prevnum DO
        { // Assume a space of the previous space qlen
          // between adjacent numbers
          qlen := qlen + prevqlen
	  ENDCASE
        }
        prevnum := TRUE
        ENDCASE

      CASE s_space:   // shapelist -> [-, Space, ln, qlen]
      CASE s_rest:    // shapelist -> [-, Rest,  ln, qlen]
        prevqlen := h4!shapelist
        qlen := qlen + prevqlen
        ENDCASE
	
      CASE s_null:    // shapelist -> [-, Null, ln]
        ENDCASE
    }
    shapelist := h1!shapelist
  }

  RESULTIS qlen
}

AND setmsecsenv(q1, q2) BE
{ // THIS IS OUT OF DATE
  // q1 and q2 are the absolute qbeat locations of the start
  // and end of the current block. q2 is greater then q1,
  // since all blocks have qlengths greater than zero.

  // This function uses tempoenv and tempoadjenv to allocate
  // and set the msecsenv vector for this block.
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

  LET FLT msecs = 0.0  // Time relative to the start of this block
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

  LET v = getvec(upb)  // Allocate the msecsenv vector to hold msecs value
                       // over the qbeat range of q1 to q2 in steps of 64.
writef("setmsecsenv: entered, q1=%n q2=%n => upb=%n*n", q1, q2, upb)

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

//writef("setmsecsenv: entered 1*n")
  h4!msecsenv := q1  // The start and end qbeat locations of
  h5!msecsenv := q2  // this version of msecsenv.

  { LET FLT tempo = adjustedshapeval(q, tempoenv, tempoadjenv)
    // The tempo at absolute qbeat location q
    // Calculate the rate at q in units of msecs per 64 qbeats
    // base on tempo in quarter notes per minute.
    LET FLT msecsper64qbeats = FLOAT (64*60*1000) / (1024.0*tempo)
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
    // its msecs timings to be scaled.

    LET FLT conductormsecs1 = getmsecs(conductormsecsenv, q1)
    LET FLT requiredmsecs   = getmsecs(conductormsecsenv, q2) - conductormsecs1
    LET FLT actualmsecs     = getmsecs(msecsenv, q2)
    LET FLT scalefactor     = requiredmsecs / actualmsecs
    FOR j = 1 TO upb DO
    { LET FLT relmsecs = v!j
      relmsecs := relmsecs * scalefactor
      v!j := relmsecs + conductormsecs1
    }
  }
}

AND getmsecs(q) = VALOF
{ // This returns the time is msecs as an integer corresponding to
  // any absolute qbeat location q. It uses the vector msecsv whose
  // elements depend on the Tempo and Tempoadj values collected by
  // the conductor. The times at the start and end of each 16 qbeat
  // region are msecsv!(q/16) and msecsv!(q/16+1). Interpolation is
  // used over the 16 qbeats region containing q.
 
  LET qby16 =  q  /  16
  LET offset = FLOAT(q MOD 16)
  LET FLT t0 = msecsv!qby16
  LET FLT t1 = msecsv!(qby16+1)

  RESULTIS FIX(t0 + (t1-t0) * offset / FLOAT(16))
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
{ // t points to a parse tree node representing the conductor part.
  // Its purpose is to create the barsxv and beatsxv vectors
  // giving the absolute qbeat locations of all barlines and
  // beats.


  // The body of the conductorpart is a Block node whose Envcomponent
  // has already been initialised with empty environment nodes for
  // all types of shape data declared within the block.


  // The first entry in barsxv is zero corresponding to the start
  // of bar zero. When the first bar line is encountered, if bar
  // zero is incomplete the next entry is currqbeat representing
  // the start of the first full bar. If bar zero is complete, the
  // entry for bar 1 is set to zero indicating that bar zero is of
  // zero length and so does not exist. In either case the first
  // full bar is at the qbeat location held in entry for bar one.
  // If bar zero does not exist the bar line separate bars one and
  // two and so the entry for bar two is set to currqbeat
  // representing the start of bar 2.
  
  // While barscan is running the current bar number is held in
  // currbarno is initialise to zero but is corrected if necessary when
  // the first barline is encountered.
  
  // t  -> [-, Conductor, ln, block, qlen]

  LET op      = h2!t   // An operator in the conductor part.
  LET ln      = h3!t
  LET block   = h4!t   // A Block or Notes node.
  
  // Initialise the barscan global variable.
  currqbeat    := 0
  maxqbeat     := 0
  currbarqbeat := 0
  currpartname := "unknown"
  currbarno    := 0  // No bar line yet
  maxbarno     := 0  // This will contain the number of the last
                     // bar of the composition.

  writef("barsxv=%n beatsxv=%n*n", barsxv, beatsxv)
//abort(1111)
  pushival(barsxv, 0)  // Bar zero, if it exists, starts at qbeat 0
  pushival(beatsxv, 0) // Beat 1 is at qbeat 0
abort(5522)
  prevbarlineqbeat  := 0
  barnodiff         := 0
  
  // The initial scaling parameters. Probably constructs requiring scaling
  // will no be allowed in the conductor's part.
  scbase := qbeat
  scfac_top, scfac_bot := 1, 1    // No scaling yet. These are set and
                                  // restored when translating Tuplet
                                  // and  shape operator constructs.

  timesig_t := 4     // Set the default time signature of 4/4
  timesig_b := 4

  qbeatsperbeat := 1024 * 4 / timesig_b
  qbeatsperbar := timesig_t * qbeatsperbeat

writef("barscan: t=%n op=%s currqbeat=%n ", t, opstr(op), currqbeat)
prlineno(ln); newline()
//abort(9438)

  barscanitem(block) // Scan a Block or a Notes node.
}

AND checkbeats() BE
{ // Check whether the current beat or bar is complete.
  WHILE currqbeat>=nextqbeat DO
  { pushival(beatsxv, nextbeatqbeat)
    nextbeatqbeat := nextbeatqbeat+qbeatsperbeat
  }
  IF currqbeat>=nextbarqbeat DO
  { IF currqbeat>nextbarqbeat DO
    { writef("A barline seems be missing at qbeat=%n*n", currqbeat)
      nextbarqbeat := currqbeat
    }
    pushival(beatsxv, nextbeatqbeat)
    currbarno := currbarno+1
    nextbarqbeat := nextbarqbeat + qbeatsperbar
  }
}

AND barscanitem(t) BE IF t DO
{ // This scans a note item in the conductor part.

  // t  -> [0, Block, ln, notes, envs, qbeat1, qbeat2]
  // or -> [0, Notes,ln, notelist, qlen]
  
  // notes -> [0, Notes, ln, notelist, qlen]

  // This scans a the list of note items incrementing currqbeat and
  // currbarno as necessary and updating entries is barsxv and
  // beatsxv at bar lines.

  IF h2!t=s_block DO t := h4!t
  
writef("barscanitem: *
       *currqbeat=%n currbarno=%n currbeatno=%n op=%s ln=%n/%n*n",
        currqbeat,   currbarno,   currbeatno,   opstr(h2!t),
        fno(h3!t),   lno(h3!t))
//abort(9439)

  SWITCHON h2!t INTO
  { DEFAULT: // Ignore all the other tree nodes
      RETURN

    CASE s_notes:    // [-, Notes, ln, notelist, qlen]
    { LET op = h2!t
      LET list = h4!t
writef("barscanitem: %i6: %s qlen=%n*n", currqbeat,  opstr(op), h5!t)
//abort(9440)
      WHILE list DO
      {
        writef("barscanitem: op=%s currqbeat=%n*n",opstr(h2!list), currqbeat)
        barscanitem(list) // Scan each item in the note list
        list := h1!list
      }
      RETURN
    }

    CASE s_name: // t -> [-, Name, ln, str]
      currpartname := h4!t
writef("barscanitem: %i5: Name %s*n", currqbeat, currpartname)
      RETURN

    CASE s_tuplet: // t -> [-, Tuplet, ln, block, qlen]
      trerr(-1, "\tuple is not permitted in the conductor's part")
      abort(999)
      RETURN

    CASE s_par:
      trerr(-1, "\par is not permitted in the conductor's part")
      RETURN

    CASE s_note:
    CASE s_notetied:
      trerr(-1, "Notes are not permitted in the conductor's part")
      // Treat as a space or rest.
    CASE s_space:
    CASE s_rest:
writef("barscanitem: %i5: %s qlen=%n*n", currqbeat, opstr(h2!t), h4!t)
      currqbeat := currqbeat + h4!t
      IF maxqbeat < currqbeat DO maxqbeat := currqbeat
      //checkcurrqbeat() // Check for barlines and beats
      RETURN

    CASE s_null:
writef("barscanitem: %i5: %s*n", currqbeat, opstr(h2!t))
      RETURN

    CASE s_block:
      
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
    CASE s_vibampadj:   // [-, op, ln, list, shape]
      // barscan only looks at the left operand of these shape constructs. 
writef("barscanitem: %i5: %s*n", currqbeat, opstr(h2!t))
      barscanitem(h4!t)
      RETURN

    CASE s_barline:
    CASE s_doublebar:
    { 
      
writef("barscanitem: %i5: %s prevbarlineqbeat=%n currbarno=%n*n",
        currqbeat, opstr(h2!t), prevbarlineqbeat,  currbarno)
//abort(8081)
      // Time signatures may only occur at the start of the part or the
      // start of a bar. They are used to specifythe number of qbeats
      // per bar and checks are made to ensure that barlines are
      // positioned correctly. The first bar of the score is permitted
      // to have fewe qbeats than a full bar. If this happens the bar
      // is becomes bar zero. The first full bar is always bar one.

      IF currbarno=0 DO
      { // This is the first barline of the score.
        IF currqbeat >= qbeatsperbar DO
	  pushival(barsxv, 0) // Bar zero does no exist
      }
      
      pushival(barsxv, currqbeat)
abort(8088)

      IF currbarno UNLESS latestbarqlen = qbeatsperbar DO
      { LET diff = qbeatsperbar-latestbarqlen
        writef("qbeatsperbar=%n latestbarqlen=%n*n",
	        qbeatsperbar,   latestbarqlen)
        TEST diff>0
	THEN trerr(-1, "The bar is too short by %n qbeats*n", diff)
	ELSE trerr(-1, "The bar is too long by %n qbeats*n", -diff)
      }

      currbarno    := currbarno+1
      maxbarno     := currbarno
      prevbarlineqbeat := currqbeat

      // Fill in entries in the beat environment vector for this bar.
      UNTIL currbarqbeat>currqbeat DO
      { pushival(beatsxv, currbarqbeat)
        writef("barscanitem: beat at currbarqbeat=%i5 currqbeat=%n*n",
                currbarqbeat, currqbeat)
        currbarqbeat := currbarqbeat+qbeatsperbeat
      }
      currbarqbeat := currqbeat
//      abort(8083)
      RETURN
    }
    
    CASE s_timesig:   // [-, Timesig, ln, sig_t, sig_b]
    { // A time signature may only occur at the start of the
      // composition or just after a barline. sig_b is the length
      // number with 4 corresponding to a quarter note, 8 to a
      // quaver etc. sig_t is the number of these notes per bar.

      // 6/8 bars are normally conducted with two beats per bar
      // and so are probably best represented by \timesig(2 s4.)
      timesig_t, timesig_b := h4!t, h5!t
writef("barscanitem: %s(%n, %n) at currqbeat=%n*n",
        opstr(h2!t), timesig_t, timesig_b, currqbeat)

      UNLESS currqbeat = prevbarlineqbeat |
             currqbeat = prevbarlineqbeat + qbeatsperbar DO
        trerr(currqbeat, "Timesig(%n %n) is not at the start of a bar",
	                 timesig_t, timesig_b)
      qbeatsperbeat := 4096/timesig_b
      qbeatsperbar := qbeatsperbeat + timesig_t
      RETURN
    }
  }
  
  writef("barscanitem: System error*n")
  abort(999)
}

// Envronments are built using self expanding vectors. When complete
// the size of the vector is known and can be compressed using the
// function compactsxv.

AND compactsxv(sxv) BE
{ // Allocate a vector of the right size using newvec and copy
  // the elements of sxv!1 into it. Then use freevec to free sxv!1 and
  // update sxv!0 and sxv!1 with zero and the new vector.
  LET oldv = sxv!1
  LET upb  = oldv!0
  LET v = newvec(upb)
  FOR i = 0 TO upb DO v!i := oldv!i
  sxv!0 := 0   // Zero because v was allocated by newvec not getvec.
  sxv!1 := v
}

// Self expanding vector for environments have the following form.
// [upb, v]
// where upb is the upper bound of v and is increased as necessary
// while the vector is being filled with data. The upper bound is
// typically increasedby about 50% each time copying the elements
// of the old v into the new v. The subscript of the last element
// of v used is in v!0. hen all elements of v have been added the
// function compactsxv is called. This saves space by allocating
// a vector of the right size and also by using newvec rather than
// getvec for the allocation.

// Items in environments consist of pairs [qbeat,val] where val is
// normally a floating point numbers specifying  the environment
// value at position qbeat. A block covers a subregion of the
// complete score and corresonds to a specified number of qbeats.
// positions within the subregions are specified by qbeat values
// from zero to the size of the region. These are called local qbeat
// values. Absolute qbeat values are those corresponding to the whole
// score as setup by the conductor part. Local qbeat values can be
// mapped to absolute values usin the call qscale(qbest). The function
// qscale uses the scaling parameters scbase, scfac_top and scfac_bot.
// The transformation is:
// absqbeat = scbase + muldiv(qbeat, scfact_t, scfac_bot)
// mulldiv is used to avoid possible overflow problems.

// Each environment is associted with a block which specifies its
// range of qbest locations. The environment data specified a curve
// of piecewise linear segments covering the whole range. When
// collecting the the envronment coordinates location at qbeat zero
// is initially given a star and later coordinates are placed in
// consecutive locations. The final location of the region is given
// a star if it does not already have a value. Placing a coordinate
// value othe than a star in a qbeat location overrides the setting
// at that loction. The envronment value at any qbeat location in
// the region is calculated by linear interpolation using the
// nearest two surrounding coordinates. Note that an instantaneous
// change of environment value can be made by giving two different
// values as consecutive qbeat locations. The first give the value
// for the earlied qbeat preriod changing to the second value at
// the start of the later qbeat period.

LET pushival(sxv, i) BE
{ pushval(sxv, i)
  //IF debugv!1 DO
  { LET v = sxv!1
    LET p = h1!v
    writef("pushival: sxv=%n updating v[%i3] with %n*n", sxv, p, i)
  }
}

AND pushfval(sxv, FLT x) BE
{ pushval(sxv, x)
  //IF debugv!1 DO
  { LET v = sxv!1
    LET p = h1!v
    writef("pushfval: sxv=%n updating v[%i3] with %8.3f*n", sxv, p, x)
  }
}

AND pushval(sxv, val) BE
{ // sxv is the control block for a self expanding vector, often used
  // to hold data for a particular kind of shape (tempo, volume, etc).
  // either sxv -> [0, 0]
  // or     sxv -> [upb, v] where upb is the upper bound of v
  // and    v   -> [n, v1, v2,..., vn] with n <= upb
  // For shape data the values v1 to vn occur in pairs (loc,value)
  // giving the integer qbeat location and floating point value
  // of each point of the shape.
  // The upper bound of v is increased automatically as needed.
  
  LET upb = sxv!0      // Current upb of v.
  LET v   = sxv!1      // v=0 or a getvec'd vector holding 
                       // the shape points.
  // If v is non zero it points to a getvec'd vector with
  // upper bound upb and v!0 will be the position in v
  // of its latest element. v!0 will be <= upb.
  // If the vector is full pushval allocates another
  // larger one copying the existing elements into it before
  // pushing val.

  LET p = v -> v!0, 0 // Position of the previous element, if any.

  // Initially p, and v are both zero.

  IF p>=upb DO
  { // v is not large enough, so a larger vector must be allocated.
    LET newupb = 3*upb/2 + 10 // Increase the size by about 1/3.
    LET newv = getvec(newupb)
//writef("pushval: allocating vector %i6 upb %n*n", newv, newupb)
//abort(2222)
    UNLESS newv DO
    { trerr(-1, "More memory needed")
      RETURN
    }
    sxv!0 := newupb
    sxv!1 := newv
    // Copy the existing elements, but not v!0
    FOR i = 0 TO upb DO newv!i := v!i
    // Pad with zeroes
    FOR i = upb+1 TO newupb DO newv!i := 0
    // Free the old vector if it existed.
    IF v DO freevec(v)

    IF debugv!1 DO
    { writef("pushval: replacing v=%i6 upb=%i6 with newv=%i7 upb=%i6 p=%n*n",
              v, upb, newv, newupb, p)
      abort(6666)
    }
    v := newv
  }
  p := p+1
  v!0, v!p := p, val
}

AND pushmsecsval(sxv, absq, FLT tempo) BE
{ // This pushes the triplet [absq, rate, 0.0] into
  // msecs self expanding vector sxv.
  
  // Convert from quarter notes per minute to msecs per qbeat.
  LET FLT rate = (60 * 1000) / (1024 * tempo)

writef("pushmsecsval: absq=%5i, adjusted tempo=%7.3f rate=%7.3f*n",
        absq, tempo, rate)
 
  pushval(sxv, absq)
  pushval(sxv, rate)
  pushval(sxv, 0.0)
abort(2840)
}

AND pushshape(sxv, absq, FLT x) BE
{ // This pushes the shape entry [absq,x] into shape
  // self expandingvector sxv. At this stage ordinary shapes
  // and adjustment shapes are independent.

//writef("pushshape: absq=%i6 x=%7.3f*n", absq, x)
  pushval(sxv, absq)
  pushval(sxv, x)
  //sortenv(env) // The latest entry may not be in sorted order.
//abort(2840)
}

AND sortenv(sxv) BE
{ // This is only called when env has just received a new entry.
  // The most recently added entry may be out of order.
  LET upb = h1!sxv
  LET v   = h2!sxv
  LET p = h1!v - 1            // Subscript of the latest entry
  LET q, FLT x = v!p, v!(p+1) // Copy the latest entry

//writef("*nsortenv: entered q=%n x=%9.3f p=%n*n", q, x, p)
//prshapes()
//IF p=7 DO
//abort(2928)

  IF p<3 RETURN  // Only one entry, so no sorting necessary.

//writef("sortenv: more than one entry v!(p-2)=%n q=%n p=%n*n", v!(p-2), q, p)
//abort(2929)

  IF v!(p-2) <= q RETURN // Latest entry already sorted.

//writef("sortenv: sorting needed v!(p-2)=%n q=%n p=%n*n", v!(p-2), q, p)
//abort(2930)

  WHILE p>=3 & v!(p-2) > q DO
  { 
//writef("sortenv: moviing entiry v!(p-2)=%n v!(p-1)=%9.3f at %n to %n*n", 
//                 v!(p-2), v!(p-1), p-2, p)
//abort(2931)
    v!p, v!(p+1) := v!(p-2), v!(p-1)
    p := p-2
  }
  // Insert the saved entry into correct position
//writef("sortenv: Storing entry q=%n x=%9.3f into position p=%n*n", 
//                 q, x, p)
  v!p, v!(p+1) := q, x

//prshapes()

//abort(2932)
}

AND getval(sxv, pos) = VALOF
{ LET upb = h1!sxv
  LET v   = h2!sxv
  LET p   = v -> h1!v, 0
  UNLESS p & 1<=pos<=p DO
    trerr(-1, "System error in getval")
  RESULTIS v!pos
}

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
    LET midimsecs = FIX absq2msecs(tqpos)
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

AND absq2beatno(absq) = VALOF
{ // This function returns the beat number of absq in the bar
  // containing absq.
  LET qbarv = barsxv!1
  LET qbeatv = beatsxv!1

  IF qbarv & qbeatv DO
  { //
    //
    //  -------------------------
    //    |                   |    Barlines
    //  --1----2----3----4----1--  Beats in bars
    //    |         |  |
    //    |         |  absq
    //    |         3              The result required
    //    |         beatqbeat1     Qbeat containing absq
    //    |         b1             Beat containing absq
    //    barno                    Barno of the bar containing absq
    //    barqbeat                 qbeat of bar containing absq
    //    beatqbeat0               Qbeat of the the first beat of this bar
    //    b0                       Beat of the start of this bar
    // The result is b1-b0+1 = 3

    LET beatv = h6!beatsxv
    LET barno = absq2barno(absq)
    LET barqbeat = qbarv!barno
    LET maxbeatno = qbeatv!0
    LET beatno0 = 1
    
    { LET qbeat = beatv!beatno0
      IF qbeat >= barqbeat BREAK
      beatno0 := beatno0+1
    } REPEAT

    FOR bno = 1 TO 100 DO
    { LET beatno1 = beatno0+bno
      IF absq < qbeat RESULTIS bno
      qbeat := beatv!beatno1
    }
    RESULTIS 1
  }


  RESULTIS 0
}

AND absq2barno(absq) = VALOF
{ // Return the number of the bar containing the given absolute qbeat location.

  // We use the simplest algorithm.
  LET v = barsxv!1
  IF v=0 | absq < v!1 RESULTIS 0
  FOR b = 1 TO v!0 IF absq<=v!b RESULTIS b
  // Otherwise return the maximum bar number.
  RESULTIS v!0
}

// Scale a local qbeat position to an absolute position
AND qscale(q) = VALOF
{ // Return the equivalent of scbase + q * scfac_top / scfac_bot
  LET res = ?
//writef("qscale: q=%n scbase=%n scfac_top=%n scfac_bot=%n*n",
//                q,   scbase,   scfac_top,   scfac_bot)

  UNLESS scfac_bot DO
  { writef("System error: scfac_bot=0*n")
    abort(999)
    RESULTIS q
  }
  TEST scfac_top=scfac_bot
  THEN res := scbase + q
  ELSE res := scbase + muldiv(q, scfac_top, scfac_bot)
//  writef("qscale: q=%n scbase=%n scfac_top=%n scfac_bot=%n => absq %n*n",
//          q, scbase, scfac_top, scfac_bot, res)
//abort(1000)
  RESULTIS res
}


AND trparts(t) BE
{ // t -> [-, Parts, ln, partlist, qlen]

  // Translate each part or solo as blocks.
  LET partlist = h4!t
writef("trparts: entered, qlen=%n*n", h5!t)
//abort(7729)
  midichannel := 0 // A value between 0 and 15

  WHILE partlist DO
  { // partlist -> [-, Part, ln, notelist, qlen, chan]
    // or       -> [-, Solo, ln, notelist, qlen, chan]
    // or       -> 0
    // Translate each part in the score as a block
    // First set up the default scaling parameters and
    // set currqbeat to zero

    // Initialise the translation of a part.
    scbase, scfac_top, scfac_bot := 0, 1, 1
    currqbeat := 0
    currbarno := 0
    variablevol := FALSE

    tlist, tqpos := 0, 0 // Initialising the tie mechanism for
    plist, pqpos := 0, 0 // this part.
    clist, cqpos := 0, 0

    IF midichannel>15 DO
    { writef("trblock: Too many parts or solos*n")
      midichannel := 1
      abort(999)
    }
writef("trparts: Translating part or solo %n, midi channel=%n*n",
        partlist, midichannel)
//abort(7724)
    trblock(partlist)
//    abort(7731)
    partlist := !partlist

    // Choose another midi channel for the next part or solo.
    // but avoid midichannel 9 which is used for percussion.
    midichannel := midichannel + 1 REPEATWHILE midichannel=9
    writef("partlist=%n Next midi channel = %n*n", partlist, midichannel)
  }
}

AND genmidi(t) BE
{ // t is the node in the parse tree to translate.
  // currqbeat holds the local qbeat number of the first qbeat
  // of this node. The corresponding absolute qbeat is computed
  // by qscale(currqbeat) which uses scbase, scfac_top and scfac_bot.

  // The environments of shape data is held in vectors such as
  // volenv and tempoenv. These were setup when the current
  // block was entered. They all have the same structure.
  // For instance volenv -> [envlist, op, ln, parent, upb, v] where
  //    upb is zero or the upper bound of v
  //    v  =  0
  //    or -> [2n, q1, x1,... qn, xn] where
  //       q1 .. qn are absolute qbeat locations and
  //       x1 .. xn are the corresponding shape values.

  // tlist and clist hold the lists of the current outstanding
  // note ties.

  // The scaling parameters are held in scbase, scfac_top and scfac_bot.
  // These are required for the implementation of (x)\tuplet(qlen)
  // allowing local qbeat locations to be mapped to absolute values.
  // scbase is the qbeat of the start of note segment x after scaling,
  // scfac_top=qlen is the number of qbeats in the note list after scaling,
  // scfac_bot is the number of qbeats in the note list before scaling,

  // qscale(q) is equivalent to: scbase + muldiv(q, scfac_top, scfac_bot).

  // Midi data is held as items in the list midilist. Its last item
  // is pointed to by midiliste. If the list is empty midilist=0 and
  // midiliste points to midilist. Each item in the list is the form
  // [link, msecs, midi_triple]. Where
  //   link is zero or points to the next midi item in the list,
  //   msecs is the integer time of this event in milli-seconds from the
  //      start of the composision,
  //   midi_triple is a packed collections of bytes #Xccbbaa where
  //      aa is the Midi status byte such as note_on or note_off
  //         including the Midi channel number
  //      bb and cc are related nidi arguments,
  //      Although not currently used, a non midi entry can be
  //      represented using aa less that 128.

  // midilist is later sorted to increasing time order.

  // t -> [-. op, ln, a1, a2]

  LET op = h2!t  // The tree node operator
  LET ln = h3!t  // The fno/ln number of the tree node
  LET midia1, midia2, midia3 = 0, 0, 0 // Midi op and its argument bytes
  LET opname  = opstr(op)
  tokln := ln

writef("genmidi: About to prlocation(currqbeat)*n")
//  IF FALSE & optNtrace DO
  { LET absq  = qscale(currqbeat)  // Scaled qbeat number
    prlocation(currqbeat)
    writef("%10t ", opname)
    writef("tempo=%i3 ",  FIX adjustedshapeval(absq, tempoenv,  tempoadjenv))
    writef("delay=%i3 ",  FIX adjustedshapeval(absq, delayenv,  delayadjenv))
    writef("vol=%i3 ",    FIX adjustedshapeval(absq, volenv,    voladjenv))
    writef("legato=%i3 ", FIX adjustedshapeval(absq, legatoenv, legatoadjenv))
    prlineno(ln)
    newline()
  abort(1003)
  }

writef("genmidi: op=%s currqbeat=%n currbarno=%n*n",
        opname, currqbeat, currbarno)
  SWITCHON op INTO
  { DEFAULT:
      // Ignore most node types
      RETURN

    CASE s_name:
      currpartname := h4!t
      IF optNtrace DO
      { prlocation(currqbeat)
        writef("%10t  %s*n", opname, currpartname)
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
    { LET dly = FIX adjustedshapeval(currqbeat, s_delay, s_delayadj)
      // currqbeat and dly are both unscaled qbeat values
      LET absq = qscale(currqbeat+dly)
      // Use the timemap data in the environment to calculate the
      // midi msecs value.
      LET midimsecs = FIX absq2msecs(absq)
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
//                 opname, n,  transposition,   pitch,  midi_note)
//abort(3219)

      UNLESS istied(midi_note, nomqs) DO
      { // This note does not resolve a previous tie, so play it.
        LET dly       = FIX adjustedshapeval(nomqs, delayenv, delayadjenv)
        LET legato    = FIX adjustedshapeval(nomqs, legatoenv, legatoadjenv)
        LET absqs     = qscale(currqbeat+dly)
        LET midimsecs = FIX getmsecs(msecsenv, absqs)
        LET chan      = midichannel // chan is in range 0 to 15

        // The +1 is a fudge at the moment to avoid a problem
        // when two different shape values are at the same
        // qbeat position. May be this should not be allowed
        // to happen.
        LET vol = FIX adjustedshapeval(nomqs, volenv, voladjenv)

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
               { LET dly = FIX adjustedshapeval(q, delayenv, delayadjenv)
                 LET legato = FIX adjustedshapeval(q, legatoenv, legatoadjenv)
                 LET absqs = qscale(q+dly)
                 LET midimsecs = FIX getmsecs(msecsenv, absqs)
                 LET vol = FIX adjustedshapeval(q+1, volenv, voladjenv)

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
               { prlocation(currqbeat+dly)
                 writef("%10t  chan=%n note=%t4  vol=%n*n",
                   "Note on", midichannel, note2str(midi_note, strv), vol)

//               apmidi(midimsecs,                 // test GS percussion
//               midi_note_on+9+(71<<8)+(vol<<16))
               }
             }
     }
//abort(3881)
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
            LET leg       = FIX adjustedshapeval(currqbeat, legatoenv, legatoadjenv)
            LET qe        = currqbeat + (qlen * leg) / 100
            LET dly       = FIX adjustedshapeval(qe, delayenv, delayadjenv)
            LET absqe     = qscale(qe+dly)
            LET midimsecs = FIX getmsecs(msecsenv, absqe)

//writef("%i6: Note off: chan=%n note=%i3  legato=%n*n",
//       midimsecs, midichannel, n, leg)
            apmidi(midimsecs,                    // Note off
                   midi_note_off+midichannel+(midi_note<<8))
            IF optNtrace DO
            { prlocation(qe+dly)
              writef("%10t  chan=%n note=%t4*n",
                      "Note off", midichannel, note2str(midi_note, strv))
            }
          }

      // Return the qbeats value of the next note item. 
      currqbeat := currqbeat + qlen
//writef("genmidi: note %n done currqbeat=%n*n", midi_note, currqbeat)
      RETURN
    }//########

    CASE s_transposition:
      transposition := h4!t
//writef("genmidi: transposition set to %n*n", transposition)
      RETURN


    CASE s_bank:
//writef("CASE s_bank:*n")
    { LET dly = FIX adjustedshapeval(currqbeat, s_delay, s_delayadj)
      // currqbeat and dly are both unscaled qbeat values
      LET absq = qscale(currqbeat+dly)
      // Use the timemap data in the environment to calculate the
      // midi msecs value.
      LET midimsecs = FIX absq2msecs(absq)

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
    { LET dly = adjustedshapeval(currqbeat, s_delay, s_delayadj)/1000
      // currqbeat and dly are both unscaled qbeat values
      LET absq = qscale(currqbeat+dly)
      // Use the timemap data in the environment to calculate the
      // midi msecs value.
      LET midimsecs = FIX absq2msecs(absq)

      apmidi(midimsecs,                            // Msecs
             midi_progchange+midichannel+(h4!t<<8))  // Patch command
      IF optNtrace DO
      { writef("%9.3d Patch:     chan=%n prog=%n*n",
               midimsecs, midichannel, h4!t)
      }
      RETURN
    }

    CASE s_part:      // [-, Part,      ln, notelist,  qlen, midichan]
    CASE s_solo:      // [-, Solo,      ln, notelist,  qlen, midichan]
      writef("genmidi: %s*n", opname)
abort(999)
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

    CASE s_delay:      // [-, delay,     ln, notelist, shapelist, qlen]
    CASE s_delayadj:   // [-, delayadj,  ln, notelist, shapelist, qlen]
    CASE s_legato:     // [-, legato,    ln, notelist, shapelist, qlen]
    CASE s_legatoadj:  // [-, legatoadj, ln, notelist, shapelist, qlen]
    CASE s_tempo:      // [-, tempo,     ln, notelist, shapelist, qlen]
    CASE s_tempoadj:   // [-, tempoadj,  ln, notelist, shapelist, qlen]
    CASE s_vibrate:    // [-, vibrate,   ln, notelist, shapelist, qlen]
    CASE s_vibrateadj: // [-, vibrateadj,ln, notelist, shapelist, qlen]
    CASE s_vibamp:     // [-, vibamp,    ln, notelist, shapelist, qlen]
    CASE s_vibampadj:  // [-, vibampadj, ln, notelist, shapelist, qlen]
    CASE s_vol:        // [-, vol,       ln, notelist, shapelist, qlen]
    CASE s_voladj:     // [-, voladj,    ln, notelist, shapelist, qlen]
      // The shape data has already been extracted and is strored
      // in the environment vectors.
      genmidi(h4!t)
      RETURN

    CASE s_notelist:       // [-, Notelist, ln, list, qlen]
    { LET list = h4!t
      //writef("genmidi: %s  ", opname; prlineno(ln); newline()
      WHILE list DO
      { genmidi(list)
        list := !list
      }
      RETURN
    }

    CASE s_barline:
    CASE s_doublebar:
    { // Check that the barline occurs at the right place.
      LET absq = qscale(currqbeat)
      LET v = barsxv!1 // v!1 to v!maxbarno are the qbeat locations
                       // of every barline.

      UNLESS scfac_top=scfac_bot DO
      { trerr(-1, "Barlines are not permitted within a tuplet")
        RETURN
      }
      
      currbarno := currbarno+1
      writef("genmidi: currbarno incremented to %n*n", currbarno)
      
      IF currbarno>maxbarno DO      
      { trerr(-1, "Part %s has more barlines than the conductor part",
                   currpartname)
        RETURN
      }
      
      UNLESS absq=v!currbarno DO
      { // There is an error.
        LET diff = currqbeat - v!currbarno
        trerr(-1, "Misplaced barline, %n qbeats too %s*n",
                   ABS diff, diff<0 -> "early", "late")
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
      LET prevclist, prevcpos = clist, cqpos
      LET prevplist, prevpqpos = -1, -1
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
             prevplist, prevpqpos := plist, pqpos
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
          THEN { LET bn1 = absq2barno(currqbeat)
                 LET bn2 = absq2barno(q0+qlen)
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

      // All members of the par construct have been processed,
      // so set tlist = clist, restore the previous clist, and
      // conditionally restore plist.

      IF prevplist>=0 DO
      { // This par construct started later than an enclosing one,
        // so all ties in plist must have been resolved by now. 
        // Check for unresolved ties in plist.
        //IF plist DO
        //{ tlist, tqpos := plist, pqpos
        //  checktlist(envblk)
        //}
        // Restore previous plist
        plist, pqpos := prevplist, prevpqpos
      }

      // Set tlist and clist appropriately.
      tlist, tqpos :=  clist,  cqpos
      clist, cqpos := prevclist, prevcpos

      currqbeat := q0 + qlen

//writef("Leaving par construct with currqbeat=%n*n", currqbeat)
//prties()
      RETURN
    }

    CASE s_tuplet:
    { // t -> [-, Tuplet, ln, block, qlen]

      LET notelist   = h4!t
      LET toqlen     = h5!t
      LET fromqlen   = calcqlen(notelist)

      // Save the previous scaling parameters
      LET prevcurrqbeat = currqbeat
      LET prevscbase, prevscfac_top, prevscfac_bot = scbase, scfac_top, scfac_bot
      scbase := qscale(currqbeat) // Absolute q position of the notelist
      setscaleparams(fromqlen, toqlen)
      currqbeat := 0 // The start position in the notelist


writef("genmidi: %s <%n/%n> saved clist  ", opstr(op)); prlineno(ln); newline()

      genmidi(notelist)

      // Restore the previous scaling parameters
      scbase, scfac_top, scfac_bot := prevscbase, prevscfac_top, prevscfac_bot

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
  // and update end_msecs if necessary.
  LET node = mk3(0, t, code)
  !midiliste := node
  midiliste := node
  IF t>end_msecs DO end_msecs := t
  writef("apmidi: t=%i6 %x8  end_msecs=%n*n", t, code, end_msecs)
}

AND absq2msecs(absq) = getmsecs(msecsenv, absq)

AND barno2msecs(bno) = VALOF
{ // bno is a barline number.
  LET q  = barno2qbeat(bno)
  LET ms = FIX absq2msecs(q)
writef("*nbarno2msecs: bno=%n => q=%n => result=%n msecs*n", bno, q, ms)
abort(1003)
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
{ // pushbyte is similar to pushval but is only used to append a byte
  // of midi data held in the self expanding vector cb.

  // cb -> [upb, v]      // upb is the upper bound of v in words
  // v  -> [pos, ... ]   // pos is the position of the next byte in v

  // It uses getvec and freevec to allocate space as needed.

  LET upb  = cb!0                  // Current upb (in words) of v
  LET bupb = upb*bytesperword      // The upb of v in bytes
  LET v    = cb!1                  // is zero or a getvec'd vector holding 
                                   // the bytes.
  LET p = v -> v!0, bytesperword-1 // Byte pos in v of prev element, if any.
                                   // The data in v are held in the byte range
				   // bytesperword to p.

//writef("cb=%n p=%n upb=%n v=%n*n", cb, p, upb, v)

  // The size of v grows as needed.

  // Initially upb and v are zero and p is batesperword-1. These values
  // are suitable for the first call of pushbyte.

  IF p>=bupb DO
  { // There is no room in v for another byte so increase its size
    // by about a third.
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
writef("midilist=%n*n", midilist)
  WHILE midilist DO
  { // midilist = 0 or 
    //          -> [next, midimsec, triple]
    LET midimsecs = midilist!1
    LET triple    = midilist!2
    LET op, a1, a2, is_note_on = ?, ?, ?, ?
    LET fn, chan = ?, ?
    LET playing = ?
    LET dr, r = 0, 0

writef("midimsecs=%n stop_msecs=%n*n", midimsecs, stop_msecs)

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

  pushnum (cb, 0)            // Delta time = 0
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
The tempo, volume and many other features of a performance can change
while it is played. These features are are specified by shape values
that can be placed anywhere within a note sequence. A typical example
is as follows

              (4c4 d e f) \vol (50 s4 100 s2 75)

Here the note sequence plays four quarter notes c, d, e and f occupying
a total of 4096 qbeats. The shape data has length 3072 (=1024+2048)
specifying three volume values 50, 100 and 75. The length of the shape
data is scaled to be the same as the note sequence causing 50 to be
placed at qbeat zero, 100 at qbeat 1365 (=1024*4096/3072) and 75 at
4096.  If the length of the shape sequence is zero, its shape value or
values are placed at the start of the note sequence.

The scope of shape data is the enclosing block. It forms a piecewise
linear graph of how the shape value changes throughout the block. The
shape data is held in the shape environment for this kind of shape.

If the shape had no explicit value at the start of the block, it is
given the value of the enclosing environment at that position.  If the
shape had no explicit value at the end of the block, it is given the
value of the enclosing shape at that position.

The shape value at a particular shape kind and qbeat is determined by
linear interpolation between the nearest two enclosing shape points in
the relevent shape environment held in the current block. If the
reqired shape environment is empty, the value is computed using the
next enclosing block. If none of the enclosing environments are non
empty, the default value for that kind of shape is used.

If the current shape environment and an enclosing shape environment
are both non empty then their values at the qbeat position are
combined by taking their average. A typical example is when the
conductor specifies a certain Vol setting and a soloist specifies
another. They both affect the volume value with equal weight.

Note that shape values in the current block are exactly the values
specified by the relevant shape sequence. The detailed evaluation of
shape values is not done until later when midi events are being
generated.

The Tempo and Tempoadj shapes are exceptional since they are used by
the conductor to construct a structure that allows the time in msecs
of each qbeat to be computed efficiently. Tempo values within an inner
block modifies the speed of playing within the inner block but this
speed is scaled to cause the inner block to take exactly the same time
as specified by the conductor. See the definition of the function
setmsecsenv for more information about how timing is computed.

*/

LET adjustedshapeval(absq, env, envadj) = VALOF
{ // This function returns the shape value at location absq of
  // env after multiplying the corresponding adjustment value
  // at the same location.

  // absq is an absolute qbeat location.
  // env -> [env, op, ln, parent, upb, v]   env is non zero
  //        upb is the upperbound of v
  //        v!0=p and v!1 .. v!p contain the shape items
  //        of the form [q,val] for this shape.
  // envadj is the corresponding adjustment environment.

  LET type = h2!env // eg s_volenv
  LET FLT val = shapeval(env,    absq)
  LET FLT adj = shapeval(envadj, absq)
  LET FLT res = (val*adj) / 100.0 // Apply the adjstment
                                  // which is a percentage.
//IF FALSE
UNLESS env=tempoenv DO
{ //writef("adjustedshapeval: absq=%i6 val=%8.3f adj=%8.3f => res=%8.3f*n",
  //        absq, val, adj, res)
  //abort(1873)
}
  RESULTIS res
}

AND averageshapeval(type, e, absq) = VALOF
{ LET res = shapeval(type, e, absq)
  IF e & h3!e RESULTIS (res+shapeval(type, h3!e, absq))/2.0
  RESULTIS res
}

AND defaultval(type) = VALOF SWITCHON type INTO
{ DEFAULT: writef("System error in defaultval, type=%s*n", opstr(type))
           abort(999)
	   RESULTIS 0

  CASE s_delayenv:
  CASE s_delay:        RESULTIS   0.0
  CASE s_delayadjenv:
  CASE s_delayadj:     RESULTIS   0.0
  CASE s_legatoenv:
  CASE s_legato:       RESULTIS  70.0
  CASE s_legatoadjenv:
  CASE s_legatoadj:    RESULTIS   0.0
  CASE s_tempoenv:
  CASE s_tempo:        RESULTIS 120.0
  CASE s_tempoadjenv:
  CASE s_tempoadj:     RESULTIS   0.0
  CASE s_vibrateenv:
  CASE s_vibrate:      RESULTIS   5.0
  CASE s_vibrateadjenv:
  CASE s_vibrateadj:   RESULTIS   0.0
  CASE s_vibampenv:
  CASE s_vibamp:       RESULTIS   0.25
  CASE s_vibampadjenv:
  CASE s_vibampadj:    RESULTIS   0.0
  CASE s_volenv:
  CASE s_vol:          RESULTIS  70.0
  CASE s_voladjenv:
  CASE s_voladj:       RESULTIS   0.0
}

AND shapeval(type, env, absq) = VALOF
{ // This calculates the value of an environment at
  // a specified absolute qbeat location. It is only called after all
  // environments have been converted to use absolute locations.
  // type is the type of the environment eg s_vol.
  // env ->  [envlist, op, ln, parent, upb, v]
  // or  =   0
  // where
  // upb,v is a self expanding vector, possibly compacted.
  // parent is zero or the enclosing environment of the same type.

  // shapeval attempts to find the nearest environment whose range
  // includes absq. If it exist the value is obtained by linear
  // interpolation, otherwise it returns the default value of the
  // specified shape type.

  LET qbeat1, qbeat2 = 0, 0 // Will hold the qbeat limits of the nearest
                            // environment, if any, that contains absq.
  LET v = 0                 // Will hold the environment coodinates.
  LET last = 0              // Will hold the subscript of the last entry,
                            // if any, of the enclosing environment.
  LET i = 3                 // The subscript of the second entry, if present.

RESULTIS defaultval(type)

  // Try to find the closest environment containing absq.
  WHILE env DO
  { v := h2!env             // The vector of environment coordinates.
    last := v!0-1           // Subscript of the last entry.
    qbeat1 := v!1           // The start and end qbeats of this
    qbeat2 := v!last        // environment's region.
    IF qbeat1<=absq<=qbeat2 BREAK
    env := h3!env           // Go out one level.
  }

  UNLESS env DO
  { // There was no suitable enclosing environment.
    writef("*nshapeval: absq=%n No suitable enclosing environment of type %s*n",
           absq, opstr(type))
    abort(2661)
    RESULTIS defaultval(type)
  }

  // env is the closest enclosing environment containing absq.

  IF i > last DO
  { writef("shapeval: System error: i=%n > last=%n*n", i, last)
    abort(999)
    RESULTIS 0.0
  }

  IF absq = v!last RESULTIS v!(last+1)
 
  // qbeat1 <= absq < qbeat2 = v!last

  // Find the surrounding pair of shape points.

  { LET q = v!i
writef("*nshapeval: absq=%n i=%n v!(i-2)=%n v!(i-1)=%7.3f v!i=%n v!(i+1)=%7.3f*n",
        absq, i, v!(i-2), v!(i-1), v!i, v!(i+1))
    IF q <= absq BREAK
    i := i+2
  } REPEAT

  RESULTIS interpolate(absq, v!(i-2), v!(i-1), v!i, v!(i+1))
}

AND interpolate(q, q1, FLT x1,
                   q2, FLT x2) = VALOF
{ // q1 <= q <= q2 and q1 < q2
  // Note the qbeat locations are integers while values are floating point.
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
{ LET midiname = "/dev/dmmidi1" // Should work with my Casio Bechstein piano
                                // when running under Linex.
				// dev/midi1 may be better.
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

AND msecs2barno(FLT m_msecs) = VALOF
{ // Set and return currbarno to the number of the barline
  // that starts the bar containing m_secs.
  // maxbarno was already set by trconductor. It returns zero
  // if m_msecs is a time before the first barline.
  
  LET v = msecsenv!1 // v!1 to v!maxbarno are the msecs times
                     // of every bar of the conductor's part.
  FOR n = 1 TO maxbarno DO
  { IF m_msecs < v!n DO
    { currbarno := n-1
      RESULTIS currbarno
    }
  }
  currbarno := maxbarno
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

