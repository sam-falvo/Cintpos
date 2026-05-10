/*

This is a program reads a .mus file representing a score and possibly
writes a corresponding MIDI file and/or plays it on a MIDI device
possibly synchronising the accompanement with a soloist using a
microphone. This program is still under development.

Modified Implemention by Martin Richards (c) 12 Feb 2023

Usage: FROM,START/N,END/N,PLAYRATE=-pr/N,TO/K,UPB/K/N,
       PP/S,LEX/S,TREE/S,STRACE/S,NTRACE/S,MTRACE/S,
       MIDI/S,PLAY/S,ACC/S,PITCH/N,GD/S,WAIT/S,CALIB/S,DEBUG/N 

FROM      The root of the .mus file, eq beanbag for beanbag.mus
START/N   The number of the first bar to play
END/N     The number of the last bar to play
PLAYRATE=-pr/N The tempo adjustment as a percentage, so 50.0 means half speed.
          This speed of playing the given score may be adjusted in real
          time by microphone input when accompanying a solist.
TO/K      The destination file for messages
UPB/N/K   The size of the BGPM work space
PP/S      Just output the .mus file after BGPM expansion
LEX/S     Just output the .mus file as a sequence of lexical tokens
TREE/S    Just output the parse tree as produced by the parser
STRACE/S  Trace the syntax analyser processing
NTRACE/S  Trace the processing of notes
MTRACE/M  Trace the generation of midi statements
MIDI/S    Generate a .mid file based on the FROM file name
PLAY/S    Play the midi translation directly to typically /dev/midi1
ACC/S     Accompany a soloist using the microphone
PITCH/N   Adjust the pitch
GD/S      Generate graph data as a debugging aid
WAIT/S    Wait before playing
CALIB/S   Calibrate the midi-microphone delay
DEBUG/N   Complement a debug flag 0 to 9


Change history

17/04/2023
Changed lex treatment of notes, spaces and rests.
token=s_note or s_notetied, eg aes''4..~ its sets
   noteletter 'a' to 'g'
   notesharps -2 to +2
   reloctave  -9 to +9
   noteqlen = Nominal note length (without dots) or -1
   dotcount = 0, 1, 2,...
token=s_space or s_rest, eq rq1024. ro s8 it sets
   noteqlen = Nominal note length (without dots) or -1
   dotcount = 0, 1, 2,...

13/04/2023
Renamed applytuplets to q2blkq.

03/02/2023
Redesigned the implementation of Tuplets and the qshiftv element
of blocks. Implemented the functions q2blkq and q2absq.

02/01/2023
Currently making major changes to the Mus language with corresponding
changes to this program.

12/05/2021
Minor modifications.

05/07/2019
Disallowed ties in shape lists. Any shape in a block must start and
end with a value. A star is automatically inserted at the start and
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

block     -> [-, Block,  ln, notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
          -> [-, Notes,  ln, notelist, qlen]     If no shape data

parts     -> [0, Parts,  ln, partlist, qlen]     Lists are always
shape     -> [0, Shape,  ln, shapelist, qlen]    operands of
envs      -> [0, Envs,   ln, envlist]            these five
notes     -> [0, Notes,  ln, notelist, qlen]     operators
par       -> [0, Par,    ln, parlist, qlen]

partlist  -> 0
          -> [partlist, Part, ln, block, qlen]
          -> [partlist, Solo, ln, block, qlen]

shapelist -> 0 
          -> [shapelist, Num,   ln, value]
          -> [shapelist, Star,  ln]
          -> [shapelist, Space, ln, qlen]
          -> [shapelist, Null,  ln]

envlist   -> 0
          -> [envlist, Vibrateenv,    ln, parent, upb, v, absq1, absq2]
          -> [envlist, Vibrateadjenv, ln, parent, upb, v, absq1, absq2]
          -> [envlist, Vibampenv,     ln, parent, upb, v, absq1, absq2]
          -> [envlist, Vibampadjenv,  ln, parent, upb, v, absq1, absq2]
          -> [envlist, Volenv,        ln, parent, upb, v, absq1, absq2]
          -> [envlist, Voladjenv,     ln, parent, upb, v, absq1, absq2]
          -> [envlist, Volmapenv,     ln, parent, upb, v, absq1, absq2]
          -> [envlist, Tempoenv,      ln, parent, upb, v, absq1, absq2]
          -> [envlist, Legatoenv,     ln, parent, upb, v, absq1, absq2]
          -> [envlist, Legatoadjenv,  ln, parent, upb, v, absq1, absq2]
          -> [envlist, Delayenv,      ln, parent, upb, v, absq1, absq2]
          -> [envlist, Delayadjenv,   ln, parent, upb, v, absq1, absq2]

In environments the range of qbeats is specified by absolute value since
local qbeat locations are not easy to determine. When a shape value is
needed at the current local qbeat location the absolute location can be
found using q2absq and there is no need to determine which Block owns the
environment.

notelist -> 0          // A list of note items linked by the h1 chain
         -> [notelist, Altoclef, ln]
         -> [notelist, Arranger, ln, string]
         -> [notelist, Bank, ln, int1, int2]
         -> [notelist, Barlabel, ln, string]
         -> [notelist, Barline,  ln]
         -> [notelist, Bassclef, ln]
         -> [notelist, Block,ln,notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
         -> [notelist, Composer, ln, string]
         -> [notelist, Control, ln, int1, int2]
         -> [notelist, Delay, ln, notes, shape, qlen]
         -> [notelist, Delayadj, ln, notes, shape, qlen]
         -> [notelist, Doublebar, ln]
         -> [notelist, Instrument, ln, string]
         -> [notelist, Instrumentname, ln, string]
         -> [notelist, Instrumentshortname, ln, string]
         -> [notelist, Keysig, ln, <letter:sharps:dots:note>, mode]
         -> [notelist, Legato, ln, notes, shape, qlen]
         -> [notelist, Legatoadj, ln, notes, shape, qlen]
         -> [notelist, Name, ln, string]
         -> [notelist, Nonvarvol, ln]
         -> [notelist, Note, ln, <letter:sharps:dots:note>, qlen]
         -> [notelist, Notes, ln, notelist, qlen]
         -> [notelist, Notetied, ln, <letter:sharps:dots:note>, qlen]
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
         -> [notelist, Tempo, ln, notes, shape, qlen]
         -> [notelist, Tempoadj, ln, notes, shape, qlen]
         -> [notelist, Tenorclef, ln]
         -> [notelist, Timesig, ln, int1, int2]
         -> [notelist, Title, ln, string]
         -> [notelist, Transposition, ln, int]
         -> [notelist, Trebleclef, ln]
         -> [notelist, Tuplet, ln, notes, qlen, parent, qbeat, toqlen]
         -> [notelist, Varvol, ln]
         -> [notelist, Vibamp, ln, notes, shape, qlen]
         -> [notelist, Vibampadj, ln, notes, shape, qlen]
         -> [notelist, Vibrate, ln, notes, shape, qlen]
         -> [notelist, Vibrateadj, ln, notes, shape, qlen]
         -> [notelist, Vol, ln, notes, shape, qlen]
         -> [notelist, Voladj, ln, notes, shape, qlen]
         -> [notelist, Volmap, ln, shape]
*/

SECTION "Playmus"

GET "libhdr"
GET "playmus.h"

LET AAAfirstfn() BE RETURN
AND ZZZlastglobal() BE RETURN

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
  LET tadjval = 1000
  LET play = FALSE
  LET b   = VEC 64
  LET bln = VEC 64
  AND s1  = VEC 10
  AND dbv = VEC 9
  debugv := dbv
  FOR i = 0 TO 9 DO debugv!i := FALSE

  msecsv := 0 // For safety
  
  playmus_version := "Playmus V2.1 (13 Apl 2023)" // Used here and in writemidi.
  writef("*n%s*n", playmus_version)

//abort(5112)
  currpartname := 0
  // Initialise the bar self expanding vextor
  barsxv_upb, barsxv_v := 0, 0
  barsxv  := @barsxv_upb

  barmsecs := 0
  
  envbits := 0 // Use by rdblock to determine which shape types are
               // defined within the block.
  
  sysin := input()
  sysout := output()
  sourcestream := 0
  tostream := 0

  veclist := 0
  killco := 0
  bgpmco := 0
  bg_base := 0
  blklist := 0
  soundv, soundp := 0, 0

  UNLESS rdargs("FROM,START/N,END/N,PLAYRATE=-pr/N,TO/K,UPB/K/N,*
                *PP/S,LEX/S,TREE=-t/N,STRACE/S,NTRACE/S,MTRACE/S,*
                *MIDI/S,PLAY/S,ACC/S,PITCH/N,GD/S,WAIT/S,CALIB/S,FACA=-fa/N,*
                *DEBUG/N",
		argv, 75) DO
  { writef("Bad arguments for PLAYMUS*n")
    RESULTIS 0
  }

  pitch := 0
  transposition := 0

  shapefaca := 0.5
  shapefacb := 1.0-shapefaca
  
  // Set default values of the switch variables.
  // These can be complemented by the /S command arguments.
  optPp       := FALSE
  optLex      := FALSE
  optTree     := FALSE
  optStrace   := FALSE
  optNtrace   := FALSE
  optMtrace   := FALSE
  optgenmidi  := FALSE
  optfaca     := 50      // Default conductor weight 50%
  optplay     := FALSE
  accompany   := FALSE
  graphdata   := FALSE
  waiting     := FALSE
  calibrating := FALSE
  
  FOR i = 1 TO 9 DO debugv!i := FALSE
  //debugv!1 := TRUE // Trace pushval 

  IF argv!0  DO filenameroot := argv!0       // FROM      The .mus file name root
  IF argv!1  DO startbarno   := !(argv!1)    // START/N   First bar to play
  IF argv!2  DO endbarno     := !(argv!2)    // END/N     Last bar to play
  IF argv!3  DO tadjval      := !(argv!3)    // PLAYRATE=-pr/N Tempo adjustment
  IF argv!4  DO toname       := argv!4       // TO/K
  IF argv!5  DO bg_baseupb   := !(argv!5)    // UPB/N/K   BGPM space
  IF argv!6  DO optPp        := ~optPp       // PP/S      Print generated text 
  IF argv!7  DO optLex       := ~optLex      // LEX/S     Trace lexical tokens
  IF argv!8  DO optTree      := !(argv!8)    // TREE=-t/N Print parse tree
                                             // -t  0  Dont
					     // -t  1  Print initial tree
					     // -t  2  Print after calcqlen
					     // -t  4  Print after findrawshapes
					     // -t  8  Print after replacestars
					     // -t 16  Print after setshapes
  IF argv!9  DO optStrace    := ~optStrace   // STRACE/S  Syntax analyser trace
  IF argv!10 DO optNtrace    := ~optNtrace   // NTRACE/S  Note tracing
  IF argv!11 DO optMtrace    := ~optMtrace   // MTRACE/S  Midi tracing
  IF argv!12 DO optgenmidi   := ~optgenmidi  // MIDI/S    Generate a .mid file
  IF argv!13 DO optplay      := ~play        // PLAY/S    Play the midi directly
  IF argv!14 DO accompany    := ~accompany   // ACC/S     Accompany. synchronize
                                             //           using mic and keyboard
  IF argv!15 DO pitch        := !(argv!16)   // PITCH/N   Change pitch
  IF argv!16 DO graphdata    := ~graphdata   // GD/S      Generate graph data
  IF argv!17 DO waiting      := ~waiting     // WAIT/S    Wait before playing
  IF argv!18 DO calibrating  := ~calibrating // CALIB/S   Calibrate Mic delay
  IF argv!19 DO optfaca := !argv!20          // FACA=-fa/N
  IF argv!20 DO { LET n = !argv!21           // DEBUG/N   Complement debug 0 to 9
                  IF 0<=n<=9 DO debugv!n := ~debugv!n
                }

  start_msecs := 0
  end_msecs   := maxint/2
  
  IF accompany DO optplay := TRUE

  IF optfaca <   0 DO optfaca :=   0
  IF optfaca > 100 DO optfaca := 100
  shapefaca := (FLOAT optfaca) / (FLOAT 100)
  shapefacb := (FLOAT 1) - shapefaca
  
  // The filename root must not contain any dots.
  FOR i = 1 TO filenameroot%0 IF filenameroot%i='.' DO
  { writef("*nThe FROM filename must not contain any dots*n")
    GOTO fin
  }

  concatext(filenameroot, ".mus", fromfilename)
  concatext(filenameroot, ".mid", midifilename)

  lineno := 0

  chbuf   := b
  chbufln := bln
  FOR i = 0 TO 63 DO chbuf!i, chbufln!i := 0, 0
  chcount := 0

  strv := s1         // Short term string buffer

  baseday := -1 // This will be initialised by first call of getrealmsecs
  chanvol := -1
  variablevol := FALSE

  killco := createco(deleteco, 500)

  errcount, errmax := 0, 5
  fin_p, fin_l := level(), fin
  rec_p, rec_l := fin_p, fin_l

  bg_baseupb := 100_000    // Default work space size for bgpm.
  bg_base := 0             // Base of BGPM workspace

  currqbeat := 0
  startbarno, endbarno := 0, maxint/2  // Range of bars to play
  start_msecs := -1 // not set yet
  end_msecs   := -1
  solochannels := 0  // No soloist(s) yet
  quitting := FALSE
  currtuplet := 0
  
  sourcestream := 0
  getstreams := 0
  tostream := 0
  bgpmco := 0

  // Space for parse tree, shape data, note data, etc.
  blklist, blkp, blkt, blkitem := 0, 0, 0, 0

  // Initialise the freelists of nodes of size 1 to 9
  mk1list, mk2list, mk3list, mk4list, mk5list  := 0, 0, 0, 0, 0
  mk6list, mk7list, mk8list, mk9list, mk10list := 0, 0, 0, 0, 0

  // Room for 100 file names
  sourcefileupb := 100
  sourcenamev := newvec(sourcefileupb)
  UNLESS sourcenamev DO
  { writef("Insufficient space available*n")
    GOTO fin
  }

  FOR i = 0 TO sourcefileupb DO sourcenamev!i := "unknown"   
  sourcefileno  := 1
  sourcenamev!1 := "built-in"

  lineno := (1<<20) + 1 // lineno value of first line of "built-in"
  prevlineno := 0

  // Sourcefile 1 is "built-in" used during initialisation.
  // Sourcefile 2 is always the FROM filename
  // Higher numbers are for GET file names

  msecsbase := -1
  oer, oem, erate := getrealmsecs(), 0, tadjval
  ocr, ocm, crate := oer, oem, erate
  bg_baseupb := 100_000    // BGPM workspace upb
 
  IF bg_baseupb<5000 DO bg_baseupb := 5000
  bg_base := getvec(bg_baseupb)    // Allocate the BGPM workspace
  UNLESS bg_base DO
    fatalerr("Unable to allocate work space (upb = %n)*n", bg_baseupb)

  { LET len = fromfilename%0
    LET str = newvec(len/bytesperword)
    IF str FOR i = 0 TO len DO str%i := fromfilename%i
    sourcefileno := sourcefileno+1
    sourcenamev!sourcefileno := str
  }

  sourcestream := findinput(fromfilename)
  lineno := (sourcefileno<<20) + 1
  // lineno is a packed word 12 bits of file number and 20 bits of line number 
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
    prevlineno := -1

    writef("*nTesting the output of the macrogenerator*n")

    { ch := rch()

      UNLESS lineno=prevlineno DO
      { newline()
        prlineno(lineno); writef(" ")
        prevlineno := lineno
      }
      IF ch<32 SWITCHON ch INTO
      { DEFAULT:      writef("<%n>", ch);          LOOP
        CASE endstreamch:  writef("<eof>", ch);    BREAK
        CASE '*n':         writef("<**n>", ch);    LOOP
        CASE '*t':         writef("<**t>", ch);    LOOP
      }
      
      wrch(ch)
    } REPEAT
    newline()
    GOTO fin
  }

  ch := rch()
 
  // Set the defaults so that the next note will be a
  // quarter note in octave 4 (middle C up to B).
  prevoctave, prevnoteletter := 4, 'f'
  prevnoteqlen, prevdotcount := 1024, 0

  tree := formtree()              // Perform Syntax Analysis
  
//sawritef("Returned from formtree*n")

  // In general all fields in parse tree node whose values
  // are not yet known are set to -1 to indicate that they
  // are not yet initialised.

  // These include all qlen fields except in Note, Rest
  // and Space nodes.
  // The parent fields in Blocks, Tuplet and environment
  // nodes are set to -1.
  // The toqlen field in Tuplet nodes are given their
  // correct values.
  // absq1 and absq1 in environment node are set to -1.
  
  IF optLex GOTO fin

  // At this point the entire score has been parsed and all
  // required Block nodes have been formed and given
  // environment lists containing empty environments for
  // each shape type needed by each block. The parent fields
  // all Block and Tuplet nodes are zero. The qshiftv field
  // of Blocks contain -1 when the Block contains Tempo data.
  // The qshiftv field is otherwise zero. The qlen fields of
  // Note, Notetied, Space and Rest nodes are set correctly
  // but the qlen fields of all other nodes are set to -1.
  
  IF (optTree&#b0001)>0 DO
  { writes("*nTree before calling calcqlen(tree)*n*n")
    prtree(tree, 0, 20)
    newline()
    abort(3981)
  }
//abort(2993)

  // Fill in all the qlen fields and parent fields of Block
  // and Tuplet nodes.
  currtuplet := 0
  currblock  := 0
  currqbeat  := 0
//  writef("About to call calcqlen(tree)*n")
  scoreqlen  := calcqlen(tree)
//  writef("Returned from calcqlen(tree)*n")
  
//writef("scoreqlen=%n*n", scoreqlen)
//abort(2992)

  IF (optTree&#b0010)>0 DO
  { writes("*nTree after calling calcqlen(tree)*n*n")
    prtree(tree, 0, 20)
    newline()
    newline()
    abort(3983)
  }

  IF errcount GOTO fin

  // tree      -> [-, Score, ln, name, conductor, parts, qlen]

  // conductorpart  -> [-, Conductor, ln, block, qlen]
  // conductorblock -> [0, Block, ln, notes, qlen, parent,
  //                       qbeat, envs, qshiftv, tupnode]
  //conductorenvs := h8!conductorblock

  // Note that conductorblock will always point to a Block node even
  // when no shape data occurs in the conductor part.


//  abort(1118)

  // Now call barscan(conductorpart) to create the bar vector.

//  writef("About to call barscan(conductorpart)*n")
  barscan(conductorpart)

  compactsxv(barsxv) // Copy the bar vector into a vector
                     // of just the size required.

  barno2absqv := barsxv!1 // Make the bars vector easily
                          // accessible to barno2absq

  // From now on barno2absq can be used but barno2msecs
  // will not work until after setshapes is called.
  
  start_msecs := -1 // Not set yet
  end_msecs   := -1

  endbarno := maxbarno
  //writef("startbarno=%n endbarno=%n*n", startbarno, endbarno)
  //writef("maxqbeat=%n*n", maxqbeat)
  //writef("maxbarno=%n*n", maxbarno)
  //writef("barsxv  -> [%n,%n]*n", barsxv!0,  barsxv!1)

  //prbartable()
  //abort(12233)
  //tstabsq2barno()

//abort(5255)

  // Find the raw shape data placing in the appropriate shape
  // environments, and update the parent, qbeat and qlen fields
  // in all Block and Tuplet nodes.

  currqbeat    := 0
  currblock    := 0
  currtuplet   := 0
  currpartname := "noname"
  //writef("About to call findrawshapes(tree)*n")
  findrawshapes(tree)
  //abort(15256)

  IF (optTree&#b0100)>0 DO
  { writes("*ntree after calling findrawshapes(tree)*n*n")
    prtree(tree, 0, 20)
    newline()
    newline()
    abort(3984)
  }

//  writef("About to call replacestars(tree)*n")
  currqbeat  := 0
  currtuplet := 0
  currblock  := 0
  replacestars(tree)
//  abort(15257)

  IF (optTree&#b1000)>0 DO
  { writes("*nTree after calling replacestars(tree)*n*n")
    prtree(tree, 0, 20)
    newline()
    newline()
    abort(3985)
  }

//  writef("About to call setshapes(tree)*n")
  currqbeat  := 0
  currtuplet := 0
  currblock  := 0
  setshapes(tree)
//  abort(15258)

  IF (optTree&#b10000)>0 DO
  { writes("*nTree after calling setshapes(tree)*n*n")
    prtree(tree, 0, 20)
    newline()
    newline()
    abort(3986)
  }

  // The score of the composition is now represented by its parse tree.

  // We can now set the start and end times in msecs.
  start_msecs := barno2msecs(startbarno)
  end_msecs   := barno2msecs(endbarno)

  midilist  := 0          // Initialise the linked list of midi statements.
  midiliste := @midilist
  end_msecs := 0

  // Translate the score into a list of midi statements.
//  writef("About to call genmidi(tree)*n")
//  abort(5357)
  currqbeat := 0
  genmidi(tree)
  
  // The score is now held as a list of midi statements which
  // are not yet been sorted.
//writef("*nUnsorted midi data*n*n")
//prmidilist(midilist)

  midilist := mergesort(midilist)
  
  // The midilist is now in sorted order.
//writef("*nSorted midi data*n*n")
//prmidilist(midilist)

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

  IF optplay DO playmidi(midilist) // Play the midi data directly to the device.
                                   // typically /dev/midi1

fin:
//  writef("*nFiles:")
//  FOR i = 1 TO sourcefileno DO
//    writef(" %n:%s", i, sourcenamev!i)
  newline()

  // Return all allocated space.
  
  WHILE veclist DO
  { //writef("fin: freeing veclist vector %n*n", veclist!1)
    freevec(veclist!1)
    veclist := !veclist
  }

  IF killco DO { deleteco(killco); killco := 0 }
  IF soundv DO { freevec(soundv);  soundv := 0 }
  IF bgpmco DO { deleteco(bgpmco); bgpmco := 0 }
  IF sourcestream UNLESS sourcestream=sysin DO endstream(sourcestream)
  IF tostream UNLESS tostream=sysout DO endstream(tostream)
  selectinput(sysin)
  selectoutput(sysout)
  IF bg_base DO { freevec(bg_base); bg_base := 0 }

  // Free all the blocks in the blklist.
  WHILE blklist DO
  { LET blk = blklist
    blklist := !blk
    freevec(blk)
  }
  
  RESULTIS 0
}

AND prbartable() BE
{ // Output the bar table
  LET v = barsxv!1
  LET upb = v -> h1!v, 0
  writef("Bar table, upb=%n", upb)
  FOR i = 1 TO upb DO
  { IF i MOD 10 = 1 DO writef("*n%i3: ", i)
    writef(" %i6", v!i)
  }
  newline()
  //abort(99901)
}

AND tstabsq2barno(q) BE
{ writef("Testing absq2barno(%n)*n", q)
  absq2barno(q-1)
  absq2barno(q)
  absq2barno(q+1)
  abort(1001)
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

  // Cause longjump(bgrec_p, bgrec_l) to behave as if BGPM has just
  // read EOF.
  
  bgrec_p, bgrec_l := level(), reteof

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
reteof:    IF getstreams=0 DO
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

           { LET n = rdbgint()      // Read the argument number.
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
	 //writef("*nrnd: -> %n evalarg(1)=%6.3f FLOAT rno=%6.3f*n",
	 //        rno,
	 //        evalarg(1),
	//	 FLOAT rno)
	 //writef("*nrnd: -> evalarg(1) ** (FLOAT rno) / 1_000_000.0 = %6.3f*n",
	 //        evalarg(1) * (FLOAT rno) / 1_000_000.0)
           // rno is a number in the range -1_000_000 to +1_000_000
	 //  abort(338866)
           bgwrnum(evalarg(1) * (FLOAT rno)  / 1_000_000.0)
           GOTO ret
         }

      CASE s_urnd:     // $urnd!expression;
                       // Return an unsigned random number is
                       //        in specified range
         { LET rno =  randno(1_000_001) - 1
	 //writef("*nurnd: -> %n*n", rno)
           // rno is a number in the range 0 to +1_000_000
           bgwrnum(evalarg(1) * FLOAT rno  / 1_000_000.0)
           GOTO ret
         }
    }
  } REPEAT
}

AND rdbgint() = VALOF
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
  // leading spaces are ignored, numbers must start
  // and end with a digit and can contain at most
  // one decimal point. Monadic operators ', +, -
  // and ~ are allowed. A basic expression can be
  // an expression enclosed in parentheses.
  bg_ch := getargch()

//sawritef("bgbexp: bg_ch=%n*n", bg_ch)
  SWITCHON bg_ch INTO
  { DEFAULT:  bg_error("Bad expression, ch=%c", ch)

    CASE '*s': LOOP // Ignore spaces within expressions

    CASE '0': CASE '1': CASE '2': CASE '3': CASE '4':
    CASE '5': CASE '6': CASE '7': CASE '8': CASE '9':
    // Numbers must start with a digit and may
    // contain at most one decimal point.
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

    CASE '+': RESULTIS   bgexp(2)     // Monadic operators
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
      CASE '|':  IF n<2 DO { LET ai = FIX a        // Less binding than &
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
{ // Read and return an unsigned floating point number from
  // characters read by rdchfn (which is getargch if in the
  // macro generator, or by rdc if not).

  // On entry the first character of the number, which must be
  // a digit, will be in ch. On exit ch will hold the first
  // character after the number.

  // Syntax: [digits] [. digits]

  // where digits is one or more decimal digits. The number
  // must start with a digit.
  // If successful, the result will be the floating point number
  // and result2 is zero. On failure the result is zero
  // and result2 to -1.
  // If the number contains no decimal point the value can be
  // represented as an integer, it is stored as an integer in
  // intval, otherwise intval is set to -1.
  // intval is used when parsing contructs such as
  // Timesig and Bank that contain integers.

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
                    // becomes too large to hold as an integer.

  LET fcount= -1    // This will the number of digits after
                    // the decimal point. It will be set to
                    // zero if a decimal point was not found.

  // After reading all the digits of the number the resulting
  // value will be the significand multiplied by 10^ecount and
  // divided by 10^fcount.
  // The global intval is set to the significand
  //     if fcount=-1 and ecount=0, // No decimal point and
  //                                // significand not overflowed
  //     but is otherwise set to -1.

  LET val = 0 // An integer to hold the significand.
  LET ignoredigits = FALSE // This is set to TRUE when the
                           // significand can no longer
                           // accumulate decimal digits
                           // without overflowing.

//sawritef("rdnum: entered*n")

  UNLESS '0'<=ch<='9' GOTO fail

  // Read the significand
  WHILE '0'<=ch<='9' | ch='.' DO
  {
//sawritef("rdnum: dealing with significand ch='%c'*n", ch)
    SWITCHON ch INTO
    { DEFAULT: BREAK

      CASE '0': CASE '1': CASE '2': CASE '3': CASE '4': 
      CASE '5': CASE '6': CASE '7': CASE '8': CASE '9':
      { LET digval = ch-'0'
        //dcount := dcount+1  // Count of decimal digits
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

//sawritef("rdnum: Exited from significand loop, fcount=%n ecount=%n*n",
//          fcount, ecount)
//	  abort(339911)

  intval := -1
  IF fcount<0 DO
  { fcount := 0
    IF ecount=0 DO intval := val // The integer value, if possible
  }
  // Note: intval is only >=0 if there is decimal point and the
  // significand is small enough to represent as an integer.

  // The true value of the number is
  //    val multiplied by 10^(ecount-fcount)

  // Convert val x 10^(ecount-fcount) to a floating point number of the
  // current BCPL word length.
  val := sys(Sys_flt, fl_mk, val, ecount-fcount)
//sawritef("rdnum: intval=%n return result %13.6f*n", intval, val)
  result2 :=  0  // Successful return
//abort(1234)
  RESULTIS val // The floating point value.

fail:
abort(2345)
  result2 := -1
  RESULTIS 0
}


AND bgwrnum(FLT x) BE TEST x<0
// If the fractional part is zero, the number is represented
// as an integer, otherwise it is represented as an integer
// followed by a decimal point and up to 3 digits with trailing
// zeros removed. If this removes all digits after the decimal
// point the decimal point is also removed.
THEN { bgputch('-')
       bgwrnum(-x)
     }
ELSE { LET FLT frac = sys(Sys_flt, fl_modf, x)
       LET intpart = FIX result2
//sawritef("bgwrnum: x=%13.6f intpart=%n frac=%13.6f*n", x, intpart, frac)
//abort(559966)
       IF frac+0.0005>=1.0 DO intpart := intpart + 1

       wrpn(intpart)
  
       IF frac > 0 DO
       { LET scaledfrac = FIX sys(Sys_flt, fl_floor,
                                  (frac+0.0005) * 1_000)
         LET digits = 0
         FOR i = 1 TO 3 DO
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
  writef("*n*nError in BGPM near "); prlineno(lineno); writef(": ")
  writef(mess, a, b, c)
  newline()
  errcount := errcount+1
  IF errcount>500 DO
  { writef("Too many errors*n")
    cowait(-2) // Indicate a fatal error
  }
  IF bg_f DO
  { writes("Incomplete calls:*n  ")
    prcalls(3, bg_f, bg_h, bg_s)
    newline()
  }
  wrs("Active macro calls:*n"); btrace(bg_p, 3)
  //wrs("*nEnvironment:*n");  wrenv(bg_e, 20)
  //wrs("######### End of error message*n")
  wrc('*n')

  errcount := errcount+1
  //IF errcount >= errmax DO fatalerr(lineno, "*nToo many errors")
  
  selectoutput(out)
  longjump(bgrec_p, bgrec_l) // Assume EOF has just been read.
}

AND prcalls(n, f, h, s) BE IF f DO
  TEST n=0
  THEN wrs(" ...")
  ELSE { prcalls(n-1, !f, f!1, f-1)
         !h := s-h
         wrcall(f+5, s)
         //newline()
       }

AND btrace(p, n) BE
{ writes("  ")
  IF n=0 DO wrs(" ...*n")
  IF p=0 | n=0 RETURN
  wrcall(p+5, p!4); wrc(c_apply); wrc('*n')
  p, n := !p, n-1
} REPEAT

AND wrcall(a, b) BE
{ LET sep = c_call
  LET lno = a!1
  //LET filename = sourcenamev!(lno>>20)
  //LET ln = lno & #xFFFFF
  //prlineno(lno)
  //writef("   ")
 
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

AND mk10(a, b, c, d, e, f, g, h, i, j) = VALOF
//{ LET p = newvec(8) // Demonstration bug
{ LET p = newvec(9)
  p!0, p!1, p!2, p!3, p!4, p!5, p!6, p!7, p!8, p!9 := a, b, c, d, e, f, g, h, i, j
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
AND unmk10(p) BE { !p := mk10list; mk10list := p }
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
  //writef("lex() => %s*n", opstr(token))
  //abort(1000)
}

AND lex1() BE
{ LET neg = FALSE

  // ch holds the first character of the token
  // and lineno holds its lineno value.
  // This function sets token to be the next lexical token
  // and tokln to be the lineno value of its first character.

  // Note that tokln is updated after white space characters
  // and comments, so when lex returns it will certainly hold
  // the lineno value of the first character of the token.

sw:

  skipwhitespace()
//writef("lex: "); prlineno(lineno); writef(": ch=%n '%c'*n", ch, ch)
//abort(1000)
  
  tokln := lineno
  SWITCHON ch INTO
  { DEFAULT:
      UNLESS ch=endstreamch DO
      { LET badch = ch
        ch := '*s'
        synerr("Illegal character %x2 '%c'", badch, badch)
      }
      token := s_eof
      RETURN

    CASE '-':
      neg := TRUE
    CASE '+':
      ch := rch()
      UNLESS '0'<=ch<='9' DO synerr("Bad number")

    CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
    CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
      fnumval := rdnum(rch)    // A floating point value.
      IF neg DO fnumval, intval := -fnumval, -1
      // Note: intval is the integer value of the number if it is
      // non negative, contains no decimal point and can be
      // represented as an integer. It is otherwise -1.
      token := intval>=0 -> s_int, s_fnum
      RETURN

    CASE 'a':CASE 'b':CASE 'c':CASE 'd':CASE 'e':CASE 'f':CASE 'g':
      // All notes start with a to g
//sawritef("lex: case 'a'-'g': reached*n")
      token      := s_note       // May change to s_notetied
      noteletter := ch

      ch := rch()
      notesharps := rdsharps()   // = 0, 1, 2, -1 or -2
		
      reloctave  :=  0           // Octaves up
      WHILE ch='*'' | ch=',' DO
      { // octaves up or down
        TEST ch='*''
        THEN reloctave := reloctave+1 // One octave up
        ELSE reloctave := reloctave-1 // One octave down
        ch := rch()
      }

      noteqlen := rdnotelength() // Nominal note length or -1
//writef("noteqlen=%n*n", noteqlen)
      dotcount := rddots()
//writef("dotcount=%n*n", dotcount)

      IF ch='~' DO
      { token := s_notetied
        ch := rch()
      }

      // token = s_note or s_notetied
      // noteletter = 'a' .. 'g'
      // notesharps = -2, -1, 0, 1, 2
      // reloctave = -9,..., 0,..., 9   an integer
      // noteqlen = Nominal note length or -1
      // dotcount = 0, 1, 2,...
      RETURN

    CASE 'r':
    CASE 's':
      token := ch='s' -> s_space, s_rest
      ch := rch()
      noteqlen := rdnotelength() // The nominal length or -1
      dotcount := rddots()
      RETURN

    CASE 'z':
      token := s_null         // A zero length space
      ch := rch()
      RETURN

    CASE '\':
      ch := rch()    // Reserved words, eg \vol gives s_vol
      token := lookupword(rdtag())
      IF token=s_word DO synerr("Unknown keyword \%s", charv)
      RETURN
 
    CASE '[': token := s_lsquare;   ch := rch(); RETURN
    CASE ']': token := s_rsquare;   ch := rch(); RETURN
    CASE '(': token := s_lparen;    ch := rch(); RETURN
    CASE ')': token := s_rparen;    ch := rch(); RETURN 
    CASE '{': token := s_lcurly;    ch := rch(); RETURN
    CASE '}': token := s_rcurly;    ch := rch(); RETURN
    
    CASE ':': // eg :s4. :sq1536 :256
              // ie :s<int><dots> :sq<int><dots> :<int>dots>
	      // It sets sfac to the scaling length for shape
	      // data. The effect is to multiply shape values
	      // by sfac.
    { LET k = 0
      LET qlen = 1024 // The default qlen for colon constructs.
      UNLESS ch='s' | '0'<=ch<='9' DO
        synerr("Colon must be followed by 's' or a digit")
      TEST ch='s'
      THEN { ch := rch()
             IF '0' <= ch <= '9' DO qlen := n2qlen(rdint())
           }
      ELSE { qlen := rdint()
           }
      k := sfac
      WHILE ch='.' DO
      { ch := rch()
        k := k/2
        sfac := sfac + k
      }
      sfac := FLOAT qlen / 1024.0
      token := s_colon
      //writef("lex: Returns Colon sfac=%5.2f*n", sfac)
      RETURN
    }

    CASE '**':  // * is used in shapes
      ch := rch()
      token := s_star
      RETURN

    CASE '|':
      ch := rch()
      IF ch='|' DO { token := s_doublebar; ch := rch(); RETURN }
      token := s_barline
      RETURN
 
    CASE '"':
    { LET len = 0
      ch := rch()
 
      UNTIL ch='"' DO
      { IF len>=255 DO synerr("Bad string constant")
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
}

AND skipwhitespace() BE
{ SWITCHON ch INTO
  { DEFAULT:
      RETURN

    CASE '*p': CASE '*n':
    CASE '*c': CASE '*t': CASE '*s':
      ch := rch()
      tokln := lineno
      LOOP

    CASE '/':
      // This must start a comment and so the next character
      // must be '/' or '**'.
      ch := rch()
      
      IF ch='/' DO
      { // Skip over a // comment
        ch := rch() REPEATUNTIL ch='*n' | ch=endstreamch
        tokln := lineno
        LOOP // Treat as white space
      }

      IF ch='**' DO
      { // Skip over a /* comment
        LET depth = 1

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
        LOOP  // Treat the comment ad a space
      }

      synerr("Bad comment")
      RETURN
  }
} REPEAT


AND rdnotelength() = VALOF TEST ch='q'
THEN { // The nominal length is given explicitly in qbeats, eg q1024
       ch := rch()
       RESULTIS rdint() // Length in qbeats
     }
ELSE { IF '0'<=ch<='9' RESULTIS n2qlen(rdint()) // eg 0 to 9
       // Not starting with q or a digit
       RESULTIS -1 // No note length given
     }

AND rdint() = VALOF
{ // This reads an unsigned integer
  LET n = 0
  WHILE '0'<= ch <= '9' DO
  { n := 10*n + ch - '0'
    ch := rch()
  }
  RESULTIS n
}

AND rdsharps() = VALOF
{ IF ch='i' DO                // sharp or double sharp
  { ch := rch()
    UNLESS ch='s' DO synerr("Bad note")
    ch := rch()
    UNLESS ch='i' RESULTIS 1  // One sharp

    ch := rch()
    UNLESS ch='s' DO synerr("Bad note")
    ch := rch()
    RESULTIS 2                // A double sharp
  }
  
  IF ch='e' DO                // flat or double double
  { ch := rch()
    UNLESS ch='s' DO synerr("Bad note")
    ch := rch()
    UNLESS ch='e' RESULTIS -1 // One flat

    ch := rch()
    UNLESS ch='s' DO synerr("Bad note")
    ch := rch()
    RESULTIS -2               // A double flat
  }
  
  RESULTIS 0                  // No sharps or flats
}

AND rddots() = VALOF
{ LET count = 0
  WHILE ch='.' DO
  { count := count+1
    ch := rch()
  }
  RESULTIS count
}
		
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
  WHILE 'a'<=ch<='z' DO
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
  UNLESS charv & nametable DO fatalerr(ln, "More workspace needed")
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
//newline()
//abort(116677)
    SWITCHON token INTO
    { DEFAULT:
         ENDCASE

      CASE s_string:
         writef(" *"%s*"", stringval)
         ENDCASE

      CASE s_fnum:
         writef(" %9.3f", fnumval)
         ENDCASE

      CASE s_int:
         writef(" %n", intval)
         ENDCASE

      CASE s_note:
      CASE s_notetied:
         writef(" %c", capitalch(noteletter))
         FOR i =  1 TO notesharps       DO wrch('#')
         FOR i = -1 TO notesharps BY -1 DO wrch('b')
         FOR i =  1 TO reloctave        DO wrch('*'')
         FOR i = -1 TO reloctave  BY -1 DO wrch(',')
         // Fall through
      CASE s_rest:
      CASE s_space:
         writef(" qlen=%n", noteqlen)
         writef(" dotcount=%n", dotcount)
         ENDCASE
    }

    IF token=s_eof DO
    { newline()
      RETURN
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

AND prnotelength(qlen) BE SWITCHON qlen INTO
{ DEFAULT:   writef("q%n", qlen);  RETURN

  CASE   64: writef( "64");        RETURN
  CASE  128: writef( "32");        RETURN
  CASE  256: writef( "16");        RETURN
  CASE  512: writef(  "8");        RETURN
  CASE 1024: writef(  "4");        RETURN
  CASE 2048: writef(  "2");        RETURN
  CASE 4096: writef(  "1");        RETURN
  CASE 8192: writef(  "0");        RETURN
}

AND prlineno(ln) BE
  writef("%s[%n]", sourcenamev!fno(ln), lno(ln))

AND fatalerr(ln, mess, a, b, c) BE
{ writes("*nFatal error")
  IF ln DO { writef(" near "); prlineno(ln) }
  writes(": "); writef(mess, a, b, c)
  writes("*nCompilation aborted*n")
  longjump(fin_p, fin_l)
}
 
AND fatalsynerr(mess, a) BE
{ writef("*nFatal error near "); prlineno(lineno); writes(": ")
  writef(mess, a)
  writef("*nRecent text:*n")
  wrchbuf()
  errcount := errcount+1
  writes("Compilation aborted*n")
  longjump(fin_p, fin_l)
}

AND synerr(mess, a, b, c) BE
{ writef("*nSyntax error near "); prlineno(lineno); writes(": ")
  writef(mess, a, b, c)
newline(); abort(98765)
  wrchbuf()
  // Skip the rest of the input line 
  UNTIL ch='*n' | ch=endstreamch DO ch := rch()
  lex()
  errcount := errcount+1
//  IF errcount >= errmax DO fatalerr(0, "Too many errors")
  longjump(rec_p, rec_l)
}

AND trerr(mess, a, b, c, d, e, f) BE
{ // currln will hold the file/line number of the current tree node.
  writef("*nError")
  newline()
  IF currpartname DO writef(" in %s", currpartname)
  newline()
  writef(" near "); prlineno(currln)
  newline()
  writef(mess, a, b, c, d, e, f)
  newline()
  abort(999)
  errcount := errcount+1
//  IF errcount >= errmax DO fatalerr(0, "Too many errors")
}

LET checkfor(tok, mess, a, b) BE
{ UNLESS token=tok DO synerr(mess, a, b)
  lex()
}
 
AND rdscore() = VALOF
{ // This reads a score consisting of a conductor part and set of
  // instrumental parts and solos. It returns
  // score   -> [-, Score, ln, name, conductor, parts, qlen]
  // or      =  0  If an error
  // where

  // conductor -> [0, Conductor, ln, block, qlen]

  // parts     -> [0, Parts, ln, partlist, qlen]
  
  // partlist   =  0
  //           -> [partlist, Part, ln, block, qlen]
  //           -> [partlist, Solo, ln, block, qlen]

  // block     -> [-, Block,ln,notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
  //           -> [-, Notes, ln, notelist, qlen]
  // notelist is a list of note items linked by the h1 field.

  // envs      -> [0. Envs, ln, envlist]
  // envlist   =  0
  //           -> [envlist, op, ln, parent, upb, v, absq1, absq2]
  // where op is eq Tempoenv
  
  // All environments belonging to a block relate to the same region
  // from absq1 to absq2.
  
  // A block is just a Notes node if it contains no shape data,
  // except the block in the Conductor is always a Block node even
  // it no shape data is present. A Notes node contains note items
  // linked by the h1 field.

  LET oldp, oldl = rec_p, rec_l // The recovery point used by synerr.

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
//abort(12333)
  conductorpart  := 0    // To check that there is exactly one conductor.
  conductorblock := 0
  conductorenvs  := 0
  conductorflag  := -1 // Nor reading the conductor part nor a solo or part
  
  // Read:   \score scorename [ conductor and parts ]
//writef("token=%s*n", opstr(token))

  UNLESS token = s_score DO
  { fatalsynerr("\score expected")
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

      CASE s_conductor:
      // conductor -> [0, Conductor, ln, block, qlen]
      // block   -> [-, Block,ln,notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
      // or      -> [-, Notes, ln, notelist, qlen]  if no shape data.
      // envs    -> [0, Envs, ln, envlist]
      // envlist -> [envlist, op, ln, parent, upb, v, absq1, absq2]
      // or      =  0
      //            op is eg Vol or Tempo

      // But for the Conductor the block is always a Block node
      // even if there is no shape data and envs is zero.
	
        IF conductorpart DO
	  synerr("Only one conductor is allowed, conductorpart=%n*n",
	         conductorpart)

        lex()

        conductorflag := 1 // Reading the outer Block in the conductor part
	// Read a list of notes enclosed in parentheses and
	// convert the resulting Notes node into a Block if
	// any shape data was found.
	checkfor(s_lparen, "'(' expected at the start of the conductor part")
        block := rdblock(rdnotes)
	checkfor(s_rparen, "')' expected at the end of the conductor part")

        // Now force the block in the conductor to be a Block node
	// even when no shape data was found.
        // block -> [-, Block,ln,notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
	UNLESS h2!block=s_block DO
	{ // Create a Block for the conductor when no shape data was found.
	  block := mk10(0, s_block,
                           ln,
                           block,
		 	   -1,      // qlen
			   -1,      // parent
			   -1,      // qbeat
			   0,       // envs
                           0,       // qshiftv will be -1 if tempo data found
                           -1)      // tupnode
          IF optStrace DO
            writef("%i6 -> [%n, %s, %n:%n, %n, %n, %n, %n, %n, %n, %n]*n",
                    block,
	            h1!block,
                    opstr(h2!block),
                    fno(h3!block), lno(h3!block),
                    h4!block,         // The body
                    h5!block,         // qlen
                    h6!block,         // parent
                    h7!block,         // qbeat
                    h8!block,         // envs
                    h9!block,         // qshiftv
                    h10!block)        // tupnode
//abort(5212)
	}

        // Remember the conductor's Block and envs
        conductorblock := block
	conductorenvs  := h8!block 

        // Create and remember the Conductor's part
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
                 conductorblock,         // Body of the conductor
                 h5!conductorpart)       // qlen (=-1)

	//writef("conductorpart=%n conductorblock=%n conductorenvs=%n*n",
        //      conductorpart, conductorblock, conductorenvs)
//	abort(11882)

        LOOP

      CASE s_part:      // [-, Part, ln, block, qlen]
      CASE s_solo:      // [-, Solo, ln, block, qlen]
         UNLESS conductorblock DO
	   synerr("The conductor must occur before any part or solo")
        // block   -> [-, Block,ln,notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
	// or      -> [-, Notes, ln, notelist, qlen]  if no shape data.
	// envs    -> [0, Envs, ln, envlist]
	// envlist -> [envlist, op, ln, parent, upb, v, absq1, absq2]
	//                      op is eg s_volenv
	// or      =  0
	  
        lex()

        conductorflag := 0 // Not in the conductor part
	// Read a list of notes enclosed in parentheses and
	// convert the resulting Notes node into a Block if
	// any shape data was found.
	checkfor(s_lparen, "'(' expected at the start of a part or solo")
        block := rdblock(rdnotes)
	checkfor(s_rparen, "')' expected at the end of a part or solo")

        part := mk5(0, op, // Convert it to a Part or Solo node
                       ln,
                       block,
                       -1)     // qlen
        IF optStrace DO
           writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
                   part,
	           h1!part,
                   opstr(op),
                   fno(ln), lno(ln),
                   h4!part,
		   h5!part)
	h1!partliste := part  // Append the part or solo to
	                      // the end of the part list
        partliste := part
//abort(7778)
	LOOP
    }
//abort(8812)
  } REPEAT

  // Check for the final ']' of the score.
  checkfor(s_rsquare, "']' expected, token=%s", opstr(token))

  UNLESS conductorpart DO fatalsynerr("The conductor part is required") 
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
  // Check that the current token is a number (s_fnum) and
  // return its (floating point) value, setting token to
  // the next lexical token.
  LET FLT a = fnumval
  checkfor(s_fnum, "Number expected")
  RESULTIS a
}

AND rdinteger() = VALOF
{ // This is only used in control, timesig, bank and patch statements.

  // Check the current token is an integer (s_int) and return its
  // integer value.
  LET res = intval
  UNLESS token = s_int DO
    synerr("An integer is expected here")
  lex()
  RESULTIS res
}

AND n2qlen(n) = VALOF
{ // Convert a note length number to a length in qbeats.
  SWITCHON n INTO
  { DEFAULT: writef("Invalid note length number %n*n", n)
             RESULTIS 1024
    CASE  0: RESULTIS 8196
    CASE  1: RESULTIS 4096
    CASE  2: RESULTIS 2048
    CASE  4: RESULTIS 1024
    CASE  8: RESULTIS  512
    CASE 16: RESULTIS  256
    CASE 32: RESULTIS  128
    CASE 64: RESULTIS   64
  }
}

AND note2qlen(q, prevqlen, dotcount) = VALOF
{ // Calculate the qlen of a note, space or rest.
  // q is the nominal length
  // preqlen is the length of the previous note, space or rest
  // dotcount is the number of dots.
  LET qlen = q

  IF q<0 DO qlen := prevqlen

  { LET q = qlen
    FOR i = 1 TO dotcount DO
    { q := q/2
      qlen := qlen + q
    }
  }
//writef("notqlen=%n*n", qlen)
  RESULTIS qlen
}

AND rdnoteprim() = VALOF
{ // Read in a note item up to its end or a dyadic operator
  // such as \vol or \tuplet
  LET op, ln = token, tokln
  LET a, b = 0, 0

//writef("rdnoteprim: op=%s  <%n/%n>*n", opstr(op), fno(ln), lno(ln))
//abort(558899)
  SWITCHON op INTO
  { DEFAULT:
      RESULTIS 0

    CASE s_fnum:
      synerr("A number in a note sequence must be an unsigned integer")
      
    CASE s_int: // An octave number
      //writef("rdnoteprim: Int %n*n", intval)
      UNLESS 0<=intval<=9 DO
        synerr("Bad octave number %n", intval)
      prevoctave := intval
      prevnoteletter := 'f' // So C to B are all in the same octave 
      lex()
      RESULTIS rdnoteprim()

    CASE s_lparen:
      // Form [-, Notes, ln, notelist, qlen]
      lex()
      a := rdnotes()
      checkfor(s_rparen, "Syntax error in ( ... ) construct")
      RESULTIS a

    CASE s_lcurly: // Return a Block or Notes node.
      // [-, Block, ln, notes, qlen,parent,qbeat,envs,qshiftv,tupnode]
      // [-, Notes, ln, notelist, qlen]
      lex()
      //writef("lcurly: about to call rdblock(rdnotes)*n")
      //abort(999123)
      a := rdblock(rdnotes)
      checkfor(s_rcurly, "Syntax error in { ... } construct")
      //abort(999124)
      RESULTIS a

    CASE s_lsquare:
      // [-, Par, ln, parlist, qlen]
      // parlist -> [parlist, Block, ln, notes, qlen, parent,
      //                      qbeat, envs, qshiftv, tupnode]
      // or      -> [parlist, Notes, ln, notelist, qlen]
      // or      =  0
      lex()
      //abort(40004)
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

    CASE s_note:     // [-, Note,     ln, <letter:sharps:dots:n>, qlen]
    CASE s_notetied: // [-, Notetied, ln, <letter:sharps:dots:n>, qlen]
    { // lex returns Note or Notetied with the following associated
      // variables set:
      //   noteletter        'a' to 'g'
      //   notesharps        -2 to +2
      //   reloctave          positive or negative
      //   noteqlen           -1 means not specified but otherwise >0
      //   dotcount           The number of dots
      //
      // Calculate the note number
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

      IF noteqlen<0 DO // If not given use the previous note length
        noteqlen := prevnoteqlen

      prevnoteqlen := noteqlen
      
      a := mk5(0, op, ln,
               noteletter<<24 |
	       (notesharps&255)<<16 |
	       (dotcount&255)<<8  |
	       notenumber,
               noteqlen)    // Nominal q length
      IF optStrace DO
        writef("%i6 -> [%n, %s, %n:%n, <%c:%n:%n:%n>, %n]*n",
                a,
		h1!a,
                opstr(h2!a),
                fno(ln), lno(ln),
                noteletter, notesharps, dotcount, notenumber,
                h5!a)  // qlen

      lex()
      RESULTIS a
    }

    CASE s_rest:  // [-, Rest,  ln, qlen]
    CASE s_space: // [-, Space, ln, qlen]
      IF noteqlen<0 DO // If not given use the previous note length
        noteqlen := prevnoteqlen

      prevnoteqlen := noteqlen
      
      a := mk5(0, op, ln, noteqlen, dotcount)
      IF optStrace DO
        writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
                a,
		h1!a,
                opstr(h2!a),
                fno(ln), lno(ln),
		h4!a,
                h5!a)
      lex()
      RESULTIS a

    CASE s_null: // [-, Null, ln]
      a := mk4(0, op, ln)
      IF optStrace DO
        writef("%i6 -> [%n, Null, %n:%n]*n",
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
    { LET plet, poct = prevnoteletter, prevoctave
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
      prevnoteletter, prevoctave := plet, poct
      checkfor(s_rparen, "')' expected")
      RESULTIS a
    }

    CASE s_transposition: // [-, Transposition, ln, semitones-up]
    { LET plet, poct = prevnoteletter, prevoctave
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
      prevnoteletter, prevoctave := plet, poct
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
      a := mk4(0, op, ln, rdshape())
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

AND rdshape(op) = VALOF
// This reads the shape data that occurs as the right hand operand of
// any shape operator such as \vol or \tempo. It returns a Shape node
// containing the list of shape items.

// This function reads a single shape item or a list of shape items enclosed
// in parentheses. It return a pointer -> [-,Shape,ln,shapelist,qlen]
// where shapelist is a list of the items chained together by their h1
// fields. A shape value is one of the following

//          [-, Fnum,  ln,  val]    A floating point shape value
//          [-, Star,  ln]
//          [-, Space, ln, qlen]
//          [-, Space, ln, qlen]
//          [-, Null,  ln]
 
// Shape values are scaled by sfac. So, for instance,
// if sfac=0.5 (corresponding to an eighth note or quaver), a tempo
// value of 120 would be halved giving a rate of 60 quarter notes
// per minute. sfac can be set within a shape sequence by
// items such as :s8 or :512. At the start of a shape sfac has
// the default value 1.0. Tempo values are stored in tempo
// environments after they are scaled and they represent a rate of
// quarter notes (or 1024 qbeats) per minute.
// The main purpose of scaling is to allow, for instance, dotted
// quaver = 120 to be specified by \tempo(:s8. 120). Scaling is
// typically not used with any other shape types.

{ LET list = 0            // For the chain of note items
  LET liste = @list
  LET firsttoken = token  // To remember whether it was Lparen.
  LET shapelistln = tokln
  LET fno1 = tokln>>20    // The lineno of the first token of the shape
  LET lno1 = tokln & #xFFFFF
  LET item = 0
  noteqlen := 1024        // The default nominal q length
  dotcount := 0           // default number of dots
  sfac := 1.0             // The default scaling factor.

  TEST token=s_lparen
  THEN { lex()
         { // Read a sequence of shape items
           item := rdshapeitem()
           UNLESS item DO
           { synerr("Shape item or ')' expected")
             RESULTIS 0
           }
           h1!liste := item
           liste := item
         } REPEATUNTIL token=s_rparen
	 lex()
       }
  ELSE { item := rdshapeitem()
	 list := item
         UNLESS item DO
         { synerr("A shape item is expected here")
           RESULTIS 0
         }
       }

  item := mk5(0, s_shape, shapelistln, list, -1)
  IF optStrace DO
    writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
            item,
	    h1!item,
	    opstr(h2!item),
            fno(shapelistln), lno(shapelistln),
            h4!item,
	    h5!item)

  noteqlen := prevnoteqlen
  RESULTIS item
}

AND rdshapeitem() = VALOF
{ // Start of loop to read shape items.
  LET ln = tokln
  LET item = 0
  LET op = token

  SWITCHON op INTO
  { DEFAULT:
      synerr("SYSTEM ERROR in rdshapeitem")
      RESULTIS 0

    CASE s_rest:       // eg r16
    CASE s_space:      // eg s4.
      item := mk5(0, op, tokln, noteqlen, dotcount)
      IF optStrace DO
       writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
              item,
		h1!item,
                opstr(h2!item),
                fno(ln), lno(ln),
                h4!item,
                h5!item)
      lex()
      RESULTIS item

    CASE s_null:      // eg z
      item := mk5(0, op)
      IF optStrace DO
       writef("%i6 -> [%n, %s, %n:%n]*n",
              item,
		h1!item,
                opstr(h2!item),
                fno(ln), lno(ln))
      lex()
      RESULTIS item

    CASE s_colon: // lex has already set the scaling factor sfac.
      lex()
      LOOP

    CASE s_int:  // Shape values are floating point numbers with or
    CASE s_fnum: // without decimal points. The value is held in
                 // fnumval.
      fnumval := fnumval * sfac // Apply the current scaling value.
      item := mk4(0, s_fnum, tokln, fnumval)
      IF optStrace DO
        writef("%i6 -> [%n, %s, %n:%n, %7.3f]*n",
               item,
               h1!item,
               opstr(h2!item),
               fno(ln), lno(ln),
               h4!item)
      lex()
      RESULTIS item

    CASE s_star:
      item := mk3(0, token, tokln)
      // Star items are not scaled by sfac.
      IF optStrace DO
        writef("%i6 -> [%n, %s, %n:%n]*n",
                item,
                h1!item,
                opstr(h2!item),
                fno(ln), lno(ln))
      lex()
      RESULTIS item
  }
} REPEAT

AND rdtupletqlen() = VALOF
{ // This reads the right hand operand of \tuplet
  // Syntactically this operand is either
  // a space or rest item, or
  // a list of space or rest items enclosed in parentheses.
  // The result is the qlen of the operand.

  LET qlen = 0
  LET prevlen = 1024     // Assume the previous length was 1024.
  LET firsttoken = token // To remember whether it was Lparen.
  //writef("rdtupletqlen: token=%s*n", opstr(token))
//abort(662233)
  IF token=s_lparen DO lex()  // Skip over '(' if present.

  WHILE token=s_space | token=s_rest DO
  { TEST noteqlen<0           // No nominal length given
    THEN noteqlen := prevlen  //  use the previous length
    ELSE prevlen := noteqlen  //  or remember this nominal length
    qlen := noteqlen

    { LET len = noteqlen      // Deal with the dots
      WHILE dotcount>0 DO
      { len, dotcount := len/2, dotcount-1
        qlen := qlen + len    // Accumulate the length
      }
    }
    lex()
    UNLESS firsttoken=s_lparen BREAK
  }

  IF firsttoken=s_lparen DO
    checkfor(s_rparen, "Error in a \tuplet shape sequence")

  // Check that qlen is valid.
  UNLESS qlen>0 DO
    synerr("A \tuplet qlen=%n must be greater than zero", qlen)

  RESULTIS qlen
}

AND op2envbit(op) = VALOF SWITCHON op INTO
{ DEFAULT:           RESULTIS 0
  CASE s_tempo:      RESULTIS b_tempo
  CASE s_volmap:     RESULTIS b_volmap

  CASE s_vibrate:    RESULTIS b_vibrate
  CASE s_vibrateadj: RESULTIS b_vibrateadj
  CASE s_vibamp:     RESULTIS b_vibamp
  CASE s_vibampadj:  RESULTIS b_vibampadj
  CASE s_vol:        RESULTIS b_vol
  CASE s_voladj:     RESULTIS b_voladj
  CASE s_legato:     RESULTIS b_legato
  CASE s_legatoadj:  RESULTIS b_legatoadj
  CASE s_delay:      RESULTIS b_delay
  CASE s_delayadj:   RESULTIS b_delayadj
}

AND rdnoteitem() = VALOF
{ // Return the parse tree of a note item or zero if none found.
  // It records in envbits the types of any shape data found.

  LET a = rdnoteprim() // Read a note up to the first dyadic
                       // operator, if any.
  UNLESS a RESULTIS 0

  { // Look for nested tuplet or shape operators.
    LET op = token
    LET ln = tokln          // Lineno of the operator if any.

    SWITCHON op INTO
    { DEFAULT:
        RESULTIS a // Just return the primary since no dyadic
	           // operator was found.

      // The infixed shape operators
      CASE s_vibrate:
      CASE s_vibrateadj:
      CASE s_vibamp:
      CASE s_vibampadj:
      CASE s_vol:
      CASE s_voladj:
      CASE s_volmap:
      CASE s_tempo:
      CASE s_legato:
      CASE s_legatoadj:
      CASE s_delay:
      CASE s_delayadj: // [-, op, ln, notes, shapelist]
        envbits := envbits | op2envbit(op)
	//writef("envbits=%12b*n", envbits)
	UNLESS h2!a=s_block | h2!a=s_notes DO
	{ a := mk5(0, s_notes, h3!a, a, -1)
          // a -> [-, Notes, ln, a, -1]
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
        a := mk5(0, op, ln, a, rdshape(op))
        // a-> [-, op, ln, notes, shape]
        IF optStrace DO
          writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
                  a,
		  h1!a,
		  opstr(op),
		  fno(ln), lno(ln),
                  h4!a,
                  h5!a)
        LOOP

      CASE s_tuplet: // [-, Tuplet, ln, notes, qlen, parent, qbeat, toqlen]
                     // eg (C D E)\tup s4
                     // or (C D E)\tup 1024
                     // or (C D E)\tup(s8. s.) 
        lex()
	// a is the parse tree of the left operand of Tuplet
	// Force it to be a Notes node.
	UNLESS h2!a=s_notes DO
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
        // Read the right hand operand of \tuplet just returning it length.
	// [-, Tuplet, ln, notes, qlen, parent, qbeat, toqlen]
	//writef("about to call rdtupletqlen*n")
	//abort(82266)
        a := mk8(0, s_tuplet, ln, a, -1, 0, 0, rdtupletqlen())
        IF optStrace DO
          writef("%i6 -> [%n, Tuplet, %n:%n, %n, %n, %n, %n, %n]*n",
                  a,
		  h1!a,
                  fno(ln), lno(ln),
                  h4!a,  // notes
                  h5!a,  // qlen    = qlen of notes
                  h6!a,  // parent  = currtuplet
                  h7!a,  // qbeat
                  h8!a)  // toqlen
        LOOP
    }
  } REPEAT

  RESULTIS 0
}

AND rdparlist() = VALOF
// This returns a list of note items linked though the h1 field to
// be used as the body of a Par node. The note items that contain
// shape data are converted into Blocks.
{ LET list = 0
  LET liste = @list
//abort(40002)
  { LET item = rdblock(rdnoteitem)
  //abort(40006)
    UNLESS item RESULTIS list
    !liste := item   // Append the latest note item.
    liste := item
  } REPEAT

  RESULTIS list  // return the list of note items.
}

AND rdblock(rdbodyfn) = VALOF
{ // If rdbodyfn is rdnotes, read a sequence of notes.
  // If rdbodyfn is rdnoteitem, read a note item.
  // If rdbodyfn returned zero the result of rdblock is zero.

  // If a note sequence or a note item was read successfully and
  // if shape data was found, the resulting parse tree is converted
  // to a Block unless it was already a Block and environents are
  // created for each kind of shape found. These environments are
  // initially empty but will be filled with data by a later by a
  // call of findrawshapes.

  // If Tempo data is found the qshiftv field of the Block is set
  // to -1 to indicate that the qshiftv must be allocated and
  // filled with data when the length of the block is known. This
  // is done by setshapes.

  LET ln          = lineno
  LET body        = 0
  LET envs        = 0         // For the Envs environment nodes
  LET qshiftv     = 0
  LET prevenvbits = envbits
  envbits := 0 // Bit pattern to identify which shape types are found.

//writef("rdblock: conductorflag=%n conductorenvs=%n*n",
//        conductorflag, conductorenvs)
//abort(40001)
  body := rdbodyfn()
  //writef("rdblock: after calling rdbodyfn body=%n envbits=%14b*n",
  //        body, envbits)
  IF envbits DO
  { envs := mkenvs(envbits)
    UNLESS (envbits&b_tempo)=0 DO
      qshiftv := -1     // Indicate that Tempo data is present.
  }
  //writef("rdblock: So envs=%n*n", envs)

  envbits := prevenvbits
  
  UNLESS body RESULTIS 0
  
  UNLESS h2!body=s_block | h2!body=s_notes DO
  { body := mk5(0, s_notes, ln, body, -1)
    IF optStrace DO
      writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
              body,
              h1!body,
	      opstr(h2!body),
              fno(h3!body), lno(h3!body),
              h4!body,
              h5!body)
    //abort(1011)
  }

  RESULTIS mkblock(body, envs, qshiftv)
}

AND mkblock(body, envs, qshiftv) = envs=0 -> body, VALOF
{ // Make a Block node because shape data has been found.
  //writef("mkblock: envbits=%32b*n", envbits)
  LET block    = body
  LET ln       = h3!body
  
  UNLESS h2!block=s_block DO
  { // block -> [-, Block,ln,notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
    block := mk10(h1!body,   // link
                  s_block,   // Block
		  ln,        // lineno
		  body,      // body
                  -1,        // qlen
	  	  -1,        // parent
		  -1,        // qbeat
		  envs,      // envs
		  qshiftv,   // qshiftv
		  -1)        // tupnode
    IF optStrace DO
      writef("%i6 -> [%n, %s, %n:%n, %n, %n, %n, %n, %n, %n, %n]*n",
                      block,
	              h1!block,
                      opstr(h2!block),
                      fno(h3!block), lno(h3!block),
                      h4!block,         // The body
                      h5!block,         // qlen
                      h6!block,         // parent
                      h7!block,         // qbeat
                      h8!block,         // envs
                      h9!block,         // qshiftv
		      h10!block)        // tupnode
    //abort(5212)
  }
  // Return either a Block or Notes node.
  RESULTIS block
}

AND mkenvs(envbits) = VALOF
{ // If envbits is nonzero so create the needed environment nodes.
  LET envs = 0
  LET envlist = 0
  WHILE envbits DO
  { LET bit = envbits & -envbits
    LET envtype = VALOF SWITCHON bit INTO
    { DEFAULT:           RESULTIS 0
      CASE b_tempo:      RESULTIS s_tempoenv
      CASE b_volmap:     RESULTIS s_volmapenv

      CASE b_vibrate:    RESULTIS s_vibrateenv
      CASE b_vibrateadj: RESULTIS s_vibrateadjenv
      CASE b_vibamp:     RESULTIS s_vibampenv
      CASE b_vibampadj:  RESULTIS s_vibampadjenv
      CASE b_vol:        RESULTIS s_volenv
      CASE b_voladj:     RESULTIS s_voladjenv
      CASE b_legato:     RESULTIS s_legatoenv
      CASE b_legatoadj:  RESULTIS s_legatoadjenv
      CASE b_delay:      RESULTIS s_delayenv
      CASE b_delayadj:   RESULTIS s_delayadjenv
    }
    //writef("envbits=%32b  bit=%32b*n", envbits, bit)
    envbits := envbits-bit
    
    // Create an empty environment node of the right type
    // but without a parent or qlen yet. These are filled in
    // later by a call of findrawshapes.
    // envlist =  0
    //         -> [envlist, op. ln, parent, upb, v, absq1, absq2]
    envlist := mk8(envlist, envtype, lineno,
                   -1,       // Parent
		   0, 0,     // (upb, v) The self expanding vector
		   -1, -1)   // absq1, absq2

    IF optStrace DO
      writef("%i6 -> [%n, %s, %n:%n, %n, %n, %n, %n, %n]*n",
              envlist,
              h1!envlist,
	      opstr(h2!envlist),
              fno(h3!envlist), lno(h3!envlist),
              h4!envlist,   // parent
              h5!envlist,   // upb
              h6!envlist,   // v
              h7!envlist,   // absq1
              h8!envlist)   // absq2
  }
  //abort(1998)

  IF envlist DO
  { // The body contained shape data so an Envs node must be created.
  
    envs := mk5(0, s_envs, lineno, envlist, -1)
    IF optStrace DO
        writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
                envs,
		h1!envs,
		opstr(h2!envs),
                fno(h3!envs), lno(h3!envs),
                h4!envs,
                h5!envs)
  }
  RESULTIS envs
}

AND rdnotes() = VALOF
// This returns a Notes node containing a list of note items
// linked though the h1 field. It returns a Notes node containing
// the list. The lparen or lcurly has already been read and it
// does not check for rparen r rcurly before returning.
{ LET ln = tokln
  LET list = 0
  LET notes = 0

  list := rdnotelist()
  
  notes := mk5(0, s_notes, ln, list, -1)

  IF optStrace DO
    writef("%i6 -> [%n, %s, %n:%n, %n, %n]*n",
            notes,
            h1!notes,
	    opstr(h2!notes),
            fno(ln), lno(ln),
            h4!notes,
            h5!notes)

  RESULTIS notes  // return the Notes node
}

AND rdnotelist() = VALOF
// This returns a list of note items linked though the h1 field.
// This is used to read the list of notes enclosed in parentheses
// curly brackets.
{ LET ln = tokln
  LET list = 0
  LET liste = @list

  // Setup new recovery point
  LET oldp, oldl = rec_p, rec_l
  rec_p, rec_l := level(), sw

  // If synerr is called, after the error message has been generated
  // it resumes by attempting to append more note items to the list.

sw:
  { LET a = rdnoteitem()
    UNLESS a BREAK
    !liste := a   // Append the latest note item.
    liste := a
  } REPEAT

  rec_p, rec_l := oldp, oldl

  RESULTIS list  // return the list of note items.
}

LET fno(ln) = ln>>20
AND lno(ln) = ln & #xFFFFF

LET opstr(op) = VALOF SWITCHON op INTO
{ DEFAULT:        sawritef("opstr: System error, op %n*n", op)
                  abort(999)
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
  CASE s_int:                 RESULTIS "Int"
  CASE s_keysig:              RESULTIS "Keysig"
  CASE s_lcurly:              RESULTIS "Lcurly"
  CASE s_legato:              RESULTIS "Legato"
  CASE s_legatoadj:           RESULTIS "Legatoadj"
  CASE s_legatoadjenv:        RESULTIS "Legatoadjenv"
  CASE s_legatoenv:           RESULTIS "Legatoenv"
  CASE s_notes:               RESULTIS "Notes"
  CASE s_shape:               RESULTIS "Shape"
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
  CASE s_fnum:                 RESULTIS "Num"
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
  CASE s_tempoenv:            RESULTIS "Tempoenv"
  CASE s_tenorclef:           RESULTIS "Tenorclef"
  CASE s_timesig:             RESULTIS "Timesig"
  CASE s_title:               RESULTIS "Title"
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
  CASE s_volmapenv:           RESULTIS "Volmapenv"
}

AND prnote(letter, sharps, dots, note, qlen) BE
{ // If qbeats<0 just output the note letter possibly followed by # or b
  // otherwise output the octave number, note letter, sharps and flats, and
  // the length in qbeats.
  LET count = 0
  dots   := dots&255       // Dots
  sharps := sharps&255     // Sharps
  // Sign extend sharps
  IF sharps>128 DO sharps := sharps-256 // Flats

  // Cause 4Ces (note 59) to print as 4Cb not 3Cb
  // Cause 3Bis (note 60) to print as 3B# not 4B#

  IF qlen>=0 DO
  { IF note DO writef("%n=", note)
    TEST note>=12   // Write the octave number
    THEN writef("%n", (note-sharps)/12-1)
    ELSE writef("-")
  }
  wrch(letter+'A'-'a')
  FOR i = 1 TO sharps  DO { wrch('#'); count := count+1 }
  FOR i = sharps TO -1 DO { wrch('b'); count := count+1 }
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

    CASE s_fnum:      // [-, Num, ln, val]
                     writef("%t8 %9.3f   ", opname, a1); prlineno(ln); RETURN

    CASE s_star:     // [-, Star, ln]
                     writef("%t8   ", opname); prlineno(ln); RETURN

    CASE s_note:     // [-, Note,     ln, <letter:sharps:dots:note>, qlen]
    CASE s_notetied: // [-, Notetied, ln, <letter:sharps:dots:note>, qlen]
    { LET letter =   h4!t>>24
      LET sharps =  (h4!t>>16) & 255
      LET dots   =  (h4!t>>8 ) & 255
      LET note   =   h4!t & 255      // MIDI number
      LET qlen   =   h5!t            // Note qlen of a quarter note = 1024
      writef("%t8 ", opname)
      prnote(letter, sharps, dots, note, qlen)
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
      // [-, Keysig, ln, [0, Note, ln, <letter:sharps:dots:noteno>, mode]]
      writef("Keysig (")
      prnote(h4!a1>>24, (h4!a1>>16) & 255, 0, -1)
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
    CASE s_par:       // [-, Par,     ln, notelist,  qlen]
    CASE s_shape:     // [-, Shape,   ln, shapelist, qlen]
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

    CASE s_part:      // [-, Part, ln, block, qlen]
    CASE s_solo:      // [-, Solo, ln, block, qlen]
      writef("%t7 qlen=%n ", opname, h5!t)
      writef("   ")
      prlineno(ln)
      v!n := "  "
      prnltree(h4!t, v, n, d)
      RETURN

    CASE s_block:
    // [-, Block, ln, notes, qlen, parent, qbeat, envs, qshiftv, tupnode]
      // envlist is the list of environments belonging to this block.
      writef("%t7 qlen=%n parent=%n qbeat=%n envs=%n qshiftv=%n tupnode=%n ",
              opname, h5!t, h6!t, h7!t, h8!t, h9!t, h10!t)
      prlineno(ln)
      v!n := "! "
      prnltree(h4!t, v, n, d) // notes
      v!n := "  "
      prnltree(h8!t, v, n, d) // envs
      RETURN

    CASE s_score:     // [-, Score, ln, name, conductor, parts, qlen]
      writef("%s *"%s*" qlen=%n   ", opname, h4!t, h7!t)
      prlineno(ln); newline()
      //abort(99999)
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
    CASE s_vibrateadj: // [-, Vibrateadj, ln, notes, shape]
    CASE s_vibampadj:  // [-, Vibampadj,  ln, notes, shape]
    CASE s_voladj:     // [-, Voladj,     ln, notes, shape]
    CASE s_legatoadj:  // [-, Legatoadj,  ln, notes, shape]
    CASE s_delayadj:   // [-, Delayadj,   ln, notes, shape]

    CASE s_vibrate:    // [-, Vibrate,    ln, notes, shape]
    CASE s_vibamp:     // [-, vibadj,     ln, notes, shape]
    CASE s_vol:        // [-, Vol,        ln, notes, shape]
    CASE s_volmap:     // [-, Volmap,     ln, notes, shape]
    CASE s_tempo:      // [-, Tempo,      ln, notes, shape]
    CASE s_legato:     // [-, Legato,     ln, notes, shape]
    CASE s_delay:      // [-, Delay,      ln, notes, shape]

      writef("%s   ", opname)
      prlineno(ln); newline()
      FOR j = 0 TO n-1 DO writes( v!j )
      writes("**-")
      v!n := "! "
      prtree(h4!t, n+1, d)
      newline()
      FOR j = 0 TO n-1 DO writes( v!j )
      writes("**-")
      v!n := "  "
      prtree(h5!t, n+1, d)
      RETURN       

    CASE s_tempoenv:      // [-, Tempoenv,      ln,parent,upb,v,qbeat1,qbeat2]
    CASE s_volmapenv:     // [-, Volmapenv,     ln,parent,upb,v,qbeat1,qbeat2]

    CASE s_delayenv:      // [-, Delayenv,      ln,parent,upb,v,qbeat1,qbeat2]
    CASE s_legatoenv:     // [-, Legatoenv,     ln,parent,upb,v,qbeat1,qbeat2]
    CASE s_vibampenv:     // [-, vibadjenv,     ln,parent,upb,v,qbeat1,qbeat2]
    CASE s_vibrateenv:    // [-, Vibrateenv,    ln,parent,upb,v,qbeat1,qbeat2]
    CASE s_volenv:        // [-, Volenv,        ln,parent,upb,v,qbeat1,qbeat2]

    CASE s_delayadjenv:   // [-, Delayadjenv,   ln,parent,upb,v,qbeat1,qbeat2]
    CASE s_legatoadjenv:  // [-, Legatoadjenv,  ln,parent,upb,v,qbeat1,qbeat2]
    CASE s_vibrateadjenv: // [-, Vibrateadjenv, ln,parent,upb,v,qbeat1,qbeat2]
    CASE s_vibampadjenv:  // [-, Vibampadjenv,  ln,parent,upb,v,qbeat1,qbeat2]
    CASE s_voladjenv:     // [-, Voladjenv,     ln,parent,upb,v,qbeat1,qbeat2]
    { LET sv = h6!t
      writef("%s   ", opname)
      prlineno(ln)
      writef("  parent=%n upb=%n v=%n absq1=%n absq2=%n",
                h4!t, h5!t, h6!t, h7!t, h8!t)
      IF sv & sv!0 DO
      { LET upb = sv!0
        FOR i = 1 TO upb-1 BY 2 DO
	{ LET k = i/2
	  IF k MOD 4 = 0 DO newline()
	  writef("   %i5 ", sv!i)
	  TEST sv!(i+1)=starval
	  THEN writef(" star")
	  ELSE writef("%6.2f", sv!(i+1))
	}
	newline()
        FOR j = 0 TO n-1 DO writes( v!j )
      }
      RETURN
    }
    
    CASE s_tuplet: // [-, Tuplet, ln, notes, qlen, parent, qbeat, toqlen]
                   // eg         (4C4 D E)\tup S4
                   // previously S4\tup(4C4 D E)
      writef("%s   qlen=%n parent=%n qbeat=%n toqlen=%n  ",
             opname, h5!t, h6!t, h7!t, h8!t)
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

/*
writef("*nTesting the absq2msecs function*n")
  FOR qb = 0 TO msecsvupb BY 512 DO
  { IF qb MOD 4 = 0 DO writef("*n%i5: ", qb)
  //abort(6111)
    writef(" %8i", absq2msecs(qb))
  }
  newline()
  abort(1238)
*/


LET mkmsecsv(block) = VALOF
{ // block -> [0, Block,ln,notes,qlen,parent,qbeat,envs,qshiftv,tupnode]

  // This function is only called when setshapes(tree) encounters
  // a Block.

  // If conductorflag is 1 the block is the outermost block of the
  // conductor part and so an msecs vector with upb=scoreqlen is
  // created and filled even when the Block contains no Tempo data.
  // Note that msecsv!q will hold the time in msecs for any absolute
  // qbeat q in the range 0 to scoreqlen. If q is not in this range
  // the time will be either zero or msecsv!scoreqlen.
  
  // If conductorflag is zero, a Part or Solo is being processed,
  // and if the block contains tempo data and qlen is its length a
  // vector v is allocated with upb=qlen. It is first filled with
  // msecs times based on the Tempo data given. These values are
  // then replaced by qbeat locations using the assignment
  //
  //     v!q := muldiv(v!q, qlen, maxmsecs)
  //
  // Note that this sets v!0 to zero and v!qlen to qlen so the
  // total performance time for the Block is the same as if no
  // tempo data were given. Within the block regions with high
  // tempo values will be performed faster than those with lower
  // values.

  LET msecs = 0 // This will hold the time in msecs of a qbeat
  LET nsecs = 0 // This will hold the remainder in nsecs
                // Note that 1_000_000 nsecs = 1 msec so
		// nsecs is in the range 0 to 999_999.
  LET qlen   = h5!block  // The qlen of theBlock
  LET parent = h6!block
  LET msv    = getvec(qlen) // Allocate the vector to hold the timings.
  LET FLT f58_593_750 = FLOAT 58_593_750 // Note 58_593_750 = 60_000_000_000/1024
  //abort(1004)
  // msv has bounds 0 to qlen of the block.
  UNLESS h2!block=s_block DO
  { writef("mkmsecsv: Arg=%m should be a block not %s*n",
            block, opstr(h2!block))

    abort(999)
  }
  h9!block := msv // The qshiftv field
  //abort(1005)

  UNLESS msv DO
  { writef("Unable to allocate an msecs vector of size %n*n", qlen)
    abort(999)
    RESULTIS 0                 // Error return from mkmsecsv
  }
  
//  writef("mkmsecsv: block=%n qlen=%n conductoflag=%n*n",
//          block, qlen, conductorflag)
//abort(445566)
  // Fill in the msecs values for all local qbeat locations
  // of the block.

  msv!0 := 0             // zero msecs for local qbeat zero.
  FOR q = 1 TO qlen DO
  { // q is a local qbeat location of the block.
    // 1  sec                   Second
    // 1_000 msec               Milli-second
    // 1_000_000 usec           Micro-second
    // 1_000_000_000 nsec       Nano-second

    // Find the tempo of the previous local qbeat.
    LET FLT tempo = shapeval(s_tempo, tempoenv, q-1)
    // The tempo is in units of Crotchets (quarter notes) per minute,
    // so tempo=60 means 1024 qbeats per second.
    // So 60/tempo is the number of seconds per 1024 qbeats.
    // So 60/(tempo*1024) is the number of seconds per qbeat.
    // So 60*1_000_000_000/(tempo*1024) is the number of nano seconds per qbeat.
    LET nsecsperqbeat = FIX(f58_593_750/tempo)
    // Note 58_593_750 = 60_000_000_000/1024
    
    // Accumulate the time in msecs with the remainder in nsecs
    // Note that nsecs is the number of nsecs after msecs,
    // ie between 0 and 999
    msecs := msecs + nsecsperqbeat  /  1_000_000
    nsecs := nsecs + nsecsperqbeat MOD 1_000_000
    msecs := msecs + nsecs  / 1_000_000
    nsecs := nsecs MOD 1_000_000

    msv!q := msecs // msecs is the time of local qbeat q of the
                   // block since the start of the block.

    // This causes msv!1024 = 1000 msecs  when tempo = 60
    // and         msv!4096 = 4000 msecs

IF FALSE DO
    { writef("q=%i5 secs=%9.3d f58_593_750=%10.1f tempo=%8.3f nsecsperqb=%n*n",
              q, msecs, f58_593_750, tempo, nsecsperqbeat)
      IF q MOD 32 = 0 DO abort(12345)
    }
  }

//abort(3232)
  RESULTIS msv
}

AND msv2qshiftv(qlen, msv) BE IF msv DO
{ // This is only called when processing a part or a solo.
  // msv is the vector held in the qshiftv field of the current Block.
  // block -> [0,Block,ln,notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
  // The upb of msv is qlen, the length of the block. Its elements
  // are times in micro seconds relative to the start of the block.
  // The entries are monotonically increasing msv!0 being zero and
  // and msv!qlen being the total time for the block based on its
  // Tempo data. 

  LET maxmsecs = msv!qlen // Total time for the Block in microsecs.
  
  // The elements of msv are now scaled scaled by multiplying them by
  // qlen/maxmsecs, giving a mapping from the local qbeat locations
  // of a Block to the local locations of the enclosing Block or
  // Tuplet taking account of the Tempo data.

  // Note that in regions of high tempo values the time per qbeat
  // is small whereas low tempo values increases the time per qbeat.
  // Regions of qbeats where the tempo is high map into smaller
  // regions, effectively increasing the rate of execution of such
  // regions. With lower tempo values the effect is the opposite.
  
  // Stars (\verb|*|) other than at the start and end of the block
  // should not be used in the Tempo data since they will not always
  // be treated with complete accuracy.
IF FALSE DO
{ writef("msv2qshiftv: maxmsecs=%n qlen=%n*n", maxmsecs, qlen)
  // Replace the elements of v for parts and solos.
  FOR q = 1 TO qlen DO
  { LET msvq = msv!q
    LET newmsvq = muldiv(msv!q, qlen, maxmsecs)
    IF q MOD 4 = 1 DO newline()
    writef(" %i4**%i3/%i3->%i3", q, msvq, maxmsecs, newmsvq)
    IF q MOD 1024 = 1 DO abort(100765)
    msv!q := newmsvq
  }
  //newline()
}
//abort(778898)
}

AND clearenvironments() BE
{ tempoenv      := 0
  volmapenv     := 0

  delayenv      := 0
  legatoenv     := 0
  vibampenv     := 0
  vibrateenv    := 0
  volenv        := 0

  delayadjenv   := 0
  legatoadjenv  := 0
  vibampadjenv  := 0
  vibrateadjenv := 0
  voladjenv     := 0
}

AND updateenvironments(envs) BE IF envs DO
{ // Update the global environment variables as specified by envs.
  // ie Update the environment variables occurring in envs leaving
  // the others unchanged. The parent (h4) field of each updated
  // environment is also set.
  // Environment variables are used when looking up the values of
  // a shape at a given local qbeat location. Typical call
  // shapeval(s_vol, volenv, q) where q is a local qbeat position.
  // setshapes updates the shape values taking account of shape
  // adjuatments and the adjusted shape values of enclosing blocks.
  // Before setshapes is called shapeval will return shape values
  // based on the raw shape data given in the source file, but
  // after setshapes has been called shapeval will use the modified
  // shape values depending on the adjustment shape data and the
  // effect ef enclosing blocks. Tempo data is exceptional in that
  // its raw values are not modified by setshapes. The vector placed
  // in the qshiftv field of block nodes is thus not affected by
  // whether setshapes has or has not been called.
  
  // envs    =  0
  //         -> [0, Envs, ln, envlist]
  // envlist =  0
  //         -> [envlist, op, ln, parent, upb, v, absq1, absq2]
  
  LET envlist = h4!envs // envlist points to an environment
                        // in the envs list pf environments.
			// The list is linked throught the
			// h1 field.
//writef("updateenvironments: envs=%n envlist=%n*n",
//        envs, envlist)
//abort(776661)
  WHILE envlist DO
  { LET envop   = h2!envlist

//writef("updateenvironments: envlist=%n envop=%s*n",
//        envlist, opstr(envop))
//abort(776662)
    SWITCHON envop INTO
    { DEFAULT:
        writef("updateenvironments: System error envlist=%n envop=%s*n",
	       envlist, opstr(envop))
        abort(999)

	// Note that multiple assignments in BCPL are done strictly
	// from left to right.

      CASE s_tempoenv: 	    h4!envlist, tempoenv      := tempoenv,      envlist
                            ENDCASE
      CASE s_volmapenv:     h4!envlist, volmapenv     := volmapenv,     envlist
                            ENDCASE

      CASE s_delayenv:      h4!envlist, delayenv       := delayenv,     envlist
                            ENDCASE
      CASE s_legatoenv:     h4!envlist, legatoenv     := legatoenv,     envlist
                            ENDCASE
      CASE s_vibampenv:     h4!envlist, vibampenv     := vibampenv,     envlist
                            ENDCASE
      CASE s_vibrateenv:    h4!envlist, vibrateenv    := vibrateenv,    envlist
                            ENDCASE
      CASE s_volenv:        h4!envlist, volenv        := volenv,        envlist
                            ENDCASE

      CASE s_delayadjenv:   h4!envlist, delayadjenv := delayadjenv,     envlist
                            ENDCASE
      CASE s_legatoadjenv:  h4!envlist, legatoadjenv  := legatoadjenv,  envlist
                            ENDCASE
      CASE s_vibampadjenv:  h4!envlist, vibampadjenv  := vibampadjenv,  envlist
                            ENDCASE
      CASE s_vibrateadjenv: h4!envlist, vibrateadjenv := vibrateadjenv, envlist
                            ENDCASE
      CASE s_voladjenv:     h4!envlist, voladjenv     := voladjenv,     envlist
                            ENDCASE
    }
    envlist := h1!envlist
  }
}

AND restoreenvironments(envs) BE IF envs DO
{ // envs    =  0
  //         -> [0, Envs, ln, envlist]
  // envlist =  0
  //         -> [envlist, op, ln, parent, upb, v, absq1, absq2]
  
  LET envlist = h4!envs

  // Retore the global environment variables as specified by envlist.
  
  WHILE envlist DO
  { // envlist -> [envlist, op, ln, parent, upb, v, absq1, absq2]
    LET parent = h4!envlist
    LET envop = h2!envlist

    SWITCHON h2!envlist INTO
    { DEFAULT:
        writef("restoreenvironments: System error envlist=%n envop=%s*n",
	       envlist, opstr(envop))
        abort(999)

      CASE s_tempoenv:      tempoenv      := parent; ENDCASE
      CASE s_volmapenv:     volmapenv     := parent; ENDCASE

      CASE s_delayenv:      delayenv      := parent; ENDCASE
      CASE s_legatoenv:     legatoenv     := parent; ENDCASE
      CASE s_vibampenv:     vibampenv     := parent; ENDCASE
      CASE s_vibrateenv:    vibrateenv    := parent; ENDCASE
      CASE s_volenv:        volenv        := parent; ENDCASE

      CASE s_delayadjenv:   delayadjenv   := parent; ENDCASE
      CASE s_legatoadjenv:  legatoadjenv  := parent; ENDCASE
      CASE s_vibampadjenv:  vibampadjenv  := parent; ENDCASE
      CASE s_vibrateadjenv: vibrateadjenv := parent; ENDCASE
      CASE s_voladjenv:     voladjenv     := parent; ENDCASE
    }
    envlist := h1!envlist
  }
}

AND prlocation(q) BE
{ // Output the location in msecs and absq.
  // q is the local qbeat value.
  LET absq = q2absq(q)
  LET msecs = absq2msecs(absq)
  writef("%10.3d %7i: ", msecs, absq)
//newline()
//abort(1187)
}


AND prshapes() BE
{ //writef("*nprshapes: entered*n")
  // Typical environment
  // tempoenv =  0
  //          => [envlist, Tempoenv, ln, parent, upb, v, absq1, absq2]
//  abort(6775)
  LET formf = "%6.2f"
  LET formi = "%6i"
  prenv(formf, tempoenv)
  prenv(formf, volmapenv)

  prenv(formf, delayenv)
  prenv(formf, legatoenv)
  prenv(formf, vibampenv)
  prenv(formf, vibrateenv)
  prenv(formf, volenv)

  prenv(formf, delayadjenv)
  prenv(formf, legatoadjenv)
  prenv(formf, vibampadjenv)
  prenv(formf, vibrateadjenv)
  prenv(formf, voladjenv)

  newline()
//abort(7745)
}

AND prenv(format, env) BE IF env & h6!env DO
{ // Print a shape environment
  // env -> [-, op, ln, parent, upb, v, absq1, absq2]
  // format is "%i6" or "%6.2f"
  //writef("prenv: entered*n")
  writef("env -> ", env)
  IF env DO
  { LET link   = h1!env
    LET op     = h2!env
    LET ln     = h3!env
    LET parent = h4!env
    LET upb    = h5!env
    LET v      = h6!env
    LET absq1  = h7!env
    LET absq2  = h8!env
    writef(" %n %s parent=%n upb=%n v=%n qbeat=%n, qlen=%n*n",
             link, opstr(op), parent, upb, v, absq1, absq2)
//abort(32871)
    IF v DO
    { LET layout = 1
      LET n = h1!v
      FOR i = 1 TO n BY 2 DO
      { LET qb, val = v!i, v!(i+1)
        IF layout>4 DO
        { newline()
	  layout := 1
	}
	layout := layout+1
	writef("  %i6: ", qb)
	TEST val=starval
	THEN writef("   star")
	ELSE writef(format, val)
      }
      newline()
    }
//    abort(4553)
  }
}

AND prmsv(msv) BE IF msv DO
{ // Print the msecs environment
  writef("msv=%n upb=%n", msv, msv!0)
  FOR i = 1 TO msv!0 DO
  { LET qb=i-1
    IF qb MOD 5 = 0 DO writef("*n%i7: ", qb)
    writef(" %i7", msv!i)
    IF qb>512 RETURN
  }
}

AND calcqlen(t) = t=0 -> 0, VALOF
{ // t points to any node in the parse tree. The initial call
  // is calcqlen(tree) in start. It returns the qlen of the
  // construct pointed to by t.

  // Note that conductorblock was set by formtree while the
  // parse tree was being constructed, and that Blocks already
  // have thei envs fields pointing to the lists of environment
  // nodes for shape types found in each Block. These
  // environments are empty ready to be filled with raw shape
  // data by findrawshapes.

  // calcqlen explores every node reachable from t to fill in the
  // qlen fields of every node. It ensures that currtuplet and
  // currblock are correctly set at all times., and the parent
  // fields of Tuplet, Block and environment nodes are set
  // when they are encountered.

  // For Block nodes
  // t -> [0,Block,ln,notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
  // it fills in the qlen, parent, qbeat and tupnode fields.

  // For Tuplet nodes
  // t -> [-, Tuplet, ln, notes, qlen, parent, qbeat, toqlen]
  // it fills in the qlen, parent, qbeat and toqlen fields.
  // Note that qlen is the length of the note sequence and
  // toqlen is the length after scaling.
  
  // The functions q2blkq and q2absq depend on the settings of
  // fields in Block and Tuplet nodes as set by calcqlen. They
  // therefore work correctly after calcqlen returns. Nte that
  // these functions do not depend on the shape values in
  // environments and so are not affected by the calls of
  // findrawshapes, replacestars and setshapes that are called
  // later. But q2msecs is dependent on Tempo data and so
  // cannot be used until setshapes returns.

  // Note that calcqlen(tree) is called before barscan so
  // so the bar numbering data in barsxv in not yet available.
  // At this stage error messages can only specify the location
  // of errors using source file names and line numbers.
  
  LET op   = h2!t  // The tree node operator
  LET ln   = h3!t  // The lineno value

//writef("calcqlen: t=%n op=%9t currqbeat=%n*n", t, opstr(op), currqbeat)
//IF currpartname DO writef(" name=%s*n", currpartname)
//writef("calcqlen: currtuplet=%n currblock=%n conductorblock=%n *
//       *conductorflag=%n*n",
//        currtuplet, currblock, conductorblock, conductorflag)
//abort(1990)

  SWITCHON op INTO // All possible note and shape items.
  { DEFAULT:
      // We only have to work on nodes that can have a non zero lengths
      // all others can be ignored returning a length of zero.
      
      //writef("calcqlen: t=%n ignoring node with op = %s*n",
      //        t, opstr(op))
      //abort(999)
      RESULTIS 0                          // Return from calcqlen

     CASE s_name: // t -> [-, Name, ln, str]
      currpartname := h4!t                // Just remember the name
      RESULTIS 0                          // Return from calcqlen

    CASE s_score: // This is needed since the first call of calcqlen
                  // is for the entire parse tree.
    { // t         -> [-, Score, ln, name, conductor, parts, qlen]
      // conductor -> [0, Conductor, ln, block, qlen]
      // parts     -> [0, Parts, ln, partlist, qlen]
      // partlist  -> [partlist, Block, ln, notes, qlen, parent,
      //                         qbeat, envs, qshiftv, tupnode]
      //           -> [partlist, Notes, ln, notelist, qlen] 

      // This applies calcqlen to the conductor and all the
      // parts and checks that they are all of the same length.
      // It checks that all simple barline a correctly placed,
      // although barlines are optional in parts and solos.

//writef("calcqlen: About to call calcqlen(conductorpart=%n)*n", conductorpart)
      scoreqlen := calcqlen(conductorpart)
      h7!t := scoreqlen
//writef("calcqlen: scoreqlen set to %n*n", scoreqlen)

      calcqlen(h6!t)   // Apply calcqlen to the parts and solos.

      RESULTIS scoreqlen                   // Return from calcqlen
    }

    CASE s_conductor:
    { // t -> [-. Conductor, ln, block, qlen]
      // block will certainly proints to a Block node even is it contains
      // no shape data.
      LET block  = h4!t
      currqbeat   := 0
      currblock   := 0
      currtuplet  := 0
      currbarno   := 1

      variablevol := FALSE // =TRUE if the volume of a note can change
                           // while the note is playing an with wind
	   	           // instruments.
//abort(1988)
      clearenvironments()

      conductorflag := op=s_conductor -> 1, 0

      UNLESS conductorflag DO
      { // For Parts and Solos update the environments with those
        // defined in the conductor part.
        //writef("start: qbeat=%n Calling updateenvironments(condutorenvs=%n)*n",
        //        currqbeat, conductorenvs)
        updateenvironments(conductorenvs)
      }

      currtuplet := 0    // Currently there is no enclosing Tuplet
      currblock  := 0    // No current Block yet
      currqbeat  := 0
      currpartname := "Unknown"
      conductorflag := 1 // About to process the conductor part
//writef("calcqlen: Conductor: About to call calcqlen(block=%n)*n", block)
      // block -> [0, Block,ln,notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
      scoreqlen := calcqlen(block)
      h5!t := scoreqlen
//writef("calcqlen: Conductor: scoreqlen set to %n*n", scoreqlen)
      RESULTIS scoreqlen                   // Return from calcqlen
    }

    CASE s_parts:  // t -> [-, Parts,  ln, partlist, qlen]
    { LET partlist = h4!t
      // partlist -> [partlist, Part, ln, block, qlen]
      // partlist -> [partlist, Solo, ln, block, qlen]
      // or       = 0

      // Apply calcqlen to each Part or Solo node in the part list.
      
      WHILE partlist DO
      { // partlist -> [-, Part, ln, block, qlen]
        // partlist -> [-, Solo, ln, block, qlen]
	LET qlen = 0
        conductorflag := 0 // About to process a part or a solo.
        currtuplet := 0
        currblock  := conductorblock
        currqbeat  := 0
	currpartname := "No name"
        qlen := calcqlen(partlist) // Apply to a Part or Solo
	UNLESS qlen=scoreqlen DO
	{ writef("Part %s has qlen=%n but scoreqlen=%n*n",
	         currpartname, qlen, scoreqlen)
	}
        partlist := !partlist       // Select the next part, if any.
	//abort(9997)
      }
//abort(6998)
      h5!t := scoreqlen
      RESULTIS scoreqlen                   // Return from calcqlen
    }

    CASE s_part:       // t -> [-. Part, ln, block, qlen]
    CASE s_solo:       // t -> [-. Solo, ln, block, qlen]
    { // If the length is not already known it calculates it using
      // a call of calcqlen and update the qlen field in the parse tree.
      LET qlen = 0
      clearenvironments()

      conductorflag := op=s_conductor -> 1, 0

      UNLESS conductorflag DO
      { // For Parts and Solos update the environments with those
        // defined in the conductor part.
        //writef("start: qbeat=%n Calling updateenvironments(condutorenvs=%n)*n",
        //        currqbeat, conductorenvs)
        updateenvironments(conductorenvs)
      }

      currqbeat  := 0

      currpartname := 0
      currtuplet := 0
      currblock := conductorblock
      qlen := calcqlen(h4!t)
      h5!t := qlen

      // test that the length if this part matches the score length.
      UNLESS qlen = scoreqlen DO
      { writef("Error: %s *"%s*" has qlen=%n but the score length is %n*n",
                opstr(h2!t), currpartname->currpartname,"",
                qlen, scoreqlen)
        writef("It is %n qbeats too %s*n",
                ABS(qlen-scoreqlen), qlen>scoreqlen->"long","short")
        abort(999)
      }

      //writef("calcqlen: %s", opstr(op))
      //IF currpartname DO writef(" %s", currpartname)
      //writef(" qlen=%n*n", qlen)
      RESULTIS qlen                   // Return from calcqlen
    }

    CASE s_block:                     // In calqlen
    // t -> [-, Block,ln,notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
    // envs    =  0
    //         -> [-, Envs, ln, envlist] 
    // envlist =  0
    //         -> [envlist, op, ln, parent, upb, v, absq1, absq2]
    { LET qlen = 0
      LET envs = h8!t
      LET envlist = envs -> h4!envs, 0
      LET blockqbeat = currqbeat

      // Update the Block fields needed by q2blkq and q2absq.
      h6!t  := conductorflag -> 0, currblock // Set the parent
      h7!t  := blockqbeat  // Set the qbeat field
      h10!t := currtuplet  // Set the tupnode field

      currblock  := t
      currtuplet := 0
      currqbeat  := 0
      qlen := calcqlen(h4!t) // The qlen of notes ie of the Block
      // Restore previous versions
      currblock  := h6!t     // parent
      currtuplet := h10!t    // tupnode
      currqbeat  := h7!t     // qbeat

//writef("calcqlen: envlist=%n blockqbeat=%n qlen=%n*n",
//        envlist, blockqbeat, qlen)
//abort(123457)
      WHILE envlist DO
      { // Set this environment's parent.
        h4!envlist := findenv(h2!envlist)
        envlist := h1!envlist
      }
      h5!t := qlen
      
      currqbeat  := blockqbeat+qlen
      currblock  := h6!t
      currtuplet := h10!t
      RESULTIS qlen                   // Return from calcqlen
    }

    CASE s_tuplet:
    // t -> [-, Tuplet, ln, notes, qlen, parent, qbeat, toqlen]
    { LET notes = h4!t
      LET toqlen= h8!t
      // The value of toqlen the right hand argument of \tuplet was
      // determined during syntax analysis.
      // Set qlen to the scaled length of notes.

      // Update the Tuplet fields needed by q2blkq and q2absq.
      h6!t := currtuplet // The parent field
      h7!t := currqbeat
      currtuplet := 0
      currqbeat := 0
      h5!t := calcqlen(notes)

      currqbeat  := h7!t+toqlen
      currtuplet := h6!t
      RESULTIS toqlen                   // Return from calcqlen
    }

    CASE s_notes:       // t -> [-, Notes, ln, notelist, qlen]
    { LET notelist = h4!t
      LET qlen = 0 // This will hold the length of the list of notes.
      // Apply calcqlen to each note item in the list and
      // return the sum of their lengths.
      WHILE notelist DO
      { LET len = calcqlen(notelist)
        qlen := qlen+len
//writef("calcqlen: %s node len=%n => qlen=%n*n",
//        opstr(h2!notelist), len, qlen)
        notelist := !notelist
      }
      h5!t := qlen // Store the sum of the length in the Notes node.
      RESULTIS qlen                   // Return from calcqlen
    }

    CASE s_note:      // t -> [-, Note,     ln, <letter:sharps:dots:n>, qlen]
    CASE s_notetied:  // t -> [-, Notetied, ln, <letter:sharps:dots:n>, qlen]
    { LET note   =  h4!t
      LET n      =  note      & 255
      LET dots   = (note>> 8) & 255
      LET sharps = (note>>16) & 255
      LET letter = (note>>24) & 255
      LET qlen = h5!t
      IF qlen<0 DO qlen, h5!t :=prevqlen, prevqlen
      // These already have their lengths in the tree.
      //prnote(letter, sharps, dots, n, qlen)
      //newline()
      currqbeat := currqbeat+qlen
      RESULTIS qlen // Return the length.
    }
    
    CASE s_fnum:
    CASE s_barline:
      //writef("calcqlen: t=%n ignoring node with op = %s*n",
      //       t, opstr(op))
      //abort(98765)
      RESULTIS 0                          // Return from calcqlen

    CASE s_doublebar:
      //writef("calcqlen: t=%n ignoring node with op = %s*n",
      //       t, opstr(op))
      //abort(98766)
      RESULTIS 0                          // Return from calcqlen

    CASE s_rest:      // t -> [-, Rest,  ln, qlen, dots]
    CASE s_space:     // t -> [-, Space, ln, qlen, dots]
      // These already have their lengths in the tree.
      currqbeat := currqbeat+h4!t
      RESULTIS h4!t // Return the length.

    CASE s_null:
    //writef("calcqlen: CASE Null*n")
    //abort(992255)
      RESULTIS 0                   // Return from calcqlen

    CASE s_par:       // t -> [-, Par, ln, parlist, qlen]
    { LET list = h4!t // list of note items
      LET qlen = -1
      currln := h3!t

      // Apply calcqlen to each member of the par list
      // and check that all members are the same length.
      WHILE list DO
      { LET len = 0
        len := calcqlen(list)
        IF qlen<0 DO qlen := len // The length of the first member
	UNLESS len=qlen DO       // Check that other members agree
          trerr("All members of a Par construct must have the *
                    *same length*nqlen=%n len=%n", qlen, len)
        list := !list
      }
      h5!t := qlen // Save qlen in the Par node.
      currqbeat := currqbeat+qlen
      RESULTIS qlen                   // Return from calcqlen
    }

    CASE s_delay:      // t -> [-, Delay,     ln, notes, shape]
    CASE s_delayadj:   // t -> [-, Delayadj,  ln, notes, shape]
    CASE s_legato:     // t -> [-, Legato,    ln, notes, shape]
    CASE s_legatoadj:  // t -> [-, Legatoadj, ln, notes, shape]
    CASE s_tempo:      // t -> [-, Tempo,     ln, notes, shape]
    CASE s_vibrate:    // t -> [-, Vibrate,   ln, notes, shape]
    CASE s_vibrateadj: // t -> [-, Vibrateadj,ln, notes, shape]
    CASE s_vibamp:     // t -> [-, Vibamp,    ln, notes, shape]
    CASE s_vibampadj:  // t -> [-, Vibampadj, ln, notes, shape]
    CASE s_vol:        // t -> [-, Vol,       ln, notes, shape]
    CASE s_voladj:     // t -> [-, Voladj,    ln, notes, shape]
    CASE s_volmap:     // t -> [-, Volmap,    ln, notes, shape]
    { // t      -> [-, op, ln, notes, shape]
      // notes  -> [-, Notes, ln, notelist, qlen]
      // shape  -> [-, Shape, ln, shapelist, qlen]
      LET notes = h4!t
      LET shape = h5!t

//writef("calcqlen: calling calcqlenshape(shape)*n")
//abort(6345)
//writef("calcqlen: t=%n notes=%n h4!notes=%n shape=%n*n", t, notes, h4!notes, shape)
      //abort(117877)
      calcqlenshape(shape)      // Find the qlen of the Shapes node
      //abort(117878)
      RESULTIS calcqlen(notes)  // Find the qlen of left operand
    }
  }
}

AND findrawshapes(t) BE
{ // This function collects raw shape data belonging to each
  // Block node contained in the tree pointed to by t. This
  // is initially called from start to find the raw shape data
  // in the entire score. t points to any tree node where
  // shape data can be found.

  // While findrawshapes is running it ensures that the
  // variables currqbeat, currtuplet and currblock are
  // correctly set. This ensures that the functions q2blkq
  // and q2absq will thus work correctly.

  // When encountering a Block node findrawshapes checks that
  // the block is allowed ie conductorflag is not 2. It then
  // updates the global environments for the shape types
  // declared in the block. These are used by addshapedata
  // when shape data belonging to the block is found. When
  // leaving the Block the previous environments are restored.

  // findrawdshapes leaves star values in shape data as
  // stars represented by the special value bit pattern
  // starval. These are replaced later by their true values
  // when replacestars is called.
  
  // The next transformation of the parse tree is performed
  // by setshapes. It replaces the raw shape values with
  // values that take account of enclosing shape values.

  // env -> [-, op, ln, parent, upb, v, absq1, absq2]

  // op is typically Tempoenv or Volenv.
  // upb and v are both initially zero and form a self
  //    expanding vector of shape data consisting of
  //    qbeat and value pairs.
  // absq1 and absq2 are filled in by an earlier call of
  //    calcqlen. They hold the the local qbeats of the
  //    start and end of the enclosing block or tuplet.
  // parent is zero or points to the nearest enclosing
  //    environment of the same type.

  // Simple barlines (s_barline) in parts and solos are
  // optional but when they occur they are checked to
  // ensure they that they only occur at the barline
  // positions as specified by the conductor part.
  // Double bars and repeat barlines are not checked.

  LET op = h2!t
  LET ln = h3!t
  
  currln := ln  // For trerr error messages.

//  writef("findrawshapes: t=%n currqbeat=%n op=%s *
//       *currpartname=%s conductorflag=%n*n",
//        t, currqbeat, opstr(op), currpartname, conductorflag)

  SWITCHON op INTO
  { DEFAULT:
      writef("findrawshapes: System error - Unexpected op = %s ", opstr(op))
      prlineno(ln)
      newline()
      //abort(999)
      RETURN                 // Return from findrawshapes

     CASE s_name: // t -> [-, Name, ln, str]
      currpartname := h4!t
//writef("findrawshapes: %i5: Name %s*n", currqbeat, currpartname)
      RETURN

    // Tree operators ignored by findrawshapes.
    CASE s_title:
    CASE s_composer:
    CASE s_timesig:
    CASE s_varvol:
    CASE s_bank:
    CASE s_patch:
    CASE s_instrumentname:
    CASE s_arranger:
    CASE s_transposition:
    CASE s_nonvarvol:
    
    CASE s_fnum:
    CASE s_doublebar:
      //abort(60001)
      RETURN                 // Return from findrawshapes

    CASE s_barline:
    { LET absq = q2absq(currqbeat) // Ignoring qshiftv fields
      //LET barnox = tstabsq2barno(absq)
      LET barno = absq2barno(absq)
      LET barabsq = result2 // Distance between absq and the start
                            // of the bar.
      //writef("Findrawshapes: currqbeat=%n absq=%n barno=%n barabsq=%n*n",
      //        currqbeat, absq, barno, barabsq)
      //writef("Findrawshapes: conductorflag=%n*n",conductorflag)
      IF barabsq DO
        trerr("Misplaced barline %n qbeats after start of bar %n",
	         barabsq, barno)
//abort(778899)
      RETURN
    }
    
    CASE s_score:
    { // This is needed since the outermost call of findrawshapes
      // is for the entire parse tree.

      // The score contains a conductor part and the collection
      // of other parts. The conductor part specifies the overall
      // structure of the score including details of time
      // signatures and the placement of bar lines. Positions
      // within the score are measured in qbeats with a region
      // of 1024 qbeats corresponding to the length of a crotchet
      // (or quarter note). With a tempo of 60 crotchets per
      // minute, one qbeat last 1000/1024=0.976 msecs.

      // t         -> [-, Score, ln, name, conductor, parts, qlen]
      // conductor -> [-, Conductor, ln, block, qlen]
      // parts     -> [-, Parts, ln, partlist, qlen]
      // partlist  =  0
      // or        -> [partlist, Part, ln, block, qlen]
      // or        -> [partlist, Solo, ln, block, qlen]
      // block     -> [-, Block,ln,notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
      // envs      -> [0, Envs, ln, envlist]
      // envlist   -> [envlist, op, ln, parent, upb, v, absq1, absq2]
      // or        -> 0
      // where op is an environment name such as s_volenv.
      // parent is zero or points to the enclosing environment of the
      //          same type.
      // The pair [upb,v] is a self expanding vector to hold the
      //          shape data.
      // upb is the upper bound of v and v!0 holds the subscript of
      //          the last element used in v. Items in v are pairs
      //          [qpos,val] starting at subscript 1 with the first
      //          qpos=absq1 and the last qpos=qbeat2. Note that
      //          the qpos values are absolute locations.
      // The elements are inserted using calls of
      //          pushival(@upb, x), pushipair(@upb, q, x).
      //          pushfval(@upb, x), pushfpair(@upb, q, x)
      // envlist is the list of environments belonging to the block.

      LET conductor = h5!t     // -> [-, Conductor, ln, block, qlen]
      LET parts     = h6!t     // -> [-. Parts, ln, partlist, qlen]
//abort(20101)
      // The parse tree already has empty shape environments where required
      // attatched to all Block nodes in the envs field.


//writef("findrawshapes: conductorpart=%n conductorblock=%n conductorenvs=%n*n",
//        conductorpart, conductorblock, conductorenvs)

      // Fill in the shape environments declared in the conductor's Block node.
      currpartname  := "No name yet"
      currtuplet    := 0
      currblock     := 0
      conductorflag := 1
      findrawshapes(conductor)
      //prshapes()
      //writef("tempoenv=%n*n", tempoenv)
      //abort(1984)

      // Safety check
      UNLESS h2!conductor=s_conductor &
             h2!conductorblock=s_block DO

      { synerr("findrawshapes: System error: conductorblock is not a Block")
        abort(999)
      }

//      writef("About to call findrawshapes(parts)*n")
      currpartname  := "None"
      currtuplet    := 0
      currblock     := 0
      conductorflag := 0
      findrawshapes(parts)
//      abort(99905)
      RETURN      
    }

    CASE s_conductor:   // t -> [-, Conductor, ln, block, qlen]
    CASE s_part:        // t -> [-. Part,      ln, block, qlen]
    CASE s_solo:        // t -> [-. Solo,      ln, block, qlen]
    { LET block = h4!t  // block is a Block or Notes node.
 
      timesig_t, timesig_b := 4, 4
      qbeatsperbeat := 4096/timesig_b // ie 1024 for quarter note beats

      currqbeat   := 0
      currblock   := 0
      currtuplet  := 0
      currbarno   := 1

      variablevol := FALSE // =TRUE if the volume of a note can change
                           // while the note is playing an with wind
	   	           // instruments.
//abort(1988)
      clearenvironments()

      conductorflag := op=s_conductor -> 1, 0

      UNLESS conductorflag DO
      { // For Parts and Solos update the environments with those
        // defined in the conductor part.
        //writef("start: qbeat=%n Calling updateenvironments(condutorenvs=%n)*n",
        //        currqbeat, conductorenvs)
        updateenvironments(conductorenvs)
      }

      currtuplet := 0
      currblock  := 0
      currqbeat  := 0
//abort(1989)
      findrawshapes(block)
//abort(1990)
      RETURN                 // Return from findrawshapes
    }

    CASE s_parts:            // t -> [-. Parts, ln, partlist, qlen]
    { LET partlist = h4!t
      WHILE partlist DO
      { //abort(1002)
        findrawshapes(partlist)
        partlist := h1!partlist
      }
//      writef("Parts node done*n")
//      abort(1991)
      RETURN                 // Return from findrawshapes
    }

    CASE s_block:            // In findrawshapes
    // t -> [-, Block, ln, notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
    { LET op      = h2!t     // op = s_block
      LET ln      = h3!t
      LET notes   = h4!t
      LET qlen    = h5!t
      LET parent  = h6!t     // The enclosing Block or Tuplet
      LET qbeat   = h7!t
      LET envs    = h8!t     // envs in a Block can only be zero in the
                             // conductor part.
      LET qshiftv = h9!t     // = 0 if no Tempo data
                             // =-1 if Tempo data is present

      LET absq1 = q2absq(qbeat)
      LET absq2 = q2absq(qbeat+qlen)

      IF conductorflag=2 DO
        trerr("The conductor part may not have inner Blocks")
      IF conductorflag=1 DO conductorflag := 2
      
      h6!t  := currblock     // The parent block
      h7!t  := currqbeat     // The current qbeat
      h10!t := currtuplet
      
      currblock  := t
      currqbeat  := 0
      currtuplet := 0        // End of the Tuplet chain

      // Setup the new shape environments needed by this block.
      //writef("Block: calling updateenvironments(%n)*n", envs)
      IF envs DO updateenvironments(envs)
      
      // Environments hold data specifying a piecewise linear graphs
      // that are used when calculating a shape value such as the
      // tempo or volume at a specified qbeat location. All shape
      // entries are pairs (q,val) where q is the absolute qbeat
      // location and val is a usually a floating point number. A
      // particular value is the bit pattern shapestar=#x12344321
      // is used to represent the stars in shape data. They are
      // later replaced by true values by replacestars.

//      writef("findrawshapes: t=%n op=%s qlen=%n parent=%n *
//             *qbeat-%n envs=%n qshift=%n*n",
//              t, opstr(op), qlen, parent, qbeat, envs, qshiftv)
      //abort(1292)

      UNLESS h2!notes=s_notes DO
      { writef("findrawshapes: System error: Notes node expected*n")
        abort(999)
      }

      //writef("calling findrawshapes(%n)*n", t)
      findrawshapes(notes)
      //prshapes()
      //abort(7743)

      // Add a star at the end of every shape environment of
      // the block where necessary.
      
      // envs    =  0
      //         -> [0, Envs, ln, envlist]
      // envlist =  0
      //         -> [envlist, op, ln, parent, upb, v, absq1, absq2]
      IF envs DO
      { LET envlist = h4!envs
        //writef("findrawshapes: envlist=%n*n", envlist)
        //abort(80000)
        WHILE envlist DO
        { LET sxv = @h5!envlist
	  LET v = sxv!1
	  LET upb = v!0
	  LET qlen = h5!currblock

          h7!envlist := absq1
	  h8!envlist := absq2
	  
	  //writef("findrawshapes: p=%n v=%n upb=%n qlen=%n*n",
	  //        p, v, upb, qlen)
	  // Possibly insert a star if the last entry is nt at qlen.
	  //abort(80001)
	  UNLESS v & v!(upb-1)=qlen DO
	    pushipair(sxv, q2absq(qlen), starval)
	  envlist := h1!envlist
        }
      }

      // If the block belongs to the conductor part or contains Tempo
      // data, a shift vector is allocated and fill it with approriate
      // values. For the conductor part these values are absolute qbeat
      // locations but for parts and solos they are qbeat location
      // shifts scaled to ensure that the total qlen of the block is
      // unchanged by the tempo shape data.

      // Restore the previous shape environment vectors
      IF envs DO restoreenvironments(conductorenvs)
      currblock  := h6!t
      currqbeat  := h7!t + qlen
      currtuplet := h10!t
      RETURN                 // Return from findrawshapes
    }

    CASE s_notes:       // t -> [-. Notes, ln, notelist, qlen]
    { LET notelist = h4!t   // List of note items
      //writef("findrawshapes: notelist=%n*n", notelist)
      WHILE notelist DO
      { //writef("findrawshapes: Calling findrawshapes notelist=%n*n", notelist)
        findrawshapes(notelist)
	//writef("findrawshapes: Returned findrawshapes notelist=%n*n",
	//        notelist)
        notelist := h1!notelist
      }
//      writef("Notes node done*n")
      //abort(1996)
      RETURN                 // Return from findrawshapes
    }

    CASE s_par:       // t -> [-. Par, ln, parlist, qlen]
    { LET parlist = h4!t
      LET qlen    = h5!t
      //writef("findrawshapes: parlist=%n*n", parlist)
      WHILE parlist DO
      { //writef("findrawshapes: Calling findrawshapes notelist=%n*n", notelist)
        LET prevqbeat = currqbeat
        findrawshapes(parlist)
	currqbeat := prevqbeat
	//writef("findrawshapes: Returned findrawshapes parlist=%n*n",
	//        notelist)
        parlist := h1!parlist
      }
      currqbeat := currqbeat + qlen
//      writef("Par node done*n")
      //abort(2996)
      RETURN                 // Return from findrawshapes
    }

    CASE s_note:        // t -> [-, Note,     ln, <letter:sharps:dots:n>, qlen]
    CASE s_notetied:    // t -> [-, Notetied, ln, <letter:sharps:dots:n>, qlen]
      // Increment currqbeat
      currqbeat := currqbeat + h5!t
      RETURN                 // Return from findrawshapes

    CASE s_rest:        // t -> [-, Rest, ln, qlen]
    CASE s_space:       // t -> [-, Space, ln, qlen]
      currqbeat := currqbeat + h4!t
      RETURN                 // Return from findrawshapes

    CASE s_null:        // t -> [-, Null, ln]
      RETURN                 // Return from findrawshapes

    CASE s_tuplet: // t -> [-, Tuplet, ln, notes, qlen, parent, qbeat, toqlen]
    { // calcqlen has already been called so all the fields in the Tuplet
      // node have be set.
      LET notes  = h4!t
      LET toqlen = h8!t   // The qlen specified by the right hand operand.
      LET prevcurrqbeat = currqbeat

      UNLESS h6!t = currtuplet DO
      { writef("findrawshapes: System error h6!t=%n currtuplet*n",
                h6!t, currtuplet)
	abort(999)
        h6!t := currtuplet  // Update the parent field
      }
      
      currtuplet := t
      currqbeat  := 0
      
      findrawshapes(notes) 

      // Restore the previous context.
      currtuplet := h6!t
      currqbeat := prevcurrqbeat + toqlen
      RETURN                 // Return from findrawshapes
    }

    // All the following shape operators have the structure:
    // t -> [-, op, ln, notes, shape]

    CASE s_tempo:      addshapedata(tempoenv,      t); RETURN
    CASE s_volmap:     addshapedata(volmapenv,     t); RETURN

    CASE s_delay:      addshapedata(delayenv,      t); RETURN
    CASE s_legato:     addshapedata(legatoenv,     t); RETURN
    CASE s_vibamp:     addshapedata(vibampenv,     t); RETURN
    CASE s_vibrate:    addshapedata(vibrateenv,    t); RETURN
    CASE s_vol:        addshapedata(volenv,        t); RETURN

    CASE s_delayadj:   addshapedata(delayadjenv,   t); RETURN
    CASE s_legatoadj:  addshapedata(legatoadjenv,  t); RETURN
    CASE s_vibampadj:  addshapedata(vibampadjenv,  t); RETURN
    CASE s_vibrateadj: addshapedata(vibrateadjenv, t); RETURN
    CASE s_voladj:     addshapedata(voladjenv,     t); RETURN
}
}

AND replacestars(t) BE
{ // This function is called after findrawshapes(tree) so all
  // enviromnent nodes have been filled with raw shape data,
  // but stars in shape data are represented by the special
  // bit pattern starval.

  // This function searches the entire score and when it
  // encounters a Block it scans every environment in the
  // Block's envs field replacing each starval bit patterns
  // with its true value based on the raw shape data in the
  // enclosing Blocks.

  // The parent fields of all environment nodes have already
  // been set correctly, so shapeval can be used to calculate
  // the required shape value.

  // Environments have the following structure
  
  // env -> [-, op, ln, parent, upb, v, absq1, absq2]
  
  // op is an environment type such as Tempoenv or Volenv.
  // upb and v form a self expanding vector of shape data.
  //    populated by a previous call of findrawshapes.
  // absq1 and absq2 respectively hold the absolute qbeat
  //    positions of the start and end of the Block covered
  //    by this environment.
  // parent is zero for the conductor's Block. For Parts and
  // Solos it points to the nearest enclosing environment of
  // the same type. This may be the conductor's Block.

  // While replacestars is running, the variables currqbeat,
  // currtuplet, currblock are maintained to ensure that
  // function such as q2blkq and q2absq work correctly.
  // When processing the conductor's part conductorflag
  // will be 1 or 2, otherwise it is zero.
  
  LET op = h2!t
  LET ln = h3!t

  currln := ln  // For trerr error messages.

//writef("replacestars: t=%n op=%s*n", t, opstr(op))
//writef("replacestars: currqbeat=%n currblock=%n currtuplet=%n*n",
//        currqbeat, currblock, currtuplet)
//IF op=s_block DO
  //abort(99999)

  SWITCHON op INTO
  { DEFAULT:
      // We (silently} ignore all tree nodes that cannot lead to
      // a Block.
      writef("replacestars ignoring a node with op = %s ", opstr(op))
      prlineno(ln)
      newline()
      abort(999)
      RETURN                          // Return from replacestars

    CASE s_name: // t -> [-, Name, ln, str]
      currpartname := h4!t
      RETURN

    CASE s_title:
    CASE s_composer:
    CASE s_timesig:
    CASE s_varvol:
    CASE s_bank:
    CASE s_patch:
    CASE s_instrumentname:
    CASE s_arranger:
    CASE s_transposition:
    CASE s_nonvarvol:
    
    CASE s_fnum:
    CASE s_barline:
    CASE s_doublebar:
      RETURN                          // Return from replacestars

    CASE s_score:
    { // This case is needed since replacestars is applied to the
      // entire score.
      // t         -> [-, Score, ln, name, conductor, parts, qlen]
      // conductor -> [-, Conductor, ln, block, qlen]
      // parts     -> [-, Parts, ln, partlist, qlen]
      // partlist  =  0
      // or        -> [partlist, Part, ln, block, qlen]
      // or        -> [partlist, Solo, ln, block, qlen]
      // block     -> [-, Block,ln,notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
      // envs      -> [0, Envs, ln, envlist]
      // envlist   -> [envlist, op, ln, parent, upb, v, absq1, absq2]
      // or        -> 0
      // where op is an environment name such as s_volenv.
      // parent is zero or points to the enclosing environment of the
      //          same type.
      // The pair [upb,v] is a self expanding vector to hold the
      //          shape data.
      // upb is the upper bound of v and v!0 holds the subscript of
      //          the last element used in v. Items in v are pairs
      //          [qpos,val] starting at subscript 1 with the first
      //          qpos=0 and the last qpos=qlen.
      // Elements are added using calls of pushival(@upb, x) and
      //           pushfval(@upb, x).
      // envlist is the list of environments belonging to the block.

      LET conductor = h5!t     // -> [-, Conductor, ln, block, qlen]
      LET parts     = h6!t     // -> [-. Parts, ln, partlist, qlen]

      // Apply setsapes to the conductor part
      replacestars(conductor)

      // Apply setsapes to the other parts and solos.
      replacestars(parts)

//      abort(776654)
      RETURN                          // Return from replacestars
    }
    
    CASE s_conductor:   // t -> [-, Conductor, ln, block, qlen]
    CASE s_part:        // t -> [-. Part,      ln, block, qlen]
    CASE s_solo:        // t -> [-. Solo,      ln, block, qlen]
    { LET block = h4!t  // block is a Block or Notes node.

      // Set the default time signature.
      timesig_t, timesig_b := 4, 4
      qbeatsperbeat := 4096/timesig_b // ie 1024 for quarter note beats

      // Set all the environment variables.
      clearenvironments()
  
      conductorflag := op=s_conductor -> 1, 0

      //writef("replacestars: t=%n *
      //       *Calling updateenvironments(condutorenvs=%n)*n",
      //        t, conductorenvs)
//abort(10005)
      updateenvironments(conductorenvs)

      currpartname := 0
      currtuplet := 0
      currblock  := conductorblock
      currqbeat  := 0
//abort(10006)
      replacestars(block)

      RETURN                          // Return from replacestars
    }

    CASE s_parts:       // t -> [-. Parts, ln, partlist, qlen]
    //abort(229933)
    { LET partlist = h4!t
      // Apply replacestars to every Part and Solo.
      WHILE partlist DO
      { replacestars(partlist) // partlist is a Part or Solo
        partlist := h1!partlist
      }
      RETURN                          // Return from replacestars
    }

    CASE s_block:
    { // Replace the stars in all environments belonging to this
      // Block.
      // t -> [-, Block, ln, notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
      // tupnode is only non zero if there is an encloing Tuplet node.
      LET op      = h2!t  // op = s_block
      LET ln      = h3!t
      LET notes   = h4!t
      LET qlen    = h5!t
      LET parent  = h6!t
      LET qbeat   = h7!t
      LET envs    = h8!t  // envs in a Block can only be zero in the
                          // conductor part.
      LET qshiftv = h9!t  // = 0 or -1 to be updated with a qshift or
                          //           msecs vector with upb=qlen
      LET tupnode = h10!t // The enclosing Tuplet node, if any

      //writef("replacestars: Block t=%n qshiftv=%n conductorflag=%n*n",
      //        t, qshiftv, conductorflag)
      //abort(448811)

      IF conductorflag=2 DO
      { trerr("The conductor part may not have inner blocks")
        RETURN
      }

      IF envs DO
      { // envs      -> [0, Envs, ln, envlist]
        // envlist   -> [envlist, op, ln, parent, upb, v, absq1, absq2]
        //
	LET envlist = h4!envs
	// Replace all the star in the environments belonging to
	// this Block.
        WHILE envlist DO
	{ // envlist -> [envlist,op,ln,parent,upb,v,absq1,absq2]
          LET envtype = h2!envlist
	  LET parent  = h4!envlist
	  LET v       = h6!envlist
	  LET upb     = v!0
	  IF v DO
	  { FOR i = 2 TO upb BY 2 IF v!i=starval DO
	    { LET absq = v!(i-1)
	      //abort(667879)
	      v!i := shapeval(envtype, parent, absq)
	      //writef("replacestars: replaced starval of *
              //       *type %s at absq=%n with %6.3f*n",
	      //        opstr(envtype), absq, v!i)
	      //abort(667880)
	    }
	    //abort(667881)
	  }
	  envlist := h1!envlist
	}
      }
      
      currtuplet := 0
      currblock  := t
      currqbeat  := 0

      UNLESS conductorflag DO updateenvironments(envs)
 
      //writef("calling replacestars(%n)*n", t)
      replacestars(notes)

      // Restore the current Blocck and Tuplet.
      currblock  := parent
      currtuplet := tupnode

      currqbeat  := qbeat+qlen

      // Restore the previous shape environment vectors
      UNLESS conductorflag DO restoreenvironments(envs)
      RETURN                          // Return from replacestars
    }
      
    CASE s_notes:       // t -> [-. Notes, ln, notelist, qlen]
    { LET notelist = h4!t
      WHILE notelist DO
      { replacestars(notelist)
        notelist := h1!notelist
      }
      RETURN                          // Return from replacestars
    }

    CASE s_par:         // t -> [-, Par,   ln, parlist, qlen]
    { LET prevqbeat = currqbeat
      LET parlist = h4!t
      WHILE parlist DO
      { currqbeat := prevqbeat
        replacestars(parlist)
        parlist := h1!parlist
      }
      currqbeat := prevqbeat+h5!t
      RETURN                          // Return from replacestars
    }

    CASE s_note:        // t -> [-, Note,     ln, <letter:sharps:dots:n>, qlen]
    CASE s_notetied:    // t -> [-, Notetied, ln, <letter:sharps:dots:n>, qlen]
      // Increment currqbeat
      currqbeat := currqbeat + h5!t
      RETURN                          // Return from replacestars

    CASE s_rest:        // t -> [-, Rest, ln, qlen]
    CASE s_space:       // t -> [-, Space, ln, qlen]
      currqbeat := currqbeat + h4!t
      RETURN                          // Return from replacestars

    CASE s_null:        // t -> [-, Null, ln]
      RETURN                          // Return from replacestars

    CASE s_tuplet: // t -> [-, Tuplet, ln, notes, qlen, parent, qbeat, toqlen]
    { // calcqlen has already been called so all the fields in the Tuplet
      // node have been set.
      LET notes  = h4!t

      currqbeat   := 0
      
      currtuplet := t
      replacestars(notes) 
      currtuplet := h6!currtuplet

      RETURN                          // Return from replacestars
    }

    // All the following shape operators have the structure:
    // t -> [-, op, ln, notes, shapes]

    CASE s_tempo:
    CASE s_volmap:

    CASE s_delay:
    CASE s_legato:
    CASE s_vibamp:
    CASE s_vibrate:
    CASE s_vol:

    CASE s_delayadj:
    CASE s_legatoadj:
    CASE s_vibampadj:
    CASE s_vibrateadj:
    CASE s_voladj:
      replacestars(h4!t)
      currqbeat := currqbeat + h5!t
      RETURN                          // Return from replacestars
  }
}

AND setshapes(t) BE
{ // This function is called after both findrawshapes and
  // replacestars have been applied t0 the entire tree. So all
  // enviromnent nodes contain shape data all all stars have
  // replace by the required raw values.

  // For shape values of type Tempo or Volmap, the call of
  // setshapes in start causes the raw shape values to be
  // updated with a weighted average of the raw value in
  // the current environment and the updated value in the
  // enclosing environment at the specified absolute
  // qbeat position. The current shape value has weight
  // shapefaca and the enclosing value has weight shapefacb,
  // with: shapefaca + shapefacb = 1.0. If there is no
  // enclosing data of the required type, the raw value is
  // left unchanged. Finally the modified shape values are
  // multiplied by the modified adjustment values of the
  // right type. But this is only done if such data is
  // available and that it is not of type Tempo or Volmap.

  // This function treats Tempo data differently. Firstly
  // it leaves it in its raw state and uses this data to
  // a vector with upper bound qlen the sice of the Block,
  // and fills it with times in msecs for each qbeat in
  // the block. These are held as integers and are in the
  // range zero to maxmsecs, the computed execution time
  // of the Block. For the conductor these times are the
  // actual times of every absolute qbeat of the score.

  // For Parts and Solos thecontents of this vector is
  // modified by applying the transformation:
  //           v!q := muldiv(qlen, v!q, maxusecs)
  // where
  // v is the vector of msecs times.
  // qlen is the length of the Block and
  // maxusecs=v!qlen.

  // Notice that this causes v!0 to set to zero and
  // v!qlen to be set to qlen. This vector is now a mapping
  // of local qbeat positions of the block to new positions
  // taking account of the Tempo data. But notethe the total
  // time to perform the Block is the same as it would be if
  // no Tempo data had been given. Note that the elements of
  // v are monotonically increasing and that regions where
  // the Tempo is high are compressed into fewer qbeats and
  // where the Tempo is low they are expanded into more qbeats,
  //// giving the required effect.
  
  // Environments have the following structure
  
  // env -> [-, op, ln, parent, upb, v, absq1, absq2]
  
  // op is an environment type such as Tempoenv or Volenv.
  // upb and v form a self expanding vector of shape data.
  //    populated by a previous call of findrawshapes.
  // absq1 and absq2 respectively hold the absolute qbeat
  //    positions of the start and end of the Block covered
  //    by this environment.
  // parent is zero for the conductor's Block. For Parts and
  // Solos it points to the nearest enclosing environment of
  // the same type. This may be an environment belonging to
  // the conductor's Block.

  // While setshapes is running, the variables currqbeat,
  // currtuplet, currblock are maintained to ensure that
  // the function q2blkq, q2absq and q2msecs work correctly.
  // When processing the conductor's part conductorflag will
  // be 1 or 2. For Parts and Solos it is zero.
  
  LET op = h2!t
  LET ln = h3!t

  currln := ln  // For trerr error messages.

//writef("setshapes: t=%n currqbeat=%n dealing op=%s  ",
//        t, currqbeat, opstr(op)); newline()
//abort(99999)

  SWITCHON op INTO
  { DEFAULT:
      // We (silently} ignore all tree nodes that cannot lead to
      // a Block.
      writef("Ignoring a node with op = %s ", opstr(op))
      prlineno(ln)
      newline()
      //abort(999)
      RETURN                          // Return from setshapes

    CASE s_name: // t -> [-, Name, ln, str]
      currpartname := h4!t
      RETURN

    CASE s_title:
    CASE s_composer:
    CASE s_timesig:
    CASE s_varvol:
    CASE s_bank:
    CASE s_patch:
    CASE s_instrumentname:
    CASE s_arranger:
    CASE s_transposition:
    CASE s_nonvarvol:
    
    CASE s_fnum:
    CASE s_barline:
    CASE s_doublebar:
      RETURN                          // Return from setshapes

    CASE s_score:
    { // t         -> [-, Score, ln, name, conductor, parts, qlen]
      // conductor -> [-, Conductor, ln, block, qlen]
      // parts     -> [-, Parts, ln, partlist, qlen]
      // partlist  =  0
      // or        -> [partlist, Part, ln, block, qlen]
      // or        -> [partlist, Solo, ln, block, qlen]
      // block     -> [-, Block,ln,notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
      // envs      -> [0, Envs, ln, envlist]
      // envlist   -> [envlist, op, ln, parent, upb, v, absq1, absq2]
      // or        -> 0
      // where op is an environment name such as s_volenv.
      // parent is zero or points to the enclosing environment of the
      //          same type.
      // The pair [upb,v] is a self expanding vector to hold the
      //          shape data.
      // upb is the upper bound of v and v!0 holds the subscript of
      //          the last element used in v. Items in v are pairs
      //          [qpos,val] starting at subscript 1 with the first
      //          qpos=0 and the last qpos=qlen.
      // Elements are added using calls of pushival(@upb, x) and
      //           pushfval(@upb, x).
      // envlist is the list of environments belonging to the block.

      LET conductor = h5!t     // -> [-, Conductor, ln, block, qlen]
      LET parts     = h6!t     // -> [-. Parts, ln, partlist, qlen]

      // Apply setsapes to the conductor part
      setshapes(conductor)

      // Apply setsapes to the other parts and solos.
      setshapes(parts)

      //abort(776654)
      RETURN                          // Return from setshapes
    }
    
    CASE s_conductor:   // t -> [-, Conductor, ln, block, qlen]
    CASE s_part:        // t -> [-. Part,      ln, block, qlen]
    CASE s_solo:        // t -> [-. Solo,      ln, block, qlen]
    { LET block = h4!t  // block is a Block or Notes node.

      // Set the default time signature.
      timesig_t, timesig_b := 4, 4
      qbeatsperbeat := 4096/timesig_b // ie 1024 for quarter note beats

      // Set all the environment variables.
      clearenvironments()
  
      conductorflag := op=s_conductor -> 1, 0

      //writef("start: qbeat=%n Calling updateenvironments(condutorenvs=%n)*n",
      //        currqbeat, conductorenvs)
      updateenvironments(conductorenvs)

      currpartname := 0
      currtuplet := 0
      currblock  := conductorblock
      currqbeat  := 0

      setshapes(block)

      RETURN                          // Return from setshapes
    }

    CASE s_parts:       // t -> [-. Parts, ln, partlist, qlen]
    //abort(229933)
    { LET partlist = h4!t
      // Apply setshapes to every Part and Solo.
      WHILE partlist DO
      { setshapes(partlist) // partlist is a Part or Solo
        partlist := h1!partlist
      }
      RETURN                          // Return from setshapes
    }

    CASE s_block:
    { // t -> [-, Block, ln, notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
      // tupnode is only non zero if there is an encloing Tuplet node.
      LET op      = h2!t  // op = s_block
      LET ln      = h3!t
      LET notes   = h4!t
      LET qlen    = h5!t
      LET parent  = h6!t
      LET qbeat   = h7!t
      LET envs    = h8!t  // envs in a Block can only be zero in the
                          // conductor part.
      LET qshiftv = h9!t  // = 0 or -1 to be updated with a qshift or
                          //           msecs vector with upb=qlen
      LET tupnode = h10!t // The enclosing Tuplet node, if any

      //writef("setshapes: Block t=%n qshiftv=%n conductorflag=%n*n",
      //        t, qshiftv, conductorflag)
      //abort(448811)

      IF conductorflag=2 DO
      { trerr("The conductor part may not have inner blocks")
        RETURN
      }

      // The first thing to do is to replace every every shape
      // value in the Block's envs list with the weighted average
      // of it and the updated value in the enclosing environment.

      IF envs DO
      { // envs      -> [0, Envs, ln, envlist]
        // envlist   -> [envlist, op, ln, parent, upb, v, absq1, absq2]
        //
	LET envlist = h4!envs
        WHILE envlist DO
	{ // envlist -> [envlist,op,ln,parent,upb,v,absq1,absq2]
	  // The parent is either zero or the appropriate parent
	  // environment.
	  LET parent  = h4!envlist
	  LET envtype = h2!envlist
	  LET adjenv  = findadjenv(envtype)

	  UNLESS envtype=s_tempoenv IF parent | adjenv DO
	  { // The environment type is not Tempo
	    // and there is either a parent environment of the right type
	    // or there is a suitable adjustment environment.
	    LET v = h6!envlist // get the vector of raw shape data
            IF v DO
            { LET upb = v!0
	      FOR i = 2 TO upb BY 2 DO // Consider every shape value
	      { LET absq = v!(i-1)     // in this environment.
                IF parent DO
                { // Only make the change if the parent exists.
		  LET FLT x1 = v!i
		  LET FLT x2 = shapeval(envtype, parent, absq) 
	          v!i := shapefaca*x1 + shapefacb*x2
	          //writef("setshapes: combined shape values %6.3f and %6.3f *
                  //       *of type %s to give %6.3f at absq=%n*n",
	          //        x1, x2, opstr(envtype), v!i, absq)
	          //abort(667880)
		}
		IF adjenv DO
                { LET FLT x1 = v!i
		  LET FLT adj = shapeval(envtype, adjenv, absq)
	          v!i := x1 * adj
	          //writef("setshapes: adjusted shape value %6.3f of *
                  //       *type %s by %6.3f to give %6.3f at absq=%n*n",
	          //        x1, opstr(envtype), adj, v!i, absq)
	          //abort(668881)
		}
	      }
	      //abort(667882)
	    }
	  }
	  envlist := h1!envlist
	}
      }
      
      currtuplet := 0
      currblock  := t
      currqbeat  := 0
      UNLESS conductorflag DO updateenvironments(envs)
 
      IF conductorflag=1 DO
      { // If conductorflag=1 this block is the root block of the conductor
        // and so its qshiftv field must be replaced by the msecs vector
        // which will be creates by mkmsecv. If qshiftv is zero there is
        // no tempo data in the conductor part so the default tempo is
        // used.

        LET msv = mkmsecsv(t)
        h9!t := msv // Update the qshiftv field of the Block.
	msecsv := msv // Note that its bounds are 0 and scoreqlen.
	//writef("setshapes: Just allocated msecsv=%n*n", msv)
	//writef("scoreqlen=%n msv!scoreqlen=%n*n", scoreqlen, msv!scoreqlen)
	IF FALSE DO
	{ writef("qshiftv for the conductor part*n")
	  FOR q = 0 TO scoreqlen DO
	  { IF q MOD 8 = 0 DO writef("*n%i5: ", q)
	    writef(" %6.3d", msv!q)
	    IF q MOD 256 = 0 DO abort(10077)
	  }
	  newline()
	  abort(18835)
	}
      }
 
      UNLESS conductorflag  DO
      { // Deal with a Part or Solo block. Only create a qshiftv
        // vector if the block contains tempo data indicated by
	// qshiftv<0.
        LET msv = qshiftv<0 -> mkmsecsv(t), 0
        h9!t := msv
	//writef("setshapes: Part or Solo msv=%n*n", msv)
	//abort(37895)
	// If necessary modify the msecs elements of msv to
	// local qbeat values.
	IF msv DO
	{ msv2qshiftv(qlen, msv)
	  //abort(6335589)
	  IF FALSE DO
	  { writef("*nqshiftv for a Part or Solo*n")
  	    FOR q = 0 TO qlen DO
	    { IF q MOD 8 = 0 DO writef("*n%i4: ", q)
	      writef(" %i4", msv!q)
	      IF q MOD 256 = 0 DO abort(66223355)
	    }
	    newline()
	    abort(37896)
	  }
	}
      }

      //writef("calling setshapes(%n)*n", t)
      setshapes(notes)

      // Restore the current Blocck and Tuplet.
      currblock  := parent
      currtuplet := tupnode

      currqbeat  := qbeat+qlen

      // Restore the previous shape environment vectors
      UNLESS conductorflag DO restoreenvironments(envs)
      RETURN                          // Return from setshapes
    }
      
    CASE s_notes:       // t -> [-. Notes, ln, notelist, qlen]
    { LET notelist = h4!t
      WHILE notelist DO
      { setshapes(notelist)
        notelist := h1!notelist
      }
      RETURN                          // Return from setshapes
    }

    CASE s_par:         // t -> [-, Par,   ln, parlist, qlen]
    { LET prevqbeat = currqbeat
      LET parlist = h4!t
      WHILE parlist DO
      { currqbeat := prevqbeat
        setshapes(parlist)
        parlist := h1!parlist
      }
      currqbeat := prevqbeat+h5!t
      RETURN                          // Return from setshapes
    }

    CASE s_note:        // t -> [-, Note,     ln, <letter:sharps:dots:n>, qlen]
    CASE s_notetied:    // t -> [-, Notetied, ln, <letter:sharps:dots:n>, qlen]
      // Increment currqbeat
      currqbeat := currqbeat + h5!t
      RETURN                          // Return from setshapes

    CASE s_rest:        // t -> [-, Rest, ln, qlen]
    CASE s_space:       // t -> [-, Space, ln, qlen]
      currqbeat := currqbeat + h4!t
      RETURN                          // Return from setshapes

    CASE s_null:        // t -> [-, Null, ln]
      RETURN                          // Return from setshapes

    CASE s_tuplet: // t -> [-, Tuplet, ln, notes, qlen, parent, qbeat, toqlen]
    { // calcqlen has already been called so all the fields in the Tuplet
      // node have been set.
      LET notes  = h4!t

      currqbeat   := 0
      
      currtuplet := t
      setshapes(notes) 
      currtuplet := h6!currtuplet

      RETURN                          // Return from setshapes
    }

    // All the following shape operators have the structure:
    // t -> [-, op, ln, notes, shapes]

    CASE s_tempo:
    CASE s_volmap:

    CASE s_delay:
    CASE s_legato:
    CASE s_vibamp:
    CASE s_vibrate:
    CASE s_vol:

    CASE s_delayadj:
    CASE s_legatoadj:
    CASE s_vibampadj:
    CASE s_vibrateadj:
    CASE s_voladj:
      setshapes(h4!t)
      currqbeat := currqbeat + h5!t
      RETURN                          // Return from setshapes
  }
}

AND findenv(envtype) = VALOF SWITCHON envtype INTO
{ DEFAULT:           writef("findenv: Bad envtype = %n %s*n",
                             envtype, opstr(envtype))
                     abort(999)
		     RESULTIS 0

  CASE s_tempo:
  CASE s_tempoenv:   RESULTIS 0
  CASE s_volmap:
  CASE s_volmapenv:  RESULTIS 0

  CASE s_delay:
  CASE s_delayenv:   RESULTIS delayenv
  CASE s_legato:
  CASE s_legatoenv:  RESULTIS legatoenv
  CASE s_vibamp:
  CASE s_vibampenv:  RESULTIS vibampenv
  CASE s_vibrate:
  CASE s_vibrateenv: RESULTIS vibrateenv
  CASE s_vol:
  CASE s_volenv:     RESULTIS volenv
}

AND findadjenv(envtype) = VALOF SWITCHON envtype INTO
{ DEFAULT:           writef("findadjenv: Bad envtype = %n %s*n",
                             envtype, opstr(envtype))
                     abort(999)
		     RESULTIS 0

  CASE s_tempo:
  CASE s_tempoenv:   RESULTIS 0
  CASE s_volmap:
  CASE s_volmapenv:  RESULTIS 0

  CASE s_delay:
  CASE s_delayenv:   RESULTIS delayadjenv
  CASE s_legato:
  CASE s_legatoenv:  RESULTIS legatoadjenv
  CASE s_vibamp:
  CASE s_vibampenv:  RESULTIS vibampadjenv
  CASE s_vibrate:
  CASE s_vibrateenv: RESULTIS vibrateadjenv
  CASE s_vol:
  CASE s_volenv:     RESULTIS voladjenv
}

AND addshapedata(env, t) BE IF t DO
{ // This adds shape data to environment env which must exist.
  // It is dealing with nodes having operators such as Tempo or Legato.

  // t      -> [-, op, ln, notes, shape]    op is eg Tempo or Legato
  
  // notes  -> [-, Notes, ln, notelist,  qlen]
  // shape  -> [-, Shape, ln, shapelist, qlen]

  // env    -> [-, op, ln, parent, upb, v, absq1, absq2] 
  // where op is a shape operator such as Tempo or Vol

  LET shapeop = h2!t
  LET ln      = h3!t
  LET notes   = h4!t
  LET shape   = h5!t 

  LET notesqlen = h5!notes
  LET shapelist = h4!shape
  LET shapeqlen = h5!shape
  LET prevqbeat = currqbeat
  LET tuplet    = currtuplet

  LET spaceqlen = 1024  // Every shape list assumes an initial space 
                        // size of 1024 qbeats between consecutive numbers.
	 	        // This is updated by Space items as they are processed.
  
  UNLESS env DO
  { writef("addshapeedata: System error: env must be non zero*n")
    abort(8822) // System error
  }

  prevnum := FALSE
  currqbeat := 0

  //writef("addshapedata: env=%n t=%n currqbeat=%n shapeop=%s notes=%n shape=%n*n",
  //        env, t, currqbeat, opstr(shapeop), notes, shape)
  //writef("addshapedata: notesqlen=%n shapeqlen=%n*n", notesqlen, shapeqlen)
  //writef("addshapedata: currblock=%n currtuplet=%n*n", currblock,currtuplet)
  //abort(8821)

  WHILE shapelist DO
  { // shapelist =  0
    //           -> [-, Num,   ln, value]      a shape value
    //           -> [-, Star,  ln]             *
    //           -> [-, Space, ln, qlen]       s<qlen>
    //           -> [-, Null,  ln]             z

    // This transfers shape data from an item in a shape list
    // to its corresponding shape environment env in the current Block.

    // The absolute qbeat position in the environment is is calculated
    // by first multiplying its local position in the shape list by
    // notesqlen and dividing by shapeqlen, if shapeqlen is non zero.
    // This gives the local position in notes which is then converted
    // to its corresponding absolute position using qlen2absq.

    LET itemop  = h2!shapelist
    LET itemln  = h3!shapelist

    UNLESS env DO
    { writef("addshapedta: System error, env=%n t=%n*n", env, t)
      abort(999)
      RETURN
    }
  
    currln := ln
  
    // Note that currqbeat is the qbeat position within the shape list,
    // initially set to zero. If shapeqlen is non zero, currqbeat must be
    // scaled by multiplying by notesqlen and deviding by shapeqlen
    // before applying q2blkq to obtain the local qbeat position
    // in the enclosing Block.

    //writef("addshapedata: t=%n currqbeat=%n itemop=%s *
    //       *prevnum=%n dpsceqlen=%n  ",
    //        t, currqbeat, opstr(itemop), prevnum, spaceqlen)
    //prlineno(itemln); newline()
    //writef("addshapedata: notesqlen=%n shapeqlen=%n*n",notesqlen, shapeqlen)
    //abort(8222)
    SWITCHON itemop INTO
    { DEFAULT:
        trerr("Bad op %s in shape list", opstr(itemop))
        ENDCASE

      CASE s_star:             // t -> [-, Star, ln]
      CASE s_fnum:             // t -> [-, Fnum, ln, fnumval]
      { LET x = itemop=s_star -> starval,     // The value is either
                                 h4!shapelist // starval or an fnum.
        LET sxv = @h5!env
        LET blkq = ?
	LET q = prevqbeat // qbeat at the start of Notes

        IF prevnum DO
        { // Insert a space of the previous space length.
          currqbeat := currqbeat + spaceqlen
        }
        prevnum := TRUE

        IF shapeqlen DO q := q + muldiv(currqbeat, notesqlen, shapeqlen)
        // Find the absolute qbeat location
        blkq := q2blkq(q, tuplet)
//writef("addshapedata: itemop=%s currqbeat=%n blkq=%n x=%5.3f*n",
//        opstr(itemop), currqbeat, blkq, x)

        // Insert the shape value into env, but if the environment
        // is empty and absq>0 first insert a star at position zero.
//writef("addshapedata: itemop=%s calling pushfpair(sxv, %n, %5.2f)*n",
//                      opstr(itemop), blkq, x)
//        abort(8333)

        pushfpair(sxv, blkq, x) 
//        abort(8334)
        ENDCASE
      }
     
      CASE s_space:            // t -> [-, Space, ln, qlen]
      { LET qlen = h4!shapelist // The qlen of this kind of space
//writef("addshapedata: itemop=%s qlen=%n*n", opstr(itemop), qlen)
        currqbeat := currqbeat + qlen
        spaceqlen := qlen
        prevnum := FALSE
        ENDCASE
      }
      
      CASE s_null:             // t -> [-, Null, ln]
//writef("addshapedata: itemop=%s*n", opstr(itemop))
        prevnum := FALSE
        ENDCASE
    }
    shapelist := h1!shapelist
  }
//abort(1199)

  currqbeat := prevqbeat

//writef("addshapedata: about to call findrawshapes(notes)*n")
//abort(1200)
  findrawshapes(notes)      // Find shape data in notes
//abort(5123)
}

AND calcqlenshape(shape) = VALOF
{ // shape points to a Shape node.
  // This function calculates the qlen of its shapelist and
  // updates the qlen field in the Shape node.
  // This returns this qlen.
  
  // shape     -> [-, Shape, ln, shapelist, qlen]
  // shapelist -> [shapelist, Num,   ln, number]
  // or        -> [shapelist, Star,  ln]
  // or        -> [shapelist, Space, ln, qlen]
  // or        =  0

  LET shapelist = h4!shape
  LET qlen      = 0
  LET prevnum   = FALSE   // Set to TRUE by Num or Star
  LET spaceqlen = 1024    // The default qlen for a space is 1024
  
  //writef("*ncalcqlenshape: Entered shape=%n shapelist=%n*n", shape, shapelist)
  
  WHILE shapelist DO
  { // shapelist   -> [-, Fnum,  ln, fnumval]
    // or          -> [-, Star,  ln]
    // or          -> [-, Space, ln, qlen]

    LET op = h2!shapelist
//writef("calcqlenshape: shapelist=%n op=%s*n", shapelist, opstr(op))
//abort(5678)
    SWITCHON op INTO
    { DEFAULT:
        trerr("Bad op %s in shape list", opstr(op))
        ENDCASE

      CASE s_star:    // shapelist -> [-, Star, ln]
      CASE s_fnum:    // shapelist -> [-, Fnum, ln, value]
        IF prevnum DO
        { // Assume a space of the previous qlen
          // between adjacent numbers or stars.
          qlen := qlen + spaceqlen
        }
        prevnum := TRUE
	//TEST op=s_star
	//THEN writef("calcqlenshape: star  qlen=%n  star*n", qlen)
	//ELSE writef("calcqlenshape: num=%7.3f  qlen=%n*n", h4!shapelist, qlen)
        ENDCASE

      CASE s_space:   // shapelist -> [-, Space, ln, qlen]
      CASE s_rest:    // shapelist -> [-, Rest,  ln, qlen]
        prevnum := FALSE
        spaceqlen := h4!shapelist
        qlen := qlen + spaceqlen
	//writef("calcqlenshape: space or rest q%n qlen=%n*n", noteqlen, qlen)
        ENDCASE
	
      CASE s_null:    // shapelist -> [-, Null, ln]
        prevnum := FALSE
	//writef("calcqlenshape: null qlen=%n*n", qlen)
        ENDCASE
    }
    shapelist := h1!shapelist
    //writef("calcqlenshape: shapelist=%n qlen=%n*n", shapelist, qlen)
  }

  h5!shape := qlen // Fill in the qlen of the Shape node.
//writef("calcqlenshape: Returning qlen=%n*n*n", qlen)
//abort(5680)
  RESULTIS qlen
}

AND barscan(t) BE IF t DO
{ // barscan is only called once and its argument t points to the
  // parse tree node of the conductor part. Its purpose is to create
  // the barsxv vector giving the absolute qbeat locations of all
  // barlines. It is called after calcqlen has been applied to the
  // score, so the bar numbering data held is barsxv is not available
  // to calcqlen.

  // Barscan only inspects the conductorpart and makes no changes
  // to the tree.
  
  // The global currbarqbeat is the absolute qbeat location of the
  // most recent bar line 0r -1 before the first barline is found.
  // barscan maintains the current number of qbeats per bar in
  // qbeatsperbar based on the current time signature.

  // When barscan returns barsxv will point to [upb,v] where upb
  // is the upperbound of v and v is a vector holding the absolute
  // qbeat positions of every barline in the score. When barscan
  // completes the entries in v it places v in the global variable
  // barno2absqv. barno2absq!0 holds the upb of barno2absqv namely
  // the number of the bar just after the end of the score. For
  // other enties such as barno2absqv!n hols the absolute qbeat
  // location of bar n.
  
  // barno2absqv!1 is the absolute qbeat location of the start of
  // the first complete bar. This is the location of the first
  // full bar and, if it is not zero, an incomplete bar starting
  // at location zero.

  // Simple barlines (s_barline) must only occur at positions
  // that agree with the current time signature. The current bar
  // number is held in currbarno. It is incremented by every
  // simple barline. Doublebars and repeat barlines alsoincrement
  // currbarno but only when they occur at a position where a
  // simple barline is allowed. This allows double bars and
  // repeat bars to occur in the middle of a bars without
  // incrementing the bar numbers.

  // Simple barlines in parts and solos are optional but when
  // they occur they are checked to ensure they are positioned
  // correctly. Bar numbers are used in error messages and when
  // selecting which bars to play.
  
  // t  -> [-, Conductor, ln, block, qlen]

  LET op      = h2!t   // An operator in the conductor part.
  LET ln      = h3!t   // The fno/lineno of \conductor
  LET block   = h4!t   // The Block node of the conductor part.
  
  // Initialise the barscan global variable.
  timesig_t := 4       // Set the default time signature of 4/4
  timesig_b := 4

  qbeatsperbeat := 1024 * 4 / timesig_b
  qbeatsperbar  := timesig_t * qbeatsperbeat

  currbarno     := 0  // No bar line yet
  maxbarno      := 0  // This will contain the number of the last
                      // bar of the conductor part.
  currbarqbeat  := -1 // No barlines yet

  currpartname  := 0

  conductorflag := 1
  currqbeat     := 0
  
  //writef("barscan: barsxv=%n*n", barsxv)
  //abort(5522)

  UNLESS h2!block=s_block DO
  { writef("barscan: SYSTEM ERROR: *
           *The conductor part does not start with a block*n")
    abort(999)
  }
  barscanitem(block) // Scan the Block
}

AND barscanitem(t) BE IF t DO
{ // This scans any node in the conductor part to fill in the
  // elements of barsxv.

  LET op = h2!t
  LET ln = h3!t

//writef("barscanitem: *
//       *currqbeat=%n currbarno=%n currbarqbeat=%n op=%s ln=%n/%n*n", 
//        currqbeat, currbarno, currbarqbeat, opstr(op), fno(ln), lno(ln))
//abort(9439)

  SWITCHON op INTO
  { DEFAULT: // Ignore all the other tree nodes
      RETURN

    CASE s_block:         //  [0, Block, ln, notes, qlen, parent,
      currln := h3!t      //      qbeat, envs, qshiftv, tupnode]
      UNLESS conductorflag=1 DO
      { writef("Error: Inner blocks are not allowed in the conductor part*n")
        abort(999)
      }
      conductorflag := 2 // To disallow inner blocks in the conductor part
      barscanitem(h4!t)
      RETURN

    CASE s_notes:    // [-, Notes, ln, notelist, qlen]
    { LET list = h4!t
//writef("barscanitem: %i6: %s qlen=%n*n", currqbeat,  opstr(op), h5!t)
//abort(9440)
      WHILE list DO
      {
        //writef("barscanitem: op=%s currqbeat=%n*n",opstr(h2!list), currqbeat)
        barscanitem(list) // Scan each item in the note list
        list := h1!list
      }
//abort(9441)
      RETURN
    }

    CASE s_name: // t -> [-, Name, ln, str]
      currpartname := h4!t
//writef("barscanitem: %i5: Name %s*n", currqbeat, currpartname)
      RETURN

    CASE s_tuplet: // t -> [-, Tuplet, ln, notes, qlen, parent, qbeat, toqlen]
      currln := h3!t
      trerr("\tuple is not permitted in the conductor's part")
      abort(999)
      RETURN

    CASE s_par:
      currln := h3!t
      trerr("\par is not permitted in the conductor's part")
      RETURN

    CASE s_note:
    CASE s_notetied:
      currln := h3!t
      trerr("Notes are not permitted in the conductor's part")
      // Treat as a space or rest.

    CASE s_space:
    CASE s_rest:
      currqbeat := currqbeat+h4!t
      maxqbeat := currqbeat
      RETURN

    CASE s_null:
//writef("barscanitem: currqbeat=%i5: %s*n", currqbeat, opstr(h2!t))
      RETURN
      
    CASE s_tempo:
    CASE s_volmap:

    CASE s_delay:
    CASE s_legato:
    CASE s_vibamp:
    CASE s_vibrate:
    CASE s_vol:

    CASE s_delayadj:
    CASE s_legatoadj:
    CASE s_vibampadj:   // [-, op, ln, notes, shape]
    CASE s_vibrateadj:
    CASE s_voladj:
      // barscan only looks at the left operand of these shape constructs. 
//writef("barscanitem: %i5: %s*n", currqbeat, opstr(h2!t))
//abort(4447)
      currln := h3!t
      barscanitem(h4!t)
      RETURN

    CASE s_barline:
    CASE s_doublebar:
    CASE s_repeatback:
    CASE s_repeatbackforward:
    CASE s_repeatforward:
      currln := h3!t
      // barline must occur at qbeat position currbarqbeat+qbeatsperbar
      // unless it is the first barline when it can occur earlier.
      // barline always increments the bar number.
      // If a doublebar or repeat barline occurs at a valid barline
      // position the bar number is incremented, but these bar lines
      // also also permitted within bars.
      // currbarqbeat is -1 or holds the qbeat position of the current
      // bar.

      IF currbarqbeat<0 DO
      { // This is the first barline.
        // barlines, doublebars and repeat barlines are allowed here.
	IF currqbeat >= qbeatsperbar DO
	{ // The first bar is complete so bar zero does not exit.
          pushival(barsxv, 0)         // Start of bar one.
          pushival(barsxv, currqbeat) // Start of bar two.
          currbarqbeat := currqbeat
	  currbarno := 2
	  maxbarno := currbarno
 	  IF currqbeat > qbeatsperbar DO
	    synerr("The first bar is too long")
	  RETURN
	}

	IF currqbeat=0 DO
	{ trerr("The conductor part must not start with a barline")
	  RETURN
	}

	// Bar zero does exit
        pushival(barsxv, currqbeat)
        currbarqbeat := currqbeat  // Start of the first full bar.
        currbarno := 1
        maxbarno := currbarno
	RETURN
      }

      // This is not the first barline

      IF currqbeat = currbarqbeat+qbeatsperbar DO
      { // If the barline is at a valid position increment currbarno.
        currbarqbeat := currqbeat
        currbarno := currbarno+1
	maxbarno := currbarno
        pushival(barsxv, currqbeat) // Start of a new bar.
        RETURN
      }

      // The bar line was not at a valid position.
      IF op=s_barline
      { // This bar line is not at a valid position.
        LET qbeaterror = currqbeat - (currbarqbeat+qbeatsperbar)
	TEST qbeaterror>0
        THEN trerr("Bar %n is %n qbeats too long", currbarno, qbeaterror)
        ELSE trerr("Bar %n is %n qbeats too short", currbarno, -qbeaterror)
	currbarqbeat := currqbeat
        pushival(barsxv, currqbeat) // Start of a new bar.
        currbarno := currbarno+1
	RETURN
      }
      // Double bars and repeat bars can occur anywhere
      RETURN
    
    CASE s_timesig:   // [-, Timesig, ln, sig_t, sig_b]
    { // A time signature may only occur at the start of the
      // composition or just after a barline.

//writef("barscanitem: %s(%n, %n) at currqbeat=%n*n",
//        opstr(h2!t), h4!t, h5!t, currqbeat)
      currln := h3!t

      UNLESS currbarqbeat<0 | currqbeat=currbarqbeat DO
        synerr("Time signature not at the start of a bar")

      // timesig_b is the length number with 4 corresponding to
      // a quarter note, 8 to a quaver etc and timesig_t is the
      // number of these notes per bar.

      // 6/8 bars are normally conducted with two beats per bar
      // and so are probably best represented by \timesig(2 s4.)

      timesig_t, timesig_b := h4!t, h5!t

      qbeatsperbeat := 4096/timesig_b
      qbeatsperbar := qbeatsperbeat * timesig_t
//writef("barscanitem: qbeatsperbeat=%n qbeatsperbar=%n*n",
//        qbeatsperbeat, qbeatsperbar)
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
{ // Allocate a vector of the right size and copy
  // the elements of sxv!1 into it. Then use freevec to free sxv!1 and
  // update sxv!1 the new vector. If the size of the new vector is
  // larger than 100 it is allocaled using getvec, otherwise it is
  // allocated using newvec and set sxv!0 to zero to stop freeing
  // the vector using freevec. This technique is used since there is
  // a limit to the size of vectors that newvec can allocate.
  LET oldv = sxv!1
  LET upb  = oldv!0
  LET v = 0
  TEST upb<100
  THEN v := newvec(upb)
  ELSE v, sxv!0 := getvec(upb), 0
  FOR i = 0 TO upb DO v!i := oldv!i
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
// mapped to absolute values usin the call q!absq(qbeat).

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

LET pushival(sxv, x) BE
{ // sxv-> [upb, v]
  // Initially both upb and v are zero, but on the first call of
  // pushival upb is given an initial size, typically 10 and the
  // vector v is allocated using getvec(upb).
  pushval(sxv, x)
  IF debugv!1 DO
  { LET v = sxv!1
    LET p = h1!v
    writef("pushival: sxv=%n updating v[%i3] with ", sxv, p)
    TEST -1_000_000 < x < 1_000_000
    THEN writef("%n*n",   x)
    ELSE writef("#%x8*n", x)
  }
  //abort(12244)
}

AND pushfval(sxv, FLT x) BE
{ pushval(sxv, x)
  IF debugv!1 DO
  { LET v = sxv!1
    LET p = h1!v
    writef("pushfval: sxv=%n updating v[%i3] with %5.2f*n", sxv, p, x)
  }
}

AND pushxval(sxv, x) BE
{ pushval(sxv, x)
  IF debugv!1 DO
  { LET v = sxv!1
    LET p = h1!v
    writef("pushfval: sxv=%n updating v[%i3] with %8x*n", sxv, p, x)
  }
}

AND pushfpair(sxv, q, x) BE
{ //writef("pushfpair: sxv!1=%n q=%n x=%6.3f*n", sxv!1, q, x)
  IF sxv!1=0 & q>0 DO pushipair(sxv, 0, starval)
  pushival(sxv, q)
  pushfval(sxv, x)
  //writef("pushfpair: sxv!1 now = %n*n", sxv!1)
//abort(70000)
}

AND pushipair(sxv, q, x) BE
{ //writef("pushipair: sxv!1=%n q=%n x=%8x*n", sxv!1, q, x)
  // Deal with a possibly empty bar zero.
//abort(70001)
  IF sxv!1=0 & q>0 DO pushipair(sxv, 0, starval)
  pushival(sxv, q)
  pushival(sxv, x)
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
  // Note that the actual values in v start at subscript one.
  
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
    LET newupb = 5*upb/4 + 10 // Increase the size by about 1/4.
                              // On average about 1/8th of the vector
			      // will be unused.
    LET newv = getvec(newupb)
//writef("pushval: allocating vector %i6 upb %n*n", newv, newupb)
//abort(2222)
    UNLESS newv DO
    { trerr("More memory needed")
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
    { writef("pushval: sxv=%n replacing v=%i5 upb=%n *
             *with newv=%i5 upb=%n p=%n*n",
              sxv, v, upb, newv, newupb, p)
      abort(6666)
    }
    v := newv
  }
  // Update v!0 and push val into the vector
  p := p+1
  v!0, v!p := p, val
  //writef("p=%i5   val= %i5*n", p, val)
  // v!0=p the subscript of the latest value pushed into v.
}

AND pushshape(env, q, x) BE
{ // env -> [-, op, ln, parent, upb, v, absq1, absq2]
  // where op is a shape enviroment operator such as Volenv.
  // qlen is length of the Block that owns this environment.
  // This function pushes the shape entry into the shape
  // environment. It uses q2blkq to find the 
  LET blkq = q2blkq(q, currtuplet)
  LET sxv = @h5!env

//writef("pushshape: q=%n => blkq=%n qlen=%n x=%7.3f*n", q, blkq, qlen, x)
  pushfpair(sxv, blkq, x)
  // The latest entry may not be in sorted order.
//abort(2841)
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

  LET a = 0

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

    
    trerr("Unresolvable tied note %s", str)
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

AND absq2barno(absq) = VALOF
{ LET n = absq2barno1(absq)
  LET res2 = result2
  //writef("absq2barno(%n) => %n, result2=%n*n", absq, n, res2)
  result2 := res2
  RESULTIS n
}

AND absq2barno1(absq) = VALOF
{ // Return the number of the bar containing the given absolute
  // qbeat location.
  // result2 is set the the the number of qbeats absq is after
  // the start of the bar.
  // We currently use the simplest algorithm.
  LET v = barsxv!1
  // v=0
  // or
  // v -> [n,q1,q2,...,qn]
  // q1 is the start of the first full bar.
  
  // If q1=0 bar 0 does not exist and bar 1 starts at qbeat q1=0
  
  // If q1~=0 bar zero does exist and starts at qbeat 0 and
  // the bar 1, the first full bar, starts at qbeat q1.
  // Note in either case the bar 1 starts at qbeat q1, and
  // q1 is at subcript position 1 of v.

  UNLESS v DO
  { trerr("absq2barno: System error: No bar table yet")
    result2 := absq
    RESULTIS 0
  }

  //abort(1000)

  // absq>=v!1
  TEST v!1
  THEN { // Bar zero exists and does not contain absq.
         IF absq < v!1 DO
         { // absq is within bar zero.
           result2 := absq
           RESULTIS 0
         }
         FOR i = 1 TO v!0 IF absq < v!i DO 
         { LET n = i-1 // The bar >= 1 containing absq.
	   result2 := absq - v!n
           RESULTIS n
         }
       }
  ELSE { // Bar zero does not exist but it does not contain absq.
         FOR n = 1 TO v!0-1 IF absq < v!(n+1) DO
         { result2 := absq - v!n
           RESULTIS n
         }
       }
  
  // Otherwise return the maximum bar number.
  result2:= absq - v!(v!0)
  RESULTIS v!0 // Max bar number.
}

AND q2blkq(q, tuplet) = VALOF
{ // Apply the tuplets in the chain pointed to by tuplet.
  // This is used when adding shape data to environments in a
  // block. It is also used by q2absq and q2msecs.

  WHILE tuplet DO
  { // tuplet -> [-, Tuplet, ln, notes, qlen, parent, qbeat, toqlen]
    LET qlen   = h5!tuplet
    LET parent = h6!tuplet
    LET qbeat  = h7!tuplet
    LET toqlen = h8!tuplet
    //writef("q2blkq: tuplet=%n q=%n qlen=%n *
    //       *parent=%n qbeat=%n toqlen=%n*n",
    //        tuplet, q, qlen, parent, qbeat, toqlen)
    //abort(6823)

    q := qbeat + muldiv(q, toqlen, qlen)
    tuplet := parent
  }
  
  RESULTIS q
}

AND q2absq(q) = VALOF
{ // Return the absolute qbeat location corresponding to the
  // given local qbeat based of the current tuplet and block,
  // ignoring the effect of tempo data in blocks.
  // This is used, for instance, when checking that a barline
  // in a part or solo is correctly positioned.
  LET block  = currblock
  LET tuplet = currtuplet  
  //writef("*nq2absq: q=%n currblock=%n currtuplet=%n*n",
  //         q, currblock, currtuplet)

  WHILE block DO
  { //writef("q2absq: block=%n tuplet=%n q=%n*n", block, tuplet, q)
    
    // First apply the tuplet chain, if any.
    q := q2blkq(q, tuplet)
    //writef("q2absq: after calling q2blkq q=%n*n", q)  
    // block -> [0, Block,  ln, notes, qlen, parent,
    //              qbeat, envs, qshiftv, tupnode]

    q := q + h7!block   // Find the local qbeat location in
                        // the enclosing Tuplet or Block.
    tuplet := h10!block // Find the enclosing Tuplet, if any. 
    block  := h6!block  // Find the enclosing Block, if any. 
  }

//writef("q2absq: block=%n tuplet=%n q=%n*n", block, tuplet, q)
  q := q2blkq(q, tuplet)
  //writef("q2absq: after calling q2blkq q=%n*n", q)  
//abort(1000)  
  // q is now the required absolute location ignoring
  // ignoring the effect of Tempo data in inner Blocks.
  // Ensure that it with the range of the score.
  //IF q<0 DO q := 0
  //IF q>scoreqlen DO q := scoreqlen
  //writef("q2absq: returning q=%n*n", q)
  //abort(66223)
  RESULTIS q
}

AND q2msecs(q) = VALOF
{ // Return the time in msecs corresponding to local location q
  // based on the current tuplet and block, This is used by genmidi
  // as it generates midi events for parts and solos.
  // This function can only be used when currblock and currtuplet
  // are correctly set.

  LET block  = currblock
  LET tuplet = currtuplet  

  WHILE block DO
  { // First apply the tuplet chain, if any.
    q := q2blkq(q, tuplet) // Find the local qbeat location
                           // in the Block.
    
    // block -> [0, Block,  ln, notes, qlen, parent,
    //              qbeat, envs, qshiftv, tupnode]

    // Apply qshiftv unless in theconductor's Block
    IF h6!block DO
    { LET qshiftv = h9!block
      IF qshiftv DO q := qshiftv!q
    }

    q := q + h7!block   // Find the local qbeat location in
                        // the enclosing Tuplet or Block.
    tuplet := h10!block // Find the enclosing Tuplet, if any. 
    block  := h6!block  // Find the enclosing Block, if any. 
  }

  // q is now the required absolute location.
  // Ensure that it with the range of the score.
  IF q<0 DO q := 0
  IF q>scoreqlen DO q := scoreqlen
  RESULTIS msecsv!q     // Return the time in msecs
}

AND genmidi(t) BE
{ // t is the node in the parse tree to translate into midi events.
  // currtuplet  holds is zero points to the current Tuplet node.
  // currblock holds the current Block. Thse are needed since
  // q2absq and q2msecs use them.
  // currqbeat holds the current local qbeat.
  // The time of this qbeat is obtained by calling q2msecs(currqbeat).
  // The initial call of genmidi(tree) is from start. This is called
  // after calcqlen, barscan, findrawshapes and setshapes have all
  // been called.

  // The environments of shape data are held in global variables such
  // as volenv and tempoenv. They all have the same structure.

  // eg volenv -> [envlist, op, ln, parent, upb, v, absq1, absq2]

  // where
  //    upb is zero or the upper bound of v (=absq2-absq1)
  //    v  =  0
  //    or -> [2n, q1, x1,... qn, xn] where
  //       q1 .. qn are absolute qbeat locations and
  //       x1 .. xn are the corresponding floating point shape values.
  //    absq1, absq2 are the absulute start and end of the region
  //       covered by this environment.

  // tlist and clist hold the lists of the current outstanding
  // note ties.

  // numbered from zero to the qlen of the context. Contexts can be
  // nested when, for instance, a Tuplet node occurs within a Block.
  // The structure of a context node is as follows.

  // Both Block and Tuplet nodes have parent fields that point to the
  // nearest enclosing context node. The parent field is zero for the
  // Block in the conductor part. The local qbeat locations of the
  // conductor part are called absolute locations and are sometimes
  // given names such as absq or absq1. Context nodes have a field
  // named qbeat giving its location as a local qbeat of the parent
  // context.
  // The field envs in Blocks holds the list of environment nodes
  // belonging to it but only of the types of shape data occurring
  // in the Block, This list is created by formtree but the data is
  // filled in later by findrawshapes. In parts and solos the field
  // qshiftv is only non zero if the block contains Tempo shape data.
  // The effect of this data is to modify the speed of performance of
  // the block by mapping its qbeat postions forward or back relative
  // to the locations of the enclosing context. These shifts are
  // scaled so that the first and last qbeats are not shifted. This
  // ensure that the total performance time of the block is the same
  // as if no tempo data was given.

  // There are two functions that perform scaling of local qbeat values.

  // q2blkq(q,tuplet) returns the local qbeat location of q
  // in the block enclosing the given tuplet. This is used when
  // setting environment data values or looking them up.

  // q2absq(q) returns the absolute location corresponding to the
  // given local qbeat location in the current block and tuplet.
  // The main purpose of absolute qbeat locations is to allow the
  // timing of these qbeats to be determined as is needed when
  // generating midi events.
  
  // Midi data is held as items in the list midilist. Its last item
  // is pointed to by midiliste. If the list is empty midilist=0 and
  // midiliste points to midilist. Each item in the list has the form
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

  IF FALSE DO
  IF optNtrace DO
  { LET absq  = q2absq(currqbeat)  // Scaled qbeat number
//writef("genmidi: About to call prlocation(currqbeat)*n")
    prlocation(currqbeat)
    writef("%10t ", opname)
    writef("tempo=%i3 ",  FIX shapeval(s_tempo,  tempoenv,  absq))
    writef("delay=%i3 ",  FIX shapeval(s_delay,  delayenv,  absq))
    writef("vol=%i3 ",    FIX shapeval(s_vol,    volenv,    absq))
    writef("legato=%i3 ", FIX shapeval(s_legato, legatoenv, absq))
    prlineno(ln)
    newline()
  //abort(1003)
  }

IF FALSE DO
{ writef("genmidi: t=%n op=%s currqbeat=%n currbarno=%n *
         *currblock=%n currtuplet=%n*n",
          t, opstr(op), currqbeat, currbarno, currblock, currtuplet)
  abort(1004)
}

//writef("genmidi: t=%n op=%s*n", t, opstr(op))
  SWITCHON op INTO      // In genmidi
  { DEFAULT:
      // Ignore most node types
      //writef("genmidi: t=%n Ignoring op=%s currqbeat=%n currbarno=%n*n",
      //        t, opname, currqbeat, currbarno)
      //abort(81004)
      RETURN

    CASE s_score:
    { // t         -> [-, Score, ln, name, conductor, parts, qlen]
      // conductor -> [-, Conductor, ln, block, qlen]
      // parts     -> [-, Parts, ln, partlist, qlen]
      LET parts = h6!t  // -> [-. Parts, ln, partlist, qlen]
      genmidi(parts)
      RETURN            // Return from genmidi
    }
    
    CASE s_parts:       // t -> [-. Parts, ln, partlist, qlen]
//    abort(229933)
    { LET partlist = h4!t
      // partlist  =  0
      // or        -> [partlist, Part, ln, block, qlen]
      // or        -> [partlist, Solo, ln, block, qlen]
      // block     -> [-, Block,ln,notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
      //           -> [-, Notes,ln,notelist,qlen]
      // envs      -> [0, Envs, ln, envlist]
      // envlist   -> [envlist, op, ln, parent, upb, v, absq1, absq2]
      // or        -> 0
      // where op is an environment name such as s_volenv.
      // parent is zero or points to the enclosing environment of the
      //          same type.
      // The pair [upb,v] is a self expanding vector to hold the
      //          shape data.
      // upb is the upper bound of v and v!0 holds the subscript of
      //          the last element used in v. Items in v are pairs
      //          [absq,val] starting at subscript 1

      midichannel := -1   // The first allocated channel will be zero.
      WHILE partlist DO
      { currqbeat := 0
        currbarno := 0
        variablevol := FALSE

        tlist, tqpos := 0, 0 // Initialising the tie mechanism for
        plist, pqpos := 0, 0 // this part.
        clist, cqpos := 0, 0

        genmidi(partlist)
        partlist := h1!partlist
      }
      RETURN                          // Return from genmidi
    }

    CASE s_part:      // [-, Part,      ln, block,  qlen]
    CASE s_solo:      // [-, Solo,      ln, block,  qlen]
      clearenvironments()

      // Update the global environments of all types found in conductorpart.
      updateenvironments(conductorenvs)

      currpartname := 0
      currqbeat   := 0
      currbarno   := 0
      currblock   := conductorblock
      currtuplet  := 0
      variablevol := FALSE
      lineno      := h3!t

      tlist, tqpos := 0, 0 // Initialising the tie mechanism for
      plist, pqpos := 0, 0 // this part.
      clist, cqpos := 0, 0
  
      //writef("genmidi: %s*n", opname)
      IF midichannel>=15 DO
        writef("Error: No more than 16 parts are allowed*n")

      // Choose next midi channel (avoiding 9 -- GM percussion)
      // midichannel will be in range 0 to 15
      midichannel := midichannel + 1 REPEATWHILE midichannel=9
      h6!t := midichannel
      chanvol := -1
//writef("genmidi: Allocated channel %n to a part or solo*n", midichannel)
//abort(1993)

      // Allow more than one solo part
      IF h2!t=s_solo & midichannel>=0 DO solochannels := 1<<midichannel

      transposition := 0 // No transposition specified yet
      currqbeat := 0
      currbarno := 0

//      trerr("Test err mess in genmidi")

      genmidi(h4!t)

      // Check that there are no outstanding ties.
//writef("genmidi: just finished generating a part or solo*n")
//prties()
//writef("genmidi: checking that there are no outstanding ties*n")
      //checktlist(envblk)
//prties()
//abort(1178)
      RETURN

    CASE s_block:
    // t -> [-, Block, ln, notes,qlen,parent,qbeat,envs,qshiftv,tupnode]
    { LET op      = h2!t  // op = s_block
      LET ln      = h3!t
      LET notes   = h4!t
      LET qlen    = h5!t
      LET parent  = h6!t  // The enclosing context
      LET qbeat   = h7!t
      LET envs    = h8!t  // envs in a Block can only be zero in the
                          // conductor part.
      LET qshiftv = h9!t  // = 0 if no Tempo data
                          // =-1 if Tempo data is present

      currqbeat  := 0
      currtuplet := 0
      currblock  := t
      
      // Environments hold data specifying a piecewise linear graphs
      // that are used when calculating a shape value such as the
      // tempo or volume at a specified absolute qbeat location. All
      // shape entries are pairs (absq,val) where absq is the absolute
      // location and val is a usually a floating point number. A
      // particular value is the bit pattern shapestar=#x12344321
      // is used to represent the star value.

      //writef("genmidi: t=%n op=%s qlen=%n parent=%n *
      //       *qbeat=%n envs=%n qshift=%n*n",
      //        t, opstr(op), qlen, parent, qbeat, envs, qshiftv)
      // Setup the new shape environments needed by this block.
      updateenvironments(envs)
      //abort(1293)

      UNLESS h2!notes=s_notes DO
      { writef("genmidi: System error: notes expected*n")
        abort(999)
      }

      //writef("calling genmidi(%n)*n", t)
      genmidi(notes)
      //prshapes()
      //abort(7748)

      // Add a star at the end of every shape environment of
      // the block where necessary.
      // envs    =  0
      //         -> [0, Envs, ln, envlist]
      // envlist =  0
      //         -> [envlist, op, ln, parent, upb, v, absq1, absq2]

      // If the block belongs to the conductor part or contains Tempo
      // data, a shift vector is allocated and fill it with approriate
      // values. For the conductor part these values are absolute qbeat
      // locations but for parts and solos they are qbeat location
      // shifts scaled to ensure that the total qlen of the block is
      // unchanged by the tempo shape data.

      // Restore the previous shape environment vectors
      restoreenvironments(conductorenvs)
      IF currtuplet DO currtuplet := h6!currtuplet
      currblock := h6!currblock
      RETURN                 // Return from genmidi
    }
                                                                                
    CASE s_name:
      currpartname := h4!t
      IF optNtrace DO
      { prlocation(currqbeat)
        writef("%10t  %s*n", opname, currpartname)
      }
//abort(1005)
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
    { LET absq = q2absq(currqbeat)
      LET dly = FIX shapeval(s_delay, delayenv, absq)
      LET midimsecs = absq2msecs(absq) + dly
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

    CASE s_note:      // t -> [-, Note,     ln, <letter:sharps:dots:n>, qlen]
    CASE s_notetied:  // t -> [-, Notetied, ln, <letter:sharps:dots:n>, qlen]
//writef("genmidi: t=%n op=%s*n", t, opstr(h2!t))
//abort(1006)
    { LET note   =  h4!t
      LET n      =  note      & 255
      LET sharps = (note>> 8) & 255
      LET letter = (note>>16) & 255
      // Find the requires Midi note number
      LET midi_note  = n + transposition + pitch  // The transposed note
      LET qlen   = h5!t // Nominal qlen of the note in qbeats

      LET nomqs  = q2absq(currqbeat)      // Nominal start abs qbeat
      LET nomqe  = q2absq(currqbeat+qlen) // Nominal end

//writef("genmidi: t=%n op=%s  n=%n transposition=%n pitch=%n => %n*n",
//                 t, opstr(h2!t), n, transposition, pitch, midi_note)
//writef("genmidi: currqbeat=%n qlen=%n currtuplet=%n*n", currqbeat, qlen, currtuplet)
//writef("genmidi: nomqs=%n nomqe=%n*n", nomqs, nomqe)
//abort(3219)
//IF FALSE DO
      UNLESS istied(midi_note, nomqs) DO
      { LET absqs     = q2absq(currqbeat)
        // This note does not resolve a previous tie, so play it.
        LET dly       = FIX shapeval(s_delay,  delayenv,  nomqs)
        LET legato    = FIX shapeval(s_legato, legatoenv, absqs)
        
        LET midimsecs = absq2msecs(absqs) + dly
        LET chan      = midichannel // chan is in range 0 to 15

        // The +1 is a fudge at the moment to avoid a problem
        // when two different shape values are at the same
        // qbeat position. May be this should not be allowed
        // to happen.
        LET vol = FIX (shapeval(s_vol, volenv, absqs))
//writef("genmidi: Note currqbeat=%n vol=%n*n", currqbeat, vol)
//abort(45678)
        // Scale volume 0 to 100_000 to midi range 0 to 127
        //vol := (vol * 127) / 100
        //IF vol>127 DO vol := 127
        //IF vol<0   DO vol := 0

//writef("*ngenmidi: msecs:%i6 %t8  ", midimsecs, opname)
//prnote(letter, sharps, dots, n, qlen)
//writef(" vol=%n legato=%n*n", vol, legato)
//writef("currqbeat=%n to %n delay=%n*n", currqbeat, currqbeat+qlen, dly)
//abort(3220)
        //IF transposition DO
        //  writef("getmidi: Transposition=%n note %i3 transposed to %i3*n  ",
	//          transposition, n, midi_note)

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
               { LET dly = FIX shapeval(s_delay, delayenv, q)
                 LET legato = FIX shapeval(s_legato, legatoenv, q)
                 LET absqs = q2absq(q+dly)
                 LET midimsecs = absq2msecs(absqs)
                 LET vol = FIX shapeval(s_vol, volenv, q+1)

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
	       //abort(1011)
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
            LET absqs = q2absq(currqbeat) // Nominal start
//writef("genmidi: The note is tied*n")
            tqpos := q2absq(currqbeat+qlen) // Nominal end
            tlist := mk3(0, midi_note, absqs)
//writef("genmidi: note=%n currqbeat=%n absqs=%n tqpos=%n in tlist*n",
//          midi_note, currqbeat, absqs, tqpos)
            //prties()
          }
     ELSE { // This note is not tied to a later one,
            // so schedule a note off command.
            // The legatoness of a note is determined at its start.
            LET leg       = FIX shapeval(s_legato, legatoenv, currqbeat)
            LET qe        = currqbeat + (qlen * leg) / 100
            LET dly       = FIX shapeval(s_delay, delayenv, qe)
            LET absqe     = q2absq(qe+dly)
            LET midimsecs = absq2msecs(absqe)

//writef("%i6: Note off: chan=%n note=%i3  legato=%n*n",
//       midimsecs, midichannel, n, leg)
//abort(1007)
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
//abort(1008)
      RETURN
    }

    CASE s_transposition:
      transposition := h4!t
//writef("genmidi: transposition set to %n*n", transposition)
      RETURN


    CASE s_bank:
//writef("CASE s_bank:*n")
    { LET absq = q2absq(currqbeat)
      LET dly = FIX shapeval(s_delay, delayenv, absq)
      LET midimsecs = absq2msecs(absq) + dly

      apmidi(midimsecs,                                   // Msecs
             midi_control+midichannel+(0<<8)+(h4!t<<16))  // Bank MSB
      apmidi(midimsecs,                                   // Msecs
             midi_control+midichannel+(32<<8)+(h5!t<<16)) // Bank LSB
      IF optNtrace DO
      { prlocation(currqbeat)
        writef("%10t  chan=%n MSB=%n*n", "Bank", midichannel, h4!t)
	prlocation(currqbeat)
        writef("%10t  chan=%n LSB=%n*n", "Bank", midichannel, h5!t)
      }
      RETURN
    }

    CASE s_patch:
//writef("CASE s_patch:*n")
    { LET absq = q2absq(currqbeat)
      LET dly = FIX shapeval(s_delay, delayenv, absq)
      LET midimsecs = absq2msecs(absq) + dly

      apmidi(midimsecs,                              // Msecs
             midi_progchange+midichannel+(h4!t<<8))  // Patch command
      IF optNtrace DO
      { prlocation(currqbeat)
        writef("%10t  chan=%n prog=%n*n", "Patch", midichannel, h4!t)
      }
      RETURN
    }

    CASE s_tempo:      // [-, tempo,     ln, notelist, shapelist, qlen]
    CASE s_volmap:     // [-, volmap,    ln, notelist, shapelist, qlen]

    CASE s_delay:      // [-, delay,     ln, notelist, shapelist, qlen]
    CASE s_legato:     // [-, legato,    ln, notelist, shapelist, qlen]
    CASE s_vibamp:     // [-, vibamp,    ln, notelist, shapelist, qlen]
    CASE s_vibrate:    // [-, vibrate,   ln, notelist, shapelist, qlen]
    CASE s_vol:        // [-, vol,       ln, notelist, shapelist, qlen]

    CASE s_delayadj:   // [-, delayadj,  ln, notelist, shapelist, qlen]
    CASE s_legatoadj:  // [-, legatoadj, ln, notelist, shapelist, qlen]
    CASE s_vibampadj:  // [-, vibampadj, ln, notelist, shapelist, qlen]
    CASE s_vibrateadj: // [-, vibrateadj,ln, notelist, shapelist, qlen]
    CASE s_voladj:     // [-, voladj,    ln, notelist, shapelist, qlen]
      // The shape data has already been extracted and is strored
      // in the environment vectors.
      genmidi(h4!t)
      RETURN

    CASE s_notes:       // [-, Notes, ln, list, qlen]
    { LET list = h4!t
      //writef("genmidi: %s  ", opname); prlineno(ln); newline()
      WHILE list DO
      { //writef("genmidi: calling genmidi(list) list=%n*n", list)
      //abort(924834)
        genmidi(list)
        list := !list
      }
      RETURN
    }

    CASE s_barline:
    CASE s_doublebar:
    { // Check that the barline occurs at the right place.
      LET absq = q2absq(currqbeat)
      LET v = barsxv!1 // v!1 to v!maxbarno are the qbeat locations
                       // of every barline.

      currbarno := currbarno+1
      //writef("genmidi: currbarno incremented to %n*n", currbarno)
      
      IF currbarno>maxbarno DO      
      { trerr("Part %s has more barlines than the conductor part",
                   currpartname)
        RETURN
      }
      
      UNLESS absq=v!currbarno DO
      { // There is an error.
        LET diff = currqbeat - v!currbarno
        //trerr("Misplaced barline, %n qbeats too %s*n",
        //           ABS diff, diff<0 -> "early", "late")
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
      LET absq0 = q2absq(q0)
      LET absq1 = q2absq(q1)
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
        genmidi(h4!t)
        //writef("genmidi: op=%s <%n/%n> finished par element %n*n",
        //        opname, fno(ln), lno(ln), count)
        //prties()

        tokln := h3!(h4!t)

        UNLESS currqbeat-q0 = qlen DO
        { TEST h2!(h4!t)=s_part | h2!(h4!t)=s_solo
          THEN { LET bn1 = absq2barno(currqbeat)
                 LET bn2 = absq2barno(q0+qlen)
                 LET qerr = q0 + qlen - currqbeat
                 trerr(
                   "Part ends %n qbeats early in bar %n, final bar is %n*n",
                   qerr, bn1, bn2)
               }
          ELSE { trerr(
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
    { // t -> [-, Tuplet, ln, notes, qlen, parent, qbeat, toqlen]

      LET notes         = h4!t
      LET qlen          = h5!t   // qlen of notes
      LET parent        = h6!t
      LET qbeat         = h7!t   // The local qbeat o this tuplet in parent.
      LET toqlen        = h8!t   // Already set from the right hand operand.
      LET notelist      = h4!notes
      LET prevcontext   = currtuplet
      LET prevcurrqbeat = currqbeat
      
      h5!t := h5!notes    // qlen of notes
      h6!t := currtuplet  // Set the parent of this tuplet node.
      h7!t := currqbeat   // The Tuplet is a new context.

      currtuplet := t
      currqbeat  := 0     // Start the new context

//writef("genmidi: %s <%n/%n> saved clist  ", opstr(op)); prlineno(ln)
// newline()

      genmidi(notes) // Apply genmidi to the notelist

      currqbeat := prevcurrqbeat + toqlen
      currtuplet := prevcontext
      
//writef("Leaving Tuplet construct with currqbeat=%n*n", currqbeat)
//prties()
      RETURN
    }
  }
}


AND apmidi(msecs, code) BE
{ // Append a node onto the end of the midi list
  // msecs is the time in msecs and
  // code is a midi duplet (op, a) or triplet (op, a, b) of bytes
  //      code = op + (a<<8) + (b<<16)
  // and update end_msecs if necessary.
  LET node = mk3(0, msecs, code)
  !midiliste := node
  midiliste := node
  IF msecs>end_msecs DO end_msecs := msecs
  //writef("apmidi: msecs=%i6 %x8  end_msecs=%n*n", msecs, code, end_msecs)
}

AND absq2msecs(absq) = VALOF
{ //writef("absq2msecs: absq=%n msecsv=%n*n", absq, msecsv)
//  abort(323245)
  // Note that the bounds of msecsv are 0 and scoreqlen.
  IF absq<=0 RESULTIS 0 // Note msecsv!0 is the upb of msecsv
  IF absq>scoreqlen DO absq := scoreqlen
  //writef("absq2msecs: absq=%n => %6.3d*n", absq, msecsv!absq)
  //abort(7632)
  RESULTIS msecsv!absq
}

AND barno2absq(bno) = VALOF
{
//abort(19242)
  IF bno <= 0 RESULTIS 0
  
  IF bno > maxbarno DO
    trerr("barno2absq: System error, bno=%n maxbarno=%n*n",
          bno, maxbarno)

  RESULTIS barno2absqv!bno
}

AND barno2msecs(bno) = VALOF
{ // bno is a barline number.
//writef("barno2msecs: bno=%n msecsv=%n*n", bno, msecsv)
//abort(19983)
{ LET absq  = barno2absq(bno)
  LET msecs = absq2msecs(absq)
//writef("*nbarno2msecs: bno=%n => q=%n => result=%n msecs*n", bno, absq, msecs)
//abort(1003)
  RESULTIS msecs
}
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
    { trerr("More memory needed")
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
//writef("midilist=%n*n", midilist)
  WHILE midilist DO
  { // midilist = 0 or 
    //          -> [next, midimsec, triple]
    LET midimsecs = midilist!1
    LET triple    = midilist!2
    LET op, a1, a2, is_note_on = ?, ?, ?, ?
    LET fn, chan = ?, ?
    LET playing = ?
    LET dr, r = 0, 0

//writef("midimsecs=%n stop_msecs=%n*n", midimsecs, stop_msecs)

    IF midimsecs>stop_msecs BREAK
    midilist := !midilist

    op :=  triple      & 255
    a1 := (triple>>8)  & 255
    a2 := (triple>>16) & 255
    fn := op & #xF0     // Midi op without the channel number
    is_note_on := fn = midi_note_on
    chan := op & #x0F   // The midi channel number
    //playing := start_msecs<=midimsecs<=end_msecs

//writef("%i7: midi op: %x2 %x2 %x2*n", 999, op, a1, a2)
//abort(5595)

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
  IF cb!0 DO freevec(cb!1)  // Free the vector holding the Midi data
                            // unless it was allocated using newvec.
}

.

SECTION "Shapefns"

GET "libhdr"
GET "playmus.h"

/*
The tempo, volume and many other features of a performance can change
while it is played. These features are specified by shape values
that can be placed anywhere within a note sequence. A typical example
is as follows

              (4c4 d e f) \vol (50 s4 100 s2 75)

Here the note sequence plays four quarter notes c, d, e and f occupying
a total of 4096 qbeats. The shape data has length 3072 (=1024+2048)
specifying three volume values 50, 100 and 75. The length of the shape
data is scaled to be the same as the note sequence causing 50 to be
placed at qbeat zero, 100 at qbeat 1365 (=1024*4096/3072) and 75 at
4096. If the length of the shape sequence is zero, its shape value or
values are placed at the start of the note sequence.

The scope of shape data is the enclosing block. It forms a piecewise
linear graph of how the shape value changes throughout the block. The
shape data is held in the shape environment for this kind of shape.

If the shape had no explicit value at the start of a Block, a star is
inserted to give it the value from the enclosing environment at that
position.  Similarly a star is inserted at the end of a Block if there
is no explicit value there.

The shape value at a specified qbeat loction is determined by linear
interpolation based on the nearest two enclosing shape points in the
shape environment. If the reqired shape environment is empty, the
value is computed using the next enclosing block. If none of the
enclosing environments are non empty, the default value for that kind
of shape is used.

If the current shape environment and an enclosing shape environment
are both non empty then their values at the qbeat position are
combined by taking their average. A typical example is when the
conductor specifies a certain Vol setting and a soloist specifies
another. They both affect the volume value with equal weight.

Note that shape values in the current block are exactly the values
specified by the relevant shape sequence. The detailed evaluation of
shape values is not done until later when midi events are being
generated.

The Tempo shape is exceptional since it is used by
the conductor to construct a vector thatstructure that allows the time in msecs
of each absolute qbeat to be computed efficiently. Tempo values within
an inner block modifies the speed of playing within the inner block
but this speed is scaled to cause the inner block to take exactly the
same time as specified by the conductor. See the definition of the
function setmsecsenv for more information about how timing is computed.
*/

//LET FLT adjustedshapevalX(envtype, absq) = VALOF
LET adjustedshapevalX(envtype, absq) = VALOF
{ // This function returns the adjusted shape value at absq
  // in the current shape environment of the specified type.

  LET env, adjenv = 0, 0
  LET FLT res = ?
  
  SWITCHON envtype INTO
  { DEFAULT: writef("adjustedshapeval: System error, envtype=%s*n",
                    opstr(envtype))
             abort(999)
  	     RESULTIS 0

    CASE s_tempo:   env, adjenv := tempoenv,   0
                    ENDCASE
    CASE s_volmap:  env, adjenv := volmapenv,  0
                    ENDCASE

    CASE s_delay:   env, adjenv := delayenv,   delayadjenv
                    ENDCASE
    CASE s_legato:  env, adjenv := legatoenv,  legatoadjenv
                    ENDCASE
    CASE s_vibamp:  env, adjenv := vibampenv,  vibampadjenv
                    ENDCASE
    CASE s_vibrate: env, adjenv := vibrateenv, vibrateadjenv
                    ENDCASE
    CASE s_vol:     env, adjenv := volenv,     voladjenv
                    ENDCASE
  }
  // env =  0
  //        -> [env, op, ln, parent, upb, v, absq1, absq2]
  //        env is non zero
  //        op is the shape environment type eg Tempoenv.
  //        upb is the upperbound of v
  //        v!0=p and v!1 .. v!p contain the shape items
  //        of the form [q,val] for this shape.
  // absq1  is the start of the region
  // absq2  is the end of the region
  // envadj is the corresponding adjustment environment.
//writef("adjustedshapeval: About to call shapeval(%s %n %n)*n",
//        opstr(envtype), env, absq)
//abort(32324)
  res := shapeval(envtype, env, absq)
//writef("adjustedshapeval: shapeval(%s %n %n) => %9.2f*n",
//        opstr(envtype), env, absq, res)
//abort(32324)
  IF adjenv DO res := res * shapeval(envtype, adjenv, absq)
//writef("adjustedshapeval: Returning %s shape value %9.2f*n",
//        opstr(envtype), res)
//abort(32325)
  RESULTIS res
}

//AND FLT defaultshapeval(envtype) = VALOF
AND defaultshapeval(envtype) = VALOF
{ LET FLT res = defaultshapeval1(envtype)
  //writef("defaultshapeval(%s) => %6.2f*n", opstr(envtype), res)
  //abort(82823)
  RESULTIS res
}

//AND FLT defaultshapeval1(envtype) = VALOF SWITCHON envtype INTO
AND defaultshapeval1(envtype) = VALOF SWITCHON envtype INTO
{ // Return the default value of the given shape type.
  DEFAULT: writef("defaultshapeval: System error, envtype=%s*n",
                   opstr(envtype))
           abort(999)
	   RESULTIS 0

  CASE s_tempoenv:
  CASE s_tempo:      RESULTIS  60.0  // quarter notes per minute
  CASE s_volmapenv:
  CASE s_volmap:     RESULTIS  40.0  // Not used yet

  CASE s_delayenv:
  CASE s_delay:      RESULTIS   0.0  // Delay in absolute qbeats
  CASE s_legatoenv:
  CASE s_legato:     RESULTIS 100.0  // Percentage legato-ness
  CASE s_vibampenv:
  CASE s_vibamp:     RESULTIS   0.25 // Vibrato amplitude in semitones
  CASE s_vibrateenv:
  CASE s_vibrate:    RESULTIS   5.0  // Vibrato rate cycles per sec
  CASE s_volenv:
  CASE s_vol:        RESULTIS  50.0  // Volume per percentage

  CASE s_delayadjenv:
  CASE s_delayadj:
  CASE s_legatoadjenv:
  CASE s_legatoadj:
  CASE s_vibampadjenv:
  CASE s_vibampadj:
  CASE s_vibrateadjenv:
  CASE s_vibrateadj:
  CASE s_voladjenv:
  CASE s_voladj:     RESULTIS   1.0  // An adjustment multiplier
}

//AND FLT shapeval(envtype, env, absq) = VALOF
AND shapeval(envtype, env, absq) = VALOF
{ // This returns the shape value of a specified type.

  // envtype is the type of the environment eg s_volenv.
  //         This is required because env may be zero and the type
  //         is needed when looking up the default value.
  // env     is the environment to use, but may be zero.
  // q       is an absolute qbeat location

  // env =  0
  //     ->  [envlist, op, ln, parent, upb, v, absq1, absq2]
  // where
  // upb,v is a self expanding vector, possibly compacted.
  //       with v!0 being the last element of v used.
  //       The elements of v are pairs [absq,val] with absq
  //       in the range absq1 to absq2, ie absq1<=absq<=absq2
  // parent is zero or the enclosing environment of the same type.
  // absq1, absq2 are the absolute locations of the start and the
  //              end of the Block covered by this environment.

  LET v    = 0         // Will hold the environment items.
  LET last = 0         // Will hold the subscript of the last item, if any.
  LET i    = 3         // The subscript of the second entry, if present.
  LET absq1, absq2 = 0, 0 // Will be set is env is non zero
  UNLESS env RESULTIS defaultshapeval(envtype)
  absq1 := h7!env
  absq2 := h8!env
  IF FALSE DO
  IF envtype=s_vol DO
  { writef("shapeval: envtype=%s env=%n, absq1=%n, absq=%n absq2=%n*n",
            opstr(envtype), env, absq1, absq, absq2)
    abort(65657)
  }

  v := h6!env
  
  UNLESS v & absq1<=absq<=absq2 DO
  { // There was no enclosing environment of type envtype or absq is
    // not in range, so return the default value of this type.
    LET FLT res = defaultshapeval(envtype)
    //writef("*nshapeval: Returning the default shape value %5.2f of type %s*n",
    //         res, opstr(envtype))
    //abort(2661)
    RESULTIS res
  }

  last := v!0-1   // Subscript of the last pair in v.
  
//IF type=s_vol DO
//  writef("shapeval: env=%n v=%n q=%n last=%n*n", env, v, q, last)
//abort(4477)

  IF i > last DO
  { writef("shapeval: System error: i=%n > last=%n*n", i, last)
    abort(999)
    RESULTIS defaultshapeval(envtype)
  }
  IF FALSE DO
  IF envtype=s_vol DO
  { FOR i = 1 TO last BY 2 DO
    { IF i MOD 8 = 1 DO newline()
      writef(" %i5 %7.3f", v!i, v!(i+1))
    }
    newline()
  }
  //IF type=s_vol DO
  //{ writef("shapeval: type=s_vol q=%n v!1=%n v!last=%n*n", q, v!1, v!last)
  //}
  IF absq<v!1    DO absq := v!1
  IF absq>v!last DO absq := v!last

//  UNLESS 0 <= absq <=v!last RESULTIS v!(last+1) 
 
  // Find the surrounding pair of shape points.

  //writef("shapeval: env=%n v=%n absq=%n last=%n*n", env, v, absq, last)

  {
    IF FALSE DO
    IF envtype=s_vol DO
      writef("shapeval: absq=%n i=%n v!(i-2)=%i5 v!(i-1)=%5.3f *
              *v!i=%i5 v!(i+1)=%5.2f*n",
              absq, i, v!(i-2), v!(i-1), v!i, v!(i+1))
    IF v!i >= absq | i>=last BREAK
    i := i+2
  } REPEAT
//abort(23456)
  IF FALSE DO
  IF envtype=s_vol DO
    RESULTIS interpolatefltdebug(absq, v!(i-2), v!(i-1), v!i, v!(i+1))
  RESULTIS interpolateflt(absq, v!(i-2), v!(i-1), v!i, v!(i+1))
}

AND interpolatefltdebug(x, x1, FLT y1,
                           x2, FLT y2) = VALOF
{ //                              *----y2
  //                         o    |
  //                    +------------  Result
  //               o    |         |
  //          o         |         |
  //     *---y1         |         |
  //     |              |         |
  //     x1             x         x2
  
  // x1 <= x <= x2 and x1 < x2
  // Note the qbeat locations are integers while values are floating point.
  LET FLT res = ?

  writef("*ninterpolate: x=%n x1=%n y1=%5.3f x2=%n y2=%5.3f*n", x, x1, y1, x2, y2)
abort(772294)
  UNLESS x1 < x2 DO
  { writef("interpolate: System error x1=%n x=%n x2=%n*n", x1, x, x2)
    abort(999)
    RESULTIS y1
  }
//writef("interpolate: x=%n x1=%n y1=%7.3f x2=%n y2=%7.3f*n", x, x1, y1, x2, y2)
//abort(18834)
  IF x=x1 DO
  { writef("interpolate: Returning %5.2f*n", y1)
    RESULTIS y1 // Optimisation
  }
  IF x=x2 DO
  { writef("interpolate: Returning %5.2f*n", y2)
    RESULTIS y2 // Optimisation
  }
  // Otherwise perform linear interpolation.
  res := y1 + (y2-y1) * FLOAT(x-x1) / FLOAT(x2-x1)
  writef("interpolate: Returning %7.3f*n", res)
  RESULTIS res
}

AND interpolateflt(x, x1, FLT y1,
                      x2, FLT y2) = VALOF
{ //                              *----y2
  //                         o    |
  //                    +------------  Result
  //               o    |         |
  //          o         |         |
  //     *---y1         |         |
  //     |              |         |
  //     x1             x         x2
  
  // x1 <= x <= x2 and x1 < x2
  // Note the qbeat locations are integers while values are floating point.
  UNLESS x1 < x2 DO
  { writef("interpolate: System error x1=%n x=%n x2=%n*n", x1, x, x2)
    abort(999)
    RESULTIS y1
  }
//writef("interpolate: x=%n x1=%n y1=%7.3f x2=%n y2=%7.3f*n", x, x1, y1, x2, y2)
//abort(18834)
  IF x=x1 RESULTIS y1 // Optimisation
  IF x=x2 RESULTIS y2 // Optimisation
  // Otherwise perform linear interpolation.
  RESULTIS y1 + (y2-y1) * FLOAT(x-x1) / FLOAT(x2-x1)
}

AND interpolateint(x, x1, y1,
                      x2, y2) = VALOF
{ // x1 <= x <= x2 and x1 < x2
  // Note the qbeat locations are integers while values are floating point.
  UNLESS x1 < x2 DO
  { writef("interpolate: System error x1=%n x=%n x2=%n*n", x1, x, x2)
    abort(999)
    RESULTIS y1
  }
//writef("interpolateint: x=%n x1=%n y1=%7.3f x2=%n y2=%7.3f*n", x, x1, y1, x2, y2)
//abort(18834)
  IF x=x1 RESULTIS y1 // Optimisation
  IF x=x2 RESULTIS y2 // Optimisation
  // Otherwise perform linear interpolation.
  RESULTIS y1 + muldiv(y2-y1, x-x1, x2-x1)
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
  checktab( 98, 2349_318) // 7d
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
  checktab(109, 4434_922) // 8c#
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
//abort(55551)
      IF rt <= real_msecs DO
      { // This midi triple is now due so output it.
        LET triple = midip!2
        LET op     = triple & 255
        LET chan   = op & #x0F
        LET a1     = (triple>> 8) & 255
        LET a2     = (triple>>16) & 255
        LET is_note_on = (op&#xF0)=midi_note_on
        midip := !midip // Make midip point to thenext midi triple.
//writef("%9.3d playmidico: triple %2x %2x %2x*n", real_msecs, op, a1, a2)

        // Unless calibrating, do not play the solo channels
        UNLESS calibrating IF ((1<<chan) & solochannels)~=0 LOOP
//writef("%9.3d playmidico: triple %2x %2x %2x*n", real_msecs, op, a1, a2)
//writef("%9.3d playmidico: mt=%9.3d [%5.3d %5.3d]*n",
//        real_msecs, mt, start_msecs, end_msecs)

        // Output the midi triple, but only note_on commands if mt is
        // between start_msecs and end_msecs.
        TEST is_note_on
        THEN IF start_msecs <= mt <= end_msecs DO
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

    // End of performance
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
//{ LET midiname = "/dev/dmmidi1" // Should work with my Casio Bechstein piano
//                                // when running under Linux.
//				// dev/midi1 may be better.
{ LET midiname = "/dev/midi1" // Works with my Casio Bechstein piano
                              // when running under Linux.
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
    //GOTO fin
  }

  // Open the Microphone input device
  micfd := 0 //sys(Sys_sound, snd_waveInOpen, micname,
             //  micformat, micchannels, micrate)

  //UNLESS micfd>0 DO
  //{ writef("Unable to open the Microphone device, rc=%n*n", micfd)
  //  GOTO fin
  //}

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

    // Try to read some microphone data into soundv
    //callco(soundco, 1234)
    // If new sound data has been read mic_msecs will have been
    // set to the approximately real time of the latest sample.
    // mic_msecs is used by the note recognition coroutines.

    real_msecs := getrealmsecs()
    midi_msecs := r2m_msecs(real_msecs, ocr, ocm, crate)

    // Test for end of performance
    IF midi_msecs>=stop_msecs BREAK

    // Output any outstanding midi triples, if any.
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

  //IF notecount DO
  //  writef("*nAverage Midi-mic error %5.3d = %5.3d/%n*n",
  //          totalerr/notecount, totalerr, notecount)

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
//    abort(33331)
  } 
  //sys(Sys_sound, snd_midiOutWrite3, midifd, a, b, c)
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


