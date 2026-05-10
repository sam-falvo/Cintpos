/*
The program rast2wav.b is used to convert a standard raster file,
typically called RASTER, to a .wav file.  The
frequency of sound generated is scaled so that low address correspond
to frequencies around two octaves below middle C, and the highest
addresses have frequencies around three octaves above middle C. It
only generates frequencies correponding to a sprecified number of
notes per octave. The default setting is 12 corresponding to semitones.
The .wav file is scaled to last for a number of seconds specified by
the user. The default length being 30 seconds. By default the output
will be mono, but if the option stereo is specified, stereo output
will be generated with low notes on the left and high notes on the
right, as on a piano. The amplitude of the generated sound is scaled
to have a reasonable maximum volume. The .wav sample size is always 16
bits and the sample rate will be 44100 per second unless the r parameter
specifies 22050 or 11025.

Implemented by Martin Richards (c) March 2020.

Usage: from,to/K,n/N,secs/N,r/N,d/N,stereo/S,d/N,b/N,t/S

from      The raster data file, default RASTER.
to/K      The .wav file, default RASTER.wav.
n/N       The number of notes per octave, default 12.
secs/N    The required length in seconds of the raster data. The
          length of the .wav file is one second longer to allow
          the sound to die out.
r/N       is the .wav sample rate, default 44100.
stereo/S  Generate a stereo file, low to high notes from left to right,
          the default is mono.
d/N       Specifies a note to debug.
b/N       The bit position in the sampled address being tested
t/S       Turn on tracing as a debugging aid.

Typical usage is as follows, from a Linux bash shell.

rastsys
raster                      Start generating rastering data to file RASTER.
bcpl com/echo.b to junk     Perform a typical computation.
raster2wav n 24 secs 60     Convert RASTER to RASTER.wav lasting about
                            60 seconds with 24 notes per octave.

History

24/02/2020
Made a major change to the way notes are selected.

31/01/2020
Renamed this file to rast2wav.b. The previous rast2wav.b has been
renamed bits2wav.b.

28/01/2020
Minor changes to match the modified RASTER file format.

*/

SECTION "rast2wav"

GET "libhdr"

GLOBAL
{ // Global functions
  smoothsamples:ug
  wrsample
  rdn
  read_raster_params
  read_raster_lines
  notecofn
  filter
  addnote
  prmemv
  note2str
  
  // Global variables
  stdout
  stdin

  fromfilename // The raster data file
  fromstream
  wavfilename  // The .wav output file
  wavstream
  
  tracing
  debugnote         // =-1 or a note to debug
  
  maxfcount         // Total number of Cintcode instruction executions
                    // in the raster data.
  FLT fmaxfcount    // = FLOAT maxfcount
  
  maxaddress        // The highest Cintcode byte address referenced.
  kval              // The number of obeyed Cintcode instructions per
                    // raster line.
  fkval             // = FLOAT kval
  sval              // The number of address byte per raster line unit.

  fcount            // Count of Cintcode instructions obeyed upto the
                    // latest raster line. It is incremented by kval
	            // on receiving a raster line.

  line_secs
  prevline_secs
  FLT line_fsecs    // Time at the end of the current raster period
  line_pos          // Sample position in the .wav data of current line
  
  stereo            // =TRUE if generating stereo
  bits_per_sample   // Will always be 16
  bytes_per_sample  // =16 or 32
  sample_rate       // .wav samples/sec set to 44100, 22050 or 11025
  FLT fsample_rate  // = FLOAT sample_rate
  
  s020 // samples per  20 msecs
  s025 // samples per  25 msecs
  s050 // samples per  50 msecs
  s100 // samples per 100 msecs
  s200 // samples per 200 msecs
  s250 // samples per 250 msecs
  s500 // samples per 500 msecs
  
  bytes_per_second

  totalsecs            // Integer length of the .wav file in seconds
  FLT ftotalsecs       // = FLOAT totalsecs
  FLT fsecs_per_fcount // Factor to convert fcount to secs
  
  samples      // The number of mono or stereo samples to be generated
               // = totalsecs * sample_rate
  FLT fsamples // = FLOAT samples
  
  data_bytes   // = samples * bytes_per_sample

  ch
  
  Lsamplev     // Vector of accumulated left samples
  Rsamplev     // Vector of accumulated right samples if stereo
  samplevupb   // UPB of Lsamplev and Rsamplev
  fsamplevupb  // = FLOAT samplevupb
  
  notes_per_octave // The number of notes per octave, typically 12.

  // Note numbers from C2=0 to C7, inclusive.
  
  C2 // Lowest note generated   = 0
  C3
  C4
  C5
  C6
  C7 // Highest note generated  = 5 * notes_per_octave

// Each note has a waveform consisting of a weighted sum of sines
// based on the notes fundamental frequency.

// The Cintcode addresses are split into equal sized ranges with one
// for each note from C2 to C7. The amplitude of a note is controlled
// by the pattern of accesses to addresses belonging to the note.
// Each note has a coroutine used to generate its sequence of samples.
// These coroutines all use the same mainfunction: notecofn.

// The coroutines are held in notecov whose subscripts range from
// C2 (=0) to C7. All these coroutines are activated by each raster
// line. A note coroutine receives the value 0 if none of its Cintcode
// addresses were accessed during the current raster period, otherwise
// it receives the value 1. The pattern of 0s and 1s received by
// a note coroutine affects how the amplitude of the note changes.
// The details are described in the definition of notecofn, below.
// Note coroutines are given the value 2 to close them down.

  notecov    // Use to hold the note coroutines.
  notecovupb // UPB of notecov (=C7)

  memvupb    // = maxaddress/sval.

  memv       // memvi will hold a bit pattern showing the recent
             // history of memory accesses of byte addresses in the
	     // range 8i to 8i+7 assuming sval=8. At each raster
	     // line memv!i is shifted left one place and a one
	     // placed in its least significant bit if one of its
	     // byte addresses have been accessed. It LS bit will
	     // otherwise be left as zero. This bit pattern is
	     // used by scanmemv to determine which notes should
	     // be activated. The bit pattern mask is used to
	     // ensure that all elements of memv are non negative.

  mask       // A mask used to ensure all elements of memv are
             // greater non negative.
  lim1       // Two bit patterns used by gennotes to determine
  lim2       // which notes should be sounded.
  
  notev      // notev!n will equal 1 for every note n that 
             // should potentially be sounded, notev!n will be zero
	     // otherwise. Before notes are sounded the elements of
	     // notev are filtered by the the function filter. This
	     // reduces the chance that notes that are too close
	     // together from being sounded simultaneously. It is
	     // still under development. After filtering the elements
	     // of notev specify whether 0 or 1 will be given to
	     // their related coroutines. Note coroutines ensure that
	     // a note will note be sounded again if it is already
	     // playing.

  noteposv   // The end sample positions of each note.
  notevupb   // UPB of notev (=C7)
}

LET start() = VALOF
{ LET argv    = VEC 50
  LET riffhdr = VEC 10

//  abort(1234)

  stdout := output()
  stdin := input()

  IF rdargs("from,to/K,n/N,s/N,r/N,d/N,stereo/S,d/N,b/N,t/S", argv, 50)=0 DO
  { writes("Bad arguments for RAST2WAV*n")
    RESULTIS 20
  }

  // Set the default values
  fromfilename := "RASTER"
  fromstream := 0
  wavfilename := "RASTER.wav"
  wavstream := 0

  notes_per_octave := 12 // Default number of notes per octave
  totalsecs := 10        // Default value, can be changed by the secs option
  //totalsecs := 30
  sample_rate := 44_100  // 11_025 or 22050, possibly changed by the r option
  //sample_rate := 11_025

  debugnote := -1        // Can be set by the d option
  //debugnote := 5
  
  stereo := FALSE        // mono by default
  //stereo := TRUE        // stereo by default
  maxfcount, maxaddress := 0, 0 // These will be set by
  kval, sval := 0, 0            // read_raster_params.
  
  IF argv!0 DO fromfilename     := argv!0     // from
  IF argv!1 DO wavfilename      := argv!1     // to/K
  IF argv!2 DO notes_per_octave := !(argv!2)  // n/N
  IF argv!3 DO totalsecs        := !(argv!3)  // s=secs/N
  IF argv!4 DO sample_rate      := !(argv!4)  // r/N
  IF argv!5 DO debugnote        := !(argv!5)  // d/N
  IF argv!6 DO stereo           := ~stereo    // stereo/S
  IF argv!7 DO testnote         := !(argv!7)  // d/S
  IF argv!8 DO bitpos           := !(argv!8)  // b/N
  tracing                       := argv!9     // t/S


  Lsamplev := 0 // Vectors to hold the left and possibly
  Rsamplev := 0 // right samples for the .wav file.
  memv     := 0
  notev    := 0
  noteposv := 0 // The final sample positions of each note.
                // A note cannot be sounded if it is still playing.
  notecov  := 0
  
  fromstream := findinput(fromfilename)
  UNLESS fromstream DO
  { sawritef("Unable to open raster file %s*n", fromfilename)
    GOTO fin
  }

  selectinput(fromstream)
  
  UNLESS read_raster_params() GOTO fin

  bits_per_sample := 16 // Always use 16 bit samples.

  writef("Converting file %s to %s*n", fromfilename, wavfilename)
  writef("sample_rate = %n*n", sample_rate)
  writef("%s %n-bit samples*n", (stereo->"stereo", "mono"), bits_per_sample)

  writef("Total time with the extra second: %n seconds*n*n", totalsecs+1)

  // The following variables are set from the raster data.
  writef("maxaddress = %9i*n", maxaddress)
  writef("maxfcount  = %9i*n", maxfcount)
  writef("kval=%n sval=%n*n", kval, sval)

//abort(8889)

  fmaxfcount       := FLOAT maxfcount
  fkval            := FLOAT kval
  ftotalsecs       := FLOAT totalsecs
  fsecs_per_fcount := ftotalsecs / fmaxfcount

  // This program generates notes in the range C2 (C two
  // octaves below middle C) to C7 (three octaves above
  // Middle C). By default there are 12 semitones per octave.

  C2 := 0                         // The lowest note
  C3 := C2 + 1*notes_per_octave
  C4 := C2 + 2*notes_per_octave   // Middle C
  C5 := C2 + 3*notes_per_octave   // C above middle C
  C6 := C2 + 4*notes_per_octave
  C7 := C2 + 5*notes_per_octave   // The highest note

  writef("c2=%n C3=%n C4=%n C5=%n C6=%n C7=%n*n", C2, C2, C4, C5, C6, C7)
  // With the default setting of notes_per_octave C7 is 60.

  sample_rate := sample_rate > 44_000 -> 44_100,
                 sample_rate > 22_000 -> 22_050,
		                         11_025
  fsample_rate := FLOAT sample_rate
  
  s020 := sample_rate/50 // samples per  20 msecs
  s025 := sample_rate/40 // samples per  25 msecs
  s050 := sample_rate/20 // samples per  50 msecs
  s100 := sample_rate/10 // samples per 100 msecs
  s200 := sample_rate/ 5 // samples per 200 msecs
  s250 := sample_rate/ 4 // samples per 250 msecs
  s500 := sample_rate/ 2 // samples per 500 msecs
  
  samples := sample_rate * totalsecs & -16
  fsamples := FLOAT samples

  // Samples is the number of mono or stereo 16 bit samples
  // up to the end of the raster data.
  
  // Add one second at the end of the .wav data to allow
  // the sound to die away at the end.
  samplevupb := samples + sample_rate
  fsamplevupb := FLOAT samplevupb
  
  bytes_per_sample := bits_per_sample / 8  // Typically 2
  IF stereo DO bytes_per_sample :=  bytes_per_sample * 2
  bytes_per_second := sample_rate * bytes_per_sample
  data_bytes       := samplevupb  * bytes_per_sample

  writef("Data bytes = %n*n", data_bytes)
  writef("Total number of samples: %n*n*n", samples)

  // Open the .wav file for output.
  wavstream := findoutput(wavfilename)
  UNLESS wavstream DO
  { sawritef("Unable to open output file %s*n", wavfilename)
    GOTO fin
  }

  // Construct the .wav file header block.
  riffhdr!0  := #x46464952           // R I F F
  riffhdr!1  := data_bytes+36        // size in bytes of the file
                                     // including this header
  riffhdr!2  := #x45564157           // W A V E
  riffhdr!3  := #x20746D66           // f m t
  riffhdr!4  := bits_per_sample      //
  riffhdr!5  := stereo -> 2<<16 | 1, // stereo
                          1<<16 | 1  // mono
  riffhdr!6  := sample_rate          // samples per second.
  riffhdr!7  := bytes_per_second
  riffhdr!8  := bits_per_sample<<16 | bytes_per_sample
  riffhdr!9  := #x61746164           // d a t a
  riffhdr!10 := data_bytes

  // Output the header information.
  selectoutput(wavstream)
  FOR i = 0 TO 43 DO binwrch(riffhdr%i)
  selectoutput(stdout)

  // What must now follow are samplevupb+1 16-bit mono or stereo
  // samples.
  
  fcount        := 0   // Cintcode instruction count

  line_secs     := 0
  prevline_secs := 0
  line_fsecs    := 0.0 // Time of the current raster line.
  line_pos      := 0   // Sample position in the .wav data of the
                       // current raster line.
  
  // Create and initialise the note coroutines.

  notecovupb := C7
  notecov    := getvec(notecovupb)
  UNLESS notecov DO
  { writef("More space needed*n")
    abort(999)
  }
  // Create typically 61 coroutines, one for each note from C2 to C7.
  FOR noteno = C2 TO C7 DO notecov!noteno := initco(notecofn, 300, noteno)

  memvupb := maxaddress / sval + 1
  memv    := getvec(memvupb)
  UNLESS memv DO
  { writef("More space needed*n")
    abort(999)
  }
  FOR a = 0 TO memvupb DO memv!a := 0

  notevupb := C7
  notev    := getvec(notevupb)
  noteposv := getvec(notevupb)
  UNLESS notev & noteposv DO
  { writef("More space needed*n")
    abort(999)
  }

  FOR n = C2 TO C7 DO notev!n, noteposv!n := 0, 0

  Lsamplev := getvec(samplevupb)
  UNLESS Lsamplev DO
  { writef("More space needed*n")
    abort(999)
  }
  
  IF stereo DO
  { Rsamplev := getvec(samplevupb)
    UNLESS Rsamplev DO
    { writef("More space needed*n")
      abort(999)
    }
  }

//sawritef("Returning from start as a debugging aid.*n")
//RESULTIS 0 // Return from rast2wav without returning any
//           // getvec'd memory as a debugging aid to see
//	     // how much memory is used.
	     
//GOTO fin // Debugging aid to check that all allocated space is freed.

  // Clear the .wav data vectors
  FOR i = 0 TO samplevupb DO
  { Lsamplev!i := 0.0
    IF stereo DO Rsamplev!i := 0.0
  }

  // Set bit pattern used in the decision of which notes to sound.
  mask := #b111_1111_1111_1111_1111_1111_1111_1111
  lim1 := #b000_0000_0000_0000_0000_0100_0000_0000
  lim2 := #b000_0000_0000_0000_0000_0111_1111_1111

  // Output the note range fo each note
  //FOR n = C2 TO C7 DO
  //{ LET a1 = (n*maxaddress) / (C7-C2+1)
  //  LET a2 = ((n+1)*maxaddress) / (C7-C2+1)

  //  IF n MOD notes_per_octave = 0 DO
  //    writef("*n%3i:", n)
  //  writef(" %8i", a1)
  //  //abort(1000)
  //}
  //newline()

  writef("debugnote=%n *n*n", debugnote, note2str(debugnote))

  read_raster_lines()
       
  // All that raster data has now been processed. All that
  // is now required is to fill in .wav data to its end by
  // pretending there is a raster line when fcount = maxfcount.

  fcount     := maxfcount
  line_fsecs := FLOAT(totalsecs+1) 
  line_pos   := samplevupb
  
  //writef("*nFinal raster line: line_sec=%12.6f line_pos=%n*n",
  //        line_fsecs, line_pos)

  // This final raster line activates no notes, allowing the sound
  // to die out. Display such a line.
  
  FOR noteno = C2 TO C7 DO
  { IF noteno MOD 10 = 0 DO wrch(' ')
    wrch('.')
  }
  writef(" %12.6f*n", line_fsecs)
  //abort(1001)

  // Give value 0 to each note coroutine, causing them to generate
  // samples using the remaining envelopes up to and including
  // sample position line_pos.

  TEST debugnote < 0
  THEN FOR noteno = C2 TO C7 DO     callco(notecov!noteno,    0)
  ELSE IF C2 <= debugnote <= C7 DO  callco(notecov!debugnote, 0)

  smoothsamples(Lsamplev, samplevupb)
  IF stereo DO smoothsamples(Rsamplev, samplevupb)
  
  // Find the maximum sample value
  { LET FLT maxsamplevalue = 0.01
    FOR i = 0 TO samplevupb DO
    { LET FLT sample = Lsamplev!i
      IF maxsamplevalue < ABS sample DO maxsamplevalue := ABS sample
      IF stereo DO
      { sample := Rsamplev!i
        IF maxsamplevalue < ABS sample DO maxsamplevalue := ABS sample
      }
    }

    writef("Maximum sample value = %8.6f*n", maxsamplevalue)

    // Write the scaled stereo or mono samples
    TEST stereo
    THEN FOR i = 0 TO samplevupb DO
           wrsample(FIX(30000 * Lsamplev!i / maxsamplevalue),
                    FIX(30000 * Rsamplev!i / maxsamplevalue))
    ELSE FOR i = 0 TO samplevupb DO
           wrsample(FIX(30000 * Lsamplev!i / maxsamplevalue), 0)
  }

fin:
  selectoutput(stdout)
  selectinput(stdin)

  IF Lsamplev DO freevec(Lsamplev)
  IF Rsamplev DO freevec(Rsamplev)
  
  IF notecov DO
  { // Close down and delete all the note coroutines
    FOR noteno = C2 TO C7 DO
    { LET cptr = notecov!noteno
      UNLESS cptr LOOP
      callco(cptr, 2)
      deleteco(cptr)
    }
    freevec(notecov)
  }
  
  IF memv     DO freevec(memv)     // BUG if commented out
  IF notev    DO freevec(notev)
  IF noteposv DO freevec(noteposv)

  IF fromstream DO { endstream(fromstream)
                     fromstream := 0
		   }
  IF wavstream  DO { endstream(wavstream)
                     wavstream   := 0
                     writef("File %s written*n", wavfilename)
		   }
  RESULTIS 0   
}

AND smoothsamples(v, upb) BE
{ LET FLT a = 0.0
  LET FLT b = 0.0
  LET FLT c = 0.0
  LET FLT d = 0.0
  LET FLT e = 0.0
  FOR i = 1 TO upb DO
  { a := b
    b := c
    c := d
    d := e
    e := v!i
    v!i := (a+b+c+d+e) / 5.0
  }
}

AND wrsample(left, right) BE
{ selectoutput(wavstream)
  binwrch(left); binwrch(left>>8)
  IF stereo DO { binwrch(right); binwrch(right>>8) }
  selectoutput(stdout)
}

AND rdn() = VALOF
{ LET res = 0
  WHILE ch='*s' | ch='*n' DO ch := rdch()
  IF ch=endstreamch RESULTIS -1
  UNLESS '0'<=ch<='9' DO
  { writef("Bad number, ch='%c'*n")
    abort(999)
  }
  res := ch-'0'
  ch := rdch()
  WHILE '0'<=ch<='9' DO
  { res := 10*res + ch - '0'
    ch := rdch()
  }
  RESULTIS res
}


AND read_raster_params() = VALOF
{ // This function reads the raster data file to set the
  // raster parameters, namely:
  
  // maxfcount   is the number of Cintcode instructions executed
  // maxaddress  is the highest address accessed
  // kval        is the number of Cintcode instructions obeyed
  //             per raster line.
  // sval        is the number of words per unit in the raster line.

  maxfcount  := 0
  maxaddress := 0
  
  ch := rdch() REPEATWHILE ch='*s'
  IF ch='F' DO
  { // Get the number of Cintcode instructions executed
    ch := rdch()
    maxfcount := rdn()
  }
  WHILE ch=' ' DO ch := rdch()
  IF ch='M' DO
  { // Get the maximum byte addres referenced
    ch := rdch()
    maxaddress := rdn()
  }
  WHILE ch=' ' DO ch := rdch()
  IF ch='K' DO
  { // Get the number of Cintcode instructions executed per raster line
    ch := rdch()
    kval := rdn()
  }
  WHILE ch=' ' DO ch := rdch()
  IF ch='S' DO
  { // Get the number of address bytes per raster point
    ch := rdch()
    sval := rdn()
  }
  UNLESS maxfcount & maxaddress DO
  { writef("Bad RASTER file*n")
    RESULTIS FALSE
  }
  fmaxfcount := FLOAT maxfcount
  fkval := FLOAT fkval
  RESULTIS TRUE
}

AND read_raster_lines() BE
{ // This is the start of the loop to process raster lines.
  LET a, b = 0, 0  // These will hold the range of Cintcode addresses
                   // in units of sval byte referenced in the latest
		   // raster line period.

  { // Start of raster line loop
    SWITCHON ch INTO
    { DEFAULT:
        sawritef("Bad ch '%c'*n", ch)
        abort(1111)

      CASE ' ':          // Ignore white space
      CASE '*c':
      CASE '*n':
        ch := rdch()
        LOOP

      CASE 'W':          // White (no access) region
        ch := rdch()
        b := a + rdn()
        // Positions a to b-1 were not accessed during
	// the latest raster line period.
        FOR i = a TO b-1 DO memv!i := memv!i<<1 & mask
        a := b           // Set a to start of the next region
	LOOP
      
      CASE 'B':          // Black(access) region
        ch := rdch()
        b := a + rdn()
        // Positions a to b-1 were accessed during
	// the raster line period just ended.
        FOR i = a TO b-1 DO memv!i := (memv!i<<1 & mask) + 1
        a := b           // Set a to start of the next region
        LOOP

      CASE 'N':          // End of a raster line period.
      CASE 'Z':
        ch := rdch()
      CASE endstreamch:  // No more raster lines.
        // Fill in the memv elements for the rest of the raster line.
        FOR i = a TO memvupb DO memv!i := memv!i<<1
        gennotes()
	IF ch=endstreamch RETURN // Return from read_raster_lines
	//abort(2666)
	LOOP
    }
  } REPEAT // Deal with the next item on the current raster line

//  abort(1357)
} REPEAT // Look for another raster line


AND gennotes() BE
{ // Generate notes based on data in memv and noteposv,
  // line_pos is the sample position of the latest raster line.
  
  LET p, q = 0, 0 // Used to hole the address range of each note.
  
  // Use the data in memv to suggest which notes should sound.
  FOR n = C2 TO C7 DO
  { p := q
    q := memvupb * (n+1) / (C7-C2+1)
    // p to q-1 is the range of address units corresponding to note n.
    prmemv(n, p, q)  // Debugging aid.
    FOR i = p TO q-1 DO
    { LET bits = memv!i
      IF lim1 <= bits < lim2 DO
      { // Typically lim1 = #b000_0000_0000_0000_0000_0100_0000_0000
        // and       lim2 = #b000_0000_0000_0000_0000_0111_1111_1111
        // so        bits = #b000_0000_0000_0000_0000_01XX_XXXX_XXXX
        // with at least one X equal to 0.
        notev!n := 1 // An address belonging to note n is active.
        BREAK
      }
    }

    // The notes between C2 and C7 that have been activated in
    // the current period have their elements set to TRUE.
	
    // Set the global parameters for the raster period
    // just ended for use by the note coroutines.

    fcount     := fcount+kval
    line_fsecs := pos2secs(fcount)
    line_secs  := FIX(sys(Sys_flt, fl_floor, line_fsecs))

    UNLESS line_secs = prevline_secs DO
    { writef("fsecs=%7.3f Time %i2 second%-%ps*n", line_fsecs, line_secs)
      prevline_secs := line_secs
      //abort(7222)
    }

    line_pos := FIX(line_fsecs * fsample_rate)

    IF tracing DO
    { FOR noteno = C2 TO C7 DO
      { IF noteno MOD 10 = 0 DO wrch(' ')
        writef("%c", notev!noteno->'X', '.')
      }
      writef(" line_pos=%n*n", line_pos)
      IF line_pos=433931 DO abort(4999)
    }

    filter(notev, C7)

    IF tracing DO
    { FOR noteno = C2 TO C7 DO
      { IF noteno MOD 10 = 0 DO wrch(' ')
        writef("%c", notev!noteno->'X', '.')
      }
      writef(" line_pos=%n*n", line_pos)
    }

    // Notev!C2 to notev!C7 will hold which notes have been
    // activated in the latest raster period. Call each note
    // coroutine, or if debugnote>=0 just call the coroutine
    // for debugnote.

    TEST debugnote >= 0

    THEN IF C2 <= debugnote <= C7 DO
      callco(notecov!debugnote, notev!debugnote)

    ELSE FOR noteno = C2 TO C7 DO
      callco(notecov!noteno, notev!noteno)
	
    BREAK // Look for another raster line
  }
}

AND notecofn(args) = VALOF
{ // This is the body of each note coroutine.
  // When first called, !args is the note number between C2 and C7
  // that this coroutine controls.

  // The following global variables are set every time a raster line
  // is processed

  // line_pos     is the sample position of the latest raster line
  // line_fsecs   is the time at the latest raster line
  
  // Variables local to this coroutine.

  // freq         The frequency of this coroutine's note

  // target_amp   These are used in the linear interpolation
  // target_pos   calculation to determine the amplitude at
  // base_amp     a sample position between base_pos and
  // base_pos     target_pos.
  //            
  
  // samples_per_cycle     is the number of samples per cycle
  // samples_per_cycle_by2 is samples_per_cycle/2
  
  //LET noteno = (!args+5) MOD (C7+1) // An experimental permutation
  LET noteno = !args

  LET prevpos = 0         // End sample position of the previous
                          // sounding of this note.
  LET n = 0               // This will hold the value passed to this
                          // coroutine at each raster line.

  // Calculate the frequency of this coroutine's note, using equal
  // temperament intervals with the A above middle C being 440.0Hz.
  LET FLT freqA4 = 440.0
  LET FLT freqC5 = freqA4 * sys(Sys_flt, fl_pow, 2.0, FLOAT 3 / FLOAT 12)
  LET FLT freqC2 = freqC5 / 8.0
  LET FLT freq   = freqC2 *
                   sys(Sys_flt, fl_pow, 2.0,
		                        FLOAT noteno / FLOAT notes_per_octave)
  // freq is the frequency of this coroutine's note.
  
  LET spc    = FIX(fsample_rate / freq) // spc = samples per cycle
                                        // as an integer
  
  // Calculate the stereo amplitude factors.
  LET FLT Lfac = FLOAT(C7-noteno) / FLOAT(C7-C2)
  LET FLT Rfac = FLOAT(noteno-C2) / FLOAT(C7-C2)




  LET count0 = 100000 // The count of recent raster lines with n=0.
  LET count1 = 0      // The count of recent raster lines with n=1.
  LET prevn  = 0      // The value of n for the previous rasterline.
                      // This is used to identify 01 and 10 transitions.

  LET lim1 =  2000 //s100 // Number of samples per 1/10 secs.
  LET lim2 =  5000 //s500 // Number of samples per 1/2 secs.

  //writef("Started note coroutine for note %i3 freq = %8.2f*n", noteno, freq)
  //writef("%3i:        Lfac=%9.6f  Rfac=%9.6f*n", noteno, Lfac, Rfac)
  //abort(7890)

  // All frequencies have been checked to be correct to at least 5 sig digits
  // when there are 12 notes per octave.

  { // Start of the loop to deal with notes activated by raster lines.
  
    // Wait for the next raster line data for the current note
    LET n = cowait(0)
    // This coroutine is called whenever a raster line is reached.
    // n=0 if the note was not activated in the raster period just ended,
    // n=1 if the note was activated,
    // n=2 if the note coroutine must close down.
    // At this moment line_pos is the sample position in the .wav data
    // of the raster line.

    IF n=2 BREAK

    IF prevpos > line_pos BREAK
    
    // If n = 0 or 1 this coroutine maintains three variables
    
    // prevn hold the previous value of n.
    
    // count0 is the count of the number of 0s received since
    //        the most recent 1 to 0 transition.
    
    // count1 is the count of the number of 1s received since
    //        the most recent 0 to 1 transition.

    // These variables are used to determine when 0 to 1 and
    // 1 to 0 transitions and when this coroutine's note should
    // be sounded. The current version of the program activates
    // a note at a 0 to 1 transition if count0 is large enough.
    // If count0+count1 is large enough the note will be loud,
    // otherwise it will be soft. These limits are held in lim1
    // and lim2.

    //line_pos := FIX(fsamples * FLOAT fcount / fmaxfcount)

//  1         ----------                    --------
//           /          \                  /
//  0  ------            ------------------
//            < count1 > <     count0     > ^
//                                          |
//                                          Sound the note if
//                                          count0 is large enough
IF noteno=debugnote DO
  writef("%i2: n=%n count0=%i5    count1=%i5 lim1=%n lim2=%n*n",
         noteno, n, count0, count1, lim1, lim2)
//IF fcount MOD 100000 = 0 DO abort(1000)
    TEST n=0
    THEN { IF prevn=1 DO
           { // This is a 1 to 0 transition.
	     count0 := 0
	   }
	   count0 := count0+1
	 }
    ELSE { IF prevn=0 & noteposv!noteno<line_pos DO
           { // This is a 0 to 1 transition.
	     //writef("01 transition for note %s count0=%n lim1=%n lim2=%n*n",
	     //        note2str(noteno), count0, lim1, lim2)
             IF count0 > lim1 DO
             { // Sound the note.
               TEST count0 > lim2
               THEN { // Sound a loud note because it has been
	              // silent for long enough.
                      LET p0, a0 = line_pos, 0.00
                      LET p1, a1 = p0+s020,  1.00
                      LET p2, a2 = p1+s025,  0.60
                      LET p3, a3 = p2+s250,  0.40
                      LET p4, a4 = p3+s050,  0.00
		      writef("time=%8.3f ", pos2secs(p0))
                      writef("%i6: Loud %s*n", p0, note2str(noteno))
                      addnote(noteno, spc, p0, a0, p1, a1, Lfac, Rfac)
                      addnote(noteno, spc, p1, a1, p2, a2, Lfac, Rfac)
                      addnote(noteno, spc, p2, a2, p3, a3, Lfac, Rfac)
                      addnote(noteno, spc, p3, a3, p4, a4, Lfac, Rfac)
		      prevpos := p4
                    }
               ELSE { // Sound a soft note since has been sounded recently
                      LET p0, a0 = line_pos, 0.00
                      LET p1, a1 = p0+s020,  0.40
                      LET p2, a2 = p1+s025,  0.15
                      LET p3, a3 = p2+s250,  0.10
                      LET p4, a4 = p3+s050,  0.00
		      writef("time=%8.3f ", pos2secs(p0))
                      writef("%i6: Soft %s*n", p0, note2str(noteno))
                      addnote(noteno, spc, p0, a0, p1, a1, Lfac, Rfac)
                      addnote(noteno, spc, p1, a1, p2, a2, Lfac, Rfac)
                      addnote(noteno, spc, p2, a2, p3, a3, Lfac, Rfac)
                      addnote(noteno, spc, p3, a3, p4, a4, Lfac, Rfac)
		      prevpos := p4
                    }
	       //abort(9788)
             }
             count1 := 0
	   }
           count1 := count1+1
	 }
    prevn := n
//    abort(4429)
  } REPEAT
}

//AND Filter(v, upb) BE  // Intentional BUG

AND filter(v, upb) BE
{ // Current plan.
  // Replace all 1 elements of v with 0s except the
  // middle elements of groups of consecutive 1s.
  LET t = -1 // Position of the first 1 in a group of 1s.
//writef("filter: line_pos=%n*n", line_pos)

  FOR p = 0 TO upb DO
  { // Search for a 1.
    IF v!p=0 LOOP
    // p is the first 1 of a group.
    v!p := 0  // Replace it with a 0 and
    t := p    // remember its position.
    
    // Find the end of a group of 1s,
    { p := p+1
      //IF p<=upb & v!p=0 BREAK  // Intentional BUG
      IF p>upb | v!p=0 BREAK  // End of a group of 1s.
      v!p := 0      // Set every element to 0
    } REPEAT

    t := (t+p)/2 // The mid point of the group.
    v!t := 1
  }
}

AND addnote(note, spc, p, FLT ap, q, FLT aq, FLT Lfac, FLT Rfac) BE
{ LET spcby2 = spc / 2     // Samples per half cycle.

  FOR s = p+1 TO q DO
  { // Calculate the amplitude for sample position s using linear
    // interpolation based on (p,ap) and (q,aq).

    // Linear interpolation calculation.
    LET FLT amp = ap + (aq-ap) * FLOAT(s-p) / FLOAT(q-p)

    // Calculate the raw square wave sample at position s depending
    // on spc (samples per cycle) based on the frequency of the
    // current coroutine's note..
    LET FLT sample = s MOD spc < spcby2 -> amp, -amp

    IF s > samplevupb DO
    { writef("System error: Trying to write beyond the end of Lsamplev*n")
      abort(999)
      RETURN
    }
    // Add the stereo or mono sample into the  .wav data.
    // The stereo samples are adjusted by Lfac and Rfac.
    TEST stereo
    THEN { Lsamplev!s := Lsamplev!s + sample * Lfac
           Rsamplev!s := Rsamplev!s + sample * Rfac
         }
    ELSE { Lsamplev!s := Lsamplev!s + sample
         }

//    writef("spc=%n spcby2=%n sMODspc=%n ", spc, spcby2, s MOD spc)
//    writef("s=%i6 amp=%7.3f sample=%7.3f  ", s, amp, sample)
//    writef("(p,ap) = (%n, %6.3f)  (q,aq) = (%n, %6.3f)",
//                      p,  ap,               q,  aq)
//    writef("%s*n", note2str(note))
//    abort(7138)
  }
//  abort(7139)  
}

AND prmemv(n, p, q) BE
{ LET notename = note2str(n)
  FOR i = p TO q IF memv!i DO
    writef("%i5: %32b  %s*n", i, memv!i, notename)
}

AND pos2secs(p) = ftotalsecs * FLOAT p / FLOAT samplevupb

AND note2str(note) = VALOF
{ // This function returns the name of a note given its note number.
  // It only works when there are 12 notes per octave.
  // It attempts to do something sensible when the number of notes
  // per octave is not 12.
  
  MANIFEST {
    // Note numbers
    C2=0; Cis2; D2; Dis2; E2; F2; Fis2; G2; Gis2; A2; Ais2 B2 
    C3;   Cis3; D3; Dis3; E3; F3; Fis3; G3; Gis3; A3; Ais3 B3 
    C4;   Cis4; D4; Dis4; E4; F4; Fis4; G4; Gis4; A4; Ais4 B4 
    C5;   Cis5; D5; Dis5; E5; F5; Fis5; G5; Gis5; A5; Ais5 B5 
    C6;   Cis6; D6; Dis6; E6; F6; Fis6; G6; Gis6; A6; Ais6 B6 
    C7
  }

  // Change the note number if the number of notes per octave
  // is not 12.
  note := (note * 12) / notes_per_octave
  
  SWITCHON note INTO
  { DEFAULT:   RESULTIS "?"

    CASE C2:   RESULTIS "C2"
    CASE Cis2: RESULTIS "Cis2"
    CASE D2:   RESULTIS "D2"
    CASE Dis2: RESULTIS "Dis2"
    CASE E2:   RESULTIS "E2"
    CASE F2:   RESULTIS "F2"
    CASE Fis2: RESULTIS "Fis2"
    CASE G2:   RESULTIS "G2"
    CASE Gis2: RESULTIS "Gis2"
    CASE A2:   RESULTIS "A2"
    CASE Ais2: RESULTIS "Ais2"
    CASE B2:   RESULTIS "B2"

    CASE C3:   RESULTIS "C3"
    CASE Cis3: RESULTIS "Cis3"
    CASE D3:   RESULTIS "D3"
    CASE Dis3: RESULTIS "Dis3"
    CASE E3:   RESULTIS "E3"
    CASE F3:   RESULTIS "F3"
    CASE Fis3: RESULTIS "Fis3"
    CASE G3:   RESULTIS "G3"
    CASE Gis3: RESULTIS "Gis3"
    CASE A3:   RESULTIS "A3"
    CASE Ais3: RESULTIS "Ais3"
    CASE B3:   RESULTIS "B3"

    CASE C4:   RESULTIS "C4"
    CASE Cis4: RESULTIS "Cis4"
    CASE D4:   RESULTIS "D4"
    CASE Dis4: RESULTIS "Dis4"
    CASE E4:   RESULTIS "E4"
    CASE F4:   RESULTIS "F4"
    CASE Fis4: RESULTIS "Fis4"
    CASE G4:   RESULTIS "G4"
    CASE Gis4: RESULTIS "Gis4"
    CASE A4:   RESULTIS "A4"
    CASE Ais4: RESULTIS "Ais4"
    CASE B4:   RESULTIS "B4"

    CASE C5:   RESULTIS "C5"
    CASE Cis5: RESULTIS "Cis5"
    CASE D5:   RESULTIS "D5"
    CASE Dis5: RESULTIS "Dis5"
    CASE E5:   RESULTIS "E5"
    CASE F5:   RESULTIS "F5"
    CASE Fis5: RESULTIS "Fis5"
    CASE G5:   RESULTIS "G5"
    CASE Gis5: RESULTIS "Gis5"
    CASE A5:   RESULTIS "A5"
    CASE Ais5: RESULTIS "Ais5"
    CASE B5:   RESULTIS "B5"

    CASE C6:   RESULTIS "C6"
    CASE Cis6: RESULTIS "Cis6"
    CASE D6:   RESULTIS "D6"
    CASE Dis6: RESULTIS "Dis6"
    CASE E6:   RESULTIS "E6"
    CASE F6:   RESULTIS "F6"
    CASE Fis6: RESULTIS "Fis6"
    CASE G6:   RESULTIS "G6"
    CASE Gis6: RESULTIS "Gis6"
    CASE A6:   RESULTIS "A6"
    CASE Ais6: RESULTIS "Ais6"
    CASE B6:   RESULTIS "B6"

    CASE C7:   RESULTIS "C7"
  }
}

