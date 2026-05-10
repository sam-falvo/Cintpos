/*
The previous version of this program is rast2wav01feb2025.b.

This program (rast2wav.b) converts a standard raster file, typically
called RASTER, to a .wav file.  The frequency of the sounds generated
is typically scaled so that low addresses correspond to frequencies
around two octaves below middle C, and the highest addresses have
frequencies around three octaves above middle C.  The user can specify
how many different notes there are per ocatave using the parameter
n. If n=12 only normal semitones are used. The default value is 24
allowing quater tones.

The .wav file is scaled to last for a number of seconds specified by
the user. The default being 60 seconds. At the end there is a short
period the let the sound die out.  Normally the output will be mono at
11025 samples per second, but, the user can specify stereo an other
data rates. When generating stereo notes to range from left to right
as the frequency increases from low to high, as on a piano. The length
of notes in msecs can be specified by the -l option.The default length
is 300 msecs.

The amplitude of the generated sound is scaled to have a reasonable
maximum volume. The .wav sample size is always 16 bits.

Implemented by Martin Richards (c) March 2020.

Usage: from,to/K,notesperoctave=-n/N,secs=-s/N,
       rate=-r/N,stereo=-st/S,notemsecs=-l/N,-t/S

from                 The raster data file name, default RASTER.
to/K                 The .wav file, default RASTER.wav.
notesperoctave=-n/N  The number of notes per octave, default 12.
secs=-s/N            The length of the generated sound in seconds.
rate=-r/N            The .wav sample rate of 11025, 22050 or 44100.
mono=-m/S            Toggle mono and stereo
notemsecs=-l/N       The length of generated notes in msecs
-t/S                 Turn on tracing as a debugging aid.

Typical usage from a Linux bash shell is as follows:

rastsys                          Enter the rastering version of BCPL
raster                           Cause the next command to generate
                                 raster data.
origbcpl com/origbcpl.b to junk  Perform a typical command.
raster2wav -n 72 -s 60           Convert the raster data to a .wav file
                                 corresponding to sound lasting 60 seconds
                                 with 72 notes per octave.

History
26/02/2025
Modified the command arguments, again.
Changed the note generation algorithm, again.

02/02/2025
Save the old version in rast2wav01feb2025.b and started to Redesigned the
sound generation algorithm

24/02/2020
Made a major change to the way notes are selected.

31/01/2020
Renamed this file to rast2wav.b. The previous rast2wav.b has been
renamed bits2wav.b.

28/01/2020
Minor changes to match the modified RASTER file format.


The raster data file starts typically with

F21486045 M496083   K1000 S8

giving the maximum byte address references (addrmax), the number of
Cintcode instructions executed (countmax),  the number cintcode
instruction obeyed per raster line (kval) and the number of byte
addresses per raster line point (sval).

*/

SECTION "rast2wav"

GET "libhdr"

GLOBAL
{ // Global functions
  stdin:ug
  stdout
  fromfilename  // The raster data file
  fromstream
  wavfilename   // The .wav output file
  wavstream

  notes_per_octave // The number of notes per octave, typically 12 or 24.
  notemsecs        // The length in msecs of each note sounded.
                   // This can be set using the notemsecs=-l/N argument.
  notelen          // The sample length of each note sounded.
                   // It equals sample_rate*notemsecs/1000
  fnotelen         // = FLOAT notelen

  
  slctcnts
  tracing
  
  Lbuf             // Left and right sampe buffers for the next second
  Rbuf
  
  FLT maxsample     // To check for 16-bit signed integer overflow.
  FLT voladjustment // = 28_000.0/maxsample
  
  addOneSample
  addNoteSamples
  ptr2noteno
  
  wrsample
  rdn
  read_and_process_raster_data
  read_raster_params
  read_raster_line

  scanmemv          // the function that chooses which notes to
                    // activate based on addreess references made
		    // during the 32 most recent lraster lines.
  
  note2str

  count             // The current raster line number, the first has
                    // number one.
  fcount            // = FLOAT count

  prevcount
  
  instrcountmax     // Number of Cintcode instruction covered by the
                    // raster data.
  countmax          // The total number of raster lines in the raster data.
  FLT fcountmax     // = FLOAT countmax

  FLT ampmax        // The maximum sample value so far. It is used to
                    // scale the samples written to the .wav file.
		    // ampmax is initially set to 30_000.0
		    
  kval              // The number of obeyed Cintcode instructions per
                    // raster line.
  FLT fkval         // = FLOAT kval
  sval              // The number of address byte per raster line unit.

  wavmax            // The number of mono or stereo samples to be generated
                    // = totalsecs * sample_rate
  FLT fwavmax       // = FLOAT wavmax

  wavpos
  nextwavpos        // wavpos position for next scanmemv
                    // typically once every 50msecs
  bufupb
  fbufupb

  btc0              // Longitudinal digits used by poscounts(m)
  btc1
  btc2
  btc3
  btc4
  btc5

  diff              // = wavpos*countmax - count*wavmax
                    // If >=0    Call read_raster_line to read more
		    //           raster data.
		    //           Increment count by 1 and subtract
		    //           wavmax from diff.
		    // If <0     Call processnotev to extract one
		    //           sample from Lbuf and Rbuf
		    //           and write it to the .wav file.
		    //           Increment wavpos and
		    //           add countmax to diff.
		    // This is essentially Bressingham's algorithm for
		    // drawing a straight line from (0,0) to (countmax,wavmax).

  line_secs
  prevline_secs
  FLT fwavsecs      // Current time in the .wav data.
  pos2fsecs         // Function to map wavpos to fwavsecs
  
  mono              // =TRUE if generating mono sound, default FALSE
  stereo            // = ~ mono
  
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
  
  data_bytes   // = samples * bytes_per_sample

  ch
  
  Lbuf     // Vector of accumulated left samples
  Rbuf     // Vector of accumulated right samples
  bufp     // Position of the next sample
  bufq     // End of valid data in Lbuf and possibly Rbuf
  bufupb   // The upperbound of these buffers

  // These buffers can hold up to 10 seconds of samples.
  // Any activated note lasts for significantly less time.
  // The samples in these buffers are between bufp and bufq.
  // If bufq need to be incremented when it is equal to
  // bufupb+1 the samples are copied to the start of the
  // buffers and bufq is set to bufq-bufp and bufp is
  // set to zero.
  // If bufq>bufp the next sample to be written to the
  // .wav file is taken for position bufp.
  
  C2 // Lowest note generated   = 0
  C3
  C4 // Middle C
  C5
  C6
  C7 // Highest note generated

  lowC     // The actual range of
  highC    // notes that will be used.
  
  addrmax    // The highest byta address referenced
  memvupb    // = addrmax/sval.

  memv       // memv!i will hold a bit pattern showing the recent
             // history of memory accesses of byte addresses in the
	     // range sval*i to sval*i+sval-1. At each raster
	     // line memv!i is shifted left one place and a one
	     // placed in its least significant bit if one of its
	     // byte addresses have been accessed. It LS bit will
	     // otherwise be left as zero. This bit pattern is
	     // used by scanmemv to determine which notes should
	     // be activated.

  notev      // The value of notev!n where n is a note number has
             // the following meaning:
	     // < 0     Note n has just been triggered, but nay not
	     //         be sounded.
             // = 0     Note n is currently silent and not triggered.
	     // > 0     The hold the wavpos value that must be reach
	     //         before note n can be sounded again.
	     
  notevupb   // UPB of notev (=highC-lowC)

  bytes_per_note
  pincrement
  
  frqv       // The vector of note frequencies
  frqvupb
  initfrqv   // Create and set the vector holding all note frequencies

  waveformv    // Vector of waveforms
  waveform     // One cycle of floating point numbers
  waveformupb  // Typically 4096
  FLT fwaveformupb // = FLOAT waveformupb
  waveformsize // = waveformupb+1
  fwaveforsize // = FLOAT waveformsize
  
  harmonics    // Vector of harmonic amplitudes uused when
               // initialisinf waveform

  envelopev    // Vector os envelopes
  envelope     // Range 0 to envelopeupb, the elements are floats
  envelopeupb  // envelopeupb, typically 1024
  FLT fenvelopeupb // = FLOAT envelopeupb
}

LET start() = VALOF
{ LET argv    = VEC 50
  LET wv = VEC 20 // For upto 20 different waveforms
  LET ev = VEC 20 // For upto 20 different envelopes
  LET riffhdr = VEC 10
  LET format = "from,to/K,octavenotes=-n/N,*
               *secs=-s/N,rate=-r/N,mono=-m/S,*
	       *notemsec=-l/N,slctcnts=-b/N,-t/S"

//  abort(1234)
  waveformv := wv
  envelopev := ev
  FOR i = 0 TO 20 DO waveformv!i := 0 // No wave forms yet
  FOR i = 0 TO 20 DO envelopev!i := 0 // No evelopes yet
  stdout := output()
  stdin := input()

  IF rdargs(format,  argv, 50)=0 DO
  { writes("Bad arguments for RAST2WAV*n")
    RESULTIS 20
  }

  // Set the default values
  fromfilename := "RASTER"
  fromstream := 0
  wavfilename := "RASTER.wav"
  wavstream := 0
  slctcnts := 1<<6 | 1<<7 | 1<<8 | 1<<9 | 1<<13 | 1<<14

  frqv     := 0 // All vectors allocated by getvec
  waveform := 0
  envelope := 0
  notev    := 0
  memv     := 0
  Lbuf     := 0
  Rbuf     := 0
  
  notes_per_octave := 36      // The default value
  sample_rate      := 11_025  // Possibly changed by the rate-r/N option
  mono             := TRUE//FALSE   
  stereo           := ~mono
  notemsecs        := 300     // Default note length in msecs

  totalsecs        := 120 // secs
  
  countmax, addrmax := 0, 0 // These will be set by read_raster_params.
  instrcountmax := 0        //
  kval, sval := 0, 0        //
  ampmax := 30_000.0        // This is the raw maximum amplitude so far.
                            // Sample int .wav file are scaled to keep
			    // the amplitude down to 30_000.
  
  IF argv!0 DO fromfilename     := argv!0     // from
  IF argv!1 DO wavfilename      := argv!1     // to/K
  IF argv!2 DO notes_per_octave := !(argv!2)  // octavenotes=-n/N
  IF argv!3 DO totalsecs        := !(argv!3)  // secs=-s/N
  IF argv!4 DO sample_rate      := !(argv!4)  // rate=-r/N
  IF argv!5 DO mono := ~mono                  // mono=-m/S  toggle it
  stereo := ~mono
  IF argv!6 DO notemsecs        := !(argv!6)  // notemsecs=-l/N
  IF argv!7 DO slctcnts         := !(argv!7)  // slctcnts=-b/N
  tracing := argv!8                           // -t/S
  
  sample_rate := sample_rate > 44_000 -> 44_100, // Standardise the rate
                 sample_rate > 22_000 -> 22_050,
		                         11_025

  fsample_rate := FLOAT sample_rate

  notelen := sample_rate * notemsecs / 1000
  fnotelen := FLOAT notelen
  
  wavmax := sample_rate * totalsecs // The total number of .wav samples
  
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
  writef("%s %n-bit samples*n", (mono->"mono", "stereo"), bits_per_sample)

  writef("Total time %n seconds*n*n", totalsecs)

  // The octave numbers are: 2, 3, 4, 5, 6, 7
  C2 := 0
  C3 := 1 * notes_per_octave
  C4 := 2 * notes_per_octave
  C5 := 3 * notes_per_octave
  C6 := 4 * notes_per_octave
  C7 := 5 * notes_per_octave

  lowC  := C2   // Must be C2, C4 or C4
  highC := C6   // Must be C5, C6 or C7

  //writef("C2=%n C3=%n C4=%n C5=%n C6=%n C7=%n*n",
  //        C2,   C3,   C4,   C5,   C6,   C7)
  
  notevupb := highC - lowC
  frqvupb  := notevupb        // Each note has a frequency
  
  // The following variables are set from the raster data.
  writef("addrmax          = %9i*n", addrmax)
  writef("instrcountmax    = %9i*n", instrcountmax)
  writef("countmax         = %9i*n", countmax)
  writef("kval             = %9i*n", kval)
  writef("sval             = %9i*n", sval)
  writef("notes_per_octave = %9i*n", notes_per_octave)
  writef("notevupb         = %9i*n", notevupb)
  writef("memvupb          = %9i*n", memvupb)

//abort(8889)

  UNLESS initfrqv()     GOTO fin
//  writef("Returned from initfrqv*n"); abort(6001)

  // Create the collection of waveforms and envelopes
  UNLESS initwaveforms() GOTO fin
  //writef("Returned from initwaveforms*n")//; abort(6002)

  fcountmax        := FLOAT countmax
  fkval            := FLOAT kval
  ftotalsecs       := FLOAT totalsecs
  fsecs_per_fcount := ftotalsecs / fcountmax

  s020 := sample_rate/50 // samples per  20 msecs
  s025 := sample_rate/40 // samples per  25 msecs
  s050 := sample_rate/20 // samples per  50 msecs
  s100 := sample_rate/10 // samples per 100 msecs
  s200 := sample_rate/ 5 // samples per 200 msecs
  s250 := sample_rate/ 4 // samples per 250 msecs
  s500 := sample_rate/ 2 // samples per 500 msecs
  
  // Add one second at the end of the .wav data to allow
  // the sound to die away at the end.
  
  bytes_per_sample := bits_per_sample / 8  // Typically 2
  IF stereo DO bytes_per_sample :=  bytes_per_sample * 2
  bytes_per_second := sample_rate * bytes_per_sample

  wavmax := (sample_rate * totalsecs + 2*notelen) & -16
  // Including th closedown time.
  fwavmax := FLOAT wavmax
  wavpos := 0
  nextwavpos := wavpos + s050 // Scan memv 20 times per second.
  
  writef("*nsample_rate=%n totalsecs=%n notelen=%n*n",
            sample_rate,   totalsecs,   notelen)
  // Wavmax is the number of mono or stereo 16 bit samples
  // including the close down time.
  
  data_bytes := wavmax * bytes_per_sample
  // The close down time allowing notes that are sounding
  // at then end to die out.
  
  writef("wavmax  = %n*n", wavmax)
  //writef("Data bytes = %n*n", data_bytes)
  //writef("The samples including the closedown time: %n*n",
  //        wavmax)
  writef("Samples per raster line =%4.2f*n*n",
          FLOAT wavmax / FLOAT countmax)
	  
//  abort(9556)

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
  riffhdr!5  := mono -> 1<<16 | 1,   // PCM mono
                        2<<16 | 1    // PCM stereo
  riffhdr!6  := sample_rate          // samples per second.
  riffhdr!7  := bytes_per_second
  riffhdr!8  := bits_per_sample<<16 | bytes_per_sample
  riffhdr!9  := #x61746164           // d a t a
  riffhdr!10 := data_bytes

  // Output the header information.
  selectoutput(wavstream)
  FOR i = 0 TO 43 DO binwrch(riffhdr%i)
  //FOR i = 1 TO 200*wavmax BY 200 DO
  //{ LET w = i MOD 20000 - 10000
  //  binwrch(w)
  //  binwrch(w>>8)
  //}
  selectoutput(stdout)
//GOTO fin
  // What must now follow are bufupb+1 16-bit mono or stereo
  // samples.
  
  count         := 0   // Cintcode instruction count at the start
                       // of the next raster line

  line_secs     := 0
  prevline_secs := 0
  fwavsecs      := 0.0 // Current time in the .wav data.
  wavpos        := 0   // Current sample position in the .wav data.

  notev := getvec(notevupb)
  UNLESS notev DO
  { writef("Unable to allocate notev*n")
    GOTO fin
  }
  FOR n = 0 TO notevupb DO notev!n := 0
  // notev!n < 0 if note n is triggered
  //         =   if note n is not currently sounding
  //         >   if note n will sound until wavpos > notev!n


  memvupb := addrmax / sval
  memv := getvec(memvupb) // Each element corresponds to sval byte addresses
  UNLESS memv DO
  { writef("Unable to allocate memv*n")
    GOTO fin
  }
  FOR a = 0 TO memvupb DO memv!a := 0

  // memv!a holds a bit pattern specifying which of the recent raster lines
  // referenced byte addresses in memory region a.
  // Note that if (memv!a & #xFF)=1 addresses in the specified region
  // have be referenced in the current raster line period but not in the
  // previous seven.

  // Allocate and clear the .wav sample buffers
  bufupb := sample_rate * 10 // Allow 10 seconds worth of .wav samples.
  Lbuf := getvec(bufupb)
  Rbuf := getvec(bufupb)
  UNLESS Lbuf & Rbuf DO
  { writef("Unable to allocate Lbuf and Rbuf*n")
    GOTO fin
  }
//writef("bufupb=%n Lbuf=%n Rbuf=%n*n", bufupb, Lbuf, Rbuf)

  FOR i = 0 TO bufupb DO
  { Lbuf!i := 1_000_000_000.0
    Rbuf!i := 1_000_000_000.0
  }
  bufp, bufq := 0, 0 // The buffers initially contain no samples
  // These buffers are large enough to hold 10 seconds of samples.

  maxsample := 1_000.0 // Small value to debug the mechanism.
  voladjustment := 28_000.0 / maxsample
  
//abort(1116)  
  read_and_process_raster_data()

//  writef("maxsample=%9.1f*n", maxsample)

fin:
  selectoutput(stdout)
  selectinput(stdin)
  IF Lbuf      DO freevec(Lbuf)
  IF Rbuf      DO freevec(Rbuf)
  IF memv      DO freevec(memv)
  IF notev     DO freevec(notev)
  IF frqv      DO freevec(frqv)
  IF envelopev FOR i = 0 TO 20 IF envelopev!i DO freevec(envelopev!i)
  IF waveformv FOR i = 0 TO 20 IF waveformv!i DO freevec(waveformv!i)

  IF fromstream DO { endstream(fromstream)
                     fromstream := 0
		   }

  IF wavstream  DO { endstream(wavstream)
                     wavstream   := 0
                     writef("File %s written*n", wavfilename)
		   }
  RESULTIS 0   
}

AND initfrqv() = VALOF
{ // Return TRUE if successfu

  // Middle C is octave 4
  // The lowest not is C two octaves below middle C
  // The highest not is C three octaves abor middle C.
  // The frequency of middle C is about 261.63

  frqv := getvec(frqvupb) // Each note has a different frequency
  UNLESS frqv DO
  { writef("Unable to allocate frqv*n")
    RESULTIS FALSE
  }
  
  { LET n = 0
    LET FLT f = 261.63
    LET fv = getvec(notes_per_octave) // To hold frequencies from C4 to C5
    LET FLT ratio = sys(Sys_flt, fl_pow, 2.0, 1.0 / FLOAT notes_per_octave)
    // This calculates the ratio of frequencies between consecutive notes
    FOR n = 0 TO notes_per_octave DO
    { //writef("n=%i3  f=%9.2f*n", n, f)
      fv!n := f
      f := f * ratio
    }
    //abort(1001)
    FOR i = 0 TO notes_per_octave DO
    { LET FLT f = fv!i
      IF lowC=C2 DO
      { frqv!(i) := f / 4.0                    // C2 to C3
        frqv!(i+1*notes_per_octave) := f / 2.0 // C3 to C4
        frqv!(i+2*notes_per_octave) := f       // C4 to C5
      }
      IF lowC=C3 DO
      { frqv!(i) := f / 2.0                    // C3 to C4
        frqv!(i+1*notes_per_octave) := f       // C4 to C5
      }
      IF lowC=C4 DO frqv!(i) := f              // C4 to C5

      IF highC=C6 DO
      { frqv!(i+notevupb-1*notes_per_octave) := f * 2.0 // C5 to C6
      }
      
      IF highC=C7 DO
      { frqv!(i+notevupb-1*notes_per_octave) := f * 4.0 // C6 to C7
        frqv!(i+notevupb-2*notes_per_octave) := f * 2.0 // C5 to C6
      }
    }

    IF FALSE DO
    { // Swap some frequencies in frqv
      setseed(123456)
      swapfrequencies(100, notes_per_octave)       // An octave
      swapfrequencies( 50, notes_per_octave*7/12)  // A fifth
      swapfrequencies( 30, notes_per_octave*4/12)  // A minor third
    }
  }

  IF FALSE DO
  { FOR i = 0 TO frqvupb DO
    { IF i MOD notes_per_octave = 0 DO newline()
      writef(" %8.2f", frqv!i)
    }
    newline()
    abort(1933)
  }
  RESULTIS TRUE
}

AND initwaveforms() = VALOF
{ // Return TRUE is successful
  waveformupb := 4096 // All waveforms are the same size
  fwaveformupb := FLOAT waveformupb
  envelopeupb := 1024 // All envelopes are the same size
  fenvelopeupb := FLOAT envelopeupb
  
  waveformv!1 := mkwaveform(     5,  // The upb
                            12_000,  // Fundamental   Frequency
                            -5_000,  // Second        * 2
		             1_500,  // Third         * 3
		            -0_500,  // Fourth        * 4
		             0_400,  // Fifth         * 5
		             0_000)  // Sixth         * 6
  envelopev!1 := mkenvelope(4, 1.0, 0.5, 0.5, 0.5)

  waveformv!2 := mkwaveform(     5,  // The upb
                            10_000,  // Fundamental   Frequency
                            -4_000,  // Second        * 2
		             6_500,  // Third         * 3
		            -1_500,  // Fourth        * 4
		             0_400,  // Fifth         * 5
		             0_000)  // Sixth         * 6
  envelopev!2 := mkenvelope(8, 0.3, 1.0, 1.0, 0.7, 0.6,  0.6, 0.4, 0.2)

  UNLESS waveform!1 & envelopev!1 RESULTIS FALSE
  RESULTIS TRUE
}

AND mkwaveform(n, a1, a2, a3, a4, a5, a6, a7, a8, a9) = VALOF
{ // Return TRUE is successful
  LET v = getvec(waveformupb)
  LET harmonics = @n
  UNLESS v DO
  { writef("Unable to allocate a vector for a waveform*n")
    RESULTIS 0
  }
  FOR i = 0 TO waveformupb DO v!i := 0.0

  FOR h = 1 TO harmonics!0 DO
  { // harmonics -> [n, amp1, amp2,..., ampn]
    // Each harmonic of sine wave has a given amplitude.
    LET FLT amp = FLOAT harmonics!h
    LET FLT fh  = FLOAT h
    //writef("Harmonic %n amplitude=%i8*n", h, FIX amp)
    FOR i = 0 TO waveformupb DO
    { LET FLT angle = fh * 2.0 * 3.14159 * FLOAT i / fwaveformupb
      // angle = h * 2Pi/waveformupb
      v!i := v!i + amp * sys(Sys_flt, fl_sin, angle)
    }
  }
  //abort(1004)

  // Scale the resulting wave form to give a peak amplitude of 20_000.0
  maxsample := 1_000.0
  FOR i = 0 TO waveformupb DO
  { LET prevmaxsample = maxsample
    LET FLT sample = v!i
    IF  sample > maxsample DO maxsample :=  sample
    IF -sample > maxsample DO maxsample := -sample
    //UNLESS maxsample=prevmaxsample DO
    //{ // maxsample has just changed
    //  writef("i=%i2 sample= %7.2f  prevmaxsample= %7.2f*n",
    //        i,    sample,        prevmaxsample)
    //}
    //abort(5001)
  }

  voladjustment := 20_000.0 / maxsample
  FOR i = 0 TO waveformupb DO v!i := v!i * voladjustment
  //writef("maxsample=%8.2f voladjustment=%8.4f*n", maxsample, voladjustment)
  //abort(1003)

  IF FALSE DO // Output the waveform samples
  { FOR i = 0 TO waveformupb DO
    { IF i MOD 10 = 0 DO writef("*n%i4: ", i)
      writef(" %8.1f", v!i)
    }
    newline()
    abort(1196)
    writef("*nHarmonics:*n")
    FOR h = 1 TO harmonics!0 DO writef("     %n: %i", h, harmonics!h)
    newline()
    newline()
    FOR i = 1 TO waveformupb-1 DO
    { LET FLT a = v!(i-1)
      LET FLT b = v!i
      LET FLT c = v!(i+1)
      IF a<=0.0 & b>0.0  DO writef("%i4: a<=b>0     a=%8.2f*n", i-1, a)
      IF a<b & b>=c      DO writef("%i4: a<b>=c     b=%8.2f*n", i, b)
      IF a>0.0 & b<=0.0  DO writef("%i4: a>0 b<=0   b=%8.2f*n", i, b)
      IF a>b & b<=c      DO writef("%i4: a>b b<=c   b=%8.2f*n", i, b)
      IF b<0.0 & c>=-1.0 DO writef("%i4: b<0 c>-1   c=%8.2f*n", i+1, c)
    }
    abort(1002)
  }
  RESULTIS v
}

AND mkenvelope(n, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = VALOF
{ // Return the newly created envelope, or zero.
  // This creates a piecewise linear curve based on the given amplitude
  // values a1, a2,.., an, all in the range 0.0 to 1.0. These are
  // augmented by a0 and a(n+1) which are both zero. The amplitude
  // values are equally spaced and placed in a vector with boundd 0 and
  // 1024=envelopevupb.

  // For example mkenvelope(3, a1, a2, a3) creates the following
  // envelope stored ina vector with bounds 0 and 1024=envelopeupd.
  
  //   1.0             a1 * * * a2
  //                 * |        |  *
  //               *   |        |      a3
  //             *     |        |      |   *
  //   0.0     a0=0.0  |        |      |       a4=0.0
  //           |       |        |      |       |
  //           x0      x1      x2      x3      x4
  //           0                             1024=envelopeupb

  LET args = @n
  LET FLT  a = 0.0
  LET FLT  b = 0.0
  LET xa = 0
  LET xb = 0

  LET v = getvec(envelopeupb)
  UNLESS v DO
  { writef("Unable to allocate envelope*n")
    RESULTIS FALSE
  }

  FOR i = 1 TO n+1 DO
  { a := b
    b := i>n -> 0.0, args!i
    xa := xb
    xb := 1024*i / (n+1)
    FOR j = xa TO xb DO
    { LET FLT fj = FLOAT j
      LET FLT fxa, FLT fxb = FLOAT xa, FLOAT xb
      v!j := (a * FLOAT(xb-j) + b * FLOAT(j-xa)) / FLOAT(xb-xa) // Linear interpolation
      //writef("i=%n j=%n a=%6.3f b=%6.3f xa=%n xb=%n v!j=%6.3f*n",
      //        i, j, a, b, xa, xb, v!j)
      //abort(1117)
    }
  }

  IF FALSE DO
  { writef("*nEnvelope %n", n)
    FOR i = 1 TO n DO writef(" %6.3f", args!i)
    newline()
    abort(1119)
    FOR i = 0 TO envelopeupb DO
    { IF i MOD 8 = 0 DO writef("*n%i5: ", i)
      writef(" %6.3f", v!i)
    }
    newline()
    abort(1003)
  }
  
  RESULTIS v
}

AND swapfrequencies(n, interval) BE FOR i = 1 TO n DO
IF FALSE DO
{ LET k = randno(1000)          // 1 <= k <= 1000
  LET p = randno(frqvupb+1) - 1 // 0 <= p <= frqvvupb
  LET d = randno(interval)      // 1 <= r <= interval
  LET q = 0                     // 0 <= q <= frqvupb
  LET a, b = 0, 0

  TEST k > 500
  THEN { q := p + d
         IF q > frqvupb DO q := p - d
       }
  ELSE { q := p - d
         IF q < 0       DO q := p + d
       }
  a, b := frqv!p, frqv!q
  frqv!p, frqv!q := b, a
}

AND read_and_process_raster_data() BE
{ // This function call read_raster_line, scanmemv and
  // processnotev repeatedly.
  
  // read_raster_line reads on raster line from the current
  // input stream. It update the bit patterns held in memv
  // indicating which bye addresses have been referenced during
  // the last 32 raster line periods. Each period corresponds
  // to the execution of kval Cintcode istructions. Element i of
  // memv relates to byte addresses from i*sval to (i+1)*sval-1.
  // Every time read_raster_line is called count is incremented
  // by one.

  // The vector notev holds the status of every note that can be
  // sounded. Its covers notes from lowC (typically C2) to highC
  // (typically C7). Its bounds are 0 and notevupb (=highC=lowC).
  // If notev!n is positive it holds the value of wavpos that
  // must be reached before note n can next be sounded. When a
  // note is sounded notev!n will be set to wavpos+notelen and
  // nearby notes will be disabled by setting their notev values
  // appropriately.
 
  // scanmemv is called just after each call of read_raster_line
  // to determine which notes should potentially be activated,
  // based on the contents of memv. If wavpos>=notev!n note n is
  // either currently sounding or is disabled and so cannot be
  // activated. But otherwise its element in notev is given a
  // negative value indicating it should be activated now.
  // Different negative values give the various reasons why the
  // note should be activated. These may affect the volume and
  // duration of the note.
  
  // processnotev inspects notev!n for each note n and if negative
  // adds it wav samples added into the sound buffers Lbuf and
  // Rbuf. It sets notev!n to wavpos+len when len the the sample
  // length of the note and also update the notev elements of
  // nearby note to disable them for an appropriate time.
  // Each call of processnotev causes wavpos to be incremented
  // by one.
  
  // Which of read_raster_line or processnotev is called next
  // chosen to keep (count, wavpos) as close to the line from
  // (0,0) to (countmax,wavmax) as possible.

  //                                       * (countmax, wavmax)
  //                               *
  //                       * (count, wavpos)
  //               *
  //  (0,0) *
    
  // To achieve this the variable diff is used. It holds the
  // value wavpos*countmax-count*wavmax. Note that diff is negative
  // when count/countmax is less than wavpos/wavmax.

  // If diff>=0  Call read_raster_line, increment count and
  //             subtract wavmax from diff.
  // otherwise   Call processnotev, increment wavpos and add
  //             countmax to diff.

  diff := 0

  IF countmax<16 DO
  { writef("ERROR: countmax=%n which is too small*n", countmax)
    abort(999)
  }
  
  // Read the first 16 raster lines to initialise memv
  FOR i = 1 TO 15 DO
  { read_raster_line()
    count := count+1
  }
  
  UNTIL count >= countmax DO
  { 
    //writef("wavpos=%i5 countmax=%i5 count=%i5 wavmax=%i5*n",
    //        wavpos,    countmax,    count,    wavmax)
    //writef("diff = wavpos**countmax-count**wavmax = %i5*n", diff)

//abort(2296)
    
    TEST diff >= 0
    THEN { //writef("Calling read_raster_line*n")
           IF count MOD 1000 = 0 DO
	     writef("count = %i5 countmax=%n*n", count, countmax)
           read_raster_line()
	   count := count+1
	   IF count MOD 500 = 0 DO
	     writef("count=%n*n", count)
	   diff := diff - wavmax
  	   //abort(2665)

           // Do not necessarily call scanmemv every time
	   // read_raster_line is called.
	   //IF count MOD 4 = 0 DO
	   { //writef("Calling scanmemv not after read_raster_line*n")
	     scanmemv()
	     //writef("Calling processnotev*n")
             //abort(2669)
             processnotev()
 	     // Do not call scanmemv or processnotev again until
	     // after 20 msecs
	     nextwavpos := wavpos + s020
	     //abort(2660)
	   }
	 }
    ELSE { // Write the next sample taken from Lbuf (and possibly Rbuf).
           //IF wavpos MOD 10000 = 0 DO
	   //  writef("wavpos=%i9: Calling writeOneSample*n", wavpos)
           writeOneSample()
	   wavpos := wavpos+1
	   diff := diff + countmax
  	   //abort(2667)
         }
  }

  //IF count MOD 100 = 0 DO writef("count = %i6*n", count)
  
  // The raster stream is now exhausted
       
  // All that is now required is to call processnotev repeatedly until
  // all currently active notes have died away.

  UNTIL wavpos > wavmax DO // Write the close down samples
  { writeOneSample()
    wavpos := wavpos+1
    //writef("wavpos=%n wavmax=%n*n", wavpos, wavmax)
    //abort(5355)
  }
//abort(2297)
}

AND processnotev() BE
{ // Find all the notes in notev that have been marked for activation
  // and then add samples to Lbuf (and possibly Rbuf) provided they are
  // not to close to another note that is currently sounding.

  //writef("processnotev: entered*n")

IF FALSE DO
//IF wavpos MOD 10 = 0 DO
  { // Output the current state of notev 
    writef("wavpos=%n: count=%n  countv is as follows:", wavpos, count)
    FOR n = 0 TO notevupb DO
    { IF n MOD 19 = 0 DO newline()
      writef(" %i4", notev!n)
    }
    newline()
    abort(1199)
    IF FALSE DO
    FOR n = 0 TO notevupb DO
    { LET w = notev!n
      IF wavpos < w DO w := 15
      IF w<0 DO w := 1
      IF n MOD 12 = 0 DO wrch(' ')
      writef("%x1", w)
    }
    //newline()
    //abort(1000)
  }
  //abort(1007)

  FOR n = 0 TO notevupb DO //IF count>10 DO
  { LET mark = notev!n // This may have been set by scanmemv
    IF mark < 0 DO
    { // The latest raster line indicates that note n should be sounded.
      // It will add notelen .wav samples into the sound buffers.
      LET wavlim = wavpos + s050
      // but nearby notes will not be sounded again for at least
      // 1/3 of its note length.
      LET a = n-notes_per_octave /6 // The range of nearby notes
      LET b = n+notes_per_octave /6 // About a full tone.
      IF a < 0        DO a := 0
      IF b > notevupb DO b := notevupb
      //writef("processnotev: mark notes %n to %n as busy*
      //        * until wavpos=%n*n",
      //        a, b, wavlim)
      FOR i = a TO b DO notev!i := wavlim
      
      wavlim := wavpos + notelen/4
      //writef("processnotev: mark note %n as busy*
      //        * until wavpos=%n*n",
      //        a, b, wavlim)
      notev!n := wavlim

      // Add samples for note n into the sound buffers.
      //writef("processnotev: count=%n n=%i3 mark=%n sounding until %n*n",
      //                      count,   n, mark, wavpos+notelen)
      //abort(1010)
      // Choose the envelope and waveform based on the mark
      waveform, envelope := waveformv!1, envelopev!1
      SWITCHON mark INTO
      { DEFAULT:  ENDCASE
        CASE -1: waveform, envelope := waveformv!1, envelopev!1; ENDCASE
        CASE -2: waveform, envelope := waveformv!2, envelopev!2; ENDCASE
        //CASE -3: waveform, envelope := waveformv!3, envelopev!3; ENDCASE
        //CASE -4: waveform, envelope := waveformv!4, envelopev!4; ENDCASE
        //CASE -5: waveform, envelope := waveformv!5, envelopev!5; ENDCASE
        //CASE -6: waveform, envelope := waveformv!6, envelopev!6; ENDCASE
      }

      // Check that a suitable waveform is available.
      UNLESS envelope & waveform DO
      { writef("Envelope or waveform for note %n and mark = %n is missing*n",
                n, mark)
        abort(999)
      }
      
      // Add the note samples
      addNoteSamples(n)
      //writef("Samples for note %n added to the sound buffers*n", n)
      //abort(7115)
    }
  }
}

AND writeOneSample() BE
{ // Call wrsample once or twice to write a sample to the
  // .wav file.
  // If Lbuf contains any samples they will be between
  // positions bufp and bufq-1 in Lbuf and possibly Rbuf.
  // These buffers are empty if either bufq=0 or bufp>= bufq.
  // In fact bufp will never be greater than bufq.
  // The samples in Lbuf and Rbuf are unscaled and so have
  // to be multiplied by voladjustment whose value if based
  // on maxsample.
  //writef("writeonesample: count=%n memv=%n bufp=%n bufq=%n*n", count, memv, bufp, bufq)
  //abort(6119)
  TEST bufp < bufq
  THEN { // There is at least one sample in the buffers.
         LET FLT fsample = Lbuf!bufp
	 LET sample = FIX (fsample * voladjustment)
	 //writef("Calling wrsample, fsample=%7.2f voladjustment=%7.3f*n",
	 //                          fsample,      voladjustment)
	 //writef("wavpos=%n  Writing sample %i5 from Lbuf*n", wavpos, FIX fsample)
         wrsample(sample)                 // Write the left sample
         IF stereo DO
 	 { LET FLT fsample = Rbuf!bufp
	   LET sample = FIX (fsample * voladjustment)
	   wrsample(sample)               // Write the right sample
	 }
         bufp := bufp+1
	 IF bufp=bufq DO bufp, bufq := 0, 0 // The buffers are now empty
       }
  ELSE { wrsample(0)     // Write a null left sample
         IF stereo DO 
	 { wrsample(0)   // Write a null right sample
	 }
       }

  //newline()
  //abort(1008)
}

AND addOneSample(pos, FLT Lval, FLT Rval) BE
{ // This adds a sample into position bufp+pos
  // of Lbuf and possibly Rbuf.

  LET p = bufp+pos


  // Note that the samples in these buffers are always between
  // positions bufp and bufq-1.
  // If p >= bufq it is necessary to advance bufq initialising
  // the new location(s) with zero samples.
  // If bufq becomes greater then bufupb the samples must be
  // moved to the start of the buffers, setting bufp to zero
  // and modifying bufq appropriately.
  LET prevmaxsample = maxsample

IF FALSE DO
  { writef("addOneSample: count=%n pos=%n p=%n *
           *Lval=%9.2f Rval=%9.2f*n",
            count, pos, p,
	    Lval, Rval)
   // writef("maxsample=%7.2f voladjustment=%6.3f*n",
   //         maxsample,      voladjustment)
//abort(9922)
  }
  //writef("addOneSample: pos=%n Lval=%7.2f Rval=%7.2f bufp=%n bufq=%n*n",
  //                      pos,   Lval,      Rval,      bufp,   bufq)

//IF p=343 DO
//abort(6669)

  // First check that bufp+pos in in range 0 to bufq.
  IF p >= bufq DO
  { // We must advance bufq, but it may be at the end of the buffers.
    IF bufq>bufupb DO
    { // bufq can only be equal bufupb+1 since pos is only incremented
      // by one on each call.
      // Move the elements of Lbuf and Rbuf between bufp and bufq-1
      // to the start of the buffers.
      //writef("Moving samples from region (%n,%n) to (%n,%n)*n",
      //        bufp, bufq, 0, bufq-bufp-1)
      //abort(6640)
      TEST mono
      THEN FOR i = 0 TO bufq-bufp-1 DO // Copy mono samples
           { Lbuf!i := Lbuf!(i+bufp)
           }
      ELSE FOR i = 0 TO bufq-bufp-1 DO // Copy stereo samples
           { Lbuf!i := Lbuf!(i+bufp)
             Rbuf!i := Rbuf!(i+bufp)
	   }
      // Note that bufq-bufp is the number of samples in then buffers.
      bufq := bufq-bufp // Set the new value of bufq
      bufp := 0
      //writef("Setting bufp=%n and bufq=%n*n", bufp, bufq)
      //abort(2229)
    }

    // bufq is less then or equal to bufupb and so can be incremented.
    Lbuf!bufq := 0.0              // Clear the new sample position
    IF stereo DO Rbuf!bufq := 0.0
    //writef("advanvcing bufq from %n to %n*n", bufq, bufq+1)
    //writef("bufq!%n = %9.2f*n", bufq, Lbuf!bufq)
    bufq := bufq+1

    //writef("bufq incremented to %n and that location cleared*n", bufq)
  }

  p := bufp+pos
  //writef("Adding Lval= %7.2f and Rval=%7.2f to buffer position %n*n",
  //        Lval, Rval, p)

  // Check whether maxsample needs to be increased.
  // The samples are stored the buffers even when they are greater
  // than maxsample. The scaling will be done when the samples are
  // extracted from the buffer as they are and written to the .wav file.
  //writef("p=%n Lbuf!p=%7.2f + Lval=%7.2f => %7.2f*n",
  //        p,   Lbuf!p,        Lval,         Lbuf!p+Lval)
  Lval := Lbuf!p + Lval                        // The modified Lsample
  Lbuf!p := Lval
  IF   Lval >  maxsample DO maxsample :=  Lval
  IF   Lval < -maxsample DO maxsample := -Lval

  IF stereo DO
  { //writef("p=%n Rbuf!p=%7.2f + Rval=%7.2f => %7.2f*n",
    //        p,   Rbuf!p,        Rval,         Rbuf!p+Rval)
    Rval := Rbuf!p + Rval                      // The modified Rsample
    Rbuf!p := Rval
    IF Rval >  maxsample DO maxsample :=  Rval
    IF Rval < -maxsample DO maxsample := -Rval
  }
  //abort(2229)
  IF Lval > 1_000_000.0 DO abort(2230)

  IF maxsample > prevmaxsample DO
  { voladjustment := 28_000.0 / maxsample
    IF FALSE DO
    { writef("Lval=%9.2f Rval=%9.2f*n", Lval, Rval)
      writef("p=%n New maxsample = %8.2f  previously %8.2f*n",
              p, maxsample, prevmaxsample)
      writef("New voladjustment = %6.4f*n", voladjustment)
      abort(7266)
    }
  }

//  writef("addOneSample(%n, Lval=%7.2f, Rval=%7.2f) bufp=%n bufq=%n*n",
//          pos, Lval, Rval, bufp, bufq)
  //abort(2239)
}

AND addNoteSamples(n) BE //IF count>10 DO
{ // Note n has been marked for activation. This function
  // repeatedly calls addonesample to add the required
  // samples to thoses in the buffers Lbuf and Rbuf.
  // frqv!n is the frequency of the note.
  // notelen is the number of samples to add.
  // One cycle of the raw sound is held in waveform.
  // The amplitude envelope is in envelope. This is scaled to
  // cover the range 0 to notelen.
  // If generating stereo sound, the left and right samples are
  // adjusted using Ladj and Radj whose values depend on the
  // note number.
  
  LET FLT frq   = frqv!n
  LET FLT fn    = FLOAT n
  LET FLT flim  = FLOAT notevupb
  LET FLT Ladj  = (flim-fn) / flim
  LET FLT Radj  = fn        / flim

  //
  //writef("Adding note samples for note %n at wavpos=%n*n", n, wavpos)

  //writef("Adding samples for count=%i7  wavpos=%i7  note=%n notelen=%i5*n",
  //                           count,     wavpos,     n,      notelen)
  //abort(1728)
  
  FOR pos = 0 TO notelen DO
  { LET FLT fpos = FLOAT pos
    LET ep = FIX(fenvelopeupb * fpos / fnotelen) // Envelope subscript
    LET FLT voladj = envelope!ep
    // secs per notelen samples   = notelen/sample_rate
    // cycles per notelen samples = notelen*261/11025
    // scale up to waveform units ie mult by 4096
    //                            = notelen*261*4096/11025
    // Position of pos within waveform
    //                            = (pos*261*4096/11025) MOD 4096

    LET wp = FIX(fpos*frq*fwaveformupb/fsample_rate) MOD waveformupb
    LET FLT rawval = waveform!wp     // The next raw sample
    LET FLT val = rawval * voladj    // Apply the envelope
    LET FLT Lval = 0
    LET FLT Rval = 0
    //writef("fpos=%7.2f ep=%i4 fenvelopupb=%6.1f fnotelen=%6.1f voladj=%6.3f*n",
    //        fpos, ep, fenvelopeupb, fnotelen, voladj)
    //writef("secs per notelen samples=%7.3f*n",
    //        fnotelen/fsample_rate)
    //writef("cycles per notelen samples=%7.3f*n",
    //        fnotelen*frq/fsample_rate)
    //writef("scaled to waveform units=%10.1f*n",
    //        fnotelen*frq*fwaveformupb/fsample_rate)
    //writef("position within the waveform=%i4 waveformupb=%n*n",
    //        FIX(fnotelen*frq*fwaveformupb/fsample_rate) MOD waveformupb,
    //        waveformupb)
    //writef("rawval=%7.2f*n", rawval)
    //writef("After applying the envelope, val=%7.2f*n", val)
    TEST mono
    THEN { Lval := val
         }
    ELSE { Lval := val * Ladj    // Adjust stereo samples
           Rval := val * Radj
         }
    //writef("addNoteSamples: n=%n pos=%i4 waveform!%n=%7.2f*n",
    //        n, pos, wp, waveform!wp)
    //writef("Stereo Ladj=%7.3f Radj=%7.3f*n", Ladj, Radj)
    //writef("ep=%n wp=%n voladj=%7.4f*n", ep, wp, voladj)
    // Insert a sample into the buffers Lbuf and Rbuf
    //writef("Note %i2: pos=%i4 wavpos=%n  %9.2f  %9.2f*n",
    //        n,        pos,    wavpos,    Lval,  Rval)
//abort(6199)
    addOneSample(pos, Lval, Rval)
    //IF pos MOD 2 = 0 DO
    //abort(3843)

    //abort(1011)
  }
  //writef("%i7: count=%i5  Note %i2  frq=%7.2f*n",
  //        wavpos, count, n, frq)
//  writef("%i7: count=%i5  Note %i2  frq %7.2f   Samples added  bufp=%n bufq=%n*n",
//          wavpos, count, n, frq, bufp, bufq)
  //abort(1788)
  //abort(3844)
  
}

AND wrsample(w) BE
{ UNLESS -32_000 < w < 32_000 DO
  { writef("wrsample: wavpos=%n w=%n  ERROR too large*n", wavpos, w)
    abort(1999)
  }
  //writef("wrsample: wavpos=%i7 sample=%i7*n", wavpos, w)
  //IF wavpos MOD 20 = 0 DO abort(6677)
  selectoutput(wavstream)
  binwrch(w); binwrch(w>>8)
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
  
  // instrcountmax  The number of Cintcode instructions represented
  //                in the raster data.
  // countmax       =instrcountmax/kval, the number of raster lines.
  // addrmax        is the highest byte address accessed
  // memvupb        = addrmax/kval, the upb of memv
  // kval           is the number of Cintcode instructions obeyed
  //                per raster line.
  // sval           The number of address bytes represented by
  //                each point in the raster data.
  // The first line of a Raster file is of the form:

  //       F<instrcountmax> M<addrmax> K<kval> S<sval>
  
  instrcountmax  := 0
  countmax  := 0
  addrmax := 0
  memvupb := 0

  IF FALSE DO
  { instrcountmax  := 1_000_000
    countmax  := 1_000_000
    addrmax := 100_000 // max byte address
    memvupb := 0
    kval := 1000
    sval := 8
    fcountmax := FLOAT countmax
    fkval := FLOAT kval

    countmax := instrcountmax/kval
    memvupb := addrmax/ sval
  
    RESULTIS TRUE
  }
  
  ch := rdch() REPEATWHILE ch='*s'

  IF ch='F' DO
  { ch := rdch()
    instrcountmax := rdn()
  }
  
  WHILE ch=' ' DO ch := rdch()
  
  IF ch='M' DO
  { ch := rdch()
    addrmax := rdn()
  }
  
  WHILE ch=' ' DO ch := rdch()
  
  IF ch='K' DO
  { ch := rdch()
    kval := rdn()
  }
  WHILE ch=' ' DO ch := rdch()

  IF ch='S' DO
  { ch := rdch()
    sval := rdn()
  }

  UNLESS instrcountmax & addrmax DO
  { writef("Bad RASTER file*n")
    RESULTIS FALSE
  }
  
  fcountmax := FLOAT countmax
  fkval := FLOAT kval

  countmax := instrcountmax/kval
  memvupb := addrmax/ sval
  
  RESULTIS TRUE
}

AND read_raster_line() = VALOF
{ // This function reads one raster line od Wn and Bn items updating
  // memv appropriately. It does not suggest notes to be sounded since
  // this is now done by scanmemv.
  
  // Each raster line specifies the byte addresses that have been accessed
  // during the execution of Cintcode instructions during the time
  // corresponding to the line. This data is stored in memv!0 to memv!memvupb
  // where memvupb = addrmax/sval.

  // All entries are shifted left by one position and those that have been
  // referenced are then incremented by one. memv indicate which addresses
  // have been reference in the last few (typicall 32 or 64) raster lines.


  // Thus memv!a holds a bit pattern indicating which of the last few raster
  // lines referenced byte addresses in the range a*sval to a*sval+sval-1.
  
  LET a, b = 0, 0  // These are subscripts of memv that will specify ranges
                   // of byte address referenced during the current raster
		   // line period.
  //writef("read_raster_line: entered, count=%n*n", count)

  // First shift all elements of memv to the left by one porition.
  FOR p = memv TO memv+memvupb DO !p := !p << 1

  //FOR k = 0 TO 9 DO
  //writef("%i5: count=%i5: %i6 %32b*n", wavpos, count, 11130+k, memv!(11131+k))
  //abort(4492)
  
  { // Start of the raster line loop

    SWITCHON ch INTO
    { DEFAULT:
        sawritef("Bad ch '%c'*n", ch)
        abort(1111)

      CASE ' ':          // Ignore white space
      CASE '*c':
      CASE '*n':
        ch := rdch()
        LOOP

      CASE 'B':          // Black (address accessed) region
      { // Only notes corresponding to the start and end of this
	// region are activated.
	LET p = ?
        ch := rdch()
        b := a + rdn()
	//writef("B from %n to %n*n", a, b)
        FOR i = a TO b DO memv!i := memv!i + 1
	

        //p := muldiv(notevupb, a, memvupb)
	//IF wavpos >= notev!p & (memv!a & #xFF) = 1 DO
	//{ notev!p := -1
	//  //writef("Note %n marked  start address %n*n", p, a)
	//}
        p := muldiv(notevupb, b, memvupb)
	//IF wavpos >= notev!p & (memv!b & #xFF) = 1 DO
	//{ notev!p := -1
	//  //writef("Note %n marked  end   address %n*n", p, b)
	//}
        a := b           // Set a to start of the next region
//abort(7183)
	LOOP
      }

      CASE 'W':          // White (address not accessed) region
//wrch(ch)
        ch := rdch()
        b := a + rdn()
	//writef("W from %n to %n*n", a, b)
        a := b           // Set a to start of the next region
//abort(7184)
	LOOP

      CASE endstreamch:
        RESULTIS FALSE
	
      CASE 'N':          // End of a raster line period.
      CASE 'Z':
        //writef("%c*n", ch)
        ch := rdch()
	//abort(1358)
	RESULTIS TRUE
	
    }
  } REPEAT // Deal with the next item on the current raster line

  //abort(1357)
} REPEAT // Process the next raster line

AND scanmemv() BE
{ // This function is called just after each call of read_raster_line.
  // It selects which notes should potentially be activated based on
  // the bit patterns in memv. A note can only be activated if its
  // entry in notev is zero. When scanmemv suggests a note to be
  // activated it places a negative value in the appropriate entry of
  // notev indicating why the note should be activate. The different
  // values possibly affect the volume and length of notes to be
  // activated.
  
  //        -------------------------------------------------------
  // memv  |                                                       |
  //        ------------------------------------------------------*
  //        0                                                  memvupb
  //        -------------------------------------------------------
  // notev |       |       |       |       |       |       |       |
  //        ---------------------------------------------------*---
  //           0       1       2       3       4            notevupb

  // C2, C3,..., C7    Note numbers separated by notes_per_octave  eg 0 12 24 .. 60
  // C4 is middle C
  // lowC  is one of C2, c3 or C4
  // highC is one of C5, C6 or C7
  
  // addrmax  highest byte address referenced              eg 480_000
  // sval     number of address bytes per element of memv  eg     8
  // notes_per_octave                                      eg    24
  // memvupb = addmax / sval                               eg 60_000
  // notevupb = highC-lowC                                 eg 60

  // The size of an i/o buffer is 4096 words (or 64384 bytes). This
  // will correspond to one octave consisting of notes_per_ocatave
  // different notes. The byte range of each note, bytes_per_note
  // is thus:

  // bytes_per_note = 64384 / notes_per_octave

  // The following expression return the note number associated with
  // address a: (a / bytes_per_note) MOD notevupb.

  // The function scanmemv tests positions in memv. This done by selecting
  // positions in memv seperated by pincrement where

  // pincrement = bytes_per_note * memvupb / addrmax

  LET p      = 20         // The initial value of p
  LET plim   = memvupb-20 // The upper limit of p
  
  LET noteno = 0
  LET mark   = 0
  LET testcount = 0
  
  bytes_per_note := 64384 / notes_per_octave
  // ie one octave per 4096 words
  
  // Note: memvupb = addrmax / sval
  // ie one element of memv per sval bytes.
  // If pincrement=1 all elements between 20 and plim of memv
  // are   tested.
  pincrement := bytes_per_note / sval
  pincrement := pincrement / 2  // Adjust the number of tests per note
  IF pincrement<1 DO pincrement := 1
  pincrement := 10
  //writef("memvupb=%n notevupb=%n bytes_per_note=%n*n",
  //        memvupb,   notevupb,   bytes_per_note)
  //writef("pincrement=%n range of p is from %n to %n*n",
  //        testspernote,         p,   plim)
  //writef("count=%n countmax=%n*n", count, countmax)
  //abort(6671)

  { // Start of the REPEAT loop for p
    
    // p is a subscript of memv

    LET m = p
    
    IF m > plim RETURN  // All positions tested

    p := p + pincrement // The next test position

    ///IF (memv!m & #x0003_C000)=0 LOOP // The centre os the region
                                     // must be active.

    noteno := ptr2noteno(m) // Get the note number based on the
                            // subscript of memv.

    //writef("count=%i5 m=%i6  memvupb=%i5 noteno=%n*n", count, m, memvupb, noteno)
    //writef("addr=%i6 addrmax=%i6 bytes_per_note=%i6*n",
    //        m*sval,  addrmax,    bytes_per_note)
    //writef("addr/bytes_per_note=%i5  notevupb=%n => noteno=%n*n",
    //        m*sval / bytes_per_note, notevupb,      noteno)
    IF FALSE DO
    IF posbits(m) DO
    { prpos(m)
      poscounts(m)
      prbtcvalues()
      writef("scanmemv: count=%i7  instrs==%i7      addr=%i7 noteno=%i3*n",
                        count,     (count-16)*kval, m*sval,  noteno)
      //abort(1670)
    }
    //IF noteno=0 DO  abort(1770)

    IF wavpos < notev!noteno LOOP // This note is currently busy.

    // It is worth inspecting this location
    testcount := testcount+1

    //IF testcount MOD 10 = 0 DO// abort(6223)
    //writef("Testing testcount=%i3 noteno=%i3  position m=%n*n",
    //                testcount,    ptr2noteno(m),       m)
    //abort(6224)

    //writef("m=%i5  noteno=%i3 memv!m=%8b %8b %8b %8b*n",
    //        m,     noteno, memv!m>>24, memv!m>>16, memv!m>>8, memv!m)
    mark := testpos(m)
    UNLESS mark LOOP

    IF tracing DO
      prpos(m)
    // A pattern has matched so the chosen note should be activated.
    writef("count=%i5: instrs=%i7   addr=%i6    Note %i3 mark=%n*n",
            count,     (count-16)*kval, m*sval, noteno,  mark)
    //  UNLESS count=prevcount DO
    //  { abort(7810)
    //    prevcount := count
    //  }
    
    notev!noteno := mark
    LOOP
  } REPEAT
  
  //writef("Leaving scanmemv*n")
  //abort(5633)
}

AND posbits(m) = VALOF
{ LET res = 0
  FOR i = +10 TO -10 BY -1 DO
    res := res + bts(memv!(m+i))
  IF res DO writef("posbits => %n*n", res)
  RESULTIS res
}

AND poscounts(m) = VALOF
{ btc0, btc1, btc2, btc3, btc4, btc5 := 0, 0, 0, 0, 0, 0
  FOR i = +10 TO -10 BY -1 DO
  { LET w = memv!(m+i)
    LET c = 0
    //writef("i=%i3: w=%32b*n", i, w)
    //abort(8214)
    UNLESS w LOOP
    c    := btc0 & w
    btc0, w := btc0 XOR w, c
    //writef("btc0=%32b c=%32b*n",  btc0, c)
    //abort(8214)
    UNLESS w LOOP
    c    := btc1 & w
    btc1, w := btc1 XOR w, c
    //writef("btc1=%32b c=%32b*n",  btc1, c)
    //abort(8214)
    UNLESS w LOOP
    c    := btc2 & w
    btc2, w := btc2 XOR w, c
    //writef("btc2=%32b c=%32b*n",  btc2, c)
    //abort(8214)
    UNLESS w LOOP
    c    := btc3 & w
    btc3, w := btc3 XOR w, c
    //writef("btc3=%32b c=%32b*n",  btc3, c)
    //abort(8214)
    UNLESS w LOOP
    c    := btc4 & w
    btc4, btc5 := btc4 XOR w, c
    //writef("btc4=%32b c=%32b*n",  btc4, c)
    //writef("btc5=%32b*n",  btc5)
    //abort(8214)
  }
}

AND prbtcvalues() BE
{ FOR sh = 31 TO 0 BY -1 DO
  { LET val = btcvalue(sh)
    writef(" %i2", val)
    IF sh MOD 8 = 0 DO wrch(' ')
  }
  newline()
}

AND btcvalue(sh) = VALOF
{ LET val = btc0>>sh & 1
  UNLESS (btc1>>sh & 1)=0 DO val := val+ 2
  UNLESS (btc2>>sh & 1)=0 DO val := val+ 4
  UNLESS (btc3>>sh & 1)=0 DO val := val+ 8
  UNLESS (btc4>>sh & 1)=0 DO val := val+16
  UNLESS (btc5>>sh & 1)=0 DO val := val+32
//  newline()
//  writef(" %32b*n", btc0)
//  writef(" %32b*n", btc1)
//  writef(" %32b*n", btc2)
//  writef(" %32b*n", btc3)
//  writef(" %32b*n", btc4)
//  writef(" %32b*n", btc5)
//  writef("btcvalue: sh=%n => %n*n", sh, val)
//  abort(8823)
  RESULTIS val
}

AND prpos(m) BE
{ FOR i = +10 TO -10 BY -1 DO
  { LET w = memv!(m+i)
    FOR sh = 31 TO 0 BY -1 DO
    { writef("%n", (w>>sh) & 1) 
      IF sh MOD 8 = 0 DO wrch(' ')
    }
    IF i=0 DO writef("  m=%n memvupb=%n", m, memvupb)
    newline()
  }
}

AND testpos(m) = VALOF
{ // m is the subscript of memv corresponding to the region
  // being tested. This region is typically consists of a
  // 32x9 rectangle of bits representing recent and near future
  // address references close to the point in the raster image
  // being tested. This rectangle is held in 9 32-bit words.
  LET wp4, wp3, wp2, wp1, wp0 = 0, 0, 0, 0, 0
  LET wn1, wn2, wn3, wn4      = 0, 0, 0, 0
  LET v = @ wp4 // Vector of 9 consecutive words in memv
  LET sum = 0
  LET mark = 0
  LET q = memv + m - 4
  LET mask = #x00FF_FF00

  UNLESS q!4 RESULTIS 0

  IF bts(q!4|q!5)>16 RESULTIS 0
  
  //writef("count=%i5 instrs=%i7 ptr=%i6 addr=%i6*n",
  //        count, (count-16)*kval, q+4-memv, (q+4-memv)*sval)
  //FOR i = 0 TO 8 DO writef("q!%n %32b*n", i, q!i)
  //abort(5227)
	   
  wp4 := q!0 & mask
  wp3 := q!1 & mask
  wp2 := q!2 & mask
  wp1 := q!3 & mask
  wp0 := q!4 & mask
  wn1 := q!5 & mask
  wn2 := q!6 & mask
  wn3 := q!7 & mask
  wn4 := q!8 & mask

  mark := VALOF
  { LET mask1s = 0
    LET mask0s = 0

    mask1s := #b_00000000_00000001_10000000_00000000 // Want ones
    mask0s := #b_00000000_00111100_00111100_00000000 // Want zeroes
    FOR slope = -5 TO 5 DO
    { LET s = 0
      FOR i = +4 TO -4 BY -1 DO
      { LET sh = i*slope / 5
        LET w = memv!(m+i)
	//writef("w=%32b  sh=%n*n", w, sh)
        IF sh > 0 DO w := w>>sh
        IF sh < 0 DO w := w<<sh
	//writef("=>%32b*n", w)
	//writef("mask1s => %n*n", bts(w & mask1s))
	//writef("mask0s => %n*n", bts(w & mask0s XOR mask0s))
        s := s + bts(w & mask1s) * 6        // Count the wanted ones * 6
        s := s + bts(w & mask0s XOR mask0s) // Count the wanted zeroes
	//writef("s = %n*n", s)
	//abort(6117)
      }
      //abort(5291)
      IF s>100 DO
      { //writef("mask1s = %32b*n", mask1s)
        //writef("mask0s = %32b*n", mask0s)
	//IF tracing DO
        //writef("slope = %i2 s= %n*n", slope, s)
	//abort(5499)
        RESULTIS slope - 15
      }
    }

RESULTIS 0
    sum :=
         matchword(wp4, #b_00000000_00000000_11111111_00000000) +
         matchword(wp3, #b_00000000_00000001_11111110_00000000) +
         matchword(wp2, #b_00000000_00000011_11111100_00000000) +
         matchword(wp1, #b_00000000_00000111_11111000_00000000) +
         matchword(wp0, #b_00000000_00001111_11110000_00000000) +
         matchword(wn1, #b_00000000_00011111_11100000_00000000) +
         matchword(wn2, #b_00000000_00111111_11000000_00000000) +
         matchword(wn3, #b_00000000_01111111_10000000_00000000) +
         matchword(wn4, #b_00000000_11111111_00000000_00000000)
    IF sum>40 DO
    { writef("pat1 sum = %n => -1*n", sum)
      abort(1880)
      RESULTIS -1
    }

    sum :=
         matchword(wp4, #b_00000000_11111111_00000000_00000000) +
         matchword(wp3, #b_00000000_01111111_10000000_00000000) +
         matchword(wp2, #b_00000000_00111111_11000000_00000000) +
         matchword(wp1, #b_00000000_00011111_11100000_00000000) +
         matchword(wp0, #b_00000000_00001111_11110000_00000000) +
         matchword(wn1, #b_00000000_00000111_11111000_00000000) +
         matchword(wn2, #b_00000000_00000011_11111100_00000000) +
         matchword(wn3, #b_00000000_00000001_11111110_00000000) +
         matchword(wn4, #b_00000000_00000000_11111111_00000000)
    IF sum>40 DO
    { writef("pat2 sum = %n => -2*n", sum)
      abort(1881)
      RESULTIS -2
    }

    // Randomly activate a note if the current location is busy.
    //IF FALSE DO
    IF randno(1_000_000) < 200_000 DO
    { writef("pat3 => -3*n")
      RESULTIS -3
    }

    { LET ww = wp4|wp3|wp2|wp1|wp0|wn1|wn2|wn3|wn4
      ww := (ww | (ww+ww)) & #x00FF_FF00
      sum := bts(ww)
      IF 2 <= sum <= 6 DO
      { writef("pat4 sum = %n => -4*n", sum)
        abort(1882)
        RESULTIS -4
      }
    }
    
    writef("No match found => 0*n")   
    //prpat(w)
    //abort(8017)
    RESULTIS 0
  }

  // This point is unreachable.
  //writef("Note activated*n")
  //abort(1883)
  RESULTIS mark
}

AND matchword(w, bits) = VALOF
{ LET a = bts(w &  bits)
  LET b = bts(w & ~bits)
  //writef("%32b => %i3 score*n", w, a+b)
  //writef("%32b => %i3*n", w &  bits, a)
  //writef("%32b => %i3*n", w & ~bits, b)
  //abort(1884)
  RESULTIS a+b
}

AND bts(w) = VALOF
{ LET sum = 0
  WHILE w DO w, sum := w & (w-1), sum+1
  RESULTIS sum
}

AND btc(w, bits) = VALOF
{ //IF FALSE DO
  { writef("btc:      w="); prbits(w); newline()
    writef("btc:   bits="); prbits(bits); newline()
    writef("btc: w&bits="); prbits(w&bits)
    writef(" => %n/%n*n", bts(w & bits), bts(bits))
abort(9914)
  }
  RESULTIS bts(w & bits)
}

AND btc2(m, w, bits0, wc, bits1, bc) = VALOF
{ // This tests region w for bits in subregion bits0
  // testing whether the count is less than wc.
  // It then test region w for bits in subregion bits1
  // testing whether the count is greater than bc.
  // If both tests succeed the result is TRUE, otherwise
  // it is FALSE.
  // m*sval is the byte address close to the region being tested.
  LET res = bts(w & bits0)<wc & bts(w&bits1)>bc -> TRUE, FALSE
  IF FALSE DO
  { writef("btc2:      w="); prbits(w);     newline()
    writef("btc2:  bits0="); prbits(bits0); newline()
    writef("btc2:  bits1="); prbits(bits1); newline()
    writef("count=%i5 addr=%i6 testing %i2<%i2 and %i2>%i2 => ",
            count, m*sval, bts(w&bits0), wc, bts(w&bits1), bc)
    TEST res
    THEN writef("TRUE*n")
    ELSE writef("FALSE*n")
    prpat(w)
//abort(9915)
  }
  RESULTIS res
}

AND prbits(w) BE FOR sh = 20 TO 0 BY -5 DO
{ writef(" %5b", w>>sh)
}

AND prpat(w) BE
{ FOR sh = 24 TO 0 BY -1 DO
  { writef("  %c", (w>>sh & 1) = 0 -> '-', 'X')
    IF sh MOD 5 = 0 DO newline()
  }
}

AND ptr2noteno(p) =  ((p * sval) / bytes_per_note) MOD notevupb

AND pos2fsecs(p) = ftotalsecs * FLOAT p / FLOAT bufupb

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

