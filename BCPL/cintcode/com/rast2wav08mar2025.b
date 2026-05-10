/*
The previous version of this program is rast2wav01feb2025.b.

This program (rast2wav.b) converts a standard raster file, typically
called RASTER, to a .wav file.  The frequency of the sounds generated
is typically scaled so that low addresses correspond to frequencies
around two octaves below middle C, and the highest addresses have
frequencies around three octaves above middle C.  The user can specify
how many different notes there are per ocatave using the parameter
n. If n=12 only normal semitones are used.

The .wav file is scaled to last for a number of seconds specified by
the user. The default being 30 seconds. At the end there is a short
period the let the sound die out.  Normally the output will be mono at
11025 samples per second, but, the user can specify stereo an other
data rates. When generating stereo notes to range from left to right
as the frequency increases from low to high, as on a piano.

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
stereo=-st/S         Generate stereo sound.
notemsecs=-l/N       The length of generated notes in msecs
-t/S                 Turn on tracing as a debugging aid.

Typical usage from a Linux bash shell is as follows:

rastsys                          Enter the rastering version of BCPL
slow                             The slow interpreter must be used.
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
  fromfilename // The raster data file
  fromstream
  wavfilename   // The .wav output file
  wavstream

  notes_per_octave // The number of notes per octave, typically 12.
  notemsecs        // The length in msecs of each note sounded.
                   // This can be set using the notemsecs=-l/N argument.
  notelen          // The sample length of each note sounded.
                   // It equals sample_rate*notemsecs/1000
  fnotelen         // = FLOAT notelen
  //parama
  //paramb
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
  bufupb
  fbufupb
  
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
  wavpos            // Current sample position in the .wav data.
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

  frqv       // The vector of note frequencies
  frqvupb
  initfrqv
  
  waveform   // One cycle of floating point numbers
  waveformupb
  fwaveformupb // = FLOAT waveformupb
  
  harmonics  // Vector of harmonic amplitudes uused when
             // initialisinf waveform
  envelope   // envelope!0 to envelope!1024 hold envelope values
  envelopeupb
}

LET start() = VALOF
{ LET argv    = VEC 50
  LET riffhdr = VEC 10
  LET format = "from,to/K,octavenotes=-n/N,*
               *secs=-s/N,rate=-r/N,stereo=-st/S,*
	       *notemsec=-l/N,-t/S"

//  abort(1234)

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

  frqv     := 0 // All vectors allocated by getvec
  waveform := 0
  envelope := 0
  notev    := 0
  memv     := 0
  Lbuf     := 0
  Rbuf     := 0
  
  notes_per_octave := 12      // The default value
  sample_rate      := 11_025  // Possibly changed by the rate-r/N option
  stereo           := FALSE   // mono is the default
  notemsecs        := 100     // Default note length in msecs

  totalsecs := 5 //30
  
  countmax, addrmax := 0, 0 // These will be set by read_raster_params.
  instrcountmax := 0        //
  kval, sval := 0, 0        //
  ampmax := 30_000.0
  
  IF argv!0 DO fromfilename     := argv!0     // from
  IF argv!1 DO wavfilename      := argv!1     // to/K
  IF argv!2 DO notes_per_octave := !(argv!2)  // octavenotes=-n/N
  IF argv!3 DO totalsecs        := !(argv!3)  // secs=-s/N
  IF argv!4 DO sample_rate      := !(argv!4)  // rate=-r/N
  stereo := argv!5                            // stereo=-st/S
  mono   := ~stereo
  IF argv!6 DO notemsecs        := !(argv!6)  // notemsecs=-l/N
  tracing                       := argv!7     // t/S

  sample_rate := sample_rate > 44_000 -> 44_100,
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
  highC := C7   // Must be C5, C6 or C7

  //writef("C2=%n C3=%n C4=%n C5=%n C6=%n C7=%n*n",
  //        C2,   C3,   C4,   C5,   C6,   C7)
  notevupb := highC - lowC
  frqvupb  := notevupb   // Each note has a frequency
  
  // The following variables are set from the raster data.
  writef("addrmax          = %9i*n", addrmax)
  writef("instrcountmax    = %9i*n", instrcountmax)
  writef("countmax         = %9i*n", countmax)
  writef("kval             = %9i*n", kval)
  writef("sval             = %9i*n", sval)
  writef("notes_per_octave = %9i*n", notes_per_octave)
  writef("notevupb         = %9i*n", notevupb)
  writef("memvupb          = %9i*n", memvupb)

  writef("notes_per_octave = %n*n", notes_per_octave)

//abort(8889)

  UNLESS initfrqv()     GOTO fin
//  writef("Returned from initfrqv*n"); abort(6001)
  UNLESS initwaveform() GOTO fin
//  writef("Returned from initwaveform*n"); abort(6002)
  UNLESS initenvelope() GOTO fin
//  writef("Returned from initenvelope*n"); abort(6003)
  
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
  fwavmax := FLOAT wavmax
  wavpos := 0
  writef("sample_rate=%n totalsecs=%n notelen=%n*n",
          sample_rate,   totalsecs,   notelen)
  // Wavmax is the number of mono or stereo 16 bit samples
  // including the close down time.
  
  data_bytes := wavmax * bytes_per_sample
  // The close down time allowing notes that are sounding
  // at then end to die out.
  
  writef("notelen = %n*n", notelen)
  writef("wavmax  = %n*n", wavmax)
  writef("Data bytes = %n*n", data_bytes)
  writef("The samples including the closedown time: %n*n",
          wavmax)
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
  riffhdr!5  := mono -> 1<<16 | 1,   // stereo
                        2<<16 | 1    // mono
  riffhdr!6  := sample_rate          // samples per second.
  riffhdr!7  := bytes_per_second
  riffhdr!8  := bits_per_sample<<16 | bytes_per_sample
  riffhdr!9  := #x61746164           // d a t a
  riffhdr!10 := data_bytes

  // Output the header information.
  selectoutput(wavstream)
  FOR i = 0 TO 43 DO binwrch(riffhdr%i)
  selectoutput(stdout)

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

  writef("maxsample=%9.1f*n", maxsample)

fin:
  selectoutput(stdout)
  selectinput(stdin)

  IF Lbuf     DO freevec(Lbuf)
  IF Rbuf     DO freevec(Rbuf)
  IF memv     DO freevec(memv)
  IF notev    DO freevec(notev)
  IF envelope DO freevec(envelope)
  IF waveform DO freevec(waveform)
  IF frqv     DO freevec(frqv)

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

AND initwaveform() = VALOF
{ // Return TRUE is successful

  harmonics := TABLE     5,   // The upb
                    12_000,   // Fundamental   Frequency
                    -5_000,   // Second        x 2
		     1_500,   // Third         # 3
		    -0_500,   // Fourth        * 4
		     0_400,   // Fifth         * 5
		       000    // Sixth         * 6
  //harmonics!1 := 10_000*0
  //harmonics!2 := 10_000*0
  //harmonics!3 := 10_000*0
  //harmonics!4 := 10_000*0
  //harmonics!5 := 10_000*0
  //harmonics!6 := 10_000*1

  waveformupb := 4096
  fwaveformupb := FLOAT waveformupb
  waveform := getvec(waveformupb)
  UNLESS waveform DO
  { writef("Unable to allocate waveform*n")
    RESULTIS FALSE
  }

  FOR i = 0 TO waveformupb DO waveform!i := 0.0

  FOR h = 1 TO harmonics!0 DO
  { // Add each hamonic sine wave of given amplitude.
    LET FLT amp = FLOAT harmonics!h
    LET FLT fh = FLOAT h
    //writef("Harmonic %n amplitude=%i8*n", h, FIX amp)
    FOR i = 0 TO waveformupb DO
    { LET FLT angle = fh * 2.0 * 3.14159 * FLOAT i / FLOAT waveformupb
      // angle = h * 2Pi/waveformupb
      waveform!i := waveform!i + amp * sys(Sys_flt, fl_sin, angle)
    }
  }
  //abort(1004)


  // Scale the wave form to give a peak amplitude of 20_000.0
  maxsample := 1_000.0
  { 
    FOR i = 0 TO waveformupb DO
    { LET prevmaxsample = maxsample
      LET FLT sample = waveform!i
      IF  sample > maxsample DO maxsample :=  sample
      IF -sample > maxsample DO maxsample := -sample
      //UNLESS maxsample=prevmaxsample DO
      //  writef("i=%i2 sample= %7.2f  prevmaxsample= %7.2f*n",
      //        i,    sample,        prevmaxsample)
      //abort(5001)
    }
  }
  voladjustment := 20_000.0 / maxsample
  FOR i = 0 TO waveformupb DO waveform!i := waveform!i * voladjustment
  //writef("maxsample=%8.2f voladjustment=%8.4f*n", maxsample, voladjustment)
  //abort(1003)



IF FALSE DO // Output the waveform samples
  { FOR i = 0 TO waveformupb DO
    { IF i MOD 10 = 0 DO writef("*n%i4: ", i)
      writef(" %8.1f", waveform!i)
    }
    newline()
    abort(1196)
    writef("*nHarmonics:*n")
    FOR h = 1 TO harmonics!0 DO writef("     %n: %i", h, harmonics!h)
    newline()
    newline()
    FOR i = 1 TO waveformupb-1 DO
    { LET FLT a = waveform!(i-1)
      LET FLT b = waveform!i
      LET FLT c = waveform!(i+1)
      IF a<=0.0 & b>0.0  DO writef("%i4: a<=b>0     a=%8.2f*n", i-1, a)
      IF a<b & b>=c      DO writef("%i4: a<b>=c     b=%8.2f*n", i, b)
      IF a>0.0 & b<=0.0  DO writef("%i4: a>0 b<=0   b=%8.2f*n", i, b)
      IF a>b & b<=c      DO writef("%i4: a>b b<=c   b=%8.2f*n", i, b)
      IF b<0.0 & c>=-1.0 DO writef("%i4: b<0 c>-1   c=%8.2f*n", i+1, c)
    }
    abort(1002)
  }
  RESULTIS TRUE
}

AND initenvelope() = VALOF
{ // Return TRUE if successful.

  // All elements of envelope will be between 0.0 and 1.0

  //   1.0             * * * * *
  //                 *             *
  //               *                   *
  //             *                         *
  //   0.0     *                               *
  //           a       b       c               c
  //           0                             1024
  envelopeupb := 1024
  envelope := getvec(envelopeupb)
  UNLESS envelope DO
  { writef("Unable to allocate envelope*n")
    RESULTIS FALSE
  }

  { LET a = 0
    LET b = envelopeupb/6
    LET c = envelopeupb/3
    LET d = envelopeupb
    
    FOR i = a   TO b-1 DO envelope!i := FLOAT (i-a) / FLOAT (b-a)
    FOR i = b   TO c   DO envelope!i := 1.0
    FOR i = c+1 TO d   DO envelope!i := FLOAT (d-i) / FLOAT (d-c)
  }

  IF FALSE DO
  { writef("*nEnvelope*n")
    FOR i = 0 TO envelopeupb DO
    { IF i MOD 8 = 0 DO writef("*n%i5: ", i)
      writef(" %6.3f", envelope!i)
    }
    newline()
    abort(1003)
  }
  RESULTIS TRUE
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
  
  UNTIL count >= countmax DO
  { 
    //writef("wavpos=%i5 countmax=%i5 count=%i5 wavmax=%i5*n",
    //        wavpos,    countmax,    count,    wavmax)
    //writef("diff = wavpos**countmax-count**wavmax = %i5*n", diff)

//abort(2296)
    
    TEST diff >= 0
    THEN { //writef("Calling read_raster_line*n")
           IF count MOD 1000 = 0 DO writef("count = %i5 countmax=%n*n",
	                                    count, countmax)
           read_raster_line()
	   count := count+1
	   diff := diff - wavmax
	   scanmemv()
  	   //abort(2666)
	 }
    ELSE { //writef("Calling processnotev*n")
           processnotev()
	   wavpos := wavpos+1
	   diff := diff + countmax
  	   //abort(2667)
         }
  }

  IF count MOD 100 = 0 DO writef("count = %i6*n", count)
  
  // The raster stream is now exhausted
       
  // All that is now required is to call processnotev repeatedly until
  // all currently active notes have died away.

  UNTIL wavpos > wavmax DO // Write the close down samples
  { processnotev()  
    wavpos := wavpos+1
    //writef("wavpos=%n wavmax=%n*n", wavpos, wavmax)
    //abort(5355)
  }
//abort(2297)
}

AND processnotev() BE
{ // Find all the notes in notev that have been marked for activation
  // and then add samples to Lbuf (and Rbuf) provided they are not to
  // close to another note that is currently sounding.

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
  {   
    IF notev!n < 0 DO
    { // The latest raster line indicates that note n should be sounded.
      // It will sound for noyelen wav samples
      LET wavlim = wavpos + notelen/3
      // but nearby notes will not be sounded for at least a little while.
      LET a = n-notes_per_octave /4 // The range of nearby notes
      LET b = n+notes_per_octave /4
      IF a < 0        DO a := 0
      IF b > notevupb DO b := notevupb
      //writef("processnotev: n=%i2  marking unavailable until wavpos=%n notes %n to %n*n",
      //        n, wavlim, a, b)
      FOR i = a TO b DO notev!i := wavlim
      notev!n := wavpos+notelen
      //writef("processnotev: n=%i2 sounding until %n*n", n, wavpos+notelen)
      //writef("Adding samples for note %n to Lbuf=%n and Rbuf=%n, bufp=%n bufq=%n*n",
      //        n, Lbuf, Rbuf, bufp, bufq)
      //abort(1010)
      addNoteSamples(n) 
    }
  }
  //writef("Calling writeOneSample*n")
  // Write the next sample taken from Lbuf (and Rbuf).
  writeOneSample()
}

AND writeOneSample() BE
{ // If Lbuf contains any samples they will be between
  // positions bufp and bufq-1
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

IF FALSE  IF p=170 DO
  { writef("addOneSample: count=%n p=%n bufp=%n  bufq=%n *
           *Lval=%9.2f*n",
            count, p, bufp, bufq, Lval)
    writef("maxsample=%7.2f voladjustment=%6.3f*n",
            maxsample,      voladjustment)
    //abort(6639)
  }
  //writef("addOneSample: pos=%n Lval=%7.2f Rval=%n bufp=%n bufq=%n*n",
  //                      pos,   Lval,      Rval,   bufp,   bufq)

  // First check that bufp+pos in in range 0 to bufq.
  IF p >= bufq DO
  { // We must advance bufq, but it may be at the end of the buffers.
    IF bufq>bufupb DO
    { // bufq can only be equal bufupb+1 since pos is only incremented
      // by one on each call.
      // Move the elements of Lbuf and Ruf between bufp and bufupn
      // to the start of the buffers.
      writef("Moving samples from region (%n,%n) to (%n,%n)*n",
              bufp, bufq, 0, bufq-bufp-1)
      abort(6640)
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
      writef("Setting bufp=%n and bufq=%n*n", bufp, bufq)
      abort(2229)
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
  //writef("Adding samples %7.2f and %7.2f to buffer position %n*n",
  //        Lval, Rval, p)

  // Check whether maxsample needs to be increased.
  // The samples are stored the buffers even when they are greater
  // than maxsample. The scaling will be done when the samples are
  // extracted from the buffer as they are and written to the .wav file.
  //writef("p=%n Lbuf!p=%7.2f + Lval=%7.2f => %7.2f*n",
  //        p,   Lbuf!p,        Lval,         Lbuf!p+Lval)
  //abort(2230)
  Lval := Lbuf!p + Lval                        // The modified Lsample
  Lbuf!p := Lval
  IF   Lval >  maxsample DO maxsample :=  Lval
  IF   Lval < -maxsample DO maxsample := -Lval

  IF stereo DO
  { Rval := Rbuf!bufp + Rval                   // The modified Rsample
    Rbuf!p := Rval
    IF Rval >  maxsample DO maxsample :=  Rval
    IF Rval < -maxsample DO maxsample := -Rval
  }

  IF maxsample > prevmaxsample DO
  { voladjustment := 28_000.0 / maxsample
    IF FALSE DO
    { writef("p=%n New maxsample = %8.2f  previously %8.2f*n",
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
  // repeatedly calls insertonesample to insert the required
  // samples into the buffers Lbuf and Rbuf.
  // frqv!n is the frequency of the note.
  // notelen is the number of samples to generate.
  // One cycle of the sound is held in waveform.
  // The aplitude envelope is in envelope. This is scaled to
  // cover the range 0 to notelen.
  // If generating stereo sound the left and right samples are
  // adjusted using Ladj and Radj whose values depend on n.
  
  LET FLT frq   = frqv!n
  LET FLT fn    = FLOAT n
  LET FLT flim  = FLOAT notevupb
  LET FLT Ladj  = (flim-fn) / flim
  LET FLT Radj  = fn        / flim

  //
//  writef("Adding note samples for note %n at wavpos=%n*n", n, wavpos)

//  writef("Adding samples for count=%i7  wavpos=%i7  note=%n notelen=%i5*n",
//                             count,     wavpos,     n,      notelen)
  //abort(1728)
  
  FOR pos = 0 TO notelen DO
  { LET FLT fpos = FLOAT pos
    LET ep = FIX(1024.0 * fpos / fnotelen) // Envelope subscript
    LET FLT voladj = envelope!ep
    // secs per notelen samples   = notelen/11025
    // cycles per notelen samples = notelen*261/11025
    // scale up to waveform units ie mult by 4096
    //                            = notelen*261*4096/11025
    // Position of pos within waveform
    //                            = (pos*261*4096/11025) MOD 4096

    LET wp = FIX(fpos*frq*fwaveformupb/fsample_rate) MOD 4096
    LET FLT Lval = waveform!wp     // The next raw sample
    LET FLT Rval = 0
    //writef("fpos=%7.2f ep=%i4 envelopupb=%i5 voladj=%6.3f*n",
    //        fpos, ep, envelopeupb, voladj)
    //writef("secs per notelen samples=%7.3f*n",
    //        fnotelen/fsample_rate)
    //writef("cycles per notelen samples=%7.3f*n",
    //        fnotelen*frq/fsample_rate)
    //writef("scaled to waveform units=%10.1f*n",
    //        fnotelen*frq*fwaveformupb/fsample_rate)
    //writef("position within the waveform=%i4 waveformupb=%n*n",
    //        FIX(fnotelen*frq*fwaveformupb/fsample_rate) MOD waveformupb,
    //        waveformupb)
    //writef("raw Lval=%7.2f %x8*n", Lval)
    Lval := Lval * voladj          // Apply the envelope
    UNLESS mono DO // Adjust stereo samples
    { Rval := Rval * Radj
      Lval := Lval * Ladj
    }
    //writef("addNoteSamples: n=%n pos=%i4 waveform!%n=%7.2f*n",
    //        n, pos, wp, waveform!wp)
    //writef("Ladj=%7.3f Radj=%7.3f*n", Ladj, Radj)
    //writef("ep=%n wp=%n voladj=%7.4f*n", ep, wp, voladj)
    // Insert a sample into the buffers Lbuf and Rbuf
    //writef("Note %i2: pos=%i4  %9.2f  %9.2f*n",
    //        n,        pos,     Lval,  Rval)
    addOneSample(pos, Lval, Rval)
    //IF pos MOD 2 = 0 DO
    //abort(3843)

    //abort(1011)
  }
  writef("%i7: count=%i5  Note %i2  frq=%7.2f*n",
          wavpos, count, n, frq)
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
  //IF wavpos MOD 100 = 0 DO
    //writef("wrsample: wavpos=%i7 sample=%i7*n", wavpos, w)
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

  abort(1357)
} REPEAT // Process the next raster line

AND scanmemv() BE
{ // This function is called just after each call of read_raster_line.
  // It selects which notes should potentially be activated based on
  // the bit patterns in memv. A note can only be activated if its
  // entry in notev is zero. When scanmemv suggests a note to be
  // activated it places a negative value in the appropriate entry of
  // notev indicating why the note should be activate. The different
  // values affect the volume and length of notes to be activated.
  
  //        -------------------------------------------------------
  // memv  |                                                       |
  //        ------------------------------------------------------*
  //        0                                                  memvupb
  //        -------------------------------------------------------
  // notev |       |       |       |       |       |       |       |
  //        ---------------------------------------------------*---
  //           0       1       2       3       4            notevupb

  // addrmax  highest byte address referenced                      eg 60000
  // sval     number of byte addresses per element of memv         eg    10
  // notes_per_octave                                              eg    12
  // C2, C3,..., C7    Note numbers separated by notes_per_octave  eg 0 12 24 .. 60
  // C4 is middle C
  // locC  is one of C2, c3 or C4
  // highC is one of C5, C6 or C7
  // memvupb = addmax / sval                                       eg 6000
  // notevupb = highC-lowC                                         eg 60
  // so each element of notev relates to                           eg 
  // memvupb/notevupb elements of memv                             eg 6000/60 = 100

  // The mapping of subscripts of memv to note numbers is done by
  // ptr2noteno(addr) where 0 <= addr <= addrmax.
  // Typically ptr2noteno(addr) = muldiv(notevupb, addr, addrmax)
  // but other versions may be used.

  LET maskl  = #xFFFE_0000
  LET maskm  = #x0001_8000
  LET maskr  = #x0000_7FFF
  LET masklr = maskl | maskr

  LET pincrement = memvupb / (3 * notevupb) // Typically 3 tests
                                            // per note
  LET plim = memvupb - pincrement
  LET p = pincrement
  
IF count MOD 100000 = 0 DO
{ writef("scanmemv: count=%n*n", count)
  abort(6671)
}
IF FALSE DO
//IF count>320 DO // A debugging aid
  { writef("count=%i5*n", count)
    FOR i = 445_502/sval TO 445_702/sval DO
    { LET a = i * sval
      LET val = memv!i
      { writef("%i6 %i6:  %32b*n", i, a, val)
      }
      //abort(4481)
    }
    newline()
    abort(4482)
    
  }

//  abort(7810)
  WHILE p < plim DO
  { // p is a subscript of memv
    LET noteno = ptr2noteno(p) // Choose the note related to the
                               // position in memv.
    LET mark = 0 // Set to a negative value if the selected note
                 // is activated.

    LET m = memv + p // Abs addresses near element p
    LET q = m - 10
    LET r = m + 10

    p := p + pincrement


    IF count = -2 DO
    { notev!C4 := -1
      writef("Note C4 activated, count=%n wavpos=%n until wavpos=%n*n",
              count, wavpos, wavpos+notelen)
      //abort(3337)
    }
    IF count = -300 DO
    { notev!C5 := -1
      writef("Note C5 activated, count=%n wavpos=%n until wavpos=%n*n",
              count, wavpos, wavpos+notelen)
      //abort(3338)
    }
    IF count = -600 DO
    { notev!C6 := -1
      writef("Note C6 activated, count=%n wavpos=%n until wavpos=%n*n",
              count, wavpos, wavpos+notelen)
      //abort(3339)
    }
    //RETURN
    
    //writef("scanmemv: memvupb=%n, notevupb=%n pincrement=%n*n",
    //                  memvupb,    notevupb,   pincrement)
    //writef("scanmemv: p=%n plim=%n m=%n q=%n r=%n noteno=%n notev!noteno=%n*n",
    //                  p,   plim,   m,   q,   r,   noteno,   notev!noteno)

    IF wavpos < notev!noteno LOOP // Only consider values of corresponding
                                  // to notes that are not disabled.

    // Look for patterns like
    //    00000000 00000000 0XXXXXXX XXXXXXX   around p-20
    //    00000000 0000000X X0000000 0000000   at p
    //    XXXXXXXX XXXXXXX0 00000000 0000000   around p+20
    // or
    //    XXXXXXXX XXXXXXX0 00000000 0000000   around p-20
    //    00000000 0000000X X0000000 0000000
    //    00000000 00000000 0XXXXXXX XXXXXXX   around p+20
    // If each line at least one X must be a 1
    
    
    IF     (!m & maskm)  = 0 LOOP
    UNLESS (!m & masklr) = 0 LOOP

    mark := -1
    IF FALSE DO
    FOR i = -1 TO +1 DO
    { LET a = q!i  // Positions arount m-10
      LET b = r!i  // Positions around m+10
      UNLESS (a&maskl) =0 & (a&maskr)~=0 &
             (b&maskl)~=0 & (b&maskr) =0 DO
      { mark := 0
        BREAK
      }
      //writef("p=%n i=%i2 a=%32b   b=%32b*n", p, i, a, b)  
    }

    IF FALSE DO
    UNLESS mark DO
    { mark := -2
      FOR i = -1 TO +1 DO
      { LET a = q!i
        LET b = r!i
        writef("mark=%i2 q=%n a=%32b   b=%32b*n", mark, q, a, b)
        UNLESS (a&maskl)~=0 & (a&maskr) =0 &
               (b&maskl) =0 & (b&maskr)~=0 DO
        { mark := 0
          BREAK
        }
        //writef("p=%n i=%i2 a=%32b   b=%32b*n", p, i, a, b)  
      }
    }

    UNLESS mark LOOP
    //writef("p=%n     !m=%32b  !m=%32b*n", p, !m, !m)
    
    notev!noteno := mark
    
    //writef("noteno=%i2 for addr=%n   count=%n        mark=%n*n",
    //        noteno,        p*sval/4, count,          mark)
    //abort(6624)
//    IF 77530 <= p <= 77550 DO
IF FALSE DO
    { writef("count=%i5 memv=%n p=%n addr %n*n", count, memv, p, p*sval)
      FOR i = p-10 TO p+10 DO
      { LET a = (i-memv) * sval
        LET val = !i
        { writef("%i6 %i6:  %16b %16b*n", i, a, val>>16, val)
        }
        //abort(4481)
      }
      newline()
      abort(4482)
    
    }
  }
  //writef("Leaving scanmemv*n")
  //abort(5633)
}

AND ptr2noteno(p) =  muldiv(notevupb, p, memvupb)

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

