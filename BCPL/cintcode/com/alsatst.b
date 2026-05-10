/*
This progam is designed to test the BCPL ALSA facilities
including wav_input, wave_output, midi_input and midi_output.

Typically load the BCPL ALSA library as a separate section,
butat the current time all ALSA features are controlled by
calls of the form sys(Sys_alsa, fno, args ...) and so there
is no need to load alsa.b
*/

//GET "libhdr"

//MANIFEST { s_alsabase=300 }

//GET "alsa.h"
//GET "alsa.b"

//.

GET "libhdr"

MANIFEST { s_alsabase=300 }
// Since the BCPL alsa library currently uses no global variables and
// defines no global functions the setting of s_alsabase has no effect.

GET "alsa.h"

/*
This only runs on cintsys if it was build with the ALSA asound library.
This can be installed by the following shell command.

  sudo apt-get install libasound2-dev

Currently a suitable cintsys can be built by

make -f Makefile-alsa clean
make -f Makefile-alsa

In due course Makefile-alsa will replace Makefile, and it will
be assumed the the package libasound2-dev has been installed.
If it has not been installed you must ad the line

#define NOALSA

near the top of cintsys.h.

*/

GLOBAL{
  stdin:ug
  stdout
  initspace
  spacev
  spacep
  spacet
  newvec
  
  notecov
  noteampv
  
  notecofn12
  notecofn10
  notecofn8
  notecofn6
  notecofn4
  
  freqtab    // The elements are scaled integers with 3 digits
             // after the decimalpoint
  setfreqtab

  fromname
  toname
  towavname
  micname
  msecs
  freq             // The frequency of genertedsound
  rate             // This can be specified by the user but will
                   // be overridden if the samples come from a
		   // .wav file. It may also be overridden when
		   // reading data from the microphone.
  sinamp
  triamp
  sqamp
  randamp
  notelo; notehi   // The range of Midi notes being tested
  cycles           // Numbe of cycles use by the note recognition algorithm
  cofn             // =4, 6, 8, 10 or 12
  debug

  sxvupb           // The two word control block for the self expanding
  sxvv             // vector to hold 32-bit samples
  
  sxv              // Will be set to @sxvupb
  
  getwav           // Read a .wav file
  
  buf              // Buffer of most recent samples. Stereo sample wiil
                   // be averaged. If the samples were created using the
		   // self expanding vextor sxv, buf will equal sxvv. At
		   // the end buf not sxvv will be freed using freevec.
  bufupb           // = buf!0 the number of samples in buf
  
  updatebuf        // Read more samples into buf from the microphone.
		   
  days; msecs; filler  // The date stamp of the latest sample in buf
}

LET initspace(upb) = VALOF
{ writef("initspace(%n): entered*n", upb)
  spacev := getvec(upb)
  spacet := spacev+upb
  spacep := spacet
  RESULTIS spacev
}

LET newvec(upb) = VALOF
{ LET v = spacep-upb-1
  IF v<spacev DO
  { writef("More space needed*n")
    abort(999)
  }
  spacep := v
  RESULTIS v
}

LET start() = VALOF
{ //
  //
  LET err  = initspace(500_000) // Initialise space for newvec 
  LET argv = newvec(50)
  LET str  = newvec(64) // Allow string of at least 256 bytes.
                        // It is used when converting C strings
	 	        // to BCPL strings.

  // file      is the filename of a .wav file used to fill a buffer buf
  //           with samples converted to signed 32-bit integer. Stereo
  //           data takes the average of the left and right channels.
  // to/K      specifies the normal output of this program.
  // towav/K   specifies the name of a .wav file that will correspond
  //           to the samples placed in buf.
  // mic/K     is the microphone device name. The arguments file and
  //           mic must not both be given.
  // msecs/K/N specified how many samples are to be placed in buf.
  //           If msecs is zero and mic is given, that samples from
  //           the microphone are analysed in real time. But if msecs
  //           in non zero, samples corresponding to this length of
  //           time are placed in buf.
  //           If msecs is non zero and neither file nor mic is
  //           given, msecs worth of zero samples are placed in buf.
  // freq/K/N  unless mic is given and msecs is zero, this specified
  //           the frequency of data added to the samples in buf
  //           when sin, tri, sq or rand is given.
  // rate/K/N  if file is not given this specifies the sample rate
  //           of data in buf. The default rate is 44100.
  // sin/K/N   unless mic is given and msecs is zero, this is the
  //           amplitude of sine data to be added to smaples in buf.
  // tri/K/N   unless mic is given and msecs is zero, this is the
  //           amplitude of triangular data to be added to samples
  //           in buf.
  // sq/K/N    unless mic is given and msecs is zero, this is the
  //           amplitude of square wave data to be added to samples
  //           in buf.
  // rand/K/N  unless mic is given and msecs is zero, this is the
  //           amplitude of random wave data to be added to samples
  //           in buf.
  // lo/K/N    is the midi number of the lowest note to be analysed.
  // hi/K/N    is the midi number of the highest note to be analysed.
  // -c/K/N    this specified the number of cycles to inspect when
  //           determining the amplitude of a particular note.
  // -cf/K/N   if given this must have one of the following values
  //           4, 6, 8, 10 or 12 which is the number of sample
  //           points to inspect per cycle when determining the
  //           amplitude of a note at a given note.
  // -d/S      causes debugging output to be generated.
  
  UNLESS rdargs("from,to/K,towav/K,mic/K,msecs/K/N,*
                *freq/K/N,rate/K/N,sin/K/N,tri/K/N,sq/K/N,rand/K/N,*
                *lo/K/N,hi/K/N,-c/K/N,-cf/K/N,-d/S", argv, 50) DO
  { writef("Bar arguments for alsatst*n")
    RESULTIS 0
  }

  notecov := newvec(127) // For the note amplitude coroutines
  
  UNLESS notecov DO
  { writef("Unable to allocate notecov*n")
    GOTO fin
  }
  FOR note = 0 TO 127 DO notecov!note := 0

  fromname  := 0
  toname    := 0
  towavname := 0
  micname   := 0
  msecs     := 0
  freq      := 440_000
  rate      := 44100
  sinamp    := 0
  triamp    := 0
  sqamp     := 0
  randamp   := 0
  notelo    := 60-24
  notehi    := 60+36
  cycles    := 24
  cofn      := 8
  debug     := FALSE

  IF argv!0  DO fromname        := argv!0    // from
  IF argv!1  DO toname          := argv!1    // to/K
  IF argv!2  DO towavname       := argv!2    // towav/K
  IF argv!3  DO micname         := argv!3    // mic/K
  IF argv!4  DO msecs           := !argv!4   // msecs/K/N
  IF argv!5  DO freq            := !argv!5   // freq/K/N
  IF argv!6  DO rate            := !argv!6   // rate/K/N
  IF argv!7  DO sinamp          := !argv!7   // sin/K/N
  IF argv!8  DO triamp          := !argv!8   // tri/K/N
  IF argv!9  DO sqamp           := !argv!9   // sq/K/N
  IF argv!10 DO randamp         := !argv!10  // rand/K/N
  IF argv!11 DO notelo          := !argv!11  // lo/K/N
  IF argv!12 DO notehi          := !argv!12  // hi/K/N
  IF argv!13 DO cycles          := !argv!13  // -c/K/N
  IF argv!14 DO cofn            := !argv!14  // -cf/K/N
  debug                         :=  argv!15  // -d/S

  IF debug DO
  { newline()
    IF fromname  DO writef(" fromname=%s",  fromname)
    IF toname    DO writef(" toname=%s",    toname)
    IF towavname DO writef(" towavname=%s", towavname)
    IF micname   DO writef(" micname=%s",   micname)
    IF msecs     DO writef(" msecs=%n",     msecs)
    IF freq      DO writef(" freq=%n",      freq)
    IF rate      DO writef(" rate=%n",      rate)
    newline()
    IF sinamp    DO writef(" sinamp=%n",    sinamp)
    IF triamp    DO writef(" triamp=%n",    triamp)
    IF sqamp     DO writef(" sqamp=%n",     sqamp)
    IF randamp   DO writef(" randamp=%n",   randamp)
    IF notelo    DO writef(" notelo=%n",    notelo)
    IF notehi    DO writef(" notehi=%n",    notehi)
    IF cycles    DO writef(" cycles=%n",    cycles)
    IF cofn      DO writef(" cofn=%n",      cofn)
  }
  newline()

  sxvupb, sxvv := 0, 0 // Initialise the control block for
  sxv := @sxvupb       // for the self expanding sample vector.
  buf, bufupb := 0, 0
  
  FOR i = 0 TO 64 DO str!i := 0
  UNLESS sys(Sys_alsa, alsa_avail) DO
  { writef("The ALSA library is not available*n")
    RESULTIS 0
  }
  //writef("The ALSA library is available*n*n")

  //sys(Sys_alsa, alsa_setscheduler) // Does not work.

  setfreqtab()
  
  IF fromname DO
  { IF micname DO
    { writef("Error: from and mic cannot both be given*n")
      GOTO fin
    }
    getwav(fromname, sxv)
    // buf will be set to sxv!1 and bufupb will be its upperbound.
  }
//abort(1006)

  FOR n = notelo TO notehi DO
  { notecov!n := initco((cofn=4  -> notecofn4,
                         cofn=6  -> notecofn6,
                         cofn=7  -> notecofn8,
                         cofn=10 -> notecofn10,
                         cofn=12 -> notecofn12,
			 notecofn8),
                        500,       // Coroutine stack size
                        freqtab!n, // The frequency eg 440_000
			rate,
		        cycles,    // The cycles
		        n)         // The midi note number

    //writef("noteco %n: Created co=%n, result2=%n*n", n, notecov!n, result2)
  }
  writef("All note coroutines have been created*n")
//  abort(1005)

  IF micname DO
  { IF debug DO
    FOR n = 0 TO 1 TEST sys(Sys_alsa, alsa_longname, n, str)
    THEN { writef("*nCard %n has name: %s*n", n, str)
           //abort(1000)
         }
    ELSE { writef("*nCard %n does not exist*n", n)
           //abort(1000)
         }

    readmicsamples()
    UNLESS msecs GOTO fin
    // msecs is non zero so readmicsamples will have allocated
    // buf and set bufupb.
  }

  // If buf is non zero it will contain 32-bit signed samples
  // and bufupb will be its upperbound.
  
  writef("Testing buf=%n bufupb=%n*n", buf, bufupb)
  abort(7233)
  UNLESS buf DO
  { // No samples were obtained from either the .wav file or
    // the microphone, so allocate buf now and fill it with
    // samples initialised toe zero ready to be modified by
    // the effect of sin, tri, sq and rand.
    UNLESS msecs DO msecs := 10_000
    bufupb := muldiv(rate, msecs, 1000)
    buf := getvec(bufupb)
    UNLESS buf DO
    { writef("Error: Unable to allocate a buffer of size %n*n", bufupb)
      GOTO fin
    }
    buf!0 := bufupb
    FOR i = 1 TO bufupb DO buf!i := 0
    writef("Just allocated buf=%n bufupb=%n*n", buf, bufupb)
  }

  
  IF sinamp DO
  { //       * * --------------------  sinamp
    //     *     *
    //   *         *
    //  *           *
    // *-------------*-------------*
    // |              *           *| 
    // |               *         * |
    // |                 *     *   |
    // |                   * *  ----- -sinamp
    // |                           |
    // 0                           c
    LET c = muldiv(rate,         // eg rate=44100   c=100_227 scaled
                   1_000_000,
		   freq)         // eg 440_000  scaled
    LET p = 1_000 // Scaled with 3 digits after the decimal point
    LET sinampf = FLOAT sinamp
    LET FLT pi2 = FLOAT(2 * 3_14159) / FLOAT 1_00000
    LET FLT pi2byc = pi2 / FLOAT c
    //writef("rate=%n freq=%n c=%n*n", rate, freq, c)
    //abort(4321)
    FOR i = 1 TO bufupb DO
    { LET FLT w = sys(Sys_flt, fl_sin, pi2byc * FLOAT p)
      buf!i := buf!i + FIX(w * sinampf)
      //writef("i=%n p=%n c=%n c2=%n bu!i=%n*n", i, p, c, c2, buf!i)
      //IF i MOD 30 = 0 DO abort(5432)
      p := p+1_000
      IF p > c DO   // after the decimal point.
        p := p-c    // c is scaled in the same way
    }
  }

  IF triamp DO
  { //         *  ------------------------   triamp
    //       * | *
    //     *   |   *
    //   *     |     *
    // *-------|-------*---------------*
    // |       |       | *           * |
    // |       |       |   *       *   |
    // |       |       |     *   *     |
    // |       |       |       *       | -- -triamp
    // |       |       |       |       |
    // 0      c/4     c/2    3c/4      c  Sample positions scaled
    
    LET c = muldiv(rate,         // eg rate=44100   c=100_227 scaled
                   1_000_000,
		   freq)         // eg 440_000  scaled
    // c is a scaled integer with 3 fractional decimal digits
    LET cby4  = c/4
    LET cby2  = c/2
    LET c3by4 = 3*c/4
    LET p = 0
    writef("rate=%n freq=%n c=%n*n", rate, freq, c)
    //writef("rate=%n freq=%n c=%n*n", rate, freq, c)
    //abort(4321)
    FOR i = 1 TO bufupb DO
    { LET w = 0     // To hold the sample value
      p := p+1000   // p is scaled with 3 decimal digits
      IF p > c DO   // after the decimal point.
        p := p-c    // c is scaled in the same way
      IF p <= cby4         DO w := p
      IF cby4 < p <= c3by4 DO w := cby2-p
      IF c3by4 < p <= c    DO w := p-c
      
      buf!i := buf!i + triamp * w * 4 / c
      //writef("i=%n p=%n c=%n c2=%n bu!i=%n*n", i, p, c, c2, buf!i)
      //IF i MOD 30 = 0 DO abort(5432)
    }
  }
  
  IF sqamp DO
  { // * * * * * * * * * --------------------  sqamp
    // *               *
    // *               *
    // *               *
    // *---------------*---------------*
    // |               *               *  
    // |               *               *
    // |               *               *
    // |               * * * * * * * * *  --- -sqamp
    // |               |               |
    // 0              c/2              c
    LET c = muldiv(rate,         // eg rate=44100   c=100_227 scaled
                   1_000_000,
		   freq)         // eg 440_000  scaled
    // c is a scaled integer with 3 fractional decimal digits
    LET c2 = c/2
    LET p = 0
    //writef("rate=%n freq=%n c=%n*n", rate, freq, c)
    //abort(4321)
    FOR i = 1 TO bufupb DO
    { p := p+1000   // p is scaled with 3 decimal digits
      IF p > c DO   // after the decimal point.
        p := p-c    // c is scaled in the same way
      buf!i := buf!i + (p<c2 -> sqamp, -sqamp)
      //writef("i=%n p=%n c=%n c2=%n bu!i=%n*n", i, p, c, c2, buf!i)
      //IF i MOD 30 = 0 DO abort(5432)
    }
  }

  IF randamp DO
  { writef("Random wave added*n")
    FOR i = 1 TO bufupb DO
      buf!i := buf!i + randno(2*randamp+1) - randamp
  }
  
  IF debug DO
  { writef("buf=%n*n", buf)
    IF buf DO writef(" buf!0=%n*n", buf!0)
  }
abort(22334)
  IF towavname DO wrwavfile(towavname)
  
  IF buf DO displaynotes(buf, buf!0)

fin:
  writef("*nend of test*n")
//  abort(6457)
  IF notecov FOR n = 0 TO 127 IF notecov!n DO
  {   deleteco(notecov!n)
      //writef("Deleted co %n,n=%n*n", notecov!n, n)
  }
  IF buf    DO freevec(buf)
  IF spacev DO freevec(spacev)
  //abort(4991)
  RESULTIS 0
}

AND readmicsamples() BE
{ // The device name of the microphone has been given and
  // placed in micname. It is typically hw:1.
  // Microphone samples are to be read, but the effect depends
  // on whether msecs was zero.
  // If msecs is zero microphone samples are read repeatly
  // and pushed into buf which is allocated with a upb large
  // enough to hold 2 seconds worth of samples. It repeatedly
  // outputs information about which notes are currently
  // recognised.
  // If msecs is non zero samples corresponding to that length
  // of time are read from the microphone and placed in buf.
  
  LET micCB = VEC 20
  LET channels     = 1         // 1=Mono 2=Stereo
  LET periodframes = rate/2    // ie 1/2 second
  
  LET periodbytes = periodframes * channels * 2 // Number of bytes per period
                                                // assuming 16-bit samples.
  LET periodwords = (periodbytes>>B2Wsh)+1 // Safe size in words of a buffer
                                           // to hold one periods
  LET periodbuf   = getvec(periodwords-1)  // BCPL vector for one period of
                                           // H/W frames

  AND bufp = ?                     // Number of frames in buf
  LET days, msecs, dummy = 0, 0, 0 // For the date stamp of the most
                                   // recent sample in buf
  LET totalsamples = 0                               

  AND res = 0
  LET k1, k2, k3, k4 = 0, 0, 0,0
  writef("*nTesting microphone: %s*n", micname)
  //abort(1000)

  // Choose the size of buf
  TEST msecs
  THEN bufupb := msecs*rate*channels // upb of buf for msecs worth of frames
  ELSE bufupb := 2000*rate*channels  // upb of buf for 2 secs worth of frames
  // Note that the frames will have been been converted to mono.
  
  buf := getvec(bufupb)              // Buffer for the mono samples.
  
  FOR i = 0 TO 20     DO micCB!i := 100+i
  FOR i = 1 TO bufupb DO buf!i   := (i-1) MOD 201 - 100
  bufp  := 0
  buf!0 := bufp // buf is initially empty
  
  // Initialise the wav_input control block for the microphone
  
  micCB!0 := channels     // channels is 1 or 2, mono or stereo
  micCB!1 := rate         // rate samples or pairs per second
  micCB!2 := periodframes // Number of frames in a period
  micCB!3 := periodbytes  // Size of a period in bytes
  micCB!4 := periodwords  // Size of a period in words
  micCB!5 := periodbuf    // The BCPL vector to hold a period

  // micCB is passed as an argument when reading microphone samples
  // or closing the microphone stream.
  
  //writef("micCB: channels=%n rate=%n *
  //       *periodframes=%n periodbytes=%n periodwords=%n periodbuf=%n*n",
  //       channels, rate, periodframes, periodbytes, periodwords, periodbuf)

  //writef("alsatst: Calling alsa_open_wav_input, *"%s*", *
  //	 *micCB=%n)*n*n", micname, micCB)

  //Open the microphone stream
  UNLESS sys(Sys_alsa, alsa_open_wav_input,micname,  micCB)
  { writef("Failed to open the microphone stream %s*n", micname)
    RETURN
  }

  writef("The microphone stream %s was successfully opened*n", micname)

  WHILE totalsamples < 4*rate*channels DO
  { // Read outstanding frames into buf
  
    //writef("alsatst: Calling sys(Sys_alsa, alsa_wav_read, *
    //	 *micCB=%n buf=%n bufupb=%n)*n*n", micCB, buf, bufupb)

    // The buffer buf currently contains buf!0 samples.
    // Read more samples if any are available.
    res := sys(Sys_alsa, alsa_wav_read, micCB, buf, bufupb)
    //writef("alsatst: res=%n*n", res)
    //abort(1357)
    // res<0  error
    // res=0  No new frames read
    // res>0  The number of new frames received
    //writef("alsatst: res=%n*n", res)
    //abort(5678)
    k1 := k1+1
    IF res<0 BREAK
    k2:= k2+1
    IF res=0 DO
    { // Delay a little while for more frames to arrive
      k3 := k3+1
      //writef("alsatst: Delaying for 50 msecs*n")
      //writef("bufp=%n bufupb=%n*n", buf[0], bufupb)
      //delay(500)
      LOOP
    }
    k4 := k4+1
    totalsamples := totalsamples+res
    bufp := buf!0
    // The most recent samples are now in buf!1 to buf!bufp
    //writef("totalsamples=%n*n", bufp, totalsamples)
    //FOR i = 0 TO 15 DO
    //{ IF i MOD 16 = 0 DO writef("*n%i5: ", i)
    //  writef(" %i5", buf!i)
    //}
    //newline()
  }

  bufp := buf!0
  // The most recent samples are now in buf!1 to buf!bufp
  writef("k1=%n k2=%n k3=%n k4=%n*n", k1, k2, k3, k4)
  writef("buf=%n bufp=%n bufupb=%n*n", buf, bufp, bufupb)
//  abort(7456)
  //FOR i = 0 TO 127 DO
  //{ IF i MOD 16 = 0 DO writef("*n%i5: ", i)
  //  writef(" %i5", buf!i)
  //}
  //newline()
  
  //writef("*nalsatst: Calling: sys(Sys_alsa, alsa_close_wav_input, micCB)*n")
  sys(Sys_alsa, alsa_close_wav_input, micCB)
  //abort(3456)
  RETURN
}

AND displaynotes(v, upb) BE
{ buf, bufupb := v, upb
  FOR p = 1 TO upb BY 44100/20 DO
  { writef("%6.2d ", (100*p)/44100)
    FOR note = notelo TO notehi DO
    { LET amp = (callco(notecov!note, p) -1000) / 25
      LET ch = '-'
      SWITCHON note MOD 12 INTO
	     { DEFAULT: ENDCASE
	       CASE  1:
	       CASE  3:
	       CASE  6:
	       CASE  8:
	       CASE 10: ch := '**'
	     }
      IF note=60 DO ch := '.'
      IF 1  <= amp <=  9 DO ch := amp+'0'
      IF 10 <= amp <= 35 DO ch := amp-10+'A'
      IF amp > 35 DO ch := '#'
      wrch(ch)
      //writef(" amp=%n*n", amp)
      //abort(92345)
    }
    newline()
  }
}




/*
  { LET days0, msecs0, dummy0 = 0, 0, 0
    LET days1, msecs1, dummy1 = 0, 0, 0
    LET co = 0
    FOR n = 0 TO 127 DO
    { co := notecov!n
      IF co BREAK
    }
    IF co DO
    { LET count = instrcount(callco, co, 1)
        
      { datstamp(@days0)
        //writef("days0=%n msecs0=%n*n", days0, msecs0)
        FOR p = 1 TO 1_000 DO
	{ LET amp = callco(co, p MOD 44100)
	  //IF p=1 DO writef("p=%n amp=%n/%n*n", p, amp, result2)
	}
        datstamp(@days1)
        //writef("days1=%n msecs1=%n*n", days1, msecs1)
      } REPEATUNTIL days0=days1
      writef("*nfreq=%5.3d rate=%n cycles=%n *
         *a=%n b=%n randamp=%n cofn=%n*n",
          freq,rate,cycles,a,b,randamp,cofn)
      writef("notecofn%n executes %n Cintcode instructions *
             *taking %n microseconds*n", cofn, count, msecs1-msecs0)
    }
*/

AND setfreqtab() BE
{ // Set freqtab so that freqtab!n = 1000 * the note frequency
  // where n is the MIDI note number. n=60 for middle C (C4).
  // Piano lowest note:   21 (0A)
  // Horn  lowest note:   35 (1B)
  // Flute highest note:  76 (7C)
  // Piano highest note: 108 (8C)
  
  freqtab := TABLE
  //  C        C#       D        D#       E        F
  //  F#       G        G#       A        A#       B
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

AND notecofn12(args) = VALOF
{ // The note coroutines are created by calls such as
  // notecov!n := initco(notecofn12,500,freq,rate,cycles,note)
  // args points to freq.
  LET frq    = args!0 // eg 440_000
  LET rate   = args!1 // eg 44100
  LET cycles = args!2 // eg 24
  LET note   = args!3 // MIDI note number, eg 69 for 4A

  // This function uses 10 of the 12 equally spaced sample points per
  // cycle of the given frequency. It tests each of 12 phases each
  // based on the sample points. The phase that gives the greatest
  // amplitude is chosen.
  // 
  // It must first initialise the coroutine and then enter a
  // loop to receive a position in buf and return the
  // estimated amplitude of the given frequency at that position
  // with the phase returned in result2. The upb of buf is held in
  // bufupb.

  LET c = muldiv(rate, 1_000_000, frq)
  // c is a scaled number with 3 digits after the decimal point.
  LET amp, phase = 123, 0
  //writef("notecofn12: frg=%5.3d rate=%n cycles=%n note=%n*n",
  //        frq, rate, cycles, note)
  //writef("notecofn12 %n: about to call cowait(%n) for the first time*n",
  //        note, amp)
  //abort(1004)

  { // The main loop
    LET p = cowait(amp) // Subscript of buf
                        // Upb of buf is bufupb

    // Amplitudes and phases for sample points in
    // the first half cycle
    LET a0, ph0 = 0, 0
    LET a1, ph1 = 0, 0
    LET a2, ph2 = 0, 0
    LET a3, ph3 = 0, 0
    LET a4, ph4 = 0, 0
    LET a5, ph5 = 0, 0

    // Sample vectors for each of the 10 sample points
    LET sv0 = buf+p
    LET sv1  = sv0 +    c / 12000
    LET sv2  = sv0 +  2*c / 12000
    LET sv3  = sv0 +  3*c / 12000
    LET sv4  = sv0 +  4*c / 12000
    LET sv5  = sv0 +  5*c / 12000
    LET sv6  = sv0 +  6*c / 12000
    LET sv7  = sv0 +  7*c / 12000
    LET sv8  = sv0 +  8*c / 12000
    LET sv9  = sv0 +  9*c / 12000
    LET sv10 = sv0 + 10*c / 12000
    LET sv11 = sv0 + 11*c / 12000

    // Sums over the cycles at each sample point
    LET s0,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11 = 0,0,0,0,0,0,0,0,0,0,0,0
    LET q = 0 // Scaled with 3 digits after the decimal point
    
    IF p + c*cycles/1000 > bufupb LOOP
    
    FOR c = 1 TO cycles DO
    { LET t = q / 1000
      s0  := s0  + sv0!t
      s1  := s1  + sv1!t
      s2  := s2  + sv2!t
      s3  := s3  + sv3!t
      s4  := s4  + sv4!t
      s5  := s5  + sv5!t
      s6  := s6  + sv6!t
      s7  := s7  + sv7!t
      s8  := s8  + sv8!t
      s9  := s9  + sv9!t
      s10 := s10 + sv10!t
      s11 := s11 + sv11!t
      q := q + c
    }

    // Calculate the ampliture for each phase in the
    // first half cycle
    a0, ph0 := s1  +s2  +s3  +s4  +s5  -s7 -s8 -s9 -s10 -s11, 0
    a1, ph1 := s0  +s1  +s2  +s3  +s4  -s6 -s7 -s8 -s9  -s10, 1
    a2, ph2 := s11 +s0  +s1  +s2  +s3  -s5 -s6 -s7 -s8  -s9,  2
    a3, ph3 := s10 +s11 +s0  +s1  +s2  -s4 -s5 -s6 -s7  -s8,  3
    a4, ph4 := s9  +s10 +s11 +s0  +s1  -s3 -s4 -s5 -s6  -s7,  4
    a5, ph5 := s8  +s9  +s10 +s11 +s0  -s2 -s3 -s4 -s5  -s6,  5

    // If the amplitude is negative the phase is
    // in the second half cycle
    IF a0<0 DO a0, ph0 := -a0, 6
    IF a1<0 DO a1, ph1 := -a1, 7
    IF a2<0 DO a2, ph2 := -a2, 8
    IF a3<0 DO a3, ph3 := -a3, 9
    IF a4<0 DO a4, ph4 := -a4, 10
    IF a5<0 DO a5, ph5 := -a5, 11

    // Choose the phase that gives the greatest amplitude
    amp, phase := a0, ph0
    IF a1>amp DO amp, phase := a1, ph1
    IF a2>amp DO amp, phase := a2, ph2
    IF a3>amp DO amp, phase := a3, ph3
    IF a4>amp DO amp, phase := a4, ph4
    IF a5>amp DO amp, phase := a5, ph5

    // Return the phase and amplitude by a call of cowait
    amp := amp/cycles/10   // 10 of the 12 sample points were used
    result2 := phase
  } REPEAT
}

AND notecofn10(args) = VALOF
{ // The note coroutines are created by calls such as
  // notecov!n := initco(notecofn10,500,freq,rate,cycles,note)
  // args points to freq.
  LET frq    = args!0 // eg 440_000
  LET rate   = args!1 // eg 44100
  LET cycles = args!2 // eg 24
  LET note   = args!3 // MIDI note number, eg 69 for 4A

  // This function uses 8 of the 10 equally spaced sample points per
  // cycle of the given frequency. It tests each of 10 phases each
  // based on the sample points. The phase that gives the greatest
  // amplitude is chosen.
  // 
  // It must first initialise the coroutine and then enter a
  // loop to receive a position in buf and return the
  // estimated amplitude of the given frequency at that position
  // with the phase returned in result2. The upb of buf is held in
  // bufupb.

  LET c = muldiv(rate, 1_000_000, frq)
  // c i a scaled number with 3 digits after the decimal point.
  LET amp, phase = 0, 0
  //writef("notecofn10: frg=%5.3d rate=%n cycles=%n note=%n*n",
  //        frq, rate, cycles, note)
  //writef("notecofn10 %n: about to call cowait(%n) for the first time*n",
  //        note, amp)
  //abort(1004)

  { // The main loop
    LET p = cowait(amp) // Subscript of buf
                        // Upb of buf is bufupb

    // Amplitudes and phases for sample points in
    // the first half cycle
    LET a0, ph0 = 0, 0
    LET a1, ph1 = 0, 0
    LET a2, ph2 = 0, 0
    LET a3, ph3 = 0, 0
    LET a4, ph4 = 0, 0

    // Sample vectors for each of the 10 sample points
    LET sv0 = buf+p
    LET sv1 = sv0 +   c / 10000
    LET sv2 = sv0 + 2*c / 10000
    LET sv3 = sv0 + 3*c / 10000
    LET sv4 = sv0 + 4*c / 10000
    LET sv5 = sv0 + 5*c / 10000
    LET sv6 = sv0 + 6*c / 10000
    LET sv7 = sv0 + 7*c / 10000
    LET sv8 = sv0 + 8*c / 10000
    LET sv9 = sv0 + 9*c / 10000

    // Sums over the cycles at each sample point
    LET s0,s1,s2,s3,s4,s5,s6,s7,s8,s9 = 0,0,0,0,0,0,0,0,0,0
    LET q = 0 // Scaled with 3 digits after the decimal point
    
    IF p + c*cycles/1000 > bufupb LOOP
    
    FOR c = 1 TO cycles DO
    { LET t = q / 1000
      s0 := s0 + sv0!t
      s1 := s1 + sv1!t
      s2 := s2 + sv2!t
      s3 := s3 + sv3!t
      s4 := s4 + sv4!t
      s5 := s5 + sv5!t
      s6 := s6 + sv6!t
      s7 := s7 + sv7!t
      s8 := s8 + sv8!t
      s9 := s9 + sv9!t
      q := q + c
    }

    // Calculate the amplitude for each phase in the
    // first half cycle
    a0, ph0 := s1+s2+s3+s4 -s6-s7-s8-s9, 0
    a1, ph1 := s0+s1+s2+s3 -s5-s6-s7-s8, 1
    a2, ph2 := s9+s0+s1+s2 -s4-s5-s6-s7, 2
    a3, ph3 := s8+s9+s0+s1 -s3-s4-s5-s6, 3
    a4, ph4 := s7+s8+s9+s0 -s2-s3-s4-s5, 4

    // If the amplitude is negative the phase is
    // in the second half cycle
    IF a0<0 DO a0, ph0 := -a0, 5
    IF a1<0 DO a1, ph1 := -a1, 6
    IF a2<0 DO a2, ph2 := -a2, 7
    IF a3<0 DO a3, ph3 := -a3, 8
    IF a4<0 DO a4, ph4 := -a4, 9

    // Choose the phase that gives the greatest amplitude
    amp, phase := a0, ph0
    IF a1>amp DO amp, phase := a1, ph1
    IF a2>amp DO amp, phase := a2, ph2
    IF a3>amp DO amp, phase := a3, ph3
    IF a4>amp DO amp, phase := a4, ph4

    // Return the phase and amplitude by a call of cowait
    amp := amp/cycles/8   // 8 of the 10 sample points were used
    result2 := phase
  } REPEAT
}

AND notecofn8(args) = VALOF
{ // The note coroutines are created by calls such as
  // notecov!n := initco(notecofn8,500,freq,rate,cycles,note)
  // args points to freq.
  LET frq    = args!0 // eg 440_000
  LET rate   = args!1 // eg 44100
  LET cycles = args!2 // eg 24
  LET note   = args!3 // MIDI note number, eg 69 for 4A

  // This function uses 6 of the 8 equally spaced sample points per
  // cycle of the given frequency. It tests each of 8 phases each
  // based on the sample points. The phase that gives the greatest
  // amplitude is chosen.
  // 
  // It must first initialise the coroutine and then enter a
  // loop to receive a position in buf and return the
  // estimated amplitude of the given frequency at that position
  // with the phase returned in result2. The upb of buf is held in
  // bufupb.

  LET c = muldiv(rate, 1_000_000, frq)
  // c is a scaled number with 3 digits after the decimal point.
  LET amp, phase = 0, 0
  //writef("notecofn8: frg=%5.3d rate=%n cycles=%n note=%n*n",
  //        frq, rate, cycles, note)
  //writef("notecofn8 %n: about to call cowait(%n) for the first time*n",
  //        note, amp)
  //abort(10044)

  { // The main loop
    LET p = cowait(amp) // Subscript of buf
                        // Upb of buf is bufupb
			
    // Amplitudes and phases for sample points in
    // the first half cycle
    LET a0, ph0 = 0, 0
    LET a1, ph1 = 0, 0
    LET a2, ph2 = 0, 0
    LET a3, ph3 = 0, 0

    // Sample vectors for each of the 8 sample points
    LET sv0 = buf+p
    LET sv1 = sv0 +   c / 8000
    LET sv2 = sv0 + 2*c / 8000
    LET sv3 = sv0 + 3*c / 8000
    LET sv4 = sv0 + 4*c / 8000
    LET sv5 = sv0 + 5*c / 8000
    LET sv6 = sv0 + 6*c / 8000
    LET sv7 = sv0 + 7*c / 8000

    // Sums over the cycles at each sample point
    LET s0,s1,s2,s3,s4,s5,s6,s7 = 0,0,0,0,0,0,0,0
    LET q = 0 // Scaled with 3 digits after the decimal point
//writef("p=%n c=%n bufupb=%n*n", p, c, bufupb)
//abort(85432)
    IF p + c*cycles/1000 > bufupb LOOP
    
    FOR c = 1 TO cycles DO
    { LET t = q / 1000
      s0 := s0 + sv0!t
      s1 := s1 + sv1!t
      s2 := s2 + sv2!t
      s3 := s3 + sv3!t
      s4 := s4 + sv4!t
      s5 := s5 + sv5!t
      s6 := s6 + sv6!t
      s7 := s7 + sv7!t
      q := q + c
    }

//writef("s0=%n s1=%n s2=%n s3=%n s4=%n s5=%n s6=%n s7=%n*n",
//        s0,s1,s2,s3,s4,s5,s6,s7)
//abort(1998)	
    // Calculate the amplitude for each phase in the
    // first half cycle
    a0, ph0 := s1+s2+s3 -s5-s6-s7, 0
    a1, ph1 := s0+s1+s2 -s4-s5-s6, 1
    a2, ph2 := s7+s0+s1 -s3-s4-s5, 2
    a3, ph3 := s6+s7+s0 -s2-s3-s4, 3

    // If the amplitude is negative the phase is
    // in the second half cycle
    IF a0<0 DO a0, ph0 := -a0, 4
    IF a1<0 DO a1, ph1 := -a1, 5
    IF a2<0 DO a2, ph2 := -a2, 6
    IF a3<0 DO a3, ph3 := -a3, 7

    // Choose the phase that gives the greatest amplitude
    amp, phase := a0, ph0
    IF a1>amp DO amp, phase := a1, ph1
    IF a2>amp DO amp, phase := a2, ph2
    IF a3>amp DO amp, phase := a3, ph3

    // Return the phase and amplitude by a call of cowait
    amp := 2*amp/cycles/6   // 6 of the 8 sample points were used
    result2 := phase
  } REPEAT
}

AND notecofn6(args) = VALOF
{ // The note coroutines are created by calls such as
  // notecov!n := initco(notecofn6,500,freq,rate,cycles,note)
  // args points to freq.
  LET frq    = args!0 // eg 440_000
  LET rate   = args!1 // eg 44100
  LET cycles = args!2 // eg 24
  LET note   = args!3 // MIDI note number, eg 69 for 4A

  // This function uses 4 of the 6 equally spaced sample points per
  // cycle of the given frequency. It tests each of 6 phases each
  // based on the sample points. The phase that gives the greatest
  // amplitude is chosen.
  // 
  // It must first initialise the coroutine and then enter a
  // loop to receive a position in buf and return the
  // estimated amplitude of the given frequency at that position
  // with the phase returned in result2. The upb of buf is held in
  // bufupb.

  LET c = muldiv(rate, 1_000_000, frq)
  // c i a scaled number with 3 digits after the decimal point.
  LET amp, phase = 0, 0
  //writef("notecofn6: frg=%5.3d rate=%n cycles=%n note=%n*n",
  //        frq, rate, cycles, note)
  //writef("notecofn6 %n: about to call cowait(%n) for the first time*n",
  //        note, amp)
  //abort(1004)

  { // The main loop
    LET p = cowait(amp) // Subscript of buf
                        // Upb of buf is bufupb
			
    // Amplitudes and phases for sample points in
    // the first half cycle
    LET a0, ph0 = 0, 0
    LET a1, ph1 = 0, 0
    LET a2, ph2 = 0, 0

    // Sample vectors for each of the 6 sample points
    LET sv0 = buf+p
    LET sv1 = sv0 +   c / 6000
    LET sv2 = sv0 + 2*c / 6000
    LET sv3 = sv0 + 3*c / 6000
    LET sv4 = sv0 + 4*c / 6000
    LET sv5 = sv0 + 5*c / 6000

    // Sums over the cycles at each sample point
    LET s0,s1,s2,s3,s4,s5 = 0,0,0,0,0,0
    LET q = 0 // Scaled with 3 digits after the decimal point
    
    IF p + c*cycles/1000 > bufupb LOOP
    
    FOR c = 1 TO cycles DO
    { LET t = q / 1000
      s0 := s0 + sv0!t
      s1 := s1 + sv1!t
      s2 := s2 + sv2!t
      s3 := s3 + sv3!t
      s4 := s4 + sv4!t
      s5 := s5 + sv5!t
      q := q + c
    }

    // Calculate the ampliture for each phase in the
    // first half cycle
    a0, ph0 := s1+s2 -s4-s5, 0
    a1, ph1 := s0+s1 -s3-s4, 1
    a2, ph2 := s5+s0 -s2-s3, 2

    // If the amplitude is negative the phase is
    // in the second half cycle
    IF a0<0 DO a0, ph0 := -a0, 3
    IF a1<0 DO a1, ph1 := -a1, 4
    IF a2<0 DO a2, ph2 := -a2, 5

    // Choose the phase that gives the greatest amplitude
    amp, phase := a0, ph0
    IF a1>amp DO amp, phase := a1, ph1
    IF a2>amp DO amp, phase := a2, ph2

    // Return the phase and amplitude by a call of cowait
    amp := amp/cycles/4   // 4 of the 6 sample points were used
    result2 := phase
  } REPEAT
}

AND notecofn4(args) = VALOF
{ // The note coroutines are created by calls such as
  // notecov!n := initco(notecofn4,500,freq,rate,cycles,note)
  // args points to freq.
  LET frq    = args!0 // eg 440_000
  LET rate   = args!1 // eg 44100
  LET cycles = args!2 // eg 24
  LET note   = args!3 // MIDI note number, eg 69 for 4A

  // This function uses 2 of the 4 equally spaced sample points per
  // cycle of the given frequency. It tests each of 4 phases each
  // based on the sample points. The phase that gives the greatest
  // amplitude is chosen.
  // 
  // It must first initialise the coroutine and then enter a
  // loop to receive a position in buf and return the
  // estimated amplitude of the given frequency at that position
  // with the phase returned in result2. The upb of buf is held in
  // bufupb.

  LET c = muldiv(rate, 1_000_000, frq)
  // c is a scaled number with 3 digits after the decimal point.
  LET amp, phase = 0, 0
  //writef("notecofn4: frg=%5.3d rate=%n cycles=%n note=%n*n",
  //        frq, rate, cycles, note)
  //writef("notecofn4 %n: about to call cowait(%n) for the first time*n",
  //        note, amp)
  //abort(1004)

  { // The main loop
    LET p = cowait(amp) // Subscript of buf
                        // Upb of buf is bufupb
			
    // Amplitudes and phases for sample points in
    // the first half cycle
    LET a0, ph0 = 0, 0
    LET a1, ph1 = 0, 0

    // Sample vectors for each of the 8 sample points
    LET sv0 = buf+p
    LET sv1 = sv0 +   c / 4000
    LET sv2 = sv0 + 2*c / 4000
    LET sv3 = sv0 + 3*c / 4000

    // Sums over the cycles at each sample point
    LET s0,s1,s2,s3 = 0,0,0,0
    LET q = 0 // Scaled with 3 digits after the decimal point
    
    IF p + c*cycles/1000 > bufupb LOOP
    
    FOR c = 1 TO cycles DO
    { LET t = q / 1000
      s0 := s0 + sv0!t
      s1 := s1 + sv1!t
      s2 := s2 + sv2!t
      s3 := s3 + sv3!t
      q := q + c
    }

    // Calculate the ampliture for each phase in the
    // first half cycle
    a0, ph0 := s1 -s3, 0
    a1, ph1 := s0 -s2, 1

    // If the amplitude is negative the phase is
    // in the second half cycle
    IF a0<0 DO a0, ph0 := -a0, 2
    IF a1<0 DO a1, ph1 := -a1, 3

    // Choose the phase that gives the greatest amplitude
    amp, phase := a0, ph0
    IF a1>amp DO amp, phase := a1, ph1

    // Return the phase and amplitude by a call of cowait
    amp := amp/cycles/2   // 2 of the 4 sample points were used
    result2 := phase
  } REPEAT
}

AND getwav(name, sxv) BE
{ LET instream = 0

  instream := findinput(name)

  UNLESS instream DO
  { writef("Trouble with .wav file %s*n", name)
    RETURN
  }
  writef("Getting wav data from file %s*n", name)

  selectinput(instream)
  UNLESS wav2v() DO
    writef("The wav file was not read correctly*n")
  endstream(instream)
  buf := sxv!1
  bufupb := buf -> buf!0, 0
  writef("Number of samples = %n (%5.3d secs)*n",
          bufupb, muldiv(bufupb, 1000, 44100))
}

AND wav2v() = VALOF
{ // Read samples from the .wav file which is the currently
  // selected input and place them as 32-bit signed integers
  // in the self expanding vector sxv. If the .wav file is
  // in stereo the left and right samples are averaged.
  LET riff  = rd4()       //  0 RIFF
  LET size  = rd4()       //  4 filesize - 8
  LET wave  = rd4()       //  8 WAVE
  LET fmt   = rd4()       // 12 fmt
  LET chksz = rd4()       // 16 16
  LET quant = rd2()       // 20 1 = linear
  LET mode  = rd2()       // 22 1=mono 2=stereo
  LET rate  = rd4()       // 24 Typically 44100
  LET brate = rd4()       // 28 byte rate
  LET bytePerSample=rd2() // 32 1, 2 or 4  = bits/8 * mode
  LET bits  = rd2()       // 34 bits per sample = 8 or 16
  LET filler= chksz=18->rd2(), 0       // 36 filler

  LET data  = rd4()       // 36 data
  LET len   = rd4()       // 40 bytes of data or -1
  LET count = 0
  
  UNLESS riff=#x46464952 DO sawritef("Bad RIFF word %x8*n", riff)
  UNLESS wave=#x45564157 DO writef("Bad WAVE word %x8*n", wave)
  UNLESS fmt =#x20746D66 DO writef("Bad fmt  word %x8*n", fmt)
  //UNLESS chksz=16        DO writef("Bad subchunk size %n*n", chksz)
  UNLESS mode=1 | mode=2 DO writef("Bad mode %n*n", mode)
  UNLESS rate=44100      DO writef("Bad rate %n*n", rate)
  UNLESS bits=16         DO writef("Bad bits per sample %n*n", bits)
  UNLESS data=#x61746164 DO writef("Bad data word %x8*n", data)

writef("rate=%n %s*n", rate, mode=2->"stereo", "mono")

  { LET w = rd2()
    IF w<0 BREAK
    IF mode=2 DO w := (w+rd2())/2 // Average of both channels
    IF (w & #x8000)~=0 DO w := w | #xFFFF0000 // Sign extend
    //IF count MOD 10 = 0 DO newline()
    //writef(" %i6", w)
    count := count+1
    IF count MOD (10*44100) = 0 DO
      writef("alsatst: count=%i8 %5.1d secs*n", count, count/4410)
    //abort(1001)
//    IF count > 85 * 44100 BREAK
    sxpushval(sxv, w)
  } REPEAT

  buf := sxvv
  bufupb := buf -> buf!0, 0
//writef("alsatst: count=%n %5.1d secs*n", count, count/4410)
  RESULTIS TRUE
}

AND wrwavfile(towavname) = VALOF
{ LET oldout = output()
  LET wavout = findoutput(towavname)
  UNLESS wavout DO
  { writef("Trouble with .wav output file %s*n", towavname)
    RESULTIS FALSE
  }
  selectoutput(wavout)
  wrriffhdr(1, rate, 16, 2*bufupb)
  FOR i = 1 TO bufupb DO wr2(buf!i)
  endwrite()
  selectoutput(oldout)
  RESULTIS TRUE
}

AND wrriffhdr(mode, rate, bits, databytes) BE
{ LET bytes_per_sample = bits/8 * mode
  LET byte_rate = bytes_per_sample * rate
  writes("RIFF")        //  0: R I F F
  wr4(36+0)             //  4: size of this file - 8
  writes("WAVE")        //  8: W A V E
  writes("fmt ")        // 12: f m t
  wr4(16)               // 16: fmt subchunk size is 16
  wr2(1)                // 20: 1 = linear quantisation
  wr2(mode)             // 22: 1 = mono, 2=stereo
  wr4(rate)             // 24: samples per second
  wr4(byte_rate)        // 28: bytes per second
  wr2(bytes_per_sample) // 32: block align -- bits/8 * mode  = 1, 2 or 4
  wr2(bits)             // 34: bits per sample  = 8 or 16
  writes("data")        // 36: d a t a
  //wr4(byte_rate * 1)    // 40: number of bytes of data or zero
  wr4(databytes)        // 40: number of bytes of data or -1
}

AND wr1(b) BE
{ binwrch(b)
}

AND wr2(w) BE
{ LET s = @w
  binwrch(s%0)
  binwrch(s%1)
}

AND rd2() = VALOF
{ LET w = 0
  LET s = @w
  LET ch1 = binrdch()
  LET ch2 = binrdch()
  IF ch1<0 RESULTIS -1
  s%0 := ch1
  s%1 := ch2
  RESULTIS w
}

AND wr4(w) BE
{ LET s = @w
  binwrch(s%0)
  binwrch(s%1)
  binwrch(s%2)
  binwrch(s%3)
}

AND rd4() = VALOF
{ LET w = 0
  LET s = @w
  s%0 := binrdch()
  s%1 := binrdch()
  s%2 := binrdch()
  s%3 := binrdch()
  RESULTIS w
}
