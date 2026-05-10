/*
This progam is designed to test various note recognition
coroutines.
*/

GET "libhdr"

GLOBAL {
  notecov:ug // Vector of note coroutines, one for each
             // selected MIDI note
  notecofn10 // The note coroutine main function
  notecofn8
  notecofn6
  notecofn4
  notecofn2
  cofn       // =2, 4, 6, 8 or 10
  
  samplesvupb  
  samplesv   // To hold BCPLWORD sized samples
  freqtab    // The elements are scaled integers with 3 digits
             // after the decimalpoint
}

LET start() = VALOF
{ LET argv = VEC 50
  // Choose the default values
  LET freq, rate, cycles, a, b, randamp = 440_000, 44100, 20, 60, 72, 0
  LET cofn = 6
  LET argform = "freq=-f/N,rate=andamp-r/K/N,cycles=-c/K/N,*
                *-a/K/N,-b/K/N,randamp=-ra/K/N,cofn=-fn/K/N"
  UNLESS rdargs(argform, argv, 50) DO
  { writef("Bad args for recog*n")
    RESULTIS 0
  }
  
  IF argv!0 DO freq    := !argv!0
  IF argv!1 DO rate    := !argv!1
  IF argv!2 DO cycles  := !argv!2
  IF argv!3 DO a       := !argv!3
  IF argv!4 DO b       := !argv!4
  IF argv!5 DO randamp := !argv!5
  IF argv!6 DO cofn    := !argv!6
  
  IF freq<20_000    DO freq   := 20_000
  IF freq>4000_000  DO freq   := 4000_000
  IF rate<8000      DO rate   := 8000
  IF rate>96000     DO rate   := 96000
  IF cycles<1       DO cycles := 1
  IF cycles>100     DO cycles := 100
  IF a<21           DO a := 21     // Piano lowest note
  IF a>21+87        DO a := 21+87  // Piano highest note
  IF b<21           DO b := 21
  IF b>21+87        DO b := 21+87

  UNLESS cofn=2 | cofn=4 | cofn=6 | cofn=8 | cofn=10 DO
  { writef("cofn should be 2, 4, 6, 8 or 10*n")
    cofn := 6
  }
  
  writef("*nrecog: freq=%5.3d rate=%n cycles=%n *
         *a=%n b=%n randamp=%n cofn=%n*n",
          freq,rate,cycles,a,b,randamp,cofn)

  samplesvupb := 2 * rate // Room for 2 seconds worth of samples
  samplesv := getvec(samplesvupb)

  UNLESS samplesv DO
  { writef("Unable to allocate samplesv&n")
    RESULTIS 0
  }
  FOR i = 0 TO samplesvupb DO samplesv!i := 0

IF randamp DO
  FOR i = 0 TO samplesvupb DO
  { // Add white noise with given amplitude
    LET x = randno(2*randamp)-randamp
    samplesv!i := samplesv!i + x
  }
  // Make sine wave samples of apmplitude 10000 for given frequency
//IF FALSE DO
  { LET FLT r = 0.0
    LET FLT pi2 = FLOAT(2 * 3_14159) / FLOAT 100000
    LET FLT samplespercyclef = (FLOAT muldiv(rate, 1_000_000, freq))/1_000.0
    LET FLT factor = pi2 / samplespercyclef
    writef("pi2=%5.3f samplespercyclef=%5.3f*n",pi2, samplespercyclef)
    writef("samplesv=%n samplesvupb=%n*n", samplesv, samplesvupb)
    //abort(1001)
    FOR i = 0 TO samplesvupb DO
    //FOR i = 0 TO 100 DO//samplesvupb DO
    { LET samplespercycle = muldiv(44100, 1_000_000, freq)
      WHILE r >= samplespercyclef DO r := r - samplespercyclef
      samplesv!i := samplesv!i +
                    FIX(sys(Sys_flt, fl_sin, r * factor) * 10_000.0)
      //writef("%i5: %i6*n", i, samplesv!i)
      //abort(1002)
      r := r + 1.0
    }
    
  }
//abort(1003)

  //FOR i = 0 TO samplesvupb DO
  FOR i = 0 TO 101 DO
  { UNLESS i MOD 10 DO writef("*n%i3: ", i)
    writef("%i6", samplesv!i)
  }
  newline()

  setfreqtab()
  
  notecov := getvec(127)
  UNLESS notecov DO
  { writef("Unable to allocate notecov*n")
    GOTO fin
  }
  FOR note = 0 TO 127 DO notecov!note := 0
  
  FOR n = a TO b DO
  { notecov!n := initco((cofn=2  -> notecofn2,
                         cofn=4  -> notecofn4,
                         cofn=6  -> notecofn6,
                         cofn=8  -> notecofn8,
                         cofn=10 -> notecofn10,
			 notecofn6),
                        500,
                        freqtab!n, // The frequency eg 440_000
			rate,
		        cycles,    // The cycles
		        n)         // The midi note number

    //writef("noteco %n: Created, result2=%n*n", n, result2)
  }
  writef("All note coroutines have been created*n")
//  abort(1005)

  FOR p = 0 TO 101 DO
  { //writef("Testing aplitudes at p=%n*n", p)
    writef("p=%i3: ", p)
    FOR n = 0 TO 127 IF notecov!n DO
    { LET co = notecov!n
      LET amp = callco(co, p)
      LET phase = result2
      writef(" %i5/%i2", amp, phase)
      LOOP
      amp := amp/1000
      IF amp>9 DO amp := 9
      TEST amp
      THEN writef("%n", amp)
      ELSE SWITCHON n MOD 12 INTO
	     { DEFAULT: writef("-"); ENDCASE
	       CASE  1:
	       CASE  3:
	       CASE  6:
	       CASE  8:
	       CASE 10: writef("**"); ENDCASE
	     }
    }
    newline()
    //abort(1006)
  }

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
//    abort(1008)
  }
  
//  abort(1007)
  
fin:
  FOR n = 0 TO 127 IF notecov!n DO deleteco(notecov!n)
  
  RESULTIS 0
}

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

AND notecofn10(args) = VALOF
{ // The note coroutines are created by calls such as
  // notecov!n := initco(notecofn10,500,freq,rate,cycles,note)
  // args points to freq.
  LET frq    = args!0 // eg 440_000
  LET rate   = args!1 // eg 44100
  LET cycles = args!2 // eg 24
  LET note   = args!3 // MIDI note number, eg 69 for 4A

  // This function uses 12 sample points per cycle of the given
  // frequency. It tests 12 phases each based on 10 of the 12 sample
  // points. The phase that gives the greatest amplitude is the
  // one that is chosen.
  // 
  // It must first initialise the coroutine and the enter a
  // loop to receive a position in samplesv and return the
  // estimated amplitude of the given frequency at that position
  // with the phase returned in result2. Receiving a negative
  // position is an indication that the coroutine is no longer
  // needed and must commit suicide.
  LET samples_per_cycle = muldiv(rate, 1_000_000, frq)
  // samples_per_cycle i a scaled number with 3 digits after the
  // decimal point.
  LET amp, phase = 123, 0
  //writef("notecofn10: frg=%5.3d rate=%n cycles=%n note=%n*n",
  //        frq, rate, cycles, note)
  //writef("notecofn10 %n: about to call cowait(%n) for the first time*n",
  //        note, amp)
  //abort(1004)

  // The main loop
  { LET p = cowait(amp)
    // Amplitudes and phases for sample points in
    // the first half cycle
    LET a0, ph0 = 0, 0
    LET a1, ph1 = 0, 0
    LET a2, ph2 = 0, 0
    LET a3, ph3 = 0, 0
    LET a4, ph4 = 0, 0
    LET a5, ph5 = 0, 0

    // Sample vectors for each of the 10 sample points
    LET sv0 = samplesv+p
    LET sv1  = sv0 +    samples_per_cycle / 12000
    LET sv2  = sv0 +  2*samples_per_cycle / 12000
    LET sv3  = sv0 +  3*samples_per_cycle / 12000
    LET sv4  = sv0 +  4*samples_per_cycle / 12000
    LET sv5  = sv0 +  5*samples_per_cycle / 12000
    LET sv6  = sv0 +  6*samples_per_cycle / 12000
    LET sv7  = sv0 +  7*samples_per_cycle / 12000
    LET sv8  = sv0 +  8*samples_per_cycle / 12000
    LET sv9  = sv0 +  9*samples_per_cycle / 12000
    LET sv10 = sv0 + 10*samples_per_cycle / 12000
    LET sv11 = sv0 + 11*samples_per_cycle / 12000

    // Sums over the cycles at each sample point
    LET s0,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11 = 0,0,0,0,0,0,0,0,0,0,0,0
    LET q = 0 // Scaled with 3 digits after the decimal point
    
    IF p<0 BREAK
    
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
      q := q + samples_per_cycle
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
    amp := amp/cycles/10
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

  // This function uses 10 sample points per cycle of the given
  // frequency. It tests 10 phases each based on 8 of the 10 sample
  // points. The phase that gives the greatest amplitude is the
  // one that is chosen.
  // 
  // It must first initialise the coroutine and the enter a
  // loop to receive a position in samplesv and return the
  // estimated amplitude of the given frequency at that position
  // with the phase returned in result2. Receiving a negative
  // position is an indication that the coroutine is no longer
  // needed and must commit suicide.
  LET samples_per_cycle = muldiv(rate, 1_000_000, frq)
  // samples_per_cycle i a scaled number with 3 digits after the
  // decimal point.
  LET amp, phase = 123, 0
  //writef("notecofn8: frg=%5.3d rate=%n cycles=%n note=%n*n",
  //        frq, rate, cycles, note)
  //writef("notecofn8 %n: about to call cowait(%n) for the first time*n",
  //        note, amp)
  //abort(1004)

  // The main loop
  { LET p = cowait(amp)
    // Amplitudes and phases for sample points in
    // the first half cycle
    LET a0, ph0 = 0, 0
    LET a1, ph1 = 0, 0
    LET a2, ph2 = 0, 0
    LET a3, ph3 = 0, 0
    LET a4, ph4 = 0, 0

    // Sample vectors for each of the 10 sample points
    LET sv0 = samplesv+p
    LET sv1 = sv0 +   samples_per_cycle / 10000
    LET sv2 = sv0 + 2*samples_per_cycle / 10000
    LET sv3 = sv0 + 3*samples_per_cycle / 10000
    LET sv4 = sv0 + 4*samples_per_cycle / 10000
    LET sv5 = sv0 + 5*samples_per_cycle / 10000
    LET sv6 = sv0 + 6*samples_per_cycle / 10000
    LET sv7 = sv0 + 7*samples_per_cycle / 10000
    LET sv8 = sv0 + 8*samples_per_cycle / 10000
    LET sv9 = sv0 + 9*samples_per_cycle / 10000

    // Sums over the cycles at each sample point
    LET s0,s1,s2,s3,s4,s5,s6,s7,s8,s9 = 0,0,0,0,0,0,0,0,0,0
    LET q = 0 // Scaled with 3 digits after the decimal point
    
    IF p<0 BREAK
    
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
      q := q + samples_per_cycle
    }

    // Calculate the ampliture for each phase in the
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
    amp := amp/cycles/8
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

  // This function uses 8 sample points per cycle of the given
  // frequency. It tests 8 phases each based on 6 of the 8 sample
  // points. The phase that gives the greatest amplitude is the
  // one that is chosen.
  // 
  // It must first initialise the coroutine and the enter a
  // loop to receive a position in samplesv and return the
  // estimated amplitude of the given frequency at that position
  // with the phase returned in result2. Receiving a negative
  // position is an indication that the coroutine is no longer
  // needed and must commit suicide.
  LET samples_per_cycle = muldiv(rate, 1_000_000, frq)
  // samples_per_cycle i a scaled number with 3 digits after the
  // decimal point.
  LET amp, phase = 123, 0
  //writef("notecofn6: frg=%5.3d rate=%n cycles=%n note=%n*n",
  //        frq, rate, cycles, note)
  //writef("notecofn6 %n: about to call cowait(%n) for the first time*n",
  //        note, amp)
  //abort(1004)

  // The main loop
  { LET p = cowait(amp)
    // Amplitudes and phases for sample points in
    // the first half cycle
    LET a0, ph0 = 0, 0
    LET a1, ph1 = 0, 0
    LET a2, ph2 = 0, 0
    LET a3, ph3 = 0, 0

    // Sample vectors for each of the 8 sample points
    LET sv0 = samplesv+p
    LET sv1 = sv0 +   samples_per_cycle / 8000
    LET sv2 = sv0 + 2*samples_per_cycle / 8000
    LET sv3 = sv0 + 3*samples_per_cycle / 8000
    LET sv4 = sv0 + 4*samples_per_cycle / 8000
    LET sv5 = sv0 + 5*samples_per_cycle / 8000
    LET sv6 = sv0 + 6*samples_per_cycle / 8000
    LET sv7 = sv0 + 7*samples_per_cycle / 8000

    // Sums over the cycles at each sample point
    LET s0,s1,s2,s3,s4,s5,s6,s7 = 0,0,0,0,0,0,0,0
    LET q = 0 // Scaled with 3 digits after the decimal point
    
    IF p<0 BREAK
    
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
      q := q + samples_per_cycle
    }

    // Calculate the ampliture for each phase in the
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
    amp := amp/cycles/6
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

  // This function uses 6 sample points per cycle of the given
  // frequency. It tests 6 phases each based on 4 of the 6 sample
  // points. The phase that gives the greatest amplitude is the
  // one that is chosen.
  // 
  // It must first initialise the coroutine and the enter a
  // loop to receive a position in samplesv and return the
  // estimated amplitude of the given frequency at that position
  // with the phase returned in result2. Receiving a negative
  // position is an indication that the coroutine is no longer
  // needed and must commit suicide.
  LET samples_per_cycle = muldiv(rate, 1_000_000, frq)
  // samples_per_cycle i a scaled number with 3 digits after the
  // decimal point.
  LET amp, phase = 123, 0
  //writef("notecofn4: frg=%5.3d rate=%n cycles=%n note=%n*n",
  //        frq, rate, cycles, note)
  //writef("notecofn4 %n: about to call cowait(%n) for the first time*n",
  //        note, amp)
  //abort(1004)

  // The main loop
  { LET p = cowait(amp)
    // Amplitudes and phases for sample points in
    // the first half cycle
    LET a0, ph0 = 0, 0
    LET a1, ph1 = 0, 0
    LET a2, ph2 = 0, 0

    // Sample vectors for each of the 8 sample points
    LET sv0 = samplesv+p
    LET sv1 = sv0 +   samples_per_cycle / 6000
    LET sv2 = sv0 + 2*samples_per_cycle / 6000
    LET sv3 = sv0 + 3*samples_per_cycle / 6000
    LET sv4 = sv0 + 4*samples_per_cycle / 6000
    LET sv5 = sv0 + 5*samples_per_cycle / 6000

    // Sums over the cycles at each sample point
    LET s0,s1,s2,s3,s4,s5 = 0,0,0,0,0,0
    LET q = 0 // Scaled with 3 digits after the decimal point
    
    IF p<0 BREAK
    
    FOR c = 1 TO cycles DO
    { LET t = q / 1000
      s0 := s0 + sv0!t
      s1 := s1 + sv1!t
      s2 := s2 + sv2!t
      s3 := s3 + sv3!t
      s4 := s4 + sv4!t
      s5 := s5 + sv5!t
      q := q + samples_per_cycle
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
    amp := amp/cycles/4
    result2 := phase
  } REPEAT

}

AND notecofn2(args) = VALOF
{ // The note coroutines are created by calls such as
  // notecov!n := initco(notecofn2,500,freq,rate,cycles,note)
  // args points to freq.
  LET frq    = args!0 // eg 440_000
  LET rate   = args!1 // eg 44100
  LET cycles = args!2 // eg 24
  LET note   = args!3 // MIDI note number, eg 69 for 4A

  // This function uses 4 sample points per cycle of the given
  // frequency. It tests 4 phases each based on 2 of the 4 sample
  // points. The phase that gives the greatest amplitude is the
  // one that is chosen.
  // 
  // It must first initialise the coroutine and the enter a
  // loop to receive a position in samplesv and return the
  // estimated amplitude of the given frequency at that position
  // with the phase returned in result2. Receiving a negative
  // position is an indication that the coroutine is no longer
  // needed and must commit suicide.
  LET samples_per_cycle = muldiv(rate, 1_000_000, frq)
  // samples_per_cycle i a scaled number with 3 digits after the
  // decimal point.
  LET amp, phase = 123, 0
  //writef("notecofn4: frg=%5.3d rate=%n cycles=%n note=%n*n",
  //        frq, rate, cycles, note)
  //writef("notecofn4 %n: about to call cowait(%n) for the first time*n",
  //        note, amp)
  //abort(1004)

  // The main loop
  { LET p = cowait(amp)
    // Amplitudes and phases for sample points in
    // the first half cycle
    LET a0, ph0 = 0, 0
    LET a1, ph1 = 0, 0

    // Sample vectors for each of the 8 sample points
    LET sv0 = samplesv+p
    LET sv1 = sv0 +   samples_per_cycle / 4000
    LET sv2 = sv0 + 2*samples_per_cycle / 4000
    LET sv3 = sv0 + 3*samples_per_cycle / 4000

    // Sums over the cycles at each sample point
    LET s0,s1,s2,s3 = 0,0,0,0
    LET q = 0 // Scaled with 3 digits after the decimal point
    
    IF p<0 BREAK
    
    FOR c = 1 TO cycles DO
    { LET t = q / 1000
      s0 := s0 + sv0!t
      s1 := s1 + sv1!t
      s2 := s2 + sv2!t
      s3 := s3 + sv3!t
      q := q + samples_per_cycle
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
    amp := amp/cycles/2
    result2 := phase
  } REPEAT

}

