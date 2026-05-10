/*

This program can be used to convert a bit stream file created by
rastsys using the raster command to a .wav sound file.

Implemented by Martin Richards (c) Sept 2016

Typical usage is as follows.

raster bits               Cause the next CLI command to generate the bit stream
                          file RASTER,bits
bcpl com/echo.b to junk   Perform a typical computation
bits2wav                  Convert RASTER.bits to RASTER.wav

Usage: from,to/k,b/n,s/n,a/n,d/n

from    is the bit stream file, default RASTER
to      is the .wav file, default RASTER.wav
b       is the bit rate, default 6000
s       is the .wav sample rate, default 11025
a       is the raw amplitude change rate
d       is the damping rate

History

31/01/2020
Renamed this file to bits2wav.b. rast2wav.b is now a program to
convert standard raster data to a sound file in .wav format.


The conversion from bits to .wav samples is as follows.

A one bit represents an input signal of +20000 which passes through
resistor R1 to charge capacitor C1 that hold the smoothed raw input
signal. A zero bit represents an input signal of -20000 driving the
smoothed signal down. The smoothed signal is passed via capacitor C2
and resistor R2 to capacitor C3 that holds the .wav sample value. This
final capacitor has resistor R3 in parallel that slowly discharges it
towards zero. The circuit is as follows.

                          smoothed input
                                |
                      ------    v   C2    ------
   input +/-20000 ---|  R1  |---*---||---|  R2  |--*-----*--> sample value
                      ------    |         ------   |     |
                                |                  |    ---
                               ---                ---  |   |
                            C1 ---             C3 ---  | R3|
                                |                  |   |___|
                                |                  |     |
   ground ----------------------*------------------*-----*

The a and d arguments combined with the bit rate effectively specify
the values of R1, R2 and R3. R1 affects the amount of smoothing and R2
and R3 typically cause the average sample value to be zero.

The bit rate specified by the b argument essentially specified the
speed of the computation in terms of memory references per second. Its
value is chosen to cause the generated sound to be in a suitable
frequency range.

*/

SECTION "bits2wav"

GET "libhdr"

MANIFEST { 
mono=1; stereo=2
mode = mono         // or stereo
bits_per_sample=16  // or 8
}

GLOBAL
{ sysout:ug
  sysin
  wavout
  fromfilename
  fromfile
  tofilename
  tofile
  bit_rate    // Typically 6000
  sample_rate // Argument value, typically <= 11024 or 22050
  samples_per_second // =44100, 22050 or 11025
  amp_gradiant
  damp_gradiant
  tracing

  samples
  bytes_per_sample
  bytes_per_second
  secs
  data_bytes

  rdbit      // Read the next bit from the bit stream
  bitpos     // Used by rdbit
  bits       // Used by rdbit
  scount
}

LET start() = VALOF
{ LET argv = VEC 50
  LET riffhdr = VEC 10
  LET filesize = 0

  sysout := output()
  sysin := input()

  IF rdargs("from,to/K,b/N,s/N,a/N,d/N,t/S", argv, 50)=0 DO
  { writes("Bad arguments for BITS2WAV*n")
    RESULTIS 20
  }

  // Set the default values
  fromfilename := "RASTER.bits"
  fromfile := 0
  tofilename := "RASTER.wav"
  tofile := 0
  bit_rate := 6000
  amp_gradiant := 1_000_000
  damp_gradiant :=   10_000
  sample_rate := 11025

  IF argv!0 DO fromfilename  := argv!0     // from
  IF argv!1 DO tofilename    := argv!1     // to/K
  IF argv!2 DO bit_rate      := !(argv!2)  // b/N
  IF argv!3 DO sample_rate   := !(argv!3)  // s/N
  IF argv!4 DO amp_gradiant  := !(argv!4)  // a/N
  IF argv!5 DO damp_gradiant := !(argv!5)  // d/N
  tracing := argv!6                        // t/S

  fromfile := findinput(fromfilename)
  UNLESS fromfile DO
  { writef("Unable to open bit stream file %s*n", fromfilename)
    GOTO fin
  }

  tofile := findoutput(tofilename)
  UNLESS tofile DO
  { writef("Unable to open output file %s*n", tofilename)
    GOTO fin
  }

  selectinput(fromfile)
  filesize := sys(Sys_filesize, @fromfile!scb_fd)

  IF tracing DO writef("file size = %n bytes*n", filesize)
//abort(1111)

  IF FALSE DO
  { // test rdbit
    FOR i = 1 TO filesize DO
    { LET bit = rdbit()
      writen(bit)
      IF i MOD 80 = 0 DO newline()
    }
    newline()
    abort(1112)
  }

  samples_per_second := 44_100
  IF sample_rate<=22_050 DO samples_per_second := 22_050
  IF sample_rate<=11_025 DO samples_per_second := 11_025

  secs := filesize * 8 / bit_rate + 2 // 8 bits per byte
  samples := samples_per_second * secs & -16
  bytes_per_sample := bits_per_sample/8 * mode // Typically 2
  bytes_per_second := samples_per_second * bytes_per_sample
  data_bytes :=  samples * bytes_per_sample

  writef("Converting bit stream file %s to %s*n*n",
         fromfilename, tofilename)
  writef("Bits per second =    %n*n", bit_rate)
  writef("Samples per second = %n*n", samples_per_second)
  TEST mode=1 THEN writef("mono %n-bit samples*n", bits_per_sample)
              ELSE writef("stereo %n-bit sample*n", bits_per_sample)
  writef("Data bytes = %n*n", data_bytes)
  writef("Total time: %n seconds*n*n", secs)
  //abort(1000)
  riffhdr!0  := #x46464952         // R I F F
  riffhdr!1  := data_bytes+36      // size
  riffhdr!2  := #x45564157         // W A V E
  riffhdr!3  := #x20746D66         // f m t
  riffhdr!4  := 16                 //
  riffhdr!5  := mode<<16 | 1       // 
  riffhdr!6  := samples_per_second // samples per second
  riffhdr!7  := bytes_per_second
  riffhdr!8  := bits_per_sample<<16 | bytes_per_sample
  riffhdr!9  := #x61746164         // d a t a
  riffhdr!10 := data_bytes
   
  selectoutput(tofile)
  FOR i = 0 TO 43 DO wrch(riffhdr%i)

  bitpos := 0
  bits := 0
  scount := 0

  gensamples(samples, bit_rate, samples_per_second)

  endstream(fromfile); fromfile := 0
  selectinput(sysin)

  endstream(tofile); tofile := 0
  selectoutput(sysout)

  writef("File %s written*n", tofilename)

fin:
  IF fromfile DO { endstream(fromfile); fromfile := 0 }
  IF tofile   DO { endstream(tofile);   tofile   := 0 }
  selectinput(sysin)
  selectoutput(sysout)

  RESULTIS 0   
}

AND rdbit() = VALOF
{ LET bit = 0
  IF bitpos=0 DO
  { bitpos := 1
    bits := binrdch()
  }
  IF (bits&bitpos)>0 DO bit := 1
  //sawritef("bits=%8b bitpos = %8b bit=%n*n", bits, bitpos, bit)
//abort(1000)
  TEST bitpos=128 THEN bitpos := 0
                  ELSE bitpos := bitpos<<1
  RESULTIS bit
}

AND gensamples(samples, brate, srate) BE
{ LET count = 0
  LET bperiod = 1_000_000_000 / brate // In nano seconds
  LET speriod = 1_000_000_000 / srate // In nano seconds
  LET nextsampletime = bperiod
  LET smoothed_value = 0
  LET prevvalue = 0
  LET nextvalue = 0
  LET factor1 =  amp_gradiant * 1000 / bit_rate
  LET factor2 = damp_gradiant * 1000 / bit_rate

  // Initialize rdbit variables
  bitpos, bits := 0, 0


  { WHILE nextsampletime < bperiod DO
    { // Generate the next .wav sample by interpolation.
      LET sample = prevvalue +
                   muldiv(nextvalue-prevvalue, nextsampletime, bperiod)

      wr2(sample)     // Write a mono 16-bit sample

      IF tracing DO
      { sawritef("factor1=%i6 nextsampletime=%i9 bperiod=%i9*n",
                  factor1,    nextsampletime,    bperiod)
        sawritef("prevvalue=%i6, nextvalue=%i6, nextsampletime=%i9 bperiod=%i9 sample=%i6*n",
                  prevvalue,     nextvalue,     nextsampletime,    bperiod,    sample)
        abort(1001) 
      }
      count := count+1
      IF count >= samples RETURN
      nextsampletime := nextsampletime + speriod
    }

    { // Read next bit value
      LET value = rdbit()=0 -> -20_000, +20_000
      LET diff = value - smoothed_value
      prevvalue := nextvalue
      nextsampletime := nextsampletime - bperiod

      // Calculate nextvalue

      smoothed_value := smoothed_value +
                        muldiv(diff, factor1, 1_000_000)

      nextvalue := nextvalue + 
                   muldiv(diff, factor1, 1_000_000)

      IF tracing DO
      { sawritef("value=%i6 diff=%i6 smoothed_value=%i6 factor1=%i6 nextvalue=%i6*n",
                  value,    diff,    smoothed_value,    factor1,    nextvalue)
      }

      nextvalue := nextvalue - muldiv(nextvalue, factor2, 1_000_000)
      IF tracing DO
      { sawritef("factor2=%i6 damped=%i6*n", factor2, nextvalue)
//abort(1000)
      }

    }
  } REPEAT
}

AND wr2(val) BE
{ IF scount MOD 1000000 = 0 DO sawritef("sample count = %i8*n", scount)
  scount := scount+2
  wrch(val); wrch(val>>8)
  IF tracing DO
  { sawritef("written sample: %i6*n", val)
    abort(1001)
  }
}





