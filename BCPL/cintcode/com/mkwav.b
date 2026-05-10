SECTION "mkwav"

GET "libhdr"

MANIFEST { 
mono=1; stereo=2
mode = stereo       // or stereo
bits_per_sample=16  // or 8
sample_rate = 44100 // or 22050  or 11025
msecs=5000

samples = sample_rate*msecs/1000 & -16
bytes_per_sample = bits_per_sample/8 * mode
bytes_per_second = sample_rate * bytes_per_sample
data_bytes =  samples * bytes_per_sample
}

GLOBAL
{ sintab:200
  sysout:201
  wavout:202
}

LET initsintab() BE
{ LET y, ydot = 0, 1_000_000_000

  FOR i = 0 TO 16383 DO
  { LET yval = (y+500)/1000
    sintab!i         :=  yval
    sintab!(32767-i) :=  yval 
    sintab!(32768+i) := -yval 
    sintab!(65535-i) := -yval 
    ydot := ydot - muldiv(y,    95874, 1_000_000_000)
    y    := y    + muldiv(ydot, 95874, 1_000_000_000)
  }
}

LET start() = VALOF
{ LET args = VEC 50
  LET riffhdr = VEC 10
  sysout := output()

  sintab := getvec(65535)

  UNLESS sintab DO 
  { writes("Not enough space*n")
    RESULTIS 20
  }

  IF rdargs("TO/K", args, 50)=0 DO
  { writes("Bad arguments for MKWAV*n")
    RESULTIS 20
  }

  UNLESS args!0 DO args!0 := "junk.wav"

  wavout := findoutput(args!0)

  UNLESS wavout DO
  { writef("Unable to open file %s*n", args!0)
    RESULTIS 20
  }

  initsintab()

  riffhdr!0  := #x46464952     // R I F F
  riffhdr!1  := data_bytes+36  // size
  riffhdr!2  := #x45564157     // W A V E
  riffhdr!3  := #x20746D66     // f m t
  riffhdr!4  := 16             //
  riffhdr!5  := mode<<16 | 1   // 
  riffhdr!6  := sample_rate    // samples per second
  riffhdr!7  := bytes_per_second
  riffhdr!8  := bits_per_sample<<16 | bytes_per_sample
  riffhdr!9  := #x61746164     // d a t a
  riffhdr!10 := data_bytes
   
  selectoutput(wavout)
  FOR i = 0 TO 43 DO wrch(riffhdr%i)

  FOR i = 1 TO samples DO wr2(f(i))

  endwrite()
  selectoutput(sysout)
  writef("File %s written*n", args!0)
  freevec(sintab)
  RESULTIS 0   
}

AND sin(x) = sintab!(x & #xFFFF)

AND f(t) = VALOF
{ LET s = muldiv(t, 65536, sample_rate)
  LET amp = mix(s, 3*440/5, TABLE      12,  // Number of harmonics
                                20000,  // Fundamental
                                50000,  // 2
                               300000,  // 3
                               200000,  // 4
                               100000,  // 5
                                50000,  // 6
                               240000,  // 7
                                20000,  // 8
                                    0,  // 9
                                    0,  // 10
                               300000,  // 11
                                    0   // 12
               )
  LET vol = envelope(s,     0,     0,
                        28000, 15000,
                        40000,  9000,
                        60000,  9000,
                       390000,     0)

// 15000          *
// 14000          | \
// 13000         |    \
// 12000         |      \
// 11000        |         \
// 10000        |           \
//  9000       |              *-------------------*
//  8000       |                                   \
//  7000      |                                     \
//  6000      |                                      \
//  5000     |                                        \
//  5000     |                                         \
//  4000    |                                           \
//  3000    |                                            \
//  2000   |                                              \
//  1000   |                                               \
//     0  *                                                 *
//                  11111111112222222222333333333344444444445
//        012345678901234567890123456789012345678901234567890
  RESULTIS muldiv(amp, vol, 1000000)
}

AND envelope(t, t0, v0, t1, v1, t2, v2, t3, v3, t4, v4) = VALOF
{ LET v, dt, dv = 0, 0, 0
  //RESULTIS 20000
  TEST t<t2
  THEN TEST t<t1
       THEN IF t>=t0 DO t, v, dt, dv := t-t0, v0, t1-t0, v1-v0
       ELSE             t, v, dt, dv := t-t1, v1, t2-t1, v2-v1
  ELSE TEST t<t3
       THEN             t, v, dt, dv := t-t2, v2, t3-t2, v3-v2
       ELSE IF t<t4  DO t, v, dt, dv := t-t3, v3, t4-t3, v4-v3
  IF dt=0 RESULTIS 0
  RESULTIS v + muldiv(t, dv, dt)
}

AND mix(s, f, v) = VALOF
{ LET amp = 0
  LET x = s*f & #xFFFF
  FOR i = 1 TO v!0 DO
    amp := amp + muldiv(sin(i*x), v!i, 1000000)
//   IF amp> 32767 DO amp :=  32767
//   IF amp<-32767 DO amp := -32767
  RESULTIS amp
}

AND wr2(val) BE { wrch(val); wrch(val>>8)  }





