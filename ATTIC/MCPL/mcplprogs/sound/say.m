GET "mcpl.h"

STATIC soundv, soundp, stdin, stdout, speakerdev

MANIFEST Soundv_upb=400000

FUN start : =>
  LET argv = VEC 50
  stdin  := input()
  stdout := output()

  UNLESS rdargs("FROM", argv, 50) DO
  { writes "Bad argument for SAY\n"
    RETURN 20
  }

  LET aufilename = "test.au"
  IF argv!0 DO aufilename := argv!0
  LET aufile = findinput aufilename
  UNLESS aufile DO
  { writef("Can't open %s\n", aufilename)
    RETURN 20
  }
  selectinput aufile

  soundv := getvec(Soundv_upb/Bpw)
  UNLESS soundv DO
  { writef("Can't allocate soundv of size %d\n", Soundv_upb)
    endread()
    RETURN 20
  }

 // FOR i = 0 TO Soundv_upb DO soundv%i := (i/20) MOD 100 + 100

  soundp := 0

  { LET byte = rdch()
    IF byte=Endstreamch BREAK
    soundv%soundp++ := byte
    IF soundp>Soundv_upb BREAK
  } REPEAT

  endread()
  selectinput stdin

  writef("soundp = %d\n", soundp)

  speakerdev := sys(15, "/dev/audio")
  UNLESS speakerdev DO
  { writes "Can't open /dev/audio device\n"
    RETURN 20
  }

  try()

  freevec soundv
  RETURN 0


FUN say : p, size => sys(13, speakerdev, soundv+p, size)

FUN try : =>
  LET pv = VEC 256
  LET lv = VEC 256
  FOR i = 0 TO 256 DO pv!i, lv!i := 0, 0

  LET ch, currch='0'

  { ch := rdch()
    MATCH capitalch ch
    : ' '  => LOOP
    : '\n' => say(pv!currch, lv!currch)
    : '.' => BREAK
    : '!' => { ch := rdch()
               IF ch='\n' OR ch=Endstreamch BREAK
               say(pv!ch, lv!ch)
             } REPEAT
    : '0'..'9' | 'A'..'Z' => currch := ch
    : ':' => pv!currch := readn()
    : '+' => pv!currch +:= 100
    : '-' => pv!currch -:= 100
    : ',' => lv!currch := readn()
    : '>' => lv!currch +:= 100
    : '<' => lv!currch -:= 100
    : '?' => writef("%c:%6d, %6d\n", currch, pv!currch, lv!currch)
    .
    
  } REPEAT




