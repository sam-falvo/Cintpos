GET "libhdr.h"

GLOBAL {
  stdin:ug
  stdout
  fromname
  instream
  toname
  tostream
  ramstream
  keystr
  keyval
  debug
  rno    // Random number generator
  seed   // Random number seed
}

LET rno(upb) = VALOF  // Return a random number in the range 0 to 127
{ // This is based on randno in blib
  seed := seed*2147001325 + 715136305
  // randseed cycles through all 2^32 possible values.
  RESULTIS (ABS(seed/3)) & #x7F
}

LET start() = VALOF
{ LET res = 0
  LET argform = "from/A,key,to/K,-d/N"
  LET argv = VEC 50
  
  stdin     := input()
  stdout    := output()
  fromname  := "xxx"
  toname    := 0
  ramstream := 0
  keystr    := "1234"
  keyval    := 0
  debug     := 0
  seed      := 12345

  IF rdargs(argform, argv, 50)=0 DO
  { writef("Bad arguments for encrypt*n")
    result2 := 0
    RESULTIS 20
  }

  fromname := argv!0
  keystr   := argv!1
  IF argv!2 DO toname := argv!2
  IF argv!3 DO debug := !argv!3

  //writef("fromname=%s key=%s", fromname, keystr)
  //IF toname DO writef(" toname=%s debug=%n", toname, debug)
  //newline()

  // Choose an initialy seed for the random number generator
  seed := 12345
  IF keystr DO
  { FOR i = 1 TO keystr%0 DO
    { LET ch = keystr%i
      FOR j = 1 TO 3 DO
      { LET rnobyte = rno()
        //writef("seed=%8x ch=%2x*n", seed, ch)
        seed := seed XOR ch
      }
    }
  }
  //writef("seed=%8x*n", seed)

  ramstream := findinoutput("RAM:dummy")
  UNLESS ramstream DO
  { writef("Unable to create the RAM stream*n")
    res := 20
    GOTO fin
  }

  instream := findinput(fromname)
  UNLESS instream DO
  { writef("Unable to the from stream from file %s*n", fromname)
    res := 20
    GOTO fin
  }
//abort(1000)
  // Encrypt the from file to the RAM stream
  selectinput(instream)
  selectoutput(ramstream)
  
  { LET byte = binrdch()
    LET rnobyte = rno()
    IF byte=endstreamch BREAK
    //selectoutput(stdout)
    //writef("byte=%x2 rnobyte=%x2 => %2x*n",
    //        byte,    rnobyte,       byte XOR rnobyte)
    //selectoutput(ramstream)
    binwrch(byte XOR rnobyte)
  } REPEAT

  endstream(instream)
  instream := 0
  rewindstream(ramstream)
  selectinput(ramstream)

  // Now copy the RAM stream to the to stream or the from filename
  // if the to filename had not been specified.

  UNLESS toname DO toname := fromname

  tostream := findoutput(toname)
  UNLESS tostream DO
  { writef("Unable to open the stream to file: %s*n", toname)
    res := 20
    GOTO fin
  }

  selectinput(ramstream)
  selectoutput(tostream)
  
  { LET byte = binrdch()
    IF byte=endstreamch BREAK
    binwrch(byte)
  } REPEAT



fin:
//  abort(1001)
  IF tostream  DO endstream(tostream)
  IF instream  DO endstream(instream)
  IF ramstream DO endstream(ramstream)

  selectinput(stdin)
  selectoutput(stdout)
  newline()
  result2 := 0
  RESULTIS res
}
