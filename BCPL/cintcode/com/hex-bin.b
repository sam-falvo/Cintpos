// This is a program convert a hex file to back to a binary file.
// It is the inverse of bin2hex.b

// Implemented by Martin Richards (c) July 2002

// Usage:

// hex2bin filename [[TO] tofile]

/*
It will convert the file:

41 42 43 44 45 46 47 48 49 4A 4B 4C 4D 4E 4F 50
51 52 53 54 55 56 57 58 59 5A 0A 31 32 33 34 35
36 37 38 39 30 0A

to:

ABCDEFGHIJKLMNOPQRSTUVWXYZ
1234567890

History

08/04/2021
Modified this program to allow comments consisting of a hash ('\#')
followed by text to the end of the line. It also has better treatment
of syntax errors in the hex file,
*/

GET "libhdr"

GLOBAL {
  eof: ug
  sysin
  sysout
  fromname
  toname
  fromstream
  tostream
  count
  debug

  ch
  lineno
  chcount
  charv
  rch
  skipwhitespace
  rdhex
  synerror
 
}

LET start() = VALOF
{ LET argv    = VEC 50
  AND v       = VEC 63   // Room forthe latest 64 characters

  charv := v
  FOR i = 0 TO 63 DO charv!i := 0
  ch := 0
  lineno := 1
  chcount := 0
  
  sysin      := input()
  sysout     := output()
  fromname   := 0
  toname     := 0
  fromstream := 0
  tostream   := 0
  count      := 0

  UNLESS rdargs("FROM/A,TO/K,-d/S", argv, 50) DO
  { writes("bad arguments for hex-bin*n")
    stop(20)
  }

  fromname := argv!0                      // FROM/A
  IF argv!1 DO toname   := argv!1         // TO
  debug := argv!2                         // -d/S
  
  fromstream := findinput(fromname)
  UNLESS fromstream DO
  { writef("can't open %s*n", fromname)
    stop(20)
  }

  IF toname DO
  { tostream := findoutput(toname)
    UNLESS tostream DO
    { writef("can't open %s*n", toname)
      endread()
      stop(20)
    }
    selectoutput(tostream)
  }

  selectinput(fromstream)
  rch()
  
  { LET byte = rdhexbyte()
  //writef("byte=%x3*n", byte)
  //IF byte=-1 DO abort(1001)
    IF byte<0 BREAK
    TEST debug
    THEN writef("%i4: %x2*n", count, byte)
    ELSE wrch(byte)
    count := count+1
  } REPEAT

  endread()

  UNLESS sysout=tostream DO endwrite()

  selectoutput(sysout)
  IF toname DO writef("%n bytes written to file *"%s*"*n", count, toname)
  RESULTIS 0
}

AND rdhexbyte() = VALOF
{ LET a, b = 0, 0
  skipwhitespace()
  // ch is a hex digit or endstreamch
  IF ch=endstreamch RESULTIS -1
  a := value(ch)
  //writef("ch=%c => a=%n*n", ch, a)
  rch()
  IF ch=endstreamch RESULTIS -1
  b := value(ch)
  //writef("ch=%c => b=%n*n", ch, b)
  rch()
  RESULTIS a<<4 | b
}

AND value(ch) = VALOF SWITCHON ch INTO
{ DEFAULT:  synerror("Hex digit expected, ch=%n", ch)
            RESULTIS 0

  CASE '0': CASE '1': CASE '2': CASE '3': CASE '4': 
  CASE '5': CASE '6': CASE '7': CASE '8': CASE '9': 
            RESULTIS ch - '0'

  CASE 'A': CASE 'B': CASE 'C': CASE 'D': CASE 'E': CASE 'F':
            RESULTIS ch - 'A' + 10

  CASE 'a': CASE 'b': CASE 'c': CASE 'd': CASE 'e': CASE 'f':
            RESULTIS ch - 'a' + 10
}

AND skipwhitespace() BE
  WHILE ch='*s' | 0<=ch<32 DO rch()

AND rch() BE
{ LET skipping = FALSE

  { IF ch='*n' DO lineno, skipping := lineno+1, FALSE
    ch := rdch()
    //writef("chcount=%i5  ch=%i3 skipping=%n", chcount, ch, skipping)
    //IF ch>=32 DO writef(" '%c'", ch)
    //newline()
    //IF ch<0 DO abort(1002)
    IF ch=endstreamch RETURN
    chcount := chcount+1
    charv!(chcount & 63) := ch
    IF ch='#' DO skipping := TRUE
  } REPEATWHILE skipping 
}

AND synerror(mess, c) BE
{ writef("*nERROR: Near line %n ", lineno)
  writef(mess, c)
  newline()
  FOR i = chcount+1 TO chcount+64 DO
  { LET k = charv!(i&63)
    IF k DO wrch(k)
  }
  newline()
  //abort(1000)
}

