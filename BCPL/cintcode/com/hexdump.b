/*
This is a program to dump files in hex and character form.
Implemented by Martin Richards (c) June 2002

Usage:
 "FROM/A,N/N,P/N,RL/K/N,RLB/K/N,TO/K,X1/S,X2/S,X4/S,LIT/S,BIG/S",

FROM/A    The name of the file to dump
N/N       The number of bytes or records to dump
P/N       The byte or record position of where to start the dump
RL/N      The record length in words
RLB/N     The record length in bytes
TO/K      The destination file name
X1/S      Output seperate bytes
X2/S      Output 16-bit words
X4/S      Output 32-bit words
LIT/S     Use little ender order for the words
BIG/S     Use big ender order for the words

The file is only treated as a file of records if RL or RLB is specified
If neither LIT nor BIG are specified the enderness is that of the machine
on which hexdump is running.

History

9 Jan 2017
Modified the rdargs decoding and other minor corrections.

*/

GET "libhdr"

GLOBAL {
 eof: ug
 stdin
 stdout
 fromname
 toname
 fromstream
 tostream
 pos
 n
 reclen
 x1
 x2
 x4
 bigender
}

LET start() = VALOF
{ LET argv       = VEC 50
  AND word = #x00010203 // To decide whether the current machine
                        // is a big or little ender.
  bigender := (@word)%0 = 0

  UNLESS rdargs("FROM/A,N/N,P/N,RL/K/N,RLB/K/N,TO/K,X1/S,X2/S,X4/S,LIT/S,BIG/S",
                 argv, 50) DO
  { writes("Bad arguments for HEXDUMP*n")
    stop(20,0)
  }

  stdin      := input()
  stdout     := output()

  // Set default values
  fromname   := 0
  toname     := 0
  fromstream := 0
  tostream   := 0
  pos, n, reclen := 0, 1000000, 0
  x1, x2, x4 := FALSE, FALSE, TRUE

  fromname := argv!0                            // FROM/A
  IF argv!1 DO n      := !(argv!1)              // N/N
  IF argv!2 DO pos    := !(argv!2)              // P/N
  IF argv!3 DO reclen := !(argv!3)*4            // RL/K/N
  IF argv!4 DO reclen := !(argv!4)              // RLB/K/N
  toname   := argv!5                            // TO/K
  IF argv!6 DO x1, x2, x4 := TRUE, FALSE, FALSE // X1/S 
  IF argv!7 DO x1, x2, x4 := FALSE, TRUE, FALSE // X2/S 
  IF argv!8 DO x1, x2, x4 := FALSE, FALSE, TRUE // X4/S 
  IF argv!9 DO bigender := FALSE                // LIT/S
  IF argv!10 DO bigender := TRUE                // BID/S

  fromstream := findinput(fromname)
  UNLESS fromstream DO
  { writef("Can't open file: *"%s*"*n", fromname)
    stop(20,0)
  }

  IF toname DO
  { tostream := findoutput(toname)
    UNLESS tostream DO
    { writef("Can't open %s*n", toname)
      endread()
      stop(20,0)
    }
    selectoutput(tostream)
  }


  selectinput(fromstream)
  eof := FALSE

  TEST reclen
  THEN writef("Dump of %s  records %n to %n  reclen=%n %s-ender mode*n*n",
              fromname, pos, pos+n-1, reclen, (bigender -> "big", "little"))
  ELSE writef("Dump of %s  from %n to %n %s-ender mode*n*n",
              fromname, pos, pos+n-1, (bigender -> "big", "little"))

  dump(pos, n, reclen)
  IF eof DO writef("End of file*n")

  endstream(fromstream)

  UNLESS stdout=tostream DO endstream(tostream)

  result2 := 0
  RESULTIS 0
}

AND dump(pos, n, reclen) BE
{ // reclen is 0 or the record length in bytes
  // reclen>0 dump n records from byte record number pos
  // reclen=0 dump n bytes from byte offset pos
  LET offset = pos
  IF reclen DO offset := pos*reclen

  // Get to first byte to dump
  FOR i = 1 TO offset IF binrdch()=endstreamch DO
  { eof := TRUE
    BREAK
  }

  TEST reclen
  THEN FOR recno = pos TO pos+n-1 DO
       { LET x = 1234
         dumphex(recno, ?, reclen)
         newline()
         IF eof BREAK
       }
  ELSE dumphex(pos, n, 0)
}

AND dumphex(pos, n, reclen) BE
// reclen>0   pos is the record number
//            n   is not used
// reclen=0   pos is the byte offset in the file
//            n   is the number of bytes to dump
{ LET word = #x00010203 // To decide whether the current machine
                        // is a big or little ender.
  LET xbits = (@word)%0 // =0 for bigender,  =3 for little ender
  LET count = 0         // Count of bytes read so far
  IF reclen DO n := reclen

  { LET v = VEC 15
    LET oldcount = count
    LET k = n - count   // Bytes remaining to be read
    FOR i = 0 TO 15 DO v!i := -1

    UNLESS k>0 RETURN

    IF k>16 DO k := 16
 
    // k = number of bytes to attempt to read
    FOR i = 0 TO k-1 DO
    { LET ch = binrdch()
      TEST ch=endstreamch THEN eof := TRUE
                          ELSE v!i, count := ch, count+1
    }
    // v!0, v!1,... hold up to 16 bytes from the file

    IF count=oldcount RETURN // Return is no bytes read

    // If at least one byte
    IF x1 DO writef("%i5:", pos+oldcount)

    IF x2 TEST reclen
    THEN writef("%x5 %i5/%i5:", pos,          pos,               oldcount/2)
    ELSE writef("%x5 %i5/%i5:", pos+oldcount, pos+oldcount, (pos+oldcount)/2)

    IF x4 TEST reclen
    THEN writef("%x6 %i5/%i5:", pos,          pos,               oldcount/4)
    ELSE writef("%x6 %i5/%i5:", pos+oldcount, pos+oldcount, (pos+oldcount)/4)

    IF x1 FOR p = 0 TO 15 DO
    { LET byte = v!p
      TEST byte>=0 THEN writef(" %x2", byte)
                   ELSE writef("   ")
    }

    IF x2 FOR p = 0 TO 15 DO
    { LET byte = bigender -> v!p, v!(p XOR 1) // Swap bytes for little ender M/Cs
      UNLESS p MOD 2 DO wrch(' ')
      TEST byte>=0 THEN writef("%x2", byte)
                   ELSE writef("  ")
    }

    IF x4 FOR p = 0 TO 15 DO
    { LET byte = bigender -> v!p, v!(p XOR 3) // Swap bytes for little ender M/Cs
      UNLESS p MOD 4 DO wrch(' ')
      TEST byte>=0 THEN writef("%x2", byte)
                   ELSE writef("  ")
    }

    writes("  ")
    FOR p = 0 TO 15 DO
    { LET byte = v!p
      IF x2 UNLESS p MOD 2 DO wrch(' ')
      IF x4 UNLESS p MOD 4 DO wrch(' ')
      TEST byte>=0 THEN wrch(filter(byte))
                   ELSE wrch(' ')
    }

    newline()

    //IF testflags(flag_b) DO
    //{ writef("************ BREAK*n")
    //  RETURN
    //}
  } REPEAT
}


AND filter(ch) = 32<=ch<127 -> ch, '.'
