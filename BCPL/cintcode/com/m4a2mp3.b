/*
This program uses ffmpeg to convert the files listed in m4afiles from
m4a to mp3 format.

Implemented by Martin Richards (c) 4 Aug 2021
*/

GET "libhdr"

GLOBAL {
  stdin:ug
  stdout
  debugging
  filesstream
}

LET start() = VALOF
{ LET argv = VEC 50

  UNLESS rdargs("-d/S", argv, 50) DO
  { writef("Bad args for m4a2mp3*n")
    RESULTIS 0
  }

  debugging := argv!0     //    -d/S

  filesstream := findinput("m4afiles")
  UNLESS filesstream DO
  { writef("Unable to open file m4afiles*n")
    RESULTIS 0
  }

  { LET ch = '*''
    LET fromfilename = VEC 50
    LET tofilename   = VEC 50
    selectinput(filesstream)
    
    { // Until the stream is exhausted read a file name and process it.
      LET len = 1
      LET ch = 0
      // Ignore initial white space
      ch := rdch() REPEATWHILE ch='*n' | ch='*s'
      IF ch=endstreamch BREAK
      
      // Read the next file name
      fromfilename%len := '"'
      tofilename%len   := '"'
      UNTIL ch='*n' | ch=endstreamch DO
      { len := len+1
        fromfilename%len := ch
        tofilename%len   := ch
	ch := rdch()
      }
      len := len+1
      fromfilename%len := '"'
      tofilename%len   := '"'
      fromfilename%0 := len
      tofilename%0   := len
      
      UNLESS fromfilename%(len-4)='.' &
             fromfilename%(len-3)='m' &
             fromfilename%(len-2)='4' &
             fromfilename%(len-1)='a' DO
      { writef("file name %s does not end with .m4a*n", fromfilename)
        RESULTIS 0
      }
      tofilename%(len-2) := 'p'
      tofilename%(len-1) := '3'

      m4a2mp3(fromfilename, tofilename)
      
    } REPEAT

fin:
    IF filesstream DO endstream(filesstream)
    RESULTIS 0
  }

  RESULTIS 0
}

AND m4a2mp3(fromname, toname) BE
{ LET comstr = VEC 64
  comstr%0 := 0         // Initially of zero length
  appendstr("ffmpeg -i ", comstr)
  appendstr(fromname, comstr)
  appendstr(" ", comstr)
  appendstr(toname, comstr)
  appendstr(" ", comstr)
  writef("Executing command: %s*n", comstr)
  sys(Sys_shellcom, comstr)
  newline()
  //delay(5000)
}

AND appendstr(s1, s2) BE
{ LET len = s2%0
  FOR i = 1 TO s1%0 DO
  { len := len+1
    s2%len := s1%i
  }
  s2%0 := len
}
