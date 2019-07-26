GET "mcpl.h"

STATIC stdin, stdout

FUN start : =>
  stdin  := input()
  stdout := output()
  LET argv = VEC 50

  UNLESS rdargs("FROM", argv, 50) DO
  { writes "Bad argument for SPEAK\n"
    RETURN 20
  }

  LET filename = "words.txt"
  IF argv!0 DO filename := argv!0

  UNLESS filename DO { sayword "test"; RETURN 0 }

  LET wordsfile = findinput filename
  UNLESS wordsfile DO
  { writef("Unable to open file: %s\n", wordsfile)
    RETURN 20
  }
  selectinput wordsfile
  LET speaker = findoutput "/dev/audio"
  UNLESS speaker DO
  { writes "Can't open /dev/audio\n"
    endread()
    RETURN 20
  }
  selectoutput speaker

  LET ch, word=CVEC 100
  { LET wp = 0
    { ch := rdch()
      IF 'A'<=ch<='Z' DO ch +:= 'a'-'A'
      UNLESS '0'<=ch<='9' OR 'a'<=ch<='z' BREAK
      word%wp++ := ch
      word%wp   := 0
    } REPEAT
    IF wp DO sayword word
  } REPEATUNTIL ch=Endstreamch

  endwrite()
  endread()
  selectinput stdin
  selectoutput stdout
  RETURN


FUN sayword : word =>
  LET oldin=input()
  LET aufilename=CVEC 100, p=0
  LET prefix="au/", postfix=".au"
  
  // form filename  au/<word>.au
  WHILE %prefix  DO aufilename%p++ := %prefix++
  LET q = word
  WHILE %q       DO aufilename%p++ := %q++
  WHILE %postfix DO aufilename%p++ := %postfix++
  aufilename%p := 0
  
  LET aufile = findinput aufilename
  UNLESS aufile DO
  { //mess("Can't open %s\n", aufilename)
    IF word%0 AND word%1 DO
    { mess("Saying %s as characters\n", word)
      { LET k = %word++
        IF k=0 RETURN
        IF 'A'<=k<='Z' DO k +:= 'a'-'A'
        UNLESS '0'<=k<='9' OR 'a'<=k<='z' LOOP 
        LET str = CVEC 2
        str%0 := k
        str%1 := 0
        sayword str
      } REPEAT
    }
    RETURN
  }
  mess("saying: %s\n", word)
  selectinput aufile

  { LET ch = rdch()
    IF ch=Endstreamch BREAK
    wrch ch
  } REPEAT

  endread()
  selectinput oldin
  RETURN 0

FUN mess : form, a, b, c =>
  LET oldout = output()
  selectoutput stdout
  writef(form, a, b, c)
  selectoutput oldout