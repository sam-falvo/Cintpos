/*

Implemented by Martin Richards (c) 29 July 2021

Usage: FROM/A,TO/K,-s/N,-r/S,-i/S,-d/S

This program copies the FROM file to the TO file replacing, deleting
or inserting a character. The position of the character is chosen at
random, but will not be in a comment or at the position of a character
with ASCII code in the range 0 to 31 or greater than 126. It will also
not make a change between a double quote (") and the end of the line.
Since comments are language dependent very occasionally the selected
position is in a comment.

FILE/A            The source file name.

TO/K              For the modified file.

-s/N              Set the random number seed.

-r/S  Replace     At the selected random position
                  change A to Z to a different capital letter
                  change a to z to a different lower case letter
                  change 0 to 9 changes to a different digit
                  otherwise change to a different random character in
                  the range 32 to 126

-i/S  Insert      After the selected random position
                  insert after A to Z changes to a different capital letter
                  insert after a to z changes to a different lower case letter
                  insert after 0 to 9 changes to a different digit
                  otherwise insert to a random character in the range 32 to 126

-d/S  Delete      Delete the character at the selected random position.

If none of -i, -d or -r are specified, one of inserting, deleting or
replacing is set with probabilities 20%, 30% and 50%.

This program can be used to test how well programming errors are
detected by compilers and runtime systems. Using this program it is
possible to obtain statistics of the effect such changes to a program
in any language. Possible effects include the following:

      Violation of data type rules
      Sybtactic error
      Semantic error
      Error dectected at run time.
      No compile or runtime error detected

The relative frequencies of these effects can be given as percentages.

For BCPL, the test program is the front end of the compiler, bcplfe.b and
the runtime test is to compile and run cmpltest.b.

For C, the test program is cinterp.c and the run time test is to compile
and execute cmpltest.b using cintsys with this modified version of cinterp.

The results 
*/


SECTION "BUGIFY"

GET "libhdr"

GLOBAL {
inputstream:ug
outputstream
replacing
inserting
deleting

fromfilename
tofilename

sxvupb       // The self expanding vector for comments
commentsv
sxv          // The pointer to the two control words

chpos
rch
ch

concatstr
findcomments
pushval
choosech
incomment
selectedpos
originalch
chosench
setposition
}

LET start() = VALOF
{ 
  LET rc = 0
  LET ch = 0
  LET oldoutput = output()
  LET argv = VEC 50
  
  inputstream := 0
  outputstream := 0

  // Test choosech
  //FOR i = 0 TO 25 DO
  //{ LET d = i MOD 10
  //  writef("%c => %c  %c => %c  %c = %c  %c => %c*n",
  //          'A'+i, choosech('A'+i),
  //          'a'+i, choosech('a'+i),
  //          '0'+d, choosech('0'+d),
  //          ' '+i, choosech(' '+i))
  //}
  //RESULTIS 0
  
  UNLESS rdargs("FROM/A,TO/K/A,-s/K/N,-r/S,-i/S,-d/S", argv, 50) DO
  { writes("Bad args for bugify*n")
    rc := 20
    GOTO fin
  }

  fromfilename := "com/bcplfe.b"
  tofilename   := "com/bugbcplfe.b"
  
  IF argv!0 DO fromfilename := argv!0                    // FROM
  IF argv!1 DO tofilename   := argv!1                    // TO/K
  IF argv!2 DO setseed(!argv!2)                          // -s/K/N

  replacing := argv!3                                    // -r/S
  inserting := argv!4                                    // -i/S
  deleting  := argv!5                                    // -d/S

  UNLESS replacing | inserting | deleting DO
  { LET r = randno(100) // A random number between 1 and 100
    // Set inserting, deleting or replacing with probabilities 20%, 30% and 50%
    TEST r < 20
    THEN inserting := TRUE
    ELSE TEST r < 50
         THEN deleting := TRUE
	 ELSE replacing := TRUE
  }

  writef("bugify: from %s to %s*n", fromfilename, tofilename)

  inputstream := findinput(fromfilename)
  
  UNLESS inputstream DO
  { writef("Can*'t open %s*n", fromfilename)
    rc := 20
    GOTO fin
  }

  outputstream := findoutput(tofilename)
  UNLESS outputstream DO
  { writef("Can*'t open %s*n", tofilename)
    rc := 20
    GOTO fin
  }

  selectinput(inputstream)
  selectoutput(outputstream)

  //writef("// junk line 1*n")

  sxvupb    := 0    // Initialise the self expanding vector of the
  commentsv := 0    // comments vector
  sxv := @sxvupb
  
  findcomments(@sxvupb)

IF FALSE DO
  IF commentsv FOR i = 1 TO commentsv!0 BY 2 DO
    sawritef("Preserve from %i5 to %i5*n", commentsv!i, commentsv!(i+1))

  { selectedpos := randno(chpos-1)
    setposition(cis, selectedpos-1)
    originalch := rdch()
    //sawritef("selectedpos = %n  originalch = '%c'*n",
    //          selectedpos, originalch)
  } REPEATWHILE 0 <= originalch <='*s' |  // Chars such as NL TAB or a space
                incomment(selectedpos)    // Or in a comment
	    
  rewindstream(cis)
  
  FOR i = 1 TO chpos-1 DO
  { LET ch = rdch()
    //UNLESS ch='*n' IF incomment(i) DO ch := '#'
    TEST i = selectedpos
    THEN { originalch := ch
           //ch := 'X'

           UNLESS deleting DO
           { IF inserting DO sawrch(ch)
             chosench := choosech(ch)
             wrch(chosench)
           }
         }
    ELSE { wrch(ch)
         }
  }
  IF deleting  DO writef("// Deleted charcter '%c'",
                         originalch)
  IF inserting DO writef("// Inserted charcter '%c' after '%c'",
                         chosench, originalch)
  IF replacing DO writef("// Replaced charcter '%c' by '%c'",
                         originalch, chosench)
  writef(" at position %n/%n*n", selectedpos, chpos)
  
fin:

  IF inputstream  DO endstream(inputstream)
  IF outputstream DO endstream(outputstream)
  IF commentsv    DO freevec(commentsv)
  result2 := 0
  RESULTIS rc
}

AND concatstr(s, t, r) BE
{ LET len = 0
  FOR i = 1 TO s%0 DO { len := len+1; r%len := s%i }
  FOR i = 1 TO t%0 DO { len := len+1; r%len := t%i }
  r%0 := len
}

LET pushval(sxv, val) BE
{ // sxv is a self expanding vector
  LET upb = sxv!0      // =0 or the upb of v
  LET v   = sxv!1      // =0 or a getveced vector
  // If v is not zero, v!0 will be the subscript of its latest element in v.
  // If the vector is full, pushval will allocate another
  // larger and copy the existing elements into it before
  // pushing x.

  LET p = v -> v!0, 0 // Position of the previous element, if any.
//sawritef("pushval: %n*n", val)

  // The upb of v grows as needed.

  // Initially upb, v, and p are all zero

  IF p>=upb DO
  { // We must allocate a new larger vector
    LET newupb = 3*upb/2 + 10
    LET newv = getvec(newupb)
//sawritef("pushval: sxv=%n allocating new vector at %i6 oldupb %n newupb %n*n",
//        sxv, newv, upb, newupb)
//abort(2222)
    UNLESS newv DO
    { writef("More memory needed for pushval*n")
      abort(999)
      RETURN
    }
    sxv!0 := newupb
    sxv!1 := newv
    
    // Copy the existing elements
    FOR i = 0 TO upb DO newv!i := v!i
    // Pad with zeroes
    FOR i = upb+1 TO newupb DO newv!i := 0
    // Free the old vector if it existed.
    IF v DO freevec(v)

    IF FALSE  DO
    { sawritef("pushval: replacing v=%i6 upb=%i6 with newv=%i7 upb=%i6 p=%n*n",
                v, upb, newv, newupb, p)
      //abort(6666)
    }
    v := newv
  }
  p := p+1
  v!0, v!p := p, val
}

AND rch() BE
{ ch := rdch()
  chpos := chpos+1
  IF FALSE &
    chpos>=154507 DO
  { sawritef("rch: ch='%c' %n chpos=%n*n", ch, ch, chpos)
    abort(2000)
  }
}

AND findcomments(sev) BE
{ LET depth = 0

  chpos := 0
  rch()

  UNTIL ch=endstreamch DO
  { // Start of main loop
    // If we are in a /* .. */ comment depth will be > 0

    SWITCHON ch INTO
    { DEFAULT:
        rch()
        LOOP  // Ignore this character

      CASE '/': // A slash not in a // comment or a string
        rch()

        IF ch='/' DO
        { // This is the start of a // comment
	  rch()
          IF depth=0 DO
	  { // A // not in a /* .. */ comment
	    // The start of this comment is at position chpos-1
            //sawritef("A '//' comment from position %n ", chpos-2)
	    pushval(sxv, chpos-2)
	  }
	  // Ignore the rest of the line even if inside /* .. */ comment
          UNTIL ch='*n' | ch=endstreamch DO rch()
	  IF depth=0 DO
          { // chpos is the position of the end of a // comment
	    //sawritef("to %n*n", chpos)
            //abort(1002)
            pushval(sxv, chpos)
	    LOOP
	  }
	  // ch is currently a newline or endstreamch
          LOOP
        }
 
        IF ch='**' DO
        { // A /* encountered not in a // comment or a string
	  rch()
          IF depth=0 DO
	  { // This is the start of a /* comment
            //sawritef("A '/**' comment from position %n ", chpos-2)
	    pushval(sxv, chpos-2)
	  }
          depth := depth + 1
          LOOP
        }

        // ch is the first character after a / but is not a /, * or "
        LOOP

      CASE '**':
        rch() REPEATWHILE ch='**'
	// ch is the first character after a sequence of asterisks
        IF ch='/' DO
        { // A */ has be found
          depth := depth-1
	  IF depth=0 DO
          { pushval(sxv, chpos)
            //sawritef("to %n*n", chpos)
	    //abort(1004)
	  }
	  rch() // Read the charater after the */
	  LOOP
        }
	// ch is the  character after a sequence of asterisks but not a /
        LOOP
      
      CASE '"':
	IF depth=0 DO pushval(sxv, chpos)
	rch()
	// A string encountered not in a commnent or a string
        // Ignore all characters until an unescaped " or a newline
	UNTIL ch='"' | ch='*n' | ch=endstreamch DO rdstrch()
	// ch is ", newline  or endstremch
	UNLESS ch=endstreamch DO rch()
	IF depth=0 DO pushval(sxv, chpos-1)
	LOOP
        
      CASE '*'':
	rch()
	// A character constant encountered not in a commnent or a string
	rdstrch()
	IF ch='*'' DO rch()
	LOOP
    }
  }
}

AND rdstrch() BE
{ rch()  // Read a character in a string
  IF ch='**' | ch='\' DO rch() // BCPL or C escape character
  rch()  // Read the next character
}

AND choosech(ch) = VALOF
{ LET res = -1
//writef("ch=%i3 %c*n", ch, ch)
  SWITCHON ch INTO
  { DEFAULT:
      res := 32 + randno(126-32) - 1 REPEATWHILE res=ch
      RESULTIS res

    CASE 'A': CASE 'B': CASE 'C': CASE 'D': CASE 'E': CASE 'F':
    CASE 'G': CASE 'H': CASE 'I': CASE 'J': CASE 'K': CASE 'L':
    CASE 'M': CASE 'N': CASE 'O': CASE 'P': CASE 'Q': CASE 'R':
    CASE 'S': CASE 'T': CASE 'U': CASE 'V': CASE 'W': CASE 'X':
    CASE 'Y': CASE 'Z':
      res := 'A' + randno(26) - 1 REPEATWHILE res=ch
      RESULTIS res

    CASE 'a': CASE 'b': CASE 'c': CASE 'd': CASE 'e': CASE 'f':
    CASE 'g': CASE 'h': CASE 'i': CASE 'j': CASE 'k': CASE 'l':
    CASE 'm': CASE 'n': CASE 'o': CASE 'p': CASE 'q': CASE 'r':
    CASE 's': CASE 't': CASE 'u': CASE 'v': CASE 'w': CASE 'x':
    CASE 'y': CASE 'z':
      res := 'a' + randno(26) - 1 REPEATWHILE res=ch
      RESULTIS res

    CASE '0': CASE '1': CASE '2': CASE '3': CASE '4':
    CASE '5': CASE '6': CASE '7': CASE '8': CASE '9':
      res := '0' + randno(10) - 1 REPEATWHILE res=ch
      RESULTIS res
  }
}

AND incomment(pos) = VALOF
{ FOR i = 1 TO commentsv!0 BY 2 DO
  { IF pos < commentsv!i LOOP
    IF pos <= commentsv!(i+1) RESULTIS TRUE
  }
  RESULTIS FALSE
}

AND setposition(scb, pos) = VALOF
{ // pos is the byte position in the stream
  LET blockno = pos  /  scb!scb_blength
  AND offset  = pos MOD scb!scb_blength
  RESULTIS point(scb, @blockno)
}
