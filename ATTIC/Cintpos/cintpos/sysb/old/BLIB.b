// (c)  Copyright:  Martin Richards  24 April 2004

/*
24/4/04
Made many changed to make BLIB more compatible between Cintpos and
single threaded Cintcode BCPL.

21/3/2003
Make instrcount(f,a,b,...) set result2 to result of f(a,b,c,....)

10/7/2000
Changed the definition of mkobj to take up to 11 initialisation
arguments. See bcplprogs/objdemo.b

28/2/2000
Added function instrcount(f,a,b,c,e,f,r,g,h,i,j,k)
which returns the number of cintcode instructions executed
when calling f(a,b,...).

30/4/1996
Added function flush()
with corresponding change in cintmain.c

7/6/1996
Defined mkobj(upb, fns, a, b) for use in object oriented programming.
See bcplprogs/objdemo.b  (args a and b added 30 March 1999).
*/

SECTION "BLIB"

GET "libhdr"
GET "manhdr"

LET stop(n) BE cowait(n) // Typically returning from a CLI command
                         // with error code n

AND fault(code) BE sawritef("BLIB: fault: code=%n*n", code)

AND clihook(a1) = start(a1)

AND intflag()          =  sys(Sys_intflag)  // returns TRUE if user interrupt

AND abort(code)        BE sys(Sys_quit, code)

AND level(p3)          =  (@p3)!-3

AND longjump(lev, lab) BE { LET p = @lev - 3; p!0, p!1 := lev, lab }

AND sardch()   = sys(Sys_sardch)

AND sawrch(ch) = sys(Sys_sawrch,ch)

// msecs>0   The stream timeout value in milli-seconds
// msecs=0   No timeout value (the default)
// msecs<0   Only perform non blocking operations on the stream
AND settimeout(scb, msecs) BE scb!scb_timeout := msecs
 
// rdch() returns the next byte from the currently selected
//        input stream.
// If the buffer is empty, it calls replenish to attempt to refill it.
//// It aborts 186 if no input stream is selected.

AND rdch() = VALOF
{ LET pos = cis!scb_pos // Position of next byte

  UNLESS cis DO abort(186)
  IF pos<cis!scb_end DO { LET ch = cis!scb_buf%pos
                          cis!scb_pos := pos+1
                          RESULTIS ch
                        }
  // No byte available, so attempt to replenish the buffer
  // If replenish returns FALSE, no chars were placed in the buffer
  // and the reason why not is placed in result2 as follows
  //    result2 = -1    end of file
  //    result2 = -2    timeout
  //    result2 = -3    polling and no characters available.
  //    result2 = code  error code
  UNLESS replenish(cis) RESULTIS result2<0 -> result2, endstreamch
  // Successful replenishment so try rdch again
  // There will be at least one ch in the buffer
} REPEAT

// unrdch() attempts to step input back by one byte position.
// It returns: TRUE if successful, and
//             FALSE otherwise
// After a call of rdch() it will always be successful at least once.
//// It aborts: 186 if not input stream, is selected.

AND unrdch() = VALOF
{ LET pos = cis!scb_pos
  //UNLESS cis DO abort(186)
  IF pos<=0 RESULTIS FALSE
  cis!scb_pos := pos-1
  RESULTIS TRUE
}

// wrch(ch) writes ch to the current output stream.
// It returns TRUE if successful, FALSE otherwise
//// It aborts: 187 if no stream is selected ???????
////            189 on depletion failure.
// *e no longer calls deplete on interactive streams MR 10/1/03

AND wrch(ch) = VALOF
{ LET pos = cos!scb_pos

//sawritef("BLIB: wrch: pos=%i3 ch=%i3 '%c'*n", pos, ch, ch)

  IF ch='*e' DO // The help removal of calls of wrch('*e')
  { sawritef("BLIB: wrch(ESC) called*n")
    abort(999)
  }

  // If the buffer is full try to deplete it.
  IF pos >= cos!scb_bufend DO
  { UNLESS deplete(cos) RESULTIS FALSE
    UNLESS cos!scb_buf  RESULTIS TRUE // Must be writing to NIL:
    pos := cos!scb_pos
  }

  // The buffer is not full
  cos!scb_buf%pos := ch
  pos := pos+1
  cos!scb_pos := pos
  // Advance end of valid data, if necessary
  IF cos!scb_end < pos DO cos!scb_end := pos
  cos!scb_write := TRUE // Set flag to indicate new data in buffer

  UNLESS cos!scb_type<0 & ch<'*s' RESULTIS TRUE

  // Check whether to call deplete for interactive streams.

  IF ch='*n' DO  wrch('*c')  // Temp fiddle for cygwin

  IF ch='*n' | ch='*p' RESULTIS deplete(cos)
  RESULTIS TRUE
}

AND binwrch(ch) = wrch(ch | 256)

AND readwords(vector, count) = VALOF
{ LET i, lim = 0, count*bytesperword
//sawritef("BLIB co=%n: readwords count=%n scb=%n block=%n pos=%n*n",
//          currco, count, cis, cis!scb_block, cis!scb_pos)

  IF count<=0 RESULTIS 0

  { LET pos = cis!scb_pos // Position of next byte
    AND end = cis!scb_end // Position past last byte
    AND buf = cis!scb_buf // Byte buffer -- replenish might change buf

    WHILE pos < end DO    // Copy bytes -- more needed
    { // At least one byte available and needed
      vector%i := buf%pos       // Copy it
      i, pos := i+1, pos+1
      IF i<lim LOOP            // More byte(s) needed
      // Successful completion
      cis!scb_pos := pos
      RESULTIS count
    }

    cis!scb_pos := pos

    // No byte available, so attempt to replenish the buffer
    UNLESS replenish(cis) RESULTIS i/bytesperword
    // Successful replenishment so copy some more bytes
    // There will be at least one byte in the buffer
  } REPEAT
}

AND writewords(vector, count) = VALOF
{ LET i, len = 0, count*bytesperword // Length in bytes

//sawritef("BLIB co=%n: writewords count=%n scb=%n block=%n pos=%n*n",
//          currco, count, cos, cos!scb_block, cos!scb_pos)

  IF len<=0 RESULTIS FALSE

  { LET pos    = cos!scb_pos
    AND bufend = cos!scb_bufend
    AND buf    = cos!scb_buf

    // If the buffer is full try to deplete it.
    WHILE pos < bufend DO
    { // There is a byte available and room in the buffer
      buf%pos := vector%i    // so copy it
      i, pos := i+1, pos+1
      IF i<len LOOP          // Loop if another byte available

      cos!scb_pos := pos     // Update SCB and return successfully
      // Advance end of valid data, if necessary
      IF cos!scb_end < pos DO cos!scb_end := pos
      cos!scb_write := TRUE // At least one byte has been written

      RESULTIS TRUE
    }

    // The buffer is full so update the SCB and deplete
    cos!scb_pos := pos
    // Advance end of valid data, if necessary
    IF cos!scb_end < pos DO cos!scb_end := pos
    IF i>0 DO cos!scb_write := TRUE // TRUE if at least one byte has been written

    UNLESS deplete(cos) RESULTIS FALSE
  } REPEAT
}

// get.record returns TRUE if successful
// it returns FALSE if eof is encountered before the whole record
// has been read.
// MR 29/7/02: First record of a file has record number 0 (not 1)
AND get.record (vector, recno, scb) = VALOF
{ LET i, len = 0, scb!scb_reclen // in bytes

//sawritef("BLIB co=%n: get.record recno=%n reclen=%n blk=%n pos=%n end=%n*n",
//   currco, recno, scb!scb_reclen, scb!scb_block, scb!scb_pos, scb!scb_end)
  recordpoint(scb, recno)
//sawritef("BLIB: get.record recno=%n reclen=%n pos=%n end=%n*n",
//           recno, scb!scb_reclen, scb!scb_pos, scb!scb_end)

  IF len<=0 RESULTIS FALSE

  { LET pos = scb!scb_pos // Position of next byte
    AND end = scb!scb_end // Position past last byte
    AND buf = scb!scb_buf // Byte buffer -- replenish might change buf

    WHILE pos < end DO    // Copy bytes -- more needed
    { // At least one byte needed
      vector%i := buf%pos
//sawritef("BLIB co=%n: get.record byte=%x2*n", currco, vector%i)
      i, pos := i+1, pos+1
      IF i<len LOOP       // More byte(s) needed
      // Successful completion
      scb!scb_pos := pos
//sawritef("BLIB co=%n: get.record recno=%n len=%n successful*n",
//          currco, recno, len)
      RESULTIS TRUE
    }

    scb!scb_pos := pos

    // No byte available, so attempt to replenish the buffer
    UNLESS replenish(scb) DO
    {
//sawritef("BLIB co=%n: get.record recno=%n len=%n hit eof at %n*n",
//          currco, recno, len, i)
      RESULTIS FALSE  // Failure due to eof, timeout, error etc
    }
    // Successful replenishment so copy some more bytes
    // There will still be at least one byte in the buffer
  } REPEAT
}

// MR 29/7/02: The first record of a file has number 0 (not 1)
AND put.record(vector, recno, scb) = VALOF
{ LET i, len = 0, scb!scb_reclen

  UNLESS scb!scb_id=id_inoutscb DO
  { sawritef("BLIB co=%n: put.record id not inout*n", currco)
    abort(999)
  }

  IF len<=0 RESULTIS FALSE // Error -- no record length

//sawritef("BLIB: put.record recno=%n reclen=%n blk=%n pos=%n end=%n*n",
//           recno, scb!scb_reclen, scb!scb_block, scb!scb_pos, scb!scb_end)
  recordpoint(scb, recno)
//sawritef("BLIB: put.record recno=%n reclen=%n blk=%n pos=%n end=%n*n",
//           recno, scb!scb_reclen, scb!scb_block, scb!scb_pos, scb!scb_end)
//abort(2222)
  { LET pos    = scb!scb_pos
    AND bufend = scb!scb_bufend
    AND buf    = scb!scb_buf

    // If the buffer is full try to deplete it.
    WHILE pos < bufend DO
    { // There is a byte available and room in the buffer
      buf%pos := vector%i    // so copy it
      i, pos := i+1, pos+1
      scb!scb_write := TRUE  // At least one byte has been written
      IF i<len LOOP          // Loop if another byte available

      scb!scb_pos := pos     // Update SCB and return successfully
      // Advance end of valid data, if necessary
      IF scb!scb_end < pos DO scb!scb_end := pos
//      scb!scb_write := TRUE // At least one byte has been written

      RESULTIS TRUE         // Successful completion
    }

    // The buffer is full so update the SCB and deplete
    scb!scb_pos := pos
    // Advance end of valid data, if necessary
    IF scb!scb_end < pos DO scb!scb_end := pos
//  IF i>0 DO scb!scb_write := TRUE // TRUE if at least one byte has been written

    UNLESS deplete(scb) RESULTIS FALSE
  } REPEAT
}

// replenish(scb) returns:
//   TRUE                Successful replenishment, at least one ch read
//   FALSE result2 = -1  End of file, no chars read     // MR 15/4/03
//   FALSE result2 = -2  Timeout, no chars read -- none yet available
//   FALSE result2 = -3  Polling, no chars read -- none available
//   FALSE result2       Error code

AND replenish(scb) = VALOF
{ LET rdfn = scb!scb_rdfn
  result2 := -1
  // The condition scb!scb_end<0 indicates that the stream is exhausted
  UNLESS scb!scb_end>=0 & rdfn & rdfn(scb) RESULTIS FALSE
  RESULTIS TRUE
}

// deplete(scb) returns:
//   TRUE  Successful depletion, or
//   FALSE otherwise.
// It aborts: 187 if scb is not a suitable stream.

AND deplete(scb) = VALOF
{ LET wrfn = scb!scb_wrfn 
  IF scb!scb_end<0 RESULTIS FALSE /// ????? result2
  UNLESS wrfn DO abort(187)
  RESULTIS wrfn(scb)
}

AND findinput   (string) =
  findstream(string, Action_findinput,    id_inscb,    0)

AND pathfindinput(string, path) =
  findstream(string, Action_findinput,    id_inscb, path)

AND findoutput  (string) =
  findstream(string, Action_findoutput,   id_outscb,   0)

AND findinoutput(string) =
  findstream(string, Action_findinoutput, id_inoutscb, 0)

AND findupdate(string) = findinoutput(string)  // MR 20/9/02

AND selectinput(scb) BE // scb=0 is occasionally used
{ UNLESS scb=0 | scb!scb_id=id_inscb | scb!scb_id=id_inoutscb DO abort(186)
  cis := scb
}

AND selectoutput(scb) BE // scb=0 is occasionally used
{ UNLESS scb=0 | scb!scb_id=id_outscb | scb!scb_id=id_inoutscb DO abort(187)
  cos := scb
}

AND endread() BE endstream(cis)

AND endwrite() BE endstream(cos)

AND endstream(scb) BE TEST scb>0
THEN { LET endfn = scb!scb_endfn
       LET res2 = result2
//  IF scb!scb_type=0 DO sawritef("BLIB: endstream of a RAM stream*n")
//sawritef("BLIB: endstream called*n"); abort(4444)
       IF endfn DO endfn(scb)
       freevec(scb)
       IF cis = scb DO cis := 0
       IF cos = scb DO cos := 0

       result2 := res2
     }
ELSE IF scb<0 DO // Safety check
     { sawritef("*nBLIB: endstream given negative scb=%n*n", scb)
       abort(999)
     }

AND input() = cis

AND output() = cos

AND note(scb, posv) = VALOF
{ LET type = scb!scb_type
  AND task = scb!scb_task
  UNLESS type=scbt_file RESULTIS FALSE // Must be a disc file
  RESULTIS sendpkt(-1, task, Action_note, 0, 0, scb, posv)
}

AND point(scb, posv) = VALOF
{ LET type = scb!scb_type
  AND id   = scb!scb_id
  AND task = scb!scb_task
//sawritef("BLIB: point posv!0=%n posv!1=%n*n", posv!0, posv!1)
  UNLESS type=scbt_file &  // Check that scb is a suitable stream
         (id=id_inscb | id=id_inoutscb) RESULTIS FALSE
  RESULTIS sendpkt(-1, task, Action_point, 0, 0, scb, posv)
}

AND readn() = VALOF
{ LET sum, ch, neg = 0, 0, FALSE

  { ch := rdch()
    IF '0'<=ch<='9' BREAK
    SWITCHON ch INTO
    { DEFAULT:   unrdch()
                 result2 := -1
                 RESULTIS 0
      CASE '*s':
      CASE '*t':
      CASE '*n': LOOP

      CASE '-':  neg := TRUE
      CASE '+':  ch := rdch()
                 BREAK
    }
  } REPEAT

  WHILE '0'<=ch<='9' DO
  { sum := 10 * sum + ch - '0'
    ch := rdch()
  }
  IF neg DO sum := -sum
  unrdch()
  result2 := 0
  RESULTIS sum
}

AND newline() BE wrch('*n')

AND writed(n, d) BE writedz(n, d, FALSE)

AND writez(n, d) BE writedz(n, d, TRUE)

AND writedz(n, d, zeroes) BE
{ LET t = VEC 10
  LET i = 0
  LET k = -n

  IF n<0 DO { d := d - 1; k := n }

  { t!i := -(k REM 10)
    k   := k/10
    i   := i + 1
  } REPEATWHILE k

  IF n<0 & zeroes DO wrch('-')
  FOR j = i+1 TO d DO wrch(zeroes -> '0', '*s')
  IF (n<0) & ~zeroes DO wrch('-')
  FOR j = i-1 TO 0 BY -1 DO wrch(t!j+'0')
}

AND writen(n) BE writed(n, 0)

AND writehex(n, d) BE 
{ IF d>1 DO writehex(n>>4, d-1)
  wrch((n&15)!TABLE '0','1','2','3','4','5','6','7',
                    '8','9','A','B','C','D','E','F')
}

AND writeoct(n, d) BE
{ IF d > 1 DO writeoct(n>>3, d-1)
  wrch((n&7)+'0')
}

AND writebin(n, d) BE
{ IF d > 1 DO writebin(n>>1, d-1)
  wrch((n&1)+'0')
}

AND writes(s) BE
{ UNLESS 0 < s < rootnode!rtn_memsize DO s := "##Bad string##"
  FOR i = 1 TO s%0 DO wrch(s%i)
}

AND writet(s, n) BE
{ writes(s)
  FOR i = 1 TO n-s%0 DO wrch('*s')
}

AND writeu(n, d) BE
{ LET m = (n>>1)/5
  IF m DO { writed(m, d-1); d := 1 }
  writed(n-m*10, d)
}


/*
        The following routines provide and extended version of writef.
They support the following extra substitution items:

        1. %F   - Takes next argument as a writef format string and
                calls writef recursively using the remaining arguments.
                The argument pointer is positioned to the next available
                argument on return.

        2. %M   - The next argument is taken as a message number and processed
                as for %F above. The message format string is looked up by
                get.text(messno, str, upb) where str is a vector local to
                writef to hold the message string.

        3. %+   - The argument pointer is incremented by 1.

        4. %-   - The argument pointer is decremented by 1.

        5. %P   - Plural formation. The singular form is use if and only if
                the next argument is one. So that the argument can be used
                twice it is normal to preceed or follow the %P item with %-.
                There are two forms as follows:

                a. %P\singular\plural\  - The appropriate text is printed, the
                        other is skipped. The '\' chars are not printed.

                b. %Pc  - The character c is output if the the next argument
                        not one.
*/

// The following version of writef is new -- MR 21/1/04
///*

// get.textblib and get.text have the same global variable number
AND get.textblib(n, str, upb) = VALOF  // Default definition of get.text
                                       // This is normally overridden.
{ LET s = "<mess:%-%n>"
  IF upb>s%0 DO upb := s%0
  str%0 := upb
  FOR i = 1 TO upb DO str%i := s%i
  RESULTIS str
}

AND writef(format,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) BE
{ LET nextarg = @a
  write.format(format, @nextarg)
}

AND write.format(format, lvnextarg) BE
{ LET res2 = result2

  UNLESS 0 < format < rootnode!rtn_memsize DO format := "##Bad format##"

  FOR p = 1 TO format%0 DO
  { LET k, type, f, n, arg = format%p, ?, ?, ?, ?
    UNLESS k='%' DO { wrch(k); LOOP }

    // Deal with a substitution item
    p := p + 1
    type, arg, n := format%p, !!lvnextarg, 0

    SWITCHON capitalch(type) INTO
    { DEFAULT:    wrch(type)
                  LOOP

      CASE 'S':   f := writes;    GOTO noargs
      CASE 'T':   f := writet;    GOTO getarg
      CASE 'C':   f := wrch;      GOTO noargs
      CASE 'O':   f := writeoct;  GOTO getarg
      CASE 'X':   f := writehex;  GOTO getarg
      CASE 'I':   f := writed;    GOTO getarg
      CASE 'N':   f := writen;    GOTO noargs
      CASE 'U':   f := writeu;    GOTO getarg
      CASE 'Z':   f := writez;    GOTO getarg
      CASE 'B':   f := writebin;  GOTO getarg

    getarg:       p := p + 1
                  n := capitalch(format%p)
                  n := '0' <= n <= '9' -> n - '0', 10 + n - 'A'

    noargs:       f(arg, n)
                  !lvnextarg := !lvnextarg + 1
                  LOOP

      CASE '$':
      CASE '+':   !lvnextarg := !lvnextarg + 1
                  LOOP

      CASE '-':   !lvnextarg := !lvnextarg - 1
                  LOOP

      CASE 'M': { LET buf = VEC 256/bytesperword
                  !lvnextarg := !lvnextarg + 1
                  UNLESS get.text(arg, buf, 256/bytesperword) DO
                    buf := "<<mess:%-%n>>"  // No message text
                  write.format(buf, lvnextarg)
                  LOOP
                }

      CASE 'F':   !lvnextarg := !lvnextarg + 1
                  write.format(arg, lvnextarg)
                  LOOP

      CASE 'P': { LET plural = arg ~= 1
                  !lvnextarg := !lvnextarg + 1
                  p := p+1
                  type := format%p
                  IF type = '\' DO
                  { // Deal with %P\singular\plural\ item
                    LET skipping = plural
                    p := p + 1
                    UNTIL p > format%0 DO
                    { LET ch = format%p
                      TEST ch = '\' THEN { skipping := ~skipping
                                           IF skipping = plural BREAK
                                         }
                                    ELSE UNLESS skipping DO wrch(ch)
                      p := p + 1
                    }
                    LOOP
                  }

                  // Deal with simple %Pc items
                  IF plural DO wrch(type)
                  LOOP
                }
    } // End of SWITCHON ...
  } // End of FOR p = ...

  result2 := res2
}

STATIC { seed = 12345 }

LET randno(upb) = VALOF  // return a random number in the range 1 to upb
{ seed := seed*2147001325 + 715136305
  RESULTIS (ABS(seed/3)) REM upb + 1
}

AND setseed(newseed) = VALOF // Added by MR 20/01/2000
{ LET oldseed = seed
  seed := newseed
  RESULTIS oldseed
}


// muldiv is now implemented in SYSLIB using the MDIV instruction
// NO -- MDIV sometimes causes a floating point exception
AND muldiv(a, b, c) = sys(Sys_muldiv, a, b, c)

AND unpackstring(s, v) BE FOR i = s%0 TO 0 BY -1 DO v!i := s%i

AND packstring(v, s) = VALOF
{ LET n = v!0 & 255
  LET size = n/bytesperword
  FOR i = 0 TO n DO s%i := v!i
  FOR i = n+1 TO (size+1)*bytesperword-1 DO s%i := 0
  RESULTIS size
}

AND capitalch(ch) = 'a' <= ch <= 'z' -> ch + 'A' - 'a', ch

AND compch(ch1, ch2) = capitalch(ch1) - capitalch(ch2)

AND compstring(s1, s2) = VALOF
{ LET lens1, lens2 = s1%0, s2%0
  LET smaller = lens1 < lens2 -> s1, s2
  FOR i = 1 TO smaller%0 DO
  { LET res = compch(s1%i, s2%i)
    IF res RESULTIS res
  }
  IF lens1 = lens2 RESULTIS 0
  RESULTIS smaller = s1 -> -1, 1
}

AND getkey(keys, i, keyword) = VALOF
{ LET len = keys%0
  LET n = 0
  LET p = 1

  UNTIL p>len DO
  { UNLESS i BREAK
    IF keys%p=',' DO i := i-1
    p := p+1
  }

  WHILE p <= len DO
  { LET ch = keys%p
    IF ch='/' | ch='=' | ch=',' BREAK
    n := n + 1
    keyword%n := keys%p
    p := p + 1
  }

  keyword%0 := n
  RESULTIS keyword
}

/* rdargs provides the programmer with the facility to read
   arguments from the currently selected input and store them in a
   vector as shown below:

                          |----------|
                          |address1  |
                          |__________|
                          | TRUE     |<- logic switch value
                          |__________| 
                          |address2  |
                          |----------|
                          |item1     |
                          |__________|
                          |item2     |
                          |__________|

The possible key qualifiers are:

  /k   keyed    -- argument requires the keyword
  /p   prompted -- prompt will be displayed  (This option may be scrapped)
  /a   required -- if this argument is not entered an error will follow
  /n   numeric  -- argument has to be a number
  /s   switch   -- exact switch word has to be entered for TRUE
*/  

AND rdargs(keys, argv, size) = VALOF
{ MANIFEST
  { required    = 1                  //  1   /A
    keyed       = 1 << 1             //  2   /K
    switch      = 1 << 2             //  4   /S
    prompt      = 1 << 3             //  8   /P
    number      = 1 << 4             // 16   /N
    control     = (number << 1) - 1  // 31   All the flags
  }

  LET w        = argv // w is a moving positional pointer of the argv vector
  LET numbargs = ?
  LET status   = TRUE   // Set to FALSE when an error is encountered
  LET keyword  = VEC 30

  !w := 0

//////////////// A BIT MAP REPRESENTING THE KEY QUALIFIERS IS ///////////////
///////////////  PUT INTO THE TOP LOCATIONS OF THE VECTOR     /////////////// 

  // A typical key string is: "FROM=DATA/A,TO/K,VAL/K/N,T=TRACE/S"
  FOR p = 1 TO keys%0 DO
  { LET kch = keys%p

    IF kch = '/' DO
    { LET c = capitalch(keys%(p+1))
      IF c = 'A' DO !w := !w | required
      IF c = 'K' DO !w := !w | keyed
      IF c = 'S' DO !w := !w | switch
      IF c = 'P' DO !w := !w | prompt
      IF c = 'N' DO !w := !w | number
    }

    IF kch = ',' DO
    { w := w + 1
      !w := 0
    }
  }

  w := w + 1
  numbargs := w - argv  // as deduced from the keys string


////////////////////////////////////////////////////////////////////////
//////////////////////// BEGINNING OF REPEAT LOOP //////////////////////
////////////////////////////////////////////////////////////////////////

  { LET argno = -1
    LET wsize = size - (w - argv) // Number of words remaining in argv
    LET itemtype = rditem(w, wsize)
//sawritef("BLIB: rdargs  itemtype=%n*n", itemtype)
    clear.words(keyword, 31)

    SWITCHON itemtype INTO
    { DEFAULT: // Unknown item type
err:
             { LET ch = ?
//sawritef("BLIB: rdargs error -- skipping to end of line*n")
               unrdch()   // MR 14/7/03
               ch := rdch() REPEATUNTIL ch='*n' |
                                        ch=';'  |
                                        ch=endstreamch

               result2 := 120  // Fault: Argument line 
                               // invalid or too long
               RESULTIS 0      // Error result
             }

      CASE 0:  // endstreamch
      CASE 3:  // newline
      CASE 4:  // semicolon
// These item types mark the end of the argument list.

// Now check for prompted arguments.

               FOR i = 0 TO numbargs -1 IF 0<=argv!i<=control DO
               { // Unset argument found
                 LET a = argv!i

                 IF status & ((a & prompt) ~= 0) DO
                 { // if ok so far & prompted
                   IF cis!scb_type = scbt_console &
                      cos!scb_type = scbt_console DO
                   { writes(getkey(keys, i, keyword))
                     UNLESS (a & switch) = 0 DO writes(" (yes/no)")
                     writes(" > ") // write prompt
                     deplete(cos) // MR 13/1/03

                     itemtype := rditem(w, wsize)
                     SWITCHON itemtype INTO
                     { CASE 1: // Unquoted item
                       CASE 2: // Quoted item
//writef("*nvalue=%s*n", w)
                               IF (a & switch) ~= 0 DO
                               { IF compstring(w, "yes") = 0 DO
                                 { argv!i := TRUE
                                   LOOP // Find next uset argument
                                 }
                                 IF compstring(w, "no") = 0 DO
                                 { argv!i := FALSE
                                   LOOP // Find next uset argument
                                 }
                               }

                               IF (a & number) ~= 0 DO
                               { argv!i := w   // numeric
                                 IF string.to.number(w) DO
                                 { !w := result2
                                   w  := w + 1
                                   LOOP // Find next uset argument
                                 }
                                 status := FALSE
                                 argv!i := 0
                                 LOOP // Find next uset argument
                               }

                               // Non switch non numeric argument
//writef("*nargv!%n = %s  non switch/num*n", i, w)
                               argv!i := w
                               w := w + w%0/bytesperword + 1
                               wsize := size - (w - argv)     
                               LOOP // Find next unset argument

                       CASE 0:
                       CASE 3:
                       CASE 4:
writef("BLIB: rdargs: case 0, 3 or 4 reached*n")
                               ENDCASE

                       DEFAULT:
                               status := FALSE
                               LOOP
                     }
                   }
                 }
                            
                 TEST (a & required) = 0  // final check for required
                 THEN argv!i := 0         // argument - clear memory
                 ELSE status := FALSE     // if no input made and not 
                                          // required
               } // End of for-loop

               UNLESS status GOTO err
               result2 := 0
               FOR i = 0 TO numbargs-1 IF argv!i DO result2 := result2 + 1
               // result2 = number of args given
//               writef("*nitemtype=%n calling rdch() => '%c'*n", itemtype, rdch())
//               writef("*nreturning rdch() => '%c'*n", rdch())
               RESULTIS w

     CASE 1:   // Ordinary item
               // notice that the CASE 1 and CASE 2 sections will
               // both be executed consecutively for any input other 
               // then items in quotes

               argno := findarg(keys, w)
//writef("Ordinary item %s  argno=%n*n", w, argno)

               TEST argno >= 0
               THEN { UNLESS 0 <= argv!argno <= control GOTO err

                      IF (argv!argno & switch) ~= 0 DO
                      { argv!argno := -1
                        LOOP
                      }

                      { LET item = rditem(w, wsize)

                        IF item = 5 DO item := rditem(w, wsize) // Skip '='

                        IF item <= 0 | item=3 | item=4 | item=5 GOTO err
                      }
                    }

               ELSE TEST rdch() = '*n' & compstring("?", w) = 0
                    THEN { writef("%s: ", keys)
                           deplete(cos) // MR 13/1/03
                           ENDCASE
                         }
                    ELSE unrdch()

// Deliberate missing 'ENDCASE'

            CASE 2: // item was not a keyword or was put in quotes
                IF argno < 0 DO      // find first non-keyed, non-switch
                  FOR i = 0 TO numbargs - 1 DO
                    IF 0 <= argv!i <= control &
                       (argv!i & switch) = 0  &
                       (argv!i & keyed)  = 0  DO 
                    { argno := i
                      BREAK
                    }

// The FOR-loop below the CASE 2 ends here

                UNLESS argno >= 0 DO
                { // keyword is not recognized
                  GOTO err     // MR 27/3/03
                  status := FALSE
                  LOOP
                }

                UNLESS 0 <= argv!argno <= control GOTO err

// This part of the module is always executed if the entered argument was
// found to be valid; an address from vector is stored in the top half
// of the vector argv

                { LET a = argv!argno

                  IF (a & number) ~= 0 DO
                  { IF string.to.number(w) DO // check if the
                    { argv!argno := w
                      !w := result2           // expected numeric input
                      w  := w + 1             // can be string converted
                      LOOP
                    }

                    // Number cannot be converted
                    status := FALSE
                    argv!argno := 0
                    LOOP
                  }

                  // Store ordinary argument value
                  argv!argno := w
                  w := w + w%0/bytesperword + 1
                  LOOP
                }
    } // End of main switch
  } REPEAT
}

// Read an item from current input stream

// returns -1    error, input too long or unmatched quote
//          0    endstreamch            *** MR change 11/12/92
//          1    unquoted item
//          2    quoted item
//          3    *n                     *** MR change 11/12/92
//          4    ;                      *** MR change 11/12/92
//          5    =                      *** MR change 12/07/03

// When an unquoted item is read its terminating character is
// unrdch-ed so that it can be read again by the next call of rdch.
// All items other items, namely strings, newline, ';', '=' and
// endstreamch, are self terminating and so do not need unrdch
// to be called.

AND rditem(v, upb) = VALOF
{ LET p, pmax = 0, (upb+1)*bytesperword-1
  // With bytesperword=4
  // upb=0 => pmax=3
  // upb=1 => pmax=7
  // ...
  LET ch, quoted = rdch(), FALSE

  FOR i = 0 TO upb DO v!i := 0

//sawritef("*nrditem first ch = '%c'*n", ch)

  // Skip over white space.
  WHILE ch='*s' | ch='*t' | ch='*c'DO ch := rdch() 

  IF ch=endstreamch RESULTIS  0   // EOF
  IF ch='*n'        RESULTIS  3   // '*n'
  IF ch=';'         RESULTIS  4   // ';'
  IF ch='='         RESULTIS  5   // '='

  IF ch='"' DO { ch :=  rdch()
                 IF ch='*c' LOOP
                 IF ch='*n' | ch=endstreamch RESULTIS -1 // Error
                 IF ch='"' RESULTIS 2 // Found a quoted string.
                 IF ch='**' DO { ch := rdch()
                                 IF capitalch(ch)='N'  DO ch := '*n'
                                 IF capitalch(ch)='*"' DO ch := '*"' // MR 8/1/03
                               }
                 p := p+1
                 IF p>pmax RESULTIS -1 // Error
                 v%0, v%p := p, ch
               } REPEAT

  // Copy chars of an unquoted item into v
  UNTIL ch='*n' | ch='*s' | ch='*t' | ch=';' | ch='=' | ch=endstreamch DO
  { p := p+1
    IF p>pmax RESULTIS -1              // Error
    v%0, v%p := p, ch
    ch := rdch() REPEATWHILE ch='*c'
  }
  // Unrdch its terminating character

//sawritef("rditem returning type 1 %s, ch=%x2 '%c'*n", v, ch, ch)
  UNLESS ch=endstreamch DO unrdch()
  RESULTIS 1                            // Unquoted item
}

AND findarg(keys, w) = VALOF
{ MANIFEST { matching = 0; skipping = 1 }
  LET state, wp, argno = matching, 0, 0
  FOR i = 1 TO keys%0 DO
  { LET kch=keys%i
    IF state=matching DO
    { IF (kch='=' | kch='/' | kch=',') & wp=w%0 DO
        RESULTIS argno
      wp := wp + 1
      UNLESS compch(kch, w%wp) = 0 DO state := skipping
    }
    IF kch=',' | kch='=' DO state, wp := matching, 0
    IF kch = ',' DO argno := argno + 1
  }
  IF state = matching & wp = w%0 RESULTIS argno
  RESULTIS -1
}

AND callseg(file, arg1, arg2, arg3, arg4) = VALOF
{ LET res = 0
  LET seg = loadseg(file)
  LET s = start
//sawritef("BLIB: callseg %s entered*n", file)

  TEST seg & globin(seg)
  THEN res := start(arg1, arg2, arg3, arg4)
  ELSE { sawritef("BLIB: Unable to callseg %s seg=%n*n", file, seg)
         abort(999)
         start := s
         RESULTIS 0
       }
  unloadseg(seg)
  start := s
  RESULTIS res
}


AND datstring(v) = VALOF
{ LET datv = VEC 2
  datstamp(datv)
  dat.to.strings(datv, v)
  RESULTIS v
}

AND dat.to.strings(datv, v) = VALOF

// Returns v containing 3 strings representing the
// time and date given in datv, where
// datv!0 = days, datv!1 = mins, datv!2 = ticks.
// On return, v contains a the date in the form
// DD-MMM-YY, v+5 contains the time in the format
// HH:MM:SS, and V+10 contains the day of the week.
// Vector v should have an upperbound of 14
// If the date is unset (days = 0) then the strings
// are all set to "<unset>"

{ LET days,  mins,  ticks = datv!0, datv!1, datv!2
  LET datestr, timestr, dowstr = v, v+5, v+10
  LET dayofweek = days REM 7
  LET dowtemp = ?
  LET year = 1978 // Cintpos base year
  LET month = 1
  LET hours, secs = ?, ?
  LET monthtab     = TABLE   0, 31, 59, 90,120,151,
                           181,212,243,273,304,334,365
  LET leapmonthtab = TABLE   0, 31, 60, 91,121,152,
                           182,213,244,274,305,335,366
  LET mchars = "JanFebMarAprMayJunJulAugSepOctNovDec"
  LET mcharbase = ?
  LET mtable = ?

//sawritef("BLIB: dat.to.string: entered*n")

  // Deal with case of unset date
  IF days = 0 DO
  { LET unset = "<unset>"
    FOR z = 0 TO unset%0 DO 
    { LET c = unset%z
      datestr%z := c
      timestr%z := c
      dowstr%z  := c
    }
    RESULTIS v
  }

  days := days + 1
  FOR j=0 TO 9 DO datestr%j := "DD-MMM-YY"%j
  FOR j=0 TO 8 DO timestr%j := "HH:MM:SS"%j

  // Construct date

  { // Loop to get year
    LET yearlen = isleap(year) -> 366, 365
    IF 0 < days <= yearlen BREAK
    days, year := days - yearlen, year + 1
  } REPEAT

  datestr%8 := year/10 REM 10 + '0'
  datestr%9 := year    REM 10 + '0'
 
  // Find month
  mtable := isleap(year) -> leapmonthtab, monthtab

  { IF days <= mtable ! month BREAK
    month := month + 1
  } REPEAT

  mcharbase := month*3 - 2
  FOR j=0 TO 2 DO datestr%(4+j) := mchars % (mcharbase + j)
  days := days - mtable ! (month - 1)
  datestr%1 := days/10 + '0'
  datestr%2 := days REM 10 + '0'

  // Construct time

  hours := mins/60
  mins := mins REM 60

  // treat ticks as unsigned integers
  secs := (ticks>>1) / (tickspersecond>>1)

  timestr%1 := hours/10 + '0'
  timestr%2 := hours REM 10 + '0'
  timestr%4 := mins/10 + '0'
  timestr%5 := mins REM 10 + '0'
  timestr%7 := secs/10 REM 10 + '0'
  timestr%8 := secs REM 10 + '0'

  // Get day of week
    
  dowtemp := VALOF SWITCHON dayofweek INTO
      { CASE 0: RESULTIS "Sunday"
        CASE 1: RESULTIS "Monday"
        CASE 2: RESULTIS "Tuesday"
        CASE 3: RESULTIS "Wednesday"
        CASE 4: RESULTIS "Thursday"
        CASE 5: RESULTIS "Friday"
        CASE 6: RESULTIS "Saturday"
      }

  FOR j = 0 TO dowtemp%0 DO dowstr%j := dowtemp%j

  RESULTIS v
}

AND isleap(year) = year REM 400 = 0 -> TRUE,
                   year REM 100 = 0 -> FALSE,
                   year REM   4 = 0 -> TRUE,
                                       FALSE

/*      result := testbit(bit, flags)

        This function returns the truth of the bit number 'bit' in the
        table of bits pointed to by 'flags'

        The order of the bits in the table is from right to left starting
        with bit 0.
*/

LET testbit(bit, flags) =
 (flags!(bit/bitsperword) & (1<<(bit REM bitsperword))) ~= 0

/*      result := setbit(bit, flags, state)

        This function sets the required bit to 'state'. the result is
        the original truth of the bit.
*/

LET setbit(bit, flags, state) = VALOF
{ LET mask = 1 << (bit REM bitsperword)
  LET word = bit / bitsperword
  LET oldstate = (flags!word & mask) ~= 0
  TEST state THEN flags!word := flags!word | mask
             ELSE flags!word := flags!word & (~mask)
  RESULTIS oldstate
}

AND string.to.number(s) = VALOF
// Return TRUE if OK with value in result2
//        FALSE and result2=0 if s is not a number
// Example strings: 
//   'A'
//  123    -99    +63
//  #377   -#x7FF +#b1011011 
{ LET p, len = 1, s%0
  LET neg, radix = FALSE, 10
  LET ch = ?

  result2 := 0
  UNLESS len RESULTIS FALSE
  ch := capitalch(s%p)
  IF ch = '*'' & len = 3 & s%3 = '*'' DO
  { result2 := s%2
    RESULTIS TRUE
  }

  IF ch = '+' | ch = '-' DO
  { neg := ch = '-'
    IF p = len RESULTIS TRUE
    p := p + 1
    ch := capitalch(s%p)
  }
  IF ch = '#' DO
  { radix := 8
    IF p = len RESULTIS TRUE
    p := p + 1
    ch := capitalch(s%p)
    IF ch = 'O' | ch = 'X' | ch = 'B' DO
    { IF ch = 'X' DO radix := 16
      IF ch = 'B' DO radix := 2
      IF p = len RESULTIS TRUE
      p := p + 1
      ch := capitalch(s%p)
    }
  }
  { LET n = '0' <= ch <= '9' -> ch - '0',
            'A' <= ch <= 'Z' -> ch - 'A' + 10, 1000
    UNLESS n < radix RESULTIS FALSE
    result2 := result2 * radix + n
    p := p + 1
    IF p > len BREAK
    ch := capitalch(s%p)
  } REPEAT

  IF neg DO result2 := -result2
  RESULTIS TRUE
}

AND string.to.dat() = VALOF
{ sawritef("function string.to.dat not implemented (BLIB)*n")
  RESULTIS 0
}

AND level(x) = (@x)!-3

AND longjump(lev, lab) BE
{ LET p = @lev - 3
  p!0, p!1 := lev, lab
}

AND createco(fn, size) = VALOF
{ LET c = getvec(size+6)
  UNLESS c RESULTIS 0
  FOR i = 6 TO size+6 DO c!i := stackword

  // Using P to denote the current stack frame
  // pointer, the following assumptions are made:
  //  P!0, P!1, P!2 contain the return link information
  //  P!3   is the variable fn
  //  P!4   is the variable size
  //  P!5   is the variable c

  // Now make the vector c into a valid BCPL
  // stack frame containg copies of fn, size
  // and c in the same relative positions.
  // Other locations in the new stack frame 
  // are used for other purposes.
  c!0 := c<<2    // resumption point
  c!1 := currco  // parent link
  c!2 := colist  // colist chain
  c!3 := fn      // the main function
  c!4 := size    // the coroutine size
  c!5 := c       // the new coroutine pointer

  colist := c  // insert into the list of coroutines

  changeco(0, c)

  // Execution now continues with the P pointer set to c<<2,
  // and so the vector c becomes the current stack frame.
  // The compiler will have generated code on
  // the assumption that fn and c are the third and fifth
  // words of the stack frame, and, since c!3 and c!5
  // were initialised to fn and c, the following repeated
  // statement will have the effect (naively) expected.
  // Note that the first call of cowait causes a return
  // from createco with result c.

  c := fn(cowait(c)) REPEAT
}

AND deleteco(cptr) = VALOF
{ LET a = @colist

  { LET co = !a
    UNLESS co DO
    { sawritef("BLIB co=%n: cannot deleteco %n -- not found*n",
         currco, cptr)
      abort(112)
      RESULTIS FALSE
    }
    IF co=cptr BREAK
    a := @ co!co_list
  } REPEAT

  IF cptr!co_parent DO
  { sawritef("BLIB co=%n: cannot deleteco %n -- has a parent*n",
       currco, cptr)
    abort(112)
    RESULTIS FALSE
  }

  !a := cptr!co_list      // Remove the coroutine from colist.
  freevec(cptr)           // Free the coroutine stack.
  RESULTIS TRUE
}

AND callco(cptr, a) = VALOF
{ IF cptr!co_parent DO abort(110)
  cptr!co_parent := currco
  RESULTIS changeco(a, cptr)
}

AND resumeco(cptr, a) = VALOF
{ LET parent = currco!co_parent
  currco!co_parent := 0
  IF cptr!co_parent DO abort(111)
  cptr!co_parent := parent
  RESULTIS changeco(a, cptr)
}

AND cowait(a) = VALOF
{ LET parent = currco!co_parent
  currco!co_parent := 0
  RESULTIS changeco(a, parent)
}

AND initco(fn, size, a, b, c, d, e, f, g, h, i, j, k) = VALOF
{ LET cptr = createco(fn, size)
  IF cptr DO callco(cptr, @a)
  RESULTIS cptr
}

/*      res := startco(body, line, stsize)

        The routine 'body' is created as a coroutine with a stacksize 'stsize'
        and 'arg' passed as an argument.  The result is the stackbase of
        the coroutine.
*/

AND startco(body, arg, stsize) = VALOF
{ LET newco = createco(body, stsize)
//sawritef("BLIB: callco(%n,%n)*n", newco, arg)
   IF newco DO callco(newco, arg)
   RESULTIS newco
}

AND stop(code) BE
{ // Return to the CLI with a return code
  returncode := code
  cowait(code)
}

// Get the ith element of vector v of 16-bit unsigned words
AND getword(v, i) = VALOF
{ LET j = i+i
  LET res = v%j + (v%(j+1)<<8)  // Assumes little ender m/c ??????????
  RESULTIS res
}

// Store least sig 16 bits of w in the ith element of vector v of 16-bit words
AND putword(v, i, w) BE    // store 16 bit word
{ LET j = i+i
  v%j, v%(j+1) := w, w>>8  // Assumes little ender m/c  ?????????????
}

AND copystring(from, to) BE
  FOR i = 0 TO from%0 DO to%i := from%i

AND copy.words(from, to, n) BE
  FOR i = 0 TO n-1 DO to!i := from!i

AND clear.words(v, n) BE
  FOR i = 0 TO n-1 DO v!i := 0

AND copy.bytes(fromlen, from, fillch, tolen, to) = VALOF
{ LET n = fromlen
  // from and to are byte addresses!!!!!
  IF n>tolen DO n := tolen
  // This code need checking!!!!!
  FOR i = 0 TO n-1 DO 0%(to+i) := 0%(from+i)
  FOR i = n TO tolen-1 DO 0%(to+i) := fillch
  RESULTIS fromlen-n // Number of non copied characters
}

AND sardch()   =  sys(Sys_sardch)

AND sawrch(ch) BE sys(Sys_sawrch,ch)

AND sawritef(format,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) BE
{ LET nextarg = @a
  LET wch, rch = wrch, rdch
  wrch, rdch := sawrch, sardch
  write.format(format, @nextarg)
  wrch, rdch := wch, rch
}

AND getvec(upb) = VALOF
{ LET v = sys(Sys_getvec, upb)
  RESULTIS v
}

AND freevec(ptr) BE IF ptr UNLESS sys(Sys_freevec, ptr) DO
{ sawritef("BLIB co=%n: freevec failure, ptr=%n*n", currco, ptr)
  abort(999)
}

AND loadseg(name) = sys(Sys_loadseg, name)

AND globin(segl) = sys(Sys_globin, segl)

AND unloadseg(segl) BE sys(Sys_unloadseg, segl)

AND deletefile(name) = sys(Sys_deletefile, name)

AND renamefile(fromname, toname) = sys(Sys_renamefile, fromname, toname)

/* cptr := findpkt(pkt)

This routine searches the pktlist for the specified packet. If found
the list item is dequeued and a pointer to the coroutine is
returned. Zero is returned if the packet is not found in pktlist.
*/

AND findpkt(pkt) = VALOF
{ LET a = @pktlist
//sawritef("BLIB findpkt: task=%n from %n => ", taskid, pkt!pkt_id)//; abort(999)

  // Search pktlist for the relevant item
  { LET p = !a
    UNLESS p DO
    { //sawritef("not found*n")
      RESULTIS 0   // Not found
    }
    IF p!1 = pkt DO
    { //sawritef("%n*n", p)
      !a := !p            // Remove from pktlist
      RESULTIS p!2        // The coroutine
    }
    a := p
  } REPEAT
}

/* res := sndpkt(link, id, act, r1, r2, a1, a2, a3, a4, a5, a6)

This routine is a substitute sendpkt for multi-event tasks.  It can
only be called when running as a non root coroutine of a task.  A
packet-coroutine pair is placed in pktlist before dispatching the
packet (using qpkt) so that when the packet returns to this task this
coroutine can be reactivated.  The globals cis, cos, pvsline and
currentdir are saved and restored by sndpkt.

Multi-event mode is entered by executing

     pktlist, sendpkt := 0, sndpkt

Re-implemented by MR 7/6/02, futher modified MR 7/5/03
*/

AND sndpkt(link, id, act, r1, r2, a1, a2, a3, a4, a5, a6) = VALOF
{ // The following variables form the pktlist node.
  // Functions other than sndpkt may
  // manipulate pktlist so its format must not change.
  LET next,      pkt,     co, ocis, ocos, ocurrentdir =
      pktlist, @link, currco,  cis,  cos,  currentdir


//sawritef("BLIB sndpkt: sending pkt=%n from %n to %n pktlist=%n*n",
//          @link, taskid, id, pktlist)
//abort(1000)

  // Safety check -- sndpkt cannot be called from the root coroutine
  IF currco!co_parent<=0 DO
  { sawritef(
     "BLIB co=%n T%n: can't sndpkt %n(id=%n,type=%n) from root coroutine*n",
       currco, taskid, pkt, id, act)
    abort(999)
  }

  pktlist := @next       // Insert it at the head of pktlist

//sawritef("BLIB:   co=%n: sndpkt %n(id=%n,type=%n) calling cowait*n",
//  currco, pkt, pkt!pkt_id, pkt!pkt_type)

//sawritef(
//   "BLIB: co=%n T%n: sndpkt calling qpkt %n(id=%n,type=%n, arg1=%n,arg2=%n)*n",
//     currco, taskid, pkt, id, act, a1, a2)

  UNLESS qpkt(pkt) DO
  { sawritef("BLIB co=%n: sndpkt -- qpkt failure*n", currco)
    abort(181)
  }

  { LET p = cowait() // Safety check -- we must resume with the
    IF p=pkt BREAK   // expected packet.
    sawritef("BLIB co=%n T=%n: sndpkt: received wrong pkt=%n should be %n*n",
              currco, taskid,  p, pkt)
    abort(182)
  } REPEAT

//sawritef("BLIB:   co=%n: sndpkt %n(res1=%n,res2=%n) resumes*n",
//  currco, pkt, pkt!pkt_res1, pkt!pkt_res2)

  // Restore the saved globals
  cis, cos, currentdir := ocis, ocos, ocurrentdir 

  result2 := r2
  RESULTIS r1
}

// Routine to set the task name. It copies the given name into the
// TCB of the current task.

AND set.process.name(name) BE
{ LET taskname = @rootnode!rtn_crntask!tcb_namebase
  LET len = name%0
  IF len>15 DO len := 15
  taskname%0 := len
  FOR i = 1 TO len DO taskname%i := name%i
}

AND setlogname(logname, logvalue) = VALOF
{ LET a = @rootnode!rtn_envlist

  // First delete current entry if it exists.
  { LET p = !a
    UNLESS p BREAK
    IF compstring(logname, p!1)=0 DO
    { !a := !p
      freevec(p)
      BREAK
    }
    a := p
  } REPEAT

  IF logvalue DO // Insert new entry
  { LET upb1 = logname%0  / bytesperword
    LET upb2 = logvalue%0 / bytesperword
    LET p = getvec(4 + upb1 + upb2) // 3 + upb1+1 + upb2+1 - 1
    LET s1 = p + 3
    LET s2 = s1 + upb1 + 1
    UNLESS p RESULTIS 0
    FOR i = 0 TO upb1 DO s1!i := logname!i
    FOR i = 0 TO upb2 DO s2!i := logvalue!i
    p!1, p!2 := s1, s2
    !p := rootnode!rtn_envlist
    rootnode!rtn_envlist := p
    RESULTIS p
  }

//sawritef("BLIB: not adding %s*n", logname)
  RESULTIS 0
}

AND getlogname(logname) = VALOF
{ LET p = rootnode!rtn_envlist
  WHILE p DO
  { IF compstring(logname, p!1)=0 RESULTIS p!2
    p := !p
  }
  RESULTIS 0
}

AND getremipaddr(scb) = VALOF
{ UNLESS scb!scb_type=scbt_tcp | scb!scb_type=scbt_net DO
  { result2 := 0
    RESULTIS 0
  }
  RESULTIS sendpkt(-1, scb!scb_task, Action_getremipaddr, 0,0, scb)
}

// object making function
AND mkobj(upb, fns, a, b, c, d, e, f, g, h, i, j, k) = VALOF
{ LET obj = getvec(upb)
  UNLESS obj=0 DO
  { !obj := fns
    InitObj#(obj, @a) // Send the InitObj message to the object
  }
  RESULTIS obj
}

AND instrcount(fn, a,b,c,d,e,f,g,h,i,j,k) = VALOF
{ LET res = 0
  LET count = sys(-1, maxint)  // Set count register to maxint
  result2 := fn(a,b,c,d,e,f,g,h,i,j,k)
  res := sys(-1, count)        // Restore previous value
                               // returning the modified count
  RESULTIS maxint - res - 32   // Correct for overhead
}
