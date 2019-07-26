// (c)  Copyright:  Martin Richards  25 May 2016

/*
07/02/18
Added %n.mf and %ng to writef, and corresponding functions
writeflt, writee and readflt.

18/05/16
Added BLIB function memoryfree() to check that the store chain is
valid and return the amount of free and used memory.

11/02/16
Added BLIB function stackfree() to return approximately how much of
the run time stack is currently unused.

21/11/14
Corrected bug in /N qualifier of rdargs and added the function rdargs2.

09/01/14
rdargs rewritten.

29/03/10
Changed the units of time to be msecs instead of the system dependent
ticks. Tickspersecond has been removed and the datstamp format changed to:
datv!0 = days  since 1 Jan 1970
datv!1 = msecs  since midnight
datv!2 = -1 to indicate that the new date and time format is being used.
sys(Sys_delay, msecs) performs a delay in msecs (not ticks).
The cli prompt is now written using
writef(promptstring, cputime, taskid, hours, mins, secs, msecs)
Try the command: prompt "%+%+%z2:%z2:%z2.%z3> "

01/10/07
Modified initco to return a second result in result2

03/07/07
Added codewrch to write extended characters using UTF8 or
GB2312. Added %# substitution item in writef to invoke it. Note that
*xU, *xG, *#hhhh, *##hhhhhhhh and *#dddd escapes have been added to
BCPL string and character constants.

29/6/02
Renamed IOLIB as DLIB (the system Dependent Library). Put system
independent code in BLIB and the rest in DLIB.

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
with corresponding change in cintsys.c and cintpos.c

7/6/1996
Defined mkobj(upb, fns, a, b) for use in object oriented programming.
See bcplprogs/objdemo.b  (args a and b added 30 March 1999).
*/

SECTION "BLIB"

GET "libhdr"

LET stop(code, reason) BE
{ // Return to the CLI with the given return code and reason.
  // It must be called from the command's main coroutine, ie
  // not an inner coroutine.
  result2 := reason
  cowait(code)
}

AND fault(code) BE sawritef("BLIB: fault: code=%n*n", code)

AND clihook(a1) = start(a1)

AND intflag() = sys(Sys_intflag)  // Returns TRUE if user interrupt

AND abort(code) = sys(Sys_quit, code)

AND level(p3) = (@p3)!-3

AND longjump(lev, lab) BE { LET p = @lev - 3; p!0, p!1 := lev, lab }

AND sardch()   = sys(Sys_sardch)

AND sawrch(ch) = sys(Sys_sawrch,ch)

// Set the timeout field
// msecs>0   The stream timeout value in milli-seconds
// msecs=0   No timeout value (the default)
// msecs<0   Only perform non blocking operations on the stream
// On a timeout (on TCP streams)
// act= 0    Repeat the current operation
// act=-1    Make timeout have the effect of EOF
// act=-2    Return timeoutch
AND settimeout(scb, msecs, act) BE
{ scb!scb_timeout := msecs
  scb!scb_timeoutact := act
}

// Just set the timeoutact field
AND settimeoutact(scb, act) BE scb!scb_timeoutact := act

AND setvec(v, n, n0, n1,  n2,  n3,  n4,  n5,  n6,  n7,
                 n8, n9, n10, n11, n12, n13, n14, n15) BE
{ LET p = @n0
  FOR i = 0 TO n-1 DO v!i := p!i
}

AND rdch() = VALOF
// Returns the next byte from the currently selected input stream,
// but ignores CR.
// If the buffer is empty, it calls replenish to attempt to refill it.
// It aborts 186 if no input stream is selected.
{ LET pos = cis!scb_pos // Position of next byte

  UNLESS cis DO abort(186)
  IF pos<cis!scb_end DO { LET ch = cis!scb_buf%pos
                          cis!scb_pos := pos+1
                          IF ch='*c' LOOP // Ignore CR
                          RESULTIS ch
                        }
  // No byte available, so attempt to replenish the buffer
  // If replenish returns FALSE, no chars were placed in the buffer
  // and the reason why not is placed in result2 as follows
  //    result2 = -1    end of file
  //    result2 = -2    timeout
  //    result2 = -3    polling and no characters available.
  //    result2 = code  error code
  UNTIL replenish(cis) DO
  { IF result2=-2 DO
    { LET act = cis!scb_timeoutact // Look at timeout action
      IF act=-2 RESULTIS timeoutch
      IF act=-1 RESULTIS endstreamch
      LOOP  // Try replenishing again
    }
    RESULTIS result2<0 -> result2, endstreamch
  }
  // Successful replenishment so try rdch again
  // There will be at least one character in the buffer
} REPEAT

AND binrdch() = VALOF
// Same as rdch but does not ignore CR.
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
  UNTIL replenish(cis) DO
  { IF result2=-2 DO
    { LET act = cis!scb_timeoutact // Look at timeout action
      IF act=-2 RESULTIS timeoutch
      IF act=-1 RESULTIS endstreamch
      LOOP  // Try replenishing again
    }
    RESULTIS result2<0 -> result2, endstreamch
  }
  // Successful replenishment so try rdch again
  // There will be at least one ch in the buffer
} REPEAT

AND unrdch() = VALOF
// Attempt to step input back by one byte position.
// It returns: TRUE if successful, and
//             FALSE otherwise
// After a call of rdch() it will always be successful at least once.
// It aborts: 186 if not input stream, is selected.
{ LET pos = cis!scb_pos
  UNLESS cis DO abort(186)
  IF pos<=0 RESULTIS FALSE // Cannot UNRDCH past origin.
  cis!scb_pos := pos-1
  RESULTIS TRUE
}

AND wrch(ch) = VALOF
// wrch(ch) writes ch to the current output stream.
// It returns TRUE if successful, FALSE otherwise
// It aborts: 187 if no stream is selected
//            189 on depletion failure.
{ LET pos = cos!scb_pos

  // If the buffer is full try to deplete it.
  IF pos >= cos!scb_bufend DO
  { UNLESS deplete(cos) RESULTIS FALSE
    UNLESS cos!scb_buf  RESULTIS TRUE // Must be writing to NIL:
    pos := cos!scb_pos
  }

  // Pack the character and advance pos.
  cos!scb_buf%pos := ch
  pos := pos+1
  cos!scb_pos := pos
  // Advance end of valid data pointer, if necessary
  IF cos!scb_end < pos DO cos!scb_end := pos
  cos!scb_write := TRUE // Set flag to indicate the buffer has changed.

  UNLESS ch<'*s' & cos!scb_type<0 RESULTIS TRUE // Normal return

  // The stream is interactive and ch is a control character.

  IF ch='*n' DO  wrch('*c')  // Fiddle for Cygwin

  // Call deplete at the end of each interactive line.
  IF ch='*n' | ch='*p' RESULTIS deplete(cos)
  RESULTIS TRUE
}

AND binwrch(ch) = wrch(ch | 256)

AND codewrch(code) BE
{ // This (misleadingly) writes either a Unicode character in
  // UTF-8 format or a code in GB2312 format.
  // A special (negative) value to select the current encoding
  // to be used on the currently selected output stream (cos).

  IF code<0 DO
  { // Set the encoding for the currently selected
    // output stream.
    cos!scb_encoding := code // UTF8 (=-1) or GB2312 (=-2)
    RETURN
  }
  // Select UTF8 unless GB2312 explicitly specified in the SCB.
  TEST cos!scb_encoding=GB2312
  THEN gb2312wrch(code)
  ELSE utf8wrch(code)
}

AND gb2312wrch(code) BE
{ // I believe the encoding is as follows:
  // code = 0 - 127 => code
  // code = xxyy    => <xx + 160> <yy + 160>
  //                   eg 4566 => <45+160> <66+160> or CD E2
  // Note that the row encoding (CD) is written first, followed
  // by the column.
  TEST code<=127
  THEN { wrch(code)
//sawritef(" gb2312: %x4 => %x2*n", code, code)
       }
  ELSE { LET hi = code  /  100 + 160 // Row encoding
         LET lo = code MOD 100 + 160 // Column encoding
         wrch(hi)
         wrch(lo)
//sawritef(" gb2312: %x4 => %x2 %x2*n", code, hi, lo)
       }
}

AND utf8wrch(code) BE
{ // Write a Unicode character in RTF-8 format
  IF code<=#x7F DO
  { wrch(code)                   // 0xxxxxxx
    RETURN
  }
  IF code<=#x7FF DO
  { wrch(#b1100_0000+(code>>6))  // 110xxxxx
    wrch(#x80+( code    &#x3F))  // 10xxxxxx
    RETURN
  }
  IF code<=#xFFFF DO
  { wrch(#b1110_0000+(code>>12)) // 1110xxxx
    wrch(#x80+((code>>6)&#x3F))  // 10xxxxxx
    wrch(#x80+( code    &#x3F))  // 10xxxxxx
    RETURN
  }
  IF code<=#x1F_FFFF DO
  { wrch(#b1111_0000+(code>>18)) // 11110xxx
    wrch(#x80+((code>>12)&#x3F)) // 10xxxxxx
    wrch(#x80+((code>>6)&#x3F))  // 10xxxxxx
    wrch(#x80+( code    &#x3F))  // 10xxxxxx
    RETURN
  }
  IF code<=#x3FF_FFFF DO
  { wrch(#b1111_1000+(code>>24)) // 111110xx
    wrch(#x80+((code>>18)&#x3F)) // 10xxxxxx
    wrch(#x80+((code>>12)&#x3F)) // 10xxxxxx
    wrch(#x80+((code>>6)&#x3F))  // 10xxxxxx
    wrch(#x80+( code    &#x3F))  // 10xxxxxx
    RETURN
  }
  IF code<=#x7FFF_FFFF DO
  { wrch(#b1111_1100+(code>>30)) // 1111110x
    wrch(#x80+((code>>24)&#x3F)) // 10xxxxxx
    wrch(#x80+((code>>18)&#x3F)) // 10xxxxxx
    wrch(#x80+((code>>12)&#x3F)) // 10xxxxxx
    wrch(#x80+((code>> 6)&#x3F)) // 10xxxxxx
    wrch(#x80+( code     &#x3F)) // 10xxxxxx
    RETURN
  }

  // Bad Unicode character
  writef("#%x4#", code)
}

AND readwords(vector, count) = VALOF
// count is the number of words to read.
// It returns the number of complete words copied into vector.
// This may be less than count if the input becomes exhausted
// before count words have been read. A few bytes may have been
// copied into vector after the last completed word if the number
// of bytes in the stream is not a multiple of bytesperword.
{ LET i, lim = 0, count*bytesperword
  // lim is the number of bytes still needed.

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
      cos!scb_write := TRUE  // At least one byte has been written

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

// get_record returns TRUE if successful
// it returns FALSE if eof is encountered before the whole record
// has been read.
// MR 29/7/02: First record of a file has record number 0
AND get_record (vector, recno, scb) = VALOF
{ LET i   = 0              // Position of next byte to put in vector.
  LET len = scb!scb_reclen // Length of the record in bytes.

  IF len<=0 RESULTIS FALSE // Fail, no record length specified.

//sawritef("BLIB co=%n: get_record recno=%n reclen=%n blk=%n pos=%n end=%n*n",
//   currco, recno, scb!scb_reclen, scb!scb_block, scb!scb_pos, scb!scb_end)
  recordpoint(scb, recno)

//sawritef("BLIB: get_record recno=%n reclen=%n pos=%n end=%n*n",
//           recno, scb!scb_reclen, scb!scb_pos, scb!scb_end)


  { // Start of reading loop
    LET pos = scb!scb_pos // Position of next byte
    AND end = scb!scb_end // Position past last byte
    AND buf = scb!scb_buf // Byte buffer -- replenish might change buf

    WHILE pos < end DO    // Copy bytes -- more needed
    { // At least one byte needed
      vector%i := buf%pos
//sawritef("BLIB co=%n: get_record byte=%x2*n", currco, vector%i)
      i, pos := i+1, pos+1
      IF i<len LOOP       // More byte(s) needed
      // Successful completion
      scb!scb_pos := pos
//sawritef("BLIB co=%n: get_record recno=%n len=%n successful*n",
//          currco, recno, len)
      RESULTIS TRUE
    }

    scb!scb_pos := pos

    // No byte available, so attempt to replenish the buffer
    UNLESS replenish(scb) DO
    {
//sawritef("BLIB co=%n: get_record recno=%n len=%n hit eof at %n*n",
//          currco, recno, len, i)
      RESULTIS FALSE  // Failure due to eof, timeout, error etc
    }
    // Successful replenishment so copy some more bytes
    // There will still be at least one byte in the buffer
  } REPEAT
}

// MR 29/7/02: The first record of a file has number 0 (not 1)
// Returns TRUE if successful
// Returns FALSE, otherwise.
AND put_record(vector, recno, scb) = VALOF
{ LET i, len = 0, scb!scb_reclen

  UNLESS scb!scb_id=id_inoutscb DO
  { sawritef("BLIB co=%n: put_record id not inout*n", currco)
    abort(999)
    RESULTIS FALSE
  }

  IF len<=0 RESULTIS FALSE // Error -- no record length

//sawritef("BLIB: put_record recno=%n reclen=%n blk=%n pos=%n end=%n*n",
//           recno, scb!scb_reclen, scb!scb_block, scb!scb_pos, scb!scb_end)
  UNLESS recordpoint(scb, recno) RESULTIS FALSE
//sawritef("BLIB: put_record recno=%n reclen=%n blk=%n pos=%n end=%n*n",
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
//  IF i>0 DO scb!scb_write := TRUE // if at least one byte has been written

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
  // The condition scb!scb_end<0 indicates that the stream is exhausted
  UNLESS scb!scb_end>=0 & wrfn & wrfn(scb) RESULTIS FALSE
  RESULTIS TRUE
}

AND findinput    (string)       =  findstream(string, id_inscb,     0)

AND pathfindinput(string, path) =  findstream(string, id_inscb,  path)

AND findoutput   (string)       =  findstream(string, id_outscb,    0)

AND findinoutput (string)       =  findstream(string, id_inoutscb,  0)

AND findupdate   (string)       =  findstream(string, id_inoutscb,  0)

AND findappend   (string)       =  findstream(string, id_appendscb, 0)

AND selectinput(scb) BE // scb=0 is occasionally used
{ UNLESS scb=0 | scb!scb_id=id_inscb | scb!scb_id=id_inoutscb DO abort(186)
  cis := scb
}

AND selectoutput(scb) BE // scb=0 is occasionally used
{ UNLESS scb=0 |
         scb!scb_id=id_outscb |
         scb!scb_id=id_appendscb |
         scb!scb_id=id_inoutscb DO abort(187)
  cos := scb
}

AND endread() BE endstream(cis)

AND endwrite() BE endstream(cos)

AND endstream(scb) BE TEST scb>0
THEN { LET endfn = scb!scb_endfn
       LET res2 = result2
//sawritef("endstream: task %i2 closing %s*n", taskid, @scb!scb_name)

       // endstream now frees the buffer
       // so endfn no longer has to.
       IF endfn DO endfn(scb)
       IF scb!scb_buf DO { 
//sawritef("endstream: task %n calling freevec(%n)*n", taskid, scb!scb_buf)
         freevec(scb!scb_buf);
         scb!scb_buf := 0
       }
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

AND newpage() BE wrch('*p')

AND writed(n, d) BE writedz(n, d, FALSE, n<0)

AND writez(n, d) BE writedz(n, d, TRUE,  n<0)

AND writedz(n, d, zeroes, neg) BE
{ // n     is the number to output
  // d     is the field width
  // zeroes    =TRUE is leading zeroes are to be output
  //           as zeroes. If FALSE leading zeroes are
  //           replaced by spaces.
  // neg       -TRUE if a minus sign is required.
  LET t = VEC 20
  LET i = 0
  LET k = -n

  IF neg DO { d := d - 1; k := n }

  { t!i := -(k MOD 10)
    k   := k/10
    i   := i + 1
  } REPEATWHILE k

  IF neg & zeroes DO wrch('-')
  FOR j = i+1 TO d DO wrch(zeroes -> '0', '*s')
  IF neg & ~zeroes DO wrch('-')
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

AND writet(s, d) BE
{ writes(s)
  FOR i = 1 TO d-s%0 DO wrch('*s')
}

AND writeu(n, d) BE
{ LET m = (n>>1)/5
  IF m DO { writed(m, d-1); d := 1 }
  writed(n-m*10, d)
}


/*
The following routines provide an extended version of writef.
They support the following extra substitution items:

        1. %F    - Takes next argument as a writef format string and
                 calls writef recursively using the remaining arguments.
                 The argument pointer is positioned to the next available
                 argument on return.

        2. %n.mF - eg %8.3f
                 Output a floating point number in a field width of n
                 with m digits after the decimal point.

        3. %M    - The next argument is taken as a message number and processed
                 as for %F above. The message format string is looked up by
                 get_text(messno, str, upb) where str is a vector local to
                 writef to hold the message string. This is provided to easy
                 the generation of messages in different languages.

        4. %+    - The argument pointer is incremented by 1.

        5. %-    - The argument pointer is decremented by 1.

        6. %P    - Plural formation. The singular form is use if and only if
                 the next argument is one. So that the argument can be used
                 twice it is normal to preceed or follow the %P item with %-.
                 There are two forms as follows:

                 a. %Pc  - The character c is output if the the next argument
                         not one.

                 b. %P\singular\plural\  - The appropriate text is printed,
                         skipping the other. The '\' chars are not printed.

Example: FOR count = 0 TO 2 DO
            writef("There %p\is\are\ %-%n thing%-%ps.*n", count)
outputs:
         There are 0 things.
         There is 1 thing.
         There are 2 things.

        6. %nOp  eg %12i as an alternative to %iB
                 where n is a decimal number and Op is a format letter
                 expecting a field width. If n is given it specifies the
                 field width otherwise it is specified, as before, by the
                 single character (0-9, A-Z) following Op.

        7. %n.md  eg %8.2d
                  print a fixed point scaled decimal number in a field
                  width of n with m digits after the decimal point. For
                  example writef("%8.2d", 1234567) would output: 12345.67
                  and     writef("%8.0d", 1234567) would output:  1234567

        8. %#     Write the next argument using codewrch, ie convert the
                  next argument to UTF-8 format.
*/

// The following version of writef is new -- MR 21/1/04

// get_textblib and get_text have the same global variable number
AND get_textblib(n, str, upb) = VALOF  // Default definition of get_text
                                       // This is normally overridden 
                                       // by get_text, defined elsewhere.
{ LET s = "<mess:%-%n>"
  IF upb>s%0 DO upb := s%0
  str%0 := upb
  FOR i = 1 TO upb DO str%i := s%i
  RESULTIS str
}

AND writef(format,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) BE
{ LET nextarg = @a
  write_format(format, @nextarg)
}

AND sawritef(format,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) BE
{ LET nextarg = @a
  LET wch, rch = wrch, rdch
  wrch, rdch := sawrch, sardch
  write_format(format, @nextarg)
  wrch, rdch := wch, rch
}

AND write_format(format, lvnextarg) BE
{ // writef and sawritef must preserve result2
  LET res2 = result2

  UNLESS 0 < format < rootnode!rtn_memsize DO format := "##Bad format##"

  FOR p = 1 TO format%0 DO
  { LET k, type, f, n, m, arg = format%p, ?, ?, ?, ?, ?
    LET widthgiven = FALSE
    UNLESS k='%' DO { wrch(k); LOOP }

    // Deal with a substitution item
    p := p + 1
    type, arg, n, m := format%p, !!lvnextarg, 0, 0

sw: SWITCHON capitalch(type) INTO
    { DEFAULT:    wrch(type)
                  LOOP

      CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
      CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
                  { n := 10*n + type - '0'
                    p := p+1
                    type := format%p
                    widthgiven := TRUE
                  } REPEATWHILE '0'<=type<='9'
                  IF type='.' DO
                  { p := p+1
                    type := format%p
                    WHILE '0'<=type<='9' DO
                    { m := 10*m + type - '0'
                      p := p+1
                      type := format%p
                    }
                  }
                  GOTO sw

      CASE 'D':   IF m DO
                  { // Deal with %n.mD eg %6.2d
                    // Writes a scaled number of the form nnn.nn
                    LET scale = 1
                    FOR i = 1 TO m DO scale := scale * 10
                    writedz(arg/scale, n-1-m, FALSE, arg<0)
                    wrch('.')
                    writez( ABS arg MOD scale, m)
                    !lvnextarg := !lvnextarg + 1
                    LOOP
                  }
                  f := writed;    GOTO getarg


      CASE 'S':   f := writes;    GOTO noargs
      CASE 'T':   f := writet;    GOTO getarg
      CASE 'C':   f := wrch;      GOTO noargs
      CASE '#':   f := codewrch;  GOTO noargs
      CASE 'O':   f := writeoct;  GOTO getarg
      CASE 'X':   f := writehex;  GOTO getarg
      CASE 'I':   f := writed;    GOTO getarg
      CASE 'N':   f := writen;    GOTO noargs
      CASE 'U':   f := writeu;    GOTO getarg
      CASE 'Z':   f := writez;    GOTO getarg
      CASE 'B':   f := writebin;  GOTO getarg

    getarg:       UNLESS widthgiven DO
                  { p := p + 1
                    n := capitalch(format%p)
                    n := '0' <= n <= '9' -> n - '0', 10 + n - 'A'
                  }

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
                  UNLESS get_text(arg, buf, 256/bytesperword) DO
                    buf := "<<mess:%-%n>>"  // No message text
                  write_format(buf, lvnextarg)
                  LOOP
                }

      CASE 'E':   !lvnextarg := !lvnextarg + 1
                  writee(arg, n, m)          // Deal with eg %13.3e
                  LOOP

      CASE 'F':   !lvnextarg := !lvnextarg + 1
                  TEST n
                  THEN writeflt(arg, n, m)          // Deal with eg %8.3f
                  ELSE write_format(arg, lvnextarg) // Deal with %f
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

AND checkpos(currch) BE
{ LET pos = cis!scb_pos
  LET ch = (cis!scb_buf)%pos
  sawritef("current ch='%c' pos=%n => '%c'*n", currch, pos, ch)
}

AND readflt() = VALOF
{ // Read and return a floating point number from the
  // currently selected input stream.
  // Syntax: spaces [+|-] [digits] [. digits] [e [+|-] digits]
  // where space is zero or more white space chacters
  // [+|-] is an optional sign
  // digits is one or more decimal digits. There must be at least
  // one digit before or after the decimal point.
  // Underscores can be embedded in digit sequences.
  // If successful, the result is the 32-bit or 64-bit representation
  // of the floating point number, and result2 is zero.
  // On failure the result is zero and result2 to -1.

  // The strategy is to read up to 9 digits following white space
  // sign and leading zeroes. Then ignore any additional digits.
  // Count the digits following the decimal point and deal with the
  // E exponent if given. Return the floating point number based on
  // the integer significand and the computed exponent using
  // sys(Sys_flt, fl_mk, val, exponent) correcting the sign if
  // necessary
  LET ch = rdch()
  LET dcount = 0    // Number of digits before and after the decimal
                    // point. This must be >0 for a valid number.
  LET ecount = 0    // Number of ignored digits. Ignored digits are
                    // treated as if they were zeroes.
  LET dpos = -1     // =-1 or is equal to dcount when a decimal point
                    // was encountered. If dpos=-1 there is no decimal
                    // point, otherwise the number of decimal digits
                    // after the decimal point is dcount-dpos.
  LET val, exponent = 0, 0
  LET neg = FALSE
  LET ignoredigits = FALSE // If ignoredigits=TRUE, val will contain
                           // more significant decimal digits than
                           // can be represented by the resulting
                           // floating point number.

//sawritef("readflt: entered*n"); checkpos(ch)

  // Ignore leading white space
  WHILE ch='*s' | ch='*t' | ch='*n' DO
  { ch := rdch()
//sawritef("readflt: reading white space*n"); checkpos(ch)
  }

  IF ch='-' | ch='+' DO
  { IF ch='-' DO neg := TRUE
    ch := rdch()
//sawritef("readflt: dealing with + or -*n"); checkpos(ch)
  }

  UNLESS '0'<=ch<='9' | ch='.' GOTO fail

  // Read the significand
  WHILE '0'<=ch<='9' | ch='_' | ch='.' DO
  {
//sawritef("readflt: dealing with significand*n"); checkpos(ch)
    SWITCHON ch INTO
    { DEFAULT: BREAK

      CASE '0':CASE '1': CASE '2': CASE '3': CASE '4': 
      CASE '5': CASE '6': CASE '7': CASE '8': CASE '9':
      { LET digval = ch-'0'
        dcount := dcount+1
        UNLESS ignoredigits DO
          IF val > maxint/10 | 10*val > maxint-digval DO
            ignoredigits := TRUE

        // Do not use this digit if the result would be too large
        // for a BCPL integer.
        TEST ignoredigits
        THEN ecount := ecount+1 // Count of ignored digits
        ELSE val := 10*val + digval

//sawritef("readflt: was digit '%c'*n", ch)
        ENDCASE
      }

      CASE '_': // Ignore underlines
//sawritef("readflt: was underline*n")
        ENDCASE

      CASE '.': 
//sawritef("readflt: was dot*n")
        UNLESS dpos<0 GOTO fail // Must less than one decimal point
        dpos := dcount
        ENDCASE
    }
    ch := rdch()
  }

//sawritef("readflt: Exited from significand*n"); checkpos(ch)

  // There must be at least one digit before or after the decimal point.
  IF dcount=0 DO { unrdch(); GOTO fail }

  IF dpos<0 DO dpos := dcount

  // Check whether an exponent is given.
  IF ch='e' | ch='E' DO
  { LET expneg = FALSE
//sawritef("readflt: reading exponent*n"); checkpos(ch)
    ch := rdch()
    IF ch='-' | ch='+' DO
    { //sawritef("readflt: reading exponent sign*n"); checkpos(ch)
      IF ch='-' DO expneg := TRUE
      ch := rdch()
    }
    // Read the exponent
    WHILE '0'<=ch<='9' | ch='_' DO
    { //sawritef("readflt: reading exponent*n"); checkpos(ch)
      UNLESS ch='_' DO exponent := 10*exponent + ch-'0'
      ch := rdch()
//sawritef("readflt: ch=%i3 '%c'*n", ch, ch)
    }
    // Correct its sign if necessary.
    IF expneg DO exponent := -exponent
  }

  // Unread the terminating character.
//sawritef("readflt: about to call unrdch before returning*n"); checkpos(ch)
//writef("Terminating ch was %n*n", ch)
  unrdch()
//writef("*n after calling unrdch() rdch() => %n*n", rdch())
//  unrdch()
//sawritef("readflt: after calling unrdch*n"); checkpos(ch)

  // exponent is the specified exponent but this must be corrected
  // by adding ecount, equivalent to multiplying val by 10^ecount.
  exponent := exponent + ecount

  // and if dpos>=0 there were (dcount-dval) digits after the decimal
  // point, so this must be subtracted from exponent, equivalent to
  // dividing by 10^(dcount-dpos).
  IF dpos>=0 DO
    exponent := exponent - dcount + dpos

succ:
  // Convert val x 10^exponent to a floating point number of the
  // current BCPL word length.
  val := sys(Sys_flt, fl_mk, val, exponent)
  IF neg DO val := sys(Sys_flt, fl_neg, val)
//sawritef("readflt: return result %13.6f*n", val); checkpos(ch)
  result2 :=  0  // Successful return
  RESULTIS val

fail:
  result2 := -1
  RESULTIS 0
}

AND writeflt(x, w, p) BE
{ // Write a floating point number x in a field width w.
  // p is the number of digits after the decimal point.
  // p will be forced to be >= 1
  // w will be forced to be >= p+3 to allow for a possible sign, 
  //   one digit before the decimal point and the decimal point.
  LET val = sys(Sys_flt, fl_unmk, x) // Upto 18 digits in val
  LET e = result2                    // x = val times 10^e
  // val has upto 18 digits on 64 bit BCPL systems
  //     and upto  9 digits on 32 bit BCPL systems
  // If val=0 the number is 0.0
  // otherwise val is an integer containing upto 18 (or 9) decimal digits
  // Its most and least significant digits will both non zero.
  LET digv = VEC 18 // will hold upto 18+1 extracted decimal digits.
                    // The +1 is because rounding may add another digit.
  LET n = ?  // digv!0,.., digv!n will hold the extracted decimal digits.
             // Positions outside the range 0 to p representing zeroes,
             // so we can thinks of the given digits to be extended by
             // zeroes in both directions.
             // The digit at position 0 represents the value digv!0 x 10^e
  LET q = ?  // The position in digv of the first digit after the
             // decimal point. It value will be -e.
  LET r = ?  // The position of the p^th digit after the decimal point,
             // ie the last digit of the fraction to be output. It value
             // will be q-p.
  LET s = ?  // The position corresponding to the sign ('-' or ' ') to
             // the left of the most significant digit.
  LET t = ?  // The position of the first character to output which will
             // be either a sign or padding space.

  LET neg = FALSE
  IF val<0 DO neg, val := TRUE, -val // Ensure that val is >=0
//writef("*nval=%n e=%n neg=%n*n", val, e, neg)
  IF val=0 DO e := 0 // The number was 0.0

  IF p<1 DO p := 1
  IF w<4 DO w := 4 // Minimum width number is: Sd.d

  // Extract the decimal digits into digv!0,..., digv!n
  n := -1

  { n := n+1
    digv!n := val MOD 10  // Extract the next digit
    val    := val  /  10
  } REPEATWHILE val
  // digv!0 is the least significant digit and
  // digv!n is the most significant digit.
  // n is >= 0

  // val is >= 0 and its digits are held in positions 0 to n
  // of digv. Position zero holds the least significant digit
  // corresponding to digv!0 x 10^e. Digits at positons outside
  // the range 0 to n are treated as zeroes.
  // We now set the values of q,r,s and t. As an example,
  // If val=1275, e=-2, w=10 and p=4 the settings are as follows:

  //       0     3           subscripts of digv
  // 0 0 0 5 7 2 1 S padding
  // |         |   |     t   position of first character
  // |         |   s         position of sign
  // |        .q=-e          units and decimal point position
  // r                       position of p^th fractional digit

//newline()
//writef("w=%n n=%n [", w, n)
//FOR j = 0 TO n DO writef(" %n", digv!j)
//writef(" ] e=%n*n", e)

  q := -e  // The position of the digit to the just left
           // of the decimal point. If e=-2 q will be 2
  // Position 0 corresponds to value digv!0 x 10^e

  r :=  q - p  // Position r holds the p^th digit after the
               // decimal point.

  // Round the digits away from zero by adding 5 to the digit
  // at position r-1, if necessary.

  IF 1 <= r <= n+1 & digv!(r-1)>=5 DO
  { LET carry = 1
    LET i = r
    WHILE carry & i<=n DO
    { LET d = digv!i + carry
      digv!i := d MOD 10
      carry  := d  /  10
      i := i+1
    }
    IF carry DO
    { n := n+1      // We are rounding eg  99999 to
      digv!n := 1   //                    100000 
    }
  }

//writef("*nAfter rounding*n")
//writef("w=%n n=%n q=%n [", w, n, q)
//FOR j = 0 TO n DO writef(" %n", digv!j)
//writef(" ]*n")

  s := q
  IF s < n DO s := n
  s := s+1
  // s is the position of the sign character.

  // Set t to the position of the first character to output
  // including padding. The decimal point occurs between
  // positions q and q-1, so t-r+1 must be w-1. This gives:

  t := r + w - 2

  // The first character to output is the sign or a padding space
  // to the left of the sign.
  IF t < s DO t := s

//  writef("w=%n n=%n q=%n [", w, n, q)
//  FOR j = 0 TO n DO writef(" %n", digv!j)
//  writef(" ]*n")

//newline()
//writef("w=%n p=%n n=%n t=%n s=%n q=%n r=%n*n", w, p, n, t, s, q, r) 
//newline()

  // If the number only has zeroes neg should be FALSE
  IF neg DO
  { LET allzero = TRUE
    FOR j = r TO q IF 0<=j<=n & digv!j DO { allzero := FALSE; BREAK }
    IF allzero DO neg := FALSE
  }

  // Write out the characters of the number.
  FOR i = t TO r BY -1 DO
  { IF i > s DO { wrch(' '); LOOP } // A padding space
    IF i = s DO
    { TEST neg THEN wrch('-')
               ELSE wrch(' ')
      LOOP
    }
    // Output a decimal digit
    TEST 0 <= i <= n THEN wrch(digv!i + '0')
                     ELSE wrch('0')
    // Conditionally output the decimal point
    IF i = q DO wrch('.') 
  }
}

AND writee(x, w, p) BE
{ // Write a floating point number x in exponential form in a
  // field width of w.
  // p number of digits after the decimal point.
  // p will be forced to be >=1
  // w will be forced to be >= p+8 to allow for the sign,
  //   one digit before the decimal point, the ceimal point,
  //   one digit after the decimal point and the exponent: eSdd. 
  LET val = sys(Sys_flt, fl_unmk, x) // Upto 18 digits in val
  LET e = result2                    // x = val times 10^e
  // val has upto 18 digits on 64 bit BCPL systems
  //     and upto  9 digits on 32 bit BCPL systems
  // If val=0 the number is 0.0
  // otherwise val is an integer containing upto 18 (or 9) decimal digits
  // Its most and least significant digits will both non zero.
  LET digv = VEC 18 // will hold upto 19 extracted decimal digits.
  LET n = ?  // digv!0,.., digv!n will hold the extracted decimal digits.
             // Positions outside the range 0 to p representing zeroes,
             // so we can thinks of the given digits to be extended by
             // zeroes in both directions.
             // The digit at position 0 represents the value digv!0 x 10^e
  LET q = ?  // The position the units digit. It value will be -e.
  LET r = ?  // The position of the p^th digit after the decimal point,
             // ie the last digit of the fraction to be output. It value
             // will be q-p.
  LET s = ?  // The position corresponding to the sign ('-' or ' ') to
             // the left of the most significant digit.
  LET t = ?  // The position of the first character to output which may
             // be a padding space.

  LET neg = FALSE
  IF val<0 DO neg, val := TRUE, -val // Ensure that val is >=0
//writef("*nval=%n e=%n neg=%n*n", val, e, neg)

  IF p<1 DO p := 1
  IF w<8 DO w := 8 // Minimum width: Sd.deSdd

  // Extract the decimal digits into digv!0,..., digv!n
  n := -1

  { n := n+1
    digv!n := val MOD 10  // Extract the next digit
    val    := val  /  10
  } REPEATWHILE val
  // digv!0 is the least significant digit and
  // digv!n is the most significant digit.
  // n is >= 0

  // val is >= 0 and its digits are held in positions 0 to n
  // of digv. Position zero holds the least significant digit
  // corresponding to digv!0 x 10^e. Digits at positions outside
  // the range 0 to n are treated as zeroes.
  // We now set the values of q,r,s and t. As an example,
  // If val=1275, e=-2, w=10 and p=4 the settings are as follows:

  //       0     3           subscripts of digv
  //     0 5 7 2 1 S padding
  //     |       | |     t   position of first character
  //     |       | s         position of sign
  //     |      .q=n         units and decimal point position
  //     r=q-p               position of p^th fractional digit

//newline()
//writef("w=%n n=%n [", w, n)
//FOR j = 0 TO n DO writef(" %n", digv!j)
//writef(" ] e=%n*n", e)

  q := n   // The position of the digit to the just left
           // of the decimal point. If e=-2 q will be 2
  // Position 0 corresponds to value digv!0 x 10^e
  e := e+n     // Correct e

  r :=  q - p  // Position r holds the p^th digit after the
               // decimal point.

  // Round the digits away from zero by adding 5 to the digit
  // at position r-1, if necessary.

  IF 1 <= r <= n+1 & digv!(r-1)>=5 DO
  { LET carry = 1
    LET i = r
    WHILE carry & i<=n DO
    { LET d = digv!i + carry
      digv!i := d MOD 10
      carry  := d  /  10
      i := i+1
    }
    IF carry DO
    { n := n+1      // We are rounding eg  99999 to
      digv!n := 1   //                    100000 
    }
  }
//writef("*nAfter rounding*n")
//writef("w=%n n=%n q=%n [", w, n, q)
//FOR j = 0 TO n DO writef(" %n", digv!j)
//writef(" ]*n")
//newline()
 
  s := q
  IF s < n DO s := n
  s := s+1
  // s is the position of the sign character.

  // Set t to the position of the first character to output
  // including padding. The decimal point occurs between
  // positions q and q-1 and there is an exponent eSdd, so
  // t-r+1 must be w-5. This gives:

  t := r + w - 6

  // If w was too small to include the sign, one digit before
  // the decimal point, the decimal point and p digits after the
  // decimal point, set t to the position of the sign character.
  IF t < n+1 DO t := n+1


  //writef("w=%n n=%n q=%n [", w, n, q)
  //FOR j = 0 TO n DO writef(" %n", digv!j)
  //writef(" ]*n")
//newline()
//writef("w=%n p=%n n=%n t=%n s=%n q=%n r=%n*n", w, p, n, t, s, q, r) 

  // If the number only has zeroes neg should be FALSE
  IF neg DO
  { LET allzero = TRUE
    FOR j = r TO q IF 0<=j<=n & digv!j DO { allzero := FALSE; BREAK }
    IF allzero DO neg := FALSE
  }

  // Write out the characters of the number.
  FOR i = t TO r BY -1 DO
  { IF i > s DO { wrch(' '); LOOP } // A padding space
    IF i = s DO
    { TEST neg THEN wrch('-')
               ELSE wrch(' ')
      LOOP
    }
    // Output a decimal digit
    TEST 0 <= i <= n THEN wrch(digv!i + '0')
                     ELSE wrch('0')
    // Conditionally output the decimal point
    IF i = q DO wrch('.') 
  }

  // Output the exponent
  TEST e<0 THEN writef("e-%z2", -e)
           ELSE writef("e+%z2",  e)
}


LET randno(upb) = VALOF  // Return a random number in the range 1 to upb
{ randseed := randseed*2147001325 + 715136305
  // randseed cycles through all 2^32 possible values.
  RESULTIS (ABS(randseed/3)) MOD upb + 1
}

AND setseed(newseed) = VALOF // Added by MR 20/01/2000
{ LET oldseed = randseed
  randseed := newseed
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

AND str2numb(s) = VALOF // Deprecated
{ LET a = 0
  FOR i = 1 TO s%0 DO { LET dig = s%i - '0'
                        IF 0<=dig<=9 DO a := 10*a + dig
                      }
  RESULTIS s%1='-' -> -a, a
}

AND getkeylen(keys, len, i, keyword) = VALOF
{ // keys  is the rdargs format string
  // len   bytes in keys go from keys%1 to keys%len. len may be greater
  //       255.
  // i     is the number counting from zero of the argument
  //       specified by keys whose first keyword is to be
  //       copied into keyword.

  LET p = 1        // Position in keys string
  LET n = 0        // For length of key word

  // Set p to start of the keyword for argument i
  UNTIL p>len DO
  { UNLESS i BREAK
    IF keys%p=',' DO i := i-1
    p := p+1
  }

  // Copy the key word (ignoring newlines) into keyword
  WHILE p <= len DO
  { LET ch = keys%p
    IF ch='/' | ch='=' | ch=',' BREAK
    UNLESS ch='*n' DO
    { n := n + 1
      keyword%n := keys%p
    }
    p := p + 1
  }

  keyword%0 := n
  RESULTIS keyword
}

/*
rdargs provides the programmer with the facility to read
arguments from the currently selected input and store them in the
given vector.

The possible key qualifiers are:

  /a    this argument is required
  /k    argument requires the keyword
  /s    argument is a switch
  /n    argument has to be a number
  /p    prompt will be displayed
*/  

AND rdargs(keys, argv, upb) = rdargslen(keys, keys%0, argv, upb)

AND rdargs2(keys1, keys2, argv, upb) = VALOF
{ LET len = 0
  LET keys = VEC 510/bytesperword
  FOR i = 1 TO keys1%0 DO { len := len+1; keys%len := keys1%i }
  FOR i = 1 TO keys2%0 DO { len := len+1; keys%len := keys2%i }
//sawritef("*nkeys: ")
//FOR i = 1 TO len DO sawrch(keys%i)
//sawrch('*n')
//abort(2222)
  RESULTIS rdargslen(keys, len, argv, upb)
}

AND rdargslen(keys, len, argv, upb) = VALOF
{ MANIFEST
  { a_bit =  1            // /A    must provide argument
    k_bit =  2            // /K    must use keyword
    s_bit =  4            // /S    switch argument
    n_bit =  8            // /N    number argument
    p_bit = 16            // /P    prompt if needed
    d_bit = 32            // argument defined bit
  }

  LET w        = 0       // w is a moving pointer into argv
  LET argmax   = 0
  LET errflag  = FALSE   // Set to TRUE when an error is encountered
  LET keyword  = VEC 30
  AND argtype  = VEC 127 // Space for argument qualifier bits

  // A typical key string is: "FROM=DATA/A,TO/K/P,VAL/K/N,T=TRACE/S"
//sawritef("argv %n to %n  upb=%n*n", argv, argv+upb, upb)
  clear_words(argv, upb+1)
  clear_words(argtype, 128)

  // Fill in the argument qualifier bits
  FOR p = 1 TO len DO
  { LET kch = keys%p

    IF kch = '/' SWITCHON capitalch(keys%(p+1)) INTO
    { DEFAULT: // Bad qualifier
               GOTO err

      CASE 'A': argtype!argmax := argtype!argmax | a_bit; ENDCASE
      CASE 'K': argtype!argmax := argtype!argmax | k_bit; ENDCASE
      CASE 'S': argtype!argmax := argtype!argmax | s_bit; ENDCASE
      CASE 'N': argtype!argmax := argtype!argmax | n_bit; ENDCASE
      CASE 'P': argtype!argmax := argtype!argmax | p_bit; ENDCASE
    }

    IF kch = ',' DO
    { argmax := argmax+1
      IF argmax>127 DO
      { sawritef("Error: rdargs format specified too many arguments*n")
        RESULTIS 0
      }
    }
  }

//sawritef("argmax = %n*n", argmax)
//abort(3333)
  // Check that no argument has both /S and /N set.
  FOR i = 0 TO argmax DO
    IF (argtype!i & (s_bit|n_bit)) = (s_bit|n_bit) GOTO err

//  FOR i = 0 TO argmax DO sawritef("%i3: %b6*n", i, argtype!i)
//sawritef("*n")

  w := argv + argmax + 1 // First free position in argv

  { // Main loop
    LET argno = -1
    LET wupb = upb - (w - argv) // Number of words remaining in argv
    LET itemtype = rditem(w, wupb)

    clear_words(keyword, 31) // Clear keyword!0 to keyword!30

    SWITCHON itemtype INTO
    { DEFAULT: // Unknown item type
               GOTO err

      CASE 0:  // endstreamch
      CASE 3:  // newline
      CASE 4:  // semicolon
// These item types mark the end of the argument list, but there may
// still be prompted input from the user.

               FOR i = 0 TO argmax DO
               { LET type = argtype!i
//sawritef("%i3: %b6*n", i, type)
//abort(4444)
                 UNLESS (argtype!i & (p_bit|d_bit))=p_bit &
                        cis!scb_type = scbt_console       &
                        cos!scb_type = scbt_console       LOOP

                 // Unset argument found with /P qualifier and both
                 // input and output are connected to a terminal so
                 // write a suitable prompt.
                 writes(getkeylen(keys, len, i, keyword))
                 UNLESS (argtype!i & s_bit) = 0 DO writes(" (yes/no)")
                 writes(" > ")
                 deplete(cos)
//sawritef("*nw=%n wupb=%n*n", w, wupb)
//abort(8888)
                 itemtype := rditem(w, wupb)
//abort(9999)

                 SWITCHON itemtype INTO
                 { CASE 0: // endstreamch
                           ENDCASE

                   CASE 1: // Unquoted item
//abort(7777)
                           IF (type & s_bit) ~= 0 DO
                           { argv!i := FALSE
                             IF compstring(w, "yes") = 0 |
                                compstring(w, "y") = 0 DO argv!i := TRUE
                             argtype!i := argtype!i | d_bit
                             GOTO skip
                           }

                           IF (type & n_bit) ~= 0 DO
                           { argv!i := w   // numeric
                             UNLESS string_to_number(w) GOTO err
                             !w := result2
                             argv!i := w
                             argtype!i := argtype!i | d_bit
                             w  := w + 1
                             wupb := wupb-1
                             GOTO skip
                           }

                   CASE 2: // Quoted item
                           // or unquoted item with neither /S or /N
//abort(5555)
                           argv!i := w
                           argtype!i := argtype!i | d_bit
                           w := w + w%0/bytesperword + 1
                           wupb := upb - (w - argv)     
//sawritef("%i4: w=%i6  %b6  wupb=%n*n", i, w, argtype!i, wupb)
//abort(6666)
skip:
                           unrdch()
                           { LET ch = rdch()
                             IF ch='*n' | ch=';' | ch=endstreamch BREAK
                           } REPEAT
                           LOOP

                   CASE 3: // newline
                           // Do not set this argument.
                           LOOP

                   CASE 4: // semicolon
                           ENDCASE

                   DEFAULT:GOTO err
                 }
               } // End of for-loop

               // Before returning, check that all the required
               // arguments have been set.
               FOR i = 0 TO argmax DO
                 IF (argtype!i & (a_bit|d_bit))=a_bit GOTO err

               result2 := 0
               RESULTIS w // Point to first unused word in argv

      CASE 1:  // Unquoted item

               argno := findarglen(keys, len, w)

               TEST argno >= 0
               THEN { // Item matches a keyword

                      // Error is argument already defined
                      IF (argtype!argno & d_bit) ~= 0 GOTO err

                      // Check for /S qualifier
                      IF (argtype!argno & s_bit) ~= 0 DO
                      { argv!argno := -1 // Set the switch argument
                        argtype!argno := argtype!argno | d_bit
                        LOOP
                      }

                      // Read the argument value
                      { LET item = rditem(w, wupb)

                        IF item = 5 DO // Skip optional '='
                          item := rditem(w, wupb)

                        // Check for suitable value
                        UNLESS item=1 | item=2 GOTO err
                      }
                    }

               ELSE TEST rdch() = '*n' & compstring("?", w) = 0
                    THEN { FOR i = 1 TO len DO wrch(keys%i)
                           writes(": ")
                           deplete(cos) // MR 13/1/03
                           ENDCASE
                         }
                    ELSE unrdch()

// Deliberate missing 'ENDCASE'

      CASE 2: // The item was either quoted or
              // was unquoted but did not match a key word.
              // So it is a positional argument.
              // Find the first unset argument no having /K or /S
              IF argno < 0 FOR i = 0 TO argmax DO
                IF (argtype!i & (d_bit|k_bit|s_bit)) = 0 DO
                { argno := i
                  BREAK
                }

              // If the argument did not match a keyword and
              // there are no more positional arguments left
              // indicate a error.
              UNLESS argno>=0 GOTO err

              // Error if this argument is already set
              IF (argtype!argno & d_bit) ~= 0 GOTO err

              IF (argtype!argno & n_bit) ~= 0 DO
              { UNLESS string_to_number(w) GOTO err
                !w := result2
                argv!argno := w
                argtype!argno := argtype!argno | d_bit
                w  := w + 1
                LOOP
              }

              // Store an ordinary or quoted argument value
              argv!argno := w
              argtype!argno := argtype!argno | d_bit
              w := w + w%0/bytesperword + 1
              LOOP

    } // End of main switch
  } REPEAT

err: // An error was detected so skip to the end of the line.
  { LET ch = ?
    unrdch()
    ch := rdch() REPEATUNTIL ch='*n' |
                             ch=';'  |
                             ch=endstreamch
    result2 := 120  // Bad argument format or bad arguments
    RESULTIS 0  // Error result
  }
}

// Read an item from current input stream

// returns -1    error, input too long or unmatched quote
//          0    endstreamch
//          1    unquoted item
//          2    quoted item
//          3    *n
//          4    ;
//          5    =

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
//sawritef("rditem: clearing from %n to %n*n", v, v+upb)
  FOR i = 0 TO upb DO v!i := 0

//sawritef("*nrditem first ch = '%c'*n", ch)

  // Skip over white space.
  WHILE ch='*s' | ch='*t' | ch='*c' DO ch := rdch() 

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

AND findarg(keys, w) = findarglen(keys, keys%0, w)

AND findarglen(keys, len, w) = VALOF
{ MANIFEST { matching = 0; skipping = 1 }
  LET state, wp, argno = matching, 0, 0
  FOR i = 1 TO len DO
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
  c!0 := c<<B2Wsh // resumption point
  c!1 := currco   // parent link
  c!2 := colist   // colist chain
  c!3 := fn       // the main function
  c!4 := size     // the coroutine size
  c!5 := c        // the new coroutine pointer

  colist := c  // insert into the list of coroutines

  changeco(0, c)

  // Execution now continues with the P pointer set to c<<B2Wsh,
  // and so  the vector c becomes the current stack frame.
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
  result2 := 0
  IF cptr DO result2 := callco(cptr, @a)
  RESULTIS cptr
}

/*      res := startco(bodyfn, arg, stsize)

        The routine 'bodyfn' is created as a coroutine with a stacksize 'stsize'
        and 'arg' passed as an argument.  The result is the stackbase of
        the new coroutine.
*/

AND startco(bodyfn, arg, stsize) = VALOF
{ LET newco = createco(bodyfn, stsize)
//sawritef("BLIB: callco(%n,%n)*n", newco, arg)
   IF newco DO callco(newco, arg)
   RESULTIS newco
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
  LET count = sys(Sys_setcount, maxint)  // Set count register to maxint
  result2 := fn(a,b,c,d,e,f,g,h,i,j,k)
  res := sys(Sys_setcount, count)        // Restore previous value
                               // returning the modified count
  RESULTIS maxint - res - 32   // Correct for overhead
}

AND datstring(v) = VALOF
{ LET datv = VEC 2
  datstamp(datv)
  dat_to_strings(datv, v)
  RESULTIS v
}

AND dat_to_strings(datv, v) = VALOF

// Returns v containing 3 strings representing the
// time and date given in datv, where
// datv!0 = days since 1 Jan 1970
// datv!1 = msecs since midnight
// datv!2 = -1
// or old format
// datv!0 = days since 1 Jan 1978
// datv!1 = mins since midnight
// datv!2 = ticks since start of current minute

// On return,
// v    contains a the date in the form DD-MMM-YYYY,
// v+5  contains the time in the format HH:MM:SS, and
// v+10 contains the day of the week.
// Vector v should have an upperbound of 14
// If the date is unset (days = 0) then the strings
// are all set to "<unset>"

{ LET days, msecs = datv!0, datv!1
  LET datestr, timestr, dowstr = v, v+5, v+10
  LET year = 1970 // BCPL, Unix and Windows epoc
  LET month = 1
  LET dayofweek = ?
  LET dowtemp = ?
  LET hours, mins, secs = ?, ?, ?
  LET monthtab     = TABLE   0, 31, 59, 90,120,151,
                           181,212,243,273,304,334,365
  LET leapmonthtab = TABLE   0, 31, 60, 91,121,152,
                           182,213,244,274,305,335,366
  LET mchars = "JanFebMarAprMayJunJulAugSepOctNovDec"
  LET mcharbase = ?
  LET mtable = ?

  IF datv!2>=0 DO
  { // Convert old dat format to new
    days := days + 2922 // Days between 1 Jan 1970 and 1978
    // Convert (mins,ticks) to msecs assuming 1000 ticks per second
    msecs  := datv!1*60_000 + datv!2
    datv!2 := -1 // mark as new dat format
  }

  dayofweek := (days+4) MOD 7 // 1 Jan 1970 was a Thursday (code=4)
  secs  := msecs/1000         // Seconds since midnight
  msecs := msecs MOD 1000     // msecs since start of current second

//sawritef("dat_to_strings: days=%n secs=%n msecs=%n*n", days, secs, msecs)
  // Deal with case of unset date
  IF days <= 0 DO
  { LET unset = "<unset>"
    FOR i = 0 TO unset%0 DO 
    { LET c = unset%i
      datestr%i := c
      timestr%i := c
      dowstr%i  := c
    }
    RESULTIS v
  }

  days := days + 1
  FOR j=0 TO 9 DO datestr%j := "DD-MMM-YYYY"%j
  FOR j=0 TO 8 DO timestr%j := "HH:MM:SS"%j

  // Construct date

  { // Loop to get year
    LET yearlen = isleap(year) -> 366, 365
    IF days <= yearlen BREAK
    days, year := days - yearlen, year + 1
  } REPEAT

  datestr%8  := year/1000 MOD 10 + '0'
  datestr%9  := year/100  MOD 10 + '0'
  datestr%10 := year/10   MOD 10 + '0'
  datestr%11 := year      MOD 10 + '0'
 
  // Find the month
  mtable := isleap(year) -> leapmonthtab, monthtab

  // 1 <= days <= 366
  month := 1 + days / 32 // Actual month or one less
  IF days > mtable ! month DO month := month+1

  mcharbase := month*3 - 2
  FOR j = 0 TO 2 DO datestr%(4+j) := mchars % (mcharbase + j)
  days := days - mtable ! (month - 1)
  datestr%1 := days/10 + '0'
  datestr%2 := days MOD 10 + '0'

  // Construct time

  mins  := secs  /  60
  hours := mins  /  60
  mins  := mins MOD 60
  secs  := secs MOD 60

  timestr%1 := hours/10 + '0'
  timestr%2 := hours MOD 10 + '0'
  timestr%4 := mins/10 + '0'
  timestr%5 := mins MOD 10 + '0'
  timestr%7 := secs/10 MOD 10 + '0'
  timestr%8 := secs MOD 10 + '0'

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

AND isleap(year) = year MOD 400 = 0 -> TRUE,
                   year MOD 100 = 0 -> FALSE,
                   year MOD   4 = 0 -> TRUE,
                                       FALSE

AND testbit(bitno, bitvec) = VALOF
// This function returns a non zero value if the specified bit in
// bitvec is a one, otherwise it returns zero.
// Bits are numbered from zero starting at the least significant bit
// of bitvec!0.
// bitvec!0 holds bits 0 to bitsperword-1
// bitvec!1 holds bits bitsperword to 2*bitsperword-1
// etc
{ LET i = bitno  /  bitsperword
  AND s = bitno MOD bitsperword
  RESULTIS bitvec!i & (1<<s)
}

AND setbit(bitno, bitvec, state) = VALOF
// This function sets the specified bit in bitvec to 1 or 0 depending
// on whether state is TRUE or FALSE, respectively. It returns a
// non-zero value if the previous setting of the bit was a one, otherwise
// it returns zero. See testbit above.
{ LET i = bitno  /  bitsperword
  AND s = bitno MOD bitsperword
  LET mask = 1 << s
  LET oldstate = bitvec!i & mask
  TEST state THEN bitvec!i := bitvec!i |  mask
             ELSE bitvec!i := bitvec!i & ~mask
  RESULTIS oldstate
}

AND string_to_number(s) = VALOF
// Return TRUE if OK with value in result2
//        FALSE and result2=0 if s is not a number
// Example strings: 
//   'A'
//  123    -99    +63
//  #377   -#x7FF +#b_1011_0011 
// It ignores underscores in digit strings
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
            'A' <= ch <= 'Z' -> ch - 'A' + 10,
            ch='_' -> -1, // Ignore underscores in numbers 
            1000
    UNLESS n < radix RESULTIS FALSE
    IF n>=0 DO result2 := result2 * radix + n
    p := p + 1
    IF p > len BREAK
    ch := capitalch(s%p)
  } REPEAT

  IF neg DO result2 := -result2
  RESULTIS TRUE
}

AND string_to_dat() = VALOF
{ sawritef("function string_to_dat not implemented (BLIB)*n")
  RESULTIS 0
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

AND copy_words(from, to, n) BE
  FOR i = 0 TO n-1 DO to!i := from!i

AND clear_words(v, n) BE
  FOR i = 0 TO n-1 DO v!i := 0

AND copy_bytes(fromlen, from, fillch, tolen, to) = VALOF
// This is an implementation of the VAX MOVC5 instruction
// for copying bytes.
{ LET n = fromlen
  // from and to are byte addresses!!!!!
  IF n>tolen DO n := tolen
  // This code need checking!!!!!
  FOR i = 0 TO n-1 DO 0%(to+i) := 0%(from+i)
  FOR i = n TO tolen-1 DO 0%(to+i) := fillch
  RESULTIS fromlen-n // Number of non copied characters
}



AND getvec(upb) = VALOF
{ LET res = ?
  IF upb<0 DO
  { sawritef("BLIB: getvec(%n) called*n", upb)
    abort(999)
  }
  res := sys(Sys_getvec, upb)
//sawritef("BLIB: task %i2 calling getvec(%i6) => %i6*n", taskid, upb, res)
  RESULTIS res
}

AND freevec(ptr) BE
{ //sawritef("BLIB: task %i2 freevec(%n)*n", taskid, ptr)

  IF ptr UNLESS sys(Sys_freevec, ptr) DO
  { sawritef("BLIB co=%n: freevec failure, ptr=%n*n", currco, ptr)
    abort(999)
  }
}

AND loadseg(name) = sys(Sys_loadseg, name)

AND globin(segl) = sys(Sys_globin, segl)

AND unloadseg(segl) BE sys(Sys_unloadseg, segl)

AND callseg(file, arg1, arg2, arg3, arg4) = VALOF
{ LET res = 0
  LET seg = loadseg(file)
  LET s = start
//sawritef("BLIB: callseg %s entered*n", file)

// BEWARE: The called segment must be careful which globals it uses!
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

AND deletefile(name) = sys(Sys_deletefile, name)

AND renamefile(fromname, toname) = sys(Sys_renamefile, fromname, toname)

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

// Example calls of splitname give the following results

// splitname(prefix, ':', "TCP:shep:9000",  1) =>  5, prefix="TCP"
// splitname(prefix, ':', "TCP:shep:9000",  5) => 10, prefix="shep"
// splitname(prefix, ':', "TCP::9000",      5) =>  6, prefix=""
// splitname(prefix, ':', "TCP:shep",       5) =>  0, prefix="shep"
// splitname(prefix, ':', "TCP:shep:",      5) => 10, prefix="shep"
// splitname(prefix, ':', "TCP:shep:",     10) =>  0, prefix=""
// splitname(prefix, ':', "TCP:shep:9000", 10) =>  0, prefix="9000"

AND splitname(prefix, ch, string, ptr) = VALOF
{ LET len = string%0
  LET res, pos = 0, 0

  WHILE ptr<=len DO
  { LET k = string%ptr
    IF k=ch DO { prefix%0 := pos; RESULTIS  ptr+1 }
    pos, ptr := pos+1, ptr+1
    prefix%pos := k 
  }
  prefix%0 := pos
  RESULTIS 0
}

// Not used
AND open_for_output(name, recsiz, maxrec) = VALOF
{ sawritef("DLIB: open_for_output(%s,%n,%n) called*n", name, recsiz, maxrec) 
  RESULTIS findstream(name, id_outscb, recsiz << 2, maxrec)
}

AND open_for_input(name) = findstream(name, id_inscb, 0)

AND open_for_update(name) = findstream(name, id_inoutscb, 0)

AND setrecordlength(scb, length) = VALOF  // length is in bytes -- MR 12/7/04
{ LET old = scb!scb_reclen
  scb!scb_reclen := length // in bytes
  RESULTIS old
}

AND recordnote(scb) = VALOF // The first record has number 0
// Returns the record number corresponding to the current position
//         of the stream.
// Returns -1 if the stream is not suitable.
{ LET blkno = scb!scb_block    // The blocksize is the buffer size in bytes
  AND reclen = scb!scb_reclen  // in bytes
  IF blkno>=0 & reclen>0 DO // Modified by MR 12/7/04
  { LET recno = muldiv(blkno, scb!scb_bufend, reclen)
    RESULTIS recno + (result2 + scb!scb_pos)/reclen  // MR 12/2/04
  }
  sawritef("BLIB: recordnote: result -1*n")
  RESULTIS -1   // MR 26/7/04
}

AND recordpoint(scb, recno) = VALOF
// MR 28/7/02: The first record of a file has number 0
// Returns TRUE if successful
// Returns FALSE, otherwise.
{ LET pvec = VEC 1
  LET type = scb!scb_type
  UNLESS type=scbt_file | type=scbt_ram DO
  { sawritef("FLIB recordpoint: only works on a disc or RAM file*n")
    abort(999)
    RESULTIS FALSE
  }
  UNLESS recno>=0 DO   // The record number must be >= 0
  { sawritef("blib: recordpoint recno=%n*n", recno)
    abort(999)
    recno := 0
  }
//sawritef("DLIB: recordpoint: muldiv(%n,%n,%n)*n",
//          scb!scb_reclen, recno, scb!scb_bufend)
//abort(1000)
  pvec!0 := muldiv(scb!scb_reclen, recno, scb!scb_bufend) // MR 29/7/02
  pvec!1 := result2
//sawritef("DLIB: recordpoint: recno %n => blockno=%n pos=%n*n",
//          recno, pvec!0, pvec!1)
//abort(1000)
//IF pvec!0>2 DO abort(8888)
  RESULTIS point(scb, pvec)
}

// Position an inout stream to its end
// This should be removed.
AND appendstream(scb) = VALOF
{ LET lblock, ldata = scb!scb_lblock, scb!scb_ldata
//sawritef("DLIB: appendstream called*n"); abort(999)
  UNLESS scb!scb_id=id_inoutscb RESULTIS FALSE
  IF scb!scb_block=lblock & scb!scb_end>ldata DO
    ldata := scb!scb_end
  UNLESS point(scb, @lblock) RESULTIS FALSE
//  scb!scb_pos := scb!scb_end
  RESULTIS TRUE
}

// Position to start of stream
AND rewindstream(scb) = VALOF // MR 17/3/02
{ LET blockno, pos = 0, 0
  RESULTIS point(scb, @blockno)
}

// Advance stream position by n words
AND stepstream(scb, n) = VALOF
{ LET pvec = VEC 1
  LET bytes, len = n * bytesperword, scb!scb_bufend
  LET blocks = bytes / len
  bytes := bytes MOD len
  note(scb, pvec)
  pvec!1 := pvec!1 + bytes
  IF pvec!1 < 0   DO pvec!0, pvec!1 := pvec!0 - 1, pvec!1 + len
  IF pvec!1 > len DO pvec!0, pvec!1 := pvec!0 + 1, pvec!1 - len
  pvec!0 := pvec!0 + blocks
  RESULTIS pvec!0 < 1 -> FALSE, point(scb, pvec)
}

AND freeobj(obj) BE freevec(obj)

AND copydir(dir) = VALOF
{ LET v = getvec((dir%0)/bytesperword)
  IF v FOR i = 0 TO dir%0 DO v%i := dir%i
//sawritef("BLIB: copydir called*n")
//abort(999)
  RESULTIS v
}

// Write the specified number of records of zeroes to the opened file
AND setbulk(scb, no_records) = VALOF
{ LET oldout = output()
  LET n = no_records     * // Number of bytes to write
          scb!scb_reclen
  MANIFEST { bytes = 2048; words=bytes/bytesperword }
  LET v = VEC words
//sawritef("BLIB: setbulk: clearing v!0 to v!%n*n", words-1)
  FOR p = @v!0 TO @v!words-1 DO !p := 0

  rewindstream(scb)
  selectoutput(scb)
  // Used writewords to write 2048 bytes at a time
//sawritef("BLIB: setbulk: writing %n bytes to file*n", n)
  WHILE n>=bytes DO { writewords(v, words); n := n-bytes }
  // and then write the few remaining bytes, if any.
//sawritef("BLIB: setbulk: writing the remaining %n bytes to file*n", n)
  FOR i = 1 TO n DO binwrch(0)
  rewindstream(scb)
  selectoutput(oldout)
  RESULTIS TRUE
}

AND datstamp(v) = sys(Sys_datstamp, v)

AND stackfree(hwm) = VALOF
{ // If hwm=TRUE the result is the number of unused stack words
  //             above the high water mark.
  // otherwise   the result is the number of words between the
  //             current stack frame pointer and the end of stack.
  LET stacksize = currco!co_size
  LET freewords = stacksize - (@hwm - currco)
  IF hwm=TRUE DO
  { FOR p = currco + currco!co_size TO currco BY -1 DO
    { IF !p=stackword LOOP
      freewords := stacksize - (p - currco)
      BREAK
    }
  }
  result2 := stacksize
  RESULTIS freewords
}

// It checks that the memory chain is valid and return the amount
// of free memory, result2 is the Cintcode memory size.
// If the memory chain is corrupt it outputs an error message and
// call abort(999).
AND memoryfree(x) = VALOF
{ LET usedwords = 0
  LET freewords = 0
  LET a = 0 // rootnode!rtn_blklist
  LET topofstore = rootnodeaddr ! rtn_memsize

  // Take highest task priority for critical section
  // (Should always be available if we are running)
  //changepri(taskid, maxpri)

  // Check that the store chain is valid.
  UNTIL !a = 0 DO
  { LET size = !a & -2
    LET next = a + size

    TEST (!a & 1) = 0
    THEN usedwords := usedwords + size
    ELSE freewords := freewords + size

//sawritef("%i8: size %i8 %s  %i8 %i8*n", a, size,
//         ((!a&1)->"used","free"), usedwords, freewords) 

    UNLESS size>=0 & next<=topofstore DO
    { //changepri(taskid, oldpri)
      sawritef("*n******Store chain corrupt!!*n*
               *Noticed at %n next=%n*n", a, next)
      abort(999)
      BREAK
    }
    a := next
  }

  UNLESS a=topofstore DO
  { //changepri(taskid, oldpri)
    sawritef("*n******Store chain corrupt!!*n")
    sawritef("The chain stopped prematurely at %n*n",a)
    sawritef("Top of store is at %n*n", topofstore)
    abort(999)
  }

  //changepri(taskid, oldpri)

  result2 := usedwords+freewords
  RESULTIS freewords
}

