/* version of BLIB for Linux
   DJA 25/2/14 */
SECTION "BLIB"

GET "libhdr"

/* NB:
   setgv must be the first procedure in blib, it is called from alib as an offset from the section "BLIB"
   setgv initially runs without a valid global vector, so must not call any blib procedures
   until all the globals are set
*/

LET setgv(p, gv, gvsize, sv) = VALOF
{
    LET maxg = 0
    LET nsegs = 0
    LET sp = 0        // number of static ptrs
	LET s = 1         // next available static slot
	LET nstatics = 0  // number of statics
	LET m = ?         // number of statics in the current module
	
    FOR i = 0 TO gvsize-1 DO
        gv!i := unglobal
            
    {
        LET sectsize = p!0
        LET t = ?

        IF sectsize = 0
            BREAK

        nsegs := nsegs + 1
        
        t := sectsize-1
        IF p!t > maxg DO
            maxg := p!t

        {
            LET offset, gn = ?, ?
            t := t - 2
            gn := p!t
            offset := p!(t+1)
            IF offset=0
                BREAK
            gv!gn := (p << 2) + offset        
        } REPEAT
		
		m := p!t
		
		IF m > 0
		{
		    FOR i = s-1 TO 0 BY -1 DO         // move table up 2 places
			    sv!(i+2) := sv!i
			sp := sp + 1
			s := s + 2
			sv!0 := p << 2                    // 1st field: base addr of section
			sv!1 := (sv << 2) + (s << 2)      // 2nd field: ptr to statics
			FOR i = 1 TO sp-1 DO
			    sv!(2*i+1) := sv!(2*i+1) + 8  // bump up static offsets
			FOR i = 0 TO m-1 DO               // copy statics from program to svec
			    sv!(s+i) := p!(t-m+i)
			s := s + m
            nstatics := nstatics + m
		}
		
		sv!(nstatics*2) := 0                  // add terminator for static ptrs
        p := p+sectsize
    } REPEAT

/* safe to call blib procedures after this point */

    selectoutput(0)  // stdout
    gv!0 := maxg
	
    RESULTIS p
}

AND unglobal(x) BE  // Arm specific code to detect undefined global calls
{
    LET p = level() >> 2  // current frame
    LET r = p!1 >> 2      // return address
    LET in1 = r!-2        // should be mov r,[rg,#n]
    LET in2 = r!-1        // should blx

    IF (in2 = #xE12FFF3E) & ((in1 & #xFFFFF000) = #xE59AE000) DO
    {
        writef("Unknown global %n*n", (in1 & #xFFF) >> 2)
    }
    stop(0)
}

AND backtrace(a, b, c, d) BE  // Arm specific code to detect calling sequence
{
    LET p = level() >> 2
    LET pc = p!2 >> 2
    LET siginfo = b >> 2

    WHILE pc!-2=#xE8A4C800 & pc!-1=#xE884000F & pc!0=#xE244B00C & (pc-5)%0=11 DO
    {
        writef("%s(%n %n %n %n) ", pc-5, p!3, p!4, p!5, p!6)
        writef("fp=0x%x8 fp'=0x%x8 ra=0x%x8 at 0x%x8", p<< 2, p!0, p!1, (pc-2) << 2)
        IF pc!-5=#x6174730B & pc!-4=#x20207472 & pc!-3=#x20202020 DO  // reached start()
            BREAK
        writes(" called from*n")
        p := p!0 >> 2
        pc := p!2 >> 2
    }
    newline()

    IF a<0 DO
    {
        writef("program stopped: memory access 0x%x8*n", siginfo!3)
        abort(-1)
    }
}

AND getvec(n) = sys(1, (n+1) << 2) >> 2

AND freevec(a) BE
    sys(2, a << 2)

AND rdch() = sys(3, cis)

AND wrch(ch) BE
{
    sys(4, ch, cos)
}

AND findinput(filename) = VALOF
{
    LET str1 = VEC 20
    LET str2 = VEC 20
    bstr2cstr(filename, str1)
    bstr2cstr("rb", str2)
    RESULTIS sys(5, str1 << 2, str2 << 2) // leave as a m/c address
}

AND findoutput(filename) = VALOF
{
    LET str1 = VEC 20
    LET str2 = VEC 20
    bstr2cstr(filename, str1)
    bstr2cstr("wb", str2)
    RESULTIS sys(5, str1 << 2, str2 << 2) // leave as a m/c address
}

AND endread() BE
{
    sys(6, cis)
}

AND endwrite() BE
{
    sys(6, cos)
}

AND endstream(s) BE
{
    sys(6, s)
}

AND pathfindinput(filename, pathname) = VALOF
{
    LET str = VEC 50
    LET p = ?

    IF pathname=0 RESULTIS findinput(filename)
    
    FOR i=1 TO pathname%0 DO
        str%i := pathname%i

    p := pathname%0
	FOR i=1 TO filename%0 DO
    {
        p := p + 1
        str%p := filename%i
    }
    str%0 := p
    RESULTIS findinput(str)
}

AND bstr2cstr(bstr, cstr) BE
{
    LET s = bstr%0
    FOR i=1 TO s DO
        cstr%(i-1) := bstr%i
    cstr%s := 0
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
{ LET t = VEC 10
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
{ 
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

AND writef(format, a, b, c, d, e, f, g, h, i, j, k) BE
{
    LET t = @a

    FOR p = 1 TO format%0 DO
    {
        LET k = format%p

        TEST k='%'

        THEN 
        {
            LET f, n = ?, ?
            p := p+1
            
            SWITCHON capitalch(format%p) INTO
            {
                DEFAULT:  wrch(format%p); ENDCASE
                CASE 'S': f := writes;    GOTO l
                CASE 'T': f := writet;    GOTO m
                CASE 'C': f := wrch;      GOTO l
                CASE 'B': f := writebin;  GOTO m
                CASE 'O': f := writeoct;  GOTO m
                CASE 'X': f := writehex;  GOTO m
                CASE 'I': f := writed;    GOTO m
                CASE 'N': f := writen;    GOTO l
                CASE 'U': f := writeu
             m:           p := p+1
                          n := format%p
                          n := '0'<=n<='9' -> n-'0', 10+n-'A'
             l:           f(!t, n)
                CASE '$': t := t+1
            }
        }
        ELSE 
            wrch(k)
    }
}


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

/*      res := startco(body, arg, stsize)

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

