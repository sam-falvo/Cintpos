// This module contains all the standard BCPL library routines that are machine
// dependant. They are seperate from Klib because they are BCPL support routines
// rather than Tripos support.

SECTION "MLIB"

GET "libhdr"


AND level(x)          =  (@x)!-3
AND longjump(lev, lab) BE
{ LET p = @lev - 3
  p!0, p!1 := lev, lab
}


AND createco(fn, size) = VALOF
{ LET c = getvec(size+6)
  IF c=0 RESULTIS 0
  FOR i = 6 TO size+6 DO c!i := 0

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
//sawritef("MLIB co=%n: deleteco %n*n", stackbase, cptr)

  { LET co = !a
    UNLESS co DO
    { sawritef("MLIB co=%n: cannot deleteco %n -- not found*n",
         stackbase, cptr)
      abort(112)
      RESULTIS FALSE
    }
    IF co=cptr BREAK
    a := @ co!co.list
  } REPEAT

  IF cptr!co.parent DO
  { sawritef("MLIB co=%n: cannot deleteco %n -- has a parent*n",
       stackbase, cptr)
    abort(112)
    RESULTIS FALSE
  }

  !a := cptr!co.list      // Remove the coroutine from colist.
  freevec(cptr)           // Free the coroutine stack.
  RESULTIS TRUE
}


AND callco(cptr, a) = VALOF
{ UNLESS cptr!co.parent=0 DO abort(110)
  cptr!co.parent := currco
  RESULTIS changeco(a, cptr)
}

AND resumeco(cptr, a) = VALOF
{ LET parent = currco!co.parent
  currco!co.parent := 0
  UNLESS cptr!co.parent=0 DO abort(111)
  cptr!co.parent := parent
  RESULTIS changeco(a, cptr)
}


AND initco(fn, size, a, b, c, d, e, f, g, h, i, j, k) = VALOF
{ LET cptr = createco(fn, size)
  UNLESS cptr=0 DO callco(cptr, @a)
  RESULTIS cptr
}

AND cowait(a) = VALOF
{ LET parent = currco!co.parent
  currco!co.parent := 0
  RESULTIS changeco(a, parent)
}


AND stop(code) BE
{ //sawritef("stop(%n) called*n", code)
  //sys(Sys_quit, 999)
  // Return to the CLI with a return code
  returncode := code
  cowait(code)
}

// Get the ith element of vector v of 16-bit unsigned words
AND getword(v, i) = VALOF
{ LET j = i+i
  LET res = v%j + (v%(j+1)<<8)  // Assumes little ender m/c
//  sawritef("MLIB: getword(%n,%n) => %n*n", v, i, res)
//  abort(1000)
  RESULTIS res
}

// Store least sig 16 bits of w in the ith element of vector v of 16-bit words
AND putword(v, i, w) BE // store 16 bit word
{ LET j = i+i
//  sawritef("MLIB: putword(%n,%n,%n)*n", v, i, w)
//  abort(1000)
  v%j, v%(j+1) := w, w>>8  // Assumes little ender m/c
}

AND copystring(from, to) BE
{ sawritef("MLIB: copystring(%s,%n)*n", from, to)
  abort(9999)
  FOR i = 0 TO from%0 DO to%i := from%i
}


AND copy.words(from, to, n) BE
{ sawritef("MLIB: copy.words(%n,%n,%n)*n", from, to, n)
  abort(9999)
  FOR i = 0 TO n-1 DO to!i := from%i
}

AND clear.words(v, n) BE
{ //sawritef("MLIB: clear.words(%n,%n)*n", v, n)
  //abort(9999)
  FOR i = 0 TO n-1 DO v!i := 0
}


AND copy.bytes(fromlen, from, fillch, tolen, to) = VALOF
{ LET n = fromlen
  IF n>tolen DO n := tolen
  sawritef("MLIB: copy.bytes(%n,%n,%n,%n,%n)*n",
           fromlen, from, fillch, tolen, to)
  abort(9999)
  // This code need checking!!!!!
  FOR i = 0 TO n-1 DO to%i := from%i
  FOR i = n TO tolen-1 DO to%i := fillch
  RESULTIS fromlen-n // Number of non copied characters
}


AND clihook(a1) = VALOF
{ //sawritef("clihook: called*n")
//FOR i = 0 TO 5 DO sawritef("currco!%n = %n*n", i, currco!i)
  RESULTIS start(a1)
}

AND sardch()   =  sys(Sys_sardch)

AND sawrch(ch) BE sys(Sys_sawrch,ch)

AND sawritef(form, a, b, c, d, e, f, g, h, i, j, k) BE
{ LET wch, rch = wrch, rdch
  wrch, rdch := sawrch, sardch
  writef(form, a, b, c, d, e, f, g, h, i, j, k)
  wrch, rdch := wch, rch
}

AND sagetvec(upb) = VALOF
{ LET v = sys(Sys_getvec, upb)
  //IF upb=4 DO abort(1234)
  RESULTIS v
}

AND safreevec(ptr) BE UNLESS ptr>=0 & sys(Sys_freevec, ptr) DO
{ sawritef("MLIB co=%n: freevec failure, ptr=%n*n", stackbase, ptr)
  abort(999)
}

AND saloadseg(name) = sys(Sys_loadseg, name)

AND saglobin(segl) = sys(Sys_globin, segl)

AND saunloadseg(segl) BE sys(Sys_unloadseg, segl)

AND sadeletefile(name) = sys(Sys_deletefile, name)

AND sarenamefile(fromname, toname) = sys(Sys_renamefile, fromname, toname)

