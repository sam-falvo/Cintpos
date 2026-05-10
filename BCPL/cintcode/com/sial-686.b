/*
This program compiles .sial files to .s files for the Pentium pro and
later Intel processors. For instance it uses the fucomip instruction
not availabke on earlier processors. The compiled code will run on
earlier processors provided no floating point operations are
compiled. It currently does not compile the SELLD and SELST
instructions so the .sial files must currently be compiled with the
NOSELST option.

Implemented by Martin Richards (c) January 2019

Change history

01/02/2019
Another change in register names. Sial registers are still A, B and C,
but x and y are used for the Pentium registers %ebx and %ecx. The
floating registers st[0] and st[1] are used. and global 11 is used
when transfering values between x and y and the floating point
registers.  The Sial register C is held in %edx.

25/01/2019
Started a major change to the way floating point operations
are optimised. Registers L and R will disappear and lbits and
abits will be renamed abits and bbits. The meaning of the
bits will change.

14/01/2019
Made minor changes to make it compatible with the latest
version of BCPL and SIAL.

21/08/2014
Working on simple optimisations to the floating point operations.

20/08/2014
Adding floating point operations
FIX, FLOAT, #ABS, #*, #/, #+, #-, #=, #~=, #<=, #>=, #< and #>. 
*/

SECTION "sial-686"

GET "libhdr"
GET "sial.h"

GLOBAL {
sialin: ug
asmout; stdin; stdout

rdf; rdp; rdg; rdk; rdw; rdl; rdc
rdcode

nextfcode // =0 or is the peeked next F code
peekfcode // If nextfcode=0 call rdf to peek next code
          // and set nextfcode to its value.

pval; gval; kval; wval; lval; mval

scan
cvf; cvfp; cvfg; cvfk; cvfw; cvfl

sectname
modletter
charv
labnumber

Areg     // =1 => A is held in x ie %ebx
         // =2 => A is held in y ie %ecx
         // =3 => A is held in z ie %edx
         // =4 => A is in st[0]
         // =5 => A is in st[1]
Ak       // =1 => A has value Avalk
Amem     // =1 => A is in Pn when n=Aval
         // =2 => A is in Gn when n=Aval
         // =3 => A is Ln when n=Aval
         // =4 => A is Mn when n=Aval
Avalk
Aval

Breg     // =1 => B is held in x ie %ebx
         // =2 => B is held in y ie %ecx
         // =3 => B is held in z ie %edx
         // =4 => B is in st[0]
         // =5 => B is in st[1]
Bk       // =1 => B has value Bvalk
Bmem     // =1 => B is in Pn when n=Bval
         // =2 => B is in Gn when n=Bval
         // =3 => B is Ln when n=Bval
         // =4 => B is Mn when n=Bval
Bvalk
Bval

tracing  // =TRUE causes debugging info to be embedded in the
         // compiled assembly code file as comments
prstate
errflag

// The move functions update the state variables

moveA2x    // Compile code to ensure A is in x
moveA2y    // Compile code to ensure A is in y
pushA2s    // Compile code to push A into the st stack

moveB2x    // Compile code to ensure B is x
moveB2y    // Compile code to ensure B is y
pushB2s    // Compile code to push B into the ST stack

genK2reg   // Compile code to ensure const k is in a register.

pops2mem   // Compile code to pop st[0] to a memory location

// The gen functions compile code but do not update the state
//they basically call sequences of writef calls.

genK2r
}

/*
Optimisation details

Sial is an assembly language for a simple machine having registers
such as A, B, P, G and PC. These registers typically map into central
registers of the target machine. For this implementation, A is %ebx, B
is %ecx, P is %ebp and G is %esi. Sial instructions such as LP P3
cause A to be set to the value of P!3 and this translates to 

      movl 12(%ebp),%ebx.

Sial now includes floating point operators, so the LP instruction
should not necessarily update %ebx. For example, consider the
compilation of x := x #+ y whose Sial code might be

 LP P3           A := P!3
 ATBLP P4        B := A; A := P!4
 FADD            A := B #+ A; B := ?
 SP P3           P!3 := A

The compiled code we might like to generate is

 flds  12(%ebp)   push P!3 into %st(0) 
 fadds 16(%ebp)   %st(0) := %st(0) #+ P!4
 fstps 12(%ebp)   pop st(0) to P!3

A dyadic floating point operations such as FADD should cause one or
both of its operands to be pushed onto the st stack. In this case only
B is pushed since the value of A is addressible allowing fadds to be
used.

To achieve this, this translator delays the generation of code by
regarding A and B as abtract registers whose values may not be in the
physical register Sial expects. The actual value of an sial register
may be held in one or more places a specified by variable such as Ap
and Aval. The possible places are the physical registers x and y, a
constant k, st[0] or st[1], or a memory locaion sucg as Pn, Gn, Ln or
Mn.

The Pentium allows an st stack of 8 floating point numbers but this
codegenerator only uses two levels. The st stack is empty if both As
and Bs are zero. A is held in st[0] if As=1 and B is held in st[0] if
Bs=1.  Similarly, A is held in st[1] if As=2 and B is held in st[1] if
Bs=2.  But there is the restriction that As and Bs cannot be equal if
either is non zero.

The state variables typically change when reading Sial statements or
after generating machine instructions. These changes must be done with
care to ensure that the state is always consistent and that no
information is lost. For example, observe how the variables change
during the translation of the Sial code given above code.

 Sial          Code            State

 LP P3
                               B=?   A=P3
 ATBLP P4
                               B=P3  A=P4
 FADD
               flds 12(%ebp)
                               B=st[0]P3  A=P4
               fadds 16(%ebp)
                               B=?  A=st[0]     --  = P3 #+ P4
 SP P3
               fstps 12(%ebp)
                               B=?   A=P3

This shows that the LP P3 statement specifies that the value of A is
in local variable 3 (A=P3). We assume that the value of B at that
moment is unspecified. The instruction ATBLP P4 causes the value in A
to be moved to B before specifying that A is in local variable P4. The
statement FADD must compile code to perform the floating point
addition of P3 and P4. It does this by first pushing P3 onto the
floating point stack (flds 12(%ebp)). At this point B=st[0]P3 stating
that the value of B is both in st[0] and in local variable 3.  The
instruction fadds 16(%ebp) then performs the floating point addition
of local 4 whose address is 16(%ebp). The resulting state shows that A
holds the result in st[0] and that B has become undefined.  Finally,
the statement SP P3, pops %st(0) from the floating point stack storing
it in local 3 (at address 12(%ebp)). A is now known to be in local 3
(A=P3) but no no longer in st[0] because the floating point stack has
been popped (by fstps).

To push a value from say A (%ebx) onto the floating point stack, it is
usually necessary to use a memory location. Often the memory location G11
is used as in:

  push %ebx
  flds 44(%esi)

The only slight disadvantage is that G11 might be corrupted by any
floating point operation.

*/

LET trace(str, a, b, c) BE IF tracing DO
  writef(str, a, b, c)

AND prstate() BE IF tracing DO
{ writef("#                 B=")
  IF (Breg+Bk+Bmem)=0 DO wrch('?')
  IF Breg=1 DO wrch('x')
  IF Breg=2 DO wrch('y')
  IF Breg=3 DO wrch('z')
  IF Breg=4 DO writef("st[0]*n")
  IF Breg=5 DO writef("st[1]*n")
  IF Bk DO writef("K%n", Bvalk)
  IF Bmem=1 DO writef("P%n", Bval)
  IF Bmem=2 DO writef("G%n", Bval)
  IF Bmem=3 DO writef("L%n", Bval)
  IF Bmem=4 DO writef("M%n", Bval)

  writef("    A=")
  IF (Areg+Ak+Amem)=0 DO wrch('?')
  IF Areg=1 DO wrch('x')
  IF Areg=2 DO wrch('y')
  IF Areg=3 DO wrch('z')
  IF Areg=4 DO writef("st[0]*n")
  IF Areg=5 DO writef("st[1]*n")
  IF Ak DO writef("K%n", Avalk)
  IF Amem=1 DO writef("P%n", Aval)
  IF Amem=2 DO writef("G%n", Aval)
  IF Amem=3 DO writef("L%n", Aval)
  IF Amem=4 DO writef("M%n", Aval)

newline()
}

LET start() = VALOF
{ LET argv = VEC 20
  LET v    = VEC 20
  LET cv   = VEC 256/bytesperword

  sectname := v
  sectname%0 := 0
  modletter := 'A'
  charv := cv
  labnumber := 0

  errflag := FALSE
  
  asmout := 0
  stdout := output()
  IF rdargs("FROM,TO/K,-t/s", argv, 20)=0 DO
  { writes("Bad args for sial-686*n")
    RESULTIS 20
  }
  IF argv!0=0 DO argv!0 := "prog.sial"  // FROM
  IF argv!1=0 DO argv!1 := "prog.s"     // TO/K
  tracing := argv!2                     // -t/s

tracing := TRUE      // Currently always have tracing on

  sialin := findinput(argv!0)
  IF sialin=0 DO
  { writef("Trouble with file %s*n", argv!0)
    RESULTIS 20
  }
  asmout := findoutput(argv!1)
   
  IF asmout=0 DO
  { writef("Trouble with file %s*n", argv!1)
    RESULTIS 20
  }
   
  //writef("Converting %s to %s*n", argv!0, argv!1)
  selectinput(sialin)
  selectoutput(asmout)

  // Initialise the state
  Areg, Ak, Amem := 0, 0, 0
  Breg, Bk, Bmem := 0, 0, 0


  nextfcode := 0 // Initialise the F code peeking mechanism

  writef("# Code generated by sial-686*n*n")
  writef(".text*n*n")
  writef(".align 16*n")

  scan()

  endread()
  UNLESS asmout=stdout DO endstream(asmout)
  selectoutput(stdout)
  writef("Conversion complete*n")
  RESULTIS 0
}

AND nextlab() = VALOF
{ labnumber := labnumber+1
  RESULTIS labnumber
}

AND rdcode(letter) = VALOF
{ // Read an Sial item of the form <letter>n
  // <let> is one of F, P, G, K, W, L, M or C
  LET a, ch, neg = 0, ?, FALSE

  ch := rdch() REPEATWHILE ch='*s' | ch='*n'

  IF ch=endstreamch RESULTIS -1

  UNLESS ch=letter DO
    error("Bad item, looking for %c found %c*n", letter, ch)

  ch := rdch()

  IF ch='-' DO { neg := TRUE; ch := rdch() }

  WHILE '0'<=ch<='9' DO { a := 10*a + ch - '0'; ch := rdch()  }

  RESULTIS neg -> -a, a
}

AND peekfcode() = VALOF
{ UNLESS nextfcode DO nextfcode := rdcode('F')
  RESULTIS nextfcode
}

AND rdf() = VALOF
{ TEST nextfcode
  THEN { LET f = nextfcode
         nextfcode := 0
         RESULTIS f
       }
  ELSE { RESULTIS rdcode('F')
       }
}

AND rdp() = VALOF { pval := rdcode('P'); RESULTIS pval }
AND rdg() = VALOF { gval := rdcode('G'); RESULTIS gval }
AND rdk() = VALOF { kval := rdcode('K'); RESULTIS kval }
AND rdw() = VALOF { wval := rdcode('W'); RESULTIS wval }
AND rdl() = VALOF { lval := rdcode('L'); RESULTIS lval }
AND rdm() = VALOF { mval := rdcode('M'); RESULTIS mval }
AND rdc() = rdcode('C')

AND error(mess, a, b, c) BE
{ LET out = output()
  UNLESS out=stdout DO
  { selectoutput(stdout)
    writef(mess, a, b, c)
    selectoutput(out)
  }
  writef(mess, a, b, c)
}

AND moveA2B() BE
{ // Generate code for B := A
  trace("# moveA2B*n")
  Bmem, Bk, Bvalk, Bmem, Bval := Amem, Ak, Avalk, Amem, Aval
  prstate()
}

AND moveB2A() BE
{ // Generate code for A := B
  trace("# moveB2A*n")
  Amem, Ak, Avalk, Amem, Aval := Bmem, Bk, Bvalk, Bmem, Bval
  prstate()
}

AND freex() BE
{ trace("# freex*n")

  IF Breg=1 & (Bk+Bmem)=0 DO
  { // B is only in x, so move it to y
    TEST Areg=2
    THEN { // Exchange x and y
           writef(" xchgl %%ebx,%%ecx*n")
           Areg, Breg := 1, 2
         }
    ELSE { // Move x to y
           writef(" movl %%ebx,%%ecx*n")
           Amem := 1
         }
    prstate()
  }

  IF Breg=1 DO
  { Breg := 0
    prstate()
  }
  trace("# freex done*n")
}

AND freey() BE
{ trace("# freey*n")

  IF Breg=2 & (Bk+Bmem)=0 DO
  { // A is only in y, so move it to x
    TEST Areg=1
    THEN { // Exchange x and y
           writef(" xchgl %%ebx,%%ecx*n")
           Areg, Breg := 2, 1
         }
    ELSE { // Move y to x
           writef(" movl %%ecx,%%ebx*n")
           Bmem := 1
         }
    prstate()
  }

  IF Areg=2 DO
  { Areg := 0
    prstate()
  }
  trace("# freey done*n")
}

AND genK2x(k) BE
{ // Compile code to put constant k in x
  trace("# moveK2x(%n)*n", k)

  TEST k
  THEN writef(" movl $%n,%%ebx*n", k)
  ELSE writef(" xorl %%ebx,%%ebx*n")

  writef(" movl $%n,%%ebx*n", k)
}

AND moveK2y(k) BE
{ // Compile code to put constant k in x
  trace("# moveK2y(%n)*n", k)

  TEST k
  THEN writef(" movl $%n,%%ecx*n", k)
  ELSE writef(" xorl %%ecx,%%ecx*n")
}

AND moveA2xyors0() BE
{ IF Areg=1 | Areg=2 | Areg=4 RETURN // A is already in x, y or st[0]
  moveA2either()
}

AND moveB2xyors0() BE
{ IF Breg=1 | Breg=2 | Breg=4 RETURN // Bis already in x, y or st[0]
  moveB2either()
}

AND moveA2either() BE
{ TEST Breg=1 THEN moveA2y()
              ELSE moveA2x()
}

AND moveB2either() BE
{ TEST Areg=1 THEN moveB2y()
              ELSE moveB2x()
}

AND moveAB2both() BE
{ // Ensure that A is in x and B is in Y
  //          or A is in y and B is in x
  IF Areg=1 DO { moveB2y(); RETURN }
  IF Areg=2 DO { moveB2x(); RETURN }
  IF Breg=1 DO { moveA2y(); RETURN }
  IF Breg=2 DO { moveA2x(); RETURN }

  // Neither A nor B are in x or y
  moveA2x()
  moveB2y()
}

AND moveA2x() BE
{ // Compile code to ensure that A is in x
  // B must not be in x
  
  trace("# moveA2x*n")

  // Check that B is OK
  IF Breg=1 TEST (Bk+Bmem)=0
            THEN { prstate()
                   writef("# SYSERROR: x must not be in B in moveA2x*n")
                 }
            ELSE { Breg := 0
                 }

  IF Areg=1 RETURN // A is already in x

  IF Areg=2 DO
  { // A is in y so copy y to x
    writef(" movl %%ecx,%%ebx*n")
    Areg := 1
    GOTO ret
  }

  IF Ak DO
  { Areg := 1
    wropKreg("movl", Avalk, Areg)
    GOTO ret
  }

  IF Areg=5 DO
  { // A is in st[1] and B must be in st[0] so swap st[0] and st[1]
    UNLESS Breg=4 DO errflag := TRUE
    writef(" fxch %%st[1],%%st*n")
    Areg := 4   // A is now in st[0]
    Breg := 5   // B is now in st[1]
    prstate()
  }
  
  IF Areg=4 DO
  { // Now pop st[0] to G11
    Amem, Aval := 2, 11
    wropmem("fstps", Amem, Aval)   // Pop st[0] to G11
    Areg := 0                      // A is no longer in st[0]
    IF Breg=5 DO
        Breg := 4                  // B is now in st[0]
    prstate()
  }
  
  IF Amem DO
  { // A is Pn, Gn, La, Mn, so move it to x
    Areg := 1
    wropmemreg("movl", Amem, Aval, Areg)
    GOTO ret
  }

  IF (Areg+Ak+Amem)=0 DO
  { // A is undefined so give it the value already in x
    // This is possibly an error
    Areg := 1
    GOTO ret
  }

  errflag := TRUE

ret:
  prstate()
  IF errflag DO writef("# SYSERROR: In moveA2x*n")
  errflag := FALSE
}

AND moveA2y() BE
{ // Compile code to ensure that A is in y
  // B must not be in y
  
  trace("# moveA2y*n")

  // Check that B is OK
  IF Breg=2 TEST (Bk+Bmem)=0
            THEN { prstate()
                   writef("# SYSERROR: y must not be in B in moveA2y*n")
                 }
            ELSE { Breg := 0
                 }

  IF Areg=2 RETURN // A is already in y

  IF Areg=1 DO
  { // A is in x so copy x to y
    writef(" movl %%ebx,%%ecx*n")
    Areg := 2
    GOTO ret
  }

  IF Ak DO
  { Areg := 2
    wropKreg("movl", Avalk, Areg)
    GOTO ret
  }

  IF Areg=5 DO
  { // A is in st[1] and B must be in st[0] so swap st[0] and st[1]
    UNLESS Breg=4 DO errflag := TRUE
    writef(" fxch %%st[1],%%st*n")
    Areg := 4   // A is now in st[0]
    Breg := 5   // B is now in st[1]
    prstate()
  }
  
  IF Areg=4 DO
  { // Now pop st[0] to G11
    Amem, Aval := 2, 11
    wropmem("fstps", Amem, Aval)   // Pop st[0] to G11
    Areg := 0                      // A is no longer in st[0]
    IF Breg=5 DO
        Breg := 4                  // B is now in st[0]
    prstate()
  }
  
  IF Amem DO
  { // A is Pn, Gn, La, Mn, so move it to y
    Areg := 2
    wropmemreg(" movl ", Amem, Aval, Areg)
    GOTO ret
  }

  IF (Areg+Ak+Amem)=0 DO
  { // A is undefined so give it the value already in y
    // This is possibly an error
    Areg := 2
    GOTO ret
  }

  errflag := TRUE

ret:
  prstate()
  IF errflag DO writef("# SYSERROR: In moveA2x*n")
  errflag := FALSE
}

AND moveB2x() BE
{ // Compile code to ensure that B is in x
  // A must not be in x
  
  trace("# moveB2x*n")

  // Check that A is OK
  IF Areg=1 TEST (Ak+Amem)=0
            THEN { prstate()
                   writef("# SYSERROR: x must not be in A in moveB2x*n")
                 }
            ELSE { Areg := 0
                 }

  IF Breg=1 RETURN // B is already in x

  IF Breg=2 DO
  { // B is in y so copy y to x
    writef(" movl %%ecx,%%ebx*n")
    Breg := 1
    GOTO ret
  }

  IF Bk DO
  { Breg := 1
    wropKreg("movl", Bval, Breg)
    GOTO ret
  }

  IF Breg=5 DO
  { // B is in st[1] and A must be in st[0] so swap st[0] and st[1]
    UNLESS Areg=4 DO errflag := TRUE
    writef(" fxch %%st[1],%%st*n")
    Breg := 4   // B is now in st[0]
    Areg := 5   // A is now in st[1]
    prstate()
  }
  
  IF Breg=4 DO
  { // Now pop st[0] to G11
    Bmem, Bval := 2, 11
    wropmem("fstps", Bmem, Bval)   // Pop st[0] to G11
    Breg := 0
    IF Areg=2 DO
      Areg := 5                    // A is now in st[0]
    prstate()
  }
  
  IF Bmem DO
  { // B is Pn, Gn, La, Mn, so move it to x
    Breg := 1
    wropmemreg(" movl ", Bmem, Bval, Breg)
    GOTO ret
  }

  IF (Breg+Bk+Bmem)=0 DO
  { // B is undefined so give it the value already in x
    // This is possibly an error
    Breg := 1
    GOTO ret
  }

  errflag := TRUE
  
ret:
  prstate()
  IF errflag DO writef("# SYSERROR: In moveB2x*n")
  errflag := FALSE
}

AND moveB2y() BE
{ // Compile code to ensure that B is in y
  // A must not be in y
  
  trace("# moveB2y*n")

  // Check that A is OK
  IF Areg=2 TEST (Ak+Amem)=0
            THEN { prstate()
                   writef("# SYSERROR: y must not be in A in moveB2y*n")
                 }
            ELSE { Areg := 0
                 }

  IF Breg=2 RETURN // B is already in y

  IF Breg=1 DO
  { // B is in x so copy x to y
    writef(" movl %%ebx,%%ecx*n")
    Breg := 2
    GOTO ret
  }

  IF Bk DO
  { Breg := 2
    wropKreg("movl", Bvalk, Breg)
    GOTO ret
  }

  IF Breg=5 DO
  { // B is in st[1] and A must be in st[0] so swap st[0] and st[1]
    UNLESS Areg=1 DO errflag := TRUE
    writef(" fxch %%st[1],%%st*n")
    Breg := 4   // B is now in st[0]
    Areg := 5   // A is now in st[1]
    prstate()
  }
  
  IF Breg=4 DO
  { // Now pop st[0] to G11
    Bmem, Bval := 2, 11
    wropmem("fstps", Bmem, Bval)   // Pop st[0] to G11
    Breg := 0                      // B is no longer in st[0]
    IF Areg=5 DO
      Areg := 4                    // A is now in st[0]
    prstate()
  }
  
  IF Bmem DO
  { // B is Pn, Gn, La, Mn, so move it to y
    Breg := 2
    wropmemreg(" movl ", Bmem, Bval, Breg)
    GOTO ret
  }

  IF (Breg+Bk+Bmem)=0 DO
  { // B is undefined so give it the value already in y
    Breg := 2
    GOTO ret
  }

  errflag := TRUE
  
ret:
  prstate()
  IF errflag DO writef("# SYSERROR: In moveB2y*n")
  errflag := FALSE
}

AND moveA2mem(mem, val) BE
{ writef("# SYSERROR: moveA2mem not implemented*n")
}

AND moveB2mem(mem, val) BE
{ writef("# SYSERROR: moveB2mem not implemented*n")
}

AND pushmem2S(mem,val) BE
{ writef("# SYSERROR: pushmem2S not implemented*n")
}

AND popS2mem(mem,val) BE
{ writef("# SYSERROR: popS2mem not implemented*n")
}

AND wropmemreg(opstr,mem,val,reg) BE
{ writef(" %s ", opstr)
  wrmem(mem, val)
  wrch(',')
  wrreg(reg)
  newline()
  IF errflag DO writef("# SYSERROR:*n")
  errflag := FALSE
}

AND wropregmem(opstr,reg,mem,val) BE
{ writef(" %s ", opstr)
  wrreg(reg)
  wrch(',')
  wrmem(mem, val)
  newline()
  IF errflag DO writef("# SYSERROR:*n")
  errflag := FALSE
}

AND wropKreg(opstr,k,reg) BE
{ writef(" %s $%n,", opstr, k)
  wrreg(reg)
  newline()
}

AND wropmem(opstr,mem,val) BE
{ writef(" %s ", opstr)
  wrmem(mem,val)
  newline()
}

AND wropreg(opstr,reg) BE
{ writef(" %s ", opstr)
  wrreg(reg)
  newline()
}

AND wropregreg(opstr,r1, r2) BE
{ writef(" %s ", opstr)
  wrreg(r1)
  wrch(',')
  wrreg(r2)
  newline()
}

AND wrreg(reg) BE SWITCHON reg INTO
{ DEFAULT:  writes("%eax")
            errflag := TRUE
	    RETURN
	    
  CASE 1:   writes("%ebx"); RETURN
  CASE 2:   writes("%ecx"); RETURN
  CASE 3:   writes("%edx"); RETURN
  CASE 4:   writes("%st"); RETURN
  CASE 5:   writes("%st[1]"); RETURN
}

AND wrmem(mem, val) BE SWITCHON mem INTO
{ DEFAULT: writef("LA1")
           errflag := TRUE
           RETURN

  CASE 1:  writef("%n(%%ebp)", 4*val)
           RETURN
  CASE 2:  writef("%n(%%esi)", 4*val)
           RETURN
  CASE 3:  writef("L%c%n", modletter, val)
           RETURN
  CASE 4:  writef("M%c%n", modletter, val)
           RETURN
}

AND scan() BE
{ LET op = rdf()
//writef("# scan: op=%n*n", op)
  SWITCHON op INTO

  { DEFAULT:       error("# Bad SIAL op %n*n", op); LOOP

    CASE -1:       RETURN
      
    CASE f_lp:     cvfp("LP") // A := P!n
                   // Specify that the value of A is now only in Pn
                   Areg, Ak, Amem, Aval := 0, 0, 1, pval
                   ENDCASE

    CASE f_lg:     cvfg("LG") // A := G!n
                   // Specify that the value of A is now only in Gn
                   Areg, Ak, Amem, Aval := 0, 0, 2, gval
                   ENDCASE

    CASE f_ll:     cvfl("LL") // A := !Ln
                   // Specify that the value of A is now only in Ln
                   Areg, Ak, Amem, Aval := 0, 0, 3, lval
                   ENDCASE

    CASE f_llp:    cvfp("LLP") // A := @ P!n
                   IF Breg=1 & (Bk+Bmem)=0 DO
                   { // B is only in xso move x to y
		     writef(" movl %%ebx,%%ecx*n")
                     Breg := 2
                     prstate()
                   }
                   writef(" leal %n(%%ebp),%%ebx*n", 4 * pval)
                   writef(" shrl $2,%%ebx*n")
                   Areg, Ak, Amem := 0, 0, 0
                   prstate()
                   ENDCASE

    CASE f_llg:    cvfg("LLG") // A := @ G!n
                   IF Breg=1 & (Bk+Bmem)=0 DO
                   { // B is only in xso move x to y
		     writef(" movl %%ebx,%%ecx*n")
                     Breg := 2
                     prstate()
                   }
                   writef(" leal %n(%%edx),%%ebx*n", 4 * gval)
                   writef(" shrl $2,%%ebx*n")
                   Areg, Ak, Amem := 1, 0, 0
                   prstate()
                   ENDCASE

    CASE f_lll:    cvfl("LLL") // A := @ !Ln
                   IF Breg=1 & (Bk+Bmem)=0 DO
                   { // B is only in xso move x to y
		     writef(" movl %%ebx,%%ecx*n")
                     Breg := 2
                     prstate()
                   }
                   writef(" leal L%c%n,%%ebx*n", modletter, lval)
                   writef(" shrl $2,%%ebx*n")
                   Areg, Ak, Amem := 1, 0, 0
                   prstate()
                   ENDCASE

    CASE f_lf:     cvfl("LF") // A := byte address of Ln
                   IF Breg=1 & (Bk+Bmem)=0 DO
                   { // B is only in xso move x to y
		     writef(" movl %%ebx,%%ecx*n")
                     Breg := 2
                     prstate()
                   }
                   writef(" leal L%c%n,%%ebx*n", modletter, lval)
                   Areg, Ak, Amem := 1, 0, 0
                   prstate()
                   ENDCASE

    CASE f_lw:     cvfm("LW") // A := Mn
                   // Specify that the value of A is now in Mn
                   Areg, Ak, Amem, Aval := 0, 0, 4, mval
                   prstate()
                   ENDCASE

    CASE f_l:      cvfk("L") // A := n
                   Areg, Ak, Avalk, Amem := 0, 1, kval, 0
                   prstate()
                   ENDCASE

    CASE f_lm:     cvfk("LM") // a := -n
                   Areg, Ak, Avalk, Amem := 0, 1, -kval, 0
                   prstate()
                   ENDCASE

    CASE f_sp:     cvfp("SP") // P!n := A
                   IF Bmem=1 & Bval=pval & Bk=0 & Breg=0 DO moveB2xyors0()

                   moveA2xyors0() // Ensure A into x, y or st[0]
                                  // but not in a register holding B

                   Amem, Aval := 1, pval // The destination addr
                   IF Areg=4 DO
                   { wropmem("fstps", Amem, Aval) // Compile pop st[0] to P!n
                     Areg := 0  // A is no longer in st[0]
                     IF Bmem=Amem & Bval=Aval DO Bmem := 0
                     // B may have moved from st[1] to st[0]
		     IF Breg = 5 DO Breg := 4
                     prstate()
                     ENDCASE
                   }
		   wropregmem("movl", Areg, Amem, Aval)
                   IF Bmem=Amem & Bval=Aval DO Bmem := 0
                   prstate()
                   ENDCASE

    CASE f_sg:     cvfg("SG") // G!n := A
                   IF Bmem=2 & Bval=gval & Bk=0 & Breg=0 DO moveB2xyors0()

                   moveA2xyors0() // Ensure A into x, y or st[0]
                                  // but not in a register holding B

                   Amem, Aval := 2, gval // The destination addr
                   IF Areg=4 DO
                   { wropmem("fstps", Amem, Aval) // Compile pop st[0] to G!n
                     Areg := 0  // A is no longer in st[0]
                     IF Bmem=Amem & Bval=Aval DO Bmem := 0
                     // B may have moved from st[1] to st[0]
		     IF Breg = 5 DO Breg := 4
		     prstate()
                     ENDCASE
                   }
		   wropregmem("movl", Areg, Amem, Aval)
                   IF Bmem=Amem & Bval=Aval DO Bmem := 0
                   prstate()
                   ENDCASE

    CASE f_sl:     cvfl("SL") // !Ln := A
                   IF Bmem=3 & Bval=lval & Bk=0 & Breg=0 DO moveB2xyors0()

                   moveA2xyors0() // Ensure A into x, y or st[0]
                                  // but not in a register holding B

                   Amem, Aval := 3, lval // The destination addr
                   IF Areg=4 DO
                   { wropmem("fstps", Amem, Aval) // Compile pop st[0] to !Ln
                     Areg := 0  // A is no longer in st[0]
                     IF Bmem=Amem & Bval=Aval DO Bmem := 0
                     // B may have moved from st[1] to st[0]
		     IF Breg = 5 DO Breg := 4
                     ENDCASE
                   }
		   wropregmem("movl", Areg, Amem, Aval)
                   IF Bmem=Amem & Bval=Aval DO Bmem := 0
                   prstate()
                   ENDCASE

    CASE f_ap:     cvfp("AP") // A := A + P!n
                   moveA2either() // Ensure A is in x or y
                   wropmemreg("addl", 1, pval, Areg)
                   Ak, Amem := 0, 0
                   prstate() 
                   ENDCASE

    CASE f_ag:     cvfg("AG") // a := a + G!n
                   moveA2either() // Ensure A is in x or y
                   wropmemreg("addl", 2, gval, Areg)
                   Ak, Amem := 0, 0
                   prstate() 
                   ENDCASE

    CASE f_a:      cvfk("A") // a := a + n
                   moveA2either() // Ensure A is in x or y
                   SWITCHON kval INTO
                   { DEFAULT: wropKreg("addl", kval, Areg)
                     CASE  0: ENDCASE
                     CASE  1: wropreg("incl", Areg); ENDCASE
                     CASE -1: wropreg("decl", Areg); ENDCASE
                   }
                   Ak, Amem := 0, 0
		   prstate()
                   ENDCASE

    CASE f_s:      cvfk("S")  // a := a - n
                   moveA2either() // Ensure A is in x or y
                   SWITCHON kval INTO
                   { DEFAULT: wropKreg("subl", kval, Areg)
                     CASE  0: ENDCASE
                     CASE  1: wropreg("decl", Areg); ENDCASE
                     CASE -1: wropreg("dinl", Areg); ENDCASE
                   }
                   Ak, Amem := 0, 0
		   prstate()
                   ENDCASE

    CASE f_lkp:    cvfkp("LKP") // a := P!n!k
                   moveA2x()
                   writef(" movl %n(%%ebp),%%eax*n", 4*pval)
                   writef(" movl %n(,%%eax,4),%%ebx*n", 4*kval)
                   Areg, Ak, Amem := 1, 0, 0
		   prstate()
                   ENDCASE

    CASE f_lkg:    cvfkg("LKG") // a := G!n!k
                   moveA2x()
                   writef(" movl %n(%%esi),%%eax*n", 4*gval)
                   writef(" movl %n(,%%eax,4),%%ebx*n", 4*kval)
                   Areg, Ak, Amem := 1, 0, 0
		   prstate()
                   ENDCASE

    CASE f_rv:     cvf("RV")  // a := ! a
                   moveA2x()
                   writef(" movl (,%%ebx,4),%%ebx*n")
                   Ak, Amem := 0, 0
		   prstate()
                   ENDCASE

    CASE f_rvp:    cvfp("RVP") // a := P!n!a
                   moveA2x()
                   writef(" addl %n(%%ebp),%%ebx*n", 4*pval)
                   writef(" movl (,%%ebx,4),%%ebx*n")
                   Ak, Amem := 0, 0
		   prstate()
                   ENDCASE

    CASE f_rvk:    cvfk("RVK") // a := a!k
                   moveA2x()
                   writef(" movl %n(,%%ebx,4),%%ebx*n", 4*kval)
                   Ak, Amem := 0, 0
		   prstate()
                   ENDCASE

    CASE f_st:     cvf("ST") // !a := b
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ecx,(,%%ebx,4)*n")
                   Amem := 0
                   Bmem := 0
		   prstate()
                   ENDCASE

    CASE f_stp:    cvfp("STP") // P!n!a := b
                   moveB2y()
                   moveA2x()
                   writef(" movl %n(%%ebp),%%eax*n", 4*pval)
                   writef(" addl %%ebx,%%eax*n")
                   writef(" movl %%ecx,(,%%eax,4)*n")
                   Amem := 0
                   Bmem := 0
		   prstate()
                   ENDCASE

    CASE f_stk:    cvfk("STK") // a!n := b
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ecx,%n(,%%ebx,4)*n", 4*kval)
                   Amem := 0
                   Bmem := 0
		   prstate()
                   ENDCASE

    CASE f_stkp:   cvfkp("STKP")  // P!n!k := a
                   moveA2x()
                   writef(" movl %n(%%ebp),%%eax*n", 4*pval)
                   writef(" movl %%ebx,%n(,%%eax,4)*n", 4*kval)
                   Amem := 0
                   Bmem := 0
		   prstate()
                   ENDCASE

    CASE f_skg:    cvfkg("SKG") // G!n!k := a
                   moveA2x()
                   writef(" movl %n(%%esi),%%eax*n", 4*gval)
                   writef(" movl %%ebx,%n(,%%eax,4)*n", 4*kval)
                   Amem := 0
                   Bmem := 0
		   prstate()
                   ENDCASE

    CASE f_xst:    cvf("XST") // !b := a
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ebx,(,%%ecx,4)*n")
                   Amem := 0
                   Bmem := 0
		   prstate()
                   ENDCASE

    CASE f_k:      cvfp("K") // Call  a(b,...) incrementing P by n
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ebx,%%eax*n")
                   writef(" movl %%ecx,%%ebx*n")
                   writef(" leal %n(%%ebp),%%edx*n", 4*pval)
                   writef(" call **%%eax*n")
                   Areg, Ak, Amem := 1, 0, 0
                   Breg, Bk, Bmem := 0, 0, 0
		   prstate()
                   ENDCASE

    CASE f_kpg:    cvfpg("KPG") // Call Gg(a,...) incrementing P by n
                   moveA2x()
                   writef(" movl %n(%%esi),%%eax*n", 4*gval)
                   writef(" leal %n(%%ebp),%%edx*n", 4*pval)
                   writef(" call **%%eax*n")
                   Areg, Ak, Amem := 1, 0, 0
                   Breg, Bk, Bmem := 0, 0, 0
		   prstate()
                   ENDCASE

    CASE f_neg:    cvf("NEG") // a := - a
                   moveA2x()
                   writef(" negl %%ebx*n") 
                   Ak, Amem := 0, 0
		   prstate()
                   ENDCASE

    CASE f_not:    cvf("NOT") // a := ~ a
                   moveA2x()
                   writef(" notl %%ebx*n") 
                   Ak, Amem := 0, 0
		   prstate()
                   ENDCASE

    CASE f_abs:    cvf("ABS") // a := ABS a
                   moveA2x()
                 { LET l = nextlab()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" jge L%n*n", l)
                   writef(" negl %%ebx*n")
                   writef("L%n:*n", l)
                   Ak, Amem := 0, 0
		   prstate()
                   ENDCASE
                 }

    CASE f_xdiv:   cvf("XDIV") // a := a / b
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ebx,%%eax*n")
                   writef(" cdq*n")
                   writef(" idiv %%ecx*n")
                   writef(" movl %%eax,%%ebx*n")
                   Ak, Amem := 0, 0
		   prstate()
		   writef("# UNCHECKED*n")
                   ENDCASE

    CASE f_xmod:   cvf("XMOD") // a := a MOD b
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ebx,%%eax*n")
                   writef(" cdq*n")
                   writef(" idiv %%ecx*n")
                   writef(" movl %%edx,%%ebx*n")
                   Ak, Amem := 0, 0
		   prstate()
                   ENDCASE

    CASE f_xsub:   cvf("XSUB") // a := a - b
                   moveB2y()
                   moveA2x()
                   writef(" subl %%ecx,%%ebx*n")
                   Ak, Amem := 0, 0
		   prstate()
                   ENDCASE

    CASE f_mul:    cvf("MUL") // a := b * a; c := ?
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ecx,%%eax*n")
                   writef(" imul %%ebx*n") // currupts edx
                   writef(" movl %%eax,%%ebx*n")
                   Ak, Amem := 0, 0
		   prstate()
                   ENDCASE

    CASE f_div:    cvf("DIV")  // a := b / a; c := ?
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ecx,%%eax*n")
                   writef(" cdq*n")
                   writef(" idiv %%ebx*n")
                   writef(" movl %%eax,%%ebx*n")
                   Ak, Amem := 0, 0
		   prstate()
                   ENDCASE

    CASE f_mod:    cvf("MOD") // a := b MOD a; c := ?
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ecx,%%eax*n")
                   writef(" cdq*n")
                   writef(" idiv %%ebx*n")
                   writef(" movl %%edx,%%ebx*n")
                   Ak, Amem := 0, 0
		   prstate()
                   ENDCASE

    CASE f_add:    cvf("ADD") // a := b + a
                   moveB2y()
                   moveA2x()
                   writef(" addl %%ecx,%%ebx*n")
                   Ak, Amem := 0, 0
		   prstate()
                   ENDCASE

    CASE f_sub:    cvf("SUB") // a := b - a
                   moveB2y()
                   moveA2x()
                   writef(" subl %%ecx,%%ebx*n")
                   writef(" negl %%ebx")
                   Ak, Amem := 0, 0
		   prstate()
                   ENDCASE

    CASE f_eq:     cvf("EQ") // a := b = a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" seteb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   ENDCASE

    CASE f_ne:     cvf("NE") // a := b ~= a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" setneb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   ENDCASE

    CASE f_ls:     cvf("LS") // a := b < a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" setlb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   ENDCASE

    CASE f_gr:     cvf("GR") // a := b > a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" setgb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   ENDCASE

    CASE f_le:     cvf("LE") // a := b <= a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" setleb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   ENDCASE

    CASE f_ge:     cvf("GE") // a := b >= a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" setgeb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   ENDCASE

    CASE f_eq0:    cvf("EQ0") // a := a = 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" seteb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   ENDCASE

    CASE f_ne0:    cvf("NE0") // a := a ~= 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" setneb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   ENDCASE

    CASE f_ls0:    cvf("LS0") // a := a < 0
                   moveA2x()
                   writef(" sarl $31,%%ebx*n")
                   ENDCASE

    CASE f_gr0:    cvf("GR0") // a := a > 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" setgb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   ENDCASE

    CASE f_le0:    cvf("LE0") // a := a <= 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" setleb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   ENDCASE

    CASE f_ge0:    cvf("GE0") // a := a >= 0
                   moveA2x()
                   writef(" sarl $31,%%ebx*n")
                   writef(" notl %%ebx*n")
                   ENDCASE

    CASE f_lsh:    cvf("LSH") // a := b << a; b := ?
                   moveB2y()
                   moveA2x()
                   writef(" xchgl %%ebx,%%ecx*n")
                   writef(" cmpl $32,%%ecx*n")
                   writef(" sbbl %%eax,%%eax*n")  // set eax to -1 or 0
                   writef(" andl %%eax,%%ebx*n")  // set ebx to b or 0
                   writef(" sall %%cl,%%ebx*n")   // now shift it
                   Ak, Amem := 0, 0
                   Breg, Bk, Bmem := 0, 0, 0
		   prstate()
                   ENDCASE

    CASE f_rsh:    cvf("RSH") // a := b >> a; b := ?
                   moveB2y()
                   moveA2x()
                   writef(" xchgl %%ebx,%%ecx*n")
                   writef(" cmpl $32,%%ecx*n")
                   writef(" sbbl %%eax,%%eax*n")  // set eax to -1 or 0
                   writef(" andl %%eax,%%ebx*n")  // set ebx to b or 0
                   writef(" shrl %%cl,%%ebx*n")   // now shift it
                   Ak, Amem := 0, 0
                   Breg, Bk, Bmem := 0, 0, 0
                   prstate()
                   ENDCASE

    CASE f_and:    cvf("AND") // a := b & a 
                   moveB2y()
                   moveA2x()
                   writef(" andl %%ecx,%%ebx*n") 
                   Ak, Amem := 0, 0
                   prstate()
                   ENDCASE

    CASE f_or:     cvf("OR") // a := b | a 
                   moveB2y()
                   moveA2x()
                   writef(" orl %%ecx,%%ebx*n") 
                   Ak, Amem := 0, 0
                   prstate()
                   ENDCASE

    CASE f_xor:    cvf("XOR") // a := b NEQV a
                   moveB2y()
                   moveA2x()
                   writef(" xorl %%ecx,%%ebx*n") 
                   Ak, Amem := 0, 0
                   prstate()
                   ENDCASE

    CASE f_eqv:    cvf("EQV") // a := b EQV a 
                   moveB2y()
                   moveA2x()
                   writef(" xorl %%ecx,%%ebx*n") 
                   writef(" notl %%ebx*n") 
                   Ak, Amem := 0, 0
                   prstate()
                   ENDCASE

    CASE f_gbyt:   cvf("GBYT") // a := b % a
                   moveB2either()
                 { LET bstr = Breg=1 -> "%ebx", "%ecx"
                   TEST Ak=1
                   THEN { TEST Avalk
                          THEN writef(" movzbl %n(,%s,4),%%ebx*n", Avalk, bstr) 
                          ELSE writef(" movzbl (,%s,4),%%ebx*n", bstr)
                        }
                   ELSE { TEST Breg=1
                          THEN { moveA2y()
                                 writef(" movzbl (%%ecx,%%ebx,4),%%ebx*n")
                               }
                          ELSE { moveA2x()
                                 writef(" movzbl (%%ebx,%%ecx,4),%%ebx*n")
                               }
                        }
                   Areg, Ak, Amem := 1, 0, 0
                   IF Breg=1 DO Breg := 0
                   prstate()
                   ENDCASE
                 }

    CASE f_xgbyt:  cvf("XGBYT") // a := a % b 
                   moveA2x()
                   TEST Bk=1
                   THEN { TEST Bval
                          THEN writef(" movzbl %n(,%%ebx,4),%%ebx*n", Bval) 
                          ELSE writef(" movzbl (,%%ebx,4),%%ebx*n")
                        }
                   ELSE { moveB2y()
                          writef(" movzbl (%%ecx,%%ebx,4),%%ebx*n")
                        }
                   Areg, Ak, Amem := 1, 0, 0
                   prstate()
                   ENDCASE

    CASE f_pbyt:   cvf("PBYT") // b % a := c
                   moveB2y()
                   moveA2x()
                   writef(" movb %%dl,(%%ebx,%%ecx,4)*n") 
                   Amem := 0
                   Bmem := 0
                   prstate()
                   ENDCASE

    CASE f_xpbyt:  cvf("XPBYT") // a % b := c 
                   moveB2y()
                   moveA2x()
                   writef(" movb %%dl,(%%ecx,%%ebx,4)*n") 
                   Amem := 0
                   Bmem := 0
                   prstate()
                   ENDCASE

// swb       Kn Ld K1 L1 ... Kn Ln   Linary chop switch, Ld default
    CASE f_swb:    cvswb()
                   ENDCASE

// swl       Kn Ld L1 ... Ln         Label vector switch, Ld default
    CASE f_swl:    cvswl()
                   ENDCASE

    CASE f_xch:    cvf("XCH") // swap a and b
                 { LET reg, k, valk, mem, val = Areg, Ak, Avalk, Amem, Aval
                   Areg, Ak, Avalk, Amem, Aval := Breg, Bk, Bvalk, Bmem, Bval
                   Breg, Bk, Bvalk, Bmem, Bval :=  reg,  k,  valk,  mem,  val
                   ENDCASE
                 }

    CASE f_atb:    cvf("ATB") // B := A
                   IF Areg=1 DO
                   { TEST Breg=2
                     THEN { writef(" xchgl %%ebx,%%ecx*n")
                            Areg := 1
                          }
                     ELSE { writef(" movl %%ebx,%%ecx*n")
                          }
                     Breg, Bk, Bvalk, Bmem, Bval :=  2,  Ak, Avalk, Amem, Aval
                     prstate()
                     ENDCASE
                   }
                   moveA2y()
                   Areg := 2
                   ENDCASE

    CASE f_atc:    cvf("ATC") // c := a
                   moveA2x()
                   writef(" movl %%ebx,%%edx*n")
                   ENDCASE

    CASE f_bta:    cvf("BTA") // A := B
                   Areg, Ak, Avalk, Amem, Aval := Breg, Bk, Bvalk, Bmem, Bval
                   prstate()
                   ENDCASE

    CASE f_btc:    cvf("BTC") // c := b
                   moveB2y()
                   writef(" movl %%ecx,%%edx*n")
                   ENDCASE

    CASE f_atblp:  cvfp("ATBLP") // b := a; a := P!n
                   Breg, Bk, Bvalk, Bmem, Bval := Areg, Ak, Avalk, Amem, Aval
                   Areg, Ak, Amem, Aval := 0, 0, 1, pval
                   prstate()
                   ENDCASE

    CASE f_atblg:  cvfg("ATBLG") // b := a; a := G!n
                   Breg, Bk, Bvalk, Bmem, Bval := Areg, Ak, Avalk, Amem, Aval
                   Areg, Ak, Amem, Aval := 0, 0, 2, gval
                   prstate()
                   ENDCASE

    CASE f_atbl:   cvfk("ATBL") // b := a; a := k
                   Breg, Bk, Bvalk, Bmem, Bval := Areg, Ak, Avalk, Amem, Aval
                   Areg, Ak, Amem, Aval := 0, 1, 0, kval
                   prstate()
                   ENDCASE

    CASE f_j:      cvfl("J") // jump to Ln
                   writef(" jmp L%c%n*n", modletter, lval)
                   Areg, Ak, Amem := 0, 0, 0
                   Breg, Bk, Bmem := 0, 0, 0
                   //prstate()
                   ENDCASE

    CASE f_rtn:    cvf("RTN") // procedure return
                   // Load A popping esp if necessary
                   moveA2x()
                   writef(" movl 4(%%ebp),%%eax*n")
                   writef(" movl 0(%%ebp),%%ebp*n")
                   writef(" jmp **%%eax*n")
                   Areg, Ak, Amem := 0, 0, 0
                   Breg, Bk, Bmem := 0, 0, 0
                   //prstate()
                   ENDCASE

    CASE f_goto:   cvf("GOTO") // jump to a
                   moveA2x()
                   writef(" jmp **%%ebx*n")
                   Areg, Ak, Amem := 0, 0, 0
                   Breg, Bk, Bmem := 0, 0, 0
                   //prstate()
                   ENDCASE

    CASE f_res:    cvf("RES")   // <res> := A
                   // RES occurs just before the jump to a result label or
                   // the label at the end of a conditional expression.
                   // It also could be just before a conditional jump in
                   // a switchon command when B = the switch expression value
                   // and B holds a case constant.
                   moveA2x()
                   moveB2y()
                   ENDCASE

    CASE f_ldres:  cvf("LDRES") // A := <res>
                   // LDRES always occurs imediately after the label
                   // jumped to by RESULTIS or the jump in a conditional
                   // expression, when the result value is in A.
                   // It is also used in switches to specify B holds
                   // the switch value. 
                   Areg, Ak, Amem := 1, 0, 0
                   Breg, Bk, Bmem := 2, 0, 0
                   prstate()
                   ENDCASE

    CASE f_ikp:    cvfkp("IKP") // a := P!n + k; P!n := a
                   moveA2x()
                   writef(" movl %n(%%ebp),%%ebx*n", 4*pval)
                   TEST kval=1
                   THEN writef(" incl %%ebx*n")
                   ELSE TEST kval=-1
                        THEN writef(" decl %%ebx*n")
                        ELSE writef(" addl $%n,%%ebx*n", kval)
                   writef(" movl %%ebx,%n(%%ebp)*n", 4*pval)
                   Areg, Ak, Amem, Aval := 1, 0, 1, pval
		   Bmem := 0
                   prstate()
                   ENDCASE

    CASE f_ikg:    cvfkg("IKG") // a := G!n + k; G!n := a
                   moveA2x()
                   writef(" movl %n(%%esi),%%ebx*n", 4*gval)
                   TEST kval=1
                   THEN writef(" incl %%ebx*n")
                   ELSE TEST kval=-1
                        THEN writef(" decl %%ebx*n")
                        ELSE writef(" addl $%n,%%ebx*n", kval)
                   writef(" movl %%ebx,%n(%%esi)*n", 4*gval)
                   Areg, Ak, Amem, Aval := 1, 0, 2, gval
		   Bmem := 0
                   prstate()
                   ENDCASE

    CASE f_ikl:    cvfkl("IKL") // a := !Ln + k; !Ln := a
                   moveA2x()
                   writef(" movl L%c%n,%%ebx*n", modletter, lval)
                   TEST kval=1
                   THEN writef(" incl %%ebx*n")
                   ELSE TEST kval=-1
                        THEN writef(" decl %%ebx*n")
                        ELSE writef(" addl $%n,%%ebx*n", kval)
                   writef(" movl %%ebx,L%c%n*n", modletter, lval)
                   Areg, Ak, Amem, Aval := 1, 0, 4, lval
		   Bmem := 0
                   prstate()
                   ENDCASE

    CASE f_ip:     cvfp("IP") // a := P!n + a; P!n := a
                   moveA2x()
                   writef(" addl %n(%%ebp),%%ebx*n", 4*pval)
                   writef(" movl %%ebx,%n(%%ebp)*n", 4*pval)
                   Areg, Ak, Amem, Aval := 1, 0, 1, pval
		   Bmem := 0
                   prstate()
                   ENDCASE

    CASE f_ig:     cvfg("IG") // a := G!n + a; G!n := a
                   moveA2x()
                   writef(" addl %n(%%esi),%%ebx*n", 4*gval)
                   writef(" movl %%ebx,%n(%%esi)*n", 4*gval)
                   Areg, Ak, Amem, Aval := 1, 0, 2, gval
		   Bmem := 0
                   prstate()
                   ENDCASE

    CASE f_il:     cvfl("IL") // a := !Ln + a; !Ln := a
                   moveA2x()
                   writef(" addl L%c%n,%%ebx*n", modletter, lval)
                   writef(" movl %%ebx,L%c%n*n", modletter, lval)
                   Areg, Ak, Amem, Aval := 1, 0, 4, lval
		   Bmem := 0
                   prstate()
                   ENDCASE

    CASE f_jeq:    cvfl("JEQ") // Jump to Ln if b = a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" je L%c%n*n", modletter, lval)
                   writef("# Needs improvement*n")
                   ENDCASE

    CASE f_jne:    cvfl("JNE") // Jump to Ln if b ~= a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" jne L%c%n*n", modletter, lval)
                   writef("# Needs improvement*n")
                   ENDCASE

    CASE f_jls:    cvfl("JLS") // Jump to Ln if b < a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" jl L%c%n*n", modletter, lval)
                   writef("# Needs improvement*n")
                   ENDCASE

    CASE f_jgr:    cvfl("JGR") // Jump to Ln if B > A
                   moveAB2both() // Move one into x and
                                 // the other into y
                   wropregreg("cmpl", Areg, Breg)
                   writef(" jg L%c%n*n", modletter, lval)
                   ENDCASE

    CASE f_jle:    cvfl("JLE") // Jump to Ln if b <= a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" jle L%c%n*n", modletter, lval)
                   writef("# Needs improvement*n")
                   ENDCASE

    CASE f_jge:    cvfl("JGE") // Jump to Ln if b >= a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" jge L%c%n*n", modletter, lval)
                   writef("# Needs improvement*n")
                   ENDCASE

    CASE f_jeq0:   cvfl("JEQ0") // Jump to Ln if a = 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" je L%c%n*n", modletter, lval)
                   writef("# Needs improvement*n")
                   ENDCASE

    CASE f_jne0:   cvfl("JNE0") // Jump to Ln if a ~= 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" jne L%c%n*n", modletter, lval)
                   writef("# Needs improvement*n")
                   ENDCASE

    CASE f_jls0:   cvfl("JLS0") // Jump to Ln if a < 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" jl L%c%n*n", modletter, lval)
                   writef("# Needs improvement*n")
                   ENDCASE

    CASE f_jgr0:   cvfl("JGR0") // Jump to Ln if a > 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" jg L%c%n*n", modletter, lval)
                   writef("# Needs improvement*n")
                   ENDCASE

    CASE f_jle0:   cvfl("JLE0") // Jump to Ln if a <= 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" jle L%c%n*n", modletter, lval)
                   writef("# Needs improvement*n")
                   ENDCASE

    CASE f_jge0:   cvfl("JGE0") // Jump to Ln if a >= 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" jge L%c%n*n", modletter, lval)
                   writef("# Needs improvement*n")
                   ENDCASE

    CASE f_jge0m:  cvfm("JGE0M") // Jump to Mn if a >= 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" jge M%c%n*n", modletter, mval)
                   writef("# Needs improvement*n")
                   ENDCASE

    // The following five opcodes are never generated by
    // the BCPL compiler
    CASE f_brk:    cvf("BRK") // Breakpoint instruction
                   writef("*n# BRK not yet implemented*n")
                   ENDCASE

    CASE f_nop:    cvf("NOP") // No operation
                   ENDCASE

    CASE f_chgco:  cvf("CHGCO") // Change coroutine
                   writef("*n# CHGCO not yet implemented*n")
                   ENDCASE

    CASE f_mdiv:   cvf("MDIV") // a := Muldiv(P!3, P!4, P!5) 
                   writef("*n# MDIV not yet implemented*n")
                   ENDCASE

    CASE f_sys:    cvf("SYS") // System function
                   writef("*n# SYS not yet implemented*n")
                   ENDCASE

    CASE f_section:  cvfs("SECTION") // Name of section
                     FOR i = 0 TO charv%0 DO sectname%i := charv%i
                     Areg, Ak, Amem := 0, 0, 0
                     Breg, Bk, Bmem := 0, 0, 0
		     //prstate()
                     ENDCASE

    CASE f_modstart: cvf("MODSTART") // Start of module  
                     sectname%0 := 0
                     Areg, Ak, Amem := 0, 0, 0
                     Breg, Bk, Bmem := 0, 0, 0
		     //prstate()
                     ENDCASE

    CASE f_modend:   cvf("MODEND") // End of module 
                     modletter := modletter+1
                     ENDCASE

    CASE f_global:   cvglobal() // Global initialisation data
                     ENDCASE

    CASE f_string:   cvstring() // String constant
                     ENDCASE

    CASE f_const:    cvconst() // Large integer constant
                     ENDCASE

    CASE f_static:   cvstatic() // Static variable or table
                     ENDCASE

    CASE f_mlab:     cvfm("MLAB") // Destination of jge0m
                     writef("M%c%n:*n", modletter, mval)
                     Areg, Ak, Amem := 0, 0, 0
                     Breg, Bk, Bmem := 0, 0, 0
		     prstate()
                     ENDCASE

    CASE f_lab:      cvfl("LAB") // Program label
                     writef("*nL%c%n:*n", modletter, lval)
                     Areg, Ak, Amem := 0, 0, 0
                     Breg, Bk, Bmem := 0, 0, 0
		     prstate()
                     ENDCASE

    CASE f_lstr:     cvfm("LSTR") // a := Mn   (pointer to string)
                     IF Breg=1 & (Bk+Bmem)=0 DO
                     { // B is in A only so move it to y
                       writef(" movl %%ebx,%%ecx*n")
                       Breg := 2
                       prstate()
                     }
                     writef(" leal M%c%n,%%ebx*n", modletter, mval)
                     writef(" shrl $2,%%ebx*n")
                     Areg, Ak, Amem := 1, 0, 0
		     prstate()
                     ENDCASE

    CASE f_entry:    cventry() // Start of a function
                     ENDCASE

    CASE f_float:    cvf("FLOAT") // st[0] := FLOAT a
                     // Ensure B is not using st[0]
                     IF Breg=4 DO popS2mem(2, 11)
                     // Ensure A is Pn, Gn, Ln, Mn or T
                     moveA2mem()
                     wropmem("filds", Areg, Aval)
                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     ENDCASE

    CASE f_fix:      cvf("FIX") // A := FIX A
                     pushA2s()
                     writef(" fistpl 44(%%esi)*n")
                     Areg, Ak, Amem, Aval := 0, 0, 2, 11 // A is in G11
                     prstate()
                     writef("# UNCHECKED*n")
                     ENDCASE

    CASE f_fabs:     cvf("FABS") // A := #ABS A
                     pushA2s()
                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     ENDCASE

    CASE f_fmul:     cvf("FMUL") // A := B #* A; B := ?
                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     ENDCASE
		     /*
                     IF (bbits & b_s) > 0 DO
                     { moveA2mem()
                       wropmem("fmuls")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (abits & b_s) > 0 DO
                     { moveB2mem()
                       wropmem("fmuls")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (bbits & b_tpglm) > 0 DO
                     { wropmem("flds")
                       bbits := b_s
                       moveA2mem()
                       wropmem("fmuls")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (abits & b_tpglm) > 0 DO
                     { wropmem("flds")
                       abits := b_s
                       moveB2mem()
                       wropmem("fmuls")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     pushB2s()
                     bbits := b_s
                     moveA2mem()
                     wropmem("fmuls")
                     bbits, abits := 0, b_s
                     prstate()
                     ENDCASE
  */
  
    CASE f_fdiv:     cvf("FDIV") // A := B #/ A; B := ?
                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     ENDCASE
		     
                     //IF (bbits & b_s) > 0 DO
                     { moveA2mem()
                       wropmem("fdivs")
                       //bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     //IF (abits & b_s) > 0 DO
                     { moveB2mem()
                       wropmem("fdivrs")
                       //bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     //IF (bbits & b_tpglm) > 0 DO
                     { wropmem("flds")
                       //bbits := b_s
                       moveA2mem()
                       wropmem("fdivs")
                       //bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     //IF (abits & b_tpglm) > 0 DO
                     { wropmem("flds")
                       //abits := b_s
                       moveB2mem()
                       wropmem("fdivrs")
                       //bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     pushB2s()
                     //bbits := b_s
                     moveA2mem()
                     wropmem("fdivs")
                     //bbits, abits := 0, b_s
                     prstate()
                     ENDCASE
  
    CASE f_fxdiv:    cvf("FXDIV") // A := A #/ B; B := ?
                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     ENDCASE
		     
                     //IF (bbits & b_s) > 0 DO
                     { moveA2mem()
                       wropmem("fdivrs")
                       //bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     //IF (abits & b_s) > 0 DO
                     { moveB2mem()
                       wropmem("fdivs")
                       //bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     //IF (bbits & b_tpglm) > 0 DO
                     { wropmem("flds")
                       //bbits := b_s
                       moveA2mem()
                       wropmem("fdivrs")
                       //bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     //IF (abits & b_tpglm) > 0 DO
                     { wropmem("flds")
                       //abits := b_s
                       moveB2mem()
                       wropmem("fdivs")
                       //bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     pushB2s()
                     //bbits := b_s
                     moveA2mem()
                     wropmem("fdivrs")
                     //bbits, abits := 0, b_s
                     prstate()
                     ENDCASE
  
    CASE f_fmod:     cvf("FMOD")  // A := B #MOD A; B := ?
                     writef("*n# FMOD not yet implemented*n")
                     ENDCASE

    CASE f_fxmod:    cvf("XFMOD") // A := A #MOD B; B := ?
                     writef("*n# FXMOD not yet implemented*n")
                     ENDCASE


    CASE f_fadd:     cvf("FADD") // A := B #+ A; B := ?
                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     ENDCASE
		     
                     //IF (bbits & b_s) > 0 DO
                     { moveA2mem()
                       wropmem("fadds")
                       //bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     //IF (abits & b_s) > 0 DO
                     { moveB2mem()
                       wropmem("fadds")
                       //bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     //IF (bbits & b_tpglm) > 0 DO
                     { wropmem("flds")
                       //bbits := b_s
                       moveA2mem()
                       wropmem("fadds")
                       //bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     //IF (abits & b_tpglm) > 0 DO
                     { wropmem("flds")
                       //abits := b_s
                       moveB2mem()
                       wropmem("fadds")
                       //bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     pushB2s()
                     //bbits := b_s
                     moveA2mem()
                     wropmem("fadds")
                     //bbits, abits := 0, b_s
                     prstate()
                     ENDCASE
  
    CASE f_fsub:     cvf("FSUB") // A := B #- A; B := ?
                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     ENDCASE

                     //IF (bbits & b_s) > 0 DO
                     { moveA2mem()
                       wropmem("fsubs")
                       //bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     //IF (abits & b_s) > 0 DO
                     { //moveB2mem()
                       //wropmem("fsubrs")
                       //bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     //IF (bbits & b_tpglm) > 0 DO
                     { wropmem("flds")
                       //bbits := b_s
                       moveA2mem()
                       wropmem("fsubs")
                       //bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     //IF (abits & b_tpglm) > 0 DO
                     { wropmem("flds")
                       //abits := b_s
                       moveB2mem()
                       wropmem("fsubrs")
                       //bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     pushB2s()
                     //bbits := b_s
                     moveA2mem()
                     wropmem("fsubs")
                     //bbits, abits := 0, b_s
                     prstate()
                     ENDCASE
  
    CASE f_fxsub:    cvf("FXSUB") // A := A #- B; B := ?
                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     ENDCASE

    CASE f_fneg:     cvf("FNEG") // A := #- A
                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     ENDCASE

                     pushA2s()
                     writef(" fchs*n")          // st[0] := #- a
                     //abits := b_s
                     prstate()
                     ENDCASE

    CASE f_feq:      cvf("FEQ")
                     moveB2y()
                     moveA2x()
                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     ENDCASE


                     // Corrected code
                     // %ebx holds A and %ecx holds B
                     writef(" pushl %%ebx*n")
                     writef(" flds 44(%%esi)*n")  // st[0] := a
                     writef(" pushl %%ecx*n")
                     writef(" flds 44(%%esi)*n")  // st[0], st[1] = b, a
                     writef(" addl $8,%%esp*n")
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")

                     writef(" seteb %%bl*n")    // Convert result to TRUE or FALSE
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")

                     //abits := b_x
                     prstate()
                     ENDCASE

    CASE f_fne:      cvf("FNE")
                     moveB2y()
                     moveA2x()
                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     ENDCASE


                     // Corrected code
                     // %ebx holds A and %ecx holds B
                     writef(" pushl %%ebx*n")
                     writef(" flds 44(%%esi)*n")  // st[0] := a
                     writef(" pushl %%ecx*n")
                     writef(" flds 44(%%esi)*n")  // st[0], st[1] = b, a
                     writef(" addl $8,%%esp*n")
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")

                     writef(" setneb %%bl*n")
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")
                     //abits := b_x
                     prstate()
                     ENDCASE

    CASE f_fls:      cvf("FLS")
                     moveB2y()
                     moveA2x()
                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     ENDCASE


                     // Corrected code
                     // %ebx holds A and %ecx holds B
                     writef(" pushl %%ebx*n")
                     writef(" flds 44(%%esi)*n")  // st[0] := a
                     writef(" pushl %%ecx*n")
                     writef(" flds 44(%%esi)*n")  // st[0], st[1] = b, a
                     writef(" addl $8,%%esp*n")
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")

                     writef(" setb %%bl*n")
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")
                     //abits := b_x
                     prstate()
                     ENDCASE

    CASE f_fgr:      cvf("FGR")
                     moveB2y()
                     moveA2x()
                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     ENDCASE


                     // Corrected code
                     // %ebx holds A and %ecx holds B
                     writef(" pushl %%ebx*n")
                     writef(" flds 44(%%esi)*n")  // st[0] := a
                     writef(" pushl %%ecx*n")
                     writef(" flds 44(%%esi)*n")  // st[0], st[1] = b, a
                     writef(" addl $8,%%esp*n")
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")

                     writef(" seta %%bl*n")
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")
                     //abits := b_x
                     prstate()
                     ENDCASE

    CASE f_fle:      cvf("FLE")
                     moveB2y()
                     moveA2x()
                     writef(" pushl %%ecx*n")
                     writef(" flds 44(%%esi)*n")  // st[0] := b
                     writef(" pushl %%ebx*n")
                     writef(" flds 44(%%esi)*n")  // st[0], st[1] = a, b
                     writef(" addl $8,%%esp*n")
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" setae %%bl*n")
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")
                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_fge:      cvf("FGE")
                     moveB2y()
                     moveA2x()
                     writef(" pushl %%ebx*n")
                     writef(" flds 44(%%esi)*n")  // st[0] := a
                     writef(" pushl %%ecx*n")
                     writef(" flds 44(%%esi)*n")  // st[0], st[1] = b, a
                     writef(" addl $8,%%esp*n")
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")

                     writef(" setae %%bl*n")
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_feq0:     cvf("FEQ0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" seteb %%bl*n")
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_fne0:     cvf("FNE0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" setneb %%bl*n")
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")
                     //abits := b_x

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_fls0:     cvf("FLS0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" setab %%bl*n")
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_fgr0:     cvf("FGR0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" setbb %%bl*n")
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_fle0:     cvf("FLE0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" setaeb %%bl*n")
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_fge0:     cvf("FGE0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" setbeb %%bl*n")
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_jfeq:     cvfl("JFEQ")
                     moveB2y()
                     moveA2x()

                     // Corrected code
                     // %ebx holds A and %ecx holds B
                     writef(" pushl %%ebx*n")
                     writef(" flds 44(%%esi)*n")  // st[0] := a
                     writef(" pushl %%ecx*n")
                     writef(" flds 44(%%esi)*n")  // st[0], st[1] = b, a
                     writef(" addl $8,%%esp*n")
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")

                     writef(" je L%c%n*n", modletter, lval)


                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_jfne:     cvfl("JFNE")
                     moveB2y()
                     moveA2x()

                     // Corrected code
                     // %ebx holds A and %ecx holds B
                     writef(" pushl %%ebx*n")
                     writef(" flds 44(%%esi)*n")  // st[0] := a
                     writef(" pushl %%ecx*n")
                     writef(" flds 44(%%esi)*n")  // st[0], st[1] = b, a
                     writef(" addl $8,%%esp*n")
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")

                     writef(" jne L%c%n*n", modletter, lval)

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_jfls:     cvfl("JFLS")
                     moveB2y()
                     moveA2x()

                     // Corrected code
                     // %ebx holds A and %ecx holds B
                     writef(" pushl %%ebx*n")
                     writef(" flds 44(%%esi)*n")  // st[0] := a
                     writef(" pushl %%ecx*n")
                     writef(" flds 44(%%esi)*n")  // st[0], st[1] = b, a
                     writef(" addl $8,%%esp*n")
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")

                     writef(" jb L%c%n*n", modletter, lval)

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_jfgr:     cvfl("JFGR")
                     moveB2y()
                     moveA2x()

                     // Corrected code
                     // %ebx holds A and %ecx holds B
                     writef(" pushl %%ebx*n")
                     writef(" flds 44(%%esi)*n")  // st[0] := a
                     writef(" pushl %%ecx*n")
                     writef(" flds 44(%%esi)*n")  // st[0], st[1] = b, a
                     writef(" addl $8,%%esp*n")
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")

                     writef(" ja L%c%n*n", modletter, lval)

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_jfle:     cvfl("JFLE")
                     moveB2y()
                     moveA2x()

                     // Corrected code
                     // %ebx holds A and %ecx holds B
                     writef(" pushl %%ebx*n")
                     writef(" flds 44(%%esi)*n")  // st[0] := a
                     writef(" pushl %%ecx*n")
                     writef(" flds 44(%%esi)*n")  // st[0], st[1] = b, a
                     writef(" addl $8,%%esp*n")
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")

                     writef(" jbe L%c%n*n", modletter, lval)

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_jfge:     cvfl("JFGE")
                     moveB2y()
                     moveA2x()
                     writef(" pushl %%ecx*n")
                     writef(" flds 44(%%esi)*n")  // st[0] := b
                     writef(" pushl %%ebx*n")
                     writef(" flds 44(%%esi)*n")  // st[0], st[1] = a, b
                     writef(" addl $8,%%esp*n")
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" jae L%c%n*n", modletter, lval)

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_jfeq0:    cvfl("JFEQ0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" je L%c%n*n", modletter, lval)

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_jfne0:    cvfl("JFNE0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" jne L%c%n*n", modletter, lval)

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_jfls0:    cvfl("JFLS0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" ja L%c%n*n", modletter, lval)

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_jfgr0:    cvfl("JFGR0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" jb L%c%n*n", modletter, lval)

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_jfle0:    cvfl("JFLE0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" jae L%c%n*n", modletter, lval)
                     //bbits, abits := b_y, b_x
                     prstate()
                     ENDCASE

    CASE f_jfge0:    cvfl("JFGE0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" jbe L%c%n*n", modletter, lval)

                     Areg, Ak, Amem := 4, 0, 0
                     prstate()
                     writef("# UNCHECKED*n")
                     prstate()
                     ENDCASE

    CASE f_selld:    cvfkk("SELLD")
                     writef("*n# SELLD not yet implemented*n")
                     ENDCASE
    CASE f_selst:    cvffkk("SELST")
                     writef("*n# SELST not yet implemented*n")
                     ENDCASE
    CASE f_xselst:   cvffkk("XSELST")
                     writef("*n# XSELST not yet implemented*n")
                     ENDCASE
  }
} REPEAT

AND cvf(s)   BE { writef("# %s*n", s)
                } 
AND cvfp(s)  BE { //prstate()
                  writef("# %t7 P%n*n", s, rdp())
                } 
AND cvfkp(s) BE { //prstate()
                  writef("# %t7 K%n P%n*n", s, rdk(), rdp())
                } 
AND cvfg(s)  BE { //prstate()
                  writef("# %t7 G%n*n", s, rdg())
                } 
AND cvfkg(s) BE { //prstate()
                  writef("# %t7 K%n G%n*n", s, rdk(), rdg())
                } 
AND cvfkl(s) BE { //prstate()
                  writef("# %t7 K%n L%n*n", s, rdk(), rdl())
                } 
AND cvfpg(s) BE { //prstate()
                  writef("# %t7 P%n G%n*n", s, rdp(), rdg())
                } 
AND cvfk(s)  BE { //prstate()
                  writef("# %t7 K%n*n", s, rdk())
                } 
AND cvfw(s)  BE { //prstate()
                  writef("# %t7 W%n*n", s, rdw())
                } 
AND cvfl(s)  BE { //prstate()
                  writef("# %t7 L%n*n", s, rdl())
                } 
AND cvfm(s)  BE { //prstate()
                  writef("# %t7 M%n*n", s, rdm())
                } 

AND cvfkk(s) BE writef("%t7 K%n K%n", s, rdk(), rdk())

AND cvffkk(s) BE
{ LET f   = rdk()
  LET len = rdk()
  LET sh  = rdk()
  LET fs  = sfname(f)
  writef("%t7 %s K%n K%n", s, fs, len, sh)
}

AND cvswl() BE
{ LET n = rdk()
  LET l = rdl()
  LET lab = nextlab()
  prstate()
  writef("# SWL K%n L%n*n", n, l)
  moveA2x()
  writef(" orl %%ebx,%%ebx*n")
  writef(" jl L%c%n*n", modletter, l)
  writef(" cmpl $%n,%%ebx*n", n)
  writef(" jge L%c%n*n", modletter, l)
  writef(" jmp **L%n(,%%ebx,4)*n", lab)
  writef(" .data*n")
  writef(" .align 4*n")
  writef("L%n:*n", lab)
  FOR i = 1 TO n DO
  { writef("# L%n*n", rdl())
    writef(" .long L%c%n*n", modletter, lval)
  }
  writef(" .text*n")
  //bbits, abits := 0, 0
  prstate()
}

AND cvswb() BE
{ LET n = rdk()
  LET l = rdl()
  prstate()
  writef("# SWB K%n L%n*n", n, l)
  moveA2x()
  FOR i = 1 TO n DO 
  { LET k = rdk()
    LET l = rdl()
    writef("# K%n L%n*n", k, l)
    writef(" cmpl $%n,%%ebx*n", k)
    writef(" je L%c%n*n", modletter, l)
  }
  writef(" jmp L%c%n*n", modletter, l)
  //bbits, abits := 0, 0
  prstate()
}

AND cvglobal() BE
{ LET n = rdk()
  writef("# GLOBAL K%n*n", n)
  IF sectname%0=0 FOR i = 0 TO 4 DO sectname%i := "prog"%i
  writef(".globl %s*n", sectname)
  writef(".globl _%s*n", sectname)
  writef("%s:*n", sectname)
  writef("_%s:*n", sectname)
  writef(" movl 4(%%esp),%%eax*n")
  FOR i = 1 TO n DO
  { LET g = rdg()
    LET n = rdl()
    writef("# G%n L%n*n", g, n)
    writef(" movl $L%c%n,%n(%%eax)*n", modletter, n, 4*g)
  }
  writef("# G%n*n", rdg())
  writef(" ret*n")
}

AND rdchars() = VALOF
{ LET n = rdk()
  charv%0 := n
  FOR i = 1 TO n DO charv%i := rdc()
  RESULTIS n
}

AND cvstring() BE
{ LET lab = rdm()
  LET n = rdchars()
  writef("# STRING  M%n K%n", lab, n)
  FOR i = 1 TO n DO writef(" C%n", charv%i)
  writef("*n.data*n")
  writef(" .align 4*n")
  writef("M%c%n:*n", modletter, lab)
  FOR i = 0 TO n DO writef(" .byte %n*n", charv%i)
  writef(" .text*n")
}

AND cvconst() BE
{ LET lab = rdm()
  LET w = rdw()
  writef("# CONST   M%n W%n*n", lab, w)
  writef(".data*n")
  writef(" .align 4*n")
  writef("M%c%n:*n", modletter, lab)
  writef(" .long %n*n", w)
  writef(" .text*n")
}

AND cvstatic() BE
{ LET lab = rdl()
  LET n = rdk()
  writef("# STATIC  L%n K%n*n", lab, n)
  writef(".data*n")
  writef(" .align 4*n")
  writef("L%c%n:*n", modletter, lab)
  FOR i = 1 TO n DO { writef("# W%n*n", rdw())
                      writef(" .long %n*n", wval)
                    }
  writef(" .text*n")
}

AND cvfs(s) BE
{ LET n = rdchars()
  writef("# %t7 K%n", s, n)
  FOR i = 1 TO n DO writef(" C%n", charv%i)
  newline()
}

AND cventry() BE
{ LET n = rdchars()
  LET op = rdf()
  LET lab = rdl()
  writef("*n# Entry to: %s*n", charv)
  writef("# %t7 K%n", "ENTRY", n)
  FOR i = 1 TO n DO writef(" C%n", charv%i)
  newline()
  TEST op=f_lab THEN writef("# LAB     L%n*n", lab)
                ELSE writef("# cventry: Bad SIAL op F%n L%n*n", op, lab)
  writef("L%c%n:*n", modletter, lab)
  writef(" movl %%ebp,(%%edx)*n")    // NP!0 := P
  writef(" movl %%edx,%%ebp*n")      // P    := NP
  writef(" popl %%edx*n")
  writef(" movl %%edx,4(%%ebp)*n")   // P!1  := return address
  writef(" movl %%eax,8(%%ebp)*n")   // P!2  := entry address
  writef(" movl %%ebx,12(%%ebp)*n")  // P!3  := arg1
  Areg, Ak, Amem, Aval := 1, 0, 1, 3
  Breg, Bk, Bmem       := 0, 0, 0
  prstate()
}

AND sfname(sfop) = VALOF SWITCHON sfop INTO
{ DEFAULT:        RESULTIS "UNKNOWN"

  CASE 0:         RESULTIS "NULL"
  CASE sf_vecap:  RESULTIS "VECAP"
  CASE sf_fmul:   RESULTIS "FMUL"
  CASE sf_fdiv:   RESULTIS "FDIV"
  CASE sf_fmod:   RESULTIS "FMOD"
  CASE sf_fadd:   RESULTIS "FADD"
  CASE sf_fsub:   RESULTIS "FSUB"
  CASE sf_mul:    RESULTIS "MUL"
  CASE sf_div:    RESULTIS "DIV"
  CASE sf_mod:    RESULTIS "MOD"
  CASE sf_add:    RESULTIS "ADD"
  CASE sf_sub:    RESULTIS "SUB"
  CASE sf_lshift: RESULTIS "LSHIFT"
  CASE sf_rshift: RESULTIS "RSHIFT"
  CASE sf_logand: RESULTIS "LOGAND"
  CASE sf_logor:  RESULTIS "LOGOR"
  CASE sf_eqv:    RESULTIS "EQV"
  CASE sf_xor:    RESULTIS "XOR"
}
