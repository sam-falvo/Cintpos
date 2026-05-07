/*
This program compiles .sial files to .s files for the
Pentium pro and later Intel processors. The compiled code
will run on earlier processors provided no floating point
operations are compiled. It currently does not compile
the SELLD and SELST instructions so the .sial files must
currently be compiled with the NOSELST option.

Implemented by Martin Richards (c) January 2019

Change history

30/01/2019

Another change in register names. Sial registers are still A, B and C,
but the Pentium integer registers are x, y and z. The floating
registers st[0] and st[1] are s and r, and t is a temporary memory
location used when transfering values between x, y and z and the
floating point registers. x, y and z are actually %ebx, %ecx and %edx.
t is global variable 11 (called tempval) with address 44(%edi).

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

MANIFEST {
  // Bits used in abits and bbits
  b_x= #x001 // value is in register %ebx
  b_y= #x002 // Value is in register %ecx
  b_z= #x004 // Value is in register %edx
  b_t= #x008 // Value is in memory location T (G11) 44(%edi)
  b_s= #x010 // Value is in st[0] -- A floating point regigster
  b_r= #x020 // Value is in st[1] -- A floating point regigster
  b_p= #x040 // Value is in Pn -- where n is in apn or bpn
  b_g= #x080 // Value is in Gn -- where n is in agn or bgn
  b_l= #x100 // Value is in Ln -- where n is in aln or bln
  b_m= #x200 // Value is in Mn -- where n is in amn or bmn
  b_k= #x400 // Value is the constant bkn

  // Bit collections
  b_none=0
  b_all= b_x+b_y+b_t+b_s+b_r+b_p+b_g+b_l+b_m+b_k
  b_xy= b_x+b_y
  b_xt= b_x+b_t
  b_xp= b_x+b_p
  b_xg= b_x+b_g
  b_xl= b_x+b_l
  b_xm= b_x+b_m
  b_yt= b_y+b_t
  b_yp= b_y+b_p
  b_yg= b_y+b_g
  b_yl= b_y+b_l
  b_ym= b_y+b_m
  ts=b_t+b_s
  b_xbt= b_x+b_y+b_t
  b_xys= b_x+b_y+b_s
  b_xysr= b_x+b_y+b_s+b_r
  b_xbts0= b_x+b_y+b_t+b_s
  b_ts=b_t+b_s
  b_pglm=b_p+b_g+b_l+b_m
  b_tpglm=b_t+b_p+b_g+b_l+b_m

  bn_x= b_all - b_x
  bn_y= b_all - b_y
  bn_t= b_all - b_t
  bn_s= b_all - b_s
  bn_r= b_all - b_r
  bn_p= b_all - b_p
  bn_g= b_all - b_g
  bn_l= b_all - b_l
  bn_m= b_all - b_m
  bn_k= b_all - b_k
}

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

abits  // Bit pattern saying where the value of A can be found
bbits  // Bit pattern saying where the value of B can be found

apn    // n if abits contains b_p
agn    // n if abits contains b_g
aln    // n if abits contains b_l
amn    // n if abits contains b_m
akn    // n if abits contains b_k

bpn    // n if bbits contains b_p
bgn    // n if bbits contains b_g
bln    // n if bbits contains b_l
bmn    // n if bbits contains b_m
bkn    // n if bbits contains b_k

tracing  // =TRUE causes debugging info to be embedded in the
         // compiled assembly code file as comments
prstate

// New move functions
moveA2x    // Compile code to ensure A is in physical reg A
moveA2y    // Compile code to ensure A is in physical reg B
moveA2t    // Compile code to ensure A is in physical location T
pushA2s    // Compile code to push A into the ST stack

moveB2x    // Compile code to ensure B is in physical reg A
moveB2y    // Compile code to ensure B is in physical reg B
moveB2t    // Compile code to ensure B is in physical location T
pushB2s    // Compile code to push B into the ST stack

moveK2a    // Compile code to ensure const k is in physical reg A
moveK2b    // Compile code to ensure const k is in physical reg B
moveK2t    // Compile code to ensure const k is in physical location T

popS2a     // Compile code to pop st[0] to A
popS2b     // Compile code to pop st[0] to B
popS2t     // Compile code to pop st[0] to T

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
physical register Sial expects. The actual value of sial register is
held in one or more places specified by abits. The possible places are
the physical registers A an B, a memory location called T, a constant
k, or one of the floating point registers st[0] or st[1]. A similar
conventions applied to register B. The Pentium allows an st stack of 8
floating point numbers but this codegenerator only uses two
levels. The st stack can be empty and this must be the case at
function entry points and at the destination of jumps. Values can be
pushed onto the st stack or popped. Sometimes dyadic floating point
instructions take two operands stored in st[0] and st[1] leaving the
result in st[0]. This operation normally reduced the size of the stack
from two to one. If the value of A is in an addressible location, only
B must be pushed into the st stack since floating point add can take
an address as it operand.


If (abits & b_x)  > 0 then the value of A is in the A register.
If (abits & b_y)  > 0 then the value of A is in the B register.
If (abits & b_t)  > 0 then the value of A is in t (G11).
If (abits & b_s) > 0 then the value of A is in the floating point
                      register st[0].
If (abits & b_r) > 0 then the value of A is in the floating point
                      register st[1].
If (abits & b_p)  > 0 then the value of A is in the local variable
                      location addressed by P!n where n is held in apn.
If (abits & b_g) > 0 then the value of A is in the global variable
                     location addressed by G!n where n is held in agn.
If (abits & b_l) > 0 then the value of A is in the memory location
                     addressed by label Ln where n is held in aln.
If (abits & b_m) > 0 then the value of A is in the memory location
                     addressed by label Mn where n is held in amn.

In none of these bits are set in abits the the value of A is undefined.

The meaning of the bits in bbits is defined similarly.

The register T is actually global variable 11 declared as tempval in libhdr.h.

These state variables typically change when reading Sial statements or
generating machine instructions. These changes must be done with care
to ensure that the state is always consistent and that no information
is lost. For example, observe how the variables change during the
translation of the Sial code given above code.

 Sial          Code            State

 LP P3
                               B=?   A=P3
 ATBLP P4
                               B=P3  A=P4
 FADD
               flds 12(%ebp)
                               B=ST0P3  A=P4
               fadds 16(%ebp)
                               B=?  A=ST0     -- st[0] = P3 #+ P4
 SP P3
               fstps 12(%ebp)
                               B=?   A=P3

This shows that the LP P3 statement specifies that the value of A is
in local variable 3 (A=P3). We assume that the value of B at that
moment is unspecified. The instruction ATBLP P4 causes the value in A
to be moved to B before specifying that A is in local variable P4. The
statement FADD must compile code to perform the floating point
addition of P3 and P4. It does this by first pushing P3 onto the
floating point stack (flds 12(%ebp)). At this point B=ST0P3 stating
that the value of B is both in ST[0] and in local variable 3.  The
instruction fadds 16(%ebp) then performs the floating point addition
of local 4 whose address is 16(%ebp). The resulting state shows that A
holds the result in ST[0] and that B has become undefined.  Finally,
the statement SP P3, pops %st(0) from the floating point stack storing
X in local 3 (at address 12(%ebp)). A is now known to be in local 3
(A=P3) but not in ST[0] because the floating point stack has been
popped (by fstps).

To push a value from say A (%ebx) onto the floating point stack, it is
necessary to use a memory location. Often the memory location T (G11)
is used as in:

  push %ebx
  flds 44(%esi)

The only slight disadvantage is that G11 might be corrupted by any
floating point operation.

*/

LET trace(str, a, b, c) BE IF tracing DO
  writef(str, a, b, c)

LET start() = VALOF
{ LET argv = VEC 20
  LET v    = VEC 20
  LET cv   = VEC 256/bytesperword

  sectname := v
  sectname%0 := 0
  modletter := 'A'
  charv := cv
  labnumber := 0

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
  abits, bbits := 0, 0
  apn, agn, aln, amn, akn := 0, 0, 0, 0, 0
  bpn, bgn, bln, bmn, bkn := 0, 0, 0, 0, 0

  nextfcode := 0 // Initialise the F code peeking mechanism

  writef("# Code generated by sial-686*n*n")
  writef(".text*n.align 16*n")

  scan()

  endread()
  UNLESS asmout=stdout DO endwrite()
  selectoutput(stdout)
  writef("Conversion complete*n")
  RESULTIS 0
}

AND nextlab() = VALOF
{ labnumber := labnumber+1
  RESULTIS labnumber
}

AND rdcode(letter) = VALOF
{ // Read an Sial iten of the form <let>n
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

AND prstate() BE IF tracing DO
{ writef("# B=")
  IF bbits=0       DO wrch('?')
  IF (bbits&b_x)>0 DO wrch('x')
  IF (bbits&b_y)>0 DO wrch('y')
  IF (bbits&b_t)>0 DO wrch('t')
  IF (bbits&b_s)>0 DO writes("s")
  IF (bbits&b_r)>0 DO writes("r")
  IF (bbits&b_p)>0 DO writef("P%n", bpn)
  IF (bbits&b_g)>0 DO writef("G%n", bgn)
  IF (bbits&b_l)>0 DO writef("L%n", bln)
  IF (bbits&b_m)>0 DO writef("M%n", bmn)
  IF (bbits&b_k)>0 DO writef("K%n", bkn)

  writef("    A=")
  IF abits=0       DO wrch('?')
  IF (abits&b_x)>0 DO wrch('x')
  IF (abits&b_y)>0 DO wrch('y')
  IF (abits&b_t)>0 DO wrch('t')
  IF (abits&b_s)>0 DO writes("s")
  IF (abits&b_r)>0 DO writes("r")
  IF (abits&b_p)>0 DO writef("P%n", apn)
  IF (abits&b_g)>0 DO writef("G%n", agn)
  IF (abits&b_l)>0 DO writef("L%n", aln)
  IF (abits&b_m)>0 DO writef("M%n", amn)
  IF (abits&b_k)>0 DO writef("K%n", akn)

  newline()
}

AND moveA2y() BE
{ // Generate code for B := A
  trace("# moveA2y*n")
  bbits, bpn, bgn, bln, bmn, bkn := abits, apn, agn, aln, amn, akn
  prstate()
}

AND moveB2x() BE
{ // Generate code for A := B
  trace("# moveB2x*n")
  abits, apn, agn, aln, amn, akn := bbits, bpn, bgn, bln, bmn, bkn
  prstate()
}

AND freex() BE
{ trace("# freex*n")
  IF bbits = b_x DO
  { // B is only in x, so move it to y
    TEST (abits & b_y) > 0
    THEN { // Exchange x and y
           writef(" xchgl %%ebx,%%ecx*n")
           abits := b_x
           bbits := b_y
         }
    ELSE { // Move x to y
           writef(" movl %%ebx,%%ecx*n")
           bbits := b_y
         }
  }

  bbits := bbits & bn_x
  prstate()
}

AND moveK2x(k) BE
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

AND moveA2x() BE
{ // Compile code to ensure that A is in x

  trace("# moveA2x*n")

  freex()

  IF (abits & b_x) > 0 RETURN // A is already in x

  UNLESS abits DO
  { // A is undefined so give it the value already in x
    abits := b_x
    prstate()
    RETURN
  }

  IF (abits & b_y) > 0 DO
  { // A is in y so copy it to x
    writef(" movl %%ecx,%%ebx*n")
    abits := b_x
    prstate()
    RETURN
  }

  IF (abits & b_k) > 0 DO
  { moveK2x(akn)
    RETURN
  }

  // A is in T or a memory location

  IF (abits & b_t) > 0 DO
  { // A is in T so move it to A
    writef(" movl 44(%%esi),%%ebx*n")
    abits := abits | b_x
    prstate()
    RETURN
  }

  // A must be in a memory location
  genopamemreg("movl", "%ebx")
  abits := abits | b_x
  prstate()

  IF (abits & b_s) > 0 DO
  { // A is in S0 only so move it to A via T
    IF bbits = b_t DO
    { // We must preserve B so move it to B.
      // Note that st[0] is not in A
      writef(" movl 44(%%esi),%%ecx*n")
      bbits := b_y
      prstate()
    }

    // Now pop st[0] to T
    writef(" fstps 44(%%esi)*n")
    abits := b_t
    prstate()
  }
}

AND genB2x() BE
{ // Compile code to ensure that B is in x
  // and A is not in x
  // ie that (bbits & b_x) > 0 and
  // that (abits & b_x) = 0

  trace("# genB2A*n")

  IF abits = b_x DO
  { // A is in A only so move it to B
    TEST bbits = b_y
    THEN { // Exchange A and B
           writef(" xchgl %%ecx,%%ebx*n")
           bbits := b_x
         }
    ELSE { writef(" movl %%ecx,%%ebx*n")
         }
    abits := b_y
    prstate()
  }

  abits := abits & bn_x

  // A is not in A

  IF (bbits & b_x) > 0 RETURN

  UNLESS bbits DO
  { // B is undefined so give it the value in A
    bbits := b_x
    prstate()
    RETURN
  }

  IF (bbits & b_y) > 0 DO
  { // B is in B so move it to A
    writef(" movl %%ecx,%%ebx*n")
    bbits := b_x
    prstate()
    RETURN
  }

  IF bbits = b_s DO
  { // B is in st(0) only so move it to A via T
    IF abits = b_t DO
    { // We must preserve A so move it to B.
      // Note that B is not in A
      writef(" movl 44(%%esi),%%ecx*n")
      abits := b_y
      prstate()
    }

    // Now pop st[0] to T
    writef(" fstps 44(%%esi)*n")
    bbits := b_t
    prstate()
  }

  // B is in T or a memory location

  IF (bbits & b_t) > 0 DO
  { // B is in T so move it to A
    writef(" movl 44(%%esi),%%ebx*n")
    bbits := b_x
    prstate()
    RETURN
  }

  // A must be in a memory location
  genopamemreg("movl", "%ebx")
  abits := b_x
  prstate()
}

AND genA2B() BE
{ // Compile code to ensure that A is in physical register B
  // and B is not in B
  // ie that (abits & b_y) > 0 and
  // that (bbits & b_y) = 0

  trace("# moveA2y*n")

  IF bbits = b_y DO
  { // B is in B only so move it to A
    TEST abits = b_x
    THEN { // Exchange A and B
           writef(" xchgl %%ecx,%%ebx*n")
           abits := b_y
         }
    ELSE { writef(" movl %%ecx,%%ebx*n")
         }
    bbits := b_x
    prstate()
  }

  bbits := bbits & bn_y

  IF (abits & b_y) > 0 RETURN

  UNLESS abits DO
  { // A is undefined so give it the value in B
    abits := b_y
    prstate()
    RETURN
  }

  IF (abits & b_x) > 0 DO
  { // A is in A so move it to B
    writef(" movl %%ebx,%%ecx*n")
    abits := b_y
    prstate()
    RETURN
  }

  IF abits = b_s DO
  { // A is in st[0] only so move it to B via T
    IF bbits = b_t DO
    { // We must preserve B so move it to A.
      // Note that A is not in B
      writef(" movl 44(%%esi),%%ebx*n")
      bbits := b_x
      prstate()
    }

    // Now pop st[0] to T
    writef(" fstps 44(%%esi)*n")
    abits := b_t
    prstate()
  }

  // A is in T or a memory location

  IF (abits & b_t) > 0 DO
  { // A is in T so move it to B
    writef(" movl 44(%%esi),%%ecx*n")
    abits := abits | b_y
    prstate()
    RETURN
  }

  // A must be in a memory location
  genopamemreg("movl", "%ecx")
  abits := abits | b_y
  prstate()
}

AND moveB2y() BE
{ // Compile code to ensure that B is in B
  // and A is not in B
  // ie that (bbits & b_y) > 0 and
  // that (abits & b_y) = 0

  trace("# moveB2y*n")

  IF abits = b_y DO
  { // A is in B only so move it to A
    TEST bbits = b_x
    THEN { // Exchange A and B
           writef(" xchgl %%ecx,%%ebx*n")
           bbits := b_y
         }
    ELSE { writef(" movl %%ecx,%%ebx*n")
         }
    abits := b_x
    prstate()
  }

  abits := abits & bn_y

  IF (bbits & b_y) > 0 RETURN

  UNLESS bbits DO
  { // B is undefined so give it the value in B
    bbits := b_y
    prstate()
    RETURN
  }

  IF (bbits & b_x) > 0 DO
  { // B is in A so move it to B
    writef(" movl %%ebx,%%ecx*n")
    bbits := b_y
    prstate()
    RETURN
  }

  IF bbits = b_s DO
  { // B is in st[0] only so move it to B via T
    IF abits = b_t DO
    { // We must preserve A so move it to A.
      // Note that A is not in B
      writef(" movl 44(%%esi),%%ebx*n")
      abits := b_x
      prstate()
    }

    // Now pop st[0] to T
    writef(" fstps 44(%%esi)*n")
    bbits := b_t
    prstate()
  }

  // B is in T or a memory location

  IF (abits & b_t) > 0 DO
  { // B is in T so move it to B
    writef(" movl 44(%%esi),%%ecx*n")
    bbits := abits | b_y
    prstate()
    RETURN
  }

  // B must be in a memory location
  genopbmemreg("movl", "%ecx")
  bbits := bbits | b_y
  prstate()
}

AND pushA2T() BE
{ // Compile code to ensure that A is in T
  // and B is not in T
  // ie that (abits & b_t) > 0 and
  // that (bbits & b_t) = 0

  trace("# pushAtoS*n")

  // Test if A is already in T
  IF (abits & b_t) > 0 RETURN

  // First ensure B is not in T
  IF bbits = b_t DO
  { IF abits = b_s DO
    { // A is only in st[0] and B is only in T
      // so swap st[0] and T
      writef(" flds 44(%%esi)*n")
      writef(" fxch %%st(1),%%st*n")
      writef(" fstps 44(%%esi)*n")
      abits := abits XOR b_ts  // A moved to st[0]
      bbits := bbits XOR b_ts  // B moved to T
      prstate()
      RETURN
    }

    // Move B to A or B
    TEST (abits & b_y)=0
    THEN { // Move B to B
           writef(" movl 44(%%esi),%%ecx*n")
           bbits := bbits XOR b_yt
         }
    ELSE { // Move B to A
           writef(" movl 44(%%esi),%%ebx*n")
           bbits := bbits XOR b_xt
           abits := abits   & bn_y   // A in not in B
         }
    prstate()
  }

  // If B is in T it is also somewhere else
  bbits := bbits & bn_t
  prstate()

  // B is no longer in T

  IF (abits & b_t) > 0 RETURN

  IF (abits = b_s) > 0 DO
  { // A is in st[0] move it to T
    writef(" fstps 44(%%esi)*n")
    abits := abits XOR b_ts
    prstate()
    RETURN
  }

  UNLESS abits DO
  { // A is undefined so give it an arbitrary value
    abits := b_t
    prstate()
    RETURN
  }

  // If A is in memory (P, G, B or M) move it to T via A
  IF (abits & b_tpglm) > 0 DO moveA2x()

  IF (abits & b_x) > 0 DO
  { writef(" movl %%ebx,44(%%esi)*n")
    abits := abits XOR b_xt
    prstate()
    RETURN
  }

  IF (abits & b_y) > 0 DO
  { writef(" movl %%ecx,44(%%esi)*n")
    abits := abits XOR b_yt
    prstate()
    RETURN
  }

  IF (abits & b_s) > 0 DO
  { writef(" fstps 44(%%esi)*n")
    abits := abits XOR b_ts
    prstate()
    RETURN
  }

  writef("# moveA2t: system error  -- ERROR*n")
}

AND moveL2S() BE
{ // Compile code to ensure that B is in T
  // and A is not in T
  // ie that (bbits & b_t) > 0 and
  // that (abits & b_t) = 0

  trace("# moveL2S*n")

  IF (bbits & b_t) > 0 RETURN

  // First ensure A is not in T
  IF abits = b_t DO
  { IF bbits = b_s DO
    { // B is only in st[0] and A is only in T
      // so swap st[0] and T
      writef(" flds 44(%%esi)*n")
      writef(" fxch %%st(1),%%st*n")
      writef(" fstp 44(%%esi)*n")
      abits := abits XOR b_ts  // A moved to st[0]
      bbits := bbits XOR b_ts  // B moved to T
      prstate()
      RETURN
    }

    // Move A to A or B
    TEST (bbits & b_x)=0
    THEN { // Move A to A
           writef(" movl 44(%%esi),%%ebx*n")
           abits := abits XOR b_xt
         }
    ELSE { // Move A to B
           writef(" movl 44(%%esi),%%ecx*n")
           abits := abits XOR b_yt
           bbits := abits  & bn_y   // B in not in B
         }
    prstate()
  }

  // A is no longer in T

  IF (bbits & b_t) > 0 RETURN

  IF (bbits = b_s) > 0 DO
  { // B is in st[0] move it to T
    writef(" fstps 44(%%esi)*n")
    bbits := bbits XOR b_ts
    prstate()
    RETURN
  }

  UNLESS bbits DO
  { // B is undefined so give it an arbitrary value
    bbits := b_t
    prstate()
    RETURN
  }

  IF (bbits & b_tpglm) > 0 DO moveB2y()

  IF (bbits & b_y) > 0 DO
  { writef(" movl %%ecx,44(%%esi)*n")
    bbits := bbits XOR b_yt
    prstate()
    RETURN
  }

  IF (bbits & b_x) > 0 DO
  { writef(" movl %%ebx,44(%%esi)*n")
    bbits := bbits XOR b_xt
    prstate()
    RETURN
  }

  IF (bbits & b_s) > 0 DO
  { writef(" fstps 44(%%esi)*n")
    bbits := bbits XOR b_ts
    prstate()
    RETURN
  }

  writef("# moveL2S: system error  -- ERROR*n")
}

AND pushA2s() BE
{ // Compile code to ensure that A is in st[0]
  // and B is not in st[0]
  // ie that (abits & b_s) > 0 and
  // that (bbits & b_s) = 0

  trace("# pushA2s*n")

  // First ensure B is not in st[0]
  IF bbits = b_s DO
  { IF abits = b_t DO
    { // B is only in st[0] and A is only in T
      // so swap st[0] and T
      writef(" flds 44(%%esi)*n")
      writef(" fxch %%st(1),%%st*n")
      writef(" fstp 44(%%esi)*n")
      bbits := bbits XOR b_ts  // B moved to T
      abits := abits XOR b_ts  // A moved to st[0]
      prstate()
      RETURN
    }
    // Move st[0] to T
    writef(" fstp 44(%%esi)*n")
    bbits := bbits XOR b_ts  // B moved to T
    prstate()
  }

  bbits := bbits & bn_s

  // B is no longer in st[0]

  
  { // B is in st[0] only so move it to T
    IF (abits = b_t) > 0 DO
    { // A is in T so swap st[0] and T
      writef(" fstp 44(%%esi)*n")
      // B is now only in T
      bbits := b_t
      prstate()
    }
  }

  IF bbits = b_s & abits = b_t DO
  { // B is only in st[0] and A is only in T
    // so swap st[0] and T
    writef(" flds 44(%%esi)*n")
    writef(" fxch %%st(1),%%st*n")
    writef(" fstp 44(%%esi)*n")
    bbits := bbits XOR b_ts  // B copied to T
    abits := abits XOR b_ts  // A copied to st[0]
    prstate()
  }

  // B is no longer in st[0]

  UNLESS abits DO
  { // A is undefined so give it an arbitrary value
    writef(" fld1*n")
    abits := b_s
    prstate()
    RETURN
  }

  IF (abits & b_tpglm) > 0 DO
  { // A is in a memory location (TPGLM) so move A to st[0]
    genopamem("flds")
    abits := abits | b_s
    prstate()
    RETURN
  }

  // A is in A, B or st[0]
  IF (abits & b_s) > 0 RETURN

  // A must be in A or B so move it to st[0] via T

  IF bbits = b_t DO
  { // B is in T only so move T into A or B
    TEST abits = b_x
    THEN { // B is only in T and A is in A
           // so move B to B
           writef(" movl 44(%%esi),%%ecx*n")
           bbits := b_y           // B is in B
           abits := abits & bn_y  // A is not in B
         }
    ELSE { // B is in T only and A is in B
           // so move B to A
           writef(" movl 44(%%esi),%%ebx*n")
           bbits := b_x           // B is in A
           abits := abits & bn_x  // A is not in A
         }
    prstate()
  }

  // A is in A or B and B is not using T.

  TEST (abits & b_x) > 0
  THEN writef(" movl %%ebx,44(%%esi)*n")
  ELSE writef(" movl %%ecx,44(%%esi)*n")

  abits := abits | b_t
  prstate()
  writef(" flds 44(%%esi)*n")
  abits := abits | b_s  // A is in st[0]
  prstate()
}

AND mushB2S() BE
{ // Generate code to push B into T
  // ie (bbits & b_s) > 0 and
  // abits ~= b_s
  trace("# pushB2s*n")

  // First ensure A is not in st[0]
  IF abits = b_s DO
  { // A is in st[0] only
    IF (bbits & b_t) = 0 DO
    { // and B not in T -- move A to T
      writef(" fstp 44(%%esi)*n")
      // A is now only in T
      abits := b_t
      prstate()
    }
  }

  IF abits = b_s & bbits = b_t DO
  { // A is only in st[0] and B is only in T
    // so swap st[0] and T
    writef(" flds 44(%%esi)*n")
    writef(" fxch %%st(1),%%st*n")
    writef(" fstp 44(%%esi)*n")
    abits := abits XOR b_ts  // A copied to T
    bbits := bbits XOR b_ts  // B copied to st[0]
    prstate()
  }

  // A is no longer in st[0]

  UNLESS bbits DO
  { // B is undefined so push 0.0 into st[0]
    writef(" fld1*n")
    bbits := b_s
    prstate()
  }

  IF (bbits & b_s) > 0 RETURN

  // If B is in a memory location (TPGLM) push B to st[0]

  IF (bbits % b_tpglm) > 0 DO
  { genopbmem("flds")
    bbits := bbits | b_s
    prstate()
    RETURN
  }

  // B must be in A or B so push it into st[0] via T
  // First ensure A is not using T

  IF abits = b_t DO
  { TEST abits = b_x
    THEN { // A is only in T and B is in A
           // so move A to B
           writef(" movl 44(%%esi),%%ecx*n")
           abits := b_y           // A is in B
           bbits := bbits & bn_y  // B is not in B
         }
    ELSE { // A is only in T and B is in B
           // so move st[0] to A ###################
           writef(" movl 44(%%esi),%%ebx*n")
           abits := b_x           // A is in A
           bbits := bbits & bn_x  // B is not in A
         }
    prstate()
  }

  // B is in A or B and A is not using T.

  TEST (bbits & b_x) > 0
  THEN writef(" movl %%ebx,44(%%esi)*n")
  ELSE writef(" movl %%ecx,44(%%esi)*n")

  bbits := bbits | b_t
  prstate()
  writef(" flds 44(%%esi)*n")
  bbits := bbits | b_s  // B is in st[0]
  prstate()
}

AND moveA2xory() BE
{ // Ensure A is in x or y

  trace("# moveA2xory*n")

  IF (abits & b_xy) > 0 RETURN // A is already in x or y 

  moveA2x()
}

AND moveB2xory() BE
{ // Ensure B is in x or y

  trace("# moveB2xory*n")

  IF (bbits & b_xy) > 0 RETURN// B is already in x or y 

  moveB2y()
}

AND movex2P(n) BE
{ writef("# movex2P(%n)*n", n)

  IF (abits & b_x) > 0 &
     (abits & b_p) > 0 &
     apn=n RETURN          // x is already in Pn
  IF (bbits & b_x) > 0 &
     (bbits & b_p) > 0 &
     bpn=n RETURN          // x is already in Pn

  writef(" movl %%ebx,%n(%%ebp)*n", 4*n)
}

AND movex2G(n) BE
{ writef("# movex2G(%n)*n", n)

  IF (abits & b_x) > 0 &
     (abits & b_g) > 0 &
     agn=n RETURN          // x is already in Gn
  IF (bbits & b_x) > 0 &
     (bbits & b_g) > 0 &
     bgn=n RETURN          // x is already in Gn

  writef(" movl %%ebx,%n(%%edi)*n", 4*n)
}

AND movex2L(n) BE
{ writef("# movex2L(%n)*n", n)

  IF (abits & b_x) > 0 &
     (abits & b_l) > 0 &
     aln=n RETURN          // x is already in Ln
  IF (bbits & b_x) > 0 &
     (bbits & b_l) > 0 &
     bln=n RETURN          // x is already in Ln

  writef(" movl %%ebx,L%c%n*n", modletter, n)
}

AND movex2M(n) BE
{ writef("# movex2M(%n)*n", n)

  IF (abits & b_x) > 0 &
     (abits & b_m) > 0 &
     amn=n RETURN          // x is already in Mn
  IF (bbits & b_x) > 0 &
     (bbits & b_m) > 0 &
     bmn=n RETURN          // x is already in Mn

  writef(" movl %%ebx,M%c%n*n", modletter, n)
}

AND moveR2mem() BE
{ // Ensure A is in Pn, Gn, Ln, Mn or T
  trace("# moveR2mem*n")

  IF (abits & b_pglm) > 0 RETURN // Already im memory

  // A is not in memory so move it to T

  // First check B is not in T

  IF bbits = b_t DO moveB2y()
  bbits := bbits & bn_t
  prstate()

  moveA2t()
}

AND moveL2mem() BE
{ // Ensure B is in Pn, Gn, Ln, Mn or T
  trace("# moveL2mem*n")

  IF (bbits & b_pglm) > 0 RETURN // Already im memory

  // B is not in memory so move it to T

  // First check A is not in T

  IF abits = b_t DO moveA2x()
  abits := abits & bn_t
  prstate()

  moveL2S()
}

AND genaddra() BE
{ // abits must correspond to a memory address
  // ie must contain b_t, b_p, b_g, b_l or b_m

  IF (abits & b_t) > 0 DO
  { writef("44(%%esi)")
    RETURN
  }

  IF (abits & b_p) > 0 DO
  { writef("%n(%%ebp)", 4*apn)
    RETURN
  }

  IF (abits & b_g) > 0 DO
  { writef("%n(%%esi)", 4*agn)
    RETURN
  }

  IF (abits & b_l) > 0 DO
  { writef("L%c%n", modletter, aln)
    RETURN
  }

  IF (abits & b_m) > 0 DO
  { writef("M%c%n", modletter, amn)
    RETURN
  }

  writef("LA1")  // Some operand is required

  //sawritef("*n# SYSERROR: abits does not specify a memory location*n")
}

AND genaddrb() BE
{ // bbits must correspond to a memory address
  // ie must contain b_t, b_p, b_g, b_l or b_m

  IF (bbits & b_t) > 0 DO
  { writef("44(%%esi)")
    RETURN
  }

  IF (bbits & b_p) > 0 DO
  { writef("%n(%%ebp)", 4*bpn)
    RETURN
  }

  IF (bbits & b_g) > 0 DO
  { writef("%n(%%esi)", 4*bgn)
    RETURN
  }

  IF (bbits & b_l) > 0 DO
  { writef("L%c%n", modletter, bln)
    RETURN
  }

  IF (bbits & b_m) > 0 DO
  { writef("M%c%n", modletter, bmn)
    RETURN
  }

  writef("LA1")  // Some operand is required

//  sawritef("*n# SYSERROR: bbits does not specify a memory location*n")
}

AND genopamem(opstr) BE
{ writef(" %s ", opstr)
  genaddra()
  newline()
}

AND genopbmem(opstr) BE
{ writef(" %s ", opstr)
  genaddrb()
  newline()
}

AND genopamemreg(opstr, regstr) BE
{ writef(" %s ", opstr)
  genaddra()
  writef(",%s*n", regstr)
}

AND genopbmemreg(opstr, regstr) BE
{ writef(" %s ", opstr)
  genaddrb()
  writef(",%s*n", regstr)
}

AND genopregamem(opstr, regstr) BE
{ writef(" %s %n", opstr, regstr)
  genaddra()
  newline()
}

AND genopregbmem(opstr, regstr) BE
{ writef(" %s %n", opstr, regstr)
  genaddrb()
  newline()
}

AND scan() BE
{ LET op = rdf()

  SWITCHON op INTO

  { DEFAULT:       error("# Bad SIAL op %n*n", op); LOOP

    CASE -1:       RETURN
      
    CASE f_lp:     cvfp("LP") // A := P!n
                   // Specify that the value of A is now in Pn
                   abits, apn := b_p, pval
                   ENDCASE

    CASE f_lg:     cvfg("LG") // A := G!n
                   // Specify that the value of A is now in Gn
                   abits, apn := b_g, gval
                   ENDCASE

    CASE f_ll:     cvfl("LL") // A := !Ln
                   // Specify that the value of A is now in Ln
                   abits, aln := b_l, lval
                   ENDCASE

    CASE f_llp:    cvfp("LLP") // A := @ P!n
                   IF (bbits & b_x) > 0 DO
                   { writef(" movl %%ebx,%%ecx*n")
                     bbits := b_y
                     prstate()
                   }
                   writef(" leal %n(%%ebp),%%ebx*n", 4 * pval)
                   writef(" shrl $2,%%ebx*n")
                   abits := b_x
                   prstate()
                   ENDCASE

    CASE f_llg:    cvfg("LLG") // A := @ G!n
                   IF (bbits & b_x) > 0 DO
                   { writef(" movl %%ebx,%%ecx*n")
                     bbits := b_y
                     prstate()
                   }
                   writef(" leal %n(%%edx),%%ebx*n", 4 * gval)
                   writef(" shrl $2,%%ebx*n")
                   abits := b_x
                   prstate()
                   ENDCASE

    CASE f_lll:    cvfl("LLL") // A := @ !Ln
                   IF (bbits & b_x) > 0 DO
                   { writef(" movl %%ebx,%%ecx*n")
                     bbits := b_y
                     prstate()
                   }
                   writef(" leal L%c%n,%%ebx*n", modletter, lval)
                   writef(" shrl $2,%%ebx*n")
                   abits := b_x
                   prstate()
                   ENDCASE

    CASE f_lf:     cvfl("LF") // A := byte address of Ln
                   IF (bbits & b_x) > 0 DO
                   { writef(" movl %%ebx,%%ecx*n")
                     bbits := b_y
                     prstate()
                   }
                   writef(" leal L%c%n,%%ebx*n", modletter, lval)
                   abits := b_x
                   prstate()
                   ENDCASE

    CASE f_lw:     cvfm("LW") // A := Mn
                   // Specify that the value of A is now in Mn
                   abits, amn := b_m, mval
                   ENDCASE

    CASE f_l:      cvfk("L") // A := n
                   IF bbits = b_x DO
                   { // B is only in x, so move it to y
                     writef(" movl %%ebx,%%ecx*n")
                     bbits := b_y
                     prstate()
                   }
                   //TEST kval
                   //THEN writef(" movl $%n,%%ebx*n", kval)
                   //ELSE writef(" xorl %%ebx,%%ebx*n")
                   abits, akn := b_k, kval
                   prstate()
                   ENDCASE

    CASE f_lm:     cvfk("LM") // a := -n
                   IF (bbits & b_x) > 0 DO
                   { writef(" movl %%ebx,%%ecx*n")
                     bbits := b_y
                     prstate()
                   }
                   TEST kval
                   THEN writef(" movl $-%n,%%ebx*n", kval)
                   ELSE writef(" xorl %%ebx,%%ebx*n")
                   abits := b_x
                   prstate()
                   ENDCASE

    CASE f_sp:     cvfp("SP") // P!n := A
                   IF bbits=b_p & bpn=pval DO moveB2xory()

                   moveA2xory() // Move A into A, B or st[0]
                   IF (abits & b_s) > 0 DO
                   { writef("fstps %n(%%ebp)*n", 4*pval)
                     abits, apn := b_p, pval
                     bbits := bbits & bn_p
                     ENDCASE
                   }
                   TEST (abits & b_x) > 0
                   THEN { writef("movl %%ebx,%n(%%ebp)*n", 4*pval)
                          abits, apn := b_xp, pval
                        }
                   ELSE { writef("movl %%ecx,%n(%%ebp)*n", 4*pval)
                          abits, apn := b_yp, pval
                        }
                   bbits := bbits & bn_p
                   ENDCASE

    CASE f_sg:     cvfg("SG") // G!n := A
                   IF bbits=b_g & bgn=gval DO moveB2xory()

                   moveA2xory() // Move A into A, B or st[0]

                   IF (abits & b_s) > 0 DO
                   { writef("fstps %n(%%esi)*n", 4*gval)
                     abits, agn := b_g, gval
                     bbits := bbits & bn_g
                     ENDCASE
                   }
                   TEST (abits & b_x) > 0
                   THEN { writef("movl %%ebx,%n(%%esi)*n", gval)
                          abits, agn := b_xg, gval
                        }
                   ELSE { writef("movl %%ecx,%n(%%esi)*n", gval)
                          abits, agn := b_yg, gval
                        }
                   bbits := bbits & bn_g
                   ENDCASE

    CASE f_sl:     cvfl("SL") // !Ln := a
                   moveA2x()
                   writef("movl %%ebx,L%c%n*n", modletter, lval)
                   abits, aln := b_xl, lval
                   bbits := bbits & bn_l
                   prstate()
                   ENDCASE

    CASE f_ap:     cvfp("AP") // a := a + P!n
                   moveA2x()
                   writef(" addl %n(%%ebp),%%ebx*n", 4*pval)
                   abits := b_x
                   prstate() 
                   ENDCASE

    CASE f_ag:     cvfg("AG") // a := a + G!n
                   moveA2x()
                   writef(" addl %n(%%esi),%%ebx*n", 4*gval)
                   abits := b_x
                   prstate() 
                   ENDCASE

    CASE f_a:      cvfk("A") // a := a + n
                   moveA2x()
                   abits := b_x 
                   IF kval=0 ENDCASE
                   IF kval=1  DO { writef(" incl %%ebx*n"); ENDCASE }
                   IF kval=-1 DO { writef(" decl %%ebx*n"); ENDCASE }
                   writef(" addl $%n,%%ebx*n", kval)
                   ENDCASE

    CASE f_s:      cvfk("S")  // a := a - n
                   moveA2x()
                   abits := b_x 
                   IF kval=0 ENDCASE
                   IF kval=1  DO { writef(" decl %%ebx*n"); ENDCASE }
                   IF kval=-1 DO { writef(" incl %%ebx*n"); ENDCASE }
                   writef(" subl $%n,%%ebx*n", kval)
                   ENDCASE

    CASE f_lkp:    cvfkp("LKP") // a := P!n!k
                   moveA2x()
                   writef(" movl %n(%%ebp),%%eax*n", 4*pval)
                   writef(" movl %n(,%%eax,4),%%ebx*n", 4*kval)
                   abits := b_x
                   ENDCASE

    CASE f_lkg:    cvfkg("LKG") // a := G!n!k
                   moveA2x()
                   writef(" movl %n(%%esi),%%eax*n", 4*gval)
                   writef(" movl %n(,%%eax,4),%%ebx*n", 4*kval)
                   abits := b_x
                   ENDCASE

    CASE f_rv:     cvf("RV")  // a := ! a
                   moveA2x()
                   writef(" movl (,%%ebx,4),%%ebx*n")
                   bbits, abits := bbits & bn_x, b_x
                   ENDCASE

    CASE f_rvp:    cvfp("RVP") // a := P!n!a
                   moveA2x()
                   writef(" addl %n(%%ebp),%%ebx*n", 4*pval)
                   writef(" movl (,%%ebx,4),%%ebx*n")
                   bbits, abits := bbits & bn_x, b_x
                   ENDCASE

    CASE f_rvk:    cvfk("RVK") // a := a!k
                   moveA2x()
                   writef(" movl %n(,%%ebx,4),%%ebx*n", 4*kval)
                   bbits, abits := bbits & bn_x, b_x
                   ENDCASE

    CASE f_st:     cvf("ST") // !a := b
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ecx,(,%%ebx,4)*n")
                   abits := abits & b_xysr
                   bbits := bbits & b_xysr
                   ENDCASE

    CASE f_stp:    cvfp("STP") // P!n!a := b
                   moveB2y()
                   moveA2x()
                   writef(" movl %n(%%ebp),%%eax*n", 4*pval)
                   writef(" addl %%ebx,%%eax*n")
                   writef(" movl %%ecx,(,%%eax,4)*n")
                   abits := abits & b_xysr
                   bbits := bbits & b_xysr
                   ENDCASE

    CASE f_stk:    cvfk("STK") // a!n := b
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ecx,%n(,%%ebx,4)*n", 4*kval)
                   abits := abits & b_xysr
                   bbits := bbits & b_xysr
                   ENDCASE

    CASE f_stkp:   cvfkp("STKP")  // P!n!k := a
                   moveA2x()
                   writef(" movl %n(%%ebp),%%eax*n", 4*pval)
                   writef(" movl %%ebx,%n(,%%eax,4)*n", 4*kval)
                   abits := 0
                   bbits := 0
                   ENDCASE

    CASE f_skg:    cvfkg("SKG") // G!n!k := a
                   moveA2x()
                   writef(" movl %n(%%esi),%%eax*n", 4*gval)
                   writef(" movl %%ebx,%n(,%%eax,4)*n", 4*kval)
                   abits := abits & b_xysr
                   bbits := bbits & b_xysr
                   ENDCASE

    CASE f_xst:    cvf("XST") // !b := a
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ebx,(,%%ecx,4)*n")
                   abits := abits & b_xysr
                   bbits := bbits & b_xysr
                   ENDCASE

    CASE f_k:      cvfp("K") // Call  a(b,...) incrementing P by n
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ebx,%%eax*n")
                   writef(" movl %%ecx,%%ebx*n")
                   writef(" leal %n(%%ebp),%%edx*n", 4*pval)
                   writef(" call **%%eax*n")
                   bbits, abits := 0, b_x
                   ENDCASE

    CASE f_kpg:    cvfpg("KPG") // Call Gg(a,...) incrementing P by n
                   moveA2x()
                   writef(" movl %n(%%esi),%%eax*n", 4*gval)
                   writef(" leal %n(%%ebp),%%edx*n", 4*pval)
                   writef(" call **%%eax*n")
                   bbits, abits := 0, b_x
                   ENDCASE

    CASE f_neg:    cvf("NEG") // a := - a
                   moveA2x()
                   writef(" negl %%ebx*n") 
                   abits := b_x
                   ENDCASE

    CASE f_not:    cvf("NOT") // a := ~ a
                   moveA2x()
                   writef(" notl %%ebx*n") 
                   abits := b_x
                   ENDCASE

    CASE f_abs:    cvf("ABS") // a := ABS a
                   moveA2x()
                 { LET l = nextlab()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" jge L%n*n", l)
                   writef(" negl %%ebx*n")
                   writef("L%n:*n", l)
                   abits := b_x
                   ENDCASE
                 }

    CASE f_xdiv:   cvf("XDIV") // a := a / b
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ebx,%%eax*n")
                   writef(" cdq*n")
                   writef(" idiv %%ecx*n")
                   writef(" movl %%eax,%%ebx*n")
                   abits := b_x
                   ENDCASE

    CASE f_xmod:   cvf("XMOD") // a := a MOD b
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ebx,%%eax*n")
                   writef(" cdq*n")
                   writef(" idiv %%ecx*n")
                   writef(" movl %%edx,%%ebx*n")
                   abits := b_x
                   ENDCASE

    CASE f_xsub:   cvf("XSUB") // a := a - b
                   moveB2y()
                   moveA2x()
                   writef(" subl %%ecx,%%ebx*n")
                   abits := b_x
                   ENDCASE

    CASE f_mul:    cvf("MUL") // a := b * a; c := ?
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ecx,%%eax*n")
                   writef(" imul %%ebx*n") // currupts edx
                   writef(" movl %%eax,%%ebx*n")
                   abits := b_x
                   ENDCASE

    CASE f_div:    cvf("DIV")  // a := b / a; c := ?
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ecx,%%eax*n")
                   writef(" cdq*n")
                   writef(" idiv %%ebx*n")
                   writef(" movl %%eax,%%ebx*n")
                   abits := b_x
                   ENDCASE

    CASE f_mod:    cvf("MOD") // a := b MOD a; c := ?
                   moveB2y()
                   moveA2x()
                   writef(" movl %%ecx,%%eax*n")
                   writef(" cdq*n")
                   writef(" idiv %%ebx*n")
                   writef(" movl %%edx,%%ebx*n")
                   abits := b_x
                   ENDCASE

    CASE f_add:    cvf("ADD") // a := b + a
                   moveB2y()
                   moveA2x()
                   writef(" addl %%ecx,%%ebx*n")
                   abits := b_x
                   ENDCASE

    CASE f_sub:    cvf("SUB") // a := b - a
                   moveB2y()
                   moveA2x()
                   writef(" subl %%ecx,%%ebx*n")
                   writef(" negl %%ebx")
                   abits := b_x
                   ENDCASE

    CASE f_eq:     cvf("EQ") // a := b = a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" seteb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   abits := b_x 
                   bbits := b_y 
                   ENDCASE

    CASE f_ne:     cvf("NE") // a := b ~= a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" setneb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   abits := b_x 
                   bbits := b_y 
                   ENDCASE

    CASE f_ls:     cvf("LS") // a := b < a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" setlb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   abits := b_x 
                   bbits := b_y 
                   ENDCASE

    CASE f_gr:     cvf("GR") // a := b > a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" setgb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   abits := b_x 
                   bbits := b_y 
                   ENDCASE

    CASE f_le:     cvf("LE") // a := b <= a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" setleb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   abits := b_x 
                   bbits := b_y 
                   ENDCASE

    CASE f_ge:     cvf("GE") // a := b >= a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" setgeb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   abits := b_x 
                   bbits := b_y 
                   ENDCASE

    CASE f_eq0:    cvf("EQ0") // a := a = 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" seteb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   abits := b_x 
                   ENDCASE

    CASE f_ne0:    cvf("NE0") // a := a ~= 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" setneb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   abits := b_x 
                   ENDCASE

    CASE f_ls0:    cvf("LS0") // a := a < 0
                   moveA2x()
                   writef(" sarl $31,%%ebx*n")
                   abits := b_x 
                   ENDCASE

    CASE f_gr0:    cvf("GR0") // a := a > 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" setgb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   abits := b_x 
                   ENDCASE

    CASE f_le0:    cvf("LE0") // a := a <= 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" setleb %%bl*n")
                   writef(" movzbl %%bl,%%ebx*n")
                   writef(" negl %%ebx*n")
                   abits := b_x 
                   ENDCASE

    CASE f_ge0:    cvf("GE0") // a := a >= 0
                   moveA2x()
                   writef(" sarl $31,%%ebx*n")
                   writef(" notl %%ebx*n")
                   abits := b_x 
                   ENDCASE

    CASE f_lsh:    cvf("LSH") // a := b << a; b := ?
                   moveB2y()
                   moveA2x()
                   writef(" xchgl %%ebx,%%ecx*n")
                   writef(" cmpl $32,%%ecx*n")
                   writef(" sbbl %%eax,%%eax*n")  // set eax to -1 or 0
                   writef(" andl %%eax,%%ebx*n")  // set ebx to b or 0
                   writef(" sall %%cl,%%ebx*n")   // now shift it
                   abits := b_x 
                   bbits := 0 
                   ENDCASE

    CASE f_rsh:    cvf("RSH") // a := b >> a; b := ?
                   moveB2y()
                   moveA2x()
                   writef(" xchgl %%ebx,%%ecx*n")
                   writef(" cmpl $32,%%ecx*n")
                   writef(" sbbl %%eax,%%eax*n")  // set eax to -1 or 0
                   writef(" andl %%eax,%%ebx*n")  // set ebx to b or 0
                   writef(" shrl %%cl,%%ebx*n")   // now shift it
                   abits := b_x 
                   bbits := 0 
                   prstate()
                   ENDCASE

    CASE f_and:    cvf("AND") // a := b & a 
                   moveB2y()
                   moveA2x()
                   writef(" andl %%ecx,%%ebx*n") 
                   abits := b_x 
                   prstate()
                   ENDCASE

    CASE f_or:     cvf("OR") // a := b | a 
                   moveB2y()
                   moveA2x()
                   writef(" orl %%ecx,%%ebx*n") 
                   abits := b_x 
                   prstate()
                   ENDCASE

    CASE f_xor:    cvf("XOR") // a := b NEQV a
                   moveB2y()
                   moveA2x()
                   writef(" xorl %%ecx,%%ebx*n") 
                   abits := b_x 
                   prstate()
                   ENDCASE

    CASE f_eqv:    cvf("EQV") // a := b EQV a 
                   moveB2y()
                   moveA2x()
                   writef(" xorl %%ecx,%%ebx*n") 
                   writef(" notl %%ebx*n") 
                   abits := b_x 
                   prstate()
                   ENDCASE

    CASE f_gbyt:   cvf("GBYT") // a := b % a
                   moveB2y()
                   TEST (abits & b_k)>0
                   THEN { TEST akn
                          THEN writef(" movzbl %n(,%%ecx,4),%%ebx*n", akn) 
                          ELSE writef(" movzbl (,%%ecx,4),%%ebx*n")
                        }
                   ELSE { moveA2x()
                          writef(" movzbl (%%ebx,%%ecx,4),%%ebx*n")
                        }
                   abits := b_x // A is now in x
                   prstate()
                   ENDCASE

    CASE f_xgbyt:  cvf("XGBYT") // a := a % b 
                   moveA2x()
                   TEST (bbits & b_k)>0
                   THEN { TEST bkn
                          THEN writef(" movzbl %n(,%%ebx,4),%%ebx*n", bkn) 
                          ELSE writef(" movzbl (,%%ebx,4),%%ebx*n")
                        }
                   ELSE { moveB2y()
                          writef(" movzbl (%%ecx,%%ebx,4),%%ebx*n")
                        }
                   abits := b_x // A is now in x
                   prstate()
                   ENDCASE

    CASE f_pbyt:   cvf("PBYT") // b % a := c
                   moveB2y()
                   moveA2x()
                   writef(" movb %%dl,(%%ebx,%%ecx,4)*n") 
                   abits := abits & b_xysr
                   bbits := bbits & b_xysr
                   prstate()
                   ENDCASE

    CASE f_xpbyt:  cvf("XPBYT") // a % b := c 
                   moveB2y()
                   moveA2x()
                   writef(" movb %%dl,(%%ecx,%%ebx,4)*n") 
                   abits := abits & b_xysr
                   bbits := bbits & b_xysr
                   prstate()
                   ENDCASE

// swb       Kn Ld K1 L1 ... Kn Ln   Linary chop switch, Ld default
    CASE f_swb:    cvswb()
                   ENDCASE

// swl       Kn Ld L1 ... Ln         Label vector switch, Ld default
    CASE f_swl:    cvswl()
                   ENDCASE

    CASE f_xch:    cvf("XCH") // swap a and b
                 { LET bits, p, g, l, m, k = abits, apn, agn, aln, amn, akn
                   abits, apn, agn, aln, amn, akn := bbits, bpn, bgn, bln, bmn, bkn
                   bbits, bpn, bgn, bln, bmn, bkn :=  bits,  p,   g,   l,   m,   k
                   ENDCASE
                 }

    CASE f_atb:    cvf("ATB") // B := A
                   IF (abits & b_x) > 0 DO
                   { TEST (bbits & b_y) > 0
                     THEN { writef(" xchgl %%ebx,%%ecx*n")
                            abits := b_x
                          }
                     ELSE { writef(" movl %%ebx,%%ecx*n")
                          }
                     bbits := b_y
                     prstate()
                     ENDCASE
                   }
                   moveA2y()
                   bbits := b_y
                   ENDCASE

    CASE f_atc:    cvf("ATC") // c := a
                   moveA2x()
                   writef(" movl %%ebx,%%edx*n")
                   ENDCASE

    CASE f_bta:    cvf("BTA") // A := B
                   abits, apn, agn, aln, amn, akn := bbits, bpn, bgn, bln, bmn, bkn
                   prstate()
                   ENDCASE

    CASE f_btc:    cvf("BTC") // c := b
                   moveB2y()
                   writef(" movl %%ecx,%%edx*n")
                   ENDCASE

    CASE f_atblp:  cvfp("ATBLP") // b := a; a := P!n
                   bbits, bpn, bgn, bln, bmn, bkn := abits, apn, agn, aln, amn, akn
                   abits, apn := b_p, pval
                   prstate()
                   ENDCASE

    CASE f_atblg:  cvfg("ATBLG") // b := a; a := G!n
                   bbits, bpn, bgn, bln, bmn, bkn := abits, apn, agn, aln, amn, akn
                   abits, agn := b_g, gval
                   ENDCASE

    CASE f_atbl:   cvfk("ATBL") // b := a; a := k
                   bbits, bpn, bgn, bln, bmn, bkn := abits, apn, agn, aln, amn, akn
                   abits, apn, agn, aln, amn, akn := b_k, 0, 0, 0, 0, kval
                   prstate()
                   ENDCASE

    CASE f_j:      cvfl("J") // jump to Ln
                   writef(" jmp L%c%n*n", modletter, lval)
                   bbits, abits := 0, 0
                   prstate()
                   ENDCASE

    CASE f_rtn:    cvf("RTN") // procedure return
                   // Load A popping esp if necessary
                   moveA2x()
                   writef(" movl 4(%%ebp),%%eax*n")
                   writef(" movl 0(%%ebp),%%ebp*n")
                   writef(" jmp **%%eax*n")
                   bbits, abits := 0, 0
                   prstate()
                   ENDCASE

    CASE f_goto:   cvf("GOTO") // jump to a
                   moveA2x()
                   writef(" jmp **%%ebx*n")
                   bbits, abits := 0, 0
                   prstate()
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
                   bbits, abits := b_y, b_x
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
                   abits, apn := b_xp, pval
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
                   abits, agn := b_xg, gval
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
                   abits, aln := b_xl, lval
                   prstate()
                   ENDCASE

    CASE f_ip:     cvfp("IP") // a := P!n + a; P!n := a
                   moveA2x()
                   writef(" addl %n(%%ebp),%%ebx*n", 4*pval)
                   writef(" movl %%ebx,%n(%%ebp)*n", 4*pval)
                   abits, apn := b_xp, pval
                   prstate()
                   ENDCASE

    CASE f_ig:     cvfg("IG") // a := G!n + a; G!n := a
                   moveA2x()
                   writef(" addl %n(%%esi),%%ebx*n", 4*gval)
                   writef(" movl %%ebx,%n(%%esi)*n", 4*gval)
                   abits, agn := b_xg, gval
                   prstate()
                   ENDCASE

    CASE f_il:     cvfl("IL") // a := !Ln + a; !Ln := a
                   moveA2x()
                   writef(" addl L%c%n,%%ebx*n", modletter, lval)
                   writef(" movl %%ebx,L%c%n*n", modletter, lval)
                   abits, aln := b_xl, lval
                   prstate()
                   ENDCASE

    CASE f_jeq:    cvfl("JEQ") // Jump to Ln if b = a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" je L%c%n*n", modletter, lval)
                   ENDCASE

    CASE f_jne:    cvfl("JNE") // Jump to Ln if b ~= a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" jne L%c%n*n", modletter, lval)
                   ENDCASE

    CASE f_jls:    cvfl("JLS") // Jump to Ln if b < a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" jl L%c%n*n", modletter, lval)
                   ENDCASE

    CASE f_jgr:    cvfl("JGR") // Jump to Ln if b > a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" jg L%c%n*n", modletter, lval)
                   ENDCASE

    CASE f_jle:    cvfl("JLE") // Jump to Ln if b <= a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" jle L%c%n*n", modletter, lval)
                   ENDCASE

    CASE f_jge:    cvfl("JGE") // Jump to Ln if b >= a
                   moveB2y()
                   moveA2x()
                   writef(" cmpl %%ebx,%%ecx*n")
                   writef(" jge L%c%n*n", modletter, lval)
                   ENDCASE

    CASE f_jeq0:   cvfl("JEQ0") // Jump to Ln if a = 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" je L%c%n*n", modletter, lval)
                   ENDCASE

    CASE f_jne0:   cvfl("JNE0") // Jump to Ln if a ~= 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" jne L%c%n*n", modletter, lval)
                   ENDCASE

    CASE f_jls0:   cvfl("JLS0") // Jump to Ln if a < 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" jl L%c%n*n", modletter, lval)
                   ENDCASE

    CASE f_jgr0:   cvfl("JGR0") // Jump to Ln if a > 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" jg L%c%n*n", modletter, lval)
                   ENDCASE

    CASE f_jle0:   cvfl("JLE0") // Jump to Ln if a <= 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" jle L%c%n*n", modletter, lval)
                   ENDCASE

    CASE f_jge0:   cvfl("JGE0") // Jump to Ln if a >= 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" jge L%c%n*n", modletter, lval)
                   ENDCASE

    CASE f_jge0m:  cvfm("JGE0M") // Jump to Mn if a >= 0
                   moveA2x()
                   writef(" orl %%ebx,%%ebx*n")
                   writef(" jge M%c%n*n", modletter, mval)
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
                     bbits, abits := 0, 0
                     ENDCASE

    CASE f_modstart: cvf("MODSTART") // Start of module  
                     sectname%0 := 0
                     bbits, abits := 0, 0
                     ENDCASE

    CASE f_modend:   cvf("MODEND") // End of module 
                     modletter := modletter+1
                     bbits, abits := 0, 0
                     ENDCASE

    CASE f_global:   cvglobal() // Global initialisation data
                     bbits, abits := 0, 0
                     ENDCASE

    CASE f_string:   cvstring() // String constant
                     ENDCASE

    CASE f_const:    cvconst() // Large integer constant
                     ENDCASE

    CASE f_static:   cvstatic() // Static variable or table
                     ENDCASE

    CASE f_mlab:     cvfm("MLAB") // Destination of jge0m
                     writef("M%c%n:*n", modletter, mval)
                     bbits, abits := 0, 0
                     prstate()
                     ENDCASE

    CASE f_lab:      cvfl("LAB") // Program label
                     writef("*nL%c%n:*n", modletter, lval)
                     bbits, abits := 0, 0
                     prstate()
                     ENDCASE

    CASE f_lstr:     cvfm("LSTR") // a := Mn   (pointer to string)
                     IF bbits = b_x DO
                     { // B is in A only so move it to B
                       writef(" movl %%ebx,%%ecx*n")
                       bbits := b_y
                       prstate()
                     }
                     writef(" leal M%c%n,%%ebx*n", modletter, mval)
                     writef(" shrl $2,%%ebx*n")
                     bbits, abits := bbits & bn_x, b_x
                     prstate()
                     ENDCASE

    CASE f_entry:    cventry() // Start of a function
                     ENDCASE

    CASE f_float:    cvf("FLOAT")
                     // Ensure B is not using st[0]
                     IF bbits = b_s DO popS2t()
                     // Ensure A is Pn, Gn, Ln, Mn or T
                     moveR2mem()
                     genopamem("filds") // st[0] := FLOAT a
                     abits := b_s
                     ENDCASE

    CASE f_fix:      cvf("FIX") // a := FIX a
                     pushA2s()
                     writef(" fistpl 44(%%esi)*n")
                     abits := b_t
                     ENDCASE

    CASE f_fabs:     cvf("FABS") // A := #ABS A
                     pushA2s()
                     writef(" fabs*n")
                     abits := b_s
                     prstate()
                     ENDCASE

    CASE f_fmul:     cvf("FMUL") // A := B #* A; B := ?

                     IF (bbits & b_s) > 0 DO
                     { moveR2mem()
                       genopamem("fmuls")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (abits & b_s) > 0 DO
                     { moveL2mem()
                       genopbmem("fmuls")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (bbits & b_tpglm) > 0 DO
                     { genopbmem("flds")
                       bbits := b_s
                       moveR2mem()
                       genopamem("fmuls")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (abits & b_tpglm) > 0 DO
                     { genopamem("flds")
                       abits := b_s
                       moveL2mem()
                       genopbmem("fmuls")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     pushB2s()
                     bbits := b_s
                     moveR2mem()
                     genopamem("fmuls")
                     bbits, abits := 0, b_s
                     prstate()
                     ENDCASE
  
    CASE f_fdiv:     cvf("FDIV") // A := B #/ A; B := ?

                     IF (bbits & b_s) > 0 DO
                     { moveR2mem()
                       genopamem("fdivs")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (abits & b_s) > 0 DO
                     { moveL2mem()
                       genopbmem("fdivrs")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (bbits & b_tpglm) > 0 DO
                     { genopbmem("flds")
                       bbits := b_s
                       moveR2mem()
                       genopamem("fdivs")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (abits & b_tpglm) > 0 DO
                     { genopamem("flds")
                       abits := b_s
                       moveL2mem()
                       genopbmem("fdivrs")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     pushB2s()
                     bbits := b_s
                     moveR2mem()
                     genopamem("fdivs")
                     bbits, abits := 0, b_s
                     prstate()
                     ENDCASE
  
    CASE f_fxdiv:    cvf("FXDIV") // A := A #/ B; B := ?

                     IF (bbits & b_s) > 0 DO
                     { moveR2mem()
                       genopamem("fdivrs")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (abits & b_s) > 0 DO
                     { moveL2mem()
                       genopbmem("fdivs")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (bbits & b_tpglm) > 0 DO
                     { genopbmem("flds")
                       bbits := b_s
                       moveR2mem()
                       genopamem("fdivrs")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (abits & b_tpglm) > 0 DO
                     { genopamem("flds")
                       abits := b_s
                       moveL2mem()
                       genopbmem("fdivs")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     pushB2s()
                     bbits := b_s
                     moveR2mem()
                     genopamem("fdivrs")
                     bbits, abits := 0, b_s
                     prstate()
                     ENDCASE
  
    CASE f_fmod:     cvf("FMOD")  // A := B #MOD A; B := ?
                     writef("*n# FMOD not yet implemented*n")
                     ENDCASE

    CASE f_fxmod:    cvf("XFMOD") // A := A #MOD B; B := ?
                     writef("*n# FXMOD not yet implemented*n")
                     ENDCASE


    CASE f_fadd:     cvf("FADD") // A := B #+ A; B := ?

                     IF (bbits & b_s) > 0 DO
                     { moveR2mem()
                       genopamem("fadds")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (abits & b_s) > 0 DO
                     { moveL2mem()
                       genopbmem("fadds")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (bbits & b_tpglm) > 0 DO
                     { genopbmem("flds")
                       bbits := b_s
                       moveR2mem()
                       genopamem("fadds")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (abits & b_tpglm) > 0 DO
                     { genopamem("flds")
                       abits := b_s
                       moveL2mem()
                       genopbmem("fadds")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     pushB2s()
                     bbits := b_s
                     moveR2mem()
                     genopamem("fadds")
                     bbits, abits := 0, b_s
                     prstate()
                     ENDCASE
  
    CASE f_fsub:     cvf("FSUB") // A := B #- A; B := ?

                     IF (bbits & b_s) > 0 DO
                     { moveR2mem()
                       genopamem("fsubs")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (abits & b_s) > 0 DO
                     { moveL2mem()
                       genopbmem("fsubrs")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (bbits & b_tpglm) > 0 DO
                     { genopbmem("flds")
                       bbits := b_s
                       moveR2mem()
                       genopamem("fsubs")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     IF (abits & b_tpglm) > 0 DO
                     { genopamem("flds")
                       abits := b_s
                       moveL2mem()
                       genopbmem("fsubrs")
                       bbits, abits := 0, b_s
                       prstate()
                       ENDCASE
                     }

                     pushB2s()
                     bbits := b_s
                     moveR2mem()
                     genopamem("fsubs")
                     bbits, abits := 0, b_s
                     prstate()
                     ENDCASE
  
    CASE f_fxsub:    cvf("FXSUB") // A := A #- B; B := ?
                     writef("*n# FXSUB not yet implemented*n")
                     ENDCASE

    CASE f_fneg:     cvf("FNEG") // A := #- A
                     pushA2s()
                     writef(" fchs*n")          // st[0] := #- a
                     abits := b_s
                     prstate()
                     ENDCASE

    CASE f_feq:      cvf("FEQ")
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

                     writef(" seteb %%bl*n")    // Convert result to TRUE or FALSE
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")

                     abits := b_x
                     prstate()
                     ENDCASE

    CASE f_fne:      cvf("FNE")
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

                     writef(" setneb %%bl*n")
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")
                     abits := b_x
                     prstate()
                     ENDCASE

    CASE f_fls:      cvf("FLS")
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

                     writef(" setb %%bl*n")
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")
                     abits := b_x
                     prstate()
                     ENDCASE

    CASE f_fgr:      cvf("FGR")
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

                     writef(" seta %%bl*n")
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")
                     abits := b_x
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
                     abits := b_x
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
                     abits := b_x
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
                     abits := b_x
                     ENDCASE

    CASE f_fne0:     cvf("FNE0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" setneb %%bl*n")
                     writef(" movzbl %%bl,%%ebx*n")
                     writef(" negl %%ebx*n")
                     abits := b_x
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
                     abits := b_x
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
                     abits := b_x
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
                     abits := b_x
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
                     abits := b_x
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
                     bbits, abits := b_y, b_x
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
                     bbits, abits := b_y, b_x
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
                     bbits, abits := b_y, b_x
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
                     bbits, abits := b_y, b_x
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
                     bbits, abits := b_y, b_x
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
                     bbits, abits := b_y, b_x
                     prstate()
                     ENDCASE

    CASE f_jfeq0:    cvfl("JFEQ0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" je L%c%n*n", modletter, lval)
                     bbits, abits := b_y, b_x
                     prstate()
                     ENDCASE

    CASE f_jfne0:    cvfl("JFNE0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" jne L%c%n*n", modletter, lval)
                     bbits, abits := b_y, b_x
                     prstate()
                     ENDCASE

    CASE f_jfls0:    cvfl("JFLS0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" ja L%c%n*n", modletter, lval)
                     bbits, abits := b_y, b_x
                     prstate()
                     ENDCASE

    CASE f_jfgr0:    cvfl("JFGR0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" jb L%c%n*n", modletter, lval)
                     bbits, abits := b_y, b_x
                     prstate()
                     ENDCASE

    CASE f_jfle0:    cvfl("JFLE0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" jae L%c%n*n", modletter, lval)
                     bbits, abits := b_y, b_x
                     prstate()
                     ENDCASE

    CASE f_jfge0:    cvfl("JFGE0")
                     pushA2s()
                     writef(" fldz*n")        // st[0], st[1] = 0.0, a
                     writef(" fucomip %%st(1),%%st*n")
                     writef(" fstp %%st*n")
                     writef(" jbe L%c%n*n", modletter, lval)
                     bbits, abits := b_y, b_x
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

AND cvf(s)   BE { prstate()
                  writef("# %s*n", s)
                } 
AND cvfp(s)  BE { prstate()
                  writef("# %t7 P%n*n", s, rdp())
                } 
AND cvfkp(s) BE { prstate()
                  writef("# %t7 K%n P%n*n", s, rdk(), rdp())
                } 
AND cvfg(s)  BE { prstate()
                  writef("# %t7 G%n*n", s, rdg())
                } 
AND cvfkg(s) BE { prstate()
                  writef("# %t7 K%n G%n*n", s, rdk(), rdg())
                } 
AND cvfkl(s) BE { prstate()
                  writef("# %t7 K%n L%n*n", s, rdk(), rdl())
                } 
AND cvfpg(s) BE { prstate()
                  writef("# %t7 P%n G%n*n", s, rdp(), rdg())
                } 
AND cvfk(s)  BE { prstate()
                  writef("# %t7 K%n*n", s, rdk())
                } 
AND cvfw(s)  BE { prstate()
                  writef("# %t7 W%n*n", s, rdw())
                } 
AND cvfl(s)  BE { prstate()
                  writef("# %t7 L%n*n", s, rdl())
                } 
AND cvfm(s)  BE { prstate()
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
  bbits, abits := 0, 0
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
  bbits, abits := 0, 0
  prstate()
}

AND cvglobal() BE
{ LET n = rdk()
  moveB2y()
  moveA2x()
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
  bbits := 0
  abits, apn := b_xp, 3
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
