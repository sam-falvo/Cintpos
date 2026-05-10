/*

This is a program to compute the fast Fourier transform of 512 values
using all 128 of the processor cores in the loki chip. This
implementation cheats by representing the cores as coroutines with
with bodies written in BCPL. The channels are also simplified and
implemented using coroutines.

Implemented by Martin Richards (c) 25 March 2019


This is version 2 of the lokifft program. The main coroutine fftco
creates a coroutine for each core. Each holds information relating to
its core such its channel buffers. Each core coroutine creates four
element coroutines that perform the calculations needed for four
elements of the Fourrier transform. Having created these, the
core coroutine returns control to fftco and plays no further part in
the algorithm until the end when fftco gives it control causing it to
delete it element coroutines.

All element coroutines are initially left suspended in lowait().  Each
knows the FFT algorithm and issuspended in lowait() waiting to start
the first step of the algorithm. At the end of each step it call
lowait() again. When all have been suspended in lowait() schedule
transfers them all to hilist where they will start to execute the next
step. The steps are as follows.

1) Copy the test data from datav to the element coroutines.

2) Copy data from the element coroutine to datav (as a debugging aid).

3} Perform the perfect shuffle.

4) Perform a step of butterfly operations. Repeated ln times.

5) Copy data from the element coroutine to datav.

6} Perform the perfect shuffle.

7) Perform a step of butterfly operations using the inverse Nth root
   of unity. Repeated ln times.

8) Divide all the element values by N.

9) Copy data from the element coroutine to datav.

The main coroutine sets ln, N and w before calling lowait() for the
first time.

After step 2, 5 and 9 it outputs datav and after step 5 it replaces w
by the inverse Nth root of unity. Before each butterfly step it sets
sep and k appropriately.

Finally it deletes all the coroutines and returns all allocated space
to free store.


The FFT computation used in this demonstration.

This is illustrated here for the FFT of 8 values, but is easily
generalised for larger problems.

We use xi and wi to denote x^i and w^i,
but ai is just the ith element of data.

f(x) is a polynomial of degree 7 and its Fourier transform is the set
of 8 values f(w0), f(w1),..., f(w7) where w is the smallest 8th root
of unity. The fft algorithm is based on the following observation:

f(x) = a0 + a1*x1 + a2*x2 + a3*x3 + a4*x4 + a5*x5 + a6*x6 + a7*x7
     = f0246(x2) + x * f1357(x2)
where  f0246(x) = a0 + a2*x1 + a4*x2 + a6*x3
and    f1357(x) = a1 + a3*x1 + a5*x2 + a7*x3

Similar rules apply to the polynomials f04, f26, f15 and f37

Since w is the 8th root of unity we have w8=1 and w4=-1.  We also have
other results, such as w4=-w0, w5=-w1, w6=-w2, w8=w0, w12=w4, and
w14=w6.

To calculate f(wi) for i = 0 .. 7, we perform the following
computations.

f(w0) = f0246(w0)  + w0 * f1357(w0)  = f0246(w0) + w0 * f1357(w0)
f(w1) = f0246(w2)  + w1 * f1357(w2)  = f0246(w2) + w1 * f1357(w2)
f(w2) = f0246(w4)  + w2 * f1357(w4)  = f0246(w4) + w2 * f1357(w4)
f(w3) = f0246(w6)  + w3 * f1357(w6)  = f0246(w6) + w3 * f1357(w6)
f(w4) = f0246(w8)  + w4 * f1357(w8)  = f0246(w0) - w0 * f1357(w0)
f(w5) = f0246(w10) + w5 * f1357(w10) = f0246(w2) - w1 * f1357(w2)
f(w6) = f0246(w12) + w6 * f1357(w12) = f0246(w4) - w2 * f1357(w4)
f(w7) = f0246(w14) + w7 * f1357(w14) = f0246(w6) - w3 * f1357(w6)

f0246(w0) = f04(w0)  + w0 * f26(w0)  = f04(w0) + w0 * f26(w0)
f0246(w2) = f04(w4)  + w2 * f26(w4)  = f04(w4) + w2 * f26(w4)
f0246(w4) = f04(w8)  + w4 * f26(w8)  = f04(w0) - w0 * f26(w0)
f0246(w6) = f04(w12) + w6 * f26(w12) = f04(w4) - w2 * f26(w4)
f1357(w0) = f15(w0)  + w0 * f37(w0)  = f15(w0) + w0 * f37(w0)
f1357(w2) = f15(w4)  + w2 * f37(w4)  = f15(w4) + w2 * f37(w4)
f1357(w4) = f15(w8)  + w4 * f37(w8)  = f15(w0) - w0 * f37(w0)
f1357(w6) = f15(w12) + w6 * f37(w12) = f15(w4) - w2 * f37(w4)

f04(w0) = a0 + w0 * a4 = a0 + a4
f04(w4) = a0 + w4 * a4 = a0 - a4
f26(w0) = a2 + w0 * a6 = a2 + a6
f26(w4) = a2 + w4 * a6 = a2 - a6
f15(w0) = a1 + w0 * a5 = a1 + a5
f15(w4) = a1 + w4 * a5 = a1 - a5
f37(w0) = a3 + w0 * a7 = a3 + a7
f37(w4) = a3 + w4 * a7 = a3 - a7

This computation working on a simple set of test data can be
represented diagramically as follows.

 0       1       2       3       4       5       6       7
 a0      a1      a2      a3      a4      a5      a6      a7   Level 0
 |       |       |       |       |       |       |       |
 .       .       .       .       .       .       .       .
 |        \      |        \     /        |      /        |
 |         \     |         \   /         |     /         |
 |          \    |          \ /          |    /          |
 |           \   |           .           |   /           |
 |            \  |          / \          |  /            |
 |             \ |         /   \         | /             |   Perfect shuffle
 |              \|        /     \        |/              |
 |               .       /       \       .               |   ie swap ap and aq
 |               |\     /         \     /|               |
 |               | \   /           \   / |               |   where the digits
 |               |  \ /             \ /  |               |
 |               |   .               .   |               |   of q are those of
 |               |  / \             / \  |               |
 |               | /   \           /   \ |               |   p in reverse order.
 |               |/     \         /     \|               |
 |               .       \       /       .               |   eg p=110  q=011
 |              /|        \     /        |\              |
 |             / |         \   /         | \             |
 |            /  |          \ /          |  \            |
 |           /   |           .           |   \           |
 |          /    |          / \          |    \          |
 |         /     |         /   \         |     \         |
 |        /      |        /     \        |      \        |
 .       .       .       .       .       .       .       .
 0       4       2       6       1       5       3       7
 a0      a4      a2      a6      a1      a5      a3      a7
 |       |       |       |       |       |       |       |
 .       *w0     .       *w0     .       *w0     .       *w0  w0 = 1
 |\     /|       |\     /|       |\     /|       |\     /|
 | \   / |       | \   / |       | \   / |       | \   / |    sep=1 k=3=ln
 |  \ /  |       |  \ /  |       |  \ /  |       |  \ /  |    01
 |   .   |       |   .   |       |   .   |       |   .   |    23
 |  / \  |       |  / \  |       |  / \  |       |  / \  |    45
 | /   \ |       | /   \ |       | /   \ |       | /   \ |    67
 |/     \|       |/     \|       |/     \|       |/     \|       
 +       -       +       -       +       -       +       -   *w4=*-w0=*-1
 4     65533     8     65533     6     65533    10     65533
f04     f04     f26     f26     f15     f15     f37     f37
(w0)    (w4)    (w0)    (w4)    (w0)    (w4)    (w0)    (w4)
 |       |       |       |       |       |       |       |
 .       .       *w0     *w2     .       .       *w0     *w2  w0=1 w2=256
 |\      |\     /|      /|       |\      |\     /|      /|
 | \     | \   / |     / |       | \     | \   / |     / |    sep=2 k=2
 |  \    |  \ /  |    /  |       |  \    |  \ /  |    /  |
 |   \   |   .   |   /   |       |   \   |   .   |   /   |    02 13
 |    \  |  / \  |  /    |       |    \  |  / \  |  /    |    46 57
 |     \ | /   \ | /     |       |     \ | /   \ | /     |
 |      \|/     \|/      |       |      \|/     \|/      |
 |       .       .       |       |       .       .       |
 |      /|\     /|\      |       |      /|\     /|\      |
 |     / | \   / | \     |       |     / | \   / | \     |
 |    /  |  \ /  |  \    |       |    /  |  \ /  |  \    |
 |   /   |   .   |   \   |       |   /   |   .   |   \   |
 |  /    |  / \  |    \  |       |  /    |  / \  |    \  |
 | /     | /   \ |     \ |       | /     | /   \ |     \ |
 |/      |/     \|      \|       |/      |/     \|      \|   *w4 = *-w0
 +       +       -       -       +       +       -       -   *w6 = *-w2
12     64509   65533    1020    16     64509   65533    1020
f0246   f0246   f0246   f0246   f1357   f1357   f1357   f1357
(w0)    (w2)    (w4)    (w6)    (w0)    (w2)    (w4)    (w6)   
 |       |       |       |       |       |       |       |
 .       .       .       .       *w0     *w1     *w2     *w3  w0=1 w1=16
 |\      |\      |\      |\     /|      /|      /|      /|    w2=256 w3=4096
 | \     | \     | \     | \   / |     / |     / |     / |
 |  \    |  \    |  \    |  \ /  |    /  |    /  |    /  |    sep = 4  k = 1
 |   \   |   \   |   \   |   .   |   /   |   /   |   /   |
 |    \  |    \  |    \  |  / \  |  /    |  /    |  /    |    04 15 26 37
 |     \ |     \ |     \ | /   \ | /     | /     | /     |
 |      \|      \|      \|/     \|/      |/      |/      |     
 |       .       .       .       .       .       .       |
 |       |\      |\     /|\     /|\     /|      /|       |     
 |       | \     | \   / | \   / | \   / |     / |       |
 |       |  \    |  \ /  |  \ /  |  \ /  |    /  |       |
 |       |   \   |   .   |   .   |   .   |   /   |       |
 |       |    \  |  / \  |  / \  |  / \  |  /    |       |
 |       |     \ | /   \ | /   \ | /   \ | /     |       |
 |       |      \|/     \|/     \|/     \|/      |       |
 |       |       .       .       .       .       |       |
 |       |      /|\     /|\     /|\     /|\      |       |     
 |       |     / | \   / | \   / | \   / | \     |       |
 |       |    /  |  \ /  |  \ /  |  \ /  |  \    |       |
 |       |   /   |   .   |   .   |   .   |   \   |       |
 |       |  /    |  / \  |  / \  |  / \  |    \  |       |
 |       | /     | /   \ | /   \ | /   \ |     \ |       |
 |       |/      |/     \|/     \|/     \|      \|       |
 |       .       .       .       .       .       .       |
 |      /|      /|      /|\     /|\      |\      |\      |
 |     / |     / |     / | \   / | \     | \     | \     |
 |    /  |    /  |    /  |  \ /  |  \    |  \    |  \    |
 |   /   |   /   |   /   |   .   |   \   |   \   |   \   |
 |  /    |  /    |  /    |  / \  |    \  |    \  |    \  |   *w4 = *-w0
 | /     | /     | /     | /   \ |     \ |     \ |     \ |   *w5 = *-w1
 |/      |/      |/      |/     \|      \|      \|      \|   *w6 = *-w2
 +       +       +       +       -       -       -       -   *w7 = *-w3
28     48061   64509   50109   65533   15420    1020   17468
f(w0)   f(w1)   f(w2)   f(w3)   f(w4)   f(w5)   f(w6)   f(w7)

Note that if p and q are positions of the left and right hand sides of
a butterfly operation for a given setting of k, then the multiplier
is: w^(q<<(k-1)) which simplifies to w^((q<<(k-1)) MOD N). This is
further simplified using the observation that w^(N/2)=-1.

The inverse FFT is exactly the same computation but using w^-1 instead
of w, and dividing the resulting values by 8 at the end.

FFT normally works with complex numbers but this demonstration used a
Galois field consisting of integers modulo 2^16+1. The arithmetic
operators are provided by the function add, sub, neg, mul, div, inv,
pow and neg.

The loki chip has 128 separate cores that execute in parallel, They
can communicate with each other using channels. Each channel has a 4
word buffer and a coroutine is held up if it tries to read from and
empty buffer or write to a full one.

In this demonstration the polynomial is of degree 511 having 512
coefficients a0 to a511. Each core performs the calculation for just 4
of these element positions using a separate coroutine for each
position. These coroutines are called element coroutines and they
communicate with other element coroutines using channels 0 to 3 of the
core on which they are running.

*/

GET "libhdr"

// Turn on DBG conditional compilation flag.
//$$DBG

// Turn on P0 conditional compilation flag.
//$$P0


MANIFEST {
  // Fields of a core control block
  c_link=0                         // This is the start of a core control block
  c_cptr // The core coroutine
  c_id   // The core number between 0 and 127

  // The general purpose registers -- currently used
  c_r0;  c_r1;  c_r2;  c_r3;  c_r4;  c_r5;  c_r6;  c_r7 
  c_r8;  c_r9;  c_r10; c_r11; c_r12; c_r13; c_r14; c_r15 
  c_r16; c_r17; c_r18; c_r19; c_r20; c_r21; c_r22; c_r23

  c_z  // The zero register
  c_pc // The program counter

  c_wrkcov  // The vector of work coroutines.
  c_bufv    // The vector of channel buffers.

  c_size    // The size of the core control block.
  c_upb=c_size-1

  // Channel buffer fields
  buf_data0=0; buf_data1; buf_data2; buf_data3  // The 4 word input buffer
  buf_count       // Number of words currently in the input buffer.
  buf_wrwaitlist  // List of coroutines waiting to send to this buffer.
  buf_wrwaitliste // Pointer to the last link field in the wrwaitlist.
  buf_rdwaitlist  // List of coroutines waiting to read from this buffer.
  buf_rdwaitliste // Pointer to the last link field in rdwaitlist.
  buf_size        // Size of the buffer control block.
  buf_upb=buf_size-1

  modulus = #x10001  // 2**16 + 1
}

GLOBAL {
  stdin:ug    // 200
  stdout
  tofilename
  tostream

  tracing    // =TRUE causes debugging output.
  walsh      // =TRUE if performing the Walsh transform.

  // Variable visible to all core, element and other coroutines.

  ln         // N = 2^ln
  N
  w          // The princpal root or inverse root of N

  prupb      // min(N-1, 31) specifying how many elements to print.

  datav      // The vector holding the test data.
  datavupb   // The upper bound of datav.

  corev      // Vector for the 128 core control blocks
  corevupb   // The upper bound of corev. 
  stepcount  // 214 The number of times a coroutine has waited for a command
             // since the start of the run.

  lolist     // 215 List of currently active coroutines that are ready to run.
             // In the loki chip the core coroutines would be executed
             // simultaneously. 
  loliste    // Pointer to the last link in the curlist chain.

  hilist     // 217 List of coroutines ready to run after lolist becomes
             // empty.
  hiliste    // Pointer to the last link in the comlist chain.
             // This allows coroutines to be added at the end of the list.
             // Coroutines in this list are only given control when
             // lolist is empty.
  deletecore
  schedule   // 222
  resumelosubco // Transfer control to another coroutine but ultimately
                  // resume the current coroutine.
  resumehisubco // Transfer control to another coroutine but ultimately
                  // resume the current coroutine.

  fftcofn  // The main function of the fft coroutine.
  fftco    // The fft coroutine
  corecofn // 227 The core coroutines have control block held in corev.
  elemcofn
  // The element coroutines are created and controlled by the
  // core coroutines.

  send
  recv

  prcortn
  prbuf   // 232
  pr
  reverse

  dv
  inv
  add   // 237
  sub
  neg
  mul
  div
  pow   // 242

  fmess
  emess
}

//LET abort() BE RETURN  // This is to disable the abort function.

LET fmess(str, a1, a2) BE
{ prcortn()
  writef("%i5 fftco:          ", stepcount)
  writef(str, a1, a2)
  newline()
}

AND emess(id, chno, str, a1, a2) BE
{ LET p = 4*id + chno
  $<P0 UNLESS p=0 RETURN $>P0
  prcortn()
  writef("%i5 elemco %n/%n p=%n: ", stepcount, id, chno, p)
  writef(str, a1, a2)
  newline()
}

LET start() = VALOF
{ LET argv = VEC 50

$<DBG writef("*nDBG conditional compilation flag turned on*n") $>DBG
$<P0  writef("*nP0  conditional compilation flag turned on*n") $>P0


  stdin := input()
  stdout := output()
  tofilename := 0
  tostream := 0

  fftco := 0

  UNLESS rdargs("ln/N,TO/K,walsh/S,-t/S", argv, 50) DO
  { writef("*nBad arguments for lokifft*n")
    GOTO err
  }

  UNLESS argv!0 DO
  { writef("*nArgs: ln/N,to/K,walsh/S,-t/S*n*n")

    writef("ln/N    Specifies the size of the problem*n")
    writef("        The transform will be performed on N=2^ln values*n")
    writef("to/K    Specifies where to send the output*n")
    writef("walsh/S Perform the Walsh transformation will be performed*n*n")
    writef("-t/S    Generate debugging output*n")

    RESULTIS 0
  }

  IF argv!0 DO ln := !argv!0          // ln/N
  IF argv!1 DO tofilename := argv!1   // to/K
  walsh   := argv!2                   // walsh/S
  tracing := argv!3                   // -t/S

  IF tofilename DO
  { tostream := findoutput(tofilename)
    UNLESS tostream DO
    { writef("ERROR: Trouble with TO stream %s*n", tofilename)
      GOTO err
    }
    selectoutput(tostream)
  }

  writef("*nLoki fft demo version 2 entered*n")

  fftco := createco(fftcofn, 200)

  TEST fftco
  THEN callco(fftco, 0)  // Run the FFT algorithm.
  ELSE writef("*nERROR: More space needed*n")

err:
  writef("End of demo*n")
  IF fftco DO deleteco(fftco)
  IF tostream DO
  { endstream(tostream)
    tostream := 0
    selectoutput(stdout)
  }
  RESULTIS 0
}

AND deletecore(id) BE IF corev!id DO
{ LET cptr = corev!id!c_cptr
  callco(cptr) // Let the core coroutine delete it element coroutines.
  deleteco(cptr)
}

// Scheduling functions

AND schedule() = VALOF
{ // This function selects the next coroutine to be given
  // control.

  // If hilist is empty all the coroutines in lolist are
  // transferred to hilist, leaving lolist empty.

  // When schedule is called hilist and lolist will not
  // both be empty.

  // If hilist is not empty, the first coroutine on the list
  // is dequeued and given control.

  // schedule returns the value passed to this coroutine when it
  // next gains control.

  prlists()

  UNLESS hilist DO
  { hilist, hiliste := lolist, loliste
    lolist, loliste := 0, @lolist
    stepcount := stepcount+1
    // All element coroutines will have completed the previous step
    // and will be waiting in schedule to start working on the next
    // step.
    $<DBG newline() $>DBG
    //prcortn()
    //writef("%i5 schedule: New stepcount*n",
    //        stepcount)
  }

  IF hilist DO
  { // Extract the first coroutine in hilist.
    LET cptr  = hilist!1
    hilist := hilist!0 // The link is always at offset zero.
    // hiliste points to the last link field in the list and
    // if hilist has just become empty loliste needs to be corrected.
    UNLESS hilist DO hiliste := @hilist

//    prcortn()
//    writef("%i5 schedule: Giving control to a coroutine %n*n",
//            stepcount, cptr)
//abort(7654)
    RESULTIS resumeco(cptr, 0)
  }

  // hilist and lolist were both empty.
  prcortn()

  writef("%i5 schedule: ERROR: Called when lolist and hilist both empty*n",
          stepcount)
  abort(999)
  RESULTIS 0
}

AND resumelosubco(cptr, val) = VALOF
{ // This places the current coroutine on the end of lolist
  // and then calls resumeco(cptr, val) returning its result.
  // This coroutine will not regain control until there are
  // no coroutines in hilist.
  LET link, co = 0, currco
  LET node = @link
  !loliste := node
  loliste  := node
  RESULTIS resumeco(cptr, val)
}

AND resumehisubco(cptr, val) = VALOF
{ // This places the current coroutine on the end of hilist
  // and then calls resumeco(cptr, val) returning its result.
  // This coroutine will regain control before any of the
  // coroutines in lolist.
  LET link, co = 0, currco
  LET node = @link
  !hiliste := node
  hiliste  := node
  RESULTIS resumeco(cptr, val)
}

AND lowait() = VALOF
{ LET link, cptr = 0, currco
  LET node = @link
  // Append this coroutine on the end of lolist.
  loliste!0 := node
  loliste := node
  RESULTIS schedule() // All coroutines in hilist will run before
                      // this coroutine regains control,
}

AND fftcofn(args) = VALOF
{ // This is the main function of the fft coroutine fftco. It is given
  // control by start using callco and returns at the end using cowait.
  // This coroutine is the controller of the FFT algorithm.
  // When started only the variables ln and walsh have been set. It
  // initialises some global variables then creates the core coroutines
  // using initco. When each core coroutine returns it will have created
  // its four element coroutines waiting in lowait() before starting the
  // first step. At this stage the core coroutine returns control to
  // fftco using cowait.

  // This coroutine runs the algorithm by repeatedly calling lowait()
  // after possibly setting some of the environment variables ln, N, w,
  // sep and k. After some steps it output the contents of datav.

  LET res = ?

  N := 1<<ln     // The number of element values on whic to apply FFT.
  datavupb := N-1
  corevupb := N/4-1 // There are 4 element values per core.

  prupb := N<=31 -> datavupb, 31 // Upper bound for printing

  w := 1            // This will hold the Nth root of unity.

  //// Choose a suitable Nth root of Unity.
  //UNLESS walsh WHILE w < modulus DO
  //{ LET x = 1
  //  FOR i = 1 TO N DO
  //  { x := mul(x, w)
  //    IF x=1 TEST i=N THEN GOTO rootfound
  //                    ELSE BREAK
  //  }
  //  w := w+1
  //}

  // Use a table of Nth roots of unity
  UNLESS walsh DO w := ln!TABLE 0, 0, 0, 16, 4, 2, 255, 2469, 141, 157

rootfound:

  datav := getvec(datavupb)
  corev := getvec(corevupb)

  UNLESS datav & corev DO
  { writef("*nERROR: More space needed*n")
    GOTO fin
  }

  FOR i = 0 TO datavupb DO datav!i := i
  FOR i = 0 TO corevupb DO corev!i := 0

  stepcount := 0
  lolist, loliste := 0, @lolist
  hilist, hiliste := 0, @hilist
  FOR id = 0 TO corevupb DO initco(corecofn, 200, id)
  // At this stage all the element coroutines have been created and
  // are suspended in lowait() waiting to start the first step of
  // the algorithm. Each core coroutine returned control using cowait.

  TEST w=1
  THEN writef("Performing the Walsh transform on %n words of data*n", N)
  ELSE writef("*nThe principal %nth root of unity is %n = %x5*n*n",
               N, w, w)

  //prcortn()
  //writef("%i5 fftco:        All element coroutines about to start step1.*n",
  //        stepcount)

  // We are now ready to start the FFT algorithm.
  $<DBG fmess("F->E To copy data to E")  $>DBG
  lowait() // Start to copy operation.

  // Start copying data from datav into the element coroutines.

  lowait()  // Wait for copy operation to complete
  $<DBG fmess("E->F Copy data to E done")  $>DBG

  // The copying operation is complete.

  $<DBG fmess("F->E Start copy data back")  $>DBG
  lowait()  // Start the copy back operation.

  lowait()  // Wait for it to complete.
  $<DBG fmess("E->F Copy data back done")  $>DBG

  writef("Initial data*n")
  pr()

  $<DBG fmess("F->E Start the shuffle")  $>DBG
  lowait() // Start the shuffle operation.

  lowait()
  $<DBG fmess("E->F Shuffle done")  $>DBG


  // The shuffle is now complete.

  IF tracing DO
  {
    $<DBG fmess("F->E Start copy data back")  $>DBG
    lowait()  // Start the copy back operation.

    lowait()  // Wait for it to complete.
    $<DBG fmess("E->F Copy back done")  $>DBG

    writef("Shuffled data*n")
    pr()
  }

  // Do the butterfly operations.

  FOR i = 1 TO ln DO
  {
    $<DBG fmess("F->E Start butterfly phase %n", i)  $>DBG
    lowait() // Start the next butterfly phase.

    lowait() // Wait for it to complete.
    $<DBG fmess("E->F Butterfly phase %n done", i)  $>DBG

    // Phase i of butterflies has just completed.

    IF tracing DO
    {
      $<DBG fmess("F->E Start copy back")  $>DBG
      lowait() // Copy data into datav.

      lowait() // Wait for copy to complete.
      $<DBG fmess("E->F Copy back done")  $>DBG

      writef("Data after a butterfly phase %n*n", i)
      pr()
    }
  }

  $<DBG fmess("F->E Start copy back")  $>DBG
  lowait() // Start the copy back operation.

  lowait() // Wait for it to complete.
  $<DBG fmess("E->F Copy back done")  $>DBG

  // Data has now been copied back to datav after the last butterfly step.
  writef("Data after performing the FFT*n")
  pr()
//abort(1001)


// We now perform the inverse FFT

  $<DBG fmess("F->E Start the shuffle")  $>DBG
  lowait() // Start the shuffle operation.

  lowait()
  $<DBG fmess("E->F Shuffle done")  $>DBG

  // The shuffle is now complete.

  IF tracing DO
  { 
    $<DBG fmess("F->E Start copying data back")  $>DBG
    lowait()  // Start the copy back operation.

    lowait()  // Wait for it to complete.
    $<DBG fmess("E->F Copy data back done")  $>DBG

    // The copying is complete.
    writef("Shuffled data*n")
    pr()
  }

  // Do the butterfly operations.

  FOR i = 1 TO ln DO
  {
    $<DBG fmess("F->E Start butterfly phase %n", i)  $>DBG
    lowait() // Wait to start the next butterfly phase.

    lowait() // Wait for it to complete.
    $<DBG fmess("E->F Butterfly phase %n done", i)  $>DBG

    // Phase i of butterflies has just completed.

    IF tracing DO
    {
      $<DBG fmess("F->E Start copy back")  $>DBG
      lowait() // Copy data into datav.

      lowait() // Wait for copy to complete.
      $<DBG fmess("E->F Copy back done")  $>DBG

      writef("Data after a butterfly phase %n*n", i)
      pr()
    }
  }

  // Butterfly operations are now complete.

  $<DBG fmess("F->E Start divide by N")  $>DBG
  lowait()  // Start the divide by N operation.

  lowait()  // Divide by N now done.
  $<DBG fmess("E->F Divide by N done")  $>DBG

  $<DBG fmess("F->E Start copy back")  $>DBG
  lowait() // Start the copy back operation.

  lowait() // Wait for it to complete.
  $<DBG fmess("E->F Copy back done")  $>DBG

  writef("Data after performing the Inverse FFT*n")
  pr()


end:  // Close down the demonstration using command 6 before deleting
      // the core coroutines
  $<DBG fmess("F->E Start close down")  $>DBG
  lowait()  // Close down

  lowait()  // Make sure everything has finished.
  $<DBG fmess("E->F Close down done")  $>DBG


fin:
  // Delete the cores in reverse order to improve the efficiency
  // of deleteco.
  FOR id = corevupb TO 0 BY -1 IF corev!id DO deletecore(id)
  IF corev DO freevec(corev)
  IF datav DO freevec(datav)

  RESULTIS 0
}

// The main functions for the core and element coroutines.

AND corecofn(args) = VALOF
{ // args -> [id]

  // This function is the body of all core coroutines. As soon
  // as a core coroutine is created, it creates its four element
  // coroutines leaving them suspended in lowait(). It then
  // returns control to fftco using cowait. It next gains control
  // at the end when it is time to delete its four element
  // coroutines. 

  LET link, cptr = 0, currco // The start of the control block.
  LET id = !args

  // This core's general purpose registers -- not currently used.
  LET  r0,  r1,  r2,  r3,  r4,  r5,  r6,  r7 = 0, 0, 0, 0, 0, 0, 0, 0
  LET  r8,  r9, r10, r11, r12, r13, r14, r15 = 0, 0, 0, 0, 0, 0, 0, 0
  LET r16, r17, r18, r19, r20, r21, r22, r23 = 0, 0, 0, 0, 0, 0, 0, 0

  LET z = 0  // The zero register
  LET pc = 0 // This core's pseudo program counter

  LET wrkcov = 0  // To hold the vector of work coroutines.
  LET bufv = 999  // To hold the vector of channel buffers.

  // Declare the vector of work coroutines for channels 0 to 3.
  LET wrkco0, wrkco1, wrkco2, wrkco3 = 0, 0, 0, 0
  // The channel buffer control blocks
  LET buf0, buf1, buf2, buf3, buf4, buf5 = 0, 0, 0, 0, 0, 0

  // The output channel destination mappings (core, chno) -- not currently used.
  LET outchcore0, outchcore1, outchcore2 = 0, 0, 0   // core control blocks
  LET outchcore3, outchcore4, outchcore5 = 0, 0, 0   // for the out channels

  LET outchchan0, outchchan1, outchchan2 = 0, 0, 0   // in channel numbers for
  LET outchchan3, outchchan4, outchchan5 = 0, 0, 0   // this the out channels.

  // Space for channel 0's buffer and control variables.
  LET data0a, data0b, data0c, data0d = 0, 0, 0, 0  // The 4 word buffer
  LET count0 = 0       // Number of buffer words currently in use.
  LET wrwaitlist0 = 0  // List of coroutines waiting to send to this buffer.
  LET wrwaitliste0 = @wrwaitlist0 // Pointer to the last link field.
  LET rdwaitlist0 = 0  // List of coroutines waiting to read from this buffer.
  LET rdwaitliste0 = @rdwaitlist0 // Pointer to the last link field.

  // Space for channel 1's buffer and control variables
  LET data1a, data1b, data1c, data1d = 0, 0, 0, 0  // The 4 word buffer
  LET count1 = 0       // Number of buffer words currently in use.
  LET wrwaitlist1 = 0  // List of coroutines waiting to send to this buffer.
  LET wrwaitliste1 = @wrwaitlist1 // Pointer to the last link field.
  LET rdwaitlist1 = 0  // List of coroutines waiting to read from this buffer.
  LET rdwaitliste1 = @rdwaitlist1 // Pointer to the last link field.

  // Space for channel 2's buffer and control variables
  LET data2a, data2b, data2c, data2d = 0, 0, 0, 0  // The 4 word buffer
  LET count2 = 0       // Number of buffer words currently in use.
  LET wrwaitlist2 = 0  // List of coroutines waiting to send to this buffer.
  LET wrwaitliste2 = @wrwaitlist2 // Pointer to the last link field.
  LET rdwaitlist2 = 0  // List of coroutines waiting to read from this buffer.
  LET rdwaitliste2 = @rdwaitlist2 // Pointer to the last link field.

  // Space for channel 3's buffer and control variables
  LET data3a, data3b, data3c, data3d = 0, 0, 0, 0  // The 4 word buffer
  LET count3 = 0       // Number of buffer words currently in use.
  LET wrwaitlist3 = 0  // List of coroutines waiting to send to this buffer.
  LET wrwaitliste3 = @wrwaitlist3 // Pointer to the last link field.
  LET rdwaitlist3 = 0  // List of coroutines waiting to read from this buffer.
  LET rdwaitliste3 = @rdwaitlist3 // Pointer to the last link field.

  // Space for channel 4's buffer and control variables
  LET data4a, data4b, data4c, data4d = 0, 0, 0, 0  // The 4 word buffer
  LET count4 = 0       // Number of buffer words currently in use.
  LET wrwaitlist4 = 0  // List of coroutines waiting to send to this buffer.
  LET wrwaitliste4 = @wrwaitlist4 // Pointer to the last link field.
  LET rdwaitlist4 = 0  // List of coroutines waiting to read from this buffer.
  LET rdwaitliste4 = @rdwaitlist4 // Pointer to the last link field.

  // Space for channel 5's buffer and control variables
  LET data5a, data5b, data5c, data5d = 0, 0, 0, 0  // The 4 word buffer
  LET count5 = 0       // Number of buffer words currently in use.
  LET wrwaitlist5 = 0  // List of coroutines waiting to send to this buffer.
  LET wrwaitliste5 = @wrwaitlist5 // Pointer to the last link field.
  LET rdwaitlist5 = 0  // List of coroutines waiting to read from this buffer.
  LET rdwaitliste5 = @rdwaitlist5 // Pointer to the last link field.

  corev!id := @link

  bufv := @buf0        // Vector of channel buffer items.

  buf0 := @data0a
  buf1 := @data1a
  buf2 := @data2a
  buf3 := @data3a
  buf4 := @data4a
  buf5 := @data5a

//  prcortn()
//  writef("%i5 coreco   %i3:      About to create to element coroutines*n",
//          stepcount, id)
  // Create the element coroutines for this core, giving them the
  // core's coroutine, its id and the appropriate channel numbers.
  wrkco0 := initco(elemcofn, 250, currco, id, bufv, 0)
  wrkco1 := initco(elemcofn, 250, currco, id, bufv, 1)
  wrkco2 := initco(elemcofn, 250, currco, id, bufv, 2)
  wrkco3 := initco(elemcofn, 250, currco, id, bufv, 3)

  UNLESS wrkco0 & wrkco0 & wrkco0 & wrkco0 DO
  { writef("More space needed*n")
    abort(999)
  }

//  prcortn()
//  writef("%i5 coreco   %i3:      About to cause them to wait in lowait for the first step*n",
//          stepcount, id)

  // Cause the four element coroutines to suspend themselves in lowait().
  resumehisubco(wrkco0, 0)
  resumehisubco(wrkco1, 0)
  resumehisubco(wrkco2, 0)
  resumehisubco(wrkco3, 0)

//  prcortn()
//  writef("%i5 coreco   %i3:      Calling cowait to return to fftco*n",
//          stepcount, id)

  cowait(0) // Return control to fftco.

  // Now waiting for fftco to call this coroutine using callco to
  // cause it to delete its element coroutines.

//  prcortn()
//  writef("%i5 coreco   %i3:      Deleting the element coroutines*n",
//          stepcount, id)


  // It is now time to release all the space allocated by this core
  deleteco(wrkco3)
  deleteco(wrkco2)
  deleteco(wrkco1)
  deleteco(wrkco0)

  cowait(0)  // Return control to fftco.

  abort(998)  // Should not be reached.
  RESULTIS 0
}

AND elemcofn(args) = VALOF
{ // args -> [coreco, id, bufv, chno]
  // This coroutine is created using initco so must first return
  // using cowait. After this call its core coroutine will again
  // give it control using resumehisubco to let it suspend itself
  // in lowait ready to start the first step of the FFT algorithm.
  // Note that this call of lowait will give control back to the
  // core coroutine.

  // This coroutine performs the computations necessary for its
  // data element which is at position 4*id_chno. Element coroutines
  // know the FFT algorithm and calls lowait at the end of each step.

  LET x = 0    // The element value processed by this element coroutine
  LET w1 = w   // Private copy of the Nth root of unity.
  LET sep, k = 0, 0

  LET coreco, id, bufv, chno = args!0, args!1, args!2, args!3
  // cptr is the core coroutine controlling this element coroutine.
  // id   is the id of the controlling core coroutine.
  // bufv is the vector of channel buffers of the controlling core.
  // chno is the channel number owned by this element coroutine.

  LET buf = bufv!chno
  LET p = 4*id + chno // p is the data element position processed by
                      // this element coroutine.
  LET q = ?           // q will be the partner position used in the
                      // perfect shuffle or butterfly operations.

  // The element coroutine has been created and initialised but is
  // waiting to be started.

  cowait(0) // Return control to the call of initco in the controlling
            // core coroutine. to enter its command loop.

  // Return control to the core coroutine and wait to start
  // the the first step.
  lowait()
  $<DBG emess(id, chno, "Getting data") $>DBG

  //prcortn()
  //writef("%i5 elemco   %i3/%n p=%n: Starting the first step*n",
  //        stepcount, id, chno, p)

  x := datav!p  // Get the element value from datav.

  //prcortn()
  //writef("%i5 elemco %i3/%n p=%n: Set x=%n from datav*n",
  //        stepcount, id, chno, p, x)

  $<DBG emess(id, chno, "Getting data done") $>DBG
  lowait() // End of the first step.

  lowait() // Wait for fftco to start the next step
  $<DBG emess(id, chno, "Copying data back") $>DBG

  datav!p := x  // Copy the element value into datav

  //prcortn()
  //writef("%i5 elemco %i3/%n p=%n: Copying x=%n back into datav*n",
  //        stepcount, id, chno, p, x)

  $<DBG emess(id, chno, "Copying data back done") $>DBG
  lowait()      // End of copy back step.

  lowait()      // Wait for fftco to start the shuffle step.
  $<DBG emess(id, chno, "Doing the shuffle") $>DBG

  // Perform the perfect shuffle.

  q := reverse(p, ln)
  TEST p=q
  THEN { // Nothing to do.
       }
  ELSE { send(q/4, q & 3, x)
         x := recv(buf)
       }
  $<DBG emess(id, chno, "Shuffle done") $>DBG
  lowait() // Shuffle done
  
  IF tracing DO
  { lowait() // Wait for fftco to start the copy back step.
    $<DBG emess(id, chno, "Copying data back") $>DBG

    datav!p := x  // Copy the element value into datav

    $<DBG emess(id, chno, "Copying data back done") $>DBG
    lowait()      // Copy back step done.
  }

  sep, k, w1 := 1, ln, w

  FOR i = 1 TO ln DO
  { lowait()  // Wait for fftco to start a butterfly step.
    $<DBG emess(id, chno, "Doing butterfly phase %n", i) $>DBG

    q := p XOR sep

    // Do the butterfly operation
    //                              |<--sep-->|
    //                              .         *wa
    //                              |\       /|
    //                              | \     / |
    //                              |  \   /  |
    //                              |   \ /   |
    //                              |    .   neg
    //                              |   / \   |
    //                              |  /   \  |
    //                              | /     \ |
    //                              |/       \|
    //                              +         +
    //                              |         |

    // p point to the current element of the butterfly and q points to
    // the other element. p may be < or > then q. The code here deals
    // with the half of the butterfly corresponding to position p.

    TEST p < q
    THEN { send(q/4, q & 3, x)
         }
    ELSE { LET e = (q<<(k-1)) MOD N
           LET we = pow(w1, e)
           x := mul(x, we)
           send(q/4, q & 3, x)
           x := neg(x)
         }

    { LET y = recv(buf)
      x := add(x, y)

      $<DBG emess(id, chno, "Butterfly phase %n done", i) $>DBG
      lowait()  // Wait for fftco to start the next step.
    }

    IF tracing DO
    { lowait()      // Wait for fftco to start the copy operation.
      $<DBG emess(id, chno, "Copying data back") $>DBG

      datav!p := x  // Copy the element value into datav

      $<DBG emess(id, chno, "Copying data back done") $>DBG
      lowait()      // Indicate that the copy is complete.
    }

   sep, k := 2*sep, k-1 // Ready for next butterfly step.
  }

  // FFT is now complete.
  lowait()   // Wait to start the copy operation.
  $<DBG emess(id, chno, "Copying data back") $>DBG

  datav!p := x  // Copy the element value into datav

  $<DBG emess(id, chno, "Copying data back done") $>DBG
  lowait()      // Indicate that copying is complete.

  // Now perform the inverse FFT operation.

  lowait()
  $<DBG emess(id, chno, "Starting the inverse FFT do the shuffle") $>DBG

  // Perform the perfect shuffle.
  q := reverse(p, ln)
  TEST p=q
  THEN { // Nothing to do.
       }
  ELSE { send(q/4, q & 3, x)
         x := recv(buf)
       }

  $<DBG emess(id, chno, "Shuffle done") $>DBG
  lowait() // Shuffle done
  
  IF tracing DO
  { lowait() // Wait for fftco to start the copy back step.
    $<DBG emess(id, chno, "Copying data back") $>DBG

    datav!p := x  // Copy the element value into datav

    $<DBG emess(id, chno, "Copying data back done") $>DBG
    lowait()      // Copy back step done.
  }

  sep, k, w1 := 1, ln, inv(w)

  FOR i = 1 TO ln DO
  { lowait()  // Wait for fftco to start a butterfly step.
    $<DBG emess(id, chno, "Doing butterfly phase %n", i) $>DBG

    q := p XOR sep

    // Do the butterfly operation
    //                              |<--sep-->|
    //                              .         *wa
    //                              |\       /|
    //                              | \     / |
    //                              |  \   /  |
    //                              |   \ /   |
    //                              |    .   neg
    //                              |   / \   |
    //                              |  /   \  |
    //                              | /     \ |
    //                              |/       \|
    //                              +         +
    //                              |         |

    // p point to the current element of the butterfly and q points to
    // the other element. p may be < or > then q. The code here deals
    // with the half of the butterfly corresponding to position p.

    TEST p < q
    THEN { //prcortn()
           //writef("%i5 elemco %i3/%n p=%n: Butterfly Sending x=%n to q=%n*n",
           //        stepcount, id, chno, p, x, q)
           send(q/4, q & 3, x)
         }
    ELSE { LET e = (q<<(k-1)) MOD N
           LET we = pow(w1, e)
           x := mul(x, we)
           send(q/4, q & 3, x)
           x := neg(x)
         }

    { LET y = recv(buf)
      x := add(x, y)

      $<DBG emess(id, chno, "Butterfly phase %n done", i) $>DBG
      lowait()  // Wait for fftco to start the next step.
    }

    IF tracing DO
    { lowait()      // Wait for fftco to start the copy operation.
      $<DBG emess(id, chno, "Copying data back") $>DBG

      datav!p := x  // Copy the element value into datav

      $<DBG emess(id, chno, "Copying data back done") $>DBG
      lowait()      // Indicate that the copy is complete.
    }

   sep, k := 2*sep, k-1 // Ready for next butterfly step.
  }

  // Butterfly operations are now complete.

  lowait() // Wait for fftco to start the divide operation.
  $<DBG emess(id, chno, "Dividing by N") $>DBG

  // Divide the element by N.
  x := div(x, N)

  $<DBG emess(id, chno, "Divide by N done") $>DBG
  lowait()  // Indicate that the divide operation has completed.

  lowait()      // Wait for fftco to start the copy back operation.
  $<DBG emess(id, chno, "Copying data back") $>DBG

  datav!p := x  // Copy the element value into datav

  $<DBG emess(id, chno, "Copying data back done") $>DBG
  lowait()      // Indicate that copying is complete.

  lowait()
  $<DBG emess(id, chno, "Closing down") $>DBG

  // Free all allocated space
  // Nothing to free.

  $<DBG emess(id, chno, "Close down done") $>DBG
  schedule() // This coroutine has finished.

  // Should never reach this point.
  abort(999)
  RESULTIS 0
}

AND send(destid, chno, val) BE
{ // Send val to input channel chno of the core destid. If the
  // buffer is full it suspends itself by calling schedule. This
  // function is used by fftco to send commands to core coroutines,
  // and by element coroutines to send values to other element
  // coroutines. Each element coroutine works on a different element
  // position and only using send to give values to other element
  // coroutines either during the perfect shuffle operation or a
  // butterfly computation. It never sends a value to its own position.

  // Find the destination buffer.
  LET destcore = corev!destid         // Destination core control block
  LET buf      = destcore!c_bufv!chno // and its in channel buffer.

  WHILE buf_count!buf = 4 DO
  { // The buffer is full, so suspend this coroutine by appending it
    // to the end of wrwiatlist.
    LET link, cptr = 0, currco // Form a node for the wait list.
    LET node = @link
    buf_wrwaitliste!buf!0 := node // Append the node on the end of the list.
    buf_wrwaitliste!buf := node

    // Suspend this coroutine using schedule.
    schedule()  // Give control to another coroutine.

    // This call of schedule only returns after another
    // coroutine extracts a value from this buffer using recv.
    // The count should now be less than 4.
  }

  // Buffer count is < 4, so val can be copied into the buffer.
  buf!buf_count!buf := val
  buf_count!buf := buf_count!buf + 1

  // Buffer count > 0
  IF buf_rdwaitlist!buf DO
  { // The buffer is not empty and there are coroutines (typically
    // just one) waiting for data from this channel, make them
    // ready to run by appending them to the end of lolist.

    LET cptr = buf_rdwaitlist!buf!1
    buf_rdwaitlist!buf := buf_rdwaitlist!buf!0
    UNLESS buf_rdwaitlist!buf DO buf_rdwaitliste!buf := @buf_rdwaitlist!buf

    // Give it control first.
    resumehisubco(cptr, 0)
  }
}

AND recv(buf) = VALOF
{ // This returns the next value from the given
  // channel buffer, waiting if necessary.

  LET val = ?

  UNTIL buf_count!buf DO
  { // The buffer is empty, so suspend this coroutine after
    // appending it to rdwaitlist.
    LET link, cptr = 0, currco
    LET node = @link

    (buf_rdwaitliste!buf)!0 := node
    buf_rdwaitliste!buf := node

    schedule()  // Give control to another coroutine, possibly the
                // core coroutine.

    // This call of schedule only returns after another element
    // coroutine sends a value to this channel.
    // The count should now be greater than zero.
  }

  // The buffer is not empty.

  val := buf_data0!buf           // Get the next value from the buffer
  buf_data0!buf := buf_data1!buf // Shift the contents.
  buf_data1!buf := buf_data2!buf
  buf_data2!buf := buf_data3!buf
  buf_count!buf := buf_count!buf - 1
  //prcortn()
  //writef("%i5 recv: extracted value %n*n", stepcount, val)

  IF buf_wrwaitlist!buf DO
  { // The buffer is no longer full and there are coroutines (typically
    // just one) waiting to send data to this buffer. These are released
    // by moving them to them all to the end of hilist.

    !hiliste := buf_wrwaitlist!buf
    hiliste := buf_wrwaitliste!buf

    buf_wrwaitlist!buf := 0
    buf_wrwaitliste!buf := @buf_wrwaitlist!buf
  }

  RESULTIS val
}

AND prcortn() BE writef("%i5 ", currco)

AND prlists() BE
{ LET p = lolist
  LET count = 0
RETURN
  writef("lolist: ")
  WHILE p & count<5 DO
  { writef(" %i5:%i5", p, p!1)
    count := count+1
    p := p!0
  }
  newline()
  p := hilist
  count := 0
  writef("hilist: ")
  WHILE p & count<5 DO
  { writef(" %i5:%i5", p, p!1)
    count := count+1
    p := p!0
  }
  newline()
}

AND prbuf(buf, id, chno) BE
{ LET len = buf_count!buf
  prcortn()
  writef("Chan %n/%n: buf=%n rdlist=%n wrlist=%n   ",
          id, chno, buf, buf_rdwaitlist!buf, buf_rdwaitlist!buf)
  writef("%n:", len)
  FOR i = 1 TO len DO writef(" %i4", buf!i)
  newline()
}

AND pr() BE
{ FOR i = 0 TO prupb DO { writef("%i5 ", datav!i)
                          IF i MOD 8 = 7 DO newline()
                        }
  newline()
}

AND reverse(x, ln) = VALOF
{ LET res = x & 1

  WHILE ln>1 DO
  { x, ln := x>>1, ln-1
    res := res<<1 | x&1
  }

  RESULTIS res
}

AND dv(a, m, b, n) = a=1 -> m,
                     a=0 -> m-n,
                     a<b -> dv(a, m, b REM a, m*(b/a)+n),
                     dv(a REM b, m+n*(a/b), b, n)


AND inv(x) = dv(x, 1, modulus-x, 1)

AND add(x, y) = VALOF
{ LET a = x+y
  IF a<modulus RESULTIS a
  RESULTIS a-modulus
}

AND sub(x, y) = add(x, neg(y))

AND neg(x)    = modulus-x

AND mul(x, y) = x=0 -> 0,
                (x&1)=0 -> mul(x>>1, add(y,y)),
                add(y, mul(x>>1, add(y,y)))

AND div(x, y) = mul(x, inv(y))

AND pow(x, e) = VALOF
{ LET res = 1
  LET xi = x
  WHILE e DO
  { IF (e&1)>0 DO res := mul(res, xi)
    e := e>>1
    xi := mul(xi, xi)
  }
  RESULTIS res
}


/*
Here are two demonstration runs of this program.

0.000> lokifftv2 3

DBG conditional compilation flag turned on

Loki fft demo version 2 entered

The principal 8th root of unity is 16 = 00010

64395     0 fftco:          F->E To copy data to E

64861     1 elemco 0/0 p=0: Getting data
64861     1 elemco 0/0 p=0: Getting data done
65129     1 elemco 0/1 p=1: Getting data
65129     1 elemco 0/1 p=1: Getting data done
65397     1 elemco 0/2 p=2: Getting data
65397     1 elemco 0/2 p=2: Getting data done
65665     1 elemco 0/3 p=3: Getting data
65665     1 elemco 0/3 p=3: Getting data done
66151     1 elemco 1/0 p=4: Getting data
66151     1 elemco 1/0 p=4: Getting data done
66419     1 elemco 1/1 p=5: Getting data
66419     1 elemco 1/1 p=5: Getting data done
66687     1 elemco 1/2 p=6: Getting data
66687     1 elemco 1/2 p=6: Getting data done
66955     1 elemco 1/3 p=7: Getting data
66955     1 elemco 1/3 p=7: Getting data done

64395     2 fftco:          E->F Copy data to E done
64395     2 fftco:          F->E Start copy data back

64861     3 elemco 0/0 p=0: Copying data back
64861     3 elemco 0/0 p=0: Copying data back done
65129     3 elemco 0/1 p=1: Copying data back
65129     3 elemco 0/1 p=1: Copying data back done
65397     3 elemco 0/2 p=2: Copying data back
65397     3 elemco 0/2 p=2: Copying data back done
65665     3 elemco 0/3 p=3: Copying data back
65665     3 elemco 0/3 p=3: Copying data back done
66151     3 elemco 1/0 p=4: Copying data back
66151     3 elemco 1/0 p=4: Copying data back done
66419     3 elemco 1/1 p=5: Copying data back
66419     3 elemco 1/1 p=5: Copying data back done
66687     3 elemco 1/2 p=6: Copying data back
66687     3 elemco 1/2 p=6: Copying data back done
66955     3 elemco 1/3 p=7: Copying data back
66955     3 elemco 1/3 p=7: Copying data back done

64395     4 fftco:          E->F Copy data back done
Initial data
    0     1     2     3     4     5     6     7 

64395     4 fftco:          F->E Start the shuffle

64861     5 elemco 0/0 p=0: Doing the shuffle
64861     5 elemco 0/0 p=0: Shuffle done
65129     5 elemco 0/1 p=1: Doing the shuffle
65397     5 elemco 0/2 p=2: Doing the shuffle
65397     5 elemco 0/2 p=2: Shuffle done
65665     5 elemco 0/3 p=3: Doing the shuffle
66151     5 elemco 1/0 p=4: Doing the shuffle
65129     5 elemco 0/1 p=1: Shuffle done
66419     5 elemco 1/1 p=5: Doing the shuffle
66419     5 elemco 1/1 p=5: Shuffle done
66687     5 elemco 1/2 p=6: Doing the shuffle
65665     5 elemco 0/3 p=3: Shuffle done
66955     5 elemco 1/3 p=7: Doing the shuffle
66955     5 elemco 1/3 p=7: Shuffle done
66151     5 elemco 1/0 p=4: Shuffle done
66687     5 elemco 1/2 p=6: Shuffle done

64395     6 fftco:          E->F Shuffle done
64395     6 fftco:          F->E Start butterfly phase 1

64861     7 elemco 0/0 p=0: Doing butterfly phase 1
65397     7 elemco 0/2 p=2: Doing butterfly phase 1
65129     7 elemco 0/1 p=1: Doing butterfly phase 1
64861     7 elemco 0/0 p=0: Butterfly phase 1 done
66419     7 elemco 1/1 p=5: Doing butterfly phase 1
65665     7 elemco 0/3 p=3: Doing butterfly phase 1
65397     7 elemco 0/2 p=2: Butterfly phase 1 done
66955     7 elemco 1/3 p=7: Doing butterfly phase 1
66151     7 elemco 1/0 p=4: Doing butterfly phase 1
66419     7 elemco 1/1 p=5: Butterfly phase 1 done
66687     7 elemco 1/2 p=6: Doing butterfly phase 1
66955     7 elemco 1/3 p=7: Butterfly phase 1 done
65129     7 elemco 0/1 p=1: Butterfly phase 1 done
65665     7 elemco 0/3 p=3: Butterfly phase 1 done
66151     7 elemco 1/0 p=4: Butterfly phase 1 done
66687     7 elemco 1/2 p=6: Butterfly phase 1 done

64395     8 fftco:          E->F Butterfly phase 1 done
64395     8 fftco:          F->E Start butterfly phase 2

64861     9 elemco 0/0 p=0: Doing butterfly phase 2
65397     9 elemco 0/2 p=2: Doing butterfly phase 2
64861     9 elemco 0/0 p=0: Butterfly phase 2 done
66419     9 elemco 1/1 p=5: Doing butterfly phase 2
66955     9 elemco 1/3 p=7: Doing butterfly phase 2
66419     9 elemco 1/1 p=5: Butterfly phase 2 done
65129     9 elemco 0/1 p=1: Doing butterfly phase 2
65665     9 elemco 0/3 p=3: Doing butterfly phase 2
65129     9 elemco 0/1 p=1: Butterfly phase 2 done
66151     9 elemco 1/0 p=4: Doing butterfly phase 2
66687     9 elemco 1/2 p=6: Doing butterfly phase 2
66151     9 elemco 1/0 p=4: Butterfly phase 2 done
65397     9 elemco 0/2 p=2: Butterfly phase 2 done
66955     9 elemco 1/3 p=7: Butterfly phase 2 done
65665     9 elemco 0/3 p=3: Butterfly phase 2 done
66687     9 elemco 1/2 p=6: Butterfly phase 2 done

64395    10 fftco:          E->F Butterfly phase 2 done
64395    10 fftco:          F->E Start butterfly phase 3

64861    11 elemco 0/0 p=0: Doing butterfly phase 3
66419    11 elemco 1/1 p=5: Doing butterfly phase 3
65129    11 elemco 0/1 p=1: Doing butterfly phase 3
66419    11 elemco 1/1 p=5: Butterfly phase 3 done
66151    11 elemco 1/0 p=4: Doing butterfly phase 3
64861    11 elemco 0/0 p=0: Butterfly phase 3 done
65397    11 elemco 0/2 p=2: Doing butterfly phase 3
66955    11 elemco 1/3 p=7: Doing butterfly phase 3
65665    11 elemco 0/3 p=3: Doing butterfly phase 3
66955    11 elemco 1/3 p=7: Butterfly phase 3 done
66687    11 elemco 1/2 p=6: Doing butterfly phase 3
65397    11 elemco 0/2 p=2: Butterfly phase 3 done
65129    11 elemco 0/1 p=1: Butterfly phase 3 done
66151    11 elemco 1/0 p=4: Butterfly phase 3 done
65665    11 elemco 0/3 p=3: Butterfly phase 3 done
66687    11 elemco 1/2 p=6: Butterfly phase 3 done

64395    12 fftco:          E->F Butterfly phase 3 done
64395    12 fftco:          F->E Start copy back

66419    13 elemco 1/1 p=5: Copying data back
66419    13 elemco 1/1 p=5: Copying data back done
64861    13 elemco 0/0 p=0: Copying data back
64861    13 elemco 0/0 p=0: Copying data back done
66955    13 elemco 1/3 p=7: Copying data back
66955    13 elemco 1/3 p=7: Copying data back done
65397    13 elemco 0/2 p=2: Copying data back
65397    13 elemco 0/2 p=2: Copying data back done
65129    13 elemco 0/1 p=1: Copying data back
65129    13 elemco 0/1 p=1: Copying data back done
66151    13 elemco 1/0 p=4: Copying data back
66151    13 elemco 1/0 p=4: Copying data back done
65665    13 elemco 0/3 p=3: Copying data back
65665    13 elemco 0/3 p=3: Copying data back done
66687    13 elemco 1/2 p=6: Copying data back
66687    13 elemco 1/2 p=6: Copying data back done

64395    14 fftco:          E->F Copy back done
Data after performing the FFT
   28 48061 64509 50109 65533 15420  1020 17468 

64395    14 fftco:          F->E Start the shuffle

66419    15 elemco 1/1 p=5: Starting the inverse FFT do the shuffle
66419    15 elemco 1/1 p=5: Shuffle done
64861    15 elemco 0/0 p=0: Starting the inverse FFT do the shuffle
64861    15 elemco 0/0 p=0: Shuffle done
66955    15 elemco 1/3 p=7: Starting the inverse FFT do the shuffle
66955    15 elemco 1/3 p=7: Shuffle done
65397    15 elemco 0/2 p=2: Starting the inverse FFT do the shuffle
65397    15 elemco 0/2 p=2: Shuffle done
65129    15 elemco 0/1 p=1: Starting the inverse FFT do the shuffle
66151    15 elemco 1/0 p=4: Starting the inverse FFT do the shuffle
65129    15 elemco 0/1 p=1: Shuffle done
65665    15 elemco 0/3 p=3: Starting the inverse FFT do the shuffle
66687    15 elemco 1/2 p=6: Starting the inverse FFT do the shuffle
65665    15 elemco 0/3 p=3: Shuffle done
66151    15 elemco 1/0 p=4: Shuffle done
66687    15 elemco 1/2 p=6: Shuffle done

64395    16 fftco:          E->F Shuffle done
64395    16 fftco:          F->E Start butterfly phase 1

66419    17 elemco 1/1 p=5: Doing butterfly phase 1
64861    17 elemco 0/0 p=0: Doing butterfly phase 1
66955    17 elemco 1/3 p=7: Doing butterfly phase 1
65397    17 elemco 0/2 p=2: Doing butterfly phase 1
65129    17 elemco 0/1 p=1: Doing butterfly phase 1
64861    17 elemco 0/0 p=0: Butterfly phase 1 done
65665    17 elemco 0/3 p=3: Doing butterfly phase 1
65397    17 elemco 0/2 p=2: Butterfly phase 1 done
66151    17 elemco 1/0 p=4: Doing butterfly phase 1
66419    17 elemco 1/1 p=5: Butterfly phase 1 done
66687    17 elemco 1/2 p=6: Doing butterfly phase 1
66955    17 elemco 1/3 p=7: Butterfly phase 1 done
65129    17 elemco 0/1 p=1: Butterfly phase 1 done
65665    17 elemco 0/3 p=3: Butterfly phase 1 done
66151    17 elemco 1/0 p=4: Butterfly phase 1 done
66687    17 elemco 1/2 p=6: Butterfly phase 1 done

64395    18 fftco:          E->F Butterfly phase 1 done
64395    18 fftco:          F->E Start butterfly phase 2

64861    19 elemco 0/0 p=0: Doing butterfly phase 2
65397    19 elemco 0/2 p=2: Doing butterfly phase 2
64861    19 elemco 0/0 p=0: Butterfly phase 2 done
66419    19 elemco 1/1 p=5: Doing butterfly phase 2
66955    19 elemco 1/3 p=7: Doing butterfly phase 2
66419    19 elemco 1/1 p=5: Butterfly phase 2 done
65129    19 elemco 0/1 p=1: Doing butterfly phase 2
65665    19 elemco 0/3 p=3: Doing butterfly phase 2
65129    19 elemco 0/1 p=1: Butterfly phase 2 done
66151    19 elemco 1/0 p=4: Doing butterfly phase 2
66687    19 elemco 1/2 p=6: Doing butterfly phase 2
66151    19 elemco 1/0 p=4: Butterfly phase 2 done
65397    19 elemco 0/2 p=2: Butterfly phase 2 done
66955    19 elemco 1/3 p=7: Butterfly phase 2 done
65665    19 elemco 0/3 p=3: Butterfly phase 2 done
66687    19 elemco 1/2 p=6: Butterfly phase 2 done

64395    20 fftco:          E->F Butterfly phase 2 done
64395    20 fftco:          F->E Start butterfly phase 3

64861    21 elemco 0/0 p=0: Doing butterfly phase 3
66419    21 elemco 1/1 p=5: Doing butterfly phase 3
65129    21 elemco 0/1 p=1: Doing butterfly phase 3
66419    21 elemco 1/1 p=5: Butterfly phase 3 done
66151    21 elemco 1/0 p=4: Doing butterfly phase 3
64861    21 elemco 0/0 p=0: Butterfly phase 3 done
65397    21 elemco 0/2 p=2: Doing butterfly phase 3
66955    21 elemco 1/3 p=7: Doing butterfly phase 3
65665    21 elemco 0/3 p=3: Doing butterfly phase 3
66955    21 elemco 1/3 p=7: Butterfly phase 3 done
66687    21 elemco 1/2 p=6: Doing butterfly phase 3
65397    21 elemco 0/2 p=2: Butterfly phase 3 done
65129    21 elemco 0/1 p=1: Butterfly phase 3 done
66151    21 elemco 1/0 p=4: Butterfly phase 3 done
65665    21 elemco 0/3 p=3: Butterfly phase 3 done
66687    21 elemco 1/2 p=6: Butterfly phase 3 done

64395    22 fftco:          E->F Butterfly phase 3 done
64395    22 fftco:          F->E Start divide by N

66419    23 elemco 1/1 p=5: Dividing by N
66419    23 elemco 1/1 p=5: Divide by N done
64861    23 elemco 0/0 p=0: Dividing by N
64861    23 elemco 0/0 p=0: Divide by N done
66955    23 elemco 1/3 p=7: Dividing by N
66955    23 elemco 1/3 p=7: Divide by N done
65397    23 elemco 0/2 p=2: Dividing by N
65397    23 elemco 0/2 p=2: Divide by N done
65129    23 elemco 0/1 p=1: Dividing by N
65129    23 elemco 0/1 p=1: Divide by N done
66151    23 elemco 1/0 p=4: Dividing by N
66151    23 elemco 1/0 p=4: Divide by N done
65665    23 elemco 0/3 p=3: Dividing by N
65665    23 elemco 0/3 p=3: Divide by N done
66687    23 elemco 1/2 p=6: Dividing by N
66687    23 elemco 1/2 p=6: Divide by N done

64395    24 fftco:          E->F Divide by N done
64395    24 fftco:          F->E Start copy back

66419    25 elemco 1/1 p=5: Copying data back
66419    25 elemco 1/1 p=5: Copying data back done
64861    25 elemco 0/0 p=0: Copying data back
64861    25 elemco 0/0 p=0: Copying data back done
66955    25 elemco 1/3 p=7: Copying data back
66955    25 elemco 1/3 p=7: Copying data back done
65397    25 elemco 0/2 p=2: Copying data back
65397    25 elemco 0/2 p=2: Copying data back done
65129    25 elemco 0/1 p=1: Copying data back
65129    25 elemco 0/1 p=1: Copying data back done
66151    25 elemco 1/0 p=4: Copying data back
66151    25 elemco 1/0 p=4: Copying data back done
65665    25 elemco 0/3 p=3: Copying data back
65665    25 elemco 0/3 p=3: Copying data back done
66687    25 elemco 1/2 p=6: Copying data back
66687    25 elemco 1/2 p=6: Copying data back done

64395    26 fftco:          E->F Copy back done
Data after performing the Inverse FFT
    0     1     2     3     4     5     6     7 

64395    26 fftco:          F->E Start close down

66419    27 elemco 1/1 p=5: Closing down
66419    27 elemco 1/1 p=5: Close down done
64861    27 elemco 0/0 p=0: Closing down
64861    27 elemco 0/0 p=0: Close down done
66955    27 elemco 1/3 p=7: Closing down
66955    27 elemco 1/3 p=7: Close down done
65397    27 elemco 0/2 p=2: Closing down
65397    27 elemco 0/2 p=2: Close down done
65129    27 elemco 0/1 p=1: Closing down
65129    27 elemco 0/1 p=1: Close down done
66151    27 elemco 1/0 p=4: Closing down
66151    27 elemco 1/0 p=4: Close down done
65665    27 elemco 0/3 p=3: Closing down
65665    27 elemco 0/3 p=3: Close down done
66687    27 elemco 1/2 p=6: Closing down
66687    27 elemco 1/2 p=6: Close down done

64395    28 fftco:          E->F Close down done
End of demo
0.210> 
0.000> loki 9

Loki fft demo version 2 entered

The principal 512th root of unity is 157 = 0009D

Initial data
    0     1     2     3     4     5     6     7 
    8     9    10    11    12    13    14    15 
   16    17    18    19    20    21    22    23 
   24    25    26    27    28    29    30    31 

Data after performing the FFT
65279 33612 53306   572 55468 34328 35024 43282 
34141 61547 19599 40098 22616 29665 42955 51991 
57800 50816 44555 19224 53222 52432 39732 52171 
52122 32105 46000 45288   656   943 39865  7146 

Data after performing the inverse FFT
    0     1     2     3     4     5     6     7 
    8     9    10    11    12    13    14    15 
   16    17    18    19    20    21    22    23 
   24    25    26    27    28    29    30    31 

End of demo
0.390>
*/
