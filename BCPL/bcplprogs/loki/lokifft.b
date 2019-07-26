/*

This is a program to compute the fast Fourier transform of 512 values
using all 128 of the processor cores in the loki chip. This
implementation cheats by representing the cores as coroutines with
with bodies written in BCPL. The channels are also simplified and
implemented using coroutines controlled by the cores.

Implemented by Martin Richards (c) 18 March 2019

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
word buffer and a core is held up if it tries to read from and empty
buffer or write to a full one.

In this demonstration the polynomial is of degree 511 having 512
coefficients a0 to a511. Each core performs the calculation for just 4
of these element positions using a separate coroutine for each
position. These coroutines are called element coroutines and they
communicate with other element coroutines using channels 0 to 3 of the
core on which they are running.

In addition to the 128 core coroutines and their element coroutines,
there is a main coroutine called fftco that controls the
demonstration. It does this by sending simple integer commands to
cores. It starts by creating a 512 element array called datav
initialised with the values 0 to 511. It then sets the global variable
w to the smallest 512th root of unity. Note that w is accessible to
all the core and element coroutines. fftco continues by sending the
command 1 to each core to load the appropriate element values into
local variables of their element coroutines. Next it issues the
command 2 to copy these values back into datav and print their
values. fftco then sends the command 3 to the core coroutines to cause
the element coroutines to perform the perfect shuffle. The result of
the shuffle can then be optionally transferred to datav and
printed. fftco next sends command 4 to the core coroutines to cause
them to perform the first phase of butterfly operations. Command 4 is
then repeated 8 more times to complete all the remaining butterfly
phases. The resulting element values are then copied back into datav
by the command 2 and printed. These are the Fourrier transform of the
original data.

As a check, fftco then performs the inverse Fourrier transform by
setting w to w^-1 and performing the same sequence of commands except
that at the end it issues the command 5 to cause all the element
values to be divided by 512. The resulting set of values should now be
the original data namely 0, 1,...,511.

*/

GET "libhdr"

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

  sep        // Used in the butterfly operation
  k

  prupb      // min(N-1, 31) specifying how many elements to print.

  datav      // The vector holding the test data.
  datavupb   // The upper bound of datav.

  corev      // Vector for the 128 core control blocks
  corevupb   // The upper bound of corev.

  commcount  // 214 The number of times a coroutine has waited for a command
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
}

//LET abort() BE RETURN  // This is to disable the abort function.

LET start() = VALOF
{ LET argv = VEC 50

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

  writef("*nLoki fft demo entered*n")

  fftco := createco(fftcofn, 500)

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

AND deletecore(id) BE
{ LET corecb = corev!id
  IF corecb DO deleteco(c_cptr!corecb)
}

// Scheduling functions

AND schedule() = VALOF
{ // This function selects the next coroutine to be given
  // control.

  // If lolist is not empty, the first coroutine on the list
  // is dequeued and given control.

  // If lolist is empty but hilist is not, control is given
  // to the first coroutine in hilist.

  // lolist and hilist will never both be empty when
  // schedule is called.

  // schedule returns the value passed to this coroutine when it
  // next gains control.

  prlists()

  IF lolist DO
  { // Extract the first coroutine in lolist.
    LET cptr  = lolist!1
    lolist := lolist!0 // The link is always at offset zero.
    // loliste points to the last link field in the list and
    // if lolist has just become empty loliste needs to be corrected.
    UNLESS lolist DO loliste := @lolist

    //prcortn()
    //writef("%i5 schedule: Giving control to a coroutine %n in lolist*n",
    //        commcount, cptr)
//abort(7654)
    RESULTIS resumeco(cptr, 0)
  }

  IF hilist DO
  { // Extract the first coroutine in hilist.
    LET cptr  = hilist!1
    hilist := hilist!0 // The link is always at offset zero.
    // hiliste points to the last link field in the list and
    // if hilist has just become empty hiliste needs to be corrected.
    UNLESS hilist DO hiliste := @hilist

    //prcortn()
    //writef("%i5 schedule: Giving control to a coroutine %n in hilist*n",
    //        commcount, cptr)
//abort(8765)
    RESULTIS resumeco(cptr, 0)
  }

  // lolist and hilist are both empty.
  prcortn()
  writef("%i5 schedule: ERROR: Called when lolist and hilist both empty*n",
          commcount)
  abort(999)
  RESULTIS 0
}

AND resumelosubco(cptr, val) = VALOF
{ // This places the current coroutine on the end of lolist
  // and then calls resumeco(cptr, val) returning its result.
  // This will regain control before any of the coroutine in
  // hilist.
  LET link, co = 0, currco
  LET node = @link
  !loliste := node
  loliste  := node
  RESULTIS resumeco(cptr, val)
}

AND resumehisubco(cptr, val) = VALOF
{ // This places the current coroutine on the end of hilist
  // and then calls resumeco(cptr, val) returning its result.
  // It will not regain control until lolist is empty.
  LET link, co = 0, currco
  LET node = @link
  !hiliste := node
  hiliste  := node
  RESULTIS resumeco(cptr, val)
}

AND docommand(c) BE
{ commcount := commcount+1

  FOR id = 0 TO corevupb DO
  {
    //prcortn()
    //writef("%i5 fftco:        Sending command %n to core %n*n",
    //        commcount, c, id)
    resumehisubco(corev!id!c_cptr, c)
  }
  //prcortn()
  //writef("%i5 fftco:        All core coroutines have completed command %n*n",
  //        commcount, c)
}

AND fftcofn(args) = VALOF
{ // This is the main function of the fft coroutine fftco. It is given
  // control by start using callco and returns at the end using cowait.
  // This coroutine is the controller of the FFT algorithm.
  // When started only the variables ln and walsh have been set. It
  // initialises some global variables then creates the core coroutines
  // and starts them using resumehisubco. This leaves each of them
  // each waiting for a command.

  // They are then each given a command as the value passed by a call
  // of resumehisubco.

  // Each core coroutine creates and starts its four element coroutines.
  // On receiving a command, a core coroutine passes it to each or its
  // element coroutines in turn using resumehisubco. This appends the core
  // coroutine to the end of lolist before transferring control.
  // In due course the element coroutine will suspend itself
  // either (1) waiting to read from a channel with an empty buffer, or
  // (2) waiting to write to a channel with a full buffer, or (3) waiting
  // for another command from its core coroutine. In each case
  // it calls schedule to transfer control to another coroutine.

  // Whenever schedule is called lolist and hilist will not both be
  // empty. It lolist is non empty control is given to the first
  // coroutine on the list, otherwise control is given to the first
  // coroutine on hilist.
  // Commands are sent to the core
  // coroutines by fftco using resumehisubco which ensures that fftco
  // will ultimately regain control. Likewise core coroutines send
  // commands to their own element coroutines using resumelosubco
  // ensuring that they will regain control before control is given
  // to fftco.

  // When fftco regains control after sending a command to all the core
  // coroutines, it will know that the execution of the command has
  // been completed by all the element coroutines and that they and the
  // core coroutines are all waiting for another command.

  // Note that after fftco has sent a command to every core core
  // coroutine, every element coroutine waiting to comunicate via a
  // channel will ultimately be released, so, in due course, all
  // element coroutines will be waiting for another command. The same
  // applies to the core coroutines.

  // When all the commands of the FFT demonstrations have been completed.
  // fftco will return control to the root coroutine using cowait.

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

  commcount := 0
  lolist, loliste := 0, @lolist
  hilist, hiliste := 0, @hilist
  FOR id = 0 TO corevupb DO initco(corecofn, 500, id)

  TEST w=1
  THEN writef("Performing the Walsh transform on %n words of data*n", N)
  ELSE writef("*nThe principal %nth root of unity is %n = %x5*n*n",
               N, w, w)

  // All the core coroutines have been created, so start them.

  FOR id = 0 TO corevupb DO resumehisubco(corev!id!c_cptr, 0)

  //prcortn()
  //writef("%i5 fftco:        All core coroutines are waiting for a command*n",
  //        commcount)

  // All the core coroutines are now waiting for their first command.

  docommand(1)

  FOR i = 0 TO N-1 DO datav!i := -1

  docommand(2) // Copy inital data back into datav.

  writef("Initial data*n")
  pr()

  docommand(3) // Perform the perfect shuffle.

  IF tracing DO
  { docommand(2) // Copy shuffled data back into datav.
    writef("Shuffled data*n")
    pr()
  }

  sep, k := 1, ln

  FOR i = 1 TO ln DO
  { docommand(4)
    sep, k := 2*sep, k-1
    IF tracing DO
    { docommand(2)
      writef("Values after a butterfly phase %n*n", i)
      pr()
    }
//abort(1112)
  }

  docommand(2)
  writef("Data after performing the FFT*n")
  pr()
//abort(1001)


// We now perforn the inverse FFT

  w := inv(w)

  // Perform the perfect shuffle.
  docommand(3)

  IF tracing DO
  { docommand(2)
    writef("Data after the perfect shuffle*n")
    pr()
  }

  sep, k := 1, ln

  FOR i = 1 TO ln DO
  { docommand(4)
    sep, k := 2*sep, k-1
    IF tracing DO
    { docommand(2)
      writef("Values after a butterfly phase %n*n", i)
      pr()
    }
//abort(1000)
  }

  // Divide all elements by N.
  docommand(5)

  docommand(2)
  writef("Data after performing the inverse FFT*n")
  pr()


end:  // Close down the demonstration using command 6 before deleting
      // the core coroutines
  docommand(6)

fin:
  FOR id = 0 TO corevupb IF corev!id DO deletecore(id)
  IF corev DO freevec(corev)
  IF datav DO freevec(datav)

  RESULTIS 0
}

// The main functions for the core and element coroutines.

AND corecofn(args) = VALOF
{ // args -> [id]

  // This function is the body of all core coroutines. As soon
  // as a core coroutine is created, it creates four element
  // coroutines and returns control to fftco using cowait. It
  // next gain control when fftco asks it to enter its command
  // loop ba a call or resumeco. The FFT algorithm is controlled
  // by fftco sending a sequence of commands to the core
  // coroutines. The commands are single integers sent via
  // channel 5. They are as follows.

  //  1  This causes the core to tell element work coroutines to
  //     obtain their element values from datav then wait for
  //     another command.
  //
  //  2  This causes the core to tell its element coroutines to
  //     copy its elements values to the appropriate locations
  //     of datav, the wait for another command.
  //
  //  3  Perform the perfect shuffle.
  //
  //  4  Perform the butterfly operations.
  //
  //  5  Divide all elements by N.
  //
  //  6  Close down releasing all its allocated space, ready
  //     to be deleted ready by the fft coroutine.

  // Core i used its four element coroutines to perform the
  // computation of elements at positions 4i to 4i+3.

  // We now declare the control block for this core coroutine.

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

  // Create the element coroutines for this core, giving them the
  // core's coroutine, its id and the appropriate channel numbers.
  wrkco0 := initco(elemcofn, 500, currco, id, bufv, 0)
  wrkco1 := initco(elemcofn, 500, currco, id, bufv, 1)
  wrkco2 := initco(elemcofn, 500, currco, id, bufv, 2)
  wrkco3 := initco(elemcofn, 500, currco, id, bufv, 3)

  UNLESS wrkco0 & wrkco0 & wrkco0 & wrkco0 DO
  { writef("More space needed*n")
    abort(999)
  }

  // All four element coroutines are waiting to be started.
  cowait(0)   // To return from initco(...).

  // This coroutine has just been asked to start its 

  // Start the four element coroutines.
  resumelosubco(wrkco0, 0)
  resumelosubco(wrkco1, 0)
  resumelosubco(wrkco2, 0)
  resumelosubco(wrkco3, 0)

  // Eack element coroutines is waiting for its first command.

  // Enter the command loop.
  { LET command = schedule() // Get the next command from fftco.

    SWITCHON command INTO
    { DEFAULT:
        writef("%i5 Core  %i3: Unknown command = %n*n", 
                commcount, id, command)
        abort(999)
        ENDCASE

      CASE 1: // Get element data values.
      CASE 2: // Copy element data into datav.
      CASE 3: // Perform the perfect shuffle.
      CASE 4: // Do the butterfly operations.
      CASE 5: // Divide the element values by N.
      CASE 6: // Free all allocated workspace.

        // Send the command to each element coroutine.
        // They will each be waiting for a command.

        resumelosubco(wrkco0, command)
        resumelosubco(wrkco1, command)
        resumelosubco(wrkco2, command)
        resumelosubco(wrkco3, command)

        // All element coroutines have completed the current command
        
        IF command=6 BREAK
    }
  } REPEAT

  // Release all the space allocated by this core
  deleteco(wrkco0)
  deleteco(wrkco1)
  deleteco(wrkco2)
  deleteco(wrkco3)

  schedule()  // Return control to fftco.

  abort(998)  // Should not be reached.
  RESULTIS 0
}

AND elemcofn(args) = VALOF
{ // args -> [coreco, id, bufv, chno]

  // This coroutine performs the computations necessary for its
  // data element. It is given commands by its controlling core
  // coroutine (coreco). When this coroutine has completed the
  // current command it calls schedule which ultimately returns
  // control to its controlling core coroutine.

  LET x = 0    // The element value processed by this element coroutine

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

  // It has now been started and will try to read its first command.

  // Enter the command loop

  { //Tell the core coroutine that this element coroutine is ready for
    // another command, and wait for the command to arrive.
    LET command =  schedule()

    // The core coroutines use resumelosubco to pass commands to the
    // element coroutines, calling schedule will ultimately return control
    // back to the element coroutine's core coroutine.

    //prcortn()
    //writef("%i5 elemco   %i3/%n p=%n: Received command %n sep=%n k=%n*n",
    //        commcount, id, chno, p, command, sep, k)

    SWITCHON command INTO
    { DEFAULT:
        writef("ERROR: Unknown command in element coroutine*n")
        abort(999)
        prcortn()
        writef("%i5 elemco %i3/%n p=%n: Calling schedule()*n",
                commcount, id, chno, p)
        LOOP

      CASE 1: // Get the element value from datav.
        x := datav!p
        //prcortn()
        //writef("%i5 elemco %i3/%n: Set x=%n from datav!%n*n",
        //        commcount, id, chno, x, p)
        LOOP

      CASE 2: // Copy the element value into datav.
        datav!p := x
        LOOP

      CASE 3: // Perform the perfect shuffle.
        q := reverse(p, ln)
        TEST p=q
        THEN { //prcortn()
               //writef("%i5 elemco %i3/%n p=%n: No swap needed since p=q*n",
               //        commcount, id, chno, p)
             }
        ELSE {
               send(q/4, q & 3, x)
               x := recv(buf)
             }
        LOOP

      CASE 4: // Perform one butterfly operation.
      { q := p XOR sep

//abort(7777)
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
               LET we = pow(w, e)
               x := mul(x, we)
               send(q/4, q & 3, x)
               x := neg(x)
             }

        x := add(x, recv(buf))
        LOOP
      }

      CASE 5: // Divide the element by N.
        x := div(x, N)
        LOOP

      CASE 6: // Free all allocated space
        BREAK
    }
  } REPEAT

  // Free all allocated space.
  // Nothing to free.
  schedule()

  abort(997) // Should never br reached.
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
    resumelosubco(cptr, 0)
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
  //writef("%i5 recv: extracted value %n*n", commcount, val)

  IF buf_wrwaitlist!buf DO
  { // The buffer is no longer full and there are coroutines (typically
    // just one) waiting to send data to this buffer. These are released
    // by moving them to them of lolist.

    !loliste := buf_wrwaitlist!buf
    loliste := buf_wrwaitliste!buf

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

0.000> lokifft 3 -t
Loki fft demo entered

The principal 8th root of unity is 16 = 00010

Initial data
    0     1     2     3     4     5     6     7 

Shuffled data
    0     4     2     6     1     5     3     7 

Values after a butterfly phase 1
    4 65533     8 65533     6 65533    10 65533 

Values after a butterfly phase 2
   12 64509 65533  1020    16 64509 65533  1020 

Values after a butterfly phase 3
   28 48061 64509 50109 65533 15420  1020 17468 

Data after performing the FFT
   28 48061 64509 50109 65533 15420  1020 17468 

Data after the perfect shuffle
   28 65533 64509  1020 48061 15420 50109 17468 

Values after a butterfly phase 1
   24    32 65529 63489 63481 32641  2040 32641 

Values after a butterfly phase 2
   16    24    32    40 65521 65281 61441     1 

Values after a butterfly phase 3
    0     8    16    24    32    40    48    56 

Data after performing the inverse FFT
    0     1     2     3     4     5     6     7 

End of demo
0.040>
0.000> loki 9

Loki fft demo entered

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
1.070>
*/
