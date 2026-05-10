/*
This file is the source of the Cintcode interpreter used by both the single
threaded BCPL Cintcode system called cintsys and the Interpretive version of the
Tripos operating system called cintpos. The differences between these two
systems are controlled by #defining the macros CINTSYSyes and CINTPOSyes.

(c) Copyright:  Martin Richards  05 Aug 2023

This is the source code for the for the three versions of the Cintcode
interpreter. These are cinterp, fasterp and rasterp. Conditional compilation
controls which version is being built. The macros CINTSYyes and CINTPOSyes
specify whether the interpreter is for cintsys/rastsys or cintpos/rastpos.

for both the BCPL Cintsys and Cintpos systems. It is written in C
and designed to run on most machines with Unix-like C libraries. It is designed
to work for both 32 and 64 bit BCPL. It is used to create the rastering version
of the interpreter. The macros CINTERPyes, FASTERyes and RASTERyes control
whether to create cinterp.o, fasterp.o or rasterp.o. If a 64-bit BCPL system
is being created the macro TARGET64 will be defined.

It is currently being modified to be the main program for both cintsys and
cintpos with the differences being controlled by the #defined macros CINTSYSyes
and CINTPOSyes. Currently cinterp.c in directory BCPL/cintcode/sysc/ and in
Cintpos/cintpos/sysc/ are different but are being coalesced and will eventually
be identical files.

This file is used to create three versions of the Cintcode interpreter
cinterp.o, fasterp.o and rasterp.o depending on which of the macro names
CINTERPyes, FASTERPyes or RASTERPyes have been #defined. Both cintsys and
cintpos contain two interpreters. The normal ones combine cinterp.o and
fasterp.o, but the alternative versions called rastsys and rastpos combine
cinterp.o and rasterp.o. The interpreter cinterp.o provides many debugging aids
while fasterp.o provides a faster interpreter by leaving out most of the
debugging aids. When fasterp.o is replaced by rasterp.o the system is capable of
generating data allowing the creation of an image of how memory was used
during the execution of a program. See rast2bmp.b and rast2wav.b for more
details.

History:

05/08/2023
Made a major change to make cintmain.c, cintmain.h and cinterp.c identical
source files for the executables: cintsys, cintpos, cintsys64, cintpos64,
rasterp, rastpos, rasterp64 and rastpos64. These version are controlled by
#defining appropriate macro names.

21/03/2020
Making many changes to improve the treatment of stack overflow and out of
range references to memory.

07/01/2020
I have just reorganised the use of macros to control the compilation of
cintsys.c and cinterp.c. cintsys.o is the compilation of cintsys.c without
CINTERPyes, FASTERPyes or RASTERPyes defined, but it will have either
CINTSYSyes or CINTPOSyes defined.  cinterp.o is the compilation of cinterp.c
with CINTERPyes defined.  fasterp.o is the compilation of cinterp.c with
FASTERPyes defined.  rasterp.o is cinterp.c compiled with RASTERPyes
defined. cinterp.o defines the function interpret and both fasterp.o and
rasterp.o contain alternative interpreters both called cintasm.  bin/cintsys
includes cintsys.o, cinterp.o and fasterp.o. while bin/rasterp includes
cintsys.o, cinterp.o and rasterp.o.

cintsys sets tallyupb and allocates tallyv both of which are used by cinterp.o
and rasterp.o. For efficiency reasons both of these have private copies of
tallyupb held in separate static variables called tallylimb. tallylimb is set
by the call sys(Sys_tally, TRUE) which turns on statistics gathering or
rastering. Note that Cintcode instructions with pc values in the range 0 to
tallylimb have their execution counts held in tallyv.

08/11/2019
lastWp, lastWg and lastst have been renamed sysp, sysg and sysst, and are set
by dosys when executing a Cintcode SYS instruction.  When not in dosys sysp is
zero. When the Cintcode memory is dumped by dumpmem, these variables are copied
into the rootnode in order to be visible to the commands dumpsys, dumppos,
sysdebug and posdebug. If a memory dump is made when executing a SYS
instruction valid P and G pointers for the BCPL sys call will be available. The
treatment of st is still under development. It should probably be the setting
of st at the time of the fault.

30/09/2019
Added case fl_64to32: in doflt to convert 64 to 32 bit floating point. Only
used when running under 64 bit BCPL.

03/09/2019
Making changes to cause Cintpos to run with 32 and 64 bit BCPL on both 32 and
64 bit machines. This requires changes in BCPLPATH, BCPL64PATH, POSPATH and
POS64PATH and most of the compilation command commands such as bc, bc32, bc64,
bs, bs32, bs64, etc. The values of POSPATH and POS64PATH will be different
specifying either cin/ or cin64/, but POSROOT and POS64ROOT will typically
specify the same directory. Similarly POSHDRS and POS64HDRS will be the same,
as will POSSCRPITS and POS64SCRIPTS. If the BCPL compiler is generating 64 bit
Cintcode and the TO field starts with cin/ it will be automatically replaced
by cin64/. This requires a minor change to com/bcplfe.b.

When using cintsys or cintsys64 to compile BCPL programs for cintpos or
cintpos64 it is necessary to specify PATHHDRS or POS64HDRS as the headers
environment variable when invoking the compiler. It will otherwise mistakenly
use BCPLHDRS or BCPL64HDRS.  The compilation scripts in BCPL/cintcode/ and
Cintpos/cintpos/ specify different headers. Currently libhdr.h and bcplfecg.h
are the same for both 32 and 64 bit BCPL and for both cintsys and cintpos.

30/03/16
Added rootnode field Rtn_system. =1 for Cintsys, =2 for Cintpos, =0 otherwise.

03/03/14
The Cintcode memory must have read, write and execute permissions.  Allocating
memory using malloc seems to provide this under windows but not under all
versions of Linux, so under Linux the Cintcode memory is now allocated using
mmap.

15/04/13
Added sys(Sys_opengl,...) to provide an interface to the OpenGL graphics library.

22/06/12
Added sys(Sys_sdl,...) to provide an interface to the SDL graphics library.

06/03/12
Implemented debug counts by defining incdcount(n) and sys(Sys_incdcount,n).
There were related changes to cintmain.h, cintpos.h, cintpos.c g/libhdr.h and 
com/dcount.b

07/02/11
Changed the format of BCPL filenames to use '/' (or '\') as separators of file
name components. Such names are converted to target system names before
use. The targets are UNIXNAMES for Linux style, WINNAMES for Windows style and
VMSNAMES for VMS style. Separators in path list can be either semicolons or
colons, except under Window when only semicolons are allowed.

09/04/2010
Put time the time stamp (days, msecs, filler) in the rootnode
approximately every msec.


07/04/10
Change BCPL epoc of 1 Jan 1978 to 1 Jan 1970 to be the same
as that used by Unix and Windows. Made corresponding changes
to blib.
Changed INT32 and INT64 to BCPLINT32 and BCPLINT64
Changed CHAR to UNSIGNEDCHAR

29/03/10
Made systematic changes to measure time in msecs rather than ticks.
This is equivalent to setting tickspersecond to 1000, but the code is
simpler.

24/02/10
Began to implement much stricter control of access to memory values shared
between threads, and, for systems that provide priority scheduling, the
Cintcode interpreter thread is given a lower priority than any thread
defined in devices.c. For further details see below.   

05/02/10
Added the low level trace functions trpush, settrcount and gettrcount,
together with the sys operations Sys_trpush, Sys_settrcount and Sys_gettrval.
These can all be safely called from any thread. The circular trace buffer
can hold 4096 values.

15/04/09
Set the pathvar field (rtn_pathvar) in rootnode to the environment name (as a
BCPL string) given by the -cin option of cintsys or cintpos. It gives the
directories to be searched by loadseg and is also used by the c command when
searching for command-command files.  The command

c filename args

first searched for filename in the current directory, then for s/filename in
the current directory and the directories specified by the pathvar environment
variable. Note that there is conventionally a directory called cin that holds
normal commands. The directory cin/syscin holds system commands and cin/holds
command-commands.

08/01/09
Added the -cin option to specify the name of the pathvar environment variable
that gives the list of directories containing cin files.  The default name is
"POSPATH". Cincode modules are loaded using loadseg which first searches the
current working directory followed by the directories specified by the pathvar
environment variable. You can observe the search process using the cintpos
option -f.

10/11/06
Added the -slow option to force using the slow interpreter (interpret) all
the time. This is useful if there seems to be a bug in cinterp or fasterp.

08/11/06
Added the -d option to set the dump flag in the rootnode (equivalent to calling
the command: dumpmem on). This will cause the entire Cintcode memory to be
dumped to DUMP.mem in a compacted form if the Cintcode system returns with a
non zero return code. The file DUMP.mem can be printed in a readable form using
the dumpsys command (always assuming you have a working BCPL system).

07/11/06
Added -v option to trace the progress of the booting process. This is primarily
to help people a new installation of Cintcode BCPL.
Added -vv option to trace the progress of the booting process. It behaves like
-v above but also does some Cintcode instruction level tracing.
Added -f option to cintsys to trace the use of environment variables such as
BCPLPATH by pathinput. This is helps to track down installation problems.

26/07/06
Made changes to cause loadseg to look first in the BCPLPATH directories then
the current directory and finally in the cin directory of BCPLROOT.  To
simplify this change, the first argument of pathinput was changes from a BCPL
string to a C string, together with related changes.  There are now three work
C strings chbuf1, chbuf2 and chbuf3. Made cintsys.c more compatible with
cintpos.c

18/01/06
Added -s, -c and -- parameters to cintpos, to prepend characters before the
start of stdin (as suggested by Dave Lewis).

01/01/06
Change -m and -t options to specify sizes in words (not thousands of words).

22/06/00
Made a correction to muldiv to stop it looping on #x8000000 This Cintcode
instruction MDIV is now not invoked since it sometimes causes a floating
point(!!) exception on a Pentium.

13/09/96
Changed to use usleep instead of sleep for finer grain clock

Notes on memory visibility between threads

Although the naive implementation of Cintpos works fine on Pentium machines
problems have be encountered when transfering the system to high performance
modern processors such as the Itanium. This is believed to be caused by
insufficient control over access to memory shared between threads and timing
problems caused by inappropriate scheduling of threads.

The original Cintpos interpreter inspected the shared variable irq every time
it interpreted an instruction. irq is set whenever a device thread wishes to
interrupt the interpreter. Unfortunately, after a device thread has set irq,
the interpreter may not notice the change (due to caching and other features of
the system). For these two threads to see the same value in irq, its must be
held in memory and we must obey basic Pthread rules about memory visibility.

One of these rules (taken from "Programming with POSIX Threads" by David
R. Butenhof is as follows:

Whatever memory values a thread can see when it unlocks a mutex, either
directly or by waiting on a condition variable, can also be seen by any thread
that later locks the same mutex. Data written after the mutex is unlocked may
not necessarily be seen by the thread that locks the mutex, even if the write
occurs before the lock.

To achieve guaranteed portability, it is necessary for the interpreter to only
access irq under control of the irq mutex. Locking and unlocking the irq mutex
on every interpreted instruction slows the interpreter by about a factor of 10.

Two possible solutions are (a) let device threads send a signal (eg SIGUSR1) to
the interpreter and make its handler set irq (now a private interpreter
variable), or (b) only inspect irq once every 100 or so instructions. I
currently favour option (b) because of my lack of detailed understanding of how
signals work.

To implement (b) a new counter (icount) has been introduced which is
decremented every time an instruction is interpreted. When it becomes negative,
if interrupts are enabled, the irq fifo is inspected and the interrupt service
routine entered. Since the fifo is inspected fairly rarely there is no need
for the variable irq. It is adequately efficient to compare the fifo pointers
to test the presents on an interrupt.

While the interpreter is waiting in sys_waitirq it is not executing
instructions. When it is released (by irq_cv) icount must be set to zero so any
pending interrupts can be dealt with.
*/

#include <stdio.h>
#include <stdlib.h>

#ifdef CINTPOSyes
#include <pthread.h>
#endif

/* cintmain.h contains machine/system dependent #defines  */
#include "cintmain.h"

#ifdef CINTERPyes
#define ADDRCHKyes
#define TRACINGyes
#define TALLYyes
#define WATCHyes
#define COUNTyes
#endif
  
#ifdef RASTERPyes
#define ADDRCHKyes
#define TRACINGyes
#define TALLYyes
#define WATCHyes
#define COUNTyes
#endif
  
#ifdef FASTERPyes
// Do not turn on any debugging aids
#endif

// Only one of CINTERPyes, RASTERPyes and FASTERPyes may be defined.

// Variables defined in cintsys or cintpos
extern BCPLWORD  result2;
extern int       tracing;
extern BCPLWORD  memupb;
extern BCPLWORD *tallyv;
extern BCPLWORD  tallyupb; // The upb of tallyv

extern BCPLFLOAT N2F(BCPLWORD  x);
extern BCPLWORD  F2N(BCPLFLOAT x);

//UBCPLWORD memupbb;

static UBCPLWORD tallylimb=0;
// tallylimb is a private to this interpreter for efficiency reasons.
// Its value is either 0 or tallyupb, set by Sys_tally.

#define Gn_currco      7
#define Gn_result2    10

// CINTCODE op codes

#define F_0       0

#define F_fltop   1
#define F_brk     2
#define F_k0      0
#define F_lf     12
#define F_lm     14
#define F_lm1    15
#define F_l0     16
#define F_fhop   27
#define F_jeq    28

#define F_k      32
#define F_kh     33
#define F_kw     34
#define F_k0g    32
#define F_k0g1   (F_k0g+32)
#define F_k0gh   (F_k0g+64)
#define F_s0g    44
#define F_s0g1   (F_s0g+32)
#define F_s0gh   (F_s0g+64)
#define F_l0g    45
#define F_l0g1   (F_l0g+32)
#define F_l0gh   (F_l0g+64)
#define F_l1g    46
#define F_l1g1   (F_l1g+32)
#define F_l1gh   (F_l1g+64)
#define F_l2g    47
#define F_l2g1   (F_l2g+32)
#define F_l2gh   (F_l2g+64)
#define F_lg     48
#define F_lg1    (F_lg+32)
#define F_lgh    (F_lg+64)
#define F_sg     49
#define F_sg1    (F_sg+32)
#define F_sgh    (F_sg+64)
#define F_llg    50
#define F_llg1   (F_llg+32)
#define F_llgh   (F_llg+64)
#define F_ag     51
#define F_ag1    (F_ag+32)
#define F_agh    (F_ag+64)
#define F_mul    52
#define F_div    53
#define F_mod    54
#define F_xor    55
#define F_sl     56
#define F_ll     58
#define F_jne    60

#define F_llp    64
#define F_llph   65
#define F_llpw   66
#define F_add    84
#define F_sub    85
#define F_lsh    86
#define F_rsh    87
#define F_and    88
#define F_or     89
#define F_lll    90
#define F_jls    92

#define F_l      96
#define F_lh     97
#define F_lw     98
#define F_rv    116
#define F_rtn   123
#define F_jgr   124

#define F_lp    128
#define F_lph   129
#define F_lpw   130
#define F_lp0   128
#define F_sys   145
#define F_swb   146
#define F_swl   147
#define F_st    148
#define F_st0   148
#define F_stp0  149
#define F_goto  155
#define F_jle   156

#define F_sp    160
#define F_sph   161
#define F_spw   162
#define F_sp0   160
#define F_s0    176
#define F_xch   181
#define F_gbyt  182
#define F_pbyt  183
#define F_atc   184
#define F_atb   185
#define F_j     186
#define F_jge   188

#define F_ap    192
#define F_aph   193
#define F_apw   194
#define F_ap0   192

#define F_xpbyt 205
#define F_lmh   206
#define F_btc   207
#define F_nop   208
#define F_a0    208
#define F_rvp0  211
#define F_st0p0 216
#define F_st1p0 218

#define F_mw    223

#define F_a     224
#define F_ah    225
#define F_aw    226
#define F_l0p0  224
#define F_s     237
#define F_sh    238

#define F_mdiv  239
#define F_chgco 240
#define F_neg   241
#define F_not   242
#define F_l1p0  240
#define F_l2p0  244
#define F_l3p0  247 
#define F_l4p0  249

#define F_selld 254
#define F_selst 255
#define F_255   255

#define sf_none    0     // Assignment operators
#define sf_vecap   1
#define sf_fmul    2
#define sf_fdiv    3
#define sf_fmod    4
#define sf_fadd    5
#define sf_fsub    6
#define sf_mul     7
#define sf_div     8
#define sf_mod     9
#define sf_add    10
#define sf_sub    11
#define sf_lshift 12
#define sf_rshift 13
#define sf_logand 14
#define sf_logor  15
#define sf_eqv    16
#define sf_xor    17

// mem  is the pointer to the cintcode memory.
// regs is the position in the Cintcode memory where the initial
//      values of the Cintcode registers to be found.
//
// The interpreter executes Cintcode instructions and returns with
// an integer result as follows:

//    -4      Enter sadebug after tracing n instructions, setup by
//            the TR n debug command.
//    -3      Enter sadebug just before executing a 
//            the current instruction
//    -2      sys(Sys_dumpmem) cause a memory dump to DUMP.mem
//    -1      sys(Sys_setcount, val) called, re-enter immediately
//     0      sys(Sys_quit, 0) called
//     1      Non existent instruction
//     2      Brk instruction
//     3      Zero count
//     4      PC too large or negative
//     5      Division by zero
//    10      Cintasm single step trap
//    11      Contents of the watch address has changed
//    12      Memory address too large or negative
//    13      SIGINT received
//    14      Unknown floating point operation
//    15
//    16      P pointer too large or negative
//    17      SIGSEGV occurred
//    18      End of instruction tracing
//     n      sys(Sys_quit, n) called
//
// On return the Cintcode registers are dumped back in the vector regs

// The following declarations and definitions depend on which of
// CINTERPyes, FASTERPyes or RASTERPyes are defined.
// Note that the macros Rb, Rh, Rw, memchp, memchpc, memchkw and  memchkb
// can only be used inside an interpreter function because they may
// reference the local variables: tallylimb, maxp, memupbb or memupbw.

#ifdef FASTERPyes
// Compiling cinterp.o and defining the function cintasm.
// Note that cintasm used to written in assembly language but
// is now implemented in C.

#define INTERPRETFN cintasm
BCPLWORD *watchaddr=0, watchval=0; // These are only defined
int SIGSEGVoccurred = 0;           // in fasterp.o.
                                   // cinterp.o and rasterp.o use
                                   // extrn references to these variables.

// Dummy rastering macros for fasterp.o
#define Rb(a)
#define Rh(a)
#define Rw(a)

// Dummy memory check macros for fasterp.o
#define memchkp(a)
#define memchkpc(a)
#define memchkw(a)
#define memchkb(a)
#endif

#ifdef CINTERPyes
// Compiling cinterp.o and defining the function interpret.
// Note CINTERPyes and RASTERPyes are never both defined at
// the same time, so there is only one definition of
// interpret.
#define INTERPRETFN interpret
extern BCPLWORD *watchaddr, watchval; // These are defined
extern int SIGSEGVoccurred;           // in fasterp.o

// Dummy rastering macros for cinterp.o
#define Rb(a)
#define Rh(a)
#define Rw(a)

// Proper memory check macros for cintsys.o
#define memchkp(a)  if((UBCPLWORD)(a) > maxp)    { res=16; goto ret; }
#define memchkpc(a) if((UBCPLWORD)(a) > memupbb) { res= 4; goto ret; }
#define memchkw(a)  if((UBCPLWORD)(a) > memupbw) { res=12; goto ret; }
#define memchkb(a)  if((UBCPLWORD)(a) > memupbb) { res=12; goto ret; }
#endif

#ifdef RASTERPyes
// Compiling cinterp.o and defining the function interpret.
// Note CINTERPyes and RASTERPyes are never both defined at
// the same time, so there is only one definition of
// interpret.

#define INTERPRETFN interpret
extern BCPLWORD *watchaddr, watchval; // These are defined
extern int SIGSEGVoccurred;           // in fasterp.o

// Rastering only happens when tallylimb is non zero.
#define Rb(a) if (tallylimb) rasterpoint( a );
#define Rh(a) if (tallylimb) rasterpoint((a)<<1);
#define Rw(a) if (tallylimb) rasterpoint((a)<<B2Wsh);

#define memchkp(a)
//#define memchkp(a)  if((UBCPLWORD)(a) > maxp)    { res=16; goto ret; }
#define memchkpc(a) if((UBCPLWORD)(a) > memupbb) { res= 4; goto ret; }
#define memchkw(a)  if((UBCPLWORD)(a) > memupbw) { res=12; goto ret; }
#define memchkb(a)  if((UBCPLWORD)(a) > memupbb) { res=12; goto ret; }
#endif

// Defines common to all three interpreters.
#define B (BP W)
#define SB (SBP W)
#define H (HP W)
#define SH (SHP W)

#ifdef BIGENDER
#define GH(x) ((WD B[x+0]<<8) | B[x+1])
#else
#define GH(x) ((WD B[x+1]<<8) | B[x])
#endif

// GW gets a 4 byte value from Cintcode byte address x. This value
// is regarded as a 32 bit signed integer. If the current BCPL word
// length is 64 bits, the value is sign extended to 64 bits. So
//    #x12345678 expands to #x00000000_12345678 and
//    #x81234567 expands to #xFFFFFFFF_81234567 ie
// the 64 bit value is signed and has the same numeric value as
// the 32 bit signed value.

// This was a change made on 10/09/2019. It was an incompatible
// change requiring modifications to various parts of the compiler.
// Note that (BCPLINT32) casts the 32 bit value to a signed 32 bit
// integer, and (BCPLWORD) sign extends this to 64 bit if necessary.

#ifdef BIGENDER
#define GW(x) \
  ((BCPLWORD)(BCPLINT32)((((((WD B[x]<<8)|B[x+1])<<8)|B[x+2])<<8)|B[x+3]))
#else
#define GW(x) \
  ((BCPLWORD)(BCPLINT32)((((((WD B[x+3]<<8)|B[x+2])<<8)|B[x+1])<<8)|B[x]))
#endif

// Note that when a 64 bit value is needed that cannot be represented
// by a sign extended 32 bit word, the 32 bit operand of an MW instruction
// must be added to the senior half of the signed extended value, but if
// the sign extended operand was negative the sign extension must be
// undone by adding one to the senior 32 bits. For example
// #xFFFFFFFF_FFFFFFFF  can be loaded by LW #xFFFFFFFF
// #xFFFFFFFF_12345678  can be loaded by MW #xFFFFFFFF
//                                       LW #x12345678
// #x00000001_81234567  can be loaded by MW #x00000002
//                                       LW #x81234567
// This last example is performing the following calculation
// #x00000002_00000000
// #xFFFFFFFF_81234567
// -------------------
// #x00000001_81234567


#ifdef RASTERPyes

// Define functions and data for rastering operations.

/* raster data is a compact representation of raster lines using
** run length encoding. It is generated by rastsys such as:
**
** F123456            Number of Cintcode instructions obeyed
**                    represented in the raster data file. 
** M450128            Maximum byte address within the raster
**                    data file. 
** K1000 S12          1000 instruction per raster line, scale 12
** W10B3W1345B1N      10 white, 3 black, 1345 white, 1 black, newline
** W13B3W12B2N        etc
** ...
** Alternatively it generates a bit stream file corresponding to
** the fifth bit of the Cintcode byte address referenced.
*/

// Define the constants and functions only needed by rastsys.

#define addrvupb      50000
#define addrhashvupb   99
#define addrhashvsize 100

static FILE *rasterfile=0;

// Note the following declarations use type int not BCPLWORD
static int addrv[addrvupb+1],
           addrlink[addrvupb+1],
           addrhashv[addrhashvupb+1],
           addrp=0;

// Addresses that are referenced during the period of a raster line
// are stored in addrv. Addresses in this vector only occur once.
// To eliminate multiple entries of the same address a hash table
// addrhashv is used. This holds lists of addresses with different
// hash values. The links for these lists are held in the vector
// addlink. At the end of each raster line period these distinct
// addresses are sorted and output to the raster file using run
// length encoding. The hash table is then cleared and a new set
// of addresses stated by setting addrp to zero. Addrp is always
// the number of addresses in the raster line.

static BCPLWORD scale=8;              // Default setting of these
static BCPLWORD fcountsperline=1000;  // parameters. There is little
                                      // need to change them.
// scale correponds to the number of byte addresses corresponding to
// each pixel position in the raster file data.

static BCPLWORD fcount=0;
// fcount is only defined when RASTERyes is specified.
// While raster data is being collected it holds the number of
// Cintcode instructions obeyed since the raster data collection
// started. When it is a multiple of fcountsperline a new raster
// line is generated. When raster data collection is complete it
// holds the value of the F parameter written at the start of the
// raster data file.

static int maxaddr=0; // This holds the maximum Cintcode byte
                      // address referenced so far in the
                      // raster data. It provides the value of the
                      // M parameter written at the start of the
                      // raster data file.

static int           soundbits=0;
static unsigned char bitpos;  // Used when soundbits=1
static unsigned char bits;    //

extern char *b2c_str(BCPLWORD bstr, char *cstr);
extern char *osfname(char *name, char *osname);

static void wrrasterline(void);

static void wraddrv(BCPLWORD fno) // A debugging aid
{ int i;
  printf("fno=%8lld: addrp=%d ", LL fno, addrp);
  for (i=1;i<=addrp; i++)
  { if(i%10==1) printf("\n");
    printf(" %6d", 8*addrv[i]);
  }
  printf("\n");
}

/*
// Debugging functions
static int crash(int x) {
  printf("fcount=%lld Calling crash\n", LL fcount);
  exit(125);
  return 0;
}

static void chkhashtab(void)   // Currently not used
{ int hashval;
  return;
  for (hashval = 0; hashval<=addrhashvupb; hashval++){
    int p = addrhashv[hashval];
    while(p!=0){
      if(addrv[p]%addrhashvsize!=hashval) {
	printf("fcount=%lld Bad hash table p=%d addrv[p]=%d hashval=%d\n",
	       LL fcount, p, addrv[p], hashval);
	crash(0);
      }
      p = addrlink[p];
    }
  }
}

static void wrhashtab()
{ int hashval;
  printf("\nfcount=%lld Hash table\n", LL fcount);
  for (hashval=0; hashval<=addrhashvupb; hashval++)
  { int p = addrhashv[hashval];
    if (p!=0)
    { printf("%3d: ", hashval);
      while (p!=0)
      { printf(" %5d", addrv[p]);
	if(addrv[p]%addrhashvsize!=hashval) {
	  printf("  ERROR hashval=%d\n", hashval);
	  crash(0);
	}
	p = addrlink[p];
      }
      printf("\n");
      //if(i==57) i=i/soundbits; // Cause a fault
    }
  }
  printf("End of hash table\n");
}
*/

static void clearhashtab()
{ int i;
  for (i=0; i<=addrhashvupb; i++) addrhashv[i] = 0;
  //printf("hash table cleared\n");
}

static int initraster(char *filename)
{ //printf("initraster(%s), soundbits=%d\n", filename, soundbits);
  if (soundbits) {
    bitpos = 1;     // Initialise the bit stream variables
    bits  = 0;
    
    rasterfile = fopen(filename, "wb");
    if (rasterfile==0) return 0;
    printf("Bit stream file %s opened\n", filename);
    return 1;
  } else {
    int i;
    
    clearhashtab(); // Initialise rastering variables
    addrp   = 0;
    fcount  = 0;
    maxaddr = 0;
    
    rasterfile = fopen(filename, "w");
    if (rasterfile==0) return 0;
    //printf("raster file %s opened, fcountsperline=%lld scale=%lld\n",
    //        filename, LL fcountsperline, LL scale); 
    // Leave space at the start for the F and M directives inserted
    // after the raster file has been read.
    fprintf(rasterfile, "                    K%lld S%lld\n",
	                LL fcountsperline, LL scale);
    return 1;
  }
}

static int endraster(void)
{ int res = -1;
  //printf("endraster called, soundbits=%lld\n", LL soundbits);
  if (rasterfile)
  { if (soundbits==0) {
      wrrasterline();
      // Insert the F and M directives at the start of the raster file.
      // Move to the start of the raster file.
      if(fseek(rasterfile, 0L, SEEK_SET)!=0)
      	printf("\nERROR: Failed to rewind the raster file\n");
      fprintf(rasterfile, "F%lld M%lld", LL fcount, LL maxaddr);
    }
    printf("\nRaster file closed, fcount=%lld maxaddr=%lld\n", LL fcount, LL maxaddr);
    res = fclose(rasterfile); // Returns 0 if successful.
    rasterfile = 0;
  }
  return res;
}

BCPLWORD setraster(BCPLWORD n, BCPLWORD val)
{ // n=0      Open the raster file
  // n=1      Set fcountsperline=val
  // n=2      Set scale=val
  // n=3      Return 0 if rastering is available
  // n=4      val=1 generate a bit stream file
  //          val=0 generate a raster file
  // n=5      Close the raster file
  
  char chbuf1[256];
  char chbuf2[256];
  
  //printf("cinterp: setraster: n=%d val=%d\n", n, val);
  //return 123;
  
  switch((int)n)
  { case 0:
      // Only valid after soundbits set to 1 or count and scale set.
      // Specify the raster file and start rastering.

      if (val) { 
        char *name = b2c_str(val, chbuf1);
	//printf("Calling initraster(%s)\n", osfname(name, chbuf2));
        return initraster(osfname(name, chbuf2));
      } else {
        return 1; // No filename
      }
      
    case 1:
      if (val>=0) fcountsperline = val;
      return fcountsperline;
      
    case 2:
      if (val>=0) scale = val;
      return scale; // bytes per pixel
      
    case 3:
      return 1;    // Rastering is available
      
    case 4:
      if (val==1) {
	soundbits = 1;  // soundbits=1 for soundbits generation
      } else {
	soundbits = 0;  // soundbits=0 for raster generation
      }
      return 0;
      
    case 5:
      return endraster(); // Return 0 if successful
  }
  
  return 0;
}

void sort(int *v, int upb)
{ // Sort elements v[1] to v[upb] using shell sort.
  BCPLWORD m = 1, i;
  while ( m<=upb) m = m*3 + 1; // Find first suitable value in the
                               // series:  1, 4, 13, 40, 121, 364, ...
  while(m>1)
  { m = m/3;
    for (i = m+1; i<=upb; i++)
    { BCPLWORD vi = v[i];
      BCPLWORD j = i;
      while(1)
      { BCPLWORD k = j - m;
        if ( k<=0 || v[k] < vi) break;
        v[j] = v[k];
        j = k;
      }
      v[j] = vi;
    }
  }
}

static void wrrasterline(void)
{ // Write an ASCII represation of the raster line.
  // Wn represents n white pixels
  // Bn represents n black pixels
  // N  marks the end of the line.
  // Not called when generating soundbits data.
  int i=1,
      k,
      a=0, // Pixel address of the first cell of the previous white region.
      b;   // Pixel address of a black cel.
  //printf("wrrasterline: Calling sort(1,%d) fcount=%d\n", addrp, fcount);
  
  if(addrp==0) return; // No more raster data to write.
  
  sort(addrv, addrp); // This corrupts the hash table.
  
  // The pixel positions of black cells are held in
  // addrv[1] to addrv[addrp] in sorted order.
  // All the pixel positions are distinct.

  
  i = 1;
  while (i<=addrp)
  { // a is pixel position of the first cell of a white region.
    int bi = i;     // i value of the first cell of a black region.
    b = addrv[i++]; // pixel position of the next black cell.
    k = b-a;        // Length of preceeding white region, possibly zero.
    fprintf(rasterfile, "W%d", k);
    //fprintf(rasterfile, "   White from %d to %d\n", a, b-1);
    a = b;          // Pixel position of the first cell of a black region.
    while (i<=addrp && addrv[i]==b+1) { b++; i++; }; // Step over consecutive
                                                     // black cells.
    // a is the pixel position of the first black cell in this region.
    // b is the pixel position of the last black cell in this region.
    fprintf(rasterfile, "B%d", b-a+1); // Output length of a black region.
    //fprintf(rasterfile, "   Black from %d to %d\n", a, b);

    a = b+1;  // Pixel position of the first cell after a black region.
  }

  // Mark the end of a raster line.
  fprintf(rasterfile, "N\n");

  //wraddrv(fcount);
  clearhashtab();
  addrp = 0;
  //printf("Just output a raster line, fcount=%d\n", fcount);
  if(fcount%1000000==0) printf("fcount = %lld\n", LL fcount);
}

void rasterpoint(BCPLWORD p)
{ // p is the Cintcode byte address referenced in the current raster line.

  if(p>maxaddr)
    { maxaddr = p;
      //printf("maxaddr=%d\n", maxaddr);
    }
  
  //  if(p>999000 && p<1001000)
  //  {
  //    printf("rasterpoint: fcount=%8d p=%8d\n", fcount, p);
  //    //maxp=p+1000;
  //  }
  
  //if (fcount>1000) crash(0);
  
  if (soundbits) {
    // Output bits packed 8 per byte to a binary bit stream. 
    // Pack a one if the fifth bit of the accessed byte address
    // is a one, otherwise pack a zero.
    p = p >> B2Wsh; // Convert to a word address
    printf("rasterpoint: Cintcode word addr = %6lld %8llX bit=%d\n",
	    LL p, LL p, (p&0x10)>0 ? 1 : 0);
    if ((p & 0x10) > 0) bits += bitpos;
    if (bitpos>=128) {
      /* Output the current 8-bit bit pattern. */
    printf("rasterpoint: byte = %2X\n", bits);
      fputc(bits, rasterfile);
      bitpos = 1; // Start a new byte
      bits = 0;
    } else {
      bitpos = bitpos+bitpos; // Set bitpos to the next bit position.
    }
  } else {
    // Change p from Cintcode byte address to pixel position.
    
    BCPLWORD a = p/scale;

    // Distinct pixel positions will be held in elements 1 to addrp
    // of the vector addrv.

    int hashval = a % addrhashvsize;
    //printf("a=%d addrhashvsize=%d hashval=%d\n", a, addrhashvsize, hashval);
    // addrhashv[hashval] is a list of all pixel positions with the
    // same hash value.

    int list = addrhashv[hashval];

    //printf("rasterpoint: fcount=%d a=%d addrp=%d hashval=%d list=%d\n",
    //	   fcount, a, addrp, hashval, list);
    //wrhashtab();
    while (list!=0)
      { //printf("list=%5d addrv[list]=%5d a=%5d\n", list, addrv[list], a);
      if (addrv[list]==a)
	{ //printf("fcount=%d addrp=%d %d found in the hash table\n",
	  //	  fcount, addrp, a);
	  return;
	}
      else list = addrlink[list];
    }
    //printf("fcount=%d addrp=%d %d not found in the hash table\n",
    //	    fcount, addrp, a);

    //printf("rasterpoint: Fcount=%d a=%d addrp=%d hashval=%d list=%d\n",
    //   fcount, a, addrp, hashval, list);
    
    if(addrp>=addrvupb) {
      printf("addrv needs to be larger\n");
      return;
    }
   
    addrv[++addrp] = a;                   // Insert a at the start
    addrlink[addrp] = addrhashv[hashval]; // of this hash chain.
    addrhashv[hashval] = addrp;
    
    //printf("a=%d not found, so add it\n", a);
    //wrhashtab();
    //printf("fcount=%d adding a=%d at position %d hashval=%d\n",
    //        fcount, a, addrp, hashval);
    //wrhashtab();
    //if(fcount>500) crash(0);
    //printf("addrp=%7lld p=%lld a=%7lld \n", LL addrp, LL p, LL a);
    //printf("fcount=%7lld fcountsperline=%lld\n",LL fcount,LL fcountsperline);
  }
}

#endif


int INTERPRETFN(BCPLWORD regs, BCPLWORD *mem)
{ // reg is the word address in Cintcode memory of the Cincode registers.
  BCPLWORD *W = mem;

  //  register int fcount = 0; // Only used when RASTERPyes is defined
   
  //register int icount = W[rootnode+Rtn_icountmax];
  register BCPLWORD icount = W[rootnode+Rtn_icountmax];

  register BCPLWORD           a  = W[regs+0];
  register BCPLWORD           b  = W[regs+1];
  BCPLWORD                    c  = W[regs+2];
  BCPLWORD                    p  = W[regs+3]>>B2Wsh;
  BCPLWORD                    g  = W[regs+4]>>B2Wsh;
  BCPLWORD                    st = W[regs+5];
  register BCPLWORD           pc = W[regs+6];
  BCPLWORD                    count = W[regs+7];
  BCPLWORD                    mw = W[regs+8];

  register BCPLWORD *Wp  = W+p,    /* Optimise access to the stack */
                    *Wg  = W+g,    /* Optimise access to the global vector */
                    *Wg1 = W+g+256;

  BCPLWORD res, k, i;

  UBCPLWORD maxp=0;                   // To hold the the value of p such
                                      // that p+255 is the upb of the current
                                      // coroutine. It is normally set by the
                                      // CHGCO instruction, and used in
                                      // cintsys whenever p changes.
  
  UBCPLWORD memupbw = memupb;         // These are used by cintsys to check memory
  UBCPLWORD memupbb = memupbw<<B2Wsh; // references using pointers by
				      // instructions such as RV, ST1P3 and PBYT.
                                              
  W[rootnode+Rtn_fast] = count<0 ? -1 : 0;
 
  int prevop = -1;      // For debugging aid
  
  Rw(regs+0); 
  Rw(regs+1); 
  Rw(regs+2); 
  Rw(regs+3); 
  Rw(regs+4); 
  Rw(regs+5); 
  Rw(regs+6); 
  Rw(regs+7); 
  Rw(regs+8); 

  /*   tracing = 1; */
  res = maxp; // To stop the unused-but-set warning.
  
fetchchk:
  if((UBCPLWORD)pc > memupbb) goto badpc; // Added 15/10/2020 to catch eg unset globals
  
  // Check P is in range.
  // Not done when in fasterp.
  //#ifdef ADDRCHKyes
  //  if((UBCPLWORD)p  > max) goto bad_p;
  //#endif

  //if(SIGSEGVoccurred) {  // This does not work yet.
  //  SIGSEGVoccurred = 0;
  //  res = 17; // SIGSEGV occurred
  //  goto ret;
  //}
   
fetch:

#ifdef WATCHyes
  /* Special watch debugging aid */
  if (watchaddr && *watchaddr!=watchval)
  { ///*
      printf("%7ld: changed from %7ld (%8lX) to %7ld (%8lX)\n",
             (long)(watchaddr-W), (long)watchval, (long)(UBCPLWORD)watchval,
             (long)*watchaddr, (long)(UBCPLWORD)*watchaddr);
    //*/
    watchval = *watchaddr;
    W[1] = watchaddr-W;  /* Make watchaddr and watchval visible to debug */
    W[2] = watchval;
    res = 11;        /* Contents of watch address has changed */
    goto ret;
  }
  /* End of watch code */
#endif

  /* count>=0  means execute count instructions (slow interpreter)
     count=-1  means go on for ever (fast interpreter)
     count=-2  means single step the fast interpreter
     count=-3  a special debugging aid to allow the debugger to
               be entered just before a selected instruction in
               rasterp is executed.
     count=-4  means set count to result2 and turn on instruction
               tracing. Used by the TRn command in sadebug.
  */
#ifdef COUNTyes
  if (count>=0)
  { if (count==0) { res = 3; goto ret; }
    count--;
  }
  //if(sizeof(BCPLWORD)==8)
  //  printf("count=%lld\n", LL count);
#endif

#ifdef TRACINGyes
  // Assume we are running in User Mode.
  if (tracing!=0) {
    if (tracing==2) { tracing=0; res = -4; goto ret; }
    trace(pc, p, a, b); // Trace a User Mode instruction
    if (tracing>0) tracing--;
    // If tracing is now zero, we must re-enter sadebug.
    // This is done by returning from the interpreter with
    // result -4. This will be detected by boot which will
    // then call sadebug.
  }
#endif

#ifdef TALLYyes
  if ((UBCPLWORD)pc < tallylimb)
  { tallyv[pc]++;
    //printf("tallyv[%lld] = %lld\n", LL pc, LL tallyv[pc]);
  }
#endif

  if(--icount<=0)
  { // Try to set icount so that this code is executed about
    // 50 times per second (20msecs).
    // It sets the date and time in the rootnode and
    // if the joystick device is open it reads the joystick values
    // into the rootnode.
    //int msecs     = W[rootnode+Rtn_msecs];
    //int icountmax = W[rootnode+Rtn_icountmax];
    
    //int msecsdiff = 0;

    BCPLWORD msecs     = W[rootnode+Rtn_msecs];
    BCPLWORD icountmax = W[rootnode+Rtn_icountmax];
    
    BCPLWORD msecsdiff = 0;
    int fd = W[rootnode+Rtn_joystickfd];
    
    // Update the days and msecs in the rootnode
    timestamp(&W[rootnode+Rtn_days]);
    msecsdiff = W[rootnode+Rtn_msecs] - msecs;

    //printf("icountmax=%lld msecsdiff=%lld\n", LL icountmax, LL msecsdiff);

    if (msecsdiff!=1)
    { if(msecsdiff>0)
        icountmax -= 100; // msecsdiff too large so decrease icountmax
      else
        icountmax += 100; // msecsdiff too small so increase icountmax

      W[rootnode+Rtn_icountmax]= icountmax; // Update the Estimate
    }

    icount = icountmax; // Set icount to the estimated number of
                        // Cintcode instructions per milli-second


    // icount = 10000;
    // icount     bench100 time
    //   1      923.370
    //   10      99.780
    //   100     16.040
    //   1000     7.470
    //   10000    6.600 (gcc -O9)
    //   100000   6.510 (gcc -O3)
    //            5.550 without this code omitted (gcc -O1)
    // trpush(0x11000000+W[rootnode+Rtn_msecs]);

#ifdef JSAvail
    if (fd)
    { //printf("Calling joyscan(%d,...)\n", fd);
      joyscan(fd, &W[g], W);
    }
#endif
  }

#ifdef RASTERPyes
  // If cli_tallyflag is set, the CLI calls sys(Sys_tally, TRUE) to
  // set tallylimb. This causes raster data to be generated during the
  // first CLI command to be executed after executing raster.

  if (tallylimb)
  { fcount++;
    //printf("icount=%d fcount=%d maxaddr=%d\n", icount, fcount, maxaddr);

    if (fcount%fcountsperline==0) {
      //printf("Calling wrrasterline()\n");
      wrrasterline();
      // It clears the hash table and sets addrp = 0;
    }
    //if(fcount % 10000 == 0) printf("fcount=%4d:\n", fcount);
    //trace(pc, p, a, b);
    //exit(120);
    //if(fcount>1000) crash(0);
  }
#endif
  
Rb(pc);

//tracing = 1;
//printf("icount=%d fcount=%d\n", icount, fcount);
switch(B[pc++])
{ default:
  case F_0:         // Cases F_0 has been added explicitly.
    res = 1; pc--;  // Unimplemented instruction
    goto ret;

  // Added 21/7/10
  case F_fltop:
  { Rb(pc);
    BCPLWORD op = B[pc++];

    //printf("fltop op=%ld\n", (long)op);
    switch (op) {
      default:
	Rw(1);
        W[1] = op;
        res = 14; goto ret;

      case fl_avail:
        a = -1; goto fetch;

      case fl_mk:
      { Rb(pc);
	BCPLWORD exponent = B[pc++]; // Signed byte
        // The senior bit represents -128
        if (exponent>=128) exponent = exponent-256;
        //printf("fl_mk calling doflt(%ld, %ld, %ld)\n",
        //        (long)op, (long)a, (long)exponent);
        a = doflt(op, a, exponent, 0);
        goto fetch;
      }

      case fl_float:
      case fl_fix:
      case fl_pos:
      case fl_neg:
      case fl_abs:
        a = doflt(op, a, 0, 0);
        goto fetch;

      case fl_mul:
      case fl_div:
      case fl_mod: // Added 14/5/18
      case fl_add:
      case fl_sub:
      case fl_eq:
      case fl_ne:
      case fl_ls:
      case fl_gr:
      case fl_le:
      case fl_ge:
        a = doflt(op, b, a, 0);
        goto fetch;
      }
    }

  // Added 21/7/10
  case F_selld:  // load a field  SELLD len sh
    { Rb(pc);
      BCPLWORD len = B[pc++];
      Rb(pc);
      BCPLWORD sh  = B[pc++];
      Rb(pc);
      BCPLWORD mask = -1;
      if (len) mask = (1<<len) - 1;
      //printf("%8llx >> %lld => %8llx\n", LL W[a], LL sh,
      //       LL (UBCPLWORD)W[a]>>sh);
      Rw(a);
      a = ((UBCPLWORD)W[a]>>sh) & mask;
      goto fetch;
    }

  // Added 21/7/10
  case F_selst: // SLCT len:sh:0 OF <arg1> op:= <arg2>
                //      len sh         a   op      b
  { Rw(a);
    BCPLWORD *ptr = &W[a];
    Rb(pc);
    BCPLWORD op  = B[pc++];
    Rb(pc);
    BCPLWORD len = B[pc++];
    Rb(pc);
    BCPLWORD sh  = B[pc++];
    Rb(pc);
    BCPLWORD mask;
    BCPLWORD val;
    BCPLWORD oldval, s;
    BCPLFLOAT t;

    if(len==0)
    {  mask = UWD(-1) >> sh;
    } else {
      mask = (1<<len) - 1;
    }
    // Care needed because >> may be arithmetic not logical
    val = WD(((UWD*ptr)>>sh)) & mask;
    oldval = val; // Old value shifted down

    // val and oldval are both the old field value shifted down
    switch(op) {
      default:          a = 0; goto fetch;
      case sf_none:     val = b;                 break;
      case sf_vecap:    Rw(val + b);
                        val = W[val + b];        break;
      case sf_fmul:     s = b;
        //printf("val=%13.5f  b=%13.5f ", N2F(val), N2F(s));
                        t = N2F(val) * N2F(s);
	//printf("=> %13.5f\n", t);
                        val = F2N(t);
                        break;
      case sf_fdiv:     s = b;
                        t = N2F(val) / N2F(s);
                        val = F2N(t);
                        break;
      case sf_fmod:     s = b;
                        t = Cfmod(N2F(val), N2F(s));
                        val = F2N(t);
                        break;
      case sf_fadd:     s = b;
                        t = N2F(val) + N2F(s);
                        val = F2N(t);
                        break;
      case sf_fsub:     s = b;
                        t = N2F(val) - N2F(s);
                        val = F2N(t);
                        break;
      case sf_mul:      val *= b;                break;
      case sf_div:      val /= b;                break;
      case sf_mod:      val %= b;                break;
      case sf_add:      val += b;                break;
      case sf_sub:      val -= b;                break;

      // Negative shifts shift in the reverse direction and care
      // is needed because >> to ensure that vacated position
      // are filled with zeroes.
      case sf_lshift:   if (abs(b) >= BperW) val=0;
                        if (b>=0) val <<= b;
                        else      val = WD((UWD val)>>(-b));
                        break;
      case sf_rshift:   if (abs(b) >= BperW) val=0;
                        if (b>=0) val = WD((UWD val)>>b);
                        else      val <<= (-b);
                        break;

      case sf_logand:   val &= b;                break;
      case sf_logor:    val |= b;                break;
      case sf_eqv:      val = ~(val ^ b);        break;
      case sf_xor:      val ^= b;                break;
    }
    //printf("selst: op=%ld len=%ld sh=%ld "
    //       "oldval=%08lX val=%08lX mask=%08lX\n",
    //       (long)op, (long)len, (long)sh, (long) UWD oldval,
    //       (long)UWD (long)val, (long)UWD mask);
    // Replace field by new value
    *ptr ^= ((val ^ oldval)&mask) << sh;
    goto fetch;
  }

  case F_mul:   a = b * a;        goto fetch;
  case F_div:   if(a==0) {res=5;  goto ret; } /* Division by zero */
                a = b / a;        goto fetch;
  case F_mod:   if(a==0) {res=5;  goto ret; } /* Division by zero */
                a = b % a;        goto fetch;
  case F_add:   a = b + a;        goto fetch;
  case F_sub:   a = b - a;        goto fetch;
  case F_neg:   a = - a;          goto fetch;

  case F_fhop:  a = 0; pc++;      goto fetch;

  // Negative shifts shift in the reverse direction and care
  // is needed because >> to ensure that vacated position
  // are filled with zeroes.
  case F_lsh:   if (abs(a) >= BperW) { a=0; goto fetch; }
                if (a>=0) a = b<<a;
                else      a = WD((UWD b)>>(-a));
                goto fetch;
  case F_rsh:   if (abs(a) >= BperW) { a=0; goto fetch; }
                if (a>=0) a = WD((UWD b)>>a);
                else      a = b<<(-a);
                goto fetch;

  case F_not:   a = ~ a;          goto fetch;
  case F_and:   a = b & a;        goto fetch;
  case F_or:    a = b | a;        goto fetch;
  case F_xor:   a = b ^ a;        goto fetch;

  //case F_goto:  pc = a;           goto fetchchk;
  case F_goto:  memchkpc(a);
                pc = a;           goto fetch;

  case F_brk:   res = 2; pc--; goto ret;  /* BREAKPOINT  */
                 
  case F_rv+6:  Rw(a+6);
                a = W[a+6]; goto fetch;
  case F_rv+5:  Rw(a+5);
                a = W[a+5]; goto fetch;
  case F_rv+4:  Rw(a+4);
                a = W[a+4]; goto fetch;
  case F_rv+3:  Rw(a+3);
                a = W[a+3]; goto fetch;
  case F_rv+2:  Rw(a+2);
                a = W[a+2]; goto fetch;
  case F_rv+1:  Rw(a+1);
                a = W[a+1]; goto fetch;
  case F_rv:    Rw(a+0);
                a = W[a+0]; goto fetch;

  case F_st+3:  Rw(a+3);
                W[a+3] = b; goto fetch;
  case F_st+2:  Rw(a+2);
                W[a+2] = b; goto fetch;
  case F_st+1:  Rw(a+1);
                W[a+1] = b; goto fetch;
  case F_st:    Rw(a);
                W[a+0] = b; goto fetch;

  case F_chgco: // p -> [oldp, retaddr, _, val, cptr]
                // ie we are in a call of changeco(val, cptr)
                // It must suspend currco giving it the resumption point oldp,
                // then resume execution of coroutine cptr by setting p to
                // its resumption point and setting currco to cptr.
                // pc is set surprisingly to the return address of the
                // changeco call, but this always point to the RTN instruction
                // in createco, callco, resumeco or cowait causing coroutine
                // cptr to resume execution correctly.
                // maxp must be set to the highest address in the coroutine
                // stack that p may have. This is 16 words before the end
                // of the coroutine stack so that instructions such as LP16,
                // AP12 and K11G do not have to check for stack overflow.
                // Stack overflow is only checked for by the slow interpreter.
                Rw(p+1);
		memchkpc(Wp[1]);
                pc = Wp[1];                      // pc      := p!1
                Rw(p+0); Rw(g+Gn_currco); Rw(Wg[Gn_currco]);
                W[Wg[Gn_currco]] = Wp[0];        // !currco := !p
                Rw(p+4); Rw(g+Gn_currco);
                Wg[Gn_currco] = Wp[4];           // currco  := cptr
                Rw(p+4); Rw(Wp[4]); 
                p = W[Wp[4]]>>B2Wsh;             // p       := !cptr
		
		// The next line was added on 20/03/2020
		maxp = Wp[4] + W[Wp[4]+4] - 16; // maxp    := cptr + cptr!4 - 16
		//printf("chgco: new currco=%d size=%d maxp=%d\n", Wp[4],
		//        W[Wp[4]+4], maxp);
		
                Wp = W+p;
		memchkp(p);
                goto fetch;

  case F_mdiv:  Rw(p+3); Rw(p+4); Rw(p+5);
                a = muldiv(Wp[3], Wp[4], Wp[5]);
                Rw(g+Gn_result2); 
                Wg[Gn_result2] = result2;

	        //{ BCPLINT64 ab = (BCPLINT64)(Wp[3]) * (BCPLINT64)(Wp[4]);
                //  BCPLWORD c = Wp[5];
                //  if(c==0) c=1;
                //  Wg[Gn_result2] = (BCPLWORD)(ab % c);
                //  a = (BCPLWORD)(ab / c);
	        //}
		
                // fall through to return
  case F_rtn:   Rw(p+1);
                pc = Wp[1];
		Rw(p);
                p  = W[p]>>B2Wsh;
                Wp = W+p; 
                goto fetchchk;

  case F_gbyt:  Rb(a+(b<<B2Wsh));
                a = B[a+(b<<B2Wsh)];        goto fetch;
	       
  case F_pbyt:  Rb(a+(b<<B2Wsh));
                B[a+(b<<B2Wsh)] = c;            goto fetch;
		
  case F_xpbyt: Rb(b+(a<<B2Wsh));
                B[b+(a<<B2Wsh)] = c;            goto fetch;
		
  case F_atc:  c = a;                      goto fetch;
  case F_btc:  c = b;                      goto fetch;
  case F_atb:  b = a;                      goto fetch;
  case F_xch:  a = a^b; b = a^b; a = a^b;  goto fetch;

  case F_swb: { BCPLWORD n, k, val, i=1;
                k = (pc+1)>>1;
                Rh(k);
                n = H[k];
                while(i<=n)
                { i += i;
                  Rh(k+i);
                  val = H[k+i];
                  if (a==val) { k += i; break; }
                  if (a<val) i++;
                }
                k++;
                Rh(k);
                pc = (k<<1) + SH[k]; // J to case or default
                goto fetchchk;
              }

  case F_swl: { BCPLWORD n,q;
                q = (pc+1)>>1;
                Rh(q);
                n = H[q++];
                if(0<=a && a<n) q += a + 1;
                Rh(q);
                pc = (q<<1) + SH[q];
                goto fetchchk;
              }

  case F_sys: switch(a) {
                default: // system call -- general case
 
		  W[regs+0]  = a;    /* Save all the registers */
		  W[regs+1]  = b;    /* for debugging purposes */
                  W[regs+2]  = c;
                  W[regs+3]  = p<<B2Wsh;
                  W[regs+4]  = g<<B2Wsh;
                  W[regs+5]  = st;
                  W[regs+6]  = pc;
                  W[regs+7]  = count;
                  W[regs+8]  = mw;
  
                  a = dosys(p, g);
                  goto fetch;

                case Sys_setcount:
                  // oldcount := sys(Sys_setcount, newcount)
                  a = count;
                  Rw(p+4);
		  count = Wp[4];
                  res = -1; // Leave and immediately re-enter
                  goto ret; // the interpreter

                case Sys_quit:
                  Rw(p+4);
                  res = Wp[4];
                  goto ret;

	      /*
                case Sys_rti:      // sys(Sys_rti, regs)
                case Sys_saveregs: // sys(Sys_saveregs, regs)
                case Sys_setst:    // sys(Sys_setst, st)
              */

		case  Sys_tally:   // sys(Sys_tally, flag)
		  // This is implemented here so that the
		  // variable tallylimb private to cinterp
		  // can be inspected and updated efficiently.
		  //printf("Sys_tally %lld has been called\n", LL W[p+4]);
                  Rw(p+4);
                  if (W[p+4]) {
#ifdef RASTERPyes
	            // In rastsys so start outputting
	            // raster or soundbits data if rasterfile is open.
		    if (rasterfile)
		    { fcount = 0;
		      tallylimb = tallyupb;
	              //printf("Sys_tally: start rastering, tallylimb=%lld\n",
		      //        LL tallylimb);
		    }
		    goto fetch;
#else
	            // Not in rasterp so clear the tally vector and
		    // start tallying.
                    tallylimb = tallyupb;
                    for(i=1; i<=tallylimb; i++) tallyv[i] = 0;
	            //printf("Sys_tally: start tallying, tallylimb=%lld\n",
		    //        LL tallylimb);
		    goto fetch;
#endif
                  } else {
	            // Stop tallying or rastering, but leave the
		    // collected data in tallyv.
	            //if (tallylimb) printf("Sys_tally: Stop tallying\n");
                    tallylimb = 0;
#ifdef RASTERPyes
		    if (rasterfile)
		    { endraster();
		      rasterfile = 0;
		      //printf("Sys_tally: rasterfile closed\n");
		    }
#endif
                  }
                  goto fetch;
     
		case Sys_watch:  /* sys(Sys_watch, addr) */
		{ Rw(p+4); Rw(Wp[4]);
		  watchaddr = &W[Wp[4]];
	          watchval = *watchaddr;
                  goto fetch;
                }
              }

  case F_lp0+16:  Rw(p+16);
                  b = a; a = Wp[16]; goto fetch;
  case F_lp0+15:  Rw(p+15);
                  b = a; a = Wp[15]; goto fetch;
  case F_lp0+14:  Rw(p+14);
                  b = a; a = Wp[14]; goto fetch;
  case F_lp0+13:  Rw(p+13);
                  b = a; a = Wp[13]; goto fetch;
  case F_lp0+12:  Rw(p+12);
                  b = a; a = Wp[12]; goto fetch;
  case F_lp0+11:  Rw(p+11);
                  b = a; a = Wp[11]; goto fetch;
  case F_lp0+10:  Rw(p+10);
                  b = a; a = Wp[10]; goto fetch;
  case F_lp0+9:   Rw(p+9);
                  b = a; a = Wp[9];  goto fetch;
  case F_lp0+8:   Rw(p+8);
                  b = a; a = Wp[8];  goto fetch;
  case F_lp0+7:   Rw(p+7);
                  b = a; a = Wp[7];  goto fetch;
  case F_lp0+6:   Rw(p+6);
                  b = a; a = Wp[6];  goto fetch;
  case F_lp0+5:   Rw(p+5);
                  b = a; a = Wp[5];  goto fetch;
  case F_lp0+4:   Rw(p+4);
                  b = a; a = Wp[4];  goto fetch;
  case F_lp0+3:   Rw(p+3);
                  b = a; a = Wp[3];  goto fetch;

  case F_lp:      Rb(pc); Rw(p+B[pc]);
                  b = a; a = Wp[B[pc++]];          goto fetch;
		
  case F_lph:     Rb(pc); Rw(p+GH(pc));
                  b = a; a = Wp[GH(pc)];  pc += 2; goto fetch;

  case F_lpw:     b = a;
#ifdef TARGET64
                  Rb(pc); Rw(p+mw+GW(pc));
                  a = Wp[mw+GW(pc)]; mw = 0;
#else
                  Rb(pc); Rw(p+GW(pc));
                  a = Wp[GW(pc)];
#endif
                  pc += 4; goto fetch;

  case F_llp:     Rb(pc);
                  b = a; a = p+B[pc++];             goto fetch;
  case F_llph:    Rb(pc);
                  b = a; a = p+GH(pc);     pc += 2; goto fetch;

  case F_llpw:    Rb(pc);
                  b = a;
#ifdef TARGET64
                 a = p+mw+GW(pc); mw = 0;
#else
                 a = p+GW(pc);
#endif
                 pc += 4; goto fetch;

  case F_sp0+16: Rw(p+16);
                 Wp[16] = a; goto fetch;
  case F_sp0+15: Rw(p+15);
                 Wp[15] = a; goto fetch;
  case F_sp0+14: Rw(p+14);
                 Wp[14] = a; goto fetch;
  case F_sp0+13: Rw(p+13);
                 Wp[13] = a; goto fetch;
  case F_sp0+12: Rw(p+12);
                 Wp[12] = a; goto fetch;
  case F_sp0+11: Rw(p+11);
                 Wp[11] = a; goto fetch;
  case F_sp0+10: Rw(p+10);
                 Wp[10] = a; goto fetch;
  case F_sp0+9:  Rw(p+9);
                 Wp[9] = a;  goto fetch;
  case F_sp0+8:  Rw(p+8);
                 Wp[8] = a;  goto fetch;
  case F_sp0+7:  Rw(p+7);
                 Wp[7] = a;  goto fetch;
  case F_sp0+6:  Rw(p+6);
                 Wp[6] = a;  goto fetch;
  case F_sp0+5:  Rw(p+5);
                 Wp[5] = a;  goto fetch;
  case F_sp0+4:  Rw(p+4);
                 Wp[4] = a;  goto fetch;
  case F_sp0+3:  Rw(p+3);
                 Wp[3] = a;  goto fetch;

  case F_sp:     Rb(pc); Rw(p+B[pc]);
                 Wp[B[pc++]] = a;                  goto fetch;

  case F_sph:    Rb(pc); Rw(p+GH(pc));
                 Wp[GH(pc)]  = a;         pc += 2; goto fetch;

  case F_spw:
#ifdef TARGET64
                Rb(pc); Rw(p+mw+GW(pc));
                Wp[mw+GW(pc)]  = a; mw = 0;
#else
		Rb(pc); Rw(p+GW(pc));
                Wp[GW(pc)]  = a;
#endif
                pc += 4; goto fetch;

  case F_lgh:   Rb(pc); Rw(g+GH(pc));
                b = a; a = Wg[GH(pc)];   pc += 2; goto fetch;
  case F_lg1:   Rb(pc); Rw(g+256+B[pc]);
                b = a; a = Wg1[B[pc++]];          goto fetch;
  case F_lg:    Rb(pc); Rw(g+B[pc]);
                b = a; a = Wg[B[pc++]];           goto fetch;

  case F_sgh:   Rb(pc); Rw(g+GH(pc));
                Wg[GH(pc)]   = a;        pc += 2; goto fetch;
  case F_sg1:   Rb(pc); Rw(g+256+B[pc]);
                Wg1[B[pc++]] = a;                 goto fetch;
  case F_sg:    Rb(pc); Rw(g+B[pc]);
                Wg[B[pc++]]  = a;                 goto fetch;

  case F_llgh: Rb(pc);
               b = a; a = g+GH(pc);      pc += 2; goto fetch;
  case F_llg1: Rb(pc);
               b = a; a = g+256+B[pc++];          goto fetch;
  case F_llg:  Rb(pc);
               b = a; a = g+B[pc++];              goto fetch;

  case F_ll+1: Rb(pc);
               i = (pc>>1) + B[pc];
	       Rh(i);
               i = (i<<1) + SH[i];
	       Rw(i>>B2Wsh);
               b = a; a = W[i>>B2Wsh];      pc++; goto fetch;

  case F_ll:   Rb(pc); Rw((pc+SB[pc])>>B2Wsh);
               b = a; a = W[(pc+SB[pc])>>B2Wsh];pc++; goto fetch;

  case F_sl+1: Rb(pc);
               i = (pc>>1) + B[pc];
               Rh(i);
	       i = (i<<1) + SH[i];
               Rw(i>>B2Wsh);
	       W[i>>B2Wsh] = a;                 pc++; goto fetch;

  case F_sl:   Rb(pc); Rw((pc+SB[pc])>>B2Wsh);
               W[(pc+SB[pc])>>B2Wsh] = a;       pc++; goto fetch;
   
  case F_lll+1:Rb(pc);
               i = (pc>>1) + B[pc];
               Rh(i);
	       i = (i<<1) + SH[i];
               b = a; a = i>>B2Wsh;             pc++; goto fetch;

  case F_lll:  Rb(pc);
               b = a; a = (pc+SB[pc])>>B2Wsh;   pc++; goto fetch;
   
  case F_l0+10: b = a; a = 10; goto fetch;
  case F_l0+9:  b = a; a =  9; goto fetch;
  case F_l0+8:  b = a; a =  8; goto fetch;
  case F_l0+7:  b = a; a =  7; goto fetch;
  case F_l0+6:  b = a; a =  6; goto fetch;
  case F_l0+5:  b = a; a =  5; goto fetch;
  case F_l0+4:  b = a; a =  4; goto fetch;
  case F_l0+3:  b = a; a =  3; goto fetch;
  case F_l0+2:  b = a; a =  2; goto fetch;
  case F_l0+1:  b = a; a =  1; goto fetch;
  case F_l0:    b = a; a =  0; goto fetch;
  case F_l0-1:  b = a; a = -1; goto fetch; 

  case F_l:     Rb(pc);
                b = a; a = B[pc++];               goto fetch;

  case F_lh:    Rb(pc+1);
                b = a; a = GH(pc);       pc += 2; goto fetch;

  case F_lw:    Rb(pc+3);
                b = a;
#ifdef TARGET64
                a = mw+GW(pc); mw = 0;
#else
                a = GW(pc);
#endif
                pc += 4; goto fetch;

  case F_lm:    Rb(pc);
                b = a; a = - WD(B[pc++]);         goto fetch;
  case F_lmh:   Rb(pc);
                b = a; a = - WD(GH(pc)); pc += 2; goto fetch;
                
  case F_lf+1:  Rb(pc);
                b = a;
		a = (pc>>1) + B[pc];
                Rh(a);
		a = (a<<1) + SH[a];         pc++; goto fetch;

  case F_lf:    Rb(pc+1);
                b = a; a = pc + SB[pc];     pc++; goto fetch;
 
  case F_k0gh+11: Rw(p+11);
                  Wp[11] = p<<B2Wsh; p += 11; goto applygh;
  case F_k0gh+10: Rw(p+10);
                  Wp[10] = p<<B2Wsh; p += 10; goto applygh;
  case F_k0gh+9:  Rw(p+9);
                  Wp[9] =  p<<B2Wsh; p += 9;  goto applygh;
  case F_k0gh+8:  Rw(p+8);
                  Wp[8] =  p<<B2Wsh; p += 8;  goto applygh;
  case F_k0gh+7:  Rw(p+7);
                  Wp[7] =  p<<B2Wsh; p += 7;  goto applygh;
  case F_k0gh+6:  Rw(p+6);
                  Wp[6] =  p<<B2Wsh; p += 6;  goto applygh;
  case F_k0gh+5:  Rw(p+5);
                  Wp[5] =  p<<B2Wsh; p += 5;  goto applygh;
  case F_k0gh+4:  Rw(p+4);
                  Wp[4] =  p<<B2Wsh; p += 4;  goto applygh;
  case F_k0gh+3:  Rw(p+3);
                  Wp[3] =  p<<B2Wsh; p += 3;  goto applygh;

  applygh:        Wp    = W+p;
                  Rw(p+1);
		  Wp[1] = pc + 2;
                  Rb(pc+1); Rw(g+GH(pc));
		  pc = Wg[GH(pc)];
                  Rw(p+2);
		  Wp[2] = pc;
                  Rw(p+3);
		  Wp[3] =  a;
                  goto fetchchk;

  case F_k0g1+11: Rw(p+11);
                  Wp[11] = p<<B2Wsh; p += 11; goto applyg1;
  case F_k0g1+10: Rw(p+10);
                  Wp[10] = p<<B2Wsh; p += 10; goto applyg1;
  case F_k0g1+9:  Rw(p+9);
                  Wp[9] =  p<<B2Wsh; p += 9;  goto applyg1;
  case F_k0g1+8:  Rw(p+8);
                  Wp[8] =  p<<B2Wsh; p += 8;  goto applyg1;
  case F_k0g1+7:  Rw(p+7);
                  Wp[7] =  p<<B2Wsh; p += 7;  goto applyg1;
  case F_k0g1+6:  Rw(p+6);
                  Wp[6] =  p<<B2Wsh; p += 6;  goto applyg1;
  case F_k0g1+5:  Rw(p+5);
                  Wp[5] =  p<<B2Wsh; p += 5;  goto applyg1;
  case F_k0g1+4:  Rw(p+4);
                  Wp[4] =  p<<B2Wsh; p += 4;  goto applyg1;
  case F_k0g1+3:  Rw(p+3);
                  Wp[3] =  p<<B2Wsh; p += 3;  goto applyg1;

  applyg1:        Wp    = W+p;
                  Rw(p+1);
		  Wp[1] = pc + 1;
                  Rb(pc); Rw(g+256+B[pc]);
		  pc    = Wg1[B[pc]];
                  Rw(p+2);
		  Wp[2] = pc;
                  Rw(p+3);
		  Wp[3] = a;
                  goto fetchchk;
 
  case F_k0g+11: Wp[11] = p<<B2Wsh; p += 11; goto applyg;
  case F_k0g+10: Wp[10] = p<<B2Wsh; p += 10; goto applyg;
  case F_k0g+9:  Wp[ 9] = p<<B2Wsh; p +=  9; goto applyg;
  case F_k0g+8:  Wp[ 8] = p<<B2Wsh; p +=  8; goto applyg;
  case F_k0g+7:  Wp[ 7] = p<<B2Wsh; p +=  7; goto applyg;
  case F_k0g+6:  Wp[ 6] = p<<B2Wsh; p +=  6; goto applyg;
  case F_k0g+5:  Wp[ 5] = p<<B2Wsh; p +=  5; goto applyg;
  case F_k0g+4:  Wp[ 4] = p<<B2Wsh; p +=  4; goto applyg;
  case F_k0g+3:  Wp[ 3] = p<<B2Wsh; p +=  3;
  applyg:        Wp    = W+p;
                 Wp[1] = pc + 1;
                 pc    = Wg[B[pc]];
                 Wp[2] = pc;
                 Wp[3] = a;
                 goto fetchchk;
 
   case F_k0+11: Rw(p+11);
                 Wp[11] = p<<B2Wsh; p += 11; goto applyk;
   case F_k0+10: Rw(p+10);
                 Wp[10] = p<<B2Wsh; p += 10; goto applyk;
   case F_k0+9:  Rw(p+9);
                 Wp[ 9] = p<<B2Wsh; p +=  9; goto applyk;
   case F_k0+8:  Rw(p+8);
                 Wp[ 8] = p<<B2Wsh; p +=  8; goto applyk;
   case F_k0+7:  Rw(p+7);
                 Wp[ 7] = p<<B2Wsh; p +=  7; goto applyk;
   case F_k0+6:  Rw(p+6);
                 Wp[ 6] = p<<B2Wsh; p +=  6; goto applyk;
   case F_k0+5:  Rw(p+5);
                 Wp[ 5] = p<<B2Wsh; p +=  5; goto applyk;
   case F_k0+4:  Rw(p+4);
                 Wp[ 4] = p<<B2Wsh; p +=  4; goto applyk;
   case F_k0+3:  Rw(p+3);
                 Wp[ 3] = p<<B2Wsh; p +=  3;
   applyk:       Wp    = W+p;
                 Rw(p+1);
		 Wp[1] = WD pc;
                 pc    = a;
                 Rw(p+2);
		 Wp[2] = pc;
                 Rw(p+3);
		 Wp[3] = a = b;
                 goto fetchchk;

  case F_k:      Rb(pc);
                 k = B[pc];
		 Rw(p+k);
		 Wp[k] = p<<B2Wsh; p +=  k;
                 Wp    = W+p;
                 Rw(p+1);
		 Wp[1] = pc + 1;
                 pc    = a;
                 Rw(p+2);
		 Wp[2] = pc;
                 Rw(p+3);
		 Wp[3] = a = b;
                 goto fetchchk;

  case F_kh:     Rb(pc);
                 k = GH(pc);
		 Rw(p+k);
		 Wp[k] = p<<B2Wsh;
		 p +=  k;
                 Wp = W+p;
                 Rw(p+1);
		 Wp[1] = pc + 2;
                 pc = a;
                 Rw(p+2);
		 Wp[2] = pc;
                 Rw(p+3);
		 Wp[3] = a = b;
                 goto fetchchk;

  case F_kw:     Rb(pc+3);
#ifdef TARGET64
                 k = mw+GW(pc); mw = 0;
#else
                 k = GW(pc);
#endif
                 Rw(p+k);
                 Wp[k] = p<<B2Wsh; p += k;
                 Wp    = W+p;
                 Rb(p+1);
                 Wp[1] = pc + 4;
                 pc    = a;
                 Rb(p+2);
                 Wp[2] = pc;
                 Rb(p+3);
                 Wp[3] = a = b;
                 goto fetchchk;

  case F_jeq:   if(b==a) { Rb(pc); pc += SB[pc];   goto fetch; }
                pc++; goto fetch;
  case F_jeq+1: if(b==a) goto indjump;
                pc++; goto fetch;
  case F_jeq+2: if(a==0) { Rb(pc); pc += SB[pc];   goto fetch; }
                pc++; goto fetch;
  case F_jeq+3: if(a==0) goto indjump;
                pc++; goto fetch;

  case F_jne:   if(b!=a) { Rb(pc); pc += SB[pc];   goto fetch; }
                pc++; goto fetch;
  case F_jne+1: if(b!=a) goto indjump;
                pc++; goto fetch;
  case F_jne+2: if(a!=0) { Rb(pc); pc += SB[pc];   goto fetch; }
                pc++; goto fetch;
  case F_jne+3: if(a!=0) goto indjump;
                pc++; goto fetch;

  case F_jls:   if(b<a) { Rb(pc); pc += SB[pc];   goto fetch; }
                pc++; goto fetch;
  case F_jls+1: if(b<a) goto indjump;
                pc++; goto fetch;
  case F_jls+2: if(a<0) { Rb(pc); pc += SB[pc];   goto fetch; }
                pc++; goto fetch;
  case F_jls+3: if(a<0) goto indjump;
                pc++; goto fetch;

  case F_jgr:   if(b>a) { Rb(pc); pc += SB[pc];   goto fetch; }
                pc++; goto fetch;
  case F_jgr+1: if(b>a) goto indjump;
                pc++; goto fetch;
  case F_jgr+2: if(a>0) { Rb(pc); pc += SB[pc];   goto fetch; }
                pc++; goto fetch;
  case F_jgr+3: if(a>0) goto indjump;
                pc++; goto fetch;

  case F_jle:   if(b<=a) {Rb(pc);  pc += SB[pc];   goto fetch; }
                pc++; goto fetch;
  case F_jle+1: if(b<=a) goto indjump;
                pc++; goto fetch;
  case F_jle+2: if(a<=0) { Rb(pc); pc += SB[pc];   goto fetch; }
                pc++; goto fetch;
  case F_jle+3: if(a<=0) goto indjump;
                pc++; goto fetch;

  case F_jge:   if(b>=a) { Rb(pc); pc += SB[pc];   goto fetch; }
                pc++; goto fetch;
  case F_jge+1: if(b>=a) goto indjump;
                pc++; goto fetch;
  case F_jge+2: if(a>=0) { Rb(pc); pc += SB[pc];   goto fetch; }
                pc++; goto fetch;
  case F_jge+3: if(a>=0) goto indjump;
                pc++; goto fetch;

  case F_j:     Rb(pc); pc += SB[pc];        goto fetch;

indjump:
  case F_j+1:   Rb(pc);
                pc = (pc>>1) + B[pc];
                Rh(pc);
		pc = (pc<<1) + SH[pc];
                goto fetch;

  case F_ap0+12: Rw(p+12);
                 a = a + Wp[12]; goto fetch;
  case F_ap0+11: Rw(p+11);
                 a = a + Wp[11]; goto fetch;
  case F_ap0+10: Rw(p+10);
                 a = a + Wp[10]; goto fetch;
  case F_ap0+9:  Rw(p+9);
                 a = a + Wp[9];  goto fetch;
  case F_ap0+8:  Rw(p+8);
                 a = a + Wp[8];  goto fetch;
  case F_ap0+7:  Rw(p+7);
                 a = a + Wp[7];  goto fetch;
  case F_ap0+6:  Rw(p+6);
                 a = a + Wp[6];  goto fetch;
  case F_ap0+5:  Rw(p+5);
                 a = a + Wp[5];  goto fetch;
  case F_ap0+4:  Rw(p+4);
                 a = a + Wp[4];  goto fetch;
  case F_ap0+3:  Rw(p+3);
                 a = a + Wp[3];  goto fetch;

  case F_ap:    Rb(pc); Rw(p+B[pc]);
                a += Wp[B[pc++]];         goto fetch;
  case F_aph:   Rb(pc+1); Rw(p+GH(pc));
                a += Wp[GH(pc)]; pc += 2; goto fetch;

  case F_apw:   Rb(pc+3);
#ifdef TARGET64
                Rw(p+mw+GW(pc));
                a += Wp[mw+GW(pc)]; mw = 0;
#else
                Rw(p+GW(pc));
                a += Wp[GW(pc)];
#endif
                pc += 4; goto fetch;

  case F_agh:   Rb(pc+1); Rw(g+GH(pc));
                a += Wg[GH(pc)]; pc += 2; goto fetch;
  case F_ag1:   Rb(pc); Rw(g+256+B[pc]);
                a += Wg1[B[pc++]];        goto fetch;
  case F_ag:    Rb(pc); Rw(g+B[pc]);
                a += Wg[B[pc++]];         goto fetch;

  case F_a0+5: a += 5; goto fetch;
  case F_a0+4: a += 4; goto fetch;
  case F_a0+3: a += 3; goto fetch;
  case F_a0+2: a += 2; goto fetch;
  case F_a0+1: a += 1; goto fetch;
  case F_nop:          goto fetch;

  // The mv instruction is only used in 64-bit Cintcode
  // when generating 32-but Cintcode.

  case F_mw: // Ignore MW on 32 bit Cintcode
#ifdef TARGET64
               Rw(pc+3);
               mw = ((BCPLWORD)GW(pc))<<32;
#endif
               pc += 4;
               goto fetch;

  case F_a:    Rb(pc);
               a += B[pc++];           goto fetch;
  case F_ah:   Rh(pc+1);
               a += GH(pc);   pc += 2; goto fetch;

  case F_aw:   Rw(pc+3);
#ifdef TARGET64
               a += mw+GW(pc); mw = 0;
#else
               a += GW(pc);
#endif
               pc += 4; goto fetch;

  case F_s:    Rb(pc);
               a -= B[pc++];           goto fetch;
  case F_sh:   Rh(pc+1);
               a -= GH(pc);   pc += 2; goto fetch;

  case F_s0+4: a -= 4; goto fetch;
  case F_s0+3: a -= 3; goto fetch;
  case F_s0+2: a -= 2; goto fetch;
  case F_s0+1: a -= 1; goto fetch;

  case F_l0p0+12: Rw(p+12); Rw(Wp[12]+0);
                  b = a; a = W[Wp[12]+0]; goto fetch;
  case F_l0p0+11: Rw(p+11); Rw(Wp[11]+0);
                  b = a; a = W[Wp[11]+0]; goto fetch;
  case F_l0p0+10: Rw(p+10); Rw(Wp[10]+0);
                  b = a; a = W[Wp[10]+0]; goto fetch;
  case F_l0p0+9:  Rw(p+9);   Rw(Wp[9]+0);
                  b = a; a = W[Wp[ 9]+0]; goto fetch;
  case F_l0p0+8:  Rw(p+8);   Rw(Wp[8]+0);
                  b = a; a = W[Wp[ 8]+0]; goto fetch;
  case F_l0p0+7:  Rw(p+7);   Rw(Wp[7]+0);
                  b = a; a = W[Wp[ 7]+0]; goto fetch;
  case F_l0p0+6:  Rw(p+6);   Rw(Wp[6]+0);
                  b = a; a = W[Wp[ 6]+0]; goto fetch;
  case F_l0p0+5:  Rw(p+5);   Rw(Wp[5]+0);
                  b = a; a = W[Wp[ 5]+0]; goto fetch;
  case F_l0p0+4:  Rw(p+4);   Rw(Wp[4]+0);
                  b = a; a = W[Wp[ 4]+0]; goto fetch;
  case F_l0p0+3:  Rw(p+3);   Rw(Wp[3]+0);
                  b = a; a = W[Wp[ 3]+0]; goto fetch;

  case F_l1p0+6:  Rw(p+6);   Rw(Wp[6]+1);
                  b = a; a = W[Wp[ 6]+1]; goto fetch;
  case F_l1p0+5:  Rw(p+5);   Rw(Wp[5]+1);
                  b = a; a = W[Wp[ 5]+1]; goto fetch;
  case F_l1p0+4:  Rw(p+4);   Rw(Wp[4]+1);
                  b = a; a = W[Wp[ 4]+1]; goto fetch;
  case F_l1p0+3:  Rw(p+3);   Rw(Wp[3]+1);
                  b = a; a = W[Wp[ 3]+1]; goto fetch;

  case F_l2p0+5:  Rw(p+5);   Rw(Wp[5]+2);
                  b = a; a = W[Wp[ 5]+2]; goto fetch;
  case F_l2p0+4:  Rw(p+4);   Rw(Wp[4]+2);
                  b = a; a = W[Wp[ 4]+2]; goto fetch;
  case F_l2p0+3:  Rw(p+3);   Rw(Wp[3]+2);
                  b = a; a = W[Wp[ 3]+2]; goto fetch;

  case F_l3p0+4:  Rw(p+4);   Rw(Wp[4]+3);
                  b = a; a = W[Wp[ 4]+3]; goto fetch;
  case F_l3p0+3:  Rw(p+3);   Rw(Wp[3]+3);
                  b = a; a = W[Wp[ 3]+3]; goto fetch;

  case F_l4p0+4:  Rw(p+4);   Rw(Wp[4]+4);
                  b = a; a = W[Wp[ 4]+4]; goto fetch;
  case F_l4p0+3:  Rw(p+3);   Rw(Wp[3]+4);
                  b = a; a = W[Wp[ 3]+4]; goto fetch;

  case F_l0gh:  Rb(pc); Rw(g+GH(pc)); Rw(Wg[GH(pc)]+0);
                b = a; a = W[Wg[GH(pc)]+0]; pc += 2; goto fetch;
  case F_l1gh:  Rb(pc); Rw(g+GH(pc)); Rw(Wg[GH(pc)]+1);
                b = a; a = W[Wg[GH(pc)]+1]; pc += 2; goto fetch;
  case F_l2gh:  Rb(pc); Rw(g+GH(pc)); Rw(Wg[GH(pc)]+2);
                b = a; a = W[Wg[GH(pc)]+2]; pc += 2; goto fetch;
  case F_l0g1:  Rb(pc); Rw(g+256+B[pc]); Rw(Wg1[B[pc]]+0);
                b = a; a = W[Wg1[B[pc++]]+0];        goto fetch;
  case F_l1g1:  Rb(pc); Rw(g+256+B[pc]); Rw(Wg1[B[pc]]+1);
                b = a; a = W[Wg1[B[pc++]]+1];        goto fetch;
  case F_l2g1:  Rb(pc); Rw(g+256+B[pc]); Rw(Wg1[B[pc]]+2);
                b = a; a = W[Wg1[B[pc++]]+2];        goto fetch;
  case F_l0g:   Rb(pc); Rw(g+B[pc]); Rw(Wg[B[pc]]+0);
                b = a; a = W[Wg[B[pc++]]+0];         goto fetch;
  case F_l1g:   Rb(pc); Rw(g+B[pc]); Rw(Wg[B[pc]]+1);
                b = a; a = W[Wg[B[pc++]]+1];         goto fetch;
  case F_l2g:   Rb(pc); Rw(g+B[pc]); Rw(Wg[B[pc]]+2);
                b = a; a = W[Wg[B[pc++]]+2];         goto fetch;

  case F_s0gh:  Rb(pc); Rw(g+GH(pc)); Rw(Wg[GH(pc)]+0);
                W[Wg[GH(pc)]+0] = a;        pc += 2; goto fetch;
  case F_s0g1:  Rb(pc); Rw(g+256+B[pc]); Rw(Wg1[B[pc]]+0);
                W[Wg1[B[pc++]]+0] = a;               goto fetch;
  case F_s0g:   Rb(pc); Rw(g+B[pc]); Rw(Wg[B[pc]]+0);
                W[Wg[B[pc++]]+0] = a;                goto fetch;

  case F_stp0+5: Rw(p+5); Rw(a+Wp[5]); W[a+Wp[5]] = b; goto fetch;
  case F_stp0+4: Rw(p+4); Rw(a+Wp[4]); W[a+Wp[4]] = b; goto fetch;
  case F_stp0+3: Rw(p+3); Rw(a+Wp[3]); W[a+Wp[3]] = b; goto fetch;

  case F_st0p0+4: Rw(p+4); Rw(Wp[4]+0); W[Wp[4]+0] = a; goto fetch;
  case F_st0p0+3: Rw(p+3); Rw(Wp[3]+0); W[Wp[3]+0] = a; goto fetch;

  case F_st1p0+4: Rw(p+4); Rw(Wp[4]+1); W[Wp[4]+1] = a; goto fetch;
  case F_st1p0+3: Rw(p+3); Rw(Wp[3]+1); W[Wp[3]+1] = a; goto fetch;
   
  case F_rvp0+7: Rw(p+7); Rw(a+Wp[7]); a = W[a+Wp[7]]; goto fetch;
  case F_rvp0+6: Rw(p+6); Rw(a+Wp[6]); a = W[a+Wp[6]]; goto fetch;
  case F_rvp0+5: Rw(p+5); Rw(a+Wp[5]); a = W[a+Wp[5]]; goto fetch;
  case F_rvp0+4: Rw(p+4); Rw(a+Wp[4]); a = W[a+Wp[4]]; goto fetch;
  case F_rvp0+3: Rw(p+3); Rw(a+Wp[3]); a = W[a+Wp[3]]; goto fetch;
 }

badpc:
   res = 4;
 
ret:
   tracing = 0;
   //printf("cinterp: returning from interpret, res=%ld\n", (long)res);
   W[regs+0]  = a;    /* Save the machine registers  */
   W[regs+1]  = b;
   W[regs+2]  = c;
   W[regs+3]  = p<<B2Wsh;
   W[regs+4]  = g<<B2Wsh;
   W[regs+5]  = st;
   W[regs+6]  = pc;
   W[regs+7]  = count;
   W[regs+8]  = mw;

   /* Save p in currco resumption point, for debugging purposes     */
   /* currco must always be set to the coroutine stack containing p */
   //W[Wg[Gn_currco]] = p;
   
   return res;  // Return from this invocation of interpret
}

