/*
This is the main program of the BCPL Cintpos system written in C
and designed to run on most machines with Unix-like C libraries. It
is designed to work for both 32 and 64 bit BCPL. It is currently
being modified to be the main program for both cintsys and cintpos
with the differences being controlled by the macros CINTSYSyes and
CINTPOSyes.

(c) Copyright:  Martin Richards  07 Mar 2021
*/

// Just testing whether this sort of comment is available. From now
// on we will assume it is available.

/*
History

04/11/2019
Tried to make the source of cintsys and cintpos identical controlled
bt whether CINTPOSyes is defined. This change is still being tested.

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
There were related changes to cintsys.h, cintpos.h, cintpos.c g/libhdr.h and 
com/dcount.b

07/02/11
Changed the format of BCPL filenames to use '/' (or '\') as separators of file
name components. Such names are converted to target system names before
use. The targets are UNIXNAMES for Linux style, WINNAMES for Windows style and
VMSNAMES for VMS style. Separators in path list can be either semicolons or
colons, except under Window when only semicolons are allowed.

12/01/11
Made INT.h define the macro FormD to have values like
"d" or "ld" depending on whether BCPLWORD is int or long. FormX
is similarly defined as "X" or "lX". These are used in printf formats
taking advantage of the C feature that concatenates consecutive string
constants at compile time, as in: printf("x = %5"FormD"\n", (BCPLWORD) x);
Made systematic changes to cintsys.c, cintsys.h and kblib.c to make use
of FormD and FormX in all calls of printf.

04/12/10
Changed the implementation of Sys_delay to use select() rather than
usleep() or nanosleep().

03/05/10
Removed Sys_usleep and reimplemented Sys_delay to sleep for a time
in msecs (possibly >= 1000).

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

07/10/09
Renamed files cinterp.h to cintsys.h (and for cintpos, cinterp.h to
cintpos.h) to distinguish between the two versions of cinterp.h

05/10/09
Moved some some configuration statements to cintsys.h and added VMSNAMES

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

22/06/06
Making a version for the GP2X handheld Linux machine.

21/06/06
Defined CHAR in cintpos.h and cintsys.h to be unsigned char and replaced nearly
all occurences of char with CHAR. This avoids a warning from some compilers.
Changed most file names to lower case to simplify use of windows machines.

18/01/06
Added -s, -c and -- parameters to cintpos, to prepend characters before the
start of stdin (as suggested by Dave Lewis).

01/01/06
Change -m and -t options to specify sizes in words (not thousands of words).

24/04/04
Made many changes to make cintsys more compatible with cintpos

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
held in memory and we mus obey basic Pthread rules about memory visibility.

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

/* cintsys.h and cintpos.h contains machine/OS dependent #defines  */

#ifdef CINTPOSyes
#include "cintpos.h"
#endif

#ifdef CINTSYSyes
#include "cintsys.h"
#endif

#include <math.h>
#include "time.h"

/* Function defined in callc.c  */
extern BCPLWORD callc(BCPLWORD *args, BCPLWORD *g);

#ifdef CINTPOSyes
// The following two vectors are declared in devices.c
extern BCPLWORD irqfifov[];         /* A fifo of interrupting devid's */
extern BCPLWORD irqfifop, irqfifoq; /* circular queue from p to q-1   */

int icount=0; // Decrement on every cintcode instruction and when <=0
              // check for interrupts in the fifo.
int kcount=0; // Decrement every 1000 cintcode instructions and when <=0
              // check clock interrupts.

pthread_mutex_t irq_mutex = PTHREAD_MUTEX_INITIALIZER;
// irq_mutex controls access to the irq fifo, device pkts and DCB fields

pthread_cond_t  irq_cv    = PTHREAD_COND_INITIALIZER;
// irq_cv is signalled every time a device thread put and interrupt request
// in the fifo

pthread_mutex_t trpush_mutex = PTHREAD_MUTEX_INITIALIZER;
// trpush_mutex controls access to trcount and trvec

/* Functions defined in devices.c  */
extern BCPLWORD initdevices(void);
extern BCPLWORD devcommand(BCPLWORD dcb, BCPLWORD com, BCPLWORD arg);

#ifndef _POSIX_THREADS
  printf("_POSIX_THREADS is not defined\n");
  printf("Cintpos requires Posix Threads to run\n");
#endif

#endif

BCPLWORD trcount = -1; // trpush initially disabled
BCPLWORD trvec[4096];  // 4096 elements in the trpush circular buffer

// Low level trace functions

void trpush(BCPLWORD val);
BCPLWORD settrcount(BCPLWORD count); // Set trcount and returns its old value
BCPLWORD gettrval(BCPLWORD count);   // Get the specified trace value
BCPLWORD incdcount(BCPLWORD n);

// Functions for keyboard input defined in kblib.c
extern int Readch(void);
extern int pollReadch(void);
extern int init_keyb(void);
extern int close_keyb(void);
extern int intflag(void);

// Function defined in soundfn.c
// This file is included if SOUND is defined
BCPLWORD soundfn(BCPLWORD *args, BCPLWORD *g);

/* Function defined in joyfn.c */
extern BCPLWORD joyfn(BCPLWORD *args, BCPLWORD *g, BCPLWORD *W);

/* Function defined in sdlfn.c */
extern BCPLWORD sdlfn(BCPLWORD *args, BCPLWORD *g, BCPLWORD *W);

/* Function defined in glfn.c */
extern BCPLWORD glfn(BCPLWORD *args, BCPLWORD *g, BCPLWORD *W);

/* Function defined in extfn.c */
extern BCPLWORD extfn(BCPLWORD *args, BCPLWORD *g, BCPLWORD *W);

// The function setraster is defined in rasterp.o, but in
// fasterp.o there is only a dummy definition.
extern BCPLWORD setraster(BCPLWORD n, BCPLWORD val);

// sysgraphics is obsolete
BCPLWORD sysGraphics(BCPLWORD p) { return 0; } // Dummy definition

#define Stackupb     500L

// Globupb is now a variable that can be set using the -g option.
// Its default value is now 2000

BCPLWORD *W;    // This will hold the pointer to the Cintcode memory

BCPLWORD sysp;  // Value of p when executing instruction SYS
BCPLWORD sysg;  // Value of g when executing instruction SYS
BCPLWORD sysst; // Value of st when executing instruction SYS
// These are stored in the rootnode when a dump of the Cintcode
// memory is made. This help the analysis of memory dumps.

BCPLWORD *lastWp;    /* Latest setting of Wp  */
BCPLWORD *lastWg;    /* Latest setting of Wg  */
BCPLWORD  lastst;    /* Latest setting of st  */

BCPLWORD rootvarstr    = 0;
BCPLWORD pathvarstr    = 0;
BCPLWORD hdrsvarstr    = 0;
BCPLWORD scriptsvarstr = 0;

BCPLWORD prefixstr = 0; // Position in the Cintcode memory of the filename
                        // prefix. The prefixstr is prepended to all non
                        // absolute file names. prefixstr is set by
                        // sys(Sys_setprefix, str) and read by
                        // sys(Sys_getprefix).
char *prefixbp;         // Address of byte holding the length of the prefix

BCPLWORD stackbase;
BCPLWORD globbase;
BCPLWORD result2;

static char chbuf1[256];
static char chbuf2[256];
static char chbuf3[256];
static char chbuf4[256];

#ifdef TARGET64

#ifdef CINTSYSyes
static char* rootvar    = "BCPL64ROOT";    // The default setting
static char* pathvar    = "BCPL64PATH";    // The default setting
static char* hdrsvar    = "BCPL64HDRS";    // The default setting
static char* scriptsvar = "BCPL64SCRIPTS"; // The default setting
#endif

#ifdef CINTPOSyes
static char* rootvar    = "POS64ROOT";    // The default setting
static char* pathvar    = "POS64PATH";    // The default setting
static char* hdrsvar    = "POS64HDRS";    // The default setting
static char* scriptsvar = "POS64SCRIPTS"; // The default setting
#endif

#else

#ifdef CINTSYSyes
static const char* rootvar    = "BCPLROOT";    // The default setting
static const char* pathvar    = "BCPLPATH";    // The default setting
static const char* hdrsvar    = "BCPLHDRS";    // The default setting
static const char* scriptsvar = "BCPLSCRIPTS"; // The default setting
#endif

#ifdef CINTPOSyes
static char* rootvar    = "POSROOT";    // The default setting
static char* pathvar    = "POSPATH";    // The default setting
static char* hdrsvar    = "POSHDRS";    // The default setting
static char* scriptsvar = "POSSCRIPTS"; // The default setting
#endif

#endif

int tracing = 0;
int settracing = 0; // If >0 it is copied to tracing just before
                    // calling interpret in case Sys_interpret:

int filetracing = 0;
int dumpflag = 0;
int slowflag = 0;   /* If non zero always use the slow interpreter */
int boottrace = 0;

BCPLWORD memupb;
BCPLWORD Globupb=2000;  // MR 28/12/2019 used to be 1000
                        // also made a similar change in boot.b
BCPLWORD tallyupb, tallyvec, *tallyv, tallylim=0;
BCPLWORD vecstatsvupb, vecstatsvec, *vecstatsv;    /* Stats of getvec/freevec */
BCPLWORD dcountv;    /* To hold the BCPL pointer to the debug count vector */

BCPLWORD taskname[4];         /* Used in getvec for debugging */

// Functions defined in this file (cintsys.c).

void copyaddrB2C(void*from, void*to); // Used for saving and restoring
void copyaddrC2B(void*from, void*to); // host machine addresses.

BCPLWORD concatsegs(BCPLWORD seg1, BCPLWORD seg2);
BCPLWORD loadsysseg(char *name);
BCPLWORD loadseg(char *name);
void  unloadseg(BCPLWORD segl);
BCPLWORD rdhex(FILEPT fp);
BCPLWORD globin(BCPLWORD segl, BCPLWORD g);
BCPLWORD getvec(BCPLWORD upb);
BCPLWORD freevec(BCPLWORD p);
BCPLWORD muldiv(BCPLWORD a, BCPLWORD b, BCPLWORD c);
FILEPT pathinput(char *name, char *pathname);
BCPLWORD dosys(BCPLWORD p, BCPLWORD g);
BCPLWORD doflt(BCPLWORD op, BCPLWORD a, BCPLWORD b, BCPLWORD c);
char *prepend_prefix(char *fromstr, char *tostr);
char *vmsfname(char *name, char *vmsname);
char *osfname(char *name, char *osname);
void c2b_str(char *cstr, BCPLWORD bstr);
char *b2c_str(BCPLWORD bstr, char *cstr);
BCPLWORD syscin2b_str(char *cstr, BCPLWORD bstr);
char *catstr2c_str(char *cstr1, char *cstr2, char *str);
void  wrcode(char *form, BCPLWORD f, BCPLWORD a);
void  wrfcode(BCPLWORD f);
void  trace(BCPLWORD pc, BCPLWORD p, BCPLWORD a, BCPLWORD b);
void  dumpmem(BCPLWORD *mem, BCPLWORD upb, BCPLWORD context);
BCPLWORD timestamp(BCPLWORD *v);
void msecdelay(unsigned int delaymsecs);

// Interpreter implemented in assembly language.
// or compiled from cintterp.c when FASTyes or RASTERINGyes
// is defined.
extern int cintasm(BCPLWORD regs, BCPLWORD *mem);

// The standard interpreter implemented in C compiled
// without FASTyes or RASTERINGyes being defined.
extern int interpret(BCPLWORD regs, BCPLWORD *mem);

#define Globword       0x8F8F0000L

#define Gn_globsize    0
#define Gn_start       1
#define Gn_sys         3
#define Gn_currco      7
#define Gn_colist      8
#define Gn_rootnode    9
#define Gn_result2    10
#define Gn_cli_returncode    137

/* relocatable object blocks  */
#define T_hunk    1000L
#define T_reloc   1001L
#define T_end     1002L
#define T_hunk64  2000L
#define T_reloc64 2001L
#define T_end64   2002L
#define T_bhunk   3000L
#define T_bhunk64 4000L

BCPLFLOAT N2F(BCPLWORD x) {
  union {
    BCPLWORD i;   // Members i and f are either
    BCPLFLOAT f;  // both 32 or 64 bit values.
  } u;
  u.i = x;
  return u.f;
}

BCPLWORD F2N(BCPLFLOAT x) {
  union {
    BCPLWORD i;   // Members i and f are either
    BCPLFLOAT f;  // both 32 or 64 bit values.
  } u;
  u.f = x;
  return u.i;
}

void copyaddrC2B(void*from, void*to) {
  // This function copies a machine address from a C program
  // to one or two BCPL words in a BCPL programs.  
#ifdef CURRENT64
#ifdef TARGET64
  // 64 bit machine address
  // 64 bit BCPL word
  // so copy one 64 bit value
  ((BCPLINT64*)to)[0] = ((BCPLINT64*)from)[0];
#else
  // 64 bit machine address
  // 32 bit BCPL word
  // so copy two 32 bit values
  ((BCPLINT32*)to)[0] = ((BCPLINT32*)from)[0];
  ((BCPLINT32*)to)[1] = ((BCPLINT32*)from)[1];
#endif
#else
#ifdef TARGET64
  // 32 bit machine address
  // 64 bit BCPL word
  // so copy the 32 bit machine address extended to 64 bits
  ((BCPLINT64*)to)[0] = (BCPLINT64)(((BCPLINT32*)from)[0]);
#else
  // 32 bit machine address
  // 32 bit BCPL word
  // so copy one 32 bit value
  ((BCPLINT32*)to)[0] = ((BCPLINT32*)from)[0];
#endif
#endif
}

void copyaddrB2C(void*from, void*to) {
  // This function copies data from one or two BCPL to
  // a C variable holding am machine address pointed to
  // by the second argument.
#ifdef CURRENT64
#ifdef TARGET64
  // 64 bit machine address
  // 64 bit BCPL word
  // so copy one 64 bit value
  ((BCPLINT64*)to)[0] = ((BCPLINT64*)from)[0];
#else
  // 64 bit machine address
  // 32 bit BCPL word
  // so copy two 32 bit values
  ((BCPLINT32*)to)[0] = ((BCPLINT32*)from)[0];
  ((BCPLINT32*)to)[1] = ((BCPLINT32*)from)[1];
#endif
#else
  // 32 bit machine address
  // 32 or 64 bit BCPL word
  // so copy a 32 bit value to the C variable
  ((BCPLINT32*)to)[0] = ((BCPLINT32*)from)[0];
#endif
}

int badimplementation(void)
{ int bad = 0;
  int A='A';
  BCPLCHAR c = (BCPLCHAR)255;
  if(sizeof(BCPLWORD)!=(1<<B2Wsh)) {
    printf("Size of a BCPL word is %ld but 1<<B2Wsh is %d\n",
	    sizeof(BCPLWORD), 1<<B2Wsh);
       bad = 1;
  }
  if(A!=65) {
       printf("Character set is not ASCII\n");
       bad = 1;
  }
  if (c/-1 != 1) {
    printf("There is a problem with BCPLCHAR\n");
    bad = 1;
  }
  /* Test vmsfname */
  /*
  vmsfname("echo.b", chbuf4);
  vmsfname("com/echo.b", chbuf4);
  vmsfname("/mrich177/distribution/bcpl/g/libhdr.h", chbuf4);
  vmsfname("vd10$disk:/mrich177/junk.b", chbuf4);
  vmsfname("../cintcode/com/bcplfe.b", chbuf4);
  vmsfname("/junk", chbuf4);
  */
  return bad;
}

/*
** The following systematic changes were made on 6 Sep 2019.
**
** Whenever a BCPL program needs to hold a host machine address it
** allocates two consecutive BCPL words and uses a BCPL pointer
** to the first. On 32 bit BCPL running on a 64 bit architecture
** both words will be used. Otherwise only the first is needed.
** In sys calls such a machine addresses will always be passed
** as a BCPL pointer to the first word of the pair. The C code will
** assume that the pair is BCPL word aligned, not necessarily 64 bit
** aligned.
**
** This mechanism is used for quantities of type FILE* eliminating
** the need for the fpvec mechanism. It is also used when
** handling machine address interfaces with SDL, OpenGL and
** Pthread operations.
**
** The C function copyaddrC2B(void*from, void*to) is used to
** copy 32 or 64 bit C address to one or two BCPL words.

** The C function copyaddrB2C(void*from, void*to) is used to
** copy 32 or 64 bit value held in one or two BCPL word to
** a C address pointed to by the second argument.
*/

int mainpid=0;

extern BCPLWORD exitflag; /* Set to 1 on receiving SIGINT or SIGGEGV */

void (*old_inthandler)(int);
void (*old_segvhandler)(int);

void inthandler(int sig)
{ 
  printf("\nSIGINT received\n");
  old_inthandler = signal(SIGINT, old_inthandler);

#ifdef CINTPOSyes
  icount = 0;
  kcount = 0;

  if (W[rootnode+Rtn_intflag]==0) {
    W[rootnode+Rtn_intflag] = -1; // Will be cleared by sadebug
    old_inthandler = signal(SIGINT, inthandler);
    return;
  }

  // A second SIGINT received before sadebug entered.
  printf("\nSecond SIGINT received\n");
#endif
  
  close_keyb();

#ifdef TARGET64

#ifdef CINTPOSyes
  printf("\nLeaving Cintpos64\n");
#else
  printf("\nLeaving Cintsys64\n");
#endif

#else

  #ifdef CINTPOSyes
  printf("\nLeaving Cintpos\n");
#else
  printf("\nLeaving Cintsys\n");
#endif

#endif

  if(W[rootnode+Rtn_dumpflag])
  { W[rootnode+Rtn_sysp] = lastWp-W;
    W[rootnode+Rtn_sysg] = lastWg-W;
    dumpmem(W, memupb, 1);
    printf("\nMemory dumped to DUMP.mem, context=1\n");
  }
  /*if(mainpid) { kill(mainpid, SIGKILL); mainpid = 0; } */
  exit(0);
}

void segvhandler(int sig)
{ 
  printf("\nSIGSEGV received\n");
  old_segvhandler  = signal(SIGSEGV,  old_segvhandler);

  close_keyb();

#ifdef TARGET64

#ifdef CINTPOSyes
  printf("\nLeaving Cintpos64\n");
#else
  printf("\nLeaving Cintsys64\n");
#endif

#else

#ifdef CINTPOSyes
  printf("\nLeaving Cintpos\n");
#else
  printf("\nLeaving Cintsys\n");
#endif

#endif

  if(W[rootnode+Rtn_dumpflag])
  { W[rootnode+Rtn_sysp] = lastWp-W;
    W[rootnode+Rtn_sysg] = lastWg-W;
    dumpmem(W, memupb, 2);
    printf("\nMemory dumped to DUMP.mem, context=2\n");
  }
  /*if(mainpid) { kill(mainpid, SIGKILL); mainpid = 0; } */
  exit(0);
}

char *inbuf = NULL;	/* Buffer for input to interpreter */
int reattach_stdin = 0;	/* If =true switch to stdin after inbuf is empty */

/* Replacement for Readch() when taking stdin from a command line parameter */
int inbuf_next()
{ static int idx = 0;
  int c = inbuf[idx];
  if (c) {
    idx++;
    return c;
  }
  else return EOF; /* -1 */
}

/*
Set up a stream of characters to be prepended to the standard input, for the
cli to read before those in the normal input stream.  If leading_string is
provided, it is prepended to stream of characters.  This is used to support
executable cintcode files and CLI scripts on Unix systems. The leading_string
feature permits support of scripts by running the "c" command followed by its
command line arguments.
*/
void prepend_stdin(char *leading_string,
                   int argc,
                   char* argv[],
                   int argvpos) {
  int j;
  int inbuf_len;
  /* Total number of fields to prepend to standard input stream */
  int field_count = (leading_string?1:0) + argc - argvpos - 1;
  if (field_count == 0) {
    return; /* nothing to prepend */
  }
  /* Count space to allocate, beginning with leading_string */
  inbuf_len = leading_string ? strlen(leading_string) : 0;
  /* Add space for remaining fields */
  for (j=argvpos+1; j<argc; j++) {
    inbuf_len += strlen(argv[j]);
  }
  /* Add room for spaces between fields plus a trailing <cr> */
  /* plus a null terminator */
  inbuf_len += field_count + 1;
  /* Allocate buffer for input stream */
  if (!(inbuf = (char*)malloc(inbuf_len))) {
    perror("malloc");
    exit(-1);
  }
  inbuf[0] = 0;
  /* Fill the buffer */
  /* First prepend the leading string, if any */
  if (leading_string) strcat(inbuf, leading_string);
  /* Concatenate remaining args, separated by spaces */
  for (argvpos++; argvpos<argc; argvpos++) {
    if (inbuf[0]) strcat(inbuf, " ");
    strcat(inbuf, argv[argvpos]);
  }
  strcat(inbuf, "\n"); /* Simulate interactive end of line */

  /*printf("\nPrepended string:\n%s\n", inbuf); */
}

/* The MC package printf function */
BCPLWORD mcprf(char *format, BCPLWORD a) {
  // This probably has a 32/64 bit problem
  printf(format, a);
  return 0;
}

int main(int argc, char* argv[])
{ int rc;
  int i;           /* for FOR loops  */
  BCPLWORD res;    /* result of interpret  */

  for (i=0; i<4; i++) taskname[i] = 0;

  memupb   =  8000000L; //4000000L;
  tallyupb =  2000000L; // Default upb of tallyv

  vecstatsvupb = 20000L;

  mainpid = getpid();

  //for (i=0; i<argc; i++) printf("%2lld: %s\n", LL i, argv[i]);

  // Decode the command arguments
  for (i=1; i<argc; i++) {

    if (strcmp(argv[i], "-m")==0) {
      memupb   = atoi(argv[++i]);    // Memory upb in words
      continue;
    }

    if (strcmp(argv[i], "-g")==0) {
      Globupb   = atoi(argv[++i]);   // Default global vector upb
      continue;
    }

    if (strcmp(argv[i], "-t")==0) {
      tallyupb = atoi(argv[++i]);    // tallyv upb
      continue;
    }

    if (strcmp(argv[i], "-cin")==0) {
      pathvar = argv[++i];  // BCPLPATH variable name
      continue;
    }

    if (strcmp(argv[i], "-f")==0) {
      filetracing = 1;      // Trace use of environment variables
      continue;
    }

    if (strcmp(argv[i], "-d")==0) {
      dumpflag = 1;         // Set the dump flag
      continue;
    }

    if (strcmp(argv[i], "-slow")==0) {
      slowflag = 1;         // For use of the slow interpreter
      continue;
    }

    if (strcmp(argv[i], "-s")==0) {
      /*
      Invoke a CLI command interpreter giving it the current file to
      execute as a sequence of commands.  It is used to enable executable CLI
      scripts to be invoke from Unix.

      Example: Make and executable file test1 containing something like:

      #!/home/mr/distribution/Cintpos/cintpos/cintpos -s
      bcpl com/bcpl.b to junk
      echo "*nCompilation done*n"

      #!/home/mr/distribution/BCPL/cintcode/cintsys -s
      bcpl com/bcpl.b to junk
      echo "*nCompilation done*n"

      and run it by typing the Unix command: test1
      */

      char *fname = argv[i + 1];
      if (!fname) {
        fprintf(stderr, "missing parameter for -s\n");
        exit(-1);
      }
      prepend_stdin("c", argc, argv, i);
      break;
    }

    if (strcmp(argv[i], "--")== 0) {
      /* Like -c but re-attach standard input after exhausting command */
      /* line characters. */
      reattach_stdin = 1; /* TRUE */
    }

    if (strcmp(argv[i], "-c")== 0 || strcmp(argv[i], "--")== 0) {
      /* Allocate a buffer to hold remaining args as a string and pass
      ** remainder of command line to the interpreter.

      ** Typical usage:

      ** cintpos -c bcpl com/bcpl.b to junk
      ** cintsys -c bcpl com/bcpl.b to junk
      */

      prepend_stdin(NULL, argc, argv, i);
      break;
    }

    if (strcmp(argv[i], "-v")== 0) { boottrace=1; continue; }

    if (strcmp(argv[i], "-vv")== 0) { boottrace=2; continue; }

    if (strcmp(argv[i], "-h")!=0) printf("Unknown command option: %s\n",
					 argv[i]);

    printf("\nValid arguments:\n\n") ;
    printf("-h            Output this help information\n");
    printf("-m n          Set Cintcode memory size to n words\n");
    printf("-t n          Set Tally vector size to n words\n");
    printf("-g n          Set the default global vector upb to n\n");
    printf("-c args       "
           "Pass args to interpreter as CLI input\n");
    printf("-- args       "
           "Pass args to interpreter as CLI input, then re-attach stdin\n");
    printf("-s file args  "
           "Invoke the interpreter with this file as CLI input\n");
    printf("-cin name     Set the pathvar environment variable name\n");
    printf("-f            Trace use of environment variables in pathinput\n");
    printf("-v            Trace the bootstrapping process\n");
    printf("-vv           As -v, but include some Cincode level tracing\n");

    printf("-d            Cause a dump of the Cintcode memory to DUMP.mem\n");
    printf("              if a fault/error is encountered\n");

    printf("-slow         Force the slow interpreter to always be selected\n");

    return 0;
  }

  if (boottrace>0) printf("Boot tracing level is set to %d\n", boottrace);

#ifdef CINTPOSyes

  //#ifdef _POSIX_THREADS
  //  printf("  _POSIX_THREADS is defined\n");
  //#endif

#ifdef _POSIX_THREAD_PRIORITY_SCHEDULING
    //printf("  _POSIX_THREAD_PRIORITY_SCHEDULING is defined\n");
    { pthread_t self = pthread_self();
      pthread_attr_t thread_attr;
      int thread_policy;
      struct sched_param thread_param;
      int status;

      status = pthread_attr_init(&thread_attr);
      if(status != 0) {
	printf("attr_init failed\n");
      }

      thread_param.sched_priority = sched_get_priority_min(SCHED_FIFO);
      if(thread_param.sched_priority==-1)
        printf("failed to get priority min\n");
      status = pthread_setschedparam(
		  self, SCHED_FIFO, &thread_param);
      if(status != 0) {
	printf("setschedparam failed\n");
        printf("status=%d\n", status);
        printf("ENOSYS=%d, ESRCH=%d, EINVAL=%d, ENOTSUP=%d, EPERM=%d\n",
           ENOSYS, ESRCH, EINVAL, ENOTSUP, EPERM);
      }

      status = pthread_getschedparam(
		  self, &thread_policy, &thread_param);
      if(status != 0) {
	printf("getschedparam failed\n");
      }
      status = pthread_attr_getschedparam(
                       &thread_attr, &thread_param);
      if(status != 0) {
	printf("getschedparam failed\n");
      }
      //printf("Interpreter thread policy is: %s\n",
      //     (thread_policy==SCHED_FIFO ? "FIFO" :
      //      thread_policy==SCHED_RR ? "RR" :
      //      thread_policy==SCHED_OTHER ? "OTHER" : "Unknown"));
      //printf("Interpreter priority is: %d\n", thread_param.sched_priority);
    }
#endif

#endif

  if (memupb<50000 || tallyupb<0) {
    printf("Bad -m or -t size\n");
    return 10;
  }

  if(boottrace) {
    char *bytestr = "ABCD1234";
    int litend = 0;

#ifdef TARGET64

#ifdef CINTPOSyes
    printf("\ncintpos64 04 Nov 2019  10:26\n\n");
#else
    printf("\ncintsys64 04 Nov 2019  10:26\n\n");
#endif

#else

#ifdef CINTPOSyes
    printf("\ncintpos 04 Nov 2019  10:26\n\n");
#else
    printf("\ncintsys 04 Nov 2019  10:26\n\n");
#endif

#endif

    printf("bytestr=\"%s\" whose first word is %8llx\n",
	   bytestr, LL *(PT bytestr));
    if((((BCPLWORD*)bytestr)[0]&255)=='A') litend = 1;

#ifdef BIGENDER
    printf("BIGENDER is defined");
    if (litend==1) printf(" but the host machine is a little ender");
#else
    printf("BIGENDER is not defined");
    if (litend==0) printf(" but the host machine is a big ender");
#endif

    printf("\n");

#ifdef CURRENT64
    printf("CURRENT64 is defined  ");
#else
    printf("CURRENT64 is not defined  ");
#endif

#ifdef TARGET64
    printf("TARGET64 is defined\n");
#else
    printf("TARGET64 is not defined\n");
#endif

    printf("sizeof(int)        = %d\n", (int)sizeof(int));
    printf("sizeof(long)       = %d\n", (int)sizeof(long));
    printf("sizeof(BCPLWORD)   = %d\n", (int)sizeof(BCPLWORD));
    printf("sizeof(BCPLWORD *) = %d\n", (int)sizeof(BCPLWORD *));
  }

  if (badimplementation())
  { printf("This implementation of C is not suitable\n");
    return 0;
  }

  /* Allocate Cintcode memory */
  { int membytes = (memupb+tallyupb+vecstatsvupb+7)<<B2Wsh;

#ifdef MMAP
    int pagesize = sysconf(_SC_PAGE_SIZE);
    if(boottrace>0)
      printf("Allocating Cintcode memory using mmap, pagesize=%d\n", pagesize);
    W = PT (mmap(NULL, membytes,
                 PROT_READ|PROT_WRITE|PROT_EXEC,
                 MAP_PRIVATE|MAP_ANON, 0, 0));
    //                 PROT_READ | PROT_WRITE | PROT_EXEC, MAP_ANON, 0, 0));
    if (boottrace>0)
      printf("Cintcode memory allocated using mmap\n");
#else
    W = PT malloc(membytes);
    if (boottrace>0)
      printf("Cintcode memory allocated using malloc\n");
#endif
 
    if (W==NULL) {
      printf("Unable to allocate Cintcode memory of size %d\n", membytes>>B2Wsh);
      return 0;
    }
  }
 
  /*  printf("Cintcode memory %8llx (unrounded)\n", LL (UBCPLWORD) W); */
  W = PT(((long) W + 7L) & -8L);
  /*  printf("Cintcode memory %8llx (rounded)\n", LL (UBCPLWORD)W);  */

  sysp  = 0; // Not currently in executing the sys instruction.
  sysg  = 0;
  sysst = 0;

  lastWp = W;
  lastWg = W;
  lastst = 3; /* Pretend to be running in the interrupt service routine */

  for (i=0; i<memupb; i++) W[i] = 0xDEADC0DE;

  if (boottrace>0) printf("Cintcode memory (upb=%lld) allocated\n",
			   LL memupb);

  W[0] = memupb+1;  /* Initialise heap space to allow getvec to work.
                       At 0 we have a free block of size memupb */
  W[memupb] = 0;    /* This is a used block of size zero marking the end of
                       memory */

  // The tally vector is used to count Cintcode instruction executions
  tallylim = 0;
  tallyvec = memupb+1;
  tallyv = &W[tallyvec];
  tallyv[0] = tallyupb;
  for (i=1; i<=tallyupb; i++) tallyv[i] = 0;

  // vecstatsv holds the number of blocks of each size currently allocated 
  vecstatsvec = tallyvec + tallyupb + 1;
  vecstatsv = &W[vecstatsvec];
  for(i=0; i<=vecstatsvupb; i++) vecstatsv[i] = 0;
 
  getvec(rootnode-14);  /* Allocate space for interrupt vectors and other
                           system variables, not used by cintsys */

  // Allocate the rootnode in exactly the correct place in Cintcode memory
  if (rootnode != (i=getvec(100L)+1)) {
    printf("The root node was at %d not at %d\n", i, rootnode);
  }

  // Allocate space for the environment variable names
  // and file prefix string.
  rootvarstr    = getvec(5*16);
  pathvarstr    = rootvarstr+1*16;
  hdrsvarstr    = rootvarstr+2*16;
  scriptsvarstr = rootvarstr+3*16;
  prefixstr     = rootvarstr+4*16;
  prefixbp      = (char *)(&W[prefixstr]);
  for (i=0; i<=5*16; i++) W[rootvarstr+i] = 0;

  c2b_str(rootvar,    rootvarstr);
  c2b_str(pathvar,    pathvarstr);
  c2b_str(hdrsvar,    hdrsvarstr);
  c2b_str(scriptsvar, scriptsvarstr);

  // dcounts can be incremented within both C and BCPL code.
  // For BCPL dcountv is held in the root node.
  dcountv    = getvec(511);        // Allocate the debug counts vector
  W[dcountv] = 511;                // Set the upper bound in dcountv!0
  for(i=1; i<=511; i++) W[dcountv+i] = 0;

  if (filetracing) {
    char *path = getenv(rootvar);
    printf("Environment variable %s", rootvar);
    printf(" = %s\n", path);
    path = getenv(pathvar);
    printf("Environment variable %s", pathvar);
    printf(" = %s\n", path);
    path = getenv(hdrsvar);
    printf("Environment variable %s", hdrsvar);
    printf(" = %s\n", path);
    path = getenv(scriptsvar);
    printf("Environment variable %s", scriptsvar);
    printf(" = %s\n", path);
  }

  stackbase = getvec(Stackupb+6);  // +6 because it will be a coroutine
  globbase  = getvec(Globupb);
  result2 = 0L;

  if (boottrace>0) printf("Boot's stack allocated at %lld\n", LL stackbase);

  W[stackbase] = Stackupb; // Tell boot the size of its stack.
  for(i=1; i<=Stackupb+6; i++) W[stackbase+i] = 0xABCD1234;
  /* boot will turn this stack into a coroutine stack */

  if (boottrace>0)
    printf("Boot's global vector allocated at %lld\n", LL globbase);

  for(i = 0; i<=Globupb; i++) W[globbase+i] = Globword+i;
  W[globbase+Gn_globsize] = Globupb;
  W[globbase+Gn_rootnode] = rootnode;

  for(i=0; i<=Rtn_upb; i++) W[rootnode+i] = 0;

#ifdef CINTPOSyes
  W[rootnode+Rtn_system]       = 2;  // 1 for Cintsys, 2 for cintpos
#else
  W[rootnode+Rtn_system]       = 1;  // 1 for Cintsys, 2 for cintpos
#endif
  
  W[rootnode+Rtn_membase]      = 0;
  W[rootnode+Rtn_memsize]      = memupb;
  W[rootnode+Rtn_blklist]      = 0;
  W[rootnode+Rtn_tallyv]       = tallyvec;
  W[rootnode+Rtn_vecstatsv]    = vecstatsvec;
  W[rootnode+Rtn_vecstatsvupb] = vecstatsvupb;
  W[rootnode+Rtn_boottrace]    = (BCPLWORD) boottrace;
  W[rootnode+Rtn_dumpflag]     = dumpflag;

  { // This fudge is for systems where the size of BCPLWORD
    // and BCPLWORD* are different. The fields mc0 to mc3 are
    // only used by the MC package
    union { BCPLWORD *p;
            BCPLWORD v[2];} pv;
    union { BCPLWORD (*f)(char*, BCPLWORD);
            BCPLWORD v[2];} fv;
    pv.v[0] = pv.v[1] = 0;
    pv.p = W;               /* Cintcode memory */
    W[rootnode+Rtn_mc0]          = pv.v[0];
    W[rootnode+Rtn_mc1]          = pv.v[1];
    fv.v[0] = fv.v[1] = 0;
    fv.f = mcprf;           /* The MC prf fn */
    W[rootnode+Rtn_mc2]          = fv.v[0];
    W[rootnode+Rtn_mc3]          = fv.v[1];
  }

  // Copy environment variable names into the rootnode as BCPL strings.
  W[rootnode+Rtn_rootvar]      = rootvarstr;
  W[rootnode+Rtn_pathvar]      = pathvarstr;
  W[rootnode+Rtn_hdrsvar]      = hdrsvarstr;
  W[rootnode+Rtn_scriptsvar]   = scriptsvarstr;

  W[rootnode+Rtn_dcountv]      = dcountv;

  W[rootnode+Rtn_hostaddrsize]  = 8 * sizeof(BCPLWORD*);
  W[rootnode+Rtn_gvecsize]  = Globupb; // Added 28/12/2019

  W[rootnode+Rtn_joystickfd]   = 0;
  W[rootnode+Rtn_joystickfd1]  = 0;
  W[rootnode+Rtn_icount] = 100000000; // Estimated Cintcode instrs per sec.

  if (boottrace>0) printf("Rootnode allocated at %d\n", rootnode);

  if (boottrace>0) printf("About to loading the resident programs\n");

  { BCPLWORD seg = loadseg("syscin/boot");
    if (seg==0) {
      printf("\nUnable to load segment syscin/boot\n");
      printf("This is probably caused by incorrect settings of\n");
      printf("environment variables such as BCPLPATH or POSPATH\n");
      printf("Try entering cintsys using the command\n\n");

#ifdef CINTPOSyes
      printf("cintpos -f -v\nor\n");
      printf("cintpos64 -f -v\n\n");
#else
      printf("cintsys -f -v\nor\n");
      printf("cintsys64 -f -v\n\n");
#endif

      printf("to see what is wrong\n");
      return 20;
    }

    W[rootnode+Rtn_boot] = globin(seg, globbase);
    if(W[rootnode+Rtn_boot]==0) {
      printf("Can't globin boot\n");
      return 20;
    }

    if (boottrace>0) printf("syscin/boot loaded successfully\n");

    seg = loadseg("syscin/blib");
    if (seg==0) { printf("Can't load syscin/blib\n"); return 20; }
    if (boottrace>0) printf("syscin/blib loaded successfully\n");

    seg = concatsegs(seg, loadseg("syscin/syslib"));
    if(seg==0) { printf("Can't load syscin/syslib\n"); return 20; }
    if (boottrace>0) printf("syscin/syslib loaded successfully\n");

    seg = concatsegs(seg, loadseg("syscin/dlib"));
    if(seg==0) { printf("Can't load syscin/dlib\n"); return 20; }
    if (boottrace>0) printf("syscin/dlib loaded successfully\n");

    W[rootnode+Rtn_blib] = globin(seg, globbase);
    if(W[rootnode+Rtn_blib]==0) {
      printf("Can't globin {blib,syslib,dlib}\n");
      return 20;
    }

#ifdef CINTPOSyes
    seg = loadseg("syscin/klib");
    if (seg==0) { printf("Trouble with klib\n"); return 20; }
    if (boottrace>0) printf("syscin/klib loaded successfully\n");
    W[rootnode+Rtn_klib] = globin(seg, globbase);
    if(W[rootnode+Rtn_klib]==0) {
      printf("Can't globin klib\n");
      return 20;
    }
#endif
    
  }
  
  /* Set handler for CTRL-C */
  old_inthandler  = signal(SIGINT, inthandler); /* To catch CTRL-C */
  /* Set handler for segment violation */
  old_segvhandler  = signal(SIGSEGV, segvhandler); /* To catch segv */

  /* Make sys available via the root node */
  W[rootnode+Rtn_sys] = W[globbase+Gn_sys];

  /*
  boot has no coroutine environment.  Note that boot's global vector is also
  used by interrupt service routines in cintpos. The chain of stack frames
  in such an environment must be terminated by a zero link (for DEBUG to
  work properly).
  */
  W[globbase+Gn_currco] = 0;
  W[globbase+Gn_colist] = 0;

  /*
  ** Set the boot registers ready for the Cintcode interpreter.
  ** These are the register setting used by the very first entry
  ** into the Cintcode interpreter. They cause start in BOOT.b to
  ** be entered. 
  */

  W[bootregs+0] = 0;                /* A */
  W[bootregs+1] = 0;                /* B */
  W[bootregs+2] = 0;                /* C */
  W[bootregs+3] = stackbase<<B2Wsh; /* P */
  W[bootregs+4] = globbase<<B2Wsh;  /* G */
  W[bootregs+5] = 2;                /* ST -- in boot, interrupts disabled */
  W[bootregs+6] = W[globbase+1];    /* PC (start in boot) */
  W[bootregs+7] = -1;               /* Count */
  W[bootregs+8] = 0;                /* MW */

  /* Uncomment the following line to trace Cintcode execution */
  /* tracing = 1; */

#ifdef CINTPOSyes
  initdevices();  // Defined in devices.c
#endif

  init_keyb();

  if (boottrace>0) printf("Calling interpret(bootreg, W)\n");
  if (boottrace>1)
  { printf("with instruction tracing on\n\n");
    tracing = 1;
  }

  /* Call the slow interpreter even though Count=-1 */
  res = interpret(bootregs, W);
  if (boottrace>0)
    printf("Returned from interpreter(..) with result %lld\n", LL res);

  close_keyb();

  if (res) {
    printf("\nExecution finished, return code %lld\n", LL res);

    if (res && W[rootnode+Rtn_dumpflag]) {
      dumpmem(W, memupb, 3);
      printf("\nCintpos memory dumped to DUMP.mem, context=3\n");
    }
  }
  printf("\n");
  /*Change suggested by Dave Lewis - return cli_returncode rather than res */
  return W[globbase+Gn_cli_returncode]; /* MR 17/01/06 */
}

BCPLWORD concatsegs(BCPLWORD seg1, BCPLWORD seg2) {
  BCPLWORD p = seg1;
  if(p==0 || seg2==0) return 0;
  while (W[p]) p = W[p];
  /*printf("concatsegs: seg1 %lld ", LL seg1); */
  /*printf("seg2 %lld ", LL seg2); */
  /*printf("p %lld\n", LL p); */
  W[p] = seg2;
  return seg1;
}

BCPLWORD loadsegfp(FILEPT fp)
{ BCPLWORD list  = 0;
  BCPLWORD liste = 0;

  for(;;)
  { BCPLWORD type = rdhex(fp);
    /*printf("loadsegtype: type = %lld\n", LL type); */
    switch((int)type)
    { default:
          err:   unloadseg(list);
                 list = 0;
      case -1:   
                 return list;
		 
#ifdef TARGET64
      case T_hunk64:
#else
      case T_hunk:
#endif
               { BCPLWORD i, n=rdhex(fp);
                 BCPLWORD space = getvec(n);
		 /*printf("loading hunk size %lld ", LL n); */
		 /*printf("to %lld\n", LL space); */
                 if(space==0) goto err;
                 W[space] = 0;
                 for(i = 1; i<=n; i++) W[space+i] = rdhex(fp);
                 if(list==0) list=space;
                 else W[liste] = space;
                 liste = space;
                 continue;
               }
		 
#ifdef TARGET64
      case T_bhunk64:
#else
      case T_bhunk:
#endif
                /* For hunks in binary (not hex) */
                { BCPLWORD n;
                  BCPLWORD space;
                  int len = fread((char *)&n, (size_t)4, (size_t)1, fp);
                  if (len!=1 && len!=4) goto err;
                  space = getvec(n);
                  if(space==0) goto err;
                  W[space] = 0;
                  len = fread((char *)&W[space+1], (size_t)4, (size_t)n, fp);
                  if (len!=n && len!=4*n) goto err;
                  if(list==0) list=space;
                  else W[liste] = space;
                  liste = space;
                  continue;
                }
		 
#ifdef TARGET64
      case T_end64:
#else
      case T_end:
#endif
	          break;
    }		 
  }		 
} 		 

BCPLWORD loadseg(char *file)
{ BCPLWORD list  = 0;
  BCPLWORD liste = 0;

  //printf("loadseg: attempting to load file %s\n", file);

  // First look in the current directory
  FILEPT fp = pathinput(file, 0);

  if (fp)
  { list = loadsegfp(fp);
    fclose(fp);
    if (list) return list;
    fp = 0;
  }
 
  // The module was not found in the current directory or it
  // was invalid so look in the directories specified by the environment
  // variable whose name is in the pathvar entry of the rootnode.
  if (fp==NULL) {
    char envname[256];
    fp = pathinput(file, b2c_str(W[rootnode+Rtn_pathvar], envname));
  }

  if (fp==NULL) return 0;

  list = loadsegfp(fp);
  fclose(fp);
  return list;
}

void unloadseg(BCPLWORD segl)
{ while (segl) { BCPLWORD s = W[segl];
                 freevec(segl);
                 segl = s;
               }
}

/* rdhex reads in one hex number including the terminating character
   and returns its BCPLWORD value. EOF returns -1.
*/
BCPLWORD rdhex(FILEPT fp)
{  BCPLWORD w = 0;
   int ch = fgetc(fp);

   while (ch==' ' || ch=='\n' || ch=='\r') ch = fgetc(fp);

   if (ch=='#') { /* Remove comments from object modules */
                  while (ch != '\n' && ch != EOF) ch = fgetc(fp);
                  return rdhex(fp);
                }

   for(;;)
   {  int d = 100;
      if('0'<=ch && ch<='9') d = ch-'0';
      if('A'<=ch && ch<='F') d = ch-'A'+10;
      if('a'<=ch && ch<='f') d = ch-'a'+10;
		 
      if(d==100) return ch==EOF ? -1 : w;
      w = (w<<4) | d;
      ch = fgetc(fp);
   }		 
}		 
		 
BCPLWORD globin(BCPLWORD segl, BCPLWORD g)
{ /* This function initialises the globals defined in
     every section in the list segl.
  */
  BCPLWORD  a = segl, globsize = W[g];

  /*printf("globin segl = %6lld  ", LL segl); */
  /*printf("g = %6lld\n", LL g); */
  while (a) { BCPLWORD base = (a+1)<<B2Wsh;
              BCPLWORD i = a + W[a+1];
              if (W[i]>globsize) return 0;
	      /*printf("globin:  base %6lld  ", LL (a+1)); */ 
	      /*printf("to %6lld  ", LL i); */
	      /*printf("size: %5lld\n", LL W[a+1]); */ 
              for(;;) { i -= 2;
                        if (W[i+1]==0) break;
                        W[g+W[i]] = base + W[i+1];
			/*printf("globin:  g[%3lld] ", LL W[i]);  */
			/*printf("= %6lld\n", LL (base+W[i+1])); */
                      }
              a = W[a];
            }
  return segl;
}

BCPLWORD getvec(BCPLWORD requpb)
{ BCPLWORD p;
  BCPLWORD q = 0; /* The start of the block list */
  BCPLWORD upb = requpb<0 ? 0 : requpb;
  BCPLWORD size = 1+requpb+1+3+1+4+1; /*  Required block size in words */
                                      /*  1 for size and flag word */
                                      /* +requpb+1 number of requested words */
                                      /* +3 pattern words for safety */
                                      /* +1 word to hold the requested upb. */
                                      /* +4 words of task name. */
                                      /* +1 word pointing to start of block. */
  size = (size+1) & (-2);  /* Round the block size up to an even number */

  //printf("getvec: requpb=%d size=%d\n", (int)requpb, (int)size);
  do
  { p = q;
    for(;;) { BCPLWORD n = W[p];
              if((n&1) != 0) break;
              if( n == 0)    return 0;
              p += n;
            }
    q = p;  /* find next used block */
    for(;;) { BCPLWORD n = W[q];
              if((n&1) == 0) break;
              q += n-1;
            }
  } while(q-p<size);
  
  if(p+size!=q)
    W[p+size] = q-p-size+1; /* If splitting, the size of the other part with a
                               flag of 1 indicating that it is free */
  W[p] = size;              /* size of this block in words with a flag of zero
                               indicating that it is allocated */
  /* The following improves the safety of memory allocation */
  /* by adding some checkable redundancy */
  W[p+size-10] = 0xDDDDDDDD;  /* Funny pattern for possible roundup word */
  W[p+size- 9] = 0xCCCCCCCC;  /* Funny patterns after the requested vector */
  W[p+size- 8] = 0x55555555;
  W[p+size- 7] = 0xAAAAAAAA;
  W[p+size- 6] = requpb;      /* The requested upb */

  W[p+size- 5] = taskname[0]; /* The taskname, if any */
  W[p+size- 4] = taskname[1]; /* The taskname, if any */
  W[p+size- 3] = taskname[2]; /* The taskname, if any */
  W[p+size- 2] = taskname[3]; /* The taskname, if any */

  W[p+size-1] = p;           /* Pointer to base of this memory block */
  /*printf("getvec: allocating block %6lld  ", LL p); */
  /*printf("upb %4lld\n", LL upb); */

  // upb is the requested upper bound (forced to be >= 0)
  if (upb>vecstatsvupb) upb = vecstatsvupb;
  W[vecstatsvec+upb]++;
  //printf("getvec req upb = %lld  count=%lld\n", LL upb, LL W[vecstatsvec+upb]);
  return p+1;
}

BCPLWORD freevec(BCPLWORD p)
{ BCPLWORD size, upb;

  if (p==0) return -1; /* Ignore null pointer */

  p--;       /* All getvec'ed blocks start on odd addresses */
  size = W[p];

  if (size & 1) {
    printf("\n#### freevec: block at %lld already free\n", LL p);
    return 0;
  }

  /*printf("#### freevec: Freeing block at %lld\n", LL p); */

  if (
     W[p+size-9]!=0xCCCCCCCC ||
     W[p+size-8]!=0x55555555 ||
     W[p+size-7]!=0xAAAAAAAA ||
     W[p+size-1]!=p) {   /* Back pointer */
       printf("\n#### freevec: block at %lld ", LL p);
       printf("size %lld corrupted", LL size);
       printf("\n#### freevec: check words\n");
       printf("%7lld: %8llx should be CCCCCCCC\n",
	       LL(p+size-9), LL (UBCPLWORD)W[p+size-9]);
       printf("%7lld: %8llx should be 55555555\n",
	       LL(p+size-8), LL (UBCPLWORD)W[p+size-8]);
       printf("%7lld: %8llx should be AAAAAAAA\n",
	       LL(p+size-7), LL (UBCPLWORD)W[p+size-7]);
       printf("%7lld: %8lld should be the requested upb\n",
	       LL(p+size-6), LL W[p+size-6]);
       printf("%7lld: %8lld should be %8lld\n", LL(p+size-1),
	       LL W[p+size-1], LL p);
       return 0;
  }
  W[p] |= 1; /* Mark the block a free */

  { /* Deal with getvec allocation statistics */
    int upb = W[p+size-6];  /* The requested upper bound */
    if (upb>vecstatsvupb) upb = vecstatsvupb;
    W[vecstatsvec+upb]--;   /* Decrement count of block of this size */
    //printf("freevec req upb = %lld count=%lld\n", LL upb, LL W[vecstatsvec+upb]);
  }
  return -1;
}

BCPLWORD muldiv1(BCPLWORD a, BCPLWORD b, BCPLWORD c)
{ // This version produces the same results as the other version
  // and seems to run about 60% faster.
  // It was used by the MDIV instruction (and the syslib muldiv function).
  // This version cannot be used when TARGET64 is defined.
  BCPLINT64 ab = (BCPLINT64)a * (BCPLINT64)b;
  if(c==0) c=1;
  result2 = (BCPLWORD)(ab % c);
  return (BCPLWORD)(ab / c);
}

BCPLWORD muldiv(BCPLWORD a, BCPLWORD b, BCPLWORD c)
{ // This is now used by sys(Sys_muldiv,...)
  unsigned BCPLWORD q=0, r=0, qn, rn;
  unsigned BCPLWORD ua, ub, uc;
  int qneg=0, rneg=0;
  if(c==0) c=1;

  if(a<0) { qneg=!qneg; rneg=!rneg; ua = -a; }
  else                              ua =  a;

  if(b<0) { qneg=!qneg; rneg=!rneg; ub = -b; }
  else                              ub =  b;

  /* Make ua no larger than ub to minimize the number of iterations */
  if(ub<ua) { uc=ua; ua=ub; ub=uc; }

  if(c<0) { qneg=!qneg;             uc = -c; }
  else                              uc =  c;
  
  qn = ub / uc;
  rn = ub % uc;
  
  while(ua)
  { //printf("qn=%8llx rn=%8llx ua=%8llx ", LL qn, LL rn, LL ua);
    //printf("q =%8llx r =%8llx\n", LL q, LL r);
    if(ua&1) { q += qn;
               r += rn;
               if(r>=uc) { q++; r -= uc; }
             }
    ua >>= 1;
    qn <<= 1;
    rn <<= 1;
    if(rn>=uc) { qn++; rn -= uc; }
    //printf("ua=%8llx q =%8llx r =%8llx\n", LL ua, LL q, LL r);
  }
  //printf("qn=%8llx rn=%8llx ", LL qn, LL rn);
  //printf("q =%8llx r =%8llx\n\n", LL q, LL r);
  result2 = rneg ? -(BCPLWORD)r : r;
  return    qneg ? -(BCPLWORD)q : q;
}

int relfilename(char *name) {
/*
name is a C string representing a Cintsys/Cintpos file name.
If it starts with '/' (or '\') or contains a colon ':', it
represents an absolute file name otherwise it is relative.
This function returns
   0 if absolute,
   1 if relative.
*/
  if(name[0]=='/' || name[0]=='\\') return 0;
  while(*name) if(*name++==':') return 0;
  return 1; 
}

/* pathinput does not use any of chbuf1, chbuf2 or chbuf3. */
FILEPT pathinput(char *name, char *pathname)
/* name is a Linux style filename using '/' (not '\') as the separator.
   If pathname is null, name is looked up in the current directory,
   otherwise name is looked up in the directories that the environment
   variable pathname specifies. These directories should now be separated 
   by ';' (nor ':') even under Linux or Cygwin.
*/
{ FILEPT fp = 0;

  if ( pathname==0 || !relfilename(name)) {
    /* If no pathname given or name is absolute just search
       the current directory */
    //printf("pathinput: pathname=0\n");
    fp = fopen(osfname(name, chbuf4), "rb");
    if(filetracing)
    { printf("Trying: %s in the current directory - ", name);
      if(fp) {
        printf("found\n");
      } else {
        printf("not found\n");
      }
    }
    return fp;
  }

  //printf("pathinput: pathname=%s\n", pathname);

  /* Look through the PATH directories if pathname is given. */
  { char *path = getenv(pathname);
 
    if(filetracing) {
      printf("pathinput: attempting to open %s", name);
      printf(" using\n  %s", pathname);
      printf(" = %s\n", path);
    }

    /* Try prefixing with each directory in the path until the
       file can be openned successfully */
    while(path)
    { char str[256];
      char *filename = &str[0];
      char *f=filename;
      char *n=name;
      char lastch=0;
      // f points to the next character of constructed filename
      // n points to the next character of name

      // Prefix the next directory name after skipping over
      // a separator, if necessary.
      while(1)
      { char ch = *path;
        if(ch==';') { path++; continue; }
#ifndef WINNAMES
        if(ch==':') { path++; continue; }
#endif
        break;
      }
      if(*path==0) break;
      /* Copy the directory name into filename */
      while(1)
      { char ch = *path;
        if(ch==0) break;
        path++;
        if(ch==';') break;
#ifndef WINNAMES
	if(ch==':') break;
#endif
        *f++ = ch;
        lastch = ch;
      }
      /* Insert a filename '/' if necessary. */
      if(lastch!='/' && lastch!='\\') *f++ = '/';

      /* Append the given file name */
      while(1)
      { char ch = *n++;
        *f++ = ch;
        if(ch==0) break;
      }

      fp = fopen(osfname(filename, chbuf4), "rb");
      if(filetracing)
      { printf("Trying: %s - ", filename);
        if(fp) {
          printf("found\n");
	} else {
          printf("not found\n");
	}
      }
      if(fp) return fp;
      // Try using the next directory in the path
    }
    // Not found in any of the path directories so look in the
    // current directory

    fp = fopen(osfname(name, chbuf4), "rb");
    if(filetracing)
    { printf("Trying: %s in the current directory - ", name);
      if(fp) {
        printf("found\n");
      } else {
        printf("not found\n");
      }
    }
    return fp;
  }
}

BCPLWORD dosys(register BCPLWORD p, register BCPLWORD g)
{ register BCPLWORD i;

  BCPLWORD res = 0; // The default result
  
  sysp = p;         // The values of p and g of the most recent call
  sysg = g;         // of dosys are saved and for use by dumpmem which
                    // stores them in the root node before dumping
                    // the entire Cintcode memory.
  
  /*printf("dosys(%lld, ", LL p); */
  /*printf("%lld, LL g); */
  /*PINTFD(") P3=%lld ", LL W[p+3]); */
  /*printf("P4=%lld\n",  LL W[p+4]); */
  switch((int)(W[p+3]))
  { default: printf("\nBad sys number: %lld\n", LL W[p+3]);
             return W[p+3];

    /* case Sys_setcount: set count               -- done in cinterp
    ** case Sys_quit:     return from interpreter -- done in cinterp

    ** case Sys_rti:      sys(Sys_rti, regs)      -- done in cinterp  Cintpos
    ** case Sys_saveregs: sys(Sys_saveregs, regs) -- done in cinterp  Cintpos
    ** case Sys_setst:    sys(Sys_setst, st)      -- done in cinterp  Cintpos
    */
    case Sys_tracing:  /* sys(Sys_tracing, b) */
            tracing = W[p+4];
            return 0;
    /* case Sys_watch:    sys(Sys_watch, addr)    -- done in cinterp
    */

    case  Sys_tally:         /* sys(Sys_tally, flag)     */
             if(W[p+4]) {
                tallylim = tallyupb;
                for(i=1; i<=tallylim; i++) tallyv[i] = 0;
              } else {
                tallylim = 0;
              }
              return 0;
     
    case Sys_interpret: // call interpreter (recursively) */
            { BCPLWORD regsv = W[p+4];
	      BCPLWORD rc;
	      // Start interpreting Cintcode instructions from
	      // the state specified by regsv in the
	      // Cintcode memory W.
              if(W[regsv+7]>=0 || slowflag || settracing>0) {
		tracing = settracing;
		settracing = 0;
	        res = interpret(regsv, W);
	      } else {
                res = cintasm(regsv, W);
	      }
	      break;
            }

	    //    case Sys_interpret: /* call interpreter (recursively) */
            //{ BCPLWORD regsv = W[p+4];
            //  if(W[regsv+7]>=0 || slowflag) return interpret(regsv, W);
            //  return CINTASM  (regsv, W);
            //}

    case Sys_getbuildno:
    { // Return the current build number
      // and set result2 to the current build flags.
      // This is currently under development.
      BCPLWORD res=0;
      BCPLWORD res2=0;
      // Set the buildno
#ifdef forLinux
      res = bld_Linux;
#elif forLinuxSDL
      res = bld_LinuxSDL;
#elif forLinuxSDL2
      res = bld_LinuxSDL2;
#elif forLinuxGL
      res = bld_LinuxGL;     // This will be phased out
#elif forLinuxSDLGL
      res = bld_LinuxSDLGL;
#elif forLinuxSDL2GL
      res = bld_LinuxSDL2GL;

#elif forLinuxRaspi
      res = bld_LinuxRaspi;
#elif forLinuxRaspiSDL
      res = bld_LinuxRaspiSDL;
#elif forLinuxRaspiSDL2
      res = bld_LinuxRaspiSDL2;
#elif forLinuxRaspiSDLGL
      res = bld_LinuxRaspiSDLGL;
#elif forLinuxRaspiSDL2GL
      res = bld_LinuxRaspiSDL2GL;

#elif forVmsVax
      res = bld_VmsVax;
#elif forWin32
      res = bld_Win32;
#elif forCYGWIN32
      res = bld_CYGWIN32;
#else
      res = bld_Unknown;
#endif
      
      // Set the flags result
#ifdef SOUND
      res2 |= 1;
#endif
#ifdef CALLC
      res2 |= 1<<1;
#endif
      W[g+Gn_result2] = res2;
      return res;
    }

    case Sys_settracing:
              settracing = W[p+4];
	      break;

    case Sys_sardch:
              { int ch;

                if (inbuf) {
                  /* Input taken from command line */
                  ch = inbuf_next();
                  if (ch == EOF) { /* inbuf is now empty */
                    if (reattach_stdin) {
                      free(inbuf);
                      inbuf = 0; /* Don't try to read more characters */
		                 /* from the buffer */
                      /* Continue with normal read from stdin */
                    } else {
                      return ch; /* EOF */
                    }
                  } else {
                    return ch; /* valid character */
                  }
                }
                /* Normal case, stdin input to interpreter */

                ch = Readch(); /* get the keyboard character  */
		/*printf("Sys_sardch: ch = %d\n", ch); */
		if(ch==127) /* RUBOUT from the keyboard */
		  ch = 8;   /* Replace by BS */
                if (ch>=0) putchar(ch);
                if(ch==13) { ch = 10; putchar(10); }
                fflush(stdout);
		/*printf("Sys_sardch returning ch = %d\n", ch); */
                //incdcount(1);
                return ch;
              }

    case Sys_sawrch:
#ifdef forCYGWIN32
              if(W[p+4] == 10) putchar(13);
#endif
              putchar((char)W[p+4]);
              fflush(stdout);
              //incdcount(2);
              return 0;

    case Sys_read:  /* bytesread := sys(Sys_read, fp, buf, bytecount) */
              { FILEPT fp = 0;
		copyaddrB2C(&W[W[p+4]], &fp);
                BCPLWORD bbuf = W[p+5]<<B2Wsh;
                BCPLWORD len  = (int) W[p+6];

                clearerr(fp); /* clear eof and error flag */

                len = fread(&(BP W)[bbuf], (size_t)1, (size_t)len, fp);

                if(ferror(fp)) { perror("sys_read");
                                 return -1; /* check for errors */
		}

                return len;
              }

    case Sys_write:
     { FILEPT fp = 0;
	copyaddrB2C(&W[W[p+4]], (BCPLWORD*)&fp);
        BCPLWORD bbuf = W[p+5]<<B2Wsh;
        BCPLWORD len = W[p+6];
        /*fseek(fp, 0L, SEEK_CUR); */ /* Why?? MR 9/7/04 */
        len = WD fwrite(&(BP W)[bbuf], (size_t)1, (size_t)len, fp);
        fflush(fp);
        return len;
      }

    case Sys_openread: // sys(Sys_openread, filename, path, fpptr)
      { char *name = b2c_str(W[p+4], chbuf1);
        FILEPT fp;
        fp = pathinput(name,                      /* Filename */
	               b2c_str(W[p+5], chbuf2));  /* Environment
	      				             variable */
        if(fp==0) return 0L;
	//printf("Sys_openread: filename=%s fp=%16lX\n", chbuf1, (BCPLINT64)fp);
       	copyaddrC2B(&fp, &W[W[p+6]]);
	//printf("Sys_openread: =%8X %8X\n", W[W[p+6]], W[W[p+6]+1]);
        return 1;
      }

    case Sys_openwrite:
      { char *name = b2c_str(W[p+4], chbuf1);
        FILEPT fp;
        fp = fopen(osfname(name, chbuf4), "wb");
        if(fp==0) return 0L;
       	copyaddrC2B(&fp, &W[W[p+5]]);
        return 1;
      }

    case Sys_openappend:
      { char *name = b2c_str(W[p+4], chbuf1);
        FILEPT fp;
        fp = fopen(osfname(name, chbuf4), "ab");
        if(fp==0) return 0L;
       	copyaddrC2B(&fp, &W[W[p+5]]);
        return 1;
      }

    case Sys_openreadwrite:
      { char *name = b2c_str(W[p+4], chbuf1);
	FILE *fp = fopen(osfname(name, chbuf4), "rb+");
        if(fp==0) fp = fopen(name, "wb+");
        if(fp==0) return 0L;
       	copyaddrC2B(&fp, &W[W[p+5]]);
        return 1;
      }

    case Sys_close:
    { FILE *fp = 0;
      copyaddrB2C(&W[W[p+4]], &fp);
      BCPLWORD res = fclose(fp);
      //freefno(W[p+4]);
      return res==0 ? -1 : 0; /* res==0 means success */
    }

    case Sys_deletefile:
    { char *name = b2c_str(W[p+4], chbuf1);
      FILEPT fp;
      name = osfname(name, chbuf4);
#ifdef VMSNAMES
      { /* Append ';*' to name */
        int len = 0;
        while (name[len]) len++;
        name[len++] = ';';
        name[len++] = '*';
        name[len] = 0;
      }
#endif
      return ! REMOVE(name);
    }

    case Sys_renamefile:
    { char *name1 = b2c_str(W[p+4], chbuf1);
      char *name2 = b2c_str(W[p+5], chbuf2);
      int len = 0;
      name1 = osfname(name1, chbuf3);
      name2 = osfname(name2, chbuf4);
#ifdef VMSNAMES
      { /* Append ';*' to name2 */
        len = 0;
        while (name2[len]) len++;
        name2[len]   = ';';
        name2[len+1] = '*';
        name2[len+2] = 0;
      }
#endif
      REMOVE(name2);
#ifdef VMSNAMES
      name2[len] = 0;
#endif
      return ! rename(name1, name2);
    }

    case Sys_getvec:
              { BCPLWORD tcb = W[rootnode+Rtn_crntask];
                if (tcb) {
                  BCPLWORD *tn = &W[tcb+Tcb_namebase];
                  char *s = (char *)tn;
                  int len = s[0];
                  // Truncate to 15 characters
                  if(len>15) s[0] = 15;
	          taskname[0] = tn[0];
	          taskname[1] = tn[1];
	          taskname[2] = tn[2];
	          taskname[3] = tn[3];
                } else {
	          taskname[0] = 0;
	          taskname[1] = 0;
	          taskname[2] = 0;
	          taskname[3] = 0;
		}
                return getvec(W[p+4]);
	      }

    case Sys_freevec:
                return freevec(W[p+4]);

    case Sys_loadseg:
              { BCPLWORD tcb = W[rootnode+Rtn_crntask];
                BCPLWORD *tn = &W[tcb+Tcb_namebase];
                char *name = b2c_str(W[p+4], chbuf2);
	        taskname[0] = (tcb==0) ? 0 : tn[0];
	        taskname[1] = (tcb==0) ? 0 : tn[1];
	        taskname[2] = (tcb==0) ? 0 : tn[2];
	        taskname[3] = (tcb==0) ? 0 : tn[3];
                return loadseg(name);
	      }

    case Sys_globin:
                return globin(W[p+4], g);

    case Sys_unloadseg:
                unloadseg(W[p+4]);
                return 0;

    case Sys_muldiv:
    { BCPLWORD res =  muldiv1(W[p+4], W[p+5], W[p+6]);
      W[g+Gn_result2] = result2;
      return res;
    }

    case Sys_intflag:
      return intflag() ? -1L : 0L;

    case Sys_setraster:
      return setraster(W[p+4], W[p+5]);

    case Sys_cputime: /* Return CPU time in milliseconds  */
      return muldiv(clock(), 1000, TICKS_PER_SEC);

    case Sys_filemodtime:
    /* sys(Sys_filemodtime, filename, datv)
       Set the elements of datv to represent the date and time of
       the last modification of the given file, returning TRUE if
       successful and FALSE otherwise. datv!0 is the number of days
       since 1 January 1970, datv!1 is the number of milli-seconds
       since midnight and datv!2=-1 indicating that the new date and
       time format is being used.
       If the file does not exist or there is an error then
       FALSE is returned and the elements of datv are set to 0, 0 and
       -1, respectively.
    */
    { struct stat buf;
      BCPLWORD days, secs, msecs;
      char *name = b2c_str(W[p+4], chbuf1);
      BCPLWORD datestamp = W[p+5];
      if (stat(osfname(name, chbuf4), &buf)) {
        W[datestamp]   = 0;
        W[datestamp+1] = 0;
        W[datestamp+2] = -1;
        return 0;
      }
      secs = buf.st_mtime;
      // nsecs = buf.st_mtimensec; // nano second time, if poss
      days = secs / (24*60*60);
      msecs = (secs % (24*60*60)) * 1000;
      W[datestamp] = days;
      W[datestamp+1] = msecs;
      W[datestamp+2] = -1;  // New dat format
      //printf("filemodtime: name=%s days=%lld msecs=%lld\n",
      //        name, LL days, LL msecs);
      return -1;
    }

    case Sys_setprefix: /* Set the file name prefix string  */
    { BCPLWORD str = W[p+4];
      char *fp = (char*)(&W[str]);
      char *tp = (char*)(&W[prefixstr]);
      int i, len=*fp;
      if(len>63) return 0;
      for (i=0; i<=len; i++) *tp++ = *fp++;
      return prefixstr;
    }

    case Sys_getprefix: /* Return the file name prefix string  */
      return prefixstr;

    case Sys_graphics: /* Perform an operation on the graphics window  */
      return sysGraphics(p);

    case 35: /* Return TRUE if no keyboard character is available */
              return -1;

    case 36:   return 0; /* Spare */

    case 37:   return 0; /* Spare  */

    case Sys_seek:  /* res := sys(Sys_seek, fd, pos)   */
    { FILEPT fp = 0;
      copyaddrB2C(&W[W[p+4]], &fp);
      BCPLWORD pos = W[p+5];
      BCPLWORD res = fseek(fp, pos, SEEK_SET);
      W[g+Gn_result2] = errno;
      /*printf("fseek pos=%lld ", LL pos); */
      /*printf("=> res=%lld\n", LL res); */
      /*printf("errno=%lld\n", LL errno); */
      return res==0 ? -1 : 0; /* res=0 succ, res=-1 error  */
    }

    case Sys_tell: /* pos := sys(Sys_tell,fd)  */
    { FILEPT fp = 0;
      copyaddrB2C(&W[W[p+4]], &fp);
      BCPLWORD pos = ftell(fp);
      W[g+Gn_result2] = errno;
      /*printf("tell => %lld\n", LL pos); */
      return pos; /* >=0 succ, -1=error */
    }

    case Sys_waitirq:
              { // sys(Sys_waitirq, msecs) 
                // Wait for irq to be set or a timeout then 
                // signal irq_cv under the control of irq_mutex
                BCPLWORD msecs = W[p+4];
                //BCPLWORD sec, nsec;
                struct timespec tspec;
                BCPLINT64    secs = 0;
                unsigned int usecs = 0;

                const unsigned int secsperday = 60*60*24;

//#if defined(forWIN32) || defined(forCYGWIN32) || defined(forLINUX)
#if defined(forWIN32) || defined(forCYGWIN32)
                // Code for Windows, Cygwin and Linux
                // ftime is obsolete and the advice is to use
                // gettimeofday or clock_gettime
                // but neither of these seem to be widely available yet.
                struct timeb tb;
                ftime(&tb);
                secs = tb.time;
                //if(tb.dstflag) secs += 60*60;
                secs -= tb.timezone * 60;
                //printf("tb.dstflag=%d tb.timezone=%d\n", tb.dstflag, tb.timezone);
                usecs = tb.millitm * 1000;
#else
                // Code for systems having the function gettimeofday.
                struct timeval tv;
                gettimeofday(&tv, NULL);
                secs = tv.tv_sec;
                usecs = tv.tv_usec;
#endif

  // secs  = seconds since 1 January 1970
  // usecs = micro seconds since start of current second

                // Add the specified delay
                secs += msecs/1000;
                usecs += (msecs % 1000) * 1000;

                while (usecs>=1000000) { secs++; usecs-=1000000; }
                //printf("waitirq: usecs=%d secs=%d\n", usecs, secs);
                tspec.tv_sec = secs;
                tspec.tv_nsec = usecs*1000;
		//printf("waitirq: msecs=%d tspec.tv_sec=%d tspec.tv_nsec=%d\n",
		//       msecs, tspec.tv_sec, tspec.tv_nsec);
                pthread_mutex_lock(&irq_mutex);
                pthread_cond_timedwait(&irq_cv, &irq_mutex, &tspec);
                pthread_mutex_unlock(&irq_mutex);
                icount = 0; // Cause the interpreter to check for interrupts
                kcount = 0; // Cause the interpreter to check the clock
                return 0;
              }
      
    case Sys_lockirq: /* Stop all devices from modifying */
                      /* packets or generating interrupts */
#ifdef CINTPOSyes
      pthread_mutex_lock(&irq_mutex);
#endif
      return 0;

    case Sys_unlockirq: /* Allow devices to modify packets */
                        /* and generate interrupts */
#ifdef CINTPOSyes
      pthread_mutex_unlock(&irq_mutex);
#endif
      return 0;

    case Sys_devcom: /* res := sys(Sys_devcom, dcb, com, arg) */
#ifdef CINTPOSyes      
      return devcommand(W[p+4], W[p+5], W[p+6]);
#else
      return 0;
#endif

    case Sys_datstamp: /* res := sys(Sys_datstamp, v)  */
    // Set v!0 = days  since 1 January 1970
    //     v!1 = msecs since midnight
    //     v!2 = ticks =-1 for new dat format
    // Return -1 if successful
      return timestamp(&W[W[p+4]]);

    case Sys_filesize:  /* res := sys(Sys_filesize, fd)   */
    { FILE *fp = 0;
      copyaddrB2C(&W[W[p+4]], &fp);
      //printf("Sys_filesize fp=%16lX\n", (BCPLINT64)fp);
      long pos = ftell(fp);
      BCPLWORD rc = fseek(fp, 0, SEEK_END);
      BCPLWORD size = ftell(fp);
      rc  = fseek(fp, pos, SEEK_SET);
      if (rc) size = -1;
      return size; /* >=0 succ, -1=error  */
    }

    case Sys_getsysval: /* res := sys(Sys_getsysval, addr) */
              { BCPLWORD addr = W[p+4];
                return W[addr];
              }

    case Sys_putsysval: /* res := sys(Sys_putsysval, addr, val) */
              { BCPLWORD addr = W[p+4];
                W[addr] = W[p+5];
                return 0;
              }

    case Sys_shellcom: /* res := sys(Sys_shellcom, comstr) */
              { BCPLWORD comstr = W[p+4]<<B2Wsh;
	        int i;
                char com[256];
                int len = ((char *)W)[comstr];
                for(i=0; i<len; i++) com[i] = ((char *)W)[comstr+i+1];
                com[len] = 0;
		/*printf("\nmain: calling shell command %s\n", com); */
                return system(com);
              }

    case Sys_getpid: /* res := sys(Sys_getpid) */
                return getpid();

    case Sys_dumpmem: /* sys(Sys_dumpmem, context) */
                dumpmem(W, memupb, W[p+4]);
                printf("\nMemory dumped to DUMP.mem, context=%lld\n",
                        LL W[p+4]);
                return 0;

    case Sys_callnative: /* res := sys(Sys_callnative, fn, a1, a2, a3) */
              { /* Call native code. */
                union { /* To allow conversion from pointer to function */
		  BCPLWORD *p;
		  BCPLWORD(*f)(BCPLWORD, BCPLWORD, BCPLWORD);
                } func;

                func.p = &W[W[p+4]];
                return (func.f)(W[p+5],W[p+6],W[p+7]);
              }

    case Sys_platform:
              { /* Return platform code, 0 if unknown */
		BCPLWORD res = 0;
#ifdef forMAC
		res = 1;
#endif
#ifdef forMIPS
		res = 2;
#endif
#ifdef forSGI
		res = 3;
#endif
#ifdef forARM
		res = 4;
#endif
#ifdef forLinux
		res = 5;
#endif
#ifdef forLinuxamd64
		res = 6;
#endif
#ifdef forCYGWIN32
		res = 7;
#endif
#ifdef forLinuxPPC
		res = 8;
#endif
#ifdef forSUN4
		res = 9;
#endif
#ifdef forSPARC
		res = 10;
#endif
#ifdef forALPHA
		res = 11;
#endif
#ifdef forMSDOS
		res = 12;
#endif
#ifdef forOS2
		res = 13;
#endif
#ifdef forSHwinCE
		res = 14;
#endif
#ifdef forMIPSwinCE
		res = 15;
#endif
                return res;
              }              

    case Sys_inc: /* newval := sys(Sys_inc, ptr, amount) */
              { /* !ptr := !ptr + amount; RESULTIS !ptr */
                return W[W[p+4]] += W[p+5];
              }

    case Sys_buttons: /* Return bit pattern of buttons currently
                          pressed on the GP2X */
#ifdef forGP2X
              { unsigned long buttons = 0;
	        int fd = open("/dev/GPIO", O_RDWR | O_NDELAY);
	        if (fd<0) return -1;
                if (read(fd, &buttons, 4) != 4) return -2;
                close(fd);
                return (BCPLWORD)buttons;
              }
#else
              return -3;

#endif

    case Sys_delay: /* sys(Sys_delay, msecs) */
              { unsigned int msecs = (unsigned int)W[p+4];
                msecdelay(msecs);
                return 0;
              }

    case Sys_sound: /* res := sys(Sys_sound, fno, a1, a2,...) */
#ifdef SOUND
                return soundfn(&W[p+4], &W[g]);
#else
                return 0;
#endif

    case Sys_joy: /* res := sys(Sys_joy, fno, a1, a2,...) */
      return joyfn(&W[p+4], &W[g], W);

    case Sys_sdl: /* res := sys(Sys_sdl, fno, a1, a2,...) */
      return sdlfn(&W[p+4], &W[g], W);

    case Sys_gl: /* res := sys(Sys_gl, fno, a1, a2,...) */
      return glfn(&W[p+4], &W[g], W);

    case Sys_ext: /* res := sys(Sys_ext, fno, a1, a2,...) */
      return extfn(&W[p+4], &W[g], W);

    case Sys_callc: /* res := sys(Sys_callc, fno, a1, a2,...) */
#ifdef CALLC
      /*
         printf("dosys: sys(Sys_callc, %lld, %lld, %lld, ...)\n",
                 LL W[p+4], LL W[p+5], LL W[p+6]);
      */
                return callc(&W[p+4], &W[g]);
#else
                return -1;
#endif

    case Sys_trpush: /* sys(Sys_trpush, val) */
                trpush(W[p+4]);
                return 0;

    case Sys_settrcount: /* res := sys(Sys_settrcount, count) */
                return settrcount(W[p+4]);

    case Sys_gettrval: /* res := sys(Sys_gettrval, count) */
                return gettrval(W[p+4]);

    case Sys_flt: /* res := sys(Sys_flt, op, a, b, c)) */
              { BCPLWORD res = doflt(W[p+4], W[p+5], W[p+6], W[p+7]);
                W[g+Gn_result2] = result2;
                return res;
              }

    case Sys_pollsardch: /* res := sys(Sys_pollsardch) */
              {
                // Return a character if available otherwise pollch (=-3)
                return pollReadch();
              }

    case Sys_incdcount: /* res := sys(Sys_incdcount, n) */
                return incdcount(W[p+4]);


    case 135:
    { /* Return system date and time in VEC 5 */
      time_t clk = time(0);
      struct tm *now = gmtime(&clk);
      BCPLWORD *arg = &W[W[p+4]];
      arg[0] = now->tm_year+1900;
      arg[1] = now->tm_mon+1;
      arg[2] = now->tm_mday;
      arg[3] = now->tm_hour;
      arg[4] = now->tm_min;
      arg[5] = now->tm_sec;
      return 0;
    }

  }
  // This is unreadchable
  // return 0;
}

void msecdelay(unsigned int delaymsecs) {
#ifdef forWIN32
  { Sleep(delaymsecs); // ?????????????????
                return;
              }
#else
	      /*
              { while(delaymsecs>=1000) {
                  usleep(990000);
                  msecs -= 990;
                }
                if (delaymsecs>0) usleep(delaymsecs*1000);
                return;
              }
              */
              { // This version uses select() rather than usleep() or
                // nanosleep() since these seem to have problems on some
                // systems.
                const BCPLWORD msecsperday = 24*60*60*1000;
                BCPLWORD days, msecs;
                BCPLWORD tv[3]; // to hold [days, msecs, -1]
                struct timeval timeout;
                timestamp(tv);
                // calculate the wakeup time
                days = tv[0];               
                msecs = tv[1] + delaymsecs;
                if (msecs>=msecsperday) { days++; msecs -= msecsperday; }

                while(1) {
                  BCPLWORD diffdays, diffmsecs;
                  timestamp(tv);
                  diffdays = days - tv[0];
                  diffmsecs = msecs - tv[1];
                  if (diffdays>0) { diffdays--; diffmsecs += msecsperday; }
		  //printf("Sys_delay: diffmsecs = %lld msec\n", LL diffmsecs);
                  if (diffmsecs<=0) return;
                  if (diffmsecs>900) diffmsecs = 900;
                  timeout.tv_sec = 0;
                  timeout.tv_usec = diffmsecs * 1000;
		  //printf("Sys_delay: waiting for %lld msec\n", LL diffmsecs);
                  select(FD_SETSIZE, NULL, NULL, NULL, &timeout);
                }
              }
#endif
}

BCPLWORD doflt(BCPLWORD op, BCPLWORD a, BCPLWORD b, BCPLWORD c) {
  // Typically a is the left operand
  // and b is typically the right operand, if required.
  // c is currently only used by fl_radius3.
  // If TARGET64 is set, BCPLWORD is the type of a 64 bit integer
  // and BCPLFLOAT is the type of a 64 bit floating point number,
  // otherwise BCPLWORD and BCPLFLOAT are both of length 32.

  switch (op) {
    default:
      printf("\ndoflt(%lld, %lld, %lld...) not implemented\n", LL op, LL a, LL b);

    case fl_avail:
      return -1;

    case fl_mk: // eg sys(Sys_flt, fl_mk, 1234, -3) => 1.234 
    { // a=mantissa, b=exponent
      BCPLFLOAT da = (BCPLFLOAT) a;
      BCPLFLOAT res;
      while(b > 5) { da *= 100000.0; b-=5; }
      while(b > 0) { da *=     10.0; b--;  }
      while(b <-5) { da /= 100000.0; b+=5; }
      while(b < 0) { da /=     10.0; b++;  }
      res = (BCPLFLOAT) da;
      return F2N(res); 
    }

    case fl_unmk: // eg for 32 bit BCPL sys(Sys_flt, fl_unmk, 1.234)
                  //     result  = 123399997   9 digits
                  //     result2 =        -8
                  // If a is zero result and result2 are both zero
                  // On 64 bit BCPL the result has up to 18 digits with
                  // an appropriate exponent. Neither the most and least
                  // significant digits of the matissa are zero.
                  // Most of the calculation is done using 64 bit
                  // arithmetic.
    { BCPLINT64 mantissa;
      int exponent=0;
      FLOAT64 d = (FLOAT64) (N2F(a));  // N2F(x) is  (*(BCPLFLOAT*)&(x))
      int neg = 0;

      if (d==0.0) {
        result2 = 0;
        return 0;
      }

      if (d<0.0) { d = -d; neg = 1; }

      // while d>=10 divide by a power of 10 and adjust the exponent.

      while (d >= 100000.0 && exponent<100) {
        d = d / 100000.0; exponent+=5;
        //printf("unmk3: d = %15.9e %d\n", d, exponent);
      }

      while (d>=10.0 & exponent<100) {
        d = d / 10.0; exponent++;
        //printf("unmk4: d = %15.9e %d\n", d, exponent);
      }

      if (exponent>=100) {
        result2 = 100;
        return 1;
      }

      // d is now less than 10.0

      //printf("unmk5: d = %15.9e %d\n", d, exponent);
      while (d<=0.00001 && exponent>-100) {
        d *= 100000.0; exponent-=5;
        //printf("unmk6: d = %15.9e %d\n", d, exponent);
      }

      while (d<1.0 && exponent>-100) {
        d *= 10.0; exponent--;
        //printf("unmk7: d = %15.9e %d\n", d, exponent);
      }

      if (exponent<=-100) {
        result2 = 0; // Treat 1E-100 as 0.0
        return 0;
      }

      //printf("unmk: d should now be >=1.0 and <10.0\n");
      // d is now <10.0 and >= 1.0
      mantissa = floor(d * 1e16); // Mantissa has 17 digits
      exponent -= 16;

      //printf("unmk8: d = %24.18f mantissa=%18lld exponent=%lld\n",
      //        LL d, LL mantissa, LL exponent);

#ifndef TARGET64
      // For 32 bit BCPL, ensure that mantissa has no more
      // than 9 digits
      //printf("unmk: mantissa=%lld exponent=%d\n", mantissa, exponent);
      while(mantissa >= 1e10) {
        mantissa = mantissa/10;  // Divide by 10 without rounding
        exponent++;
	//printf("unmk: mantissa=%lld exponent=%d\n", mantissa, exponent);
      }
      if(mantissa >= 1e9) {
        mantissa = (mantissa+5)/10;  // Divide by 10 with rounding
        exponent++;
	//printf("unmk: mantissa=%lld exponent=%d\n", mantissa, exponent);
      }
#endif

      // Divide mantissa by 10 with rounding and correct the exponent.
      mantissa = (mantissa+5)/10;
      exponent++;

      // Ensure the least significant decimal digit of mantissa
      // is not 0. Checking mantissa non zero for safety.
      while (mantissa%10 == 0 && mantissa != 0) {
        mantissa = mantissa/10;
        exponent++;
	//printf("unmk: mantissa=%lld exponent=%d\n", mantissa, exponent);
      }

      if (mantissa==0) exponent=0; // For safety.

      //printf("unmk9: d = %24.18f mantissa=%18lld exponent=%d\n", d, mantissa, exponent);
      result2 = (BCPLWORD)exponent;
      if (neg) return (BCPLWORD)(-mantissa);
      return (BCPLWORD)(mantissa);
    }    

    case fl_float:
    { BCPLFLOAT fa = (BCPLFLOAT) a;
      return F2N(fa);
    }

    case fl_fix: // Return nearest integer
    { BCPLFLOAT fa = N2F(a);
      if(fa<0.0) return (BCPLWORD)(fa - 0.5);
      else       return (BCPLWORD)(fa + 0.5);
    }

    case fl_abs:
    { BCPLFLOAT fa = N2F(a);
      if (fa<0.0) fa = -fa;
      return F2N(fa);
    }

    case fl_mul:
    { BCPLFLOAT res = N2F(a) * N2F(b);
      return F2N(res); 
    }

    case fl_div:
    { BCPLFLOAT res = N2F(a) / N2F(b);
      return F2N(res); 
    }

    case fl_mod:
    { BCPLFLOAT res = Cfmod(N2F(a), N2F(b));
      return F2N(res);  // Return the floating point remainder
                        // after dividing a by b. Ie return
                        // x - n * y where n is the integer
                        // quotient of x / y. ie how many times
                        // y can be subtracted from x before
                        // the sign changes.
    }

    case fl_add:
    { BCPLFLOAT res = N2F(a) + N2F(b);
      return F2N(res);
    }

    case fl_sub:
    { BCPLFLOAT res = N2F(a) - N2F(b);
      return F2N(res);
    }

    case fl_pos:
      return a;

    case fl_neg:
    { BCPLFLOAT res = N2F(a);
      res = -res;
      return F2N(res);
    }

    case fl_eq:
      return N2F(a) == N2F(b) ? -1 : 0;

    case fl_ne:
      return N2F(a) != N2F(b) ? -1 : 0;

    case fl_ls:
      //printf("cintsys: fl_ls of %8X and %8X\n", a, b);
      //printf("cintsys: fl_ls of %13.4f and %13.4f\n", N2F(a), N2F(b));
      return N2F(a) < N2F(b) ? -1 : 0;

    case fl_gr:
      return N2F(a) > N2F(b) ? -1 : 0;

    case fl_le:
      return N2F(a) <= N2F(b) ? -1 : 0;

    case fl_ge:
      return N2F(a) >= N2F(b) ? -1 : 0;

    case fl_acos:
    { BCPLFLOAT res = Cacos(N2F(a));
      return F2N(res);
    }

    case fl_asin:
    { BCPLFLOAT res = Casin(N2F(a));
      return F2N(res);
    }

    case fl_atan:
    { BCPLFLOAT res = Catan(N2F(a));
      return F2N(res);
    }

    case fl_atan2:
    { BCPLFLOAT res = Catan2(N2F(a), N2F(b));
      return F2N(res);
    }

    case fl_cos:
    { BCPLFLOAT res = Ccos(N2F(a));
      return F2N(res);
    }

    case fl_sin:
    { BCPLFLOAT res = Csin(N2F(a));
      return F2N(res);
    }

    case fl_tan:
    { BCPLFLOAT res = Ctan(N2F(a));
      return F2N(res);
    }

    case fl_cosh:
    { BCPLFLOAT res = Ccosh(N2F(a));
      return F2N(res);
    }

    case fl_sinh:
    { BCPLFLOAT res = Csinh(N2F(a));
      return F2N(res);
    }

    case fl_tanh:
    { BCPLFLOAT res = Ctanh(N2F(a));
      return F2N(res);
    }

    case fl_exp:
    { BCPLFLOAT res = Cexp(N2F(a));
      return F2N(res);
    }

    case fl_frexp:
    // If a is non zero it returns a fraction f in the
    // range 0.5 (inclusive) to 1.0 (exclusive) and an
    // integer exponent e in result2 such that f = a x 2^e
    { int r2=222;
      // frexp expects an int* as its second argument but
      // actually places a floating point integer there.
      BCPLFLOAT fa = frexp(N2F(a), &r2);
      //printf("frexp: a=%13.3f => %13.6f  e=%d\n", N2F(a), fa, r2);
      result2 = r2;
      //printf("frexp: resul2=%d\n", result2);
      return F2N(fa);
    }

  case fl_ldexp:  // Returns a x 2^b where a is floating
                  //                 and b is an integer
    { BCPLFLOAT res = ldexp(N2F(a), b);
      return F2N(res);
    }

    case fl_log:
    { BCPLFLOAT res = log(N2F(a));
      return F2N(res);
    }

    case fl_log10:
    { BCPLFLOAT res = log10(N2F(a));
      return F2N(res);
    }

    case fl_modf:
    { BCPLFLOAT r1, r2;
#ifdef TARGET64
      r1 = modf(N2F(a), &r2);
#else
      r1 = modff(N2F(a), &r2);
#endif
      result2 = F2N(r2);   // The integer part of x as a BCPLFLOAT
      return F2N(r1);      // The fractional part of x
                          // both are the same sign a x
    }

    case fl_pow:
    { BCPLFLOAT res = pow(N2F(a), N2F(b));
      return F2N(res); 
    }

    case fl_sqrt:
    { BCPLFLOAT res = sqrt(N2F(a));
      return F2N(res);
    }

    case fl_ceil:
    { BCPLFLOAT res = ceil(N2F(a));
      return F2N(res);
    }

    case fl_floor:
    { BCPLFLOAT res = floor(N2F(a));
      return F2N(res);
    }

    case fl_N2F:
    { // eg sys(Sys_flt, fl_N2F, 1_000, 1_234) => 1.234
      BCPLFLOAT res = (BCPLFLOAT)b / (BCPLFLOAT)a;
      return F2N(res);
    } 

    case fl_F2N:
    { // eg sys(Sys_flt, fl_F2N, 1_000, 1.234) => 1_234
      BCPLFLOAT fa = (BCPLFLOAT)a;
      BCPLFLOAT fb = N2F(b);
      BCPLFLOAT res = fa * fb;
      if(res<0) { res = res - 0.5; }
      else      { res = res + 0.5; }
      return (BCPLWORD)res;
    }

    case fl_radius2:
    { // eg sys(Sys_flt, fl_radius2, 3.0, 4.0) => 5.0
      // since 9 + 16 = 25
      BCPLFLOAT fa = N2F(a);
      BCPLFLOAT fb = N2F(b);
      BCPLFLOAT res = sqrt(fa*fa + fb*fb); 
      return F2N(res);
    }

    case fl_radius3:
    { // eg sys(Sys_flt, fl_radius3, 1.0, 2.0, 2.0) => 3.0
      // since 1 + 4 + 4 = 9
      BCPLFLOAT fa = N2F(a);
      BCPLFLOAT fb = N2F(b);
      BCPLFLOAT fc = N2F(c);
      BCPLFLOAT res = sqrt(fa*fa + fb*fb + fc*fc); 
      return F2N(res);
    }

    case fl_64to32: // sys(Sys_flt, fl_64to32, a)
                    // a is a 32 or 64 bit floating point number.
                    // The result is the nearest 32 bit floating
                    // point number. Normally only used under
                    // 64 bit Cintcode.
    { FLOAT32 a32 = (FLOAT32)(*((BCPLFLOAT*)(&a)));
      BCPLWORD b32 = (BCPLWORD)(*((BCPLUINT32*)(&a32)));
      //printf("fl_64to32: a32=%f  b32=%d\n", a32, b32);
      return (BCPLWORD)(*((BCPLUINT32*)(&a32)));
    }
  }
}

BCPLWORD timestamp(BCPLWORD *v) {
  // Set v[0] = days since 1 January 1970
  //     v[1] = msecs since midnight
  //     v[2] = -1 for new date and time format
  // Return -1 if successful
#if defined(forMacOSPPC) || defined(forMacOSX)
  v[0]=v[1]=0;
  v[2]=-1;
  return 0;   /* To indicate failure */
#else
  unsigned int days  = 0;
  BCPLINT64    secs = 0;
  unsigned int msecs = 0;

  const unsigned int secsperday = 60*60*24;

#if defined(forWIN32) || defined(forCYGWIN32)
  // || defined(forLinux)
  { // Code for Windows, Cygwin and Linux
    // ftime is obsolete and the advice is to use
    // gettimeofday or clock_gettime
    // neither of these are widely available yet.
    struct timeb tb;
    ftime(&tb);
    secs = tb.time;
    //if(tb.dstflag) secs += 60*60;
    secs -= tb.timezone * 60;
    //printf("tb.dstflag=%lld tb.timezone=%lld\n",
    //        tb.dstflag, tb.timezone);
    msecs = tb.millitm;
  }
#else
  // Code for systems having the function gettimeofday.
  struct timeval tv;
  gettimeofday(&tv, NULL);
  secs = tv.tv_sec;
  msecs = tv.tv_usec / 1000;
  //secs += 60*60; /* Fudge -- Add one hour for British Summer Time */
#endif

  // secs  = seconds since 1 January 1970
  // msecs = micro seconds since start of current second

  secs += W[rootnode+Rtn_adjclock] * 60; /* Add clock adjustment */

  days  += secs / secsperday; // convert secs to days
  secs  %= secsperday; // Seconds since midnight
  msecs += secs*1000;  // milliseconds since midnight

  v[0] = days;  // days  since on 1 January 1970
  v[1] = msecs; // msecs  since midnight
  v[2] = -1;    // For new dat format
  //  printf("days=%lld msecs=%lld\n", days, msecs);
  return -1;   /* To indicate success */
#endif
}

char *vmsfname(char *name, char *vmsname) {
/*
This function converts a cintsys/cintpos filename to a VMS filename

Examples:

Name                       VMS name

echo.b                     echo.b
com/echo.b                 [.com]echo.b
/mrich177/distribution/bcpl/g/libhdr.h
                           [mrich177.distribution.bcpl.g]libhdr.h
vd10$disk:/mrich177/junk.b vd10$disk:[mrich177]junk.b
../cintcode/com/bcplfe.b   [-.cintcode.com]bcplfe.b
*/
  int ch;
  int i=0; // Next character in name
  int j=0; // Next character in vmsname
  int lastslashpos=-1; // Position of last slash in name

  /* If name contains a colon, copy all
     characters up to and including the colon.
  */
  while (1) {
    int ch = name[i];
    if (ch==0) {
      /* No colon in name */
      i = 0;
      break;
    }
    if (ch==':') {
      /* Copy up to and including the colon */
      while (j<=i) { vmsname[j] = name[j]; j++; }
      i = j;
      break;
    }
    i++;
  }
  /* Find position of last slash, if any */
  while (1) {
    int ch = name[i];
    if(ch==0) break;
    if(ch=='/') lastslashpos = i;
    i++;
  }

  /* No slashes  => nothing
     Leading /   => [
     Slashes but no leading slash so insert [. or [-

     name is then copied converting all slashes except the leading
     and last ones to dots, and converting the last slash to ].
  */
  i = j;
  if(name[i]=='/') {
    /* if leading slash but not the last convert it to [ */
    if (i!=lastslashpos) vmsname[j++] = '[';
    /* Otherwise skip over this leading slash */
    i++;
  } else {
    if (lastslashpos>=0) {
      /* Slashes but no leading slash, so insert [. or [-  */
      vmsname[j++] = '[';
      if(name[i]!='.' || name[i+1]!='.') {
        vmsname[j++] = '.';
      }
    }
  }

  while (1) {
    /* Copy characters
       replacing last /      by ]
       and       non last /  by .
       and       ..          by -
    */
    int ch = name[i];
    if(ch=='.' && name[i+1]=='.') {
      /* Convert .. to - */
      ch = '-';
      i++;
    }
    if(ch=='/') {
      if (i==lastslashpos) ch = ']';
      else                 ch = '.';
    }
    vmsname[j++] = ch;
    if(ch==0) break;
    i++;
  }
  /*
  printf("vmsfname of %s\n", name);
  printf("gives:      %s\n", vmsname);
  */

  return vmsname;
}

char *winfname(char *name, char *osname) {
/*
This function converts a cintsys/cintpos filename to a WIN filename

This copies name to winname replacing all '/' characters by '\'s.
*/
  char *p = osname;
  while(1) {
    int ch = *name++;
    if(ch=='/') ch = '\\';
    *p++ = ch;
    if(ch==0) return osname;
  }
}

char *unixfname(char *name, char *osname) {
/*
This function converts a cintsys/cintpos filename to a Unix filename
This copies name to winname replacing all '/' characters by '\'s.
*/
  char *p = osname;
  //printf("unixfname: name=%s\n", name);
  while(1) {
    int ch = *name++;
    if(ch=='\\') ch = '/';
    *p++ = ch;
    if(ch==0) return osname;
  }
}

char *osfname(char *name, char *osname) {
/*
This function converts the cintsys/cintpos filename to the OS filename
format. The possible formats are UNIX, WIN or VMS.
*/
  char *res=0;
  char buf[256];

  //printf("osfname: name=%s\n", name);

#ifdef VMSNAMES
  res = vmsfname(prepend_prefix(name, buf), osname);
#endif
#ifdef WINNAMES
  res = winfname(prepend_prefix(name, buf), osname);
#endif
#ifdef UNIXNAMES
  res = unixfname(prepend_prefix(name, buf), osname);
#endif
  if(res==0) {
    printf("Configuration error: ");
    printf("One of UNIXNAMES, WINNAMES or VMSNAMES must be set\n");
    return 0;
  }
  if(filetracing) {
    printf("osfname: %s => %s\n", name, res);
  }
  return res;
}

/*
Copy the prefix string into tostr and then append the fromstr
inserting '/' if necessary.
*/
char *prepend_prefix(char *fromstr, char *tostr)
{ char *pfxp = prefixbp;
  int pfxlen = *pfxp++;
  int i = 0;
  //printf("prepend_prefix: fromstr=%s pfxlen=%d\n", fromstr, pfxlen);
  if(pfxlen==0) return fromstr;
  if(!relfilename(fromstr)) return fromstr;

  //printf("prepend_prefix: prepending the prefix\n");

  while(pfxlen--) tostr[i++] = *pfxp++;
  /* Insert separator '/' between the prefix and name, if necessary. */
  if(tostr[i-1]!='/' || tostr[i-1]!='\\') tostr[i++] = '/';

  while (1)
  { char ch = *fromstr++;
    tostr[i++] = ch;
    if(ch==0) break;
  }
  //printf("prepend_prefix: gives tostr=%s\n", tostr);
  return tostr;
}

/*
** c2b_str converts a C string to a BCPL string.
*/
void c2b_str(char *cstr, BCPLWORD bstr)
{ int len = 0;
  char *p = cstr;
  char *str = ((char*)(W+bstr));
  while (*p && len<64) str[++len] = *p++; 
  str[0] = len;
}

/* b2c_str converts a BCPL string to a C character string. */
char *b2c_str(BCPLWORD bstr, char *cstr)
{ BCPLWORD bp = bstr<<B2Wsh;
  char *p = cstr;
  int len = (BP W)[bp++];
  if (bstr==0) return 0;
  while (len--) *p++ = (BP W)[bp++];
  *p = 0;
  return cstr;
}

/* syscin2b_str converts a syscin filename string to a BCPL string. */
BCPLWORD syscin2b_str(char *cstr, BCPLWORD bstr)
{  char *prfx = "syscin/";    /* System cin directory */
   char *bp = &((char *)W)[bstr<<B2Wsh];
   int len = 0;
   int i = 0;
   while (prfx[i]) { bp[++len] = prfx[i++]; }
   i = 0;
   while (cstr[i]) { bp[++len] = cstr[i++]; }
   bp[0] = len;
   return bstr;
}

/* catstr2c_str concatenates two optional C strings into str. */
char *catstr2c_str(char *cstr1, char *cstr2, char *str) {
  char *p = str;
  if (cstr1)
    while(*cstr1) *p++ = *cstr1++;
  if (cstr1)
    while(*cstr2) *p++ = *cstr2++;
  *p = 0;
  return str;
}

void wrcode(char *form, BCPLWORD f, BCPLWORD a)
{  wrfcode(f);
   printf ("  ");
   printf(form, a);
   printf ("\n");
} 

void wrfcode(BCPLWORD f)
{ char *s;
  int i, n = (f>>5) & 7;
  switch((int)f&31)
  { default:
    case  0: s = "     -     K   LLP     L    LP    SP    AP     A"; break;
    case  1: s = " FLTOP    KH  LLPH    LH   LPH   SPH   APH    AH"; break;
    case  2: s = "   BRK    KW  LLPW    LW   LPW   SPW   APW    AW"; break;
    case  3: s = "    K3   K3G  K3G1  K3GH   LP3   SP3   AP3  L0P3"; break;
    case  4: s = "    K4   K4G  K4G1  K4GH   LP4   SP4   AP4  L0P4"; break;
    case  5: s = "    K5   K5G  K5G1  K5GH   LP5   SP5   AP5  L0P5"; break;
    case  6: s = "    K6   K6G  K6G1  K6GH   LP6   SP6   AP6  L0P6"; break;
    case  7: s = "    K7   K7G  K7G1  K7GH   LP7   SP7   AP7  L0P7"; break;
    case  8: s = "    K8   K8G  K8G1  K8GH   LP8   SP8   AP8  L0P8"; break;
    case  9: s = "    K9   K9G  K9G1  K9GH   LP9   SP9   AP9  L0P9"; break;
    case 10: s = "   K10  K10G K10G1 K10GH  LP10  SP10  AP10 L0P10"; break;
    case 11: s = "   K11  K11G K11G1 K11GH  LP11  SP11  AP11 L0P11"; break;
    case 12: s = "    LF   S0G  S0G1  S0GH  LP12  SP12  AP12 L0P12"; break;
    case 13: s = "   LF$   L0G  L0G1  L0GH  LP13  SP13 XPBYT     S"; break;
    case 14: s = "    LM   L1G  L1G1  L1GH  LP14  SP14   LMH    SH"; break;
    case 15: s = "   LM1   L2G  L2G1  L2GH  LP15  SP15   BTC  MDIV"; break;
    case 16: s = "    L0    LG   LG1   LGH  LP16  SP16   NOP CHGCO"; break;
    case 17: s = "    L1    SG   SG1   SGH   SYS    S1    A1   NEG"; break;
    case 18: s = "    L2   LLG  LLG1  LLGH   SWB    S2    A2   NOT"; break;
    case 19: s = "    L3    AG   AG1   AGH   SWL    S3    A3  L1P3"; break;
    case 20: s = "    L4   MUL   ADD    RV    ST    S4    A4  L1P4"; break;
    case 21: s = "    L5   DIV   SUB   RV1   ST1   XCH    A5  L1P5"; break;
    case 22: s = "    L6   REM   LSH   RV2   ST2  GBYT  RVP3  L1P6"; break;
    case 23: s = "    L7   XOR   RSH   RV3   ST3  PBYT  RVP4  L2P3"; break;
    case 24: s = "    L8    SL   AND   RV4  STP3   ATC  RVP5  L2P4"; break;
    case 25: s = "    L9   SL$    OR   RV5  STP4   ATB  RVP6  L2P5"; break;
    case 26: s = "   L10    LL   LLL   RV6  STP5     J  RVP7  L3P3"; break;
    case 27: s = "  FHOP   LL$  LLL$   RTN  GOTO    J$ ST0P3  L3P4"; break;
    case 28: s = "   JEQ   JNE   JLS   JGR   JLE   JGE ST0P4  L4P3"; break;
    case 29: s = "  JEQ$  JNE$  JLS$  JGR$  JLE$  JGE$ ST1P3  L4P4"; break;
    case 30: s = "  JEQ0  JNE0  JLS0  JGR0  JLE0  JGE0 ST1P4 SELLD"; break;
    case 31: s = " JEQ0$ JNE0$ JLS0$ JGR0$ JLE0$ JGE0$    MW SELST"; break;
  }

  for(i = 6*n; i<=6*n+5; i++) putchar(s[i]);
} 

void trval(BCPLWORD w) {
  BCPLWORD gw = w & 0xFFFF0000;
  BCPLWORD gn = w & 0x0000FFFF;
    if(gw==Globword && gn<=1000) {
      printf("     #G%03lld# ", LL gn);
    } else {
      if(-10000000<=w && w<=10000000) {
        printf(" %10lld ", LL w);
      } else {
        printf(" #x%8llx ", LL (UBCPLWORD)w);
      }
    }
}

void trace(BCPLWORD pc, BCPLWORD p, BCPLWORD a, BCPLWORD b)
{ printf("A="); trval(a);
  printf("B="); trval(b);
  printf("P=%5lld ", LL p);
  printf("%9lld: ", LL pc);
  wrcode("(%3lld)", LL (BP W)[pc], LL (BP W)[pc+1]);
  putchar(13);
}

void dumpmem(BCPLWORD *mem, BCPLWORD upb, BCPLWORD context)
{ FILEPT fp;
  BCPLWORD count = upb;
  BCPLWORD p=0, q, r=0;
  int len = 0;
  BCPLWORD datstamp[3];

  fp = fopen("DUMP.mem", "wb");
  if(fp==0) goto bad;

  mem[rootnode + Rtn_sysp]   = lastWp-W;
  mem[rootnode + Rtn_sysg]   = lastWg-W;
  mem[rootnode + Rtn_sysst]  = lastst;

  mem[rootnode + Rtn_context] = context;
  /* context = 1   SIGINT received                 (sysp, sysg)
  ** context = 2   SIGSEGV received                (sysp, sysg)
  ** context = 3   fault while running boot        (bootregs)
  ** context = 4   user called sys(Sys_quit, -2)   (klibregs)
  ** context = 5   fault while user running        (klibregs)
  */

  datstamp[0]=datstamp[1]=datstamp[2]=0;
  timestamp(datstamp);
  mem[rootnode + Rtn_days]  = datstamp[0]; // days
  mem[rootnode + Rtn_msecs] = datstamp[1]; // msecs
  mem[rootnode + Rtn_ticks] = -1;          // new dat format

  /* Write out size of cintcode memory */
  /*printf("\n%8lld Memory upb\n", upb); */
  len = fwrite((char*)&count, (size_t)4, (size_t)1, fp);
  if(len!=1) goto bad;

  while(r<=upb)
  { BCPLWORD dataword = mem[r];
    q = r;
    while(r<=upb && mem[++r]==dataword) continue;
    if(r<=upb && r-q < 200) continue;

    /* mem[p]..mem[q-1] is the block
    ** mem[q]..mem[r-1] are repeated occurrences of dataword
    ** Write out count of words in next block
    */
    count = q-p; /* count>=0 */
    if(count)
    { /*printf("%8lld BLOCK\n", count); */
      len = fwrite((char*)&count, (size_t)4, (size_t)1, fp);
      if(len!=1) goto bad;
      len = fwrite((char*)&mem[p], (size_t)4, (size_t)count, fp);
      if(len!=count) goto bad;
    }

    /* Write out repetition count (negated) followed by the data word */
    count = q-r; /* count<=0 */
    if(count) {
      /*printf("%8lld x %8llx\n", -count, (UBCPLWORD)dataword); */
      len = fwrite((char*)&count,    (size_t)4, (size_t)1, fp);
      if(len!=1) goto bad;
      len = fwrite((char*)&dataword, (size_t)4, (size_t)1, fp);
      if(len!=1) goto bad;
    }

    p = r;
  }

 bad:
  if(fp) fclose(fp);
  /*printf("\nMemory dumped to DUMP.mem, context=%lld\n", context); */
  return; 
}

void trpush(BCPLWORD val) {
  // If trcount>=0 push val into the circular trace buffer
  // possibly preceeded by a time stamp.
  // Note that trpush is disabled if trcount=-1
#ifdef CINTPOSyes
  pthread_mutex_lock(&trpush_mutex);
#endif
  if(trcount>=0) {
#if defined(forMacOSPPC) || defined(forMacOSX)
#else
    BCPLWORD v[3];
    BCPLWORD msecs;
    timestamp(v);
    msecs = v[1]; /* milli-seconds since midnight */
    // Push the time stamp: hex 66000000 + <msecs since midnight>
    trvec[trcount++ & 4095] = 0x66000000 + msecs%60000;
#endif
    trvec[trcount++ & 4095] = val;
  }
#ifdef CINTPOSyes
  pthread_mutex_unlock(&trpush_mutex);
#endif
}

BCPLWORD settrcount(BCPLWORD count) {
  // Set trcount returning the previous value.
  // Setting trcount to -1 disables trpush.
#ifdef CINTPOSyes
  BCPLWORD res;
  pthread_mutex_lock(&trpush_mutex);
  res = trcount;
  trcount = count;
  pthread_mutex_unlock(&trpush_mutex);
#else
  BCPLWORD res = trcount;
  trcount = count;
#endif
  return res;
}

BCPLWORD gettrval(BCPLWORD count) {
  // Return the trace value corresponding to position count.
  // The result is only valid if the circular buffer has not overflowed
#ifdef CINTPOSyes
  BCPLWORD res;
  pthread_mutex_lock(&trpush_mutex);
  res = trvec[count & 4095];
  pthread_mutex_unlock(&trpush_mutex);
  return res;
#else
  return trvec[count & 4095];
#endif
}

BCPLWORD incdcount(BCPLWORD n) {
  BCPLWORD dv = W[rootnode + Rtn_dcountv];
    if(0 < n && n <= W[dv]) return ++W[dv+n];
  return -1;
}

#ifdef SOUND
#include "soundfn.c"
#endif


