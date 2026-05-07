/*
** This is a 32 bit CINTCODE interpreter written in C designed
** to run on most machines with Unix-like C libraries.
**
** (c) Copyright:  Martin Richards  September 2003
*/

/*
18/01/06 Added -c, --, and -s parameters to cintpos, to prepend characters
         before the start of stdin.
         -m and -t parameters now specify sizes in words (not words*1000).
29/10/03 Added memory dumping facility
18/11/02 Added getvec/freevec allocation statistics
13/11/01 Changed to use pthreads instead of fork and shared memory
19/7/01 Change to use shm key of IPC_PRIVATE
13/9/96 Changed to use usleep instead of sleep for finer grain clock
1/7/96 This program is based on cintmain.c from the Cintcode distribution
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <pthread.h>

/* cinterp.h contains machine/system dependent #defines  */
#include "cinterp.h"

#include <fcntl.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/timeb.h>

int irq=0;

char *inbuf = NULL; // For prepended standard input
int reattach_stdin = 0; // If =1 switch to stdin after inbuf is empty

pthread_mutex_t irq_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  irq_cv    = PTHREAD_COND_INITIALIZER;

/* Functions defined in devices.c  */
extern INT32 initdevices(void);
extern INT32 devcommand(INT32, INT32, INT32);

/* Functions defined in kblib.c  */
extern int Readch(void);
extern int init_keyb(void);
extern int close_keyb(void);
extern int intflag(void);

/* Functions defined in rastlib.c (or nullrastlib.c)  */
extern INT32 setraster(INT32 n, INT32 val);

/* Function defined in graphics.c  */
extern INT32 sysGraphics(INT32 p);
INT32 sysGraphics(INT32 p) { return 0; } /* Dummy definition */

#define Stackupb     500L
#define Globupb     1000L
#define Trbufupb    1001L

INT32pt W;  /* This will hold the pointer to the Cintcode memory */

INT32 *lastWp;    // Latest setting of Wp
INT32 *lastWg;    // Latest setting of Wg
INT32  lastst;    // Latest setting of st

INT32 prefix = 0; // Position in the Cintcode memory of the filename
                  // prefix. Zero means no prefix. The prefix is
                  // prepended to all non absolute file names.
                  // prefix is set by sys(Sys_setprefix, str)
                  // and read by sys(Sys_getprefix).

INT32 bstring,    /* Position in Cintcode memory of BCPL string workspace */
      stackbase,
      globbase,
      result2;

int tracing = 0;

INT32 memupb;
INT32 tallyupb, tallyvec, *tallyv, tallylim=0;
INT32 vecstatsvupb, vecstatsvec, *vecstatsv;    // Stats of getvec/freevec

INT32 taskname[4];         // Used in getvec for debugging

INT32 concatsegs(INT32 seg1, INT32 seg2);
INT32 loadsegc(char *name);
INT32 loadseg(INT32 name);
void  unloadseg(INT32 segl);
INT32 rdhex(FILE *fp);
INT32 globin(INT32 segl, INT32 g);
INT32 getvec(INT32 upb);
INT32 freevec(INT32 p);
INT32 muldiv(INT32 a, INT32 b, INT32 c);
FILE *pathinput(INT32 name, char *pathname);
INT32 dosys(INT32 p, INT32 g);
char *b2c_fname(INT32 bstr, char *cstr);
char *b2c_str(INT32 bstr, char *cstr);
INT32 syscin2b_str(char *cstr, INT32 bstr);
INT32 c2b_str(char *cstr, INT32 bstr);
void  wrcode(char *form, INT32 f, INT32 a);
void  wrfcode(INT32 f);
void  trace(INT32 pc, INT32 p, INT32 a, INT32 b);
void  dumpmem(INT32 *mem, INT32 upb, INT32 context);

extern int cintasm(INT32 regs, INT32pt mem);
extern int interpret(INT32 regs, INT32pt mem);

#define Globword  0x8F8F0000L

#define Gn_globsize    0
#define Gn_start       1
#define Gn_sys         3
#define Gn_currco      7
#define Gn_colist      8
#define Gn_rootnode    9
#define Gn_result2    10

/* relocatable object blocks  */
#define T_hunk  1000L
#define T_bhunk 3000L
#define T_end   1002L

int badimplementation(void)
{ int bad = 0, A='A';
  SIGNEDCHAR c = (SIGNEDCHAR)255;
  if(sizeof(INT32)!=4 || A!=65) bad = 1;
  if (c/-1 != 1) { PRINTFD("There is a problem with SIGNEDCHAR\n", 0);
                   bad = 1;
                 }
  return bad;
}

/* The following four functions are necessary since the type FILE*
** is too large for a BCPL word on some machines (such as the DEC Alpha)
*/

#ifdef forALPHA
#define Fnolim 100

FILE *fpvec[Fnolim];

int initfpvec(void)
{ INT32 i;
  for(i=1;i<Fnolim;i++) fpvec[i]=NULL;
  return 0;
}

INT32 newfno(FILE *fp)
{ INT32 i;
  for(i=1;i<Fnolim;i++) if(fpvec[i]==NULL){ fpvec[i]=fp; return i; }
  return 0;
}

INT32 freefno(INT32 fno)
{ if(0<fno && fno<Fnolim){fpvec[fno]=NULL; return 1; }
  return 0;
}

FILE *findfp(INT32 fno)
{ if(0<fno && fno<Fnolim) return fpvec[fno];
  return 0;
}

#else

int   initfpvec(void)    { return 0; }
INT32 newfno(FILE *fp)   { return WD fp; }
INT32 freefno(INT32 fno) { return fno; }
FILE *findfp(INT32 fno)  { return (FILE *)fno; }

#endif

int mainpid=0;
extern INT32 exitflag; // Set to one on receiving SIGINT or SIGGEGV

void (*old_inthandler)(int);
void (*old_segvhandler)(int);

void inthandler(int sig)
{ 
  old_inthandler = signal(SIGINT, old_inthandler);

  if (W[rootnode+Rtn_intflag]==0) {
    W[rootnode+Rtn_intflag] = -1; // Will be cleared by sadebug is running
    irq = 1;        // Get the interpreter to look at exitflag
    old_inthandler = signal(SIGINT, inthandler);
    return;
  }

  // A second SIGINT received before sadebug entered.
  PRINTFD("\nSIGINT received\n", 0);

  close_keyb();
  printf("Leaving Cintpos\n");
  if(W[rootnode+Rtn_dumpflag])
  { dumpmem(W, memupb, 1);
    printf("\nCintpos memory dumped to DUMP.mem, context=1\n");
  }
  //if(mainpid) { kill(mainpid, SIGKILL); mainpid = 0; }
  exit(0);
}

void segvhandler(int sig)
{ 
  printf("\nSIGSEGV received\n");
  old_segvhandler  = signal(SIGSEGV,  old_segvhandler);

  close_keyb();
  printf("Leaving Cintpos\n");
  if(W[rootnode+Rtn_dumpflag])
  { dumpmem(W, memupb, 2);
    printf("\nCintpos memory dumped to DUMP.mem, context=2\n");
  }
  //if(mainpid) { kill(mainpid, SIGKILL); mainpid = 0; }
  exit(0);
}

/* Read next from inbuf up to EOF, freeing inbuf when done */
static inline int inbuf_next()
{ static char *bp = NULL;
  if (inbuf) {
    if (bp == NULL) bp = inbuf;
    int c = *bp++;
    if (c) {
      return c;
    } else {
      free(inbuf);
      inbuf = NULL; // Don't try to read more characters from buffer
    }
  }
  return (EOF);
}

/* Replacement for Readch() when taking stdin from a command line parameter */
int read_buf_ch()
{ if (inbuf) { // Input taken from initial unix command line
    int ch = inbuf_next();
    if ((ch == EOF) && reattach_stdin) {
      return (Readch()); // Continue with normal read from stdin
    }
    return ch; // valid character or EOF
  }
  return (Readch()); // Normal case, stdin input to interpreter
}

/* Set up a stream of characters to be prepended to the standard input,
 * such that the interpreter reads these characters prior to those in the
 * normal input stream. If leading_string is provided, it is prepended
 * to stream of characters.
 * This is used to support executable cintcode files and CLI scripts
 * on Unix systems. The leading_string feature permits support of scripts
 * by running the "c" command followed by its command line arguments.
 */
void prepend_stdin(char *leading_string, int argc, char* argv[], int argvpos) {
  int j;
  /* Total number of fields to prepend to standard input stream */
  int field_count = (leading_string?1:0) + argc - argvpos - 1;
  if (field_count == 0) {
    return; /* nothing to prepend */
  }
  /* Count space to allocate, beginning with leading_string */
  int inbuf_len = leading_string?strlen(leading_string):0;
  /* Add space for remaining fields */
  for (j=argvpos+1; j<argc; j++) {
    inbuf_len += strlen(argv[j]);
  }
  /* Add room for spaces between fields plus a trailing <cr> */
  /* plus a null terminator */
  inbuf_len += field_count + 1;
  /* Allocate buffer for input stream */
  if (!(inbuf = malloc(inbuf_len))) {
    perror("malloc");
    exit(-1);
  }
  inbuf[0] = 0;
  /* Fill the buffer */
  /* Prepend the leading string if any */
  if (leading_string) strcat(inbuf, leading_string);
  /* Concatenate remaining args, separated by spaces */
  for (argvpos++; argvpos<argc; argvpos++) {
    if (inbuf[0]) strcat(inbuf, " ");
    strcat(inbuf, argv[argvpos]);
  }
  strcat(inbuf, "\n"); /* Simulate interactive end of line */

}

int main(int argc, char* argv[])
{ int rc;
  INT32 i;      /* for FOR loops  */
  INT32 res;    /* result of interpret  */

  for (i=0; i<4; i++) taskname[i] = 0;

  mainpid = getpid();

  memupb       = 4000000L;
  tallyupb     = 1000000L;
  vecstatsvupb =   20000L;

  for (i=1; i<argc; i++) {
    if (strcmp(argv[i], "-m")==0) {
      memupb   = atoi(argv[++i]);
      continue;
    }

    if (strcmp(argv[i], "-t")==0) {
      tallyupb = atoi(argv[++i]);
      continue;
    }

    if (strcmp(argv[i], "-s")==0) {
      // Invoke a cinterp/cintpos command interpreter to evaluate the
      // named file. Used to enable executable cinterp scripts on Unix.
      // Do not advance argument pointer, keep file name in remaining arg list.
      char *fname = argv[i + 1];
      if (!fname) {
        fprintf(stderr, "missing parameter for -s\n");
        exit(-1);
      }
      prepend_stdin("c", argc, argv, i);
      break;
    }

    if (strcmp(argv[i], "-c")== 0) {
      // Allocate a buffer to hold remaining args as a string and pass
      // remainder of command line to the interpreter. Used to enable
      // executable bytecode files on Unix.
      prepend_stdin(NULL, argc, argv, i);
      break;
    }

    if (strcmp(argv[i], "--")== 0) {
      // Pass remainder of command line to the interpreter.
      // Reattach standard input after exhausting command line characters.
      // Similar to -c, except that interpreter does not exit.
      reattach_stdin = 1; // TRUE
      // Allocate a buffer to hold remaining args as a string
      prepend_stdin(NULL, argc, argv, i);
      break;
    }

    if (strcmp(argv[i], "-h")!=0) PRINTFD("Unknown command option\n", 0);

    PRINTF("\nValid arguments:\n\n") ;
    PRINTF("-h            Output this help information\n", 0);
    PRINTF("-m n          Set Cintcode memory size to n words\n", 0);
    PRINTF("-t n          Set Tally vector size to n words\n", 0);
    PRINTF("-c args       "
           "Pass args to interpreter as standard input (executable bytecode)\n", 0);
    PRINTF("-- args       "
           "Pass args to interpreter standard input, then reattach stdin\n", 0);
    PRINTF("-s file args  "
           "Invoke command interpreter on file with args (executable scripts)\n", 0);
    return 0;
  }

  if (memupb<50000 || tallyupb<0) {
    PRINTFD("Bad -m or -k size\n", 0);
    return 10;
  }

  if(badimplementation())
  { PRINTFD("This implementation of C is not suitable\n", 0);
    return 0;
  }

  initfpvec();

  W = PT malloc((memupb+tallyupb+vecstatsvupb+3)<<2);

  if(W==NULL)
  { PRINTFD("Insufficient memory for memvec\n", 0);
    return 0;
  }
  W = PT(((long) W + 3L) & -4L);

  lastWp = W;
  lastWg = W;
  lastst = 3; // Pretend to be in the interrupt service routine

  for (i=0; i<memupb; i++) W[i] = 0xDEADC0DE;

  W[0] = memupb+1;  /* Initialise heap space */
  W[memupb] = 0;

  tallylim = 0;
  tallyvec = memupb+1;
  tallyv = &W[tallyvec];
  tallyv[0] = tallyupb;
  for(i=1; i<=tallyupb; i++) tallyv[i] = 0;

  vecstatsvec = tallyvec + tallyupb + 1;
  vecstatsv = &W[vecstatsvec];
  for(i=0; i<=vecstatsvupb; i++) vecstatsv[i] = 0;
 
  getvec(200L);  /* Allocate space for interrupt vectors
                    and the rootnode  */

  bstring   = getvec(64L);         // Workspace for c2b_str
  stackbase = getvec(Stackupb+6);  // +6 because it will be a coroutine
  globbase  = getvec(Globupb);
  result2 = 0L;

  //printf("initialising BOOT's stack\n");

  W[stackbase] = Stackupb; // Tell BOOT how large its stack is.
  for(i=1; i<=Stackupb+6; i++) W[stackbase+i] = 0xABCD1234;

  //printf("initialising BOOT's global vector\n");

  for(i = 0; i<=Globupb; i++) W[globbase+i] = Globword+i;
  W[globbase+Gn_globsize] = Globupb;
  W[globbase+Gn_rootnode] = rootnode;

  //printf("initialising the root node\n");

  for(i=0; i<=Rtn_upb; i++) W[rootnode+i] = 0;

  W[rootnode+Rtn_membase]      = 0;
  W[rootnode+Rtn_memsize]      = memupb;
  W[rootnode+Rtn_blklist]      = 0;
  W[rootnode+Rtn_tallyv]       = tallyvec;
  W[rootnode+Rtn_vecstatsv]    = vecstatsvec;
  W[rootnode+Rtn_vecstatsvupb] = vecstatsvupb;

  // Set up the trace circular buffer for KLIB debugging
  { INT32 trbuf = getvec(Trbufupb);
    if(trbuf==0) { printf("getvec failure\n"); return 20; }
    W[rootnode+Rtn_trword] = 0xBFBFBFBF;
    W[rootnode+Rtn_trbuf]  = trbuf;
    W[trbuf+0] = Trbufupb;
    W[trbuf+1] = 2;
    W[trbuf+2] = 0xBFBFBFBF;
    for(i=3; i<=Trbufupb; i++) W[trbuf+i] = 0;
  }

  //printf("Loading all resident programs and libraries\n");

  { INT32 seg = loadsegc("BOOT");
    if(seg==0) { printf("Trouble with BOOT\n"); return 20; }
    W[rootnode+Rtn_boot] = globin(seg, globbase);
    if(W[rootnode+Rtn_boot]==0) {
      printf("Can't globin BOOT\n");
      return 20;
    }

    seg = loadsegc("KLIB");
    if(seg==0) { printf("Trouble with KLIB\n"); return 20; }
    W[rootnode+Rtn_klib] = globin(seg, globbase);
    if(W[rootnode+Rtn_klib]==0) {
      printf("Can't globin KLIB\n");
      return 20;
    }

    seg = loadsegc("BLIB");
    if(seg==0) { printf("Trouble with BLIB\n"); return 20; }
    seg = concatsegs(seg, loadsegc("SYSLIB"));
    if(seg==0) { printf("Trouble with SYSLIB\n"); return 20; }
    seg = concatsegs(seg, loadsegc("DLIB"));
    if(seg==0) { printf("Trouble with DLIB\n"); return 20; }
    W[rootnode+Rtn_blib] = globin(seg, globbase);
    if(W[rootnode+Rtn_blib]==0) {
      printf("Can't globin {BLIB,SYSLIB,DLIB}\n");
      return 20;
    }
  }

  // Set handler for CTRL-C
  old_inthandler  = signal(SIGINT, inthandler); /* To catch CTRL-C */
  // Set handler for segment violation
  old_segvhandler  = signal(SIGSEGV, segvhandler); /* To catch segv */

  // Make sys available via the root node
  W[rootnode+Rtn_sys] = W[globbase+Gn_sys];

  // BOOT has no coroutine environment.
  // Note that BOOT's global vector is also used by interrupt
  // service routines. The chain of stack frames in such an environment
  // must be terminated by a zero link (for DEBUG to work properly).
  W[globbase+Gn_currco] = 0;
  W[globbase+Gn_colist] = 0;

  // Set the BOOT registers ready for the Cintcode interpreter

  W[bootregs+0] = 0;                // A
  W[bootregs+1] = 0;                // B
  W[bootregs+2] = 0;                // C
  W[bootregs+3] = stackbase<<2;     // P
  W[bootregs+4] = globbase<<2;      // G
  W[bootregs+5] = 2;                // ST -- in BOOT, interrupts disabled
  W[bootregs+6] = W[globbase+1];    // PC (start in BOOT)
  W[bootregs+7] = -1;               // Count

  initdevices();  // Defined in devices.c
  init_keyb();
  // Call the slow interpreter even though Count=-1
  //printf("cinterp: calling interpret\n");
  res = interpret(bootregs, W);
  //printf("cintpos: return from interpret\n");
  close_keyb();

  if (res) {
    PRINTFD("\nExecution finished, return code %ld\n", (long)res);

    if (W[rootnode+Rtn_dumpflag]) {
      printf("cintpos: dumpflag is true\n");
      dumpmem(W, memupb, 3);
      printf("\nCintpos memory dumped to DUMP.mem, context=3\n");
    }
  }
  printf("\n");
  return res;
}

INT32 concatsegs(INT32 seg1, INT32 seg2) {
  INT32 p = seg1;
  if(p==0 || seg2==0) return 0;
  while(W[p]) p = W[p];
  //printf("concatsegs: seg1 %d seg2 %d p %d\n", seg1, seg2, p);
  W[p] = seg2;
  return seg1;
}

INT32 loadsegc(char *name) {
  return loadseg(syscin2b_str(name, bstring));
}

INT32 loadseg(INT32 file)
{ INT32 list  = 0;
  INT32 liste = 0;
  
  FILE *fp = pathinput(file, "POSPATH");
  if(fp==NULL) return 0;
  //printf("loadseg: loading a file\n");
  for(;;)
  { INT32 type = rdhex(fp);
    //printf("loadsegtype = %d\n", type);
    switch((int)type)
    { default:
          err:   unloadseg(list);
                 list = 0;
      case -1:   fclose(fp);
                 return list;
		 
      case T_hunk:
               { INT32 i, n=rdhex(fp);
                 INT32 space = getvec(n);
		 //printf("loading hunk size %d to %d\n", n, space);
                 if(space==0) goto err;
                 W[space] = 0;
                  for(i = 1; i<=n; i++) W[space+i] = rdhex(fp);
                  if(list==0) list=space;
                  else W[liste] = space;
                  liste = space;
                  continue;
                }
		 
      case T_bhunk: /* For hunks in binary (not hex) */
                { INT32 n;
                  INT32 space;
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
		 
      case T_end:
	          break;
    }		 
  }		 
} 		 
		 
void unloadseg(INT32 segl)
{ while(segl) { INT32 s = W[segl];
                freevec(segl);
                segl = s;
              }
}

/* rdhex reads in one hex number including the terminating character
   and returns its INT32 value. EOF returns -1.
*/
INT32 rdhex(FILE *fp)
{  INT32 w = 0;
   int ch = fgetc(fp);

   while(ch==' ' || ch=='\n' || ch=='\r') ch = fgetc(fp);

   if (ch=='#') { /* remove comments from object modules */
                  while (ch != '\n' && ch != EOF) ch = fgetc(fp);
                  return rdhex(fp);
                }

   for(;;)
   {  int d = 100;
      if('0'<=ch && ch<='9') d = ch-'0';
      if('A'<=ch && ch<='F') d = ch-'A'+10;
      if('a'<=ch && ch<='f') d = ch-'a'+10;
      //if(d==100)   printf("rdhex:  w = %8x\n", w);
		 
      if(d==100) return ch==EOF ? -1 : w;
      w = (w<<4) | d;
      ch = fgetc(fp);
   }		 
}		 
		 
INT32 globin(INT32 segl, INT32 g)
{ INT32  a = segl, globsize = W[g];
//printf("globin segl = %6ld  g = %6ld\n", segl, g); 
  while (a) { INT32 base = (a+1)<<2;
              INT32 i = a + W[a+1];
              if (W[i]>globsize) return 0;
	      //printf("globin:  base %6ld  to %6ld  size: %5ld\n", a+1, i, W[a+1]); 
              for(;;) { i -= 2;
                        if (W[i+1]==0) break;
                        W[g+W[i]] = base + W[i+1];
			//printf("globin:  g[%3ld] = %6ld\n", W[i], base+W[i+1]); 
                      }
              a = W[a];
            }
  return segl;
}

INT32 getvec(INT32 requpb)
{ INT32 upb = requpb+4+4;  // Allocate 4 words at the end for safety
                           // plus 4 words of task name.
  INT32 p;
  INT32 q = 0; /* the start of the block list */
  INT32 n = (upb+1+1+1) & 0xFFFFFFFE;  /* Add 1 word for size and alloc bit
                                          and 1 word for word zero and
                                          then round up to an even size */
  
  do
  { p = q;
    for(;;) { INT32 size = W[p];
              if((size&1) != 0) break;
              if( size == 0)    return 0;
              p += size;
            }
    q = p;  /* find next used block */
    for(;;) { INT32 size = W[q];
              if((size&1) == 0) break;
              q += size-1;
            }
  } while(q-p<n);
  
  if(p+n!=q) W[p+n] = q-p-n+1;
  W[p] = n;
  // The following is intended to improve the safety of memory allocation
  // by adding checkable redundancy
  W[p+n-9] = 0xCCCCCCCC;  // Funny pattern for possible roundup word
  W[p+n-8] = 0x55555555;  // Special patterns
  W[p+n-7] = 0xAAAAAAAA;
  W[p+n-6] = requpb;      // The requested upb

  W[p+n-5] = taskname[0]; // The taskname, if any
  W[p+n-4] = taskname[1]; // The taskname, if any
  W[p+n-3] = taskname[2]; // The taskname, if any
  W[p+n-2] = taskname[3]; // The taskname, if any

  W[p+n-1] = p;           // Pointer to base of this memory block
  //printf("getvec: allocating block %6d  upb %4d\n", p, upb);

  if(requpb>vecstatsvupb) requpb = vecstatsvupb;
  W[vecstatsvec+requpb]++;
  return p+1;
}

INT32 freevec(INT32 p)
{ INT32 res = -1;  // =TRUE
  INT32 n, upb;

  if(p==0) return res;

  p--;       // All getvec'ed blocks start on odd addresses
  n = W[p];

  if(n & 1) {
    printf("\n#### freevec: block at %ld already free\n", p);
    return 0;
  }

//printf("#### freevec: Freeing block at %ld\n", p);

  if(W[p+n-1]!=p ||
     W[p+n-7]!=0xAAAAAAAA ||
     W[p+n-8]!=0x55555555) {
       printf("\n#### freevec: block at %ld size %ld corrupted", p, n);
       printf("\n#### freevec: last 4 words %8x %8x %6d %7d",
              W[p+n-8],W[p+n-7],W[p+n-6],W[p+n-1]);
       printf("\n#### freevec: should be    %8x %8x requpb %7d\n\n",
              0x55555555, 0xAAAAAAAA, p);
       res = 0;
  }
  W[p] |= 1;
  // Deal with getvec allocation statistics
  upb = W[p+n-6];       // The requested size
  if(upb>vecstatsvupb) upb = vecstatsvupb;
  W[vecstatsvec+upb]--; // Decrement count of block of this size
  return res;
}

INT32 muldiv(INT32 a, INT32 b, INT32 c)
{ unsigned INT32 q=0, r=0, qn, rn;
  unsigned INT32 ua, ub, uc;
  int qneg=0, rneg=0;
  if(c==0) c=1;
  if(a<0) { qneg=!qneg; rneg=!rneg; ua = -a; }
  else                              ua =  a;
  if(b<0) { qneg=!qneg; rneg=!rneg; ub = -b; }
  else                              ub =  b;
  if(c<0) { qneg=!qneg;             uc = -c; }
  else                              uc =  c;
  
  qn = ub / uc;
  rn = ub % uc;
  
  while(ua)
  { if(ua&1) { q += qn;
               r += rn;
               if(r>=uc) { q++; r -= uc; }
             }
    ua >>= 1;
    qn <<= 1;
    rn <<= 1;
    if(rn>=uc) {qn++; rn -= uc; }
  }
  result2 = rneg ? -(INT32)r : r;
  return qneg ? -(INT32)q : q;
}

static char chbuf[256], chbuf2[256]; /* to hold filenames */

int relfilename(INT32 name)
{ char *bp = BP(&W[name]);
  if(bp[1]=='/' || bp[1]=='\\' ||
     /* The following is fiddle for MSDOS/Windows */
     FILE_SEP_CH=='\\' && 'A'<=bp[1] && bp[1]<='Z' && bp[2]==':')
       return 0; /* Absolute file names don't use paths */
  return 1; 
}

FILE *pathinput(INT32 name, char *pathname)
{ FILE *fp = 0;
  // Convert the BCPL filename to a C string prefixed by prefix if relative
  b2c_fname(name, chbuf);
  //printf("pathinput applied to %s pathname='%s'\n", chbuf, pathname);
  // Look through the PATH directories first
  if (pathname)
  { char filename[256];
    int itemsep = FILE_SEP_CH=='/' ? ':' : ';';
    //printf("pathinput file %s not found\n", chbuf);
    if (pathname && relfilename(name))
    { char *path = getenv(pathname);
      //printf("pathinput: searching directories %s\n", path);
      // Try prefixing with each directory in the path.
      while(path && fp==0)
      { char *f=filename;
        char *n=BP (&W[name]);
        int len = *n++;
        while(*path==itemsep) path++;
        if(*path==0) break;
        while(*path!=0 && *path!=itemsep)
        { char ch = *path++;
          if(ch==':' || ch=='/' || ch=='\\') ch = FILE_SEP_CH;
          *f++ = ch;
        }
        if(f[-1]!=FILE_SEP_CH) *f++ = FILE_SEP_CH;
        while(len--)
        { char ch = *n++;
          if(ch==':' || ch=='/' || ch=='\\') ch = FILE_SEP_CH;
          *f++ = ch;
        }
        *f = 0;
        //printfs("pathinput: trying filename = %s\n", filename);
        fp = fopen(filename, "rb");
      }
    }
  }

  if(fp==0) {
     //printf("Now try looking up %s in the current directory\n", chbuf);
     // Look through the current directory if necessary
     fp = fopen(chbuf, "rb");
  }
  //if(fp==0) printf("pathinput: failed to find %s anywhere\n", chbuf);
  //else      printf("pathinput: success\n");
  return fp;
}

INT32 dosys(register INT32 p, register INT32 g)
{ register INT32 i;
  //printf("dosys(%d, %d) P3=%d P4=%d\n", p, g, (long)W[p+3], (long)W[p+4]);
  switch((int)(W[p+3]))
  { default: PRINTFD("\nBad sys number: %ld\n", (long)W[p+3]);
             return W[p+3];

    // case Sys_setcount: set count               -- done in cinterp
    // case Sys_quit:     return from interpreter -- done in cinterp

    // case Sys_rti:      sys(Sys_rti, regs)      -- done in cinterp
    // case Sys_saveregs: sys(Sys_saveregs, regs) -- done in cinterp
    // case Sys_setst:    sys(Sys_setst, st)      -- done in cinterp
    case Sys_tracing:  // sys(Sys_tracing, b)
            tracing = W[p+4];
            return 0;
    // case Sys_watch:    sys(Sys_watch, addr)    -- done in cinterp

     case  Sys_tally:         /* sys(Sys_tally, flag)     */
             if(W[p+4]) {
                tallylim = tallyupb;
                for(i=1; i<=tallylim; i++) tallyv[i] = 0;
              } else {
                tallylim = 0;
              }
              return 0;
     
     case Sys_interpret: /* call interpreter (recursively) */
            { INT32 regsv = W[p+4];
              if(W[regsv+7]<0) return CINTASM  (regsv, W);
              return interpret(regsv, W);
            }

     case Sys_sardch:
              { int ch = read_buf_ch(); /* get the keyboard character  */
		//printf("Sys_sardch: ch = %d\n", ch);
                if (ch>=0) putchar(ch);
                if(ch==13) { ch = 10; putchar(10); }
                fflush(stdout);
		//printf("Sys_sardch returning ch = %d\n", ch);
                return ch;
              }

     case Sys_sawrch:
#ifdef forCYGWIN32
              if(W[p+4] == 10) putchar(13);
#endif
              putchar((char)W[p+4]);
              fflush(stdout);
              return 0;

     case Sys_read:  // bytesread := sys(Sys_read, fp, buf, bytecount)
              { FILE *fp = findfp(W[p+4]);
                INT32 bbuf = W[p+5]<<2;
                INT32 len   = (int) W[p+6];
                clearerr(fp); /* clear eof and error flag */
                len = fread(&(BP W)[bbuf], (size_t)1, (size_t)len, fp);
                if(ferror(fp)) { perror("sys_read");
                                 return -1; /* check for errors */
		}
                return len;
              }

     case Sys_write:
              { FILE *fp = findfp(W[p+4]);
                INT32 bbuf = W[p+5]<<2;
                INT32 len = W[p+6];
                //fseek(fp, 0L, SEEK_CUR);  // Why?? MR 9/7/04
                len = WD fwrite(&(BP W)[bbuf], (size_t)1, (size_t)len, fp);
                fflush(fp);
                return len;
              }

     case Sys_openread:
              { FILE *fp = pathinput(W[p+4], b2c_str(W[p+5], chbuf2));
                if(fp==0) return 0L;
                return newfno(fp);
              }

     case Sys_openwrite:
              { FILE *fp = fopen(b2c_fname(W[p+4], chbuf), "wb");
                if(fp==0) return 0L;
                return newfno(fp);
              }

     case Sys_openreadwrite:
              { FILE *fp = fopen(b2c_fname(W[p+4], chbuf), "rb+");
                if(fp==0) fp = fopen(b2c_fname(W[p+4], chbuf), "wb+");
                if(fp==0) return 0L;
                return newfno(fp);
              }

     case Sys_close:
              { INT32 res = fclose(findfp(W[p+4]));
                freefno(W[p+4]);
                return res==0 ? -1 : 0; // res=0 means success
              }

     case Sys_deletefile:
                return ! REMOVE(b2c_fname(W[p+4], chbuf));

     case Sys_renamefile:
                REMOVE(b2c_fname(W[p+5], chbuf2));
                return ! rename(b2c_fname(W[p+4], chbuf), chbuf2);

     case Sys_getvec:
              { INT32 tcb = W[rootnode+Rtn_crntask];
                INT32 *tn = &W[tcb+Tcb_namebase];
	        taskname[0] = (tcb==0) ? 0 : tn[0];
	        taskname[1] = (tcb==0) ? 0 : tn[1];
	        taskname[2] = (tcb==0) ? 0 : tn[2];
	        taskname[3] = (tcb==0) ? 0 : tn[3];
                return getvec(W[p+4]);
	      }

     case Sys_freevec:
                return freevec(W[p+4]);

     case Sys_loadseg:
              { INT32 tcb = W[rootnode+Rtn_crntask];
                INT32 *tn = &W[tcb+Tcb_namebase];
	        taskname[0] = (tcb==0) ? 0 : tn[0];
	        taskname[1] = (tcb==0) ? 0 : tn[1];
	        taskname[2] = (tcb==0) ? 0 : tn[2];
	        taskname[3] = (tcb==0) ? 0 : tn[3];
                return loadseg(W[p+4]);
	      }

     case Sys_globin:
                return globin(W[p+4], g);

     case Sys_unloadseg:
                unloadseg(W[p+4]);                    return 0;

     case Sys_muldiv:
              { INT32 res =  muldiv(W[p+4], W[p+5], W[p+6]);
                W[g+Gn_result2] = result2;
                return res;
              }

     case Sys_intflag:
                return intflag() ? -1L : 0L;

     case Sys_setraster:
                return setraster(W[p+4], W[p+5]);

     case Sys_cputime: /* Return CPU time in milliseconds  */
              return muldiv(clock(), 1000, TICKS_PER_SEC);

     case Sys_filemodtime: /* Return time of last modification of file
                              whose name is in p[4]  */
              { struct stat buf;
                if (stat(b2c_fname(W[p+4], chbuf), &buf)) return 0;
                return buf.st_mtime;
              }

     case Sys_setprefix: /* Set the file prefix string  */
              prefix = W[p+4];
              return prefix;

     case Sys_getprefix: /* Return the file prefix string  */
              return prefix;

     case Sys_graphics: /* Perform an operation on the graphics window  */
              return sysGraphics(p);

     case 35:   return 0; // Spare

     case 36:   return 0; // Spare

     case 37:   return 0; // Spare

     case Sys_seek:  /* res := seek(fd, pos)   */
              { FILE *fp = findfp(W[p+4]);
                INT32 res = fseek(fp, (long)W[p+5], SEEK_SET);
                W[g+Gn_result2] = errno;
		//printf("fseek pos=%d => res=%d errno=%d\n",
                //       (long)W[p+5],res, errno);
                return res==0 ? -1 : 0; /* res=0 succ, res=-1 error  */
	      }

     case Sys_tell: /* pos := sys(Sys_tell,fd)  */
              { FILE *fp = findfp(W[p+4]);
                INT32 pos = ftell(fp);
                W[g+Gn_result2] = errno;
                return pos; // >=0 succ, -1=error */
	      }

     case Sys_waitirq: /* Wait for irq */
                pthread_mutex_lock  (         &irq_mutex);
                pthread_cond_wait   (&irq_cv, &irq_mutex);
                pthread_mutex_unlock(         &irq_mutex);
                return 0;

     case Sys_lockirq:   // Stop all devices from modifying
                         // packets or generating interrupts
                pthread_mutex_lock  (         &irq_mutex);
                return 0;

     case Sys_unlockirq: // Allow devices to modify packets
                         // and generate interrput
                pthread_mutex_unlock(         &irq_mutex);
                return 0;

     case Sys_devcom: /* res := sys(Sys_devcom, com, arg) */
                return devcommand(W[p+4], W[p+5], W[p+6]);

     case Sys_ftime: /* return result of calling ftime */
              { struct timeb tb;
                INT32 *v = &W[W[p+4]];
                ftime(&tb);

                // **************** BEWARE ************************
                // The date will OVERFLOW on 19-Jan-2038 at 3:14:07
                v[0] = 0; //(INT32)(tb.time>>32);
                v[1] = (INT32)tb.time; // Seconds since epoch
                v[2] = tb.millitm;     // milli-seconds
                v[3] = tb.timezone;    // Minutes west of Greenwich
                v[4] = tb.dstflag;     // non zero => Daylight saving time applies

                daylight = 1;          // Fudge for windows
                daylight = 0;          // Fudge for windows MR 31/10/03
                tzset(); // Should be done separately
		//printf("cintpos: timezone=%d daylight=%d %s %s\n",
                //        (INT32)timezone,(INT32)daylight, tzname[0],
		//        tzname[1]);
                if(((INT32)timezone)%3600==0) // Fudge for windows
                  v[1] -= (INT32)timezone;    // Correct for timezone
		if (daylight) v[1] += 60*60;  // Add one hour in DST
                v[1] += W[rootnode+Rtn_adjclock] * 60; // Add adjustment
                return -1;   // To indicate success
              }

#ifndef forWIN32
     case Sys_usleep: /* usleep for some micro-seconds */
                return usleep(W[p+4]);
#endif

     case Sys_filesize:  /* res := sys(Sys_filesize, fd)   */
              { FILE *fp   = findfp(W[p+4]);
                INT32 pos  = ftell(fp);
                INT32 rc   = fseek(fp, 0, SEEK_END);
                INT32 size = ftell(fp);
                rc  = fseek(fp, pos, SEEK_SET);
                if (rc) size = -1;
                return size; /* >=0 succ, -1=error  */
	      }

    case Sys_getsysval: /* res := sys(Sys_getsysval, addr) */
              { INT32 addr = W[p+4];
                return W[addr];
              }

    case Sys_putsysval: /* res := sys(Sys_putsysval, addr, val) */
              { INT32 addr = W[p+4];
                W[addr] = W[p+5];
                return 0;
              }

    case Sys_shellcom: /* res := sys(Sys_shellcom, comstr) */
              { INT32 comstr = W[p+4]<<2;
	        int i;
                char com[256];
                int len = ((char *)W)[comstr];
                for(i=0; i<len; i++) com[i] = ((char *)W)[comstr+i+1];
                com[len] = 0;
		//printf("\ncintpos: calling shell command %s\n", com);
                return system(com);
              }

    case Sys_getpid: /* res := sys(Sys_getpid) */
                return getpid();

    case Sys_dumpmem: /* sys(Sys_dumpmem, context) */
                dumpmem(W, memupb, W[p+4]);
                printf("\nCintpos memory dumped to DUMP.mem, context=%d\n",
                       W[p+4]);
                return 0;

    case Sys_callnative: /* res := sys(Sys_callnative, f, a1, a2, a3) */
              { // Call native code.
                INT32(*f)(INT32, INT32, INT32) =
		  (INT32(*)(INT32,INT32,INT32))&W[W[p+4]];
                return f(W[p+5],W[p+6],W[p+7]);
              }

    case Sys_platform:
              { // Return platform code, 0 if unknown
		INT32 res = 0;
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
#ifdef forLINUX
		res = 5;
#endif
#ifdef forLINUXamd64
		res = 6;
#endif
#ifdef forCYGWIN32
		res = 7;
#endif
#ifdef forLINUXPPC
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

    case 135: { /* Return system date and time in VEC 5 */
                time_t clk = time(0);
	        struct tm *now = gmtime(&clk);
	        INT32 *arg = &W[W[p+4]];
                arg[0] = now->tm_year+1900;
	        arg[1] = now->tm_mon+1;
	        arg[2] = now->tm_mday;
	        arg[3] = now->tm_hour;
	        arg[4] = now->tm_min;
	        arg[5] = now->tm_sec;
		printf("%d %d %d %d %d %d\n",
                        arg[0],arg[1],arg[2],arg[3],arg[4],arg[5]);
                return 0;
              }

  }
} 

/* b2c_fname converts the BCPL string for a file name to a C character
** string.  The character '/' (or '\') is treated as a separator and is
** converted to FILE_SEP_CH ('/' for unix, '\' for MSDOS or ':' for MAC).
** If prefix is set and the filename is relative, the prefix is prepended.
*/
char *b2c_fname(INT32 bstr, char * cstr)
{  INT32 bp = bstr<<2;
   int len = (BP W)[bp++];
   int i=0;
   if (bstr==0) return 0;
   if (prefix && relfilename(bstr))
   { // Prepend the filename with prefix
     INT32 pfxp = prefix<<2;
     int pfxlen = (BP W)[pfxp++];
     while(pfxlen--)
     { char ch = (BP W)[pfxp++];
       if(ch=='/' || ch=='\\' || ch==':') ch = FILE_SEP_CH;
       cstr[i++] = ch;
     }
     if (cstr[i-1] != FILE_SEP_CH) cstr[i++] = FILE_SEP_CH;
   }

   while (len--)
   { char ch = (BP W)[bp++];
     if(ch=='/' || ch=='\\' || ch==':') ch = FILE_SEP_CH;
     cstr[i++] = ch;
   }
   cstr[i] = 0;
   //if (prefix) { printfs("filename = %s\n", cstr); busywait(2000); }
   return cstr;
}

/* b2c_str converts a BCPL string to a C character string. */
char *b2c_str(INT32 bstr, char * cstr)
{  INT32 bp = bstr<<2;
   char *p = cstr;
   int len = (BP W)[bp++];
   if (bstr==0) return 0;
   while (len--) *p++ = (BP W)[bp++];
   *p = 0;
   return cstr;
}

/* syscin2b_str converts a syscin filename string to a BCPL string. */
INT32 syscin2b_str(char *cstr, INT32 bstr)
{  char *prfx = "syscin/";    // System cin directory
   char *bp = &(BP W)[bstr<<2];
   int len = 0;
   int i = 0;
   while (prfx[i]) { bp[++len] = prfx[i++]; }
   i = 0;
   while (cstr[i]) { bp[++len] = cstr[i++]; }
   bp[0] = len;
   return bstr;
}

/* c2b_str converts the C string to a BCPL string. */
INT32 c2b_str(char *cstr, INT32 bstr)
{  char *bp = &(BP W)[bstr<<2];
   int len = 0;
   while (cstr[len]) { bp[len+1] = cstr[len]; len++; }
   bp[0] = len;
   return bstr;
}

void wrcode(char *form, INT32 f, INT32 a)
{  wrfcode(f);
   PRINTFD("  ", 0);
   PRINTFD(form, (long)a);
   PRINTFD("\n", 0);
} 

void wrfcode(INT32 f)
{ char *s;
  int i, n = (f>>5) & 7;
  switch((int)f&31)
  { default:
    case  0: s = "     -     K   LLP     L    LP    SP    AP     A"; break;
    case  1: s = "     -    KH  LLPH    LH   LPH   SPH   APH    AH"; break;
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
    case 15: s = "   LM1   L2G  L2G1  L2GH  LP15  SP15   BTC     -"; break;
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
    case 30: s = "  JEQ0  JNE0  JLS0  JGR0  JLE0  JGE0 ST1P4     -"; break;
    case 31: s = " JEQ0$ JNE0$ JLS0$ JGR0$ JLE0$ JGE0$     -     -"; break;
  }

  for(i = 6*n; i<=6*n+5; i++) putchar(s[i]);
} 

void trace(INT32 pc, INT32 p, INT32 a, INT32 b)
{ PRINTFD("%9ld: ", (long)pc);
  PRINTFD("A=%9ld  ", (long)a);
  PRINTFD("B=%9ld    ", (long)b);
  PRINTFD("P=%5ld ", (long)p);
  wrcode("(%3ld)", WD (BP W)[pc], WD (BP W)[pc+1]);
  putchar(13);
}

void dumpmem(INT32 *mem, INT32 upb, INT32 context)
{ FILE *fp;
  INT32 count = upb;
  INT32 p=0, q, r=0;
  int len = 0;

  fp = fopen("DUMP.mem", "wb");
  if(fp==0) goto bad;

  mem[rootnode + Rtn_lastp]   = lastWp-W;
  mem[rootnode + Rtn_lastg]   = lastWg-W;
  mem[rootnode + Rtn_lastst]  = lastst;

  mem[rootnode + Rtn_context] = context;
  // context = 1   SIGINT received                 (lastp, lastg)
  // context = 2   SIGSEGV received                (lastp, lastg)
  // context = 3   fault while running BOOT        (bootregs)
  // context = 4   user called sys(Sys_quit, -2)   (klibregs)
  // context = 5   fault while user running        (klibregs)

  // Write out size of cintcode memory
  //printf("\n%8d Memory upb\n", upb);
  len = fwrite((char*)&count, (size_t)4, (size_t)1, fp);
  if(len!=1) goto bad;

  while(r<=upb)
  { INT32 dataword = mem[r];
    q = r;
    while(r<=upb && mem[++r]==dataword) continue;
    if(r<=upb && r-q < 200) continue;

    // mem[p]..mem[q-1] is the block
    // mem[q]..mem[r-1] are repeated occurrences of dataword
    // Write out count of words in next block
    count = q-p; // count>=0
    if(count) {
      //printf("%8d BLOCK\n", count);
      len = fwrite((char*)&count, (size_t)4, (size_t)1, fp);
      if(len!=1) goto bad;
      len = fwrite((char*)&mem[p], (size_t)4, (size_t)count, fp);
      if(len!=count) goto bad;
    }

    // Write out repetition count (negated) followed by the data word
    count = q-r; // count<=0
    if(count) {
      //printf("%8d x %8X\n", -count, dataword);
      len = fwrite((char*)&count,    (size_t)4, (size_t)1, fp);
      if(len!=1) goto bad;
      len = fwrite((char*)&dataword, (size_t)4, (size_t)1, fp);
      if(len!=1) goto bad;
    }

    p = r;
  }

 bad:
  //ret:
  if(fp) fclose(fp);
  //PRINTFD("\nCintpos memory dumped to DUMP.mem, context=%d\n", context);
  return; 
}
