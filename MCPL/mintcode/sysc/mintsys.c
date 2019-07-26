/*
** This is a 32 bit MINTCODE interpreter written in C designed
** to run on most machines with Unix-like C libraries.
**
** In this implementation MCPL pointers are offsets in bytes relative
** to the base of the Mintcode memory vector (B). On some implementations
** the Mintcode memory vector is at zero and the byte offset is a
** machine address.
**
** Sizes of structures that are normally measured in 32 bit words rather
** than bytes. In particular this applies for the root node,
** global vectors, blocks in the block list, code segments and the 
** mintcode memory itself.
**
** (c) Copyright:  Martin Richards  1 November 2006
**
*/

/* 
1/11/06
Added the options -c -s and -- to be similar to those in the BCPL Cintcode
system.

25/9/01 Made changes so that it can now run under Window 95/98/NT.
7/12/95 Incorporate the recent changes made to cintmain.c
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

/* minterp.h contains machine/system dependent #defines  */
#include "minterp.h"

/* MEMUPB and TALLYUPB are in words, 
   TALLYV is a byte offset
*/
#define MEMUPB   4000000L
#define TALLYUPB  500000L
#define TALLYV ((MEMUPB+1)<<2)

FILE *logfp = NULL;

/* Functions defined in kblib.c  */
extern int Readch(void);
extern int init_keyb(void);
extern int close_keyb(void);
extern int intflag(void);

/* Functions defined in mrastlib.c (or mnullrastlib.c)  */
extern INT32 setraster(INT32 n, INT32 val);

#define Stackupb    5000L
#define Globupb     1000L

#define Rtn_membase   0L
#define Rtn_memsize   1L
#define Rtn_blklist   2L
#define Rtn_tallyv    3L
#define Rtn_msyslib   4L
#define Rtn_mlib      5L
#define Rtn_mboot     6L
#define Rtn_upb      20L

unsigned char *B;  /* This will hold the pointer to the Mintcode memory */

INT32 rootregs,
      rootnode,
      stackbase,
      globbase,
      result2;

int tracing = 0;
int filetracing = 0;

INT32 memupb;
INT32 tallyupb, tallyvec, *tallyv, tallylim=0;

INT32 loadseg(char *fp);
void unloadseg(INT32 segl);
INT32 rdhex(FILE *fp);
INT32 globin(INT32 segl, INT32 g);
INT32 getvec(INT32 upb);
void freevec(INT32 p);
INT32 muldiv(INT32 a, INT32 b, INT32 c);
FILE *pathinput(char *name, char *pathname);
INT32 dosys(INT32 *p, INT32 *g);
char *file_str(char *str, char *cstr);
void wrcode(char *form, INT32 f, INT32 a); 
void wrfcode(INT32 f);
void trace(INT32 pc, INT32 p, INT32 a, INT32 b);

extern int mintasm(INT32 regs, unsigned char *mem);
extern int interpret(INT32 regs, unsigned char *mem);

#define Globword  0xEFEF0000L

#define Gn_globsize    0
#define Gn_start       1
#define Gn_sys         3
#define Gn_currco      7
#define Gn_colist      8
#define Gn_rootnode    9
#define Gn_result2    10
#define Gn_cli_returncode    137

/* relocatable object blocks  */
#define T_hunk  2000L
#define T_reloc 2001L
#define T_end   2002L

int badimplementation(void)
{ int bad = 0, A='A';
  SIGNEDCHAR c = 255;
  if(sizeof(INT32)!=4 || A!=65) bad = 1;
  if (c/-1 != 1) { printf("There is a problem with SIGNEDCHAR\n");
                   bad = 1;
                 }
  return bad;
}

/* The following four functions are necessary since the type FILE*
** is too large for a MCPL word on some machines (such as the DEC Alpha)
*/

#ifdef ALPHAyes
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

void (*old_handler)(int);

void handler(int sig)
{ 
  printf("SIGINT received\n");
  old_handler = signal(SIGINT, old_handler);
  close_keyb();
  exit(20);
}

char *inbuf = NULL;	// Buffer for input to interpreter
int reattach_stdin = 0;	// FALSE, if true switch to stdin after inbuf is empty

/* Replacement for Readch() when taking stdin from a command line parameter */
int inbuf_next()
{
  static int idx = 0;
  int c = inbuf[idx];
  if (c) {
    idx++;
    return c;
  }
  else return EOF; // -1
}

/* Set up a stream of characters to be prepended to the standard input,
 * for the cli to read before those in the normal input stream.
 * If leading_string is provided, it is prepended to stream of characters.
 * This is used to support executable cintcode files and CLI scripts
 * on Unix systems. The leading_string feature permits support of scripts
 * by running the "c" command followed by its command line arguments.
 */
void prepend_stdin(char *leading_string, int argc, char* argv[], int argvpos) {
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

  //PRINTFS("\nPrepended string:\n%s\n", inbuf);
}

int main(int argc, char* argv[])
{ INT32 i;      /* for FOR loops  */
  INT32 res;    /* result of interpret  */
  INT32 *W=0;

  memupb   = 4000000L;
  tallyupb = 1000000L;

  //for (i=0; i<argc; i++) printf("%2d: %s\n", i, argv[i]);

  for (i=1; i<argc; i++) {

    if (strcmp(argv[i], "-m")==0) {
      memupb   = atoi(argv[++i]);
      continue;
    }

    if (strcmp(argv[i], "-t")==0) {
      tallyupb = atoi(argv[++i]);
      continue;
    }

    if (strcmp(argv[i], "-d")==0) {
      filetracing = 1;
      continue;
    }

    if (strcmp(argv[i], "-s")==0) {
      // Enter cintsys (or cintpos) giving its CLI the name of the current
      // input file as the CLI command to run.
      // Used to enable executable cli scripts on Unix.
      // Do not advance argument pointer, keep file name in remaining arg list.
      // EG: Make and executable file test1 containing something like:

      // #!/home/mr/distribution/BCPL/cintcode/cintsys -s
      // bcpl com/bcpl.b to junk
      // echo "*nCompilation done*n"

      // and run it by typing the Unix command: test1

      char *fname = argv[i + 1];
      if (!fname) {
        fprintf(stderr, "missing parameter for -s\n");
        exit(-1);
      }
      prepend_stdin("c", argc, argv, i);
      break;
    }

    if (strcmp(argv[i], "--")== 0) {
      // Like -c but reattach standard input after exhausting command
      // line characters.
      reattach_stdin = 1; // TRUE
    }

    if (strcmp(argv[i], "-c")== 0 || strcmp(argv[i], "--")== 0) {
      // Allocate a buffer to hold remaining args as a string and pass
      // remainder of command line to the interpreter.

      // Typical usage:

      // cintsys -c bcpl com/bcpl.b to junk

      prepend_stdin(NULL, argc, argv, i);
      break;
    }

    if (strcmp(argv[i], "-h")!=0) PRINTF("Unknown command option: %s\n", argv[i]);

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
    PRINTF("Bad -m or -t size\n");
    return 10;
  }

  if(badimplementation())
  { printf("This implementation of C is not suitable\n");
    return 0;
  }

   old_handler = signal(SIGINT, handler);

   initfpvec();

   B = BP MALLOC(MEMUPB+TALLYUPB+3); /* arg is in words */

   if(B==NULL)
   { printf("Insufficient memory for memvec\n");
     return 0;
   }
   B = BP(((long) B + 3L) & -4L);  /* round to an INT32 boundary */
   W = PT B;

   /* Sizes in the block list are given in INT32 units */
   W[0] = MEMUPB+1;  /* Initialise heap space */
   W[MEMUPB] = 0;
   
   tallylim = 0;
   tallyvec = MEMUPB+1;
   tallyv = &W[tallyvec];
   tallyv[0] = TALLYUPB;
   for(i=1; i<=TALLYUPB; i++) tallyv[i] = 0;

   rootregs  = getvec(9L);
   rootnode  = getvec(Rtn_upb);
   stackbase = getvec(Stackupb);
   globbase  = getvec(Globupb);
   result2   = 0L;

   for(i = 0; i<=Globupb; i++) (PT (B+globbase))[i] = Globword+i;
   (PT (B+globbase))[Gn_globsize] = Globupb;
   (PT (B+globbase))[Gn_rootnode] = rootnode;
   (PT (B+rootnode))[Rtn_membase] = 0;
   (PT (B+rootnode))[Rtn_memsize] = MEMUPB;
   (PT (B+rootnode))[Rtn_blklist] = 0;
   (PT (B+rootnode))[Rtn_tallyv]  = TALLYV;
   (PT (B+rootnode))[Rtn_msyslib] = globin(loadseg("sysmin/MSYSLIB"),
                                           globbase);
   if((PT (B+rootnode))[Rtn_msyslib]==0)
     printf("Can't load: min/sysmin/MSYSLIB");
   (PT (B+rootnode))[Rtn_mlib]    = globin(loadseg("sysmin/MLIB"),
                                           globbase);
   if((PT (B+rootnode))[Rtn_mlib]==0)
     printf("Can't load: min/sysmin/MLIB");
   (PT (B+rootnode))[Rtn_mboot]   = globin(loadseg("sysmin/MBOOT"),
                                           globbase);
   if((PT (B+rootnode))[Rtn_mboot]==0)
     printf("Can't load: min/sysmin/MBOOT");

   for(i=0; i<=Stackupb; i++) (PT (B+stackbase))[i] = 0;
    
   (PT (B+rootregs))[0] = 0;                    /* A          */
   (PT (B+rootregs))[1] = 0;                    /* B          */
   (PT (B+rootregs))[2] = 0;                    /* C          */
   (PT (B+rootregs))[3] = stackbase;            /* P          */
   (PT (B+rootregs))[4] = globbase;             /* G          */
   (PT (B+rootregs))[5] = 0;                    /* ST         */
   (PT (B+rootregs))[6] = (PT (B+globbase))[1]; /* PC = start */
   (PT (B+rootregs))[7] = -1;                   /* Count      */
   (PT (B+rootregs))[8] = 0;                    /* H          */

   init_keyb();
   res = interpret(rootregs, B);
   close_keyb();

   if (res) printf("\nExecution finished, return code %ld\n", (long)res);

   return res;
} 

INT32 loadseg(char *filename)
{ INT32 list  = 0;
  INT32 liste = 0;
  INT32 base  = 0;
  
  FILE *fp = pathinput(filename, "MCPLPATH");

  //if(fp==NULL) printf("Can't find file %s in MCPLPATH*n", filename);

  if(fp==NULL) return 0;
  for(;;)
  { INT32 type = rdhex(fp);

    switch((int)type)
    { default:
          err:   unloadseg(list);
                 list = 0;
      case -1:   fclose(fp);
                 return list;
		 
      case T_hunk:
               {  INT32 i, *hunk;
                  INT32 n=rdhex(fp); /* hunk size in INT32 units */
                  base = getvec(n);  /* Relative byte address */
                  if(base==0) goto err;
                  hunk = PT (B+base);
                  hunk[0] = 0;
                  for(i = 1; i<=n; i++) hunk[i] = rdhex(fp);
                  if(list==0) list=base;
                  else (PT (B+liste))[0] = base;
                  liste = base;
                  continue;
               }
                

      case T_reloc:
               {  INT32 i, n=rdhex(fp);
                  if(base==0) goto err;
                  for(i = 1; i<=n; i++)
                  { INT32 offset = rdhex(fp); /* Relative byte address */
                    (PT (B+base+4+offset))[0] += base+4;
                  }
                  continue;
                }

      case T_end:;
    }
  }
} 

void unloadseg(INT32 segl) /* segl is a relative byte address */
{ while(segl) { INT32 s = (PT (B+segl))[0];
                freevec(segl);
                segl = s;
              }
}

INT32 rdhex(FILE *fp)
{  INT32 w = 0;
   int ch = fgetc(fp);

   while(ch==' ' || ch=='\n') ch = fgetc(fp);

   if (ch=='#') { /* remove comments from object modules */
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

INT32 globin(INT32 segl, INT32 g)
{ INT32  a = segl, *gvec = PT (B+g);
  INT32 globsize = gvec[Gn_globsize];
 
  while (a) { INT32 base = a+4;       /* Relative byte address */
              INT32 size = (PT (B+base))[0]; /* in INT32 units */
              INT32 *p = PT (B+a) + size;
              if (*p-- > globsize) return 0;
              for(;;) { INT32 offset = *p--;
                        INT32 gn = *p--;
                        if (offset==0) break;
                        gvec[gn] = base+offset;
                      }
              a = (PT (B+a))[0];
            }
  return segl;
}

INT32 getvec(INT32 upb)
{ INT32 *W = PT B;   /* treat memory as a vector of words   */
  INT32 p;           /* using p and q as integer subscripts */
  INT32 q = 0;       /* The block list starts at 0          */
  INT32 n = (upb+3) & 0xFFFFFFFE; /* round up to even size, INT32 units */

  do
  { p = q;
    for(;;) { INT32 size = W[p];
              if((size&1) != 0) break;    /* the block is free */
              if( size    == 0) return 0; /* end of block list */
              p += size;
            }
    q = p;  /* find next used block */
    for(;;) { INT32 size = W[q];
              if((size&1) == 0) break;  /* the block is in use */
              q += size-1;
            }
    /* There is a free block from p to q-1 inclusive */
  } while(q-p<n); /* repeat if it is too small */
  
  /* the block is large enough */
  if (p+n != q) W[p+n] = q-p-n+1;
  W[p] = n; /* fill in current block size and mark in use */

  return (p+1)<<2;
}

void freevec(INT32 p)
{ (PT (B+p))[-1] |= 1; /* mark block as free */
}

INT32 muldiv(INT32 a, INT32 b, INT32 c)
{ INT32 q=0, r=0, qn, rn;
  int qneg=0, rneg=0;
  if (c==0) c=1;
  if (a<0) { qneg=!qneg; rneg=!rneg; a = -a; }
  if (b<0) { qneg=!qneg; rneg=!rneg; b = -b; }
  if (c<0) { qneg=!qneg;             c = -c; }
  
  qn = b / c;
  rn = b % c;
  
  while(a)
  { if (a&1) { q += qn;
               r += rn;
               if(r>=c) { q++; r -= c; }
             }
    a  >>= 1;
    qn <<= 1;
    rn <<= 1;
    if (rn>=c) {qn++; rn -= c; }
  }
  result2 = rneg ? -r : r;
  return qneg ? -q : q;
}

static char chbuf[256], chbuf2[256]; /* to hold filenames */

int relfilename(char *name)
{ if(name[0]==FILE_SEP_CH ||
     /* The following is fiddle for MSDOS/Windows */
     FILE_SEP_CH=='\\' && 'A'<=name[0] && name[0]<='Z' && name[1]==':')
       return 0; /* Absolute file names don't use paths */
  return 1; 
}

FILE *pathinput(char *name, char *pathname)
{ FILE *fp = fopen(name, "r");
  char filename[1024];
  int itemsep = FILE_SEP_CH=='/' ? ':' : ';';
  if (fp==0)
  { if (pathname && relfilename(name))
    { char *path = getenv(pathname);
      while(path && fp==0)
      { char *f=filename,
             *n=name;
        while(*path==itemsep) path++;
        if(*path==0) break;
        while(*path!=0 && *path!=itemsep) *f++ = *path++;
        if(f[-1]!=FILE_SEP_CH) *f++ = FILE_SEP_CH;
        while(*n) *f++ = *n++;
        *f = 0;
        fp = fopen(filename, "r");
      }
    }
  }
  return fp;
}

INT32 dosys(register INT32 *p, register INT32 *g)
{ register INT32 i;
  switch((int)p[3])
  {  default: printf("\nBad sys %ld\n", (long)p[3]);  return p[3];
  
     case 1: /* use MINTASM if count register is less than 0 */
            { INT32 *regsv = PT (B+p[4]);
              if(regsv[7]<0) return MINTASM(p[4], B);
              return interpret(p[4], B);
            }


     case  2: tracing = 1;                     return 0;
     case  3: tracing = 0;                     return 0;

     case  4: tallylim = TALLYUPB;
              for(i=1; i<=tallylim; i++) (PT (B+TALLYV))[i] = 0;
/*              logfp = fopen("LOGFILE", "w");*/
              return 0;

     case  5: tallylim = 0;
/*              fclose(logfp);*/
/*              logfp = NULL;*/
              return 0;
     
     case 10:
              { int ch;

                if (inbuf) {
                  // Input taken from command line
                  ch = inbuf_next();
                  if (ch == EOF) { // inbuf is now empty
                    if (reattach_stdin) {
                      free(inbuf);
                      inbuf = 0; // Don't try to read more characters from buffer
                      // Continue with normal read from stdin 
                    } else {
                      return ch; // EOF
                    }
                  } else {
                    return ch; // valid character
                  }
                }
                // Normal case, stdin input to interpreter

                ch = Readch(); /* get the keyboard character  */
		//PRINTFD("Sys_sardch: ch = %d\n", ch);
                if (ch>=0) putchar(ch);
                if(ch==13) { ch = 10; putchar(10); }
                fflush(stdout);
		//PRINTFD("Sys_sardch returning ch = %d\n", ch);
                return ch;
              }
	      /*
              { int ch = Readch();
                if (ch>=0) putchar((char)ch);
                if(ch==13) { ch = 10; putchar(10); }
                fflush(stdout);
                return ch;
              }
	      */

     case 11: if(p[4] == 10) putchar(13);
              putchar((char)p[4]);
              fflush(stdout);                      return 0;

     case 12: { FILE *fp = findfp(p[4]);
                INT32 bbuf = p[5];
                INT32 len  = p[6];
                len = fread(&B[bbuf], (size_t)1, (size_t)len, fp);
                return len;
              }

     case 13: { FILE *fp = findfp(p[4]);
                INT32 bbuf = p[5];
                INT32 len = p[6];
                len = WD fwrite(&B[bbuf], (size_t)1, (size_t)len, fp);
                fflush(fp);
                return len;
              }

     case 14: { FILE *fp = p[5] ? pathinput(file_str(B+p[4], chbuf), 
                                            file_str(B+p[5], chbuf))
                                : pathinput(file_str(B+p[4], chbuf), 0);
                if(fp==0) return 0L;
                return newfno(fp);
              }
     case 15: { FILE *fp = fopen(file_str(B+p[4], chbuf), "w");
                if(fp==0) return 0L;
                return newfno(fp);
              }

     case 16: { INT32 res = ! fclose(findfp(p[4]));
                freefno(p[4]);
                return res;
	      }
     case 17: return ! REMOVE(file_str(B+p[4], chbuf));
     case 18: REMOVE(file_str(B+p[5], chbuf2));
              return ! rename(file_str(B+p[4], chbuf), chbuf2);

     case 21: return getvec(p[4]);
     case 22: freevec(p[4]);                      return 0;
     case 23: return loadseg(B+p[4]);
     case 24: return globin(p[4], (INT32)(BP g-B));
     case 25: unloadseg(p[4]); return 0;
     case 26: { INT32 res =  muldiv(p[4], p[5], p[6]);
                g[Gn_result2] = result2;
                return res;
              }

     case 27: return setraster(p[4], p[5]);

     case 28: return intflag() ? -1L : 0L;

     case 30: /* Return CPU time in milliseconds  */
              return muldiv(clock(), 1000, TICKS_PER_SEC);

     case 31: /* Return time of last modification of file
                 whose name is in p[4]  */
              { struct stat buf;
                if (stat(file_str(B+p[4], chbuf), &buf)) return 0;
                return buf.st_mtime;
              }
  }
} 

/* file_str converts an MCPL file name to a C character string.
** The character '/' (or '\') is treated as a separator and is
** converted to FILE_SEP_CH ('/' for unix, '\' for MSDOS or ':' for MAC)
*/
char *file_str(char *str, char *cstr)
{  char *p = cstr;
   while(*str)
   { char ch = *str++;
     if(ch=='/' || ch=='\\') ch = FILE_SEP_CH;
     *p++ = ch;
   }
   *p = 0;
   return cstr;
} 

void wrcode(char *form, INT32 f, INT32 a)
{  wrfcode(f);
   printf("  ");
   printf(form, (long)a);
   printf("\n");
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
    case 13: s = "   LF$   L0G  L0G1  L0GH  LP13  SP13  INDW     S"; break;
    case 14: s = "    LM   L1G  L1G1  L1GH  LP14  SP14   LMH    SH"; break;
    case 15: s = "   LM1   L2G  L2G1  L2GH  LP15  SP15   BTC  MDIV"; break;
    case 16: s = "    L0    LG   LG1   LGH  LP16  SP16   NOP CHGCO"; break;
    case 17: s = "    L1    SG   SG1   SGH   SYS    S1    A1   NEG"; break;
    case 18: s = "    L2   LLG  LLG1  LLGH LVIND    S2    A2   NOT"; break;
    case 19: s = "    L3    AG   AG1   AGH   STB    S3    A3 INC1B"; break;
    case 20: s = "    L4   MUL   ADD    RV    ST    S4    A4 INC4B"; break;
    case 21: s = "    L5   DIV   SUB   RV1   ST1   XCH    A5 DEC1B"; break;
    case 22: s = "    L6   MOD   LSH   RV2   ST2  INDB  RVP3 DEC4B"; break;
    case 23: s = "    L7   XOR   RSH   RV3   ST3 INDB0  RVP4 INC1A"; break;
    case 24: s = "    L8    SL   AND   RV4  STP3   ATC  RVP5 INC4A"; break;
    case 25: s = "    L9   SL$    OR   RV5  STP4   ATB  RVP6 DEC1A"; break;
    case 26: s = "   L10    LL   LLL   RV6  STP5     J  RVP7 DEC4A"; break;
    case 27: s = "  FHOP   LL$  LLL$   RTN     -    J$ ST0P3     -"; break;
    case 28: s = "   JEQ   JNE   JLS   JGR   JLE   JGE ST0P4  HAND"; break;
    case 29: s = "  JEQ$  JNE$  JLS$  JGR$  JLE$  JGE$ ST1P3 HAND$"; break;
    case 30: s = "  JEQ0  JNE0  JLS0  JGR0  JLE0  JGE0 ST1P4   UNH"; break;
    case 31: s = " JEQ0$ JNE0$ JLS0$ JGR0$ JLE0$ JGE0$   CTA RAISE"; break;
  }

  for(i = 6*n; i<=6*n+5; i++) putchar(s[i]);
} 

void trace(INT32 pc, INT32 p, INT32 a, INT32 b)
{ printf("A=%9ld  B=%9ld    ", (long)a, (long)b);
  printf("P=%5ld %9ld:", (long)p, (long)pc);
  wrcode("(%3ld)", (INT32) B[pc], (INT32) B[pc+1]);
  putchar(13);
}

