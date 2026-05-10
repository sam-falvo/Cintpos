/*
This is CLIB for 64 bit BCPL compiled into native code on
64 bit hardware such as an AMD64 machine. The BCPL word length
and machine addresses are both 64 bits long.

It file contains the definition of main and enters BCPL compiled
machine code using callstart defined in mlib.s in eg amd64b64.

It is based on cintmain.c from the BCPL Cintcode system and is
meant to run on most machines with a Unix-like C libraries.

(c) Copyright:  Martin Richards  10 Jun 2024

This file is currently requires significant modification.

History

10/06/2024
Updating this file for 64 bit BCPL on 64 bit machines.
It uses define.h built by running mkdefines-h.c taken from
BCPL/cintcode/sysc. This provides #defines giving for example
the C type of 64 bit BCPL words.
*/

/* bcpl.h contains machine/system dependent #defines  */
#include "bcpl64.h"

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <math.h>

#if defined(forVmsItanium) || defined(forVmsVax)
#include <timeb.h>
#define TICKS_PER_SEC 1000
#else
#include <sys/timeb.h>
#endif

int gvecupb = 2000;
int stackupb = 50000;
void initsections(BCPLWORD *g) {
  return;
}

BCPLWORD callstart(BCPLWORD *p, BCPLWORD *g) {
  printf("callstart entered\n");
  return 0;
}

BCPLWORD rootnode[Rtn_upb+1];

static char *parms;  /* vector of command-line arguments */
static int  parmp=1; /* subscript of next command-line character. */
static int  ttyinp;  /* =1 if stdin is a tty, =0 otherwise */

/* Function defined in callc.c  */
extern BCPLWORD callc(BCPLWORD *args, BCPLWORD *g);

BCPLWORD trcount = -1; // trpush initially disabled
BCPLWORD trvec[4096];  // 4096 elements in the trpush circular buffer

/* Low level trace functions */

void trpush(BCPLWORD val);
BCPLWORD settrcount(BCPLWORD count); // Set trcount and returns its old value
BCPLWORD gettrval(BCPLWORD count); // Get the specified trace value
BCPLWORD incdcount(BCPLWORD n);


// Functions for keyboard input defined in kblib.c
extern int Readch(void);
extern int pollReadch(void);
extern int init_keyb(void);
extern int close_keyb(void);
extern int intflag(void);

/* prototypes for forward references */
static BCPLWORD muldiv(BCPLWORD a, BCPLWORD b, BCPLWORD c);
static BCPLWORD doflt(BCPLWORD op, BCPLWORD a, BCPLWORD b, BCPLWORD c);
static char *prepend_prefix(char *fromstr, char *tostr);
static char *osfname(char *name, char *osname);
static char *unixfname(char *name, char *osname);
static char *winfname(char *name, char *osname);
static char *vmsfname(char *name, char *osname);
char *b2c_str(BCPLWORD bstr, char *cstr);
BCPLWORD c2b_str(char *cstr, BCPLWORD bstr);

char *catstr2c_str(char *cstr1, char *cstr2, char *str);
void  dumpmem(BCPLWORD *mem, BCPLWORD upb, BCPLWORD context);
BCPLWORD timestamp(BCPLWORD *v);
void msecdelay(unsigned int delaymsecs);

/* Function defined in sdlfn.c */
BCPLWORD sdlfn(BCPLWORD *args, BCPLWORD *g, BCPLWORD *W);

/* Function normally defined in graphics.c  */
extern BCPLWORD sysGraphics(BCPLWORD *p);
BCPLWORD sysGraphics(BCPLWORD *p) { return 0; } /* Dummy definition */

BCPLWORD rootvarstr = 0;
BCPLWORD pathvarstr = 0;
BCPLWORD hdrsvarstr = 0;
BCPLWORD scriptsvarstr = 0;

BCPLWORD prefixstr = 0; /* Position in the Cintcode memory of the filename */
                        /* prefix. The prefixstr is prepended to all non */
                        /* absolute file names. prefixstr is set by */
                        /* sys(Sys_setprefix, str) and read by */
                        /* sys(Sys_getprefix). */
char *prefixbp;         /* Address of byte holding the length of the prefix */

BCPLWORD *stackbase;
BCPLWORD *globbase;
BCPLWORD result2;

static char chbuf1[256]; /* These buffers are used to hold filenames */
static char chbuf2[256];
static char chbuf3[256];
static char chbuf4[256];
static char* rootvar    = "BCPLROOT";    /* The default setting */
static char* pathvar    = "BCPLPATH";    /* The default setting */
static char* hdrsvar    = "BCPLHDRS";    /* The default setting */
static char* scriptsvar = "BCPLSCRIPTS"; /* The default setting */

int tracing = 0;
int filetracing = 0;

BCPLWORD memupb;        // In words
BCPLWORD Globupb=2000;  // MR 28/12/2019 used to be 1000
                        // also made a similar change in boot.b
BCPLWORD taskname[4];   // Used in getvec for debugging

BCPLWORD *W;    // This will hold the pointer to the memory used by getvec


/* Function defined in mlib -- the m/c library */
BCPLWORD callstart(BCPLWORD *p, BCPLWORD *g);

void copyaddrB2C(void*from, void*to); // Used for saving and restoring
void copyaddrC2B(void*from, void*to); // host machine addresses.


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

int badimplementation(void)
{ int bad = 0;
  int A='A';
  BCPLCHAR c = (BCPLCHAR)255;
  if(sizeof(BCPLWORD)!=(1<<B2Wsh)) {
       printf("Bytes per BCPL word is not %d\n", 1<<B2Wsh);
       bad = 1;
  }
  if(A!=65) {
       printf("Character set is not ASCII\n");
       bad = 1;
  }
  if (c/-1 != 1) {
    printf("BCPLCHAR should be a signed char\n");
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

/* The following four functions are necessary since the type FILE*
** is too large for a BCPL word on some machines (such as the DEC Alpha)
*/

#if defined(forALPHA) || defined(forLINUXAMD64)
#define Fnolim 100

FILEPT fpvec[Fnolim];

int initfpvec(void)
{ BCPLWORD i;
  for(i=1;i<Fnolim;i++) fpvec[i]=NULL;
  return 0;
}

BCPLWORD newfno(FILEPT fp)
{ BCPLWORD i;
  for(i=1;i<Fnolim;i++) if(fpvec[i]==NULL){ fpvec[i]=fp; return i; }
  return 0;
}

BCPLWORD freefno(BCPLWORD fno)
{ if(0<fno && fno<Fnolim){fpvec[fno]=NULL; return 1; }
  return 0;
}

FILEPT findfp(BCPLWORD fno)
{ if(0<fno && fno<Fnolim) return fpvec[fno];
  return 0;
}

#else

int   initfpvec(void)    { return 0; }
BCPLWORD newfno(FILEPT fp)   { return WD (long)fp; }
BCPLWORD freefno(BCPLWORD fno) { return fno; }
FILEPT findfp(BCPLWORD fno)  { return (FILEPT )(long)fno; }

#endif

/* storage for SIGINT handler */
void (*old_handler)(int);

void handler(int sig)
{ 
  printf("SIGINT received\n");
  old_handler = signal(SIGINT, old_handler);
  close_keyb();
  exit(20);
}

int main(int argc, char *argv[])
{ int rc;
  int i;                         // for FOR loops
  BCPLWORD res;

  printf("main: entered\n");

  if ( badimplementation() )
  { printf("This implementation of C is not suitable\n");
    return 20;
  }

  for(i=0; i<=Rtn_upb; i++) rootnode[i] = 0;

  /* Try to reconstruct the command line arguments from argv */
  parms = (char *)(MALLOC(256));

  { int p=0;
    parms[0] = 0;

    for (i=1; i<argc; i++) {
      char *arg = argv[i];
      int len = strlen(arg);
      int j;
      int is_string = 0; // =1 if arg contains ", space, or newline.white space
      /*printf("clib: getting command line, len=%d\n", len); */
      for (j=0; j<len; j++)
        if( arg[j]=='"' || arg[j]==' ' || arg[j]=='\n') is_string = 1;
      /*printf("clib: getting command line, is_string=%d\n", is_string); */
      parms[++p] = ' ';
      if(is_string)
      { parms[++p] = '"';
        for (j=0; j<len; j++)
        { int ch = arg[j];
          if(ch=='\n') { parms[p++] = '*'; parms[++p] = 'n'; }
          if(ch=='"')  { parms[p++] = '*'; parms[++p] = '"'; }
	  else parms[++p] = ch;
        }
        parms[++p] = '"';
      } else {
        for (j=0; j<len; j++) parms[++p] = arg[j];
      }
    }
    parms[++p] = '\n';  /* Put a newline at the end of the argument string */
    parms[0] = p; /* Fill in the BCPL string length */
    parmp = 1;    /* Subscript of the first character of the */
                  /* command-line argument */

    //printf("clib: args: len=%d\n", parms[0]);
    //for(i=1; i<=parms[0];i++) printf("parm[%d]=%d\n", i, parms[i]);
    //printf("\n");
  }

  /*  parms = (BCPLWORD *)(MALLOC(argc+1)); */
  /*parms[0] = argc > 1 ? argc : 0; */
  /*for (i = 0; i < argc; i++) { */
  /*  BCPLWORD v = (BCPLWORD)(MALLOC(1+strlen(argv[i]) >> B2Wsh)) >> B2Wsh; */
  /*  c2b_str(argv[i], v); */
  /*  parms[1+i] = v; */
  /*} */

  old_handler = signal(SIGINT, handler);
  initfpvec();

  // Allocate space for the environment variable names
  // and file prefix string.
  rootvarstr    = ((BCPLWORD)malloc(5*16*4))>>B2Wsh;
  pathvarstr    = rootvarstr+1*16;
  hdrsvarstr    = rootvarstr+2*16;
  scriptsvarstr = rootvarstr+3*16;
  prefixstr     = rootvarstr+4*16;
  prefixbp      = (char *)(prefixstr<<B2Wsh);
  for(i=0; i<=5*16; i++) ((BCPLWORD*)(rootvarstr<<B2Wsh))[i] = 0;
  printf("clib.c: rootnode[Rtn_hdrsvar]=%ld\n", LL rootnode[Rtn_hdrsvar]);

  c2b_str(rootvar, rootvarstr);
  c2b_str(pathvar, pathvarstr);
  c2b_str(hdrsvar, hdrsvarstr);
  c2b_str(scriptsvar, scriptsvarstr);

  rootnode[Rtn_rootvar]      = rootvarstr;
  rootnode[Rtn_pathvar]      = pathvarstr;
  rootnode[Rtn_hdrsvar]      = hdrsvarstr;
  rootnode[Rtn_scriptsvar]   = scriptsvarstr;

  printf("rootnode=%ld\n", (BCPLWORD)rootnode/4);
  printf("rootnode[Rtn_hdrsvar]=%ld\n", rootnode[Rtn_hdrsvar]);
  if(filetracing||1) {
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
  stackupb +=00;
  printf("1: stackupb=%d gvecupb=%d\n", stackupb, gvecupb);
  stackbase = (BCPLWORD *)(calloc((stackupb+1), 1<<B2Wsh));
  printf("5: stackupb=%d gvecupb=%d\n", stackupb, gvecupb);
  stackbase = (BCPLWORD *)(calloc((stackupb+1), 1<<B2Wsh));
  printf("6: stackupb=%d gvecupb=%d\n", stackupb, gvecupb);
  stackbase = (BCPLWORD *)(calloc((stackupb+1), 1<<B2Wsh));
  if(stackbase==0) 
    { printf("unable to allocate space for stackbase\n");
      exit(20);
    }

  gvecupb = 16380+0; stackupb;
  printf("2: stackupb=%d gvecupb=%d\n", stackupb, gvecupb);
  //globbase = (BCPLWORD *)(calloc((stackupb+1), 1<<B2Wsh));
  globbase = (BCPLWORD *)(calloc(( gvecupb+1), 1<<B2Wsh));
  printf("7: stackupb=%d gvecupb=%d\n", stackupb, gvecupb);
  //stackbase = (BCPLWORD *)(calloc((gvecupb+1), 1<<B2Wsh));
  //globbase  = (BCPLWORD *)(malloc((gvecupb+1)));
  //globbase  = (BCPLWORD *)(malloc(10000));
  if(globbase==0) 
    { printf("unable to allocate space for globbase\n");
      exit(20);
    }
  return 129;

  globbase[0] = gvecupb;

  for (i=1;i<=gvecupb;i++) globbase[i] = Globword + i;
  globbase[Gn_rootnode] = ((BCPLWORD)rootnode)>>B2Wsh;
  //printf("globbase[Gn_rootnode]=%d\n", globbase[Gn_rootnode]);

  for (i=0;i<=stackupb;i++) stackbase[i] = 0;

  printf("clib: gvecupb=%d stackupb=%d\n", gvecupb, stackupb);
  /* initsections, gvecupb and stackupb are defined in the file */
  /* (typically) initprog.c created by a call of the command makeinit. */

  printf("Calling initsections\n");
  initsections(globbase);
  printf("Calling init_keyb\n");
  ttyinp = init_keyb();

  /*
  printf("globbase[Gn_rootnode]=%d\n", globbase[Gn_rootnode]);

  { int i;
    for (i=0; i<20; i++) printf("g%2d = #x%8X %d\n", i, globbase[i], globbase[i]);
  }
  */

  printf("clib: calling callstart(%ld, %ld)\n",
  	 LL stackbase, LL globbase);
  /* Enter BCPL start function: callstart is defined in i386/mlib.s */

  res = callstart(stackbase, globbase);

  close_keyb();

  //for (i=0; i<20; i++) {
  //  if(i % 5 == 0) printf("\nG%3i:", i, globbase[i]);
  //  printf(" %08X", globbase[i]);  //}
  printf("\n");
  if (res) printf("Execution finished, return code %ld\n", (long)res);

  free(globbase);
  free(stackbase);
  free(parms);

  printf("main: returning with res=%ld\n", LL res);
  return res;
}

BCPLWORD muldiv1(BCPLWORD a, BCPLWORD b, BCPLWORD c)
{ // This version produces the same results as muldiv1
  // and seem to run about 60% faster.
  // It is used by the MDIV instruction (and the syslib muldiv function).
  BCPLINT64 ab = (BCPLINT64)a * (BCPLINT64)b;
  //printf("muldiv: entered\n");
  if(c==0) c=1;
  result2 = (BCPLWORD)(ab % c);
  return (BCPLWORD)(ab / c);
}

BCPLWORD muldiv(BCPLWORD a, BCPLWORD b, BCPLWORD c)
{ // This is used by sys(Sys_muldiv,...)
  unsigned BCPLWORD q=0, r=0, qn, rn;
  unsigned BCPLWORD ua, ub, uc;
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
    if(rn>=uc) { qn++; rn -= uc; }
  }
  result2 = rneg ? -(BCPLWORD)r : r;
  //printf("muldiv: result2=%d\n", result2);
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
    /* If no pathname given or name is absolute just search the current directory */
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

BCPLWORD dosys(register BCPLWORD *p, register BCPLWORD *g)
{ register BCPLWORD i;
  /*printfD("dosys(%" FormD ", ", (BCPLWORD)p); */
  /*printfD("%" FormD, g); */
  /*PINTFD(") P3=%" FormD " ", p[3]); */
  /*printfD("P4=%" FormD "\n",  p[4]); */

  switch((int)(p[3]))
  { default: printf("\nBad sys %ld\n", (long)p[3]);  return p[3];
  
    // case Sys_setcount: set count               -- done in cinterp
    case Sys_quit:     exit(p[4]); //return from interpreter

    /*
    ** case Sys_rti:      sys(Sys_rti, regs)      -- done in cinterp  Cintpos
    ** case Sys_saveregs: sys(Sys_saveregs, regs) -- done in cinterp  Cintpos
    ** case Sys_setst:    sys(Sys_setst, st)      -- done in cinterp  Cintpos
    */
    case Sys_tracing:  /* sys(Sys_tracing, b)  */
      tracing = p[4];
      return 0;
    /* case Sys_watch:    sys(Sys_watch, addr)    -- done in cinterp
    */
    /*
    case  Sys_tally:         // sys(Sys_tally, flag) 
      if(p[4]) {
        tallylim = tallyupb;
        for(i=1; i<=tallylim; i++) tallyv[i] = 0;
      } else {
        tallylim = 0;
      }
      return 0;
     
    case Sys_interpret: // call interpreter (recursively)
    { BCPLWORD regsv = p[4];
      if(W[regsv+7]>=0 || slowflag) return interpret(regsv, W);
      return CINTASM  (regsv, W);
    }
    */

    case Sys_sardch:
    { BCPLWORD ch;
      /*printf("parmp=%d parms[0]=%d\n", parmp, parms[0]); */
      if(parmp<=parms[0]) {  /* Added MR 10/04/06 */
        /* Read the command arguments (without echo) first. */
        /*printf("sardch: parmp=%d parms[0]=%d\n", parmp, parms[0]); */
        /*printf("sardch: returning %d\n", parms[parmp]); */
        return parms[parmp++];
      }
      ch = Readch();
      if (ttyinp) {  /* echo tty input only */
        if (ch>=0) putchar((char)ch);
        if(ch==13) { ch = 10; putchar(10); }
        fflush(stdout);
      }
      return ch;
    }

    case Sys_sawrch:
      if(p[4] == 10) putchar(13);
      putchar((char)p[4]);
      fflush(stdout);
      return 0;

    case Sys_read:  /* bytesread := sys(Sys_read, fp, buf, bytecount) */
    { FILE *fp = findfp(p[4]);
      char *bbuf = (char *)(p[5]<<B2Wsh);
      BCPLWORD len   = p[6];
      len = fread(bbuf, (size_t)1, (size_t)len, fp);
      return len;
    }

    case Sys_write:
      { FILE *fp = findfp(p[4]);
        char *bbuf = (char *)(p[5]<<B2Wsh);
        BCPLWORD len = p[6];
        len = WD fwrite(bbuf, (size_t)1, (size_t)len, fp);
        fflush(fp);
        return len;
      }

    case Sys_openread:
      { char *name = b2c_str(p[4], chbuf1);
        FILEPT fp;
        fp = pathinput(name,                      /* Filename */
                       b2c_str(p[5], chbuf2));    /* Environment variable */
        if(fp==0) return 0L;
        return newfno(fp);
      }

    case Sys_openwrite:
      { char *name = b2c_str(p[4], chbuf1);
        FILEPT fp;
        fp = fopen(osfname(name, chbuf4), "wb");
        if(fp==0) return 0L;
        return newfno(fp);
      }

    case Sys_openappend:
      { char *name = b2c_str(p[4], chbuf1);
        FILEPT fp;
        fp = fopen(osfname(name, chbuf4), "ab");
        if(fp==0) return 0L;
        return newfno(fp);
      }

    case Sys_openreadwrite:
      { char *name = b2c_str(p[4], chbuf1);
	FILEPT fp;
        fp = fopen(osfname(name, chbuf4), "rb+");
        if(fp==0) fp = fopen(name, "wb+");
        if(fp==0) return 0L;
        return newfno(fp);
      }

    case Sys_close:
    { BCPLWORD res = fclose(findfp(p[4]));
      freefno(p[4]);
      return res==0 ? -1 : 0; /* res==0 means success */
    }

    case Sys_deletefile:
    { char *name = b2c_str(p[4], chbuf1);
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
    { char *name1 = b2c_str(p[4], chbuf1);
      char *name2 = b2c_str(p[5], chbuf2);
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
      return ((BCPLWORD)(malloc((1+p[4])<<B2Wsh)))>>B2Wsh;

    case Sys_freevec:
      free((void *)(p[4]<<B2Wsh)); return -1;
/*
    case Sys_loadseg:
      return loadseg(b2c_str(p[4], chbuf1));

    case Sys_globin:
      return globin(p[4], g);

    case Sys_unloadseg:
      unloadseg(p[4]);                    return 0;
*/

    case Sys_muldiv:
      //printf("dosys: calling muldiv(%d, %d, %d)\n", p[4], p[5], p[6]);
    { BCPLWORD res =  muldiv(p[4], p[5], p[6]);
	//printf("res=%d   result2=%d\n", res, result2);
      globbase[Gn_result2] = result2;
      return res;
    }

    case Sys_intflag:
      return intflag() ? -1L : 0L;

/*
    case Sys_setraster:
      return setraster(p[4], p[5]);
*/

    case Sys_cputime: /* Return CPU time in milliseconds  */
      //return muldiv(clock(), 1000, TICKS_PER_SEC);
      return muldiv(clock(), 1000, 1000);

#ifndef forWinCE
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
      { //struct stat buf;
      BCPLWORD days, secs, msecs;
      char *name = b2c_str(p[4], chbuf1);
      BCPLWORD *datestamp = (BCPLWORD *)(p[5]<<B2Wsh);
      ///if (stat(osfname(name, chbuf4), &buf)) {
        datestamp[0] = 0;
        datestamp[1] = 0;
        datestamp[2] = -1;
        return 0;
	///}

	secs = 0; //buf.st_mtime;
      // nsecs = buf.st_mtimensec; // nano second time, if poss
      days = secs / (24*60*60);
      msecs = (secs % (24*60*60)) * 1000;
      datestamp[0] = days;
      datestamp[1] = msecs;
      datestamp[2] = -1;  // New dat format
      //printf("filemodtime: name=%s days=%" FormD " msecs=%" FormD "\n",
      //        name, days, msecs);
      return -1;
    }
#endif

    case Sys_setprefix: /* Set the file name prefix string  */
    { BCPLWORD str = p[4];
      char *fp = (char*)(str<<B2Wsh);
      char *tp = prefixbp;
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
#ifdef forWinCE
              return chBufEmpty() ? -1 : 0;
#else
              return -1;
#endif

    case 36:   return 0; /* Spare */

    case 37:   return 0; /* Spare  */

    case Sys_seek:  /* res := sys(Sys_seek, fd, pos)   */
    { FILEPT fp = findfp(p[4]);
      BCPLWORD pos = p[5];
      BCPLWORD res = fseek(fp, pos, SEEK_SET);
      /*printf("fseek => res=%d errno=%d\n", res, errno); */
      /*g[Gn_result2] = errno; */
      return res==0 ? -1 : 0; /* res=0 succ, res=-1 error  */
    }

    case Sys_tell: /* pos := sys(Sys_tell,fd)  */
    { FILE *fp = findfp(p[4]);
      BCPLWORD pos = ftell(fp);
      /*g[Gn_result2] = errno; */
      return pos; /* >=0 succ, -1=error */
    }

    case Sys_waitirq: /* Wait for irq */
      /*
      pthread_mutex_lock  (         &irq_mutex);
      pthread_cond_wait   (&irq_cv, &irq_mutex);
      pthread_mutex_unlock(         &irq_mutex);
      */
      return 0;

    case Sys_lockirq: /* Stop all devices from modifying */
                      /* packets or generating interrupts */
      /*
      pthread_mutex_lock(&irq_mutex);
      */
      return 0;

    case Sys_unlockirq: /* Allow devices to modify packets */
                        /* and generate interrupts */
      /*
      pthread_mutex_unlock(&irq_mutex);
      */
      return 0;

    case Sys_devcom: /* res := sys(Sys_devcom, dcb, com, arg) */
      return 0; /*devcommand(W[p+4], W[p+5], W[p+6]); */

    case Sys_datstamp: /* res := sys(Sys_datstamp, v)  */
    // Set v!0 = days  since 1 January 1970
    //     v!1 = msecs since midnight
    //     v!2 = ticks =-1 for new dat format
    // Return -1 if successful
      return timestamp((BCPLWORD*)(p[4]));

    case Sys_filesize:  /* res := sys(Sys_filesize, fd)   */
      { FILEPT fp   = findfp(p[4]);
        long pos  = ftell(fp);
        BCPLWORD rc   = fseek(fp, 0, SEEK_END);
        BCPLWORD size = ftell(fp);
        rc  = fseek(fp, pos, SEEK_SET);
        if (rc) size = -1;
        return size; /* >=0 succ, -1=error  */
      }

     case Sys_getsysval: /* res := sys(Sys_getsysval, addr) */
     { BCPLWORD *addr = (BCPLWORD*)p[4];
       return *addr;
     }

     case Sys_putsysval: /* res := sys(Sys_putsysval, addr, val) */
     { BCPLWORD *addr = (BCPLWORD*)p[4];
       *addr = p[5];
       return 0;
     }

     case Sys_shellcom: /* res := sys(Sys_shellcom, comstr) */
     { char *comstr = (char*)(p[4]<<2);
       int i;
       char com[256];
       int len = strlen(comstr);
       for(i=0; i<len; i++) com[i] = comstr[i+1];
       com[len] = 0;
       /*
       printf("\ndosys: calling shell command %s\n", com);
       */
       return system(com);
     }

     case Sys_getpid: /* res := sys(Sys_getpid) */
       return getpid();

     case Sys_dumpmem: /* sys(Sys_dumpmem, context) */
       printf("\nCintpos memory not dumped to DUMP.mem\n");
       return 0;

     case Sys_callnative:
     { /* Call native code. */
       int(*rasmfn)(void) = (int(*)(void))&p[4];
       return rasmfn();
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
                return 0; /**p[4] += p[5];*/
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
              { unsigned int msecs = (unsigned int)p[4];
                msecdelay(msecs);
                return 0;
              }

    case Sys_sound: /* res := sys(Sys_sound, fno, a1, a2,...) */
#ifdef SOUND
                return soundfn(&p[4], &g[0]);
#else
                return 0;
#endif

    case Sys_sdl: /* res := sys(Sys_sdl, fno, a1, a2,...) */
#ifdef SDLavail
      return sdlfn(&((BCPLWORD*)p)[4], &((BCPLWORD*)g)[0], 0/*W*/);
#else
      return 0;
#endif

    case Sys_callc: /* res := sys(Sys_callc, fno, a1, a2,...) */
#ifdef CALLC
      /*
         printf("dosys: sys(Sys_callc, %" FormD ", %" FormD ", %" FormD", ...)\n",
                 W[p+4], W[p+5], W[p+6]);
      */
      return 0;//callc(&p[4], &g[0]);
#else
                return -1;
#endif

    case Sys_trpush: /* sys(Sys_trpush, val) */
                trpush(p[4]);
                return 0;

    case Sys_settrcount: /* res := sys(Sys_settrcount, count) */
                return settrcount(p[4]);

    case Sys_gettrval: /* res := sys(Sys_gettrval, count) */
                return gettrval(p[4]);

    case Sys_flt: /* res := sys(Sys_flt, op, a, b)) */
              { BCPLWORD res = doflt(p[4], p[5], p[6], p[7]);
                globbase[Gn_result2] = result2;
		//g[Gn_result2] = result2;
		//if(W[p+4]==35)
		//printf("sys_flt: op=%d res=%08" FormX " result2=%08" FormX "\n",
                //       W[p+4], (UBCPLWORD)res, (UBCPLWORD)result2);
                return res;
              }

    case Sys_pollsardch: /* res := sys(Sys_pollsardch) */
              {
                // Return a character if available otherwise pollch (=-3)
                return pollReadch();
              }

    case Sys_incdcount: /* res := sys(Sys_incdcount, n) */
                return incdcount(p[4]);


		//#ifndef forWinCE
    case 135:
    { /* Return system date and time in VEC 5 */
      time_t clk = time(0);
      struct tm *now = gmtime(&clk);
      BCPLWORD *arg = PT(p[4] << B2Wsh);
      arg[0] = now->tm_year+1900;
      arg[1] = now->tm_mon+1;
      arg[2] = now->tm_mday;
      arg[3] = now->tm_hour;
      arg[4] = now->tm_min;
      arg[5] = now->tm_sec;
      return 0;
    }

    case 136: /* Return current directory in VEC 1 + 256/bytesperword */
    { char *res = getcwd(chbuf1, 256);
      c2b_str(chbuf1, p[4]);
      return 0;
    }

    case 137:
      return (BCPLWORD)parms >> B2Wsh;
  }
  // This is unreadchable
  // return 0;
}

void msecdelay(unsigned int delaymsecs) {
#if (defined(forWIN32) || defined(forWinCE))
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
		  //printf("Sys_delay: diffmsecs = %" FormD " msec\n", diffmsecs);
                  if (diffmsecs<=0) return;
                  if (diffmsecs>900) diffmsecs = 900;
                  timeout.tv_sec = 0;
                  timeout.tv_usec = diffmsecs * 1000;
		  //printf("Sys_delay: waiting for %" FormD " msec\n", diffmsecs);
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
    printf("doflt(%ld, %ld, %ld) not implemented\n", LL op, LL a, LL b);

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
      return F2N (res);
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
      FLOAT64 d = (FLOAT64) (N2F a);  // N2F is defined to be *(BCPLFLOAT*)&
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

      //printf("unmk8: d = %24.18f mantissa=%18lld exponent=%d\n", d, mantissa, exponent);

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
      return F2N fa;
    }

    case fl_fix: // Return nearest integer
    { BCPLFLOAT fa = N2F a;
      if(fa<0.0) return (BCPLWORD)(fa - 0.5);
      else       return (BCPLWORD)(fa + 0.5);
    }

    case fl_abs:
    { BCPLFLOAT fa = N2F a;
      if (fa<0.0) fa = -fa;
      return F2N fa;
    }

    case fl_mul:
    { BCPLFLOAT res = N2F a * N2F b;
      return F2N res;
    }

    case fl_div:
    { BCPLFLOAT res = N2F a / N2F b;
      return F2N res;
    }

    case fl_mod:
    { BCPLFLOAT res = Cfmod(N2F a, N2F b);
      return F2N res;   // Return the floating point remainder
                        // after dividing a by b. Ie return
                        // x - n * y where n is the integer
                        // quotient of x / y. ie how many times
                        // y can be subtracted from x before
                        // the sign changes.
    }

    case fl_add:
    { BCPLFLOAT res = N2F a + N2F b;
      return F2N res;
    }

    case fl_sub:
    { BCPLFLOAT res = N2F a - N2F b;
      return F2N res;
    }

    case fl_pos:
      return a;

    case fl_neg:
    { BCPLFLOAT res = N2F a;
      res = -res;
      return F2N res;
    }

    case fl_eq:
      return N2F a == N2F b ? -1 : 0;

    case fl_ne:
      return N2F a != N2F b ? -1 : 0;

    case fl_ls:
      //printf("cintsys: fl_ls of %8X and %8X\n", a, b);
      //printf("cintsys: fl_ls of %13.4f and %13.4f\n", N2F a, N2F b);
      return N2F a < N2F b ? -1 : 0;

    case fl_gr:
      return N2F a > N2F b ? -1 : 0;

    case fl_le:
      return N2F a <= N2F b ? -1 : 0;

    case fl_ge:
      return N2F a >= N2F b ? -1 : 0;

    case fl_acos:
    { BCPLFLOAT res = Cacos(N2F a);
      return F2N res;
    }

    case fl_asin:
    { BCPLFLOAT res = Casin(N2F a);
      return F2N res;
    }

    case fl_atan:
    { BCPLFLOAT res = Catan(N2F a);
      return F2N res;
    }

    case fl_atan2:
    { BCPLFLOAT res = Catan2(N2F a, N2F b);
      return F2N res;
    }

    case fl_cos:
    { BCPLFLOAT res = Ccos(N2F a);
      return F2N res;
    }

    case fl_sin:
    { BCPLFLOAT res = Csin(N2F a);
      return F2N res;
    }

    case fl_tan:
    { BCPLFLOAT res = Ctan(N2F a);
      return F2N res;
    }

    case fl_cosh:
    { BCPLFLOAT res = Ccosh(N2F a);
      return F2N res;
    }

    case fl_sinh:
    { BCPLFLOAT res = Csinh(N2F a);
      return F2N res;
    }

    case fl_tanh:
    { BCPLFLOAT res = Ctanh(N2F a);
      return F2N res;
    }

    case fl_exp:
    { BCPLFLOAT res = Cexp(N2F a);
      return F2N res;
    }

    case fl_frexp:
    // If a is non zero it returns a fraction f in the
    // range 0.5 (inclusive) to 1.0 (exclusive) and an
    // integer exponent e in result2 such that f = a x 2^e
    { int r2=222;
      // frexp expects an int* as its second argument but
      // actually places a floating point integer there.
      BCPLFLOAT fa = frexp(N2F a, &r2);
      //printf("frexp: a=%13.3f => %13.6f  e=%d\n", N2F a, fa, r2);
      result2 = r2;
      //printf("frexp: resul2=%d\n", result2);
      return F2N fa;
    }

  case fl_ldexp:  // Returns a x 2^b where a is floating
                  //                 and b is an integer
    { BCPLFLOAT res = ldexp(N2F a, b);
      return F2N res;
    }

    case fl_log:
    { BCPLFLOAT res = log(N2F a);
      return F2N res;
    }

    case fl_log10:
    { BCPLFLOAT res = log10(N2F a);
      return F2N res;
    }

    case fl_modf:
    { BCPLFLOAT r1, r2;
#ifdef TARGET64
      r1 = modf(N2F a, &r2);
#else
      r1 = modff(N2F a, &r2);
#endif
      result2 = F2N r2;   // The integer part of x as a BCPLFLOAT
      return F2N r1;      // The fractional part of x
                          // both are the same sign a x
    }
 
    case fl_pow:
    { BCPLFLOAT res = pow(N2F a, N2F b);
      return F2N res; 
    }

    case fl_sqrt:
    { BCPLFLOAT res = sqrt(N2F a);
      return F2N res;
    }

    case fl_ceil:
    { BCPLFLOAT res = ceil(N2F a);
      return F2N res;
    }

    case fl_floor:
    { BCPLFLOAT res = floor(N2F a);
      return F2N res;
    }

    case fl_N2F:
    { // eg sys(Sys_flt, fl_N2F, 1_000, 1_234) => 1.234
      BCPLFLOAT res = (BCPLFLOAT)b / (BCPLFLOAT)a;
      return F2N res;
    } 

    case fl_F2N:
    { // eg sys(Sys_flt, fl_F2N, 1_000, 1.234) => 1_234
      BCPLFLOAT fa = (BCPLFLOAT)a;
      BCPLFLOAT fb = N2F b;
      BCPLFLOAT res = fa * fb;
      if(res<0) { res = res - 0.5; }
      else      { res = res + 0.5; }
      return (BCPLWORD)res;
    }

    case fl_radius2:
    { // eg sys(Sys_flt, fl_radius2, 3.0, 4.0) => 5.0
      // since 9 + 16 = 25
      BCPLFLOAT fa = N2F a;
      BCPLFLOAT fb = N2F b;
      BCPLFLOAT res = sqrt(fa*fa + fb*fb); 
      return F2N res;
    }

    case fl_radius3:
    { // eg sys(Sys_flt, fl_radius3, 1.0, 2.0, 2.0) => 3.0
      // since 1 + 4 + 4 = 9
      BCPLFLOAT fa = N2F a;
      BCPLFLOAT fb = N2F b;
      BCPLFLOAT fc = N2F c;
      BCPLFLOAT res = sqrt(fa*fa + fb*fb+fc*fc); 
      return F2N res;
    }
  }
}

BCPLWORD timestamp(BCPLWORD *v) {
  // Set v[0] = days since 1 January 1970
  //     v[1] = msecs since midnight
  //     v[2] = -1 for new dat format
  // Return -1 if successful
#if defined(forWinCE) || defined(forMacOSPPC) || defined(forMacOSX)
  v[0]=v[1]=0;
  v[2]=-1;
  return 0;   /* To indicate failure */
#else
  unsigned int days  = 0;
  BCPLINT64    secs = 0;
  unsigned int msecs = 0;

  const unsigned int secsperday = 60*60*24;

#if defined(forWIN32) || defined(forCYGWIN32) || defined(forLINUX)
  { // Code for Windows, Cygwin and Linux
    // ftime is obsolete and the advice is to use
    // gettimeofday or clock_gettime
    // neither of these are widely available yet.
    struct timeb tb;
    ftime(&tb);
    secs = tb.time;
    //if(tb.dstflag) secs += 60*60;
    secs -= tb.timezone * 60;
    //printf("tb.dstflag=%" FormD " tb.timezone=%" FormD "\n",
    //        tb.dstflag, tb.timezone);
    msecs = tb.millitm;
  }
#else
  // Code for systems having the function gettimeofday.
  struct timeval tv;
  gettimeofday(&tv, NULL);
  secs = tv.tv_sec;
  msecs = tv.tv_usec / 1000;
  secs += 60*60;     /* Fudge -- Add one hour for BST */
#endif

  // secs  = seconds since 1 January 1970
  // msecs = micro seconds since start of current second

  secs += rootnode[Rtn_adjclock] * 60; /* Add adjustment */

  days  += secs / secsperday; // convert secs to days
  secs  %= secsperday; // Seconds since midnight
  msecs += secs*1000;  // milliseconds since midnight

  v[0] = days;  // days  since on 1 January 1970
  v[1] = msecs; // msecs  since midnight
  v[2] = -1;    // For new dat format
  //  printf("days=%" FormD " msecs=%" FormD "\n", days, msecs);
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
BCPLWORD c2b_str(char *cstr, BCPLWORD bstr) {
  char *bp = (char *)(bstr << B2Wsh);
  int len = 0;
  while (cstr[len]) {
    bp[len+1] = cstr[len];
      ++len;
  }
  bp[0] = len;
  return bstr;
}

char *b2c_str(BCPLWORD bstr, char * cstr)
{ char i, len;
  char *bp = (char *)(bstr<<B2Wsh);
  if (bstr==0) return 0;
  len = *bp++;
  for(i = 0; i<len; i++) cstr[i] = *bp++;
  cstr[len] = 0;
  return cstr;
} 

void trpush(BCPLWORD val) {
  // If trcount>=0 push val into the circular trace buffer
  // possibly preceeded by a time stamp.
  // Note that trpush is disabled if trcount=-1
  /*  pthread_mutex_lock(&trpush_mutex); */
  if(trcount>=0) {
#if defined(forWinCE) || defined(forMacOSPPC) || defined(forMacOSX)
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
  /*  pthread_mutex_unlock(&trpush_mutex); */
}

BCPLWORD settrcount(BCPLWORD count) {
  // Set trcount returning the previous value.
  // Setting trcount to -1 disables trpush.
  BCPLWORD res;
  /* pthread_mutex_lock(&trpush_mutex); */
  res = trcount;
  trcount = count;
  /* pthread_mutex_unlock(&trpush_mutex); */
  return res;
}

BCPLWORD gettrval(BCPLWORD count) {
  // Return the trace value corresponding to position count.
  // The result is only valid if the circular buffer has not overflowed
  BCPLWORD res;
  //pthread_mutex_lock(&trpush_mutex);
  res = trvec[count & 4095];
  //pthread_mutex_unlock(&trpush_mutex);
  return res;
}

BCPLWORD incdcount(BCPLWORD n) {
  //  BCPLWORD dv = W[rootnode + Rtn_dcountv];
  //  if(0 < n && n <= W[dv]) return ++W[dv+n];
  return -1;
}

#ifdef SOUND
#include "soundfn.c"
#endif
