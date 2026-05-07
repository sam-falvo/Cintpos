/*
** This is CLIB for MCPL compiled into native code
**
** It is based on clib.c from the BCPL native code system and is
** meant to run on most machines with a Unix-like C libraries.
**
** (c) Copyright:  Martin Richards  12 May 1997
**
*/

/*
23/7/96  First implementation
7/11/96  Systematic changes to allow 64 bit implementation on the ALPHA
12/5/96  Converted for the MCPL native code system
*/


#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

/* bcpl.h contains machine/system dependent #defines  */
#include "mcplsys.h"

/* Functions defined in kblib.c  */
extern int Readch(void);
extern int init_keyb(void);
extern int close_keyb(void);
extern int intflag(void);

/* external declared in init.c  */
extern WORD stackupb, gvecupb;
extern void init(void);

WORD *globbase, *stackbase, result2;

WORD getvec(WORD upb);
void freevec(WORD p);
WORD callstart(WORD *p, WORD *g);
WORD muldiv(WORD a, WORD b, WORD c);
WORD dosys(WORD *p, WORD *g);

char *m2c_str(WORD bstr, char *cstr);

#define Globword  0xEFEF0000L

#define Gn_globsize    0
#define Gn_start       1
#define Gn_currco      7
#define Gn_colist      8
#define Gn_rootnode    9
#define Gn_result2    10

int badimplementation(void)
{ int bad = 0, A='A';
  SIGNEDCHAR c = 255;
  if(sizeof(WORD)!=BPW || A!=65) bad = 1;
  if (c/-1 != 1) { printf("There is a problem with SIGNEDCHAR\n");
                   bad = 1;
                 }
  return bad;
}

int   initfpvec(void)    { return 0; }
WORD newfno(FILE *fp)   { return WD fp; }
WORD freefno(WORD fno) { return fno; }
FILE *findfp(WORD fno)  { return (FILE *)fno; }

void (*old_handler)(int);

void handler(int sig)
{ 
  printf("SIGINT received\n");
  old_handler = signal(SIGINT, old_handler);
  close_keyb();
  exit(20);
}

int main()
{ WORD i;      /* for FOR loops  */
  WORD res;    /* result of interpret  */

  if(badimplementation())
  { printf("This implementation of C is not suitable\n");
    return 0;
  }

  old_handler = signal(SIGINT, handler);

  initfpvec();

  globbase  = (WORD *)malloc((gvecupb+1)*BPW);
  stackbase = (WORD *)malloc((stackupb+1)*BPW);

  globbase[0] = gvecupb;

  for (i=1;i<=gvecupb;i++) globbase[i] = Globword + i;
  for (i=0;i<=stackupb;i++) stackbase[i] = 0;

  initsections(globbase);

  init_keyb();

  /* Enter MCPL start function */

  res = callstart(stackbase, globbase); /* defined in syslib.s */

  close_keyb();

  if (res) printf("\nExecution finished, return code %ld\n", (long)res);

  free(globbase);
  free(stackbase);

  return res;
}

WORD muldiv(WORD a, WORD b, WORD c)
{ WORD q=0, r=0, qn, rn;
  int qneg=0, rneg=0;
  if(c==0) c=1;
  if(a<0) { qneg=!qneg; rneg=!rneg; a = -a; }
  if(b<0) { qneg=!qneg; rneg=!rneg; b = -b; }
  if(c<0) { qneg=!qneg;             c = -c; }
  
  qn = b / c;
  rn = b % c;
  
  while(a)
  { if(a&1) { q += qn;
              r += rn;
              if(r>=c) { q++; r -= c; }
            }
    a  >>= 1;
    qn <<= 1;
    rn <<= 1;
    if(rn>=c) {qn++; rn -= c; }
  }
  result2 = rneg ? -r : r;
  return qneg ? -q : q;
}

WORD dosys(register WORD *p, register WORD *g)
{ register WORD i;
  static char chbuf[256], chbuf2[256]; /* to hold filenames */

  switch((int)(p[3]))
  {  default: printf("\nBad sys %ld\n", (long)p[3]);  return p[3];
  
     case 0:  if(p[4]!=0) printf("\nAbort %ld\n", (long)p[4]);
              exit(p[4]);

     case 10: { WORD ch = Readch();
                if (ch>=0) putchar((char)ch);
                if(ch==13) { ch = 10; putchar(10); }
                fflush(stdout);
                return ch;
              }

     case 11: if(p[4] == 10) putchar(13);
              putchar((char)p[4]);
              fflush(stdout);
              return 0;

     case 12: { FILE *fp = findfp(p[4]);
                char *bbuf = (char *)(p[5]);
                WORD len   = p[6];
                len = fread(bbuf, (size_t)1, (size_t)len, fp);
                return len;
              }

     case 13: { FILE *fp = findfp(p[4]);
                char *bbuf = (char *)(p[5]);
                WORD len = p[6];
                len = WD fwrite(bbuf, (size_t)1, (size_t)len, fp);
                fflush(fp);
                return len;
              }

     case 14: { FILE *fp = fopen(m2c_str(p[4], chbuf), "r");
                if(fp==0) return 0L;
                return newfno(fp);
              }
     case 15: { FILE *fp = fopen(m2c_str(p[4], chbuf), "w");
                if(fp==0) return 0L;
                return newfno(fp);
              }

     case 16: { WORD res = ! fclose(findfp(p[4]));
                freefno(p[4]);
                return res;
              }
     case 17: return ! REMOVE(m2c_str(p[4], chbuf));
     case 18: REMOVE(m2c_str(p[5], chbuf2));
              return ! rename(m2c_str(p[4], chbuf), chbuf2);

     case 21: return ((WORD)(malloc((1+p[4])*BPW)));
     case 22: free((void *)(p[4]));                  return 0;
/*
     case 23: return loadseg(m2c_str(p[4], chbuf));
     case 24: return globin(p[4], g);
     case 25: unloadseg(p[4]);                       return 0;
*/
     case 26: { WORD res =  muldiv(p[4], p[5], p[6]);
                g[Gn_result2] = result2;
                return res;
              }
/*
     case 27: return setraster(p[4], p[5]);
*/
     case 28: return intflag() ? -1L : 0L;

     case 29: return 0; /* was aptovec(f, upb) */

     case 30: /* Return CPU time in milliseconds  */
              return muldiv(clock(), 1000, TICKS_PER_SEC);

     case 31: /* Return time of last modification of file
                 whose name is in p[4]  */
              { struct stat buf;
                if (stat(m2c_str(p[4], chbuf), &buf)) return 0;
                return buf.st_mtime;
              }
  }
} 

/* m2c_str converts the MCPL string for a file name to a C character
** string.  The character '/' (or '\') is treated as a separator and is
** converted to FILE_SEP_CH ('/' for unix, '\' for MSDOS or ':' for MAC)
*/
char *m2c_str(WORD mstr, char * cstr)
{  char *cp = cstr;
   char *mp = (char *)(mstr);
   for(;;)
   { char ch = *mp++;
     if(ch=='/' || ch=='\\') ch = FILE_SEP_CH;
     *cp++ = ch;
     if(ch==0) return cstr;
   }
} 




