/* rastlib generates a relatively compact representation of
** raster lines using run length encoding.
**
** K1000 S12          1000 instruction per raster line, scale 12
** W10B3W1345B1N      10 white 3 black 1345 white 1 black newline
** W13B3W12B2N        etc
** ...
** Alternatively it generates a bit stream file corresponding to
** the fifth bit of the address of every memory location accessed.
*/

#include <stdio.h>
#include <stdlib.h>

/* cintsys.h contains machine/system dependent #defines  */
#include "cintsys.h"

/* Upb of address references buffer */
#define UPB 50000

static FILE *rasterfile;
static BCPLWORD addr[UPB+1], addp;
//static BCPLWORD p=0;
static BCPLWORD count=1000, scale=12, fcounttrig;
BCPLWORD fcount;
static BCPLWORD sound=0;
static unsigned char bitpos;
static unsigned char bits;

extern char *b2c_str(BCPLWORD bstr, char *cstr);
extern char *osfname(char *name, char *osname);

static void wrline(void);

static int initraster(char *filename)
{ //printf("initraster(%s), sound=%d\n", filename, sound);
  if (sound) {
    rasterfile = fopen(filename, "wb");
    if (rasterfile==0) return 0;
    printf("Bit stream file %s opened\n", filename);
    return 1;
  } else {
    fcounttrig = count;
    addp = 0;
    fcount = 0;
    //printf("initraster: openning file %s\n",filename);
    rasterfile = fopen(filename, "w");
    if (rasterfile==0) return 0;
    //printf("Raster file %s opened, count=%d scale=%d\n", filename, count, scale);
    fprintf(rasterfile, "K%"FormD" S%"FormD"\n", count, scale);
    return 1;
  }
}


static int endraster(void)
{ if (rasterfile)
    { if (sound==0) wrline();
    return fclose(rasterfile); /* Return 0 if successful */
  }
  return -1;
}

BCPLWORD setraster(BCPLWORD n, BCPLWORD val)
{ // n=0      Open the raster file
  // n=1      Set count=val
  // n=2      Set scale=val
  // n=3      Return 0 if rastering is available
  // n=4      val=1 generate a bit stream sound file
  //          val=0 generate a raster file

  char chbuf1[256];
  char chbuf2[256];
  //printf("setraster: n=%d\n", n);
  switch((int)n)
  { case 0: /* Only valid after sound set to 1 or count and scale set.
               Specify the raster file and start rastering.
            */
            if (val) 
            { char *name = b2c_str(val, chbuf1);
	      //printf("Calling initraster(%s)\n", osfname(name, chbuf2));
              return initraster(osfname(name, chbuf2));
            } else {
              return 1; // No filename
            }
    case 1: if(val>=0) count = val; return count;
    case 2: if(val>=0) scale = val; return scale;
    case 3: return 1;    /* Rastering is available */
    case 4: sound = val; /* sound=1 for sound generation
                            sound=0 for raster generation
                         */
            if (sound) {
              bitpos = 1; /* Initialis the bit stream variables */
              bits  = 0;
	    }
            return 0;
    case 5: return endraster(); /* Return 0 if successful */
  }
  return 0;
}

void sort(BCPLWORD p, BCPLWORD q)
{ if(p<q)
  { BCPLWORD t;
    BCPLWORD m = addr[(p+q)>>1];
    BCPLWORD i=p, j=q;
    while(1)
    { while(addr[i]<m) i++;
      /* all k in p..i-1 => addr[k]<m
      ** addr[i] >= m
      */
      while(j>i && addr[j]>=m) j--;
      /* all k in j+1 .. q => addr[k]>=m
      ** j>=i
      */
      if(i==j) break;
      /* j>i and addr[i]>m and addr[j]<m
      */
      t = addr[i];
      addr[i] = addr[j];
      addr[j] = t;
      /* j>i and addr[i]<m and addr[j]>m
      */
      i++;
    }
    sort(p, i-1);
    j = q;
    while(1)
    { while(i<=j && addr[i]==m) i++;
      while(addr[j]!=m) j--;
      if (j<i) break;
      addr[j] = addr[i];
      addr[i] = m;
      i++;
    }
    sort(i, q);
  }
}

static void wrline(void)
{ BCPLWORD i=1, k, a=0, b;
  sort(1, addp);
  while(i<=addp && addr[i]<0) i++;
  
  while(i<=addp)
  { 
    b = addr[i++]; /* addr of next black */
    k = b-a;       /* white count, possibly zero */
    fprintf(rasterfile, "W%ld", (long)k);
    a = b; /* start of next black region */
    /* find next white */
    b++;
    while (i<=addp && addr[i]<=b) b=addr[i++]+1; 
    k = b-a;
    a = b; /* start of next white region */
    fprintf(rasterfile, "B%ld", (long)k);
  }
  fprintf(rasterfile, "N\n");
  addp = 0;
  fcounttrig += count;
  if(fcount%1000000==0) printf("fcount = %ld\n", (long)fcount);
}

void rasterpoint(BCPLWORD p)
{ if (sound) {
    /* Test the fifth bit of every accessed address */
    if ((p & 0x10) > 0) bits += bitpos;
    if (bitpos>=128) {
      /* Output the current 8-bit bit pattern. */
      fputc(bits, rasterfile);
      bitpos = 1;
      bits = 0;
    } else {
      bitpos = bitpos+bitpos;
    }
  } else {
    BCPLWORD a = p/scale;
    if (addp<UPB) addr[++addp] = a;
/*  printf("%7ld %7ld\n", (long)addp, (long)a);*/
    if (fcount >= fcounttrig) wrline();
  }
}





