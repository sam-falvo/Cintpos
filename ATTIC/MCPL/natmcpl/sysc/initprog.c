#include <stdio.h>
#include "mcplsys.h"

WORD stackupb=10000, gvecupb=1000;
extern WORD *stackbase, *globbase;

/* MCPL sections */
extern prog(WORD *g);

void initsections(void)
{ int i;
  /* Initialise MCPL sections (initialises globals) */
  mlib(globbase); 
/* printf("calling prog\n"); */
  prog(globbase);

/*  for(i=0;i<4; i++) printf("G%d = %16x\n", i, globbase[i]); */

  return;
}
