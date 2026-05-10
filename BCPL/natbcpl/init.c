/* Initialisation file written by MakeInit version 2.0  */
#include "bcpl.h"

int stackupb=50000;

int gvecupb=1000;

/* BCPL sections  */
extern BLIB(BCPLWORD *g); 	/* file (run-time library)  */
extern DLIB(BCPLWORD *g); 	/* file (system dependent library)  */
extern t0(BCPLWORD *g); 	/* file t0.b  */

void initsections(BCPLWORD *g) {
       BLIB(g); 	/* file (run-time library)  */
       DLIB(g); 	/* file (system dependent library)  */
       t0(g); 	/* file t0.b  */

       return;
}
