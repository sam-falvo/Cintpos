/* This header file contains machine/system dependent #defines
** These are dependent on the -D parameter specified in Makefile.
** The possible -D parameters are:
**
**  -DforMAC           for Apple MAC (not tested)
**  -DforMIPS          for DEC R2000/3000 Workstations under Ultrix 4.3
**  -DforLINUX         for Linux
**  -DforSUN4          for Sun4m under SunOS 4.1.3
**  -DforSPARC         for Sun4m spac under SunOS 5.4
**  -DforALPHA         for DEC Alpha under OSF1 V3.2 17
**  -DforMSDOS         for MSDOS 32 bit protected mode usinf Borland C v4.0
**  -DforCYGWIN32      for Windows 95/98/NT using the GygWin tools
**  -DforOS2           for OS/2 V2.1 using Cset/2 and Borland Tasm
*/

/* The MAC version just about works on a Power Macintosh 7100/66
** using Symantec Think C 6.0
** I used mintmain.c minterp.c mkblib.c and mnullrastlib.c linked
** with the standard libraries ANSI and unix. The partition size was 5000K
** and #define forMAC was edited into this file. (I don't know how to
** get Think C to do that #definition for me).

** The following remarks concern BCPL not MCPL (they will be edited later)
** The command command files bc bs bp bco and bso had to be edited to
** change /s to :s and ../ to :: etc. These versions are in sys/MAC.
** If someone would suggest how to get a visible cursor I would be grateful.
*/

#define SIGNEDCHAR signed char
/* #define SIGNEDCHAR char */

#ifdef forMAC
#include <unix.h>
#include <time.h.h>
#define INT32 long
#define MALLOC(n) malloc((n)<<2)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define MINTASM interpret
#define REMOVE unlink
#define FILE_SEP_CH ':'
#define BIGENDER
#endif

#ifdef forMIPS
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#define INT32 long
#define MALLOC(n) malloc((n)<<2)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define MINTASM interpret
#define REMOVE unlink
#define FILE_SEP_CH '/'
#endif

#ifdef forLINUX
#include <sys/stat.h>
#include <time.h>
#define INT32 long
#define MALLOC(n) malloc((n)<<2)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define MINTASM interpret
#define REMOVE unlink
#define FILE_SEP_CH '/'
#endif

#ifdef forSUN4
#include <sys/stat.h>
#define INT32 long
#define MALLOC(n) malloc((n)<<2)
#define TICKS_PER_SEC (1000000)
#define MINTASM interpret
#define REMOVE unlink
#define FILE_SEP_CH '/'
#define BIGENDER
#endif

#ifdef forSPARC
#include <sys/stat.h>
#include <time.h>
#define INT32 long
#define MALLOC(n) malloc((n)<<2)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define MINTASM interpret
#define REMOVE unlink
#define FILE_SEP_CH '/'
#define BIGENDER
#endif

#ifdef forALPHA
#include <sys/stat.h>
#include <stdlib.h>
#define INT32 int
#define MALLOC(n) malloc((n)<<2)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define MINTASM interpret
#define REMOVE unlink
#define FILE_SEP_CH '/'
#endif

#ifdef forMSDOS
#include <sys\stat.h>
#include <time.h>
#define INT32 long
#define MALLOC(n) malloc((n)<<2)
#define TICKS_PER_SEC (CLK_TCK)
#define MINTASM interpret
#define REMOVE unlink
#define FILE_SEP_CH '\\'
#endif

#ifdef forCYGWIN32
#include <sys/stat.h>
#include <time.h>
#define INT32 long
#define MALLOC(n) malloc((n)<<2)
#define TICKS_PER_SEC (CLK_TCK)
#define MINTASM interpret
#define REMOVE unlink
#define FILE_SEP_CH '\\'
#endif

#ifdef forOS2
#include <sys\stat.h>
#define INT32 long
#define MALLOC(n) malloc((n)<<2)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define MINTASM interpret
#define REMOVE remove
#define FILE_SEP_CH '\\'
#endif

#define PRINTFS printf
#define PRINTFD printf
#define PRINTF printf
#define FILEPT FILE*

typedef INT32 *INT32pt;

#define WD (INT32)
#define UWD (unsigned INT32)
#define PT (INT32 *)
#define BP (unsigned char *)
#define SBP (SIGNEDCHAR *)
#define HP (unsigned short *)
#define SHP (short *)

