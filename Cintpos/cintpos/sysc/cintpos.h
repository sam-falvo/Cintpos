/* This header file contains machine/system dependent #defines.
** It has been modified to allow CURRENT64 and TARGET64 to be set
** automatically. This allows the same C source code to be used
** when compiled in either a 32 or 64 bit architecture and whether
** it implements a 32 or 64 bit version of Cintcode. The 64 bit
** modules such as sysc/cintsys64.c and sysc/cinterp64.c are now
** obsolete and will be deleted in due course.
**
** The possible -D parameters given to the C compiler are as follows:
**
**  -DforMAC           for Apple MAC (not recently tested)
**  -DforMIPS          for DEC R2000/3000 Workstations under Ultrix 4.3
**  -DforSGI           for SGI MIPS machines under Ultrix
**  -DforARM           for ARM under RISC OS (under development)
**  -DforLinux         for Linux on a Pentium
**  -DforLinuxSDL      for Linux on a Pentium with SDL
**  -DforLinuxGL       for Linux on a Pentium with SDL and GL
**  -DforRaspi         for the Raspberry Pi without graphics
**  -DforRaspiSDL      for the Raspberry Pi with SDL graphics
**  -DforRaspiGL       for the Raspberry Pi with OpenGL ES graphics
**  -DforVmsItanium    for the Itanium under VMS
**  -DforVmsVax        for the Vax under VMS
**  -DforLinux64       for 64-bit Linux
**  -DforGP2X          for the GP2X handheld Linux gaming machine
**  -DforLinuxAMD64    for Linux on the AMD 64
**  -DforLinuxPPC      for Linux on a PowerMac G4
**  -DforMacOSPPC      for Mac OS X on a Mac Power PC G4
**  -DforMacOSX        for Mac OS X
**  -DforSUN4          for Sun4m under SunOS 4.1.3
**  -DforSPARC         for Sun4m sparc under SunOS 5.4
**  -DforALPHA         for DEC Alpha under OSF1 V3.2 17
**  -DforMSDOS         for MSDOS 32 bit protected mode usinf Borland C v4.0
**  -DforWin32         for Windows (eg XP) using Microsoft Visual C
**  -DforCYGWIN32      for Windows (eg XP) using GNU Cygnus Solutions
**  -DforBC4           for Windows (eg XP) using Borland C 4.0 and TASM
**  -DforOS2           for OS/2 V2.1 using Cset/2 and Borland Tasm
**  -DforSHwinCE       for WinCE 2.0 (SH3 processor)
*/

/* INT.h is created by mkint-h (source mkint-h.c), it defines
** the macros such as BCPLINT32 and BCPLINT64
*/

#include "INT.h"

/* Conditionally set TARGET64 is the BCPL wordlength is to be 64 bits. */

#if defined(forLinux64) || defined(forLinuxAMD64)
#define TARGET64
#endif

// TARGET64 is defined if we are compiling a 64 bit Cintcode system.

#ifdef TARGET64

/* For 64-bit versions of Cintcode */
#define B2Wsh 3
#define BperW 64
#define BCPLWORD BCPLINT64
#define UBCPLWORD BCPLUINT64
#define BCPLFLOAT FLOAT64
#define FormD FormD64
#define FormX FormX64

#define Cacos acos
#define Casin asin
#define Catan atan
#define Catan2 atan2
#define Ccos cos
#define Csin sin
#define Ctan tan
#define Ccosh cosh
#define Csinh sinh
#define Ctanh tanh
#define Cexp exp
#define Cfrexp frexp
#define Cldexp ldexp
#define Clog log
#define Clog10 log10
#define Cfmod fmodf
#define Cmodf fmod
#define Cpow pow
#define Csqrt sqrt
#define Cceil ceil
#define Cfloor floor

#else

/* For 32-bit versions of Cintcode */
#define B2Wsh 2
#define BperW 32
#define BCPLWORD BCPLINT32
#define UBCPLWORD BCPLUINT32
#define BCPLFLOAT FLOAT32
#define FormD FormD32
#define FormX FormX32

#define Cacos acosf
#define Casin asinf
#define Catan atanf
#define Catan2 atan2f
#define Ccos cosf
#define Csin sinf
#define Ctan tanf
#define Ccosh coshf
#define Csinh sinhf
#define Ctanh tanhf
#define Cexp expf
#define Cfrexp frexpf
#define Cldexp ldexpf
#define Clog logf
#define Clog10 log10f
#define Cfmod fmodf
#define Cmodf modff
#define Cpow powf
#define Csqrt sqrt
#define Cceil ceilf
#define Cfloor floorf

#endif

// Macro to force a BCPLWORD bitpattern to be treated as
// a floating point number of type BCPLFLOAT represented by
// the same bit pattern.
#define N2F *(BCPLFLOAT*)&
// Macro to force a floating point number to be treated as
// a BCPLWORD without changing its bit pattern.
#define F2N *(BCPLWORD*)&

// If SDL or OpenGL is being used the macro names SDLavail, GLavail,
// EGLavail may need to be defined depending on the machine and operating
// system being used.

// For the Pentium Linux these as set as follows:

#ifdef forLinuxSDL
#define SDLavail
#define forLinux
#endif

#ifdef forLinuxGL
#define GLavail
#define SDLavail
#define forLinux
#endif

// For the Raspberry Pi these are set as follows:

#ifdef forRaspi
#define forARM
#endif

#ifdef forRaspiSDL
#define SDLavail
#define forARM
#endif

#ifdef forRaspiGL
#define GLavail
#define EGLavail
#define forARM
#endif


#ifndef forWinCE
#include <stdio.h>
#endif

#ifndef forWIN32
#include <unistd.h>
#endif

#ifndef forWinCE
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#endif

#ifndef forWIN32
#include <sys/time.h>
#endif

#ifndef forWinCE
#include <time.h>
#include <errno.h>
#include <math.h>
#include <pthread.h>
#endif

/* The MAC version just about works on a Power Macintosh 7100/66
** using Symantec Think C 6.0
** I used cintsys.c cinterp.c kblib.c and nullrastlib.c linked
** with the standard libraries ANSI and unix. The partition size was 5000K
** and #define forMAC was edited into this file. (I don't know how to
** get Think C to do that #definition for me).
** The command command files bc bs bp bco and bso had to be edited to
** change /s to :s and ../ to :: etc. These versions are in sys/MAC.
** If someone would suggest how to get a visible cursor I would be grateful.
*/

/*
** Cintsys/Cintpos and cinterp need both the type signed char and
** unsigned char but these are system depended. The file sysc/INT.h
** created by mkint-h.c define the macros BCPLCHAR and UBCPLCHAR
** for these.
*/

/* Note that
**   #define CINTASM cintasm        will use a hand written cintcode
**                                  interpreter in addition to the
**                                  one implemented in C.
**   #define CINTASM interpreter    will only use the interperter
**                                  written in C, this typically runs
**                                  2-3 times slower.
*/

/*
** CINTASM is now always defined to be cintasm. The function cintasm
** is either defined in files such as sysasm/LINUX/cintasm.s of
** in cinterp.c when compiled with FASTyes defined.
*/
#define CINTASM cintasm

/*
MMAP is defined if mmap is used to allocate Cintcode memory
*/

#ifdef forMAC
#include <unix.h>
#include <time.h>
#include <sys/timeb.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define BIGENDER
#endif

#ifdef forMIPS
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <sys/timeb.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#endif

#ifdef forSGI
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <sys/timeb.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#endif

#ifdef forARM
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <sys/timeb.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define MMAP
#endif

#ifdef forLinux
#include <sys/stat.h>
#include <time.h>
#include <fcntl.h>
//#include <sys/wait.h>
#include <sys/time.h>
/* #include <sys/timeb.h> */
#ifdef SOUND
#include <sys/ioctl.h>
#include <sys/unistd.h>
#include <sys/fcntl.h>
#include <sys/soundcard.h>
#endif
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define MMAP
#endif

#ifdef forLinux64
#include <sys/stat.h>
#include <time.h>
#include <fcntl.h>
// #include <sys/wait.h>
#include <sys/time.h>
/* #include <sys/timeb.h> */
#ifdef SOUND
#include <sys/ioctl.h>
#include <sys/unistd.h>
#include <sys/fcntl.h>
#include <sys/soundcard.h>
#endif
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define MMAP
#endif

#ifdef forVmsItanium
#define VMSNAMES
#include <stat.h>
#include <time.h>
#include <fcntl.h>
#include <wait.h>
#include <time.h>
#include <timeb.h>
#include <unistd.h>
#include <inet.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (1000)
#define REMOVE unlink
#define VMSNAMES
#undef CINTASM
#define CINTASM interpret
typedef unsigned int socklen_t;
#endif

#ifdef forVmsVax
#define VMSNAMES
#include <stat.h>
#include <time.h>
#include <fcntl.h>
#include <wait.h>
#include <time.h>
#include <timeb.h>
#include <unistd.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (1000)
#define REMOVE unlink
#define VMSNAMES
#endif

#ifdef forGP2X
#include <sys/stat.h>
#include <time.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/timeb.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#endif

#ifdef forLinuxAMD64
#include <sys/stat.h>
#include <time.h>
#include <sys/timeb.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define MMAP
#endif

#ifdef forMacOSPPC
#include <sys/stat.h>
#include <time.h>
#include <sys/timeb.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define BIGENDER
#endif

#ifdef forMacOSX
#include <sys/stat.h>
#include <time.h>
#include <sys/timeb.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define MMAP
#endif

#ifdef forCYGWIN32
#include <sys/stat.h>
#include <time.h>
#include <sys/timeb.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#endif

#ifdef forLinuxPPC
#include <sys/stat.h>
#include <time.h>
#include <sys/timeb.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define BIGENDER
#endif

#ifdef forSUN4
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/timeb.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (1000000)
#define REMOVE unlink
#define UNIXNAMES
#define BIGENDER
#endif

#ifdef forSPARC
#include <sys/stat.h>
#include <time.h>
#include <sys/timeb.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define BIGENDER
#endif

#ifdef forALPHA
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/timeb.h>
#include <stdlib.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#endif

#ifdef forMSDOS
#include <sys\stat.h>
#include <time.h>
#include <sys/timeb.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLK_TCK)
#define REMOVE unlink
#define WINNAMES
#endif

#ifdef forBC4
#include <sys\stat.h>
#include <time.h>
#include <sys/timeb.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLK_TCK)
#define REMOVE unlink
#define WINNAMES
#endif

#ifdef forOS2
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/timeb.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE remove
#define WINNAMES
#endif

#ifdef forWIN32
#include <sys/stat.h>
#include <time.h>
#include <sys/timeb.h>
#include <windows.h>
#include <mmsystem.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLK_TCK)
#define REMOVE _unlink
#define tzset _tzset
#define WINNAMES
#endif

#ifdef forSHwinCE
#define forWinCE
#endif

#ifdef forWinCE
#undef BCPLWORD
#define BCPLWORD long
#define MALLOC(n) LocalAlloc(LPTR, (n)<<B2Wsh)
#define TICKS_PER_SEC 1000
#define REMOVE unlink
#define WINNAMES
/*#include <sys\stat.h> */
/*#include <time.h> */
#include <windows.h>         /* For all that Windows stuff */
#include <commctrl.h>        /* Command bar includes */
#include "../sysasm/shWinCE/ceBCPL.h" /* Program-specific stuff */
#include "Objidl.h"
#include <stdlib.h>

#define PRINTFS printfs
#define PRINTFD printfd
#define PRINTF(s) printfd(s, 0)
#define FILEPT HANDLE

/* Unix style library declarations */

#define FILEPT HANDLE

#define SEEK_SET FILE_BEGIN
#define SEEK_END FILE_END

int fclose(FILEPT fp);
int fgetc(FILEPT fp);
void putchar(char ch);
void fflush(FILEPT fp);
FILEPT fopen(char *, char *);
int fread(char *buf, size_t size, size_t len, FILEPT fp);
int fwrite(char *buf, size_t size, size_t len, FILEPT fp);
int fseek(FILEPT fp, long pos, int method);
int ftell(FILEPT fp);
int unlink(char *);
int rename(char *, char *);
int clock();
char *getenv(char *);
int main();

FILEPT stdout=0;

#define EOF -1

#else

// For all systems other than WinCE
#define PRINTFS printf
#define PRINTFD printf
#define PRINTF printf
#define FILEPT FILE*

#endif

#ifdef MMAP
#include <sys/mman.h>
#endif

void trpush(BCPLWORD val);

//typedef BCPLWORD *BCPLWORDpt;

#define WD (BCPLWORD)
#define UWD (UBCPLWORD)
#define PT (BCPLWORD *)
#define BP (UBCPLCHAR *)
#define SBP (BCPLCHAR *)
#define HP (unsigned short *)
#define SHP (short *)

#define Gn_sys         3
#define Gn_currco      7
#define Gn_colist      8
#define Gn_rootnode    9
#define Gn_result2    10

#define bootregs  11
#define klibregs  21
#define saveregs  31
#define isrregs   41

#define rootnode 100

#define Rtn_tasktab         0L
#define Rtn_devtab          1L
#define Rtn_tcblist         2L
#define Rtn_crntask         3L
#define Rtn_blklist         4L
#define Rtn_tallyv          5L
#define Rtn_clkintson       6L
#define Rtn_lastch          7L
#define Rtn_insadebug       8L
#define Rtn_bptaddr         9L
#define Rtn_bptinstr       10L
#define Rtn_dbgvars        11L
#define Rtn_clwkq          12L
#define Rtn_membase        13L
#define Rtn_memsize        14L
#define Rtn_info           15L
#define Rtn_sys            16L
#define Rtn_boot           17L
#define Rtn_klib           18L
#define Rtn_blib           19L
#define Rtn_keyboard       20L
#define Rtn_screen         21L
#define Rtn_vecstatsv      22L
#define Rtn_vecstatsvupb   23L
#define Rtn_intflag        24L
#define Rtn_dumpflag       25L
#define Rtn_envlist        26L
#define Rtn_abortcode      27L
#define Rtn_context        28L
#define Rtn_lastp          29L
#define Rtn_lastg          30L
#define Rtn_lastst         31L
#define Rtn_idletcb        32L
#define Rtn_adjclock       33L
#define Rtn_dcountv        34L
#define Rtn_rootvar        35L
#define Rtn_pathvar        36L
#define Rtn_hdrsvar        37L
#define Rtn_scriptsvar     38L
#define Rtn_boottrace      39L
#define Rtn_days           40L
#define Rtn_msecs          41L
#define Rtn_ticks          42L
#define Rtn_mc0            43L
#define Rtn_mc1            44L
#define Rtn_mc2            45L
#define Rtn_mc3            46L
#define Rtn_system         47L
#define Rtn_icount         48L
#define Rtn_joystickfd     49L
#define Rtn_joybuttoncount 50L
#define Rtn_joyaxiscount   51L
#define Rtn_joycurrbuttons 52L
#define Rtn_joybuttons     53L
#define Rtn_joyaxis0       54L
#define Rtn_joyaxis1       55L
#define Rtn_joyaxis2       56L
#define Rtn_joyaxis3       57L
#define Rtn_joyaxis4       58L
#define Rtn_joyaxis5       59L
#define Rtn_joyaxis6       60L

#define Rtn_upb            80L


#define Tcb_namebase       19L /* Space for upto 15 chars of task name */


/* SYS functions */

#define Sys_setcount      (-1)
#define Sys_quit            0
#define Sys_rti             1
#define Sys_saveregs        2
#define Sys_setst           3
#define Sys_tracing         4
#define Sys_watch           5
#define Sys_tally           6
#define Sys_interpret       7

#define Sys_sardch         10
#define Sys_sawrch         11
#define Sys_read           12
#define Sys_write          13
#define Sys_openread       14
#define Sys_openwrite      15
#define Sys_close          16
#define Sys_deletefile     17
#define Sys_renamefile     18
#define Sys_openappend     19

#define Sys_getvec         21
#define Sys_freevec        22
#define Sys_loadseg        23
#define Sys_globin         24
#define Sys_unloadseg      25
#define Sys_muldiv         26
#define Sys_intflag        28
#define Sys_setraster      29
#define Sys_cputime        30
#define Sys_filemodtime    31
#define Sys_setprefix      32
#define Sys_getprefix      33
#define Sys_graphics       34       /* Windows CE only */

#define Sys_seek           38
#define Sys_tell           39
#define Sys_waitirq        40
#define Sys_lockirq        41
#define Sys_unlockirq      42
#define Sys_devcom         43
#define Sys_datstamp       44

#define Sys_filesize       46
#define Sys_openreadwrite  47
#define Sys_getsysval      48
#define Sys_putsysval      49
#define Sys_shellcom       50
#define Sys_getpid         51
#define Sys_dumpmem        52
#define Sys_callnative     53
#define Sys_platform       54
#define Sys_inc            55
#define Sys_buttons        56
#define Sys_delay          57
#define Sys_sound          58
#define Sys_callc          59
#define Sys_trpush         60
#define Sys_settrcount     61
#define Sys_gettrval       62
#define Sys_flt            63
#define Sys_pollsardch     64
#define Sys_incdcount      65
#define Sys_sdl            66
#define Sys_gl             67
#define Sys_ext            68
#define Sys_joy            69

#define fl_avail  0
#define fl_mk     1
#define fl_unmk   2
#define fl_float  3
#define fl_fix    4
#define fl_abs    5
#define fl_mul    6
#define fl_div    7
#define fl_mod    8
#define fl_add    9
#define fl_sub   10
#define fl_pos   11 
#define fl_neg   12
#define fl_eq    13
#define fl_ne    14
#define fl_ls    15
#define fl_gr    16
#define fl_le    17
#define fl_ge    18

#define fl_acos  20
#define fl_asin  21
#define fl_atan  22
#define fl_atan2 23
#define fl_cos   24
#define fl_sin   25
#define fl_tan   26
#define fl_cosh  27
#define fl_sinh  28
#define fl_tanh  29
#define fl_exp   30
#define fl_frexp 31
#define fl_ldexp 32
#define fl_log   33
#define fl_log10 34

#define fl_pow   36
#define fl_sqrt  37
#define fl_ceil  38
#define fl_floor 39
#define fl_modf  40  // Modified from fmod 14/5/18

#define fl_N2F   41
#define fl_F2N   42
#define fl_radius2   43
#define fl_radius3   44

// The DCB structure
#define Dcb_type       0
#define Dcb_devid      1
#define Dcb_wkq        2
#define Dcb_op         3
#define Dcb_arg        4
#define Dcb_threadp    5
#define Dcb_cvp        6
#define Dcb_intson     7
#define Dcb_irq        8
#define Dcb_var0       9
#define Dcb_flag      10
#define Dcb_var1      11
#define Dcb_var2      12
#define Dcb_var3      13
#define Dcb_var4      14


// Device types
#define Devt_clk       1
#define Devt_ttyin     2
#define Devt_ttyout    3
#define Devt_fileop    4
#define Devt_tcpdev    5

// Device commands
#define Devc_create    1
#define Devc_destroy   2
#define Devc_start     3
#define Devc_stop      4
#define Devc_setintson 5

// Packet structure
#define Pkt_link    0
#define Pkt_id      1
#define Pkt_type    2
#define Pkt_res1    3
#define Pkt_res2    4
#define Pkt_arg1    5
#define Pkt_arg2    6
#define Pkt_arg3    7
#define Pkt_arg4    8

// Packet types for TCP devices
#define Tcp_name2ipaddr  1
#define Tcp_name2port    2
#define Tcp_socket       3
#define Tcp_reuseaddr    4
#define Tcp_sndbufsz     5
#define Tcp_rcvbufsz     6
#define Tcp_bind         7
#define Tcp_connect      8
#define Tcp_listen       9
#define Tcp_accept      10
#define Tcp_recv        11
#define Tcp_send        12
#define Tcp_close       13
