/*
This header file contains machine/system dependent #defines.
It has been modified to allow CURRENT64 and TARGET64 to be set
automatically. This allows the same C source code to be used
when compiled on either a 32 or 64 bit architecture and whether
it implements a 32 or 64 bit version of Cintcode. The 64 bit
modules such as sysc/cintsys64.c and sysc/cinterp64.c are now
obsolete and will be deleted in due course.

The possible -D parameters given to the C compiler are as follows:

  -DforLinux         for Linux on a Pentium
  -DforiSH           for Alpine Linux on the iSH app for iPad and iPhone
  -DforLinuxSDL      for Linux on a Pentium with SDL
  -DforLinuxGL       for Linux on a Pentium with SDL and GL
  -DforLinuxSDL2GL   for Linux on a Pentium with SDL2 and GL
  -DforRaspi         for the Raspberry Pi without graphics
  -DforRaspiSDL      for the Raspberry Pi with SDL graphics
  -DforRaspiGL       for the Raspberry Pi with OpenGL ES graphics
  -DforVmsVax        for the Vax under VMS
  -DforMacOSX        for Mac OSX
  -DforWin32         for Windows (eg XP) using Microsoft Visual C ???
  -DforCYGWIN        for Windows (eg XP) using GNU Cygnus Solutions

  -DforARM           for ARM Linux without any graphics libraries
  -DforLinux64       for 64 bit BCPL without graphics

Other #defines

  -DCALLC
  -DSOUND
  -DSDLavail
  -DSDL2avail
  -DEXTavail
  -DJSavail

  -DCINTSYSyes
  -DRASTERPyes
  -DFASTERPyes
  -DCINTERPyes
  -DTARGET64

*/

// defines.h is created by mkdefines-h (source mkdefines-h.c), it defines
// the macros such as BCPLINT32 and BCPLINT64

#include "defines.h"

// Conditionally set TARGET64 is the BCPL wordlength is to be 64 bits.

#ifdef forLinux64
#define TARGET64
#endif


#ifdef TARGET64

// For 64-bit versions
#define B2Wsh 3
#define BperW 64
#define BCPLWORD BCPLINT64
#define UBCPLWORD BCPLUINT64
#define BCPLFLOAT FLOAT64

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

// For 32-bit versions of Cintcode
#define B2Wsh 2
#define BperW 32
#define BCPLWORD BCPLINT32
#define UBCPLWORD BCPLUINT32
#define BCPLFLOAT FLOAT32

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

// Macro to force a suitably aligned BCPLWORD bitpattern to be
// treated as a floating point number of type BCPLFLOAT of the
// same length represented by the same bit pattern. The type
// conversion has zero cost.
///#define N2F(x) (*(BCPLFLOAT*)&(x))
// Macro to force a floating point number to be treated as
// a BCPLWORD of the same length without changing its bit pattern.
///#define F2N(x) (*(BCPLWORD*)&(x))
// Both of the above macros can only be applied to expressions
// corresponding to locations in memory.

// Replaced both macros by the functions N2F and F2N defined and
// only used in cintsys.c and cintpos.c.

// If SDL or OpenGL is being used the macro names SDLavail, GLavail,
// EGLavail may need to be defined depending on the machine and operating
// system being used.

//#define F2N(x) F2Nfn(x)
//#define N2F(x) N2Ffn(x)

/*

Systems sometimes disagree whether the C type is signed or unsigned. To resolve
this problem the file sysc/defines.h (created by mkdefines-h.c) defines
the macros BCPLCHAR and UBCPLCHAR appropriately.


The function cintasm used to be implemented in hand written assembly
language but is now defined in cinterp.c when compiled with with
either FASTERPyes or RASTERyes defined.
*/

// MMAP is defined if mmap is used to allocate Cintcode memory

#ifdef forLinux
//#include "defines.h     This was included at the start of this file.
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <math.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>
#include <sys/time.h>

#ifdef SDLavail
#include <SDL/SDL.h>
#endif

#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define MMAP

#ifdef SOUND
#include <sys/ioctl.h>
#include <sys/fcntl.h>
#include <sys/soundcard.h>
#endif

#endif

// For Alpine Linux using the app iSH on an iPad or iPhone
#ifdef forLinuxiSH
#include "defines.h"
// The macro names ALSAavail, SDLavail and SDLavail are not #defined.
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <math.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>
#include <sys/time.h>

#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define MMAP

#ifdef SOUND
#include <sys/ioctl.h>
#include <sys/fcntl.h>
#include <sys/soundcard.h>
#endif

#endif

#ifdef forLinuxSDL
#define SDLavail
#endif

#ifdef SDLavail
#include "defines.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <math.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>

#include <SDL/SDL.h>
#include <wchar.h>  /* for the function wmemset */

#include <fcntl.h>
//#include <sys/wait.h>

#ifdef SOUND
#include <sys/ioctl.h>
#include <sys/fcntl.h>
#include <sys/soundcard.h>
#endif

#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define MMAP
#endif

#ifdef forLinuxGL
#define SDLavail
#define GLavail
#endif

#ifdef GLavail
#include "defines.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <math.h>
#include <sys/stat.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>

#include <SDL/SDL.h>
#include <wchar.h>  /* for the function wmemset */

#include <fcntl.h>
//#include <sys/wait.h>
/* #include <sys/timeb.h> */
#include <GL/gl.h>
#ifdef SOUND
#include <sys/ioctl.h>
#include <sys/fcntl.h>
#include <sys/soundcard.h>
#endif
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define MMAP
#endif



#ifdef forRaspi
#include "defines.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <math.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>
#include <time.h>
#include <fcntl.h>

#ifdef SDLavail
#include <SDL/SDL.h>
#endif

#ifdef SOUND
#include <sys/ioctl.h>
//#include <sys/unistd.h>
#include <sys/fcntl.h>
#include <sys/soundcard.h>
#endif

#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define MMAP
#endif

#ifdef forRaspiSDL
#define SDLavail
#include "defines.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <math.h>
#include <sys/stat.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include <SDL/SDL.h>
#include <wchar.h>  /* for the function wmemset */

#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define MMAP
#endif

#ifdef forRaspiGL
#define SDLavail
#define EGLavail
#define GLavail
#include "defines.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <unistd.h>

#include "bcm_host.h"

#include "GLES2/gl2.h"
#include "EGL/egl.h"
#include "EGL/eglext.h"

#include <signal.h>
#include <errno.h>
#include <sys/stat.h>
#include <time.h>

#include <SDL/SDL.h>
#include <wchar.h>  /* for the function wmemset */


#include <fcntl.h>
#include <sys/time.h>

#ifdef SOUND
#include <sys/ioctl.h>
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
// #include <sys/timeb.h>
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


#ifdef forMacOSX
#include "defines.h"
#include <sys/stat.h>
#include <sys/time.h>
#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define MMAP
#endif

#ifdef forCYGWIN
#include "defines.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <math.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/time.h>
#define MALLOC(n) malloc((n)<<B2Wsh)
#define TICKS_PER_SEC (CLOCKS_PER_SEC)
#define REMOVE unlink
#define UNIXNAMES
#define MMAP

#ifdef SOUND
#include <sys/ioctl.h>
#include <sys/fcntl.h>
#include <sys/soundcard.h>
#endif

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

#define FILEPT FILE*

#ifdef MMAP
#include <sys/mman.h>
#endif

void trpush(BCPLWORD val);

typedef BCPLWORD *BCPLWORDpt;

#define WD (BCPLWORD)
#define UWD (UBCPLWORD)
#define PT (BCPLWORD *)
#define BP (UBCPLCHAR *)
#define SBP (BCPLCHAR *)
#define HP (unsigned short *)
#define SHP (short *)

#define LL (long long)
// The length of long long is 64 bits under both Linux and Windows
// LL is mainly used in printf calls.

#define Gn_sys         3
#define Gn_currco      7
#define Gn_colist      8
#define Gn_rootnode    9
#define Gn_result2    10

#define bootregs  11
#define klibregs  21
#define saveregs  31
#define isrregs   41

#define mainloopregs 51
// mainloopregs is used by mainLoop functions such as
// glutMainLoop in OpenGL. It holds the Cintcode registers
// at the time when eg glutMainLoop is called during the
// execution of sys*Sys_gl, gl_glutMainLoop).

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
#define Rtn_sysp           29L
#define Rtn_sysg           30L
#define Rtn_sysst          31L
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
//#define Rtn_icount         48L
#define Rtn_icountmax      48L
#define Rtn_joystickfd     49L
#define Rtn_joystickfd1    50L
#define Rtn_joybuttoncount 51L
#define Rtn_joyaxiscount   52L
#define Rtn_joycurrbuttons 53L
#define Rtn_joybuttons     54L
#define Rtn_joyaxis0       55L
#define Rtn_joyaxis1       56L
#define Rtn_joyaxis2       57L
#define Rtn_joyaxis3       58L
#define Rtn_joyaxis4       59L
#define Rtn_joyaxis5       60L
#define Rtn_joyaxis6       61L

#define Rtn_hostaddrsize   62L
#define Rtn_gvecsize       63L

// The following are for the new implementatin of tty input.
// boot allocates ttyinbuf withupb=255
#define Rtn_ttyinwkq       64L
#define Rtn_ttyinwkqe      65L

#define Rtn_quietflag      66L
#define Rtn_fast           67L

#define Rtn_upb            80L

#define Rtn_stderr         81L

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
#define Sys_glutMainLoop    8
// Sys_glutMainLoop calls the OpenGL function gl_glutMainLoop
// after saving the Cintcode registers in mainloopregs for
// use by registered BCPL callback fuctions. Callback functions
// must not return normally but instread call quit(0, -15).

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
#define Sys_settracing     70
#define Sys_getbuildno     71
#define Sys_alsa           72
#define Sys_memmovewords   73
#define Sys_memmovebytes   74
#define Sys_errwrch        75

#define bld_Unknown         0

#define bld_Linux           1
#define bld_LinuxSDL        2
#define bld_LinuxSDL2       3
#define bld_LinuxGL         4
#define bld_LinuxSDLGL      5
#define bld_LinuxSDL2GL     6
#define bld_LinuxiSH        7

#define bld_Raspi          21
#define bld_RaspiSDL       22
#define bld_RaspiSDLGL     23
#define bld_RaspiSDL2GL    24

#define bld_MacOSX         31
#define bld_MacOSXSDL      32
#define bld_MacOSXSDL2     33
#define bld_MacOSXSDLGL    34
#define bld_MacOSXSDL2GL   35

#define bld_VmsVax         41
#define bld_Win32          42
#define bld_CYGWIN         43


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
#define fl_64to32    45 // Convert 64 to 32 bit floating point, only
                        // used when running under 64 bit BCPL.
#define fl_32to64    46 // Convert 32 to 64 bit floating point, only
                        // used when running under 32 bit BCPL.

#define sf_none    0    // Assignment operators
#define sf_vecap   1
#define sf_fmul    2
#define sf_fdiv    3
#define sf_fmod    4    // Incompatible change 26/11/18
#define sf_fadd    5
#define sf_fsub    6
#define sf_mul     7
#define sf_div     8
#define sf_mod     9
#define sf_add    10
#define sf_sub    11
#define sf_lshift 12
#define sf_rshift 13
#define sf_logand 14
#define sf_logor  15
#define sf_eqv    16
#define sf_xor    17

// The DCB structure
#define Dcb_type       0
#define Dcb_devid      1
#define Dcb_wkq        2
#define Dcb_op         3
#define Dcb_arg        4
#define Dcb_irq        5
#define Dcb_intson     6
#define Dcb_flag       7
#define Dcb_var0       8
#define Dcb_var1       9
#define Dcb_var2      10
#define Dcb_var3      11
#define Dcb_var4      12
#define Dcb_threadp   14
#define Dcb_cvp       16


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

// Functions defined in cintsys.c or cintpos.c
// These are accessible to cinterp.c, sdlfn.c, glfn.c, etc

extern void copyaddrB2C(void*from, void*to);
extern void copyaddrC2B(void*from, void*to);

extern BCPLWORD dosys(BCPLWORD p, BCPLWORD g);
extern BCPLWORD doflt(BCPLWORD op, BCPLWORD a, BCPLWORD b, BCPLWORD c);
extern BCPLWORD muldiv(BCPLWORD a, BCPLWORD b, BCPLWORD c);

extern void wrcode(char *form, BCPLWORD f, BCPLWORD a); 
extern void wrfcode(BCPLWORD f);
extern void trace(BCPLWORD pc, BCPLWORD p, BCPLWORD a, BCPLWORD b);
extern BCPLWORD timestamp(BCPLWORD *datstamp);

extern void joyscan(int fd, BCPLWORD *g, BCPLWORD *W);


