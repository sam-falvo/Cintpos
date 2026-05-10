/* this module defines the machine dependent keyboard interface

   int Readch(void)     returns the ASCII code for the next key pressed
                        without echo.
   int pollReadch(void) returns the next character or -3 if none available.
   int init_keyb(void)  initialises the keyboard interface.
   int close_keyb(void) restores the keyboard to its original state.
   int intflag(void)    returns 1 if interrupt key combination pressed.

Following Colin Liebenrood's suggestion (for LINUX),

   init_keyb return 1 is stdin is a tty, 0 otherwise
and
   Readch() return endstreamch if the stdin is exhausted or ^D read.
*/


#include <stdio.h>
#include <stdlib.h>


/* cintmain.h contains machine/system dependent #defines  */
#include "cintmain.h"

#if defined(forMIPS) || defined(forSUN4) || defined(forALPHA) || \
    defined(forLinuxPPC) || defined(forMacOSPPC) || defined(forMacOSX)
#include <sys/ioctl.h>
#include <sgtty.h>

int init_keyb(void)
{ struct sgttyb ttyb;

  ioctl(0, TIOCGETP, &ttyb);
  ttyb.sg_flags = CBREAK+EVENP+ODDP+CRMOD;
  ioctl(0, TIOCSETP, &ttyb);
  return 0;
}

int close_keyb(void)
{ struct sgttyb ttyb;
  ioctl(0, TIOCGETP, &ttyb);
  ttyb.sg_flags = ECHO+EVENP+ODDP+CRMOD;
  ioctl(0, TIOCSETP, &ttyb);
  return 0;
}

int Readch(void)
{ return getchar();
}

int pollReadch(void)
{ return Readch();
}

int intflag(void)
{ return 0;
}
#endif


#if defined(forLinux)||defined(forCYGWIN)||defined(forSPARC)||\
  defined(forGP2X)||defined(forLinuxAMD64)||defined(forARM)||\
  defined(forLinux64)||defined(forLinuxSDL)||defined(forLinuxGL)||\
  defined(forRaspi)||defined(forRaspiSDL)||defined(forRaspiGL)||\
  defined(forLinuxiSH)
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <errno.h>

/* Use this variable to remember original terminal attributes.  */
     
struct termios saved_attributes;
     
void
reset_input_mode (void)
{
  tcsetattr (STDIN_FILENO, TCSANOW, &saved_attributes);
}
     
void
set_input_mode (void)
{
  struct termios tattr;

  if (!isatty(STDIN_FILENO)) return;  
   
  /* Save the terminal attributes so we can restore them later.  */
  tcgetattr (STDIN_FILENO, &saved_attributes);
  atexit (reset_input_mode);

  /* Set the funny terminal modes.  */
  tcgetattr (STDIN_FILENO, &tattr);
  tattr.c_lflag &= ~(ICANON|ECHO); /* Clear ICANON and ECHO.   */
  tattr.c_cc[VMIN] = 1;
  tattr.c_cc[VTIME] = 0;
  tcsetattr (STDIN_FILENO, TCSAFLUSH, &tattr);
}
     
int Readch()
{ char ch;
  int rc = read(STDIN_FILENO, &ch, 1);
  if(rc==0) ch = -1;
  //if(rc==0) 
  //printf("rc=%d ch=%3d errno=%d\n", rc, ch, errno);
  return ch;
}

int pollReadch(void)
{ struct timeval tv;
  fd_set read_fd;
  int rc=0;
  tv.tv_sec  = 0;
  tv.tv_usec = 0;
  FD_ZERO(&read_fd);
  FD_SET(0, &read_fd);
  rc=select(1, &read_fd, 0, 0, &tv);
  if(rc==0) return -3; // pollingch
  if(rc>0 && FD_ISSET(0, &read_fd)) return Readch();
  return -1; // Error or EOF
}

int init_keyb(void)
{ set_input_mode();
  return 0;
}

int close_keyb(void)
{ return 0;
}

int intflag(void)
{ return 0;
}
#endif

#ifdef forMAC
#include <console.h>

int Readch(void)
{ int ch = EOF;
  while (ch==EOF) ch = getchar(); /* horrible!!!! */
  return ch;
}

int pollReadch(void)
{ return Readch();
}

int init_keyb(void)
{ console_options.title = "\pBCPL Cintcode";
  console_options.pause_atexit = 0;
  cshow(stdin);
  csetmode(C_RAW, stdin);
  return 0;
}

int close_keyb()
{ return 0;
}

int intflag(void)
{ long theKeys[4];
  GetKeys(theKeys);
  return theKeys[1]==0x8005;  /* Command-Option-Shift depressed  */
}
#endif

#if defined(forMSDOS) || defined(forBC4) || defined(forVmsItanium)
#include <signal.h>

extern int getch(void);

int Readch()
{ //int ch=getch();
  int ch=getchar();   // Itanium version
  if(ch==3) { /* ctrl-C */
    raise(SIGINT);
  }
  return ch;
}

int pollReadch(void)
{ return Readch();
}

int init_keyb(void)
{ return 0;
}

int close_keyb(void)
{ return 0;
}

int intflag(void)
{ return 0;
}
#endif

#ifdef forWIN32
#include <conio.h>
#include <signal.h>

extern int getch(void);

int Readch()
{ int ch=_getch();
  if(ch==3) { /* ctrl-C */
    raise(SIGINT);
  }
  return ch;
}

int pollReadch(void)
{ if (_kbhit()) return _getch();
  return -3; /* pollingch */
}

int init_keyb(void)
{ return 0;
}

int close_keyb(void)
{ return 0;
}

int intflag(void)
{ return 0;
}
#endif

#ifdef forOS2
#include <conio.h>

int Readch(void)
{ int ch = getch();
  return ch;
}

int pollReadch(void)
{ return Readch();
}

int init_keyb(void)
{ return 0;
}

int close_keyb(void)
{ return 0;
}

int intflag(void)
{ return 0;
}
#endif

#ifdef forSHwinCE

extern int getch(void);

int Readch()
{ return chBufGet();
}

int pollReadch(void)
{ return Readch();
}

int init_keyb(void)
{ return 0;
}

int close_keyb(void)
{ return 0;
}

int intflag(void)
{ INT32 flag = Interrupted;
  Interrupted = 0;
  return flag;
}

/* Unix style library */

FILEPT fopen(char *name, char *str) {
	FILEPT fp=NULL;
	TCHAR szName[100];
	DWORD access = 0;
	DWORD mode = 0;
        DWORD share = 0;
        DWORD creation = 0;
        DWORD attrs = 0;
	int i;
	for (i=0; *name; i++) szName[i] = *name++;
	szName[i] = 0;

	while(*str) {
	  if (str[0]=='r') {
            access =   GENERIC_READ;
            share =    FILE_SHARE_READ;
            creation = OPEN_EXISTING; /* fail if doesn't exist */
            attrs =    FILE_ATTRIBUTE_NORMAL;
          }
          if(*str=='w') {
            access =   GENERIC_WRITE;
            share =    FILE_SHARE_WRITE;
            creation = CREATE_ALWAYS; /* create or truncate */
            attrs =    FILE_ATTRIBUTE_NORMAL;
	  }
          if(*str=='+') {
            access =   GENERIC_READ | GENERIC_WRITE;
            share =    FILE_SHARE_READ | FILE_SHARE_WRITE;
            creation = OPEN_ALWAYS; /* Open or create, no truncation */
            attrs =    FILE_FLAG_RANDOM_ACCESS;
	  }
          str++;
	}
        fp = CreateFile(szName, access, share, NULL, creation, attrs, 0);
	if (fp==INVALID_HANDLE_VALUE) fp = 0;
	return fp;
}

int fclose(FILEPT fp) {
	return CloseHandle(fp) ? 0 : 1;
}

int clock() {
	return GetTickCount();
}

void putchar(char ch) {
	Wrch(ch);
}

void fflush(FILEPT fp) {
	return;
}

int fread(char *buf, size_t size, size_t len, FILEPT fp) {
	DWORD n=0;
	BOOL rc = ReadFile(fp, buf, (DWORD)size*len, &n, NULL);
	if(!rc) {
          DWORD err = GetLastError();
	  /*
          PRINTFD("fread: ReadFile, err=%ld\n", (long)(DWORD)err);
	  */
          return -1;
	}
	/*
	PRINTFD("fread trying to read from fd=%ld\n", (long)(DWORD)fp);
	PRINTFD("fread trying to read %ld bytes, ", (long)(DWORD)(size*len));
	PRINTFD("got %ld\n", (long)n);
	*/
	return n;
}

int fwrite(char *buf, size_t size, size_t len, FILEPT fp) {
	DWORD n=0;
	if(WriteFile(fp, buf, (DWORD)size*len, &n, NULL))
	  return n; /* Success */
	return -1;
}

int fseek(FILEPT fp, INT32 pos, int method) { /* Set the file position */
  SetFilePointer(fp, (LONG)pos, NULL, method);
  return 0;
}

int ftell(FILEPT fp) { /* Return the current file position */
  return SetFilePointer(fp, 0, NULL, FILE_CURRENT);
}


int unlink(char *name) {
        /* Delete (remove) a named file. */
	TCHAR szName[100];
	int i;
	for (i=0; *name; i++) szName[i] = *name++;
	szName[i] = 0;
	return ! DeleteFile(szName);
}

int rename(char *from, char *to) {
	TCHAR szFrom[100];
	TCHAR szTo[100];
	int i;
	for (i=0; *from; i++) szFrom[i] = *from++;
	szFrom[i] = 0;
	for (i=0; *to; i++) szTo[i] = *to++;
	szTo[i] = 0;
	return ! MoveFile(szFrom, szTo);
}

int fgetc(FILEPT fp) {
	BYTE ch;
	DWORD n=0;
	ReadFile(fp, &ch, 1, &n, NULL);

	return n==0 ? EOF : ch;
}

int eqstr(char *s1, char *s2) {
  while(*s1 && *s2) if(*s1++ != *s2++) return 0;
  return *s1==*s2;
}

char *getenv(char *name) {
 if(eqstr(name, "BCPLPATH")) return "\\BCPL\\cintcode\\cin";
 if(eqstr(name, "BCPLROOT")) return "\\BCPL\\cintcode";
 if(eqstr(name, "BCPLHDRS")) return "\\BCPL\\cintcode\\g";
 return "";
}
#endif

