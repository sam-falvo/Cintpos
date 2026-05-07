typedef unsigned long uLong;
typedef unsigned long long uQuad;
typedef unsigned short uWord;
typedef unsigned char uByte;

typedef struct Buffer Buffer;
typedef struct Line Line;
typedef struct Position Position;
typedef struct String String;

struct Position { Buffer *buffer;
                  Line *line;
                  uLong offset;
                };

#include <stdio.h>

/* chrutils.c */

char *skipspaces (char *s);
char *uptospace (char *s);
int eoltest (char *s);

/* ch_screen_*.c */

int ch_screen_init (void);		/* initiate screen mode */
String *ch_screen_read (void);		/* update screen / read command string */
void ch_screen_refresh (void);		/* refresh (redraw) screen */
void ch_screen_term (void);		/* terminate screen mode */
void ch_screen_prompt (String *prompt);	/* display prompt string */
void ch_screen_message (const char *message); /* display (error) message */
uLong ch_screen_chr2col (uLong linesz, const char *linebf); /* convert character count to column count */
uLong ch_screen_col2chr (uLong linesz, const char *linebf, uLong ncols); /* convert column count to character count */
String *ch_screen_rdkpad (void);	/* read keypad codes from keyboard */

/* cmd_change.c */

extern Line *ch_screen_top_line;	/* pointer to what we want for top line on screen */
extern int ch_screen_shiftleft;		/* number of characters to leave off beg of line */
extern int ch_screen_num_lines;		/* total number of lines on the screen */
extern int ch_screen_tmar_lines;	/* number of lines to reserve for top margin */
extern int ch_screen_bmar_lines;	/* number of lines to reserve for bottom margin */
extern int ch_screen_width;		/* number of columns on the screen, including any being used for 'set number show' */
extern int ch_screen_numofs;		/* number of columns on left currently being used for 'set number show' */
extern int autoshift;
extern char *searchstr;
extern uLong searchlen;
extern Position sel_position;

/* cmd_help.c */

void cmd_help (char *cp);

/* cmd_set.c */

extern char *(*xstrstr) ();
extern int  (*xstrncmp) ();

/* crash.c */

void crash (char *message);

/* edt.c */

extern Buffer *main_buffer;
extern char *journal_name;
extern const char *pn;
extern FILE *journal_file, *recover_file;
extern int showlfs;
extern int shownums;
extern Position cur_position;

/* journaling.c */

String *jnl_readprompt (const char *prompt);
int jnl_readkeyseq (String *keystring);
void jnl_flush (void);
void jnl_close (int del);

/* keypad.c */

void show_keypad (void);
const char *keypad_getdef (const char *keyname);
void keypad_setdef (const char *keyname, const char *command);
int keypad_getname (const char *keystring, char keyname[16]);
int keypad_decode (String *keystring, String *cmdstring);

/* line.c */

Buffer *buffer_create (int namel, const char *name);
void buffer_delete (Buffer *buffer);
Buffer *buffer_next (Buffer *buffer);
Line *buffer_first_line (Buffer *buffer);
Line *buffer_last_line (Buffer *buffer);
uLong buffer_linecount (Buffer *buffer);
Position *buffer_savpos (Buffer *buffer);
const char *buffer_name (Buffer *buffer);
const char *buffer_filename (Buffer *buffer);
void buffer_setreadfile (Buffer *buffer, FILE *readfile);
FILE *buffer_getreadfile (Buffer *buffer);
void buffer_setfile (Buffer *buffer, const char *filename);
int buffer_dirty (Buffer *buffer, int newdirty);
Line *line_insert (Buffer *buffer, Line *next, String *string);
String *line_remove (Line *line);
const char *line_number (Line *line);
int line_numcmp (Line *line, const char *number);
Line *line_next (Line *line);
Line *line_prev (Line *line);
String *line_string (Line *line);
void line_reseq (Line *line);
void line_print (Line *line);

/* ln_command.c */

int ln_command (const char *cmdstr);

/* os.c */

extern char *help_name;
void os_initialization (void);
void os_screenmode (int on);
String *os_readprompt (const char *prompt);
int os_readkeyseq (String *keystring);
int os_writebuffer (int size, const char *buff);
int os_getscreensize (int *width_r, int *length_r);
char *os_makejnlname (const char *filename);
int os_readonlyfile (char const *name);
int os_soleditor (char const *name);
FILE *os_crenewfile (const char *name);
char *os_defaultinitname (void);

/* output.c */

void outerr (int extra, const char *format, ...);
void outfmt (int extra, const char *format, ...);
void outstr (const char *string);
void outchr (char c);
void outbuf (int size, const char *buff);
void output (void);

/* range.c */

extern const char bufnamechars[];
int range_single (char *cp, char **cp_r, Position *pos_r);
int range_multiple (char *cp, char **cp_r, int (*entry) (void *param, Buffer *buffer, Line *line), void *param);
int matchkeyword (const char *cp, const char *kw, int min);

/* read_file.c */

void read_file (FILE *input_file, Buffer *buffer, Line *next_line);
String *readfileline (FILE *input_file, String *string);

/* relposition.c */

int relposition (Position *p1, Position *p2);

/* representation.c */

const char *representation (char c, char temp[16], int col);

/* string.c */

String *string_create (uLong len, const char *val);
void string_delete (String *string);
const char *string_getval (String *string);
uLong string_getlen (String *string);
void string_setval (String *string, uLong len, const char *value);
void string_concat (String *string, uLong len, const char *value);
void string_remove (String *string, uLong length, uLong offset);
void string_insert (String *string, uLong offset, uLong length, const char *insert);
int string_scanchr (String *string, char chr);
int string_scanstr (String *string, const char *str);

/* write_file.c */

int write_file (const char *out_name, Line *beg_line, Line *end_line);



#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include dcdef
#include iodef
#include ssdef
#include tt2def

static int jnlflushenable = 0;		/* 0: not in read routine, ast should not do anything */
					/* 1: in read routine, it's ok for ast routine to flush */
static int jnlflushtimerip = 0;		/* 0: no timer is queued */
					/* 1: timer request queued */
static int ttsaveof = 0;		/* 0: last read ended normally */
					/* 1: last read ended in eof */
static uByte originalmodes[12];		/* original terminal modes */
static uQuad nextjournalflush = 0;	/* 0: the journal was freshly flushed and a read is */
					/*    in progress, so no journal flushing need be done */
					/* else: datetime of next journal flush */
static uWord ttchan = 0;		/* 0: channel has not been assigned yet */
					/* else: i/o channel to the terminal */

void lib$stop ();
uLong sys$assign ();
uLong sys$exit ();
uLong sys$gettim ();
uLong sys$qio ();
uLong sys$qiow ();
uLong sys$setast ();
uLong sys$setimr ();
uLong sys$synch ();







int initkeypad() {
  char *p, *ttname;
  struct { uLong size;
           char *buff;
         } ttdesc;
  uLong sts;

  struct { uWord sts, len;
           char trm[4];
         } iosb;

  /* Assign I/O channel to terminal */

  ttname = "TT";
  ttdesc.size = strlen (ttname);
  ttdesc.buff = ttname;
  sts = sys$assign (&ttdesc, &ttchan, 0, 0);
  if (!(sts & 1)) {
    fprintf (stderr, "error 0x%x assigning channel to terminal %s\n", sts, ttname);
    sys$exit (sts);
  }

  /* Sense mode - get original modes and make sure it is a terminal */

  sts = sys$qiow (1, ttchan, IO$_SENSEMODE, &iosb, 0, 0,
                  originalmodes, sizeof originalmodes,
                  0, 0, 0, 0);
  if (sts & 1) sts = iosb.sts;
  if (!(sts & 1)) {
    fprintf (stderr, "error 0x%x sensing terminal %s modes\n", sts, ttname);
    sys$exit (sts);
  }
  if (originalmodes[0] != DC$_TERM) {
    fprintf (stderr, "device %s is not a terminal\n", ttname);
    sys$exit (SS$_IVDEVNAM);
  }
  return 1;
}




int readkeyseq(char *keystr) {

  char *kp = keystr;
  char buf[256];
  struct { uWord sts, len;
           char termchr;
           char fill1;
           char termlen;
           char fill2;
         } iosb;
  uLong sts;


  /* Read without echoing, wait for one character */

  sts = sys$qiow (1, ttchan, IO$_READVBLK | IO$M_NOECHO,
                  &iosb, 0, 0, buf, 1,
                  0, 0, 0, 0);

  /* Check termination status, treat any error like an eof */

  if (sts & 1) sts = iosb.sts;
  if (!(sts & 1)) return (0);                // EOF

  /* Concat any fetched data to string */

  for (i=0; i<iosb.len; i++) *kp++ = buf[i];
  for (i=0; i<iosb.termlen; i++) *kp++ = (&iosb.termchr)[i];


  /* Read again, but just get whatever happens to be in read-ahead, don't wait */

  sts = sys$qiow (1, ttchan, IO$_READVBLK | IO$M_NOECHO | IO$M_TIMED, &iosb, 0, 0, buf, sizeof buf, 0, 0, 0, 0);

  /* Check termination status, treat any error like an eof */

  if (sts & 1) sts = iosb.sts;
  if (sts == SS$_TIMEOUT) sts |= 1;
  if (!(sts & 1)) return (0);                  // EOF

  /* Concat any fetched data to string */

  for (i=0; i<iosb.len; i++) *kp++ = buf[i];
  for (i=0; i<iosb.termlen; i++) *kp++ = (&iosb.termchr)[i];
  *kp = 0;

  return (1);
}

int main() {
  char buf[256];

  printf("\nvmsrdtest entered\n");

  initkeypad();

  for(i=1; i<=5; i++)
    { if(readkeyseq(buf)) {
        char *p = buf;
        printf("\nkeys pressed\n");
        while(*p) {
          int ch = *p++;
          printf("%3d ", ch);
          if(ch>=32) printf("'%c'", ch);
          printf("\n");
        }
      }
    }
}


