/*
This is a program to test the performance of Pthreads, as used in the BCPL
Cintpos system.

Implemented by Martin Richards (c) April 2010

There are three threads:

The main thread corresponds to the Cintpos interpreter. It sends characters to
the tty device, receives interrupts from the clock device while otherwise
pretending to be cpu bound running the interpreter or suspended in the Idle
task waiting for an interrupt.

The tty thread corresponds to the tty output device in the Cintpos system. It
waits to be given a character from the main thread, then outputs it before
sending a interrupt request to the main thread indicating that it is ready for
another character.

The clock thread sends a steady stream of 10 clock interrupts per second to the
main thread.

The following variables are used to control the communication between the main
thread and the device threads.

ttycom       =1 indicates there is a character in ttych for the tty
                device to output. It is reset to zero by the tty device.
             =2 indicates that the tty device should commit suicide.

ttych        Holds a character being sent from the main thread to the tty
             device.

ttyirq       =1 if the tty thread wishes to interrupt the main thread to
                tell it the latest command has been completed. This it reset
                to 0 by the main thread when the interrupt is serviced.

tty_cv       is a condition variable used to wake up the tty device when it
             is waiting for a command from the main thread.

clkcom       =1 indicates that the clock device should commit suicide. The
                clock thread periodically inspects this variable.

clkirq       =1 if the clock device is requesting and interrupt. This
                is acknowledged by the main thread resetting it to zero.

irq_mutex    controls access to the variables: ttycom, ttych, ttyst.clkcom
             and clkirq.

irq_cv       is used by device threads to wake up the main thread to service
             and interrupt. Normally the main thread repeated polls ttyirq and
             clkirq, but if it has run out of work to do it suspends itself
             in pthread_cond_wait waiting for either ttyirq are clkirq to
             become set. It is signalled whenever either ttyirq or clkirq is
             set to one.

 
The program runs for about one second outputting the charaters A to Z on
separate lines at a rate of four characters per clock interrupt. It then
outputs a commented trace of the events that occurred during the run. This
trace is written to the file trace.txt and includes the event times measured in
milli-seconds.

The main thread simulates entering the Cintpos Idle task, but doubles the
number of simulated Cintcode instructions obeyed each time. The system
thus becomes more and more CPU bound causing the Idle task to be entered
less frequently.

The program can be configured to use priority scheduling, if this is available.

Typical output (to trace.txt) from this program (when running under Linux) is
as follows:

The following constants are defined:
  _POSIX_THREADS
  _POSIX_THREAD_PRIORITY_SCHEDULING
Scheduling priorities
  SCHED_FIFO  min=  1  max= 99
  SCHED_RR    min=  1  max= 99
  SCHED_OTHER min=  0  max=  0
Interpreter thread policy is: OTHER
Interpreter priority is: 0
Device threads policy is: OTHER
Device minpri=0  maxpri=0  priority=0
main: clk thread created successfully

  49.700:        1/1D  idle: entered #############
  49.700:        2/1D  idle: waiting for interrupt
  49.794:         /55  clk:  requesting interrupt
  49.794:        3/1D  idle: interrupted
  49.794:        0/CC  main: interrupt received from clk
  49.794:    'A'  /C1  main: sending a character to tty
  49.794:         /A1  tty:  received a command
  49.794:        1/DD  main: interrupt received from tty
  49.794:    'B'  /C1  main: sending a character to tty
  49.794:    'A'  /A2  tty:  character written
  49.794:         /A1  tty:  received a command
  49.794:    'B'  /A2  tty:  character written
  49.794:        1/DD  main: interrupt received from tty
  49.794:    'C'  /C1  main: sending a character to tty
  49.794:         /A1  tty:  received a command
  49.794:    'C'  /A2  tty:  character written
  49.794:        1/DD  main: interrupt received from tty
  49.794:    'D'  /C1  main: sending a character to tty
  49.794:         /A1  tty:  received a command
  49.794:    'D'  /A2  tty:  character written
  49.794:        1/DD  main: interrupt received from tty
  49.814:        1/1D  idle: entered #############
  49.814:        2/1D  idle: waiting for interrupt
  49.895:         /55  clk:  requesting interrupt
  49.895:        3/1D  idle: interrupted
  49.895:        1/CC  main: interrupt received from clk
  49.895:    'E'  /C1  main: sending a character to tty
  49.895:         /A1  tty:  received a command
  49.895:        1/DD  main: interrupt received from tty
  49.895:    'F'  /C1  main: sending a character to tty
  49.895:    'E'  /A2  tty:  character written
  49.895:         /A1  tty:  received a command
  49.895:    'F'  /A2  tty:  character written
  49.895:        1/DD  main: interrupt received from tty
  49.895:    'G'  /C1  main: sending a character to tty
  49.895:         /A1  tty:  received a command
  49.895:    'G'  /A2  tty:  character written
  49.895:        1/DD  main: interrupt received from tty
  49.895:    'H'  /C1  main: sending a character to tty
  49.895:         /A1  tty:  received a command
  49.895:    'H'  /A2  tty:  character written
  49.895:        1/DD  main: interrupt received from tty
  49.929:        1/1D  idle: entered #############
  49.929:        2/1D  idle: waiting for interrupt
  49.996:         /55  clk:  requesting interrupt
  49.996:        3/1D  idle: interrupted
  49.996:        2/CC  main: interrupt received from clk
  49.996:    'I'  /C1  main: sending a character to tty
  49.996:         /A1  tty:  received a command
  49.996:        1/DD  main: interrupt received from tty
  49.996:    'J'  /C1  main: sending a character to tty
  49.996:    'I'  /A2  tty:  character written
  49.996:         /A1  tty:  received a command
  49.996:    'J'  /A2  tty:  character written
  49.996:        1/DD  main: interrupt received from tty
  49.996:    'K'  /C1  main: sending a character to tty
  49.996:         /A1  tty:  received a command
  49.996:    'K'  /A2  tty:  character written
  49.996:        1/DD  main: interrupt received from tty
  49.996:    'L'  /C1  main: sending a character to tty
  49.996:         /A1  tty:  received a command
  49.996:    'L'  /A2  tty:  character written
  49.996:        1/DD  main: interrupt received from tty
  50.055:        1/1D  idle: entered #############
  50.055:        2/1D  idle: waiting for interrupt
  50.097:         /55  clk:  requesting interrupt
  50.097:        3/1D  idle: interrupted
  50.097:        3/CC  main: interrupt received from clk
  50.097:    'M'  /C1  main: sending a character to tty
  50.097:         /A1  tty:  received a command
  50.097:        1/DD  main: interrupt received from tty
  50.097:    'N'  /C1  main: sending a character to tty
  50.097:    'M'  /A2  tty:  character written
  50.097:         /A1  tty:  received a command
  50.097:        1/DD  main: interrupt received from tty
  50.097:    'N'  /A2  tty:  character written
  50.097:    'O'  /C1  main: sending a character to tty
  50.097:         /A1  tty:  received a command
  50.097:    'O'  /A2  tty:  character written
  50.097:        1/DD  main: interrupt received from tty
  50.097:    'P'  /C1  main: sending a character to tty
  50.097:         /A1  tty:  received a command
  50.097:    'P'  /A2  tty:  character written
  50.097:        1/DD  main: interrupt received from tty
  50.198:         /55  clk:  requesting interrupt
  50.198:        4/CC  main: interrupt received from clk
  50.198:    'Q'  /C1  main: sending a character to tty
  50.198:         /A1  tty:  received a command
  50.198:        1/DD  main: interrupt received from tty
  50.198:    'R'  /C1  main: sending a character to tty
  50.198:    'Q'  /A2  tty:  character written
  50.198:         /A1  tty:  received a command
  50.198:    'R'  /A2  tty:  character written
  50.198:        1/DD  main: interrupt received from tty
  50.198:    'S'  /C1  main: sending a character to tty
  50.198:         /A1  tty:  received a command
  50.198:    'S'  /A2  tty:  character written
  50.198:        1/DD  main: interrupt received from tty
  50.198:    'T'  /C1  main: sending a character to tty
  50.198:         /A1  tty:  received a command
  50.198:    'T'  /A2  tty:  character written
  50.198:        1/DD  main: interrupt received from tty
  50.207:        1/1D  idle: entered #############
  50.207:        2/1D  idle: waiting for interrupt
  50.298:         /55  clk:  requesting interrupt
  50.298:        3/1D  idle: interrupted
  50.298:        5/CC  main: interrupt received from clk
  50.298:    'U'  /C1  main: sending a character to tty
  50.298:         /A1  tty:  received a command
  50.298:        1/DD  main: interrupt received from tty
  50.298:    'V'  /C1  main: sending a character to tty
  50.298:    'U'  /A2  tty:  character written
  50.298:         /A1  tty:  received a command
  50.298:        1/DD  main: interrupt received from tty
  50.298:    'W'  /C1  main: sending a character to tty
  50.298:    'V'  /A2  tty:  character written
  50.298:         /A1  tty:  received a command
  50.298:        1/DD  main: interrupt received from tty
  50.298:    'X'  /C1  main: sending a character to tty
  50.298:    'W'  /A2  tty:  character written
  50.298:         /A1  tty:  received a command
  50.298:    'X'  /A2  tty:  character written
  50.298:        1/DD  main: interrupt received from tty
  50.399:         /55  clk:  requesting interrupt
  50.399:        6/CC  main: interrupt received from clk
  50.399:    'Y'  /C1  main: sending a character to tty
  50.399:         /A1  tty:  received a command
  50.399:        1/DD  main: interrupt received from tty
  50.399:    'Z'  /C1  main: sending a character to tty
  50.399:    'Y'  /A2  tty:  character written
  50.399:         /A1  tty:  received a command
  50.399:    'Z'  /A2  tty:  character written
  50.399:        1/DD  main: interrupt received from tty
  50.399:    '/'  /C1  main: sending a character to tty
  50.399:         /A1  tty:  received a command
  50.399:    '/'  /A2  tty:  character written
  50.399:        1/DD  main: interrupt received from tty
  50.399:    'A'  /C1  main: sending a character to tty
  50.399:         /A1  tty:  received a command
  50.399:    'A'  /A2  tty:  character written
  50.399:        1/DD  main: interrupt received from tty
  50.499:         /55  clk:  requesting interrupt
  50.499:        7/CC  main: interrupt received from clk
  50.499:    'B'  /C1  main: sending a character to tty
  50.499:         /A1  tty:  received a command
  50.499:        1/DD  main: interrupt received from tty
  50.499:    'C'  /C1  main: sending a character to tty
  50.499:    'B'  /A2  tty:  character written
  50.499:         /A1  tty:  received a command
  50.499:    'C'  /A2  tty:  character written
  50.499:        1/DD  main: interrupt received from tty
  50.499:    'D'  /C1  main: sending a character to tty
  50.499:         /A1  tty:  received a command
  50.499:    'D'  /A2  tty:  character written
  50.499:        1/DD  main: interrupt received from tty
  50.499:    'E'  /C1  main: sending a character to tty
  50.499:         /A1  tty:  received a command
  50.499:    'E'  /A2  tty:  character written
  50.499:        1/DD  main: interrupt received from tty
  50.511:        1/1D  idle: entered #############
  50.511:        2/1D  idle: waiting for interrupt
  50.599:         /55  clk:  requesting interrupt
  50.599:        3/1D  idle: interrupted
  50.599:        8/CC  main: interrupt received from clk
  50.599:    'F'  /C1  main: sending a character to tty
  50.599:         /A1  tty:  received a command
  50.599:        1/DD  main: interrupt received from tty
  50.599:    'G'  /C1  main: sending a character to tty
  50.599:    'F'  /A2  tty:  character written
  50.599:         /A1  tty:  received a command
  50.599:        1/DD  main: interrupt received from tty
  50.599:    'G'  /A2  tty:  character written
  50.599:    'H'  /C1  main: sending a character to tty
  50.599:         /A1  tty:  received a command
  50.599:    'H'  /A2  tty:  character written
  50.599:        1/DD  main: interrupt received from tty
  50.599:    'I'  /C1  main: sending a character to tty
  50.599:         /A1  tty:  received a command
  50.599:    'I'  /A2  tty:  character written
  50.599:        1/DD  main: interrupt received from tty
  50.700:         /55  clk:  requesting interrupt
  50.700:        9/CC  main: interrupt received from clk
  50.700:    'J'  /C1  main: sending a character to tty

End of run


Typical output (to trace.txt) from this program (when running under VMS on an
Itanium) is as follows:


The following constants are defined:
  _POSIX_THREADS
  _POSIX_THREAD_PRIORITY_SCHEDULING
Scheduling priorities
  SCHED_FIFO  min= 16  max= 31
  SCHED_RR    min= 16  max= 31
  SCHED_OTHER min=  8  max= 15
Interpreter thread policy is: OTHER
Interpreter priority is: 11

   7.146:        1/1D  idle: entered #############
   7.146:        2/1D  idle: waiting for interrupt
   7.246:         /55  clk:  requesting interrupt
   7.346:        3/1D  idle: interrupted
   7.346:        0/CC  main: interrupt received from clk
   7.346:    'A'  /C1  main: sending a character to tty
   7.346:         /55  clk:  requesting interrupt
   7.446:         /55  clk:  requesting interrupt
   7.546:         /A1  tty:  received a command
   7.546:    'A'  /A2  tty:  character written
   7.546:        1/DD  main: interrupt received from tty
   7.546:        1/CC  main: interrupt received from clk
   7.546:    'B'  /C1  main: sending a character to tty
   7.546:         /55  clk:  requesting interrupt
   7.646:         /55  clk:  requesting interrupt
   7.746:         /A1  tty:  received a command
   7.746:    'B'  /A2  tty:  character written
   7.746:        1/DD  main: interrupt received from tty
   7.746:        2/CC  main: interrupt received from clk
   7.746:    'C'  /C1  main: sending a character to tty
   7.746:         /55  clk:  requesting interrupt
   7.847:         /55  clk:  requesting interrupt
   7.947:         /A1  tty:  received a command
   7.947:    'C'  /A2  tty:  character written
   7.947:        1/DD  main: interrupt received from tty
   7.947:        3/CC  main: interrupt received from clk
   7.947:    'D'  /C1  main: sending a character to tty
   7.947:         /55  clk:  requesting interrupt
   8.047:         /55  clk:  requesting interrupt
   8.147:         /A1  tty:  received a command
   8.147:    'D'  /A2  tty:  character written


End of run
*/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <pthread.h>
#include <errno.h>
#include <time.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/timeb.h>

#define BCPLWORD int

FILE *outstream=0;

pthread_t *ttythread;
pthread_t *clkthread;

pthread_mutex_t irq_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  irq_cv    = PTHREAD_COND_INITIALIZER;
pthread_cond_t  tty_cv    = PTHREAD_COND_INITIALIZER;

int ttycom=0;  // =1 write, =2 die
int ttych=0;   // character to output
int ttyirq=0;  // tty command done

int clkcom=0;  // =1 die
int clkirq=0;  // clock interrupt
int ticks = 0;
int prevticks = 0;
int chcount = 4;

BCPLWORD trcount = -1; // trpush initially disabled
BCPLWORD trvec[4096];  // 4096 elements in the trpush circular buffer

pthread_mutex_t trpush_mutex = PTHREAD_MUTEX_INITIALIZER;
// trpush_mutex controls access to trcount and trvec

/* Low level trace functions */

void trpush(BCPLWORD val);
BCPLWORD settrcount(BCPLWORD count); // Set trcount and returns its old value
BCPLWORD gettrval(BCPLWORD count); // Get the specified trace value

void trpush(BCPLWORD val) {
  // If trcount>=0 push val into the circular trace buffer
  // Note that trpush is disabled if trcount=-1
  pthread_mutex_lock(&trpush_mutex);
  if(trcount>=0) {
    struct timeb tb;
    BCPLWORD secs, msecs;
    ftime(&tb);

    secs = ((BCPLWORD)tb.time % 60); /* Seconds is current minute */
    msecs = tb.millitm;              /* milli-seconds */
    // Push the time stamp
    trvec[trcount++ & 4095] = 0x66000000 + secs*1000 + msecs;
    trvec[trcount++ & 4095] = val;
  }
  pthread_mutex_unlock(&trpush_mutex);
}

BCPLWORD settrcount(BCPLWORD count) {
  // Set trcount returning the previous value
  BCPLWORD res;
  pthread_mutex_lock(&trpush_mutex);
  res = trcount;
  trcount = count;
  pthread_mutex_unlock(&trpush_mutex);
  return res;
}

BCPLWORD gettrval(BCPLWORD count) {
  // Return the trace value corresponding to position count.
  // The result is only valid if the circular buffer has not overflowed
  BCPLWORD res;
  pthread_mutex_lock(&trpush_mutex);
  res = trvec[count & 4095];
  pthread_mutex_unlock(&trpush_mutex);
  return res;
}

void prtrace(k) {
  int p = k - 4096;
  if(p<0) p = 0;
  //fprintf(outstream, "\n\nprtrace: k=%d\n", k);
  while(p<k) {
    int val = gettrval(p++);
    int flag = (val>>24) & 255;
    val = val & 0xFFFFFF;
    switch(flag) {
    default:
      fprintf(outstream, " %8d/%2X", val, flag);
      break;

    case 0x66:
      fprintf(outstream, "\n%4d.%03d:", val/1000, val % 1000);
      break;

    case 0xA1:
      fprintf(outstream, "         /%2X  tty:  received a command", flag);
      break;

    case 0xA2:
      if(val==10) val = '/';
      fprintf(outstream, "    '%c'  /%2X  tty:  character written", val, flag);
      break;

    case 0xC1:
      if(val==10) val = '/';
      fprintf(outstream, "    '%c'  /%2X  main: sending a character to tty",
              val, flag);
      break;

    case 0x1D:
      fprintf(outstream, " %8d/%2X  ",  val, flag);
      if(val==1) fprintf(outstream, "idle: entered #############");
      if(val==2) fprintf(outstream, "idle: waiting for interrupt");
      if(val==3) fprintf(outstream, "idle: interrupted");
      break;

    case 0xCC:
      fprintf(outstream, " %8d/%2X  main: interrupt received from clk",  val, flag);
      break;

    case 0xDD:
      fprintf(outstream, " %8d/%2X  main: interrupt received from tty",  val, flag);
      break;

    case 0x55:
      fprintf(outstream, "         /%2X  clk:  requesting interrupt",  flag);
      break;
    }
  }
  fprintf(outstream, "\n\n");
}


void *ttycode(void *arg) {
  // Body of the tty thread
  int i;
  int ch = 0;
  int com = 0;

  while(1) {
    // Wait for a command from the main thread
    //fprintf(outstream, "tty: calling lock\n");
    pthread_mutex_lock(&irq_mutex);
    //fprintf(outstream, "tty: obtained lock\n");

    //fprintf(outstream, "tty: waiting for a tty command\n");
    // Perform a conditional wait for a command from the main thread
    while (ttycom==0) {
      //fprintf(outstream, "tty: calling cond_wait because ttycom=%d\n", ttycom);
      pthread_cond_wait(&tty_cv, &irq_mutex);
      //fprintf(outstream, "tty: running after cond_wait\n");
    }
    // ttycom is non zero
    com = ttycom;
    ch = ttych;
    ttycom = 0;
    ttyirq = 1; // Interrupt to acknowledge the command

    trpush(0xA1000000);       // tty: received command, sending interrupt to main
    //fprintf(outstream, "tty: calling cond_signal\n");
    pthread_cond_signal(&tty_cv); // Wake up the idle task if necessary

    //fprintf(outstream, "tty: calling unlock\n");
    pthread_mutex_unlock(&irq_mutex);

    //fprintf(outstream, "tty: com=%d ch=%d\n", com, ch);

    if(com==2) {
      fprintf(outstream, "tty: Die command received\n");
      break;
    }

    if(com==1) {
      // Write the character
      putchar(ch);
      fflush(stdout);
      trpush(0xA2000000+ch);   // tty: character written
    }
    // Repeat
  }

  fprintf(outstream, "\n\ntty thread dying\n");
  return 0;
}

void *clkcode(void *arg) {
  // Body of the clk thread

  while(1) {
    // Check for die command
    //fprintf(outstream, "clk: calling lock\n");
    pthread_mutex_lock(&irq_mutex);

    if( clkcom==1) {
      // Die command received
      clkirq = 1;
      //fprintf(outstream, "clk: calling unlock\n");
      pthread_mutex_unlock(&irq_mutex);
      break;
    }

    //fprintf(outstream, "clk: calling unlock\n");
    pthread_mutex_unlock(&irq_mutex);

    //fprintf(outstream, "clk: calling unsleep\n");
    usleep(100000); // Sleep for so many micro-seconds (1/10 sec)
    //fprintf(outstream, "clk: return from usleep\n");

    // Send a clock interrupt request
    //fprintf(outstream, "clk: calling lock\n");
    pthread_mutex_lock(&irq_mutex);
    clkirq = 1;
    trpush(0x55000000); // clk: sending interrupt to main

    // Wake up the idle task if necessary
    //fprintf(outstream, "clk: calling cond_signal\n");
    pthread_cond_signal(&irq_cv);

    //fprintf(outstream, "clk: calling unlock\n");
    pthread_mutex_unlock(&irq_mutex);
  }

  // Die command received, so die.
  fprintf(outstream, "\n\nClk thread dying\n");
  return 0;
}

int main() {
  int rc;
  int i = 0;
  int count = 0;
  int countmax = 1000000;
  int icount = 0;
  int ttyready = 1;
  int ch = 'A';

  pthread_attr_t custom_sched_attr;
  pthread_attr_t *custom_sched_attr_ptr = 0;
  struct sched_param param;
  int min_prio, max_prio;

  struct timeb tb;
  int secs0, msecs0;  // Start time
  int secs, msecs;    // Current time

  outstream = fopen("trace.txt", "wb");

  ftime(&tb);
  secs0 = ((int)tb.time % 60); /* Seconds is current minute */
  msecs0 = tb.millitm;              /* milli-seconds */

  fprintf(outstream, "\nThe following constants are defined:\n");
#ifdef _POSIX_THREADS
  fprintf(outstream, "  _POSIX_THREADS\n");
#endif
#ifdef _POSIX_THREAD_PRIORITY_SCHEDULING
  fprintf(outstream, "  _POSIX_THREAD_PRIORITY_SCHEDULING\n");
  { pthread_t self = pthread_self();
    pthread_attr_t thread_attr;
    struct sched_param thread_param;
    int thread_policy;

    rc = pthread_attr_init(&thread_attr);
    if (rc) {
      fprintf(outstream, "attr_init failed\n");
    }

    fprintf(outstream, "Scheduling priorities\n");
    fprintf(outstream, "  SCHED_FIFO  min=%3d  max=%3d\n",
            sched_get_priority_min(SCHED_FIFO),
	    sched_get_priority_max(SCHED_FIFO));
    fprintf(outstream, "  SCHED_RR    min=%3d  max=%3d\n",
            sched_get_priority_min(SCHED_RR),
	    sched_get_priority_max(SCHED_RR));
    fprintf(outstream, "  SCHED_OTHER min=%3d  max=%3d\n",
            sched_get_priority_min(SCHED_OTHER),
	    sched_get_priority_max(SCHED_OTHER));

    rc = pthread_getschedparam(
		  self, &thread_policy, &thread_param);
    if (rc) {
	fprintf(outstream, "getschedparam failed\n");
    }
    rc = pthread_attr_getschedparam(
                       &thread_attr, &thread_param);
    if (rc) {
	fprintf(outstream, "getschedparam failed\n");
    }
    fprintf(outstream, "Interpreter thread policy is: %s\n",
	     (thread_policy==SCHED_FIFO ?  "FIFO" :
              thread_policy==SCHED_RR ?    "RR" :
              thread_policy==SCHED_OTHER ? "OTHER" : "Unknown"));

    fprintf(outstream, "Interpreter priority is: %d\n",
                        thread_param.sched_priority);
  }    
#endif

  ttythread = (pthread_t *)malloc(sizeof(pthread_t));
  clkthread = (pthread_t *)malloc(sizeof(pthread_t));

  if (ttythread==0 || clkthread==0) {
    fprintf(outstream, "Unable to create one of the device threads\n");
    if (ttythread) free(ttythread);
    if (clkthread) free(clkthread);
    return 0;
  }

  //fprintf(outstream, "Main: Creating ttythread\n");
#ifdef _POSIX_THREAD_PRIORITY_SCHEDULING
   custom_sched_attr_ptr = &custom_sched_attr;

   rc = pthread_attr_init(custom_sched_attr_ptr);
   if (rc) {
    fprintf(outstream, "attr_init() => rc=%d\n", rc);
    fprintf(outstream, "ENOMEM=%d\n", ENOMEM);
   }
   rc = pthread_attr_setinheritsched(custom_sched_attr_ptr,
                                          PTHREAD_EXPLICIT_SCHED);
   if (rc) {
     fprintf(outstream, "setinheritsched() => rc=%d\n", rc);
     fprintf(outstream, "EINVAL=%d ENOTSUP=%d\n", EINVAL, ENOTSUP);
   }

   { int policy = SCHED_OTHER;
     int priority = 0;

      fprintf(outstream, "Device threads policy is: %s\n",
	     (policy==SCHED_FIFO ?  "FIFO" :
              policy==SCHED_RR ?    "RR" :
              policy==SCHED_OTHER ? "OTHER" : "Unknown"));

     rc = pthread_attr_setschedpolicy(custom_sched_attr_ptr, policy);
     if (rc) {
       fprintf(outstream, "setschedpolicy() => rc=%d\n", rc);
       fprintf(outstream, "EINVAL=%d ENOTSUP=%d\n", EINVAL, ENOTSUP);
     }

     min_prio = sched_get_priority_min(policy);
     max_prio = sched_get_priority_max(policy);
     priority = (min_prio+max_prio)/2;

     fprintf(outstream, "Device minpri=%d  maxpri=%d  priority=%d\n",
             min_prio, max_prio, priority);
     param.sched_priority = priority;
     rc = pthread_attr_setschedparam(&custom_sched_attr, &param);
     if (rc) {
       fprintf(outstream, "setschedparam() => rc=%d\n", rc);
       fprintf(outstream, "EINVAL=%d ENOTSUP=%d\n", EINVAL, ENOTSUP);
     }
   }

   rc = pthread_attr_setscope(&custom_sched_attr,
                              PTHREAD_SCOPE_SYSTEM);
   if (rc) {
     fprintf(outstream, "setscope() => rc=%d\n", rc);
     fprintf(outstream, "EINVAL=%d ENOTSUP=%d\n", EINVAL, ENOTSUP);
   }

   //custom_sched_attr_ptr = NULL; // Disable priority scheduling
#endif


  ttyirq = 0;
  ttych = 0;
  ttycom = 0;

  rc =  pthread_create(ttythread, custom_sched_attr_ptr, ttycode, 0);
  if (rc) {
    fprintf(outstream, "Unable to create ttythread, rc=%d\n", rc);
    fprintf(outstream, "EAGAIN=%d EINVAL=%d EPERM=%d\n", EAGAIN, EINVAL, EPERM);
    goto fin;
  }

  rc =  pthread_detach(*ttythread);
  if (rc) {
    fprintf(outstream, "Unable to detach ttythread, rc=%d\n", rc);
    fprintf(outstream, "EINVAL=%d ESRCH=%d\n", EINVAL, ESRCH);
  }

  //fprintf(outstream, "main: tty thread created successfully\n");

  rc =  pthread_create(clkthread, custom_sched_attr_ptr, clkcode, 0);
  if (rc) {
    fprintf(outstream, "Unable to create clkthread, rc=%d\n", rc);
    fprintf(outstream, "EAGAIN=%d EINVAL=%d EPERM=%d\n", EAGAIN, EINVAL, EPERM);
    goto fin;
  }

  rc =  pthread_detach(*clkthread);
  if (rc) {
    fprintf(outstream, "Unable to detach clkthread, rc=%d\n", rc);
    fprintf(outstream, "EINVAL=%d ESRCH=%d\n", EINVAL, ESRCH);
  }

  fprintf(outstream, "main: clk thread created successfully\n");

  count = settrcount(0); // Start tr-pushing

  // Pretend to run the Cintpos interpreter

  prevticks = ticks;

 fetch:
  if(ticks>=10) goto fin;

  if(--icount<0) {
    // Every 1000 instructions poll for an interrupt from
    // the tty or clk device
    icount = 1000;

    ftime(&tb);
    secs = (int)tb.time % 60;
    msecs = (int)tb.millitm;

    //if(secs != secs0 && msecs/100 == msecs0/100) {
    //  // Stop after about 1 second
    //  goto fin;
    //}

    //fprintf(outstream, "Main: calling lock\n");
    pthread_mutex_lock(&irq_mutex);
    //fprintf(outstream, "Main: obtained lock\n");
    
    if(ttyirq) {
      trpush(0xDD000001);       // main: tty interrupt received
      ttyready = 1;
      ttyirq = 0;
    }

    if(clkirq) {
      trpush(0xCC000000+ticks); // main: clk interrupt received
      clkirq = 0;
      ticks++;
    }

    //fprintf(outstream, "Main: calling unlock\n");
    pthread_mutex_unlock(&irq_mutex);
  }
  count++; // Number of Cintcode instructions interpreted

  // Pretend to execute a Cintcode instruction
  for(i=1; i<10; i++) continue;


  if(ttyready && ticks!=prevticks) {
    //printf("\nticks=%d prevticks=%d chcount=%d ", ticks, prevticks, chcount);
    if(--chcount<=0) {
      chcount = 4; // Write 4 characters per tick
      prevticks = ticks;
    }
    // Write next character

    //fprintf(outstream, "Main: calling lock\n");
    pthread_mutex_lock(&irq_mutex);

    // Output A to Z, newline, repeatedly
    if(ch<'A') ch = 'A';
    if(ch>'Z') ch = '\n';
    trpush(0xC1000000+ch); // main: sending ch to tty device
    ttych = ch++;
    ttycom = 1;  // Write
    ttyready = 0;

    //fprintf(outstream, "main: calling cond_signal\n");
    pthread_cond_signal(&tty_cv);

    //fprintf(outstream, "Main: calling unlock\n");
    pthread_mutex_unlock(&irq_mutex);

    sched_yield(); // This makes a big difference under Linux!!
  }

  if(count>countmax) {
    // Every so often pretend to suspend in the idle task
    count = 0;
    // repeatedly reduce the frequency of entering the idle task
    countmax = countmax*2;
    //fprintf(outstream, "\ncountmax=%d\n", countmax);
    trpush(0x1D000001);           // idle: task entered

    pthread_mutex_lock(&irq_mutex);
    if(ttyirq==0 && clkirq==0) {
      trpush(0x1D000002);         // idle; waiting for interrupt
      pthread_cond_wait(&irq_cv, &irq_mutex);
    }
    // Ensure that the interpreter checks for interrupts
    icount = 0;
    pthread_mutex_unlock(&irq_mutex);

    trpush(0x1D000003);           // idle: interrupted
  }
  goto fetch;

 fin:
  count = settrcount(-1); // Stop tr-pushing
 
  prtrace(count);

  fprintf(outstream, "End of run\n");

  if(outstream) fclose(outstream);

  printf("\n");
  return 0;
}
