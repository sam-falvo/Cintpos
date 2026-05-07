/*
** This module contains the code to handle devices.
**
** (c) Copyright:  Martin Richards  2 February 2010
*/

/*

24/01/06
Removed custom scheduling attributes from calls of pthread_create, and modified
ttyin to prefix the string inbuf if non empty (for the -s and -c options).

26/03/03
Beginning to add TCP timeouts

04/02/02
Initial version implemented.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <pthread.h>

/* cintpos.h contains machine/system dependent #defines  */
#include "cintpos.h"

#include <fcntl.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/timeb.h>

/* includes for the TCP/IP code */

#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>

#ifdef forVmsItanium
#include <inet.h>
#endif

extern BCPLWORD *W;
extern int mainpid;
extern BCPLWORD Readch(void);
extern void msecdelay(unsigned int delaymsecs);
extern int boottrace;

extern char *inbuf;        // For prepended standard input
extern int reattach_stdin; // If =1 switch to stdin after inbuf is empty
extern int inbuf_next(void); // Defined in cintsys or cintpos

extern pthread_mutex_t irq_mutex; // Defined in cintpos.c
extern pthread_cond_t irq_cv;     // Defined in cintpos.c

void *clkcode    (void *dp);
void *ttyincode  (void *dp);
void *ttyoutcode (void *dp);
void *fileopcode (void *dp);
void *tcpdevcode (void *dp);

BCPLWORD irqfifov[1024]; /* This holds a fifo of device ids that wish to
                            interrupt the interpreter */
BCPLWORD irqfifop=0, irqfifoq=0; /* fifo queue from p to q-1  */

/*
// Acquire lock on the interrupt mutex, ensuring that any outstanding
// interrupt has been serviced before proceeding.
static inline void acquire_irq_lock() {
  for (;;) {
    if (irq == 0) { // no unserviced interrupt, try to acquire lock
      pthread_mutex_lock(&irq_mutex);
      if (irq == 0) return; // locked and irq clear, OK to proceed
      else pthread_mutex_unlock(&irq_mutex);
    }
    // printf("(wait for irq clear)\n"); fflush (stdout);
    usleep(1); // retry after shortest possible delay
  }
}

// Release lock on the interrupt mutex.
static inline void release_irq_lock() {
  pthread_mutex_unlock(&irq_mutex);
}
*/
  
BCPLWORD initdevices() {
  // Initialise the fifo to contain no device interrupt requests.
  // This is called before any devices have been created, so
  // the lock and unlock calls are unnecessary.
  int i;
  pthread_mutex_lock(&irq_mutex);
  for (i=0; i<1024; i++) irqfifov[i] = 0;
  irqfifop=irqfifoq=0;
  pthread_mutex_unlock(&irq_mutex);
  return 0;
}

/*
devcommand(dcb, com, arg) causes the device with the given dcb
to execute command com with argument arg. The commands are:

Devc_create    Create a condition variable and thread for the device.
Devc_destroy   Stop the device, generate interrupts to return
               all the packets from the wkq, let the thread commit
               suicide and deallocate the condition variable.
Devc_start     Signal to the device thread that there is probably a
               packet to process at the head of the wkq.
Devc_stop      Abort the current I/O operation, if any, and put the
               device in stopped state. In this state the device will
               not look at packets on its wkq or generate new interrupts,
               however there may still be interrupt requests in the fifo.
Devc_setintson Set the intson state (to true or false) for the device.
               arg=TRUE  enable interrupts and wakeup the device thread
               arg=FALSE disable further interrupts from this device.
                         Note that there may still be interrupts for
                         this device in the fifo.

After a device has put an interrupt request into the fifo and before
it receives another command (usually Devc_start), the kernel is free
to add or remove packets from its wkq.
              
*/

void copyaddr(char*from, char*to) {
  // This copies machine addresses between C code and
  // BCPL structures.
  int i=0;
  while(i<sizeof(int*)) {
    *to++ = *from++;
    i++;
  }
}

// devcommand is invoked by sys(Sys_devcom, com, arg

// NOTE: it runs in the interpreter thread (NOT in a device thread)

BCPLWORD devcommand(BCPLWORD dcb, BCPLWORD com, BCPLWORD arg) {
  BCPLWORD *dp     = (BCPLWORD *)&W[dcb];
  BCPLWORD devtype = 0;
  BCPLWORD devid   = 0;

  // Put com and arg into the dcb (safely) - since its device thread
  // may be running

  pthread_mutex_lock(&irq_mutex);
  devtype = dp[Dcb_type];
  devid   = dp[Dcb_devid];
  dp[Dcb_op]   = com;
  dp[Dcb_arg]  = arg;
  pthread_mutex_unlock(&irq_mutex);

  //if(devid<=-4)
  //  printf("devcom: devid=%d com=%d arg=%d\n", devid, com, arg);

  // Perform a command: create, destroy, start, stop or setintson
  switch (com) {
    default: printf("devcommand: Bad device command %lld\n", LL com);
             return 0;

    case Devc_create:
      { // This runs on the interpreter thread
        int rc=1;
        void *(*threadcode)(void*);

        // Setting the scheduling priority was added by MR 6/1/05
        // Device threads now run with highest priority.
        pthread_attr_t custom_sched_attr;
        pthread_attr_t *custom_sched_attr_ptr = NULL;
        int fifo_max_prio, fifo_min_prio;
        struct sched_param fifo_param;

        // Allocate space for the Pthread
        pthread_t *threadp = 
                  (pthread_t *)malloc(sizeof(pthread_t));
        // Allocate space for the condition variable
        pthread_cond_t *cvp =
                  (pthread_cond_t *)malloc(sizeof(pthread_cond_t));
	//printf("sizeof(pthread_t) = %lld\n", LL sizeof(pthread_t));
	//printf("sizeof(pthread_cond_t) = %lld\n", LL sizeof(pthread_cond_t));
        if(threadp==0 || cvp==0) {
          printf("devcommand: malloc failed\n");
          if (threadp) free(threadp);
          if (cvp)     free(cvp);
          return 0; // 0 = failure
        }
	pthread_cond_init(cvp, NULL);
        // Put pointers to the Pthread and condition variable (safely)
        // into the DCB
        pthread_mutex_lock(&irq_mutex);
	copyaddr((char*)&threadp, (char*)&dp[Dcb_threadp]);
        // dp[Dcb_threadp] = threadp;
	copyaddr((char*)&cvp, (char*)&dp[Dcb_cvp]);
        //dp[Dcb_cvp]     = (BCPLWORD)cvp;
        pthread_mutex_unlock(&irq_mutex);

#ifdef _POSIX_THREAD_PRIORITY_SCHEDULING
        custom_sched_attr_ptr = &custom_sched_attr;

        rc = pthread_attr_init(custom_sched_attr_ptr);
        if (rc) printf("attr_init() => rc=%d\n", rc);
        rc = pthread_attr_setinheritsched(custom_sched_attr_ptr,
	//                                     PTHREAD_INHERIT_SCHED);
                                          PTHREAD_EXPLICIT_SCHED);
        if (rc) printf("setinheritsched() => rc=%d\n", rc);
        rc = pthread_attr_setschedpolicy(custom_sched_attr_ptr,
                                    SCHED_OTHER);
        ///if (rc) printf("setschedpolicy() => rc=%d\n", rc);

        fifo_min_prio = sched_get_priority_min(SCHED_FIFO);
        fifo_max_prio = sched_get_priority_max(SCHED_FIFO);

        //printf("devices: FIFO min pri = %lld  maxpri=%lld\n",
        //        LL fifo_min_prio, LL fifo_max_prio);
        fifo_param.sched_priority = (fifo_min_prio+fifo_max_prio)/2;
        rc = pthread_attr_setschedparam(&custom_sched_attr,
                                   &fifo_param);
        if (rc && boottrace) printf("setschedparam() => rc=%d\n", rc);

        rc = pthread_attr_setscope(&custom_sched_attr,
                                   //PTHREAD_SCOPE_PROCESS); // Not supported
	                           PTHREAD_SCOPE_SYSTEM);
        if (rc && boottrace) {
          printf("setscope() => rc=%d\n", rc);
          printf("%d=EINVAL  %d=ENOTSUP\n", EINVAL, ENOTSUP);
        }
        //custom_sched_attr_ptr = NULL;
#endif

        // Create the device thread, with appropriate code for each
        // type of device.

      sw:
        switch (dp[Dcb_type]) {
	  default: printf("Devc_create: Unknown device type %lld\n",
                   LL dp[Dcb_type]);
                   return 0;

          case Devt_clk:
            threadcode = clkcode;
            break;

          case Devt_ttyin:
            threadcode = ttyincode;
            break;

          case Devt_ttyout:
            threadcode = ttyoutcode;
            break;

          case Devt_fileop:
            threadcode = fileopcode;
            break;

          case Devt_tcpdev:
            threadcode = tcpdevcode;
            break;
        }

        rc =  pthread_create(threadp, &custom_sched_attr, threadcode, dp);
        if (rc) {
          //printf("Unable to create a FIFO priority scheduled thread\n");
          rc =  pthread_create(threadp, NULL, threadcode, dp);
        }
        //printf("Devc_create: rc=%d\n",rc);

        if (rc) {
	  printf("Devc_create: rc=%d\n",rc);
	  printf("EAGAIN=%d EINVAL=%d EPERM=%d\n", EAGAIN, EINVAL, EPERM);
          printf("Devc_create: Unable to create thread for dev=%lld\n", LL devid);
          return 0;
        }
        rc =  pthread_detach(*threadp);
        if (rc) {
          printf("Devc_create: Unable to detach thread for dev=%lld\n", LL devid);
          return 0;
        }
	//printf("Devc_create: dev=%lld done\n", LL devid);
        { int policy;
          struct sched_param param;
          rc = pthread_getschedparam(*threadp, &policy, &param);
          //printf("getschedparam()=> %lld\n",LL rc);
          //printf("policy=%lld SCHED_FIFO=%lld, SCHED_RR=%lld, SCHED_OTHER=%lld\n",
          //        LL policy, LL SCHED_FIFO, LLSCHED_RR, SCHED_OTHER);
          //printf("priority=%lld\n", LL param.sched_priority);
        }
        return 1;
      }

    case Devc_destroy:
      { pthread_cond_t *cvp;
        // This runs on the interpreter thread
        //printf("devcommand: Devc_destroy called\n");
        pthread_mutex_lock(&irq_mutex);
        //printf("Devc_destroy: sending signal to dev=%lld\n",
        //        LL dp[Dcb_devid]);
        // Wakeup the device thread just in case it was waiting on its
        // condition variable.

	copyaddr((char*)&dp[Dcb_cvp], (char*)&cvp);
        pthread_cond_signal(cvp);
        //pthread_cond_signal((pthread_cond_t *)dp[Dcb_cvp]);

        // When the thread runs it will commit suicide

        while(dp[Dcb_devid]) {
          pthread_mutex_unlock(&irq_mutex);
          //printf("Devc_destroy: wait for dev %lld thread to die\n", LL devid);
          msecdelay(20); // Wait 1/50 second and try again
          pthread_mutex_lock(&irq_mutex);
        }
    
        //printf("Devc_detroy: dev %lld thread has died\n", LL devid);

        // The thread does not use these fields, so freeing them is safe
        { void *p1, *p2;
	  void *nulladdr = (void*)0;
	  copyaddr((char *)&dp[Dcb_threadp], (char*)&p1);
	  //BCPLWORD *p1 = (BCPLWORD *)dp[Dcb_threadp];
	  copyaddr((char *)&dp[Dcb_cvp], (char*)&p2);
          //BCPLWORD *p2 = (BCPLWORD *)dp[Dcb_cvp];
          copyaddr((char*)nulladdr, (char*)&dp[Dcb_threadp]);
          //dp[Dcb_threadp] = 0;
          dp[Dcb_cvp] = 0;

          pthread_mutex_unlock(&irq_mutex);

          if(p1) free(p1);
          if(p2) free(p2);
        }
        return 1;
      }

    case Devc_start:  // Start processing a new packet
      { pthread_cond_t *cvp;
	// This runs on the interpreter thread

        //if(devid==-2)
        //  printf("Devc_start: dev=%lld\n", LL devid);
        pthread_mutex_lock(&irq_mutex);
        // Tell the device thread that the wkq may be non empty
        dp[Dcb_flag] = 0;

        copyaddr((char*)&dp[Dcb_cvp], (char*)&cvp);
        pthread_cond_signal(cvp);

	//pthread_cond_signal((pthread_cond_t *) dp[Dcb_cvp]);
        pthread_mutex_unlock(&irq_mutex);
        return 1;
      }

    case Devc_stop:
      { pthread_cond_t *cvp;
        // This runs on the interpreter thread
        printf("Devc_stop: dev=%lld\n", LL devid);
        pthread_mutex_lock(&irq_mutex);
	
        copyaddr((char*)&dp[Dcb_cvp], (char*)&cvp);
        pthread_cond_signal(cvp);

 //        pthread_cond_signal((pthread_cond_t *) dp[Dcb_cvp]);
        pthread_mutex_unlock(&irq_mutex);
        return 1;
      }
    case Devc_setintson:
        // This runs on the interpreter thread
        printf("Devc_setintson: dev=%lld intson set to %lld\n",
               LL devid, LL arg);
        pthread_mutex_lock(&irq_mutex);
        dp[Dcb_intson] = arg;
        if (arg) {
          pthread_cond_t *cvp;	
          copyaddr((char*)&dp[Dcb_cvp], (char*)&cvp);
          pthread_cond_signal(cvp);

          //pthread_cond_signal((pthread_cond_t *) dp[Dcb_cvp]);
	}
        pthread_mutex_unlock(&irq_mutex);
        return 1;
  }

  //return 0;
}


/************************* CLK ******************************/


void *clkcode(void *dcbp)
{ // This runs on the clock thread
  BCPLWORD *dp = (BCPLWORD *)dcbp;
  BCPLWORD devid = dp[Dcb_devid];
  BCPLWORD count = 0;  // for debugging
  /*
  struct timeb tb;
  int secs, msecs;        // Time for next clk interrupt
  int tickpersecond = 50; // The default
  int msecspertick;
#ifdef forItanium
  tickspersecond = 5;
#endif

  printf("clkcode: entered tickspersecond = %lld\n", LL tickspersecond);
  msecspertick = 1000/tickspersecond;

  ftime(&tb);         // Get the current real time
  secs = tb.time;
  msecs = tb.millitm + msecspertick;
  while(msecs>=1000) { secs++; msecs -= 1000; }

  if(devid!=-1) printf("The clock must be device -1\n");
  //printf("dev %lld thread starting\n", LL devid);
  */

  while(1) {

    // The clk thread is now a dummy since clock interrupts are detected
    // by the interpreter.

    //if(++count%100 == 0) printf("\nclkcode: count=%lld\n", LL count);

    //printf("dev %lld thread trying to lock irq_mutex\n", LL devid);
    pthread_mutex_lock(&irq_mutex);
    //printf("dev %lld thread got irq_mutex\n", LL devid);
    //printf("clkcode: dcb op = %lld\n", LL dp[Dcb_op]);
    while(dp[Dcb_op]!=Devc_destroy) {
      pthread_cond_t *cvp;	
      copyaddr((char*)&dp[Dcb_cvp], (char*)&cvp);
      pthread_cond_wait(cvp, &irq_mutex);
      //pthread_cond_wait((pthread_cond_t *) dp[Dcb_cvp], &irq_mutex);
    }
    pthread_mutex_unlock(&irq_mutex);
    break;
  }
    /*
    //printf("clkcode: Dcb_intson %lld clkintson %lld insadebug %lld\n",
    //LL dp[Dcb_intson], LL W[rootnode+Rtn_clkintson], LL W[rootnode+Rtn_insadebug]);

    if( dp[Dcb_intson] &&
        W[rootnode+Rtn_clkintson] &&
        W[rootnode+Rtn_insadebug]==0) {
      // Cause a clock interrupt
      irqfifov[irqfifoq] = -1;          // The clock device is -1
      irqfifoq = (irqfifoq+1) & 1023;
      if(irqfifop==irqfifoq) // Possibly loose a very old interrupt
        irqfifop = (irqfifop+1) & 1023;
      //printf("dev %lld thread sending signal irq_cv\n", LL devid);

      //printf("dev %lld thread setting irq=1\n", LL devid);
      pthread_cond_signal(&irq_cv); // Wakeup the IDLE task, if necessary
    }

    pthread_mutex_unlock(&irq_mutex);
    //printf("dev %lld thread unlocked irq_mutex\n", LL devid);

    while(tb.time<secs || tb.millitm<msecs) // Wait for next Cintpos tick
    { usleep(2000);  // Poll the time every 2 msecs
      ftime(&tb);    // Get real time again
      //if(++count%50 == 0)
      //   printf("\nclkcode: in polling loop, count=%lld\n", LL count);
    }

    msecs = tb.millitm + msecspertick;
    while(msecs>=1000) { secs++; msecs -= 1000; }
    //printf("clkcode: msecs=%lld\n", LL msecs);
  } // End of while(1) loop
    */

  // This point is only reached when a destroy command is received.
  //pthread_mutex_unlock(&irq_mutex);
  //printf("dev %lld thread unlocked irq_mutex\n", LL devid);
  //printf("dev %lld thread committing suicide\n", LL devid);
  return 0;
}





/************************ TTYIN ***********************/


void *ttyincode(void *dcbp)
{ BCPLWORD *dp = (BCPLWORD *)dcbp;
  BCPLWORD devid = dp[Dcb_devid];
  BCPLWORD ch = 0; // polling ch
  W[rootnode+Rtn_lastch] = -3; // polling ch

  //printf("\nttyincode: entered\n");

  while(1) {
    // irq_mutex is unlocked
    while (W[rootnode+Rtn_lastch]!=-3) {
      // This polling input method is used by the standalone debugger,
      // see rch in BOOT.b
      msecdelay(20); // Sleep for 20 msecs, then try again
      //printf("\nttyincode: polling rtn_lastch\n");
    }

    if (inbuf) { // Feature added -- MR 18/01/06
      // Input taken from command line
      ch = inbuf_next();
      if (ch == EOF) { // inbuf is now empty
        if (reattach_stdin) {
          free(inbuf);
          inbuf = 0; // Don't try to read more characters from buffer
          // Continue with normal read from stdin 
          ch = Readch();
        }
      }
    } else {

      // Normal case, input from stdin

      //printf("ttyincode: calling Readch()\n");
      ch = Readch();   // Read a character from stdin
    }

    // ch now holds the next input character, either from the
    // command line or standard input (the keyboard).

    //printf("ttyincode: dcb op1 = %lld\n", LL dp[Dcb_op]);

    //printf("devices: ttyincode: ch = %lld '%c'\n", LL ch, ((ch==-1)?0:ch));
    //if(ch<0) exit(0);

    //printf("dev %lld thread trying to lock irq_mutex\n", LL devid);
    pthread_mutex_lock(&irq_mutex);
    //printf("dev %lld thread got irq_mutex\n", LL devid);

    W[rootnode+Rtn_lastch] = ch; // For sadebug polling input

    // Wait until the character is taken by sadebug indicated by lastch
    // in the rootnode being reset to pollingch (=-3),
    // or until it can be given to a packet, or a destroy command is received.

    while(W[rootnode+Rtn_lastch]!=-3 || dp[Dcb_op]==Devc_destroy) {
      //printf("devices: ttyincode: ch = %lld waiting to go dcb_irq=%lld\n",
      //        LL ch, LL dp[Dcb_irq]);
      BCPLWORD pkt = dp[Dcb_wkq];

      if(dp[Dcb_intson] &&             // ttyin interrupts are enabled
         dp[Dcb_irq]==0 &&             // and previous interrupt has been serviced
         pkt &&                        // and there is a packet packet waiting
                                       //     to receive the character
         W[rootnode+Rtn_insadebug]==0) // and we are not in sadebug.
      {
	//The character can be given to a waiting packet.
	//printf("devices: ttyincode: ch = %lld going to packet\n", LL ch);

	BCPLWORD *p = &W[pkt];
	p[Pkt_res1] = ch;          // The character.
	p[Pkt_res2] = 0;           // Indicates successful return.
        W[rootnode+Rtn_lastch]=-3; // Mark the character as taken.

	//printf("pkt: %lld %lld %lld %lld %lld\n",LL
	//p[0],LL p[1],LL p[2],LL p[3],LL p[4]);

        // Put the device id in the interrupt FIFO
	irqfifov[irqfifoq++] = dp[Dcb_devid];
	irqfifoq &= 1023;
	if(irqfifop==irqfifoq) // Loose a very old interrupt!!
	  irqfifop = (irqfifop+1) & 1023;

	//printf("dev %lld thread setting irq=1\n", LL devid);
	dp[Dcb_irq] = -1; /* Leave flag to indicate interrupt from this dev */

	//printf("dev %lld thread sending signal irq_cv\n", LL devid);
	// Wakeup the interpreter if it were suspended waiting for irq to be set
	pthread_cond_signal(&irq_cv);
	break;
      }

      // Sleep for 10 msec if the character has not been accepted
      // by sadebug or given to a packet.

      pthread_mutex_unlock(&irq_mutex);
      //printf("dev %lld thread unlocked irq_mutex\n", LL devid);
      //printf("ttyincode: sleep because ");
      //printf("lastch=%3d, intson=%lld dcbirq=%lld wkq=%lld insadebug=%lld\n",
      //     W[rootnode+Rtn_lastch],
      //     LL dp[Dcb_intson],
      //     LL dp[Dcb_irq],
      //     LL dp[Dcb_wkq],
      //     LL W[rootnode+Rtn_insadebug]
      //    );

      msecdelay(10);
      //printf("ttyincode: done sleeping for 5 msecs\n");
      //printf("dev %lld thread trying to lock irq_mutex\n", LL devid);
      pthread_mutex_lock(&irq_mutex);
      //printf("dev %lld thread got irq_mutex\n", LL devid);
    }

    //printf("ttyincode: out of loop\n");

    //printf("dev %lld thread waiting for cv\n", LL devid);
    // Wait on the DCB's cv until allowed to proceed
    //pthread_cond_wait((pthread_cond_t *) dp[Dcb_cvp], &irq_mutex);

    //printf("ttyincode: dcb op2 = %lld\n", LL dp[Dcb_op]);
    if(dp[Dcb_op]==Devc_destroy) break;

    pthread_mutex_unlock(&irq_mutex);
    //printf("dev %lld thread unlocked irq_mutex\n", LL devid);
  } // End of while(1) loop

  pthread_mutex_unlock(&irq_mutex);
  //printf("dev %lld thread unlocked irq_mutex\n", LL devid);
  //printf("dev %lld thread commiting suicide\n", LL devid);
  return 0;
}



/************************ TTYOUT ***********************/


void *ttyoutcode(void *dcbp)
{ BCPLWORD *dp = (BCPLWORD *)dcbp;
  BCPLWORD devid = dp[Dcb_devid];
  BCPLWORD pkt=0, ch=-1;
  //printf("\ndev %lld: loop entered\n", LL devid);

  while(1) {
    //printf("dev %lld thread trying to lock irq_mutex\n", LL devid);
    pthread_mutex_lock(&irq_mutex);
    //printf("dev %lld thread got irq_mutex\n", LL devid);
    //printf("ttyoutcode: dcb op1 = %lld wkq=%lld\n",
    //       LL dp[Dcb_op], LL dp[Dcb_wkq]);

    while(1) {
      pthread_cond_t *cvp;	
      //int i;
      pkt = dp[Dcb_wkq];
      //for(i=0; i<9; i++) printf("dp[%lld]=%lld\n", LL i, LL dp[i]);
      // Wait for the destroy command
      // or   for the wkq to be non empty
      //      and for flag=0
      if(dp[Dcb_op]==Devc_destroy) goto ret;
      if(pkt && dp[Dcb_flag]==0) break;
      //printf("dev %lld: currently pkt=%lld and flag=%lld\n",
      //       LL devid, LL pkt, LL dp[Dcb_flag]);
      //printf("dev %lld: cond wait for pkt and flag=0\n", LL devid);
      copyaddr((char*)&dp[Dcb_cvp], (char*)&cvp);
      pthread_cond_wait(cvp, &irq_mutex);
      //pthread_cond_wait((pthread_cond_t *) dp[Dcb_cvp], &irq_mutex);
      //printf("dev %lld: cond wait done\n", LL devid);
    }

    dp[Dcb_flag] = 1;
    //printf("dev %lld: Dcb_wkq=%lld pkt=%lld\n",
    //        LL devid, LL Dcb_wkq, LL pkt);
    //printf("dev %lld: op=%lld pkt=%lld\n",
    //        LL devid, LL dp[Dcb_op], LL pkt);
    ch = W[pkt+Pkt_arg1];
    pthread_mutex_unlock(&irq_mutex);
    //printf("dev %lld thread unlocked irq_mutex\n", LL devid);

    // The flag is zero and ch is a character to write
    //printf("dev %lld: writing ch %2x '%c'\n",
    //        LL devid, ch, (ch>=0) ? ch : '?');
    if(ch>=0) {
      /* Do ttyout operation */
      #if defined(forCYGWIN32)
      if(ch==10) putchar(13);
      #endif
      putchar(ch);
      fflush(stdout);
    }

    //printf("\nDev %lld operation done\n", LL devid);

    //printf("dev %lld thread trying to lock irq_mutex\n", LL devid);
    pthread_mutex_lock(&irq_mutex);
    //printf("dev %lld thread got irq_mutex\n", LL devid);
    //printf("ttyoutcode: dcb op1 = %lld\n", LL dp[Dcb_op]);
    if(dp[Dcb_op]==Devc_destroy) goto ret;

    // Wait for intson for this device
    while(dp[Dcb_intson]==0) {
      pthread_cond_t *cvp;	
      copyaddr((char*)&dp[Dcb_cvp], (char*)&cvp);
      pthread_cond_wait(cvp, &irq_mutex);
      //pthread_cond_wait((pthread_cond_t *) dp[Dcb_cvp], &irq_mutex);
    }
    //printf("dev %lld intson=%lld\n", LL devid, LL dp[Dcb_intson]);
    // The device can now request and interrupt
    dp[Dcb_flag] = 1;

    irqfifov[irqfifoq++] = dp[Dcb_devid];
    irqfifoq &= 1023;
    if(irqfifop==irqfifoq) // Loose an old interrupt!
      irqfifop = (irqfifop+1) & 1023;

    trpush(0xF3000000);
    //printf("dev %lld thread sending signal irq_cv\n", LL devid);
    // Wakeup the interpreter if it were suspended waiting for irq to be set
    pthread_cond_signal(&irq_cv);
    trpush(0xF3000001);

    pthread_mutex_unlock(&irq_mutex);
  }

 ret:
  pthread_mutex_unlock(&irq_mutex);
  //printf("dev %lld thread unlocked irq_mutex\n", LL devid);
  //printf("dev %lld thread commiting suicide\n", LL devid);
  pthread_exit(0);
  return 0;
}


/******************* FILEOP **********************************/


void *fileopcode(void *dcbp)
{ BCPLWORD *dp = (BCPLWORD *)dcbp;
  BCPLWORD devid = dp[Dcb_devid];
  BCPLWORD op=0;       // The command sent by devcommand(...)
  BCPLWORD rtnpkt=0;   // =1 when there is a pkt to return to cinterp

  //printf("\nfileopcode entered\n");

  // The following code repeated does:
  // (1) if command to do (op!=0)
  //     (2) do it and wait for it to complete.
  //     (3) possibly return a packet by putting the device id
  //         into the fifo queue.
  // (4) waiting for the next command.

  while(1) {
    pthread_mutex_lock(&irq_mutex);
    op = dp[Dcb_op];                 // Get the devcommand op
    dp[Dcb_op] = 0;

    //printf("fileopcode: devid:%lld processing op = %lld\n",
    //        LL devid, LL op);
    switch(op) {
      default: printf("fileop: Unknown op %lld\n", LL op);
               break;

      case 0:          break;  // No op present -- nothing to do

      case Devc_create:  break;

      case Devc_destroy: goto done;

      case Devc_start: // Process a new packet
        { BCPLWORD pkt = dp[Dcb_wkq];
	  BCPLWORD *p = &W[pkt];

          if (pkt==0) break; // No pkt in the wkq -- nothing to do

          pthread_mutex_unlock(&irq_mutex);

          // Process the packet
          rtnpkt = 1;   // Whatever happens the packet will be returned.

          // Note that this thread will only be servicing one packet
          // at a time and so there is no need to lock its variables
          // while it proceeds.  The cinterp thread can run in parallel.

          //printf("fileopdev: pkt=%lld type %lld from cortn %lld\n",
          //        LL pkt, LL p[Pkt_type], LL p[10]);
          switch(p[Pkt_type]) {
	    default: 
	      { int i;
                printf("fileopdev: Unknown pkt type %lld in Devc_start dev %lld\n",
                             LL p[Pkt_type], LL devid);
                printf("pkt=%lld\n", LL pkt);
                for(i=0; i<=10; i++) printf("%2i: %lld\n", i, LL W[pkt+i]);
                break;
              }

	    case 1: // typical fileop command
                p[Pkt_res1] = 123;
                break;

	    case 2: // typical fileop command
                p[Pkt_res1] = 234;
                break;
	  }

          pthread_mutex_lock(&irq_mutex);

          if(rtnpkt && dp[Dcb_intson]) {
            // Return the packet to the client task by putting
            // this device id into the irq fifo.
            rtnpkt = 0;
            //printf("fileopdev %lld thread setting irq=1\n", LL devid);
            irqfifov[irqfifoq++] = dp[Dcb_devid];
            irqfifoq &= 1023;
            if(irqfifop==irqfifoq) // Loose an old interrupt!!
              irqfifop = (irqfifop+1) & 1023;

            // Cinterp will not inspect the fifo until this thread
            // releases the irq lock. It will not reset irq to zero
            // before it obtains the irq lock.

            // It is necessary to signal irq_cv since 
            // cinterp may be waiting on irq_cv in Sys_waitirq
            //printf("fileopdev %lld thread sending signal irq_cv\n", LL devid);
            pthread_cond_signal(&irq_cv);
          }
          break;
	} // End of case Devc_start
    } // End of switch on Dcb_op

    pthread_mutex_unlock(&irq_mutex);

    // Wait on the DCB's cv until next device command arrives
    pthread_mutex_lock(&irq_mutex);
    while(dp[Dcb_op]==0)
    { //printf("fileopdev %lld thread waiting for cv\n", LL dp[Dcb_devid]);
      pthread_cond_t *cvp;	
      copyaddr((char*)&dp[Dcb_cvp], (char*)&cvp);
      pthread_cond_wait(cvp, &irq_mutex);
      //pthread_cond_wait((pthread_cond_t *) dp[Dcb_cvp], &irq_mutex);
      //printf("fileopdev %lld thread woken up\n", LL devid);
    }
    pthread_mutex_unlock(&irq_mutex);
    //printf("fileopdev %lld thread received command\n",
    //        LL devid, LL dp[Dcb_op]);

  } // End of device command loop

done:
  pthread_mutex_unlock(&irq_mutex);
  //printf("fileopdev %lld thread unlocked irq_mutex\n", LL devid);
  printf("fileopdev %lld thread committing suicide\n", LL devid);
  return 0;
}


/************************ TCPDEV ****************************/

BCPLWORD name2ipaddr(BCPLWORD name) { // name => ipaddr (host format)
  struct hostent *hp;
  char *hname = (char *)&W[name];
  char chv[128];
  char *cp = chv;
  int i;

  if (name==0) return INADDR_ANY;

  for (i=1; i<=*hname; i++) *cp++ = hname[i];
  *cp=0;

  { struct in_addr ipaddr; // Not BCPLWORD
    if(inet_aton(chv, &ipaddr)) return ntohl(ipaddr.s_addr);
  }

  hp = gethostbyname(chv);
  if(hp==NULL) return -1; // Unknown host
  return ntohl(((struct in_addr *)hp->h_addr)->s_addr);
}

BCPLWORD name2port(BCPLWORD name) { // name => port (host format)
  struct servent *sp;
  //  struct hostent *hp;
  char *endptr;
  short port;
  char *sname = (char *)&W[name];
  char chv[128];
  char *cp;
  int i;

  if(name==0) return -1;

  cp = chv;
  for (i=1; i<=*sname; i++) *cp++ = sname[i];
  *cp=0;
  port = strtol(chv, &endptr, 0);
  if(*endptr == '\0') {
    //printf("devices: returning port=%lld\n", LL port);
    return port;
  } else {
    //printf("devices: calling getservbyname\n");
    sp = getservbyname(chv, "tcp");
    if(sp==NULL) return -1;
    //printf("devices: calling ntohs(%lld)\n", LL sp->s_port);
    return ntohs(sp->s_port);
  }
}

// isconnected is taken from Snader's book p 185
int isconnected( int s, fd_set *rd, fd_set *wr, fd_set *ex)
{ int err;
  socklen_t len = sizeof(err);

  // When a connection is established the socket becomes writable.
  // If an error occurs it becomes both readable and writable.
  // We can't rely on this since connect may have succeeded and data
  // may be ready to read before inconnected is called. So if it is both
  // readable and writable we must check the error status (using getsockopt).
  errno = 0;    // assume no error
  if (!FD_ISSET(s, rd) && !FD_ISSET(s, wr)) {
//printf("isconnected finds that socket %lld is neither readable nor writable\n", LL s);
    return 0; // The socket is neither readable nor writable, so not connected
  }
//printf("isconnected: socket %lld is readable or writable or both\n", LL s);
  if( getsockopt(s, SOL_SOCKET, SO_ERROR, &err, &len)<0) {
//printf("isconnected: socket %lld has an error\n", LL s);
    return 0;  // The socket has an error, so not connected
  }
//printf("isconnected: socket %lld getsockopt => err=%lld\n", LL s, LL err);
  errno = err; // err ~= 0 if the socket has an error
  //if (err) {
  //  printf("isconnected finds that socket %lld has an error, errno=%lld\n", LL s, LL errno);
  //} else {
  //  printf("isconnected finds that socket %lld has no error\n", LL s);
  //}
  if (err==0) return -1;
  return 0;
}

// tcpdevcode only runs in a TCP device thread.
// The DCB fields op and arg are written to by devcommand running
// in the interpreter thread sychronised using irq_mutex.
// These are read and reset to zero by tcpdevcode synchronised
// using irq_mutex.

void *tcpdevcode(void *dcbp)
{ BCPLWORD *dp = (BCPLWORD *)dcbp;
  BCPLWORD dcb = dp-W;
  BCPLWORD devid = dp[Dcb_devid]; // The device id does not change
  BCPLWORD op=0;       // The command op  sent by devcommand(...)
  BCPLWORD arg=0;      // The command arg sent by devcommand(...)
  BCPLWORD rtnpkt=0;   // =1 when there is a packet to return to cinterp

  struct sockaddr_in peer;
  BCPLWORD s;              // A socket
  int sndsz = 1440;	// default ethernet mss

  //printf("\ntcpdevcode entered\n");

  // The following code loops checking
  // (1) if command to do (op!=0)
  //     (2) do it and wait for it to complete
  //     (3) possibly returning a packet by putting the device id
  //         into the fifo queue, and
  // (4) waiting for the next command.

  pthread_mutex_lock(&irq_mutex);
  // The only ways to unlock the irq mutex are:
  // (1) Execute a destroy command
  // (2) Start processing a packet operation
  // (3) Wait in pthread_cond_wait for another devcom command.
  while(1) {
    //pthread_mutex_lock(&irq_mutex);
    op    = dp[Dcb_op];      // Get the devcommand op
    arg   = dp[Dcb_arg];     // Get the devcommand arg
    dp[Dcb_op]  = 0;
    dp[Dcb_arg] = 0;

    if(devid != dp[Dcb_devid]) {
printf("tcpdevcode: bad DCB=%lld devid=%lld/%lld op=%lld arg=%lld\n",
        LL dcb, LL devid, LL dp[Dcb_devid], LL op, LL arg);
      pthread_mutex_unlock(&irq_mutex);
      return 0;
    }

    //printf("tcpdevcode: dcb=%lld devid=%lld/%lld op=%lld arg=%lld\n",
    //    LL dcb, LL devid, LL dp[Dcb_devid], LL op, LL arg);
    switch (op) {
      default: printf("tcpdev: Unknown op %lld\n", LL op);
printf("tcpdevcode: devid=%lld op=%lld arg=%lld\n", LL devid, LL op, LL arg);
               break;

      case 0:
//printf("tcpdev: op=0\n");
                         break;  // No op present -- nothing to do

      case Devc_create:
//printf("tcpdev: op=create\n");
	                 break;  // Do nothing

      case Devc_destroy: 
//printf("tcpdev: op=destroy\n");
//printf("tcpdevcode: destroying devid=%lld op=%lld arg=%lld\n",
//        LL devid, LL op, LL arg);
	                 dp[Dcb_devid] = 0;  // Mark as destroyed
                         pthread_mutex_unlock(&irq_mutex);
//printf("dev %lld thread unlocked irq_mutex\n", LL devid);
//printf("devices: devid=%lld thread committing suicide\n", LL devid);

                         // This thread will now commit suicide.
                         return 0;

      case Devc_start: // Process a new packet
//printf("tcpdev: op=start\n");
        { BCPLWORD pkt = dp[Dcb_wkq];
	  BCPLWORD *p  = &W[pkt];

          if (pkt==0) break; // No pkt in the wkq -- nothing to do
	                     // Wait for next command

          // Process the packet
          rtnpkt = 1;   // Whatever happens the packet will be returned.

          // Note that this thread will only be processing one packet
          // at a time and so there is no need to lock its variables
          // while it proceeds.  The cinterp thread can run in parallel
          // but, by convention, it will not be changing the content of
          // the packet.

          //printf("tcpdev: pkt=%lld type %lld from cortn %lld\n",
          //        LL pkt, LL p[Pkt_type], LL p[10]);

          // This thread can run in parallel with the interpreter
          // so unlock the irq mutex.
          pthread_mutex_unlock(&irq_mutex);

          switch(p[Pkt_type]) {
	    default: printf("tcpdev: Unknown pkt type %lld in Devc_start dev %lld\n",
                             LL p[Pkt_type], LL devid);
	             break;


	    case Tcp_name2ipaddr: // name -> ipaddr
	      //printf("tcpdev: name2ipaddr\n");
                p[Pkt_res1] = name2ipaddr(p[Pkt_arg1]);
                break;

	    case Tcp_name2port:   // name -> port
	      //printf("tcpdev: name2port\n");
                p[Pkt_res1] = name2port(p[Pkt_arg1]);
                break;

	    case Tcp_socket:
	      //printf("tcpdev: socket\n");
                p[Pkt_res1] = socket( AF_INET, SOCK_STREAM, 0 );
                // AF_INET specified IPv4 internet protocols
                // SOCK_STREAM specifies sequenced, reliable, two way,
                //         connection based byte stream.
                // 0       is the protocol (IP?)
                // res1 = -1   on error
                // res1 >= 0   the new socket descriptor
                break;

            case Tcp_reuseaddr:
              { BCPLWORD s = p[Pkt_arg1];
                BCPLWORD n = p[Pkt_arg2];
		//printf("tcpdev: reuseaddr sock=%lld n=%lld\n", LL s, LL n);
		p[Pkt_res1] = setsockopt( s, SOL_SOCKET, SO_REUSEADDR,
                                          ( char * )&n, sizeof( n ) );
                // s               The socket descriptor
                // SOL_SOCKET      Modify options at the socket level
                // SO_REUSEADDR    Cause bind to allow reuse of local addresses
                // n=1             Set option
                // n=0             Reset option
                // res1 = 0        Indicates success
                // res1 = -1       Indicates failure
                break;
              }

            case Tcp_sndbufsz:
              { BCPLWORD s  = p[Pkt_arg1];
                BCPLWORD sz = p[Pkt_arg2];
		//printf("tcpdev: sndbufsz sock=%lld sz=%lld\n",
		//        LL s, LL sz);
		p[Pkt_res1] = setsockopt( s, SOL_SOCKET, SO_SNDBUF,
                                          ( char * )&sz, sizeof( sz ) );
                // Set socket send buffer maximum size
                // res1 = 0        Indicates success
                // res1 = -1       Indicates failure
                break;
              }

            case Tcp_rcvbufsz:
              { BCPLWORD s  = p[Pkt_arg1];
                BCPLWORD sz = p[Pkt_arg2];
		//printf("tcpdev: rcvbufsz sock=%lld sz=%lld\n",
		//        LL s, LL sz);
		p[Pkt_res1] = setsockopt( s, SOL_SOCKET, SO_RCVBUF,
                                          ( char * )&sz, sizeof( sz ) );
                // Set socket receive buffer maximum size
                // res1 = 0        Indicates success
                // res1 = -1       Indicates failure
                break;
              }

            case Tcp_bind:
              { BCPLWORD s      = p[Pkt_arg1];
	        BCPLWORD ipaddr = p[Pkt_arg2]; // in host format
	        BCPLWORD port   = p[Pkt_arg3]; // in host format
                struct sockaddr_in addr;
//printf("tcpdev: bind sock=%lld ipaddr=%8x port=%lld\n",
//        LL s, LL ipaddr, LL port);
                bzero(&addr, sizeof(addr)); // See Snader book p200, MR 23/9/04
                addr.sin_family = AF_INET;
                addr.sin_port = htons(port);
                addr.sin_addr.s_addr = htonl(ipaddr);
                p[Pkt_res1] = bind( s, (struct sockaddr *)&addr,
                                    sizeof(addr) );
                // Assign local host and port numbers to a socket
                // res1 = 0        Indicates success
                // res1 = -1       Indicates failure
                break;
              }

            case Tcp_connect:
              // Return packet with
              // res1=0          Successful connection
              // res1=-1 res2>0  Error
              // res1=-1 res2=-1 Connection closed by remote host
              // res1=-1 res2=-2 Timeout
              // res1=-1 res2=-3 No connection yet (polling) ???

              // This has been reimplemented to use a timeouts
              // See pp183-185 of JC Snader's book
              { BCPLWORD s       = p[Pkt_arg1];
                BCPLWORD ipaddr  = p[Pkt_arg2];
                BCPLWORD port    = p[Pkt_arg3];
                BCPLWORD timeout = p[Pkt_arg4];  // MR 19/9/03
                // timeout is:
                //  0    means no timeout
                // -1    means polling -- not implemented
                // >0    means timeout of the specified number of msecs
                struct sockaddr_in peer;
//printf("tcpdev: connect sock=%lld ipaddr=%8x port=%lld timeout=%lld\n",
//       LL s, ipaddr, LL port, LL timeout);
                bzero(&peer, sizeof(peer)); // See Snader book p200, MR 23/9/04
                peer.sin_family = AF_INET;
                peer.sin_port = htons(port);
                peer.sin_addr.s_addr = htonl(ipaddr);

                if (timeout==0)
		{ // No timeout
                  p[Pkt_res1] = connect( s, ( struct sockaddr * )&peer,
                                         sizeof( peer ) );
                  p[Pkt_res2] = 1;
                  // Connect socket s to socket (ipaddr, port)
                  // res1= 0         Indicates success
                  // res1=-1 res2=1  Indicates failure
//printf("tcpdev: returned connect sock=%lld\n", LL s);
                  break;
                }

//printf("tcpdev: timeout not zero\n");
                if(timeout>0)
		{ // Connect with given timeout
                  // Connect will usually return very quickly
                  // indicating either a successful connection
                  // or that a connection cannot be made.
                  // On rare occasions the system make take
                  // a long time to decide in which case a
                  // timeout may occur. A timeout of 5 seconds
                  // should almost always be adequate.
                  fd_set rdevents;
                  fd_set wrevents;
                  fd_set exevents;
                  int rc;
                  struct timeval tv;
                  int flags = fcntl(s, F_GETFL, 0);
//printf("tcpdev: connect sock=%lld ipaddr=%8x port=%lld\n",
//        LL s, ipaddr, LLport);
//printf("tcpdev: pkt=%lld connect with timeout %lld\n",
//        LL pkt, LL timeout);

                  if (flags<0)
		  { // fcntl(F_GETFL) failed
//printf("tcpdev: connect with timeout %lld fcntl(F_GETFL) failed\n", LL timeout);
		    p[Pkt_res1] = -1;
                    p[Pkt_res2] =  0;
                    break;
		  }
//printf("tcpdev: connect with timeout %lld fcntl(F_GETFL) ok\n", LL timeout);
                  if(fcntl(s, F_SETFL, flags | O_NONBLOCK) < 0)
		  { // fcntl(F_SETFL) failed
//printf("tcpdev: connect with timeout %lld fcntl(F_SETFL) failed\n", LL timeout);
		    p[Pkt_res1] = -1;
                    p[Pkt_res2] =  0;
                    break;
		  }
//printf("tcpdev: connect with timeout %lld fcntl(F_SETFL) ok\n", LL timeout);

                  // Make non-blocking connect call
                  rc = connect(s, (struct sockaddr *)&peer, sizeof(peer));
                  if (rc && errno != EINPROGRESS)
		  { // non-blocking connect failed
//printf("tcpdev: non-blocking connect failed\n");
		    p[Pkt_res1] = -1;
                    p[Pkt_res2] =  0;
                    break;
		  }
//printf("tcpdev: non-blocking connect ok, rc=%d\n", rc);
                  if( rc==0)
		  { // already connected
//printf("tcpdev: already connected\n");
                    rc = fcntl(s, F_SETFL, flags);
                    if(rc<0)
                    { // fcntl(F_SETFL) failed
//printf("tcpdev: connect with timeout %lld fcntl(F_SETFL) failed\n", LL timeout);
		      p[Pkt_res1] = -1;
                      p[Pkt_res2] =  0;
                      break;
		    }
//printf("tcpdev: connect with timeout %lld fcntl(F_SETFL) ok\n", LL timeout);
                    p[Pkt_res1] = 0; // To indicate success
                    p[Pkt_res2] = 0;
                    break;
		  }

                  FD_ZERO( &rdevents);
                  FD_SET( s, &rdevents);
                  wrevents = rdevents;
                  exevents = rdevents;

                  tv.tv_sec = timeout / 1000;           // seconds
                  tv.tv_usec = (timeout % 1000) * 1000; // micro-seconds

//printf("tcpdev: connect with timeout %lld calling select s=%lld\n",
//        LL timeout, LL s);
                  //rc = select(s+1, &rdevents, &wrevents, &exevents, &tv);
                  rc = select(s+1, NULL, &wrevents, NULL, &tv);
//printf("tcpdev: select => rc=%d\n", rc);
                  if(rc<0) {
//printf("tcpdev: pkt=%lld connect with timeout %lld select failed\n",
//        LL pkt, LL timeout);
                    p[Pkt_res1] = -1;  // select failure
                    p[Pkt_res2] =  0;
                    break;
                  }
                  if(rc==0) {
//printf("tcpdev: pkt=%lld connect timed out\n", LL pkt);
                    p[Pkt_res1] = -2;  // Indicate timeout
                    p[Pkt_res2] =  0;
                    break;
                  }
//printf("tcpdev: pkt=%lld connect with timeout select did not time out\n",LL pkt);
                  if(isconnected(s, &rdevents, &wrevents, &exevents)) {
//printf("tcpdev: isconnected socket %lld => yes\n", LL s);
                    rc = fcntl(s, F_SETFL, flags);
                    if(rc<0) {
                      // fcntl(F_SETFL) failed
//printf("tcpdev: connect with timeout %lld fcntl(F_SETFL) failed\n", LLpkt, LL timeout);
		      p[Pkt_res1] = -1;
                      p[Pkt_res2] =  0;
                      break;
		    }
//printf("tcpdev: connect with timeout %lld fcntl(F_SETFL) ok\n", LL timeout);
//printf("tcpdev: connect Tcp_connect pkt returned with res1=0\n");
                    p[Pkt_res1] = 0; // To indicate success
                    p[Pkt_res2] = 0;
                    break;
		  }
//printf("tcpdev: connect with timeout %lld connect failed\n", LL timeout);
                  p[Pkt_res1] = -1; // To indicate failure
                  p[Pkt_res2] =  0;
                  break;
	        }
//printf("tcpdev: polling connect sock=%lld not implemented\n", LL s);
                p[Pkt_res1] = -1; // To indicate failure
                p[Pkt_res2] =  0;
                break;
              }

            case Tcp_listen:
              { BCPLWORD s = p[Pkt_arg1];
                BCPLWORD n = p[Pkt_arg2];
		//printf("tcpdev: calling listen sock=%lld n=%lld\n",
		//        LL s, LL n);
                p[Pkt_res1] = listen(s, n);
		//printf("tcpdev: listen sock=%lld n=%lld =>%lld\n",
		//        LL s, LL n, LL p[Pkt_res1]);
                // Specify a willingness to accept incoming calls
                // n               queue limit for incoming connections
                // res1 = 0        Indicates success
                // res1 = -1       Indicates failure
                break;
              }

	  case Tcp_accept: // a1: sock, a2: tcp, a4: timeout
	    // res1>0 = connection socket if successful
            // res1=0 res2=-1 connection closed by remote host
            // res1=0 res2=-2 timeout
            // res1=0 res2=-3 no connection yet (polling)
              { BCPLWORD s = p[Pkt_arg1];  // The listening socket
                struct sockaddr_in peer;
                socklen_t peerlen = sizeof(peer);
	        BCPLWORD timeout = p[Pkt_arg4];
                BCPLWORD res1=0, res2=0;
                BCPLWORD eno = 0, rc=0;
//printf("tcpdev: Tcp_accept listening sock=%lld timeout=%lld\n",
//        LL s, LL timeout);
//printf("tcpdev: pkt=%lld calling recv sock=%lld buf=%lld len=%lld timeout=%lld\n",
//        pkt, s, p[Pkt_arg2], len, timeout);

                if(timeout>0) { // accept with timeout
                  fd_set readmask;
                  struct timeval tv;
                  FD_ZERO( &readmask);
                  FD_SET( s, &readmask);
                  tv.tv_sec = timeout / 1000;           // seconds
                  tv.tv_usec = (timeout % 1000) * 1000; // micro-seconds
//printf("tcpdev: pkt=%lld accept with timeout=%lld\n", pkt, timeout);
                  rc = select(s+1, &readmask, NULL, NULL, &tv);
                  if(rc<0) {
//printf("tcpdev: pkt=%lld read with timeout select failed\n", pkt);
                    p[Pkt_res1] = 0;  // select failure
                    p[Pkt_res2] = 1;
                    break;
                  }
                  if(rc==0) {
//printf("tcpdev: pkt=%lld accept timed out\n", pkt);
                    p[Pkt_res1] = 0;  // select failure
                    p[Pkt_res2] = -2; // timeoutch = -2
                    break;
                  }
                  // accept should not block, but it just possible, so
                  // we must set the socket non-blocking mode.
                  //???????????????????????????????????????????????????
//printf("tcpdev: accept: a read event has occurred\n");
//printf("tcpdev: calling accept\n");
                  p[Pkt_res1] = accept(s, (struct sockaddr *)&peer, &peerlen);
                  p[Pkt_res2] = ntohl(peer.sin_addr.s_addr);
                  // Accept the first connection request from the queue
                  // res1 >= 0       A new socket for the connection
                  // res1 = -1       On error
                  // res2            IP address of the connecting machine
//printf("tcpdev: returned from accept res1=%lld\n", p[Pkt_res1]);
//printf("tcpdev: returned from accept errno=%lld  EAGAIN=%lld\n", errno, EAGAIN);
                  break;
                }

                if(timeout<0) { // polling
                  int flags = fcntl(s, F_GETFL, 0);
//printf("tcpdev: pkt=%lld polling read\n", pkt);
                  if(flags<0 || fcntl(s, F_SETFL, flags|O_NONBLOCK)<0) {
//printf("tcpdev: pkt=%lld polling read fcntl failure 1\n", LL pkt);
                    p[Pkt_res1] = 0;  // fcntl failure
                    p[Pkt_res2] = 1;
                    break;
                  }

                  p[Pkt_res1] = accept(s, (struct sockaddr *)&peer, &peerlen);
                  p[Pkt_res2] = ntohl(peer.sin_addr.s_addr);
                  // Accept the first connection request from the queue
                  // res1 >= 0       A new socket for the connection
                  // res1 = -1       On error
                  // res2            IP address of the connecting machine
                  if(p[Pkt_res1]==-1) {
                    p[Pkt_res1] = 0;
                    if(errno==EAGAIN) {
                      p[Pkt_res2] = -3; // no connection available yet
                    } else {
                      p[Pkt_res2] = 0;  // accept error
                    }
                  } else {
                    p[Pkt_res2] = -1; // connection closed if res1=0
                    eno = errno;
                  }
//printf("tcpdev: pkt=%lld polling recv sock=%lld buf=%lld len=%lld res1=%lld errno=%lld\n",
//         LL pkt, LL s, LL p[Pkt_arg2], LL len, p[Pkt_res1], eno);

                  if(fcntl(s, F_SETFL, flags)<0) {
//printf("tcpdev: pkt=%lld polling read fcntl failure 2\n", LL pkt);
                    p[Pkt_res1] = 0;  // fcntl failure
                    p[Pkt_res2] = 0;
                    break;
                  }

                  // Request up to len bytes into buf from socket s
                  // res1 >= 0   the number of chars received
                  // res = -1    on error
                  break;
                }

                // Accept without timeout

	        p[Pkt_res1] = accept(s, (struct sockaddr *)&peer, &peerlen);
                p[Pkt_res2] = ntohl(peer.sin_addr.s_addr);
                // Accept the first connection request from the queue
                // res1 >= 0       A new socket for the connection
                // res1 = -1       On error
                // res2            IP address of the connecting machine
//printf("tcpdev: returned from accept\n");
                eno = errno;
                // Request up to len bytes into buf from socket s
                // res1 >= 0   the number of chars received
                // res = -1    on error
//printf("tcpdev: pkt=%lld back from recv sock=%lld buf=%lld len=%lld res1=%lld errno=%lld\n",
//         LL pkt, LL s, LL p[Pkt_arg2], LL len, LL p[Pkt_res1], LL eno);
                break;
              }

            case Tcp_recv:
              { BCPLWORD s       =            p[Pkt_arg1];
	        char *buf     = (char *)&W[p[Pkt_arg2]];
	        BCPLWORD len     =            p[Pkt_arg3];
	        BCPLWORD timeout =            p[Pkt_arg4];
                BCPLWORD res1=0, res2=0;
                BCPLWORD eno = 0, rc=0;
//printf("tcpdev: pkt=%lld calling recv sock=%lld buf=%lld len=%lld timeout=%lld\n",
//        LL pkt, LL s, LL p[Pkt_arg2], LL len, LL timeout);

                if(timeout>0) { // Read with timeout
                  fd_set readmask;
                  struct timeval tv;
                  FD_ZERO( &readmask);
                  FD_SET( s, &readmask);
                  tv.tv_sec = timeout / 1000;           // seconds
                  tv.tv_usec = (timeout % 1000) * 1000; // micro-seconds
//printf("tcpdev: pkt=%lld read with timeout\n", LL pkt);
                  rc = select(s+1, &readmask, NULL, NULL, &tv);
                  if(rc<0) {
//printf("tcpdev: pkt=%lld read with timeout select failed\n", LL pkt);
                    p[Pkt_res1] = 0;  // select failure
                    p[Pkt_res2] = 0;
                    break;
                  }
                  if(rc==0) {
//printf("tcpdev: pkt=%lld read timed out\n", LL pkt);
                    p[Pkt_res1] = 0;  // select failure
                    p[Pkt_res2] = -2; // timeoutch = -2
                    break;
                  }
                  // recv should not block
                  p[Pkt_res1] = recv( s, buf, len, 0 );
                  p[Pkt_res2] = -1;   // endstreamch = -1
                  // res1 >= 0   the number of chars received
                  // res = -1    on error
//printf("tcpdev: pkt=%lld back from recv sock=%lld buf=%lld len=%lld res1=%lld errno=%lld\n",
//         LL pkt, LL s, LL p[Pkt_arg2], LL len, LL p[Pkt_res1], LL eno);
                  break;
                }
                if(timeout<0) { // polling
                  int flags = fcntl(s, F_GETFL, 0);
//printf("tcpdev: pkt=%lld polling read\n", LL pkt);
                  if(flags<0 || fcntl(s, F_SETFL, flags|O_NONBLOCK)<0) {
//printf("tcpdev: pkt=%lld polling read fcntl failure 1\n", LL pkt);
                    p[Pkt_res1] = 0;  // fcntl failure
                    p[Pkt_res2] = 0;
                    break;
                  }

                  p[Pkt_res1] = recv( s, buf, len, 0 );
                  if(p[Pkt_res1]==-1) {
                    p[Pkt_res1] = 0;
                    if(errno==EAGAIN) {
                      p[Pkt_res2] = -3; // no ch available yet
                    } else {
                      p[Pkt_res2] = 0;  // recv error
                    }
                  } else {
                    p[Pkt_res2] = -1; // EOF if res1=0
                    eno = errno;
                  }
//printf("tcpdev: pkt=%lld polling recv sock=%lld buf=%lld len=%lld res1=%lld errno=%lld\n",
//         LL pkt, LL s, LL p[Pkt_arg2], LL len, LL p[Pkt_res1], LL eno);

                  if(fcntl(s, F_SETFL, flags)<0) {
//printf("tcpdev: pkt=%lld polling read fcntl failure 2\n", LL pkt);
                    p[Pkt_res1] = 0;  // fcntl failure
                    p[Pkt_res2] = 0;
                    break;
                  }

                  // Request up to len bytes into buf from socket s
                  // res1 >= 0   the number of chars received
                  // res = -1    on error
                  break;
                }

                // Read without timeout
                p[Pkt_res1] = recv( s, buf, len, 0 );
                p[Pkt_res2] = -1;  // MR 15/4/03
                eno = errno;
                // Request up to len bytes into buf from socket s
                // res1 >= 0   the number of chars received
                // res = -1    on error
//printf("tcpdev: pkt=%lld back from recv sock=%lld buf=%lld len=%lld res1=%lld errno=%lld\n",
//         LL pkt, LL s, LL p[Pkt_arg2], LL len, LLp[Pkt_res1], eno);
                break;
              }

            case Tcp_send:
              // This should be re-implemented using select to
              // deal with timeout delays
              { BCPLWORD s   = p[Pkt_arg1];
	        char *buf = (char *)&W[p[Pkt_arg2]];
	        BCPLWORD len = p[Pkt_arg3];
	        BCPLWORD timeout = p[Pkt_arg4];
//printf("tcpdev: send sock=%lld buf=%lld len=%lld, timeout=%lld\n",
//           LL s, LL p[Pkt_arg2], LL len, LL timeout);
                p[Pkt_res1] = send( s, buf, len, 0 );
                // res1 is the number of chars sent or -1 if error
                // Send len bytes from buf to socket s
                // res1 >= 0   the number of chars sent
                // res = -1    on error
                break;
              }

            case Tcp_close:
              { BCPLWORD s = p[Pkt_arg1];
//printf("tcpdev: close sock=%lld\n", LL s);
                 p[Pkt_res1] = close(s);
                // Close socket s
                // res1 = 0        Indicates success
                // res1 = -1       Indicates failure
                break;
              }
	  }

          pthread_mutex_lock(&irq_mutex);

          if(rtnpkt && dp[Dcb_intson]) {
            // Return the packet to the client task by putting
            // this device id into the irq fifo. It is de-queued
            // from the DCB wkq by code in function irqrtn in BOOT.b
            rtnpkt = 0;
            //printf("tcpdev %lld thread setting irq=1\n", LL devid);
            irqfifov[irqfifoq++] = dp[Dcb_devid];
            irqfifoq &= 1023;
            if(irqfifop==irqfifoq) // Possibly loose a very old interrupt!!
              irqfifop = (irqfifop+1) & 1023;

            // Cinterp will not inspect the fifo until this thread
            // releases the irq lock. It will not reset irq to zero
            // before it obtains the irq lock.

            // It is necessary to signal irq_cv since 
            // cinterp may be waiting on irq_cv in the IDLE task
            //printf("dev %lld thread sending signal irq_cv\n", LL devid);
            pthread_cond_signal(&irq_cv);
          }
          break;
	} // End of case Devc_start
    } // End of switch on Dcb_op

    ////pthread_mutex_unlock(&irq_mutex);

    // Wait on the DCB's cv until next device command arrives
    ////pthread_mutex_lock(&irq_mutex);
    while(dp[Dcb_op]==0)
    { //printf("tcpdev %lld thread waiting for cv\n", LL devid);
      pthread_cond_t *cvp;	
      copyaddr((char*)&dp[Dcb_cvp], (char*)&cvp);
      pthread_cond_wait(cvp, &irq_mutex);
      //pthread_cond_wait((pthread_cond_t *) dp[Dcb_cvp], &irq_mutex);
      //printf("tcbdev %lld thread woken up, op=%lld\n",
      //        LL devid, LL dp[Dcb_op]);
    }
    ////pthread_mutex_unlock(&irq_mutex);
    // irq has been set to 1 and the interrupting devices id places in
    // the fifo. When the interpreter thread is next able to service an
    // interrupt it will cause the function intrtn (defined in BOOT.b) to
    // be called. This will process the interrupt by dequeing the packet
    // from the DCB and sending it to the client task. It will also start
    // the device again if there is another packet on the DCB.

    // Now go back and process this device command, if any.
  } // End of device command loop

//done:   This point is never reached
//printf("devices: ERROR: label done reached\n");
//  pthread_mutex_unlock(&irq_mutex);
  //printf("dev %lld thread unlocked irq_mutex\n", LL devid);
  //printf("devices: devid=%lld
  //thread committing suicide\n", LL devid);

  // This thread will now commit suicide.
  //  return 0;
}



