/*
** This module contains the code to handle devices.
**
** (c) Copyright:  Martin Richards  October 2002
*/

/*
4/2/02  Initial version implemented.
*/



#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <pthread.h>

/* cinterp.h contains machine/system dependent #defines  */
#include "cinterp.h"

#include <fcntl.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/timeb.h>

/* include for the TCP/IP code */

#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>

// The DCB structure
#define Dcb_type       0
#define Dcb_devid      1
#define Dcb_wkq        2
#define Dcb_op         3
#define Dcb_threadp    4
#define Dcb_cvp        5
#define Dcb_intson     6
#define Dcb_irq        7

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


extern INT32 *W;
extern int irq;
extern pthread_mutex_t irq_mutex;
extern pthread_cond_t irq_cv;

void *clkcode    (void *dp);
void *ttyincode  (void *dp);
void *ttyoutcode (void *dp);
void *fileopcode (void *dp);
void *tcpdevcode (void *dp);

INT32 irqfifov[1024];         // This holds a fifo of dcb pointers
INT32 irqfifop=0, irqfifoq=0; // fifo queue from p to q-1


INT32 initdevices() {
  // Initialise the fifo to contain no device interrupt requests.
  int i;
  for (i=0; i<1024; i++) irqfifov[i] = 0;
  irqfifop=irqfifoq=0;
}

/*
devcommand(dcb, com, arg) causes the device with the given dcb
to execute command com with argument arg. The commands are:

Devc_create     Create a condition variable and thread for the device.
Devc_destroy    Stop the device, generate interrupts to return
                all the packets from the wkq, let the thread comit suicide
                and deallocate the condition variable.
Devc_start      Signal to the device thread that there is a packet
                to process at the head of the wkq.
Devc_stop       Abort the current I/O operation, if any, and put the
                device in stopped state. In this state a device will
                not look at packets on its wkq or generate interrupts,
                however there may still be interrupt requests in the fifo.
Devc_setintson  Set the intson state for the device to a1.
                a1=TRUE  enable interrupts and wakeup the device thread
                a1=FALSE disable further interrupts from this device.
                         Note that there may still be interrupts for this
                         device in the fifo.

After a device has put an interrupt request into the fifo and before
it receives another command (usually Devc_start), the kernel is free
to add or remove packets from the wkq.
              
*/
  

// devcommand is invoked by sys(Sys_devcom, com, arg)

INT32 devcommand(INT32 dcb, INT32 com, INT32 arg) {
  INT32 *dp     = (INT32 *)&W[dcb];
  INT32 devtype = dp[Dcb_type];
  INT32 devid   = dp[Dcb_devid];

  //printf("devcom: devid=%d com %d\n", dp[Dcb_devid], com);

  // Put com and args into the dcb (safely) - its thread may be running

  pthread_mutex_lock(&irq_mutex);
  dp[Dcb_op]   = com;
  //pthread_cond_signal((pthread_cond_t *)dp[Dcb_cvp]);
  pthread_mutex_unlock(&irq_mutex);


  // Perform a command: create, destroy, start, stop or setintson
  switch(com) {
    default: printf("devcommand: Bad device command %d\n", com);
             return 0;

    case Devc_create:
      { int rc=1;
        pthread_t *threadp = 
                  (pthread_t *)malloc(sizeof(pthread_t));
        pthread_cond_t *cvp =
                  (pthread_cond_t *)malloc(sizeof(pthread_cond_t));
	//printf("sizeof(pthread_t) = %d\n", sizeof(pthread_t));
	//printf("sizeof(pthread_cond_t) = %d\n", sizeof(pthread_cond_t));
        if(threadp==0 || cvp==0) {
          printf("devcommand: malloc failed\n");
          if (threadp) free(threadp);
          if (cvp)     free(cvp);
          return 0;
        }
	pthread_cond_init(cvp, NULL);
        pthread_mutex_lock(&irq_mutex);
        dp[Dcb_cvp]     = (INT32)cvp;
        dp[Dcb_threadp] = (INT32)threadp;
        pthread_mutex_unlock(&irq_mutex);

        // Create the device thread (with appropriate code)

      sw:
        switch (dp[Dcb_type]) {
	  default: printf("devcommand: Unknown device type %d\n",
                   dp[Dcb_type]);
                   return 0;

          case Devt_clk:
            rc =  pthread_create(threadp, 0, clkcode, dp);
            break;

          case Devt_ttyin:
            rc =  pthread_create(threadp, 0, ttyincode, dp);
            break;

          case Devt_ttyout:
            rc =  pthread_create(threadp, 0, ttyoutcode, dp);
            break;

          case Devt_fileop:
            rc =  pthread_create(threadp, 0, fileopcode, dp);
            break;

          case Devt_tcpdev:
            rc =  pthread_create(threadp, 0, tcpdevcode, dp);
            break;

        }

        if (rc) {
          perror("devcommand"); // Print a system error message
	  printf("rc=%d errno=%n\n",rc, errno);
          printf("devcommand: Unable to create thread for device %d\n", devid);
          return 0;
        }
        rc =  pthread_detach(*threadp);
        if (rc) {
          printf("devcommand: Unable to detach thread for device %d\n", devid);
          return 0;
        }
	//printf("devcommand: Devc_create done\n");
        return 1;
      }

    case Devc_destroy:
      //printf("devcommand: Devc_destroy called\n");
        pthread_mutex_lock(&irq_mutex);
        //printf("devcommand: Devc_destroy sending signal to thread\n");
        pthread_cond_signal((pthread_cond_t *)dp[Dcb_cvp]);
        // The thread should now begin to commit suicide
        pthread_mutex_unlock(&irq_mutex);

        // The thread does not use these fields, so freeing them is safe
        if(dp[Dcb_threadp]) {
          free((INT32 *)dp[Dcb_threadp]);
          dp[Dcb_threadp] = 0;
	}
        if(dp[Dcb_cvp]) {
          free((INT32 *)dp[Dcb_cvp]);
          dp[Dcb_cvp] = 0;
	}

        return 1;

    case Devc_start:  // Start processing a new packet
      { int rc=1;
        pthread_cond_t *cvp = 0;
        //printf("devcommand: wakeup  thread\n");
        pthread_mutex_lock(&irq_mutex);

        cvp = (pthread_cond_t *) dp[Dcb_cvp];
        pthread_cond_signal(cvp);
        pthread_mutex_unlock(&irq_mutex);
        return 1;
      }

    case Devc_stop:
	printf("devcommand: Devc_stop not implemented yet\n");
        return 1;

    case Devc_setintson:
        pthread_mutex_lock(&irq_mutex);
        dp[Dcb_intson] = arg;
        if(arg) {
          pthread_cond_t *cvp = (pthread_cond_t *) dp[Dcb_cvp];
          pthread_cond_signal(cvp);
	}
        pthread_mutex_unlock(&irq_mutex);
	printf("devcommand: Devc_setintson not implemented yet\n");
        return 1;
  }

  return 0;
}


/************************* CLK ******************************/


void *clkcode(void *dcbp)
{ INT32 *dp = (INT32 *)dcbp;
  INT32 devid = dp[Dcb_devid];

  struct timeb tb;
  int msecs=0;
  ftime(&tb);         // Get the current real time
  msecs = tb.millitm;

  if(devid!=-1) printf("The clock must be device -1\n");
  //printf("dev %d thread starting\n", devid);

  while(1) {
    //printf("dev %d thread trying to lock irq_mutex\n", devid);
    pthread_mutex_lock(&irq_mutex);
    //printf("dev %d thread got irq_mutex\n", devid);
    //printf("clkcode: dcb op = %d\n", dp[Dcb_op]);
    if(dp[Dcb_op]==Devc_destroy) break;

    //printf("clkcode: Dcb_intson %d clkintson %d insadebug %d\n",
    //dp[Dcb_intson], W[rootnode+Rtn_clkintson], W[rootnode+Rtn_insadebug]);

    if( dp[Dcb_intson] &&
        W[rootnode+Rtn_clkintson] &&
        W[rootnode+Rtn_insadebug]==0) {
      irqfifov[irqfifoq++] = dp[Dcb_devid];
      irqfifoq &= 1023;
      if(irqfifop==irqfifoq)           
        irqfifop = (irqfifop+1) & 1023; // Lose a very old interrupt!!
      //printf("dev %d thread sending signal irq_cv\n", devid);

      //printf("dev %d thread setting irq=1\n", devid);
      irq = 1;        /* Tell cinterp that there is an interrupt    */
      pthread_cond_signal(&irq_cv); // Wakeup the IDLE task if necessary
    }

    //printf("dev %d thread waiting for cv\n");
    // Wait on the DCB's cv until allowed to proceed
    //pthread_cond_wait((pthread_cond_t *) dp[Dcb_cvp], &irq_mutex);
    //printf("clkcode: dcb op2 = %d\n", dp[Dcb_op]);
    if(dp[Dcb_op]==Devc_destroy) break;
    pthread_mutex_unlock(&irq_mutex);
    //printf("dev %d thread unlocked irq_mutex\n", devid);

    while((tb.millitm+1000-msecs)%1000 < 20) // 20 msec ticks
    { usleep(2000);  // Poll the time every 2 msecs
      ftime(&tb);    // Get real time again
    }
    msecs = tb.millitm;
    //printf("clkcode: tick!  msecs=%d\n", msecs);
  }

  pthread_mutex_unlock(&irq_mutex);
  //printf("dev %d thread unlocked irq_mutex\n", devid);
  //printf("dev %d thread committing suicide\n", devid);
  return 0;
}





/************************ TTYIN ***********************/


void *ttyincode(void *dcbp)
{ INT32 *dp = (INT32 *)dcbp;
  INT32 devid = dp[Dcb_devid];
  INT32 ch = -1;

  //printf("\nttyincode entered\n");

  while(1) {
    //printf("dev %d thread trying to lock irq_mutex\n", devid);
    pthread_mutex_lock(&irq_mutex);
    //printf("dev %d thread got irq_mutex\n", devid);
    //printf("ttyincode: dcb op1 = %d\n", dp[Dcb_op]);
    if(dp[Dcb_op]==Devc_destroy) break;

    W[rootnode+Rtn_lastch] = ch; // For sadebug polling input

    if(ch>=0 && 
       dp[Dcb_intson] &&
       dp[Dcb_wkq] &&
       W[rootnode+Rtn_insadebug]==0) {
      // A character has been received and interrupt are enabled
      // and there is a packet
      INT32 *pkt = &W[dp[Dcb_wkq]];
      pkt[Pkt_res1] = ch;
      pkt[Pkt_res2] = 0;
      //printf("pkt: %d %d %d %d %d\n", pkt[0], pkt[1], pkt[2], pkt[3], pkt[4]);
      //printf("dev %d thread setting irq=1\n", devid);
      dp[Dcb_irq] = 1;  /* Leave flag to indicate interrupt from dev1 */
      irq = 1;          /* Tell cinterp that there is an interrupt    */
      irqfifov[irqfifoq++] = dp[Dcb_devid];
      irqfifoq &= 1023;
      if(irqfifop==irqfifoq) // Loose an old interrupt!
        irqfifop = (irqfifop+1) & 1023;
      //printf("dev %d thread sending signal irq_cv\n", devid);
      pthread_cond_signal(&irq_cv); // Wakeup the IDLE task if necessary
    }

    //printf("dev %d thread waiting for cv\n");
    // Wait on the DCB's cv until allowed to proceed
    //pthread_cond_wait((pthread_cond_t *) dp[Dcb_cvp], &irq_mutex);

    //printf("ttyincode: dcb op2 = %d\n", dp[Dcb_op]);
    if(dp[Dcb_op]==Devc_destroy) break;
    pthread_mutex_unlock(&irq_mutex);
    //printf("dev %d thread unlocked irq_mutex\n", devid);

    /* Do ttyin operation */
    //printf("ttyincode: sleep 4 secs\n");
    //sleep(4);
    //printf("ttyincode: calling Readch()\n");
    ch = Readch();          // Read next tty input character
    //if(ch==10) putchar(13); // read with echo
    //putchar(ch);
    //fflush(stdout);

    //printf("ttyincode: ch = %2x '%c'\n", ch, ch);
  }

  pthread_mutex_unlock(&irq_mutex);
  //printf("dev %d thread unlocked irq_mutex\n", devid);
  //printf("dev %d thread commiting suicide\n", devid);
  return 0;
}



/************************ TTYOUT ***********************/


void *ttyoutcode(void *dcbp)
{ INT32 *dp = (INT32 *)dcbp;
  INT32 devid = dp[Dcb_devid];
  INT32 pkt=0, ch=-1;
  //printf("\nttyoutcode entered, devid=%d\n", devid);

  while(1) {
    //printf("dev %d thread trying to lock irq_mutex\n", devid);
    pthread_mutex_lock(&irq_mutex);
    //printf("dev %d thread got irq_mutex\n", devid);
    //printf("ttyoutcode: dcb op1 = %d\n", dp[Dcb_op]);
    if(dp[Dcb_op]==Devc_destroy) break;

    pkt = dp[Dcb_wkq];
    if (pkt) ch = W[pkt+Pkt_arg1];
    pthread_mutex_unlock(&irq_mutex);
    //printf("dev %d thread unlocked irq_mutex\n", devid);

    if(ch>=0) {
      //printf("ttyout: writing ch %2x '%c'\n", ch, ch);
      /* Do ttyout operation */
      if(ch==10) putchar(13);
      putchar(ch);
      fflush(stdout);
    }

    //printf("Dev %d operation done\n", devid);

    pthread_mutex_lock(&irq_mutex);
    //printf("dev %d thread got irq_mutex\n", devid);
    //printf("ttyoutcode: dcb op1 = %d\n", dp[Dcb_op]);
    if(dp[Dcb_op]==Devc_destroy) break;

    if(dp[Dcb_wkq] && dp[Dcb_intson]) {
      //printf("dev %d thread setting irq=1\n", devid);
      dp[Dcb_irq] = 1;  /* Leave flag to indicate interrupt from ttyout */
      irq = 1;          /* Tell cinterp that there is an interrupt    */
      irqfifov[irqfifoq++] = dp[Dcb_devid];
      irqfifoq &= 1023;
      if(irqfifop==irqfifoq) // Loose an old interrupt!
        irqfifop = (irqfifop+1) & 1023;
      //printf("dev %d thread sending signal irq_cv\n", devid);
      pthread_cond_signal(&irq_cv); // Wakeup the IDLE task if necessary
    }

    //printf("dev %d thread waiting for cv\n", devid);
    // Wait on the DCB's cv until allowed to proceed
    pthread_cond_wait((pthread_cond_t *) dp[Dcb_cvp], &irq_mutex);
    //printf("ttyoutcode: dcb op2 = %d\n", dp[Dcb_op]);
    if(dp[Dcb_op]==Devc_destroy) break;
    pthread_mutex_unlock(&irq_mutex);
  }

  pthread_mutex_unlock(&irq_mutex);
  //printf("dev %d thread unlocked irq_mutex\n", devid);
  //printf("dev %d thread commiting suicide\n", devid);
  pthread_exit(0);
}


/******************* FILEOP **********************************/


void *fileopcode(void *dcbp)
{ INT32 *dp = (INT32 *)dcbp;
  INT32 devid = dp[Dcb_devid];
  INT32 op=0;       // The command sent by devcommand(...)
  INT32 rtnpkt=0;   // =1 when there is a pkt to return to cinterp

  printf("\nfileopcode entered\n");

  // The following code repeated does:
  // (1) if command to do (op!=0)
  //     (2) do it and wait for it to complete.
  //     (3) possibly return a packet by putting the device id
  //         into the fifo queue.
  // (4) waiting for the next command.

  while(1) {
    pthread_mutex_lock(&irq_mutex);
    op = dp[Dcb_op];  // Get the devcommand op
    dp[Dcb_op] = 0;

    //printf("fileopcode: devid:%d processing op = %d\n", devid, op);
    switch(op) {
      default: printf("fileop: Unknown op %d\n", op);
               break;

      case 0:          break;  // No op present -- nothing to do

      case Devc_create:  break;

      case Devc_destroy: goto done;

      case Devc_start: // Process a new packet
        { INT32 pkt = dp[Dcb_wkq];
	  INT32 *p = &W[pkt];

          if (pkt==0) break; // No pkt in the wkq -- nothing to do

          pthread_mutex_unlock(&irq_mutex);

          // Process the packet
          rtnpkt = 1;   // Whatever happens the packet will be returned.

          // Note that this thread will only be servicing one packet
          // at a time and so there is no need to lock its variables
          // while it proceeds.  The cinterp thread can run in parallel.

          //printf("fileopdev: pkt=%d type %d from cortn %d\n",
          //         pkt, p[Pkt_type], p[10]);
          switch(p[Pkt_type]) {
	    default: 
	      { int i;
                printf("fileopdev: Unknown pkt type %d in Devc_start dev %d\n",
                             p[Pkt_type], devid);
                printf("pkt=%d\n", pkt);
                for(i=0; i<=10; i++) printf("%2i: %d\n", i, W[pkt+i]);
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
            //printf("fileopdev %d thread setting irq=1\n", devid);
            irqfifov[irqfifoq++] = dp[Dcb_devid];
            irqfifoq &= 1023;
            if(irqfifop==irqfifoq) // Loose an old interrupt!!
              irqfifop = (irqfifop+1) & 1023;

            // Tell cinterp that there is a new interrupt request.
            irq = 1;
            // Cinterp will not inspect the fifo until this thread
            // releases the irq lock. It will not reset irq to zero
            // before it obtains the irq lock.

            // It is necessary to signal irq_cv since 
            // cinterp may be waiting on irq_cv in the IDLE task
            //printf("fileopdev %d thread sending signal irq_cv\n", devid);
            pthread_cond_signal(&irq_cv);
          }
          break;
	} // End of case Devc_start
    } // End of switch on Dcb_op

    pthread_mutex_unlock(&irq_mutex);

    // Wait on the DCB's cv until next device command arrives
    pthread_mutex_lock(&irq_mutex);
    while(dp[Dcb_op]==0)
    { //printf("fileopdev %d thread waiting for cv\n", dp[Dcb_devid]);
      pthread_cond_wait((pthread_cond_t *) dp[Dcb_cvp], &irq_mutex);
      //printf("fileopdev %d thread woken up\n", devid);
    }
    pthread_mutex_unlock(&irq_mutex);
  } // End of device command loop

done:
  pthread_mutex_unlock(&irq_mutex);
  //printf("fileopdev %d thread unlocked irq_mutex\n", devid);
  printf("fileopdev %d thread committing suicide\n", devid);
  return 0;
}


/************************ TCPDEV ****************************/

INT32 name2ipaddr(INT32 name) { // name => ipaddr (host format)
  struct hostent *hp;
  char *hname = (char *)&W[name];
  char chv[128];
  char *cp = chv;
  int i;
  INT32 ipaddr = -1;

  if (name==0) return INADDR_ANY;

  for (i=1; i<=*hname; i++) *cp++ = hname[i];
  *cp=0;
  if(inet_aton(chv, &ipaddr)) return ntohl(ipaddr);
  hp = gethostbyname(chv);
  if(hp==NULL) return -1; // Unknown host
  return ntohl(((struct in_addr *)hp->h_addr)->s_addr);
}

INT32 name2port(INT32 name) { // name => port (host format)
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
    return port;
  } else {
    sp = getservbyname(chv, "tcp");
    if(sp==NULL) return -1;
    return ntohs(sp->s_port);
  }
}




void *tcpdevcode(void *dcbp)
{ INT32 *dp = (INT32 *)dcbp;
  INT32 devid = dp[Dcb_devid];
  INT32 op=0;       // The command sent by devcommand(...)
  INT32 rtnpkt=0;   // =1 when there is to return to cinterp

  struct sockaddr_in peer;
  INT32 s; // A socket
  int sndsz = 1440;	/* default ethernet mss */

  //printf("\ntcpdevcode entered\n");

  // The following code loops checking
  // (1) if command to do (op!=0)
  //     (2) do it and wait for it to complete
  //     (3) possibly returning a packet by putting the device id
  //         into the fifo queue, and
  // (4) waiting for the next command.

  while(1) {
    pthread_mutex_lock(&irq_mutex);
    op = dp[Dcb_op];  // Get the devcommand op
    dp[Dcb_op] = 0;

    //printf("tcpdevcode: devid:%d processing op = %d\n", devid, op);
    switch(op) {
      default: printf("tcpdev: Unknown op %d\n", op);
               break;

      case 0:          break;  // No op present -- nothing to do

      case Devc_create:  break;

      case Devc_destroy: goto done;

      case Devc_start: // Process a new packet
        { INT32 pkt = dp[Dcb_wkq];
	  INT32 *p = &W[pkt];

          if (pkt==0) break; // No pkt in the wkq -- nothing to do

          pthread_mutex_unlock(&irq_mutex);

          // Process the packet
          rtnpkt = 1;   // Whatever happens the packet will be returned.

          // Note that this thread will only be servicing one packet
          // at a time and so there is no need to lock its variables
          // while it proceeds.  The cinterp thread can run in parallel.

          //printf("tcpdev: pkt=%d type %d from cortn %d\n",
          //         pkt, p[Pkt_type], p[10]);
          switch(p[Pkt_type]) {
	    default: printf("tcpdev: Unknown pkt type %d in Devc_start dev %d\n",
                             p[Pkt_type], devid);
	    { int i;
	      printf("pkt=%d\n", pkt);
	      for(i=0; i<=10; i++) printf("%2i: %d\n", i, W[pkt+i]);
            }
	             break;


	    case Tcp_name2ipaddr: // name -> ipaddr
                p[Pkt_res1] = name2ipaddr(p[Pkt_arg1]);
                break;

	    case Tcp_name2port:   // name -> port
                p[Pkt_res1] = name2port(p[Pkt_arg1]);
                break;

	    case Tcp_socket:
                p[Pkt_res1] = socket( AF_INET, SOCK_STREAM, 0 );
                break;

            case Tcp_reuseaddr:
              { INT32 s = p[Pkt_arg1];
                INT32 n = p[Pkt_arg2];
		p[Pkt_res1] = setsockopt( s, SOL_SOCKET, SO_REUSEADDR,
                                          ( char * )&n, sizeof( n ) );
                break;
              }

            case Tcp_sndbufsz:
              { INT32 s  = p[Pkt_arg1];
                INT32 sz = p[Pkt_arg2];
		p[Pkt_res1] = setsockopt( s, SOL_SOCKET, SO_SNDBUF,
                                          ( char * )&sz, sizeof( sz ) );
                break;
              }

            case Tcp_rcvbufsz:
              { INT32 s  = p[Pkt_arg1];
                INT32 sz = p[Pkt_arg2];
		p[Pkt_res1] = setsockopt( s, SOL_SOCKET, SO_RCVBUF,
                                          ( char * )&sz, sizeof( sz ) );
                break;
              }

            case Tcp_bind:
              { INT32 s      = p[Pkt_arg1];
	        INT32 ipaddr = p[Pkt_arg2]; // host format
	        INT32 port   = p[Pkt_arg3]; // host format
                struct sockaddr_in addr;
                addr.sin_family = AF_INET;
                addr.sin_port = htons(port);
                addr.sin_addr.s_addr = htonl(ipaddr);
                p[Pkt_res1] = bind( s, ( struct sockaddr * )&addr,
                                    sizeof( addr ) );
                break;
              }

            case Tcp_connect:
              { INT32 s      = p[Pkt_arg1];
                INT32 ipaddr = p[Pkt_arg2];
                INT32 port   = p[Pkt_arg3];
                struct sockaddr_in peer;
                peer.sin_family = AF_INET;
                peer.sin_port = htons(port);
                peer.sin_addr.s_addr = htonl(ipaddr);
                p[Pkt_res1] = connect( s, ( struct sockaddr * )&peer,
                                       sizeof( peer ) );
                break;
              }

            case Tcp_listen:
              { INT32 s = p[Pkt_arg1];
                INT32 n = p[Pkt_arg2];
                p[Pkt_res1] = listen(s, n);
                break;
              }

            case Tcp_accept:
              { INT32 s = p[Pkt_arg1];
                INT32 n = p[Pkt_arg2];
                struct sockaddr_in peer;
                int peerlen = sizeof(peer);
	        p[Pkt_res1] = accept(s, (struct sockaddr *)&peer, &peerlen);
                p[Pkt_res2] = ntohl(peer.sin_addr.s_addr);
                break;
              }

            case Tcp_recv:
              { INT32 s = p[Pkt_arg1];
	        char *buf = (char *)&W[p[Pkt_arg2]];
	        INT32 len = p[Pkt_arg3];
                p[Pkt_res1] = recv( s, buf, len, 0 );
                // res1 is the number of chars received or -1 if error
                break;
              }

            case Tcp_send:
              { INT32 s   = p[Pkt_arg1];
	        char *buf = (char *)&W[p[Pkt_arg2]];
	        INT32 len = p[Pkt_arg3];
                p[Pkt_res1] = send( s, buf, len, 0 );
                // res1 is the number of chars sent or -1 if error
                break;
              }

            case Tcp_close:
              { INT32 s = p[Pkt_arg1];
                close(s);
                break;
              }
	  }
          pthread_mutex_lock(&irq_mutex);


          if(rtnpkt && dp[Dcb_intson]) {
            // Return the packet to the client task by putting
            // this device id into the irq fifo.
            rtnpkt = 0;
            //printf("tcpdev %d thread setting irq=1\n", devid);
            irqfifov[irqfifoq++] = dp[Dcb_devid];
            irqfifoq &= 1023;
            if(irqfifop==irqfifoq) // Loose an old interrupt!!
              irqfifop = (irqfifop+1) & 1023;

            // Tell cinterp that there is a new interrupt request.
            irq = 1;
            // Cinterp will not inspect the fifo until this thread
            // releases the irq lock. It will not reset irq to zero
            // before it obtains the irq lock.

            // It is necessary to signal irq_cv since 
            // cinterp may be waiting on irq_cv in the IDLE task
            //printf("dev %d thread sending signal irq_cv\n", devid);
            pthread_cond_signal(&irq_cv);
          }
          break;
	} // End of case Devc_start
    } // End of switch on Dcb_op

    pthread_mutex_unlock(&irq_mutex);

    // Wait on the DCB's cv until next device command arrives
    pthread_mutex_lock(&irq_mutex);
    while(dp[Dcb_op]==0)
    { //printf("tcpdev %d thread waiting for cv\n", dp[Dcb_devid]);
      pthread_cond_wait((pthread_cond_t *) dp[Dcb_cvp], &irq_mutex);
      //printf("tcbdev %d thread woken up\n", devid);
    }
    pthread_mutex_unlock(&irq_mutex);
  } // End of device command loop

done:
  pthread_mutex_unlock(&irq_mutex);
  //printf("dev %d thread unlocked irq_mutex\n", devid);
  //printf("dev %d thread committing suicide\n", devid);
  return 0;
}



