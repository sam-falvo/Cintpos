/*
This program connects stdin and stdout to a full duplex TCP/IP connection with
with another machine.

Usage:

mconn [-h host] [-p port]

If port is omitted, port 9000 is used by default.  If host is omitted the
program waits for a TCP/IP connection by another machine using the specified
port on the local machine. If host is specified, the program attempts to make a
connection to the specified host and port.

Once a connection is established, it copies all input from stdin to the host
and all data received from the host goes to stdout.  This program can be killed
by typing ctrl-c, or by the host breaking the connection. When running under
xterm, it essentially acts as a vt100 terminal.

The program is based on examples in Jon Snader's book: "Effective TCP/IP
Programming".

Implementation by Martin Richards (c) March 2011

26/04/2011
This version works under both Linux and the Itanium running VMS.
*/


#include "cintpos.h"


/* includes for the TCP/IP code */

#include <netdb.h>
//#include <poll.h>

/* Functions defined in kblib.c  */
extern int Readch(void);
extern int pollReadch(void);
extern int init_keyb(void);
extern int close_keyb(void);
extern int intflag(void);

int connsock;
void (*old_inthandler)(int);

void inthandler(int sig)
{
  old_inthandler = signal(SIGINT, old_inthandler);

  if(connsock>0) close(connsock);
  close_keyb();
  exit(0);
}


void prerrno(int n) {
  /* Output an error message */
  char *err_str = strerror(n);
  printf("Error %d: %s\n", n, err_str);
}

int result2 = 0;

int name2ipaddr(char *hname) { // name => ipaddr (host format)
  struct hostent *hp;
  struct in_addr inaddr;

  memset(&inaddr, 0, sizeof(struct in_addr)); // Tip 27 of JC Snader

  if (hname==0) return INADDR_ANY;

  printf("name2ipaddr: %s\n", hname);

  if (inet_aton(hname, &inaddr)) return ntohl(inaddr.s_addr);

  hp = gethostbyname(hname);
  if (hp==NULL) return -1; // Unknown host

  return ntohl(((struct in_addr *)hp->h_addr)->s_addr);
}

int name2port(char *pname) { // name => port (host format)
  struct servent *sp;
  char *endptr;
  short port;

  if(pname==0) return -1;

  port = strtol(pname, &endptr, 0);
  if (*endptr == '\0') return port;

  sp = getservbyname(pname, "tcp");
  if (sp==NULL) return -1;
  return ntohs(sp->s_port);
}

int newsocket() {
  int rc = socket(AF_INET, SOCK_STREAM, 0);
  if (rc<0) prerrno(errno);
  return rc;
}

int reuseaddr(int s, int n) {
  int rc = setsockopt(s, SOL_SOCKET, SO_REUSEADDR,
                      (char *)&n, sizeof(n));
  if (rc<0) prerrno(errno);
  return rc;
}

int setsndbufsz(int s, int sz) {
  int rc = setsockopt(s, SOL_SOCKET, SO_SNDBUF,
                     (char *)&sz, sizeof(sz));
  // Under Open VMS this requires
  if (rc<0) prerrno(errno);
  return rc;
}

int setrcvbufsz(int s, int sz) {
  int rc = setsockopt(s, SOL_SOCKET, SO_RCVBUF,
                      (char *)&sz, sizeof(sz));
  if (rc<0) prerrno(errno);
  return rc;
}

int tcpbind(int s, int ipaddr, int port) {
  int rc;
  struct sockaddr_in addr;
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  addr.sin_addr.s_addr = htonl(ipaddr);
  //printf("mconn.c: tcpbind: %d %08x %d\n", s, ipaddr, port);
  rc = bind(s, (struct sockaddr *)&addr, sizeof(addr));
  if (rc<0) prerrno(errno);
  return rc;
}

int tcpconnect(int s, int ipaddr, int port) {
  int rc;
  struct sockaddr_in peer;
  peer.sin_family = AF_INET;
  peer.sin_port = htons(port);
  peer.sin_addr.s_addr = htonl(ipaddr);
  rc = connect(s, (struct sockaddr *)&peer, sizeof(peer));
  if (rc<0) prerrno(errno);
  return rc;
}

int tcplisten(int s, int n) {
  int rc = listen(s, n);
  if (rc<0) prerrno(errno);
  return rc;
}

int tcpaccept(int s) {
  struct sockaddr_in peer;
  socklen_t peerlen = sizeof(peer);  // socklen_t good
                                     // size_t bad
  int rc = accept(s, (struct sockaddr *)&peer, &peerlen);
  if (rc<0) prerrno(errno);
  result2 = ntohl(peer.sin_addr.s_addr);
  return rc;
}

int copydata(int s) {
  // Copy characters from stdin to socket s,
  // and characters from socket s to stdout.
  fd_set allreads;
  fd_set readmask;
  int rc;
  char buf[128];

  connsock = s; // So it can be closed on SIGINT
  old_inthandler = signal(SIGINT, inthandler);

  FD_ZERO(  &allreads);
  FD_SET(s, &allreads);  // the TCP/IP socket
  /// On some systems select only works on sockets!
  /// So use pollReadch on stdin.

  while(1) {
    readmask = allreads;
    /*
    printf("copydata: Calling select (%d,...)\n", s+1);
    */
    { int i;
      struct timeval timeout;
      timeout.tv_sec  = 0; // Timeout of 5 msecs
      timeout.tv_usec = 5000;

      // Test whether we can read from socket s.
      rc = select(s+1, &readmask, NULL, NULL, &timeout);


      if(rc<0) {
        prerrno(errno);
        printf("\nError: select(%d ...) returned %d\n", s, rc);
        break;
      }

      if(FD_ISSET(s, &readmask)) {
        // Characters can be read from socket s.

        rc = recv(s, buf, sizeof(buf)-1, 0);
	//printf("recv(%d,...) => %d\n\r", s, rc);
        // It read rc chars from socket s into buf
        if(rc==0) {
          printf("\nremote host disconnected\n\r");
          break;
        }
        if(rc<0) {
          prerrno(errno);
          printf("\nError: recv(%d ...) returned %d\n\r", s, rc);
          break;
        }
        // Write them to stdout
        for(i=0; i<rc; i++) {
          int ch = buf[i];
          //if(ch==13) putchar(10);
          putchar(ch);
        }
        fflush(stdout);
      }
    }

    // Test whether characters are available from stdin.
    //{ struct pollfd filedes[1];
    //  filedes[0].fd = 0; // stdin
    //  filedes[0].events = POLLIN; // Can we read
    //  filedes[0].revents = 0; 

    //  // I fear on VMS all fds in filedes must be sockets
    //  // as with select.
    //  rc = poll(filedes, 1, 4000); // Timeout = 4000 msecs
    //  if(rc<0) {
    //    prerrno(errno);
    //    printf("\nError: poll(...) returned %d\n", rc);
    //    break;
    //  }
    //  if(rc>0) {
    //    // read some characters from stdin
    //    printf("copydata: calling read(0 ...)\n");

    //    rc = read(0, buf, sizeof(buf)-1);
    //    if(rc==0) {
    //      printf("\ndisconnected -- EOF received\n");
    //      break;
    //    }

    //    // Send them to the socket
    //    rc = send(s, buf, rc, 0);
    //    if (rc < 0) {
    //      prerrno(errno);
    //      printf("\nerror: send(%d ...) failure\n", s);
    //      break;
    //    }
    //  }
    //}

    while(1)
    { // try to read a character from standard input
      int ch = pollReadch();
      char buf[1];
      if(ch==-3) break; // No character available
      //printf("pollReadch() => %2X\n\r", ch);
      if(ch==13) putchar(10); // Put CR after LF for the Itanium
      putchar(ch);
      //fflush(stdout);
      buf[0] = ch;
      rc = send(s, buf, 1, 0);
      fflush(0); // Flush all output streams
      if (rc < 0) {
        prerrno(errno);
        printf("\nerror: send(%d ...) failure\n", s);
        break;
      }
    }
    // Delay briefly  
    usleep(2000); // delay 2 msecs
  }

  return 0;
}

int client(int ipaddr, int port) {
  int s;
  int c;
  int bufsz = 4096;
  int sndsz = 1440;	/* default ethernet mss */

  s = newsocket();
  if ( s<0 ) {
    printf("\nsocket call failed" );
  }

  if ( reuseaddr(s, 1) < 0) {
    printf("\nreuseaddr failed" );
    return 0;
  }

  // setsndbufsz requires some priviledge on 
  // some versions of Open VMS.
  //if ( setsndbufsz( s, bufsz)) {
  //  printf("\nsetsockopt SO_SNDBUF failed" );
  //  return 0;
  //}

  if ( tcpconnect( s, ipaddr, port) ) {
    printf("\nconnect failed" );
    return 0;
  }

  printf("Client connection established via socket %d\n\r", s);

  copydata(s);

  close(s);
  return 0;
}

int server(int ipaddr, int port) {
  int s, s1;
  int c;
  int bufsz = 4096;
  int sndsz = 1440;	/* default ethernet mss */

again: // If the connection is closed by the client
       // start again here.

  s = newsocket();
  if ( s<0 ) {
    printf("socket call failed\n" );
  }

  if ( reuseaddr(s, 1) < 0) {
    printf("reuseaddr failed\n" );
    return 0;
  }

  //printf("calling bind, sock=%d ipaddr=%08x port=%d\n", s, ipaddr, port );

  if (tcpbind(s, ipaddr, port) < 0) {
    printf("bind failed, sock=%d ipaddr=%08x port=%d\n", s, ipaddr, port );
    return 0;
  }

  //if ( reuseaddr(s, 1) < 0) {
  //  printf("\nreuseaddr failed" );
  //  return 0;
  //}

  printf("Waiting for a connection request\n\r");

  if ( tcplisten( s, 5) < 0 ) {
    printf("listen failed\n" );
    return 0;
  }

  s1 = tcpaccept(s);

  if(s1<0) {
    printf("accept failed\n" );
    return 0;
  }

  close(s); // Close the listening socket

  printf("Server accepted connection via socket %d\n\r", s1);

  copydata(s1);

printf("server: return from copydata -- closing socket %d\n\r", s1);
  close(s1);

  printf("connection closed\n");
  goto again;
}


/* main program  */
int main( int argc, char **argv )
{ int s;
  int c;
  char *hostname=0;
  char *portname=0;
  int ipaddr=0, port=0;

  connsock = 0;

  opterr = 0;
  while ( ( c = getopt( argc, argv, "h:p:" ) ) != EOF )
  { switch ( c )
    { case 'h' :
        hostname = optarg;
        break;

      case 'p' :
        portname = optarg;
        break;

      case '?' :
        printf("usage: mconn [-h host] [-p port]\n");
        return 0;
    }
  }

  if(portname==0) portname = "9000";

  //if (hostname) printf("hostname: %s\n", hostname);
  //printf("portname: %s\n", portname);

  ipaddr = name2ipaddr(hostname);
  port   = name2port(portname);
  //printf("ipaddr: %8x  port: %d\n", ipaddr, port);

  init_keyb();

  if (hostname) client(ipaddr, port);
  else          server(ipaddr, port);

  close_keyb();

  printf("\nconnection closed\n");
}

