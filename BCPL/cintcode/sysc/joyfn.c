/*
This implements the BCPL interface with the joystick, if any.

Imlemented by Martin Richards (c) Januay 2018

History

20/01/2018
Initial implementation


This module defines the following functions:

fd :=joyopen()
            fd is a small nonnegative integer or -1.
            This is only called from dosys in cintsys.c or cintpos.c.
            It opens the joystick device /dev/input/js0 (for Linux)
            and initialises the all the joystick variables in the
            rootnode to zero.
            If the joystick was successfully opened, it sets
            joybuttoncount, joyaxiscount and joyname appropriately.
            joyopen returns -1 if the joystick is not opened
            successfully, otherwise it returns the joystick's fd value.
            This value is copied into the rootnode variable joystckfd.
            The joystck is opened in non blocking mode so that read can
            poll for events. The result of read is negative if there
            are no outstanding events.

joyclose(fd)
            Close the specified joystick device.

joyclear()  Set put the rootnode value currjoybutton into joybuttons.

            The joystick values in the rootnode are:

            joystickfd       The file descriptor for the joystick,
                             or -1.
            joybuttoncount   Either zero or the number of buttons.
            joyaxiscount     Either zero or the number of axes.
            joyname          This is zero or the name of the joystick
                             as a BCPL string.
            joycurrbuttons   Set of button currently being pressed,
                             Bits are set by button pressed events
                             and cleared by button release events.
                             This bit pattern is ORed into joybuttons
                             every time a button is pressed. 
            joybuttons       Set of buttons that have been pressed
                             since last call of joyclear.
            joyaxis0         The current value of axes 0
            joyaxis1         The current value of axes 1
            joyaxis2         The current value of axes 2
            joyaxis3         The current value of axes 3
            joyaxis4         The current value of axes 4
            joyaxis5         The current value of axes 5
            joyaxis6         The current value of axes 6

Note that my Saitek Cyborg X joystick has 14 buttons and 7 axes.

joyclear()  This copies joycurrbuttons into joybuttons.

joyscan(fd, W)
            Process any outstanding events from the joystick
            with file descriptor fd transferring the information
            received to the joysticj values in the rootnode.
            This function is typically called about 50 times
            per second by the interpreter.

All axis values are in the range -32767 and +32767

The application program typically repeated reads joybuttons and the axis values
calling joyclear each time.
*/

#include "cintmain.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#ifdef JSAvail
#include <linux/joystick.h>
#endif

// These must agree with the declarations in g/joy.h
#define joy_avail           0
#define joy_open            1
#define joy_close           2
#define joy_clear           3

BCPLWORD joyopen() {
  // Only called from dosys in cintsys.c or cintpos.c
  char *filename = "/dev/input/js0";
  int fd = -1;

  
#ifdef JSAvail
  fd = open(filename, O_RDONLY | O_NONBLOCK);
  char name[128];
  char number_of_axes = 0;
  char number_of_buttons = 0;

  if (fd<0) return 0;

  if (ioctl(fd, JSIOCGNAME(sizeof(name)), name) < 0)
    strncpy(name, "Unknown", sizeof(name));

  ioctl(fd, JSIOCGBUTTONS, &number_of_buttons);
  ioctl(fd, JSIOCGAXES, &number_of_axes);

  printf("Joystick name: %s has %d buttons and %d axes\n",
	 name, number_of_buttons, number_of_axes);
#endif

  return fd;
}

void joyclose(int fd) {
  if (fd) close(fd);
}

void joyscan(int fd, BCPLWORD *g, BCPLWORD *W) {
  // This is called frequently from the Cintcode interpreter.
  //printf("joyscan(%d) called\n", fd);

#ifdef JSAvail
  while(1) {
    // Deal with all outstanding joystick events.
    struct js_event e;
    int rc = read(fd, &e, sizeof(e));
    //printf("%d %d %d %d %d\n", rc, e.time, e.value, e.type, e.number);

    if (rc<=0) return; // No outstanding events

    e.type = e.type & ~JS_EVENT_INIT;

    if (e.type==JS_EVENT_BUTTON ) {
      BCPLWORD butno = e.number;
      BCPLWORD bit = 1 << butno;
      if (e.value) {
        //printf("Button %d pressed\n", butno);
        W[rootnode + Rtn_joycurrbuttons] |= bit;
        W[rootnode + Rtn_joybuttons]     |= bit;
      } else {
        //printf("Button #%d released\n", butno);
        W[rootnode + Rtn_joycurrbuttons] &= ~bit;
      }
    }

    if (e.type==JS_EVENT_AXIS ) {
      int axisno = e.number;
      int value  = e.value;
      printf("Axis %d value %d\n", axisno, value);
      if (0<=axisno && axisno<=6)
        W[rootnode + Rtn_joyaxis0 + axisno] =  value;
    }
  }
#else
  return;
#endif
}

BCPLWORD joyfn(BCPLWORD *a, BCPLWORD *g, BCPLWORD *W) {
  int fd = -1;
  char tmpstr[256];

  //printf("joyfn: fno=%d a1=%d a2=%d a3=%d a4=%d\n",
  //        a[0], a[1], a[2], a[3], a[4]);

  switch(a[0]) {
  default:
    printf("joyfn: Unknown op: fno=%ld a1=%ld a2=%ld a3=%ld a4=%ld\n",
	   (long)a[0], (long)a[1], (long)a[2], (long)a[3], (long)a[4]);
    return 0;

  case joy_avail: // Test whether JSAavail is set
#ifdef JSAvail
    return -1;    // JSAvail is set
#else
    return 0;     // JSAvail is not set
#endif

  case joy_open:  // Open the joystick device
    fd = joyopen();
    W[rootnode+Rtn_joystickfd] = fd;
    return fd;

  case joy_close: // Close the joystick device
    if (a[1]) close((int)a[1]);
    return -1;

  case joy_clear: // Copy currbutton into buttons
    W[rootnode+Rtn_joybuttons] = W[rootnode+Rtn_joycurrbuttons];
    return -1;
  }
}

  
