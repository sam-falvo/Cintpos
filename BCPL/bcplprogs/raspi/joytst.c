// This is a test program for the joystick

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <linux/joystick.h>


//struct js_event {
//  unsigned int time;
//  short value;
//  unsigned char type;
//  unsigned char number;
//};

#define JS_EVENT_BUTTON 0x01   // Button pressed or released
#define JS_EVENT_AXIS 0x02     // Joystck moved
#define JS_EVENT_INIT 0x80     // Initial state of the device

int main() {
  char *filename = "/dev/input/js0";
  int fd = open(filename, O_RDONLY | O_NONBLOCK);
  struct js_event e;
  char name[128];
  char number_of_axes = 0;
  char number_of_buttons = 0;

  if (fd<0) {
    printf("Unable to open %s\n", filename);
    return 0;
  } else {
    printf("%s opened successfully\n", filename);
  }

  if (ioctl(fd, JSIOCGNAME(sizeof(name)), name) < 0)
    strncpy(name, "Unknown", sizeof(name));

  ioctl(fd, JSIOCGBUTTONS, &number_of_buttons);
  ioctl(fd, JSIOCGAXES, &number_of_axes);

  printf("Joystick name: %s has %d buttons and %d axes\n",
	 name, number_of_buttons, number_of_axes);
  
  while(1) {
    int rc = read(fd, &e, sizeof(e));
    //printf("%d %d %d %d %d\n", rc, e.time, e.value, e.type, e.number);

    if (rc>0) {
      if (e.type == JS_EVENT_BUTTON || e.type == JS_EVENT_AXIS) {
        if (e.type==JS_EVENT_BUTTON )
	  printf("button #%d value %d\n", e.number, e.value);
        else
          printf("axis #%d value %d\n", e.number, e.value);
      } else {
        printf("Init events\n");
      }
      continue;
    }
    usleep(200000);
  }

  return 0;
}
