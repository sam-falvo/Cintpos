#include <stfio.h>
#include "defines.h"

int main() {
#ifdef SDFavail
  sys(Sys_shellcom, "sdl-config --cflags")
#endif
}
