// This program creates the file g/glmanifests.h for use by BCPL
// programs using OpenGL. It creates a dummy version if neither
// forLinuxGL nor forLinuxSDFGL is defined.

// Implemented by Martin Richards (c) May 2020

#include <stdio.h>
#include "cintmain.h"


#ifdef GLavail
#include <GL/gl.h>
#endif

void w(const char *str, int val) { // const needed to keep g++ happy.
  printf("%s = %d\n", str, val);
}

int main() {
  int val = 0;
  printf("// This file was created by sysc/mkglmanifests.c\n\n");
  printf("MANIFEST {\n");

#ifdef GLavail
  val = GL_VERTEX_ARRAY;
#endif
    w("GL_VERTEX_ARRAY", val);
#ifdef GLavail
  val = GL_VERTEX_SHADER;
#endif
    w("GL_VERTEX_SHADER", val);
#ifdef GLavail
  val = GL_DEPTH_TEST;
#endif
    w("GL_DEPTH_TEST", val);
#ifdef GLavail
  val = GL_LESS;
#endif
    w("GL_LESS", val);

#ifdef GLavail
  val = GL_POINTS;
#endif
    w("GL_POINTS", val);
#ifdef GLavail
  val = GL_LINES;
#endif
    w("GL_LINES", val);
#ifdef GLavail
  val = GL_LINE_STRIP;
#endif
    w("GL_LINE_STRIP", val);
#ifdef GLavail
  val = GL_LINE_LOOP;
#endif
    w("GL_LINE_LOOP", val);
#ifdef GLavail
  val = GL_TRIANGLES;
#endif
    w("GL_TRIANGLES", val);
#ifdef GLavail
  val = GL_TRIANGLE_STRIP;
#endif
    w("GL_TRIANGLE_STRIP", val);
#ifdef GLavail
  val = GL_TRIANGLE_FAN;
#endif
    w("GL_TRIANGLE_FAN", val);

#ifdef GLavail
  val = GL_COLOR_BUFFER_BIT;
#endif
    w("GL_COLOR_BUFFER_BIT", val);
#ifdef GLavail
  val = GL_DEPTH_BUFFER_BIT;
#endif
    w("GL_DEPTH_BUFFER_BIT", val);

#ifdef GLavail
  val = GL_BLEND;
#endif
  w("GL_BLEND", val);
#ifdef GLavail
  val = GL_SRC_ALPHA;
#endif
    w("GL_SRC_ALPHA", val);
#ifdef GLavail
  val = GL_ONE_MINUS_SRC_ALPHA;
#endif
    w("GL_ONE_MINUS_SRC_ALPHA", val);

  printf("}\n");
  return 0;
}

