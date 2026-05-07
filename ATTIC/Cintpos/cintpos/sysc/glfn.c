/*
This contains the implemetation of the sys(Sys_sd, fno, ...) facility.

###### Still under development ############

Implemented by Martin Richards (c) Feb 2014

This file is planned to provide and interface to either OpenGL using SDL or
OpenGL ES using EGL (typically for the Raspberry Pi).  To hide the differences
between these two versions of OpenGL, BCPL programs should use the g/gl.b
library with the g/gl.h header file.

SDLavail is defined only if the SDL libraries are available
GLavail is defined if OpenGL or OpenGL ES libraries are available.
EGLavail is defined if the EGL libraries are available

Whichever version of OpenGL is used the BCPL interface using

res := sys(Sys_gl, fno, a1, a2, a3, a4,...)

is the same.

Note that this calls glfn(args, g)
where args[0] = fno, args[1]=a1,... etc
and   g points to the base of the global vector.

fno=0  Test that a version of OpenGL is available
       res is TRUE if it is.

fno=1 ...
*/

#include "cintpos.h"
#include <stdio.h>
#include <stdlib.h>


#ifndef GLavail
BCPLWORD glfn(BCPLWORD *args, BCPLWORD *g, BCPLWORD *W) {
  printf("glfn: GLavail was not defined\n");
    return 0;   // GL is not available
}
#endif

#ifdef GLavail

#ifndef EGLavail
// Include SDL headers if not using EGL
#ifdef forWIN32
#include <SDL.h>
#else
#include <SDL/SDL.h>
#endif
#endif

// OpenGL or OpenGL ES headers
#ifdef forRaspiGL
#include "bcm_host.h"
#include "GLES/gl.h"
#else
#include <GL/gl.h>
//#include <GL/glu.h>
//#include <GL/glut.h>
#endif
#endif

#ifdef EGLavail
// EGL headers
#ifdef forRaspiGL
#include "EGL/egl.h"
#include "EGL/eglext.h"
#else
#include <EGL/egl.h>
#endif
#endif

#ifdef GLavail
// These must agree with the declarations in g/gl.h
#define gl_Init                1
#define gl_SetFltScale         2
#define gl_Quit                3
#define gl_GetError            4
#define gl_MkScreen            5
#define gl_SwapBuffers         6
#define gl_MkProg              7
#define gl_CompileVshader      8
#define gl_CompileFshader      9
#define gl_GetAttribLocation  10
#define gl_GetUniformLocation 11
#define gl_DeleteShader       12
#define gl_UseProgram         13
#define gl_LinkProgram        14
#define gl_BindAttribLocation 20
#define gl_UniformMatrix4fv   21
#define gl_ClearColour        22
#define gl_ClearBuffer        23
#define gl_M4mulM4            24
#define gl_pollevent          25
#define gl_Enable             26
#define gl_Disable            27
#define gl_DepthFunc          28
#define gl_VertexData         29
#define gl_DrawTriangles      30
#define gl_EnableVertexAttribArray 31
#define gl_DisableVertexAttribArray 32
#define gl_GenVertexBuffer    33
#define gl_GenIndexBuffer     34
#define gl_VertexAttribPointer 35

//float fltscale;
//float invfltscale;

const SDL_VideoInfo* info = NULL;
int width = 700;
int height =200;
int bpp = 0;
int flags=0; // Flag to pass to SDL_SetVideoMode

GLuint glProgram;


BCPLWORD decodeevent(SDL_Event*e, BCPLWORD *ptr) {
  if(e) {
    ptr[0] = (BCPLWORD)(e->type);
    switch (e->type) {
    default:
      printf("glfn: Unknown event type %d\n", e->type);
      return -1;

    case SDL_ACTIVEEVENT:      // 1
      ptr[1] = (BCPLWORD)(e->active).gain;  // 0 if loss, 1 if gain
      ptr[2] = (BCPLWORD)(e->active).state; // 0=mouse focus, 1=keyboard focus,
                                            // 2=minimised
      return -1;

    case SDL_KEYDOWN:          // 2
    case SDL_KEYUP:            // 3
      //printf("getevent: KEYDOWN or UP\n");
    { SDL_keysym *ks = &(e->key).keysym;
      BCPLWORD sym = ks->sym;
      BCPLWORD mod = ks->mod;
      BCPLWORD ch = (BCPLWORD)(ks->unicode);
      if(ch==0) ch = sym;
      ptr[1] = mod;
      ptr[2] = ch;
      return -1;
    }

    case SDL_MOUSEMOTION:      // 4
      ptr[1] = (BCPLWORD)(e->motion).state;
      ptr[2] = (BCPLWORD)(e->motion).x;
      ptr[3] = (BCPLWORD)(e->motion).y;
      //printf("getevent: MOUSEMOTION %4d %4d %4d\n", ptr[1], ptr[2], ptr[3]);
      return -1;

    case SDL_MOUSEBUTTONDOWN:  // 5
    case SDL_MOUSEBUTTONUP:    // 6
      ptr[1] = (BCPLWORD)(e->button).state;
      ptr[2] = (BCPLWORD)(e->button).x;
      ptr[3] = (BCPLWORD)(e->button).y;
      //printf("getevent: MOUSEBUTTONDOWN/UP %4d %4d %4d\n", ptr[1], ptr[2], ptr[3]);
      return -1;

    case SDL_JOYAXISMOTION:    // 7
      ptr[1] = (BCPLWORD)(e->jaxis).which;  // Which joystick
      ptr[2] = (BCPLWORD)(e->jaxis).axis;   // Which axis
                                            // 0 = aileron
                                            // 1 = elevator
                                            // 2 = throttle
      ptr[3] = (BCPLWORD)(e->jaxis).value;  // What value  -32768 to + 32767
      return -1;

    case SDL_JOYBALLMOTION:    // 8
      ptr[1] = (BCPLWORD)(e->jball).which;  // Which joystick
      ptr[2] = (BCPLWORD)(e->jball).ball;   // Which ball
      ptr[3] = (BCPLWORD)(e->jball).xrel;   // X relative motion
      ptr[4] = (BCPLWORD)(e->jball).yrel;   // Y relative motion
      return -1;

    case SDL_JOYHATMOTION:     // 9
      ptr[1] = (BCPLWORD)(e->jhat).which;  // Which joystick
      ptr[2] = (BCPLWORD)(e->jhat).hat;    // Which hat
      ptr[3] = (BCPLWORD)(e->jhat).value;  // Hat position
      return -1;

    case SDL_JOYBUTTONDOWN:    // 10
    case SDL_JOYBUTTONUP:      // 11
      ptr[1] = (BCPLWORD)(e->jbutton).which;  // Which joystick
      ptr[2] = (BCPLWORD)(e->jbutton).button; // Which button
      ptr[3] = (BCPLWORD)(e->jbutton).state;  // What state
      return -1;

    case SDL_QUIT:             // 12
      return -1;

    case SDL_SYSWMEVENT:       // 13
      return -1;

    case SDL_VIDEORESIZE:      // 16
      ptr[1] = (BCPLWORD)(e->resize).w;  // New window width
      ptr[2] = (BCPLWORD)(e->resize).h;  // New window height
      printf("VIDEORESIZE=%d\n", SDL_VIDEORESIZE);
      return -1;

    case SDL_VIDEOEXPOSE:      // 17
      // Screen needs to be redrawn
      printf("VIDEOEXPOSE=%d\n", SDL_VIDEOEXPOSE);
      return -1;

    case SDL_USEREVENT:        // 24
      return -1;
    }
  }
  *ptr = 0;
  return 0;
}


BCPLWORD glfn(BCPLWORD *a, BCPLWORD *g, BCPLWORD *W) {
  char tmpstr[256];
  int argc = 0;

  //printf("glfn: GLavail was defined\n");

  //printf("glfn: fno=%d a1=%d a2=%d a3=%d a4=%d\n",
  //        a[0], a[1], a[2], a[3], a[4]);

  switch(a[0]) {
  default:
    printf("glfn: Unknown op: fno=%d a1=%d a2=%d a3=%d a4=%d\n",
            a[0], a[1], a[2], a[3], a[4]);
    return 0;

  case gl_Init:  // Initialise all SDL features
    { int argc = 0;
      //BCPLWORD res = (BCPLWORD) SDL_Init(SDL_INIT_EVERYTHING);
      BCPLWORD res = (BCPLWORD) SDL_Init(SDL_INIT_VIDEO);
      if (res<0) {
        fprintf(stderr, "Video initialization failed: %s\n", "error");
		//	SDL_GetError());
        SDL_Quit();
      }

      //printf("glfn: SDL_init returned ok\n");

      info = SDL_GetVideoInfo();
  
      if( !info ) {
        fprintf(stderr, "Video query failed: %s\n",
                SDL_GetError());
        SDL_Quit();
        exit(0);
      }

      bpp = info->vfmt->BitsPerPixel;
      //printf("bpp=%d\n", bpp);

      SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 5);
      SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 5);
      SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 5);
      SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
      SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);

      return res;
    }

    // The following will probably never be used.
    //case gl_SetFltScale:  // Set the floating point scale factors
    //  fltscale = 1000.0;
    //  invfltscale = 1.0 / fltscale;
    //  return 0;

    case gl_Quit:      // Shut down SDL
      SDL_Quit();
      return -1;

      ///*
    case gl_GetError:   // str -- fill str with BCPL string for the latest SDL error
    { char *str = SDL_GetError();
      printf("sdl_GetError: %s\n", str);
      return c2b_str(str, a[1]); // Convert to BCPL string format
    }
    //*/

    case gl_MkScreen: // (title, width, height)
    { char tmpstr[256];
      int i;
      char *title = (char *)(a[1]);

      width  = a[2];
      height = a[3];

      float ratio = (float)width / (float)height;

      flags = SDL_OPENGL;

      if( SDL_SetVideoMode(width, height, bpp, flags)==0){
        fprintf(stderr, "Video mode set failed: %s\n",
                SDL_GetError());
        SDL_Quit();
        exit(0);
      }

      b2c_str(a[1], tmpstr);

      //printf("gl_MkScreen: title=%s width=%d height=%d\n",
      //        tmpstr, a[2], a[3]);
      SDL_WM_SetCaption(tmpstr, 0);

      // Enable Unicode translation of keyboard events.
      SDL_EnableUNICODE(1);
      SDL_JoystickEventState(SDL_ENABLE);

      // Initialise the floating point scale factors
      //fltscale = 1000.0;
      //invfltscale = 1.0 / fltscale;
      //printf("gl_MkScreen: returning -1\n");
      return -1;
    }

    case gl_MkProg: // ()
    { GLuint prog =  glCreateProgram();
      //printf("glfn: glCreateProgram => %d\n", prog);
      return (BCPLWORD) prog;
    }

    case gl_CompileVshader: // (prog, cstr)
    { // Compiler the vertex shader whose source is in the
      // C string cstr, and attach it to the given GL program.
      GLuint prog = (GLuint) a[1];
      const char* cstr = (char *) (&W[a[2]]);

      GLuint Vshader = glCreateShader(GL_VERTEX_SHADER);

      glShaderSource(Vshader, 1, &cstr, NULL);
      glCompileShader(Vshader);

      GLint nCompileResult = 0;

      glGetShaderiv(Vshader, GL_COMPILE_STATUS, &nCompileResult);

      if(!nCompileResult)
      { int i;
        char Log[1024];
        GLint nLength;
        glGetShaderInfoLog(Vshader, 1024, &nLength, Log);
        for(i=0; i<nLength; i++) printf("%c", Log[i]);
        printf("\n");
      }      

      glAttachShader(prog, Vshader);
      return Vshader;
    }

    case gl_CompileFshader: // (prog, cstr)
    { // Compiler the fragment shader whose source is in the
      // C string cstr, and attach it to the given GL program.
      GLuint prog = (GLuint) a[1];
      const char* cstr = (char *) (&W[a[2]]);
      GLuint Fshader = glCreateShader(GL_FRAGMENT_SHADER);

      glShaderSource(Fshader, 1, &cstr, NULL);
      glCompileShader(Fshader);

      GLint nCompileResult = 0;

      glGetShaderiv(Fshader, GL_COMPILE_STATUS, &nCompileResult);

      if(!nCompileResult)
      { int i;
        char Log[1024];
        GLint nLength=20;
        glGetShaderInfoLog(Fshader, 1024, &nLength, Log);
        for(i=0; i<nLength; i++) printf("%c", Log[i]);
        printf("\n");
      }      

      glAttachShader(prog, Fshader);
      return Fshader;
    }

    case gl_LinkProgram: // (prog)
    { GLuint prog = (GLuint)a[1];
      glLinkProgram(prog);

      GLint nLinkResult = 0;

      glGetProgramiv(prog, GL_LINK_STATUS, &nLinkResult);

      if(!nLinkResult)
      { int i;
        char Log[1024];
        GLint nLength;
        glGetProgramInfoLog(prog, 1024, &nLength, Log);
        for(i=0; i<nLength; i++) printf("%c", Log[i]);
        printf("\n");
      }
      //printf("glfn: gl_LinkProgram returning -1\n");
      return -1;
    }

    case gl_BindAttribLocation: // (prog, loc, name)
    { // Specify the location of an attribute before linking
      GLuint prog = (GLuint) a[1];
      GLuint loc = (GLuint) a[2];
      b2c_str(a[3], tmpstr);
      printf("glfn: BindAttribLocation prog=%d loc=%d name=%s\n", prog, loc, tmpstr);
      return (BCPLWORD) glBindAttribLocation(prog, loc, tmpstr);
    }

    case gl_GetAttribLocation: // (prog, name)
    { // Find out where the linker put an attribute variable
      GLuint prog = (GLuint) a[1];
      b2c_str(a[2], tmpstr);
      //printf("glfn: GetAttribLocation prog=%d name=%s\n", prog, tmpstr);
      return (BCPLWORD) glGetAttribLocation(prog, tmpstr);
    }

    case gl_GetUniformLocation: // (prog, name)
    { // Find out where the linker put a uniform variable
      GLuint prog = (GLuint) a[1];
      b2c_str(a[2], tmpstr);
      //printf("glfn: GetAttribLocation prog=%d name=%s\n", prog, tmpstr);
      return (BCPLWORD) glGetUniformLocation(prog, tmpstr);
    }

    case gl_UniformMatrix4fv: // (loc, prog, matrix) -- 4x4 matrix
    { 
      GLuint loc = (GLuint) a[1];
      GLuint prog = (GLuint) a[2];
      float *matrix = (float *) (&W[a[3]]);
      int i;
      //for(i=0; i<16; i++) printf("%9.3f\n", matrix[i]);
      return (BCPLWORD) glUniformMatrix4fv(loc, prog, GL_FALSE, matrix);
    }

    case gl_DeleteShader: // (shader)
    { GLuint shader = (GLuint) a[1];
      glDeleteShader(shader);
      return -1;
    }

    case gl_UseProgram: // (prog)
    { GLuint prog = (GLuint)a[1];
      glUseProgram(prog);
      return -1;
    }

    case gl_Enable: // (op)
    { GLint op = (GLint)a[1];
      glEnable(op);
      return -1;
    }

    case gl_Disable: // (op)
    { GLint op = (GLint)a[1];
      glDisable(op);
      return -1;
    }

    case gl_DepthFunc: // (relation)
    { GLint relation = (GLint)a[1];
      glDepthFunc(relation);
      return -1;
    }

    case gl_VertexData: // (loc, n, stride, datav))
    { // datav<32 the a vertex object is being used and datav is an offset
      // The are n vertex items each containing stride floating point numbers
      GLint loc = (GLint)a[1];
      GLint n = (GLint)a[2];
      GLint stride = (GLint)(a[3]*4);
      GLfloat *datav = (GLfloat *)((0<=a[4] && a[4]<32) ? (const void *)(a[4]*4) : &W[a[4]]);
      int i;
      //printf("glfn: calling glVertexAttribPointer loc=%d n=%d stride=%d a[4]=%d\n",
      //     loc, n, stride, a[4]);
      //printf("glfn: calling       glVertexAttribPointer loc=%d n=%d stride=%d a[4]=%d\n",
      //     loc, n, a[3], a[4]);

      glVertexAttribPointer(loc,
                            n, GL_FLOAT,   // n elements of type float
                            GL_FALSE,      // Do not normalise
                            stride,        // Stride
                            datav);
      glEnableVertexAttribArray(loc);
      //printf("glfn: gl_VertexData loc=%d n=%d stride=%d\n", loc, n, stride);
      //for(i = 0; i<3; i++) printf("%3d: %5.3f\n", i, datav[i]);
      //printf("glfn: returned from glVertexAttribPointer loc=%d n=%d stride=%d a[4]=%d\n",
      //     loc, n, a[3], a[4]);
      return -1;
    }

    case gl_DrawTriangles: // (n, indexv)
      // n = number of index values ( ie 3*n/3 triangles)
      // indexv is a vector of 16-bit integers.
      // If indexv=0 objects are being used
    { GLint n = (GLint)(a[1]); // Number of index values
      GLushort *datav = (GLushort *)(a[2] ? &W[a[2]]: 0);

      //printf("glfn: gl_DrawTriangles n=%d a[2]=%d\n", n, a[2]);
      //int i;
      //for(i=0; i<24; i++)
      //  printf("glfn: DrawTriangles i=%2d  datav[i]=%d\n", i, datav[i]);
      glDrawElements(GL_TRIANGLES,
                     n,                 // Number of vertices
                     GL_UNSIGNED_SHORT, // Type of index elements
                     datav);            // Index data
      //printf("glfn: returned from gl_DrawTriangles n=%d\n", n);
      return -1;
    }

    case gl_EnableVertexAttribArray: // (attrib)
    { GLint attrib = (GLint)(a[1]);
      //printf("glfn: EnableVertexAttribArray(%d)\n", attrib);
      glEnableVertexAttribArray(attrib);
      return -1;
    }

    case gl_DisableVertexAttribArray: // (attrib)
    { GLint attrib = (GLint)(a[1]);
      printf("glfn: DisableVertexAttribArray(%d)\n", attrib);
      glDisableVertexAttribArray(attrib);
      return -1;
    }

    case gl_GenVertexBuffer: // (size, data)
    { GLint size = (GLint)a[1]; // Number of floats
      GLfloat *data = (GLfloat *)&W[a[2]];
      GLuint buffer;
      glGenBuffers(1, &buffer);
      glBindBuffer(GL_ARRAY_BUFFER, buffer);
      glBufferData(GL_ARRAY_BUFFER,        // Copy vertex data to graphics memory
                   size * sizeof(GLfloat), // The size of data in bytes
                   data,                   // The vertex data
                   GL_STATIC_DRAW);        // Usage hint
      return (BCPLWORD)buffer;
    }

    case gl_GenIndexBuffer: // (data, size)
    { GLushort *data = (GLushort *)&W[a[1]];
      GLint size = (GLint)a[2]; // Number of 16-bit indices
      GLuint buffer;
      //int i;
      //for(i=0; i<size; i++) printf("glfn: i=%2d index=%3d\n", i, data[i]);
      glGenBuffers(1, &buffer);
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buffer);
      glBufferData(GL_ELEMENT_ARRAY_BUFFER, // Copy index data to graphics memory
                   size * sizeof(GLushort), // The size index data in bytes
                   data,                    // The vertex data
                   GL_STATIC_DRAW);         // Usage hint
      return (BCPLWORD)buffer;
    }

    case gl_ClearColour: // (r, g, b, a)
      glClearColor(a[1]/255.0f, a[2]/255.0f, a[3]/255.0f, a[4]/255.0f);
      return -1;

    case gl_ClearBuffer: // ()
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      //glClear(GL_COLOR_BUFFER_BIT);
      return -1;

    case gl_SwapBuffers: // ()
      SDL_GL_SwapBuffers();
      return -1;

  case gl_pollevent:    // (pointer) to [type, args, ... ] to hold details of
			// the next event
    { SDL_Event test_event;
      if (SDL_PollEvent(&test_event))
      { decodeevent(&test_event, &W[a[1]]);
        return -1;
      }
      decodeevent(0, &W[a[1]]);
      return 0;
    }

    case gl_M4mulM4: // (A, B, C) performs C := A * B
    { float *A = (float *)(&W[a[1]]);
      float *B = (float *)(&W[a[2]]);
      float *C = (float *)(&W[a[3]]);
      float a00=A[ 0], a10=A[ 1], a20=A[ 2], a30=A[ 3];
      float a01=A[ 4], a11=A[ 5], a21=A[ 6], a31=A[ 7];
      float a02=A[ 8], a12=A[ 9], a22=A[10], a32=A[11];
      float a03=A[12], a13=A[13], a23=A[14], a33=A[15];

      float b00=B[ 0], b10=B[ 1], b20=B[ 2], b30=B[ 3];
      float b01=B[ 4], b11=B[ 5], b21=B[ 6], b31=B[ 7];
      float b02=B[ 8], b12=B[ 9], b22=B[10], b32=B[11];
      float b03=B[12], b13=B[13], b23=B[14], b33=B[15];

      //printf("gl_M4mulM4: entered %d %d %d\n", a[1], a[2], a[3]);

      //printf("%8.3f %8.3f %8.3f %8.3f \n",   a00, a01, a02, a03);
      //printf("%8.3f %8.3f %8.3f %8.3f \n",   a10, a11, a12, a13);
      //printf("%8.3f %8.3f %8.3f %8.3f \n",   a20, a21, a22, a23);
      //printf("%8.3f %8.3f %8.3f %8.3f \n\n", a30, a31, a32, a33);

      //printf("%8.3f %8.3f %8.3f %8.3f \n",   b00, b01, b02, b03);
      //printf("%8.3f %8.3f %8.3f %8.3f \n",   b10, b11, b12, b13);
      //printf("%8.3f %8.3f %8.3f %8.3f \n",   b20, b21, b22, b23);
      //printf("%8.3f %8.3f %8.3f %8.3f \n\n", b30, b31, b32, b33);

      C[ 0] = a00*b00 + a01*b10 + a02*b20 + a03*b30; // c00
      C[ 1] = a10*b00 + a11*b10 + a12*b20 + a13*b30; // c10
      C[ 2] = a20*b00 + a21*b10 + a22*b20 + a23*b30; // c20
      C[ 3] = a30*b00 + a31*b10 + a32*b20 + a33*b30; // c30

      C[ 4] = a00*b01 + a01*b11 + a02*b21 + a03*b31; // c01
      C[ 5] = a10*b01 + a11*b11 + a12*b21 + a13*b31; // c11
      C[ 6] = a20*b01 + a21*b11 + a22*b21 + a23*b31; // c21
      C[ 7] = a30*b01 + a31*b11 + a32*b21 + a33*b31; // c31

      C[ 8] = a00*b02 + a01*b12 + a02*b22 + a03*b32; // c02
      C[ 9] = a10*b02 + a11*b12 + a12*b22 + a13*b32; // c12
      C[10] = a20*b02 + a21*b12 + a22*b22 + a23*b32; // c22
      C[11] = a30*b02 + a31*b12 + a32*b22 + a33*b32; // c32

      C[12] = a00*b03 + a01*b13 + a02*b23 + a03*b33; // c03
      C[13] = a10*b03 + a11*b13 + a12*b23 + a13*b33; // c13
      C[14] = a20*b03 + a21*b13 + a22*b23 + a23*b33; // c23
      C[15] = a30*b03 + a31*b13 + a32*b23 + a33*b33; // c33

      //printf("%8.3f %8.3f %8.3f %8.3f \n",   C[0], C[4], C[ 8], C[12]);
      //printf("%8.3f %8.3f %8.3f %8.3f \n",   C[1], C[5], C[ 9], C[13]);
      //printf("%8.3f %8.3f %8.3f %8.3f \n",   C[2], C[6], C[10], C[14]);
      //printf("%8.3f %8.3f %8.3f %8.3f \n\n", C[3], C[7], C[11], C[15]);
    }
  }
}
#endif
