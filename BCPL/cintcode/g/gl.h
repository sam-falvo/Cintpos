/*
######## UNDER DEVELOPMENT ################

This is the header file for the BCPL graphics interface that should
work with both OpenGL ES and the full version of OpenGL. The intention
is for BCPL programs to work without change under either version of
OpenGL and with any GL API.

This will be compiled with one of the following macro names defined.

  OpenGL       for the full OpenGL library used with SDL
  OpenGLES     for OpenGL ES for the Raspberry Pi

Implemented by Martin Richards (c) Jan 2014

History:

24/01/2021
Being modified to work with APIs EGL, SDL, SDL2, GLUT and GLFw. It will
always use the event callback mechanism to deal with event. The eventloop
is either implemented in gl.b or is already implemented in the GLU or GLFW
libraries.

30/04/2020
Modified to use g/glmanifests.h

07/10/2019
Modified to work with 32 and 64 bit BCPL on 32 and 64 bit machines.

12/01/14
Initial implementation

g_glbase is set in libhdr to be the first global used in the gl library
It can be overridden by re-defining g_glbase after GETting libhdr.

A program wishing to use the SDL library should contain the following lines.

GET "libhdr"
MANIFEST { g_glbase=nnn  }  // Only used if the default setting of 450 in
                            // libhdr is not suitable.
GET "gl.h"
GET "gl.b"                  // Insert the library source code
.
GET "libhdr"
MANIFEST { g_glbase=nnn  }  // Only used if the default setting of 450 in
                            // libhdr is not suitable.
GET "gl.h"
Rest of the program
*/

GET "glmanifests.h"

GLOBAL {
// More functions will be included in due course
// All these functions capitalise the first letter of each
// word except the the g of gl, eg glMkScreen.
  glInit: g_glbase
  glAPIno            // This will hold a number indicating which
                     // GL API is being used.

  glMainEventLoop    // Enter the main event loop
  glEndEventLoop

  returnFromCallback // Return from an event callback function.
                     // This calls stop(-15) if using GLUT or GLFW
		     // otherwise it does nothing.

  glCallBackfnv      // Vector of registed callback functions. The
                     // elements are zero for all event type having
		     // no registered callback functions.
		     // The upb of this vector is currently 50.
  
  screen             // Handle to the screen surface
  format             // Handle to the screen format, used by eg setcolour

  screenxsize
  screenysize
  FLT screenaspect   // The aspect ration of the screen

  getevent           // If not using GLUT or GLFW this polls for
                     // the next event, if any.
  eventtype          // Event type set by getevent() if there is an event.
  eventa1
  eventa2
  eventa3
  eventa4
  eventa5

  
  glMkScreen       // (title, xsize, ysize)
  glSetPerspective // (mat4, fov, aspect, n, f)    Set the perspective matrix
  glRadius2        // (x,y)   Return sqrt(x**2+y**2)
                   // x, y and the result are floats
  glRadius3        // (x,y,z) Return sqrt(x**2+y**2+z**2)
                   // x, y, z and the result are floats

  loadmodel  // (filename, modelv) -- modelv is typically @Vvec
             // The globals vvec, vvecupb, ivec, ivecupb, dvec and
	     // dvecupb are consecutive.

  lex        // These are used by loadmodel
  ch
  lineno
  token
  lexval

  // The following global are used when writing to the pixel plane.

  pxlxsize
  pxlysize
  pxlv        // Vector of 32 bit RGBA values representing the
              // pixel plane as a texture.
  currx
  curry
  currcolour
  colourkey

  leftxv
  rightxv
  miny
  maxy

  moveto
  moveby
  drawto
  drawby
  drawpoint

  drawtriangle
  drawfilltriangle
  drawrect
  drawrndrect
  drawfillrect
  drawfillrndrect
  drawquad
  drawcircle

  drawf
  drawch
  drawwrch
  write_ch_slice
  drawstring
  plotwrch

  drawstr
  drawfstr
  
  plotf      // (x, y, format, args...)  Not available yet
  plotfstr   // Used by plotf

  lastglglobal // Used to check global variable allocation
}

MANIFEST {
// ops used in calls of the form: sys(Sys_gl, op,...)
// They must have the same values as those used in sysc/flfn.c,
// They work when using a properly configured BCPL Cintcode system
// running under Linux, Windows or or OSX provided the OpenGL libraries
// have been installed.

gl_Init=1           // initialise the GL library
gl_SetFltScale=2    // Return the bit pattern that represents floating 1.0
gl_Quit=3           // Shut down GL
gl_GetError=4       // str -- fill str with BCPL string for the latest GL error
gl_MkScreen=5       // width height
gl_SwapBuffers=6

gl_MkProg=7         // ()
gl_CompileVshader=8
gl_CompileFshader=9
gl_GetAttribLocation=10
gl_GetUniformLocation=11
gl_DeleteShader=12
gl_UseProgram=13
gl_LinkProgram=14
gl_Uniform1f=15
gl_Uniform2f=16
gl_Uniform3f=17
gl_Uniform4f=18
gl_LoadModel=19
gl_BindAttribLocation=20
gl_UniformMatrix4fv=21
gl_ClearColour=22
gl_ClearBuffer=23
gl_M4mulM4=24

gl_pollevent=25 // When not using a GL API that has its own event loop,
                // this returns a pointer to [type, args,..] holding
                // details of the next event. It returns 0 if there are
		// no outstanding events.
gl_Enable=26
gl_Disable=27
gl_DepthFunc=28
gl_VertexData=29
gl_DrawElements=30
gl_EnableVertexAttribArray=31
gl_DisableVertexAttribArray=32
gl_GenVertexBuffer=33
gl_GenIndexBuffer=34
gl_VertexAttribPointer=35
gl_M4mulV=36
gl_ScreenSize=37
gl_PrimitiveRestartIndex=38
gl_test=39                     // Added 23/01/2020
gl_Clear=40

gl_DeleteBuffer=60   // Delete one buffer, added 5/11/2020
gl_BlendFunc=61      // Delete one buffer, added 5/11/2020

gl_GenTextures=62    // (n, texIds)     Typically (1, @texId)
gl_DeleteTextures=63 // (n, texIds)     Typically (1, @texId)
gl_BindTexture=64    // (target, id)
gl_TexImage2D=65     // (target, ...)

sdle_active          = 1  // window gaining or losing focus
sdle_keydown         = 2  // => mod ch
sdle_keyup           = 3  // => mod ch
sdle_mousemotion     = 4  // => x y
sdle_mousebuttondown = 5  // => buttonbits
sdle_mousebuttonup   = 6  // => buttonbits
sdle_joyaxismotion   = 7
sdle_joyballmotion   = 8
sdle_joyhatmotion    = 9
sdle_joybuttondown   = 10
sdle_joybuttonup     = 11
sdle_quit            = 12
sdle_syswmevent      = 13
sdle_videoresize     = 14
sdle_userevent       = 15

sdle_arrowup         = 273
sdle_arrowdown       = 274
sdle_arrowright      = 275
sdle_arrowleft       = 276

  s_vs=1    // Used by loadmodel
  s_x
  s_y
  s_z
  s_r
  s_g
  s_b
  s_k
  s_d

  s_is
  s_i
  
  s_ds
  s_t

  s_num        // Floating pont value in lexval
  s_eof

}
