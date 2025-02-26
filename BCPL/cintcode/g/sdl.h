
/*
######## UNDER DEVELOPMENT ################

This is the BCPL header file for the SDL library interface.

Implemented by Martin Richards (c) Dec 2013

History:

12/03/2018
Modified 3D drawing functions to use floating point depths.

12/12/12
Added drawtriangle(3d) and drawquad(3d)

28/08/12
Started a major modification of the library.

30/05/12
Initial implementation



g_sdlbase is set in libhdr to be the first global used in the sdl library
It can be overridden by re-defining g_sdlbase after GETting libhdr.

A program wishing to use the SDL library should contain the following lines.

GET "libhdr"
MANIFEST { g_sdlbase=nnn  } // Only used if the default setting of 450 in
                            // libhdr is not suitable.
GET "sdl.h"
GET "sdl.b"                 // Insert the library source code
.
GET "libhdr"
MANIFEST { g_sdlbase=nnn  } // Only used if the default setting of 450 in
                            // libhdr is not suitable.
GET "sdl.h"
Rest of the program
*/

GLOBAL {
// More functions will be included in due course
initsdl: g_sdlbase
mkscreen           // (title, xsize, ysize)
setcaption         // (title)
closesdl           // ()

screen             // Handle to the screen surface
format             // Handle to the screen format, used by eg setcolour

lefts              // Used by drawtriangle and drawquad
leftds             // Used by drawtriangle3d and drawquad3d
rights             // Used by drawtriangle and drawquad
rightds            // Used by drawtriangle3d and drawquad3d
depthscreen        // Used by drawtriangle3d and drawquad3d
                   // holding the depth of a drawn pixels
                   // Depths are always floating point values.

miny               // Used by drawtriangle(3d) and drawquad(3d)
maxy               // Used by drawtriangle(3d) and drawquad(3d)

joystick

screenxsize
screenysize
fscreenxsize       // Floating point version of screnxsize
fscreenysize       // Floating point version of screnysize

currcolour         // Current colour for screen
maprgb             // (r, g, b) create colour for current screen format 

resizescreen       // (xsize, ysize)
setcolour          // (colour) sets colour

currx              // The coords of latest point drawn, possibly off screen
curry              // These are always integers.
FLT currz          // The depth coordinate is always a floating point value.

prevdrawn          // = TRUE if the pixel at (currx, curry) has been drawn.
                   // and so does not need to be drawn again.

mousex             // Mouse state set by getmousestate
mousey
mousebuttons

eventtype          // Event type set by getevent()
eventa1
eventa2
eventa3
eventa4
eventa5

mksurface          // (width, height, key)
freesurface        // (surf)
selectsurface      // (surf, xsize, ysize)
currsurf           // Currently selected surface for drawing
currxsize          // its width
currysize          // its height
setcolourkey       // (col)

drawpoint          // (x, y) equivalent to drawfillrect(x,y,1,1)
drawpoint3d        // (x, y, FLT z) set (currx,curry,currz) and
                   // draw the point.
moveto             // (x,y) set (currx,curry) to (x,y)
moveby             // (dx,dy) set (currx,curry) to (currx+dx, curry+dy)
drawto             // (x,y) in colour from (currx,curry) to (x,y)
drawby             // (dx,dy) in colour from (currx, curry) to (currx+dx,curry+dy)

moveto3d           // (x,y, FLT z) set (currx,curry,currz) to (x,y,z)
moveby3d           // (dx,dy, FLT dz) draw the line from (currx,curry,currz)
                   //  to (currx+dx,curry+dy,curry+dz).
drawto3d           // (x,y, FLT z) draw line from (currx,curry,currz)
                   //  to (x,y,z)
drawby3d           // (dx,dy,dz) draw the line from (currx,curry,currz)
                   //  to (currx+dx,curry+dy,currz+dz)

drawquad           // (x1,y1,x2,y2,x3,y3,x4,y4) draw a filled quadraleral
drawtriangle       // (x1,y1,x2,y2,x3,y3) draw a filled triangle
setlims            // used by drawtriangle and drawquad (sets lefts and rights)
drawquad3d         // (x1,y1,FLT z1,x2,y2,FLT z2,x3,y3,FLT z3,x4,y4,FLT z4) draw
                   // a filled 3D quadraleral.
drawtriangle3d     // (x1,y1,FLT z1,x2,y2,FLT z2,x3,y3,FLT z3) draw a filled
                   //  3D triangle.
setlims3d          // used by drawtriangle3d and drawquad3d (sets lefts, rights, leftds, rightds)

drawstring         // (str)
drawcircle         // (ox, oy, r)
drawrect           // (x,y,w,h)
drawellipse        // (ox, oy, rx, ry)
drawfillellipse    // (ox, oy, rx, ry)
drawroundrect      // (x,y,w,h,r)  rect with rounded corners
drawfillroundrect  // (x,y,w,h,r)  rect with rounded corners
drawfillcircle     // (ox, oy, r)
drawfillrect       // (x,y,w,h)

fillsurf           // (surf)
movesurf           // (surf, dx, dy) move entire surface filling vacated pixels with colour
                   // eg movesurf(screen, -1, 0) move the screen left by one pixel

blitsurf           // (src, dsr, x, y)
blitsurfrect       // (src, sx, sy, sw, sh, dsr, dx, dy)

getmousestate      // set (mousex, mousey, buttons)
getevent           // sets event state

sdldelay           // (msecs)  using the SDL delay mechanism
sdlmsecs           // ()       returns msecs since start of run

hidecursor         // ()
showcursor         // ()
updatescreen       // ()       display the current screen

plotf              // (x, y, format, args...)
plotfstr           // Used by plotf

inprod             // (v1, v2) inner product
crossprod          // (v1, v2, v3) v3 becomes the cross product of v1 and v2
standardize        // (v) Force v to be of unit length
}

MANIFEST {
// ops used in calls of the form: sys(Sys_sdl, op,...)
// These should work when using a properly configured BCPL Cintcode system
// running under Linux, Windows or or OSX provided the SDL libraries have been
// installed.
sdl_avail=0
sdl_init           // initialise SDL with everything
sdl_setvideomode   // width, height, bbp, flags
sdl_quit           // Shut down SDL
sdl_locksurface    // surf
sdl_unlocksurface  // surf
sdl_getsurfaceinfo // surf, and a pointer to [flag, format, w, h, pitch, pixels]
sdl_getfmtinfo     // fmt, and a pointer to [palette, bitspp, bytespp,
                   // rloss, rshift, gloss, gshift, bloss, bshift, aloss, ashift,
                   // colorkey, alpha]
sdl_geterror       // str -- fill str with BCPL string for the latest SDL error
sdl_updaterect     // surf, left, top, right, bottom
sdl_loadbmp        // filename of a .bmp image
sdl_blitsurface    // src, srcrect, dest, destrect
sdl_setcolourkey   // surf, flags, colorkey
sdl_freesurface    // surf
sdl_setalpha       // surf, flags, alpha
sdl_imgload        // filename -- using the SDL_image library
sdl_delay          // msecs -- the SDL delay function
sdl_flip           // surf -- Double buffered update of the screen
sdl_displayformat  // surf -- convert surf to display format
sdl_waitevent      // pointer to [type, args, ... ] to hold details of the next event
                   // return 0 if no events available
sdl_pollevent      // pointer to [type, args, ... ] to hold details of the next event
                   // return 0 if no events available
sdl_getmousestate  // pointer to [x, y] returns bit pattern of buttons currently pressed
sdl_loadwav        // file, spec, buff, len
sdl_freewav        // buffer

sdl_wm_setcaption  // string
sdl_videoinfo      // v => [ flags, blit_fill, video_mem, vfmt]
sdl_maprgb         // format, r, g, b
sdl_drawline       //27
sdl_drawhline      //28
sdl_drawvline      //29
sdl_drawcircle     //30
sdl_drawrect       //31
sdl_drawpixel      //32
sdl_drawellipse    //33
sdl_drawfillellipse   //34
sdl_drawround      //35
sdl_drawfillround  //36
sdl_drawfillcircle //37
sdl_drawfillrect   //38

sdl_fillrect       //39
sdl_fillsurf       //40

// Joystick functions
sdl_numjoysticks       // 41 (index)
sdl_joystickopen       // 42 (index) => joy
sdl_joystickclose      // 43 (index)
sdl_joystickname       // 44 (index)
sdl_joysticknumaxes    // 45 (joy)
sdl_joysticknumbuttons // 46 (joy)
sdl_joysticknumballs   // 47 (joy)
sdl_joysticknumhats    // 48 (joy)


sdl_joystickeventstate //49  sdl_enable=1 or sdl_ignore=0
sdl_getticks           //50  () => msecs since initialisation

sdl_showcursor         //51
sdl_hidecursor         //52
sdl_mksurface          //53
sdl_setcolourkey       //54

sdl_joystickgetbutton  //55
sdl_joystickgetaxis    //56
sdl_joystickgetball    //57
sdl_joystickgethat     //58

/*
// OpenGL and OpenGL ES functions typically having names starting gl_
gl_setvideomode=200     //200
gl_avail                //201
gl_ShadeModel           //202
gl_CullFace             //203
gl_FrontFace            //204
gl_Enable               //205
gl_ClearColor           //206
gl_ViewPort             //207
gl_MatrixMode           //208
gl_LoadIdentity         //209
glu_Perspective         //210
gl_Clear                //211
gl_Translate            //212
gl_Begin                //213
gl_End                  //214
gl_Color4v              //215
gl_Vertex3v             //216
gl_SwapBuffers          //217
gl_Rotate               //218


// OpenGL constants typically have names starting GL_
GL_SMOOTH=7425
GL_BACK=1029
GL_CCW=2305
GL_CULL_FACE=2884
GL_MODELVIEW=5888
GL_PROJECTION=5889
GL_COLOR_BUFFER_BIT=#x4000
GL_DEPTH_BUFFER_BIT=#x100
GL_TRIANGLES=4

// more to come ...
*/

// SDL events
sdl_ignore           = 0
sdl_enable           = 1  // eg enable joystick events

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

sdl_init_everything = #xFFFF

sdl_SWSURFACE   = #x00000000 // Surface is in system memory
sdl_HWSURFACE   = #x00000001 // Surface is in video memory

sdl_ANYFORMAT	= #x10000000 // Allow any video depth/pixel-format
sdl_HWPALETTE	= #x20000000 // Surface has exclusive palette
sdl_DOUBLEBUF	= #x40000000 // Set up double-buffered video mode
sdl_FULLSCREEN	= #x80000000 // Surface is a full screen display
sdl_OPENGL      = #x00000002 // Create an OpenGL rendering context
sdl_OPENGLBLIT	= #x0000000A // Create an OpenGL rendering context and use it for blitting
sdl_RESIZABLE	= #x00000010 // This video mode may be resized
sdl_NOFRAME	= #x00000020 // No window caption or edge frame
}
