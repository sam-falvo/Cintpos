
/*
######## UNDER DEVELOPMENT ################

This is the BCPL header file for the SDL library interface.

Implemented by Martin Richards (c) Dec 2013

History:

25/04/2021          ### Incompatible change ###
Eventually decided to adopt the OpenGL convention of using the left
hand convention for the screen axes. So positive x and y are screen
right and up. Positive z is into the screen, so larger z values are
further from the viewer and will be be hidden by pixels with less
positive values at the same (x,y) location. The 3D drawing functions
such as drawby3d and drawtiangle3d use floating point numbers for the
screen coordinates. These functions convert them to integer values
before placing them on the screen. Since the z component is only used
for hidden surface removal it is scaled by zfac (currently 100.0) to
improve the accuracy of the intersection of nearly parallel planes.
The integer z values are clamped to lie between -1_000_000_000 and
+1_000_000_000. Note that these values have greater precision than the
correponding floating point numbers.

03/06/2021
Adding a 12x18 font as an alternative to the 8x12 font previously
available. The larger font is better when large window sizes are used.
Use selectfont(12) or selectfont(18) to select the font. It updates
fontW, fontH, drawch and write_ch_slice and sets fonttab to the
appropriate bitmap table.

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
inprod: g_sdlbase             // Typically G:450
crossprod
standardize
distance
radius

screen; screen1    // Two word handle to the screen surface

currsurf;currsurf1 // Two words holding a machine address used by the SDL
                   // library to represent the current surface.

format             // Two word handle to the screen format, used by eg setcolour
format1

joystick; joystick1 // Two BCPL words used to hold the machine address used
                    // by the SDL library to represent a joystick.

screenxsize
screenysize
FLT fscreenxsize   // Floating point version of screnxsize
FLT fscreenysize   // Floating point version of screnysize
FLT fscreencentrex // Floating point version of screnxsize/2
FLT fscreencentrey // Floating point version of screnysize/2

currxsize          // These hold the width and height of the 
currysize          // currently selected surface.

currxupb           // = currxsize-1,  set whenever currxsize changes.
curryupb           // = currysize-1,  set whenever currysize changes.

leftxv             // This vector holds for each relevant y the x value of
                   // the leftmost pixel of a triangle 
leftzv             // This holds the z value for each pixel described by leftxv.
                   // 64 units of an element of leftzv represent a distance of
		   // one pixel.
		   
rightxv            // This vector holds for each relevant y the x value of
                   // the rightmost pixel of a triangle 
rightzv            // This holds the z value for each pixel described by rightxv.
                   // 64 units of an element of rightzv represent a distance of
		   // one pixel.
		   
miny               // This holds the minimum y value of any pixel in a triangle.
maxy               // This holds the maximum y value of any pixel in a triangle.
                   // These are used when drawing 2D and 3D triangles.
		   
depthv             // Used by the 3D drawing functions. zfac units of depth
                   // correspond to a distance of one pixel.
depthvupb          // =currxsize*currysize-1  The upb of the depth matrix

maxdepth           // = +1_000_000_000  integer scaled maximum depth
mindepth           // = -1_000_000_000  integer scaled minimum depth

neardepth          // These are for depth culling of 3D lines and triangles.
fardepth           // They use integer scaled screen units.
                   // These variables are only set using setdepthlimits.

currcolour         // This holds the colour of the next pixel to be drawn

currx              // These hold the integer coordinates of the next 2D pixel
curry              // to be drawn. One unit in each of these corresponds
                   // to a distance of one pixel. These are only used by
		   // the 2D drawing functions moveto, moveby, drawto,
		   // drawby and drawch. They allow convenient drawing
		   // of 2D line sequences and characters.

currx3d            // These hold the integer coordinates of the next 3D
curry3d            // pixel to be drawn. 1.0 in each corresponds to a
                   // distance of one pixel.
currsz3d           // This holds the scaled z component of the next 3D
                   // pixel. These are only used by the 3D drawing
		   // functions moveto3d, moveby3d, drawto3d and drawby3d.
		   // They allow convenient drawing of 3D line sequences.

mousex             // Mouse state set by getmousestate
mousey

mousebuttons       // A bit pattern indicating which joystick buttons are currently
                   // being pressed.

eventtype          // Event type set by getevent()
eventa1            // Arguments of the latest event.
eventa2
eventa3
eventa4
eventa5

// More functions will be included in due course
initsdl            // () Initialise the SDL library interface
mkscreen           // (title, xsize, ysize)
mkscreen3d         // (title, xsize, ysize) for 3D drawing, allocates depthv.
maprgb             // (r,g,b) create colour for current screen format 
setcaption         // (title)
closesdl           // ()

setcolour          // (colour) sets the current colour
setcolourkey       // (col)  When updatescreen is called only pixels
                   //        with colour different from col are copied to
		   //        the frame buffer.
mksurface          // (width, height, key, surfptr)
freesurface        // (surfptr)
selectsurface      // (surfptr, xsize, ysize)

// The following functions are for 2D drawing.
moveto             // (x,y) set (currx,curry) to (x,y)
moveby             // (dx,dy) set (currx,curry) to (currx+dx, curry+dy)
drawto             // (x,y)    Draw a line from (currx,curry) to (x,y) using
                   //          currcolour. Leave (currx,curry) set to (x,y).
drawby             // (dx,dy)  Draw a line from (currx, curry)
                   //          to (currx+dx,curry+dy) using currcolour.
		   //          Leave (currx, curry) set to (currx+dx,curry+dy).
drawch             // (ch)     Draw character ch at position (currx,curry) as
                   //          a 9x11 image advancing the position appropriately.

fontW              // Typically  8 or 12
fontH              // Typically 12 or 18
charHsep           // Character horizontal separation in pixels, typically 2 or 3 
charVsep           // Character verticalal separation in pixels, typically 3 or 4 
charLmargin        // Typixally 10 fixel

write_ch_slice     // Used by drawch
fonttab            // The font table used by write_ch_slice

resizescreen       // (xsize,ysize)
selectfont         // Sets fontW, fontH, drawch, write_ch_slice and fonttab

fillsurf           // (surfptr)       Fill the surface with currcolour.
                   //                 surfptr points to the word pair holding
		   //                 the machine address of representing the
		   //                 surface.
movesurf           // (surfptr,dx,dy) Scroll entire surface to position (x,y) of
                   //                 the screen filling vacated pixels with
		   //                 currcolour
                   //                 eg movesurf(screenptr, -1, 0) moves the
		   //                 screen left by one pixel

blitsurf           // (srcptr,dsrptr,x,y)
blitsurfrect       // (srcptr,sx,sy,sw,sh,dsr,dx,dy)

interpolate        // (p, p1,p2, q1,q2) Return the interpolated value q

drawpoint          // (x,y)      Draw a pixel at position (x,y).
drawstr            // (x,y,str)  Draw a string at position (x,y) using drawch.
drawtriangle       // (x1,y1, x2,y2, x3,y3)
                   //            Draw a filled triangle using currcolour.
drawquad           // (x1,y1, x2,y2, x3,y3, x4,y4)
                   //            The draw a filled quadraleral by calling
                   //            drawtriangle(x1,y1, x2,y2, x3,y3) and
		   //            drawtriangle(x2,y2, x3,y3, x4,y4).
setlims            // (x0,y0, x1,y1) This is used by drawtriangle to set leftxv
                   //                rightxv, miny and maxy.

drawcircle         // (x,y,radius) Draw a circle with centre (x,y) and
                   //              given radius.
drawrect           // (x,y,w,h)
drawrndrect        // (x,y,w,h,radius)  rect with rounded corners
drawellipse        // (x,y,w,h)

drawfillcircle     // (x,y,radius)
drawfillrect       // (x,y,w,h)
drawfillrndrect    // (x,y,w,h,radius)  rect with rounded corners
drawfillellipse    // (x,y,w,h)


// In the following 3D drawing functions, the coordinates are integers
// with one unit corresponding to a distance of one pixel.
// The following four functions update currx3d, curry3d and currsz3d
// making the drawing of consective 3D lines convenient.

moveto3d           // (x, y, sz)
                   //    Set (currx3d,curry3d,currsz3d) to
                   //    (x, y, sz).
moveby3d           // (dx, dy, dz)
                   //    Call moveto3d(currx+dx, curry+dy, currsx+dz*zfac).
drawto3d           // (x, y, sz)
                   //    Draw a 3D line from (currx,curry,currsz) to (x,y,sz)
		   //    and set (currx,curry,currsz) to (x, y, sz)
drawby3d           // (dx, dy, dsz)
                   //    Call drawto3d(currx3d + dx,
		   //                  curry3d + dy,
		   //                  currsz3d + dz).


drawpoint3d        // (x,y,sz)
                   //    Draw a 3D pixel at integer position (x,y,sz) where
		   //    sz is the scaled depth.

drawtriangle3d     // (x1, y1, z1,
                   //  x2, y2, z2,
		   //  x3, y3, z3)
                   //     Draw a filled filled 3D triangle using  currcolour.

drawquad3d         // (x1, y1, z1,
                   //  x2, y2, z2,
		   //  x3, y3, z3,
		   //  x4, y4, z4)
                   //     Draw a filled 3D quadraleral using two two calls
		   //     of drawtriangle3D.
		   
setlims3d          // (x0,y0,sz0, x1,y1,sz1)
                   //     This is used by drawtriangle3d to update leftxv,
                   //     rightxv, leftdzv, rightdzv, miny and maxy.

setdepthlimits     // (near, far) This functions sets neardepth
                   // and fardepth in integer scaled screen units. It is
		   // the only way to set neardepth and fardepth.



getmousestate      // set (mousex,mousey,buttons)
getevent           // sets event state

sdldelay           // (msecs)  using the SDL delay mechanism
sdlmsecs           // ()       returns msecs since start of run

hidecursor         // ()
showcursor         // ()
updatescreen       // ()       Send the current screen to the framebuffer.

drawf              // (x, y, format, args...) Output characters to the screen
                   //                         using writef.
drawfstr           // A character vector Used by drawf.  Possibly G:551

lastsdlglobal      // Used to check global variable allocation
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
sdl_locksurface    // surfptr
sdl_unlocksurface  // surfptr
sdl_getsurfaceinfo // surfptr, and a pointer to [flag, format, w, h, pitch, pixels]
sdl_getfmtinfo     // fmtptr, and a pointer to [palette, bitspp, bytespp,
                   // rloss, rshift, gloss, gshift, bloss, bshift, aloss, ashift,
                   // colorkey, alpha]
sdl_geterror       // str -- fill str with BCPL string for the latest SDL error
sdl_updaterect     // surfptr, left, top, right, bottom
sdl_loadbmp        // filename of a .bmp image
sdl_blitsurface    // src, srcrect, dest, destrect
sdl_setcolourkey   // surfptr, flags, colorkey
sdl_freesurface    // surfptr
sdl_setalpha       // surfptr, flags, alpha
sdl_imgload        // filename -- using the SDL_image library
sdl_delay          // msecs -- the SDL delay function
sdl_flip           // surfptr -- Double buffered update of the screen
sdl_displayformat  // surfptr -- convert surf to display format
sdl_waitevent      // pointer to [type, args, ... ] to hold details of the next event
                   // return 0 if no events available
sdl_pollevent      // pointer to [type, args, ... ] to hold details of the next event
                   // return 0 if no events available
sdl_getmousestate  // pointer to [x,y] returns bit pattern of buttons currently pressed
sdl_loadwav        // file, spec, buff, len
sdl_freewav        // buffer

sdl_wm_setcaption  // string
sdl_videoinfo      // v => [ flags,blit_fill,video_mem,vfmt]
sdl_maprgb         // (formatptr,r,g,b)
sdl_drawline       //27
sdl_drawhline      //28
sdl_drawvline      //29
sdl_drawcircle     //30
sdl_drawrect       //31
sdl_drawpixel      //32
sdl_drawellipse    //33
sdl_drawfillellipse //34
sdl_drawround      //35
sdl_drawfillround  //36
sdl_drawfillcircle //37
sdl_drawfillrect   //38

sdl_fillrect       //39
sdl_fillsurf       //40

// Joystick functions
sdl_numjoysticks       // 41 (index)
sdl_joystickopen       // 42 (index, jpyptr)
sdl_joystickclose      // 43 (index)
sdl_joystickname       // 44 (index)
sdl_joysticknumaxes    // 45 (joyptr)
sdl_joysticknumbuttons // 46 (joyptr)
sdl_joysticknumballs   // 47 (joyptr)
sdl_joysticknumhats    // 48 (joyptr)

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
sdl_OPENGLBLIT	= #x0000000A // Create an OpenGL context for blitting
sdl_RESIZABLE	= #x00000010 // This video mode may be resized
sdl_NOFRAME	= #x00000020 // No window caption or edge frame
}
