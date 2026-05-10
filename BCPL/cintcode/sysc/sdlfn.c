/*
This contains the implemetation of the sys(Sys_sdl, fno, ...) facility.

Implemented by Martin Richards (c) June 2013

09/09/2019

Made systematic changes to allow this library to work on both 32 and 64 bit
machine using both 32 and 64 bit BCPL.  Unfortunately this involves making
changes to all BCPL programs interfacing with SDL. BCPL program must allocate
always allocate two condecutive BCPL words to hold machine addresses although
often only the first word will be used. All SDL functions needing or returning
a machine address must now use pointers to the word pair. Functions returning
a machine address will have to pass a pointer to the pair as an extra argument.
So, for instance, the call mkwindow(name, xsize, ysize) will have to be
mkwindow(name, xsize, ysize, ptr).

24/07/2014
Began to add SDL2 

24/09/2012
Added joystick events


Specification of res := sys(Sys_sdl, fno, a1, a2, a3, a4,...)

Note that this calls sdlfn(args, g)
where args[0] = fno, args[1]=a1,... etc
and   g points to the base of the global vector.

fno=0  Test the sdl is available
       res is TRUE if the sdl features are implemented.

fno=1 ...
Withine sdlfn.c typicale fno numbers are
sdl_avial, sdl_init, etc
Within g/sdl.h the same names are declared as manifest constants.
*/

#include "cintmain.h"

// cintmain.h must be included early since it defines such constants as
// BCPLWORD, SDLavail and GLavail.

#ifndef SDLavail
BCPLWORD sdlfn(BCPLWORD *args, BCPLWORD *g, BCPLWORD *W) {
    return 0;   // SDL is not available
}
#endif


#ifdef SDLavail
// SDL is available

//#include <stdio.h>
//#include <stdlib.h>
//#include "sdldraw.h"

//#ifdef forWIN32
//#include <SDL.h>
//#else
//#include <SDL/SDL.h>
//#endif

// These must agree with the declarations in g/sdl.h
#define sdl_avail           0
#define sdl_init            1
#define sdl_setvideomode    2
#define sdl_quit            3
#define sdl_locksurface     4
#define sdl_unlocksurface   5
#define sdl_getsurfaceinfo  6
#define sdl_getfmtinfo      7
#define sdl_geterror        8
#define sdl_updaterect      9
#define sdl_loadbmp        10
#define sdl_blitsurface    11
#define sdl_setcolorkey    12
#define sdl_freesurface    13
#define sdl_setalpha       14
#define sdl_imgload        15
#define sdl_delay          16
#define sdl_flip           17
#define sdl_displayformat  18
#define sdl_waitevent      19
#define sdl_pollevent      20
#define sdl_getmousestate  21
#define sdl_loadwav        22
#define sdl_freewav        23
// more to come ...

#define sdl_wm_setcaption  24
#define sdl_videoinfo      25
#define sdl_maprgb         26

//#define sdl_drawline       27
//#define sdl_drawhline      28
//#define sdl_drawvline      29
//#define sdl_drawcircle     30
//#define sdl_drawrect       31
//#define sdl_drawpixel      32
//#define sdl_drawellipse    33
//#define sdl_drawfillellipse   34
//#define sdl_drawround      35
//#define sdl_drawfillround  36
//#define sdl_drawfillcircle 37
//#define sdl_drawfillrect   38

#define sdl_fillrect       39
#define sdl_fillsurf       40

// Joystick functions
#define sdl_numjoysticks       41
#define sdl_joystickopen       42
#define sdl_joystickclose      43
#define sdl_joystickname       44
#define sdl_joysticknumaxes    45
#define sdl_joysticknumbuttons 46
#define sdl_joysticknumballs   47
#define sdl_joysticknumhats    48

#define sdl_joystickeventstate 49
#define sdl_getticks           50
#define sdl_showcursor         51
#define sdl_hidecursor         52
#define sdl_mksurface          53
#define sdl_setcolourkey       54

#define sdl_joystickgetbutton  55
#define sdl_joystickgetaxis    56
#define sdl_joystickgetball    57
#define sdl_joystickgethat     58


extern char *b2c_str(BCPLWORD bstr, char *cstr);
extern BCPLWORD c2b_str(const char *cstr, BCPLWORD bstr);
extern void copyaddrB2C(void*from, void*to);
extern void copyaddrC2B(void*from, void*to);

BCPLWORD decodeevent(SDL_Event*e, BCPLWORD *ptr) {
  if(e) {
    ptr[0] = (BCPLWORD)(e->type);
    switch (e->type) {
    default:
      printf("sdlfn: Unknown event type %d\n", e->type);
      return -1;

    case SDL_ACTIVEEVENT:      // 1
      ptr[1] = (BCPLWORD)(e->active).gain;  // 0 if loss, 1 if gain
      ptr[2] = (BCPLWORD)(e->active).state; // 0=mouse focus, 1=keyboard focus,
                                            // 2=minimised
      return -1;

    case SDL_KEYDOWN:          // 2
    case SDL_KEYUP:            // 3
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
      return -1;

    case SDL_MOUSEBUTTONDOWN:  // 5
    case SDL_MOUSEBUTTONUP:    // 6
      ptr[1] = (BCPLWORD)(e->button).state;
      ptr[2] = (BCPLWORD)(e->button).x;
      ptr[3] = (BCPLWORD)(e->button).y;
      return -1;

#ifndef JSAvail
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
#endif

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
      //printf("VIDEOEXPOSE=%d\n", SDL_VIDEOEXPOSE);
      return -1;

    case SDL_USEREVENT:        // 24
      return -1;
    }
  }
  *ptr = 0;
  return 0;
}


BCPLWORD sdlfn(BCPLWORD *a, BCPLWORD *g, BCPLWORD *W) {
  // Note that the op in a[0] is always of the form sdl_...
  char tmpstr[256];

  //printf("sdlfn: fno=%d a1=%d a2=%d a3=%d a4=%d a5=%d\n",
  //	 a[0], a[1], a[2], a[3], a[4], a[5]);

  switch(a[0]) {
  default:
    printf("sdlfn: Unknown op:");
    printf("fno=%ld ", (long)a[0]);
    printf("a1=%ld ",  (long)a[1]);
    printf("a2=%ld ",  (long)a[2]);
    printf("a3=%ld ",  (long)a[3]);
    printf("a4=%ld ",  (long)a[4]);
    printf("a5=%ld\n", (long)a[5]);
    return 0;

  case sdl_avail: // Test whether SDL is available
    return -1;    // SDL is available
    
  case sdl_init:  // Initialise all SDL features
    //printf("sdl_init\n");
  {
#ifdef JSAVAIL
    int rc = SDL_Init(SDL_INIT_TIMER |
                      SDL_INIT_AUDIO |
                      SDL_INIT_VIDEO
                     );
#else
    int rc = SDL_Init(SDL_INIT_EVERYTHING);
#endif
    //printf("SDL_Init => %d\n", rc);
    if(rc<0)
    { printf("SDL could not initialize! SDL_Error: %s\n", SDL_GetError());
      return (BCPLWORD) rc;
    }
    // Enable Unicode translation of keyboard events.
    SDL_EnableUNICODE(1);
    SDL_JoystickEventState(SDL_ENABLE);
    return (BCPLWORD) rc;
  }

  case sdl_setvideomode:  // width, height, bbp, flags, scrptr
  { // scrptr is a BCPL pointer to two BCPLWORDs to hold the
    // machine address of the screen.
    //printf("About to call STL_SetVideoMode\n");
    SDL_Surface *scr = SDL_SetVideoMode((int)a[1],    // width
					(int)a[2],    // height
					(int)a[3],    // bbp
					(Uint32)a[4]);// flags
    //printf("About to call STL_Flip\n");
    SDL_Flip(scr);
    copyaddrC2B(&scr, &W[a[5]]);
    return -1;  // Success
  }

  case sdl_quit:      // Shut down SDL
    //printf("sdl_quit\n");
    SDL_Quit();
    return -1;

  case sdl_locksurface: // surf
  { // Return 0 on success
    // Return -1 on failure
    SDL_Surface* surf;
    copyaddrB2C(&W[a[1]], &surf);
    return (BCPLWORD) SDL_LockSurface(surf);
  }
  
  case sdl_unlocksurface: // surf
  { // Return 0 on success
    // Return -1 on failure
    SDL_Surface* surf;
    copyaddrB2C(&W[a[1]], &surf);
    SDL_UnlockSurface(surf);
    return 0;
  }
  
  case sdl_getsurfaceinfo: // surfptr, surfinfo
  { // surfptr
    // surfinfo -> [flag, format, format1,
    //              w, h, pitch,
    //              pixels, pixels1,
    //              cliprectx, cliprecty, cliprectw, cliprecth,
    //              refcount]
    SDL_Surface* surf;
    BCPLWORD *info = &W[a[2]];
    copyaddrB2C(&W[a[1]], &surf);
    info[ 0] = (BCPLWORD) (surf->flags);

    copyaddrC2B(&(surf->format), &info[1]);     // Possible two word address

    info[ 3] = (BCPLWORD) (surf->w);
    info[ 4] = (BCPLWORD) (surf->h);
    info[ 5] = (BCPLWORD) (surf->pitch);

    //copyaddrC2B(&(surf->pixels), &info[6]);   // Possible two word address
    
    info[ 8] = (BCPLWORD) (surf->clip_rect.x);  // Clip rect x
    info[ 9] = (BCPLWORD)  (surf->clip_rect.y);  // Clip rect y
    info[10] = (BCPLWORD) (surf->clip_rect.w+321);  // Clip rect w
    info[11] = (BCPLWORD) (surf->clip_rect.h+654);  // Clip rect h
    info[ 12] = (BCPLWORD) (surf->refcount);
    return 0;        
  }

  case sdl_getfmtinfo:  // fmtptr, pxlinfo
  // fmt
  // pxlinfo -> [palette, bitspp, bytespp, rmask, gmask, rmask, amask,
  //             rloss, rshift, gloss, gshift, bloss, bshift, aloss, ashift,
  //             colorkey, alpha]
  { SDL_Palette* palette;
    SDL_PixelFormat *fmt;
    BCPLWORD *info = &W[a[2]];
    copyaddrB2C(&W[a[1]], &palette);
    fmt = (SDL_PixelFormat*)(palette);
    //printf("getfmtinfo: format=%d\n", (BCPLWORD)fmt);

    copyaddrC2B(&(fmt->palette), &info[ 0]); // Possible two word address

    info[ 2] = (BCPLWORD) (fmt->BitsPerPixel);
    info[ 3] = (BCPLWORD) (fmt->BytesPerPixel);
    info[ 4] = (BCPLWORD) (fmt->Rmask);
    info[ 5] = (BCPLWORD) (fmt->Gmask);
    info[ 6] = (BCPLWORD) (fmt->Bmask);
    info[ 7] = (BCPLWORD) (fmt->Amask);
    info[ 8] = (BCPLWORD) (fmt->Rshift);
    info[ 9] = (BCPLWORD) (fmt->Gshift);
    info[10] = (BCPLWORD) (fmt->Bshift);
    info[11] = (BCPLWORD) (fmt->Ashift);
    info[12] = (BCPLWORD) (fmt->Rloss);
    info[13] = (BCPLWORD) (fmt->Gloss);
    info[14] = (BCPLWORD) (fmt->Rloss);
    info[15] = (BCPLWORD) (fmt->Aloss);
    info[16] = (BCPLWORD) (fmt->colorkey);
    info[17] = (BCPLWORD) (fmt->alpha);

    return 0;        
  }

  case sdl_geterror: // str Fill str with BCPL string for the SDL error
  { char *str = SDL_GetError();
    printf("sdl_geterror: %s\n", str);
    return c2b_str(str, a[1]); // Convert to BCPL string format
  }

  case sdl_updaterect: // surfptr, left, top, right, bottom
    return 0;     // Not yet available

  case sdl_loadbmp:    // filename, surfptr
  { char tmpstr[256];
    SDL_Surface *surf;
    b2c_str(a[1], tmpstr);      // The bmp filename
    surf = SDL_LoadBMP(tmpstr);
    copyaddrC2B(&surf, &W[a[2]]);  // The surface
    return -1; // Success
  }

  case sdl_mksurface: //(formatptr, w, h, surfptr)
  { SDL_PixelFormat *fmt;
    SDL_Surface *surf;
    copyaddrB2C(&W[a[1]], &fmt);
    Uint32 rmask = fmt->Rmask;
    Uint32 gmask = fmt->Gmask;
    Uint32 bmask = fmt->Bmask;
    Uint32 amask = fmt->Amask;
    //printf("rmask=%8x gmask=%8x bmask=%8x amask=%8x\n",
    //        rmask, gmask, bmask, amask);
    surf = SDL_CreateRGBSurface(
                         SDL_SWSURFACE,
                         a[2], a[3], // Width, Height
                         32,     // Not using a palette
                         rmask, gmask, bmask, amask);
    if(surf==0) return 0;       // Return FALSE
    copyaddrC2B(&surf, &W[a[4]]);
    return -1;                  // Return TRUE
  }

  case sdl_blitsurface: // srcptr, srcrect, destptr, destrect
  { //printf("blitsurface: %d, %d, %d, %d\n", a[1], a[2], a[3], a[4]);
    // Return 0 if successful
    SDL_Surface *src;
    copyaddrB2C(&W[a[1]], &src);
    SDL_Rect *srcrect = 0;         // Ignore the src rect
    SDL_Surface *dest;
    copyaddrB2C(&W[a[3]], &dest);
    BCPLWORD *p = &W[a[4]];
    SDL_Rect dstrect = {p[0],p[1],p[2],p[3]};
    //printf("blitsurface: distrect -> [ %d %d %d %d ]\n", p[0],p[1],p[2],p[3]);
    return (BCPLWORD) SDL_BlitSurface(src,
				      srcrect, // =0
				      dest,
                                      &dstrect);
  }

  case sdl_setcolourkey: //(surfptr, key)
  { // If key=-1 unset colour key
    // otherwise set colour key to given value.
    // key must be in the pixel format of the given surface
    // It returns 0 if successful
    //printf("sdl_setcolourkey: %8x\n", a[2]);
    SDL_Surface *surf;
    copyaddrB2C(&W[a[1]], &surf);
    if(a[2]==-1) {
      return (BCPLWORD)SDL_SetColorKey(surf, 0, (Uint32)a[2]);
    } else {
      return (BCPLWORD)SDL_SetColorKey(surf, SDL_SRCCOLORKEY, (Uint32)a[2]);
    }
  }
  
  case sdl_freesurface: // surfptr
  { SDL_Surface *surf;
    copyaddrB2C(&W[a[1]], &surf);
    SDL_FreeSurface(surf);
    return 0;
  }
  
  case sdl_setalpha:    // surf, flags, alpha
    return 0;     // Not yet available

  case sdl_imgload:     // filename -- using the SDL_image library
    return 0;     // Not yet available

  case sdl_delay:       // msecs -- the SDL delay function
    SDL_Delay((int)a[1]);
    return 0;

  case sdl_getticks:    // return msecs since initialisation
    return (BCPLWORD)SDL_GetTicks();

  case sdl_showcursor:  // Show the cursor
    return (BCPLWORD)SDL_ShowCursor(SDL_ENABLE);

  case sdl_hidecursor:  // Hide the cursor
    return (BCPLWORD)SDL_ShowCursor(SDL_DISABLE);

  case sdl_flip:        // surfptr -- Double buffered update of the screen
  { SDL_Surface *surf;
    copyaddrB2C(&W[a[1]], &surf);
    //printf("About to call SDL_Flip\n");
    return (BCPLWORD) SDL_Flip(surf);
  }
  
  case sdl_displayformat: // surfptr -- convert surf to display format
    return 0;     // Not yet available

  case sdl_waitevent:    // (pointer) to [type, args, ... ] to hold details of
                         // the next event
                 // return 0 if no events available
    return 0;     // Not yet available

  case sdl_pollevent:    // (pointer) to [type, args, ... ] to hold details of
			 // the next event
    { SDL_Event test_event;
      if (SDL_PollEvent(&test_event))
      { decodeevent(&test_event, &W[a[1]]);
        return -1;
      }
      decodeevent(0, &W[a[1]]);
      return 0;
    }

  case sdl_getmousestate: // pointer to [x, y] returns bit pattern of buttons
                          // currently pressed
    return 0;     // Not yet available

  case sdl_loadwav:      // file, spec, buff, len
    return 0;     // Not yet available

  case sdl_freewav:      // buffer
    return 0;     // Not yet available

  case sdl_wm_setcaption:      // surf, string
  { char tmpstr[256];
    b2c_str(a[1], tmpstr);
    //printf("sdl_wm_setcaption: %s\n", tmpstr);
    SDL_WM_SetCaption(tmpstr, 0);
    return 0;
  }

  case sdl_videoinfo:      // buffer
  { const SDL_VideoInfo* p = SDL_GetVideoInfo();
    BCPLWORD *info = &W[a[1]];
    info[ 0] = (BCPLWORD) ((p->hw_available) |
                           (p->hw_available)<<1 |
                           (p->blit_hw)<<2 |
                           (p->blit_hw_CC)<<3 |
                           (p->blit_hw_A)<<4 |
                           (p->blit_sw)<<5 |
                           (p->blit_sw_CC)<<6 |
                           (p->blit_sw_A)<<7
                          );
    //info[ 1] = (BCPLWORD) (p->blit_fill);
    //info[ 2] = (BCPLWORD) (p->video_mem);

    //copyaddrC2B((void*)(&(p->vfmt)), &info[3]); // Possible two word address

    //info[ 5] = (BCPLWORD) (p->vfmt->BitsPerPixel);
    //printf("videoinfo: a[2]=%d %8X %8X %d %d %d\n",
    //          a[2], info[0], info[1], info[2], info[3], info[4]);
 
    return 0;
  }


  case sdl_maprgb:      // format, r, g, b
  { SDL_PixelFormat *format;
    copyaddrB2C(&W[a[1]], &format);
    //printf("sdl_maprgb: r=%d g=%d b=%d\n", a[2], a[3], a[4]);
    return (BCPLWORD) SDL_MapRGB(format, a[2], a[3], a[4]); 
  }

  //case sdl_drawline:
  //{ SDL_Surface *surf;
  //  copyaddrB2C(&W[a[1]], &surf);
  //  //printf("\nDraw Line: %d %d %d %d %d %8x\n", a[1], a[2], a[3], a[4], a[5], a[6]);
  //  Draw_Line(surf, a[2], a[3], a[4], a[5], a[6]);
  //  return 0;
  //}

  //case sdl_drawhline:
  //case sdl_drawvline:
  //case sdl_drawcircle:
  //case sdl_drawrect:
  //case sdl_drawpixel:
  //case sdl_drawellipse:
  //case sdl_drawfillellipse:
  //case sdl_drawround:
  //case sdl_drawfillround:
  //  return 0;

    //  case sdl_drawfillcircle: // surfptr, x, y, r, colour
    //{ SDL_Surface *surf;
    // copyaddrB2C(&W[a[1]], &surf);
    //Draw_FillCircle(surf, a[2], a[3], a[4], a[5]);
    //return 0;
    //}
    //  case sdl_drawfillrect:
    //return  Draw_FillRect((SDL_Surface*)a[1], 500,200, 50,70, 0xF0FF00);

  case sdl_fillrect: // surfptr, x, y, w, h, colour
  { SDL_Surface *surf;
    copyaddrB2C(&W[a[1]], &surf);
    SDL_Rect rect = {a[2],a[3],a[4],a[5]};
    //printf("\nfillrect: surface=%d rect=(%d,%d,%d,%d) col=%8x\n",
    //       a[1], a[2], a[3], a[4], a[5], a[6]);
    SDL_FillRect(surf, &rect, (Uint32)a[6]);
    return 0;
  }

  case sdl_fillsurf: // surfptr, colour
  { SDL_Surface *surf;
    copyaddrB2C(&W[a[1]], &surf);
    //printf("\nfillsurf: surface=%d col=%8x\n",
    //        a[1], a[2]);
    SDL_FillRect(surf, 0, a[2]);
    return 0;
  }
  
// Joystick functions
  case sdl_numjoysticks:       // 41 ()
    return SDL_NumJoysticks();

  case sdl_joystickopen:       // 42 (index, joyptr)
  { SDL_Joystick *joystick;
    joystick = SDL_JoystickOpen(a[1]);
    copyaddrC2B(&joystick, &W[a[2]]);
    return 0;
  }
  
  case sdl_joystickclose:      // 43 (joyptr)
  { SDL_Joystick *joystick;
    copyaddrB2C(&W[a[1]], &joystick);
    SDL_JoystickClose(joystick);
    return 0;
  }
  
  case sdl_joystickname:       // 44 (index, name)
  { const char *name = SDL_JoystickName(a[1]);
    return c2b_str(name, a[2]);
  }

  case sdl_joysticknumaxes:    // 45 (joyptr)
  { SDL_Joystick *joystick;
    copyaddrB2C(&W[a[1]], &joystick);
    return (BCPLWORD)SDL_JoystickNumAxes(joystick);
  }
  
  case sdl_joysticknumbuttons: // 46 (joyptr)
  { SDL_Joystick *joystick;
    copyaddrB2C(&W[a[1]], &joystick);
    return SDL_JoystickNumButtons(joystick);
  }
  
  case sdl_joysticknumballs:   // 47 (joyptr)
  { SDL_Joystick *joystick;
    copyaddrB2C(&W[a[1]], &joystick);
    return SDL_JoystickNumBalls(joystick);
  }
  
  case sdl_joysticknumhats:    // 47 (joyptr)
  { SDL_Joystick *joystick;
    copyaddrB2C(&W[a[1]], &joystick);
    return SDL_JoystickNumHats(joystick);
  }
      
  case sdl_joystickeventstate: //49  sdl_enable=1 or sdl_ignore=0
    return SDL_JoystickEventState(a[1]);

  case sdl_joystickgetbutton:  // 55 (joyptr)
  { SDL_Joystick *joystick;
    copyaddrB2C(&W[a[1]], &joystick);
    return SDL_JoystickGetButton(joystick, a[2]);
  }
  
  case sdl_joystickgetaxis:    // 56 (joyptr)
  { SDL_Joystick *joystick;
    copyaddrB2C(&W[a[1]], &joystick);
    return SDL_JoystickGetAxis(joystick, a[2]);
  }
  
  case sdl_joystickgethat:     // 58 (joyptr)
  { SDL_Joystick *joystick;
    copyaddrB2C(&W[a[1]], &joystick);
    return SDL_JoystickGetHat(joystick, a[2]);
  }
  }
}
#endif
