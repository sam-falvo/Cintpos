/*
This tests the new getbuilt facility

Implemented by Martin Richards (c) 03/02/2021

History

03/02/2021
Initial implementation

*/

GET "libhdr"

LET start() = VALOF
{ LET buildno = sys(Sys_getbuildno)
  LET flags = result2

  writef("Build number: %n %s*n", buildno, buildno2str(buildno))
  writef("Flags: %8b", flags)
  prflags(flags)

  RESULTIS 0
}

AND buildno2str(n) = VALOF SWITCHON n INTO
{ DEFAULT:  RESULTIS "Unknown"

  CASE bld_linux:             RESULTIS "Linux"
  CASE bld_linuxSDL:          RESULTIS "LinuxSDL"
  CASE bld_linuxSDL2:         RESULTIS "LinuxSDL2"
  CASE bld_linuxGL:           RESULTIS "LinuxGL"
  CASE bld_linuxSDLGL:        RESULTIS "LinuxSDLGL"
  CASE bld_linuxSDL2GL:       RESULTIS "LinuxSDL2GL"
  CASE bld_linuxiSH:          RESULTIS "LinuxiSH"

  CASE bld_Raspi:             RESULTIS "Raspi"
  CASE bld_RaspiSDL:          RESULTIS "RaspiSDL"
  CASE bld_RaspiSDL2:         RESULTIS "RaspiSDL2"
  CASE bld_RaspiSDLGL:        RESULTIS "RaspiSDLGL"
  CASE bld_RaspiSDL2GL:       RESULTIS "RaspiSDL2GL"

  CASE bld_MacOSX:            RESULTIS "MacOSX"
  CASE bld_MacOSXSDL:         RESULTIS "MacOSXSDL"
  CASE bld_MacOSXSDL2:        RESULTIS "MacOSXSDL2"
  CASE bld_MacOSXSDLGL:       RESULTIS "MacOSXSDLGL"
  CASE bld_MacOSXSDL2GL:      RESULTIS "MacOSXSDL2GL"

  CASE bld_VmsVax:            RESULTIS "VmsVax"
  CASE bld_Win32:             RESULTIS "Win32"
  CASE bld_CYGWIN:            RESULTIS "CYGWIN"
}

AND prflags(flags) BE
{ UNLESS (flags&bldf_sound)=0    DO writef(" SOUND")
  UNLESS (flags&bldf_callc)=0    DO writef(" CALLC")
  UNLESS (flags&bldf_joystick)=0 DO writef(" joystick")
  newline()
}
