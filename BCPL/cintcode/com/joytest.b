GET "libhdr"
GET "joy.h"

LET start() = VALOF
{ LET stdin = input()
  LET fd = 0

  writef("Testing the joystick*n")

  writef("*nObserving how icountmax changes*n")
  FOR i = 0 TO 9 DO
  { IF i MOD 10 = 0 DO writef("*n%i3: ", i)
    writef(" %i8", rootnode!rtn_icountmax)
    deplete(cos)
    FOR i = 1 TO 10000000 LOOP
  }
  newline()

  UNLESS sys(Sys_joy, joy_avail) DO
  { writef("JSAvail is not set so the joystick is not available*n")
    RESULTIS 0
  }

  //writef("JSAvail is set*n*n")

  fd := sys(Sys_joy, joy_open)

  UNLESS fd DO
  { writef("No joystick device found*n")
    RESULTIS 0
  }

  //writef("Joystick fd %n*n", rootnode!rtn_joystickfd)

  FOR i = 1 TO 9999 DO
  { FOR j = 1 TO 100000 LOOP
    // Stop if both buttons 1 and 2 are pressed.
    IF rootnode!rtn_joycurrbuttons = 3 BREAK
    writef("joytest: %i4 %16b", i, rootnode!rtn_joybuttons)
    writef(" %16b ", rootnode!rtn_joycurrbuttons)
    FOR k = 0 TO 6 DO writef(" %i7", (@rootnode!rtn_joyaxis0)!k)
    wrch('*c')
    deplete(cos)
    IF i MOD 50 = 0 DO sys(Sys_joy, joy_clear)   
  }

  newline()

  sys(Sys_joy, joy_close, fd)

  writef("About %n Cintcode instructions per second*n", rootnode!rtn_icountmax)

  writef("End of test*n")

  RESULTIS 0
}
