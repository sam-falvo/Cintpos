GET "libhdr"

LET start() = VALOF
{ LET byte = 0

  LET js = findinput("/dev/input/js0")

  UNLESS js DO
  { writef("Unable to find the joystick*n")
    RESULTIS 0
  }

  selectinput(js)

  FOR i = 1 TO 50 DO
  { LET byte = binrdch()
    writef("js: byte = %x8  %c*n", byte, 32<=byte<=127 -> byte, '-')
    delay(500)
  }

  writef("End of output*n")
  RESULTIS 0
}
