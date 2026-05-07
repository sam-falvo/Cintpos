GET "mcpl.h"

/*
  This is a demonstration MCPL implementation of the widely
  used RC4 encryption algorithm.
*/

FUN rc4 : key, data =>
  LET k = CVEC 255
  LET s = CVEC 255
  LET keyp = 0
  FOR i = 0 TO 255 DO { LET byte = key%keyp++
                        k%i, s%i := byte, i
                        IF byte DO keyp := 0
		      }
  FOR i = 0 TO 255 DO { LET j = (s%i+k%i) & 255
		        s%i, s%j := s%j, s%i
		      }
  LET p=0, i=0, j=0

  { LET byte = data%p
    IF byte=0 RETURN
    i := (i+1) & 255
    j := (j+s%i) & 255
    s%i, s%j := s%j, s%i
    LET bits = s%((s%i+s%j) & 255)
    data%p := byte XOR bits
    writef("  %2x %2x => %2x\n", byte, bits, data%p)
    p++
  } REPEAT
.

FUN start : =>
  LET key  = "My Secret Key"
  LET data = "My Message"

  writef("Data: \"%s\"\n", data)
  writef(" Key: \"%s\"\n", key)

  rc4(key, data)

  writef "\nNow decode by encrypting a second time with the same key.\n"

  rc4(key, data)

  writef("\nDecoded data: \"%s\"\n", data)

  RETURN 0
.

