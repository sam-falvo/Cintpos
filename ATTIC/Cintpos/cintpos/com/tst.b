GET "libhdr"

GLOBAL { g759:759 }

LET start() BE
{ LET x = 0
//  FOR upb=0 TO 4 DO
//  { LET v = getvec(upb)
//    FOR i = 0 TO upb DO v!i := #x11111111
//    sawritef("*ngetvec(%n) => %n*n", upb, v)
//    abort(1000)
//    freevec(v)
//  }
//  RETURN
  LET n1 = "TCP:localhost:9000"
  LET n2 = "TCP:localhost:9001"
  LET tcp1 = findoutput(n1)
  LET tcp2 = findoutput(n2)
  LET in = 0
sawritef("calling pathfindinput(taskobj:config, PVSPATH)*n")
  in := pathfindinput("taskobj:config", "PVSPATH")
sawritef("pathfindinput(taskobj:config, PVSPATH) => %n*n", in)
selectinput(in)
FOR i = 1 TO 100 DO 
{ LET ch = rdch()
  IF ch=endstreamch BREAK
  wrch(ch)
}
newline()
endread()
RETURN
writef("*n")
writef("a*n")
writef("ab*n")
writef("abc*n")
writef("abcd*n")
writef("abcde*n")
writef("abcdef*n")
writef("abcdefg*n")
writef("abcdefgh*n")
writef("abcdefghi*n")
writef("abcdefghij*n")
writef("abcdefghijk*n")
writef("abcdefghijkl*n")
writef("abcdefghijklm*n")
writef("abcdefghijklmn*n")
writef("abcdefghijklmno*n")
writef("abcdefghijklmnop*n")
writef("abcdefghijklmnopq*n")

sawritef("tst: tcp1=%n tcp2=%n*n", tcp1, tcp2)

  TEST tcp1 THEN { selectoutput(tcp1)
                   writef("Text sent to tcp1, port 9000*n")
                 }
            ELSE sawritef("Can't open stream %s*n", n1)
 
  TEST tcp2 THEN { selectoutput(tcp2)
                   writef("Text sent to tcp2, port 9001*n")
                 }
            ELSE sawritef("Can't open stream %s*n", n2)

  endstream(tcp1)
  endstream(tcp2)
  RETURN

  sawritef("Making indirect reference to: %x8 (%n)*n", g759, g759)
  x := !g759
  sawritef("Calling global 759*n")
  g759()
}
