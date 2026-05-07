// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "CreateDir"

GET "LIBHDR"

LET start() BE
$( LET v = VEC 50
   LET rc, res2 = 0, 0
   TEST rdargs("/A", v, 50)=0 THEN
   $( res2 := result2
      writes("Bad args*N")
      rc := 20
   $)
   ELSE
   $( LET obj = ?

//// patch for PB filing system

/**/  obj := locateobj ( v!0 )
/**/  IF obj \= 0 THEN $( freeobj ( obj )
/**/                      writef ( "%S already exists*N", v!0 )
/**/                      stop ( 20 )
/**/                   $)
      obj := createdir ( v!0 )
      freeobj(obj)
      IF obj=0
      THEN
      $( res2 := result2
         writef("Can't create directory %S*N", v!0)
         rc := 10
      $)
   $)
   result2 := res2
   stop(rc)
$)
