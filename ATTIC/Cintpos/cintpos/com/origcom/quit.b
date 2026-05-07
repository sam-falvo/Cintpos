SECTION "Quit"
GET "libhdr"
GET "clihdr"
LET charstonum(s) = VALOF
  $( LET n,i,m = 0,1,s % 0
     LET neg = FALSE
     result2:=0
     IF s=0 THEN RESULTIS 0

     WHILE s % i = ' ' DO i := i + 1

     IF (s % i = '+') | (s % i = '-') THEN
       $( neg := s % i = '-'
          i := i + 1
       $)

     TEST ('0'<=s%i<='9') THEN
     WHILE ('0' <= s % i <= '9') & (i <= m) DO
       $( n := n * 10 + (s % i - '0')
          i := i + 1
       $)
     ELSE result2:=20

     RESULTIS (neg -> -n,n)
  $)


LET start() BE
$(  LET ch=?
    LET no=VEC 50
    LET failed= 0=rdargs("RC",no,50)
    result2:=0
    IF failed THEN writes("Bad arguments for Quit*N")
    $(  LET rc=(failed->0,charstonum(no!0))
        IF result2>0 THEN writes("Bad return code spec for Quit*N")
        UNLESS cli.standardinput = cli.currentinput
        THEN ch:=rdch() REPEATUNTIL ch=endstreamch
        result2:=0
        stop(rc)
    $)

$)
