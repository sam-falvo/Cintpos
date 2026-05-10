// (c)  Copyright:  Martin Richards  30 April 2014

/*
21/11/14
Corrected bug in /N qualifier of rdargs and added the function rdargs2.

09/01/14
rdargs rewritten.

29/03/10
Changed the units of time to be msecs instead of the system dependent
ticks. Tickspersecond has been removed and the datstamp format changed to:
datv!0 = days  since 1 Jan 1970
datv!1 = msecs  since midnight
datv!2 = -1 to indicate that the new date and time format is being used.
sys(Sys_delay, msecs) performs a delay in msecs (not ticks).
The cli prompt is now written using
writef(promptstring, cputime, taskid, hours, mins, secs, msecs)
Try the command: prompt "%+%+%z2:%z2:%z2.%z3> "

01/10/07
Modified initco to return a second result in result2

03/07/07
Added codewrch to write extended characters using UTF8 or
GB2312. Added %# substitution item in writef to invoke it. Note that
*xU, *xG, *#hhhh, *##hhhhhhhh and *#dddd escapes have been added to
BCPL string and character constants.

29/6/02
Renamed IOLIB as DLIB (the system Dependent Library). Put system
independent code in BLIB and the rest in DLIB.

24/4/04
Made many changed to make BLIB more compatible between Cintpos and
single threaded Cintcode BCPL.

21/3/2003
Make instrcount(f,a,b,...) set result2 to result of f(a,b,c,....)

10/7/2000
Changed the definition of mkobj to take up to 11 initialisation
arguments. See bcplprogs/objdemo.b

28/2/2000
Added function instrcount(f,a,b,c,e,f,r,g,h,i,j,k)
which returns the number of cintcode instructions executed
when calling f(a,b,...).

30/4/1996
Added function flush()
with corresponding change in cintsys.c and cintpos.c

7/6/1996
Defined mkobj(upb, fns, a, b) for use in object oriented programming.
See bcplprogs/objdemo.b  (args a and b added 30 March 1999).
*/

SECTION "BLIB"


GET "libhdr"

LET stop(code, reason) BE
{ // Return to the CLI with the given return code and reason.
  // It must be called from the command's main coroutine, ie
  // not an inner coroutine.
  result2 := reason
  cowait(code)
}

LET clihook(stackupb) = start()

