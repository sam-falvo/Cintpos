// (c)  Copyright:  Martin Richards  17 June 1997

/*
16/6/1997
  Defined mkobj(upb, fns) for use in object oriented programming.
  See mcplprogs/objdemo.m
*/

MODULE mlib

GET "mcpl.h"
GET "io.h"

MANIFEST
    B2Wsh=2,  // for a 32 bit implementation
//  B2Wsh=3,  // for a 64 bit implementation
    Bpw=1<<B2Wsh 

FUN clihook : stackupb =>
  rootnode := VEC Rtn_upb
  FOR i = 0 TO Rtn_upb DO rootnode!i := 0
  hptr := 0

  selectoutput(findoutput "*")
  selectinput(findinput "*")

  writes "Native MCPL entered\n"

  currco := @(@ stackupb)!(-3-7) // Initialise the coroutine environment
  colist := currco
  currco!Co_pptr   := 0
  currco!Co_parent := -1  // Mark as root coroutine.
  currco!Co_list   := 0

  currco!Co_fn   := clihook      // fn
  currco!Co_size := stackupb     // stackupb (first arg of clihook)
  currco!5       := 0            // c
  currco!6       := hptr         // h

  RETURN start 0

FUN stop : n => cowait n

FUN intflag : => sys 28   // returns TRUE if user interrupt

FUN abort : code => sys(0, code)

FUN sardch : =>  sys 10 

FUN sawrch : ch  => sys(11,ch)

FUN sawritef : form, a, b, c, d, e, f, g, h, i, j, k, m, n, o =>
  LET wch = wrch
  wrch := sawrch
  writef(form, a, b, c, d, e, f, g, h, i, j, k, m, n, o)
  wrch := wch

FUN rdch : => 
  LET pos = cis!Scb_pos
  IF pos < cis!Scb_end DO { cis!Scb_pos := pos+1
                            RETURN cis%pos
                          }
  RETURN (cis!Scb_rdfn)(cis)

FUN unrdch : =>
  LET pos = cis!Scb_pos
  IF pos<=Scb_bufstart RETURN FALSE // Cannot UNRDCH past origin.
  cis!Scb_pos := pos-1
  RETURN TRUE

FUN wrch : ch =>
  LET pos = cos!Scb_pos
  cos%pos := ch
  cos!Scb_pos := pos+1
  IF pos>=cos!Scb_end UNLESS (cos!Scb_wrfn) cos DO abort 189

FUN findinput : string => findstream(string, Id_inscb)

FUN findoutput : string => findstream(string, Id_outscb)

FUN findstream : name, id =>
  LET console = compstring("*", name)=0

  IF console DO
  { IF id=Id_inscb AND rootnode!Rtn_keyboard~=0
       RETURN rootnode!Rtn_keyboard
    IF id=Id_outscb AND rootnode!Rtn_screen~=0
       RETURN rootnode!Rtn_screen
  } 

  LET scb = getvec Scb_upb
  IF scb=0 RETURN 0

  scb!Scb_pos   := 0
  scb!Scb_end   := 0
  scb!Scb_file  := 0
  scb!Scb_id    := id
  scb!Scb_work  := 0
  scb!Scb_rdfn  := falsefn
  scb!Scb_wrfn  := falsefn
  scb!Scb_endfn := falsefn

  IF console DO
  { scb!Scb_file := -1               // Console stream
    scb!Scb_work := FALSE
    IF id=Id_inscb  DO { scb!Scb_rdfn := cnslrdfn
                         rootnode!Rtn_keyboard := scb
                       }
    IF id=Id_outscb DO { scb!Scb_wrfn := cnslwrfn
                         scb!Scb_pos := Scb_bufstart
                         scb!Scb_end := Scb_bufstart
                         rootnode!Rtn_screen := scb
                       }
    RETURN scb
  }
  IF id=Id_inscb  DO { scb!Scb_file := sys(14, name)
                       scb!Scb_rdfn := filerdfn
                     }
  IF id=Id_outscb DO { scb!Scb_file := sys(15, name)
                       scb!Scb_pos  := Scb_bufstart
                       scb!Scb_end  := Scb_bufend
                       scb!Scb_wrfn := filewrfn
                     }
  IF scb!Scb_file=0 DO { freevec(scb); RETURN 0 }
  scb!Scb_endfn := fileendfn
  RETURN scb

FUN falsefn : scb => FALSE

FUN cnslrdfn : scb =>
  LET p = Scb_bufstart
  IF scb!Scb_work RETURN Endstreamch

  {  LET ch = sys 10
     MATCH ch
     : Endstreamch => scb!Scb_work := TRUE
                      IF p=Scb_bufstart RETURN Endstreamch
                      BREAK 
     : '\n'        => scb%p++ := ch
                      BREAK
     : '\b'        => IF p>Scb_bufstart DO p--
                      sys(11, ' ')
                      sys(11, '\b')
                      LOOP
     :             => scb%p++ := ch
                      IF p<Scb_bufend LOOP
                      BREAK
  } REPEAT

  scb!Scb_pos, scb!Scb_end := Scb_bufstart+1, p
  RETURN scb%Scb_bufstart


FUN cnslwrfn : scb =>
  sys(11, scb%Scb_bufstart)
  scb!Scb_pos := Scb_bufstart
  RETURN TRUE


FUN filerdfn : scb =>
  LET buf = scb + Scb_bufstart
  LET len = sys(12, scb!Scb_file, buf, Scb_buflen)
  IF len=0 RETURN Endstreamch
  scb!Scb_pos := Scb_bufstart+1
  scb!Scb_end := Scb_bufstart+len
  RETURN scb%Scb_bufstart

   
FUN filewrfn : scb =>
  LET buf = scb + Scb_bufstart
  LET len = scb!Scb_pos - Scb_bufstart
  scb!Scb_pos := Scb_bufstart
  RETURN sys(13, scb!Scb_file, buf, len)=len -> TRUE, FALSE

FUN fileendfn : scb => sys(16, scb!Scb_file)

FUN selectinput : scb =>
  IF scb=0 OR scb!Scb_id~=Id_inscb DO abort 186
  cis := scb

FUN selectoutput : scb =>
  IF scb=0 OR scb!Scb_id~=Id_outscb DO abort 187
  cos := scb

FUN endread : =>
  UNLESS (cis!Scb_endfn)(cis) DO abort 190
  freevec(cis)

FUN endwrite : =>
  UNLESS (cos!Scb_wrfn)(cos) DO abort 189
  UNLESS (cos!Scb_endfn)(cos) DO abort 191
  freevec(cos)

FUN input : => cis

FUN output : => cos


FUN readn : => 
  LET sum=0, neg, ch
  ch := rdch() REPEATWHILE ch='\s' OR ch='\t' OR ch='\n' 
  neg := ch='-'
  IF ch='-' OR ch='+' DO ch := rdch()
  UNLESS '0'<=ch<='9' DO { unrdch(); result2 := -1; RETURN 0 }
  WHILE '0'<=ch<='9' DO { sum := 10*sum + ch - '0'; ch := rdch() }
  unrdch()
  result2 := 0
  RETURN neg -> -sum, sum

FUN newline : => wrch '\n'

FUN newpage : => wrch '\p'

FUN writed : n, d =>
  LET t=VEC 20, i=0, k=-n
  IF n<0 DO d, k := d-1, n
  t%i, i, k := -(k MOD 10), i+1, k/10 REPEATUNTIL k=0
  FOR j = i+1 TO d DO wrch '\s'
  IF n<0 DO wrch '-'
  FOR j = i-1 TO 0 BY -1 DO wrch(t%j+'0')

FUN writeu : n, d =>
  LET m = (n>>1)/5
  UNLESS m=0 DO { writed(m, d-1); d := 1 }
  writed(n-m*10, d)

FUN writebin :n, d =>
  IF d>1 DO writebin(n>>1, d-1)
  wrch((n&1)+'0')

FUN writeoct :n, d =>
  IF d>1 DO writeoct(n>>3, d-1)
  wrch((n&7)+'0')

FUN writehex : n, d =>
  IF d>1 DO writehex(n>>4, d-1)
  wrch("0123456789ABCDEF"%(n&15) )

FUN writes : s => { LET ch = s++%0
                    IF ch=0 RETURN
                    wrch ch 
                  } REPEAT

FUN writet : s, d => { LET ch = s++%0
                       IF ch=0 BREAK
                       wrch(ch)
                       d--
                     } REPEAT
                     FOR i = 1 TO d DO wrch '\s'

FUN writef : format, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p =>
  LET t = @ a

  { LET k = format++%0
    IF k=0 RETURN

    TEST k='%'

    THEN { LET f, n=0
           { k := format++%0
             UNLESS '0'<=k<='9' BREAK
             n := 10*n + k - '0'
           } REPEAT
           { MATCH capitalch k
             : 'T' => f := writet
             : 'B' => f := writebin
             : 'O' => f := writeoct
             : 'X' => f := writehex
             : 'D' => f := writed
             : 'U' => f := writeu
             : 'S' => f := writes
             : 'C' => f := wrch
             : '$' => t := t+1; LOOP
             :     => wrch k; LOOP
           }

           f(!t, n)
           t+++
         }

    ELSE wrch k
  } REPEAT

STATIC seed = 12345

FUN randno : upb =>  // return a random number in the range 1 to upb
  seed := seed*2147001325 + 715136305
  RETURN ABS(seed/3) MOD upb + 1

FUN muldiv : a, b, c => sys(26, a, b, c)

FUN capitalch : ch => 'a' <= ch <= 'z' -> ch + 'A' - 'a', ch

FUN compch : ch1, ch2 =>
  LET res = capitalch(ch1) - capitalch(ch2)
//sawritef("compch: ch1 = %c  ch2 = %c  res = %d\n", ch1, ch2, res)
  res

FUN compstring : s1, s2 =>
 { LET ch1=%s1++, ch2=%s2++
   LET cmp = compch(ch1, ch2)
   IF cmp>0 RETURN 1
   IF cmp<0 RETURN -1
   IF ch1=0 RETURN 0
 } REPEAT


FUN str2numb : s =>
  LET a=0, neg=FALSE
  { LET ch = %s++
    IF ch='-' DO neg := TRUE
    IF ch=0 RETURN neg-> -a, a
    LET dig = ch  - '0'
    IF 0<=dig<=9 DO a := 10*a + dig
  } REPEAT

FUN rdargs : keys, argv, upb =>
// rdargs reads the arguments of a command upto and including
// the newline or semicolon that terminates the argument list.
// On failure, rdarg reads input until '\n', ';' or end-of-file
// and returns 0
  LET w=argv, wmax=@argv!(upb+1), numbargs=1

  !w := 0
  LET p = keys

  { MATCH %p++
    :  0  => BREAK
    : '/' => { MATCH capitalch(%p++)
               : 'A' => !w := !w | 1
               : 'K' => !w := !w | 2
               : 'S' => !w := !w | 4
             }
    : ',' => numbargs++
             UNLESS +++w>wmax DO !w := 0
    :     => LOOP
  } REPEAT

  w+++

// At this stage, the argument elements of argv have been
// initialised to  0    -
//                 1   /A
//                 2   /K
//                 3   /A/K
//                 4   /S
//                 5   /S/A
//                 6   /S/K
//                 7   /S/A/K

  { LET wupb = wmax - w

    { LET item = rditem(w, wupb)

      MATCH item
      : 3 | 4 | 0 =>  // '\n',  ';'  or  Endstreamch
             FOR i = 0 TO numbargs - 1 DO
             {  LET a = argv!i
                IF 0<=a<=7 TEST a&1 = 0 THEN argv!i := 0 
                                        ELSE RAISE 13 // error
             }
             RETURN w

      : 1 | 2 => // ordinary item or quoted item
             LET argno = -1

             IF item=1 DO
             { IF compstring("?", w)=0 DO
               { writef("\n%s: ", keys) // help facility
                 UNLESS rdch()='\n' DO unrdch()
                 LOOP
               }
               argno := findarg(keys, w)

               IF argno>=0 DO
                  TEST 4 <= argv!argno <= 7
                  THEN {  argv!argno := -1  // a switch arg
                          LOOP
                       }
                  ELSE IF rditem(w, wupb)<=0 RAISE 11 // Error
               // we must have found a positional argument
             }

             IF argno<0 FOR i = 0 TO numbargs-1 DO
                        { MATCH argv!i
                          : 0 | 1 => argno := i; BREAK
                          : 2 | 3 => RAISE 12 // error
                          :       =>
                        }
             UNLESS argno>=0 RAISE 11 // Error

             argv!argno := w
             WHILE %w DO w++
             w++

      :   => RAISE 10 // error
    }
  } REPEAT
  HANDLE : err =>
      LET ch = ?
      ch := rdch() REPEATUNTIL ch='\n' OR ch=';' OR ch=Endstreamch
      RETURN 0 // Error



// rditem read an item from command line
//    into byte vector   p 
//    with upper bound   upb
// returns -1    error
//          0    Endstreamch
//          1    unquoted item
//          2    quoted item
//          3    \n
//          4    ;

FUN rditem : p, upb =>
  LET pmax = p+upb
  LET ch = rdch()

  %p := 0

  // Skip over blank space.
  WHILE ch='\s' OR ch='\t' DO ch := rdch()

  IF ch=Endstreamch RETURN 0
  IF ch='\n'        RETURN 3
  IF ch=';'         RETURN 4

  IF ch='"' DO { ch :=  rdch()
                 IF ch='\n' OR ch=Endstreamch RETURN -1  // Error
                 IF ch='"'  RETURN 2 // Found a quoted string.
                 IF ch='\\' DO {  ch := rdch()
                                  IF capitalch(ch)='N' DO ch := '\n'
                               }
                 IF p>pmax RETURN -1  // Error
                 %p++ := ch
                 %p   := 0
               } REPEAT

  UNTIL ch='\n' OR ch='\s' OR ch=';' OR ch=Endstreamch DO
  { IF p>pmax RETURN -1  // Error
    %p++ := ch
    %p   := 0
    ch := rdch()
  }

  UNLESS ch=Endstreamch DO unrdch()
  RETURN 1  // Found an unquoted item


FUN findarg : keys, w =>  // =argno  if found
                          // =-1     otherwise
  LET matching=TRUE, argno=0, p=w

  { MATCH(  %keys++,  %p++, matching)

    : '='|'/'|','|0,     0,   TRUE => RETURN argno

    :             0,     ?,      ? => RETURN -1

    :           '/',     ?,      ? => matching, p := FALSE, w

    :           '=',     ?,      ? => matching, p := TRUE,  w

    :           ',',     ?,      ? => matching, p := TRUE,  w
                                      argno++

    :             ?,     0,      ? => matching, p := FALSE, w

    :             ?,     ?,  FALSE => 

    :             k,    ch,      ? => UNLESS compch(k, ch)=0 DO
                                             matching := FALSE
  } REPEAT


FUN createco : fn, size =>
  LET c = getvec(size+7)
  LET h = 0
sawritef("createco: fn=%d size=%d\n", fn, size)
  IF c=0 RETURN 0
  FOR i = 7 TO size+7 DO c!i := 0

  // Using P to denote the current stack frame
  // pointer, the following assumptions are made:
  //  P!0, P!1, P!2 contain the return link information
  //  P!3   is the variable fn
  //  P!4   is the variable size
  //  P!5   is the variable c
  //  P!6   is the variable h

  // Now make the vector c into a valid BCPL
  // stack frame containg copies of fn, size
  // and c in the same relative positions.
  // Other locations in the new stack frame 
  // are used for other purposes.
  c!0 := c       // resumption point
  c!1 := currco  // parent link
  c!2 := colist  // colist chain
  c!3 := fn      // the main function
  c!4 := size    // the coroutine size in words
  c!5 := c       // the new coroutine pointer
  c!6 := h       // the h pointer for this coroutine

  colist := c  // insert into the list of coroutines

  changeco(0, c)
  // Execution now continues with the P pointer set to c,
  // and so  the vector c becomes the current stack frame.
  // The compiler will have generated code on
  // the assumption that fn and c are the third and fifth
  // words of the stack frame, and, since c!3 and c!5
  // were initialised to fn and c, the following repeated
  // statement will have the effect (naively) expected.
  // Note that the first call of cowait causes a return
  // from createco with RETURN c.

  c := fn(cowait c) REPEAT


FUN deleteco : cptr =>
  LET a = @colist
  { LET co = !a
    IF co=cptr OR co=0 BREAK
    a := @ co!Co_list
  } REPEAT
  IF !a=0 RETURN FALSE  // Coroutine not found.
  UNLESS cptr!1=0 DO abort 112
  !a := cptr!Co_list      // Remove the coroutine from colist.
  freevec cptr            // Free the coroutine stack.
  RETURN TRUE

FUN callco : cptr, a =>
  UNLESS cptr!Co_parent=0 DO abort 110
  cptr!Co_parent := currco
  RETURN changeco(a, cptr)

FUN cowait : a =>
  LET parent = currco!Co_parent
  currco!Co_parent := 0
  RETURN changeco(a, parent)

FUN resumeco : cptr, a =>
  LET parent = currco!Co_parent
  currco!Co_parent := 0
  UNLESS cptr!Co_parent=0 DO abort 111
  cptr!Co_parent := parent
  RETURN changeco(a, cptr)

FUN initco : fn, size, a, b, c, d, e, f, g, h, i, j, k =>
  LET cptr = createco(fn, size)
  UNLESS cptr=0 DO callco(cptr, @a)
  RETURN cptr


/**************************************************************
*MANIFEST { 
*sizebits = #xFFFFFFFE
*freebit  = 1
*}
*
*LET getvec : upb =>
*{  LET p, q=rootnode!Rtn_blklist
*   LET n = (upb+3) & sizebits      // round up to an even size
*
*   {  p := q
*      WHILE (!p & freebit) = 0 DO  // chain through used blocks
*       TEST !p=0 THEN RETURN 0   // end of block list
*                 ELSE p := p + !p
*      q := p  // find next used block
*      UNTIL (!q & freebit) = 0 DO q := q + !q - freebit
*   } REPEATUNTIL q-p>=n  // until a large enough block found
*
*   UNLESS p+n=q DO p!n := q-p-n+freebit
*   !p := n
*   RETURN p+1
*}
*
*AND freevec : p => p!-1 := p!-1 | freebit
*
*AND loadseg : file =>
*{  LET list  = 0
*   LET liste = @list
*   LET oldin = input()
*   LET newin = findinput file
*   
*   IF newin=0 RETURN 0
*   selectinput newin
*
*   {  LET type = rdhex()
*
*      SWITCHON type INTO
*      {  DEFAULT:
*          err:            unloadseg list
*                          list := 0
*         CASE 0:          BREAK
*             
*         CASE t_hunk:  {  LET n = rdhex()
*                          LET space = getvec n
*                          IF space=0 GOTO err
*                          space!0 := 0
*                          FOR i = 1 TO n DO space!i := rdhex()
*
*                          !liste := space
*                          liste := space
*                       }
*         CASE t_end:
*      }
*    } REPEAT
*    
*    endread()
*    selectinput(oldin)
*    RETURN list
*}
*
*AND rdhex : =>
*{  LET ch=rdch(), res=0
*
*   WHILE ch='\s' OR ch='\n' DO ch := rdch()
*   
*   {  TEST '0'<=ch<='9'
*      THEN res := res<<4 | ch-'0'
*      ELSE TEST 'A'<=ch<='F'
*           THEN res := res<<4 | ch-'A'+10
*           ELSE TEST 'a'<=ch<='z'
*                THEN res := res<<4 | ch-'a'+10
*                ELSE BREAK
*      ch := rdch()
*   } REPEAT
*   
*   RETURN res
*}
*   
*AND globin : segl =>
*{  LET a=segl, g=@globsize
*   UNTIL a=0 DO
*   {  LET base=(a+1)<<2, e
*      LET p = a+a!1
*      IF !p>globsize RETURN 0  // Global vector not large enough.
*      {  p := p-2
*         IF p!1=0 BREAK
*         g!(p!0) := base + p!1
*      } REPEAT
*      a := !a
*   }
*   RETURN segl
*}
*
*AND unloadseg : segl => UNTIL segl=0 DO {  LET s = !segl
*                                           freevec(segl)
*                                           segl := s
*                                        }
**************************************************************/


FUN getvec : upb => sys(21, upb)

FUN freevec : ptr => sys(22, ptr)

FUN loadseg : name => sys(23, name)

FUN globin : segl => sys(24, segl)

FUN unloadseg : segl => sys(25, segl)

FUN callseg : file, a1, a2, a3, a4 =>
  LET res = 0
  LET segl = loadseg(file)
  LET s = start
  UNLESS segl=0 OR globin(segl)=0 DO res := start(a1, a2, a3, a4)
  unloadseg(segl)
  start := s
  RETURN res

FUN deletefile : name => sys(17, name)

FUN renamefile : fromname, toname => sys(18, fromname, toname)

FUN mkobj :upb, fns => // object making function
  LET a = getvec(upb)
  UNLESS a=0 | fns=0 DO
  { !a := fns
    Initobj # a    // Send the init message to the object
  }
  RETURN a

