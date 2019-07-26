// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

/*

History

This is essentially the Tripos text editor designed for used
by terminatls such as a teletype. It has a brief description
in doc/edithelp.

15/4/2019
Systematically modified for the cintsys system. It has no yet
been fully debugged.

*/

SECTION "EDIT"

GET "libhdr"

MANIFEST
{ smax=80;  amax=80
  gmax=10;  cfmax=10
  fmin=5;   fmax=30
  lmax=120; pmax=20

  s_in=0; s_out=1

  // File offsets
  f_lk=0;   f_sp=1
  f_lc=2;   f_ex=3
  f_io=4;   f_fn=5

  // Command file offsets
  cf_el=0;  cf_sp=1
  cf_cp=2;  cf_cl=3
  cf_cb=4

  // Line offsets
  l_next=0; l_prev=1
  l_numb=2; l_cch=3
  l_len=4;  l_buf=4

  // Error codes
  err_uc=0;   err_udc=1
  err_bra=2;  err_cntx=3
  err_pm=4;   err_num=5
  err_line=6; err_fnx=7
  err_str=8;  err_nom=9
  err_rep=10; err_noin=11
  err_nopr=12;err_cr=13
  err_glob=14;err_ff=15
  err_cf=16;  err_ffa=17
  err_arg=18; err_opt=19
  err_rn=20;  err_gv=21
  err_cfv=22; err_qw=23
  err_brk=24

  // Repeatable double letter commands
  c_sa = ('S'<<8) | 'A'
  c_sb = ('S'<<8) | 'B'
  c_dl = ('D'<<8) | 'L'
  c_df = ('D'<<8) | 'F'
  c_dt = ('D'<<8) | 'T'
  c_pa = ('P'<<8) | 'A'
  c_pb = ('P'<<8) | 'B'
  c_nc = ('N'<<8) | 'C'
}

GLOBAL
{ tempname:250
  isinteractive:251
  openstreams:252
  closestreams:253
  rewind:254
  windup:255
  closeout:256
  closein:257
  newvec:258
  discardvec:259

  edit:260

  checkvalidchar:261
  checkspaceornl:262
  readcommline:263
  commrdch:264
  uncommrdch:265
  nextcomm:266
  readplusminus:267
  commreadn:268
  numarg:269
  readcontext:270
  abe_args:271
  dps_arg:272
  lf_arg:273
  readfiletitle:274
  addfilespec:275
  findfilespec:276
  losefilespec:277
  closefile:278
  changecom:279
  revertcom:280
  changeout:281
  changein:282

  renumber:283
  split:284
  concatenate:285
  insert:286
  readline:287
  writeline:288
  getline:289
  putline:290
  nextline:291
  prevline:292
  move:293
  ver:294
  verline:295

  error:296
  truncate:297
  expand:298
  compress:299
  condense:300
  incrementp:301
  subst:302
  index:303
  readglobal:304
  deleteglobal:305
  findglobal:306
  changeglobal:307


  e_to:320
  e_from:321
  e_work:322
  e_ver:323
  e_with:324
  e_workout:325
  e_workin:326
  e_backup:327
  currentoutput:328
  currentinput:329
  primaryoutput:330
  primaryinput:331
  textin:332
  textout:333
  edits:334
  verout:335
  cfstack:336
  cfsp:337
  maxlinel:338
  maxplines:339
  freelines:340
  oldestline:341
  currentline:342
  current:343
  pointer:344
  expanded:345
  condensed:346
  exhausted:347
  quiet:348
  deleting:349
  repeating:350
  unchanged:351
  nosubs:352
  ceiling:353
  linev:354
  linel:355
  commbuf:356
  commpoint:357
  commlinel:358
  comm:359
  delim:360
  cch:361
  sw_comm:362
  str_comm:363
  lf_comm:364
  str_match:365
  str_repl:366
  lf_match:267
  z_match:368
  globcount:369
  g_match:370
  g_repl:371
  verifying:372
  uppercase:373
  trailing:374
  filelist:375
  veclist:376
  opened:377
  zerolevel:378
  editlevel:379
  quitlevel:380
  editlab:381
  quitlab:382
  rc:383
}


LET start() BE
{ LET argv = VEC amax
  LET cvec = VEC cfmax
  LET gvec = VEC 2*gmax
  LET iovec = VEC 2*fmin
  LET oldoutput = output()
  LET oldinput = input()

  rc := 0
  opened := FALSE
  quitlevel := level()
  zerolevel := 0
  verout := oldoutput
  edits := oldinput
  commlinel := 0
  veclist := 0
  filelist := 0
  cfsp := 0

  IF rdargs("FROM/A,TO,WITH/K,VER/K,OPT/K",argv,amax)=0 DO
    error(err_arg)

  e_from := argv!0
  e_to := argv!1
  e_work := e_to
  e_with := argv!2
  e_ver := argv!3
  e_workin  := "TEMP1"      //tempname("T:EDIT-T00-WORK1")
  e_workout := "TEMP2"      //tempname("T:EDIT-T00-WORK2")
  e_backup  := "EDITBACKUP" //"T:EDIT-BACKUP"
  IF e_to=0 DO
  { e_to := e_from
    e_work := e_workin
    e_workin := e_workout
    e_workout := e_work
  }
  UNLESS e_ver=0 DO
  { LET s = findoutput(e_ver)
    IF s=0 DO error(err_ffa,e_ver)
    verout := s
  }
  UNLESS e_with=0 DO
  { LET s = findinput(e_with)
    IF s=0 DO error(err_ffa,e_with)
    edits := s
  }

  maxlinel := lmax
  maxplines := pmax
  UNLESS argv!4=0 DO
  { LET opts = argv!4
    LET i = 1
    LET rdn(opts, lvi) = VALOF
    { LET n = 0
      LET i = !lvi+1
      LET c = opts%i
      WHILE i<=opts%0 & '0'<=c<='9' DO
      { n := n*10+c-'0'
        i := i+1
        c := opts%i
      }
      !lvi := i-1
      RESULTIS n
    }

    WHILE i<=opts%0 DO
    { SWITCHON capitalch(opts%i) INTO
      { CASE 'W': maxlinel := rdn(opts, @i)
                  ENDCASE

        CASE 'P': maxplines := rdn(opts, @i)
                  ENDCASE

      }
      i := i+1
    }

    UNLESS maxlinel>0 & maxplines>0 DO
      error(err_opt)
  }

  freelines := newvec((1+l_buf+maxlinel)*(maxplines+2))
  freelines!l_next := 0
  FOR i = 1 TO maxplines+1 DO
  { LET l = freelines+(1+l_buf+maxlinel)*i
    LET n = freelines!l_next
    freelines!l_next := l
    l!l_next := n
  }

  commbuf := newvec(maxlinel/bytesperword)
  str_match := newvec(smax)
  str_repl := newvec(smax)
  lf_match := newvec(smax)
  z_match := newvec(smax)

  g_match := gvec
  g_repl := gvec+gmax
  cfstack := cvec
  primaryoutput := iovec
  primaryinput := iovec+fmin

  cfstack!0 := edits
  verifying := isinteractive(edits)
  selectoutput(verout)
  trailing, uppercase := FALSE, TRUE
  str_comm, lf_comm := c_nc, c_nc
  z_match!0, z_match!1 := 1, 'Z'

  openstreams()
  IF verifying DO writes("EDIT ready*N")
  edit(0)

quitlab:
  UNLESS verout=oldoutput DO closeout(verout)
  UNLESS edits=oldinput DO closein(edits)
  UNTIL filelist=0 DO losefilespec(filelist)
  UNTIL veclist=0 DO discardvec(veclist+1)
  stop(rc)
}


//AND tempname(string) = VALOF
// { LET n = string%0/bytesperword
//    LET s = newvec(n)
//    FOR i = 0 TO n DO s!i := string!i
//    s%9 := (taskid/10) REM 10 + '0'
//    s%10 := taskid REM 10 + '0'
//    RESULTIS s
// }


AND isinteractive(s) = s!scb_type<=0


AND openstreams() BE
{ textin := findinput(e_from)
  IF textin=0 DO error(err_ffa,e_from)
  textout := findoutput(e_work)
  IF textout=0 DO
  { closein(textin)
    error(err_ffa,e_work)
  }
  primaryoutput!f_sp := textout
  primaryinput!f_sp := textin
  primaryinput!f_lc := 0
  primaryinput!f_ex := FALSE
  currentoutput := primaryoutput
  currentinput := primaryinput
  currentline := freelines
  freelines := currentline!l_next
  oldestline := currentline
  currentline!l_next := 0
  currentline!l_prev := 0
  linev := currentline+l_buf
  expanded :=  FALSE
  linel, pointer := 0, 0
  cch := endstreamch
  current, exhausted := 0, FALSE
  unchanged, nosubs := TRUE, TRUE
  globcount := 0
  ceiling := maxint
  opened := TRUE
}


AND closestreams() BE
{ opened := FALSE
  UNTIL oldestline=0 DO writeline()
  UNLESS currentoutput=primaryoutput DO
    losefilespec(currentoutput)
  UNLESS currentinput=primaryinput DO
    losefilespec(currentinput)
  closeout(primaryoutput!f_sp)
  closein(primaryinput!f_sp)
}


AND rewind() BE
{ e_from := e_work
  e_work := e_workin
  e_workin := e_workout
  e_workout := e_work
}


AND windup() BE UNLESS e_work=e_to DO
{ renamefile(e_to,e_backup)
  IF renamefile(e_work,e_to)=0 DO
    error(err_rn,e_work,e_to)
  deletefile(e_workin)
}


AND closeout(s) BE UNLESS s=0 DO
{ LET o = output()
  selectoutput(s)
  endwrite()
  UNLESS o=s DO selectoutput(o)
}


AND closein(s) BE UNLESS s=0 DO
{ LET i = input()
  selectinput(s)
  endread()
  UNLESS i=s DO selectinput(i)
}


AND newvec(n) = VALOF
{ LET v = getvec(n+1)
  IF v=0 DO error(err_gv)
  !v := veclist
  veclist := v
  RESULTIS v+1
}


AND discardvec(v) BE
{ LET p = @veclist
  UNTIL !p=0 DO
  { LET t = !p
    IF t=v-1 DO
    { !p := !t
      freevec(t)
      BREAK
    }
    p := t
  }
}


LET edit(n) BE
{ LET counting = FALSE
  LET count, countp = 0, 0

editlab:
  IF n=0 DO
  { editlevel := level()
    readcommline()
  }
  counting := FALSE

  // repeat loop to get commands
  { LET e, s, c, p, q = 0, 0, 0, 0, 0

    IF intflag() DO error(err_brk)
//    IF testflags(1) DO error(err_brk)
    nextcomm()
    quiet := NOT verifying
    deleting, repeating := FALSE, FALSE

 sw:sw_comm := comm
    SWITCHON comm INTO

    { DEFAULT:
        error(err_uc, comm)

      CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
      CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
        // Read a repeat count
         count := commreadn()
         IF count=0 & zerolevel=0 DO
           zerolevel := editlevel
         countp := commpoint
         counting := TRUE
         LOOP

      CASE '*S':
         LOOP

      CASE '[':CASE '(':  // Nested commands can be enclosed in brackets
         edit(n+1)
         ENDCASE

      CASE ']':CASE ')':
         IF n<=0 DO error(err_bra)
         RETURN

      CASE endstreamch:
         UNLESS cfsp=0 DO
         { revertcom()
           RETURN
         }
      CASE 'W':    // W   Windup (normal finish to edit session)
      CASE 'Q':    // Q   Abandon whole edit session
         nextcomm()
         UNLESS comm='*N' | comm=endstreamch DO
           error(err_qw,sw_comm)
         UNLESS sw_comm='Q' DO move(maxint)
         closestreams()
         UNLESS sw_comm='Q' DO windup()
         UNTIL cfsp=0 DO revertcom()
         longjump(quitlevel, quitlab)

      CASE '**':   // Rewind to first line
         move(maxint)
         closestreams()
         rewind()
         openstreams()
         ENDCASE

      CASE '|':   // ignore rest of command line
      CASE '\':
         UNTIL comm='*N' DO commrdch()
      CASE '*N':
nl:      UNLESS n=0 DO error(err_bra)
         IF isinteractive(edits) DO
           TEST quiet | unchanged
           THEN newline()
           ELSE ver(sw_comm, '*E')
         GOTO editlab

      CASE '?':   // verify current line
      CASE '!':   // verify indicating case or hex value
         nextcomm()
         IF comm='*N' & isinteractive(edits) DO
         { quiet, unchanged := FALSE, FALSE
           GOTO nl
         }
         uncommrdch()
         ver(sw_comm, '*N')
         ENDCASE

      CASE '>':   // Step character pointer
         incrementp()
         ENDCASE

      CASE '<':   // Reset character pointer
         condense()
         UNLESS pointer=0 DO unchanged := FALSE
         pointer := 0
         ENDCASE

      CASE ':':   // Move character pointer to end of line
         UNLESS pointer=linel DO unchanged := FALSE
         pointer := linel
         ENDCASE

      CASE '#':   // Delete character
         IF incrementp() DO
         { linev!pointer := -1
           condensed := FALSE
           nosubs := FALSE
         }
         ENDCASE

      CASE '_':   // Convert character to space
         IF incrementp() DO
         { linev!pointer := '*S'
           nosubs := FALSE
         }
         ENDCASE

      CASE '%':   // Force character to upper case
         IF incrementp() DO
         { LET a = linev+pointer
           LET value = !a
           IF 'a'<=value<='z' DO
           { !a := value+'A'-'a'
             nosubs := FALSE
           }
         }
         ENDCASE

      CASE '$':   // Force character to lower case
         IF incrementp() DO
         { LET a = linev+pointer
           LET value = !a
           IF 'A'<=value<='Z' DO
           { !a := value-'A'+'a'
             nosubs := FALSE
           }
         }
         ENDCASE

      CASE 'U':   // U+/-   Set/Unset uppercasing before string matching
         uppercase := readplusminus()
         ENDCASE

      CASE 'V':   // V+/-   Turn Verification on/off
         verifying := readplusminus()
         ENDCASE

      CASE 'Z':   // Z/s/   Set terminator for I & R (default Z)
      { LET n = z_match!0
        delim :=commrdch()
        readcontext(z_match)
        IF z_match!0=0 DO
        { z_match!0 := n
          error(err_cntx, 'Z')
        }
        ENDCASE
      }

      CASE '=':   // =n     Renumber current line n
        renumber(numarg(FALSE,FALSE))
        ENDCASE

      CASE '-':   // -n     Delete n lines
        deleting := TRUE
      CASE '+':   // +n     Move forward by n lines
        e := numarg(TRUE,FALSE)
        FOR i = 1 TO e DO nextline()
        ENDCASE

      CASE 'N':   // N      Move to next line
        nextline()
        ENDCASE

      CASE 'M':   // Mn     Move to line n
        move(numarg(FALSE,FALSE))
        ENDCASE

      CASE 'I':   // Insert before the current line
        quiet := TRUE
        move(numarg(FALSE,FALSE))
        insert()
        ENDCASE

      CASE 'T':   // Tn     Type n lines
                  // TLn    Type n lines with Line numbers
                  // TP     Type buffer of Previous lines
                  // TN     Type Next <P> lines (option P)
                  // TR+/-  Set/Unset stripping of trailing spaces
        commrdch()
        c := comm
        SWITCHON comm INTO
        { CASE 'R':   // TR+/-  Set/Unset stripping of trailing spaces
            trailing := readplusminus()
            ENDCASE

          CASE 'P':   // TP     Type buffer of Previous lines
            UNTIL currentline!l_prev=0 DO
              prevline()
          CASE 'N':   // TN     Type Next <P> lines (option P)
            e := maxplines
            GOTO tlab

          DEFAULT:   // Tn      Type n lines
            checkvalidchar()
          CASE 'L':  // TLn     Type n lines with Line numbers
            e :=  numarg(TRUE,FALSE)
tlab:       quiet := TRUE
            FOR i = 1 TO e DO
            { UNLESS linel=0 & (current=0 | exhausted) DO
              { IF c='L' DO
                TEST current=-1
                THEN writes("  +++  ")
                ELSE writef("%I5  ", current)
                verline('?')
              }
              IF exhausted BREAK
              nextline()
            }
            unchanged := FALSE
            ENDCASE
        }
        ENDCASE

      CASE 'H':   // Halt at line n
        ceiling := numarg(FALSE,FALSE)
        ENDCASE

      CASE 'O':   // O  file    Send Output to file
        changeout()
        ENDCASE

      CASE 'C':   // C file     Obey commands from file
                  // CC/x/      Set Carriage Control to *x (x = N,C,E, or P)
                  // CC/?/      Print Carriage Control for current line
                  // CF  file   Close File
                  // CL         Concatenate next Line to current one
        commrdch()
        SWITCHON comm INTO
        { DEFAULT:   // C  file Obey Commands from file
            checkspaceornl()
            changecom()
            edit(0)
            ENDCASE

          CASE 'C':   // CC/x/   Set Carriage Control to *x (x = N,C,E, or P)
                      // CC/?/   Print Carriage Control for current line
            delim := commrdch()
            commrdch()
            TEST comm='?'
            THEN { s := cch='*C' -> "**C",
                        cch='*E' -> "**E",
                        cch='*N' -> "**N",
                        cch='*P' -> "**P", "?"
                   writes(s)
                   newline()
                 }
            ELSE { cch := comm='C' -> '*C',
                          comm='E' -> '*E',
                          comm='N' -> '*N',
                          comm='P' -> '*P', endstreamch
                 }
            UNTIL comm=delim | comm='*N' DO
              commrdch()
              ENDCASE


          CASE 'L':   // CL       Concatenate next Line to current one
            compress()
            concatenate()
            ENDCASE

          CASE 'F':   // CF file  Close File
            closefile()
            ENDCASE
        }
        ENDCASE

      CASE 'S':   // S file       Read Source from file
                  // SA/s/        Split current line After s
                  // SB/s/        Split current line Before s
        commrdch()
ssw:    SWITCHON comm INTO
        { DEFAULT:   // S file    Read Source from file
            checkspaceornl()
            changein()
            ENDCASE

          CASE 'A':    // SA/s/   Split current line After s
          CASE 'B':    // SB/s/   Split current line Before s
            c := comm='A' -> c_sa, c_sb
            dps_arg(c)
            compress()
            e := index(linev, pointer,
                       linel, str_match)
            IF e<0 DO error(err_nom)
            IF c=c_sa DO e := e+str_match!0
            split(e)
            ENDCASE
        }
        ENDCASE

      CASE 'P':   // P             Move to Previous line
                  // PA/s/         Point After s in current line
                  // PB/s/         Point Before s in current line
        commrdch()
psw:    SWITCHON comm INTO
        { DEFAULT:   // P          Move to Previous line
            uncommrdch()
            prevline()
            ENDCASE

          CASE 'A':   // PA/s/     Point After s in current line
          CASE 'B':   // PB/s/     Point Before s in current line
            c := comm='A' -> c_pa, c_pb
            dps_arg(c)
            compress()
            e := index(linev, pointer,
                       linel, str_match)
            IF e<0 DO error(err_nom)
            pointer := c=c_pa -> e+str_match!0, e
            nosubs := FALSE
            ENDCASE
        }
        ENDCASE

      CASE 'A':    // A/s/t/     Insert after
      CASE 'B':    // B/s/t/     Insert before
      CASE 'E':    // E/s/t/     Exchange s by t
        abe_args(comm)
        compress()
        p := index(linev, pointer,
                   linel, str_match)
        IF p<0 DO error(err_nom)
        q := p+str_match!0
        IF str_comm='B' DO q := p
        IF str_comm='A' DO p := q
        subst(p, q, str_repl)
        ENDCASE

      CASE 'L':   // L/s/        Locate line containing s
      CASE 'F':   // F/s/        Find line beginning with s
        lf_arg(comm)
        compress()
        p := lf_match!0

        { IF sw_comm='L' DO p := linel
          UNLESS p>linel DO
            IF index(linev, pointer, p, lf_match) >=0 BREAK
          nextline()
        } REPEAT

        ENDCASE

      CASE 'G':   // G/s/t/      Globally exchange s by t
        readglobal()
        ENDCASE

      CASE 'D':   // Dn  (m)     Delete line(s) n (to m)
                  // DF/s/       Delete up to line starting with s
                  // DL/s/       Delete up to line containing s
                  // DT/s/       Delete characters To s in current line
                  // DG/s/       Delete Global exchange for s
                  // DG//        Delete all Global exchanges
        commrdch()
dsw:    SWITCHON comm INTO
        { CASE 'F': // DF/s/     Delete up to line starting with s
          CASE 'L': // DL/s/     Delete up to line containing s
            c := comm='F' -> c_df, c_dl
            UNLESS repeating DO
              str_comm, lf_comm := c, c
            deleting, quiet := TRUE, TRUE
            GOTO sw

          CASE 'T':  // DT/s/    Delete characters To s in current line
            dps_arg(c_dt)
            compress()
            e := index(linev, pointer,
                       linel, str_match)
            IF e<0 DO error(err_nom)
            UNLESS e=pointer DO
            { FOR i = 1 TO linel-e DO
                linev!(pointer+i) := linev!(e+i)
              linel := pointer+linel-e
              nosubs := FALSE
            }
            ENDCASE

          CASE 'G':  // DG/s/    Delete Global exchange for s
                     // DG//     Delete all Global exchanges
            deleteglobal()
            ENDCASE

          DEFAULT:
            checkvalidchar()
            GOTO drlab
        }
        ENDCASE

drlab:
      CASE 'R':   // Rn (m) (file) Replace line(s) n (to m) (by file)
          { LET a1 = numarg(FALSE,FALSE)
            LET a2 = numarg(FALSE,TRUE,a1)
            IF sw_comm='R' DO quiet := TRUE
            move(a1)
            deleting, quiet := TRUE, TRUE
            move(a2)
            TEST exhausted THEN { linel, pointer := 0, 0
                                  unchanged := FALSE
                                  cch := endstreamch
                                }
                           ELSE nextline()
            IF sw_comm='R' DO insert()
            ENDCASE
          }

      CASE '"':   // Equivalent to N'
        nextline()
      CASE '*'':  // Repeat last explicit A,B,E,DL,DF,DT,PA,PB,
                  // SA,SB,L or F command
        repeating := TRUE
        comm := str_comm
        GOTO sw

      CASE '&':   // Repeat last L,F,DL or DF command
        repeating := TRUE
        comm := lf_comm
        GOTO sw

      // Repeated double letter commands
      CASE c_df:   // DF/s/      Delete up to line starting with s
      CASE c_dl:   // DL/s/      Delete up to line containing s
      CASE c_dt:   // DT/s/      Delete characters To s in current line
        comm := comm&255
        GOTO dsw

      CASE c_pa:   // PA/s/      Point After s in current line
      CASE c_pb:   // PB/s/      Point Before s in current line
        comm := comm&255
        GOTO psw

      CASE c_sa:   // SA/s/      Split current line After s
      CASE c_sb:   // SB/s/      Split current line Before s
        comm := comm&255
        GOTO ssw

      CASE c_nc:
        error(err_rep)
    }

    UNLESS nosubs DO unchanged := FALSE

    IF counting DO
    { UNLESS count=0 DO
      { count := count-1
        IF count=0 DO
        { counting := FALSE
          LOOP
        }
      }
      commpoint := countp
    }
  } REPEAT
}


LET checkvalidchar() BE
  TEST comm='*S' | comm='*N' |
       comm='**' | comm='.'  | '0'<=comm<='9'
  THEN uncommrdch()
  ELSE error(err_udc, sw_comm, comm)


AND checkspaceornl() BE
  TEST comm='*S' | comm='*N'
  THEN uncommrdch()
  ELSE error(err_udc, sw_comm, comm)


AND readcommline() BE
{ commlinel := 0
  selectinput(edits)

  { LET ch = rdch()
    IF ch='*E' | ch='*N' | ch='*C' | ch='*P' BREAK
    IF ch=endstreamch DO
    { IF commlinel=0 DO commlinel := -1
      BREAK
    }
    commlinel := commlinel+1
    UNLESS commlinel>maxlinel DO
      commbuf%commlinel := ch
  } REPEAT

  IF commlinel>maxlinel DO
  { commlinel := maxlinel
    writes("****** Command line truncated*N")
    rc := 10
  }
  commpoint := 0
}


AND commrdch() = VALOF
{ commpoint := commpoint+1
  comm := commlinel=-1        -> endstreamch,
          commpoint>commlinel ->        '*N',
          capitalch(commbuf%commpoint)
  RESULTIS comm
}


AND uncommrdch() BE
  commpoint := commpoint-1


AND nextcomm() BE
  commrdch() REPEATWHILE comm='*S'


AND readplusminus() = VALOF
{ commrdch()
  IF comm='+' RESULTIS TRUE
  IF comm='-' RESULTIS FALSE
  error(err_pm, sw_comm)
}


AND commreadn() = VALOF
{ LET a = 0
  { a := a*10+comm-'0'
   commrdch()
    } REPEATWHILE '0'<=comm<='9'
    uncommrdch()
    RESULTIS a
 }


// Read a number argument
// '*' => end of document
// '.' =>  -> 1, CURRENT
AND numarg(add, opt, def) = VALOF
{ nextcomm()
  IF comm = '.' RESULTIS add -> 1, current
  IF comm = '**' RESULTIS maxint
  IF '0'<=comm<='9' RESULTIS commreadn()
  IF opt DO { uncommrdch(); RESULTIS def }
  error(err_num, sw_comm)
}


// Read a context string argument
AND readcontext(v) BE
{ LET i = 0

  { commrdch()
    IF comm=delim | comm='*N' BREAK
    IF i>=smax DO error(err_str)
    i := i+1
    v!i := commbuf%commpoint
  } REPEAT
  v!0 := i
}


AND abe_args(c) BE  UNLESS repeating DO
{ dps_arg(c)
  readcontext(str_repl)
}


AND dps_arg(c) BE UNLESS repeating DO
{ str_comm := c
  delim := commrdch()
  readcontext(str_match)
}


AND lf_arg(c) BE  UNLESS repeating DO
{ UNLESS deleting DO
  str_comm, lf_comm := c, c
  delim := commrdch()
  readcontext(lf_match)
}


// Read a file title argument
AND readfiletitle(v) = VALOF
{ LET i = 0
  nextcomm()
  UNTIL comm='*S' | comm='*N' DO
  { IF i>=fmax*bytesperword DO error(err_str)
    i := i+1
    v%i := commbuf%commpoint
    commrdch()
  }
  v%0 := i
  RESULTIS i
}


// Add a file spec to the file list
AND addfilespec(v, type) = VALOF
{ LET p = newvec(fmin+fmax)
  LET s = type=s_in -> findinput(v), findoutput(v)
  IF s=0 DO error(err_ff, v)
  !p := filelist
  filelist := p
  FOR i = 0 TO v%0 DO (p+f_fn)%i := v%i
  p!f_lc := 0
  p!f_ex := FALSE
  p!f_io := type
  p!f_sp := s
  RESULTIS p
}


// Find a file spec in the file list
AND findfilespec(v, type) = VALOF
{ LET p = @filelist
  UNTIL !p=0 DO
  { LET t = !p
    IF compstring(t+f_fn, v)=0 & type=t!f_io RESULTIS t
    p := t
  }
  RESULTIS 0
}


// Close a file and remove it from the list
AND losefilespec(pf) BE
{ LET p = @filelist
  UNTIL !p=0 DO
  { LET t = !p
    TEST t = pf THEN
    { LET close = t!f_io=s_in -> closein, closeout
      close(t!f_sp)
      !p := !t
      discardvec(t)
      BREAK
    }
    ELSE p := t
  }
}


AND closefile() BE
{ LET v = VEC fmax
  LET e = readfiletitle(v)
  IF e=0 DO error(err_fnx)
  e := findfilespec(v, s_out)
  UNLESS e=0 DO
  { IF e=currentoutput DO
    { UNTIL oldestline=currentline DO writeline()
      currentoutput := primaryoutput
      textout := currentoutput!f_sp
    }
    losefilespec(e)
    RETURN
  }
  e := findfilespec(v, s_in)
  UNLESS e=0 DO
  { IF e=currentinput DO
    { renumber(-1)
      currentinput := primaryinput
      current := currentinput!f_lc
      exhausted := currentinput!f_ex
      textin := currentinput!f_sp
    }
    losefilespec(e)
    RETURN
  }
  error(err_cf, v)
}


// Change the command input stream
// stack the current command line and its pointers
AND changecom() BE
{ LET v = VEC fmax
  LET e = readfiletitle(v)
  LET f = 0
  LET s = 0
  IF e=0 DO error(err_fnx)
  IF cfsp>cfmax DO error(err_cfv)
  e := findinput(v)
  IF e=0 DO error(err_ff, v)
  s := commlinel/bytesperword
  f := newvec(cf_cb+s)
  f!cf_cp := commpoint
  f!cf_cl := commlinel
  f!cf_sp := edits
  f!cf_el := editlevel
  FOR i = 0 TO s DO
    (f+cf_cb)!i := commbuf!i
  cfstack!cfsp := f
  cfsp := cfsp+1
  edits := e
}


// Revert to the previous command stream
AND revertcom() BE
{ LET f = 0
  closein(edits)
  cfsp := cfsp-1
  f := cfstack!cfsp
  commpoint := f!cf_cp
  commlinel := f!cf_cl
  edits := f!cf_sp
  editlevel := f!cf_el
  FOR i = 0 TO commlinel/bytesperword DO
    commbuf!i := (f+cf_cb)!i
  discardvec(f)
}


// Change the current output stream
// Read file name and look it up
// if not found then open it
AND changeout() BE
{ LET v = VEC fmax
  LET e = readfiletitle(v)
  TEST e=0 | compstring(v, "#")=0
  THEN { e := primaryoutput
       }
  ELSE { e := findfilespec(v, s_out)
         IF e=0 DO e := addfilespec(v, s_out)
       }
  UNTIL oldestline=currentline DO writeline()
  currentoutput := e
  textout := currentoutput!f_sp
}


// Change the current input stream
AND changein() BE
{ LET v = VEC fmax
  LET e = readfiletitle(v)
  TEST e=0 | compstring(v, "#")=0
  THEN e := primaryinput
  ELSE { e := findfilespec(v, s_in)
         IF e=0 DO e := addfilespec(v, s_in)
       }
  renumber(-1)
  currentinput := e
  textin := e!f_sp
  IF currentline!l_next=0 DO exhausted := e!f_ex
}


// Renumber all lines in store
LET renumber(n) BE
{ LET l = currentline
  current := n
  UNTIL l=0 DO
  { l!l_numb := n
    UNLESS n=-1 DO n := n+1
    l := l!l_next
  }
  UNLESS n=-1 DO currentinput!f_lc := n-1
  l := currentline!l_prev
  UNTIL l=0 DO { l!l_numb := -1; l := l!l_prev }
}


// Split the current line
AND split(p) BE
{ LET l = freelines
  freelines := l!l_next
  l!l_prev := currentline
  l!l_next := currentline!l_next
  UNLESS currentline!l_next=0 DO
    currentline!l_next!l_prev := l
  currentline!l_next := l
  nosubs := FALSE
  l!l_len := linel-p
  l!l_numb := current
  l!l_cch := cch
  FOR i = p+1 TO linel DO (l+l_buf)!(i-p) := linev!i
  cch := '*N'
  linel := p
  exhausted := FALSE
  putline()
  currentline := l
  getline()
  IF currentline!l_next=0 DO
    exhausted := currentinput!f_ex
  current := -1
  nosubs := FALSE
  IF freelines=0 DO writeline()
}


// Concatenate the next line
AND concatenate() BE
{ LET l = 0
  LET s = linel
  LET p = pointer
  nosubs := TRUE
  nextline()
  putline()
  l := currentline
  currentline := currentline!l_prev
  getline()
  FOR i = linel+1 TO s DO linev!i := '*S'
  linel := s
  subst(linel, linel, l+l_buf)
  pointer := p
  currentline!l_next := l!l_next
  UNLESS l!l_next=0 DO
    l!l_next!l_prev := currentline
  l!l_next := freelines
  freelines := l
}


// Insert material before the current line
AND insert() BE
{ LET v = VEC fmax
  LET e = readfiletitle(v)
  LET i = 0
  LET l = currentline
  LET p = pointer
  LET s = nosubs
  TEST e=0 THEN { UNTIL comm='*N' DO commrdch()
                  selectinput(edits)
                }
           ELSE { i := findinput(v)
                  IF i=0 DO error(err_ff, v)
                  selectinput(i)
                }
  nosubs := TRUE
  putline()
  current := -1

  { currentline := freelines
    readline()
    IF i=0 & linel=z_match!0 &
       index(linev, 0, linel, z_match)=0 BREAK
    IF linel=0 & cch=endstreamch BREAK
      freelines := currentline!l_next
    currentline!l_next := l
    currentline!l_prev := l!l_prev
    UNLESS l!l_prev=0 DO
      l!l_prev!l_next := currentline
    l!l_prev := currentline
    IF oldestline=l DO oldestline := currentline
    putline()
    IF freelines=0 DO writeline()
//    IF testflags(1) DO
    IF intflag() DO { UNLESS i=0 DO endread()
                      currentline := l
                      getline()
                      error(err_brk)
                    }
  } REPEAT

  UNLESS i=0 DO endread()
  currentline := l
  getline()
  nosubs := s
  pointer := p
}


// read an input line
AND readline() BE
{ linev := currentline+l_buf
  linel := 0

  { cch := rdch()
    IF cch<'*S' DO
      IF cch='*E' | cch='*N' |
         cch='*C' | cch='*P' BREAK
    IF cch=endstreamch BREAK
    linel := linel+1
    UNLESS linel>maxlinel DO linev!linel := cch
  } REPEAT

  IF truncate(linel) DO linel := maxlinel
  UNLESS trailing DO
    WHILE linel>pointer & linev!linel='*S' DO linel := linel-1
  nosubs := TRUE
  expanded := FALSE
}


// Write an output line
AND writeline() BE
{ LET l = oldestline
  LET v = oldestline+l_buf
  IF l=currentline DO putline()
  selectoutput(textout)
  FOR p = 1 TO v!0 DO wrch(v!p)
  UNLESS l!l_cch=endstreamch DO wrch(l!l_cch)
  selectoutput(verout)
  oldestline := l!l_next
  UNLESS oldestline=0 DO oldestline!l_prev := 0
  l!l_next := freelines
  freelines := l
}


// Set up a new current line
AND getline() BE
{ linev := currentline+l_buf
  linel := currentline!l_len
  cch := currentline!l_cch
  current := currentline!l_numb
  nosubs := TRUE
  expanded := FALSE
}


// Store the current line
AND putline() BE
{ pointer := 0
  UNLESS quiet | nosubs DO ver('?', '*N')
  compress()
  UNLESS trailing DO
    WHILE linel>0 & linev!linel='*S' DO
      linel := linel-1
  currentline!l_cch := cch
  currentline!l_len := linel
  currentline!l_numb := current
}


// Move on to the next line
AND nextline() BE
// { IF testflags(1) DO error(err_brk)
{ IF intflag() DO error(err_brk)
  IF current>0 & current>=ceiling DO error(err_cr)
  IF exhausted DO error(err_noin)
  pointer := 0
  UNLESS deleting DO putline()
  TEST currentline!l_next=0
  THEN { UNLESS deleting DO
         { freelines!l_prev := currentline
           currentline!l_next := freelines
           currentline := freelines
           freelines := freelines!l_next
           currentline!l_next := 0
           IF freelines=0 DO writeline()
         }
         current := currentinput!f_lc+1
         selectinput(textin)
         readline()
         FOR i = 1 TO globcount DO changeglobal(i)
         exhausted := cch=endstreamch
         currentinput!f_lc := current
         currentinput!f_ex := exhausted
       }
  ELSE { currentline := currentline!l_next
         getline()
         IF currentline!l_next=0 DO
           exhausted := currentinput!f_ex
         IF deleting DO
         { LET p = currentline!l_prev
           currentline!l_prev := p!l_prev
           UNLESS p!l_prev=0 DO
             p!l_prev!l_next := currentline
           p!l_next := freelines
           freelines := p
           IF oldestline=p DO oldestline := currentline
         }
       }
  IF exhausted & zerolevel\=0 DO error(err_noin)
  unchanged := FALSE
}


// Move back to the previous line
AND prevline() BE
{ IF currentline!l_prev=0 DO error(err_nopr)
  putline()
  currentline := currentline!l_prev
  getline()
  exhausted := FALSE
  unchanged := FALSE
}


// Move on to line N
AND move(n) BE UNLESS n=current DO
{ UNLESS deleting DO
  { LET l = currentline!l_prev
    UNTIL l=0 DO
    { LET m = l!l_numb
      UNLESS m=-1 DO
      { IF m=n DO
        { putline()
          currentline := l
          getline()
          exhausted := FALSE
          unchanged := FALSE
          RETURN
        }
        IF m<n BREAK
      }
      l := l!l_prev
    }
  }
  UNTIL n=current DO
  { IF current>0 & current>=n DO
      error(err_line, n)
    IF exhausted & n=maxint DO
    { IF deleting DO linel := 0
      BREAK
    }
    nextline()
  }
}


// Verify the current line
AND ver(c, n) BE
{ TEST current=-1
  THEN writes("+++")
  ELSE writen(current)
  wrch(exhausted -> '**', '.')
  newline()
  UNLESS linel=0 & (current=0 | exhausted) DO
  { verline(c)
    UNLESS pointer=0 DO
    { FOR i = 1 TO pointer-1 DO wrch('*S')
      wrch('>')
      wrch(n)
    }
  }
  unchanged, nosubs := TRUE, TRUE
}


// Write out a verification line
AND verline(c) BE
{ LET vch1(ch) =
         #40<=ch< #177 -> ch,
              ch ='*T' -> '*S',
              ch<    0 -> '*S', '?'

  AND vch2(ch) =
         #40<=ch< #177 -> ch,
              ch ='*T' -> '*S',
              ch<    0 -> '*S', hex(ch>>4)

  AND vch3(ch) =
         #40<=ch< #100 -> '*S',
        #100<=ch< #140 -> '-',
        #140<=ch< #177 -> '*S',
              ch ='*T' -> 'T',
              ch<    0 -> '.', hex(ch&15)

  AND hex(ch) =
        ch>9 -> 'A'+ch-10, '0'+ch

  AND wrl(vch) BE
  { LET l = linel
    WHILE l>0 & vch(linev!l)='*S' DO l := l-1
    FOR p = 1 TO l DO wrch(vch(linev!p))
      newline()
  }

  expand()
  condense()
  TEST c='!' THEN { wrl(vch2)
                    wrl(vch3)
                  }
             ELSE { wrl(vch1)
                  }
}


LET error(n, a, b) BE
{ LET r = 10
  LET z = zerolevel
  LET s = VALOF SWITCHON n INTO
  { CASE err_uc:   RESULTIS "Unknown command - %C"
    CASE err_udc:  RESULTIS "Unknown command - %C%C"
    CASE err_bra:  RESULTIS "Unmatched parenthesis"
    CASE err_cntx: RESULTIS "Null context after %C"
    CASE err_pm:   RESULTIS "+ or - expected after %C"
    CASE err_num:  RESULTIS "Number expected after %C"
    CASE err_line: RESULTIS "Line number %N too small"
    CASE err_fnx:  RESULTIS "Filename expected"
    CASE err_str:  RESULTIS "String too long"
    CASE err_nom:  RESULTIS "No match"
    CASE err_rep:  RESULTIS "Nothing to repeat"
    CASE err_noin: RESULTIS "Input exhausted"
    CASE err_nopr: RESULTIS "No more previous lines"
    CASE err_cr:   RESULTIS "Ceiling reached"
    CASE err_glob: RESULTIS "Too many globals"
    CASE err_ffa:  r := 20
    CASE err_ff:   RESULTIS "Can't open %S"
    CASE err_cf:   RESULTIS "Can't close %S"
    CASE err_arg:  r := 20
                   RESULTIS "Bad args"
    CASE err_opt:  r := 20
                   RESULTIS "Invalid option values"
    CASE err_rn:   r := 20
                   RESULTIS "Can't rename %S as %S"
    CASE err_gv:   r := 20
                   RESULTIS "Run out of store"
    CASE err_cfv:  r := 20
                   RESULTIS "Command file stack ovf"
    CASE err_qw:   RESULTIS "Invalid %C command"
    CASE err_brk:  RESULTIS "****BREAK"
  }

  zerolevel := 0

  { IF editlevel=z & n=err_noin DO
    { IF isinteractive(edits) UNLESS verifying DO newline()
      GOTO l
    }
    IF cfsp=0 BREAK
    revertcom()
  } REPEAT

  UNLESS commlinel<=0 | isinteractive(edits) DO
  { FOR i = 1 TO commlinel DO wrch(commbuf%i)
    UNLESS commbuf%commlinel='*N' DO newline()
    FOR i = 1 TO commpoint-1 DO wrch('*S')
    writes("!*N")
  }
  writef(s, a, b)
  newline()
  rc := r
  IF rc=20 | NOT isinteractive(edits) DO
  { UNLESS rc=20 DO ver('?', '*N')
    IF opened DO closestreams()
    longjump(quitlevel, quitlab)
  }
l:IF verifying DO ver('?', '*E')
  longjump(editlevel, editlab)
}


AND truncate(p) = VALOF
{ UNLESS p>maxlinel RESULTIS FALSE
  TEST current=-1
  THEN writes("****** Line +++ truncated*N")
  ELSE writef("****** Line %I3 truncated*N", current)
  rc := 10
  RESULTIS TRUE
}


// Expand tabs in the current line with dummy characters
AND expand() BE UNLESS expanded DO
{ LET j = 0
  LET t = maxlinel-linel
  LET p = t+pointer
  LET c, f = 0, FALSE
  FOR i = linel TO 1 BY -1 DO linev!(t+i) := linev!i
  UNTIL t>=maxlinel DO
  { IF j+(c='*T' -> 1,0) > t DO
    { t := t+1
      FOR i = linel TO t BY -1 DO linev!i := linev!(i-1)
      f := TRUE
      LOOP
    }
    j := j+1
    TEST c='*T' THEN linev!j := -1
                ELSE { t := t+1
                       c := linev!t
                       linev!j := c
                     }
    IF j REM 8 = 0 DO c := 0
    IF t=p DO pointer := j
  }
  IF f DO truncate(maxint)
  linel := j
  expanded, condensed := TRUE, TRUE
}


// Remove all dummy characters from the current line
AND compress() BE IF expanded DO
{ LET i, j = 0, 0
  UNTIL i>=linel DO
  { i := i+1
    UNLESS linev!i<0 DO { j := j+1
                          linev!j := linev!i
                        }
    IF pointer=i DO pointer := j
  }
  linel := j
  expanded := FALSE
}


// Remove all dummy characters from the current line
// leaving tabs expanded
AND condense() BE IF expanded DO UNLESS condensed DO
{ LET i, j = 0, 0
  UNTIL i>=linel DO
  { i := i+1
    IF pointer=i DO pointer := j+(linev!i<0 -> 0,1)
    UNLESS linev!i<0 DO
    { j := j+1
      linev!j := linev!i
      IF linev!i='*T' DO
             UNTIL j REM 8 = 0 DO { j := j+1
                                     linev!j := -1
                                  }
    }
  }
  linel := j
  condensed := TRUE
}


// Step the character pointer
AND incrementp() = VALOF
{ expand()
  IF pointer=lmax RESULTIS FALSE
  pointer := pointer+1
  unchanged := FALSE
  IF pointer>linel DO { linev!pointer := '*S'
                        linel := pointer
                      }
  RESULTIS TRUE
}


// Substitute a string for line positions P+1 to Q
AND subst(p, q, v) BE
{ LET s = v!0
  LET t = linel-q
  LET r = 0
  truncate(p+s+t)
  IF p+s>maxlinel DO s := maxlinel-p
  r := p+s
  IF r+t>maxlinel DO t := maxlinel-r
  linel := r+t
  UNLESS r=q TEST r>q THEN FOR i = t TO 1 BY -1 DO
                             linev!(r+i) := linev!(q+i)
                      ELSE FOR i = 1 TO t DO
                             linev!(r+i) := linev!(q+i)
  FOR i = 1 TO s DO linev!(p+i) := v!i
  nosubs := FALSE
}


// Search line positions P+1 to Q for a string
AND index(l, p, q, v) = VALOF
{ LET s = v!0
  q := q-s
  UNTIL p>q DO
  { LET r = l+p
    FOR i = 1 TO s DO
      TEST uppercase | v=z_match
      THEN UNLESS compch(r!i, v!i)=0 GOTO l
      ELSE UNLESS r!i=v!i GOTO l
    RESULTIS p
l:  p := p+1
  }
  RESULTIS -1
}


AND readglobal() BE
{ LET v = VEC smax
  LET s = 0
  LET p = 0
  LET n = 0
  LET l = currentline
  delim := commrdch()
  readcontext(v)
  s := v!0
  IF s=0 DO error(err_cntx, 'G')
  n := findglobal(v)
  TEST n<0 THEN { IF globcount>=gmax DO error(err_glob)
                  globcount := globcount+1
                  n := globcount
                  p := newvec(s)
                  FOR i = 0 TO s DO p!i := v!i
                  g_match!n := p
                }
           ELSE { discardvec(g_repl!n)
                }
  readcontext(v)
  s := v!0
  p := newvec(s)
  FOR i = 0 TO s DO p!i := v!i
  g_repl!n := p
  p := pointer
  s := nosubs
  nosubs := TRUE

  { putline()
    currentline := currentline!l_next
    IF currentline=0 DO currentline := l
    getline()
    changeglobal(n)
  } REPEATUNTIL currentline=l

  IF nosubs DO nosubs := s
  pointer := p
}

AND deleteglobal() BE
{ LET v = VEC smax
  delim := commrdch()
  readcontext(v)
  TEST v!0=0 THEN { FOR i = 1 TO globcount DO
                    { discardvec(g_match!i)
                      discardvec(g_repl!i)
                    }
                    globcount := 0
                  }
             ELSE { LET n = findglobal(v)
                    IF n<0 DO error(err_nom)
                    discardvec(g_match!n)
                    discardvec(g_repl!n)
                    FOR i = n TO globcount-1 DO
                      g_match!i,g_repl!i := g_match!(i+1),g_repl!(i+1)
                    globcount := globcount-1
                  }
}

AND findglobal(v) = VALOF
{ FOR i = 1 TO globcount DO
  { LET w = g_match!i
    IF v!0=w!0 & index(v, 0, v!0, w)=0 RESULTIS i
  }
    RESULTIS -1
 }


AND changeglobal(i) BE
{ LET p = 0
  LET v = g_match!i
  LET w = g_repl!i

  { LET n = index(linev, p, linel, v)
    IF n<0 BREAK
    subst(n, n+v!0, w)
    p := n+w!0
  } REPEAT
}
