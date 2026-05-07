SECTION "Test1"

GET "libhdr"

GLOBAL {
  stdin:ug
  stdout
}

/****************
GLOBAL { g250:250; g500:500; g800:800 }
STATIC { a=0 }

LET testmemchk(p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13) BE
{ LET x = 0
  a    := p3 // The pos argument
  g250 := a
  g500 := a
  g800 := a
  p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13 :=
  a, a, a, a, a, a, a, a,  a,  a,  a

sys(Sys_tracing, TRUE)
//x := !a    // RV
//x := a!1   // RV1
//x := a!2   // RV2
//x := a!3   // RV3
//x := a!4   // RV4
//x := a!5   // RV5
//x := a!6   // RV6
//x := a!7   // RV

//x := p3!0    // L0P3
//x := p3!1    // L1P3
//x := p3!2    // L2P3
//x := p3!3    // L3P3
//x := p3!4    // L4P3
//x := p3!5    // RV5

//x := p4!0    // L0P4
//x := p4!1    // L1P4
//x := p4!2    // L2P4
//x := p4!3    // L3P4
//x := p4!4    // L4P4
//x := p4!5    // RV5

//x := p5!0    // L0P5
//x := p5!1    // L1P5
//x := p5!2    // L2P5
//x := p5!3    // RV3

//x := p6!0    // L0P6
//x := p6!1    // L1P6
//x := p6!2    // RV2

//x := p7!0    // L0P7
//x := p7!1    // RV1

//x := p8!0    // L0P8
//x := p9!0    // L0P9
//x := p10!0   // L0P10
//x := p11!0   // L0P11
//x := p12!0   // L0P12
//x := p13!0   // RV

//x := g800!0   // L0GH 800
//x := g800!1   // L1GH 800
//x := g800!2   // L2GH 800
//x := g800!3   // RV3

//x := g500!0   // L0G1 244
//x := g500!1   // L1G1 244
//x := g500!2   // L2G1 244
//x := g500!3   // RV3

//x := g250!0   // L0G 250
//x := g250!1   // L1G 250
//x := g250!2   // L2G 250
//x := g250!3   // RV3

//!g800 := 0    // S0GH 800
//!g500 := 0    // S0G1 244
//!g250 := 0    // S0G 250

//p3!a := 0     // STP3
//p4!a := 0     // STP4
//p5!a := 0     // STP5
//p6!a := 0     // ST

//p3!0 := 0     // ST0P3
//p3!1 := 0     // ST1P3
//p3!2 := 0     // ST2

//p4!0 := 0     // ST0P4
//p4!1 := 0     // ST1P4
//p4!2 := 0     // ST2

//x := p3!a     // RVP3
//x := p4!a     // RVP4
//x := p5!a     // RVP5
//x := p6!a     // RVP6
//x := p7!a     // RVP7
//x := p8!a     // RV

//x := p3%0     // GBYT
//p3%0 := p3    // PBYT
//p3%0 := 0     // XPBYT

sys(Sys_tracing, FALSE)
}
**************************************/

LET start() = VALOF
{ LET argv  = VEC 30
  LET in, inout, out, pos = 0, 0, 0, 0
  LET posv  = VEC 1
  LET buf   = VEC 128/bytesperword

  stdin, stdout := input(), output()

  UNLESS rdargs("FROM,POS", argv, 30) DO
  { writef("Bad arguments for test1*n")
    stop(20)
  }

//argv!0, argv!1, argv!2 := 11,22,33
//writef("%n %n %n*n", argv[0],argv[1], argv[2])
//argv!0, argv!1, argv!2 := @argv[2], 222,333
//writef("%n %n %n*n", !argv[0], argv[1], argv[2])
//RESULTIS 0

//{ LET v = VEC 20
//  datstamp(v)
//  writef("date stamp %x8 %x8 %x8*n", v!0, v!1, v!2)

//  dat_to_strings((TABLE #x2245, #x03f3, #xB50E), v)
//  writef("date %s*n", v)
//  RESULTIS 0
//} 
/*
{ LET stream = findoutput("junk")
  LET stdout = output()
  LET stdin  = input()
  selectoutput(stream)
  FOR i = 1 TO 2 FOR ch = 0 TO 255 DO wrch(ch)
  endwrite()
  selectoutput(stdout)
  selectinput(findinput("junk"))
  FOR i = 1 TO 2 FOR ch = 0 TO 255 DO
  { LET k = rdch()
    UNLESS ch=k DO writef("ch=%x2 k=%x2*n", ch, k)
  }
  UNLESS rdch()=endstreamch DO writef("endstreamch expected*n")
  endread()
  selectinput(stdin)
  writes("end of test*n")
  RESULTIS 0
}
*/

//testmemchk(2000001)
//RESULTIS 0


  UNLESS argv!0 DO argv!0 := "junk"
  IF argv!1 & string_to_number(argv!1) DO pos := result2

  writef("file: %s  pos: %n*n", argv!0, pos)

  in := findinput(argv!0)
  UNLESS in DO
  { writef("Can't open file '%s'*n", argv!0)
    stop(20)
  }
  selectinput(in)

  writef("size: %n*n", sys(Sys_filesize, in!scb_fd))

  setrecordlength(in, 16) // 64 bytes
  FOR i = 0 TO 4 DO
  { get_record(buf, i, in)
    writef("record %i4: ", i)
    pr(buf, 64)
    get_record(buf, 62+i, in)
    writef("record %i4: ", 62+i)
    pr(buf, 64)
  }

  endread()
  selectinput(stdin)

  inout := findinoutput(argv!0)
  UNLESS inout DO
  { writef("Can't open %s for inout mode*n", argv!0)
    RESULTIS 0
  }

writef("*nfile %s opened in inout mode, size %n bytes*n",
          argv!0, sys(Sys_filesize, inout!scb_fd))

  setrecordlength(inout, 16) // 64 bytes
  get_record(buf, 2, inout)
  pr(buf, 64)

  FOR i = 0 TO 63 DO buf%i := i<63 -> 'A', '*n'
  put_record(buf, 0, inout)
  FOR i = 0 TO 63 DO buf%i := i<63 -> 'B', '*n'
  put_record(buf, 65, inout)
  FOR i = 0 TO 63 DO buf%i := i<63 -> 'C', '*n'
  put_record(buf, 4, inout)

writef("test1: calling endstream(inout)*n")

  endstream(inout)

  writef("End of test1*n")

  RESULTIS 0
}

AND rd(n) BE
{ LET out = output()
  LET ch = 0
  selectoutput(stdout)
  FOR i = 1 TO n DO 
  { ch := rdch()
    TEST ch=endstreamch
    THEN { writes("<EOF>"); BREAK }
    ELSE wrch(ch)
  }
  UNLESS ch='*n' DO newline()
  selectoutput(out)
}

AND pr(v, n) BE
{ LET out = output()
  LET ch = 0
  selectoutput(stdout)
  FOR i = 0 TO n-1 DO 
  { ch := v%i
    wrch(ch)
  }
  UNLESS ch='*n' DO newline()
  selectoutput(out)
}


