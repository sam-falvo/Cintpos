
// This is a compiler and interpreter for the VSPL
// implemented in vspl based on vspl.b

// (c) Martin Richards 29 Sep 2025

// History

// 01/10/2025
// Used putbyte and getbyte in placw of %.

// 29/09/2025
// VSPL has now been entended to provid e I/O to and from files.

// 27/09/2025
// Just started. It needs a slightly extended version of VSPL including
//extra sys functions to allows files to be opened for reading or writing
// and closed.

manifest
  // Sys function opcodes
  sys_exit = 0,
  sys_interpret,
  sys_settracing,
  sys_setcount,

  bytesperword = 4,
  bitsperword = 32, //bytesperword * 8,
  bigender = false,
  
  //  Selectors
  h1=0, h2, h3, h4, h5, h6,

  nametablesize = 541,

  c_tab         =   9,
  c_newline     =  10,
  c_newpage     =  12,
  c_space       =  32,
  c_cr          =  13,

  endstreamch   =  -1,

  // Lexical tokens, parse tree operators and op-codes

  Num=1, Name, String, True, False,
  Valof, Fnap, Lv, Ind, Vecap,
  Neg, Not, Mul, Div, Mod, Add, Sub,
  Eq, Ne, Le, Ge, Lt, Gt, Lsh, Rsh, And, Or, Xor,
  Comma, Fndef, Rtdef, Assign, Rtap,
  Resultis, Break, Loop, Return, Seq,
  Test, If, Unless, While, Until, For,
  Let, Vec, Static, Statvec, Manifest, Initval, Decl, Var, Const,
  Lparen, Rparen, Lsquare, Rsquare, Lcurly, Rcurly,
  To, Do, Then, Else, Be, Eof, Semicolon,
  Rtrn, Fnrn, Addr, Local, Lab, Data, Jt, Jf, Jump,
  Ln, Lp, Llp, Ll, Laddr, Sp, Sl, Stind, Lres,
  Entry, Stack, Printf, Sys, Halt

static

  rdargs,
  getvec,
  writes,
  newline,
  maxint,
  writef,
  writen,
  writed,
  writehex,
  stdin,
  stdout,
  result2,
  errorcount,
    recover,
  putch,
  
  
  
  // Global functions and variables

  argv[50],
  chbuf[64],
  chv[64],
  progname[64],
  toname[64],
  
  //rec_p, rec_l, fin_p, fin_l,
  errcount, errmax,
  progstream, tostream,
  treep, treevec,
  optTokens, optTree, optCode, optTrace,

// Globals used in LEX
  charv, ch, token, lexval, wordnode,
  chcount, lineno,
  namestart, nametable,

// Globals used in TRN and the interpreter
 
  dvec, dvece, dvecp, dvect,
  comline, procname, resultlab, ssp,
  mem, memt, regs,
  codev, codep, codet,
  datav, datap, datat,
  stack, stackt,
  labv, refv, labmax,
  labnumber

// Declare the library functions

let initlib() be return
//{ stdin  := findinput("*");
//  stdout := findoutput("*");
//  selectinput(stdin);
//  selectoutput(stdout);
//  printf("stdin=%d stdout=%d\n", stdin, stdout)
//}

let putbyte(str, i, ch) be return
//{ let p = i/bytesperword;
//  let sh = (i mod bytesperword) * 8;
//  if bigender do sh := bitsperword - sh;
//  str[p] := str[p] & ~(255<<sh) | ch<<sh
//}

let getbyte(str, i) = 0 //valof
//{ let p = i/bytesperword;
//  let sh = (i mod bytesperword) * 8;
//  if bigender do sh := bitsperword - sh;
//  resultis  str[p]>>sh & 255
//}

let findinput(filename)     =  sys(4, filename)

let findoutput(filename)    =  sys(5, filename)

let selectinput(instream)   be sys(6, instream)

let selectoutput(outstream) be sys(7, outstream)

let input()    =  sys(8)

let output()   =  sys(9)

let rdch(ch)   be sys(10)

let wrch(ch)   be sys(11, ch)

let endread()  be sys(12)

let endwrite() be sys(13)

let abort(n)   =  sys(14, n)

let gevec(upb) = sys(15, upb)

let freevec(p) be sys(16, p)

//let getargs(argv, upb) = 0 //valof
//{ let arg = 0
//  let res = false
  
  //for i = 0 to 256 do
  //{ putbyte(progname, i, 0)
  //  putbyte(toname,   i, 0)
  //}

  //optTokens    := false
  //optTree      := false
  //optCode      := false
  //optTrace     := false
  
  //while true=mem do
  //{ let arg = 0//getarg()
    //if arg=0 resultis res
    //printf("getarg => %s\n", arg)
    //if eqstr(arg, "-l") do { optTokens := true; loop }
    //if eqstr(arg, "-p") do { optTree   := true; loop }
    //if eqstr(arg, "-c") do { optCode   := true; loop }
    //if eqstr(arg, "-t") do { optTrace  := true; loop }

    //if getbyte(progname,0)=0 do
    //{ for i = 0 to 255 do
    //  { let ch = getbyte(arg, i)
    //    putbyte(progname, i, ch)
    //    if ch=0 break
    //  }
    //  //loop
    //}

    //if getbyte(toname,0)=0 do
    //{ for i = 0 to 255 do
    //  { let ch = getbyte(arg, i)
    //    putbyte(toname, i, ch)
    //    if ch=0 break
    //  }
    //  //loop
    //}

    //res := true
  //}

//  resultis res
//}

//let getarg() = valof
//{ let i = 0
//  for j = 0 to 255 do putbyte(chv, j, 0)
//  until i=255 do
//  { let ch = rdch()
//    printf("i=%3d %3d  %c\n", i, ch, ch)
//    if 'a'<=ch & ch <= 'z' |
//       'A'<=ch & ch <= 'Z' |
//       '0'<=ch & ch <= '9' |
//       ch='.' | ch='/'     |
//       ch='*'              do { putbyte(chv, i, ch); loop }
//    break
//  }
//  putbyte(chv, i, i)
//  resultis chv
//}

//let eqstr(s1, s2) = valof
//{ let i = 0
//  while i<=255 do
//  { let ch1 = getbyte(s1, i)
//    let ch2 = getbyte(s2, i)
//    if ch1=0 & ch2=0 resultis true
//    unless ch1=ch2 resultis false
//  }
//  resultis false
//}

let start() = valof
{ while true do
  { let treesize = 0;
    let memsize = 0;

    initlib();
    
    errmax   := 2;
    errcount := 0;

    treevec := 0;
    labv    := 0;
    refv    := 0;
    mem     := 0;

    progstream := 0;
    tostream   := 0;
   
    printf("*nVSPL (11 Oct 2025) Implemented in VSPL\n");
 
    //if getargs() do
    { printf("Bad arguments for vspl\n");
      resultis 0
    };

    treesize :=  500000;   // Values increased 29 Sep 2025
    memsize  := 1000000;
    labmax   :=    8000;

    unless getbyte(progname, 0)=0 do
    { printf("Bad arguments for vspl: No prog name given\n");
      resultis 0
    };

    progstream := findinput(progname);

    if progstream=0 do
    { fatalerr("Trouble with file %s\n", progname);
      break
    };
    selectinput(progstream);

    unless getbyte(toname, 0)=0 do
    { tostream := findoutput(toname);

      if tostream=0 do
      { fatalerr("Trouble with to file %s\n", toname);
        break
      };
      selectoutput(tostream)
    };

    progstream := findinput(progname);

    if progstream=0 do
    { fatalerr("Trouble with file %s\n", progname);
      break
    };
    selectinput(progstream);

    treevec := getvec(treesize);
    mem     := getvec(memsize);
    memt    := memsize;
    labv := getvec(labmax);
    refv := getvec(labmax);

    unless treevec & mem & labv & refv do
       fatalerr("Insufficient memory\n");
   
    selectoutput(tostream);
resultis 5555;

    { let tree = 0;
      for i = 0 to 63 do putbyte(chbuf, i, 0);
      chcount := 0;
      lineno  := 1;
      rch();
 
      treep := treevec + treesize;

      tree := formtree();             // Perform Syntax Analysis

      if optTokens do resultis 0;

      if optTree do { writes("Parse Tree*n");
                      plist(tree, 0, 20);
                      newline()
                    };
  
      if errcount do resultis 0;

      regs  := 10;
      codev := 100;
      codep := codev;
      codet := 50000;
      datav := codet;
      datap := datav;
      datat := memt;

      for i = 0 to memt do mem[i] := 0;

      trprog(tree);                    // Translate the tree

      stack := datap;
      stackt := memt;

      if errcount do resultis 0;
      
      { let rv = mem+regs;
        let sv = mem+stack;
        rv[0] := 0;        // result register
        rv[1] := stack;    // p pointer
        rv[2] := stack+2;  // sp
        rv[3] := codev;    // pc (=100)
        rv[4] := maxint;   // count

        sv[0] := 0;
        sv[1] := 0;
        sv[2] := 0;

        { let ret = interpret(regs, mem);   // Execute the interpreter
          if ret do writef("Return code %n\n", ret);
          writef("\nInstructions executed: %n\n", maxint-rv[4])
        }
      }
    }
  }; // End of while true loop
   
// fin: reached by resultis 0 in the valof block above.
  if treevec       do freevec(treevec);
  if mem           do freevec(mem);
  if labv          do freevec(labv);
  if refv          do freevec(refv);
  if progstream    do { selectinput(progstream); endread()  };
  if tostream      do { selectoutput(tostream);
                        unless tostream=stdout do endwrite() };

  selectoutput(stdout);
  result2 := 0;     // No reason given
  test errorcount
  then resultis 20
  else resultis  0
}

let lex() be token := valof
{ while ch='\n' | ch='\p' | ch='\c' | ch='\t' | ch='\s' do
  { if ch='\p' | ch='\n' do lineno := lineno + 1;
    rch()
  };

  if '0'<=ch & ch<='9' do
  { lexval := 0;
    while '0'<=ch<='9' do
    { lexval := 10*lexval + ch - '0';
      rch()
    };
    resultis Num
  };

  if ch='#' do // Hexadecimal constants, eg #7FF
  { lexval := 0;  // Added 9/08/2021
    rch();
    while '0'<=ch & ch<='9' |
          'A'<=ch & ch<='F' |
	  'a'<=ch & ch<='f' do
    { test '0'<=ch & ch<='9'
      then lexval := (lexval<<4) + ch - '0'
      else test 'A'<=ch & ch<='F'
           then lexval := (lexval<<4) + ch - 'A' + 10
           else lexval := (lexval<<4) + ch - 'a' + 10;
      rch()
    };
    
    resultis Num
  };
    
  if 'A'<=ch & ch<='F' |
     'a'<=ch & ch<='f' do
    resultis lookupword(rdtag());

  if ch='{'  do { rch(); resultis Lcurly };
  if ch='}'  do { rch(); resultis Rcurly };
  if ch='['  do { rch(); resultis Lsquare };
  if ch=']'  do { rch(); resultis Rsquare };
  if ch='('  do { rch(); resultis Lparen };
  if ch=')'  do { rch(); resultis Rparen };
  if ch='!'  do { rch(); resultis Ind };
  if ch='@'  do { rch(); resultis Lv };
  if ch='+'  do { rch(); resultis Add };
  if ch='-'  do { rch(); resultis Sub };
  if ch=','  do { rch(); resultis Comma };
  if ch=';'  do { rch(); resultis Semicolon };
  if ch='&'  do { rch(); resultis And };
  if ch='|'  do { rch(); resultis Or };
  if ch='='  do { rch(); resultis Eq };
  if ch='*'  do { rch(); resultis Mul };
  if ch='^'  do { rch(); resultis Xor};
 
  if ch='/' do
  { rch();
    if ch='/' do
    { rch();
      until ch='\n' | ch=endstreamch do rch();
      resultis lex()
    };
    resultis Div
  };
 
  if ch='~' do
  { rch();
    if ch='=' do { rch(); resultis Ne };
    resultis Not
  };
 
  if ch='<' do
  { rch();
    if ch='=' do { rch(); resultis Le };
    if ch='<' do { rch(); resultis Lsh };
    resultis Lt
  };
  
  if ch='>' do
  { rch();
    if ch='=' do { rch(); resultis Ge };
    if ch='>' do { rch(); resultis Rsh };
    resultis Gt
  };
 
  if ch=':' do
  { rch();
    if ch='=' do { rch(); resultis Assign };
    synerr("'=' expected after ':'");
    resultis 0
  };
 
  if ch='"' do
  { let len = 0;
    rch();
 
    until ch='"' do
    { if len=255 do synerr("Bad string constant");
      len := len + 1;
      putbyte(charv, len, rdstrch())
    };
 
    putbyte(charv, 0, len);
    wordnode := newvec(len/bytesperword+2);
    h1[wordnode] := String;
    for i = 0 to len do putbyte(@h2[wordnode], i, getbyte(charv, i));
    resultis String
  };
 
  if ch='\'' do
  { rch();
    lexval := rdstrch();
    unless ch='\'' do synerr("Bad character constant");
    resultis Num
  };

  if ch=endstreamch do resultis Eof;
  
  { let badch = ch;
    ch := '\s';
    synerr("Illegal character %x2", badch)
  };
  
  resultis 0
}
 
let lookupword(word) = valof
{ let len = getbyte(word, 0);
  let i   = 0;
  let hashval = len;
  for i = 1 to len do
    hashval := (13*hashval + getbyte(word, i)) & #FFFFFF;
  hashval := hashval mod nametablesize;
  wordnode := nametable[hashval];
 
  while wordnode & i<=len do
    test getbyte(@h3[wordnode], i) = getbyte(word, i)
    then i := i+1
    else { wordnode := h2[wordnode]; i := 0 };

  if wordnode=0 do
  { wordnode := newvec(len/bytesperword+3);
    h1[wordnode] := Name;
    h2[wordnode] := nametable[hashval];
    for i = 0 to len do putbyte(@h3[wordnode], i, getbyte(word, i));
    nametable[hashval] := wordnode
  };

  resultis h1[wordnode]
}
 
let dsw(word, tok) be { lookupword(word); h1[wordnode] := tok }
 
let declsyswords() be
{ dsw("be", Be);             dsw("do", Do);         dsw("else", Else);
  dsw("false", False);       dsw("if", If);         dsw("for", For);
  dsw("let", Let);           dsw("mod", Mod);       dsw("printf", Printf);
  dsw("resultis", Resultis); dsw("return", Return); dsw("static", Static);
  dsw("sys", Sys);           dsw("test", Test);     dsw("to", To);
  dsw("true", True);         dsw("then", Then);     dsw("valof", Valof);
  dsw("vec", Vec);           dsw("unless", Unless); dsw("until", Until);
  dsw("while", While);
  lookupword("start"); // Create the name: start
  namestart := wordnode
} 
 
let rch() be
{ ch := rdch();
  chcount := chcount+1;
  putbyte(chbuf, chcount&63, ch)
}
 
let wrchbuf() be
{ writes("*n...");
  for p = chcount-63 to chcount do
  { let k = getbyte(chbuf, p&63);
    if 0<k<255 do wrch(k)
  };
  newline()
}
 
let rdtag() = valof
{ let len = 0;
  while 'a'<=ch & ch<='z' |
        'A'<=ch & ch<='Z' |
	'0'<=ch & ch<='9' |
	ch='_' do
  { len := len+1;
    if len>255 do synerr("Name too long");
    putbyte(charv, len, ch);
    rch()
  };
  putch(charv, 0, len);
  resultis charv
}
 
let rdstrch() = valof
{ let res = ch;

  if ch='\n' | ch='\p' do
  { lineno := lineno+1;
    synerr("Unescaped newline character")
  };

  if ch='\\' do
  { rch();
    res := valof
    { if ch='\\' |
         ch='\'' |
	 ch='\"' do
      { res := ch;
        rch();
	resultis res
      };
      if ch='t' | ch='T' do
      { res := c_tab;
        rch();
	resultis res
      };
      if ch='n' | ch='N' do
      { res := c_newline;
        rch();
	resultis res
      };
      if ch='s' | ch='S' do
      { res := c_space;
        rch();
	resultis res
      };
      if ch='p' | ch='P' do
      { res := c_newpage;
        rch();
	resultis res
      };
      if ch='n' | ch='N' do
      { res := c_newline;
        rch();
	resultis res
      };
      if ch='c' | ch='C' do
      { res := c_cr;
        rch();
	resultis res
      };

      synerr("Bad string or character constant")
    }
  };
  
  rch();
  resultis res
}

let newvec(n) = valof
{ treep := treep - n - 1;
  if treep<=treevec do fatalerr("More workspace needed");
  resultis treep
}
 
let mk1(a) = valof
{ let p = newvec(0);
  p[0]:= a;
  resultis p
}
 
let mk2(a, b) = valof
{ let p = newvec(1);
  p[0] := a;
  p[1] := b;
  resultis p
}
 
let mk3(a, b, c) = valof
{ let p = newvec(2);
  p[0] := a;
  p[1] := b;
  p[2] := c;
  resultis p
}
 
let mk4(a, b, c, d) = valof
{ let p = newvec(3);
  p[0] := a;
  p[1] := b;
  p[2] := c;
  p[3] := d;
  resultis p
}
 
let mk5(a, b, c, d, e) = valof
{ let p = newvec(4);
  p[0] := a;
  p[1] := b;
  p[2] := c;
  p[3] := d;
  p[4] := e;
  resultis p
}
 
let mk6(a, b, c, d, e, f) = valof
{ let p = newvec(5);
  p[0] := a;
  p[1] := b;
  p[2] := c;
  p[3] := d;
  p[4] := e;
  p[5] := f;
  resultis p
}
 
let formtree() = valof
{ let res = 0;
  //rec_p := level();
  //rec_l := recover;

  charv := newvec(256/bytesperword);     
  nametable := newvec(nametablesize);
  unless charv & nametable do fatalerr("More workspace needed");
  for i = 0 to nametablesize do nametable[i] := 0;
  declsyswords();
  lex();

  while optTokens do            // For debugging lex.
  { if token=Eof do resultis 0;
    writef("token = %i3 %s", token, opstr(token));
    if token=Num    do writef("       %n",  lexval);
    if token=Name   do writef("      %s",   charv);
    if token=String do writef("    \"%s\"", charv);
    newline();
    lex()
  };

  // This point is reached when synerr is called.
//recover:
  res := rdprog();
  unless token=Eof do fatalerr("Incorrect termination");
  resultis res
}

// Rather than using longjump after generating an error
// message the following error functions just return
// with result -1.

let fatalerr(mess, a) = valof
{ writef("\nFatal error:  ");
  writef(mess, a);
  writes("\nCompilation aborted*n");
  errcount := errcount+1;
  resultis -1
}

let synerr(mess, a) = valof
{ writef("\nError near line %n:  ", lineno);
  writef(mess, a);
  wrchbuf();
  errcount := errcount+1;
  if errcount >= errmax do
    resultis fatalerr("Too many errors");

  // Skip the rest of the input line 
  until ch='\n' | ch=endstreamch do rch();
  lex();
  resultis -1
}

let checkfor(tok, mess) be
{ unless token=tok do synerr(mess);
  lex()
}
 
let rdprog() = valof
{ let ln = lineno;

  if token=Eof do resultis 0;

  if token=Static do
  { let d = 0;
    lex();
    d := mk3(Static, rstatlist(), ln);
    resultis mk3(Decl, d, rdprog())
  };

  if token=Let do
  { let n    = 0;
    let args = 0;
    lex();
    n := rname();
    checkfor(Lparen, "'(' missing");
    if token=Name do args := rnamelist();
    checkfor(Rparen, "')' missing");
 
    if token=Be do
    { let d = mk5(Rtdef, n, args, rncom(), ln);
      resultis mk3(Decl, d, rdprog())
    };
 
    if token=Eq do
    { let d = mk5(Fndef, n, args, rnexp(0), ln);
      resultis mk3(Decl, d, rdprog())
    };

    resultis synerr("Bad procedure heading")
  };
  
  resultis synerr("Bad outer level declaration*n")
}

let rdblockbody() = valof
{ let res = 0;
  //let orec_p = rec_p;
  //let orec_l = rec_l;
  let op = token;
  //rec_p := level();
  //rec_l := recover;

  // A block body starts with let, vec, static or
  // is a sequence of commands.
  
// The recovery point after a syntax error
///recover:

  if op=Let | op=Vec do
  { let n  = 0;
    let e  = 0;
    let ln = lineno;
    lex();
    n := rname();
    test op=Let
    then { checkfor(Eq, "Missing '='");
           e := rexp(0)
         }
    else { checkfor(Lsquare, "Missing '['");
           e := rexp(0);
           unless h1[e]=Num do synerr("Bad 'vec' declaration");
           checkfor(Rsquare, "Missing ']'")
         };
    checkfor(Semicolon, "';' expected");
    res := mk5(op, n, e, rdblockbody(), ln);
    //rec_p := orec_p;
    //rec_l := orec_l;
    resultis res
  };
  
  res := rdseq();

  //rec_p := orec_p;
  //rec_l := orec_l;
  resultis res
}
 
let rdseq() = valof
{ let a = 0;
  a := rcom();
  if token=Rcurly | token=Eof do resultis a;
  checkfor(Semicolon, "';' expected");
  resultis mk3(Seq, a, rdseq())
}

let rnamelist() = valof
{ let a = rname();
  unless token=Comma do resultis a;
  lex();
  resultis mk3(Comma, a, rnamelist())
}

let rexplist() = valof
{ let a = rexp(0);
  unless token=Comma do resultis a;
  lex();
  resultis mk3(Comma, a, rexplist())
}
 
let rstatlist() = valof
{ let a = rname();
  if token=Lsquare do
  { let b = rnexp(0);
    unless h1[b]=Num do synerr("Number expected");
    checkfor(Rsquare, "']' expected");
    a := mk3(Statvec, a, b)
  };
  unless token=Comma do resultis a;
  lex();
  resultis mk3(Comma, a, rstatlist())
}

let rname() = valof
{ let a = wordnode;
  checkfor(Name, "Name expected");
  resultis a
}
 
let rbexp() = valof
{ let a  = 0;
  let op = token;
  let ln = lineno;
 
  if op=True |
     op=False |
     op=Name |
     op=String do
  { a := wordnode;
    lex();
    resultis a
  };
 
  if op=Num do
  { a := mk2(Num, lexval);
    lex();
    resultis a
  };
 
  if op=Printf |
     op=Sys do
  { lex();
    checkfor(Lparen, "'(' missing");
    a := 0;
    unless token=Rparen do a := rexplist();
    checkfor(Rparen, "')' missing");
    resultis mk3(op, a, ln)
  };

  if op=Lparen do
  { a := rnexp(0);
    checkfor(Rparen, "')' missing");
    resultis a
  };
  
  if op=Valof do
    resultis mk2(Valof, rncom());
 
  if op=Ind |
     op=Lv do resultis mk2(op, rnexp(7));
 
  if op=Add do resultis rnexp(5);
 
  if op=Sub do
  { a := rnexp(5);
    test h1[a]=Num then h2[a] := - h2[a]
                   else a := mk2(Neg, a);
    resultis a
  };

  if op=Not do resultis mk2(Not, rnexp());

  resultis synerr("Error in expression")
}
 
let rnexp(n) = valof { lex(); resultis rexp(n) }
 
let rexp(n) = valof
{ let a = rbexp();
  let b = 0;
  let p = 0;

  while true do
  { // Test whether there is an expression operator that can
    // be read. Set b to -1 if not.
    b := valof
    { let op = token;
      let ln = lineno;

      if op=Lparen do
      { lex();
        b := 0;
        unless token=Rparen do b := rexplist();
        checkfor(Rparen, "')' missing");
        resultis mk4(Fnap, a, b, ln)
      };

      if op=Lsquare do
      { b := rnexp(0);
        checkfor(Rsquare, "']' missing");
        resultis mk3(Vecap, a, b)
      };
 
      if op=Mul |
         op=Div |
	 op=Mod do
      { if n>=7 do resultis -1; // Don't read this op
        resultis mk3(op, a, rnexp(p))
      };

      if op=Add |
         op=Sub do
      { if n>=6 do resultis -1; // Don't read this op
        resultis mk3(op, a, rnexp(p))
      };

      if op=Lsh |
         op=Rsh do
      { if n>=5 do resultis -1; // Don't read this op
        resultis mk3(op, a, rnexp(p))
      };

      if op=Eq |
         op=Le |
	 op=Lt |
	 op=Ne |
	 op=Ge |
	 op=Gt do
      { if n>=4 do resultis -1; // Don't read this op
        resultis mk3(op, a, rnexp(p))
      };

      if op=And do
      { if n>=3 do resultis -1; // Don't read this op
        resultis mk3(op, a, rnexp(p))
      };

      if op=Or do
      { if n>=2 do resultis -1; // Don't read this op
        resultis mk3(op, a, rnexp(p))
      };

      if op=Xor do
      { if n>=1 do resultis -1; // Don't read this op
        resultis mk3(op, a, rnexp(p))
      };

      resultis -1 // op is not an infixed expression operator
    };

    if b<0 do resultis a;
    a := b
  } // See if there is another expression operator.
}
  
let rcom() = valof
{ let n  = 0;
  let a  = 0;
  let b  = 0;
  let op = token;
  let ln = lineno;
 
  if op=Name   |
     op=Num    |
     op=Lparen |
     op=Ind    |
     op=Sys    |
     op=Printf |
     op=Break  |
     op=Loop   do
  { // All tokens that can start an expression.
    a := rexp(0);
 
    if token=Assign do
    { // a is the LHS of an assignment and so must have
      // be one of the following forms.
      unless h1[a]=Name  |
             h1[a]=Vecap |
             h1[a]=Ind   do
         synerr("Bad assigment statement");
       resultis mk4(Assign, a, rnexp(0), ln)
    };
 
    if h1[a]=Fnap do
    { // If a is syntactically a function call turn it into
      // a routine call.
      h1[a] := Rtap;
      resultis a
    };

    // If the command was not an assignment or a routine call
    // it can only be a sys or printf command.
    unless h1[a]=Sys |
           h1[a]=Printf do
      synerr("Error in command");
    resultis a
  };

  if op=Resultis do
    resultis mk3(op, rnexp(0), ln);
 
  if op=If    |
    op=Unless |
    op=While  |
    op=Until  do
  { a := rnexp(0);
    if token=Do do lex();
    resultis mk4(op, a, rcom(), ln)
  };

  if op=Test do
  { a := rnexp(0);
    checkfor(Then, "'then' missing");
    b := rcom();
    checkfor(Else, "'else' missing");
    resultis mk5(Test, a, b, rcom(), ln)
  };
 
  if op=For do
  { lex();
    n := rname();
    checkfor(Eq, "'=' expected");
    a := rexp(0);
    checkfor(To, "'to' expected");
    b := rexp(0);
    if token=Do do lex();
    resultis mk6(For, n, a, b, rcom(), ln)
  };

  if op=Return do
  { lex();
    resultis mk2(op, ln)
  };
 
  if op=Lcurly do
  { lex();
    a := rdblockbody();
    checkfor(Rcurly, "'}' expected");
    resultis a
  };

  resultis synerr("Command expected")
}

let rncom() = valof { lex(); resultis rcom() }

let opstr(op) = valof
{ if op=Assign do    resultis "Assign";
  if op=Add do       resultis "Add";
  if op=And do       resultis "And";
  if op=Be do        resultis "Be";
  if op=Comma do     resultis "Comma";
  if op=Data do      resultis "Data";
  if op=Decl do      resultis "Decl";
  if op=Div do       resultis "Div";
  if op=Do do        resultis "Do";
  if op=Else do      resultis "Else";
  if op=Entry do     resultis "Entry";
  if op=Eq do        resultis "Eq";
  if op=False do     resultis "False";
  if op=Fnap do      resultis "Fnap";
  if op=For do       resultis "For";
  if op=Fndef do     resultis "Fndef";
  if op=Fnrn do      resultis "Fnrn";
  if op=Ge do        resultis "Ge";
  if op=Gt do        resultis "Gt";
  if op=Halt do      resultis "Halt";
  if op=If do        resultis "If";
  if op=Ind do       resultis "Ind";
  if op=Jf do        resultis "Jf";
  if op=Jt do        resultis "Jt";
  if op=Jump do      resultis "Jump";
  if op=Lab do       resultis "Lab";
  if op=Laddr do     resultis "Laddr";
  if op=Lcurly do    resultis "Lcurly";
  if op=Le do        resultis "Le";
  if op=Let do       resultis "Let";
  if op=Ll do        resultis "Ll";
  if op=Llp do       resultis "Llp";
  if op=Ln do        resultis "Ln";
  if op=Lp do        resultis "Lp";
  if op=Lparen do    resultis "Lparen";
  if op=Lres do      resultis "Lres";
  if op=Lsh do       resultis "Lsh";
  if op=Lsquare do   resultis "Lsquare";
  if op=Lt do        resultis "Lt";
  if op=Lv do        resultis "Lv";
  if op=Mod do       resultis "Mod";
  if op=Mul do       resultis "Mul";
  if op=Name do      resultis "Name";
  if op=Ne do        resultis "Ne";
  if op=Neg do       resultis "Neg";
  if op=Not do       resultis "Not";
  if op=Num do       resultis "Num";
  if op=Or do        resultis "Or";      
  if op=Printf do    resultis "Printf";
  if op=Rcurly do    resultis "Rcurly";
  if op=Resultis do  resultis "Resultis";
  if op=Return do    resultis "Return";
  if op=Rparen do    resultis "Rparen";
  if op=Rsh do       resultis "Rsh";
  if op=Rsquare do   resultis "Rquare";
  if op=Rtap do      resultis "Rtap";
  if op=Rtdef do     resultis "Rtdef";
  if op=Rtrn do      resultis "Rtrn";
  if op=Semicolon do resultis "Semicolon";
  if op=Seq do       resultis "Seq";
  if op=Sl do        resultis "Sl";
  if op=Sp do        resultis "Sp";
  if op=Stack do     resultis "Stack";
  if op=Static do    resultis "Static";
  if op=Statvec do   resultis "Statvec";
  if op=String do    resultis "String";
  if op=Stind do     resultis "Stind";
  if op=Sub do       resultis "Sub";
  if op=Sys do       resultis "Sys";
  if op=Test do      resultis "Test";
  if op=Then do      resultis "Then";
  if op=To do        resultis "To";
  if op=True do      resultis "True";
  if op=Valof do     resultis "Valof";
  if op=Vecap do     resultis "Vecap";
  if op=Vec do       resultis "Vec";
  if op=Unless do    resultis "Unless";
  if op=Until do     resultis "Until";
  if op=While do     resultis "While";
  if op=Xor do       resultis "Xor";

  resultis "Unknown"
}

let plistaux(x, n, d, ln, size, v) be
{ if n=d do { writes("Etc"); return };
  writef("%s", opstr(h1[x]));
  if ln do writef("  -- line %n", ln);
  for i = 2 to size do { newline();
                         for j=0 to n-1 do writes( v[j] );
                         writes("*-");
                         test i=size
			 then v[n] := "  "
			 else v[n] := "! ";
                         plist(h1[x+i-1], n+1, d)
                       }
}

let plist(x, n, d) be
{ let s    = 0;
  let size = 0;
  let ln   = 0;
  let op = h1[x];
  vec v[20];
  for i = 0 to 19 do v[i] := 0;

  if x=0 do       { writes("Nil");         return };
 
  if op=Num do    { writen(h2[x]);         return };
  if op=Name do   { writes(x+2);           return };
  if op=String do { writef("\"%s\"", x+1); return };

  if op=For do
  { plistaux(x, n, d, h6[x], 5);
    return
  };
    
  if op=Fndef |
     op=Rtdef do 
  { plistaux(x, n, d, h5[x], 4, v);
    return
  };
    
  if op=Let  |
     op=Vec  |
     op=Test do
  { plistaux(x, n, d, h5[x], 4, v);
    return
  };

  if op=Vecap   |
     op=Mul     |
     op=Div     |
     op=Mod     |
     op=Add     |
     op=Sub     |
     op=Eq      |
     op=Ne      |
     op=Lt      |
     op=Gt      |
     op=Le      |
     op=Ge      |
     op=Lsh     |
     op=Rsh     |
     op=And     |
     op=Or      |
     op=Xor     |
     op=Comma   |
     op=Seq     |
     op=Decl    |
     op=Statvec do
  { plistaux(x, n, d, 0, 3, v);
    return
  };

  if op=Assign |
     op=Rtap   |
     op=Fnap   |
     op=If     |
     op=Unless |
     op=While  |
     op=Until  do
  { plistaux(x, n, d, h4[x], 3, v);
    return
  };
    
  if op=Valof |
     op=Lv    |
     op=Ind   |
     op=Neg   |
     op=Not   do
  { plistaux(x, n, d, 0, 2, v);
    return
  };

  if op=Printf   |
     op=Sys      |
     op=Static   |
     op=Resultis do
  { plistaux(x, n, d, h3[x], 2, v);
    return
  };
    
  if op=True  |
     op=False do
  { plistaux(x, n, d, 0, 1, v);
    return
  };

  plistaux(x, n, d, 0, 1, v)
}

let trnerr(mess, a) = valof
{ writes("Error");
  if procname do writef(" in %s", @h3[procname]);
  if comline do writef(" near line %n", comline);
  writes(":   ");
  writef(mess, a);
  newline();
  errcount := errcount + 1;
  if errcount >= errmax do
    resultis fatalerr("*nCompilation aborted*n");
  resultis -1
}

let trprog(x) be
{ dvec  := treevec;
  dvect := treep;
  h1[dvec] := 0;
  h2[dvec] := 0;
  h3[dvec] := 0;
  dvece := dvec+3;
  for i = 0 to nametablesize-1 do
  { let name = nametable[i];
    until name=0 do
    { let next = h2[name];
      h2[name] := 0; // Mark undeclared
      name := next
    }
  };

  for i = 0 to labmax do
  { labv[i] := -1;
    refv[i] := 0
  };

  resultlab := -2;
  comline   := 1;
  procname  := 0;
  labnumber := 1;
  ssp := 2;

  // Place the three initial instructions starting at codep (=100)
  outfl(Laddr, 1); ssp := ssp+1;  // 1 = lab number of start
  outfn(Fnap, 3);  ssp := ssp-1;
  outf(Halt);

  declstatnames(x);
  checkdistinct(dvec+3);
  while x do { trdecl(h2[x]); x:=h3[x] };
  resolvelabels();
  writef("Program size: %n   Data size: %n*n", codep-codev, datap-datav)
}

let trnext(next) be
{ // Compile Rtrn, Jump or nothing depending
  // on the value of next.
  if next<0 do outf(Rtrn);
  if next>0 do outfl(Jump, next)
}
 
let trcom(x, next) be
// x       is the command to translate
// next<0  compile x followed by Rtrn
// next>0  compile x followed by Jump next
// next=0  compile x only
{ let op = h1[x];

  if op=Let do
  { let e = dvece;
    let s = ssp;
    comline := h5[x];
    addname(h2[x], Local, ssp+1);
    load(h3[x]);
    trcom(h4[x], next);
    undeclare(e);
    outfn(Stack, s);
    ssp := s;
    return
  };
  
  if op=Vec do
  { let e = dvece;
    let s = ssp;
    comline := h5[x];
    addname(h2[x], Vec, ssp+1);
    ssp := ssp + h2[h3[x]];
    outfn(Stack, ssp);
    trcom(h4[x], next);
    undeclare(e);
    outfn(Stack, s);
    ssp := s;
    return
  };
  
  if op=Assign do
  { comline := h4[x];
    assign(h2[x], h3[x]);
    trnext(next);
    return
  };
  
// Note that if a function is called it sets res but this value is ignored
  if op=Rtap do
  { let s = ssp;
    comline := h4[x];
    ssp := ssp+3;
    outfn(Stack, ssp);
    loadlist(h3[x]);      // Load the arguments
    load(h2[x]);          // Load the function entry address
    outfn(Rtap, s+1);    // s+1 is the p pointer increment
    ssp := s;
    trnext(next);
    return
  };
 
  if op=Printf |  // Printf does not set res
     op=Sys    do // Sys may set res, but this is ignore if called as a command
  { let s = ssp;
    let op = h1[x];
    comline := h3[x];
    loadlist(h2[x]);
    outfn(op, s+1);
    ssp := s;
    trnext(next);
    return
  };
 
  if op=Unless |
     op=If     do
  { comline := h4[x];
    test next>0
    then { jumpcond(h2[x], op=Unless, next);
           trcom(h3[x], next)
         }
    else { let l = nextlab();
           jumpcond(h2[x], op=Unless, l);
           trcom(h3[x], next);
           outlab(l);
           trnext(next)
         };
    return
  };
   
  if op=Test do
  { let l = nextlab();
    let m = 0;
    comline := h5[x];
    jumpcond(h2[x], false, l);
         
    test next=0
    then { m := nextlab(); trcom(h3[x], m) }
    else trcom(h3[x], next);
                     
    outlab(l);
    trcom(h4[x], next);
    unless m=0 do outlab(m);
    return
  };
 
  if op=Return do
  { comline := h2[x];
    outf(Rtrn);
    return
  };

  if op=Resultis do
  { comline := h3[x];
    if resultlab=-1 do { fnbody(h2[x]); return };
    unless resultlab>0 do
    { trnerr("resultis out of context");
      return
    };
    load(h2[x]);
    outfl(Resultis, resultlab);
    ssp := ssp - 1;
    return
  };

  if op=Until |    // [Until, E, C, ln]
     op=While do   // [While, E, C, ln]
  { let l = nextlab();
    let m = next;
    comline := h4[x];
    if next<=0 do m := nextlab();
    jumpcond(h2[x], op=Until, m);
    outlab(l);
    trcom(h3[x], 0);
    comline := h4[x];
    jumpcond(h2[x], op=While, l);
    if next<=0 do outlab(m);
    trnext(next);
    return
  };
 
  if op=For do
  { let e = dvece;
    let s = ssp;
    let l = nextlab();
    let m = nextlab();
    comline := h5[x];
    addname(h2[x], Local, ssp+1);
    load(h3[x]);  // The control variable at s+1
    load(h4[x]);  // The end limit        at s+2

    outfl(Jump, m);               // Jump to test

    outlab(l);                    // Start of body
    trcom(h5[x], 0);

    outfn(Lp, s+1); ssp := ssp+1; // Inc control variable
    outfn(Ln, 1);   ssp := ssp+1;
    outf(Add);      ssp := ssp-1;
    outfn(Sp, s+1); ssp := ssp-1;

    outlab(m);
    outfn(Lp, s+1); ssp := ssp+1; // Compare withlimit
    outfn(Lp, s+2); ssp := ssp+1;
    outf(Le);       ssp := ssp-1;
    outfl(Jt, l);   ssp := ssp-1;

    undeclare(e);
    outfn(Stack, s);
    ssp := s;
    trnext(next);
    return
  };
  
  if op=Seq do
  { trcom(h2[x], 0);
    trcom(h3[x], next)
  };

  trnerr("Compiler error in Trans");
  return
}

let declstatnames(x) be while x do
{ // D  -> [Static, SL, ln]
  //       [Fndef, N, NL, E, ln]
  //       [Rtdef, N, NL, C, ln]

  // SL -> S
  //       [Comma, S, SL]

  // S  -> N
  //       [Statvec, N, [Num, k]]

  let op = h1[x];
  let d  = h2[x];

  unless op=Static | op=Fndef | op=Rtdef do
  { trnerr("Compiler error in declstatnames");
    return
  };
  
  if op=Static do
  { let p  = h2[d];
    let np = 0;
    while p do
    { let op = h1[p];

      unless op=Comma | op=Name | op=Statvec do
      { trnerr("Compiler error in declstatnames");
        return
      };
      
      if op=Comma do
      { declstatnames(h2[x]);
        declstatnames(h3[x]);
        return
      };

      if op=Name do
      { let lab = nextlab();
        outvar(lab);
        addname(p, Var, lab);
        p := np;
        np := 0
      };

      if op=Statvec do
      { // p -> [Statvec, N, [Num, upb]]
        let lab = nextlab();
        let upb = h2[h3[p]];
        outstatvec(lab, upb);
        addname(h2[p], Addr, lab);
        p := np;
        np := 0
      }
    }
  };

  if op=Fndef |
     op=Rtdef do 
  { // x ->  [Fndef, N, NL, E, ln]
    //       [Rtdef, N, NL, C, ln]
    let name = h2[d];
    let lab  = 1;
    unless name=namestart do lab := nextlab();
    addname(name, Addr, lab);
    declstatnames(h3[x]);
    return
  }
}

let decldyn(x) be unless x=0 do
{ if h1[x]=Name do
  { ssp := ssp+1;
    addname(x, Local, ssp);
    return
  };
 
  if h1[x]=Comma do
  { ssp := ssp+1;
    addname(h2[x], Local, ssp);
    decldyn(h3[x]);
    return
  };
 
  trnerr("Compiler error in Decldyn")
}
 
let checkdistinct(p) be
{ let lim = dvece - 3;
  let q = p;
  while q <=  lim-3 do
  { let n = h1[q];
    let c = q+3;
    while c <= lim do
    { if h1[c]=n do trnerr("Name %s defined twice", @h3[n]);
      q := q+3
    }
  }
}
 
let addname(name, k, a) be
{ let p = dvece + 3;
  if p>dvect do { trnerr("More workspace needed"); return };
  h1[dvece] := name;
  h2[dvece] := k;
  h3[dvece] := a;
  h2[name] := dvece; // Remember the declaration
  dvece := p
}
 
let undeclare(e) be 
{ let t = e;
  while t <= dvece-3 do
  { let name = h1[t];
    h2[name] := 0;   // Forget its declaration
    t := t+3
  };
  dvece := e
}

let cellwithname(n) = valof
{ let t = h2[n];
  unless t=0 do resultis t;  // It has been looked up before
  t := dvece-3;
  until h1[t]=n | h1[t]=0 do t := t-3;
  h2[n] := t;  // Associate the name with declaration item
  resultis t
}
 
let trdecl(x) be
{ // x -> [ Static, ...]
  //      { Fndef, ...]
  //      { Rtdef, ...]
  // If this declaration is for a function or routin
  // compile it,
  // Static declarations are compiled in declstatnames
  let op = h1[x];


  if op=Fndef | op=Rtdef do
  { let e = dvece;
    let name = h2[x];
    let t = cellwithname(name);
    let strlab = nextlab();

    resultlab := -2; // Not currently in a valof block.
    
    procname := name;

    outstring(strlab, @h3[procname]);
    outentry(h3[t], strlab);
    ssp := 2;
    decldyn(h3[x]);  // Declare the formal paramenters
    checkdistinct(e);
    outfn(Stack, ssp);
    test h1[x]=Rtdef then trcom(h4[x], -1)
                     else fnbody(h4[x]);
 
    undeclare(e);
    procname := 0
  }
}
 
let jumpcond(x, b, l) be
{ let sw = b;
  let op = h2[x];

  if op=False do
  { unless b do outfl(Jump, l);
    return
  };

  if op=True do
  { if b do outfl(Jump, l);
    return
  };

  if op=Not do
  { jumpcond(h2[x], ~b, l);
    return
  };

  if op=And do
  { test ~sw then { jumpcond(h2[x], b, l);
                    jumpcond(h3[x], b, l);
                    return
                  }
             else { let m = nextlab();
                    jumpcond(h2[x], ~b, m);
                    jumpcond(h3[x], b, l);
                    outlab(m);
                    return
                  }
  };

  if op=Or do
  { test sw then { jumpcond(h2[x], b, l);
                   jumpcond(h3[x], b, l);
                   return
                 }
            else { let m = nextlab();
                   jumpcond(h2[x], ~b, m);
                   jumpcond(h3[x], b, l);
                   outlab(m);
                   return
                 }
  };

  load(x);
  test b then outfl(Jt, l)
         else outfl(Jf, l);
  ssp := ssp-1;
  return
}

let load(x) be
{ let op = h1[x];

  if op=Vecap |
     op=Mul   | op=Div | op=Mod | op=Add | op=Sub |
     op=Eq    | op=Ne  | op=Lt  | op=Gt  | op=Le  | op=Ge |
     op=Lsh   | op=Rsh | op=And | op=Or  | op=Xor do
  { load(h2[x]); load(h3[x]); outf(op);
    ssp := ssp-1;
    return
  };

  if op=Ind |
     op=Neg |
     op=Not do
  { load(h2[x]);
    outf(op);
    return
  };

  if op=Lv do
  { loadlv(h2[x]);
    return
  };

  if op=Num do
  { outfn(Ln, h2[x]);
    ssp := ssp+1;
    return
  };

  if op=True do
  { outfn(Ln, -1);
    ssp := ssp+1;
    return
  };

  if op=False do
  { outfn(Ln, 0);
    ssp := ssp+1;
    return
  };
 
  if op=String do
  { let strlab = nextlab();
    outstring(strlab, @h2[x]);
    outfl(Laddr, strlab);
    ssp := ssp+1;
    return
  };

  if op=Name do
  { transname(x, Lp, Ll, Llp, Laddr);
    ssp := ssp+1;
    return
  };

  if op=Valof do
  { let rl = resultlab;
    resultlab := nextlab();
    trcom(h2[x], 0);
    // Note that resultis set res then jumps to resultlab
    outlab(resultlab);
    outfn(Stack, ssp);
    outf(Lres); ssp := ssp+1;
    resultlab := rl;
    return
  };

// Note that if a routine is called res is not set so the result is undefined.
  if op=Fnap do
  { let s = ssp;
    ssp := ssp+3;      // Leave space for old p, ret addr, entry addr
    outfn(Stack, ssp);
    loadlist(h3[x]);    // Load the arguments
    load(h2[x]);        // Load the entry address
    outfn(Fnap, s+1);
    outf(Lres); ssp := s+1; // Cause res to be loaded onto the stack
    return
  };

  if op=Printf |  // Printf does note set res, so the value is undefined.
     op=Sys    do // If Sys sets res the result to be defined.
  { let s = ssp;
    let op = h1[x];
    comline := h3[x];
    loadlist(h2[x]);
    outfn(op, s+1);
    ssp := s;
    outf(Lres);
    ssp := ssp+1;
    return
  };

  trnerr("Compiler error in Load, op=%n", op);
  outfl(Ln, 0);
  ssp := ssp+1;
  return
}

let loadlv(x) be
{ let op = h1[x];

  if op=Name do
  { transname(x, Llp, Laddr, 0, 0);
    ssp := ssp+1;
    return
  };

  if op=Ind do
  { load(h2[x]);
    return
  };

  if op=Vecap do
  { load(h2[x]);
    load(h3[x]);
    outf(Add);
    ssp := ssp-1;
    return
  };

  trnerr("Bad operand to @");
  outf(Lres);
  ssp := ssp+1;
  return
}

let fnbody(x) be
{ let op = h1[x];
                   
  if op=Valof do
  { let e  = dvece;
    let rl = resultlab;
    resultlab := -1;
    trcom(h2[x], -1);
    resultlab := rl;
    undeclare(e);
    return
  };

  load(x);
  outf(Fnrn);
  ssp := ssp-1;
  return
}
 
let loadlist(x) be unless x=0 do
  test h1[x]=Comma
  then { loadlist(h2[x]); loadlist(h3[x]) }
  else load(x)

let assign(x, y) be
{ let op = h1[x];

  if op=Name do
  { load(y);
    transname(x, Sp, Sl, 0, 0);
    ssp := ssp-1;
    return
  };

  if op=Vecap do
  { load(y);
    load(h2[x]);
    load(h3[x]);
    outf(Add);
    ssp := ssp-1;
    outf(Stind);
    ssp := ssp-2;
    return
  };
  
  if op=Ind do
  { load(y);
    load(h2[x]);
    outf(Stind);
    ssp := ssp-2;
    return
  };
  
  trnerr("Bad assignment");
  return
}

let transname(x, p, l, v, a) be
{ let c = cellwithname(x);
  let k = h2[c];
  let n = h3[c];
  let name = @h3[x];
  let op = h1[x];

  if k=Local do
  { outfn(p, n);
    return
  };
 
  if k=Var do
  { outfl(l, n);
    return
  };

  if k=Vec do
  { if v=0 do
    { trnerr("Misuse of local vector '%s'", name);
      v := p
    };
    outfn(v, n);
    return
  };

  if k=Addr do
  { if a=0 do
    { trnerr("Misuse of entry name '%s'", name);
      a := l
    };
    outfl(a, n);
    return
  };

  trnerr("Name '%s' not declared", name);
  return
}
 
let wrf(form, a, b, c, d) be if optCode do writef(form, a, b, c, d)

let outf(op) be
{ wrf("%i5: %s*n", codep, opstr(op));
  putc(op)
}

let outfn(op, a) be
{ wrf("%i5: %s %n*n", codep, opstr(op), a);
  putc(op);
  putc(a)
}

let outfl(op, lab) be
{ wrf("%i5: %s L%n*n", codep, opstr(op), lab);
  putc(op);
  putref(lab)
}

let outlab(lab) be
{ wrf("%i5: Lab L%n*n", codep, lab);
  setlab(lab, codep)
}

let outentry(l1, l2) be
{ wrf("%i5: Entry L%n L%n*n", codep, l1, l2);
  putref(l2);
  setlab(l1, codep)
}

let outstring(lab, str) be
{ let sv = mem+datap;
  let s = datap;
  let len = getbyte(str, 0);  // The BCPL string has its length in byte zero
  wrf("%i5: String L%n %s*n", datap, lab, str);
  setlab(lab, s);     // The VSPL string is at position datap relative
                      // to the start of VSPL memory
  for i = 0 to len do // The VSPL string is zero terminated
  { if i mod bytesperword = 0 do
      putd(0); // Start a new word of bytes. The next byte of the
               // VSPL string will be at position i relative to
	       // the start of the string. The zero terminating byte
	       // is at position len.
	       // putd advances datap every time it is called. It
	       // will be called at least once.
    // assemble a zero terminated string
    test i<len
    then putbyte(sv, i, getbyte(str, i+1))
    else putbyte(sv, i, 0)
  };
//  writef("BCPL string *"%s*" compiles into VSPL location from %n to %n*n",
//          str, s, datap-1)
  if optCode do for i = s to datap-1 do test bytesperword=4
    then writef("%i5: %8x*n",  i, mem[i])
    else writef("%i5: %16x*n", i, mem[i])
}

let outstatvec(lab, a) be
{ wrf("%i5: Statvec L%n size %n*n", datap, lab, a);
  setlab(lab, datap);
  for i = 0 to a-1 do putd(0)
}

let outvar(lab) be
{ wrf("%i5: Var L%n*n", datap, lab);
  setlab(lab, datap);
  putd(0)
}
 
let putc(w) be test codep>codet
               then trnerr("More code space needed")
               else { mem[codep] := w;
                      codep := codep+1
                    }

let putd(w) be test datap>datat
               then trnerr("More data space needed")
               else { mem[datap] := w;
                      datap := datap+1
                    }

let putref(lab) be test codep>codet
                   then trnerr("More code space needed")
                   else { mem[codep] := refv[lab];
                          refv[lab] := codep;
                          codep := codep+1
                        }

let setlab(lab, addr) be labv[lab] := addr

let nextlab() = valof
{ test labnumber>=labmax
  then fatalerr("More label space needed")
  else labnumber := labnumber + 1;
  resultis labnumber
}
 

let resolvelabels() be for lab = 1 to labnumber do
{ let p = refv[lab];
  let labval = labv[lab];
  if p & labval<0 do test lab=1 then trnerr("start not defined")
                                else trnerr("Label %n unset", lab);
  while p do { let np = mem[p];
               mem[p] := labval;
               p      := np
             }
}

let interpret(regs, mem) = valof
{ // reg    address relative to mem of the registers vector
  // mem    pointer to the start of VSPL memory
  // It repeatedly executes instructions until leaveinterpreter
  // is set to true when it returns the error code held in retcode.
  // leaveinterpreter = false;

  let retcode = 0;
  // Possible error codes are:
  // 0      No error
  // 1      Unknown instruction
  // 3      count <= 0
  // value returned by halt
  // value returned by sys(0, code)
  // value returned by recursive call of interpret.
  
  let rv = mem+regs;
  let res   = rv[0];     // The result register
  let pp    = mem+rv[1];  // Absolute VSPL p pointer
  let sp    = mem+rv[2];  // Absolute VSPL stack pointer
  let pc    = mem+rv[3];  // Absolute VSPL program counter
  let count = rv[4];      // The count register

  while true do
  { // Within this loop
    // loop    will resume execution at this point to try to
    //         execute the next instruction.
    // break   will cause interpret to return with error code
    //         retcode after saving the registers in rv.

    let op = pc[0];   // Fetch next instruction

    if optTrace do
    { // Output the current state with addresses relative to the
      // base of VSPL memory (mem).
      writef("p:%i5  sp:%i5 B=%iA A=%iA  %i5: %t8",
              pp-mem,    sp-mem, sp[-1], sp[0], pc-mem, opstr(op));
      if hasOperand(op) do writef(" %n", pc[1]);
      newline()
    };
    
    if count=0 do
    { retcode := 3;
      break   // exit from this while loop.
    };
    // Note a negative count is treated as infinity.

    pc := pc+1;

    if count>0 do count := count-1;
    
    // pc is now the absolute address just after the op byte
    //    of the current instruction. It either points to an
    //    instruction operand or the op code of the next instruction.
    // op is the op code of this instruction.
    // sp points to the top item of the stack.
    // pp points to the base of the current stack frame.
    // pp[0] points to the previous stack frame.
    // pp[1] is the function return address.
    // pp[2] holds the entry address of the current function.

    if sp-stack>stackt do
    { writef("Stack overflow, sp-stack=%n stackt=%n\n",
              sp-stack, stackt);
      //abort(9999);
      loop
    };
    
    if op=Halt do
    { retcode := res;
      break
    };

    if op=Laddr do
    { sp := sp+1;
      sp[0]:= !pc;
      pc := pc+1;
      loop
    };
    
    if op=Ln do
    { sp := sp+1;
      sp[0]:= !pc;
      pc := pc+1;
      loop
    };
    
    if op=Lp do
    { sp := sp+1;
      sp[0] := pp[pc[0]];
      pc := pc+1;
      loop
    };

    if op=Llp do
    { sp := sp+1;
      sp[0] := pp+!pc-mem;
      pc := pc+1;
      loop
    };

    if op=Ll do
    { sp := sp+1;
      sp[0]:= mem[pc[0]];
      pc := pc+1;
      loop
    };
    
    if op=Sp do
    { pp[pc[0]] := sp[0]; sp := sp-1;  pc := pc+1; loop
    };
    if op=Sl do
    { mem[pc[0]]:= sp[0]; sp := sp-1;  pc := pc+1; loop
    };
    if op=Lres do
    { sp := sp+1; sp[0]:= res;                   loop
    };
    if op=Ind do
    { sp[0]:=  mem[sp[0]];                       loop
    };
    if op=Neg do
    { sp[0]:=  -  sp[0];                          loop
    };
    if op=Not do
    { sp[0]:= ~ sp[0];                          loop
    };
    if op=Stind do
    { sp := sp-2; mem[sp[2]] := sp[1];            loop
    };
    if op=Vecap do
    { sp := sp-1; sp[0]:= mem[sp[0]+ sp[1]];     loop
    };
    if op=Mul do
    { sp := sp-1; sp[0]:= sp[0] *  sp[1];         loop
    };
    if op=Div do
    { sp := sp-1; sp[0]:= sp[0] /  sp[1];         loop
    };
    if op=Mod do
    { sp := sp-1; sp[0]:= sp[0] mod sp[1];         loop
    };
    if op=Add do
    { sp := sp-1; sp[0]:= sp[0] +  sp[1];         loop
    };
    if op=Sub do
    { sp := sp-1; sp[0]:= sp[0] -  sp[1];         loop
    };
    if op=Eq do
    { sp := sp-1; sp[0]:= sp[0] =  sp[1];         loop
    };
    if op=Ne do
    { sp := sp-1; sp[0]:= sp[0]~=  sp[1];         loop
    };
    if op=Le do
    { sp := sp-1; sp[0]:= sp[0]<=  sp[1];         loop
    };
    if op=Ge do
    { sp := sp-1; sp[0]:= sp[0]>=  sp[1];         loop
    };
    if op=Lt do
    { sp := sp-1; sp[0]:= sp[0] <  sp[1];         loop
    };
    if op=Gt do
    { sp := sp-1; sp[0]:= sp[0] >  sp[1];         loop
    };
    if op=Lsh do
    { sp := sp-1; sp[0]:= sp[0]<<  sp[1];         loop
    };
    if op=Rsh do
    { sp := sp-1; sp[0]:= sp[0]>>  sp[1];         loop
    };
    if op=And do
    { sp := sp-1; sp[0]:= sp[0] &  sp[1];         loop
    };
    if op=Or do
    { sp := sp-1; sp[0]:= sp[0] |  sp[1];         loop
    };
    if op=Xor do
    { sp := sp-1; sp[0]:= sp[0] ^ sp[1];          loop
    };
    if op=Jt do
    { sp := sp-1;
      pc := pc+1;
      test sp[1] then pc := pc[0]+mem
                 else pc := pc+1;
      loop
    };
    if op=Jf do
    { sp := sp-1;
      pc := pc+1;
      test sp[1] then pc := pc+1
                 else pc := pc[0]+mem;
      loop
    };
    if op=Resultis do
    { sp := sp-1; res := sp[1]; pc := pc[0]+mem;    loop
    };
    if op=Jump do
    { pc := pc[0]+mem;                             loop
    };
    if op=Stack do
    { sp := pp + pc[0]; pc := pc+1;                loop
    };


    if op=Fnrn do
    { let prevpp  = pp[0]+mem;
      let retaddr = pp[1]+mem;
      res := sp[0]; // Place the function result in res
      sp := pp-1;
      pp := prevpp;
      pc := retaddr;
      loop
    };

    if op=Rtrn do
    { let prevpp  = pp[0]+mem;
      let retaddr = pp[1]+mem;
      sp := pp-1;
      pp := prevpp;
      pc := retaddr;
      loop
    };

    if op=Rtap | op=Fnap do
    { let prevpp  = pp;
      let retaddr = pc+1;
      pp := pp+pc[0];
      pc := sp[0]+mem;
      // The first three words of a stack frame hold:
      // the previous p pointer,
      // the return address, and
      // the function entry address.
      // The word before the entry address holds a
      // pointer to the name of the function.
      pp[0] := prevpp -mem;  // The previous p pointer
      pp[1] := retaddr-mem;  // The return address
      pp[2] := pc     -mem;  // The function entry address
      sp := pp+2;
      // The function arguments start at pp+3
      loop
    };

    if op=Printf do
    { // pc[0] is the position in the current stack frame of
      // the printf arguments
      sp := pp + pc[0] - 1; // The value of ss on return
      pc := pc+1;         // The return address
      //sawritef("Printf: pp=%n sp=%n args %n %n %n*n",
      //          pp-mem, sp-mem, sp[1], sp[2], sp!3)
      //abort(1012);
      printf(mem, sp[1], sp+2); // mem, format, the vector of args
      loop
    };

    if op=Sys do
    { // syn(n, arg) is a function call, so if the is a result
      // it is placed in res ready to be accessed by Lres after
      // the return.
      // pc[0] is the position in the current stack frame of
      // the sys arguments [n, args,...]

      sp := pp + pc[0] - 1; // The value of ss on return
      pc := pc+1;           // The return address

      while true do // This is a fake loop which is exited at
                    // many places using break.
      { let n   = sp[1];
        let arg = sp[2];
	
	if n=0 do  // sys(0, retcode)
	{ retcode  := arg;
	  break
	};
	
	if op=1 do // sys(1, regsv, mem) Enter the interpreter
	{ retcode := interpret(sp[2], mem);
	  break
	};
	
        if op=2 do // sys(2, flag) Set tracing on or off
	{ optTrace := arg;
          break
	};
	
        if op=3 do // sys(3, value) Set count returning the previous value
	{ let r = count;
	  count := arg;
          res := r;
	  break
	};

        // The following cases were added on 29 Sep 2025
        // to allow I/O to and from files.

        if op=4 do  // sys(4, filename) -- findinput(filename)
	{ res := sys(4, sp[2]);
	  break
	};

	if op=5 do  // sys(5, filename) -- findoutput(filename)
	{ res := sys(5, sp[2]);
	  break
	};

	if op=6 do   // sys(6, instream) -- selectinput(instream)
	{ sys(6, sp[2]);
          break
	};

	if op=7 do   // sys(7, outstream) // selectoutput(outstream)
	{ sys(7, sp[2]);
          break
	};

        if op=8 do   // sys(8)           // input()
	{ res := sys(8);
	  break
	};

	if op=9 do  // sys(9)            // output()
        { res := sys(9);
	  break
	};

	 if op=10 do // sys(10)          // rdch()
         { res := sys(10);
	   break
	 };

	 if op=11 do // sys(11, ch)      // wrch(ch)
         { sys(11, sp[2]);
           break
	 };

	 if op=12 do // sys(12)          // endread()
         { sys(12);
           break
	 };
	 
	 if op=13 do // sys(13)          // endwrite()
         { sys(13);
           break
	 };

         retcode  := 2;
         break
     
      }; // End of fake while loop dealing with sys..

      // This point is reached when break is
      // encountered in the above fake while loop. It caused
      // interpret to return with result in res.
      loop
    };

    retcode := 1;     // Unknown op code
    break             // Leave the interpreter.

  }; // End of while loop executing vspl instructions.
 
  // This point is reached by break in the execution loop.
  // It causes the interpret to return with result retcode
  // after saving the registers in rv.

  rv[0] := res;
  rv[1] := pp-mem;
  rv[2] := sp-mem;
  rv[3] := pc-mem;
  rv[4] := count;
  resultis retcode
}

let prf(mem, formatstr, p) be
{ let fmt = formatstr + mem;
  let i = 0; // Position of next byte in the format string.
  // The possible substitution items are: % <digits> followed
  // by d, s, x or c.
  // Strings are bytes packed in words terminated by zeroes.

  { let k = getbyte(fmt, i);
    i := i+1;
    if k=0 do return; // End of format string.
    if k='%' do
    { let n = 0;
      k := getbyte(fmt, 0); // Get the first format byte

      while '0'<=k & k<='9' do
      { // Deal with a digit
        n := 10*n + k - '0';
        i := i+1;
	k := getbyte(fmt, i)  // Evaluate the length value, if any.
      };

      // n holds the length if given.

      k := valof
      { if k='d' do{ writed  (p[0],     n); p := p+1; resultis 0};
        if k='s' do{ wrs     (mem+p[0], n); p := p+1; resultis 0};
        if k='x' do{ writehex(p[0],     n); p := p+1; resultis 0};
        if k='c' do{ wrch    (p[0]);        p := p+1; resultis 0};
	wrch(k);                                      resultis 0
      }
    };
    
    wrch(k) // Not a % item.
  }
}

let wrs(s, n) be
{ let len = 0;
  while getbyte(s, len) do len := len+1; // Compute the length of the string.
  for i = len+1 to n do wrch(' '); // Ensure the length of output
                                   // is at least n.
  for i = 0 to len-1 do wrch(getbyte(s, i))
}

let hasOperand(op) = valof
{ if op=Fnrn  |
     op=Rtrn  |
     op=Lres  |
     op=Halt  |
     op=Vecap |
     op=Ind   |
     op=Stind |
     op=Neg   |
     op=Not   |
     op=Mul   |
     op=Div   |
     op=Mod   |
     op=Add   |
     op=Sub   |
     op=Eq    |
     op=Ne    |
     op=Le    |
     op=Ge    |
     op=Lt    |
     op=Gt    |
     op=Lsh   |
     op=Rsh   |
     op=And   |
     op=Or    |
     op=Xor   do resultis false; // The instruction has no operand

  resultis true   // The instruction has an operand
}

