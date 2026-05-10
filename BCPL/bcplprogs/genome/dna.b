GET "libhdr"

/*
The genome is a sequence of the letters A, C, G and T. These are encoded
as 0, 1, 2 and 3, or in binary 00, 01, 10 and 11.
*/

GLOBAL {
  stdin:ug
  stdout
  fromfilename
  fromstream
  tofilename
  tostream
  tracing
  datav        // Each element holds 16 or 32 letters encoded as 00, 01, 10
               // and 11 in big ended order. This vector will hold the
	       //
  indexv       // The elements of this vector are subscripts of datav and
               // will be sorted to give a list of all substrings of the
	       // sequence in alphabetical order.

  // Typically within the program local variables
  // x, y and z will be subscripts of datav representing strings of
  //            dna letters,
  // a, b and c will be elements of datav,
  // p, q and r will be subscripts of indexv.
}

LET start() = VALOF
{ LET len = 0
  LET argv = VEC 50

  stdout := output()
  stdin  := input()

  fromfilename := 0
  fromstream := 0
  tofilename := 0
  tostream := 0
  datav := 0
  indexv := 0
  
  UNLESS rdargs("from,to/K,-t/S", argv, 50) DO
  { writef("Bad arguments for dnalen*n")
    GOTO ret
  }
  
  fromfilename := argv!0                                 // from
  UNLESS fromfilename DO fromfilename := "genome000192"
  tofilename := argv!1                                   // to/K
  tracing := argv!2                                      // -t/S
  
  fromstream := findinput(fromfilename)
  UNLESS fromstream DO
  { writef("Trouble with file: %s*n*n", fromfilename)
    GOTO ret
  }

  selectinput(fromstream)

  { LET ch = capitalch(rdch())
    SWITCHON ch INTO
    { DEFAULT:          LOOP
    
      CASE endstreamch: BREAK
      
      CASE 'A':
      CASE 'C':
      CASE 'G':
      CASE 'T':         len := len+1
    }
  } REPEAT
  
  rewindstream(fromstream)

  writef("Length of the DNA sequence is %n*n", len)

  datav  := getvec(len+256) // Room for 256 As at the end.
  indexv := getvec(len)
  UNLESS datav & indexv DO
  { writef("More space needed*n")
    GOTO ret
  }
  
  len := 0

  // Copy the genome sequence into datav.
  { LET ch = capitalch(rdch())
    SWITCHON ch INTO
    { DEFAULT:          LOOP
    
      CASE endstreamch: BREAK
      
      CASE 'A': len := len+1; datav!len := #b00; LOOP 
      CASE 'C': len := len+1; datav!len := #b01; LOOP 
      CASE 'G': len := len+1; datav!len := #b10; LOOP 
      CASE 'T': len := len+1; datav!len := #b11; LOOP 
    }
  } REPEAT
  
  endstream(fromstream)
  fromstream := 0

  FOR i = len+1 TO len+256 DO datav!i := #b00 // Pad with As
  
  // Pack 16 consective letters in each element of datav in
  // bigender order.
  { LET a = 0
    FOR i = len+256 TO 1 BY -1 DO
    { a := datav!i<<30 | a>>2
      datav!i := a
    }
  }
  
  // Note that the first letter of the genome sequence is
  // held in datav!1.

  // Initialise the index vector.
  FOR i = 0 TO len DO indexv!i := i

  //len := 20 // #### As a debugging aid to test qsort. ###
  
  //writef("*nData before sorting*n")
  //prdata(1, len)

  //FOR i = 1 TO len DO gt(indexv!i, indexv!(i+1))
  //GOTO ret
  
  qsort(1, len)

  //writef("*nData after sorting*n")

  IF tofilename DO tostream := findoutput(tofilename)
  IF tostream   DO selectoutput(tostream)
  
  prdata(1, len)
  endstream(tostream)
  tostream := 0
  
  selectoutput(stdout)
  writef("End of output*n")
  
ret:
  IF fromstream DO endstream(fromstream)
  IF tostream   DO endstream(tostream)
  IF datav      DO freevec(datav)
  IF indexv     DO freevec(indexv)
  RESULTIS 0
}

AND wrletters(w) BE
{ wrch(' ')
  FOR sh = 30 TO 0 BY -2 SWITCHON w>>sh & #b11 INTO
  { CASE #b00: wrch('A'); LOOP
    CASE #b01: wrch('C'); LOOP
    CASE #b10: wrch('G'); LOOP
    CASE #b11: wrch('T'); LOOP
  }
}

AND prseq(x) BE
{ // x is a subscript of datav.
  // Write 64 dna letters from position x.
  writef("%7i: ", x)
  wrletters(datav!x)
  wrletters(datav!(x+16))
  wrletters(datav!(x+32))
  wrletters(datav!(x+48))
  newline()
  //IF i MOD 10 = 0 DO abort(1000)
}

AND prdata(p, q) BE
{ // p and q are subscripts of indexv.
  FOR i = p TO q DO
  { prseq(indexv!i)
    //IF i MOD 10 = 0 DO abort(1000)
  }
}

AND qsort(p, q) BE
{ // p and q are pointers to elements of indexv.
  // The elements of indexv are subscripts of datav
  // representing strings of dna letters starting from
  // different positions in the genome sequence.
  
  //writef("qsort: entered %i6 %i6*n", p, q)
  //abort(5678)

  WHILE p+8<q DO
  { LET midpt = (p+q)/2
    // Select a good(ish) median value.
    LET median   = middle(indexv!p, indexv!midpt, indexv!q)
    LET i = partition(median, p, q)
    //writef("qsort: p=%i6 i=%i6 q=%i6*n", p, i, q)
    // Only use recursion on the smaller partition.
    TEST i > midpt
    THEN { qsort(i, q);   q := i-1 }
    ELSE { qsort(p, i-1); p := i   }
    //qsort(p, i-1); qsort(i, q);   RETURN
  }
  
  //writef("insertion sort*n")
  //abort(8888)
  
  FOR i = p+1 TO q DO          // Now perform insertion sort.
  { FOR j = i-1 TO p BY -1 DO
    { LET x, y = indexv!j, indexv!(j+1)
      //writef("comparing the following pair*n")
      //prseq(x)
      //prseq(y)
      //writef("y>x = %n*n", gt(y,x))
      IF gt(y, x) BREAK
      //writef("swap*n")
      indexv!j, indexv!(j+1) := y, x  // Swap the elements of indexv
    }
    UNLESS sorted(p, i) DO
    { writef("ERROR: data not sorted p=%n i=%n*n", p, i)
      prdata(p, i)
      newline()
      abort(999)
    }
  }
}

AND sorted(p, q)  = VALOF
{ LET res = sorted1(p,q)
  //writef("sorted: p=%n q=%n => %n*n", p, q, res)
  RESULTIS res
}

AND sorted1(p, q)  = VALOF
{ FOR i = p TO q-1 IF gt(indexv!i, indexv!(i+1)) RESULTIS FALSE
  RESULTIS TRUE
}

AND middle(x, y, z) = VALOF
{ // x, y and z are subscripts of datav
  LET res = middle1(x,y,z)
  //writef("=>*n")
  //prseq(res)
  //newline()
  //abort(2345)
  RESULTIS res
}

AND middle1(x, y, z) = VALOF
{ // x, y and z are subscripts datav.
  // It returns the middle element of x, y and z.
  //writef("middle of the following:*n")
  //prseq(x)
  //prseq(y)
  //prseq(z)
  RESULTIS gt(y,x) -> gt(z,y) -> y,            // x< y,  y< z
                                 gt(z,x) -> z, // x< y,  y>=z,  x< z
                                            x, // x< y,  y>=z,  x>=z
                      gt(z,y) -> gt(z,x) -> x, // x>=y,  y< z,  x< z
                                            z, // x>=y,  y< z,  x>=z
                                 y             // x>=y,  y>=z
}

AND partition(median, p, q) = VALOF
{ LET res = ?
  //writef("partition: p=%n q=%n  The median is:*n", p, q)
  //prseq(median)
  //newline()
  //prdata(p, q)
  res := partition1(median, p, q)
  //writef("partition: res=%n*n", res)
  //prdata(p, q)
  //newline()
  RESULTIS res
}
  
AND partition1(median, p, q) = VALOF
{ // median is a position in datav.
  // p and q are subscripts of indexv.
  // indexv!p and indexv!q are positions in datav
  LET t = ?
  //writef("partition: median=%n p=%n q=%n*n", median, p, q)
  //abort(8765)
  WHILE gt(median, indexv!p) DO
  { p := p+1
    //writef("partition: inc p=%n*n", p)
  }
  // indexv!p is ge median
  WHILE gt(indexv!q, median) DO
  { q := q-1
    //writef("partition: dec q=%n*n", q)
  }
  // indexv!q is le median
  IF p >= q DO
  { //writef("partition: partition point p=%n, q=%n*n", p, q)
    //abort(9999)
  }
  IF p >= q RESULTIS p
  //writef("partition: swapping p=%n q=%n*n", p, q)
  //abort(3456)
  { LET t  = indexv!p
    indexv!p := indexv!q
    indexv!q := t
  }
  //prdata(p, q)
  //abort(7777)
  p, q := p+1, q-1
} REPEAT

AND gt(x, y) = VALOF
{ // Compare dna sequences starting at positions x and y
  // of the genome sequence.
  LET res = gt1(x, y)
  //writef(res -> "=> TRUE*n", "=> FALSE*n")
  //abort(1234)
  RESULTIS res
}

AND gt1(x, y) = VALOF
{ // Compare dna sequences of length up to 64 letters starting
  // at positions x and y of the genome sequence.
  //writef("gt:*n")
  //prseq(x)
  //prseq(y)
  //abort(1233)
  
  IF x=y RESULTIS FALSE

  FOR i = 0 TO 48 BY 16 DO
  { LET a = datav!(x+i)
    AND b = datav!(y+i)
    //writef("gt x=%n y=%n i=%2i %32b %32b*n", x, y, i, a, b)
    //abort(7621)
    IF a=b LOOP
    TEST a>=0
    THEN TEST b>=0
         THEN RESULTIS a-b>0     // a is +ve  b is +ve
	 ELSE RESULTIS FALSE     // a is +ve  b is -ve
    ELSE TEST b>=0
         THEN RESULTIS TRUE      // a is -ve  b is +ve
	 ELSE { //writef("%32b > %32b = %n*n", a, b, a>b)
	        RESULTIS a>b     // a is -ve  b is -ve
	      }
  }
  RESULTIS FALSE
}

AND ge(x, y) = VALOF
{ // Compare strings of up to 64 letters.
  IF x=y RESULTIS TRUE
  FOR i = 0 TO 48 BY 16 DO
  { LET a = datav!(x+i)
    AND b = datav!(y+i)
    //writef("ge x=%n y=%n i=%2i %32b %32b*n", x, y, i, a, b)
    IF a=b LOOP
    TEST a>=0
    THEN TEST b>=0
         THEN RESULTIS a>b
	 ELSE RESULTIS TRUE
    ELSE TEST b>=0
         THEN RESULTIS FALSE
	 ELSE RESULTIS a>b
  }
  RESULTIS TRUE
}

