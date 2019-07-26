/*
   This is a demonstration tautology checker for expressions
   containing no more than 32 variables given as a set of
   conjunctive normal form terms. Typical data is as follows:

   (1 3 -5)
   (1 3 9)
   (1 3 -10)
   (1 -3 9)
   (1 4 -7)
   (1 -4 -5)
   (1 -5 8)
   ...
   (5 7 -9)
   (-5 7 8)
   (-5 9 -10)
   (6 -8 9)
   (-6 -7 -9)
   (-6 8 -9)
*/

GET "mcpl.h"

STATIC
  stackv, stackp, stackt, varcount, trycount=0,
  p2 = VEC 16, n2 = VEC 16, p3 = VEC 16, n3 = VEC 16

FUN bits : 0 => 0
         : w => 1 + bits( w&(w-1))

FUN start : =>
//LET filename = "../cnfdata"
//LET filename = "../satdata/3-sat-10-43.1"
//LET filename = "../satdata/3-sat-10-43.2"
//LET filename = "../satdata/3-sat-20-86.1"
//LET filename = "../satdata/3-sat-20-86.2"
//LET filename = "../satdata/3-sat-30-129.1"
//LET filename = "../satdata/3-sat-30-129.2"
  LET filename = "../satdata/5-sat-32-631"

  LET argv = VEC 50

  UNLESS rdargs("DATA,TO/K", argv,50) DO
  { writef "Bad arguments for SAT\n"
    RETURN 20
  }

  IF argv!0 DO filename := argv!0

  stackv := getvec 500000
  stackp := stackv
  stackt := @stackv!500000

  IF argv!1 DO selectoutput(findoutput(argv!1))

  IF readterms filename DO
  { writef("Solving SAT problem: %s\n", filename)
    writef("It has %d variables and %d terms\n\n",
                   varcount,        (stackp-stackv)/(2*Bpw))
    try(stackv, stackp, 0, 0)
    writef("Try count = %d\n", trycount)
  }
  IF argv!1 DO endwrite()
  freevec stackv
  RETURN 0

// readterms reads a file representing a cnf expression.
// Typical data is as follows:
//         (1 -2) (1 2 3) (-1 -2 3)

FUN readterms : filename =>
  LET stdin   = input()
  LET data    = findinput filename
  LET ch=0, all

  IF data=0 DO { writef("Can't find file: %s\n", filename)
                 RETURN FALSE
               }

  selectinput data

  { // Skip to start of next term (if any).
    ch := rdch() REPEATUNTIL ch='(' OR ch=Endstreamch

    IF ch=Endstreamch DO { endread()
                           selectinput stdin
                           varcount := bits all
                           RETURN TRUE
                         }

    LET pterm=0, nterm=0

    { LET var = readn()      // Read a variable.
      MATCH var
      :      0    => BREAK   // No more variables in this term.
      :   1 .. 32 => pterm |:= 1<<( var-1)
      : -32 .. -1 => nterm |:= 1<<(-var-1)
      :      ?    => writef("Var %4d out of range\n", var)
    } REPEAT

    // Test the term for validity.
    UNLESS pterm|nterm DO writef "An empty term found\n"
    IF     pterm&nterm DO writef "A tautologous term found\n"

    all |:= pterm|nterm

    !stackp+++ := pterm // Insert into the term stack
    !stackp+++ := nterm
  } REPEAT

FUN filter : p, q, tset, fset =>
  UNTIL p>=q DO
  { LET pterm = !p+++  // Get a term
    LET nterm = !p+++

    // If it is unsatisfied push it onto the term stack.

    UNLESS pterm&tset OR nterm&fset DO { !stackp+++ := pterm
                                         !stackp+++ := nterm
                                       }
  }

FUN try : p, q, tset, fset =>

// p       points to the first term
// q       points to just beyond the last
// tset    variables currently set true
// fset    variables currently set false

  trycount++

  LET t, tcount, count
  LET pterm, nterm, avail, tposs, fposs

//writef "looking for singletons\n"
  // Scan for empty or singleton terms
  { t, tcount, count := p, 0, 0
    UNTIL t>=q DO
    { pterm := !t+++
      nterm := !t+++
/*
      writef("\nTerm %d\n", (t-p)/8)
      writef("tset:  %32b\n", tset)
      writef("fset:  %32b\n", fset)
      writef("pterm: %32b\n", pterm)
      writef("nterm: %32b\n", nterm)
*/
      IF pterm&tset OR nterm&fset LOOP // Term already satisfied.

      avail := ~(tset|fset)
      tposs := pterm & avail // Remaining pos vars in this term.
      fposs := nterm & avail // Remaining neg vars in this term.
/*
      writef("avail: %32b\n", avail)
      writef("tposs: %32b\n", tposs)
      writef("fposs: %32b  bits=%d\n", fposs, bits(tposs|fposs))
*/
      LET vars = tposs|fposs
      IF vars=0 RETURN   // An empty term can't be satified.
      vars &:= vars-1    // Remove one variable.
      TEST vars=0 THEN { tcount++      // A singleton term found.
                         tset |:= tposs    
                         fset |:= fposs
                       }
                  ELSE count++         // A larger term found.
    }
  } REPEATWHILE tcount    // repeat until no singletons found.

//writef("New %3d terms from %3d\n", count, (q-p)/(2*Bpw))

  UNLESS count DO { writef("%7d: Solution found:\n", trycount)
                    prterm(tset, fset)
                    newline()
                    RETURN
                  }

  LET s = stackp

  IF count < (q-p)*2/(2*Bpw*3) DO // Filter if less than 2/3 remain.
  { filter(p, q, tset, fset)
    p, q := s, stackp
  }

  FOR n = 0 TO 16 DO p2!n, n2!n, p3!n, n3!n ALL:= 0

  t := p

//writef "\nscanning for doublets and larger terms\n"

  // Scan for doublet or larger terms
  UNTIL t>=q DO
  { pterm := !t+++
    nterm := !t+++
/*
    writef("\nTerm %d\n", (t-p)/(2*Bpw))
    writef("tset:  %32b\n", tset)
    writef("fset:  %32b\n", fset)
    writef("pterm: %32b\n", pterm)
    writef("nterm: %32b\n", nterm)
*/
    IF pterm&tset OR nterm&fset LOOP // Term already satisfied

    avail := ~(tset|fset)
    tposs := pterm & avail // remaining pos vars in this term
    fposs := nterm & avail // remaining neg vars in this term

/*
    writef("avail: %32b\n", avail)
    writef("tposs: %32b\n", tposs)
    writef("fposs: %32b  bits=%d\n", fposs, bits(tposs|fposs))
*/
    TEST bits(tposs|fposs) = 2 // Is the term a doublet or larger?
    THEN { inc(p2, tposs); inc(n2, fposs) } // A doublet
    ELSE { inc(p3, tposs); inc(n3, fposs) } // A larger term
  }

  LET pv=p2, nv=n2, k=16

  UNTIL pv!k OR nv!k OR k<0 DO k--      // Search the doublet counts

  IF k<0 DO { pv, nv, k := p3, n3, 16
              UNTIL pv!k OR nv!k DO k-- // Search the larger terms
            }
  // pv!k ~= 0 or nv!k ~= 0 or both.

  // Find variable(s) with maximal count (at least one exists).
  LET pbit=pv!k, nbit=nv!k
  UNTIL --k<0 DO { LET pw=pbit & pv!k, nw=nbit & nv!k
                   IF pw|nw DO pbit, nbit := pw, nw
                 }

  TEST pbit
  THEN { pbit &:= -pbit             // Choose just one variable
         try(p, q, tset+pbit, fset) // Try setting it to set true
         try(p, q, tset, fset+pbit) // Try setting it to set false
       }
  ELSE { nbit &:= -nbit             // Choose just one variable
         try(p, q, tset, fset+nbit) // Try setting it to set false
         try(p, q, tset+nbit, fset) // Try setting it to set true
       }

  stackp := s

FUN prterm : tset, fset => // Print the setting, eg:
                           // 2 -3 5 6 -11
  LET i = 0
  WHILE tset|fset DO { i++
                       IF tset&1 DO writef(" %d", i)
                       IF fset&1 DO writef(" %d", -i)
                       tset, fset >>:= 1, 1
                     }

FUN inc : p, w => WHILE w DO { !p, w := !p XOR w, !p & w; p+++ }

FUN val : p, n, bit => LET res = 0
                       UNTIL n<0 TEST bit & p!n--
                                 THEN res := 2*res + 1
                                 ELSE res := 2*res
                       RETURN res


