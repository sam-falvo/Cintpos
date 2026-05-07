// TRIPOS library command.  It loads an object module
// and adds its sections to the end of the BLIB
// library chain originating from the rtn_blib field
// in the rootnode.

// It is however potentially dangerous since it can be
// asked to remove sections of code and uninitialising
// their globals. Normally this function is only used
// right at the start of a cintsys or cintpos session
// to modify the resident BLIB library.

// arguments:  FROM,CANCEL/K,OVERRIDE/S,LIST/S,-G/S,TO/K

// FROM     specifies the name of the object module to
//          append to the end of the BLIB libraries.
//          Unless OVERRIDE has been specified the
//          new sections must not have the same names
//          as those previously present.
// CANCEL   specifies a the name of a section to remove
//          from the list of sections.
// OVERRIDE will remove all sections from the BLIB library
//          before appending the FROM object module.
// LIST     will output the list of section names in the
//          BLIB library after all other operations
//          have completed.
// -G       will list sections and their defined global functions
// TO       specifies the name of the output file.
SECTION "LIBRARY"

GET "libhdr"

MANIFEST {
  nameupb  = 11  // The full length of a section name.
  // Note that if the user specified a section name
  // with more that 11 characters, the compiler will
  // replace it by the first and last 5 characters
  // separated by a prime ('). In this program this
  // convertion is performed by convert_section_name(..)
}

LET start() = VALOF
{ LET cancelname = 0
  LET cancelvec  = VEC nameupb/bytesperword
  LET sectname   = VEC nameupb/bytesperword
  LET segment    = 0
  LET stdout = output()
  LET outstream = 0

  LET argv       = VEC 50

  IF rdargs("from,cancel/k,override/s,list/s,-g/s,TO/K", argv, 50) = 0 DO
  { writes("Invalid parameters*n")
    stop(20,0)
  }

  IF argv!5 DO
  { outstream := findoutput(argv!5)
    UNLESS outstream DO
    { writef("ERROR: Cannot open file %s*n", argv!5)
      stop(20, 0)
    }
  }

  IF argv!4 DO argv!3 := TRUE

  IF argv!0 DO                               // FROM
  { segment := loadseg(argv!0)
//sawritef("library: segment=%n*n", segment)

    UNLESS segment DO
    { writef("Unable to load segment *"%s*"*n", argv!0)
      stop(20,0)
    }
  }

  IF argv!1 DO                               // CANCEL/K
  { // Deal with CANCEL parameter

    // Remove a specified named section from the
    // BLIB library.
    LET cancelname = VEC nameupb/bytesperword
    LET sec = 0

    IF segment DO
    { writef("Cannot CANCEL a section if the FROM file is specified*n")
      stop(20, 0)
    }

    // Convert the section name.
    convert_section_name(argv!1, cancelname)
    //writef("Removing section %s*n", cancelname)

    UNLESS remove_section(@rtn_blib!rootnode, cancelname) DO
    { writef("Failed to find section *"%s*" in the BLIB list*n",
             cancelname)
    }

    IF argv!3 DO
      list_section_names(rtn_blib!rootnode, argv!4, outstream)

    RESULTIS 0
  }

//sawritef("Blib = %n*n", rtn_blib!rootnode)

  IF argv!2 DO                              // OVERRIDE/S
  { // OVERRIDE is specified, so remove all sections with
    // names that match those in the FROM file.

    LET seglist  = segment
    LET bliblist = rtn_blib!rootnode

    UNLESS segment DO
    { writef("OVERRIDE specified but no FROM file given*n")
      stop(20, 0)
    }

    WHILE seglist DO
    { // Look at all sections in the loaded module
      LET section = 0
      copy_section_name(seglist+1, sectname)
      remove_section(@rtn_blib!rootnode, sectname)
      seglist := !seglist
    }
  }
//abort(1006)

  IF segment DO
  { // The FROM parameter was given and the module successfully loaded.


    LET seglist  = segment
    LET bliblist = rtn_blib!rootnode

    WHILE seglist DO
    { // Check that none of the sections are already in the BLIB list.
      LET section = ?
      copy_section_name(seglist+1, sectname)
      IF remove_section(@rtn_blib!rootnode, sectname) DO
      { writef("Section %s was already in BLIB list*n", sectname)
      }
      seglist := !seglist
    }

    // None of the sections in the FROM module are in the BLIB list.
    //writef("None of the sections are already in BLIB list*n")


    // Now append the object module's sections to the end of
    // the BLIB library.

//sawritef("library:*
//         * Append the FROM module sections onto the end of the BLIB list*n")

    { LET p = @rtn_blib!rootnode
      UNTIL !p=0 DO p := !p
//sawritef("library: updating the last link field %n with %n*n", p, segment)
      !p := segment
    }

    // Re-initialise the BLIB library globals.
//    writes("Re-initialising the BLIB globals*n")

    UNLESS globin(rtn_blib!rootnode) DO
    { writes("Unable to re-initialise the BLIB globals*n")
      stop(20,0)
    }
  }

  IF argv!3 DO                               // LIST/S
    list_section_names(rtn_blib!rootnode, argv!4, outstream)

  RESULTIS 0
}

AND findptr(chainptr, hunk) = VALOF
{ // If found, return the address of the word that
  // points to hunk, otherwise return 0.

  { LET p = !chainptr
sawritef("findptr: chainptr=%n p=%n hunk=%n*n", chainptr, p, hunk)
abort(1005)
    IF p = hunk RESULTIS chainptr
    IF p = 0    RESULTIS 0
    chainptr := !chainptr
  } REPEAT
}

AND convert_section_name(name, v) BE
{ // Expand or contract the section name to length 11
  LET len = name%0
  v%0 := 11
  FOR j = 1 TO 11 DO v%j := ' '
  TEST len<=11
  THEN { FOR j = 1 TO len DO v%j := name%j
       }
  ELSE { // Replace over long name by first and
         // last 5 characters separated by a prime.
         FOR j = 1 TO 5 DO v%j := name%j
         v%6 := '*''
         FOR j = 7 TO 11 DO v%j := name%(len+j-11) 
       } 
}

AND copy_section_name(hunk, v) = VALOF
// Returns TRUE if there is a valid section name,
// and if so it copies the section name into v.
{ LET size = hunk!0
//sawritef("copy_section_name: hunk=%n [%n %x8 %x8 %x8]*n",
//          hunk, hunk!0, hunk!1, hunk!2, hunk!3)
  IF size >= 11 & hunk!1 = sectword & (hunk+2)%0 = 11 DO
  { LET name = @hunk!2
    FOR i = 0 TO name%0 DO v%i := name%i
//sawritef("copy_section_name: %s copied*n", v)
    RESULTIS TRUE
  }
//sawritef("copy_section_name: no section name found*n")
  RESULTIS FALSE
}

AND remove_section(listptr, name) = VALOF
{ // This searches the given list of sections for
  // sections with the given name. Each one found
  // is unlinked it from the list, has it globals unset,
  // and is returned to free store.

  // !listptr -> [next, size, <body of hunk>]
  LET found = FALSE
  LET v = VEC nameupb/bytesperword

  WHILE !listptr DO
  { LET section = !listptr
//sawritef("remove_section: listptr=%i5 -> [%n,%n,%x8...] %s*n",
//          listptr, section!0, section!1, section!2, @section!3)
    copy_section_name(section+1, v)
//sawritef("remove_section: comparing %s with %s*n", v, name)
    TEST compstring(v, name) = 0
    THEN { // Matching section found, so remove it.
//sawritef("remove_section: the name %s matches*n", name)
           found := TRUE
           !listptr := !section
           !section := 0
//sawritef("remove_section: calling unglobin(%n)*n", section)
//abort(1007)
           unglobin(section)
//sawritef("remove_section: calling freevec(%n)*n", section)
//abort(1008)
           unloadseg(section)
         }
    ELSE { //sawritef("remove_section: the names no not match*n")
           listptr := !listptr
         }
  }
  RESULTIS found
}

AND list_section_names(list, globs, tostream) BE
{ // This output a list of all section names found
  // in the specified library list.
  LET layout = 0
  LET name = VEC nameupb/bytesperword

  LET stdout = output()
  IF tostream DO selectoutput(tostream)

  WHILE list DO
  { IF copy_section_name(list+1, name) TEST globs
    THEN { LET base = list+1
           LET p = base + !base - 1 // Last word of the section
           { p := p-2
             IF p!1=0 BREAK
             writef("G%i3:  ", p!0)
             writearg((base<<B2Wsh) + p!1)
             writef("  %s", name) 
             newline()
           } REPEAT
         }
    ELSE { layout := layout+1 MOD 5
           IF layout=0 DO newline()
           writef(" %s", name) 
         }
    list := !list
  }
  newline()

  IF tostream DO endstream(tostream)
  selectoutput(stdout)
}

AND unglobin(section) BE
{ // This will uninitialise all the globals of the
  // given section of code.
  LET base = section+1
  LET size = base!0 // Size of the hunk
  LET p    = @base!size - 1 // Pointer to the last word of the hunk
  LET gv   = @globsize
  LET gn, reladdr = 0, 0
//sawritef("unglobin: base %n -> [%n,%x8] %s*n", base, base!0, base!1, @base!2)
//sawritef("unglobin: pointer to last word p=%n -> [%n] highest refd global*n",
//          p, !p)
//abort(1000)

  { p := p-2
    gn      := p!0
    reladdr := p!1 // Relative byte address
    UNLESS reladdr BREAK
//sawritef("unglobin: Unsetting global %i3 reladdr=%n*n", gn, reladdr)
//abort(1002)
    IF gv!gn = (base<<B2Wsh)+reladdr DO
    { //abort(1001)
      gv!gn := globword+gn
    }
  } REPEAT
//sawritef("unglobin: Done*n")
}

AND writearg(n) BE
{ LET name = fname(n)
  TEST bitsperword=32
  THEN // Write value in a field width of 21
       TEST name
       THEN { LET len = name%0
              WHILE len>0 & name%len=' ' DO len := len-1
              FOR i = len+1 TO 13 DO wrch(' ')
              FOR i = 1 TO len DO wrch(name%i)  // MR 17/11/06
            }
       ELSE TEST -10000000<=n<=10000000
            THEN writef("  %iB", n)
            ELSE writef("   #x%x8", n)
  ELSE // Write value in a field width of 21
       TEST name
       THEN { LET len = name%0
              WHILE len>0 & name%len=' ' DO len := len-1
              FOR i = len+1 TO 21 DO wrch(' ')
              FOR i = 1 TO len DO wrch(name%i)  // MR 17/11/06
            }
       ELSE TEST -10000000<=n<=10000000
            THEN writef("  %iJ", n)
            ELSE writef("   #x%xG", n)
}

AND fname(f) = VALOF
{ LET nameoffset = bitsperword=32 -> 3, 2
  LET membase, memlim = 0, 4_000_000
  LET n = (f>>B2Wsh) - nameoffset
  UNLESS (f&(bytesperword-1))=0 &
         membase+2<n<=memlim RESULTIS 0 // MR 25/9/03
  IF n!-1=entryword & n%0=11 RESULTIS n 
  RESULTIS 0
}

