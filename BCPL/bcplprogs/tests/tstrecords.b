GET "libhdr"

MANIFEST {
  reclen = 50
}

GLOBAL {
  stdin:ug
  stdout
  filename
  filestream
  newfile
  rec
  recno
  maxrecno
}

LET start() = VALOF
{ LET argv = VEC 50
  LET pos = 0
  LET recv = VEC (reclen-1)/bytesperword // reclen is known > 0
  rec := recv

  stdin := input()
  stdout := output()

  UNLESS rdargs("file,size/N,new/S", argv, 50) DO
  { writef("*nBad arguments for tstrecords*n")
    RESULTIS 0
  }

  filename := "junkrecs"
  maxrecno := 9                       // Default of 10 records

  IF argv!0 DO filename := argv!1     // file
  IF argv!1 DO maxrecno := !(argv!1)  // size/N
  newfile := argv!2                   // new/S

  
  // Try to delete the file if new is specified
  IF newfile DO deletefile(filename)

  filestream := findinoutput(filename)

  UNLESS filestream DO
  { writef("*nTrouble with file %s*n", filename)
    GOTO fin
  }

  { LET filesize = sys(Sys_filesize, filestream!scb_fd)
    writef("File %s has length of %n byte%-%ps*n", filename, filesize)
  }
  
  setrecordlength(filestream, reclen)

  FOR i = 0 TO reclen-1 DO rec%i := i

  FOR recno = 0 TO maxrecno DO put_record(rec, recno, filestream)

  writef("Next record position is %n*n", recordnote(filestream))

  // You can write a record just after the last one but not
  // beyond that.
  FOR i = 0 TO reclen-1 DO rec%i := #xFF

  put_record(rec, recordnote(filestream), filestream)

  writef("Next record position is %n*n", recordnote(filestream))


fin:
  IF filestream DO endstream(filestream)
  selectinput(stdin)
  selectoutput(stdout)
  writef("End of test*n") 
  RESULTIS 0
}
