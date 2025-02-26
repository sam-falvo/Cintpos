|| (C) Copyright 1979 Tripos Research Group
||     University of Cambridge
||     Computer Laboratory

|| Disc Editor for TRIPOS
|| For version 3 of the filing system (Jan 1979)
|| Version for high capacity and floppy discs ( default dp0-0: )

GET "LIBHDR"
GET "IOHDR"
GET "FH3manifests"


GLOBAL
    $(
    lbn              : ug + 0
    cyl              : ug + 1
    sur              : ug + 2
    sec              : ug + 3
    blockbuff        : ug + 5
    blockwrittenback : ug + 6
    ch               : ug + 7
    style            : ug + 8
    stringstyle      : ug + 9
    mask             : ug + 10
    pattern          : ug + 11
    charstyle        : ug + 12
    lbnbase          : ug + 13
    drive            : ug + 14
    nullnumber       : ug + 15
    cylbase          : ug + 16
    block.read       : ug + 17
    write.protected  : ug + 18
    readvalue       : ug + 19
    check.block.read: ug + 20
    readdiscaddr    : ug + 21
    discaction      : ug + 22
    convertdiscaddr : ug + 23
    skiplayout      : ug + 24
    readchar        : ug + 25
    block.checksum  : ug + 26
    printloc        : ug + 27
    disc.devid      : ug + 28
    setdevicetype   : ug + 29
    datseg          : ug + 30

    // Disc description globals

//  N.cylindersperdisc          : ug + 35                       //RS130681
    N.blockspertrack            : ug + 36
    N.sectorsperblock           : ug + 37
    Sector.Origin               : ug + 38
    N.surfaces                  : ug + 39
    LowerCylinder               : ug + 40
    rootkey                     : ug + 41
    Size.Block                  : ug + 42
    size.hashtable              : ug + 43
    b.file.secondarytype        : ug + 44
    b.file.infobits             : ug + 45
    b.file.parentdir            : ug + 46
    b.file.hashchain            : ug + 47
    b.file.filename             : ug + 48
    b.file.creationdate         : ug + 49
    ndrives                     : ug + 50
    maxcyl                      : ug + 51
    nblocks                     : ug + 52
    dev.type                    : ug + 53
    ncyls                       : ug + 54
    b.file.infovec              : ug + 55
    $)

MANIFEST
    $(
    escbit = #X100
    filenameupb = 29
    $)




..
SECTION "Disced1"

GET ""


LET start(zero.or.packet) BE
    $(
    LET match = ?
    LET temp = 0                                                //RS130681
    LET currword = 0
    LET ntype = disctype.big                                    //RS130681
    LET partition = 0                                           //RS130681

    TEST zero.or.packet \= 0 THEN
      $( qpkt(zero.or.packet)
         initio()
         selectinput(findinput("**"))
         selectoutput(findoutput("**"))
         ntype := disctype.floppy
         datseg := 0
      $)
    ELSE
      $( LET argv = VEC 10

         IF rdargs ( "type/k", argv, 10 ) = 0 THEN
         $(
            writes ( "bad arguments*N" ) ; stop ( 20 )
         $)
         UNLESS argv!0=0 DO                                     //RS130681
         TEST compstring(argv!0,"F")=0                          //RS130681
         THEN ntype := disctype.floppy
         ELSE TEST compstring ( argv!0, "P" ) = 0
         THEN ntype := disctype.big
         ELSE TEST compstring ( argv!0, "PB") = 0               //RS130681
         THEN $( partition := -1                                //RS130681
                 ntype := disctype.big $)                       //RS130681
         ELSE TEST compstring ( argv!0, "P0") = 0               //RS130681
         THEN $( partition := 0                                 //RS130681
                 ntype := disctype.big $)                       //RS130681
         ELSE TEST compstring ( argv!0, "P1") = 0               //RS130681
         THEN $( partition := 1                                 //RS130681
                 ntype := disctype.big $)                       //RS130681
         ELSE TEST compstring ( argv!0, "P2") = 0               //RS130681
         THEN $( partition := 2                                 //RS130681
                 ntype := disctype.big $)                       //RS130681
         ELSE TEST compstring ( argv!0, "P3") = 0               //RS130681
         THEN $( partition := 3                                 //RS130681
                 ntype := disctype.big $)                       //RS130681
         ELSE TEST compstring ( argv!0, "K" ) = 0
         THEN ntype := disctype.cart
         ELSE $( writes ( "Bad disc type*N" ) ; stop ( 20 ) $)

         datseg := loadseg ( "sys:l.dat-to-strings" )

         TEST datseg = 0
         THEN writes ( "****** warning - unable to load date segment*N" )
         ELSE globin ( datseg )
      $)

    writes("TRIPOS Disc Editor version 3.0*N")

    setdevicetype (ntype, partition)                            //RS130681

    writes("# *E")

    $( || Main loop
    ch := rdch()

    SWITCHON capitalch(ch) INTO
        $(
        CASE '*E': newline()
        CASE '*N': writes("# *E")
                   ENDCASE

        CASE '*S': ENDCASE

        CASE 'B':  || Reset logical block
                   || number base
                   temp := readvalue()

                   UNLESS 0 <= temp < nblocks
                   THEN
                       $(
                       writes("****** warning - silly value %N*N", temp)
                       ENDCASE
                       $)

                   lbnbase := temp
                   writef(" Logical block number *
                     *base set to %N*N", lbnbase)
                   ENDCASE

        CASE 'C':  || Print n characters
                   check.block.read()
                   writes(" '")

                   FOR j = 0 TO readvalue() - 1
                   DO wrch(getbyte(blockbuff+currword,j))

                   writes("'*N")
                   ENDCASE

        CASE 'D':  || Set the disc drive number
                   temp := readvalue()
                   UNLESS 0 <= temp < ndrives
                   THEN
                       $(
                       writef("****** warning - silly drive number %N*N", temp)
                       ENDCASE
                       $)

                   drive := temp
                   writef(" Drive %N selected*N",drive)
                   ENDCASE

        CASE 'F':  || set the device type

                   SWITCHON capitalch ( rdch () ) INTO
                   $( CASE 'F' : ntype := disctype.floppy
                                 ENDCASE
                      CASE 'P' : ntype := disctype.big
                                 temp := capitalch ( rdch () )  //RS130681
                                 IF temp = 'B' partition := -1  //RS130681
                                 IF '0'<=temp<='3'              //RS130681
                                    partition := temp - '0'     //RS130681
                                 ENDCASE
                      CASE 'K' : ntype := disctype.cart
                                 ENDCASE
                      CASE '*N': writes ( "reseting parameters*N" )
                                 ENDCASE
                      DEFAULT  : writes ( "****** warning - unrecognised*N" )
                                 GOTO skip.reset
                   $)
                   setdevicetype (ntype, partition)             //RS130681
                   skip.reset:
                   UNTIL temp = '*N' DO temp := readchar ()
                   ENDCASE

        CASE 'G':  || Get block from disc
                   $(
                   LET oldlbn = lbn
                   LET addrok = readdiscaddr()
                   LET newlbn = lbn

                   UNLESS blockwrittenback | NOT block.read
                   THEN
                   $(
        writes("****** warning - last block not written back!!*N*
        *******         - type N if you do not want it updated *E")

                       UNTIL rdch()='*N' LOOP
                       temp := rdch()

                       UNLESS (temp='N') | (temp='n')
                       THEN
                           $(
                           unrdch()
                           lbn := oldlbn
                           convertdiscaddr(oldlbn+lbnbase)
                           UNLESS discaction(act.write)
                           THEN ENDCASE
                           lbn := newlbn
                           convertdiscaddr(newlbn+lbnbase)
                           $)
                       $)

                   IF addrok
                   THEN discaction(act.read)
                   $)
                   ENDCASE

        CASE 'H':  || Calculate hash value given
                   || string
                   $(
                   LET s = VEC filenameupb+1
                   LET p = 0
                   LET h = 0

                   skiplayout()
                   ch := rdch()
                   IF ch = '"' THEN ch := readchar()

                   UNTIL (ch = '*N') | (ch = '"') |
                         (ch = ' ') | (p=filenameupb)
                   DO
                       $(
                       p := p + 1
                       s!p := capitalch(ch)
                       ch := rdch()
                       $)
                   IF ch = '*N' THEN unrdch()

                   h := p
                   FOR z = 1 TO p
                   DO h := (h*13 + s!z) & #X7FF
                   writef(" Hash value = %N*N",
                       B.Dir.HashTab +
                       (h REM Size.HashTable))
                   ENDCASE
                   $)

        CASE 'I':  || Print file info
                   || Print the information in the
                   || first few words of the block
                   $(
                   LET type = blockbuff ! B.File.Type
                   LET data.block = ?

                   check.block.read()
                   IF (type & T.Deleted) \= 0
                   THEN writes(" Deleted")

                   type := type & (NOT T.deleted)
                   writef(" %S*N", VALOF
                   SWITCHON type
                   INTO
                       $(
                       CASE T.Long:
                            RESULTIS "Long file"
                       CASE T.Short:
                            RESULTIS "Short file"
                       CASE T.Data:
                            RESULTIS "Data block"
                       CASE T.List:
                            RESULTIS "List"

                       DEFAULT:
                            RESULTIS "Corrupt type"
                       $)
                   )

                   data.block := type = T.Data
                   writef(" Header key:       %N*N",
                          blockbuff ! B.File.Ownkey)
                   TEST data.block
                   THEN writes(" Sequence number:  ")
                   ELSE writes(" Highest seq num:  ")
                   writef("%N*N",
                          blockbuff ! B.File.HighSeq)
                   writef(" Data size:        %N*N",
                          blockbuff ! B.File.Datasize)
                   TEST data.block
                   THEN writes(" Next data block:  ")
                   ELSE writes(" First data block: ")
                   writef("%N*N",
                          blockbuff ! B.File.FirstKey)
                   writef(" Checksum:         %N*N",
                          blockbuff ! B.File.Checksum)
                   UNLESS data.block
                   THEN
                   $(
                   LET datvec = VEC 14
                   writef(" Secondary type:   %S*N",
                     VALOF SWITCHON blockbuff!B.File.Secondarytype
                     INTO
                       $(
                       CASE ST.File:     RESULTIS "File"
                       CASE ST.Root:     RESULTIS "Root block"
                       CASE ST.UserDir:  RESULTIS "User directory"
                       DEFAULT:          RESULTIS "(Unset)"
                       $) )
                    IF [ blockbuff ! b.file.secondarytype = st.file ] THEN
                       writef(" Extension block : %N*N",
                              blockbuff ! B.File.Infobits)
                    writef(type=t.list -> " File header     : %N*N",
                                          " Parent Directory: %N*N",
                           blockbuff ! B.File.ParentDir)
                    writef(" Hash Chain:       %N*N",
                           blockbuff ! B.File.HashChain)
                    writef(" Filename:         *"%S*"*N",
                           blockbuff + B.File.Filename)
                   IF zero.or.packet = 0
                   THEN
                     $( // Running as command
                        start(blockbuff+B.File.CreationDate,datvec)
                        writef(" Created:          %S %S %S*N",
                                 datvec+10, datvec, datvec+5)
                     $)
                   IF [ type = t.short ] &
                      [ (  blockbuff ! b.file.secondarytype = st.file    ) |
                        (  blockbuff ! b.file.secondarytype = st.userdir ) ] THEN
                   $(
                      LET comment = blockbuff + b.file.infovec

                      IF comment%0 \= 0 THEN
                         TEST comment%0 > 50
                         THEN writes ( " ****** comment field corrupt*N"  )
                         ELSE writef ( " Comment:          %S*N", comment )
                   $)
                   $)
                   $)
                   ENDCASE


        CASE 'K':  || Calculate and check checksum
                   check.block.read()
                   $(
                   LET cs = block.checksum()

                   TEST cs = 0
                   THEN writes(" Checksum correct*N")
                   ELSE
                     $(
                     LET bad.cs = blockbuff ! B.File.Checksum
                     LET correct.cs = 0-(cs-bad.cs)
                     blockbuff ! B.File.Checksum := correct.cs
                     writef("****** warning - checksum incorrect*N*
                            *******         - value in block was %N*N*
                            *******         - corrected to       %N*N",
                            bad.cs, correct.cs)
                     blockwrittenback := FALSE
                     $)
                   ENDCASE
                   $)


        CASE 'L':  || Locate words which match
                   || pattern under mask
                   match := TRUE
           search: $(
                   LET lwb = readvalue()
                   LET upb = size.block - 1
                   LET maskedpat = mask & pattern
                   check.block.read()
                   skiplayout()

                   UNLESS nullnumber
                   THEN
                       $(
                       UNLESS 0 <= lwb < size.block
                       THEN
                           $(
                           writes("****** warning - invalid lower bound*N")
                           lwb := 0
                           $)

                       upb := readvalue()
                       UNLESS 0 <= upb < size.block
                       THEN
                           $(
                           writes("****** warning - invalid upper bound*N")
                           upb := size.block - 1
                           $)
                       $)

                   FOR j = lwb TO upb
                   DO
                       $(
                       LET v = blockbuff ! j
                       IF TESTFLAGS(1) BREAK

                       IF ((v & mask) = maskedpat) = match
                       THEN
                           $(
                           writef(" %I3:", j)
                           printloc(j)
                           $)
                       $)
                   $)
                   ENDCASE

        CASE 'M':  || Set locate mask
                   mask := readvalue()
                   ENDCASE

        CASE 'N':  // Locate words which don't
                   // match pattern under mask
                   match := false
                   GOTO SEARCH

        CASE 'P':  || Put block back on disc
                   IF readdiscaddr()
                   THEN discaction(act.write)
                   ENDCASE

        CASE 'Q':  || Quit
                   freevec(blockbuff) ; unloadseg ( datseg )
                   RETURN

        CASE 'R':  || Identify Root Block
                   writef(" Root Block is block %N*N",
                     RootKey)
                   ENDCASE

        CASE 'S':  || Set printing style
                   stringstyle := FALSE
                   charstyle   := FALSE

                   style := VALOF
                         SWITCHON capitalch(rdch())
                         INTO
                         $(
                         CASE 'C': charstyle := TRUE
                                   RESULTIS style

                         CASE 'S': stringstyle := TRUE
                                   RESULTIS style

                         CASE 'O': RESULTIS " %O6 "

                         CASE 'X': RESULTIS " %X4 "

                         CASE 'D': RESULTIS " %N "

                         DEFAULT:  writes(" Try c,s,o*
                                   *,x or d*N")
                                   RESULTIS style
                         $)

                   printloc(currword)
                   ENDCASE

        CASE 'T':  || Type range of locations
                   $(
                   LET lwb = readvalue()
                   LET upb = readvalue()

                   check.block.read()
                   IF nullnumber
                   THEN upb := lwb

                   FOR j = lwb TO upb
                   DO
                       $(
                       IF TESTFLAGS(1) BREAK
                       writef(" %I3: ", j)
                       printloc(j)
                       $)
                   $)
                   ENDCASE

        CASE 'U':  // Alter disc device id (unit?)
                   disc.devid := readvalue()
                   writef(" Now using device %n*n", disc.devid)
                   TEST rootnode ! rtn.devtab ! [ ABS disc.devid ] = 0
                   THEN writes ( "****** warning - device not mounted*N" )
                   ELSE FOR z = 0 TO ndrives - 1 DO
                        $( sendpkt ( notinuse, disc.devid, act.seek,
                                     ?, ?, ?, ?, z,   2 )
                           sendpkt ( notinuse, disc.devid, act.seek,
                                     ?, ?, ?, ?, z, -80 )
                        $)
                   ENDCASE

        CASE 'V':  || Set value for locate
                   pattern := readvalue()
                   ENDCASE

        CASE 'W':  || Windup
                   UNLESS blockwrittenback
                   THEN UNLESS discaction(act.write)
                        THEN ENDCASE
                   freevec(blockbuff) ; unloadseg ( datseg )
                   RETURN

        CASE 'X':  || Invert write protect mode
                   write.protected := NOT write.protected
                   writef(" Write protect mode %Sset*N",
                     write.protected -> "","un")
                   ENDCASE


        CASE 'Y':  || Set cylinder base
                   temp := readvalue()

                   UNLESS lowercylinder <= temp <= maxcyl       //RS130681
                   THEN
                       $(
                       writef("****** warning - value should be in range*
                          * %N to %N*N", lowercylinder, maxcyl) //RS130681
                       ENDCASE
                       $)

                   cylbase := temp
                   writef(" Cylinder base set to %N*N", cylbase)
                   ENDCASE


        CASE 'Z':  || Zero buffer
                   FOR i = 0 TO size.block - 1 DO
                     blockbuff ! i := 0
                   ENDCASE
        CASE '-': CASE '+': CASE '#':
        CASE '0': CASE '1': CASE '2':
        CASE '3': CASE '4': CASE '5':
        CASE '6': CASE '7': CASE '8':
        CASE '9':  || Read the offset in the buffer
                   || of the new current word
                   $(
                   LET n = ?
                   unrdch()

                   n := readvalue()
                   IF (n < 0) | (n >= size.block)
                   THEN
                       $(
                       writef("****** warning - offset %N invalid*N", n)
                       ENDCASE
                       $)
                   currword := n
                   $)
                   ENDCASE

        CASE '=':  || Print default values set
                   writef(" Disc drive %N*N",drive)
                   writef(" Block number %N: cyl %N,*
                     * sur %N, sec %N*N", lbn, cyl,
                     sur, sec)
                   writef(" Cylinder base = %N*N*
                     * Write protect mode is %Sset*N*
                     * Current block is %Sup to date*
                     * on disc*N",
                     cylbase,
                     write.protected -> "", "not ",
                     (blockwrittenback -> "", "not ") )
                   writef(" Current word offset = *
                     *%N*N", currword)
                   writef(" Logical block number *
                     *base = %N*N", lbnbase)
                   ENDCASE

        CASE '/':  || Open location
                   $(
                   LET v = readvalue()

                   check.block.read()
                   TEST nullnumber
                   THEN printloc(currword)
                   ELSE
                       $(
                       blockbuff ! currword := v
                       blockwrittenback := FALSE
                       $)
                   $)
                   ENDCASE

        CASE '*'': || Row of characters
                   $(
                   LET stringbase = currword*bytesperword
                   LET boffset = stringbase
                   LET bofflim = size.block * bytesperword

                   check.block.read()
                   ch := readchar()

                   UNTIL (ch = '*'')
                   DO
                       $(
                       IF boffset >= bofflim
                       THEN
                       $(
                       writes("****** warning - attempt to overflow block*N")
                       BREAK
                       $)

                       putbyte(blockbuff, boffset, ch)
                       boffset := boffset + 1
                       ch := readchar()
                       $)
                   $)
                   blockwrittenback := FALSE
                   ENDCASE

        CASE '"':  || String
                   $(
                   LET stringbase = currword*bytesperword
                   LET boffset = stringbase + 1
                   LET bofflim = size.block * bytesperword

                   check.block.read()
                   ch := readchar()

                   UNTIL (ch = '"')
                   DO
                       $(
                       IF boffset >= bofflim
                       THEN
                       $(
                       writes("****** warning - attempt to overflow block*N")
                       BREAK
                       $)

                       putbyte(blockbuff, boffset, ch)
                       boffset := boffset + 1
                       ch := readchar()
                       $)

                   || Fill in the length
                   putbyte(blockbuff,stringbase,
                          boffset-1-stringbase)
                   $)
                       blockwrittenback := FALSE
                   ENDCASE

        CASE '?':  writes(" Current values set are :*N" )
                   writef(" disc type            : %S*N",
                            dev.type = disctype.floppy -> "floppy",
                            dev.type = disctype.cart   -> "cartridge",
                            dev.type = disctype.big    -> "big disc pack",
                                                          "****error****"
                         )
                   IF dev.type = disctype.big DO                //RS130681
                   writef(" partition            = %N*N", partition)
                   writef(" lowercylinder        = %N*N", lowercylinder )
                   writef(" uppercylinder        = %N*N", maxcyl )
                   writef(" number of cylinders  = %N*N", ncyls )
                   writef(" number of surfaces   = %N*N", n.surfaces )
                   writef(" number of blocks     = %N*N", nblocks )
                   writef(" block size ( words ) = %N*N", size.block )
                   writef(" blocks per track     = %N*N", n.blockspertrack )
                   writef(" sectors per block    = %N*N", n.sectorsperblock )
                   writef(" number of drives     = %N*N", ndrives )
                   writef(" TRIPOS device id     = %N*N", disc.devid )
                   writef(" drive number         = %N*N", drive )
                   ENDCASE

        unknown:
        DEFAULT:   writef("****** warning - unknown command %C*N", ch)
                   ch := rdch() REPEATUNTIL ch = '*N'
                   unrdch()
        $)
    $) REPEAT
    $)



..
SECTION "Disced2"

GET ""

LET setdevicetype (newtype, partition) BE                       //RS130681
$(
   dev.type := newtype

   SWITCHON dev.type INTO
   $(
      CASE disctype.floppy :

           maxcyl             := 76                             //RS130681
           n.blockspertrack   := 6
           n.sectorsperblock  := 4
           sector.origin      := 1
           n.surfaces         := 1
           lowercylinder      := 0
           size.block         := 256
           disc.devid         := -2
           ndrives            := 2
           writes ( "using floppy disc characteristics*N" )
           ENDCASE

      CASE disctype.big :

           maxcyl             := 222 + partition * 200          //RS130681
           n.blockspertrack   := 32
           n.sectorsperblock  := 1
           sector.origin      := 0
           n.surfaces         := 5
           lowercylinder      := partition < 0 -> 0,            //RS130681
                                  23 + partition * 200          //RS130681
           size.block         := 256
           disc.devid         := -5
           ndrives            := 1
           writef ( "using big disc partition %N characteristics*N",
                        partition )                             //RS130681
           ENDCASE

      CASE disctype.cart :

           maxcyl             := 202                            //RS130681
           n.blockspertrack   := 12
           n.sectorsperblock  := 1
           sector.origin      := 0
           n.surfaces         := 5
           lowercylinder      := 8
           size.block         := 256
           disc.devid         := -5
           ndrives            := 1
           writes ( "using cartridge disc characteristics*N" )
           ENDCASE
   $)

// rootkey              := ((n.cylindersperdisc-lowercylinder) *//RS130681
//                           n.blockspertrack*n.surfaces)/2     //RS130681
   size.hashtable       := size.block - 56
   b.file.secondarytype := size.block - 1
   b.file.infobits      := size.block - 2
   b.file.parentdir     := size.block - 3
   b.file.hashchain     := size.block - 4
   b.file.filename      := size.block - 20
   b.file.creationdate  := size.block - 23
   b.file.infovec       := size.block - 50
// maxcyl               := n.cylindersperdisc - 1               //RS130681
// ncyls                := n.cylindersperdisc - lowercylinder   //RS130681
   ncyls                := maxcyl + 1 - lowercylinder           //RS130681
   nblocks              := ncyls*n.surfaces*n.blockspertrack
   rootkey              := nblocks / 2                          //RS130681
   drive                := 0
   cylbase              := lowercylinder
   write.protected      := TRUE
   block.read           := FALSE
   style                := " %N "
   cyl, sur, sec        := -1, -1, -1
   lbnbase              := 0
   lbn                  := -1
   stringstyle          := FALSE
   charstyle            := FALSE
   mask, pattern        := -1, 0
   nullnumber           := FALSE
   freevec ( blockbuff )
   blockbuff            := getvec ( size.block - 1 )

   IF blockbuff = 0 THEN $( writes    ( "****** error   - can't get new buffer*N" )
                            unloadseg ( datseg )
                            stop      ( 20 )
                         $)
   blockwrittenback     := TRUE

   writef ( "Write protect mode set, cylinder base set to %N*N", cylbase )

   TEST rootnode ! rtn.devtab ! [ ABS disc.devid ] = 0
   THEN
        writes ( "****** warning - this device is not mounted*N" )
   ELSE
        // for the floppies ...

        FOR z = 0 TO ndrives-1 DO
        $( sendpkt ( notinuse, disc.devid, act.seek, ?, ?, ?, ?, z,   2 )
           sendpkt ( notinuse, disc.devid, act.seek, ?, ?, ?, ?, z, -80 )
        $)
$)

AND skiplayout() BE
    $(
    LET c = rdch()

    WHILE (c = ' ') | (c = '*T')
    DO c := rdch()

    unrdch()
    $)


AND printloc(offset) BE
    $(
    IF (offset < 0) | (offset >= size.block)
    THEN
        $(
        writef("****** warning - offset %N invalid*N", offset)
        RETURN
        $)

    TEST stringstyle
    THEN writef(" *"%S*" *N", blockbuff + offset)
    ELSE
        TEST charstyle
        THEN
            $(
            writes(" '")
            FOR j = 0 TO bytesperword-1
            DO wrch(getbyte(blockbuff+offset, j))
            writes("' *N")
            $)
        ELSE
            $(
            writef(style, blockbuff ! offset,
                (blockbuff!offset) >> 8)
            newline()
            $)
    $)


AND discaction(action) = VALOF
    $(
    || Does a disc read or write as specified by action,
    || using blockbuff. The disc address is given by the
    || globals cyl, sur and sec.
    LET s =  ?

    IF rootnode ! rtn.devtab ! [ ABS disc.devid ] = 0 THEN
    $(
       writes ( "****** warning - this device is not mounted*N" )
       RESULTIS FALSE
    $)

    IF action = act.write
    THEN
      $(
      IF block.checksum() \= 0
      THEN writes(" ****** warning - block has incorrect checksum*N" )

      IF write.protected
      THEN
        $( writes(" ****** warning - write protect set - block not written*N" )
           RESULTIS FALSE
        $)
      $)

    UNLESS 0 <= cyl <= maxcyl
    THEN
        $(
        writef("****** warning - cylinder %N is out of range*N", cyl)
        RESULTIS FALSE
        $)

    FOR r = 1 TO 10
    DO
        $(
        s := sendpkt(notinuse,disc.devid,action,?,?,
                     blockbuff,size.block,drive,cyl,sur,sec)

        IF s = 0
        THEN
            $(
            IF r > 2
            THEN writef("****** warning - %N tries needed to transfer*
                        * cyl %N, sur %N, sec %N*N",
                        r, cyl, sur, sec)
            IF action = act.read
            THEN block.read := TRUE

            blockwrittenback := TRUE

            writef(" Block %N %S (cyl %N, sur %N, sec %N)*N",
              lbn, (action=act.write-> "updated","read"),
              cyl, sur, sec)
            RESULTIS TRUE
            $)
        $)

    writef("****** warning - transfer failed: cyl %N, sur %N, sec %N*
           *, status %O6*N",cyl,sur,sec,result2)
    RESULTIS FALSE
    $)




AND readdiscaddr() = VALOF
    $(
    || Reads a new disc address.
    || There are 2 possible cases:
    ||  (a) No numbers after the command - keep
    ||      the current address
    ||
    ||  (b) A new block number is specified.
    ||      Set globals cyl, sur and sec from it.
    ||
    || Exits with lbn, cyl, sur and sec set. Returns
    || TRUE if the address is valid, FALSE if an
    || error is detected.

    LET val = readvalue()
    UNLESS nullnumber
    THEN $( lbn := val; skiplayout() $)

    UNLESS convertdiscaddr(lbn+lbnbase)
    THEN RESULTIS FALSE

    RESULTIS TRUE
    $)



AND readchar() = VALOF
    $(
    || Returns the next character, after interpreting
    || escape combinations
    || If the character is the result of an escape
    || combination, then bit #X100 is set in the
    || result. This is so that unescaped quotes
    || can be detected by the caller.

    ch := rdch()

    TEST ch = '**'
    THEN
        $(
        ch := capitalch( rdch() )

        SWITCHON ch INTO
            $(
            CASE '**':RESULTIS '**' | escbit
            CASE '*'':RESULTIS '*'' | escbit
            CASE '*"':RESULTIS '*"' | escbit
            CASE 'N': RESULTIS '*N' | escbit
            CASE 'E': RESULTIS '*E' | escbit
            CASE 'S': RESULTIS '*S' | escbit
            CASE 'T': RESULTIS '*T' | escbit
            CASE 'B': RESULTIS '*B' | escbit

            CASE 'X': || Read 2 digit hex value
                      RESULTIS ((hexval(rdch()) << 4) +
                               hexval(rdch())) | escbit

            CASE '0': CASE '1': CASE '2':
            CASE '3': CASE '4': CASE '5':
            CASE '6': CASE '7':
                      || Read a 3 digit octal value
                      $(
                      LET val = ch - '0'

                      FOR i = 1 TO 2
                      DO
                          $(
                          ch := rdch()

                          UNLESS '0' <= ch <= '7'
                          THEN
                              $(
                              writef("****** warning - invalid octal *
                              *digit %C*N", ch)
                              BREAK
                              $)

                          val := val*8 + ch - '0'
                          $)

                      IF val > 255
                      THEN writef("****** warning - value %O3 too big*N",
                                   val)
                      RESULTIS val | escbit
                      $)

            DEFAULT:  writef("****** warning - unknown escape %C*N",ch)
                      RESULTIS ch | escbit
            $)
        $)
    ELSE
        $(
        IF (ch = '*N') | (ch = '*E')
        THEN unrdch()

        RESULTIS ch
        $)
    $)


AND hexval(c) = VALOF
    $(
    || Gives the value of the hex digit c
    IF '0' <= c <= '9' THEN RESULTIS c - '0'
    IF 'a' <= c <= 'f' THEN RESULTIS c - 'a' + 10
    IF 'A' <= c <= 'F' THEN RESULTIS c - 'A' + 10
    writef("****** warning - invalid hex digit %C*N", c)
    RESULTIS 0
    $)



AND convertdiscaddr(blockno) = VALOF
    $(
    || Sets globals cyl sur and sec.
    || Returns TRUE if blockno is valid, FALSE otherwise.

    IF (blockno >= nblocks) | (blockno < 0)
    THEN
        $(
        writef("****** warning - block number %N is invalid*N", blockno)
        RESULTIS FALSE
        $)

    cyl := blockno/(N.Surfaces * N.blockspertrack) + cylbase
    sur := blockno REM N.Surfaces
    sec := (blockno/N.Surfaces) REM N.blockspertrack *
           N.SectorsPerBlock + Sector.Origin
    RESULTIS TRUE
    $)



AND readvalue() = VALOF
    $(
    || Reads: optionally signed decimal number
    ||        # octal number
    ||        #X hex number
    ||        '<single char>
    nullnumber := FALSE
    skiplayout()
    ch := rdch()

    SWITCHON ch INTO
        $(
        CASE '-': RESULTIS -readvalue()
        CASE '+': RESULTIS readvalue()

        CASE '#': ch := rdch()
                  TEST (ch = 'X') | (ch = 'x')
                  THEN RESULTIS readhexnum()
                  ELSE
                      $(
                      unrdch()
                      RESULTIS readoctnum()
                      $)

        CASE '*'':RESULTIS rdch()

        CASE '0': CASE '1': CASE '2':
        CASE '3': CASE '4': CASE '5':
        CASE '6': CASE '7': CASE '8':
        CASE '9': unrdch()
                  RESULTIS readn()

        DEFAULT:  nullnumber := TRUE
                  unrdch()
                  RESULTIS 0
        $)
    $)


AND readhexnum() = VALOF
    $(
    LET val = 0

    ch := rdch()

    UNLESS ('0' <= ch <= '9') |
           ('A' <= capitalch(ch) <= 'F')
    THEN nullnumber := TRUE

    WHILE ('0' <= ch <= '9') |
          ('A' <= capitalch(ch) <= 'F')
    DO
        $(
        val := (val << 4) + hexval(ch)
        ch := rdch()
        $)

    unrdch()
    RESULTIS val
    $)


AND readoctnum() = VALOF
    $(
    LET val = 0
    ch := rdch()

    UNLESS '0' <= ch <= '7'
    THEN nullnumber := TRUE

    WHILE ('0' <= ch <= '7')
    DO
        $(
        val := (val << 3) + ch - '0'
        ch := rdch()
        $)

    unrdch()
    RESULTIS val
    $)





AND check.block.read() BE
    UNLESS block.read
    THEN writes("****** warning - No block has been read into buffer!*N")


AND block.checksum() = VALOF
    $(
    LET cs = 0
    FOR z=0 TO size.block-1
    DO cs := cs + blockbuff!z

    RESULTIS cs
    $)
