// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "REFLECT"

GET "LIBHDR"
GET "CLIHDR"


GLOBAL $( subch         :  ug
          busch         :  ug +  1
          defch         :  ug +  2
          dirch         :  ug +  3
          ch            :  ug +  4

          instream      :  ug +  5
          outstream     :  ug +  6

          rdargskey     :  ug +  7
          parameters    :  ug +  8
          keyword       :  ug +  9
          defstart      :  ug + 10
          keygiven      :  ug + 11

          par.stream    :  ug + 12
       $)


MANIFEST $( fileupb        =      20
            rdargskeyupb   =      50
            keywordupb     =      10
            keycharsmax    =      21
            parsupb        =     100
         $)



LET start() BE
  $( LET cfile = VEC fileupb
     LET item = ?
     LET rkvec  = VEC rdargskeyupb
     LET parvec = VEC parsupb
     LET keyvec = VEC keywordupb


     instream  := 0
     outstream := 0
     par.stream := cli.currentinput

     keygiven := FALSE

     subch, busch := '<', '>'
     defch, dirch := '$', '.'

     rdargskey  := rkvec
     parameters := parvec
     keyword    := keyvec

       $( item := rditem(cfile,fileupb)
          IF item <= 0 THEN
            $( IF item \= 0 THEN
                 reportcerr("Incorrect file name")
               RETURN
            $)
          instream := findinput(cfile)

          IF instream = 0 THEN // Try default directory
            $( LET dir = currentdir
               currentdir := locatedir(":S")
               instream := findinput(cfile)
               freeobj(currentdir)
               currentdir := dir
            $)
          IF instream = 0 THEN
            reportcerr("Can't open %S",cfile)
       $)


     selectinput(instream)


     ch := rdch()

     UNTIL ch = endstreamch DO
       TEST ch = dirch THEN
         handledirective()
        ELSE
         substitute()

     endread()


  $)



AND handledirective() BE // Called after reading 'dirch'
  $( ch := rdch()
     UNLESS ch = '*N' | ch = ' ' | ch = endstreamch THEN
       $( LET item,c = ?,?
          unrdch()

         item := rditem(keyword,keywordupb)
          c := (item \= 1 -> -1,
                   findarg("KEY,K,DEFAULT,DEF,*
                           *BRA,KET,DOLLAR,DOT",keyword))
          IF c < 0 THEN
            reportcerr("Invalid directive")

          SWITCHON c INTO

            $( CASE 0: CASE 1:
                 // KEY for RDARGS.
                 IF keygiven THEN
                   reportcerr("More than one K directive")

                 $( LET item = rditem(rdargskey,rdargskeyupb)
                    IF item <= 0 THEN
                      reportcerr("Illegal K directive")

                    selectinput(par.stream)
                    defstart := rdargs(rdargskey,
                      parameters,parsupb); unrdch()
                    selectinput(instream)
                    IF defstart = 0 THEN
                      reportcerr("Parameters unsuitable for*
                                 * key *"%S*"", rdargskey)

                    keygiven := TRUE
                 $)
                 ENDCASE

               CASE 2: CASE 3:
                 // DEFAULT keyword [=] value
                 $( LET item = rditem(keyword,keywordupb)
                    LET keyn = ?

                    IF item < 0 THEN
                      reportcerr("Illegal keyword")

                    IF item = 0 THEN
                      ENDCASE

                    UNLESS keygiven THEN
                      reportcerr("No K directive")

                    keyn := findarg(rdargskey,keyword)

                    IF keyn >= 0 & parameters ! keyn = 0 THEN
                      $( LET dupb = parsupb+parameters-defstart
                         item := rditem(defstart,dupb)

                         IF item = -2 THEN
                           item := rditem(defstart,dupb)

                         IF item <= 0 THEN
                           $( IF item \= 0 THEN
                                reportcerr("Illegal D item")
                              ENDCASE
                           $)

                         parameters ! keyn := defstart
                         defstart := defstart +
                           (defstart % 0)/bytesperword + 1
                      $)
                    ENDCASE

                 $)


               DEFAULT: // Set new character.
                 (@ subch) ! (c - 4) := getch()
                 ENDCASE

            $)

          ch := rdch()

       $)

     UNTIL ch = '*N' | ch = endstreamch DO ch := rdch()

     ch := rdch()

  $)



AND substitute() BE
  $( LET writing, substituting = TRUE, FALSE
     UNTIL ch = '*N' | ch = endstreamch DO

     TEST ch = subch & writing THEN // <key$default>
       $( LET keyn, l = ?,0
          writing := FALSE
          substituting := TRUE

          UNLESS keygiven THEN
            reportcerr("No K directive")

          ch := rdch()

          UNTIL ch = busch | ch = defch |
                ch = '*N'  | ch = endstreamch DO
            $( IF l >= keycharsmax THEN
                 reportcerr("Keyword too long*N")
               l := l + 1
               keyword % l := ch
               ch := rdch()
            $)


          keyword % 0 := l

          keyn := findarg(rdargskey,keyword)

          TEST keyn < 0 | parameters ! keyn = 0 THEN
            writing := TRUE
           ELSE
            TEST parameters ! keyn = -1 THEN
              writes(keyword)
             ELSE
              writes(parameters ! keyn)

          IF ch = defch THEN
            ch := rdch()

       $)
      ELSE
       $( TEST ch = busch & substituting THEN
            $( writing := TRUE
               substituting := FALSE
            $)
           ELSE
            IF writing THEN
              wrch(ch)
          ch := rdch()
       $)

     wrch('*N')
     ch := rdch()

  $)


AND getch() = VALOF // Get single character item.
  $( LET item = rditem(keyword,keywordupb)

     IF item = 0 THEN
       $( ch := rdch(); unrdch()
          IF ch = '*N' | ch = endstreamch THEN
            RESULTIS -2
       $)

     IF item <= 0 | keyword % 0 \= 1 THEN
       reportcerr("Invalid directive argument")

     RESULTIS keyword % 1
  $)


AND consume.rest.of.line() BE
  $( LET ch = ?
     ch := rdch() REPEATUNTIL ch = '*N' | ch = '*E' |
                              ch = ';'  | ch = endstreamch
  $)



AND reportcerr(format,parm1,parm2) BE
  $( IF outstream \= 0 THEN
       $( endwrite()
          selectoutput(cli.currentoutput)
       $)
     IF instream \= 0 THEN
       endread()
     writes("C: "); writef(format,parm1,parm2)
     wrch('*N')
     stop(return.hard)
  $)






