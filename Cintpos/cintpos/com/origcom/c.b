// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "C"

GET "g/libhdr.h"
GET "g/clihdr.h"


GLOBAL $( subch         :  ug
          busch         :  ug +  1
          defch         :  ug +  2
          dirch         :  ug +  3
          ch            :  ug +  4

          instream      :  ug +  5
          outstream     :  ug +  6
          sys_stream    :  ug +  7

          rdargskey     :  ug +  8
          parameters    :  ug +  9
          keyword       :  ug + 10
          defstart      :  ug + 11
          keygiven      :  ug + 12

          newfile       :  ug + 13

          par_stream    :  ug + 14

          err_p         :  ug + 15
          err_l         :  ug + 16

          c_rcode       :  ug + 17
          c_result2     :  ug + 18
       $)



MANIFEST $( fileupb        =      20
            workfileupb    =       8
            taskposlwb     =      14
            taskposupb     =      15
            switchpos      =      11

            parsupb        =     100
            rdargskeyupb   =      50
            keywordupb     =      10
            keycharsmax    =      21
         $)



LET start(command_stream,parameter_stream) = VALOF
  // C command. If the parameter 'command_stream' is zero,
  //  then the first item on the command line is used as
  //  the command file. I.e. C file args .....
  // In this case, both the file name, and the parameters
  //  are read from the currently selected stream, and this
  //  is also used as the rest of the current command file.
  //  If the parameter is non-zero, then it is used as the
  //  command stream, and closed when the command finishes.
  //  The parameters are read from the second stream parameter,
  //  which is left open, at the end of the first line.
  $( LET cfile = VEC fileupb
     LET item = ?

     LET rkvec  = VEC rdargskeyupb
     LET parvec = VEC parsupb
     LET keyvec = VEC keywordupb

     LET newf    = VEC workfileupb
     LET workfile = "T-Command-0-Tnn"
//     LET workfile = "T:Command-0-Tnn"
     LET newstream = ?

     err_p     := level()
     c_rcode   := 0
     c_result2 := 0

     instream  := command_stream
     sys_stream:= output()
     par_stream:= instream = 0 -> cli_currentinput, parameter_stream
     outstream := 0

     keygiven := FALSE

     subch, busch := '<', '>'
     defch, dirch := '$', '.'

     newfile    := newf
     rdargskey  := rkvec
     parameters := parvec
     keyword    := keyvec

     IF command_stream = 0 THEN // Must read item.
       $( item := rditem(cfile,fileupb)
          IF item <= 0 THEN
            $( IF item ~= 0 THEN
                 reportcerr("Incorrect file name")
               RESULTIS 20
            $)
          instream := findinput(cfile)

          IF instream = 0 THEN // Try default directory
            $( LET dir = currentdir
               currentdir := locatedir("SYS:S")
               instream := findinput(cfile)
               freeobj(currentdir)
               currentdir := dir
            $)
          IF instream = 0 THEN
            reportcerr("Can't open %S",cfile)
       $)


     selectinput(instream)

     // Construct work-file name
     FOR j = 0 TO workfile % 0 DO
       newfile % j := workfile % j

     $( LET t = taskid
        FOR j = taskposupb TO taskposlwb BY -1 DO
          $( newfile % j := t REM 10 + '0'
             t := t/10
          $)
     $)

     IF cli_currentinput ~= cli_standardinput THEN
       IF cli_commandfile % 0 >= switchpos THEN
         newfile % switchpos :=
           cli_commandfile % switchpos NEQV 1

     outstream := findoutput(newfile)

     IF outstream = 0 THEN
       reportcerr("Can't open work file *"%S*"",newfile)

     selectoutput(outstream)

     IF cli_interactive THEN testflags(Flag_commbreak)

     ch := rdch()

     UNTIL ch = endstreamch DO
       TEST ch = dirch THEN
         handledirective()
        ELSE
         substitute()

     endread()


     // First read rest of parameter line.

     consume_rest_of_line()

     // Copy rest of current input.

     selectinput(cli_currentinput)

     IF cli_currentinput ~= cli_standardinput THEN
       $(
          $( ch := rdch()
             IF ch = endstreamch THEN
               BREAK
             wrch(ch)
          $) REPEAT

       $)

     endwrite()

     newstream :=findinput(newfile)

     IF cli_currentinput ~= cli_standardinput THEN
       $( endread()
          deleteobj(cli_commandfile)
       $)

     cli_currentinput := newstream

     FOR j = 0 TO newfile % 0 DO
       cli_commandfile % j := newfile % j


  err_l:
     selectoutput(sys_stream)

     // Code added 26.03.82 to pass back a return code in the
     // event of an error (when not callseged):  CGG

     result2 := c_result2
     IF command_stream=0 THEN stop(c_rcode)

     RESULTIS c_rcode
  $)



AND handledirective() BE // Called after reading 'dirch'
  $( ch := rdch()
     UNLESS ch = '*N' | ch = ' ' | ch = endstreamch THEN
       $( LET item,c = ?,?
          unrdch()

         item := rditem(keyword,keywordupb)
          c := (item ~= 1 -> -1,
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

                    selectinput(par_stream)
                    selectoutput(sys_stream)
                    defstart := rdargs(rdargskey,
                      parameters,parsupb); unrdch()
                    selectoutput(outstream)
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
                           $( IF item ~= 0 THEN
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

     IF item <= 0 | keyword % 0 ~= 1 THEN
       reportcerr("Invalid directive argument")

     RESULTIS keyword % 1
  $)


AND consume_rest_of_line() BE
  $( LET ch = ?
     selectinput(par_stream)
     ch := rdch() REPEATUNTIL ch = '*N' | ch = '*E' |
                              ch = ';'  | ch = endstreamch
  $)



AND reportcerr(format,parm1,parm2) BE
  $( c_result2 := result2

     IF outstream ~= 0 THEN
       $( endwrite()
          deleteobj(newfile)
          selectoutput(sys_stream)
       $)
     IF instream ~= 0 THEN
       endread()
     consume_rest_of_line()
     writes("C: "); writef(format,parm1,parm2)
     wrch('*N')
     c_rcode := 20
     longjump(err_p, err_l)
  $)
