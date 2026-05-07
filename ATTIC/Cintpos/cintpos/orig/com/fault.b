SECTION "FAULT" 

GET     "g/libhdr.h"
GET     "g/iohdr.h"
GET     "g/manhdr.h"
GET     "g/clihdr.h"

// Modifications:
// 19 Apr 83 by BJK: Spelling improved.

//$<COMMAND
LET start ( status, fault_no ) BE
$(
   LET co = createco ( faultco, 500 )

   TEST co = 0 THEN faultco ( @status ) ELSE $( callco   ( co, @status )
                                                deleteco ( co )
                                             $)
$)
//$>COMMAND

/*
$<BLIB
LET fault ( fault_no ) BE
$(
   LET co = createco ( faultco, 500 )
   LET fv = VEC 1

   fv!0, fv!1 := -1, fault_no

   TEST co = 0 THEN faultco ( fv ) ELSE $( callco   ( co, fv )
                                           deleteco ( co )
                                        $)
$)
$>BLIB
*/

AND faultco ( lvp ) BE
$(
   LET status   = lvp ! 0
   LET file     = "info/faults-table"
//   LET file     = "sys:info.faults-table"
   LET args     = ",,,,,,,,,"
   LET argv     = VEC 25
   LET errv     = VEC 40
   LET command  = [ status = 0 ]
   LET pos      = ?
   LET f_stream = ?
   LET n_args   = ?
   LET arg_list = VEC 10
   LET old_in   =  input ()
   LET old_out  = output ()
   LET found    = VEC 10
   LET why      = FALSE

   IF old_out = 0 THEN selectoutput ( findoutput ( "**" ) )

//$<COMMAND
   TEST command
        THEN $( IF rdargs ( args, argv, 25 ) = 0 THEN
                $(
                   writef ( "%S failed - bad arguments for *"%S*"*N",
                                               cli_commandname, args  )
                   stop   ( Return_severe )
                $)

                FOR i = 0 TO 9 DO IF argv ! i \= 0 THEN
                $(
                   LET fault_no  = number ( argv ! i )
                   LET duplicate = FALSE

                   UNLESS result2 DO
                   $(
                      writef ( "%S: argument *"%S*" ignored*N",
                                               cli_commandname, argv ! i )
                      LOOP
                   $)

                   FOR i = 1 TO n_args DO
                                IF arg_list ! i = fault_no THEN
                                                           duplicate := TRUE

                   UNLESS duplicate DO $(            n_args := n_args + 1
                                          arg_list ! n_args := fault_no
                                       $)
                $)

                IF n_args = 0 THEN $( arg_list ! 1 := cli_result2
                                      n_args       := 1
                                      why          := TRUE
                                   $)
             $)
           ELSE $( n_args := 1 ; arg_list ! 1 := lvp ! 1 $)
//$>COMMAND
/*
$<BLIB
   n_args       := 1
   arg_list ! 1 := lvp ! 1
$>BLIB
*/
   IF why & [ arg_list ! 1 = 0 ] THEN
   $(
      writes ( "The last command did not set a return code*N" )
      RETURN
   $)

   IF why THEN writes ( "The last command failed because " )

   f_stream := findinput ( file )

   IF f_stream = 0 THEN
   $(
      TEST command &
           NOT why THEN writef ( "Error file *"%S*" not available*N", file )
                   ELSE writef ( "fault %N occurred*N", arg_list ! 1 )

      IF old_out = 0 THEN endwrite ()

      TEST command THEN stop ( Return_severe ) ELSE RETURN
   $)

   selectinput ( f_stream )

   FOR i = 1 TO n_args DO
   $(
      LET fault_no = arg_list ! i
      LET hashent  = ?

      pointto     ( fault_no & #X03FF )
      readwords   ( @hashent, 1 )

      pos := get2bytes ( @hashent, 0 )

      UNTIL pos = 0 DO
      $(
         pointto ( pos )

         IF readwords ( errv, 40 ) < 40 THEN $( pos := 0 ; LOOP $)

         IF get2bytes ( errv, 1 ) = fault_no THEN
         $(
            FOR c = 1 TO errv%6 DO wrch(errv%(6+c))

            newline () ; found ! i := TRUE ; GOTO next_error
         $)

         pos := get2bytes ( errv, 0 )
      $)

      found ! i := FALSE

      /**/ next_error : /**/
   $)

   endread () ; selectinput ( old_in )

   FOR i = 1 TO n_args DO UNLESS found ! i DO
   $(
      TEST command &
           NOT why THEN writef ( "No fault message for %N*N",  arg_list ! i )
                   ELSE writef ( "fault %N (#X%X4) occurred*N", arg_list ! 1,
                                                               arg_list ! 1 )
   $)

   IF old_out = 0 THEN endwrite ()
$)

AND pointto ( pos ) BE
$(
   LET pvec = VEC 2

   pvec ! 0, pvec ! 1, pvec ! 2 := 0, pos * 2, 0

   //  Note by IDW:  12-Feb-87
   //    MFR needs boiling sometimes!  In the following code, he always called
   //    "task_filehandler", even if the current input stream corresponded to
   //    a different filing system!

   sendpkt ( notinuse, ABS cis!Scb_type, Action_point, 0, 0, cis!Scb_arg1, pvec )
$)

AND number ( str ) = VALOF
$(
   LET num = 0
   LET rx  = 10
   LET f   = 1
   LET neg = FALSE

   IF str % 1 = '#' THEN TEST [ str % 2 = 'X' ] |
                              [ str % 2 = 'x' ] THEN $( f := 3 ; rx := 16 $)
                         ELSE $( f := 2 ; rx := 8 $)

   IF rx = 10 & [ str % 1 = '-' ] THEN $( neg := TRUE ; f := 2 $)

   FOR c = f TO str % 0 DO
   $(
      LET ch = str % c

      ch := [ '0' <= ch <= '9' ] -> ch - '0',
            [ 'A' <= ch <= 'Z' ] -> ch - 'A' + 10,
            [ 'a' <= ch <= 'z' ] -> ch - 'a' + 10, 999

      TEST 0 <= ch <= rx THEN num := num * rx + ch
                         ELSE $( result2 := FALSE ; RESULTIS 0 $)
   $)

   result2 := TRUE ; RESULTIS neg -> -num, +num
$)


