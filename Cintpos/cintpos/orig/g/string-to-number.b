MANIFEST
$( maxint_div_10        = maxint/10
   maxint_rem_10        = maxint REM 10
$)

LET string_to_number(string) = VALOF
// Convert string to number routine
// Returns TRUE if STRING represents a valid number
// (number in RESULT2), FALSE otherwise
$( LET len = string%0
   LET pos = TRUE
   LET ch = ?
   IF len=0 RESULTIS FALSE
   result2 := 0

   FOR i = 1 TO len DO
   $( LET ch = string%i
      IF ch='-' | ch='+' DO
      $( IF i~=1 | len=1 RESULTIS FALSE
         pos := ch='+'
         LOOP
      $)

      UNLESS '0'<=ch<='9' RESULTIS FALSE

      IF result2>maxint_div_10 |
         (result2=maxint_div_10 &
            (ch-'0')>(maxint_rem_10 + (pos -> 0, 1)))
      THEN RESULTIS FALSE

      result2 := result2*10 + ch - '0'
   $)
   UNLESS pos DO result2 := -result2
   RESULTIS TRUE
$)
