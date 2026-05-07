/*
 CMS REPLACEMENT HISTORY, Element STRTODAT.BPL
 *1    10-AUG-1998 13:33:26 RUDOLPH "insert missing source"
 CMS REPLACEMENT HISTORY, Element STRTODAT.BPL

18 Feb 2002   M. Richards     Modified for Cintpos
*/

// string to internal date format
// v!0 = days since 1-jan-1978
// v!1 = minutes since midnight
// v!2 = ticks in minute (20ms (was 1ms) ticks)

SECTION "strtodat"

GET "libhdr"
GET "manhdr"

// This is meant to called using callseg("strtodat", datv, datestring)

LET start( datv, datestring ) = VALOF
{  
//    LET oldinput, oldoutput = input(), output()
    LET year		= 0
    LET days_since_78   = 0
    LET days_since_start_of_year   = 0
    LET days		= 0
    LET day_offset	= 0
    LET offset		= 0
    LET dayofweek	= 0
    LET month_str	= VEC 2
    LET monthtab	= TABLE 0,31,59,90,120,151,181,
				212,243,273,304,334,365

    LET leapmonthtab	= TABLE 0,31,60,91,121,152,182,
				213,244,274,305,335,366

    LET daytab		= TABLE 31,28,31,30,31,30,31,31,30,31,30,31
    LET leapdaytab	= TABLE 31,29,31,30,31,30,31,31,30,31,30,31

    LET mchars   	= VEC 36/bytesperword
    LET mcharbase	= 0		
    LET ms		= 0
    LET temp		= VEC 3
    LET mtable 		= ?	
    LET dtable		= ?
    LET valid_no_of_days= 0
    LET i		= 0
    LET rel_to_today_str = 
	"Tomorrow,Today,Yesterday,"
    LET days_of_week_str = 
	"Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,"

    IF datestring = 0 THEN
    {
       RESULTIS FALSE
    }

    datstamp( datv )		// get current date
    datv!1 := 0
    datv!2 := 0

    TEST match( rel_to_today_str, datestring, @offset ) THEN
    {
//       datstamp( datv )		// get current date

       day_offset := VALOF
       SWITCHON offset INTO
       {
          CASE 0: RESULTIS  1	// tomorrow
	  CASE 1: RESULTIS  0   // today
	  CASE 2: RESULTIS -1   // yesterday
       }
       datv!0 := datv!0 + day_offset
    }
    ELSE
    TEST match( days_of_week_str, datestring, @offset ) THEN
    {
       dayofweek := datv!0 REM 7

       day_offset := ( offset < dayofweek ) -> 
			offset-dayofweek, offset-dayofweek-7

       datv!0 := datv!0 + day_offset
    }
    ELSE
    {

       // gc's calculation.
       datv!0 := 0

       year := (datestring%8 - '0')*10
       year := year + (datestring%9 - '0')
       
       // if year <= 50 THEN treat all years < 1978 as year 2000+

       IF year <= 77 THEN
       {
          // year 2000+
          year := year + 100
       }

       mtable := (year REM 4)=0 -> leapmonthtab, monthtab
       dtable := (year REM 4)=0 -> leapdaytab, daytab
 
       {
          year := year - 1
          IF ( year < 78 ) THEN BREAK
          days_since_78 := days_since_78 + ((year REM 4)=0 -> 366, 365)
       } REPEAT

       // now lets work out the month

       mchars := "JanFebMarAprMayJunJulAugSepOctNovDec"

       month_str%0 := 3
       month_str%1 := datestring%4
       month_str%2 := datestring%5
       month_str%3 := datestring%6

       {
          IF ms >= 12 THEN
	  {
	     RESULTIS FALSE
          }
          mcharbase := 3*ms

          temp%0 := 3
          FOR j = 1 TO 3 DO temp%(j) := mchars % (mcharbase + j)

          IF compstring( temp, month_str ) = 0 THEN
          {
	     days_since_start_of_year := mtable!ms
             valid_no_of_days	      := dtable!ms
             BREAK
          }
          ms := ms + 1
       } REPEAT

       // now add on days in month
       days := (datestring%1-'0')*10 + (datestring%2-'0')
       TEST days > valid_no_of_days THEN
       {
	  RESULTIS FALSE
       }
       ELSE
       {
          days_since_78 := days_since_78 + days_since_start_of_year + days-1
	  datv!0 := days_since_78
       }
    }
}

AND match( pattern_string, string, offset ) = VALOF
{
   LET i 	= 0
   LET j 	= 0
   LET comma	= 0
   LET found 	= FALSE
   LET temp	= VEC 10

   UNTIL ( found ) | ( i >= pattern_string%0 ) DO
   {
      j := 0
      {
	 i := i + 1
         IF pattern_string%i = ',' BREAK
	 j := j + 1
	 temp%j := pattern_string%i
      } REPEAT
      comma := comma + 1
      temp%0 := j
      IF compstring( temp, string ) = 0 THEN
      {
         found := TRUE
	 BREAK
      }
   }
   !offset := comma - 1
   RESULTIS found
}
///////////////////////    E N D   O F   M O D U L E    ////////////////////////
