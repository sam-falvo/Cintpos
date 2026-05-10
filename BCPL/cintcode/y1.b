//GET "libhdr"

GLOBAL { w:200; f start:1 }

LET start(x) = f(2)

LET f //(x) = //EVERY (x) // P3:x P4:result P5:args 
//: 1 => 101
: 22 => VALOF
        { LET y = 123 // P6:y
          RESULTIS y
        }


