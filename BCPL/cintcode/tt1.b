// Some junk comment

GET "libhdr.h"

LET start() = VALOF
{
LET x = #o_1011_ + 2    // the name x
writef("#o_1011+2=%4b*n", x)
//.            // a section separating dot
x := .1     // same as 0.1
writef(".1 = %12.5f*n", x)
//IF DO SKIP
//..2    // same as .. 2
//3..4   // same as 3 .. 4
x := 5.     // same as 5.0
writef("5. = %12.5f*n", x)
x := 6.e2   // same as 600.0
writef("6.e2 = %12.5f*n", x)
x := 7.1e-3 // same as 0.0071
writef("7.1e-3 = %12.5f*n", x)
x := 8.1e+3 // same as 8100.0
writef("8.1e+3 = %12.5f*n", x)

//x := 9.1e   // bad number
//writef("9.1e = %12.5f*n", x)

//x := 33e     // bad number
//x := "//44e-XX    // bad number
//x := .e5    // bad number
//writef(".e5 = %12.5f*n", x)
x := 5.e5    // same as .  e55
writef("5.e5 = %12.5f*n", x)
x := .1e5    // same as .  e55
writef(".1e5 = %12.5f*n", x)
  RESULTIS 0
}

LET f(x) = x+1

// aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
// aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
