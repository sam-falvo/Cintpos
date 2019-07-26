/*
This program is designed to adjust the layout of
BCPL programs. All it does is add or remove spaces
at the start of lines to hopefully improve the
indentation.

Implemented by Martin Richards (c) 11 Dec 2018

Usage:  prettify FROM/A,TO/K,-t/S

The rules are (probably):

Start with an indentation level of zero.

After { set a new indentation level 2 larger than the current one.
After $( set a new indentation level 3 larger than the current one.
Before } or $) return to the previous indentation level.
Be careful with multi line strings and comments.

Possibly replace old lexical tokens by the new versions.
Eg change LSHIFT to << and GE to >=

Possibly replace untagged $( and $) with { and }.

There is a stack of indentation levels of size 1000. That should be
enough!

History

11/12/2018
Just started work on it.
*/

SECTION "prettify"

GET "libhdr"
GET "bcplfecg"

LET start() = VALOF
{ writef("*nPrettify is not yet implemented*n*n")
  RESULTIS 0
}

