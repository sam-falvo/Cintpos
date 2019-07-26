?

// This file is designed to test the lexical analyser lex.

/* Testing nested comments /* inner comment */
   should still be in comment
*/

1234
12.
12.3
1234e-2
1234E-3

abc
asb.Def
Abc_xyz

""
"ABC"
TRUE
FALSE
VALOF
@
!
?
-
ABS
*
/
MOD
REM
+
-
=
~=
<
>
<=
>=
SLCT
OF
::
%
#(
~
\
NOT
<<
>>
&
|
EQV
XOR
NEQV
->
TABLE
NEEDS
SECTION
:=
GOTO
RESULTIS
:
TEST
FOR
IF
UNLESS
WHILE
REPEAT
REPEATWHILE
REPEATUNTIL
LOOP
BREAK
RETURN
FINISH
ENDCASE
SWITCHON
CASE
DEFAULT
<>
LET
AND
MANIFEST
GLOBAL
STATIC
BE
$(
$(tag
$)tag
$)
{
}
INTO
TO
BY
DO
THEN
ELSE
VEC
(
)
[
]
.
BITSPERBCPLWORD

// Floating point operators added 15/07/2010
FLOAT
FIX
#ABS
#*
#/
#MOD
#+
#-
#=
#~=
#<
#>
#<=
#>=
#->

// Assign operators added 27/11/2018
#:=
!:=
*:=
/:=
MOD:=
+:=
#*:=
#/:=
#MOD:=
#REM:=
#+:=
#-:=
<<:=
>>:=
&:=
|:=
EQV:=
NEQV:=
XOR:=







