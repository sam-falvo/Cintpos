/*
This library provides some functions that interface with the ALSA
sound libary.

Implemented by Martin Richards (c) June 2023

Change history:

07/06/2023
Initial implementation.


It should typically be included as a separate section for programs that
need it. Such programs typically have the following structure.

GET "libhdr"
MANIFEST { g_alsabase=nnn  } // Only used if the default setting of 500 in
                             // libhdr is not suitable.
GET "alsa.h"
GET "alsa.b"                 // Insert the library source code
.
GET "libhdr"
MANIFEST { g_alsabase=nnn  } // Only used if the default setting of 450 in
                             // libhdr is not suitable.
GET "alsa.h"
Rest of the program
 
*/

LET xxx(v, w) = 123



