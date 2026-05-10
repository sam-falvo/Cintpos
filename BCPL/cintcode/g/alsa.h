
/*
######## UNDER DEVELOPMENT ################

This is the BCPL header file for the ALSA library interface.

Implemented by Martin Richards (c) June 2023

History:

07/07/2023
Initial implementation.



g_alsabase is set in libhdr to be the first global used in the alsa library
It can be overridden by re-defining g_alsabase after GETting libhdr.

A program wishing to use the ALSA library should contain the following lines.

GET "libhdr"
MANIFEST { g_alsabase=nnn  } // Only used if the default setting of 500 in
                             // libhdr is not suitable.
GET "alsa.h"
GET "alsa.b"                  // Insert the library source code
.
GET "libhdr"
MANIFEST { g_alsabase=nnn  } // Only used if the default setting of 450 in
                             // libhdr is not suitable.
GET "alsa.h"
Rest of the program
*/

GLOBAL {
xxx: g_alsabase             // Typically G:500
yyy
zzz

}

MANIFEST {
// ops used in calls of the form: sys(Sys_sdl, op,...)
// These should work when using a properly configured BCPL Cintcode system
// running under Linux, Windows or or OSX provided the SDL libraries have been
// installed.
alsa_avail=0            //  0 Returns TRUE is ALSA is available
alsa_open_wav_input     //  1 (name, format, channels, rate, buf, upb)
                        //    name = eg hw:1
			//    format = 16 or 8, number of bits per sample
			//    channels = 1 or 2, ie mono or stereo
			//    rate = eg 44100, samples or pairs per second
			//    buf is a buf with given upb to hold the samples
			//      held as signed word length integers
			//    upb is the upper bound of buf
			//    Returns a wav input handle or -1.
alsa_pause_wav_input    //  2 Not implemented
alsa_resume_wav_input   //  3 Not implemented
alsa_wav_read           //  4 (handle) Push input samples into buf
                        //    Return the number of samples now in buf
			//    or -1 if error
alsa_close_wav_input    //  5 (handle} Close the specified wav input

alsa_open_wav_output    //  6
alsa_wav_write          //  7   
alsa_close_wav_output   //  8

alsa_open_midi_input    //  9
alsa_midi_read          // 10
alsa_close_midi_inpur   // 11

alsa_open_midi_output   // 12
alsa_midi_write1        // 13
alsa_midi_write2        // 14
alsa_midi_write3        // 15
alsa_midi_write         // 16
alsa_close_midi_output  // 17

alsa_init               // 18 Initialise the ALSA librry
alsa_longname           // 19 (n,str) Obtain the long name of device n
alsa_setscheduler       // 20
}
