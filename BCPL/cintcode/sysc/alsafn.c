/*
This contains the implemetation of the sys(Sys_alsa, fno, ...) facility.

###### Still under ealy development ############

Implemented by Martin Richards (c) Jun 2023

History

07/06/2023
Started the implementation, loosely based on soundfn.c.


This file is planned to provide and interface to ALSA sound library.
BCPL programs should use the g/alsa.b library with the g/alsa.h header
file.

The BCPL interface using

res := sys(Sys_alsa, fno, a1, a2, a3, a4,...)

Note that this calls alsafn(args, g, w) from dosys in cintsys.c
where args[0] = fno, args[1]=a1,... etc
and   g is the m/c address of global zero.
and   w is the m/c address of the base of the Cintcode memory.

This interface currently assumes 32-bit BCPL is being used. This may one day
be extended to work with 64-bit BCPL.

The macro name ALSAavail is only defined if the ALSA features are available. If
ALSAavail is not defined a dummy version of alsafn is defined.

The fnos have manifest names such as: alsa_avail, alsa_init, alsa_quit or
alsa_open_wav_input. They provide all the ALSA features available to BCPL.

Information between BCPL code and the ALSA library is oftem passed in control
blocks such as wav_inputCB or midi_outputCB.  A suitable control block is
always passed when openning a sound device. A description of the fields of each
kind of control block is given close to the case label for openning that kind
of device.

This BCPL library provides an interface to a small subset of the facilities
provided by the ALSA system. But it is sufficient to allow typically CD quality
reading and writing of PCM digital data. It also allows access to MIDI devices.

The digital data consists of streams of samples which in this library are
limited to either signed 16-bit integers or unsigned bytes. A frame is a set of
samples from different channels that all occur at the same time. For this
library a frame has only one sample for mono data and two for stereo with the
first corresponding to the left channel. A period (sometimes called a fragment)
is a group of Frames. The number of frames in a period is chosen by the
user. When communicating with an audio device data is passed one period at a
time. In this library flow control is done by polling. So, when trying to read
a period of frames, the result indicates whether all the frames were ready, or
whether the read operation should be tried again later. The user gives the
audio device a buffer large enough to hold several frames. These allow the
audio device to store samples until they are read by the user. If the data is
not read soon enough, samples will be lost.  The collection of periods is
essentially a circular buffer with the latest samples being placed in the
oldest period. Similar flow control is used when outputing data.

In playmus a vector is maintained that hold consecutive samples representing
the most recent data from the microphone. These samples are held as BCPL
integers and are read from the audio device one period at a time by calls of
res:=sys(Sys_alsa, alsa_read, wav_input_CB, p, n). The result res normally the
number of samples actually transferred or -1 if the latest period is not yet
complete. The date stamp of the latest sample transferred is maintained in
wav_input_CB. When necessary data in the buffer is moved down by calls of
sys(Sys_memmove, dest, src, n) which is implemented efficiently.
 
*/



#include "cintmain.h"
// Note that cintmain.h conditionally defines quantities such as
// ALSAavail, SDLaval and GLavail so must be included early.


#ifndef ALSAavail
// ALSA in not required so define a dummy version of alsafn.
BCPLWORD alsafn(BCPLWORD *args, BCPLWORD *g, BCPLWORD *W) {
  printf("ALSAavail was not defined\n");
  return 0;   // All Sys_alsa calls return FALSE if ALSA is not available
}
#endif


#ifdef ALSAavail
// This is the proper version of alsafn.

#include <sched.h>
#include <alsa/asoundlib.h>

// Conversion between BCPLORD and BCPLFLOAT
extern BCPLFLOAT N2F(BCPLWORD  x);
extern BCPLWORD  F2N(BCPLFLOAT x);

// Conversion between BCPL and C strings
extern char *b2c_str(BCPLWORD bstr, char *cstr);
extern BCPLWORD c2b_str(const char *cstr, BCPLWORD bstr);
extern void copyaddrB2C(void*from, void*to);
extern void copyaddrC2B(void*from, void*to);

static void setscheduler(void);

// The following constants are used by alsafn.c and
// they must agree with those declared in g/alsa.h

#define alsa_avail                0

#define alsa_open_wav_input       1
#define alsa_pause_wav_input      2
#define alsa_resume_wav_input     3
#define alsa_wav_read             4   
#define alsa_close_wav_input      5

#define alsa_open_wav_output      6
#define alsa_wav_write            7   
#define alsa_close_wav_output     8

#define alsa_open_midi_input      9
#define alsa_midi_read           10
#define alsa_close_midi_input    11

#define alsa_open_midi_output    12
#define alsa_midi_write1         13
#define alsa_midi_write2         14
#define alsa_midi_write3         15
#define alsa_midi_write          16
#define alsa_close_midi_output   17

#define alsa_init                18
#define alsa_longname            19
#define alsa_setscheduler        20



// This is the proper definition of alsafn providing the BCPL
// interface with ALSA. The actual definitions depend on the
// hst operating system, typically Linux or Windows.

#if defined(forLinux)||defined(forLinuxSDL)||defined(forLinuxGL)||\
  defined(forARM)||defined(forRaspiSDL)
/******************** Linux Version *********************************/
/*
// This function does not seem to work when running on Ubuntu Linux
// under an Oracle Virtual Machine under Windows 10. So for the time
// being thereis no attempt to change the scheduling priority of
// this program.
 
void setscheduler(void)
{ // This function fails to set the round robin prority########
    struct sched_param sched_param;
 
    if (sched_getparam(0, &sched_param) < 0) {
        printf("Scheduler getparam failed...\n");
        return;
    }
    sched_param.sched_priority = sched_get_priority_max(SCHED_RR)/10;
    
    if (!sched_setscheduler(0, SCHED_RR, &sched_param)) {
        printf("Scheduler set to Round Robin with priority %lld...\n",
	        sched_param.sched_priority);
        fflush(stdout);
        return;
    }
    printf("!!!Scheduler set to Round Robin with priority %lld FAILED!!!\n",
	    LL sched_param.sched_priority);
}
*/

BCPLWORD alsafn(BCPLWORD *a, BCPLWORD *g, BCPLWORD *W) {
  snd_pcm_t *rd_handle = 0;
  snd_pcm_t *wr_handle = 0;
  BCPLWORD fno = a[0];
  //printf("alsafn: Entered: fno=%lld\n", LL a[0]);

  switch(fno) {
    default:
      printf("alsafn: Unknown ALSA fno: "
  	     "fno=%lld a1=%lld a2=%lld a3=%lld a4=%lld\n",
	      LL fno, LL a[1], LL a[2], LL a[3], LL a[4]);
      return 0;

    case alsa_avail:    // sys(Sys_alsa, alsa_avail)
      // Return TRUE is ALSA ia available.
      return (BCPLWORD)-1;

  case alsa_setscheduler:
    //setscheduler();   // Not yet available
    return -1;
    
  case alsa_open_wav_input:
  case alsa_open_wav_output:
  { // res := sys(Sys_alsa, alsa_open_wav_input,  name, cb)
    // res := sys(Sys_alsa, alsa_open_wav_output, name, cb)
    
    // a1 = BCPL device name string, eg "hw:0,0" or "hw:1"
    // a2 = cb, the wav_input or wav_output control block
    // Return TRUE if successful

    int i;
    int err;
    BCPLWORD name         = a[1];     // Device name as a BCPL string
    BCPLWORD *cb          = &W[a[2]]; // Machine address of the control block
    
    BCPLWORD channels     = cb[0];    // 1=mono 2=stereo
    UBCPLWORD rate        = cb[1];    // Rate, typically 44100
    long unsigned int periodframes = cb[2];   // Number of frames per period
    BCPLWORD periodbytes  = cb[3];    // Period size in bytes
    BCPLWORD periodwords  = cb[4];    // Period size in words
    char *periodbuf = (char*)&W[cb[5]];
    // periodbuf is the M/C address of a BCPL buffer to hold one period
    // worth of raw frames. One or two 16-bit samples per frame.

    unsigned int rrate; // The actual rate to use, chosen by the device
                        // and placed in cb[1]

    UBCPLWORD origperiodframes = periodframes;
    snd_pcm_t *pcm_handle;
    snd_pcm_hw_params_t *hw_params;
    
    // Only allow signed 16-bit little ended samples
    // may allow big ended samples later
    snd_pcm_format_t wavformat = SND_PCM_FORMAT_S16_LE;
    
    char devname[64]; // For thedevice name as a C string, eg "hw:0,0"
    int dir;
 
    b2c_str(a[1], devname);
    printf("alsafn: devname=%s  channels=%lld rate=%lld "
	   "periodframes=%lld periodbytes=%lld periodwords=%lld\n",
	   devname, LL channels, LL rate,
	   LL periodframes, LL periodbytes, LL periodwords);

    if (fno==alsa_open_wav_input) {
      printf("alsafn: alsa_open_wav_input: devname=%s\n", devname);
      rd_handle = 0; // In case of error
      err = snd_pcm_open (&pcm_handle,
                          devname, // eg hw:1 as a C string
	                  SND_PCM_STREAM_CAPTURE,
	                  0);
    } else {
      printf("alsafn: alsa_open_wav_output: devname=%s\n", devname);
      wr_handle = 0; // In case of error
      err = snd_pcm_open (&pcm_handle,
                          devname, // eg hw:0 as a C string
		          SND_PCM_STREAM_PLAYBACK,
		          0);
    }

    if (err<0) {
      printf ("alsafn: cannot open audio device %s (%s)\n", 
               devname, snd_strerror (err));
      return 0;
    }

    snd_pcm_hw_params_alloca(&hw_params);

    err = snd_pcm_hw_params_any(pcm_handle, hw_params);
    if (err < 0) {
      printf("Broken configuration for %s PCM: no configurations available\n",
	      snd_strerror(err));
      return 0;
    }
      
    err = snd_pcm_hw_params_set_access(pcm_handle, hw_params,
 			               SND_PCM_ACCESS_RW_INTERLEAVED);
    if (err < 0) {
      printf("Access type not available: %s\n", snd_strerror(err));
      return 0;
    }
      
    err = snd_pcm_hw_params_set_format(pcm_handle,
				       hw_params,
				       wavformat);
    if (err < 0) {
      printf("Sample format not available: %s\n", snd_strerror(err));
      return 0;
    }
      
    err = snd_pcm_hw_params_set_channels(pcm_handle,
					 hw_params,
					 channels);
    if (err < 0) {
      printf("Channels count (%lld) not available: %s\n",
              LL channels, snd_strerror(err));
      return 0;
    }
      
    rrate = rate;
    //err = snd_pcm_hw_params_set_rate_near(pcm_handle, hw_params, &rrate, 0);
    err = snd_pcm_hw_params_set_rate(pcm_handle,
				     hw_params,
				     rate,
				     0);
    if (err < 0) {
      printf("Rate %lld Hz not available: %s\n", LL rate, snd_strerror(err));
      return 0;
    }
    if ((int)rrate != rate) {
      printf("Rate doesn't match (requested %lld Hz, get %lld Hz)\n",
	     LL rate, LL rrate);
      cb[1] = rrate;
    }

    { snd_pcm_uframes_t frames;
      snd_pcm_hw_params_set_period_size_near(pcm_handle,
                                             hw_params,
					     &periodframes,
					     &dir);
      if(frames!=origperiodframes) {
        printf("alsafn: Choose a periodframes=%lld instead of %lld\n",
	       LL frames, LL origperiodframes);
	cb[2]= periodframes;
      }
    }
    
    /* Write the parameters to the driver */
    err = snd_pcm_hw_params(pcm_handle, hw_params);
     if (err < 0) {
       fprintf(stderr,
              "unable to set hw parameters: %s\n",
               snd_strerror(err));
      return 0;
    }

    printf( "hw_params have been applied\n");
	
    snd_pcm_hw_params_get_period_size(hw_params,
                                      (snd_pcm_uframes_t *)&periodframes, &dir);
      
    //snd_pcm_hw_params_free (hw_params);

    //printf("hw_params freed\n");
      
    // Remember the handle for later audio calls
    if (fno==alsa_open_wav_input) {
      rd_handle = pcm_handle;
    } else {
      wr_handle = pcm_handle;
    }
    return -1; // Successful return
  }

  case alsa_pause_wav_input:  // Pause wave input sampling
  case alsa_resume_wav_input: // Restart wave input sampling
      return -1;

  case alsa_wav_read:
  { // Read all outstanding samples from a sound wave input device
    // storing them in buf. buf[0] is the position o the latest
    // sample. The samples in buf are pushed down when ever necessary.
    //   a1 = the wav_input control block, in BCPL address space.
    //   a2 = the buffer
    //   a3 = upb of the buffer

    int i;
    int err;
    BCPLWORD *cb           = &W[a[1]]; // Machine address of the control block
    BCPLWORD *buf          = &W[a[2]]; // M/C address of the 32-bit samples
    BCPLWORD upb           = a[3];     // The upb of buf in words
    BCPLWORD bufp          = buf[0];   // The number of samples in buf
    
    BCPLWORD channels      = cb[0];    // 1=mono 2=stereo
    UBCPLWORD rate         = cb[1];    // Rate, typically 44100
    UBCPLWORD periodframes = cb[2];   // Number of frames per period
    BCPLWORD periodbytes   = cb[3];    // Period size in bytes
    BCPLWORD periodwords   = cb[4];    // Period size in words
    char *periodbuf = (char*)&W[cb[5]];
    // periodbuf is the M/C address of a BCPL buffer to hold one period
    // worth of raw frames. One or two 16-bit samples per frame.

    BCPLWORD len = 0;

    if(rd_handle==0) {
      printf("Error; Trying to read when the audio input stream is not active\n");
      buf[bufp] = 0;
      return 0;
    }
      
    printf("alsafn: wav_read called buf=%lld upb=%lld p=%lld\n",
	   LL a[1], LL upb, LL bufp);
    return 0;
      
    while(1)
    { // Loop reading microphone samples and transferring them to buf
       int len = snd_pcm_readi(rd_handle, periodbuf, periodframes);
      // len is either negative or the number of frames read

      if (len == -EPIPE) {
        // EPIPE means overrun
        fprintf(stderr, "overrun occurred\n");
        snd_pcm_prepare(rd_handle);
        bufp = 0;
	buf[0] = bufp;
	continue;
      }
      
      if (len < 0) {
        fprintf(stderr,
                "error from read: %s\n",
                snd_strerror(len));
	return 0;
      }
      
      if (len != (int)periodframes) {
        fprintf(stderr, "short read, read %lld frames\n", LL len);
      }
      
      { // len is the number of new samples in periodbuf
	// copy them to buf, pushingsamples down buf if necessary
        short *sv = (short*)periodbuf;
        int count = len*2;
        int j = 0;
        printf("count=%lld\n", LL count);
        for (j = 0; j<count; j++)
        { if(j%10 == 0) printf("\n%6lli: ", LL j);
	  printf(" X%6d", (int)sv[j]);
        }
        printf("\n");
	exit(0);
      }

     return -1;
    }
  }
  
  case alsa_close_wav_input:  // Close sound wave input device
  { // Read samples from a sound wave input device
    //   a1 = the wav_input control block, in BCPL address space.
    //        This control block holds the wav_input handle
    // The result is  0 if successful
    //            or -1 if error
    int i;
    BCPLWORD *cb      = &W[a[1]];  // Machine address of the control block
    BCPLWORD format   = cb[0];     // eg 16 for S16_LE, 8 for U8
    BCPLWORD channels = cb[1];     // 1=mono 2=stereo
    BCPLWORD rate     = cb[2];     // Typically 44100
    BCPLWORD *buf     = &W[a[2]];  // M/C address of the samples
    BCPLWORD upb      = a[3];      // The upb of buf
    BCPLWORD p        = cb[5];     // Subscript of buf just after the
                                   // last sample.
    int err;
    BCPLWORD len = 0;
      
    printf("alsafn: wav_close_inpu called a[1]=%lld a[2]=%lld a[3]=%lld\n",
	   LL a[1], LL a[2], LL a[3]);

    printf("alsafn: About to call snd_pcm_close\n");
    err = snd_pcm_close(rd_handle);
    // err is either negative if failure
    printf("alsafn: err=%lld\n", LL err);

    if (err < 0) {
      printf ("error in snd_pcm_readi (%s)\n",
               snd_strerror (err));
      return -1;
    }
    return 0;
  }

  case alsa_wav_write:
  { // Write n bytes of sound samples
    //   a1 = the file descriptor
    //   a2 = the buffer
    //   a3 = the number of bytes to write
    //   res = the number of bytes actually transferred, -1 if error
    return -1;
  }

  case alsa_close_wav_output:
    // Close a sound wave output device
    // a1 = the file (or device) descriptor
    snd_pcm_close(wr_handle);
    return 0;


  case alsa_open_midi_input:
    // Open MIDI device for input
    // a1 = typically "/dev/midi", "/dev/dmmidi1" or a small integer
    // res is the file (or device) descriptor of the opened device
    //     or -1 if error.
    { /* char *mididevname = b2c_str(a[1], chbuf1);
      int audio_fd = -1;

      audio_fd = open(osfname(mididevname, chbuf2), O_RDONLY, 0);
      // Returns fd or -1

      return audio_fd;
      */
      return -1;
    }

  case alsa_midi_read:
    // Read n MIDI bytes from a MIDI input device
    // a1 = the file (or device) descriptor
    // a2 = the buffer
    // a3 = the number of MIDI bytes to write
    // res = the number of bytes actually transferred
    //       or -1 if error
    { /* int midi_fd = a[1];
      char *buf = (char*)(&W[a[2]]);
      int n = a[3];  // Number of bytes to read
      int len = read(midi_fd, buf, n);
      return len;  // Number of bytes actually or -1
      */
      return -1;
    }

  case alsa_close_midi_input:
    // Close a MIDI input device
    // a1 = the file (or device) descriptor
    return close(a[1]);

  case alsa_open_midi_output:
    // Open MIDI device for output
    // a1 = typically "/dev/midi", "/dev/dmmidi1" or a small integer
    // res is the file (or device) descriptor of the opened device
    //     or -1 if error.
    { /* char *mididevname = b2c_str(a[1], chbuf1);
      int audio_fd = -1;

      audio_fd = open(osfname(mididevname, chbuf2), O_WRONLY, 0);
      // Returns fd or -1

      return audio_fd;
      */
      return -1;
    }

  case alsa_midi_write1:
    // Write a one byte MIDI message
    // a1 = the file descriptor
    // a2 = the status byte
    { /* int midi_fd = a[1];
      char buf[4];
      buf[0] = a[2];
      int len = write(midi_fd, buf, 1);
      return len;  // Number of bytes written or -1
      */
      return -1;
    }

  case alsa_midi_write2:
    // Write a two byte MIDI message
    // a1 = the file descriptor
    // a2 = the status byte
    // a3 = the second byte
    { /* int midi_fd = a[1];
      char buf[4];
      buf[0] = a[2];
      buf[1] = a[3];
      int len = write(midi_fd, buf, 2);
      return len;  // Number of bytes written or -1
      */
      return -1;
    }

  case alsa_midi_write3:
    // Write a three byte MIDI message
    //   a1 = the file descriptor
    //   a2 = the status byte
    //   a3 = the second byte
    //   a4 = the third byte
    { /* int midi_fd = a[1];
      char buf[4];
      buf[0] = a[2];
      buf[1] = a[3];
      buf[2] = a[4];
      int len = write(midi_fd, buf, 3);
      return len;  // Number of bytes written or -1
      */
      return -1;
    }

  case alsa_midi_write:
    // Write bytes to a MIDI device
    //   a1 = the file descriptor
    //   a2 = the buffer
    //   a3 = the number of MIDI bytes to write
    //   res = the number of bytes actuallty transferred or -1
    { /* int midi_fd = a[1];
      char *buf = (char*)(&W[a[2]]);
      int n = a[3];  // Number of bytes to write
      int len = write(midi_fd, buf, n);
      return len;  // Number of bytes written or -1
      */
      return -1;
    }

  case alsa_close_midi_output:
    // Close a MIDI output device
    // a1 = the file (or device) descriptor
    return close(a[1]);
      
  case alsa_init:
    // sys(Sys_alsa, alsa_init)
    return (BCPLWORD)0;

  case alsa_longname:
    // sys(Sys_alsa, alsa_longname, cardno, str)
    { BCPLWORD n    = a[1]; // ALSA card number
      BCPLWORD bstr = a[2]; // BCPL string to hold the long name
      char *cardname[256];
      int err;
      //printf("alsa_longname1: n=%lld bstr=%lld\n", LL n, LL bstr);
      err = snd_card_get_longname((int)n, cardname);
      if (err) return (BCPLWORD)0;
      //printf("alsa_Longname2: %s\n", *cardname);
      //printf("alsa_longname3: calling c2b_str(**cardname, bstr)\n");
      err = (BCPLWORD)c2b_str(*cardname, bstr);
      //printf("alsa_longname4: It returned %lld\n", LL err);
      return (BCPLWORD)err;
    }



    
  }  // End of switch

// Note that it may be necessary to run alsamixer to enable the sound
// device and adjust its volume setting.

}
#endif

#ifdef forWIN32
/******************** WIN32 Version *********************************/

/* Declare the Win32 control block structure */

#define INP_BUFFER_SIZE (4096*2/4)
#define OUTP_BUFFER_SIZE (4096*2)

typedef struct waveInCB { /* The wave input control block structure */
  WAVEHDR *pWaveHdr1, *pWaveHdr2;
  PBYTE pBuf1, pBuf2;
  HWAVEIN hWaveIn;
  int currbufno; /* =1 or =2 */
  int pos; // Position of next sample in the current buffer
} waveInCB;

typedef struct waveOutCB { /* The wave output control block structure */
  WAVEHDR *pWaveHdr1, *pWaveHdr2;
  PBYTE pBuf1, pBuf2;
  HWAVEOUT hWaveOut;
  int currbufno; /* =1 or =2 */
  int pos; // Position of next sample to put in the current buffer
} waveOutCB;

struct midiInCB { /* The MIDI input control block structure */
  int n;
};

struct midiOutCB { /* The MIDI input control block structure */
  int n;
};

BCPLWORD soundfn(BCPLWORD *args, BCPLWORD *g) {
  //printf("soundfn: fno=%lld a1=%lld a[2]=%lld a[3]=%lld a[4]=%lld\n",
  //        LL a[0], LL a[1], LL a[2], LL a[3], LL a[4]);

  switch(a[0]) {
  default:
    printf("soundfn: Unknown sound operation %lld\n", LL a[0]);
    return 0;

  case 0:  // Test for sound
    //printf("The Win32 sound functions are available\n");
    return -1; /* Sound is available */

  case 1: // Open Win32 sound wave device for input
    { char *micname = 0;//b2c_str(a[1], chbuf1); // Wave input device
      int format   = a[2];  // Typically 16 for S16_LE
                               // ie 16-bit signed little ender samples
      int channels = a[3];  // 1=mono or 2=stereo
      int speed    = a[4];  // Typically 44100 samples per second

// Allocate control block
// 2 hdrs, 2 bufs waveIn handle
// call waveInOpen
// Set up headers and prepare them
// (The buffers will now begin to fill with samples)
// Return the control block (or -1).

      waveInCB *wicb = (waveInCB*)malloc(sizeof(waveInCB));
      WAVEFORMATEX waveform;

      if(wicb==NULL) {
	printf("Unable to allocate waveInCB\n");
        return -1;
      }

      wicb->pBuf1 = malloc(INP_BUFFER_SIZE);
      wicb->pBuf2 = malloc(INP_BUFFER_SIZE);
      wicb->pWaveHdr1 = malloc(sizeof(WAVEHDR));
      wicb->pWaveHdr2 = malloc(sizeof(WAVEHDR));
      wicb->currbufno = 1;
      wicb->pos = 0;

      // Assume 16 bit mono!!
      waveform.wFormatTag      = WAVE_FORMAT_PCM;
      waveform.nChannels       = channels; // eg 1
      waveform.nSamplesPerSec  = speed;    // eg 44100
      waveform.nAvgBytesPerSec = channels*2*speed;
      waveform.nBlockAlign     = 1;
      waveform.wBitsPerSample  = format; //16;
      waveform.cbSize          = 0;
   
      if(waveInOpen(&(wicb->hWaveIn), WAVE_MAPPER, &waveform, 0, 0, 0)) {
        // Failed to open wave in device
        free(wicb->pBuf1);
        free(wicb->pBuf2);
        free(wicb->pWaveHdr1);
        free(wicb->pWaveHdr2);
        free(wicb);
	printf("Failed to open wave in device\n");
        return -1;
      }

      //printf("Successfully opened waveIn device\n");

      // Setup both headers and prepare them

      wicb->pWaveHdr1->lpData          = wicb->pBuf1;
      wicb->pWaveHdr1->dwBufferLength  = INP_BUFFER_SIZE;
      wicb->pWaveHdr1->dwBytesRecorded = 0;
      wicb->pWaveHdr1->dwUser          = 0;
      wicb->pWaveHdr1->dwFlags         = 0;
      wicb->pWaveHdr1->dwLoops         = 1;
      wicb->pWaveHdr1->lpNext          = NULL;
      wicb->pWaveHdr1->reserved        = 0;

      waveInPrepareHeader(wicb->hWaveIn, wicb->pWaveHdr1, sizeof(WAVEHDR));

      wicb->pWaveHdr2->lpData          = wicb->pBuf2;
      wicb->pWaveHdr2->dwBufferLength  = INP_BUFFER_SIZE;
      wicb->pWaveHdr2->dwBytesRecorded = 0;
      wicb->pWaveHdr2->dwUser          = 0;
      wicb->pWaveHdr2->dwFlags         = 0;
      wicb->pWaveHdr2->dwLoops         = 1;
      wicb->pWaveHdr2->lpNext          = NULL;
      wicb->pWaveHdr2->reserved        = 0;

      waveInPrepareHeader(wicb->hWaveIn, wicb->pWaveHdr2, sizeof(WAVEHDR));

      // Add the buffers

      waveInAddBuffer(wicb->hWaveIn, wicb->pWaveHdr1, sizeof(WAVEHDR));
      waveInAddBuffer(wicb->hWaveIn, wicb->pWaveHdr2, sizeof(WAVEHDR));

      // Begin sampling
      waveInStart(wicb->hWaveIn);

      if(0)
      { int i, oldk1=0, oldf1=0, oldk2=0, oldf2=0;

        for(i=1; i<100000000; i++) {
          int k1 = wicb->pWaveHdr1->dwBytesRecorded;
          int f1 = wicb->pWaveHdr1->dwFlags;
          int k2 = wicb->pWaveHdr2->dwBytesRecorded;
          int f2 = wicb->pWaveHdr2->dwFlags;
          if(oldk1!=k1 || oldf1!=f1 || oldk2!=k2 || oldf2!=f2)
            printf("%10d:  %7d  %8x  %7d %8x\n", i, k1, f1, k2, f2);
          oldk1 = k1;
          oldf1 = f1;
          oldk2 = k2;
          oldf2 = f2;
	}
      }

      return (BCPLWORD)wicb;

      //    rderr:
      //if(wicb) close(wicb->hWaveIn);

      return -1;
    }

  case 2:  // Pause wave input sampling
  case 3:  // Restart wave input sampling
      return -1;

  case 4:  // Read 32-bit samples from a sound wave input device.
           // The 16-bit samples are sign extended.
    { waveInCB *wicb = (waveInCB*) a[1]; // The Wave In control block
      BCPLWORD *v = (BCPLWORD*)(&W[a[2]]); // vector for samples
      int n = a[3];  /* Number of samples to read */
      int len = 0;      // Number of samples tranferred to v so far

      int currbufno = wicb->currbufno;
      WAVEHDR *waveHdr = (WAVEHDR*)(currbufno==1    ?
                                    wicb->pWaveHdr1 :
                                    wicb->pWaveHdr2);

      while(waveHdr->dwFlags & WHDR_DONE) {
        short *buf = (short*)(currbufno==1 ? wicb->pBuf1 : wicb->pBuf2);
        int pos =  wicb->pos;
        int samplecount = (waveHdr->dwBytesRecorded)/2;
        // Copy samples

	//printf("case 4: pos=%lld samplecount=%lld len=%lld n=%lld\n",
        //      LL pos, LL samplecount, LL len, LL n);

        while(pos<samplecount && len<n) {
          v[len++] = buf[pos++];
          //printf("%7lld: %6lld\n", LL len, LL v[len-1]);
	}

        wicb->pos = pos;

        if(pos>=samplecount) {
          waveInAddBuffer(wicb->hWaveIn, waveHdr, sizeof(WAVEHDR));
          // Select the other buffer
          currbufno = 1+2-currbufno;
          wicb->currbufno = currbufno;
          wicb->pos = 0;
          waveHdr = (WAVEHDR*)(currbufno==1    ?
                               wicb->pWaveHdr1 :
                               wicb->pWaveHdr2);
	  //printf("waveInAddBuffer called -- now using buf%d\n", currbufno);
          continue;
        }

        if(len>=n) break;
      }

      return len;  /* Number of samples tranferred */
    }

  case 5:  // Close sound wave input device
    { waveInCB *wicb = (waveInCB*) a[1]; // The Wave In control block
      int currbufno = wicb->currbufno;
      WAVEHDR *waveHdr = (WAVEHDR*)(currbufno==1    ?
                                    wicb->pWaveHdr1 :
                                    wicb->pWaveHdr2);
      int rc;

      //printf("case 5: closing wave in device\n");
      waveInReset(wicb->hWaveIn);

      rc = waveInUnprepareHeader(wicb->hWaveIn,
                                 wicb->pWaveHdr1, sizeof(WAVEHDR));
      if(rc!=MMSYSERR_NOERROR)
        printf("soundfn.c: Unable to UnprepareHeader of Hdr1\n");
      rc = waveInUnprepareHeader(wicb->hWaveIn,
                                 wicb->pWaveHdr2, sizeof(WAVEHDR));
      if(rc!=MMSYSERR_NOERROR)
        printf("soundfn.c: Unable to UnprepareHeader of Hdr2\n");

      free(wicb->pBuf1);
      free(wicb->pBuf2);
      free(wicb->pWaveHdr1);
      free(wicb->pWaveHdr2);
      free(wicb);
      //printf("WAV in device closed\n");

      return -1;
    }

    // **************************  WAVE Output

  case 6: // Open Win32 sound wave device for output
          // rc := sys(Sys_sound, snd_waveOutOpen, dev, format, mode, rate)
    { char *devname = 0;//b2c_str(a[1], chbuf1); // Wave output device
      int format   = a[2];  /* Typically 16 or 8 */
      int channels = a[3];  // 1=mono or 2=stereo
      int speed    = a[4];  // Typically 44100 samples per second
      int bitsPerSample = 16;
      int rc=0;

// Allocate control block
// 2 hdrs, 2 bufs and the waveIn handle
// call waveOutOpen
// Set up the headers and prepare them
// Return the control block (or -1).

      waveOutCB *wocb = (waveOutCB*)malloc(sizeof(waveOutCB));
      WAVEFORMATEX waveform;

      if(wocb==NULL) {
	printf("Unable to allocate waveOutCB\n");
        return -1;
      }

      wocb->pBuf1 = malloc(OUTP_BUFFER_SIZE);
      wocb->pBuf2 = malloc(OUTP_BUFFER_SIZE);
      wocb->pWaveHdr1 = malloc(sizeof(WAVEHDR));
      wocb->pWaveHdr2 = malloc(sizeof(WAVEHDR));
      wocb->currbufno = 1; // Next buffer to used (1 or 2)
      wocb->pos = 0;       // Position of next sample to place in buf

      //printf("Calling waveOutOpen format=%d channels=%d rate=%d\n",
      //        format, channels, speed);
      // Assume 16 bit mono!!
      waveform.wFormatTag      = WAVE_FORMAT_PCM;
      waveform.nChannels       = channels; // eg 1
      waveform.nSamplesPerSec  = speed;    // eg 44100
      waveform.nAvgBytesPerSec = channels*2*speed;
      waveform.nBlockAlign     = 1;
      waveform.wBitsPerSample  = format; //16;
      waveform.cbSize          = 0;
   
      rc = waveOutOpen(&(wocb->hWaveOut), WAVE_MAPPER, &waveform, 0, 0, 0);
      if(rc != MMSYSERR_NOERROR) {
        // Failed to open wave out device
        char mess[256];
        waveOutGetErrorText(rc, mess, 256);
        printf("Error: %s\n", mess);
        free(wocb->pBuf1);
        free(wocb->pBuf2);
        free(wocb->pWaveHdr1);
        free(wocb->pWaveHdr2);
        free(wocb);
	printf("Failed to open wave out device\n");
        return -1;
      }

      //printf("Successfully opened waveOut device\n");

      // Setup both headers and prepare them

      wocb->pWaveHdr1->lpData          = wocb->pBuf1;
      wocb->pWaveHdr1->dwBufferLength  = OUTP_BUFFER_SIZE;
      wocb->pWaveHdr1->dwBytesRecorded = 0;
      wocb->pWaveHdr1->dwUser          = 0;
      wocb->pWaveHdr1->dwFlags         = 0;
      wocb->pWaveHdr1->dwLoops         = 1;
      wocb->pWaveHdr1->lpNext          = NULL;
      wocb->pWaveHdr1->reserved        = 0;

      waveOutPrepareHeader(wocb->hWaveOut, wocb->pWaveHdr1, sizeof(WAVEHDR));
      wocb->pWaveHdr1->dwFlags         |= WHDR_DONE;

      wocb->pWaveHdr2->lpData          = wocb->pBuf2;
      wocb->pWaveHdr2->dwBufferLength  = OUTP_BUFFER_SIZE;
      wocb->pWaveHdr2->dwBytesRecorded = 0;
      wocb->pWaveHdr2->dwUser          = 0;
      wocb->pWaveHdr2->dwFlags         = 0;
      wocb->pWaveHdr2->dwLoops         = 1;
      wocb->pWaveHdr2->lpNext          = NULL;
      wocb->pWaveHdr2->reserved        = 0;

      waveOutPrepareHeader(wocb->hWaveOut, wocb->pWaveHdr2, sizeof(WAVEHDR));
      wocb->pWaveHdr2->dwFlags         |= WHDR_DONE;

      return (BCPLWORD)wocb;

    wrerr:
      if(wocb) close(wocb->hWaveOut);

      return -1;
    }

  case 7:  // Write 32-bit samples to a sound wave output device
           // a[1] is the waveOut control block
           // a[2] is the buffer of 32-bit samples to write
           // a[3] is the number of 32-bit sample to write.
           // If the current buffer can take more samples,  copy data
           // into it.
           // When full call waveOutWrite and swap buffers and hdrs
           // and copy samples into the other buffer if possible, as above.
           // When no more samples can be written, return the number of 
           // samples actually written.
           // If the number of bytes written is less than the number in
           // the data buffer, the caller should delay a while and try again.

    { waveOutCB *wocb = (waveOutCB*) a[1]; // The Wave Out control block
      BCPLWORD *v = (BCPLWORD*)(&W[a[2]]); // vector of samples to write
      int n = a[3];  /* Number of samples to write */
      int len = 0;      // Number of samples tranferred from v so far

      int currbufno = wocb->currbufno;
      WAVEHDR *waveHdr = (WAVEHDR*)(currbufno==1    ?
                                    wocb->pWaveHdr1 :
                                    wocb->pWaveHdr2);

      //printf("Write 32-bit wave samples, currbufno=%d\n", currbufno);
      //printf("dwFlags=%8x\n\n", waveHdr->dwFlags);

      while(waveHdr->dwFlags & WHDR_DONE) {
        short *buf = (short*)(currbufno==1 ? wocb->pBuf1 : wocb->pBuf2);
        int pos =  wocb->pos;
        int samplecount = (waveHdr->dwBufferLength)/2; // Buffer size
        // Copy samples into a device buffer
        //printf("Writing samples\n");

        while(pos<samplecount && len<n) {
          //printf("%10d\n", v[len]);
          buf[pos++] = v[len++];
	}

        wocb->pos = pos;

        if(pos>=samplecount) {
          waveOutWrite(wocb->hWaveOut, waveHdr, sizeof(WAVEHDR));
	  //printf("waveOutWrite called using buf%d\n", currbufno);
          // Select the other buffer
          currbufno = 1+2-currbufno;
          wocb->currbufno = currbufno;
          wocb->pos = 0;
          waveHdr = (WAVEHDR*)(currbufno==1    ?
                               wocb->pWaveHdr1 :
                               wocb->pWaveHdr2);
          continue;
        }

        if(len>=n) break;
      }

      //printf("%d samples written\n", len);
      return len;  /* Number of samples tranferred */
    }

  case 8:  // Close sound wave output device
           // a[1] is zero or the opened wave out control block
           // If zero return TRUE for success
           // othewise
           // Unprepare both hdrs
           // and freevec both hdrs and buffers
           // Return TRUE for success.

    { waveOutCB *wocb = (waveOutCB*) a[1]; // The Wave Out control block
      int currbufno = wocb->currbufno;
      WAVEHDR *waveHdr = (WAVEHDR*)(currbufno==1    ?
                                    wocb->pWaveHdr1 :
                                    wocb->pWaveHdr2);
      int rc;

      //printf("Closing the wave output device\n");

      waveOutReset(wocb->hWaveOut);

      rc = waveOutUnprepareHeader(wocb->hWaveOut,
                                  wocb->pWaveHdr1, sizeof(WAVEHDR));
      if(rc!=MMSYSERR_NOERROR)
        printf("soundfn.c: Unable to UnprepareHeader of Hdr1\n");
      rc = waveOutUnprepareHeader(wocb->hWaveOut,
                                  wocb->pWaveHdr2, sizeof(WAVEHDR));
      if(rc!=MMSYSERR_NOERROR)
        printf("soundfn.c: Unable to UnprepareHeader of Hdr2\n");

      free(wocb->pBuf1);
      free(wocb->pBuf2);
      free(wocb->pWaveHdr1);
      free(wocb->pWaveHdr2);
      free(wocb);
      //printf("soundfn: Wave Out device closed\n");

      return -1;
    }

     // **************************  MIDI Input

  case 9:  // Open a MIDI device for input
    return -1;

  case 10: // Read bytes from a MIDI input device
    return -1;

  case 11: // Close a MIDI input device
    //return close(a[1]);
    return -1;


    // **************************  MIDI Output

  case 12: // Open MIDI device for output
    { HMIDIOUT hMidiOut;
      int rc = midiOutOpen(&hMidiOut, -1, 0, 0, 0); // Using MIDIMAPPER
      //if(rc==0) printf("Successfully opened MIDI output device\n");
      if(rc) { 
        //printf("Unable to open MIDI output device\n");
        return 0;
      }
      rc = midiOutSetVolume(hMidiOut, 0xFFFF); // Set max volume
      //if(rc) printf("Unable to set Midi volume, rd=%d\n", rc);
      //if(rc==MMSYSERR_INVALHANDLE) printf("INVALHANDLE\n");
      //if(rc==MMSYSERR_NOMEM) printf("NOMEM\n");
      //if(rc==MMSYSERR_NOTSUPPORTED) printf("NOTSUPPORTED\n");
      return (BCPLWORD)hMidiOut;
    }

  case 13: // Write a one byte MIDI message
    { HMIDIOUT handle = (HMIDIOUT)a[1];
      DWORD data=0;
      ((char*)&data)[0] = a[2];
      //printf("Writing data = '%8X'\n", data);
      return midiOutShortMsg(handle, data);
    }

  case 14: // Write a two byte MIDI message
    { HMIDIOUT handle = (HMIDIOUT)a[1];
      DWORD data=0;
      ((char*)&data)[0] = a[2];
      ((char*)&data)[1] = a[3];
      //printf("Writing data = '%8X'\n", data);
      return midiOutShortMsg(handle, data);
    }

  case 15: // Write a three byte MIDI message
    { HMIDIOUT handle = (HMIDIOUT)a[1];
      DWORD data=0;
      ((char*)&data)[0] = a[2];
      ((char*)&data)[1] = a[3];
      ((char*)&data)[2] = a[4];
      //printf("Writing data = '%8X'\n", data);
      return midiOutShortMsg(handle, data);
    }

  case 16: // Write (many) bytes to a MIDI output device
           // Typically used only for SysEx messages
           // Returns 0 if successful
    { HMIDIOUT handle = (HMIDIOUT)a[1];
      MIDIHDR midiHdr;
      UINT    err;
      int i;
      char *buf = (char*)(&W[a[2]]);
      int n = a[3];  /* Number of bytes to write */

      midiHdr.lpData = (LPBYTE)buf;
      midiHdr.dwBufferLength = n;
      midiHdr.dwFlags = 0;

      err = midiOutPrepareHeader(handle, &midiHdr, sizeof(MIDIHDR));
      if(!err) {
        // output the SysEx message
        err = midiOutLongMsg(handle, &midiHdr, sizeof(MIDIHDR));
        while(MIDIERR_STILLPLAYING ==
	      midiOutUnprepareHeader(handle, &midiHdr, sizeof(MIDIHDR))) {
          Sleep(10);
        }
      }
      return err;
    }

  case 17: // Close MIDI output device
      midiOutClose((HMIDIOUT)a[1]);
      return 0;
  }
}
#endif




#endif



