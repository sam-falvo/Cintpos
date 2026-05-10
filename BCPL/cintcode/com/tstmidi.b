// This is a MIDI output test program designed to write to the
// currently selected MIDI output device. It runs on the BCPL Cintcode
// system under Windows.

// Implemented by Martin Richards (c) 15 May 2008 

GET "libhdr"
GET "sound.h"

GLOBAL {
  stdin:ug
  stdout
  midiname
  midifd
  prog
  tempo
  msecsperbeat
  instr
  channelno
  legatoval
  volumelevel
}

MANIFEST { tickspersecond=1000 }

LET start() = VALOF
{ LET argv = VEC 50

  stdout := output()
  stdin  := input()
  tempo := 120*4
  msecsperbeat := 60_000 / tempo

  midiname := "/dev/snd/midiC1D0"
  midiname := "/dev/midi1"
  midifd := 0
  instr := 1
  

  UNLESS rdargs("INSTR/n,TEMPO/n", argv, 50) DO
  { writef("Bad arguments for tstmidi*n")
    RESULTIS 0
  }

  IF argv!0 DO instr    := !(argv!0)   // INSTR
  IF argv!1 DO tempo    := !(argv!1)   // TEMPO

  msecsperbeat := 60_000 / tempo

  midifd := sys(Sys_sound, snd_midiOutOpen, midiname)

  writef("midiname=%s midifd=%n*n", midiname, midifd)
  abort(1234)
  
  UNLESS midifd DO
  { writef("Trouble with %s*n", midiname)
    RESULTIS 0
  }

  writef("*nSending MIDI data instr=%n tempo=%n*n*n*n",
          instr, tempo)
  channel(1)  // select the MIDI channel (1..16)

  wrmid2(#xC0+channelno, instr)       // Set Program number
  delay(msecsperbeat)

  FOR patch = 0 TO 448 BY 10 DO
  { writef("*nPatch %n*n", patch+1)

    bankselect((93<<8) + (7 + patch/128))
    wrmid2(#xC0+channelno, patch & 127)       // Set Program number

    FOR i = 0 TO 24 DO
    { LET vol = 64+8*i

      IF vol>127 DO vol := 127

      vol := 20
      volume(100) // Select the volume
      legato(80)  // Select the percentage on time

      TEST i MOD 12 = 0
      THEN n1(60-12+i)
      ELSE n4(60-12+i) // Play a crochet
    }
  }

  delay(msecsperbeat)

  sys(Sys_sound, snd_midiOutClose, midifd) // Close the midi output device
  selectoutput(stdout)
  writef("End of test*n")

  RESULTIS 0
}

AND playnote(duration, n) BE
{ LET ontime  = duration*legatoval/100
  LET offtime = duration - ontime

  wrmid3(#x90+channelno, n, volumelevel/10)
  delay(ontime)
  wrmid3(#x80+channelno, n, 0)
  delay(offtime)
}

AND n1(n)  BE playnote(msecsperbeat*4,  n)
AND n2(n)  BE playnote(msecsperbeat*2,  n)
AND n4(n)  BE playnote(msecsperbeat,    n)
AND n8(n)  BE playnote(msecsperbeat/2,  n)
AND n16(n) BE playnote(msecsperbeat/4,  n)
AND n32(n) BE playnote(msecsperbeat/8,  n)
AND n64(n) BE playnote(msecsperbeat/16, n)

AND volume(vol) BE volumelevel := vol
AND legato(p) BE legatoval := p
AND channel(n) BE channelno := (n-1) & 15

AND bankselect(bank) BE
{ LET mm = bank>>8 & 255
  LET ll = bank    & 255
  wrmid3(#xB0+channelno, #x00, mm)
  wrmid3(#xB0+channelno, #x20, ll)
}
AND delay(msecs) BE
{ LET ticks = tickspersecond * msecs / 1000
  deplete(cos)
  sys(Sys_delay, ticks)
}

AND wrmid1(a) BE
{
  sawritef(" %x2*n", a)
  sys(Sys_sound, snd_midiOutWrite1, midifd, a)
}

AND wrmid2(a, b) BE
{
  sawritef(" %x2 %x2*n", a, b)
  sys(Sys_sound, snd_midiOutWrite2, midifd, a, b)
}

AND wrmid3(a, b, c) BE
{
  sawritef(" %x2 %x2 %x2*n", a, b, c)
  sys(Sys_sound, snd_midiOutWrite3, midifd, a, b, c)
}
