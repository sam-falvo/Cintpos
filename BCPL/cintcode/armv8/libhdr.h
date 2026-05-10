// Standard BCPL header

// Modified by Martin Richards (c) 15 May 2013
// Simplified for Raspian Linux by DJA 20 June 2016

MANIFEST 
{
  B2Wsh = 2  // 32-bit implementation
}

// Globals used in the standard (single threaded) BCPL Cintcode System
GLOBAL 
{
  globsize:            0
  start:               1
  stop:                2
  sys:                 3  // ALIB   DJA 20/6/16
  muldiv:              5  // ALIB   DJA 20/6/16
  changeco:            6  // ALIB   DJA 20/6/16
  currco:              7
  colist:              8
  rootnode:            9  // For compatibility with native BCPL
  result2:            10
  returncode:         11
  cis:                12
  cos:                13
  currentdir:         14
  level:              15
  longjump:           16
  createco:           17
  deleteco:           18
  callco:             19
  cowait:             20
  resumeco:           21
  initco:             22
  startco:            23
  getvec:             25
  backtrace:          26  // DJA 17/06/2016
  freevec:            27
  abort:              28
  packstring:         30
  unpackstring:       31
  getword:            32
  putword:            33
  randno:             34
  setseed:            35
  rdch:               38
  binrdch:            39
  unrdch:             40
  wrch:               41
  binwrch:            42
  readwords:          44
  writewords:         45
  findinput:          48
  findoutput:         49
  findinoutput:       50
  pathfindinput:      53
  selectinput:        56
  selectoutput:       57
  input:              58
  output:             59
  endread:            60
  endwrite:           61
  endstream:          62
  rewindstream:       65
  appendstream:       66
  deletefile:         76
  renamefile:         77
  copydir:            79
  locatedir:          80
  createdir:          82
  readn:              83
  newline:            84
  writed:             85
  writen:             86
  writehex:           87
  writeoct:           88
  writes:             89
  writet:             90
  writeu:             91
  writez:             92
  writef:             94  //BLIB
  sawritef:           95
  capitalch:          96
  compch:             97
  compstring:         98
  copystring:         99
  setbit:            112
  testbit:           113
  newpage:           120
  stackfree:         123  // Returns the number of free stack locations
  randseed:          127
}

MANIFEST 
{
  ug = 200   // First user global

  bytesperword    = 1<<B2Wsh
  bitsperbyte     = 8
  bitsperword     = bitsperbyte * bytesperword
  minint          = 1<<(bitsperword-1)  // = #x80....0
  maxint          = minint - 1          // = #x7F....F

  endstreamch     = -1  // ch returned at EOF
  
  rtn_hdrsvar     = 0   // not used

  // Unicode encodings
  UTF8            = -1
  GB2312          = -2

  stackword       = #xFFFFFFFFABCD1234
  sectword        = #x000000000000FDDF
  entryword       = #x000000000000DFDF

  // co-routine stackbase offsets

  co_pptr         = 0
  co_parent
  co_list
  co_fn
  co_size
  co_c
}
