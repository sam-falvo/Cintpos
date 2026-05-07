/*
This is the header file for playmus.b

Wriiten by Martin Richards (c) February 2009
*/

GLOBAL {
// BGPM global variables
bg_s:ug
bg_t
bg_h
bg_p
bg_f
bg_c
bg_e
bg_ch

bgpmco

playmus_version // eg "Playmus v2.0"
sysin; sysout; sourcestream; tostream
sourcenamev; sourcefileno; sourcefileupb
getstreams
lineno        // lineno value of the current character in bg_ch or ch.
plineno       // lineno value of the previous character.
tokln         // lineno value of the first character of the current token

startbarno  // Number of the first bar to play
endbarno    // Number of the last bar to play
start_msecs // Midi msecs of the start of the first bar to play
end_msecs   // Midi msecs of the end of the last bar to play

optPp; optLex; optTree; optPtree
optStrace   // Trace the creation of parse tree nodes.
optNtrace   // Trace notes as they are generated.
optMtrace   // Trace Midi commands as they are played.
tempoadj    // playing tempo adjustment as a percentage, 100=don't adjust
accompany   // =TRUE if accompaning
pitch       // Number of semitones to transpose every note up by
graphdata   // =TRUE to generate graph data
calibrating // =TRUE if calibrating Midi to Mic delay
waiting     // =TRUE if playmidi waits before playing
quitting    // =TRUE if quitting
killco      // Killer coroutine

notecount   // Count of recognised note events
totalerr    // Sum of all note event errors

// The free lists for ewach size
mk1list; mk2list; mk3list; mk4list; mk5list
mk6list; mk7list; mk8list; mk9list

blklist  // List of blocks of work space
blkp
blkt
blkitem
bg_base; bg_baseupb; rec_p; rec_l; fin_p; fin_l

debugv              // To hold system debug flags 0 to 9
errcount; errmax
strv       // Short term sting buffer, typically for op names

ch; chbuf; chbufln; chcount; tree
token
FLT numval
noteletter; prevnoteletter
prevoctave; reloctave; notenumber
notesharps; notelengthnum; prevlengthnum; dotcount
argp; argt
wrc; wrs; chpos; charv; wordnode; stringval
nametable
sclt; sclb  // qbeat scaling rational number sclt/scalb
            // with common factors removed. These are
            // used by the \tuplet construction.
qbeat       // The number of the current q beat.
absmsecs    // Number of milli seconds since the start of
            // the composition.

// Functions in section Playmus

die         // Function to cause a coroutine to commit suicide
concatext   // Concatenate two strings if the first does not contain
            // a dot.

// Functions in section Bgpm

bgputch
bgpush
bggetch
arg
lookup
arg2str
define
bgpmfn
rdint
performget
evalarg
bgbexp
bgexp
getargch
rdnum
bgwrnum
wrpn
wrc
wrs
wrn
bg_error
error
prcall
btrace
wrcall
wrarg
wrenv
newvec
mk1; mk2; mk3; mk4; mk5; mk6; mk7; mk8; mk9
// Return blks to their free lists
unmk1; unmk2; unmk3; unmk4; unmk5; unmk6; unmk7; unmk8; unmk9

// Functions in section Lex

rch
lex
lookupword
dsw
declsyswords
wrchbuf
rdtag
rdstrch
formtree
prlineno
fatalerr
fatalsynerr
synerr
trerr
checkfor
rdscore
rdstring
rdnumber
rdlength
rdoctave
rdinteger
note2qlen
rdnoteprim
rdnoteitem
rdparlist
rdnoteseq
rdnotelist
rdshapeseq
mkmsecsmap
rdtupletqlen
applyshapeseq
applyshapeitem
containsshapeops
combineshapes
combineshape
fno
lno
opstr
prnote
prtree

// Globals in section Trans

trblock
prshapes
prshape

calcqlen

findshapedata
addshapedata
setscaleparams
setmsecsenv
gcd
barscan
trscore
trpartset
qbeatlength

currpartname

veclist        // List of vectors that must be freevec'd
pushval
pushmsecsval
pushshape
getval
shapelookup
istied
checktlist
prties

transposition  // Number of semitones to transpose this part up by

currqbeat      // The qbeat position of the current item
blkstartq
blkendq
currbarqbeat   // The qbeat position of the start of the current bar
currbeatqbeat  // The qbeat position of the start of the current beat

maxqbeat       // The largest value of currqbeat

prevnum        // =TRUE if the previous item in a shape list
               // was a number.
prevqlen       // The qlen of the previous space in s shape list 

firstnoteqbeat // Only used if bar zero exists

currbarno      // Current bar number used in trscores
maxbarno       // Total number of bars in the piece
currbeatno     // Current bar number used in trscores
maxbeatno      // Total number of beats in the piece,
               // ie the sum of beats in each bar
timesig_t      // The number of beats per bar
timesig_b      // The length number of a beat

barqerr

barno2qbeats
qbeats2barno

scbase  // Parameters for scaling localqbeat to absolute qbeat
scfac_t // using
scfac_b //        absq = scbase + muldiv(localq, scfact, scfacb)
        // or
qscale  //        absq = qscale(q)

q2msecs
barno2msecs

midilist       // The start of the midi list -> [link, msecs, <midi triple>]
midiliste      // Pointer to the last midi list item, or 0.

plist   // Previous tlist just before current par or tuplet construct.
pqpos   // Abs qbeat terminating position of items in ptlist
tlist   // Outstanding ties in the current note thread
tqpos   // Abs qbeat terminating position of items in tlist
clist   // Outstanding ties in the other concurrent note threads
cqpos   // Abs qbeat terminating position of items in clist

// Player globals
midichannel // 0 .. 15
micbuf
tempodata     // Mapping from absolute qbeat values to msecs
              // tempodata!i the time is msecs of absolute qbeat 32*i

barmsecs      // Mapping from bar number to midi msecs
beatmsecs     // mapping from beat number to midi msecs

solochannels  // Bit pattern with 1<<chan set if chan is a solo channel

baseday       // Used by getrealmsecs
rmsecs0       // real msecs at startmsecs

soundv        // Buffer of cumulative data
soundp        // Position of next element of soundv to be updated
soundval      // Latest soundv element
soundmsecs    // real msecs of latest sound sample in soundv

//trnoteseq
genmidi
apmidi

soundco       // Coroutine to read sound samples into soundv
keyco         // The coroutine reading the keyboard
playmidico    // The coroutine to output midi data

barenv        // The bar table environment
beatenv       // The beat table environment
qbeatsperbar  // The number of qbeats per bar
qbeatsperbeat // = 4096/timesigb, ie 1024 for crotchet beats
prevbarqbeat  // The qbeats value of the previous beat
beatcount     // Count of the most recent beat. In 6 8 time
              // beatcount will be 1, 2, 3, 4, 5,or 6. It must
              // be 1 at the next time signature or bar line. 

notecov       // Vector of recognition coroutines for notes 0..127
notecoupb     // Note coroutines are from votecov!1 to notecov!notecoupb
notecop       // Position of next note coroutine to run

freqtab       // Frequency table for notes 0..127
initfreqtab   // Function to initialise freqtab

eventv        // Circular table of recent events [mt, rt, weight, op]
              // Each event is separated from previous by at least 10 msecs.
              // op=-1 for a barline event, =-2 for a beat event,
              // otherwise it is a note-number.
eventp        // Position in eventv to store the next event item.
prevrt        // Real time of the most recent event in eventv, the next event
              // must be at least 10 msecs later.
pushevent     // Put and event in the eventv circular buffer.
newevents     // =TRUE when a newevent is in eventv. It is reset by calcrates

msecsbase     // Real time at first call of getrealmsecs
real_msecs    // msecs since msecsbase
midi_msecs    // Current midi time
interpolate
setenvlimits

variablevol   // TRUE means volume can change while a not is being played.
chanvol       // -1 or the current channel volume

// The following two variables are used to convert between real and midi msecs.

ocr; ocm; crate // The origin of the current play line
oer; oem; erate // The origin of the estimated play line

calcrates     // Function to compute new values for play_rate, play_offset
              // curr_rate, curr_offset and revert_msecs. These values are
              // based on their previous values and the events in eventv.

calc_msecs    // Real time when calcrates should next be called.

midifd
micfd



// Functions in section Writemidi

pushbyte
pushh
pushh
pushw
pushw24
pushstr
pushnum
pushpfx
packw
writemidi

// Functions in section Shapefns

getshapeval
shapeval


// Functions in section Playmidi

genrecogfn
getrealmsecs
notecofn
setfreqtab
checktab
findtimes
addevent
clearevents   // Remove all previous events
calcrates     // Calculate new estimated and correction play lines
soundcofn
playmidicofn
keycofn
playmidi
r2m_msecs     // (r, or, om, rate) This converts real to midi msecs using
              // om + muldiv(real_msecs-or, rate, 1000)
m2r_msecs     // (r, or, om, rate) This converts midi to real msecs using
              // or + muldiv(real_msecs-om, 1000, rate)
msecs2barno
msecs2beatno
msdelay
wrmid1; wrmid2; wrmid3
prmidilist
note2str       // s := note2str(noteno, str)
editnoteoffs
mergesort
mergelist


msecsenv    // Used by q2msecs
            // Each entry in this table is of the form [q, rate, msecs]
            // where q is the absolute qbeat position in the composition
            //       rate is the tempo after ajustment in msecs per qbeats.
            //  and  msecs is time of the start of qbeat q from the start
            //             of the composition.
            // The data in this environment is constructed by merging the
            // tempo and tempoadj. The tempo values are held in units of
            // msec per qbeat which is proportional to the inverse of 
            // beats per minute given by the user and intermediate values
            // are determined by linear interpolation on these values.
            // Suppose the tempo as specified by the user changes from
            // 100 to 200 beats per minute. Using linear interpolation
            // on these values would give 150 at the mid point, but if
            // linear interpolation on the msecs per qbeats was used
            // we get a different result. 100 beats per minute
            // corresponds to (60*1000)/(1024*100)=0.586 msecs/qbeat
            // and 200 corresponds tp 0.293. This gives a midpoint
            // value of 0.439 which corresponds to 133 beats per minute.
            // Using tempo rate in units of msecs/qbeat means that
            // tempoadj values must be divided rather than multiplied,
            // since, for instance, a tempoadj value of 2 would halve
            // the msecs per qbeat rate. Tempoadj values are thus held
            // as the inverse of the values supplied by the
            // user, and Intermediate tempoadj values are determined
            // using linear interpolation of these inverse values.
            // These modified tempo and tempoadj are both linear
            // functions of q. Their product is thus a quadratic
            // of the form r(q) = A + Bq + Cq^2. The time between
            // two qbeat values q1 and q2 is thus the integral of
            // r(q) between q1 and q2. The result is thus

            // Aq2 + Bq2^2/2 + Cq2^3/3 - Aq1 + Bq1^2/2 + Cq1^3/3

            // This formula is used to fill in the msecs values in
            // the msecs environment and it is also used when
            // calculating  intermediate times.

tempoenv
tempoadjenv

vibrateenv         // -> [upb, v, prevenv, sq,eq,def] etc
vibrateadjenv
vibampenv
vibampadjenv
volenv
voladjenv
legatoenv
legatoadjenv
delayenv
delayadjenv

msecsenv           // -> [upb, v, prevmsecsenv, sq,eq,-]
                   // v has a msecs value for the start of every
                   // 64 qbeat group enclosing sq to eq.
conductormsecsenv  // The conductor's version of msecsenv.
}

MANIFEST {
nametablesize = 541 // A prime number
blkupb = 10_000
micbufupb = 1023
soundvupb = #xFFFF  // Room for about 1.5 seconds of sound
eventvupb = 4*20-1  // Allow for 20 event items [rt, mt, weight, note]

// BGPM markers
s_eof     =  -2     // end of file character
s_eom     =  -3     // end of macro body character

// BGPM builtin macros
s_def     =  -4
s_set     =  -5
s_get     =  -6
s_eval    =  -7
s_lquote  =  -8
s_rquote  =  -9
s_comment = -10
s_char    = -11
s_rep     = -12
s_rnd     = -13  // Signed random number
s_urnd    = -14  // Unsigned random number

// BGPM special characters
c_call    = '$'
c_apply   = ';'
c_sep     = '!'
c_comment = '%' 
c_lquote  = '<'
c_rquote  = '>'
c_arg     = '#'

// General selectors for node fields
h1=0; h2; h3; h4; h5; h6; h7; h8; h9

// Lex tokens and other symbols
s_altoclef=1          // notelist -> [-, Altoclef, ln]
s_arranger            // notelist -> [-, Arranger, ln, str]
s_bank                // notelist -> [-, Bank, ln, byte, byte]
s_barlabel            // notelist -> [-, Barlabel, ln, str]
s_barline             // notelist -> [-, Barline, ln]
s_bassclef            // notelist -> [-, Bassclef, ln]
s_block               // notelist -> [-, Block, ln, noteseq, qlen]
s_colon               // shapelist -> [-, Colon, ln, qlen]
s_composer            // notelist -> [-, Composer, ln, str]
s_conductor           // conductor -> [-, Conductor, ln, note-item]
s_control             // notelist -> [-, Control, ln, byte, byte]
s_delay               // notelist -> [-, Delay, ln, note_item, shapelist]
s_delayadj            // notelist -> [-, Delayadj, ln, note_item, shapelist]
s_doublebar           // notelist -> [-, Doublebar, ln]
s_instrument          // notelist -> [-, Instrument, ln, str]
s_instrumentname      // notelist -> [-, Instrumentname, ln, str]
s_instrumentshortname // notelist -> [-, Instrumentshortname, ln, str]
s_keysig              // notelist -> [-, Keysig, ln, note, mode]
s_legato              // notelist -> [-, Legato, ln, note_item, shapelist]
s_legatoadj           // notelist -> [-, Legatoadj, ln, note_item, shapelist]
s_legon               // notelist -> [-, Legon, ln]
s_legoff              // notelist -> [-, Legoff, ln]

s_partset             // partset  -> [-, Partset,  ln, partlist,  qlen]
s_blockseq            // notelist -> [-, Blockseq, ln, notelist,  qlen]
s_noteseq             // notelist -> [-, Noteseq,  ln, notelist,  qlen]
s_shapeseq            // shapeseq -> [-, Shapeseq, ln, shapelist, qlen]
s_tenorclef           // notelist -> [-, Tenorclef, ln]

s_lcurly
s_lparen
s_lsquare
s_major               // A mode
s_minor               // A mode
s_msecsmap            // [-, Msecsmap, v]
s_name                // notelist -> [-, Name, ln, str]
s_neg
s_nonvarvol           // notelist -> [-, Nonvarvol, ln]
s_note                // notelist -> [-, Note, ln, <letter:sharps:n>, qlen]
s_notetied            // notelist -> [-, Notetied, ln, <letter:sharps:n>, qlen]
s_null                // notelist -> [-, Null, ln]
s_num                 // notelist -> [-, Num, ln, value]
                      // shapelist -> [-, Num, ln, value]
s_numtied             // notelist -> [-, Numtied, ln, value]
s_opus                // notelist -> [-, Opus, ln, str]
s_par                 // notelist -> [-, Par, ln, noteseqlist, qlen]
s_noteseqlist         // noteseqlist is a list of noteseqs
s_part                // partlist -> [-, Part, ln, notelist, channel]
s_partlabel           // notelist -> [-, partlabel, ln, str]
s_patch               // notelist -> [-, patch, ln, byte]
s_pedon               // notelist -> [-, Pedon, ln]
s_pedoff              // notelist -> [-, Pedoff, ln]
s_pedoffon            // notelist -> [-, Pedoffon, ln]
s_portaon             // notelist -> [-, Portaon, ln]
s_portaoff            // notelist -> [-, Portaoff, ln]
s_rcurly
s_repeatback          // notelist -> [-, Repeatback, ln]
s_repeatbackforward   // notelist -> [-, Repeatforwardback, ln]
s_repeatforward       // notelist -> [-, Repeatforward, ln]
s_rest                // notelist -> [-, Rest, ln, qlen]
s_rparen
s_rsquare
s_score               // score -> [-, Score, ln, str, conductor, parts]
s_space               // notelist -> [-, Space, ln, qlen]
s_star                // shapelist -> [-, Star, ln]
s_startied            // shapelist -> [-, Startied, ln]
s_string
s_softon              // notelist -> [-, Softon, ln]
s_softoff             // notelist -> [-, Softoff, ln]
s_solo                // partlist -> [-, Solo, ln, noteseq, channel]
s_tempo               // notelist -> [-, Tempo, ln, noteseq, shapeseq]
s_tempoadj            // notelist -> [-, Tempoadj, ln, noteseq, shapeseq]
s_tenorclef           // notelist -> [-, Tenorclef, ln]
s_timesig             // notelist -> [-, Timesig, ln, timesiga, timesigb]
s_title               // notelist -> [-, Title, ln, str]
s_transposition       // notelist -> [-, Transposition, ln, semitones_up]
s_trebleclef          // notelist -> [-, Trebleclef, ln]
s_tuplet              // notelist -> [-, Tuplet, ln, noteseq, shapeseq]
s_varvol              // notelist -> [-, Varvol, ln]
s_vibrate             // notelist -> [-, Vibrate, ln, noteseq, shapeseq]
s_vibrateadj          // notelist -> [-, Vibrateadj, ln, noteseq, shapeseq]
s_vibamp              // notelist -> [-, Vibamp, ln, noteseq, shapeseq]
s_vibampadj           // notelist -> [-, Vibampadj, ln, noteseq, shapeseq]
s_vol                 // notelist -> [-, Vol, ln, noteseq, shapeseq]
s_voladj              // notelist -> [-, Voladj, ln, noteseq, shapeseq]
s_volmap              // notelist -> [-, Volmap, ln, shapeseq]
s_word

// MIDI opcodes
midi_note_off     = #x80
midi_note_on      = #x90
midi_keypressure  = #xA0
midi_control      = #xB0
midi_progchange   = #xC0
midi_chanpressure = #xD0
midi_pitchbend    = #xE0
midi_sysex        = #xF0
}
