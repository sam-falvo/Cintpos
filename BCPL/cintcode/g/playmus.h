/*
This is the header file for playmus.b

Written by Martin Richards (c) November 2022

History

08 Dec 2022
Started a major revision of this program.
Reorganising how environments work.

01 Feb 2009
Initial implementation started

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

bgrec_p // For errors detected in BGPM
bgrec_l

bgpmco // The BGPM macro generator is implemented as a corouitne.
       // ch := callco(bgplco, 0)   get the next character
playmus_version // eg "Playmus v2.0"
sysin; sysout; sourcestream; tostream
sourcenamev; sourcefileno; sourcefileupb
getstreams
lineno        // lineno value of the current character in bg_ch or ch.
prevlineno    // lineno value of the previous character.
tokln         // lineno value of the first character of the current token

startbarno  // Number of the first bar to play
endbarno    // Number of the last bar to play
start_msecs // Midi msecs of the start of the first bar to play
end_msecs   // Midi msecs of the end of the last bar to play

optPp; optLex; optTree; optfaca
optStrace   // Trace the creation of parse tree nodes.
optNtrace   // Trace notes as they are generated.
optMtrace   // Trace Midi commands as they are played.
optgenmidi  // Generate a MIDI file
optplay     // Play the score on a midi device such as /dev/midi1

playrate    // playing tempo adjustment as a percentage, 100=don't adjust

accompany   // =TRUE if accompaning
pitch       // Number of semitones to transpose every note up by
graphdata   // =TRUE to generate graph data
calibrating // =TRUE if calibrating Midi to Mic delay
waiting     // =TRUE if playmidi waits before playing

quitting    // =TRUE if quitting
killco      // Killer coroutine

notecount   // Count of recognised note events
totalerr    // Sum of all note event errors

// The free lists for each size
mk1list; mk2list; mk3list; mk4list; mk5list
mk6list; mk7list; mk8list; mk9list; mk10list

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
FLT fnumval // Note numbers from BGPM are floating point.
intval      // >=0 if lex returned s_int ie if a posive number
            // was found containg no decimal point and is
	    // representable as an integer.
noteletter; prevnoteletter
prevoctave; reloctave; notenumber
notesharps;
n2qlen       // Convert note length number to a length in qbeats.
prevnoteqlen // qlen of the previous note, space or rest
prevdotcount // The dot count of the previous note, space or rest.q
noteqlen     // The nominal qlen of notes before the dots as set by lex
dotcount
sfac         // The shape scaling factor set by eg :s4.

FLT shapefaca // Shape factors used by setshapes when combining
FLT shapefacb // a shape values with their parent values.
              // Combined value is shapevala*val+shapefacb*pval
	      // Usually shapefaca+shapefacb = 1.0

argp; argt
chpos; charv; wordnode; stringval
nametable

qbeat       // The number of the current quantum beat.

// Section Playmus

die         // Function to cause a coroutine to commit suicide
concatext   // Concatenate two strings if the first does not contain
            // a dot.

// Section Bgpm

bgputch
bgpush
bggetch
arg
lookup
arg2str
define
bgpmfn
rdbgint
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
prcall
btrace
wrcall
wrarg
wrenv
newvec
mk1; mk2; mk3; mk4; mk5; mk6; mk7; mk8; mk9; mk10
// Return blks to their free lists
unmk1; unmk2; unmk3; unmk4; unmk5; unmk6; unmk7; unmk8; unmk9; unmk10

// Section Lex

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
rdint
rdinteger
note2qlen
rdnoteprim
rdnoteitem
rdparlist
rdblock

rdnoteseq
rdnotelist
rdshape
rdtupletqlen

fno
lno
opstr
prnote

prtree     // (tree, idepth, maxdepth)   Print a tree
prnltree

// Section Trans

prshapes
prshape

calcqlen

findrawshapes
addshapedata
replacestars // This is called after findrawshapes(tree) has completed
setshapes    // This is called after replacestars(tree) has completed

updateenvironments
restoreenvironments

setscaleparams
setmsecsenv
barscan
barscanitem
trscore

currpartname   // To hold the name of the current part.
currln         // Used by trerr

veclist        // List of vectors that must be freevec'd
pushipair
pushfpair
pushival
pushfval
pushval
pushmsecsval
pushshape
shapelookup
istied
checktlist
prties

transposition  // Number of semitones to transpose this part up by

currqbeat      // The qbeat position of the current item
maxqbeat       // The largest value of currqbeat. It will be the
               // length of the score in qbeats.

blkstartqbeat  // While in trscore, these are the qbeat positions of
blkendqbeat    // the start and end of the current block.

currbarqbeat   // The qbeat position of the start of the current bar

currbarno      // Current bar number, equal to -1 until bar one is found.
               // Typically used in error messages.

maxbarno       // The number of the last barline in the composition.
               // This will hold the last barline number of the conductor part.
currbeatno     // Current beat number used in the coposition

maxbeatno      // Total number of beats in the composition,
               // ie the sum of beats in each bar. Why is this needed?

currblock      // Points to the current Block node
currtuplet     // Points to the current Tuplet or zero

timesig_t      // The number of beats per bar
timesig_b      // The length number of a beat, eg 4 = a quarter note, etc

prevnum        // =TRUE if the previous item in a shape list
               // was a number.
prevqlen       // The qlen of the previous space in a shape list 

barno2absq
barno2absqv
absq2barno   // Find the number of the bar at or earlier than the
             // given q value.
tstabsq2barno 

absq2msecs
barno2msecs

q2blkq       // blkq = q2blkq(q, tuplet)
             // Apply all the Tuplet nodes in the given Tuplet chain
	     // linked though the parent field. The result specifies
	     // the local qbeat location within the current block.
	     // It is used when adding shape data to an environment
	     // in the current block It is also used by q2absq and
	     // q2msecs.
	     
q2absq       // absq = q2absq(q)
             // Return the absolute qbeat location corresponding
	     // to the given local qbeat location based on the
             // the current Tuplet and Block ignoring the effecct
	     // of tempo data in inner blocks. So the qshiftv
	     // field in blocks are ignored. This is used when
	     // checking that barlines are correctly placed.

q2msecs      // absq = q2msecs(q)
             // Return the time in msecs corresponding to the
	     // given local qbeat location based on the current
	     // Tuplet and Block and taking account of Tempo
	     // statements in inner blocks. So data in the
	     // qshiftv fields of blocks are used.

midilist    // The start of the midi list -> [link, msecs, <midi triple>]
midiliste   // Pointer to the last midi list item, or 0.

// Implementation of ties
plist       // Previous tlist just before current par or tuplet construct.
pqpos       // Abs qbeat terminating position of items in ptlist
tlist       // Outstanding ties in the current note thread
tqpos       // Abs qbeat terminating position of items in tlist
clist       // Outstanding ties in the other concurrent note threads
cqpos       // Abs qbeat terminating position of items in clist

// Player globals
midichannel // 0 .. 15 while running trpart?
micbuf

//tempodata     // A vector mapping from absolute qbeat values to floating
              // point msecs.
              // tempodata!i the time is msecs of absolute qbeat 32*i.
	      // This vector is used by absq2msecs.

barmsecs      // Mapping from bar number to midi msecs

solochannels  // Bit pattern with bit 1<<chan set if chan is a solo channel.

baseday       // Used by getrealmsecs
rmsecs0       // real msecs at startmsecs

soundv        // Buffer of cumulative data
soundp        // Position of next element of soundv to be updated
soundval      // Latest soundv element
soundmsecs    // real msecs of latest sound sample in soundv

genmidi
apmidi

soundco       // Coroutine to read sound samples into soundv
keyco         // The coroutine reading the keyboard
playmidico    // The coroutine to output midi data

barsxv_upb
barsxv_v
barsxv        // The bar self expanding vector

msecsv        // This gives the mapping from absolute qbeats to msecs
              // msecsv!0 is the upb of msecsv. This vector is created
	      // by mkmsecsv called from setshapes after calcqlen and
	      // findrawshapes have been called.

qbeatsperbar     // The current number of qbeats per bar
qbeatsperbeat    // = 4096/timesigb, ie 1024 for crotchet beats

notecov       // Vector of recognition coroutines for notes 0..127
notecoupb     // Note coroutines are from notecov!1 to notecov!notecoupb
notecop       // Subscript of next note coroutine to run

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

interpolateflt // y := interpolate( x, x1,x2,  y1,y2),
               // This performs integer interpolation.
interpolateint

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


// Global functions and variables

// Section Writemidi

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

// Section Shapefns

//FLT shapeval
shapeval


// Section Playmidi

genrecogfn
getrealmsecs
notecofn
setfreqtab
checktab
findtimes
addevent
clearevents   // Remove all previous events
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

conductorpart
conductorblock
conductorenvs
conductorflag  // =0 when translating psrtd snd solos
               // =1 at the start of processing the conductor part
	       // =2 when processing the body of the conductor block

scoreqlen          // The qlen of the score, returned by calcqlen(tree)

//msecsenv    // -> [upb, v, prevmsecsenv, sq, eq,-]
            // v has a msecs value for the start of every
            // 64 qbeat group enclosing sq to eq.

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
            // user, and Intermediate tempoadj
	    values are determined
            // using linear interpolation of these inverse values.
            // These modified tempo are both linear  ???????
            // functions of q. Their product is thus a quadratic
            // of the form r(q) = A + Bq + Cq^2. The time between
            // two qbeat values q1 and q2 is thus the integral of
            // r(q) between q1 and q2. The result is thus

            // Aq2 + Bq2^2/2 + Cq2^3/3 - Aq1 + Bq1^2/2 + Cq1^3/3

            // This formula is used to fill in the msecs values in
            // the msecs environment and it is also used when
            // calculating  intermediate times.

// Items in environments are of the form [absq,val]. Absolute locations
// are used since environments do not contain sufficient information to
// convert a local location to an absolute one. The shape valus val is
// always a floating point number. The elements of v range from zero
// to absq2-absq1.

delayenv    // =0 or -> [-, Volenv, ln, parent, upb, v, absq1, absq2] etc
delayadjenv
legatoenv
legatoadjenv
tempoenv
vibrateenv
vibrateadjenv
vibampenv
vibampadjenv
volenv
voladjenv
volmapenv

mkenvs
mkmsecsv
msv2qshiftv
prmsv

//FLT defaultshapeval
defaultshapeval

compactsxv
envbits

ZZZlastglobal
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
h1=0; h2; h3; h4; h5; h6; h7; h8; h9; h10

// Lex tokens and other symbols
s_altoclef=1          // [-, Altoclef, ln]
s_arranger            // [-, Arranger, ln, str]
s_bank                // [-, Bank, ln, byte, byte]
s_barlabel            // [-, Barlabel, ln, str]
s_barline             // [-, Barline, ln]
s_bassclef            // [-, Bassclef, ln]
s_block               // [-, Block,  ln, notes, qlen, parent,
                      //     qbeat, envs, shiftenv]
s_colon               // [-, Colon, ln, qlen]
s_composer            // [-, Composer, ln, str]
s_conductor           // [-, Conductor, ln, note-item]
s_control             // [-, Control, ln, byte, byte]
s_delay               // [-, Delay, ln, note_item, shapelist]
s_delayadj            // [-, Delayadj, ln, note_item, shapelist]
s_doublebar           // [-, Doublebar, ln]
s_instrument          // [-, Instrument, ln, str]
s_instrumentname      // [-, Instrumentname, ln, str]
s_instrumentshortname // [-, Instrumentshortname, ln, str]
s_int                 // [-, Int, ln, intval]
s_keysig              // [-, Keysig, ln, note, mode]
s_legato              // [-, Legato, ln, note_item, shapelist]
s_legatoadj           // [-, Legatoadj, ln, note_item, shapelist]
s_legon               // [-, Legon, ln]
s_legoff              // [-, Legoff, ln]

s_blocklist           // [-, Blocklist, ln, notelist,  qlen]
s_shape               // [-, Shape,     ln, shapelist, qlen]

s_tenorclef           // [-, Tenorclef, ln]

s_lcurly              // Token for {
s_lparen              // Token for (
s_lsquare             // Token for [
s_major               // Token for \major
s_minor               // Token for \minor
s_msecsmap            // [-, Msecsmap, v]
s_name                // [-, Name, ln, str]
s_neg                 // Token for -
s_nonvarvol           // [-, Nonvarvol, ln]
s_note                // [-, Note, ln, <letter:sharps:n>, qlen]
s_notetied            // [-, Notetied, ln, <letter:sharps:n>, qlen]
s_null                // [-, Null, ln]
s_fnum                // [-, Fnum, ln, value]
s_opus                // [-, Opus, ln, str]
s_par                 // [-, Par, ln, parlist, qlen]
s_notes               // [-, Notes, ln, notelist, qlen]
s_notelist            // [-, Notelist, ln, notelist, qlen]
s_parts               // [partlist, Parts, ln, notelist, qlen]
s_parlist             // [parlist, Parlist, ln, block, qlen]
s_part                // [-, Part, ln, notelist, qlen]
s_partlabel           // [-, partlabel, ln, str]
s_patch               // [-, patch, ln, byte]
s_pedon               // [-, Pedon, ln]
s_pedoff              // [-, Pedoff, ln]
s_pedoffon            // [-, Pedoffon, ln]
s_portaon             // [-, Portaon, ln]
s_portaoff            // [-, Portaoff, ln]
s_rcurly              // Token for }
s_repeatback          // [-, Repeatback, ln]
s_repeatbackforward   // [-, Repeatforwardback, ln]
s_repeatforward       // [-, Repeatforward, ln]
s_rest                // [-, Rest, ln, qlen]
s_rparen              // Token for )
s_rsquare             // Token for ]
s_score               // [-, Score, ln, str, conductor, parts]
s_space               // [-, Space, ln, qlen]
s_star                // [-, Star, ln]
s_startied            // [-, Startied, ln]
s_string
s_softon              // [-, Softon, ln]
s_softoff             // [-, Softoff, ln]
s_solo                // [-, Solo, ln, noteseq, channel]
s_tempo               // [-, Tempo, ln, notelist, qlen]
s_tenorclef           // [-, Tenorclef, ln]
s_timesig             // [-, Timesig, ln, timesiga, timesigb]
s_title               // [-, Title, ln, str]
s_transposition       // [-, Transposition, ln, semitones_up]
s_trebleclef          // [-, Trebleclef, ln]
s_tuplet              // [-, Tuplet, ln, noteseq, shapeseq]
s_varvol              // [-, Varvol, ln]
s_vibrate             // [-, Vibrate, ln, noteseq, shapeseq]
s_vibrateadj          // [-, Vibrateadj, ln, noteseq, shapeseq]
s_vibamp              // [-, Vibamp, ln, noteseq, shapeseq]
s_vibampadj           // [-, Vibampadj, ln, noteseq, shapeseq]
s_vol                 // [-, Vol, ln, noteseq, shapeseq]
s_voladj              // [-, Voladj, ln, noteseq, shapeseq]
s_volmap              // [-, Volmap, ln, shapeseq]

s_word

// Environment operators
s_tempoenv
s_volmapenv

s_delayenv            // [-, Delayenv, ln, parent, upb, v, absq1, absq2]
s_legatoenv           // etc
s_vibrateenv
s_vibampenv
s_volenv

s_delayadjenv
s_legatoadjenv
s_vibrateadjenv
s_vibampadjenv
s_voladjenv

s_shiftenv            // [-, Shiftenv, ln, upb, v, qlen]

s_envs

// Environment bit patterns
b_tempo      = #b0000_0000_0001
b_volmap     = #b0000_0000_0010

b_delay      = #b0000_0000_0100
b_legato     = #b0000_0000_1000
b_vibrate    = #b0000_0001_0000
b_vibamp     = #b0000_0010_0000
b_vol        = #b0000_0100_0000

b_delayadj   = #b0000_1000_0100
b_legatoadj  = #b0001_0000_0000
b_vibrateadj = #b0010_0000_0000
b_vibampadj  = #b0100_0000_0000
b_voladj     = #b1000_0000_0000

// MIDI opcodes
midi_note_off     = #x80
midi_note_on      = #x90
midi_keypressure  = #xA0
midi_control      = #xB0
midi_progchange   = #xC0
midi_chanpressure = #xD0
midi_pitchbend    = #xE0
midi_sysex        = #xF0

starval = #x12344321  // Used to represent a star in an environment vector
}
