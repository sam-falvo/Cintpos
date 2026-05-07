/*
This is the header file for playmus.b

Wriiten by Martin Richards (c) February 2009
*/

GLOBAL {
// BGPM globals
bg_s:ug
bg_t
bg_h
bg_p
bg_f
bg_c
bg_e
bg_ch

bggetch; bgputch;  bgwrn
error

bgpmco
bgpmfn

sysin; sysout; sourcestream; tostream
sourcenamev; sourcefileno; sourcefileupb
getstreams; lineno

startbarno // Number of the first bar to play
endbarno   // Number of the last bar to play
startmsecs // Msecs of the start of the first bar to play
endmsecs   // Msecs of the end of the last bar to play
pptrace; optTokens; optTree; optPtree
optNtrace // Trace notes as they are compiled
optMtrace // Trace Midi commands as they are executed
tempoadj  // playing tempo adjustment as a percentage, 100=don't adjust

newvec; mk1; mk2; mk3; mk4; mk5; mk6; mk7
// Return blks to their free lists
unmk1; unmk2; unmk3; unmk4; unmk5; unmk6; unmk7
// The free lists for ewach size
mk1list; mk2list; mk3list; mk4list; mk5list; mk6list; mk7list;

blklist  // List of blocks of work space
blkb
blkp
blkt
blkitem
//treevec; treep; treet
base; upb; rec_p; rec_l; fin_p; fin_l

debugv              // To hold system debug flags 0 to 9
errcount; errmax
fatalerr; synerr; fatalsynerr
trerr
strv    // Short term sting buffer

rch; ch; chbuf; chcount; formtree; tree
prnote; prtree; opstr; prlineno
token; lexval; numval; noteletter; prevnoteletter
octave;  prevoctave; reloctave; notenumber; noteqbeats
notesharps; notelength; prevnotelength; dotcount
bgexp; bgbexp; argp; argt
wrc; wrs; chpos; charv; wordnode; stringval
rdtag
dsw
lookupword
rdstrch
nametable
noteqbeats
rdscore
rdscores

// Globals for the translation stage
trscores
trtree
walktree

performbarscan

performshapescan

performtemposcan
performvolscan
performlegatoscan
performdelayscan

performnotescan

pushval
timesiga
timesigb
transposition  // Number of semitones to transpose this part up by
conductor_bartab
tempov
volumev
qbeat2msec
barno2msec
interpolate
midilist       // The start of the midi list -> [link, msecs, <midi triple>]
midiliste      // Pointer to the last midi list item, or 0.
editnoteoffs
mergesort
prmidilist
note2str       // s := note2str(noteno, str)

// Player globals
playmidi
mididata
tempodata
midifd
selectbank
selectpatch

writemidi
}

MANIFEST {
nametablesize = 541
blkupb = 4000

// BGPM markers
s_eof     =  -2
s_eom     =  -3

// BGPM builtin macros
s_def     =  -4
s_set     =  -5
s_get     =  -6
s_eval    =  -7
s_lquote  =  -8
s_rquote  =  -9
s_comment = -10
s_rep     = -19

// BGPM special characters
c_call    = '$'
c_apply   = ';'
c_sep     = '!'
c_comment = '%' 
c_lquote  = '<'
c_rquote  = '>'
c_arg     = '#'

// General selectors
h1=0; h2; h3; h4; h5; h6; h7

// Selector for most mus nodes
n_next=0; n_op; n_ln; n_a1; n_a2; n_a3; n_a4

// Lex tokens and other symbols
s_altoclef=1
s_arranger
s_bank
s_barlabel
s_barline
s_bassclef
s_colon
s_composer
s_conductor
s_control
s_delay
s_doublebar
s_instrument
s_instrumentname
s_instrumentshortname
s_interval
s_keysig
s_legato
s_legatoadj
s_legon
s_legoff
s_lparen
s_lsquare
s_major
s_minor
s_name
s_neg
s_note
s_notetied
s_num
s_numtied
s_opus
s_par
s_part
s_partlabel
s_patch
s_pedon
s_pedoff
s_pedoffon
s_portaon
s_portaoff
s_repeatback
s_repeatbackforward
s_repeatforward
s_rest
s_rparen
s_rsquare
s_score
s_seq
s_space
s_string
s_softon
s_softoff
s_tempo
s_tempoadj
s_tenorclef
s_timesig
s_title
s_transpose
s_transposition
s_trebleclef
s_tune
s_tuneadj
s_tuplet
s_vol
s_voladj
s_volmap
s_null
s_vibamp
s_vibrate

// act values
w_barscan =-3
w_length  =-4
w_notes   =-5

// Score fields
sc_next=0
sc_op
sc_ln
sc_name
sc_parts
sc_env

// Env fields
e_name=0              // eg "piano right hand"
e_instrumentname      // eg "french horn"
e_instrumentshortname // eg "Hn1"
e_midichannel         // 0=conductor, otherwise 1-16
e_maxbarno            // The number of conductor bars
e_qbeats              // The number of conductor qbeats
e_bartab              // The conductor's bar table
e_tempo               // Only used by conductor
e_qtimev              // Conductor's mapping vector from semiquavers
                      // to msecs. (A semiquaver has 256 qbeats)
e_qtimevupb           // The UPB of qtimev
e_vol
e_voladj
e_delay
e_legato
e_notes
e_upb

// MIDI opcodes
midi_note_off      = #x80
midi_note_on       = #x90
midi_keypressure   = #xA0
midi_controlchange = #xB0
midi_progchange    = #xC0
midi_chanpressure  = #xD0
midi_pitchbend     = #xE0
midi_sysex         = #xF0
}
