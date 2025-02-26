//******************************************************************************
//*                                                                            *
//*                                                                            *
//*    ##    ##   ######    ######   ##    ##  ##    ##  ######    #######     *
//*    ###  ###  ########  ########  ##   ##   ##    ##  #######   ########    *
//*    ########  ##        ##    ##  ##  ##    ##    ##  ##    ##  ##    ##    *
//*    ## ## ##  #######    ######   ####      ########  ##    ##  ########    *
//*    ##    ##  ##    ##  ##    ##  ## ##     ##    ##  ##    ##  #######     *
//*    ##    ##  ##    ##  ##    ##  ##  ##    ##    ##  ##    ##  ##  ##      *
//*    ##    ##   ######   ########  ##   ##   ##    ##  #######   ##   ##     *
//*    ##    ##    ####     ######   ##    ##  ##    ##  ######    ##    ##    *
//*                                                                            *
//*                                                                            *
//******************************************************************************

$$68K

$<370
MANIFEST                                                               /* 370 */
$(                                                                     /* 370 */
    ug  =  fg                                                          /* 370 */
$)                                                                     /* 370 */
$>370



GLOBAL
$(
    abortassembly         :  ug + 0
    aborted               :  ug + 1
    absloc                :  ug + 2
    absmax                :  ug + 3
    absmin                :  ug + 4
    absolute              :  ug + 5
    absrp16               :  ug + 6
    absrp32               :  ug + 7
    absrvec16             :  ug + 8
    absrvec32             :  ug + 9
    absvec                :  ug + 10
    addressmask           :  ug + 11
    align                 :  ug + 12
    ascii.value           :  ug + 13
    asmlabel              :  ug + 14
    block1                :  ug + 15
    block2                :  ug + 16
    block3                :  ug + 17
    bracount              :  ug + 18
    bytesize              :  ug + 19
    bytesized             :  ug + 20
    bytesonline           :  ug + 21
    ch                    :  ug + 22
    changemode            :  ug + 23
    charpos               :  ug + 24
    charsperline          :  ug + 25
    checkexpression       :  ug + 26
    checkfor              :  ug + 27
    checklabel            :  ug + 28
    checkregister         :  ug + 29
    checktagsize          :  ug + 30
    clearbits             :  ug + 31
    clearbuffer           :  ug + 32
    codebuff              :  ug + 33
    codestream            :  ug + 34
    codevec               :  ug + 35
    codeword              :  ug + 36
    codewords             :  ug + 37
    commentline           :  ug + 38
    complain              :  ug + 39
    crossreference        :  ug + 40
    currentfile           :  ug + 41
    dataptr               :  ug + 42
    datavector            :  ug + 43
    datestring            :  ug + 44
    declare               :  ug + 45
    declsyswords          :  ug + 46
    defineblock           :  ug + 47
    defineconstants       :  ug + 48
    definestorage         :  ug + 49
    dest.ea               :  ug + 50
    directive             :  ug + 51
    dodir                 :  ug + 52
    doget                 :  ug + 53
    doline                :  ug + 54
    dumpsymbols           :  ug + 55
    eafield               :  ug + 56
    effective.address     :  ug + 57
    ended                 :  ug + 58
    entrysymbols          :  ug + 59
    error                 :  ug + 60
    error.found           :  ug + 61
    errormessages         :  ug + 62
    errors                :  ug + 63
    errorvec              :  ug + 64
    evalexp               :  ug + 65
    evaluate              :  ug + 66
    exp                   :  ug + 67
    expression            :  ug + 68
    exptype               :  ug + 69
    expvec                :  ug + 70
    expvecp               :  ug + 71
    externalref           :  ug + 72
    externalsymb          :  ug + 73
    extrnsymbols          :  ug + 74
    failed                :  ug + 75
    fatalerror            :  ug + 76
    fatalerrorp           :  ug + 77
    forwardref            :  ug + 78
    forwardreftype        :  ug + 79
    genea                 :  ug + 80
    generate              :  ug + 81
    getchunk              :  ug + 82
    getfrombuffer         :  ug + 83
    getlevel              :  ug + 84
    getstore              :  ug + 85
    heap2                 :  ug + 86
    heap3                 :  ug + 87
    in.movem              :  ug + 88
    initstore             :  ug + 89
    inmacro               :  ug + 90
    inputbuff             :  ug + 91
    instr.mask            :  ug + 92
    instr.masktype        :  ug + 93
    instr.size            :  ug + 94
    intelhexmodule        :  ug + 95
    labelset              :  ug + 96
    labelvec              :  ug + 97
    length                :  ug + 98
    linenumber            :  ug + 99
    linepos               :  ug + 100
    linesperpage          :  ug + 101
    listed                :  ug + 102
    listing               :  ug + 103
    listline              :  ug + 104
    liststream            :  ug + 105
    llenfixed             :  ug + 106
    location              :  ug + 107
    locmode               :  ug + 108
    lookup                :  ug + 109
    macrobase             :  ug + 110
    macrodepth            :  ug + 111
    macrochar             :  ug + 112
    macroend              :  ug + 113
    macroname             :  ug + 114
    makefile              :  ug + 115
    maxextlength          :  ug + 116
    maxloc                :  ug + 117
    message               :  ug + 118
    minloc                :  ug + 119
    motorolamodule        :  ug + 120
    name.hdrfile          :  ug + 121
    nargs                 :  ug + 122
    nextsymb              :  ug + 123
    nitems                :  ug + 124
    noobj                 :  ug + 125
    number                :  ug + 126
    objectmodule          :  ug + 127
    onpage                :  ug + 128
    op.ea                 :  ug + 129
    op1.ea                :  ug + 130
    op1.exp               :  ug + 131
    op1.exptype           :  ug + 132
    op1.externalref       :  ug + 133
    op1.externalsymb      :  ug + 134
    op1.registers         :  ug + 135
    outbuff               :  ug + 136
    pagenumber            :  ug + 137
    paging                :  ug + 138
    parmlisting           :  ug + 139
    pass1                 :  ug + 140
    pass2                 :  ug + 141
    plenfixed             :  ug + 142
    printbuffer           :  ug + 143
    printequates          :  ug + 144
    putinbuffer           :  ug + 145
    rch                   :  ug + 146
    readlabel             :  ug + 147
    readopcode            :  ug + 148
    readsymb              :  ug + 149
    readtag               :  ug + 150
    recoverlabel          :  ug + 151
    recoverlevel          :  ug + 152
    registers             :  ug + 153
    regnum                :  ug + 154
    release               :  ug + 155
    relloc                :  ug + 156
    relmax                :  ug + 157
    relmin                :  ug + 158
    relocatable           :  ug + 159
    relocate              :  ug + 160
    relocvec16            :  ug + 161
    relocvec32            :  ug + 162
    relp16                :  ug + 163
    relp32                :  ug + 164
    relrp16               :  ug + 165
    relrp32               :  ug + 166
    relrvec16             :  ug + 167
    relrvec32             :  ug + 168
    relvec                :  ug + 169
    resetflags            :  ug + 170
    setlabel              :  ug + 171
    setloc                :  ug + 172
    settitle              :  ug + 173
    sizefield             :  ug + 174
    sizevalue             :  ug + 175
    skiplayout            :  ug + 176
    skiplevel             :  ug + 177
    skipping              :  ug + 178
    skiprest              :  ug + 179
    source.ea             :  ug + 180
    sourcestream          :  ug + 181
    spacelines            :  ug + 182
    specialinstruction    :  ug + 183
    stackvalue            :  ug + 184
    storage.root          :  ug + 185
    storage.chunksize     :  ug + 186
    storage.high          :  ug + 187
    storage.low           :  ug + 188
    storage.totalwords    :  ug + 189
    storage.wordsused     :  ug + 190
    swapoperands          :  ug + 191
    symb                  :  ug + 192
    symbolchar            :  ug + 193
    symbtype              :  ug + 194
    sysout                :  ug + 195
    systemwords           :  ug + 196
    tagsize.given         :  ug + 197
    tagtable1             :  ug + 198
    tagtable2             :  ug + 199
    tagv                  :  ug + 200
    timestring            :  ug + 201
    titlevec              :  ug + 202
    totalwords            :  ug + 203
    triposmodule          :  ug + 204
    ts.default            :  ug + 205
    undefined             :  ug + 206
    uninitstore           :  ug + 207
    uppercase             :  ug + 208
    value                 :  ug + 209
    version               :  ug + 210
    verstream             :  ug + 211
    warning               :  ug + 212
    wordsized             :  ug + 213
    wrchsave              :  ug + 214
    writechar             :  ug + 215
    writehexvalue         :  ug + 216
    writenumber           :  ug + 217
    writestring           :  ug + 218
    write0                :  ug + 219
    xref                  :  ug + 220
    xreftable             :  ug + 221
$)



MANIFEST
$(
    //  Basic constants
    //  ===============

    storesize              =  500
    tagtablesize           =  500
    expsize                =  100
    titlecharsmax          =  60
    codesize               =  100
    errorsize              =  200
    maxllen                =  200
    minllen                =  60
    maxplen                =  200
    minplen                =  20
    maxmacrodepth          =  10
    maxgetlevel            =  10
    macroargs              =  36

    tabspace               =  8
    tabmask                =  #XFFFFFFF8


    //  Symbol types  (for symbol table)
    //  ================================

    s.abs16                =  0     // Tag with absolute value  (16 bit address)
    s.abs32                =  1     // Tag with absolute value  (32 bit address)
    s.dir                  =  2     // Assembler directive
    s.rel                  =  3     // Relocatable symbols
    s.instr                =  4     // Instruction mnemonic
    s.new                  =  5     // New entry in the symbol table
    s.Dr                   =  6     // Data register
    s.Ar                   =  7     // Address register
    s.SR                   =  8     // Status Register
    s.CCR                  =  9     // Condition Codes Register
    s.USP                  = 10     // User stack pointer
    s.SP                   = 11     // SP is equivalent to A7
    s.PC                   = 12     // Program Counter
    s.macro                = 13     // User defined macro name
    s.ext                  = 14     // Externally defined symbols
    s.reg                  = 15     // Register mask


    //  Other symbol types (do not appear in symbol table)
    //  ==================================================

    s.star                 = 16     // Location counter Symbol
    s.none                 = 17     // A NULL symbol (space or newline)
    s.bra                  = 18     // Left parenthesis
    s.number               = 19     // Number (123. or 'e', say)
    s.operator             = 20     // Operator
    s.slash                = 21
    s.hyphen               = 22     // Symbols for MOVEM


    //  Operators
    //  =========

    s.plus                 = 23
    s.minus                = 24
    s.over                 = 25
    s.times                = s.star
    s.logand               = 26
    s.logor                = 27
    s.lshift               = 28
    s.rshift               = 29

    s.Ar.indirect          = 30
    s.Ar.postincr          = 31
    s.Ar.predecr           = 32

    s.ket                  = 33
    s.literal              = 34
    s.colon                = 35
    s.monminus             = 36
    s.abs                  = 37
    s.comma                = 38
    s.opapply              = 39


    //  Instruction types
    //  =================

    i.zop                  = #B00000   // No operands for this instruction
    i.1op                  = #B01000   // One operand for this instruction
    i.2op                  = #B10000   // Two operands for this instruction


    //  Possible instruction sizes
    //  ==========================

    size.b                 = #B001
    size.w                 = #B010
    size.l                 = #B100

    size.bw                = size.b + size.w
    size.wl                = size.w + size.l
    size.bwl               = size.b + size.w + size.l


    //  Instruction combinations with the Number of operands, and
    //  the sizes of instructions allowed

    ins.zop                = ( i.zop               << 8 )  +  s.instr
    ins.1op                = ( i.1op               << 8 )  +  s.instr
    ins.2op                = ( i.2op               << 8 )  +  s.instr

    ins.1op.b              = (( i.1op + size.b   ) << 8 )  +  s.instr
    ins.1op.w              = (( i.1op + size.w   ) << 8 )  +  s.instr
    ins.1op.l              = (( i.1op + size.l   ) << 8 )  +  s.instr
    ins.1op.bw             = (( i.1op + size.bw  ) << 8 )  +  s.instr
    ins.1op.wl             = (( i.1op + size.wl  ) << 8 )  +  s.instr
    ins.1op.bwl            = (( i.1op + size.bwl ) << 8 )  +  s.instr

    ins.2op.b              = (( i.2op + size.b   ) << 8 )  +  s.instr
    ins.2op.w              = (( i.2op + size.w   ) << 8 )  +  s.instr
    ins.2op.l              = (( i.2op + size.l   ) << 8 )  +  s.instr
    ins.2op.bw             = (( i.2op + size.bw  ) << 8 )  +  s.instr
    ins.2op.wl             = (( i.2op + size.wl  ) << 8 )  +  s.instr
    ins.2op.bwl            = (( i.2op + size.bwl ) << 8 )  +  s.instr


    //  Directives
    //  ==========

    d.equ                  = 0
    d.equr                 = 1
    d.set                  = 2
    d.org                  = 3
    d.rorg                 = 4
    d.dc                   = 5
    d.ds                   = 6
    d.page                 = 7
    d.list                 = 8
    d.nolist               = 9
    d.nol                  = 9
    d.spc                  = 10
    d.nopage               = 11
    d.llen                 = 12
    d.plen                 = 13
    d.ttl                  = 14
    d.noobj                = 15
//  d.size                 = 16
    d.ifeq                 = 17
    d.ifne                 = 18
    d.endc                 = 19
    d.macro                = 20
    d.endm                 = 21
    d.mexit                = 22
    d.end                  = 23
    d.fail                 = 24
    d.get                  = 25
    d.cnop                 = 26
    d.extrn                = 27
    d.entry                = 28
    d.reg                  = 29
    d.dcb                  = 30
    d.iflt                 = 31
    d.ifle                 = 32
    d.ifgt                 = 33
    d.ifge                 = 34


    //  Other assorted constants
    //  ========================

    tagchars              = 30
    tagsize               = tagchars / bytesperword + 1

    bytesper68000word     = 4
    wordsper68000word     = bytesper68000word/bytesperword

    yes                   = TRUE
    no                    = FALSE


    //  Symbol table entry offsets
    //  ==========================

    st.link                = 0
    st.type                = 1
    st.flags               = 2
    st.template            = 3
    st.value.high          = 4
    st.value.low           = 5
    st.definition          = 6
    st.references          = 7
    st.name                = 8

    st.size                = 8

    //  To maintain compatibility with the previous version

    st.value               = st.value.low


    //  Symbol table type bits
    //  ======================

    stb.muldef             = #B00001
    stb.setnow             = #B00010
    stb.setever            = #B00100
    stb.equ                = #B01000
    stb.set                = #B10000

    st.type.mask           = #B1111


    //  Special cross-reference values
    //  ==============================

    cr.undefined           = -1
    cr.multiple            = -2
    cr.setsymbol           = -3


    //  Address Mode Type Bits
    //  ======================

    am.Dr                  = #B0000000000000001
    am.Ar                  = #B0000000000000010
    am.Ar.ind              = #B0000000000000100
    am.Ar.pi               = #B0000000000001000
    am.Ar.pd               = #B0000000000010000
    am.Ar.disp             = #B0000000000100000
    am.Ar.index            = #B0000000001000000
    am.abs16               = #B0000000010000000
    am.abs32               = #B0000000100000000
    am.PC.disp             = #B0000001000000000
    am.PC.index            = #B0000010000000000
    am.imm3                = #B0000100000000000
    am.imm16               = #B0001000000000000
    am.imm32               = #B0010000000000000
    am.imm                 = #B0011100000000000
    am.special             = #B0100000000000000

    am.all                 = #B0011111111111111

    am.data                = am.Dr          +
                             am.Ar.ind      +
                             am.Ar.pi       +
                             am.Ar.pd       +
                             am.Ar.disp     +
                             am.Ar.index    +
                             am.abs16       +
                             am.abs32       +
                             am.PC.disp     +
                             am.PC.index    +
                             am.imm

    am.mem                 = am.Ar.ind      +
                             am.Ar.pi       +
                             am.Ar.pd       +
                             am.Ar.disp     +
                             am.Ar.index    +
                             am.abs16       +
                             am.abs32       +
                             am.PC.disp     +
                             am.PC.index    +
                             am.imm

    am.alt                 = am.Dr          +
                             am.Ar          +
                             am.Ar.ind      +
                             am.Ar.pi       +
                             am.Ar.pd       +
                             am.Ar.disp     +
                             am.Ar.index    +
                             am.abs16       +
                             am.abs32

    am.mem.alt             = am.mem         &
                             am.alt

    am.data.alt            = am.data        &
                             am.alt

    am.contr               = am.Ar.ind      +
                             am.Ar.disp     +
                             am.Ar.index    +
                             am.abs16       +
                             am.abs32       +
                             am.PC.disp     +
                             am.PC.index

    am.contr.alt           = am.contr       &
                             am.alt


    //  Effective address types
    //  =======================

    ea.R.disp               = 1
    ea.R.index              = 2
    ea.exp                  = 3
    ea.literal              = 4


    //  Tag size specifiers
    //  ===================

    ts.byte                  = 1
    ts.word                  = 2
    ts.long                  = 3
    ts.short                 = 4

    ts.none                  = 0


    //  Masks for MC68000 and MC68010 processors
    //  ========================================
    
    mask.68000               =  #XFF000000
    mask.68010               =  #XF0000000


    //  Object module types available.
    //  ==============================

    o.none                 = 0
    o.tripos               = 1
    o.motorola             = 2
    o.intelhex             = 3


    //  Tripos object module identifiers.
    //  =================================

    t.hunk                 = 1000
    t.reloc                = 1001
    t.end                  = 1002
    t.abshunk              = 1003
    t.absrel               = 1004
    t.symbols              = 2001

    t.reloc32              = t.reloc
    t.reloc16              = 1010
    t.absrel32             = t.absrel
    t.absrel16             = 1011

    t.ext                  = 1005

    ext.defrel             = 1
    ext.defabs             = 2
    ext.ref                = 129


    //  Selectors in assorted data structures.
    //  ======================================

    e.link                 = 0
    e.symbol               = 1
    e.countr               = 2
    e.refsr                = 3
    e.counta               = 4
    e.refsa                = 5

    e.size                 = 6

    eb.line                =  0
    eb.code                =  1
    eb.file                =  2
    eb.size                =  3
    
    cb.dtype               =  0
    cb.dsize               =  1
    cb.dvalue              =  2
    cb.dext                =  3
    cb.dsymb               =  4
    cb.size                =  5
    
    m.buff                 =  0
    m.length               =  1
    m.link                 =  2

    p.ptr0                 =  0
    p.ptr1                 =  1
    p.ptr2                 =  2

    r.link                 =  0
    r.line                 =  1
    r.address              =  1
    r.file                 =  2

    //  Offsets in the TRIPOS argument vector
    //  =====================================

    a.from                 =  0
    a.to                   =  1
    a.ver                  =  2
    a.list                 =  3
    a.hdr                  =  4
    a.equ                  =  5
    a.symbols              =  6
    a.opt                  =  7
$)



MANIFEST
$(
    //  Return codes
    //  ============

$<370
    rc.catastrophic        =  32                                       /* 370 */
    rc.aborted             =  16                                       /* 370 */
    rc.errors              =  8                                        /* 370 */
$>370

$<CAP
    rc.catastrophic        =  32                                       /* CAP */
    rc.aborted             =  16                                       /* CAP */
    rc.errors              =  8                                        /* CAP */
$>CAP

$<68K
    rc.catastrophic        =  100                                      /* 68K */
    rc.aborted             =  20                                       /* 68K */
    rc.errors              =  10                                       /* 68K */
$>68K
$)


$<CAP
MANIFEST                                                               /* CAP */
$(                                                                     /* CAP */
    maxint                 = #X7FFFFFFF                                /* CAP */
    minint                 = #X80000000                                /* CAP */
$)                                                                     /* CAP */
$>CAP


