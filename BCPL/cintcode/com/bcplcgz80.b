/*

This is a variant of the Z80 codegenerator written by I.D.Wilson dated
19/02/85 for the Systems Research Group at Cambridge.  This version
has been modified to work with the modern BCPL Cintcode frontend
(bcplsyn.b and bcpltrn.b). This allows modern BCPL to be compiled into
Intel Hex format for the Z80. Certain BCPL features such as floating
point are not be available.

The program z80bcpl.b is just like bcpl.b but combines bcplsyn.b and
bcpltrn.b with bcplcgz80.b rather than bcplcgcin.b.  The frontend has
been modified to include the options MAP and LIST needed by the Z80
and possibly other codegenerators.

This modified version is being constructed by
Martin Richards (c) September 2022

*/




/*****************************************************************************\
*                           Systems Research Group                            *
*******************************************************************************


                  #####    ######   ##    ##  ######    #######  
                 #######  ########  ##    ##  #######   ######## 
                ##        ##        ##    ##  ##    ##  ##    ## 
                ##        ##  ####  ########  ##    ##  ######## 
                ##        ##    ##  ##    ##  ##    ##  #######  
                ##        ##    ##  ##    ##  ##    ##  ##  ##   
                 #######  ########  ##    ##  #######   ##   ##  
                  #####    ######   ##    ##  ######    ##    ## 


*******************************************************************************
*   I. D. Wilson           Last Modified   -   IDW   -   18/02/85             *
\*****************************************************************************/

SECTION "bcplcgz80"

GET "libhdr"
GET "bcplfecg"

GLOBAL
$(
    addname:cgg
    addslaveinfo
    arg1
    arg2
    bcplalign
    block2
    block3
    callmonfunction
    cg_apply
    cg_branch
    cg_byteop
    cg_comparejump
    cg_constant
    cg_data
    cg_dyadic
    cg_endproc
    cg_entry
    cg_finish
    cg_global
    cg_goto
    cg_initstore
    cg_jump
    cg_loadt
    cg_monadic
    cg_neg
    cg_plusminus
    cg_putbyte
    cg_res
    cg_return
    cg_rstack
    cg_save
    cg_setlabel
    cg_stack
    cg_stackop
    cg_stind
    cg_stindop
    cg_store
    cg_storeitem
    cg_switchon
    cg_uninitstore
    cg_unslavelocal
    cg_unslavestorage
    cgerror
    cglabel
    cgwarning
    cherish
    chseq
    chseqr
    code_i_1
    code_i_2
    code_il_2
    code_il_3
    code_im_1
    code_ir_3
    code_l_2
    code_li_3
    code_lr_4
    code_m_1
    code_n_1
    code_n_2
    code_r_2
    code_ri_3
    code_rl_3
    code_rl_4
    code_rn_2
    code_rn_3
    code_rr_1
    code_s_1
    code_ss_1
    code_ss_2
    compare_cc
    condition
    currentloc
    datae
    datap
    //debugging       replaced by debug in bcplfecg.h (= 0, 1 or 2)
    destackitem
    discardslave
    evaltype
    evaluate
    findfreer
    freeblock2
    freeblock3
    freecherished
    freelist2
    freelist3
    freenode
    fromfile
    fromstream
    getstore
    highbyte
    incode
    labeladdr
    labelrefs
    lengthlab
    // listfile     renamed listfilename and set by the frontend
    // listing      Use liststream to determine whether LISTING is enabled
    liststream
    loadindexvalue
    lookinslave
    lookuplabel
    lowbyte
    mapfile
    mapping
    mapstream
    maxgn
    maxlabel
    maxln
    memchunks
    monargument
    monfunction
    monloadfunction
    monstorefunction
    movetoanybutr
    movetoanyr
    movetor
    namelist
    newlabel
    notcond
    notcherished
    parseocode
    printdebuginfo
    procdepth
    programbase
    rchnode
    rchseq
    rdg
    rdl
    //rdn      This is defined in bcplfe.{h and b}
    rdop
    reverse
    rinfo
    rstcount
    rstfuncs
    rstinsts
    scanforregister
    setlabel
    setlabelrefs
    ssp
    stackcherished
    stackitem
    stackp
    startcoding
    stopcoding
    storage_chunksize
    storage_high
    storage_low
    storage_root
    storecherished
    storeregister
    symmetric
    sysout
    systemindex
    totalstorage
    totalused
    uncherish
    unsetslave
    update
    writel
$)



MANIFEST
$(

//    s_true               =  4          
//    s_false              =  5          
//    s_rv                 =  8          
//    s_fnap               =  10         
//    s_query              =  11         
//    s_neg                =  12        
//    s_abs                =  13        
//    s_mul                =  14         
//    s_div                =  15         
//    s_mod                =  16         
//    s_add                =  17         
//    s_sub                =  18         
//    s_eq                 =  19         
//    s_ne                 =  20        
//    s_ls                 =  21         
//    s_gr                 =  22         
//    s_le                 =  23         
//    s_ge                 =  24       
//    s_not                =  29       
//    s_lshift             =  30       
//    s_rshift             =  31
//    s_logand             =  32
//    s_logor              =  33         
//    s_eqv                =  34
//    s_xor                =  35         
//    s_needs              =  39
//    s_section            =  40
//    s_rtap               =  42
//    s_goto               =  43
//    s_finish             =  59
//    s_switchon           =  60
//    s_global             =  68

//    s_lf                 =  97
//    s_lp                 =  98
//    s_lg                 =  99
//    s_ln                 =  100
//    s_lstr               =  101
//    s_ll                 =  102
//    s_llp                =  103
//    s_llg                =  104
//    s_lll                =  105

//    s_sp                 =  106
//    s_sg                 =  107
//    s_sl                 =  108
//    s_stind              =  109
//    s_jump               =  110         
//    s_jt                 =  111
//    s_jf                 =  112

//    s_lab                =  114
//    s_stack              =  115
//    s_store              =  116
//    s_rstack             =  117
//    s_entry              =  118
//    s_save               =  119
//    s_fnrn               =  120
//    s_rtrn               =  121
//    s_res                =  122
//    s_datalab            =  123
//    s_itemn              =  124
//    s_endproc            =  125
//    s_endfor             =  113  
//    s_getbyte            =  126
//    s_putbyte            =  127

    s_itemb              =  200    // Not in ocode
//    s_debug              =  201    // Not in ocode
//    s_blab               =  202    // Not in ocode         $)
    s_iteml              =  204
    s_end                =  205    // Not in ocode

$)


MANIFEST
$(
    error_toolarge        =  1000       
    error_nospace         =  1001       

    savespacesize         =  2          
    bytesperz80word       =  2          

    NIL                   =  0

    bytesperchunk         =  1024
    wordsperchunk         =  bytesperchunk/bytesperword

    maxchunks             =  20

    t_local               =  1000       
    t_global              =  1001       
    t_label               =  1002       
    t_lv_local            =  1003       
    t_lv_global           =  1004       
    t_lv_label            =  1005       
    t_number              =  1006       
    t_stack               =  1007       
    t_register            =  1008       
    t_cherished           =  1009       
    t_fnlab               =  1010  // Added 4 Oct 2020
    
    r_hl                  =  0          
    r_de                  =  1          
    r_bc                  =  2          

    r_a                   =  3
    r_b                   =  4
    r_c                   =  5
    r_d                   =  6
    r_e                   =  7
    r_h                   =  8
    r_l                   =  9
    r_ihl                 =  10

    r_none                =  -1         

    b_link                =  0          

    a_link                =  0          
    a_ssp                 =  1          
    a_node                =  2          

    n_type                =  0          
    n_arg1                =  1          
    n_arg2                =  2          

    d_link                =  0          
    d_type                =  1          
    d_value               =  2          

    l_link                =  0          
    l_type                =  1          
    l_value               =  2          

    r_link                =  0          
    r_addr                =  1

    nl_link               =  0
    nl_name               =  1
    nl_addr               =  2
    nl_type               =  3
    nl_value              =  4
    nl_size               =  4

    cond_always           =  2000       
    cond_eq               =  2001       
    cond_ne               =  2002       
    cond_ls               =  2003       
    cond_gr               =  2004       
    cond_le               =  2005       
    cond_ge               =  2006       
    cond_carry            =  2007
    cond_nocarry          =  2008

    cc_none               =  3000       
    cc_z                  =  3001       
    cc_nz                 =  3002       
    cc_p                  =  3003       
    cc_m                  =  3004       
    cc_c                  =  3005
    cc_nc                 =  3006
$)



MANIFEST
$(
    dupl0                 =  #B000000000 
    dupl1                 =  #B100000000 

    i_inc                 =  #B00000011 
    i_dec                 =  #B00001011 
    i_and                 =  #B10100000 
    i_or                  =  #B10110000 
    i_xor                 =  #B10101000 
    i_sbchl               =  #B01000010 
    i_addhl               =  #B00001001 
    i_ldrr                =  #B01000000 
    i_cpl                 =  #B00101111 
    i_ldrn                =  #B00000110 
    i_ldrnn               =  #B00000001 + dupl0
    i_ldrll               =  #B00000001 + dupl1
    i_push                =  #B11000101 
    i_pop                 =  #B11000001 
    i_ret                 =  #B11001001 
    i_jpihl               =  #B11101001 
    i_exx                 =  #B11011001 
    i_jr                  =  #B00011000 + dupl0
    i_jrz                 =  #B00101000 
    i_jrnz                =  #B00100000 
    i_jrc                 =  #B00111000 + dupl0
    i_jrnc                =  #B00110000
    i_jp                  =  #B11000011 
    i_jpz                 =  #B11001010 
    i_jpnz                =  #B11000010 
    i_jpnc                =  #B11010010
    i_jpc                 =  #B11011010
    i_jpp                 =  #B11110010 
    i_jpm                 =  #B11111010 
    i_ldri                =  #B01000110 
    i_ldir                =  #B01110000 
    i_ldhll               =  #B00101010 
    i_ldrl                =  #B01001011 
    i_srl                 =  #B00111000 + dupl1 
    i_rr                  =  #B00011000 + dupl1
    i_ldlhl               =  #B00100010 
    i_ldlr                =  #B01000011 
    i_cpir                =  #B10110001
    i_nop                 =  #B00000000 
    i_rst00               =  #B11000111 
    i_rst08               =  #B11001111 
    i_rst10               =  #B11010111 
    i_rst18               =  #B11011111 
    i_rst20               =  #B11100111 
    i_rst28               =  #B11101111 
    i_rst30               =  #B11110111
    i_rst38               =  #B11111111

    esc_cb                =  #B11001011 
    esc_dd                =  #B11011101 
    esc_ed                =  #B11101101 
    esc_fd                =  #B11111101

    m_jpentrysize         =  3

    m_default             =  -1

    m_apply               =  0  * m_jpentrysize
    m_fixup               =  1  * m_jpentrysize
    m_loadix              =  2  * m_jpentrysize
    m_loadiy              =  3  * m_jpentrysize
    m_storeix             =  4  * m_jpentrysize
    m_storeiy             =  5  * m_jpentrysize
    m_setlink0            =  6  * m_jpentrysize
    m_setlink1            =  7  * m_jpentrysize
    m_setlink2            =  8  * m_jpentrysize
    m_setlink3            =  9  * m_jpentrysize        
    m_return              =  10 * m_jpentrysize         
    m_finish              =  11 * m_jpentrysize         
    m_loadlvix            =  12 * m_jpentrysize        
    m_loadlviy            =  13 * m_jpentrysize        
    m_multiply            =  14 * m_jpentrysize        
    m_divide              =  15 * m_jpentrysize        
    m_rem                 =  16 * m_jpentrysize        
    m_lshift              =  17 * m_jpentrysize        
    m_rshift              =  18 * m_jpentrysize        
    m_eq                  =  19 * m_jpentrysize        
    m_ne                  =  20 * m_jpentrysize        
    m_ls                  =  21 * m_jpentrysize        
    m_gr                  =  22 * m_jpentrysize        
    m_le                  =  23 * m_jpentrysize        
    m_ge                  =  24 * m_jpentrysize        
    m_rdivide             =  25 * m_jpentrysize        
    m_rrem                =  26 * m_jpentrysize        
    m_rlshift             =  27 * m_jpentrysize        
    m_rrshift             =  28 * m_jpentrysize        
    m_abs                 =  29 * m_jpentrysize        
    m_linsearch           =  30 * m_jpentrysize
$)


/*****************************************************************************\
*                           Systems Research Group                            *
*******************************************************************************


                       #####    ######                ##
                      #######  ########              ###
                     ##        ##                   ####
                     ##        ##  ####   #######     ##
                     ##        ##    ##               ##
                     ##        ##    ##               ##
                      #######  ########               ##
                       #####    ######                ##


*******************************************************************************
*   I. D. Wilson           Last Modified   -   IDW   -   21/02/85             *
\*****************************************************************************/



///SECTION "CG-1"


LET codegenerate(v, vupb)  BE
$( //  Main routine of the BCPL code generator.

   // v, vupb is the available workspace
    sysout  :=  output()

    // listfilename and mapfilename are set by the frontend.

    // The Ocode is read from a buffer in memory using rdn, rdgn, etc
    // The intel hex output is sent to tostream
    
    liststream  :=  0
    IF listfilename DO
    $( liststream  :=  findoutput( listfilename )
       IF liststream = 0  DO
         cgwarning( "Cannot open listing file *"%S*"", listfilename )
    $)

    //  Now open the map stream, of there is one.
    mapstream  :=  0
    IF mapfilename DO
    $( mapstream  :=  findoutput( mapfilename )
       UNLESS mapstream DO
         cgwarning( "Cannot open mapping file *"%S*"", mapfilename )
    $)

    //  Say hello to the outside world.

    writes( "Z80CG  Version 1.15*N*N" )

    //  We can now select the streams, and away we go!

    selectoutput( tostream )

    freelist2     :=  NIL
    freelist3     :=  NIL

    namelist      :=  NIL

    totalstorage  :=  0
    totalused     :=  0

    cg_initstore( 2500 )

    maxlabel      :=  1000
    labeladdr     :=  getstore( maxlabel )
    labelrefs     :=  getstore( maxlabel )

    //  Initialise the buffer which will hold pointers to the compiled code.
    memchunks     :=  getstore( maxchunks )

    FOR  i = 0  TO  maxchunks-1  DO  memchunks!i  :=  NIL

    //  The following tables define the restart functions which are available.
    //  The vector "rstfuncs" is set to the functions which it is desirable to
    //  call directly.  The vector "rstinsts" is a list of the instructions
    //  which correspond to the functions, and "rstcount" is the number of
    //  free restart instructions for the current operating system.

    rstfuncs      :=  TABLE  0,        m_apply,   m_fixup,   m_loadix,
                             m_loadiy, m_storeix, m_storeiy, m_loadlvix

    rstinsts      :=  TABLE  i_rst38,  0,         0,         0,
                             0,        0,         0,         0

    rstcount      :=  0

    cg_initdata()

    cg_initstack( savespacesize )

    rinfo         :=  getstore( r_bc )
    rchnode       :=  getstore( r_bc )
    rchseq        :=  getstore( r_bc )

    currentloc    :=  0
    programbase   :=  0

    readocode()
    moduleend()

    IF mapstream DO writef( "*NProgram size:  %N bytes*N", currentloc )

    IF  liststream UNLESS liststream=sysprint DO { endstream(liststream); liststream := 0 }
    IF  mapstream  UNLESS mapstream =sysprint DO { endstream(mapstream);  mapstream  := 0 }

    cg_uninitstore()

    selectoutput( sysout )

    writef( "%N bytes of code generated.*N*
            *%N out of %N words of workspace used.*N",
            currentloc, totalused, totalstorage )
$)



AND readocode()  BE
$(
//  Main loop of the code generator.  Read in OCODE statements one by one,
//  and parse them.  This main loop SHOULD be repeated...

    LET op  =  initsection( rdop() )
    LET o   =  output()

    IF  op = s_end | op=0  THEN  RETURN

    $(  //  Now, enter the main loop, generating code for the sections.
        //  The OCODE statement "GLOBAL" terminates a section.

        parseocode( op )

        IF  op = s_global  THEN  BREAK

        op  :=  rdop()
    $)
    REPEAT

    uninitsection()

    selectoutput( sysout )
    newline()
    selectoutput( o )
$)
REPEAT



AND initsection( op )  =  op = s_end  ->  op,  VALOF
$(
//  Initialise all the variables for a section.

    FOR  r = r_hl  TO  r_bc  DO
    $(
        rinfo!r    :=  NIL
        rchnode!r  :=  NIL
        rchseq!r   :=  0
    $)

    FOR  l = 0  TO  maxlabel  DO
    $(
        labeladdr!l  :=  NIL
        labelrefs!l  :=  NIL
    $)

    cglabel     :=  maxlabel
    procdepth   :=  0
    maxgn       :=  0
    maxln       :=  0
    chseq       :=  0
    stackp      :=  0
    lengthlab   :=  maxlabel

    TEST   op = s_section
        THEN  op  :=  cg_section()
        ELSE  moduledef( "NoName" )

    WHILE  op = s_needs  DO  op  :=  cg_needs()

    incode  :=  TRUE

    IF  liststream  THEN  writel( "    REL" )

    code_n_1( 'B' )
    code_n_1( 'C' )
    code_n_1( 'P' )
    code_n_1( 'L' )

    code_l_2( lengthlab, 0 )

    incode  :=  FALSE


    RESULTIS  op
$)



AND cg_section()  =  VALOF
$(
    LET length  =  rdn()
    LET o       =  output()
    LET buffer  =  VEC 256/bytesperword

    FOR  i = 1  TO  length   DO  buffer % i   :=  rdn()

    buffer % 0   :=  length

    IF  liststream  THEN  writel( "%S  TITLE  *"SECTION %S*"", buffer, buffer )

    IF  mapstream  THEN
    $(
        selectoutput( mapstream )
        writef( "Section *"%S*"    ", buffer )
        selectoutput( o )
    $)

    moduledef( buffer )
//sawritef("returned from moduledef*n")
    selectoutput( sysout )
    writef( "SECTION *"%S*"*N", buffer )
    selectoutput( o )

    RESULTIS  rdop()
$)



AND cg_needs()  =  VALOF
$(
    LET length  =  rdn()
    LET o       =  output()
    LET buffer  =  VEC 256/bytesperword

    FOR  i = 1  TO  length  DO  buffer % i  :=  rdch()

    buffer % 0  :=  length

    IF  liststream  THEN  writel( "    NEEDS *"%S*"", buffer )

    selectoutput( sysout )
    writef( "NEEDS *"%S*"*N", buffer )
    selectoutput( o )

    RESULTIS  rdop()
$)



AND uninitsection()  BE
$(
//  Generate the closing sequence.  This means writing out the code generated,
//  and dealing with all the relocation information.

    LET length  =  currentloc - programbase
    LET lenh    =  (length >> 8)  &  #XFF
    LET lenl    =  (length)       &  #XFF

    update( programbase+4, lenl )
    update( programbase+5, lenh )

    IF  liststream  THEN
    $(
        writel( "L%N:  EQU  %N", lengthlab, length )

        writel( "    END" )
    $)

    IF  mapstream  THEN  printmap( length )

    moduledata( length )

    modulereloc()

    programbase  :=  currentloc
$)



AND moduledef( name )  BE
$(
//  Generate the module definition name.

    LET csum    =  #X05 + #X01
    LET length  =  name % 0

    wrch( '$' )

    FOR  i = 1  TO  6  DO
    $(
        LET ch  =  i > length  ->  '*S',  name % i

        csum  :=  csum  +  ch

        wrch( ch )
    $)

    writef( "0501%X2*N", -csum )
$)



AND moduledata( length )  BE
$(
//  Generate the module data definitions.

    FOR  address = 0  TO  length-1  BY  bytesperchunk  DO
    $(
        LET chunk   =  address/bytesperchunk
        LET buffer  =  memchunks!chunk
        LET base    =  programbase + address
        LET size    =  length - address

        IF  size > bytesperchunk  THEN  size  :=  bytesperchunk

        FOR  offset  =  0  TO  size-1  BY  32  DO
        $(
            LET bytes  =  size - offset
            LET addr   =  base + offset
            LET addrh  =  (addr >> 8)  &  #XFF
            LET addrl  =  (addr)       &  #XFF
            LET csum   =  addrh + addrl

            IF  bytes > 32  THEN  bytes  :=  32

            csum  :=  csum + bytes

            writef( ":%X2%X400", bytes, addr )

            FOR  i = 0  TO  bytes-1  DO
            $(
                LET byte  =  buffer % (offset + i)

                writehex( byte, 2 )

                csum  :=  csum + byte
            $)

            writef( "%X2*N", -csum )
        $)
    $)
$)



AND modulereloc()  BE
$(
//  Generate the relocation information from the "label reference" lists.

    LET list     =  NIL
    LET ptr      =  @list
    LET entries  =  0

    //  First, change the series of lists into one long one, keeping note of
    //  the number of entries.

    FOR  i = 0  TO  lengthlab-1  DO
    $(
        LET llist  =  labelrefs!i

        UNLESS  llist = NIL  DO
        $(
            ptr!r_link  :=  llist

            UNTIL  ptr!r_link = NIL  DO
            $(
                ptr      :=  ptr!r_link
                entries  :=  entries + 1
            $)
        $)
    $)

    FOR  entry = 0  TO  entries-1  BY  16  DO
    $(
        LET size  =  entries - entry
        LET csum  =  #X04

        IF  size > 16  THEN  size  :=  16

        csum  :=  csum + size

        writef( "$%X2000004", size )

        FOR  i = 0  TO  size-1  DO
        $(
            LET link   =  list!r_link
            LET addr   =  list!r_addr
            LET addrh  =  (addr >> 8)  &  #XFF
            LET addrl  =  (addr)       &  #XFF

            writehex( addr, 4 )

            csum  :=  csum + addrh + addrl

            freeblock2( list )

            list  :=  link
        $)

        writef( "%X2*N", -csum )
    $)

    freeblock2( labelrefs!lengthlab )
$)



AND moduleend()  BE
$(
//  Generate a module end record.

    LET csum  =  #XFF + #XFF + #X01

    writef( ":00FFFF01%X2*N", -csum )
$)



AND printmap( length )  BE
$(
    LET o       =  output()
    LET lcount  =  0

    selectoutput( mapstream )

    writef( "%X4 - %X4  (%N bytes)*N*N*N", programbase, currentloc, length )

    writes( "Procedures:*N*N" )

    UNTIL  namelist = NIL  DO
    $(
        LET link   =  namelist!nl_link
        LET name   =  namelist!nl_name
        LET addr   =  namelist!nl_addr
        LET type   =  namelist!nl_type
        LET value  =  namelist!nl_value

        writef( "%X4  -  ", addr )
        writestring( name, 20 )

        TEST  type = t_global
            THEN  writef( "global %N", value )
            ELSE  writes( "local" )

        newline()

        namelist  :=  link
    $)

    writef( "*NHighest global referenced:  %N*N*N*N", maxgn )

    writes( "Labels:*N" )

    FOR  i = 0  TO  maxlabel-1  DO
    $(
        LET addr  =  labeladdr!i

        UNLESS  addr = NIL  DO
        $(
            IF  lcount = 0  THEN  newline()

            wrch( 'L' )
            writenumber( i, 4 )

            writef( " %X4   ", addr )

            lcount  :=  (lcount + 1)  REM  6
        $)
    $)

    writes( "*N*N*N" )

    writef( "Label allocation:*N*N*
            *    OCODE:           %I4  -  %I4*N*
            *    Code Generator:  %I4  -  %I4*N*N*N", 1,       maxln,
                                                      cglabel, maxlabel-1 )

    selectoutput( o )
$)



AND writestring( string, width )  BE
$(
//  Write out a string in a fixed width, padding with spaces.

    writes( string )

    FOR  i = string % 0  TO  width-1  DO  wrch( '*S' )
$)



AND writenumber( number, width )  BE
$(
//  Write out a number in a fixed width, padding with spaces.

    writen( number )

    FOR  i = numberofdigits( number )  TO  width-1  DO  wrch( '*S' )
$)



AND numberofdigits( n )  =  n < 10  ->  1, numberofdigits( n/10 ) + 1



AND parseocode( op )  BE
$(
//  Parse a single OCODE statement.

    IF  testflags( #B0001 )  THEN  cgerror( "BREAK" )
    IF  testflags( #B1000 )  THEN
      cgwarning( "%N bytes of code compiled", currentloc  )

    IF  debug & liststream  THEN
      writel( ";  OP:%S,  SSP:%N,  STACKP:%N", opname(op), ssp, stackp )

    SWITCHON  op  INTO
    $(	
        DEFAULT:
                 cgerror("Unknown Ocode op %s(%n) is not allowed",
		         opname(op), op)
                 ENDCASE

        // Ocode ops with three operands
        CASE s_selst:
                   rdn()

        // Ocode ops with two operands
        CASE s_selld:
                   rdn()

	// Ocode ops with one argument
	           rdn()

	// Ocode ops with no arguments
        CASE s_float: CASE s_fix: CASE s_fneg: CASE s_fabs:
        CASE s_fmul: CASE s_fdiv:CASE s_fmod:
        CASE s_fadd:CASE s_fsub:
        CASE s_feq: CASE s_fne:
        CASE s_fls:CASE s_fgr:CASE s_fle:CASE s_fge:
                 cgerror("BCPL feature using Ocode op %s(%n) is not allowed",
		         opname(op), op)
                 ENDCASE

        CASE s_needs:
               { LET n = rdn()  // Ignore NEEDS directives.
                 FOR i = 1 TO n DO rdn()
                 ENDCASE
               }

        CASE s_lp       :  cg_loadt( t_local, rdn() )       ;  ENDCASE
        CASE s_lg       :  cg_loadt( t_global, rdg() )      ;  ENDCASE
        CASE s_ln       :  cg_loadt( t_number, rdn() )      ;  ENDCASE
        CASE s_ll       :  cg_loadt( t_label, rdl() )       ;  ENDCASE

        CASE s_llp      :  cg_loadt( t_lv_local,  rdn() )   ;  ENDCASE
        CASE s_llg      :  cg_loadt( t_lv_global, rdg() )   ;  ENDCASE
        CASE s_lll      :  cg_loadt( t_lv_label,  rdl() )   ;  ENDCASE
        CASE s_lf       :  cg_loadt( t_fnlab,     rdl() )   ;  ENDCASE

        CASE s_true     :  cg_loadt( t_number, TRUE )       ;  ENDCASE
        CASE s_false    :  cg_loadt( t_number, FALSE )      ;  ENDCASE

        CASE s_query    :  cg_loadt( t_local, ssp )         ;  ENDCASE

        CASE s_lstr     :  cg_loadstring( rdn() )           ;  ENDCASE

        CASE s_sp       :  cg_storet( t_local, rdn() )      ;  ENDCASE
        CASE s_sg       :  cg_storet( t_global, rdg() )     ;  ENDCASE
        CASE s_sl       :  cg_storet( t_label, rdl() )      ;  ENDCASE

        CASE s_stind    :  cg_stind()                       ;  ENDCASE
        CASE s_putbyte  :  cg_putbyte()                     ;  ENDCASE

        CASE s_jump     :  cg_jump( rdl() )                 ;  ENDCASE

        CASE s_jf       :
        CASE s_jt       :  cg_condjump( op, rdl() )         ;  ENDCASE

        CASE s_endfor   :  cg_endfor( rdl() )               ;  ENDCASE

        CASE s_goto     :  cg_goto()                        ;  ENDCASE

        //CASE s_blab     :  // The op in cg_setlabel is not used
        CASE s_lab      :  cg_setlabel( op, rdl() )         ;  ENDCASE

        CASE s_save     :  cg_save( rdn() )                 ;  ENDCASE
        CASE s_stack    :  cg_stack( rdn() )                ;  ENDCASE
        CASE s_store    :  cg_store( 0, ssp )               ;  ENDCASE

        CASE s_entry    :  cg_entry( )                      ;  ENDCASE

        CASE s_fnap     :
        CASE s_rtap     :  cg_apply( op, rdn() )            ;  ENDCASE

        CASE s_fnrn     :
        CASE s_rtrn     :  cg_return( op )                  ;  ENDCASE

        CASE s_endproc  :  cg_endproc( )  // ENDPROC no longer has an argument
                           cg_flushdata()                   ;  ENDCASE

        CASE s_res      :  cg_res( rdl() )                  ;  ENDCASE
        CASE s_rstack   :  cg_rstack( rdn() )               ;  ENDCASE
        CASE s_finish   :  cg_finish()                      ;  ENDCASE

        CASE s_switchon :
	                 { LET n = rdn()
			   LET v = VEC 2*500
			   IF n > 500 DO cgerror( "Bad swritchon n=%n", n )
			   cg_switchon(v, n*2 )
			   ENDCASE
			 }

        CASE s_global   :  cg_global( rdn() )               ;  RETURN // was ENDCASE

        CASE s_datalab  : 
        CASE s_iteml    :  cg_data( op, rdl() )             ;  ENDCASE

        CASE s_itemn    :  cg_data( op, rdn() )             ;  ENDCASE


        CASE s_getbyte  :  CASE s_add      :  CASE s_sub      :
        CASE s_logand   :  CASE s_logor    :  CASE s_eqv      :
        CASE s_xor      :  CASE s_lshift   :  CASE s_rshift   :
        CASE s_mul      :  CASE s_div      :  CASE s_mod      :
        CASE s_eq       :  CASE s_ne       :  CASE s_ls       :
        CASE s_gr       :  CASE s_le       :  CASE s_ge       :

                           cg_dyadicop( op )
                           ENDCASE


        CASE s_rv       :
	CASE s_neg      :
	CASE s_not      :
        CASE s_abs      :

                           cg_monadicop( op )
                           ENDCASE


        CASE s_end      :  RETURN


        //CASE s_debug    :  printdebuginfo()
        //                   ENDCASE

    $)
$)



AND cg_initstack()  BE
$(
//  Initialise the slaved stack at start of day.  The size of the stack is
//  the "save space" reserved by the compiler.

    ssp   :=  savespacesize - 2

    arg2  :=  NIL
    arg1  :=  NIL

    cg_loadt( t_local, ssp )
    cg_loadt( t_local, ssp )
$)



AND cg_stack( newssp )  BE
$(
//  Set the size of the slave stack to be the value given.

    LET down  =  newssp < ssp
    LET diff  =  0

    //  First, bring the stack down until we are less than or equal to the
    //  correct position.

    UNTIL  ssp <= newssp  DO  cg_unloadt()

    //  After doing that, we should then (if necessary) bring the stack
    //  up to its required amount.

    diff  :=  newssp - ssp

    TEST  diff < 2  THEN
          FOR  i = 1  TO  diff  DO  cg_loadt( t_local, ssp )

    ELSE
    $(
        //  Initialise a new portion of stack frame.

        ssp  :=  newssp - 2

        cg_loadt( t_local, ssp )
        cg_loadt( t_local, ssp )
    $)

    //  Given that the stack frame size has changed, we had better modify our
    //  register slave (if any) to reflect this change.

    IF  down  THEN  cg_unslavelocal()
$)



AND cg_loadt( type, value )  BE
$(
//  Load an item with the type and value given onto the stack.  Remembered
//  with the item itself is its original place on the stack.  This is so that
//  we can save it safely, even which the stack position has changed.

    LET item  =  block3( arg1, ssp, block3( type, value, ssp ) )

    arg2  :=  arg1
    arg1  :=  item
    ssp   :=  ssp + 1
$)



AND cg_unloadt()  BE
$(
//  Unload the top item from the stack, and throw it away.

    freeblock3( arg1 )

    arg1  :=  arg2
    arg2  :=  arg2!a_link

    ssp   :=  arg1!a_ssp + 1
$)



AND cg_loadstring( length )  BE
$(
//  Load a pointer to a string onto the stack.  We do not compile code for
//  this yet, but just buffer the string up.

    LET label   =  newlabel()

    cg_data( s_datalab, label )
    cg_data( s_itemb, length )

    FOR  i = 1  TO  length  DO  cg_data( s_itemb, rdn() )

    cg_loadt( t_lv_label, label )
$)



AND cg_dyadicop( op )  BE
$(
//  Code generate for a dyadic operator.  This means taking the top two
//  items on the stack, and operating on them, and storing the result back
//  on the stack.

    LET node  =  block3( op, arg2!a_node, arg1!a_node )

    cg_stack( ssp-1 )

    arg1!a_node  :=  node
$)



AND cg_monadicop( op )  BE
$(
//  Code generate for a monadic operator.  Take the top item on the stack,
//  and operate on it, storing the result back on the stack.

    LET node  =  block2( op, arg1!a_node )

    arg1!a_node  :=  node
$)



AND cg_store( ssp_f, ssp_t )  BE
$(
//  Store those items on the stack between SSP locations "ssp_f" and "ssp_t".

    LET arg  =  arg1

    IF  debug & liststream  THEN  writel( ";  CGStore %N:%N", ssp_f, ssp_t )

    UNTIL  arg = NIL  DO
    $(
        //  Make sure that this item is within the range given, and if it is,
        //  store it on the stack.

        LET s  =  arg!a_ssp

        IF  ssp_f <= s < ssp_t  THEN
        $(
            //  Look to see if the item here is in fact what should be here,
            //  and if not, evaluate it, and store it.

            LET node  =  arg!a_node
            LET type  =  node!n_type
            LET a1    =  node!n_arg1

            UNLESS  type = t_local  &  a1 = s  DO
            $(
                cg_storeitem( node, t_local, s )

                arg!a_node  :=  block3( t_local, s, s )
            $)
        $)

        arg  :=  arg!a_link
    $)
$)



AND cg_storet( type, value )  BE
$(
//  Take the top item on the stack, and store it in the location given by
//  "type" and "value".

    LET node  =  arg1!a_node

    cg_storeitem( node, type, value )

    cg_stack( ssp-1 )
$)



AND cg_endfor( label )  BE
$(
//  Code generate for the end of a FOR loop.  This is logically equivalent
//  to:
//
//      LE
//      JT  L<label>

    LET node1  =  arg1!a_node
    LET node2  =  arg2!a_node

    cg_stack( ssp-2 )

    cg_comparejump( label, s_le, node2, node1 )

    freenode( node1 )
    freenode( node2 )
$)



AND cg_jump( label )  BE
$(
//  Code generate for an unconditional jump.  This means that we should
//  make sure that all items are stored.

    cg_store( 0, ssp )

    cg_branch( cond_always, label )
$)



AND cg_condjump( jumptype, label )  BE
$(
//  Generate code for a conditional jump.  The jump type is one of JT or
//  JF, depending on the condition.  We can usually optimise this as the
//  previous operation is a comparison.

    LET node  =  arg1!a_node
    LET type  =  node!n_type

    cg_stack( ssp-1 )

    cg_store( 0, ssp )

    TEST  conditional( type )  THEN
    $(
        //  This is something of the form  "IF x <cond> y THEN", and the
        //  condition can be optimised.

        IF  jumptype = s_jf  THEN  type  :=  notcond( type )

        cg_comparejump( label, type, node!n_arg1, node!n_arg2 )
    $)
    ELSE
    $(
        //  Here, we are testing a single value, and jumping on either TRUE
        //  or FALSE.  We can optimise this, since we know that FALSE is
        //  always zero.

        type  :=  jumptype = s_jf  ->  s_eq, s_ne

        cg_comparejump( label, type, node, NIL )
    $)

    freenode( node )
$)



AND symmetric( op )    =  op = s_add   |  op = s_logand  |  op = s_logor  |
                          op = s_eqv   |  op = s_xor     |  op = s_mul    |

                          conditional( op )



AND conditional( op )  =  op = s_eq    |  op = s_ne      |  op = s_ls     |
                          op = s_gr    |  op = s_le      |  op = s_ge



AND reverse( op )  =      op = s_add     ->  s_add,
                          op = s_logand  ->  s_logand,
                          op = s_logor   ->  s_logor,
                          op = s_eqv     ->  s_eqv,
                          op = s_xor     ->  s_xor,
                          op = s_mul     ->  s_mul,
                          op = s_eq      ->  s_eq,
                          op = s_ne      ->  s_ne,
                          op = s_ls      ->  s_gr,
                          op = s_gr      ->  s_ls,
                          op = s_le      ->  s_ge,
                          op = s_ge      ->  s_le,

                                             cgerror( "reverse( %N )", op )



AND notcond( op )  =      op = s_eq      ->  s_ne,
                          op = s_ne      ->  s_eq,
                          op = s_ls      ->  s_ge,
                          op = s_gr      ->  s_le,
                          op = s_le      ->  s_gr,
                          op = s_ge      ->  s_ls,

                                             cgerror( "notcond( %N )", op )



AND condition( op )  =    op = s_eq  ->      cond_eq,
                          op = s_ne  ->      cond_ne,
                          op = s_ls  ->      cond_ls,
                          op = s_gr  ->      cond_gr,
                          op = s_le  ->      cond_le,
                          op = s_ge  ->      cond_ge,

                                             cgerror( "condition( %N )", op )



AND cg_data( type, value )  BE
$(
//  Queue up an item of data onto the data queue.  The queue is flushed at
//  convenient moments, for example between the code of procedures.

    LET data  =  block3( NIL, type, value )

    datae!d_link  :=  data
    datae         :=  data
$)



AND cg_flushdata()  BE
$(
//  Flush the static data which we have carefully buffered up.

    LET data  =  datap

    UNTIL  data = NIL  DO
    $(
        LET link   =  data!d_link
        LET type   =  data!d_type
        LET value  =  data!d_value

        cg_constant( type, value )

        freeblock3( data )

        data  :=  link
    $)

    //  When we drop out of that loop, we should set the data pointers back
    //  to their initial values.

    cg_initdata()
$)



AND cg_initdata()  BE
$(
//  Initialise the chain associated with the data buffer.

    datap  :=  NIL
    datae  :=  @datap
$)



AND block2( a, b )  =  VALOF
$(
//  Return a store element of 2 words.  First, look in the chain of free
//  blocks, and if that proves useless, then allocate a new piece of
//  storage.

    LET block  =  0

    TEST  freelist2 = NIL  THEN  block  :=  getstore( 1 )

    ELSE
    $(
        block      :=  freelist2
        freelist2  :=  block!b_link
    $)

    block!0  :=  a
    block!1  :=  b

    RESULTIS  block
$)



AND block3( a, b, c )  =  VALOF
$(
//  Return a store element of 3 words.  First, look in the chain of free
//  blocks, and if that proves useless, then allocate a new piece of
//  storage.

    LET block  =  0

    TEST  freelist3 = NIL  THEN  block  :=  getstore( 2 )

    ELSE
    $(
        block      :=  freelist3
        freelist3  :=  block!b_link
    $)

    block!0  :=  a
    block!1  :=  b
    block!2  :=  c

    RESULTIS  block
$)



AND freeblock2( block )  BE
$(
//  Release a block of size 2 so that it can be re-used.

    block!b_link  :=  freelist2
    freelist2     :=  block
$)



AND freeblock3( block )  BE
$(
//  Release a block of size 3 so that it can be reused.

    block!b_link  :=  freelist3
    freelist3     :=  block
$)



AND cg_initstore( chunksize )  BE
$(
//  Initialise the storage package, defining the size of chunks which will
//  be grabbed from the standard storage manager.

    storage_chunksize  :=  chunksize
    storage_root       :=  0
    storage_high       :=  0
    storage_low        :=  0
$)



AND getstore( upb )  =  VALOF
$(
//  Analagous to "getvec"  -  allocate a vector whose word upperbound
//  is "upb" from the heap.  If there is not enough room in the current
//  chunk, then allocate a new chunk.

    LET size   =  upb + 1
    LET chunk  =  0

    IF  size > storage_chunksize  THEN  abort( error_toolarge )

    IF  (storage_high - storage_low)  <  size  THEN
    $(
        //  Not enough room left in the current chunk, so allocate a
        //  new chunk, and try again.

        LET chunksize  =  storage_chunksize + 1
        LET newchunk   =  getvec( chunksize )

        IF  newchunk = 0  THEN  abort( error_nospace )

        newchunk!0    :=  storage_root
        storage_root  :=  newchunk
        storage_low   :=  newchunk + 1
        storage_high  :=  storage_low + chunksize

        totalstorage  :=  totalstorage + chunksize + 1
    $)

    chunk        :=  storage_low
    storage_low  :=  storage_low + size

    totalused    :=  totalused + size

    RESULTIS  chunk
$)



AND cg_uninitstore()  BE
$(
//  Free all the storage in use by the storage package.  The base of the
//  storage chain is pointed to by "storage_root".

    UNTIL  storage_root = 0  DO
    $(
        LET next  =  storage_root!0

        freevec( storage_root )

        storage_root  :=  next
    $)
$)



AND freenode( node )  BE
$(
//  Free the storage associated with the node.  The size of the storage
//  used by the node depends on its type.

    LET type  =  node!n_type

    SWITCHON  type  INTO
    $(
        CASE t_local      :  CASE t_global     :
        CASE t_label      :  CASE t_number     :
        CASE t_lv_local   :  CASE t_lv_global  :
        CASE t_lv_label   :  CASE t_register   :
        CASE t_stack      :  CASE t_cherished  :
        CASE t_fnlab      :

                             freeblock3( node )
                             ENDCASE


        CASE s_add        :  CASE s_sub        :
        CASE s_logand     :  CASE s_logor      :
        CASE s_eqv        :  CASE s_xor        :
        CASE s_lshift     :  CASE s_rshift     :
        CASE s_mul        :  CASE s_div        :
        CASE s_mod        :  CASE s_eq         :
        CASE s_ne         :  CASE s_ls         :
        CASE s_gr         :  CASE s_le         :
        CASE s_ge         :  CASE s_getbyte    :

                             freenode( node!n_arg1 )
                             freenode( node!n_arg2 )
                             freeblock3( node )
                             ENDCASE


        CASE s_neg        :  CASE s_not        :
        CASE s_abs        :  CASE s_rv         :

                             freenode( node!n_arg1 )
                             freeblock2( node )
                             ENDCASE


        DEFAULT           :  cgerror( "freenode( %N )", type )
    $)
$)


/*****************************************************************************\
*                           Systems Research Group                            *
*******************************************************************************


                       #####    ######               ####
                      #######  ########             ######
                     ##        ##                  ##    ##
                     ##        ##  ####   #######        ##
                     ##        ##    ##                 ##
                     ##        ##    ##               ##
                      #######  ########             #######
                       #####    ######             ########


*******************************************************************************
*   I. D. Wilson           Last Modified   -   IDW   -   21/02/85             *
\*****************************************************************************/



///SECTION "CG-2"


LET cg_storeitem( node, type, value )  BE
$(
//  Store the expression represented by "node" into the location represented
//  by "type" and "value".

    LET v  =  evaluate( node )
    LET t  =  evaltype

    //  Having evaluated the expression, we had better store it in the
    //  location given.

    LET r  =  movetoanyr( t, v, TRUE )

    //  Now, store the register in the location given, and add this value
    //  to the information in the slave.

    IF  debug & liststream  THEN  writel( ";  Store [%N,%N] -> [%N,%N]", t, v, type, value )

    storeregister( r, type, value )

    //  Now remove any slave info relevant to the item being stored, and add
    //  the only correct one.

    scanslaveentries( type, value )

    addslaveinfo( r, type, value )

    //  We have now finished with the tree representing the expression, and
    //  it can now be freed.

    freenode( node )
$)



AND scanslaveentries( type, value )  BE
$(
//  Scan the slave entries, removing anything with a reference to "type" and
//  "value".

    FOR  r = r_hl  TO  r_bc  DO
    $(
        LET list  =  scanslaveentry( rinfo!r, type, value )

        unsetslave( r )

        rinfo!r  :=  list
    $)
$)



AND scanslaveentry( list, type, value )  =  list = NIL  ->  NIL,  VALOF
$(
//  Make a copy of the list "list", removing all references to "type" and
//  "value".

    LET l  =  list!l_link
    LET t  =  list!l_type
    LET v  =  list!l_value

    TEST  t = type  &  v = value
        THEN  RESULTIS  scanslaveentry( l, type, value )
        ELSE  RESULTIS  block3( scanslaveentry( l, type, value ), t, v )
$)



AND evaluate( node )  =  VALOF
$(
//  Evaluate the tree represented by "node".  The result is the register in
//  which the result is stored.

    LET type  =  node!n_type

    SWITCHON  type  INTO
    $(
        CASE t_local      :  CASE t_global     :
        CASE t_label      :  CASE t_number     :
        CASE t_lv_local   :  CASE t_lv_global  :
        CASE t_lv_label   :  CASE t_register   :
        CASE t_stack      :  CASE t_cherished  :
        CASE t_fnlab      :

                             evaltype  :=  type

                             RESULTIS  node!n_arg1



        CASE s_add        :  CASE s_sub        :
        CASE s_logand     :  CASE s_logor      :
        CASE s_eqv        :  CASE s_xor        :
        CASE s_lshift     :  CASE s_rshift     :
        CASE s_mul        :  CASE s_div        :
        CASE s_mod        :  CASE s_eq         :
        CASE s_ne         :  CASE s_ls         :
        CASE s_gr         :  CASE s_le         :
        CASE s_ge         :  CASE s_getbyte    :

                             RESULTIS  cg_dyadic( type, node!n_arg1, node!n_arg2 )


        CASE s_neg        :  CASE s_not        :
        CASE s_abs        :  CASE s_rv         :

                             RESULTIS  cg_monadic( type, node!n_arg1 )


        DEFAULT           :  cgerror( "evaluate( %N )", type )
    $)
$)



AND cg_stind()  BE    pseudo.dyadic( s_stind, FALSE )



AND cg_putbyte()  BE  pseudo.dyadic( s_putbyte, TRUE )



AND pseudo.dyadic( op, swapargs )  BE
$(
//  Code generate for one of the pseudo dyadic operators (stind or putbyte).
//  This is a fudge, but it is by far the most efficient way of doing the
//  job.

    LET node1  =  arg1!a_node
    LET node2  =  arg2!a_node
    LET reg1   =  0
    LET reg2   =  0

    cg_stack( ssp-2 )

    //  It is possible that, in one of the nodes, there is a reference to
    //  the result register HL.  We must make sure that this register is
    //  saved on the BCPL stack if we are to avoid having problems with
    //  stack twisting!
    
    reg1  :=  scanforregister( node1 )
    reg2  :=  scanforregister( node2 )
    
    IF  reg1  &  reg2  THEN

        //  Oh dear - there was a register reference in both nodes, and
        //  so we should moan.

        cgerror( "pseudo.dyadic( 2regs )" )

    //  Having done that, we can safely swap the arguments if necessary, and
    //  get on with compiling the code for the operator.

    IF  swapargs  THEN
    $(
        LET node  =  node1

        node1  :=  node2
        node2  :=  node
    $)
    
    IF  op = s_putbyte  THEN
    
        //  If we are dealing with the "putbyte" operator, then there is a special
        //  case which we must consider.  This arises when the item being stored
        //  is the result of a function call (i.e. in HL).  The calculating of the
        //  address corrupts HL, and so we should store it now.
    
        IF  scanforregister( arg1!a_node )  &  (reg1 | reg2)  THEN
    
            //  Oh dear.  We have found register references in more than one
            //  place, and so we should complain about it.
        
            cgerror( "pseudo.dyadic( 2regs )" )

    cg_dyadic( op, node1, node2 )

    freenode( node1 )
    freenode( node2 )
$)



AND scanforregister( node )  =  VALOF
$(
//  Scan the data structure associated with the node given, and look for
//  a reference to the result register HL.  When found, we store this
//  register at its original location in the BCPL stack.

    LET type   =  node!n_type
    LET value  =  node!n_arg1
    LET ossp   =  0
    LET copy   =  0
    LET reg1   =  0
    LET reg2   =  0

    SWITCHON  type  INTO
    $(
        CASE t_register   :  //  This is a register node, and so we should
                             //  check that it refers to the right register,
                             //  and then save the register.

                             UNLESS  value = r_hl  DO

                                 //  This is the wrong register - oops!

                                 cgerror( "scanforregister( %N )", value )

                             //  This is the correct register, so call the
                             //  routine to store the register.

                             ossp         :=  node!n_arg2
                             copy         :=  block3( type, value, ossp )

                             cg_storeitem( copy, t_local, ossp )

                             node!n_type  :=  t_local
                             node!n_arg1  :=  ossp
                             node!n_arg2  :=  ossp

                             RESULTIS  TRUE


        CASE t_local      :  CASE t_global     :
        CASE t_label      :  CASE t_number     :
        CASE t_lv_local   :  CASE t_lv_global  :
        CASE t_lv_label   :  CASE t_stack      :
        CASE t_cherished  :
        CASE t_fnlab      :

                             RESULTIS  FALSE


        CASE s_add        :  CASE s_sub        :
        CASE s_logand     :  CASE s_logor      :
        CASE s_eqv        :  CASE s_xor        :
        CASE s_lshift     :  CASE s_rshift     :
        CASE s_mul        :  CASE s_div        :
        CASE s_mod        :  CASE s_eq         :
        CASE s_ne         :  CASE s_ls         :
        CASE s_gr         :  CASE s_le         :
        CASE s_ge         :  CASE s_getbyte    :

                             reg1  :=  scanforregister( node!n_arg1 )
                             reg2  :=  scanforregister( node!n_arg2 )

                             IF  reg1  &  reg2  THEN

                                 //  Oh dear - registers in both halves of the
                                 //  expression, and so we should complain.

                                 cgerror( "scanforregister( 2regs )" )

                             RESULTIS  reg1  |  reg2


        CASE s_neg        :  CASE s_not        :
        CASE s_abs        :  CASE s_rv         :

                             RESULTIS  scanforregister( node!n_arg1 )


        DEFAULT           :  cgerror( "scanforregister( %N )", type )
    $)
$)



AND cg_dyadic( op, node1, node2 )  =  VALOF
$(
//  Evaluate a dyadic operator's operands, and generate code to evaluate the
//  expression.

    LET v1  =  0
    LET t1  =  0
    LET v2  =  0
    LET t2  =  0

    //  Before evaluating node 1, we should cherish node 2, if the value is
    //  a register.

    IF  node2!n_type = t_register  THEN
    $(
        //  The second argument is already in a register, so cherish it to
        //  stop it being clobbered by the evaluation of the first argument.

        LET r      =  node2!n_arg1
        LET rnode  =  block2( t_register, r )

        node2!n_type  :=  t_cherished
        node2!n_arg1  :=  rnode

        cherish( r, rnode )
    $)

    //  Now, evaluate the first argument, and see what sort of a revelation
    //  this brings.

    v1  :=  evaluate( node1 )
    t1  :=  evaltype

    //  Look at the data type of the first operand, and if it is a register
    //  then we must take special precautions to stop the register being
    //  destroyed.

    IF  t1 = t_register  THEN
    $(
        LET node  =  block2( t1, v1 )

        cherish( v1, node )

        t1  :=  t_cherished
        v1  :=  node
    $)

    //  Now evaluate the second argument.  We know that the first one will be
    //  safe, since it will be cherished if it is a register.

    v2  :=  evaluate( node2 )
    t2  :=  evaltype

    //  Having evaluated both operands, we can remove their cherished status

    IF  t1 = t_cherished  THEN
    $(
        LET node  =  v1

        t1  :=  node!n_type
        v1  :=  node!n_arg1

        IF  t1 = t_register  THEN  uncherish( v1 )

        freeblock2( node )
    $)

    IF  t2 = t_cherished  THEN
    $(
        LET node  =  v2

        t2  :=  node!n_type
        v2  :=  node!n_arg1

        IF  t2 = t_register  THEN  uncherish( v2 )

        freeblock2( node )
    $)

    //  First, look for the simple case where the operands are both numbers,
    //  and if they are, we can do some constant folding.

    IF  t1 = t_number  &  t2 = t_number  THEN
    $(
        evaltype  :=  t_number

        SWITCHON  op  INTO
        $(
            CASE s_add        :  RESULTIS  v1  +   v2
            CASE s_sub        :  RESULTIS  v1  -   v2
            CASE s_logand     :  RESULTIS  v1  &   v2
            CASE s_logor      :  RESULTIS  v1  |   v2
            CASE s_eqv        :  RESULTIS  v1 EQV  v2
            CASE s_xor        :  RESULTIS  v1 NEQV v2
            CASE s_lshift     :  RESULTIS  v1  <<  v2
            CASE s_rshift     :  RESULTIS  v1  >>  v2
            CASE s_mul        :  RESULTIS  v1  *   v2
            CASE s_div        :  RESULTIS  v1  /   v2
            CASE s_mod        :  RESULTIS  v1 REM  v2
            CASE s_eq         :  RESULTIS  v1  =   v2
            CASE s_ne         :  RESULTIS  v1  \=  v2
            CASE s_ls         :  RESULTIS  v1  <   v2
            CASE s_gr         :  RESULTIS  v1  >   v2
            CASE s_le         :  RESULTIS  v1  <=  v2
            CASE s_ge         :  RESULTIS  v1  >=  v2

            CASE s_getbyte    :
            CASE s_putbyte    :
            CASE s_stind      :  ENDCASE

            DEFAULT           :  cgerror( "cg_dyadic( %N )", op )
        $)
    $)

    //  Now, look at the case where only one of the operands is a constant,
    //  but it is one which can be optimised.  First, swap the operands if
    //  necessary.

    IF  symmetric( op )  &  t1 = t_number  THEN
    $(
        LET ot1  =  t1
        LET ov1  =  v1

        t1  :=  t2
        v1  :=  v2

        t2  :=  ot1
        v2  :=  ov1

        op  :=  reverse( op )
    $)

    //  Now look to see if the second operand is a number, and if it is,
    //  look to see if any optimisations can be done on it.

    IF  t2 = t_number  THEN
    $(
        //  If the number is zero, then many arithmetic operators become
        //  trivial.  This can be simulated here.

        IF  v2 = 0  THEN
            SWITCHON  op  INTO
            $(
                CASE s_add        :
                CASE s_sub        :
                CASE s_logor      :
                CASE s_xor        :
                CASE s_lshift     :
                CASE s_rshift     :  evaltype  :=  t1

                                     RESULTIS  v1


                CASE s_div        :
                CASE s_mod        :
                CASE s_logand     :
                CASE s_eqv        :
                CASE s_mul        :  evaltype  :=  t_number

                                     RESULTIS  0


                DEFAULT           :  ENDCASE
            $)

        //  If the second value is 1, then the multiplication/division
        //  operators become trivial.  Simulate this here.

        IF  v2 = 1  THEN
            SWITCHON  op  INTO
            $(
                CASE s_div        :
                CASE s_mul        :  evaltype  :=  t1

                                     RESULTIS  v1


                CASE s_mod        :  evaltype  :=  t_number

                                     RESULTIS  0


                DEFAULT           :  ENDCASE
            $)
    $)

    //  Otherwise, look to see if the operator is symmetric, and if so
    //  see if the second operand is already in a register.  If it is, then
    //  we can swap the operands.

    IF  symmetric( op )  &  t2 = t_register  THEN
    $(
        LET ot1  =  t1
        LET ov1  =  v1

        t1  :=  t2
        v1  :=  v2

        t2  :=  ot1
        v2  :=  ov1

        op     :=  reverse( op )
    $)

    //  At this point, we have decided that we do in fact need to compile
    //  some code to do the operation.  The code we compile depends very
    //  much on the operator concerned.

    SWITCHON  op  INTO
    $(
        CASE s_add        :
        CASE s_sub        :  RESULTIS  cg_plusminus( op, t1, v1, t2, v2 )


        CASE s_logand     :
        CASE s_logor      :
        CASE s_eqv        :
        CASE s_xor        :  RESULTIS  cg_logicalop( op, t1, v1, t2, v2 )


        CASE s_lshift     :  RESULTIS  cg_lshift( t1, v1, t2, v2 )


        CASE s_rshift     :  RESULTIS  cg_rshift( t1, v1, t2, v2 )


        CASE s_mul        :
        CASE s_div        :
        CASE s_mod        :
        CASE s_eq         :
        CASE s_ne         :
        CASE s_ls         :
        CASE s_gr         :
        CASE s_le         :
        CASE s_ge         :  RESULTIS  cg_stackop( op, t1, v1, t2, v2 )


        CASE s_getbyte    :
        CASE s_putbyte    :  RESULTIS  cg_byteop( op, t1, v1, t2, v2 )


        CASE s_stind      :  RESULTIS  cg_stindop( t1, v1, t2, v2 )


        DEFAULT           :  cgerror( "cg_dyadic( %N )", op )
    $)
$)



AND cg_plusminus( op, t1, v1, t2, v2 )  =  VALOF
$(
//  Generate code for a PLUS or MINUS operator.  At this point, we know that
//  the first operand may be in a register, and the second operand may be
//  a number.  If the second operand is a number, and is negative, then
//  we can alter the sign, and the corresponding operator.

    LET r  =  0

    IF  op = s_sub  &  t2 = t_number  THEN
    $(
        //  This is a negative number.  Make the number positive, and
        //  reverse the operator.

        op  :=  s_add
        v2  :=  -v2
    $)

    //  If the second operand is a small number, then we can use the
    //  register increment and decrement instructions.

    IF  t2 = t_number  &  (-5 < v2 < +5)  THEN
    $(
        LET negv2  =  v2 < 0
        LET absv2  =  ABS v2
        LET inst   =  op = s_add ->  (negv2 -> i_dec, i_inc),
                   /* op = s_sub */  (negv2 -> i_inc, i_dec)

        r  :=  movetoanyr( t1, v1, FALSE )

        FOR  i = 1  TO  absv2  DO  code_ss_1( inst, r )

        UNLESS  absv2 = 0  DO  unsetslave( r )

        evaltype  :=  t_register

        RESULTIS  r
    $)

    //  Otherwise, there is nothing for it but to compile an ADD or an SBC
    //  instruction.  The target register must be HL, but the second operand
    //  can be in any register.

    IF  op = s_add  &  (t1 = t_register  &  v1 \= r_hl)  THEN
    $(
        //  This is an addition, and the first operand is already in a
        //  register, but unfortunately the wrong one!  No matter, since
        //  we can swap the operands.

        LET ot1  =  t1
        LET ov1  =  v1

        t1  :=  t2
        v1  :=  v2
        t2  :=  ot1
        v2  :=  ov1
    $)

    //  We can now load the operands into their respective registers, and
    //  compile the instruction.  We must be very careful in which order
    //  we do the following moves (in case both arguments are on the stack,
    //  or the wrong argument is already in a register).

    TEST  (t1 = t_stack  &  t2 = t_stack  &  (v1 < v2))  |  t2 = t_register  THEN
    $(
        //  Items are on the stack in the wrong order, or the second value is
        //  already in a register.

        r  :=  movetoanybutr( r_hl, t2, v2, TRUE )

        movetor( r_hl, t1, v1 )
    $)
    ELSE
    $(
        //  The operands are on the stack and in the right order, or not on
        //  the stack at all.

        movetor( r_hl, t1, v1 )

        r  :=  movetoanybutr( r_hl, t2, v2, TRUE )
    $)

    TEST  op = s_add  THEN  code_ss_1( i_addhl, r )
    ELSE
    $(
        code_s_1( i_or, r_a )
        code_ss_2( esc_ed, i_sbchl, r )

        //  In this case, and only in this case are the condition codes set
        //  ready for a branch.  Set the condition code flag to indicate
        //  this.

        compare_cc  :=  TRUE
    $)

    unsetslave( r_hl )

    evaltype  :=  t_register

    RESULTIS  r_hl
$)



AND cg_logicalop( op, t1, v1, t2, v2 )  =  VALOF
$(
//  Code generate for a logical operator, which is one of:
//
//      AND
//      OR
//      EQV
//      NEQV
//
//  All the operators are symmetric, and they will already have been put
//  the optimal way round for our purposes.  We must first put operand 1
//  into a register.

    LET r1    =  0
    LET r1h   =  0
    LET r1l   =  0

    //  The instructions to be compiled depend on the operation being
    //  performed.  Both EQV and NEQV are in fact XOR operation, with
    //  the added overhead with EQV being that the result must be
    //  complemented.

    LET inst  =  op = s_logand  ->  i_and,
                 op = s_logor   ->  i_or,
                                    i_xor

    LET cmpl  =  op = s_eqv

    //  If the items are on the stack in the wrong order, or the second
    //  operand is already in a register, then we should swap them, since
    //  all logical operators are symmetric.

    IF  (t1 = t_stack  &  t2 = t_stack  &  (v1 < v2))  |  t2 = t_register  THEN
    $(
        LET ot1  =  t1
        LET ov1  =  v1

        t1  :=  t2
        v1  :=  v2
        t2  :=  ot1
        v2  :=  ov1
    $)

    //  We can now safely move the items into their relevant register(s).

    r1   :=  movetoanyr( t1, v1, FALSE )
    r1h  :=  highbyte( r1 )
    r1l  :=  lowbyte( r1 )

    //  Now look at the data type of the second operand, and see if we can
    //  optimise the operation.

    TEST  t2 = t_number  THEN
    $(
        //  We can optimise this operation, since there is no need to use a
        //  second register.  We may also be able to optimise the operation
        //  further if either if the 8 bit operations involve constants
        //  which are all zeros or all ones.  Also, if the operation is EQV,
        //  then we can do the complement now, and not compile code to do it.

        LET v2h  =  0
        LET v2l  =  0

        IF  cmpl  THEN  v2  :=  NOT v2

        v2h  :=  (v2 >> 8)  &  #XFF
        v2l  :=  (v2)       &  #XFF

        cg_logical_byte( inst, r1h, v2h )
        cg_logical_byte( inst, r1l, v2l )
    $)
    ELSE
    $(
        //  We don't know what this is, so we had better load it into a
        //  register for good measure.

        LET r2  =  movetoanybutr( r1, t2, v2, TRUE )

        LET r2h  =  highbyte( r2 )
        LET r2l  =  lowbyte( r2 )

        //  Having done that, we can compile the code.

        cg_logical_byter( inst, r1h, r2h, cmpl )
        cg_logical_byter( inst, r1l, r2l, cmpl )
    $)

    //  Whatever code we compiled, the result is in register "r1", and so
    //  we can return that now.

    unsetslave( r1 )

    evaltype  :=  t_register

    RESULTIS  r1
$)



AND cg_logical_byte( inst, r, v )  BE
$(
//  Code generate for a 1 byte logical instruction.  We can perhaps
//  optimise the instruction depending on what the constant is.

    IF  (v = #X00)  &  (inst = i_and)  THEN
    $(
        //  Anything ANDed with #X00 is in fact #X00, and hence
        //  we can generate a sequence for this easily.

        code_s_1( i_xor, r_a )
        code_rr_1( i_ldrr, r, r_a )

        RETURN
    $)

    IF  (v = #XFF)  &  (inst = i_or)  THEN
    $(
        //  Anything ORed with #XFF is #XFF, and hence we can
        //  optimise this case as well.

        code_s_1( i_xor, r_a )
        code_i_1( i_cpl )
        code_rr_1( i_ldrr, r, r_a )

        RETURN
    $)

    IF  ((v = #X00)  &  (inst = i_or  |  inst = i_xor))  |
        ((v = #XFF)  &  (inst = i_and))  THEN

        //  This is easy to do, since anything ORed or XORed with
        //  #X00 is itself, as is anything ANDed with #XFF.

        RETURN

    IF  (v = #XFF)  &  (inst = i_xor)  THEN
    $(
        //  Anything XORed with #XFF is in fact the complement of it,
        //  and hence we can compile that.

        code_rr_1( i_ldrr, r_a, r )
        code_i_1( i_cpl )
        code_rr_1( i_ldrr, r, r_a )

        RETURN
    $)

    //  If all those potential optimisations fail, then we had better
    //  compile the code properly.

    code_rn_2( i_ldrn, r_a, v )
    code_s_1( inst, r )
    code_rr_1( i_ldrr, r, r_a )
$)



AND cg_logical_byter( inst, r1, r2, cmpl )  BE
$(
//  Code generate for a register to register logical operation.  The result
//  should be left in "r1", and the flag "cmpl" says whether the result
//  should be complemented or not.

    code_rr_1( i_ldrr, r_a, r1 )
    code_s_1( inst, r2 )

    IF  cmpl  THEN  code_i_1( i_cpl )

    code_rr_1( i_ldrr, r1, r_a )
$)



AND cg_rshift( t1, v1, t2, v2 )  =  VALOF
$(
//  Generate code for a ">>" operation.  We can optimise this if the amount to
//  be shifted by is a number, and that number is 8.

    TEST  t2 = t_number  &  v2 = 8  THEN
    $(
        //  This is simple.  We just move the value to be shifted into a
        //  register, and then swap the bytes.

        LET r   =  movetoanyr( t1, v1, FALSE )
        LET rh  =  highbyte( r )
        LET rl  =  lowbyte( r )

        code_rr_1( i_ldrr, rl, rh )
        code_s_1( i_xor, r_a )
        code_rr_1( i_ldrr, rh, r_a )

        unsetslave( r )

        evaltype  :=  t_register

        RESULTIS  r
    $)
    ELSE

    //  We cannot optimise this easily, so take the easy way out, and call
    //  the monitor function to do the job for us.

    RESULTIS  cg_stackop( s_rshift, t1, v1, t2, v2 )
$)



AND cg_lshift( t1, v1, t2, v2 )  =  VALOF
$(
//  Generate code for a "<<" operation.  We can optimise this if the
//  amount to be shifted by is a number, and that number is a factor of
//  two.

    TEST  t2 = t_number  &  factorof2( v2 )  THEN
    $(
        //  We can optimise shifting left by 2 just by adding registers
        //  together.  We can optimise even further if we are shifting by
        //  8, in which case we can just swap the halves of the register

        LET r   =  0
        LET rh  =  0
        LET rl  =  0

        TEST  v2 = 8  THEN
        $(
            r   :=  movetoanyr( t1, v1, FALSE )
            rh  :=  highbyte( r )
            rl  :=  lowbyte( r )

            code_rr_1( i_ldrr, rh, rl )
            code_s_1( i_xor, r_a )
            code_rr_1( i_ldrr, rl, r_a )
        $)
        ELSE
        $(
            //  This is only slightly more complicated, since we can perform
            //  the shift by repeated addition.

            movetor( r_hl, t1, v1 )

            FOR  i = 1  TO  v2  DO  code_ss_1( i_addhl, r_hl )

            r  :=  r_hl
        $)

        //  The result is now in register "r", and we can return this as the
        //  result of the operation.

        unsetslave( r )

        evaltype  :=  t_register

        RESULTIS  r
    $)
    ELSE

    //  We cannot optimise this easily, so take the easy way out, and turn
    //  this into a stack operation.

    RESULTIS  cg_stackop( s_lshift, t1, v1, t2, v2 )
$)



AND factorof2( n )  =  (n & -n)  =  n


/*****************************************************************************\
*                           Systems Research Group                            *
*******************************************************************************


                       #####    ######               ####   
                      #######  ########             ######  
                     ##        ##                  ##    ## 
                     ##        ##  ####   #######     ##### 
                     ##        ##    ##                  ## 
                     ##        ##    ##            ##    ## 
                      #######  ########             ######  
                       #####    ######               ####   


*******************************************************************************
*   I. D. Wilson           Last Modified   -   IDW   -   18/02/85             *
\*****************************************************************************/



///SECTION "CG-3"


LET cg_stackop( op, t1, v1, t2, v2 )  =  VALOF
$(
//  Code generate for operators which must call monitor functions.  We must
//  put the arguments onto the stack, and then call the routine.  We then
//  leave the result on the stack, in case we are about to call another
//  monitor routine.

    IF  t1 = t_register  THEN
    $(
        //  Already in a register, so stack it immediately.

        v1  :=  stackitem( t1, v1 )
        t1  :=  t_stack
    $)

    IF  t2 = t_register  THEN
    $(
        //  Also already in a register, so stack it immediately.

        v2  :=  stackitem( t2, v2 )
        t2  :=  t_stack
    $)

    //  If either of the arguments were in registers, then thay have already
    //  been stacked.  We should now stack any other items which might
    //  themselves need registers.

    UNLESS  t1 = t_stack  DO  v1  :=  stackitem( t1, v1 )
    UNLESS  t2 = t_stack  DO  v2  :=  stackitem( t2, v2 )

    //  We should now make sure that the operands are on the stack in the
    //  right order.

    TEST  (v1 = stackp-2  &  v2 = stackp-1)  THEN

        //  This is a standard monitor function without the necessity to
        //  change the operation.

        monfunction( op, FALSE )

    ELSE

    TEST  (v2 = stackp-2  &  v1 = stackp-1)  THEN

        //  This is a monitor function, where the operation must be
        //  reversed.

        monfunction( op, TRUE )

    //  If neither of the above are true, then we have somehow got our
    //  stack in a twist.  This is an internal error, and hence we should
    //  moan.

    ELSE  cgerror( "cg_stackop( %N %N )", v1, v2 )

    //  After all that, the result is left on the stack, and so all we do is
    //  bring the stack down by one, and return the result.

    destackitem()

    evaltype  :=  t_stack

    RESULTIS  stackp-1
$)



AND stackitem( type, value )  =  VALOF
$(
//  Put the item given onto the stack.  It may already be in a register,
//  in which case we can push it immediately.  Otherwise, we must look to
//  see if we require a system call to get the item, in which case we can
//  leave it on the stack after the system call.  Otherwise, we must put
//  it in a register, and push that.

    LET r  =  lookinslave( type, value )

    IF  debug & liststream  THEN  writel( ";  Stack %N,%N  SP=%N", type, value, stackp )

    IF  r = r_none  THEN

        //  There is no mention of this item in the slave already, so we
        //  must adopt a different approach.

        TEST  systemindex( type, value )  THEN
        $(
            //  This is a value which we must call the system to get,
            //  and so we can stack the value immediately.

            stackcherished()

            RESULTIS  loadindexvalue( type, value )
        $)
        ELSE

            //  It appears as though there is nothing for it, but to move this
            //  value into a register.

            r  :=  movetoanyr( type, value, TRUE )

    //  At this point, "r" is the register containing the value we want.
    //  generate code to push the value.

    stackcherished()

    code_ss_1( i_push, r )

    stackp  :=  stackp + 1

    RESULTIS  stackp-1
$)



AND destackitem()  BE  
$(
    stackp  :=  stackp - 1

    IF  debug & liststream  THEN  writel( ";  Destack  SP=%N", stackp )
$)



AND cg_monadic( op, node )  =  VALOF
$(
//  Generate code for a monadic operator.  Do any constant folding if
//  possible.

    LET v  =  evaluate( node )
    LET t  =  evaltype

    //  If the value is a constant, then we can evaluate it here and
    //  now.

    IF  t = t_number  THEN
    $(
        evaltype  :=  t

        SWITCHON  op  INTO
        $(
            CASE s_neg  :  RESULTIS  -v
            CASE s_not  :  RESULTIS  NOT v
            CASE s_abs  :  RESULTIS  ABS v

            CASE s_rv   :  ENDCASE

            DEFAULT     :  cgerror( "cg_monadic( %N )", op )
        $)
    $)

    //  If we drop through there, then there is nothing for it but to compile
    //  some code.

    SWITCHON  op  INTO
    $(
        CASE s_neg  :  RESULTIS  cg_neg( t, v )
        CASE s_not  :  RESULTIS  cg_not( t, v )
        CASE s_abs  :  RESULTIS  cg_abs( t, v )
        CASE s_rv   :  RESULTIS  cg_rv( t, v )

        DEFAULT     :  cgerror( "cg_monadic( %N )", op )
    $)
$)



AND cg_neg( type, value )  =  VALOF
$(
//  Generate code for the "negate" function.  Since we are working on a
//  twos complement machine, this is the same as a NOT, followed by an
//  increment.

    LET v  =  cg_not( type, value )
    LET t  =  evaltype

    LET r  =  movetoanyr( t, v, TRUE )

    code_ss_1( i_inc, r )

    unsetslave( r )

    evaltype  :=  t_register

    RESULTIS  r
$)



AND cg_not( type, value )  =  VALOF
$(
//  Code generate for a logical "NOT" operation.

    LET r   =  movetoanyr( type, value, FALSE )
    LET rh  =  highbyte( r )
    LET rl  =  lowbyte( r )

    code_rr_1( i_ldrr, r_a, rh )
    code_i_1( i_cpl )
    code_rr_1( i_ldrr, rh, r_a )

    code_rr_1( i_ldrr, r_a, rl )
    code_i_1( i_cpl )
    code_rr_1( i_ldrr, rl, r_a )

    unsetslave( r )

    evaltype  :=  t_register

    RESULTIS  r
$)



AND cg_abs( type, value )  =  VALOF
$(
//  Code generate for the ABS operator.  This involves a call to the operating
//  system, and hence we must put the argument onto the stack.

    UNLESS  type = t_stack  DO  value  :=  stackitem( type, value )

    //  At this point, the item is on the stack, but we must do an internal
    //  check to make sure that it is in the right place.

    UNLESS  value = stackp-1  DO  cgerror( "cg_abs( %N )", value )

    monfunction( s_abs, FALSE )

    //  The result is on the stack, so return this as the value.

    evaltype  :=  t_stack

    RESULTIS  value
$)



AND cg_rv( type, value )  =  VALOF
$(
//  Code generate for the "!" operator.  This means moving the value into
//  HL, making it a machine pointer, and then indirecting on it.

    movetor( r_hl, type, value )

    //  The operand is now in the HL register.  Before we can indirect on
    //  it, we must make it a byte address.

    code_ss_1( i_addhl, r_hl )

    //  We can now load the indirect value into HL.

    code_rr_1( i_ldrr, r_a, r_ihl )
    code_ss_1( i_inc, r_hl )
    code_rr_1( i_ldrr, r_h, r_ihl )
    code_rr_1( i_ldrr, r_l, r_a )

    unsetslave( r_hl )

    evaltype  :=  t_register

    RESULTIS  r_hl
$)



AND cg_byteop( op, t1, v1, t2, v2 )  =  VALOF
$(
//  Code generate for a "getbyte" or "putbyte" operation.  This involves
//  making a machine representation of the first operand, adding in the
//  second operand, and then indirecting on the result.

    LET noadd  =  FALSE

    IF  t1 = t_number  &  t2 = t_number  THEN
    $(
        //  This is a simple case.  We can evaluate the address immediately.
        
        v1     :=  (v1 * bytesperz80word)  +  v2
        v2     :=  0
        
        noadd  :=  TRUE
    $)

    IF  t1 = t_number  &  v1 = 0  THEN
    $(
        //  This is also very simple, since we know that the address is now
        //  (t2,v2).
        
        t1     :=  t2
        v1     :=  v2
        
        t2     :=  t_number
        v2     :=  0
        
        noadd  :=  TRUE
    $)

    //  We really do have to compile the addition.  We have to deal with
    //  all the complicated cases where the arguments are all mixed up.

    IF  t1 = t_register  THEN
    $(
        //  Better cherish the register, just to make sure it doesn't get
        //  clobbered by the following load.
            
        LET rnode  =  block2( t1, v1 )
        
        cherish( v1, rnode )

        t1  :=  t_cherished
        v1  :=  rnode
    $)

    UNLESS  (t1 = t_stack  &  t2 = t_stack  &  (v1 > v2))  |  t2 = t_number  DO
    $(
        LET r  =  movetoanybutr( r_hl, t2, v2, TRUE )
        
        t2  :=  t_register
        v2  :=  r
    $)

    //  Now, restore the status of the first argument, and move it into HL.
    
    IF  t1 = t_cherished  THEN
    $(
        LET node  =  v1
        
        t1  :=  node!n_type
        v1  :=  node!n_arg1
        
        IF  t1 = t_register  THEN  uncherish( v1 )
        
        freeblock2( node )
    $)

    TEST  noadd  THEN

        //  This is a simple case, since we have already calculated the address
        //  and hence we need not do the addition.
             
        movetor( r_hl, t1, v1 )
            
    ELSE
    $(
        //  More complicated.  We have to calculate the address by
        //  addition.

        movetor( r_hl, t1, v1 )

        code_ss_1( i_addhl, r_hl )

        unsetslave( r_hl )
    $)
    
    //  Now generate code to add the two numbers together, and then indirect
    //  on the result.
    
    v1  :=  cg_plusminus( s_add, t_register, r_hl, t2, v2 )
    t1  :=  evaltype

    movetor( r_hl, t1, v1 )

    //  We must now decide whether we are generating for a "getbyte" or a
    //  "putbyte" operation.

    IF  op = s_putbyte  THEN
    $(
        //  Since the address we want is in HL, we had better cherish the
        //  register PDQ, since we don't want it to be overwritten.

        LET node1  =  arg1!a_node
        LET node2  =  block2( t_register, r_hl )

        cherish( r_hl, node2 )

        //  We are now in a state to evaluate the value which we wish to
        //  store in this location.

        cg_stack( ssp-1 )

        v1  :=  evaluate( node1 )
        t1  :=  evaltype

        t2  :=  node2!n_type
        v2  :=  node2!n_arg1
        
        IF  t2 = t_register  THEN  uncherish( v2 )

        freenode( node1 )
        freeblock2( node2 )

        //  We have now evaluated both the address and the value.  Move the
        //  address back into HL (if it had been moved), and then decide on
        //  how we intend to store the value.

        UNLESS  (t1 = t_stack  &  t2 = t_stack  &  (v1 < v2))  |  t1 = t_number  DO
        $(
            LET r  =  movetoanybutr( r_hl, t1, v1, TRUE )
            
            t1  :=  t_register
            v1  :=  r
        $)

        //  It is now safe to bring the indirection address back into HL from
        //  wherever it was hiding.

        movetor( r_hl, t2, v2 )

        TEST  t1 = t_number  THEN
        $(
            //  If we are storing a number, then there is no point in wasting
            //  another register.  We just store the byte as data.

            LET byte  =  v1  &  #XFF

            code_rn_2( i_ldrn, r_ihl, byte )
        $)
        ELSE
        $(
            //  Otherwise, we must load the data into a register, and then
            //  store it.

            LET r  =  movetoanybutr( r_hl, t1, v1, TRUE )

            code_rr_1( i_ldrr, r_ihl, lowbyte( r ) )
            
            unsetslave( r )
        $)

        unsetslave( r_hl )

        RESULTIS  NIL
    $)

    //  Otherwise, we assume that we are dealing with a "getbyte" operation,
    //  and so we can get on with it.

    code_rr_1( i_ldrr, r_l, r_ihl )
    code_s_1( i_xor, r_a )
    code_rr_1( i_ldrr, r_h, r_a )

    unsetslave( r_hl )

    evaltype  :=  t_register

    RESULTIS  r_hl
$)



AND cg_stindop( t1, v1, t2, v2 )  =  VALOF
$(
//  Code generate for the STIND operation.  We have used the "dyadic" op
//  routines to evaluate the arguments.  We should now calculate the
//  machine address of the first operand, and then store the second operand
//  in that location.

    IF  t1 = t_register  THEN
    $(
        //  Better cherish the register, just to make sure it doesn't get
        //  clobbered by the following load.
        
        LET rnode  =  block2( t1, v1 )
        
        cherish( v1, rnode )

        t1  :=  t_cherished
        v1  :=  rnode
    $)

    UNLESS  (t1 = t_stack  &  t2 = t_stack  &  (v1 > v2))  |  t2 = t_number  DO
    $(
        LET r  =  movetoanybutr( r_hl, t2, v2, TRUE )
        
        t2  :=  t_register
        v2  :=  r
    $)

    //  Now, restore the status of the first argument, and move it into HL.
    
    IF  t1 = t_cherished  THEN
    $(
        LET node  =  v1
        
        t1  :=  node!n_type
        v1  :=  node!n_arg1
        
        IF  t1 = t_register  THEN  uncherish( v1 )
        
        freeblock2( node )
    $)

    movetor( r_hl, t1, v1 )

    code_ss_1( i_addhl, r_hl )

    unsetslave( r_hl )

    //  Now, look at the second operand, and decide what to do.  If it is
    //  a number, then we store the number immediately.  Otherwise, we
    //  put the value into a register, and then store the register.

    TEST  t2 = t_number  THEN
    $(
        LET hb  =  (v2 >> 8)  &  #XFF
        LET lb  =  (v2)       &  #XFF

        code_rn_2( i_ldrn, r_ihl, lb )
        code_ss_1( i_inc, r_hl )
        code_rn_2( i_ldrn, r_ihl, hb )
    $)
    ELSE
    $(
        //  We must load the second value into a register, and then store
        //  that register.

        LET r   =  movetoanybutr( r_hl, t2, v2, TRUE )
        LET rh  =  highbyte( r )
        LET rl  =  lowbyte( r )

        code_rr_1( i_ldrr, r_ihl, rl )
        code_ss_1( i_inc, r_hl )
        code_rr_1( i_ldrr, r_ihl, rh )
    $)

//******  UNSAFE
//******  cg_unslavestorage()
//******  UNSAFE

    RESULTIS  NIL
$)



AND cg_goto()  BE
$(
//  Generate code for a GOTO statement.  The top item on the stack represents
//  an address to jump to.

    LET node  =  arg1!a_node
    LET v     =  0
    LET t     =  0

    cg_stack( ssp-1 )
    cg_store( 0, ssp )

    //  Having got the stack into a sanitary state, we can evaluate the node
    //  expression, and put the resulting value either into HL, or on the
    //  stack.  We can then generate either a JP (HL) or a RET instruction.

    v  :=  evaluate( node )
    t  :=  evaltype

    UNLESS  t = t_stack  DO

        //  The item is not on the stack, so look to see where it is.  If we
        //  must access the value via the stack, then there is no point in
        //  destacking the item.

        TEST  systemindex( t, v )  THEN
        $(
            v  :=  loadindexvalue( t, v )
            t  :=  t_stack
        $)
        ELSE
        $(
            movetor( r_hl, t, v )

            v  :=  r_hl
            t  :=  t_register
        $)

    //  We should now generate either a JP (HL) or a RET instruction,
    //  depending on where the value is.

    TEST  t = t_stack  THEN
    $(
        code_i_1( i_ret )

        destackitem()
    $)
    ELSE  code_i_1( i_jpihl )

    //  Having done all that, we can leave the realm of compiled code,
    //  discarding all the registers etc.

    freenode( node )

    stopcoding()
$)



AND cg_setlabel( type, label )  BE
$(
//  Set a label.  On finding a label, we can assume that code is reachable
//  again, and hence we can start coding.  The type is "blab" or "lab",
//  the difference being irrelevant to us.

    startcoding()

    cg_store( 0, ssp )
    discardslave()

    setlabel( label )
    setlabelrefs( label )
$)



AND cg_entry( )  BE
$(
//  Code generate an entry point to a procedure.  The value passed to us is the
//  length of the name of the procedure.

    LET label  =  rdl()
    LET n      =  rdn()     // Modern Cintcode has the label first
    LET o      =  output()
    LET s      =  getstore( n/bytesperword )

    procdepth  :=  procdepth + 1
    
    startcoding()

    bcplalign()

    FOR  i = 1  TO  n  DO  s % i  :=  rdn()

    s % 0  :=  n

    addname( s, label )

    IF  liststream  THEN  writel( ";  Entry to *"%S*"", s )

    selectoutput( sysout )
    writef( "%X4:  *"%S*"*N", currentloc, s )
    selectoutput( o )

    cg_setlabel( s_lab, label )
$)



AND cg_save( n )  BE
$(
//  Code generate for a SAVE statement.  This occurs only after entry to a
//  procedure, and gives an indication of how many arguments are expected.
//  All the linkage set up is done by one of three system routines depending
//  on how many arguments there are.

    LET args  =  n - savespacesize
    LET regs  =  args > 3  ->  3, args

    LET monf  =  args = 0  ->  m_setlink0,
                 args = 1  ->  m_setlink1,
                 args = 2  ->  m_setlink2,
                               m_setlink3

    //  Bring the stack down to a reasonable position, and then load the
    //  register arguments onto the stack.

    cg_stack( savespacesize )

    callmonfunction( monf )

    //  Stack the registers, and other local variables...

    FOR  i = 0  TO  regs-1  DO
    $(
        //  Add the register slave information, and then load the register
        //  concerned onto the simulated stack.
        
        LET r  =  i    //  See MANIFEST declarations

        addslaveinfo( r, t_local, ssp )

        cg_loadt( t_local, ssp )
    $)

    //  We should now load the relevant number of local variables onto the
    //  stack, and we are ready for the body of the procedure.

    FOR  i = regs  TO  args-1  DO  cg_loadt( t_local, ssp )
$)



AND cg_return( op )  BE
$(
//  Code generate for the RETURN operation.  The return link is on the BCPL
//  stack, and there may or may not be a result to be passed back.

    IF  op = s_fnrn  THEN
    $(
        //  We must return a result.  The result is currently in "arg1", and
        //  it should be evaluated and placed in HL before returning.

        LET node  =  arg1!a_node
        LET v     =  evaluate( node )
        LET t     =  evaltype

        cg_stack( ssp-1 )

        movetor( r_hl, t, v )

        freenode( node )
    $)

    //  We must now return.  The code after this is unreachable, so stop
    //  compiling code.

    callmonfunction( m_return )

    stopcoding()
$)



AND cg_endproc( )  BE // ENDPROC no longer has an argument
$(
//  End of a procedure.  This means that we should stop coding, and decrement
//  the procedure depth value.

    //UNLESS  n = 0  DO  cgerror( "cg_endproc( %N )", n )
 
    IF  liststream  THEN  writel( ";  End of procedure" )

    stopcoding()

    procdepth  :=  procdepth - 1
$)



AND cg_res( label )  BE
$(
//  Code generate for a RESULTIS statement.  The value to be evaluated is
//  currently in ARG1, with the label to be jumped to in "label".  We can
//  usually optimise this, since OCODE of the following form is common:
//
//      RES Ln   LAB Ln
//  or  RES Ln   STACK n  LAB Ln

    LET op    =  0
    LET node  =  arg1!a_node
    LET v     =  0
    LET t     =  0

    //  First, move the evaluated argument into HL, and then bring the stack
    //  down by one.

    cg_store( 0, ssp-1 )

    v  :=  evaluate( node )
    t  :=  evaltype

    movetor( r_hl, t, v )

    cg_stack( ssp-1 )

    freenode( node )

    //  Now, look ahead in the ocode, for the corresponding LAB statement.

    op  :=  rdop()

    WHILE  op = s_stack  DO
    $(
        cg_stack( rdn() )

        op  :=  rdop()
    $)

    //  Now, look to see if we have to set a label.  If not, we had better
    //  undo the damage done, and generate the jump instruction after
    //  all.

    TEST  op = s_lab  THEN
    $(
        //  This is a LAB statement, but is it the right one ?

        LET l  =  rdl()

        UNLESS  l = label  DO

            //  This is a label, but not the one we were expecting.  We should
            //  generate a jump for the RES, and then set the new label.

            cg_branch( cond_always, label )

        cg_setlabel( s_lab, l )
    $)
    ELSE
    $(
        //  We are even further up the creek, since this isn't even a LAB
        //  instruction.  Generate the JUMP, and then parse the OCODE
        //  statement.

        cg_branch( cond_always, label )

        stopcoding()

        parseocode( op )
    $)
$)



AND cg_rstack( n )  BE
$(
//  Generate code for the RSTACK instruction.  This is called immediately after
//  a RES instruction, and hence we know that the result to be stacked is
//  in HL.

    cg_stack( n )

    discardslave()
    addslaveinfo( r_hl, t_local, ssp )

    cg_loadt( t_register, r_hl )
$)


/*****************************************************************************\
*                           Systems Research Group                            *
*******************************************************************************


                       #####    ######                ###   
                      #######  ########              ####   
                     ##        ##                   ## ##   
                     ##        ##  ####   #######  ######## 
                     ##        ##    ##                ##   
                     ##        ##    ##                ##   
                      #######  ########                ##   
                       #####    ######                 ##   


*******************************************************************************
*   I. D. Wilson           Last Modified   -   IDW   -   25/02/85             *
\*****************************************************************************/



///SECTION "CG-4"


LET cg_apply( op, stacksize )  BE
$(
//  Generate code for a routine or function application.  The value "op" is
//  an indication as to whether this is a routine or function application,
//  and "stacksize" is the size of the stack after the call.  We must put the
//  first three arguments into registers, and the rest of the items onto
//  the BCPL stack.

    LET args   =  (ssp - 1) - stacksize - savespacesize
    LET regs   =  args < 3  ->  args, 3
    
    LET tvec   =  VEC 2
    LET vvec   =  VEC 2

    //  The top item on the stack is the address of the routine to be called.
    //  We should evaluate the results in reverse order, and store them on
    //  the stack if necessary.

    LET pnode  =  arg1!a_node
    LET pt     =  0
    LET pv     =  0
    
    LET stckp  =  stacksize + savespacesize + regs

    //  Bring the stack down by one, and then scan the stack looking for any
    //  reference to the result resister.  We must unfortunately store this
    //  register, since the evaluation of the procedure address may corrupt it.

    cg_stack( ssp-1 )
    
    scanstack()

    //  We must evaluate the "pnode" to find out what sort of an animal it is.

    pv  :=  evaluate( pnode )
    pt  :=  evaltype
    
    //  Now, look to see if the address of the routine is already in
    //  a register, and if it is, we must cherish it, since we are about
    //  to swap register sets.

    IF  pt = t_register  THEN  
    $(
        LET rnode  =  block2( t_register, pv )
        
        cherish( pv, rnode )
        
        pt  :=  t_cherished
        pv  :=  rnode
    $)

    //  Now, store all those items which are below the level of the new
    //  stack frame.

    cg_store( 0, stacksize )

    //  Now, handle this arguments which must be stacked rather than passed
    //  in registers.

    cg_store( stckp, ssp )

    cg_stack( stckp )

    //  We must now take the other arguments, and put them into registers.
    //  This may require much munging around, and hence we must be careful
    //  in which order we do things.

    FOR  r = regs-1  TO  0  BY  -1  DO
    $(
        //  Pick up the relevant item on the stack, and evaluate it, keeping
        //  note of where the result was put.

        LET node     =  arg1!a_node

        LET v        =  evaluate( node )
        LET t        =  evaltype

        LET regnode  =  block2( t_register, r )

        movetor( r, t, v )
        cherish( r, regnode )

        cg_stack( ssp-1 )

        tvec!r  :=  t_cherished
        vvec!r  :=  regnode

        freenode( node )
    $)

    //  The registers now contain what they should, except they may have been
    //  cherished behind our back.  We must bring the registers back from
    //  the stack in this instance.
    
    FOR  r = 0  TO  regs-1  DO
    $(
        LET t   =  tvec!r
        LET v   =  vvec!r
        LET nt  =  v!n_type
        LET nv  =  v!n_arg1

        tvec!r  :=  nt
        vvec!r  :=  nv
        
        freeblock2( v )

        IF  nt = t_register  THEN  uncherish( nv )
    $)

    //  Now scan the table of cherished values, until no registers need
    //  destacking.
    
    $(  //  Repeat loop to scan the type and value tables until no change
        //  can be made.
        
        LET rmax     =  r_none
        LET vmax     =  0
        
        FOR  r = 0  TO  regs-1  DO
        $(
            LET t  =  tvec!r
            LET v  =  vvec!r
            
            IF  t = t_stack  THEN
            
                //  This item is still on the stack, and so should be
                //  considered, but only if it is the top item.

                UNLESS  v < vmax  DO
                $(
                    rmax  :=  r
                    vmax  :=  v
                $)
        $)
        
        IF  rmax = r_none  THEN  BREAK
        
        //  Otherwise, destack this register, and continue looking.
        
        destackregister( rmax, vmax )
        
        tvec!rmax  :=  t_register
        vvec!rmax  :=  rmax
    $)
    REPEAT  //  Until all stacked items have been destacked.
        
    //  All arguments are now set up for the big day.  We should load
    //  the procedure address, and then call the run time system to call
    //  the procedure in a BCPL environment.
    
    IF  pt = t_cherished  THEN
    $(
        //  The procedure address is cherished, and so we should restore it
        //  to its former glory.
        
        LET nt  =  pv!n_type
        LET nv  =  pv!n_arg1
        
        IF  nt = t_register  THEN  uncherish( nv )
        
        pt  :=  nt
        pv  :=  nv
    $)
    
    //  We have to decide here how to load the procedure address into HL.  It
    //  may already be in a register, in which case we must stack it.  It may
    //  be only available after a call to the run time system, in which case
    //  we should also stack it.  Otherwise it is safe to leave it after the
    //  register set switch.

    UNLESS  args = 0  DO  
    $(
        //  There are some arguments, so we must swap register sets.
        
        IF  pt = t_register  |  systemindex( pt, pv )  THEN
        $(
            //  Value is not directly loadable after a register set switch, so
            //  put it on the stack in order to preserve its value.
            
            pv  :=  stackitem( pt, pv )
            pt  :=  t_stack
        $)
        
        //  It is now safe to swap register sets, since the value is in store.

        code_i_1( i_exx )
    $)

    discardslave()

    movetor( r_hl, pt, pv )

    callmonfunction( m_apply )
    monargument( stacksize )

    freenode( pnode )

    //  On return from the procedure call, we must clear the register slave,
    //  and set the stack back to what we originally thought.  If this is a
    //  function application, then there will be a result which should be
    //  stacked.

    discardslave()

    cg_stack( stacksize )

    IF  op = s_fnap  THEN
    $(
        addslaveinfo( r_hl, t_local, ssp )

        cg_loadt( t_register, r_hl )
    $)
$)



AND scanstack()  BE
$(
//  Scan the stack, looking for elements which reference the result register.
//  When found, this item should be stored.

    LET found  =  FALSE
    LET arg    =  arg1
    
    UNTIL  arg = NIL  DO
    $(
        LET node  =  arg!a_node
        LET link  =  arg!a_link
        LET bool  =  scanforregister( node )
        
        IF  found & bool  THEN  cgerror( "scanstack( 2regs )" )
        
        found  :=  found | bool
        arg    :=  link
    $)
$)

    

AND cg_finish()  BE
$(
//  Generate code for the FINISH statement.  This is simple, since it is
//  just a call into the run time system.

    callmonfunction( m_finish )
    
    stopcoding()
$)



AND cg_global( n )  BE
$(
//  Generate code for the end of a section.  The value "n" is the number
//  of entries in the global table.

    incode  :=  TRUE

    IF  n = 0  THEN  cgwarning( "Code is unreachable" )

    bcplalign()

    code_n_2( 0 )

    FOR  i = 1  TO  n  DO
    $(
        LET gn  =  rdg()
        LET gl  =  rdl()
        LET nl  =  namelist

        code_n_2( gn*bytesperz80word ) // Note that the global numbers are scaled by 2
        code_l_2( gl, lookuplabel( gl ) )

        //  Look in the name list for this entry.  When we have found it,
        //  update the entry so that it reflects the "global" status.
        
        UNTIL  nl = NIL  DO
        $(
            IF  nl!nl_type = t_local  &  nl!nl_value = gl  THEN
            $(
                nl!nl_type   :=  t_global
                nl!nl_value  :=  gn
                
                BREAK
            $)
            
            nl  :=  nl!nl_link
        $)
    $)

    code_n_2( maxgn*bytesperz80word )

    stopcoding()
$)



AND cg_comparejump( label, op, node1, node2 )  BE
$(
//  Generate code for a "compare" followed by a "jump" instruction.  We can
//  look to see if this can be optimised.  If "node2" is NIL, this means that
//  a second operand of zero is assumed.

    TEST  node2 = NIL  THEN
    $(
        //  We must evaluate the node represented by "node1" and then set the
        //  condition codes on the result.  

        LET v  =  evaluate( node1 )
        LET t  =  evaltype
        
        //  If the value is numeric, then we know know whether we should jump
        //  or no.
        
        TEST  t = t_number  THEN

            //  Look at the value and decide whether an unconditional branch
            //  should be compiled.
            
            IF  (op = s_eq  &  v = 0)  |  (op = s_ne  &  v \= 0)  THEN

                //  The condition matches, and so an unconditional branch
                //  should be compiled.
                
                cg_jump( label )
                
        ELSE
        $(
            //  Not quite so easy.  This value should be moved into a register,
            //  and then tested for equality with zero.
            
            LET r   =  movetoanyr( t, v, TRUE )
            LET rh  =  highbyte( r )
            LET rl  =  lowbyte( r )
            
            //  If the original node indicates that an indirection had to be
            //  made, then the result will already be in HL, and the low byte
            //  will already be in "A".
            
            UNLESS  node1!n_type = s_rv  DO  code_rr_1( i_ldrr, r_a, rl )

            code_s_1( i_or, rh )
            
            cg_branch( condition( op ), label )
        $)
    $)
    ELSE
    $(
        //  This is not quite so easy.  We should evaluate the condition
        //  by subtraction, and then decide what to do.  First, we should
        //  get rid of the two conditions which we cannot handle.
            
        LET t       =  0
        LET v       =  0
        LET t1      =  0
        LET v1      =  0
        LET t2      =  0
        LET v2      =  0
        LET noload  =  0

        IF  op = s_gr  |  op = s_le  THEN
        $(
            LET n  =  node1
                
            node1  :=  node2
            node2  :=  n
                
            op     :=  reverse( op )
        $)
        
        //  Look to see whether this is an indirection, followed by a
        //  comparison with zero.  If this is the case, then we can ignore
        //  one of the loads later on.
        
        IF  node1!n_type = s_rv  &  (node2!n_type = t_number  &  node2!n_arg1 = 0)  THEN
        
            //  This is exactly what we want.  The first operand is an
            //  indirection, and the second operand is zero.  Set the
            //  "noload" flag, so we can save ourselves work later.
            
            noload  :=  TRUE
            
        //  We now know that we have a condition which we can turn into
        //  a conditional branch.

        IF  node2!n_type = t_register  THEN
        $(
            //  The second argument is already in a register, so cherish it to
            //  stop it being clobbered by the evaluation of the first argument.
        
            LET r      =  node2!n_arg1
            LET rnode  =  block2( t_register, r )
            
            node2!n_type  :=  t_cherished
            node2!n_arg1  :=  rnode
            
            cherish( r, rnode )
        $)
    
        //  Now, evaluate the first argument, and see what sort of a revelation
        //  this brings.
    
        v1  :=  evaluate( node1 )
        t1  :=  evaltype
    
        //  Look at the data type of the first operand, and if it is a register
        //  then we must take special precautions to stop the register being
        //  destroyed.
    
        IF  t1 = t_register  THEN
        $(
            LET node  =  block2( t1, v1 )
    
            cherish( v1, node )
    
            t1  :=  t_cherished
            v1  :=  node
        $)
    
        //  Now evaluate the second argument.  We know that the first one will be
        //  safe, since it will be cherished if it is a register.
    
        v2  :=  evaluate( node2 )
        t2  :=  evaltype
    
        //  Having evaluated both operands, we can remove their cherished status
    
        IF  t1 = t_cherished  THEN
        $(
            LET node  =  v1
    
            t1  :=  node!n_type
            v1  :=  node!n_arg1
    
            IF  t1 = t_register  THEN  uncherish( v1 )
    
            freeblock2( node )
        $)
    
        IF  t2 = t_cherished  THEN
        $(
            LET node  =  v2
    
            t2  :=  node!n_type
            v2  :=  node!n_arg1
    
            IF  t2 = t_register  THEN  uncherish( v2 )
    
            freeblock2( node )
        $)

        //  Now, clear the "compare_cc" flag, and execute the subtraction
        //  which is going to compile the comparison.
        
        compare_cc  :=  FALSE

        v           :=  cg_plusminus( s_sub, t1, v1, t2, v2 )
        t           :=  evaltype
            
        //  We can now look to see if the result is a number.  If it is
        //  then we can compile (or not) an unconditional branch.
            
        TEST  t = t_number  THEN

            //  The result is numeric, and so if the value corresponds to
            //  the jumping condition, we can compile an unconditional
            //  jump.
                
            IF  (op = s_eq  &  v = 0)  |  (op = s_ne  &  v \= 0)  |
                (op = s_ls  &  v < 0)  |  (op = s_ge  &  v >= 0)  THEN
                    
                //  The condition is met, so compile the unconditional
                //  jump.
                    
                cg_jump( label )

        ELSE
        $(
            //  If the value is not a number, then we would expect it to
            //  be in a register.  If not, then there has been some sort
            //  of optimisation, which we must unoptimise!
                
            LET cc  =  0

            UNLESS  t = t_register  DO  
            $(
                v  :=  movetoanyr( t, v, TRUE )
                t  :=  t_register
            $)
                
            UNLESS  compare_cc  DO
            $(
                //  The condition codes have not been set unfortunately,
                //  but no matter, since we can set the condition codes
                //  now.
                    
                LET rh  =  highbyte( v )
                LET rl  =  lowbyte( v )
                
                //  At this point, it is worth checking for the common case
                //  of  (rv <cond> 0).  If this is the case, then the value
                //  will already by in HL, and A is already set up to contain
                //  L.
                
                TEST  op = s_eq  |  op = s_ne  THEN

                    //  This involves the OR of both halves of the register.
                    //  If the "noload" flag is set, then we have no need to
                    //  load the low byte of the register.

                    UNLESS  noload  DO  code_rr_1( i_ldrr, r_a, rl )

                ELSE

                    //  We must just OR the high byte of the register with
                    //  itself.
                    
                    code_rr_1( i_ldrr, r_a, rh )

                code_s_1( i_or, rh )
            $)

            //  It is worth putting some effort here into optimising the
            //  following:
            //
            //       xxxx
            //       Jx   L1
            //       JUMP L2
            //   L1: xxxx
            
            cc  :=  op
            
            op  :=  rdop()
            
            WHILE  op = s_stack  DO
            $(
                cg_stack( rdn() )
                
                op  :=  rdop()
            $)
            
            TEST  op = s_jump  THEN
            $(
                LET l1  =  rdl()
                
                //  Now read the next ocode statement, and decide whether
                //  we can optimise the jump.
                
                op  :=  rdop()
                
                WHILE  op = s_stack  DO
                $(
                    cg_stack( rdn() )
                
                    op  :=  rdop()
                $)
                
                TEST  op = s_lab  THEN
                $(
                    LET l2  =  rdl()
                    
                    //  We are now in the situation where we can do the
                    //  optimisation.  It may all have been for nothing,
                    //  but now is the moment of truth.
                    
                    TEST  l2 = label  THEN
                    $(
                        cg_branch( condition( notcond( cc ) ), l1 )
                        cg_setlabel( s_lab, l2 )
                    $)
                    ELSE
                    $(
                        //  Oh dear.  We have done all this lookahead, and
                        //  it is to no avail.
                        
                        cg_branch( condition( cc ), label )
                        cg_jump( l1 )
                        cg_setlabel( s_lab, l2 )
                    $)
                $)
                ELSE
                $(
                    //  This wasn't the OCODE we were expecting, so do
                    //  something about it.
                    
                    cg_branch( condition( cc ), label )
                    cg_jump( l1 )
                    
                    parseocode( op )
                $)
            $)
            ELSE
            $(
                //  We hardly got anywhere with the optimisation.  Oh well, 
                //  better luck next time!
                
                cg_branch( condition( cc ), label )
                
                parseocode( op )
            $)
        $)
    $)
$)



AND cg_branch( condition, label )  BE
$(
//  Generate a conditional branch instruction, depending on the condition
//  code given.

    LET labeladdr  =  lookuplabel( label )
    LET cc         =  z80cc( condition )

    IF  labeladdr \= NIL  &  relpossible( cc )  THEN
    $(
        //  This label has been set (backward jump), and hence we may be
        //  able to generate a "JR" instruction.
        
        LET offset  =  labeladdr - (currentloc + 2)
        
        UNLESS  offset < -128  DO
        $(
            //  Cooer.  This is a backward jump, and is in fact in range.
            
            cg_reljump( cc, label, offset )
            
            RETURN
        $)
    $)

    //  If we drop through that, then this means that this is an long branch
    //  and so we must generate absolute code.
    
    cg_absjump( cc, label, labeladdr )
$)



AND relpossible( cc )  =  cc = cc_none  |  cc = cc_z   |  cc = cc_nz  |
                          cc = cc_c     |  cc = cc_nc




AND cg_reljump( cc, label, offset )  BE
$(
//  Generate code for a relative jump operation.  The condition code for
//  the jump is "cc".

    LET inst  =  cc = cc_none  ->  i_jr,
                 cc = cc_z     ->  i_jrz,
                 cc = cc_nz    ->  i_jrnz,  
                 cc = cc_c     ->  i_jrc,
                 cc = cc_nc    ->  i_jrnc,  cgerror( "cg_reljump( %N )", cc )

    code_il_2( inst, label, offset & #XFF )
$)



AND cg_absjump( cc, label, location )  BE
$(
//  Generate code for an absolute jump.  The location given is either NIL,
//  implying that this label hasn't been set yet.

    LET inst  =  cc = cc_none  ->  i_jp,
                 cc = cc_z     ->  i_jpz,
                 cc = cc_nz    ->  i_jpnz,
                 cc = cc_p     ->  i_jpp,
                 cc = cc_m     ->  i_jpm,  
                 cc = cc_c     ->  i_jpc,
                 cc = cc_nc    ->  i_jpnc,  cgerror( "cg_absjump( %N )", cc )

    code_il_3( inst, label, location )
$)



AND z80cc( condition )  =  condition = cond_always  ->  cc_none,
                           condition = cond_carry   ->  cc_c,
                           condition = cond_nocarry ->  cc_nc,
                           condition = cond_eq      ->  cc_z,
                           condition = cond_ne      ->  cc_nz,
                           condition = cond_ls      ->  cc_m,
                           condition = cond_ge      ->  cc_p,  
                           
                           //  The following are not strictly correct, but
                           //  we should have them here, because certain
                           //  optimisations require them.
                           
                           condition = cond_gr      ->  cc_p,
                           condition = cond_le      ->  cc_m,
                           
                                                        cgerror( "z80cc( %N )", condition )

        
        
AND systemindex( t, v )  =  VALOF
$(
//  Return TRUE if this value can only be loaded by calling the monitor.

    TEST  (t = t_lv_local  |  t = t_lv_global)  THEN
          RESULTIS  TRUE

    ELSE

    TEST  (t = t_local  |  t = t_global)  &  outofrange( v )  THEN
          RESULTIS  TRUE

    ELSE  RESULTIS  FALSE
$)



AND outofrange( v )  =  NOT  (0 <= v <= 127)



AND loadindexvalue( t, v )  =  VALOF
$(
//  Generate code to call the monitor function to load the value given onto
//  the stack.  If the item being handled is "local" or "global", then
//  we must subtract off the number which are already addressable.

    IF  t = t_local  |  t = t_global  THEN  v  :=  v - 128

    monloadfunction( t )
    monargument( v )

    stackp  :=  stackp + 1

    RESULTIS  stackp-1
$)



AND highbyte( r )     =  r = r_hl  ->  r_h,
                         r = r_de  ->  r_d,
                         r = r_bc  ->  r_b,  cgerror( "highbyte( %N )", r )



AND lowbyte( r )   =  r = r_hl  ->  r_l,
                      r = r_de  ->  r_e,
                      r = r_bc  ->  r_c,  cgerror( "lowbyte( %N )", r )




AND movetor( r, t, v )  BE
$(
//  Generate code to move the value "t,v" into register "r".  What we do
//  depends very much on the value we need to move in.

    LET s  =  lookinslave( t, v )

    //  We must check to make sure that this register is not in fact
    //  cherished, and if it is, store its value away.

    UNLESS  notcherished( r )  DO
    $(
        LET seq  =  rchseq!r
        
        FOR  i = 1  TO  seq  DO
        $(
            LET rr  =  chseqr( i )
            
            UNLESS  rr = r_none  DO  storecherished( rr )
        $)
    $)

    //  Look to see if the item we wish to move is actually in a
    //  register, and if it is, save ourselves the trouble of doing any
    //  more.
    
    UNLESS  s = r_none  DO
    $(
        //  The value we want is in the slave.  All we need to do is to
        //  copy the information across.

        UNLESS  s = r  DO
        $(
            //  We must compile some code to move the value from register
            //  "s" to register "r".
            
            LET sh  =  highbyte( s )
            LET rh  =  highbyte( r )
            LET sl  =  lowbyte( s )
            LET rl  =  lowbyte( r )
            
            code_rr_1( i_ldrr, rl, sl )
            code_rr_1( i_ldrr, rh, sh )
            
            unsetslave( r )
            copyslave( s, r )
        $)
        
        RETURN
    $)

    //  Ok.  The value was not in the slave, and so we had better do
    //  something about this.

    SWITCHON  t  INTO
    $(
        CASE t_local      :
        CASE t_global     :  TEST  systemindex( t, v )  
                                 THEN  destackregister( r, loadindexvalue( t, v ) )
                                 ELSE  loadixiy( r, t, v )
                             ENDCASE


        CASE t_lv_local   :
        CASE t_lv_global  :  destackregister( r, loadindexvalue( t, v ) )
                             ENDCASE


        CASE t_label      :  loadstatic( r, v )
                             ENDCASE


        CASE t_lv_label   :  loadlvstatic( r, v )
                             ENDCASE

        CASE t_fnlab      :  loadfnlab( r, v )
                             ENDCASE


        CASE t_number     :  loadnumber( r, v )
                             ENDCASE


        CASE t_stack      :  destackregister( r, v )
                             ENDCASE
                             
                             
        CASE t_cherished  :  movetor( r, v!n_type, v!n_arg1 )
                             ENDCASE


        DEFAULT           :  cgerror( "movetor( %N, %N )", t, v )
    $)

    //  When we drop out of there, the item has been moved into the register,
    //  and so we should update the slave to reflect this.
    
    unsetslave( r )
    
    UNLESS  t = t_register  |  t = t_stack  DO  addslaveinfo( r, t, v )
$)



AND loadixiy( r, t, v )  BE
$(
//  Load the item at offset "v" on the stack or in the global vector into
//  register "r".

    LET esc     =  t = t_local  ->  esc_dd, esc_fd
    LET offset  =  (v * 2) - 128
    
    LET rh      =  highbyte( r )
    LET rl      =  lowbyte( r )
    
    code_ri_3( esc, i_ldri, rl, offset+0 )
    code_ri_3( esc, i_ldri, rh, offset+1 )
$)



AND loadnumber( r, n )  BE  code_rn_3( i_ldrnn, r, n )



AND loadstatic( r, l )  BE
$(
//  Generate code to load a label from a static location.  The size of the
//  instruction depends on whether the register is HL or not.

    LET labeladdr  =  lookuplabel( l )
    
    TEST  r = r_hl  THEN

        //  This is a 3 byte instruction, which should be used in preference
        //  to the more general 4 byte version.

        code_il_3( i_ldhll, l, labeladdr )

    ELSE

        //  This is unfortunately a 4 byte instruction, but what the hell - it's
        //  still shorter than loading from the stack.
        
        code_rl_4( esc_ed, i_ldrl, r, l, labeladdr )
$)



AND loadlvstatic( r, l )  BE
$(
//  Load the BCPL address of a static location into the register "r".  We can
//  load the machine address immediately, but we must shift it to the right
//  in order to get the BCPL address.

    LET labeladdr  =  lookuplabel( l )
    
    LET rh         =  highbyte( r )
    LET rl         =  lowbyte( r )

    code_rl_3( i_ldrll, r, l, labeladdr )

    //  Now, generate the code to shift the value right.
    
    code_r_2( esc_cb, i_srl, rh )
    code_r_2( esc_cb, i_rr, rl )
$)


AND loadfnlab( r, l )  BE
$(
//  Load the BCPL address of a static location into the register "r".  We can
//  load the machine address immediately, but we must shift it to the right
//  in order to get the BCPL address.

    LET labeladdr  =  lookuplabel( l )
    
    LET rh         =  highbyte( r )
    LET rl         =  lowbyte( r )

    code_rl_3( i_ldrll, r, l, labeladdr )
$)



AND destackregister( r, n )  BE
$(
//  Generate code to destack the item on the stack into a register.  We must
//  make sure that the item is actually on the top of the stack.

    UNLESS  n = stackp-1  DO  cgerror( "destackregister( %N )", n )

    code_ss_1( i_pop, r )

    destackitem()
$)

    

AND movetoanyr( t, v, readonly )  =  movetoanybutr( r_none, t, v, readonly )



AND movetoanybutr( r, t, v, readonly )  =  VALOF
$(
//  General routine to attempt to move the value "t,v" into any register
//  but "r".  The value "readonly" is a flag saying whether exclusive
//  access is required for this register.

    LET slaver  =  searchslave( r, t, v )
    LET freer   =  0

    //  If we have found this item in the register slave, and we only want
    //  the register "read only", then we can return the item without
    //  further ado.
    
    IF  slaver \= r_none  &  (readonly  |  notcherished( slaver ))  THEN  
    
        RESULTIS  slaver

    //  Otherwise, we must move the value into a register and return that
    //  register.  If there are free registers, then return a free register,
    //  otherwise we must make one free and then use it.  Otherwise, use the
    //  one that was least recently cherished.
    
    freer  :=  findfreer( r )

    IF  freer = r_none  THEN

        //  There are no free registers around, and so we must stack the 
        //  oldest cherished register.
        
        freer  :=  freecherished( r )

    //  We now have a free register into which we can put the value, so call
    //  the next level to do just that.
         
    movetor( freer, t, v )

    //  When we drop out of there, the value "freer" is the register in
    //  which the item is held.
    
    RESULTIS  freer
$)



AND lookinslave( t, v )  =  searchslave( r_none, t, v )



AND searchslave( r, t, v )  =  VALOF
$(
//  Search the register slave for the value "t,v", excluding register "r".
//  We return the name of the register containing the value we want, or
//  "r_none" if the item is not in the slave.

    IF  t = t_register  THEN
        UNLESS  v = r  DO
            RESULTIS  v

    FOR  rr = r_hl  TO  r_bc  DO
    
        //  First, check to see if this register is allowed, and if it is,
        //  search further to see if there is a relevant entry.
        
        UNLESS  rr = r  DO
        
            //  Search the list of items associated with this register in the
            //  slave, and if the item is in there, return it.

            IF  isinlist( rinfo!rr, t, v )  THEN  
            
                //  We can return the name of this register as the result of
                //  the procedure.

                RESULTIS  rr

    //  If we drop out of that loop without returning, then the value we want
    //  is not in the slave, and hence we should return nothing.
    
    RESULTIS  r_none
$)



AND copyslave( r1, r2 )  BE
$(
//  Copy the slave information from register r1 to register r2.

    unsetslave( r2 )

    rinfo!r2  :=  copyof( rinfo!r1 )
$)



AND copyof( item )  =  

    //  Return a copy of the list given.  The list will be short, so we can
    //  do the copy recursively.

    item = NIL  ->  NIL,   
                    block3( copyof( item!l_link ), item!l_type, item!l_value )



AND isinlist( list, t, v )  =

    //  Return a boolean saying whether there is an entry for "t,v" in the
    //  list "list".  The list will be short, so we can search it recursively.

    (list = NIL)                            ->  FALSE,
    (list!l_type = t  &  list!l_value = v)  ->  TRUE,
                                                isinlist( list!l_link, t, v )


/*****************************************************************************\
*                           Systems Research Group                            *
*******************************************************************************


                       #####    ######             ######## 
                      #######  ########            ######## 
                     ##        ##                  ##       
                     ##        ##  ####   #######  #######  
                     ##        ##    ##                  ## 
                     ##        ##    ##            ##    ## 
                      #######  ########             ######  
                       #####    ######               ####   


*******************************************************************************
*   I. D. Wilson           Last Modified   -   IDW   -   18/02/85             *
\*****************************************************************************/



///SECTION "CG-5"


LET cg_switchon( switchbuff, switchsize )  BE
$(
//  Generate code for the SWITCHON directive.  The cases come to us as a
//  series of case values with associated case labels.  We sort the values,
//  and then decide what sort of code to generate depending on the spread
//  of the cases.

    LET defaultl  =  rdl()
    
    LET cases     =  switchsize / 2
    LET caseval   =  switchbuff + 0
    LET caselab   =  switchbuff + cases

    //  Take a pointer to the node representing the value being switched
    //  on, and bring the stack down.
    
    LET snode     =  arg1!a_node
    LET type      =  0
    LET value     =  0
    
    cg_stack( ssp-1 )

    //  Now read the arguments to the SWITCHON directive, and sort them
    //  in the the "caseval" and "caselab" vectors.
    
    FOR  i = 0  TO  cases-1  DO
    $(
        //  For each case entry, read the case value and case label associated
        //  with it, sorting it into its correct position in the sort buffers.
        
        LET val  =  rdn()
        LET lab  =  rdl()
        LET pos  =  i
        
        UNTIL  pos = 0  DO
        $(
            //  Search backwards in the buffers to find the correct slot for
            //  this item.
            
            IF  val > caseval!(pos - 1)  THEN  BREAK
            
            caseval!pos  :=  caseval!(pos - 1)
            caselab!pos  :=  caselab!(pos - 1)
            
            pos          :=  pos - 1
        $)
        
        //  When we drop out of that loop, we have found the correct place
        //  to put this entry, so put it there and continue.
        
        caseval!pos  :=  val
        caselab!pos  :=  lab
    $)
    
    //  We have now sorted the case values and labels.  Prepare the stack,
    //  and evaluate the value being switched on.
    
    cg_store( 0, ssp )
    
    value  :=  evaluate( snode )
    type   :=  evaltype
    
    freenode( snode )
    
    //  We can now decide which form of code generation we wish to apply.  The
    //  methods are:
    //
    //      a)  If the number of cases is small, do a linear search 
    //          immediately.
    //
    //      b)  If the spread is not greater than 256, then we can compile
    //          a table search using "CPIR".
    //
    //      c)  If a look up table would be more than half full, then this
    //          method is used.
    //
    //      d)  If the look up table would be too expensive, then a binary
    //          search method is used instead.
    
    TEST  type = t_number  THEN
    $(
        //  What is the programmer playing at?  He is switching on
        //  a number.  We can compile this without any further ado.
                
        LET found  =  FALSE
                
        FOR  i = 0  TO  cases-1  DO
                
             //  Look at the value associated with this case, and see
             //  if it is equal to the number being switched on.

             IF  value = caseval!i  THEN
             $(
                 //  Generate an unconditional jump to the case label,
                 //  and stop looking.
                         
                 cg_jump( caselab!i )
                         
                 found  :=  TRUE
                         
                 BREAK
             $)

        //  At this point, we should look to see whether the item was
        //  found, and if not, compile an unconditional jump to the
        //  default label.
                
        UNLESS  found  DO  cg_jump( defaultl )
    $)
    ELSE

    TEST  cases < 10  THEN  
    $(
        //  Small enough number of cases for us to be able to do a linear
        //  search.  This assumes the value to be in DE.

        movetor( r_de, type, value )

        linearsearch( caseval, caselab, cases, defaultl )
    $)
    ELSE
    $(
        //  Too many cases to do a linear search, so chose one of the other
        //  methods.  Whichever method we use, we need unsigned comparisons,
        //  so turn the numbers into unsigned values.

        LET highest  =  caseval!(cases - 1)
        LET lowest   =  caseval!(0)
        LET spread   =  (highest/2) - (lowest/2)
        
        FOR  i = 0  TO  cases-1  DO  caseval!i  :=  caseval!i - lowest

        //  For the CPIR case to be the most efficient, the spread must be
        //  less than 256, and the case values must be 60% sparse or
        //  more.
        
        TEST  spread < 128  &  (cases*3 < spread*4)  THEN
        $(
            //  The spread is such that a CPIR instruction can be compiled.
            //  The case values have already had the base value subtracted
            //  from them.
            
            LET r     =  0
            LET rh    =  0
            LET rl    =  0
            
            LET lab1  =  newlabel()
            LET val1  =  lookuplabel( lab1 )
            LET lab2  =  newlabel()
            LET val2  =  lookuplabel( lab2 )

            //  Now, we can subtract the lowest value from the item we are
            //  switching on.
            
            value  :=  cg_plusminus( s_sub, type, value, t_number, lowest )
            type   :=  evaltype
            
            //  The previous subtraction could well have caused overflow, but
            //  this is not relevant, since if overflow occurred, then the
            //  top byte of the register cannot be zero.
            
            r      :=  movetoanyr( type, value, TRUE )
            rh     :=  highbyte( r )
            rl     :=  lowbyte( r )
                
            code_rr_1( i_ldrr, r_a, rh )
            code_s_1( i_or, r_a )
                
            //  If the high byte is not zero, then the value can't possibly
            //  be in range, so jump to the default label.
            
            cg_branch( cond_ne, defaultl )
                
            //  Otherwise, load the low byte into the A register, and
            //  then set up the registers for a CPIR.
                
            code_rr_1( i_ldrr, r_a, rl )
                
            code_rl_3( i_ldrll, r_hl, lab1, val1 )
            code_rn_3( i_ldrnn, r_bc, cases )
            code_i_2( esc_ed, i_cpir )
                
            //  After executing the CPIR, we should jump if the 'Z' flag
            //  is not set to the default label.
                
            cg_branch( cond_ne, defaultl )
                
            //  Otherwise, BC contains the offset into the table where
            //  the label to jump to can be found.
                
            code_rl_3( i_ldrll, r_hl, lab2, val2 )
            code_ss_1( i_addhl, r_bc )
            code_ss_1( i_addhl, r_bc )
            
            //  HL now points at a location in the table where the location
            //  to be jumped to can be found.
                
            jumpihl()
                
            //  We can now generate the look up tables as pieces of data.
            //  These will be flushed on exit from the current routine.
                
            cg_data( s_datalab, lab1 )
                
            FOR  i = 0  TO  cases-1  DO  cg_data( s_itemb, caseval!i )
            
            //  Now generate the look up table of the labels.  Since the table
            //  is effectively searched in reverse order, we must put the
            //  labels in reverse order!
                
            cg_data( s_datalab, lab2 )
                
            FOR  i = cases-1  TO  0  BY -1  DO  cg_data( s_iteml, caselab!i )
        $)
        ELSE
        $(
            //  Too large a spread to use CPIR, so use one of the other
            //  methods.  Look to see what the range is, and then decide
            //  what to do.  The value "spread" is actually the spread
            //  divided by 2.
            
            //  First, subtract off the lowest number, thus giving us
            //  an unsigned offset into the lookup table.
                
            value  :=  cg_plusminus( s_sub, type, value, t_number, lowest )
            type   :=  evaltype
                
            movetor( r_de, type, value )
                
            TEST  spread < cases  THEN
            $(
                //  This is ok.  A look up table, when generated, would be
                //  more than half full, so it is deemed to be reasonable.
                //  The arithmetic done is unsigned, twos complement.  We
                //  need not worry about overflow, because to be doing this
                //  type of switchon, the spread must be small.
                
                LET lab     =  newlabel()
                LET val     =  lookuplabel( lab )
                LET offset  =  highest - lowest + 1
                LET entry   =  0
                
                //  At this point, the offset is defined to be in DE, and
                //  at this point we take over generating instructions
                //  directly.
                
                code_rn_3( i_ldrnn, r_hl, -offset )
                code_ss_1( i_addhl, r_de )
                
                cg_branch( cond_carry, defaultl )

                //  We now believe that we are within range, and hence
                //  should generate code to load the label value, and then
                //  jump.

                code_rl_3( i_ldrll, r_hl, lab, val )
                code_ss_1( i_addhl, r_de )
                code_ss_1( i_addhl, r_de )
                
                //  HL now points to the location in the table where the
                //  label address will be found.  Jump to it!
                
                jumpihl()
                
                //  We can now generate the labels themselves.
                
                cg_data( s_datalab, lab )

                FOR  i = 0  TO  offset-1  DO
                
                    //  Generate the label if possible, but the default label
                    //  if not.
                    
                    TEST  caseval!entry = i  THEN
                    $(
                        //  This is the correct entry, so generate the labe;
                        //  and step the entry pointer on.
                        
                        cg_data( s_iteml, caselab!entry )
                        
                        entry  :=  entry + 1
                    $)
                    ELSE

                        //  This is a hole in the table, which we must fill
                        //  up with the default label.
                        
                        cg_data( s_iteml, defaultl )
            $)
            ELSE

                //  Oh dear.  We can't use CPIR, and we have too big a range
                //  to use a table lookup.  This means that we should do a
                //  binary chop method.  The value being switched on is
                //  already in DE.
                
                binarychop( caseval, caselab, cases, defaultl )
        $)    
    $)
    
    //  We have now done the switchon.  Whatever happened, we must flush the
    //  register slave, and stop coding.
    
    discardslave()
    stopcoding()
$)



AND binarychop( caseval, caselab, cases, defaultl )  BE
$(
//  First, we should split the cases into two halves, and then decide in
//  which half this particular item belongs.

    TEST  cases < 10  THEN  linearsearch( caseval, caselab, cases, defaultl )
    ELSE
    $(    
        LET midpoint   =  cases/2
        LET midvalue   =  caseval!midpoint
        LET midlabel   =  caselab!midpoint
        LET lesslabel  =  newlabel()
    
        movetor( r_hl, t_number, midvalue )
    
        code_s_1( i_or, r_a )
        code_ss_2( esc_ed, i_sbchl, r_de )
        
        cg_branch( cond_eq,      midlabel )
        cg_branch( cond_nocarry, lesslabel )
        
        //  If we drop though here, then the value is greater than the
        //  mid point, so binary chop that bit.
        
        binarychop( caseval+midpoint+1, caselab+midpoint+1, cases-midpoint-1, defaultl )
        
        //  We should now set the label which corresponds to the lower
        //  half of the table.
        
        cg_setlabel( s_lab, lesslabel )
        
        binarychop( caseval, caselab, midpoint, defaultl )
    $)
$)



AND linearsearch( caseval, caselab, cases, defaultl )  BE
$(
//  Compile a linear search of the case table given.  This is done by a
//  monitor function.

    callmonfunction( m_linsearch )
    monargument( cases )
    
    FOR  i = 0  TO  cases-1  DO
    $(
        LET value  =  caseval!i
        LET label  =  caselab!i

        code_n_2( value )
        code_l_2( label, lookuplabel( label ) )
    $)
    
    //  At the end of the table, we generate the address of the defaule label
    //  which we jump to if the search fails.
    
    code_l_2( defaultl, lookuplabel( defaultl ) )
$)



AND jumpihl()  BE
$(
//  Generate code to jump to the label contained in the location pointed to by
//  HL.

    code_rr_1( i_ldrr, r_a, r_ihl )
    code_ss_1( i_inc, r_hl )
    code_rr_1( i_ldrr, r_h, r_ihl )
    code_rr_1( i_ldrr, r_l, r_a )
    
    code_i_1( i_jpihl )
$)



AND storeregister( r, t, v )  BE
$(
//  Generate code to store the register "r" at the location given by "t,v".

    TEST  t = t_local  |  t = t_global  THEN  storeindex( r, t, v )  ELSE
    TEST  t = t_label                   THEN  storestatic( r, v )    ELSE

          cgerror( "storeregister( %N )", t )
$)



AND storeindex( r, t, v )  BE
$(
//  Store the register "r" in location "t,v".  We must look to see if this is
//  out of range of the index registers, and if it is, store it by calling
//  the monitor.

    TEST  systemindex( t, v )  THEN
    $(
        //  The location is out of range of the normal store instruction,
        //  so push the register onto the stack, and call the monitor
        //  function to do the job.
        
        code_ss_1( i_push, r )
        
        monstorefunction( t )
        monargument( v-128 )
    $)
    ELSE  storeixiy( r, t, v )
$)



AND storeixiy( r, t, v )  BE
$(
//  Store register "r" at offset "v" on the stack or in the global vector.

    LET esc     =  t = t_local  ->  esc_dd, esc_fd
    LET offset  =  (v * bytesperz80word) - 128
    
    LET rh      =  highbyte( r )
    LET rl      =  lowbyte( r )
    
    code_ir_3( esc, i_ldir, rl, offset+0 )
    code_ir_3( esc, i_ldir, rh, offset+1 )
$)



AND storestatic( r, l )  BE
$(
//  Generate code to store register "r" into static label "l".  The instruction
//  which we generate depends on whether the register is in fact HL.

    LET labeladdr  =  lookuplabel( l )

    TEST  r = r_hl  THEN

        code_li_3( i_ldlhl, l, labeladdr )

    ELSE

        code_lr_4( esc_ed, i_ldlr, r, l, labeladdr )
$)



AND addslaveinfo( r, t, v )  BE
$(
//  Add the information about the item "t,v" to the register slave entry
//  for register "r".  We only do this if "t,v" itself is not a register.

    UNLESS  t = t_register  DO
    $(
        LET node  =  block3( rinfo!r, t, v )
        
        rinfo!r  :=  node
    $)
$)



AND cg_unslavelocal()  BE
$(
//  Scan the register slave, and discard any slave entries which represent
//  items above the current stack high water mark.

    FOR  r = r_hl  TO  r_bc  DO
    $(
        LET list  =  scanlocals( rinfo!r )
        
        unsetslave( r )
        
        rinfo!r  :=  list
    $)
$)



AND cg_unslavestorage()  BE
$(
//  Scan the register slave, and discard any slave entries which represent
//  items in main storage.

    FOR  r = r_hl  TO  r_bc  DO
    $(
        LET list  =  scanstorage( rinfo!r )
        
        unsetslave( r )
        
        rinfo!r  :=  list
    $)
$)



AND scanlocals( list )  =  list = NIL  ->  NIL,  VALOF
$(
//  Make a copy of the list "list" omitting any items which are outside
//  the current stack frame.

    LET l  =  list!l_link
    LET t  =  list!l_type
    LET v  =  list!l_value

    TEST  t = t_local  &  v >= ssp  
        THEN  RESULTIS  scanlocals( l )
        ELSE  RESULTIS  block3( scanlocals( l ), t, v )
$)



AND scanstorage( list )  =  list = NIL  ->  NIL,  VALOF
$(
//  Make a copy of the list "list" omitting any items which are in storage.

    LET l  =  list!l_link
    LET t  =  list!l_type
    LET v  =  list!l_value

    TEST  t = t_local  |  t = t_global  |  t = t_label
        THEN  RESULTIS  scanlocals( l )
        ELSE  RESULTIS  block3( scanlocals( l ), t, v )
$)



AND unsetslave( r )  BE
$(
//  Clear all the slave information for register "r".  This means throwing
//  away all the information we had so carefully stored about it!  Ho, humm.

    LET list  =  rinfo!r
    
    UNTIL  list = NIL  DO
    $(
        LET node  =  list
        
        list  :=  list!l_link
        
        freeblock3( node )
    $)

    rinfo!r  :=  NIL
$)



AND cherish( r, node )  BE
$(
//  Mark the register "r" to say that it contains interesting information.
//  The "cherished" field of the register data structure should be set to
//  point to the node "node", so that should the register be required, the
//  upper levels can be notified of this fact.

    UNLESS  rchnode!r = NIL  DO  cgerror( "cherish( %N )", r )

    IF  debug & liststream  THEN  writel( ";  Cherish R%N", r )

    chseq      :=  chseq + 1

    rchnode!r  :=  node
    rchseq!r   :=  chseq
$)



AND uncherish( r )  BE
$(
//  Unset the "cherished" state of the register "r".

    IF  rchnode!r = NIL  THEN  cgerror( "uncherish( %N )", r )

    IF  debug & liststream  THEN  writel( ";  Uncherish R%N", r )

    rchnode!r  :=  NIL
    rchseq!r   :=  0
$)



AND stackcherished()  BE
$(
//  Stack all items which are cherished.  We must stack them in the order
//  in which they were cherished.

    FOR  i = 1  TO  chseq  DO
    $(
        LET r  =  chseqr( i )
        
        //  If the value "r" is "r_none", then this cherished slot has been
        //  used, and discarded.  Ignore it.
        
        UNLESS  r = r_none  DO  storecherished( r )
    $)

    //  At this point, all the cherished registers have been stacked, and
    //  so we can reset the "cherish sequence".
    
    chseq  :=  0
$)



AND chseqr( seq )  =  VALOF
$(
//  Return the identity of the register which has "cherish sequence" "seq".

    FOR  r = r_hl  TO  r_bc  DO
         IF  rchseq!r = seq  THEN
             RESULTIS  r

    //  Otherwise, this slot is now unused, and we can return an error value.

    RESULTIS  r_none
$)
            


AND findfreer( r )  =  VALOF
$(
//  Return a pointer to a free register.  We return the first one which is
//  uncherished and undefined, and if that fails, return the first one which
//  is uncherished.

    LET notc  =  VEC r_bc
    LET notr  =  VEC r_bc
    LET notd  =  VEC r_bc

    FOR  rr = r_hl  TO  r_bc  DO
    $(
        //  Find out all we can about the registers.  The three important
        //  pieces of information are:
        //
        //      a)  Is the register cherished?
        //      b)  Is the register referenced?
        //      c)  Is the register defined?
        
        notc!rr  :=  notcherished( rr )
        notr!rr  :=  notreferenced( rr )
        notd!rr  :=  notdefined( rr )
    $)

    FOR  rr = r_hl  TO  r_bc  DO
         UNLESS  rr = r  DO
             IF  notc!rr  &  notr!rr  &  notd!rr  THEN
                 RESULTIS  rr

    FOR  rr = r_hl  TO  r_bc  DO
         UNLESS  rr = r  DO
             IF  notc!rr  &  notr!rr  THEN
                 RESULTIS  rr

    FOR  rr = r_hl  TO  r_bc  DO
         UNLESS  rr = r  DO
             IF  notc!rr  THEN
                 RESULTIS  rr

    RESULTIS  r_none
$)



AND freecherished( r )  =  VALOF
$(
//  Return a pointer to a register (previously cherished) which has been
//  successfully freed.

    FOR  i = 1  TO  chseq  DO
    $(
        LET rr  =  chseqr( i )
        
        UNLESS  rr = r_none  DO
        $(
            storecherished( rr )

            //  Unless this register has been explicitly precluded, we should
            //  return it immediately.
        
            UNLESS  rr = r  DO  RESULTIS  rr
        $)
    $)

    //  We should never drop out of that loop, but if we do, print out an
    //  error message.
    
    cgerror( "freecherished( %N )", r )
$)



AND storecherished( r )  BE
$(
//  We need the register "r", and it has been cherished.  We should therefore
//  put it onto the stack, and mark the cherished node as now containing a
//  stacked item.

    LET node  =  rchnode!r
    
    IF  node = NIL  THEN  cgerror( "storecherished( %N )", r )

    code_ss_1( i_push, r )
    
    node!n_type  :=  t_stack
    node!n_arg1  :=  stackp

    stackp       :=  stackp + 1

    uncherish( r )
$)



AND notcherished( r )  =  (rchnode!r = NIL)



AND notdefined( r )    =  (rinfo!r = NIL)



AND notreferenced( r )  =  VALOF
$(
//  Scan the simulated stack, returning a boolean to say whether this register
//  is referenced or not.

    LET arg  =  arg1

    UNTIL  arg = NIL  DO
    $(
        LET node   =  arg!a_node
        
        IF  isreferenced( node, r )  THEN  RESULTIS  FALSE
        
        arg  :=  arg!a_link
    $)

    RESULTIS  TRUE
$)



AND isreferenced( node, r )  =  VALOF
$(
//  Scan the data structure associated with the node given, and search for
//  references to the register "r".

    LET type  =  node!n_type

    SWITCHON  type  INTO
    $(
        CASE t_register   :  //  This is a register node, and so we should
                             //  set it up as being cherished.

                             RESULTIS  node!n_arg1 = r


        CASE t_local      :  CASE t_global     :
        CASE t_label      :  CASE t_number     :
        CASE t_lv_local   :  CASE t_lv_global  :
        CASE t_lv_label   :  CASE t_stack      :  
        CASE t_cherished  :  
        CASE t_fnlab      :
                             ENDCASE


                             RESULTIS  FALSE


        CASE s_add        :  CASE s_sub        :
        CASE s_logand     :  CASE s_logor      :
        CASE s_eqv        :  CASE s_xor        :
        CASE s_lshift     :  CASE s_rshift     :
        CASE s_mul        :  CASE s_div        :
        CASE s_mod        :  CASE s_eq         :
        CASE s_ne         :  CASE s_ls         :
        CASE s_gr         :  CASE s_le         :
        CASE s_ge         :  CASE s_getbyte    :

                             RESULTIS  isreferenced( node!n_arg1, r )  |
                                       isreferenced( node!n_arg2, r )


        CASE s_neg        :  CASE s_not        :
        CASE s_abs        :  CASE s_rv         :

                             RESULTIS  isreferenced( node!n_arg1, r )


        DEFAULT           :  cgerror( "isreferenced( %N )", type )
    $)
$)



AND monfunction( op, alt )  BE
$(
//  Generate a monitor call for the monitor function represented by "op".
//  If the "alt" flag is set, then use the alternative form of the function.

    LET monf  =  op = s_mul     ->  m_multiply,
                 op = s_div     ->  m_divide,
                 op = s_mod     ->  m_rem,
                 op = s_lshift  ->  m_lshift,
                 op = s_rshift  ->  m_rshift,
                 op = s_eq      ->  m_eq,
                 op = s_ne      ->  m_ne,
                 op = s_ls      ->  m_ls,
                 op = s_gr      ->  m_gr,
                 op = s_le      ->  m_le,
                 op = s_ge      ->  m_ge,
                 op = s_abs     ->  m_abs,
                                    cgerror( "monfunction( %N )", op )

    //  Now generate the call to the monitor, using the alternative form
    //  of the function if necessary.
    
    callmonfunction( alt  ->  alternative( monf ), monf )
$)



AND monloadfunction( type )  BE
$(
//  Use the monitor to load an out of range value onto the stack.  The
//  value "type" is the type of item to be loaded.

    LET monf  =  type = t_local      ->  m_loadix,
                 type = t_global     ->  m_loadiy,
                 type = t_lv_local   ->  m_loadlvix,
                 type = t_lv_global  ->  m_loadlviy,

                               cgerror( "monloadfunction( %N )", type )

    //  Look to see if the "short" flag is set, and if so, get the alternative
    //  form of the function.
    
    callmonfunction( monf )
$)



AND monstorefunction( type )  BE
$(
//  Generate a monitor store instruction.

    LET monf  =  type = t_local   ->  m_storeix,
                 type = t_global  ->  m_storeiy,
                                      cgerror( "monstorefunction( %N )", type )

    //  Now call the function, using the alternative form if necessary.
    
    callmonfunction( monf )
$)



AND monargument( value )  BE
$(
//  Define an argument to the monitor.  The size of the item we store 
//  depends on its value.  It is assumed that "value" is positive.

    LET low7   =  (value)       &  #B0000000001111111
    LET high8  =  (value << 1)  &  #B1111111100000000

    TEST  high8 = 0  THEN  code_n_1( low7 )
    ELSE
    $(
        code_n_1( low7 + #B10000000 )
        code_n_1( high8 >> 8 )
    $)
$)



AND callmonfunction( monf )  BE
$(
//  Generate code to call a monitor function.  This means generating an
//  RST instruction.  Which one we generate depends on the type of
//  monitor function.

    LET inst  =  rstinsts!0
    LET def   =  TRUE

    FOR  i = 1  TO  rstcount  DO // rstcount is currently zero.
    $(
        //  Look at the relevant entry in the restart table, and decide whether
        //  this is a direct restart.  Otherwise, make it a default one with
        //  argument.

        IF  monf = rstfuncs!i  THEN
        $(
            //  This matches, so set the instructions up.
            
            inst  :=  rstinsts!i
            def   :=  FALSE
        $)
    $)

    code_im_1( inst, (def -> m_default, monf) )

    IF  def  THEN  code_m_1( monf )
$)



AND alternative( monf )  =  monf = m_multiply  ->  m_multiply,
                            monf = m_divide    ->  m_rdivide,
                            monf = m_rem       ->  m_rrem,
                            monf = m_lshift    ->  m_rlshift,
                            monf = m_rshift    ->  m_rrshift,
                            monf = m_eq        ->  m_eq,
                            monf = m_ne        ->  m_ne,
                            monf = m_ls        ->  m_gr,
                            monf = m_gr        ->  m_ls,
                            monf = m_le        ->  m_ge,
                            monf = m_ge        ->  m_le,
                            monf = m_abs       ->  m_abs,
                                                   cgerror( "alternative( %N )", monf )



AND discardslave()  BE
$(
    //  Ignore the slaving information in the register slave.
    
    FOR  r = r_hl  TO  r_bc  DO  unsetslave( r )

    chseq  :=  0
$)



AND addname( name, label )  BE
$(
//  Add the name of a BCPL routine to the name data structure.

    LET node  =  getstore( nl_size )
    LET ptr   =  @namelist

    node!nl_link   :=  NIL
    node!nl_name   :=  name
    node!nl_addr   :=  currentloc
    node!nl_type   :=  t_local
    node!nl_value  :=  label

    UNTIL  ptr!nl_link = NIL  DO  ptr  :=  ptr!nl_link

    ptr!nl_link    :=  node
$)



AND bcplalign()  BE

    //  Align the code onto a BCPL word boundary.
    
    IF  odd( currentloc )  THEN  code_i_1( i_nop )



AND odd( value )  =  (value & 1)  \=  0



AND setlabelrefs( label )  BE
$(
    //  Update the label references for label "label".  This means scanning
    //  down the list of label references updating the buffered code for
    //  each label reference.  Since the list may be long, we should not
    //  use recursion.

    LET refs  =  labelrefs!label
    LET addr  =  labeladdr!label

    UNTIL  refs = NIL  DO
    $(
        setlabelref( refs!r_addr, addr )
        
        refs  :=  refs!r_link
    $)
$)



AND setlabelref( location, value )  BE
$(
    //  Set a reference to a label with value "value" at location "location".

    update( location+0, (value)      & #XFF )
    update( location+1, (value >> 8) & #XFF )
$)



AND lookuplabel( label )  =  VALOF
$(
//  Look up the value of label "label".

    UNLESS  0 <= label <= maxlabel  DO  cgerror( "lookuplabel( %N )", label )

    RESULTIS  labeladdr!label
$)



AND startcoding()  BE

//  Start coding, so long as we are within a procedure.

    incode  :=  (procdepth > 0)



AND stopcoding()  BE
$(
//  Stop generating code.  We should store all the items onto the stack,
//  and flush the register slave.

    discardslave()

    incode  :=  FALSE
$)



AND cg_constant( type, value )  BE
$(
//  Code generate a constant.  What we do depends on the type of the constant.
//  We must always generate code at this point.

    LET oldic  =  incode

    incode  :=  TRUE

    SWITCHON  type  INTO
    $(
        CASE s_itemb    :  code_n_1( value )
                           ENDCASE

        CASE s_itemn    :  code_n_2( value )
                           ENDCASE

        CASE s_iteml    :  code_l_2( value, lookuplabel( value ) )
                           ENDCASE

        CASE s_datalab  :  bcplalign()
                           setlabel( value )
                           setlabelrefs( value )
                           ENDCASE


        DEFAULT         :  cgerror( "cg_constant( %N )", type )
    $)

    incode  :=  oldic
$)



AND newlabel()  =  VALOF
$(
//  Return the value of a new code generator generated label.  We must check
//  that the one we are about to generate is not in the range generated by
//  the translator.

    cglabel  :=  cglabel - 1

    UNLESS  maxln < cglabel < maxlabel  DO  cgerror( "newlabel( %N )", cglabel )

    RESULTIS  cglabel
$)



AND rdop()  =  rdn()



AND rdl()  =  VALOF
$(
//  Read a new label number from the input stream.

    LET l  =  rdn()

    IF  l > maxln  THEN  maxln  :=  l

    RESULTIS  l
$)



AND rdg()  =  VALOF
$(
//  Read a new global number from the input stream.

    LET g  =  rdn()

    IF  g > maxgn  THEN  maxgn  :=  g

    RESULTIS  g
$)



// AND rdn()  =  readn()      rdn is defined in the frontend and reads Ocode values
//                            from a buffer in memory.



AND cgerror( format, arg1, arg2, arg3 )  BE
$(
//  Print out an error message, and stop.

    cgwarning( format, arg1, arg2, arg3 )

    abort( 9999 )

    printdebuginfo()

    IF tostream   DO { endstream( tostream );   tostream   := 0 }
    IF liststream DO { endstream( liststream ); liststream := 0 }
    IF mapstream  DO { endstream( mapstream );  mapstream  := 0 }

    cg_uninitstore()

    selectoutput( sysout )

    writes( "*NCode Generation Abandoned.*N" )

    stop( 20 )
$)



AND cgwarning( format, arg1, arg2, arg3 )  BE
$(
//  Print out an error message.

    LET o  =  output()

    selectoutput( sysout )

    writes( "*N****** Z80CG:  " )
    writef( format, arg1, arg2, arg3 )
    newline()

    selectoutput( o )
$)


/*****************************************************************************\
*                           Systems Research Group                            *
*******************************************************************************


                       #####    ######              ######  
                      #######  ########            ######## 
                     ##        ##                  ##       
                     ##        ##  ####   #######  #######  
                     ##        ##    ##            ##    ## 
                     ##        ##    ##            ##    ## 
                      #######  ########             ######  
                       #####    ######               ####   


*******************************************************************************
*   I. D. Wilson           Last Modified   -   IDW   -   21/02/85             *
\*****************************************************************************/



///SECTION "CG-6"


LET code_ss_1( inst, r )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( mnemonic( inst ), regname( r ) )

    zbyte( inst  |  (regbits( r ) << 4) )
$)



AND code_ss_2( esc, inst, r )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( mnemonic( inst ), regname( r ) )

    zbyte( esc )
    zbyte( inst  |  (regbits( r ) << 4) )
$)



AND code_s_1( inst, r )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( mnemonic( inst ), regname( r ) )

    zbyte( inst  |  regbits( r ) )
$)



AND code_rr_1( inst, r1, r2 )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( mnemonic( inst ), regname( r1 ), regname( r2 ) )

    zbyte( inst  |  (regbits( r1 ) << 3)  |  regbits( r2 ) )
$)



AND code_rn_2( inst, r, n )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( mnemonic( inst ), regname( r ), n )

    zbyte( inst  |  (regbits( r ) << 3) )
    zbyte( n )
$)



AND code_rn_3( inst, r, nn )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( mnemonic( inst ), regname( r ), nn )

    zbyte( inst  |  (regbits( r ) << 4) )
    zbyte( byte0( nn ) )
    zbyte( byte1( nn ) )
$)



AND code_i_1( inst )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( mnemonic( inst ) )

    zbyte( inst )
$)



AND code_i_2( esc, inst )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( mnemonic( inst ) )

    zbyte( esc )
    zbyte( inst )
$)



AND code_im_1( inst, monf )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( mnemonic( inst ), monfname( monf ) )

    zbyte( inst )
$)



AND code_m_1( monf )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( "    DEFB  #X%X2  ; %S", monf, monfname( monf ) )

    zbyte( monf )
$)



AND code_n_1( n )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( "    DEFB  #X%X2", n )

    zbyte( n )
$)



AND code_n_2( nn )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( "    DEFW  #X%X4", nn )

    zbyte( byte0( nn ) )
    zbyte( byte1( nn ) )
$)



AND code_l_2( l, nn )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( "    DEFW  L%N", l )

    addlabelref( l )

    zbyte( byte0( nn ) )
    zbyte( byte1( nn ) )
$)



AND code_il_2( inst, l, n )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( mnemonic( inst ), l )

    zbyte( inst )
    zbyte( n )
$)



AND code_r_2( esc, inst, r )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( mnemonic( inst ), regname( r ) )

    zbyte( esc )
    zbyte( inst  |  regbits( r ) )
$)



AND code_il_3( inst, l, nn )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( mnemonic( inst ), l )

    zbyte( inst )

    addlabelref( l )

    zbyte( byte0( nn ) )
    zbyte( byte1( nn ) )
$)



AND code_ir_3( esc, inst, r, offset )  BE  IF  incode  THEN
$(
    LET sign  =  "+"
    LET offs  =  offset
    
    IF  offset < 0  THEN
    $(
        offs  :=  -offs
        sign  :=  "-"
    $)
    
    IF  liststream  THEN  writel( mnemonic( inst ), escname( esc ), sign, offs, regname( r ) )

    zbyte( esc )
    zbyte( inst  |  regbits( r ) )
    zbyte( offset )
$)



AND code_ri_3( esc, inst, r, offset )  BE  IF  incode  THEN
$(
    LET sign  =  "+"
    LET offs  =  offset
    
    IF  offset < 0  THEN
    $(
        offs  :=  -offs
        sign  :=  "-"
    $)
    
    IF  liststream  THEN  writel( mnemonic( inst ), regname( r ), escname( esc ), sign, offs )

    zbyte( esc )
    zbyte( inst  |  (regbits( r ) << 3) )
    zbyte( offset )
$)



AND code_rl_3( inst, r, l, nn )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( mnemonic( inst ), regname( r ), l )

    zbyte( inst  |  (regbits( r ) << 4) )
    
    addlabelref( l )

    zbyte( byte0( nn ) )
    zbyte( byte1( nn ) )
$)



AND code_li_3( inst, l, nn )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( mnemonic( inst ), l )

    zbyte( inst )

    addlabelref( l )

    zbyte( byte0( nn ) )
    zbyte( byte1( nn ) )
$)



AND code_rl_4( esc, inst, r, l, nn )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( mnemonic( inst ), regname( r ), l )

    zbyte( esc )
    zbyte( inst  |  (regbits( r ) << 4) )
    
    addlabelref( l )

    zbyte( byte0( nn ) )
    zbyte( byte1( nn ) )
$)



AND code_lr_4( esc, inst, r, l, nn )  BE  IF  incode  THEN
$(
    IF  liststream  THEN  writel( mnemonic( inst ), l, regname( r ) )

    zbyte( esc )
    zbyte( inst  |  (regbits( r ) << 4) )

    addlabelref( l )

    zbyte( byte0( nn ) )
    zbyte( byte1( nn ) )
$)



AND setlabel( label )  BE
$(
    //  Set the label entry for this label to the current location value.

    UNLESS  0 <= label <=  maxlabel  DO  cgerror( "setlabel( %N )", label )
    UNLESS  labeladdr!label = NIL    DO  cgerror( "setlabel( %N )", label )

    labeladdr!label  :=  currentloc

    IF  liststream  THEN  writel( "L%N:", label )

    setlabelrefs( label )
$)



AND writel( format, arg1, arg2, arg3, arg4 )  BE
$(
//  Print out listing information to the list stream.

    LET o  =  output()
        
    selectoutput( liststream )

    writef( format, arg1, arg2, arg3, arg4 )
    newline()
        
    selectoutput( o )
$)



AND regbits( r )  =  (TABLE   #B10,       //  HL
                              #B01,       //  DE
                              #B00,       //  BC
                             #B111,       //  A
                             #B000,       //  B
                             #B001,       //  C
                             #B010,       //  D
                             #B011,       //  E
                             #B100,       //  H
                             #B101,       //  L
                             #B110)!r     //  (HL)



AND zbyte( byte )  BE
$(
    update( currentloc, byte )

    currentloc  :=  currentloc + 1
$)



AND byte0( word )  =  word  &  #XFF



AND byte1( word )  =  (word >> 8)  &  #XFF



AND update( location, byte )  BE
$(
    LET address  =  location - programbase
    LET chunk    =  address  /  bytesperchunk
    LET offset   =  address REM bytesperchunk

    TEST  chunk < maxchunks  THEN
    $(
        LET buffer  =  memchunks!chunk
        
        IF  buffer = NIL  THEN
        $(
            buffer           :=  getstore( wordsperchunk )
            memchunks!chunk  :=  buffer
        $)
        
        buffer % offset  :=  byte
    $)
    ELSE  cgerror( "Program too large" )
$)



AND regname( r )  =  VALOF
$(
    SWITCHON  r  INTO
    $(
        CASE r_hl   :  RESULTIS  "HL"
        CASE r_de   :  RESULTIS  "DE"
        CASE r_bc   :  RESULTIS  "BC"
        CASE r_a    :  RESULTIS  "A"
        CASE r_b    :  RESULTIS  "B"
        CASE r_c    :  RESULTIS  "C"
        CASE r_d    :  RESULTIS  "D"
        CASE r_e    :  RESULTIS  "E"
        CASE r_h    :  RESULTIS  "H"
        CASE r_l    :  RESULTIS  "L"
        CASE r_ihl  :  RESULTIS  "(HL)"

        DEFAULT     :  cgerror( "regname( %N )", r )
    $)
$)



AND escname( esc )  =  VALOF
$(
    SWITCHON  esc  INTO
    $(
        CASE esc_dd  :  RESULTIS  "IX"
        CASE esc_fd  :  RESULTIS  "IY"
        
        DEFAULT      :  cgerror( "escname( %N )", esc )
    $)
$)



AND monfname( monf )  =  VALOF
$(
    SWITCHON  monf  INTO
    $(
        CASE m_apply      :  RESULTIS  "M_APPLY"
        CASE m_fixup      :  RESULTIS  "M_FIXUP"
        CASE m_loadix     :  RESULTIS  "M_LOADIX"
        CASE m_loadiy     :  RESULTIS  "M_LOADIY"
        CASE m_storeix    :  RESULTIS  "M_STOREIX"
        CASE m_storeiy    :  RESULTIS  "M_STOREIY"  
        CASE m_setlink0   :  RESULTIS  "M_SETLINK0"
        CASE m_setlink1   :  RESULTIS  "M_SETLINK1"
        CASE m_setlink2   :  RESULTIS  "M_SETLINK2"
        CASE m_setlink3   :  RESULTIS  "M_SETLINK3"
        CASE m_return     :  RESULTIS  "M_RETURN"
        CASE m_finish     :  RESULTIS  "M_FINISH"
        CASE m_loadlvix   :  RESULTIS  "M_LOADLVIX"
        CASE m_loadlviy   :  RESULTIS  "M_LOADLVIY"
        CASE m_multiply   :  RESULTIS  "M_MULTIPLY"
        CASE m_divide     :  RESULTIS  "M_DIVIDE"
        CASE m_rem        :  RESULTIS  "M_REM"
        CASE m_lshift     :  RESULTIS  "M_LSHIFT"
        CASE m_rshift     :  RESULTIS  "M_RSHIFT"
        CASE m_eq         :  RESULTIS  "M_EQ"
        CASE m_ne         :  RESULTIS  "M_NE"
        CASE m_ls         :  RESULTIS  "M_LS"
        CASE m_gr         :  RESULTIS  "M_GR"
        CASE m_le         :  RESULTIS  "M_LE"
        CASE m_ge         :  RESULTIS  "M_GE"
        CASE m_rdivide    :  RESULTIS  "M_RDIVIDE"
        CASE m_rrem       :  RESULTIS  "M_RREM"
        CASE m_rlshift    :  RESULTIS  "M_RLSHIFT"
        CASE m_rrshift    :  RESULTIS  "M_RRSHIFT"
        CASE m_abs        :  RESULTIS  "M_ABS"
        CASE m_linsearch  :  RESULTIS  "M_LINSEARCH"

        CASE m_default    :  RESULTIS  "M_DEFAULT"

        DEFAULT           :  cgerror( "monfname( %N )", monf )
    $)
$)



AND mnemonic( inst )  =  VALOF
$(
    SWITCHON  inst  INTO
    $(
        CASE i_inc    :  RESULTIS  "    INC   %S"
        CASE i_dec    :  RESULTIS  "    DEC   %S"
        CASE i_and    :  RESULTIS  "    AND   %S"
        CASE i_or     :  RESULTIS  "    OR    %S"
        CASE i_xor    :  RESULTIS  "    XOR   %S"
        CASE i_sbchl  :  RESULTIS  "    SBC   HL,%S"
        CASE i_addhl  :  RESULTIS  "    ADD   HL,%S"
        CASE i_ldrr   :  RESULTIS  "    LD    %S,%S"
        CASE i_cpl    :  RESULTIS  "    CPL"
        CASE i_ldrn   :  RESULTIS  "    LD    %S,#X%X2"
        CASE i_ldrnn  :  RESULTIS  "    LD    %S,#X%X4"
        CASE i_ldrll  :  RESULTIS  "    LD    %S,L%N"
        CASE i_push   :  RESULTIS  "    PUSH  %S"
        CASE i_pop    :  RESULTIS  "    POP   %S"
        CASE i_ret    :  RESULTIS  "    RET"
        CASE i_jpihl  :  RESULTIS  "    JP    (HL)"
        CASE i_exx    :  RESULTIS  "    EXX"
        CASE i_jr     :  RESULTIS  "    JR    L%N-$"
        CASE i_jrz    :  RESULTIS  "    JR    Z,L%N-$"
        CASE i_jrnz   :  RESULTIS  "    JR    NZ,L%N-$"
        CASE i_jrc    :  RESULTIS  "    JR    C,L%N-$"
        CASE i_jrnc   :  RESULTIS  "    JR    NC,L%N-$"
        CASE i_jp     :  RESULTIS  "    JP    L%N"
        CASE i_jpz    :  RESULTIS  "    JP    Z,L%N"
        CASE i_jpnz   :  RESULTIS  "    JP    NZ,L%N"
        CASE i_jpc    :  RESULTIS  "    JP    C,L%N"
        CASE i_jpnc   :  RESULTIS  "    JP    NC,L%N"
        CASE i_jpp    :  RESULTIS  "    JP    P,L%N"
        CASE i_jpm    :  RESULTIS  "    JP    M,L%N"
        CASE i_ldri   :  RESULTIS  "    LD    %S,(%S%S%N)"
        CASE i_ldir   :  RESULTIS  "    LD    (%S%S%N),%S"
        CASE i_ldhll  :  RESULTIS  "    LD    HL,(L%N)"
        CASE i_ldrl   :  RESULTIS  "    LD    %S,(L%N)"
        CASE i_srl    :  RESULTIS  "    SRL   %S"
        CASE i_rr     :  RESULTIS  "    RR    %S"
        CASE i_ldlhl  :  RESULTIS  "    LD    (L%N),HL"
        CASE i_ldlr   :  RESULTIS  "    LD    (L%N),%S"
        CASE i_nop    :  RESULTIS  "    NOP"
        CASE i_cpir   :  RESULTIS  "    CPIR"

        CASE i_rst00  :  RESULTIS  "    RST   #X00  ; %S"
        CASE i_rst08  :  RESULTIS  "    RST   #X08  ; %S"
        CASE i_rst10  :  RESULTIS  "    RST   #X10  ; %S"
        CASE i_rst18  :  RESULTIS  "    RST   #X18  ; %S"
        CASE i_rst20  :  RESULTIS  "    RST   #X20  ; %S"
        CASE i_rst28  :  RESULTIS  "    RST   #X28  ; %S"
        CASE i_rst30  :  RESULTIS  "    RST   #X30  ; %S"
        //CASE i_rst38  :  RESULTIS  "    RST   #X38  ; %S"
        CASE i_rst38  :  RESULTIS  "    RST   #X38      " // MR 20/08/2022

        DEFAULT       :  cgerror( "mnemonic( %N )", inst )
    $)
$)



AND addlabelref( label )  BE
$(
//  Add a reference to label "label" for the current location.

    UNLESS  0 <= label <= maxlabel  DO  cgerror( "addlabelref( %N )", label )

    labelrefs!label  :=  block2( labelrefs!label, currentloc )
$)



AND printdebuginfo()  BE
$(
//  Print out as much information as possible about the state of the current
//  world.

    LET o  =  output()

    selectoutput( sysout )

    writef( "*NDebug at #X%X4*N", currentloc )

    writes( "*NSimulated stack:*N" )
    printstack( arg1 )

    writes( "*NRegister slave:*N" )
    FOR  r = r_hl  TO  r_bc  DO  printregister( r )

    newline()

    selectoutput( o )
$)



AND printstack( arg )  BE
$(
//  Print out the stack information for the current stack node.

    UNTIL  arg = NIL  DO
    $(
        writes( arg = arg1  ->  "Arg1  ",
                arg = arg2  ->  "Arg2  ",
                                "      " )

        writef( "%I4  ", arg!a_ssp )
        printnode( arg!a_node )
        newline()

        arg  :=  arg!a_link
    $)
$)



AND printregister( r )  BE
$(
//  Print information about the register "r".

    LET info  =  rinfo!r
    LET chs   =  rchseq!r
    LET chn   =  rchnode!r
    
    writef( "%S  ", regname( r ) )
    
    UNTIL  info = NIL  DO
    $(
        //  Now write out the list of slave information associated with
        //  this register.
        
        LET t  =  info!l_type
        LET v  =  info!l_value
        
        writes( "  " )
        printtv( t, v )
        
        info  :=  info!l_link
    $)
    
    //  If the register is cherished, then we should print out what it
    //  is cherished as.
    
    UNLESS  chs = 0  DO  writef( "  ChSeq=%N", chs )

    UNLESS  chn = NIL  DO
    $(
        writes( "  ChNode=" )
        printnode( chn )
    $)

    newline()
$)



AND printnode( node )  BE
$(
//  Print the information contained in this node.

    LET type  =  node!n_type

    SWITCHON  type  INTO
    $(
        CASE t_cherished  :  wrch( '[' )
                             printnode( node!n_arg1 )
                             wrch( ']' )
                             ENDCASE


        CASE t_local      :  CASE t_global     :
        CASE t_label      :  CASE t_number     :
        CASE t_lv_local   :  CASE t_lv_global  :
        CASE t_lv_label   :  CASE t_register   :
        CASE t_stack      :
        CASE t_fnlab      :

                             printtv( type, node!n_arg1 )
                             writef( ":%N", node!n_arg2 )
                             ENDCASE


        CASE s_add        :  CASE s_sub        :
        CASE s_logand     :  CASE s_logor      :
        CASE s_eqv        :  CASE s_xor        :
        CASE s_lshift     :  CASE s_rshift     :
        CASE s_mul        :  CASE s_div        :
        CASE s_mod        :  CASE s_eq         :
        CASE s_ne         :  CASE s_ls         :
        CASE s_gr         :  CASE s_le         :
        CASE s_ge         :  CASE s_getbyte    :

                             wrch( '(' )
                             printnode( node!n_arg1 )
                             writes( opstr( type ) )
                             printnode( node!n_arg2 )
                             wrch( ')' )
                             ENDCASE


        CASE s_neg        :  CASE s_not        :
        CASE s_abs        :  CASE s_rv         :

                             writes( opstr( type ) )
                             wrch( '(' )
                             printnode( node!n_arg1 )
                             wrch( ')' )
                             ENDCASE


        DEFAULT           :  cgerror( "printnode( %N )", type )
    $)
$)



AND printtv( type, value )  BE
$(
    SWITCHON  type  INTO
    $(
        CASE t_local      :  writef( "P%N", value )     ;  ENDCASE
        CASE t_global     :  writef( "G%N", value )     ;  ENDCASE
        CASE t_label      :  writef( "L%N", value )     ;  ENDCASE
        CASE t_number     :  writef( "%N", value )      ;  ENDCASE
        CASE t_lv_local   :  writef( "LP%N", value )    ;  ENDCASE
        CASE t_lv_global  :  writef( "LG%N", value )    ;  ENDCASE
        CASE t_lv_label   :  writef( "LL%N", value )    ;  ENDCASE
        CASE t_fnlab      :  writef( "FL%N", value )    ;  ENDCASE
        CASE t_register   :  writes( regname( value ) ) ;  ENDCASE
        CASE t_stack      :  writef( "S%N", value )     ;  ENDCASE

        DEFAULT           :  cgerror( "printtv( %N )", type )
    $)
$)



AND opstr( op )  =  VALOF
$(
    SWITCHON  op  INTO
    $(
        CASE s_add        :  RESULTIS  " + "
        CASE s_sub        :  RESULTIS  " - "
        CASE s_logand     :  RESULTIS  " & "
        CASE s_logor      :  RESULTIS  " | "
        CASE s_eqv        :  RESULTIS  " EQV "
        CASE s_xor        :  RESULTIS  " XOR "
        CASE s_lshift     :  RESULTIS  " << "
        CASE s_rshift     :  RESULTIS  " >> "
        CASE s_mul        :  RESULTIS  " ** "
        CASE s_div        :  RESULTIS  " / "
        CASE s_mod        :  RESULTIS  " REM "
        CASE s_eq         :  RESULTIS  " = "
        CASE s_ne         :  RESULTIS  " \= "
        CASE s_ls         :  RESULTIS  " < "
        CASE s_gr         :  RESULTIS  " > "
        CASE s_le         :  RESULTIS  " <= "
        CASE s_ge         :  RESULTIS  " >= "
        CASE s_getbyte    :  RESULTIS  " % "
        CASE s_neg        :  RESULTIS  " -"
        CASE s_not        :  RESULTIS  " NOT"
        CASE s_abs        :  RESULTIS  " ABS"
        CASE s_rv         :  RESULTIS  " !"
        
        DEFAULT           :  cgerror( "opstr( %N )", op )
    $)
$)

