//******************************************************************************
//*                                                                            *
//*    ##    ##   ######    ######   ##    ##    ####     ######   ##    ##    *
//*    ###  ###  ########  ########  ##   ##    ######   ########  ###  ###    *
//*    ########  ##        ##    ##  ##  ##    ##    ##  ##        ########    *
//*    ## ## ##  #######    ######   ####      ########  #######   ## ## ##    *
//*    ##    ##  ##    ##  ##    ##  ## ##     ##    ##        ##  ##    ##    *
//*    ##    ##  ##    ##  ##    ##  ##  ##    ##    ##        ##  ##    ##    *
//*    ##    ##   ######   ########  ##   ##   ##    ##  ########  ##    ##    *
//*    ##    ##    ####     ######   ##    ##  ##    ##   ######   ##    ##    *
//*                                                                            *
//*    ====================================================================    *
//*          Macro Assembler for the Motorola MC68000 Microprocessor           *
//*    ====================================================================    *
//*                                                                            *
//******************************************************************************



//******************************************************************************
//*                                                                            *
//*  There are certain portions of the code which are dependent to particular  *
//*  machines and operating systems, and these are surrounded by conditional   *
//*  compilation tags of the form "$<xxx  $>xxx", where "xxx" is the name of   *
//*  the dependency.                                                           *
//*  These portions of code are also flagged by the special comment symbol     *
//*  "/* xxx */" in the right hand margin.                                     *
//*                                                                            *
//*                                                                            *
//*          TAG        Computer                                  O/S          *
//*          ---        --------                                  ---          *
//*                                                                            *
//*          370        IBM 370  (3081)                           (OS/MVT)     *
//*          CAP        Cambridge CAP computer                    (CHAOS)      *
//*          68K        Cambridge processor bank MC68000 computer (TRIPOS)     *
//*                                                                            *
//*                                                                            *
//*    N.B.  This assembler ASSUMES a 32-bit implementation of BCPL, but will  *
//*          run on a 16 bit machine, provided that only 16 bit arithmetic is  *
//*          required.                                                         *
//*                                                                            *
//*                                                                            *
//******************************************************************************
//*    I. D. Wilson              Last Modified:      IDW     24/07/85          *
//******************************************************************************





SECTION "M68KASM1"



$<370
NEEDS "M68KASM2"                                                       /* 370 */
NEEDS "M68KASM3"                                                       /* 370 */
NEEDS "M68KASM4"                                                       /* 370 */
NEEDS "M68KASM5"                                                       /* 370 */
NEEDS "M68KASM6"                                                       /* 370 */
$>370

@@

GET "libhdr"

GET "m68khdr.h"



LET start( parm )  BE
$(
$<68K
    LET av  =  VEC  50                                                 /* 68K */
    LET sv  =  VEC  15                                                 /* 68K */
    LET as  =  "FROM/A,TO/K,VER/K,LIST/K,HDR/K,EQU/K,SYMBOLS/K,OPT/K"  /* 68K */
$>68K

    LET name.sourcestream  =  0
    LET name.liststream    =  0
    LET name.codestream    =  0
    LET name.verstream     =  0
    LET name.equstream     =  0
    LET name.symbolstream  =  0

    fatalerrorp     :=  level()

$<370
    datestring      :=  date()                                         /* 370 */
    timestring      :=  timeofday()                                    /* 370 */
$>370

$<CAP
    datestring      :=  date()                                         /* CAP */
    timestring      :=  time()                                         /* CAP */
$>CAP

$<68K
    sv              :=  datstring( sv )                                /* 68K */
    datestring      :=  sv + 0                                         /* 68K */
    timestring      :=  sv + 5                                         /* 68K */
$>68K

    version         :=  9
    release         :=  170

    initstore( storesize )

    tagv            :=  getchunk( tagsize, TRUE )
    macroname       :=  getchunk( tagsize, TRUE )
    labelvec        :=  getchunk( tagsize, TRUE )
    expvec          :=  getchunk( expsize, TRUE )
    expvecp         :=  expvec + expsize

    inputbuff       :=  getchunk( maxllen/bytesperword,       TRUE )
    titlevec        :=  getchunk( titlecharsmax/bytesperword, TRUE )
    outbuff         :=  getchunk( maxllen/bytesperword,       TRUE )
    codebuff        :=  getchunk( codesize*cb.size,           TRUE )
    errorvec        :=  getchunk( errorsize*eb.size,          TRUE )

    extrnsymbols    :=  0
    entrysymbols    :=  0
    sourcestream    :=  0
    liststream      :=  0
    codestream      :=  0
    verstream       :=  0
    sysout          :=  output()

    linenumber      :=  0
    maxextlength    :=  7

    failed          :=  no
    in.movem        :=  no
    errormessages   :=  no
    crossreference  :=  no
    paging          :=  yes
    parmlisting     :=  no
    xref            :=  no
    externalref     :=  no
    pass1           :=  no
    pass2           :=  no

    objectmodule    :=  o.none
    ts.default      :=  ts.word

    addressmask     :=  mask.68000

$<370
    IF  sysout = 0  THEN                                               /* 370 */
    $(                                                                 /* 370 */
        // No output stream - "SYSPRINT" was not given.                /* 370 */
        // Write a panic message and give up.  On the IBM/370 this     /* 370 */
        // is done by writing to the HASP log.  Most other systems     /* 370 */
        // will have some safety valve which can be used.              /* 370 */
                                                                       /* 370 */
        writetolog( "****** M68KASM:  DD for *"SYSPRINT*" missing" )   /* 370 */
        stop( 16 )                                                     /* 370 */
    $)                                                                 /* 370 */
                                                                       /* 370 */
    sourcestream  :=  findinput( "SYSIN" )                             /* 370 */
    checkopen( sourcestream, "SYSIN" )                                 /* 370 */
                                                                       /* 370 */
    liststream    :=  findoutput( "LISTING" )                          /* 370 */
    checkopen( liststream, "LISTING" )                                 /* 370 */
                                                                       /* 370 */
    codestream    :=  findoutput( "CODE" )                             /* 370 */
    checkopen( codestream, "CODE" )                                    /* 370 */
                                                                       /* 370 */
    currentfile   :=  makefile( "SYSIN" )                              /* 370 */
$>370


$<CAP
    $(  //  Do the parameter decoding for CAP.  We have to extract the /* CAP */
        //  parameter string, and then add on any options which were   /* CAP */
        //  implied by the files given.                                /* CAP */
                                                                       /* CAP */
        LET userparm     =  VEC 256/bytesperword                       /* CAP */
        LET defaultparm  =  "  "                                       /* CAP */
        LET lparm        =  defaultparm % 0                            /* CAP */
        LET rc           =  keyarg( "OPT", parms.string )              /* CAP */
                                                                       /* CAP */
        sourcestream  :=  findinput( "PROGRAM|1" )                     /* CAP */
                                                                       /* CAP */
        IF  sourcestream = 0  THEN                                     /* CAP */
        $(                                                             /* CAP */
            writes( "****** Cannot open PROGRAM source file*N" )       /* CAP */
                                                                       /* CAP */
            failed  :=  yes                                            /* CAP */
        $)                                                             /* CAP */
                                                                       /* CAP */
        liststream   :=  findoutput( "LIST" )                          /* CAP */
        codestream   :=  findoutput( "TO" )                            /* CAP */
                                                                       /* CAP */
        currentfile  :=  makefile( "PROGRAM" )                         /* CAP */
                                                                       /* CAP */
        //  Having attempted to open the listing and output files, we  /* CAP */
        //  should construct the parameter string to be decoded later. /* CAP */
                                                                       /* CAP */
        parm  :=  getstore( 256/bytesperword )                         /* CAP */
                                                                       /* CAP */
        TEST  rc = parms.ok                                            /* CAP */
            THEN  movestring( k.n0, userparm )                         /* CAP */
            ELSE  userparm % 0  :=  0                                  /* CAP */
                                                                       /* CAP */
        FOR  i = 0  TO  lparm  DO  parm % i  :=  defaultparm % i       /* CAP */
                                                                       /* CAP */
        TEST  liststream = 0  THEN  liststream  :=  findoutput( "/A" ) /* CAP */
                              ELSE  parm % 1    :=  'L'                /* CAP */
                                                                       /* CAP */
        TEST  codestream = 0  THEN  codestream  :=  findoutput( "/A" ) /* CAP */
                              ELSE  parm % 2    :=  'T'                /* CAP */
                                                                       /* CAP */
        FOR  i = 1  TO  userparm % 0  DO                               /* CAP */
        $(                                                             /* CAP */
            lparm         :=  lparm + 1                                /* CAP */
            parm % lparm  :=  userparm % i                             /* CAP */
        $)                                                             /* CAP */
                                                                       /* CAP */
        parm % 0  :=  lparm                                            /* CAP */
                                                                       /* CAP */
        IF  liststream = 0  THEN  failed  :=  yes                      /* CAP */
        IF  codestream = 0  THEN  failed  :=  yes                      /* CAP */
    $)                                                                 /* CAP */
$>CAP                                                                  /* CAP */


$<68K
    //  Tripos form of the argument decoding.  Like Cap, we must make  /* 68K */
    //  the final options string out of the one given, and the one     /* 68K */
    //  implied by the extra arguments.                                /* 68K */
                                                                       /* 68K */
    TEST  rdargs( as, av, 50 ) = 0  THEN                               /* 68K */
    $(                                                                 /* 68K */
        writes( "******  Bad arguments for string:*N" )                /* 68K */
        writef( "******  *"%S*"*N", as )                               /* 68K */
                                                                       /* 68K */
        failed  :=  yes                                                /* 68K */
    $)                                                                 /* 68K */
    ELSE                                                               /* 68K */
    $(                                                                 /* 68K */
        LET ns           =  av!a.from                                  /* 68K */
        LET nc           =  av!a.to                                    /* 68K */
        LET nv           =  av!a.ver                                   /* 68K */
        LET nl           =  av!a.list                                  /* 68K */
        LET nh           =  av!a.hdr                                   /* 68K */
        LET neq          =  av!a.equ                                   /* 68K */
        LET nsymb        =  av!a.symbols                               /* 68K */
        LET opt          =  av!a.opt                                   /* 68K */
                                                                       /* 68K */
        LET defaultparm  =  "  "                                       /* 68K */
        LET parmlength   =  defaultparm % 0                            /* 68K */
        LET uparm        =  opt = 0  ->  "", opt                       /* 68K */
        LET uparmlength  =  uparm % 0                                  /* 68K */
        LET plength      =  parmlength + uparmlength                   /* 68K */
                                                                       /* 68K */
        parm  :=  getstore( 256/bytesperword )                         /* 68K */
                                                                       /* 68K */
        FOR  i = 1  TO  parmlength  DO  parm % i  :=  defaultparm % i  /* 68K */
                                                                       /* 68K */
        FOR  i = 1  TO  uparmlength  DO                                /* 68K */
             parm % (parmlength + i)  :=  uppercase( uparm % i )       /* 68K */
                                                                       /* 68K */
        parm % 0           :=  plength                                 /* 68K */
                                                                       /* 68K */
        name.sourcestream  :=  ns                                      /* 68K */
        name.codestream    :=  nc = 0  ->  "nil:", nc                  /* 68K */
        name.verstream     :=  nv = 0  ->  "**",   nv                  /* 68K */
        name.liststream    :=  nl = 0  ->  "nil:", nl                  /* 68K */
        name.equstream     :=  neq                                     /* 68K */
        name.symbolstream  :=  nsymb                                   /* 68K */
        name.hdrfile       :=  nh                                      /* 68K */
                                                                       /* 68K */
        sourcestream       :=  findinput( name.sourcestream )          /* 68K */
        checkopen( sourcestream, result2, name.sourcestream, "input" ) /* 68K */
                                                                       /* 68K */
        liststream         :=  findoutput( name.liststream )           /* 68K */
        checkopen( liststream, result2, name.liststream, "output" )    /* 68K */
                                                                       /* 68K */
        UNLESS  nl = 0  DO  parm % 1  :=  'L'                          /* 68K */
                                                                       /* 68K */
        codestream   :=  findoutput( name.codestream )                 /* 68K */
        checkopen( codestream, result2, name.codestream, "output" )    /* 68K */
                                                                       /* 68K */
        UNLESS  nc = 0  DO  parm % 2  :=  'T'                          /* 68K */
                                                                       /* 68K */
        verstream    :=  findoutput( name.verstream )                  /* 68K */
        checkopen( verstream, result2, name.verstream, "output" )      /* 68K */
                                                                       /* 68K */
        currentfile  :=  makefile( name.sourcestream )                 /* 68K */
    $)                                                                 /* 68K */
$>68K


    IF  failed  THEN  abortassembly()


    //  Decode the parm string.  Parameters have the following meaning:
    //
    //  X          -    Produce an Alphabetic Cross Reference
    //  L          -    Produce an Assembler Listing
    //  T          -    Produce a  TRIPOS     object module
    //  M          -    Produce a  MOTOROLA   object module
    //  H          -    Produce an INTEL HEX  object module
    //  E          -    Allow long external symbol names
    //  A          -    Allow extended (28 bit) addressing

    FOR  i = 1  TO  parm % 0  DO
    $(
        SWITCHON  uppercase( parm % i )   INTO
        $(
            CASE  'X'  :  xref          :=  yes            ;  LOOP
            CASE  'L'  :  parmlisting   :=  yes            ;  LOOP
            CASE  'T'  :  objectmodule  :=  o.tripos       ;  LOOP
            CASE  'M'  :  objectmodule  :=  o.motorola     ;  LOOP
            CASE  'H'  :  objectmodule  :=  o.intelhex     ;  LOOP
            CASE  'E'  :  maxextlength  :=  15             ;  LOOP
            CASE  'A'  :  addressmask   :=  mask.68010     ;  LOOP


            CASE  '*S' :
            CASE  '*T' :
            CASE  ','  :  //  Ignorable characters
                          LOOP


            DEFAULT    :  writef( "******  Option *"%C*" ignored*N", parm % i )
                          LOOP
        $)
    $)

    //  Now, set up the tag tables.  There are two, one for each class of
    //  symbol.

    tagtable1  :=  getchunk( tagtablesize, TRUE )
    tagtable2  :=  getchunk( tagtablesize, TRUE )

    FOR  i = 0  TO  tagtablesize-1  DO
    $(
        tagtable1!i  :=  0
        tagtable2!i  :=  0
    $)

$<68K
    sysout  :=  verstream                                              /* 68K */
$>68K

    selectoutput( sysout )

    writef( "MC68000 Macro Assembler  Version %N.%N", version, release )

$<370
    UNLESS  parm % 0  =  0  DO  writef( "  Opt = *"%S*"", parm )       /* 370 */
$>370

    newline()

    systemwords  :=  yes
    declsyswords()
    systemwords  :=  no

    selectoutput( liststream )
    selectinput( sourcestream )

    firstpass()

    selectoutput( sysout )
    newline()                             // Mark end of first pass
    selectoutput( liststream )

    secondpass( name.sourcestream )

    UNLESS  noobj  DO
            TEST  objectmodule = o.tripos      THEN  triposmodule()       ELSE
            TEST  objectmodule = o.motorola    THEN  motorolamodule()     ELSE
            TEST  objectmodule = o.intelhex    THEN  intelhexmodule()     ELSE

                  UNLESS  objectmodule = o.none  DO  complain( 0 )

    //  We now terminate the run by printing out all the relevant information
    //  about the run.  For this, we should set "listing" to be TRUE, and then
    //  rely on "parmlisting" to tell us whether to produce output.

    listing  :=  TRUE

    //  Write out the errors (which have been stacked up in Errorvec)

    IF  parmlisting  THEN
    $(
        clearbuffer()

        TEST  errors = 0  THEN
        $(
            spacelines( 3 )

            listed   :=  no
            linepos  :=  0

            writestring( "No errors found in this assembly" )
            printbuffer()
        $)
        ELSE
        $(
            IF  aborted  THEN
            $(
                spacelines( 3 )

                listed   :=  no
                linepos  :=  0

                writestring( "Fatal error  -  assembly aborted" )
                printbuffer()
            $)

            settitle( "ERROR-DIAGNOSTICS" )

            errormessages  :=  yes
            onpage         :=  0

            FOR  i = 0  TO  errors-1  DO
            $(
                LET offset  =  i*eb.size
                LET line    =  errorvec!(offset + eb.line)
                LET code    =  errorvec!(offset + eb.code)
                LET file    =  errorvec!(offset + eb.file)

                clearbuffer()

                linepos  :=  0
                writestring( file )

                linepos  :=  34
                writenumber( line, 5 )

                linepos  :=  43
                writestring( message( code ) )

                printbuffer()
            $)

            clearbuffer()
            spacelines( 3 )

            listed   :=  no
            linepos  :=  0

            writenumber( errors, 4 )
            writestring( " error" )

            UNLESS  errors = 1  DO  writechar( 's' )

            writestring( " found in this assembly" )

            printbuffer()
        $)
    $)

    IF  xref  &  parmlisting  THEN  // Print a cross reference table
    $(
        errormessages   :=  no
        crossreference  :=  yes
        xreftable       :=  0

        FOR  i = 0  TO  tagtablesize-1  DO
        $(
            LET t  =  tagtable2!i

            UNTIL  t = 0  DO
            $(
                UNLESS  t!st.definition = 0  DO  putinxreftable( t, @xreftable )

                t  :=  t!st.link
            $)
        $)

        printxreftable()
    $)

    selectoutput( sysout )

    //  Now print the equates file if we have been asked to do so.  We look
    //  to see if the DDname or keyword EQU has been given, and if so, we
    //  print out the equates file.

$<370
    printequates( findoutput( "EQU" ), "EQU" )                         /* 370 */
$>370


$<CAP
    printequates( findoutput( "EQU" ), "EQU" )                         /* CAP */
$>CAP


$<68K
    UNLESS  name.equstream = 0  DO                                     /* 68K */
    $(                                                                 /* 68K */
        LET stream  =  findoutput( name.equstream )                    /* 68K */
                                                                       /* 68K */
        //  First, check that the file has actually been opened        /* 68K */
        //  properly, then then do the symbol table dumping.           /* 68K */
                                                                       /* 68K */
        checkopen( stream, result2, name.equstream, "output" )         /* 68K */
                                                                       /* 68K */
        IF  failed  THEN  abortassembly()                              /* 68K */
                                                                       /* 68K */
        printequates( stream, name.sourcestream )                      /* 68K */
    $)                                                                 /* 68K */
$>68K

    //  Having dumped the absolute symbols in an "equate" manner, we should
    //  look to see if we have been asked to dump the symbols for the debugger.

$<370
    dumpsymbols( findoutput( "SYMBOLS" ), "SYMBOLS" )                  /* 370 */
$>370


$<CAP
    dumpsymbols( findoutput( "SYMBOLS" ), "SYMBOLS" )                  /* CAP */
$>CAP


$<68K
    UNLESS  name.symbolstream = 0  DO                                  /* 68K */
    $(                                                                 /* 68K */
        LET stream  =  findoutput( name.symbolstream )                 /* 68K */
                                                                       /* 68K */
        //  First, check that the file has actually been opened        /* 68K */
        //  properly, then then do the symbol table dumping.           /* 68K */
                                                                       /* 68K */
        checkopen( stream, result2, name.symbolstream, "output" )      /* 68K */
                                                                       /* 68K */
        IF  failed  THEN  abortassembly()                              /* 68K */
                                                                       /* 68K */
        dumpsymbols( stream, name.sourcestream )                       /* 68K */
    $)                                                                 /* 68K */
$>68K

    //  Now, print out the statistics associated with the assembly.
    //  This is the last thing we have to do before we can push off home!

    TEST  errors = 0  THEN  writes( "No " )
                      ELSE  writef( "%N ", errors )

    writef( "error%S found in this assembly*N", errors = 1  ->  "", "s" )

    writes( "*NAssembly statistics (32 bit words)*N*N*
             *           Absolute   Relocatable*N" )

    writef( "Code        %I5         %I5*N", (absmax-absmin)/bytesper68000word,
                                                      relmax/bytesper68000word )

    writef( "Reloc [16]  %I5         %I5*N", absrp16, relrp16  )
    writef( "      [32]  %I5         %I5*N", absrp32, relrp32 )

fatalerror:
    IF  aborted  THEN  writes( "*N*N******  Assembly Aborted*N" )

    uninitstore()

    writef( "*N%N out of %N words of workspace used*N",
             storage.wordsused, storage.totalwords )

    selectoutput( liststream )    ;   endwrite()
    selectoutput( codestream )    ;   endwrite()
    selectoutput( sysout )        ;   endwrite()

    selectinput( sourcestream )   ;   endread()

    stop( aborted  ->  rc.aborted, errors > 0  ->  rc.errors, 0 )
$)



$<370
AND checkopen( stream, DDname )  BE                                    /* 370 */
$(                                                                     /* 370 */
    IF  stream = 0  THEN                                               /* 370 */
    $(                                                                 /* 370 */
        writef( "****** Cannot Open DDname *"%S*"*N",                  /* 370 */
                 DDname )                                              /* 370 */
        failed  :=  yes                                                /* 370 */
    $)                                                                 /* 370 */
$)                                                                     /* 370 */
$>370



$<68K
AND checkopen( stream, r2, name, type )  BE                            /* 68K */
$(                                                                     /* 68K */
    //  Called after a findinput or findoutput, we must check to       /* 68K */
    //  see that the stream specified has been opened properly.        /* 68K */
                                                                       /* 68K */
    IF  stream = 0  THEN                                               /* 68K */
    $(                                                                 /* 68K */
        writef( "****** Cannot open %S for %S:  ", name, type )        /* 68K */
        fault( r2 )                                                    /* 68K */
                                                                       /* 68K */
        failed  :=  yes                                                /* 68K */
    $)                                                                 /* 68K */
$)                                                                     /* 68K */
$>68K



$<370
AND abort( code, address, oldstack, data )  BE                         /* 370 */
$(                                                                     /* 370 */
    LET scc  =  (code >> 12) & #XFFF                                   /* 370 */
    LET ucc  =  (code)       & #XFFF                                   /* 370 */
    LET ssp  =  findoutput( "SYSPRINT" )                               /* 370 */
                                                                       /* 370 */
    IF  ssp = 0  THEN                                                  /* 370 */
    $(                                                                 /* 370 */
        LET log  =  findlog()                                          /* 370 */
                                                                       /* 370 */
        IF  log = 0  THEN                                              /* 370 */
        $(                                                             /* 370 */
            writetolog( "****** M68KASM:  Cannot open SYSPRINT" )      /* 370 */
            stop( rc.catastrophic )                                    /* 370 */
        $)                                                             /* 370 */
                                                                       /* 370 */
        selectoutput( log )                                            /* 370 */
                                                                       /* 370 */
        writef( "****** M68KASM Abend at :%X6 SCC=%X3 UCC=%X3*N",      /* 370 */
                 address, scc, ucc )                                   /* 370 */
                                                                       /* 370 */
        stop( 100 )                                                    /* 370 */
    $)                                                                 /* 370 */
                                                                       /* 370 */
    selectoutput( ssp )                                                /* 370 */
                                                                       /* 370 */
    //  Post mortem dump for M68KASM on the IBM370.  Print out the     /* 370 */
    //  values of certain variables, and if the error code looked      /* 370 */
    //  important, ask the users to see IDW.                           /* 370 */
                                                                       /* 370 */
    writef( "*N*NM68KASM Error at address :%X6 on %S at %S*N*N",       /* 370 */
             address, date(), timeofday() )                            /* 370 */
                                                                       /* 370 */
    //  Look to see if the abend is something like 083, E37, 013,      /* 370 */
    //  084, 0D1, 80A and if so, explain it.                           /* 370 */
                                                                       /* 370 */
    TEST  scc = #X083  |  scc = #XE37  |  scc = #X013  |               /* 370 */
          scc = #X084  |  scc = #X0D1  |  scc = #X80A  THEN            /* 370 */
    $(                                                                 /* 370 */
        writef( "System completion code %X3:  ", scc )                 /* 370 */
        writes( scc = #X083  ->  "CPU time limit exceeded",            /* 370 */
                scc = #X0D1  ->  "CPU time limit exceeded",            /* 370 */
                scc = #XE37  ->  "Cannot extend output file",          /* 370 */
                scc = #X013  ->  "Cannot open PDS member",             /* 370 */
                scc = #X80A  ->  "Insufficient IOSPACE",               /* 370 */
                scc = #X084  ->  "Printer limit exceeded", "" )        /* 370 */
                                                                       /* 370 */
        writes( "*N*N" )                                               /* 370 */
        stop( rc.catastrophic )                                        /* 370 */
    $)                                                                 /* 370 */
                                                                       /* 370 */
    ELSE                                                               /* 370 */
    $(                                                                 /* 370 */
        //  The result is more serious, and we must continue with      /* 370 */
        //  the entire mapstore.  Print out a message, so that the     /* 370 */
        //  output will be send to someone who understands it.         /* 370 */
                                                                       /* 370 */
        writef( "Fatal Abend:   SCC = %X3   UCC = %N*N*N",             /* 370 */
                 scc, ucc )                                            /* 370 */
                                                                       /* 370 */
        writef( "Abend occurred on line %N in pass %n",                /* 370 */
                 linenumber, (pass1  ->  1, (pass2  ->  2, 0)) )       /* 370 */
                                                                       /* 370 */
        IF  getlevel   > 0  THEN  writes( ", in GET file" )            /* 370 */
        IF  macrodepth > 0  THEN  writes( ", in MACRO expn." )         /* 370 */
        IF  inmacro         THEN  writes( ", in MACRO defn." )         /* 370 */
        IF  skiplevel  > 0  THEN  writes( ", in Cond. Assembly")       /* 370 */
        IF  skipping        THEN  writes( ", whilst skipping" )        /* 370 */
                                                                       /* 370 */
        writef( ".*N%S code type at location %X6.",                    /* 370 */
                 locmode = s.rel  ->  "Relocatable", "Absolute",       /* 370 */
                 location )                                            /* 370 */
                                                                       /* 370 */
        writef( "*NError %Sfound,  Errors = %N,  %SAborted",           /* 370 */
                 error.found  ->  "", "not ", errors,                  /* 370 */
                 aborted      ->  "", "Not " )                         /* 370 */
                                                                       /* 370 */
        writef( "*NEA:  Mode = %X4  Type = %I3  Exp = %X8",            /* 370 */
                 op.ea, exptype, exp )                                 /* 370 */
                                                                       /* 370 */
        writef( "*NEA': Mode = %X4  Type = %I3  Exp = %X8",            /* 370 */
                 op1.ea, op1.exptype, op1.exp )                        /* 370 */
                                                                       /* 370 */
        writes( "*N*N" )                                               /* 370 */
                                                                       /* 370 */
        FOR  i = 1  TO  5  DO                                          /* 370 */
             writes( "########  Please report this error*N" )          /* 370 */
                                                                       /* 370 */
        writes( "*N*N" )                                               /* 370 */
    $)                                                                 /* 370 */
                                                                       /* 370 */
    TEST  oldstack = !(@code-2)  THEN  backtrace()                     /* 370 */
    ELSE  writes( "*N*NStack Pointer Corrupted.*N*N" )                 /* 370 */
                                                                       /* 370 */
    mapstore()                                                         /* 370 */
    stop( rc.catastrophic )                                            /* 370 */
$)                                                                     /* 370 */
$>370



AND abortassembly()  BE
$(
//  This routine is called on some sort of error, usually when we run out of
//  store.  Close down the streams which have been opened, and free any store
//  which has been allocated.  Then stop, without futher ado.

    UNLESS  sourcestream = 0  DO
    $(
        selectinput( sourcestream )
        endread()
    $)

    UNLESS  liststream = 0  DO
    $(
        selectoutput( liststream )
        endwrite()
    $)

    UNLESS  codestream = 0  DO
    $(
        selectoutput( codestream )
        endwrite()
    $)

$<68K
    UNLESS  verstream = 0  DO                                          /* 68K */
    $(                                                                 /* 68K */
        selectoutput( verstream )                                      /* 68K */
        endwrite()                                                     /* 68K */
    $)                                                                 /* 68K */
$>68K

    uninitstore()

    stop( rc.catastrophic )
$)



AND firstpass()  BE
$(
//  Perform the first pass of the assembly.

    relmin          :=  maxint
    relmax          :=  minint
    absmin          :=  maxint
    absmax          :=  minint

    absrp16         :=  0
    absrp32         :=  0
    relrp16         :=  0
    relrp32         :=  0
    absloc          :=  0
    relloc          :=  0

    locmode         :=  s.rel
    relp16          :=  relrp16
    relp32          :=  relrp32
    minloc          :=  relmin
    maxloc          :=  relmax
    location        :=  0

    errors          :=  0
    skiplevel       :=  0
    skipping        :=  0
    macrodepth      :=  0
    getlevel        :=  0
    macrobase       :=  0
    macroend        :=  0
    asmlabel        :=  0
    pass1           :=  yes
    pass2           :=  no
    inmacro         :=  no
    errormessages   :=  no
    forwardreftype  :=  s.abs16
    charpos         :=  1
    ended           :=  no
    aborted         :=  no
    noobj           :=  objectmodule  =  o.none
    listing         :=  parmlisting

    settitle( "" )

    length          :=  -1
    charpos         :=  0
    linepos         :=  0
    onpage          :=  0
    linenumber      :=  0
    pagenumber      :=  0
    linesperpage    :=  60
    charsperline    :=  132
    llenfixed       :=  no
    plenfixed       :=  no

    selectinput( sourcestream )

$<370
    setwindow( maxllen )                                               /* 370 */
$>370

    //  If the GET input stream is present, then we must do an
    //  implicit "GET" of this file before the rest of the program
    //  is dealt with.

    resetflags()

    listed  :=  yes

$<370
    doget( "GET" )                                                     /* 370 */
$>370

$<CAP
    doget( "HDR" )                                                     /* CAP */
$>CAP

$<68K
    UNLESS  name.hdrfile = 0  DO  doget( name.hdrfile )                /* 68K */
$>68K

    listed  :=  no

    //  Fasten your safety belts...

    UNTIL  ended | aborted  DO
    $(
        resetflags()

        doline()
    $)

    //  Force the saving of final location values.

    IF  absmin = maxint   THEN  absmin  :=  0
    IF  relmin = maxint   THEN  relmin  :=  0
    IF  absmax = minint   THEN  absmax  :=  0
    IF  relmax = minint   THEN  relmax  :=  0

    IF  minloc = maxint   THEN  minloc  :=  0
    IF  maxloc = minint   THEN  maxloc  :=  0

    //  Align the code buffers to word boundaries

    changemode( s.rel ) ; align( bytesper68000word )
    changemode( s.abs ) ; align( bytesper68000word )
    changemode( s.rel )

    absmin  :=  absmin - (absmin REM bytesper68000word )
$)




AND secondpass( name.sourcestream )  BE
$(
//  Allocate the store required for the assembled code.  All the absolute and
//  relocatable extreme values have already been rounded up or down.

    LET avec  =  getchunk( (absmax - absmin)/bytesperword, TRUE )

    absvec     :=  avec - absmin/bytesperword

    absrvec16  :=  getchunk( absrp16, TRUE )
    absrvec32  :=  getchunk( absrp32, TRUE )
    relvec     :=  getchunk( relmax/bytesperword, TRUE )
    relrvec16  :=  getchunk( relrp16, TRUE )
    relrvec32  :=  getchunk( relrp32, TRUE )

    //  Clear the code buffers (Tripos SYSLINK depends on this)

    FOR  i = absmin  TO  absmax-1  DO  absvec % i  :=  0
    FOR  i = relmin  TO  relmax-1  DO  relvec % i  :=  0

    relmin          :=  maxint
    relmax          :=  0
    absmin          :=  maxint
    absmax          :=  0

    absrp16         :=  0
    absrp32         :=  0
    relrp16         :=  0
    relrp32         :=  0
    absloc          :=  0
    relloc          :=  0

    locmode         :=  s.rel
    codevec         :=  relvec
    relocvec16      :=  relrvec16
    relocvec32      :=  relrvec32
    relp16          :=  relrp16
    relp32          :=  relrp32

    minloc          :=  relmin
    maxloc          :=  relmax
    location        :=  0

    ended           :=  no
    aborted         :=  no
    errors          :=  0
    skiplevel       :=  0
    skipping        :=  0
    macrodepth      :=  0
    getlevel        :=  0
    macrobase       :=  0
    macroend        :=  0
    asmlabel        :=  0
    pass1           :=  no
    pass2           :=  yes
    inmacro         :=  no
    errormessages   :=  no
    forwardreftype  :=  s.abs16
    noobj           :=  objectmodule  =  o.none
    listing         :=  parmlisting

    clearbits()

    settitle( "" )

    length          :=  -1
    charpos         :=  0
    linepos         :=  0
    onpage          :=  0
    linenumber      :=  0
    pagenumber      :=  0
    llenfixed       :=  no
    plenfixed       :=  no

    // Rewind the input stream and start again from the beginning.

    UNLESS  rewind( name.sourcestream )  DO
    $(
        selectoutput( sysout )

        writes( "****** Unable to rewind input stream*N" )

        abortassembly()
    $)

$<370
    setwindow( maxllen )                                               /* 370 */
$>370

    //  If the GET input stream is present, then we must do an
    //  implicit "GET" of this file before the rest of the program
    //  is dealt with.

    resetflags()

    listed  :=  yes

$<370
    doget( "GET" )                                                     /* 370 */
$>370

$<CAP
    doget( "HDR" )                                                     /* CAP */
$>CAP

$<68K
    UNLESS  name.hdrfile = 0  DO  doget( name.hdrfile )                /* 68K */
$>68K

    listed  :=  no

    UNTIL  ended | aborted  DO
    $(
        resetflags()

        doline()
    $)

    UNLESS  skipping = 0  DO  warning( 103 )

    IF  inmacro  THEN  warning( 113 )

    IF  relmin = maxint  THEN  relmin  :=  0
    IF  absmin = maxint  THEN  absmin  :=  0
    IF  minloc = maxint  THEN  minloc  :=  0

    //  Align the code buffers to word boundaries

    changemode( s.rel ) ; align( bytesper68000word )
    changemode( s.abs ) ; align( bytesper68000word )
    changemode( s.rel )

    absmin  :=  absmin - (absmin REM bytesper68000word )
$)



$<68K
AND rewind( name.sourcestream )  =  VALOF                              /* 68K */
$(                                                                     /* 68K */
//  Substitute REWIND for the TRIPOS BCPL system.  Close               /* 68K */
//  and re-open the sourcestream.                                      /* 68K */
                                                                       /* 68K */
    endread()                                                          /* 68K */
                                                                       /* 68K */
    sourcestream  :=  findinput( name.sourcestream )                   /* 68K */
                                                                       /* 68K */
    selectinput( sourcestream )                                        /* 68K */
                                                                       /* 68K */
    RESULTIS  sourcestream  \=  0                                      /* 68K */
$)                                                                     /* 68K */
$>68K



AND doline()  BE
$(
//  Parse, and generate code for an entire input line.

$<68K
    IF  testflags( #B0001 )  THEN                                      /* 68K */
        error( 149 )                                                   /* 68K */
                                                                       /* 68K */
    IF  testflags( #B1000 )  THEN                                      /* 68K */
    $(                                                                 /* 68K */
        selectoutput( sysout )                                         /* 68K */
                                                                       /* 68K */
        writef( "******  Pass %N  File *"%S*"  Line %N  Errors %N*N",  /* 68K */
                 pass1 -> 1, 2, currentfile, linenumber, errors )      /* 68K */
                                                                       /* 68K */
        selectoutput( liststream )                                     /* 68K */
    $)                                                                 /* 68K */
$>68K

    labelset      :=  no
    undefined     :=  no
    recoverlevel  :=  level()

    rch()

    SWITCHON  ch  INTO
    $(
        CASE '**'     : // Comment line
        CASE '!'      : // New style comment line
        CASE '*N'     : // Blank line

                        skiprest()

                        symb         :=  s.none
                        commentline  :=  yes

                        ENDCASE

        CASE '.'      :
        CASE '_'      :

        CASE 'A'      : CASE 'B'      : CASE 'C'      : CASE 'D'      :
        CASE 'E'      : CASE 'F'      : CASE 'G'      : CASE 'H'      :
        CASE 'I'      : CASE 'J'      : CASE 'K'      : CASE 'L'      :
        CASE 'M'      : CASE 'N'      : CASE 'O'      : CASE 'P'      :
        CASE 'Q'      : CASE 'R'      : CASE 'S'      : CASE 'T'      :
        CASE 'U'      : CASE 'V'      : CASE 'W'      : CASE 'X'      :
        CASE 'Y'      : CASE 'Z'      :

        CASE 'a'      : CASE 'b'      : CASE 'c'      : CASE 'd'      :
        CASE 'e'      : CASE 'f'      : CASE 'g'      : CASE 'h'      :
        CASE 'i'      : CASE 'j'      : CASE 'k'      : CASE 'l'      :
        CASE 'm'      : CASE 'n'      : CASE 'o'      : CASE 'p'      :
        CASE 'q'      : CASE 'r'      : CASE 's'      : CASE 't'      :
        CASE 'u'      : CASE 'v'      : CASE 'w'      : CASE 'x'      :
        CASE 'y'      : CASE 'z'      :

                        readlabel()
                        IF  ch = ':'  THEN  rch()

                        UNLESS  tagsize.given = ts.none  DO  complain( 1 )

                        FOR  i = 0  TO  tagsize-1  DO  labelvec!i  :=  tagv!i

                        labelset  :=  yes
                        skiplayout()

                        undefined  :=  no
                        readopcode()

                        ENDCASE


        CASE '\'      : UNLESS  inmacro  DO  complain( 117 )
                        UNTIL  ch = '*S'  |  ch = '*T'  |  ch = '*N'  DO  rch()

        CASE '*S'     :
        CASE '*T'     : skiplayout()

                        IF  ch = '*N'  THEN
                        $(
                            symb         :=  s.none
                            commentline  :=  yes

                            ENDCASE
                        $)

                        IF  ch = '\'  THEN
                        $(
                            UNLESS  inmacro  DO  complain( 117 )

                            UNTIL  ch = '*S'  |  ch = '*T'  |  ch = '*N'  DO
                                   rch()

                            symb  :=  s.none

                            ENDCASE
                        $)

                        UNLESS  symbolchar( ch, FALSE )  DO

                                //  Not a valid start to a symbol name, and
                                //  so we should complain.

                                complain( 2 )

                        readlabel()

                        TEST  ch = ':'  THEN
                        $(
                            //  This really is a label, since it has the
                            //  terminating ":" character.

                            rch()

                            UNLESS  tagsize.given = ts.none  DO  complain( 1 )

                            FOR  i = 0  TO  tagsize-1  DO  labelvec!i  :=  tagv!i

                            labelset  :=  yes
                            skiplayout()

                            undefined  :=  no
                            readopcode()
                        $)
                        ELSE
                        $(
                            //  This isn't a label at all, and is in fact an
                            //  opcode.  We should look this symbol up in the
                            //  opcode table.

                            undefined  :=  no

                            lookup( tagv, tagtable1 )
                        $)

                        ENDCASE


        CASE endstreamch :
                        symb  :=  s.none

                        UNLESS  getlevel = 0  DO
                        $(
                            //  End of a GET file, so close it, and
                            //  Return to the previous level.

                            getlevel    :=  getlevel - 1
                            linenumber  :=  linenumber - 1
                            ended       :=  no

                            RETURN
                        $)

                        IF  pass2  THEN
                        $(
                            selectoutput( sysout )

                            writes( "******  'END' statement missing*N*N" )

                            selectoutput( liststream )
                        $)

                        ended  :=  yes

                        ENDCASE


        DEFAULT       : complain( 5 )
    $)

    IF  undefined  &  pass2  THEN

        //  This is an undefined symbol in the opcode field.  This is serious
        //  unless we are in a macro, in which case this could just be
        //  something with a "\" in it.

        UNLESS  inmacro  DO  complain( 96 )

    //  Go on to decode the opcode/directive field.

    TEST  skiplevel > 0  THEN
    $(
          //  We are in a conditional section, and so we must only
          //  do something if we have met an ENDC or another IF..
          //  directive.

          IF  symb = s.dir  &  (symbtype!st.value = d.endc  |
                                symbtype!st.value = d.ifeq  |
                                symbtype!st.value = d.ifne  |
                                symbtype!st.value = d.iflt  |
                                symbtype!st.value = d.ifle  |
                                symbtype!st.value = d.ifgt  |
                                symbtype!st.value = d.ifge  )  THEN
              dodir()

          commentline  :=  yes
    $)
    ELSE

    TEST  inmacro  THEN
    $(
        //  We are in a macro body, and unless this is an
        //  ENDM directive, we must stack up the current
        //  line in a buffer.  The exception to this is if
        //  another MACRO directive is found, in which case
        //  an error must be flagged.

        TEST  symb = s.dir  &   (symbtype!st.value = d.macro  |
                                 symbtype!st.value = d.endm )  THEN
              dodir()

        ELSE

        IF  pass1  THEN
        $(
            LET newbuff  =  getstore( length/bytesperword )
            LET newnode  =  heap3( 0, 0, 0 )

            FOR  i = 0  TO  length-1  DO  newbuff % i  :=  inputbuff % i

            macroend!m.buff    :=  newbuff
            macroend!m.length  :=  length
            macroend!m.link    :=  newnode

            macroend           :=  newnode
        $)

        commentline  :=  yes
    $)
    ELSE

    TEST  symb = s.instr  THEN  doinstr()      ELSE
    TEST  symb = s.dir    THEN  dodir()        ELSE
    TEST  symb = s.macro  THEN  domacro()      ELSE
    TEST  symb = s.none   THEN

          IF  labelset  THEN
              setlabel( locmode, location, no )

    ELSE  complain( 3 )

recoverlabel:                       // Recover here on error
    skiprest()                      // Just in case it hasn't been done before

    listline()
$)



AND doinstr()  BE
$(
//  We have decoded some sort of instruction.  Decode further to determine if
//  it is a special type of instruction, how many operands it takes, and what
//  its mask type is.

    LET t      =  symbtype!st.type
    LET vh     =  symbtype!st.value.high
    LET vl     =  symbtype!st.value.low
    LET sizes  =  0

    instr.mask      :=  symbtype!st.template   // instruction mask
    instr.masktype  :=  (t >> 4) & #B1111      // instruction mask type
    source.ea       :=  vh                     // source operand EA
    dest.ea         :=  vl                     // destination operand EA

    //  Instructions MUST be word aligned.

    UNLESS  aligned( 2 )  DO
    $(
        warning( 102 )

        align( 2 )
    $)

    IF  labelset  THEN  setlabel( locmode, location, no )

    nargs  :=  (t >> 11) & #B11         // Number of arguments
    sizes  :=  (t >>  8) & #B111        // Possible sizes allowed

    TEST  tagsize.given \= ts.none  THEN  UNLESS  tagsize.given = ts.short  DO
    $(
        LET sizebit  =  1 << (tagsize.given - 1)

        TEST  (sizes & sizebit) \= 0  THEN
              tagsize.given  :=  sizevalue( sizebit )

        ELSE  complain( 6 )
    $)
    ELSE  tagsize.given  :=  ts.none

    instr.size  :=  tagsize.given

    TEST  instr.masktype = 0  THEN  specialinstruction( dest.ea )
    ELSE
    $(
        //  The size of the instruction has been verified as being correct.
        //  Now read the operands (each is the form of an effective address)

        IF  nargs = 0  THEN  readsymb()

        IF  instr.size = ts.short  THEN  complain( 86 )

        IF  nargs = 1  THEN
        $(
            nextsymb()
            evaluate( effective.address() )

            IF  (dest.ea & op.ea) = 0  THEN  complain( 7 )
        $)

        IF  nargs = 2  THEN
        $(
            nextsymb()
            evaluate( effective.address() )

            IF  (source.ea & op.ea) = 0  THEN  complain( 8 )

            //  the first operand is correct, so store it away, and read the
            //  second.

            swapoperands()

            checkfor( s.comma, 10 )

            evaluate( effective.address() )

            IF  (dest.ea & op.ea) = 0  THEN  complain( 9 )
        $)

        //  the operands should have been terminated.
        //  If they were terminated by a ','  then there
        //  are too many arguments.  If not by a space, tab
        //  or newline, then bad termination of arguments.

        TEST  symb = s.comma  THEN  complain( 11 )     ELSE
        TEST  symb \= s.none  THEN  complain( 12 )     ELSE

              skiprest()

        generate( instr.masktype )
    $)
$)



AND aligned( boundary )  =  location REM boundary  =  0



AND domacro()  BE
$(
//  This line is a macro, and must be decoded as such.  Set up the argument
//  vector, and call "expandmacro" to actually do the expansion.

    LET argvec      =  VEC macroargs
    LET macrovalue  =  symbtype!st.value

    checklabel( no )

    //  Check for forward reference to a MACRO definition (which is illegal).

    IF  (symbtype!st.flags & stb.setnow) = 0  THEN  complain( 151 )

    FOR  i = 1  TO  macroargs  DO  argvec!i  :=  ""

    instr.size  :=  tagsize.given

    argvec!0    :=  instr.size  =  ts.byte  ->  "B",
                    instr.size  =  ts.word  ->  "W",
                    instr.size  =  ts.long  ->  "L",
                    instr.size  =  ts.short ->  "S",  ""

    skiplayout()

    FOR  i = 1  TO  macroargs  DO
    $(
        //  Read the arguments for the macro.

        LET argbuffer  =  VEC maxllen/bytesperword
        LET arglength  =  0
        LET argb       =  0

        TEST  ch = '<'  |  ch = '['  THEN    // Bracketed argument
        $(
            LET bracket  =  ch = '<'  ->  '>', ']'

            rch()

            UNTIL  ch = bracket  |  ch = '*N'  DO
            $(
                arglength              :=  arglength + 1
                argbuffer % arglength  :=  ch

                rch()
            $)

            TEST  ch = '*N'
                THEN  complain( 114 )
                ELSE  rch()
        $)
        ELSE

        UNTIL  ch = ','  |  ch = '*S'  |  ch = '*N'  |  ch = '*T'  DO
        $(
            arglength              :=  arglength + 1
            argbuffer % arglength  :=  ch

            rch()
        $)

        argbuffer % 0  :=  arglength
        argb           :=  getstore( arglength/bytesperword )

        FOR  j = 0  TO  arglength  DO  argb % j  :=  argbuffer % j

        argvec!i  :=  argb

        readsymb()

        TEST  symb = s.none   THEN   BREAK
        ELSE
            UNLESS  symb = s.comma  DO
                complain( 115 )
    $)

    IF  symb = s.comma  THEN  complain( 118 )

    expandmacro( macrovalue, argvec )
$)



AND expandmacro( macroptr, argvec )  BE
$(
//  Expand the source macro, whose text is pointed to by "macroptr".
//  The current depth of macro nesting is given by "depth".  The
//  restriction is that depth must not be greater than 3.  This is
//  a MOTOROLA restriction, and the macro depth for this implementation
//  is given by "maxmacrodepth".

    LET macroline  =  macroptr!m.buff
    LET asml       =  0
    LET depth      =  macrodepth
    LET skip       =  skipping
    LET skipl      =  skiplevel

    //  Macro nesting too deep.  Possibly in a recursive
    //  loop of macro expansion.

    IF  macrodepth = maxmacrodepth  THEN  complain( 108 )

    //  Before we expand the macro, we must list the line that
    //  the macro name is on.

    commentline  :=  yes

    listline()

    macrodepth  :=  macrodepth + 1

    resetflags()

    UNTIL  macroline = 0  |  macrodepth = depth  |  ended  |  aborted  DO
    $(
        LET sptr     =  0
        LET mptr     =  0
        LET wcode    =  0
        LET mlength  =  macroptr!m.length

        FOR  i = 0  TO  maxllen-1  DO  inputbuff % i  :=  '*S'

        UNTIL  mptr = mlength  DO
        $(
            LET char  =  macroline % mptr

            TEST  char = '\'  THEN
            $(
                mptr  :=  mptr + 1

                IF  mptr = mlength  THEN
                $(
                    wcode  :=  109

                    BREAK
                $)

                char  :=  macroline % mptr

                TEST  char = '@'  THEN
                $(
                    //  This is an assembler generated label.
                    //  The first time that this is encountered in
                    //  a macro expansion, a new label is obtained
                    //  from the function "newasmlabel".  Thereafter
                    //  this value is used in the entire macro
                    //  expansion from now on.

                    LET chbuff  =  VEC 10
                    LET size    =  0
                    LET label   =  0

                    IF  asml = 0  THEN  asml  :=  newasmlabel()

                    label  :=  asml
                    size   :=  digits( asml )

                    IF  size < 3  THEN  size  :=  3

                    FOR  i = size  TO  1  BY  -1  DO
                    $(
                        chbuff!i  :=  (label REM 10) + '0'
                        label     :=  label / 10
                    $)

                    putinbuffer( inputbuff, sptr, '.' )

                    FOR  i = 1  TO  size  DO
                         putinbuffer( inputbuff, sptr + i, chbuff!i )

                    sptr  :=  sptr + size + 1
                    mptr  :=  mptr + 1
                $)
                ELSE

                //  This should be an argument number (in the range
                //  '0' to '9' or 'A' to 'Z'), and can be treated as as index
                //  into "argvec".  Any other character is an error.

                TEST  macrochar( char )  THEN
                $(
                    LET argnumber  =  argoffset( char )
                    LET arg        =  argvec!argnumber

                    FOR  j = 1  TO  arg % 0  DO
                    $(
                        putinbuffer( inputbuff, sptr, arg % j )

                        sptr  :=  sptr + 1
                    $)

                    mptr  :=  mptr + 1
                $)
                ELSE
                $(
                    wcode  :=  109

                    BREAK
                $)
            $)
            ELSE
            $(
                TEST  macroline % mptr = '*S'  THEN
                $(
                    //  First, skip all spaces from the macro record
                    //  to find the column of the next non-space char.

                    WHILE  mptr < mlength  &  macroline % mptr = '*S'   DO
                           mptr  :=  mptr + 1

                    //  Now pad the output record with spaces up to this
                    //  column.  We MUST pad at least one space.

                    sptr  :=  sptr + 1  REPEATWHILE  sptr < mptr
                $)
                ELSE
                $(
                    putinbuffer( inputbuff,  sptr, char )

                    sptr  :=  sptr + 1
                    mptr  :=  mptr + 1
                $)
            $)
        $)

        //  Having filled up the buffer, look to see if we have overfilled
        //  it, and if so, complain.

        UNLESS  sptr < maxllen  DO
        $(
            wcode  :=  189
            sptr   :=  maxllen
        $)

        //  Now call "doline" with the newly constructed, macro expanded
        //  line.  Then get the next line in the macro expansion (found at
        //  macroptr!m.link), and carry on.

        TEST  wcode \= 0  THEN
        $(
            //  There has been some problem in decoding the formal parameters
            //  to the Macro, and so, we must print out the offending line
            //  and not bother to expand anything from it.

            FOR  i = 0  TO  mlength-1  DO  inputbuff % i  :=  macroline % i

            length              :=  mlength
            inputbuff % length  :=  '*N'
            commentline         :=  yes

            warning( wcode )

            listline()
        $)
        ELSE
        $(
            length              :=  sptr
            inputbuff % length  :=  '*N'
            charpos             :=  0

            doline()
        $)

        macroptr   :=  macroptr!m.link
        macroline  :=  macroptr!m.buff

        resetflags()
    $)

    IF  macrodepth = depth  DO
    $(
        //  If we have executed a "MEXIT" then we must reset the
        //  "skipping" and "skiplevel" variables.

        skipping    :=  skip
        skiplevel   :=  skipl

    $)

    macrodepth  :=  depth
    listed      :=  yes
$)



AND argoffset( char )  =  '0' <= char <= '9'  ->  char - '0',
                       /* 'A' <= char <= 'Z' */   char - 'A' + 10



AND digits( value )  =  value < 10  ->  1,  (digits( value/10 ) + 1)



AND newasmlabel()  =  VALOF
$(
//  Returns a new assembler generated label.

    asmlabel  :=  asmlabel + 1

    RESULTIS  asmlabel
$)



AND putinxreftable( node, ptr )  BE
$(
//  "node" points to a node which we want to insert into the sorted
//  tag table, and ptr is a pointer to the location which should point
//  to this node, when the node is inserted into the tree.

    TEST  !ptr = 0  THEN  !ptr  :=  heap3( node, 0, 0 )
    ELSE
    $(
        LET p  =  !ptr

        TEST  comparestrings( node+st.name, p!p.ptr0+st.name )  < 0
              THEN  putinxreftable( node, p+p.ptr1 )
              ELSE  putinxreftable( node, p+p.ptr2 )
    $)
$)



AND comparestrings( p1, p2 )  =  VALOF
$(
//  Compare the "strings" pointed to by p1 and p2, and return
//  <0  if  p1 < p2    and
//  >0  if  p1 > p2

    LET l1  =  p1 % 0
    LET l2  =  p2 % 0

    FOR  i = 1  TO  (l1 < l2  ->  l1, l2)  DO
    $(
        LET ch1   =  p1 % i
        LET ch2   =  p2 % i
        LET diff  =  ch1 - ch2

        UNLESS  diff = 0  DO  RESULTIS  diff
    $)

    RESULTIS  l1 - l2
$)



AND printxreftable()  BE
$(
    listing  :=  yes
    paging   :=  yes
    listed   :=  no
    onpage   :=  0

    settitle( "CROSS-REFERENCE" )

    clearbuffer()
    printnode( xreftable )

    clearbuffer()
$)



AND printnode( node )  BE  UNLESS  node = 0  DO
$(
    LET t       =  node!p.ptr0
    LET l       =  node!p.ptr1
    LET r       =  node!p.ptr2

    LET type    =  t!st.type  &  st.type.mask
    LET value   =  t!st.value
    LET line    =  t!st.definition
    LET refs    =  t!st.references
    LET name    =  t+st.name

    LET online  =  0


    printnode( l )

    linepos  :=  0
    writestring( name )

    linepos  :=  32

    TEST  (t!st.type & st.type.mask) = s.ext  THEN
          writestring( "******EXTERNAL******" )        ELSE

    TEST  line = cr.undefined  THEN
          writestring( "******UNDEFINED******" )       ELSE

    TEST  line = cr.multiple   THEN
          writestring( "******MULTIPLE******" )        ELSE

    TEST  line = cr.setsymbol  THEN
          writestring( "**********SET************" )   ELSE

          writenumber( line, 5 )

    linepos  :=  40

    IF  line > 0  THEN

        SWITCHON  type  INTO
        $(
            CASE s.rel     :  writehexvalue( value, 4  )
                              writechar( '*'' )
                              ENDCASE

            CASE s.reg     :  writehexvalue( value, 4  )
                              writechar( 'R' )
                              ENDCASE

            CASE s.abs16   :  writehexvalue( value, 4 )
                              ENDCASE

            CASE s.abs32   :  writehexvalue( value, 8 )
                              ENDCASE

            CASE s.Dr      :  writechar( 'D' )
                              writehexvalue( value, 1 )
                              ENDCASE

            CASE s.Ar      :  writechar( 'A' )
                              writehexvalue( value, 1 )
                              ENDCASE

            DEFAULT        :  writestring( "????" )
                              ENDCASE
        $)

    //  Now print out the references to this particular symbol.

    linepos  :=  52

    TEST  refs = 0  THEN
    $(
        linepos  :=  37

        IF  line > 0  THEN  writechar( 'U' )
    $)
    ELSE
    $(
        UNTIL  refs = 0  DO
        $(
            IF  online = 10  THEN
            $(
                printbuffer()
                clearbuffer()

                linepos  :=  50
                writestring( "- " )

                online   :=  0
            $)

            writenumber( refs!r.line, 5 )

            TEST  refs!r.file = currentfile
                THEN  writestring( "  " )
                ELSE  writestring( "** " )

            refs    :=  refs!r.link
            online  :=  online  +  1
        $)
    $)

    printbuffer()
    clearbuffer()

    printnode( r )
$)

.

//******************************************************************************
//*   Assembler for the Motorola MC68000 Microprocessor:  Section 2            *
//******************************************************************************


SECTION "M68KASM2"


GET "libhdr"

GET "m68khdr.h"



LET specialinstruction( inum )  BE
$(
    LET ofwd    =  0
    LET r.m     =  0
    LET regs    =  0
    LET opmode  =  0
    LET sz      =  0

    SWITCHON  inum  INTO
    $(
        CASE  1 :  //  ABCD  ADDX  SBCD  SUBX
                   //  Possible operands are:
                   //
                   //      a)      Dy,Dx
                   //      b)    -(Ay),-(Ax)

                   checkshort()
                   nextsymb()

                   evaluate( effective.address() )
                   checkfor( s.comma, 10 )

                   r.m  :=  op.ea  =  am.Dr     ->  0,
                            op.ea  =  am.Ar.pd  ->  1,
                                      complain( 8 )

                   swapoperands()

                   evaluate( effective.address() )

                   TEST  op.ea = op1.ea  THEN
                   $(
                       checkfor( s.none, 12 )

                       codeword(  instr.mask                          |
                                  (exp << 9)                          |
                                  (sizefield( instr.size ) << 6)      |
                                  (r.m << 3)                          |
                                  (op1.exp)                           )
                   $)
                   ELSE  complain( 9 )

                   ENDCASE


        CASE  2 :  //  AND  OR
                   //  Possible operand types:
                   //
                   //  <ea>,Dn                  <ea> = data.
                   //  Dn,<ea>                  <ea> = mem  alt.

                   checkshort()
                   nextsymb()

                   evaluate( effective.address() )
                   checkfor( s.comma, 10 )
                   swapoperands()

                   evaluate( effective.address() )
                   checkfor( s.none, 12 )

                   TEST  op.ea = am.Dr  THEN
                   $(
                       r.m  :=  0

                       TEST  (op1.ea & am.data) = 0
                           THEN  complain( 13 )
                           ELSE  swapoperands()
                   $)
                   ELSE

                   TEST  op1.ea \= am.Dr           THEN  complain( 14 )   ELSE
                   TEST  (op.ea & am.mem.alt) = 0  THEN  complain( 15 )

                   ELSE  r.m  :=  1

                   codeword(  instr.mask                                  |
                              (op1.exp  <<  9)                            |
                              (r.m      <<  8)                            |
                              (sizefield( instr.size )  <<  6)            |
                              (eafield() )                                )

                   genea()

                   ENDCASE


        CASE  3 :  //  ADD  SUB
                   //  Possible operands are:
                   //
                   //       a)    <ea>,Dr                    <ea> = all
                   //       b)    Dr,<ea>                    <ea> = mem alt.

                   checkshort()
                   nextsymb()

                   evaluate( effective.address() )
                   checkfor( s.comma, 10 )
                   swapoperands()

                   evaluate( effective.address() )
                   checkfor( s.none, 12 )

                   TEST  op.ea = am.Dr THEN
                   $(
                       r.m  :=  0

                       IF  (op1.ea & am.all) = 0  THEN  complain( 8 )

                       swapoperands()
                   $)
                   ELSE
                   $(
                       r.m  :=  1

                       TEST  op1.ea \= am.Dr  THEN  complain( 14 )
                       ELSE

                           IF  (op.ea & am.mem.alt) = 0  THEN  complain( 15 )
                   $)

                   codeword(  instr.mask                                    |
                              (op1.exp  <<  9)                              |
                              (r.m      <<  8)                              |
                              (sizefield( instr.size ) << 6)                |
                              (eafield())                                   )

                   genea()

                   ENDCASE


        CASE  4 :  //  Bcc  DBcc  BRA   BSR   DBRA
                   //  The condition codes required for this instruction is
                   //  in fact held in "source.ea"

                   nextsymb()

                   IF  nargs = 2  THEN     // A DB.. type instruction
                   $(
                       checkshort()
                       evaluate( effective.address() )
                       checkfor( s.comma, 10 )

                       IF  op.ea \= am.Dr  THEN  complain( 14 )

                       swapoperands()
                   $)

                   //  We must allow for the possibility of a forward
                   //  reference to a 32 bit value.  This is illegal in most
                   //  cases, but not here.

                   ofwd            :=  forwardreftype
                   forwardreftype  :=  s.abs32

                   evaluate( effective.address() )

                   forwardreftype  :=  ofwd

                   //  Having read the effective address, make sure that it
                   //  is compatible with the current program counter.

                   checkfor( s.none, 12 )

                   //  We cannot allow external references in Branch
                   //  instructions (as we require a displacement).

                   IF  externalref  THEN  complain( 152 )

                   TEST  op.ea = am.PC.disp   THEN
                       UNLESS  locmode = s.rel  DO  complain( 18 )

                   ELSE

                   TEST  (op.ea = am.abs16 | op.ea = am.abs32)    THEN
                       UNLESS  pass1  DO
                           UNLESS  locmode = s.abs  DO  complain( 17 )

                   ELSE  complain( 16 )

                   //  We can perform an optimisation here, if the reference is
                   //  backward, and we are assembling a Bcc instruction, and
                   //  the user has not asked for a LONG branch explicitly.

                   IF  nargs = 1  &  instr.size = ts.none  THEN
                   $(
                       LET offset   =  exp - (location + 2)
                       LET inrange  =  -128 <= offset <= +127

                       IF  inrange  &  offset \= 0  &  NOT forwardref  THEN
                           instr.size  :=  ts.short
                   $)

                   TEST  instr.size = ts.short  THEN
                   $(
                       LET offset  =  exp - (location + 2)

                       IF  offset = 0  &  pass2  THEN  complain( 19 )

                       codeword(  instr.mask                           |
                                  (source.ea  <<  8)                   |
                                  (offset  &  #XFF)                    )

                       UNLESS  (-128 <= offset <= +127)  |  pass1  DO
                           complain( 20 )
                   $)
                   ELSE
                   $(
                       //  Long branch, either because this is a forward
                       //  reference, or because the user asked for it
                       //  explicitly using a ".L" size specifier.

                       LET offset  =  exp - (location + 2)

                       TEST  (-32768 <= offset <= +32767)  |  pass1  THEN
                       $(
                           codeword(  instr.mask                             |
                                      (source.ea  <<  8)                     |
                                      (nargs = 2  ->  op1.exp, #B000)        )

                           codeword(  offset  &  #XFFFF                      )
                       $)
                       ELSE  complain( 21 )
                   $)

                   ENDCASE


        CASE  5  : //  ASL  ASR  LSL  LSR   ROL  ROR  ROXL  ROXR
                   //  The identifier for the particular instruction
                   //  is again held in "source.ea"
                   //
                   //  There are various types of addressing modes:
                   //
                   //     a)    Dr,Dr
                   //     b) #imm3,Dr
                   //     c)    <ea>                  mem alt.


                   checkshort()
                   nextsymb()

                   evaluate( effective.address() )

                   TEST  op.ea = am.Dr  |  (op.ea & am.imm3) \= 0  THEN
                   $(
                       LET r    =  exp & #B111
                       LET i.r  =  op.ea = am.Dr  ->  1, 0
                       LET dr   =  source.ea & 1

                       checkfor( s.comma, 10 )

                       swapoperands()

                       evaluate( effective.address() )
                       checkfor( s.none, 12 )

                       TEST  op.ea = am.Dr  THEN
                             codeword(  instr.mask                           |
                                        (r  <<  9)                           |
                                        (dr <<  8)                           |
                                        (sizefield( instr.size )  << 6)      |
                                        (i.r << 5)                           |
                                        (exp)                                )

                       ELSE  complain( 22 )
                   $)
                   ELSE

                   TEST  (op.ea & am.mem.alt) \= 0  THEN
                   $(
                       LET mask  =  source.ea!(TABLE   #XE0C0, #XE0C0,
                                                       #XE2C0, #XE2C0,
                                                       #XE6C0, #XE6C0,
                                                       #XE4C0, #XE4C0)

                       LET dr    =  source.ea & 1

                       checkfor( s.none, 12 )

                       codeword(  mask                                      |
                                  (dr  <<  8)                               |
                                  (eafield() )                              )

                       genea()
                   $)
                   ELSE  complain( 8 )

                   ENDCASE


        CASE  6  : //  BCHG  BCLR  BSET  BTST
                   //  Possible operands are:
                   //
                   //     a)     Dr,<ea>                          data alt.
                   //     b)   #imm,<ea>                          data alt.
                   //                                             (data if BTST)

                   checkshort()
                   nextsymb()

                   evaluate( effective.address() )
                   checkfor( s.comma, 10 )

                   swapoperands()

                   TEST  op1.ea = am.Dr  THEN
                   $(
                       LET btst  =  source.ea = #B00

                       evaluate( effective.address() )
                       checkfor( s.none, 12 )

                       TEST  (op.ea & (btst  ->  am.data, am.data.alt) = 0)  THEN
                           complain( btst  ->  33, 23 )

                       ELSE
                       $(
                           codeword(  #X0100                                 |
                                      (op1.exp  <<  9)                       |
                                      (source.ea << 6)                       |
                                      (eafield() )                           )
                           genea()
                       $)
                   $)
                   ELSE

                   TEST  (op1.ea & am.imm16) \= 0  THEN
                   $(
                       evaluate( effective.address() )
                       checkfor( s.none, 12 )

                       TEST  (op.ea & am.data.alt) = 0  THEN  complain( 23 )
                       ELSE
                       $(
                           codeword(  #X0800                                 |
                                      (source.ea  <<  6)                     |
                                      (eafield() )                           )

                           codeword( op1.exp & #XFF )
                           genea()
                       $)
                   $)
                   ELSE  complain( 8 )

                   ENDCASE


        CASE  7  : //  EXG  EXGA  EXGM  EXGD
                   //  Possible operands are <register>,<register>,
                   //  but various restrictions are placed depending on
                   //  then mnemonic used.

                   checkshort()

                   UNLESS  (source.ea & #B01) = 0  DO  regs := regs | am.Dr
                   UNLESS  (source.ea & #B10) = 0  DO  regs := regs | am.Ar

                   nextsymb()

                   evaluate( effective.address() )
                   checkfor( s.comma, 10 )

                   IF  (op.ea & regs) = 0  THEN  complain( 24 )

                   swapoperands()

                   evaluate( effective.address() )
                   checkfor( s.none, 12 )

                   IF  (op.ea & regs) = 0  THEN  complain( 24 )

                   opmode   :=  op.ea  =  op1.ea  ->  (op.ea = am.Dr -> #B01000,
                                                                        #B01001),
                                                                        #B10001

                   UNLESS  op.ea = op1.ea  DO
                       IF  op1.ea = am.Ar  THEN
                           swapoperands()

                   codeword(  instr.mask                                |
                              (op1.exp  <<  9)                          |
                              (opmode   <<  3)                          |
                              (exp)                                     )

                   ENDCASE


        CASE  8  : //  All the MOVE instructions.   These require MUCH more
                   //  decoding...
                   //  The values of "source.ea" represent:
                   //
                   //       0   -   MOVE
                   //       1   -   MOVEA
                   //       2   -   MOVEM
                   //       3   -   MOVEP
                   //       4   -   MOVEQ

                   checkshort()

                   SWITCHON  source.ea  INTO
                   $(
                       CASE  0 :  genmove()                  ; ENDCASE
                       CASE  1 :  genmovea()                 ; ENDCASE

                       CASE  2 :  in.movem  :=  yes
                                  genmovem()
                                  in.movem  :=  no           ; ENDCASE

                       CASE  3 :  genmovep()                 ; ENDCASE
                       CASE  4 :  genmoveq()                 ; ENDCASE


                       DEFAULT :  complain( 0 )
                   $)

                   ENDCASE


        CASE  9  : //  JMP  JSR
                   //  These are perfectly innocuous instructions, but Motorola
                   //  in their infinite wisdon have decided that they should
                   //  have ".S" and ".L" addressing modes, just like BRA.

                   nextsymb()
                   evaluate( effective.address() )

                   checkfor( s.none, 12 )

                   IF  (op.ea & am.contr) = 0  THEN  complain( 180 )

                   //  Ok.  We have something which is approximately the
                   //  right shape.  We should now look at the instruction
                   //  size to make sure that everything matches.

                   TEST  instr.size = ts.none  THEN
                   $(
                       //  No size given, so this is the same instruction as
                       //  before.

                       codeword(  instr.mask     |
                                  eafield()      )

                       genea()
                   $)
                   ELSE
                   $(
                       //  More tricky.  This instruction has a specific size,
                       //  and so we should check that all is well.

                       LET ea    =  0
                       LET size  =  0
                       LET type  =  0

                       UNLESS  op.ea = am.abs16    |
                               op.ea = am.abs32    |
                               op.ea = am.PC.disp  DO  complain( 181 )

                       TEST  instr.size = ts.short  THEN
                       $(
                           //  Short addressing mode.  This is OK, providing
                           //  that we have not got a 32 bit absolute argument!

                           IF  op.ea = am.abs32  THEN  complain( 182 )

                           ea     :=  op.ea
                           size   :=  bytesize( ts.word )
                           type   :=  s.abs16
                           op.ea  :=  am.abs16
                       $)
                       ELSE
                       $(
                           //  Long addressing mode.  Everything can be fitted
                           //  into this.

                           ea     :=  op.ea
                           size   :=  bytesize( ts.long )
                           type   :=  s.abs32
                           op.ea  :=  am.abs32
                       $)

                       IF  pass1 & forwardref  THEN  relocate( 0, size )

                       //  Having worked out what the changes should be, we can
                       //  generate the opcode word, and then stack the address
                       //  we have calculated.

                       codeword(  instr.mask     |
                                  eafield()      )

                       TEST  ea = am.PC.disp
                           THEN  stackvalue( s.rel, size, exp, externalref, externalsymb )
                           ELSE  stackvalue( type,  size, exp, externalref, externalsymb )
                   $)

                   ENDCASE


        CASE 11  : //  TRAP
                   //  Operand type:    #imm  (4 bit)

                   checkshort()
                   nextsymb()

                   evaluate( effective.address() )
                   checkfor( s.none, 12 )

                   TEST  (op.ea & am.imm) = 0     THEN  complain( 25 )   ELSE
                   TEST  NOT (0 <= exp <= 15)     THEN  complain( 26 )

                   ELSE  codeword( instr.mask  |  exp )

                   ENDCASE


        CASE 12  : //  ANDI   EORI   ORI
                   //  These are special, because the destination operand
                   //  may be the CCR or SR.  A ".B" size is implied
                   //  for the CCR and a ".W" is implied for the SR.
                   //  ".L" sizes are flagged as errors.

                   checkshort()
                   nextsymb()

                   evaluate( effective.address() )
                   checkfor( s.comma, 10 )

                   IF  (op.ea & am.imm) = 0  THEN  complain( 27 )

                   swapoperands()

                   evaluate( effective.address() )
                   checkfor( s.none, 12 )

                   TEST  op.ea = am.special  THEN
                   $(
                       LET size  =  exp = s.SR   ->  ts.word,
                                    exp = s.CCR  ->  ts.byte,
                                                     complain( 9 )

                       UNLESS  instr.size = ts.none  DO
                           UNLESS  instr.size = size  DO
                               complain( 28 )

                       codeword(  instr.mask                            |
                                  (sizefield( size )  <<  6)            |
                                  (#B111100)                            )

                       codeword(  op1.exp )
                   $)
                   ELSE

                   TEST  (op.ea & am.data.alt) = 0  THEN  complain( 23 )

                   ELSE
                   $(
                       codeword(  instr.mask                          |
                                  (sizefield( instr.size )  <<  6)    |
                                  (eafield() )                        )

                       IF  instr.size = ts.long  THEN
                           codeword( op1.exp >> 16 )

                       TEST  instr.size = ts.byte
                             THEN  codeword( op1.exp & #XFF )
                             ELSE  codeword( op1.exp & #XFFFF )

                       genea()
                   $)

                   ENDCASE



        CASE 13  : //  ADDA, SUBA, CMPA
                   //  Must check, as BYTE mode is not allowed, and this
                   //  Is banked on on the bit pattern.

                   checkshort()

                   sz  :=  instr.size = ts.word  ->  0,
                           instr.size = ts.none  ->  0,
                           instr.size = ts.long  ->  1,
                           instr.size = ts.byte  ->  complain( 29 ),
                                                     complain( 6 )

                   nextsymb()

                   evaluate( effective.address() )
                   checkfor( s.comma, 10 )

                   TEST  (op.ea & am.all) = 0  THEN  complain( 8 )
                   ELSE
                   $(
                       swapoperands()

                       evaluate( effective.address() )
                       checkfor( s.none, 12 )

                       TEST  op.ea \= am.Ar  THEN  complain( 30 )
                       ELSE
                       $(
                           swapoperands()
                           codeword(  instr.mask                       |
                                      (op1.exp  <<  9)                 |
                                      (sz       <<  8)                 |
                                      (eafield() )                     )

                           genea()
                       $)
                   $)

                   ENDCASE


        CASE 14  : //  CMPM
                   //  A silly instruction if ever there was one!
                   //  The operand types are:
                   //
                   //    (Ay)+,(Ax)+

                   checkshort()
                   nextsymb()

                   evaluate( effective.address() )
                   checkfor( s.comma, 10 )

                   IF  op.ea  \=  am.Ar.pi  THEN   complain( 31 )

                   swapoperands()

                   evaluate( effective.address() )
                   checkfor( s.none, 12 )

                   IF  op.ea  \=  am.Ar.pi  THEN  complain( 32 )

                   codeword(  instr.mask                                 |
                              (exp  <<  9)                               |
                              (sizefield( instr.size )  <<  6)           |
                              (op1.exp)                                  )

                   ENDCASE


        CASE 15  : //  EOR
                   //  Operands are:   Dr,<ea>
                   //  where <ea> is data alterable.

                   checkshort()
                   nextsymb()

                   evaluate( effective.address() )
                   checkfor( s.comma, 10 )

                   IF  op.ea \= am.Dr  THEN  complain( 14 )

                   swapoperands()

                   evaluate( effective.address() )
                   checkfor( s.none, 12 )

                   IF  (op.ea & am.data.alt) = 0  THEN  complain( 23 )

                   codeword( instr.mask                     |
                             (op1.exp << 9)                 |
                             (sizefield( instr.size ) << 6) |
                             (eafield())                    )

                   genea()

                   ENDCASE


        DEFAULT  : complain( 0 )
    $)

    skiprest()
$)



AND checkshort()  BE  IF  instr.size = ts.short  THEN  complain( 86 )



AND genmove()  BE
$(
//  Generate code for a general MOVE instruction.
//  This can take quite a few forms,  viz:
//
//      a)    <ea>,<ea>               all,data alt        BWL
//      b)    <ea>,CCR                data                .W.
//      c)    <ea>,SR                 data                .W.
//      d)    SR,<ea>                 data alt            .W.
//      e)    USP,Ar                                      ..L
//      f)    Ar,USP                                      ..L

    nextsymb()

    evaluate( effective.address() )
    checkfor( s.comma, 10 )

    swapoperands()

    evaluate( effective.address() )
    checkfor( s.none, 12 )

    //  The   ...,CCR  and  ...,SR  can be picked out easily.

    IF  op.ea = am.special  THEN
    $(
        LET size  =  exp = s.SR  ->  ts.word,
                     exp = s.CCR ->  ts.byte,
                                           -1

        UNLESS  size = -1  DO
        $(
            TEST  (op1.ea & am.data) = 0  THEN  complain( 13 )
            ELSE
            $(
                UNLESS  instr.size = ts.none  DO
                        UNLESS  instr.size = ts.word  DO  complain( 28 )

                swapoperands()

                codeword(  #X44C0                              |
                           (sizefield( size ) << 9)            |
                           (eafield())                         )
                genea()

                RETURN
            $)
        $)
    $)

    //  Next, the move FROM SR instruction.

    IF  op1.ea = am.special  THEN
    $(
        IF  op1.exp = s.SR  THEN
        $(
            UNLESS  instr.size = ts.word | instr.size = ts.none  DO  complain( 34 )

            TEST  (op.ea & am.data.alt) = 0  THEN  complain( 23 )
            ELSE
            $(
                codeword(  #X40C0                            |
                           (eafield() )                      )
                genea()

                RETURN
            $)
        $)
    $)

    //  Now the Ar,USP and USP,Ar instructions.

    IF    (op1.ea = am.special & op1.exp = s.USP) |
          (op.ea  = am.special & exp     = s.USP)    THEN
    $(
        LET dr  =  op1.ea = am.special  ->  1, 0

        UNLESS  instr.size = ts.long  |  instr.size = ts.none  DO  complain( 35 )

        IF  op.ea  =  am.special  THEN  swapoperands()

        TEST  op.ea = am.Ar  THEN
              codeword(  #X4E60                               |
                         (dr  <<  3)                          |
                         (exp)                                )

        ELSE  complain( dr = 1  ->  30, 36 )

        RETURN
    $)

    //  This leaves  the good old MOVE <ea>,<ea> instruction!

    TEST  (op1.ea & am.all) = 0      THEN  complain( 8 )                ELSE
    TEST  (op.ea & am.alt) = 0       THEN  complain( 99 )               ELSE

          generalmove()
$)



AND genmovea()  BE
$(
//  Move address into address register
//  First, read in the operands:

    nextsymb()

    evaluate( effective.address() )
    checkfor( s.comma, 10 )

    swapoperands()

    evaluate( effective.address() )
    checkfor( s.none, 12 )

    TEST  (op1.ea & am.all) = 0     THEN  complain( 8 )                ELSE
    TEST  (op.ea \= am.Ar)          THEN  complain( 30 )               ELSE

          generalmove()
$)



AND generalmove()  BE
$(
    LET operand1  =  0
    LET operand2  =  0

    swapoperands()

    operand2  :=  eafield()

    swapoperands()

    operand1  :=  eafield()
    operand1  :=  ((operand1 << 3) | (operand1 >> 3))  &  #X3F

    codeword(  #X0000                                   |
               (movesize( instr.size )  <<  12)         |
               (operand1  <<  6)                        |
               (operand2)                               )

    swapoperands()
    genea()

    swapoperands()
    genea()
$)



AND movesize( size )  =  VALOF
$(
    SWITCHON  size  INTO
    $(
        CASE ts.byte     :  RESULTIS  #B01
        CASE ts.word     :  RESULTIS  #B11
        CASE ts.long     :  RESULTIS  #B10
        CASE ts.none     :  RESULTIS  movesize( ts.default )

        DEFAULT          :  complain( 37 )
    $)
$)



AND genmovem()  BE
$(
    LET dr     =  0
    LET sz     =  0
    LET rbits  =  0

    nextsymb()

    TEST  symb = s.Ar  |  symb = s.Dr  |  symb = s.reg  THEN
    $(
        LET bits  =  readregisters()

        checkfor( s.comma, 10 )

        evaluate( effective.address() )
        checkfor( s.none, 12 )

        TEST  (op.ea & am.contr.alt) \= 0  THEN  rbits  :=  bits    ELSE
        TEST  (op.ea & am.Ar.pd) \= 0      THEN  rbits  :=  reverse( bits )

              ELSE  complain( 9 )

        dr  :=  0
    $)
    ELSE
    $(
        LET bits  =  0

        evaluate( effective.address() )
        checkfor( s.comma, 10 )

        bits  :=  readregisters()

        checkfor( s.none, 12 )

        TEST  (op.ea & am.contr)  \=  0         THEN  rbits  :=  bits  ELSE
        TEST  (op.ea & am.Ar.pi)  \=  0         THEN  rbits  :=  bits  ELSE

              complain( 8 )

        dr  :=  1
    $)

    sz  :=  instr.size  =  ts.long   ->  1,
            instr.size  =  ts.word   ->  0,
            instr.size  =  ts.none   ->  0,
                                         complain( 38 )

    codeword(  #X4880                                                  |
               (dr  <<  10)                                            |
               (sz  <<  6)                                             |
               (eafield() )                                            )

    codeword(  rbits  &  #XFFFF )

    genea()
$)



AND reverse( bits )  =  VALOF
$(
    LET newbits  =  0

    FOR  i = 1  TO  16  DO
    $(
        newbits  :=  (newbits << 1)  +  (bits & 1)
        bits     :=  bits >> 1
    $)

    RESULTIS newbits
$)




AND evalm( ptr )  =  VALOF
$(
    LET ptr0    =  ptr!p.ptr0
    LET ptr1    =  ptr!p.ptr1
    LET ptr2    =  ptr!p.ptr2

    LET r1      =  0
    LET r2      =  0
    LET rtype1  =  0
    LET rtype2  =  0
    LET rnum1   =  0
    LET rnum2   =  0

    SWITCHON  ptr0  INTO
    $(
        CASE s.Ar     : RESULTIS  #B0000000100000000  <<  ptr1
        CASE s.Dr     : RESULTIS  #B0000000000000001  <<  ptr1

        CASE s.slash  : RESULTIS  evalm( ptr1 )  |  evalm( ptr2 )

        CASE s.hyphen : r1      :=  evalm( ptr1 )
                        r2      :=  evalm( ptr2 )

                        rtype1  :=  ptr1!p.ptr0
                        rtype2  :=  ptr2!p.ptr0
                        rnum1   :=  ptr1!p.ptr1
                        rnum2   :=  ptr2!p.ptr1

                        TEST  rtype1 = rtype2  THEN
                        $(
                            LET result  =  0

                            IF  rnum2 < rnum1  THEN
                            $(
                                LET t  =  r1

                                r1  :=  r2
                                r2  :=  t
                            $)

                            result  :=  r1

                            UNTIL  r1 = r2  DO
                            $(
                                r1      :=  r1 << 1
                                result  :=  result | r1
                            $)

                            RESULTIS  result
                        $)
                        ELSE  complain( 39 )

                        RESULTIS 0


        DEFAULT       : complain( 0 )
    $)
$)



AND readregisters()  =  VALOF
$(
//  Read either a single REG type symbol, or a list of registers separated
//  by "-" or "/".

    TEST  symb = s.reg  THEN
    $(
        //  The current symbol has exactly the right shape, and so we should
        //  remember its value, and read the next symbol.  Check here for
        //  a forward reference (which is illegal).

        LET bits  =  symbtype!st.value

        IF  (symbtype!st.flags & stb.setnow) = 0  THEN  complain( 183 )

        readsymb()

        RESULTIS  bits
    $)
    ELSE

    //  Not a special symbol, so we should read the register mask now, and
    //  return the corresponding bit pattern.

    RESULTIS  evalm( readmult() )
$)



AND readmult()  =  VALOF
$(
    LET result  =  readreg()

    $(  //  Repeat loop to read a list of registers separated by either
        //  hyphens or slashes.

        TEST  symb = s.over    THEN
        $(
            readsymb()

            RESULTIS  block3( s.slash, result, readmult() )
        $)
        ELSE

        TEST  symb = s.minus    THEN
        $(
            readsymb()

            result  :=  block3( s.hyphen, result, readreg() )

            IF  symb = s.over  THEN  LOOP

            RESULTIS  result

        $)

        ELSE  RESULTIS  result
    $)
    REPEAT
$)



AND readreg()  =  VALOF
$(
//  Read a register definition.  Since we are not going through the normal
//  evaluation channels, we should check to see that we are not using a
//  register which was defined using a forward reference.

    TEST  symb = s.Ar  |  symb = s.Dr  THEN
    $(
        IF  (symbtype!st.flags & stb.setnow) = 0  THEN  complain( 148 )

        TEST  tagsize.given  \=  ts.none  THEN  complain( 40 )
        ELSE
        $(
            LET result  =  block3( symb, regnum, 0 )

            readsymb()

            RESULTIS  result
        $)
    $)
    ELSE  complain( 41 )

    RESULTIS  0
$)



AND genmovep()  BE
$(
//  The possible address modes allowed are:
//
//    d(Ay),Dx
//    Dx,d(Ay)

    LET dr  =  0
    LET sz  =  0

    nextsymb()

    evaluate( effective.address() )
    checkfor( s.comma, 10 )

    swapoperands()

    evaluate( effective.address() )
    checkfor( s.none, 12 )

    TEST  op1.ea = am.Dr  THEN

          TEST  op.ea = am.Ar.disp  THEN
          $(
              dr  :=  1

              swapoperands()
          $)
          ELSE  complain( 42 )

    ELSE

    TEST  op1.ea \= am.Ar.disp      THEN   complain( 43 )            ELSE
    TEST  op.ea  \= am.Dr           THEN   complain( 22 )            ELSE

          dr  :=  0

    sz  :=  instr.size  =  ts.long        ->  1,
            instr.size  =  ts.word        ->  0,
            instr.size  =  ts.none        ->  0,
                                              complain( 44 )

    codeword(  #X0108                          |
               (exp   <<  9)                   |
               (dr    <<  7)                   |
               (sz    <<  6)                   |
               (op1.registers!p.ptr1)          )

    codeword( op1.exp & #XFFFF )
$)



AND genmoveq()  BE
$(
//  The Ubiquitous MOVEQ instruction.
//  The possible operands are:
//
//    #imm,Dr

    nextsymb()

    evaluate( effective.address() )
    checkfor( s.comma, 10 )

    IF  (op.ea & am.imm) = 0  THEN  complain( 27 )

    swapoperands()

    evaluate( effective.address() )
    checkfor( s.none, 12 )

    UNLESS  op.ea = am.Dr  DO  complain( 22 )

    UNLESS  -128 <= op1.exp <= +127  DO  complain( 45 )

    codeword(  #X7000                                   |
               (exp  <<  9)                             |
               (op1.exp  &  #XFF)                       )
$)



AND dodir()  BE
$(
//  Deal with the assembler directives.

    LET restype  =  0
    LET ressize  =  0
    LET skip     =  0
    LET tempvec  =  VEC 256/bytesperword

    instr.size  :=  tagsize.given
    directive   :=  symbtype!st.value

    UNLESS  directive = d.org     |
            directive = d.dc      |
            directive = d.dcb     |
//          directive = d.size    |
            directive = d.ds      DO  checktagsize()

    UNLESS  directive = d.org     DO  checkshort()

    SWITCHON  directive  INTO
    $(
        CASE d.set    :
        CASE d.equ    :
        CASE d.equr   :
        CASE d.reg    : checklabel( yes )
                        nextsymb()

                        TEST  directive = d.reg  THEN
                        $(
                            //  This does not take a simple expression, but
                            //  a set of registers.

                            restype  :=  s.reg
                            value    :=  readregisters()
                        $)
                        ELSE
                        $(
                            //  Just a simple expression, so we can read it
                            //  and look at its data type.

                            restype  :=  evalexp( expression() )

                            checkexpression( restype, TRUE )
                        $)

                        setlabel( restype, value,  directive = d.set )

                        IF  pass2  &  (listing | error.found)  THEN
                        $(
                            clearbuffer()

                            linepos  :=  10
                            writechar( directive = d.set  ->  '>', '=' )

                            linepos  :=  11

                            IF  restype = s.Dr   THEN  writechar( 'D' )
                            IF  restype = s.Ar   THEN  writechar( 'A' )

                            writehexvalue( value, (restype = s.abs32  ->  8,
                                                   restype = s.Ar  |
                                                   restype = s.Dr     ->  1,
                                         /* s.rel, s.abs16 or s.reg */    4) )

                            IF  restype = s.rel  THEN  writechar( '*'' )
                            IF  restype = s.reg  THEN  writechar( 'R' )

                            IF  error.found  THEN
                            $(
                                linepos  :=  35
                                writechar( 'E' )

                                error.found  :=  no
                            $)

                            linepos  :=  38
                            writenumber( linenumber, 5 )

                            IF  macrodepth > 0  &  NOT inmacro  THEN
                            $(
                                linepos  :=  43
                                writechar( '+' )
                            $)

                            linepos  :=  44
                            FOR  i = 0  TO length-1  DO  writechar( inputbuff % i )

                            printbuffer()

                            listed  :=  yes
                        $)

                        ENDCASE


        CASE d.org    : ressize  :=  VALOF
                            SWITCHON  instr.size  INTO
                            $(
                                CASE ts.long    : RESULTIS s.abs32

                                CASE ts.none    :
                                CASE ts.short   :
                                CASE ts.word    : RESULTIS s.abs16

                                DEFAULT         : complain( 46 )
                            $)

                        nextsymb()
                        restype  :=  evalexp( expression() )

                        checkexpression( restype, TRUE )

                        UNLESS  (value & addressmask) = 0  DO  complain( 138 )

                        changemode( s.abs )

                        forwardreftype  :=  ressize

                        setloc( value )

                        IF  labelset  THEN  setlabel( locmode, location, no )

                        ENDCASE


        CASE d.rorg   : nextsymb()

                        checkexpression( evalexp( expression() ), TRUE )
                        changemode( s.rel )

                        forwardreftype  :=  s.abs16

                        setloc( value )

                        IF  labelset  THEN  setlabel( locmode, location, no )

                        ENDCASE


        CASE d.dc     : defineconstants( instr.size )

                        ENDCASE


        CASE d.ds     : definestorage( instr.size )

                        ENDCASE


        CASE d.dcb    : defineblock( instr.size )

                        ENDCASE


        CASE d.list   : checklabel( no )
                        readsymb()

                        checkfor( s.none, 47 )

                        listing  :=  parmlisting
                        listed   :=  yes

                        ENDCASE


        CASE d.nolist : checklabel( no )
                        readsymb()

                        checkfor( s.none, 47 )

                        listing  :=  no
                        listed   :=  yes

                        ENDCASE


        CASE d.spc    : checklabel( no )
                        nextsymb()

                        checkexpression( evalexp( expression() ), TRUE )
                        spacelines( value )

                        listed  :=  yes

                        ENDCASE


        CASE d.page   : checklabel( no )
                        readsymb()

                        checkfor( s.none, 47 )

                        onpage  :=  0
                        listed  :=  yes

                        ENDCASE


        CASE d.nopage : checklabel( no )
                        readsymb()

                        checkfor( s.none, 47 )

                        paging       :=  no
                        commentline  :=  yes

                        ENDCASE


        CASE d.plen   :
        CASE d.llen   : checklabel( no )
                        nextsymb()

                        checkexpression( evalexp( expression() ), TRUE )

                        TEST  directive = d.plen  THEN
                              TEST  plenfixed   THEN  complain( 124 )
                              ELSE

                              TEST  minplen <= value <= maxplen  THEN
                              $(
                                  linesperpage  :=  value
                                  plenfixed     :=  yes
                              $)
                              ELSE  complain( 100 )

                        ELSE  TEST  llenfixed  THEN  complain( 125 )
                              ELSE

                              TEST  minllen <= value <= maxllen  THEN
                              $(
                                  charsperline  :=  value
                                  llenfixed     :=  yes
                              $)
                              ELSE  complain( 101 )

                        listed  :=  yes

                        ENDCASE


        CASE d.ttl    : checklabel( no )
                        skiplayout()

                        ressize  :=  1

                        UNTIL  ressize > titlecharsmax  |  ch = '*N'  DO
                        $(
                            tempvec % ressize  :=  ch
                            ressize            :=  ressize + 1

                            rch()
                        $)

                        tempvec % 0  :=  ressize - 1

                        UNLESS  ch = '*N'  DO   warning( 49 )

                        settitle( tempvec )

                        listed  :=  yes

                        ENDCASE


        CASE d.noobj  : checklabel( no )
                        readsymb()

                        checkfor( s.none, 47 )

                        noobj        :=  yes
                        commentline  :=  yes

                        ENDCASE


        CASE d.ifeq   :
        CASE d.ifne   :
        CASE d.iflt   :
        CASE d.ifle   :
        CASE d.ifgt   :
        CASE d.ifge   : checklabel( no )

                        //  There are two possibilities here.  There is:
                        //
                        //          IFxx  <expression>
                        //      or  IFxx  '<string1>','<string2>'
                        //
                        //  We must look at the first character of the operand
                        //  to see which case it is.

                        skiplayout()

                        TEST  ch = '*''
                            THEN  skip  :=  do.ifstrings()
                            ELSE  skip  :=  do.ifvalue()

                        skipping  :=  skipping + 1

                        UNLESS  skiplevel > 0  DO
                            IF  skip  THEN
                                skiplevel  :=  skipping

                        IF  pass2  &  (listing | error.found)  THEN
                        $(
                            clearbuffer()

                            linepos  :=  10
                            writechar( '=' )

                            writehexvalue( value, 8 )

                            IF  error.found  THEN
                            $(
                                linepos  :=  35
                                writechar( 'E' )
                            $)

                            linepos  :=  38
                            writenumber( linenumber, 5 )

                            IF  macrodepth > 0  &  NOT inmacro  THEN
                            $(
                                linepos  :=  43
                                writechar( '+' )
                            $)

                            linepos  :=  44
                            FOR  i = 0  TO  length-1  DO  writechar( inputbuff % i )

                            printbuffer()

                            listed  :=  yes
                        $)

                        ENDCASE



        CASE d.endc   : checklabel( no )
                        readsymb()

                        TEST  symb \= s.none  THEN  complain( 47 )        ELSE
                        TEST  skipping = 0    THEN  complain( 107 )       ELSE

                              skipping  :=  skipping - 1

                        IF  skipping < skiplevel  THEN  skiplevel  :=  0

                        commentline  :=  yes

                        ENDCASE


        CASE d.macro  : checklabel( yes )
                        readsymb()

                        checkfor( s.none, 47 )

                        IF  inmacro  THEN   complain( 110 )

                        IF  macrodepth > 0  THEN  complain( 121 )

                        FOR  i = 0  TO  tagsize-1  DO  macroname!i  :=  labelvec!i

                        macrobase    :=  pass1  ->  heap3( 0, 0, 0 ),  0
                        macroend     :=  macrobase
                        macrodepth   :=  macrodepth + 1
                        inmacro      :=  yes

                        commentline  :=  yes

                        ENDCASE


        CASE d.endm   : checklabel( no )
                        readsymb()

                        checkfor( s.none, 47 )

                        TEST  inmacro
                            THEN  macrodepth  :=  macrodepth - 1
                            ELSE  complain( macrodepth = 0  ->  111, 120 )

                        FOR  i =  0  TO  tagsize-1  DO  labelvec!i  :=  macroname!i

                        inmacro  :=  no

                        setlabel( s.macro, macrobase, no )

                        commentline  :=  yes

                        ENDCASE


        CASE d.mexit  : checklabel( no )
                        readsymb()

                        checkfor( s.none, 47 )

                        TEST  macrodepth = 0
                            THEN  complain( 112 )
                            ELSE  macrodepth  :=  macrodepth - 1

                        listed  :=  yes

                        ENDCASE


//      CASE  d.size  : //  this is a non-standard feature, and hence if it
//                      //  does not work, nothing will be done to fix it.
//
//                      checklabel( no )
//                      readsymb()
//
//                      TEST  symb = s.none  THEN
//                          TEST  tagsize.given = ts.none  |  tagsize.given = ts.short  THEN
//                                complain( 98 )
//                          ELSE  ts.default  :=  tagsize.given
//
//                      ELSE  complain( 47 )
//                      commentline  :=  yes
//                      ENDCASE



        CASE d.end    : IF  labelset  THEN  setlabel( locmode, location, no )
                        readsymb()

                        checkfor( s.none, 47 )

                        IF  macrodepth > 0  THEN  complain( 119 )
                        IF  getlevel   > 0  THEN  complain( 126 )

                        ended        :=  yes
                        commentline  :=  NOT  labelset

                        ENDCASE


        CASE d.get    : checklabel( no )
                        skiplayout()

                        TEST  ch = '*''  |  ch = '*"'  THEN
                        $(
                            LET quote  =  ch

                            rch()
                            ressize  :=  0

                            UNTIL  ch = quote  |  ch = '*N'  DO
                            $(
                                ressize            :=  ressize + 1
                                tempvec % ressize  :=  ch

                                rch()
                            $)

                            IF  ch = '*N'  THEN  complain( 127 )

                            //  The following line used to be in the assembler,
                            //  but for no readily apparent reason.  It should
                            //  be reinstated if the (now forgotten) problem
                            //  comes to light again!
                            //
                            //  IF  macrodepth > 0  THEN  complain( 133 )

                            tempvec % 0  :=  ressize

                            rch()
                            readsymb()
                            checkfor( s.none, 47 )

$<370
                            UNLESS  doget( tempvec )  DO               /* 370 */
                            $(                                         /* 370 */
                                selectoutput( sysout )                 /* 370 */
                                writef( "******  Cannot open *"%S*"*N",/* 370 */
                                         tempvec )                     /* 370 */
                                selectoutput( liststream )             /* 370 */
                                                                       /* 370 */
                                error( 128 )                           /* 370 */
                            $)                                         /* 370 */
$>370

$<CAP
                            UNLESS  doget( tempvec )  DO               /* CAP */
                            $(                                         /* CAP */
                                selectoutput( sysout )                 /* CAP */
                                writef( "******  Cannot open *"%S*"*N",/* CAP */
                                         tempvec )                     /* CAP */
                                selectoutput( liststream )             /* CAP */
                                                                       /* CAP */
                                error( 128 )                           /* CAP */
                            $)                                         /* CAP */
$>CAP

$<68K
                            UNLESS  triposget( tempvec )  DO           /* 68K */
                            $(                                         /* 68K */
                                selectoutput( sysout )                 /* 68K */
                                writef( "******  Cannot open *"%S*": ",/* 68K */
                                         tempvec )                     /* 68K */
                                fault( result2 )                       /* 68K */
                                selectoutput( liststream )             /* 68K */
                                                                       /* 68K */
                                error( 128 )                           /* 68K */
                            $)                                         /* 68K */
$>68K

                            listed  :=  NOT  error.found
                        $)
                        ELSE  complain( 129 )

                        ENDCASE


        CASE d.cnop   : $(  //  Conditional No-Op (c.f. the IBM 370).  Due to
                            //  a problem with DS.L only performing WORD
                            //  alignments, CNOP  offset,base aligns the code
                            //  to an offset "offset" from the nearest position
                            //  "base".

                            LET offset  =  0
                            LET base    =  0

                            nextsymb()
                            checkexpression( evalexp( expression() ), FALSE )

                            offset  :=  value

                            checkexpression( evalexp( expression() ), TRUE )

                            base    :=  value

                            checkfor( s.none, 12 )

                            TEST  base = 0  THEN  complain( 150 )
                            ELSE
                            $(
                                LET loc  =  location

                                align( base )

                                UNLESS  offset = 0  DO
                                $(
                                    setloc( location + offset )

                                    UNLESS  (location - loc)  <  base  DO
                                        setloc( location - base )
                                $)
                            $)

                            IF  labelset  THEN  setlabel( locmode, location, no )
                        $)
                        ENDCASE


        CASE d.entry  : //  Define a symbol to be internal to the current
                        //  section.  We ignore this line in the first pass,
                        //  and wait until all the symbols on the line are
                        //  (Hopefully) defined.

                        IF  pass2  THEN
                        $(
                            checklabel( no )
                            nextsymb()

                            $(  //  Now the loop to read a list of names
                                //  valid data types are:
                                //
                                //      abs32
                                //      abs16
                                //      rel

                                TEST  symb = s.abs32  |  symb = s.abs16  |  symb = s.rel  THEN
                                $(
                                    IF  (symbtype+st.name) % 0  >  maxextlength  THEN
                                            warning( 165 )

                                    entrysymbols  :=  heap2( entrysymbols, symbtype )
                                $)

                                ELSE

                                    complain( symb = s.new  ->  153,
                                              symb = s.ext  ->  154, 155 )

                                readsymb()

                                TEST  symb = s.comma
                                    THEN  readsymb()
                                    ELSE  BREAK
                            $)
                            REPEAT

                            checkfor( s.none, 156 )
                        $)

                        commentline  :=  yes

                        ENDCASE


        CASE d.extrn  : //  Symbols defined as external.  These must NOT exist
                        //  when we look them up, and if they do, then this is
                        //  an error.

                        checklabel( no )
                        nextsymb()

                        $(  //  Loop to read a list of symbols.
                            //  On the first pass, the symbols MUST be new,
                            //  and on the second pass, they must be EXT.

                            LET correct  =  pass1  ->  s.new,  s.ext
                            LET type     =  symbtype!st.type  &  st.type.mask
                            LET flags    =  symbtype!st.flags

                            TEST  symb = correct  THEN
                            $(
                                IF  pass2  THEN
                                    IF  (symbtype+st.name) % 0  >  maxextlength  THEN
                                            warning( 166 )

                                IF  pass1  THEN
                                $(
                                    LET space  =  getstore( e.size )

                                    type              :=  s.ext
                                    flags             :=  stb.setever
                                    space!e.link      :=  extrnsymbols
                                    space!e.symbol    :=  symbtype
                                    space!e.countr    :=  0
                                    space!e.refsr     :=  0
                                    space!e.counta    :=  0
                                    space!e.refsa     :=  0
                                    extrnsymbols      :=  space
                                $)
                            $)
                            ELSE  complain( (symb = s.abs16  |
                                             symb = s.abs32  |
                                             symb = s.rel)    ->  157, 158 )

                            symbtype!st.type   :=  type
                            symbtype!st.flags  :=  flags | stb.setnow

                            readsymb()

                            TEST  symb = s.comma
                                THEN  readsymb()
                                ELSE  BREAK
                        $)
                        REPEAT

                        checkfor( s.none, 159 )

                        commentline  :=  yes

                        ENDCASE


        CASE d.fail   : complain( 122 )

                        ENDCASE


        DEFAULT       : complain( 0 )
    $)

    skiprest()
$)



AND do.ifstrings()  =  VALOF
$(
//  Read the two strings, and compare them for lexical equality.  Only the
//  IFEQ and IFNE directives have a meaning with strings ...

    LET buff1  =  VEC maxllen/bytesperword
    LET buff2  =  VEC maxllen/bytesperword
    LET len1   =  0
    LET len2   =  0
    LET equal  =  TRUE

    len1  :=  readstring( buff1 )

    UNLESS  ch = ','  DO  complain( 10 )

    rch()

    len2  :=  readstring( buff2 )

    readsymb()
    checkfor( s.none, 47 )

    //  Having read the strings into the two buffers, we should compare the
    //  lengths, and if equal, compare the characters.

    TEST  len1 = len2  THEN

        //  The lengths are equal, and so we should compare the rest of the
        //  buffers.

        FOR  i = 0  TO  len1-1  DO
            UNLESS  buff1 % i  =  buff2 % i  DO
                equal  :=  FALSE

    ELSE  equal  :=  FALSE

    //  Having decided whether the strings are equal or not, we should
    //  set the variable "value" so it can be printed out, and return a boolean
    //  saying whether we should skip or not.

    value  :=  equal  ->  0,  (NOT 0)

    RESULTIS  NOT(  directive = d.ifeq  ->  equal,
                    directive = d.ifne  ->  NOT equal,
                 /* Anything else ... */    complain( 186 )  )
$)



AND readstring( buffer )  =  VALOF
$(
//  Read a quoted string into a buffer, and return the length of the buffer.

    LET length  =  0

    UNLESS  ch = '*''  DO  complain( 187 )

    $(  //  Repeat loop to read the string into the buffer.  We break when we
        //  find a quote which is not followed by another quote.

        rch()

        IF  ch = '*N'  THEN  complain( 188 )

        IF  ch = '*''  THEN
        $(
            rch()

            UNLESS  ch = '*''  DO  BREAK
        $)

        buffer % length  :=  ch
        length           :=  length + 1
    $)
    REPEAT

    RESULTIS  length
$)



AND do.ifvalue()  =  VALOF
$(
//  Look at the expression given, and return a boolean saying whether,
//  depending on the value read, the following items should be skipped.

    readsymb()

    checkexpression( evalexp( expression() ), TRUE )

    RESULTIS  NOT(  directive = d.ifeq  ->  value =  0,
                    directive = d.ifne  ->  value \= 0,
                    directive = d.iflt  ->  value <  0,
                    directive = d.ifle  ->  value <= 0,
                    directive = d.ifgt  ->  value >  0,
                 /* directive = d.ifge  */  value >= 0  )
$)





$<68K
AND triposget( file )  =  VALOF
$(
    LET defaultdir  =  "SYS:G.ASM"
    LET stream      =  findinput( file )

    IF  stream = 0  THEN
    $(
        //  Can't open the file, so look in the default place.

        LET save  =  currentdir
        LET lock  =  locateobj( defaultdir )

        UNLESS  lock = 0  DO
        $(
            currentdir  :=  lock
            stream      :=  findinput( file )
            currentdir  :=  save

            freeobj( lock )
        $)
    $)

    RESULTIS  streamget( file, stream )
$)
$>68K



AND doget( file )  =  streamget( file, findinput( file ) )



AND streamget( filename, inputstream )  =   inputstream = 0  ->  no,  VALOF
$(
    LET oldinput  =  input()
    LET oldgl     =  getlevel
    LET oldln     =  linenumber
    LET oldcf     =  currentfile

    IF  getlevel = maxgetlevel  THEN
    $(
        selectinput( inputstream )
        endread()
        selectinput( oldinput )

        warning( 130 )

        RESULTIS  yes
    $)

    commentline  :=  yes
    currentfile  :=  makefile( filename )

    listline()

    getlevel    :=  getlevel + 1
    linenumber  :=  0

    selectinput( inputstream )

    UNTIL  getlevel = oldgl  |  ended  DO
    $(
        resetflags()

        doline()
    $)

    endread()
    selectinput( oldinput )

    linenumber   :=  oldln
    currentfile  :=  oldcf

    IF  inmacro  THEN
    $(
        inmacro  :=  no

        warning( 132 )
    $)

    RESULTIS  yes
$)



AND settitle( string )  BE
$(
    LET l  =  string % 0
    LET m  =  (titlecharsmax - l)/2 - 1

    FOR  i = 0  TO  titlecharsmax-1  DO  titlevec % i      :=  '*S'
    FOR  i = 1  TO  l                DO  titlevec % (m+i)  :=  string % i
$)



AND setlabel( type, value, alterable )  BE
$(
//  Set the label held in "labelvec" if the flag "labelset" is set to
//  TRUE

    LET savesymb  =  symb
    LET savest    =  symbtype
    LET muldef    =  no
    LET tagtable  =  type = s.macro  ->  tagtable1, tagtable2

    undefined  :=  no

    //  We must first translate all "s.abs" into "s.abs16" or "s.abs32"

    IF  type = s.abs  THEN
        type  :=  wordsized( value )  ->  s.abs16, s.abs32

    //  Look this label up in the relevant symbol table, so that we can update
    //  the symbol table entry.

    lookup( labelvec, tagtable )

    //  Having done that, we had better check that all is in order.  The only
    //  likely error is that we are redefining a symbol.

    UNLESS  symb = s.new  DO

            //  Check to see that the definition is compatible with
            //  what has gone before.

            TEST  alterable
                  THEN  UNLESS  (symbtype!st.flags & stb.equ) = 0  DO
                                 complain( 104 )
                  ELSE  UNLESS  (symbtype!st.flags & stb.set) = 0  DO
                                 complain( 105 )


    SWITCHON  alterable  ->  s.new, symb  INTO
    $(
        CASE s.Ar           :
        CASE s.Dr           :
        CASE s.reg          :
        CASE s.abs16        :
        CASE s.abs32        :
        CASE s.rel          :  //  An "EQU" directive is being used.  This is only
                               //  valid if it is pass2, and the symbol this
                               //  time has the same value as last time.

                               IF  symb = s.Ar  |  symb = s.Dr  THEN
                                   UNLESS  directive = d.equr  DO
                                       complain( 52 )

                               IF  symb = s.reg  THEN
                                   UNLESS  directive = d.reg  DO
                                       complain( 184 )

                               TEST  pass2  THEN
                               $(
                                   LET otype   =  symbtype!st.type  &  st.type.mask
                                   LET ovalue  =  symbtype!st.value

                                   UNLESS  errors > 0  DO
                                       UNLESS  otype = type  &  ovalue = value  DO
                                           complain( 51 )

                                   symbtype!st.flags  :=  symbtype!st.flags  |
                                                          stb.setnow
                               $)
                               ELSE
                               $(
                                   // Is he just redefining the system
                                   // parameters given to him (e.g. SP) ?
                                   // Allow this, providing that he is
                                   // defining it to the value it used
                                   // to be.

                                   LET otype   =  symbtype!st.type & st.type.mask
                                   LET ovalue  =  symbtype!st.value

                                   UNLESS  otype  = type    &
                                           ovalue = value   &
                                           symbtype!st.definition = 0  DO
                                   $(
                                       symbtype!st.flags  :=  symbtype!st.flags |
                                                              stb.muldef

                                       muldef             :=  yes
                                   $)
                               $)

                               ENDCASE


        CASE s.macro        :  TEST  pass2  THEN
                                     symbtype!st.flags  :=  symbtype!st.flags  |
                                                            stb.setnow

                               ELSE
                               $(
                                   symbtype!st.flags  :=  symbtype!st.flags  |
                                                          stb.muldef

                                   muldef             :=  yes
                               $)
                               ENDCASE


        CASE s.new          :  symbtype!st.type   :=  (symbtype!st.type-symb)    |
                                                      type

                               symbtype!st.flags  :=  symbtype!st.flags          |
                                                      (stb.setnow + stb.setever) |
                                                      (alterable  ->  stb.set,
                                                                      stb.equ)

                               symbtype!st.value  :=  value

                               ENDCASE


        CASE s.ext          :  complain( 160 )
        CASE s.instr        :  complain( 53 )
        CASE s.dir          :  complain( 54 )

        DEFAULT             :  complain( 55 )
    $)

    IF  undefined  &  pass2  THEN  complain( 95 )

    IF  pass1  &  NOT systemwords  THEN
        symbtype!st.definition  :=  muldef     ->  cr.multiple,
                                    alterable  ->  cr.setsymbol,
                                                   linenumber

    symb      :=  savesymb
    symbtype  :=  savest
$)



AND resetflags()  BE
$(
//  Reset all pointers, etc. at the beginning of a line.

    error.found      :=  no
    listed           :=  no
    commentline      :=  no
    undefined        :=  no
    forwardref       :=  no
    externalref      :=  no
    op1.externalref  :=  no
    expvecp          :=  expvec + expsize
    codewords        :=  0
    bytesonline      :=  0
    nitems           :=  0
    op.ea            :=  0
    op1.ea           :=  0
$)

.

//******************************************************************************
//*   Assembler for the Motorola MC68000 Microprocessor:  Section 3            *
//******************************************************************************



SECTION "M68KASM3"



GET "libhdr"

GET "m68khdr.h"



LET readsymb()  BE
$(
//  Read a Basic Symbol in, and set SYMB and SYMBTYPE

    SWITCHON  ch  INTO
    $(
        CASE '*S' :
        CASE '*T' :
        CASE '*N' : symb  :=  s.none
                    ENDCASE


        CASE '('  : rch()    ;    symb      :=  s.bra
                    ENDCASE


        CASE ')'  : rch()    ;    symb      :=  s.ket
                    ENDCASE

        CASE '#'  : rch()    ;    symb      :=  s.literal
                    ENDCASE


        CASE ','  : rch()    ;    symb      :=  s.comma
                    ENDCASE

        CASE ':'  : rch()    ;    symb      :=  s.colon
                    ENDCASE


        CASE '+'  : rch()    ;    symb      :=  s.plus
                    ENDCASE


        CASE '-'  : rch()    ;    symb      :=  s.minus
                    ENDCASE


        CASE '**' : rch()    ;    symb      :=  s.times
                    ENDCASE


        CASE '/'  : rch()    ;    symb      :=  s.over
                    ENDCASE


        CASE '&'  : rch()    ;    symb      :=  s.logand
                    ENDCASE


        CASE '!'  : rch()    ;    symb      :=  s.logor
                    ENDCASE


        CASE '<'  : rch()

                    TEST  ch = '<'  THEN
                    $(
                        rch()
                        symb  :=  s.lshift
                    $)
                    ELSE  complain( 56 )

                    ENDCASE


        CASE '>'  : rch()

                    TEST  ch = '>'  THEN
                    $(
                        rch()
                        symb  :=  s.rshift
                    $)
                    ELSE  complain( 56 )

                    ENDCASE


        CASE '0'  :  CASE '1'  :  CASE '2'  : CASE '3'  :
        CASE '4'  :  CASE '5'  :  CASE '6'  : CASE '7'  :
        CASE '8'  :  CASE '9'  :

                    number   :=  readnumber( 10 )
                    symb     :=  s.number

                    ENDCASE


        CASE '$'  : rch()
                    number   :=  readnumber( 16 )
                    symb     :=  s.number

                    ENDCASE


        CASE '@'  : rch()
                    number   :=  readnumber( 8 )
                    symb     :=  s.number

                    ENDCASE


        CASE '%'  : rch()
                    number   :=  readnumber( 2 )
                    symb     :=  s.number

                    ENDCASE


        CASE '\'  : UNLESS  inmacro  DO  complain( 117 )
                    UNTIL  ch = '*S'  |  ch = '*T'  |  ch = '*N'  DO  rch()

                    symb  :=  s.none

                    ENDCASE


        CASE '.'  :
        CASE '_'  :

        CASE 'A'  :  CASE 'B'  : CASE 'C'  : CASE 'D'  :
        CASE 'E'  :  CASE 'F'  : CASE 'G'  : CASE 'H'  :
        CASE 'I'  :  CASE 'J'  : CASE 'K'  : CASE 'L'  :
        CASE 'M'  :  CASE 'N'  : CASE 'O'  : CASE 'P'  :
        CASE 'Q'  :  CASE 'R'  : CASE 'S'  : CASE 'T'  :
        CASE 'U'  :  CASE 'V'  : CASE 'W'  : CASE 'X'  :
        CASE 'Y'  :  CASE 'Z'  :


        CASE 'a'  :  CASE 'b'  : CASE 'c'  : CASE 'd'  :
        CASE 'e'  :  CASE 'f'  : CASE 'g'  : CASE 'h'  :
        CASE 'i'  :  CASE 'j'  : CASE 'k'  : CASE 'l'  :
        CASE 'm'  :  CASE 'n'  : CASE 'o'  : CASE 'p'  :
        CASE 'q'  :  CASE 'r'  : CASE 's'  : CASE 't'  :
        CASE 'u'  :  CASE 'v'  : CASE 'w'  : CASE 'x'  :
        CASE 'y'  :  CASE 'z'  :

                    readtag()
                    ENDCASE


        CASE '*'' : rch()

                    symb    :=  s.number
                    number  :=  0

                    FOR  count = 1  TO  4  DO
                    $(
                        IF  ch = '*N'  THEN  complain( 57 )

                        IF  ch = '*''  THEN
                        $(
                            rch()

                            UNLESS  ch = '*''  DO

                                //  This is an ascii literal which is
                                //  shorter than the maximum length.
                                      
                                ENDCASE
                        $)

                        number  :=  (number << 8)  +  ascii.value( ch )

                        rch()
                    $)
                          
                    //  If we drop out of there, then we should check 
                    //  that the terminating quote is present, and
                    //  complain if not.

                    TEST  ch = '*''  
                        THEN  rch()
                        ELSE  complain( 58 )

                    ENDCASE


        DEFAULT   : complain( 59 )
    $)
$)



$<370
AND ascii.value( char )  =  char ! (TABLE                              /* 370 */
      0,     0,     0,     0,     0,   #11,     0,     0,              /* 370 */
      0,     0,     0,   #13,   #14,   #15,     0,     0,              /* 370 */
      0,     0,     0,     0,     0,   #12,     0,     0,              /* 370 */
      0,     0,     0,     0,     0,     0,     0,     0,              /* 370 */
      0,     0,     0,     0,     0,   #12,     0,     0,              /* 370 */
      0,     0,     0,     0,     0,     0,     0,     0,              /* 370 */
      0,     0,     0,     0,     0,     0,     0,     0,              /* 370 */
      0,     0,     0,     0,     0,     0,     0,     0,              /* 370 */
    #40,     0,  #133,  #135,     0,     0,     0,     0,              /* 370 */
      0,     0,     0,   #56,   #74,   #50,   #53,  #174,              /* 370 */
    #46,     0,     0,     0,     0,     0,     0,     0,              /* 370 */
      0,     0,   #41,   #44,   #52,   #51,   #73,  #176,              /* 370 */
    #55,   #57,  #134,     0,     0,  #136,  #137,     0,              /* 370 */
      0,     0,     0,   #54,   #45,  #137,   #76,   #77,              /* 370 */
      0,     0,     0,     0,     0,     0,     0,     0,              /* 370 */
      0,  #140,   #72,   #43,  #100,   #47,   #75,   #42,              /* 370 */
      0,  #141,  #142,  #143,  #144,  #145,  #146,  #147,              /* 370 */
   #150,  #151,     0,     0,     0,     0,     0,     0,              /* 370 */
      0,  #152,  #153,  #154,  #155,  #156,  #157,  #160,              /* 370 */
   #161,  #162,     0,     0,     0,     0,     0,     0,              /* 370 */
      0,     0,  #163,  #164,  #165,  #166,  #167,  #170,              /* 370 */
   #171,  #172,     0,     0,     0,     0,     0,     0,              /* 370 */
      0,     0,     0,     0,     0,     0,     0,     0,              /* 370 */
      0,     0,     0,     0,     0,     0,     0,     0,              /* 370 */
      0,  #101,  #102,  #103,  #104,  #105,  #106,  #107,              /* 370 */
   #110,  #111,     0,     0,     0,     0,     0,     0,              /* 370 */
      0,  #112,  #113,  #114,  #115,  #116,  #117,  #120,              /* 370 */
   #121,  #122,     0,     0,     0,     0,     0,     0,              /* 370 */
      0,     0,  #123,  #124,  #125,  #126,  #127,  #130,              /* 370 */
   #131,  #132,     0,     0,     0,     0,     0,     0,              /* 370 */
    #60,   #61,   #62,   #63,   #64,   #65,   #66,   #67,              /* 370 */
    #70,   #71,     0,     0,     0,     0,     0,     0)              /* 370 */
$>370



$<CAP
AND ascii.value( ch )  =  (ch & #X7F)                                  /* CAP */
$>CAP



$<68K
AND ascii.value( ch )  =  (ch & #X7F)                                  /* 68K */
$>68K



AND readnumber( base )  =  VALOF
$(
//  Reads a number from the source, with given radix "base"

    LET val  =  digitval( ch )
    LET num  =  0

    UNLESS  val < base  DO  complain( 60 )

    WHILE  val < base  DO
    $(
        num  :=  num * base  +  val

        rch()

        val  :=  digitval( ch )
    $)

    RESULTIS  num
$)




AND digitval( char )  =     '0'  <=  char  <=  '9'   ->  char - '0',
                            'A'  <=  char  <=  'F'   ->  char - 'A' + 10,
                            'a'  <=  char  <=  'f'   ->  char - 'a' + 10,
                                                         100




AND effective.address()  =  VALOF
$(
//  An effective address is of the form:
//
//    <expression>          or
//    <expression>(R)       or
//    <expression>(Ar,R)    or
//    <expression>(PC)      or
//    <expression>(PC,R)    or
//    #<expression>

    LET result  =  0

    bracount  :=  0

    SWITCHON  symb  INTO
    $(
        CASE s.none     : complain( 61 )


        CASE s.literal  : readsymb()
                          RESULTIS  block2( ea.literal, expression() )


        DEFAULT         : // Assume that all others are starts to
                          // expressions

                          result  :=  expression()

                          TEST  symb = s.bra  THEN
                          $(
                              LET rtype1  =  0
                              LET rtype2  =  0
                              LET rnum1   =  0
                              LET rnum2   =  0
                              LET rsize1  =  0
                              LET rsize2  =  0

                              //  We are now decoding either:
                              //     1)  e(R)    or
                              //     2)  e(PC)   or
                              //     3)  e(Ar,R) or
                              //     4)  e(PC,R)

                              readsymb()

                              UNLESS  symb = s.Ar | symb = s.Dr | symb = s.PC  DO
                                  complain( 62 )

                              rtype1  :=  symb
                              rnum1   :=  regnum
                              rsize1  :=  tagsize.given

                              readsymb()

                              TEST  symb = s.ket  THEN
                              $(
                                  //  This is an  e(R) type addressing
                                  //  mode.  More, we cannot say yet.

                                  readsymb()

                                  RESULTIS  block3( ea.R.disp, result,
                                               block3( rtype1, rnum1, rsize1 ) )
                              $)
                              ELSE

                              TEST  symb = s.comma  THEN
                              $(
                                  //  This is an "e(X,R)" type addressing
                                  //  mode.  "rtype1" must be Ar or PC, and the
                                  //  expression MUST be absolute, although
                                  //  we will not be able to verify this, until
                                  //  it is evaluated.

                                  UNLESS  rtype1 = s.Ar | rtype1 = s.PC  DO  
                                      complain( 63 )

                                  readsymb()

                                  UNLESS  symb = s.Ar | symb = s.Dr  DO  complain( 64 )

                                  rtype2  :=  symb
                                  rnum2   :=  regnum
                                  rsize2  :=  tagsize.given

                                  readsymb()
                                  checkfor( s.ket, 66 )

                                  RESULTIS  block3( ea.R.index, result,
                                              block2(
                                                block3( rtype1, rnum1, rsize1 ),
                                                block3( rtype2, rnum2, rsize2 ) ) )
                              $)
                              ELSE   complain( 65 )
                          $)
                          ELSE  RESULTIS block2( ea.exp, result )
    $)
$)




AND expression()  =  VALOF
$(
    LET f  =  factor()

    WHILE  (symb = s.plus  |  symb = s.minus)  DO
    $(
        LET op  =  symb

        readsymb()

        f  :=  block2( s.opapply, block3( op, f, factor() ) )
    $)

    RESULTIS  f
$)



AND factor()  =  VALOF
$(
    LET t  =  term()

    WHILE  (symb = s.times  |  symb = s.over)  DO
    $(
        LET op  =  symb

        readsymb()

        t  :=  block2( s.opapply, block3( op, t, term() ) )
    $)

    RESULTIS  t
$)



AND term()  =  VALOF
$(
    LET s  =  secondary()

    WHILE  (symb = s.logand  |  symb = s.logor)  DO
    $(
        LET op  =  symb

        readsymb()

        s  :=  block2( s.opapply, block3( op, s, secondary() ) )
    $)

    RESULTIS  s
$)



AND secondary()  =  VALOF
$(
    LET p  =  primary()

    WHILE  (symb = s.lshift  |  symb = s.rshift)  DO
    $(
        LET op  =  symb

        readsymb()

        p  :=  block2( s.opapply, block3( op, p, primary() ) )
    $)

    RESULTIS  p
$)



AND primary()  =  VALOF
$(
    LET result  =  0

    SWITCHON  symb  INTO
    $(
        CASE s.Dr        :
        CASE s.Ar        : // At this point, ch holds the terminating character

                           IF  (symbtype!st.flags & stb.setnow) = 0  THEN
                               complain( 148 )

                           result  :=  block3( symb, regnum, tagsize.given )

                           IF  ch = '-'  |  ch = '/'  THEN  UNLESS  in.movem  DO

                               //  It is likely that this is a MOVEM
                               //  type of register list.  If it is, then
                               //  it is illegal here.

                               complain( 67 )

                           checktagsize()

                           readsymb()
                           RESULTIS  result


        CASE s.SR       :
        CASE s.CCR      :
        CASE s.USP      : 
        CASE s.PC       : checktagsize()

        CASE s.star     : result  :=  block1( symb )
                          readsymb()
                          RESULTIS  result


        CASE s.ext      : IF  (symbtype!st.flags & stb.setnow) = 0  THEN
                              complain( 174 )

        CASE s.rel      :
        CASE s.abs16    :
        CASE s.abs32    :
        CASE s.new      : IF  pass2 & undefined  THEN  complain( 97 )
                          checktagsize()

                          result  :=  block2( symb, symbtype )
                          readsymb()
                          RESULTIS  result


        CASE s.number   : result  :=  block2( s.number, number )
                          readsymb()
                          RESULTIS  result


        CASE s.minus    : //  This could be   -<expression>
                          //  or              -(Ar)
                          //  or even         -(<expression>)

                          readsymb()

                          IF  symb = s.bra  THEN   // More work to do
                          $(
                              readsymb()

                              IF  symb = s.Ar  THEN
                              $(
                                  IF  bracount > 0  THEN  complain( 68 )

                                  result  :=  block2( s.Ar.predecr,
                                                block3( symb, regnum, tagsize.given ) )
                                  readsymb()
                                  checkfor( s.ket, 65 )

                                  RESULTIS  result
                              $)

                              result  :=  expression()
                              checkfor( s.ket, 69 )

                              RESULTIS  block2( s.monminus, result )
                          $)

                          RESULTIS  block2( s.monminus, primary() )


        CASE s.bra      : //  This could be  (<expression>)
                          //  or             (Ar)
                          //  or             (Ar)+

                          readsymb()
                          
                          IF  symb = s.Ar  THEN
                          $(
                              IF  bracount > 0  THEN   complain( 68 )

                              result  :=  block3( symb, regnum, tagsize.given )
                              readsymb()
                              checkfor( s.ket, 65 )

                              TEST  symb = s.plus  THEN
                              $(
                                  readsymb()


                                  RESULTIS  block2( s.Arx.postincr, result )
                              $)
                              ELSE  RESULTIS  block2( s.Ar.indirect, result )
                          $)
                          
                          IF  symb = s.Dr  THEN  complain( 68 )

                          bracount  :=  bracount + 1
                          result    :=  expression()
                          bracount  :=  bracount - 1

                          checkfor( s.ket, 69 )

                          RESULTIS  result

        CASE s.literal  : complain( 139 )

        CASE s.plus     :
        CASE s.over     :
        CASE s.logand   :
        CASE s.logor    :
        CASE s.lshift   :
        CASE s.rshift   : complain( 140 )

        CASE s.none     : complain( 141 )
        CASE s.ket      : complain( 142 )
        CASE s.comma    : complain( 143 )
        CASE s.colon    : complain( 144 )
        CASE s.instr    : complain( 135 )
        CASE s.dir      : complain( 136 )
        CASE s.macro    : complain( 137 )
        CASE s.reg      : complain( 185 )

        DEFAULT         : complain( 70 )
    $)
$)



AND evaluate( ptr )  BE  UNLESS  ptr = 0  DO
$(
//  Will eventually evaluate the expression pointed to by pointer, and
//  set up the operand effective address: op.ea
//  The result is a boolean, corresponding to whether the evaluation
//  has detected an error.

    LET ptr0  =  ptr!p.ptr0
    LET ptr1  =  ptr!p.ptr1

    forwardref  :=  no
    op.ea       :=  0

    SWITCHON  ptr0  INTO
    $(
        //  The possible types of effective address are:
        //
        //      #<expression>                 ea.literal
        //      <expression>                  ea.exp
        //      <expression>(X)               ea.R.disp
        //      <expression>(X,R)             ea.R.index


        CASE ea.literal   :  exptype  :=  datatype( evalabsexp( ptr1 ) )
                             exp      :=  value

                             UNLESS  externalref  DO
                             $(
                                 IF  1 <= exp <= 8     THEN  op.ea  :=  op.ea | am.imm3
                                 IF  wordsized( exp )  THEN  op.ea  :=  op.ea | am.imm16
                             $)

                             op.ea  :=  op.ea | am.imm32

                             SWITCHON  instr.size  INTO
                             $(
                                 CASE ts.byte :  UNLESS  bytesized( exp )  DO
                                                     warning( 176 )
                                                 ENDCASE

                                 CASE ts.none :
                                 CASE ts.word :  UNLESS  wordsized( exp )  DO
                                                     warning( 175 )
                                                 ENDCASE

                                 CASE ts.long :  ENDCASE
                                 
                                 
                                 DEFAULT      :  complain( 0 )
                             $)
                                                    
                             ENDCASE


        CASE ea.exp        : // This could be absolute, or relocatable,
                             // and, if on the first pass, contain variables
                             // which are undefined.

                             exptype  :=  datatype( evalexp( ptr1 ) )

                             exp    :=   value
                             op.ea  :=   VALOF

                             SWITCHON  (absolute( exptype ) & forwardref)  ->  forwardreftype,
                                        exptype  INTO
                             $(
                                 CASE s.rel          : RESULTIS am.PC.disp
                                 CASE s.abs16        : RESULTIS abs16addr( exp )
                                 CASE s.abs32        : RESULTIS am.abs32
                                 CASE s.Ar           : RESULTIS am.Ar
                                 CASE s.Dr           : RESULTIS am.Dr
                                 CASE s.Ar.predecr   : RESULTIS am.Ar.pd
                                 CASE s.Ar.postincr  : RESULTIS am.Ar.pi
                                 CASE s.Ar.indirect  : RESULTIS am.Ar.ind
                                 CASE s.SR           :
                                 CASE s.CCR          :
                                 CASE s.USP          : RESULTIS am.special

                                 DEFAULT             : complain( 70 )
                             $)
                             ENDCASE


        CASE ea.R.disp    :  //  This could be one of the two forms:
                             //
                             //      <expression>(Ar)
                             //      <expression>'(Ar)
                             //      <expression>'(Dr)
                             //
                             //      <expression>(PC)
                             //      <expression>'(PC)
                             //
                             //  The first of which requires the <expression> to
                             //  be absolute and the Address register to be
                             //  unsized.  The others allow the <expression> to
                             //  be relocatable, and the register to be sized
                             //  (either word or long word sized).
                             
                             exptype  :=  datatype( evalexp( ptr1 ) )
                                 
                             IF  externalref  THEN  complain( 162 )
                                 
                             registers  :=  ptr!p.ptr2
                             
                             IF  registers!p.ptr0 = s.PC  THEN
                             $(
                                 //  This is the e(PC) form of the instruction.
                                 //  The expression is evaluated relative to
                                 //  the current PC.
                                 
                                 TEST  forwardref & pass1  THEN
                                 $(
                                     //  Can't calculate the offset yet, but no
                                     //  matter, since we can do it next time.
                                     
                                     exptype  :=  locmode
                                     exp      :=  location
                                 $)
                                 ELSE

                                     //  All is OK.
                                     
                                     exp  :=  value
                                     
                                 //  We can now return with the PC relative
                                 //  addressing mode.
                                 
                                 op.ea  :=  am.PC.disp
                                 
                                 ENDCASE
                             $)

                             //  Otherwise, this is a normal type of addressing
                             //  mode, and we should handle it as before.

                             exp      :=  value
                             op.ea    :=  VALOF

                                 SWITCHON  exptype  INTO
                                 $(
                                     //  The expression can be relocatable,
                                     //  in which case, it is the PC+index
                                     //  addressing mode.  It can also be
                                     //  of type "abs16", in which case it
                                     //  is the Ar+disp addressing mode.
                                     //  If it is "new", then we can do nothing
                                     //  until the next pass (if there is one).

                                     CASE s.rel     : RESULTIS  am.PC.index

                                     CASE s.abs16   :
                                     CASE s.abs32   : RESULTIS  am.Ar.disp

                                     DEFAULT        : complain( 70 )
                                 $)

                             ENDCASE


        CASE ea.R.index   :  //  This must be of the form:
                             //
                             //      <expression>(X,R)
                             //
                             //  This may be PC indexed mode or Ar indexed
                             //  mode.  More work has to be done to find out
                             //  which it is.

                             exptype  :=  datatype( evalexp( ptr1 ) )

                             IF  externalref  THEN  complain( 162 )
                             
                             //  We must decide whether X is an address 
                             //  register, or the special value PC.
                             
                             registers  :=  ptr!p.ptr2
                             
                             IF  registers!p.ptr0!p.ptr0 = s.PC  THEN
                             $(
                                 //  Aha!  This is easier than we thought.  
                                 //  This is actually the PC index mode.
                                 
                                 registers  :=  registers!p.ptr1
                                 
                                 TEST  forwardref & pass1  THEN
                                 $(
                                     //  Can't calculate the offset yet, but no
                                     //  matter, since we can do it next time.
                                     
                                     exptype  :=  locmode
                                     exp      :=  location
                                 $)
                                 ELSE

                                     //  All is OK.
                                     
                                     exp  :=  value
                                     
                                 //  We can now return with the PC indexed
                                 //  addressing mode.
                                 
                                 op.ea  :=  am.PC.index
                                 
                                 ENDCASE
                             $)
                             
                             //  Otherwise, this is one of the boring modes
                             //  which we are used to handling.

                             IF  exptype = s.rel  THEN  complain( 71 )

                             exp    :=  value
                             op.ea  :=  am.Ar.index

                             ENDCASE


        DEFAULT           :  complain( 0 )
    $)
$)



AND abs16addr( addr )  =  (forwardref  &  pass1)  ->  am.abs16,  VALOF
$(
//  Calculate the data type of the (supposedly) 16 bit address given to us.
//  Unfortunately, this address is only allowed to be 15 bits, and if larger,
//  must NOT be a forward reference.

    TEST  (addr & #X7FFF) = addr  THEN  RESULTIS  am.abs16
    ELSE
    $(
        //  More than a 15 bit address, and so we must check for backward or
        //  forward reference.  Backward reference is ok, but forward reference
        //  must be flagged as an error.
        
        IF  forwardref  THEN  complain( 73 )

        //  Otherwise, all is ok, and we can return the addressing mode as 
        //  being long absolute.
        
        RESULTIS  am.abs32
    $)
$)



AND evalabsexp( ptr )  =  ptr = 0  ->  0,  VALOF
$(
//  Evaluate the expression pointed to by "ptr", and return its data type.
//  This must be an absolute expression.

    LET type  =  datatype( evalexp( ptr ) )

    TEST  absolute( type )  THEN  RESULTIS  type
    ELSE
    $(
        //  Not an absolute symbol, so complain about it.
        
        SWITCHON  type  INTO
        $(
            CASE s.Ar          :
            CASE s.Dr          :  complain( 145 )
            
            CASE s.rel         :  complain( 71 )

            CASE s.Ar.indirect :  
            CASE s.Ar.postincr :
            CASE s.Ar.predecr  :  complain( 146 )
            
            CASE s.SR          :
            CASE s.CCR         :
            CASE s.USP         :
            CASE s.PC          :  complain( 147 )
            
            DEFAULT            :  complain( 70 )
        $)
    $)
$)



AND evalexp( ptr )  =  ptr = 0  ->  0,  VALOF
$(
//  Evaluate the general expression, pointed to by "ptr".

    LET ptr0  =  ptr!p.ptr0
    LET ptr1  =  ptr!p.ptr1
    LET ptr2  =  ptr!p.ptr2
    LET fref  =  0

    SWITCHON  ptr0  INTO
    $(
        CASE s.Dr           :
        CASE s.Ar           : value  :=  checkregister( ptr )
                              RESULTIS ptr0

        CASE s.Ar.indirect  :
        CASE s.Ar.postincr  :
        CASE s.Ar.predecr   : value  :=  checkregister( ptr1 )
                              RESULTIS ptr0

        CASE s.SR           :
        CASE s.CCR          :
        CASE s.USP          :
        CASE s.PC           : value  :=  ptr0
                              RESULTIS  ptr0


        CASE s.new          : forwardref :=  yes
                              value      :=  1
                              RESULTIS   forwardreftype


        CASE s.abs16        :
        CASE s.abs32        :
        CASE s.rel          : fref        :=  (ptr1!st.flags & stb.setnow) = 0
                              forwardref  :=  forwardref | fref

                              IF  (ptr1!st.flags & stb.set) \= 0  &  fref  THEN

                                  //  Illegal forward reference to a symbol
                                  //  defined by "SET"

                                  complain( 106 )

                              value  :=  ptr1!st.value
                              RESULTIS  ptr0


        CASE s.star         : value  :=  location
                              RESULTIS  relocatable( locmode )  ->  locmode,
                                        wordsized( value )      ->  s.abs16,
                                                                    s.abs32

        CASE s.number       : value  :=  ptr1
                              RESULTIS  wordsized( value )  ->  s.abs16,
                                                                s.abs32

        CASE s.monminus     : exptype  :=  evalexp( ptr1 )

                              IF  externalref  THEN  complain( 163 )

                              value  :=  VALOF
                                  SWITCHON  exptype  INTO
                                  $(
                                      CASE s.rel   : complain( 74 )

                                      CASE s.abs16 :
                                      CASE s.abs32 : RESULTIS -value

                                      DEFAULT      : complain( 70 )
                                  $)

                              RESULTIS  wordsized( value )  ->  s.abs16,
                                                                s.abs32


        CASE s.opapply      : //  Apply a dyadic operator.  Only certain
                              //  operations are allowed on operands of
                              //  specific data types.

                              // External symbols can have a limited
                              // amount of arithmetic done on them.
                              $(
                                  LET type1   =  0
                                  LET type2   =  0
                                  LET value1  =  0
                                  LET value2  =  0
                                  LET ext1    =  0
                                  LET ext2    =  0
                                  LET ext     =  externalref
                                  LET result  =  0

                                  externalref  :=  no
                                  type1        :=  evalexp( ptr1!p.ptr1 )
                                  value1       :=  value
                                  ext1         :=  externalref

                                  externalref  :=  no
                                  type2        :=  evalexp( ptr1!p.ptr2 )
                                  value2       :=  value
                                  ext2         :=  externalref

                                  //  External symbols are allowed in
                                  //  expressions, provided that:
                                  //
                                  //    a)  Only one per expression
                                  //    b)  E+absexp  or  absexp+E
                                  //    c)  E-absexp

                                  IF  ext1  |  ext2  THEN
                                  $(
                                      LET op  =  ptr1!p.ptr0

                                      //  This needs checking further.  It is
                                      //  illegal if "ext" is set, or both "ext1"
                                      //  and "ext2" are set.

                                      IF  ext  |  (ext1 & ext2)  THEN
                                          complain( 163 )

                                      //  Ok - only one of them is set, so
                                      //  check the operator/operand pair

                                      TEST  ext1  THEN
                                            UNLESS  (op = s.plus | op = s.minus)  &
                                                    (absolute( type2 ))  DO
                                                        complain( 163 )

                                      ELSE  UNLESS  (op = s.plus)    &
                                                    (absolute( type1 ))  DO
                                                        complain( 163 )
                                  $)


                                  value  :=  VALOF
                                      SWITCHON  ptr1!p.ptr0  INTO
                                      $(
                                          CASE s.plus   : RESULTIS value1  + value2
                                          CASE s.minus  : RESULTIS value1  - value2
                                          CASE s.times  : RESULTIS value1  * value2
                                          CASE s.over   : RESULTIS value1  / value2
                                          CASE s.logand : RESULTIS value1  & value2
                                          CASE s.logor  : RESULTIS value1  | value2
                                          CASE s.lshift : RESULTIS value1 << value2
                                          CASE s.rshift : RESULTIS value1 >> value2

                                          DEFAULT       : complain( 0 )
                                      $)

                                  externalref  :=  ext | ext1 | ext2

                                  result  :=  finaltype( type1, type2, ptr1!p.ptr0, value )

                                  RESULTIS  externalref  ->  s.abs32, result
                              $)


        CASE s.ext          : externalref  :=  yes
                              externalsymb :=  ptr1
                              value        :=  0
                              RESULTIS  s.abs32


        DEFAULT             : complain( 0 )
    $)
$)



AND datatype( type )  =  (pass1  &  forwardref  &  type = s.rel)  ->
                          s.abs16,  type



AND sizevalue( sizebit )  =  VALOF
$(
    SWITCHON  sizebit  INTO
    $(
        CASE size.b  :  RESULTIS ts.byte
        CASE size.w  :  RESULTIS ts.word
        CASE size.l  :  RESULTIS ts.long

        DEFAULT      :  RESULTIS ts.none
    $)
$)




AND finaltype( type1, type2, op, value )  =  VALOF
$(
//  The operator table is as follows:   A represents an Absolute symbol
//                                      R represents a  Relocatable symbol
//                                      x represents an error
//
//


//========================================================================//
//                                                                        //
//                              OPERANDS                                  //
//                                                                        //
//                  |          |          |          |                    //
//      Operator    |    AA    |    RR    |    AR    |    RA              //
//   ---------------+----------+----------+----------+----------          //
//                  |          |          |          |                    //
//         +        |     A    |     x    |     R    |     R              //
//         -        |     A    |     A    |     x    |     R              //
//         *        |     A    |     x    |     x    |     x              //
//         /        |     A    |     x    |     x    |     x              //
//         &        |     A    |     x    |     x    |     x              //
//         !        |     A    |     x    |     x    |     x              //
//         >>       |     A    |     x    |     x    |     x              //
//         <<       |     A    |     x    |     x    |     x              //
//                                                                        //
//========================================================================//



    LET abs1  =  absolute( type1 )
    LET abs2  =  absolute( type2 )
    LET rel1  =  relocatable( type1 )
    LET rel2  =  relocatable( type2 )

    LET AA    =  abs1 & abs2
    LET RR    =  rel1 & rel2
    LET AR    =  abs1 & rel2
    LET RA    =  rel1 & abs2

    LET ws    =  wordsized( value )


    SWITCHON  op  INTO
    $(
        CASE  s.times      :
        CASE  s.over       :
        CASE  s.logand     :
        CASE  s.logor      :
        CASE  s.lshift     :
        CASE  s.rshift     : TEST  AA  THEN  RESULTIS  ws  ->  s.abs16, s.abs32
                                       ELSE  complain( 75 )


        CASE s.plus        : TEST  AA  THEN  RESULTIS  ws  ->  s.abs16, s.abs32  ELSE
                             TEST  AR  THEN  RESULTIS  s.rel                     ELSE
                             TEST  RA  THEN  RESULTIS  s.rel                     ELSE
                                             complain( 76 )


        CASE s.minus       : TEST  AA  THEN  RESULTIS  ws  ->  s.abs16, s.abs32  ELSE
                             TEST  RR  THEN  RESULTIS  ws  ->  s.abs16, s.abs32  ELSE
                             TEST  RA  THEN  RESULTIS  s.rel                     ELSE
                             TEST  AR  THEN  TEST  pass1 THEN  RESULTIS s.abs16
                                             ELSE  complain( 76 )
                                       ELSE  complain( 76 )


        DEFAULT            : complain( 0 )
    $)
$)

.

//******************************************************************************
//*   Assembler for the Motorola MC68000 Microprocessor:  Section 4            *
//******************************************************************************



SECTION "M68KASM4"



GET "libhdr"

GET "m68khdr.h"



LET defineconstants( size )  BE
$(
//  Deal with a "DC" directive.

    LET restype  =  0
    LET bs       =  bytesize( size )

    skiplayout()

    align( bytesize( size = ts.byte  ->  size, ts.word ) )

    IF  labelset  THEN  setlabel( locmode, location, no )

    nitems  :=  0

    $(  //  Repeat loop to read all the items on the line.  We are already
        //  aligned to the correct boundary.

        externalref  :=  no
        forwardref   :=  no

        TEST  ch = '*''  THEN
        $(
            //  This is the most revolting feature of the Motorola 68000
            //  assembler.  We are allowed to use DC.L and DC.W to declare
            //  aligned string constants as well as DC.B.

            LET charcount  =  0

            $(  //  Repeat loop to read the characters in the string.

                rch()

                IF  ch = '*''  THEN
                $(
                    rch()
                    UNLESS  ch = '*''  BREAK
                $)

                IF  ch = '*N'  THEN  complain( 57 )

                stackvalue( s.abs16, 1, ascii.value( ch ), no, 0 )
                    
                charcount  :=  charcount + 1
            $)
            REPEAT

            readsymb()
                
            //  We now have to ensure that we are aligned to the right sort
            //  of boundary by filling up with the right number of nulls.
                
            UNTIL  (charcount REM bs)  =  0  DO
            $(
                stackvalue( s.abs16, 1, 0, no, 0 )
                    
                charcount  :=  charcount + 1
            $)
        $)
        ELSE
        $(
            readsymb()

            restype  :=  evalexp( expression() )

            UNLESS  size = ts.byte  DO
                IF  pass1 & forwardref  THEN  
                    relocate( 0, bs )

            IF  externalref  &  size \= ts.long  THEN
                complain( 161 )
                
            IF  size = ts.byte  &  NOT absolute( restype )  THEN
                complain( 123 )

            IF  pass2  THEN
            $(
                IF  size = ts.word  &  NOT wordsized( value )  THEN
                    warning( 175 )

                IF  size = ts.byte  &  NOT bytesized( value )  THEN
                    warning( 176 )
            $)

            stackvalue( restype, bs, value, externalref, externalsymb )
        $)
    $)
    REPEATWHILE  symb = s.comma

    //  If we drop through here, then, either we have reached the end
    //  of the list, and have come to an "s.none", or else, this is some
    //  sort of syntax error.

    checkfor( s.none, 77 )
$)



AND defineblock( size )  BE
$(
//  Handle the DCB directive - define a block of items which are all the same
//  value.

    LET incr     =  0
    LET newloc   =  0
    LET restype  =  0
    LET bs       =  bytesize( size )
    
    align( bytesize( size = ts.byte  ->  size, ts.word ) )

    IF  labelset  THEN  setlabel( locmode, location, no )
    
    //  First read the count, which will tell us where the final location will
    //  be.
    
    nextsymb()

    restype  :=  evalexp( expression() )

    TEST  forwardref                THEN  complain( 79 )   ELSE
    TEST  externalref               THEN  complain( 164 )  ELSE
    TEST  NOT absolute( restype )   THEN  complain( 71 )   ELSE
    TEST  value < 0                 THEN  complain( 48 )   ELSE

          incr  :=  value * bs
          
    //  We now have an increment in our hands, and so can calculate what
    //  the new address would be.  If out of address range, then we should
    //  complain now.
    
    newloc  :=  location + incr
    
    UNLESS  (newloc & addressmask) = 0  DO  complain( 48 )

    //  Having got this far, we can read the argument to the directive, which
    //  is the value to be repeated.

    checkfor( s.comma, 10 )

    forwardref   :=  no
    externalref  :=  no
    restype      :=  evalexp( expression() )
    
    checkfor( s.none, 47 )
    
    IF  size = ts.byte  &  NOT absolute( restype )  THEN
        complain( 123 )
        
    IF  externalref  &  size \= ts.long  THEN
        complain( 161 )

    IF  pass2  THEN
    $(
        IF  size = ts.word  &  NOT wordsized( value )  THEN
            warning( 175 )

        IF  size = ts.byte  &  NOT bytesized( value )  THEN
            warning( 176 )
    $)

    //  We now have the count and the item to be repeated.  We should enter a
    //  loop updating the store buffer with the value, and relocating the
    //  symbol if necessary.
    
    TEST  pass1  THEN
    $(
        //  Nothing to be done in the first pass.  Just relocate the symbol
        //  many times is necessary.
        
        UNTIL  location = newloc  DO
        $(
            UNLESS  size = ts.byte  DO  
                IF  relocatable( restype )  |  forwardref  THEN
                    relocate( 0, bs )
            
            setloc( location + bs )
        $)
    $)
    ELSE
    $(
        //  This is the second pass, and we must update the code vector with
        //  the value.  We must remember to relocate the symbol and add
        //  external references if necessary.
        
        IF  listing  THEN
        $(
            clearbuffer()

            linepos  :=  0

            writehexvalue( location, locmode = s.rel  ->  4, 6 )

            IF  locmode = s.rel  THEN  writechar( '*'' )

            linepos  :=  10
            writechar( '=' )
            writehexvalue( incr, 4 )

            IF  error.found  THEN
            $(
                linepos  :=  35
                writechar( 'E' )
                error.found  :=  no
            $)

            linepos :=  38
            writenumber( linenumber, 5 )

            IF  macrodepth > 0  &  NOT inmacro  THEN
            $(
                linepos  :=  43
                writechar( '+' )
            $)

            linepos  :=  44
            FOR  i = 0  TO  length-1  DO  writechar( inputbuff % i )

            printbuffer()

            listed  :=  yes
        $)

        //  Having listed the line, fill in all the values which we have 
        //  prepared so carefully.

        UNTIL  location = newloc  DO
        $(
            IF  relocatable( restype )  THEN  relocate( location, bs )
            IF  externalref             THEN  addexternalref( externalsymb, location )

            codebytes( bs, value )
        $)
    $)
$)



AND definestorage( size )  BE
$(
//  Deal with a "DS" directive

    LET incr     =  0
    LET newloc   =  0
    LET restype  =  0
    LET bs       =  bytesize( size )

    align( bytesize( size = ts.byte  ->  size, ts.word ) )

    IF  labelset  THEN  setlabel( locmode, location, no )

    nextsymb()

    restype  :=  evalexp( expression() )

    TEST  forwardref                THEN  complain( 79 )   ELSE
    TEST  externalref               THEN  complain( 164 )  ELSE
    TEST  symb  \=  s.none          THEN  complain( 47 )   ELSE
    TEST  NOT absolute( restype )   THEN  complain( 71 )   ELSE
    TEST  value < 0                 THEN  complain( 48 )   ELSE

          incr  :=  value * bs
          
    //  We now have an increment in our hands, and so can calculate what
    //  the new address would be.  If out of address range, then we should
    //  complain now.
    
    newloc  :=  location + incr
    
    UNLESS  (newloc & addressmask) = 0  DO  complain( 48 )

    IF  pass2  &  listing  THEN
    $(
        clearbuffer()

        linepos  :=  0

        writehexvalue( location, locmode = s.rel  ->  4, 6 )

        IF  locmode = s.rel  THEN  writechar( '*'' )

        linepos  :=  10
        writechar( '=' )
        writehexvalue( incr, 4 )

        IF  error.found  THEN
        $(
            linepos  :=  35
            writechar( 'E' )
            error.found  :=  no
        $)

        linepos :=  38
        writenumber( linenumber, 5 )

        IF  macrodepth > 0  &  NOT inmacro  THEN
        $(
            linepos  :=  43
            writechar( '+' )
        $)

        linepos  :=  44
        FOR  i = 0  TO  length-1  DO  writechar( inputbuff % i )

        printbuffer()

        listed  :=  yes
    $)

    setloc( newloc )
$)



AND checktagsize()  BE
    UNLESS  tagsize.given = ts.none  DO
            complain( 80 )



AND wordsized( operand )  =  -32768 <= operand <= +32767    |
                             (operand & #XFFFF) = operand



AND bytesized( operand )  =  -128 <= operand <= +127         |
                             (operand & #XFF) = operand



AND absolute( ea )        =   ea = s.abs16   |   ea = s.abs32



AND relocatable( ea )     =   ea = s.rel



AND checkregister( reg )  =  VALOF
$(
    LET rnum   =  reg!p.ptr1
    LET rsize  =  reg!p.ptr2

    TEST  rsize = ts.none
        THEN  RESULTIS  rnum
        ELSE  complain( 81 )
$)



AND checklabel( possible )  BE
    UNLESS  labelset = possible  DO
        complain(  possible  ->  82, 83 )



AND nextsymb()  BE
$(
//  Get the next symbol from the input stream, irrespective of layout
//  characters.

    skiplayout()
    readsymb()
$)



AND spacelines( n )  BE  IF  pass2  &  listing  THEN
$(
    clearbuffer()
    FOR  i = 1  TO  n  DO  printbuffer()
    listed  :=  yes
$)



AND printbuffer()  BE    IF   pass2  &  (error.found  |  listing)  THEN
$(
//  Print the output buffer to the "listing" output stream
//  On the IBM/370 the record output is done using WRITEREC for
//  efficiency.  On other machines, replace the IBM/370 code
//  by that in the following comments.
//  First, strip trailing spaces.

    LET linelength  =  0

    FOR  i = charsperline-1  TO  0  BY  -1  DO
         UNLESS  outbuff % i  =  '*S'  DO
         $(
             linelength  :=  i + 1
             BREAK
         $)

    IF  (onpage REM (linesperpage-5) = 0)  &  paging  THEN  pageheading()


$<CAP
    FOR  i = 0  TO  linelength-1  DO  wrch( outbuff % i )              /* CAP */
    newline()                                                          /* CAP */
$>CAP


$<68K
    FOR  i = 0  TO  linelength-1  DO  wrch( outbuff % i )              /* 68K */
    newline()                                                          /* 68K */
$>68K


$<370
    writerec( outbuff, linelength )                                    /* 370 */
$>370


    onpage  :=  onpage + 1
$)



AND pageheading()  BE  IF  pass2  &  paging  THEN
$(
    wrchsave    :=  wrch
    wrch        :=  wch
    linepos     :=  0
    pagenumber  :=  pagenumber  +  1

    writef( "*PMC68000 ASSEMBLER VERSION %N.%N   ", version, release )
    FOR  i = 0  TO  titlecharsmax-1  DO  wrch( titlevec % i )

    writef( " %S %S     PAGE %N*N*N", datestring, timestring, pagenumber )

    TEST  crossreference  THEN
          writes( "            SYMBOL               DEFN   VALUE          *
                  *   REFERENCES*N" )

    ELSE  
    
    TEST  errormessages  THEN
          writes( "             FILE                  STMT                *
                  *  ERROR MESSAGE*N" )

    ELSE  writes( "   LOC              OBJECT             STMT            *
                  *SOURCE STATEMENT*N" )

    writes( "*N*N" )

    wrch    :=  wrchsave
    onpage  :=  0
$)



AND wch( ch )  BE
$(
    TEST  ch = '*N'  THEN
    $(
        wrchsave( '*N' )
        linepos  :=  0
    $)
    ELSE

    UNLESS  linepos >= charsperline  DO
    $(
        wrchsave( ch )
        linepos  :=  linepos + 1
    $)
$)



AND bytesize( size )  =  VALOF
$(
    SWITCHON  size  INTO
    $(
        CASE ts.long    : RESULTIS 4
        CASE ts.word    : RESULTIS 2
        CASE ts.byte    : RESULTIS 1

        CASE ts.none    : RESULTIS bytesize( ts.default )

        DEFAULT         : complain( 0 )
    $)
$)



AND checkexpression( type, endofexpression )  BE
$(
//  Match the expression, just read in, with that which is
//  theoretically expected for the directive in "directive".
//
//  Check that:
//
//    a) The data type of the expression was correct
//    b) The expression was terminated correctly
//    c) It contained no forward references.

    TEST  forwardref       THEN  complain( 79 )   ELSE
    TEST  externalref      THEN  complain( 164 )  ELSE

    TEST  endofexpression  
        THEN  checkfor( s.none, 47 )  
        ELSE  checkfor( s.comma, 10 )

    SWITCHON  directive  INTO
    $(
        CASE d.equr    : // Requires "register" data type

                         UNLESS  type = s.Ar | type = s.Dr  DO
                                 complain( 84 )

                         ENDCASE


        CASE d.ifeq    :
        CASE d.ifne    :
        CASE d.spc     :
        CASE d.plen    :
        CASE d.llen    :
        CASE d.cnop    :
        CASE d.org     : // Requires "absolute" data type

                         UNLESS  absolute( type )  DO
                                 complain( 71 )

                         ENDCASE


        DEFAULT        : // All the rest require "relocatable" or
                         // "absolute" data types

                         UNLESS  absolute( type )  |  relocatable( type )  DO
                                 complain( 85 )
    $)
$)



AND listline()  BE  
    TEST  pass2  &  (listing  |  error.found)
        THEN  printline()
        ELSE  codeline()



AND printline()  BE  UNLESS  (listed  &  NOT error.found)    DO
$(
//  We are about to list a line...

    clearbuffer()

    linepos  :=  0

    UNLESS  commentline  DO
    $(
        writehexvalue( location, locmode = s.rel  ->  4, 6 )
        IF  locmode = s.rel  THEN  writechar( '*'' )
    $)

    linepos  :=  38
    writenumber( linenumber, 5 )

    IF  macrodepth > 0  &  NOT inmacro  THEN
    $(
        linepos  :=  43
        writechar( '+' )
    $)

    linepos  :=  44
    FOR  i = 0  TO  length - 1  DO  writechar( inputbuff % i )

    FOR  itemsprinted = 0  TO  nitems-1  DO
    $(
        LET offset  =  itemsprinted * cb.size
        LET dtype   =  codebuff!(offset + cb.dtype)
        LET dsize   =  codebuff!(offset + cb.dsize)
        LET dvalue  =  codebuff!(offset + cb.dvalue)
        LET dext    =  codebuff!(offset + cb.dext)
        LET dsymb   =  codebuff!(offset + cb.dsymb)

        IF  dext  THEN  addexternalref( dsymb, location )

        writebytes( dsize, dvalue )

        IF  dtype = s.rel  THEN  relocate( location, dsize )

        codebytes( dsize, dvalue )
    $)

    IF  error.found  THEN
    $(
        linepos  :=  35
        writechar( 'E' )
    $)

    printbuffer()
$)



AND codeline()  BE
$(
//  Acts just like "printline", except does not prepare the buffer for 
//  printing.  This is used in the first pass, and when the listing option
//  is switched off.

    FOR  itemscoded = 0  TO  nitems-1  DO
    $(
        LET offset  =  itemscoded * cb.size
        LET dtype   =  codebuff!(offset + cb.dtype)
        LET dsize   =  codebuff!(offset + cb.dsize)
        LET dvalue  =  codebuff!(offset + cb.dvalue)
        LET dext    =  codebuff!(offset + cb.dext)
        LET dsymb   =  codebuff!(offset + cb.dsymb)

        IF  dext  THEN  addexternalref( dsymb, location )

        IF  dtype = s.rel  THEN  relocate( location, dsize )

        codebytes( dsize, dvalue )
    $)
$)



AND writebytes( dsize, dvalue )  BE
    FOR  i = dsize-1  TO  0  BY  -1  DO
         writebyte( (dvalue >> i*8) & #XFF )



AND codebytes( dsize, dvalue )  BE
    FOR  i = dsize-1  TO  0  BY  -1  DO
         codebyte( (dvalue >> i*8) & #XFF )



AND writebyte( byte )  BE
$(
    IF  bytesonline = 8  THEN
    $(
        printbuffer()
        clearbuffer()

        commentline  :=  yes
        bytesonline  :=  0
    $)

    linepos  :=  bytesonline!( TABLE  11, 13, 17, 19, 23, 25, 29, 31 )

    writehexvalue( byte, 2 )

    bytesonline  :=  bytesonline + 1
$)



AND codebyte( byte )  BE
$(
    IF  pass2  THEN  codevec % location  :=  byte

    setloc( location + 1 )
$)



AND align( boundary )  BE
$(
    LET try   =  (location + boundary - 1)
    LET decr  =  try REM boundary

    setloc( try - decr )
$)



AND writehexvalue( h, d )  BE
$(
    IF  d > 1  THEN  writehexvalue( h >> 4, d-1 )
    writechar( (h & #XF)!  TABLE  '0', '1', '2', '3', '4', '5', '6', '7',
                                  '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' )
$)



AND writenumber( n, d )  BE
$(
    IF  d > 1  THEN  writenumber( n/10, d-1 )
    writechar(  n = 0  ->  '*S',  (n REM 10  +  '0') )
$)



AND writestring( string )  BE
    FOR  i = 1  TO  string % 0  DO
         writechar( string % i )



AND writechar( char )  BE
$(
    IF  linepos >= charsperline  THEN  RETURN

    outbuff % linepos  :=  char
    linepos            :=  linepos + 1
$)



AND clearbits()  BE
$(
//  Clear the bits in both the symbol tables.

    cleartable( tagtable1 )
    cleartable( tagtable2 )
$)



AND cleartable( tagtable )  BE
$(
//  Clear all the symbol table bits in the table "tagtable".

    FOR  i = 0  TO  tagtablesize-1  DO
    $(
        LET ptr  =  tagtable!i

        UNTIL  ptr = 0  DO
        $(
            UNLESS  ptr!st.definition = 0  DO
                ptr!st.flags  :=  ptr!st.flags  &  (NOT stb.setnow)

            ptr  :=  !ptr
        $)
    $)
$)



AND relocate( address, size )  BE
$(
    LET re  =  size = 4  ->  relocvec32,
               size = 2  ->  relocvec16,
                             complain( 0 )

    LET rp  =  size = 4  ->  @relp32,
               size = 2  ->  @relp16,
                             complain( 0 )

    LET p   =  !rp

    IF  pass2  THEN  re!p  :=  address

    !rp  :=  p + 1
$)



AND generate( masktype )  BE
$(
    SWITCHON  masktype  INTO
    $(
        CASE  1 :  swapoperands()
                   codeword(  instr.mask                            |
                              (op1.exp << 9)                        |
                              (sizefield( instr.size ) << 6)        |
                              (eafield())                           )
                   genea()

                   ENDCASE


        CASE  2 :  codeword(  instr.mask                            |
                              (sizefield( instr.size ) << 6)        |
                              (eafield())                           )

                              UNLESS  source.ea = 0  DO
                              $(
                                  //  There is some Immediate data to deal with.
                                  IF  instr.size = ts.long  THEN
                                       codeword(  op1.exp  >>  16  )
                                       codeword(  op1.exp & #XFFFF )
                              $)

                              genea()

                              ENDCASE


        CASE  4 :  IF  op1.ea = am.Ar  |  op1.ea = am.Dr  THEN  swapoperands()
                   codeword(  instr.mask   |   exp  )

                   UNLESS  source.ea = 0  DO   codeword(  op1.exp & #XFFFF  )

                   ENDCASE


        CASE  5 :  codeword(  instr.mask                            |
                              ((op1.exp & #B111) << 9)              |
                              (sizefield( instr.size )  <<  6)      |
                              (eafield())                           )
                   genea()

                   ENDCASE


        CASE  6 :  codeword(  instr.mask                            |
                              (source.ea  <<  8)                    |
                              (eafield())                           )
                   genea()

                   ENDCASE


        CASE  7 :  swapoperands()
                   codeword(  instr.mask                            |
                              (op1.exp  <<  9)                      |
                              (eafield())                           )
                   genea()

                   ENDCASE


        CASE  9 :  codeword(  instr.mask                            |
                              (eafield())                           )
                   genea()

                   ENDCASE


        CASE 10 :  codeword(  instr.mask                            |
                              ((instr.size = ts.long -> 1,0) << 6)  |
                              (exp)                                 )

                   ENDCASE


        CASE 15 :  codeword(  instr.mask  )
                   UNLESS  dest.ea = 0  DO  codeword( exp & #XFFFF )

                   ENDCASE


        DEFAULT :  complain( 0 )
    $)
$)



AND sizefield( size )  =  VALOF
$(
    SWITCHON  size  INTO
    $(
        CASE ts.long     : RESULTIS  #B10
        CASE ts.word     : RESULTIS  #B01
        CASE ts.byte     : RESULTIS  #B00
        CASE ts.none     : RESULTIS  sizefield( ts.default )

        CASE ts.short    : complain( 86 )
        DEFAULT          : complain( 0 )
    $)
$)



AND eafield()  =  VALOF
$(
//  Look at the effective address  represented by op.ea, etc.
//  and return the 6 bit representation of it.

    SWITCHON  op.ea  INTO
    $(
        CASE am.Dr           : RESULTIS #B000000  +  exp
        CASE am.Ar           : RESULTIS #B001000  +  exp
        CASE am.Ar.ind       : RESULTIS #B010000  +  exp
        CASE am.Ar.pi        : RESULTIS #B011000  +  exp
        CASE am.Ar.pd        : RESULTIS #B100000  +  exp
        CASE am.abs16        : RESULTIS #B111000
        CASE am.abs32        : RESULTIS #B111001
        CASE am.PC.disp      : RESULTIS #B111010
        CASE am.PC.index     : RESULTIS #B111011

//      CASE am.imm3         :
//      CASE am.imm16        :
//      CASE am.imm32        :
        DEFAULT              : RESULTIS #B111100

        CASE am.Ar.disp      : RESULTIS #B101000  +  registers!p.ptr1
        CASE am.Ar.index     : RESULTIS #B110000  +  registers!p.ptr0!p.ptr1
    $)
$)



AND genea()  BE
$(
    LET bs  =  0

    SWITCHON  op.ea  INTO
    $(
        CASE am.Ar           : IF  instr.size = ts.byte  THEN  complain( 29 )
        CASE am.Dr           :
        CASE am.Ar.ind       :
        CASE am.Ar.pi        :
        CASE am.Ar.pd        : 

                               RETURN


        CASE am.Ar.disp      : //  registers  ->  (Ar, regnum, rsize)

                               TEST  pass1  THEN  codeword( 0 )
                               ELSE

                               TEST  registers!p.ptr0 = s.Ar  THEN
                                   TEST  registers!p.ptr2 = ts.none
                                         THEN  codeword( exp )
                                         ELSE  complain( 81 )

                               ELSE  complain( 134 )

                               ENDCASE


        CASE am.Ar.index     : //  registers  ->  ((Ar, rnum, rsize), (Ir, rnum, rsize ))

                               TEST  pass1  THEN  codeword( 0 )
                               ELSE
                               $(
                                   LET Ar  =  registers!p.ptr0
                                   LET Ir  =  registers!p.ptr1
                                   LET It  =  Ir!p.ptr0
                                   LET In  =  Ir!p.ptr1
                                   LET Is  =  Ir!p.ptr2

                                   LET l   =  Is = ts.long  ->  1,
                                              Is = ts.word  ->  0,
                                              Is = ts.none  ->  0,
                                                                complain( 87 )

                                   LET r   =  It = s.Ar     ->  1,  0

                                   UNLESS  -128 <= exp <= +127  DO  complain( 72 )

                                   TEST  Ar!p.ptr2 = ts.none
                                         THEN  codeword( (r  << 15)    |
                                                         (In << 12)    |
                                                         (l  << 11)    |
                                                         (exp & #XFF)  )

                                   ELSE  complain( 81 )
                               $)

                               ENDCASE


        CASE am.abs32        : IF  pass2  &  externalref  THEN
                                   addexternalref( externalsymb, location + codewords*2  )

                               codeword( exp >> 16    )

        CASE am.abs16        : codeword( exp & #XFFFF )

                               ENDCASE


        CASE am.PC.disp      : //  The current program counter and the expression
                               //  MUST be of the same data type.

                               TEST  pass1  THEN  codeword( 0 )
                               ELSE
                               $(
                                   LET pc  =  location + (codewords * 2)
                                   LET o   =  exp - pc

                                   UNLESS  (locmode = s.abs  &  absolute( exptype ))  |
                                           (locmode = s.rel  &  relocatable( exptype ))  DO

                                           complain( 88 )

                                   UNLESS  -32768 <= o <= +32767  DO  complain( 177 )

                                   codeword( o & #XFFFF )
                               $)

                               ENDCASE


        CASE am.PC.index     : TEST  pass1  THEN  codeword( 0 )
                               ELSE
                               $(
                                   LET Ir  =  registers
                                   LET It  =  Ir!p.ptr0
                                   LET In  =  Ir!p.ptr1
                                   LET Is  =  Ir!p.ptr2

                                   LET l   =  Is = ts.long  ->  1,
                                              Is = ts.word  ->  0,
                                              Is = ts.none  ->  0,
                                                                complain( 87 )

                                   LET r   =  It = s.Ar     ->  1,  0

                                   UNLESS  (locmode = s.abs  &  absolute( exptype ))  |
                                           (locmode = s.rel  &  relocatable( exptype ))  DO

                                           complain( 88 )

                                   exp  :=  exp - (location + 2)

                                   UNLESS  -128 <= exp <= +127  DO  complain( 72 )

                                   codeword(  (r  <<  15)    |
                                              (In << 12)     |
                                              (l  <<  11)    |
                                              (exp & #XFF)   )
                               $)

                               ENDCASE


//      CASE am.imm16        :
//      CASE am.imm32        :
        DEFAULT              : //  Immediate data.  The size is given
                               //  by "instr.size"

                               bs  :=  bytesize( instr.size )

                               IF  bs = 4  &  (pass2 & externalref)  THEN
                                   addexternalref( externalsymb, location + codewords*2 )

                               IF    bs = 4  THEN  codeword( exp >> 16 )
                               TEST  bs = 1  THEN  codeword( exp & #XFF )
                                             ELSE  codeword( exp & #XFFFF )
    $)
$)



AND addexternalref( symbol, address )  BE  IF  pass2  THEN
$(
    LET s  =  extrnsymbols

    UNTIL  s = 0  DO
    $(
        TEST  s!e.symbol = symbol  THEN
        $(
            LET refsr   =  s!e.refsr
            LET refsa   =  s!e.refsa
            LET countr  =  s!e.countr
            LET counta  =  s!e.counta

            TEST  locmode = s.abs  THEN
            $(
                //  Update the absolute list.

                s!e.refsa   :=  heap2( refsa, address )
                s!e.counta  :=  counta + 1
            $)
            ELSE
            $(
                //  Update the relocatable list

                s!e.refsr   :=  heap2( refsr, address )
                s!e.countr  :=  countr + 1
            $)

            RETURN
        $)

        ELSE  s  :=  s!e.link
    $)

    complain( 0 )
$)



AND codeword( word )  BE
$(
    codewords  :=  codewords + 1

    stackvalue( s.abs16, 2, word, no, 0 )
$)




AND stackvalue( dtype, dsize, dvalue, dext, dsymb )  BE
$(
    LET offset  =  nitems * cb.size

    codebuff!(offset + cb.dtype)   :=  dtype
    codebuff!(offset + cb.dsize)   :=  dsize
    codebuff!(offset + cb.dvalue)  :=  dvalue
    codebuff!(offset + cb.dext)    :=  dext
    codebuff!(offset + cb.dsymb)   :=  dsymb

    nitems                         :=  nitems + 1

    IF  nitems > codesize  THEN  error( 178 )
$)



AND clearbuffer()  BE
    FOR  i = 0  TO  maxllen-1  DO  outbuff % i  :=  '*S'



AND swapoperands()  BE
$(
    LET t1  =  op.ea
    LET t2  =  exptype
    LET t3  =  exp
    LET t4  =  registers
    LET t5  =  externalref
    LET t6  =  externalsymb

    op.ea             :=  op1.ea
    exptype           :=  op1.exptype
    exp               :=  op1.exp
    registers         :=  op1.registers
    externalref       :=  op1.externalref
    externalsymb      :=  op1.externalsymb

    op1.ea            :=  t1
    op1.exptype       :=  t2
    op1.exp           :=  t3
    op1.registers     :=  t4
    op1.externalref   :=  t5
    op1.externalsymb  :=  t6
$)





AND setloc( newloc )  BE
$(
    UNLESS  (newloc & addressmask) = 0  DO  complain( 138 )

    IF  newloc > maxloc  THEN  maxloc  :=  newloc
    IF  newloc < minloc  THEN  minloc  :=  newloc

    location  :=  newloc
$)



AND changemode( newmode )  BE
$(
    UNLESS  locmode = newmode  DO
    $(
        TEST  locmode = s.abs  THEN
        $(
            absmin      :=  minloc
            absmax      :=  maxloc
            absloc      :=  location
            absrp16     :=  relp16
            absrp32     :=  relp32
            minloc      :=  relmin
            maxloc      :=  relmax
            location    :=  relloc
            codevec     :=  relvec
            relocvec16  :=  relrvec16
            relocvec32  :=  relrvec32
            relp16      :=  relrp16
            relp32      :=  relrp32
        $)
        ELSE
        $(
            relmin      :=  minloc
            relmax      :=  maxloc
            relloc      :=  location
            relrp16     :=  relp16
            relrp32     :=  relp32
            minloc      :=  absmin
            maxloc      :=  absmax
            location    :=  absloc
            codevec     :=  absvec
            relocvec16  :=  absrvec16
            relocvec32  :=  absrvec32
            relp16      :=  absrp16
            relp32      :=  absrp32
        $)
    $)

    locmode  :=  newmode
$)



AND triposmodule()  BE
$(
//  Output the object module.

    LET o     =  output()
    LET eabs  =  countextrnsymbols( e.counta )
    LET erel  =  countextrnsymbols( e.countr )

    selectoutput( codestream )

    //  First output the Relocatable section...
    //  Buffered in units of 4 bytes.

    UNLESS  relmax = 0  DO
    $(
        LET r  =  relmax/bytesper68000word

$<68K
        IF  toobig( r )  THEN  error( 167 )                            /* 68K */
$>68K

        systemword( t.hunk )
        writeword( r )
        writewords( relvec, r )
    $)

    // If it has any 16 bit relocation information, this next

    UNLESS  relrp16 = 0   DO
    $(
$<68K
        IF  toobig( relrp16 )  THEN  error( 168 )                      /* 68K */
$>68K

        systemword( t.reloc16 )
        writeword( relrp16 )
        writewordvec( relrvec16, relrp16 )
    $)

    //  Now the 32 bit relocation info

    UNLESS  relrp32 = 0  DO
    $(
$<68K
        IF  toobig( relrp32 )  THEN  error( 169 )                      /* 68K */
$>68K

        systemword( t.reloc32 )
        writeword( relrp32 )
        writewordvec( relrvec32, relrp32 )
    $)


    //  We must now put out the external references in the relocatable
    //  section, and the internal definitions of both sections.


    UNLESS  entrysymbols = 0  &  erel = 0  DO
    $(
        LET ptr  =  entrysymbols

        systemword( t.ext )

        UNTIL  ptr = 0  DO
        $(
            LET symbol  =  ptr!e.symbol
            LET type    =  symbol!st.type  &  st.type.mask
            LET value   =  symbol!st.value
            LET name    =  symbol+st.name
            LET l       =  name % 0
            LET length  =  l > maxextlength  ->  maxextlength, l
            LET size    =  maxextlength = 7  ->  2, 4
            LET buff    =  VEC 16/bytesperword

            buff % 0  :=  relocatable( type )  ->  ext.defrel, ext.defabs

            FOR  i = 1  TO  length  DO
                 buff % i  :=  ascii.value( name % i )

            FOR  i = length + 1  TO  maxextlength  DO
                 buff % i  :=  ascii.value( '*S' )

            writewords( buff, size )
            writeword( value )

            ptr  :=  ptr!e.link
        $)

        //  Now do the external references.

        ptr  :=  extrnsymbols

        UNTIL  ptr = 0  DO
        $(
            LET symbol  =  ptr!e.symbol
            LET refs    =  ptr!e.refsr
            LET count   =  ptr!e.countr
            LET name    =  symbol+st.name
            LET l       =  name % 0
            LET length  =  l > maxextlength  ->  maxextlength, l
            LET size    =  maxextlength = 7  ->  2, 4
            LET buff    =  VEC 16/bytesperword

            UNLESS  count = 0  DO
            $(
                buff % 0  :=  ext.ref

                FOR  i = 1  TO  length  DO
                     buff % i  :=  ascii.value( name % i )

                FOR  i = length + 1  TO  maxextlength  DO
                     buff % i  :=  ascii.value( '*S' )

                writewords( buff, size )
                writeword( count )

                FOR  i = 1  TO  count  DO
                $(
                    writeword( refs!r.address )

                    refs  :=  refs!r.link
                $)
            $)

            ptr  :=  ptr!e.link
        $)

        systemword( 0 )
    $)

    //  Now the absolute section - very much the same as before.

    UNLESS  absmax = 0  DO
    $(
        LET a  =  (absmax - absmin)/bytesper68000word

$<68K
        IF  toobig( a )  THEN  error( 170 )                            /* 68K */
$>68K

        systemword( t.abshunk )
        writeword( absmin/bytesper68000word )
        writeword( a )
        writewords( absvec + absmin/bytesperword, a )
    $)

    // If it has any 16 bit relocation information, this next

    UNLESS  absrp16 = 0   DO
    $(
$<68K
        IF  toobig( absrp16 )  THEN  error( 171 )                      /* 68K */
$>68K

        systemword( t.absrel16 )
        writeword( absrp16 )
        writewordvec( absrvec16, absrp16 )
    $)

    //  Now the 32 bit relocation info

    UNLESS  absrp32 = 0  DO
    $(
$<68K
        IF  toobig( absrp32 )  THEN  error( 172 )                      /* 68K */
$>68K

        systemword( t.absrel32 )
        writeword( absrp32 )
        writewordvec( absrvec32, absrp32 )
    $)

    //  Now the external references for the absolute section.

    UNLESS  eabs = 0  DO
    $(
        LET ptr  =  extrnsymbols

        systemword( t.ext )

        UNTIL  ptr = 0  DO
        $(
            LET symbol  =  ptr!e.symbol
            LET refs    =  ptr!e.refsa
            LET count   =  ptr!e.counta
            LET name    =  symbol+st.name
            LET l       =  name % 0
            LET length  =  l > maxextlength  ->  maxextlength, l
            LET size    =  maxextlength = 7  ->  2, 4
            LET buff    =  VEC 16/bytesperword

            UNLESS  count = 0  DO
            $(
                buff % 0  :=  ext.ref

                FOR  i = 1  TO  length  DO
                     buff % i  :=  ascii.value( name % i )

                FOR  i = length + 1  TO  maxextlength  DO
                     buff % i  :=  ascii.value( '*S' )

                writewords( buff, size )
                writeword( count )

                FOR  i = 1  TO  count  DO
                $(
                    writeword( refs!r.address )

                    refs  :=  refs!r.link
                $)
            $)

            ptr  :=  ptr!e.link
        $)

        systemword( 0 )
    $)

    systemword( t.end )

$<370
    newline()                                                          /* 370 */
$>370

$<CAP
    newline()                                                          /* CAP */
$>CAP

    selectoutput( o )
$)



$<68K
//  The following function is needed if the version of TRIPOS is       /* 68K */
//  using the Cambridge File Server directly, without the Filing       /* 68K */
//  Machine to split up large blocks                                   /* 68K */
//                                                                     /* 68K */
//  AND toobig( v )  =  v > (32767 / bytesperword)                     /* 68K */
                                                                       /* 68K */
AND toobig( v )  =  FALSE                                              /* 68K */
$>68K



AND countextrnsymbols( offset )  =  VALOF
$(
    LET count  =  0
    LET ptr    =  extrnsymbols

    UNTIL  ptr = 0  DO
    $(
        count  :=  count + ptr!offset
        ptr    :=  ptr!e.link
    $)

    RESULTIS  count
$)



$<370
AND systemword( word )  BE                                             /* 370 */
$(                                                                     /* 370 */
    writef( "*N*N%X8", word )                                          /* 370 */
    totalwords  :=  0                                                  /* 370 */
$)                                                                     /* 370 */
                                                                       /* 370 */
                                                                       /* 370 */
                                                                       /* 370 */
AND writewords( wordvec, words )  BE                                   /* 370 */
    FOR  i = 0  TO  words-1  DO  writeword( wordvec!i )                /* 370 */
                                                                       /* 370 */
                                                                       /* 370 */
                                                                       /* 370 */
AND writewordvec( wordvec, words )  BE                                 /* 370 */
    FOR  i = 0  TO  words-1  DO  writeword( wordvec!i )                /* 370 */
                                                                       /* 370 */
                                                                       /* 370 */
                                                                       /* 370 */
AND writeword( word )  BE                                              /* 370 */
$(                                                                     /* 370 */
    IF  totalwords REM 8  =  0  THEN  newline()                        /* 370 */
    writef( "%X8  ", word )                                            /* 370 */
    totalwords  :=  totalwords + 1                                     /* 370 */
$)                                                                     /* 370 */
$>370



$<CAP
AND systemword( word )  BE                                             /* CAP */
$(                                                                     /* CAP */
    writef( "*N*N%X8", word )                                          /* CAP */
    totalwords  :=  0                                                  /* CAP */
$)                                                                     /* CAP */
                                                                       /* CAP */
                                                                       /* CAP */
                                                                       /* CAP */
AND writewords( wordvec, words )  BE                                   /* CAP */
    FOR  i = 0  TO  words-1  DO  writeword( wordvec!i )                /* CAP */
                                                                       /* CAP */
                                                                       /* CAP */
                                                                       /* CAP */
AND writewordvec( wordvec, words )  BE                                 /* CAP */
    FOR  i = 0  TO  words-1  DO  writeword( wordvec!i )                /* CAP */
                                                                       /* CAP */
                                                                       /* CAP */
                                                                       /* CAP */
AND writeword( word )  BE                                              /* CAP */
$(                                                                     /* CAP */
    IF  totalwords REM 8  =  0  THEN  newline()                        /* CAP */
    writef( "%X8  ", word )                                            /* CAP */
    totalwords  :=  totalwords + 1                                     /* CAP */
$)                                                                     /* CAP */
$>CAP



$<68K
AND systemword( word )  BE  writeword( word )                          /* 68K */
                                                                       /* 68K */
                                                                       /* 68K */
                                                                       /* 68K */
AND writeword( word )  BE  writewords( @word, 1 )                      /* 68K */
                                                                       /* 68K */
                                                                       /* 68K */
                                                                       /* 68K */
AND writewordvec( wordvec, words )  BE  writewords( wordvec, words )   /* 68K */
$>68K



AND motorolamodule()  BE
$(
// Output a Motorola type Object Module.  The Specification of this module
// does not allow for relocation, and so, if the user has compiled relocatable
// code, this is an error.

    LET o  =  output()

    UNLESS  relmax = 0  &  relrp16 = 0  &  relrp32 = 0  DO
    $(
        selectoutput( sysout )
        writes( "MOTOROLA module cannot handle Relocatable code*N" )
        selectoutput( o )

        RETURN
    $)

    UNLESS  extrnsymbols = 0  &  entrysymbols = 0  DO
    $(
        selectoutput( sysout )
        writes( "MOTOROLA module cannot handle External Symbols*N" )
        selectoutput( o )

        RETURN
    $)

    selectoutput( codestream )

    $(  //  Loop to write out the records of the module.

        LET cs  =  0

        FOR  addr = absmin  TO  absmax-1  BY  32  DO
        $(
            LET left    =  absmax - addr
            LET nbytes  =  left > 32  ->  32, left
            LET length  =  4 + nbytes

            cs  :=  length + ((addr)       & #XFF) + 
                             ((addr >> 8)  & #XFF) +
                             ((addr >> 16) & #XFF)

            writef( "S2%X2", length )

            writehex( addr, 6 )

            FOR  i = addr  TO  addr + nbytes - 1  DO
            $(
                LET byte  =  absvec % i

                cs  :=  cs + byte

                writehex( byte, 2 )
            $)

            writef( "%X2*N", NOT cs )
        $)

        cs    :=  length + ((absmin)       & #XFF) + 
                           ((absmin >> 8)  & #XFF) +
                           ((absmin >> 16) & #XFF)

        writef( "S804%X6%X2*N", absmin, NOT cs )
    $)

    UNLESS  absrp16 = 0  &  absrp32 = 0  DO
    $(
        selectoutput( sysout )
        writes( "MOTOROLA module cannot deal with Relocation within code*N" )
    $)

    selectoutput( o )
$)



AND intelhexmodule()  BE
$(
//  Output an INTEL standard HEX module.  This will work with both
//  absolute and relocatable code, provided that there are no 32-bit
//  relocatable values involved.  Unfortunately, it is not possible to mix
//  Relocatable and Absolute code in this module format, and so if both have
//  been produced, this is also an error.

    LET o  =  output()

    UNLESS  relmax = 0  NEQV  absmax = 0  DO
    $(
        selectoutput( sysout )

        UNLESS  relmax = 0  &  absmax = 0  DO
            writes( "INTEL HEX module cannot deal with mixed Absolute and *
                    *Relocatable code*N" )

        selectoutput( o )

        RETURN
    $)

    UNLESS  relrp32 = 0  &  absrp32 = 0  DO
    $(
        selectoutput( sysout )
        writes( "INTEL HEX module cannot deal with 32-bit relocation*N" )
        selectoutput( o )

        RETURN
    $)

    UNLESS  extrnsymbols = 0  &  entrysymbols = 0  DO
    $(
        selectoutput( sysout )
        writes( "INTEL HEX module cannot handle External symbols*N" )
        selectoutput( o )

        RETURN
    $)

    $(  //  Loop to write the records of the INTEL format.

        LET absm   =  relmax = 0

        LET base   =  absm  ->  absmin, 0
        LET size   =  absm  ->  (absmax - absmin), relmax

        LET top    =  base + size

        LET bvec   =  absm  ->  absvec,    relvec
        LET rvec   =  absm  ->  absrvec16, relrvec16
        LET rvecp  =  absm  ->  absrp16,   relrp16

        selectoutput( codestream )

        writes( absm -> "$      0500FE*N", "$      0501FD*N" )

        FOR  addr = base  TO  top-1  BY  32  DO
        $(
            LET left    =  top - addr
            LET nbytes  =  left > 32  ->  32, left

            LET lbyte   =  (addr)       &  #XFF
            LET hbyte   =  (addr >> 8)  &  #XFF

            LET cs      =  nbytes + lbyte + hbyte

            UNLESS  wordsized( addr )  DO
            $(
                selectoutput( sysout )
                writes( "INTEL HEX module cannot handle 24-bit addresses*N" )
                selectoutput( o )
                RETURN
            $)

            writef( ":%X2%X2%X200", nbytes, hbyte, lbyte )

            FOR  i = addr  TO  addr + nbytes - 1  DO
            $(
                LET byte  =  bvec % i

                cs  :=  cs + byte

                writehex( byte, 2 )
            $)

            writef( "%X2*N", -cs )
        $)

        //  Now the relocation information.

        FOR  i = 0  TO  rvecp-1  BY  16  DO
        $(
            LET nwords  =  rvecp - i
            LET nbytes  =  (nwords > 16  ->  16, nwords) * 2
            LET cs      =  nbytes + 4

            writef( "$%X2000004", nbytes )

            FOR  j = 0  TO  nbytes/2 - 1  DO
            $(
                LET reladdr  =  rvec!j
                LET lbyte    =  (reladdr)       &  #XFF
                LET hbyte    =  (reladdr >> 8)  &  #XFF

                UNLESS  wordsized( reladdr )  DO
                $(
                    selectoutput( sysout )
                    writes( "INTEL HEX module cannot handle 24-bit relocation *
                            *addresses" )
                    selectoutput( o )
                    RETURN
                $)

                writehex( hbyte, 2 )
                writehex( lbyte, 2 )

                cs  :=  cs + hbyte + lbyte
            $)

            writef( "%X2*N", -cs )

            rvec  :=  rvec + nbytes/2
        $)

        writes( ":00000001FF*N" )
    $)

    selectoutput( o )
$)



.

//******************************************************************************
//*   Assembler for the Motorola MC68000 Microprocessor:  Section 5            *
//******************************************************************************



SECTION "M68KASM5"


GET "libhdr"

GET "m68khdr.h"



LET complain( code )  BE  asmerror( code, yes )



AND warning( code )   BE  asmerror( code, no )



AND asmerror( code, fatal )  BE
$(
    TEST  pass2  THEN
    $(
        LET offset  =  errors * eb.size

        TEST  errors = errorsize  THEN
        $(
            selectoutput( sysout )
            writef( "*N******  More than %N errors detected  -  assembly aborted.*N", errorsize )
            selectoutput( liststream )

            aborted  :=  yes
            ended    :=  yes         // premature ending
        $)
        ELSE  
        $(
            errorvec!(offset + eb.line)  :=  linenumber
            errorvec!(offset + eb.code)  :=  code
            errorvec!(offset + eb.file)  :=  currentfile
            errors                       :=  errors + 1

            selectoutput( sysout )

            writes( "******  " )

            FOR  i = 0  TO  length-1  DO  wrch( inputbuff % i )

            newline()

            writef( "******  File *"%S*" line %N  -  %S*N*N", 
                     currentfile, linenumber, message( code ) )

            selectoutput( liststream )
        $)
    $)
    ELSE  errors  :=  errors + 1

    error.found  :=  yes

    IF  fatal  THEN
    $(
        commentline  :=  yes

        longjump( recoverlevel, recoverlabel )
    $)
$)



AND error( code )  BE
$(
//  Fatal error, so print out the error message to the console, and then
//  wind up.

    selectoutput( sysout )

    writef( "*N******  Pass %N, line %N.  Fatal error  -  %S*N",
             (pass1 -> 1, (pass2 -> 2, 0)), linenumber, message( code ) )

    aborted  :=  yes
    ended    :=  yes

    longjump( fatalerrorp, fatalerror )
$)



AND checkfor( symbol, messagenumber )  BE
    TEST  symb = symbol  THEN  readsymb()
                         ELSE  complain( messagenumber )



AND message( code )  =  VALOF
$(
    SWITCHON  code  INTO
    $(
        CASE  0  :  RESULTIS  "INTERNAL ERROR IN ASSEMBLER  -  PLEASE REPORT"
        CASE  1  :  RESULTIS  "Size specifier is illegal on Label"
        CASE  2  :  RESULTIS  "Garbage found in Opcode field"
        CASE  3  :  RESULTIS  "Illegal Opcode field"
//      CASE  4  :  RESULTIS  "'END' statement missing"
        CASE  5  :  RESULTIS  "Garbage found in Label field"
        CASE  6  :  RESULTIS  "Illegal size specifier for this Opcode"
        CASE  7  :  RESULTIS  "Illegal address mode for this Operand"
        CASE  8  :  RESULTIS  "Illegal address mode for First Operand"
        CASE  9  :  RESULTIS  "Illegal address mode for Second Operand"
        CASE 10  :  RESULTIS  "',' expected after First Operand"
        CASE 11  :  RESULTIS  "Too many operands for this Opcode"
        CASE 12  :  RESULTIS  "Garbage found after Operand field"
        CASE 13  :  RESULTIS  "First Operand must be of 'DATA' mode"
        CASE 14  :  RESULTIS  "First Operand must be of 'DATA REGISTER' mode"
        CASE 15  :  RESULTIS  "Second Operand must be of 'MEMORY ALTERABLE' mode"
        CASE 16  :  RESULTIS  "Illegal address mode for Relative Branch"
        CASE 17  :  RESULTIS  "Label must be of 'RELOCATABLE' type"
        CASE 18  :  RESULTIS  "Label must be of 'ABSOLUTE' type"
        CASE 19  :  RESULTIS  "Short Branch displacement of ZERO is illegal"
        CASE 20  :  RESULTIS  "Location out of range for Short Branch"
        CASE 21  :  RESULTIS  "Location out of range for Long Branch"
        CASE 22  :  RESULTIS  "Second Operand must be of 'DATA REGISTER' mode"
        CASE 23  :  RESULTIS  "Second Operand must be of 'DATA ALTERABLE' mode"
        CASE 24  :  RESULTIS  "Illegal register combination for 'EXG' Opcode"
        CASE 25  :  RESULTIS  "Operand must be of 'IMMEDIATE' mode"
        CASE 26  :  RESULTIS  "Immediate value out of range of 4-Bit Unsigned Number"
        CASE 27  :  RESULTIS  "First Operand must be of 'IMMEDIATE' mode"
        CASE 28  :  RESULTIS  "Inconsistent Size Specifier for 'SR/CCR' Operand"
        CASE 29  :  RESULTIS  "'.B' size specifier illegal for 'ADDRESS REGISTER' mode"
        CASE 30  :  RESULTIS  "Second Operand must be of 'ADDRESS REGISTER' mode"
        CASE 31  :  RESULTIS  "First Operand must be of 'ADDRESS REGISTER POST INCREMENT' mode"
        CASE 32  :  RESULTIS  "Second Operand must be of 'ADDRESS REGISTER POST INCREMENT' mode"
        CASE 33  :  RESULTIS  "Second Operand must be of 'DATA' mode"
        CASE 34  :  RESULTIS  "Illegal Size Specifier for 'SR' Operand (W expected)"
        CASE 35  :  RESULTIS  "Illegal Size Specifier for 'USP' Operand (L expected)"
        CASE 36  :  RESULTIS  "First Operand must be of 'ADDRESS REGISTER' mode"
        CASE 37  :  RESULTIS  "'SHORT' Size Specifier illegal for 'MOVE' Opcode"
        CASE 38  :  RESULTIS  "Illegal Size Specifier for 'MOVEM' Opcode (W or L expected)"
        CASE 39  :  RESULTIS  "Register Range must be for Registers of same type"
        CASE 40  :  RESULTIS  "Illegal Size Specifier in 'REGISTER MASK'"
        CASE 41  :  RESULTIS  "Register missing or malformed in 'REGISTER MASK'"
        CASE 42  :  RESULTIS  "Second Operand must be of 'ADDRESS REGISTER + OFFSET' mode"
        CASE 43  :  RESULTIS  "First Operand must be of 'ADDRESS REGISTER + OFFSET' mode"
        CASE 44  :  RESULTIS  "Illegal Size Specifier for 'MOVEP' Opcode (W or L expected)"
        CASE 45  :  RESULTIS  "'QUICK' Operand value out of range of 8-Bit Signed Number"
        CASE 46  :  RESULTIS  "Illegal Size Specifier for 'ORG' directive (W or L expected)"
        CASE 47  :  RESULTIS  "Garbage found after Directive"
        CASE 48  :  RESULTIS  "Invalid length parameter for 'DS/DCB' directive"
        CASE 49  :  RESULTIS  "'TTL' string longer than 60 characters"
//      CASE 50  :  RESULTIS  "Unimplemented Directive!"
        CASE 51  :  RESULTIS  "INTERNAL ERROR:  Phasing Difference  -  PLEASE REPORT"
        CASE 52  :  RESULTIS  "'REGISTER' is illegal in Label field"
        CASE 53  :  RESULTIS  "'INSTRUCTION' is illegal in Label field"
        CASE 54  :  RESULTIS  "'DIRECTIVE' is illegal in Label field"
        CASE 55  :  RESULTIS  "Illegal Label field"
        CASE 56  :  RESULTIS  "Malformed 'SHIFT' Operator"
        CASE 57  :  RESULTIS  "Closing 'QUOTE' missing from 'ASCII LITERAL'"
        CASE 58  :  RESULTIS  "'ASCII LITERAL' Longer than 4 characters"
        CASE 59  :  RESULTIS  "Illegal character in Source File"
        CASE 60  :  RESULTIS  "Malformed Number"
        CASE 61  :  RESULTIS  "Too few Operands for this Opcode"
        CASE 62  :  RESULTIS  "Register or 'PC' missing after '('"
        CASE 63  :  RESULTIS  "First Register after '(' must be 'ADDRESS REGISTER' or 'PC'"
        CASE 64  :  RESULTIS  "Register missing afer ','"
        CASE 65  :  RESULTIS  "')' missing after Register"
        CASE 66  :  RESULTIS  "')' missing after Registers"
        CASE 67  :  RESULTIS  "'-' or '/' after Register only valid in 'REGISTER MASK'"
        CASE 68  :  RESULTIS  "Illegal use of 'REGISTER'"
        CASE 69  :  RESULTIS  "Overall Parenthesis Mismatch"
        CASE 70  :  RESULTIS  "Syntax Error in Expression"
        CASE 71  :  RESULTIS  "Symbol/Expression must be of 'ABSOLUTE' type"
        CASE 72  :  RESULTIS  "Index value out of range for 8-Bit Signed Number"
        CASE 73  :  RESULTIS  "Forward Reference must not be 'LONG ABSOLUTE' mode"
        CASE 74  :  RESULTIS  "Illegal negation of 'RELOCATABLE' value"
        CASE 75  :  RESULTIS  "Dyadic Operator must have 'ABSOLUTE' Operands"
        CASE 76  :  RESULTIS  "Illegal Operands for Diadic Operator"
        CASE 77  :  RESULTIS  "Illegal termination of 'CONSTANTS LIST'"
        CASE 78  :  RESULTIS  "Value out of range for 8-Bit Unsigned Number"
        CASE 79  :  RESULTIS  "Illegal Forward Reference"
        CASE 80  :  RESULTIS  "Size Specifier in illegal position"
        CASE 81  :  RESULTIS  "Size Specifier on 'REGISTER' illegal here"
        CASE 82  :  RESULTIS  "Statement must have Label field"
        CASE 83  :  RESULTIS  "Statement must not have Label field"
        CASE 84  :  RESULTIS  "Operand must be a Register"
        CASE 85  :  RESULTIS  "Invalid Operand type for this Directive"
        CASE 86  :  RESULTIS  "'.S' Size Specifier only valid on 'BRANCH' Opcodes"
        CASE 87  :  RESULTIS  "Illegal Size Specifier on Index Register (W or L expected)"
        CASE 88  :  RESULTIS  "Displacement type mismatch"
        CASE 89  :  RESULTIS  "Index value out of range for 16-Bit Signed Number"
        CASE 90  :  RESULTIS  "Tag Symbol longer than 30 characters"
        CASE 91  :  RESULTIS  "Illegal Size Specifier (B W L or S expected)"
        CASE 92  :  RESULTIS  "Multiply Defined Symbol"
        CASE 93  :  RESULTIS  "Workspace Exhausted"
        CASE 94  :  RESULTIS  "INTERNAL ERROR:  Parse Stack Overflow  -  PLEASE REPORT"
        CASE 95  :  RESULTIS  "Undefined Symbol in Label Field"
        CASE 96  :  RESULTIS  "Undefined Symbol in Opcode Field"
        CASE 97  :  RESULTIS  "Undefined Symbol in Operand Field"
//      CASE 98  :  RESULTIS  "Illegal Size Specifier for 'SIZE' Directive (B W or L expected)"
        CASE 99  :  RESULTIS  "Second Operand must be of 'ALTERABLE' mode"
        CASE 100 :  RESULTIS  "Invalid parameter for 'PLEN' directive"
        CASE 101 :  RESULTIS  "Invalid parameter for 'LLEN' directive"
        CASE 102 :  RESULTIS  "Instruction alignment error (Must be WORD aligned)"
        CASE 103 :  RESULTIS  "'ENDC' statement missing"
        CASE 104 :  RESULTIS  "Illegal use of 'SET' on a symbol defined by 'EQU'"
        CASE 105 :  RESULTIS  "Illegal use of 'EQU' on a symbol defined by 'SET'"
        CASE 106 :  RESULTIS  "Forward reference must not be to symbol defined by 'SET'"
        CASE 107 :  RESULTIS  "Mismatched 'ENDC' statement"
        CASE 108 :  RESULTIS  "Macro nesting too deep"
        CASE 109 :  RESULTIS  "Bad reference to Macro Operand"
        CASE 110 :  RESULTIS  "Illegally nested Macro Definitions"
        CASE 111 :  RESULTIS  "Mismatched 'ENDM' statement"
        CASE 112 :  RESULTIS  "Mismatched 'MEXIT' statement"
        CASE 113 :  RESULTIS  "'ENDM' statement missing"
        CASE 114 :  RESULTIS  "Mismatched Macro Brackets"
        CASE 115 :  RESULTIS  "Incorrect termination of Macro Operand Field"
//      CASE 116 :  RESULTIS  "Too many Assembler Generated Labels"
        CASE 117 :  RESULTIS  "Illegal use of Macro Operands outside a Macro Body"
        CASE 118 :  RESULTIS  "Too many operands for Macro"
        CASE 119 :  RESULTIS  "Illegal generation of 'END' in Macro Expansion"
        CASE 120 :  RESULTIS  "Illegal generation of 'ENDM' in Macro Expansion"
        CASE 121 :  RESULTIS  "Illegal generation of 'MACRO' in Macro Expansion"
        CASE 122 :  RESULTIS  "User 'FAIL' Statement"
        CASE 123 :  RESULTIS  "Byte value must not be of 'RELOCATABLE' type"
        CASE 124 :  RESULTIS  "Illegal re-definition of 'PLEN' parameter"
        CASE 125 :  RESULTIS  "Illegal re-definition of 'LLEN' parameter"
        CASE 126 :  RESULTIS  "Illegal use of 'END' within 'INCLUDE' file"
        CASE 127 :  RESULTIS  "Terminating 'QUOTE' missing from 'INCLUDE' argument"
        CASE 128 :  RESULTIS  "Input not provided for 'INCLUDE'"
        CASE 129 :  RESULTIS  "Malformed argument for 'INCLUDE'"
        CASE 130 :  RESULTIS  "'INCLUDE' nesting too deep"
        CASE 131 :  RESULTIS  "'ENDC' missing in 'INCLUDE' file"
        CASE 132 :  RESULTIS  "'ENDM' missing in 'INCLUDE' file"
//      CASE 133 :  RESULTIS  "Illegal generation of 'INCLUDE' in Macro Expansion"
        CASE 134 :  RESULTIS  "Register after '(' must be 'ADDRESS REGISTER' or 'PC'"
        CASE 135 :  RESULTIS  "Illegal use of 'INSTRUCTION' in Operand Field"
        CASE 136 :  RESULTIS  "Illegal use of 'DIRECTIVE' in Operand Field"
        CASE 137 :  RESULTIS  "Illegal use of 'MACRO NAME' in Operand Field"
        CASE 138 :  RESULTIS  "Address value too large"
        CASE 139 :  RESULTIS  "Illegal '#' found in expression"
        CASE 140 :  RESULTIS  "Illegal 'OPERATOR' found in expression"
        CASE 141 :  RESULTIS  "Unexpected end of expression"
        CASE 142 :  RESULTIS  "Illegal ')' found in expression"
        CASE 143 :  RESULTIS  "Illegal ',' found in expression"
        CASE 144 :  RESULTIS  "Illegal ':' found in expression"
        CASE 145 :  RESULTIS  "Illegal 'REGISTER' found in expression"
        CASE 146 :  RESULTIS  "Illegal '(An)/-(An)/(An)+' found in expression"
        CASE 147 :  RESULTIS  "Illegal 'SR/CCR/USP/PC' found in expression"
        CASE 148 :  RESULTIS  "Illegal forward reference to register defined by 'EQUR'"
        CASE 149 :  RESULTIS  "BREAK"
        CASE 150 :  RESULTIS  "Illegal 'CNOP' alignment value of ZERO"
        CASE 151 :  RESULTIS  "Illegal forward reference to MACRO name"
        CASE 152 :  RESULTIS  "Illegal use of 'XREF' symbol in BRANCH instruction"
        CASE 153 :  RESULTIS  "Undefined symbol in 'XDEF' list"
        CASE 154 :  RESULTIS  "Illegal 'XREF' symbol in 'XDEF' list"
        CASE 155 :  RESULTIS  "Illegal symbol found in 'XDEF' list"
        CASE 156 :  RESULTIS  "Illegal termination of 'XDEF' list"
        CASE 157 :  RESULTIS  "'XREF' symbol is already defined"
        CASE 158 :  RESULTIS  "Illegal symbol found in 'XREF' list"
        CASE 159 :  RESULTIS  "Illegal termination of 'XREF' list"
        CASE 160 :  RESULTIS  "'XREF' symbol is illegal in Label Field"
        CASE 161 :  RESULTIS  "Illegal size for 'XREF' symbol (Must be L)"
        CASE 162 :  RESULTIS  "Illegal use of 'XREF' symbol as displacement"
        CASE 163 :  RESULTIS  "Illegal arithmetic on 'XREF' symbol"
        CASE 164 :  RESULTIS  "Illegal use of 'XREF' symbol as argument to 'DIRECTIVE'"
        CASE 165 :  RESULTIS  "'XDEF' Symbol is too long"
        CASE 166 :  RESULTIS  "'XREF' Symbol is too long"
        CASE 167 :  RESULTIS  "TRIPOS Module HUNK too large"
        CASE 168 :  RESULTIS  "TRIPOS Module RELOC16 too large"
        CASE 169 :  RESULTIS  "TRIPOS Module RELOC32 too large"
        CASE 170 :  RESULTIS  "TRIPOS Module ABSHUNK too large"
        CASE 171 :  RESULTIS  "TRIPOS Module ABSREL16 too large"
        CASE 172 :  RESULTIS  "TRIPOS Module ABSREL32 too large"
        CASE 173 :  RESULTIS  "Overlong input record  -  truncated"
        CASE 174 :  RESULTIS  "Illegal forward reference to 'XREF' symbol"
        CASE 175 :  RESULTIS  "Illegal value for WORD sized operand"
        CASE 176 :  RESULTIS  "Illegal value for BYTE sized operand"
        CASE 177 :  RESULTIS  "'RELOCATABLE' symbol out of range"
        CASE 178 :  RESULTIS  "INTERNAL ERROR:  Code buffer overflow  -  PLEASE REPORT"
        CASE 179 :  RESULTIS  "INTERNAL ERROR:  Bad store request  -  PLEASE REPORT"
        CASE 180 :  RESULTIS  "Operand must be of 'CONTROL' mode"
        CASE 181 :  RESULTIS  "Illegal address mode for 'ABSOLUTE' jump"
        CASE 182 :  RESULTIS  "Location out of range for 'SHORT ABSOLUTE' jump"
        CASE 183 :  RESULTIS  "Illegal forward reference to symbol defined by 'REG'"
        CASE 184 :  RESULTIS  "'REGISTER MASK' is illegal in Label field"
        CASE 185 :  RESULTIS  "Illegal 'REGISTER MASK' found in expression"
        CASE 186 :  RESULTIS  "'STRING' arguments only valid with 'IFEQ/IFNE'"
        CASE 187 :  RESULTIS  "Opening 'QUOTE' missing from 'STRING'"
        CASE 188 :  RESULTIS  "Closing 'QUOTE' missing from 'STRING'"
        CASE 189 :  RESULTIS  "Macro expansion too long"

        DEFAULT  :  RESULTIS  "INTERNAL ERROR:  Undefined Error Code  -  PLEASE REPORT"
    $)
$)



AND write0( number, field )  BE
$(
    IF  field > 1  THEN  write0( number/10, field-1 )

    writechar( number REM 10  +  '0' )
$)



AND readtag()  BE
$(
//  Read a tag.  This is always guaranteed to be a "type 2" symbol, which we
//  can look up immediately.

    readtagsymbol( 0 )
    
    lookup( tagv, tagtable2 )
$)



AND readlabel()  BE

//  Read a label.  This should be a "type 2" symbol, but we don't actually
//  want to look it up yet.

    readtagsymbol( 0 )



AND readopcode()  BE
$(
//  Read an opcode.  This is always a "type 1" symbol, and can therefore
//  be looked up immediately.  The only valid alternative to an opcode symbol
//  is "end of line".

    TEST  ch = '*N'  THEN  symb  :=  s.none
    ELSE
    $(
        readtagsymbol( 2 )

        lookup( tagv, tagtable1 )
    $)
$)



AND readtagsymbol( errorcode )  BE
$(
//  Reads the tag, whose first character is in "ch".  We strip any suffix if
//  one has been given, but that is all at this stage.

    LET length  =  0

    WHILE  symbolchar( ch, TRUE )  DO
    $(  
        //  Loop to read the individual characters of the tag.  We keep on
        //  looking until we hit an invalid character, or the symbol gets
        //  too long.

        TEST  length = tagchars  THEN
        $(
            warning( 90 )

            //  The symbol is too long for us, so we will just take the
            //  first "tagchars" characters of it.  Ignore the rest of the
            //  tag.

            WHILE  symbolchar( ch, TRUE )  DO  rch()

            BREAK
        $)
        ELSE
        $(
            //  Take only the first "tagchars" characters of the
            //  symbol name.

            length         :=  length + 1
            tagv % length  :=  uppercase( ch )

            rch()
        $)
    $)

    //  We should check at this point to make sure that the first character is
    //  valid.  If not, the length will be zero.

    IF  length = 0  THEN  complain( errorcode )

    tagv % 0  :=  length
    
    IF  ch = '\'  THEN
    $(
        TEST  inmacro  
            THEN  UNTIL  ch = '*S'  |  ch = '*T'  |  ch = '*N'  DO  rch()
            ELSE  complain( 117 )

        tagsize.given  :=  ts.none

        RETURN
    $)

    //  This symbol could be followed by a ".", and then by a length
    //  specifier.  If so, we check for a suffix on the name we have read
    //  in, and strip the suffix.

    TEST  checksuffix( tagv, ".L" )  THEN  tagsize.given  :=  ts.long   ELSE
    TEST  checksuffix( tagv, ".W" )  THEN  tagsize.given  :=  ts.word   ELSE
    TEST  checksuffix( tagv, ".B" )  THEN  tagsize.given  :=  ts.byte   ELSE
    TEST  checksuffix( tagv, ".S" )  THEN  tagsize.given  :=  ts.short  ELSE

        //  No suffix given, so set the "tagsize.given" flag.
        
        tagsize.given  :=  ts.none
$)



AND checksuffix( string, suffix )  =  VALOF
$(
//  Check to see if the string given has suffix "suffix", and if so, strip
//  it.  Return a boolean to say what we have done.

    LET strl  =  string % 0
    LET sufl  =  suffix % 0
    
    TEST  strl > sufl  THEN
    $(
        //  It is possible that this is a correct suffix, and so we should do
        //  the check.  We search backwards...
        
        FOR  i = 0  TO  sufl-1  DO
        $(
            LET strch  =  uppercase( string % (strl - i) )
            LET sufch  =  uppercase( suffix % (sufl - i) )
            
            UNLESS  strch = sufch  DO  RESULTIS  FALSE
        $)
        
        //  If we drop through here, then the suffix matches, and so we should
        //  strip it.
        
        string % 0  :=  strl - sufl
        
        RESULTIS  TRUE
    $)
    ELSE  RESULTIS  FALSE
$)



AND uppercase( ch )   =   'a' <= ch <= 'z'  ->  ch - 'a' + 'A',  ch



AND symbolchar( char, digits )  =  uppercasechar( char )         |
                                   lowercasechar( char )         |
                                   (digits & digitchar( char ))  |
                                   ch = '_'  |  ch = '.'



AND macrochar( char )  =  uppercasechar( char )  |  digitchar( char )



$<370
AND uppercasechar( char )  =  VALOF                                    /* 370 */
$(                                                                     /* 370 */
//  See whether the character is an uppercase alphabetic character.    /* 370 */
//  We cannot do this the simple minded way, because "\" is part of    /* 370 */
//  the A-Z character set.                                             /* 370 */
                                                                       /* 370 */
    SWITCHON  char  INTO                                               /* 370 */
    $(                                                                 /* 370 */
        CASE 'A' :  CASE 'B' :  CASE 'C' :  CASE 'D' :                 /* 370 */
        CASE 'E' :  CASE 'F' :  CASE 'G' :  CASE 'H' :                 /* 370 */
        CASE 'I' :  CASE 'J' :  CASE 'K' :  CASE 'L' :                 /* 370 */
        CASE 'M' :  CASE 'N' :  CASE 'O' :  CASE 'P' :                 /* 370 */
        CASE 'Q' :  CASE 'R' :  CASE 'S' :  CASE 'T' :                 /* 370 */
        CASE 'U' :  CASE 'V' :  CASE 'W' :  CASE 'X' :                 /* 370 */
        CASE 'Y' :  CASE 'Z' :                                         /* 370 */
                                                                       /* 370 */
                    RESULTIS  TRUE                                     /* 370 */
                                                                       /* 370 */
                                                                       /* 370 */
        DEFAULT  :  RESULTIS  FALSE                                    /* 370 */
    $)                                                                 /* 370 */
$)                                                                     /* 370 */
                                                                       /* 370 */
                                                                       /* 370 */
                                                                       /* 370 */
AND lowercasechar( char )  =  VALOF                                    /* 370 */
$(                                                                     /* 370 */
//  See whether the character is a lowercase alphabetic character.     /* 370 */
                                                                       /* 370 */
    SWITCHON  char  INTO                                               /* 370 */
    $(                                                                 /* 370 */
        CASE 'a' :  CASE 'b' :  CASE 'c' :  CASE 'd' :                 /* 370 */
        CASE 'e' :  CASE 'f' :  CASE 'g' :  CASE 'h' :                 /* 370 */
        CASE 'i' :  CASE 'j' :  CASE 'k' :  CASE 'l' :                 /* 370 */
        CASE 'm' :  CASE 'n' :  CASE 'o' :  CASE 'p' :                 /* 370 */
        CASE 'q' :  CASE 'r' :  CASE 's' :  CASE 't' :                 /* 370 */
        CASE 'u' :  CASE 'v' :  CASE 'w' :  CASE 'x' :                 /* 370 */
        CASE 'y' :  CASE 'z' :                                         /* 370 */
                                                                       /* 370 */
                    RESULTIS  TRUE                                     /* 370 */
                                                                       /* 370 */
                                                                       /* 370 */
        DEFAULT  :  RESULTIS  FALSE                                    /* 370 */
    $)                                                                 /* 370 */
$)                                                                     /* 370 */
$>370



$<68K
AND uppercasechar( char )  =  'A' <= char <= 'Z'                       /* 68K */
AND lowercasechar( char )  =  'a' <= char <= 'z'                       /* 68K */
$>68K



$<CAP
AND uppercasechar( char )  =  'A' <= char <= 'Z'                       /* CAP */
AND lowercasechar( char )  =  'a' <= char <= 'z'                       /* CAP */
$>CAP



AND digitchar( char )  =  '0' <= char <= '9'



AND lookup( tagvector, tagtable )  BE
$(
//  Looks up the tag (packed in tagvector) in the symbol table.
//  If no entry exists, then a new one is created, with type
//  "s.new" and value 0.
//  Returns with "symbtype" pointing to the entry in the table.

    LET hashval   =  0
    LET offset    =  0
    LET length    =  tagvector % 0
    LET tagfound  =  FALSE
    LET noxref    =  (tagtable = tagtable1)

    FOR  i = 0  TO  (length < 10)  -> length, 10  DO
         hashval  :=  (hashval << 1) + tagvector % i

    hashval   :=  ABS (hashval)  REM  tagtablesize
    symbtype  :=  tagtable!hashval

    UNTIL  symbtype  =  0  |  tagfound  DO
    $(
        LET name  =  symbtype + st.name

        IF  length = name % 0  THEN

        tagfound  :=  VALOF
                      $(
                          FOR  i = 1  TO  length  DO
                               UNLESS  tagvector % i  =  name % i  DO
                                       RESULTIS  FALSE

                          RESULTIS  TRUE
                      $)

        UNLESS  tagfound  DO  symbtype  :=  symbtype!st.link
    $)

    IF  symbtype = 0  THEN
    $(
        //  This is a new entry, and we must create it.

        symbtype                :=  getstore( st.size + length/bytesperword )
        symbtype!st.link        :=  tagtable!hashval
        tagtable!hashval        :=  symbtype

        FOR  i = 0  TO  length  DO  (symbtype + st.name) % i  :=  tagvector % i

        symbtype!st.type        :=  s.new
        symbtype!st.flags       :=  0
        symbtype!st.value.high  :=  0
        symbtype!st.value.low   :=  0
        symbtype!st.definition  :=  (systemwords | noxref)  ->  0, cr.undefined
        symbtype!st.references  :=  0
    $)

    symb    :=  symbtype!st.type  &  st.type.mask
    regnum  :=  symbtype!st.value

    IF  pass2  THEN
    $(
        IF  xref  THEN
            UNLESS  symbtype!st.definition = 0  DO  
                addref( symbtype, linenumber )

        IF  symb = s.new                           THEN  undefined  :=  yes
        IF  (symbtype!st.flags & stb.muldef) \= 0  THEN  complain( 92 )
    $)
$)



AND addref( p, ln )  BE
$(
    LET t  =  p + st.references

    IF  symbtype!st.definition = ln  THEN  RETURN

    UNTIL  !t = 0  DO
    $(
        t  :=  !t + r.link

        IF  t!r.line = ln  &  t!r.file = currentfile  THEN  
        
            //  We have already added a reference for this item on this line
            //  in this file.  No more to be done.
            
            RETURN
    $)

    !t  :=  heap3( 0, ln, currentfile )
$)



AND heap3( a, b, c )  =  VALOF
$(
    LET s  =  getstore( 2 )

    s!0  :=  a
    s!1  :=  b
    s!2  :=  c

    RESULTIS  s
$)



AND heap2( a, b )  =  VALOF
$(
    LET s  =  getstore( 1 )

    s!0  :=  a
    s!1  :=  b

    RESULTIS  s
$)



AND skiprest()  BE  charpos  :=  maxint   //  Force a Pseudo SKIPREC



AND skiplayout()  BE  
$(
//  Skip layout characters, and a user comment (if one has been given).

    WHILE  ch = '*S'  |  ch = '*T'  DO  rch()

    IF  ch = '!'  THEN

        //  Comment symbol, so skip everything until end of line.
        
        UNTIL  ch = '*N'  DO  rch()
$)



AND rch()  BE
$(
    IF  charpos > length  THEN  nextline()

    ch       :=  ended  ->  endstreamch,  (inputbuff % charpos)
    charpos  :=  charpos + 1
$)



AND nextline()  BE
$(
    charpos  :=  0
    ended    :=  readline()
$)



AND readline()  =  VALOF
$(
//  Read the next input line from the source file.  On an IBM machine, we use
//  record I/O, and then copy the result in order to expand tabs.  On other
//  machines, we use character I/O, and expand tabs on the fly.

$<CAP
    LET char  =  rdch()                                                /* CAP */
                                                                       /* CAP */
    length      :=  0                                                  /* CAP */
    linenumber  :=  linenumber + 1                                     /* CAP */
                                                                       /* CAP */
    IF  char = endstreamch  THEN  RESULTIS  yes                        /* CAP */
                                                                       /* CAP */
    UNTIL  char = '*N'  |  char = endstreamch  DO                      /* CAP */
    $(                                                                 /* CAP */
        TEST  char = '*T'  THEN                                        /* CAP */
        $(                                                             /* CAP */
            //  Tab character, so expand it into spaces now, so as to  /* CAP */
            //  avoid copying later on.                                /* CAP */
                                                                       /* CAP */
            LET nlength  =  (length + tabspace)  &  tabmask            /* CAP */
                                                                       /* CAP */
            UNTIL  length = nlength  DO                                /* CAP */
            $(                                                         /* CAP */
                putinbuffer( inputbuff, length, '*S' )                 /* CAP */
                                                                       /* CAP */
                length  :=  length + 1                                 /* CAP */
            $)                                                         /* CAP */
        $)                                                             /* CAP */
        ELSE                                                           /* CAP */
        $(                                                             /* CAP */
            //  Simple character.  Add it to the buffer, and then go   /* CAP */
            //  back for more.                                         /* CAP */
                                                                       /* CAP */
            putinbuffer( inputbuff, length, char )                     /* CAP */
                                                                       /* CAP */
            length  :=  length + 1                                     /* CAP */
        $)                                                             /* CAP */
                                                                       /* CAP */
        char  :=  rdch()                                               /* CAP */
    $)                                                                 /* CAP */
$>CAP

$<68K
    LET char  =  rdch()                                                /* 68K */
                                                                       /* 68K */
    length      :=  0                                                  /* 68K */
    linenumber  :=  linenumber + 1                                     /* 68K */
                                                                       /* 68K */
    IF  char = endstreamch  THEN  RESULTIS  yes                        /* 68K */
                                                                       /* 68K */
    UNTIL  char = '*N'  |  char = endstreamch  DO                      /* 68K */
    $(                                                                 /* 68K */
        TEST  char = '*T'  THEN                                        /* 68K */
        $(                                                             /* 68K */
            //  Tab character, so expand it into spaces now, so as to  /* 68K */
            //  avoid copying later on.                                /* 68K */
                                                                       /* 68K */
            LET nlength  =  (length + tabspace)  &  tabmask            /* 68K */
                                                                       /* 68K */
            UNTIL  length = nlength  DO                                /* 68K */
            $(                                                         /* 68K */
                putinbuffer( inputbuff, length, '*S' )                 /* 68K */
                                                                       /* 68K */
                length  :=  length + 1                                 /* 68K */
            $)                                                         /* 68K */
        $)                                                             /* 68K */
        ELSE                                                           /* 68K */
        $(                                                             /* 68K */
            //  Simple character.  Add it to the buffer, and then go   /* 68K */
            //  back for more.                                         /* 68K */
                                                                       /* 68K */
            putinbuffer( inputbuff, length, char )                     /* 68K */
                                                                       /* 68K */
            length  :=  length + 1                                     /* 68K */
        $)                                                             /* 68K */
                                                                       /* 68K */
        char  :=  rdch()                                               /* 68K */
    $)                                                                 /* 68K */
$>68K

$<370
    LET tempbuff    =  VEC  maxllen/bytesperword                       /* 370 */
    LET templength  =  0                                               /* 370 */
                                                                       /* 370 */
    length      :=  readrec( tempbuff )                                /* 370 */
    linenumber  :=  linenumber + 1                                     /* 370 */
                                                                       /* 370 */
    IF  length = endstreamch  THEN  RESULTIS  yes                      /* 370 */
                                                                       /* 370 */
    IF  length < 0  THEN                                               /* 370 */
    $(                                                                 /* 370 */
        //  We did not read the whole record.  Skip the rest of the    /* 370 */
        //  record.                                                    /* 370 */
                                                                       /* 370 */
        skiprec()                                                      /* 370 */
                                                                       /* 370 */
        length  :=  ABS length                                         /* 370 */
    $)                                                                 /* 370 */
                                                                       /* 370 */
    //  Having read the record, we should now copy it to the main      /* 370 */
    //  buffer, expanding tabs as we go.                               /* 370 */
                                                                       /* 370 */
    FOR  i = 0  TO  length-1  DO                                       /* 370 */
    $(                                                                 /* 370 */
        LET char  =  getfrombuffer( tempbuff, i )                      /* 370 */
                                                                       /* 370 */
        TEST  char = '*T'  THEN                                        /* 370 */
        $(                                                             /* 370 */
            LET nlength  =  (templength + tabspace)  &  tabmask        /* 370 */
                                                                       /* 370 */
            UNTIL  templength = nlength  DO                            /* 370 */
            $(                                                         /* 370 */
                putinbuffer( inputbuff, templength, '*S' )             /* 370 */
                                                                       /* 370 */
                templength  :=  templength + 1                         /* 370 */
            $)                                                         /* 370 */
        $)                                                             /* 370 */
        ELSE                                                           /* 370 */
        $(                                                             /* 370 */
            //  Simple character.  Add it to the buffer, and then go   /* 370 */
            //  back for more.                                         /* 370 */
                                                                       /* 370 */
            putinbuffer( inputbuff, templength, char )                 /* 370 */
                                                                       /* 370 */
            templength  :=  templength + 1                             /* 370 */
        $)                                                             /* 370 */
    $)                                                                 /* 370 */
                                                                       /* 370 */
    length  :=  templength                                             /* 370 */
$>370
    
    //  Check to see whether the line has been truncated, and if it has,
    //  put out a warning message.

    UNLESS  length < maxllen  DO
    $(
        length  :=  maxllen

        warning( 173 )
    $)

    //  Strip trailing spaces, in case this hasn't been done by the
    //  run time system.

    FOR  i = length-1  TO  0  BY  -1  DO
         TEST  inputbuff % i  =  '*S'  
             THEN  length  :=  length - 1
             ELSE  BREAK

    inputbuff % length  :=  '*N'

    RESULTIS  no
$)



AND getfrombuffer( buffer, offset )  =

//  Read the "offset" character from the given buffer.  We check to see whether
//  we are within the range 0 -> maxllen-1, and if not, return a dummy 
//  character.

    offset < maxllen  ->  buffer % offset,  '*S'



AND putinbuffer( buffer, offset, char )  BE

//  Store the character "ch" at position "offset" in the buffer given.
//  We check to make sure that we are within range of 0 -> maxllen-1.

    IF  offset < maxllen  THEN
        buffer % offset  :=  char



AND declare( tagtable, words )  BE
$(
//  Take words, separated by '/' from the string "words" and creates symbol 
//  table entries for them.  A null word marks the end of "words".

    LET i       =  1
    LET length  =  0

    $(  // Main Decoding Loop

        LET ch  =  words % i

        TEST  ch = '/'  THEN
        $(
            //  We have read a complete word.
            //  If it is a null word, then we are at the end of the string

            LET t  =  0

            IF  length = 0  THEN  RETURN

            tagv % 0  :=  length

            lookup( tagv, tagtable )

            //  When we return from lookup, "symb" should point to the entry 
            //  we have just created.  Update the "type" and "value" fields of 
            //  the entry.  Data for  the updating comes from "datavector", 
            //  pointed to by "dataptr".

            symbtype!st.template    :=  dataptr!0

            symbtype!st.type        :=  (dataptr!1)         +  // Instruction type
                                        (dataptr!2  <<  4)     // Mask type

            symbtype!st.flags       :=  stb.setnow             // Always defined

            symbtype!st.value.high  :=  dataptr!3              // Source operand type
            symbtype!st.value.low   :=  dataptr!4              // Destination operand type

            dataptr                 :=  dataptr + 5

            length  :=  0
        $)
        ELSE
        $(
            //  Read the next character, trusting that no word
            //  is longer than "tagchars" characters

            length         :=  length + 1
            tagv % length  :=  ch
        $)

        i  :=  i + 1
    $)
    REPEAT
$)



AND block1( a )  =  VALOF
$(
    LET space  =  expspace( 1 )

    space!0  :=  a

    RESULTIS  space
$)



AND block2( a, b )  =  VALOF
$(
    LET space  =  expspace( 2 )

    space!0  :=  a
    space!1  :=  b

    RESULTIS  space
$)



AND block3( a, b, c )  =  VALOF
$(
    LET space  =  expspace( 3 )

    space!0  :=  a
    space!1  :=  b
    space!2  :=  c

    RESULTIS  space
$)



AND expspace( n )  =  VALOF
$(
    expvecp  :=  expvecp - n

    IF  expvecp < expvec  THEN  error( 94 )

    RESULTIS expvecp
$)



AND makefile( name )  =  VALOF
$(
//  Return a copy of the file name given, truncating it to a reasonable
//  number of characters if necessary.

    LET namel   =  name % 0
    LET length  =  namel > 30  ->  30, namel
    LET store   =  getstore( length/bytesperword )

    //  If we can copy the whole string, then all well and good.  If not, then
    //  we should put an indication that the file has been truncated.
    
    TEST  namel > length  THEN
    $(
        //  This means a truncation is necessary.  Take the last letters of
        //  the filename, but put some dots to imply truncation.
        
        FOR  i = 1  TO  3       DO  store % i  :=  '.'
        FOR  i = 4  TO  length  DO  store % i  :=  name % (namel - length + i)
    $)
    ELSE
    
        //  Easier.  All we need do is to copy the string across verbatim,
        //  since it fits anyway.
        
        FOR  i = 1  TO  namel  DO  store % i  :=  name % i

    //  Having copied the characters, we can set the length, and return a
    //  pointer to the copied string.

    store % 0  :=  length

    RESULTIS  store
$)



AND initstore( chunksize )  BE
$(
//  Initialise the storage package, defining the size of chunks which will
//  be grabbed from the standard storage manager.

    storage.chunksize   :=  chunksize
    storage.root        :=  0
    storage.high        :=  0
    storage.low         :=  0

    storage.wordsused   :=  0
    storage.totalwords  :=  0
$)



AND getstore( upb )  =  VALOF
$(
//  Analagous to "getvec"  -  allocate a vector whose word upperbound
//  is "upb" from the heap.  If there is not enough room in the current
//  chunk, then allocate a new chunk.

    LET size   =  upb + 1
    LET chunk  =  0

    IF  size > storage.chunksize  THEN  error( 179 )

    IF  (storage.high - storage.low)  <  size  THEN
    $(
        //  Not enough room left in the current chunk, so allocate a
        //  new chunk, and try again.

        LET newchunk  =  getchunk( storage.chunksize, FALSE )

        storage.low   :=  newchunk
        storage.high  :=  storage.low + storage.chunksize
    $)

    chunk              :=  storage.low
    storage.low        :=  storage.low + size
    storage.wordsused  :=  storage.wordsused + size

    RESULTIS  chunk
$)



AND getchunk( chunksize, allused )  =  VALOF
$(
//  Get a new chunk of store, and link it onto the chain of those chunks
//  which have been allocated already.

    LET size   =  chunksize + 1
    LET chunk  =  getvec( size )
    
    IF  chunk = 0  THEN
    $(
        //  We have failed to allocate the storage, and so we should do
        //  something about out.  Perhaps we can put out a helpful
        //  message?
        
        selectoutput( sysout )
        
        writef( "******  Failed to allocate %N words*N", size )
        writef( "******  %S*N", (size < 10000  ->  "Increase workspace size",
                                                   "Bad 'ORG' or 'DS' directive?") )
                                               
        selectoutput( liststream )
        
        error( 93 )
    $)

    chunk!0             :=  storage.root
    storage.root        :=  chunk
    storage.totalwords  :=  storage.totalwords + size + 1
    
    IF  allused  THEN  storage.wordsused  :=  storage.wordsused + size + 1

    RESULTIS  chunk + 1
$)



AND uninitstore()  BE
$(
//  Free all the storage in use by the storage package.  The base of the
//  storage chain is pointed to by "storage.root".

    UNTIL  storage.root = 0  DO
    $(
        LET next  =  storage.root!0

        freevec( storage.root )

        storage.root  :=  next
    $)
$)



AND printequates( stream, sourcename )  BE  UNLESS  stream = 0  DO
$(
//  Print out the symbol table as a series of equates which are acceptable
//  to the assembler.

    LET o  =  output()

    selectoutput( stream )

    printbanner()

    writef( "**  Equates for file *"%S*" written on %S at %S*N",
             sourcename, datestring, timestring )

    printbanner()

    newline()

    //  Now, scan the symbol table itself, and print out all equates we
    //  can.  Only absolute symbols are printed out properly.  
    
    FOR  i = 0  TO  tagtablesize-1  DO
    $(
        LET t  =  tagtable2!i
        
        UNTIL  t = 0  DO
        $(
            //  Look at the current item on the tag table chain, and decide
            //  whether to print it out.
            
            UNLESS  t!st.definition = 0  DO
            $(
                //  This is a user symbol, so decide how to print it out.
                
                LET line   =  t!st.definition
                LET type   =  t!st.type  &  st.type.mask
                LET value  =  t!st.value
                
                IF  line > 0  &  absolute( type )  THEN
                $(
                    LET name    =  t + st.name
                    LET length  =  name % 0
                    
                    writef( "%S  ", name )
                    
                    FOR  j = length  TO  30  DO  wrch( '*S' )
                    
                    writef( "EQU    $%X8*N", value )
                $)
            $)
            
            t  :=  t!st.link
        $)
    $)
        
    //  When we drop out of there, we can do no more than close the stream
    //  down, and return to the caller.
    
    endwrite()
    
    selectoutput( o )
$)



AND printbanner()  BE
$(
    FOR  i = 1  TO  80  DO  wrch( '**' )

    newline()
$)



AND dumpsymbols( stream, sourcename )  BE  UNLESS  stream = 0  DO
$(
//  Print out the symbol table in a form which as acceptable to the 68000
//  debugger.

    LET o  =  output()

    selectoutput( stream )

    writef( "[{!}]*N*
            *Symbol table of *"%S*" dumped on %S at %S*N",
             sourcename, datestring, timestring )

    //  Now, scan the symbol table itself, and print out all the symbols we
    //  can.
    
    FOR  i = 0  TO  tagtablesize-1  DO
    $(
        LET t  =  tagtable2!i
        
        UNTIL  t = 0  DO
        $(
            //  Look at the current item on the tag table chain, and decide
            //  whether to print it out.
            
            UNLESS  t!st.definition = 0  DO
            $(
                //  This is a user symbol, so decide how to print it out.
                
                LET line   =  t!st.definition
                LET type   =  t!st.type  &  st.type.mask
                LET value  =  t!st.value
                
                IF  line > 0  &  (absolute( type ) | relocatable( type ))  THEN
                $(
                    LET name    =  t + st.name
                    LET length  =  name % 0
                    
                    writef( "%S/", name )
                    writef( "%S/", absolute( type )  ->  "A", "R" )
                    writef( "%N*N", value )
                $)
            $)
            
            t  :=  t!st.link
        $)
    $)
        
    //  When we drop out of there, we can do no more than close the stream
    //  down, and return to the caller.
    
    endwrite()
    
    selectoutput( o )
$)

.

//******************************************************************************
//*   Assembler for the Motorola MC68000 Microprocessor:  Section 6            *
//******************************************************************************



SECTION "M68KASM6"



GET "libhdr"

GET "m68khdr.h"



LET declsyswords()  BE
$(
    datavector  :=  TABLE

/* Instr        Template    Type         Mask        Source       Destination  */
/* =====        ========    ====         ====        ======       ===========  */

/* ABCD    */    #XC100,   ins.2op.b,      0,           0,              1,
/* ADD     */    #XD000,   ins.2op.bwl,    0,           0,              3,
/* ADDA    */    #XD0C0,   ins.2op.wl,     0,           0,             13,
/* ADDI    */    #X0600,   ins.2op.bwl,    2,      am.imm,    am.data.alt,
/* ADDQ    */    #X5000,   ins.2op.bwl,    5,     am.imm3,         am.alt,
/* ADDX    */    #XD100,   ins.2op.bwl,    0,           0,              1,
/* AND     */    #XC000,   ins.2op.bwl,    0,           0,              2,
/* ANDI    */    #X0200,   ins.2op.bwl,    0,           0,             12,
/* ASL     */    #XE000,   ins.2op.bwl,    0,       #B001,              5,
/* ASR     */    #XE000,   ins.2op.bwl,    0,       #B000,              5,
/* BCC     */    #X6000,   ins.1op.l,      0,      #B0100,              4,
/* BCS     */    #X6000,   ins.1op.l,      0,      #B0101,              4,
/* BEQ     */    #X6000,   ins.1op.l,      0,      #B0111,              4,
/* BGE     */    #X6000,   ins.1op.l,      0,      #B1100,              4,
/* BGT     */    #X6000,   ins.1op.l,      0,      #B1110,              4,
/* BHI     */    #X6000,   ins.1op.l,      0,      #B0010,              4,
/* BHS     */    #X6000,   ins.1op.l,      0,      #B0100,              4,
/* BLE     */    #X6000,   ins.1op.l,      0,      #B1111,              4,
/* BLO     */    #X6000,   ins.1op.l,      0,      #B0101,              4,
/* BLS     */    #X6000,   ins.1op.l,      0,      #B0011,              4,
/* BLT     */    #X6000,   ins.1op.l,      0,      #B1101,              4,
/* BMI     */    #X6000,   ins.1op.l,      0,      #B1011,              4,
/* BNE     */    #X6000,   ins.1op.l,      0,      #B0110,              4,
/* BPL     */    #X6000,   ins.1op.l,      0,      #B1010,              4,
/* BVC     */    #X6000,   ins.1op.l,      0,      #B1000,              4,
/* BVS     */    #X6000,   ins.1op.l,      0,      #B1001,              4,
/* BCHG    */    #X0000,   ins.2op,        0,        #B01,              6,
/* BCLR    */    #X0000,   ins.2op,        0,        #B10,              6,
/* BRA     */    #X6000,   ins.1op.l,      0,      #B0000,              4,
/* BSET    */    #X0000,   ins.2op,        0,        #B11,              6,
/* BSR     */    #X6000,   ins.1op.l,      0,      #B0001,              4,
/* BTST    */    #X0000,   ins.2op,        0,        #B00,              6,
/* CHK     */    #X4180,   ins.2op.w,      7,     am.data,          am.Dr,
/* CLR     */    #X4200,   ins.1op.bwl,    2,           0,    am.data.alt,
/* CMP     */    #XB000,   ins.2op.bwl,    1,      am.all,          am.Dr,
/* CMPA    */    #XB0C0,   ins.2op.bwl,    0,           0,             13,
/* CMPI    */    #X0C00,   ins.2op.bwl,    2,      am.imm,    am.data.alt,
/* CMPM    */    #XB108,   ins.2op.bwl,    0,           0,             14,
/* DBCC    */    #X50C8,   ins.2op,        0,      #B0100,              4,
/* DBCS    */    #X50C8,   ins.2op,        0,      #B0101,              4,
/* DBEQ    */    #X50C8,   ins.2op,        0,      #B0111,              4,
/* DBF     */    #X50C8,   ins.2op,        0,      #B0001,              4,
/* DBGE    */    #X50C8,   ins.2op,        0,      #B1100,              4,
/* DBGT    */    #X50C8,   ins.2op,        0,      #B1110,              4,
/* DBHI    */    #X50C8,   ins.2op,        0,      #B0010,              4,
/* DBHS    */    #X50C8,   ins.2op,        0,      #B0100,              4,
/* DBLE    */    #X50C8,   ins.2op,        0,      #B1111,              4,
/* DBLO    */    #X50C8,   ins.2op,        0,      #B0101,              4,
/* DBLS    */    #X50C8,   ins.2op,        0,      #B0011,              4,
/* DBLT    */    #X50C8,   ins.2op,        0,      #B1101,              4,
/* DBMI    */    #X50C8,   ins.2op,        0,      #B1011,              4,
/* DBNE    */    #X50C8,   ins.2op,        0,      #B0110,              4,
/* DBPL    */    #X50C8,   ins.2op,        0,      #B1010,              4,
/* DBT     */    #X50C8,   ins.2op,        0,      #B0000,              4,
/* DBVC    */    #X50C8,   ins.2op,        0,      #B1000,              4,
/* DBVS    */    #X50C8,   ins.2op,        0,      #B1001,              4,
/* DBRA    */    #X50C8,   ins.2op,        0,      #B0001,              4,
/* DIVS    */    #X81C0,   ins.2op.w,      7,     am.data,          am.Dr,
/* DIVU    */    #X80C0,   ins.2op.w,      7,     am.data,          am.Dr,
/* EOR     */    #XB100,   ins.2op.bwl,    0,           0,             15,
/* EORI    */    #X0A00,   ins.2op.bwl,    0,           0,             12,
/* EXG     */    #XC100,   ins.2op.l,      0,        #B11,              7,
/* EXGD    */    #XC100,   ins.2op.l,      0,        #B01,              7,
/* EXGA    */    #XC100,   ins.2op.l,      0,        #B10,              7,
/* EXGM    */    #XC100,   ins.2op.l,      0,        #B11,              7,
/* EXT     */    #X4880,   ins.1op.wl,    10,           0,          am.Dr,
/* JMP     */    #X4EC0,   ins.1op.l,      0,           0,              9,
/* JSR     */    #X4E80,   ins.1op.l,      0,           1,              9,
/* LEA     */    #X41C0,   ins.2op.l,      7,    am.contr,          am.Ar,
/* LINK    */    #X4E50,   ins.2op,        4,       am.Ar,       am.imm16,
/* LSL     */    #XE008,   ins.2op.bwl,    0,       #B011,              5,
/* LSR     */    #XE008,   ins.2op.bwl,    0,       #B010,              5,
/* MOVE    */    #X0000,   ins.2op.bwl,    0,           0,              8,
/* MOVEA   */    #X0040,   ins.2op.bwl,    0,           1,              8,
/* MOVEM   */    #X4880,   ins.2op.wl,     0,           2,              8,
/* MOVEP   */    #X0008,   ins.2op.wl,     0,           3,              8,
/* MOVEQ   */    #X7000,   ins.2op.l,      0,           4,              8,
/* MULS    */    #XC1C0,   ins.2op.w,      7,     am.data,          am.Dr,
/* MULU    */    #XC0C0,   ins.2op.w,      7,     am.data,          am.Dr,
/* NBCD    */    #X4800,   ins.1op.b,      9,           0,    am.data.alt,
/* NEG     */    #X4400,   ins.1op.bwl,    2,           0,    am.data.alt,
/* NEGX    */    #X4000,   ins.1op.bwl,    2,           0,    am.data.alt,
/* NOP     */    #X4E71,   ins.zop,       15,           0,              0,
/* NOT     */    #X4600,   ins.1op.bwl,    2,           0,    am.data.alt,
/* OR      */    #X8000,   ins.2op.bwl,    0,           0,              2,
/* ORI     */    #X0000,   ins.2op.bwl,    0,           0,             12,
/* PEA     */    #X4840,   ins.1op.l,      9,           0,       am.contr,
/* RESET   */    #X4E70,   ins.zop,       15,           0,              0,
/* ROL     */    #XE018,   ins.2op.bwl,    0,       #B101,              5,
/* ROR     */    #XE018,   ins.2op.bwl,    0,       #B100,              5,
/* ROXL    */    #XE010,   ins.2op.bwl,    0,       #B111,              5,
/* ROXR    */    #XE010,   ins.2op.bwl,    0,       #B110,              5,
/* RTE     */    #X4E73,   ins.zop,       15,           0,              0,
/* RTR     */    #X4E77,   ins.zop,       15,           0,              0,
/* RTS     */    #X4E75,   ins.zop,       15,           0,              0,
/* SBCD    */    #X8100,   ins.2op.b,      0,           0,              1,
/* SCC     */    #X50C0,   ins.1op.b,      6,      #B0100,    am.data.alt,
/* SCS     */    #X50C0,   ins.1op.b,      6,      #B0101,    am.data.alt,
/* SEQ     */    #X50C0,   ins.1op.b,      6,      #B0111,    am.data.alt,
/* SF      */    #X50C0,   ins.1op.b,      6,      #B0001,    am.data.alt,
/* SGE     */    #X50C0,   ins.1op.b,      6,      #B1100,    am.data.alt,
/* SGT     */    #X50C0,   ins.1op.b,      6,      #B1110,    am.data.alt,
/* SHI     */    #X50C0,   ins.1op.b,      6,      #B0010,    am.data.alt,
/* SHS     */    #X50C0,   ins.1op.b,      6,      #B0100,    am.data.alt,
/* SLE     */    #X50C0,   ins.1op.b,      6,      #B1111,    am.data.alt,
/* SLO     */    #X50C0,   ins.1op.b,      6,      #B0101,    am.data.alt,
/* SLS     */    #X50C0,   ins.1op.b,      6,      #B0011,    am.data.alt,
/* SLT     */    #X50C0,   ins.1op.b,      6,      #B1101,    am.data.alt,
/* SMI     */    #X50C0,   ins.1op.b,      6,      #B1011,    am.data.alt,
/* SNE     */    #X50C0,   ins.1op.b,      6,      #B0110,    am.data.alt,
/* SPL     */    #X50C0,   ins.1op.b,      6,      #B1010,    am.data.alt,
/* ST      */    #X50C0,   ins.1op.b,      6,      #B0000,    am.data.alt,
/* SVC     */    #X50C0,   ins.1op.b,      6,      #B1000,    am.data.alt,
/* SVS     */    #X50C0,   ins.1op.b,      6,      #B1001,    am.data.alt,
/* STOP    */    #X4E72,   ins.1op,       15,           0,       am.imm16,
/* SUB     */    #X9000,   ins.2op.bwl,    0,           0,              3,
/* SUBA    */    #X90C0,   ins.2op.wl,     0,           0,             13,
/* SUBI    */    #X0400,   ins.2op.bwl,    2,      am.imm,    am.data.alt,
/* SUBQ    */    #X5100,   ins.2op.bwl,    5,     am.imm3,         am.alt,
/* SUBX    */    #X9100,   ins.2op.bwl,    0,           0,              1,
/* SWAP    */    #X4840,   ins.1op.w,      4,           0,          am.Dr,
/* TAS     */    #X4AC0,   ins.1op.b,      9,           0,    am.data.alt,
/* TRAP    */    #X4E40,   ins.1op,        0,           0,             11,
/* TRAPV   */    #X4E76,   ins.zop,       15,           0,              0,
/* TST     */    #X4A00,   ins.1op.bwl,    2,           0,    am.data.alt,
/* UNLK    */    #X4E58,   ins.1op,        4,           0,          am.Ar,


      /*  End of the instructions, now deal with the Directives  */
      /*  =====================================================  */

/* EQU     */         0,   s.dir,          0,           0,          d.equ,
/* EQUR    */         0,   s.dir,          0,           0,         d.equr,
/* SET     */         0,   s.dir,          0,           0,          d.set,
/* ORG     */         0,   s.dir,          0,           0,          d.org,
/* RORG    */         0,   s.dir,          0,           0,         d.rorg,
/* DC      */         0,   s.dir,          0,           0,           d.dc,
/* DS      */         0,   s.dir,          0,           0,           d.ds,
/* PAGE    */         0,   s.dir,          0,           0,         d.page,
/* LIST    */         0,   s.dir,          0,           0,         d.list,
/* NOLIST  */         0,   s.dir,          0,           0,       d.nolist,
/* NOL     */         0,   s.dir,          0,           0,       d.nolist,
/* SPC     */         0,   s.dir,          0,           0,          d.spc,
/* NOPAGE  */         0,   s.dir,          0,           0,       d.nopage,
/* LLEN    */         0,   s.dir,          0,           0,         d.llen,
/* PLEN    */         0,   s.dir,          0,           0,         d.plen,
/* TTL     */         0,   s.dir,          0,           0,          d.ttl,
/* NOOBJ   */         0,   s.dir,          0,           0,        d.noobj,
/* IFEQ    */         0,   s.dir,          0,           0,         d.ifeq,
/* IFNE    */         0,   s.dir,          0,           0,         d.ifne,
/* IFLT    */         0,   s.dir,          0,           0,         d.iflt,
/* IFLE    */         0,   s.dir,          0,           0,         d.ifle,
/* IFGT    */         0,   s.dir,          0,           0,         d.ifgt,
/* IFGE    */         0,   s.dir,          0,           0,         d.ifge,
/* ENDC    */         0,   s.dir,          0,           0,         d.endc,
/* MACRO   */         0,   s.dir,          0,           0,        d.macro,
/* ENDM    */         0,   s.dir,          0,           0,         d.endm,
/* MEXIT   */         0,   s.dir,          0,           0,        d.mexit,
//  /* SIZE    */     0,   s.dir,          0,           0,         d.size,
/* GET     */         0,   s.dir,          0,           0,          d.get,
/* INCLUDE */         0,   s.dir,          0,           0,          d.get,
/* END     */         0,   s.dir,          0,           0,          d.end,
/* FAIL    */         0,   s.dir,          0,           0,         d.fail,
/* CNOP    */         0,   s.dir,          0,           0,         d.cnop,
/* EXTRN   */         0,   s.dir,          0,           0,        d.extrn,
/* XREF    */         0,   s.dir,          0,           0,        d.extrn,
/* ENTRY   */         0,   s.dir,          0,           0,        d.entry,
/* XDEF    */         0,   s.dir,          0,           0,        d.entry,
/* REG     */         0,   s.dir,          0,           0,          d.reg,
/* DCB     */         0,   s.dir,          0,           0,          d.dcb,


    /*  Now the registers, and their Mnemonic Synonyms  */
    /*  ==============================================  */

/* D0      */         0,   s.Dr,           0,           0,              0,
/* D1      */         0,   s.Dr,           0,           0,              1,
/* D2      */         0,   s.Dr,           0,           0,              2,
/* D3      */         0,   s.Dr,           0,           0,              3,
/* D4      */         0,   s.Dr,           0,           0,              4,
/* D5      */         0,   s.Dr,           0,           0,              5,
/* D6      */         0,   s.Dr,           0,           0,              6,
/* D7      */         0,   s.Dr,           0,           0,              7,

/* A0      */         0,   s.Ar,           0,           0,              0,
/* A1      */         0,   s.Ar,           0,           0,              1,
/* A2      */         0,   s.Ar,           0,           0,              2,
/* A3      */         0,   s.Ar,           0,           0,              3,
/* A4      */         0,   s.Ar,           0,           0,              4,
/* A5      */         0,   s.Ar,           0,           0,              5,
/* A6      */         0,   s.Ar,           0,           0,              6,
/* A7      */         0,   s.Ar,           0,           0,              7,

/* SR      */         0,   s.SR,           0,           0,              0,
/* CCR     */         0,   s.CCR,          0,           0,              0,
/* SP      */         0,   s.Ar,           0,           0,              7,
/* USP     */         0,   s.USP,          0,           0,              0,
/* PC      */         0,   s.PC,           0,           0,              0


    dataptr   :=   datavector


    //  Now declare the Instruction Mnemonics.
    //  ======================================

    declare( tagtable1, "ABCD/ADD/ADDA/ADDI/ADDQ/ADDX/AND/ANDI/ASL/ASR/*
                        *BCC/BCS/BEQ/BGE/BGT/BHI/BHS/BLE/BLO/BLS/BLT/BMI/*
                        *BNE/BPL/BVC/BVS/BCHG/BCLR/BRA/BSET/BSR/BTST//" )

    declare( tagtable1, "CHK/CLR/CMP/CMPA/CMPI/CMPM/DBCC/DBCS/DBEQ/DBF/DBGE/*
                        *DBGT/DBHI/DBHS/DBLE/DBLO/DBLS/DBLT/DBMI/DBNE/DBPL/*
                        *DBT/DBVC/DBVS/DBRA/DIVS/DIVU//" )

    declare( tagtable1, "EOR/EORI/EXG/EXGD/EXGA/EXGM/EXT/JMP/JSR/LEA/LINK/*
                        *LSL/LSR/MOVE/MOVEA/MOVEM/MOVEP/MOVEQ/MULS/MULU/NBCD/*
                        *NEG/NEGX/NOP/NOT/OR/ORI//" )

    declare( tagtable1, "PEA/RESET/ROL/ROR/ROXL/ROXR/RTE/RTR/RTS/SBCD/*
                        *SCC/SCS/SEQ/SF/SGE/SGT/SHI/SHS/SLE/SLO/SLS/SLT/*
                        *SMI/SNE/SPL/ST/SVC/SVS//" )

    declare( tagtable1, "STOP/SUB/SUBA/SUBI/SUBQ/SUBX/SWAP/TAS/TRAP/TRAPV/*
                        *TST/UNLK//" )


    //  Now the directives:
    //  ===================

    declare( tagtable1, "EQU/EQUR/SET/ORG/RORG/DC/DS/PAGE/LIST/NOLIST/NOL/*
                        *SPC/NOPAGE/LLEN/PLEN/TTL/NOOBJ/IFEQ/IFNE/IFLT/IFLE/*
                        *IFGT/IFGE//" )

    declare( tagtable1, "ENDC/MACRO/ENDM/MEXIT/GET/INCLUDE/END/FAIL/CNOP/*
                        *EXTRN/XREF/ENTRY/XDEF/REG/DCB//" )


    //  Register names  and synonyms:
    //  =============================

    declare( tagtable2, "D0/D1/D2/D3/D4/D5/D6/D7/A0/A1/A2/A3/A4/A5/A6/A7/*
                        *SR/CCR/SP/USP/PC//" )
$)







