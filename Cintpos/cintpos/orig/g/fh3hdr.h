
GLOBAL
$(
   disc.dev                   : ug + 0    // disc driver device number
   deviceID                   : ug + 0    // synonym for ex, discinfo, etc.
   unit.no                    : ug + 1    // unit number on device
   unitNo                     : ug + 1    // again synonym for ex etc.

   cokill                     : ug + 2    // coroutine killer !!!!
   locate                     : ug + 3    // locate coroutine code
   delete                     : ug + 4    // delete .....
   rename                     : ug + 5    // rename .....
   create                     : ug + 6    // create ......
   access                     : ug + 7    // access routine

   workfail                   : ug + 8    //          workfail ( pkt )
   checklock                  : ug + 9    //  key := checklock ( lock )
   getlock                    : ug + 10   // lock := getlock   (  key, access )
   freelock                   : ug + 11   //         freelock  ( lock )
   readblock                  : ug + 12   //  buf := readblock ( key, opt )
   writeblock                 : ug + 13   //         writeblock( buf, key )
   getblock                   : ug + 14   //  buf := getblock  ( hold )
   getkey                     : ug + 15   //  key := getkey    ( nearkey )
   freekey                    : ug + 16   //         freekey   ( key )

   type                       : ug + 17   // type := type ( buf )
   isdir                      : ug + 18   //  res := isdir( buf )
   setvec                     : ug + 19   //    v := setvec ( v, upb, a...l )
   hash                       : ug + 20   // hval := hash  ( name )
   append                     : ug + 21   //        append ( LVqueue, obj )
   checksum                   : ug + 22   // csum := checksum ( v, upb )

   disc                       : ug + 23   // disc driving coroutine

   kill.co                    : ug + 24   // killer coroutine
   disc.co                    : ug + 25   //   disc coroutine

   lock.queue                 : ug + 26   // current lock chain
   free.buffer.queue          : ug + 27   // free buffer queue !!
   valid.buffer.queue         : ug + 28   // valid .....
   pending.queue              : ug + 29   // disc action pending queue

   prev.key                   : ug + 30   // previous key set by find
   obj.key                    : ug + 31   //   object key ...........

   xfer                       : ug + 32   // xfer ( a, b, upb )

   disc.running               : ug + 33   // disc coroutine running flag
   buffer.waiting.queue       : ug + 34   // coroutines waiting for buffers
   fh3init                    : ug + 35   // initialisation
   fh3readinit                : ug + 36   // character read initialisation
   fh3writeinit               : ug + 37   // ........ write ........

   wrprot                     : ug + 38   // write protect flag
   writeprotect               : ug + 38   // synonym .....
   bitmap                     : ug + 39   // allocation bit map
   map                        : ug + 39   // synonym .....
   bitchk                     : ug + 40   // bit map checksum
   bitmap.upb                 : ug + 41   // bit map upper bound
   usedblocks                 : ug + 42   // number of allocated blocks

   exobject                   : ug + 46   // examine object
   exnext                     : ug + 47   // examine next object
   workdone                   : ug + 48   // workdone ( pkt, res )
   comment		      : ug + 49   // comment coroutine code

   b.file.upb                 : ug + 50   // file block size minus one
   b.file.keymax              : ug + 51   // file header key maximum

   waiting.queue              : ug + 52   // write-waiting queue

   freeblock                  : ug + 55   // freeblock ( buf, validity )

   maskv                      : ug + 56   // globals used....
   entry                      : ug + 57   // ...inside the...
   state                      : ug + 58   // ...bitmap section

   log.read                   : ug + 61   // logical disc reads
   log.write                  : ug + 62   // logical disc writes
   phy.read                   : ug + 63   // physical disc reads
   phy.write                  : ug + 64   // physical disc writes
   phy.preread                : ug + 65   // physical disc pre-reads
   cyl.seeks                  : ug + 66   // total seek distance
   cyl.at                     : ug + 67   // current cylinder position

   size.block                 : ug + 70   // disc block.size
   sector.origin              : ug + 71   // sector of origin
   n.surfaces                 : ug + 72   // number of surfaces
   n.secsperblock             : ug + 73   // sectors per block
   n.blockspertrk             : ug + 74   // blocks per track
   n.resblocks                : ug + 75   // reserved blocks
   prealloc                   : ug + 76   // preallocation factor
   intleave                   : ug + 77   // interleave factor
   lowercyl                   : ug + 78   // lower useable cylinder
   uppercyl                   : ug + 79   // upper .......
   n.blockspercyl             : ug + 80   // blocks per cylinder
   keylwb                     : ug + 81   // lowest available key
   keyupb                     : ug + 82   // maximum available key
   n.blocks                   : ug + 83   // number of blocks
   rootkey                    : ug + 84   // directory root key

   b.file.stype               : ug + 85   // secondary type offset
// b.file.infobits            : ug + 86
   b.file.parent              : ug + 87   // parent pointer offset
   b.file.hashchain           : ug + 88   // hashchain offset
   b.file.name                : ug + 89   // file name offset
   b.file.date                : ug + 90   // creation date offset
   b.file.infobase            : ug + 91
// b.file.keyvecbase          : ug + 91
   b.dir.hashmax              : ug + 92   // directory max. hash offset
   b.size.hashtab             : ug + 93   // director hash table size
// b.size.veryshortfile       : ug + 93
// size.fcb                   : ug + 94
// size.bitmap                : ug + 95
   b.file.extension           : ug + 96  // long file extension block

$)


MANIFEST
$(

   // coroutine stack sizes

   access.stsize     = 150
   locate.stsize     = 110
   rename.stsize     = 160
   create.stsize     = 150
   delete.stsize     = 110
   kill.stsize       = 25
   disc.stsize       = 80
   exobj.stsize      = 50
   exnext.stsize     = 50
   comm.stsize       = 100

   // block offsets

   buf.prefix      = 6        // size of buffer block prefix
   buf.chain       = 0        // chain linking for queues
   buf.co          = 1        // owning coroutine
   buf.pkt         = 1        // owning packet
   buf.state       = 2        // state on pending queue
   buf.next        = 3        // status after disc action
   buf.key         = 4        // key on pending and valid queues
   buf.buf         = 5        // pointer at actual buffer for getblock

   buf.valid       = TRUE     // buffer valid in freeblock
   buf.invalid     = FALSE    // buffer invalid....

   grab            = TRUE     // readblock grab before write....
   wait            = FALSE    // .... options

   // fixed disc block offsets

   b.file.type     = 0
   b.file.ownkey   = 1
   b.file.highseq  = 2
   b.file.datasize = 3
   b.file.firstkey = 4
   b.file.checksum = 5
   b.file.database = 6


   b.data.type     = 0
   b.data.hdrkey   = 1
   b.data.seq      = 2
   b.data.datasize = 3
   b.data.nextkey  = 4
   b.data.checksum = 5
   b.data.database = 6


   b.dir.hashtab   = 6


   // lock offsets

   lock.link       = 0
   lock.key        = 1
   lock.access     = 2
   lock.task       = 3
   lock.upb        = 3

   exclusive.lock  = -1
   shared.lock     = -2

   // queueing element offsets

   quel.link       = 0
   quel.co         = 1

   // file types

   t.deleted       = 1
   t.short         = 2
   t.long          = 4
   t.data          = 8
   t.list          = 16

   st.file         = -3
   st.root         = 1
   st.userdir      = 2

   // offsets into info vector for examine functions
   // these are fixed so that the same 'examine' command
   // will work for both real disc file handlers and
   // file servers handlers

   info.key        =  0       // disc key on drive
   info.dtype      =  1       // directory entry type
   info.name       =  2
   info.type       = 30       // actual entry type
   info.size       = 31
   info.date       = 33
   info.comment    = 36
   info.upb        = 64       // upper bound of vector

   // offsets into the info vector for the discinfo
   // action and the associated reply manifests

   discinfo.type   =  0       // disc type
   discinfo.unit   =  1       // unit number
   discinfo.state  =  2       // disc state
   discinfo.space  =  3       // space available
   discinfo.used   =  4       // space used
   discinfo.alloc  =  5       // unit of allocation for above

   disctype.floppy = 90       // disc is a floppy
   disctype.cart   = 91       // disc is a cartridge ( rk05 like )
   disctype.big    = 92       // big disc
   disctype.fs     = 93       // fileserver filing system
   disctype.real   = 94       // unspecified real disc

   discstate.writeprotected   = 80  // disc is write protected
   discstate.notvalidated     = 81  // disc is not validated
   discstate.validated        = 82  // disc is validated
$)


