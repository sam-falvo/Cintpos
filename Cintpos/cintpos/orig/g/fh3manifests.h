
MANIFEST
$(

   // coroutine stack sizes

   access.stsize     = 70
   locate.stsize     = 70
   rename.stsize     = 70
   create.stsize     = 70
   delete.stsize     = 70
   kill.stsize       = 20
   disc.stsize       = 60
   exobj.stsize      = 50
   exnext.stsize     = 50

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
   info.upb        = 64

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
   disctype.fs     = 93       // disc is a file server image

   discstate.writeprotected   = 80  // disc is write protected
   discstate.notvalidated     = 81  // disc is not validated
   discstate.validated        = 82  // disc is validated
$)


