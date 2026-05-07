// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

/*      Initialisation of a TRIPOS disc
*/


// This version contains constants specifically for an 80 Mbyte disc.

SECTION "INITIALISE"


GET "libhdr"
GET "FH3manifests"


GET "CLIHDR"


MANIFEST
    $(
    rootkey     = 16000
    $)


GLOBAL
    $(
    Block       : ug
    UnitNo      : ug + 1
    Deviceid    : ug + 2
    $)




LET Start() BE
$( LET ArgV = VEC 100
   LET BlockV = VEC Size.Block-1+Size.CachePrefix
   LET Sum = 0
   LET Unit, DiscName, Partition = ?, ?, ?
   LET Floppy, HiCap = ?, ?
   LET Seg = ?
   LET Root = RootKey
   Block := BlockV

   IF RdArgs("UNIT/A/K,NAME/A/K,PARTITION/K,FLOPPY/S,HICAP/S",
                 ArgV, 100)=0 DO
   $( Writef("Bad arguments for %S*N", Cli.CommandName)
      Stop(20)
   $)

   Unit         := ArgV!0
   DiscName     := ArgV!1
   Partition    := ArgV!2
   Floppy       := ArgV!3\=0
   HiCap        := ArgV!4\=0

   UnitNo := Unit%1-'0'
   DeviceId := 0

   IF [UnitNo\=0 & UnitNo\=1] |
        DiscName%0=0 | DiscName%0>30 |
        Partition\=0 |
        [HiCap & UnitNo=0] |
        [Floppy EQV HiCap] DO
   $( Writef("Arguments not acceptable to %S:*N", Cli.CommandName)
      Writef("[UNIT %S NAME%S %S%S%S%S]*N",
             Unit, Discname,
             [Partition=0 -> "", " PARTITION "],
             [Partition=0 -> "", Partition],
             [Floppy -> " FLOPPY", ""],
             [HiCap -> " HICAP", ""])
      Stop(20)
   $)

// // Load device driver
// Seg := LoadSeg(":L.Floppy-Dev")

// // Create device
// UNLESS Seg=0 DO DeviceId := CreateDev(Seg)
// IF Seg=0 | DeviceId=0 DO
// $( Writes("Failed to create device driver*N")
//    Stop(20)
// $)

   TEST HiCap
   THEN DeviceId := -5
   ELSE DeviceId := -2

   // Clear BLOCK
   FOR i = 0 TO Size.Block-1 DO Block!i := 0

   Block!B.File.Type            := T.Short
   Block!B.File.DataSize        := Size.HashTable
   Block!B.File.SecondaryType   := ST.Root

   FOR i = 0 TO DiscName%0 DO
        [Block+B.File.Filename]%i := DiscName%i

   FOR i = 0 TO Size.Block-1 DO Sum := Sum+Block!i
   Block!B.File.CheckSum := -Sum

   IF Floppy DO
   $( Seek(2)
      Seek(-80)
   $)

   IF Floppy THEN Root := 231
   Device(Act.Write, Root, Block)

// DeleteDev(DeviceId)
// UnloadSeg(Seg)

   Writes("Device has been initialised*N")
$)

and checkblock() = true
