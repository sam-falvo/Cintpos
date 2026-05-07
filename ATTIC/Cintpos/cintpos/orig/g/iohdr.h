/***********************************************************************
**             (C) Copyright 1980  TRIPOS Research Group              **
**            University of Cambridge Computer Laboratory             **
************************************************************************

            ########   ######   ##    ##  ######    #######
            ########  ########  ##    ##  #######   ########
               ##     ##    ##  ##    ##  ##    ##  ##    ##
               ##     ##    ##  ########  ##    ##  ########
               ##     ##    ##  ##    ##  ##    ##  #######
               ##     ##    ##  ##    ##  ##    ##  ##  ##
            ########  ########  ##    ##  #######   ##   ##
            ########   ######   ##    ##  ######    ##    ##

************************************************************************
**                                                                    **
***********************************************************************/


// TRIPOS Input/Output header.


MANIFEST
$( // General actions.
   Act_dummy      =1000
   Act_read       =1001
   Act_write      =1002
   Act_seek       =1008
   Act_endinput   =1003
   Act_endoutput  =1004
   Act_findinput  =1005
   Act_findoutput =1006
   Act_end        =1007
   Act_findupdate =1008
   Act_findappend =1009

   // Single character I/O through terminal handlers

   Act_sc_mode    = 994
   Act_sc_read    = 995
   Act_sc_write   = 996
   Act_sc_msg     = 997

   // Mag tape
   Act_offline    =1007
   Act_wreof      =1008
   Act_spacefw    =1009
   Act_spacerv    =1010
   Act_wreig      =1011
   Act_rewind     =1012


   // Device packet offset manifests.
   // Common:
   Pkt_action     =Pkt_type
   Pkt_status     =Pkt_res1
   Pkt_status2    =Pkt_res2
   // Timer:
   Pkt_time1      =Pkt_res1
   Pkt_time2      =Pkt_res2
   Pkt_delay      =Pkt_arg1
   // Disc & MT drivers:
   Pkt_buffAddr   =Pkt_arg1
   Pkt_wordCount  =Pkt_arg2
   Pkt_drive      =Pkt_arg3
   Pkt_unit       =Pkt_drive
   Pkt_cylinder   =Pkt_arg4
   Pkt_surface    =Pkt_arg5
   Pkt_sector     =Pkt_arg6

   // Stream control block.
   Id_inscb       =('S'<<BitsPerByte)+'I'
   Id_outscb      =('S'<<BitsPerByte)+'O'
   Id_updscb      =('S'<<BitsPerByte)+'U'
   Scb_link       =0
   Scb_id         =1
   Scb_type       =2
   Scb_buf        =3
   Scb_pos        =4
   Scb_end        =5
   Scb_funcs      =6
   Scb_func1      =6
   Scb_rdch       = Scb_func1
   Scb_func2      =7
   Scb_wrch       = Scb_func2
   Scb_func3      =8
   Scb_args       =9
   Scb_arg1       =9
   Scb_arg2       =10
   Scb_nfunc      =Scb_args-Scb_funcs
   Scb_upb        =10

   // Load format types
   T_hunk         =1000
   T_reloc        =1001
   T_end          =1002
   T_abshunk      =1003
   T_absreloc     =1004

   // Assignment vectors
   Ass_link       = 0
   Ass_task       = 1
   Ass_dir        = 2

   Ass_type       = 3
   Ass_dev        = 4
   Ass_name       = 5

   // Device types

   Dt_disc        = 1
$)
