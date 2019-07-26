// (c)  Copyright:   Martin Richards   August 1994
// MCPL i/o header file
  
MANIFEST
  Char_bs  = 8,

  // Stream control block.
  Id_inscb      = 1,
  Id_outscb     = 2,

  Scb_pos = 0,
  Scb_end,
  Scb_file,
  Scb_id,
  Scb_work,
  Scb_rdfn,
  Scb_wrfn,
  Scb_endfn,
  Scb_buf,
  Scb_bufstart  = Scb_buf*Bytesperword,      // first buf byte offset
  Scb_buflen    = 2048,
  Scb_bufend    = Scb_bufstart+Scb_buflen-1, // last buf byte offset
  Scb_upb       = Scb_bufend/Bytesperword
