/*
(c) Martin Richards  15 December 1997

Header file for: chk.h rnobj.m r2obj.m and tgobj.m

(rnobj.m is currently either r3obj.m or r8obj.m)
*/

GET "mcpl.h"

MANIFEST  E_Space,            // Exceptions
          E_FalseTermFound,
          E_NoTerms,
          E_Syntax

MANIFEST  Rn_Close,           // Methods for the Rn object
          Rn_AddTerm3,
          Rn_AddTerm4,
          Rn_AddTerm5,
          Rn_AddTerm8,
          Rn_PrTerms,
          Rn_Compact,
          Rn_Unit,
          Rn_Pair,
          Rn_Check,
          Rn_FnsSize

MANIFEST  Tg_Close,           // Methods for the termgen object
          Tg_Gen,
          Tg_FnsSize

MANIFEST  R2_Close,           // Methods for the R2 object
          R2_Put,
          R2_Eq,
          R2_Ne,
          R2_Simp8,
          R2_Intersect,
          R2_Unite,
          R2_Print,
          R2_FnsSize

GLOBAL    mkRnobj:Ug,         // Object constructors
          mkTgobj,
          mkR2obj,

          r2obj,
          rnobj,
          tgobj,

          change,
          debug,
          bug,

          // Functions working on 8-variable terms
          not7,not6,not5,not4,not3,not2,not1,not0,
          dcv7,dcv6,dcv5,dcv4,dcv3,dcv2,dcv1,dcv0,
          sw76,sw75,sw74,sw73,sw72,sw71,sw70,
          sw65,sw64,sw63,sw62,sw61,sw60,
          sw54,sw53,sw52,sw51,sw50,
          sw43,sw42,sw41,sw40,
          sw32,sw31,sw30,
          sw21,sw20,
          sw10

MANIFEST  NotY=#b00111100,    // 3-variable term operators
          And =#b10000111,
          Nand=#b01111000,
          Or  =#b11100001,
          Nor =#b00011110,
          Imp =#b10110100,
          Eqv =#b10010110,
          Xor =#b01101001

