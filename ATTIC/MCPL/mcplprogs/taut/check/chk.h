/*
(c) Martin Richards  13 November 1997

Header file for: chk.h rnobj.m and tgobj.m

(rnobj.m is currently either r3obj.m or r8obj.m)
*/

GET "mcpl.h"

MANIFEST  E_Space,            // Exceptions
          E_FalseTermFound,
          E_NoTerms,
          E_Syntax

MANIFEST  Rn_Init,            // Methods for the Rn object
          Rn_Close,
          Rn_AddTerm3,
          Rn_AddTerm4,
          Rn_AddTerm5,
          Rn_AddTerm8,
          Rn_PrTerms,
          Rn_PrMap,
          Rn_Compact,
          Rn_Unit,
          Rn_Pair,
          Rn_Check,
          Rn_FnsSize

MANIFEST  Tg_Init,            // Methods for the termgen object
          Tg_Close,
          Tg_Gen,
          Tg_FnsSize

MANIFEST  R2_Init,            // Method for the R2 object
          R2_Close,
          R2_Put,
          R2_Eq,
          R2_Ne,
          R2_Simp8,
          R2_Intesrsect,
          R2_Unite,
          R2_Print,
          R2_FnsSize

GLOBAL    mkRnobj:Ug,         // Object constructors
          mkTgobj,
          mkR2obj,
          change,
          debug,
          bug,
          not7,not6,not5,not4,not3,not2,not1,not0,
          dcv7,dcv6,dcv5,dcv4,dcv3,dcv2,dcv1,dcv0

MANIFEST  NotY=#b00111100,    // Term operators
          And =#b10000111,
          Nand=#b01111000,
          Or  =#b11100001,
          Nor =#b00011110,
          Imp =#b10110100,
          Eqv =#b10010110,
          Xor =#b01101001

