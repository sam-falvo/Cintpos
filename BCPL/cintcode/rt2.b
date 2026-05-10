/*
This program is a test forrast2wav.
It reada every Cintcode memory location from
word address 0 to 1_000_000
It runs for just over 7_000_000 Cintcode instruction executions.

The compiled code is:

pc/8          pc          instruction

3125       25004          K4G 1           After the break point in CLI

7447       59580          L1              Entry to start
7447       59581          SP3
7447       59582          LW 250000
7448       59587          SP4
7448       59588          JGR L4
7448       59590     L3:  L0P3
7448       59591          JNE0 L5
7449       59593     L5:  L1
7449       59594          AP3
7449       59595          SP3
7449       59596          LP4
7449       59597          JLE L3
7449       59599          L0
7450       59600          RTN
*/

GET "libhdr"

MANIFEST { upb=250000 }

LET start() = VALOF
{ FOR p = 1 TO upb DO IF !p LOOP
  RESULTIS 0
}
