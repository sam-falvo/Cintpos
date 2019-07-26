// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "PROMPT"

GET "libhdr"

LET start() = VALOF
{ LET prompt = "%5.3d> "
  LET v = VEC 15

  UNLESS rdargs("PROMPT,P0/S,P1/S,P2/S,P3/S,P4/S,*
                       *P5/S,P6/S,P7/S,p8/S,P9/S,NO/S",v,15) DO
  { writes("Parameters no good for PROMPT*N")
    RETURN
  }

  IF v!0  DO prompt := v!0                         // PROMPT
  IF v!1  DO prompt := "> "                        // P0/S
  IF v!2  DO prompt := "%+%+%n:%2z:%2z> "          // P1/S
  IF v!3  DO prompt := "%+%+%n:%2z:%2z.%3z> "      // P2/S
  IF v!4  DO prompt := "%5.3d %+%n:%2z:%2z> "      // P3/S
  IF v!5  DO prompt := "%5.3d %+%n:%2z:%2z.%3z> "  // P4/S
  IF v!6  DO prompt := "%+%n> "                    // P5/S
  IF v!7  DO prompt := "%+%n %n:%2z:%2z> "         // P6/S
  IF v!8  DO prompt := "%+%n %n:%2z:%2z.%3z> "     // P7/S
  IF v!9  DO prompt := "%5.3d %n %n:%2z:%2z> "     // P8/S
  IF v!10 DO prompt := "%5.3d %n %n:%2z:%2z.%3z> " // P9/S

  IF prompt FOR i = 0 TO prompt%0 DO cli_prompt%i := prompt%i

  TEST v!11                                        // NO/S
  THEN cli_status := cli_status |  clibit_noprompt
  ELSE cli_status := cli_status & ~clibit_noprompt
}
