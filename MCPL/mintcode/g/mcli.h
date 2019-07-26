// Header for the CLI and some commands.
//    (e.g. C, SPOOL, STACK, etc.)

MANIFEST
  Return_severe    =  20,
  Return_hard      =  10,
  Return_soft      =   5,
  Return_ok        =   0,
  Cli_module_gn    =  149,
  Cli_initialstack =  60000,
  Cli_faillevel    = Return_hard,
  Globword         = #xEFEF0000

GLOBAL
  cli_tallyflag:     132,
  cli_result2:       134,
  cli_returncode:    137,
  cli_commandname:   138,
  cli_prompt:        140,
  cli_standardinput: 141,
  cli_currentinput:  142,
  cli_commandfile:   143,
  cli_interactive:   144,
  cli_preloadlist:   145,
  cli_defaultstack:  147,
  cli_standardoutput:148,
  cli_module:        149
