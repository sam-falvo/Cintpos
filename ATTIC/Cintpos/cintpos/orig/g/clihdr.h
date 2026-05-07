// Header for TRIPOS CLI and some commands.
//    (e.g. C, SPOOL, STACK, etc.)

MANIFEST
$(
Return_severe  =  20
Return_hard    =  10
Return_soft    =   5
Return_ok      =   0
Flag_break     =   1
Flag_commbreak =   2
Cli_module_gn        =  149
Cli_initialstack     = 5000
Cli_initialfaillevel = Return_hard
$)

GLOBAL
$(
cli_init:          133
cli_result2:       134
cli_undefglobval:  135
cli_commanddir:    136
cli_returncode:    137
cli_commandname:   138
cli_faillevel:     139
cli_prompt:        140
cli_standardinput: 141
cli_currentinput:  142
cli_commandfile:   143
cli_interactive:   144
cli_background:    145
cli_currentoutput: 146
cli_defaultstack:  147
cli_standardoutput:148
cli_module:        149
$)
