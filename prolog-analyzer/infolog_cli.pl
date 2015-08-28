
% header file to start command-line version of infolog
% currently does not work; probably would require extended runtime license ??

infolog_start_cli :-
    prolog_flag(argv,ArgV),
    (infolog_run_cli(ArgV) -> true ; print('*** infolog_run_cli failed'),nl).

infolog_run_cli(ArgV) :-
     print(argv(ArgV)),nl,
     analyze(ArgV),
     lint.


:- include(analyzer).

runtime_entry(start) :- infolog_start_cli.

%save_infolog_cli :- save_program('infolog_cli.sav').

