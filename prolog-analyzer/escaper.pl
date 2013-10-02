:- module(escaper,[escaping_format/3, escape_single_argument/2, escaping_format/2]).

:- use_module(library(lists)).
:- use_module(library(codesio)).

escaping_format(S, FormatString,Arguments) :-
    maplist(escape_argument,Arguments,EscapedArguments),
    format(S, FormatString,EscapedArguments).

% for old analyzer only

escaping_format(FormatString,Arguments) :-
    maplist(escape_argument,Arguments,EscapedArguments),
    format(FormatString,EscapedArguments).
%

escape_argument(Arg,EscArg) :-
    format_to_codes('~w', [Arg], Codes),
    escape_codes_list(Codes,EscCodes),
    atom_codes(EscArg,EscCodes).

escape_codes_list([],[]).
escape_codes_list([34|T],[92,34|T2]) :-
    escape_codes_list(T,T2).
escape_codes_list([H|T],[H|T2]) :-
    escape_codes_list(T,T2).


escape_single_argument(Arg,EscArg) :-
    format_to_codes('~w', [Arg], Codes),
    escape_sequence_list(Codes,EscCodes),
    atom_codes(EscArg,EscCodes).

escape_sequence_list([],[]).
escape_sequence_list([60|T],[38,108,116,59|T2]) :-
	escape_sequence_list(T,T2).
escape_sequence_list([H|T],[H|T2]) :-
    escape_sequence_list(T,T2).
