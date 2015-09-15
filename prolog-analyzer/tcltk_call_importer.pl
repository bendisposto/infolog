:- module(tcltk_call_importer,[process_file/1, tcltk_call/4, generate_prolog_file/1]).


process_file(Filename) :-
    print(opening(Filename)),nl,
    open(Filename,read,S),
    call_cleanup(read_all(S),close(S)),
    export(user_output).

read_all(S) :-
	read_line(S,Line),
	( Line==end_of_file -> true
	;  process_line(Line),
	    read_all(S)).

process_line(Line) :- %print(Line),nl,
  (split_list(Line,":",Res)
   -> register_tcltk_call(Res)
    ; format(user_error,'Could not process line: ~s~n',[Line])).

:- dynamic tcltk_call/4.
register_tcltk_call([File,Line,Call]) :- Call \= '', atom_to_number(Line,Nr), !,
   assert(tcltk_call(Call,undefined_module,File,Nr)).
register_tcltk_call([File,Line,Module,Call]) :-  Call \= '', atom_to_number(Line,Nr), !,
   assert(tcltk_call(Call,Module,File,Nr)).
register_tcltk_call(Res) :- format(user_error,'Could not process result ~w~n',[Res]).

atom_to_number(Atom,Number) :- atom(Atom), atom_codes(Atom,C), number_codes(Number,C).

split_list([],_,R) :- !,R=[].
split_list(List,Sep,Res) :- get_next_word(List,Sep,Word,Tail),!,
  (Word=[] -> split2(Tail,Sep,Res)
   ; Res=[Atom|TA], clever_codes_to_atom(Word,Atom), split_list(Tail,Sep,TA)).

clever_codes_to_atom([C|T],Atom) :- ignore(C),!,clever_codes_to_atom(T,Atom).
clever_codes_to_atom(Codes,Atom) :- atom_codes(Atom,Codes).
ignore(32). % whitespace
ignore(34). %'
ignore(39). % "
ignore(40). % (
ignore(123). % {

get_next_word([],_Sep,[],[]).
get_next_word([H|T],Sep,Word,Tail) :-
  member(H,Sep) -> Word=[],Tail=T
    ; Word=[H|TR], get_next_word(T,Sep,TR,Tail).

generate_prolog_file(Filename) :- open(Filename,write,S),
     write(S,'% Prolog calls from tcl Files'),nl(S),
     write(S,':- module(tcltk_calls, [tcltk_call/4]).'),nl(S),
     call_cleanup(export(S),close(S)).

export(S) :- tcltk_call(Call,Module,File,Line),
   write_term(S,tcltk_call(Call,Module,File,Line),[quoted(true)]), write(S,'.'), nl(S),fail.
export(_).
