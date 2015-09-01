:- module(clojure_exporter,[export_to_clj_file/1, export/1]).

:- use_module(infolog_tools,[git_revision/1]).

% ==========================================

% export analysis results to clojure file:

export_to_clj_file(OutputFile) :-
    open(OutputFile,write,Stream),
    export(Stream),
    flush_output(Stream),
    close(Stream).


export(Stream) :-
 export_all(Stream).

% read all characters from a stream
stream2code(S,Atom) :-
  read_line(S,Text),
  atom_codes(Atom,Text).



export_all(S) :-
 git_revision(Sha),

 format(S, '[ ~n',[]),
 format(S, '[git "~a"]~n',[Sha]),

 export_X2(S,predicate),
 export_X2(S,is_dynamic),
 export_X2(S,is_volatile),
 export_X2(S,is_exported),
 export_X2(S,is_multifile),
 export_X2(S,is_meta),
 export_X2(S,declared_mode),
 export_X2(S,is_blocking),

 export_defined_modules(S),
 export_clause(S),
 export_calling(S),
 export_operator(S),
 export_problems(S),
 export_dependencies(S),
 format(S, ']~n',[]).


export_dependencies(S) :-
   (depends_on(LM,IM),
    write_clojure(S,dependency,[LM,string,IM,string]),fail
    ; true).

export_defined_modules(S) :-
  (defined_module(M,File), write_clojure(S,module,[M,string, File,string]),fail
   ; true).

export_problems(S) :-
  findall([P], problem(P),L),
  maplist(escaping_format(S,'[problem "~w"]~n'),L).

export_clause(S) :-
   (klaus(M,P/A, Start, End),
    write_clojure(S,clause,[M,string, P,string, A, number, Start, number, End, number]), fail
    ; true).

export_operator(S) :-
  (operator(M:P/A,Prio, Fix, Assoc),
   write_clojure(S, operator,[M,string,  P,string,  A, number,
            Prio, number,  Fix, string,  Assoc, string]),fail
    ; true).

export_calling(S) :-
  (calling(M,P/A, CM,CP/CA, Start, End), 
   write_clojure(S,call,[M,string, P,string, A, number,
            CM, string, CP, string, CA, number, Start, number, End, number]), fail
    ; true).


%clojure_fact_wrap(S,X,E) :-
% format(S,'[ ~a ',[X]),
% call(E),
% format(S,']~n',[]).


export_X1(S, X) :-
  (call(X,M:P/A),write_clojure(S,X,[M,string, P,string, A, number]),fail ; true),!.

export_X2(S, X) :-
   (call(X,M:P/A,Args),
    write_clojure(S,X,[M,string, P,string, A, number,  Args, string]),fail ; true).

write_clojure(S,X,E) :-
 format(S,'[ ~a ',[X]),
 %clojure_fact_wrap(S,X,write_clojure2(S,X,E)),
 write_clojure2(E,S,X),
 format(S,']~n',[]).

write_clojure2([],_,_).
write_clojure2([Content,Type|T],S,X) :-
  write_clojure_type(Type, S,X, Content),
  write_clojure2(T,S,X).

write_clojure_type(number, S,_, Content) :- !,
  format(S, '~d ',[Content]).
%default type is string
write_clojure_type(_, S,_, Content) :-
  escaping_format(S, '"~w" ',[Content]).