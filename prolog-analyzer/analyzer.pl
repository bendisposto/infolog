:- prolog_flag(compiling,_,debugcode).
:- prolog_flag(source_info,_,on).
:- prolog_flag(profiling,_,on).

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(system)).
:- use_module(library(file_systems)).
:- use_module(library(codesio)).

:- use_module(escaper).
:- include(prob_search_paths).

:- op(300, fy, ~~).


:-  dynamic 
    defined_module/2,         % module(name,file)
    predicate/1,     % predicate(module:name/arity)
    is_dynamic/1,    % is_dynamic(module:name/arity)
    is_volatile/1,   % is_dynamic(module:name/arity)
    is_meta/2,       % is_meta(module:name/arity, meta_arguments)
    clause/4,        % clause(module:name/arity, some_clause_integer_id, startline, endline)
    calling/4,       % calling(some_clause_integer_id, module:name/arity, startline, endline)
    declared_mode/2, % declared_mode(module:name/arity, mode_arguments)
    is_exported/1,   % is_exported(module:name/arity)
    depends_on/2,    % depends_on(local_module, imported_module) ;; local_module uses imported_module
    is_multifile/1,  % is_multifile(module:name/arity)
    is_blocking/2,   % is_blocking(module:name/arity, block_arguments)
    operator/4,      % operator(module:name/arity, priority, fixity, associativity)  ;; fixity : {prefix, infix, postfix}
    problem/1,
    klaus_count/1.    % problem(details) 

export :- 
 clojure_map_wrap(export_all).

export_all :- 
 write(':predicates'),
 clojure_list_wrap(export_X1(predicate)),
 write(':dynamic'),
 clojure_list_wrap(export_X1(is_dynamic)),
 write(':volatile'),
 clojure_list_wrap(export_X1(is_volatile)),
 write(':exported'),
 clojure_list_wrap(export_X1(is_exported)),
 write(':multifile'),
 clojure_list_wrap(export_X1(is_multifile)),
 write(':meta'),
 clojure_list_wrap(export_X2(is_meta)),
 write(':mode'),
 clojure_list_wrap(export_X2(declared_mode)),
 write(':blocking'),
 clojure_list_wrap(export_X2(is_blocking)),
 write(':modules'),
 clojure_list_wrap(export_defined_modules),
 write(':clauses'),
 clojure_list_wrap(export_clause),
 write(':calling'),
 clojure_list_wrap(export_calling),
 write(':operators'),
 clojure_list_wrap(export_operator), 
 write(':problems'),
 clojure_list_wrap(export_problems)
 .

export_defined_modules :- 
  findall([':m',M,string,':file', File,string], defined_module(M,File),L),
  maplist(write_clojure,L).

export_problems :- 
  findall([P], problem(P),L),
  maplist(format('"~w"'),L).  

export_clause :- 
  findall( [':m',M,string,
            ':p', P,string, 
            ':a', A, number,
            ':id', Id, number,
            ':start', Start, number,
            ':end', End, number], clause(M:P/A,Id, Start, End),L),
  maplist(write_clojure,L).


export_operator :- 
  findall( [':m',M,string,
            ':p', P,string, 
            ':a', A, number,
            ':prio', Prio, number,
            ':fix', Fix, string,
            ':associativity', Assoc, string], operator(M:P/A,Prio, Fix, Assoc),L),
  maplist(write_clojure,L).

export_calling :- 
  findall( [':m',M,string,
            ':p', P,string, 
            ':a', A, number,
            ':id', Id, number,
            ':start', Start, number,
            ':end', End, number], calling(Id, M:P/A, Start, End),L),
  maplist(write_clojure,L).


clojure_map_wrap(X) :- 
 format(' { ',[]),
 call(X),
 format('}~n',[]). 

clojure_list_wrap(X) :- 
 format(' [',[]),
 call(X),
 format('] ',[]). 

export_X1(X) :-
  F =.. [X,M:P/A],
  findall([':m',M,string,':p', P,string, ':a', A, number], F,L),
  maplist(write_clojure,L).

export_X2(X) :-
  F =.. [X,M:P/A,Args],
  findall([':m',M,string,':p', P,string, ':a', A, number, ':args', Args, string], F,L),
  maplist(write_clojure,L).

write_clojure(E) :- 
 clojure_map_wrap(write_clojure2(E)).

write_clojure2([]).
write_clojure2([Key,Content,Type|T]) :-
  write_clojure(Key,Content,Type),write_clojure2(T). 


write_clojure(Key,Content, number) :- !,
  format('~a ~d ',[Key,Content]).

%default type is string
write_clojure(Key,Content, _) :- 
  format('~a "~w" ',[Key,Content]).

klaus_count(1).
next_klaus(Y) :- 
  retract(klaus_count(Y)),
  Y1 is Y+1,
  assert(klaus_count(Y1)).

aflatten(List,FlatList) :- flatten1(List,[],FlatList).
flatten1([],L,L) :- !.
flatten1([H|T],Tail,List) :- !, flatten1(H,FlatList,List), flatten1(T,Tail,FlatList).
flatten1(NonList,Tail,[NonList|Tail]).


bind_args(Args,VC,VCN) :-
    term_variables(Args,Variables),
    bind_args2(Variables,VC,VCN).
bind_args2([],X,X).
bind_args2([V|Vs],VC,VCN) :-
    number_codes(VC,CodesVC),
    append("v",CodesVC,Codes),
    atom_codes(V,Codes),
    VCNT is VC + 1,
    bind_args(Vs,VCNT,VCN).

layout_sub_term([],_,[]).
layout_sub_term([H|T],N,Res) :-
    (N=<1 -> Res=H ; N1 is N-1, layout_sub_term(T,N1,Res)).

get_position(Layout, StartLine, EndLine) :- 
  get_position1(Layout, Start, End),
  (Start = [] -> StartLine = -1, StartLine = Start),
  (End = [] -> EndLine = -1, EndLine = End).

get_position1(Layout, StartLine, EndLine) :- 
    aflatten(Layout,[StartLine|FlatLayout]),
    (FlatLayout = [] -> EndLine = StartLine ; last(FlatLayout,EndLine)).

     % calling(some_clause_integer_id, module:name/arity, startline, endline)
assert_call(Id, Predicate, Layout,  no_dcg) :- 
    get_position(Layout, StartLine, EndLine),
    (Predicate = Module:Call -> true; Call=Predicate, Module=module_yet_unknown),
    functor(Call, Name, Arity),
    assert(calling(Id, Module:Name/Arity, StartLine, EndLine)).

assert_call(Id, Predicate, Layout, dcg) :- 
    get_position(Layout, StartLine, EndLine),
    (Predicate = Module:Call -> true; Call=Predicate, Module=module_yet_unknown),
    functor(Call, Name, WrongArity),
    Arity is WrongArity + 2,
    assert(calling(Id, Module:Name/Arity, StartLine, EndLine)).  


analyze_body(X,Layout, Clause, DCG) :- 
    var(X), !, assert_call(Clause, built_in:call(X), Layout, DCG).

analyze_body(Module:X,Layout, Clause, DCG) :- 
    var(X), !, assert_call(Clause, built_in:call(Module:X), Layout, DCG).

% { ... } prevents DCGs from adding additional arguments
analyze_body({X},Layout,Clause,_DCG) :- !,
    analyze_body(X,Layout,Clause,no_dcg).

analyze_body(X,_,_,dcg) :- is_list(X),!.


analyze_body(\+(X),Layout, Clause, DCG) :- 
    !, 
    assert_call(Clause, built_in:not(X), Layout, DCG),
    analyze_body(X,Layout,Clause,DCG).


analyze_body((A -> B ; C),Layout, Clause, DCG) :-
    !, 
    assert_call(Clause, built_in:'->'(_,_,_), Layout, DCG), 
    layout_sub_term(Layout,2,LayoutAB),
    layout_sub_term(LayoutAB,2,LayoutA),
    layout_sub_term(LayoutAB,3,LayoutB),
    layout_sub_term(Layout,3,LayoutC),
    analyze_body(A,LayoutA, Clause, DCG),
    analyze_body(B,LayoutB, Clause, DCG),
    analyze_body(C,LayoutC, Clause, DCG).

analyze_body((A -> B),Layout, Clause, DCG) :- 
    !, 
    assert_call(Clause, built_in:'->'(_,_), Layout, DCG),
    layout_sub_term(Layout,2,LayoutA),
    layout_sub_term(Layout,3,LayoutB),
    analyze_body(A,LayoutA,Clause, DCG),
    analyze_body(B,LayoutB,Clause, DCG).


analyze_body(if(A,B,C),Layout, Clause, DCG) :- 
    !,
    assert_call(Clause, built_in:if(A,B,C), Layout, DCG),
    layout_sub_term(Layout,2,LayoutA),
    layout_sub_term(Layout,3,LayoutB),
    layout_sub_term(Layout,4,LayoutC),
    analyze_body(A,LayoutA, Clause, DCG),
    analyze_body(B,LayoutB, Clause, DCG),
    analyze_body(C,LayoutC, Clause, DCG).

analyze_body(when(A,B),Layout, Clause, DCG) :- 
 !,
  assert_call(Clause, built_in:when(A,B), Layout, DCG), 
  layout_sub_term(Layout,2,LayoutA),
  layout_sub_term(Layout,3,LayoutB),
  analyze_body(A,LayoutA, Clause, DCG),
  analyze_body(B,LayoutB, Clause, DCG).

analyze_body(assert(A),Layout, Clause, DCG) :- 
  !,
  assert_call(Clause, built_in:assert(A), Layout, DCG), 
  layout_sub_term(Layout,2,LayoutA),
  analyze_body(A,LayoutA,Clause,DCG).

analyze_body(retract(A),Layout, Clause, DCG) :- 
  !,
  assert_call(Clause, built_in:retract(A), Layout, DCG), 
  layout_sub_term(Layout,2,LayoutA),
  analyze_body(A,LayoutA,Clause,DCG).


analyze_body((A,B),Layout, Clause, DCG) :- 
 !,
  layout_sub_term(Layout,2,LayoutA),
  layout_sub_term(Layout,3,LayoutB),
  analyze_body(A,LayoutA, Clause, DCG),
  analyze_body(B,LayoutB, Clause, DCG).

analyze_body((A;B),Layout, Clause, DCG) :- 
 !,
  layout_sub_term(Layout,2,LayoutA),
  layout_sub_term(Layout,3,LayoutB),
  analyze_body(A,LayoutA, Clause, DCG),
  analyze_body(B,LayoutB, Clause, DCG).

analyze_body(Module:Call,Layout, Clause, DCG) :-
  clause(Module:N/A, Clause,_,_), 
  (functor(Call,N,A) -> 
    assert_call(Clause, Module:recursive_call, Layout, DCG) ; 
    assert_call(Clause, Module:Call, Layout, DCG)).

analyze_body(Call,Layout, Clause, DCG) :-
  clause(Module:N/A, Clause,_,_), 
  (functor(Call,N,A) -> 
    assert_call(Clause, Module:recursive_call, Layout, DCG) ; 
    assert_call(Clause, module_yet_unknown:Call, Layout, DCG)).

mk_problem(P) :- assert(problem(P)).

%% analyzing Prolog Code

add_fact(Fact, Module, Name/Arity) :- !, 
    Predicate = Module:Name/Arity,
    (predicate(Predicate) -> true; assert(predicate(Predicate))),
    X =..[Fact, Predicate], 
    assert(X).

add_fact(Fact, Module, Term ) :- 
    functor(Term,Name,Arity),
    Term =..[_Fun|Arguments],
    Predicate = Module:Name/Arity,
    (predicate(Predicate) -> true; assert(predicate(Predicate))),
    X =..[Fact, Predicate, Arguments], assert(X).


pairs_to_list((X,Y), [X|R]) :- pairs_to_list(Y,R).
pairs_to_list(X, [X]).


fixity(fy, prefix, right,1).
fixity(fx, prefix, not,1).

fixity(yf, prefix, left,1).
fixity(xf, prefix, not,1).

fixity(xfx, infix, not,2).
fixity(xfy, infix, right,2).
fixity(yfx, infix, left,2).


dependency(Module, Name) :-
  unwrap_module(Name,UnwrappedName), 
  assert(depends_on(Module,UnwrappedName)).


analyze((:- module(Name, ListOfExported)), _Layout, Module, File) :-
    !,
    (Name = Module -> true; mk_problem(wrong_filename(Module,Name,File))),
    (defined_module(Name2,File) -> mk_problem(multiple_modules_in_file(File, Name, Name2)); true),
    assert(defined_module(Name,File)),
    maplist(add_fact(is_exported, Module),ListOfExported).

analyze((:- use_module(Name, ListOfImported)), _Layout,Module, _File) :- % IMPORTS
    !, dependency(Module,Name), Implement_List_Of_Imports=1.

analyze((:- use_module(X)), _Layout, Module, _File) :- 
    (is_list(X) -> maplist(dependency(Module),X); dependency(Module,X)).

analyze((:- dynamic(X)), _Layout,Module,_File) :-
       !, 
       pairs_to_list(X,L),
       maplist(add_fact(is_dynamic, Module),L).


analyze((:- meta_predicate(X)), _Layout,Module, _File) :-
    !, 
    pairs_to_list(X,L),
    maplist(add_fact(is_meta, Module), L).

%blocking, operator declarations, volatile, multifile, 	mode

analyze((:- mode(X)), _Layout, Module, _File) :-
    !, 
    pairs_to_list(X,L),
    maplist(add_fact(declared_mode, Module), L).


analyze((:- block(X)), _Layout, Module, _File) :-
    !, 
    pairs_to_list(X,L),
    maplist(add_fact(is_blocking, Module), L).

analyze((:- op(Priority,FixityTerm,Name)), _Layout,Module, _File) :- 
 fixity(FixityTerm, Fixity, Associativity, Arity),
 assert( operator(Module:Name/Arity,Priority,Fixity,Associativity) ).

analyze((:- volatile(X)), _Layout,Module, _File) :-
       !, 
       pairs_to_list(X,L),
       maplist(add_fact(is_volatile, Module),L).


analyze((:- multifile(X)), _Layout, Module, _File) :-
       !, 
       pairs_to_list(X,L),
       maplist(add_fact(is_multifile, Module),L).
	

analyze((Head :- Body), [LayoutHead | LayoutSub], Module, _File) :-
    !,
    functor(Head,Name,Arity),
    Predicate = Module:Name/Arity, 
    assert_head(Predicate, LayoutHead,Id),
    analyze_body(Body,LayoutSub, Id, no_dcg).

analyze((Head --> Body), [LayoutHead | LayoutSub], Module, _File) :-
    !,
    functor(Head,Name,WrongArity),
    Arity is WrongArity + 2,
    Predicate = Module:Name/Arity, 
    assert_head(Predicate, LayoutHead, Id),
    analyze_body(Body,LayoutSub, Id, dcg).


analyze(Fact, Layout, Module, _File) :-
    !,
    functor(Fact,Name,Arity),
    Predicate = Module:Name/Arity, 
    assert_head(Predicate, Layout, _Id).

assert_head(Predicate, Layout, Id) :- 
    (predicate(Predicate) -> true; assert(predicate(Predicate))),
    get_position(Layout, StartLine, EndLine),
    next_klaus(Id), 
    assert(clause(Predicate, Id, StartLine, EndLine)).


get_module(Name, Arity, CallingModule, built_in) :- 
   functor(Call, Name, Arity),
   predicate_property(CallingModule:Call,built_in),!.

get_module(Name, Arity, CallingModule, Module) :- 
   functor(Call, Name, Arity),
   predicate_property(CallingModule:Call,imported_from(Module)),!.


get_module(Name, Arity, CallingModule, CallingModule) :- 
   functor(Call, Name, Arity),
   (predicate_property(CallingModule:Call,interpreted); predicate_property(CallingModule:Call,compiled)),!.

get_module(Name, Arity, CallingModule, undefined_module) :- 
  mk_problem(could_not_infer_module(Name, Arity, CallingModule)).


update :- 
  findall(c(Clause, Name, Arity, Start, End, CallingModule), 
    (calling(Clause, module_yet_unknown:Name/Arity, Start, End),
    clause(CallingModule:_/_, Clause, _, _)),L),
  maplist(update,L).

update(c(Clause,Name, Arity, Start, End, CallingModule)) :- 
    retract(calling(Clause, module_yet_unknown:Name/Arity, Start, End)),
    get_module(Name, Arity, CallingModule,Module),
    assert(calling(Clause, Module:Name/Arity, Start, End)).

:- multifile user:term_expansion/6.


user:term_expansion(Term, Layout, Tokens, Term, [], [codeq | Tokens]) :-
    prolog_load_context(module, Module),
    % print(module(Module)), nl, 
    prolog_load_context(file, File),
    nonmember(codeq, Tokens), % do not expand if already expanded
    analyze(Term, Layout, Module, File),
    !.
