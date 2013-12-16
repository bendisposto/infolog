:- prolog_flag(compiling,_,debugcode).
:- prolog_flag(source_info,_,on).
:- prolog_flag(profiling,_,on).

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(system)).
:- use_module(library(file_systems)).
:- use_module(library(codesio)).
:- use_module(library(process)).

:- use_module(escaper).


x_unwrap_module(library(X),Y) :- !, X=Y.
x_unwrap_module(probsrc(X),Y) :- !, X=Y.
x_unwrap_module(probcspsrc(X),Y) :- !, X=Y.
x_unwrap_module(bparser(X),Y) :- !, X=Y.
x_unwrap_module(plugins(X),Y) :- !, X=Y.
x_unwrap_module(abstract_domains(X),Y) :- !, X=Y.
x_unwrap_module(tclsrc(X),Y) :- !, X=Y.
x_unwrap_module(extension(E),Y) :- !,
    atom_chars(E,ExtensionPath),
    suffix(ExtensionPath,Module),
    atom_chars(Y,Module).
x_unwrap_module(Path,X) :-
    atom_chars(Path,PathChars),
    ( append(Base,[.,p,l],PathChars),
      suffix(Base,XChars)  % module loaded with .pl ending
    ; suffix(PathChars,XChars)), % or without
    x_remove_path(XChars,CharsWithoutPath),
    atom_chars(X,CharsWithoutPath).
x_unwrap_module(X,X) :- !. % might even be unwrapped

x_remove_path(L,L2) :-
    reverse(L,LR),
    nth0(N,LR,'/',_), %key code of /
    sublist(LR, LR2, 0, N, _),
    reverse(LR2,L2).

% :- op(300, fy, ~~).


:-  dynamic 
    defined_module/2,         % module(name,file)
    predicate/1,     % predicate(module:name/arity)
    is_dynamic/1,    % is_dynamic(module:name/arity)
    is_volatile/1,   % is_dynamic(module:name/arity)
    is_meta/2,       % is_meta(module:name/arity, meta_arguments)
    klaus/3,        % klaus(module:name/arity,  startline, endline)
    calling/4,       % calling(callingmodule:callingpredicate/callingarity, module:name/arity, startline, endline)
    declared_mode/2, % declared_mode(module:name/arity, mode_arguments)
    is_exported/1,   % is_exported(module:name/arity)
    depends_on/2,    % depends_on(local_module, imported_module) ;; local_module uses imported_module
    is_multifile/1,  % is_multifile(module:name/arity)
    is_blocking/2,   % is_blocking(module:name/arity, block_arguments)
    operator/4,      % operator(module:name/arity, priority, fixity, associativity)  ;; fixity : {prefix, infix, postfix}
    problem/1.       % problem(details) 

export(Stream) :- 
 export_all(Stream).

% read all characters from a stream
stream2code(S,Atom) :-
  read_line(S,Text),
  atom_codes(Atom,Text).

git_revision(Sha) :- 
   absolute_file_name('$SHELL', Shell),
   process_create(Shell, ['-c','cd $PROB_HOME && git rev-parse HEAD && cd - >/dev/null'],[stdout(pipe(F)), process(P)]), 
   process_wait(P,_ExitCode),
   stream2code(F,Sha).


export_all(S) :- 
 git_revision(Sha),

 format(S, '[ ~n',[]),
 format(S, '[git "~a"]~n',[Sha]),
 
 export_X1(S,predicate),
 export_X1(S,is_dynamic),
 export_X1(S,is_volatile),
 export_X1(S,is_exported),
 export_X1(S,is_multifile),
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
   findall([LM,string,IM,string], depends_on(LM,IM),L),
   maplist(write_clojure(S,dependency),L).

export_defined_modules(S) :- 
  findall([M,string, File,string], defined_module(M,File),L),
  maplist(write_clojure(S,module),L).

export_problems(S) :- 
  findall([P], problem(P),L),
  maplist(escaping_format(S,'[problem "~w"]~n'),L).  

export_clause(S) :- 
  findall( [M,string,
            P,string, 
            A, number,
            Start, number,
            End, number], klaus(M:P/A, Start, End),L),
  maplist(write_clojure(S,clause),L).

export_operator(S) :- 

  findall( [M,string,
            P,string, 
            A, number,
            Prio, number,
            Fix, string,
            Assoc, string], operator(M:P/A,Prio, Fix, Assoc),L),
  maplist(write_clojure(S, operator),L).

export_calling(S) :- 
  findall( [M,string,
            P,string, 
            A, number,
            CM, string,
            CP, string,
            CA, number,              
            Start, number,
            End, number], calling(M:P/A, CM:CP/CA, Start, End),L),
  maplist(write_clojure(S,call),L).

clojure_fact_wrap(S,X,E) :- 
 format(S,'[ ~a ',[X]),
 call(E),
 format(S,']~n',[]). 


export_X1(S, X) :-
  F =.. [X,M:P/A],
  findall([M,string, P,string, A, number], F,L),
  maplist(write_clojure(S,X),L).

export_X2(S, X) :-
  F =.. [X,M:P/A,Args],
  findall([M,string, P,string, A, number,  Args, string], F,L),
  maplist(write_clojure(S,X),L).

write_clojure(S,X,E) :- 
 clojure_fact_wrap(S,X,write_clojure2(S,X,E)).

write_clojure2(_,_,[]).
write_clojure2(S,X,[Content,Type|T]) :-
  write_clojure(S,X, Content,Type),write_clojure2(S,X, T). 


write_clojure(S,_, Content, number) :- !,
  format(S, '~d ',[Content]).

%default type is string
write_clojure(S,_, Content, _) :- 
  escaping_format(S, '"~w" ',[Content]).


aflatten(List,FlatList) :- flatten1(List,[],FlatList).
flatten1([],L,L) :- !.
flatten1([H|T],Tail,List) :- !, flatten1(H,FlatList,List), flatten1(T,Tail,FlatList).
flatten1(NonList,Tail,[NonList|Tail]).

dcg_specialcase('=',2).
dcg_specialcase('!',0).
dcg_specialcase(':',2).

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
  (Start = [] -> StartLine = -1; StartLine = Start),
  (End = [] -> EndLine = -1; EndLine = End).

get_position1(Layout, StartLine, EndLine) :- 
    aflatten(Layout,[StartLine|FlatLayout]),
    (FlatLayout = [] -> EndLine = StartLine ; last(FlatLayout,EndLine)).

     % calling(cmodule:cname/carity, module:name/arity, startline, endline)
assert_call(CallingPredicate, Predicate, Layout,  no_dcg) :- 
    get_position(Layout, StartLine, EndLine),
    (Predicate = Module:Call -> true; Call=Predicate, Module=module_yet_unknown),
    functor(Call, Name, Arity),
    assert(calling(CallingPredicate, Module:Name/Arity, StartLine, EndLine)).

assert_call(CallingPredicate, Predicate, Layout, dcg) :- 
    get_position(Layout, StartLine, EndLine),
    (Predicate = Module:Call -> true; Call=Predicate, Module=module_yet_unknown),
    functor(Call, Name, WrongArity),
    Arity is WrongArity + 2,
    assert(calling(CallingPredicate, Module:Name/Arity, StartLine, EndLine)).  


analyze_body(':'(_,_,_,FIX_THIS_CLAUSE),Layout, CallingPredicate, dcg).

analyze_body(X,Layout, CallingPredicate, DCG) :- 
    var(X), !, assert_call(CallingPredicate, built_in:call(X), Layout, DCG).

analyze_body(Module:X,Layout, CallingPredicate, DCG) :- 
    var(X), !, assert_call(CallingPredicate, built_in:call(Module:X), Layout, DCG).

analyze_body(X,Layout,CallingPredicate,dcg) :- 
    functor(X,F,A),
   % print(sc(X,F,A)),
    dcg_specialcase(F,A), !,
    assert_call(CallingPredicate, built_in:F/A, Layout, no_dcg).

% { ... } prevents DCGs from adding additional arguments
analyze_body({X},Layout,CallingPredicate,_DCG) :- !,
    analyze_body(X,Layout,CallingPredicate,no_dcg).

%analyze_body(~~X,Layout,CallingPredicate,DCG) :- !,
%    analyze_body(X,Layout,CallingPredicate,DCG).    

analyze_body(get_atts(_,_),Layout,CallingPredicate,DCG) :- !,
    CallingPredicate = M:_/_,
    assert_call(CallingPredicate, M:get_atts/2, Layout, DCG).   

analyze_body(put_atts(_,_),Layout,CallingPredicate,DCG) :- !,
    CallingPredicate = M:_/_,
    assert_call(CallingPredicate, M:put_atts/2, Layout, DCG).   

analyze_body(X,_,_,dcg) :- is_list(X),!.


analyze_body(\+(X),Layout, CallingPredicate, DCG) :- 
    !, 
    assert_call(CallingPredicate, built_in:not(X), Layout, DCG),
    analyze_body(X,Layout,CallingPredicate,DCG).


analyze_body((A -> B ; C),Layout, CallingPredicate, DCG) :-
    !, 
    assert_call(CallingPredicate, built_in:'->'(_,_,_), Layout, DCG), 
    layout_sub_term(Layout,2,LayoutAB),
    layout_sub_term(LayoutAB,2,LayoutA),
    layout_sub_term(LayoutAB,3,LayoutB),
    layout_sub_term(Layout,3,LayoutC),
    analyze_body(A,LayoutA, CallingPredicate, DCG),
    analyze_body(B,LayoutB, CallingPredicate, DCG),
    analyze_body(C,LayoutC, CallingPredicate, DCG).

analyze_body((A -> B),Layout, CallingPredicate, DCG) :- 
    !, 
    assert_call(CallingPredicate, built_in:'->'(_,_), Layout, DCG),
    layout_sub_term(Layout,2,LayoutA),
    layout_sub_term(Layout,3,LayoutB),
    analyze_body(A,LayoutA,CallingPredicate, DCG),
    analyze_body(B,LayoutB,CallingPredicate, DCG).


analyze_body(if(A,B,C),Layout, CallingPredicate, DCG) :- 
    !,
    assert_call(CallingPredicate, built_in:if(A,B,C), Layout, DCG),
    layout_sub_term(Layout,2,LayoutA),
    layout_sub_term(Layout,3,LayoutB),
    layout_sub_term(Layout,4,LayoutC),
    analyze_body(A,LayoutA, CallingPredicate, DCG),
    analyze_body(B,LayoutB, CallingPredicate, DCG),
    analyze_body(C,LayoutC, CallingPredicate, DCG).

analyze_body(when(A,B),Layout, CallingPredicate, DCG) :- 
 !,
  assert_call(CallingPredicate, built_in:when(A,B), Layout, DCG), 
  layout_sub_term(Layout,2,LayoutA),
  layout_sub_term(Layout,3,LayoutB),
  analyze_body(A,LayoutA, CallingPredicate, DCG),
  analyze_body(B,LayoutB, CallingPredicate, DCG).

analyze_body(assert(A),Layout, CallingPredicate, DCG) :- 
  !,
  assert_call(CallingPredicate, built_in:assert(A), Layout, DCG), 
  layout_sub_term(Layout,2,LayoutA),
  analyze_body(A,LayoutA,CallingPredicate,DCG).

analyze_body(retract(A),Layout, CallingPredicate, DCG) :- 
  !,
  assert_call(CallingPredicate, built_in:retract(A), Layout, DCG), 
  layout_sub_term(Layout,2,LayoutA),
  analyze_body(A,LayoutA,CallingPredicate,DCG).


analyze_body((A,B),Layout, CallingPredicate, DCG) :- 
 !,
  layout_sub_term(Layout,2,LayoutA),
  layout_sub_term(Layout,3,LayoutB),
  analyze_body(A,LayoutA, CallingPredicate, DCG),
  analyze_body(B,LayoutB, CallingPredicate, DCG).

analyze_body((A;B),Layout, CallingPredicate, DCG) :- 
 !,
  layout_sub_term(Layout,2,LayoutA),
  layout_sub_term(Layout,3,LayoutB),
  analyze_body(A,LayoutA, CallingPredicate, DCG),
  analyze_body(B,LayoutB, CallingPredicate, DCG).



analyze_body(Module:Call,Layout, CallingPredicate, DCG) :-
  CallingPredicate = Module:N/A, 
  (functor(Call,N,A) -> 
    assert_call(CallingPredicate, Module:recursive_call, Layout, DCG) ; 
    assert_call(CallingPredicate, Module:Call, Layout, DCG)).

analyze_body(Call,Layout, CallingPredicate, DCG) :-
  CallingPredicate = Module:N/A,  
  (functor(Call,N,A) -> 
    assert_call(CallingPredicate, Module:recursive_call, Layout, DCG) ; 
    assert_call(CallingPredicate, module_yet_unknown:Call, Layout, DCG)).

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
  x_unwrap_module(Name,UnwrappedName), 
  (defined_module(UnwrappedName,_) -> true; 
    ( atom_concat('sicstus/', UnwrappedName, _T),
      atom_concat(_T,'.pl',File),
      assert(defined_module(UnwrappedName,File)))),
  assert(depends_on(Module,UnwrappedName)).


analyze((:- module(Name, ListOfExported)), _Layout, Module, File) :-
    !,
    x_unwrap_module(File,UnwrappedName),
    (Name = UnwrappedName -> true; mk_problem(wrong_filename(Module,Name,File))),
    (defined_module(Name2,File) -> mk_problem(multiple_modules_in_file(File, Name, Name2)); true),
    retractall(defined_module(Name,_)),
    assert(defined_module(Name,File)),
    maplist(add_fact(is_exported, Name),ListOfExported).

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
 (predicate(Module:Name/Arity) -> true; assert(predicate(Module:Name/Arity))),
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
    assert_head(Predicate, LayoutHead),
    analyze_body(Body,LayoutSub, Predicate, no_dcg).

analyze((Head --> Body), [LayoutHead | LayoutSub], Module, _File) :-
    !,
    functor(Head,Name,WrongArity),
    Arity is WrongArity + 2,
    Predicate = Module:Name/Arity, 
    assert_head(Predicate, LayoutHead),
    analyze_body(Body,LayoutSub, Predicate, dcg).


analyze(foreign(Name, PredSpec), Layout, Module, _File, (:- dynamic(Name/Arity))) :-
    !,
    functor(PredSpec,_,Arity),
    Predicate = Module:Name/Arity, 
    assert_head(Predicate, Layout).


analyze(foreign(Name, _Lang, PredSpec), Layout, Module, _File, TermOut) :-
    analyze(foreign(Name, PredSpec), Layout, Module, _File, TermOut).



analyze(Fact, Layout, Module, _File) :-
    !,
    functor(Fact,Name,Arity),
    Predicate = Module:Name/Arity, 
    assert_head(Predicate, Layout).

assert_head(Predicate, Layout) :- 
    (predicate(Predicate) -> true; assert(predicate(Predicate))),
    get_position(Layout, StartLine, EndLine),
    assert(klaus(Predicate,  StartLine, EndLine)).


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
  findall(c(CallingPredicate, Name, Arity, Start, End), 
    calling(CallingPredicate, module_yet_unknown:Name/Arity, Start, End),L),
  maplist(update,L).

update(c(CallingPredicate,Name, Arity, Start, End)) :-
    retract(calling(CallingPredicate, module_yet_unknown:Name/Arity, Start, End)),
    CallingPredicate = CallingModule:_/_,
    get_module(Name, Arity, CallingModule, Module),
    (predicate(Module:Name/Arity) -> true; assert(predicate(Module:Name/Arity))),
    assert(calling(CallingPredicate, Module:Name/Arity, Start, End)).

:- multifile user:term_expansion/6.

analyze(InputFile,OutputFile) :- 
  open(OutputFile,write,Stream),
  print('loading modules'),nl,
  use_module(InputFile),
  nl, print('updating calls'), nl,
  update,
  nl,
  export(Stream),
  flush_output(Stream),
  close(Stream).

user:term_expansion(Term, Layout, Tokens, TermOut, [], [codeq | Tokens]) :-
    prolog_load_context(module, Module),
    prolog_load_context(file, File),
    member(rm_debug_calls,Tokens),nonmember(codeq, Tokens), % do not expand if already expanded
  % print(expand(Module,Term)),nl,
    (analyze(Term, Layout, Module, File, TermOut) ; (analyze(Term, Layout, Module, File), TermOut = Term)),
    !.