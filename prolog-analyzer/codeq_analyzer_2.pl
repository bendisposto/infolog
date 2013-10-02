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


:-  dynamic called/4, 
    current_file/1, 
    file_name/1, 
    exports/4, 
    imports/4, 
    imports/2, 
    predicates/9, 
    dynamics/2, 
    metas/3, 
    volatiles/2, 
    multifiles/2, 
    in_module/2, 
    in_clause/2, 
    module_pos/3, 
    ops/4, 
    blocking/3, 
    modes/3, 
    stream/1,
    problems/1. 



emphasize_predicate(_Module,_Name,_Ar,_Code,Calls,_Body,_Start,_End,_FileName,'Predicates calling retract'):-
	member(call(_,assert,_), Calls).

emphasize_predicate(_Module,Name,Ar,_Code,_Calls,_Body,_Start,_End,FileName,'Meta Predicates'):-
	metas(FileName, Name/Ar,_).





in_module('user').
module_pos(1,1).

flatten(List,FlatList) :- flatten1(List,[],FlatList).
flatten1([],L,L) :- !.
flatten1([H|T],Tail,List) :- !, flatten1(H,FlatList,List), flatten1(T,Tail,FlatList).
flatten1(NonList,Tail,[NonList|Tail]).


write_emphasize(Module,Name,Ar,Code,Calls,StartLines,EndLines,VNC,File):-
	emphasize_predicate(Module,Name,Ar,Code,Calls,StartLines,EndLines,VNC,File,Message), 
	stream(Stream),
	write(Stream,'\t\t\t<emphasize>'),
	escaping_format(Stream, '~w', [Message]),
	write(Stream,'</emphasize>\n'),
	fail.
write_emphasize(_Module,_Name,_Ar,_Code,_Calls,_StartLines,_EndLines,_VNC,_File).
	
write_exports(File) :-
    exports(File,Module,Name,Arity),
	stream(Stream),
    escaping_format(Stream,'\n\t<export>\n\t\t<module>"~w"</module>\n\t\t<name>"~w"</name>\n\t\t<arity>~w</arity>\n\t</export>\n\n', [Module,Name,Arity]),
    fail.
write_exports(_File).

write_import1(File) :-
	stream(Stream),
	imports(File,Name),
    escaping_format(Stream,'\n\t<imported_module>\n\t\t<name>"~w"</name>\n</imported_module>\n',[Name]),
    fail.
write_import1(_File).
    
write_import3(File) :-
    imports(File,Module,Name,Arity),
	stream(Stream),
    escaping_format(Stream,'\n\t<import>\n\t\t<module>"~w"</module>\n\t\t<name>"~w"</name>\n\t\t<arity>~w</arity>\n\t</import>\n\n', [Module,Name,Arity]),
    fail.
write_import3(_File).

write_ops3(File) :-
    ops(File,Prio,Ass,Name),
	stream(Stream),
    escaping_format(Stream,'\n\t<op>\n\t\t<priority>"~w"</priority>\n\t\t<ass>"~w"</ass>\n\t\t<name>~w</name>\n\t</op>\n\n', [Prio,Ass,Name]),
    fail.
write_ops3(_File).

write_mode(File,Name/Ar):-
 	modes(File,Name/Ar, Args), 
	stream(Stream),
	escaping_format(Stream,'\n\t\t\t<mode>~w~w</mode>', [Name, Args]),
	fail.
write_mode(_File,_Name/_Ar).

write_blocking(File,Name/Ar):-
 	blocking(File,Name/Ar, Args), 
	stream(Stream),
	escaping_format(Stream,'\n\t\t\t<blocking>~w~w</blocking>', [Name, Args]),
	fail.
write_blocking(_File,_Name/_Ar).

write_multifiles(File) :-
 	multifiles(File,Name/Ar), 
	stream(Stream),
	escaping_format(Stream,'\n\t<multifile>~w/~w</multifile>', [Name, Ar]),
	fail.
write_multifiles(_File).

write_dynamics(File) :-
 	dynamics(File,Name/Ar), 
	stream(Stream),
	escaping_format(Stream,'\n\t<dynamics>~w/~w</dynamics>\n', [Name, Ar]),
	fail.
write_dynamics(_File).

write_metas(File,Name/Ar) :-
 	stream(Stream),
	metas(File,Name/Ar, Arg), 
	escaping_format(Stream,'~w~w\t', [Name, Arg]),
	fail.
write_metas(_File,_Name/_Ar).

write_predicates(_File,[]).
write_predicates(File,[pr(Name,Ar)|Names]) :-
    write_predicates2(_,Name,Ar,[],[],[],[],0,File),
    write_predicates(File,Names).

% write_predicates Module writes predicates of a given module

% write_predicates(Module) :-		
%    findall(pr(Name,Ar), predicates(Module,Name,Ar,_,_,_,_,_), ListOfNames),
%    remove_dups(ListOfNames,ListOfNames2),
%    write_predicates(ListOfNames2).

write_predicates(File) :-
    findall(pr(Name,Ar), predicates(_,Name,Ar,_,_,_,_,_,File), ListOfNames),
    remove_dups(ListOfNames,ListOfNames2),
    write_predicates(File,ListOfNames2).

write_problems(Stream) :-
    findall(X, problems(X), L),
    write_problems2(L,Stream).

write_problems2([],_).
write_problems2([H|T],Stream) :- 
    write(Stream, '\n\t<problem>'),
    write(Stream, H),
    write(Stream, '</problem>'),
    write_problems2(T,Stream).


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

is_dynamic(File,Name,Ar,'<dynamic>true</dynamic>') :- dynamics(File,Name/Ar), !.
is_dynamic(_File,_Name,_Ar,'<dynamic>false</dynamic>').

is_meta(File,Name,Ar,Return) :- metas(File,Name/Ar, _Args), format_to_codes('<meta>true</meta>', [], Codes), atom_codes(Return, Codes), !.
is_meta(_File,_Name,_Ar,'<meta>false</meta>').

is_volatile(File,Name,Ar,'\n\t\t<volatile>true</volatile>') :- volatiles(File,Name/Ar), !.
is_volatile(_File,_Name,_Ar,'\n\t\t<volatile>false</volatile>').

is_multifile(File,Name,Ar,'\n\t\t<multifile>true</multifile>') :- multifiles(File,Name/Ar), !.
is_multifile(_File,_Name,_Ar,'\n\t\t<multifile>false</multifile>').


write_predicates2(Module,Name,Ar,Code,Calls,StartLines,EndLines,VC,FileName) :-
    retract(predicates(Module,Name,Ar,Args1,Body1,Calls1,StartLine,EndLine,FileName)),
    bind_args(Args1,VC,VCN),
    bind_args(Body1,VCN,VCN2),
    NewCode = [ Args1, Body1|Code],
    append(Calls,Calls1,NewCalls),
    write_predicates2(Module,Name,Ar,NewCode,NewCalls,[StartLine|StartLines],[EndLine|EndLines],VCN2,FileName).
write_predicates2(Module,Name,Ar,Code,Calls,StartLines,EndLines,VNC,File) :-
    is_dynamic(File,Name,Ar,Dynamic), is_meta(File,Name,Ar,Meta), is_volatile(File,Name,Ar,Volatile), is_multifile(File,Name,Ar,Multifile),
    stream(Stream),
	escaping_format(Stream,'\t<predicate>\n\t\t<name>"~w"</name>\n\t\t<arity>~w</arity>\n\t\t<startlines>~w</startlines>\n\t\t<endlines>~w</endlines>\n\t\t~w\n\t\t~w ~w ~w\n\t\t<calls>',[Name,Ar,StartLines,EndLines,Dynamic,Meta, Volatile,Multifile]),
	setify(Calls, CallsNoDups),
	write_calls(CallsNoDups), write(Stream,'\n\t\t</calls>'),
	write(Stream, '\n\t\t<meta_args>'),
	write_metas(File,Name/Ar),
	write(Stream, '</meta_args>'),
	write(Stream,'\n\t\t<block>'),
	write_blocking(File,Name/Ar),
	write(Stream,'\n\t\t</block>'),
	write(Stream,'\n\t\t<modedeclaration>'),
	write_mode(File,Name/Ar),
	write(Stream,'\n\t\t</modedeclaration>'),
	write(Stream, '\n\t\t<emphasized_predicates>\n\n'),
	write_emphasize(Module,Name,Ar,Code,Calls,StartLines,EndLines,VNC,File),
   	write(Stream, '\n\t\t</emphasized_predicates>\n'),
	write(Stream,'\n\t</predicate>\n').
	    
write_calls([]).
write_calls([call(Module,Name,Ar)|Calls]) :-
	escape_single_argument(Name, EscapedName),
    stream(Stream),
	escaping_format(Stream,'\n\t\t\t<call>\n\t\t\t\t<module>"~w"</module>\n\t\t\t\t<name>~w</name>\n\t\t\t\t<arity>~w</arity>\n\t\t\t</call>', [Module,EscapedName,Ar]),
    write_calls(Calls).

write_xml_representation(File) :-
    update_calls_all_preds(File),
    stream(Stream),
	write(Stream,'\n\n<programm>\n'), nl,
	write(Stream,'<file>'),
	write(Stream, File),
	write(Stream,'</file>\n'),
    write(Stream,'<problems>'),
    write_problems(Stream),
    write(Stream,'\n</problems>\n'),
    in_module(File,Module),
    module_pos(File,StartLine,EndLine),
    escaping_format(Stream,'<module>"~w"</module>\n\n', [Module]),
    escaping_format(Stream,'<module_startline>~w</module_startline>\n', [StartLine]),
    escaping_format(Stream,'<module_endline>~w</module_endline>\n', [EndLine]),
    write(Stream,'<exports>\n'), write_exports(File), write(Stream,'</exports>\n'), nl,
	write(Stream,'\n<multifiles>\n'), write_multifiles(File), write(Stream,'\n</multifiles>\n'),
	write(Stream,'\n<dynamic_predicates>\n'), write_dynamics(File), write(Stream,'\n</dynamic_predicates>\n'),
    write(Stream,'\n<predicates>\n\n'), write_predicates(File), write(Stream,'</predicates>\n'), nl,
    write(Stream,'<import_modules>'), write_import1(File), write(Stream,'</import_modules>\n'), nl,
    write(Stream,'<import_predicates>\n'), write_import3(File), write(Stream,'</import_predicates>\n'), nl,
	write(Stream,'\n<ops>\n'), write_ops3(File), write(Stream,'</ops>\n'), nl,
    write(Stream,'</programm>\n\n').

update_calls_all_preds(File) :-
    findall(pred(Module,Name,Ar,Arguments,Body,Calls,Start,End,File),
	    predicates(Module,Name,Ar,Arguments,Body,Calls,Start,End,File),
	    ListOfAssertedPreds),
    maplist(update_calls,ListOfAssertedPreds).

update_calls(pred(Module,Name,Ar,Arguments,Body,Calls,Start,End,FileName)) :-
    maplist(update_call,Calls,UpdatedCalls),
    retract(predicates(Module,Name,Ar,Arguments,Body,Calls,Start,End,FileName)),
    assert(predicates(Module,Name,Ar,Arguments,Body,UpdatedCalls,Start,End,FileName)).

update_call(call(Module,Call,Arity),call(Module2,Call,Arity)) :-
    Module = nil
    -> update_module(Call,Arity,Module2)
    ;  Module2 = Module.


setify(L,R) :- setify(L,[],R).
setify([],A,A).
setify([H|T],A,R) :- (member(H,T) -> setify(T,A,R); setify(T,[H|A],R)).

update_module('RECURSIVE_CALL',_A,X) :- !, current_file(File),in_module(File,X).
update_module(Call,Arity,Module2) :-
    current_file(File),
	in_module(File,X),
 	functor(CallAndVar,Call,Arity),
    (predicate_property(X:CallAndVar,built_in) -> Module2 = built_in ;
     predicate_property(X:CallAndVar,imported_from(From)) -> Module2 = From ;
     predicates(_,Call,Arity,_,_,_,_,_,File) -> Module2 = X ;
     imports(File,ModuleI,Call,Arity) -> Module2 = ModuleI ;
     dynamics(File,Call/Arity) -> Module2 = 'dynamic predicate';
     otherwise -> Module2 = foo_error).

layout_sub_term([],_,[]).
layout_sub_term([H|T],N,Res) :-
    (N=<1 -> Res=H ; N1 is N-1, layout_sub_term(T,N1,Res)).

analyze_body(X,_Layout,[call('built_in', 'call', 1)]) :- var(X), !.
analyze_body(_X:M,_Layout,[call('built_in', 'Use variable Module', 1)]) :- var(M), !.

analyze_body(\+(X),Layout,[call('built_in','not',1)|Calls]) :-
    !, analyze_body(X,Layout,Calls).
%analyze_body('~~'(X),Layout) :-
%    !, analyze_body(X,Layout).
analyze_body((A -> B),Layout,[call('built_in', '->' , 2)|Calls]) :-
    !, layout_sub_term(Layout,2,LayoutA),
    layout_sub_term(Layout,3,LayoutB),
    analyze_body(A,LayoutA,CallsA),
    analyze_body(B,LayoutB,CallsB),
    append(CallsA,CallsB,Calls).
analyze_body((A -> B ; C),Layout,[call('built_in', '->', 3)|Calls]) :-
    !, layout_sub_term(Layout,2,LayoutAB),
    layout_sub_term(LayoutAB,2,LayoutA),
    layout_sub_term(LayoutAB,3,LayoutB),
    layout_sub_term(Layout,3,LayoutC),
    analyze_body(A,LayoutA,CallsA),
    analyze_body(B,LayoutB,CallsB),
    analyze_body(C,LayoutC,CallsC),
    append(CallsA,CallsB,CallsT), append(CallsT,CallsC,Calls).
analyze_body(if(A,B,C),Layout,[call('built_in', 'if', 3)|Calls]) :-
    !, layout_sub_term(Layout,2,LayoutA),
    layout_sub_term(Layout,3,LayoutB),
    layout_sub_term(Layout,4,LayoutC),
    analyze_body(A,LayoutA,CallsA),
    analyze_body(B,LayoutB,CallsB),
    analyze_body(C,LayoutC,CallsC),
    append(CallsA,CallsB,CallsT), append(CallsT,CallsC,Calls).

analyze_body(when(A,B),Layout,[call('built_in', 'when', 2)|Calls]) :-
    !, layout_sub_term(Layout,2,LayoutA),
    layout_sub_term(Layout,3,LayoutB),
    analyze_body(A,LayoutA,CallsA),
    analyze_body(B,LayoutB,CallsB),
    append(CallsA,CallsB,Calls).

analyze_body(assert(A),Layout,[call('built_in', 'assert', 1)|CallsA]) :-
    !, layout_sub_term(Layout,2,LayoutA),
    analyze_body(A,LayoutA,CallsA).

analyze_body(retract(A),Layout,[call('built_in', 'retract', 1)|CallsA]) :-
    !, layout_sub_term(Layout,2,LayoutA),
    analyze_body(A,LayoutA,CallsA).

analyze_body((A,B),Layout,Calls) :-
    !, layout_sub_term(Layout,2,LayoutA),
    layout_sub_term(Layout,3,LayoutB),
    analyze_body(A,LayoutA,CallsA),
    analyze_body(B,LayoutB,CallsB),
    append(CallsA,CallsB,Calls).
analyze_body((A;B),Layout,Calls) :-
    !, layout_sub_term(Layout,2,LayoutA),
    layout_sub_term(Layout,3,LayoutB),
    analyze_body(A,LayoutA,CallsA),
    analyze_body(B,LayoutB,CallsB),
    append(CallsA,CallsB,Calls).
analyze_body(M:X,_Layout,[call(M, FunOut, Ar)]) :-
    !, functor(X,Fun,Ar),
    (recursive_call(M,Fun,Ar) -> FunOut = 'RECURSIVE_CALL' ; FunOut = Fun).
analyze_body(X,_Layout,[call(nil, FunOut, Ar)]) :-
    !, functor(X,Fun,Ar),
    (recursive_call(_M,Fun,Ar) -> FunOut = 'RECURSIVE_CALL' ; FunOut = Fun).

recursive_call(Mod,Fun,Ar) :-
    in_module(Mod), in_clause(Fun,Ar).

assert_exports(Name,N/A) :-
    !, file_name(File),
	assert(exports(File,Name,N,A)).
assert_imports(Name,N/A) :-
    !, file_name(File),
	assert(imports(File,Name,N,A)).
assert_imports(Name) :-
    !, file_name(File),
	assert(imports(File,Name)).
assert_dynamics((X,Y)) :-
    !, file_name(File),
 	assert(dynamics(File,X)), assert_dynamics(Y).
assert_dynamics(X) :-
    !, file_name(File),
	assert(dynamics(File,X)).

assert_metas((X,Y)) :-
    !, assert_metas(X), assert_metas(Y).
assert_metas(Term) :-
    !, functor(Term,Fun,Ar),
	Term =..[_Fun|Args],
	file_name(File),
    (metas(File,Fun/Ar, Args) -> true ; assert(metas(File,Fun/Ar, Args))).


%% assert mode

assert_mode((X,Y)) :-
    !, assert_mode(X), assert_mode(Y).
assert_mode(Term) :-
    !, functor(Term,Fun,Ar),
	Term =..[_Fun|Args],
	file_name(File),
    (modes(File,Fun/Ar, Args) -> true ; assert(modes(File,Fun/Ar, Args))).

%assert block

assert_blocking((X,Y)) :-
    !, assert_blocking(X), assert_blocking(Y).
assert_blocking(Term) :-
    !, functor(Term,Fun,Ar),
	Term =..[_Fun|Args],
	file_name(File),
    (blocking(File,Fun/Ar, Args) -> true ; assert(blocking(File,Fun/Ar, Args))).

%assert volatile 

assert_volatile((X,Y)) :-
    !,
	file_name(File),
	assert(volatiles(File,X)), assert_volatile(Y).
assert_volatile(X) :-
    !, file_name(File),
	assert(volatiles(File,X)).

%assert multifile 

assert_multifile((X,Y)) :-
    !, 
 	file_name(File),	
	assert(multifiles(File,X)), assert_multifile(Y).
assert_multifile(X) :-
    !,
 	file_name(File),
	assert(multifiles(File,X)).

%% analyzing Prolog Code

analyze((:- module(Name, ListOfExported)), _Layout, (:- module(Name,ListOfExported))) :-
    !, flatten(_Layout,[StartLine|FlatLayout]),
    (FlatLayout = [] -> EndLine = StartLine ; last(FlatLayout,EndLine)),
    file_name(File),
    (in_module(_,X) -> (X=user -> true; assert(problems(multiple_modules_in_file(File, X, Name))));true),
	retract(module_pos(File,_,_)), retract(in_module(File,_)),
    assert(in_module(File,Name)), assert(module_pos(File,StartLine,EndLine)),  maplist(assert_exports(Name),ListOfExported).
analyze((:- use_module(Name, ListOfImported)), _Layout, (:- true)) :-
    !, unwrap_module(Name,UnwrappedName),
    maplist(assert_imports(UnwrappedName),ListOfImported).
analyze((:- use_module([H|T])), _Layout, (:- true)) :- 
    !, maplist(unwrap_module,[H|T],Names),
    maplist(assert_imports,Names).
analyze((:- use_module(Name)), _Layout, (:- true)) :- 
    !, unwrap_module(Name,UnwrappedName),
    file_name(File),
	assert(imports(File,UnwrappedName)).

analyze((:- dynamic(X)), _Layout, (:- dynamic(X))) :-
    !, assert_dynamics(X).
analyze((:- meta_predicate(X)), _Layout, (:- true)) :-
    !, assert_metas(X).

%blocking, operator declarations, volatile, multifile, 	mode

analyze((:- mode(X)), _Layout, (:- true)) :-
    !, assert_mode(X).
analyze((:- block(X)), _Layout, (:- true)) :-
    !, assert_blocking(X).
analyze((:- op(P,T,N)), _Layout, (:- op(P,T,N))) :- 
	file_name(File),
	assert( ops(File,P,T,N) ).
analyze((:- volatile(X)), _Layout, (:- volatile(X))) :-
    !, assert_volatile(X).
analyze((:- multifile(X)), _Layout, (:- multifile(X))) :-
    !, assert_multifile(X).
	
analyze((:- _),_Layout,(:- true)) :- !.
analyze((?- X),_Layout,(?- X)) :- !.
analyze(end_of_file,_Layout,end_of_file) :- !.



analyze((Head :- Body), [LayoutHead | LayoutSub], (Head :- Body)) :-
    !,layout_sub_term([LayoutHead|LayoutSub],3,SubLay),
    analyze_body(Body,SubLay,Calls),
    functor(Head,Fun,Ar),
    Head =.. [Fun|Args],
	(atom_codes(Fun, "-->") -> Args=[DCG_Name,_Rest], atom_concat(DCG_Name,-->,Name); Name=Fun),				% Analyze dcgs
    flatten([LayoutHead|LayoutSub],[StartLine|FlatLayout]),
    (FlatLayout = [] -> EndLine = StartLine ; last(FlatLayout,EndLine)),
    in_module(Module),
	file_name(FileName),
	assert(predicates(Module,Name,Ar,Args,Body,Calls,StartLine,EndLine,FileName)),
    assert(in_clause(Name,Ar)).


analyze(Fact, Layout, Fact) :-
    !, functor(Fact,Fun,Ar),
    Fact =.. [Fun|Args],
   	(atom_codes(Fun, "-->") -> Args=[DCG_Name,_Rest], atom_concat(DCG_Name,-->,Name); Name=Fun),				% Analyze dcgs
 	flatten(Layout,[StartLine|FlatLayout]),
    (FlatLayout = [] -> EndLine = StartLine ; last(FlatLayout,EndLine)),
	in_module(Module),
	file_name(FileName),
    assert(predicates(Module,Name,Ar,Args,'',[],StartLine,EndLine,FileName)).

analyze_file(FileName):-
    prolog_flag(redefine_warnings, _, off),
    on_exception(X,
        (   
            assert(file_name(FileName)),
            set_user_module(FileName),
            
            use_module(FileName),
            Stream = user_output,
            assert(stream(Stream)), 
            write(Stream,'<?xml version="1.0" encoding="UTF-8"?>\n'),   
            
     
            assert(current_file(File)),
     
       trace,
            write_xml_representation(FileName),

            retract(current_file(File)),
 
            retract(file_name(FileName)),
            retract(stream(Stream)),
        
            delete_user_module(FileName)
          %  ,halt(0)
        ),
    (
        print('{:error \"'),print(X),print('\"}'),nl,halt(1))
    ).

analyze_file(FileName, XMLFile):-
	prolog_flag(redefine_warnings, _, off),
	on_exception(X,
		(	
			assert(file_name(FileName)),
			set_user_module(FileName),
			
			use_module(FileName),
			open(XMLFile,write,Stream),
			assert(stream(Stream)),	
			write(Stream,'<?xml version="1.0" encoding="UTF-8"?>\n'),	
			
			assert(current_file(File)),
			write_xml_representation(FileName),
			retract(current_file(File)),
			
			close(Stream),
			retract(file_name(FileName)),
			retract(stream(Stream)),
		
			delete_user_module(FileName),
			halt(0)
		),
	(
		print('{:error \"'),print(X),print('\"}'),nl,halt(1))
	).
	


analyze_list_of_files(FileList, XMLFile):-
	prolog_flag(redefine_warnings, _, off),
	on_exception(X,
		(	
			use_all_modules(FileList),
			open(XMLFile,write,Stream),
			assert(stream(Stream)),	
			write(Stream,'<?xml version="1.0" encoding="UTF-8"?>\n'),	
			write_all_xmls(FileList),
			close(Stream),
			retract(stream(Stream)),
			halt(0)
		),
	(
		print('{:error \"'),print(X),print('\"}'),nl,halt(1))
	).


set_user_module(File):-
	assert(in_module(File,'user')),
	assert(module_pos(File,1,1)).

delete_user_module(File):-
	assert(in_module(File,_)),
	assert(module_pos(File,_,_)).

use_all_modules([]).
use_all_modules([File|Tail]):-
	assert(file_name(File)),
	set_user_module(File),
	use_module(File),
	retract(file_name(File)),
	use_all_modules(Tail).

write_all_xmls([]).
write_all_xmls([File|Tail]):-
	assert(current_file(File)),
	write_xml_representation(File),
	retract(current_file(File)),
	write_all_xmls(Tail).


user:term_expansion(Term1, Lay1, Tokens1, Term2, [], [codeq | Tokens1]) :-
    nonmember(codeq, Tokens1), % do not expand if already expanded
    analyze(Term1, Lay1, Term2),
    %write(S,Term1),nl,
    %write(S,Term2),nl,
    !.