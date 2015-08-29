
% a first Quick & Dirty version of a simple InfolLog Error/Warning Browser
% recycling the ProB Tree Inspector

:- use_module(library(tcltk)).


:- use_module(library(lists)).


% The module provides the descriptions and access to various trees displayed by tree_inspector.tcl
% We have various TreeTypes : infolog_problems, ....
% Each Tree has a certain number of columns for the nodes described by ttii_column_name/3
% Each Tree node can be inspected using ttii_get_node_info
% The Paths in the tree (ttii_Path) record the number of each child selected (starting at 0)

ttii_number_of_columns(empty,Nr) :- !, Nr=0.
ttii_number_of_columns(Type,Nr) :-
   findall(C,(ttii_column_name(Type,C,_), number(C)),L),
   length(L,Nr), print(nr_cols(Type,Nr)),nl.

% column name and width descriptions: 
ttii_column_name(TreeType,Column,Name) :-
   ttii_column_info(TreeType,Column,Name,_MinWidth,_Width,_Anchor).

ttii_column_info(infolog_problems,header,'Message',100,400,w).
ttii_column_info(infolog_problems,0,'Category',20,80,center).
ttii_column_info(infolog_problems,1,'Location',20,90,w).
ttii_column_info(infolog_problems,2,'Hash',20,80,center).

ttii_column_info(_,_,'??',20,40,center).

% possible Tags are defined in file tree_inspector.tcl: error, inac, ptrue, pfalse, subst
% ttii_get_node_info(Type,Pindex,Text,Columns,Subs,Tags) :-
ttii_get_node_info(Type,Pindex,Text,Columns,Subs,Tags) :- print(get(Type,Pindex,Text,Columns,Subs,Tags)),nl,fail.
ttii_get_node_info(empty,_,'-',list([]),0,list([])) :- !.
ttii_get_node_info(infolog_problems,Pindex,Text,list(Columns),NrOfSubs,list(Tags)) :- !,
   infolog_node(Pindex,Text,Columns,NrOfSubs,Tags),
   print(result(Text,Columns,NrOfSubs,Tags)),nl.

infolog_node([],'',[0,0,0],3,[]). % top-level
infolog_node([Ttii_Cat],Text,Columns,NrSubs,Tags) :- !,
    infolog_category(Ttii_Cat,Text,Columns,NrSubs,Tags).
infolog_node([Ttii_Cat|Path],Text,Columns,NrSubs,Tags) :- 
   infolog_entry(Ttii_Cat,Path,Text,Columns,NrSubs,Tags).

infolog_category(0,'Errors',[Nr,-,-],Nr,[pfalse]) :- nr_errors(Nr).
infolog_category(1,'Warnings',[0,-,-],0,[]).
infolog_category(2,'Infos',[0,-,-],0,[ptrue]).
infolog_category(3,'Other????',[0,0,0],0,[pfalse]).

:- dynamic errors/4, nr_errors/1.
compute_errors :- retractall(errors(_,_,_,_)),retractall(nr_errors(_)),
    findall(err(Cat,P,Loc),infolog_problem(Cat,error,P,Loc),Errors),
    length(Errors,Nr), print(nr_errors(Nr)),nl,
    assert(nr_errors(Nr)),
    nth0(Index,Errors,err(Cat,Info,Loc)),
    print(err(Index,Cat,Info,Loc)),nl,
    information_to_atom(Info,Str),
    information_to_atom(string(Cat),CatS),
    location_to_atom(Loc,LocStr),
    assert(errors(Index,Str,CatS,LocStr)),fail.
compute_errors :- print(done),nl.
init_ttii :- compute_errors.

infolog_entry(0,[Nr],Msg,[Cat,Loc,33],0,[pfalse]) :- errors(Nr,Msg,Cat,Loc).





goa :- analyze('/Users/leuschel/git_root/prob_prolog/src/prob_cli.pl'), infolog_ui.
infolog_ui :-
    init_ttii,
    tk_new([top_level_events,name('InfoLog')], X),
    tcl_eval(X, 'source infolog_tk_gui.tcl', _),
    tk_main_loop,
    tcl_delete(X).



:- include(analyzer).

:- print('Start GUI with infolog_ui.'),nl.
