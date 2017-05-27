% (c) 2009-2015 Lehrstuhl fuer Softwaretechnik und Programmiersprachen, 
% Heinrich Heine Universitaet Duesseldorf
% This software is licenced under EPL 1.0 (http://www.eclipse.org/org/documents/epl-v10.html) 


:- module(infolog_dot_graph_generator,[il_gen_dot_graph/6,il_gen_dot_graph/7]).


 :- use_module(library(lists)).


% note: one can provide none for some of the predicates

il_gen_dot_graph(F,Module,NodePredicate,TransPredicate,SameRankPred,SubGraphPred) :-
   il_gen_dot_graph(F,[with_page_size],Module,NodePredicate,TransPredicate,SameRankPred,SubGraphPred).

il_gen_dot_graph(F,WithSize,Module,NodePredicate,TransPredicate,SameRankPred,SubGraphPred) :-
   print('% Generating Dot File: '), print(F),nl,
   reset_ids,
   open(F,write,FStream),
   (fgen_dot_graph(FStream,WithSize,Module,NodePredicate,TransPredicate,SameRankPred,SubGraphPred) -> true ; true),
   close(FStream),
   print('% Done'),nl.

% -------------------------------

node_id(Module,P,NodeID) :-
   node_predicate_call(Module,P,NodeID,_Sub,_NodeDesc,_Shape,_Style,_Color).
node_predicate_call(Module,P,TNodeID,SubGraph,NodeDesc,Shape,Style,Color) :-
   %Call =.. [P,NodeID,SubGraph,NodeDesc,Shape,Style,Color],
   call(Module:P,NodeID,SubGraph,NodeDesc,Shape,Style,Color),
   translate_id(NodeID,TNodeID).
   
trans_predicate_call(Module,P,TNodeID,Label,TSuccNodeID,Color,Style,PenWidth) :-
   %Call =.. [P,NodeID,Label,SuccNodeID,Color],
   call(Module:P,NodeID,Label,SuccNodeID,Color,Style,PenWidth),
   translate_id(NodeID,TNodeID),
   translate_id(SuccNodeID,TSuccNodeID).
   
same_rank_call(Module,P,TNodes) :- P \= none,
   %Call =.. [P,Nodes],
   call(Module:P,Nodes),
   Nodes \= [], % empty list provides no information
   maplist(translate_id,Nodes,TNodes).
   /* should succeed once for every set of NodeIDs which should be of same rank */

% should succeed once for every subgraph and generate a subgraphID which is passed to the node predicate
subgraph_call(Module,P,SubGraphID,Style,Color) :- P \= none,
   %Call =.. [P,SubGraphID,Style,Color],
   call(Module:P,SubGraphID,Style,Color).
   /* Notes: SubGraphID: should be none if not in a subgraph; Style and Color can be none */
   
:- dynamic stored_id/2.
:- dynamic next_id/1.
next_id(0).

reset_ids :- retractall(stored_id(_,_)), retractall(next_id(_)),
    assert(next_id(0)).

% translate ids to numbers; ensure that dot can deal with them
translate_id(ID,TransID) :-
    (number(ID) -> TransID=ID
      ; stored_id(ID,SID) -> TransID=SID
      ;  retract(next_id(TransID)),
         assert(stored_id(ID,TransID)),
         N1 is TransID+1,
         assert(next_id(N1))).

/* ---------------------------------------------------------------------- */   


fgen_dot_graph(FStream,WithSize,Module,NodePredicate,TransPredicate,SameRankPred,SubGraphPred) :-
    print_graph_header(FStream,prob_graph,WithSize),
    (node_id(Module,NodePredicate,_) -> true
       ; (format(user_error,"No nodes in gen_dot_graph: ~w, ~w, ~w ~w ~w.~n~n",
           [Module,NodePredicate,TransPredicate,SameRankPred,SubGraphPred]),fail)),
	print_nodes(FStream,Module,NodePredicate,SubGraphPred),
	fail.   
fgen_dot_graph(FStream,_WithSize,Module,_NodePredicate,_TransPredicate,SameRankPred,_SubGraphPred) :-
    same_rank_call(Module,SameRankPred,Nodes),
    print_same_ranks(FStream,Nodes),
    fail.
fgen_dot_graph(FStream,_WithSize,Module,_NodePredicate,TransPredicate,_SameRankPred,_SubGraphPred) :-
	print_transitions(FStream,_NodeID,Module,TransPredicate),
	fail.
fgen_dot_graph(FStream,_,_,_,_,_,_) :-
    print_graph_footer(FStream).

/* ---------------------------------------------------------------------- */   

print_graph_header(Type) :- print_graph_header(user_output,Type,[with_page_size]).
print_graph_header(FStream,Type,Opts) :-
    (member(with_page_size,Opts) -> PSize='page="8.5, 11",ratio=fill,size="7.5,10"' ; PSize=''),
    (member(horizontal,Opts) -> RDir = ' rankdir=LR;' ; RDir=''), 
    format(FStream,'digraph ~w { graph [~w];~w~n',[Type,PSize,RDir]).
   % print('graph [orientation=landscape, page="8.5, 11",ratio=fill,size="7.5,10"];'),nl,

print_graph_footer :- print_graph_footer(user_output).
print_graph_footer(FStream) :-  format(FStream,'}~n',[]).   
    
    
/* ---------------------------------------------------------------------- */   


print_nodes(FStream,Module,NodePredicate,SubGraphPred) :-
   subgraph_call(Module,SubGraphPred,SubgraphID,Style,Color),
   format(FStream,'  subgraph cluster_~w {~n',[SubgraphID]),
   (Style = none -> true ; format(FStream,'      style=~w;~n',[Style])),
   (Color = none -> true ; format(FStream,'      color=~w;~n',[Color])),
   format(FStream,'      label=~w;~n',[SubgraphID]),
   print_nodes2(FStream,SubgraphID,Module,NodePredicate),
   write(FStream,'  }'),nl(FStream),
   fail.
print_nodes(FStream,Module,NodePredicate,_SubGraphPred) :-
   print_nodes2(FStream,none,Module,NodePredicate),
   nl(FStream).
   
   
print_nodes2(FStream,SubGraph,Module,NodePredicate) :-
   node_predicate_call(Module,NodePredicate,NodeID,SubGraph,NodeDesc,Shape,Style,Color),
   format(FStream,' ~w [shape=~w',[NodeID,Shape]),  /* options: triangle, .... */
   (Style = none
    -> true
    ;  format(FStream,', style=~w',[Style])  /* options: filled */
   ),
   (Color = none
    -> true
    ;  format(FStream,', color="~w"',[Color])  /* options: red,green,... */
   ),
   FSize=12, %get_preference(dot_node_font_size,FSize),
   format(FStream,', fontsize=~w, label="',[FSize]), 
   ((true, %preference(dot_print_node_ids,true),
     \+ stored_id(_,_),  % no translations performed
     NodeID \= NodeDesc) % otherwise we already print it as description
     -> format(FStream,'~w:\\n',[NodeID]) ; true),
    format(FStream,'~w"];~n',[NodeDesc]),
    fail.
print_nodes2(FStream,_Subgraph,_,_) :- nl(FStream).

print_transitions(FStream,NodeID,Module,TransPredicate) :-
	trans_predicate_call(Module,TransPredicate,NodeID,Label,SuccID,Color,Style,PenWidth),
	
    %(NodeID=root -> preference(dot_print_root,true) ; true),
    (true %preference(dot_print_leaves,true)
      -> true
      ; % check that we have at least one successor
        (trans_predicate_call(Module,TransPredicate,SuccID,_,_,_,_,_)->true;fail)),

	%(NodeID \= SuccID -> true ; preference(dot_print_self_loops,true)),
	
	format(FStream,' ~w ->  ~w [',[NodeID,SuccID]),
	(true %preference(dot_print_arc_colors,true) 
	  -> format(FStream,'color="~w",',[Color])
	  ;  true
	),
	(PenWidth \= 1
	  -> format(FStream,'penwidth="~w",',[PenWidth])
	  ;  true
	),
	% acceptable styles Style ::= solid, bold, dotted, dashed, invis,  arrowhead(none,Style)
	print_style(Style,FStream),
	FSize=12, %get_preference(dot_edge_font_size,FSize),
	(Label='' -> format(FStream,'];~n',[])
	  ; format(FStream,' label="~w", fontsize=~w];~n',[Label,FSize])),
	fail.
print_transitions(FStream,_NodeID,_,_) :- nl(FStream).

print_style(solid,_) :- !.
print_style(arrowhead(AS,S),FStream) :- !, format(FStream,'arrowhead=~w,',[AS]), print_style(S,FStream).
print_style(arrowtail(AS,S),FStream) :- !, format(FStream,'dir=both,arrowtail=~w,',[AS]), print_style(S,FStream).
print_style(Style,FStream) :- format(FStream,'style=~w,',[Style]).

print_same_ranks(FStream,L) :-
    write(FStream,' { rank=same; '),
    print_same_ranks2(FStream,L),
    write(FStream,' }'),nl(FStream).

print_same_ranks2(_FStream,[]).
print_same_ranks2(FStream,[H|T]) :- 
   write(FStream,H), write(FStream,'; '),
   print_same_ranks2(FStream,T).