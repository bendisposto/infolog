% set ProB namespaces

% some pathes may be set by environment variables during compilation,
:- dynamic compile_time_env_path/2.
lookup_env_path(Pathname, Varname) :-
    ( environ(Varname,Value) ->
        print(user_error,'Hard-wired path for alias '),print(user_error,Pathname),print(user_error,': '),print(user_error,Value),nl,
        assert(compile_time_env_path(Pathname,Value))
    ; otherwise ->
        true).
% compile-time pathes
:- lookup_env_path(prob_comp_home,'PROB_COMP_HOME'). % hard-wired version of the runtime_application_path
                                                     % This is used on systems where it absolutely clear where the
                                                     % ProB's application directory will be, namely Debian/Ubuntu packages
% run-time pathes
:- lookup_env_path(prob_home,'PROB_HOME').
:- lookup_env_path(examples,'PROB_EXAMPLES').        % hard-wired pathes, see above (about Debian/Ubuntu packages)
:- lookup_env_path(prob_lib,'PROB_LIB').             % hard-wired pathes, see above (about Debian/Ubuntu packages)
:- lookup_env_path(prob_tcl,'PROB_TCL').

% removes an (optional) trailing /src directory from the path,
% the result will not end with a slash
remove_src_dir(Orig,Dir) :-
    name(Orig,COrig),
    ( append(CDir,"/src/",COrig) -> true
    ; append(CDir,"/src",COrig)  -> true
    ; append(CDir,"/src/proz",COrig) -> true
    ; append(CDir,"/src/proz/",COrig) -> true
    ; append(CDir,"/",COrig)     -> true
    ; otherwise -> COrig=CDir),
    name(Dir,CDir).



% returns the path to the application directory (at run-time)
runtime_application_path(Dir) :-
   ( environ('PROB_HOME',Dir) -> % the user has set the environment variable PROB_HOME and thus overrides any other setting
       true                      % the Rodin plugin used this mechanism 
   ; application_path2(Dir),is_correct_prob_home_path(Dir) -> 
       true
   ; application_path2(D),absolute_file_name(D,D1),get_parent_directory(D1,Dir),
       is_correct_prob_home_path(Dir) ->
       true
   ; compile_time_env_path(prob_comp_home,Dir) ->  % e.g. Debian/Ubuntu systems: the path was hard-wired at compile-time
       true
   ; otherwise ->  % usually a run from source code, where we the current directory is prob/src
       current_directory(Current), remove_src_dir(Current,Dir)).
application_path2(Dir) :-
   \+(prolog_flag(system_type,development)),
   environ('SP_APP_DIR',Dir).  /* /usr/local/bin/sicstus on development systems */

check_if_hard_wired(Alias, _Prefix, _Dir, Full) :-
    compile_time_env_path(Alias,Full),!.
check_if_hard_wired(_Alias, Prefix, Dir, Full) :-
    atom_concat(Prefix,Dir,Full).

set_search_path(Alias, Prefix, Dir) :-
    check_if_hard_wired(Alias, Prefix, Dir, Full),
    %%  print(setting_path(Alias,Full)),nl, %%
    ( catch( user:file_search_path(Alias,Full), _, fail) -> true
    ; otherwise ->
        assertz(user:file_search_path(Alias,Full))).
   
compiletime_application_path(Dir) :-
    compile_time_env_path(prob_comp_home,Dir),!.
compiletime_application_path(Dir) :-
    runtime_application_path(Dir).
set_compile_time_search_pathes :-
    compiletime_application_path(App),
    set_search_path(extension, App, '/extensions'),
    set_search_path(probsrc, App, '/src'),
    set_search_path(probcspsrc, App, '/src/cia'),
    set_search_path(bparser, App, '/src/bparser'),
    set_search_path(plugins, App, '/plugins'),
    set_search_path(abstract_domains, App, '/plugins/absint/abstract_domains'),
    set_search_path(tclsrc, App, '/tcl').
:- set_compile_time_search_pathes.

unwrap_module(library(X),Y) :- !, X=Y.
unwrap_module(probsrc(X),Y) :- !, X=Y.
unwrap_module(probcspsrc(X),Y) :- !, X=Y.
unwrap_module(bparser(X),Y) :- !, X=Y.
unwrap_module(plugins(X),Y) :- !, X=Y.
unwrap_module(abstract_domains(X),Y) :- !, X=Y.
unwrap_module(tclsrc(X),Y) :- !, X=Y.
unwrap_module(extension(E),Y) :- !,
    atom_chars(E,ExtensionPath),
    suffix(ExtensionPath,Module),
    atom_chars(Y,Module).
unwrap_module(Path,X) :-
    atom_chars(Path,PathChars),
    ( append(Base,[.,p,l],PathChars),
      suffix(Base,XChars)	 % module loaded with .pl ending
    ; suffix(PathChars,XChars)), % or without
    remove_path(XChars,CharsWithoutPath),
    atom_chars(X,CharsWithoutPath).
unwrap_module(X,X) :- !. % might even be unwrapped

:- use_module(library(lists)).

remove_path(L,L2) :-
    reverse(L,LR),
    nth0(N,LR,'/',_), %key code of /
    sublist(LR, LR2, 0, N, A),
    reverse(LR2,L2).