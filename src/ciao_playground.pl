:- module(ciao_playground, [], [assertions, fsyntax, dcg]).

% TODO: this should be the file with js_foreign declarations for a ciao_playground active module
%   (interface to ciao_playground foreign active module)

:- doc(title, "Driver for Ciao Playground server (using JSON protocol)").

:- doc(module, "This module implements a driver for a Ciao Playground
   server.

   This module requires:

   @begin{itemize}
   @item @lib{ciao-playground.js}: available JS-side commands and 
     generic UI components.
   @end{itemize}

   which depend on:
   @begin{itemize}
   @item @lib{ciao-actmod.js}: JS interface to Ciao active modules
     (needs @lib{actmod_http.pl})
   @item @lib{actmod_http.pl}: HTTP gateway for active modules
   @end{itemize}
").

:- doc(bug, "server side continuations must be the last in the list;
   client side does not support async yet (but it would be trivial to
   fix)").
:- doc(bug, "generalize; it could interface with any JS code (not only playground)").
:- doc(bug, "use html2terms/2 and string(_) to pass HTML as strings in json terms").

% ---------------------------------------------------------------------------

:- include(wui(wui_hooks)).

% ---------------------------------------------------------------------------
:- doc(section, "Encode menu as JSON (for playground)").

:- use_module(library(pillow/json), [atomic_to_json_str/2, atomiclst_to_json_strlist/2]).

:- use_module(library(menu/menu_generator), [generate_offline_menu/2]).
:- use_module(library(menu/menu_generator), [decomp_menu_node/3]).

:- export(menu_to_json/2).
menu_to_json(MenuName, Items) :-
	generate_offline_menu(MenuName, MenuItems),
	items_to_json(MenuItems, Items).

% From menu_item/7 to JSON representation
items_to_json([], []).
items_to_json([M|Ms], [X|Xs]) :-
	M = menu_item(Menu, Flag, Title, Help, Options, Def_Opt, Guard),
	decomp_menu_node(Menu, Menu0, Level),
	X = json([kind = string("menu_item"),
	          menu = ~atomic_to_json_str(Menu0),
	          level = ~atomic_to_json_str(Level),
	          flag = ~atomic_to_json_str(Flag),
		  title = ~atomic_to_json_str(Title),
		  help = ~atomic_to_json_str(Help),
		  options = ~opts_to_json(Options),
		  def_opt = ~atomic_to_json_str(Def_Opt),
		  guard = ~or_guard(Guard)]),
	items_to_json(Ms, Xs).	  

opts_to_json(ask(A, _)) := R :- !, % TODO: write a different type
	R = ~atomic_to_json_str(A).
opts_to_json(alist(As)) := R :- !, % TODO: write a different type
	atomiclst_to_json_strlist(As, R).
opts_to_json(Options) := _ :- % TODO: incorrect
	throw(error(bad_options(Options), menu_to_json/2)).

or_guard([]) := [] :- !.
or_guard([G|Gs]) := [~and_guard(G)|Xs] :-
	Xs = ~or_guard(Gs).

and_guard([]) := [] :- !.
and_guard([G|Gs]) := [~guard_lit(G)|Xs] :-
	Xs = ~and_guard(Gs).

% TODO: write in a nicer way
guard_lit(F=V) := R :-
	( V = neq(A) ->
	    R = [string("!="), ~atomic_to_json_str(F), ~atomic_to_json_str(A)]
	; R = [string("=="), ~atomic_to_json_str(F), ~atomic_to_json_str(V)]
	).

% ---------------------------------------------------------------------------

:- use_module(library(menu/menu_generator), [comp_menu_node/3]).

:- export(get_menu_values/2).
% (from json answer to flag values in the same format required by
%  menu_generator:restore_menu_flags_list/1)
get_menu_values(json(Xs), FlagValuesList) :-
	get_menu_values_(Xs, FlagValuesList).

get_menu_values_([], []).
get_menu_values_([Flag=Args|Fs], [(Menu,Flag,Value)|FVs]) :-
	% TODO: not very nice (decodes menu, flag, and value)
	Args = [string(Menu0),string(Level0),string(Value0)],
	number_codes(Level, Level0),
	atom_codes(Menu1, Menu0),
	comp_menu_node(Menu1, Level, Menu),
	atom_codes(Value, Value0), % TODO: value may be numeric too!
	!,
	get_menu_values_(Fs, FVs).
get_menu_values_([_|Fs], FVs) :-
	get_menu_values_(Fs, FVs).

% ---------------------------------------------------------------------------
:- doc(section, "Encode a toolbar as JSON (for playground)").

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(lists), [member/2]).

:- export(toolbar_to_json/3).
toolbar_to_json(ToolbarName, Mod, Items) :-
	findall(X, toolbar_json(ToolbarName, Mod, X), Items).

% (nondet)
toolbar_json(ToolbarName, Mod, X) :-
	(ToolbarName as toolbar).def(Title, Cmd0),
	nonvar(Cmd0),
	( Cmd0 = Cmd-Props ->
	    cmd_opts(Props, Extra, Extra1),
	    extra(Props, Extra1, [])
	; Cmd = Cmd0,
	  Extra = []
	),
	atom_concat(Mod, '.', MCmd0), % TODO: use mod_concat
	atom_concat(MCmd0, Cmd, MCmd),
	X = json([text = ~atomic_to_json_str(Title),
	          value = ~atomic_to_json_str(MCmd)|Extra]).

cmd_opts(Props) -->
	( { member(use_input, Props) } -> [use_input = true] ; [] ),
	( { member(use_upload, Props) } -> [upload = true] ; [] ).

extra(Props) --> { member(extra(Extra), Props) }, !,
	emit(Extra).
extra(_Props) --> [].

emit([]) --> [].
emit([X|Xs]) --> [X], emit(Xs).


