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
:- doc(section, "Encode a toolbar as JSON (for playground)").

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(lists), [member/2]).
:- use_module(library(pillow/json), [atomic_to_json_str/2]).

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


