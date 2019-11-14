:- module(logcmd, [], [assertions, fsyntax, datafacts]).

:- doc(title, "Command execution logs (for deployment machine)").

:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(wui(timestamp_date), [timestamp_atom/1]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(stream_utils), [string_to_file/2]).
:- use_module(library(pathnames), [path_concat/3]).

:- data logging/1.

% Enable logcmd
:- export(logcmd_enable/1).
logcmd_enable(Dir) :-
    retractall_fact(logging(_)),
    !,
    assertz_fact(logging(Dir)).

% Disable logcmd
:- export(logcmd_disable/0).
logcmd_disable :-
    retractall_fact(logging(_)).

% logcmd is enabled
:- export(logcmd_enabled/0).
logcmd_enabled :-
    current_fact(logging(_)), !.

:- export(logcmd_id/2).
% Create a new Id for logging (using temporary Mod name)
logcmd_id(TmpMod, LogId) :- logging(_), !,
    timestamp_atom(T),
    atom_concat(T, '-', T2),
    atom_concat(T2, TmpMod, LogId).
logcmd_id(_, none).

:- export(logcmd_string/3).
% Write a string
logcmd_string(LogId, Suffix, Str) :-
    logcmd_file(LogId, Suffix, F),
    !,
    string_to_file(Str, F).
logcmd_string(_LogId, _Suffix, _Str). % no logging

:- export(logcmd_term/3).
% Write a term
logcmd_term(LogId, Suffix, T) :-
    logcmd_file(LogId, Suffix, F),
    !,
    open(F,write,S),
    displayq(S,T),nl(S),
    close(S).
logcmd_term(_LogId, _Suffix, _T). % no logging

% (fail if logging is not enabled)
logcmd_file(LogId, Suffix, F) :-
    logging(D),
    !,
    atom_concat(LogId, '-', Name0),
    atom_concat(Name0, Suffix, Name),
    path_concat(D, Name, F).
