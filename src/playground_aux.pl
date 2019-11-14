:- module(playground_aux, [], [assertions, fsyntax, dcg]).

:- use_module(library(pathnames), [path_concat/3, path_splitext/3]).
:- use_module(library(lists), [member/2]).

% ---------------------------------------------------------------------------

:- export(detect_langmode/2).
% Detect language from filename F
detect_langmode(F, LangMode) :-
    % Detect from extension
    path_splitext(F, _, Ext),
    ext_langmode(Ext, LangMode),
    !.
detect_langmode(_, 'text/plain'). % plain text otherwise

% TODO: duplicated somewhere else!
ext_langmode('.xc', 'text/x-csrc').
ext_langmode('.c', 'text/x-csrc').
ext_langmode('.h', 'text/x-chdr').
ext_langmode('.cc', 'text/x-c++src').
ext_langmode('.hh', 'text/x-c++hdr').
ext_langmode('.java', 'text/x-java').
ext_langmode('.js', 'javascript').
ext_langmode('.pl', 'ciao').
ext_langmode('.txt', 'text/plain').

% ---------------------------------------------------------------------------

:- use_module(library(sort), [sort/2]).
:- use_module(library(system), [directory_files/2]).

:- export(examples_at/4).
% Enumerate all files F+Ext at Dir+Prefix directory as Prefix+F+Ext
examples_at(_Dir, Prefix, Ext, Rel) :-
    atom_concat('NEW', Ext, NewName),
    path_concat(Prefix, NewName, Rel). % TODO: dummy for NEW files (improve?)
examples_at(Dir, Prefix, Ext, Rel) :-
    ( Prefix = '' -> Dir2 = Dir ; path_concat(Dir, Prefix, Dir2) ),
    directory_files(Dir2, Xs),
    sort(Xs, Xs2),
    member(X, Xs2),
    \+ X = '.',
    \+ X = '..',
    atom_concat(_, Ext, X),
    path_concat(Prefix, X, Rel).

% ---------------------------------------------------------------------------

:- use_module(library(pathnames), [path_is_relative/1, path_split_list/2]).

:- export(sane_path/1).
% Check that the path is relative and does not contain '..'
sane_path(File) :-
    path_is_relative(File),
    path_split_list(File, Cs),
    \+ (member(C, Cs), C = '..').



