:- package(wui).
% Package for defining Web-based UI components
% Author: Jose F. Morales

:- use_module(library(fibers/fibers_data)).
:- use_module(library(fibers/fibers_rt), ['$fiber_susp'/2]).

:- use_module(wui(wui_html)).
:- include(wui(wui_hooks)).

