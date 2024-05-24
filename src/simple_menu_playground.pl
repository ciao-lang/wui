%(included file)
%
% \title Base for a simple Ciao playground using library(menu)
% 
% \module This file provides the base implementation for a playground
%   that uses library(menu). Implement by filling the next undefined
%   predicates.

% Detect the toolbar needed for the given file
:- pred detect_toolbar(LangMode, Filename, Toolbar). % (abstract)

% Set menu flags for specific commands
:- pred prepare_cmd_flags(Cmd). % (abstract)

% Execute command (given flags)
:- pred run_cmd(InFile). % (abstract)

% Obtain output (OutName = '' if no output)
:- pred output_file(Name, OutName, Kind). % (abstract)

% Default output for new inputs
:- pred default_output(LangMode, OutputKind, Output). % (abstract)

% ---------------------------------------------------------------------------
% (Configuration)

%debug_mode.
debug_mode :- fail.

% Directory for saving persistent data (and logs)
% (ignored if not existing)
:- multifile deploy_data_root_dir/1.
deploy_data_root_dir('/saved'). % our docker deployment (see ciaopp_webdemo repository)
%deploy_data_root_dir('/tmp/saved'). % (must be created by user)
%deploy_data_root_dir(_) :- fail. % no logging

timeout(10000). % milliseconds % TODO: customize?

% TODO: use watchdog for other timeouts (active module, container, and
% server based level)

% ---------------------------------------------------------------------------

:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(library(lists)).
:- use_module(library(system)).

:- use_module(library(io_port_reify), [io_once_port_reify/4]).
:- use_module(library(port_reify), [port_call/1]).

:- use_module(library(timeout), [call_with_time_limit/3]). % TODO: experimental!

:- use_module(library(system_extra), [del_file_nofail/1]).

:- use_module(library(pathnames), [path_concat/3, path_split/3]).

:- use_module(library(menu/menu_json)).
:- use_module(wui(ciao_playground)).
:- use_module(wui(ciao_pg_ui), _, [active]).

:- use_module(library(pillow/json)).

:- use_module(wui(playground_aux)).

% ---------------------------------------------------------------------------

% (Definition of toolbars, menus, etc.)
:- include(wui(wui_hooks)).

% ---------------------------------------------------------------------------

% TODO: see toolbar uses of this command, replace by stub call
:- suspendable(kill).
kill :- fail.

% ---------------------------------------------------------------------------

:- suspendable('__init__').
'__init__' :-
    pg_setup(~pg_title), 
    get_toolbar_and_init.

:- suspendable(get_toolbar_and_init, [cached_step]).
get_toolbar_and_init :-
    load_file(~splash_file).

% ---------------------------------------------------------------------------

:- suspendable(load_file(atm), [nosideff_step]).
load_file(Filename) :-
    load_example(Filename, InputPrg),
    load_common(Filename, InputPrg).

:- suspendable(load_common(term, term)).
load_common(Filename, InputPrg) :-
    detect_langmode(Filename, LangMode),
    detect_toolbar(LangMode, Filename, Toolbar),
    toolbar_to_json(Toolbar, ~actmod_get_self, Items), % TODO: add others
    default_output(LangMode, OutputKind, Output),
    output_kind_split(OutputKind, OutputKind1, OutputKind2),
    set_main_toolbar(Items),
    set_buf('in', ""), % clean input
    buf_mode('in', LangMode),
    set_buf('in', InputPrg),
    pg_set_output(OutputKind1, OutputKind2, Output),
    focus_buf('in').

output_kind_split(editor(LangMode), editor, LangMode).
output_kind_split(html_view, view, html).

% TODO: allow terms in encoding?
:- suspendable(pg_set_output(atm, atm, string)).
pg_set_output(editor, LangMode, Output) :- !,
    buf_mode('out', LangMode),
    set_buf('out', Output).
pg_set_output(view, html, Output) :- !,
    set_html_view('out', Output).

% ---------------------------------------------------------------------------

:- suspendable(upload_file(blob(atm, blob1_filename), blob(string, blob1))).
upload_file(InputFilename, InputPrg) :-
    % NOTE: InputFilename is the name of the uploaded file!
    load_common(InputFilename, InputPrg).

% ---------------------------------------------------------------------------

:- suspendable(help).
help :-
    % Help message dialog
    show_modal([
      html = string(~help_html),
      toolbar = ~toolbar_to_json(bar_close, ~actmod_get_self)
    ]).

% Single button close toolbar
:- impl(toolbar, bar_close).

(bar_close as toolbar).def('Close', cancel).

% ---------------------------------------------------------------------------

:- use_module(library(aggregates), [findall/3]).

:- suspendable(viewfiles).
viewfiles :-
    % File selection dialog
    findall(X, enum_examples(X), Files),
    open_msg(Msg),
    show_modal([
      html = string(Msg),
      itemlist = ~atomiclst_to_json_strlist(Files),
      html = string("<br />"),
      toolbar = ~toolbar_to_json(bar_load_cancel, ~actmod_get_self)
    ]).

% "Load or cancel" toolbar
:- impl(toolbar, bar_load_cancel).
% TODO: additional use_upload_filename property?
(bar_load_cancel as toolbar).def('Load', upload_file-[use_upload]).
(bar_load_cancel as toolbar).def('Cancel', cancel).

% ---------------------------------------------------------------------------

:- suspendable(cancel).
cancel.

% ---------------------------------------------------------------------------

:- suspendable(customize, [cached_step]).
customize :- % A customized command 
    %
    % TODO:T253 set_buf/2 is not needed here; it is a dummy goal
    %   on browser to enforce RPC method 'cached_step'. The reason
    %   is that toolbar_json does not support 'cached_step'. It
    %   should share code with async_json_encode_response_lit/2 .
    %
    set_buf('console', ""), % dummy!
    get_menu_and_show.

% TODO:T253 document 'cached_step': memoize the local computation on
%    the remote node, re-execute the continuation on the remote node

:- suspendable(get_menu_and_show, [cached_step]).
get_menu_and_show :-
    menu_to_json(all, Items), % TODO: add 'java_all' too
    show_modal([
      html = string("Select flag values for a custom command:"),
      menu = Items,
      toolbar = ~toolbar_to_json(bar_custom, ~actmod_get_self)
    ]).

% "Custom command" toolbar
:- impl(toolbar, bar_custom).
(bar_custom as toolbar).def('Proceed', custom_do-[use_input, extra([show_in_progress = true])]).
(bar_custom as toolbar).def('Cancel', cancel).

% ---------------------------------------------------------------------------

% TODO: add a delay before show_in_progress? (so that fast enough operations
%   do not show it)

:- suspendable(pg_cmd_common(term, term)).
pg_cmd_common(Cmd, Input) :-
    do_cmd(Cmd, Input, Output, OutputKind, ConsoleStr),
    output_kind_split(OutputKind, OutputKind1, OutputKind2),
    show_in_progress(false),
    set_buf('console', ConsoleStr),
    pg_set_output(OutputKind1, OutputKind2, Output),
    focus_buf('in'). % (focus on input again)

% ---------------------------------------------------------------------------
% TODO: part of this functionality should be in ciao/ciaopp itself
% TODO: see ciaopp_master.pl (for distributed, timeouts, etc.)

% TODO: save previous flags

:- use_module(library(persdb/datadir), [ensure_datadir/2]).
:- use_module(library(system), [mktemp_in_tmp/2]).
:- use_module(library(stream_utils), [string_to_file/2, file_to_string/2]).

:- use_module(wui(logcmd)).

logcmd_dir(Dir) :-
    Dir = ~path_concat(~self_mod, 'logcmd').

check_logging :-
    logcmd_enabled, % already enabled
    !.
check_logging :-
    using_deploy,
    ensure_datadir(~logcmd_dir, Dir),
    !,
    logcmd_enable(Dir).
check_logging. % cannot enable

% Running on a machine with a deploy data directory
using_deploy :-
    deploy_data_root_dir(D),
    file_exists(D).

do_cmd(Cmd, Input, Output, OutputKind, ConsoleStr) :-
    check_logging, % (enable logcmd if possible and required)
    %
    % Create a temporary module with Input
    create_tmp_base(InBase), % (new name)
    atom_concat(InBase, '.pl', InFile),
    string_to_file(Input, InFile),
    path_split(InBase, _, InMod),
    % Log input (optional)
    logcmd_id(InMod, LogId),
    logcmd_term(LogId, cmd, Cmd),
    logcmd_string(LogId, input, Input),
    % Prepare menu flags for run_cmd/1
    get_menu_flags(OldFlags),
    prepare_cmd_flags(Cmd),
    % Execute (through run_cmd/1) and get standard output/error messages
    io_once_port_reify(run_cmd_with_time_limit(InFile, GotTimeout),
                       Port, OutString, ErrString),
    ( GotTimeout == yes -> % Timeout!
        OutputFile = '',
        timeout_msg(Output, ConsoleStr)
    ; append(OutString, ErrString, ConsoleStr),
      output_file(InFile, OutputFile, OutputKind),
      % Read output
      ( OutputFile = '' -> Output = "% (no output)"
      ; file_to_string(OutputFile, Output)
      )
    ),
    % Log output, console, and exit status (optional)
    logcmd_string(LogId, output, Output),
    logcmd_string(LogId, console, ConsoleStr),
    logcmd_term(LogId, exit, Port),
    % Remove temporary files
    del_file_nofail(InFile),
    ( OutputFile = '' -> true ; del_file_nofail(OutputFile) ),
    % Restore previous flags
    restore_menu_flags_list(OldFlags),
    % Use run_cmd/1 exit status
    port_call(Port).

run_cmd_with_time_limit(InFile, GotTimeout) :-
    call_with_time_limit(~timeout, run_cmd(InFile), GotTimeout = yes),
    ( var(GotTimeout) -> GotTimeout = no ; true ).

timeout_msg("% No output due to timeout!", "Timeout!").

create_tmp_base(TmpBase) :- % TODO: should we create TmpBase file??? otherwise there may be conflicts
    mktemp_in_tmp('modXXXXXX', TmpBase).

:- use_module(library(menu/menu_generator), [
    % TODO: at least these operations should be in a separate module (menu_db?)
    get_menu_flags/1,
    restore_menu_flags_list/1]).

% ---------------------------------------------------------------------------
% Debugging logs

% Stored at <datadir>/Mod/debug.log

debug_log_file(LogFile) :-
    ensure_datadir(~self_mod, Dir),
    LogFile = ~path_concat(Dir, 'debug.log').

% Display T on debug log
debug_log(T) :-
    debug_log_file(LogFile),
    open(LogFile,append,S),
    display(S,T),nl(S),
    close(S).
