% ===========================================================================
% Traits for Web-based UI

:- use_package(traits).

% Page layout
:- trait(layout, [
    css_links/1,
    js_scripts/1,
    render_main/1
]).

% Navigation tabs
:- trait(navtab, [
    title/1,
    render/1
]).

% Toolbars
:- trait(toolbar, [
    def/2 % def(ButtonText, Cmd): where Cmd is an async command
]).

% For menus (settings)
:- use_package('menu/menu').

