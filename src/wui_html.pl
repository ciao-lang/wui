:- module(wui_html, [], [assertions, dcg, fsyntax, hiord, fibers, datafacts]).

:- doc(title, "Helper HTML rendering for Web-based UI").
:- doc(author, "Jose F. Morales").

:- use_module(engine(io_basic)).
:- use_module(library(lists), [member/2, append/3]).
:- use_module(library(pathnames), [path_concat/3]).

:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(ciaobld(third_party_custom), [third_party_custom_path/2]).

% ---------------------------------------------------------------------------

:- use_module(library(fibers/fibers_blt)).
:- use_module(library(fibers/fibers_data)).
:- use_module(library(fibers/fibers_rt)).
:- use_module(library(actmod/actmod_rt), [actmod_get_self_mod/1]).
:- use_module(library(actmod_http), ['fiberSusp->query_str'/2]).

% ---------------------------------------------------------------------------

:- include(wui(wui_hooks)).

% ===========================================================================
% Internal state for env_render_page/1

% TODO: Add as 'temporary' transient data
% TODO: merge with run_cmd (e.g., ciao-playground-actmod.js)

:- data env_data/2.

:- export(env_init/0).
env_init :-
    retractall_fact(env_data(_,_)).

:- export(env_write/2).
env_write(Name, Value) :-
    retractall_fact(env_data(Name, _)),
    assertz_fact(env_data(Name, Value)).

:- export(env_read/2).
env_read(Name) := Value :-
    ( current_fact(env_data(Name, Value0)) -> Value = Value0 ; fail ).

% ===========================================================================
% Local-side Env updates

:- suspendable(set_title(term)).
set_title(Title) :-
    env_write(title, Title).

:- suspendable(set_layout(term)).
set_layout(Layout) :-
    env_write(layout, Layout).

:- suspendable(set_output_html(term)).
set_output_html(Xs) :-
    env_write(output_html, Xs).

:- suspendable(pause_and_call(term, term)).
% Execute FiberSusp after DelayMs milliseconds
pause_and_call(DelayMs, FiberSusp) :-
    InitJSCmd = ~'fiberSusp->jscmd'(FiberSusp),
    JSCmd = set_timeout(InitJSCmd, DelayMs),
    yield_residue(jscmd(JSCmd)). % (forces suspension for residue_reify/2)

% ===========================================================================

% Set fact on transient data
% :- meta_predicate do_set_fact(transient).
:- suspendable(do_set_fact(term)).
do_set_fact(Opt) :-
    t_defined_data(Opt),
    t_set_fact(Opt).

:- suspendable(push_window_location(term)). % (builtin)
% (see 'fiberSusp->jscmd'/2 for implementation)
push_window_location(_) :- throw(error(undefined, wui_html:push_window_location/1)).

:- suspendable(redraw(term)). % (builtin)
% Redraw using: push_window_location + dumpHTML
% (see 'fiberSusp->jscmd'/2 for implementation)
redraw(_) :- throw(error(undefined, wui_html:redraw/1)).

:- suspendable(redraw_nb(term)). % (builtin)
% Redraw using: replace_window_location + dumpHTML
% (see 'fiberSusp->jscmd'/2 for implementation)
redraw_nb(_) :- throw(error(undefined, wui_html:redraw_nb/1)).

% TODO: is there a better way?
% (hook for '$fiber_susp'/2)
'$fiber_susp_hook'('wui_html.redraw'(PA2)) := PA :- !,
    PA2 = '$fiberSusp'(FiberData, G2),
    PA = '$fiberSusp'(FiberData, 'wui_html.redraw'(G2)).
'$fiber_susp_hook'('wui_html.redraw_nb'(PA2)) := PA :- !,
    PA2 = '$fiberSusp'(FiberData, G2),
    PA = '$fiberSusp'(FiberData, 'wui_html.redraw_nb'(G2)).

% ---------------------------------------------------------------------------

:- suspendable(dumpHTML(term)). % (internal -- needs goal as argument)
% Recompute the HTML view after running Mod:setup (on
% actmod_get_self_mod/1) and Goal (env_init/0 is called to cleanup the
% view data).  This view may encode residual goals.
dumpHTML(Goal) :-
    actmod_get_self_mod(MainMod),
    env_init,
    residue_reify(wui_html:setup_and_call(MainMod, Goal), Residue),
    render_and_output(Residue).

:- suspendable(setup_and_call(term,term)). % (internal)
setup_and_call(Mod, Goal) :-
    fiber_meta_call(Mod:setup),
    fiber_meta_call(Goal).

:- suspendable(render_and_output(term)). % (internal)
render_and_output(Residue) :-
    % Render page (from current env) into HTML
    env_render_page(Residue, HTML),
    yield_residue(html(HTML)). % (forces suspension for residue_reify/2)

% ---------------------------------------------------------------------------

:- export(wui_redraw/2).
wui_redraw(PA) := ~fiberSusp(redraw(PA)).

:- export(wui_redraw_nb/2).
wui_redraw_nb(PA) := ~fiberSusp(redraw_nb(PA)).

% ===========================================================================

:- doc(section, "Page rendering code").

% :- export(env_render_page/2).
env_render_page(Cont, Out) :-
    Layout = ~get_layout,
    % TODO: translate Cont to JS before
    render_layout(Layout, Cont, Out, []).

get_layout := Layout :-
    ( Layout0 = ~env_read(layout) -> Layout = Layout0
    ; Layout = none
    ).

% Render page as HTML
render_layout(Layout, Cont) -->
    [start],
    [begin(head)],
    ( { Title = ~env_read(title) } ->
        [title(Title)]
    ; []
    ),
    { (Layout as layout).css_links(Links) },
    env_links(Links),
    [end(head)],
    % TODO: use role='document' in body? (see web standards)
    [begin(body, [])],
    { (Layout as layout).render_main(MainR) },
    emit(MainR),
    % Placed at the end of the document so the pages load faster
    % TODO: Add ciao-actmod.js if needed
    { (Layout as layout).js_scripts(Scripts) },
    env_scripts(Scripts),
    emit_residual_js(Cont),
    [end(body)],
    [end].

% JS code for residual continuation
emit_residual_js(Cont) -->
    % TODO: use residual json instead!
    % TODO: use actmod_http:encode_response instead
    { Cont = ['fibers_blt.yield_residue'(jscmd(JSCmd))] },
    !,
    { jscmd_to_js(JSCmd, JS, []) },
    env_script_src(JS).
emit_residual_js(Cont) -->
    { Cont = [] -> true 
    ; display(user_error, bug_cont_residual_ignored(Cont)), nl(user_error)
    }.

% ---------------------------------------------------------------------------

:- impl(layout, none).

(none as layout).render_main(R) :-
    R = [~env_read(output_html)].

(none as layout).css_links([]).

(none as layout).js_scripts([]).

% ---------------------------------------------------------------------------

:- impl(layout, bootstrap_layout/2).

% TODO: bootstrap components documented at http://getbootstrap.com/components
% TODO: customized styles: http://getbootstrap.com/customize/

layout_theme(bootstrap_layout(Theme,_)) := Theme.

% Render
(bootstrap_layout(_, Main) as layout).render_main(A) :-
    render_main(Main, A, []).

(bootstrap_layout(Theme, Main) as layout).css_links(Xs) :-
    Layout = bootstrap_layout(Theme, Main),
    findall(X, css_link(Layout, X), Xs).

(bootstrap_layout(_,_) as layout).js_scripts(Xs) :-
    ( use_bootstrap -> Xs = ['jquery.min.js', 'js/bootstrap.min.js']
    ; Xs = []
    ).

render_main(nav(Brand, Navtabs)) -->
    { t_current_fact(~apply1(navtab, Navtab)) },
    render_navbar(Brand, Navtabs),
    render_navtab(Navtab).
render_main(singletab(Navtab)) -->
    render_navtab(Navtab).

% Render a navtab
render_navtab(Navtab) -->
    { (Navtab as navtab).render(R) },
    [env(div, [class='container-fluid', role='main'], R)].

% ---------------------------------------------------------------------------

% Navtab with rendering of a single element (specified by CurrNavtab)

% 'ui_pred.depends'([navtab/1])
render_navbar(Brand, Navtabs) -->
    % Fixed navbar for selecting the current navtab
    { t_current_fact(~apply1(navtab, CurrNavtab)) -> true ; fail },
    { navtab_queries(Navtabs, CurrNavtab, Queries, []) },
    wui_elem(navbar(Brand, Queries)).

navtab_queries([], _CurrNavtab) --> [].
navtab_queries([O|Os], CurrNavtab) -->
    navtab_query(O, CurrNavtab),
    navtab_queries(Os, CurrNavtab).

navtab_query(Navtab, CurrNavtab) -->
    { Navtab = CurrNavtab -> Atts = [active|Atts2] ; Atts = Atts2 },
    { Atts2 = [] }, % TODO: add .atts?
    { (Navtab as navtab).title(TitleR) },
    [wui_item(~wui_redraw(~fiberSusp(do_set_fact(navtab(Navtab)))), Atts, TitleR)].

% ---------------------------------------------------------------------------
% Bootstrap themes

% use_bootstrap :- fail.
use_bootstrap.

:- include(library(http/http_server_hooks)).

% Path to bootstrap HTML framework
'httpserv.file_path'('', F) :-
    path_concat(~third_party_custom_path(bower_components), 'bootstrap/dist', F).
% (jquery is needed for bootstrap dropdown menus)
'httpserv.file_path'('', F) :-
    path_concat(~third_party_custom_path(bower_components), 'jquery/dist', F).

:- export(bootstrap_theme/1).
% TODO: ad-hoc (see bootswatch)
% (bootstrap)
bootstrap_theme(default).
bootstrap_theme(extended).
% (bootswatch)
bootstrap_theme(amelia).
bootstrap_theme(paper).
bootstrap_theme(cerulean).
bootstrap_theme(readable).
bootstrap_theme(cosmo).
bootstrap_theme(sandstone).
bootstrap_theme(custom).
bootstrap_theme(simplex).
bootstrap_theme(cyborg).
bootstrap_theme(slate).
bootstrap_theme(darkly).
bootstrap_theme(spacelab).
bootstrap_theme(flatly).
bootstrap_theme(superhero).
bootstrap_theme(journal).
bootstrap_theme(united).
bootstrap_theme(lumen).
bootstrap_theme(yeti).

% ---------------------------------------------------------------------------
% CSS for (bootstrap) themes

theme_dir(Theme, Dir) :-
    bootstrap_theme(Theme),
    atom_concat(Theme, '-theme', Dir).

'httpserv.file_path'(Dir, F) :-
    theme_dir(Theme, ThemeDir),
    atom_concat('/', ThemeDir, Dir),
    path_concat(~third_party_custom_path(bower_components), 'bootswatch', D),
    path_concat(D, Theme, F).

css_link(_) := 'css/lpdoc.css'. % LPdoc % TODO: make it optional
css_link(Layout) := ~theme_css_link(~layout_theme(Layout)) :- use_bootstrap.
css_link(_Layout) := ~common_css_link.

common_css_link := 'css/theme.css'. % Custom style
common_css_link := 'css/ciao-htmlfontify.css'. % Custom style for emacs htmlfontify

theme_css_link(default) := R :- !,
    R = 'css/bootstrap.min.css'.
theme_css_link(extended) := R :- !,
    ( R = 'css/bootstrap.min.css'
    ; R = 'css/bootstrap-theme.min.css'
    ).
theme_css_link(Theme) := R :-
    theme_dir(Theme, ThemeDir),
    R = ~path_concat(ThemeDir, 'bootstrap.min.css').

%% css('/bower_components/source-sans-pro/source-sans-pro.css').
%% css('/bower_components/source-code-pro/source-code-pro.css').
%% css('/css/normalize.css').
%% css('/css/ciao-playground.css').
%% js_script('/js/split.min.js').
%% js_script('/js/cm-ciao-mode.js').
%% js_script('/js/ciao-actmod.js').
%% js_script('/js/ciao-playground-ui.js').
%% js_script('/js/ciao-playground-actmod.js').

% ---------------------------------------------------------------------------
% TODO: duplicated (htmlurl/1 changes)

htmlurl := '/'.

:- pred prefix_htmlurl(Path, Path2) :: atm * atm
   # "Prefix @var{Path} with value of @tt{htmlurl} (if needed)".
prefix_htmlurl(Path) := Path2 :-
    HtmlURL = ~htmlurl,
    !,
    ( HtmlURL = '' -> Path2 = Path
    ; path_concat(HtmlURL, Path, Path2)
    ).
prefix_htmlurl(Path) := Path. % no htmlurl value

% ---------------------------------------------------------------------------

:- export(env_links/3).
env_links([]) --> [].
env_links([X|Xs]) --> env_link(X), env_links(Xs).

:- export(env_scripts/3).
env_scripts([]) --> [].
env_scripts([X|Xs]) --> env_script(X), env_scripts(Xs).

:- export(env_script/3).
env_script(Src) -->
    { prefix_htmlurl(Src, Src2) },
    [env(script, [src=Src2], [])].

:- export(env_script_src/3).
env_script_src(Src) -->
    [begin(script, [])],
    emit(Src),
    [end(script)].

:- export(env_link/3).
env_link(Src) -->
    { prefix_htmlurl(Src, Src2) },
    [elem(link, [rel='stylesheet', href=Src2])].

% ---------------------------------------------------------------------------

:- doc(section, "Custom views and widgets").

:- export(emit_iframe/3).
emit_iframe(File) -->
    { prefix_htmlurl(File, File2) },
    [env(div, [style='position: relative; width: 100%; height: 0px; padding-bottom: 60%;'], [
       env(iframe, [style='position: absolute; left: 0px; top: 0px; width: 100%; height: 100%; border: 1px solid #eee',
                    src=File2], [])
     ])].

:- export(emit_svg/3).
emit_svg(File) -->
    { prefix_htmlurl(File, File2) },
    % TODO: add HTML 'id' if needed
    [env(object, [data=File2, type='image/svg+xml', style='max-width: 100%'], [])].

% ---------------------------------------------------------------------------

:- doc(section, "Expand WUI elements").

:- export(emit/3).
emit([]) --> [].
emit([X|Xs]) --> [X], emit(Xs).

% Expand WUI elements as HTML
:- export(wui_elem/3).
% wui_elem(+,+,?)
wui_elem(dropdown(Text, Items)) --> !,
    emit_dropdown(Text, Items).
wui_elem(nav(Items)) --> !,
    emit_nav(Items).
wui_elem(navbar(Brand, Items)) --> !,
    emit_navbar(Brand, Items).
wui_elem(breadcrumb(Items)) --> !,
    emit_breadcrumb(Items).
wui_elem(button(PA, Style, Text)) --> !,
    emit_button(PA, Style, Text).
wui_elem(link(PA, Atts, Text)) --> !,
    [~fiberSusp_to_link(PA, Atts, Text)].

% ---------------------------------------------------------------------------

emit_button(PA, Style, Text) -->
    { button_style(Style, HtmlAtts) },
    [~fiberSusp_to_link(PA, HtmlAtts, Text)].

button_style(default, [role='button', class='btn btn-xs btn-default']).
button_style(primary, [role='button', class='btn btn-xs btn-primary']).
button_style(success, [role='button', class='btn btn-xs btn-success']).
button_style(info, [role='button', class='btn btn-xs btn-info']).
button_style(warning, [role='button', class='btn btn-xs btn-warning']).
button_style(danger, [role='button', class='btn btn-xs btn-danger']).

% ---------------------------------------------------------------------------

fiberSusp_to_link(PA, Atts, Text) := ~jscmd_to_link(~'fiberSusp->jscmd'(PA), Atts, Text).

% ---------------------------------------------------------------------------

emit_dropdown(Text, Items) -->
    [begin(div, [class='btn-group'])],
    { append(Text, [' ', env(span, [class='caret'], [])], Text2) },
    [env(button, [type='button', class='btn btn-default dropdown-toggle', 'data-toggle'='dropdown'], Text2)],
    [begin(ul, [class='dropdown-menu', role='menu'])],
    emit_dropdown_items(Items),
    [end(ul)],
    [end(div)].

emit_dropdown_items([]) --> [].
emit_dropdown_items([X|Xs]) -->
    emit_dropdown_item(X),
    emit_dropdown_items(Xs).

emit_dropdown_item(--) --> !,
    [env(li, [class='divider'], [])].
emit_dropdown_item(wui_item(PA, Atts, Text)) -->
    { Xs = ~fiberSusp_to_link(PA, Atts, Text) },
    [env(li, [], Xs)].

% ---------------------------------------------------------------------------

:- use_module(library(aggregates), [findall/3]).

:- export(emit_option_dropdown/5).
:- meta_predicate emit_option_dropdown(?,?,?,?,?).
emit_option_dropdown(F, Range, Text) -->
    emit_option_dropdown(F, Range, Text, default_term_render).

:- export(emit_option_dropdown/6).
:- meta_predicate emit_option_dropdown(?,?,?,pred(2),?,?).
% emit_option_dropdown(F,Range,Text,Render,?,?):
%   A dropdown for an option (unary transient data) with options from
%   list Range, rendered by predicate Render.

emit_option_dropdown(F, Range, Text, Render) -->
    { CurrFact = ~apply1(F, CurrX) },
    { ( t_current_fact(CurrFact) ->
          MaybeCurrX = yes(CurrX),
          Render(CurrX, CurrText)
      ; MaybeCurrX = no,
        CurrText = '(select)'
      ),
      FullText = [Text, ': ', CurrText]
    },
    { option_items(Range, F, MaybeCurrX, default_term_render, Items, []) },
    wui_elem(dropdown(FullText, Items)).

:- meta_predicate option_items(?,?,?,pred(2),?,?).
option_items([], _F, _MaybeCurrX, _RenderOpt) --> [].
option_items([X|Xs], F, MaybeCurrX, RenderOpt) -->
    option_item(X, F, MaybeCurrX, RenderOpt),
    option_items(Xs, F, MaybeCurrX, RenderOpt).

:- meta_predicate option_item(?,?,?,pred(2),?,?).
option_item(X, F, MaybeCurrX, RenderOpt) -->
    { MaybeCurrX = yes(X) -> Atts = [active] ; Atts = [] },
    { Fact = ~apply1(F, X) },
    { RenderOpt(X, Text) },
    [wui_item(~wui_redraw(~fiberSusp(do_set_fact(Fact))), [], Text)].

:- export(default_term_render/2).
default_term_render(X, Text) :-
    term_to_str(X, Str),
    Text = [Str].

% ---------------------------------------------------------------------------

emit_nav(Items) -->
    [begin(ul, [class='nav nav-tabs'])],
    nav_items(Items),
    [end(ul)].

nav_items([]) --> [].
nav_items([X|Xs]) -->
    nav_item(X),
    nav_items(Xs).

nav_item(wui_item(PA, Atts, Text)) -->
    { Xs = ~fiberSusp_to_link(PA, Atts, Text) },
    !,
    { member(active, Atts) -> Atts2 = [class='active'|Atts3] ; Atts2 = Atts3 },
    { Atts3 = [role='presentation'] },
    [env(li, Atts2, Xs)].
nav_item(_Query) --> [].

% ---------------------------------------------------------------------------

emit_navbar(Brand, Items) -->
    [begin(div, [class='navbar navbar-default navbar-fixed-top',
                 role='navigation'])],
    [begin(div, [class='container'])],
    navbar_header(Brand),
    navbar_contents(Items),
    [end(div)],
    [end(div)].

navbar_header(Brand) -->
    [begin(div, [class='navbar-header'])],
    [begin(button, [type='button',
                    class='navbar-toggle collapsed',
                    'data-toggle'='collapse',
                    'data-target'='.navbar-collapse']),
       env(span, [class='sr-only'], ['Toggle navigation']),
       env(span, [class='icon-var'], []),
       env(span, [class='icon-var'], []),
       env(span, [class='icon-var'], []),
     end(button)],
    [env(a, [class='navbar-brand', href='#'], Brand)],
    [end(div)].

navbar_contents(Items) -->
    [begin(div, [class='navbar-collapse collapse'])],
    [begin(ul, [class='nav navbar-nav'])],
    emit_navbar_items(Items, leftnav),
    [end(ul)],
    [begin(ul, [class='nav navbar-nav navbar-right'])],
    emit_navbar_items(Items, rightnav),
    [end(ul)],
    [end(div)]. % /.nav-collapse

% TODO: extend to allow dropdown here?
emit_navbar_items([], _Pos) --> [].
emit_navbar_items([X|Xs], Pos) -->
    emit_navbar_item(X, Pos),
    emit_navbar_items(Xs, Pos).

emit_navbar_item(wui_item(PA, Atts, Text), Pos) -->
    { Xs = ~fiberSusp_to_link(PA, Atts, Text) },
    { member(rightnav, Atts) -> Pos2 = rightnav ; Pos2 = leftnav },
    { Pos = Pos2 },
    !,
    { member(active, Atts) -> Atts2 = [class='active'] ; Atts2 = [] },
    [env(li, Atts2, Xs)].
emit_navbar_item(_Query, _Pos) --> [].

%                begin(li, [class='dropdown']),
%                  env(a, [href='#', class='dropdown-toggle', 'data-toggle'='dropdown'], ['Dropdown ', env(span, [class='caret'], [])]),
%                  begin(ul, [class='dropdown-menu', role='menu']),
%                    env(li, [], [env(a, [href='#'], ['Action'])]),
%                    env(li, [], [env(a, [href='#'], ['Another action'])]),
%                    env(li, [], [env(a, [href='#'], ['Something else here'])]),
%                    env(li, [class='divider'], []),
%                    env(li, [class='dropdown-header'], ['Nav header']),
%                    env(li, [], [env(a, [href='#'], ['Separated link'])]),
%                    env(li, [], [env(a, [href='#'], ['One more separated link'])]),
%                  end(ul),
%                end(li),

% ---------------------------------------------------------------------------

emit_breadcrumb(Items) -->
    [begin(ol, [class='breadcrumb'])],
    emit_breadcrumb_items(Items),
    [end(ol)].

emit_breadcrumb_items([]) --> [].
emit_breadcrumb_items([X|Xs]) -->
    emit_breadcrumb_item(X),
    emit_breadcrumb_items(Xs).

emit_breadcrumb_item(wui_item(PA, Atts, Text)) -->
    { Xs = ~fiberSusp_to_link(PA, Atts, Text) },
    !,
    { member(active, Atts) -> Atts2 = [class='active'] ; Atts2 = [] },
    [env(li, Atts2, Xs)].
emit_breadcrumb_item(_Query) --> [].

% ---------------------------------------------------------------------------
% Reduced command set for browser
% 
% We provide predicates to translate to JS or <a></a> links (when possible)

% TODO: move somewhere else?
% TODO: add window.open?

jscmd_to_js(set_timeout(Then, Timeout)) --> !,
    "setTimeout(function() { ",
    jscmd_to_js(Then),
    "}, ", 
    { number_codes(Timeout, TimeoutCs) },
    emit(TimeoutCs),
    ")".
%
% Load HTML contents from specified location (HRef), add previous
% location to history.
jscmd_to_js(push_window_location(HRef)) --> !,
    "window.location = \'",
    emit(HRef),
    "\');".
%
% Like set_window_location, but it does not add entries to history.
% TODO: back button still seems working... hmmm
jscmd_to_js(replace_window_location(HRef)) --> !,
    "window.location.replace(\'",
    emit(HRef),
    "\');".
% Query HRef asynchronously
% TODO: merge with ciao-actmod.js model
jscmd_to_js(rpc_GET(HRef)) --> !,
    "var xhr = new XMLHttpRequest();",
    "xhr.open('GET', ",
    "\'", emit(HRef), "\'",
    ", true",
    ");",
    "xhr.onload = function(e) {};",
    "xhr.send(null);".

% Obtain a <a></a> that invokes the given JSCmd on click
jscmd_to_link(JSCmd, HtmlAtts, Body, R) :- JSCmd = push_window_location(HRef), !,
    % Special case which not need JS
    R = env(a, [href=HRef|HtmlAtts], Body).
jscmd_to_link(JSCmd, HtmlAtts, Body, R) :-
    % (otherwise it needs JS)
    jscmd_to_js(JSCmd, JS, []),
    R = env(a, [href="javascript:void(0)", onclick=JS|HtmlAtts], Body).

% ---------------------------------------------------------------------------
% Compile '$fiberSusp' into jscmd

% TODO: where should I move this?

'fiberSusp->jscmd'('$fiberSusp'(FiberData, 'wui_html.redraw'(G))) := JSCmd :- !,
    HRef = ~'fiberSusp->query_str'('$fiberSusp'(FiberData, 'wui_html.dumpHTML'(G))),
    JSCmd = push_window_location(HRef).
'fiberSusp->jscmd'('$fiberSusp'(FiberData, 'wui_html.redraw_nb'(G))) := JSCmd :- !,
    HRef = ~'fiberSusp->query_str'('$fiberSusp'(FiberData, 'wui_html.dumpHTML'(G))),
    JSCmd = replace_window_location(HRef).
'fiberSusp->jscmd'('$fiberSusp'(_, 'wui_html.push_window_location'(HRef))) := JSCmd :- !,
    % TODO: assume that FiberData corresponds to the current actI here
    JSCmd = push_window_location(HRef).
'fiberSusp->jscmd'(PA) := JSCmd :- !,
    HRef = ~'fiberSusp->query_str'(PA),
    JSCmd = rpc_GET(HRef).

% ---------------------------------------------------------------------------

:- doc(section, "Auxiliary term to string conversions").

% TODO: format:format_to_string/3 implementation is not complete
:- use_module(library(format_to_string), [format_to_string/3]).

:- export(term_to_str/2).
term_to_str(X, Str) :-
    format_to_string("~q", [X], Str).

% ---------------------------------------------------------------------------

:- doc(section, "Custom elements").

:- export(glyphicon_env/2).
% A glyphicon
glyphicon_env(Name) := R :-
    atom_concat('glyphicon glyphicon-', Name, Class),
    R = env(span, [class=Class], []).

:- export(label_env/3).
% Kind one of 'default', 'primary', 'success', 'info', 'danger', 'warning'
label_env(Kind, Text) := [env(span, [class=Class], [Text])] :-
    atom_concat('label label-', Kind, Class).

% ---------------------------------------------------------------------------

:- doc(section, "Built-in editor (CodeMirror)").

:- use_module(library(stream_utils), [file_to_string/2]).

% TODO: Fix ciao mode (see mode/ciao/ciao.js and theme/ciao-light.css)
% TODO: Only one editor instance is allowed at the same time

% (See http://codemirror.net/mode/)
% lang_mode(Lang, Script, MIME)
lang_mode('ciao',       'mode/ciao/ciao.js',   'text/x-ciao').
lang_mode('c',          'mode/clike/clike.js', 'text/x-csrc').
lang_mode('c++',        'mode/clike/clike.js', 'text/x-c++src').
lang_mode('java',       'mode/clike/clike.js', 'text/x-java').
lang_mode('javascript', 'mode/javascript/javascript.js', 'text/javascript').
lang_mode('xml',        'mode/xml/xml.js', 'application/xml').
lang_mode('html',       'mode/xml/xml.js', 'text/html').
lang_mode('css',        'mode/css/css.js', 'text/css').
lang_mode('sh',         'mode/shell/shell.js', 'text/sh').

'httpserv.file_path'('', F) :-
    path_concat(~third_party_custom_path(bower_components), 'codemirror', F).

:- export(emit_builtin_edit/4).
emit_builtin_edit(Lang, Path) -->
    { file_to_string(Path, Content) },
    { Lang = text ->
        ModeMIME = ''
    ; lang_mode(Lang, ModeScript, ModeMIME),
      % TODO: make it depend on mode
      ModeStyle = 'theme/ciao-light.css',
      ModeTheme = 'ciao-light'
    },
    %
    env_link('css/ciao-codemirror.css'),
    %
    env_link('lib/codemirror.css'),
    env_link('addon/dialog/dialog.css'),
    env_script('lib/codemirror.js'),
    %
    ( { Lang = text } ->
        []
    ; env_script(ModeScript),
      env_link(ModeStyle)
    ),
    %
    env_script('keymap/emacs.js'),
    env_script('addon/edit/matchbrackets.js'),
    env_script('addon/comment/comment.js'),
    env_script('addon/dialog/dialog.js'),
    env_script('addon/search/searchcursor.js'),
    env_script('addon/search/search.js'),
    [env(style, [type='text/css'], [
      '.CodeMirror {',
%         'height: 100%;',' ',
%         'border-top: 1px solid #eee;',' ',
%         'border-bottom: 1px solid #eee;',
      'border: 1px solid #eee;',
      '}'
    ])],
    [begin(form)],
    [begin(textarea, [id='code', name='code'])],
    [Content],
    [end(textarea)],
    [end(form)],
    { setup_code_mirror(ModeMIME, ModeTheme, Script, []) },
    env_script_src(Script).

setup_code_mirror(ModeMIME, ModeTheme) -->
    ['CodeMirror.commands.save = function() {',
     '  var elt = editor.getWrapperElement();',
     '  elt.style.background = "#def";',
     '  setTimeout(function() { elt.style.background = ""; }, 300);',
     '};'],
    ['var editor = CodeMirror.fromTextArea(document.getElementById("code"), {',
     '  lineNumbers: true,',
     '  matchBrackets: true,',
     '  extraKeys: { "Tab": "indentAuto" },'],
    ( { ModeMIME = '' } ->
        []
    ; ['  mode: "', ModeMIME, '",']
    ),
    ['  keyMap: "emacs",',
     '  theme: "', ModeTheme, '",',
     '  tabSize: 8',
     '});'],
    ['editor.setSize("100%","80%");'].

% ---------------------------------------------------------------------------

% error_html(Xs) -->
%       [begin(div, [class='alert alert-danger', role='alert'])],
%       emit(Xs),
%       [end(div)].

