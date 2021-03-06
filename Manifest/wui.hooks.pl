:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for WUI").

:- use_module(ciaobld(third_party_custom)).

% (hook)
'$builder_hook'(custom_run(fetch_externals, [])) :- !,
    third_party_custom_install(wui).

m_bundle_foreign_dep(wui, cmd, 'node', 'Node (http://nodejs.org)').
m_bundle_foreign_dep(wui, cmd, 'npm', 'NPM (http://www.npmjs.com)').
%
m_bundle_foreign_dep(wui, npm, 'codemirror', 'https://codemirror.net').
%
% m_bundle_foreign_dep(wui, npm, '@popperjs/core', 'https://popper.js.org'). % TODO: requires for bootstrap
% m_bundle_foreign_dep(wui, npm, 'jquery', 'https://jquery.com'). % TODO: avoid this dep
% m_bundle_foreign_dep(wui, npm, 'bootstrap', 'http://getbootstrap.com'). % TODO: avoid this dep
% m_bundle_foreign_dep(wui, npm, 'bootswatch', 'http://bootswatch.com'). % TODO: avoid this dep
% Some web fonts
m_bundle_foreign_dep(wui, npm, 'source-sans-pro', 'https://github.com/adobe-fonts/source-sans-pro').
m_bundle_foreign_dep(wui, npm, 'source-code-pro', 'https://github.com/adobe-fonts/source-code-pro').

% ---------------------------------------------------------------------------
% Prepare site/ for distribution (or file serving)

:- use_module(ciaobld(site_aux)).

% (hook)
'$builder_hook'(custom_run(dist, [])) :- !,
    site_build.

site_build :-
    % TODO: explain: this is similar to linking an executable!
    %   (it puts all resources together)
    site_copy_files,
    site_link_npm_node_modules.

site_copy_files :-
    % TODO: use other paths?
    % TODO: remove 'httpserv.file_path'/2 entries
    site_glob_cp(wui, 'src', '*.js', '/js'),
    site_glob_cp(wui, 'html/js', '*.js', '/js'),
    site_glob_cp(wui, 'html/css', '*.css', '/css'),
    % (for codemirror)
    site_glob_cp(wui, 'html/mode/ciao', '*.js', '/mode/ciao'),
    site_glob_cp(wui, 'html/theme', '*.css', '/theme'),
    % TODO: share
    site_glob_cp(core, 'library/syntax_highlight/css', '*.css', '/css'),
    site_glob_cp(lpdoc, 'etc', '*.css', '/css'),
    % TODO: share
    site_glob_cp(core, 'library/actmod_http', '*.js', '/js').

