:- module(_, [], [doccfg]).

%! \title Configuration for Web-UI manual
%  \author Jose F. Morales

output_name := 'wui'.
doc_structure := 'wui_html'-[ % TODO: add a manual
    % wui,
    % wui_html,
    ciao_playground,
    logcmd,
    % simple_menu_playground,
    playground_aux,
    timestamp_date
  ].

% TODO: port this manual
allow_markdown := no.
syntax_highlight := no.
