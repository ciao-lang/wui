:- module(_, [], [actmod]). % TODO: .stub.pl should be automatic
:- suspendable(set_buf(const, string)).
:- suspendable(focus_buf(const)).
:- suspendable(buf_mode(const, const)).
:- suspendable(set_html_view(const, string)).
:- suspendable(show_in_progress(json)).
:- suspendable(show_modal(tagval_list)).
:- suspendable(pg_setup(string)).
:- suspendable(set_main_toolbar(json)).

