:- module(timestamp_date, [], [dcg]).

:- use_module(library(lists), [append/3]).
:- use_module(library(system), [datime/9]).

:- export(timestamp_atom/1).
% Obtain a timestamp atom from date in YYYYMMDDHHMMSS format
timestamp_atom(Timestamp) :-
	datime(_, Year, Month, Day, Hour, Min, Sec, _WeekDay, _YearDay),
	timestamp_str(Year, Month, Day, Hour, Min, Sec, Str, []),
	atom_codes(Timestamp, Str).

timestamp_str(Year, Month, Day, Hour, Min, Sec) -->
	emit_num(Year),
	emit_num2(Month),
	emit_num2(Day),
	emit_num2(Hour),
	emit_num2(Min),
	emit_num2(Sec).

emit_num2(X) --> { X < 10 }, !, "0", emit_num(X).
emit_num2(X) --> emit_num(X).

emit_num(X) --> { number_codes(X, Str) }, emit_string(Str).

emit_string(Str, Xs, Xs0) :- append(Str, Xs0, Xs).

