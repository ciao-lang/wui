:- bundle(wui).
version('0.1').
depends([
    core
]).
alias_paths([wui = 'src']).
lib('src').
%
manual('wui', [main='doc/SETTINGS.pl']).

