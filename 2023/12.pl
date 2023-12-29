:- set_prolog_flag(double_quotes, codes).
[library(dcg/basics)].
[library(dcg/high_order)].
[library(pio)].

spring(#) --> "#".
spring(.) --> ".".
spring(_) --> "?".

line(Springs, Numbers) --> sequence(spring, Springs), " ", sequence(integer, ",", Numbers).

input_lines([S|Sl], [N|Nl]) -->
    line(S, N),
    eol,
    input_lines(Sl, Nl).
input_lines([],[]) --> eos.

%% check_line, force_space, add_space and check_part are stolen from
%% https://gist.github.com/klemens/b61f141ade46da56fc7d
check_line([], []) :- !.
check_line(Line, [Part|Rest]) :-
    Rest \= [],
    add_space(Line, Line2),
    check_part(Line2, Line3, Part),
    force_space(Line3, Line4),
    check_line(Line4, Rest).
check_line(Line, [Part|[]]) :-
    add_space(Line, Line2),
    check_part(Line2, Line3, Part),
    add_space(Line3, Line4),
    check_line(Line4, []).

force_space(['.'|Line], Line).

add_space(Line, Line).
add_space(['.'|Line], RestLine) :-
    add_space(Line, RestLine).

check_part(Line, Line, 0).
check_part(['#'|Line], RestLine, N) :-
    N > 0,
    N1 is N - 1,
    check_part(Line, RestLine, N1).

line_solution_count(S, N, SolCount) :-
    aggregate_all(count, check_line(S, N), SolCount).

part1(Sum) :-
    phrase_from_file(input_lines(Sl, Nl), '12input'),
    maplist(line_solution_count, Sl, Nl, Counts),
    sum_list(Counts, Sum).
