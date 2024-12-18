:- use_module(library(simplex)).
:- set_prolog_flag(double_quotes, chars).

buttons(S) :-
    gen_state(S0),
    post_constraints(S0, S1),
    minimize([3*a, b], S1, S).

post_constraints -->
    constraint([94*a, 22*b] = 10000000008400),
    constraint([34*a, 67*b] = 10000000005400),
    constraint([a] >= 0),
    constraint([b] >= 0).
