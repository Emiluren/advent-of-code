:- set_prolog_flag(double_quotes, codes).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(ordsets)).
:- use_module(library(ugraphs)).

line(A-B) --> [A1, A2], "-", [B1, B2], { atom_codes(A, [A1, A2]), atom_codes(B, [B1, B2]) }.

input_lines([A-B, B-A|Conns], [A,B|Computers]) -->
    line(A-B),
    eol,
    input_lines(Conns, Computers).
input_lines([], []) --> eos.

connected3(S, Ts, Graph) :-
    member(A, Ts),
    neighbors(A, Graph, ANs),
    member(B, ANs),
    neighbors(B, Graph, BNs),
    member(C, ANs),
    member(C, BNs),
    list_to_ord_set([A,B,C], S).

starts_with_t(S) :-
    atom_chars(S, Cs),
    nth0(0, Cs, T),
    T == t.

solve_part1(L) :-
    phrase_from_file(input_lines(ConnList, Computers), '23input'),
    vertices_edges_to_ugraph([], ConnList, Graph),
    include(starts_with_t, Computers, Ts),
    setof(S, connected3(S, Ts, Graph), Ss),
    length(Ss, L).

