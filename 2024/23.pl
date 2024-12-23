:- set_prolog_flag(double_quotes, codes).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(ordsets)).

line(A-B) --> [A1, A2], "-", [B1, B2], { atom_codes(A, [A1, A2]), atom_codes(B, [B1, B2]) }.

input_lines([S|Sl]) -->
    line(S),
    eol,
    input_lines(Sl).
input_lines([]) --> eos.

%% part1(Sum) :-
%%     phrase_from_file(input_lines(SlStr), '23testinput'),

%% connected1(kh,tc).
%% connected1(qp,kh).
%% connected1(de,cg).
%% connected1(ka,co).
%% connected1(yn,aq).
%% connected1(qp,ub).
%% connected1(cg,tb).
%% connected1(vc,aq).
%% connected1(tb,ka).
%% connected1(wh,tc).
%% connected1(yn,cg).
%% connected1(kh,ub).
%% connected1(ta,co).
%% connected1(de,co).
%% connected1(tc,td).
%% connected1(tb,wq).
%% connected1(wh,td).
%% connected1(ta,ka).
%% connected1(td,qp).
%% connected1(aq,cg).
%% connected1(wq,ub).
%% connected1(ub,vc).
%% connected1(de,ta).
%% connected1(wq,aq).
%% connected1(wq,vc).
%% connected1(wh,yn).
%% connected1(ka,de).
%% connected1(kh,ta).
%% connected1(co,tc).
%% connected1(wh,qp).
%% connected1(tb,vc).
%% connected1(td,yn).

connected(A, B, Conns) :- member(A-B, Conns).
connected(A, B, Conns) :- member(B-A, Conns).

connected3(A, B, C, Conns) :-
    connected(A, B, Conns),
    connected(B, C, Conns),
    connected(A, C, Conns).

starts_with_t(S) :-
    atom_chars(S, Cs),
    nth0(0, Cs, T),
    T == t.

has_t(A, _, _) :- starts_with_t(A).
has_t(_, B, _) :- starts_with_t(B).
has_t(_, _, C) :- starts_with_t(C).

connected_ts(A, B, C, Conns) :-
    connected3(A, B, C, Conns),
    is_ordset([A, B, C]),
    has_t(A, B, C).

solve_part1(L) :-
    phrase_from_file(input_lines(ConnList), '23input'),
    list_to_ord_set(ConnList, Conns),
    setof([A, B, C], connected_ts(A, B, C, Conns), Ss),
    length(Ss, L).

