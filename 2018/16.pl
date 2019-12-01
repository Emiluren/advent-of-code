:- use_module(library(clpfd)).

op(addr, Regs, A, B, Cval) :-
    nth0(A, Regs, Aval),
    nth0(B, Regs, Bval),
    Cval #= Aval + Bval.

op(addi, Regs, A, Bval, Cval) :-
    nth0(A, Regs, Aval),
    Cval #= Aval + Bval.

op(mulr, Regs, A, B, Cval) :-
    nth0(A, Regs, Aval),
    nth0(B, Regs, Bval),
    Cval #= Aval * Bval.

op(muli, Regs, A, Bval, Cval) :-
    nth0(A, Regs, Aval),
    Cval #= Aval * Bval.

op(banr, Regs, A, B, Cval) :-
    nth0(A, Regs, Aval),
    nth0(B, Regs, Bval),
    Cval #= Aval /\ Bval.

op(bani, Regs, A, Bval, Cval) :-
    nth0(A, Regs, Aval),
    Cval #= Aval /\ Bval.

op(borr, Regs, A, B, Cval) :-
    nth0(A, Regs, Aval),
    nth0(B, Regs, Bval),
    Cval #= Aval \/ Bval.

op(bori, Regs, A, Bval, Cval) :-
    nth0(A, Regs, Aval),
    Cval #= Aval \/ Bval.

op(setr, Regs, A, _, Cval) :-
    nth0(A, Regs, Cval).

op(seti, _, Aval, _, Aval).

op(gtir, Regs, Aval, B, 1) :-
    nth0(B, Regs, Bval),
    Aval > Bval.
op(gtir, Regs, Aval, B, 0) :-
    nth0(B, Regs, Bval),
    Aval =< Bval.

op(gtri, Regs, A, Bval, Cval) :-
    op(gtir, Regs, Bval, A, Cval).

op(gtrr, Regs, A, B, Cval) :-
    nth0(A, Regs, Aval),
    op(gtir, Regs, Aval, B, Cval).

op(eqir, Regs, Aval, B, 1) :-
    nth0(B, Regs, Aval).
op(eqir, Regs, Aval, B, 0) :-
    nth0(B, Regs, Bval),
    Aval \= Bval.

op(eqri, Regs, A, Bval, Cval) :-
    op(eqir, Regs, Bval, A, Cval).

op(eqrr, Regs, A, B, Cval) :-
    nth0(A, Regs, Aval),
    op(eqir, Regs, Aval, B, Cval).

replaceNth([_|Xs], 0, Y, [Y|Xs]).
replaceNth([X|Xs], N, Y, [X|Ys]) :-
    NMinusOne #= N - 1,
    replaceNth(Xs, NMinusOne, Y, Ys).

applyOp(Op, Regs, A, B, C, NewRegs) :-
    op(Op, Regs, A, B, Cval),
    replaceNth(Regs, C, Cval, NewRegs).

countPossibilities(example(before(Before), instr(_, A, B, C), after(After)), Count) :-
    aggregate_all(
        count,
        (applyOp(_Op, Before, A, B, C, After)),
        Count
    ).

%% Does not work (Infinite loop)
moreThanOrThree(Example) :-
    countPossibilities(Example, Count),
    Count #>= 3.

example(before([2, 3, 2, 2]), instr(15, 3, 2, 2), after([2, 3, 4, 2])).

example(before([3, 2, 2, 1]), instr(3, 1, 0, 1), after([3, 1, 2, 1])).

example(before([3, 3, 2, 1]), instr(5, 3, 2, 1), after([3, 3, 2, 1])).

example(before([0, 1, 2, 2]), instr(10, 1, 0), after([0, 1, 2, 2])).

example(before([0, 1, 2, 1]), instr(8, 0, 0, 3), after([0, 1, 2, 0])).

example(before([2, 3, 0, 3]), instr(11, 0, 3), after([2, 3, 0, 0])).

example(before([2, 3, 1, 0]), instr(0, 0, 2, 3), after([2, 3, 1, 4])).

example(before([2, 0, 1, 1]), instr(7, 2, 1, 2), after([2, 0, 1, 1])).

example(before([1, 3, 3, 1]), instr(6, 0, 2, 0), after([2, 3, 3, 1])).

example(before([1, 2, 2, 1]), instr(5, 3, 2, 3), after([1, 2, 2, 3])).

example(before([1, 0, 1, 2]), instr(13, 1, 0), after([1, 0, 1, 1])).

example(before([1, 2, 3, 0]), instr(6, 1, 3, 1), after([1, 6, 3, 0])).

example(before([1, 0, 0, 3]), instr(11, 0, 3), after([0, 0, 0, 3])).

example(before([0, 3, 2, 1]), instr(5, 3, 2, 2), after([0, 3, 3, 1])).

example(before([2, 0, 0, 0]), instr(2, 3, 0, 3), after([2, 0, 0, 2])).

example(before([1, 0, 2, 1]), instr(15, 2, 2), after([4, 0, 2, 1])).

example(before([0, 1, 2, 3]), instr(4, 3, 2, 3), after([0, 1, 2, 5])).

example(before([1, 0, 0, 2]), instr(13, 1, 0), after([1, 0, 0, 2])).

example(before([3, 1, 2, 1]), instr(5, 3, 2, 0), after([3, 1, 2, 1])).

example(before([1, 1, 3, 0]), instr(12, 0, 2), after([1, 1, 3, 0])).

example(before([1, 0, 2, 1]), instr(13, 1, 0), after([1, 0, 1, 1])).

example(before([2, 2, 3, 1]), instr(6, 2, 3, 2), after([2, 2, 9, 1])).

example(before([2, 1, 2, 3]), instr(4, 1, 1, 0), after([2, 1, 2, 3])).

example(before([1, 1, 0, 1]), instr(6, 3, 2, 1), after([1, 2, 0, 1])).

example(before([3, 2, 2, 3]), instr(3, 1, 0, 3), after([3, 2, 2, 1])).

example(before([0, 1, 2, 2]), instr(8, 0, 0, 3), after([0, 1, 2, 0])).

example(before([3, 0, 1, 0]), instr(2, 1, 0, 0), after([3, 0, 1, 0])).

example(before([3, 2, 3, 3]), instr(3, 1, 0, 0), after([1, 2, 3, 3])).

example(before([0, 1, 0, 3]), instr(12, 0, 3), after([0, 1, 3, 3])).

example(before([0, 1, 2, 1]), instr(10, 1, 0), after([1, 1, 2, 1])).

example(before([2, 1, 1, 2]), instr(12, 2, 3), after([2, 1, 1, 3])).

example(before([1, 0, 2, 2]), instr(7, 0, 1, 3), after([1, 0, 2, 1])).

example(before([2, 1, 3, 2]), instr(5, 3, 1, 1), after([2, 3, 3, 2])).

example(before([1, 0, 3, 2]), instr(13, 1, 0), after([1, 0, 3, 1])).

example(before([3, 2, 2, 0]), instr(3, 1, 0, 2), after([3, 2, 1, 0])).

example(before([2, 3, 3, 3]), instr(11, 0, 3), after([2, 3, 3, 0])).

example(before([0, 1, 3, 3]), instr(10, 1, 0), after([0, 1, 1, 3])).

example(before([1, 1, 3, 3]), instr(4, 0, 3, 1), after([1, 4, 3, 3])).

example(before([1, 0, 1, 0]), instr(7, 0, 1, 1), after([1, 1, 1, 0])).

example(before([3, 0, 1, 3]), instr(14, 1, 2), after([3, 1, 1, 3])).

example(before([1, 1, 1, 2]), instr(4, 0, 1, 3), after([1, 1, 1, 2])).

example(before([0, 1, 2, 1]), instr(15, 2, 2), after([0, 1, 2, 4])).

example(before([0, 3, 3, 3]), instr(8, 0, 0, 2), after([0, 3, 0, 3])).

example(before([0, 0, 1, 2]), instr(1, 0, 1, 0), after([1, 0, 1, 2])).

example(before([3, 1, 1, 1]), instr(4, 3, 1, 2), after([3, 1, 2, 1])).

example(before([3, 2, 3, 2]), instr(3, 1, 0, 2), after([3, 2, 1, 2])).

example(before([1, 1, 3, 2]), instr(12, 0, 3), after([1, 1, 3, 3])).

example(before([1, 2, 2, 2]), instr(15, 1, 2), after([1, 2, 4, 2])).

example(before([2, 0, 1, 2]), instr(14, 1, 2), after([2, 0, 1, 1])).

example(before([1, 0, 2, 3]), instr(7, 0, 1, 3), after([1, 0, 2, 1])).

example(before([1, 0, 2, 2]), instr(2, 2, 0, 2), after([1, 0, 3, 2])).

example(before([1, 0, 0, 0]), instr(13, 1, 0), after([1, 0, 0, 1])).

example(before([1, 0, 1, 0]), instr(7, 0, 1, 0), after([1, 0, 1, 0])).

example(before([3, 0, 0, 1]), instr(2, 2, 0, 0), after([3, 0, 0, 1])).

example(before([0, 2, 3, 0]), instr(8, 0, 0, 3), after([0, 2, 3, 0])).

example(before([0, 1, 1, 0]), instr(4, 1, 2, 0), after([2, 1, 1, 0])).

example(before([0, 2, 3, 0]), instr(0, 1, 2, 1), after([0, 4, 3, 0])).

example(before([3, 2, 1, 3]), instr(3, 1, 0, 0), after([1, 2, 1, 3])).

example(before([0, 2, 3, 0]), instr(2, 3, 1, 3), after([0, 2, 3, 2])).

example(before([3, 2, 0, 2]), instr(0, 1, 2, 3), after([3, 2, 0, 4])).

example(before([0, 0, 1, 3]), instr(8, 0, 0, 2), after([0, 0, 0, 3])).

example(before([1, 0, 1, 0]), instr(7, 2, 1, 0), after([1, 0, 1, 0])).

example(before([1, 3, 1, 2]), instr(12, 2, 3), after([3, 3, 1, 2])).

example(before([2, 3, 3, 1]), instr(5, 3, 2, 1), after([2, 3, 3, 1])).

example(before([2, 3, 1, 0]), instr(0, 0, 2, 1), after([2, 4, 1, 0])).

example(before([1, 2, 2, 1]), instr(15, 2, 2), after([1, 2, 2, 4])).

example(before([1, 1, 1, 3]), instr(11, 0, 3), after([1, 1, 1, 0])).

example(before([3, 2, 3, 1]), instr(5, 3, 2, 0), after([3, 2, 3, 1])).

example(before([2, 2, 1, 2]), instr(0, 3, 2, 1), after([2, 4, 1, 2])).

example(before([1, 1, 3, 3]), instr(12, 1, 3), after([1, 1, 3, 3])).

example(before([3, 0, 3, 2]), instr(4, 0, 2, 3), after([3, 0, 3, 6])).

example(before([3, 2, 2, 2]), instr(3, 1, 0, 3), after([3, 2, 2, 1])).

example(before([2, 1, 3, 2]), instr(0, 0, 2, 3), after([2, 1, 3, 4])).

example(before([0, 3, 1, 1]), instr(8, 0, 0, 2), after([0, 3, 0, 1])).

example(before([1, 0, 0, 1]), instr(13, 1, 0), after([1, 0, 0, 1])).

example(before([3, 0, 2, 2]), instr(15, 2, 2), after([3, 4, 2, 2])).

example(before([0, 0, 1, 1]), instr(7, 3, 1, 0), after([1, 0, 1, 1])).

example(before([2, 2, 2, 3]), instr(4, 1, 3, 1), after([2, 5, 2, 3])).

example(before([1, 0, 2, 1]), instr(13, 1, 0), after([1, 1, 2, 1])).

example(before([0, 0, 1, 1]), instr(14, 1, 2), after([0, 0, 1, 1])).

example(before([2, 3, 1, 3]), instr(4, 1, 3, 1), after([2, 6, 1, 3])).

example(before([2, 2, 3, 2]), instr(0, 0, 2, 0), after([4, 2, 3, 2])).

example(before([1, 0, 3, 3]), instr(11, 0, 3), after([1, 0, 0, 3])).

example(before([3, 3, 2, 1]), instr(5, 3, 2, 2), after([3, 3, 3, 1])).

example(before([2, 3, 2, 1]), instr(5, 3, 2, 0), after([3, 3, 2, 1])).

example(before([3, 2, 3, 1]), instr(3, 1, 0, 1), after([3, 1, 3, 1])).

example(before([1, 1, 2, 0]), instr(15, 2, 2), after([4, 1, 2, 0])).

example(before([0, 1, 0, 3]), instr(10, 1, 0), after([0, 1, 0, 1])).

example(before([3, 0, 1, 2]), instr(0, 3, 2, 2), after([3, 0, 4, 2])).

example(before([1, 2, 1, 3]), instr(9, 1, 0, 1), after([1, 1, 1, 3])).

example(before([2, 0, 3, 3]), instr(11, 0, 3), after([0, 0, 3, 3])).

example(before([0, 0, 1, 3]), instr(1, 0, 1, 1), after([0, 1, 1, 3])).

example(before([1, 3, 3, 3]), instr(4, 1, 3, 0), after([6, 3, 3, 3])).

example(before([1, 2, 2, 2]), instr(15, 3, 2), after([4, 2, 2, 2])).

example(before([2, 0, 0, 1]), instr(14, 1, 3), after([1, 0, 0, 1])).

example(before([0, 0, 0, 3]), instr(1, 0, 1, 0), after([1, 0, 0, 3])).

example(before([1, 2, 3, 3]), instr(12, 0, 3), after([3, 2, 3, 3])).

example(before([1, 0, 2, 3]), instr(13, 1, 0), after([1, 1, 2, 3])).

example(before([2, 1, 3, 3]), instr(2, 1, 0, 2), after([2, 1, 3, 3])).

example(before([0, 1, 1, 0]), instr(10, 1, 0), after([1, 1, 1, 0])).

example(before([1, 0, 2, 1]), instr(7, 0, 1, 3), after([1, 0, 2, 1])).

example(before([0, 1, 0, 2]), instr(8, 0, 0, 0), after([0, 1, 0, 2])).

example(before([3, 0, 0, 1]), instr(14, 1, 3), after([3, 1, 0, 1])).

example(before([2, 1, 1, 1]), instr(12, 0, 1), after([2, 1, 1, 3])).

example(before([0, 0, 3, 1]), instr(8, 0, 0, 1), after([0, 0, 3, 1])).

example(before([3, 2, 2, 1]), instr(3, 1, 0, 0), after([1, 2, 2, 1])).

example(before([2, 1, 2, 3]), instr(11, 0, 3), after([2, 1, 2, 0])).

example(before([3, 2, 2, 0]), instr(3, 1, 0, 0), after([1, 2, 2, 0])).

example(before([1, 2, 0, 3]), instr(12, 2, 3), after([1, 2, 3, 3])).

example(before([1, 2, 1, 2]), instr(12, 0, 3), after([3, 2, 1, 2])).

example(before([1, 0, 3, 0]), instr(7, 0, 1, 0), after([1, 0, 3, 0])).

example(before([3, 2, 2, 2]), instr(3, 1, 0, 2), after([3, 2, 1, 2])).

example(before([0, 1, 3, 1]), instr(5, 3, 2, 2), after([0, 1, 3, 1])).

example(before([1, 3, 1, 1]), instr(2, 2, 1, 3), after([1, 3, 1, 3])).

example(before([0, 0, 3, 1]), instr(14, 1, 3), after([0, 1, 3, 1])).

example(before([3, 2, 1, 1]), instr(3, 1, 0, 3), after([3, 2, 1, 1])).

example(before([1, 0, 3, 1]), instr(13, 1, 0), after([1, 0, 1, 1])).

example(before([2, 2, 1, 0]), instr(0, 0, 2, 2), after([2, 2, 4, 0])).

example(before([0, 0, 1, 1]), instr(14, 1, 2), after([1, 0, 1, 1])).

example(before([3, 0, 0, 1]), instr(7, 3, 1, 1), after([3, 1, 0, 1])).

example(before([0, 2, 3, 3]), instr(8, 0, 0, 0), after([0, 2, 3, 3])).

example(before([3, 2, 2, 0]), instr(15, 1, 2), after([4, 2, 2, 0])).

example(before([3, 2, 0, 1]), instr(3, 1, 0, 3), after([3, 2, 0, 1])).

example(before([3, 0, 1, 3]), instr(14, 1, 2), after([1, 0, 1, 3])).

example(before([2, 0, 2, 3]), instr(11, 0, 3), after([2, 0, 0, 3])).

example(before([2, 1, 1, 2]), instr(0, 3, 2, 2), after([2, 1, 4, 2])).

example(before([2, 0, 1, 2]), instr(14, 1, 2), after([1, 0, 1, 2])).

example(before([1, 2, 0, 0]), instr(9, 1, 0, 2), after([1, 2, 1, 0])).

example(before([1, 2, 3, 1]), instr(9, 1, 0, 0), after([1, 2, 3, 1])).

example(before([0, 3, 3, 2]), instr(2, 0, 1, 3), after([0, 3, 3, 3])).

example(before([1, 3, 1, 3]), instr(11, 0, 3), after([0, 3, 1, 3])).

example(before([3, 2, 3, 0]), instr(3, 1, 0, 2), after([3, 2, 1, 0])).

example(before([2, 3, 2, 3]), instr(4, 1, 3, 1), after([2, 6, 2, 3])).

example(before([2, 1, 3, 3]), instr(12, 0, 1), after([2, 1, 3, 3])).

example(before([1, 0, 1, 2]), instr(13, 1, 0), after([1, 1, 1, 2])).

example(before([0, 0, 0, 1]), instr(8, 0, 0, 2), after([0, 0, 0, 1])).

example(before([1, 0, 3, 0]), instr(13, 1, 0), after([1, 0, 3, 0])).

example(before([2, 1, 2, 1]), instr(5, 3, 2, 1), after([2, 3, 2, 1])).

example(before([1, 0, 3, 2]), instr(7, 0, 1, 3), after([1, 0, 3, 1])).

example(before([0, 3, 2, 3]), instr(8, 0, 0, 1), after([0, 0, 2, 3])).

example(before([0, 0, 1, 1]), instr(1, 0, 1, 0), after([1, 0, 1, 1])).

example(before([0, 0, 1, 0]), instr(1, 0, 1, 0), after([1, 0, 1, 0])).

example(before([3, 2, 3, 3]), instr(3, 1, 0, 2), after([3, 2, 1, 3])).

example(before([2, 1, 3, 3]), instr(4, 1, 1, 1), after([2, 2, 3, 3])).

example(before([3, 1, 3, 2]), instr(6, 0, 3, 3), after([3, 1, 3, 9])).

example(before([0, 1, 0, 1]), instr(10, 1, 0), after([1, 1, 0, 1])).

example(before([2, 1, 0, 2]), instr(5, 3, 1, 1), after([2, 3, 0, 2])).

example(before([0, 3, 0, 1]), instr(8, 0, 0, 3), after([0, 3, 0, 0])).

example(before([0, 1, 3, 3]), instr(10, 1, 0), after([0, 1, 3, 3])).

example(before([3, 2, 2, 3]), instr(3, 1, 0, 2), after([3, 2, 1, 3])).

example(before([2, 1, 0, 1]), instr(2, 1, 0, 3), after([2, 1, 0, 3])).

example(before([1, 2, 3, 3]), instr(9, 1, 0, 1), after([1, 1, 3, 3])).

example(before([0, 1, 1, 1]), instr(10, 1, 0), after([1, 1, 1, 1])).

example(before([3, 0, 2, 3]), instr(12, 1, 3), after([3, 0, 2, 3])).

example(before([0, 1, 0, 3]), instr(10, 1, 0), after([0, 1, 1, 3])).

example(before([1, 0, 1, 0]), instr(13, 1, 0), after([1, 0, 1, 0])).

example(before([0, 0, 2, 1]), instr(8, 0, 0, 0), after([0, 0, 2, 1])).

example(before([1, 1, 2, 3]), instr(11, 0, 3), after([0, 1, 2, 3])).

example(before([1, 1, 0, 2]), instr(5, 3, 1, 2), after([1, 1, 3, 2])).

example(before([0, 3, 2, 1]), instr(5, 3, 2, 1), after([0, 3, 2, 1])).

example(before([2, 2, 1, 3]), instr(4, 0, 3, 2), after([2, 2, 5, 3])).

example(before([0, 1, 3, 1]), instr(10, 1, 0), after([0, 1, 3, 1])).

example(before([0, 1, 0, 2]), instr(10, 1, 0), after([0, 1, 0, 1])).

example(before([2, 0, 2, 1]), instr(14, 1, 3), after([2, 0, 1, 1])).

example(before([2, 1, 0, 3]), instr(11, 0, 3), after([0, 1, 0, 3])).

example(before([2, 2, 2, 1]), instr(15, 2, 2), after([4, 2, 2, 1])).

example(before([0, 2, 1, 2]), instr(0, 1, 2, 0), after([4, 2, 1, 2])).

example(before([1, 0, 2, 1]), instr(14, 1, 3), after([1, 0, 1, 1])).

example(before([1, 0, 2, 1]), instr(13, 1, 0), after([1, 0, 2, 1])).

example(before([2, 3, 1, 3]), instr(2, 0, 2, 1), after([2, 3, 1, 3])).

example(before([0, 3, 0, 2]), instr(2, 0, 3, 2), after([0, 3, 2, 2])).

example(before([1, 2, 1, 3]), instr(12, 2, 3), after([1, 3, 1, 3])).

example(before([1, 0, 2, 1]), instr(13, 1, 0), after([1, 0, 2, 1])).

example(before([1, 0, 2, 2]), instr(7, 0, 1, 1), after([1, 1, 2, 2])).

example(before([1, 0, 1, 1]), instr(13, 1, 0), after([1, 1, 1, 1])).

example(before([1, 1, 2, 1]), instr(12, 0, 2), after([1, 1, 2, 3])).

example(before([1, 1, 0, 3]), instr(4, 3, 3, 1), after([1, 6, 0, 3])).

example(before([1, 3, 3, 3]), instr(11, 0, 3), after([0, 3, 3, 3])).

example(before([0, 2, 2, 1]), instr(15, 2, 2), after([0, 4, 2, 1])).

example(before([2, 1, 2, 3]), instr(11, 0, 3), after([0, 1, 2, 3])).

example(before([2, 2, 1, 3]), instr(4, 3, 3, 3), after([2, 2, 1, 6])).

example(before([3, 2, 2, 2]), instr(15, 2, 2), after([4, 2, 2, 2])).

example(before([1, 1, 3, 3]), instr(4, 2, 3, 0), after([6, 1, 3, 3])).

example(before([0, 2, 3, 1]), instr(0, 1, 2, 0), after([4, 2, 3, 1])).

example(before([0, 2, 0, 2]), instr(0, 3, 2, 2), after([0, 2, 4, 2])).

example(before([1, 0, 0, 2]), instr(13, 1, 0), after([1, 0, 0, 1])).

example(before([3, 1, 1, 2]), instr(5, 3, 1, 0), after([3, 1, 1, 2])).

example(before([2, 2, 1, 1]), instr(2, 2, 0, 2), after([2, 2, 3, 1])).

example(before([1, 3, 0, 3]), instr(11, 0, 3), after([1, 3, 0, 0])).

example(before([1, 1, 3, 2]), instr(5, 3, 1, 2), after([1, 1, 3, 2])).

example(before([0, 2, 0, 1]), instr(2, 0, 3, 1), after([0, 1, 0, 1])).

example(before([0, 0, 3, 0]), instr(2, 0, 2, 1), after([0, 3, 3, 0])).

example(before([3, 0, 3, 1]), instr(14, 1, 3), after([3, 1, 3, 1])).

example(before([0, 3, 2, 0]), instr(8, 0, 0, 0), after([0, 3, 2, 0])).

example(before([0, 3, 2, 3]), instr(8, 0, 0, 2), after([0, 3, 0, 3])).

example(before([0, 1, 3, 3]), instr(10, 1, 0), after([0, 1, 3, 1])).

example(before([0, 0, 0, 0]), instr(1, 0, 1, 3), after([0, 0, 0, 1])).

example(before([0, 1, 3, 1]), instr(10, 1, 0), after([0, 1, 3, 1])).

example(before([0, 0, 2, 1]), instr(14, 1, 3), after([1, 0, 2, 1])).

example(before([2, 2, 1, 0]), instr(2, 1, 2, 1), after([2, 3, 1, 0])).

example(before([1, 0, 0, 3]), instr(11, 0, 3), after([1, 0, 0, 3])).

example(before([0, 1, 3, 0]), instr(10, 1, 0), after([0, 1, 3, 0])).

example(before([3, 3, 1, 1]), instr(2, 2, 1, 0), after([3, 3, 1, 1])).

example(before([0, 2, 1, 0]), instr(2, 2, 1, 0), after([3, 2, 1, 0])).

example(before([2, 1, 2, 3]), instr(12, 1, 3), after([2, 3, 2, 3])).

example(before([0, 0, 1, 2]), instr(8, 0, 0, 0), after([0, 0, 1, 2])).

example(before([2, 3, 2, 0]), instr(6, 2, 3, 1), after([2, 6, 2, 0])).

example(before([3, 0, 2, 1]), instr(5, 3, 2, 1), after([3, 3, 2, 1])).

example(before([2, 1, 1, 3]), instr(2, 2, 0, 1), after([2, 3, 1, 3])).

example(before([2, 2, 0, 0]), instr(2, 3, 0, 2), after([2, 2, 2, 0])).

example(before([3, 2, 3, 0]), instr(3, 1, 0, 3), after([3, 2, 3, 1])).

example(before([0, 1, 3, 2]), instr(10, 1, 0), after([0, 1, 1, 2])).

example(before([1, 2, 3, 0]), instr(2, 0, 1, 2), after([1, 2, 3, 0])).

example(before([1, 3, 2, 3]), instr(11, 0, 3), after([1, 3, 0, 3])).

example(before([0, 0, 2, 1]), instr(15, 2, 2), after([0, 4, 2, 1])).

example(before([3, 2, 0, 0]), instr(3, 1, 0, 1), after([3, 1, 0, 0])).

example(before([1, 0, 0, 1]), instr(14, 1, 3), after([1, 1, 0, 1])).

example(before([2, 0, 1, 3]), instr(7, 2, 1, 2), after([2, 0, 1, 3])).

example(before([1, 0, 0, 0]), instr(13, 1, 0), after([1, 0, 1, 0])).

example(before([1, 0, 1, 1]), instr(13, 1, 0), after([1, 0, 1, 1])).

example(before([1, 0, 2, 0]), instr(13, 1, 0), after([1, 0, 1, 0])).

example(before([3, 1, 1, 2]), instr(12, 1, 3), after([3, 1, 1, 3])).

example(before([0, 1, 1, 0]), instr(12, 0, 1), after([1, 1, 1, 0])).

example(before([1, 0, 3, 2]), instr(13, 1, 0), after([1, 0, 3, 2])).

example(before([2, 2, 1, 3]), instr(11, 0, 3), after([2, 0, 1, 3])).

example(before([0, 0, 2, 3]), instr(1, 0, 1, 0), after([1, 0, 2, 3])).

example(before([1, 2, 0, 3]), instr(11, 0, 3), after([1, 2, 0, 3])).

example(before([1, 0, 3, 1]), instr(7, 3, 1, 0), after([1, 0, 3, 1])).

example(before([1, 0, 0, 1]), instr(13, 1, 0), after([1, 1, 0, 1])).

example(before([1, 2, 3, 0]), instr(9, 1, 0, 2), after([1, 2, 1, 0])).

example(before([1, 1, 3, 3]), instr(11, 0, 3), after([1, 1, 0, 3])).

example(before([2, 3, 2, 3]), instr(11, 0, 3), after([2, 0, 2, 3])).

example(before([0, 1, 2, 2]), instr(10, 1, 0), after([0, 1, 1, 2])).

example(before([2, 0, 1, 2]), instr(2, 0, 2, 2), after([2, 0, 3, 2])).

example(before([1, 2, 1, 1]), instr(9, 1, 0, 1), after([1, 1, 1, 1])).

example(before([0, 0, 2, 0]), instr(12, 1, 2), after([2, 0, 2, 0])).

example(before([3, 1, 1, 1]), instr(6, 0, 3, 3), after([3, 1, 1, 9])).

example(before([1, 1, 2, 0]), instr(2, 2, 0, 2), after([1, 1, 3, 0])).

example(before([0, 0, 1, 3]), instr(8, 0, 0, 1), after([0, 0, 1, 3])).

example(before([2, 1, 2, 2]), instr(15, 3, 2), after([4, 1, 2, 2])).

example(before([3, 1, 0, 2]), instr(12, 1, 3), after([3, 3, 0, 2])).

example(before([0, 0, 1, 1]), instr(14, 1, 2), after([0, 1, 1, 1])).

example(before([1, 2, 3, 3]), instr(2, 1, 0, 2), after([1, 2, 3, 3])).

example(before([0, 3, 2, 3]), instr(15, 2, 2), after([4, 3, 2, 3])).

example(before([2, 0, 0, 2]), instr(2, 1, 3, 0), after([2, 0, 0, 2])).

example(before([2, 2, 1, 0]), instr(6, 0, 3, 2), after([2, 2, 6, 0])).

example(before([0, 1, 1, 2]), instr(5, 3, 1, 0), after([3, 1, 1, 2])).

example(before([2, 3, 3, 3]), instr(4, 1, 3, 3), after([2, 3, 3, 6])).

example(before([0, 1, 1, 0]), instr(10, 1, 0), after([0, 1, 1, 1])).

example(before([0, 0, 2, 1]), instr(14, 1, 3), after([0, 0, 1, 1])).

example(before([2, 0, 1, 2]), instr(0, 0, 2, 0), after([4, 0, 1, 2])).

example(before([3, 2, 1, 0]), instr(3, 1, 0, 0), after([1, 2, 1, 0])).

example(before([0, 1, 2, 0]), instr(10, 1, 0), after([0, 1, 2, 0])).

example(before([1, 1, 3, 3]), instr(12, 1, 2), after([1, 3, 3, 3])).

example(before([2, 0, 2, 0]), instr(15, 2, 2), after([2, 0, 2, 4])).

example(before([1, 2, 3, 1]), instr(5, 3, 2, 2), after([1, 2, 3, 1])).

example(before([1, 0, 3, 1]), instr(14, 1, 3), after([1, 0, 1, 1])).

example(before([1, 2, 3, 2]), instr(0, 3, 2, 0), after([4, 2, 3, 2])).

example(before([0, 1, 2, 2]), instr(10, 1, 0), after([0, 1, 2, 1])).

example(before([1, 2, 0, 2]), instr(9, 1, 0, 3), after([1, 2, 0, 1])).

example(before([3, 2, 2, 2]), instr(3, 1, 0, 1), after([3, 1, 2, 2])).

example(before([3, 3, 2, 2]), instr(15, 3, 2), after([3, 4, 2, 2])).

example(before([2, 0, 2, 3]), instr(11, 0, 3), after([0, 0, 2, 3])).

example(before([0, 0, 2, 3]), instr(1, 0, 1, 3), after([0, 0, 2, 1])).

example(before([2, 0, 2, 1]), instr(7, 3, 1, 2), after([2, 0, 1, 1])).

example(before([0, 0, 2, 0]), instr(1, 0, 1, 2), after([0, 0, 1, 0])).

example(before([2, 2, 0, 3]), instr(11, 0, 3), after([2, 0, 0, 3])).

example(before([1, 0, 1, 0]), instr(14, 1, 2), after([1, 0, 1, 1])).

example(before([0, 0, 1, 3]), instr(7, 2, 1, 3), after([0, 0, 1, 1])).

example(before([3, 2, 1, 3]), instr(4, 1, 3, 1), after([3, 5, 1, 3])).

example(before([1, 0, 2, 3]), instr(11, 0, 3), after([0, 0, 2, 3])).

example(before([1, 0, 0, 0]), instr(13, 1, 0), after([1, 1, 0, 0])).

example(before([1, 2, 0, 3]), instr(0, 1, 2, 0), after([4, 2, 0, 3])).

example(before([3, 3, 3, 0]), instr(6, 2, 3, 1), after([3, 9, 3, 0])).

example(before([1, 1, 2, 1]), instr(15, 2, 2), after([1, 1, 4, 1])).

example(before([2, 1, 0, 0]), instr(6, 1, 2, 3), after([2, 1, 0, 2])).

example(before([2, 0, 0, 3]), instr(11, 0, 3), after([2, 0, 0, 3])).

example(before([0, 3, 1, 3]), instr(12, 2, 3), after([3, 3, 1, 3])).

example(before([2, 0, 1, 1]), instr(7, 2, 1, 1), after([2, 1, 1, 1])).

example(before([1, 2, 2, 3]), instr(9, 1, 0, 3), after([1, 2, 2, 1])).

example(before([3, 0, 2, 0]), instr(6, 2, 3, 3), after([3, 0, 2, 6])).

example(before([3, 0, 0, 2]), instr(2, 2, 0, 0), after([3, 0, 0, 2])).

example(before([3, 2, 0, 2]), instr(3, 1, 0, 2), after([3, 2, 1, 2])).

example(before([0, 1, 1, 0]), instr(10, 1, 0), after([0, 1, 1, 0])).

example(before([0, 0, 0, 2]), instr(1, 0, 1, 0), after([1, 0, 0, 2])).

example(before([0, 2, 1, 1]), instr(2, 2, 1, 1), after([0, 3, 1, 1])).

example(before([0, 0, 3, 1]), instr(8, 0, 0, 3), after([0, 0, 3, 0])).

example(before([0, 1, 2, 0]), instr(10, 1, 0), after([0, 1, 2, 1])).

example(before([3, 2, 1, 0]), instr(3, 1, 0, 1), after([3, 1, 1, 0])).

example(before([3, 3, 0, 1]), instr(6, 0, 3, 1), after([3, 9, 0, 1])).

example(before([0, 1, 3, 1]), instr(2, 0, 2, 0), after([3, 1, 3, 1])).

example(before([2, 3, 1, 1]), instr(6, 1, 3, 2), after([2, 3, 9, 1])).

example(before([1, 0, 3, 3]), instr(2, 1, 2, 3), after([1, 0, 3, 3])).

example(before([1, 1, 2, 3]), instr(11, 0, 3), after([1, 1, 2, 0])).

example(before([3, 3, 3, 2]), instr(4, 2, 2, 0), after([6, 3, 3, 2])).

example(before([3, 1, 0, 0]), instr(2, 1, 0, 1), after([3, 3, 0, 0])).

example(before([2, 0, 2, 2]), instr(15, 2, 2), after([2, 4, 2, 2])).

example(before([0, 1, 3, 2]), instr(8, 0, 0, 2), after([0, 1, 0, 2])).

example(before([1, 2, 2, 0]), instr(9, 1, 0, 3), after([1, 2, 2, 1])).

example(before([1, 3, 2, 1]), instr(6, 1, 3, 0), after([9, 3, 2, 1])).

example(before([0, 1, 0, 1]), instr(10, 1, 0), after([0, 1, 1, 1])).

example(before([3, 0, 1, 1]), instr(7, 3, 1, 1), after([3, 1, 1, 1])).

example(before([0, 3, 1, 0]), instr(8, 0, 0, 3), after([0, 3, 1, 0])).

example(before([0, 1, 2, 1]), instr(10, 1, 0), after([0, 1, 2, 1])).

example(before([0, 1, 1, 3]), instr(10, 1, 0), after([0, 1, 1, 1])).

example(before([3, 0, 1, 3]), instr(7, 2, 1, 1), after([3, 1, 1, 3])).

example(before([3, 2, 1, 3]), instr(3, 1, 0, 1), after([3, 1, 1, 3])).

example(before([0, 1, 0, 0]), instr(10, 1, 0), after([0, 1, 0, 1])).

example(before([1, 3, 2, 1]), instr(5, 3, 2, 2), after([1, 3, 3, 1])).

example(before([2, 2, 2, 1]), instr(5, 3, 2, 2), after([2, 2, 3, 1])).

example(before([2, 0, 2, 1]), instr(15, 2, 2), after([2, 4, 2, 1])).

example(before([3, 2, 2, 2]), instr(6, 0, 3, 1), after([3, 9, 2, 2])).

example(before([2, 3, 3, 3]), instr(4, 2, 2, 2), after([2, 3, 6, 3])).

example(before([0, 0, 3, 2]), instr(1, 0, 1, 1), after([0, 1, 3, 2])).

example(before([3, 3, 3, 2]), instr(0, 3, 2, 0), after([4, 3, 3, 2])).

example(before([0, 1, 2, 3]), instr(8, 0, 0, 2), after([0, 1, 0, 3])).

example(before([1, 2, 1, 3]), instr(0, 1, 2, 0), after([4, 2, 1, 3])).

example(before([0, 0, 1, 0]), instr(8, 0, 0, 1), after([0, 0, 1, 0])).

example(before([0, 0, 2, 1]), instr(1, 0, 1, 2), after([0, 0, 1, 1])).

example(before([1, 0, 1, 1]), instr(13, 1, 0), after([1, 0, 1, 1])).

example(before([0, 3, 2, 0]), instr(8, 0, 0, 2), after([0, 3, 0, 0])).

example(before([2, 0, 1, 0]), instr(7, 2, 1, 2), after([2, 0, 1, 0])).

example(before([0, 1, 3, 0]), instr(10, 1, 0), after([1, 1, 3, 0])).

example(before([2, 0, 1, 3]), instr(14, 1, 2), after([2, 1, 1, 3])).

example(before([0, 0, 1, 3]), instr(1, 0, 1, 0), after([1, 0, 1, 3])).

example(before([0, 1, 3, 1]), instr(10, 1, 0), after([0, 1, 1, 1])).

example(before([0, 0, 1, 3]), instr(14, 1, 2), after([0, 1, 1, 3])).

example(before([0, 0, 0, 0]), instr(1, 0, 1, 2), after([0, 0, 1, 0])).

example(before([0, 1, 3, 2]), instr(10, 1, 0), after([0, 1, 3, 2])).

example(before([3, 3, 3, 1]), instr(5, 3, 2, 3), after([3, 3, 3, 3])).

example(before([0, 0, 0, 0]), instr(8, 0, 0, 3), after([0, 0, 0, 0])).

example(before([0, 1, 2, 3]), instr(8, 0, 0, 3), after([0, 1, 2, 0])).

example(before([1, 3, 1, 2]), instr(0, 3, 2, 3), after([1, 3, 1, 4])).

example(before([0, 0, 1, 1]), instr(8, 0, 0, 0), after([0, 0, 1, 1])).

example(before([0, 0, 3, 1]), instr(1, 0, 1, 1), after([0, 1, 3, 1])).

example(before([2, 3, 1, 1]), instr(4, 3, 2, 3), after([2, 3, 1, 2])).

example(before([1, 2, 2, 0]), instr(9, 1, 0, 0), after([1, 2, 2, 0])).

example(before([3, 3, 0, 0]), instr(6, 0, 3, 2), after([3, 3, 9, 0])).

example(before([1, 0, 1, 2]), instr(14, 1, 2), after([1, 1, 1, 2])).

example(before([3, 2, 3, 1]), instr(3, 1, 0, 0), after([1, 2, 3, 1])).

example(before([0, 1, 0, 0]), instr(10, 1, 0), after([0, 1, 1, 0])).

example(before([1, 2, 3, 2]), instr(9, 1, 0, 1), after([1, 1, 3, 2])).

example(before([0, 1, 1, 3]), instr(4, 1, 3, 2), after([0, 1, 4, 3])).

example(before([3, 0, 1, 2]), instr(14, 1, 2), after([1, 0, 1, 2])).

example(before([3, 2, 1, 1]), instr(3, 1, 0, 2), after([3, 2, 1, 1])).

example(before([1, 2, 3, 3]), instr(0, 1, 2, 0), after([4, 2, 3, 3])).

example(before([3, 1, 1, 3]), instr(12, 2, 3), after([3, 1, 1, 3])).

example(before([3, 0, 1, 0]), instr(7, 2, 1, 1), after([3, 1, 1, 0])).

example(before([1, 0, 3, 2]), instr(0, 3, 2, 0), after([4, 0, 3, 2])).

example(before([0, 2, 3, 3]), instr(0, 1, 2, 2), after([0, 2, 4, 3])).

example(before([0, 1, 3, 1]), instr(4, 2, 1, 2), after([0, 1, 4, 1])).

example(before([1, 0, 3, 3]), instr(13, 1, 0), after([1, 0, 3, 1])).

example(before([3, 1, 3, 0]), instr(4, 1, 1, 1), after([3, 2, 3, 0])).

example(before([1, 2, 0, 3]), instr(9, 1, 0, 2), after([1, 2, 1, 3])).

example(before([0, 2, 2, 3]), instr(4, 3, 2, 1), after([0, 5, 2, 3])).

example(before([1, 1, 1, 1]), instr(4, 0, 2, 3), after([1, 1, 1, 2])).

example(before([1, 1, 2, 3]), instr(11, 0, 3), after([1, 0, 2, 3])).

example(before([2, 0, 1, 1]), instr(7, 3, 1, 1), after([2, 1, 1, 1])).

example(before([2, 3, 0, 0]), instr(2, 2, 1, 3), after([2, 3, 0, 3])).

example(before([1, 0, 1, 1]), instr(7, 0, 1, 0), after([1, 0, 1, 1])).

example(before([1, 0, 0, 0]), instr(7, 0, 1, 2), after([1, 0, 1, 0])).

example(before([1, 0, 3, 0]), instr(12, 0, 2), after([1, 3, 3, 0])).

example(before([1, 2, 3, 2]), instr(9, 1, 0, 2), after([1, 2, 1, 2])).

example(before([1, 2, 3, 0]), instr(9, 1, 0, 0), after([1, 2, 3, 0])).

example(before([2, 2, 1, 3]), instr(0, 0, 2, 2), after([2, 2, 4, 3])).

example(before([2, 1, 1, 3]), instr(6, 3, 2, 0), after([6, 1, 1, 3])).

example(before([2, 1, 2, 0]), instr(15, 2, 2), after([2, 1, 2, 4])).

example(before([0, 0, 0, 0]), instr(1, 0, 1, 0), after([1, 0, 0, 0])).

example(before([1, 0, 2, 1]), instr(12, 1, 2), after([1, 0, 2, 2])).

example(before([0, 0, 1, 3]), instr(14, 1, 2), after([0, 0, 1, 1])).

example(before([1, 1, 0, 3]), instr(11, 0, 3), after([1, 0, 0, 3])).

example(before([0, 2, 2, 0]), instr(8, 0, 0, 1), after([0, 0, 2, 0])).

example(before([3, 1, 2, 0]), instr(6, 2, 3, 3), after([3, 1, 2, 6])).

example(before([1, 2, 0, 0]), instr(2, 3, 1, 2), after([1, 2, 2, 0])).

example(before([2, 1, 0, 3]), instr(4, 3, 3, 2), after([2, 1, 6, 3])).

example(before([3, 2, 0, 0]), instr(3, 1, 0, 0), after([1, 2, 0, 0])).

example(before([3, 0, 1, 1]), instr(14, 1, 3), after([3, 1, 1, 1])).

example(before([3, 1, 1, 1]), instr(6, 0, 3, 2), after([3, 1, 9, 1])).

example(before([0, 0, 1, 1]), instr(1, 0, 1, 1), after([0, 1, 1, 1])).

example(before([2, 1, 3, 3]), instr(0, 0, 2, 2), after([2, 1, 4, 3])).

example(before([0, 2, 2, 1]), instr(8, 0, 0, 2), after([0, 2, 0, 1])).

example(before([1, 0, 2, 2]), instr(15, 3, 2), after([1, 0, 2, 4])).

example(before([0, 1, 3, 1]), instr(2, 0, 3, 0), after([1, 1, 3, 1])).

example(before([2, 0, 3, 1]), instr(5, 3, 2, 2), after([2, 0, 3, 1])).

example(before([3, 2, 0, 3]), instr(3, 1, 0, 1), after([3, 1, 0, 3])).

example(before([1, 3, 3, 3]), instr(11, 0, 3), after([1, 3, 0, 3])).

example(before([0, 1, 1, 2]), instr(10, 1, 0), after([1, 1, 1, 2])).

example(before([3, 2, 1, 2]), instr(2, 1, 2, 3), after([3, 2, 1, 3])).

example(before([1, 0, 0, 1]), instr(13, 1, 0), after([1, 0, 1, 1])).

example(before([1, 0, 2, 3]), instr(13, 1, 0), after([1, 0, 2, 1])).

example(before([0, 0, 0, 2]), instr(1, 0, 1, 2), after([0, 0, 1, 2])).

example(before([2, 1, 0, 2]), instr(5, 3, 1, 2), after([2, 1, 3, 2])).

example(before([1, 0, 0, 3]), instr(13, 1, 0), after([1, 0, 1, 3])).

example(before([2, 1, 3, 2]), instr(5, 3, 1, 2), after([2, 1, 3, 2])).

example(before([1, 2, 3, 0]), instr(2, 3, 1, 3), after([1, 2, 3, 2])).

example(before([1, 2, 2, 1]), instr(9, 1, 0, 0), after([1, 2, 2, 1])).

example(before([0, 0, 3, 3]), instr(1, 0, 1, 1), after([0, 1, 3, 3])).

example(before([3, 2, 2, 3]), instr(4, 1, 3, 0), after([5, 2, 2, 3])).

example(before([3, 2, 1, 2]), instr(3, 1, 0, 0), after([1, 2, 1, 2])).

example(before([2, 1, 3, 1]), instr(6, 3, 2, 3), after([2, 1, 3, 2])).

example(before([3, 1, 2, 3]), instr(15, 2, 2), after([3, 1, 4, 3])).

example(before([2, 0, 3, 3]), instr(11, 0, 3), after([2, 0, 3, 3])).

example(before([0, 0, 2, 2]), instr(1, 0, 1, 0), after([1, 0, 2, 2])).

example(before([3, 2, 2, 1]), instr(15, 2, 2), after([3, 2, 2, 4])).

example(before([3, 3, 1, 2]), instr(6, 0, 3, 3), after([3, 3, 1, 9])).

example(before([2, 1, 0, 2]), instr(12, 0, 1), after([2, 1, 0, 3])).

example(before([2, 1, 1, 2]), instr(12, 0, 1), after([2, 1, 1, 3])).

example(before([0, 2, 1, 3]), instr(12, 2, 3), after([0, 3, 1, 3])).

example(before([3, 1, 1, 3]), instr(4, 1, 3, 3), after([3, 1, 1, 4])).

example(before([0, 0, 3, 0]), instr(1, 0, 1, 0), after([1, 0, 3, 0])).

example(before([1, 0, 0, 3]), instr(13, 1, 0), after([1, 1, 0, 3])).

example(before([3, 0, 1, 0]), instr(14, 1, 2), after([1, 0, 1, 0])).

example(before([3, 2, 0, 1]), instr(3, 1, 0, 1), after([3, 1, 0, 1])).

example(before([3, 1, 3, 2]), instr(12, 1, 2), after([3, 3, 3, 2])).

example(before([2, 0, 0, 1]), instr(7, 3, 1, 3), after([2, 0, 0, 1])).

example(before([3, 3, 3, 1]), instr(5, 3, 2, 1), after([3, 3, 3, 1])).

example(before([0, 0, 3, 0]), instr(8, 0, 0, 2), after([0, 0, 0, 0])).

example(before([1, 3, 1, 1]), instr(6, 1, 2, 2), after([1, 3, 6, 1])).

example(before([1, 2, 2, 0]), instr(9, 1, 0, 2), after([1, 2, 1, 0])).

example(before([0, 2, 3, 1]), instr(6, 1, 3, 2), after([0, 2, 6, 1])).

example(before([0, 0, 1, 2]), instr(14, 1, 2), after([1, 0, 1, 2])).

example(before([2, 1, 3, 1]), instr(0, 0, 2, 0), after([4, 1, 3, 1])).

example(before([1, 2, 0, 3]), instr(9, 1, 0, 0), after([1, 2, 0, 3])).

example(before([1, 3, 0, 3]), instr(11, 0, 3), after([1, 3, 0, 3])).

example(before([2, 2, 0, 3]), instr(11, 0, 3), after([2, 2, 0, 3])).

example(before([3, 2, 3, 1]), instr(3, 1, 0, 2), after([3, 2, 1, 1])).

example(before([1, 0, 1, 3]), instr(13, 1, 0), after([1, 0, 1, 1])).

example(before([1, 0, 1, 0]), instr(13, 1, 0), after([1, 1, 1, 0])).

example(before([1, 3, 1, 3]), instr(11, 0, 3), after([1, 3, 1, 0])).

example(before([0, 1, 1, 2]), instr(8, 0, 0, 1), after([0, 0, 1, 2])).

example(before([2, 2, 0, 2]), instr(0, 3, 2, 3), after([2, 2, 0, 4])).

example(before([0, 1, 1, 2]), instr(8, 0, 0, 2), after([0, 1, 0, 2])).

example(before([1, 2, 3, 3]), instr(2, 1, 0, 1), after([1, 3, 3, 3])).

example(before([1, 0, 1, 1]), instr(7, 0, 1, 3), after([1, 0, 1, 1])).

example(before([2, 1, 3, 1]), instr(5, 3, 2, 1), after([2, 3, 3, 1])).

example(before([0, 2, 3, 3]), instr(8, 0, 0, 2), after([0, 2, 0, 3])).

example(before([3, 0, 0, 3]), instr(2, 2, 0, 3), after([3, 0, 0, 3])).

example(before([1, 0, 3, 3]), instr(12, 0, 2), after([1, 3, 3, 3])).

example(before([3, 2, 0, 2]), instr(3, 1, 0, 1), after([3, 1, 0, 2])).

example(before([3, 1, 2, 1]), instr(4, 0, 2, 0), after([5, 1, 2, 1])).

example(before([0, 1, 0, 3]), instr(10, 1, 0), after([1, 1, 0, 3])).

example(before([1, 2, 0, 1]), instr(2, 0, 1, 2), after([1, 2, 3, 1])).

example(before([1, 3, 0, 3]), instr(11, 0, 3), after([0, 3, 0, 3])).

example(before([1, 0, 0, 2]), instr(0, 3, 2, 2), after([1, 0, 4, 2])).

example(before([0, 1, 0, 1]), instr(10, 1, 0), after([0, 1, 0, 1])).

example(before([1, 0, 1, 3]), instr(13, 1, 0), after([1, 1, 1, 3])).

example(before([1, 2, 0, 1]), instr(9, 1, 0, 1), after([1, 1, 0, 1])).

example(before([2, 2, 3, 2]), instr(0, 1, 2, 2), after([2, 2, 4, 2])).

example(before([0, 0, 1, 3]), instr(14, 1, 2), after([0, 0, 1, 3])).

example(before([0, 1, 0, 2]), instr(8, 0, 0, 2), after([0, 1, 0, 2])).

example(before([2, 2, 3, 1]), instr(5, 3, 2, 0), after([3, 2, 3, 1])).

example(before([1, 0, 0, 3]), instr(13, 1, 0), after([1, 0, 0, 1])).

example(before([1, 1, 0, 3]), instr(11, 0, 3), after([1, 1, 0, 3])).

example(before([0, 0, 3, 2]), instr(2, 0, 2, 2), after([0, 0, 3, 2])).

example(before([1, 2, 2, 2]), instr(9, 1, 0, 0), after([1, 2, 2, 2])).

example(before([3, 2, 3, 1]), instr(3, 1, 0, 3), after([3, 2, 3, 1])).

example(before([1, 2, 3, 1]), instr(9, 1, 0, 2), after([1, 2, 1, 1])).

example(before([0, 1, 0, 1]), instr(8, 0, 0, 3), after([0, 1, 0, 0])).

example(before([2, 2, 2, 3]), instr(15, 2, 2), after([4, 2, 2, 3])).

example(before([0, 1, 2, 2]), instr(10, 1, 0), after([1, 1, 2, 2])).

example(before([1, 1, 2, 1]), instr(5, 3, 2, 3), after([1, 1, 2, 3])).

example(before([2, 0, 0, 1]), instr(14, 1, 3), after([2, 1, 0, 1])).

example(before([3, 0, 2, 0]), instr(12, 3, 2), after([2, 0, 2, 0])).

example(before([0, 0, 3, 3]), instr(12, 1, 3), after([0, 0, 3, 3])).

example(before([3, 0, 1, 1]), instr(14, 1, 3), after([1, 0, 1, 1])).

example(before([1, 1, 3, 1]), instr(4, 2, 1, 2), after([1, 1, 4, 1])).

example(before([0, 2, 0, 0]), instr(0, 1, 2, 0), after([4, 2, 0, 0])).

example(before([0, 2, 2, 1]), instr(5, 3, 2, 0), after([3, 2, 2, 1])).

example(before([3, 2, 0, 0]), instr(3, 1, 0, 3), after([3, 2, 0, 1])).

example(before([3, 2, 0, 2]), instr(3, 1, 0, 0), after([1, 2, 0, 2])).

example(before([0, 2, 3, 3]), instr(0, 1, 2, 1), after([0, 4, 3, 3])).

example(before([2, 3, 1, 3]), instr(2, 0, 2, 0), after([3, 3, 1, 3])).

example(before([1, 2, 0, 2]), instr(9, 1, 0, 0), after([1, 2, 0, 2])).

example(before([0, 1, 0, 0]), instr(10, 1, 0), after([1, 1, 0, 0])).

example(before([2, 0, 3, 1]), instr(7, 3, 1, 1), after([2, 1, 3, 1])).

example(before([3, 2, 3, 2]), instr(3, 1, 0, 1), after([3, 1, 3, 2])).

example(before([1, 2, 3, 3]), instr(4, 3, 2, 1), after([1, 6, 3, 3])).

example(before([0, 3, 2, 0]), instr(15, 2, 2), after([0, 3, 4, 0])).

example(before([0, 3, 3, 2]), instr(0, 3, 2, 3), after([0, 3, 3, 4])).

example(before([0, 1, 2, 0]), instr(10, 1, 0), after([0, 1, 1, 0])).

example(before([3, 2, 0, 0]), instr(6, 1, 3, 1), after([3, 6, 0, 0])).

example(before([0, 1, 1, 3]), instr(10, 1, 0), after([1, 1, 1, 3])).

example(before([3, 0, 3, 3]), instr(4, 3, 2, 1), after([3, 6, 3, 3])).

example(before([1, 0, 0, 3]), instr(11, 0, 3), after([1, 0, 0, 0])).

example(before([1, 0, 3, 2]), instr(13, 1, 0), after([1, 1, 3, 2])).

example(before([1, 0, 3, 2]), instr(6, 2, 3, 1), after([1, 9, 3, 2])).

example(before([1, 0, 3, 0]), instr(6, 0, 2, 2), after([1, 0, 2, 0])).

example(before([1, 2, 1, 0]), instr(9, 1, 0, 3), after([1, 2, 1, 1])).

example(before([0, 0, 3, 2]), instr(1, 0, 1, 3), after([0, 0, 3, 1])).

example(before([1, 0, 1, 3]), instr(13, 1, 0), after([1, 0, 1, 3])).

example(before([0, 2, 2, 2]), instr(15, 1, 2), after([0, 2, 2, 4])).

example(before([0, 1, 3, 2]), instr(10, 1, 0), after([1, 1, 3, 2])).

example(before([1, 0, 3, 0]), instr(13, 1, 0), after([1, 0, 3, 1])).

example(before([0, 1, 1, 2]), instr(10, 1, 0), after([0, 1, 1, 1])).

example(before([1, 2, 1, 3]), instr(9, 1, 0, 2), after([1, 2, 1, 3])).

example(before([1, 3, 2, 2]), instr(6, 3, 3, 3), after([1, 3, 2, 6])).

example(before([2, 1, 2, 3]), instr(15, 2, 2), after([2, 4, 2, 3])).

example(before([3, 2, 3, 1]), instr(6, 2, 3, 3), after([3, 2, 3, 9])).

example(before([3, 0, 1, 1]), instr(14, 1, 2), after([3, 0, 1, 1])).

example(before([1, 1, 3, 2]), instr(12, 0, 3), after([1, 1, 3, 2])).

example(before([1, 2, 2, 0]), instr(15, 2, 2), after([1, 2, 4, 0])).

example(before([0, 1, 3, 0]), instr(8, 0, 0, 0), after([0, 1, 3, 0])).

example(before([0, 0, 0, 3]), instr(1, 0, 1, 1), after([0, 1, 0, 3])).

example(before([0, 0, 0, 3]), instr(1, 0, 1, 2), after([0, 0, 1, 3])).

example(before([3, 2, 0, 3]), instr(3, 1, 0, 3), after([3, 2, 0, 1])).

example(before([2, 1, 2, 0]), instr(12, 2, 1), after([2, 1, 2, 3])).

example(before([0, 2, 1, 0]), instr(2, 3, 1, 2), after([0, 2, 2, 0])).

example(before([0, 0, 1, 0]), instr(1, 0, 1, 1), after([0, 1, 1, 0])).

example(before([3, 2, 3, 1]), instr(5, 3, 2, 2), after([3, 2, 3, 1])).

example(before([2, 2, 0, 3]), instr(11, 0, 3), after([2, 2, 0, 0])).

example(before([3, 2, 0, 0]), instr(3, 1, 0, 2), after([3, 2, 1, 0])).

example(before([1, 0, 2, 0]), instr(13, 1, 0), after([1, 1, 2, 0])).

example(before([0, 3, 0, 0]), instr(2, 0, 1, 3), after([0, 3, 0, 3])).

example(before([0, 1, 1, 2]), instr(10, 1, 0), after([0, 1, 1, 2])).

example(before([0, 0, 0, 2]), instr(1, 0, 1, 1), after([0, 1, 0, 2])).

example(before([0, 1, 1, 2]), instr(12, 0, 1), after([0, 1, 1, 2])).

example(before([0, 0, 3, 0]), instr(1, 0, 1, 2), after([0, 0, 1, 0])).

example(before([0, 1, 2, 1]), instr(10, 1, 0), after([0, 1, 2, 1])).

example(before([0, 1, 3, 1]), instr(4, 2, 2, 1), after([0, 6, 3, 1])).

example(before([0, 1, 2, 3]), instr(12, 1, 2), after([0, 1, 3, 3])).

example(before([1, 0, 1, 1]), instr(14, 1, 3), after([1, 0, 1, 1])).

example(before([1, 0, 1, 3]), instr(13, 1, 0), after([1, 0, 1, 3])).

example(before([1, 1, 3, 1]), instr(5, 3, 2, 1), after([1, 3, 3, 1])).

example(before([1, 2, 2, 3]), instr(9, 1, 0, 0), after([1, 2, 2, 3])).

example(before([0, 0, 2, 1]), instr(14, 1, 3), after([0, 0, 2, 1])).

example(before([0, 3, 2, 0]), instr(8, 0, 0, 1), after([0, 0, 2, 0])).

example(before([1, 0, 3, 1]), instr(12, 0, 2), after([1, 0, 3, 1])).

example(before([3, 0, 1, 2]), instr(0, 3, 2, 3), after([3, 0, 1, 4])).

example(before([0, 2, 2, 2]), instr(15, 1, 2), after([0, 2, 4, 2])).

example(before([0, 1, 1, 1]), instr(10, 1, 0), after([0, 1, 1, 1])).

example(before([1, 0, 0, 2]), instr(13, 1, 0), after([1, 0, 1, 2])).

example(before([0, 0, 1, 1]), instr(14, 1, 3), after([1, 0, 1, 1])).

example(before([3, 2, 0, 1]), instr(3, 1, 0, 0), after([1, 2, 0, 1])).

example(before([0, 0, 2, 0]), instr(1, 0, 1, 1), after([0, 1, 2, 0])).

example(before([0, 2, 0, 2]), instr(2, 0, 3, 0), after([2, 2, 0, 2])).

example(before([0, 0, 1, 2]), instr(14, 1, 2), after([0, 0, 1, 2])).

example(before([1, 1, 0, 1]), instr(4, 1, 1, 1), after([1, 2, 0, 1])).

example(before([0, 1, 2, 1]), instr(5, 3, 2, 2), after([0, 1, 3, 1])).

example(before([0, 3, 1, 3]), instr(8, 0, 0, 3), after([0, 3, 1, 0])).

example(before([0, 0, 2, 1]), instr(7, 3, 1, 3), after([0, 0, 2, 1])).

example(before([0, 1, 1, 2]), instr(0, 3, 2, 0), after([4, 1, 1, 2])).

example(before([1, 2, 2, 2]), instr(9, 1, 0, 1), after([1, 1, 2, 2])).

example(before([2, 3, 1, 2]), instr(0, 3, 2, 1), after([2, 4, 1, 2])).

example(before([2, 0, 2, 2]), instr(15, 3, 2), after([4, 0, 2, 2])).

example(before([1, 2, 1, 3]), instr(9, 1, 0, 3), after([1, 2, 1, 1])).

example(before([3, 2, 2, 3]), instr(15, 1, 2), after([3, 4, 2, 3])).

example(before([1, 0, 2, 2]), instr(13, 1, 0), after([1, 0, 2, 1])).

example(before([3, 0, 1, 0]), instr(7, 2, 1, 2), after([3, 0, 1, 0])).

example(before([0, 2, 2, 3]), instr(8, 0, 0, 0), after([0, 2, 2, 3])).

example(before([2, 0, 1, 3]), instr(14, 1, 2), after([2, 0, 1, 1])).

example(before([0, 1, 1, 1]), instr(4, 1, 1, 2), after([0, 1, 2, 1])).

example(before([1, 0, 1, 3]), instr(11, 0, 3), after([1, 0, 1, 0])).

example(before([1, 3, 2, 1]), instr(5, 3, 2, 0), after([3, 3, 2, 1])).

example(before([1, 0, 2, 1]), instr(15, 2, 2), after([1, 0, 4, 1])).

example(before([3, 2, 2, 3]), instr(3, 1, 0, 1), after([3, 1, 2, 3])).

example(before([0, 1, 2, 3]), instr(10, 1, 0), after([1, 1, 2, 3])).

example(before([2, 0, 0, 1]), instr(14, 1, 3), after([2, 0, 1, 1])).

example(before([0, 1, 2, 3]), instr(4, 1, 3, 0), after([4, 1, 2, 3])).

example(before([3, 2, 0, 2]), instr(0, 1, 2, 1), after([3, 4, 0, 2])).

example(before([1, 0, 2, 3]), instr(12, 0, 2), after([1, 0, 3, 3])).

example(before([3, 2, 1, 0]), instr(3, 1, 0, 3), after([3, 2, 1, 1])).

example(before([1, 1, 3, 1]), instr(6, 0, 2, 2), after([1, 1, 2, 1])).

example(before([2, 0, 3, 0]), instr(0, 0, 2, 0), after([4, 0, 3, 0])).

example(before([0, 0, 3, 3]), instr(8, 0, 0, 0), after([0, 0, 3, 3])).

example(before([0, 0, 0, 1]), instr(14, 1, 3), after([0, 0, 0, 1])).

example(before([1, 0, 2, 2]), instr(13, 1, 0), after([1, 0, 2, 2])).

example(before([0, 0, 2, 2]), instr(15, 2, 2), after([0, 4, 2, 2])).

example(before([2, 2, 3, 2]), instr(0, 3, 2, 2), after([2, 2, 4, 2])).

example(before([1, 2, 0, 0]), instr(9, 1, 0, 3), after([1, 2, 0, 1])).

example(before([3, 2, 1, 3]), instr(3, 1, 0, 3), after([3, 2, 1, 1])).

example(before([0, 0, 3, 1]), instr(1, 0, 1, 2), after([0, 0, 1, 1])).

example(before([1, 0, 1, 1]), instr(4, 3, 2, 1), after([1, 2, 1, 1])).

example(before([0, 1, 2, 3]), instr(10, 1, 0), after([0, 1, 1, 3])).

example(before([1, 1, 3, 2]), instr(12, 1, 3), after([1, 3, 3, 2])).

example(before([0, 0, 0, 3]), instr(8, 0, 0, 0), after([0, 0, 0, 3])).

example(before([0, 2, 0, 0]), instr(8, 0, 0, 2), after([0, 2, 0, 0])).

example(before([1, 0, 1, 0]), instr(13, 1, 0), after([1, 0, 1, 1])).

example(before([1, 0, 2, 2]), instr(7, 0, 1, 2), after([1, 0, 1, 2])).

example(before([3, 0, 2, 3]), instr(15, 2, 2), after([3, 0, 4, 3])).

example(before([3, 2, 3, 3]), instr(3, 1, 0, 3), after([3, 2, 3, 1])).

example(before([3, 2, 1, 2]), instr(3, 1, 0, 1), after([3, 1, 1, 2])).

example(before([0, 0, 3, 2]), instr(1, 0, 1, 0), after([1, 0, 3, 2])).

example(before([1, 2, 0, 1]), instr(9, 1, 0, 3), after([1, 2, 0, 1])).

example(before([2, 0, 2, 3]), instr(11, 0, 3), after([2, 0, 2, 3])).

example(before([1, 1, 1, 2]), instr(5, 3, 1, 1), after([1, 3, 1, 2])).

example(before([3, 2, 2, 1]), instr(3, 1, 0, 2), after([3, 2, 1, 1])).

example(before([0, 2, 2, 2]), instr(15, 3, 2), after([0, 2, 4, 2])).

example(before([0, 2, 2, 3]), instr(8, 0, 0, 1), after([0, 0, 2, 3])).

example(before([3, 2, 2, 3]), instr(3, 1, 0, 0), after([1, 2, 2, 3])).

example(before([2, 2, 1, 3]), instr(4, 3, 3, 2), after([2, 2, 6, 3])).

example(before([2, 3, 1, 2]), instr(12, 2, 3), after([2, 3, 1, 2])).

example(before([3, 0, 1, 1]), instr(14, 1, 2), after([3, 0, 1, 1])).

example(before([1, 1, 3, 0]), instr(4, 2, 1, 1), after([1, 4, 3, 0])).

example(before([0, 0, 0, 1]), instr(14, 1, 3), after([1, 0, 0, 1])).

example(before([1, 1, 3, 0]), instr(4, 1, 1, 0), after([2, 1, 3, 0])).

example(before([0, 1, 0, 0]), instr(10, 1, 0), after([0, 1, 0, 0])).

example(before([3, 2, 3, 2]), instr(3, 1, 0, 3), after([3, 2, 3, 1])).

example(before([2, 0, 3, 2]), instr(2, 1, 0, 3), after([2, 0, 3, 2])).

example(before([1, 2, 3, 3]), instr(11, 0, 3), after([1, 2, 0, 3])).

example(before([1, 2, 0, 3]), instr(9, 1, 0, 1), after([1, 1, 0, 3])).

example(before([3, 2, 0, 3]), instr(4, 3, 3, 1), after([3, 6, 0, 3])).

example(before([2, 3, 1, 0]), instr(2, 3, 0, 3), after([2, 3, 1, 2])).

example(before([0, 1, 2, 3]), instr(8, 0, 0, 1), after([0, 0, 2, 3])).

example(before([2, 3, 3, 3]), instr(4, 3, 2, 1), after([2, 6, 3, 3])).

example(before([0, 0, 2, 0]), instr(8, 0, 0, 1), after([0, 0, 2, 0])).

example(before([1, 0, 0, 2]), instr(13, 1, 0), after([1, 1, 0, 2])).

example(before([1, 0, 2, 3]), instr(11, 0, 3), after([1, 0, 2, 0])).

example(before([1, 2, 1, 3]), instr(9, 1, 0, 0), after([1, 2, 1, 3])).

example(before([2, 0, 0, 3]), instr(11, 0, 3), after([2, 0, 0, 3])).

example(before([1, 0, 0, 0]), instr(13, 1, 0), after([1, 0, 0, 0])).

example(before([0, 2, 1, 3]), instr(0, 1, 2, 1), after([0, 4, 1, 3])).

example(before([0, 3, 1, 2]), instr(6, 3, 3, 2), after([0, 3, 6, 2])).

example(before([1, 2, 0, 0]), instr(9, 1, 0, 0), after([1, 2, 0, 0])).

example(before([1, 0, 3, 0]), instr(13, 1, 0), after([1, 1, 3, 0])).

example(before([0, 0, 1, 3]), instr(1, 0, 1, 3), after([0, 0, 1, 1])).

example(before([0, 2, 1, 1]), instr(8, 0, 0, 0), after([0, 2, 1, 1])).

example(before([3, 0, 1, 1]), instr(14, 1, 3), after([3, 0, 1, 1])).

example(before([3, 2, 0, 3]), instr(3, 1, 0, 0), after([1, 2, 0, 3])).

example(before([1, 2, 1, 0]), instr(9, 1, 0, 1), after([1, 1, 1, 0])).

example(before([1, 2, 3, 0]), instr(0, 1, 2, 2), after([1, 2, 4, 0])).

example(before([1, 2, 2, 2]), instr(9, 1, 0, 2), after([1, 2, 1, 2])).

example(before([0, 1, 3, 1]), instr(10, 1, 0), after([1, 1, 3, 1])).

example(before([0, 2, 2, 0]), instr(2, 0, 1, 0), after([2, 2, 2, 0])).

example(before([0, 0, 2, 2]), instr(1, 0, 1, 2), after([0, 0, 1, 2])).

example(before([1, 2, 3, 0]), instr(9, 1, 0, 3), after([1, 2, 3, 1])).

example(before([0, 0, 2, 0]), instr(1, 0, 1, 3), after([0, 0, 2, 1])).

example(before([0, 3, 2, 0]), instr(8, 0, 0, 3), after([0, 3, 2, 0])).

example(before([0, 0, 1, 2]), instr(1, 0, 1, 3), after([0, 0, 1, 1])).

example(before([3, 2, 3, 0]), instr(3, 1, 0, 1), after([3, 1, 3, 0])).

example(before([1, 0, 3, 1]), instr(5, 3, 2, 2), after([1, 0, 3, 1])).

example(before([1, 0, 1, 0]), instr(14, 1, 2), after([1, 0, 1, 0])).

example(before([2, 3, 3, 3]), instr(11, 0, 3), after([2, 3, 0, 3])).

example(before([1, 2, 1, 0]), instr(2, 3, 1, 2), after([1, 2, 2, 0])).

example(before([0, 2, 3, 2]), instr(0, 3, 2, 2), after([0, 2, 4, 2])).

example(before([0, 3, 3, 0]), instr(6, 2, 3, 0), after([9, 3, 3, 0])).

example(before([1, 0, 1, 1]), instr(14, 1, 3), after([1, 0, 1, 1])).

example(before([0, 1, 0, 3]), instr(10, 1, 0), after([0, 1, 0, 3])).

example(before([3, 0, 1, 2]), instr(14, 1, 2), after([3, 0, 1, 2])).

example(before([1, 3, 2, 3]), instr(4, 3, 3, 0), after([6, 3, 2, 3])).

example(before([2, 3, 2, 2]), instr(15, 3, 2), after([2, 3, 2, 4])).

example(before([2, 0, 1, 1]), instr(14, 1, 2), after([2, 1, 1, 1])).

example(before([3, 2, 1, 1]), instr(3, 1, 0, 1), after([3, 1, 1, 1])).

example(before([1, 2, 3, 3]), instr(4, 3, 2, 0), after([6, 2, 3, 3])).

example(before([1, 0, 1, 1]), instr(7, 3, 1, 0), after([1, 0, 1, 1])).

example(before([1, 3, 3, 1]), instr(5, 3, 2, 3), after([1, 3, 3, 3])).

example(before([2, 0, 3, 1]), instr(14, 1, 3), after([2, 1, 3, 1])).

example(before([1, 0, 3, 3]), instr(11, 0, 3), after([1, 0, 3, 0])).

example(before([2, 0, 2, 1]), instr(14, 1, 3), after([2, 0, 2, 1])).

example(before([0, 0, 0, 0]), instr(8, 0, 0, 0), after([0, 0, 0, 0])).

example(before([3, 1, 2, 1]), instr(5, 3, 2, 2), after([3, 1, 3, 1])).

example(before([0, 2, 3, 0]), instr(8, 0, 0, 1), after([0, 0, 3, 0])).

example(before([2, 1, 1, 3]), instr(4, 3, 3, 3), after([2, 1, 1, 6])).

example(before([2, 1, 0, 2]), instr(0, 3, 2, 0), after([4, 1, 0, 2])).

example(before([0, 0, 2, 3]), instr(1, 0, 1, 1), after([0, 1, 2, 3])).

example(before([2, 1, 2, 3]), instr(15, 2, 2), after([4, 1, 2, 3])).

example(before([1, 2, 1, 0]), instr(9, 1, 0, 2), after([1, 2, 1, 0])).

example(before([1, 2, 1, 2]), instr(9, 1, 0, 0), after([1, 2, 1, 2])).

example(before([1, 0, 3, 3]), instr(13, 1, 0), after([1, 1, 3, 3])).

example(before([0, 1, 1, 2]), instr(8, 0, 0, 3), after([0, 1, 1, 0])).

example(before([1, 0, 3, 3]), instr(13, 1, 0), after([1, 0, 1, 3])).

example(before([0, 1, 2, 1]), instr(5, 3, 2, 0), after([3, 1, 2, 1])).

example(before([1, 3, 2, 1]), instr(6, 2, 3, 2), after([1, 3, 6, 1])).

example(before([3, 2, 2, 1]), instr(3, 1, 0, 3), after([3, 2, 2, 1])).

example(before([2, 0, 1, 1]), instr(14, 1, 2), after([2, 0, 1, 1])).

example(before([2, 1, 0, 2]), instr(2, 1, 0, 2), after([2, 1, 3, 2])).

example(before([0, 2, 1, 0]), instr(2, 1, 2, 0), after([3, 2, 1, 0])).

example(before([3, 2, 2, 0]), instr(4, 0, 2, 0), after([5, 2, 2, 0])).

example(before([1, 0, 1, 3]), instr(7, 0, 1, 3), after([1, 0, 1, 1])).

example(before([0, 2, 1, 2]), instr(2, 0, 2, 0), after([1, 2, 1, 2])).

example(before([3, 0, 2, 3]), instr(12, 1, 2), after([2, 0, 2, 3])).

example(before([2, 2, 1, 0]), instr(2, 0, 2, 2), after([2, 2, 3, 0])).

example(before([3, 1, 2, 2]), instr(15, 3, 2), after([4, 1, 2, 2])).

example(before([0, 3, 2, 2]), instr(15, 3, 2), after([0, 3, 4, 2])).

example(before([1, 0, 3, 2]), instr(13, 1, 0), after([1, 0, 1, 2])).

example(before([1, 1, 0, 2]), instr(0, 3, 2, 1), after([1, 4, 0, 2])).

example(before([0, 1, 0, 2]), instr(10, 1, 0), after([1, 1, 0, 2])).

example(before([3, 1, 2, 1]), instr(5, 3, 2, 3), after([3, 1, 2, 3])).

example(before([3, 1, 3, 3]), instr(4, 2, 2, 0), after([6, 1, 3, 3])).

example(before([3, 3, 1, 0]), instr(6, 1, 3, 2), after([3, 3, 9, 0])).

example(before([2, 1, 1, 3]), instr(4, 3, 1, 3), after([2, 1, 1, 4])).

example(before([0, 0, 2, 3]), instr(8, 0, 0, 1), after([0, 0, 2, 3])).

example(before([1, 3, 2, 3]), instr(11, 0, 3), after([1, 0, 2, 3])).

example(before([2, 2, 1, 0]), instr(0, 0, 2, 3), after([2, 2, 1, 4])).

example(before([1, 0, 2, 1]), instr(14, 1, 3), after([1, 0, 2, 1])).

example(before([0, 1, 2, 2]), instr(5, 3, 1, 3), after([0, 1, 2, 3])).

example(before([1, 0, 2, 3]), instr(7, 0, 1, 1), after([1, 1, 2, 3])).

example(before([2, 2, 1, 3]), instr(11, 0, 3), after([2, 2, 1, 0])).

example(before([0, 1, 2, 3]), instr(10, 1, 0), after([0, 1, 2, 1])).

example(before([1, 0, 0, 3]), instr(7, 0, 1, 0), after([1, 0, 0, 3])).

example(before([0, 1, 0, 3]), instr(8, 0, 0, 0), after([0, 1, 0, 3])).

example(before([2, 1, 0, 0]), instr(2, 1, 0, 2), after([2, 1, 3, 0])).

example(before([0, 0, 3, 1]), instr(1, 0, 1, 3), after([0, 0, 3, 1])).

example(before([1, 0, 0, 1]), instr(14, 1, 3), after([1, 0, 0, 1])).

example(before([2, 0, 1, 2]), instr(2, 1, 3, 2), after([2, 0, 2, 2])).

example(before([1, 0, 2, 2]), instr(2, 1, 3, 0), after([2, 0, 2, 2])).

example(before([3, 2, 0, 1]), instr(3, 1, 0, 2), after([3, 2, 1, 1])).

example(before([1, 0, 2, 3]), instr(11, 0, 3), after([1, 0, 0, 3])).

example(before([2, 2, 1, 2]), instr(12, 2, 3), after([3, 2, 1, 2])).

example(before([2, 0, 1, 3]), instr(11, 0, 3), after([2, 0, 0, 3])).

example(before([2, 0, 3, 1]), instr(14, 1, 3), after([2, 0, 3, 1])).

example(before([1, 2, 3, 0]), instr(9, 1, 0, 1), after([1, 1, 3, 0])).

example(before([0, 0, 2, 2]), instr(15, 2, 2), after([4, 0, 2, 2])).

example(before([0, 1, 0, 2]), instr(10, 1, 0), after([0, 1, 0, 2])).

example(before([0, 1, 0, 3]), instr(12, 0, 1), after([0, 1, 1, 3])).

example(before([2, 1, 0, 3]), instr(11, 0, 3), after([2, 1, 0, 3])).

example(before([2, 3, 0, 3]), instr(6, 3, 2, 1), after([2, 6, 0, 3])).

example(before([0, 0, 2, 3]), instr(15, 2, 2), after([0, 0, 4, 3])).

example(before([3, 2, 1, 1]), instr(3, 1, 0, 0), after([1, 2, 1, 1])).

example(before([0, 2, 2, 0]), instr(15, 2, 2), after([0, 2, 4, 0])).

example(before([0, 3, 0, 1]), instr(2, 2, 1, 1), after([0, 3, 0, 1])).

example(before([0, 3, 0, 1]), instr(8, 0, 0, 0), after([0, 3, 0, 1])).

example(before([2, 1, 1, 3]), instr(11, 0, 3), after([0, 1, 1, 3])).

example(before([1, 0, 3, 1]), instr(14, 1, 3), after([1, 0, 3, 1])).

example(before([1, 1, 2, 3]), instr(4, 0, 3, 3), after([1, 1, 2, 4])).

example(before([2, 0, 2, 1]), instr(15, 0, 2), after([4, 0, 2, 1])).

example(before([1, 0, 3, 2]), instr(12, 0, 2), after([1, 0, 3, 3])).

example(before([3, 2, 2, 2]), instr(6, 0, 3, 3), after([3, 2, 2, 9])).

example(before([1, 0, 1, 2]), instr(13, 1, 0), after([1, 0, 1, 2])).

example(before([2, 3, 1, 3]), instr(4, 3, 3, 2), after([2, 3, 6, 3])).

example(before([3, 0, 3, 1]), instr(7, 3, 1, 1), after([3, 1, 3, 1])).

example(before([1, 0, 3, 1]), instr(13, 1, 0), after([1, 1, 3, 1])).

example(before([0, 0, 1, 2]), instr(1, 0, 1, 1), after([0, 1, 1, 2])).

example(before([1, 0, 0, 3]), instr(13, 1, 0), after([1, 0, 0, 3])).

example(before([0, 1, 3, 2]), instr(10, 1, 0), after([0, 1, 3, 1])).

example(before([3, 0, 0, 3]), instr(6, 3, 2, 2), after([3, 0, 6, 3])).

example(before([2, 2, 0, 2]), instr(6, 0, 3, 3), after([2, 2, 0, 6])).

example(before([1, 2, 1, 3]), instr(2, 1, 2, 2), after([1, 2, 3, 3])).

example(before([3, 0, 1, 3]), instr(14, 1, 2), after([3, 0, 1, 1])).

example(before([2, 1, 0, 2]), instr(5, 3, 1, 0), after([3, 1, 0, 2])).

example(before([1, 0, 2, 3]), instr(11, 0, 3), after([1, 0, 2, 3])).

example(before([0, 3, 1, 1]), instr(6, 1, 3, 2), after([0, 3, 9, 1])).

example(before([1, 1, 0, 3]), instr(11, 0, 3), after([1, 1, 0, 0])).

example(before([3, 2, 0, 2]), instr(3, 1, 0, 3), after([3, 2, 0, 1])).

example(before([1, 0, 1, 2]), instr(7, 2, 1, 3), after([1, 0, 1, 1])).

example(before([2, 0, 1, 0]), instr(14, 1, 2), after([2, 0, 1, 0])).

example(before([1, 0, 3, 3]), instr(13, 1, 0), after([1, 0, 3, 3])).

example(before([1, 2, 0, 3]), instr(11, 0, 3), after([1, 0, 0, 3])).

example(before([0, 1, 1, 1]), instr(10, 1, 0), after([0, 1, 1, 1])).

example(before([1, 0, 1, 0]), instr(13, 1, 0), after([1, 0, 1, 0])).

example(before([1, 1, 0, 1]), instr(4, 0, 1, 1), after([1, 2, 0, 1])).

example(before([0, 0, 3, 2]), instr(0, 3, 2, 0), after([4, 0, 3, 2])).

example(before([0, 3, 2, 3]), instr(15, 2, 2), after([0, 3, 4, 3])).

example(before([0, 1, 0, 0]), instr(8, 0, 0, 0), after([0, 1, 0, 0])).

example(before([2, 0, 2, 3]), instr(12, 1, 3), after([2, 0, 2, 3])).

example(before([2, 0, 3, 2]), instr(0, 3, 2, 2), after([2, 0, 4, 2])).

example(before([0, 0, 1, 2]), instr(7, 2, 1, 2), after([0, 0, 1, 2])).

example(before([3, 1, 0, 2]), instr(5, 3, 1, 2), after([3, 1, 3, 2])).

example(before([0, 0, 3, 2]), instr(1, 0, 1, 2), after([0, 0, 1, 2])).

example(before([1, 1, 1, 2]), instr(5, 3, 1, 0), after([3, 1, 1, 2])).

example(before([3, 2, 1, 0]), instr(6, 0, 3, 0), after([9, 2, 1, 0])).

example(before([0, 0, 0, 1]), instr(8, 0, 0, 3), after([0, 0, 0, 0])).

example(before([2, 0, 1, 2]), instr(14, 1, 2), after([2, 1, 1, 2])).

example(before([1, 0, 2, 0]), instr(13, 1, 0), after([1, 0, 2, 1])).

example(before([0, 0, 3, 3]), instr(1, 0, 1, 2), after([0, 0, 1, 3])).

example(before([0, 2, 2, 1]), instr(5, 3, 2, 2), after([0, 2, 3, 1])).

example(before([3, 1, 0, 1]), instr(4, 3, 1, 3), after([3, 1, 0, 2])).

example(before([2, 2, 2, 1]), instr(15, 0, 2), after([2, 2, 2, 4])).

example(before([0, 1, 1, 0]), instr(4, 1, 1, 3), after([0, 1, 1, 2])).

example(before([2, 2, 3, 3]), instr(0, 1, 2, 0), after([4, 2, 3, 3])).

example(before([1, 0, 1, 2]), instr(14, 1, 2), after([1, 0, 1, 1])).

example(before([3, 3, 1, 1]), instr(6, 1, 3, 1), after([3, 9, 1, 1])).

example(before([1, 2, 2, 1]), instr(15, 1, 2), after([1, 4, 2, 1])).

example(before([1, 1, 2, 3]), instr(4, 3, 2, 1), after([1, 5, 2, 3])).

example(before([1, 0, 2, 3]), instr(13, 1, 0), after([1, 0, 2, 3])).

example(before([0, 1, 0, 2]), instr(8, 0, 0, 1), after([0, 0, 0, 2])).

example(before([0, 1, 2, 1]), instr(10, 1, 0), after([0, 1, 1, 1])).

example(before([3, 0, 3, 1]), instr(7, 3, 1, 2), after([3, 0, 1, 1])).

example(before([2, 2, 3, 3]), instr(11, 0, 3), after([2, 2, 0, 3])).

example(before([3, 0, 1, 0]), instr(14, 1, 2), after([3, 0, 1, 1])).

example(before([0, 0, 1, 2]), instr(12, 2, 3), after([3, 0, 1, 2])).

example(before([0, 0, 1, 1]), instr(1, 0, 1, 2), after([0, 0, 1, 1])).

example(before([1, 2, 0, 3]), instr(9, 1, 0, 3), after([1, 2, 0, 1])).

example(before([0, 1, 3, 0]), instr(10, 1, 0), after([0, 1, 3, 1])).

example(before([2, 2, 1, 3]), instr(2, 2, 0, 2), after([2, 2, 3, 3])).

example(before([0, 3, 3, 0]), instr(8, 0, 0, 0), after([0, 3, 3, 0])).

example(before([1, 2, 0, 1]), instr(9, 1, 0, 2), after([1, 2, 1, 1])).

example(before([0, 1, 1, 3]), instr(10, 1, 0), after([0, 1, 1, 3])).

example(before([2, 1, 1, 3]), instr(11, 0, 3), after([2, 1, 1, 0])).

example(before([3, 0, 1, 1]), instr(7, 3, 1, 3), after([3, 0, 1, 1])).

example(before([1, 0, 2, 2]), instr(13, 1, 0), after([1, 0, 1, 2])).

example(before([3, 2, 1, 2]), instr(6, 0, 3, 0), after([9, 2, 1, 2])).

example(before([0, 2, 3, 0]), instr(2, 0, 1, 3), after([0, 2, 3, 2])).

example(before([2, 2, 1, 2]), instr(6, 3, 3, 1), after([2, 6, 1, 2])).

example(before([0, 1, 1, 2]), instr(5, 3, 1, 1), after([0, 3, 1, 2])).

example(before([1, 2, 2, 1]), instr(6, 1, 3, 3), after([1, 2, 2, 6])).

example(before([1, 2, 0, 2]), instr(9, 1, 0, 1), after([1, 1, 0, 2])).

example(before([1, 0, 3, 1]), instr(4, 2, 2, 2), after([1, 0, 6, 1])).

example(before([1, 0, 2, 2]), instr(13, 1, 0), after([1, 1, 2, 2])).

example(before([0, 1, 2, 3]), instr(10, 1, 0), after([0, 1, 2, 3])).

example(before([1, 2, 0, 2]), instr(9, 1, 0, 2), after([1, 2, 1, 2])).

example(before([3, 2, 2, 2]), instr(15, 3, 2), after([3, 2, 4, 2])).

example(before([3, 2, 0, 3]), instr(3, 1, 0, 2), after([3, 2, 1, 3])).

example(before([2, 1, 3, 1]), instr(5, 3, 2, 2), after([2, 1, 3, 1])).

example(before([1, 0, 3, 1]), instr(13, 1, 0), after([1, 0, 3, 1])).

example(before([1, 0, 2, 0]), instr(13, 1, 0), after([1, 0, 2, 0])).

example(before([0, 0, 1, 3]), instr(7, 2, 1, 1), after([0, 1, 1, 3])).

example(before([1, 0, 3, 0]), instr(13, 1, 0), after([1, 0, 1, 0])).

example(before([0, 2, 1, 0]), instr(8, 0, 0, 1), after([0, 0, 1, 0])).

example(before([0, 0, 1, 2]), instr(0, 3, 2, 2), after([0, 0, 4, 2])).

example(before([0, 0, 3, 3]), instr(1, 0, 1, 3), after([0, 0, 3, 1])).

example(before([2, 3, 0, 2]), instr(6, 1, 2, 2), after([2, 3, 6, 2])).

example(before([3, 1, 2, 2]), instr(4, 0, 2, 1), after([3, 5, 2, 2])).

example(before([3, 2, 3, 0]), instr(2, 3, 1, 1), after([3, 2, 3, 0])).

example(before([1, 0, 2, 1]), instr(7, 0, 1, 1), after([1, 1, 2, 1])).

example(before([0, 0, 2, 1]), instr(6, 2, 3, 1), after([0, 6, 2, 1])).
