% processo recursivo
multByRec([], _, []). % multiplying a empty list results in a empty list
multByRec([Head | Tail], N, [Result | NewTail]) :- 
    Result is Head * N,
    multByRec(Tail, N, NewTail).


% processo iterativo
multByIter(_, [], []).
multByIter(N, [Head|Tail], [Result|NewTail]) :-
    multByIter_i(N, [Head|Tail], [], [Result|NewTail]).

multByIter_i(_, [], RevAcc, RevAcc).
multByIter_i(N, [Head|Tail], Acc, Result) :-
    NewResult is Head * N,
    multByIter_i(N, Tail, [NewResult|Acc], Result).


% processo funcional
multiply_by_n(Element, N, Result) :-
    Result is Element * N.

multByFunc(N, List, Result) :-
    maplist(multiply_by_n(N), List, Result).

