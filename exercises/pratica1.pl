% printList(List)
printList([]).
printList([Head | Tail]) :-
    writeln(Head),
    printList(Tail).


% multByXRec (List, N, Res)
multByXRec([], _, []).
multByXRec([Head | Tail], N, [NewHead | TailRes]) :-
    NewHead is Head * N,
    multByXRec(Tail, N, TailRes).

% multByXIter (List, N, Res)
multByXIter(List, N, Res) :- multByXIter_i(List, N, [], Res).
multByXIter_i([], _, Res, Res).
multByIter_i([Head | Tail], N, Acc, L) :-
    NewHead is Head * N,
    append(Acc, [NewHead], Acc1),
    multByIter_i(Tail, N, Acc1, Res).


is_even(N) :- 
    0 is N mod 2.

filterEvenList([], []).
filterEvenList([P | R], [P | R1]) :-
    is_even(P), 
    filterEvenList(R, R1).
filterEvenList([P | R], Res) :- 
    not(is_even(P)),
    filterEvenList(R, Res).