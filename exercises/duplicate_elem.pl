% iterative
duplElemIter(List, Res) :- duplElemIter_i(List, Res, []).

duplElemIter_i([], Res, Res).
duplElemIter_i([Head | Tail], Res, Acc) :-
    append(Acc, [Head, Head], NewAcc),
    duplElemIter_i(Tail, Res, NewAcc).

% functional
dupl(Head, [Head, Head]).
duplElemFunc(List, Res) :- 
    maplist(dupl, List, Res).


% recursive
duplRec([], []).
duplRec([Head | Tail], [Head, Head | DuplTail]) :-
    duplRec(Tail, DuplTail).