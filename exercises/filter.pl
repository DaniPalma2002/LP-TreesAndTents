filter([P | R], P, R).
filter([P | R], N, [P | S]) :- filter(R, N, S).


% removes all occurences (number, list, result)
filterAll(_, [], []).
filterAll(Element, [Element | Tail], NewList) :-
    filterAll(Element, Tail, NewList).
filterAll(Element, [Head | Tail], [Head | NewTail]) :-
    dif(Head, Element),
    filterAll(Element, Tail, NewTail).

