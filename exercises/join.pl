join([], L, L).
join([Head | Tail], L1, [Head | L2]) :-
    join(Tail, L1, L2).