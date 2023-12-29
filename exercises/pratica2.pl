% NAO FUNCIONA
insereOrd(E, [], [E]).
insereOrd(E, [P | R], [E, P | R]) :- E < P.
insereOrd(E, [P | R], T) :-
    E >= P,
    insereOrd(E, R, [P | T]).


insereOrdFunc(E, L1, L2) :-
    findall(X, (member(X, L1), X > 5), Maiores),
    append(Menores, Maiores, L1),
    append([Menores, [E], Maiores], L2).