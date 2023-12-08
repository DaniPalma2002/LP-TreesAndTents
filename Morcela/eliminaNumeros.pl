% ?- eliminaNumeros([1, o, 2, 4, l, 1, a], ListaSemNumeros).
% ListaSemNumeros = [o,l,a].

% ListaSemNumeros = [o,l,a]

eliminaNumeros([], []).
eliminaNumeros([H | T], [H | T1]) :-
    not(number(H)),
    eliminaNumeros(T, T1).
eliminaNumeros([_ | T] , Res) :-
    eliminaNumeros(T, Res).


% functional
exclude_numbers(List, Result) :-
    exclude(number, List, Result).

