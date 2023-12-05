% na parte recursiva vai retirando a head da lista até essa
% mesma ser X (caso terminal)
membro(X, [X|_]). % caso terminal
membro(X, [_|R]) :- membro(X, R). % parte recursiva


%my_list([1, 2, 3, 4, 5]).
is_in_list(Number, List) :-
    %my_list(List),
    member(Number, List).