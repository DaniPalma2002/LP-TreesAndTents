escreve_lista([]).
escreve_lista([Head | Tail]) :-
    write(Head), nl,
    escreve_lista(Tail).


print_list(List) :-
    forall(member(X, List), (write('->'), write(X), nl)).
    
    
    