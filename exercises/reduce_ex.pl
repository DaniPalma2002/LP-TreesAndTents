sum_acc(Acc, Elem, NewAcc) :-
    NewAcc is Acc + Elem * 2.

sumList(List, Result) :-
    foldl(sum_acc, List, 0, Result).
    

