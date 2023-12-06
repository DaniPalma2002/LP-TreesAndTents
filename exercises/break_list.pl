% ?- break([1,2,3,4,5,6],3, L1, L2).
% L1 = [1, 2],
% L2 = [3, 4, 5, 6]
break([], _, [], []).
break([P | R], N, [P | R1], L2) :- 
    P < N,
    break(R, N, R1, L2).
break([P | R], N, L1, [P | R2]) :- 
    P >= N,
    break(R, N, L1, R2).