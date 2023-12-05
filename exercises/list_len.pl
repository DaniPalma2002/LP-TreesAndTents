% recursive version
len([], 0).
len([_|R], Z) :- 
    len(R, C), 
    Z is C + 1.


% iterative version
lenIter(L, C) :- lenIter_i(L, 0, C).
lenIter_i([], Ac, Ac).
lenIter_i([_|R], Ac, C) :- 
    Ac_N is Ac+1,
    lenIter_i(R, Ac_N, C).
