% Daniel Pereira 99194
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ['puzzlesAcampar.pl']. % Ficheiro dado. No Mooshak tera mais puzzles.

% Auxiliares ===================================================================

% tamanho_tabuleiro(Tabuleiro, Linhas, Colunas)
tamanho_tabuleiro(Tabuleiro, Linhas, Colunas) :-
    length(Tabuleiro, Linhas),
    nth0(0, Tabuleiro, C),
    length(C, Colunas).

% obtem_objeto(Tabuleiro, (L, C), Objeto)
obtem_objeto(Tabuleiro, (L, C), Objeto) :-
    tamanho_tabuleiro(Tabuleiro, Linhas, Colunas),
    ((L < 1; C < 1; L > Linhas; C > Colunas) -> 
        true;
        nth1(L, Tabuleiro, Linha), nth1(C, Linha, Objeto)
    ).

% nao_tem_objeto(Tabuleiro, Obj, (L, C))
nao_tem_objeto(Tabuleiro, Obj, (L, C)) :-
    obtem_objeto(Tabuleiro, (L, C), O),
    not((O == Obj) ; var(Obj)).

% numero_obj_lista(Objeto, Lista, N)
numero_obj_lista(_, [], 0).
numero_obj_lista(Obj, [P | R], N) :-
    numero_obj_lista(Obj, R, N1),
    ((var(Obj), var(P); Obj == P, not(var(P))) -> 
        N is N1 + 1; 
        N is N1
    ),
    !.

% troca_elemento(Lista, Indice, Obj, NovaLista)
troca_elemento(Lista, Indice, Obj, NovaLista) :- 
    troca_elemento_i(Lista, Indice, Obj, NovaLista, 1), !.
troca_elemento_i([], _, _, [], _).
troca_elemento_i([_ | R], Indice, Obj, [Obj | R1], Cont) :-
    Cont =:= Indice,
    Cont1 is Cont + 1,
    troca_elemento_i(R, Indice, Obj, R1, Cont1).
troca_elemento_i([P | R], Indice, Obj, [P | R1], Cont) :-
    Cont1 is Cont + 1,
    troca_elemento_i(R, Indice, Obj, R1, Cont1).

% print_tabuleiro(Tabuleiro)
print_tabuleiro(Tabuleiro) :-
    writeln('Tabuleiro:'),
    maplist(writeln, Tabuleiro),
    writeln('--------').

% indice_mesmo_valor(L1, L2, Indice)
indices_mesmo_valor(L1, L2, Indices) :-
    findall(I, 
        (nth1(I, L1, Value), nth1(I, L2, Value)), 
        Indices
    ).

% Consultas ====================================================================

% vizinhanca((L , C), Vizinhanca)
vizinhanca((L, C), [(L1, C), (L, C1), (L, C2), (L2, C)]) :-
    L1 is L - 1,
    L2 is L + 1,
    C1 is C - 1,
    C2 is C + 1.
    
% vizinhancaAlargada((L , C), VizinhancaAlargada)
vizinhancaAlargada((L, C), [(L1, C1), (L1, C), (L1, C2),
                            (L, C1)          , (L, C2), 
                            (L2, C1), (L2, C), (L2, C2)]) :-
    L1 is L - 1,
    L2 is L + 1,
    C1 is C - 1,
    C2 is C + 1.


% todasCelulas(Tabuleiro, TodasCelulas, Objecto)
todasCelulas(Tabuleiro, TodasCelulas) :-
    tamanho_tabuleiro(Tabuleiro, Linhas, Colunas),
    findall((Li, Co), 
        (between(1, Linhas, Li), between(1, Colunas, Co)), 
        TodasCelulas
    ).

todasCelulas(Tabuleiro, Celulas, Objecto) :-
    todasCelulas(Tabuleiro, TodasCelulas),
    exclude(nao_tem_objeto(Tabuleiro, Objecto), TodasCelulas, Celulas).
    
    
% calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto)
calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Obj) :-
    maplist(numero_obj_lista(Obj), Tabuleiro, ContagemLinhas),
    transpose(Tabuleiro, TransposeTab),
    maplist(numero_obj_lista(Obj), TransposeTab, ContagemColunas),
    !.
    

% celulaVazia(Tabuleiro, (L, C))
celulaVazia(Tabuleiro, (L, C)) :-
    obtem_objeto(Tabuleiro, (L, C), Obj),
    var(Obj).


% Inserção de tendas e relva ===================================================

% insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C))
insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C)) :-
    nth1(L, Tabuleiro, Linha),
    troca_elemento(Linha, C, TendaOuRelva, NovaLinha),
    troca_elemento(Tabuleiro, L, NovaLinha, Tabuleiro).

% insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)) # TODO PRECISA DE SER APENAS NA MESMA LINHA??
insereObjectoEntrePosicoes(_, _, (_, C1), (_, C2)) :- C1 > C2, !.
insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)) :-
    C1 =< C2,
    insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C1)),
    NewC1 is C1 + 1,
    insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, NewC1), (L, C2)).
% AUX: insereObjectoEntrePosicoes_fill(Tabuleiro, IndiceLinhas, TendaOuRelva)
insereObjectoEntrePosicoes_fill(_, [], _).
insereObjectoEntrePosicoes_fill(Tabuleiro, [L | R], TendaOuRelva) :-
    length(Tabuleiro, Len),
    insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, 1), (L, Len)),
    insereObjectoEntrePosicoes_fill(Tabuleiro, R, TendaOuRelva).

% Estratégias ==================================================================

% relva(Puzzle)
relva((Tabuleiro, TendasPLinha, TendasPColuna)) :-
    calculaObjectosTabuleiro(Tabuleiro, CLinhas, CColunas, t), 
    indices_mesmo_valor(TendasPLinha, CLinhas, IndiceLinhas), 
    indices_mesmo_valor(TendasPColuna, CColunas, IndiceColunas), 
    writeln(TendasPLinha), writeln(CLinhas), writeln(IndiceLinhas), nl,
    writeln(TendasPColuna), writeln(CColunas), writeln(IndiceColunas), nl,
    insereObjectoEntrePosicoes_fill(Tabuleiro, IndiceLinhas, r),
    transpose(Tabuleiro, TabTranspose),
    insereObjectoEntrePosicoes_fill(TabTranspose, IndiceColunas, r),
    transpose(TabTranspose, Tabuleiro),
    print_tabuleiro(Tabuleiro),
    !.
    
    
    



    