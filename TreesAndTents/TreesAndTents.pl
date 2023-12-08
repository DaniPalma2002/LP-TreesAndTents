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

% numero_obj_lista(Lista, Objeto, N)
numero_obj_lista(_, [], 0).
numero_obj_lista(Obj, [P | R], N) :-
    numero_obj_lista(Obj, R, N1),
    ((var(Obj), var(P); Obj == P, not(var(P))) -> 
        N is N1 + 1; 
        N is N1
    ).

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
% insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C)) :-
%     insereObjectoCelula_i(Tabuleiro, TendaOuRelva, (L, C), [], (L_cur, C_curr)).

% insereObjectoCelula_i([], _, _, NewTabuleiro, _).
% insereObjectoCelula_i() :-




    