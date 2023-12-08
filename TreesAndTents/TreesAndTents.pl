% Daniel Pereira 99194
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ['puzzlesAcampar.pl']. % Ficheiro dado. No Mooshak tera mais puzzles.

% Auxiliares ===================================================================
% obtem_objeto(Tabuleiro, (L, C), Objeto)
obtem_objeto(Tabuleiro, (L, C), Objeto) :-
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, Objeto).
    

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
    length(Tabuleiro, Linhas),
    nth0(0, Tabuleiro, C),
    length(C, Colunas),
    findall((Li, Co), (between(1, Linhas, Li), between(1, Colunas, Co)), TodasCelulas).

nao_tem_objeto(Tabuleiro, Obj, (A, B)) :-
    obtem_objeto(Tabuleiro, (A, B), O),
    not(O == Obj).

todasCelulas(Tabuleiro, TodasCelulas, Objecto) :-
    var(Objecto),
    todasCelulas(Tabuleiro, TodasCelulas).

todasCelulas(Tabuleiro, Celulas, Objecto) :-
    not(var(Objecto)),
    todasCelulas(Tabuleiro, TodasCelulas),
    exclude(nao_tem_objeto(Tabuleiro, Objecto), TodasCelulas, Celulas).
    
    

% calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto)

% celulaVazia(Tabuleiro, (L, C))

% Inserção de tendas e relva ===================================================