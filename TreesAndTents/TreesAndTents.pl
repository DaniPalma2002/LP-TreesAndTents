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
% dado a linha e a coluna, devolve o valor dessas coordenadas
obtem_objeto(Tabuleiro, (L, C), Objeto) :-
    tamanho_tabuleiro(Tabuleiro, Linhas, Colunas),
    ((L < 1; C < 1; L > Linhas; C > Colunas) -> 
        true;
        nth1(L, Tabuleiro, Linha), nth1(C, Linha, Objeto)
    ).

% nao_tem_objeto(Tabuleiro, Obj, (L, C))
% nessas coordenadas nao tem o objeto Obj
nao_tem_objeto(Tabuleiro, Obj, (L, C)) :-
    celula_valida(Tabuleiro, (L, C)),
    obtem_objeto(Tabuleiro, (L, C), O),
    not((O == Obj) ; var(Obj)).

% nao_tem_var(Tabuleiro, Obj, (L, C))
% nessas coordenadas nao esta vazio
nao_tem_var(Tabuleiro, (L, C)) :-
    celula_valida(Tabuleiro, (L, C)),
    obtem_objeto(Tabuleiro, (L, C), Obj),
    not(var(Obj)).


% numero_obj_lista(Objeto, Lista, N)
% devolve o numero de objetos (Objeto) da lista
numero_obj_lista(_, [], 0).
numero_obj_lista(Obj, [P | R], N) :-
    numero_obj_lista(Obj, R, N1),
    ((var(Obj), var(P); Obj == P, not(var(P))) -> 
        N is N1 + 1; 
        N is N1
    ),
    !.

% troca_elemento(Lista, Indice, Obj, NovaLista)
% muda o valor da lista que esta no indice dado
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
    writeln('TABULEIRO:'),
    maplist(print_linha, Tabuleiro).
print_linha([]) :- nl.
print_linha([P | R]) :-
    print_valor(P),
    print_linha(R).
print_valor(Valor) :-
    (var(Valor) -> write('_'); write(Valor)),
    write('  ').

% print_puzzle
print_puzzle((Tabuleiro, TendasPLinha, TendasPColuna)) :-
    writeln('PUZZLE: ============'),
    write('   '), maplist(print_valor, TendasPColuna), nl,
    print_linha2(Tabuleiro, TendasPLinha).
print_linha2([], _) :- nl.
print_linha2([PT | RT], [P | R]) :-
    write(P), write('  '),
    print_linha(PT),
    print_linha2(RT, R).

% indice_mesmo_valor(L1, L2, Indices)
% dadas duas listas, devolve os indices em que elas tem o mesmo valor
indices_mesmo_valor(L1, L2, Indices) :-
    findall(I, 
        (nth1(I, L1, Value), nth1(I, L2, Value)), 
        Indices
    ).

adiciona_valores(X, Y, Res) :-
    Res is X + Y.

% celula_valida(Tabuleiro, (L, C))
celula_valida(Tabuleiro, (L, C)) :-
    tamanho_tabuleiro(Tabuleiro, Linhas, Colunas),
    number(L), number(C),
    L > 0, C > 0, L =< Linhas, C =< Colunas.

% Consultas ====================================================================

% vizinhanca((L , C), Vizinhanca)
vizinhanca((L, C), [(L1, C), (L, C1), (L, C2), (L2, C)]) :-
    L1 is L - 1,
    L2 is L + 1,
    C1 is C - 1,
    C2 is C + 1.

% devolve vizinhanca mas so as que sao vazias
vizinhanca_vazia(Tabuleiro, (L, C), Vizinhanca) :-
    vizinhanca((L, C), Viz),
    exclude(nao_tem_var(Tabuleiro), Viz, VizinhancaT),
    include(celula_valida(Tabuleiro), VizinhancaT, Vizinhanca).

% devolve vizinhanca mas so as que tem tendas
vizinhanca_tendas(Tabuleiro, (L, C), Vizinhanca) :-
    vizinhanca((L, C), Viz),
    exclude(nao_tem_objeto(Tabuleiro, t), Viz, VizinhancaT),
    include(celula_valida(Tabuleiro), VizinhancaT, Vizinhanca).


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
% devolve todas as celulas (L, C) que tem esse objeto
todasCelulas(Tabuleiro, Celulas, Objecto) :-
    not(var(Objecto)),
    todasCelulas(Tabuleiro, TodasCelulas),
    exclude(nao_tem_objeto(Tabuleiro, Objecto), TodasCelulas, Celulas).
todasCelulas(Tabuleiro, Celulas, Objecto) :-
    var(Objecto),
    todasCelulas(Tabuleiro, TodasCelulas),
    exclude(nao_tem_var(Tabuleiro), TodasCelulas, Celulas).


    
% calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto)
% devolve numero de objetos Obj em cada linha e em cada coluna
calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Obj) :-
    maplist(numero_obj_lista(Obj), Tabuleiro, ContagemLinhas),
    transpose(Tabuleiro, TransposeTab),
    maplist(numero_obj_lista(Obj), TransposeTab, ContagemColunas),
    !.
    

% celulaVazia(Tabuleiro, (L, C))
celulaVazia(Tabuleiro, (L, C)) :-
    obtem_objeto(Tabuleiro, (L, C), Obj),
    var(Obj).


% Insercao de tendas e relva ===================================================

% insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C))
insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C)) :-
    %write(L), writeln(C),
    nth1(L, Tabuleiro, Linha),
    troca_elemento(Linha, C, TendaOuRelva, NovaLinha),
    troca_elemento(Tabuleiro, L, NovaLinha, Tabuleiro).


% insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2))
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

% Estrategias ==================================================================

% relva(Puzzle)
% coloca relva em todas as linhas e colunas que ja tem asa tendas todas
relva((Tabuleiro, TendasPLinha, TendasPColuna)) :-
    calculaObjectosTabuleiro(Tabuleiro, CLinhas, CColunas, t), 
    indices_mesmo_valor(TendasPLinha, CLinhas, IndiceLinhas), 
    indices_mesmo_valor(TendasPColuna, CColunas, IndiceColunas), 
    insereObjectoEntrePosicoes_fill(Tabuleiro, IndiceLinhas, r),
    transpose(Tabuleiro, TabTranspose),
    insereObjectoEntrePosicoes_fill(TabTranspose, IndiceColunas, r),
    transpose(TabTranspose, Tabuleiro),
    !.
    

% inacessiveis(Tabuleiro)
% coloca relva na posicao x se x nao for uma arvore e nao tiver arvores na sua vizinhanca 
inacessiveis((Tabuleiro, _, _)) :-
    inacessiveis(Tabuleiro), !.
inacessiveis(Tabuleiro) :- 
    processa_linhas(Tabuleiro, (1, 1), Tabuleiro).
% AUX: processa_linhas(Tabuleiro, (L, C), Tabuleiro)
processa_linhas([], _, _).
processa_linhas([P | R], (L, C), Tabuleiro) :-
    processa_colunas(P, (L, C), Tabuleiro),
    L1 is L + 1,
    processa_linhas(R, (L1, 1), Tabuleiro).
% AUX: processa_colunas(Linha, (L, C), Tabuleiro) :-
processa_colunas([], _, _).
processa_colunas([Obj | R], (L, C), Tabuleiro) :-
    vizinhanca((L, C), Viz),
    maplist(obtem_objeto(Tabuleiro), Viz, ValoresViz),
    numero_obj_lista(a, ValoresViz, N),
    (((var(Obj); Obj \= a), N == 0) -> 
        insereObjectoCelula(Tabuleiro, r, (L, C)); 
        true
    ),
    C1 is C + 1,
    processa_colunas(R, (L, C1), Tabuleiro).

    
% aproveita(Puzzle)
% coloca tendas nas linhas e colunas em que os espacos vazios mais as tendas ja 
% colocadas e igual ao numero de tendas para colocar no total da linha/coluna 
aproveita((Tabuleiro, TendasPLinha, TendasPColuna)) :-
    aproveita_i(Tabuleiro, TendasPLinha),
    transpose(Tabuleiro, TabTranspose),
    aproveita_i(TabTranspose, TendasPColuna),
    transpose(TabTranspose, Tabuleiro).

aproveita_i(Tabuleiro, TendasPLinha) :-
    calculaObjectosTabuleiro(Tabuleiro, CLinhasTenda, _, t),
    calculaObjectosTabuleiro(Tabuleiro, CLinhasVazio, _, _),
    maplist(adiciona_valores, CLinhasVazio, CLinhasTenda, CLinhas),
    indices_mesmo_valor(TendasPLinha, CLinhas, IndiceLinhas),
    insereObjectoEntrePosicoes_fill(Tabuleiro, IndiceLinhas, t),
    !.
    

% limpaVizinhancas(Puzzle)
% coloca relva em todas as posicoes a volta de uma tenda (com diagonais)
limpaVizinhancas((Tabuleiro, _, _)) :-
    todasCelulas(Tabuleiro, Celulas, t),
    preenche_vizinhanca_celulas(Tabuleiro, Celulas, r),
    !.

preenche_vizinhanca_celulas(_, [], _).
preenche_vizinhanca_celulas(Tabuleiro, [P | R], Obj) :-
    vizinhancaAlargada(P, VizAl),
    insereObjCelulas(Tabuleiro, Obj, VizAl),
    preenche_vizinhanca_celulas(Tabuleiro, R, Obj).

insereObjCelulas(_, _, []).
insereObjCelulas(Tabuleiro, Obj, [(L, C) | RC]) :-
    (celula_valida(Tabuleiro, (L, C)) -> 
        insereObjectoCelula(Tabuleiro, Obj, (L, C));
        true
    ),
    insereObjCelulas(Tabuleiro, Obj, RC).


% unicaHipotese(Puzzle)
unicaHipotese((Tabuleiro, _, _)) :-
    todasCelulas(Tabuleiro, Celulas, a),
    preenche_vizinhanca_arvores(Tabuleiro, Celulas, t),
    !.

preenche_vizinhanca_arvores(_, [], _).
preenche_vizinhanca_arvores(Tabuleiro, [P | R], Obj) :-
    vizinhanca_vazia(Tabuleiro, P, Viz),
    vizinhanca_tendas(Tabuleiro, P, Tendas),
    length(Viz, N),
    length(Tendas, Nt),
    ((N == 1, Nt == 0) ->
        insereObjCelulas(Tabuleiro, Obj, Viz);
        true
    ),
    preenche_vizinhanca_arvores(Tabuleiro, R, Obj).




% arv_tem_tenda(Arv, Ten)
arv_tem_tenda((La, Ca), (Lt, Ct)) :-
    abs(La - Lt, DifL),
    abs(Ca - Ct, DifC),
    DifL =< 1,
    DifC =< 1,
    (La == Lt; Ca == Ct).

% valida(LArv, LTen)
% vefifica se tem relacao 1 para 1 entre tendas e arvores
valida(LArv, LTen) :- % # TODO
    length(LArv, Na),
    length(LTen, Nt),
    Na == Nt.


% resolve(Puzzle)
resolve(Puzzle) :- 
    resolve(Puzzle, 0).

resolve((T, TpL, TpC), Cont) :-
    Cont < 100,
    todasCelulas(T, Arv, a),
    todasCelulas(T, Ten, t),
    not(valida(Arv, Ten)),

    relva((T, TpL, TpC)),
    inacessiveis((T, TpL, TpC)),
    aproveita((T, TpL, TpC)),
    limpaVizinhancas((T, TpL, TpC)),
    unicaHipotese((T, TpL, TpC)),

    Cont1 is Cont + 1,
    resolve((T, TpL, TpC), Cont1), !.

resolve((T, _, _), _) :-
    todasCelulas(T, Arv, a),
    todasCelulas(T, Ten, t),
    valida(Arv, Ten),
    !.
