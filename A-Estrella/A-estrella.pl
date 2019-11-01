% //////////////////////////////////////////////////////////////////////////
% //////////////////////// A*

:-dynamic enFrontera/1, enVisitados/1.

% Representaciones:
%   nodo(Estado, ListaCamino, CostoG, CostoF). CostoF = CostoG(N) + h(N)
%   enFrontera(Nodo).
%   enVisitados(Nodo).

buscar(Estado, Solucion) :- 
    retractall(enFrontera(_)),
    retractall(enVisitados(_)),
    h(Estado, CostoH),
    assertz(enFrontera(nodo(Estado,[], 0, CostoH))), 
    buscarA(Solucion).

% Caso base
buscarA(Solucion) :- 
    seleccionarMejorEnFrontera(Nodo),
    esMeta(Nodo), !,
    Nodo = nodo(Estado, Camino, _CostoG, CostoF),
    reverse([Estado|Camino], SolucionAux),
    Solucion = [SolucionAux, CostoF].
    
% Caso recursivo
buscarA(Solucion) :-
    seleccionarMejorEnFrontera(Nodo),
    assertz(enVisitados(Nodo)), % Se agrega el nodo a visitados
    retract(enFrontera(Nodo)), % Se elimina el nodo de la frontera
    generarVecinos(Nodo, Vecinos),
    agregarVecinos(Vecinos),
    buscarA(Solucion). 

generarVecinos(Nodo, Vecinos) :-
    findall(NodoVecino, nodoVecino(Nodo, NodoVecino), Vecinos).

nodoVecino(nodo(EPadre, CaminoPadre, CostoG, _CostoF), NodoVecino) :-
    estadoSuc(EPadre, EVecino, CostoStep), % backtracking point
    h(EVecino, CostoH),
    CostoGNuevo is CostoG + CostoStep,
    CostoFNueva is CostoGNuevo + CostoH,
    NodoVecino = nodo(EVecino, [EPadre|CaminoPadre], CostoGNuevo, CostoFNueva).


agregarVecinos([]).
agregarVecinos([NodoVecino|T]) :-
    % El nuevo nodo vecino solo sera agregado si supera los siguientes controles
    superaControlDeVisitados(NodoVecino),
    superaControlDeFrontera(NodoVecino), !,
    assertz(enFrontera(NodoVecino)),
    agregarVecinos(T).
agregarVecinos([_|T]) :- agregarVecinos(T). % Cuando no supera los controles

superaControlDeVisitados(nodo(Estado, _Camino, _CostoG, _CostoF)) :- not(enVisitados(nodo(Estado,_,_,_))), !.
superaControlDeVisitados(nodo(Estado, _Camino, _CostoG, CostoF)) :-
    enVisitados(nodo(Estado, _, _, CostoFV)), CostoFV > CostoF,
    retract(enVisitados(nodo(Estado, _, _, CostoFV))). 

superaControlDeFrontera(nodo(Estado, _Camino, _CostoG, _CostoF)) :- not(enFrontera(nodo(Estado,_,_,_))), !.
superaControlDeFrontera(nodo(Estado, _Camino, _CostoG, CostoF)) :-
    enFrontera(nodo(Estado, _, _, CostoFV)), CostoFV > CostoF,
    retract(enFrontera(nodo(Estado, _, _, CostoFV))). 

    
estadoSuc(X, Y, Costo) :- estado_suc(X, Y, Costo).
estadoSuc(X, Y, Costo) :- estado_suc(Y, X, Costo).

estado_suc(a, c, 20).
estado_suc(a, b, 10).
estado_suc(a, d, 15).
estado_suc(c, b, 5).
estado_suc(d, b, 5).
estado_suc(d, f, 5).
estado_suc(b, g, 15).
estado_suc(b, e, 5).
estado_suc(e, g, 2).
estado_suc(f, e, 10).

h(a, 20).
h(b, 10).
h(c, 15).
h(d, 7).
h(e, 5).
h(f, 0).
h(g, 0).

esMeta(nodo(f, _, _, _)).
esMeta(nodo(g, _, _, _)).

% esMeta(nodo(z, _, _, _)).

% ////////////////////////////////////////////////////////////////////////////////////////
% //////////////////////// Auxiliares

seleccionarMejorEnFrontera(Nodo) :-
    enFrontera(Nodo),
    Nodo = nodo(_, _, _, MenorCosto),
    forall(enFrontera(nodo(_, _, _, Costo)), Costo >= MenorCosto), !.

my_write(Text, Variable) :-
    write(Text), writeln(Variable).
