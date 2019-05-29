nat(0).
nat(N) :- nat(N1), N is N1 + 1.

unPaso([_, C8], [0, C8]). % vaciar Cubo5
unPaso([C5, _], [C5, 0]). % vaciar Cubo8

unPaso([_, C8], [5, C8]). % llenar Cubo5
unPaso([C5, _], [C5, 8]). % llenar Cubo8

unPaso([C5, C8], [C5p, C8p]) :- % verter un cubo en otro
    member([LR, CV, CR, CVp, CRp], [[8, C5, C8, C5p, C8p], [5, C8, C5, C8p, C5p]]),
    CRp is min(CR + CV, LR),
    CVp is CR + CV - CRp.

camino(E,E, C,C).
camino(EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal) :-
    unPaso(EstadoActual, EstSiguiente),
    \+member(EstSiguiente, CaminoHastaAhora),
    camino(EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal).

solucionOptima :-
    nat(N),
    camino([0,0], [0,4], [[0,0]], C),
    length(C, N),
    reverse(C, I),
    write(I).

hacerAguas :- solucionOptima.
