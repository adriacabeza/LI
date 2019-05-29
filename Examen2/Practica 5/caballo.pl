betweenchk(X, L, R) :- L =< X, X =< R.

unPaso(N, [Px, Py], [Pxp, Pyp]) :-
    member([SaltoX, SaltoY], [[1,2],[1,-2],[-1,2],[-1,-2],[2,1],[2,-1],[-2,1],[-2,-1]]),

    Pxp is Px + SaltoX,
    betweenchk(Pxp, 1, N),

    Pyp is Py + SaltoY,
    betweenchk(Pyp, 1, N).

camino(_,_, E,E, C,C).
camino(N, L, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal) :-
    length(CaminoHastaAhora, X), X < L,
    unPaso(N, EstadoActual, EstSiguiente),
    \+member(EstSiguiente, CaminoHastaAhora),
    camino(N,L, EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal).

movimientosCaballo(N, Pi, Pf, P) :-
    L is P + 1, % El estado inicial no cuenta como paso
    camino(N, L, Pi, Pf, [Pi], C),
    reverse(C, I),
    write(I).
