
camino( E,E, C,C ,_).
camino( EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal, N):-
    length(CaminoHastaAhora,X), X < N,
    unPaso( EstadoActual, EstSiguiente ),
    \+member(EstSiguiente,CaminoHastaAhora),
    camino( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal, N).
    
tamanoTablero(8).
ok(F,C):- tamanoTablero(N), F >= 1, F =< N, C >= 1, C =< N.


solucionOptima(P,FI,CI,FF,CF):-
    N is P + 1,
    camino([FI,CI],[FF,CF],[[FI,CI]],C,N),
    reverse(C,S),
    write(S).
    
unPaso([FI,CI],[FF,CF]):- FF is FI + 2, CF is CI - 1, ok(FF,CF).
unPaso([FI,CI],[FF,CF]):- FF is FI + 2, CF is CI + 1, ok(FF,CF).  
unPaso([FI,CI],[FF,CF]):- FF is FI - 2, CF is CI - 1, ok(FF,CF).
unPaso([FI,CI],[FF,CF]):- FF is FI - 2, CF is CI + 1, ok(FF,CF).
unPaso([FI,CI],[FF,CF]):- FF is FI - 1, CF is CI + 2, ok(FF,CF).
unPaso([FI,CI],[FF,CF]):- FF is FI + 1, CF is CI + 2, ok(FF,CF).
unPaso([FI,CI],[FF,CF]):- FF is FI - 1, CF is CI - 2, ok(FF,CF).
unPaso([FI,CI],[FF,CF]):- FF is FI + 1, CF is CI - 2, ok(FF,CF).
