camino( E,E, C,C ).
camino( EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ):-
    unPaso( EstadoActual, EstSiguiente ),
    \+member(EstSiguiente,CaminoHastaAhora),
    camino( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal).

nat(0).
nat(N):- nat(N1), N is N1 + 1.
    
solucionOptima:-
    nat(N),
    camino([0,0],[0,4],[[0,0]],C), 
    length(C,N),
    reverse(C,I),
    write(I).

unPaso([_,C2], [5,C2]).
unPaso([C1,_], [C1,8]).
unPaso([_,C2], [0,C2]).
unPaso([C1,_], [C1,0]).
unPaso([C1,C2], [5,C22]):- C2+C1 >= 5, C22 is C2-5+C1.
unPaso([C1,C2], [C11,0]):- C2+C1 < 5, C11 is C2+C1.
unPaso([C1,C2], [C11,8]):- C2+C1 >= 8, C11 is C2+C1-8.
unPaso([C1,C2], [0,C22]):- C2+C1 < 8, C22 is C2+C1.
