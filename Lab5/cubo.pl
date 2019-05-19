camino(E,E,C,C). 
camino(EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal):-
	unPaso(EstadoActual, EstSiguiente), write(EstadoActual),nl,
	\+member(EstSiguiente,CaminoHastaAhora),
	camino( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal ).

unPaso([C1,C2],[C1n,C2n]):- Aux is (5-C1), Dif is min(Aux,C2),  C1n is (C1+Dif),  C2n is (C2-Dif). %emplenar el cubo 1 a partir del 2
unPaso([C1,C2],[C1n,C2n]):- Aux is (8-C2), Dif is min(Aux,C1),  C1n is (C1-Dif), C2n is (C2+Dif). %emplenar el cubo 2 a partir del 1



unPaso([C1,C2],[0,C2]):- C1 > 0.%buidar cubo1 
unPaso([C1,C2],[C1,0]):- C2 > 0. %buidar cubo2



unPaso([C1,C2],[5, C2]):- C1 < 5. %emplenar el cubo 1
unPaso([C1,C2], [C1, 8]):- C2 < 8. %emplenar el cubo 2



nat(0).
nat(N):- nat(N1), N is N1+1.



solucionOptima:-
	nat(N),                        % Buscamos solución de "coste" 0; si no, de 1, etc.
	camino([0,0],[0,4],[[0,0]],C), % En "hacer aguas": -un estado es [cubo5,cubo8], y
	length(C,N),                   % -el coste es la longitud de C.
	reverse(C,C1),write(C1),halt.
