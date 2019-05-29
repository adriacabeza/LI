camino( E,E, C,C ).
camino( EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ):-
    unPaso( EstadoActual, EstSiguiente ),
    \+member(EstSiguiente,CaminoHastaAhora),
    camino( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal).

nat(0).
nat(N):- nat(N1), N is N1 + 1.
    
aigua:-
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


%//////////////////////////////////////////////////////////////////////////

camino( E,E, C,C ,_).
camino( EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal, N):-
    length(CaminoHastaAhora,X), X < N,
    unPaso2( EstadoActual, EstSiguiente ),
    \+member(EstSiguiente,CaminoHastaAhora),
    camino( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal, N).
    
tamanoTablero(8).
ok(F,C):- tamanoTablero(N), F >= 1, F =< N, C >= 1, C =< N.


caballo(P,FI,CI,FF,CF):-
    N is P + 1,
    camino([FI,CI],[FF,CF],[[FI,CI]],C,N),
    reverse(C,S),
    write(S).
    
unPaso2([FI,CI],[FF,CF]):- FF is FI + 2, CF is CI - 1, ok(FF,CF).
unPaso2([FI,CI],[FF,CF]):- FF is FI + 2, CF is CI + 1, ok(FF,CF).  
unPaso2([FI,CI],[FF,CF]):- FF is FI - 2, CF is CI - 1, ok(FF,CF).
unPaso2([FI,CI],[FF,CF]):- FF is FI - 2, CF is CI + 1, ok(FF,CF).
unPaso2([FI,CI],[FF,CF]):- FF is FI - 1, CF is CI + 2, ok(FF,CF).
unPaso2([FI,CI],[FF,CF]):- FF is FI + 1, CF is CI + 2, ok(FF,CF).
unPaso2([FI,CI],[FF,CF]):- FF is FI - 1, CF is CI - 2, ok(FF,CF).
unPaso2([FI,CI],[FF,CF]):- FF is FI + 1, CF is CI - 2, ok(FF,CF).

%/////////////////////////////////////////////////////////////////////////

programa(L):- append([begin],X,S), append(S,[end],L), instrucciones(X), !.

instrucciones(X):- instruccio(X).
instrucciones(X):- append(L,M,X), append(S,[;],L), instruccio(S), instrucciones(M).

instruccio([X,=,Y,+,Z]):- variable(X), variable(Y), variable(Z).
instruccio([if,X,=,Y,then|SS]):- append(S,L1,SS), append(else,L2,L1), append(L,endif,L2), varible(X), varible(Y), 
instrucciones(S), instrucciones(L).

variable(x).
variable(y).
variable(z).

%////////////////////////////////////////////////////////////////////////////////

respuesta(C,R,E,D):-
    findall(I, (nth1(I,C,X), nth1(I,R,X)), L1), length(L1,E),
    findall(I-J, (nth1(I,C,X), nth1(J,R,X), I \= J, not(member(I,L1)), not(member(J,L1))), L2), length(L2,D),!.

intentos([ [ [r,a,v,l], [1,1] ], [ [m,n,v,l], [1,0] ], [ [v,l,v,l], [0,0] ], [ [r,a,m,m], [1,1] ], [ [r,n,a,n], [2,2] ]]).


nuevoInento(A):- secuenciasPossibles(A), intentos(L), intento(A,L).

intento(R,[[C,[E,D]]]):- respuesta(R,C,E,D), !.
intento(R,[[C,[E,D]]|L]):- respuesta(R,C,E,D), intento(R,L).

secuenciasPossibles(R):- L = [r,a,v,l,n,m], member(P,L), member(S,L), member(T,L), member(Q,L), R = [P,S,T,Q]. 


