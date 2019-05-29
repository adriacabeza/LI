%C1
respuesta(C,R,E,D):-
    findall(I, (nth1(I,C,X), nth1(I,R,X)), L1), length(L1,E),
    findall(I-J, (nth1(I,C,X), nth1(J,R,X), I \= J, not(member(I,L1)), not(member(J,L1))), L2), length(L2,D),!.

%respuesta(C,R,E,D):- iguals(C,R,E), esMembre(C,R,D1), D is D1-E, !.

%iguals([C],[R],1):- C == R.
%iguals([C],[R],0):- C \= R.
%iguals([C1|C],[R1|R],N):- C1 = R1, iguals(C,R,N1), N is N1+1.
%iguals([C1|C],[R1|R],N):- C1 \= R1, iguals(C,R,N).

%esMembre([C],R,1):- member(C,R).
%esMembre([C],R,0):- not(member(C,R)).
%esMembre([C1|C],R,N):- member(C1,R), select(C1,R,R1), esMembre(C,R1,N1), N is N1+1.
%esMembre([C1|C],R,N):- not(member(C1,R)), esMembre(C,R,N).

%C2

intentos([ [ [r,a,v,l], [1,1] ], [ [m,n,v,l], [1,0] ], [ [v,l,v,l], [0,0] ], [ [r,a,m,m], [1,1] ], [ [r,n,a,n], [2,2] ]]).


nuevoInento(A):- secuenciasPossibles(A), intentos(L), intento(A,L).

intento(R,[[C,[E,D]]]):- respuesta(R,C,E,D), !.
intento(R,[[C,[E,D]]|L]):- respuesta(R,C,E,D), intento(R,L).

secuenciasPossibles(R):- L = [r,a,v,l,n,m], member(P,L), member(S,L), member(T,L), member(Q,L), R = [P,S,T,Q]. 
