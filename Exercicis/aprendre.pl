pert(X,[X|_]).
pert(X,[_|L]):- pert(X,L).

concat([]|,L,L).
concat([X|L1],L2,[X|L3]):-concat(L1,L2,L3).
% pert(X,L):- concat(_,[X|_],L). AIXO NO HO VEIG HO HE DE PROVAR


fact(0,1).
fact(X,F):- X1 is X-1, fact(X1,F1), F is X*F1.


nat(0).
nat(N):- nat(N1), N is N1 +1. %pq polles no és N1 is N +1


%mcm(X,Y,M):- nat(M), M>0, 0 is M mod X, 0 is M mod Y.
%versió eficient
mcm(X,Y.M):- nat(M), N>0, M is N*X, 0 is M mod Y


pert_con_resto(X,L,Resto):- concat(L1,[X|L2],L), concat(L1,L2,Resto).

%longitud d'una llista
long([],0).
long([_|L],M): long(L,N), M is N+1.



factores_primos(1,[]):- !.
factores_primos(N,[F|L):- nat(F), F>1, 0 is N mod F,
N1 is N // F, factores_primos(N1,L),!.


%aquesta no me l'he mirat
permutacion([],[]).
permutacion(L,[X|P]) :- pert_con_resto(X,L,R), permutacion(R,P).

subcjto([],[]). %subconjunto(L,S) comprueba si S es subconjunto de L
subcjto([X|C],[X|S]):-subcjto(C,S).
subcjto([_|C],S):-subjto(C,S).


cifras(L,N):- subcjto(L,S), permutacion(S,P), expresion(P,E), N is E, write(E), nl,fail.

expresion([X],X).
expresion(L,E1+E2):- concat(L1,L2,L), L1\=[],L2\=[], expresion(L1,E1), expresion(L2,E2).
expresion(L,E1-E2):- concat(L1,L2,L), L1\=[],L1\=[], expresion(L1,E1), expresion(L2,E2).


:
