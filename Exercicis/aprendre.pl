pert(X,[X|_]).
pert(X,[_|L]):- pert(X,L).


fact(0,1).
fact(X,F):- X1 is X-1, fact(X1,F1), F is X*F1.


nat(0).
nat(N):- nat(N1), N is N1 +1. %pq polles no és N1 is N +1


%mcm(X,Y,M):- nat(M), M>0, 0 is M mod X, 0 is M mod Y.
%versió eficient
mcm(X,Y.M):- nat(M), N>0, M is N*X, 0 is M mod Y.


pert_con_resto(X,L,Resto):- concat(L1,[X|L2],L), concat(L1,L2,Resto).
pert_r(X,[X|L],L).
pert_r(X,[Y|L],[Y|R]):- pert_r(X,L,R). 


perm([],[]).
perm([X|L], P):- perm(L,P1), append(PA,PB,P1), append(PA, [X|PB], P).


permutacion([],[]).
permutacion(L,[X|P]) :- pert_con_resto(X,L,R), permutacion(R,P).

subcjto([],[]). %subconjunto(L,S) comprueba si S es subconjunto de L
subcjto([X|C],[X|S]):-subcjto(C,S).
subcjto([_|C],S):-subcjto(C,S).


cifras(L,N):- subcjto(L,S), permutacion(S,P), expresion(P,E), N is E, write(E), nl,fail.

expresion([X],X).
expresion(L,E1+E2):- concat(L1,L2,L), L1\=[],L2\=[], expresion(L1,E1), expresion(L2,E2).
expresion(L,E1-E2):- concat(L1,L2,L), L1\=[],L1\=[], expresion(L1,E1), expresion(L2,E2).


concat([],L,L).
concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).
% pert(X,L):- concat(_,[X|_],L). AIXO NO HO VEIG HO HE DE PROVAR


factores_primos(1,[]):- !.
factores_primos(N,[F|L]):- nat(F), F>1, 0 is N mod F, N1 is N // F, factores_primos(N1,L),!.

%longitud d'una llista
long([],0).
long([_|L],M):- long(L,N), M is N+1.




%anem a continuar
%2. prod(L,P)
prod([],1). 
prod([L1|L],P):- prod(L,P2), P = P2* L1. 

%3.pescalar 
pescalar([],[],0).
pescalar([X|L1],[Y|L2],R):- pescalar(L1,L2,N), R is X*Y + N. 

%4. interseccion y unionFALTA INTERSECCIÓN

union([],L,L).
union([X|L1],L2,L3):- member(X,L2), !, union(L1,L2,L3).
union([X|L1],L2,[X|L3]):- union(L1,L2,L3).

%5. reverse and last PROVAR EL LAST AMB EL ULTIMO
last([X],X).
last([_|L],N):- last(L,N). 

ultimo(L,X):- concat(_,[X],L).

reverse([X],[X]).
reverse(L,[X|R]):- concat(L2,[X],L), reverse(L2,R).

%6. fib 
fib(1,1).
fib(2,1).
fib(N,F):- N > 2, N1 is N-1, N2 is N-2, fib(N1,F1), fib(N2,F2), F is F1+F2.

%7. dados
dados(0,0,[]).
dados(P,N,[X|L]) :- N>0, pert(X,[1,2,3,4,5,6]), Q is P-X, M is N-1, dados(Q,M,L). 
    
%aux suma 
suma([],0).
suma([X|L],S) :- suma(L,S1), S is S1+X.

%aux nse si funcionarà
count(_,[],0).
count(X,[X2|L],N):- X2 is X, count(X,L,N1), N is N1+1. 

%8. suma_demas(L)
suma_demas(L) :- pert_con_resto(X,L,R), suma(R,X), !. % si encontramos uno basta

%9 sumas_ants(L)
suma_ants(L) :-   concat(L1,[X|_],L),  suma(L1,X), !.

%10 card(L)
car([],[]).
car(X|L,[[X,N1]|Cr]):- car(L,C), pert_con_resto([X,N],C,Cr), N1 is N+1.
car(X|L,[[X,1]|C]):- car(L,C).


card(L) :- car(L,R), write(R).

%11 esta_ordenada(L)
esta_ordenada([]).
esta_ordenada([_]):- !.
esta_ordenada([X,Y|L]):- X =< Y, esta_ordenada([Y|L]).

%12 ordenacion
ordenacion(L1,L2):- permutacion(L1,L2), esta_ordenada(L2).

insercion(X,[],[X]).
insercion(X,[Y|L],[X,Y|L]) :- X=<Y. 
insercion(X,[Y|L],[Y|L1]) :- X>Y, insercion(X,L,L1). 

ord([],[]).
ord([X|L1],L2):- ord(L1,N), insercion(X,N,L2).

merge([],L1,L1) :- !.
merge(L1,[],L1).
merge([X|L1],[Y|L2],[X|L3]):- X =< Y,!, merge(L1,[Y|L2],L3).
merge([X|L1],[Y|L2],[Y|L3]):- merge([X|L1],L2,L3).
 
%comprovar el merge sort
merge_sort([],[])   :- !.
merge_sort([X],[X]) :- !. 
mergeSort(L1,L2):- concat(L11,L12,L1), mergeSort(L11,LF1), mergeSort(L12,LF2), merge(LF1,LF2,L2).


diccionario(A,N):- nmembers(A,N,L), escribir(L), fail.

nmembers(_,0,[]):-!.
nmembers(A,N,[X|L]):- nmembers(A,N1,L), pert(X,A), N1 is N-1.

escribir([]):-write(' '),nl,!.
escribir(X|L):- write(X), escribir(L).