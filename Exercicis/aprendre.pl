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

%4. interseccion y union
interseccion([],_,[]).
interseccion([X|L1],[X|L2],[X|L]):- interseccion(L1,L2,L),!.
interseccion([_|L1],L2,L3):-interseccion(L1,L2,L3).


removeElement(X,L,NL):- concat(L1,[X|L2],L), concat(L1,L2,NL).
removeElement2(X,L,NL):- append(L1,[X|L2],L), append(L1,L2,NL).

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

palindromos2(L1):- permutacion(L1,L), reverse(L,R), L = R,  write(R), nl, fail.


palindromos(L) :- permutacion(L,P), es_palindromo(P), 
  write(P), nl, fail. 
palindromos(_). 

es_palindromo([]).
es_palindromo([_]) :- !. % regla adecuada
es_palindromo([X|L]) :- concat(L1,[X],L), es_palindromo(L1). 

palindroms(L) :- setof(P,(permutation(L,P), es_palindromo(P)),S), write(S). 


%19
suma([],[],[],C,C).
suma([X1|L1],[X2|L2],[X3|L3],Cin,Cout) :-
	X3 is (X1 + X2 + Cin) mod 10,
	C  is (X1 + X2 + Cin) //  10,
	suma(L1,L2,L3,C,Cout).


send_more_money1 :-

	L = [S, E, N, D, M, O, R, Y, _, _],
	permutacion(L, [0,1,2,3,4,5,6,7,8,9]),
	suma([D, N, E, S], [E, R, O, M], [Y, E, N, O], 0, M),

	write('S = '), write(S), nl,
	write('E = '), write(E), nl,
	write('N = '), write(N), nl,
	write('D = '), write(D), nl,
	write('M = '), write(M), nl,
	write('O = '), write(O), nl,
	write('R = '), write(R), nl,
	write('Y = '), write(Y), nl,
	write('  '), write([S,E,N,D]), nl,
	write('  '), write([M,O,R,E]), nl,
	write('-------------------'), nl,
	write([M,O,N,E,Y]), nl.


inverso([],[]).
inverso(L,[X|L1]):- concat(L2,[X],L), inverso(L2,L1).


%21
mis:- camino( [lado1,3,3], [lado2,0,0], [[lado1,3,3]] ).

camino(Fin,Fin,Cam):- inverso(Cam,Sol), write(Sol), nl.
camino(Ini,Fin,Cam):- paso(Ini,E), novisitado(E,Cam), camino(E,Fin,[E|Cam]).

novisitado(E,Cam):- pert(E,Cam), !,fail.
novisitado(_,_).

paso( [lado1,M1,C1], [lado2,M2,C2] ):- pasan(M,C), M2 is M1-M, C2 is C1-C, safe(M2,C2).
paso( [lado2,M1,C1], [lado1,M2,C2] ):- pasan(M,C), M2 is M1+M, C2 is C1+C, safe(M2,C2).

pasan(M,C):- member( [M,C], [ [0,1], [0,2], [1,0], [1,1], [2,0] ] ).
FALTA INTERSECCIÓN
safe(M,C):- M>=0, M=<3, C>=0, C=<3, nocomen( M, C),
            M1 is 3-M,  C1 is 3-C,  nocomen(M1,C1).

nocomen(0,_).
nocomen(M,C):- M>=C.

%ANNA MESTRES EXERCICIS

flatten([], []):- !.
flatten(X, [X]):- X\=[_|_].
flatten([L|List], FList):- flatten(L, L2), flatten(List, List2), append(L2, List2, FList).

flattenNoRepetitions(L, RL):- flatten(L, L2), noRepes(L2, RL).

noRepes([], []):- !.
noRepes([L|List], [L|Res]):- \+member(L, List), noRepes(List, Res).
noRepes([_|List], Res):- noRepes(List, Res).

/*
2. Tenemos una fila de cinco casas, con cinco vecinos con casas de colores diferentes, y cinco 
profesiones, animales, bebidas y nacionalidades diferentes, y sabiendo que:
    1 - El que vive en la casa roja es de Peru
    2 - Al frances le gusta el perro
    3 - El pintor es japones
    4 - Al chino le gusta el ron
    5 - El hungaro vive en la primera casa
    6 - Al de la casa verde le gusta el coñac
    7 - La casa verde esta a la izquierda de la blanca
    8 - El escultor cría caracoles
    9 - El de la casa amarilla es actor
   10 - El de la tercera casa bebe cava
   11 - El que vive al lado del actor tiene un caballo
   12 - El hungaro vive al lado de la casa azul
   13 - Al notario la gusta el whisky
   14 - El que vive al lado del medico tiene un ardilla,
Escribe un programa Prolog que averigue para cada persona todas sus 
caracteristicas de la forma [num_casa,color,profesion,animal,bebida,pais] 
averiguables. Ayuda: sigue el siguiente esquema: */

vecinoIzq(R1, R2):- R2 is R1 + 1.
vecinoDer(R1, R2):- R2 is R1 - 1.

vecino(R1, R2):- vecinoIzq(R1, R2).
vecino(R1, R2):- vecinoDer(R1, R2).

casas:- Sol = [[1, _, _, _, _, _],
               [2, _, _, _, _, _],
               [3, _, _, _, _, _],
               [4, _, _, _, _, _],
               [5, _, _, _, _, _]],
               member([_, roja, _, _, _, peru], Sol),
               member([_, _, _, perro, _, francia], Sol),
               member([_, _, pintor, _, _, japon], Sol),
               member([_, _, _, _, ron, china], Sol),
               member([1, _, _, _, _, hongria], Sol),
               member([_, verde, _, _, coñac, _], Sol),
               member([R1, verde, _, _, _, _], Sol),
               member([R2, blanca, _, _, _, _], Sol),
               vecinoIzq(R1, R2),
               member([_, _, escultor, caracoles, _, _], Sol),
               member([_, amarilla, actor, _, _, _], Sol),
               member([3, _, _, _, cava, _], Sol),
               member([R4, _, _, caballo, _, _], Sol),
               member([R5, _, actor, _, _, _], Sol),
               vecino(R4, R5),
               member([R6, _, _, _, _, hongria], Sol),
               member([R7, azul, _, _, _, _], Sol),
               vecino(R6, R7),
               member([_, _, notario, _, whisky, _], Sol),
               member([R8, _, _, ardilla, _, _], Sol),
               member([R9, _, medico, _, _, _], Sol),
               vecino(R8, R9),
               write(Sol), nl.



/* AQUEST ME L'HE DE MIRAR
3. Haz un programa prolog que escriba la manera de colocar sobre un tablero de
   ajedrez ocho reinas sin que éstas se ataquen entre sí.
   Por ejemplo, ésta sería una solucion:
         
      . . x . . . . .
      . . . . . x . .
      . . . x . . . .
      . x . . . . . .
      . . . . . . . x
      . . . . x . . .
      . . . . . . x . 
      x . . . . . . . */
      
      


      
queens :- permutation([1,2,3,4,5,6,7,8], PermutedList),
          allOK(PermutedList),
          printQueens(PermutedList).

% Se segur que no hi haura cap reina a la mateixa columna ni fila, 
% nomes he de comprovar les diagonals
allOK([]).
allOK([L|List]):- checkQueen(L, List, 1), allOK(List).

checkQueen(_, [], _).
checkQueen(L, [L2|List], Dist):- L - L2 =\= Dist,
                                 L2 - L =\= Dist,
                                 Dist2 is Dist + 1,
                                 checkQueen(L, List, Dist2).
          
% Printing code          
printQueens([]).
printQueens([L|List]):- printRow(L), nl, printQueens(List).

printRow(L):- printPart(L), write('X'), printPart2(L).

printPart(1):- !.
printPart(L):- write('.'), L2 is L - 1, printPart(L2).

printPart2(8):- !.
printPart2(L):- write('.'), L2 is L + 1, printPart2(L2).

