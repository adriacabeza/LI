:- use_module(library(clpfd)).

ejemplo(0,   26, [1,2,5,10] ).  % Solution: [1,0,1,2]
ejemplo(1,  361, [1,2,5,13,17,35,157]).

main:- 
    ejemplo(1,Amount,Coins),
    nl, write('Paying amount '), write(Amount), write(' using the minimal number of coins of values '), write(Coins), nl,nl,
    length(Coins,N), 
    length(Vars,N), % get list of N prolog vars  
    Vars ins 0..Amount,
    suma(Coins,Vars,Sum), write(Sum),nl,
    Sum #= Amount,
    sum(Vars, #=, Min),
    labeling([min(Min)], Vars),
    nl, write(Vars), nl,nl, halt.
    
suma([X1|X],[Y1|Y],Sum1 + X1 * Y1):- suma(X,Y,Sum1).
suma([],[],0).
    
sumatoria([X1|X], S):-
    S is S1 + X1,
    sumatoria(X,S1).
sumatoria([],0).
