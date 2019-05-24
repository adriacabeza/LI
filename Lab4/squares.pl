:- use_module(library(clpfd)).

%ejemplo(_, Big, [S1...SN]): how to fit all squares of sizes S1...SN in a square of size Big?
ejemplo(0,  3,[2,1,1,1,1,1]).
ejemplo(1,  4,[2,2,2,1,1,1,1]).
ejemplo(2,  5,[3,2,2,2,1,1,1,1]).
ejemplo(3, 19,[10,9,7,6,4,4,3,3,3,3,3,2,2,2,1,1,1,1,1,1]).
ejemplo(4,112,[50,42,37,35,33,29,27,25,24,19,18,17,16,15,11,9,8,7,6,4,2]).
ejemplo(5,175,[81,64,56,55,51,43,39,38,35,33,31,30,29,20,18,16,14,9,8,5,4,3,2,1]).

main:- 
	ejemplo(3,Big,Sides),
	nl, write('Fitting all squares of size '), write(Sides), write(' into big square of size '), write(Big), nl,nl,
	length(Sides,N), 
	length(RowVars,N), % get list of N prolog vars: Row coordinates of each small square
	length(ColVars,N),
	insideBigSquare(N,Big,Sides,RowVars),
	insideBigSquare(N,Big,Sides,ColVars),
	nonoverlapping(N,Sides,RowVars,ColVars),
	append(RowVars,ColVars,Vars),
	labeling([ff],Vars),
	displaySol(N,Sides,RowVars,ColVars), halt.


displaySol(N,Sides,RowVars,ColVars):- 
	between(1,N,Row), nl, between(1,N,Col),
	nth1(K,Sides,S),    
	nth1(K,RowVars,RV),    RVS is RV+S-1,     between(RV,RVS,Row),
	nth1(K,ColVars,CV),    CVS is CV+S-1,     between(CV,CVS,Col),
	writeSide(S), fail.
displaySol(_,_,_,_):- nl,nl,!.

writeSide(S):- S<10, write('  '),write(S),!.
writeSide(S):-       write(' ' ),write(S),!.


% CHECK BIG LIMITS 
insideBigSquare(0,_,[],[]).
insideBigSquare(N, Big,[X|Sides],[V|Vars]):-
       		Max is Big - X + 1,
		V in 1..Max,
		N2 is N - 1,
	       	insideBigSquare(N2, Big, Sides, Vars).

% CHOOSE ONE SQUARE
nonoverlapping(_,[],[],[]).
nonoverlapping(_,[X|Sides],[R|RowVars],[C|ColVars]):- 
	nonoverlapping2(X, Sides,R,RowVars,C,ColVars), 
	nonoverlapping(_,Sides,RowVars,ColVars).


% TAKE THE CHOSEN SQUARE AND CHECK IF OVERLAPS TO ANOTHER ONE
nonoverlapping2(_,[],_,[],_,[]).
nonoverlapping2(X,[X2|Sides],R,[R2|RowVars],C,[C2|ColVars]):-
	nonoverlapping3(X,X2,R,C,R2,C2), 
	nonoverlapping2(X,Sides,R,RowVars,C,ColVars). 

% CHECK OVERLAPPING
nonoverlapping3(X,X2,R,C,R2,C2):- 
	X+R #=< R2 #\/
	X+C #=< C2 #\/
	X2+R2 #=< R #\/
       	X2+C2 #=< C.


