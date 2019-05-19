:- use_module(library(clpfd)).
% pagar 361 euros con monedas de [1,2,5,13,17,35,157] usando el mínimo numero de monedas
main:- Coins = [1,2,5,13,17,35,157], Amount=361,
	length(Coins,N),
	length(Vars,N),
	Vars ins 0..Amount,
	expresionAmount(Vars,Coins,E), E #= Amount,
	expresionsuma(Vars,ES),
	labeling([min(ES)],Vars), write(E), nl.


expresionsuma([V],V).
expresionsuma([V|Vars],V+E):- expresionsuma(Vars,E).

growing([]).
growing([X,Y|L]):- X#=<Y, growing([y|L]).

expresionAmount([V],[C],V*C).
expresionAmount([V|Vars],[C|Coins],V*C+E):- expresionAmount(Vars,Coins,E).
