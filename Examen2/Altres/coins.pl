:- use_module(library(clpfd)).

ejemplo(0,   26, [1,2,5,10] ).  % Solution: [1,0,1,2]
ejemplo(1,  361, [1,2,5,13,17,35,157]).

main:- 
    ejemplo(1,Amount,Coins),
    nl, write('Paying amount '), write(Amount), write(' using the minimal number of coins of values '), write(Coins), nl,nl,
    length(Coins,N), 
    length(Vars,N), % get list of N prolog vars    
    Vars ins 0..Amount,
    scalar_product(Coins,Vars,#=,Amount),
    sum(Vars,#=,Suma),
    labeling([min(Suma)],Vars),
    nl, write(Vars), nl,nl, halt.


