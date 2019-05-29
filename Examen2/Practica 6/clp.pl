:- use_module(library(clpfd)).

ejemplo(0,   26, [1,2,5,10] ).  % Solution: [1,0,1,2]
ejemplo(1,  361, [1,2,5,13,17,35,157]).

coins:- 
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


%Complete the following program.
%example(_, Big, [S1...SN]): how to fit all squares of sizes S1...SN in a square of size Big?
example(0,  3,[2,1,1,1,1,1]).
example(1,  4,[2,2,2,1,1,1,1]).
example(2,  5,[3,2,2,2,1,1,1,1]).
example(3, 19,[10,9,7,6,4,4,3,3,3,3,3,2,2,2,1,1,1,1,1,1]).
example(4, 40,[24,16,16,10,9,8,8,7,7,6,6,3,3,3,2,1,1]).

%% Possible output solution for example 3:
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  1  1
%%   3  3  3  3  3  3  4  4  4  4  3  3  3  3  3  3  3  3  3
%%   3  3  3  3  3  3  4  4  4  4  3  3  3  3  3  3  3  3  3
%%   3  3  3  3  3  3  1  1  1  1  3  3  3  3  3  3  3  3  3


% The following examples are VERY hard, out of scope for this technology:
% example(5,112,[50,42,37,35,33,29,27,25,24,19,18,17,16,15,11,9,8,7,6,4,2]).
% example(6,175,[81,64,56,55,51,43,39,38,35,33,31,30,29,20,18,16,14,9,8,5,4,3,2,1]).

square:- 
    example(2,Big,Sides),
    nl, write('Fitting all squares of size '), write(Sides), write(' into big square of size '), write(Big), nl,nl,
    length(Sides,N), 
    length(RowVars,N), % get list of N prolog vars: Row coordinates of each small square
    length(ColVars,N), % idem cols.
    RowVars ins 1..Big,
    ColVars ins 1..Big,
    insideBigSquare(RowVars,ColVars,Sides,Big),
    nonoverlapping(RowVars,ColVars,Sides),
    append(RowVars,ColVars,Vars),
    labeling([],Vars),
    write(Vars),nl,
    displaySol(Big,Sides,RowVars,ColVars), halt.

insideBigSquare([R|Row],[C|Col],[S|Sides],Big):-
    Max is Big - S + 1,
    R #=< Max,
    C #=< Max,
    insideBigSquare(Row,Col,Sides,Big).
insideBigSquare([],[],[],_).
    
nonoverlapping([R|Row],[C|Col],[S|Sides]):-
    noOverLap(R,Row,C,Col,S,Sides),
    nonoverlapping(Row,Col,Sides).
nonoverlapping([],[],[]).
    
noOverLap(R,[R1|Rows],C,[C1|Cols],S,[S1|Sides]):-
    R+S #=< R1 #\/ C+S #=< C1 #\/ R1+S1 #=< R #\/ C1+S1 #=< C,
    noOverLap(R,Rows,C,Cols,S,Sides).
noOverLap(_,[],_,[],_,[]).
    
displaySol(N,Sides,RowVars,ColVars):- 
    between(1,N,Row), nl, between(1,N,Col),
    nth1(K,Sides,S),    
    nth1(K,RowVars,RV),    RVS is RV+S-1,     between(RV,RVS,Row),
    nth1(K,ColVars,CV),    CVS is CV+S-1,     between(CV,CVS,Col),
    writeSide(S), fail.
displaySol(_,_,_,_):- nl,nl,!.

writeSide(S):- S<10, write('  '),write(S),!.
writeSide(S):-       write(' ' ),write(S),!.



% El dels daus va molt lent, s'hauria d'optimitazar


%% A (6-sided) "letter dice" has on each side a different letter.
%% Find four of them, with the 24 letters abcdefghijklmnoprstuvwxy such
%% that you can make all the following words: bake, onyx, echo, oval,
%% gird, smug, jump, torn, luck, viny, lush, wrap.

%Some helpful predicates:

word( [b,a,k,e] ).
word( [o,n,y,x] ).
word( [e,c,h,o] ).
word( [o,v,a,l] ).
word( [g,i,r,d] ).
word( [s,m,u,g] ).
word( [j,u,m,p] ).
word( [t,o,r,n] ).
word( [l,u,c,k] ).
word( [v,i,n,y] ).
word( [l,u,s,h] ).
word( [w,r,a,p] ).

num(X,N):- nth1( N, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u,v,w,x,y], X ).

constraints([L|List],D1,D2,D3,D4):-
    noRepetidoEnDado(L,D1),
    noRepetidoEnDado(L,D2),
    noRepetidoEnDado(L,D3),
    noRepetidoEnDado(L,D4),
    constraints(List,D1,D2,D3,D4).
constraints([],_,_,_,_).

noRepetidoEnDado([S1,S2,S3,S4],[X1,X2,X3,X4,X5,X6]):-
    num(S1,L1),
    num(S2,L2),
    num(S3,L3),
    num(S4,L4),
    comparacions(X1,X2,L1,L2,L3,L4),
    comparacions(X1,X3,L1,L2,L3,L4),
    comparacions(X1,X4,L1,L2,L3,L4),
    comparacions(X1,X5,L1,L2,L3,L4),
    comparacions(X1,X6,L1,L2,L3,L4),
    comparacions(X2,X3,L1,L2,L3,L4),
    comparacions(X2,X4,L1,L2,L3,L4),
    comparacions(X2,X5,L1,L2,L3,L4),
    comparacions(X2,X6,L1,L2,L3,L4),
    comparacions(X3,X4,L1,L2,L3,L4),
    comparacions(X3,X5,L1,L2,L3,L4),
    comparacions(X3,X6,L1,L2,L3,L4),
    comparacions(X4,X5,L1,L2,L3,L4),
    comparacions(X4,X6,L1,L2,L3,L4),
    comparacions(X5,X6,L1,L2,L3,L4).
        

comparacions(X1,X2,L1,L2,L3,L4):-
    X1 #\= L1 #\/ X2 #\= L2,
    X1 #\= L1 #\/ X2 #\= L3,
    X1 #\= L1 #\/ X2 #\= L4,
    X1 #\= L2 #\/ X2 #\= L1,
    X1 #\= L2 #\/ X2 #\= L3,
    X1 #\= L2 #\/ X2 #\= L4,
    X1 #\= L3 #\/ X2 #\= L1,
    X1 #\= L3 #\/ X2 #\= L2,
    X1 #\= L3 #\/ X2 #\= L4,
    X1 #\= L4 #\/ X2 #\= L1,
    X1 #\= L4 #\/ X2 #\= L2,
    X1 #\= L4 #\/ X2 #\= L3.


daus:-
    length(D1,6),
    length(D2,6),
    length(D3,6),
    length(D4,6),

    D1 ins 1..24,
    D2 ins 1..24,
    D3 ins 1..24,
    D4 ins 1..24,

    append(D1,D2,A1),
    append(A1,D3,A2),
    append(A2,D4,Todos),
    
    all_different(Todos),
    
    findall(X, word(X), L),
    constraints(L, D1, D2, D3, D4),
    
    labeling([],Todos),
    writeN(D1), 
    writeN(D2), 
    writeN(D3), 
    writeN(D4),
    halt.
    
writeN(D):- findall(X,(member(N,D),num(X,N)),L), write(L), nl, !.

