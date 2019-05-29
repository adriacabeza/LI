:- use_module(library(clpfd)).

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


main:-
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
