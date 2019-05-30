%%%%%%%%%%%%%%%%%%%% input %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

letter(L):-member(L,[a,c,j,m,n,o,r,t,u]).
word( [c,a,r] ).
word( [r,u,n] ).
word( [n,o,t] ).
word( [j,a,m] ).
word( [j,u,t] ).
word( [j,a,r] ).
word( [j,o,t] ).
word( [m,o,c] ).


%%%%%%% This is what the students are supposed to do

splitInto3([],[],[],[]).
splitInto3([X,Y,Z|L],[X|L1],[Y|L2],[Z|L3]):-
    splitInto3(L,L1,L2,L3).

dices(D1,D2,D3):-
    findall(L,letter(L),ListLetters),
    permutation(ListLetters,P),
    splitInto3(P,D1,D2,D3).



%%%%%%%%%%%%%%%%%%%% input %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

letter(L):-member(L,[a,c,j,m,n,o,r,t,u]).
word( [c,a,r] ).
word( [r,u,n] ).
word( [n,o,t] ).
word( [j,a,m] ).
word( [j,u,t] ).
word( [j,a,r] ).
word( [j,o,t] ).
word( [m,o,c] ).


splitInto3([],[],[],[]).
splitInto3([X,Y,Z|L],[X|L1],[Y|L2],[Z|L3]):-
    splitInto3(L,L1,L2,L3).

dices(D1,D2,D3):-
    findall(L,letter(L),ListLetters),
    permutation(ListLetters,P),
    splitInto3(P,D1,D2,D3).

allowWords(_,_,_,[]).
allowWords(D1,D2,D3,[[L1,L2,L3] | ListWords]):-
    permutation([L1,L2,L3],[E1,E2,E3]),
    member(E1,D1),
    member(E2,D2),
    member(E3,D3),!,
    allowWords(D1,D2,D3,ListWords).

allowWords(D1,D2,D3):-
    findall([L1,L2,L3],word([L1,L2,L3]), ListWords),
    allowWords(D1,D2,D3, ListWords).

solve:-
    dices(D1,D2,D3),
    allowWords(D1,D2,D3),
    write('First dice: '),  write(D1), nl,
    write('Second dice: '), write(D2), nl,
    write('Third dice: '),  write(D3), nl,!.
