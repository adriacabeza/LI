chain([],[]).
chain([[X,Y]],[[X,Y]]).
chain([[Y,X]],[[X,Y]]).
chain([[X,Y]|L],[[X,Y]|R]):- chain(L,R), R = [[Y,_]|_].
chain([[X,Y]|L],[[Y,X]|R]):- chain(L,R), R = [[X,_]|_].

%%%%%%%%%%%%%%%%%%%%%OPCIO AMB MÉS MERDES 

flip([],[]).
flip([[X,Y]|L],[[X,Y]|R]):- flip(L,R).
flip([[X,Y]|L],[[Y,X]|R]):- flip(L,R).

ok([]).
ok([_]).
ok([[_,Y],[Y,Z]|L]):- ok([[Y,Z]|L]).

chain2(X,L):- flip(X,L), ok(L).

perm([],[]).
perm([X|L], P):- perm(L,P1), append(PA,PB,P1), append(PA, [X|PB], P).


subconj([],[]).
subconj([_|L],R):-subconj(L,R).
subconj([X|L],[X|R]):- subconj(L,R).


all_chains(L):- subconj(L,S), perm(S,R), chain(R,P), write(P), nl, nl, fail.
all_chains(_).