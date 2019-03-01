padre(juan,pedro).
padre(maria,pedro).
hermano(pedro,enrique).
hermano(pedro,carlos).
tio(S,T):-padre(S,P), hermano(P,T).

member(X,[a,b,c]), write(X), nl, fail 
append(L1,L2,[a,b,c]),write(L1), nl, fail
append([a,b],[b,c,d],L).
