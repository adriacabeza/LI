concat([],L,L).
concat([X|L1],L2,[X|L3)):-concat(L1,L2,L3).

