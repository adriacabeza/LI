pert(X,[X|_]).
pert(X,[_|L]):-pert(X,L).
