programa(L):- append([begin],X,S), append(S,[end],L), instrucciones(X), !.

instrucciones(X):- instruccio(X).
instrucciones(X):- append(L,M,X), append(S,[;],L), instruccio(S), instrucciones(M).

instruccio([X,=,Y,+,Z]):- variable(X), variable(Y), variable(Z).
instruccio([if,X,=,Y,then|SS]):- append(S,L1,SS), append(else,L2,L1), append(L,endif,L2), varible(X), varible(Y), 
instrucciones(S), instrucciones(L).

variable(x).
variable(y).
variable(z).
