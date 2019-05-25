programa(X):- append([begin|L],[end],X), instrucciones(L).

instrucciones(X):- instruccion(X). %es una instruccion 
instrucciones(X):-  append(L,[;],AUX), append(AUX,L2, X), instruccion(L), instrucciones(L2).


instruccion([V,=,X,+,Y]):- variable(V), variable(X), variable(Y). %suma normal 
instruccion([if,A,=,B,then|X]):-variable(A), variable(B), append(L,[else], Aux),append(Aux,R,Aux2), append(Aux2,[endif],X) , instrucciones(L), instrucciones(R). %if

variable(x).
variable(y).
variable(z).
