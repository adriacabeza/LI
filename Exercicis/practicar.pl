% Exercice 1

flatten([], []).
flatten([X|L], F) :- flatten(X, F1), flatten(L, F2), append(F1, F2, F), !.
flatten(X, [X]).

flattenNoRepetitions([], []).
flattenNoRepetitions([X|L], FNR) :-
	flattenNoRepetitions(X, F1), flattenNoRepetitions(L, F2),
	union(F1, F2, FNRU), sort(FNRU, FNR), !.
flattenNoRepetitions(X, [X]).

union([], L, L).
union([X|L1], L2, L3) :- member(X, L2), !, union(L1, L2, L3).
union([X|L1], L2, [X|L3]) :- union(L1, L2, L3).

% Exercice 2

casas :-
	Sol = [
		[1,_,_,_,_,_],
		[2,_,_,_,_,_],
		[3,_,_,_,_,_],
		[4,_,_,_,_,_],
		[5,_,_,_,_,_]
	],
	member([_,roja,_,_,_,peru], Sol), % El que vive en la casa roja es de Peru
	member([_,_,_,perro,_,francia], Sol), % Al frances le gusta el perro
	member([_,_,pintor,_,_,japon], Sol), % El pintor es japones
	member([_,_,_,_,ron,china], Sol), % Al chino le gusta el ron
	member([1,_,_,_,_,hungria], Sol), % El hungaro vive en la primera casa
	member([_,verde,_,_,coñac,_], Sol), % Al de la casa verde le gusta el coñac
	member([A,verde,_,_,_,_], Sol), member([B,white,_,_,_,_], Sol), 1 is B-A, % La casa verde esta a la izquierda de la blanca
	member([_,_,escultor,caracol,_,_], Sol), % El escultor cría caracoles
	member([_,amarillo,actor,_,_,_], Sol), % El de la casa amarilla es actor
	member([3,_,_,_,cava,_], Sol), % El de la tercera casa bebe cava
	member([C,_,actor,_,_,_], Sol), member([D,_,_,caballo,_,_], Sol), 1 is abs(C-D), % El que vive al lado del actor tiene un caballo
	member([E,_,_,_,_,hungria], Sol), member([F,azul,_,_,_,_], Sol), 1 is abs(E-F), % El hungaro vive al lado de la casa azul
	member([_,_,notario,_,whisky,_], Sol), % Al notario la gusta el whisky
	member([G,_,medico,_,_,_], Sol), member([H,_,_,ardilla,_,_], Sol), 1 is abs(G-H), % El que vive al lado del medico tiene un ardilla
	write(Sol), nl, !.

% Exercice 3

programa(P) :- append([begin|IS], [end], P), instructions(IS), !.

instructions(IS) :- instruction(IS), !.
instructions(IS) :- append(I, [;], I1), !, append(I1, I2, IS), instruction(I), instructions(I2).

instruction([R,=,O1,+,O2]) :- variable(O1), variable(O2), variable(R).
instruction(CI) :-
	append([if|C], [endif], CI),
	append(BE, [then|CB], C), boolean_expression(BE),
	append(II, [else|IE], CB),
	instruction(II), instruction(IE).

boolean_expression([O1,=,O2]) :- variable(O1), variable(O2).

variable(x).
variable(y).
variable(z).

