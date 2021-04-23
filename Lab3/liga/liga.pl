:- include(entradaLigasSmall).
symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.


restriccionKrepeticiones(4).

%%%%%% Some helpful definitions to make the code cleaner:
equipo(I):- numEquipos(R), between(1,R,I). 
jornada(J):- numEquipos(R), Aux is (R-1)*2, between(1,Aux,J).
equipoDif(E1,E2):- equipo(E2), E1 \= E2.

%%%%%%  1. SAT Variables:

% equipo E juega en casa contra el equipo F en la jornada J
satVariable(partido(E,F,J)):- equipo(E), equipo(F), jornada(J). % el quipo E juega un partido contra F en la jornada J
satVariable(home(E,J)):- equipo(E), jornada(J). % el equipo E juega la jornada J en casa
satVariable(double(E,J)):- jornada(J), equipo(E). % el equipo E repite en casa o fuera en la jornada K

%%%%%%  2. Clause generation:

relationHomeAndDouble:-
    equipo(E1), jornada(J1), J2 is J1+1, jornada(J2),
    writeClause([  -home(E1,J1),  -home(E1,J2),    double(E1,J1) ]),
    writeClause([   home(E1,J1),   home(E1,J2),    double(E1,J1) ]),
    writeClause([  -home(E1,J1),   home(E1,J2),   -double(E1,J1) ]),
    writeClause([   home(E1,J1),  -home(E1,J2),   -double(E1,J1) ]), 
    fail.
relationHomeAndDouble.


matchImpliesHomeorAway:-
	equipo(E1),
	equipoDif(E1,E2),
	jornada(J),
	writeClause([-partido(E1,E2,J), home(E1,J)]),
	writeClause([-partido(E1,E2,J), -home(E2,J)]),
	fail.
matchImpliesHomeorAway.


noTriplications:-
	equipo(E),
	jornada(J1),
	J2 is J1-1,
	jornada(J2),
	writeClause([-double(E,J1), -double(E,J2)]),
	fail.
noTriplications.

noDoubles:-
	jornada(J1),
	norepes(J1,_),
	equipo(E1),
	writeClause([-double(E1, J1)]),
	fail.
noDoubles.


notHomes:- 
	nocasa(E1,J),
	writeClause([-home(E1,J)]),
	fail.
notHomes.


notAway:-
	nofuera(E1,J),
	writeClause([home(E1,J)]),
	fail.
notAway.


siPartido:-
	sipartido(E1,E2,J),
	writeClause([partido(E1,E2,J)]),
	fail.
siPartido.


noPartido:-
	nopartido(E1,E2,J),
	writeClause([-partido(E1,E2,J)]),
	fail.
noPartido.


eachMatchOnce:-
	equipo(E1),
	equipoDif(E1,E2),
	findall(partido(E1,E2,J), jornada(J), Lits),
	exactly(1,Lits),
	fail.
eachMatchOnce.


eachTeamEachDayExactlyOneMatch:-
	equipo(E1),
	jornada(J),
	findall(partido(E1,E2,J),equipoDif(E1,E2), Home),
	findall(partido(E2,E1,J),equipoDif(E1,E2), Visitor),
	append(Home,Visitor,Lits),
	exactly(1,Lits),
	fail.
eachTeamEachDayExactlyOneMatch.


atMostKDoubles:-
	equipo(E),
	restriccionKrepeticiones(K),
	findall(double(E,J), jornada(J), Lits),
	atMost(K, Lits),
	fail.
atMostKDoubles.

writeClauses:-
	siPartido,
	noPartido,
	noTriplications,
	matchImpliesHomeorAway,
	relationHomeAndDouble,
	noDoubles,
	%atMostKDoubles,  EXTENSION, VERY SLOW
	notHomes,
	notAway,
	eachMatchOnce,
	eachTeamEachDayExactlyOneMatch,
    	true,!.                    % this way you can comment out ANY previous line of writeClauses
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.


%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:

displaySol(M):- write('Solución:'),nl, writeJornada(M), nl, fail. 
displaySol(M):- writeRepeticiones(M), nl, fail. 
displaySol(_):- nl,nl.

writeRepeticiones(M):- nl,  write('Repeticiones: '), findall(E1-J1, member(double(E1,J1),M),L), nl,  writeEquipoJornada(L).
writeEquipoJornada([]). 
writeEquipoJornada([E1-J1|L]):- write('	El equipo '), write(E1), write(' repite el día '), write(J1), nl, writeEquipoJornada(L).
writeJornada(M):- nl, jornada(J),  write('Jornada: '), write(J), nl, writePartidos(J,M), fail.
writePartidos(J,M):- findall(E1-E2, member(partido(E1,E2,J),M),L),  writePartido(L).
writePartido([]).
writePartido([E1-E2|L]):- write('	'), write(E1), write(' vs '), write(E2), nl, writePartido(L).
%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything below is given as a standard library, reusable for solving 
%    with SAT many different problems.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> or('), write(Lits), write(')'), nl, !. 
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.

% Express that Var is equivalent to the conjunction of Lits:
expressAnd( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> and('), write(Lits), write(')'), nl, !. 
expressAnd( Var, Lits):- member(Lit,Lits), negate(Var,NVar), writeClause([ NVar, Lit ]), fail.
expressAnd( Var, Lits):- findall(NLit, (member(Lit,Lits), negate(Lit,NLit)), NLits), writeClause([ Var | NLits]), !.


%%%%%% Cardinality constraints on arbitrary sets of literals Lits:

exactly(K,Lits):- symbolicOutput(1), write( exactly(K,Lits) ), nl, !.
exactly(K,Lits):- atLeast(K,Lits), atMost(K,Lits),!.

atMost(K,Lits):- symbolicOutput(1), write( atMost(K,Lits) ), nl, !.
atMost(K,Lits):-   % l1+...+ln <= k:  in all subsets of size k+1, at least one is false:
	negateAll(Lits,NLits),
	K1 is K+1,    subsetOfSize(K1,NLits,Clause), writeClause(Clause),fail.
atMost(_,_).

atLeast(K,Lits):- symbolicOutput(1), write( atLeast(K,Lits) ), nl, !.
atLeast(K,Lits):-  % l1+...+ln >= k: in all subsets of size n-k+1, at least one is true:
	length(Lits,N),
	K1 is N-K+1,  subsetOfSize(K1, Lits,Clause), writeClause(Clause),fail.
atLeast(_,_).

negateAll( [], [] ).
negateAll( [Lit|Lits], [NLit|NLits] ):- negate(Lit,NLit), negateAll( Lits, NLits ),!.

negate( -Var,  Var):-!.
negate(  Var, -Var):-!.

subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).


%%%%%% main:

main:-  symbolicOutput(1), !, writeClauses, halt.   % print the clauses in symbolic form and halt
main:-  initClauseGeneration,
tell(clauses), writeClauses, told,          % generate the (numeric) SAT clauses and call the solver
tell(header),  writeHeader,  told,
numVars(N), numClauses(C),
write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
shell('cat header clauses > infile.cnf',_),
write('Calling solver....'), nl,
shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
	treatResult(Result),!.

treatResult(20):- write('Unsatisfiable'), nl, halt.
treatResult(10):- write('Solution found: '), nl, see(model), symbolicModel(M), seen, displaySol(M), nl,nl,halt.
treatResult( _):- write('cnf input error. Wrote anything strange in your cnf?'), nl,nl, halt.
    

initClauseGeneration:-  %initialize all info about variables and clauses:
	retractall(numClauses(   _)),
	retractall(numVars(      _)),
	retractall(varNumber(_,_,_)),
	assert(numClauses( 0 )),
	assert(numVars(    0 )),     !.

writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w(-Var):- symbolicOutput(1), satVariable(Var), write(-Var), write(' '),!. 
w( Var):- symbolicOutput(1), satVariable(Var), write( Var), write(' '),!. 
w(-Var):- satVariable(Var),  var2num(Var,N),   write(-), write(N), write(' '),!.
w( Var):- satVariable(Var),  var2num(Var,N),             write(N), write(' '),!.
w( Lit):- told, write('ERROR: generating clause with undeclared variable in literal '), write(Lit), nl,nl, halt.


% given the symbolic variable V, find its variable number N in the SAT solver:
:-dynamic(varNumber / 3).
var2num(V,N):- hash_term(V,Key), existsOrCreate(V,Key,N),!.
existsOrCreate(V,Key,N):- varNumber(Key,V,N),!.                            % V already existed with num N
existsOrCreate(V,Key,N):- newVarNumber(N), assert(varNumber(Key,V,N)), !.  % otherwise, introduce new N for V

writeHeader:- numVars(N),numClauses(C), write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-     retract( numClauses(N0) ), N is N0+1, assert( numClauses(N) ),!.
newVarNumber(N):- retract( numVars(   N0) ), N is N0+1, assert(    numVars(N) ),!.

% Getting the symbolic model M from the output file:
symbolicModel(M):- get_code(Char), readWord(Char,W), symbolicModel(M1), addIfPositiveInt(W,M1,M),!.
symbolicModel([]).
addIfPositiveInt(W,L,[Var|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, varNumber(_,Var,N),!.
addIfPositiveInt(_,L,L).
readWord( 99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ c
readWord(115,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ s
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.
%========================================================================================
