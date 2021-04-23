:- include(entradaFlow1).
symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.

%%%%%% Some helpful definitions to make the code cleaner:
node(X,Y):- size(N), between(1,N,X), between(1,N,Y).
color(C):- c(C,_,_,_,_).
start(X,Y):- c(_,X,Y,_,_).
end(X,Y):- c(_,_,_,X,Y).

%%%%%%  1. SAT Variables:
% means that the node N has the color C
satVariable(nodeColor(X,Y,C)):- node(X,Y), color(C).
satVariable(nodeConnected(X,Y,X1,Y1)) :- node(X,Y), node(X1,Y1).

/*
This could happen
R R R G
R R R G
R R R G	
G G G G
*/

%%%%%%  2. Clause generation:
writeClauses:-
		exactly2Connected,
		exactlyOneColorPerNode,
		implyConnected,
		filledInputValues,
		nodeConnectedImpliesColors,
		exactly1Connected,
    	true,!.                    % this way you can comment out ANY previous line of writeClauses
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.


exactlyOneColorPerNode:-
	node(X,Y),
	findall(nodeColor(X,Y,C), color(C), Lits),
	exactly(1,Lits),
	fail.
exactlyOneColorPerNode.	

implyConnected:-
	node(X,Y),
	node(X1,Y1),
	writeClause([-nodeConnected(X,Y,X1,Y1), nodeConnected(X1,Y1,X,Y)]),
	writeClause([-nodeConnected(X1,Y1,X,Y), nodeConnected(X,Y,X1,Y1)]),
	fail.
implyConnected.


nodeConnectedImpliesColors:-
	color(C),
	node(X,Y),
	connected(X,Y,X1,Y1),
	writeClause([-nodeConnected(X,Y,X1,Y1), -nodeColor(X,Y,C),  nodeColor(X1,Y1,C)]),
	fail.
nodeConnectedImpliesColors.

% connected in the X axis
connected(X,Y,X1,Y):- node(X1,Y), X1 is X-1.
connected(X,Y,X1,Y):- node(X1,Y), X1 is X+1. 
 % connected in the Y axis
connected(X,Y,X,Y1):- node(X,Y1), Y1 is Y+1.
connected(X,Y,X,Y1):- node(X,Y1), Y1 is Y-1.

theSame(X,Y,X,Y).


exactly1Connected:-
	start(Xini,Yini),
	node(Xini,Yini),
	end(Xfin,Yfin),
	node(Xfin,Yfin),
	findall(nodeConnected(Xfin,Yfin,X,Y), connected(Xfin,Yfin,X,Y), LitsFin),
	exactly(1,LitsFin),
	findall(nodeConnected(Xini,Yini,X,Y), connected(Xini,Yini,X,Y), LitsIni),
	exactly(1,LitsIni),
	fail.
exactly1Connected.


notEndpoints(X,Y):-
	findall(node(Xfin,Yfin), end(Xfin,Yfin), LitsFin),
	findall(node(Xini,Yini),start(Xini,Yini),  LitsInit),
	\+ member(node(X,Y), LitsInit),
	\+ member(node(X,Y), LitsFin).


exactly2Connected:-
	node(X,Y), 
	notEndpoints(X,Y),
	findall(nodeConnected(X,Y,X2,Y2),connected(X,Y,X2,Y2), Lits), 
	%findall(nodeConnected(X,Y,X2,Y2),(connected(X,Y,X2,Y2), notEndpoints(X2,Y2)), Lits), with this LINE IT DOES NOT WORK
	exactly(2,Lits),
	fail.
exactly2Connected.


filledInputValues:-
	color(C),
	node(X,Y),
	node(X1,Y1),
	c(C,X,Y,X1,Y1),
	writeClause([ nodeColor(X,Y,C) ]), 
	writeClause([ nodeColor(X1,Y1,C) ]), 
	fail.
filledInputValues.



%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:
getColorCode(blue,'\033[34mBlu ').
getColorCode(brown,'\033[90mBro ').
getColorCode(red, '\033[31mRed ').
getColorCode(cyan,'\033[36mCya ').
getColorCode(green, '\033[32mGre ').
getColorCode(yellow,'\033[33mYel ').
getColorCode(pink,'\033[35mPin ').
getColorCode(violet,'\033[95mVio ').
getColorCode(orange,'\033[37mOra ').

displaySol(M):- displayEndpoints(M), nl, fail.
displaySol(M):-  writeAux(M), nl, fail.
%displaySol(M):- writeColor(M), nl, fail.
displaySol(_):- nl.

%writeColor(M):- color(C), nl,nl, getColorCode(C,Code), write(Code), write('path:'), writePath(C,M), fail.
writePath(C,M):- nl, findall((X,Y), member(nodeColor(X,Y,C),M), Lits), write(Lits),nl, write('Connections:'), writeConnected(M,Lits),fail.
writeConnected(_).
writeConnected(M,[(X,Y)|L]):- nl, findall((X1,Y1), member(nodeConnected(X,Y,X1,Y1),M), Lits), write(X),write(','), write(Y), write(':'), write(Lits), writeConnected(M,L).

displayEndpoints(M):- 
	findall((C:Xini,Yini),(start(Xini,Yini), member(nodeColor(Xini,Yini,C),M)),  LitsInit),
	findall((C:Xfin,Yfin),( end(Xfin,Yfin), member(nodeColor(Xfin,Yfin,C),M)),LitsFin),
	nl,
	write('InitialPoints:'),
	nl,
	write(LitsInit),
	nl,nl,
	write('Endpoints:'),
	nl,
      	write(LitsFin),
       	nl.

writeAux(M):- size(N), between(1,N,Y), writeResult(Y,M), fail.
writeResult(Y,M):- findall((X,C),member(nodeColor(X,Y,C),M), Lits), writeColors(Lits),nl, fail.
writeColors([]):- write('\033[0m').
writeColors([(_,C)|L]):- getColorCode(C, Code), write(Code), writeColors(L).
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
