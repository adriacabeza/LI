:-dynamic(varNumber/3).
symbolicOutput(0). % set to 1 to see symbolic output only; 0 otherwise.

% A company needs to distribute its employees in working teams of size
% between minSize and maxSize. In order to avoid fights between them,
% the psychology department has computed a score between 0 an 10 for
% each worker, that estimates her leadership skills.  It has been
% decided that two workers whose scores sum more than maxScore cannot
% be in the same team.
%
% Complete the following program in order to find a possible team
% distribution.
%
%
%%%%%%%%%%%%%%%%%%%% input %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

numWorkers(20).
minSize(3). % min size of any team
maxSize(6). % max size of any team
maxScore(14). % two workers whose scores sum more than 14 cannot go together
numTeams(5). % exact number of teams needed

%score(workerId, score)
score( 1, 2).
score( 2, 6).
score( 3, 3).
score( 4, 4).
score( 5, 6).
score( 6, 10).
score( 7, 2).
score( 8, 1).
score( 9, 4).
score(10, 2).
score(11, 7).
score(12, 9).
score(13, 3).
score(14, 4).
score(15, 9).
score(16, 1).
score(17, 3).
score(18, 8).
score(19, 2).
score(20, 1).


%%%%%%%%%%%%%%%%% end input %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%% Some helpful definitions to make the code cleaner:
worker(W):-numWorkers(N), between(1,N,W).
workerScore(W,S):-worker(W), score(W,S).
team(T):- numTeams(N), between(1,N,T).
incompatibleWorkers(W1,W2):- score(W1,S1), score(W2,S2), maxScore(M), S1+S2 > M ,true. 

%%%%%%  SAT Variables:

% wt(W,T) means "worker W is in team T"
satVariable( wt(W,T) ):- worker(W), team(T).

writeClauses:-  
    eachWorkerExactlyOneTeam, 
    eachTeamAtLeastSize,
    eachTeamAtMostSize,
	workersCompatible,
    true,!.
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl.

workersCompatible:- worker(W1), worker(W2), team(T),incompatibleWorkers(W1,W2),
writeClause([-wt(W1,T),-wt(W2,T)]), fail. 
workersCompatible.

eachTeamAtMostSize:- team(T), maxSize(M),  findall(wt(W,T), worker(W),Lits) ,atMost(M,Lits), fail.
eachTeamAtMostSize.

eachTeamAtLeastSize:- team(T), minSize(M),  findall(wt(W,T), worker(W),Lits) , atLeast(M,Lits),fail.
eachTeamAtLeastSize.

eachWorkerExactlyOneTeam:- worker(W), team(T), findall(wt(W,T), team(T), Lits) , exactly(1,Lits), fail. 
eachWorkerExactlyOneTeam.
    

%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% show the solution. Here M contains the literals that are true in the model:
displaySol(M):- team(T), write('Team '), write(T), write(': '), findall(W-score(S),(member(wt(W,T),M),workerScore(W,S)),L), write(L), findall(Sc, (member(W1-score(S1),L), member(W2-score(S2),L), W1 \= W2, Sc is S1+S2), ListSums), max_list(ListSums,Max), write(' ## Max sum of pairs '), write(Max),  nl, fail.
displaySol(_):- nl.

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

main:-  symbolicOutput(1), !, writeClauses, true.    % print the clauses in symbolic form and halt
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
