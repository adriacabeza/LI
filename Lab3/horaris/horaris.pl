:- include(entradaHoraris1).
symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.

%%%%%% Some helpful definitions to make the code cleaner:
day(D)               :- between(1,5,D).
hour(H)              :- between(1,12,H).
year(Y)              :- numCursos(N), between(1,N,Y).
course(C)            :- numAssignatures(N), between(1,N,C).
teacher(T)           :- numProfes(N), between(1,N,T).
room(R)              :- numAules(N), between(1,N,R).
lectureOfCourse(C,L) :- assig(_,C,N,_,_), between(1,N,L). 
courseHours(C,N)     :- assig(_,C,N,_,_).
courseYear(Y,C)      :- assig(Y,C,_,_,_). 
courseRooms(C,Rs)	 :- assig(_,C,_,Rs,_).
courseTeachers(C,Ts) :- assig(_,C,_,_,Ts).
% Day slots => Monday slots [1,..,12], Tuesday [13,..24], ...
slot(S)              :- between(1,60,S). 
% given the day, it computes possible slots
daytoSlot(D,S)       :- hour(H), S is (D-1)*12 + H. 
% given the slot, it computes the day
slottoDay(S,D)		 :-  day(D), D is ceil(S/12).
% given slot it computes the hour of the day
slottoHour(S,H)      :- hour(H), H is ((S-1) mod 12)+1.


% SAT VARIABLES
satVariable( cls(C,L,S) ):- course(C), lectureOfCourse(C,L), slot(S).
satVariable( cr(C,R) ):- course(C), room(R).
satVariable( cp(C,T) ):- course(C), teacher(T).
satVariable( cs(C,S) ):- course(C), slot(S).
satVariable( ys(Y,S) ):- year(Y), slot(S).


writeClauses:- 
    oneSlotPerLecture,
    atMost1LecturePerDay,
    eachCourseatExactlyOneRoom,
    eachCourseExactlyOneTeacher,
    overlapRoom,
    overlapTeacher,
    csImpliescls,
    ysImpliescs,
	forbiddenHours,
    notOverlapping,
    atMost6hoursPerDay, 
	compactTimeTable,
    true.


%at most 6 hours per day
atMost6hoursPerDay:-
    year(Y),
    day(D),
    findall(ys(Y,S), daytoSlot(D,S),Lits),
    atMost(6,Lits),
    fail.
atMost6hoursPerDay.


% one slot for each lecture
oneSlotPerLecture:-
    course(C),
    lectureOfCourse(C,L),
    findall(cls(C,L,S),slot(S),Lits),
    exactly(1,Lits),
    fail.
oneSlotPerLecture.


%  CS -> CLS V CLS V CLS V CLS V CLS 
csImpliescls:-
    course(C),
    slot(S),
    findall(cls(C,L,S), lectureOfCourse(C,L), Lits),
    expressOr(cs(C,S), Lits),
    fail.
csImpliescls.


%  YS -> CS V CS V CS V
ysImpliescs:-
    year(Y),
    slot(S),
    findall(cs(C,S),courseYear(Y,C), Lits),
    expressOr(ys(Y,S), Lits),
    fail.
ysImpliescs.


% at most 1 lecture per day
atMost1LecturePerDay:-
    course(C),
    day(D),
    findall(cls(C,L,S), (daytoSlot(D,S), lectureOfCourse(C,L)), Lits),
    atMost(1,Lits),
    fail.
atMost1LecturePerDay.


%  all lectures of a course are taught by the same teacher 
eachCourseExactlyOneTeacher:-
	courseTeachers(C,Teachers),
    findall(cp(C,T), member(T,Teachers), Lits),
    exactly(1,Lits),
    fail.
eachCourseExactlyOneTeacher.


%  all lectures of a course are at the same room 
eachCourseatExactlyOneRoom:-
	courseRooms(C,Rooms),
    findall(cr(C,R), member(R,Rooms), Lits),
    exactly(1,Lits),
    fail.
eachCourseatExactlyOneRoom.


%  each teacher cannot work the forbidden hours
forbiddenHours:-
	course(C),
	teacher(T),
	courseTeachers(C,Teachers1),
	member(T,Teachers1),
	horesProhibides(T,Hs),
	slot(S),
	member(S,Hs),
	writeClause([-cs(C,S),-cp(C,T)]),
	fail.
forbiddenHours.


overlapRoom:-
    room(R),
    slot(S),
	courseRooms(C1,Rooms1),
	courseRooms(C2,Rooms2),
    C1 \= C2,
    member(R,Rooms2),
    member(R,Rooms1),
    %writeClause es falso que cs(C1, S) y  cr(C2,S)  y cr(C1,R) -> not(cr(C2, R))
    writeClause([-cs(C1,S), -cs(C2,S), -cr(C1,R), -cr(C2,R)]),
    fail.
overlapRoom.


overlapTeacher:-
    teacher(T),
    slot(S),
	courseTeachers(C1,Teachers1),
	courseTeachers(C2,Teachers2),
    C1 \= C2,
    member(T,Teachers1),
    member(T,Teachers2),
    writeClause([-cs(C1,S), -cs(C2,S), -cp(C1,T), -cp(C2,T)]),
    fail.
overlapTeacher.


%  - two lectures belonging to the same year cannot be taught at the same time
notOverlapping:-
    year(Y),
    courseYear(Y,C),
    courseYear(Y,C2),
    C \= C2,
    slot(S),
    writeClause([-cs(C,S), -cs(C2,S)]),
    fail.
notOverlapping.


compactTimeTable:-
	year(Y),
	slot(S), slot(S2), slot(S3),
	S < S2, S2 < S3,
	day(D),
	slottoDay(S,D), slottoDay(S3,D), 
	writeClause([-ys(Y,S),-ys(Y,S3),ys(Y,S2)]),
	fail.
compactTimeTable.


%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:

extraBlank(N):- 
    N < 10, !, write(' ').
extraBlank(_).

drawTail(Y, Hour):-
    Hour > 48, 
    write('  Curs: '), write(Y), nl.
drawTail(_, _).

drawCell(Y, S, M):-
    member(cls(C,L,S), M),                   %% -------- ADAPTA la SAT variable cls(C,L,S)
    assig(Y, C, _, _, _), !,
    write(' '), extraBlank(C), write(C), write(' - '),
    extraBlank(L), write(L), 
    write('  ['), member(cr(C,R), M),        %% -------  ADAPTA la SAT variable cr(C,R)
    write('A:'), extraBlank(R), write(' '), write(R), write(']'),
    write('  ['), member(cp(C,T), M),        %% -------  ADAPTA la SAT variable cp(C,T)
    write('P:'), extraBlank(T), write(' '), write(T), write(']'),
    write(' ').
drawCell(_, _, _):- 
    write('                           ').    

drawRow(Row, _):-
    1 is Row mod 2,
    H is Row // 2 + 8, 
    extraBlank(H), 
    write(' '), write(H), write(':00 '), 
    between(1, 141, _), write('='), 
    fail.
drawRow(Row, _):-
    1 is Row mod 2, !, nl.

drawRow(Row, M):-
    year(Y),
    write('       |'),
    between(0, 4, Day), 
    Hour is Row // 2 + Day * 12,
    drawCell(Y, Hour, M), 
    write('|'), 
    drawTail(Y, Hour), 
    fail.
drawRow(_, _).

drawHeader:-
    nl, nl, 
    write(' Format de sortida: Assignatura - Hora [A: Aula] [P: Professor]'), 
    nl, nl, 
    write('                 Dilluns                     Dimarts                     dimecres                     Dijous                    Divendres').

displaySchedule(M):-
    drawHeader, nl,
    between(1, 25, Row), 
    drawRow(Row, M), 
    fail.

drawHeaderYear(Y):-
    nl, nl, 
    write('----------------------------------------------------------------------------------------------------------------------------------------------------'),
    nl,
    write(' Horari del curs '), write(Y),
    nl,
    write(' Format de sortida: Assignatura - Hora [A: Aula] [P: Professor]'), 
    nl, nl, 
    write('                 Dilluns                     Dimarts                     dimecres                     Dijous                    Divendres').

drawTailYear(Hour):-
    Hour > 48, nl.
drawTailYear(_, _).

drawRowYear(Row, _, _):-
    1 is Row mod 2,
    H is Row // 2 + 8, 
    extraBlank(H), 
    write(' '), write(H), write(':00 '), 
    between(1, 141, _), write('='), 
    fail.
drawRowYear(Row, _, _):-
    1 is Row mod 2, !, nl.
drawRowYear(Row, Y, M):-
    write('       |'),
    between(0, 4, Day), 
    Hour is Row // 2 + Day * 12,
    drawCell(Y, Hour, M), 
    write('|'),
    drawTailYear(Hour), 
    fail.
drawRowYear(_, _, _).

displayScheduleYear(Y,M):-
    drawHeaderYear(Y), nl,
    between(1, 25, Row), 
    drawRowYear(Row, Y, M), 
    fail.

displaySol(M):- displaySchedule(M), fail.
displaySol(M):- year(Y), displayScheduleYear(Y,M), fail.
%displaySol(M):- year(Y), findall((C,S), (member(cs(C,S), M), courseYear(Y,C)), Lits), write('Assignatures del curs '), write(Y), nl, write(Lits), nl, fail.
displaySol(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything below is given as a standard library, reusable for solving 
%    with SAT many different problems.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Express that Var is equivalent to the disjuncpion of Lits:
expressOr( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> or('), write(Lits), write(')'), nl, !. 
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.
% Express that Var is equivalent to the conjuncpion of Lits:
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
