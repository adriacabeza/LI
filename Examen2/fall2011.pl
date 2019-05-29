% Fall 2013
% We have three dice (a die in Spanish is “dado”, and the plural of die is dice). They are fair (each one of their six sides has the same probability of coming up) 
% and their sides have numbers between 1 and 9 (not between 1 and 6!). Now suppose we play a game (many times): I pick a die; after that, you pick another die, 
% we roll both dice, and the player who gets the highest number receives one Euro from the other player. Can you design the dice (putting the numbers on them) 
% in such a way that you can become rich, that is, so that you can always pick a die that is better than mine (here better means that it wins with probability p > 0.5)? 
% Write a Prolog program that checks whether this is possible or not. Include all non-predefined predicates you use. To make the problem easier, assume that die A 
% has number A1 on two of its sides, A2 on two sides and A3 on two sides, and similarly, die B has B1, B2, B3 and die C has C1, C2 and C3 (each number on two sides), 
% where all nine numbers A1,A2,A3, B1,B2,B3, C1,C2,C3 are different and between 1 and 9. Also note that die A is better than die B if A wins in at least five of the nine 
% possible outcomes (A1,B1),(A1,B2),...,(A3,B3), and that you have to make die A better than die B, die B better than C, and C better than A.

dice:- permutation([1,2,3,4,5,6,7,8,9],[A1,A2,A3,B1,B2,B3,C1,C2,C3]),
       better([A1,A2,A3],[B1,B2,B3]),
       better([B1,B2,B3],[C1,C2,C3]),
       better([C1,C2,C3],[A1,A2,A3]),
       write([A1, A2, A3] - [B1, B2, B3] - [C1, C2, C3]), nl.
       
better(D1,D2):-
    findall(A-B, (member(A,D1),member(B,D2), A > B), Lits),
    length(Lits, L),
    L >= 5.
    
% Fall 2014
% Consider two groups of 10 people each. In the first group, as expected, the percentage of people with lung 
% cancer among smokers is higher than among non-smokers. In the second group, the same is the case. 
% But if we consider the 20 people of the two groups together, then the situation is the opposite: 
% the proportion of people with lung cancer is higher among non-smokers than among smokers! 
% Can this be true? Write a little Prolog program to find it out.

num(X):- between(1,7,X).

cancer:-
    num(NFNC),
    num(NFC),
    num(FNC),
    num(FC),
    10 is NFNC + NFC + FNC + FC,
    FC/(FC+FNC) > NFC/(NFC+NFNC),
    num(NFNC2),
    num(NFC2),
    num(FNC2),
    num(FC2),
    10 is NFNC2 + NFC2 + FNC2 + FC2,
    FC2/(FC2+FNC2) > NFC2/(NFC2+NFNC2),
    (FC2+FC)/(FC2+FNC2+FC+FNC) <(NFC2+NFC)/(NFC2+NFNC2+NFC+NFNC),
    write([FC,FNC,NFC,NFNC,FC2,FNC2,NFC2,NFNC2]).
    
