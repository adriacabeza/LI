:- use_module(library(clpfd)).

% Now you are flying back from China, and you should write such a program to compute how many
% units of each one of six products you should take in your suitcase with capacity 80Kg, if you want
% to maximize the total value, and the products have the following weights (Kg) and values (Euros):
%           p1  p2  p3  p4  p5  p6
%   ------------------------------
%   weight: 1   2   3   5   6   7
%   value:  1   4   7   11  14  15

main:-
    W = [1,2,3,5,6,7],
    V = [1,4,7,11,14,15],
    length(Vars,6),
    Vars ins 0..80,
    producte(W,Vars,Pes),
    producte(V,Vars,Suma),
    Pes #=< 80,
    %scalar_product(W,Vars,#=<, 80),
    %scalar_product(V,Vars,#=, Suma),
    labeling([max(Suma)], Vars),
    write(Vars).
    
producte([W|Weights],[V|Vars], Sum + W*V):- producte(Weights,Vars,Sum).
producte([W],[V],V*W).
