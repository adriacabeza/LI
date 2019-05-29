:- use_module(library(clpfd)).

% Now you are flying back from China, and you should write such a program to compute how many
% units of each one of six products you should take in your suitcase with capacity 80Kg, if you want
% to maximize the total value, and the products have the following weights (Kg) and values (Euros):
%           p1  p2  p3  p4  p5  p6
%   ------------------------------
%   weight: 1   2   3   5   6   7
%   value:  1   4   7   11  14  15

%main:-
%    Unitats = [X1,X2,X3,X4,X5,X6],
%    Unitats ins 0..80,
%    SumValors #= X1*1 + X2*4 + X3*7 + X4*11 + X5*14 + X6*15,
%    SumPesos #= X1*1 + X2*2 + X3*3 + X4*5 + X5*6 + X6*7,
%    SumPesos #=< 80,
%    labeling([max(SumValors)], Unitats),
%    write(SumValors),nl,
%    write(SumPesos), nl,
%    write(Unitats), nl.
    
china:- Weight = [1, 2, 3, 5, 6, 7],
    	Value = [1, 4, 7, 11, 14, 15],
    	Capacity is 80,
    	length(Vars, 6),
    	Vars ins 0..80,
    	scalar_product(Weight, Vars, #=<, Capacity),
    	scalar_product(Value, Vars, #=, Val),
        labeling([max(Val), min(Capacity)], Vars), write(Vars).
